#!/usr/bin/env python3
"""
End-to-end test harness for PP-BESM.

A PP-BESM source programme is pushed through the whole toolchain:

  1. encode   pp-besm turns the `.pp` source into the hex tape image that the
              compiler expects on MD-2.
  2. compile  besm-vm runs the compiler (bootloader + MP-1/2/3) headlessly. At
              the end of PP-3 the finished programme sits on MD-1 and the
              final storage layout (C0*, K0*, ...) is in the `final-header`
              cells of PP-3.
  3. run      MD-1 is loaded as the instruction store and executed from K0*+1.
  4. check    the final contents of the store (and anything printed by PN) are
              compared against the expectations of a test spec.

Usage:

  besm_test.py run  PROGRAM.pp [--disasm]   compile, run and report
  besm_test.py test [SPEC.toml | DIR ...]   run test specs (default: harness/tests)

See harness/README.md for the spec format.
"""

import argparse
import glob
import os
import re
import shutil
import subprocess
import sys
import tempfile
import tomllib
from dataclasses import dataclass, field

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
VM_DIR = os.path.join(ROOT, "besm-vm")
DEFAULT_TESTS = os.path.join(ROOT, "harness", "tests")

COMPILER_START = 1025  # bootloader lives at the start of the DS
IS_SIZE = 1023

# Order of the cells in PP-3's `final-header` (see PP3.hs, "Final layout").
HEADER_FIELDS = ["c0", "k0", "gamma0", "o0", "m0", "r0", "rf"]


class HarnessError(Exception):
    pass


# ---------------------------------------------------------------------------
# Tools


@dataclass
class Tools:
    vm: str
    pp_besm: list
    images: str  # directory holding boot.txt, mp{1,2,3}.txt and pp3.sm
    max_steps: int
    timeout: float


def find_pp_besm():
    if os.environ.get("PP_BESM"):
        return [os.environ["PP_BESM"]]
    candidates = glob.glob(os.path.join(ROOT, "pp-besm", ".stack-work", "install", "*", "*", "*", "bin", "pp-besm"))
    if candidates:
        return [max(candidates, key=os.path.getmtime)]
    if shutil.which("stack"):
        return ["stack", "--stack-yaml", os.path.join(ROOT, "pp-besm", "stack.yaml"), "exec", "--", "pp-besm"]
    raise HarnessError("could not find pp-besm; build it with `stack build` in pp-besm/ or set PP_BESM")


def find_vm(build):
    if os.environ.get("BESM_VM"):
        return os.environ["BESM_VM"]
    if build:
        res = subprocess.run(["cargo", "build", "--release", "--quiet"], cwd=VM_DIR, capture_output=True, text=True)
        if res.returncode != 0:
            raise HarnessError("failed to build besm-vm:\n" + res.stderr)
    vm = os.path.join(VM_DIR, "target", "release", "besm-vm")
    if not os.path.exists(vm):
        raise HarnessError(f"{vm} does not exist; run `cargo build --release` in besm-vm/")
    return vm


# ---------------------------------------------------------------------------
# Words


def decode_float(word):
    """Decode a 39-bit BESM word as a number.

    Bits 33..38 hold the exponent (6-bit two's complement), bit 32 the sign and
    bits 0..31 the mantissa, a binary fraction: value = mant * 2^(exp - 32).
    """
    exp = (word >> 33) & 0x3F
    if exp >= 32:
        exp -= 64
    sign = (word >> 32) & 1
    mant = word & 0xFFFFFFFF
    value = mant * 2.0 ** (exp - 32)
    return -value if sign else value


def parse_memory_dump(stdout):
    """Parse the output of `besm-vm trace --print-memory` into {addr: word}."""
    mem = {}
    for line in stdout.splitlines():
        parts = line.split()
        # <addr> <instruction...> <float> <hex> <binary>
        if len(parts) >= 4 and parts[0].isdigit() and re.fullmatch(r"[01]{39}", parts[-1]):
            mem[int(parts[0])] = int(parts[-2], 16)
    return mem


@dataclass
class VMResult:
    status: str  # "stopped", "error", "infinite-loop", "step-limit", "timeout"
    detail: str
    memory: dict
    printed: list
    stdout: str
    stderr: str


def run_vm(tools, args, print_memory):
    cmd = [tools.vm, "trace", "--no-trace", f"--max-steps={tools.max_steps}", *args]
    for rng in print_memory:
        cmd += ["--print-memory", rng]
    try:
        res = subprocess.run(cmd, capture_output=True, text=True, timeout=tools.timeout)
    except subprocess.TimeoutExpired as e:
        out = e.stdout.decode() if isinstance(e.stdout, bytes) else (e.stdout or "")
        return VMResult("timeout", f"no stop after {tools.timeout}s", {}, [], out, "")

    stderr = res.stderr.strip()
    if res.returncode != 0:
        status, detail = "error", f"VM crashed (exit {res.returncode}): {stderr.splitlines()[-1] if stderr else ''}"
    elif m := re.search(r"^Stopped at address (\d+)", stderr, re.M):
        status, detail = "stopped", f"stopped at {m.group(1)}"
    elif m := re.search(r"^Step limit reached.*", stderr, re.M):
        status, detail = "step-limit", m.group(0).lower()
    elif "InfiniteLoop" in stderr:
        status, detail = "infinite-loop", "infinite loop detected"
    else:
        status, detail = "error", " / ".join(stderr.splitlines()) or "VM exited without stopping"

    printed = [line.split()[1] for line in res.stdout.splitlines() if line.startswith("PN ")]
    return VMResult(status, detail, parse_memory_dump(res.stdout), printed, res.stdout, res.stderr)


# ---------------------------------------------------------------------------
# Source programmes


def quantity_names(source):
    """The entries of section 3 (constants and variable quantities), in order.

    PP-3 lays block C out in this order directly after the parameter counters,
    so entry i lives in cell C0* + 1 + i of the finished programme.
    """
    m = re.search(r"^\s*3\.[^\n]*\n(.*?)^\s*4\.", source, re.S | re.M)
    if not m:
        return []
    body = "\n".join(line for line in m.group(1).splitlines() if not line.lstrip().startswith("#"))
    return [item.strip() for item in body.split(",") if item.strip()]


def is_constant(name):
    try:
        float(name)
        return True
    except ValueError:
        return False


def header_address(images):
    """Address of PP-3's `final-header`, taken from the PP-3 source map."""
    sm = os.path.join(images, "pp3.sm")
    try:
        with open(sm) as f:
            for line in f:
                if line.split()[1:] == ["uk.", "final-header"]:
                    return int(line.split()[0])
    except FileNotFoundError:
        pass
    raise HarnessError(f"could not find `final-header` in {sm}")


# ---------------------------------------------------------------------------
# Pipeline


@dataclass
class Execution:
    source: str
    workdir: str
    compile: VMResult = None
    header: dict = field(default_factory=dict)
    run: VMResult = None
    values: dict = field(default_factory=dict)  # quantity name -> number
    start: int = None


def execute(tools, pp_path, workdir, start=None):
    with open(pp_path) as f:
        source = f.read()
    ex = Execution(source=source, workdir=workdir)

    # 1. encode
    tape = os.path.join(workdir, "tape.txt")
    res = subprocess.run([*tools.pp_besm, "print", pp_path], capture_output=True, text=True)
    if res.returncode != 0:
        raise HarnessError(f"pp-besm failed to encode {pp_path}:\n{res.stderr.strip()}")
    with open(tape, "w") as f:
        f.write(res.stdout)

    # 2. compile
    md1 = os.path.join(workdir, "md1.bin")
    hdr = header_address(tools.images)
    img = lambda name: os.path.join(tools.images, name)
    ex.compile = run_vm(
        tools,
        [
            f"--bootloader={img('boot.txt')}",
            f"--md0={img('mp1.txt')}",
            f"--md4={img('mp2.txt')}",
            f"--md3={img('mp3.txt')}",
            f"--md2={tape}",
            f"--md1-out={md1}",
            f"--start-address={COMPILER_START}",
        ],
        [f"{hdr}:{hdr + len(HEADER_FIELDS) - 1}"],
    )
    if ex.compile.status != "stopped":
        return ex
    ex.header = {name: ex.compile.memory[hdr + i] & 0x7FF for i, name in enumerate(HEADER_FIELDS)}
    ex.start = start if start is not None else ex.header["k0"] + 1

    # 3. run. MD-1 cell n holds the word destined for address n, while the IS
    # image file starts at address 1, so drop drum cell 0.
    image = os.path.join(workdir, "programme.bin")
    with open(md1, "rb") as src, open(image, "wb") as dst:
        dst.write(src.read()[8:])
    ex.run = run_vm(tools, [image, f"--start-address={ex.start}"], [f"1:{IS_SIZE}"])

    # 4. name the results
    if ex.run.memory:
        for i, name in enumerate(quantity_names(source)):
            addr = ex.header["c0"] + 1 + i
            if not is_constant(name) and addr in ex.run.memory:
                ex.values[name] = decode_float(ex.run.memory[addr])
    return ex


# ---------------------------------------------------------------------------
# Specs


@dataclass
class Spec:
    path: str
    name: str
    program: str
    compile: str = "stopped"
    run: str = "stopped"
    values: dict = field(default_factory=dict)
    memory: dict = field(default_factory=dict)
    printed: list = None
    tolerance: float = 1e-9
    start: int = None
    xfail: str = None
    skip: str = None


def load_spec(path):
    with open(path, "rb") as f:
        data = tomllib.load(f)
    base = os.path.dirname(path)
    if "source" in data:
        program = os.path.normpath(os.path.join(base, data["source"]))
    elif "program" in data:
        program = None
    else:
        raise HarnessError(f"{path}: needs either `source` or `program`")
    exp = data.get("expect", {})
    spec = Spec(
        path=path,
        name=data.get("name", os.path.splitext(os.path.basename(path))[0]),
        program=program,
        compile=exp.get("compile", "stopped"),
        run=exp.get("run", "stopped"),
        values=exp.get("values", {}),
        memory={int(k): v for k, v in exp.get("memory", {}).items()},
        printed=exp.get("printed"),
        tolerance=exp.get("tolerance", 1e-9),
        start=data.get("start"),
        xfail=data.get("xfail"),
        skip=data.get("skip"),
    )
    spec.inline = data.get("program")
    return spec


def close(actual, expected, tol):
    return abs(actual - expected) <= tol * max(1.0, abs(expected))


def check(spec, ex):
    """Return a list of failure messages (empty if the spec is satisfied)."""
    failures = []
    if ex.compile.status != spec.compile:
        failures.append(f"compile: expected {spec.compile}, got {ex.compile.detail}")
        return failures
    if ex.run is None:
        return failures
    if ex.run.status != spec.run:
        failures.append(f"run: expected {spec.run}, got {ex.run.detail}")
    for name, expected in spec.values.items():
        if name not in ex.values:
            failures.append(f"{name}: not a variable quantity of the programme")
        elif not close(ex.values[name], expected, spec.tolerance):
            failures.append(f"{name}: expected {expected}, got {ex.values[name]:g}")
    for addr, expected in spec.memory.items():
        word = ex.run.memory.get(addr)
        if word is None:
            failures.append(f"[{addr}]: not in memory dump")
        elif isinstance(expected, str):
            if word != int(expected, 16):
                failures.append(f"[{addr}]: expected word {expected}, got {word:010x}")
        elif not close(decode_float(word), expected, spec.tolerance):
            failures.append(f"[{addr}]: expected {expected}, got {decode_float(word):g}")
    if spec.printed is not None:
        expected = [p.lower() for p in spec.printed]
        if ex.run.printed != expected:
            failures.append(f"printed: expected {expected}, got {ex.run.printed}")
    return failures


# ---------------------------------------------------------------------------
# Reporting


def disassemble(tools, ex, start, end):
    """Use the VM's own disassembler on the finished programme."""
    res = run_vm(tools, [os.path.join(ex.workdir, "programme.bin"), "--start-address=1023"], [f"{start}:{end}"])
    return [line for line in res.stdout.splitlines() if line[:4].strip().isdigit()]


def report(tools, ex, disasm):
    print(f"compile: {ex.compile.detail}")
    if ex.compile.printed:
        print(f"  printed: {' '.join(ex.compile.printed)}")
    if ex.compile.status != "stopped":
        print(ex.compile.stderr.rstrip())
        return
    h = ex.header
    print(
        "layout:  P 1-{c0}  C {c}-{k0}  K {k}-{gamma0}  γ {g}-{o0}  O {o}-{m0}  R {r}-{rf}".format(
            c=h["c0"] + 1, k=h["k0"] + 1, g=h["gamma0"] + 1, o=h["o0"] + 1, r=h["r0"] + 1, **h
        )
    )
    if disasm:
        for line in disassemble(tools, ex, 1, max(h["gamma0"], h["k0"] + 1)):
            print("  " + line)
    print(f"run:     {ex.run.detail} (started at {ex.start})")
    if ex.run.printed:
        print(f"  printed: {' '.join(ex.run.printed)}")
    if ex.values:
        print("values:")
        for name, value in ex.values.items():
            print(f"  {name} = {value:g}")


def make_workdir(keep, name):
    if keep:
        path = os.path.join(keep, name)
        os.makedirs(path, exist_ok=True)
        return path
    return tempfile.mkdtemp(prefix=f"besm-{name}-")


def cmd_run(tools, args):
    name = os.path.splitext(os.path.basename(args.program))[0]
    workdir = make_workdir(args.keep, name)
    try:
        ex = execute(tools, args.program, workdir, args.start)
        report(tools, ex, args.disasm)
        ok = ex.compile.status == "stopped" and ex.run.status == "stopped"
    finally:
        if not args.keep:
            shutil.rmtree(workdir, ignore_errors=True)
    if args.keep:
        print(f"artifacts in {workdir}")
    return 0 if ok else 1


def collect_specs(paths):
    files = []
    for p in paths or [DEFAULT_TESTS]:
        if os.path.isdir(p):
            files += sorted(glob.glob(os.path.join(p, "*.toml")))
        else:
            files.append(p)
    return [load_spec(f) for f in files]


def cmd_test(tools, args):
    specs = collect_specs(args.specs)
    if args.filter:
        specs = [s for s in specs if args.filter in s.name]
    counts = {"PASS": 0, "FAIL": 0, "XFAIL": 0, "XPASS": 0, "SKIP": 0, "ERROR": 0}

    for spec in specs:
        if spec.skip:
            counts["SKIP"] += 1
            print(f"SKIP   {spec.name}  ({spec.skip})")
            continue
        workdir = make_workdir(args.keep, spec.name)
        try:
            program = spec.program
            if program is None:
                program = os.path.join(workdir, spec.name + ".pp")
                with open(program, "w") as f:
                    f.write(spec.inline)
            ex = execute(tools, program, workdir, spec.start)
            failures = check(spec, ex)
        except HarnessError as e:
            ex, failures = None, [f"harness error: {e}"]
            outcome = "ERROR"
        else:
            if spec.xfail:
                outcome = "XPASS" if not failures else "XFAIL"
            else:
                outcome = "FAIL" if failures else "PASS"
        finally:
            if not args.keep:
                shutil.rmtree(workdir, ignore_errors=True)

        counts[outcome] += 1
        note = f"  ({spec.xfail})" if spec.xfail else ""
        print(f"{outcome:6} {spec.name}{note}")
        if failures and (outcome != "XFAIL" or args.verbose):
            for msg in failures:
                print(f"         {msg}")
        if args.verbose and ex is not None and ex.run is not None:
            for name, value in ex.values.items():
                print(f"         {name} = {value:g}")

    print()
    print("  ".join(f"{n} {k.lower()}" for k, n in counts.items() if n))
    return 1 if counts["FAIL"] or counts["XPASS"] or counts["ERROR"] else 0


def main():
    common = argparse.ArgumentParser(add_help=False)
    common.add_argument("--images", default=VM_DIR, help="directory with boot.txt, mp{1,2,3}.txt, pp3.sm (default: besm-vm/)")
    common.add_argument("--max-steps", type=int, default=2_000_000, help="instructions per VM run before giving up")
    common.add_argument("--timeout", type=float, default=60, help="seconds before a VM run is killed")
    common.add_argument("--no-build", action="store_true", help="don't (re)build besm-vm before running")
    common.add_argument("--keep", metavar="DIR", help="keep intermediate files (tape, MD-1 image) in DIR")
    parser = argparse.ArgumentParser(description="Compile PP-BESM programmes and check the results of running them.")
    sub = parser.add_subparsers(dest="command", required=True)

    run = sub.add_parser("run", parents=[common], help="compile and run a single programme, reporting the results")
    run.add_argument("program", help="PP-BESM source file")
    run.add_argument("--disasm", action="store_true", help="disassemble the compiled programme")
    run.add_argument("--start", type=int, help="start address of the compiled programme (default: K0*+1)")

    test = sub.add_parser("test", parents=[common], help="run test specs")
    test.add_argument("specs", nargs="*", help="spec files or directories (default: harness/tests)")
    test.add_argument("-k", dest="filter", help="only run specs whose name contains this string")
    test.add_argument("-v", "--verbose", action="store_true", help="show values and expected failures")

    args = parser.parse_args()
    try:
        tools = Tools(
            vm=find_vm(not args.no_build),
            pp_besm=find_pp_besm(),
            images=args.images,
            max_steps=args.max_steps,
            timeout=args.timeout,
        )
        return {"run": cmd_run, "test": cmd_test}[args.command](tools, args)
    except HarnessError as e:
        print(f"error: {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
