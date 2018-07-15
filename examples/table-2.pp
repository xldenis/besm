# This is not valid syntax in the current compiler but is instead presented literally.
# n is a meta-linguistic variable

# this program calculates the formula for seidel iterations

# [k 0 => ω [t 0 => s [j P(j, 0101, 0102/[t, t]); L0101 s + a_tj * x_j => s L 0102];
#   (b_t - s): a_tt - x_t => δ; P(δ, 0103, 0104/[0, 0]); L0103 x_t + δ => x_t; 1 => ω L0104]]; Stop
#
# k: from 1 to ω = 0
# t: from 0 to k
# j: from 0 to n
#
# Array a - n^2 locations
#
# <a_tj> = n.t + 1.j
# <a_tt> = (n + 1).t
#
# Array b - n locations
#
# <b_t> = 1.t
#
# Array x - n locations
#
# <x_j> = 1.j
# <x_t> = 1.t

1. Variable Addresses

(a) Block a (n^2 cells)

<a_tj> = n.t + 1.j
<a_tt> = (n + 1).t

(b) Block b (n cells)

<b_t> = 1.t

(x) Block x (n cells)

<x_j> = 1.j
<x_t> = 1.t

2. Parameters

k : k_in = 1, to  ω = 0 # k demonstrates a 'characteristic specific loop'
t : t_in = 0, t_fin = k # t exhibits a dependence on a 'higher-order' parameter
j : j_in = 0, j_fin = n

3. List of Constants and Variable Quantities

0, 1, n, δ

4. Logical Scheme

[k
  0 => ω
  [t
    0 => s
    [j
      P(j, 0101, 0102/[t, t]);
      L0101 s + a_tj * x_j => s L 0102
    ];
    (b_t - s): a_tt - x_t => δ;
    P(δ, 0103, 0104/[0, 0]);
    L0103 x_t + δ => x_t;
    1 => ω L0104
  ]
]; Stop
