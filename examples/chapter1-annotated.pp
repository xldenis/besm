Below is given an example of the assembly of the logical scheme for the problem of calculating the elements of a tenth-order matrix and inversion of the matrix obtained with checking of the inversion.

1. Variable Addresses

(a) Block a (100 cells)

# The expressions below calculate the index each variable points to within the block.
<a_αβ> = 10.α + 1.β
<a_ll> = 11.l
<a_mk> = 10.m + 1.k
<a_mm> = 11.m
<a_im> = 10.i + 1.m
<a_pr> = 10.p + 1.r
<a_ij> = 10.i + 1.j

(b) Block b (10 cells)

<b_α> = 1.α
<b_s> = 1.s

(c) Block d (10 cells)

<d_k> = 1.k
<d_m> = 1.m
<d_j> = 1.j

(d) Block e (10 cells)

<e_s> = 1.s
<e_r> = 1.r

2. Parameters

# All loop parameters must be listed ahead of time. They always step by 1.
# There are two other kinds of loops not described here: characteristic and non-characteristic (logical) loops.

β : β_in = 0, β_fin = 10
α : α_in = 0, α_fin = 10
l : l_in = 0, l_fin = 10
k : k_in = 0, k_fin = 10
j : j_in = 0, j_fin = 10
i : i_in = 0, i_fin = 10
m : m_in = 0, m_fin = 10
p : p_in = 0, p_fin = 10
r : r_in = 0, r_fin = 10
s : s_in = 0, s_fin = 10

3. List of Constants and Variable Quantities

0,10,1,a,c,f,R

4. Logical Scheme

[α # loop over variable α
  0 => b_α # assign 0 to variable address b_α
  [β
    α - β => a;
    # logical operator essentially a switch statement. takes a variable, a default operator sign
    # (label) and a set of ranges and operator signs. It then checks if the variable is within
    # one of the ranges provided and goes to that operator if it isn't it goes to the default operator.
    P(β, 0101; 0102 / (-∞, α));
    L0101 a + 10 => a # operator sign 0101
    L0102 Form a ,=> 0 # Form transforms a number to decimal, `, => 0` prints the expression
    b_α + a => b_α;
    a => a_αβ
  ]
]
[l
  a_ll - 1 => a_ll
]
[m
  [k
    a_mk => d_k;
    0 => a_mk
  ]
  1 => a_mm;
  d_m + 1 => c;
  Form c, => 0
  [i
    a_im : c => f # : is division
    [j
      a_ij - j * d_j => a_ij
    ]
  ]
]
[r
  0 => e_r
  [p
    e_r + a_pr => e_r
  ]
]
0 => R
[s
  R + b_s * e_s => R
]
Form R, => 0;
stop # stops the machine
