1. Variable Addresses

(a) Block a (11 cells)

<a_0> = 0
<a_j> = -1 * j + 10

2. Parameters

j : j_in = 0, j_fin = 11

3. List of Constants and Variable Quantities

0, 11, 10, 5, y, 400, 999, i

4. Logical Scheme

(Ma, 080, 0, a_0); (Mb, 0, 11, 0);

[j
  10 - j => i;
  sqrt mod a_j + 5 * a_j^3 => y;
  P(y, 0102; 0101 / (400, ∞));
  L0101 Form i ,=>0; Form 999 ,=> 0; /0103;
  L0102 Form i ,=>0; Form y ,=> 0; L0103;
]; stop
