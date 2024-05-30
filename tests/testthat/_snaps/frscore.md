# frscore works for mv

    Code
      frscore(mv_all[[1]][[1]], dat = mvdat)
    Output
      processing 6 unique model types,
      maxsols set to 50, excluding 0 model types from scoring
      
      0 / 30 submodel relations tested1 / 30 potential submodel relations tested4 / 30 potential submodel relations tested7 / 30 potential submodel relations tested2 / 30 potential submodel relations tested 3 / 30 potential submodel relations tested 12 / 30 potential submodel relations tested 13 / 30 potential submodel relations tested 18 / 30 potential submodel relations tested 20 / 30 potential submodel relations tested 22 / 30 potential submodel relations tested 24 / 30 potential submodel relations tested 25 / 30 potential submodel relations tested 25 / 30 potential submodel relations tested 26 / 30 potential submodel relations tested 26 / 30 potential submodel relations tested 27 / 30 potential submodel relations tested 28 / 30 potential submodel relations tested 29 / 30 potential submodel relations tested 30 / 30 potential submodel relations tested 30 / 30 potential submodel relations tested 30 / 30 potential submodel relations tested 30 / 30 potential submodel relations tested 
      
      FRscore, score normalization: truemax 
      
      maxsols set to 50 -- 0 solution types excluded from scoring 
      
      -----
       
      Model types: 
      
                                                                  model score tokens
      1                                           A=3*E=2+D=1*E=1<->B=2    13      2
      2             (C=3*D=3*E=1+D=2*E=1<->A=2)*(A=3*E=2+D=1*E=1<->B=2)     8      4
      3                               A=2*D=2*E=2+A=3*E=2+D=1*E=1<->B=2     6      3
      4 (B=1*C=3*E=1+B=3*C=3*E=1+D=2*E=1<->A=2)*(A=3*E=2+D=1*E=1<->B=2)     5      1
      5             (B=3*C=3*E=1+D=2*E=1<->A=2)*(A=3*E=2+D=1*E=1<->B=2)     5      2
      6             (B=1*C=3*E=1+D=2*E=1<->A=2)*(A=3*E=2+D=1*E=1<->B=2)     3      1
        norm.score
      1  1.0000000
      2  0.6153846
      3  0.4615385
      4  0.3846154
      5  0.3846154
      6  0.2307692
      
      

# frscore works for cs

    Code
      frscore(ss)
    Output
      processing 13 unique model types,
      maxsols set to 50, excluding 0 model types from scoring
      
      0 / 156 submodel relations tested2 / 156 potential submodel relations tested4 / 156 potential submodel relations tested12 / 156 potential submodel relations tested16 / 156 potential submodel relations tested19 / 156 potential submodel relations tested10 / 156 potential submodel relations tested 11 / 156 potential submodel relations tested 15 / 156 potential submodel relations tested 31 / 156 potential submodel relations tested 32 / 156 potential submodel relations tested 41 / 156 potential submodel relations tested 51 / 156 potential submodel relations tested 61 / 156 potential submodel relations tested 71 / 156 potential submodel relations tested 80 / 156 potential submodel relations tested 85 / 156 potential submodel relations tested 90 / 156 potential submodel relations tested 95 / 156 potential submodel relations tested 100 / 156 potential submodel relations tested 103 / 156 potential submodel relations tested 106 / 156 potential submodel relations tested 108 / 156 potential submodel relations tested 108 / 156 potential submodel relations tested 114 / 156 potential submodel relations tested 115 / 156 potential submodel relations tested 116 / 156 potential submodel relations tested 117 / 156 potential submodel relations tested 117 / 156 potential submodel relations tested 118 / 156 potential submodel relations tested 119 / 156 potential submodel relations tested 120 / 156 potential submodel relations tested 120 / 156 potential submodel relations tested 121 / 156 potential submodel relations tested 122 / 156 potential submodel relations tested 123 / 156 potential submodel relations tested 129 / 156 potential submodel relations tested 129 / 156 potential submodel relations tested 130 / 156 potential submodel relations tested 131 / 156 potential submodel relations tested 132 / 156 potential submodel relations tested 132 / 156 potential submodel relations tested 132 / 156 potential submodel relations tested 133 / 156 potential submodel relations tested 134 / 156 potential submodel relations tested 135 / 156 potential submodel relations tested 136 / 156 potential submodel relations tested 136 / 156 potential submodel relations tested 137 / 156 potential submodel relations tested 138 / 156 potential submodel relations tested 138 / 156 potential submodel relations tested 139 / 156 potential submodel relations tested 140 / 156 potential submodel relations tested 141 / 156 potential submodel relations tested 142 / 156 potential submodel relations tested 143 / 156 potential submodel relations tested 143 / 156 potential submodel relations tested 144 / 156 potential submodel relations tested 145 / 156 potential submodel relations tested 146 / 156 potential submodel relations tested 146 / 156 potential submodel relations tested 146 / 156 potential submodel relations tested 147 / 156 potential submodel relations tested 148 / 156 potential submodel relations tested 149 / 156 potential submodel relations tested 150 / 156 potential submodel relations tested 151 / 156 potential submodel relations tested 152 / 156 potential submodel relations tested 153 / 156 potential submodel relations tested 154 / 156 potential submodel relations tested 155 / 156 potential submodel relations tested 156 / 156 potential submodel relations tested 156 / 156 potential submodel relations tested 156 / 156 potential submodel relations tested 156 / 156 potential submodel relations tested 
      
      FRscore, score normalization: truemax 
      
      maxsols set to 50 -- 0 solution types excluded from scoring 
      
      -----
       
      Model types: 
      
                         model score tokens norm.score
      1                  A<->E    22      1 1.00000000
      2                A+B<->E    21      2 0.95454545
      3              A+B*C<->E    19      3 0.86363636
      4        A*D+A*c+B*C<->E    18      4 0.81818182
      5            A*c+B*C<->E    16      4 0.72727273
      6            A*D+B*C<->E    14      2 0.63636364
      7        A*B+A*D+B*C<->E    10      2 0.45454545
      8        A*B+A*D+A*c<->E     5      2 0.22727273
      9      (E<->A)*(A+B<->E)     4      1 0.18181818
      10                 E<->A     4      1 0.18181818
      11 (D*E+E*c<->A)*(A<->E)     2      1 0.09090909
      12   (E<->A)*(A+C*D<->E)     2      1 0.09090909
      13   (E<->A)*(B+C*D<->E)     1      1 0.04545455
      
      

# frscore corner cases work

    Code
      frscore(ss2)
    Output
      processing 3 unique model types,
      maxsols set to 50, excluding 0 model types from scoring
      
      FRscore, score normalization: truemax 
      
      no submodel checks were needed, argument 'maxsols' ignored 
      -----
       
      Model types: 
      
                  model score tokens norm.score
      1 A*D+A*c+B*C<->E     4      3          1
      2 A*D+A*c+B*X<->E     0      1          0
      3 A*D+A*c+B*Y<->E     0      1          0
      
      

---

    Code
      frscore(ss[1])
    Output
      processing 1 unique model types,
      maxsols set to 50, excluding 0 model types from scoring
      
      FRscore, score normalization: truemax 
      
      maxsols set to 50 -- 0 solution types excluded from scoring 
      
      -----
       
      Model types: 
      
                  model score tokens norm.score
      1 A*D+A*c+B*C<->E     0      1          0
      
      

---

    Code
      frscore(ss2[1:2])
    Output
      processing 1 unique model types,
      maxsols set to 50, excluding 0 model types from scoring
      
      FRscore, score normalization: truemax 
      
      maxsols set to 50 -- 0 solution types excluded from scoring 
      
      -----
       
      Model types: 
      
                  model score tokens norm.score
      1 A*D+A*c+B*C<->E     2      2          1
      
      

---

    Code
      frscore(ss, maxsols = 1)
    Output
      processing 13 unique model types,
      maxsols set to 1, excluding 12 model types from scoring
      
      FRscore, score normalization: truemax 
      
      maxsols set to 1 -- 12 solution types excluded from scoring 
      
      -----
       
      Model types: 
      
        model score tokens norm.score
      1 A<->E     0      1          0
      
      

