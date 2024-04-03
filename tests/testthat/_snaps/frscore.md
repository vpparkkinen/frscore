# frscore works for mv

    Code
      frscore(mv_all[[1]][[1]], dat = mvdat)
    Output
      processing 6 unique model types, maxsols set to 50 
      
      FRscore, score type: full || score normalization: truemax 
      
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
      processing 13 unique model types, maxsols set to 50 
      
      FRscore, score type: full || score normalization: truemax 
      
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
      
      

