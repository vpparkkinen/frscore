# frscored_cna works

    Code
      frscored_cna(d.error)
    Output
      0 / 16 reanalyses completed 1 / 16 reanalyses completed 2 / 16 reanalyses completed 3 / 16 reanalyses completed 4 / 16 reanalyses completed 5 / 16 reanalyses completed 6 / 16 reanalyses completed 7 / 16 reanalyses completed 8 / 16 reanalyses completed 9 / 16 reanalyses completed 10 / 16 reanalyses completed 11 / 16 reanalyses completed 12 / 16 reanalyses completed 13 / 16 reanalyses completed 14 / 16 reanalyses completed 15 / 16 reanalyses completed 16 / 16 reanalyses completed
      
      processing 13 unique model types, maxsols set to 50 
      
      FR-scored reanalysis series with fit range 1 to 0.7 with granularity 0.1 
      Score type: full || score normalization: truemax 
      maxsols set to 50 -- 0 model types excluded from scoring 
      
      ----- 
       
      Model types: 
       
         outcome             condition consistency  coverage complexity inus score
      1  E                       A<->E   0.8750000 0.7777778          1 TRUE    22
      2  E                     A+B<->E   0.7500000 1.0000000          2 TRUE    21
      3  E                   A+B*C<->E   0.9000000 1.0000000          3 TRUE    19
      4  E             A*c+A*D+B*C<->E   1.0000000 1.0000000          6 TRUE    18
      5  E                 A*c+B*C<->E   1.0000000 0.8888889          4 TRUE    16
      6  E                 A*D+B*C<->E   1.0000000 0.7777778          4 TRUE    14
      7  E             A*B+A*D+B*C<->E   1.0000000 0.8888889          6 TRUE    10
      8  E             A*B+A*c+A*D<->E   1.0000000 0.7777778          6 TRUE     5
      9  A,E         (E<->A)*(A+B<->E)   0.7500000 0.8750000          3 TRUE     4
      10 A                       E<->A   0.7777778 0.8750000          1 TRUE     4
      11 A,E       (E<->A)*(A+C*D<->E)   0.7777778 0.8750000          4 TRUE     2
      12 A,E     (c*E+D*E<->A)*(A<->E)   0.8571429 0.7500000          5 TRUE     2
      13 A,E       (E<->A)*(B+C*D<->E)   0.7000000 0.7777778          4 TRUE     1
         tokens norm.score
      1       1 1.00000000
      2       2 0.95454545
      3       3 0.86363636
      4       4 0.81818182
      5       4 0.72727273
      6       2 0.63636364
      7       2 0.45454545
      8       2 0.22727273
      9       1 0.18181818
      10      1 0.18181818
      11      1 0.09090909
      12      1 0.09090909
      13      1 0.04545455
      

---

    Code
      frscored_cna(d.error, normalize = "idealmax")
    Output
      0 / 16 reanalyses completed 1 / 16 reanalyses completed 2 / 16 reanalyses completed 3 / 16 reanalyses completed 4 / 16 reanalyses completed 5 / 16 reanalyses completed 6 / 16 reanalyses completed 7 / 16 reanalyses completed 8 / 16 reanalyses completed 9 / 16 reanalyses completed 10 / 16 reanalyses completed 11 / 16 reanalyses completed 12 / 16 reanalyses completed 13 / 16 reanalyses completed 14 / 16 reanalyses completed 15 / 16 reanalyses completed 16 / 16 reanalyses completed
      
      processing 13 unique model types, maxsols set to 50 
      
      FR-scored reanalysis series with fit range 1 to 0.7 with granularity 0.1 
      Score type: full || score normalization: idealmax 
      maxsols set to 50 -- 0 model types excluded from scoring 
      
      ----- 
       
      Model types: 
       
         outcome             condition consistency  coverage complexity inus score
      1  E                       A<->E   0.8750000 0.7777778          1 TRUE    22
      2  E                     A+B<->E   0.7500000 1.0000000          2 TRUE    21
      3  E                   A+B*C<->E   0.9000000 1.0000000          3 TRUE    19
      4  E             A*c+A*D+B*C<->E   1.0000000 1.0000000          6 TRUE    18
      5  E                 A*c+B*C<->E   1.0000000 0.8888889          4 TRUE    16
      6  E                 A*D+B*C<->E   1.0000000 0.7777778          4 TRUE    14
      7  E             A*B+A*D+B*C<->E   1.0000000 0.8888889          6 TRUE    10
      8  E             A*B+A*c+A*D<->E   1.0000000 0.7777778          6 TRUE     5
      9  A,E         (E<->A)*(A+B<->E)   0.7500000 0.8750000          3 TRUE     4
      10 A                       E<->A   0.7777778 0.8750000          1 TRUE     4
      11 A,E       (E<->A)*(A+C*D<->E)   0.7777778 0.8750000          4 TRUE     2
      12 A,E     (c*E+D*E<->A)*(A<->E)   0.8571429 0.7500000          5 TRUE     2
      13 A,E       (E<->A)*(B+C*D<->E)   0.7000000 0.7777778          4 TRUE     1
         tokens norm.score
      1       1 0.70967742
      2       2 0.67741935
      3       3 0.61290323
      4       4 0.58064516
      5       4 0.51612903
      6       2 0.45161290
      7       2 0.32258065
      8       2 0.16129032
      9       1 0.12903226
      10      1 0.12903226
      11      1 0.06451613
      12      1 0.06451613
      13      1 0.03225806
      

---

    Code
      frscored_cna(d.error, normalize = "none")
    Output
      0 / 16 reanalyses completed 1 / 16 reanalyses completed 2 / 16 reanalyses completed 3 / 16 reanalyses completed 4 / 16 reanalyses completed 5 / 16 reanalyses completed 6 / 16 reanalyses completed 7 / 16 reanalyses completed 8 / 16 reanalyses completed 9 / 16 reanalyses completed 10 / 16 reanalyses completed 11 / 16 reanalyses completed 12 / 16 reanalyses completed 13 / 16 reanalyses completed 14 / 16 reanalyses completed 15 / 16 reanalyses completed 16 / 16 reanalyses completed
      
      processing 13 unique model types, maxsols set to 50 
      
      FR-scored reanalysis series with fit range 1 to 0.7 with granularity 0.1 
      Score type: full || score normalization: none 
      maxsols set to 50 -- 0 model types excluded from scoring 
      
      ----- 
       
      Model types: 
       
         outcome             condition consistency  coverage complexity inus score
      1  E                       A<->E   0.8750000 0.7777778          1 TRUE    22
      2  E                     A+B<->E   0.7500000 1.0000000          2 TRUE    21
      3  E                   A+B*C<->E   0.9000000 1.0000000          3 TRUE    19
      4  E             A*c+A*D+B*C<->E   1.0000000 1.0000000          6 TRUE    18
      5  E                 A*c+B*C<->E   1.0000000 0.8888889          4 TRUE    16
      6  E                 A*D+B*C<->E   1.0000000 0.7777778          4 TRUE    14
      7  E             A*B+A*D+B*C<->E   1.0000000 0.8888889          6 TRUE    10
      8  E             A*B+A*c+A*D<->E   1.0000000 0.7777778          6 TRUE     5
      9  A,E         (E<->A)*(A+B<->E)   0.7500000 0.8750000          3 TRUE     4
      10 A                       E<->A   0.7777778 0.8750000          1 TRUE     4
      11 A,E       (E<->A)*(A+C*D<->E)   0.7777778 0.8750000          4 TRUE     2
      12 A,E     (c*E+D*E<->A)*(A<->E)   0.8571429 0.7500000          5 TRUE     2
      13 A,E       (E<->A)*(B+C*D<->E)   0.7000000 0.7777778          4 TRUE     1
         tokens
      1       1
      2       2
      3       3
      4       4
      5       4
      6       2
      7       2
      8       2
      9       1
      10      1
      11      1
      12      1
      13      1
      

---

    Code
      frscored_cna(d.error, verbose = TRUE)
    Output
      0 / 16 reanalyses completed 1 / 16 reanalyses completed 2 / 16 reanalyses completed 3 / 16 reanalyses completed 4 / 16 reanalyses completed 5 / 16 reanalyses completed 6 / 16 reanalyses completed 7 / 16 reanalyses completed 8 / 16 reanalyses completed 9 / 16 reanalyses completed 10 / 16 reanalyses completed 11 / 16 reanalyses completed 12 / 16 reanalyses completed 13 / 16 reanalyses completed 14 / 16 reanalyses completed 15 / 16 reanalyses completed 16 / 16 reanalyses completed
      
      processing 13 unique model types, maxsols set to 50 
      
      FR-scored reanalysis series with fit range 1 to 0.7 with granularity 0.1 
      Score type: full || score normalization: truemax 
      maxsols set to 50 -- 0 model types excluded from scoring 
      
      ----- 
       
      Model types: 
       
         outcome             condition consistency  coverage complexity inus score
      1  E                       A<->E   0.8750000 0.7777778          1 TRUE    22
      2  E                     A+B<->E   0.7500000 1.0000000          2 TRUE    21
      3  E                   A+B*C<->E   0.9000000 1.0000000          3 TRUE    19
      4  E             A*c+A*D+B*C<->E   1.0000000 1.0000000          6 TRUE    18
      5  E                 A*c+B*C<->E   1.0000000 0.8888889          4 TRUE    16
      6  E                 A*D+B*C<->E   1.0000000 0.7777778          4 TRUE    14
      7  E             A*B+A*D+B*C<->E   1.0000000 0.8888889          6 TRUE    10
      8  E             A*B+A*c+A*D<->E   1.0000000 0.7777778          6 TRUE     5
      9  A,E         (E<->A)*(A+B<->E)   0.7500000 0.8750000          3 TRUE     4
      10 A                       E<->A   0.7777778 0.8750000          1 TRUE     4
      11 A,E       (E<->A)*(A+C*D<->E)   0.7777778 0.8750000          4 TRUE     2
      12 A,E     (c*E+D*E<->A)*(A<->E)   0.8571429 0.7500000          5 TRUE     2
      13 A,E       (E<->A)*(B+C*D<->E)   0.7000000 0.7777778          4 TRUE     1
         tokens norm.score
      1       1 1.00000000
      2       2 0.95454545
      3       3 0.86363636
      4       4 0.81818182
      5       4 0.72727273
      6       2 0.63636364
      7       2 0.45454545
      8       2 0.22727273
      9       1 0.18181818
      10      1 0.18181818
      11      1 0.09090909
      12      1 0.09090909
      13      1 0.04545455
      
      
      Score composition: 
      ----- 
      $`A<->E`
                         model score
      8      (E<->A)*(A+B<->E)     1
      5    (E<->A)*(A+C*D<->E)     1
      4  (c*E+D*E<->A)*(A<->E)     1
      1        A*B+A*D+B*C<->E     2
      2        A*B+A*c+A*D<->E     2
      6            A*D+B*C<->E     2
      3        A*c+A*D+B*C<->E     4
      7            A*c+B*C<->E     4
      9              A+B*C<->E     3
      10               A+B<->E     2
      
      $`A+B<->E`
                      model score
      6   (E<->A)*(A+B<->E)     1
      1     A*B+A*D+B*C<->E     2
      2     A*B+A*c+A*D<->E     2
      4         A*D+B*C<->E     2
      3     A*c+A*D+B*C<->E     4
      5         A*c+B*C<->E     4
      7           A+B*C<->E     3
      12              A<->E     1
      111           A+B<->E     2
      
      $`A+B*C<->E`
                    model score
      1   A*B+A*D+B*C<->E     2
      3       A*D+B*C<->E     2
      2   A*c+A*D+B*C<->E     4
      4       A*c+B*C<->E     4
      11          A+B<->E     2
      12            A<->E     1
      101       A+B*C<->E     4
      
      $`A*c+A*D+B*C<->E`
                   model score
      7      A*D+B*C<->E     2
      8      A*c+B*C<->E     4
      10       A+B*C<->E     3
      11         A+B<->E     2
      12           A<->E     1
      81 A*c+A*D+B*C<->E     6
      
      $`A*c+B*C<->E`
                   model score
      1  A*c+A*D+B*C<->E     4
      10       A+B*C<->E     3
      11         A+B<->E     2
      12           A<->E     1
      91     A*c+B*C<->E     6
      
      $`A*D+B*C<->E`
                   model score
      1  A*B+A*D+B*C<->E     2
      2  A*c+A*D+B*C<->E     4
      10       A+B*C<->E     3
      11         A+B<->E     2
      12           A<->E     1
      71     A*D+B*C<->E     2
      
      $`A*B+A*D+B*C<->E`
                   model score
      7      A*D+B*C<->E     2
      10       A+B*C<->E     3
      11         A+B<->E     2
      12           A<->E     1
      51 A*B+A*D+B*C<->E     2
      
      $`A*B+A*c+A*D<->E`
                   model score
      11         A+B<->E     2
      12           A<->E     1
      61 A*B+A*c+A*D<->E     2
      
      $`(E<->A)*(A+B<->E)`
           model score
      11 A+B<->E     2
      12   A<->E     1
      13   E<->A     1
      
      $`E<->A`
                        model score
      4     (E<->A)*(A+B<->E)     1
      2   (E<->A)*(A+C*D<->E)     1
      3   (E<->A)*(B+C*D<->E)     1
      1 (c*E+D*E<->A)*(A<->E)     1
      
      $`(E<->A)*(A+C*D<->E)`
         model score
      12 A<->E     1
      13 E<->A     1
      
      $`(c*E+D*E<->A)*(A<->E)`
         model score
      12 A<->E     1
      13 E<->A     1
      
      $`(E<->A)*(B+C*D<->E)`
         model score
      13 E<->A     1
      

---

    Code
      frscored_cna(d.pban)
    Output
      0 / 16 reanalyses completed 1 / 16 reanalyses completed 2 / 16 reanalyses completed 3 / 16 reanalyses completed 4 / 16 reanalyses completed 5 / 16 reanalyses completed 6 / 16 reanalyses completed 7 / 16 reanalyses completed 8 / 16 reanalyses completed 9 / 16 reanalyses completed 10 / 16 reanalyses completed 11 / 16 reanalyses completed 
    Condition
      Warning in `cna::csf()`:
      Not all csf solutions have been recorded. csf() with a higher value of n.init might find more solutions.
    Output
      12 / 16 reanalyses completed 13 / 16 reanalyses completed 14 / 16 reanalyses completed 15 / 16 reanalyses completed 
    Condition
      Warning in `cna::csf()`:
      Not all csf solutions have been recorded. csf() with a higher value of n.init might find more solutions.
    Output
      16 / 16 reanalyses completed
      
      processing 687 unique model types, maxsols set to 50 
      
      FR-scored reanalysis series with fit range 1 to 0.7 with granularity 0.1 
      Score type: full || score normalization: truemax 
      maxsols set to 50 -- 637 model types excluded from scoring 
      
      ----- 
       
      Model types: 
       
                  outcome
      1  PB=1            
      2  T=0,T=2,V=0     
      3  PB=1,T=2        
      4  PB=1,T=2        
      5  PB=1            
      6  T=0,T=2,V=0     
      7  PB=1,T=0,T=2,V=0
      8  PB=1,T=0,T=2,V=0
      9  PB=1,T=0,T=2,V=0
      10 PB=1,T=0,T=2,V=0
      11 PB=1,T=0,T=2,V=0
      12 PB=1            
      13 PB=1            
      14 PB=1            
      15 PB=1,T=0,T=2,V=0
      16 PB=1,T=2        
      17 PB=1            
      18 T=0,T=2,V=0     
      19 T=0,T=2,V=0     
      20 PB=1,T=0,T=2,V=0
                                                                          condition
      1                                                              C=1+T=2<->PB=1
      2                           (C=0*F=0+C=1*F=0<->T=0)*(PB=1<->T=2)*(PB=1<->V=0)
      3                                        (C=1+C=2+F=2<->PB=1)*(F=1+F=2<->T=2)
      4                                            (C=1+T=2<->PB=1)*(F=1+F=2<->T=2)
      5                                                              T=1+T=2<->PB=1
      6                          (C=1*F=0+F=0*PB=0<->T=0)*(PB=1<->T=2)*(PB=1<->V=0)
      7       (C=1+T=2<->PB=1)*(C=0*F=0+C=1*F=0<->T=0)*(F=1+F=2<->T=2)*(PB=1<->V=0)
      8          (C=1+T=2<->PB=1)*(C=0*F=0+C=1*F=0<->T=0)*(PB=1<->T=2)*(PB=1<->V=0)
      9      (C=1+T=2<->PB=1)*(C=1*F=0+C=0*PB=0<->T=0)*(F=1+F=2<->T=2)*(PB=1<->V=0)
      10        (C=1+T=2<->PB=1)*(C=1*F=0+C=0*PB=0<->T=0)*(PB=1<->T=2)*(PB=1<->V=0)
      11        (C=1+T=2<->PB=1)*(C=1*F=0+F=0*PB=0<->T=0)*(PB=1<->T=2)*(PB=1<->V=0)
      12                                             C=1+F=2+C=2*F=0+C=0*F=1<->PB=1
      13                                                         C=1+T=1+T=2<->PB=1
      14                                                             F=2+V=0<->PB=1
      15 (C=1+C=2+F=2<->PB=1)*(C=1*F=0+F=0*PB=0<->T=0)*(F=1+F=2<->T=2)*(PB=1<->V=0)
      16                                           (F=1+F=2<->PB=1)*(F=1+F=2<->T=2)
      17                                                         C=1+F=2+T=1<->PB=1
      18                         (C=1*F=0+C=0*PB=0<->T=0)*(PB=1<->T=2)*(PB=1<->V=0)
      19                      (C=1*F=0+F=0*PB=0<->T=0)*(F=1+F=2<->T=2)*(PB=1<->V=0)
      20  (C=1+C=2+F=2<->PB=1)*(C=0*F=0+C=1*F=0<->T=0)*(F=1+F=2<->T=2)*(PB=1<->V=0)
         consistency  coverage complexity inus score tokens norm.score
      1    0.9500000 0.9047619          2 TRUE    18      2  1.0000000
      2    0.7380952 0.9393939          6 TRUE     8      2  0.4444444
      3    0.8048780 0.9285714          5 TRUE     8      2  0.4444444
      4    0.8048780 0.9047619          4 TRUE     8      2  0.4444444
      5    0.9318182 0.9761905          2 TRUE     8      2  0.4444444
      6    0.7380952 0.9393939          6 TRUE     6      2  0.3333333
      7    0.7380952 0.9047619          9 TRUE     6      2  0.3333333
      8    0.7380952 0.9047619          8 TRUE     6      2  0.3333333
      9    0.7380952 0.9047619          9 TRUE     6      2  0.3333333
      10   0.7380952 0.9047619          8 TRUE     6      2  0.3333333
      11   0.7380952 0.9047619          8 TRUE     6      2  0.3333333
      12   1.0000000 0.9047619          6 TRUE     6      2  0.3333333
      13   0.9333333 1.0000000          3 TRUE     6      2  0.3333333
      14   0.9500000 0.9047619          2 TRUE     6      2  0.3333333
      15   0.7380952 0.9285714         10 TRUE     5      2  0.2777778
      16   0.8048780 0.9285714          4 TRUE     5      2  0.2777778
      17   0.9722222 0.8333333          3 TRUE     5      1  0.2777778
      18   0.7380952 0.9393939          6 TRUE     4      2  0.2222222
      19   0.7380952 0.9393939          7 TRUE     4      1  0.2222222
      20   0.7380952 0.9285714         10 TRUE     4      2  0.2222222
      
      ...there were 30 more scored model types, use 'print.all = TRUE' to print all 

---

    Code
      frscored_cna(d.jobsecurity, fit.range = c(0.8, 0.7), granularity = 0.1,
      outcome = "JSR")
    Output
      0 / 4 reanalyses completed 1 / 4 reanalyses completed 2 / 4 reanalyses completed 3 / 4 reanalyses completed 4 / 4 reanalyses completed
      
      processing 742 unique model types, maxsols set to 50 
      
      FR-scored reanalysis series with fit range 0.8 to 0.7 with granularity 0.1 
      Score type: full || score normalization: truemax 
      maxsols set to 50 -- 692 model types excluded from scoring 
      
      ----- 
       
      Model types: 
       
         outcome               condition consistency coverage complexity inus score
      1  JSR                     R<->JSR   0.7910000    0.791          1 TRUE    55
      2  JSR                   C+R<->JSR   0.7218494    0.968          2 TRUE    40
      3  JSR                     L<->JSR   0.7933333    0.714          1 TRUE    32
      4  JSR                   L+R<->JSR   0.7094543    0.923          2 TRUE    26
      5  JSR                   C+L<->JSR   0.7325175    0.838          2 TRUE    16
      6  JSR               C*L+R*p<->JSR   0.8308227    0.717          4 TRUE    10
      7  JSR       C*L+C*R+L*R+R*p<->JSR   0.8153693    0.817          8 TRUE     7
      8  JSR       C*L+C*R+L*R+R*v<->JSR   0.8005698    0.843          8 TRUE     7
      9  JSR           C*L+C*R+L*R<->JSR   0.8413242    0.737          6 TRUE     7
      10 JSR       C*L+C*V+R*p+R*V<->JSR   0.8048780    0.858          8 TRUE     7
      11 JSR       C*L+L*R+R*p+R*v<->JSR   0.8117409    0.802          8 TRUE     7
      12 JSR               C*L+R*v<->JSR   0.8148148    0.726          4 TRUE     7
      13 JSR         S*L+c*R+C*r*v<->JSR   0.8276923    0.807          7 TRUE     7
      14 JSR               S*R+C*L<->JSR   0.8232469    0.857          4 TRUE     7
      15 JSR           C*L+C*V+L*R<->JSR   0.8326446    0.806          6 TRUE     6
      16 JSR       C*L+L*R+L*v+R*p<->JSR   0.8040201    0.800          8 TRUE     6
      17 JSR       C*L+L*R+R*p+R*V<->JSR   0.8390093    0.813          8 TRUE     6
      18 JSR     C*L+L*R+R*p+s*C*r<->JSR   0.8165503    0.819          9 TRUE     6
      19 JSR     C*L+L*R+R*v+s*C*r<->JSR   0.8017078    0.845          9 TRUE     6
      20 JSR               C*L+c*R<->JSR   0.8237052    0.827          4 TRUE     6
         tokens norm.score
      1       1  1.0000000
      2       1  0.7272727
      3       1  0.5818182
      4       1  0.4727273
      5       1  0.2909091
      6       1  0.1818182
      7       1  0.1272727
      8       1  0.1272727
      9       1  0.1272727
      10      1  0.1272727
      11      1  0.1272727
      12      1  0.1272727
      13      2  0.1272727
      14      2  0.1272727
      15      1  0.1090909
      16      1  0.1090909
      17      1  0.1090909
      18      1  0.1090909
      19      1  0.1090909
      20      2  0.1090909
      
      ...there were 30 more scored model types, use 'print.all = TRUE' to print all 

