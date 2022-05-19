# frscored_cna works

    Code
      frscored_cna(d.error)
    Output
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
      

