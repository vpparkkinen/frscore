# frscore works for mv

    Code
      frscore(mv_all[[1]][[1]])
    Output
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
      
      

---

    Code
      frscore(mv_all[[2]][[1]])
    Output
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
      
      

---

    Code
      frscore(mv_all[[3]][[1]])
    Output
      FRscore, score type: full || score normalization: truemax 
      
      maxsols set to 50 -- 0 solution types excluded from scoring 
      
      -----
       
      Model types: 
      
                                                                                                       model
      1                                                                                A=1*D=1+A=3*C=1<->B=1
      2                                                      (C=3*E=3+D=3*E=3<->A=3)*(A=1*D=1+A=3*C=1<->B=1)
      3                          (B=1*C=1*D=3+B=1*D=2+C=3*E=3+D=3*E=3<->A=3)*(A=1*D=1+A=3*C=1+A=3*E=1<->B=1)
      4                                                                                A=1*D=1+A=3*E=1<->B=1
      5                                          (B=1*C=1*D=3+C=3*E=3+D=3*E=3<->A=3)*(A=1*D=1+A=3*E=1<->B=1)
      6                                                  (B=1*C=1*D=3+C=3*E=3<->A=3)*(A=1*D=1+A=3*E=1<->B=1)
      7                                              (B=1*D=2+C=3*E=3+D=3*E=3<->A=3)*(A=1*D=1+A=3*C=1<->B=1)
      8                                              (B=1*D=2+C=3*E=3+D=3*E=3<->A=3)*(A=1*D=1+A=3*E=1<->B=1)
      9      (B=2*E=1+B=2*E=2+B=3*C=1+B=3*C=2<->A=2)*(B=1*D=2+C=3*E=3+D=3*E=3<->A=3)*(A=1*D=1+A=3*C=1<->B=1)
      10                                                                       A=1*D=1+A=3*C=1+A=3*E=1<->B=1
      11                                     (B=1*C=1*D=3+B=2*C=3*E=3+D=3*E=3<->A=3)*(A=1*D=1+A=3*E=1<->B=1)
      12                                         (B=1*D=2+B=2*C=3*E=3+D=3*E=3<->A=3)*(A=1*D=1+A=3*C=1<->B=1)
      13 (B=2*E=1+B=2*E=2+B=3*C=1+B=3*C=2<->A=2)*(B=1*C=1*D=3+C=3*E=3+D=3*E=3<->A=3)*(A=1*D=1+A=3*E=1<->B=1)
      14             (B=2*E=1+B=2*E=2+B=3*C=1+B=3*C=2<->A=2)*(C=3*E=3+D=3*E=3<->A=3)*(A=1*D=1+A=3*C=1<->B=1)
      15                                         (B=1*D=2+B=2*C=3*E=3+D=3*E=3<->A=3)*(A=1*D=1+A=3*E=1<->B=1)
      16             (B=2*C=1+B=2*E=1+B=2*E=2+B=3*C=2<->A=2)*(C=3*E=3+D=3*E=3<->A=3)*(A=1*D=1+A=3*C=1<->B=1)
      17             (B=2*C=1+B=2*E=1+B=2*E=2+B=3*E=1<->A=2)*(C=3*E=3+D=3*E=3<->A=3)*(A=1*D=1+A=3*C=1<->B=1)
      18             (B=2*C=1+B=2*E=1+B=3*C=1+B=3*C=2<->A=2)*(C=3*E=3+D=3*E=3<->A=3)*(A=1*D=1+A=3*C=1<->B=1)
      19             (B=2*C=1+B=2*E=1+B=3*C=2+B=3*E=1<->A=2)*(C=3*E=3+D=3*E=3<->A=3)*(A=1*D=1+A=3*C=1<->B=1)
      20             (B=2*C=1+B=2*E=2+B=3*C=1+B=3*C=2<->A=2)*(C=3*E=3+D=3*E=3<->A=3)*(A=1*D=1+A=3*C=1<->B=1)
         score tokens norm.score
      1     18      1  1.0000000
      2     16      1  0.8888889
      3     10      2  0.5555556
      4     10      1  0.5555556
      5      6      1  0.3333333
      6      6      1  0.3333333
      7      6      1  0.3333333
      8      5      1  0.2777778
      9      4      1  0.2222222
      10     4      1  0.2222222
      11     3      1  0.1666667
      12     3      1  0.1666667
      13     3      1  0.1666667
      14     3      1  0.1666667
      15     2      1  0.1111111
      16     2      1  0.1111111
      17     2      1  0.1111111
      18     2      1  0.1111111
      19     2      1  0.1111111
      20     2      1  0.1111111
      
      ...there were 8 more model types, use 'print.all = TRUE' to print all 
      

