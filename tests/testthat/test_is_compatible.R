library(testthat)

mvdatgen <- function(x){
  fct <- full.ct(x)
  fct_u <- apply(fct, 2, unique)
  mv_values <- lapply(fct_u,
                      function(x) {if(length(unique(x)) < 3){
                        x <- min(x):(max(x)+(3-length(x)))
                      } else {
                        x <- x
                      }
                        return(x)})
  out <- full.ct(x = mv_values)
  return(out)
}

test_that("is_compatible() works", {
  y <-  "(T+R<->Y)*(A+Y*K<->C)*(C*H+C*I<->E)"
  x <- "H*T+A*I<->E"
  expect_true(is_compatible(x,y))
  x <- "C*H+I*T<->E"
  expect_false(is_compatible(x,y))

  y <- "(C*A+B<->R)*(R+B+X<->Y)*(Y*N+B*n<->L)*(L*a*b+U<->E)"
  x <- "A+B<->E" #FALSE
  expect_false(is_compatible(x,y))

  y <- "(A+B<->C)*(C+X<->E)*(E*R*T*x+X*a*b+Y*a*b<->Z)"
  x <- "A+X<->Z"
  expect_true(is_compatible(x,y))

  y <- "(T+R<->Y)*(A+B<->C)*(t*C+a*Y<->E)"
  x <- "A+T<->E"
  expect_true(is_compatible(x,y))

  y <- "(L+B<->A)*(T+A+R<->Y)*(X+Y<->C)*(T*C+A<->E)"
  x <- "L+T<->E"
  expect_true(is_compatible(x,y))

  y <- "(A+B<->C)*(T+R<->Y)*(C*Y+c*y<->E)"
  x <-  "t*r+C<->E"
  expect_true(is_compatible(x,y))

  y <- "(A*X+B<->C)*(C+A*R<->Y)*(C*a*U+Z*y<->E)"
  x <- "R<->E"
  expect_false(is_compatible(x,y))

  y <- "(A+B*F<->C)*(D+B*f<->E)*(C+E*T<->G)"
  x <- "(A+B*F<->C)*(D+B*f<->E)*(C+E+F<->G)"
  expect_false(is_compatible(x,y))
  x <- "A+B*F+E<->G"
  expect_true(is_compatible(x,y))

  y <- "(D*F+a*b<->C)*(F*c<->G)*(B*f+D*c+a*c<->E)"
  x <-"D*f+d*B<->E"
  expect_true(is_compatible(x,y))

  # y <- "(C*e*f+E*F*c+F*c*g<->B)*(B*e+E*G*c<->D)*(F*G+d*e<->A)"
  # x <- "e*b+F*G<->A"
  # expect_true(is_compatible(x,y))

  y <- "(E*c*d+a*c+a*d*g<->F)*(A*F+D*E*g+a*d*f<->B)"
  x <- "A*F+a*d*G+E*D*g<->B"
  expect_false(is_compatible(x,y))

  y <- "(L+B<->A)*(T+A+R<->Y)*(X+Y<->C)*(C+U<->E)"
  x <- "(L+B<->Y)*(X+Y<->E)"
  expect_true(is_compatible(x,y))

  y <- "(L+B<->A)*(T+A+R<->Y)*(X+Y<->C)*(T*C+A<->E)"
  x <- "(L+T<->A)*(X+Y<->E)"
  expect_false(is_compatible(x,y))

  x <- "L+T<->Y"
  x <- "R+C<->E"
  expect_false(is_compatible(x, y))

  y <- "(C+D<->E)*(A+B<->C)*(E+F<->G)*(G+H+C<->I)"
  x <- "(C+E<->G)*(G+F<->I)"
  expect_false(is_compatible(x,y))

  y <- "(A + B <-> C)*(C + Z <-> F)*(C + D <-> E)"
  x <- "(A + B <-> F)*(A + D <-> E)"
  expect_true(is_compatible(x,y))

  y <- "(A+B*D<->C)*(C+D<->G)"
  x <- "(A+B*D<->C)*(C<->G)"
  expect_false(is_compatible(x,y))

  y <- "(A+B<->C)*(C+D<->E)*(X+F<->G)*(G+H+c<->I)"
  x <- "(G+c<->I)*(C+D<->E)"
  expect_true(is_compatible(x,y))

  y <- "(A+B<->C)*(C+D<->E)*(E+F<->G)*(G+H<->I)"
  x <- "(A+C<->C)*(C+D<->E)"
  expect_false(is_compatible(x,y))

  y <- "(B*D*F*G+B*F*d*g+b*f*g<->A)*(A*D+G*b*d+G*f<->E)"
  x <- "(b*f*g+D*F*E<->A)*(G<->E)"
  expect_false(is_compatible(x,y))

  y <- "(B*D*F*G+B*F*d*g+b*f*g<->A)*(A*D+G*b*d+G*f<->E)"
  x <- "(b*f*g+D*F*G<->A)*(G<->E)"
  expect_true(is_compatible(x,y))

  y <- "(C*D+C*F+C*b<->E)*(B*D*E+C*b*f<->A)"
  x <- "(f*E<->A)*(C<->E)"
  expect_false(is_compatible(x,y))

  y <- "(D*E<->B)*(A*c*g+A*e*g<->D)"
  x <- "(A*c*g*E<->B)*(A*c*g+A*g*e<->D)"
  expect_false(is_compatible(x,y))

  y <- "(C*d*g+b*g<->A)*(B*G<->E)*(C*E*d+D*e+b*g<->F)"
  x <- "(g*b<->A)*(G*B<->E)*(b+D*e<->F)"
  expect_true(is_compatible(x,y))

  y <- "(E*f+F*c*d<->B)*(B*F+D*b*c<->A)*(a*c*d<->G)"
  x <- "(F*c<->A)*(E*f+c*d<->B)*(c*d*a<->G)"
  expect_false(is_compatible(x,y))

  y <- "(F*d+D*G<->A)*(a*f<->C)*(D*F*G<->B)*(g*A<->E)"
  x <- "(G+F*d<->A)*(F*D*G<->B)*(f*a<->C)*(g*A<->E)"
  expect_true(is_compatible(x,y))

  y <- "(A=1+B=2<->C=3)*(C=3+D=5<->E=2)"
  dat <- mvdatgen(y)
  x <- "(A=1+B=2<->E=2)"
  expect_true(is_compatible(x,y, dat = dat))

  y <- "(B=2*D=0+D=1*B=1<->A=0)*(B=1*D=2+A=0*D=0<->C=1)"
  x <- "(B=2*D=0+D=1*B=1<->C=1)"
  dat <- mvdatgen(y)
  expect_false(is_compatible(x,y, dat))

  y <- "(C+D<->E)*(A+B<->C)*(E+F<->A)"
  x <- "(C+D<->E)*(A+B<->C)"
  expect_true(is_compatible(x,y))

  y <- "(A+B*D<->C)*(C+H<->G)*(G+D<->I)"
  x <- "(A+B*D<->C)*(C<->I)"
  expect_false(is_compatible(x,y))

  y <- "(A+B<->C)*(C+D<->E)*(E+F<->G)*(G+H<->I)"
  x <- "(A+B<->E)*(E+H<->I)"
  expect_true(is_compatible(x,y))
})





