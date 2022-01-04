## This is a resubmission that implements minor fixes described below

* One of the examples in the documentation of the function frscored_cna()
started throwing an error after an update to the package 'cna', which 'frscore' depends on. This is now fixed and the example runs as intended.

* The function frscore() would occasionally give the warning '[...] cannot xtfrm data frames' on R versions 4.0 and above. This did not affect the intended behavior of the function, but was potentially confusing to the user. The function has been changed so that it avoids this warning being displayed.  

## Test environments 
* local Windows 10 installation, R 4.1.2 
* MacOS (Github Actions), R 4.1.2 
* Ubuntu 20.04 (Github Actions), R 4.1.2 
* Ubuntu 20.04 (Github Actions), devel

## R CMD check results
0 errors | 0 warnings | 0 notes


