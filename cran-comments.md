## This is a resubmission that implements minor fixes described below

* Due to changes in the package 'cna' which 'frscore' depends on, one of the examples in the documentation 
would result in error. This is now fixed and the example runs as intended.

* The function frscore() would occasionally give the warning '[...] cannot xtfrm data frames' on R versions 4.0 and above. This did not affect the intended behavior of the function, but was potentially confusing to the user. The function has been changed so that it avoids this warning.  

* In addition, recent attempt to resubmit had following issues: doi instead of URL in README.md, leading zero in version number. Sorry to have caused extra work here, these are now fixed as instructed: DOI changed to full URL, and version number changed to 0.1.1.

## Test environments 
* local Windows 10 installation, R 4.1.2 
* MacOS (Github Actions), R 4.1.2 
* Ubuntu 20.04 (Github Actions), R 4.1.2 
* Ubuntu 20.04 (Github Actions), devel

## R CMD check results
0 errors | 0 warnings | 0 notes


