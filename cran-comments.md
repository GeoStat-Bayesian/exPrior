## Local test environment
* Ubuntu Linux 18.04 LTS, R 3.6.1

## R CMD check results
There were no ERRORs no WARNINGs no NOTEs. 

## R-hub test environments
* Fedora Linux, R-devel, clang, gfortran
* Ubuntu Linux 16.04 LTS, R-release, GCC
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit

## R-hub check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Falk Hesse <falk.hesse@ufz.de>'

A few times (mostly not though) there was an ERROR on the
Fedora Linux platform

* checking package dependencies ... ERROR
Package suggested but not available: ‘devtools’

This has to be a problem with the testing system on
the platform since ‘devtools’ is a very common package

## misc notes

I did not manage to get the example for the genExPrior
function even close to 5 seconds. To still pass check_rhub(),
I wrapped the example in \donttest. I had to do the same
to the plotExPrior and the plotHyperDist function examples
since they use the output from the genExPrior function.
