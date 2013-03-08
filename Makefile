#
#  Top Level Makefile for LAPACK
#  Version 3.1.1
#  February 2007
#

include make.inc

all: lapack_install lib 

test: lapack_testing

lib: lapacklib tmglib

clean: cleanlib cleantesting

lapack_install:
	( cd INSTALL; $(MAKE); ./testlsame; ./testslamch; \
	  ./testdlamch; ./testsecond; ./testdsecnd; ./testversion )


lapacklib:	lapack_install
	( cd SRC; $(MAKE) )
	( cd WRAPPERS; make)

tmglib:
	( cd TESTING/MATGEN; $(MAKE) )

lapack_testing:	lib
	( cd TESTING ; $(MAKE) )


cleanlib:
	( cd INSTALL; $(MAKE) clean )
	( cd SRC; $(MAKE) clean )
	( cd WRAPPERS; $(MAKE) clean )	
	( cd TESTING/MATGEN; $(MAKE) clean )


cleantesting:
	( cd TESTING/LIN; $(MAKE) clean )
	( cd TESTING/EIG; $(MAKE) clean )
	( cd TESTING; rm -f xlin* xeig* )

cleanall: cleanlib cleantesting 
	rm -f *.a *.so TESTING/*.out INSTALL/test*

