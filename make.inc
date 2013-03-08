####################################################################
#  LAPACK make include file.                                       #
#  LAPACK, Version 3.1.1                                           #
#  February 2007                                                   #
####################################################################
#
# See the INSTALL/ directory for more examples.
#
SHELL = /bin/sh
#
#  The machine (platform) identifier to append to the library names
#
PLAT = 
#  
#  Modify the FORTRAN and OPTS definitions to refer to the
#  compiler and desired compiler options for your machine.  NOOPT
#  refers to the compiler options desired when NO OPTIMIZATION is
#  selected.  Define LOADER and LOADOPTS to refer to the loader and 
#  desired load options for your machine.
#
FORTRAN  = xlf_r
CC       = xlc_r
OPTS     = -qpic -O3   -Q-dlamch:slamch:dlamc3:slamc3 -q64  -qmaxmem=-1  -qarch=pwr6 -qcache=auto  -qtune=pwr6  \
           -qnosave -qstrict \
           -qhot=simd -qenablevmx -qsclk=micro \
           -qhot=vector -qessl

DRVOPTS  = $(OPTS)
NOOPT    = -O0   -qpic -Q-dlamch:slamch:dlamc3:slamc3 -q64  -qmaxmem=-1  -qarch=pwr6 -qcache=auto  -qtune=pwr6  \
           -qnosave -qstrict 
	   
LOADER   = xlf_r
LOADOPTS = -q64  -qmaxmem=-1  -qarch=pwr6 -qcache=auto  -qtune=pwr6  \
           -qnosave -qstrict \
           -qhot=simd -qenablevmx -qsclk=micro \
           -qhot=vector -qessl -Wl,--no-allow-shlib-undefined -lxlsmp -lgcc_s
	   
	   
#WLD =xlf_r 
#WLDOPTS= -Wl,--as-needed -qmkshrobj -qpic -q64  -qarch=pwr6 -qcache=auto  -qtune=pwr6\
#  -qhot=simd -qenablevmx -qhot=vector 
WLD =ld 
WLDOPTS= -L/sara/sw/gcc/4.2.1/lib/gcc/powerpc64-unknown-linux-gnu/4.2.1 \
--eh-frame-hdr -V -R/sara/sw/fftw3/3.1.2/lib -R/usr/local/ihpct_2.2/lib -R/usr/local/ihpct_2.2/lib64 -R/usr/local/perfctr/lib -R/usr/local/perfctr/lib64 -Qy -shared -m elf64ppc -rpath /sara/sw/gcc/4.2.1/lib64 \
-shared -L/opt/ibmcmp/xlsmp/1.8/lib64/ -L/opt/ibmcmp/xlf/12.1/lib64/ \
-lxlf90_r -lxlfmath -lxlf90 -lxlopt  -lxl -lxlfmath \
--as-needed -lxlomp_ser -ldl -lrt -lpthread -lm -lc  -lc --no-as-needed
#-lgcc_s -Wl,--no-allow-shlib-undefined 
#  -L/opt/ibmcmp/xlf/12.1/lib64/ -lxlf90_r -lxlfmath
#
# Timer for the SECOND and DSECND routines
#
# Default : SECOND and DSECND will use a call to the EXTERNAL FUNCTION ETIME
#TIMER    = EXT_ETIME
# For RS6K : SECOND and DSECND will use a call to the EXTERNAL FUNCTION ETIME_
 TIMER    = EXT_ETIME_
# For gfortran compiler: SECOND and DSECND will use a call to the INTERNAL FUNCTION ETIME
# TIMER    = INT_ETIME 
# If your Fortran compiler does not provide etime (like Nag Fortran Compiler, etc...)
# SECOND and DSECND will use a call to the INTERNAL FUNCTION CPU_TIME 
# TIMER    = INT_CPU_TIME
# If neither of this works...you can use the NONE value... In that case, SECOND and DSECND will always return 0
# TIMER     = NONE
#
#  The archiver and the flag(s) to use when building archive (library)
#  If you system has no ranlib, set RANLIB = echo.
#
ARCH     = ar
ARCHFLAGS= cr
RANLIB   = ranlib
#
#  The location of the libraries to which you will link.  (The 
#  machine-specific, optimized BLAS library should be used whenever
#  possible.)
#
BLASLIB      = -lessl
LAPACKLIB    = lapack11.a
LAPACKSO     = libelapack.so
LAPACKSOSMP     = libelapacksmp.so
LDLAPACK    = -L../../ -lelapack
LDLAPACKSMP    = -L../../ -lelapacksmp

TMGLIB       = tmglib$(PLAT).a
EIGSRCLIB    = eigsrc$(PLAT).a
LINSRCLIB    = linsrc$(PLAT).a
