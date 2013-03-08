include ../make.inc
OBJ=sgetrf.o cgetrf.o dgetrf.o zgetrf.o \
    sgetri.o cgetri.o dgetri.o zgetri.o \
    sgetrs.o cgetrs.o dgetrs.o zgetrs.o \
    sgesv.o  cgesv.o dgesv.o zgesv.o \
    spotrf.o cpotrf.o dpotrf.o zpotrf.o \
    spotri.o cpotri.o dpotri.o zpotri.o \
    spotrs.o cpotrs.o dpotrs.o zpotrs.o \
    sposv.o cposv.o dposv.o zposv.o \
    spptrf.o cpptrf.o dpptrf.o zpptrf.o \
    spptrs.o cpptrs.o dpptrs.o zpptrs.o \
    sppsv.o cppsv.o dppsv.o zppsv.o \
    strtri.o ctrtri.o dtrtri.o ztrtri.o \
    stptri.o ctptri.o dtptri.o ztptri.o \
    spptri.o dpptri.o \
    dgels.o \
    dgeqrf.o \



#LDFLAGS= $(WLDOPTS) -Wl,-shared -Wl,-init -Wl,_init_essl_proxy   -ldl -lm -lessl -lm 
#LDFLAGSSMP= $(WLDOPTS) -Wl,-shared -Wl,-init -Wl,_init_essl_proxy   -ldl -lm -lesslsmp -lpthread -lm -lxlsmp
LDFLAGS= $(WLDOPTS) -shared -init _init_essl_proxy   -ldl -lm -lessl -lm 
LDFLAGSSMP= $(WLDOPTS) -shared -init _init_essl_proxy   -ldl -lm -lesslsmp -lpthread -lm -lxlsmp

all: ../$(LAPACKSO) ../$(LAPACKSOSMP)

clean:
	rm -f *.o ../$(LAPACKSO)

../$(LAPACKSO): $(OBJ) essl_proxy.c
	$(CC) $(OPTS) -c essl_proxy.c
	$(WLD)  $(LDFLAGS) -o ../$(LAPACKSO) $(OBJ) --whole-archive ../SRC/lapack.a --no-whole-archive essl_proxy.o
#	$(WLD)  $(LDFLAGS) -o ../$(LAPACKSO) $(OBJ) -Wl,--whole-archive ../SRC/lapack.a -Wl,--no-whole-archive essl_proxy.o

../$(LAPACKSOSMP): $(OBJ) essl_proxy.c
	$(CC) $(OPTS) -Wp,-DESSL_SMP -c essl_proxy.c
	$(WLD)   $(LDFLAGSSMP) -o ../$(LAPACKSOSMP) $(OBJ) --whole-archive ../SRC/lapack.a --no-whole-archive essl_proxy.o
#	$(WLD)   $(LDFLAGSSMP) -o ../$(LAPACKSOSMP) $(OBJ) -Wl,--whole-archive ../SRC/lapack.a -Wl,--no-whole-archive essl_proxy.o

.f.o: 
	$(FORTRAN) $(OPTS) -c $< -o $@

.c.o: 
	$(CC) $(OPTS) -c $< -o $@


