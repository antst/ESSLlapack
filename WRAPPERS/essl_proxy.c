#include <stdlib.h>
#include <dlfcn.h>


#define essl_name(name) __essl_ ## name ## _ptr

#define ESSL_LOAD(name) *(void **) (&essl_name(name))=dlsym(__essl_library_handler,#name);\
if (essl_name(name)==NULL){error_string=dlerror();\
	printf("ESSL Wrapper Error!\n"); \
	printf("%s\n",error_string);\	
	exit(1);}\
if (*(void **) (&essl_name(name))==dlsym(NULL,#name)){\
	printf("Error!!! Symbol \"%s\" already loaded from ESSL!\n",#name);\
	printf("elapack library must be linked before ESSL! \n");\	
	exit(1);}

#define _PDEBUG
// printf("goes via proxy\n");

#define ESSL_EXPORT(name,argdef,args)\
void (*essl_name(name)) argdef;\
	void essl_##name argdef{_PDEBUG;(*__essl_##name##_ptr) args ;};\
	void essl_##name##_ argdef{_PDEBUG;(*__essl_##name##_ptr) args ;};\


void *__essl_library_handler=NULL;


ESSL_EXPORT(sspev,(),());
ESSL_EXPORT(chpev,(),());
ESSL_EXPORT(dspev,(),());
ESSL_EXPORT(zhpev,(),());
ESSL_EXPORT(sspsv,(),());
ESSL_EXPORT(chpsv,(),());
ESSL_EXPORT(dspsv,(),());
ESSL_EXPORT(zhpsv,(),());
ESSL_EXPORT(sgegv,(),());
ESSL_EXPORT(dgegv,(),());
ESSL_EXPORT(ssygv,(),());
ESSL_EXPORT(dsygv,(),());


ESSL_EXPORT(dgeev,
    (void *iopt, void *a, void *lda, void *w, void *z, void *ldz, void *select,void *n, void *aux, void *naux),
    (iopt, a, lda, w, z, ldz, select, n, aux, naux));

ESSL_EXPORT(zgeev,
    (int *iopt, double *a, int *lda, double *w, double *z, int *ldz, int * select,int *n, double *aux, int *naux),
    (iopt, a, lda, w, z, ldz, select, n, aux, naux));

ESSL_EXPORT(sgeev,
    (int *iopt, float *a, int *lda, float *w, float *z, int *ldz, int * select,int *n, float *aux, int *naux),
    (iopt, a, lda, w, z, ldz, select, n, aux, naux));

ESSL_EXPORT(cgeev,
    (int *iopt, float *a, int *lda, float *w, float *z, int *ldz, int * select,int *n, float *aux, int *naux),
    (iopt, a, lda, w, z, ldz, select, n, aux, naux));


ESSL_EXPORT(zgetrf,
    (int * M, int *N, double *A, int * LDA, int * IPIV, int * INFO ),
    ( M, N, A, LDA, IPIV, INFO ));

ESSL_EXPORT(dgetrf,
    (int * M, int *N, double *A, int * LDA, int * IPIV, int * INFO ),
    ( M, N, A, LDA, IPIV, INFO ));

ESSL_EXPORT(cgetrf,
    (int * M, int *N, float *A, int * LDA, int * IPIV, int * INFO ),
    ( M, N, A, LDA, IPIV, INFO ));

ESSL_EXPORT(sgetrf,
    (int * M, int *N, float *A, int * LDA, int * IPIV, int * INFO ),
    ( M, N, A, LDA, IPIV, INFO ));

ESSL_EXPORT(sgetri,
    (int * N, float *A, int *LDA, int *IPIV, float *WORK, int * LWORK, int *INFO ),
    ( N, A, LDA, IPIV, WORK, LWORK, INFO ));

ESSL_EXPORT(cgetri,
    (int * N, float *A, int *LDA, int *IPIV, float *WORK, int * LWORK, int *INFO ),
    ( N, A, LDA, IPIV, WORK, LWORK, INFO ));

ESSL_EXPORT(dgetri,
    (int * N, double *A, int *LDA, int *IPIV, double *WORK, int * LWORK, int *INFO ),
    ( N, A, LDA, IPIV, WORK, LWORK, INFO ));

ESSL_EXPORT(zgetri,
    (int * N, double *A, int *LDA, int *IPIV, double *WORK, int * LWORK, int *INFO ),
    ( N, A, LDA, IPIV, WORK, LWORK, INFO ));

ESSL_EXPORT(sgetrs,
    (char * TRANS, int * N, int * NRHS, float *A, int *LDA, int *IPIV, float *B, int *LDB, int *INFO ),
    ( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO ));

ESSL_EXPORT(cgetrs,
    (char * TRANS, int * N, int * NRHS, float *A, int *LDA, int *IPIV, float *B, int *LDB, int *INFO ),
    ( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO ));

ESSL_EXPORT(dgetrs,
    (char * TRANS, int * N, int * NRHS, double *A, int *LDA, int *IPIV, double *B, int *LDB, int *INFO ),
    ( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO ));

ESSL_EXPORT(zgetrs,
    (char * TRANS, int * N, int * NRHS, double *A, int *LDA, int *IPIV, double *B, int *LDB, int *INFO ),
    ( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO ));

ESSL_EXPORT(sgesv,
    (int * N,int * NRHS, float *A,int * LDA,int * IPIV, float *B,int * LDB,int * INFO ),
    ( N, NRHS, A, LDA, IPIV, B, LDB, INFO ));

ESSL_EXPORT(cgesv,
    (int * N,int * NRHS, float *A,int * LDA,int * IPIV, float *B,int * LDB,int * INFO ),
    ( N, NRHS, A, LDA, IPIV, B, LDB, INFO ));
    
ESSL_EXPORT(dgesv,
    (int * N,int * NRHS, double *A,int * LDA,int * IPIV, double *B,int * LDB,int * INFO ),
    ( N, NRHS, A, LDA, IPIV, B, LDB, INFO ));

ESSL_EXPORT(zgesv,
    (int * N,int * NRHS, double *A,int * LDA,int * IPIV, double *B,int * LDB,int * INFO ),
    ( N, NRHS, A, LDA, IPIV, B, LDB, INFO ));

ESSL_EXPORT(cpotrf,
    (void * UPLO,void * N,void * A, void *LDA, void *INFO ),
    ( UPLO, N, A, LDA, INFO ));

ESSL_EXPORT(spotrf,
    (void * UPLO,void * N,void * A, void *LDA, void *INFO ),
    ( UPLO, N, A, LDA, INFO ));

ESSL_EXPORT(dpotrf,
    (void * UPLO,void * N,void * A, void *LDA, void *INFO ),
    ( UPLO, N, A, LDA, INFO ));

ESSL_EXPORT(zpotrf,
    (void * UPLO,void * N,void * A, void *LDA, void *INFO ),
    ( UPLO, N, A, LDA, INFO ));

ESSL_EXPORT(spotri,
    (void * UPLO,void * N,void * A, void *LDA, void *INFO ),
    ( UPLO, N, A, LDA, INFO ));

ESSL_EXPORT(cpotri,
    (void * UPLO,void * N,void * A, void *LDA, void *INFO ),
    ( UPLO, N, A, LDA, INFO ));

ESSL_EXPORT(dpotri,
    (void * UPLO,void * N,void * A, void *LDA, void *INFO ),
    ( UPLO, N, A, LDA, INFO ));

ESSL_EXPORT(zpotri,
    (void * UPLO,void * N,void * A, void *LDA, void *INFO ),
    ( UPLO, N, A, LDA, INFO ));

ESSL_EXPORT(spotrs,
    ( void *UPLO, void *N, void *NRHS, void *A, void *LDA, void *B, void *LDB, void *INFO ),
    ( UPLO, N, NRHS, A, LDA, B, LDB, INFO ));

ESSL_EXPORT(cpotrs,
    ( void *UPLO, void *N, void *NRHS, void *A, void *LDA, void *B, void *LDB, void *INFO ),
    ( UPLO, N, NRHS, A, LDA, B, LDB, INFO ));

ESSL_EXPORT(dpotrs,
    ( void *UPLO, void *N, void *NRHS, void *A, void *LDA, void *B, void *LDB, void *INFO ),
    ( UPLO, N, NRHS, A, LDA, B, LDB, INFO ));

ESSL_EXPORT(zpotrs,
    ( void *UPLO, void *N, void *NRHS, void *A, void *LDA, void *B, void *LDB, void *INFO ),
    ( UPLO, N, NRHS, A, LDA, B, LDB, INFO ));

ESSL_EXPORT(sposv,
    ( void *UPLO, void *N, void *NRHS, void *A, void *LDA, void *B, void *LDB, void *INFO ),
    ( UPLO, N, NRHS, A, LDA, B, LDB, INFO ));

ESSL_EXPORT(cposv,
    ( void *UPLO, void *N, void *NRHS, void *A, void *LDA, void *B, void *LDB, void *INFO ),
    ( UPLO, N, NRHS, A, LDA, B, LDB, INFO ));

ESSL_EXPORT(dposv,
    ( void *UPLO, void *N, void *NRHS, void *A, void *LDA, void *B, void *LDB, void *INFO ),
    ( UPLO, N, NRHS, A, LDA, B, LDB, INFO ));

ESSL_EXPORT(zposv,
    ( void *UPLO, void *N, void *NRHS, void *A, void *LDA, void *B, void *LDB, void *INFO ),
    ( UPLO, N, NRHS, A, LDA, B, LDB, INFO ));

ESSL_EXPORT(spptrf,
    ( void *UPLO, void *N, void *AP, void *INFO ),
    ( UPLO, N, AP, INFO ));
    
ESSL_EXPORT(cpptrf,
    ( void *UPLO, void *N, void *AP, void *INFO ),
    ( UPLO, N, AP, INFO ));
    
ESSL_EXPORT(dpptrf,
    ( void *UPLO, void *N, void *AP, void *INFO ),
    ( UPLO, N, AP, INFO ));
    
ESSL_EXPORT(zpptrf,
    ( void *UPLO, void *N, void *AP, void *INFO ),
    ( UPLO, N, AP, INFO ));

ESSL_EXPORT(dpptri,
    ( void *UPLO, void *N, void *AP, void *INFO ),
    ( UPLO, N, AP, INFO ));

ESSL_EXPORT(spptri,
    ( void *UPLO, void *N, void *AP, void *INFO ),
    ( UPLO, N, AP, INFO ));

ESSL_EXPORT(spptrs,    
    ( void *UPLO, void *N, void *NRHS, void *AP, void *B, void *LDB, void *INFO ),
    ( UPLO, N, NRHS, AP, B, LDB, INFO ));

ESSL_EXPORT(cpptrs,    
    ( void *UPLO, void *N, void *NRHS, void *AP, void *B, void *LDB, void *INFO ),
    ( UPLO, N, NRHS, AP, B, LDB, INFO ));

ESSL_EXPORT(dpptrs,    
    ( void *UPLO, void *N, void *NRHS, void *AP, void *B, void *LDB, void *INFO ),
    ( UPLO, N, NRHS, AP, B, LDB, INFO ));

ESSL_EXPORT(zpptrs,    
    ( void *UPLO, void *N, void *NRHS, void *AP, void *B, void *LDB, void *INFO ),
    ( UPLO, N, NRHS, AP, B, LDB, INFO ));

ESSL_EXPORT(sppsv,
    ( void *UPLO, void *N, void *NRHS, void *AP, void *B, void *LDB, void *INFO ),
    ( UPLO, N, NRHS, AP, B, LDB, INFO ));

ESSL_EXPORT(cppsv,
    ( void *UPLO, void *N, void *NRHS, void *AP, void *B, void *LDB, void *INFO ),
    ( UPLO, N, NRHS, AP, B, LDB, INFO ));

ESSL_EXPORT(dppsv,
    ( void *UPLO, void *N, void *NRHS, void *AP, void *B, void *LDB, void *INFO ),
    ( UPLO, N, NRHS, AP, B, LDB, INFO ));

ESSL_EXPORT(zppsv,
    ( void *UPLO, void *N, void *NRHS, void *AP, void *B, void *LDB, void *INFO ),
    ( UPLO, N, NRHS, AP, B, LDB, INFO ));

ESSL_EXPORT(strtri,
    ( void *UPLO, void *DIAG, void *N, void *A, void *LDA, void *INFO ),
    ( UPLO, DIAG, N, A, LDA, INFO ));
    
ESSL_EXPORT(ctrtri,
    ( void *UPLO, void *DIAG, void *N, void *A, void *LDA, void *INFO ),
    ( UPLO, DIAG, N, A, LDA, INFO ));
    
ESSL_EXPORT(dtrtri,
    ( void *UPLO, void *DIAG, void *N, void *A, void *LDA, void *INFO ),
    ( UPLO, DIAG, N, A, LDA, INFO ));
    
ESSL_EXPORT(ztrtri,
    ( void *UPLO, void *DIAG, void *N, void *A, void *LDA, void *INFO ),
    ( UPLO, DIAG, N, A, LDA, INFO ));
    
ESSL_EXPORT(stptri,
    ( void *UPLO, void *DIAG, void *N, void *AP, void *INFO ),
    ( UPLO, DIAG, N, AP, INFO ));

ESSL_EXPORT(ctptri,
    ( void *UPLO, void *DIAG, void *N, void *AP, void *INFO ),
    ( UPLO, DIAG, N, AP, INFO ));

ESSL_EXPORT(dtptri,
    ( void *UPLO, void *DIAG, void *N, void *AP, void *INFO ),
    ( UPLO, DIAG, N, AP, INFO ));

ESSL_EXPORT(ztptri,
    ( void *UPLO, void *DIAG, void *N, void *AP, void *INFO ),
    ( UPLO, DIAG, N, AP, INFO ));
    
ESSL_EXPORT(dgels,    
    ( void *TRANS, void *M, void *N, void *NRHS, void *A, void *LDA, void *B, void *LDB, void *WORK, void *LWORK, void *INFO ),
    ( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK, INFO ));

ESSL_EXPORT(dgeqrf,
    ( void *M, void *N, void *A, void *LDA, void *TAU, void *WORK, void *LWORK, void *INFO ),
    ( M, N, A, LDA, TAU, WORK, LWORK, INFO ));
    
_init_essl_proxy(){
	char *error_string;
	if (__essl_library_handler==NULL){
#if defined(ESSL_SMP)	
		__essl_library_handler=dlopen("libesslsmp.so",RTLD_NOW);
#else
		__essl_library_handler=dlopen("libessl.so",RTLD_NOW);		
#endif
		if (__essl_library_handler==NULL){
			error_string=dlerror();
			printf("ESSL Wrapper Error!\n");
			printf("%s\n",error_string);
			exit(1);
		}

		ESSL_LOAD(zgeev);
		ESSL_LOAD(sgeev);
		ESSL_LOAD(cgeev);
		ESSL_LOAD(dgeev);
		
		ESSL_LOAD(sspev);
		ESSL_LOAD(chpev);
		ESSL_LOAD(dspev);		
		ESSL_LOAD(zhpev);
		
		ESSL_LOAD(sspsv);
		ESSL_LOAD(chpsv);
		ESSL_LOAD(dspsv);		
		ESSL_LOAD(zhpsv);
		
		ESSL_LOAD(sgegv);
		ESSL_LOAD(dgegv);
		ESSL_LOAD(ssygv);			
		ESSL_LOAD(dsygv);
		
		ESSL_LOAD(dgetrf);		
		ESSL_LOAD(zgetrf);
		ESSL_LOAD(sgetrf);		
		ESSL_LOAD(cgetrf);
		
		ESSL_LOAD(dgetri);
		ESSL_LOAD(zgetri);
		ESSL_LOAD(sgetri);		
		ESSL_LOAD(cgetri);

		ESSL_LOAD(dgetrs);
		ESSL_LOAD(zgetrs);
		ESSL_LOAD(sgetrs);		
		ESSL_LOAD(cgetrs);

		ESSL_LOAD(sgesv);
		ESSL_LOAD(cgesv);		
		ESSL_LOAD(dgesv);
		ESSL_LOAD(zgesv);

		ESSL_LOAD(spotrf);
		ESSL_LOAD(cpotrf);		
		ESSL_LOAD(dpotrf);
		ESSL_LOAD(zpotrf);

		ESSL_LOAD(spotri);	
		ESSL_LOAD(cpotri);	
		ESSL_LOAD(dpotri);	
		ESSL_LOAD(zpotri);	

		ESSL_LOAD(spotrs);
		ESSL_LOAD(cpotrs);
		ESSL_LOAD(dpotrs);
		ESSL_LOAD(zpotrs);		
	
		ESSL_LOAD(sposv);
		ESSL_LOAD(cposv);
		ESSL_LOAD(dposv);
		ESSL_LOAD(zposv);

		ESSL_LOAD(spptrf);
		ESSL_LOAD(cpptrf);
		ESSL_LOAD(dpptrf);
		ESSL_LOAD(zpptrf);

		ESSL_LOAD(spptri);
		ESSL_LOAD(dpptri);

		ESSL_LOAD(spptrs);
		ESSL_LOAD(cpptrs);
		ESSL_LOAD(dpptrs);
		ESSL_LOAD(zpptrs);

		ESSL_LOAD(sppsv);
		ESSL_LOAD(cppsv);
		ESSL_LOAD(dppsv);
		ESSL_LOAD(zppsv);

		ESSL_LOAD(strtri);
		ESSL_LOAD(ctrtri);
		ESSL_LOAD(dtrtri);
		ESSL_LOAD(ztrtri);

		ESSL_LOAD(stptri);
		ESSL_LOAD(ctptri);
		ESSL_LOAD(dtptri);
		ESSL_LOAD(ztptri);
		
		ESSL_LOAD(dgels);
		ESSL_LOAD(dgeqrf);		
	}
}

