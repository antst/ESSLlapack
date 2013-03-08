      SUBROUTINE SPPTRI( UPLO, N, AP, INFO )
*
*  -- LAPACK routine (version 2.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*  -- ESSL CCI enablement (version 1.1) --
*     Univ. of Tennessee, IBM Kingston and Yorktown,
*     August 1, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, N
*     ..
*     .. Array Arguments ..
      REAL               AP( * )
*     ..
*
*  Purpose
*  =======
*
*  SPPTRI computes the inverse of a real symmetric positive definite
*  matrix A using the Cholesky factorization A = U**T*U or A = L*L**T
*  computed by SPPTRF.
*
*  ESSL Enablement Comments
*  ==== ========== ========
*
*  This is a stub routine that calls the ESSL subroutine SPPICD when the
*  input matrix is stored in lower packed format ONLY (UPLO = 'L').  If
*  UPLO = 'U', SPPTRI is the LAPACK routine SPPTRI.  In all cases, the
*  results returned will be identical in structure to those of the
*  normal LAPACK routine SPPTRI.  Note that this stub will allocate
*  workspace needed by SPPICD that is not provided by SPPTRI.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangular factor is stored in AP;
*          = 'L':  Lower triangular factor is stored in AP.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  AP      (input/output) REAL array, dimension (N*(N+1)/2)
*          On entry, the triangular factor U or L from the Cholesky
*          factorization A = U**T*U or A = L*L**T, packed columnwise as
*          a linear array.  The j-th column of U or L is stored in the
*          array AP as follows:
*          if UPLO = 'U', AP(i + (j-1)*j/2) = U(i,j) for 1<=i<=j;
*          if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = L(i,j) for j<=i<=n.
*
*          On exit, the upper or lower triangle of the (symmetric)
*          inverse of A, overwriting the input factor U or L.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, the (i,i) element of the factor U or L is
*                zero, and the inverse could not be computed.
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE
      PARAMETER          ( ONE = 1.0E+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
      CHARACTER*8        SV2115
      INTEGER            IERR1, IERR2, J, JC, JJ
      REAL               AJJ, DUM1, DUM2
*     ..
*     .. Local Arrays ..
      REAL, ALLOCATABLE  :: AUX( : )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           EINFO, ERRSAV, ERRSET, ERRSTR, SPPICD, SSCAL,
     $                   SSPR, STPTRI, XERBLA
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SPPTRI', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
      CALL ESSL_SPPTRI( UPLO, N, AP, INFO )
*
      RETURN
*
*     End of SPPTRI
*
      END
