      SUBROUTINE SPOTRI( UPLO, N, A, LDA, INFO )
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
      INTEGER            INFO, LDA, N
*     ..
*     .. Array Arguments ..
      REAL               A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  SPOTRI computes the inverse of a real symmetric positive definite
*  matrix A using the Cholesky factorization A = U**T*U or A = L*L**T
*  computed by SPOTRF.
*
*  ESSL Enablement Comments
*  ==== ========== ========
*
*  This is a stub routine that calls the ESSL subroutine SPOICD.
*  In all cases, the results returned will be identical in structure
*  to those of the normal LAPACK routine SPOTRI.  Note that this
*  stub will allocate workspace needed by SPOICD that is not
*  provided by SPOTRI.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  A       (input/output) REAL array, dimension (LDA,N)
*          On entry, the triangular factor U or L from the Cholesky
*          factorization A = U**T*U or A = L*L**T, as computed by
*          SPOTRF.
*          On exit, the upper or lower triangle of the (symmetric)
*          inverse of A, overwriting the input factor U or L.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, the (i,i) element of the factor U or L is
*                zero, and the inverse could not be computed.
*
*  =====================================================================
*
*     .. Local Scalars ..
      CHARACTER*8        SV2115
      INTEGER            IERR1, IERR2
      REAL               DUM1, DUM2
*     ..
*     .. Local Arrays ..
      REAL, ALLOCATABLE  :: AUX( : )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           EINFO, ERRSAV, ERRSET, ERRSTR, SPOICD, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF( .NOT.LSAME( UPLO, 'U' ) .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SPOTRI', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
      CALL ESSL_SPOTRI( UPLO, N, A, LDA, INFO )
*
      RETURN
*
*     End of SPOTRI
*
      END
