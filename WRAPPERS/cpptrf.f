      SUBROUTINE CPPTRF( UPLO, N, AP, INFO )
*
*  -- LAPACK routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, N
*     ..
*     .. Array Arguments ..
      COMPLEX            AP( * )
*     ..
*
*  Purpose
*  =======
*
*  CPPTRF computes the Cholesky factorization of a complex Hermitian
*  positive definite matrix A stored in packed format.
*
*  The factorization has the form
*     A = U**H * U,  if UPLO = 'U', or
*     A = L  * L**H,  if UPLO = 'L',
*  where U is an upper triangular matrix and L is lower triangular.
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
*  AP      (input/output) COMPLEX array, dimension (N*(N+1)/2)
*          On entry, the upper or lower triangle of the Hermitian matrix
*          A, packed columnwise in a linear array.  The j-th column of A
*          is stored in the array AP as follows:
*          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
*          if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.
*          See below for further details.
*
*          On exit, if INFO = 0, the triangular factor U or L from the
*          Cholesky factorization A = U**H*U or A = L*L**H, in the same
*          storage format as A.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, the leading minor of order i is not
*                positive definite, and the factorization could not be
*                completed.
*
*  Further Details
*  ===============
*
*  The packed storage scheme is illustrated by the following example
*  when N = 4, UPLO = 'U':
*
*  Two-dimensional storage of the Hermitian matrix A:
*
*     a11 a12 a13 a14
*         a22 a23 a24
*             a33 a34     (aij = conjg(aji))
*                 a44
*
*  Packed storage of the upper triangle of A:
*
*  AP = [ a11, a12, a22, a13, a23, a33, a14, a24, a34, a44 ]
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            J, JC, JJ
      REAL               AJJ
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      COMPLEX            CDOTC
      EXTERNAL           LSAME, CDOTC
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHPR, CSSCAL, CTPSV, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          REAL, SQRT
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
         CALL XERBLA( 'CPPTRF', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
      CALL ESSL_CPPTRF( UPLO, N, AP, INFO )
*
      RETURN
*
*     End of CPPTRF
*
      END