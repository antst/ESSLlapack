      SUBROUTINE ZTPTRI( UPLO, DIAG, N, AP, INFO )
*
*  -- LAPACK routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          DIAG, UPLO
      INTEGER            INFO, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         AP( * )
*     ..
*
*  Purpose
*  =======
*
*  ZTPTRI computes the inverse of a complex upper or lower triangular
*  matrix A stored in packed format.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  A is upper triangular;
*          = 'L':  A is lower triangular.
*
*  DIAG    (input) CHARACTER*1
*          = 'N':  A is non-unit triangular;
*          = 'U':  A is unit triangular.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  AP      (input/output) COMPLEX*16 array, dimension (N*(N+1)/2)
*          On entry, the upper or lower triangular matrix A, stored
*          columnwise in a linear array.  The j-th column of A is stored
*          in the array AP as follows:
*          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
*          if UPLO = 'L', AP(i + (j-1)*((2*n-j)/2) = A(i,j) for j<=i<=n.
*          See below for further details.
*          On exit, the (triangular) inverse of the original matrix, in
*          the same packed storage format.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, A(i,i) is exactly zero.  The triangular
*                matrix is singular and its inverse can not be computed.
*
*  Further Details
*  ===============
*
*  A triangular matrix A can be transferred to packed storage using one
*  of the following program segments:
*
*  UPLO = 'U':                      UPLO = 'L':
*
*        JC = 1                           JC = 1
*        DO 2 J = 1, N                    DO 2 J = 1, N
*           DO 1 I = 1, J                    DO 1 I = J, N
*              AP(JC+I-1) = A(I,J)              AP(JC+I-J) = A(I,J)
*      1    CONTINUE                    1    CONTINUE
*           JC = JC + J                      JC = JC + N - J + 1
*      2 CONTINUE                       2 CONTINUE
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE, ZERO
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ),
     $                   ZERO = ( 0.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            NOUNIT, UPPER
      INTEGER            J, JC, JCLAST, JJ
      COMPLEX*16         AJJ
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZSCAL, ZTPMV
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      NOUNIT = LSAME( DIAG, 'N' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOUNIT .AND. .NOT.LSAME( DIAG, 'U' ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZTPTRI', -INFO )
         RETURN
      END IF
*
      CALL ESSL_ZTPTRI( UPLO, DIAG, N, AP, INFO )
*
      RETURN
*
*     End of ZTPTRI
*
      END
