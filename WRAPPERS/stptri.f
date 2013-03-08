      SUBROUTINE STPTRI( UPLO, DIAG, N, AP, INFO )
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
      CHARACTER          DIAG, UPLO
      INTEGER            INFO, N
*     ..
*     .. Array Arguments ..
      REAL               AP( * )
*     ..
*
*  Purpose
*  =======
*
*  STPTRI computes the inverse of a real upper or lower triangular
*  matrix A stored in packed format.
*
*  ESSL Enablement Comments
*  ==== ========== ========
*
*  This is a stub routine that calls the ESSL subroutine STPI.  In all
*  cases, the results returned will be identical in structure to those
*  of the normal LAPACK routine STPTRI.
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
*  AP      (input/output) REAL array, dimension (N*(N+1)/2)
*          On entry, the upper or lower triangular matrix A, stored
*          columnwise in a linear array.  The j-th column of A is stored
*          in the array AP as follows:
*          if UPLO = 'U', AP((j-1)*j/2 + i) = A(i,j) for 1<=i<=j;
*          if UPLO = 'L',
*             AP((j-1)*(n-j) + j*(j+1)/2 + i-j) = A(i,j) for j<=i<=n.
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
*     .. Local Scalars ..
      LOGICAL            NOUNIT, UPPER
      CHARACTER*8        SV2145
      INTEGER            IERR1, IERR2
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           EINFO, ERRSAV, ERRSET, ERRSTR, STPI, XERBLA
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
         CALL XERBLA( 'STPTRI', -INFO )
         RETURN
      END IF
*
      CALL ESSL_STPTRI( UPLO, DIAG, N, AP, INFO )
*
      RETURN
*
*     End of STPTRI
*
      END
