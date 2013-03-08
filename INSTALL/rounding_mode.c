#include <float.h>
#define ROUNDING_MODE rounding_mode

int ROUNDING_MODE()

/*
*  Synopsis
*  ========
*
*  LAPACK auxiliary routine
*  E. Anderson, Cray Research Inc.
*  April 30, 1996
*
*  Purpose
*  =======
*
*  ROUNDING_MODE() is called by the new Fortran 90 version of SLAMCH
*  to identify whether or not the target machine has IEEE arithmetic.
*  It returns 0 for (chopped) Cray arithmetic and 1 for (rounded) IEEE
*  by reading the value of FLT_ROUNDS from float.h.
*/

{
    return (FLT_ROUNDS);
}
