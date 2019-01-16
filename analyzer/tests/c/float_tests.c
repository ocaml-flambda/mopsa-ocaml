/*************************************************/
/*                                               */
/* Unit tests for the abstraction of floats in C */
/*                                               */
/*************************************************/

#include "mopsa.h"

void test_float_initialization() {
  float f = 1.1;
  _mopsa_assert(f == 1.1);
}
