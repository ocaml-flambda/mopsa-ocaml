#include "mopsa.h"
/* #include<stdio.h> */

int test_0() {
  unsigned char c = 255;
  unsigned char d = 6;
  unsigned char e = 10 * c + d;
  _mopsa_assert_error(INTEGER_OVERFLOW)
}
