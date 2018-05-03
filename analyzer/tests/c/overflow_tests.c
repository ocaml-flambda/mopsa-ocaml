#include "mopsa.h"

int test_0() {
  unsigned char c = 25;
  unsigned char d = 6;
  unsigned char e = 10 * c + d;
  _mopsa_assert_error_exists(INTEGER_OVERFLOW);
}
