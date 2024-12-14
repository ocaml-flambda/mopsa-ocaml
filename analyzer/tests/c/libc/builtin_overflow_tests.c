/* Tests for compiler builtins __builtin_{s,u}{add,mul}{l,ll}?_overflow */

#include <limits.h>

extern unsigned char uc_rand();

void test_builtin_add_overflow() {
  unsigned int a = uc_rand()%10;
  unsigned int b = UINT_MAX - 5;
  unsigned int c = UINT_MAX;

  unsigned int r = 0;

  if(__builtin_uadd_overflow(a, b, &r)) {
    _mopsa_assert(6 <= a && a <= 9);
    _mopsa_assert(b > UINT_MAX - a);
  } else {
    _mopsa_assert(a >= 0 && a <= 5);
  }
}
