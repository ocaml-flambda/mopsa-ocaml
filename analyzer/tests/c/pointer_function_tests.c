#include "mopsa.h"

int incr(int x) {
  return (x+1);
}

int ddouble(int x) {
  return 2 * x;
}

void test_0() {
  int (*f)(int);
  void* g;
  f = incr;
  _mopsa_assert_true((*f)(0) == 1);
  g = (void*) ddouble;
  f = g;
  _mopsa_assert_true((f)(5) == 10);
  f = &incr;
  _mopsa_assert_true((f)(0) == 1);
}
