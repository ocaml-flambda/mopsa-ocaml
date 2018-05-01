#include "mopsa.h"

int incr(int x) {
  return (x+1);
}

int ddouble(int x) {
  return 2 * x;
}

void test_call_by_deref() {
  int (*f)(int);
  f = incr;
  _mopsa_assert_true((*f)(0) == 1);
}

void test_call_as_function() {
  int (*f)(int);
  f = incr;
  _mopsa_assert_true(f(0) == 1);
}

void test_pointer_to_pointer_assign() {
  int (*f)(int);
  void* g;
  g = (void*) ddouble;
  f = g;
  _mopsa_assert_true((f)(5) == 10);
}
