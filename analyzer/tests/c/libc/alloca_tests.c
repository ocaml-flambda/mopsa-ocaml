#include <alloca.h>

int *p;

int incr(int x) {
   p = (int*)alloca(sizeof(int));
  *p = x;
  return (*p + 1);
}

void test_safe() {
  int a = 10;
  int b = incr(a);
  _mopsa_assert_safe();
  _mopsa_assert(b == a + 1);
}

void test_unsafe() {
  int a = 10;
  p = &a;
  int b = incr(a);
  b = *p + 1;
  _mopsa_assert_unsafe();
}
