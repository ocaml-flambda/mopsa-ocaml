#include "mopsa.h"

typedef union u_ {
  struct {int i1; int i2;} i;
  long int l;
} u_t;

void test_initialization() {
  u_t u = {1};
  _mopsa_assert_true(u.l == 1);
  _mopsa_assert_true(u.i.i1 == 1);
}


void test_initialization_with_designed_name() {
  u_t u = {.l = 1};
  _mopsa_assert_true(u.l == 1);
  _mopsa_assert_true(u.i.i1 == 1);
}
