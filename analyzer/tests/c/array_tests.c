#include "mopsa.h"

int glob[20];

void test_array_in_lval() {
  int a[10];
  int i = 500;
  a[5] = i;
  _mopsa_assert_true(a[5] == 500);
}

void test_array_in_rval() {
  int a[10];
  int i;
  a[8] = 500;
  i = a[8];
  _mopsa_assert_true(i == 500);
}

void test_multidim_array() {
  int a[10][10];
  a[1][1] = 5;
  a[2][4] = a[1][1] * 100;
  _mopsa_assert_true(a[2][4] == 500);
}

void test_global_init_to_zero() {
  _mopsa_assert_true(glob[1] == 0);
}

void test_initialization() {
  int a[4] = {1, 2, 3, 4};
  _mopsa_assert_true(a[1] == 2);
}
