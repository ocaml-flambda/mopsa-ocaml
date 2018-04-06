#include "mopsa.h"

int glob1[5];
int glob2[5][5];
int glob3[5] = {1, 2};

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

void test_multi_array() {
  int a[5][5];
  a[1][1] = 5;
  a[2][4] = a[1][1] * 100;
  _mopsa_assert_true(a[2][4] == 500);
}

void test_global_init_with_zero() {
  _mopsa_assert_true(glob1[1] == 0);
  _mopsa_assert_true(glob2[1][2] == 0);
  _mopsa_assert_true(glob3[2] == 0);
}

void test_initialization() {
  int a[4] = {1, 2, 3, 4};
  _mopsa_assert_true(a[1] == 2);
}

void test_initialization_multi_array() {
  int a[2][2][3] = {{{1, 2, 3}, {4, 5, 6}}, {{7, 8, 9}, {10, 11, 12}}};
  _mopsa_assert_true(a[1][1][2] == 12);
}
