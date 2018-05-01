#include "mopsa.h"

void test_add() {
  int i, j, k;
  i = 10;
  j = 20;
  k = i + j;
  _mopsa_assert_true(k == 30);
}

void test_mult() {
  int i, j;
  i = 10;
  j = 20;
  _mopsa_assert_true(i * j == 200);
}

void test_div() {
  int i, j;
  i = 10;
  j = 2;
  _mopsa_assert_true(i / j == 5);
}

void test_mod() {
  int i, j;
  i = 10;
  j = 3;
  _mopsa_assert_true(i % j == 1);
}

int glob;

void test_global_init_with_zero() {
  _mopsa_assert_true(glob == 0);
}

void test_rand_int() {
  int a = _mopsa_rand_int(0, 10);
  _mopsa_assert_true(a >= 0 && a <= 10);
}

void test_interval_congruence_reduction() {
  int i = 1;
  while (i <= 10) {
    i = i + 2;
  }
  if (i >= 12) {
    i = 0;
  }
  _mopsa_assert_true(i > 0);
}
