#include "mopsa.h"

void test_add() {
  int i, j;
  i = 10;
  j = 20;
  _mopsa_assert_true(i + j == 30);
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
