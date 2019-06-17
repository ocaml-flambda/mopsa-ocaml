#include "mopsa.h"

void test_add() {
  char i, j, k;
  i = 27;
  j = 100;
  k = i + j;
  _mopsa_assert(k == 127);
}

void test_mult() {
  int i, j;
  i = 10;
  j = 20;
  _mopsa_assert(i * j == 200);
}

void test_div() {
  int i, j;
  i = 10;
  j = 2;
  _mopsa_assert(i / j == 5);
}

void test_mod() {
  char i, j;
  i = 10;
  j = 3;
  _mopsa_assert(i % j == 1);
}

void test_bit_not() {
  int i = 10;
  int j = ~i;
  _mopsa_assert(j == -11);
}

int glob;

void test_global_init_with_zero() {
  _mopsa_assert(glob == 0);
}

void test_rand_int() {
  int a = _mopsa_range_int(0, 10);
  _mopsa_assert(a >= 0 && a <= 10);
}

void test_divide_by_constant() {
  int a = 4 / 2;
  _mopsa_assert_safe();
  int c = a / (1 - 1);
  _mopsa_assert_error(DIVISION_BY_ZERO);
}

void test_divide_by_variable() {
  int x = 1;
  int a = 4 / x;
  _mopsa_assert_safe();
  int y = x - 1;
  int c = a / y;
  _mopsa_assert_error(DIVISION_BY_ZERO);
}

void test_divide_by_range() {
  int x = _mopsa_range_int(-5,-1);
  int a = 4 / x;
  _mopsa_assert_safe();
  int y = x + 2;
  int c = a / y;
  _mopsa_assert_error_exists(DIVISION_BY_ZERO);
}

int test_overflow() {
  unsigned char c = 25;
  unsigned char d = 6;
  unsigned char e = 10 * c + d;
  _mopsa_assert_error_exists(INTEGER_OVERFLOW);
}

int test_condition_value() {
  int x = 1;

  int b1 = (x == 1);
  _mopsa_assert(b1 == 1);

  int b2 = (x == 0);
  _mopsa_assert(b2 == 0);

  int b3 = (x >= 0);
  _mopsa_assert(b3 == 1);

  int b4 = (x <= 0);
  _mopsa_assert(b4 == 0);
}


/* void test_interval_congruence_reduction() { */
/*   int i = 1; */
/*   while (i <= 10) { */
/*     i = i + 2; */
/*   } */
/*   _mopsa_assert(i == 11); */
/* } */
