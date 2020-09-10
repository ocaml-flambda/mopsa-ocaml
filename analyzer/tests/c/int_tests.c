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
  _mopsa_assert_unsafe();
}

void test_divide_by_variable() {
  int x = 1;
  int a = 4 / x;
  _mopsa_assert_safe();
  int y = x - 1;
  int c = a / y;
  _mopsa_assert_unsafe();
}

void test_divide_by_range() {
  int x = _mopsa_range_int(-5,-1);
  int a = 4 / x;
  _mopsa_assert_safe();
  int y = x + 2;
  int c = a / y;
  _mopsa_assert_unsafe();
}

int test_overflow() {
  signed char c = 25;
  signed char d = 6;
  signed char e = 10 * c + d;
  _mopsa_assert_unsafe();
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

void test_safe_right_shift() {
  int x = 120 >> 4;
  _mopsa_assert_safe();
  _mopsa_assert(x == 7);
}

void test_unsafe_right_shift_with_large_operand() {
  int x = 120 >> 99;
  _mopsa_assert_unsafe();
}

void test_unsafe_right_shift_with_negative_operand() {
  int a = -1;
  int x = 120 >> a;
  _mopsa_assert_unsafe();
}

void test_safe_left_shift() {
  int x = 120 << 4;
  _mopsa_assert_safe();
  _mopsa_assert(x == 1920);
}

void test_unsafe_left_shift_with_large_operand() {
  int x = 120 << 99;
  _mopsa_assert_unsafe();
}

void test_unsafe_left_shift_with_negative_operand() {
  int a = -1;
  int x = 120 << a;
  _mopsa_assert_unsafe();
}

void test_int_of_float_cast() {
  float f1 = 1.0;
  _mopsa_assert((int)f1 == 1);

  float f2 = 2.8;
  int i = (int) f2;
  _mopsa_assert(i == 2);
}


void test_int_of_double_cast() {
  double f1 = 1.0;
  _mopsa_assert((int)f1 == 1);

  double f2 = 2.8;
  int i = (int) f2;
  _mopsa_assert(i == 2);
}

void test_int_as_condition() {
  int x = 0;
  if (x) {
    _mopsa_assert_unreachable();
  } else {
    _mopsa_assert_reachable();
  }
}

void test_int_addition_as_condition() {
  int x = 0;
  int y = 1;
  if (x + y) {
    _mopsa_assert_reachable();
  } else {
    _mopsa_assert_unreachable();
  }
}
