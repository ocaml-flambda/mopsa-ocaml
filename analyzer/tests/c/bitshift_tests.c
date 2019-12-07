#include "mopsa.h"

int  x = 12;
char y = 13;
long int l = 12;

void test_rshift_ok_1() {
  _mopsa_assert(x >> 0 == 12);
}

void test_rshift_ok_2() {
  _mopsa_assert(x >> 1 == 6);
}

void test_rshift_ok_3() {
  _mopsa_assert(x >> 31 == 0);
}

void test_rshift_ok_4() {
  _mopsa_assert(y >> 31 == 0);
}

void test_rshift_ok_5() {
  _mopsa_assert(l >> 32 == 0);
}

void test_rshift_bad_1() {
  x >> 32;
  _mopsa_assert_unsafe();
}

void test_rshift_bad_2() {
  x >> (-1);
  _mopsa_assert_unsafe();
}

void test_rshift_bad_3() {
  l >> 64;
  _mopsa_assert_unsafe();
}

void test_lshift_ok_1() {
  _mopsa_assert(x << 0 == 12);
}

void test_lshift_ok_2() {
  _mopsa_assert(x << 1 == 24);
}

void test_lshift_bad_1() {
  x << 32;
  _mopsa_assert_unsafe();
}

void test_lshift_bad_2() {
  x << (-1);
  _mopsa_assert_unsafe();
}
