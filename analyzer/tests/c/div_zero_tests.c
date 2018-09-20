#include "mopsa.h"

void test_divide_by_constant() {
  int a = 4 / 2;
  _mopsa_assert_safe();
  int c = a / (1 - 1);
  _mopsa_assert_error(DIVISION_BY_ZERO);
}

/* void test_divide_by_variable() { */
/*   int x = 1; */
/*   int a = 4 / x; */
/*   _mopsa_assert_safe(); */
/*   int y = x - 1; */
/*   int c = a / y; */
/*   _mopsa_assert_error(DIVISION_BY_ZERO); */
/* } */

/* void test_divide_by_range() { */
/*   int x = _mopsa_rand_int(-5,-1); */
/*   int a = 4 / x; */
/*   _mopsa_assert_safe(); */
/*   int y = x + 2; */
/*   int c = a / y; */
/*   _mopsa_assert_error_exists(DIVISION_BY_ZERO); */
/* } */
