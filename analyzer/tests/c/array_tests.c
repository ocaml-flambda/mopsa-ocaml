#include "mopsa.h"


/************************/
/* Array initialization */
/************************/

unsigned char glob[5];

void test_global_init_with_zero() {
  _mopsa_assert_exists(glob[0] == 0);
}

void test_initialization() {
  int a[4] = {1, 2, 3, 4};
  _mopsa_assert(a[0] == 1);
}

void test_initialization_multi_array() {
  int a[2][2][3] = {{{1, 2, 3}, {4, 5, 6}}, {{7, 8, 9}, {10, 11, 12}}};
  _mopsa_assert(a[0][0][0] == 1);
}


/*******************************/
/* Array access with constants */
/*******************************/

void test_array_in_lval() {
  int a[10];
  int i = 500;
  a[5] = i;
  _mopsa_assert(a[5] == 500);
}

void test_array_in_rval() {
  int a[10];
  int i;
  a[8] = 500;
  i = a[8];
  _mopsa_assert(i == 500);
}

void test_multi_array() {
  int a[5][5];
  a[1][1] = 5;
  a[2][4] = a[1][1] * 100;
  _mopsa_assert(a[2][4] == 500);
}



/*********************************************/
/* Out-of-bound checks with a constant index */
/*********************************************/

void test_array_out_of_bound_with_constant_index() {
  int a[100];
  int j = 100;
  int x = a[j];
  _mopsa_assert_unsafe();
}

void test_array_safe_access_with_constant() {
  int a[100];
  int j = 99;
  int x = a[j];
  _mopsa_assert_safe();
}


/*********************************************/
/* Out-of-bound checks with an interval index */
/*********************************************/

void test_array_safe_access_with_range() {
  int a[100];
  int j = _mopsa_range_int(0, 10);
  int x = a[j];
  _mopsa_assert_safe();
}

void test_array_out_of_bound_access_with_range() {
  int a[100];
  int j = _mopsa_range_int(100, 200);
  int x = a[j];
  _mopsa_assert_unsafe();
}

void test_may_array_out_of_bound_access_with_range() {
  int a[100];
  int j = _mopsa_range_int(10, 200);
  int x = a[j];
  _mopsa_assert_unsafe();
}

void test_array_safe_access_with_range_in_lval() {
  int a[100];
  int j = _mopsa_range_int(10, 20);
  a[j] = 10;
  _mopsa_assert_safe();
}

void test_array_range_in_rval() {
  int a[100];
  int j = _mopsa_range_int(10, 80);
  int x = a[j] + 0;
  _mopsa_assert_safe();
}


/****************/
/* Weak updates */
/****************/

void test_weak_update() {
  int a[100];
  a[0] = 0;
  a[1] = 1;
  int j = _mopsa_range_int(0, 1);
  a[j] = 3;
  _mopsa_assert_exists(a[0] == 0);
  _mopsa_assert_exists(a[0] == 3);
  _mopsa_assert_exists(a[1] == 1);
  _mopsa_assert_exists(a[1] == 3);
}


/************************/
/* Variable-size arrays */
/************************/

void test_variable_size_array_with_constant_expression() {
  int n = 10;
  int a[n];
  a[0] = 1;
  _mopsa_assert_safe();
  a[9] = 1;
  _mopsa_assert_safe();
  a[10] = 0;
  _mopsa_assert_unsafe();
}

void test_variable_size_array() {
  int n = _mopsa_range_int(1,10);
  int a[n];
  a[0] = 1;
  _mopsa_assert_safe();
  a[10] = 0;
  _mopsa_assert_unsafe();
}
