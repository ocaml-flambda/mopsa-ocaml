#include "mopsa.h"

void test_deref_in_rval() {
  int i = 10;
  int* p = &i;
  i = i + 1;
  _mopsa_assert_true(*p == 11);
}

void test_deref_in_lval() {
  int i = 10;
  int* p = &i;
  *p = *p + 1;
  _mopsa_assert_true(i == 11);
}

void test_double_deref() {
  int i = 10;
  int* p = &i;
  int** q = &p;
  **q = *p + 1;
  _mopsa_assert_true(i == 11);
}

void test_equality() {
  int i;
  int *p, *q;
  p = &i;
  q = &i;
  _mopsa_assert_true(p == q);
}

void test_non_equality() {
  int i, j;
  int *p, *q;
  p = &i;
  q = &j;
  _mopsa_assert_true(p != q);
}

void test_array_deref() {
  int a[10];
  a[0] = 10;
  _mopsa_assert_true(*a == 10);
}

void test_array_as_address() {
  int a[10];
  int *p;
  p = a;
  *p = 10;
  _mopsa_assert_true(a[0] == 10);
}

void test_address_of_array() {
  int a[10];
  int *p;
  p = &a;
  *p = 10;
  _mopsa_assert_true(a[0] == 10);
}

void test_multidim_array_address() {
  int a[10][10];
  int **p;
  p = a;
  **p = 10;
  _mopsa_assert_true(a[0][0] == 10);
}
