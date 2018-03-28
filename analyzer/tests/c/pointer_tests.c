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
  int i, j;
  int *p, *q, *r;
  p = &i;
  q = &i;
  r = &j;
  _mopsa_assert_true(p == q);
}
