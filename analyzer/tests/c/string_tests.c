#include "mopsa.h"

char g0[] = "global string";
char g1[5];

void test_string_global_initialization() {
  _mopsa_assert_true(g0[0] == 'g');
}

void test_string_local_initialization() {
  char s1[] = "local string";
  _mopsa_assert_true(s1[0] == 'l');
}

void test_null_fill_at_initialization() {
  _mopsa_assert_true(g1[0] == '\0');
}

void test_same_string_from_string_table() {
  char *p, *q;
  p = "string";
  q = "string";
  _mopsa_assert_true(p == q);
}

void test_different_strings_from_string_table() {
  char *p, *q;
  p = "string1";
  q = "string2";
  _mopsa_assert_true(p != q);
}
