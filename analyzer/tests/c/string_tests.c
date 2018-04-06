#include "mopsa.h"

char g0[] = "global string";
char g1[5];

void test_string_global_initialization() {
  _mopsa_assert_true(g0[0] == 'g');
  _mopsa_assert_true(g0[1] != 'o');
  _mopsa_assert_true(g1[0] == '\0');
}

void test_string_local_initialization() {
  char s1[] = "local string";
  _mopsa_assert_true(s1[0] == 'l');
  _mopsa_assert_true(s1[1] != 'a');
}

void test_null_fill_at_initialization() {
  char s2[10] = "part";
  _mopsa_assert_true(s2[4] == '\0');
}
