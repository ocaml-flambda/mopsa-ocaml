#include "mopsa.h"

char s0[] = "global string";

void test_string_global_initialization() {
  _mopsa_assert_true(s0[0] == 'g');
  _mopsa_assert_true(s0[1] != 'o');
}

void test_string_local_initialization() {
  char s1[] = "local string";
  _mopsa_assert_true(s1[0] == 'l');
  _mopsa_assert_true(s1[1] != 'a');
}

void test_nul_fill_at_initialization() {
  char s2[10] = "part";
  _mopsa_assert_true(s2[4] == '\0');
}
