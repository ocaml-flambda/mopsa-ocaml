/*
 * Tests of compiler builtins
 */

void test_builtin_constant_p() {
  _mopsa_assert(__builtin_constant_p(1) == 1);
  _mopsa_assert(__builtin_constant_p(1.1) == 1);
  _mopsa_assert(__builtin_constant_p('a') == 1);
  _mopsa_assert(__builtin_constant_p("abcd") == 1);

  int x = 1;
  _mopsa_assert_exists(__builtin_constant_p(x) == 1);
}
