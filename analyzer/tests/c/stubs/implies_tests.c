/*
 * Tests for the `implies` operator
 */

/*$
 * ensures: cond implies (return == then);
 * ensures: !cond implies (return == orelse);
 * ensures: return == then or return == orelse;
 */
int check(int cond, int then, int orelse);

void test_implies_without_overlapping() {
  int x = 1;
  _mopsa_assert(check(x == 1, 2, 3) == 2);
  _mopsa_assert(check(x == 0, 2, 3) == 3);
}

void test_implies_with_overlapping() {
  int x = _mopsa_rand();
  _mopsa_assert_exists(check(x == 1, 2, 3) == 2);
  _mopsa_assert_exists(check(x == 1, 2, 3) == 3);
  _mopsa_assert(check(x == 1, 2, 3) != 0);
}

/*$
 * requires: x >= 0 implies y >= 0;
 */
void assert_sign_implication(int x, int y);

void test_implies_in_requirement() {
  assert_sign_implication(0, 1);
  _mopsa_assert_safe();

  assert_sign_implication(-1, 1);
  _mopsa_assert_safe();

  assert_sign_implication(1, -1);
  _mopsa_assert_unsafe();
}
