/*$$
 * predicate pos(x):
 *   x >= 0;
 */

/*$
 * requires: pos(a);
 * ensures: pos(return);
 */
int sqrt_(int a);

void test_global_predicate() {
  int r = 10;
  _mopsa_assert(sqrt_(r) >= 0);
}
