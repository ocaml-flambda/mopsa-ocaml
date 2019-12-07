/*$
 * ensures: return == x + 1;
 */
int incr(int x);

/*$
 * local : int xx = incr(x);
 * local : int yy = incr(y);
 * ensures: return == xx + yy;
 */
int sum_incr(int x, int y);

void test_local() {
  int x = 1;
  int y = 2;
  int z = sum_incr(x, y);
  _mopsa_assert(z == 5);
}
