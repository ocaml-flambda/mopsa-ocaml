/*$
 * ensures: x == 1;
 */
int x;

void test_init_global_scalar() {
  _mopsa_assert(x == 1);
}
