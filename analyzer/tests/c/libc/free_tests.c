/*
 * Tests of the free function
 */


void test_safe_free() {
  char *p = malloc(100);
  free(p);
  _mopsa_assert_safe();
}


void test_unsafe_double_free() {
  char *p = malloc(100);
  free(p);
  free(p);
  _mopsa_assert_unsafe();
}
