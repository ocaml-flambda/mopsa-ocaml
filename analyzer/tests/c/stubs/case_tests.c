#include <stddef.h>

/*$
 * case "positive":
 *   assumes: x >= 0;
 *   ensures: return == 1;
 * ;
 *
 * case "negative":
 *   assumes: x < 0;
 *   ensures: return == 0;
 * ;
 */
unsigned int is_positive(int x);

void test_case_disjunction() {
  _mopsa_assert(is_positive(1) == 1);
  _mopsa_assert(is_positive(-1) == 0);

  int x = _mopsa_rand_int(-1, 1);
  _mopsa_assert_exists(is_positive(x) == 1);
  _mopsa_assert_exists(is_positive(x) == 0);
}

/*$
 * case "safe":
 *   local : void* addr = new SomeResource;
 *   ensures: return == addr;
 * ;
 *
 * case "no space":
 *   ensures: return == NULL;
 * ;
 */
void* alloc();

void test_case_non_determinism() {
  void *p = alloc();
  _mopsa_print();
}
