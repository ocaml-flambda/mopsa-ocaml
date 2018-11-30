#include "mopsa.h"

/* Test resetting a region of an array */
/* *********************************** */

/*$
 * requires: l in [0, size(p) - 1] and
 *           u in [0, size(p) - 1];
 * assigns: p[l, u];
 * ensures: forall int i in [l, u]: p[i] == 0;
 */
void reset_region(int*p, int l, int u) {}

void test_assign_int() {
  int a[5] = {1, 2, 3, 4, 5};

  reset_region(a, 3, 4);
  _mopsa_assert(a[0] == 1);
  _mopsa_assert(a[3] == 0);
}


/* Test initialize an array with a constant */
/* **************************************** */

/*$
 * requires: n in [0, size(p)];
 * assigns: ((char*)p)[0, n - 1];
 * ensures: forall int i in [0, n - 1]: ((char*)p)[i] == c;
 */
void mymemset(void*p, char c, unsigned int n) {}

void test_memset() {
  char a[5] = {0, 1, 2, 3, 4};

  mymemset(a, 10, 5);
  _mopsa_assert(a[0] == 10);
}
