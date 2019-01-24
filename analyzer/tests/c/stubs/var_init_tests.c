#include <stddef.h>

/*$
 * ensures: x == 1;
 */
int x;

void test_init_global_scalar() {
  _mopsa_assert(x == 1);
}



/*$
 * ensures: a[0] == 0 and
 *          a[1] == 1 and
 *          a[2] == 10;
 */
int a[10];

void test_init_global_array() {
  _mopsa_assert(a[0] == 0);
  _mopsa_assert(a[1] == 1);
  _mopsa_assert(a[2] == 10);
}


/*$
 * local: void *addr = new MyResource;
 * ensures: s == addr;
 */
void *s;

void test_init_resource() {
  _mopsa_assert(s != NULL);
}
