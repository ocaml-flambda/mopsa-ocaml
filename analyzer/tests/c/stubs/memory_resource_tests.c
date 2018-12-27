#include <stdlib.h>

/*$
 * local   : void *addr = new Memory;
 * ensures : return == addr and
 *           return:bytes == n;
 */
void *malloc_(size_t n);

void test_new_int() {
  int *x = (int*)malloc_(sizeof(int));
  *x = 10;
  _mopsa_assert(*x == 10);
}

void test_new_int_array() {
  int *a = (int*)malloc_(10 * sizeof(int));
  a[0] = 10;
  a[9] = 100;
  _mopsa_assert(a[0] + a[9] == 110);
}



/*$
 * requires: p in Memory;
 * free: p;
 */
void free_(void*p);

void test_free() {
  int *p = (int *)malloc_(sizeof(int));
  *p = 10;
  free_(p);
  int y = *p;
  _mopsa_assert_unsafe();
}
