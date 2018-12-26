#include <stdlib.h>

/*$
 * local   : void *addr = new Memory;
 * ensures : return == addr and
 *           return:bytes == n;
 */
void *my_malloc(size_t n);

void test_new_int() {
  int *x = (int*)my_malloc(sizeof(int));
  *x = 10;
  _mopsa_assert(*x == 10);
}

void test_new_int_array() {
  int *a = (int*)my_malloc(10 * sizeof(int));
  a[0] = 10;
  a[9] = 100;
  _mopsa_assert(a[0] + a[9] == 110);
}



/*$
 * requires: p in Memory;
 * free: p;
 */
void my_free(void*p);

void test_free() {
  int *p = (int *)my_malloc(sizeof(int));
  *p = 10;
  my_free(p);
  int y = *p;
  _mopsa_assert_unsafe();
}
