#include "mopsa.h"
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
