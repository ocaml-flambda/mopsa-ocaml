#include "mopsa.h"

/*$
 * assigns: *p;
 * ensures: *p == old(*p) + 1;
 */
void incr(int*p) {}

void main() {
  int x = 1;
  _mopsa_print();

  incr(&x);
  _mopsa_print();
}
