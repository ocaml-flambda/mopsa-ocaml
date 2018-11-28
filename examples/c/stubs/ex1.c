#include "mopsa.h"

/*$
 * requires: p != q;
 * assigns: *p;
 * assigns: *q;
 * ensures: *p == old(*q) and *q == old(*p) and return == 0;
 */
int swap(int*p, int*q) {}

void main() {
  int x = 1;
  int y = 2;

  swap(&x, &y);

  _mopsa_print();
}
