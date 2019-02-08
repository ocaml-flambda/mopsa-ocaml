#include "mopsa.h"

/*$
 * requires: 0 <= i and i < size(p);
 * ensures: return == p[i];
 */
int get(int *p, int i) {}


/*$
 * requires: 0 <= i and i < size(p);
 * assigns: p[i];
 * ensures: p[i] == n and return == n;
 */
int set(int *p, int i, int n) {}

void main() {
  int a[5] = {0, 1, 2, 3, 4};
  int b[5];
  int x = get(a, 1);
  _mopsa_print();

  set(b, 0, get(a, 1));
  _mopsa_print();
}
