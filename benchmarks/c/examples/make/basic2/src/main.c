#include "common.h"

extern void incr(int*);

void main() {
  int i = INIT_VALUE;
  incr(&i);
}
