#include "mopsa.h"

typedef struct {
  int* f;
} s;

int buf[10];

void init(s* x) {
  x[1].f = buf;
}

void test_allamigeon() {
  s a[2][2];
  s* ptr = (s*) &a[1];
  init(ptr);
  ptr = (s*) &a[0];
  a[1][1].f[0] = 10;
  a[1][1].f[10] = 20;
}
