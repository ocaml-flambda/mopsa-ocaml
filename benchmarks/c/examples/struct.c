/* #include <stdio.h> */

typedef struct {
   int    x;
   int    y;
} point;

typedef struct {
   point    a;
   point    b;
} toto;



int main() {
  toto e = { .a = { .x = 1 }, .b = { .x = 2 } };
  e.a.x = 1;
  e.b.x = 3;
  e.a.y = 2;
  e.b.y = 4;
  toto * e2 = &e;
  (e2->b).x = 100;
}
