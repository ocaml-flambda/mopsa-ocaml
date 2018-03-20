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
  toto e ;
  e.a.x = 1;
  e.b.x = 3;
  a.a.y = 2;
  e.b.y = 4;
  void * e2 = (void*) (&e);
  /* point aa = e.a; */
  aa.x = 10;
  /* printf("%d", e.a.x); */
}
