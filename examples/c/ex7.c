/* #include <stdio.h> */

typedef struct {
   int    x;
   int    y;
} point;

typedef struct {
   point    a;
   int  b[20];
} toto;


int main () {
  toto e;
  e.a.x = 0;
  e.a.y = 0;
  int * p = (int *) &(e.b);
  *(p-1) = 1;
  p = p-1;
}
