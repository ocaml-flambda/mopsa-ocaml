typedef struct {
   int    x;
   int    y;
} point;

int main () {
  point e[10];
  (e[0]).x = 0;
  (e[0]).y = 0;
  int* p = &(e[1]);
  *(p-1) = 1;
}
