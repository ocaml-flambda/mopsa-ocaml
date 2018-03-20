void swap (int *x , int * y) {
  int tmp = *x;
  *x = *y;
  *y = tmp;
}

int main () {
  int a = 0 ;
  int b = 0 ;
  swap(&a,&b);
  a = 2;
  b = 0;
  swap(&a,&b);
  a = 2;
  b = 1;
  swap(&a,&b);
  a = 0;
  b = 2;
  swap(&a,&b);
  a = 2;
  b = 2;
  swap(&a,&b);
}
