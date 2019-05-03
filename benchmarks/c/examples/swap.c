void swap (int *x , int * y) {
  int tmp = *x;
  *x = *y;
  *y = tmp;
}

int main () {
  int a, b;

  a = 10 ;
  b = 5 ;
  swap(&a,&b);

  a = 2;
  b = 0;
  swap(&a,&b);
}
