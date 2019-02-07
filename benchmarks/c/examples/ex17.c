int main () {
  char x[10];
  char * y = &(*x);
  int i = 0 ;
  while (i < 10) {
    i = i+1;
    *y = 1;
    y = y+1;
  }
}
