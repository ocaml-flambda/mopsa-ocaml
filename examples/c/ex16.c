int main () {
  char x[10];
  char * y = &(*x);
  int i = 0 ;
  while (i < 10) {
    i = i+1;
    if (i == 5) {
      *y = 0;
    } else {
      *y = 1;
    }
    y = y+1;
  }
}
