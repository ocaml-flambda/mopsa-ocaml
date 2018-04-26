unsigned char i, j;
long int a[2];

void main() {
  i = 0;
  char*p;
  p = &(a[1]);
  *p = 100;
  while(i < 2) {
    a[i] = i;
    i++;
  }
}
