unsigned char i, j;
long int a[100];

void main() {
  i = 0;
  char*p;
  p = &(a[1]);
  *p = 100;
  while(i < 100) {
    a[i] = i;
    i++;
  }
}
