void main() {
  int a[10], b[10];
  int i = 0, *p = b;

  while (i < 10) {
    a[i] = i;
    *p = 10 - i;
    i++;
    p++;
  }
}
