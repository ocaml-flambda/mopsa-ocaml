int a[10];

void main() {
  int i;
  for(i = 1; i < 10; i++) {
    a[i] = a[i-1] + 1;
  }
}
