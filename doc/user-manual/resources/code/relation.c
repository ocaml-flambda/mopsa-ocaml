int main(int argc, char* argv[]) {
  int a[20];
  int i = 0, j = 20;
  while (i<10) {
    j--;
    a[i] = i;
    a[j] = j;
    i++;
  }
  return 0;
}
