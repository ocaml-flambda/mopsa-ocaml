int main () {
  int x = 2;
  int *q = &x;
  int **p = &q;
  **p = 1;
}
