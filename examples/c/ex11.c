void toto(int * p) {
  *p = *p +1;
}

int main () {
  int b = 5;
  int *p = &b;
  toto(p);
  *p = 5;
  toto(p);
}
