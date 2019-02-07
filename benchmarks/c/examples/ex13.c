void inc (int * p) {
  *p = *p +1;
}

int main () {
  int i = 2;
  int* p = &i;
  inc(p);
  inc(p);
  inc(p);
}
