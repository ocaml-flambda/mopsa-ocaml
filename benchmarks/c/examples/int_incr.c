void incr (int * p) {
  *p = *p +1;
}

int main () {
  int i = 2;
  int* p = &i;
  incr(p);
  incr(p);
  incr(p);
}
