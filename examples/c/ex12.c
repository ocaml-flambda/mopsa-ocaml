void inc (int * p) {
  *p = *p +1;
}

void doub(int * p) {
  int y = 0;
  inc(p);
}

void doub2(int * p) {
  int y = 0;
  inc(p);
}

void doub3(int * p) {
  int z = 0;
  inc(p);
}

int main () {
  int i= 0;
  int* p = &i;
  doub(p);
  doub2(p);
  doub3(p);
}
