int inc(int x) {
  return x + 1;
}

int dec(int x) {
  return x - 1;
}

void main() {
  int i = 0;
  int j = 10;
  while (i < j) {
    i = inc(i);
    j = dec(j);
  }
}
