int remainder(int a, int b) {
  int r = a;
  while (r >= b) {
    r = r - b;
  }
  return r;
}

int main() {
  int x = 10;
  int y = _mopsa_range_int(2, 5);
  int z = remainder(x, y);
}
