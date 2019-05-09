int main () {
  int y = _mopsa_rand_int(-500,500);
  int x = y;
  if (x < 0) x = -y;
  if (x < 100) _mopsa_print();
}
