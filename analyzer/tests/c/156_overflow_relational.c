// Reported by Jérôme Boillot in issue #156

int main() {
	int n;
	if (n < 0) return 0;

	int x = n;
	int y = 0;
	while (x > 0) {
		x--;
		y++;
		_mopsa_assert(x + y == n);
	}
	_mopsa_assert(x == 0 && y == n);

  x = n;
  y = 0;
  while (x > 0) {
    int cc;
    if (cc < 0 || cc > x)
      return 0;
    x -= cc;
    y += cc;
  }
  _mopsa_assert(x == 0 && y == n);
}
