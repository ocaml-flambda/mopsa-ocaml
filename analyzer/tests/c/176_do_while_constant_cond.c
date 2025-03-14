void test_break() {
	int x = 0;

	do {
		x = 1;
		break;
	} while (0);

	_mopsa_assert_reachable();
	_mopsa_assert(x == 1);
}

void test_continue() {
	int x = 0;

	do {
		x = 1;
		continue;
	} while (0);

	_mopsa_assert_reachable();
	_mopsa_assert(x == 1);
}

void test_nested_break() {
	int x = 0;
	int y = 0;

	while (x < 10) {
		if (_mopsa_rand_s32()) break;

		do {
			y = 1;
			break;
		} while (0);

		_mopsa_assert_reachable();
		_mopsa_assert(y == 1);

		x++;
		y = 0;
	}

	_mopsa_assert(x <= 10);
}

void test_nested_continue() {
	int x = 0;
	int y = 0;

	while (x < 10) {
		if (_mopsa_rand_s32()) continue;

		do {
			y = 1;
			break;
		} while (0);

		_mopsa_assert_reachable();
		_mopsa_assert(y == 1);

		x++;
		y = 0;
	}

	_mopsa_assert(x == 10);
}
