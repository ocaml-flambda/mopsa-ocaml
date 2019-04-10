void main () {
  int i = 1;

  while (i <= 10)
    i = i + 2;

  if (i >= 12)
    i = 0;

  float f = 1.0 / i; // Should be safe with a reduced product interval x congruence
}
