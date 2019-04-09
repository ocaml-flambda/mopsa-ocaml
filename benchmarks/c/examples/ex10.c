void main () {
  int i = 1;

  while (i <= 10)
    i = i + 2;

  if (i >= 12)
    i = 0; // Should not be reachable with a reduced product interval x congruence
}
