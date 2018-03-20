/* #include <stdio.h> */

int inc(int x) {
  return x +1;
}

int dec(int x) {
  return x - 1;
}

int tim2(int x) {
  int y = x;
  int i = 0;
  while (i < x) {
    y = inc(y);
    i ++;
  }
  return y;
}
void main() {
  int i = 0;
  int j = 0;
  while (i < 10) {
    i = inc(i);
    j = tim2(j);
  }
}
