unsigned char s[10] = "abcd";

void main() {
  unsigned char *p = s;

  p[2] = '\0';
  s[2] = 'b';
  s[2] = '\0';

  unsigned char before = s[0];
  unsigned char at = p[2];
  unsigned char after = s[3];

  int len = 0;
  while (s[len]) len++;
}
