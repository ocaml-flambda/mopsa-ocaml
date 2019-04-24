unsigned char s[10] = "abcd";

/*$
 * requires: exists unsigned int i in [0, size(s) - 1]: s[i] == 0;
 * ensures:  return in [0, size(s) - 1];
 * ensures:  s[return] == 0;
 * ensures:  forall unsigned int i in [0, return - 1]: s[i] != 0;
 */
unsigned int strlen(unsigned char*s);

void main() {
  unsigned char *p = s;

  p[2] = '\0';
  s[2] = 'b';
  s[2] = '\0';

  unsigned char before = s[0];
  unsigned char at = p[2];
  unsigned char after = s[3];

  int len1 = 0;
  while (s[len1]) len1++;

  int len2 = strlen(s);
}
