unsigned char global_uninitilized[10];
unsigned char normal[10] = "abcd";
unsigned char string_longer_than_size[3] = "abcd";

void main() {
  unsigned char local_uninitilized[10];
  unsigned char *p = normal;

  p[2] = '\0';
  normal[2] = 'b';
  normal[2] = '\0';

  string_longer_than_size[0] = '1';

  unsigned char before, at, after;
  before = normal[0];
  at = normal[2];
  after = normal[3];
}
