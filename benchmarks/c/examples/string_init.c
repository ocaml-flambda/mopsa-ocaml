char global_uninitilized[10];
char normal[10] = "abcd";
char string_longer_than_size[3] = "abcd";

void main() {
  char local_uninitilized[10];
  char *p = normal;

  p[2] = '\0';
  normal[2] = 'b';
  normal[2] = '\0';

  string_longer_than_size[0] = '1';

  int x;

  x = normal[0];
  x = normal[2];
  x = normal[3];
}
