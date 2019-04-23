char global_uninitilized[10];
char normal[10] = "abcd";
char string_longer_than_size[3] = "abcd";

void main() {
  char local_uninitilized[10];

  normal[2] = '\0';
  normal[2] = 'b';

  string_longer_than_size[0] = '1';
}
