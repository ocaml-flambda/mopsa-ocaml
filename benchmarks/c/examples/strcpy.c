char * _strcpy(char *dst, const char *src)
{
    while ((*dst++ = *src++) != 0)
	;
    return dst;
}

int main() {
  char s1[20] = "salut";
  char s2[30] = " ";
  char s3[40] = "toto";
  _strcpy(s1,s2);
  _strcpy(s1,s3);
}
