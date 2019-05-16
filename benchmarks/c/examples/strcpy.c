char * strcpy(char *dst, const char *src)
{
    while ((*dst++ = *src++) != 0)
	;
    return dst;
}

int main() {
  char s1[20] = "salut";
  char s2[30] = " ";
  char s3[40] = "toto";
  strcpy(s1,s2);
  strcpy(s1,s3);
}
