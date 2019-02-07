char * strcpy(char *s1, const char *s2)
{
    char *s = s1;
    while ((*s++ = *s2++) != 0)
	;
    return (s1);
}

int main() {
  char s1[20] = "salut";
  char s2[30] = " ";
  char s3[40] = "toto";
  strcpy(s1,s2);
  strcpy(s1,s3);
}
