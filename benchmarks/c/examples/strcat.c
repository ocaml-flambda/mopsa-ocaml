char *strcat(char *dest, const char *src)
{
    char *ret = dest;
    while (*dest)
        dest++;
    while (*dest++ = *src++)
        ;
    return ret;
}

int main() {
  char s1[20] = "salut";
  char s2[30] = " ";
  char s3[40] = "toto";
  strcat(s1,s2);
  strcat(s1, strcat(s2, s3));
}
