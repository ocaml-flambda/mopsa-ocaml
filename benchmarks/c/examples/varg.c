#include <stdio.h>

void
version_etc_arn (const char * program, const char * const * authors, size_t n_authors)
{
  printf("program: %s\n", program);
  for(int i = 0; i < n_authors; i++)
    printf("author: %s\n", authors[i]);
}

void
version_etc_va (const char *program, va_list authors)
{
  size_t n_authors;
  const char *authtab[10];

  for (n_authors = 0;
       n_authors < 10
         && (authtab[n_authors] = va_arg (authors, const char *)) != NULL;
       n_authors++)
    ;
  version_etc_arn (program,
                   authtab, n_authors);
}

void
version_etc (const char *program, /* const char *author1, ...*/ ...)
{
  va_list authors;

  va_start (authors, program);
  version_etc_va (program, authors);
  va_end (authors);
}

#define PROGRAM "ls"

#define AUTHORS \
  "Richard M. Stallman", \
  "David MacKenzie"


int main() {
  version_etc(PROGRAM, AUTHORS, NULL);
}
