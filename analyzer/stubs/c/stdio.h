#ifndef _STDIO_H
#define _STDIO_H

typedef void FILE;

void* stdin;
char* fgets(char *str, int n, FILE *stream);

#endif
