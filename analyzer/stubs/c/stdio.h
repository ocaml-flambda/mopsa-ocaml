#ifndef _STDIO_H
#define _STDIO_H

typedef void FILE;

void* stdin;
char* fgets(char *str, int n, FILE *stream);

// Warning : the implementation in the stdlib of the following
// function is more than poorly done.
int fscanf(FILE *stream, const char *format, ...);
#endif
