#include <stdio.h>
#include <string.h>

int main(int argc, char *argv[]) {
  char *name = argv[0];
  for(int i = 0; i < argc; i++) {
    printf("|argv[%d]| = %d\n", i, strlen(argv[i]));
  }
}
