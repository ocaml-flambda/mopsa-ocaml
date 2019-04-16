#include <getopt.h>
#include <stdlib.h>

void help() { }

void version() { }

void error() { }

int main(int argc, char *argv[]) {
  int c;
  while ((c = getopt(argc, argv, "hv")) != -1) {
    switch (c) {
    case 'h':
      help();
      break;

    case 'v':
      version();
      break;

    default:
      error();
      exit(EXIT_FAILURE);
    }
  }
  exit(EXIT_SUCCESS);
}
