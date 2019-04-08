#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <getopt.h>

#define VERSION 1
#define PROGRAM_NAME "ex23"


static struct option const longopts[] =
{
  {"version", no_argument, NULL, 'v'},
  {"help", no_argument, NULL, 'h'},
};


void usage (int status)
{
  printf ("Usage: %s [OPTION]\n", PROGRAM_NAME);
  fputs ("\
Example program using getopt.\n\
", stdout);
  fputs ("\
\n\
  -v, --version     print version number\n\
  -h, --help        print this message\n\
", stdout);
  exit (status);
}


int
main (int argc, char **argv)
{
  int optc;
  bool ok = false;


  while ((optc = getopt_long (argc, argv, "vh", longopts, NULL))
         != -1)
    {
      switch (optc)
        {
        case 'v':
          ok = true;
          break;

        case 'h':
          usage (EXIT_SUCCESS);

        default:
          usage (EXIT_FAILURE);
        }
    }

  if (ok)
    {
      printf ("Version: %d\n", VERSION);
    }


  return EXIT_SUCCESS;
}
