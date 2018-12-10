/****************************************************************************/
/*                   Copyright (C) 2018 The MOPSA Project                   */
/*                                                                          */
/*   This program is free software: you can redistribute it and/or modify   */
/*   it under the terms of the CeCILL license V2.1.                         */
/*                                                                          */
/****************************************************************************/


// TODO: not implemented yet


/*
  libc stub
  based on header from glibc-2.27-r6
*/
#include <stdlib.h>
#include "mopsa_libc_utils.h"

size_t __ctype_get_mb_cur_max (void);

double atof (const char *__nptr);

int atoi (const char *__nptr);

long int atol (const char *__nptr);

#ifdef __USE_ISOC99

long long int atoll (const char *__nptr);

#endif


double strtod (const char *__restrict __nptr,
               char **__restrict __endptr);

#ifdef	__USE_ISOC99

float strtof (const char *__restrict __nptr,
              char **__restrict __endptr);

long double strtold (const char *__restrict __nptr,
                     char **__restrict __endptr);

#endif

long int strtol (const char *__restrict __nptr,
                 char **__restrict __endptr, int __base);


unsigned long int strtoul (const char *__restrict __nptr,
                           char **__restrict __endptr, int __base);


#ifdef __USE_MISC

long long int strtoq (const char *__restrict __nptr,
                      char **__restrict __endptr, int __base);

unsigned long long int strtouq (const char *__restrict __nptr,
                                char **__restrict __endptr, int __base);

#endif

#ifdef __USE_ISOC99

long long int strtoll (const char *__restrict __nptr,
                       char **__restrict __endptr, int __base);

unsigned long long int strtoull (const char *__restrict __nptr,
                                 char **__restrict __endptr, int __base);

#endif

#if __GLIBC_USE (IEC_60559_BFP_EXT)
int strfromd (char *__dest, size_t __size, const char *__format,
              double __f);

int strfromf (char *__dest, size_t __size, const char *__format,
              float __f);

int strfroml (char *__dest, size_t __size, const char *__format,
              long double __f);

#endif


// omitted: __USE_GNU functions


#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED

char *l64a (long int __n);

long int a64l (const char *__s);

#endif

#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED

long int random (void);

void srandom (unsigned int __seed);

char *initstate (unsigned int __seed, char *__statebuf, size_t __statelen);

char *setstate (char *__statebuf);

# ifdef __USE_MISC

int random_r (struct random_data *__restrict __buf,
              int32_t *__restrict __result);

int srandom_r (unsigned int __seed, struct random_data *__buf);

int initstate_r (unsigned int __seed, char *__restrict __statebuf,
                 size_t __statelen,
                 struct random_data *__restrict __buf);

int setstate_r (char *__restrict __statebuf,
                struct random_data *__restrict __buf);

# endif
#endif


int rand (void);

void srand (unsigned int __seed);

#ifdef __USE_POSIX199506

int rand_r (unsigned int *__seed);
#endif

#if defined __USE_MISC || defined __USE_XOPEN

double drand48 (void);

double erand48 (unsigned short int __xsubi[3]);

long int lrand48 (void);

long int nrand48 (unsigned short int __xsubi[3]);

long int mrand48 (void);

long int jrand48 (unsigned short int __xsubi[3]);

void srand48 (long int __seedval);

unsigned short int *seed48 (unsigned short int __seed16v[3]);

void lcong48 (unsigned short int __param[7]);

# ifdef __USE_MISC

int drand48_r (struct drand48_data *__restrict __buffer,
               double *__restrict __result);

int erand48_r (unsigned short int __xsubi[3],
               struct drand48_data *__restrict __buffer,
               double *__restrict __result);

int lrand48_r (struct drand48_data *__restrict __buffer,
               long int *__restrict __result);

int nrand48_r (unsigned short int __xsubi[3],
               struct drand48_data *__restrict __buffer,
               long int *__restrict __result);

int mrand48_r (struct drand48_data *__restrict __buffer,
               long int *__restrict __result);

int jrand48_r (unsigned short int __xsubi[3],
               struct drand48_data *__restrict __buffer,
               long int *__restrict __result);

int srand48_r (long int __seedval, struct drand48_data *__buffer);

int seed48_r (unsigned short int __seed16v[3],
              struct drand48_data *__buffer);

int lcong48_r (unsigned short int __param[7],
               struct drand48_data *__buffer);

# endif
#endif

void *malloc (size_t __size);

void *calloc (size_t __nmemb, size_t __size);

void *realloc (void *__ptr, size_t __size);


#ifdef __USE_GNU

void *reallocarray (void *__ptr, size_t __nmemb, size_t __size);

#endif

void free (void *__ptr);

#if (defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K)    \
  || defined __USE_MISC

void *valloc (size_t __size);

#endif

#ifdef __USE_XOPEN2K

int posix_memalign (void **__memptr, size_t __alignment, size_t __size);

#endif

#ifdef __USE_ISOC11

void *aligned_alloc (size_t __alignment, size_t __size);

#endif

void abort (void);

int atexit (void (*__func) (void));

#if defined __USE_ISOC11 || defined __USE_ISOCXX11

int at_quick_exit (void (*__func) (void)) ;

#endif

#ifdef	__USE_MISC

int on_exit (void (*__func) (int __status, void *__arg), void *__arg);

#endif
void exit (int __status);

#if defined __USE_ISOC11 || defined __USE_ISOCXX11

void quick_exit (int __status);

#endif

#ifdef __USE_ISOC99

void _Exit (int __status);

#endif

char *getenv (const char *__name);

#ifdef __USE_GNU

char *secure_getenv (const char *__name);

#endif

#if defined __USE_MISC || defined __USE_XOPEN

int putenv (char *__string);

#endif

#ifdef __USE_XOPEN2K

int setenv (const char *__name, const char *__value, int __replace);

int unsetenv (const char *__name);

#endif

#ifdef	__USE_MISC

int clearenv (void);

#endif

#if defined __USE_MISC \
    || (defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K8)

char *mktemp (char *__template);

#endif

#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8

int mkstemp (char *__template);

int mkstemp64 (char *__template);

#endif

#ifdef __USE_MISC

int mkstemps (char *__template, int __suffixlen);

int mkstemps64 (char *__template, int __suffixlen);

#endif
  
#ifdef __USE_XOPEN2K8
  
char *mkdtemp (char *__template) ;

#endif

#ifdef __USE_GNU

int mkostemp (char *__template, int __flags);

int mkostemp64 (char *__template, int __flags);

int mkostemps (char *__template, int __suffixlen, int __flags);

int mkostemps64 (char *__template, int __suffixlen, int __flags);

#endif

int system (const char *__command);


#ifdef	__USE_GNU

char *canonicalize_file_name (const char *__name);

#endif

#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED

char *realpath (const char *__restrict __name,
                char *__restrict __resolved);

#endif


void *bsearch (const void *__key, const void *__base,
               size_t __nmemb, size_t __size, __compar_fn_t __compar);

void qsort (void *__base, size_t __nmemb, size_t __size,
            __compar_fn_t __compar);

#ifdef __USE_GNU

void qsort_r (void *__base, size_t __nmemb, size_t __size,
              __compar_d_fn_t __compar, void *__arg);

#endif

int abs (int __x);

long int labs (long int __x);

#ifdef __USE_ISOC99

long long int llabs (long long int __x);

#endif

div_t div (int __numer, int __denom);

ldiv_t ldiv (long int __numer, long int __denom);

#ifdef __USE_ISOC99

lldiv_t lldiv (long long int __numer, long long int __denom);

#endif

#if (defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K8) \
    || defined __USE_MISC

char *ecvt (double __value, int __ndigit, int *__restrict __decpt,
            int *__restrict __sign);

char *fcvt (double __value, int __ndigit, int *__restrict __decpt,
            int *__restrict __sign);

char *gcvt (double __value, int __ndigit, char *__buf);

#endif

#ifdef __USE_MISC

char *qecvt (long double __value, int __ndigit,
             int *__restrict __decpt, int *__restrict __sign);

char *qfcvt (long double __value, int __ndigit,
             int *__restrict __decpt, int *__restrict __sign);

char *qgcvt (long double __value, int __ndigit, char *__buf);

int ecvt_r (double __value, int __ndigit, int *__restrict __decpt,
            int *__restrict __sign, char *__restrict __buf,
            size_t __len);

int fcvt_r (double __value, int __ndigit, int *__restrict __decpt,
            int *__restrict __sign, char *__restrict __buf,
            size_t __len);

int qecvt_r (long double __value, int __ndigit,
             int *__restrict __decpt, int *__restrict __sign,
             char *__restrict __buf, size_t __len);

int qfcvt_r (long double __value, int __ndigit,
             int *__restrict __decpt, int *__restrict __sign,
             char *__restrict __buf, size_t __len);

#endif


int mblen (const char *__s, size_t __n);

int mbtowc (wchar_t *__restrict __pwc,
            const char *__restrict __s, size_t __n);

int wctomb (char *__s, wchar_t __wchar);


size_t mbstowcs (wchar_t *__restrict  __pwcs,
                 const char *__restrict __s, size_t __n);

size_t wcstombs (char *__restrict __s,
                 const wchar_t *__restrict __pwcs, size_t __n);

#ifdef __USE_MISC

int rpmatch (const char *__response);

#endif


#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8

int getsubopt (char **__restrict __optionp,
               char *const *__restrict __tokens,
               char **__restrict __valuep);

#endif


#ifdef __USE_XOPEN

void setkey (const char *__key);

#endif


#ifdef __USE_XOPEN2KXSI

int posix_openpt (int __oflag);

#endif

#ifdef __USE_XOPEN_EXTENDED

int grantpt (int __fd);

int unlockpt (int __fd);

char *ptsname (int __fd) ;

#endif

#ifdef __USE_GNU

int ptsname_r (int __fd, char *__buf, size_t __buflen);

int getpt (void);

#endif

#ifdef __USE_MISC

int getloadavg (double __loadavg[], int __nelem);

#endif

#if defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K

int ttyslot (void);

#endif

