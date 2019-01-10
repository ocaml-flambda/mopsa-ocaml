/****************************************************************************/
/*                   Copyright (C) 2018 The MOPSA Project                   */
/*                                                                          */
/*   This program is free software: you can redistribute it and/or modify   */
/*   it under the terms of the CeCILL license V2.1.                         */
/*                                                                          */
/****************************************************************************/

/* Stubs for the extension of stdio as found in Sun's Solaris OS */

#include <stdio_ext.h>
#include <limits.h>

/*$
 * requires: __fp in File;
 * ensures:  return in [0, INT_MAX];
 */
size_t __fbufsize (FILE *__fp);


/*$
 * requires: __fp in File;
 * ensures:  return in [0, INT_MAX];
 */
int __freading (FILE *__fp);

/*$
 * requires: __fp in File;
 * ensures:  return in [0, INT_MAX];
 */
int __fwriting (FILE *__fp);


/*$
 * requires: __fp in File;
 * ensures:  return in [0, INT_MAX];
 */
int __freadable (FILE *__fp);

/*$
 * requires: __fp in File;
 * ensures:  return in [0, INT_MAX];
 */
int __fwritable (FILE *__fp);

/*$
 * requires: __fp in File;
 * ensures:  return in [0, INT_MAX];
 */
int __flbf (FILE *__fp);


/*$
 * requires: __fp in File;
 */
void __fpurge (FILE *__fp);

/*$
 * requires: __fp in File;
 * ensures:  return in [0, INT_MAX];
 */
size_t __fpending (FILE *__fp);

/*$
 * // emtpy stub
 */
void _flushlbf (void);


/*$
 * requires: __fp in File;
 * ensures:  return == FSETLOCKING_INTERNAL or
             return == FSETLOCKING_BYCALLER;
 */
int __fsetlocking (FILE *__fp, int __type);
