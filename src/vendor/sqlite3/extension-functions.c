/*
This library will provide common mathematical and string functions in
SQL queries using the operating system libraries or provided
definitions.  It includes the following functions:

Math: acos, asin, atan, atn2, atan2, acosh, asinh, atanh, difference,
degrees, radians, cos, sin, tan, cot, cosh, sinh, tanh, coth, exp,
log, log10, power, sign, sqrt, square, ceil, floor, pi.

String: replicate, charindex, leftstr, rightstr, ltrim, rtrim, trim,
replace, reverse, proper, padl, padr, padc, strfilter.

Aggregate: stdev, variance, mode, median, lower_quartile,
upper_quartile.

The string functions ltrim, rtrim, trim, replace are included in
recent versions of SQLite and so by default do not build.

Compilation instructions:
 Compile this C source file into a dynamic library as follows:
 * Linux:
   gcc -fPIC -lm -shared extension-functions.c -o libsqlitefunctions.so
 * Mac OS X:
   gcc -fno-common -dynamiclib extension-functions.c -o libsqlitefunctions.dylib
 (You may need to add flags
  -I /opt/local/include/ -L/opt/local/lib -lsqlite3
  if your sqlite3 is installed from Mac ports, or
  -I /sw/include/ -L/sw/lib -lsqlite3
  if installed with Fink.)
 * Windows:
  1. Install MinGW (http://www.mingw.org/) and you will get the gcc
  (gnu compiler collection)
  2. add the path to your path variable (isn't done during the
   installation!)
  3. compile:
   gcc -shared -I "path" -o libsqlitefunctions.so extension-functions.c
   (path = path of sqlite3ext.h; i.e. C:\programs\sqlite)

Usage instructions for applications calling the sqlite3 API functions:
  In your application, call sqlite3_enable_load_extension(db,1) to
  allow loading external libraries.  Then load the library libsqlitefunctions
  using sqlite3_load_extension; the third argument should be 0.
  See http://www.sqlite.org/cvstrac/wiki?p=LoadableExtensions.
  Select statements may now use these functions, as in
  SELECT cos(radians(inclination)) FROM satsum WHERE satnum = 25544;

Usage instructions for the sqlite3 program:
  If the program is built so that loading extensions is permitted,
  the following will work:
   sqlite> SELECT load_extension('./libsqlitefunctions.so');
   sqlite> select cos(radians(45));
   0.707106781186548
  Note: Loading extensions is by default prohibited as a
  security measure; see "Security Considerations" in
  http://www.sqlite.org/cvstrac/wiki?p=LoadableExtensions.
  If the sqlite3 program and library are built this
  way, you cannot use these functions from the program, you
  must write your own program using the sqlite3 API, and call
  sqlite3_enable_load_extension as described above, or else
  rebuilt the sqlite3 program to allow loadable extensions.

Alterations:
The instructions are for Linux, Mac OS X, and Windows; users of other
OSes may need to modify this procedure.  In particular, if your math
library lacks one or more of the needed trig or log functions, comment
out the appropriate HAVE_ #define at the top of file.  If you do not
wish to make a loadable module, comment out the define for
COMPILE_SQLITE_EXTENSIONS_AS_LOADABLE_MODULE.  If you are using a
version of SQLite without the trim functions and replace, comment out
the HAVE_TRIM #define.

Liam Healy

History:
2010-01-06 Correct check for argc in squareFunc, and add Windows
compilation instructions.
2009-06-24 Correct check for argc in properFunc.
2008-09-14 Add check that memory was actually allocated after
sqlite3_malloc or sqlite3StrDup, call sqlite3_result_error_nomem if
not.  Thanks to Robert Simpson.
2008-06-13 Change to instructions to indicate use of the math library
and that program might work.
2007-10-01 Minor clarification to instructions.
2007-09-29 Compilation as loadable module is optional with
COMPILE_SQLITE_EXTENSIONS_AS_LOADABLE_MODULE.
2007-09-28 Use sqlite3_extension_init and macros
SQLITE_EXTENSION_INIT1, SQLITE_EXTENSION_INIT2, so that it works with
sqlite3_load_extension.  Thanks to Eric Higashino and Joe Wilson.
New instructions for Mac compilation.
2007-09-17 With help from Joe Wilson and Nuno Luca, made use of
external interfaces so that compilation is no longer dependent on
SQLite source code.  Merged source, header, and README into a single
file.  Added casts so that Mac will compile without warnings (unsigned
and signed char).
2007-09-05 Included some definitions from sqlite 3.3.13 so that this
will continue to work in newer versions of sqlite.  Completed
description of functions available.
2007-03-27 Revised description.
2007-03-23 Small cleanup and a bug fix on the code.  This was mainly
letting errno flag errors encountered in the math library and checking
the result, rather than pre-checking.  This fixes a bug in power that
would cause an error if any non-positive number was raised to any
power.
2007-02-07 posted by Mikey C to sqlite mailing list.
Original code 2006 June 05 by relicoder.

*/

//#include "config.h"

#define COMPILE_SQLITE_EXTENSIONS_AS_LOADABLE_MODULE 1
#define HAVE_ACOSH 1
#define HAVE_ASINH 1
#define HAVE_ATANH 1
#define HAVE_SINH 1
#define HAVE_COSH 1
#define HAVE_TANH 1
#define HAVE_LOG10 1
#define HAVE_ISBLANK 1
#define SQLITE_SOUNDEX 1
#define HAVE_TRIM 1		/* LMH 2007-03-25 if sqlite has trim functions */

#ifdef COMPILE_SQLITE_EXTENSIONS_AS_LOADABLE_MODULE
#include "sqlite3ext.h"
SQLITE_EXTENSION_INIT1
#else
#include "sqlite3.h"
#endif

#include <ctype.h>
/* relicoder */
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>		/* LMH 2007-03-25 */

#include <stdlib.h>
#include <assert.h>

#ifndef _MAP_H_
#define _MAP_H_

#include <stdint.h>

/*
** Simple binary tree implementation to use in median, mode and quartile calculations
** Tree is not necessarily balanced. That would require something like red&black trees of AVL
*/

typedef int(*cmp_func)(const void *, const void *);
typedef void(*map_iterator)(void*, int64_t, void*);

typedef struct node{
  struct node *l;
  struct node *r;
  void* data;
  int64_t count;
} node;

typedef struct map{
  node *base;
  cmp_func cmp;
} map;

/*
** creates a map given a comparison function
*/
map map_make(cmp_func cmp);

/*
** inserts the element e into map m
*/
void map_insert(map *m, void *e);

/*
** executes function iter over all elements in the map, in key increasing order
*/
void map_iterate(map *m, map_iterator iter, void* p);

/*
** frees all memory used by a map
*/
void map_destroy(map *m);

/*
** compares 2 integers
** to use with map_make
*/
int int_cmp(const void *a, const void *b);

/*
** compares 2 doubles
** to use with map_make
*/
int double_cmp(const void *a, const void *b);

#endif /* _MAP_H_ */

typedef uint8_t         u8;
typedef uint16_t        u16;
typedef int64_t         i64;

static char *sqlite3StrDup( const char *z ) {
    char *res = sqlite3_malloc( strlen(z)+1 );
    return strcpy( res, z );
}

/*
** These are copied verbatim from fun.c so as to not have the names exported
*/

/* LMH from sqlite3 3.3.13 */
/*
** This table maps from the first byte of a UTF-8 character to the number
** of trailing bytes expected. A value '4' indicates that the table key
** is not a legal first byte for a UTF-8 character.
*/
static const u8 xtra_utf8_bytes[256]  = {
/* 0xxxxxxx */
0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,     0, 0, 0, 0, 0, 0, 0, 0,

/* 10wwwwww */
4, 4, 4, 4, 4, 4, 4, 4,     4, 4, 4, 4, 4, 4, 4, 4,
4, 4, 4, 4, 4, 4, 4, 4,     4, 4, 4, 4, 4, 4, 4, 4,
4, 4, 4, 4, 4, 4, 4, 4,     4, 4, 4, 4, 4, 4, 4, 4,
4, 4, 4, 4, 4, 4, 4, 4,     4, 4, 4, 4, 4, 4, 4, 4,

/* 110yyyyy */
1, 1, 1, 1, 1, 1, 1, 1,     1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1,     1, 1, 1, 1, 1, 1, 1, 1,

/* 1110zzzz */
2, 2, 2, 2, 2, 2, 2, 2,     2, 2, 2, 2, 2, 2, 2, 2,

/* 11110yyy */
3, 3, 3, 3, 3, 3, 3, 3,     4, 4, 4, 4, 4, 4, 4, 4,
};


/*
** This table maps from the number of trailing bytes in a UTF-8 character
** to an integer constant that is effectively calculated for each character
** read by a naive implementation of a UTF-8 character reader. The code
** in the READ_UTF8 macro explains things best.
*/
static const int xtra_utf8_bits[] =  {
  0,
  12416,          /* (0xC0 << 6) + (0x80) */
  925824,         /* (0xE0 << 12) + (0x80 << 6) + (0x80) */
  63447168        /* (0xF0 << 18) + (0x80 << 12) + (0x80 << 6) + 0x80 */
};

/*
** If a UTF-8 character contains N bytes extra bytes (N bytes follow
** the initial byte so that the total character length is N+1) then
** masking the character with utf8_mask[N] must produce a non-zero
** result.  Otherwise, we have an (illegal) overlong encoding.
*/
static const int utf_mask[] = {
  0x00000000,
  0xffffff80,
  0xfffff800,
  0xffff0000,
};

/* LMH salvaged from sqlite3 3.3.13 source code src/utf.c */
#define READ_UTF8(zIn, c) { \
  int xtra;                                            \
  c = *(zIn)++;                                        \
  xtra = xtra_utf8_bytes[c];                           \
  switch( xtra ){                                      \
    case 4: c = (int)0xFFFD; break;                    \
    case 3: c = (c<<6) + *(zIn)++;                     \
    case 2: c = (c<<6) + *(zIn)++;                     \
    case 1: c = (c<<6) + *(zIn)++;                     \
    c -= xtra_utf8_bits[xtra];                         \
    if( (utf_mask[xtra]&c)==0                          \
        || (c&0xFFFFF800)==0xD800                      \
        || (c&0xFFFFFFFE)==0xFFFE ){  c = 0xFFFD; }    \
  }                                                    \
}

static int sqlite3ReadUtf8(const unsigned char *z){
  int c;
  READ_UTF8(z, c);
  return c;
}

#define SKIP_UTF8(zIn) {                               \
  zIn += (xtra_utf8_bytes[*(u8 *)zIn] + 1);            \
}

/*
** pZ is a UTF-8 encoded unicode string. If nByte is less than zero,
** return the number of unicode characters in pZ up to (but not including)
** the first 0x00 byte. If nByte is not less than zero, return the
** number of unicode characters in the first nByte of pZ (or up to
** the first 0x00, whichever comes first).
*/
static int sqlite3Utf8CharLen(const char *z, int nByte){
  int r = 0;
  const char *zTerm;
  if( nByte>=0 ){
    zTerm = &z[nByte];
  }else{
    zTerm = (const char *)(-1);
  }
  assert( z<=zTerm );
  while( *z!=0 && z<zTerm ){
    SKIP_UTF8(z);
    r++;
  }
  return r;
}

/*
** X is a pointer to the first byte of a UTF-8 character.  Increment
** X so that it points to the next character.  This only works right
** if X points to a well-formed UTF-8 string.
*/
#define sqliteNextChar(X)  while( (0xc0&*++(X))==0x80 ){}
#define sqliteCharVal(X)   sqlite3ReadUtf8(X)

/*
** This is a macro that facilitates writting wrappers for math.h functions
** it creates code for a function to use in SQlite that gets one numeric input
** and returns a floating point value.
**
** Could have been implemented using pointers to functions but this way it's inline
** and thus more efficient. Lower * ranking though...
**
** Parameters:
** name:      function name to de defined (eg: sinFunc)
** function:  function defined in math.h to wrap (eg: sin)
** domain:    boolean condition that CAN'T happen in terms of the input parameter rVal
**            (eg: rval<0 for sqrt)
*/
/* LMH 2007-03-25 Changed to use errno and remove domain; no pre-checking for errors. */
#define GEN_MATH_WRAP_DOUBLE_1(name, function) \
static void name(sqlite3_context *context, int argc, sqlite3_value **argv){\
  double rVal = 0.0, val;\
  assert( argc==1 );\
  switch( sqlite3_value_type(argv[0]) ){\
    case SQLITE_NULL: {\
      sqlite3_result_null(context);\
      break;\
    }\
    default: {\
      rVal = sqlite3_value_double(argv[0]);\
      errno = 0;\
      val = function(rVal);\
      if (errno == 0) {\
        sqlite3_result_double(context, val);\
      } else {\
        sqlite3_result_error(context, strerror(errno), errno);\
      }\
      break;\
    }\
  }\
}\


/*
** Example of GEN_MATH_WRAP_DOUBLE_1 usage
** this creates function sqrtFunc to wrap the math.h standard function sqrt(x)=x^0.5
*/
GEN_MATH_WRAP_DOUBLE_1(sqrtFunc, sqrt)

/* trignometric functions */
GEN_MATH_WRAP_DOUBLE_1(acosFunc, acos)
GEN_MATH_WRAP_DOUBLE_1(asinFunc, asin)
GEN_MATH_WRAP_DOUBLE_1(atanFunc, atan)

/*
** Many of systems don't have inverse hyperbolic trig functions so this will emulate
** them on those systems in terms of log and sqrt (formulas are too trivial to demand
** written proof here)
*/

#ifndef HAVE_ACOSH
static double acosh(double x){
  return log(x + sqrt(x*x - 1.0));
}
#endif

GEN_MATH_WRAP_DOUBLE_1(acoshFunc, acosh)

#ifndef HAVE_ASINH
static double asinh(double x){
  return log(x + sqrt(x*x + 1.0));
}
#endif

GEN_MATH_WRAP_DOUBLE_1(asinhFunc, asinh)

#ifndef HAVE_ATANH
static double atanh(double x){
  return (1.0/2.0)*log((1+x)/(1-x)) ;
}
#endif

GEN_MATH_WRAP_DOUBLE_1(atanhFunc, atanh)

/*
** math.h doesn't require cot (cotangent) so it's defined here
*/
static double cot(double x){
  return 1.0/tan(x);
}

GEN_MATH_WRAP_DOUBLE_1(sinFunc, sin)
GEN_MATH_WRAP_DOUBLE_1(cosFunc, cos)
GEN_MATH_WRAP_DOUBLE_1(tanFunc, tan)
GEN_MATH_WRAP_DOUBLE_1(cotFunc, cot)

static double coth(double x){
  return 1.0/tanh(x);
}

/*
** Many systems don't have hyperbolic trigonometric functions so this will emulate
** them on those systems directly from the definition in terms of exp
*/
#ifndef HAVE_SINH
static double sinh(double x){
  return (exp(x)-exp(-x))/2.0;
}
#endif

GEN_MATH_WRAP_DOUBLE_1(sinhFunc, sinh)

#ifndef HAVE_COSH
static double cosh(double x){
  return (exp(x)+exp(-x))/2.0;
}
#endif

GEN_MATH_WRAP_DOUBLE_1(coshFunc, cosh)

#ifndef HAVE_TANH
static double tanh(double x){
  return sinh(x)/cosh(x);
}
#endif

GEN_MATH_WRAP_DOUBLE_1(tanhFunc, tanh)

GEN_MATH_WRAP_DOUBLE_1(cothFunc, coth)

/*
** Some systems lack log in base 10. This will emulate it
*/

#ifndef HAVE_LOG10
static double log10(double x){
  static double l10 = -1.0;
  if( l10<0.0 ){
    l10 = log(10.0);
  }
  return log(x)/l10;
}
#endif

GEN_MATH_WRAP_DOUBLE_1(logFunc, log)
GEN_MATH_WRAP_DOUBLE_1(log10Func, log10)
GEN_MATH_WRAP_DOUBLE_1(expFunc, exp)

/*
** Fallback for systems where math.h doesn't define M_PI
*/
#undef M_PI
#ifndef M_PI
/*
** static double PI = acos(-1.0);
** #define M_PI (PI)
*/
#define M_PI 3.14159265358979323846
#endif

/* Convert Degrees into Radians */
static double deg2rad(double x){
  return x*M_PI/180.0;
}

/* Convert Radians into Degrees */
static double rad2deg(double x){
  return 180.0*x/M_PI;
}

GEN_MATH_WRAP_DOUBLE_1(rad2degFunc, rad2deg)
GEN_MATH_WRAP_DOUBLE_1(deg2radFunc, deg2rad)

/* constant function that returns the value of PI=3.1415... */
static void piFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  sqlite3_result_double(context, M_PI);
}

/*
** Implements the sqrt function, it has the peculiarity of returning an integer when the
** the argument is an integer.
** Since SQLite isn't strongly typed (almost untyped actually) this is a bit pedantic
*/
static void squareFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  i64 iVal = 0;
  double rVal = 0.0;
  assert( argc==1 );
  switch( sqlite3_value_type(argv[0]) ){
    case SQLITE_INTEGER: {
      iVal = sqlite3_value_int64(argv[0]);
      sqlite3_result_int64(context, iVal*iVal);
      break;
    }
    case SQLITE_NULL: {
      sqlite3_result_null(context);
      break;
    }
    default: {
      rVal = sqlite3_value_double(argv[0]);
      sqlite3_result_double(context, rVal*rVal);
      break;
    }
  }
}

/*
** Wraps the pow math.h function
** When both the base and the exponent are integers the result should be integer
** (see sqrt just before this). Here the result is always double
*/
/* LMH 2007-03-25 Changed to use errno; no pre-checking for errors.  Also removes
  but that was present in the pre-checking that called sqlite3_result_error on
  a non-positive first argument, which is not always an error. */
static void powerFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  double r1 = 0.0;
  double r2 = 0.0;
  double val;

  assert( argc==2 );

  if( sqlite3_value_type(argv[0]) == SQLITE_NULL || sqlite3_value_type(argv[1]) == SQLITE_NULL ){
    sqlite3_result_null(context);
  }else{
    r1 = sqlite3_value_double(argv[0]);
    r2 = sqlite3_value_double(argv[1]);
    errno = 0;
    val = pow(r1,r2);
    if (errno == 0) {
      sqlite3_result_double(context, val);
    } else {
      sqlite3_result_error(context, strerror(errno), errno);
    }
  }
}

/*
** atan2 wrapper
*/
static void atn2Func(sqlite3_context *context, int argc, sqlite3_value **argv){
  double r1 = 0.0;
  double r2 = 0.0;

  assert( argc==2 );

  if( sqlite3_value_type(argv[0]) == SQLITE_NULL || sqlite3_value_type(argv[1]) == SQLITE_NULL ){
    sqlite3_result_null(context);
  }else{
    r1 = sqlite3_value_double(argv[0]);
    r2 = sqlite3_value_double(argv[1]);
    sqlite3_result_double(context, atan2(r1,r2));
  }
}

/*
** Implementation of the sign() function
** return one of 3 possibilities +1,0 or -1 when the argument is respectively
** positive, 0 or negative.
** When the argument is NULL the result is also NULL (completly conventional)
*/
static void signFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  double rVal=0.0;
  i64 iVal=0;
  assert( argc==1 );
  switch( sqlite3_value_type(argv[0]) ){
    case SQLITE_INTEGER: {
      iVal = sqlite3_value_int64(argv[0]);
      iVal = ( iVal > 0) ? 1: ( iVal < 0 ) ? -1: 0;
      sqlite3_result_int64(context, iVal);
      break;
    }
    case SQLITE_NULL: {
      sqlite3_result_null(context);
      break;
    }
    default: {
 /* 2nd change below. Line for abs was: if( rVal<0 ) rVal = rVal * -1.0;  */

      rVal = sqlite3_value_double(argv[0]);
      rVal = ( rVal > 0) ? 1: ( rVal < 0 ) ? -1: 0;
      sqlite3_result_double(context, rVal);
      break;
    }
  }
}


/*
** smallest integer value not less than argument
*/
static void ceilFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  double rVal=0.0;
  assert( argc==1 );
  switch( sqlite3_value_type(argv[0]) ){
    case SQLITE_INTEGER: {
      i64 iVal = sqlite3_value_int64(argv[0]);
      sqlite3_result_int64(context, iVal);
      break;
    }
    case SQLITE_NULL: {
      sqlite3_result_null(context);
      break;
    }
    default: {
      rVal = sqlite3_value_double(argv[0]);
      sqlite3_result_int64(context, (i64) ceil(rVal));
      break;
    }
  }
}

/*
** largest integer value not greater than argument
*/
static void floorFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  double rVal=0.0;
  assert( argc==1 );
  switch( sqlite3_value_type(argv[0]) ){
    case SQLITE_INTEGER: {
      i64 iVal = sqlite3_value_int64(argv[0]);
      sqlite3_result_int64(context, iVal);
      break;
    }
    case SQLITE_NULL: {
      sqlite3_result_null(context);
      break;
    }
    default: {
      rVal = sqlite3_value_double(argv[0]);
      sqlite3_result_int64(context, (i64) floor(rVal));
      break;
    }
  }
}

/*
** Given a string (s) in the first argument and an integer (n) in the second returns the
** string that constains s contatenated n times
*/
static void replicateFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  unsigned char *z;        /* input string */
  unsigned char *zo;       /* result string */
  i64 iCount;              /* times to repeat */
  i64 nLen;                /* length of the input string (no multibyte considerations) */
  i64 nTLen;               /* length of the result string (no multibyte considerations) */
  i64 i=0;

  if( argc!=2 || SQLITE_NULL==sqlite3_value_type(argv[0]) )
    return;

  iCount = sqlite3_value_int64(argv[1]);

  if( iCount<0 ){
    sqlite3_result_error(context, "domain error", -1);
  }else{

    nLen  = sqlite3_value_bytes(argv[0]);
    nTLen = nLen*iCount;
    z=sqlite3_malloc(nTLen+1);
    zo=sqlite3_malloc(nLen+1);
    if (!z || !zo){
      sqlite3_result_error_nomem(context);
      if (z) sqlite3_free(z);
      if (zo) sqlite3_free(zo);
      return;
    }
    strcpy((char*)zo, (char*)sqlite3_value_text(argv[0]));

    for(i=0; i<iCount; ++i){
      strcpy((char*)(z+i*nLen), (char*)zo);
    }

    sqlite3_result_text(context, (char*)z, -1, SQLITE_TRANSIENT);
    sqlite3_free(z);
    sqlite3_free(zo);
  }
}

/*
** Some systems (win32 among others) don't have an isblank function, this will emulate it.
** This function is not UFT-8 safe since it only analyses a byte character.
*/
#ifndef HAVE_ISBLANK
int isblank(char c){
  return( ' '==c || '\t'==c );
}
#endif

static void properFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  const unsigned char *z;     /* input string */
  unsigned char *zo;          /* output string */
  unsigned char *zt;          /* iterator */
  char r;
  int c=1;

  assert( argc==1);
  if( SQLITE_NULL==sqlite3_value_type(argv[0]) ){
    sqlite3_result_null(context);
    return;
  }

  z = sqlite3_value_text(argv[0]);
  zo = (unsigned char *)sqlite3StrDup((char *) z);
  if (!zo) {
    sqlite3_result_error_nomem(context);
    return;
  }
  zt = zo;

  while( (r = *(z++))!=0 ){
    if( isblank(r) ){
      c=1;
    }else{
      if( c==1 ){
        r = toupper(r);
      }else{
        r = tolower(r);
      }
      c=0;
    }
    *(zt++) = r;
  }
  *zt = '\0';

  sqlite3_result_text(context, (char*)zo, -1, SQLITE_TRANSIENT);
  sqlite3_free(zo);
}

/*
** given an input string (s) and an integer (n) adds spaces at the begining of  s
** until it has a length of n characters.
** When s has a length >=n it's a NOP
** padl(NULL) = NULL
*/
static void padlFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  i64 ilen;          /* length to pad to */
  i64 zl;            /* length of the input string (UTF-8 chars) */
  int i = 0;
  const char *zi;    /* input string */
  char *zo;          /* output string */
  char *zt;

  assert( argc==2 );

  if( sqlite3_value_type(argv[0]) == SQLITE_NULL ){
    sqlite3_result_null(context);
  }else{
    zi = (char *)sqlite3_value_text(argv[0]);
    ilen = sqlite3_value_int64(argv[1]);
    /* check domain */
    if(ilen<0){
      sqlite3_result_error(context, "domain error", -1);
      return;
    }
    zl = sqlite3Utf8CharLen(zi, -1);
    if( zl>=ilen ){
      /* string is longer than the requested pad length, return the same string (dup it) */
      zo = sqlite3StrDup(zi);
      if (!zo){
        sqlite3_result_error_nomem(context);
        return;
      }
      sqlite3_result_text(context, zo, -1, SQLITE_TRANSIENT);
    }else{
      zo = sqlite3_malloc(strlen(zi)+ilen-zl+1);
      if (!zo){
        sqlite3_result_error_nomem(context);
        return;
      }
      zt = zo;
      for(i=1; i+zl<=ilen; ++i){
        *(zt++)=' ';
      }
      /* no need to take UTF-8 into consideration here */
      strcpy(zt,zi);
    }
    sqlite3_result_text(context, zo, -1, SQLITE_TRANSIENT);
    sqlite3_free(zo);
  }
}

/*
** given an input string (s) and an integer (n) appends spaces at the end of  s
** until it has a length of n characters.
** When s has a length >=n it's a NOP
** padl(NULL) = NULL
*/
static void padrFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  i64 ilen;          /* length to pad to */
  i64 zl;            /* length of the input string (UTF-8 chars) */
  i64 zll;           /* length of the input string (bytes) */
  int i = 0;
  const char *zi;    /* input string */
  char *zo;          /* output string */
  char *zt;

  assert( argc==2 );

  if( sqlite3_value_type(argv[0]) == SQLITE_NULL ){
    sqlite3_result_null(context);
  }else{
    zi = (char *)sqlite3_value_text(argv[0]);
    ilen = sqlite3_value_int64(argv[1]);
    /* check domain */
    if(ilen<0){
      sqlite3_result_error(context, "domain error", -1);
      return;
    }
    zl = sqlite3Utf8CharLen(zi, -1);
    if( zl>=ilen ){
      /* string is longer than the requested pad length, return the same string (dup it) */
      zo = sqlite3StrDup(zi);
      if (!zo){
        sqlite3_result_error_nomem(context);
        return;
      }
      sqlite3_result_text(context, zo, -1, SQLITE_TRANSIENT);
    }else{
      zll = strlen(zi);
      zo = sqlite3_malloc(zll+ilen-zl+1);
      if (!zo){
        sqlite3_result_error_nomem(context);
        return;
      }
      zt = strcpy(zo,zi)+zll;
      for(i=1; i+zl<=ilen; ++i){
        *(zt++) = ' ';
      }
      *zt = '\0';
    }
    sqlite3_result_text(context, zo, -1, SQLITE_TRANSIENT);
    sqlite3_free(zo);
  }
}

/*
** given an input string (s) and an integer (n) appends spaces at the end of  s
** and adds spaces at the begining of s until it has a length of n characters.
** Tries to add has many characters at the left as at the right.
** When s has a length >=n it's a NOP
** padl(NULL) = NULL
*/
static void padcFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  i64 ilen;           /* length to pad to */
  i64 zl;             /* length of the input string (UTF-8 chars) */
  i64 zll;            /* length of the input string (bytes) */
  int i = 0;
  const char *zi;     /* input string */
  char *zo;           /* output string */
  char *zt;

  assert( argc==2 );

  if( sqlite3_value_type(argv[0]) == SQLITE_NULL ){
    sqlite3_result_null(context);
  }else{
    zi = (char *)sqlite3_value_text(argv[0]);
    ilen = sqlite3_value_int64(argv[1]);
    /* check domain */
    if(ilen<0){
      sqlite3_result_error(context, "domain error", -1);
      return;
    }
    zl = sqlite3Utf8CharLen(zi, -1);
    if( zl>=ilen ){
      /* string is longer than the requested pad length, return the same string (dup it) */
      zo = sqlite3StrDup(zi);
      if (!zo){
        sqlite3_result_error_nomem(context);
        return;
      }
      sqlite3_result_text(context, zo, -1, SQLITE_TRANSIENT);
    }else{
      zll = strlen(zi);
      zo = sqlite3_malloc(zll+ilen-zl+1);
      if (!zo){
        sqlite3_result_error_nomem(context);
        return;
      }
      zt = zo;
      for(i=1; 2*i+zl<=ilen; ++i){
        *(zt++) = ' ';
      }
      strcpy(zt, zi);
      zt+=zll;
      for(; i+zl<=ilen; ++i){
        *(zt++) = ' ';
      }
      *zt = '\0';
    }
    sqlite3_result_text(context, zo, -1, SQLITE_TRANSIENT);
    sqlite3_free(zo);
  }
}

/*
** given 2 string (s1,s2) returns the string s1 with the characters NOT in s2 removed
** assumes strings are UTF-8 encoded
*/
static void strfilterFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  const char *zi1;        /* first parameter string (searched string) */
  const char *zi2;        /* second parameter string (vcontains valid characters) */
  const char *z1;
  const char *z21;
  const char *z22;
  char *zo;               /* output string */
  char *zot;
  int c1 = 0;
  int c2 = 0;

  assert( argc==2 );

  if( sqlite3_value_type(argv[0]) == SQLITE_NULL || sqlite3_value_type(argv[1]) == SQLITE_NULL ){
    sqlite3_result_null(context);
  }else{
    zi1 = (char *)sqlite3_value_text(argv[0]);
    zi2 = (char *)sqlite3_value_text(argv[1]);
    /*
    ** maybe I could allocate less, but that would imply 2 passes, rather waste
    ** (possibly) some memory
    */
    zo = sqlite3_malloc(strlen(zi1)+1);
    if (!zo){
      sqlite3_result_error_nomem(context);
      return;
    }
    zot = zo;
    z1 = zi1;
    while( (c1=sqliteCharVal((unsigned char *)z1))!=0 ){
      z21=zi2;
      while( (c2=sqliteCharVal((unsigned char *)z21))!=0 && c2!=c1 ){
        sqliteNextChar(z21);
      }
      if( c2!=0){
        z22=z21;
        sqliteNextChar(z22);
        strncpy(zot, z21, z22-z21);
        zot+=z22-z21;
      }
      sqliteNextChar(z1);
    }
    *zot = '\0';

    sqlite3_result_text(context, zo, -1, SQLITE_TRANSIENT);
    sqlite3_free(zo);
  }
}

/*
** Given a string z1, retutns the (0 based) index of it's first occurence
** in z2 after the first s characters.
** Returns -1 when there isn't a match.
** updates p to point to the character where the match occured.
** This is an auxiliary function.
*/
static int _substr(const char* z1, const char* z2, int s, const char** p){
  int c = 0;
  int rVal=-1;
  const char* zt1;
  const char* zt2;
  int c1,c2;

  if( '\0'==*z1 ){
    return -1;
  }

  while( (sqliteCharVal((unsigned char *)z2) != 0) && (c++)<s){
    sqliteNextChar(z2);
  }

  c = 0;
  while( (sqliteCharVal((unsigned char *)z2)) != 0 ){
    zt1 = z1;
    zt2 = z2;

    do{
      c1 = sqliteCharVal((unsigned char *)zt1);
      c2 = sqliteCharVal((unsigned char *)zt2);
      sqliteNextChar(zt1);
      sqliteNextChar(zt2);
    }while( c1 == c2 && c1 != 0 && c2 != 0 );

    if( c1 == 0 ){
      rVal = c;
      break;
    }

    sqliteNextChar(z2);
    ++c;
  }
  if(p){
    *p=z2;
  }
  return rVal >=0 ? rVal+s : rVal;
}

/*
** given 2 input strings (s1,s2) and an integer (n) searches from the nth character
** for the string s1. Returns the position where the match occured.
** Characters are counted from 1.
** 0 is returned when no match occurs.
*/

static void charindexFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  const u8 *z1;          /* s1 string */
  u8 *z2;                /* s2 string */
  int s=0;
  int rVal=0;

  assert( argc==3 ||argc==2);

  if( SQLITE_NULL==sqlite3_value_type(argv[0]) || SQLITE_NULL==sqlite3_value_type(argv[1])){
    sqlite3_result_null(context);
    return;
  }

  z1 = sqlite3_value_text(argv[0]);
  if( z1==0 ) return;
  z2 = (u8*) sqlite3_value_text(argv[1]);
  if(argc==3){
    s = sqlite3_value_int(argv[2])-1;
    if(s<0){
      s=0;
    }
  }else{
    s = 0;
  }

  rVal = _substr((char *)z1,(char *)z2,s,NULL);
  sqlite3_result_int(context, rVal+1);
}

/*
** given a string (s) and an integer (n) returns the n leftmost (UTF-8) characters
** if the string has a length<=n or is NULL this function is NOP
*/
static void leftFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  int c=0;
  int cc=0;
  int l=0;
  const unsigned char *z;       /* input string */
  const unsigned char *zt;
  unsigned char *rz;            /* output string */

  assert( argc==2);

  if( SQLITE_NULL==sqlite3_value_type(argv[0]) || SQLITE_NULL==sqlite3_value_type(argv[1])){
    sqlite3_result_null(context);
    return;
  }

  z  = sqlite3_value_text(argv[0]);
  l  = sqlite3_value_int(argv[1]);
  zt = z;

  while( sqliteCharVal(zt) && c++<l)
    sqliteNextChar(zt);

  cc=zt-z;

  rz = sqlite3_malloc(zt-z+1);
  if (!rz){
    sqlite3_result_error_nomem(context);
    return;
  }
  strncpy((char*) rz, (char*) z, zt-z);
  *(rz+cc) = '\0';
  sqlite3_result_text(context, (char*)rz, -1, SQLITE_TRANSIENT);
  sqlite3_free(rz);
}

/*
** given a string (s) and an integer (n) returns the n rightmost (UTF-8) characters
** if the string has a length<=n or is NULL this function is NOP
*/
static void rightFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  int l=0;
  int c=0;
  int cc=0;
  const char *z;
  const char *zt;
  const char *ze;
  char *rz;

  assert( argc==2);

  if( SQLITE_NULL == sqlite3_value_type(argv[0]) || SQLITE_NULL == sqlite3_value_type(argv[1])){
    sqlite3_result_null(context);
    return;
  }

  z  = (char *)sqlite3_value_text(argv[0]);
  l  = sqlite3_value_int(argv[1]);
  zt = z;

  while( sqliteCharVal((unsigned char *)zt)!=0){
    sqliteNextChar(zt);
    ++c;
  }

  ze = zt;
  zt = z;

  cc=c-l;
  if(cc<0)
    cc=0;

  while( cc-- > 0 ){
    sqliteNextChar(zt);
  }

  rz = sqlite3_malloc(ze-zt+1);
  if (!rz){
    sqlite3_result_error_nomem(context);
    return;
  }
  strcpy((char*) rz, (char*) (zt));
  sqlite3_result_text(context, (char*)rz, -1, SQLITE_TRANSIENT);
  sqlite3_free(rz);
}

#ifndef HAVE_TRIM
/*
** removes the whitespaces at the begining of a string.
*/
const char* ltrim(const char* s){
  while( *s==' ' )
    ++s;
  return s;
}

/*
** removes the whitespaces at the end of a string.
** !mutates the input string!
*/
void rtrim(char* s){
  char* ss = s+strlen(s)-1;
  while( ss>=s && *ss==' ' )
    --ss;
  *(ss+1)='\0';
}

/*
**  Removes the whitespace at the begining of a string
*/
static void ltrimFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  const char *z;

  assert( argc==1);

  if( SQLITE_NULL==sqlite3_value_type(argv[0]) ){
    sqlite3_result_null(context);
    return;
  }
  z = sqlite3_value_text(argv[0]);
  sqlite3_result_text(context, ltrim(z), -1, SQLITE_TRANSIENT);
}

/*
**  Removes the whitespace at the end of a string
*/
static void rtrimFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  const char *z;
  char *rz;
  /* try not to change data in argv */

  assert( argc==1);

  if( SQLITE_NULL==sqlite3_value_type(argv[0]) ){
    sqlite3_result_null(context);
    return;
  }
  z = sqlite3_value_text(argv[0]);
  rz = sqlite3StrDup(z);
  rtrim(rz);
  sqlite3_result_text(context, rz, -1, SQLITE_TRANSIENT);
  sqlite3_free(rz);
}

/*
**  Removes the whitespace at the begining and end of a string
*/
static void trimFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  const char *z;
  char *rz;
  /* try not to change data in argv */

  assert( argc==1);

  if( SQLITE_NULL==sqlite3_value_type(argv[0]) ){
    sqlite3_result_null(context);
    return;
  }
  z = sqlite3_value_text(argv[0]);
  rz = sqlite3StrDup(z);
  rtrim(rz);
  sqlite3_result_text(context, ltrim(rz), -1, SQLITE_TRANSIENT);
  sqlite3_free(rz);
}
#endif

/*
** given a pointer to a string s1, the length of that string (l1), a new string (s2)
** and it's length (l2) appends s2 to s1.
** All lengths in bytes.
** This is just an auxiliary function
*/
// static void _append(char **s1, int l1, const char *s2, int l2){
//   *s1 = realloc(*s1, (l1+l2+1)*sizeof(char));
//   strncpy((*s1)+l1, s2, l2);
//   *(*(s1)+l1+l2) = '\0';
// }

#ifndef HAVE_TRIM

/*
** given strings s, s1 and s2 replaces occurrences of s1 in s by s2
*/
static void replaceFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  const char *z1;     /* string s (first parameter) */
  const char *z2;     /* string s1 (second parameter) string to look for */
  const char *z3;     /* string s2 (third parameter) string to replace occurrences of s1 with */
  int lz1;
  int lz2;
  int lz3;
  int lzo=0;
  char *zo=0;
  int ret=0;
  const char *zt1;
  const char *zt2;

  assert( 3==argc );

  if( SQLITE_NULL==sqlite3_value_type(argv[0]) ){
    sqlite3_result_null(context);
    return;
  }

  z1 = sqlite3_value_text(argv[0]);
  z2 = sqlite3_value_text(argv[1]);
  z3 = sqlite3_value_text(argv[2]);
  /* handle possible null values */
  if( 0==z2 ){
    z2="";
  }
  if( 0==z3 ){
    z3="";
  }

  lz1 = strlen(z1);
  lz2 = strlen(z2);
  lz3 = strlen(z3);

#if 0
  /* special case when z2 is empty (or null) nothing will be changed */
  if( 0==lz2 ){
    sqlite3_result_text(context, z1, -1, SQLITE_TRANSIENT);
    return;
  }
#endif

  zt1=z1;
  zt2=z1;

  while(1){
    ret=_substr(z2,zt1 , 0, &zt2);

    if( ret<0 )
      break;

    _append(&zo, lzo, zt1, zt2-zt1);
    lzo+=zt2-zt1;
    _append(&zo, lzo, z3, lz3);
    lzo+=lz3;

    zt1=zt2+lz2;
  }
  _append(&zo, lzo, zt1, lz1-(zt1-z1));
  sqlite3_result_text(context, zo, -1, SQLITE_TRANSIENT);
  sqlite3_free(zo);
}
#endif

/*
** given a string returns the same string but with the characters in reverse order
*/
static void reverseFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  const char *z;
  const char *zt;
  char *rz;
  char *rzt;
  int l = 0;
  int i = 0;

  assert( 1==argc );

  if( SQLITE_NULL==sqlite3_value_type(argv[0]) ){
    sqlite3_result_null(context);
    return;
  }
  z = (char *)sqlite3_value_text(argv[0]);
  l = strlen(z);
  rz = sqlite3_malloc(l+1);
  if (!rz){
    sqlite3_result_error_nomem(context);
    return;
  }
  rzt = rz+l;
  *(rzt--) = '\0';

  zt=z;
  while( sqliteCharVal((unsigned char *)zt)!=0 ){
    z=zt;
    sqliteNextChar(zt);
    for(i=1; zt-i>=z; ++i){
      *(rzt--)=*(zt-i);
    }
  }

  sqlite3_result_text(context, rz, -1, SQLITE_TRANSIENT);
  sqlite3_free(rz);
}

/*
** An instance of the following structure holds the context of a
** stdev() or variance() aggregate computation.
** implementaion of http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Algorithm_II
** less prone to rounding errors
*/
typedef struct StdevCtx StdevCtx;
struct StdevCtx {
  double rM;
  double rS;
  i64 cnt;          /* number of elements */
};

/*
** An instance of the following structure holds the context of a
** mode() or median() aggregate computation.
** Depends on structures defined in map.c (see map & map)
** These aggregate functions only work for integers and floats although
** they could be made to work for strings. This is usually considered meaningless.
** Only usuall order (for median), no use of collation functions (would this even make sense?)
*/
typedef struct ModeCtx ModeCtx;
struct ModeCtx {
  i64 riM;            /* integer value found so far */
  double rdM;         /* double value found so far */
  i64 cnt;            /* number of elements so far */
  double pcnt;        /* number of elements smaller than a percentile */
  i64 mcnt;           /* maximum number of occurrences (for mode) */
  i64 mn;             /* number of occurrences (for mode and percentiles) */
  i64 is_double;      /* whether the computation is being done for doubles (>0) or integers (=0) */
  map* m;             /* map structure used for the computation */
  int done;           /* whether the answer has been found */
};

/*
** called for each value received during a calculation of stdev or variance
*/
static void varianceStep(sqlite3_context *context, int argc, sqlite3_value **argv){
  StdevCtx *p;

  double delta;
  double x;

  assert( argc==1 );
  p = sqlite3_aggregate_context(context, sizeof(*p));
  /* only consider non-null values */
  if( SQLITE_NULL != sqlite3_value_numeric_type(argv[0]) ){
    p->cnt++;
    x = sqlite3_value_double(argv[0]);
    delta = (x-p->rM);
    p->rM += delta/p->cnt;
    p->rS += delta*(x-p->rM);
  }
}

/*
** called for each value received during a calculation of mode of median
*/
static void modeStep(sqlite3_context *context, int argc, sqlite3_value **argv){
  ModeCtx *p;
  i64 xi=0;
  double xd=0.0;
  i64 *iptr;
  double *dptr;
  int type;

  assert( argc==1 );
  type = sqlite3_value_numeric_type(argv[0]);

  if( type == SQLITE_NULL)
    return;

  p = sqlite3_aggregate_context(context, sizeof(*p));

  if( 0==(p->m) ){
    p->m = calloc(1, sizeof(map));
    if( type==SQLITE_INTEGER ){
      /* map will be used for integers */
      *(p->m) = map_make(int_cmp);
      p->is_double = 0;
    }else{
      p->is_double = 1;
      /* map will be used for doubles */
      *(p->m) = map_make(double_cmp);
    }
  }

  ++(p->cnt);

  if( 0==p->is_double ){
    xi = sqlite3_value_int64(argv[0]);
    iptr = (i64*)calloc(1,sizeof(i64));
    *iptr = xi;
    map_insert(p->m, iptr);
  }else{
    xd = sqlite3_value_double(argv[0]);
    dptr = (double*)calloc(1,sizeof(double));
    *dptr = xd;
    map_insert(p->m, dptr);
  }
}

/*
**  Auxiliary function that iterates all elements in a map and finds the mode
**  (most frequent value)
*/
static void modeIterate(void* e, i64 c, void* pp){
  i64 ei;
  double ed;
  ModeCtx *p = (ModeCtx*)pp;

  if( 0==p->is_double ){
    ei = *(int*)(e);

	if( p->mcnt==c ){
      ++p->mn;
    }else if( p->mcnt<c ){
      p->riM = ei;
      p->mcnt = c;
	  p->mn=1;
    }
  }else{
    ed = *(double*)(e);

	if( p->mcnt==c ){
      ++p->mn;
    }else if(p->mcnt<c){
      p->rdM = ed;
      p->mcnt = c;
	  p->mn=1;
    }
  }
}

/*
**  Auxiliary function that iterates all elements in a map and finds the median
**  (the value such that the number of elements smaller is equal the the number of
**  elements larger)
*/
static void medianIterate(void* e, i64 c, void* pp){
  i64 ei;
  double ed;
  double iL;
  double iR;
  int il;
  int ir;
  ModeCtx *p = (ModeCtx*)pp;

  if(p->done>0)
    return;

  iL = p->pcnt;
  iR = p->cnt - p->pcnt;
  il = p->mcnt + c;
  ir = p->cnt - p->mcnt;

  if( il >= iL ){
    if( ir >= iR ){
    ++p->mn;
      if( 0==p->is_double ){
        ei = *(int*)(e);
        p->riM += ei;
      }else{
        ed = *(double*)(e);
        p->rdM += ed;
      }
    }else{
      p->done=1;
    }
  }
  p->mcnt+=c;
}

/*
** Returns the mode value
*/
static void modeFinalize(sqlite3_context *context){
  ModeCtx *p;
  p = sqlite3_aggregate_context(context, 0);
  if( p && p->m ){
    map_iterate(p->m, modeIterate, p);
    map_destroy(p->m);
    free(p->m);

    if( 1==p->mn ){
      if( 0==p->is_double )
        sqlite3_result_int64(context, p->riM);
      else
        sqlite3_result_double(context, p->rdM);
    }
  }
}

/*
** auxiliary function for percentiles
*/
static void _medianFinalize(sqlite3_context *context){
  ModeCtx *p;
  p = (ModeCtx*) sqlite3_aggregate_context(context, 0);
  if( p && p->m ){
    p->done=0;
    map_iterate(p->m, medianIterate, p);
    map_destroy(p->m);
    free(p->m);

    if( 0==p->is_double )
      if( 1==p->mn )
      	sqlite3_result_int64(context, p->riM);
      else
      	sqlite3_result_double(context, p->riM*1.0/p->mn);
    else
      sqlite3_result_double(context, p->rdM/p->mn);
  }
}

/*
** Returns the median value
*/
static void medianFinalize(sqlite3_context *context){
  ModeCtx *p;
  p = (ModeCtx*) sqlite3_aggregate_context(context, 0);
  if( p!=0 ){
    p->pcnt = (p->cnt)/2.0;
    _medianFinalize(context);
  }
}

/*
** Returns the lower_quartile value
*/
static void lower_quartileFinalize(sqlite3_context *context){
  ModeCtx *p;
  p = (ModeCtx*) sqlite3_aggregate_context(context, 0);
  if( p!=0 ){
    p->pcnt = (p->cnt)/4.0;
    _medianFinalize(context);
  }
}

/*
** Returns the upper_quartile value
*/
static void upper_quartileFinalize(sqlite3_context *context){
  ModeCtx *p;
  p = (ModeCtx*) sqlite3_aggregate_context(context, 0);
  if( p!=0 ){
    p->pcnt = (p->cnt)*3/4.0;
    _medianFinalize(context);
  }
}

/*
** Returns the stdev value
*/
static void stdevFinalize(sqlite3_context *context){
  StdevCtx *p;
  p = sqlite3_aggregate_context(context, 0);
  if( p && p->cnt>1 ){
    sqlite3_result_double(context, sqrt(p->rS/(p->cnt-1)));
  }else{
    sqlite3_result_null(context);
  }
}

/*
** Returns the variance value
*/
static void varianceFinalize(sqlite3_context *context){
  StdevCtx *p;
  p = sqlite3_aggregate_context(context, 0);
  if( p && p->cnt>1 ){
    sqlite3_result_double(context, p->rS/(p->cnt-1));
  }else{
    sqlite3_result_null(context);
  }
}

#ifdef SQLITE_SOUNDEX

/* relicoder factored code */
/*
** Calculates the soundex value of a string
*/

static void soundex(const u8 *zIn, char *zResult){
  int i, j;
  static const unsigned char iCode[] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 2, 3, 0, 1, 2, 0, 0, 2, 2, 4, 5, 5, 0,
    1, 2, 6, 2, 3, 0, 1, 0, 2, 0, 2, 0, 0, 0, 0, 0,
    0, 0, 1, 2, 3, 0, 1, 2, 0, 0, 2, 2, 4, 5, 5, 0,
    1, 2, 6, 2, 3, 0, 1, 0, 2, 0, 2, 0, 0, 0, 0, 0,
  };

  for(i=0; zIn[i] && !isalpha(zIn[i]); i++){}
  if( zIn[i] ){
    zResult[0] = toupper(zIn[i]);
    for(j=1; j<4 && zIn[i]; i++){
      int code = iCode[zIn[i]&0x7f];
      if( code>0 ){
        zResult[j++] = code + '0';
      }
    }
    while( j<4 ){
      zResult[j++] = '0';
    }
    zResult[j] = 0;
  }else{
    strcpy(zResult, "?000");
  }
}

/*
** computes the number of different characters between the soundex value fo 2 strings
*/
static void differenceFunc(sqlite3_context *context, int argc, sqlite3_value **argv){
  char zResult1[8];
  char zResult2[8];
  char *zR1 = zResult1;
  char *zR2 = zResult2;
  int rVal = 0;
  int i = 0;
  const u8 *zIn1;
  const u8 *zIn2;

  assert( argc==2 );

  if( sqlite3_value_type(argv[0])==SQLITE_NULL || sqlite3_value_type(argv[1])==SQLITE_NULL ){
    sqlite3_result_null(context);
    return;
  }

  zIn1 = (u8*)sqlite3_value_text(argv[0]);
  zIn2 = (u8*)sqlite3_value_text(argv[1]);

  soundex(zIn1, zR1);
  soundex(zIn2, zR2);

  for(i=0; i<4; ++i){
    if( sqliteCharVal((unsigned char *)zR1)==sqliteCharVal((unsigned char *)zR2) )
      ++rVal;
    sqliteNextChar(zR1);
    sqliteNextChar(zR2);
  }
  sqlite3_result_int(context, rVal);
}
#endif

#ifndef SQLITE_AMALGAMATION
# if !defined(NDEBUG) && !defined(SQLITE_DEBUG)
#  define NDEBUG 1
# endif
# if defined(NDEBUG) && defined(SQLITE_DEBUG)
#  undef NDEBUG
# endif
# include <string.h>
# include <stdio.h>
# include <stdlib.h>
# include <assert.h>
# define ALWAYS(X)  1
# define NEVER(X)   0
  typedef unsigned char u8;
  typedef unsigned short u16;
#endif
#include <ctype.h>

#ifndef SQLITE_OMIT_VIRTUALTABLE

/*
** Character classes for ASCII characters:
**
**   0   ''        Silent letters:   H W
**   1   'A'       Any vowel:   A E I O U (Y)
**   2   'B'       A bilabeal stop or fricative:  B F P V W
**   3   'C'       Other fricatives or back stops:  C G J K Q S X Z
**   4   'D'       Alveolar stops:  D T
**   5   'H'       Letter H at the beginning of a word
**   6   'L'       Glide:  L
**   7   'R'       Semivowel:  R
**   8   'M'       Nasals:  M N
**   9   'Y'       Letter Y at the beginning of a word.
**   10  '9'       Digits: 0 1 2 3 4 5 6 7 8 9
**   11  ' '       White space
**   12  '?'       Other.
*/
#define CCLASS_SILENT         0
#define CCLASS_VOWEL          1
#define CCLASS_B              2
#define CCLASS_C              3
#define CCLASS_D              4
#define CCLASS_H              5
#define CCLASS_L              6
#define CCLASS_R              7
#define CCLASS_M              8
#define CCLASS_Y              9
#define CCLASS_DIGIT         10
#define CCLASS_SPACE         11
#define CCLASS_OTHER         12

/*
** The following table gives the character class for non-initial ASCII
** characters.
*/
static const unsigned char midClass[] = {
 /*   */ CCLASS_OTHER,    /*   */ CCLASS_OTHER,   /*   */ CCLASS_OTHER,
 /*   */ CCLASS_OTHER,    /*   */ CCLASS_OTHER,   /*   */ CCLASS_OTHER,
 /*   */ CCLASS_OTHER,    /*   */ CCLASS_OTHER,   /*   */ CCLASS_OTHER,
 /*   */ CCLASS_SPACE,    /*   */ CCLASS_OTHER,   /*   */ CCLASS_OTHER,
 /*   */ CCLASS_SPACE,    /*   */ CCLASS_SPACE,   /*   */ CCLASS_OTHER,
 /*   */ CCLASS_OTHER,    /*   */ CCLASS_OTHER,   /*   */ CCLASS_OTHER,
 /*   */ CCLASS_OTHER,    /*   */ CCLASS_OTHER,   /*   */ CCLASS_OTHER,
 /*   */ CCLASS_OTHER,    /*   */ CCLASS_OTHER,   /*   */ CCLASS_OTHER,
 /*   */ CCLASS_OTHER,    /*   */ CCLASS_OTHER,   /*   */ CCLASS_OTHER,
 /*   */ CCLASS_OTHER,    /*   */ CCLASS_OTHER,   /*   */ CCLASS_OTHER,
 /*   */ CCLASS_OTHER,    /*   */ CCLASS_OTHER,   /*   */ CCLASS_SPACE,
 /* ! */ CCLASS_OTHER,    /* " */ CCLASS_OTHER,   /* # */ CCLASS_OTHER,
 /* $ */ CCLASS_OTHER,    /* % */ CCLASS_OTHER,   /* & */ CCLASS_OTHER,
 /* ' */ CCLASS_SILENT,   /* ( */ CCLASS_OTHER,   /* ) */ CCLASS_OTHER,
 /* * */ CCLASS_OTHER,    /* + */ CCLASS_OTHER,   /* , */ CCLASS_OTHER,
 /* - */ CCLASS_OTHER,    /* . */ CCLASS_OTHER,   /* / */ CCLASS_OTHER,
 /* 0 */ CCLASS_DIGIT,    /* 1 */ CCLASS_DIGIT,   /* 2 */ CCLASS_DIGIT,
 /* 3 */ CCLASS_DIGIT,    /* 4 */ CCLASS_DIGIT,   /* 5 */ CCLASS_DIGIT,
 /* 6 */ CCLASS_DIGIT,    /* 7 */ CCLASS_DIGIT,   /* 8 */ CCLASS_DIGIT,
 /* 9 */ CCLASS_DIGIT,    /* : */ CCLASS_OTHER,   /* ; */ CCLASS_OTHER,
 /* < */ CCLASS_OTHER,    /* = */ CCLASS_OTHER,   /* > */ CCLASS_OTHER,
 /* ? */ CCLASS_OTHER,    /* @ */ CCLASS_OTHER,   /* A */ CCLASS_VOWEL,
 /* B */ CCLASS_B,        /* C */ CCLASS_C,       /* D */ CCLASS_D,
 /* E */ CCLASS_VOWEL,    /* F */ CCLASS_B,       /* G */ CCLASS_C,
 /* H */ CCLASS_SILENT,   /* I */ CCLASS_VOWEL,   /* J */ CCLASS_C,
 /* K */ CCLASS_C,        /* L */ CCLASS_L,       /* M */ CCLASS_M,
 /* N */ CCLASS_M,        /* O */ CCLASS_VOWEL,   /* P */ CCLASS_B,
 /* Q */ CCLASS_C,        /* R */ CCLASS_R,       /* S */ CCLASS_C,
 /* T */ CCLASS_D,        /* U */ CCLASS_VOWEL,   /* V */ CCLASS_B,
 /* W */ CCLASS_B,        /* X */ CCLASS_C,       /* Y */ CCLASS_VOWEL,
 /* Z */ CCLASS_C,        /* [ */ CCLASS_OTHER,   /* \ */ CCLASS_OTHER,
 /* ] */ CCLASS_OTHER,    /* ^ */ CCLASS_OTHER,   /* _ */ CCLASS_OTHER,
 /* ` */ CCLASS_OTHER,    /* a */ CCLASS_VOWEL,   /* b */ CCLASS_B,
 /* c */ CCLASS_C,        /* d */ CCLASS_D,       /* e */ CCLASS_VOWEL,
 /* f */ CCLASS_B,        /* g */ CCLASS_C,       /* h */ CCLASS_SILENT,
 /* i */ CCLASS_VOWEL,    /* j */ CCLASS_C,       /* k */ CCLASS_C,
 /* l */ CCLASS_L,        /* m */ CCLASS_M,       /* n */ CCLASS_M,
 /* o */ CCLASS_VOWEL,    /* p */ CCLASS_B,       /* q */ CCLASS_C,
 /* r */ CCLASS_R,        /* s */ CCLASS_C,       /* t */ CCLASS_D,
 /* u */ CCLASS_VOWEL,    /* v */ CCLASS_B,       /* w */ CCLASS_B,
 /* x */ CCLASS_C,        /* y */ CCLASS_VOWEL,   /* z */ CCLASS_C,
 /* { */ CCLASS_OTHER,    /* | */ CCLASS_OTHER,   /* } */ CCLASS_OTHER,
 /* ~ */ CCLASS_OTHER,    /*   */ CCLASS_OTHER,   
};
/* 
** This tables gives the character class for ASCII characters that form the
** initial character of a word.  The only difference from midClass is with
** the letters H, W, and Y.
*/
static const unsigned char initClass[] = {
 /*   */ CCLASS_OTHER,    /*   */ CCLASS_OTHER,   /*   */ CCLASS_OTHER,
 /*   */ CCLASS_OTHER,    /*   */ CCLASS_OTHER,   /*   */ CCLASS_OTHER,
 /*   */ CCLASS_OTHER,    /*   */ CCLASS_OTHER,   /*   */ CCLASS_OTHER,
 /*   */ CCLASS_SPACE,    /*   */ CCLASS_OTHER,   /*   */ CCLASS_OTHER,
 /*   */ CCLASS_SPACE,    /*   */ CCLASS_SPACE,   /*   */ CCLASS_OTHER,
 /*   */ CCLASS_OTHER,    /*   */ CCLASS_OTHER,   /*   */ CCLASS_OTHER,
 /*   */ CCLASS_OTHER,    /*   */ CCLASS_OTHER,   /*   */ CCLASS_OTHER,
 /*   */ CCLASS_OTHER,    /*   */ CCLASS_OTHER,   /*   */ CCLASS_OTHER,
 /*   */ CCLASS_OTHER,    /*   */ CCLASS_OTHER,   /*   */ CCLASS_OTHER,
 /*   */ CCLASS_OTHER,    /*   */ CCLASS_OTHER,   /*   */ CCLASS_OTHER,
 /*   */ CCLASS_OTHER,    /*   */ CCLASS_OTHER,   /*   */ CCLASS_SPACE,
 /* ! */ CCLASS_OTHER,    /* " */ CCLASS_OTHER,   /* # */ CCLASS_OTHER,
 /* $ */ CCLASS_OTHER,    /* % */ CCLASS_OTHER,   /* & */ CCLASS_OTHER,
 /* ' */ CCLASS_OTHER,    /* ( */ CCLASS_OTHER,   /* ) */ CCLASS_OTHER,
 /* * */ CCLASS_OTHER,    /* + */ CCLASS_OTHER,   /* , */ CCLASS_OTHER,
 /* - */ CCLASS_OTHER,    /* . */ CCLASS_OTHER,   /* / */ CCLASS_OTHER,
 /* 0 */ CCLASS_DIGIT,    /* 1 */ CCLASS_DIGIT,   /* 2 */ CCLASS_DIGIT,
 /* 3 */ CCLASS_DIGIT,    /* 4 */ CCLASS_DIGIT,   /* 5 */ CCLASS_DIGIT,
 /* 6 */ CCLASS_DIGIT,    /* 7 */ CCLASS_DIGIT,   /* 8 */ CCLASS_DIGIT,
 /* 9 */ CCLASS_DIGIT,    /* : */ CCLASS_OTHER,   /* ; */ CCLASS_OTHER,
 /* < */ CCLASS_OTHER,    /* = */ CCLASS_OTHER,   /* > */ CCLASS_OTHER,
 /* ? */ CCLASS_OTHER,    /* @ */ CCLASS_OTHER,   /* A */ CCLASS_VOWEL,
 /* B */ CCLASS_B,        /* C */ CCLASS_C,       /* D */ CCLASS_D,
 /* E */ CCLASS_VOWEL,    /* F */ CCLASS_B,       /* G */ CCLASS_C,
 /* H */ CCLASS_SILENT,   /* I */ CCLASS_VOWEL,   /* J */ CCLASS_C,
 /* K */ CCLASS_C,        /* L */ CCLASS_L,       /* M */ CCLASS_M,
 /* N */ CCLASS_M,        /* O */ CCLASS_VOWEL,   /* P */ CCLASS_B,
 /* Q */ CCLASS_C,        /* R */ CCLASS_R,       /* S */ CCLASS_C,
 /* T */ CCLASS_D,        /* U */ CCLASS_VOWEL,   /* V */ CCLASS_B,
 /* W */ CCLASS_B,        /* X */ CCLASS_C,       /* Y */ CCLASS_Y,
 /* Z */ CCLASS_C,        /* [ */ CCLASS_OTHER,   /* \ */ CCLASS_OTHER,
 /* ] */ CCLASS_OTHER,    /* ^ */ CCLASS_OTHER,   /* _ */ CCLASS_OTHER,
 /* ` */ CCLASS_OTHER,    /* a */ CCLASS_VOWEL,   /* b */ CCLASS_B,
 /* c */ CCLASS_C,        /* d */ CCLASS_D,       /* e */ CCLASS_VOWEL,
 /* f */ CCLASS_B,        /* g */ CCLASS_C,       /* h */ CCLASS_SILENT,
 /* i */ CCLASS_VOWEL,    /* j */ CCLASS_C,       /* k */ CCLASS_C,
 /* l */ CCLASS_L,        /* m */ CCLASS_M,       /* n */ CCLASS_M,
 /* o */ CCLASS_VOWEL,    /* p */ CCLASS_B,       /* q */ CCLASS_C,
 /* r */ CCLASS_R,        /* s */ CCLASS_C,       /* t */ CCLASS_D,
 /* u */ CCLASS_VOWEL,    /* v */ CCLASS_B,       /* w */ CCLASS_B,
 /* x */ CCLASS_C,        /* y */ CCLASS_Y,       /* z */ CCLASS_C,
 /* { */ CCLASS_OTHER,    /* | */ CCLASS_OTHER,   /* } */ CCLASS_OTHER,
 /* ~ */ CCLASS_OTHER,    /*   */ CCLASS_OTHER,   
};

/*
** Mapping from the character class number (0-13) to a symbol for each
** character class.  Note that initClass[] can be used to map the class
** symbol back into the class number.
*/
static const unsigned char className[] = ".ABCDHLRMY9 ?";

/*
** Generate a "phonetic hash" from a string of ASCII characters
** in zIn[0..nIn-1].
**
**   * Map characters by character class as defined above.
**   * Omit double-letters
**   * Omit vowels beside R and L
**   * Omit T when followed by CH
**   * Omit W when followed by R
**   * Omit D when followed by J or G
**   * Omit K in KN or G in GN at the beginning of a word
**
** Space to hold the result is obtained from sqlite3_malloc()
**
** Return NULL if memory allocation fails.  
*/
static unsigned char *phoneticHash(const unsigned char *zIn, int nIn){
  unsigned char *zOut = sqlite3_malloc64( nIn + 1 );
  int i;
  int nOut = 0;
  char cPrev = 0x77;
  char cPrevX = 0x77;
  const unsigned char *aClass = initClass;

  if( zOut==0 ) return 0;
  if( nIn>2 ){
    switch( zIn[0] ){
      case 'g': 
      case 'k': {
        if( zIn[1]=='n' ){ zIn++; nIn--; }
        break;
      }
    }
  }
  for(i=0; i<nIn; i++){
    unsigned char c = zIn[i];
    if( i+1<nIn ){
      if( c=='w' && zIn[i+1]=='r' ) continue;
      if( c=='d' && (zIn[i+1]=='j' || zIn[i+1]=='g') ) continue;
      if( i+2<nIn ){
        if( c=='t' && zIn[i+1]=='c' && zIn[i+2]=='h' ) continue;
      }
    }
    c = aClass[c&0x7f];
    if( c==CCLASS_SPACE ) continue;
    if( c==CCLASS_OTHER && cPrev!=CCLASS_DIGIT ) continue;
    aClass = midClass;
    if( c==CCLASS_VOWEL && (cPrevX==CCLASS_R || cPrevX==CCLASS_L) ){
       continue; /* No vowels beside L or R */ 
    }
    if( (c==CCLASS_R || c==CCLASS_L) && cPrevX==CCLASS_VOWEL ){
       nOut--;   /* No vowels beside L or R */
    }
    cPrev = c;
    if( c==CCLASS_SILENT ) continue;
    cPrevX = c;
    c = className[c];
    assert( nOut>=0 );
    if( nOut==0 || c!=zOut[nOut-1] ) zOut[nOut++] = c;
  }
  zOut[nOut] = 0;
  return zOut;
}

/*
** This is an SQL function wrapper around phoneticHash().  See
** the description of phoneticHash() for additional information.
*/
static void phoneticHashSqlFunc(
  sqlite3_context *context,
  int argc,
  sqlite3_value **argv
){
  const unsigned char *zIn;
  unsigned char *zOut;

  zIn = sqlite3_value_text(argv[0]);
  if( zIn==0 ) return;
  zOut = phoneticHash(zIn, sqlite3_value_bytes(argv[0]));
  if( zOut==0 ){
    sqlite3_result_error_nomem(context);
  }else{
    sqlite3_result_text(context, (char*)zOut, -1, sqlite3_free);
  }
}

/*
** Return the character class number for a character given its
** context.
*/
static char characterClass(char cPrev, char c){
  return cPrev==0 ? initClass[c&0x7f] : midClass[c&0x7f];
}

/*
** Return the cost of inserting or deleting character c immediately
** following character cPrev.  If cPrev==0, that means c is the first
** character of the word.
*/
static int insertOrDeleteCost(char cPrev, char c, char cNext){
  char classC = characterClass(cPrev, c);
  char classCprev;

  if( classC==CCLASS_SILENT ){
    /* Insert or delete "silent" characters such as H or W */
    return 1;
  }
  if( cPrev==c ){
    /* Repeated characters, or miss a repeat */
    return 10;
  }
  if( classC==CCLASS_VOWEL && (cPrev=='r' || cNext=='r') ){
    return 20;  /* Insert a vowel before or after 'r' */
  }
  classCprev = characterClass(cPrev, cPrev);
  if( classC==classCprev ){
    if( classC==CCLASS_VOWEL ){
      /* Remove or add a new vowel to a vowel cluster */
      return 15;
    }else{
      /* Remove or add a consonant not in the same class */
      return 50;
    }
  }

  /* any other character insertion or deletion */
  return 100;
}

/*
** Divide the insertion cost by this factor when appending to the
** end of the word.
*/
#define FINAL_INS_COST_DIV  4

/*
** Return the cost of substituting cTo in place of cFrom assuming
** the previous character is cPrev.  If cPrev==0 then cTo is the first
** character of the word.
*/
static int substituteCost(char cPrev, char cFrom, char cTo){
  char classFrom, classTo;
  if( cFrom==cTo ){
    /* Exact match */
    return 0;
  }
  if( cFrom==(cTo^0x20) && ((cTo>='A' && cTo<='Z') || (cTo>='a' && cTo<='z')) ){
    /* differ only in case */
    return 0;
  }
  classFrom = characterClass(cPrev, cFrom);
  classTo = characterClass(cPrev, cTo);
  if( classFrom==classTo ){
    /* Same character class */
    return 40;
  }
  if( classFrom>=CCLASS_B && classFrom<=CCLASS_Y
      && classTo>=CCLASS_B && classTo<=CCLASS_Y ){
    /* Convert from one consonant to another, but in a different class */
    return 75;
  }
  /* Any other subsitution */
  return 100;
}

/*
** Given two strings zA and zB which are pure ASCII, return the cost
** of transforming zA into zB.  If zA ends with '*' assume that it is
** a prefix of zB and give only minimal penalty for extra characters
** on the end of zB.
**
** Smaller numbers mean a closer match.
**
** Negative values indicate an error:
**    -1  One of the inputs is NULL
**    -2  Non-ASCII characters on input
**    -3  Unable to allocate memory 
**
** If pnMatch is not NULL, then *pnMatch is set to the number of bytes
** of zB that matched the pattern in zA. If zA does not end with a '*',
** then this value is always the number of bytes in zB (i.e. strlen(zB)).
** If zA does end in a '*', then it is the number of bytes in the prefix
** of zB that was deemed to match zA.
*/
static int editdist1(const char *zA, const char *zB, int *pnMatch){
  int nA, nB;            /* Number of characters in zA[] and zB[] */
  int xA, xB;            /* Loop counters for zA[] and zB[] */
  char cA = 0, cB;       /* Current character of zA and zB */
  char cAprev, cBprev;   /* Previous character of zA and zB */
  char cAnext, cBnext;   /* Next character in zA and zB */
  int d;                 /* North-west cost value */
  int dc = 0;            /* North-west character value */
  int res;               /* Final result */
  int *m;                /* The cost matrix */
  char *cx;              /* Corresponding character values */
  int *toFree = 0;       /* Malloced space */
  int nMatch = 0;
  int mStack[60+15];     /* Stack space to use if not too much is needed */

  /* Early out if either input is NULL */
  if( zA==0 || zB==0 ) return -1;

  /* Skip any common prefix */
  while( zA[0] && zA[0]==zB[0] ){ dc = zA[0]; zA++; zB++; nMatch++; }
  if( pnMatch ) *pnMatch = nMatch;
  if( zA[0]==0 && zB[0]==0 ) return 0;

#if 0
  printf("A=\"%s\" B=\"%s\" dc=%c\n", zA, zB, dc?dc:' ');
#endif

  /* Verify input strings and measure their lengths */
  for(nA=0; zA[nA]; nA++){
    if( zA[nA]&0x80 ) return -2;
  }
  for(nB=0; zB[nB]; nB++){
    if( zB[nB]&0x80 ) return -2;
  }

  /* Special processing if either string is empty */
  if( nA==0 ){
    cBprev = (char)dc;
    for(xB=res=0; (cB = zB[xB])!=0; xB++){
      res += insertOrDeleteCost(cBprev, cB, zB[xB+1])/FINAL_INS_COST_DIV;
      cBprev = cB;
    }
    return res;
  }
  if( nB==0 ){
    cAprev = (char)dc;
    for(xA=res=0; (cA = zA[xA])!=0; xA++){
      res += insertOrDeleteCost(cAprev, cA, zA[xA+1]);
      cAprev = cA;
    }
    return res;
  }

  /* A is a prefix of B */
  if( zA[0]=='*' && zA[1]==0 ) return 0;

  /* Allocate and initialize the Wagner matrix */
  if( nB<(sizeof(mStack)*4)/(sizeof(mStack[0])*5) ){
    m = mStack;
  }else{
    m = toFree = sqlite3_malloc64( (nB+1)*5*sizeof(m[0])/4 );
    if( m==0 ) return -3;
  }
  cx = (char*)&m[nB+1];

  /* Compute the Wagner edit distance */
  m[0] = 0;
  cx[0] = (char)dc;
  cBprev = (char)dc;
  for(xB=1; xB<=nB; xB++){
    cBnext = zB[xB];
    cB = zB[xB-1];
    cx[xB] = cB;
    m[xB] = m[xB-1] + insertOrDeleteCost(cBprev, cB, cBnext);
    cBprev = cB;
  }
  cAprev = (char)dc;
  for(xA=1; xA<=nA; xA++){
    int lastA = (xA==nA);
    cA = zA[xA-1];
    cAnext = zA[xA];
    if( cA=='*' && lastA ) break;
    d = m[0];
    dc = cx[0];
    m[0] = d + insertOrDeleteCost(cAprev, cA, cAnext);
    cBprev = 0;
    for(xB=1; xB<=nB; xB++){
      int totalCost, insCost, delCost, subCost, ncx;
      cB = zB[xB-1];
      cBnext = zB[xB];

      /* Cost to insert cB */
      insCost = insertOrDeleteCost(cx[xB-1], cB, cBnext);
      if( lastA ) insCost /= FINAL_INS_COST_DIV;

      /* Cost to delete cA */
      delCost = insertOrDeleteCost(cx[xB], cA, cBnext);

      /* Cost to substitute cA->cB */
      subCost = substituteCost(cx[xB-1], cA, cB);

      /* Best cost */
      totalCost = insCost + m[xB-1];
      ncx = cB;
      if( (delCost + m[xB])<totalCost ){
        totalCost = delCost + m[xB];
        ncx = cA;
      }
      if( (subCost + d)<totalCost ){
        totalCost = subCost + d;
      }

#if 0
      printf("%d,%d d=%4d u=%4d r=%4d dc=%c cA=%c cB=%c"
             " ins=%4d del=%4d sub=%4d t=%4d ncx=%c\n",
             xA, xB, d, m[xB], m[xB-1], dc?dc:' ', cA, cB,
             insCost, delCost, subCost, totalCost, ncx?ncx:' ');
#endif

      /* Update the matrix */
      d = m[xB];
      dc = cx[xB];
      m[xB] = totalCost;
      cx[xB] = (char)ncx;
      cBprev = cB;
    }
    cAprev = cA;
  }

  /* Free the wagner matrix and return the result */
  if( cA=='*' ){
    res = m[1];
    for(xB=1; xB<=nB; xB++){
      if( m[xB]<res ){
        res = m[xB];
        if( pnMatch ) *pnMatch = xB+nMatch;
      }
    }
  }else{
    res = m[nB];
    /* In the current implementation, pnMatch is always NULL if zA does
    ** not end in "*" */
    assert( pnMatch==0 );
  }
  sqlite3_free(toFree);
  return res;
}

/*
** Function:    editdist(A,B)
**
** Return the cost of transforming string A into string B.  Both strings
** must be pure ASCII text.  If A ends with '*' then it is assumed to be
** a prefix of B and extra characters on the end of B have minimal additional
** cost.
*/
static void editdistSqlFunc(
  sqlite3_context *context,
  int argc,
  sqlite3_value **argv
){
  int res = editdist1(
                    (const char*)sqlite3_value_text(argv[0]),
                    (const char*)sqlite3_value_text(argv[1]),
                    0);
  if( res<0 ){
    if( res==(-3) ){
      sqlite3_result_error_nomem(context);
    }else if( res==(-2) ){
      sqlite3_result_error(context, "non-ASCII input to editdist()", -1);
    }else{
      sqlite3_result_error(context, "NULL input to editdist()", -1);
    }
  }else{ 
    sqlite3_result_int(context, res);
  }
}

/* End of the fixed-cost edit distance implementation
******************************************************************************
*****************************************************************************
** Begin: Configurable cost unicode edit distance routines
*/
/* Forward declaration of structures */
typedef struct EditDist3Cost EditDist3Cost;
typedef struct EditDist3Config EditDist3Config;
typedef struct EditDist3Point EditDist3Point;
typedef struct EditDist3From EditDist3From;
typedef struct EditDist3FromString EditDist3FromString;
typedef struct EditDist3To EditDist3To;
typedef struct EditDist3ToString EditDist3ToString;
typedef struct EditDist3Lang EditDist3Lang;


/*
** An entry in the edit cost table
*/
struct EditDist3Cost {
  EditDist3Cost *pNext;     /* Next cost element */
  u8 nFrom;                 /* Number of bytes in aFrom */
  u8 nTo;                   /* Number of bytes in aTo */
  u16 iCost;                /* Cost of this transformation */
  char a[4]    ;            /* FROM string followed by TO string */
  /* Additional TO and FROM string bytes appended as necessary */
};

/*
** Edit costs for a particular language ID 
*/
struct EditDist3Lang {
  int iLang;             /* Language ID */
  int iInsCost;          /* Default insertion cost */
  int iDelCost;          /* Default deletion cost */
  int iSubCost;          /* Default substitution cost */
  EditDist3Cost *pCost;  /* Costs */
};


/*
** The default EditDist3Lang object, with default costs.
*/
static const EditDist3Lang editDist3Lang = { 0, 100, 100, 150, 0 };

/*
** Complete configuration
*/
struct EditDist3Config {
  int nLang;             /* Number of language IDs.  Size of a[] */
  EditDist3Lang *a;      /* One for each distinct language ID */
};

/*
** Extra information about each character in the FROM string.
*/
struct EditDist3From {
  int nSubst;              /* Number of substitution cost entries */
  int nDel;                /* Number of deletion cost entries */
  int nByte;               /* Number of bytes in this character */
  EditDist3Cost **apSubst; /* Array of substitution costs for this element */
  EditDist3Cost **apDel;   /* Array of deletion cost entries */
};

/*
** A precompiled FROM string.
*
** In the common case we expect the FROM string to be reused multiple times.
** In other words, the common case will be to measure the edit distance
** from a single origin string to multiple target strings.
*/
struct EditDist3FromString {
  char *z;                 /* The complete text of the FROM string */
  int n;                   /* Number of characters in the FROM string */
  int isPrefix;            /* True if ends with '*' character */
  EditDist3From *a;        /* Extra info about each char of the FROM string */
};

/*
** Extra information about each character in the TO string.
*/
struct EditDist3To {
  int nIns;                /* Number of insertion cost entries */
  int nByte;               /* Number of bytes in this character */
  EditDist3Cost **apIns;   /* Array of deletion cost entries */
};

/*
** A precompiled FROM string
*/
struct EditDist3ToString {
  char *z;                 /* The complete text of the TO string */
  int n;                   /* Number of characters in the TO string */
  EditDist3To *a;          /* Extra info about each char of the TO string */
};

/*
** Clear or delete an instance of the object that records all edit-distance
** weights.
*/
static void editDist3ConfigClear(EditDist3Config *p){
  int i;
  if( p==0 ) return;
  for(i=0; i<p->nLang; i++){
    EditDist3Cost *pCost, *pNext;
    pCost = p->a[i].pCost;
    while( pCost ){
      pNext = pCost->pNext;
      sqlite3_free(pCost);
      pCost = pNext;
    }
  }
  sqlite3_free(p->a);
  memset(p, 0, sizeof(*p));
}
static void editDist3ConfigDelete(void *pIn){
  EditDist3Config *p = (EditDist3Config*)pIn;
  editDist3ConfigClear(p);
  sqlite3_free(p);
}

/* Compare the FROM values of two EditDist3Cost objects, for sorting.
** Return negative, zero, or positive if the A is less than, equal to,
** or greater than B.
*/
static int editDist3CostCompare(EditDist3Cost *pA, EditDist3Cost *pB){
  int n = pA->nFrom;
  int rc;
  if( n>pB->nFrom ) n = pB->nFrom;
  rc = strncmp(pA->a, pB->a, n);
  if( rc==0 ) rc = pA->nFrom - pB->nFrom;
  return rc;
}

/*
** Merge together two sorted lists of EditDist3Cost objects, in order
** of increasing FROM.
*/
static EditDist3Cost *editDist3CostMerge(
  EditDist3Cost *pA,
  EditDist3Cost *pB
){
  EditDist3Cost *pHead = 0;
  EditDist3Cost **ppTail = &pHead;
  EditDist3Cost *p;
  while( pA && pB ){
    if( editDist3CostCompare(pA,pB)<=0 ){
      p = pA;
      pA = pA->pNext;
    }else{
      p = pB;
      pB = pB->pNext;
    }
    *ppTail = p;
    ppTail =  &p->pNext;
  }
  if( pA ){
    *ppTail = pA;
  }else{
    *ppTail = pB;
  }
  return pHead;
}

/*
** Sort a list of EditDist3Cost objects into order of increasing FROM
*/
static EditDist3Cost *editDist3CostSort(EditDist3Cost *pList){
  EditDist3Cost *ap[60], *p;
  int i;
  int mx = 0;
  ap[0] = 0;
  ap[1] = 0;
  while( pList ){
    p = pList;
    pList = p->pNext;
    p->pNext = 0;
    for(i=0; ap[i]; i++){
      p = editDist3CostMerge(ap[i],p);
      ap[i] = 0;
    }
    ap[i] = p;
    if( i>mx ){
      mx = i;
      ap[i+1] = 0;
    }
  }
  p = 0;
  for(i=0; i<=mx; i++){
    if( ap[i] ) p = editDist3CostMerge(p,ap[i]);
  }
  return p;
}

/*
** Load all edit-distance weights from a table.
*/
static int editDist3ConfigLoad(
  EditDist3Config *p,      /* The edit distance configuration to load */
  sqlite3 *db,            /* Load from this database */
  const char *zTable      /* Name of the table from which to load */
){
  sqlite3_stmt *pStmt;
  int rc, rc2;
  char *zSql;
  int iLangPrev = -9999;
  EditDist3Lang *pLang = 0;

  zSql = sqlite3_mprintf("SELECT iLang, cFrom, cTo, iCost"
                         " FROM \"%w\" WHERE iLang>=0 ORDER BY iLang", zTable);
  if( zSql==0 ) return SQLITE_NOMEM;
  rc = sqlite3_prepare(db, zSql, -1, &pStmt, 0);
  sqlite3_free(zSql);
  if( rc ) return rc;
  editDist3ConfigClear(p);
  while( sqlite3_step(pStmt)==SQLITE_ROW ){
    int iLang = sqlite3_column_int(pStmt, 0);
    const char *zFrom = (const char*)sqlite3_column_text(pStmt, 1);
    int nFrom = zFrom ? sqlite3_column_bytes(pStmt, 1) : 0;
    const char *zTo = (const char*)sqlite3_column_text(pStmt, 2);
    int nTo = zTo ? sqlite3_column_bytes(pStmt, 2) : 0;
    int iCost = sqlite3_column_int(pStmt, 3);

    assert( zFrom!=0 || nFrom==0 );
    assert( zTo!=0 || nTo==0 );
    if( nFrom>100 || nTo>100 ) continue;
    if( iCost<0 ) continue;
    if( iCost>=10000 ) continue;  /* Costs above 10K are considered infinite */
    if( pLang==0 || iLang!=iLangPrev ){
      EditDist3Lang *pNew;
      pNew = sqlite3_realloc64(p->a, (p->nLang+1)*sizeof(p->a[0]));
      if( pNew==0 ){ rc = SQLITE_NOMEM; break; }
      p->a = pNew;
      pLang = &p->a[p->nLang];
      p->nLang++;
      pLang->iLang = iLang;
      pLang->iInsCost = 100;
      pLang->iDelCost = 100;
      pLang->iSubCost = 150;
      pLang->pCost = 0;
      iLangPrev = iLang;
    }
    if( nFrom==1 && zFrom[0]=='?' && nTo==0 ){
      pLang->iDelCost = iCost;
    }else if( nFrom==0 && nTo==1 && zTo[0]=='?' ){
      pLang->iInsCost = iCost;
    }else if( nFrom==1 && nTo==1 && zFrom[0]=='?' && zTo[0]=='?' ){
      pLang->iSubCost = iCost;
    }else{
      EditDist3Cost *pCost;
      int nExtra = nFrom + nTo - 4;
      if( nExtra<0 ) nExtra = 0;
      pCost = sqlite3_malloc64( sizeof(*pCost) + nExtra );
      if( pCost==0 ){ rc = SQLITE_NOMEM; break; }
      pCost->nFrom = (u8)nFrom;
      pCost->nTo = (u8)nTo;
      pCost->iCost = (u16)iCost;
      memcpy(pCost->a, zFrom, nFrom);
      memcpy(pCost->a + nFrom, zTo, nTo);
      pCost->pNext = pLang->pCost;
      pLang->pCost = pCost; 
    }
  }
  rc2 = sqlite3_finalize(pStmt);
  if( rc==SQLITE_OK ) rc = rc2;
  if( rc==SQLITE_OK ){
    int iLang;
    for(iLang=0; iLang<p->nLang; iLang++){
      p->a[iLang].pCost = editDist3CostSort(p->a[iLang].pCost);
    }
  }
  return rc;
}

/*
** Return the length (in bytes) of a utf-8 character.  Or return a maximum
** of N.
*/
static int utf8Len(unsigned char c, int N){
  int len = 1;
  if( c>0x7f ){
    if( (c&0xe0)==0xc0 ){
      len = 2;
    }else if( (c&0xf0)==0xe0 ){
      len = 3;
    }else{
      len = 4;
    }
  }
  if( len>N ) len = N;
  return len;
}

/*
** Return TRUE (non-zero) if the To side of the given cost matches
** the given string.
*/
static int matchTo(EditDist3Cost *p, const char *z, int n){
  assert( n>0 );
  if( p->a[p->nFrom]!=z[0] ) return 0;
  if( p->nTo>n ) return 0;
  if( strncmp(p->a+p->nFrom, z, p->nTo)!=0 ) return 0;
  return 1;
}

/*
** Return TRUE (non-zero) if the From side of the given cost matches
** the given string.
*/
static int matchFrom(EditDist3Cost *p, const char *z, int n){
  assert( p->nFrom<=n );
  if( p->nFrom ){
    if( p->a[0]!=z[0] ) return 0;
    if( strncmp(p->a, z, p->nFrom)!=0 ) return 0;
  }
  return 1;
}

/*
** Return TRUE (non-zero) of the next FROM character and the next TO
** character are the same.
*/
static int matchFromTo(
  EditDist3FromString *pStr,  /* Left hand string */
  int n1,                     /* Index of comparison character on the left */
  const char *z2,             /* Right-handl comparison character */
  int n2                      /* Bytes remaining in z2[] */
){
  int b1 = pStr->a[n1].nByte;
  if( b1>n2 ) return 0;
  assert( b1>0 );
  if( pStr->z[n1]!=z2[0] ) return 0;
  if( strncmp(pStr->z+n1, z2, b1)!=0 ) return 0;
  return 1;
}

/*
** Delete an EditDist3FromString objecct
*/
static void editDist3FromStringDelete(EditDist3FromString *p){
  int i;
  if( p ){
    for(i=0; i<p->n; i++){
      sqlite3_free(p->a[i].apDel);
      sqlite3_free(p->a[i].apSubst);
    }
    sqlite3_free(p);
  }
}

/*
** Create a EditDist3FromString object.
*/
static EditDist3FromString *editDist3FromStringNew(
  const EditDist3Lang *pLang,
  const char *z,
  int n
){
  EditDist3FromString *pStr;
  EditDist3Cost *p;
  int i;

  if( z==0 ) return 0;
  if( n<0 ) n = (int)strlen(z);
  pStr = sqlite3_malloc64( sizeof(*pStr) + sizeof(pStr->a[0])*n + n + 1 );
  if( pStr==0 ) return 0;
  pStr->a = (EditDist3From*)&pStr[1];
  memset(pStr->a, 0, sizeof(pStr->a[0])*n);
  pStr->n = n;
  pStr->z = (char*)&pStr->a[n];
  memcpy(pStr->z, z, n+1);
  if( n && z[n-1]=='*' ){
    pStr->isPrefix = 1;
    n--;
    pStr->n--;
    pStr->z[n] = 0;
  }else{
    pStr->isPrefix = 0;
  }

  for(i=0; i<n; i++){
    EditDist3From *pFrom = &pStr->a[i];
    memset(pFrom, 0, sizeof(*pFrom));
    pFrom->nByte = utf8Len((unsigned char)z[i], n-i);
    for(p=pLang->pCost; p; p=p->pNext){
      EditDist3Cost **apNew;
      if( i+p->nFrom>n ) continue;
      if( matchFrom(p, z+i, n-i)==0 ) continue;
      if( p->nTo==0 ){
        apNew = sqlite3_realloc64(pFrom->apDel,
                                sizeof(*apNew)*(pFrom->nDel+1));
        if( apNew==0 ) break;
        pFrom->apDel = apNew;
        apNew[pFrom->nDel++] = p;
      }else{
        apNew = sqlite3_realloc64(pFrom->apSubst,
                                sizeof(*apNew)*(pFrom->nSubst+1));
        if( apNew==0 ) break;
        pFrom->apSubst = apNew;
        apNew[pFrom->nSubst++] = p;
      }
    }
    if( p ){
      editDist3FromStringDelete(pStr);
      pStr = 0;
      break;
    }
  }
  return pStr;
}

/*
** Update entry m[i] such that it is the minimum of its current value
** and m[j]+iCost.
*/
static void updateCost(
  unsigned int *m,
  int i,
  int j,
  int iCost
){
  unsigned int b;
  assert( iCost>=0 );
  assert( iCost<10000 );
  b = m[j] + iCost;
  if( b<m[i] ) m[i] = b;
}

/*
** How much stack space (int bytes) to use for Wagner matrix in 
** editDist3Core().  If more space than this is required, the entire
** matrix is taken from the heap.  To reduce the load on the memory
** allocator, make this value as large as practical for the
** architecture in use.
*/
#ifndef SQLITE_SPELLFIX_STACKALLOC_SZ
# define SQLITE_SPELLFIX_STACKALLOC_SZ  (1024)
#endif

/* Compute the edit distance between two strings.
**
** If an error occurs, return a negative number which is the error code.
**
** If pnMatch is not NULL, then *pnMatch is set to the number of characters
** (not bytes) in z2 that matched the search pattern in *pFrom. If pFrom does
** not contain the pattern for a prefix-search, then this is always the number
** of characters in z2. If pFrom does contain a prefix search pattern, then
** it is the number of characters in the prefix of z2 that was deemed to 
** match pFrom.
*/
static int editDist3Core(
  EditDist3FromString *pFrom,  /* The FROM string */
  const char *z2,              /* The TO string */
  int n2,                      /* Length of the TO string */
  const EditDist3Lang *pLang,  /* Edit weights for a particular language ID */
  int *pnMatch                 /* OUT: Characters in matched prefix */
){
  int k, n;
  int i1, b1;
  int i2, b2;
  EditDist3FromString f = *pFrom;
  EditDist3To *a2;
  unsigned int *m;
  unsigned int *pToFree;
  int szRow;
  EditDist3Cost *p;
  int res;
  sqlite3_uint64 nByte;
  unsigned int stackSpace[SQLITE_SPELLFIX_STACKALLOC_SZ/sizeof(unsigned int)];

  /* allocate the Wagner matrix and the aTo[] array for the TO string */
  n = (f.n+1)*(n2+1);
  n = (n+1)&~1;
  nByte = n*sizeof(m[0]) + sizeof(a2[0])*n2;
  if( nByte<=sizeof(stackSpace) ){
    m = stackSpace;
    pToFree = 0;
  }else{
    m = pToFree = sqlite3_malloc64( nByte );
    if( m==0 ) return -1;            /* Out of memory */
  }
  a2 = (EditDist3To*)&m[n];
  memset(a2, 0, sizeof(a2[0])*n2);

  /* Fill in the a1[] matrix for all characters of the TO string */
  for(i2=0; i2<n2; i2++){
    a2[i2].nByte = utf8Len((unsigned char)z2[i2], n2-i2);
    for(p=pLang->pCost; p; p=p->pNext){
      EditDist3Cost **apNew;
      if( p->nFrom>0 ) break;
      if( i2+p->nTo>n2 ) continue;
      if( p->a[0]>z2[i2] ) break;
      if( matchTo(p, z2+i2, n2-i2)==0 ) continue;
      a2[i2].nIns++;
      apNew = sqlite3_realloc64(a2[i2].apIns, sizeof(*apNew)*a2[i2].nIns);
      if( apNew==0 ){
        res = -1;  /* Out of memory */
        goto editDist3Abort;
      }
      a2[i2].apIns = apNew;
      a2[i2].apIns[a2[i2].nIns-1] = p;
    }
  }

  /* Prepare to compute the minimum edit distance */
  szRow = f.n+1;
  memset(m, 0x01, (n2+1)*szRow*sizeof(m[0]));
  m[0] = 0;

  /* First fill in the top-row of the matrix with FROM deletion costs */
  for(i1=0; i1<f.n; i1 += b1){
    b1 = f.a[i1].nByte;
    updateCost(m, i1+b1, i1, pLang->iDelCost);
    for(k=0; k<f.a[i1].nDel; k++){
      p = f.a[i1].apDel[k];
      updateCost(m, i1+p->nFrom, i1, p->iCost);
    }
  }

  /* Fill in all subsequent rows, top-to-bottom, left-to-right */
  for(i2=0; i2<n2; i2 += b2){
    int rx;      /* Starting index for current row */
    int rxp;     /* Starting index for previous row */
    b2 = a2[i2].nByte;
    rx = szRow*(i2+b2);
    rxp = szRow*i2;
    updateCost(m, rx, rxp, pLang->iInsCost);
    for(k=0; k<a2[i2].nIns; k++){
      p = a2[i2].apIns[k];
      updateCost(m, szRow*(i2+p->nTo), rxp, p->iCost);
    }
    for(i1=0; i1<f.n; i1+=b1){
      int cx;    /* Index of current cell */
      int cxp;   /* Index of cell immediately to the left */
      int cxd;   /* Index of cell to the left and one row above */
      int cxu;   /* Index of cell immediately above */
      b1 = f.a[i1].nByte;
      cxp = rx + i1;
      cx = cxp + b1;
      cxd = rxp + i1;
      cxu = cxd + b1;
      updateCost(m, cx, cxp, pLang->iDelCost);
      for(k=0; k<f.a[i1].nDel; k++){
        p = f.a[i1].apDel[k];
        updateCost(m, cxp+p->nFrom, cxp, p->iCost);
      }
      updateCost(m, cx, cxu, pLang->iInsCost);
      if( matchFromTo(&f, i1, z2+i2, n2-i2) ){
        updateCost(m, cx, cxd, 0);
      }
      updateCost(m, cx, cxd, pLang->iSubCost);
      for(k=0; k<f.a[i1].nSubst; k++){
        p = f.a[i1].apSubst[k];
        if( matchTo(p, z2+i2, n2-i2) ){
          updateCost(m, cxd+p->nFrom+szRow*p->nTo, cxd, p->iCost);
        }
      }
    }
  }

#if 0  /* Enable for debugging */
  printf("         ^");
  for(i1=0; i1<f.n; i1++) printf(" %c-%2x", f.z[i1], f.z[i1]&0xff);
  printf("\n   ^:");
  for(i1=0; i1<szRow; i1++){
    int v = m[i1];
    if( v>9999 ) printf(" ****");
    else         printf(" %4d", v);
  }
  printf("\n");
  for(i2=0; i2<n2; i2++){
    printf("%c-%02x:", z2[i2], z2[i2]&0xff);
    for(i1=0; i1<szRow; i1++){
      int v = m[(i2+1)*szRow+i1];
      if( v>9999 ) printf(" ****");
      else         printf(" %4d", v);
    }
    printf("\n");
  }
#endif

  /* Free memory allocations and return the result */
  res = (int)m[szRow*(n2+1)-1];
  n = n2;
  if( f.isPrefix ){
    for(i2=1; i2<=n2; i2++){
      int b = m[szRow*i2-1];
      if( b<=res ){ 
        res = b;
        n = i2 - 1;
      }
    }
  }
  if( pnMatch ){
    int nExtra = 0;
    for(k=0; k<n; k++){
      if( (z2[k] & 0xc0)==0x80 ) nExtra++;
    }
    *pnMatch = n - nExtra;
  }

editDist3Abort:
  for(i2=0; i2<n2; i2++) sqlite3_free(a2[i2].apIns);
  sqlite3_free(pToFree);
  return res;
}

/*
** Get an appropriate EditDist3Lang object.
*/
static const EditDist3Lang *editDist3FindLang(
  EditDist3Config *pConfig,
  int iLang
){
  int i;
  for(i=0; i<pConfig->nLang; i++){
    if( pConfig->a[i].iLang==iLang ) return &pConfig->a[i];
  }
  return &editDist3Lang;
}

/*
** Function:    editdist3(A,B,iLang)
**              editdist3(tablename)
**
** Return the cost of transforming string A into string B using edit
** weights for iLang.
**
** The second form loads edit weights into memory from a table.
*/
static void editDist3SqlFunc(
  sqlite3_context *context,
  int argc,
  sqlite3_value **argv
){
  EditDist3Config *pConfig = (EditDist3Config*)sqlite3_user_data(context);
  sqlite3 *db = sqlite3_context_db_handle(context);
  int rc;
  if( argc==1 ){
    const char *zTable = (const char*)sqlite3_value_text(argv[0]);
    rc = editDist3ConfigLoad(pConfig, db, zTable);
    if( rc ) sqlite3_result_error_code(context, rc);
  }else{
    const char *zA = (const char*)sqlite3_value_text(argv[0]);
    const char *zB = (const char*)sqlite3_value_text(argv[1]);
    int nA = sqlite3_value_bytes(argv[0]);
    int nB = sqlite3_value_bytes(argv[1]);
    int iLang = argc==3 ? sqlite3_value_int(argv[2]) : 0;
    const EditDist3Lang *pLang = editDist3FindLang(pConfig, iLang);
    EditDist3FromString *pFrom;
    int dist;

    pFrom = editDist3FromStringNew(pLang, zA, nA);
    if( pFrom==0 ){
      sqlite3_result_error_nomem(context);
      return;
    }
    dist = editDist3Core(pFrom, zB, nB, pLang, 0);
    editDist3FromStringDelete(pFrom);
    if( dist==(-1) ){
      sqlite3_result_error_nomem(context);
    }else{
      sqlite3_result_int(context, dist);
    }
  } 
}

/*
** Register the editDist3 function with SQLite
*/
static int editDist3Install(sqlite3 *db){
  int rc;
  EditDist3Config *pConfig = sqlite3_malloc64( sizeof(*pConfig) );

  if( pConfig==0 ) return SQLITE_NOMEM;
  memset(pConfig, 0, sizeof(*pConfig));
  rc = sqlite3_create_function_v2(db, "editdist3",
              2, SQLITE_UTF8|SQLITE_DETERMINISTIC, pConfig,
              editDist3SqlFunc, 0, 0, 0);
  if( rc==SQLITE_OK ){
    rc = sqlite3_create_function_v2(db, "editdist3",
                3, SQLITE_UTF8|SQLITE_DETERMINISTIC, pConfig,
                editDist3SqlFunc, 0, 0, 0);
  }
  if( rc==SQLITE_OK ){
    rc = sqlite3_create_function_v2(db, "editdist3",
                1, SQLITE_UTF8|SQLITE_DETERMINISTIC, pConfig,
                editDist3SqlFunc, 0, 0, editDist3ConfigDelete);
  }else{
    sqlite3_free(pConfig);
  }
  return rc;
}
/* End configurable cost unicode edit distance routines
******************************************************************************
******************************************************************************
** Begin transliterate unicode-to-ascii implementation
*/

#if !SQLITE_AMALGAMATION
/*
** This lookup table is used to help decode the first byte of
** a multi-byte UTF8 character.
*/
static const unsigned char sqlite3Utf8Trans1[] = {
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
  0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
  0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  0x00, 0x01, 0x02, 0x03, 0x00, 0x01, 0x00, 0x00,
};
#endif

/*
** Return the value of the first UTF-8 character in the string.
*/
static int utf8Read(const unsigned char *z, int n, int *pSize){
  int c, i;

  /* All callers to this routine (in the current implementation)
  ** always have n>0. */
  if( NEVER(n==0) ){
    c = i = 0;
  }else{
    c = z[0];
    i = 1;
    if( c>=0xc0 ){
      c = sqlite3Utf8Trans1[c-0xc0];
      while( i<n && (z[i] & 0xc0)==0x80 ){
        c = (c<<6) + (0x3f & z[i++]);
      }
    }
  }
  *pSize = i;
  return c;
}

/*
** Return the number of characters in the utf-8 string in the nIn byte
** buffer pointed to by zIn.
*/
static int utf8Charlen(const char *zIn, int nIn){
  int i;
  int nChar = 0;
  for(i=0; i<nIn; nChar++){
    int sz;
    utf8Read((const unsigned char *)&zIn[i], nIn-i, &sz);
    i += sz;
  }
  return nChar;
}

typedef struct Transliteration Transliteration;
struct Transliteration {
 unsigned short int cFrom;
 unsigned char cTo0, cTo1, cTo2, cTo3;
#ifdef SQLITE_SPELLFIX_5BYTE_MAPPINGS
 unsigned char cTo4;
#endif
};

/*
** Table of translations from unicode characters into ASCII.
*/
static const Transliteration translit[] = {
  { 0x00A0,  0x20, 0x00, 0x00, 0x00 },  /*   to   */
  { 0x00B5,  0x75, 0x00, 0x00, 0x00 },  /* µ to u */
  { 0x00C0,  0x41, 0x00, 0x00, 0x00 },  /* À to A */
  { 0x00C1,  0x41, 0x00, 0x00, 0x00 },  /* Á to A */
  { 0x00C2,  0x41, 0x00, 0x00, 0x00 },  /* Â to A */
  { 0x00C3,  0x41, 0x00, 0x00, 0x00 },  /* Ã to A */
  { 0x00C4,  0x41, 0x65, 0x00, 0x00 },  /* Ä to Ae */
  { 0x00C5,  0x41, 0x61, 0x00, 0x00 },  /* Å to Aa */
  { 0x00C6,  0x41, 0x45, 0x00, 0x00 },  /* Æ to AE */
  { 0x00C7,  0x43, 0x00, 0x00, 0x00 },  /* Ç to C */
  { 0x00C8,  0x45, 0x00, 0x00, 0x00 },  /* È to E */
  { 0x00C9,  0x45, 0x00, 0x00, 0x00 },  /* É to E */
  { 0x00CA,  0x45, 0x00, 0x00, 0x00 },  /* Ê to E */
  { 0x00CB,  0x45, 0x00, 0x00, 0x00 },  /* Ë to E */
  { 0x00CC,  0x49, 0x00, 0x00, 0x00 },  /* Ì to I */
  { 0x00CD,  0x49, 0x00, 0x00, 0x00 },  /* Í to I */
  { 0x00CE,  0x49, 0x00, 0x00, 0x00 },  /* Î to I */
  { 0x00CF,  0x49, 0x00, 0x00, 0x00 },  /* Ï to I */
  { 0x00D0,  0x44, 0x00, 0x00, 0x00 },  /* Ð to D */
  { 0x00D1,  0x4E, 0x00, 0x00, 0x00 },  /* Ñ to N */
  { 0x00D2,  0x4F, 0x00, 0x00, 0x00 },  /* Ò to O */
  { 0x00D3,  0x4F, 0x00, 0x00, 0x00 },  /* Ó to O */
  { 0x00D4,  0x4F, 0x00, 0x00, 0x00 },  /* Ô to O */
  { 0x00D5,  0x4F, 0x00, 0x00, 0x00 },  /* Õ to O */
  { 0x00D6,  0x4F, 0x65, 0x00, 0x00 },  /* Ö to Oe */
  { 0x00D7,  0x78, 0x00, 0x00, 0x00 },  /* × to x */
  { 0x00D8,  0x4F, 0x00, 0x00, 0x00 },  /* Ø to O */
  { 0x00D9,  0x55, 0x00, 0x00, 0x00 },  /* Ù to U */
  { 0x00DA,  0x55, 0x00, 0x00, 0x00 },  /* Ú to U */
  { 0x00DB,  0x55, 0x00, 0x00, 0x00 },  /* Û to U */
  { 0x00DC,  0x55, 0x65, 0x00, 0x00 },  /* Ü to Ue */
  { 0x00DD,  0x59, 0x00, 0x00, 0x00 },  /* Ý to Y */
  { 0x00DE,  0x54, 0x68, 0x00, 0x00 },  /* Þ to Th */
  { 0x00DF,  0x73, 0x73, 0x00, 0x00 },  /* ß to ss */
  { 0x00E0,  0x61, 0x00, 0x00, 0x00 },  /* à to a */
  { 0x00E1,  0x61, 0x00, 0x00, 0x00 },  /* á to a */
  { 0x00E2,  0x61, 0x00, 0x00, 0x00 },  /* â to a */
  { 0x00E3,  0x61, 0x00, 0x00, 0x00 },  /* ã to a */
  { 0x00E4,  0x61, 0x65, 0x00, 0x00 },  /* ä to ae */
  { 0x00E5,  0x61, 0x61, 0x00, 0x00 },  /* å to aa */
  { 0x00E6,  0x61, 0x65, 0x00, 0x00 },  /* æ to ae */
  { 0x00E7,  0x63, 0x00, 0x00, 0x00 },  /* ç to c */
  { 0x00E8,  0x65, 0x00, 0x00, 0x00 },  /* è to e */
  { 0x00E9,  0x65, 0x00, 0x00, 0x00 },  /* é to e */
  { 0x00EA,  0x65, 0x00, 0x00, 0x00 },  /* ê to e */
  { 0x00EB,  0x65, 0x00, 0x00, 0x00 },  /* ë to e */
  { 0x00EC,  0x69, 0x00, 0x00, 0x00 },  /* ì to i */
  { 0x00ED,  0x69, 0x00, 0x00, 0x00 },  /* í to i */
  { 0x00EE,  0x69, 0x00, 0x00, 0x00 },  /* î to i */
  { 0x00EF,  0x69, 0x00, 0x00, 0x00 },  /* ï to i */
  { 0x00F0,  0x64, 0x00, 0x00, 0x00 },  /* ð to d */
  { 0x00F1,  0x6E, 0x00, 0x00, 0x00 },  /* ñ to n */
  { 0x00F2,  0x6F, 0x00, 0x00, 0x00 },  /* ò to o */
  { 0x00F3,  0x6F, 0x00, 0x00, 0x00 },  /* ó to o */
  { 0x00F4,  0x6F, 0x00, 0x00, 0x00 },  /* ô to o */
  { 0x00F5,  0x6F, 0x00, 0x00, 0x00 },  /* õ to o */
  { 0x00F6,  0x6F, 0x65, 0x00, 0x00 },  /* ö to oe */
  { 0x00F7,  0x3A, 0x00, 0x00, 0x00 },  /* ÷ to : */
  { 0x00F8,  0x6F, 0x00, 0x00, 0x00 },  /* ø to o */
  { 0x00F9,  0x75, 0x00, 0x00, 0x00 },  /* ù to u */
  { 0x00FA,  0x75, 0x00, 0x00, 0x00 },  /* ú to u */
  { 0x00FB,  0x75, 0x00, 0x00, 0x00 },  /* û to u */
  { 0x00FC,  0x75, 0x65, 0x00, 0x00 },  /* ü to ue */
  { 0x00FD,  0x79, 0x00, 0x00, 0x00 },  /* ý to y */
  { 0x00FE,  0x74, 0x68, 0x00, 0x00 },  /* þ to th */
  { 0x00FF,  0x79, 0x00, 0x00, 0x00 },  /* ÿ to y */
  { 0x0100,  0x41, 0x00, 0x00, 0x00 },  /* Ā to A */
  { 0x0101,  0x61, 0x00, 0x00, 0x00 },  /* ā to a */
  { 0x0102,  0x41, 0x00, 0x00, 0x00 },  /* Ă to A */
  { 0x0103,  0x61, 0x00, 0x00, 0x00 },  /* ă to a */
  { 0x0104,  0x41, 0x00, 0x00, 0x00 },  /* Ą to A */
  { 0x0105,  0x61, 0x00, 0x00, 0x00 },  /* ą to a */
  { 0x0106,  0x43, 0x00, 0x00, 0x00 },  /* Ć to C */
  { 0x0107,  0x63, 0x00, 0x00, 0x00 },  /* ć to c */
  { 0x0108,  0x43, 0x68, 0x00, 0x00 },  /* Ĉ to Ch */
  { 0x0109,  0x63, 0x68, 0x00, 0x00 },  /* ĉ to ch */
  { 0x010A,  0x43, 0x00, 0x00, 0x00 },  /* Ċ to C */
  { 0x010B,  0x63, 0x00, 0x00, 0x00 },  /* ċ to c */
  { 0x010C,  0x43, 0x00, 0x00, 0x00 },  /* Č to C */
  { 0x010D,  0x63, 0x00, 0x00, 0x00 },  /* č to c */
  { 0x010E,  0x44, 0x00, 0x00, 0x00 },  /* Ď to D */
  { 0x010F,  0x64, 0x00, 0x00, 0x00 },  /* ď to d */
  { 0x0110,  0x44, 0x00, 0x00, 0x00 },  /* Đ to D */
  { 0x0111,  0x64, 0x00, 0x00, 0x00 },  /* đ to d */
  { 0x0112,  0x45, 0x00, 0x00, 0x00 },  /* Ē to E */
  { 0x0113,  0x65, 0x00, 0x00, 0x00 },  /* ē to e */
  { 0x0114,  0x45, 0x00, 0x00, 0x00 },  /* Ĕ to E */
  { 0x0115,  0x65, 0x00, 0x00, 0x00 },  /* ĕ to e */
  { 0x0116,  0x45, 0x00, 0x00, 0x00 },  /* Ė to E */
  { 0x0117,  0x65, 0x00, 0x00, 0x00 },  /* ė to e */
  { 0x0118,  0x45, 0x00, 0x00, 0x00 },  /* Ę to E */
  { 0x0119,  0x65, 0x00, 0x00, 0x00 },  /* ę to e */
  { 0x011A,  0x45, 0x00, 0x00, 0x00 },  /* Ě to E */
  { 0x011B,  0x65, 0x00, 0x00, 0x00 },  /* ě to e */
  { 0x011C,  0x47, 0x68, 0x00, 0x00 },  /* Ĝ to Gh */
  { 0x011D,  0x67, 0x68, 0x00, 0x00 },  /* ĝ to gh */
  { 0x011E,  0x47, 0x00, 0x00, 0x00 },  /* Ğ to G */
  { 0x011F,  0x67, 0x00, 0x00, 0x00 },  /* ğ to g */
  { 0x0120,  0x47, 0x00, 0x00, 0x00 },  /* Ġ to G */
  { 0x0121,  0x67, 0x00, 0x00, 0x00 },  /* ġ to g */
  { 0x0122,  0x47, 0x00, 0x00, 0x00 },  /* Ģ to G */
  { 0x0123,  0x67, 0x00, 0x00, 0x00 },  /* ģ to g */
  { 0x0124,  0x48, 0x68, 0x00, 0x00 },  /* Ĥ to Hh */
  { 0x0125,  0x68, 0x68, 0x00, 0x00 },  /* ĥ to hh */
  { 0x0126,  0x48, 0x00, 0x00, 0x00 },  /* Ħ to H */
  { 0x0127,  0x68, 0x00, 0x00, 0x00 },  /* ħ to h */
  { 0x0128,  0x49, 0x00, 0x00, 0x00 },  /* Ĩ to I */
  { 0x0129,  0x69, 0x00, 0x00, 0x00 },  /* ĩ to i */
  { 0x012A,  0x49, 0x00, 0x00, 0x00 },  /* Ī to I */
  { 0x012B,  0x69, 0x00, 0x00, 0x00 },  /* ī to i */
  { 0x012C,  0x49, 0x00, 0x00, 0x00 },  /* Ĭ to I */
  { 0x012D,  0x69, 0x00, 0x00, 0x00 },  /* ĭ to i */
  { 0x012E,  0x49, 0x00, 0x00, 0x00 },  /* Į to I */
  { 0x012F,  0x69, 0x00, 0x00, 0x00 },  /* į to i */
  { 0x0130,  0x49, 0x00, 0x00, 0x00 },  /* İ to I */
  { 0x0131,  0x69, 0x00, 0x00, 0x00 },  /* ı to i */
  { 0x0132,  0x49, 0x4A, 0x00, 0x00 },  /* Ĳ to IJ */
  { 0x0133,  0x69, 0x6A, 0x00, 0x00 },  /* ĳ to ij */
  { 0x0134,  0x4A, 0x68, 0x00, 0x00 },  /* Ĵ to Jh */
  { 0x0135,  0x6A, 0x68, 0x00, 0x00 },  /* ĵ to jh */
  { 0x0136,  0x4B, 0x00, 0x00, 0x00 },  /* Ķ to K */
  { 0x0137,  0x6B, 0x00, 0x00, 0x00 },  /* ķ to k */
  { 0x0138,  0x6B, 0x00, 0x00, 0x00 },  /* ĸ to k */
  { 0x0139,  0x4C, 0x00, 0x00, 0x00 },  /* Ĺ to L */
  { 0x013A,  0x6C, 0x00, 0x00, 0x00 },  /* ĺ to l */
  { 0x013B,  0x4C, 0x00, 0x00, 0x00 },  /* Ļ to L */
  { 0x013C,  0x6C, 0x00, 0x00, 0x00 },  /* ļ to l */
  { 0x013D,  0x4C, 0x00, 0x00, 0x00 },  /* Ľ to L */
  { 0x013E,  0x6C, 0x00, 0x00, 0x00 },  /* ľ to l */
  { 0x013F,  0x4C, 0x2E, 0x00, 0x00 },  /* Ŀ to L. */
  { 0x0140,  0x6C, 0x2E, 0x00, 0x00 },  /* ŀ to l. */
  { 0x0141,  0x4C, 0x00, 0x00, 0x00 },  /* Ł to L */
  { 0x0142,  0x6C, 0x00, 0x00, 0x00 },  /* ł to l */
  { 0x0143,  0x4E, 0x00, 0x00, 0x00 },  /* Ń to N */
  { 0x0144,  0x6E, 0x00, 0x00, 0x00 },  /* ń to n */
  { 0x0145,  0x4E, 0x00, 0x00, 0x00 },  /* Ņ to N */
  { 0x0146,  0x6E, 0x00, 0x00, 0x00 },  /* ņ to n */
  { 0x0147,  0x4E, 0x00, 0x00, 0x00 },  /* Ň to N */
  { 0x0148,  0x6E, 0x00, 0x00, 0x00 },  /* ň to n */
  { 0x0149,  0x27, 0x6E, 0x00, 0x00 },  /* ŉ to 'n */
  { 0x014A,  0x4E, 0x47, 0x00, 0x00 },  /* Ŋ to NG */
  { 0x014B,  0x6E, 0x67, 0x00, 0x00 },  /* ŋ to ng */
  { 0x014C,  0x4F, 0x00, 0x00, 0x00 },  /* Ō to O */
  { 0x014D,  0x6F, 0x00, 0x00, 0x00 },  /* ō to o */
  { 0x014E,  0x4F, 0x00, 0x00, 0x00 },  /* Ŏ to O */
  { 0x014F,  0x6F, 0x00, 0x00, 0x00 },  /* ŏ to o */
  { 0x0150,  0x4F, 0x00, 0x00, 0x00 },  /* Ő to O */
  { 0x0151,  0x6F, 0x00, 0x00, 0x00 },  /* ő to o */
  { 0x0152,  0x4F, 0x45, 0x00, 0x00 },  /* Œ to OE */
  { 0x0153,  0x6F, 0x65, 0x00, 0x00 },  /* œ to oe */
  { 0x0154,  0x52, 0x00, 0x00, 0x00 },  /* Ŕ to R */
  { 0x0155,  0x72, 0x00, 0x00, 0x00 },  /* ŕ to r */
  { 0x0156,  0x52, 0x00, 0x00, 0x00 },  /* Ŗ to R */
  { 0x0157,  0x72, 0x00, 0x00, 0x00 },  /* ŗ to r */
  { 0x0158,  0x52, 0x00, 0x00, 0x00 },  /* Ř to R */
  { 0x0159,  0x72, 0x00, 0x00, 0x00 },  /* ř to r */
  { 0x015A,  0x53, 0x00, 0x00, 0x00 },  /* Ś to S */
  { 0x015B,  0x73, 0x00, 0x00, 0x00 },  /* ś to s */
  { 0x015C,  0x53, 0x68, 0x00, 0x00 },  /* Ŝ to Sh */
  { 0x015D,  0x73, 0x68, 0x00, 0x00 },  /* ŝ to sh */
  { 0x015E,  0x53, 0x00, 0x00, 0x00 },  /* Ş to S */
  { 0x015F,  0x73, 0x00, 0x00, 0x00 },  /* ş to s */
  { 0x0160,  0x53, 0x00, 0x00, 0x00 },  /* Š to S */
  { 0x0161,  0x73, 0x00, 0x00, 0x00 },  /* š to s */
  { 0x0162,  0x54, 0x00, 0x00, 0x00 },  /* Ţ to T */
  { 0x0163,  0x74, 0x00, 0x00, 0x00 },  /* ţ to t */
  { 0x0164,  0x54, 0x00, 0x00, 0x00 },  /* Ť to T */
  { 0x0165,  0x74, 0x00, 0x00, 0x00 },  /* ť to t */
  { 0x0166,  0x54, 0x00, 0x00, 0x00 },  /* Ŧ to T */
  { 0x0167,  0x74, 0x00, 0x00, 0x00 },  /* ŧ to t */
  { 0x0168,  0x55, 0x00, 0x00, 0x00 },  /* Ũ to U */
  { 0x0169,  0x75, 0x00, 0x00, 0x00 },  /* ũ to u */
  { 0x016A,  0x55, 0x00, 0x00, 0x00 },  /* Ū to U */
  { 0x016B,  0x75, 0x00, 0x00, 0x00 },  /* ū to u */
  { 0x016C,  0x55, 0x00, 0x00, 0x00 },  /* Ŭ to U */
  { 0x016D,  0x75, 0x00, 0x00, 0x00 },  /* ŭ to u */
  { 0x016E,  0x55, 0x00, 0x00, 0x00 },  /* Ů to U */
  { 0x016F,  0x75, 0x00, 0x00, 0x00 },  /* ů to u */
  { 0x0170,  0x55, 0x00, 0x00, 0x00 },  /* Ű to U */
  { 0x0171,  0x75, 0x00, 0x00, 0x00 },  /* ű to u */
  { 0x0172,  0x55, 0x00, 0x00, 0x00 },  /* Ų to U */
  { 0x0173,  0x75, 0x00, 0x00, 0x00 },  /* ų to u */
  { 0x0174,  0x57, 0x00, 0x00, 0x00 },  /* Ŵ to W */
  { 0x0175,  0x77, 0x00, 0x00, 0x00 },  /* ŵ to w */
  { 0x0176,  0x59, 0x00, 0x00, 0x00 },  /* Ŷ to Y */
  { 0x0177,  0x79, 0x00, 0x00, 0x00 },  /* ŷ to y */
  { 0x0178,  0x59, 0x00, 0x00, 0x00 },  /* Ÿ to Y */
  { 0x0179,  0x5A, 0x00, 0x00, 0x00 },  /* Ź to Z */
  { 0x017A,  0x7A, 0x00, 0x00, 0x00 },  /* ź to z */
  { 0x017B,  0x5A, 0x00, 0x00, 0x00 },  /* Ż to Z */
  { 0x017C,  0x7A, 0x00, 0x00, 0x00 },  /* ż to z */
  { 0x017D,  0x5A, 0x00, 0x00, 0x00 },  /* Ž to Z */
  { 0x017E,  0x7A, 0x00, 0x00, 0x00 },  /* ž to z */
  { 0x017F,  0x73, 0x00, 0x00, 0x00 },  /* ſ to s */
  { 0x0192,  0x66, 0x00, 0x00, 0x00 },  /* ƒ to f */
  { 0x0218,  0x53, 0x00, 0x00, 0x00 },  /* Ș to S */
  { 0x0219,  0x73, 0x00, 0x00, 0x00 },  /* ș to s */
  { 0x021A,  0x54, 0x00, 0x00, 0x00 },  /* Ț to T */
  { 0x021B,  0x74, 0x00, 0x00, 0x00 },  /* ț to t */
  { 0x0386,  0x41, 0x00, 0x00, 0x00 },  /* Ά to A */
  { 0x0388,  0x45, 0x00, 0x00, 0x00 },  /* Έ to E */
  { 0x0389,  0x49, 0x00, 0x00, 0x00 },  /* Ή to I */
  { 0x038A,  0x49, 0x00, 0x00, 0x00 },  /* Ί to I */
  { 0x038C,  0x4f, 0x00, 0x00, 0x00 },  /* Ό to O */
  { 0x038E,  0x59, 0x00, 0x00, 0x00 },  /* Ύ to Y */
  { 0x038F,  0x4f, 0x00, 0x00, 0x00 },  /* Ώ to O */
  { 0x0390,  0x69, 0x00, 0x00, 0x00 },  /* ΐ to i */
  { 0x0391,  0x41, 0x00, 0x00, 0x00 },  /* Α to A */
  { 0x0392,  0x42, 0x00, 0x00, 0x00 },  /* Β to B */
  { 0x0393,  0x47, 0x00, 0x00, 0x00 },  /* Γ to G */
  { 0x0394,  0x44, 0x00, 0x00, 0x00 },  /* Δ to D */
  { 0x0395,  0x45, 0x00, 0x00, 0x00 },  /* Ε to E */
  { 0x0396,  0x5a, 0x00, 0x00, 0x00 },  /* Ζ to Z */
  { 0x0397,  0x49, 0x00, 0x00, 0x00 },  /* Η to I */
  { 0x0398,  0x54, 0x68, 0x00, 0x00 },  /* Θ to Th */
  { 0x0399,  0x49, 0x00, 0x00, 0x00 },  /* Ι to I */
  { 0x039A,  0x4b, 0x00, 0x00, 0x00 },  /* Κ to K */
  { 0x039B,  0x4c, 0x00, 0x00, 0x00 },  /* Λ to L */
  { 0x039C,  0x4d, 0x00, 0x00, 0x00 },  /* Μ to M */
  { 0x039D,  0x4e, 0x00, 0x00, 0x00 },  /* Ν to N */
  { 0x039E,  0x58, 0x00, 0x00, 0x00 },  /* Ξ to X */
  { 0x039F,  0x4f, 0x00, 0x00, 0x00 },  /* Ο to O */
  { 0x03A0,  0x50, 0x00, 0x00, 0x00 },  /* Π to P */
  { 0x03A1,  0x52, 0x00, 0x00, 0x00 },  /* Ρ to R */
  { 0x03A3,  0x53, 0x00, 0x00, 0x00 },  /* Σ to S */
  { 0x03A4,  0x54, 0x00, 0x00, 0x00 },  /* Τ to T */
  { 0x03A5,  0x59, 0x00, 0x00, 0x00 },  /* Υ to Y */
  { 0x03A6,  0x46, 0x00, 0x00, 0x00 },  /* Φ to F */
  { 0x03A7,  0x43, 0x68, 0x00, 0x00 },  /* Χ to Ch */
  { 0x03A8,  0x50, 0x73, 0x00, 0x00 },  /* Ψ to Ps */
  { 0x03A9,  0x4f, 0x00, 0x00, 0x00 },  /* Ω to O */
  { 0x03AA,  0x49, 0x00, 0x00, 0x00 },  /* Ϊ to I */
  { 0x03AB,  0x59, 0x00, 0x00, 0x00 },  /* Ϋ to Y */
  { 0x03AC,  0x61, 0x00, 0x00, 0x00 },  /* ά to a */
  { 0x03AD,  0x65, 0x00, 0x00, 0x00 },  /* έ to e */
  { 0x03AE,  0x69, 0x00, 0x00, 0x00 },  /* ή to i */
  { 0x03AF,  0x69, 0x00, 0x00, 0x00 },  /* ί to i */
  { 0x03B1,  0x61, 0x00, 0x00, 0x00 },  /* α to a */
  { 0x03B2,  0x62, 0x00, 0x00, 0x00 },  /* β to b */
  { 0x03B3,  0x67, 0x00, 0x00, 0x00 },  /* γ to g */
  { 0x03B4,  0x64, 0x00, 0x00, 0x00 },  /* δ to d */
  { 0x03B5,  0x65, 0x00, 0x00, 0x00 },  /* ε to e */
  { 0x03B6,  0x7a, 0x00, 0x00, 0x00 },  /* ζ to z */
  { 0x03B7,  0x69, 0x00, 0x00, 0x00 },  /* η to i */
  { 0x03B8,  0x74, 0x68, 0x00, 0x00 },  /* θ to th */
  { 0x03B9,  0x69, 0x00, 0x00, 0x00 },  /* ι to i */
  { 0x03BA,  0x6b, 0x00, 0x00, 0x00 },  /* κ to k */
  { 0x03BB,  0x6c, 0x00, 0x00, 0x00 },  /* λ to l */
  { 0x03BC,  0x6d, 0x00, 0x00, 0x00 },  /* μ to m */
  { 0x03BD,  0x6e, 0x00, 0x00, 0x00 },  /* ν to n */
  { 0x03BE,  0x78, 0x00, 0x00, 0x00 },  /* ξ to x */
  { 0x03BF,  0x6f, 0x00, 0x00, 0x00 },  /* ο to o */
  { 0x03C0,  0x70, 0x00, 0x00, 0x00 },  /* π to p */
  { 0x03C1,  0x72, 0x00, 0x00, 0x00 },  /* ρ to r */
  { 0x03C3,  0x73, 0x00, 0x00, 0x00 },  /* σ to s */
  { 0x03C4,  0x74, 0x00, 0x00, 0x00 },  /* τ to t */
  { 0x03C5,  0x79, 0x00, 0x00, 0x00 },  /* υ to y */
  { 0x03C6,  0x66, 0x00, 0x00, 0x00 },  /* φ to f */
  { 0x03C7,  0x63, 0x68, 0x00, 0x00 },  /* χ to ch */
  { 0x03C8,  0x70, 0x73, 0x00, 0x00 },  /* ψ to ps */
  { 0x03C9,  0x6f, 0x00, 0x00, 0x00 },  /* ω to o */
  { 0x03CA,  0x69, 0x00, 0x00, 0x00 },  /* ϊ to i */
  { 0x03CB,  0x79, 0x00, 0x00, 0x00 },  /* ϋ to y */
  { 0x03CC,  0x6f, 0x00, 0x00, 0x00 },  /* ό to o */
  { 0x03CD,  0x79, 0x00, 0x00, 0x00 },  /* ύ to y */
  { 0x03CE,  0x69, 0x00, 0x00, 0x00 },  /* ώ to i */
  { 0x0400,  0x45, 0x00, 0x00, 0x00 },  /* Ѐ to E */
  { 0x0401,  0x45, 0x00, 0x00, 0x00 },  /* Ё to E */
  { 0x0402,  0x44, 0x00, 0x00, 0x00 },  /* Ђ to D */
  { 0x0403,  0x47, 0x00, 0x00, 0x00 },  /* Ѓ to G */
  { 0x0404,  0x45, 0x00, 0x00, 0x00 },  /* Є to E */
  { 0x0405,  0x5a, 0x00, 0x00, 0x00 },  /* Ѕ to Z */
  { 0x0406,  0x49, 0x00, 0x00, 0x00 },  /* І to I */
  { 0x0407,  0x49, 0x00, 0x00, 0x00 },  /* Ї to I */
  { 0x0408,  0x4a, 0x00, 0x00, 0x00 },  /* Ј to J */
  { 0x0409,  0x49, 0x00, 0x00, 0x00 },  /* Љ to I */
  { 0x040A,  0x4e, 0x00, 0x00, 0x00 },  /* Њ to N */
  { 0x040B,  0x44, 0x00, 0x00, 0x00 },  /* Ћ to D */
  { 0x040C,  0x4b, 0x00, 0x00, 0x00 },  /* Ќ to K */
  { 0x040D,  0x49, 0x00, 0x00, 0x00 },  /* Ѝ to I */
  { 0x040E,  0x55, 0x00, 0x00, 0x00 },  /* Ў to U */
  { 0x040F,  0x44, 0x00, 0x00, 0x00 },  /* Џ to D */
  { 0x0410,  0x41, 0x00, 0x00, 0x00 },  /* А to A */
  { 0x0411,  0x42, 0x00, 0x00, 0x00 },  /* Б to B */
  { 0x0412,  0x56, 0x00, 0x00, 0x00 },  /* В to V */
  { 0x0413,  0x47, 0x00, 0x00, 0x00 },  /* Г to G */
  { 0x0414,  0x44, 0x00, 0x00, 0x00 },  /* Д to D */
  { 0x0415,  0x45, 0x00, 0x00, 0x00 },  /* Е to E */
  { 0x0416,  0x5a, 0x68, 0x00, 0x00 },  /* Ж to Zh */
  { 0x0417,  0x5a, 0x00, 0x00, 0x00 },  /* З to Z */
  { 0x0418,  0x49, 0x00, 0x00, 0x00 },  /* И to I */
  { 0x0419,  0x49, 0x00, 0x00, 0x00 },  /* Й to I */
  { 0x041A,  0x4b, 0x00, 0x00, 0x00 },  /* К to K */
  { 0x041B,  0x4c, 0x00, 0x00, 0x00 },  /* Л to L */
  { 0x041C,  0x4d, 0x00, 0x00, 0x00 },  /* М to M */
  { 0x041D,  0x4e, 0x00, 0x00, 0x00 },  /* Н to N */
  { 0x041E,  0x4f, 0x00, 0x00, 0x00 },  /* О to O */
  { 0x041F,  0x50, 0x00, 0x00, 0x00 },  /* П to P */
  { 0x0420,  0x52, 0x00, 0x00, 0x00 },  /* Р to R */
  { 0x0421,  0x53, 0x00, 0x00, 0x00 },  /* С to S */
  { 0x0422,  0x54, 0x00, 0x00, 0x00 },  /* Т to T */
  { 0x0423,  0x55, 0x00, 0x00, 0x00 },  /* У to U */
  { 0x0424,  0x46, 0x00, 0x00, 0x00 },  /* Ф to F */
  { 0x0425,  0x4b, 0x68, 0x00, 0x00 },  /* Х to Kh */
  { 0x0426,  0x54, 0x63, 0x00, 0x00 },  /* Ц to Tc */
  { 0x0427,  0x43, 0x68, 0x00, 0x00 },  /* Ч to Ch */
  { 0x0428,  0x53, 0x68, 0x00, 0x00 },  /* Ш to Sh */
  { 0x0429,  0x53, 0x68, 0x63, 0x68 },  /* Щ to Shch */
  { 0x042A,  0x61, 0x00, 0x00, 0x00 },  /*  to A */
  { 0x042B,  0x59, 0x00, 0x00, 0x00 },  /* Ы to Y */
  { 0x042C,  0x59, 0x00, 0x00, 0x00 },  /*  to Y */
  { 0x042D,  0x45, 0x00, 0x00, 0x00 },  /* Э to E */
  { 0x042E,  0x49, 0x75, 0x00, 0x00 },  /* Ю to Iu */
  { 0x042F,  0x49, 0x61, 0x00, 0x00 },  /* Я to Ia */
  { 0x0430,  0x61, 0x00, 0x00, 0x00 },  /* а to a */
  { 0x0431,  0x62, 0x00, 0x00, 0x00 },  /* б to b */
  { 0x0432,  0x76, 0x00, 0x00, 0x00 },  /* в to v */
  { 0x0433,  0x67, 0x00, 0x00, 0x00 },  /* г to g */
  { 0x0434,  0x64, 0x00, 0x00, 0x00 },  /* д to d */
  { 0x0435,  0x65, 0x00, 0x00, 0x00 },  /* е to e */
  { 0x0436,  0x7a, 0x68, 0x00, 0x00 },  /* ж to zh */
  { 0x0437,  0x7a, 0x00, 0x00, 0x00 },  /* з to z */
  { 0x0438,  0x69, 0x00, 0x00, 0x00 },  /* и to i */
  { 0x0439,  0x69, 0x00, 0x00, 0x00 },  /* й to i */
  { 0x043A,  0x6b, 0x00, 0x00, 0x00 },  /* к to k */
  { 0x043B,  0x6c, 0x00, 0x00, 0x00 },  /* л to l */
  { 0x043C,  0x6d, 0x00, 0x00, 0x00 },  /* м to m */
  { 0x043D,  0x6e, 0x00, 0x00, 0x00 },  /* н to n */
  { 0x043E,  0x6f, 0x00, 0x00, 0x00 },  /* о to o */
  { 0x043F,  0x70, 0x00, 0x00, 0x00 },  /* п to p */
  { 0x0440,  0x72, 0x00, 0x00, 0x00 },  /* р to r */
  { 0x0441,  0x73, 0x00, 0x00, 0x00 },  /* с to s */
  { 0x0442,  0x74, 0x00, 0x00, 0x00 },  /* т to t */
  { 0x0443,  0x75, 0x00, 0x00, 0x00 },  /* у to u */
  { 0x0444,  0x66, 0x00, 0x00, 0x00 },  /* ф to f */
  { 0x0445,  0x6b, 0x68, 0x00, 0x00 },  /* х to kh */
  { 0x0446,  0x74, 0x63, 0x00, 0x00 },  /* ц to tc */
  { 0x0447,  0x63, 0x68, 0x00, 0x00 },  /* ч to ch */
  { 0x0448,  0x73, 0x68, 0x00, 0x00 },  /* ш to sh */
  { 0x0449,  0x73, 0x68, 0x63, 0x68 },  /* щ to shch */
  { 0x044A,  0x61, 0x00, 0x00, 0x00 },  /*  to a */
  { 0x044B,  0x79, 0x00, 0x00, 0x00 },  /* ы to y */
  { 0x044C,  0x79, 0x00, 0x00, 0x00 },  /*  to y */
  { 0x044D,  0x65, 0x00, 0x00, 0x00 },  /* э to e */
  { 0x044E,  0x69, 0x75, 0x00, 0x00 },  /* ю to iu */
  { 0x044F,  0x69, 0x61, 0x00, 0x00 },  /* я to ia */
  { 0x0450,  0x65, 0x00, 0x00, 0x00 },  /* ѐ to e */
  { 0x0451,  0x65, 0x00, 0x00, 0x00 },  /* ё to e */
  { 0x0452,  0x64, 0x00, 0x00, 0x00 },  /* ђ to d */
  { 0x0453,  0x67, 0x00, 0x00, 0x00 },  /* ѓ to g */
  { 0x0454,  0x65, 0x00, 0x00, 0x00 },  /* є to e */
  { 0x0455,  0x7a, 0x00, 0x00, 0x00 },  /* ѕ to z */
  { 0x0456,  0x69, 0x00, 0x00, 0x00 },  /* і to i */
  { 0x0457,  0x69, 0x00, 0x00, 0x00 },  /* ї to i */
  { 0x0458,  0x6a, 0x00, 0x00, 0x00 },  /* ј to j */
  { 0x0459,  0x69, 0x00, 0x00, 0x00 },  /* љ to i */
  { 0x045A,  0x6e, 0x00, 0x00, 0x00 },  /* њ to n */
  { 0x045B,  0x64, 0x00, 0x00, 0x00 },  /* ћ to d */
  { 0x045C,  0x6b, 0x00, 0x00, 0x00 },  /* ќ to k */
  { 0x045D,  0x69, 0x00, 0x00, 0x00 },  /* ѝ to i */
  { 0x045E,  0x75, 0x00, 0x00, 0x00 },  /* ў to u */
  { 0x045F,  0x64, 0x00, 0x00, 0x00 },  /* џ to d */
  { 0x1E02,  0x42, 0x00, 0x00, 0x00 },  /* Ḃ to B */
  { 0x1E03,  0x62, 0x00, 0x00, 0x00 },  /* ḃ to b */
  { 0x1E0A,  0x44, 0x00, 0x00, 0x00 },  /* Ḋ to D */
  { 0x1E0B,  0x64, 0x00, 0x00, 0x00 },  /* ḋ to d */
  { 0x1E1E,  0x46, 0x00, 0x00, 0x00 },  /* Ḟ to F */
  { 0x1E1F,  0x66, 0x00, 0x00, 0x00 },  /* ḟ to f */
  { 0x1E40,  0x4D, 0x00, 0x00, 0x00 },  /* Ṁ to M */
  { 0x1E41,  0x6D, 0x00, 0x00, 0x00 },  /* ṁ to m */
  { 0x1E56,  0x50, 0x00, 0x00, 0x00 },  /* Ṗ to P */
  { 0x1E57,  0x70, 0x00, 0x00, 0x00 },  /* ṗ to p */
  { 0x1E60,  0x53, 0x00, 0x00, 0x00 },  /* Ṡ to S */
  { 0x1E61,  0x73, 0x00, 0x00, 0x00 },  /* ṡ to s */
  { 0x1E6A,  0x54, 0x00, 0x00, 0x00 },  /* Ṫ to T */
  { 0x1E6B,  0x74, 0x00, 0x00, 0x00 },  /* ṫ to t */
  { 0x1E80,  0x57, 0x00, 0x00, 0x00 },  /* Ẁ to W */
  { 0x1E81,  0x77, 0x00, 0x00, 0x00 },  /* ẁ to w */
  { 0x1E82,  0x57, 0x00, 0x00, 0x00 },  /* Ẃ to W */
  { 0x1E83,  0x77, 0x00, 0x00, 0x00 },  /* ẃ to w */
  { 0x1E84,  0x57, 0x00, 0x00, 0x00 },  /* Ẅ to W */
  { 0x1E85,  0x77, 0x00, 0x00, 0x00 },  /* ẅ to w */
  { 0x1EF2,  0x59, 0x00, 0x00, 0x00 },  /* Ỳ to Y */
  { 0x1EF3,  0x79, 0x00, 0x00, 0x00 },  /* ỳ to y */
  { 0xFB00,  0x66, 0x66, 0x00, 0x00 },  /* ﬀ to ff */
  { 0xFB01,  0x66, 0x69, 0x00, 0x00 },  /* ﬁ to fi */
  { 0xFB02,  0x66, 0x6C, 0x00, 0x00 },  /* ﬂ to fl */
  { 0xFB05,  0x73, 0x74, 0x00, 0x00 },  /* ﬅ to st */
  { 0xFB06,  0x73, 0x74, 0x00, 0x00 },  /* ﬆ to st */
};

static const Transliteration *spellfixFindTranslit(int c, int *pxTop){
  *pxTop = (sizeof(translit)/sizeof(translit[0])) - 1;
  return translit;
}

/*
** Convert the input string from UTF-8 into pure ASCII by converting
** all non-ASCII characters to some combination of characters in the
** ASCII subset.
**
** The returned string might contain more characters than the input.
**
** Space to hold the returned string comes from sqlite3_malloc() and
** should be freed by the caller.
*/
static unsigned char *transliterate(const unsigned char *zIn, int nIn){
#ifdef SQLITE_SPELLFIX_5BYTE_MAPPINGS
  unsigned char *zOut = sqlite3_malloc64( nIn*5 + 1 );
#else
  unsigned char *zOut = sqlite3_malloc64( nIn*4 + 1 );
#endif
  int c, sz, nOut;
  if( zOut==0 ) return 0;
  nOut = 0;
  while( nIn>0 ){
    c = utf8Read(zIn, nIn, &sz);
    zIn += sz;
    nIn -= sz;
    if( c<=127 ){
      zOut[nOut++] = (unsigned char)c;
    }else{
      int xTop, xBtm, x;
      const Transliteration *tbl = spellfixFindTranslit(c, &xTop);
      xBtm = 0;
      while( xTop>=xBtm ){
        x = (xTop + xBtm)/2;
        if( tbl[x].cFrom==c ){
          zOut[nOut++] = tbl[x].cTo0;
          if( tbl[x].cTo1 ){
            zOut[nOut++] = tbl[x].cTo1;
            if( tbl[x].cTo2 ){
              zOut[nOut++] = tbl[x].cTo2;
              if( tbl[x].cTo3 ){
                zOut[nOut++] = tbl[x].cTo3;
#ifdef SQLITE_SPELLFIX_5BYTE_MAPPINGS
                if( tbl[x].cTo4 ){
                  zOut[nOut++] = tbl[x].cTo4;
                }
#endif /* SQLITE_SPELLFIX_5BYTE_MAPPINGS */
              }
            }
          }
          c = 0;
          break;
        }else if( tbl[x].cFrom>c ){
          xTop = x-1;
        }else{
          xBtm = x+1;
        }
      }
      if( c ) zOut[nOut++] = '?';
    }
  }
  zOut[nOut] = 0;
  return zOut;
}

/*
** Return the number of characters in the shortest prefix of the input
** string that transliterates to an ASCII string nTrans bytes or longer.
** Or, if the transliteration of the input string is less than nTrans
** bytes in size, return the number of characters in the input string.
*/
static int translen_to_charlen(const char *zIn, int nIn, int nTrans){
  int i, c, sz, nOut;
  int nChar;

  i = nOut = 0;
  for(nChar=0; i<nIn && nOut<nTrans; nChar++){
    c = utf8Read((const unsigned char *)&zIn[i], nIn-i, &sz);
    i += sz;

    nOut++;
    if( c>=128 ){
      int xTop, xBtm, x;
      const Transliteration *tbl = spellfixFindTranslit(c, &xTop);
      xBtm = 0;
      while( xTop>=xBtm ){
        x = (xTop + xBtm)/2;
        if( tbl[x].cFrom==c ){
          if( tbl[x].cTo1 ){
            nOut++;
            if( tbl[x].cTo2 ){
              nOut++;
              if( tbl[x].cTo3 ){
                nOut++;
              }
            }
          }
          break;
        }else if( tbl[x].cFrom>c ){
          xTop = x-1;
        }else{
          xBtm = x+1;
        }
      }
    }
  }

  return nChar;
}


/*
**    spellfix1_translit(X)
**
** Convert a string that contains non-ASCII Roman characters into 
** pure ASCII.
*/
static void transliterateSqlFunc(
  sqlite3_context *context,
  int argc,
  sqlite3_value **argv
){
  const unsigned char *zIn = sqlite3_value_text(argv[0]);
  int nIn = sqlite3_value_bytes(argv[0]);
  unsigned char *zOut = transliterate(zIn, nIn);
  if( zOut==0 ){
    sqlite3_result_error_nomem(context);
  }else{
    sqlite3_result_text(context, (char*)zOut, -1, sqlite3_free);
  }
}

/*
**    spellfix1_scriptcode(X)
**
** Try to determine the dominant script used by the word X and return
** its ISO 15924 numeric code.
**
** The current implementation only understands the following scripts:
**
**    215  (Latin)
**    220  (Cyrillic)
**    200  (Greek)
**
** This routine will return 998 if the input X contains characters from
** two or more of the above scripts or 999 if X contains no characters
** from any of the above scripts.
*/
static void scriptCodeSqlFunc(
  sqlite3_context *context,
  int argc,
  sqlite3_value **argv
){
  const unsigned char *zIn = sqlite3_value_text(argv[0]);
  int nIn = sqlite3_value_bytes(argv[0]);
  int c, sz;
  int scriptMask = 0;
  int res;
  int seenDigit = 0;
# define SCRIPT_LATIN       0x0001
# define SCRIPT_CYRILLIC    0x0002
# define SCRIPT_GREEK       0x0004
# define SCRIPT_HEBREW      0x0008
# define SCRIPT_ARABIC      0x0010

  while( nIn>0 ){
    c = utf8Read(zIn, nIn, &sz);
    zIn += sz;
    nIn -= sz;
    if( c<0x02af ){
      if( c>=0x80 || midClass[c&0x7f]<CCLASS_DIGIT ){
        scriptMask |= SCRIPT_LATIN;
      }else if( c>='0' && c<='9' ){
        seenDigit = 1;
      }
    }else if( c>=0x0400 && c<=0x04ff ){
      scriptMask |= SCRIPT_CYRILLIC;
    }else if( c>=0x0386 && c<=0x03ce ){
      scriptMask |= SCRIPT_GREEK;
    }else if( c>=0x0590 && c<=0x05ff ){
      scriptMask |= SCRIPT_HEBREW;
    }else if( c>=0x0600 && c<=0x06ff ){
      scriptMask |= SCRIPT_ARABIC;
    }
  }
  if( scriptMask==0 && seenDigit ) scriptMask = SCRIPT_LATIN;
  switch( scriptMask ){
    case 0:                res = 999; break;
    case SCRIPT_LATIN:     res = 215; break;
    case SCRIPT_CYRILLIC:  res = 220; break;
    case SCRIPT_GREEK:     res = 200; break;
    case SCRIPT_HEBREW:    res = 125; break;
    case SCRIPT_ARABIC:    res = 160; break;
    default:               res = 998; break;
  }
  sqlite3_result_int(context, res);
}

/* End transliterate
******************************************************************************
******************************************************************************
** Begin spellfix1 virtual table.
*/

/* Maximum length of a phonehash used for querying the shadow table */
#define SPELLFIX_MX_HASH  32

/* Maximum number of hash strings to examine per query */
#define SPELLFIX_MX_RUN   1

typedef struct spellfix1_vtab spellfix1_vtab;
typedef struct spellfix1_cursor spellfix1_cursor;

/* Fuzzy-search virtual table object */
struct spellfix1_vtab {
  sqlite3_vtab base;         /* Base class - must be first */
  sqlite3 *db;               /* Database connection */
  char *zDbName;             /* Name of database holding this table */
  char *zTableName;          /* Name of the virtual table */
  char *zCostTable;          /* Table holding edit-distance cost numbers */
  EditDist3Config *pConfig3; /* Parsed edit distance costs */
};

/* Fuzzy-search cursor object */
struct spellfix1_cursor {
  sqlite3_vtab_cursor base;    /* Base class - must be first */
  spellfix1_vtab *pVTab;       /* The table to which this cursor belongs */
  char *zPattern;              /* rhs of MATCH clause */
  int idxNum;                  /* idxNum value passed to xFilter() */
  int nRow;                    /* Number of rows of content */
  int nAlloc;                  /* Number of allocated rows */
  int iRow;                    /* Current row of content */
  int iLang;                   /* Value of the langid= constraint */
  int iTop;                    /* Value of the top= constraint */
  int iScope;                  /* Value of the scope= constraint */
  int nSearch;                 /* Number of vocabulary items checked */
  sqlite3_stmt *pFullScan;     /* Shadow query for a full table scan */
  struct spellfix1_row {       /* For each row of content */
    sqlite3_int64 iRowid;         /* Rowid for this row */
    char *zWord;                  /* Text for this row */
    int iRank;                    /* Rank for this row */
    int iDistance;                /* Distance from pattern for this row */
    int iScore;                   /* Score for sorting */
    int iMatchlen;                /* Value of matchlen column (or -1) */
    char zHash[SPELLFIX_MX_HASH]; /* the phonehash used for this match */
  } *a; 
};

/*
** Construct one or more SQL statements from the format string given
** and then evaluate those statements. The success code is written
** into *pRc.
**
** If *pRc is initially non-zero then this routine is a no-op.
*/
static void spellfix1DbExec(
  int *pRc,              /* Success code */
  sqlite3 *db,           /* Database in which to run SQL */
  const char *zFormat,   /* Format string for SQL */
  ...                    /* Arguments to the format string */
){
  va_list ap;
  char *zSql;
  if( *pRc ) return;
  va_start(ap, zFormat);
  zSql = sqlite3_vmprintf(zFormat, ap);
  va_end(ap);
  if( zSql==0 ){
    *pRc = SQLITE_NOMEM;
  }else{
    *pRc = sqlite3_exec(db, zSql, 0, 0, 0);
    sqlite3_free(zSql);
  }
}

/*
** xDisconnect/xDestroy method for the fuzzy-search module.
*/
static int spellfix1Uninit(int isDestroy, sqlite3_vtab *pVTab){
  spellfix1_vtab *p = (spellfix1_vtab*)pVTab;
  int rc = SQLITE_OK;
  if( isDestroy ){
    sqlite3 *db = p->db;
    spellfix1DbExec(&rc, db, "DROP TABLE IF EXISTS \"%w\".\"%w_vocab\"",
                  p->zDbName, p->zTableName);
  }
  if( rc==SQLITE_OK ){
    sqlite3_free(p->zTableName);
    editDist3ConfigDelete(p->pConfig3);
    sqlite3_free(p->zCostTable);
    sqlite3_free(p);
  }
  return rc;
}
static int spellfix1Disconnect(sqlite3_vtab *pVTab){
  return spellfix1Uninit(0, pVTab);
}
static int spellfix1Destroy(sqlite3_vtab *pVTab){
  return spellfix1Uninit(1, pVTab);
}

/*
** Make a copy of a string.  Remove leading and trailing whitespace
** and dequote it.
*/
static char *spellfix1Dequote(const char *zIn){
  char *zOut;
  int i, j;
  char c;
  while( isspace((unsigned char)zIn[0]) ) zIn++;
  zOut = sqlite3_mprintf("%s", zIn);
  if( zOut==0 ) return 0;
  i = (int)strlen(zOut);
#if 0  /* The parser will never leave spaces at the end */
  while( i>0 && isspace(zOut[i-1]) ){ i--; }
#endif
  zOut[i] = 0;
  c = zOut[0];
  if( c=='\'' || c=='"' ){
    for(i=1, j=0; ALWAYS(zOut[i]); i++){
      zOut[j++] = zOut[i];
      if( zOut[i]==c ){
        if( zOut[i+1]==c ){
          i++;
        }else{
          zOut[j-1] = 0;
          break;
        }
      }
    }
  }
  return zOut;
}


/*
** xConnect/xCreate method for the spellfix1 module. Arguments are:
**
**   argv[0]   -> module name  ("spellfix1")
**   argv[1]   -> database name
**   argv[2]   -> table name
**   argv[3].. -> optional arguments (i.e. "edit_cost_table" parameter)
*/
static int spellfix1Init(
  int isCreate,
  sqlite3 *db,
  void *pAux,
  int argc, const char *const*argv,
  sqlite3_vtab **ppVTab,
  char **pzErr
){
  spellfix1_vtab *pNew = 0;
  /* const char *zModule = argv[0]; // not used */
  const char *zDbName = argv[1];
  const char *zTableName = argv[2];
  int nDbName;
  int rc = SQLITE_OK;
  int i;

  nDbName = (int)strlen(zDbName);
  pNew = sqlite3_malloc64( sizeof(*pNew) + nDbName + 1);
  if( pNew==0 ){
    rc = SQLITE_NOMEM;
  }else{
    memset(pNew, 0, sizeof(*pNew));
    pNew->zDbName = (char*)&pNew[1];
    memcpy(pNew->zDbName, zDbName, nDbName+1);
    pNew->zTableName = sqlite3_mprintf("%s", zTableName);
    pNew->db = db;
    if( pNew->zTableName==0 ){
      rc = SQLITE_NOMEM;
    }else{
      rc = sqlite3_declare_vtab(db, 
           "CREATE TABLE x(word,rank,distance,langid, "
           "score, matchlen, phonehash HIDDEN, "
           "top HIDDEN, scope HIDDEN, srchcnt HIDDEN, "
           "soundslike HIDDEN, command HIDDEN)"
      );
#define SPELLFIX_COL_WORD            0
#define SPELLFIX_COL_RANK            1
#define SPELLFIX_COL_DISTANCE        2
#define SPELLFIX_COL_LANGID          3
#define SPELLFIX_COL_SCORE           4
#define SPELLFIX_COL_MATCHLEN        5
#define SPELLFIX_COL_PHONEHASH       6
#define SPELLFIX_COL_TOP             7
#define SPELLFIX_COL_SCOPE           8
#define SPELLFIX_COL_SRCHCNT         9
#define SPELLFIX_COL_SOUNDSLIKE     10
#define SPELLFIX_COL_COMMAND        11
    }
    if( rc==SQLITE_OK && isCreate ){
      spellfix1DbExec(&rc, db,
         "CREATE TABLE IF NOT EXISTS \"%w\".\"%w_vocab\"(\n"
         "  id INTEGER PRIMARY KEY,\n"
         "  rank INT,\n"
         "  langid INT,\n"
         "  word TEXT,\n"
         "  k1 TEXT,\n"
         "  k2 TEXT\n"
         ");\n",
         zDbName, zTableName
      );
      spellfix1DbExec(&rc, db,
         "CREATE INDEX IF NOT EXISTS \"%w\".\"%w_vocab_index_langid_k2\" "
            "ON \"%w_vocab\"(langid,k2);",
         zDbName, zTableName, zTableName
      );
    }
    for(i=3; rc==SQLITE_OK && i<argc; i++){
      if( strncmp(argv[i],"edit_cost_table=",16)==0 && pNew->zCostTable==0 ){
        pNew->zCostTable = spellfix1Dequote(&argv[i][16]);
        if( pNew->zCostTable==0 ) rc = SQLITE_NOMEM;
        continue;
      }
      *pzErr = sqlite3_mprintf("bad argument to spellfix1(): \"%s\"", argv[i]);
      rc = SQLITE_ERROR; 
    }
  }

  if( rc && pNew ){
    *ppVTab = 0;
    spellfix1Uninit(0, &pNew->base);
  }else{
    *ppVTab = (sqlite3_vtab *)pNew;
  }
  return rc;
}

/*
** The xConnect and xCreate methods
*/
static int spellfix1Connect(
  sqlite3 *db,
  void *pAux,
  int argc, const char *const*argv,
  sqlite3_vtab **ppVTab,
  char **pzErr
){
  return spellfix1Init(0, db, pAux, argc, argv, ppVTab, pzErr);
}
static int spellfix1Create(
  sqlite3 *db,
  void *pAux,
  int argc, const char *const*argv,
  sqlite3_vtab **ppVTab,
  char **pzErr
){
  return spellfix1Init(1, db, pAux, argc, argv, ppVTab, pzErr);
}

/*
** Clear all of the content from a cursor.
*/
static void spellfix1ResetCursor(spellfix1_cursor *pCur){
  int i;
  for(i=0; i<pCur->nRow; i++){
    sqlite3_free(pCur->a[i].zWord);
  }
  pCur->nRow = 0;
  pCur->iRow = 0;
  pCur->nSearch = 0;
  if( pCur->pFullScan ){
    sqlite3_finalize(pCur->pFullScan);
    pCur->pFullScan = 0;
  }
}

/*
** Resize the cursor to hold up to N rows of content
*/
static void spellfix1ResizeCursor(spellfix1_cursor *pCur, int N){
  struct spellfix1_row *aNew;
  assert( N>=pCur->nRow );
  aNew = sqlite3_realloc64(pCur->a, sizeof(pCur->a[0])*N);
  if( aNew==0 && N>0 ){
    spellfix1ResetCursor(pCur);
    sqlite3_free(pCur->a);
    pCur->nAlloc = 0;
    pCur->a = 0;
  }else{
    pCur->nAlloc = N;
    pCur->a = aNew;
  }
}


/*
** Close a fuzzy-search cursor.
*/
static int spellfix1Close(sqlite3_vtab_cursor *cur){
  spellfix1_cursor *pCur = (spellfix1_cursor *)cur;
  spellfix1ResetCursor(pCur);
  spellfix1ResizeCursor(pCur, 0);
  sqlite3_free(pCur->zPattern);
  sqlite3_free(pCur);
  return SQLITE_OK;
}

#define SPELLFIX_IDXNUM_MATCH  0x01         /* word MATCH $str */
#define SPELLFIX_IDXNUM_LANGID 0x02         /* langid == $langid */
#define SPELLFIX_IDXNUM_TOP    0x04         /* top = $top */
#define SPELLFIX_IDXNUM_SCOPE  0x08         /* scope = $scope */
#define SPELLFIX_IDXNUM_DISTLT 0x10         /* distance < $distance */
#define SPELLFIX_IDXNUM_DISTLE 0x20         /* distance <= $distance */
#define SPELLFIX_IDXNUM_ROWID  0x40         /* rowid = $rowid */
#define SPELLFIX_IDXNUM_DIST   (0x10|0x20)  /* DISTLT and DISTLE */

/*
**
** The plan number is a bitmask of the SPELLFIX_IDXNUM_* values defined
** above.
**
** filter.argv[*] values contains $str, $langid, $top, $scope and $rowid
** if specified and in that order.
*/
static int spellfix1BestIndex(sqlite3_vtab *tab, sqlite3_index_info *pIdxInfo){
  int iPlan = 0;
  int iLangTerm = -1;
  int iTopTerm = -1;
  int iScopeTerm = -1;
  int iDistTerm = -1;
  int iRowidTerm = -1;
  int i;
  const struct sqlite3_index_constraint *pConstraint;
  pConstraint = pIdxInfo->aConstraint;
  for(i=0; i<pIdxInfo->nConstraint; i++, pConstraint++){
    if( pConstraint->usable==0 ) continue;

    /* Terms of the form:  word MATCH $str */
    if( (iPlan & SPELLFIX_IDXNUM_MATCH)==0 
     && pConstraint->iColumn==SPELLFIX_COL_WORD
     && pConstraint->op==SQLITE_INDEX_CONSTRAINT_MATCH
    ){
      iPlan |= SPELLFIX_IDXNUM_MATCH;
      pIdxInfo->aConstraintUsage[i].argvIndex = 1;
      pIdxInfo->aConstraintUsage[i].omit = 1;
    }

    /* Terms of the form:  langid = $langid  */
    if( (iPlan & SPELLFIX_IDXNUM_LANGID)==0
     && pConstraint->iColumn==SPELLFIX_COL_LANGID
     && pConstraint->op==SQLITE_INDEX_CONSTRAINT_EQ
    ){
      iPlan |= SPELLFIX_IDXNUM_LANGID;
      iLangTerm = i;
    }

    /* Terms of the form:  top = $top */
    if( (iPlan & SPELLFIX_IDXNUM_TOP)==0
     && pConstraint->iColumn==SPELLFIX_COL_TOP
     && pConstraint->op==SQLITE_INDEX_CONSTRAINT_EQ
    ){
      iPlan |= SPELLFIX_IDXNUM_TOP;
      iTopTerm = i;
    }

    /* Terms of the form:  scope = $scope */
    if( (iPlan & SPELLFIX_IDXNUM_SCOPE)==0
     && pConstraint->iColumn==SPELLFIX_COL_SCOPE
     && pConstraint->op==SQLITE_INDEX_CONSTRAINT_EQ
    ){
      iPlan |= SPELLFIX_IDXNUM_SCOPE;
      iScopeTerm = i;
    }

    /* Terms of the form:  distance < $dist or distance <= $dist */
    if( (iPlan & SPELLFIX_IDXNUM_DIST)==0
     && pConstraint->iColumn==SPELLFIX_COL_DISTANCE
     && (pConstraint->op==SQLITE_INDEX_CONSTRAINT_LT
          || pConstraint->op==SQLITE_INDEX_CONSTRAINT_LE)
    ){
      if( pConstraint->op==SQLITE_INDEX_CONSTRAINT_LT ){
        iPlan |= SPELLFIX_IDXNUM_DISTLT;
      }else{
        iPlan |= SPELLFIX_IDXNUM_DISTLE;
      }
      iDistTerm = i;
    }

    /* Terms of the form:  distance < $dist or distance <= $dist */
    if( (iPlan & SPELLFIX_IDXNUM_ROWID)==0
     && pConstraint->iColumn<0
     && pConstraint->op==SQLITE_INDEX_CONSTRAINT_EQ
    ){
      iPlan |= SPELLFIX_IDXNUM_ROWID;
      iRowidTerm = i;
    }
  }
  if( iPlan&SPELLFIX_IDXNUM_MATCH ){
    int idx = 2;
    pIdxInfo->idxNum = iPlan;
    if( pIdxInfo->nOrderBy==1
     && pIdxInfo->aOrderBy[0].iColumn==SPELLFIX_COL_SCORE
     && pIdxInfo->aOrderBy[0].desc==0
    ){
      pIdxInfo->orderByConsumed = 1;  /* Default order by iScore */
    }
    if( iPlan&SPELLFIX_IDXNUM_LANGID ){
      pIdxInfo->aConstraintUsage[iLangTerm].argvIndex = idx++;
      pIdxInfo->aConstraintUsage[iLangTerm].omit = 1;
    }
    if( iPlan&SPELLFIX_IDXNUM_TOP ){
      pIdxInfo->aConstraintUsage[iTopTerm].argvIndex = idx++;
      pIdxInfo->aConstraintUsage[iTopTerm].omit = 1;
    }
    if( iPlan&SPELLFIX_IDXNUM_SCOPE ){
      pIdxInfo->aConstraintUsage[iScopeTerm].argvIndex = idx++;
      pIdxInfo->aConstraintUsage[iScopeTerm].omit = 1;
    }
    if( iPlan&SPELLFIX_IDXNUM_DIST ){
      pIdxInfo->aConstraintUsage[iDistTerm].argvIndex = idx++;
      pIdxInfo->aConstraintUsage[iDistTerm].omit = 1;
    }
    pIdxInfo->estimatedCost = 1e5;
  }else if( (iPlan & SPELLFIX_IDXNUM_ROWID) ){
    pIdxInfo->idxNum = SPELLFIX_IDXNUM_ROWID;
    pIdxInfo->aConstraintUsage[iRowidTerm].argvIndex = 1;
    pIdxInfo->aConstraintUsage[iRowidTerm].omit = 1;
    pIdxInfo->estimatedCost = 5;
  }else{
    pIdxInfo->idxNum = 0;
    pIdxInfo->estimatedCost = 1e50;
  }
  return SQLITE_OK;
}

/*
** Open a new fuzzy-search cursor.
*/
static int spellfix1Open(sqlite3_vtab *pVTab, sqlite3_vtab_cursor **ppCursor){
  spellfix1_vtab *p = (spellfix1_vtab*)pVTab;
  spellfix1_cursor *pCur;
  pCur = sqlite3_malloc64( sizeof(*pCur) );
  if( pCur==0 ) return SQLITE_NOMEM;
  memset(pCur, 0, sizeof(*pCur));
  pCur->pVTab = p;
  *ppCursor = &pCur->base;
  return SQLITE_OK;
}

/*
** Adjust a distance measurement by the words rank in order to show
** preference to common words.
*/
static int spellfix1Score(int iDistance, int iRank){
  int iLog2;
  for(iLog2=0; iRank>0; iLog2++, iRank>>=1){}
  return iDistance + 32 - iLog2;
}

/*
** Compare two spellfix1_row objects for sorting purposes in qsort() such
** that they sort in order of increasing distance.
*/
static int SQLITE_CDECL spellfix1RowCompare(const void *A, const void *B){
  const struct spellfix1_row *a = (const struct spellfix1_row*)A;
  const struct spellfix1_row *b = (const struct spellfix1_row*)B;
  return a->iScore - b->iScore;
}

/*
** A structure used to pass information from spellfix1FilterForMatch()
** into spellfix1RunQuery().
*/
typedef struct MatchQuery {
  spellfix1_cursor *pCur;          /* The cursor being queried */
  sqlite3_stmt *pStmt;             /* shadow table query statment */
  char zHash[SPELLFIX_MX_HASH];    /* The current phonehash for zPattern */
  const char *zPattern;            /* Transliterated input string */
  int nPattern;                    /* Length of zPattern */
  EditDist3FromString *pMatchStr3; /* Original unicode string */
  EditDist3Config *pConfig3;       /* Edit-distance cost coefficients */
  const EditDist3Lang *pLang;      /* The selected language coefficients */
  int iLang;                       /* The language id */
  int iScope;                      /* Default scope */
  int iMaxDist;                    /* Maximum allowed edit distance, or -1 */
  int rc;                          /* Error code */
  int nRun;                  /* Number of prior runs for the same zPattern */
  char azPrior[SPELLFIX_MX_RUN][SPELLFIX_MX_HASH];  /* Prior hashes */
} MatchQuery;

/*
** Run a query looking for the best matches against zPattern using
** zHash as the character class seed hash.
*/
static void spellfix1RunQuery(MatchQuery *p, const char *zQuery, int nQuery){
  const char *zK1;
  const char *zWord;
  int iDist;
  int iRank;
  int iScore;
  int iWorst = 0;
  int idx;
  int idxWorst = -1;
  int i;
  int iScope = p->iScope;
  spellfix1_cursor *pCur = p->pCur;
  sqlite3_stmt *pStmt = p->pStmt;
  char zHash1[SPELLFIX_MX_HASH];
  char zHash2[SPELLFIX_MX_HASH];
  char *zClass;
  int nClass;
  int rc;

  if( pCur->a==0 || p->rc ) return;   /* Prior memory allocation failure */
  zClass = (char*)phoneticHash((unsigned char*)zQuery, nQuery);
  if( zClass==0 ){
    p->rc = SQLITE_NOMEM;
    return;
  }
  nClass = (int)strlen(zClass);
  if( nClass>SPELLFIX_MX_HASH-2 ){
    nClass = SPELLFIX_MX_HASH-2;
    zClass[nClass] = 0;
  }
  if( nClass<=iScope ){
    if( nClass>2 ){
      iScope = nClass-1;
    }else{
      iScope = nClass;
    }
  }
  memcpy(zHash1, zClass, iScope);
  sqlite3_free(zClass);
  zHash1[iScope] = 0;
  memcpy(zHash2, zHash1, iScope);
  zHash2[iScope] = 'Z';
  zHash2[iScope+1] = 0;
#if SPELLFIX_MX_RUN>1
  for(i=0; i<p->nRun; i++){
    if( strcmp(p->azPrior[i], zHash1)==0 ) return;
  }
#endif
  assert( p->nRun<SPELLFIX_MX_RUN );
  memcpy(p->azPrior[p->nRun++], zHash1, iScope+1);
  if( sqlite3_bind_text(pStmt, 1, zHash1, -1, SQLITE_STATIC)==SQLITE_NOMEM
   || sqlite3_bind_text(pStmt, 2, zHash2, -1, SQLITE_STATIC)==SQLITE_NOMEM
  ){
    p->rc = SQLITE_NOMEM;
    return;
  }
#if SPELLFIX_MX_RUN>1
  for(i=0; i<pCur->nRow; i++){
    if( pCur->a[i].iScore>iWorst ){
      iWorst = pCur->a[i].iScore;
      idxWorst = i;
    }
  }
#endif
  while( sqlite3_step(pStmt)==SQLITE_ROW ){
    int iMatchlen = -1;
    iRank = sqlite3_column_int(pStmt, 2);
    if( p->pMatchStr3 ){
      int nWord = sqlite3_column_bytes(pStmt, 1);
      zWord = (const char*)sqlite3_column_text(pStmt, 1);
      iDist = editDist3Core(p->pMatchStr3, zWord, nWord, p->pLang, &iMatchlen);
    }else{
      zK1 = (const char*)sqlite3_column_text(pStmt, 3);
      if( zK1==0 ) continue;
      iDist = editdist1(p->zPattern, zK1, 0);
    }
    if( iDist<0 ){
      p->rc = SQLITE_NOMEM;
      break;
    }
    pCur->nSearch++;
    
    /* If there is a "distance < $dist" or "distance <= $dist" constraint,
    ** check if this row meets it. If not, jump back up to the top of the
    ** loop to process the next row. Otherwise, if the row does match the
    ** distance constraint, check if the pCur->a[] array is already full.
    ** If it is and no explicit "top = ?" constraint was present in the
    ** query, grow the array to ensure there is room for the new entry. */
    assert( (p->iMaxDist>=0)==((pCur->idxNum & SPELLFIX_IDXNUM_DIST) ? 1 : 0) );
    if( p->iMaxDist>=0 ){
      if( iDist>p->iMaxDist ) continue;
      if( pCur->nRow>=pCur->nAlloc && (pCur->idxNum & SPELLFIX_IDXNUM_TOP)==0 ){
        spellfix1ResizeCursor(pCur, pCur->nAlloc*2 + 10);
        if( pCur->a==0 ) break;
      }
    }

    iScore = spellfix1Score(iDist,iRank);
    if( pCur->nRow<pCur->nAlloc ){
      idx = pCur->nRow;
    }else if( iScore<iWorst ){
      idx = idxWorst;
      sqlite3_free(pCur->a[idx].zWord);
    }else{
      continue;
    }

    pCur->a[idx].zWord = sqlite3_mprintf("%s", sqlite3_column_text(pStmt, 1));
    if( pCur->a[idx].zWord==0 ){
      p->rc = SQLITE_NOMEM;
      break;
    }
    pCur->a[idx].iRowid = sqlite3_column_int64(pStmt, 0);
    pCur->a[idx].iRank = iRank;
    pCur->a[idx].iDistance = iDist;
    pCur->a[idx].iScore = iScore;
    pCur->a[idx].iMatchlen = iMatchlen;
    memcpy(pCur->a[idx].zHash, zHash1, iScope+1);
    if( pCur->nRow<pCur->nAlloc ) pCur->nRow++;
    if( pCur->nRow==pCur->nAlloc ){
      iWorst = pCur->a[0].iScore;
      idxWorst = 0;
      for(i=1; i<pCur->nRow; i++){
        iScore = pCur->a[i].iScore;
        if( iWorst<iScore ){
          iWorst = iScore;
          idxWorst = i;
        }
      }
    }
  }
  rc = sqlite3_reset(pStmt);
  if( rc ) p->rc = rc;
}

/*
** This version of the xFilter method work if the MATCH term is present
** and we are doing a scan.
*/
static int spellfix1FilterForMatch(
  spellfix1_cursor *pCur,
  int argc,
  sqlite3_value **argv
){
  int idxNum = pCur->idxNum;
  const unsigned char *zMatchThis;   /* RHS of the MATCH operator */
  EditDist3FromString *pMatchStr3 = 0; /* zMatchThis as an editdist string */
  char *zPattern;                    /* Transliteration of zMatchThis */
  int nPattern;                      /* Length of zPattern */
  int iLimit = 20;                   /* Max number of rows of output */
  int iScope = 3;                    /* Use this many characters of zClass */
  int iLang = 0;                     /* Language code */
  char *zSql;                        /* SQL of shadow table query */
  sqlite3_stmt *pStmt = 0;           /* Shadow table query */
  int rc;                            /* Result code */
  int idx = 1;                       /* Next available filter parameter */
  spellfix1_vtab *p = pCur->pVTab;   /* The virtual table that owns pCur */
  MatchQuery x;                      /* For passing info to RunQuery() */

  /* Load the cost table if we have not already done so */
  if( p->zCostTable!=0 && p->pConfig3==0 ){
    p->pConfig3 = sqlite3_malloc64( sizeof(p->pConfig3[0]) );
    if( p->pConfig3==0 ) return SQLITE_NOMEM;
    memset(p->pConfig3, 0, sizeof(p->pConfig3[0]));
    rc = editDist3ConfigLoad(p->pConfig3, p->db, p->zCostTable);
    if( rc ) return rc;
  }
  memset(&x, 0, sizeof(x));
  x.iScope = 3;  /* Default scope if none specified by "WHERE scope=N" */
  x.iMaxDist = -1;   /* Maximum allowed edit distance */

  if( idxNum&2 ){
    iLang = sqlite3_value_int(argv[idx++]);
  }
  if( idxNum&4 ){
    iLimit = sqlite3_value_int(argv[idx++]);
    if( iLimit<1 ) iLimit = 1;
  }
  if( idxNum&8 ){
    x.iScope = sqlite3_value_int(argv[idx++]);
    if( x.iScope<1 ) x.iScope = 1;
    if( x.iScope>SPELLFIX_MX_HASH-2 ) x.iScope = SPELLFIX_MX_HASH-2;
  }
  if( idxNum&(16|32) ){
    x.iMaxDist = sqlite3_value_int(argv[idx++]);
    if( idxNum&16 ) x.iMaxDist--;
    if( x.iMaxDist<0 ) x.iMaxDist = 0;
  }
  spellfix1ResetCursor(pCur);
  spellfix1ResizeCursor(pCur, iLimit);
  zMatchThis = sqlite3_value_text(argv[0]);
  if( zMatchThis==0 ) return SQLITE_OK;
  if( p->pConfig3 ){
    x.pLang = editDist3FindLang(p->pConfig3, iLang);
    pMatchStr3 = editDist3FromStringNew(x.pLang, (const char*)zMatchThis, -1);
    if( pMatchStr3==0 ){
      x.rc = SQLITE_NOMEM;
      goto filter_exit;
    }
  }else{
    x.pLang = 0;
  }
  zPattern = (char*)transliterate(zMatchThis, sqlite3_value_bytes(argv[0]));
  sqlite3_free(pCur->zPattern);
  pCur->zPattern = zPattern;
  if( zPattern==0 ){
    x.rc = SQLITE_NOMEM;
    goto filter_exit;
  }
  nPattern = (int)strlen(zPattern);
  if( zPattern[nPattern-1]=='*' ) nPattern--;
  zSql = sqlite3_mprintf(
     "SELECT id, word, rank, coalesce(k1,word)"
     "  FROM \"%w\".\"%w_vocab\""
     " WHERE langid=%d AND k2>=?1 AND k2<?2",
     p->zDbName, p->zTableName, iLang
  );
  if( zSql==0 ){
    x.rc = SQLITE_NOMEM;
    pStmt = 0;
    goto filter_exit;
  }
  rc = sqlite3_prepare_v2(p->db, zSql, -1, &pStmt, 0);
  sqlite3_free(zSql);
  pCur->iLang = iLang;
  x.pCur = pCur;
  x.pStmt = pStmt;
  x.zPattern = zPattern;
  x.nPattern = nPattern;
  x.pMatchStr3 = pMatchStr3;
  x.iLang = iLang;
  x.rc = rc;
  x.pConfig3 = p->pConfig3;
  if( x.rc==SQLITE_OK ){
    spellfix1RunQuery(&x, zPattern, nPattern);
  }

  if( pCur->a ){
    qsort(pCur->a, pCur->nRow, sizeof(pCur->a[0]), spellfix1RowCompare);
    pCur->iTop = iLimit;
    pCur->iScope = iScope;
  }else{
    x.rc = SQLITE_NOMEM;
  }

filter_exit:
  sqlite3_finalize(pStmt);
  editDist3FromStringDelete(pMatchStr3);
  return x.rc;
}

/*
** This version of xFilter handles a full-table scan case
*/
static int spellfix1FilterForFullScan(
  spellfix1_cursor *pCur,
  int argc,
  sqlite3_value **argv
){
  int rc = SQLITE_OK;
  int idxNum = pCur->idxNum;
  char *zSql;
  spellfix1_vtab *pVTab = pCur->pVTab;
  spellfix1ResetCursor(pCur);
  assert( idxNum==0 || idxNum==64 );
  zSql = sqlite3_mprintf(
     "SELECT word, rank, NULL, langid, id FROM \"%w\".\"%w_vocab\"%s",
     pVTab->zDbName, pVTab->zTableName,
     ((idxNum & 64) ? " WHERE rowid=?" : "")
  );
  if( zSql==0 ) return SQLITE_NOMEM;
  rc = sqlite3_prepare_v2(pVTab->db, zSql, -1, &pCur->pFullScan, 0);
  sqlite3_free(zSql);
  if( rc==SQLITE_OK && (idxNum & 64) ){
    assert( argc==1 );
    rc = sqlite3_bind_value(pCur->pFullScan, 1, argv[0]);
  }
  pCur->nRow = pCur->iRow = 0;
  if( rc==SQLITE_OK ){
    rc = sqlite3_step(pCur->pFullScan);
    if( rc==SQLITE_ROW ){ pCur->iRow = -1; rc = SQLITE_OK; }
    if( rc==SQLITE_DONE ){ rc = SQLITE_OK; }
  }else{
    pCur->iRow = 0;
  }
  return rc;
}


/*
** Called to "rewind" a cursor back to the beginning so that
** it starts its output over again.  Always called at least once
** prior to any spellfix1Column, spellfix1Rowid, or spellfix1Eof call.
*/
static int spellfix1Filter(
  sqlite3_vtab_cursor *cur, 
  int idxNum, const char *idxStr,
  int argc, sqlite3_value **argv
){
  spellfix1_cursor *pCur = (spellfix1_cursor *)cur;
  int rc;
  pCur->idxNum = idxNum;
  if( idxNum & 1 ){
    rc = spellfix1FilterForMatch(pCur, argc, argv);
  }else{
    rc = spellfix1FilterForFullScan(pCur, argc, argv);
  }
  return rc;
}


/*
** Advance a cursor to its next row of output
*/
static int spellfix1Next(sqlite3_vtab_cursor *cur){
  spellfix1_cursor *pCur = (spellfix1_cursor *)cur;
  int rc = SQLITE_OK;
  if( pCur->iRow < pCur->nRow ){
    if( pCur->pFullScan ){
      rc = sqlite3_step(pCur->pFullScan);
      if( rc!=SQLITE_ROW ) pCur->iRow = pCur->nRow;
      if( rc==SQLITE_ROW || rc==SQLITE_DONE ) rc = SQLITE_OK;
    }else{
      pCur->iRow++;
    }
  }
  return rc;
}

/*
** Return TRUE if we are at the end-of-file
*/
static int spellfix1Eof(sqlite3_vtab_cursor *cur){
  spellfix1_cursor *pCur = (spellfix1_cursor *)cur;
  return pCur->iRow>=pCur->nRow;
}

/*
** Return columns from the current row.
*/
static int spellfix1Column(
  sqlite3_vtab_cursor *cur,
  sqlite3_context *ctx,
  int i
){
  spellfix1_cursor *pCur = (spellfix1_cursor*)cur;
  if( pCur->pFullScan ){
    if( i<=SPELLFIX_COL_LANGID ){
      sqlite3_result_value(ctx, sqlite3_column_value(pCur->pFullScan, i));
    }else{
      sqlite3_result_null(ctx);
    }
    return SQLITE_OK;
  }
  switch( i ){
    case SPELLFIX_COL_WORD: {
      sqlite3_result_text(ctx, pCur->a[pCur->iRow].zWord, -1, SQLITE_STATIC);
      break;
    }
    case SPELLFIX_COL_RANK: {
      sqlite3_result_int(ctx, pCur->a[pCur->iRow].iRank);
      break;
    }
    case SPELLFIX_COL_DISTANCE: {
      sqlite3_result_int(ctx, pCur->a[pCur->iRow].iDistance);
      break;
    }
    case SPELLFIX_COL_LANGID: {
      sqlite3_result_int(ctx, pCur->iLang);
      break;
    }
    case SPELLFIX_COL_SCORE: {
      sqlite3_result_int(ctx, pCur->a[pCur->iRow].iScore);
      break;
    }
    case SPELLFIX_COL_MATCHLEN: {
      int iMatchlen = pCur->a[pCur->iRow].iMatchlen;
      if( iMatchlen<0 ){
        int nPattern = (int)strlen(pCur->zPattern);
        char *zWord = pCur->a[pCur->iRow].zWord;
        int nWord = (int)strlen(zWord);

        if( nPattern>0 && pCur->zPattern[nPattern-1]=='*' ){
          char *zTranslit;
          int res;
          zTranslit = (char *)transliterate((unsigned char *)zWord, nWord);
          if( !zTranslit ) return SQLITE_NOMEM;
          res = editdist1(pCur->zPattern, zTranslit, &iMatchlen);
          sqlite3_free(zTranslit);
          if( res<0 ) return SQLITE_NOMEM;
          iMatchlen = translen_to_charlen(zWord, nWord, iMatchlen);
        }else{
          iMatchlen = utf8Charlen(zWord, nWord);
        }
      }

      sqlite3_result_int(ctx, iMatchlen);
      break;
    }
    case SPELLFIX_COL_PHONEHASH: {
      sqlite3_result_text(ctx, pCur->a[pCur->iRow].zHash, -1, SQLITE_STATIC);
      break;
    }
    case SPELLFIX_COL_TOP: {
      sqlite3_result_int(ctx, pCur->iTop);
      break;
    }
    case SPELLFIX_COL_SCOPE: {
      sqlite3_result_int(ctx, pCur->iScope);
      break;
    }
    case SPELLFIX_COL_SRCHCNT: {
      sqlite3_result_int(ctx, pCur->nSearch);
      break;
    }
    default: {
      sqlite3_result_null(ctx);
      break;
    }
  }
  return SQLITE_OK;
}

/*
** The rowid.
*/
static int spellfix1Rowid(sqlite3_vtab_cursor *cur, sqlite_int64 *pRowid){
  spellfix1_cursor *pCur = (spellfix1_cursor*)cur;
  if( pCur->pFullScan ){
    *pRowid = sqlite3_column_int64(pCur->pFullScan, 4);
  }else{
    *pRowid = pCur->a[pCur->iRow].iRowid;
  }
  return SQLITE_OK;
}

/*
** This function is called by the xUpdate() method. It returns a string
** containing the conflict mode that xUpdate() should use for the current
** operation. One of: "ROLLBACK", "IGNORE", "ABORT" or "REPLACE".
*/
static const char *spellfix1GetConflict(sqlite3 *db){
  static const char *azConflict[] = {
    /* Note: Instead of "FAIL" - "ABORT". */
    "ROLLBACK", "IGNORE", "ABORT", "ABORT", "REPLACE"
  };
  int eConflict = sqlite3_vtab_on_conflict(db);

  assert( eConflict==SQLITE_ROLLBACK || eConflict==SQLITE_IGNORE
       || eConflict==SQLITE_FAIL || eConflict==SQLITE_ABORT
       || eConflict==SQLITE_REPLACE
  );
  assert( SQLITE_ROLLBACK==1 );
  assert( SQLITE_IGNORE==2 );
  assert( SQLITE_FAIL==3 );
  assert( SQLITE_ABORT==4 );
  assert( SQLITE_REPLACE==5 );

  return azConflict[eConflict-1];
}

/*
** The xUpdate() method.
*/
static int spellfix1Update(
  sqlite3_vtab *pVTab,
  int argc,
  sqlite3_value **argv,
  sqlite_int64 *pRowid
){
  int rc = SQLITE_OK;
  sqlite3_int64 rowid, newRowid;
  spellfix1_vtab *p = (spellfix1_vtab*)pVTab;
  sqlite3 *db = p->db;

  if( argc==1 ){
    /* A delete operation on the rowid given by argv[0] */
    rowid = *pRowid = sqlite3_value_int64(argv[0]);
    spellfix1DbExec(&rc, db, "DELETE FROM \"%w\".\"%w_vocab\" "
                           " WHERE id=%lld",
                  p->zDbName, p->zTableName, rowid);
  }else{
    const unsigned char *zWord = sqlite3_value_text(argv[SPELLFIX_COL_WORD+2]);
    int nWord = sqlite3_value_bytes(argv[SPELLFIX_COL_WORD+2]);
    int iLang = sqlite3_value_int(argv[SPELLFIX_COL_LANGID+2]);
    int iRank = sqlite3_value_int(argv[SPELLFIX_COL_RANK+2]);
    const unsigned char *zSoundslike =
           sqlite3_value_text(argv[SPELLFIX_COL_SOUNDSLIKE+2]);
    int nSoundslike = sqlite3_value_bytes(argv[SPELLFIX_COL_SOUNDSLIKE+2]);
    char *zK1, *zK2;
    int i;
    char c;
    const char *zConflict = spellfix1GetConflict(db);

    if( zWord==0 ){
      /* Inserts of the form:  INSERT INTO table(command) VALUES('xyzzy');
      ** cause zWord to be NULL, so we look at the "command" column to see
      ** what special actions to take */
      const char *zCmd = 
         (const char*)sqlite3_value_text(argv[SPELLFIX_COL_COMMAND+2]);
      if( zCmd==0 ){
        pVTab->zErrMsg = sqlite3_mprintf("NOT NULL constraint failed: %s.word",
                                         p->zTableName);
        return SQLITE_CONSTRAINT_NOTNULL;
      }
      if( strcmp(zCmd,"reset")==0 ){
        /* Reset the  edit cost table (if there is one). */
        editDist3ConfigDelete(p->pConfig3);
        p->pConfig3 = 0;
        return SQLITE_OK;
      }
      if( strncmp(zCmd,"edit_cost_table=",16)==0 ){
        editDist3ConfigDelete(p->pConfig3);
        p->pConfig3 = 0;
        sqlite3_free(p->zCostTable);
        p->zCostTable = spellfix1Dequote(zCmd+16);
        if( p->zCostTable==0 ) return SQLITE_NOMEM;
        if( p->zCostTable[0]==0 || sqlite3_stricmp(p->zCostTable,"null")==0 ){
          sqlite3_free(p->zCostTable);
          p->zCostTable = 0;
        }
        return SQLITE_OK;
      }
      pVTab->zErrMsg = sqlite3_mprintf("unknown value for %s.command: \"%w\"",
                                       p->zTableName, zCmd);
      return SQLITE_ERROR;
    }
    if( iRank<1 ) iRank = 1;
    if( zSoundslike ){
      zK1 = (char*)transliterate(zSoundslike, nSoundslike);
    }else{
      zK1 = (char*)transliterate(zWord, nWord);
    }
    if( zK1==0 ) return SQLITE_NOMEM;
    for(i=0; (c = zK1[i])!=0; i++){
       if( c>='A' && c<='Z' ) zK1[i] += 'a' - 'A';
    }
    zK2 = (char*)phoneticHash((const unsigned char*)zK1, i);
    if( zK2==0 ){
      sqlite3_free(zK1);
      return SQLITE_NOMEM;
    }
    if( sqlite3_value_type(argv[0])==SQLITE_NULL ){
      if( sqlite3_value_type(argv[1])==SQLITE_NULL ){
        spellfix1DbExec(&rc, db,
               "INSERT INTO \"%w\".\"%w_vocab\"(rank,langid,word,k1,k2) "
               "VALUES(%d,%d,%Q,nullif(%Q,%Q),%Q)",
               p->zDbName, p->zTableName,
               iRank, iLang, zWord, zK1, zWord, zK2
        );
      }else{
        newRowid = sqlite3_value_int64(argv[1]);
        spellfix1DbExec(&rc, db,
            "INSERT OR %s INTO \"%w\".\"%w_vocab\"(id,rank,langid,word,k1,k2) "
            "VALUES(%lld,%d,%d,%Q,nullif(%Q,%Q),%Q)",
            zConflict, p->zDbName, p->zTableName,
            newRowid, iRank, iLang, zWord, zK1, zWord, zK2
        );
      }
      *pRowid = sqlite3_last_insert_rowid(db);
    }else{
      rowid = sqlite3_value_int64(argv[0]);
      newRowid = *pRowid = sqlite3_value_int64(argv[1]);
      spellfix1DbExec(&rc, db,
             "UPDATE OR %s \"%w\".\"%w_vocab\" SET id=%lld, rank=%d, langid=%d,"
             " word=%Q, k1=nullif(%Q,%Q), k2=%Q WHERE id=%lld",
             zConflict, p->zDbName, p->zTableName, newRowid, iRank, iLang,
             zWord, zK1, zWord, zK2, rowid
      );
    }
    sqlite3_free(zK1);
    sqlite3_free(zK2);
  }
  return rc;
}

/*
** Rename the spellfix1 table.
*/
static int spellfix1Rename(sqlite3_vtab *pVTab, const char *zNew){
  spellfix1_vtab *p = (spellfix1_vtab*)pVTab;
  sqlite3 *db = p->db;
  int rc = SQLITE_OK;
  char *zNewName = sqlite3_mprintf("%s", zNew);
  if( zNewName==0 ){
    return SQLITE_NOMEM;
  }
  spellfix1DbExec(&rc, db, 
     "ALTER TABLE \"%w\".\"%w_vocab\" RENAME TO \"%w_vocab\"",
     p->zDbName, p->zTableName, zNewName
  );
  if( rc==SQLITE_OK ){
    sqlite3_free(p->zTableName);
    p->zTableName = zNewName;
  }else{
    sqlite3_free(zNewName);
  }
  return rc;
}


/*
** A virtual table module that provides fuzzy search.
*/
static sqlite3_module spellfix1Module = {
  0,                       /* iVersion */
  spellfix1Create,         /* xCreate - handle CREATE VIRTUAL TABLE */
  spellfix1Connect,        /* xConnect - reconnected to an existing table */
  spellfix1BestIndex,      /* xBestIndex - figure out how to do a query */
  spellfix1Disconnect,     /* xDisconnect - close a connection */
  spellfix1Destroy,        /* xDestroy - handle DROP TABLE */
  spellfix1Open,           /* xOpen - open a cursor */
  spellfix1Close,          /* xClose - close a cursor */
  spellfix1Filter,         /* xFilter - configure scan constraints */
  spellfix1Next,           /* xNext - advance a cursor */
  spellfix1Eof,            /* xEof - check for end of scan */
  spellfix1Column,         /* xColumn - read data */
  spellfix1Rowid,          /* xRowid - read data */
  spellfix1Update,         /* xUpdate */
  0,                       /* xBegin */
  0,                       /* xSync */
  0,                       /* xCommit */
  0,                       /* xRollback */
  0,                       /* xFindMethod */
  spellfix1Rename,         /* xRename */
};

/*
** Register the various functions and the virtual table.
*/
static int spellfix1Register(sqlite3 *db){
  int rc = SQLITE_OK;
  int i;
  rc = sqlite3_create_function(db, "spellfix1_translit", 1,
                               SQLITE_UTF8|SQLITE_DETERMINISTIC, 0,
                                transliterateSqlFunc, 0, 0);
  if( rc==SQLITE_OK ){
    rc = sqlite3_create_function(db, "spellfix1_editdist", 2,
                                 SQLITE_UTF8|SQLITE_DETERMINISTIC, 0,
                                  editdistSqlFunc, 0, 0);
  }
  if( rc==SQLITE_OK ){
    rc = sqlite3_create_function(db, "spellfix1_phonehash", 1,
                                 SQLITE_UTF8|SQLITE_DETERMINISTIC, 0,
                                  phoneticHashSqlFunc, 0, 0);
  }
  if( rc==SQLITE_OK ){
    rc = sqlite3_create_function(db, "spellfix1_scriptcode", 1,
                                  SQLITE_UTF8|SQLITE_DETERMINISTIC, 0,
                                  scriptCodeSqlFunc, 0, 0);
  }
  if( rc==SQLITE_OK ){
    rc = sqlite3_create_module(db, "spellfix1", &spellfix1Module, 0);
  }
  if( rc==SQLITE_OK ){
    rc = editDist3Install(db);
  }

  /* Verify sanity of the translit[] table */
  for(i=0; i<sizeof(translit)/sizeof(translit[0])-1; i++){
    assert( translit[i].cFrom<translit[i+1].cFrom );
  }

  return rc;
}

#endif /* SQLITE_OMIT_VIRTUALTABLE */

/*
** Extension load function.
*/
#ifdef _WIN32
__declspec(dllexport)
#endif
int sqlite3_spellfix_init(
  sqlite3 *db, 
  char **pzErrMsg, 
  const sqlite3_api_routines *pApi
){
  SQLITE_EXTENSION_INIT2(pApi);
#ifndef SQLITE_OMIT_VIRTUALTABLE
  return spellfix1Register(db);
#endif
  return SQLITE_OK;
}



/*
** This function registered all of the above C functions as SQL
** functions.  This should be the only routine in this file with
** external linkage.
*/
int RegisterExtensionFunctions(sqlite3 *db){
  static const struct FuncDef {
     char *zName;
     signed char nArg;
     u8 argType;           /* 0: none.  1: db  2: (-1) */
     u8 eTextRep;          /* 1: UTF-16.  0: UTF-8 */
     u8 needCollSeq;
     void (*xFunc)(sqlite3_context*,int,sqlite3_value **);
  } aFuncs[] = {
    /* math.h */
    { "acos",               1, 0, SQLITE_UTF8,    0, acosFunc  },
    { "asin",               1, 0, SQLITE_UTF8,    0, asinFunc  },
    { "atan",               1, 0, SQLITE_UTF8,    0, atanFunc  },
    { "atn2",               2, 0, SQLITE_UTF8,    0, atn2Func  },
    /* XXX alias */
    { "atan2",              2, 0, SQLITE_UTF8,    0, atn2Func  },
    { "acosh",              1, 0, SQLITE_UTF8,    0, acoshFunc  },
    { "asinh",              1, 0, SQLITE_UTF8,    0, asinhFunc  },
    { "atanh",              1, 0, SQLITE_UTF8,    0, atanhFunc  },

    { "difference",         2, 0, SQLITE_UTF8,    0, differenceFunc},
    { "degrees",            1, 0, SQLITE_UTF8,    0, rad2degFunc  },
    { "radians",            1, 0, SQLITE_UTF8,    0, deg2radFunc  },

    { "cos",                1, 0, SQLITE_UTF8,    0, cosFunc  },
    { "sin",                1, 0, SQLITE_UTF8,    0, sinFunc },
    { "tan",                1, 0, SQLITE_UTF8,    0, tanFunc },
    { "cot",                1, 0, SQLITE_UTF8,    0, cotFunc },
    { "cosh",               1, 0, SQLITE_UTF8,    0, coshFunc  },
    { "sinh",               1, 0, SQLITE_UTF8,    0, sinhFunc },
    { "tanh",               1, 0, SQLITE_UTF8,    0, tanhFunc },
    { "coth",               1, 0, SQLITE_UTF8,    0, cothFunc },

    { "exp",                1, 0, SQLITE_UTF8,    0, expFunc  },
    { "log",                1, 0, SQLITE_UTF8,    0, logFunc  },
    { "log10",              1, 0, SQLITE_UTF8,    0, log10Func  },
    { "power",              2, 0, SQLITE_UTF8,    0, powerFunc  },
    { "sign",               1, 0, SQLITE_UTF8,    0, signFunc },
    { "sqrt",               1, 0, SQLITE_UTF8,    0, sqrtFunc },
    { "square",             1, 0, SQLITE_UTF8,    0, squareFunc },

    { "ceil",               1, 0, SQLITE_UTF8,    0, ceilFunc },
    { "floor",              1, 0, SQLITE_UTF8,    0, floorFunc },

    { "pi",                 0, 0, SQLITE_UTF8,    1, piFunc },


    /* string */
    { "replicate",          2, 0, SQLITE_UTF8,    0, replicateFunc },
    { "charindex",          2, 0, SQLITE_UTF8,    0, charindexFunc },
    { "charindex",          3, 0, SQLITE_UTF8,    0, charindexFunc },
    { "leftstr",            2, 0, SQLITE_UTF8,    0, leftFunc },
    { "rightstr",           2, 0, SQLITE_UTF8,    0, rightFunc },
#ifndef HAVE_TRIM
    { "ltrim",              1, 0, SQLITE_UTF8,    0, ltrimFunc },
    { "rtrim",              1, 0, SQLITE_UTF8,    0, rtrimFunc },
    { "trim",               1, 0, SQLITE_UTF8,    0, trimFunc },
    { "replace",            3, 0, SQLITE_UTF8,    0, replaceFunc },
#endif
    { "reverse",            1, 0, SQLITE_UTF8,    0, reverseFunc },
    { "proper",             1, 0, SQLITE_UTF8,    0, properFunc },
    { "padl",               2, 0, SQLITE_UTF8,    0, padlFunc },
    { "padr",               2, 0, SQLITE_UTF8,    0, padrFunc },
    { "padc",               2, 0, SQLITE_UTF8,    0, padcFunc },
    { "strfilter",          2, 0, SQLITE_UTF8,    0, strfilterFunc },

  };
  /* Aggregate functions */
  static const struct FuncDefAgg {
    char *zName;
    signed char nArg;
    u8 argType;
    u8 needCollSeq;
    void (*xStep)(sqlite3_context*,int,sqlite3_value**);
    void (*xFinalize)(sqlite3_context*);
  } aAggs[] = {
    { "stdev",            1, 0, 0, varianceStep, stdevFinalize  },
    { "variance",         1, 0, 0, varianceStep, varianceFinalize  },
    { "mode",             1, 0, 0, modeStep,     modeFinalize  },
    { "median",           1, 0, 0, modeStep,     medianFinalize  },
    { "lower_quartile",   1, 0, 0, modeStep,     lower_quartileFinalize  },
    { "upper_quartile",   1, 0, 0, modeStep,     upper_quartileFinalize  },
  };
  int i;

  for(i=0; i<sizeof(aFuncs)/sizeof(aFuncs[0]); i++){
    void *pArg = 0;
    switch( aFuncs[i].argType ){
      case 1: pArg = db; break;
      case 2: pArg = (void *)(-1); break;
    }
    //sqlite3CreateFunc
    /* LMH no error checking */
    sqlite3_create_function(db, aFuncs[i].zName, aFuncs[i].nArg,
        aFuncs[i].eTextRep, pArg, aFuncs[i].xFunc, 0, 0);
#if 0
    if( aFuncs[i].needCollSeq ){
      struct FuncDef *pFunc = sqlite3FindFunction(db, aFuncs[i].zName,
          strlen(aFuncs[i].zName), aFuncs[i].nArg, aFuncs[i].eTextRep, 0);
      if( pFunc && aFuncs[i].needCollSeq ){
        pFunc->needCollSeq = 1;
      }
    }
#endif
  }

  for(i=0; i<sizeof(aAggs)/sizeof(aAggs[0]); i++){
    void *pArg = 0;
    switch( aAggs[i].argType ){
      case 1: pArg = db; break;
      case 2: pArg = (void *)(-1); break;
    }
    //sqlite3CreateFunc
    /* LMH no error checking */
    sqlite3_create_function(db, aAggs[i].zName, aAggs[i].nArg, SQLITE_UTF8,
        pArg, 0, aAggs[i].xStep, aAggs[i].xFinalize);
#if 0
    if( aAggs[i].needCollSeq ){
      struct FuncDefAgg *pFunc = sqlite3FindFunction( db, aAggs[i].zName,
          strlen(aAggs[i].zName), aAggs[i].nArg, SQLITE_UTF8, 0);
      if( pFunc && aAggs[i].needCollSeq ){
        pFunc->needCollSeq = 1;
      }
    }
#endif
  }

  /*  assert(SQLITE_OK == spellfix1Register(db));  */
  
  return 0;
}

#ifdef COMPILE_SQLITE_EXTENSIONS_AS_LOADABLE_MODULE
int sqlite3_extension_init(
    sqlite3 *db, char **pzErrMsg, const sqlite3_api_routines *pApi){
  SQLITE_EXTENSION_INIT2(pApi);
  RegisterExtensionFunctions(db);
  /* added by Ahhgust:
     registerint spellfix within 
  RegisterExtensionFunctions(db)
  did not appear to work. This, however, does */
  sqlite3_spellfix_init(db, pzErrMsg, pApi);
  
  return 0;
}
#endif /* COMPILE_SQLITE_EXTENSIONS_AS_LOADABLE_MODULE */

map map_make(cmp_func cmp){
  map r;
  r.cmp=cmp;
  r.base = 0;

  return r;
}

void* xcalloc(size_t nmemb, size_t size, char* s){
  void* ret = calloc(nmemb, size);
  return ret;
}

void xfree(void* p){
  free(p);
}

void node_insert(node** n, cmp_func cmp, void *e){
  int c;
  node* nn;
  if(*n==0){
    nn = (node*)xcalloc(1,sizeof(node), "for node");
    nn->data = e;
    nn->count = 1;
    *n=nn;
  }else{
    c=cmp((*n)->data,e);
    if(0==c){
      ++((*n)->count);
      xfree(e);
    }else if(c>0){
      /* put it right here */
      node_insert(&((*n)->l), cmp, e);
    }else{
      node_insert(&((*n)->r), cmp, e);
    }
  }
}

void map_insert(map *m, void *e){
  node_insert(&(m->base), m->cmp, e);
}

void node_iterate(node *n, map_iterator iter, void* p){
  if(n){
    if(n->l)
      node_iterate(n->l, iter, p);
    iter(n->data, n->count, p);
    if(n->r)
      node_iterate(n->r, iter, p);
  }
}

void map_iterate(map *m, map_iterator iter, void* p){
  node_iterate(m->base, iter, p);
}

void node_destroy(node *n){
  if(0!=n){
    xfree(n->data);
    if(n->l)
      node_destroy(n->l);
    if(n->r)
      node_destroy(n->r);

    xfree(n);
  }
}

void map_destroy(map *m){
  node_destroy(m->base);
}

int int_cmp(const void *a, const void *b){
  int64_t aa = *(int64_t *)(a);
  int64_t bb = *(int64_t *)(b);
  /* printf("cmp %d <=> %d\n",aa,bb); */
  if(aa==bb)
    return 0;
  else if(aa<bb)
    return -1;
  else
    return 1;
}

int double_cmp(const void *a, const void *b){
  double aa = *(double *)(a);
  double bb = *(double *)(b);
  /* printf("cmp %d <=> %d\n",aa,bb); */
  if(aa==bb)
    return 0;
  else if(aa<bb)
    return -1;
  else
    return 1;
}

