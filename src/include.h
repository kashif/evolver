/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/**************************************************************
*  File: include.h                                            *
*  Purpose: master include file for all Evolver source files. * 
**************************************************************/

 
#if defined(WINTHREADS) || defined(PTHREADS)
#define THREADS
#define PARALLEL_MACHINE
#define SHARED_MEMORY
#endif

/* for tracking memory usage */
#ifdef MEMSTRINGS
#define mycalloc(num,size)  kb_calloc(num,size,__FILE__,__LINE__)
#define temp_calloc(num,size)  kb_temp_calloc(num,size,__FILE__,__LINE__)
#define temp_realloc(oldptr,size)  kb_temp_realloc(oldptr,size,__FILE__,__LINE__)
#define kb_realloc(ptr,new) KB_realloc(ptr,new,__FILE__,__LINE__)
#define dmatrix(a,b,c,d) kb_dmatrix(a,b,c,d,__FILE__,__LINE__)
#define dmatrix3(a,b,c) kb_dmatrix3(a,b,c,__FILE__,__LINE__)
#define dmatrix4(a,b,c,d) kb_dmatrix4(a,b,c,d,__FILE__,__LINE__)
#define temp_dmatrix(a,b,c,d) kb_temp_dmatrix(a,b,c,d,__FILE__,__LINE__)
#define temp_dmatrix3(a,b,c) kb_temp_dmatrix3(a,b,c,__FILE__,__LINE__)
#define temp_dmatrix4(a,b,c,d) kb_temp_dmatrix4(a,b,c,d,__FILE__,__LINE__)
#define my_list_calloc(a,b,c) list_calloc(a,b,c,__FILE__,__LINE__)
#define my_list_realloc(a,b,c) list_realloc(a,b,c,__FILE__,__LINE__)
#else
#define mycalloc(num,size)  kb_calloc(num,size)
#define temp_calloc(num,size)  kb_temp_calloc(num,size)
#define temp_realloc(oldptr,size)  kb_temp_realloc(oldptr,size)
#define kb_realloc(ptr,new) KB_realloc(ptr,new)
#define dmatrix(a,b,c,d) kb_dmatrix(a,b,c,d)
#define dmatrix3(a,b,c) kb_dmatrix3(a,b,c)
#define dmatrix4(a,b,c,d) kb_dmatrix4(a,b,c,d)
#define temp_dmatrix(a,b,c,d) kb_temp_dmatrix(a,b,c,d)
#define temp_dmatrix3(a,b,c) kb_temp_dmatrix3(a,b,c)
#define temp_dmatrix4(a,b,c,d) kb_temp_dmatrix4(a,b,c,d)
#define my_list_calloc(a,b,c) list_calloc(a,b,c)
#define my_list_realloc(a,b,c) list_realloc(a,b,c)
#endif
#include <time.h>

/* speed up to replace rep stosd; assumes size multiple of int */
#ifdef KBMEMSET
#define memset0(dest,count) { int im; int *pm = (int*)(dest) ;  \
    for ( im = (count)/sizeof(int) ; im > 0 ; im-- ) *(pm++) = 0; }
#else
#define memset0(dest,count) memset(dest,0,count)
#endif

/* Precision */
#ifdef LONGDOUBLE
#define REAL  long double 
#define DOT    dot 
#define DWIDTH ((sizeof(REAL)==16) ? 35 : 22)
#define DPREC ((sizeof(REAL)==16) ? 32 : 19)
#else
#ifdef FLOAT
#define REAL float
#define DOT  dotf
#define v3d  v3f
#define v2d  v2f  
#else 
#define REAL  double 
#define DOT    dot 
#endif
#endif

#ifdef USE_READLINE //CSL
#define MOREPROMPT (char *)1
#define CONTPROMPT (char *)2
#endif

/* following also works for Cray */
#ifdef GENERIC
/* Adjust header file names if yours are different */
#include <ctype.h>
#include <fcntl.h>
#include <errno.h>
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <setjmp.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/param.h>
#define drand48()    ((REAL)rand()/RAND_MAX)
#define srand48(seed)    srand(seed)
#define is_finite(x) (((x)>(-1e300))&&((x)<1e300)) 
/* MAXALLOC is maximum size allocable by calloc() */
#define    MAXALLOC 0x7FFFFFFFL
/* Some don't declare calloc, getenv, and bsearch in header files */
#if defined(__GNUC__) /* CSL */
/* PATHCHAR is name-separating character in paths */
#define PATHCHAR '/'
/* ENVPATHCHAR is the path separating character in environment strings */
#define ENVPATHCHAR ":"
#define FCAST (int(*)(const void*,const void *))
#else
char *calloc();
char *getenv();
char *bsearch();
/* PATHCHAR is name-separating character in paths */
#define PATHCHAR '/'
/* ENVPATHCHAR is the path separating character in environment strings */
#define ENVPATHCHAR ":"
/* NOPROTO should be defined for systems that don't do ANSI prototypes */
#define NOPROTO
#endif
#endif

#ifdef _HPUX_SOURCE
#include <ctype.h>
#include <fcntl.h>
#include <errno.h>
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <setjmp.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/param.h>
#define is_finite(x) (((x)>(-1e300))&&((x)<1e300)) 
#define    MAXALLOC 0x7FFFFFFFL
char *getenv();
#define PATHCHAR '/'
#define ENVPATHCHAR ":"
#define FCAST (int(*)(const void*,const void *))
#endif

#if defined(LINUX) || defined(__CYGWIN__)
#include <ctype.h>
#include <fcntl.h>
#include <errno.h>
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <setjmp.h>
#include <signal.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/param.h>
#include <glob.h>
#define is_finite(x) (((x)>(-1e300))&&((x)<1e300)) 
/* MAXALLOC is maximum size allocable by calloc() */
#define    MAXALLOC 0x7FFFFFFFL
/* Some don't declare calloc, getenv, and bsearch in header files */
/* Commenting these out now, since too many conflicts with headers.
#ifndef MAC_OS_X
char *calloc();
char *getenv();
char *bsearch();
#endif
*/
/* PATHCHAR is name-separating character in paths */
#define PATHCHAR '/'
/* ENVPATHCHAR is the path separating character in environment strings */
#define ENVPATHCHAR ":"
#define FCAST (int(*)(const void*,const void *))
#endif

#ifdef DECALPHA
#include <ctype.h>
#include <fcntl.h>
#include <errno.h>
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <setjmp.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/param.h>
#define is_finite(x) finite(x)
#define    MAXALLOC 0x7FFFFFFFL
char *getenv();
#define PATHCHAR '/'
#define ENVPATHCHAR ":"
#endif


#ifdef MAC_APP
/* this was done with THINK C */
/* Adjust header file names if yours are different */
#include <ctype.h>
#include <fcntl.h>
#include <errno.h>
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <setjmp.h>
#include <signal.h>
#include <unix.h>
#define is_finite(x) (((x)>(-1e300))&&((x)<1e300)) 
/* MAXALLOC is maximum size allocable by calloc() */
#define    MAXALLOC 0x7FFFFFFFL
/* Macs don't have environments */
#define getenv(a)  ":fe;:doc"
/* PATHCHAR is name-separating character in paths */
#define PATHCHAR ':'
/* ENVPATHCHAR is the path separating character in environment strings */
#define ENVPATHCHAR ";"
#define drand48()    ((REAL)rand()/RAND_MAX)
#define srand48(seed)    srand(seed)
#define NOPIPE
/* cast for comparison functions for qsort() and bsearch() */
#define FCAST (__cmp_func)
/* to avoid Think C name conflict in a header file */
#define extend my_extend
#define NO_YACC_DEBUG
/* for workaround of Symantec 8.0.5 68K compiler bug */
#define ANSI_DEF
#endif


#ifdef MAC_CW
/* this was done with CODE WARRIOR */
/* Adjust header file names if yours are different */
#include <ctype.h>
#include <fcntl.h>
#include <errno.h>
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <setjmp.h>
#include <signal.h>
#include <unix.h>
#include <alloca.h>
#define is_finite(x) (((x)>(-1e300))&&((x)<1e300)) 
/* MAXALLOC is maximum size allocable by calloc() */
#define    MAXALLOC 0x7FFFFFFFL
/* Macs don't have environments */
#define getenv(a)  ":fe;:doc;::fe;::doc;:::fe;:::doc"
/* PATHCHAR is name-separating character in paths */
#define PATHCHAR ':'
/* ENVPATHCHAR is the path separating character in environment strings */
#define ENVPATHCHAR ";"
#define drand48()    ((REAL)rand()/RAND_MAX)
#define srand48(seed)    srand(seed)
#define NOPIPE
/* cast for comparison functions for qsort() and bsearch() */
#define FCAST (void*)
/* to avoid Think C name conflict in a header file */
#define extend my_extend
/* for workaround of Symantec 8.0.5 68K compiler bug */
#define ANSI_DEF
#endif


#ifdef IRIS

/* this weirdness was needed on an IRIX 5.2 machine */
/* due to inconsistent use of _SIZE_T and _SIZE_T_ in headers */
#if !defined(_SIZE_T) && !defined(_SIZE_T_)
typedef unsigned size_t;
#endif
#define _SIZE_T
#define _SIZE_T_

/* Kludge for IRIX 6.5 */
#include <ctype.h>
#undef isalpha
#undef isdigit
#undef isprint
#undef islower
#undef isupper

#include <fcntl.h>
#include <errno.h>
#include <math.h>
#include <malloc.h>
#include <string.h>
#include <bstring.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <memory.h>
#include <setjmp.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/param.h>
#include <stropts.h>
#include <poll.h>
#include <fp_class.h>
#define  is_finite(x) finite(x)
#define    MAXALLOC 0x7FFFFFFFL
#define PATHCHAR '/'
#define ENVPATHCHAR ":"
#define FCAST (int(*)(const void*,const void *))
#ifdef SGI_MULTI
#define PARALLEL_MACHINE
#define SHARED_MEMORY
#define GET_THREAD_ID m_get_myid()
#include <task.h>
extern int _fork(void (*)(), ...);
extern int _park_procs(void);
extern int _rele_procs(void);
extern void _sync(void);
extern int _kill_procs(void);
extern int _get_myid(void);
extern int _set_procs(int);
extern int _get_numprocs(void);
extern unsigned m_next(void);
extern void m_lock(void);              
extern void m_unlock(void); 

#endif
#endif

#ifdef SGI_MULTI
#include <ulocks.h>
#include <task.h>
#define M_LOCK(addr) (locklist[0]?ussetlock(locklist[((long)(addr))&(_MAXLOCKS-1)]):0)
#define M_UNLOCK(addr) (locklist[0]?usunsetlock(locklist[((long)(addr))&(_MAXLOCKS-1)]):0)
#endif


#ifdef GCC
#include <ctype.h>
#include <fcntl.h>
#include <errno.h>
#include <math.h>
#include <malloc.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <setjmp.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/param.h>
#define is_finite(x) (((x)>(-1e300))&&((x)<1e300)) 
#define MAXALLOC 0x7FFFFFFFL
#ifdef MSDOS
#define PATHCHAR '\\'
#define ENVPATHCHAR ";"
#else
#define PATHCHAR '/'
#define ENVPATHCHAR ":"
#endif
#define FCAST (int(*)(const void*,const void *))
#define drand48() ((REAL)random()/0x7FFFFFFF)
#define srand48(seed)    srandom(seed)
#endif


#ifdef SUN
#include <ctype.h>
#include <fcntl.h>
#include <errno.h>
#include <math.h>
#include <malloc.h>
#ifdef strdup
#undef strdup
#endif
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <setjmp.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/param.h>
#include <values.h>
#define is_finite(x)  finite(x)
#define MAXALLOC 0x7FFFFFFFL
char *getenv();
/* char *calloc(); */
#define PATHCHAR '/'
#define ENVPATHCHAR ":"
/* #define  memmove(dest,src,n) kb_memmove(dest,src,n) */
#define NOPROTO
#define FCAST (int(*)(const void*,const void *))
/* following is to undo something mysterious done by prof.h */
#undef MARK
#endif


#ifdef MSC
#define _CRT_SECURE_NO_DEPRECATE 1
#pragma warning(disable:4996)
#include <windows.h>
#define _USERENTRY
#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <float.h>
#include <fcntl.h>
#include <io.h>
#include <malloc.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <memory.h>
#include <setjmp.h>
#include <signal.h>
#include <limits.h>
#include <direct.h>
#include <process.h>
#include <sys/types.h>
#define is_finite(x) (((x)>(-1e300))&&((x)<1e300)) 
#ifndef MAXINT
#define MAXINT INT_MAX
#endif
#define    MAXALLOC 0x7FFFFFFFL
#define PATHCHAR '\\'
#define ENVPATHCHAR ";"
#define farcalloc(num,size)  (void far *)halloc(num,size)
#define farfree(ptr)            hfree((void huge *)ptr)
#define FCAST (int(*)(const void*,const void *))
#define NOPIPEXX
#define popen _popen
#define pclose _pclose
#define _export
#define FPRESET _fpreset()
/* since graphics in parallel thread */
#define PARALLEL_MACHINE
#endif

#if defined(__WIN32__) && !defined(__CYGWIN__) 
#include <windows.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <mem.h>
#include <alloc.h>
#include <malloc.h>
#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <fcntl.h>
#include <conio.h>
#include <setjmp.h>
#include <signal.h>
#include <values.h>
#include <process.h>
#define is_finite(x) (((x)>(-1e300))&&((x)<1e300)) 
#define    MAXALLOC 0x7FFFFFFFL
#define PATHCHAR '\\'
#define ENVPATHCHAR ";"
#define NOPIPEXX
#pragma hdrstop
/* so qsort() and bsearch() below agree with prototypes */
#define FCAST (int(*)(const void*,const void *))
#define drand48()    ((REAL)rand()/RAND_MAX)
#define srand48(seed)    srand(seed)
#else
#define __int32 int
#endif

#ifdef MAC_OS_X
/* rogue constant from some header */
#endif

#ifdef MPI_EVOLVER
#define PARALLEL_MACHINE
#include "mpi.h"
#undef MPI_REAL
#ifdef LONG_DOUBLE
#define MPI_REAL MPI_LONG_DOUBLE
#elif defined(FLOAT)
#define MPI_REAL MPI_FLOAT
#else
#define MPI_REAL MPI_DOUBLE
#endif
#define MPI_EXPORT_MAX 4
#endif

#ifndef SGI_MULTI
#define m_get_myid() 0
#endif

#ifdef TC
#define atold _atold
#endif

#ifndef TC
#ifndef huge
#define huge
#endif
#endif

#ifndef FCAST
#define FCAST
#endif

typedef int DY_OFFSET ;



/* Some don't have these manifest constants in math.h (namely, Microsoft) */
#ifndef M_LN2
#define M_E                 2.71828182845904523536
#define M_PI                3.14159265358979323846
#define M_LN2              0.693147180559945309417
#endif

#ifdef LONGDOUBLE
#undef  M_E
#define M_E                 2.7182818284590452353602874713527L
#undef  M_PI
#ifdef TC
#define M_PI (atan(1.0)*4)
#else
#define M_PI                3.1415926535897932384626433832795L
#endif
#undef  M_LN2
#define M_LN2              0.693147180559945309417L
#endif

#ifndef DBL_EPSILON
#define DBL_EPSILON      2.2204460492503131e-16
#endif

/* can undefine or redefine these if your system has decent string functions */
#ifndef stricmp
#define stricmp(s1,s2)  kb_stricmp((s1),(s2))
#define strnicmp(s1,s2,n)  kb_strnicmp((s1),(s2),(n))
#endif
#ifndef strstr
#define strstr(s1,s2) kb_strstr((s1),(s2))
#endif
#ifndef strupr
#define strupr(a) kb_strupr(a)
#endif

/* Since tolower and toupper don't always  check case before converting */
#undef tolower
#undef toupper
#ifndef MSC
#define tolower(c)    (isupper(c) ? ((c)-'A'+'a') : c)
#define toupper(c)    (islower(c) ? ((c)-'a'+'A') : c)
#endif

#ifndef MAXDOUBLE
#define MAXDOUBLE 1.0e38
#endif

#ifdef NOPROTO
/* some compilers still don't like prototypes with arguments */
#define ARGS(x) ()
#else
#define ARGS(x) x
#endif

/* for converting old-style function defs to new */
#ifdef __cplusplus
#define ARGS1(old,a)  (a)
#define ARGS2(old,a,b)  (a,b)
#define ARGS3(old,a,b,c)  (a,b,c)
#define ARGS4(old,a,b,c,d)  (a,b,c,d)
#define ARGS5(old,a,b,c,d,e)  (a,b,c,d,e)
#define ARGS6(old,a,b,c,d,e,f)  (a,b,c,d,e,f)
#define CONST const
#else
#define ARGS1(old,a)  old  a;
#define ARGS2(old,a,b)  old a; b;
#define ARGS3(old,a,b,c)  old a; b; c;
#define ARGS4(old,a,b,c,d)  old a; b; c; d;
#define ARGS5(old,a,b,c,d,e)  old a; b; c; d; e;
#define ARGS6(old,a,b,c,d,e,f)  old a; b; c; d; e; f;
#ifndef CONST
#define CONST
#endif
#endif

#ifdef ENABLE_DLL
#ifndef WIN32
#include <dlfcn.h>
#endif
#endif

#ifdef USE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif


#ifdef PTHREADS
#include <pthread.h>
#endif

/* Evolver header files */
#ifdef __cplusplus
extern "C" {
#endif
#include "model.h"
#include "storage.h"
#include "skeleton.h"
#include "extern.h"
#include "express.h"
#include "quantity.h"
#include "web.h"
#include "lex.h"
#ifdef __cplusplus
}
#endif
#include "proto.h"

/* in case of non-parallel machines */
#ifndef M_LOCK
#define M_LOCK(addr)
#define M_UNLOCK(addr) 
#endif

#ifndef MAXINT
#define MAXINT (~(1<<(8*sizeof(int)-1)))
#endif

#ifndef FPRESET
#define FPRESET
#endif

#if defined(LONGDOUBLE) && !defined(NOLONGMATHFUNC)
/* have to do these after math.h */
#define sin sinl
#define cos cosl
#define tan tanl
#define asin asinl
#define acos acosl
#define atan atanl
#define sinh sinhl
#define cosh coshl
#define tanh tanhl
#define asinh asinhl
#define acosh acoshl
#define atanh atanhl
#define exp expl
#define log logl
#define pow powl
#define sqrt sqrtl
#define ceil ceill
#define fabs fabsl
extern REAL fabsl(REAL); /* wasn't in IRIX6.1 math.h */
#define floor floorl
#define fmod fmodl
#define modf modfl
#ifdef LINUX
long double strtold(const char *, char **);
#define atof(a) strtold(a,NULL)
#else
#define atof atold
#endif
#endif

#ifdef INLINE
#include "inline.h"
#endif
/* for things we really want to be plain double */
#ifndef DOUBLE
#define DOUBLE double
#endif

#ifdef MSC
#define toupper(c) kb_upper_array[c]
#define tolower(c) kb_lower_array[c]
#endif

#ifdef MPI_EVOLVER
#include "mpi_evolver.h"
#endif
