/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/* machine.c */

/* missing routines on some systems; see include.h for defines */

#include "include.h"

/**********************************************************************8
*
* function: set_ctypes()
*
* purpose: implement toupper and tolower as arrays and set up
*          ctype flag bits.
*/
void set_ctypes()
{
  int c;
  for ( c = 0xFF ; c > 0 ; c-- )
  { kb_upper_array[c] = (char)c;
    kb_lower_array[c] = (char)c;
  }
  for ( c = 'a' ; c <= 'z' ; c++ )
    kb_upper_array[c] = (char)(c + 'A' - 'a');
  for ( c = 'A' ; c <= 'Z' ; c++ )
    kb_lower_array[c] = (char)(c - 'A' + 'a');

}


int kb_stricmp(a,b)
char *a,*b;
{ register char aa,bb;  /* lower case versions of characters */
  for(;;a++,b++)
  { aa = tolower(*a); 
    bb = tolower(*b);
    if ( aa < bb ) return -1;
    if ( aa > bb ) return 1;
    if ( !aa ) break;  /* have reached both nulls */
  }
  return 0;  /* equal strings */
}

int kb_strnicmp(a,b,n)
char *a,*b;
int n;  /* maximum characters to compare */
{ register char aa,bb;  /* lower case versions of characters */
  for(;n;n--,a++,b++)
  { aa = tolower(*a); 
    bb = tolower(*b);
    if ( aa < bb ) return -1;
    if ( aa > bb ) return 1;
    if ( !aa ) break;  /* have reached both nulls */
  }
  return 0;  /* equal strings */
}


void kb_strupr(s)
char *s;
{
  while ( *s )
     { *s = (char)toupper(*s);
        s++;
     }
}

/* finds string b in string a */
char *kb_strstr(a,b)
char *a;
char *b;
{
  char *ptr,*ch;

  for ( ; *a ; a++ )
     { for ( ptr = a, ch = b; *ch && (*ptr == *ch) ; ptr++,ch++ ) ; 
        if ( *ch == '\0' ) return a;
     }
  return NULL;
}


void kb_memmove(dest,src,n)
char *dest;
char *src;
size_t n;
{
  /* crude bytewise move */
  if ( (dest - src) > 0 )  /* move from top down */
     { 
        src += n; dest += n; 
        for ( ; n ; n-- ) *(--dest) = *(--src);
     }
  else  /* move from bottom up */
     { 
        for ( ; n ; n-- ) *(dest++) = *(src++);
     }
}


