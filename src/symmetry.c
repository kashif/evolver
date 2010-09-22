/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*************************************************************
/*
/*     file:     symmetry.c
/*
/*     Purpose:  Functions dealing with symmetries of surface.
/*               Has the three functions for torus.
/*               If user wants to define his/her own symmetry
/*               group, put your functions in this file,
/*               and change names right below here.  But leave
/*               torus functions intact, so torus domain can
/*               be used.
/*
 */

#include "include.h"

/* change torus stuff in following lines to your own names */
char *symmetry_name = "torus";  /* name appearing in datafile. lowercase! */
#ifdef NOPROTO
void torus_wrap();   /* declare */
WRAPTYPE torus_compose();   /* declare */
WRAPTYPE torus_inverse();   /* declare */
void (*sym_func)() = torus_wrap; /* set global pointer to function */
WRAPTYPE (*sym_compose)() = torus_compose; /* set global pointer to function */
WRAPTYPE (*sym_inverse)() = torus_inverse; /* set global pointer to function */
#else
void torus_wrap(REAL*,REAL*,WRAPTYPE);   /* declare */
WRAPTYPE torus_compose(WRAPTYPE,WRAPTYPE);   /* declare */
WRAPTYPE torus_inverse(WRAPTYPE);   /* declare */
void (*sym_func)(REAL*,REAL*,WRAPTYPE) = torus_wrap;
WRAPTYPE (*sym_compose)(WRAPTYPE,WRAPTYPE) = torus_compose; 
WRAPTYPE (*sym_inverse)(WRAPTYPE) = torus_inverse; 
#endif

/*
       Group elements are encoded in three-bit fields in "wrap",
       one for each dimension, with "001" for positive wrap
       and "011" for negative wrap.  You are free to encode
       any way you want, but use wrap = 0 for the identity.
*/

#define ALLWRAPMASK 03333333333

/*******************************************************************
*
*  function: torus_wrap
*
*  purpose:  Provide adjusted coordinates for vertices that get
*            wrapped around torus.  Serves as example for user-written
*            symmetry function.
*
*            This function uses the values of the torus periods read
*            in from the data file. Yours doesn't have the privilege
*            of defining its own datafile syntax, but you can access
*            values of parameters defined in the datafile as
*            web.params[n].value.
*/

void torus_wrap(x,y,wrap)
REAL *x;   /* original coordinates */
REAL *y;   /* wrapped coordinates  */
WRAPTYPE wrap;  /* encoded symmetry group element, 3 bits per dimension */
{
  int i,j;

  memcpy((char*)y,(char*)x,web.space_dimension*sizeof(REAL));
  for ( i = 0 ; wrap != 0 ; i++, wrap >>= TWRAPBITS )
    switch ( wrap & WRAPMASK )
      {  case 0: break;
         case POSWRAP: for ( j = 0 ; j < web.space_dimension ; j++ )
                   y[j] += web.torus_period[i][j];
                 break;
         case NEGWRAP: for ( j = 0 ; j < web.space_dimension ; j++ )
                   y[j] -= web.torus_period[i][j];
                 break;
         default: error("Unexpectedly large torus wrap.\n",WARNING);
      }
}

/********************************************************************
*
*  function: torus_compose()
*
*  purpose:  do composition of two group elements
*
*/

WRAPTYPE torus_compose(wrap1,wrap2)
WRAPTYPE wrap1,wrap2;  /* the elements to compose */
{
  return (wrap1 + wrap2) & ALLWRAPMASK;
}


/********************************************************************
*
*  function: torus_inverse()
*
*  purpose:  return inverse of group element.
*
*/

WRAPTYPE torus_inverse(wrap)
WRAPTYPE wrap;  /* the element invert */
{
  return ((~ALLWRAPMASK)-wrap) & ALLWRAPMASK;
}
  

  
