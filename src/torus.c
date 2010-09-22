/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*************************************************************
*
*      file:      torus.c
*
*      Purpose:  Functions dealing with symmetries of surface.
*                 Has the three functions for torus.
*                 If user wants to define his/her own symmetry
*                 group, write a file like this and put function
*                 names in registry.c.
*
*/

#include "include.h"

/*
         Group elements are encoded in two-bit fields in "wrap",
         one for each dimension, with "000001b" for positive wrap
         and "011111b" for negative wrap.  You are free to encode
         any way you want, but use wrap = 0 for the identity.
*/

/* specific case of torus symmetry representation */
/* these also defined in extern.h since torus built-in */
#ifndef TWRAPBITS
#define TWRAPBITS 6
#define POSWRAP    1
#define WRAPMASK  037
#define ALLWRAPMASK 03737373737
#define NEGWRAP    WRAPMASK
#define WRAPNUM(x) ( (x)>(1<<(TWRAPBITS-2)) ? (x)-(1<<(TWRAPBITS-1)) : (x))
#endif
#define TERRDETECT  06060606060

/*******************************************************************
*
*  function: torus_wrap
*
*  purpose:  Provide adjusted coordinates for vertices that get
*            wrapped around torus.  Serves as example for user-written
*            symmetry function.
*
*            This function uses the values of the torus periods read
*            in from the data file. 
*/

void torus_wrap(x,y,wrap)
REAL *x;    /* original coordinates */
REAL *y;    /* wrapped coordinates  */
WRAPTYPE wrap;  /* encoded symmetry group element, TWRAPBITS bits per dimension */
{
  int i,j;
  int wrapnum;

  if ( x != y )
     for ( i = 0 ; i < SDIM ; i++ ) y[i] = x[i];
  for ( i = 0 ; wrap != 0 ; i++, wrap >>= TWRAPBITS )
  { wrapnum =  WRAPNUM(wrap & WRAPMASK);
    if ( wrapnum )
       for ( j = 0 ; j < SDIM ; j++ )
          y[j] += wrapnum*web.torus_period[i][j];
  }
}


/*******************************************************************
*
*  function: torus_form_pullback
*
*  purpose:  Pull back differentail forms at vertices that get
*                wrapped around torus.  Serves as example for user-written
*                symmetry function.
*
*                Torus is flat, so implementation is trivial copy.
*/

void torus_form_pullback(x,xform,yform,wrap)
REAL *x;    /* original coordinates */
REAL *xform; /* pulled back form */
REAL *yform;    /* wrapped form, input  */
WRAPTYPE wrap;  /* encoded symmetry group element, 3 bits per dimension */
{
  memcpy((char*)xform,(char*)yform,SDIM*sizeof(REAL));
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
{ int w = (wrap1 + wrap2) & ALLWRAPMASK;
#ifdef _DEBUG
  int i;
  int ww = w;

  if ( verbose_flag )
  for ( i = 0, ww=w ; i < SDIM ; i++ )
  { switch(ww&WRAPMASK)
    { case 0: case POSWRAP: case NEGWRAP: break;
      default: kb_error(3917,"Bad wrap as result of torus_compose.\n",
         WARNING);
    }
    ww >>= TWRAPBITS; 
  }
 #endif
  if ( ~(wrap1|wrap2) & TERRDETECT & w )
    kb_error(4553,"Wrap out of bounds as result of torus_compose.\n",
         WARNING);
  return w;
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
  

/*********************************************************************
*
* function: torus_edge_unwrap()
*
* purpose: get wrap of edge to 0 (preliminary to deletion or other
*          topology change) so endpoints near as possible in 
*          a fundamental region.
*/
void torus_unwrap_edge(e_id)
edge_id e_id;
{ edge_id pos_e = positive_id(e_id);
  vertex_id tailv,headv;
  REAL *x;
  WRAPTYPE wrap = get_edge_wrap(pos_e);
  WRAPTYPE tailwrap,headwrap;
  REAL u[MAXCOORD]; /* tail unit cell coords */
  int i;
  
  if ( wrap == 0 ) return;
  tailv = get_edge_tailv(pos_e);
  headv = get_edge_headv(pos_e);
  x = get_coord(tailv);
  matvec_mul(web.inverse_periods,x,u,SDIM,SDIM);
  tailwrap = 0;
  for ( i = SDIM-1 ; i >= 0 ; i-- )
  { tailwrap <<= TWRAPBITS;
    tailwrap |= ((int)(floor(u[i]))) & WRAPMASK;
  }   
  tailwrap = torus_inverse(tailwrap);
  headwrap = torus_compose(tailwrap,wrap);
  wrap_vertex(tailv,tailwrap);
  wrap_vertex(headv,headwrap);
}  

/*************************************************************
*
* Functions: Identity symmetry functions.
*/

void identity_wrap(x,y,wrap)
REAL *x;    /* original coordinates */
REAL *y;    /* wrapped coordinates  */
WRAPTYPE wrap;  /* encoded symmetry group element, TWRAPBITS bits per dimension */
{
  int i;

  if ( x != y )
     for ( i = 0 ; i < SDIM ; i++ ) 
       y[i] = x[i];
}
     
void identity_form_pullback(x,xform,yform,wrap)
REAL *x;    /* original coordinates */
REAL *xform; /* pulled back form */
REAL *yform;    /* wrapped form, input  */
WRAPTYPE wrap;  /* encoded symmetry group element, 3 bits per dimension */
{
  memcpy((char*)xform,(char*)yform,SDIM*sizeof(REAL));
}

WRAPTYPE identity_compose(wrap1,wrap2)
WRAPTYPE wrap1,wrap2;  /* the elements to compose */
{ return 0;
}

WRAPTYPE identity_inverse(wrap)
WRAPTYPE wrap;  /* the element invert */
{
  return 0;
}
 
void identity_unwrap_edge(e_id)
edge_id e_id;
{}