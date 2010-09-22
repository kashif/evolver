/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*************************************************************
*
*   file:      khyp.c
*
*   Purpose:  Illustration of how to do quotient spaces.
*             This example does a Klein hyperbolic model in R^2.
*             The user needs to define the integer representation
*             used for group elements and three functions to
*             do group operations.  WRAPTYPE is the type of
*             group element representation, currently long int.
*             You must write your own versions of group_wrap(),
*             group_compose(), and group_inverse().
*
*             You don't have the privilege
*             of defining your own datafile syntax, but you can access
*             values of parameters defined in the datafile as
*             *var, where var = get_global(char *name).
*/

#include "include.h"


#define PDIM  4  /* Minkowski space dimension */
/* Minkowski dim is tacked on before other dimensions so this file
    can be used in different dimensions */

/* Description of group element encoding: 
    The Klein  disk is always mapped back to a Minkowski hyperbola
    for transformations.
    There are 8 translation elements that translate the fundamental region
    to one of its neighbors.  Translating around a vertex gives a
    circular string of the 8 elements.  The group elements encoded are
    substrings of the 8, with null string being the identity.  Encoding
    is 4 bits for start element, 4 bits for stop (actually the one
    after stop so 0 0 is identity).
*/

#define GENUS2BITS 4
#define GENUS2MASK 7

#define CHOP(g) ((g)&GENUS2MASK)
#define END(g) CHOP((g)>>GENUS2BITS)
#define DFF(g,h,d) (CHOP((h)-(g))==(d))
#define ENC(g1,gk) (CHOP(g1) | (CHOP(gk)<<GENUS2BITS))
#define WRAP(g1,gk) (DFF(g1,gk,0)? 0:(DFF(g1,gk,7)? ENC(g1+3,g1+4):ENC(g1,gk)))

/* define the two group generators used in generating the 8 elements */
/* Group elements alway act in Minkowski space */
#undef COSH
#undef SINH
#undef SIN
#undef COS
#define COSH 2.414213562373 /* 1+sqrt(2) */
#define SINH 2.19736822693562
#define SIN 0.70710678118655 /* sqrt(2)/2 */
#define COS -0.70710678118655 /* sqrt(2)/2 */
REAL TrMat[PDIM][PDIM] =
     {{COSH,SINH,0.,0.},{SINH,COSH,0.,0.},{0.,0.,1.,0.},{0.,0.,0.,1.}};
REAL RotMat[PDIM][PDIM] = /* rotate by 135 deg clockwise */
     {{1.,0.,0.,0.},{0.,COS,SIN,0.},{0.,-SIN,COS,0.},{0.,0.,0.,1.}};
REAL id_mat[PDIM][PDIM] = 
    {{1.,0.,0.,0.},{0.,1.,0.,0.},{0.,0.,1.,0.},{0.,0.,0.,1.}};

int q_init_flag;     /* set once stuff initialized */
REAL eg[8][PDIM][PDIM];  /* generator matrices */

typedef REAL ptype[PDIM]; 
typedef REAL pptype[PDIM][PDIM]; 


/*****************************************************************
* 
* Some utility functions for matrix operations on fixed size
* matrices.
*/
  
static void copy ARGS((ptype,ptype));
static void matmult ARGS((pptype,ptype,ptype));
static void applygen ARGS((int,ptype,ptype));
static void copymat ARGS((pptype,pptype));
static void matmatmult ARGS((pptype,pptype,pptype));
static void genmat ARGS((int,pptype));
static void q_init ARGS ((void));
void khyp_wrap ARGS((REAL*,REAL*,WRAPTYPE));
WRAPTYPE khyp_compose ARGS((WRAPTYPE,WRAPTYPE));
WRAPTYPE khyp_inverse ARGS((WRAPTYPE));
void khyp_form_pullback ARGS(( REAL *,REAL *,REAL *,WRAPTYPE));

static void copy(z,w)
REAL z[PDIM],w[PDIM];
{
     int j;

     for (j=0; j<PDIM; j++)
          w[j]=z[j];
}

static void matmult(m,z,w)
REAL m[PDIM][PDIM],z[PDIM],w[PDIM];
{
     int i,j;
     REAL t[PDIM];

     for (j=0; j<=SDIM; j++)
          for (i=0,t[j]=0.; i<=SDIM; i++)
                t[j] += m[j][i] * z[i];
     copy(t,w);
}

static void applygen(g,z,w)
int g;
REAL z[PDIM],w[PDIM];
{
     REAL t[PDIM];
     int i;

     g = CHOP(g);
     copy(z,t);
     for (i=0;i<g;i++)
          matmult(RotMat,t,t);
     matmult(TrMat,t,t);
     matmult(TrMat,t,t);
     for (i=g;i<8;i++)
          matmult(RotMat,t,t);
     copy(t,w);
}

static void copymat(a,b)
REAL a[PDIM][PDIM],b[PDIM][PDIM];
{
  memcpy((char*)b,(char*)a,PDIM*PDIM*sizeof(REAL));
}

static void matmatmult(a,b,c)
REAL a[PDIM][PDIM],b[PDIM][PDIM],c[PDIM][PDIM];
{ int i,j,k;
  REAL t[PDIM][PDIM];
  REAL *tp;
  for ( i = 0 ; i <=SDIM ; i++ )
     for ( k = 0, tp = t[i] ; k <= SDIM ; k++,tp++ )
        { *tp = 0.0;
          for ( j = 0 ; j <= SDIM ; j++ )
             *tp += a[i][j]*b[j][k];
        }
  copymat(t,c);
}

static void genmat(g,t)
int g;
REAL t[PDIM][PDIM];
{
     int i;

     for (i=0;i<g;i++)
          matmatmult(RotMat,t,t);
     matmatmult(TrMat,t,t);
     matmatmult(TrMat,t,t);
     for (i=g;i<8;i++)
          matmatmult(RotMat,t,t);
}

static void q_init()
{ int i;
  
  for ( i = 0 ; i < 8 ; i++ )
    { copymat(id_mat,eg[i]);
      genmat(i,eg[i]);
    }

  q_init_flag = 1;
}

/*******************************************************************
*
*  function: khyp_wrap
*
*  purpose:  Provide adjusted coordinates for vertices that get
*                wrapped around torus.  Serves as example for user-written
*                symmetry function.
*
*/

void khyp_wrap(x,y,wrap)
REAL *x;    /* original coordinates */
REAL *y;    /* wrapped coordinates  */
WRAPTYPE wrap;  /* encoded symmetry group element */
{
     REAL z[2][PDIM]; /* two work vectors */
     int j,i;
     int g1,gk, g;

     if ( q_init_flag == 0 ) q_init();

     if (wrap==0)
     {
          for (j=0; j<SDIM; j++)
                y[j]=x[j];
          return;
     }

     g1 = CHOP(wrap);
     gk = CHOP(wrap>>GENUS2BITS);
     if (gk<g1) gk += 8;

     copy(x,z[0]+1);
     z[0][0] = 1.;
     for (g=gk-1,j=0; g>=g1; g--,j=1-j)
     {
          matmult(eg[CHOP(g)],z[j],z[1-j]);
     }
    for ( i = 0 ; i < SDIM ; i++ ) y[i] = z[j][i+1]/z[j][0];
}


/********************************************************************
*
*  function: khyp_compose()
*
*  purpose:  do composition of two group elements
khyp_wrap(khyp_wrap(pt,w),v) = khyp_wrap(pt,khyp_compose(v,w))
If the current wrap is v, and we get to an edge with wrap w,
and its head is at pt, then either of these should give the unwrapped
coordinates of its head (relative to khyp_wrap(tail,v) being its tail).
*
*/

/* generators 1=A,2=b,3=C,4=d,5=a,6=B,7=c,0=8=D */
/* inverses differ by 4 */

#define TRY(g1,gk,h1,hk) if (DFF(gk,h1,0)) return WRAP(g1,hk)

WRAPTYPE khyp_compose(gw,hw)
WRAPTYPE gw,hw;  /* the elements to compose */
{
     int g1,gk, h1,hk;

     if (gw==0) return hw;
     if (hw==0) return gw;
     g1 = CHOP(gw); gk = END(gw);
     h1 = CHOP(hw); hk = END(hw);

     TRY(g1,gk,h1,hk);  /* h nicely follows after g */
     if (DFF(h1,hk,1)) TRY(g1,gk,h1-3,h1-4); /* equiv string of 7 */
     if (DFF(g1,gk,1)) TRY(g1-3,g1-4,h1,hk); /* equiv string of 7 */
     if (DFF(h1,hk,1) && DFF(g1,gk,1)) TRY(g1-3,g1-4,h1-3,h1-4);

     sprintf(msg,"Trying to compose %d-%d and %d-%d\n",g1,gk,h1,hk);
     strcat(msg,"khyp: Wrap outside known range\n");
     kb_error(1305,msg,WARNING);

     return 0;
}


/********************************************************************
*
*  function: khyp_inverse()
*
*  purpose:  return inverse of group element.
*
*/

WRAPTYPE khyp_inverse(wrap)
WRAPTYPE wrap;  /* the element to invert */
{
    return WRAP(END(wrap),CHOP(wrap));
}
  
/*******************************************************************
*
*  function: khyp_form_pullback
*
*  purpose:  Pull back differential forms at vertices that get
*                wrapped.
*
*/

void khyp_form_pullback(x,xform,yform,wrap)
REAL *x;    /* original coordinates */
REAL *xform;  /* result pullback */
REAL *yform;    /* original form input  */
WRAPTYPE wrap;  /* encoded symmetry group element */
{
  int i,j;
  REAL trans[PDIM][PDIM];
  REAL jac[PDIM][PDIM];  /* Jacobian matrix */
  REAL y[PDIM];
  REAL w[PDIM];  /* Minkowski coord of x */
  int g1,gk,g;    /* element numbers */

  if ( wrap == 0 ) /* just copy */
     { memcpy((char *)xform,(char*)yform,SDIM*sizeof(REAL));
        return;
     }

  g1 = CHOP(wrap);
  gk = CHOP(wrap>>GENUS2BITS);
  if (gk<g1) gk += 8;

  /* get total linear transformation */
  copymat(eg[g1],trans);
  for ( g = g1+1; g < gk ; g++ )
    matmatmult(trans,eg[CHOP(g)],trans);

  /* get transformed point in Minkowski */
  memcpy((char *)(w+1),(char*)x,SDIM*sizeof(REAL));
  w[0] = 1.0;
  matmult(trans,w,y);

  /* set up Jacobian */
  for ( i = 0 ; i < SDIM ; i++ )
    for ( j = 0 ; j < SDIM ; j++ )
      jac[i][j] = (trans[i+1][j+1] - y[i+1]*trans[0][j+1]/y[0])/y[0];

  /* pull back form with transpose of jacobian */
  for ( i = 0 ; i < SDIM ; i++ )
     for ( j = 0, xform[i] = 0. ; j < SDIM ; j++ )
        xform[i] += jac[j][i]*yform[j];

}

