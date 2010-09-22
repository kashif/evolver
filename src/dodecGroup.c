/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/****************************************************************************
* This file generates the elements of the group of translations of
* hyperbolic 3 space tiled with right-angled dodecahedra. The elements of the
* group are represented as integers. There are 32 generators of the group
* so each generator is represented by five bits.  Under this scheme any
* element that is the composition of up to five generators can be
* represented.
*/

#include "include.h"
#include <stdio.h>
#include <math.h>

#define PDIM 4
#define BITS1 1
#define BITS2 3
#define BITS3 7
#define BITS5 31

#define NUMGEN(g) ((g)&BITS3)
#define FINDGEN(g) ((g)&BITS5)
#define GENINV(g) FINDGEN(((g)+16))
#define CHOP(g) ((g)>>5)

static REAL ident_mat[PDIM][PDIM] = {{1.0,.0,.0,.0},{.0,1.0,.0,.0},
                                            {.0,.0,1.0,.0},{.0,.0,.0,1.0}};

static REAL centerRotate[PDIM][PDIM] =
{{1.000000, 0.000000, 0.000000, 0.000000},
    {0.000000, 0.000000, 1.000000, 0.000000},
    {0.000000, -1.000000, 0.000000, 0.000000},
    {0.000000, 0.000000, 0.000000, 1.000000}};

static REAL rotates[PDIM][PDIM][PDIM] =
{{{1.,1.253445216142309e-16,
      -1.573266467599309e-16,-1.902396940417021e-16},
     {1.824701509283392e-16,-10.47216984175156,
      12.46875301145284,16.25226577604197},
     {-8.46614841566922e-17,3.996614189627698,
      -6.472114212317669,-7.54063573964542},
     {1.902396940417009e-16,-11.16419572451243,
      14.01278216621864,17.94428405406906}},
    {{-1.618048902555269,-4.116373394860049,
      3.236086078076509,5.388400994886382},
        {-0.00001548628343482931,-2.236098819547112,
      2.544065459407748,3.236109854867625},
        {3.202383078490913e-6,2.544051352436071,
      -1.000010980411733,-2.544055668570577},
        {-1.272038620211825,-5.236111643244772,
      4.116375119861672,6.854158702514121}},
    {{-1.61804018386751,4.116370344397128,
      -3.236064438109621,-5.388383050270264},
        {5.581793090786349e-6,-2.236104800200569,
      2.544047980565907,3.2361002464282},
        {-4.388099372461566e-6,2.544047980565911,
      -0.999990892452044,-2.544044400640008},
        {1.272027529835902,-5.236110160905107,
      4.11634242501839,6.854135876520134}},
    {{-5.23603242311647e-12,2.272015247697663,
      0.272030671522342,-2.058167624859446},
        {-2.272005895364247,-4.162032037143264,
      -0.6180552894206566,4.676164433331837},
        {-0.2720295517580531,-0.6180552894206578,
      0.925999618361096,0.5598818723761604},
        {-2.058157152794606,-4.676168977352982,
      -0.5598824164363831,5.236032418787402}}};
static int q_init_flag = 0;
REAL gen[32][PDIM][PDIM];

typedef REAL pmat[PDIM];
typedef REAL ppmat[PDIM][PDIM];
static void copy ARGS(( pmat, pmat ));
static void copyMat ARGS(( ppmat, ppmat));
static void matMult ARGS(( ppmat, ppmat, ppmat));
static void matVecMult ARGS(( ppmat , pmat , pmat ));
static void convertToHyp ARGS(( pmat , pmat ));
static void calcGen ARGS(( WRAPTYPE , ppmat ));
static void calcElem ARGS(( WRAPTYPE , ppmat ));
static WRAPTYPE check_inverse ARGS(( WRAPTYPE , WRAPTYPE ));
void init_gen ARGS((void));
WRAPTYPE dodec_inverse ARGS((WRAPTYPE));
void dodec_wrap ARGS(( REAL x[PDIM], REAL y[PDIM],WRAPTYPE));
WRAPTYPE dodec_compose ARGS((WRAPTYPE, WRAPTYPE));
void dodec_form_pullback ARGS((REAL *,REAL *,REAL *,WRAPTYPE ));

static void copy(z,w)
REAL z[PDIM],w[PDIM];
{
     int j;

     for (j=0; j<PDIM; j++)
          w[j]=z[j];
}

static void copyMat(mat1, mat2)
      REAL mat1[PDIM][PDIM], mat2[PDIM][PDIM];
{
  int i,j;

  for(i=0;i<PDIM;i++)
     for(j=0;j<PDIM;j++)
        mat2[i][j] = mat1[i][j];
}

static void matMult(mat1, mat2, newMat)
      REAL mat1[PDIM][PDIM], mat2[PDIM][PDIM], newMat[PDIM][PDIM];
{
  int i, j, k;
  REAL temp1[PDIM][PDIM], temp2[PDIM][PDIM];

  copyMat(mat1, temp1);
  copyMat(mat2, temp2);

  for(i=0;i<PDIM;i++)
     for(j=0;j<PDIM;j++) {
        newMat[i][j] = 0;
        for(k=0;k<PDIM;k++)
          newMat[i][j] += temp1[i][k] * temp2[k][j];
     }
}

static void matVecMult(mat, x, y)
      REAL mat[PDIM][PDIM], x[PDIM], y[PDIM];
{
  int i, j;
  REAL temp[PDIM];

  copy(x, temp);
  for(i=0;i<PDIM;i++) {
     y[i] = 0;
     for(j=0;j<PDIM;j++)
        y[i] += temp[j]*mat[j][i];
  }
}

static void convertToHyp(x, y)
      REAL x[PDIM], y[PDIM];
{
  REAL ww;

  ww = 1/sqrt(1 - x[0]*x[0] - x[1]*x[1] - x[2]*x[2]);

  y[0] = x[0]*ww;
  y[1] = x[1]*ww;
  y[2] = x[2]*ww;
  y[3] = ww;
}

/* This function calculates the matrix associated with a given group
    generator represented by a 5-bit integer */
static void calcGen(ggen, mat)
      WRAPTYPE ggen;
      REAL mat[PDIM][PDIM];
{
  int i;
  
  copyMat(ident_mat,mat);
  if(((ggen&16)>>4)==0) {
     for(i=0;i<(ggen&3);i++)
        matMult(mat,centerRotate,mat);
     matMult(mat, rotates[((ggen&12)>>2)], mat);
     for(i=0;i<((5-(ggen&3))%4);i++)
        matMult(mat, centerRotate, mat);
  } else {
     for(i=0;i<(((ggen&3)+3)%4);i++)
        matMult(mat, centerRotate, mat);
     for(i=0;i<3;i++)
        matMult(mat, rotates[((ggen&12)>>2)], mat);
     for(i=0;i<((4-(ggen&3))%4);i++)
        matMult(mat, centerRotate, mat);
  }
}

/* This function calculates the matrix associated with a given group
    genrator represented by an int */
static void calcElem(element, mat)
      WRAPTYPE element;
      REAL mat[PDIM][PDIM];
{
  int i, numGens;
  REAL genMat[PDIM][PDIM];

  numGens = NUMGEN(element);
  element >>= 3;
  copyMat(ident_mat, mat);
  for(i=0;i<numGens;i++) {
     calcGen(FINDGEN(element), genMat);
     matMult(mat, gen[FINDGEN(element)], mat);
     element = CHOP(element);
  }
}

static WRAPTYPE check_inverse(elem1, elem2)
      WRAPTYPE elem1, elem2;
{
  int i;
  int numgen1, numgen2;
  WRAPTYPE pres, next;
  WRAPTYPE endelem;
  WRAPTYPE *gens;

  endelem = 0;
  numgen1=NUMGEN(elem1);
  elem1 >>= 3;
  numgen2=NUMGEN(elem2);
  elem2 >>= 3;
  gens = (WRAPTYPE*) malloc(sizeof(WRAPTYPE)*(numgen1+numgen2));
  for(i=0;i<numgen2;i++) {
     gens[i] = FINDGEN(elem2);
     elem2 = CHOP(elem2);
  }
  for(i=0;i<numgen1;i++) {
     gens[i+numgen2] = FINDGEN(elem1);
     elem1 = CHOP(elem1);
  }
  pres = gens[0];
  for(i=1;i<(numgen1+numgen2);i++) {
     next = gens[i];
     if((pres^next)==16) {
        if(NUMGEN(endelem)==0) {
          if(i<(numgen1+numgen2-1)) {
             pres = gens[i+1];
             i++;
          } else {
             free((char*)gens);
             return(endelem);
          }
        } else {
          pres = FINDGEN(endelem>>(3+5*(NUMGEN(endelem)-1)));
          endelem = (endelem&(~(pres<<(3+5*(NUMGEN(endelem)-1))))) - 1;
        }
     } else {
        endelem += 1 + (pres<<(3+NUMGEN(endelem)*5));
        pres = next;
     }
  }
  endelem += 1 + (pres<<(3+NUMGEN(endelem)*5));
  free((char*)gens);
  return(endelem);
}
     
/* This function returns the inverse of a group element in its integer
    representation. */
WRAPTYPE dodec_inverse(element)
      WRAPTYPE element;
{
  int i, numGens;
  WRAPTYPE inverse;

  numGens = NUMGEN(element);
  element >>= 3;
  inverse = 0;
  for(i=0;i<numGens;i++) {
     inverse = inverse*32 + GENINV(FINDGEN(element));
     element = CHOP(element);
  }
  inverse = (inverse<<3) + numGens;
  return(inverse);
}

/* This function initializes the group generators */
void init_gen()
{
  WRAPTYPE i;

  for(i=0;i<32;i++)
     calcGen(i, gen[i]);

  q_init_flag = 1;
}

/* This function returns the coordinate for a point after and element of
    the group has been applied to it. */
void dodec_wrap(x, y, element)
      REAL x[PDIM], y[PDIM];
      WRAPTYPE element;
{
  REAL mat[PDIM][PDIM];
  int i;

  if(q_init_flag==0) init_gen();
  
  if(element==0)
     for(i=0;i<SDIM;i++)
        y[i] = x[i];
  else {
     convertToHyp(x, y);
     calcElem(element, mat);
     matVecMult(mat, y, y);
     for(i=0;i<SDIM;i++)
        y[i] /= y[3];
  }
}

/* This function composes two elements of the group and returns their integer
    representation */
WRAPTYPE dodec_compose(elem1, elem2)
      WRAPTYPE elem1, elem2;
{
  int numGens;
  WRAPTYPE compp;
  numGens = NUMGEN(elem1);
  if((numGens+NUMGEN(elem2)>5))
     compp = (check_inverse(elem1,elem2));
  else {
     elem1 >>= 3;
     elem1 <<= 3+(NUMGEN(elem2)*5);
     compp = (elem1 + elem2 + numGens);
  }
  if(NUMGEN(compp)>5)
     printf("too many generators \n");

     return(compp);
}

/*******************************************************************
*
*  function: group_form_pullback
*
*  purpose:  Pull back differential forms at vertices that get
*                wrapped.
*
*/

void dodec_form_pullback(x,xform,yform,wrap)
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

  if ( wrap == 0 ) /* just copy */
     { memcpy((char *)xform,(char*)yform,SDIM*sizeof(REAL));
        return;
     }

  calcElem(wrap, trans);

  /* get transformed point in Minkowski */
  memcpy((char *)(w),(char*)x,SDIM*sizeof(REAL));
  w[3] = 1.0;
  matVecMult(trans,w,y);

  /* set up Jacobian */
  for ( i = 0 ; i < SDIM ; i++ )
    for ( j = 0 ; j < SDIM ; j++ )
      jac[i][j] = (trans[j][i] - y[i]*trans[j][3]/y[3])/y[3];

  /* pull back form with transpose of jacobian */
  for ( i = 0 ; i < SDIM ; i++ )
     for ( j = 0, xform[i] = 0. ; j < SDIM ; j++ )
        xform[i] += jac[j][i]*yform[j];

}
