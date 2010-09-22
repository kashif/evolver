/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*********************************************************************
*
*     file:        method3.c
*
*     contents:  quantities for facets, continuation of method2.c
*/

#include "include.h"


/*********************************************************************

                    facet_2form_integral method

Integral of 2-form over facet.  Works in n dim.
2-form components in method instance expr list in lexicographic order,
i.e. 01,02,03,12,13,23.

*********************************************************************/
/*********************************************************************
*
* function: facet_2form_integral_init()
*
* purpose:  Check illegalities
*
*/

void facet_2form_integral_init(mode,mi)
int mode;
struct method_instance *mi;
{
  if ( web.dimension != 2 )
     kb_error(1778,"facet_2form_integral method only for 2D facets.\n",RECOVERABLE);

  if ( web.modeltype == QUADRATIC )
     kb_error(1779,"facet_2form_integral method only for LINEAR and LAGRANGE models.\n",RECOVERABLE);

}

/*********************************************************************
*
* function: facet_2form_integral()
*
* purpose:  method value
*
*/

REAL facet_2form_integral(f_info)
struct qinfo *f_info;
{ int m,i,j,k;
  REAL value=0.0;
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;

  if ( web.modeltype == LAGRANGE ) return facet_2form_integral_lagrange(f_info);

  for ( m = 0 ; m < gauss2D_num ; m++ )
  { 
    f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    for ( i = 0, k = 0 ; i < SDIM ; i++ )
      for ( j = i+1 ; j < SDIM ; j++,k++ )
      { REAL  form = gauss2Dwt[m]*
           eval(METH_INSTANCE(f_info->method)->expr[k],f_info->gauss_pt[m],f_info->id,NULL);
        value += (f_info->sides[0][0][i]*f_info->sides[0][1][j]
                 - f_info->sides[0][0][j]*f_info->sides[0][1][i])*form;
      }
  }
  return sign*value/2;  /* 2 is triangle factor for normal */
}

/*********************************************************************
*
* function: facet_2form_integral_grad()
*
* purpose:  method gradient
*
*/

REAL facet_2form_integral_grad(f_info)
struct qinfo *f_info;
{ int m,n,i,j,k;
  REAL value = 0.0;
  REAL form[MAXCOORD][MAXCOORD]; /* as antisymmetric matrix */
  REAL derivs[MAXCOORD][MAXCOORD][MAXCOORD]; /* coord is last index */
  REAL sum;
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;

  if ( web.modeltype == LAGRANGE )
    return facet_2form_integral_lagrange_grad(f_info);

  for ( i = 0 ; i < SDIM  ; i++ ) form[i][i] = 0.0;
  for ( m = 0 ; m < gauss2D_num ; m++ )
  { REAL weight = sign*gauss2Dwt[m];
    f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    for ( i = 0, k = 0 ; i < SDIM ; i++ ) 
    { for ( n = 0 ; n < SDIM ;n++ ) derivs[i][i][n] = 0.0;
      for ( j = i+1 ; j < SDIM ; j++,k++ ) 
      { eval_all(METH_INSTANCE(f_info->method)->expr[k],f_info->gauss_pt[m],SDIM,
              form[i]+j, derivs[i][j],f_info->id);
        form[j][i] = - form[i][j];
        for ( n = 0 ; n < SDIM ;n++ )
             derivs[j][i][n] = -derivs[i][j][n];
      }
    }
    for ( i = 0, sum = 0.0 ; i < SDIM ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
        sum += f_info->sides[0][0][i]*form[i][j]*f_info->sides[0][1][j];
    value += weight*sum;
    for ( k = 0 ; k < SDIM ; k++ )
    { for ( i = 0, sum = 0.0 ; i < SDIM ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
          sum += f_info->sides[0][0][i]*derivs[i][j][k]*f_info->sides[0][1][j];
      f_info->grad[0][k] += weight*(gauss2Dpt[m][0]*sum
                    - SDIM_dot(form[k],f_info->sides[0][1])
                    + SDIM_dot(form[k],f_info->sides[0][0]))/2;
      f_info->grad[1][k] += weight*(gauss2Dpt[m][1]*sum
                    + SDIM_dot(form[k],f_info->sides[0][1]))/2;
      f_info->grad[2][k] += weight*(gauss2Dpt[m][2]*sum
                    - SDIM_dot(form[k],f_info->sides[0][0]))/2;
     }
  }

  return value/2;
}


/*********************************************************************
*
* function: facet_2form_integral_hess()
*
* purpose:  method gradient and hessian
*
*/

/* derivatives of facet sides with respect to vertices of facet */
int sign1[FACET_VERTS] = { -1, 1, 0 };
int sign2[FACET_VERTS] = { -1, 0, 1 };

REAL facet_2form_integral_hess(f_info)
struct qinfo *f_info;
{ int m,n,i,j,k,p,q;
  REAL value = 0.0;
  REAL form[MAXCOORD][MAXCOORD]; /* as antisymmetric matrix */
  REAL derivs[MAXCOORD][MAXCOORD][MAXCOORD]; /* coord is last index */
  MAT4D(second,MAXCOORD,MAXCOORD,MAXCOORD,MAXCOORD);
  REAL sum;
  REAL *s1 = f_info->sides[0][0];
  REAL *s2 = f_info->sides[0][1];
  REAL s1Fu,s2Fu,Fvs1,Fvs2,s1Fuvs2;
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;

  if ( web.modeltype == LAGRANGE ) 
     return facet_2form_integral_lagrange_hess(f_info);

  for ( i = 0, k = 0 ; i < SDIM ; i++ ) 
  { form[i][i] = 0.0;
     for ( n = 0 ; n < SDIM ;n++ )
     { derivs[i][i][n] = 0.0;
        for ( j = 0 ; j < SDIM ;j++ )
          second[i][i][n][j] = 0.0;
     }
  }
  for ( m = 0 ; m < gauss2D_num ; m++ )
  { REAL weight = sign*gauss2Dwt[m];
    f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    for ( i = 0, k = 0 ; i < SDIM ; i++ ) 
    { for ( j = i+1 ; j < SDIM ; j++,k++ ) 
      { eval_second(METH_INSTANCE(f_info->method)->expr[k],f_info->gauss_pt[m],SDIM,
                         form[i]+j, derivs[i][j],second[i][j],f_info->id);
        form[j][i] = - form[i][j];
        for ( n = 0 ; n < SDIM ;n++ )
           derivs[j][i][n] = -derivs[i][j][n];
        for ( n = 0 ; n < SDIM ;n++ )
           for ( p = 0 ; p < SDIM ;p++ )
             second[j][i][n][p] = -second[i][j][n][p];
      }
    }
    for ( i = 0, sum = 0.0 ; i < SDIM ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
        sum += s1[i]*form[i][j]*s2[j];
    value += weight*sum;
    /* gradient */
    for ( k = 0 ; k < SDIM ; k++ )
    { for ( i = 0, sum = 0.0 ; i < SDIM ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
              sum += s1[i]*derivs[i][j][k]*s2[j];
      f_info->grad[0][k] += weight*(gauss2Dpt[m][0]*sum
                    - SDIM_dot(form[k],s2)
                    + SDIM_dot(form[k],s1))/2;
      f_info->grad[1][k] += weight*(gauss2Dpt[m][1]*sum
                    + SDIM_dot(form[k],s2))/2;
      f_info->grad[2][k] += weight*(gauss2Dpt[m][2]*sum
                    - SDIM_dot(form[k],s1))/2;
    }
    /* hessian */
    for ( p = 0 ; p < SDIM ; p++ )
     for ( q = 0 ; q < SDIM ; q++ )
     { for ( i = 0, s1Fu = 0.0 ; i < SDIM ; i++ )
          s1Fu += s1[i]*derivs[i][q][p];
       for ( i = 0, s2Fu = 0.0 ; i < SDIM ; i++ )
         s2Fu += s2[i]*derivs[i][q][p];
       for ( i = 0, Fvs1 = 0.0 ; i < SDIM ; i++ )
         Fvs1 += derivs[p][i][q]*s1[i];
       for ( i = 0, Fvs2 = 0.0 ; i < SDIM ; i++ )
         Fvs2 += derivs[p][i][q]*s2[i];
       for ( i = 0, s1Fuvs2 = 0.0 ; i < SDIM ; i++ )
         for ( j = 0 ; j < SDIM ; j++ )
            s1Fuvs2 += s1[i]*second[i][j][p][q]*s2[j];
       for ( i = 0 ; i < FACET_VERTS ; i++ )
         for ( j = 0 ; j < FACET_VERTS ; j++ )
            f_info->hess[i][j][p][q] += weight*(
                   sign1[i]*form[p][q]*sign2[j] - sign2[i]*form[p][q]*sign1[j]
                     + gauss2Dpt[m][i]*(s1Fu*sign2[j] - s2Fu*sign1[j])
                     + gauss2Dpt[m][j]*(sign1[i]*Fvs2 - sign2[i]*Fvs1)
                     + gauss2Dpt[m][i]*gauss2Dpt[m][j]*s1Fuvs2 )/2;
     }
  }

  return value/2;
}

/******************************************************************************
                         facet_2form_integral Lagrangian model
******************************************************************************/

/*********************************************************************
*
* function: facet_2form_integral_lagrange()
*
* purpose:  method value
*
*/

REAL facet_2form_integral_lagrange(f_info)
struct qinfo *f_info;
{ int m,i,j,k;
  REAL value=0.0,form;
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  int dim = web.dimension;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];

  for ( i = 0, k = 0 ; i < SDIM ; i++ )
     for ( j = i+1 ; j < SDIM ; j++,k++ )
     { for ( m = 0 ; m < gl->gnumpts ; m++ )
       {
          f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
          form = gl->gausswt[m]*
             eval(METH_INSTANCE(f_info->method)->expr[k],f_info->gauss_pt[m],f_info->id,NULL);
          value += (f_info->sides[m][0][i]*f_info->sides[m][1][j]
                 - f_info->sides[m][0][j]*f_info->sides[m][1][i])*form;
        }
     }
  return sign*value/2;  /* 2 is triangle factor */
}

/*********************************************************************
*
* function: facet_2form_integral_lagrange_grad()
*
* purpose:  method gradient
*
*/


REAL facet_2form_integral_lagrange_grad(f_info)
struct qinfo *f_info;
{ int m,n,i,j,k;
  REAL value = 0.0;
  REAL form[MAXCOORD][MAXCOORD]; /* as antisymmetric matrix */
  REAL derivs[MAXCOORD][MAXCOORD][MAXCOORD]; /* coord is last index */
  REAL sum;
  int dim = web.dimension;
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];
  int ctrl = web.skel[FACET].ctrlpts;

  for ( i = 0 ; i < SDIM  ; i++ ) 
  { form[i][i] = 0.0;
    for ( n = 0 ; n < SDIM ;n++ ) derivs[i][i][n] = 0.0;
  }
  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL weight = sign*gl->gausswt[m]/2;
    f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    for ( i = 0, k = 0 ; i < SDIM ; i++ ) 
    { for ( j = i+1 ; j < SDIM ; j++,k++ ) 
      { eval_all(METH_INSTANCE(f_info->method)->expr[k],f_info->gauss_pt[m],SDIM,
              form[i]+j, derivs[i][j],f_info->id);
        form[j][i] = - form[i][j];
        for ( n = 0 ; n < SDIM ;n++ )
           derivs[j][i][n] = -derivs[i][j][n];
      }
    }
    for ( i = 0, sum = 0.0 ; i < SDIM ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
     {
        sum += f_info->sides[m][0][i]*form[i][j]*f_info->sides[m][1][j];
        for ( n = 0 ; n < ctrl ; n++ )
        {
          f_info->grad[n][i] += weight*gl->gpolypart[m][0][n]*form[i][j]
                      *f_info->sides[m][1][j];
          for (  k= 0 ; k < SDIM ; k++ )
                  f_info->grad[n][k] += weight*f_info->sides[m][0][i]
                      *gl->gpoly[m][n]*derivs[i][j][k]*f_info->sides[m][1][j];
          f_info->grad[n][j] += weight*f_info->sides[m][0][i]*form[i][j]
                      *gl->gpolypart[m][1][n];
        } 
     }
    value += weight*sum;
  }

  return value;
}


/*********************************************************************
*
* function: facet_2form_integral_lagrange_hess()
*
* purpose:  method gradient and hessian
*
*/

REAL facet_2form_integral_lagrange_hess(f_info)
struct qinfo *f_info;
{ int m,n,i,j,k,p;
  REAL value = 0.0;
  REAL form[MAXCOORD][MAXCOORD]; /* as antisymmetric matrix */
  REAL derivs[MAXCOORD][MAXCOORD][MAXCOORD]; /* coord is last index */
  MAT4D(second,MAXCOORD,MAXCOORD,MAXCOORD,MAXCOORD);
  REAL sum;
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  int dim = web.dimension;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];
  int ctrl = web.skel[FACET].ctrlpts;

  for ( i = 0; i < SDIM ; i++ )
  { form[i][i] = 0.0;
     for ( n = 0 ; n < SDIM ;n++ )
     { derivs[i][i][n] = 0.0;
        for ( j = 0 ; j < SDIM ;j++ )
          second[i][i][n][j] = 0.0;
     }
  }
  for ( m = 0 ; m < gl->gnumpts ; m++ )
    { REAL weight = sign*gl->gausswt[m]/2;
      f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
      for ( i = 0, k = 0 ; i < SDIM ; i++ ) 
        { for ( j = i+1 ; j < SDIM ; j++,k++ ) 
          { eval_second(METH_INSTANCE(f_info->method)->expr[k],f_info->gauss_pt[m],
                SDIM, form[i]+j, derivs[i][j],second[i][j],f_info->id);
             form[j][i] = - form[i][j];
             for ( n = 0 ; n < SDIM ;n++ )
                derivs[j][i][n] = -derivs[i][j][n];
             for ( n = 0 ; n < SDIM ;n++ )
                for ( p = 0 ; p < SDIM ;p++ )
                  second[j][i][n][p] = -second[i][j][n][p];
          }
        }


        for ( i = 0, sum = 0.0 ; i < SDIM ; i++ )
         for ( j = 0 ; j < SDIM ; j++ )
         {
            sum += f_info->sides[m][0][i]*form[i][j]*f_info->sides[m][1][j];
            for ( n = 0 ; n < ctrl ; n++ )
            { int nn,kk;
              /* gradient terms */
              f_info->grad[n][i] += weight*gl->gpolypart[m][0][n]*form[i][j]
                      *f_info->sides[m][1][j];
              for (  k= 0 ; k < SDIM ; k++ )
                  f_info->grad[n][k] += weight*f_info->sides[m][0][i]
                      *gl->gpoly[m][n]*derivs[i][j][k]*f_info->sides[m][1][j];
              f_info->grad[n][j] += weight*f_info->sides[m][0][i]*form[i][j]
                      *gl->gpolypart[m][1][n];

              /* hessian terms */
              for ( nn = 0; nn < ctrl ; nn++ )
              { 
                 for ( kk = 0 ; kk < SDIM ; kk++ )
                     f_info->hess[n][nn][i][kk] += weight*gl->gpolypart[m][0][n]
                        *gl->gpoly[m][nn]*derivs[i][j][kk]*f_info->sides[m][1][j];
                 f_info->hess[n][nn][i][j] += weight*gl->gpolypart[m][0][n]
                    *form[i][j]*gl->gpolypart[m][1][nn];
                 for (  k= 0 ; k < SDIM ; k++ )
                 { f_info->hess[n][nn][k][i] += weight*gl->gpolypart[m][0][nn]
                      *gl->gpoly[m][n]*derivs[i][j][k]*f_info->sides[m][1][j];
                    for ( kk = 0 ; kk < SDIM ; kk++ )
                        f_info->hess[n][nn][k][kk] += weight*f_info->sides[m][0][i]
                          *gl->gpoly[m][n]*second[i][j][k][kk]*gl->gpoly[m][nn]
                             *f_info->sides[m][1][j];
                    f_info->hess[n][nn][k][j] += weight*f_info->sides[m][0][i]
                      *gl->gpoly[m][n]*derivs[i][j][k]*gl->gpolypart[m][1][nn];
                 }
                 f_info->hess[n][nn][j][i] += weight*gl->gpolypart[m][0][nn]
                     *form[i][j]*gl->gpolypart[m][1][n];
                 for ( kk = 0 ; kk < SDIM ; kk++ )
                     f_info->hess[n][nn][j][kk] += weight*f_info->sides[m][0][i]
                         *derivs[i][j][kk]*gl->gpoly[m][nn]*gl->gpolypart[m][1][n];
              } 

            } 
         }
        value += weight*sum;
     }

  return value;
}

/*********************************************************************

                    facet_2form_sq_integral method

Integral of facetwise square of 2-form over facet.  Works in n dim.
2-form components in method instance expr list in lexicographic order,
i.e. 01,02,03,12,13,23.  For Andy Hanson's symplectic area minimization.

*********************************************************************/
/*********************************************************************
*
* function: facet_2form_sq_integral_init()
*
* purpose:  Check illegalities
*
*/

void facet_2form_sq_integral_init(mode,mi)
int mode;
struct method_instance *mi;
{
  if ( web.dimension != 2 )
     kb_error(2412,"facet_2form_sq_integral method only for 2D facets.\n",RECOVERABLE);

  if ( web.modeltype != LINEAR )
     kb_error(2413,"facet_2form_sq_integral method only for LINEAR model.\n",RECOVERABLE);

}

/*********************************************************************
*
* function: facet_2form_sq_integral()
* 
* purpose:  method value
*
*/

REAL facet_2form_sq_integral(f_info)
struct qinfo *f_info;
{ int m,i,j,k;
  REAL value=0.0;

  for ( m = 0 ; m < gauss2D_num ; m++ )
  { 
    f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    for ( i = 0, k = 0 ; i < SDIM ; i++ )
      for ( j = i+1 ; j < SDIM ; j++,k++ )
      { REAL  form = gauss2Dwt[m]*
         eval(METH_INSTANCE(f_info->method)->expr[k],f_info->gauss_pt[m],
            f_info->id,NULL);
        value += (f_info->sides[0][0][i]*f_info->sides[0][1][j]
                 - f_info->sides[0][0][j]*f_info->sides[0][1][i])*form;
      }
  }
  return value*value/4;  /* 2 is triangle factor for normal */
}

/*********************************************************************
*
* function: facet_2form_sq_integral_grad()
*
* purpose:  method gradient
*
*/

REAL facet_2form_sq_integral_grad(f_info)
struct qinfo *f_info;
{ int m,n,i,j,k;
  REAL value = 0.0;
  REAL form[MAXCOORD][MAXCOORD]; /* as antisymmetric matrix */
  REAL derivs[MAXCOORD][MAXCOORD][MAXCOORD]; /* coord is last index */
  REAL sum;

  for ( i = 0 ; i < SDIM  ; i++ ) form[i][i] = 0.0;
  for ( m = 0 ; m < gauss2D_num ; m++ )
  { REAL weight = gauss2Dwt[m];
    f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    for ( i = 0, k = 0 ; i < SDIM ; i++ ) 
    { for ( n = 0 ; n < SDIM ;n++ ) derivs[i][i][n] = 0.0;
      for ( j = i+1 ; j < SDIM ; j++,k++ ) 
      { eval_all(METH_INSTANCE(f_info->method)->expr[k],f_info->gauss_pt[m],SDIM,
              form[i]+j, derivs[i][j],f_info->id);
        form[j][i] = - form[i][j];
        for ( n = 0 ; n < SDIM ;n++ )
             derivs[j][i][n] = -derivs[i][j][n];
      }
    }
    for ( i = 0, sum = 0.0 ; i < SDIM ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
        sum += f_info->sides[0][0][i]*form[i][j]*f_info->sides[0][1][j];
    value += weight*sum;
    for ( k = 0 ; k < SDIM ; k++ )
    { for ( i = 0, sum = 0.0 ; i < SDIM ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
          sum += f_info->sides[0][0][i]*derivs[i][j][k]*f_info->sides[0][1][j];
      f_info->grad[0][k] += weight*(gauss2Dpt[m][0]*sum
                    - SDIM_dot(form[k],f_info->sides[0][1])
                    + SDIM_dot(form[k],f_info->sides[0][0]))/2;
      f_info->grad[1][k] += weight*(gauss2Dpt[m][1]*sum
                    + SDIM_dot(form[k],f_info->sides[0][1]))/2;
      f_info->grad[2][k] += weight*(gauss2Dpt[m][2]*sum
                    - SDIM_dot(form[k],f_info->sides[0][0]))/2;
    }
  }
  /* adjust grad for being the square */
  for ( i = 0 ; i < FACET_VERTS ; i++ )
    for ( k = 0 ; k < SDIM ; k++ )
      f_info->grad[i][k] *= value;

  return value*value/4;
}


/******************************************************************************
  
         Soapfilm Gravity quantity stuff

******************************************************************************/


/***************************************************************
*
*  function: gravity_init()
*
*  purpose: initialization for gravity method
*/

void gravity_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{
  /* method modulus is gravitation constant */
  if ( gravity_quantity_num >= 0 )
     GEN_QUANT(gravity_quantity_num)->modulus = 
         web.gravflag ? web.grav_const : 0.0;
}

/**********************************************************************************
*
*  function: full_gravity_init()
*
*  purpose: initialization for gravity method using built-in G and body densities
*/

void full_gravity_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{
  /* method modulus is gravitation constant */
  mi->flags &= FAKE_IMPLICIT;
  GEN_QUANT(gravity_quantity_num)->modulus = 
         web.gravflag ? web.grav_const : 0.0;
}

/**************************************************************
*
*  function: gravity_all()
*
*  purpose: calculates value, gradient, and hessian of one 
*        facet due to gravitational potential.
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL gravity_all_q ARGS((struct qinfo *,int));
REAL gravity_all_lagrange ARGS((struct qinfo *,int));

REAL gravity_all(f_info,mode)
struct qinfo *f_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */
{
  int i,j;
  REAL jac; /* jacobian */
  REAL sum = 0.0;  /* quadratic z sum of facet */
  REAL djdx[MAXCOORD],djdy[MAXCOORD],dsdz[MAXCOORD];
  REAL c = 1/24.;  /* coefficient */
  body_id b_id;
  REAL gdensity;

  if ( web.modeltype == QUADRATIC ) return gravity_all_q(f_info,mode);
  if ( web.modeltype == LAGRANGE )  
      return gravity_all_lagrange(f_info,mode);

  b_id = get_facet_body(f_info->id);
  if ( METH_INSTANCE(f_info->method)->flags & (IMPLICIT_INSTANCE|FAKE_IMPLICIT) )
  { /* include density difference */
     gdensity = 0.0;
     if ( valid_id(b_id) )
          gdensity += get_body_density(b_id);
     b_id = get_facet_body(facet_inverse(f_info->id));
     if ( valid_id(b_id) )
          gdensity -= get_body_density(b_id);
     if ( gdensity == 0.0 )
        return 0.0;
     c *= gdensity;
  }

  jac = f_info->sides[0][0][0]*f_info->sides[0][1][1] 
            - f_info->sides[0][1][0]*f_info->sides[0][0][1]; 
  for ( i = 0 ; i < f_info->vcount ; i++ )
    for ( j = 0 ; j <= i ; j++ )
      sum += f_info->x[i][2]*f_info->x[j][2];

  if ( mode == METHOD_VALUE ) return jac*sum*c;

  djdx[0] = (-f_info->sides[0][1][1] + f_info->sides[0][0][1]);
  djdx[1] = f_info->sides[0][1][1];
  djdx[2] = -f_info->sides[0][0][1];
  djdy[0] = (-f_info->sides[0][0][0] + f_info->sides[0][1][0]);
  djdy[1] = -f_info->sides[0][1][0];
  djdy[2] = f_info->sides[0][0][0];
  dsdz[0] = 2*f_info->x[0][2]+f_info->x[1][2]+f_info->x[2][2];
  dsdz[1] = f_info->x[0][2]+2*f_info->x[1][2]+f_info->x[2][2];
  dsdz[2] = f_info->x[0][2]+f_info->x[1][2]+2*f_info->x[2][2];

  for ( i = 0 ; i < 3;  i++ )
  { f_info->grad[i][0] = djdx[i]*sum*c;
    f_info->grad[i][1] = djdy[i]*sum*c;
    f_info->grad[i][2] = dsdz[i]*jac*c;
  }

  if ( mode == METHOD_GRADIENT ) return jac*sum*c;

  /* second partials, self */
  for ( i = 0 ; i < 3;  i++ )
  { f_info->hess[i][i][0][2] = f_info->hess[i][i][2][0] = djdx[i]*dsdz[i]*c;
    f_info->hess[i][i][1][2] = f_info->hess[i][i][2][1] = djdy[i]*dsdz[i]*c; 
    f_info->hess[i][i][2][2] = 2*jac*c;
    f_info->hess[i][i][0][0] = f_info->hess[i][i][0][1] 
        = f_info->hess[i][i][1][0] = f_info->hess[i][i][1][1] = 0.0;
  }

  /* second partials, mixed */
  for ( i = 0 ; i < 3 ; i++ )
    for ( j = 0 ; j < 3 ; j++ )
     { if ( j == i ) continue;
        f_info->hess[i][j][0][0] = f_info->hess[i][j][1][1] = 0.0;
        f_info->hess[i][j][0][1] = (j==((i+1)%3)) ? sum*c : -sum*c;
        f_info->hess[i][j][1][0] = (j==((i+1)%3)) ? -sum*c : sum*c;
        f_info->hess[i][j][0][2] = djdx[i]*dsdz[j]*c;
        f_info->hess[i][j][2][0] = djdx[j]*dsdz[i]*c;
        f_info->hess[i][j][1][2] = djdy[i]*dsdz[j]*c;
        f_info->hess[i][j][2][1] = djdy[j]*dsdz[i]*c;
        f_info->hess[i][j][2][2] = jac*c;
     }
  return jac*sum*c;
}

/**************************************************************
*
*  function: gravity_all_q()
*
*  purpose: calculates value, gradient, and hessian of one 
*        facet due to gravitational potential.  Quadratic model.
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL gravity_all_q(f_info,mode)
struct qinfo *f_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */
{
  int k,kk,m;
  REAL jac; /* jacobian */
  REAL **x,**tang;
  REAL **g=NULL,****h=NULL;
  REAL value = 0.0;
  body_id b_id;
  REAL gdensity = 1.0;

  x = f_info->x;

  if ( mode == METHOD_GRADIENT ) 
  { g = f_info->grad;
  }
  else if ( mode == METHOD_HESSIAN ) 
  { g = f_info->grad;
    h = f_info->hess;
  }

  if ( METH_INSTANCE(f_info->method)->flags & (IMPLICIT_INSTANCE|FAKE_IMPLICIT) )
  { /* include density difference */
     b_id = get_facet_body(f_info->id);
     gdensity = 0.0;
     if ( valid_id(b_id) )
          gdensity += get_body_density(b_id);
     b_id = get_facet_body(facet_inverse(f_info->id));
     if ( valid_id(b_id) )
          gdensity -= get_body_density(b_id);
     if ( gdensity == 0.0 ) return 0.0;
  }

  for ( m = 0 ; m < gauss2D_num ; m++ )
   { REAL weight = gdensity*gauss2Dwt[m]/2;
     REAL z;

      for ( k = 0, z = 0.0 ; k < ctrl_num ; k++ )
              z += gpoly[m][k]*x[k][2];
      tang = f_info->sides[m];
      jac = tang[0][0]*tang[1][1] - tang[0][1]*tang[1][0];
      value += weight*z*z*jac/2;

      if ( mode == METHOD_VALUE ) continue;

      for ( k = 0 ; k < ctrl_num ; k++ )
      { g[k][0] += weight*z*z/2*(gpolypartial[m][0][k]*tang[1][1]
                                              - tang[0][1]*gpolypartial[m][1][k]);
         g[k][1] += weight*z*z/2*(tang[0][0]*gpolypartial[m][1][k]
                                              - gpolypartial[m][0][k]*tang[1][0]);
         g[k][2] += weight*z*gpoly[m][k]*jac;
      }  

      if ( mode == METHOD_GRADIENT ) continue;

      /* hessian */
      for ( k = 0 ; k < ctrl_num ; k++ )
        for ( kk = 0 ; kk < ctrl_num ; kk++ )
      { 
         h[k][kk][0][1] += weight*z*z/2*(gpolypartial[m][0][k]*gpolypartial[m][1][kk]
                         - gpolypartial[m][0][kk]*gpolypartial[m][1][k]);
         h[k][kk][0][2] += weight*z*gpoly[m][kk]*(gpolypartial[m][0][k]*tang[1][1]
                         - tang[0][1]*gpolypartial[m][1][k]);
         h[k][kk][1][0] += weight*z*z/2*(gpolypartial[m][0][kk]*gpolypartial[m][1][k]
                         - gpolypartial[m][0][k]*gpolypartial[m][1][kk]);
         h[k][kk][1][2] += weight*z*gpoly[m][kk]*(tang[0][0]*gpolypartial[m][1][k]
                         - gpolypartial[m][0][k]*tang[1][0]);
         h[k][kk][2][0] += weight*z*gpoly[m][k]*(gpolypartial[m][0][kk]*tang[1][1]
                        - tang[0][1]*gpolypartial[m][1][kk]);
         h[k][kk][2][1] += weight*z*gpoly[m][k]*(tang[0][0]*gpolypartial[m][1][kk]
                        - gpolypartial[m][0][kk]*tang[1][0]);
         h[k][kk][2][2] += weight*gpoly[m][kk]*gpoly[m][k]*jac;
      }  

  }
  return value;
}


/**************************************************************
*
*  function: gravity_all_lagrange()
*
*  purpose: calculates value, gradient, and hessian of one 
*        facet due to gravitational potential.  Lagrange model.
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL gravity_all_lagrange(f_info,mode)
struct qinfo *f_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */
{
  REAL value = 0.0;
  body_id b_id;
  REAL gdensity = 1.0;
  int i,m,j,k,jj,ii,kk;
  REAL z;
  REAL val;
  MAT2D(mat,MAXCOORD,MAXCOORD);
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  int dim = web.dimension;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];
  MAT4D(dethess,MAXCOORD,MAXCOORD,MAXCOORD,MAXCOORD);

  if ( METH_INSTANCE(f_info->method)->flags & (IMPLICIT_INSTANCE|FAKE_IMPLICIT) )
  { /* include density difference */
     b_id = get_facet_body(f_info->id);
     gdensity = 0.0;
     if ( valid_id(b_id) )
          gdensity += get_body_density(b_id);
     b_id = get_facet_body(facet_inverse(f_info->id));
     if ( valid_id(b_id) )
          gdensity -= get_body_density(b_id);
     if ( gdensity == 0.0 ) return 0.0;
  }

  for ( j = 0 ; j < SDIM ; j++ )
        mat[web.dimension][j] = 0.0;

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL det;
     REAL weight = sign*gdensity*gl->gausswt[m]/factorial[dim];
     for ( i = 0 ; i < web.dimension ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
          mat[i][j] = f_info->sides[m][i][j];  /* mat destroyed by det */
     z = f_info->gauss_pt[m][SDIM-1];
     val = z*z/2;
     mat[web.dimension][SDIM-1] = val;
     if ( mode == METHOD_HESSIAN ) det_hess(mat,dethess,SDIM);
     det = det_adjoint(mat,SDIM);

     value += weight*det;
     
     if ( mode == METHOD_VALUE ) continue;

     /* gradient */
     for ( k = 0 ; k < gl->lagpts; k++ )
      for ( j = 0 ; j < SDIM ; j++ ) 
        for ( i = 0 ; i < dim ; i++ )
        { f_info->grad[k][j] += weight*gl->gpolypart[m][i][k]*mat[j][i];
        }
     for ( k = 0 ; k < gl->lagpts ; k++ )
          f_info->grad[k][SDIM-1] += weight*gl->gpoly[m][k]*z*mat[SDIM-1][dim];

     if ( mode == METHOD_GRADIENT ) continue;

     /* hessian */
     for ( k = 0 ; k < gl->lagpts  ; k++ )
        for ( kk = 0 ; kk < gl->lagpts ; kk++ )
        { REAL h;
          for ( j = 0 ; j < SDIM ; j++ )
             for ( jj = 0 ; jj < SDIM ; jj++ )
             { h = 0.0;
                for ( i = 0 ; i < dim ; i++ )
                 for ( ii  = 0 ; ii < dim ; ii++ )
                 { h += dethess[i][j][ii][jj]
                            *gl->gpolypart[m][i][k]*gl->gpolypart[m][ii][kk];
                 }
                f_info->hess[k][kk][j][jj]  += weight*h;
             }

          for ( j = 0 ; j < SDIM ; j++ )
             { h = 0.0;
                for ( i = 0 ; i < dim ; i++ )
                  h += dethess[i][j][dim][SDIM-1]*gl->gpolypart[m][i][k]
                              *gl->gpoly[m][kk]*z;
                f_info->hess[k][kk][j][SDIM-1]  += weight*h;
             }
          for ( jj = 0 ; jj < SDIM ; jj++ )
             { h = 0.0;
                for ( ii = 0 ; ii < dim ; ii++ )
                  h += dethess[dim][SDIM-1][ii][jj]*gl->gpolypart[m][ii][kk]
                              *gl->gpoly[m][k]*z;
                f_info->hess[k][kk][SDIM-1][jj]  += weight*h;
             }
          h = gl->gpoly[m][k]*gl->gpoly[m][kk]*mat[SDIM-1][web.dimension];
          f_info->hess[k][kk][SDIM-1][SDIM-1]  += weight*h;
         }
  }

  return value;  
}



/**************************************************************
*
*  function: gravity_energy()
*
*  purpose: calculates energy of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL gravity_energy(f_info)
struct qinfo *f_info;
{
 return gravity_all(f_info,METHOD_VALUE);
}



/**************************************************************
*
*  function: gravity_grads()
*
*  purpose: calculates gradient of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL gravity_grads(f_info)
struct qinfo *f_info;
{
 return gravity_all(f_info,METHOD_GRADIENT);
}


/**************************************************************
*
*  function: gravity_hessian()
*
*  purpose: calculates hessian of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL gravity_hessian(f_info)
struct qinfo *f_info;
{
 return gravity_all(f_info,METHOD_HESSIAN);
}

/******************************************************************
*
*  function: pos_area_hess()
*
*  purpose: facet area hessian adjustable to force it to be
*              positive definite.
*
*/

REAL gga_coeff; /* term coefficient for part of hessian */
REAL fgagfa_coeff; /* term coefficient for part of hessian */
REAL gfa_2_coeff; /* term coefficient for part of hessian */
REAL gfagfa_coeff; /* term coefficient for neg part of hessian */
REAL gfaafg_coeff; /* term coefficient for correction */

void pos_area_hess_init(mode,mi)
int mode;
struct method_instance *mi;
{ int k;

  k = lookup_global("gga_coeff");
  if ( k >= 0 )
      gga_coeff = globals(k)->value.real;
  else gga_coeff = 1.0;

  k = lookup_global("fgagfa_coeff");
  if ( k >= 0 )
      fgagfa_coeff = globals(k)->value.real;
  else fgagfa_coeff = -1.0;

  k = lookup_global("gfa_2_coeff");
  if ( k >= 0 )
      gfa_2_coeff = globals(k)->value.real;
  else gfa_2_coeff = 1.0;

  k = lookup_global("gfagfa_coeff");
  if ( k >= 0 )
      gfagfa_coeff = globals(k)->value.real;
  else gfagfa_coeff = -1.0;

  k = lookup_global("gfaafg_coeff");
  if ( k >= 0 )
      gfaafg_coeff = globals(k)->value.real;
  else gfaafg_coeff = 0.0;

}

REAL pos_area_hess(f_info)
struct qinfo *f_info;
{
  MAT2D(Ainv,MAXCOORD,MAXCOORD);
  MAT2D(FtAinv,MAXCOORD,MAXCOORD);
  MAT2D(FtAF,MAXCOORD,MAXCOORD);
  MAT2D(FtAAF,MAXCOORD,MAXCOORD);
  int i,j,m,n;
  int dim = web.dimension;
  REAL area;

  mat_mul_tr(f_info->sides[0],f_info->sides[0],Ainv,dim,SDIM,dim);
  area = det_adjoint(Ainv,dim);
  if ( area == 0.0 ) return area;
  for ( i = 0 ; i < dim ; i++ )  /* get true inverse */
     for ( j = 0 ; j < dim  ; j++ ) 
        Ainv[i][j] /= area;
  area = sqrt(area)/web.simplex_factorial;  /* simplex area */
  tr_mat_mul(f_info->sides[0],Ainv,FtAinv,dim,SDIM,dim);
  mat_mult(FtAinv,f_info->sides[0],FtAF,SDIM,dim,SDIM);
  mat_mul_tr(FtAinv,FtAinv,FtAAF,SDIM,dim,SDIM);

  /* gradient */
  for ( i = 0 ; i < dim ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
     { f_info->grad[i+1][j] = FtAinv[j][i]*area;
        f_info->grad[0][j] -= FtAinv[j][i]*area;
     }

  /* approximate hessian */
  for ( m = 0 ; m < dim ; m++ )
     for ( n = 0 ; n < dim ; n++ )
        for ( i = 0 ; i < SDIM ; i++ )
          for ( j = 0 ; j < SDIM ; j++ )
          { REAL val;
             val = (i==j) ? gga_coeff*Ainv[m][n] : 0.0;
             val += gfagfa_coeff*FtAinv[i][n]*FtAinv[j][m]; 
             val += fgagfa_coeff*FtAF[i][j]*Ainv[m][n];
             val += (m==n) ? gfaafg_coeff*FtAAF[i][j] : 0.0;
             val += gfa_2_coeff*FtAinv[i][m]*FtAinv[j][n];
             val *= area;
             f_info->hess[m+1][n+1][i][j] += val;
             f_info->hess[0][n+1][i][j] -= val;
             f_info->hess[m+1][0][i][j] -= val;
             f_info->hess[0][0][i][j] += val;
          }
  return area;
}

/********************************************************************
      Sobolev area hessian, after Renka and Neuberger
*********************************************************************/

void sobolev_area_init(mode,mi)
int mode;
struct method_instance *mi;
{
  if ( web.modeltype != LINEAR ) 
     kb_error(2146,"Sobolev_area only for LINEAR model.\n",RECOVERABLE);
}

REAL sobolev_area_hess(f_info)
struct qinfo *f_info;
{
  MAT2D(Ainv,MAXCOORD,MAXCOORD);
  MAT2D(FtAinv,MAXCOORD,MAXCOORD);
  MAT2D(FtAF,MAXCOORD,MAXCOORD);
  int i,j,m,n;
  int dim = web.dimension;
  REAL area;

  mat_mul_tr(f_info->sides[0],f_info->sides[0],Ainv,dim,SDIM,dim);
  area = det_adjoint(Ainv,dim);
  if ( area == 0.0 ) return area;
  for ( i = 0 ; i < dim ; i++ )  /* get true inverse */
     for ( j = 0 ; j < dim  ; j++ ) 
        Ainv[i][j] /= area;
  area = sqrt(area)/web.simplex_factorial;  /* simplex area */
/*  area *= get_facet_density(f_info->id); */
  tr_mat_mul(f_info->sides[0],Ainv,FtAinv,dim,SDIM,dim);
  mat_mult(FtAinv,f_info->sides[0],FtAF,SDIM,dim,SDIM);

  /* gradient */
  for ( i = 0 ; i < dim ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
     { f_info->grad[i+1][j] = FtAinv[j][i]*area;
        f_info->grad[0][j] -= FtAinv[j][i]*area;
     }

  /* approximate hessian */
  for ( m = 0 ; m < dim ; m++ )
     for ( n = 0 ; n < dim ; n++ )
        for ( i = 0 ; i < SDIM ; i++ )
          for ( j = 0 ; j < SDIM ; j++ )
          { REAL val;
             val = (i==j) ? Ainv[m][n] : 0.0;
             val -= FtAF[i][j]*Ainv[m][n];
             val += FtAinv[i][m]*FtAinv[j][n];
             val *= area;
             f_info->hess[m+1][n+1][i][j] += val;
             f_info->hess[0][n+1][i][j] -= val;
             f_info->hess[m+1][0][i][j] -= val;
             f_info->hess[0][0][i][j] += val;
          }
  return area;
}


/********************************************************************
      Dirichelt area hessian, after Polthier and Pinkall
*********************************************************************/
void dirichlet_area_init(mode,mi)
int mode;
struct method_instance *mi;
{
  if ( web.modeltype != LINEAR ) 
     kb_error(2147,"dirichlet_area only for LINEAR model.\n",RECOVERABLE);
}

REAL dirichlet_area_hess(f_info)
struct qinfo *f_info;
{
  MAT2D(Ainv,MAXCOORD,MAXCOORD);
  MAT2D(FtAinv,MAXCOORD,MAXCOORD);
  MAT2D(FtAF,MAXCOORD,MAXCOORD);
  int i,j,m,n;
  int dim = web.dimension;
  REAL area;

  mat_mul_tr(f_info->sides[0],f_info->sides[0],Ainv,dim,SDIM,dim);
  area = det_adjoint(Ainv,dim);
  if ( area == 0.0 ) return area;
  for ( i = 0 ; i < dim ; i++ )  /* get true inverse */
     for ( j = 0 ; j < dim  ; j++ ) 
        Ainv[i][j] /= area;
  area = sqrt(area)/web.simplex_factorial;  /* simplex area */
/*  area *= get_facet_density(f_info->id); */
  tr_mat_mul(f_info->sides[0],Ainv,FtAinv,dim,SDIM,dim);
  mat_mult(FtAinv,f_info->sides[0],FtAF,SDIM,dim,SDIM);

  /* gradient */
  for ( i = 0 ; i < dim ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
     { f_info->grad[i+1][j] = FtAinv[j][i]*area;
        f_info->grad[0][j] -= FtAinv[j][i]*area;
     }

  /* approximate hessian */
  for ( m = 0 ; m < dim ; m++ )
     for ( n = 0 ; n < dim ; n++ )
        for ( i = 0 ; i < SDIM ; i++ )
          { REAL val;
             val = Ainv[m][n];
             val *= area;
             f_info->hess[m+1][n+1][i][i] += val;
             f_info->hess[0][n+1][i][i] -= val;
             f_info->hess[m+1][0][i][i] -= val;
             f_info->hess[0][0][i][i] += val;
          }
  return area;
}

/*********************************************************************

                    stress_integral method

*********************************************************************/

/*********************************************************************
*
* function: stress_integral_init()
*
* purpose:  Check illegalities
*
*/

void stress_integral_init(mode,mi)
int mode;
struct method_instance *mi;
{
  if ( web.dimension != 2 )
     kb_error(1781,"stress_integral method only for 2D facets.\n",RECOVERABLE);

  if ( SDIM != 3 )
     kb_error(1782,"stress_integral method only for 3D space.\n",RECOVERABLE);

  if ( web.modeltype != LINEAR )
     kb_error(1783,"stress_integral method only for LINEAR model.\n",RECOVERABLE);

}
/*********************************************************************
*
* function: stress_integral()
*
* purpose:  method value
*
*/

REAL stress_integral(f_info)
struct qinfo *f_info;
{ REAL value = 0.0;
  REAL area;
  REAL ss,st,tt;
  int stress_comp;
  REAL un[MAXCOORD];
  REAL density = get_facet_density(f_info->id);
  ss = SDIM_dot(f_info->sides[0][0],f_info->sides[0][0]);
  st = SDIM_dot(f_info->sides[0][0],f_info->sides[0][1]);
  tt = SDIM_dot(f_info->sides[0][1],f_info->sides[0][1]);
  area = sqrt(ss*tt-st*st)/2;
  un[0] = f_info->normal[0]/(2*area);
  un[1] = f_info->normal[1]/(2*area);
  un[2] = f_info->normal[2]/(2*area);

  stress_comp = (int)eval(METH_INSTANCE(f_info->method)->expr[0],NULL,f_info->id,NULL);

  switch( stress_comp ) {
  case 11:
     value = (REAL)1 - un[0]*un[0]; break;
  case 12:
     value =  - un[0]*un[1]; break;
  case 13:
     value =  - un[0]*un[2]; break;
  case 22:
     value = (REAL)1 - un[1]*un[1]; break;
  case 23:
     value =  - un[1]*un[2]; break;
  case 33:
     value = (REAL)1 - un[2]*un[2]; break;
  case 10:
     value = - un[1]*un[1] + un[2]*un[2]; break;
  case 20:
     value = - un[2]*un[2] + un[0]*un[0]; break;
  case 30:
     value = - un[0]*un[0] + un[1]*un[1]; break;
  case 40:
     value = (REAL)3 - un[0]*un[0] - un[1]*un[1] - un[2]*un[2]; break;
  case 120:
     value = - un[0]*un[0] + un[1]*un[1]; break;
  case 130:
     value = - un[0]*un[0] + un[2]*un[2]; break;
  case 230:
     value = - un[1]*un[1] + un[2]*un[2]; break;
  case 210:
     value = - un[1]*un[1] + un[0]*un[0]; break;
  case 310:
     value = - un[2]*un[2] + un[0]*un[0]; break;
  case 320:
     value = - un[2]*un[2] + un[1]*un[1]; break;
  default:
     ;
  }
  value *= area;
  value *= density;
  return value;
}

/*********************************************************************
*
* function: stress_integral_grad()
*
* purpose:  method gradient
*
*/


REAL stress_integral_grad(f_info)
struct qinfo *f_info;
{
  return 0.0;
}


/**********************************************************************
                    Linear volume quantity
**********************************************************************/
/*********************************************************************
*
* function: q_facet_volume_init()
*
* purpose:  Check illegalities
*
*/

void q_facet_volume_init(mode,mi)
int mode;
struct method_instance *mi;
{

  if ( (web.representation == SIMPLEX) && !(web.modeltype == LAGRANGE) )
  { kb_error(1454,"Use Lagrange model for simplex volumes.\n",RECOVERABLE);
  }
  else if ( (web.modeltype != LINEAR) && (web.gauss2D_order < 3) )
     { web.gauss2D_order = 3; gauss_setup(); }
}

/**********************************************************************
*
*  function: q_facet_volume()
*
*  purpose: value of volume integral on facet
*/

REAL q_facet_volume(f_info)
struct qinfo *f_info;
{ REAL **x,**s;
  REAL vol;

  if ( web.torus_flag ) return q_facet_torus_volume(f_info);
  if ( web.modeltype == QUADRATIC ) return q_facet_volume_q(f_info);
  if ( web.modeltype == LAGRANGE )  return lagrange_facet_volume(f_info);
  if ( web.representation == SIMPLEX )  return lagrange_facet_volume(f_info);

  x = f_info->x;
  s = f_info->sides[0];
  vol =  (x[0][2]+x[1][2]+x[2][2])*(s[0][0]*s[1][1] - s[0][1]*s[1][0])/6;
 
  return vol;
}

/**********************************************************************
*
*  function: q_facet_volume_grad()
*
*  purpose: gradient and value of volume integral on quadratic facet
*/

REAL q_facet_volume_grad(f_info)
struct qinfo *f_info;
{ REAL **x;
  REAL zsum,ssum;

  if ( web.torus_flag ) return q_facet_torus_volume_grad(f_info);
  if ( web.modeltype == QUADRATIC ) return q_facet_volume_q_grad(f_info);
  if ( web.modeltype == LAGRANGE )  return lagrange_facet_volume_grad(f_info);
  if ( web.representation == SIMPLEX )  return lagrange_facet_volume_grad(f_info);

   x = f_info->x;
   zsum = (x[0][2]+x[1][2]+x[2][2])/6;
   ssum = f_info->sides[0][0][0]*f_info->sides[0][1][1]
               - f_info->sides[0][0][1]*f_info->sides[0][1][0];
   f_info->grad[0][0] = zsum*(x[1][1]-x[2][1]);
   f_info->grad[0][1] = zsum*(x[2][0]-x[1][0]);
   f_info->grad[0][2] = ssum/6;
   f_info->grad[1][0] = zsum*(x[2][1]-x[0][1]);
   f_info->grad[1][1] = zsum*(x[0][0]-x[2][0]);
   f_info->grad[1][2] = ssum/6;
   f_info->grad[2][0] = zsum*(x[0][1]-x[1][1]);
   f_info->grad[2][1] = zsum*(x[1][0]-x[0][0]);
   f_info->grad[2][2] = ssum/6;
   return zsum*ssum;
}

/**********************************************************************
*
*  function: q_facet_volume_hess()
*
*  purpose: hessian, grad, and value of volume integral on quadratic facet
*/

REAL q_facet_volume_hess(f_info)
struct qinfo *f_info;
{ REAL **x,****h;
  REAL zsum,ssum;
  int i;

  if ( web.torus_flag ) return q_facet_torus_volume_hess(f_info);
  if ( web.modeltype == QUADRATIC ) return q_facet_volume_q_hess(f_info);
  if ( web.modeltype == LAGRANGE )  return lagrange_facet_volume_hess(f_info);
  if ( web.representation == SIMPLEX )  return lagrange_facet_volume_hess(f_info);

  x = f_info->x;
  zsum = (x[0][2]+x[1][2]+x[2][2])/6;
  ssum = f_info->sides[0][0][0]*f_info->sides[0][1][1]
                 - f_info->sides[0][0][1]*f_info->sides[0][1][0];
  f_info->grad[0][0] = zsum*(x[1][1]-x[2][1]);
  f_info->grad[0][1] = zsum*(x[2][0]-x[1][0]);
  f_info->grad[0][2] = ssum/6;
  f_info->grad[1][0] = zsum*(x[2][1]-x[0][1]);
  f_info->grad[1][1] = zsum*(x[0][0]-x[2][0]);
  f_info->grad[1][2] = ssum/6;
  f_info->grad[2][0] = zsum*(x[0][1]-x[1][1]);
  f_info->grad[2][1] = zsum*(x[1][0]-x[0][0]);
  f_info->grad[2][2] = ssum/6;

  h = f_info->hess;
  h[1][2][0][1] = h[1][0][1][0] = h[0][2][1][0] =  zsum;
  h[2][1][1][0] = h[0][1][0][1] = h[2][0][0][1] =  zsum;
  h[1][0][0][1] = h[0][2][0][1] = h[1][2][1][0] = -zsum;
  h[0][1][1][0] = h[2][0][1][0] = h[2][1][0][1] = -zsum;

  for ( i = 0 ; i < 3 ; i++ )
  { h[i][0][2][0] = h[0][i][0][2] = (x[1][1]-x[2][1])/6;
     h[i][0][2][1] = h[0][i][1][2] = (x[2][0]-x[1][0])/6;
     h[i][1][2][0] = h[1][i][0][2] = (x[2][1]-x[0][1])/6;
     h[i][1][2][1] = h[1][i][1][2] = (x[0][0]-x[2][0])/6;
     h[i][2][2][0] = h[2][i][0][2] = (x[0][1]-x[1][1])/6;
     h[i][2][2][1] = h[2][i][1][2] = (x[1][0]-x[0][0])/6;
  }

  return zsum*ssum;
}



/*********************************************************************

                    Facet area upper bound quantity

*********************************************************************/
void q_facet_tension_u_init(mode,mi)
int mode;
struct method_instance *mi;
{ 
  if ( web.gauss2D_order < 5 ) 
     { web.gauss2D_order = 5; gauss_setup(); }
}

REAL q_facet_tension_u_value(f_info)
struct qinfo *f_info;
{
  if ( web.modeltype == QUADRATIC ) return q_facet_tension_uq(f_info); 
  if ( web.modeltype == LAGRANGE ) 
     kb_error(1785,"Non-Lagrange quantity: facet_tension_u\n",RECOVERABLE);

  return q_facet_tension_value(f_info);
}

REAL q_facet_tension_u_gradient(f_info)
struct qinfo *f_info;
{
  if ( web.modeltype == QUADRATIC ) return q_facet_tension_uq_grad(f_info); 
  if ( web.modeltype == LAGRANGE ) 
     kb_error(1786,"Non-Lagrange quantity: facet_tension_u\n",RECOVERABLE);

  return q_facet_tension_gradient(f_info);
}

REAL q_facet_tension_u_hessian(f_info)
struct qinfo *f_info;
{
  if ( web.modeltype == QUADRATIC ) return q_facet_tension_uq_hess(f_info); 
  if ( web.modeltype == LAGRANGE ) 
     kb_error(1787,"Non-Lagrange quantity: facet_tension_u\n",RECOVERABLE);

  return q_facet_tension_hessian(f_info);
}




/*********************************************************************

                    facet_general method, linear and quadratic

    Integrand is general function of position and normal.  3D only.

*********************************************************************/

/* antisymmetric tensor for cross product */
static REAL e[3][3][3] = { {{0.,0.,0.},{0.,0.,1.},{0.,-1.,0.}},
                                  {{0.,0.,-1.},{0.,0.,0.},{1.,0.,0.}},
                                  {{0.,1.,0.},{-1.,0.,0.},{0.,0.,0.}} };

/*********************************************************************
*
* function: facet_general_init()
*
* purpose:  check illegalities
*
*/

void facet_general_init(mode,mi)
int mode;
struct method_instance *mi;
{ 
  if ( web.dimension != 2 )
     kb_error(1788,"facet_general_integral method only for surface dimension 2.\n",RECOVERABLE);
/*
  if ( SDIM != 3 )
     kb_error(1789,"facet_general_integral method only for space dimension 3.\n",RECOVERABLE);
*/
}

/*********************************************************************
*
* function: facet_general_value()
*
* purpose:  method value
*
*/

REAL facet_general_value(f_info)
struct qinfo *f_info;
{ int m,k;
  REAL value = 0.0;
  REAL z[2*MAXCOORD+1];  /*  pointers to coord and tangent */
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  REAL *normal = z+SDIM;
 
  if ( web.modeltype == LAGRANGE )
     return facet_general_value_lagr(f_info);

  facet_general_flag = 1;

  for ( m = 0 ; m < gauss2D_num ; m++ )
  { for ( k = 0 ; k < SDIM ; k++ ) z[k] = f_info->gauss_pt[m][k]; 
    cross_prod(f_info->sides[m][0],f_info->sides[m][1],normal);
    if ( sign == -1.0 ) 
        for ( k = 0 ; k < SDIM ; k++ ) z[k+SDIM] *= sign ; 
    z[2*SDIM] = m; /* kludge for attr interpolation. */
    value += gauss2Dwt[m]*eval(METH_INSTANCE(f_info->method)->expr[0],z,f_info->id,NULL);
  }
  facet_general_flag = 0;
  return value/web.simplex_factorial;  /* triangle factor */
}

/*********************************************************************
*
* function: facet_general_grad()
*
* purpose:  method gradient
*
*/


REAL facet_general_grad(f_info)
struct qinfo *f_info;
{ int m,j,k;
  REAL value = 0.0;
  REAL val;
  REAL derivs[2*MAXCOORD];
  REAL z[2*MAXCOORD+1];  /*  pointers to coord and tangent */
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  REAL *normal = z+SDIM;

  if ( web.modeltype == LAGRANGE )
     return facet_general_grad_lagr(f_info);

  facet_general_flag = 1;
  for ( m = 0 ; m < gauss2D_num ; m++ )
  { 
     REAL coeff = gauss2Dwt[m]/web.simplex_factorial; 
     REAL **s = f_info->sides[m];
     z[2*SDIM] = m; /* kludge for attr interpolation. */
     for ( k = 0 ; k < SDIM ; k++ ) z[k] = f_info->gauss_pt[m][k]; 
     cross_prod(f_info->sides[m][0],f_info->sides[m][1],normal);
     if ( sign == -1.0 ) 
        for ( k = 0 ; k < SDIM ; k++ ) z[k+SDIM] *= sign ; 
     eval_all(METH_INSTANCE(f_info->method)->expr[0],z,2*SDIM,&val,derivs,f_info->id);
     value += coeff*val;
     for ( j = 0 ; j < 3 ; j++ )
      { int jj = (j+1)%3;
         int jjj = (j+2)%3;
         for ( k = 0 ; k < ctrl_num ; k++ )
         { REAL **gpd = gpolypartial[m];
            f_info->grad[k][j] += coeff*(gpoly[m][k]*derivs[j]
              - sign*derivs[jj+SDIM]*(s[1][jjj]*gpd[0][k]-s[0][jjj]*gpd[1][k])
              - sign*derivs[jjj+SDIM]*(s[0][jj]*gpd[1][k]-s[1][jj]*gpd[0][k]));
         }
      }
  }
  facet_general_flag = 0;
  return value; 
}

/*********************************************************************
*
* function: facet_general_hess()
*
* purpose:  method gradient and hessian
*
*/


REAL facet_general_hess(f_info)
struct qinfo *f_info;
{ int m,j,k,kk,J;
  REAL value = 0.0;
  REAL val;
  REAL derivs[2*MAXCOORD];
  REAL z[2*MAXCOORD+1];  /*  pointers to coord and tangent */
  MAT2D(second,2*MAXCOORD,2*MAXCOORD); /* second derivatives */
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  REAL *normal = z+SDIM;

  if ( web.modeltype == LAGRANGE )
     return facet_general_hess_lagr(f_info);

  facet_general_flag = 1;
  for ( m = 0 ; m < gauss2D_num ; m++ )
  { 
     REAL coeff = gauss2Dwt[m]/web.simplex_factorial;
     REAL **gpd = gpolypartial[m];
     REAL **s = f_info->sides[m];
     z[2*SDIM] = m; /* kludge for attr interpolation. */
     for ( k = 0 ; k < SDIM ; k++ ) z[k] = f_info->gauss_pt[m][k]; 
     cross_prod(f_info->sides[m][0],f_info->sides[m][1],normal);
     if ( sign == -1.0 ) 
        for ( k = 0 ; k < SDIM ; k++ ) z[k+SDIM] *= sign ; 
     eval_second(METH_INSTANCE(f_info->method)->expr[0],z,2*SDIM,&val,derivs,second,f_info->id);
     value += coeff*val;
     /* gradients */
     for ( j = 0 ; j < 3 ; j++ )
      { int jj = (j+1)%3;
         int jjj = (j+2)%3;
         for ( k = 0 ; k < ctrl_num ; k++ )
         { 
            f_info->grad[k][j] += coeff*(gpoly[m][k]*derivs[j]
              - sign*derivs[jj+SDIM]*(s[1][jjj]*gpd[0][k]-s[0][jjj]*gpd[1][k])
              - sign*derivs[jjj+SDIM]*(s[0][jj]*gpd[1][k]-s[1][jj]*gpd[0][k]));
         }
      }
     /* hessian */
     for ( k = 0 ; k < ctrl_num ; k++ )
      for ( kk = 0 ; kk < ctrl_num ; kk++ )
        for ( j = 0 ; j < 3 ; j++ )
         for ( J = 0 ; J < 3 ; J++ )
         { int jj = (j==2) ? 0 : j+1;
            int jjj = (j==0) ? 2 : j-1 ;
            int JJ = (J==2) ? 0 : J+1;
            int JJJ = (J==0) ? 2 : J-1 ;
            REAL sum;

            /* have to break into parts, else too big expression for compiler */
              sum = gpoly[m][k]*gpoly[m][kk]*second[j][J];

              sum += sign*SDIM_dot(e[j][J],derivs+SDIM)
                          *(gpd[1][kk]*gpd[0][k]-gpd[0][kk]*gpd[1][k]);

              sum += sign*gpoly[m][kk]*(s[1][jj]*gpd[0][k]-s[0][jj]*gpd[1][k])
                          *second[J][jjj+SDIM]
                - sign*gpoly[m][kk]*(s[1][jjj]*gpd[0][k]-s[0][jjj]*gpd[1][k])
                          *second[J][jj+SDIM]
                + sign*gpoly[m][k]*(s[1][JJ]*gpd[0][kk]-s[0][JJ]*gpd[1][kk])
                          *second[j][JJJ+SDIM]
                - sign*gpoly[m][k]*(s[1][JJJ]*gpd[0][kk]-s[0][JJJ]*gpd[1][kk])
                          *second[j][JJ+SDIM];

              sum += (s[1][jj]*gpd[0][k] - s[0][jj]*gpd[1][k])
                  *(s[1][JJ]*gpd[0][kk] - s[0][JJ]*gpd[1][kk])
                     *second[jjj+SDIM][JJJ+SDIM]
                - (s[1][jjj]*gpd[0][k] - s[0][jjj]*gpd[1][k])
                  *(s[1][JJ]*gpd[0][kk] - s[0][JJ]*gpd[1][kk])
                     *second[jj+SDIM][JJJ+SDIM]
                - (s[1][jj]*gpd[0][k] - s[0][jj]*gpd[1][k])
                  *(s[1][JJJ]*gpd[0][kk] - s[0][JJJ]*gpd[1][kk])
                     *second[jjj+SDIM][JJ+SDIM]
                + (s[1][jjj]*gpd[0][k] - s[0][jjj]*gpd[1][k])
                  *(s[1][JJJ]*gpd[0][kk] - s[0][JJJ]*gpd[1][kk])
                     *second[jj+SDIM][JJ+SDIM];

            f_info->hess[k][kk][j][J] += coeff*sum;
         }
  }
  facet_general_flag = 0;
  return value; 
}

/********************************************************************
             Lagrange version of facet_general_integral
********************************************************************/

/*********************************************************************
*
* function: facet_general_value_lagr()
*
* purpose:  method value
*
*/

REAL facet_general_value_lagr(f_info)
struct qinfo *f_info;
{ int m,k;
  REAL value = 0.0;
  REAL z[2*MAXCOORD+1];  /*  pointers to coord and tangent */
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  int dim = web.dimension;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];
  REAL *normal = z+SDIM;
 
  facet_general_flag = 1;
  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { for ( k = 0 ; k < SDIM ; k++ ) z[k] = f_info->gauss_pt[m][k]; 
    cross_prod(f_info->sides[m][0],f_info->sides[m][1],normal);
    if ( sign == -1.0 ) 
        for ( k = 0 ; k < SDIM ; k++ ) z[k+SDIM] *= sign ; 
    z[2*SDIM] = m; /* kludge for attr interpolation. */
    value += gl->gausswt[m]*eval(METH_INSTANCE(f_info->method)->expr[0],z,f_info->id,NULL);
  }
  facet_general_flag = 0;
  return value/web.simplex_factorial; 
}

/*********************************************************************
*
* function: facet_general_grad_lagr()
*
* purpose:  method gradient
*
*/


REAL facet_general_grad_lagr(f_info)
struct qinfo *f_info;
{ int m,j,k;
  REAL value = 0.0;
  REAL val;
  REAL derivs[2*MAXCOORD];
  REAL z[2*MAXCOORD+1];  /*  pointers to coord and tangent */
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  int dim = web.dimension;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];
  REAL *normal = z+SDIM;

  facet_general_flag = 1;
  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { 
     REAL coeff = gl->gausswt[m]/web.simplex_factorial;
     REAL **s = f_info->sides[m];
     z[2*SDIM] = m; /* kludge for attr interpolation. */
     for ( k = 0 ; k < SDIM ; k++ ) z[k] = f_info->gauss_pt[m][k]; 
     cross_prod(f_info->sides[m][0],f_info->sides[m][1],normal);
     if ( sign == -1.0 ) 
        for ( k = 0 ; k < SDIM ; k++ ) z[k+SDIM] *= sign ; 
     eval_all(METH_INSTANCE(f_info->method)->expr[0],z,2*SDIM,&val,derivs,f_info->id);
     value += coeff*val;
     for ( j = 0 ; j < SDIM ; j++ )
      { int jj = (j+1)%3;
         int jjj = (j+2)%3;
         for ( k = 0 ; k < gl->lagpts ; k++ )
         { REAL **gpd = gl->gpolypart[m];
            f_info->grad[k][j] += coeff*(gl->gpoly[m][k]*derivs[j]
              - sign*derivs[jj+SDIM]*(s[1][jjj]*gpd[0][k]-s[0][jjj]*gpd[1][k])
              - sign*derivs[jjj+SDIM]*(s[0][jj]*gpd[1][k]-s[1][jj]*gpd[0][k]));
         }
      }
  }
  facet_general_flag = 0;
  return value; 
}

/*********************************************************************
*
* function: facet_general_hess_lagr()
*
* purpose:  method gradient and hessian
*
*/


REAL facet_general_hess_lagr(f_info)
struct qinfo *f_info;
{ int m,j,k,kk,J;
  REAL value = 0.0;
  REAL val;
  REAL derivs[2*MAXCOORD];
  REAL z[2*MAXCOORD+1];  /*  pointers to coord and tangent */
  MAT2D(second,2*MAXCOORD,2*MAXCOORD); /* second derivatives */
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  int dim = web.dimension;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];
  REAL *normal = z+SDIM;

  facet_general_flag = 1;
  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { 
     REAL coeff = gl->gausswt[m]/web.simplex_factorial;
     REAL **gpd = gl->gpolypart[m];
     REAL **s = f_info->sides[m];
     z[2*SDIM] = m; /* kludge for attr interpolation. */
     for ( k = 0 ; k < SDIM ; k++ ) z[k] = f_info->gauss_pt[m][k]; 
     cross_prod(f_info->sides[m][0],f_info->sides[m][1],normal);
     if ( sign == -1.0 ) 
        for ( k = 0 ; k < SDIM ; k++ ) z[k+SDIM] *= sign ; 
     eval_second(METH_INSTANCE(f_info->method)->expr[0],z,2*SDIM,&val,derivs,
          second,f_info->id);
     value += coeff*val;
     /* gradients */
     for ( j = 0 ; j < SDIM ; j++ )
      { int jj = (j+1)%3;
         int jjj = (j+2)%3;
         for ( k = 0 ; k < gl->lagpts ; k++ )
         { 
            f_info->grad[k][j] += coeff*(gl->gpoly[m][k]*derivs[j]
              - sign*derivs[jj+SDIM]*(s[1][jjj]*gpd[0][k]-s[0][jjj]*gpd[1][k])
              - sign*derivs[jjj+SDIM]*(s[0][jj]*gpd[1][k]-s[1][jj]*gpd[0][k]));
         }
      }
     /* hessian */
     for ( k = 0 ; k < gl->lagpts ; k++ )
      for ( kk = 0 ; kk < gl->lagpts ; kk++ )
        for ( j = 0 ; j < SDIM ; j++ )
         for ( J = 0 ; J < SDIM ; J++ )
         { int jj = (j==2) ? 0 : j+1;
            int jjj = (j==0) ? 2 : j-1 ;
            int JJ = (J==2) ? 0 : J+1;
            int JJJ = (J==0) ? 2 : J-1 ;
            REAL sum;

            /* have to break into parts, else too big expression for compiler */
              sum = gl->gpoly[m][k]*gl->gpoly[m][kk]*second[j][J];

              sum += sign*SDIM_dot(e[j][J],derivs+SDIM)
                          *(gpd[1][kk]*gpd[0][k]-gpd[0][kk]*gpd[1][k]);

              sum += sign*gl->gpoly[m][kk]*(s[1][jj]*gpd[0][k]-s[0][jj]*gpd[1][k])
                          *second[J][jjj+SDIM]
                - sign*gl->gpoly[m][kk]*(s[1][jjj]*gpd[0][k]-s[0][jjj]*gpd[1][k])
                          *second[J][jj+SDIM]
                + sign*gl->gpoly[m][k]*(s[1][JJ]*gpd[0][kk]-s[0][JJ]*gpd[1][kk])
                          *second[j][JJJ+SDIM]
                - sign*gl->gpoly[m][k]*(s[1][JJJ]*gpd[0][kk]-s[0][JJJ]*gpd[1][kk])
                          *second[j][JJ+SDIM];

              sum += (s[1][jj]*gpd[0][k] - s[0][jj]*gpd[1][k])
                  *(s[1][JJ]*gpd[0][kk] - s[0][JJ]*gpd[1][kk])
                     *second[jjj+SDIM][JJJ+SDIM]
                - (s[1][jjj]*gpd[0][k] - s[0][jjj]*gpd[1][k])
                  *(s[1][JJ]*gpd[0][kk] - s[0][JJ]*gpd[1][kk])
                     *second[jj+SDIM][JJJ+SDIM]
                - (s[1][jj]*gpd[0][k] - s[0][jj]*gpd[1][k])
                  *(s[1][JJJ]*gpd[0][kk] - s[0][JJJ]*gpd[1][kk])
                     *second[jjj+SDIM][JJ+SDIM]
                + (s[1][jjj]*gpd[0][k] - s[0][jjj]*gpd[1][k])
                  *(s[1][JJJ]*gpd[0][kk] - s[0][JJJ]*gpd[1][kk])
                     *second[jj+SDIM][JJ+SDIM];

            f_info->hess[k][kk][j][J] += coeff*sum;
         }
  }
  facet_general_flag = 0;
  return value; 
}


/*********************************************************************

                                Facet area^2 quantity
                                due to Renka and Neuberger

*********************************************************************/

/*********************************************************************
*
*  function: area_square_value()
*
*  purpose:  General quantity value of facet tension.
*/

REAL area_square_value(f_info)
struct qinfo *f_info;
{ REAL energy;

  mat_tsquare(f_info->sides[0],f_info->ss,web.dimension,SDIM);
  energy = det_adjoint(f_info->ss,web.dimension);
  return energy/web.simplex_factorial/web.simplex_factorial;
}

/*********************************************************************
*
*  function: area_square_gradient()
*
*  purpose:  General quantity value and gradient 
*/

REAL area_square_gradient(f_info)
struct qinfo *f_info;
{ REAL energy;
  int i,j;

  mat_tsquare(f_info->sides[0],f_info->ss,web.dimension,SDIM);
  energy = det_adjoint(f_info->ss,web.dimension);
  for ( i = 0 ; i < web.dimension ; i++ )
     for ( j = 0 ; j < web.dimension ; j++ ) 
        f_info->ss[i][j] *= 2/web.simplex_factorial/web.simplex_factorial;
  mat_mult(f_info->ss,f_info->sides[0],f_info->grad+1,web.dimension,web.dimension,
            SDIM); /* head forces */
  memset((char*)f_info->grad[0],0,SDIM*sizeof(REAL));
  for ( i = 0 ; i < web.dimension ; i++ )  /* tail forces */
     vector_sub(f_info->grad[0],f_info->grad[i+1],SDIM);
  return energy/web.simplex_factorial/web.simplex_factorial;
}

