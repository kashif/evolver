/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*********************************************************************
*
*     file:        method2.c
*
*     contents:  quantities for facets
*/

#include "include.h"

/*********************************************************************

                                Facet area quantity

Linear simplex volume:
Let S[i][j] be side i vector.
Let U[i][j] = L[i][k]*S[k][j] be orthogonalized version.
Let diag[i] = U[i]*U[i].
Let det = det(diag).
Let D = diag^-1.
Then area = sqrt(det)/n!
      d(area)/dS[i][j] = area*(L^T D U)[i][j]
      dd(area)/dS[i][j]dS[k][l] = area*( (L^T D L)[i][k]*I[j][l]
         - (L^T D L)[i][k]*(U^T D U)[j][l] + (L^T D U)[i][j]*(L^T D U)[k][l]
         - (L^T D U)[k][j]*(L^T D U)[i][l])
*********************************************************************/
/*********************************************************************
*
*  function: q_facet_tension_init()
*
*  purpose:  initialize web.total_area to 0.
*/

void q_facet_tension_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{
  /* if ( everything_quantities_flag && (mode==METHOD_VALUE) ) web.total_area = 0.0; */
}

/*********************************************************************
*
*  function: q_facet_tension_value()
*
*  purpose:  General quantity value of facet tension.
*/

REAL q_facet_tension_value(f_info)
struct qinfo *f_info;
{ REAL area;
  int i,j,k;
  REAL diag[MAXCOORD];
  REAL D[MAXCOORD]; /* diag^-1 */
  REAL U[MAXCOORD][MAXCOORD];

  if ( web.modeltype == QUADRATIC ) return q_facet_tension_q(f_info); 
  if ( web.modeltype == LAGRANGE ) 
      return lagrange_facet_tension_value(f_info);
  /* new way, using Gram-Schmidt */
  area = 1.0;
  for ( i = 0 ; i < web.dimension ; i++ )
  { for ( j = 0 ; j < SDIM ; j++ ) U[i][j] = f_info->sides[0][i][j];
     for ( j = 0 ; j < i ; j++ )
     { REAL su = SDIM_dot(f_info->sides[0][i],U[j])*D[j];
        for ( k = 0 ; k < SDIM ; k++ ) U[i][k] -= su*U[j][k];
     }
     diag[i] = SDIM_dot(U[i],U[i]);
     if ( diag[i] > 0.0 ) D[i] = 1/diag[i];
     else D[i] = 1.0;
     area *= diag[i];
  }

  if ( area > 0.0 )
     area = sqrt(area)/web.simplex_factorial;
  else area = 0.0;
  if ( METH_INSTANCE(f_info->method)->flags & DEFAULT_INSTANCE )
  { set_facet_area(f_info->id,area);
#ifdef SHARED_MEMORY
     if ( nprocs > 1 ) 
      proc_total_area[GET_THREAD_ID] += area;
     else
#endif
     binary_tree_add(web.total_area_addends,area);
  }
  if ( METH_INSTANCE(f_info->method)->flags & USE_DENSITY )
      area *= get_facet_density(f_info->id);
  return area;
}

/*********************************************************************
*
*  function: q_facet_tension_gradient()
*
*  purpose:  General quantity value of facet tension.
*/

REAL q_facet_tension_gradient(f_info)
struct qinfo *f_info;
{ REAL area;
  int i,j;
  REAL fudge;

  if ( web.modeltype == QUADRATIC ) return q_facet_tension_q_grad(f_info); 
  if ( web.modeltype == LAGRANGE ) 
     return lagrange_facet_tension_grad(f_info);

#define OLDWAY
#ifdef OLDWAY
  mat_tsquare(f_info->sides[0],f_info->ss,web.dimension,SDIM);
  area = det_adjoint(f_info->ss,web.dimension);
  if ( area > 0.0 )
  { area = sqrt(area);
    fudge = 1/(area*web.simplex_factorial);
    area /= web.simplex_factorial;
  }
  else fudge = area = 0.0;
  if ( METH_INSTANCE(f_info->method)->flags & USE_DENSITY )
  { REAL density = get_facet_density(f_info->id);
    area *= density; fudge *= density;
  }
  for ( i = 0 ; i < web.dimension ; i++ )
     for ( j = 0 ; j < web.dimension ; j++ ) 
        f_info->ss[i][j] *= fudge;
  mat_mult(f_info->ss,f_info->sides[0],f_info->grad+1,web.dimension,
         web.dimension, SDIM); /* head forces */
  memset((char*)f_info->grad[0],0,SDIM*sizeof(REAL));
  for ( i = 0 ; i < web.dimension ; i++ )  /* tail forces */
     vector_sub(f_info->grad[0],f_info->grad[i+1],SDIM);
#else

  /* new way, using Gram-Schmidt */
  {
  REAL diag[MAXCOORD];
  REAL D[MAXCOORD];
  REAL U[MAXCOORD][MAXCOORD];
  REAL L[MAXCOORD][MAXCOORD];
  REAL DL[MAXCOORD][MAXCOORD];
  REAL LDU[MAXCOORD][MAXCOORD];
  REAL det;
  int k;

  det = 1.0;
  for ( i = 0 ; i < web.dimension ; i++ )
  { REAL invd;
     for ( j = 0 ; j < SDIM ; j++ ) U[i][j] = f_info->sides[0][i][j];
     for ( j = 0 ; j < i ; j++ )
     { REAL su = SDIM_dot(f_info->sides[0][i],U[j])*D[j];
        for ( k = 0 ; k < SDIM ; k++ ) U[i][k] -= su*U[j][k];
        L[i][j] = -su;
     }
     diag[i] = SDIM_dot(U[i],U[i]);
     if ( diag[i] <= 0.0 ) invd = 1.0;
     else invd = 1/diag[i];
     D[i] = invd;
     DL[i][i] = invd;
     for ( k = 0 ; k < i ; k++ )
        DL[i][k] = invd*L[i][k];
     det *= diag[i];
  }
  area = sqrt(det)/web.simplex_factorial; 
  if ( METH_INSTANCE(f_info->method)->flags & USE_DENSITY )
      area *= get_facet_density(f_info->id);
  for ( i = 0 ; i < web.dimension ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
     { REAL sum = DL[i][i]*U[i][j];
        for ( k = i+1 ; k < web.dimension ; k++ ) 
            sum += DL[k][i]*U[k][j];
        LDU[i][j] = sum;
     }
  for ( j = 0 ; j < SDIM ; j++ ) f_info->grad[0][j] = 0.0;
  for ( i = 0 ; i < web.dimension ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
     { REAL gr = area*LDU[i][j];
        f_info->grad[i+1][j] = gr;
        f_info->grad[0][j] -= gr;
     }
  }
#endif
  return area;
}


/*********************************************************************
*
*  function: q_facet_tension_hessian()
*
*  purpose:  General quantity value, gradient and hessian of facet area.
*/

REAL q_facet_tension_hessian(f_info)
struct qinfo *f_info;
{ int i,j,k,m;
  REAL val;
  REAL area;
  REAL ssdet,fudge,energy;

  if ( dirichlet_flag )
  { if ( web.modeltype != LINEAR )
       kb_error(2144,"Dirichlet_mode requires linear model.\n",RECOVERABLE);
    return dirichlet_area_hess(f_info);
  }
  if ( sobolev_flag )
  { if ( web.modeltype != LINEAR )
       kb_error(2145,"Sobolev_mode requires linear model.\n",RECOVERABLE);
    return sobolev_area_hess(f_info);
  }
  if ( web.modeltype == QUADRATIC ) return q_facet_tension_q_hess(f_info); 
  if ( web.modeltype == LAGRANGE ) return lagrange_facet_tension_hess(f_info);

#ifdef OLDWAY
  {
  MAT2D(AS,MAXCOORD,MAXCOORD);
  MAT2D(SAS,MAXCOORD,MAXCOORD);
#define S (f_info->sides[0])
#define A (f_info->ss)

  /* area derivatives */
  mat_tsquare(S,A,web.dimension,SDIM);
  ssdet = det_adjoint(A,web.dimension);
  if ( ssdet <= 0.0 ) {  return 0.0; }
  energy = area = sqrt(ssdet)/web.simplex_factorial;
  fudge = 1/ssdet;
  if ( METH_INSTANCE(f_info->method)->flags & USE_DENSITY )
  { REAL density = get_facet_density(f_info->id);
     energy *= density; area *= density;
  }
  for ( i = 0 ; i < web.dimension ; i++ )
     for ( j = 0 ; j < web.dimension ; j++ ) 
        A[i][j] *= fudge;    /* now inverse of ss, times density */
  mat_mult(A,S,AS,web.dimension,web.dimension, SDIM); 

  /* head forces */
  for ( i = 0 ; i < web.dimension ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
        f_info->grad[i+1][j] = area*AS[i][j];
  /* tail forces */
  memset((char*)f_info->grad[0],0,SDIM*sizeof(REAL));
  for ( i = 0 ; i < web.dimension ; i++ )  
     vector_sub(f_info->grad[0],f_info->grad[i+1],SDIM);

  /* hessian */
  tr_mat_mul(S,AS,SAS,web.dimension,SDIM,SDIM);
  for ( m = 0 ; m < web.dimension ; m++ )
    for ( i = 0 ; i < web.dimension ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
      for ( k = 0 ; k < SDIM ; k++ )
        { val = area*(AS[m][j]*AS[i][k] + (j==k?A[i][m]:0.0)
                     - A[i][m]*SAS[j][k] - AS[i][j]*AS[m][k]);
          f_info->hess[m+1][i+1][j][k] = val;
          f_info->hess[0][0][j][k] += val;
          f_info->hess[0][i+1][j][k] -= val;
          f_info->hess[m+1][0][j][k] -= val;
        }
  return energy;
#undef S
#undef A
  }
#else

  /* new way, using Gram-Schmidt */
  {
  REAL diag[MAXCOORD];
  REAL U[MAXCOORD][MAXCOORD];
  REAL D[MAXCOORD];
  REAL L[MAXCOORD][MAXCOORD]; /* only subdiagonal explicit */
  REAL DL[MAXCOORD][MAXCOORD];
  REAL LDU[MAXCOORD][MAXCOORD];
  REAL LDL[MAXCOORD][MAXCOORD];
  REAL UDU[MAXCOORD][MAXCOORD];
  REAL det;
  det = 1.0;
  for ( i = 0 ; i < web.dimension ; i++ )
  { REAL invd;
     for ( j = 0 ; j < SDIM ; j++ ) U[i][j] = f_info->sides[0][i][j];
     for ( j = 0 ; j < i ; j++ )
     { REAL su = SDIM_dot(f_info->sides[0][i],U[j])*D[j];
        for ( k = 0 ; k < SDIM ; k++ ) U[i][k] -= su*U[j][k];
        L[i][j] = -su;
     }
     diag[i] = SDIM_dot(U[i],U[i]);
     if ( diag[i] <= 0.0 ) invd = 0.0;
     else invd = 1/diag[i];
     D[i] = invd;
     DL[i][i] = invd;
     for ( k = 0 ; k < i ; k++ )
         DL[i][k] = invd*L[i][k];
     det *= diag[i];
  }
  area = sqrt(det)/web.simplex_factorial; 
  if ( METH_INSTANCE(f_info->method)->flags & USE_DENSITY )
      area *= get_facet_density(f_info->id);
  /* form LDL, LDU, UDU */
  for ( i = 0 ; i < web.dimension ; i++ )
     for ( j = 0 ; j <= i ; j++ )
     { REAL sum = DL[i][j];
        for ( k = i+1 ; k < web.dimension ; k++ ) 
            sum += L[k][i]*DL[k][j];
        LDL[i][j] = LDL[j][i] = sum;
     }
  for ( i = 0 ; i < web.dimension ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
     { REAL sum = DL[i][i]*U[i][j];
        for ( k = i+1 ; k < web.dimension ; k++ ) 
            sum += DL[k][i]*U[k][j];
        LDU[i][j] = sum;
     }
  for ( i = 0 ; i < SDIM ; i++ )
     for ( j = 0 ; j <= i ; j++ )
     { REAL sum = 0.0;
        for ( k = 0 ; k < web.dimension ; k++ )
          sum += U[k][i]*D[k]*U[k][j];
        UDU[i][j] = UDU[j][i] = sum;
     }

  for ( i = 0 ; i < web.dimension ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
     { REAL gr = area*LDU[i][j];
        f_info->grad[i+1][j] = gr;
        f_info->grad[0][j] -= gr;
     }
  for ( m = 0 ; m < web.dimension ; m++ )
    for ( i = 0 ; i < web.dimension ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
      for ( k = 0 ; k < SDIM ; k++ )
        { val = area*(LDU[m][j]*LDU[i][k] + (j==k?LDL[i][m]:0.0)
                     - LDL[i][m]*UDU[j][k] - LDU[i][j]*LDU[m][k]) ;
          f_info->hess[m+1][i+1][j][k] = val;
          f_info->hess[0][0][j][k] += val;
          f_info->hess[0][i+1][j][k] -= val;
          f_info->hess[m+1][0][j][k] -= val;
        }
  return area;
  }
#endif
        
}


/*********************************************************************

                    facet_scalar_integral method
            2D facets only, in any space dimension

*********************************************************************/

/*********************************************************************
*
* function: facet_scalar_integral_init()
*
* purpose:  check illegalities
*
*/

void facet_scalar_integral_init(mode,mi)
int mode;
struct method_instance *mi;
{ if ( web.dimension != 2 )
    kb_error(1768,"facet_scalar_integral method only for SOAPFILM model.\n",
       RECOVERABLE);
}

/*********************************************************************
*
* function: facet_scalar_integral()
*
* purpose:  method value
*
*/

REAL facet_scalar_integral(f_info)
struct qinfo *f_info;
{ int m;
  REAL value = 0.0;
  REAL area;
  REAL ss,st,tt;

  if ( web.modeltype == QUADRATIC ) return facet_scalar_integral_q(f_info);
  if ( web.modeltype == LAGRANGE ) return facet_scalar_integral_lagr(f_info);

  ss = SDIM_dot(f_info->sides[0][0],f_info->sides[0][0]);
  st = SDIM_dot(f_info->sides[0][0],f_info->sides[0][1]);
  tt = SDIM_dot(f_info->sides[0][1],f_info->sides[0][1]);
  area = ss*tt-st*st;
  if ( area <= 0.0 ) return 0.0;
  area = sqrt(area)/2;
  for ( m = 0 ; m < gauss2D_num ; m++ )
  {  f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
     value += gauss2Dwt[m]*
       eval(METH_INSTANCE(f_info->method)->expr[0],f_info->gauss_pt[m],f_info->id,NULL);
  }
  value *= area;
  return value;
}

/*********************************************************************
*
* function: facet_scalar_integral_grad()
*
* purpose:  method gradient
*
*/

REAL facet_scalar_integral_grad(f_info)
struct qinfo *f_info;
{ int m,i,j;
  REAL value = 0.0;
  REAL val;
  REAL derivs[MAXCOORD];
  REAL area;
  REAL ss,st,tt;

  if ( web.modeltype == QUADRATIC ) return facet_scalar_integral_q_grad(f_info);
  if ( web.modeltype == LAGRANGE )  return facet_scalar_integral_lagr_grad(f_info);

  for ( m = 0 ; m < FACET_VERTS ; m++ )
     for ( j = 0 ; j < SDIM ; j++ ) 
        f_info->grad[m][j] = 0.0;

  ss = SDIM_dot(f_info->sides[0][0],f_info->sides[0][0]);
  st = SDIM_dot(f_info->sides[0][0],f_info->sides[0][1]);
  tt = SDIM_dot(f_info->sides[0][1],f_info->sides[0][1]);
  area = ss*tt-st*st;
  if ( area <= 0.0 ) return 0.0;
  area = sqrt(area)/2;

  for ( m = 0 ; m < gauss2D_num ; m++ )
  { 
    f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    eval_all(METH_INSTANCE(f_info->method)->expr[0],f_info->gauss_pt[m],SDIM,&val,
                                                       derivs,f_info->id);
    value += gauss2Dwt[m]*val;
    for ( i = 0 ; i < FACET_VERTS ; i++ )
      for ( j = 0 ; j < SDIM ; j++ )
        f_info->grad[i][j] += gauss2Dwt[m]*gauss2Dpt[m][i]*derivs[j]*area;
  }
  for ( j = 0 ; j < SDIM ; j++ )
  { REAL tmp1,tmp2;
    tmp1 = tt*f_info->sides[0][0][j] - st*f_info->sides[0][1][j];
    tmp2 = ss*f_info->sides[0][1][j] - st*f_info->sides[0][0][j];
    f_info->grad[0][j] -= value*(tmp1+tmp2)/4/area;
    f_info->grad[1][j] += value*tmp1/4/area;
    f_info->grad[2][j] += value*tmp2/4/area;
  }

  return area*value;
}


/*********************************************************************
*
* function: facet_scalar_integral_hess()
*
* purpose:  method gradient and hessian 
*
*/

REAL facet_scalar_integral_hess(f_info)
struct qinfo *f_info;
{ int n,m,j,k,i;
  REAL value = 0.0;
  REAL sum,val;
  REAL derivs[MAXCOORD];
  REAL areagrad[MAXCOORD][MAXCOORD],sumgrad[MAXCOORD][MAXCOORD];
  REAL areahess[MAXCOORD][MAXCOORD][MAXCOORD][MAXCOORD],
            sumhess[MAXCOORD][MAXCOORD][MAXCOORD][MAXCOORD];
  REAL area;
  REAL ssdet;
  MAT2D(AS,MAXCOORD,MAXCOORD);
  MAT2D(SAS,MAXCOORD,MAXCOORD);
  MAT2D(second,MAXCOORD,MAXCOORD);


#define S (f_info->sides[0])
#define A (f_info->ss)

  if ( web.modeltype == QUADRATIC ) return facet_scalar_integral_q_hess(f_info);
  if ( web.modeltype == LAGRANGE ) return facet_scalar_integral_lagr_hess(f_info);

  /* first, area derivatives */
  mat_tsquare(S,A,web.dimension,SDIM);
  ssdet = det_adjoint(A,web.dimension);
  if ( ssdet <= 0.0 ) {  return 0.0; }
  area = sqrt(ssdet)/web.simplex_factorial;
  for ( i = 0 ; i < web.dimension ; i++ )
     for ( j = 0 ; j < web.dimension ; j++ ) 
        A[i][j] /= ssdet;    /* now inverse of ss */
  mat_mult(A,S,AS,web.dimension,web.dimension, SDIM); 

  /* head forces */
  for ( i = 0 ; i < web.dimension ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
        areagrad[i+1][j] = area*AS[i][j];
  /* tail forces */
  memset((char*)areagrad[0],0,SDIM*sizeof(REAL));
  for ( i = 0 ; i < web.dimension ; i++ )  
     vector_sub(areagrad[0],areagrad[i+1],SDIM);

  /* hessian */
  tr_mat_mul(S,AS,SAS,web.dimension,SDIM,SDIM);
  for ( j = 0 ; j < SDIM ; j++ )
    for ( k = 0 ; k < SDIM ; k++ )
     for ( m = 0 ; m <= web.dimension ; m++ )
        { areahess[m][0][j][k] = 0.0;
          areahess[0][m][j][k] = 0.0;
        }
  for ( m = 0 ; m < web.dimension ; m++ )
    for ( i = 0 ; i < web.dimension ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
      for ( k = 0 ; k < SDIM ; k++ )
        { val = area*(AS[m][j]*AS[i][k] + (j==k?A[i][m]:0.0)
                     - A[i][m]*SAS[j][k] - AS[i][j]*AS[m][k]);
          areahess[m+1][i+1][j][k] = val;
          areahess[0][0][j][k] += val;
          areahess[0][i+1][j][k] -= val;
          areahess[m+1][0][j][k] -= val;
        }

  /* gaussian sum derivatives */
  memset((char*)sumgrad,0,sizeof(sumgrad));
  memset((char*)sumhess,0,sizeof(sumhess));
  for ( m = 0, sum = 0.0 ; m < gauss2D_num ; m++ )
  { 
    f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    eval_second(METH_INSTANCE(f_info->method)->expr[0],f_info->gauss_pt[m],SDIM,&val,
                                                   derivs,second,f_info->id);
    sum += gauss2Dwt[m]*val;
    for ( i = 0 ; i <= web.dimension ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
        sumgrad[i][j] += gauss2Dwt[m]*gauss2Dpt[m][i]*derivs[j];
    for ( i = 0 ; i <= web.dimension ; i++ )
     for ( n = 0 ; n <= web.dimension ; n++ )
      for ( j = 0 ; j < SDIM ; j++ )
       for ( k = 0 ; k < SDIM ; k++ )
         sumhess[i][n][j][k] += gauss2Dwt[m]*gauss2Dpt[m][i]*gauss2Dpt[m][n]
                                  *second[j][k];
  }

  /* final values */
  value = area*sum;
  for ( m = 0 ; m <= web.dimension ; m++ )
    for ( j = 0 ; j < SDIM ; j++ )
      f_info->grad[m][j] = areagrad[m][j]*sum + area*sumgrad[m][j];
  for ( m = 0 ; m <= web.dimension ; m++ )
   for ( i = 0 ; i <= web.dimension ; i++ )
    for ( j = 0 ; j < SDIM ; j++ ) 
     for ( k = 0 ; k < SDIM ; k++ ) 
       f_info->hess[m][i][j][k] = areahess[m][i][j][k]*sum
            + areagrad[m][j]*sumgrad[i][k] + sumgrad[m][j]*areagrad[i][k]
            + area*sumhess[m][i][j][k];
  return value;
}


/*********************************************************************

                  quadratic facet_scalar_integral method
                  2D facets only, in any space dimension

*********************************************************************/

/*********************************************************************
*
* function: facet_scalar_integral_q()
*
* purpose:  method value
*
*/

REAL facet_scalar_integral_q(f_info)
struct qinfo *f_info;
{ int m;
  REAL value = 0.0;
  REAL area;
  REAL ss,st,tt;

  for ( m = 0 ; m < gauss2D_num ; m++ )
  { REAL **tang = f_info->sides[m];
    ss = SDIM_dot(tang[0],tang[0]);
    st = SDIM_dot(tang[0],tang[1]);
    tt = SDIM_dot(tang[1],tang[1]);
    area = ss*tt-st*st;
    if ( area <= 0.0 ) continue;
    f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    value += gauss2Dwt[m]*sqrt(area)*
       eval(METH_INSTANCE(f_info->method)->expr[0],f_info->gauss_pt[m],f_info->id,NULL);
  } 
  return value/2; /* triangle factor */
}

/*********************************************************************
*
* function: facet_scalar_integral_q_grad()
*
* purpose:  method gradient
*
*/


REAL facet_scalar_integral_q_grad(f_info)
struct qinfo *f_info;
{ int m,j,k;
  REAL value = 0.0;
  REAL val;
  REAL derivs[MAXCOORD];
  REAL detgrad[FACET_CTRL][MAXCOORD];
  REAL area,det;
  REAL ss,st,tt;

  for ( m = 0 ; m < FACET_CTRL ; m++ )
     for ( j = 0 ; j < SDIM ; j++ ) 
        f_info->grad[m][j] = 0.0;

  for ( m = 0 ; m < gauss2D_num ; m++ )
  { REAL **tang = f_info->sides[m];
    ss = SDIM_dot(tang[0],tang[0]);
    st = SDIM_dot(tang[0],tang[1]);
    tt = SDIM_dot(tang[1],tang[1]);
    det = ss*tt-st*st;
    if ( det <= 0.0 ) continue;
    area = sqrt(det)/2; 
    f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    eval_all(METH_INSTANCE(f_info->method)->expr[0],f_info->gauss_pt[m],SDIM,&val,
                                                       derivs,f_info->id);
    value += gauss2Dwt[m]*area*val;

    for ( k = 0 ; k < FACET_CTRL ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
        detgrad[k][j] = 2*gpolypartial[m][0][k]*tang[0][j]*tt
                + ss*2*gpolypartial[m][1][k]*tang[1][j]
                - 2*st*(gpolypartial[m][0][k]*tang[1][j]
               + gpolypartial[m][1][k]*tang[0][j]);

    for ( k = 0 ; k < FACET_CTRL ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
        f_info->grad[k][j] += gauss2Dwt[m]*
          (val/sqrt(det)/4*detgrad[k][j] + area*derivs[j]*gpoly[m][k]);
  }

  return value;
}


/*********************************************************************
*
* function: facet_scalar_integral_q_hess()
*
* purpose:  method gradient and hessian 
*
*/

REAL facet_scalar_integral_q_hess(f_info)
struct qinfo *f_info;
{ int m,j,k,kk,jj;
  REAL value = 0.0;
  REAL val;
  REAL derivs[MAXCOORD];
  REAL detgrad[FACET_CTRL][MAXCOORD];
  REAL dethess[FACET_CTRL][FACET_CTRL][MAXCOORD][MAXCOORD];
  REAL area,det,ss,st,tt;
  MAT2D(second,MAXCOORD,MAXCOORD);


  for ( m = 0 ; m < gauss2D_num ; m++ )
  { REAL **gpp = gpolypartial[m];
    REAL **tang = f_info->sides[m];
    ss = SDIM_dot(tang[0],tang[0]);
    st = SDIM_dot(tang[0],tang[1]);
    tt = SDIM_dot(tang[1],tang[1]);
    det = ss*tt-st*st;
    if ( det <= 0.0 ) continue;
    area = sqrt(det)/2; 
    f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    eval_second(METH_INSTANCE(f_info->method)->expr[0],f_info->gauss_pt[m],SDIM,&val,
                                                  derivs,second,f_info->id);
    value += gauss2Dwt[m]*area*val;

    /* gradients */
    for ( k = 0 ; k < FACET_CTRL ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
        detgrad[k][j] = 2*gpp[0][k]*tang[0][j]*tt + ss*2*gpp[1][k]*tang[1][j]
                        - 2*st*(gpp[0][k]*tang[1][j] + gpp[1][k]*tang[0][j]);

    for ( k = 0 ; k < FACET_CTRL ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
        f_info->grad[k][j] += gauss2Dwt[m]*
           (val/sqrt(det)/4*detgrad[k][j] + area*derivs[j]*gpoly[m][k]);

    /* hessian */
    for ( k = 0 ; k < FACET_CTRL ; k++ )
      for ( kk = 0 ; kk < FACET_CTRL ; kk++ )
        for ( j = 0 ; j < SDIM ; j++ )
          for ( jj = 0 ; jj < SDIM ; jj++ )
          { dethess[k][kk][j][jj] = 
                     2*gpp[0][k]*tang[0][j]*2*gpp[1][kk]*tang[1][jj]
                  + 2*gpp[0][kk]*tang[0][jj]*2*gpp[1][k]*tang[1][j]
                  - 2*(gpp[0][kk]*tang[1][jj] + gpp[1][kk]*tang[0][jj])
                    *(gpp[0][k]*tang[1][j] + gpp[1][k]*tang[0][j]);
            if (j==jj) 
              dethess[k][kk][j][jj] += 2*gpp[0][k]*gpp[0][kk]*tt
                + ss*2*gpp[1][k]*gpp[1][kk]
                - 2*st*(gpp[0][k]*gpp[1][kk] + gpp[1][k]*gpp[0][kk]);
          }

     for ( k = 0 ; k < FACET_CTRL ; k++ )
      for ( kk = 0 ; kk < FACET_CTRL ; kk++ )
        for ( j = 0 ; j < SDIM ; j++ )
         for ( jj = 0 ; jj < SDIM ; jj++ )
            f_info->hess[k][kk][j][jj] += gauss2Dwt[m]*
              (derivs[jj]*gpoly[m][kk]/sqrt(det)/4*detgrad[k][j] 
                -0.5*val/sqrt(det)/det/4*detgrad[kk][jj]*detgrad[k][j]
                + val/sqrt(det)/4*dethess[k][kk][j][jj]
                + 1/sqrt(det)/4*detgrad[kk][jj]*derivs[j]*gpoly[m][k]
                + area*second[j][jj]*gpoly[m][k]*gpoly[m][kk]);
  }
  return value;
}


/*********************************************************************

                  Lagrange facet_scalar_integral method
                  2 D facets only, in any space dimension

*********************************************************************/

/*********************************************************************
*
* function: facet_scalar_integral_lagr()
*
* purpose:  method value
*
*/

REAL facet_scalar_integral_lagr(f_info)
struct qinfo *f_info;
{ int m;
  REAL value = 0.0;
  REAL area;
  REAL ss,st,tt;
  struct gauss_lag *gl = &gauss_lagrange[web.dimension][web.gauss2D_order];

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL **tang = f_info->sides[m];
    ss = SDIM_dot(tang[0],tang[0]);
    st = SDIM_dot(tang[0],tang[1]);
    tt = SDIM_dot(tang[1],tang[1]);
    area = ss*tt-st*st;
    if ( area <= 0.0 ) continue;
    f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    value += gl->gausswt[m]*sqrt(area)*
       eval(METH_INSTANCE(f_info->method)->expr[0],f_info->gauss_pt[m],f_info->id,NULL);
  } 
  return value/2; /* triangle factor */
}

/*********************************************************************
*
* function: facet_scalar_integral_lagr_grad()
*
* purpose:  method gradient
*
*/

REAL facet_scalar_integral_lagr_grad(f_info)
struct qinfo *f_info;
{ int m,j,k;
  REAL value = 0.0;
  REAL val;
  REAL derivs[MAXCOORD];
  REAL detgrad;
  REAL area,det;
  REAL ss,st,tt;
  struct gauss_lag *gl = &gauss_lagrange[web.dimension][web.gauss2D_order];

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL **tang = f_info->sides[m];
    ss = SDIM_dot(tang[0],tang[0]);
    st = SDIM_dot(tang[0],tang[1]);
    tt = SDIM_dot(tang[1],tang[1]);
    det = ss*tt-st*st;
    if ( det <= 0.0 ) continue;
    area = sqrt(det)/2; 
    f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    eval_all(METH_INSTANCE(f_info->method)->expr[0],f_info->gauss_pt[m],SDIM,&val,
                                                          derivs,f_info->id);
    value += gl->gausswt[m]*area*val;

    for ( k = 0 ; k < gl->lagpts ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
      { detgrad = 2*gl->gpolypart[m][0][k]*tang[0][j]*tt
                + ss*2*gl->gpolypart[m][1][k]*tang[1][j]
                - 2*st*(gl->gpolypart[m][0][k]*tang[1][j]
                            + gl->gpolypart[m][1][k]*tang[0][j]);

        f_info->grad[k][j] += gl->gausswt[m]*
           (val/sqrt(det)/4*detgrad + area*derivs[j]*gl->gpoly[m][k]);
      }
  }

  return value;
}


/*********************************************************************
*
* function: facet_scalar_integral_lagr_hess()
*
* purpose:  method gradient and hessian 
*
*/

REAL facet_scalar_integral_lagr_hess(f_info)
struct qinfo *f_info;
{ int m,j,k,kk,jj;
  REAL value = 0.0;
  REAL val;
  REAL derivs[MAXCOORD];
  REAL detgrad[MAXVCOUNT][MAXCOORD];
  REAL dethess;
  REAL area,det,ss,st,tt;
  MAT2D(second,MAXCOORD,MAXCOORD);
  struct gauss_lag *gl = &gauss_lagrange[web.dimension][web.gauss2D_order];

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL **gpp = gl->gpolypart[m];
    REAL **tang = f_info->sides[m];
    ss = SDIM_dot(tang[0],tang[0]);
    st = SDIM_dot(tang[0],tang[1]);
    tt = SDIM_dot(tang[1],tang[1]);
    det = ss*tt-st*st;
    if ( det <= 0.0 ) continue;
    area = sqrt(det)/2; 
    f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    eval_second(METH_INSTANCE(f_info->method)->expr[0],f_info->gauss_pt[m],SDIM,&val,
                                                   derivs,second,f_info->id);
    value += gl->gausswt[m]*area*val;

    /* gradients */
    for ( k = 0 ; k < gl->lagpts ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
      { detgrad[k][j] = 2*gpp[0][k]*tang[0][j]*tt + ss*2*gpp[1][k]*tang[1][j]
                         - 2*st*(gpp[0][k]*tang[1][j] + gpp[1][k]*tang[0][j]);

        f_info->grad[k][j] += gl->gausswt[m]*
            (val/sqrt(det)/4*detgrad[k][j] + area*derivs[j]*gl->gpoly[m][k]);
      }

    /* hessian */
    for ( k = 0 ; k < gl->lagpts ; k++ )
      for ( kk = 0 ; kk < gl->lagpts ; kk++ )
        for ( j = 0 ; j < SDIM ; j++ )
          for ( jj = 0 ; jj < SDIM ; jj++ )
          { dethess = 
                     2*gpp[0][k]*tang[0][j]*2*gpp[1][kk]*tang[1][jj]
                  + 2*gpp[0][kk]*tang[0][jj]*2*gpp[1][k]*tang[1][j]
                  - 2*(gpp[0][kk]*tang[1][jj] + gpp[1][kk]*tang[0][jj])
                    *(gpp[0][k]*tang[1][j] + gpp[1][k]*tang[0][j]);
            if (j==jj) 
              dethess += 2*gpp[0][k]*gpp[0][kk]*tt
                + ss*2*gpp[1][k]*gpp[1][kk]
                - 2*st*(gpp[0][k]*gpp[1][kk] + gpp[1][k]*gpp[0][kk]);

            f_info->hess[k][kk][j][jj] += gl->gausswt[m]*
              (derivs[jj]*gl->gpoly[m][kk]/sqrt(det)/4*detgrad[k][j] 
                -0.5*val/sqrt(det)/det/4*detgrad[kk][jj]*detgrad[k][j]
                + val/sqrt(det)/4*dethess
                + 1/sqrt(det)/4*detgrad[kk][jj]*derivs[j]*gl->gpoly[m][k]
                + area*second[j][jj]*gl->gpoly[m][k]*gl->gpoly[m][kk]);
          }
  }
  return value;
}


/*********************************************************************

                    facet_vector_integral method

Integral of vectorfield over facet.  2D facet in 3D only.

*********************************************************************/
/*********************************************************************
*
* function: facet_vector_integral_init()
*
* purpose:  Check illegalities
*
*/

void facet_vector_integral_init(mode,mi)
int mode;
struct method_instance *mi;
{
  if ( web.dimension != 2 )
     kb_error(1772,"facet_vector_integral method only for 2D facets.\n",RECOVERABLE);

  if ( SDIM != 3 )
     kb_error(1773,"facet_vector_integral method only for 3D space.\n",RECOVERABLE);

}

/*********************************************************************
*
* function: facet_vector_integral()
*
* purpose:  method value
*
*/

REAL facet_vector_integral(f_info)
struct qinfo *f_info;
{ int m,j;
  REAL value=0.0;
  if ( web.modeltype == QUADRATIC ) return facet_vector_integral_q(f_info);
  if ( web.modeltype == LAGRANGE ) return lagrange_vector_integral(f_info);
  for ( m = 0 ; m < gauss2D_num ; m++ )
  { 
    f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    for ( j = 0 ; j < SDIM ; j++ )
     { REAL  green = gauss2Dwt[m]*
         eval(METH_INSTANCE(f_info->method)->expr[j],f_info->gauss_pt[m],f_info->id,NULL);
       value += f_info->normal[j]*green;
     }
  }
  return value/2;  /* 2 is triangle factor for normal */
}

/*********************************************************************
*
* function: facet_vector_integral_grad()
*
* purpose:  method gradient
*
*/


REAL facet_vector_integral_grad(f_info)
struct qinfo *f_info;
{ int m,j,k;
  REAL value = 0.0;
  REAL val[MAXCOORD];
  REAL derivs[MAXCOORD][MAXCOORD];
  REAL sum;
  REAL cross0[MAXCOORD],cross1[MAXCOORD];
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;

  if ( web.modeltype == QUADRATIC ) return facet_vector_integral_q_grad(f_info);
  if ( web.modeltype == LAGRANGE ) return lagrange_vector_integral_grad(f_info);
  for ( j = 0 ; j < SDIM ; j++ ) 
    for ( m = 0 ; m < FACET_VERTS ; m++ )
       f_info->grad[m][j] = 0.0;
    for ( m = 0 ; m < gauss2D_num ; m++ )
    { REAL weight = sign*gauss2Dwt[m];
      f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
      for ( j = 0 ; j < SDIM ; j++ ) 
        eval_all(METH_INSTANCE(f_info->method)->expr[j],f_info->gauss_pt[m],SDIM,val+j,
                                                        derivs[j],f_info->id);
      value += weight*SDIM_dot(val,f_info->normal);
      cross_prod(val,f_info->sides[0][0],cross0);
      cross_prod(val,f_info->sides[0][1],cross1);
      for ( k = 0 ; k < SDIM ; k++ )
      { for ( sum = 0.0, j = 0 ; j < SDIM ; j++ )
          sum += derivs[j][k]*f_info->normal[j];
        f_info->grad[0][k] += weight*(gauss2Dpt[m][0]*sum
                    + cross1[k] - cross0[k])/2;
        f_info->grad[1][k] += weight*(gauss2Dpt[m][1]*sum
                    - cross1[k])/2;
        f_info->grad[2][k] += weight*(gauss2Dpt[m][2]*sum
                    + cross0[k])/2;
       }
     }

  return value/2;
}


/*********************************************************************
*
* function: facet_vector_integral_hess()
*
* purpose:  method gradient and hessian
*
*/

REAL facet_vector_integral_hess(f_info)
struct qinfo *f_info;
{ int m,i,j,k;
  REAL value = 0.0;
  REAL val[MAXCOORD];
  REAL derivs[MAXCOORD][MAXCOORD];
  REAL sum;
  REAL cross0[MAXCOORD],cross1[MAXCOORD];
  MAT3D(second,MAXCOORD,MAXCOORD,MAXCOORD);
  REAL *s1 = f_info->sides[0][0];
  REAL *s2 = f_info->sides[0][1];
  int p,q,r,s;
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;

  if ( web.modeltype == QUADRATIC ) return facet_vector_integral_q_hess(f_info);
  if ( web.modeltype == LAGRANGE ) return lagrange_vector_integral_hess(f_info);
  for ( m = 0 ; m < gauss2D_num ; m++ )
  { REAL weight = sign*gauss2Dwt[m];
    f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    for ( j = 0 ; j < SDIM ; j++ ) 
      eval_second(METH_INSTANCE(f_info->method)->expr[j],f_info->gauss_pt[m],SDIM,val+j,
                                              derivs[j],second[j],f_info->id);
    value += weight*SDIM_dot(val,f_info->normal);
    cross_prod(val,f_info->sides[0][0],cross0);
    cross_prod(val,f_info->sides[0][1],cross1);
    for ( k = 0 ; k < SDIM ; k++ )
    { for ( sum = 0.0, j = 0 ; j < SDIM ; j++ )
          sum += derivs[j][k]*f_info->normal[j];
      f_info->grad[0][k] += weight*(gauss2Dpt[m][0]*sum
                    + cross1[k] - cross0[k])/2;
      f_info->grad[1][k] += weight*(gauss2Dpt[m][1]*sum
                    - cross1[k])/2;
      f_info->grad[2][k] += weight*(gauss2Dpt[m][2]*sum
                    + cross0[k])/2;
    }
    for ( r = 0 ; r < FACET_VERTS ; r++ )
      for ( s = 0 ; s < FACET_VERTS ; s++ )
        for ( p = 0 ; p < SDIM ; p++ )
          for ( q = 0 ; q < SDIM ; q++ )
            { sum = 0.0;
              for ( i = 0 ; i < 3 ; i++ )
               { REAL tmp;
                 j = (i+1)%3;
                 k = (j+1)%3;

                 if ( (r==1) && (p==i) ) 
                  { sum += s2[j]*gauss2Dpt[m][s]*derivs[k][q];
                    if ( (s==2) && (q==j) ) sum += val[k];
                    if ( (s==0) && (q==j) ) sum -= val[k];
                  }
                 if ( (r==0) && (p==i) ) 
                  { sum -= s2[j]*gauss2Dpt[m][s]*derivs[k][q];
                    if ( (s==2) && (q==j) ) sum -= val[k];
                    if ( (s==0) && (q==j) ) sum += val[k];
                  }

                 if ( (r==2) && (p==j) ) 
                  { sum += s1[i]*gauss2Dpt[m][s]*derivs[k][q];
                    if ( (s==1) && (q==i) ) sum += val[k];
                    if ( (s==0) && (q==i) ) sum -= val[k];
                  }
                 if ( (r==0) && (p==j) ) 
                  { sum -= s1[i]*gauss2Dpt[m][s]*derivs[k][q];
                    if ( (s==1) && (q==i) ) sum -= val[k];
                    if ( (s==0) && (q==i) ) sum += val[k];
                  }

                 if ( (s==1) && (q==i) ) sum += s2[j]*gauss2Dpt[m][r]*derivs[k][p];
                 if ( (s==0) && (q==i) ) sum -= s2[j]*gauss2Dpt[m][r]*derivs[k][p];
                 if ( (s==2) && (q==j) ) sum += s1[i]*gauss2Dpt[m][r]*derivs[k][p];
                 if ( (s==0) && (q==j) ) sum -= s1[i]*gauss2Dpt[m][r]*derivs[k][p];
                 tmp = s1[i]*s2[j]*gauss2Dpt[m][r]*gauss2Dpt[m][s];
                 sum += tmp*second[k][p][q];
              }
              f_info->hess[r][s][p][q] += weight * sum/2;
              sum = 0.0;
              for ( i = 0 ; i < 3 ; i++ )
               { REAL tmp;
                 j = (i+2)%3;
                 k = (j+2)%3;
                 if ( (r==1) && (p==i) ) 
                  { sum += s2[j]*gauss2Dpt[m][s]*derivs[k][q];
                    if ( (s==2) && (q==j) ) sum += val[k];
                    if ( (s==0) && (q==j) ) sum -= val[k];
                  }
                 if ( (r==0) && (p==i) ) 
                  { sum -= s2[j]*gauss2Dpt[m][s]*derivs[k][q];
                    if ( (s==2) && (q==j) ) sum -= val[k];
                    if ( (s==0) && (q==j) ) sum += val[k];
                  }
                 if ( (r==2) && (p==j) ) 
                  { sum += s1[i]*gauss2Dpt[m][s]*derivs[k][q];
                    if ( (s==1) && (q==i) ) sum += val[k];
                    if ( (s==0) && (q==i) ) sum -= val[k];
                  }
                 if ( (r==0) && (p==j) ) 
                  { sum -= s1[i]*gauss2Dpt[m][s]*derivs[k][q];
                    if ( (s==1) && (q==i) ) sum -= val[k];
                    if ( (s==0) && (q==i) ) sum += val[k];
                  }
                 if ( (s==1) && (q==i) ) sum += s2[j]*gauss2Dpt[m][r]*derivs[k][p];
                 if ( (s==0) && (q==i) ) sum -= s2[j]*gauss2Dpt[m][r]*derivs[k][p];
                 if ( (s==2) && (q==j) ) sum += s1[i]*gauss2Dpt[m][r]*derivs[k][p];
                 if ( (s==0) && (q==j) ) sum -= s1[i]*gauss2Dpt[m][r]*derivs[k][p];
                 tmp = s1[i]*s2[j]*gauss2Dpt[m][r]*gauss2Dpt[m][s];
                 sum += tmp*second[k][p][q];
              }
              f_info->hess[r][s][p][q] -= weight * sum/2;
            }
     }

  return value/2;
}

/*********************************************************************

                  quadratic  facet_vector_integral method

Integral of vectorfield over facet.  2D facet in 3D only.

*********************************************************************/

/*********************************************************************
*
* function: facet_vector_integral_q()
*
* purpose:  method value
*
*/

REAL facet_vector_integral_q(f_info)
struct qinfo *f_info;
{ int m,j;
  REAL value=0.0,val[MAXCOORD];
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  for ( m = 0 ; m < gauss2D_num ; m++ )
  {
    REAL **tang = f_info->sides[m];
    f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    for ( j = 0 ; j < SDIM ; j++ )
    { int jj = (j+1)%SDIM; 
      int jjj = (j+2)%SDIM; 
      val[j] = gauss2Dwt[m]*
         eval(METH_INSTANCE(f_info->method)->expr[j],f_info->gauss_pt[m],f_info->id,NULL);
      value += val[j]*(tang[0][jj]*tang[1][jjj] - tang[0][jjj]*tang[1][jj]);
    }
  }
  return sign*value/2;  /* 2 is triangle factor */
}


/*********************************************************************
*
* function: facet_vector_integral_q_grad()
*
* purpose:  method gradient
*
*/

REAL facet_vector_integral_q_grad(f_info)
struct qinfo *f_info;
{ int m,i,j,k;
  REAL value = 0.0;
  REAL val;
  REAL derivs[MAXCOORD];
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;

  for ( k = 0 ; k < FACET_CTRL ; k++ )
     for ( j = 0 ; j < SDIM ; j++ ) 
        f_info->grad[k][j] = 0.0;

  for ( m = 0 ; m < gauss2D_num ; m++ )
  { 
     REAL **gpp = gpolypartial[m];
     REAL wt = sign*0.5*gauss2Dwt[m]; /* include triangle factor */
     REAL **tang = f_info->sides[m];
     f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
     for ( j = 0 ; j < SDIM ; j++ )
     { int jj = (j+1)%SDIM; 
       int jjj = (j+2)%SDIM; 
       eval_all(METH_INSTANCE(f_info->method)->expr[j],f_info->gauss_pt[m],SDIM,&val,
                                                        derivs,f_info->id);
       value += wt*val*(tang[0][jj]*tang[1][jjj] - tang[0][jjj]*tang[1][jj]);

       for ( k = 0 ; k < FACET_CTRL ; k++ )
        { 
          f_info->grad[k][jj] += wt*val
                *(gpp[0][k]*tang[1][jjj] - tang[0][jjj]*gpp[1][k]);
          f_info->grad[k][jjj] += wt*val
                *(tang[0][jj]*gpp[1][k] - gpp[0][k]*tang[1][jj]);
          for ( i = 0 ; i < SDIM ; i++ )
             f_info->grad[k][i] += wt*derivs[i]*gpoly[m][k]
                     *(tang[0][jj]*tang[1][jjj] - tang[0][jjj]*tang[1][jj]);
        }
     }
  }

  return value;
}

/*********************************************************************
*
* function: facet_vector_integral_q_hess()
*
* purpose:  method gradient and hessian
*
*/


REAL facet_vector_integral_q_hess(f_info)
struct qinfo *f_info;
{ int m,i,j,k,ii,kk;
  REAL value = 0.0;
  REAL val;
  REAL derivs[MAXCOORD];
  MAT2D(second,MAXCOORD,MAXCOORD);
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;

  for ( m = 0 ; m < gauss2D_num ; m++ )
   { REAL **gpp = gpolypartial[m];
     REAL wt = sign*0.5*gauss2Dwt[m]; /* include triangle factor */
     REAL **tang = f_info->sides[m];
     f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
     for ( j = 0 ; j < SDIM ; j++ )
     { int jj = (j+1)%SDIM; 
       int jjj = (j+2)%SDIM; 
       eval_second(METH_INSTANCE(f_info->method)->expr[j],f_info->gauss_pt[m],SDIM,
               &val, derivs,second,f_info->id);
       value += wt*val*(tang[0][jj]*tang[1][jjj] - tang[0][jjj]*tang[1][jj]);

       for ( k = 0 ; k < FACET_CTRL ; k++ )
       { f_info->grad[k][jj] += wt*val
                *(gpp[0][k]*tang[1][jjj] - tang[0][jjj]*gpp[1][k]);
         f_info->grad[k][jjj] += wt*val
                *(tang[0][jj]*gpp[1][k] - gpp[0][k]*tang[1][jj]);
         for ( i = 0 ; i < SDIM ; i++ )
             f_info->grad[k][i] += wt*derivs[i]*gpoly[m][k]
                     *(tang[0][jj]*tang[1][jjj] - tang[0][jjj]*tang[1][jj]);
       }

        /* hessian */
        for ( k = 0 ; k < FACET_CTRL ; k++ )
          for ( kk = 0 ; kk < FACET_CTRL ; kk++ )
          { f_info->hess[k][kk][jj][jjj] += wt*val
                 *(gpp[0][k]*gpp[1][kk] - gpp[0][kk]*gpp[1][k]);
             f_info->hess[k][kk][jjj][jj] += wt*val
                 *(gpp[0][kk]*gpp[1][k] - gpp[0][k]*gpp[1][kk]);
             for ( ii = 0 ; ii < SDIM ; ii++ )
             { f_info->hess[k][kk][jj][ii] += wt*derivs[ii]*gpoly[m][kk]
                  *(gpp[0][k]*tang[1][jjj] - tang[0][jjj]*gpp[1][k]);
                f_info->hess[k][kk][jjj][ii] += wt*derivs[ii]*gpoly[m][kk]
                  *(tang[0][jj]*gpp[1][k] - gpp[0][k]*tang[1][jj]);
             }
             for ( i = 0 ; i < SDIM ; i++ )
             {
                for ( ii = 0 ; ii < SDIM ; ii++ )
                  f_info->hess[k][kk][i][ii] += 
                      wt*second[i][ii]*gpoly[m][k]*gpoly[m][kk]
                     *(tang[0][jj]*tang[1][jjj] - tang[0][jjj]*tang[1][jj]);
                f_info->hess[k][kk][i][jj] += wt*derivs[i]*gpoly[m][k]
                     *(gpp[0][kk]*tang[1][jjj] - tang[0][jjj]*gpp[1][kk]);
                f_info->hess[k][kk][i][jjj] += wt*derivs[i]*gpoly[m][k]
                     *(tang[0][jj]*gpp[1][kk] - gpp[0][kk]*tang[1][jj]);
             }
          }
     }
  }

  return value;
}


/***********************************************************************
    Named Method spherical_area
    Assumes vertices of a facet are on a unit sphere (in any dimension).
    Value is the area of the spherical triangle.
***********************************************************************/


REAL spherical_area_value(f_info)
struct qinfo *f_info;
{ int i;
  REAL area;
  REAL a[FACET_EDGES+2];  /* squares of side lengths */
  REAL *b;
 
  /* Value done by angle excess */
  a[0] = SDIM_dot(f_info->sides[0][0],f_info->sides[0][0]);
  a[2] = SDIM_dot(f_info->sides[0][1],f_info->sides[0][1]);
  a[1] = a[0] + a[2] - 2*SDIM_dot(f_info->sides[0][0],f_info->sides[0][1]);
  a[FACET_EDGES] = a[0];
  a[FACET_EDGES+1] = a[1];
  for ( area = -M_PI, i = 0, b = a ; i < FACET_VERTS ; i++,b++ )
  { REAL v = sqrt(b[1]*b[2]*(1-b[1]/4)*(1-b[2]/4));
     REAL w = (b[1]+b[2]-b[0]-b[1]*b[2]/2);
     REAL u = w/2/v;
     area += acos(u);
  }
  return area;
}


REAL spherical_area_grad(f_info)
struct qinfo *f_info;
{ int i,j;
  REAL area;
  REAL a[FACET_EDGES+2];  /* squares of side lengths */
  REAL *b;
  REAL *x[FACET_VERTS+2];
 
  /* Value done by angle excess */
  a[0] = SDIM_dot(f_info->sides[0][0],f_info->sides[0][0]);
  a[2] = SDIM_dot(f_info->sides[0][1],f_info->sides[0][1]);
  a[1] = a[0] + a[2] - 2*SDIM_dot(f_info->sides[0][0],f_info->sides[0][1]);
  a[FACET_EDGES] = a[0];
  a[FACET_EDGES+1] = a[1];
  for ( i = 0 ; i < FACET_VERTS ; i++ ) x[i] = f_info->x[i];
  x[FACET_VERTS] = x[0];
  x[FACET_VERTS+1] = x[1];
  for ( area = -M_PI, i = 0, b = a ; i < FACET_VERTS ; i++,b++ )
  { REAL v = sqrt(b[1]*b[2]*(1-b[1]/4)*(1-b[2]/4));
    REAL w = (b[1]+b[2]-b[0]-b[1]*b[2]/2);
    REAL u = w/2/v;
    REAL dudb0 = -1/2./v;
    REAL dudb1 = (1-b[2]/2)/2./v - w/4/v/v/v*(b[2]*(1-b[1]/4)*(1-b[2]/4)-b[1]*b[2]*(1./4)*(1-b[2]/4));
    REAL dudb2 = (1-b[1]/2)/2./v - w/4/v/v/v*(b[1]*(1-b[1]/4)*(1-b[2]/4)-b[1]*b[2]*(1./4)*(1-b[1]/4));
    area += acos(u);
    for ( j = 0 ; j < SDIM ; j++ )
     { f_info->grad[i][j] += -1/sqrt(1-u*u)*2*(dudb2*(x[i][j]-x[i+2][j])+dudb0*(x[i][j]-x[i+1][j]));
       f_info->grad[(i+1)%3][j] += -1/sqrt(1-u*u)*2*(dudb1*(x[i+1][j]-x[i+2][j])+dudb0*(x[i+1][j]-x[i][j]));
       f_info->grad[(i+2)%3][j] += -1/sqrt(1-u*u)*2*(dudb1*(x[i+2][j]-x[i+1][j])+dudb2*(x[i+2][j]-x[i][j]));
     }
  }

  return area;
}

