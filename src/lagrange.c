/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*********************************************************************
*
*     file:        lagrange.c
*
*/

#include "include.h"

/*********************************************************************
**********************************************************************
                     Lagrange model quantities
**********************************************************************
**********************************************************************/



/*********************************************************************
                     Edge length quantity

    Uses Gaussian integration.  Not known to be upper bound on area.

**********************************************************************/

/*********************************************************************
*
*  Returns energy due to edge.
*  Lagrange version.
*/

REAL lagrange_edge_tension_value(e_info)
struct qinfo *e_info;
{
  REAL value = 0.0;
  int m,i,j;
  REAL det;
  REAL **tang;
  int dim = (web.representation==STRING) ? 1 : web.dimension-1;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss1D_order];
  MAT2D(mat,MAXCOORD,MAXCOORD);
     
  for ( m = 0 ; m < gl->gnumpts ; m++ )  /*  integration point number */
  { tang = e_info->sides[m];
     for ( i = 0 ; i < dim ; i++ )
        for ( j = 0 ; j <= i ; j++ )
          mat[i][j] = mat[j][i] = SDIM_dot(tang[i],tang[j]);
     det = det_adjoint(mat,dim);
     value += gl->gausswt[m]*sqrt(det);
  }

  value /= factorial[dim]; /* triangle factor */
  if ( METH_INSTANCE(e_info->method)->flags & DEFAULT_INSTANCE )
  { set_edge_length(e_info->id,value);
#ifdef SHARED_MEMORY
     if ( nprocs > 1 ) 
      proc_total_area[GET_THREAD_ID] += value;
     else
#endif
     binary_tree_add(web.total_area_addends,value);
  }
  if ( METH_INSTANCE(e_info->method)->flags & USE_DENSITY )
      value *= get_edge_density(e_info->id);
  return value;

}

/*********************************************************************
*
*  Returns gradient and energy due to edge.
*  Lagrange version.
*/

REAL lagrange_edge_tension_grad(e_info)
struct qinfo *e_info;
{
  REAL value = 0.0;
  int i,ii,m,j,k;
  REAL det;
  REAL density,fudge;
  int dim = (web.representation==STRING) ? 1 : web.dimension-1;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss1D_order];
  MAT2D(mat,MAXCOORD,MAXCOORD);

  if ( METH_INSTANCE(e_info->method)->flags & USE_DENSITY )
      density = get_edge_density(e_info->id);
  else density = 1.0;

  for ( m = 0 ; m < gl->gnumpts ; m++ )  /*  integration point number */
  { REAL **tang = e_info->sides[m];
     /* calculate tangents and det */ 
     for ( i = 0 ; i < dim ; i++ )
        for ( j = 0 ; j <= i ; j++ )
          mat[i][j] = mat[j][i] = SDIM_dot(tang[i],tang[j]);
     det = det_adjoint(mat,dim);
     value += gl->gausswt[m]*sqrt(det);
     /* gradients */
     fudge = density*gl->gausswt[m]/sqrt(det)/factorial[dim];
     for ( k = 0 ; k < gl->lagpts ; k++ )
        for ( j = 0 ; j < SDIM ; j++ )
        { REAL sum = 0.0;
          for ( i = 0 ; i < dim ; i++ )
             for ( ii = 0 ; ii < dim ; ii++ )
                sum += tang[i][j]*gl->gpolypart[m][ii][k]*mat[i][ii];
          e_info->grad[k][j] += fudge*sum;
        }
  }

  return density*value/factorial[dim];

}


/*********************************************************************
*
*  Returns hessian, gradient and energy due to edge.
*  Lagrange version.
*/

REAL lagrange_edge_tension_hess(e_info)
struct qinfo *e_info;
{
  REAL value = 0.0;
  int i,ii,m,j,k,jj,kk;
  REAL det;
  REAL density,fudge;
  int dim = (web.representation==STRING) ? 1 : web.dimension-1;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss1D_order];
  MAT2D(mat,MAXCOORD,MAXCOORD);
  REAL **sums;
  REAL ****dethess=NULL;
  REAL detinv;

  if ( METH_INSTANCE(e_info->method)->flags & USE_DENSITY )
      density = get_edge_density(e_info->id);
  else density = 1.0;

  sums = dmatrix(0,gl->lagpts-1,0,SDIM-1);
  if ( dim > 2 ) dethess = dmatrix4(dim,dim,dim,dim);

  for ( m = 0 ; m < gl->gnumpts ; m++ )  /*  integration point number */
  { REAL **tang = e_info->sides[m];
     REAL **gp = gl->gpolypart[m];
     /* calculate tangents and det */ 
     for ( i = 0 ; i < dim ; i++ )
        for ( j = 0 ; j <= i ; j++ )
          mat[i][j] = mat[j][i] = SDIM_dot(tang[i],tang[j]);
     if ( dim > 2 ) det_hess(mat,dethess,dim);  /* for hessian */
     det = det_adjoint(mat,dim);
     detinv = (det == 0.0) ? 0.0 : 1/det;
     value += gl->gausswt[m]*sqrt(det);
     /* gradients */
     fudge = density*gl->gausswt[m]*sqrt(detinv)/factorial[dim];
     for ( k = 0 ; k < gl->lagpts ; k++ )
        for ( j = 0 ; j < SDIM ; j++ )
        { REAL sum = 0.0;
          for ( i = 0 ; i < dim ; i++ )
             for ( ii = 0 ; ii < dim ; ii++ )
                sum += tang[i][j]*gp[ii][k]*mat[i][ii];
          sums[k][j] = sum;
          e_info->grad[k][j] += fudge*sum;
        }
     /* hessians */
     for ( k = 0 ; k < gl->lagpts ; k++ )
      for ( kk = 0 ; kk <= k ; kk++ )
      { REAL  jjjsum;
        REAL gp01;
        REAL gp10;
        REAL gp00=0.0;
        REAL gp11=0.0;
        REAL coeff1=0.0;
        REAL coeff2=0.0;
        if ( dim == 2 )
        { gp01 = gp[0][k]*gp[1][kk];
          gp10 = gp[1][k]*gp[0][kk];
          gp00 = gp[0][k]*gp[0][kk];
          gp11 = gp[1][k]*gp[1][kk];
          coeff1 = 2*gp01-gp10;
          coeff2 = 2*gp10-gp01;
        }
        for ( i = 0,jjjsum=0.0 ; i < dim ; i++ )
          for ( ii = 0 ; ii < dim ; ii++ )
            jjjsum += gp[i][k]*gp[ii][kk]*mat[i][ii];
        for ( j = 0 ; j < SDIM ; j++ )
        { int jjend = (k==kk) ? j+1 : SDIM;
          for ( jj = 0 ; jj < jjend ; jj++ )
          { REAL sum,h;
            int i1,i2,jj1,jj2;
            h = -sums[k][j]*sums[kk][jj]*detinv;
            if ( dim == 2 )
            {
              h +=  (coeff1*tang[0][j] - gp00*tang[1][j])*tang[1][jj]
                    + (coeff2*tang[1][j] - gp11*tang[0][j])*tang[0][jj];
            }
            else if ( dim > 2 )
            { sum = 0.0;
              for (i1 = 0 ; i1 < dim ; i1++ )
              { REAL suma = 0.0;
                 for ( jj1 = 0 ; jj1 < dim ; jj1++ )
                 { REAL sumb = 0.0;
                    for ( i2 = 0 ; i2 < dim ; i2++ )
                    { /* note: inner loops here pretty well optimized */
                      for ( jj2 = 0 ; jj2 < dim ; jj2++ ) 
                      { REAL dh = dethess[i1][jj1][i2][jj2];
                         if ( dh == 0.0 ) continue; 
                         /* using symmetry of dethess */
                         sumb += dh*(tang[i2][jj]*gp[jj2][kk]+tang[jj2][jj]*gp[i2][kk]);
                      }
                    }
                    suma += gp[jj1][k]*sumb;
                 }
                 sum += tang[i1][j]*suma;
              }
              h += sum;
            }
            if ( j==jj ) h += jjjsum;
            h *= fudge;
            e_info->hess[k][kk][j][jj] += h;
            if ( (kk != k) || (jj != j) )
              e_info->hess[kk][k][jj][j] += h;
          }
        }
     }
  }

  free_matrix(sums);
  if ( dim > 2 ) free_matrix4(dethess);

  return density*value/factorial[dim];

}

/*********************************************************************
                     Film area quantity

    Uses Gaussian integration.  Not known to be upper bound on area.

**********************************************************************/

/*********************************************************************
*
*  Returns energy due to facet.
*  Lagrange version.
*/

REAL lagrange_facet_tension_value(f_info)
struct qinfo *f_info;
{
  REAL value = 0.0;
  int m,i,j;
  REAL det;
  REAL **tang;
  int dim = web.dimension;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];
  MAT2D(mat,MAXCOORD,MAXCOORD);

  for ( m = 0 ; m < gl->gnumpts ; m++ )  /*  integration point number */
  { tang = f_info->sides[m];
     for ( i = 0 ; i < dim ; i++ )
        for ( j = 0 ; j <= i ; j++ )
          mat[i][j] = mat[j][i] = SDIM_dot(tang[i],tang[j]);
     det = det_adjoint(mat,dim);
     if ( det <= 0.0 ) continue;
     value += gl->gausswt[m]*sqrt(det);
  }

  value /= factorial[dim]; /* triangle factor */
  if ( METH_INSTANCE(f_info->method)->flags & DEFAULT_INSTANCE )
  { set_facet_area(f_info->id,value);
#ifdef SHARED_MEMORY
     if ( nprocs > 1 ) 
      proc_total_area[GET_THREAD_ID] += value;
     else
#endif
     binary_tree_add(web.total_area_addends,value);
  }
  if ( METH_INSTANCE(f_info->method)->flags & USE_DENSITY )
      value *= get_facet_density(f_info->id);
  return value;

}

/*********************************************************************
*
*  Returns gradient and energy due to facet.
*  Lagrange version.
*/

REAL lagrange_facet_tension_grad(f_info)
struct qinfo *f_info;
{
  REAL value = 0.0;
  int i,ii,m,j,k;
  REAL det;
  REAL density,fudge;
  int dim = web.dimension;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];
  MAT2D(mat,MAXCOORD,MAXCOORD);

  if ( METH_INSTANCE(f_info->method)->flags & USE_DENSITY )
      density = get_facet_density(f_info->id);
  else density = 1.0;

  for ( m = 0 ; m < gl->gnumpts ; m++ )  /*  integration point number */
  { REAL **tang = f_info->sides[m];
     /* calculate tangents and det */ 
     for ( i = 0 ; i < dim ; i++ )
        for ( j = 0 ; j <= i ; j++ )
          mat[i][j] = mat[j][i] = SDIM_dot(tang[i],tang[j]);
     det = det_adjoint(mat,dim);
     if ( det <= 0.0 ) continue;
     value += gl->gausswt[m]*sqrt(det);
     /* gradients */
     fudge = density*gl->gausswt[m]/sqrt(det)/factorial[dim];
     for ( k = 0 ; k < gl->lagpts ; k++ )
        for ( j = 0 ; j < SDIM ; j++ )
        { REAL sum = 0.0;
          for ( i = 0 ; i < dim ; i++ )
             for ( ii = 0 ; ii < dim ; ii++ )
                sum += tang[i][j]*gl->gpolypart[m][ii][k]*mat[i][ii];
          f_info->grad[k][j] += fudge*sum;
        }
  }

  return density*value/factorial[dim];

}

/*********************************************************************
*
*  Returns hessian, gradient and energy due to facet.
*  Lagrange version.
*/

REAL lagrange_facet_tension_hess(f_info)
struct qinfo *f_info;
{
  REAL value = 0.0;
  int i,ii,m,j,k,jj,kk;
  REAL det;
  REAL density,fudge;
  int dim = web.dimension;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];
  MAT2D(mat,MAXCOORD,MAXCOORD);
  REAL **sums;
  REAL ****dethess=NULL;
  REAL detinv;

  if ( METH_INSTANCE(f_info->method)->flags & USE_DENSITY )
      density = get_facet_density(f_info->id);
  else density = 1.0;

  sums = dmatrix(0,gl->lagpts-1,0,SDIM-1);
  if ( dim > 2 ) dethess = dmatrix4(dim,dim,dim,dim);

  for ( m = 0 ; m < gl->gnumpts ; m++ )  /*  integration point number */
  { REAL **tang = f_info->sides[m];
    REAL **gp = gl->gpolypart[m];

    /* calculate tangents and det */ 
    for ( i = 0 ; i < dim ; i++ )
       for ( j = 0 ; j <= i ; j++ )
          mat[i][j] = mat[j][i] = SDIM_dot(tang[i],tang[j]);
    if ( dim > 2 ) det_hess(mat,dethess,dim);  /* for hessian */
    det = det_adjoint(mat,dim);
    if ( det <= 0.0 ) continue;
    detinv = (det == 0.0) ? 0.0 : 1/det;
    value += gl->gausswt[m]*sqrt(det);

    /* gradients */
    fudge = density*gl->gausswt[m]*sqrt(detinv)/factorial[dim];
    for ( k = 0 ; k < gl->lagpts ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
      { REAL sum = 0.0;
        for ( i = 0 ; i < dim ; i++ )
           for ( ii = 0 ; ii < dim ; ii++ )
              sum += tang[i][j]*gp[ii][k]*mat[i][ii];
        sums[k][j] = sum;
        f_info->grad[k][j] += fudge*sum;
      }

    /* hessians */
    for ( k = 0 ; k < gl->lagpts ; k++ )
      for ( kk = 0 ; kk <= k ; kk++ )
      { REAL  jjjsum;
        REAL gp01;
        REAL gp10;
        REAL gp00=0.0;
        REAL gp11=0.0;
        REAL coeff1=0.0;
        REAL coeff2=0.0;
        if ( dim == 2 )
        { gp01 = gp[0][k]*gp[1][kk];
          gp10 = gp[1][k]*gp[0][kk];
          gp00 = gp[0][k]*gp[0][kk];
          gp11 = gp[1][k]*gp[1][kk];
          coeff1 = 2*gp01-gp10;
          coeff2 = 2*gp10-gp01;
        }
        for ( i = 0,jjjsum=0.0 ; i < dim ; i++ )
          for ( ii = 0 ; ii < dim ; ii++ )
            jjjsum += gp[i][k]*gp[ii][kk]*mat[i][ii];
        for ( j = 0 ; j < SDIM ; j++ )
        { int jjend = (k==kk) ? j+1 : SDIM;
          REAL term1 =  (coeff1*tang[0][j] - gp00*tang[1][j]);
          REAL term2 =  (coeff2*tang[1][j] - gp11*tang[0][j]);
          REAL hterm = -sums[k][j]*detinv;

          for ( jj = 0 ; jj < jjend ; jj++ )
          { REAL sum,h;
            int i1,i2,jj1,j2;

            h = hterm*sums[kk][jj];
            if ( dim == 2 )
            {
              h +=  term1*tang[1][jj] + term2*tang[0][jj];
            }
            else if ( dim > 2 )
            { sum = 0.0;
              for (i1 = 0 ; i1 < dim ; i1++ )
              { REAL suma = 0.0;
                 for ( jj1 = 0 ; jj1 < dim ; jj1++ )
                 { REAL sumb = 0.0;
                    for ( i2 = 0 ; i2 < dim ; i2++ )
                    { /* note: inner loops here pretty well optimized */
                      for ( j2 = 0 ; j2 < dim ; j2++ ) 
                      { REAL dh = dethess[i1][jj1][i2][j2];
                         if ( dh == 0.0 ) continue; 
                         /* using symmetry of dethess */
                         sumb += dh*(tang[i2][jj]*gp[j2][kk]+tang[j2][jj]*gp[i2][kk]);
                      }
                    }
                    suma += gp[jj1][k]*sumb;
                 }
                 sum += tang[i1][j]*suma;
              }
              h += sum;
            }
            if ( j==jj ) h += jjjsum;
            h *= fudge;
            f_info->hess[k][kk][j][jj] += h;

/* do outside of gauss point loop
            if ( (kk != k) || (jj != j) )
              f_info->hess[kk][k][jj][j] += h;
*/
          }
        }
     }
  }

  /* transpose part */
  for ( k = 0 ; k < gl->lagpts ; k++ )
    for ( kk = 0 ; kk <= k ; kk++ )
      for ( j = 0 ; j < SDIM ; j++ )
      { int jjend = (k==kk) ? j+1 : SDIM;
        for ( jj = 0 ; jj < jjend ; jj++ )
        { if ( (kk != k) || (jj != j) )
             f_info->hess[kk][k][jj][j] += f_info->hess[k][kk][j][jj];
        }
      }

  free_matrix(sums);
  if ( dim > 2 ) free_matrix4(dethess);

  return density*value/factorial[dim];

}


/*********************************************************************

                  Lagrange edge_vector_integral method
                  For 1D edges only.

*********************************************************************/

/*********************************************************************
*
* function: edge_vector_integral_lagrange()
*
* purpose:  method value
*
*/

REAL edge_vector_integral_lagrange(e_info)
struct qinfo *e_info;
{ int m,j,k;
  REAL value=0.0;
  REAL tang[MAXCOORD];
  REAL sign = (get_eattr(e_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  struct gauss_lag *gl = &gauss_lagrange[1][web.gauss1D_order];

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL weight = sign*gl->gausswt[m];
    e_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
     for ( j = 0 ; j < SDIM ; j++ )
     { tang[j] = 0.0;
        for ( k = 0 ; k < gl->lagpts ; k++ )
          tang[j] += gl->gpolypart[m][0][k]*e_info->x[k][j];
        value += weight*tang[j]*eval(METH_INSTANCE(abs(e_info->method))->expr[j],
                  e_info->gauss_pt[m],e_info->id,NULL);
     }
  }
  return value;
}

/*********************************************************************
*
* function: edge_vector_integral_lagrange_grad()
*
* purpose:  method gradient
*
*/


REAL edge_vector_integral_lagrange_grad(e_info)
struct qinfo *e_info;
{ int m,j,k,i;
  REAL value = 0.0;
  REAL val[MAXCOORD];
  REAL derivs[MAXCOORD][MAXCOORD];
  REAL sum;
  REAL tang[MAXCOORD];
  REAL sign = (get_eattr(e_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  struct gauss_lag *gl = &gauss_lagrange[1][web.gauss1D_order];

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL weight = sign*gl->gausswt[m];
    e_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
     for ( j = 0 ; j < SDIM ; j++ ) 
        { tang[j] = 0.0;
          for ( k = 0 ; k < gl->lagpts ; k++ )
              tang[j] += gl->gpolypart[m][0][k]*e_info->x[k][j];
        }
        for ( j = 0 ; j < SDIM ; j++ ) 
          eval_all(METH_INSTANCE(abs(e_info->method))->expr[j],
               e_info->gauss_pt[m],SDIM,val+j,derivs[j],e_info->id);
        value += weight*SDIM_dot(val,tang);
        for ( k = 0 ; k < SDIM ; k++ )
          { for ( sum = 0.0, j = 0 ; j < SDIM ; j++ )
                 sum += derivs[j][k]*tang[j];
             for ( i = 0 ; i < gl->lagpts ; i++ )
                e_info->grad[i][k] += 
                  weight*(gl->gpolypart[m][0][i]*val[k] + gl->gpoly[m][i]*sum);
          }
     }

  return value;
}

/*********************************************************************
*
* function: edge_vector_integral_lagrange_hess()
*
* purpose:  method gradient and hessian
*
*/


REAL edge_vector_integral_lagrange_hess(e_info)
struct qinfo *e_info;
{ int m,i,j,k,ii,kk;
  REAL value = 0.0;
  REAL val[MAXCOORD];
  REAL derivs[MAXCOORD][MAXCOORD];
  REAL sum;
  MAT3D(second,MAXCOORD,MAXCOORD,MAXCOORD);
  REAL tang[MAXCOORD];
  REAL sign = (get_eattr(e_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  struct gauss_lag *gl = &gauss_lagrange[1][web.gauss1D_order];

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL weight = sign*gl->gausswt[m];
    e_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
     for ( j = 0 ; j < SDIM ; j++ ) 
        { tang[j] = 0.0;
          for ( k = 0 ; k < gl->lagpts ; k++ )
              tang[j] += gl->gpolypart[m][0][k]*e_info->x[k][j];
        }
        for ( j = 0 ; j < SDIM ; j++ ) 
          eval_second(METH_INSTANCE(abs(e_info->method))->expr[j],
            e_info->gauss_pt[m],SDIM,val+j,derivs[j],second[j],e_info->id);
        value += weight*SDIM_dot(val,tang);
        for ( k = 0 ; k < SDIM ; k++ )
          { for ( sum = 0.0, j = 0 ; j < SDIM ; j++ )
                 sum += derivs[j][k]*tang[j];
             for ( i = 0 ; i < gl->lagpts ; i++ )
                e_info->grad[i][k] += 
                  weight*(gl->gpolypart[m][0][i]*val[k] + gl->gpoly[m][i]*sum);
          }

        for ( ii = 0 ; ii < SDIM ; ii++ )
         for ( i = 0 ; i < SDIM ; i++ )
          { for ( sum = 0.0, j = 0 ; j < SDIM ; j++ )
                 sum += second[j][ii][i]*tang[j];
             for ( k = 0 ; k < gl->lagpts ; k++ )
                for ( kk = 0 ; kk < gl->lagpts ; kk++ )
                  e_info->hess[k][kk][i][ii] += weight*
                  ( sum*gl->gpoly[m][k]*gl->gpoly[m][kk]
                     + gl->gpolypart[m][0][k]*derivs[i][ii]*gl->gpoly[m][kk]
                     + gl->gpolypart[m][0][kk]*derivs[ii][i]*gl->gpoly[m][k]
                  );
          }
     }

  return value;
}



/*********************************************************************

                    facet_vector_integral  method

Integral of vectorfield over facet.  nD facet in (n+1)D only.
Lagrange model

*********************************************************************/
REAL lagrange_vector_integral_all ARGS((struct qinfo*,int));

/*********************************************************************
*
* function: lagrange_vector_integral()
*
* purpose:  method value
*
*/

REAL lagrange_vector_integral(f_info)
struct qinfo *f_info;
{ int i,m,j;
  REAL value=0.0;
  MAT2D(mat,MAXCOORD,MAXCOORD);
  int dim = web.dimension;
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { 
    for ( i = 0 ; i < dim ; i++ )
      for ( j = 0 ; j < SDIM ; j++ )
          mat[i][j] = f_info->sides[m][i][j];
    f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    for ( j = 0 ; j < SDIM ; j++ )
      mat[dim][j] = 
         eval(METH_INSTANCE(abs(f_info->method))->expr[j],
              f_info->gauss_pt[m],f_info->id,NULL);
     value += gl->gausswt[m]*det_adjoint(mat,SDIM);
  }
  return sign*value/factorial[dim]; 
}

/*********************************************************************
*
* function: lagrange_vector_integral_grad()
*
* purpose:  method gradient
*
*/


REAL lagrange_vector_integral_grad(f_info)
struct qinfo *f_info;
{ int i,m,j,k,jj;
  REAL value = 0.0;
  REAL val[MAXCOORD];
  REAL derivs[MAXCOORD][MAXCOORD];
  MAT2D(mat,MAXCOORD,MAXCOORD);
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  int dim = web.dimension;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL weight = sign*gl->gausswt[m]/factorial[dim];
    f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
     for ( i = 0 ; i < dim ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
          mat[i][j] = f_info->sides[m][i][j];  /* mat destroyed by det */
     for ( j = 0 ; j < SDIM ; j++ )
     { eval_all(METH_INSTANCE(abs(f_info->method))->expr[j],f_info->gauss_pt[m],SDIM,
             val+j, derivs[j],f_info->id);
       mat[web.dimension][j] = val[j];
     }
     value += weight*det_adjoint(mat,SDIM);
     for ( k = 0 ; k < gl->lagpts; k++ )
      for ( j = 0 ; j < SDIM ; j++ ) 
        for ( i = 0 ; i < dim ; i++ )
        { f_info->grad[k][j] += weight*gl->gpolypart[m][i][k]*mat[j][i];
        }
     for ( k = 0 ; k < gl->lagpts ; k++ )
      for ( j = 0 ; j < SDIM ; j++ ) 
        for ( jj = 0 ; jj < SDIM ; jj++ ) 
        {
          f_info->grad[k][j] += weight
                *gl->gpoly[m][k]*derivs[jj][j]*mat[jj][dim];
        }
  }
  return value;  
}

/*********************************************************************
*
* function: lagrange_vector_integral_hess()
*
* purpose:  method hessian
*
*/
REAL lagrange_vector_integral_hess(f_info)
struct qinfo *f_info;
{ return lagrange_vector_integral_all(f_info,METHOD_HESSIAN);
}
REAL lagrange_vector_integral_all(f_info,mode)
struct qinfo *f_info;
int mode;
{ int i,m,j,k,jj,ii,kk;
  REAL value = 0.0;
  REAL val[MAXCOORD];
  REAL derivs[MAXCOORD][MAXCOORD];
  MAT3D(seconds,MAXCOORD,MAXCOORD,MAXCOORD);
  MAT2D(mat,MAXCOORD,MAXCOORD);
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  int dim = web.dimension;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];
  MAT4D(dethess,MAXCOORD,MAXCOORD,MAXCOORD,MAXCOORD);

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL det;
    REAL weight = sign*gl->gausswt[m]/factorial[dim];
    f_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    for ( i = 0 ; i < web.dimension ; i++ )
      for ( j = 0 ; j < SDIM ; j++ )
          mat[i][j] = f_info->sides[m][i][j];  /* mat destroyed by det */
    for ( j = 0 ; j < SDIM ; j++ )
    { eval_second(METH_INSTANCE(abs(f_info->method))->expr[j],
             f_info->gauss_pt[m],SDIM,val+j, derivs[j],seconds[j],f_info->id);
      mat[web.dimension][j] = val[j];
     }
     det_hess(mat,dethess,SDIM);
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
      for ( j = 0 ; j < SDIM ; j++ ) 
        for ( jj = 0 ; jj < SDIM ; jj++ ) 
        {
          f_info->grad[k][j] += weight
                *gl->gpoly[m][k]*derivs[jj][j]*mat[jj][dim];
        }

     if ( mode == METHOD_GRADIENT ) continue;

     /* hessian */
     for ( k = 0 ; k < gl->lagpts  ; k++ )
        for ( kk = 0 ; kk < gl->lagpts ; kk++ )
          for ( j = 0 ; j < SDIM ; j++ )
             for ( jj = 0 ; jj < SDIM ; jj++ )
             { REAL h = 0.0;
                int jjj;
                for ( i = 0 ; i < dim ; i++ )
                 for ( ii  = 0 ; ii < dim ; ii++ )
                 { h += dethess[i][j][ii][jj]
                            *gl->gpolypart[m][i][k]*gl->gpolypart[m][ii][kk];
                 }
                for ( i = 0 ; i < dim ; i++ )
                 for ( jjj = 0 ; jjj < SDIM ; jjj++ )
                  h += dethess[i][j][dim][jjj]*gl->gpolypart[m][i][k]
                              *gl->gpoly[m][kk]*derivs[jjj][jj];
                for ( ii = 0 ; ii < dim ; ii++ )
                 for ( jjj = 0 ; jjj < SDIM ; jjj++ )
                  h += dethess[dim][jjj][ii][jj]*gl->gpolypart[m][ii][kk]
                              *gl->gpoly[m][k]*derivs[jjj][j];
                for ( i = 0 ; i < SDIM ; i++ ) 
                  h += gl->gpoly[m][k]*gl->gpoly[m][kk]*seconds[i][j][jj]
                            *mat[i][web.dimension];
                f_info->hess[k][kk][j][jj]  += weight*h;
            }
  }

  return value;  
}



/*********************************************************************

                    lagrange_k_vector_integral  method

Integral of simple k-vectorfield over element. Edges and facets.
Lagrange model

*********************************************************************/
REAL lagrange_k_vector_integral_all ARGS((struct qinfo*,int));

/*********************************************************************
*
* function: lagrange_k_vector_integral()
*
* purpose:  method value
*
*/

REAL lagrange_k_vector_integral(f_info)
struct qinfo *f_info;
{ int i,m,j,k;
  REAL value=0.0;
  MAT2D(mat,MAXCOORD,MAXCOORD);
  int dim = web.dimension;
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];

  if ( id_type(f_info->id) == EDGE )
  { dim = (web.representation==STRING)?1:web.dimension-1; 
     gl = &gauss_lagrange[dim][web.gauss1D_order];
  }

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { for ( i = 0 ; i < dim ; i++ )
      for ( j = 0 ; j < SDIM ; j++ )
        mat[i][j] = f_info->sides[m][i][j];
    for ( k = 0 ; k+dim < SDIM ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
        mat[k+dim][j] = 
              eval(METH_INSTANCE(abs(f_info->method))->expr[j+k*SDIM],
                   f_info->gauss_pt[m],NULLID,NULL);
    value += gl->gausswt[m]*det_adjoint(mat,SDIM);
  }
  return sign*value/factorial[dim]; 
}

/*********************************************************************
*
* function: lagrange_k_vector_integral_grad()
*
* purpose:  method gradient
*
*/


REAL lagrange_k_vector_integral_grad(f_info)
struct qinfo *f_info;
{ int i,m,j,k,jj;
  REAL value = 0.0;
  REAL val[MAXCOORD];
  REAL derivs[MAXCOORD][MAXCOORD][MAXCOORD];
  MAT2D(mat,MAXCOORD,MAXCOORD);
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  int dim = web.dimension;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];

  if ( id_type(f_info->id) == EDGE )
  { dim = (web.representation==STRING)?1:web.dimension-1; 
     gl = &gauss_lagrange[dim][web.gauss1D_order];
  }

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL weight = sign*gl->gausswt[m]/factorial[dim];
     for ( i = 0 ; i < dim ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
          mat[i][j] = f_info->sides[m][i][j];  /* mat destroyed by det */
     for ( k = 0 ; k+dim < SDIM ; k++ )
        for ( j = 0 ; j < SDIM ; j++ )
        { eval_all(METH_INSTANCE(abs(f_info->method))->expr[j+k*SDIM],
               f_info->gauss_pt[m],SDIM,val+j,derivs[k][j],f_info->id);
          mat[k+dim][j] = val[j];
        }
     value += weight*det_adjoint(mat,SDIM);
     for ( k = 0 ; k < gl->lagpts; k++ )
      for ( j = 0 ; j < SDIM ; j++ ) 
        for ( i = 0 ; i < dim ; i++ )
        { f_info->grad[k][j] += weight*gl->gpolypart[m][i][k]*mat[j][i];
        }
     for ( k = 0 ; k < gl->lagpts ; k++ )
      for ( j = 0 ; j < SDIM ; j++ ) 
        for ( jj = 0 ; jj < SDIM ; jj++ ) 
        for ( i = dim ; i < SDIM ; i++ )
        {
          f_info->grad[k][j] += weight
                *gl->gpoly[m][k]*derivs[i-dim][jj][j]*mat[jj][i];
        }
  }
  return value;  
}

/*********************************************************************
*
* function: lagrange_k_vector_integral_hess()
*
* purpose:  method hessian
*
*/
REAL lagrange_k_vector_integral_hess(f_info)
struct qinfo *f_info;
{ return lagrange_k_vector_integral_all(f_info,METHOD_HESSIAN);
}
REAL lagrange_k_vector_integral_all(f_info,mode)
struct qinfo *f_info;
int mode;
{ int i,m,j,k,jj,ii,kk;
  REAL value = 0.0;
  REAL val[MAXCOORD];
  REAL derivs[MAXCOORD][MAXCOORD][MAXCOORD];
  MAT4D(seconds,MAXCOORD,MAXCOORD,MAXCOORD,MAXCOORD);
  MAT2D(mat,MAXCOORD,MAXCOORD);
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  int dim = web.dimension;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];
  MAT4D(dethess,MAXCOORD,MAXCOORD,MAXCOORD,MAXCOORD);
  int order = METH_INSTANCE(f_info->method)->vec_order;
  
  if ( id_type(f_info->id) == EDGE )
  { dim = (web.representation==STRING)?1:web.dimension-1; 
    gl = &gauss_lagrange[dim][web.gauss1D_order];
  }

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL det;
     REAL weight = sign*gl->gausswt[m]/factorial[dim];
     for ( i = 0 ; i < order ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
          mat[i][j] = f_info->sides[m][i][j];  /* mat destroyed by det */
     for ( k = 0 ; k+order < SDIM ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
      { eval_second(METH_INSTANCE(abs(f_info->method))->expr[j+k*SDIM],
          f_info->gauss_pt[m],SDIM,val+j,derivs[k][j],seconds[k][j],f_info->id);
        mat[k+order][j] = val[j];
      }
     det_hess(mat,dethess,SDIM);
     det = det_adjoint(mat,SDIM);

     value += weight*det;
     
     if ( mode == METHOD_VALUE ) continue;

     /* gradient */
     for ( k = 0 ; k < gl->lagpts; k++ )
      for ( j = 0 ; j < SDIM ; j++ ) 
        for ( i = 0 ; i < order ; i++ )
        { f_info->grad[k][j] += weight*gl->gpolypart[m][i][k]*mat[j][i];
        }
     for ( k = 0 ; k < gl->lagpts ; k++ )
      for ( j = 0 ; j < SDIM ; j++ ) 
        for ( jj = 0 ; jj < SDIM ; jj++ ) 
        for ( i = order ; i < SDIM ; i++ )
        {
          f_info->grad[k][j] += weight
                *gl->gpoly[m][k]*derivs[i-order][jj][j]*mat[jj][i];
        }

     if ( mode == METHOD_GRADIENT ) continue;

     /* hessian */
     for ( k = 0 ; k < gl->lagpts  ; k++ )
        for ( kk = 0 ; kk < gl->lagpts ; kk++ )
          for ( j = 0 ; j < SDIM ; j++ )
             for ( jj = 0 ; jj < SDIM ; jj++ )
             { REAL h = 0.0;
                int jjj;
                for ( i = 0 ; i < order ; i++ )
                 for ( ii  = 0 ; ii < order ; ii++ )
                 { h += dethess[i][j][ii][jj]
                            *gl->gpolypart[m][i][k]*gl->gpolypart[m][ii][kk];
                 }
                for ( i = 0 ; i < order ; i++ )
                 for ( ii  = order ; ii < SDIM ; ii++ )
                 for ( jjj = 0 ; jjj < SDIM ; jjj++ )
                  h += dethess[i][j][ii][jjj]*gl->gpolypart[m][i][k]
                              *gl->gpoly[m][kk]*derivs[ii-dim][jjj][jj];
                for ( i = order ; i < SDIM ; i++ )
                for ( ii = 0 ; ii < order ; ii++ )
                 for ( jjj = 0 ; jjj < SDIM ; jjj++ )
                  h += dethess[i][jjj][ii][jj]*gl->gpolypart[m][ii][kk]
                              *gl->gpoly[m][k]*derivs[i-order][jjj][j];
                for ( i = 0 ; i < SDIM ; i++ ) 
                 for ( ii  = order ; ii < SDIM ; ii++ )
                  h += gl->gpoly[m][k]*gl->gpoly[m][kk]*seconds[ii-order][i][j][jj]
                            *mat[i][ii];
                f_info->hess[k][kk][j][jj]  += weight*h;
            }
  }

  return value;  
}



/*********************************************************************

                    facet_volume  method

Integral of vectorfield over facet.  nD facet in (n+1)D only.
Lagrange model

*********************************************************************/
REAL lagrange_facet_volume_all ARGS((struct qinfo*,int));

/*********************************************************************
*
* function: lagrange_facet_volume()
*
* purpose:  method value
*
*/

REAL lagrange_facet_volume(f_info)
struct qinfo *f_info;
{ int i,m,j;
  REAL value = 0.0;
  MAT2D(mat,MAXCOORD,MAXCOORD);
  int dim = web.dimension;
  REAL sign = (dim&1) ? -1.0 : 1.0;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { for ( i = 0 ; i < dim ; i++ )
        for ( j = 0 ; j < dim ; j++ )
          mat[i][j] = f_info->sides[m][i][j];
    value += gl->gausswt[m]*det_adjoint(mat,dim)*f_info->gauss_pt[m][dim];
  }
  return sign*value/factorial[dim]; 
}

/*********************************************************************
*
* function: lagrange_facet_volume_grad()
*
* purpose:  method gradient
*
*/


REAL lagrange_facet_volume_grad(f_info)
struct qinfo *f_info;
{ int i,m,j,k;
  REAL value = 0.0;
  MAT2D(mat,MAXCOORD,MAXCOORD);
  int dim = web.dimension;
  REAL sign = (dim&1) ? -1.0 : 1.0;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];
  REAL z,det;

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL weight = sign*gl->gausswt[m]/factorial[dim];
     for ( i = 0 ; i < dim ; i++ )
        for ( j = 0 ; j < dim ; j++ ) 
          mat[i][j] = f_info->sides[m][i][j];  /* mat destroyed by det */
     z = f_info->gauss_pt[m][dim];
     det = det_adjoint(mat,dim);
     value += weight*det*z;
     for ( k = 0 ; k < gl->lagpts; k++ )
      for ( j = 0 ; j < dim ; j++ ) 
        for ( i = 0 ; i < dim ; i++ )
          f_info->grad[k][j] += weight*z*gl->gpolypart[m][i][k]*mat[j][i];
     for ( k = 0 ; k < gl->lagpts ; k++ )
        f_info->grad[k][dim] += weight*gl->gpoly[m][k]*det;
  }
  return value;  
}

/*********************************************************************
*
* function: lagrange_facet_volume_hess()
*
* purpose:  method hessian
*
*/
REAL lagrange_facet_volume_hess(f_info)
struct qinfo *f_info;
{ return lagrange_facet_volume_all(f_info,METHOD_HESSIAN);
}
REAL lagrange_facet_volume_all(f_info,mode)
struct qinfo *f_info;
int mode;
{ int i,m,j,k,jj,ii,kk;
  REAL sum,value = 0.0;
  MAT2D(mat,MAXCOORD,MAXCOORD);
  int dim = web.dimension;
  REAL sign = (dim&1) ? -1.0 : 1.0;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];
  MAT4D(dethess,MAXCOORD,MAXCOORD,MAXCOORD,MAXCOORD);

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL det;
     REAL z = f_info->gauss_pt[m][dim];
     REAL weight = sign*gl->gausswt[m]/factorial[dim];
     for ( i = 0 ; i < dim ; i++ )
        for ( j = 0 ; j < dim ; j++ )
          mat[i][j] = f_info->sides[m][i][j];  /* mat destroyed by det */
     det_hess(mat,dethess,dim);
     det = det_adjoint(mat,dim);

     value += weight*det*z;
     
     if ( mode == METHOD_VALUE ) continue;

     /* gradient */
     for ( k = 0 ; k < gl->lagpts; k++ )
      for ( j = 0 ; j < dim ; j++ ) 
        for ( i = 0 ; i < dim ; i++ )
          f_info->grad[k][j] += weight*z*gl->gpolypart[m][i][k]*mat[j][i];
     for ( k = 0 ; k < gl->lagpts ; k++ )
        f_info->grad[k][dim] += weight*gl->gpoly[m][k]*det;
     
     if ( mode == METHOD_GRADIENT ) continue;

     /* hessian */
     for ( k = 0 ; k < gl->lagpts  ; k++ )
        for ( kk = 0 ; kk < gl->lagpts ; kk++ )
        { 
          if ( dim == 2 )
          {
             f_info->hess[k][kk][0][1]  += weight*z*
                    (gl->gpolypart[m][0][k]*gl->gpolypart[m][1][kk]
                     - gl->gpolypart[m][1][k]*gl->gpolypart[m][0][kk]);
             f_info->hess[k][kk][1][0]  += weight*z*
                    (gl->gpolypart[m][1][k]*gl->gpolypart[m][0][kk]
                     - gl->gpolypart[m][0][k]*gl->gpolypart[m][1][kk]);
          }
          else if ( dim > 2 )
          { for ( j = 0 ; j < dim ; j++ )
             { for ( jj = 0 ; jj < dim ; jj++ )
                  for ( i = 0 ; i < dim ; i++ )
                     for ( ii = 0 ; ii < dim ; ii++ )
                { 
                  f_info->hess[k][kk][j][jj]  += weight*z*dethess[i][j][ii][jj]
                    *gl->gpolypart[m][i][k]*gl->gpolypart[m][ii][kk];
                }
             }
          }
          for ( j = 0 ; j < dim ; j++ )
          { for ( i = 0, sum = 0.0 ; i < dim ; i++ )
                  sum += gl->gpolypart[m][i][k]*mat[j][i];
             f_info->hess[k][kk][j][dim] += weight*gl->gpoly[m][kk]*sum;
          }
          for ( jj = 0 ; jj < dim ; jj++ )
          { for ( ii = 0,sum = 0.0 ; ii < dim ; ii++ )
                sum +=  gl->gpolypart[m][ii][kk]*mat[jj][ii];
             f_info->hess[k][kk][dim][jj] += weight*gl->gpoly[m][k]*sum;
          }
        }
  }

  return value;  
}

