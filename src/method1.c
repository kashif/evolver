/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*********************************************************************
*
*     file:        method1.c
*
*     contents:  quantities for vertices and edges
*/

#include "include.h"


/***********************************************************************
   null_length - just sets edge length and returns 0 for everything.
***********************************************************************/
REAL null_length_value(e_info)
struct qinfo *e_info;
{ q_edge_tension_value(e_info);
  return 0.0;
}
REAL null_length_grad(e_info)
struct qinfo *e_info;
{ return 0.0;
}
REAL null_length_hess(e_info)
struct qinfo *e_info;
{ return 0.0;
}

/***********************************************************************
   null_area - just sets facet length and returns 0 for everything.
***********************************************************************/
REAL null_area_value(f_info)
struct qinfo *f_info;
{ q_facet_tension_value(f_info);
  return 0.0;
}
REAL null_area_grad(f_info)
struct qinfo *f_info;
{ return 0.0;
}
REAL null_area_hess(f_info)
struct qinfo *f_info;
{ return 0.0;
}
/*********************************************************************

                    vertex_scalar_integral method

*********************************************************************/

/*********************************************************************
*
* function: vertex_scalar_integral_init()
*
* purpose:  check things 
*
*/

void vertex_scalar_integral_init(mode,mi)
int mode;
struct method_instance  *mi;
{
}

/*********************************************************************
*
* function: vertex_scalar_integral()
*
* purpose:  method value
*
*/

REAL vertex_scalar_integral(v_info)
struct qinfo *v_info;
{ REAL area;
  struct method_instance *mi = METH_INSTANCE(v_info->method);
  
  area = eval(mi->expr[0],v_info->x[0],v_info->id, NULL);

  if ( mi->flags & DEFAULT_INSTANCE )
  { /* add to facet area */
    body_id b_id,bb_id;
    edge_id e_id = get_vertex_edge(v_info->id);
    facetedge_id fe_id = get_edge_fe(e_id);
    facetedge_id fe;
    facet_id f_id;

    b_id = GEN_QUANT(mi->quant)->b_id;
    fe = fe_id;
    do
    { f_id = get_fe_facet(fe);
      bb_id = get_facet_body(f_id);
      if ( equal_id(b_id,bb_id) )
         add_facet_area(f_id,-area);
      bb_id = get_facet_body(inverse_id(f_id));
      if ( equal_id(b_id,bb_id) )
         add_facet_area(f_id,-area);
      fe = get_next_facet(fe);
    } while ( !equal_id(fe,fe_id) );
  }

  return area;
}

/*********************************************************************
*
* function: vertex_scalar_integral_grad()
*
* purpose:  method gradient
*
*/

REAL vertex_scalar_integral_grad(v_info)
struct qinfo *v_info;
{ REAL value = 0.0;

  eval_all(METH_INSTANCE(v_info->method)->expr[0],v_info->x[0],SDIM,&value,
     v_info->grad[0],v_info->id);

  return value;
}

/*********************************************************************
*
* function: vertex_scalar_integral_hess()
*
* purpose:  method gradient and hessian
*
*/

REAL vertex_scalar_integral_hess(v_info)
struct qinfo *v_info;
{ 
  REAL value = 0.0;

  eval_second(METH_INSTANCE(v_info->method)->expr[0],v_info->x[0],SDIM,&value,
        v_info->grad[0], v_info->hess[0][0],v_info->id);
  return value;
}


/*********************************************************************

                                Edge length quantity

*********************************************************************/

/*********************************************************************
*
*  function: q_edge_tension_init()
*
*/

void q_edge_tension_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{
}

/*********************************************************************
*
*  function: q_edge_tension_value()
*
*  purpose:  General quantity value of edge tension.
*/
 
REAL q_edge_tension_value(e_info)
struct qinfo *e_info;
{ REAL energy;
  if ( web.modeltype == QUADRATIC ) return edge_length_q_value(e_info);
  if ( web.modeltype == LAGRANGE ) 
      return lagrange_edge_tension_value(e_info);
  energy = sqrt(SDIM_dot(e_info->sides[0][0],e_info->sides[0][0]));

  if ( METH_INSTANCE(e_info->method)->flags & DEFAULT_INSTANCE )
  { 
#ifdef SHARED_MEMORY
     if ( nprocs > 1 ) 
      proc_total_area[GET_THREAD_ID] += energy;
     else
#endif
     binary_tree_add(web.total_area_addends,energy);
     set_edge_length(e_info->id,energy);
  }
  
  if ( METH_INSTANCE(e_info->method)->flags & USE_DENSITY )
    energy *= get_edge_density(e_info->id);
    
  return energy; 
}


/*********************************************************************
*
*  function: q_edge_tension_gradient()
*
*  purpose:  General quantity value and gradient of edge tension.
*/

REAL q_edge_tension_gradient(e_info)
struct qinfo *e_info;
{ REAL energy;
  REAL fudge;
  int j;

  if ( web.modeltype == QUADRATIC ) 
     return edge_length_q_grad(e_info);
  if ( web.modeltype == LAGRANGE ) 
      return lagrange_edge_tension_grad(e_info);
  energy = sqrt(SDIM_dot(e_info->sides[0][0],e_info->sides[0][0]));
  fudge = 1/energy;
  if ( METH_INSTANCE(e_info->method)->flags & USE_DENSITY )
  {  REAL density = get_edge_density(e_info->id);
     energy *= density; fudge *= density;
  }
  for ( j = 0 ; j < SDIM ; j++ ) 
     { e_info->grad[0][j] = -e_info->sides[0][0][j]*fudge;
       e_info->grad[1][j] =  e_info->sides[0][0][j]*fudge;
     }
  return energy;
}

/*********************************************************************
*
*  function: q_edge_tension_hessian()
*
*  purpose:  General quantity value, gradient, and hessian of edge length.
*
*  Remark to programmers: e_info->hess[m][n][i][j] is the entry for
*     coordinate i of vertex m of the edge and coordinate j of vertex n.
*/

REAL q_edge_tension_hessian(e_info)
struct qinfo *e_info;
{ REAL energy;
  int i,j;
  REAL e1,e3,ss;
  REAL fudge,len;

  if ( web.modeltype == QUADRATIC ) return edge_length_q_hess(e_info);
  if ( web.modeltype == LAGRANGE )  return lagrange_edge_tension_hess(e_info);

  energy = len = sqrt(SDIM_dot(e_info->sides[0][0],e_info->sides[0][0]));
  fudge = 1/len;
  if ( METH_INSTANCE(e_info->method)->flags & USE_DENSITY )
  {  REAL density = get_edge_density(e_info->id);
     energy *= density; fudge *= density;
  }
  for ( j = 0 ; j < SDIM ; j++ ) 
     { e_info->grad[0][j] = -e_info->sides[0][0][j]*fudge;
       e_info->grad[1][j] =  e_info->sides[0][0][j]*fudge;
     }
  e3 = fudge/(len*len);
  e1 = fudge;
  for ( i = 0 ; i < SDIM ; i++ ) 
    for ( j = 0 ; j < SDIM ; j++ ) 
    { ss = e_info->sides[0][0][i]*e_info->sides[0][0][j]*e3;
      e_info->hess[0][0][i][j] = -ss;
      e_info->hess[1][1][i][j] = -ss;
      e_info->hess[0][1][i][j] =  ss;
      e_info->hess[1][0][i][j] =  ss;
    }
  for ( i = 0 ; i < SDIM ; i++ ) 
    { e_info->hess[0][0][i][i] += e1;
      e_info->hess[1][1][i][i] += e1;
      e_info->hess[0][1][i][i] -= e1; 
      e_info->hess[1][0][i][i] -= e1;
    }
  return energy;
}


/*********************************************************************

         quadratic edge_length method

*********************************************************************/

/*********************************************************************
*
* function: edge_length_q_value()
*
* purpose:  method value
*
*/

REAL edge_length_q_value(e_info)
struct qinfo *e_info;
{ int j,k,m;
  REAL value = 0.0;
  REAL tang[MAXCOORD];

  for ( m = 0 ; m < gauss1D_num ; m++ )
  { for ( j = 0 ; j < SDIM ; j ++ )
    { tang[j] = 0.0;
      for ( k = 0 ; k < edge_ctrl ; k++ )
         tang[j] += gauss1polyd[k][m]*e_info->x[k][j];
    }
    value += gauss1Dwt[m]*sqrt(SDIM_dot(tang,tang));
  }
  if ( METH_INSTANCE(e_info->method)->flags & DEFAULT_INSTANCE )
  {
#ifdef SHARED_MEMORY
     if ( nprocs > 1 ) 
      proc_total_area[GET_THREAD_ID] += value;
     else
#endif
     binary_tree_add(web.total_area_addends,value);
     set_edge_length(e_info->id,value); 
  }
  if ( METH_INSTANCE(e_info->method)->flags & USE_DENSITY )
      value *= get_edge_density(e_info->id);
  return value;
}

/*********************************************************************
*
* function: edge_length_integral_q_grad()
*
* purpose:  method gradient
*
*/

REAL edge_length_q_grad(e_info)
struct qinfo *e_info;
{ int m,k,j;
  REAL value = 0.0;
  REAL len,fudge;
  REAL tang[MAXCOORD];
  REAL density;

  if ( METH_INSTANCE(e_info->method)->flags & USE_DENSITY )
     density = get_edge_density(e_info->id);
  else density = 1.0;

  for ( j = 0 ; j < SDIM ; j++ )
     for ( m = 0 ; m < edge_ctrl ; m++ )
        e_info->grad[m][j] = 0.0;
  for ( m = 0 ; m < gauss1D_num ; m++ )
  { for ( j = 0 ; j < SDIM ; j ++ )
    { tang[j] = 0.0;
      for ( k = 0 ; k < edge_ctrl ; k++ )
          tang[j] += gauss1polyd[k][m]*e_info->x[k][j];
    }
    len = sqrt(SDIM_dot(tang,tang));
    if ( len == 0.0 ) continue;
    value += gauss1Dwt[m]*len;
    fudge = density*gauss1Dwt[m]/len;
    for ( k = 0 ; k < edge_ctrl ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
         e_info->grad[k][j] += fudge*tang[j]*gauss1polyd[k][m];
  }

  return density*value;
}

/*********************************************************************
*
* function: edge_length_q_hess()
*
* purpose:  method gradient and hessian
*
*/

REAL edge_length_q_hess(e_info)
struct qinfo *e_info;
{ int m,j,jj,k,kk;
  REAL value = 0.0;
  REAL len,density,fudge;
  REAL sumgrad[2][MAXCOORD];
  REAL sumhess[2][2][MAXCOORD][MAXCOORD];
  REAL tang[MAXCOORD];

  if ( METH_INSTANCE(e_info->method)->flags & USE_DENSITY )
     density = get_edge_density(e_info->id);
  else density = 1.0;

  /* derivatives of gaussian sum part */
  memset((char*)sumgrad,0,sizeof(sumgrad));
  memset((char*)sumhess,0,sizeof(sumhess));
  for ( m = 0 ; m < gauss1D_num ; m++ )
  { for ( j = 0 ; j < SDIM ; j ++ )
    { tang[j] = 0.0;
      for ( k = 0 ; k < edge_ctrl ; k++ )
          tang[j] += gauss1polyd[k][m]*e_info->x[k][j];
    }
    len = sqrt(SDIM_dot(tang,tang));
    if ( len == 0.0 ) continue;
    value += gauss1Dwt[m]*len;
    fudge = density*gauss1Dwt[m]/len;
    for ( k = 0 ; k < edge_ctrl ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
         e_info->grad[k][j] += fudge*tang[j]*gauss1polyd[k][m];
    for ( k = 0 ; k < edge_ctrl ; k++ )
      for ( kk = 0 ; kk < edge_ctrl ; kk++ )
         for ( j = 0 ; j < SDIM ; j++ )
            for ( jj = 0 ; jj < SDIM ; jj++ )
              e_info->hess[k][kk][j][jj] += fudge*
            ( - tang[j]*tang[jj]*gauss1polyd[k][m]*gauss1polyd[kk][m]/len/len
            + ((j==jj)? gauss1polyd[k][m]*gauss1polyd[kk][m] : 0.0));
  }

  return density*value;
}


/*********************************************************************

                    edge_scalar_integral method

*********************************************************************/

/*********************************************************************
*
* function: edge_scalar_integral()
*
* purpose:  method value
*
*/

REAL edge_scalar_integral(e_info)
struct qinfo *e_info;
{ int m;
  REAL value = 0.0;

  if ( web.modeltype == QUADRATIC ) return edge_scalar_integral_q(e_info);
  if ( web.modeltype == LAGRANGE ) return edge_scalar_integral_lagr(e_info);

  for ( m = 0 ; m < gauss1D_num ; m++ )
  { e_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    value += gauss1Dwt[m]*eval(METH_INSTANCE(e_info->method)->expr[0],
              e_info->gauss_pt[m], e_info->id,NULL);
  }
  value *= sqrt(SDIM_dot(e_info->sides[0][0],e_info->sides[0][0]));
  return value;
}

/*********************************************************************
*
* function: edge_scalar_integral_grad()
*
* purpose:  method gradient
*
*/

REAL edge_scalar_integral_grad(e_info)
struct qinfo *e_info;
{ int m,j;
  REAL value = 0.0;
  REAL len,val;
  REAL derivs[MAXCOORD];

  if ( web.modeltype == QUADRATIC ) return edge_scalar_integral_q_grad(e_info);
  if ( web.modeltype == LAGRANGE ) return edge_scalar_integral_lagr_grad(e_info);

  for ( j = 0 ; j < SDIM ; j++ ) 
     for ( m = 0 ; m < 2 ; m++ )
        e_info->grad[m][j] = 0.0;
  len = sqrt(SDIM_dot(e_info->sides[0][0],e_info->sides[0][0]));
  for ( m = 0 ; m < gauss1D_num ; m++ )
  { e_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    eval_all(METH_INSTANCE(e_info->method)->expr[0],e_info->gauss_pt[m],SDIM,&val,
                                                   derivs,e_info->id);
    value += gauss1Dwt[m]*val;
    for ( j = 0 ; j < SDIM ; j++ )
    { e_info->grad[0][j] += gauss1Dwt[m]*gauss1poly[0][m]*derivs[j]*len;
      e_info->grad[1][j] += gauss1Dwt[m]*gauss1poly[1][m]*derivs[j]*len;
    }
  }
  for ( j = 0 ; j < SDIM ; j++ )
  { e_info->grad[0][j] -= value*e_info->sides[0][0][j]/len;
    e_info->grad[1][j] += value*e_info->sides[0][0][j]/len;
  }

  return len*value;
}

/*********************************************************************
*
* function: edge_scalar_integral_hess()
*
* purpose:  method gradient and hessian
*
*/

REAL edge_scalar_integral_hess(e_info)
struct qinfo *e_info;
{ int m,j,k,i;
  REAL value = 0.0;
  REAL len,sum,val;
  REAL derivs[MAXCOORD];
  REAL lengrad[2][MAXCOORD],sumgrad[2][MAXCOORD];
  REAL lenhess[2][2][MAXCOORD][MAXCOORD],sumhess[2][2][MAXCOORD][MAXCOORD];
  MAT2D(second,MAXCOORD,MAXCOORD);

  if ( web.modeltype == QUADRATIC ) return edge_scalar_integral_q_hess(e_info);
  if ( web.modeltype == LAGRANGE ) return edge_scalar_integral_lagr_hess(e_info);

  len = SDIM_dot(e_info->sides[0][0],e_info->sides[0][0]);
  if ( len <= 0.0 )
  { 
     return 0.0;
  }
  len = sqrt(len);

  /* derivatives of gaussian sum part */
  sum = 0.0;
  memset((char*)sumgrad,0,sizeof(sumgrad));
  memset((char*)sumhess,0,sizeof(sumhess));
  for ( m = 0 ; m < gauss1D_num ; m++ )
  { e_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    eval_second(METH_INSTANCE(e_info->method)->expr[0],e_info->gauss_pt[m],SDIM,&val,
                                                 derivs,second,e_info->id);
    sum += gauss1Dwt[m]*val;
    for ( j = 0 ; j < SDIM ; j++ )
    { sumgrad[0][j] += gauss1Dwt[m]*gauss1poly[0][m]*derivs[j];
      sumgrad[1][j] += gauss1Dwt[m]*gauss1poly[1][m]*derivs[j];
    }
    for ( j = 0 ; j < SDIM ; j++ )
      for ( k = 0 ; k < SDIM ; k++ )
      { sumhess[0][0][j][k] += gauss1Dwt[m]*gauss1poly[0][m]*gauss1poly[0][m]
                                              *second[j][k];
        sumhess[0][1][j][k] += gauss1Dwt[m]*gauss1poly[0][m]*gauss1poly[1][m]
                                              *second[j][k];
        sumhess[1][0][j][k] += gauss1Dwt[m]*gauss1poly[1][m]*gauss1poly[0][m]
                                              *second[j][k];
        sumhess[1][1][j][k] += gauss1Dwt[m]*gauss1poly[1][m]*gauss1poly[1][m]
                                              *second[j][k];
      }
  }

  /* derivatives of length part */
  for ( j = 0 ; j < SDIM ; j++ )
  { lengrad[0][j] = -e_info->sides[0][0][j]/len;
    lengrad[1][j] = e_info->sides[0][0][j]/len;
  }
  for ( j = 0 ; j < SDIM ; j++ )
    for ( k = 0 ; k < SDIM ; k++ )
      { val = -e_info->sides[0][0][j]*e_info->sides[0][0][k]/len/len/len;
        if ( j == k ) val += 1/len;
        lenhess[0][0][j][k] = lenhess[1][1][j][k] = val;
        lenhess[0][1][j][k] = lenhess[1][0][j][k] = -val;
      }

  /* final values */
  value = len*sum;
  for ( j = 0 ; j < SDIM ; j++ )
    for ( m = 0 ; m < 2 ; m++ )
      e_info->grad[m][j] = lengrad[m][j]*sum + len*sumgrad[m][j];
  for ( j = 0 ; j < SDIM ; j++ ) 
    for ( k = 0 ; k < SDIM ; k++ ) 
     for ( m = 0 ; m < 2 ; m++ )
      for ( i = 0 ; i < 2 ; i++ )
         e_info->hess[m][i][j][k] = lenhess[m][i][j][k]*sum
            + lengrad[m][j]*sumgrad[i][k] + sumgrad[m][j]*lengrad[i][k]
            + len*sumhess[m][i][j][k];
  return value;
}


/*********************************************************************

         quadratic edge_scalar_integral method

*********************************************************************/

/*********************************************************************
*
* function: edge_scalar_integral_q()
*
* purpose:  method value
*
*/

REAL edge_scalar_integral_q(e_info)
struct qinfo *e_info;
{ int j,k,m;
  REAL value = 0.0;
  REAL tang[MAXCOORD];

  for ( m = 0 ; m < gauss1D_num ; m++ ) 
  { for ( j = 0 ; j < SDIM ; j ++ )
      { tang[j] = 0.0;
        for ( k = 0 ; k < edge_ctrl ; k++ )
           tang[j] += gauss1polyd[k][m]*e_info->x[k][j];
      }
    e_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    value += gauss1Dwt[m]*eval(METH_INSTANCE(e_info->method)->expr[0],
             e_info->gauss_pt[m],e_info->id,NULL)*sqrt(SDIM_dot(tang,tang));
  }
  return value;
}

/*********************************************************************
*
* function: edge_scalar_integral_q_grad()
*
* purpose:  method gradient
*
*/

REAL edge_scalar_integral_q_grad(e_info)
struct qinfo *e_info;
{ int m,k,j;
  REAL value = 0.0;
  REAL len,val;
  REAL derivs[MAXCOORD];
  REAL tang[MAXCOORD];

  for ( j = 0 ; j < SDIM ; j++ )
     for ( m = 0 ; m < edge_ctrl ; m++ )
        e_info->grad[m][j] = 0.0;
  for ( m = 0 ; m < gauss1D_num ; m++ )
  { e_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    eval_all(METH_INSTANCE(e_info->method)->expr[0],e_info->gauss_pt[m],SDIM,&val,
                                                         derivs,e_info->id);
    for ( j = 0 ; j < SDIM ; j ++ )
    { tang[j] = 0.0;
      for ( k = 0 ; k < edge_ctrl ; k++ )
          tang[j] += gauss1polyd[k][m]*e_info->x[k][j];
    }
    len = sqrt(SDIM_dot(tang,tang));
    if ( len == 0.0 ) continue;
    value += gauss1Dwt[m]*val*len;
    for ( k = 0 ; k < edge_ctrl ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
         e_info->grad[k][j] += gauss1Dwt[m]*(gauss1poly[k][m]*derivs[j]*len
                                      + val*tang[j]/len*gauss1polyd[k][m]);
  }

  return value;
}

/*********************************************************************
*
* function: edge_scalar_integral_q_hess()
*
* purpose:  method gradient and hessian
*
*/

REAL edge_scalar_integral_q_hess(e_info)
struct qinfo *e_info;
{ int m,j,jj,k,kk;
  REAL value = 0.0;
  REAL len,val;
  REAL derivs[MAXCOORD];
  REAL sumgrad[2][MAXCOORD];
  REAL sumhess[2][2][MAXCOORD][MAXCOORD];
  MAT2D(second,MAXCOORD,MAXCOORD);
  REAL tang[MAXCOORD];

  /* derivatives of gaussian sum part */
  memset((char*)sumgrad,0,sizeof(sumgrad));
  memset((char*)sumhess,0,sizeof(sumhess));
  for ( m = 0 ; m < gauss1D_num ; m++ )
  { e_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    eval_second(METH_INSTANCE(e_info->method)->expr[0],e_info->gauss_pt[m],SDIM,&val,
                                                    derivs,second,e_info->id);
    for ( j = 0 ; j < SDIM ; j ++ )
    { tang[j] = 0.0;
      for ( k = 0 ; k < edge_ctrl ; k++ )
          tang[j] += gauss1polyd[k][m]*e_info->x[k][j];
    }
    len = sqrt(SDIM_dot(tang,tang));
    if ( len == 0.0 ) continue;
    value += gauss1Dwt[m]*val*len;
    for ( k = 0 ; k < edge_ctrl ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
         e_info->grad[k][j] += gauss1Dwt[m]*(gauss1poly[k][m]*derivs[j]*len
                                      + val*tang[j]/len*gauss1polyd[k][m]);
    for ( k = 0 ; k < edge_ctrl ; k++ )
      for ( kk = 0 ; kk < edge_ctrl ; kk++ )
         for ( j = 0 ; j < SDIM ; j++ )
            for ( jj = 0 ; jj < SDIM ; jj++ )
              e_info->hess[k][kk][j][jj] += gauss1Dwt[m]*
     ( second[j][jj]*gauss1poly[k][m]*gauss1poly[kk][m]*len
     + derivs[j]*gauss1poly[k][m]*tang[jj]*gauss1polyd[kk][m]/len
     + derivs[jj]*gauss1poly[kk][m]*tang[j]*gauss1polyd[k][m]/len
     - val*tang[j]*tang[jj]*gauss1polyd[k][m]*gauss1polyd[kk][m]/len/len/len
     + ((j==jj)? val*gauss1polyd[k][m]*gauss1polyd[kk][m]/len : 0.0));
  }

  return value;
}


/*********************************************************************

         Lagrange edge_scalar_integral method

*********************************************************************/

/*********************************************************************
*
* function: edge_scalar_integral_lagr()
*
* purpose:  method value
*
*/

REAL edge_scalar_integral_lagr(e_info)
struct qinfo *e_info;
{ int m;
  REAL value = 0.0;
  struct gauss_lag *gl = &gauss_lagrange[1][web.gauss1D_order];

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { e_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    value += gl->gausswt[m]
         *eval(METH_INSTANCE(e_info->method)->expr[0],e_info->gauss_pt[m],e_info->id,NULL)
         *sqrt(SDIM_dot(e_info->sides[m][0],e_info->sides[m][0]));
  }
  return value;
}

/*********************************************************************
*
* function: edge_scalar_integral_lagr_grad()
*
* purpose:  method gradient
*
*/

REAL edge_scalar_integral_lagr_grad(e_info)
struct qinfo *e_info;
{ int m,k,j;
  REAL value = 0.0;
  REAL len,val;
  REAL derivs[MAXCOORD];
  REAL *tang;
  struct gauss_lag *gl = &gauss_lagrange[1][web.gauss1D_order];

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { e_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    eval_all(METH_INSTANCE(e_info->method)->expr[0],e_info->gauss_pt[m],SDIM,&val,
                                                         derivs,e_info->id);
    tang = e_info->sides[m][0];
    len = sqrt(SDIM_dot(tang,tang));
    if ( len == 0.0 ) continue;
    value += gl->gausswt[m]*val*len;
    for ( k = 0 ; k < gl->lagpts ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
         e_info->grad[k][j] += 
          gl->gausswt[m]*(gl->gpoly[m][k]*derivs[j]*len
              + val*tang[j]/len*gl->gpolypart[m][0][k]);
  }
  return value;
}

/*********************************************************************
*
* function: edge_scalar_integral_lagr_hess()
*
* purpose:  method gradient and hessian
*
*/

REAL edge_scalar_integral_lagr_hess(e_info)
struct qinfo *e_info;
{ int m,j,jj,k,kk;
  REAL value = 0.0;
  REAL len,val;
  REAL derivs[MAXCOORD];
  REAL sumgrad[2][MAXCOORD];
  REAL sumhess[2][2][MAXCOORD][MAXCOORD];
  MAT2D(second,MAXCOORD,MAXCOORD);
  REAL *tang;
  struct gauss_lag *gl = &gauss_lagrange[1][web.gauss1D_order];

  /* derivatives of gaussian sum part */
  memset((char*)sumgrad,0,sizeof(sumgrad));
  memset((char*)sumhess,0,sizeof(sumhess));
  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { e_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    eval_second(METH_INSTANCE(e_info->method)->expr[0],e_info->gauss_pt[m],SDIM,&val,
                                                     derivs,second,e_info->id);
    tang = e_info->sides[m][0];
    len = sqrt(SDIM_dot(tang,tang));
    if ( len == 0.0 ) continue;
    value += gl->gausswt[m]*val*len;
    for ( k = 0 ; k < gl->lagpts ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
         e_info->grad[k][j] += 
             gl->gausswt[m]*(gl->gpoly[m][k]*derivs[j]*len
                                  + val*tang[j]/len*gl->gpolypart[m][0][k]);
    for ( k = 0 ; k < gl->lagpts ; k++ )
      for ( kk = 0 ; kk < gl->lagpts ; kk++ )
         for ( j = 0 ; j < SDIM ; j++ )
            for ( jj = 0 ; jj < SDIM ; jj++ )
              e_info->hess[k][kk][j][jj] += gl->gausswt[m]*
     ( second[j][jj]*gl->gpoly[m][k]*gl->gpoly[m][kk]*len
     + derivs[j]*gl->gpoly[m][k]*tang[jj]*gl->gpolypart[m][0][kk]/len
     + derivs[jj]*gl->gpoly[m][kk]*tang[j]*gl->gpolypart[m][0][k]/len
     - val*tang[j]*tang[jj]*gl->gpolypart[m][0][k]*gl->gpolypart[m][0][kk]/len/len/len
     + ((j==jj)? val*gl->gpolypart[m][0][k]*gl->gpolypart[m][0][kk]/len : 0.0));
  }

  return value;
}


/*********************************************************************

                    edge_vector_integral method

*********************************************************************/

/*********************************************************************
*
* function: edge_vector_integral()
*
* purpose:  method value
*
*/

REAL edge_vector_integral(e_info)
struct qinfo *e_info;
{ int m,j;
  REAL value=0.0;
  if ( web.modeltype == QUADRATIC ) return edge_vector_integral_q(e_info);
  if ( web.modeltype == LAGRANGE ) return edge_vector_integral_lagrange(e_info);
  for (  m = 0 ; m < gauss1D_num ; m++ )
  { e_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    for ( j = 0 ; j < SDIM ; j++ )
    { REAL green;
      green = gauss1Dwt[m]*eval(METH_INSTANCE(e_info->method)->expr[j],
                       e_info->gauss_pt[m], e_info->id,NULL);
      value += e_info->sides[0][0][j]*green;
    }
  }  
  return (get_eattr(e_info->id) & NEGBOUNDARY) ? -value : value;
}

/*********************************************************************
*
* function: edge_vector_integral_grad()
*
* purpose:  method gradient
*
*/


REAL edge_vector_integral_grad(e_info)
struct qinfo *e_info;
{ int m,j,k;
  REAL value = 0.0;
  REAL val[MAXCOORD];
  REAL derivs[MAXCOORD][MAXCOORD];
  REAL sum;
  REAL sign = (get_eattr(e_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;

  if ( web.modeltype == QUADRATIC ) return edge_vector_integral_q_grad(e_info);
  if ( web.modeltype == LAGRANGE ) 
     return edge_vector_integral_lagrange_grad(e_info);
  for ( k = 0 ; k < 2 ; k++ )
     for ( j = 0 ; j < SDIM ; j++ ) 
        e_info->grad[k][j] = 0.0;
  for ( m = 0 ; m < gauss1D_num ; m++ )
  { REAL weight = sign*gauss1Dwt[m];
    e_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    for ( j = 0 ; j < SDIM ; j++ ) 
      eval_all(METH_INSTANCE(e_info->method)->expr[j],e_info->gauss_pt[m],SDIM,
        val+j, derivs[j],e_info->id);
    value += gauss1Dwt[m]*SDIM_dot(val,e_info->sides[0][0]);
    for ( k = 0 ; k < SDIM ; k++ )
    { for ( sum = 0.0, j = 0 ; j < SDIM ; j++ )
        sum += derivs[j][k]*e_info->sides[0][0][j];
      e_info->grad[0][k] += weight*(-val[k] + gauss1poly[0][m]*sum);
      e_info->grad[1][k] += weight*(val[k] + gauss1poly[1][m]*sum);
    }
  }

  return sign*value;
}

/*********************************************************************
*
* function: edge_vector_integral_hess()
*
* purpose:  method gradient and hessian
*
*/

REAL edge_vector_integral_hess(e_info)
struct qinfo *e_info;
{ int m,i,j,k;
  REAL value = 0.0;
  REAL val[MAXCOORD];
  REAL derivs[MAXCOORD][MAXCOORD];
  REAL sum;
  MAT3D(second,MAXCOORD,MAXCOORD,MAXCOORD);
  REAL sign = (get_eattr(e_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;

  if ( web.modeltype == QUADRATIC ) return edge_vector_integral_q_hess(e_info);
  if ( web.modeltype == LAGRANGE ) 
     return edge_vector_integral_lagrange_hess(e_info);
  for ( m = 0 ; m < gauss1D_num ; m++ )
  { REAL weight = sign*gauss1Dwt[m];
    e_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    for ( j = 0 ; j < SDIM ; j++ ) 
      eval_second(METH_INSTANCE(e_info->method)->expr[j],e_info->gauss_pt[m],SDIM,val+j,
                                              derivs[j],second[j],e_info->id);
    value += weight*SDIM_dot(val,e_info->sides[0][0]);
    for ( k = 0 ; k < SDIM ; k++ )
    { for ( sum = 0.0, j = 0 ; j < SDIM ; j++ )
          sum += derivs[j][k]*e_info->sides[0][0][j];
      e_info->grad[0][k] += weight*(-val[k] + gauss1poly[0][m]*sum);
      e_info->grad[1][k] += weight*(val[k] + gauss1poly[1][m]*sum);
    }
    for ( k = 0 ; k < SDIM ; k++ )
     for ( i = 0 ; i < SDIM ; i++ )
     { for ( sum = 0.0, j = 0 ; j < SDIM ; j++ )
         sum += second[j][k][i]*e_info->sides[0][0][j];
       e_info->hess[0][0][k][i] += weight*gauss1poly[0][m]
            *(gauss1poly[0][m]*sum - derivs[k][i] - derivs[i][k]);
       e_info->hess[0][1][k][i] += weight
            *(gauss1poly[0][m]*gauss1poly[1][m]*sum 
            - gauss1poly[1][m]*derivs[k][i] + gauss1poly[0][m]*derivs[i][k]);
       e_info->hess[1][0][k][i] += weight
            *(gauss1poly[0][m]*gauss1poly[1][m]*sum 
            + gauss1poly[0][m]*derivs[k][i] - gauss1poly[1][m]*derivs[i][k]);
       e_info->hess[1][1][k][i] += weight*gauss1poly[1][m]
            *(gauss1poly[1][m]*sum + derivs[k][i] + derivs[i][k]);
     }
  }

  return value;
}


/*********************************************************************

                  quadratic edge_vector_integral method

*********************************************************************/

/*********************************************************************
*
* function: edge_vector_integral_q()
*
* purpose:  method value
*
*/

REAL edge_vector_integral_q(e_info)
struct qinfo *e_info;
{ int m,j,k;
  REAL value=0.0;
  REAL tang[MAXCOORD];
  REAL sign = (get_eattr(e_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;

  for ( m = 0 ; m < gauss1D_num ; m++ )
  { REAL weight = sign*gauss1Dwt[m];
    e_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
     for ( j = 0 ; j < SDIM ; j++ )
     { tang[j] = 0.0;
        for ( k = 0 ; k < edge_ctrl ; k++ )
          tang[j] += gauss1polyd[k][m]*e_info->x[k][j];
        value += weight*tang[j]*eval(METH_INSTANCE(e_info->method)->expr[j],
                          e_info->gauss_pt[m],e_info->id,NULL);
     }
  }
  return value;
}

/*********************************************************************
*
* function: edge_vector_integral_q_grad()
*
* purpose:  method gradient
*
*/


REAL edge_vector_integral_q_grad(e_info)
struct qinfo *e_info;
{ int m,j,k,i;
  REAL value = 0.0;
  REAL val[MAXCOORD];
  REAL derivs[MAXCOORD][MAXCOORD];
  REAL sum;
  REAL tang[MAXCOORD];
  REAL sign = (get_eattr(e_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;

  for ( m = 0 ; m < gauss1D_num ; m++ )
  { REAL weight = sign*gauss1Dwt[m];
    e_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    for ( j = 0 ; j < SDIM ; j++ ) 
    { tang[j] = 0.0;
      for ( k = 0 ; k < edge_ctrl ; k++ )
           tang[j] += gauss1polyd[k][m]*e_info->x[k][j];
    }
    for ( j = 0 ; j < SDIM ; j++ ) 
      eval_all(METH_INSTANCE(e_info->method)->expr[j],e_info->gauss_pt[m],SDIM,val+j,
                                                   derivs[j],e_info->id);
    value += weight*SDIM_dot(val,tang);
    for ( k = 0 ; k < SDIM ; k++ )
    { for ( sum = 0.0, j = 0 ; j < SDIM ; j++ )
        sum += derivs[j][k]*tang[j];
      for ( i = 0 ; i < edge_ctrl ; i++ )
        e_info->grad[i][k] += 
                  weight*(gauss1polyd[i][m]*val[k] + gauss1poly[i][m]*sum);
    }
  }

  return value;
}

/*********************************************************************
*
* function: edge_vector_integral_q_hess()
*
* purpose:  method gradient and hessian
*
*/


REAL edge_vector_integral_q_hess(e_info)
struct qinfo *e_info;
{ int m,i,j,k,ii,kk;
  REAL value = 0.0;
  REAL val[MAXCOORD];
  REAL derivs[MAXCOORD][MAXCOORD];
  REAL sum;
  MAT3D(second,MAXCOORD,MAXCOORD,MAXCOORD);
  REAL tang[MAXCOORD];
  REAL sign = (get_eattr(e_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;

  for ( m = 0 ; m < gauss1D_num ; m++ )
  { REAL weight = sign*gauss1Dwt[m];
    e_info->gauss_pt[m][2*SDIM] = m; /* kludge for attr interpolation. */
    for ( j = 0 ; j < SDIM ; j++ ) 
    { tang[j] = 0.0;
      for ( k = 0 ; k < edge_ctrl ; k++ )
          tang[j] += gauss1polyd[k][m]*e_info->x[k][j];
    }
    for ( j = 0 ; j < SDIM ; j++ ) 
      eval_second(METH_INSTANCE(e_info->method)->expr[j],e_info->gauss_pt[m],SDIM,
        val+j, derivs[j],second[j],e_info->id);
    value += weight*SDIM_dot(val,tang);
    for ( k = 0 ; k < SDIM ; k++ )
      { for ( sum = 0.0, j = 0 ; j < SDIM ; j++ )
             sum += derivs[j][k]*tang[j];
         for ( i = 0 ; i < edge_ctrl ; i++ )
            e_info->grad[i][k] += 
              weight*(gauss1polyd[i][m]*val[k] + gauss1poly[i][m]*sum);
      }

    for ( ii = 0 ; ii < SDIM ; ii++ )
     for ( i = 0 ; i < SDIM ; i++ )
      { for ( sum = 0.0, j = 0 ; j < SDIM ; j++ )
             sum += second[j][ii][i]*tang[j];
         for ( k = 0 ; k < edge_ctrl ; k++ )
            for ( kk = 0 ; kk < edge_ctrl ; kk++ )
              e_info->hess[k][kk][i][ii] += weight*
              ( sum*gauss1poly[k][m]*gauss1poly[kk][m]
                 + gauss1polyd[k][m]*derivs[i][ii]*gauss1poly[kk][m]
                 + gauss1polyd[kk][m]*derivs[ii][i]*gauss1poly[k][m]
              );
      }
  }

  return value;
}



/**********************************************************************
                    Linear area quantity (STRING model)
**********************************************************************/

/**********************************************************************
*
*  function: q_edge_area()
*
*  purpose: value of area integral on edge
*/

REAL q_edge_area(e_info)
struct qinfo *e_info;
{ REAL **x;
  REAL area;

  if ( web.torus_flag ) area = q_edge_torus_area(e_info);
  else if ( web.modeltype == QUADRATIC ) area = q_edge_area_q(e_info);
  else if ( web.modeltype == LAGRANGE )  area = q_edge_area_lagrange(e_info);
  else 
  { x = e_info->x;

    /* main integral over edge */
    area = (x[0][1]+x[1][1])*(x[0][0] - x[1][0])/2;
  }

  if ( METH_INSTANCE(e_info->method)->flags & DEFAULT_INSTANCE )
  { /* add to facet area */
    body_id b_id,bb_id;
    facetedge_id fe_id = get_edge_fe(e_info->id);
    facetedge_id fe;
    facet_id f_id;

    b_id = GEN_QUANT(METH_INSTANCE(e_info->method)->quant)->b_id;
    fe = fe_id;
    do
    { f_id = get_fe_facet(fe);
      bb_id = get_facet_body(f_id);
      if ( equal_id(b_id,bb_id) )
         add_facet_area(f_id,area);
      bb_id = get_facet_body(inverse_id(f_id));
      if ( equal_id(b_id,bb_id) )
         add_facet_area(f_id,area);
      fe = get_next_facet(fe);
    } while ( !equal_id(fe,fe_id) );
  }

  return area;
}

/**********************************************************************
*
*  function: q_edge_area_grad()
*
*  purpose: value and gradient of area integral on edge
*/

REAL q_edge_area_grad(e_info)
struct qinfo *e_info;
{ REAL **x,**g;
  REAL area;

  if ( web.torus_flag ) return q_edge_torus_area_grad(e_info);
  if ( web.modeltype == QUADRATIC ) return q_edge_area_q_grad(e_info);
  if ( web.modeltype == LAGRANGE ) return q_edge_area_lagrange_grad(e_info);

  x = e_info->x;
  g = e_info->grad;

  /* main integral over edge */
  area = (x[0][1]+x[1][1])*(x[0][0] - x[1][0])/2;
  g[0][0] = (x[1][1] + x[0][1])/2;
  g[1][0] = -(x[1][1] + x[0][1])/2;
  g[0][1] = (x[0][0] - x[1][0])/2;
  g[1][1] = (x[0][0] - x[1][0])/2;
  return area;
}


/**********************************************************************
*
*  function: q_edge_area_hess()
*
*  purpose: value and gradient and hessian of area integral on edge
*/

REAL q_edge_area_hess(e_info)
struct qinfo *e_info;
{ REAL **x,**g,****h;
  REAL area;

  if ( web.torus_flag ) return q_edge_torus_area_hess(e_info);
  if ( web.modeltype == QUADRATIC ) return q_edge_area_q_hess(e_info);
  if ( web.modeltype == LAGRANGE )  return q_edge_area_lagrange_hess(e_info);

  x = e_info->x;
  g = e_info->grad;
  h = e_info->hess;

  /* main integral over edge */
  area = (x[0][1]+x[1][1])*(x[0][0] - x[1][0])/2;
  g[0][0] = (x[1][1] + x[0][1])/2;
  g[1][0] = -(x[1][1] + x[0][1])/2;
  g[0][1] = (x[0][0] - x[1][0])/2;
  g[1][1] = (x[0][0] - x[1][0])/2;
  h[0][1][0][1] += 0.5;
  h[0][0][0][1] += 0.5;
  h[1][1][0][1] -= 0.5;
  h[1][0][0][1] -= 0.5;
  h[0][0][1][0] += 0.5;
  h[0][1][1][0] -= 0.5;
  h[1][0][1][0] += 0.5;
  h[1][1][1][0] -= 0.5;

  return area;
}


/**********************************************************************
                    Quadratic area quantity (STRING model)
**********************************************************************/

/**********************************************************************
*
*  function: q_edge_area_q()
*
*  purpose: value of area integral on edge
*/

REAL q_edge_area_q(e_info)
struct qinfo *e_info;
{ REAL **x;
  REAL area;
  int i,j;

  x = e_info->x;

  /* main integral over edge */
  area = 0.0;
  for ( i = 0 ; i < edge_ctrl ; i++ )
     for ( j = 0 ; j < edge_ctrl ; j++ )
        { REAL v = scoeff[j][i];
          area += v*x[i][0]*x[j][1];
        }

  return area;
}

/**********************************************************************
*
*  function: q_edge_area_q_grad()
*
*  purpose: value and gradient of area integral on edge
*/

REAL q_edge_area_q_grad(e_info)
struct qinfo *e_info;
{ REAL **x,**g;
  REAL area;
  int i,j,k;

  x = e_info->x;
  g = e_info->grad;
  /* gradients */
  for ( k = 0 ; k < edge_ctrl ; k++ )
     for ( j = 0 ; j < SDIM ; j++ )
        g[k][j] = 0.0;

  /* main integral over edge */
  area = 0.0;
  for ( i = 0 ; i < edge_ctrl ; i++ )
     for ( j = 0 ; j < edge_ctrl ; j++ )
        { REAL v = scoeff[j][i];
          area += v*x[i][0]*x[j][1];
          g[i][0] += v*x[j][1];
          g[j][1] += v*x[i][0];
        }

  return area;
}


/**********************************************************************
*
*  function: q_edge_area_q_hess()
*
*  purpose: value and gradient and hessian of area integral on edge
*/

REAL q_edge_area_q_hess(e_info)
struct qinfo *e_info;
{ REAL **x,**g,****h;
  REAL area;
  int i,j;

  x = e_info->x;
  g = e_info->grad;
  h = e_info->hess;

  /* main integral over edge */
  area = 0.0;
  for ( i = 0 ; i < edge_ctrl ; i++ )
     for ( j = 0 ; j < edge_ctrl ; j++ )
        { REAL v = scoeff[j][i];
          if ( v == 0.0 ) continue;
          area += v*x[i][0]*x[j][1];
          g[i][0] += v*x[j][1];
          g[j][1] += v*x[i][0];
          h[i][j][0][1] += v;
          h[j][i][1][0] += v;
        }

  return area;
}


/**********************************************************************
                    Lagrange area quantity (STRING model)
**********************************************************************/

/**********************************************************************
*
*  function: q_edge_area_lagrange()
*
*  purpose: value of area integral on edge
*/

REAL q_edge_area_lagrange(e_info)
struct qinfo *e_info;
{ REAL **x;
  REAL area;
  struct gauss_lag *gl = &gauss_lagrange[web.dimension][web.gauss1D_order];
  int ctrl = web.skel[EDGE].ctrlpts;
  int m,k;

  x = e_info->x;

  /* main integral over edge */
  area = 0.0;
  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL y,dx;
     for ( y = 0.0, dx = 0.0, k = 0 ; k < ctrl ; k++ )
     { y += gl->gpoly[m][k]*x[k][1];
        dx += gl->gpolypart[m][0][k]*x[k][0];
     }
     area -= gl->gausswt[m]*y*dx;
  }
     
  return area;
}

/**********************************************************************
*
*  function: q_edge_area_lagrange_grad()
*
*  purpose: value and gradient of area integral on edge
*/

REAL q_edge_area_lagrange_grad(e_info)
struct qinfo *e_info;
{ REAL **x,**g;
  REAL area;
  int m,k;
  struct gauss_lag *gl = &gauss_lagrange[web.dimension][web.gauss1D_order];
  int ctrl = web.skel[EDGE].ctrlpts;

  x = e_info->x;
  g = e_info->grad;

  /* main integral over edge */
  area = 0.0;
  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL y,dx;
     for ( y = 0.0, dx = 0.0, k = 0 ; k < ctrl ; k++ )
     { y += gl->gpoly[m][k]*x[k][1];
        dx += gl->gpolypart[m][0][k]*x[k][0];
     }
     area -= gl->gausswt[m]*y*dx;
     for ( k = 0 ; k < ctrl ; k++ )
     { g[k][0] -= gl->gausswt[m]*y*gl->gpolypart[m][0][k];
        g[k][1] -= gl->gausswt[m]*dx*gl->gpoly[m][k];
     }
  }

  return area;
}


/**********************************************************************
*
*  function: q_edge_area_lagrange_hess()
*
*  purpose: value and gradient and hessian of area integral on edge
*/

REAL q_edge_area_lagrange_hess(e_info)
struct qinfo *e_info;
{ REAL **x,**g,****h;
  REAL area;
  int m,k,kk;
  struct gauss_lag *gl = &gauss_lagrange[web.dimension][web.gauss1D_order];
  int ctrl = web.skel[EDGE].ctrlpts;

  x = e_info->x;
  g = e_info->grad;
  h = e_info->hess;

  /* main integral over edge */
  area = 0.0;
  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL y,dx;
     for ( y = 0.0, dx = 0.0, k = 0 ; k < ctrl ; k++ )
     { y += gl->gpoly[m][k]*x[k][1];
        dx += gl->gpolypart[m][0][k]*x[k][0];
     }
     area -= gl->gausswt[m]*y*dx;
     for ( k = 0 ; k < ctrl ; k++ )
     { g[k][0] -= gl->gausswt[m]*y*gl->gpolypart[m][0][k];
        g[k][1] -= gl->gausswt[m]*dx*gl->gpoly[m][k];
        for ( kk = 0 ; kk < ctrl ; kk++ )
        { h[k][kk][0][1] -= gl->gausswt[m]*gl->gpoly[m][kk]*gl->gpolypart[m][0][k];
          h[k][kk][1][0] -= gl->gausswt[m]*gl->gpolypart[m][0][kk]*gl->gpoly[m][k];
        }
     }
  }

  return area;
}


/**********************************************************************
                    Linear torus area quantity (STRING model)
**********************************************************************/

/**********************************************************************
*
*  function: q_edge_torus_area()
*
*  purpose: value of area integral on edge
*/

REAL q_edge_torus_area(e_info)
struct qinfo *e_info;
{ REAL **x;
  REAL area;
  REAL **dx = web.inverse_periods;
  MAT2D(u,EDGE_VERTS,MAXCOORD); /* affine coordinates of vertices */
  int wrap;

  if ( !dx )
     kb_error(2142,"Need torus model to use edge_torus_area method.\n",
        RECOVERABLE);
  if ( web.modeltype == QUADRATIC ) return q_edge_torus_area_q(e_info);
  if ( web.modeltype == LAGRANGE ) 
     return q_edge_torus_area_lagrange(e_info);

  x = e_info->x;

  /* get affine coordinates of vertices */
  mat_mul_tr(x,dx,u,EDGE_VERTS,SDIM,SDIM);
  /* main integral over edge */
  area = (u[0][1]+u[1][1])*(u[0][0] - u[1][0])/2;

  /* wrap correction */
  wrap = (get_edge_wrap(e_info->id)>>TWRAPBITS) & WRAPMASK;
  area += WRAPNUM(wrap)*u[1][0];

  return area*web.torusv;
}

/**********************************************************************
*
*  function: q_edge_torus_area_grad()
*
*  purpose: value and gradient of area integral on edge
*/

REAL q_edge_torus_area_grad(e_info)
struct qinfo *e_info;
{ REAL **x;
  REAL area;
  REAL **dx = web.inverse_periods;
  MAT2D(u,EDGE_VERTS,MAXCOORD); /* affine coordinates of vertices */
  MAT2D(g,EDGE_VERTS,MAXCOORD);
  int wrap,wrapnum;

  if ( web.modeltype == QUADRATIC ) return q_edge_torus_area_q_grad(e_info);
  if ( web.modeltype == LAGRANGE ) 
     return q_edge_torus_area_lagrange_grad(e_info);

  x = e_info->x;

  /* get affine coordinates of vertices */
  mat_mul_tr(x,dx,u,EDGE_VERTS,SDIM,SDIM);
  /* main integral over edge */
  area = (u[0][1]+u[1][1])*(u[0][0] - u[1][0])/2;
  g[0][0] = (u[1][1] + u[0][1])/2*web.torusv;
  g[1][0] = -(u[1][1] + u[0][1])/2*web.torusv;
  g[0][1] = (u[0][0] - u[1][0])/2*web.torusv;
  g[1][1] = (u[0][0] - u[1][0])/2*web.torusv;

  /* wrap correction */
  wrap = (get_edge_wrap(e_info->id)>>TWRAPBITS) & WRAPMASK;
  wrapnum = WRAPNUM(wrap);
  area += wrapnum*u[1][0]; 
  g[1][0] += wrapnum*web.torusv; 

  mat_mult(g,dx,e_info->grad,EDGE_VERTS,SDIM,SDIM);
  return area*web.torusv;
}


/**********************************************************************
*
*  function: q_edge_torus_area_hess()
*
*  purpose: value and gradient and hessian of area integral on edge
*/

REAL q_edge_torus_area_hess(e_info)
struct qinfo *e_info;
{ REAL **x;
  REAL area;
  REAL **dx = web.inverse_periods;
  MAT2D(u,EDGE_VERTS,MAXCOORD); /* affine coordinates of vertices */
  MAT2D(g,EDGE_VERTS,MAXCOORD);
  MAT4D(h,EDGE_VERTS,EDGE_VERTS,MAXCOORD,MAXCOORD);
  MAT2D(temph,MAXCOORD,MAXCOORD);
  int i,ii,j,jj,wrap,wrapnum;

  if ( web.modeltype == QUADRATIC ) return q_edge_torus_area_q_hess(e_info);
  if ( web.modeltype == LAGRANGE )  
     return q_edge_torus_area_lagrange_hess(e_info);

  x = e_info->x;
  for ( i = 0 ; i < EDGE_VERTS ; i++ )
     for ( ii = 0 ; ii < EDGE_VERTS ; ii++ )
        for ( j = 0 ; j < SDIM ; j++ )
            for ( jj = 0 ; jj < SDIM ; jj++ )
              h[i][ii][j][jj] = 0.0;

  /* get affine coordinates of vertices */
  mat_mul_tr(x,dx,u,EDGE_VERTS,SDIM,SDIM);
  /* main integral over edge */
  area = (u[0][1]+u[1][1])*(u[0][0] - u[1][0])/2;
  g[0][0] = (u[1][1] + u[0][1])/2*web.torusv;
  g[1][0] = -(u[1][1] + u[0][1])/2*web.torusv;
  g[0][1] = (u[0][0] - u[1][0])/2*web.torusv;
  g[1][1] = (u[0][0] - u[1][0])/2*web.torusv;
  h[0][1][0][1] += 0.5*web.torusv;
  h[0][0][0][1] += 0.5*web.torusv;
  h[1][1][0][1] -= 0.5*web.torusv;
  h[1][0][0][1] -= 0.5*web.torusv;
  h[0][0][1][0] += 0.5*web.torusv;
  h[0][1][1][0] -= 0.5*web.torusv;
  h[1][0][1][0] += 0.5*web.torusv;
  h[1][1][1][0] -= 0.5*web.torusv;

  /* wrap correction */
  wrap = (get_edge_wrap(e_info->id)>>TWRAPBITS) & WRAPMASK;
  wrapnum = WRAPNUM(wrap);
  area += wrapnum*u[1][0]; 
  g[1][0] += wrapnum*web.torusv; 

  /* form pullback */
  mat_mult(g,dx,e_info->grad,EDGE_VERTS,SDIM,SDIM);
  for ( i = 0 ; i < EDGE_VERTS ; i++ )
    for ( ii = 0 ; ii < EDGE_VERTS ; ii++ )
    { mat_mult(h[i][ii],dx,temph,SDIM,SDIM,SDIM);
      tr_mat_mul(dx,temph,e_info->hess[i][ii],SDIM,SDIM,SDIM);
    }

  return area*web.torusv;
}


/**********************************************************************
                    Quadratic torus area quantity (STRING model)
**********************************************************************/

/**********************************************************************
*
*  function: q_edge_torus_area_q()
*
*  purpose: value of area integral on edge
*/

REAL q_edge_torus_area_q(e_info)
struct qinfo *e_info;
{ REAL **x;
  REAL area;
  REAL **dx = web.inverse_periods;
  MAT2D(u,EDGE_CTRL,MAXCOORD); /* affine coordinates of vertices */
  int i,j;
  int wrap;

  x = e_info->x;

  /* get affine coordinates of vertices */
  mat_mul_tr(x,dx,u,edge_ctrl,SDIM,SDIM);
  /* main integral over edge */
  area = 0.0;
  for ( i = 0 ; i < edge_ctrl ; i++ )
    for ( j = 0 ; j < edge_ctrl ; j++ )
    { REAL v = scoeff[i][j];
      if ( v == 0.0 ) continue;
      area += v*u[i][1]*u[j][0];
    }

  /* wrap correction */
  wrap = (get_edge_wrap(e_info->id)>>TWRAPBITS) & WRAPMASK;
  area += WRAPNUM(wrap)*u[2][0];

  return area*web.torusv;
}

/**********************************************************************
*
*  function: q_edge_torus_area_q_grad()
*
*  purpose: value and gradient of area integral on edge
*/

REAL q_edge_torus_area_q_grad(e_info)
struct qinfo *e_info;
{ REAL **x;
  REAL area;
  REAL **dx = web.inverse_periods;
  MAT2D(u,EDGE_CTRL,MAXCOORD); /* affine coordinates of vertices */
  MAT2D(g,EDGE_CTRL,MAXCOORD);
  int i,j,k;
  int wrap,wrapnum;

  x = e_info->x;
  for ( i = 0 ; i < edge_ctrl ; i++ )
     for ( j = 0 ; j < SDIM ; j++ ) g[i][j] = 0.0;

  /* get affine coordinates of vertices */
  mat_mul_tr(x,dx,u,edge_ctrl,SDIM,SDIM);
  /* main integral over edge */
  area = 0.0;
  for ( i = 0 ; i < edge_ctrl ; i++ )
    for ( j = 0 ; j < edge_ctrl ; j++ )
    { REAL v = scoeff[j][i];
      if ( v == 0.0 ) continue;
      area += v*u[i][0]*u[j][1];
      g[i][0] += v*u[j][1];
      g[j][1] += v*u[i][0];
    }

  /* wrap correction */
  wrap = (get_edge_wrap(e_info->id)>>TWRAPBITS) & WRAPMASK;
  wrapnum = WRAPNUM(wrap);
  area += wrapnum*u[2][0]; 
  g[2][0] += wrapnum;

  for ( k = 0 ; k < edge_ctrl ; k++ )
     for ( j = 0 ; j < SDIM ; j++ )
        g[k][j] *= web.torusv;
  mat_mult(g,dx,e_info->grad,edge_ctrl,SDIM,SDIM);

  return area*web.torusv;
}


/**********************************************************************
*
*  function: q_edge_torus_area_q_hess()
*
*  purpose: value and gradient and hessian of area integral on edge
*/

REAL q_edge_torus_area_q_hess(e_info)
struct qinfo *e_info;
{ REAL **x;
  REAL area;
  REAL **dx = web.inverse_periods;
  MAT2D(u,EDGE_CTRL,MAXCOORD); /* affine coordinates of vertices */
  MAT2D(g,EDGE_CTRL+1,MAXCOORD);
  REAL ****h;
  MAT2D(temph,MAXCOORD,MAXCOORD);
  int i,ii,j,k;
  int wrap,wrapnum;

  x = e_info->x;
  h = e_info->hess;
  for ( i = 0 ; i < edge_ctrl ; i++ )
     for ( j = 0 ; j < SDIM ; j++ ) g[i][j] = 0.0;

  /* get affine coordinates of vertices */
  mat_mul_tr(x,dx,u,edge_ctrl,SDIM,SDIM);
  /* main integral over edge */
  area = 0.0;
  for ( i = 0 ; i < edge_ctrl ; i++ )
     for ( j = 0 ; j < edge_ctrl ; j++ )
        { REAL v = scoeff[j][i];
          if ( v == 0.0 ) continue;
          area += v*u[i][0]*u[j][1];
          g[i][0] += v*u[j][1];
          g[j][1] += v*u[i][0];
          h[i][j][0][1] += v*web.torusv;
          h[j][i][1][0] += v*web.torusv;
        }

  /* wrap correction */
  wrap = (get_edge_wrap(e_info->id)>>TWRAPBITS) & WRAPMASK;
  wrapnum = WRAPNUM(wrap);
  area += wrapnum*u[2][0]; 
  g[2][0] += wrapnum; 

  for ( k = 0 ; k < edge_ctrl ; k++ )
     for ( j = 0 ; j < SDIM ; j++ )
        g[k][j] *= web.torusv;

  /* form pullback */
  mat_mult(g,dx,e_info->grad,edge_ctrl,SDIM,SDIM);
  for ( i = 0 ; i < edge_ctrl ; i++ )
    for ( ii = 0 ; ii < edge_ctrl ; ii++ )
    { mat_mult(h[i][ii],dx,temph,SDIM,SDIM,SDIM);
      tr_mat_mul(dx,temph,e_info->hess[i][ii],SDIM,SDIM,SDIM);
    }
  return area*web.torusv;
}


/**********************************************************************
                    Lagrange torus area quantity (STRING model)
**********************************************************************/

/**********************************************************************
*
*  function: q_edge_torus_area_lagrange()
*
*  purpose: value of area integral on edge
*/

REAL q_edge_torus_area_lagrange(e_info)
struct qinfo *e_info;
{ REAL **x;
  REAL area;
  struct gauss_lag *gl = &gauss_lagrange[web.dimension][web.gauss1D_order];
  int ctrl = web.skel[EDGE].ctrlpts;
  int m,k;
  REAL **u = e_info->u;
  int wrap;

  x = e_info->x;
  /* get affine coordinates of vertices */
  mat_mul_tr(x,web.inverse_periods,u,ctrl,SDIM,SDIM);

  /* main integral over edge */
  area = 0.0;
  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL y,dx;
     for ( y = 0.0, dx = 0.0, k = 0 ; k < ctrl ; k++ )
     { y += gl->gpoly[m][k]*u[k][1];
        dx += gl->gpolypart[m][0][k]*u[k][0];
     }
     area -= gl->gausswt[m]*y*dx;
  }
     
  /* wrap correction */
  wrap = (get_edge_wrap(e_info->id)>>TWRAPBITS) & WRAPMASK;
  area += WRAPNUM(wrap)*u[ctrl-1][0];

  return area * web.torusv;
}

/**********************************************************************
*
*  function: q_edge_torus_area_lagrange_grad()
*
*  purpose: value and gradient of area integral on edge
*/

REAL q_edge_torus_area_lagrange_grad(e_info)
struct qinfo *e_info;
{ REAL **x;
  REAL area;
  int m,j,k,i;
  struct gauss_lag *gl = &gauss_lagrange[web.dimension][web.gauss1D_order];
  int ctrl = web.skel[EDGE].ctrlpts;
  REAL **u = e_info->u;
  int wrap,wrapnum;
  MAT2D(g,MAXVCOUNT,MAXCOORD);

  x = e_info->x;
  for ( i = 0 ; i < ctrl ; i++ )
     for ( j = 0 ; j < SDIM ; j++ ) g[i][j] = 0.0;

  /* get affine coordinates of vertices */
  mat_mul_tr(x,web.inverse_periods,u,edge_ctrl,SDIM,SDIM);

  /* main integral over edge */
  area = 0.0;
  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL y,dx;
     for ( y = 0.0, dx = 0.0, k = 0 ; k < ctrl ; k++ )
     { y += gl->gpoly[m][k]*u[k][1];
        dx += gl->gpolypart[m][0][k]*u[k][0];
     }
     area -= gl->gausswt[m]*y*dx;

     for ( k = 0 ; k < ctrl ; k++ )
     { g[k][0] -= gl->gausswt[m]*y*gl->gpolypart[m][0][k];
        g[k][1] -= gl->gausswt[m]*dx*gl->gpoly[m][k];
     }
  }

  /* wrap correction */
  wrap = (get_edge_wrap(e_info->id)>>TWRAPBITS) & WRAPMASK;
  wrapnum = WRAPNUM(wrap);
  area += wrapnum*u[ctrl-1][0]; 
  g[ctrl-1][0] += wrapnum;

  for ( k = 0 ; k < ctrl ; k++ )
    for ( j = 0 ; j < SDIM ; j++ )
      g[k][j] *= web.torusv;
  mat_mult(g,web.inverse_periods,e_info->grad,ctrl,SDIM,SDIM);
  return area*web.torusv;
}


/**********************************************************************
*
*  function: q_edge_torus_area_lagrange_hess()
*
*  purpose: value and gradient and hessian of area integral on edge
*/

REAL q_edge_torus_area_lagrange_hess(e_info)
struct qinfo *e_info;
{ REAL **x,****h;
  REAL area;
  int i,ii,j;
  int m,k,kk;
  struct gauss_lag *gl = &gauss_lagrange[web.dimension][web.gauss1D_order];
  int ctrl = web.skel[EDGE].ctrlpts;
  REAL **u = e_info->u;
  int wrap,wrapnum;
  MAT2D(temph,MAXCOORD,MAXCOORD);
  MAT2D(g,MAXVCOUNT,MAXCOORD);

  x = e_info->x;
  h = e_info->hess;
  for ( i = 0 ; i < ctrl ; i++ )
     for ( j = 0 ; j < SDIM ; j++ ) g[i][j] = 0.0;

  /* get affine coordinates of vertices */
  mat_mul_tr(x,web.inverse_periods,u,edge_ctrl,SDIM,SDIM);

  /* main integral over edge */
  area = 0.0;
  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL y,dx;
     for ( y = 0.0, dx = 0.0, k = 0 ; k < ctrl ; k++ )
     { y += gl->gpoly[m][k]*u[k][1];
        dx += gl->gpolypart[m][0][k]*u[k][0];
     }
     area -= gl->gausswt[m]*y*dx;
     for ( k = 0 ; k < ctrl ; k++ )
     { g[k][0] -= gl->gausswt[m]*y*gl->gpolypart[m][0][k];
        g[k][1] -= gl->gausswt[m]*dx*gl->gpoly[m][k];
        for ( kk = 0 ; kk < ctrl ; kk++ )
        { h[k][kk][0][1] -= gl->gausswt[m]*gl->gpoly[m][kk]*gl->gpolypart[m][0][k];
          h[k][kk][1][0] -= gl->gausswt[m]*gl->gpolypart[m][0][kk]*gl->gpoly[m][k];
        }
     }
  }

  /* wrap correction */
  wrap = (get_edge_wrap(e_info->id)>>TWRAPBITS) & WRAPMASK;
  wrapnum = WRAPNUM(wrap);
  area += wrapnum*u[ctrl-1][0]; 
  g[ctrl-1][0] += wrapnum;

  for ( k = 0 ; k < ctrl ; k++ )
     for ( j = 0 ; j < SDIM ; j++ )
        g[k][j] *= web.torusv;

  /* form pullback */
  mat_mult(g,web.inverse_periods,e_info->grad,ctrl,SDIM,SDIM);
  for ( i = 0 ; i < ctrl ; i++ )
     for ( ii = 0 ; ii < ctrl ; ii++ )
        { mat_mult(h[i][ii],web.inverse_periods,temph,SDIM,SDIM,SDIM);
          tr_mat_mul(web.inverse_periods,temph,e_info->hess[i][ii],SDIM,SDIM,SDIM);
        }
  return area*web.torusv;
}


/*******************************************************************
  
     Hooke Energy - Hooke's Law to keep edge lengths nearly 
     uniform.

********************************************************************/

#define POWER_NAME "hooke_power"
#define LENGTH_NAME "hooke_length"
static int exponent_param;
static int length_param;
static REAL hooke_length, hooke_power;

/***************************************************************
*
*  function: hooke_energy_init()
*
*  purpose: initialization for hooke_energy() and 
*              hooke_energy_gradient().
*
*    No special prep.
*/

void hooke_energy_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{
  if ( web.modeltype != LINEAR )
     kb_error(1766,"hooke_energy only for LINEAR model.\n",RECOVERABLE);


  exponent_param = lookup_global(POWER_NAME);
  if ( exponent_param < 0 ) /* missing, so add */
  { exponent_param = add_global(POWER_NAME);
    globals(exponent_param)->value.real = 2.0;  /* default */
    globals(exponent_param)->flags |=  ORDINARY_PARAM;
  }
  hooke_power =  globals(exponent_param)->value.real; 
  length_param = lookup_global(LENGTH_NAME);
  if ( length_param < 0 ) /* missing, so add */
  { length_param = add_global(LENGTH_NAME);
    if ( web.representation == STRING )
       globals(length_param)->value.real 
          = web.total_area/web.skel[EDGE].count; /* default */
    else globals(length_param)->value.real 
          = sqrt(2.3*web.total_area/web.skel[FACET].count); /* default */
    globals(length_param)->flags |=  ORDINARY_PARAM;
  }
  hooke_length =  globals(length_param)->value.real; 
}

/*******************************************************************
*
*  function: hooke_energy
*
*  purpose:  energy of one edge, deviation from set edge length
*
*/

REAL hooke_energy(e_info)
struct qinfo *e_info;
{
  REAL d,diff;

  d = sqrt(SDIM_dot(e_info->sides[0][0],e_info->sides[0][0]));
  diff = fabs(d - hooke_length);
  if ( hooke_power == 0.0 ) return -log(diff);
  return pow(diff,hooke_power);
}



/*******************************************************************
*
*  function: hooke_energy_gradient
*
*  purpose:  energy grad of one edge, deviation from set edge length
*
*/

REAL hooke_energy_gradient(e_info)
struct qinfo *e_info;
{
  REAL d,diff,coeff;
  REAL energy;
  int j;

  d = sqrt(SDIM_dot(e_info->sides[0][0],e_info->sides[0][0]));
  diff = d - hooke_length;
  if ( diff == 0.0 ) return 0.0;
  if ( hooke_power == 0.0 )
  { energy = -log(fabs(diff));
     coeff = -1/diff/d;
  }
  else
  { energy =  pow(fabs(diff),hooke_power);
     coeff = hooke_power*energy/diff/d;
  }
  for ( j = 0 ; j < SDIM ; j++ )
     {
        e_info->grad[0][j] = -coeff*e_info->sides[0][0][j];
        e_info->grad[1][j] = coeff*e_info->sides[0][0][j];
     }
  return energy;
}




/*******************************************************************
*
*  function: hooke_energy_hessian
*
*  purpose:  energy hessian of one edge, deviation from set edge length
*
*/

REAL hooke_energy_hessian(e_info)
struct qinfo *e_info;
{
  REAL d,diff,coeff,egrad,ehess;
  REAL energy;
  int j,jj; 
  REAL ****h = e_info->hess;

  d = sqrt(SDIM_dot(e_info->sides[0][0],e_info->sides[0][0]));
  if ( d == 0.0 )
    return 0.0;
  diff = d - hooke_length;
  if ( diff == 0.0 )
  { energy = 0.0;
    egrad = hooke_power == 1 ? 1.0 : 0.0;
    ehess = hooke_power == 2 ? 1.0 : 0.0;
  }
  else
  {
    if ( hooke_power == 0.0 )
    { energy = -log(fabs(diff));
      coeff = -1/diff/d;
    }
    else
    { energy =  pow(fabs(diff),hooke_power);
      coeff = hooke_power*energy/diff/d;
    }
    for ( j = 0 ; j < SDIM ; j++ )
    {
       e_info->grad[0][j] = -coeff*e_info->sides[0][0][j];
       e_info->grad[1][j] = coeff*e_info->sides[0][0][j];
    }
    egrad = energy/diff;
    ehess = energy/diff/diff;
  }

  if ( hooke_power == 0.0 )
  {  if ( diff )
     for ( j = 0 ; j < SDIM ; j++ )
     { REAL f;
        f = -1/diff/d;
        h[1][1][j][j] += f;
        h[1][0][j][j] -= f;
        h[0][1][j][j] -= f;
        h[0][0][j][j] += f;
        for ( jj = 0 ; jj < SDIM ; jj++ )
        { f = (1/diff + 1/d)/diff*e_info->sides[0][0][j]*e_info->sides[0][0][jj]/d/d;
          h[1][1][j][jj] += f;
          h[1][0][j][jj] -= f;
          h[0][1][j][jj] -= f;
          h[0][0][j][jj] += f;
        }
     }
  }
  else
  {
     for ( j = 0 ; j < SDIM ; j++ )
     { REAL f;
        f = hooke_power*egrad/d;
        h[1][1][j][j] += f;
        h[1][0][j][j] -= f;
        h[0][1][j][j] -= f;
        h[0][0][j][j] += f;
        for ( jj = 0 ; jj < SDIM ; jj++ )
        { f = hooke_power*(ehess*(hooke_power-1) - egrad/d)
                     *e_info->sides[0][0][j]*e_info->sides[0][0][jj]/d/d;
          h[1][1][j][jj] += f;
          h[1][0][j][jj] -= f;
          h[0][1][j][jj] -= f;
          h[0][0][j][jj] += f;
        }
     }
  } 
  return energy;
}


/*******************************************************************
  
     Hooke Energy 2 - Hooke's Law to keep edge lengths nearly 
     uniform. Version using extra edge attribute "hooke_size"
     as base length.  Also example of using extra attributes
     in quantities.

********************************************************************/

#define POWER2_NAME "hooke2_power"
#define HOOKE2_ATTR_NAME "hooke_size"
static REAL hooke2_power;
static int hooke2_attr;  /* index number of hooke_size attribute */

/***************************************************************
*
*  function: hooke2_energy_init()
*
*  purpose: initialization for hooke2_energy() and 
*              hooke2_energy_gradient().
*
*/

void hooke2_energy_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{ edge_id e_id;

  if ( web.modeltype != LINEAR )
     kb_error(1451,"hooke2_energy only for LINEAR model.\n",RECOVERABLE);


  exponent_param = lookup_global(POWER2_NAME);
  if ( exponent_param < 0 ) /* missing, so add */
        { exponent_param = add_global(POWER2_NAME);
          globals(exponent_param)->value.real = 2.0;  /* default */
          globals(exponent_param)->flags |=  ORDINARY_PARAM;
        }
  hooke2_power =  globals(exponent_param)->value.real; 

  /* extra edge atribute */
  hooke2_attr = find_attribute(EDGE,HOOKE2_ATTR_NAME);
  if ( hooke2_attr < 0 ) /* not found */
  { int one = 1;
    hooke2_attr = add_attribute(EDGE,HOOKE2_ATTR_NAME,REAL_TYPE,0,&one /*dim*/,
          DUMP_ATTR,NULL);
     FOR_ALL_EDGES(e_id)  /* initialize to current length */
     { calc_edge(e_id);
        *((REAL*)(get_extra(e_id,hooke2_attr))) = get_edge_length(e_id);
     }
  }
}

/*******************************************************************
*
*  function: hooke2_energy
*
*  purpose:  energy of one edge, deviation from set edge length
*
*/

REAL hooke2_energy(e_info)
struct qinfo *e_info;
{
  REAL d,diff;

  d = sqrt(SDIM_dot(e_info->sides[0][0],e_info->sides[0][0]));
  diff = fabs(d - *((REAL*)get_extra(e_info->id,hooke2_attr)));
  if ( hooke2_power == 0.0 ) return -log(diff);
  return pow(diff,hooke2_power);
}



/*******************************************************************
*
*  function: hooke2_energy_gradient
*
*  purpose:  energy grad of one edge, deviation from set edge length
*
*/

REAL hooke2_energy_gradient(e_info)
struct qinfo *e_info;
{
  REAL d,diff,coeff;
  REAL energy;
  int j;

  d = sqrt(SDIM_dot(e_info->sides[0][0],e_info->sides[0][0]));
  diff = (d - *((REAL*)get_extra(e_info->id,hooke2_attr)));
  if ( diff == 0.0 ) return 0.0;
  if ( hooke2_power == 0.0 )
  { energy = -log(fabs(diff));
    coeff = -1/diff/d;
  }
  else
  { energy =  pow(fabs(diff),hooke2_power);
    coeff = hooke2_power*energy/diff/d;
  }
  for ( j = 0 ; j < SDIM ; j++ )
  {
     e_info->grad[0][j] = -coeff*e_info->sides[0][0][j];
     e_info->grad[1][j] = coeff*e_info->sides[0][0][j];
  }
  return energy;
}


/*******************************************************************
*
*  function: hooke2_energy_hessian
*
*  purpose:  energy hessian of one edge, deviation from set edge length
*
*/

REAL hooke2_energy_hessian(e_info)
struct qinfo *e_info;
{
  REAL d,diff,coeff,egrad,ehess;
  REAL energy;
  int j,jj;
  REAL ****h = e_info->hess;

  d = sqrt(SDIM_dot(e_info->sides[0][0],e_info->sides[0][0]));
  if ( d == 0.0 )
    return 0.0;
  diff = (d - *((REAL*)get_extra(e_info->id,hooke2_attr)));
  if ( diff == 0.0 ) 
  { energy = 0.0;
    egrad = hooke2_power == 1 ? 1 : 0.0;
    ehess = hooke2_power == 2 ? 1 : 0.0;
  }
  else
  {
    if ( hooke2_power == 0.0 )
    { energy = -log(fabs(diff));
      coeff = -1/diff/d;
    }
    else
    { energy = pow(fabs(diff),hooke2_power);
      coeff = hooke2_power*energy/diff/d;
    }
    for ( j = 0 ; j < SDIM ; j++ )
    {
      e_info->grad[0][j] = -coeff*e_info->sides[0][0][j];
      e_info->grad[1][j] = coeff*e_info->sides[0][0][j];
    }
    egrad = energy/diff;
    ehess = energy/diff/diff;
  }
  
  if ( hooke2_power == 0.0 )
  {  if ( diff )
     for ( j = 0 ; j < SDIM ; j++ )
     { REAL f;
        f = -1/diff/d;
        h[1][1][j][j] += f;
        h[1][0][j][j] -= f;
        h[0][1][j][j] -= f;
        h[0][0][j][j] += f;
        for ( jj = 0 ; jj < SDIM ; jj++ )
        { f = (1/diff + 1/d)/diff*e_info->sides[0][0][j]*e_info->sides[0][0][jj]/d/d;
          h[1][1][j][jj] += f;
          h[1][0][j][jj] -= f;
          h[0][1][j][jj] -= f;
          h[0][0][j][jj] += f;
        }
     }
  }
  else
  {
     for ( j = 0 ; j < SDIM ; j++ )
     { REAL f;
        f = hooke2_power*egrad/d;
        h[1][1][j][j] += f;
        h[1][0][j][j] -= f;
        h[0][1][j][j] -= f;
        h[0][0][j][j] += f;
        for ( jj = 0 ; jj < SDIM ; jj++ )
        { f = hooke2_power*(ehess*(hooke2_power-1) - egrad/d)
                     *e_info->sides[0][0][j]*e_info->sides[0][0][jj]/d/d;
          h[1][1][j][jj] += f;
          h[1][0][j][jj] -= f;
          h[0][1][j][jj] -= f;
          h[0][0][j][jj] += f;
        }
     }
  } 
  return energy;
}


/*******************************************************************
  
     Hooke Energy 3 - Hooke's Law using elastic model.
     Uses "hooke_size" edge attribute as equilibrium length.
     energy = 0.5*(length-hooke_size)^2/hooke_size
    
********************************************************************/

#define POWER3_NAME "hooke3_power"
#define HOOKE3_ATTR_NAME "hooke_size"
static REAL hooke3_power;
static int hooke3_attr;  /* index number of hooke_size attribute */

static int frickenhaus_flag; /* special feature for S. Frickenhaus */

/***************************************************************
*
*  function: hooke3_energy_init()
*
*  purpose: initialization for hooke3_energy() and 
*              hooke3_energy_gradient().
*
*/

void hooke3_energy_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{ edge_id e_id;
  int n;

  if ( web.modeltype != LINEAR )
     kb_error(1767,"hooke3_energy only for LINEAR model.\n",RECOVERABLE);

  exponent_param = lookup_global(POWER3_NAME);
  if ( exponent_param < 0 ) /* missing, so add */
  { exponent_param = add_global(POWER3_NAME);
    globals(exponent_param)->value.real = 2.0;  /* default */
    globals(exponent_param)->flags |=  ORDINARY_PARAM;
  }
  hooke3_power =  globals(exponent_param)->value.real; 

  /* extra edge atribute */
  hooke3_attr = find_attribute(EDGE,HOOKE3_ATTR_NAME);
  if ( hooke3_attr < 0 ) /* not found */
  { int one = 1;
    hooke3_attr = add_attribute(EDGE,HOOKE3_ATTR_NAME,REAL_TYPE,0,&one /*dim*/,
          DUMP_ATTR,NULL);
     FOR_ALL_EDGES(e_id)  /* initialize to current length */
     { calc_edge(e_id);
        *((REAL*)(get_extra(e_id,hooke3_attr))) = get_edge_length(e_id);
     }
  }

  n = lookup_global("frickenhaus_flag");
  if ( (n >= 0) && (globals(n)->value.real != 0.0) ) frickenhaus_flag = 1;
  else frickenhaus_flag = 0;
}

/*******************************************************************
*
*  function: hooke3_energy
*
*  purpose:  energy of one edge, deviation from set edge length
*
*/

REAL hooke3_energy(e_info)
struct qinfo *e_info;
{
  REAL d,diff,length;

  length = *((REAL*)get_extra(e_info->id,hooke3_attr));
  if ( length == 0.0 )
  { sprintf(errmsg,"Edge %s has %s zero.\n",ELNAME(e_info->id),
        HOOKE3_ATTR_NAME);
     kb_error(2143,errmsg,RECOVERABLE);
  }
  d = sqrt(SDIM_dot(e_info->sides[0][0],e_info->sides[0][0]));
  if ( frickenhaus_flag && (d < length) ) return 0.0;
  diff = fabs(d - length);
  if ( hooke3_power == 0.0 ) return -log(diff);
  return 0.5*pow(diff,hooke3_power)/length;
}



/*******************************************************************
*
*  function: hooke3_energy_gradient
*
*  purpose:  energy grad of one edge, deviation from set edge length
*
*/

REAL hooke3_energy_gradient(e_info)
struct qinfo *e_info;
{
  REAL d,diff,coeff;
  REAL energy;
  int j;
  REAL length;

  length = *((REAL*)get_extra(e_info->id,hooke3_attr));
  d = sqrt(SDIM_dot(e_info->sides[0][0],e_info->sides[0][0]));
  if ( frickenhaus_flag && (d < length) ) return 0.0;
  diff = (d - length);
  if ( diff == 0.0 ) return 0.0;
  if ( hooke3_power == 0.0 )
  { energy = -log(fabs(diff));
    coeff = -1/diff/d;
  }
  else
  { energy = 0.5*pow(fabs(diff),hooke3_power)/length;
    coeff  = hooke3_power*energy/diff/d;
  }
  for ( j = 0 ; j < SDIM ; j++ )
  {
     e_info->grad[0][j] = -coeff*e_info->sides[0][0][j];
     e_info->grad[1][j] = coeff*e_info->sides[0][0][j];
  }
  return energy;
}


/*******************************************************************
*
*  function: hooke3_energy_hessian
*
*  purpose:  energy hessian of one edge, deviation from set edge length
*
*/

REAL hooke3_energy_hessian(e_info)
struct qinfo *e_info;
{
  REAL d,diff,coeff,egrad,ehess;
  REAL energy;
  int j,jj;
  REAL ****h = e_info->hess;
  REAL length;

  length = *((REAL*)get_extra(e_info->id,hooke3_attr));
  d = sqrt(SDIM_dot(e_info->sides[0][0],e_info->sides[0][0]));
  if ( d == 0 ) 
    return 0.0;
  if ( frickenhaus_flag && (d < length) ) 
    return 0.0;
  diff = (d - length);
  if ( diff == 0.0 ) 
  { energy = 0.0;
    egrad = hooke3_power == 1 ? 1 : 0;
    ehess = hooke3_power == 2 ? 1 : 0;
  }
  else
  {
    if ( hooke3_power == 0.0 )
    { energy = -log(fabs(diff));
      coeff = -1/diff/d;
    }
    else
    { energy = 0.5*pow(fabs(diff),hooke3_power)/length;
      coeff  = hooke3_power*energy/diff/d;
    }
    for ( j = 0 ; j < SDIM ; j++ )
    {
      e_info->grad[0][j] = -coeff*e_info->sides[0][0][j];
      e_info->grad[1][j] = coeff*e_info->sides[0][0][j];
    }
    egrad = energy/diff;
    ehess = energy/diff/diff;
  }
  
  if ( hooke3_power == 0.0 )
  {  if ( diff )
     for ( j = 0 ; j < SDIM ; j++ )
     { REAL f;
        f = -1/diff/d;
        h[1][1][j][j] += f;
        h[1][0][j][j] -= f;
        h[0][1][j][j] -= f;
        h[0][0][j][j] += f;
        for ( jj = 0 ; jj < SDIM ; jj++ )
        { f = (1/diff + 1/d)/diff*e_info->sides[0][0][j]*e_info->sides[0][0][jj]/d/d;
          h[1][1][j][jj] += f;
          h[1][0][j][jj] -= f;
          h[0][1][j][jj] -= f;
          h[0][0][j][jj] += f;
        }
     }
  }
  else
  {
     for ( j = 0 ; j < SDIM ; j++ )
     { REAL f;
        f = hooke3_power*egrad/d;
        h[1][1][j][j] += f;
        h[1][0][j][j] -= f;
        h[0][1][j][j] -= f;
        h[0][0][j][j] += f;
        for ( jj = 0 ; jj < SDIM ; jj++ )
        { f = hooke3_power*(ehess*(hooke3_power-1) - egrad/d)
                     *e_info->sides[0][0][j]*e_info->sides[0][0][jj]/d/d;
          h[1][1][j][jj] += f;
          h[1][0][j][jj] -= f;
          h[0][1][j][jj] -= f;
          h[0][0][j][jj] += f;
        }
     }
  } 
  return energy;
}

/*********************************************************************

         circular_arc_length method

  Models an edge as a circular arc through three points.
Usable with quadratic model.  Uses arclength formula

  L = ||v1 - v0||*asin(sinth)/sinth

where sinth is the sin of the exterior angle between
the short chords, found with cross product.

  At request of Wacharin Wichiramala, 12-24-99.

*********************************************************************/

void circ_debug ARGS((REAL,REAL,REAL,REAL,REAL*,REAL*,REAL*,REAL*));
void circular_arc_aux ARGS((REAL,REAL,REAL,REAL,REAL*, REAL [][2],
    REAL [][2][3][2], REAL *,REAL [][2],REAL [][2][3][2],REAL *,REAL [][2],
    REAL [][2][3][2], REAL *,REAL [][2],REAL [][2][3][2],int));
REAL cirf ARGS((REAL));
REAL cirfp ARGS((REAL));
REAL cirfpp ARGS((REAL));
REAL circular_arc_length_all ARGS(( struct qinfo *, int ));
REAL caf ARGS((REAL));
REAL cafp ARGS((REAL));
REAL cafpp ARGS((REAL));
REAL circular_arc_area_all ARGS(( struct qinfo *, int ));

/*********************************************************************
*
*  function: circular_arc_length_init()
*
*  purpose:  check that we are in quadratic model.
*/

void circular_arc_length_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{ 
  if ( (web.modeltype == LAGRANGE) && (web.lagrange_order >= 2) )
    kb_error(3371,"circular_arc_length method needs LINEAR or QUADRATIC model.\n",RECOVERABLE);
  circular_arc_flag = 1;
}

/************************************************************************
*
* function circular_arc_aux()
*
* purpose: calculate various quantities common to circular arc functions
*
*/
void circ_debug(dx1,dy1,dx2,dy2,chord,sinth,costh,angle)
REAL dx1,dx2,dy1,dy2,*chord,*sinth,*costh,*angle;
{ REAL denom1,denom2;
  denom1 = sqrt(dx1*dx1+dy1*dy1);
  denom2 = sqrt(dx2*dx2+dy2*dy2);
  if (denom1*denom2 == 0.0) return;
  *chord = sqrt((dx1+dx2)*(dx1+dx2)+(dy1+dy2)*(dy1+dy2));
  *sinth = (dx1*dy2 - dy1*dx2)/denom1/denom2;
  *costh = (dx1*dx2 + dy1*dy2)/denom1/denom2;
  *angle = atan2(*sinth,*costh);
}

void circular_arc_aux(dx1,dy1,dx2,dy2,chordptr,dchord,ddchord,sinthptr,dsinth,
  ddsinth,costhptr,dcosth,ddcosth,angleptr,dangle,ddangle,mode)
REAL dx1,dy1,dx2,dy2;
REAL *chordptr,dchord[][2],ddchord[][2][3][2];
REAL *sinthptr,dsinth[][2],ddsinth[][2][3][2];
REAL *costhptr,dcosth[][2],ddcosth[][2][3][2];
REAL *angleptr,dangle[][2],ddangle[][2][3][2];
int mode;
{ REAL chord,sinth,costh,angle;
  REAL denom1,denom2;
  int i,j,ii,jj;

  denom1 = sqrt(dx1*dx1+dy1*dy1);
  denom2 = sqrt(dx2*dx2+dy2*dy2);
  if (denom1*denom2 == 0.0) {*chordptr = 0.0; return ;}
  *chordptr = chord = sqrt((dx1+dx2)*(dx1+dx2)+(dy1+dy2)*(dy1+dy2));
  *sinthptr = sinth = (dx1*dy2 - dy1*dx2)/denom1/denom2;
  *costhptr = costh = (dx1*dx2 + dy1*dy2)/denom1/denom2;
  angle = atan2(sinth,costh);
  *angleptr = angle;

  if ( mode == METHOD_VALUE ) return;

  dchord[0][0] = -(dx1+dx2)/chord;
  dchord[0][1] = -(dy1+dy2)/chord;
  dchord[1][0] = 0.0;
  dchord[1][1] = 0.0;
  dchord[2][0] = -dchord[0][0];
  dchord[2][1] = -dchord[0][1];
  dsinth[0][0] = -dy2/denom1/denom2 + sinth/denom1/denom1*dx1;
  dsinth[0][1] =  dx2/denom1/denom2 + sinth/denom1/denom1*dy1;
  dsinth[2][0] = -dy1/denom1/denom2 - sinth/denom2/denom2*dx2;
  dsinth[2][1] =  dx1/denom1/denom2 - sinth/denom2/denom2*dy2;
  dsinth[1][0] = -dsinth[0][0]-dsinth[2][0];
  dsinth[1][1] = -dsinth[0][1]-dsinth[2][1];
  dcosth[0][0] = -dx2/denom1/denom2 + costh/denom1/denom1*dx1;
  dcosth[0][1] = -dy2/denom1/denom2 + costh/denom1/denom1*dy1;
  dcosth[2][0] =  dx1/denom1/denom2 - costh/denom2/denom2*dx2;
  dcosth[2][1] =  dy1/denom1/denom2 - costh/denom2/denom2*dy2;
  dcosth[1][0] = -dcosth[0][0]-dcosth[2][0];
  dcosth[1][1] = -dcosth[0][1]-dcosth[2][1];
  for ( i = 0 ; i < 3 ; i++ )
   for ( j = 0 ; j < 2 ; j++ )
     dangle[i][j] = dsinth[i][j]*costh - dcosth[i][j]*sinth;

  if ( mode == METHOD_GRADIENT ) return;

  for ( i = 0 ; i < 3 ; i++ )
   for ( ii = 0 ; ii < 3; ii++ )
    for ( j = 0 ; j < SDIM  ; j++ )
     for ( jj = 0 ; jj < SDIM  ; jj++ )
      ddchord[i][j][ii][jj] = 0.0;

  for ( ii = 0 ; ii < 3; ii++ )
    for ( jj = 0 ; jj < SDIM  ; jj++ )
    { ddchord[0][0][ii][jj] = (dx1+dx2)/chord/chord*dchord[ii][jj];
      ddchord[0][1][ii][jj] = (dy1+dy2)/chord/chord*dchord[ii][jj];
    }
  ddchord[0][0][0][0] +=  1/chord;
  ddchord[0][0][2][0] += -1/chord;
  ddchord[0][1][0][1] +=  1/chord;
  ddchord[0][1][2][1] += -1/chord;
  for ( j = 0 ; j < SDIM  ; j++ )
   for ( ii = 0 ; ii < 3; ii++ )
    for ( jj = 0 ; jj < SDIM  ; jj++ )
      ddchord[2][j][ii][jj] = -ddchord[0][j][ii][jj];
    

  /* dsinth[0][0] = -dy2/denom1/denom2 + sinth/denom1/denom1*dx1; */
  ddsinth[0][0][0][0] =
   -(-dy2/denom1/denom2 + 2*sinth/denom1/denom1*dx1)/denom1*(-dx1)/denom1
   + dsinth[0][0]/denom1/denom1*dx1
    - sinth/denom1/denom1;
  ddsinth[0][0][0][1] =
   -(-dy2/denom1/denom2 + 2*sinth/denom1/denom1*dx1)/denom1*(-dy1)/denom1
   + dsinth[0][1]/denom1/denom1*dx1;
  ddsinth[0][0][2][0] = dy2/denom1/denom2/denom2*dx2/denom2
     + dsinth[2][0]/denom1/denom1*dx1;
  ddsinth[0][0][2][1] = -1/denom1/denom2 + dy2/denom1/denom2/denom2*dy2/denom2
   + dsinth[2][1]/denom1/denom1*dx1;
  ddsinth[0][0][1][0] = -ddsinth[0][0][0][0] - ddsinth[0][0][2][0];
  ddsinth[0][0][1][1] = -ddsinth[0][0][0][1] - ddsinth[0][0][2][1];

  /* dsinth[0][1] =  dx2/denom1/denom2 + sinth/denom1/denom1*dy1; */
  ddsinth[0][1][0][0] =
         -(dx2/denom1/denom2 + 2*sinth/denom1/denom1*dy1)/denom1*(-dx1)/denom1
        + dsinth[0][0]/denom1/denom1*dy1;
  ddsinth[0][1][0][1] =
         -(dx2/denom1/denom2 + 2*sinth/denom1/denom1*dy1)/denom1*(-dy1)/denom1
        + dsinth[0][1]/denom1/denom1*dy1 -sinth/denom1/denom1;
  ddsinth[0][1][2][0] = 1/denom1/denom2 - dx2/denom1/denom2/denom2*dx2/denom2
          + dsinth[2][0]/denom1/denom1*dy1;
  ddsinth[0][1][2][1] = -dx2/denom1/denom2/denom2*dy2/denom2
          + dsinth[2][1]/denom1/denom1*dy1;
  ddsinth[0][1][1][0] = -ddsinth[0][1][0][0] - ddsinth[0][1][2][0];
  ddsinth[0][1][1][1] = -ddsinth[0][1][0][1] - ddsinth[0][1][2][1];


  for ( j = 0 ; j < 2 ; j++ )
    for ( jj = 0 ; jj < 2 ; jj++ )
      ddsinth[2][j][0][jj] = ddsinth[0][jj][2][j];
  /* dsinth[2][0] = -dy1/denom1/denom2 - sinth/denom2/denom2*dx2; */
  ddsinth[2][0][2][0] =
    -(-dy1/denom1/denom2 - 2*sinth/denom2/denom2*dx2)/denom2*dx2/denom2
          - dsinth[2][0]/denom2/denom2*dx2
          - sinth/denom2/denom2;
  ddsinth[2][0][2][1] = 
    -(-dy1/denom1/denom2 - 2*sinth/denom2/denom2*dx2)/denom2*dy2/denom2
          - dsinth[2][1]/denom2/denom2*dx2;
  for ( jj = 0  ; jj < 2 ; jj++ )
   ddsinth[2][0][1][jj] = -ddsinth[2][0][0][jj]-ddsinth[2][0][2][jj];

  ddsinth[2][1][2][0] = ddsinth[2][0][2][1];
  ddsinth[2][1][2][1] =
    -(dx1/denom1/denom2 - 2*sinth/denom2/denom2*dy2)/denom2*dy2/denom2
    - dsinth[2][1]/denom2/denom2*dy2
    - sinth/denom2/denom2;
  for ( jj = 0  ; jj < 2 ; jj++ )
   ddsinth[2][1][1][jj] = -ddsinth[2][1][0][jj]-ddsinth[2][1][2][jj];

  for ( j = 0 ; j < 2 ; j++ )
   for ( ii = 0 ; ii < 3 ; ii++ )
    for ( jj = 0 ; jj < 2 ; jj++ )
      ddsinth[1][j][ii][jj] = -ddsinth[0][j][ii][jj]-ddsinth[2][j][ii][jj];

  /* dcosth[0][0] = -dx2/denom1/denom2 + costh/denom1/denom1*dx1; */
  ddcosth[0][0][0][0] = 
    -(-dx2/denom1/denom2 + 2*costh/denom1/denom1*dx1)/denom1*(-dx1)/denom1
     + dcosth[0][0]/denom1/denom1*dx1
     - costh/denom1/denom1; 
  ddcosth[0][0][0][1] = 
    -(-dx2/denom1/denom2 + 2*costh/denom1/denom1*dx1)/denom1*(-dy1)/denom1
     + dcosth[0][1]/denom1/denom1*dx1;
  ddcosth[0][0][2][0] = -1/denom1/denom2 + dx2/denom1/denom2/denom2*dx2/denom2
      + dcosth[2][0]/denom1/denom1*dx1;
  ddcosth[0][0][2][1] =  dx2/denom1/denom2/denom2*dy2/denom2
      + dcosth[2][1]/denom1/denom1*dx1;
  for ( jj = 0 ; jj < 2 ; jj++ )
    ddcosth[0][0][1][jj] = -ddcosth[0][0][0][jj]-ddcosth[0][0][2][jj];

  /* dcosth[0][1] = -dy2/denom1/denom2 + costh/denom1/denom1*dy1; */
  ddcosth[0][1][0][0] = 
    -(-dy2/denom1/denom2 + 2*costh/denom1/denom1*dy1)/denom1*(-dx1)/denom1
     + dcosth[0][0]/denom1/denom1*dy1;
  ddcosth[0][1][0][1] = 
    -(-dy2/denom1/denom2 + 2*costh/denom1/denom1*dy1)/denom1*(-dy1)/denom1
     + dcosth[0][1]/denom1/denom1*dy1
     - costh/denom1/denom1; 
  ddcosth[0][1][2][0] = dy2/denom1/denom2/denom2*dx2/denom2
      + dcosth[2][0]/denom1/denom1*dy1;
  ddcosth[0][1][2][1] =  -1/denom1/denom2+dy2/denom1/denom2/denom2*dy2/denom2
      + dcosth[2][1]/denom1/denom1*dy1;
  for ( jj = 0 ; jj < 2 ; jj++ )
    ddcosth[0][1][1][jj] = -ddcosth[0][1][0][jj]-ddcosth[0][1][2][jj];

  for ( j = 0 ; j < 2 ; j++ )
    for ( jj = 0 ; jj < 2 ; jj++ )
      ddcosth[2][j][0][jj] = ddcosth[0][jj][2][j];
  /* dcosth[2][0] =  dx1/denom1/denom2 - costh/denom2/denom2*dx2; */
  ddcosth[2][0][2][0] = 
     -(dx1/denom1/denom2 - 2*costh/denom2/denom2*dx2)/denom2*dx2/denom2
     - dcosth[2][0]/denom2/denom2*dx2
     - costh/denom2/denom2;
  ddcosth[2][0][2][1] =  
     -(dx1/denom1/denom2 - 2*costh/denom2/denom2*dx2)/denom2*dy2/denom2
     - dcosth[2][1]/denom2/denom2*dx2;
  for ( jj = 0  ; jj < 2 ; jj++ )
   ddcosth[2][0][1][jj] = -ddcosth[2][0][0][jj]-ddcosth[2][0][2][jj];

  /* dcosth[2][1] =  dy1/denom1/denom2 - costh/denom2/denom2*dy2; */
  ddcosth[2][1][2][0] = ddcosth[2][0][2][1];
  ddcosth[2][1][2][1] = 
     -(dy1/denom1/denom2 -2*costh/denom2/denom2*dy2)/denom2*dy2/denom2
     - dcosth[2][1]/denom2/denom2*dy2
     - costh/denom2/denom2;
  for ( jj = 0  ; jj < 2 ; jj++ )
   ddcosth[2][1][1][jj] = -ddcosth[2][1][0][jj]-ddcosth[2][1][2][jj];

  for ( j = 0 ; j < 2 ; j++ )
   for ( ii = 0 ; ii < 3 ; ii++ )
    for ( jj = 0 ; jj < 2 ; jj++ )
      ddcosth[1][j][ii][jj] = -ddcosth[0][j][ii][jj]-ddcosth[2][j][ii][jj];

  for ( i = 0 ; i < 3 ; i++ )
   for ( j = 0 ; j < 2 ; j++ )
    for ( ii = 0 ; ii < 3; ii++ )
     for ( jj = 0 ; jj < 2  ; jj++ )
       ddangle[i][j][ii][jj] = 
          ddsinth[i][j][ii][jj]*costh - ddcosth[i][j][ii][jj]*sinth
        + dsinth[i][j]*dcosth[ii][jj] - dcosth[i][j]*dsinth[ii][jj];


#ifdef XXX
/* debugging */
{ REAL dx[2][2];
  REAL ch[4],si[4],co[4],an[4];
  REAL delta = 1e-4;
  dx[0][0] = dx1;
  dx[0][1] = dy1;
  dx[1][0] = dx2;
  dx[1][1] = dy2;

/* check numerically */
  

  for ( i = 0 ; i < 2; i++ )
   for ( j = 0 ; j < 2; j++ )
    for ( ii = 0 ; ii < 2 ; ii ++ )
     for ( jj = 0 ; jj < 2 ; jj ++ )
     { dx[i][j] += delta;
       dx[ii][jj] += delta;
       circ_debug(dx[0][0],dx[0][1],dx[1][0],dx[1][1],&ch[0],&si[0],
         &co[0],&an[0]);
       dx[i][j] -= 2*delta;
       circ_debug(dx[0][0],dx[0][1],dx[1][0],dx[1][1],&ch[1],&si[1],
         &co[1],&an[1]);
       dx[ii][jj] -= 2*delta;
       circ_debug(dx[0][0],dx[0][1],dx[1][0],dx[1][1],&ch[3],&si[3],
         &co[3],&an[3]);
       dx[i][j] += 2*delta;
       circ_debug(dx[0][0],dx[0][1],dx[1][0],dx[1][1],&ch[2],&si[2],
         &co[2],&an[2]);
       dx[ii][jj] += 2*delta;

       printf("ddchord[%d][%d][%d][%d]: %18.15f %18.15f\n",2*i,j,2*ii,jj,
          (ch[0]-ch[1]-ch[2]+ch[3])/delta/delta/4,ddchord[2*i][j][2*ii][jj]);
       printf("ddsinth[%d][%d][%d][%d]: %18.15f %18.15f\n",2*i,j,2*ii,jj,
          (si[0]-si[1]-si[2]+si[3])/delta/delta/4,ddsinth[2*i][j][2*ii][jj]);
       printf("ddcosth[%d][%d][%d][%d]: %18.15f %18.15f\n",2*i,j,2*ii,jj,
          (co[0]-co[1]-co[2]+co[3])/delta/delta/4,ddcosth[2*i][j][2*ii][jj]);
       printf("ddangle[%d][%d][%d][%d]: %18.15f %18.15f\n",2*i,j,2*ii,jj,
          (an[0]-an[1]-an[2]+an[3])/delta/delta/4,ddangle[2*i][j][2*ii][jj]);
     }
}
#endif

   return;
}

/*********************************************************************
*
* function: circular_arc_length_value()
*
* purpose:  method value
*
*/

REAL circular_arc_length_value(e_info)
struct qinfo *e_info;
{ 
  REAL value = 0.0;
  REAL dx1,dx2,dy1,dy2,sinth,costh,chord,angle;
  REAL density = 1.0;

  if ( web.modeltype != QUADRATIC ) 
       return q_edge_tension_value(e_info);

  if ( METH_INSTANCE(e_info->method)->flags & USE_DENSITY )
      density = get_edge_density(e_info->id);

  dx1 = e_info->x[1][0] - e_info->x[0][0];
  dy1 = e_info->x[1][1] - e_info->x[0][1];
  dx2 = e_info->x[2][0] - e_info->x[1][0];
  dy2 = e_info->x[2][1] - e_info->x[1][1];
  circular_arc_aux(dx1,dy1,dx2,dy2,&chord,NULL,NULL,&sinth,NULL,
    NULL,&costh,NULL,NULL,&angle,NULL,NULL,METHOD_VALUE);

  
  if ( chord == 0.0 ) return 0.0;
  if ( sinth == 0.0 ) value = chord;
  else 
  { vertex_id v_id = e_info->v[1];
    value = angle*chord/sinth;
    /* fix up midpoint to center of arc */
    e_info->x[1][0] = e_info->x[0][0]+(dx1+dx2)/2+(dy1+dy2)/2*sinth/(1+costh);
    e_info->x[1][1] = e_info->x[0][1]+(dy1+dy2)/2-(dx1+dx2)/2*sinth/(1+costh);
 
    if (get_vattr(v_id) & BOUNDARY )
    { /* update boundary parameter to agree with coordinates */
      b_extrapolate(get_boundary(v_id),get_coord(v_id),e_info->x[1],
         get_coord(v_id),get_param(v_id), get_param(v_id),v_id);
    }
  }

  if ( METH_INSTANCE(e_info->method)->flags & DEFAULT_INSTANCE )
  { binary_tree_add(web.total_area_addends,value);
    set_edge_length(e_info->id,value); 
  }

  value *= density;
  return value;
}

REAL cirf(th)
REAL th;
{ if ( th == 0.0 ) return 1.0;
  return th/sin(th);
}

REAL cirfp(th)
REAL th;
{ if ( fabs(th) < .1 )
   return th*(1./3 + th*th*(7./90 + th*th*(31./2520
       + th*th*(127./75600 + th*th*73./342144))));
  return (sin(th)-th*cos(th))/sin(th)/sin(th);
}

REAL cirfpp(th)
REAL th;
{ REAL s,c;
  if ( fabs(th) < .1 )
   return (1./3 + th*th*(7./30 + th*th*(31./504
       + th*th*(127./10800 + th*th*73./38016))));
  s = sin(th); c = cos(th);
  return (-2*c/s + th*c*c/s/s + th/s/s)/s;
}

/*********************************************************************
*
* function: circular_arc_length_all()
*
* purpose:  method value, gradient and hessian
*
* NOTE: hessian_diff does not work with hessian here, since
*    value() moves midpoints.
*/

REAL circular_arc_length_all(e_info,mode)
struct qinfo *e_info;
int mode;
{ int i,j,ii,jj;
  REAL angle,value = 0.0;
  REAL dx1,dx2,dy1,dy2,sinth,costh,chord;
  REAL dchord[3][2];
  REAL dsinth[3][2];
  REAL dcosth[3][2];
  REAL dangle[3][2];
  REAL ddchord[3][2][3][2];
  REAL ddsinth[3][2][3][2];
  REAL ddcosth[3][2][3][2];
  REAL ddangle[3][2][3][2];
  REAL density = 1.0;

  if ( web.modeltype != QUADRATIC )
       return q_edge_tension_gradient(e_info);

  if ( METH_INSTANCE(e_info->method)->flags & USE_DENSITY )
      density = get_edge_density(e_info->id);

  dx1 = e_info->x[1][0] - e_info->x[0][0];
  dy1 = e_info->x[1][1] - e_info->x[0][1];
  dx2 = e_info->x[2][0] - e_info->x[1][0];
  dy2 = e_info->x[2][1] - e_info->x[1][1];
  circular_arc_aux(dx1,dy1,dx2,dy2,&chord,dchord,ddchord,&sinth,dsinth,
  ddsinth,&costh,dcosth,ddcosth,&angle,dangle,ddangle,mode);

  if (chord == 0.0) return 0.0;
  value = chord*cirf(angle);

  if ( mode == METHOD_VALUE )
   if ( METH_INSTANCE(e_info->method)->flags & DEFAULT_INSTANCE )
     binary_tree_add(web.total_area_addends,value);

  value *= density;
  if ( mode == METHOD_VALUE ) return value;

  for ( i = 0 ; i < 3 ; i++ )
    for ( j = 0 ; j < SDIM ; j++ )
      e_info->grad[i][j] = density*(dchord[i][j]*cirf(angle)
          + chord*cirfp(angle)*dangle[i][j]);
 
  if ( mode == METHOD_GRADIENT ) return value;

  for ( i = 0 ; i < 3 ; i++ )
    for ( j = 0 ; j < SDIM ; j++ )
      for ( ii = 0 ; ii < 3 ; ii++ )
        for ( jj = 0 ; jj < SDIM ; jj++ )
          e_info->hess[i][ii][j][jj] =  density*(
              ddchord[i][j][ii][jj]*cirf(angle)
                + dchord[i][j]*cirfp(angle)*dangle[ii][jj]
          + dchord[ii][jj]*cirfp(angle)*dangle[i][j]
          + chord*cirfpp(angle)*dangle[ii][jj]*dangle[i][j]
          + chord*cirfp(angle)*ddangle[i][j][ii][jj]);
 
  return value;
} /* end circular_arc_length_all */
 
REAL circular_arc_length_grad(e_info)
struct qinfo *e_info;
{ if ( web.modeltype != QUADRATIC )
       return q_edge_tension_gradient(e_info);
  return circular_arc_length_all(e_info,METHOD_GRADIENT);
}

REAL circular_arc_length_hess(e_info)
struct qinfo *e_info;
{ if ( web.modeltype != QUADRATIC )
       return q_edge_tension_hessian(e_info);
  return circular_arc_length_all(e_info,METHOD_HESSIAN);
}

REAL edge_symmetric_area_all ARGS(( struct qinfo *, int)); 
REAL spherical_arc_length_all ARGS(( struct qinfo *, int)); 
REAL spherical_arc_area_all ARGS(( struct qinfo *, int,int)); 

/**********************************************************************
*
*  function: edge_symmetric_area_all()
*
*  purpose: value and gradient and hessian of area integral on edge
*           using symmetric area. Mostly for use of circular_arc_area
*           in linear case.
*/

REAL edge_symmetric_area_all(e_info,mode)
struct qinfo *e_info;
int mode; /* METHODVALUE, etc */
{ REAL **x,**g,****h;
  REAL area;

  if ( web.torus_flag ) return q_edge_torus_area_hess(e_info);
  if ( web.modeltype == QUADRATIC ) return q_edge_area_q_hess(e_info);
  if ( web.modeltype == LAGRANGE )  return q_edge_area_lagrange_hess(e_info);

  x = e_info->x;
  g = e_info->grad;
  h = e_info->hess;

  /* main integral over edge */
  area = (x[0][0]*x[1][1] - x[0][1]*x[1][0])/2;
  if ( mode == METHOD_VALUE ) return area;

  g[0][0] = x[1][1]/2;
  g[1][0] = -x[0][1]/2;
  g[0][1] = -x[1][0]/2;
  g[1][1] = x[0][0]/2;
  if ( mode == METHOD_GRADIENT ) return area;

  h[0][1][0][1] += 0.5;
  h[1][0][0][1] -= 0.5;
  h[0][1][1][0] -= 0.5;
  h[1][0][1][0] += 0.5;

  return area;
}


/*********************************************************************

         circular_arc_area method

  Models an edge as a circular arc through three points.
Usable with quadratic model.  Uses arclength formula
of lune area plus trapezoid under chord:

  A =  ||s1+s2||^2/4*(asin(sinth)/sinth^2
           - (s1 dot s2)/(s1 cross s2))
           + (v1.y+v0.y)*(v0.x-v1.x)/2

where sinth is the sin of the exterior angle between
the short chords s1 and s2, found with cross product.

Or, in a form more amenable to small and zero angles,

  A = ||s1+s2||^2/4*(th - sin(th)*cos(th))/sin(th)^2
           + (v1.y+v0.y)*(v0.x-v1.x)/2

  At request of Wacharin Wichiramala, 12-24-99.

*********************************************************************/

REAL caf(th)
REAL th;
{ if ( fabs(th) > .1 )
    return (th - sin(th)*cos(th))/sin(th)/sin(th);
  return th*(2./3 + th*th*(4./45 + th*th*(4./315 + th*th*(8./4725 
   + th*th*4./18711))));
}

REAL cafp(th)
REAL th;
{ if ( fabs(th) > .1 )
   return  2*(sin(th) - th*cos(th))/sin(th)/sin(th)/sin(th);
  return 2./3 + th*th*(4./15 + th*th*(4./63 + th*th*(8./675 + th*th*4./2079)));
}

REAL cafpp(th)
REAL th;
{ if ( fabs(th) > .1 )
  { REAL s = sin(th), c = cos(th);
    return (4*th + 2*th*(c*c-s*s) - 6*s*c)/s/s/s/s;
  }
  return th*(8./15 + th*th*(16./63 + th*th*(16./225 + th*th*32./2079)));
}
/*********************************************************************
*
*  function: circular_arc_area_init()
*
*  purpose:  check that we are in quadratic model.
*/

void circular_arc_area_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{
  if ( (web.modeltype == LAGRANGE) && (web.lagrange_order >= 2) )
    kb_error(2860,"circular_arc_area method needs LINEAR or QUADRATIC model.\n",RECOVERABLE);
}

/**************************************************************************
*
* function: circular_arc_area_all()
*
* purpose: combined function for circular_arc_area method.
*/

REAL circular_arc_area_all(e_info,mode)
struct qinfo *e_info;
int mode;
{ int i,j,ii,jj;
  REAL value = 0.0;
  REAL dx1,dx2,dy1,dy2,sinth,chord,costh,angle;
  REAL dsinth[3][2],dchord[3][2],dangle[3][2],dcosth[3][2];
  REAL ddsinth[3][2][3][2],ddchord[3][2][3][2],ddangle[3][2][3][2];
  REAL ddcosth[3][2][3][2];
  int wrap=0;
  MAT2D(u,EDGE_CTRL,MAXCOORD); /* affine coordinates of vertices */

  if ( web.modeltype != QUADRATIC )
       return q_edge_area_grad(e_info);

  dx1 = e_info->x[1][0] - e_info->x[0][0];
  dy1 = e_info->x[1][1] - e_info->x[0][1];
  dx2 = e_info->x[2][0] - e_info->x[1][0];
  dy2 = e_info->x[2][1] - e_info->x[1][1];
  circular_arc_aux(dx1,dy1,dx2,dy2,&chord,dchord,ddchord,&sinth,dsinth,
    ddsinth,&costh,dcosth,ddcosth,&angle,dangle,ddangle,mode);

  if (chord == 0.0) return 0.0;

  if ( web.torus_flag )
  { wrap = (get_edge_wrap(e_info->id)>>TWRAPBITS) & WRAPMASK;
    /* get affine coordinates of vertices */
    mat_mul_tr(e_info->x,web.inverse_periods,u,edge_ctrl,SDIM,SDIM);
    value = chord*chord/4*caf(angle)
             + web.torusv*(u[0][1]+u[2][1])*(u[0][0] - u[2][0])/2;
    if ( wrap )
    { /* wrap correction */
      value += WRAPNUM(wrap)*u[2][0]*web.torusv;
    }
  }
  else if ( web.symmetric_content )
  { value = chord*chord/4*caf(angle)
      + (e_info->x[0][0]*e_info->x[2][1]-e_info->x[0][1]*e_info->x[2][0])/2;
  }
  else
  { value = chord*chord/4*caf(angle)
            - (dx1+dx2)*(e_info->x[2][1]+e_info->x[0][1])/2;
  }


  if ( mode == METHOD_VALUE ) return value;

  /* Gradient */

  /* parallelogram gradient */
  if ( web.torus_flag )
  { MAT2D(g,EDGE_CTRL,MAXCOORD);
    g[0][0] = (u[2][1] + u[0][1])/2*web.torusv;
    g[2][0] = -(u[2][1] + u[0][1])/2*web.torusv;
    g[0][1] = (u[0][0] - u[2][0])/2*web.torusv;
    g[2][1] = (u[0][0] - u[2][0])/2*web.torusv;
    g[1][0] = g[1][1] = 0.0;
    if ( wrap )
      g[2][0] += WRAPNUM(wrap)*web.torusv;
    mat_mult(g,web.inverse_periods,e_info->grad,edge_ctrl,SDIM,SDIM);
  }
  else if ( web.symmetric_content )
  { e_info->grad[2][1] +=  e_info->x[0][0]/2;
    e_info->grad[0][1] -=  e_info->x[2][0]/2;
    e_info->grad[0][0] +=  e_info->x[2][1]/2;
    e_info->grad[2][0] -=  e_info->x[0][1]/2;
  }
  else
  { e_info->grad[2][1] -= (dx1+dx2)/2;
    e_info->grad[0][1] -= (dx1+dx2)/2;
    e_info->grad[0][0] -= -(e_info->x[2][1]+e_info->x[0][1])/2;
    e_info->grad[2][0] -=  (e_info->x[2][1]+e_info->x[0][1])/2;
  }
  /* lune gradient */
  for ( i = 0 ; i < 3 ; i++ )
   for ( j = 0 ; j < 2 ; j++ )
     e_info->grad[i][j] += chord*dchord[i][j]/2*caf(angle)
                           + chord*chord/4*cafp(angle)*dangle[i][j];


  if ( mode == METHOD_GRADIENT ) return value;

  /* Hessian */

  /* parallelogram hessian */
  if ( web.torus_flag )
  { MAT2D(temph,MAXCOORD,MAXCOORD);
    MAT4D(h,EDGE_CTRL,EDGE_CTRL,MAXCOORD,MAXCOORD);
    for ( i = 0 ; i < EDGE_CTRL ; i++ )
     for ( ii = 0 ; ii < EDGE_CTRL ; ii++ )
       for ( j = 0 ; j < SDIM ; j++ )
        for ( jj = 0 ; jj < SDIM ; jj++ )
          h[i][ii][j][jj] = 0.0;
    h[0][2][0][1] = 0.5*web.torusv;
    h[0][0][0][1] = 0.5*web.torusv;
    h[2][2][0][1] = -0.5*web.torusv;
    h[2][0][0][1] = -0.5*web.torusv;
    h[0][0][1][0] = 0.5*web.torusv;
    h[0][2][1][0] = -0.5*web.torusv;
    h[2][0][1][0] = 0.5*web.torusv;
    h[2][2][1][0] = -0.5*web.torusv;
    /* form pullback */
    for ( i = 0 ; i < EDGE_CTRL ; i++ )
      for ( ii = 0 ; ii < EDGE_CTRL ; ii ++ )
      { mat_mult(h[i][ii],web.inverse_periods,temph,SDIM,SDIM,SDIM);
        tr_mat_mul(web.inverse_periods,temph,e_info->hess[i][ii],SDIM,SDIM,SDIM);
      }
  }
  else if ( web.symmetric_content )
  { e_info->hess[2][0][1][0] +=  1./2;
    e_info->hess[0][2][1][0] -=  1./2;
    e_info->hess[0][2][0][1] +=  1./2;
    e_info->hess[2][0][0][1] -=  1./2;
  }
  else
  { e_info->hess[2][0][1][0] += 1./2;
    e_info->hess[2][2][1][0] -= 1./2;
    e_info->hess[0][0][1][0] += 1./2;
    e_info->hess[0][2][1][0] -= 1./2;
    e_info->hess[0][0][0][1] += 1./2;
    e_info->hess[0][2][0][1] += 1./2;
    e_info->hess[2][0][0][1] -= 1./2;
    e_info->hess[2][2][0][1] -= 1./2;
  }

  /* chord hessian */
  for ( i = 0 ; i < 3 ; i++ )
   for ( j = 0 ; j < 2 ; j++ )
    for ( ii = 0 ; ii < 3 ; ii++ )
     for ( jj = 0 ; jj < 2 ; jj++ )
       e_info->hess[i][ii][j][jj] += 
           dchord[ii][jj]*dchord[i][j]/2*caf(angle)
         + chord*ddchord[i][j][ii][jj]/2*caf(angle)
         + chord*dchord[i][j]/2*cafp(angle)*dangle[ii][jj]
         + 2*dchord[ii][jj]*chord/4*cafp(angle)*dangle[i][j]
         + chord*chord/4*cafpp(angle)*dangle[ii][jj]*dangle[i][j]
         + chord*chord/4*cafp(angle)*ddangle[i][j][ii][jj];

  return value; 
} /* end circular_arc_area_all() */



/*********************************************************************
*
* function: circular_arc_area_value()
*
* purpose:  method value
*
*/

REAL circular_arc_area_value(e_info)
struct qinfo *e_info;
{ REAL area;

  if ( web.modeltype != QUADRATIC )
  { if ( web.symmetric_content )
      area = edge_symmetric_area_all(e_info,METHOD_VALUE);
    else
      return q_edge_area(e_info);  /* does own facet area */
  }
  else
    area = circular_arc_area_all(e_info,METHOD_VALUE);

  if ( METH_INSTANCE(e_info->method)->flags & DEFAULT_INSTANCE )
  { /* add to facet area */
    body_id b_id,bb_id;
    facetedge_id fe_id = get_edge_fe(e_info->id);
    facetedge_id fe;
    facet_id f_id;

    b_id = GEN_QUANT(METH_INSTANCE(e_info->method)->quant)->b_id;
    fe = fe_id;
    do
    { f_id = get_fe_facet(fe);
      bb_id = get_facet_body(f_id);
      if ( equal_id(b_id,bb_id) )
         add_facet_area(f_id,area);
      bb_id = get_facet_body(inverse_id(f_id));
      if ( equal_id(b_id,bb_id) )
         add_facet_area(f_id,area);
      fe = get_next_facet(fe);
    } while ( !equal_id(fe,fe_id) );
  }
  return area;
}

/*********************************************************************
*
* function: circular_arc_area_grad()
* purpose:  method gradient
*
*/

REAL circular_arc_area_grad(e_info)
struct qinfo *e_info;
{
  if ( web.modeltype != QUADRATIC )
  { if ( web.symmetric_content )
      return edge_symmetric_area_all(e_info,METHOD_GRADIENT);
    else
       return q_edge_area_grad(e_info);
  }

  return circular_arc_area_all(e_info,METHOD_GRADIENT);
}

/*********************************************************************
*
* function: circular_arc_area_hess()
* purpose:  method hessian
*
*/

REAL circular_arc_area_hess(e_info)
struct qinfo *e_info;
{
  if ( web.modeltype != QUADRATIC )
  { if ( web.symmetric_content )
      return edge_symmetric_area_all(e_info,METHOD_HESSIAN);
    else
       return q_edge_area_hess(e_info);
  }

  return circular_arc_area_all(e_info,METHOD_HESSIAN);
}

/*************************************************************************/

/**********************************************************************
  SPHERICAL ARC methods
    Linear model only for the moment
**********************************************************************/

/*********************************************************************
*
* function: spherical_arc_length_all()
*
* purpose:  method value, gradient and hessian
*
*/

REAL spherical_arc_length_all(e_info,mode)
struct qinfo *e_info;
int mode;
{ int i,j;
  REAL density = 1.0;
  REAL radius = sqrt(SDIM_dot(e_info->x[0],e_info->x[0]));
  REAL c,value;

  /* Linear mode only for now */

  if ( METH_INSTANCE(e_info->method)->flags & USE_DENSITY )
      density = get_edge_density(e_info->id);

  c = SDIM_dot(e_info->sides[0][0],e_info->sides[0][0]);
  value = 2*radius*asin(sqrt(c)/2/radius);
  

  if ( mode == METHOD_VALUE )
   if ( METH_INSTANCE(e_info->method)->flags & DEFAULT_INSTANCE )
   { binary_tree_add(web.total_area_addends,value);
     set_edge_length(e_info->id,value); 
   }

  value *= density;
  if ( mode == METHOD_VALUE ) return value;

  /* gradient */
  for ( i = 0 ; i < SDIM ; i++ )
  { REAL g =
      density*2*radius/sqrt(4*radius*radius - c)/sqrt(c)*
            e_info->sides[0][0][i];
    e_info->grad[0][i] = -g;
    e_info->grad[1][i] =  g;
  }
 
  if ( mode == METHOD_GRADIENT ) return value;

  /* hessian */
  for ( i = 0 ; i < SDIM ; i++ )
    for ( j = 0 ; j < SDIM ; j++ )
    { REAL h,d;
      d = sqrt(4*radius*radius - c);
      h = (-.5)/d/d/d*(-2*e_info->sides[0][0][j])/sqrt(c)* 
            e_info->sides[0][0][i]
       +  1/sqrt(4*radius*radius - c)*(-.5)/sqrt(c)
             /sqrt(c)/sqrt(c)*2*e_info->sides[0][0][j]*
            e_info->sides[0][0][i];
      if ( i == j ) 
        h +=  1/sqrt(4*radius*radius - c)/sqrt(c);
      h *= density*2*radius;
      e_info->hess[0][0][i][j] =  h;
      e_info->hess[1][0][i][j] = -h;
      e_info->hess[0][1][i][j] = -h;
      e_info->hess[1][1][i][j] =  h;
    }  
 
  return value;
} /* end spherical_arc_length_all */
 
REAL spherical_arc_length_value(e_info)
struct qinfo *e_info;
{ if ( web.modeltype != LINEAR )
       return q_edge_tension_gradient(e_info);
  return spherical_arc_length_all(e_info,METHOD_VALUE);
}

REAL spherical_arc_length_grad(e_info)
struct qinfo *e_info;
{ if ( web.modeltype != LINEAR )
       return q_edge_tension_gradient(e_info);
  return spherical_arc_length_all(e_info,METHOD_GRADIENT);
}

REAL spherical_arc_length_hess(e_info)
struct qinfo *e_info;
{ if ( web.modeltype != LINEAR )
       return q_edge_tension_hessian(e_info);
  return spherical_arc_length_all(e_info,METHOD_HESSIAN);
}


/***********************************************************************
    Named Method spherical_arc_area
    Assumes vertices of an edge are on a sphere (in any dimension).
    Value is the area of the spherical triangle to the south pole.
***********************************************************************/

#define NORTH 1
#define SOUTH -1

void spherical_arc_area_init(mode,mi)
int mode;
struct method_instance *mi;
{
  if ( web.modeltype != LINEAR )
    kb_error(3944,"spherical_arc_area only in LINEAR mode.\n",RECOVERABLE);
}

REAL spherical_arc_area_all(e_info,mode,pole)
struct qinfo *e_info;
int mode;
int pole; /* NORTH or SOUTH */
{ int i,j,ii,jj;
  REAL area;
  REAL *x[2];
  REAL rad = pole*sqrt(SDIM_dot(e_info->x[0],e_info->x[0]));
  REAL normal[3],dndx[3][2][3]; /* normal component derivatives */
  REAL ss1,ss2,ss12;
  REAL legnorm[2][3],dlegnorm[2][3][2][3];
  REAL ang1,ang2,ang12;
  REAL denom[2],denom12;
  REAL ddenom[2][2][3],ddenom12[2][3];

  x[0] = e_info->x[0];
  x[1] = e_info->x[1];

  /* area from pole */

  normal[0] = x[0][1]*x[1][2] - x[0][2]*x[1][1];
  normal[1] = x[0][2]*x[1][0] - x[0][0]*x[1][2];
  normal[2] = x[0][0]*x[1][1] - x[0][1]*x[1][0];
 
  /* L'Huilier's formula, nonsingular for degenerate triangles */
  ss1 = (x[0][0]*x[0][0] + x[0][1]*x[0][1] + (x[0][2]-rad)*(x[0][2]-rad))/
             rad/rad/4;
  ss2 = (x[1][0]*x[1][0] + x[1][1]*x[1][1] + (x[1][2]-rad)*(x[1][2]-rad))/
             rad/rad/4;
  ss12 = ((x[1][0]-x[0][0])*(x[1][0]-x[0][0]) 
        + (x[1][1]-x[0][1])*(x[1][1]-x[0][1]) 
        + (x[1][2]-x[0][2])*(x[1][2]-x[0][2]))/rad/rad/4;
  ang1 = atan(sqrt(ss1/(1-ss1)));
  ang2 = atan(sqrt(ss2/(1-ss2)));
  ang12 = atan(sqrt(ss12/(1-ss12)));
  area = 4*atan(sqrt(fabs(tan((ang1+ang2+ang12)/2)*tan((-ang1+ang2+ang12)/2)*
                     tan((ang1-ang2+ang12)/2)*tan((ang1+ang2-ang12)/2))));
                     /* need fabs here in case of slightly negative values */
  area *= rad*rad;
/*printf("%20.15f %20.15f %20.15f\n",ang1,ang2,ang12);*/
  if ( (normal[2] > 0.0 && pole==SOUTH) || (normal[2] < 0.0 && pole==NORTH) ) 
    area = -area;
   
     
  if ( mode == METHOD_VALUE ) return area;

  /* Gradient follows geometric derivation of strip widths,
     rather than blindly differentiating value expression.
     Get nice expression for gradient on a side from A to B:
             (A x B)*R/(R^2 + A.B)
     which is nonsingular for small edges.
  */

  memset(legnorm,0,sizeof(legnorm));
  legnorm[0][0] =  pole*x[0][1];
  legnorm[0][1] = -pole*x[0][0];
  legnorm[1][0] = -pole*x[1][1];
  legnorm[1][1] =  pole*x[1][0];
  denom[0] = rad*rad + rad*x[0][2];
  denom[1] = rad*rad + rad*x[1][2];
  denom12  = rad*rad + x[0][0]*x[1][0] + x[0][1]*x[1][1] + x[0][2]*x[1][2];
  for ( i = 0 ; i < 2 ; i++ )
    for ( j = 0 ; j < 3 ; j++ )
      e_info->grad[i][j] = 
             pole*rad*(legnorm[i][j]/denom[i] - normal[j]/denom12);

  if ( mode == METHOD_GRADIENT ) return area;

  /* gradient of normal */
  memset(dndx,0,sizeof(dndx));
  /* i = normal component, j = vertex number, k = vertex coord */
  for ( i = 0 ; i < 3 ; i++ )
  { int ii = i >= 2 ? i-2 : i+1;
    int iii = i >= 1 ? i-1 : i+2;

    dndx[i][0][ii] = x[1][iii];
    dndx[i][0][iii] = -x[1][ii];
    dndx[i][1][iii] = x[0][ii];
    dndx[i][1][ii] = -x[0][iii];
  }

  memset(ddenom,0,sizeof(ddenom));
  ddenom[0][0][2] = rad;
  ddenom[1][1][2] = rad;

  for ( i = 0 ; i < 2 ; i++ )
    for ( j = 0 ; j < 3 ; j++ )
      ddenom12[i][j] = x[1-i][j];

  memset(dlegnorm,0,sizeof(dlegnorm));
  dlegnorm[0][0][0][1] = -pole;
  dlegnorm[0][1][0][0] = pole;
  dlegnorm[1][0][1][1] = pole;
  dlegnorm[1][1][1][0] = -pole;


    /* Hessian */
  for ( i = 0 ; i < 2 ; i++ )
   for ( ii = 0 ; ii < 2 ; ii++ )
    for ( j = 0 ; j < 3 ; j++ )
     for ( jj = 0 ; jj < 3; jj++ )
     { e_info->hess[i][ii][j][jj] =
             pole*rad*(
    dlegnorm[i][j][ii][jj]/denom[i] - dndx[j][ii][jj]/denom12
    + legnorm[i][j]/denom[i]*(-1/denom[i]*ddenom[i][ii][jj]) 
       - normal[j]/denom12*(-1/denom12*ddenom12[ii][jj])
         );
     }

  return area;
}

 
REAL spherical_arc_area_n_value(e_info)
struct qinfo *e_info;
{ if ( web.modeltype != LINEAR )
       return 0;
  return spherical_arc_area_all(e_info,METHOD_VALUE,NORTH);
}

REAL spherical_arc_area_n_grad(e_info)
struct qinfo *e_info;
{ if ( web.modeltype != LINEAR )
       return 0;
  return spherical_arc_area_all(e_info,METHOD_GRADIENT,NORTH);
}

REAL spherical_arc_area_n_hess(e_info)
struct qinfo *e_info;
{ if ( web.modeltype != LINEAR )
       return 0;
  return spherical_arc_area_all(e_info,METHOD_HESSIAN,NORTH);
}

 
REAL spherical_arc_area_s_value(e_info)
struct qinfo *e_info;
{ if ( web.modeltype != LINEAR )
       return 0;
  return spherical_arc_area_all(e_info,METHOD_VALUE,SOUTH);
}

REAL spherical_arc_area_s_grad(e_info)
struct qinfo *e_info;
{ if ( web.modeltype != LINEAR )
       return 0;
  return spherical_arc_area_all(e_info,METHOD_GRADIENT,SOUTH);
}

REAL spherical_arc_area_s_hess(e_info)
struct qinfo *e_info;
{ if ( web.modeltype != LINEAR )
       return 0;
  return spherical_arc_area_all(e_info,METHOD_HESSIAN,SOUTH);
}

