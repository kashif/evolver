/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/************************************************************************
*
*  File:  metric.c
*
*  Contents: Functions to calculate area, energy and their gradients
*                according to the LINEAR STRING model.
*                With background metric.
*/

#include "include.h"

REAL euclidean_area;    /* euclidean area for conformal metrics */

/************************************************************************
*
*  Calculates all forces on control points due to edge and
*  accumulates them at each control point.
*/

void edge_force_l_metric(e_id)
edge_id e_id;
{
  REAL v[MAXCOORD],len,f,fp,*tforce,*hforce;
  int i,j,k;
  vertex_id tv = get_edge_tailv(e_id);
  vertex_id hv = get_edge_headv(e_id);
  REAL density = get_edge_density(e_id);
  REAL g[MAXCOORD][MAXCOORD];
  REAL g_partial[MAXCOORD][MAXCOORD][MAXCOORD];
  REAL *xt=get_coord(tv);
  REAL *xh=get_coord(hv);
  REAL midx[MAXCOORD];
  REAL gg=0.0,gg_partial[MAXCOORD];

  /* force due to linear tension, metric evaluated at midpoint */
  for ( i = 0 ; i < SDIM ; i++ )
  { midx[i] = (xt[i] + xh[i])/2;
    v[i] = xh[i] - xt[i];
  }
  if ( web.conformal_flag )
  { eval_all(&web.metric[0][0],midx,SDIM,&gg,gg_partial,e_id);
    len = gg*SDIM_dot(v,v);
  }
  else
  { for ( i = 0 ; i < SDIM ; i++ )
      for ( j = 0 ; j < SDIM ; j++ )
        eval_all(&web.metric[i][j],midx,SDIM,&g[i][j],g_partial[i][j],e_id);
    for ( len = 0.0, i = 0 ; i < SDIM ; i++ )
      for ( j = 0 ; j < SDIM ; j++ )
        len += v[i]*g[i][j]*v[j];
  }
  len = sqrt(len);
  tforce = get_force(tv);
  hforce = get_force(hv);
  for ( k = 0 ; k < SDIM ; k++ )
  { if ( web.conformal_flag )
    { fp = gg_partial[k]*SDIM_dot(v,v);
      f  = gg*v[k];
    }
    else
      for ( f = fp = 0.0, i = 0 ; i < SDIM ; i++ )
      { for ( j = 0 ; j < SDIM ; j++ )
          fp += g_partial[i][j][k]*v[i]*v[j]/4;
        f += g[k][i]*v[i];
      }
    tforce[k] += density*(f-fp)/len;
    hforce[k] -= density*(f+fp)/len;
  }
  set_edge_length(e_id,len);

}


/************************************************************************
*
*  Returns energy due to one edge.
*
*/

void edge_energy_l_metric(e_id)
edge_id e_id;
{
  REAL energy;
  REAL midx[MAXCOORD];
  int i,j;
  vertex_id tv = get_edge_tailv(e_id);
  vertex_id hv = get_edge_headv(e_id);
  REAL *xt=get_coord(tv);
  REAL *xh=get_coord(hv);
  REAL v[MAXCOORD];
  REAL euclidean;

  /* energy due to linear tension, metric evaluated at midpoint */
  for ( i = 0 ; i < SDIM ; i++ )
  { midx[i] = (xt[i] + xh[i])/2;
    v[i] = xh[i] - xt[i];
  }
  if ( web.conformal_flag )
  { REAL gg = eval(&web.metric[0][0],midx,e_id,NULL);
    euclidean = SDIM_dot(v,v);
    energy = gg*euclidean;
    euclidean_area += sqrt(euclidean);
  }
  else
    for ( energy = 0.0, i = 0 ; i < SDIM ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
        energy += v[i]*v[j]*eval(&web.metric[i][j],midx,e_id,NULL);
  energy = sqrt(energy);

  if ( web.representation == STRING )
      /* don't count triple junction as area */
      binary_tree_add(web.total_area_addends,energy);

  energy *= get_edge_density(e_id);
  binary_tree_add(web.total_energy_addends,energy);

}

/************************************************************************
*
*  Function: edge_energy_q_metric()
*
    Purpose:  Finds energy due to one edge in metric model.
*
*  Quadratic version.
*/

void edge_energy_q_metric(e_id)
edge_id e_id;
{
  REAL *pt[EDGE_CTRL];
  REAL tang[MAXCOORD];
  vertex_id v[EDGE_CTRL];
  int i,j,k;
  REAL gpt[MAXCOORD];
  REAL len;
  REAL euclidean;

  v[0] = get_edge_tailv(e_id);
  v[1] = get_edge_midv(e_id);
  v[2] = get_edge_headv(e_id);
  for ( i = 0 ; i < EDGE_CTRL ; i++ )
    pt[i] = get_coord(v[i]);
     
  /* calculate tangents at integration points and accumulate */
  for ( i = 0 ; i < EDGE_INTERP ; i++ )
  { for ( j = 0 ; j < SDIM ; j ++ )
    { tang[j] = 0.0;
      gpt[j] = 0.0;
      for ( k = 0 ; k < EDGE_CTRL ; k++ )
      { tang[j] += sdip[k][i]*pt[k][j];
        gpt[j] += gcombo[k][i]*pt[k][j];
      }
    }
    if ( web.conformal_flag )
    { euclidean = eval(&web.metric[0][0],gpt,e_id,NULL);
      len = euclidean*SDIM_dot(tang,tang);
      euclidean_area += gauss2wt[i]*sqrt(euclidean);
    }
    else
      for ( len = 0.0, k = 0 ; k < SDIM ; k++ )
        for ( j = 0 ; j < SDIM ; j++ )
          len += tang[k]*tang[j]*eval(&web.metric[k][j],gpt,e_id,NULL);
    len = gauss2wt[i]*sqrt(len);
    if ( web.representation == STRING )
    { /* don't count triple junction as area */
      binary_tree_add(web.total_area_addends,len);
      /* accumulate  area around each vertex to scale motion */
      add_vertex_star(v[0],len);
      add_vertex_star(v[1],len);
      add_vertex_star(v[2],len);
    }

    len *= get_edge_density(e_id);
    binary_tree_add(web.total_energy_addends,len);
  }
     
  return;
}

/************************************************************************
*
*  Function: edge_force_q_metric()
*
*  Purpose:  Finds force due to one edge in metric model.
*
*  Quadratic version.
*/

void edge_force_q_metric(e_id)
edge_id e_id;
{
  REAL *pt[EDGE_CTRL];
  REAL tang[MAXCOORD];
  vertex_id v[EDGE_CTRL];
  int i,j,k,m,n;
  REAL gpt[MAXCOORD];
  REAL g[MAXCOORD][MAXCOORD];
  REAL g_partial[MAXCOORD][MAXCOORD][MAXCOORD];
  REAL *force[EDGE_CTRL];
  REAL f,fp,fudge;
  REAL density = get_edge_density(e_id);
  REAL len;
  REAL vv = 1.0;
          
  v[0] = get_edge_tailv(e_id);
  v[1] = get_edge_midv(e_id);
  v[2] = get_edge_headv(e_id);
  for ( i = 0 ; i < EDGE_CTRL ; i++ )
  { pt[i] = get_coord(v[i]);
    force[i] = get_force(v[i]);
  }
     
  /* calculate tangents at integration points and accumulate */
  for ( i = 0 ; i < EDGE_INTERP ; i++ )
  { for ( j = 0 ; j < SDIM ; j ++ )
    { tang[j] = 0.0;
      gpt[j] = 0.0;
      for ( k = 0 ; k < EDGE_CTRL ; k++ )
      { tang[j] += sdip[k][i]*pt[k][j];
        gpt[j] += gcombo[k][i]*pt[k][j];
      }
    }
    if ( web.conformal_flag )
    { eval_all(&web.metric[0][0],gpt,SDIM,&g[0][0],g_partial[0][0],e_id);
      vv = SDIM_dot(tang,tang);
      len = vv*g[0][0];
    }
    else
    { for ( k = 0 ; k < SDIM ; k++ )
        for ( j = 0 ; j < SDIM ; j++ )
          eval_all(&web.metric[k][j],gpt,SDIM,&g[k][j],g_partial[k][j],e_id);
        for ( len = 0.0, k = 0 ; k < SDIM ; k++ )
          for ( j = 0 ; j < SDIM ; j++ )
            len += tang[k]*g[k][j]*tang[j];
    }
    if ( len <= 0.0 ) continue;
    len = sqrt(len);
    fudge = density*gauss2wt[i]/len/2;
    for ( m = 0 ; m < EDGE_CTRL ; m++ )
      for ( n = 0 ; n < SDIM ; n++ )
      { if ( web.conformal_flag )
        { fp = g_partial[0][0][n]*vv;
          f  = g[0][0]*v[n];
        }
        else
          for ( f = fp = 0.0, k = 0 ; k < SDIM ; k++ )
          { for ( j = 0 ; j < SDIM ; j++ )
              fp += g_partial[k][j][n]*v[k]*v[j];
            f += g[n][k]*v[k];
          }
          force[m][n] -= fudge*(2*sdip[m][i]*f + gcombo[m][i]*fp);
     }
  }

  return;
}

/**********************************************************************
*
*  function: simplex_energy_metric()
*
*  purpose:  universal simplex area calculator.  Does both linear
*                and quadratic models.
*
*  return value: area of simplex. caller must multiply by facet density.
*
*  globals used: web.dimension
*                     SDIM
*                     gauss_wt
*                     gauss_pt
*                     gpoly
*                     gpolypartial
*                     ctrl_num
*                     gauss2D_num
*
**********************************************************************/

REAL simplex_energy_metric(v,pt)
vertex_id *v; /* list of vertices */
REAL **pt;  /* pointer to list of vertex coords for simplex */
                    /* order must agree with that used to set up gauss arrays */
{ int i,j,k;
  REAL gpt[MAXCOORD];
  REAL area = 0.0;  /* total */
  REAL new_area;
  MAT2D(tang,MAXCOORD,MAXCOORD);
  REAL det;

  /* calculate integrands at integration points and accumulate */
  for ( k = 0 ; k < gauss2D_num ; k++ )
  { /* get gauss pt */
    vec_mat_mul(gpoly[k],pt,gpt,ctrl_num,SDIM);

    /* get tangents */
    mat_mult(gpolypartial[k],pt,tang,web.dimension,ctrl_num, SDIM);

    /* evaluate metric and fill in determinant */
    if ( web.conformal_flag )
    { metric[0][0] = eval(&web.metric[0][0],gpt,NULLID,NULL);
      for (  i = 0 ; i < web.dimension ; i++ )
        for ( j = 0 ; j <= i ; j++ )
          det_array[i][j] = det_array[j][i] =
                 metric[0][0]*SDIM_dot(tang[i],tang[j]);
    }
    else
    { for (  i = 0 ; i < SDIM ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
          metric[i][j] = eval(&web.metric[i][j],gpt,NULLID,NULL);
        for (  i = 0 ; i < web.dimension ; i++ )
          for ( j = 0 ; j <= i ; j++ )
            det_array[i][j] = det_array[j][i] =
                    quadratic_form(tang[i],metric,tang[j],SDIM);
    }
    /* evaluate determinant and add to total */
    det = determinant(det_array,web.dimension);
    if ( det < 0.0 )
      kb_error(2170,"Metric not positive definite.\n",RECOVERABLE);
    new_area = gauss2Dwt[k]*sqrt(det);
    area += new_area;
    if ( web.conformal_flag )
      euclidean_area += new_area/pow(metric[0][0],web.dimension/2.0)
                                /web.simplex_factorial;
  }

  area /= web.simplex_factorial;

  return area;
}


/**********************************************************************
*
*  function: simplex_force_metric()
*
*  purpose:  Universal simplex force calculator.  Does both linear
*                and quadratic models.  Adds forces to vertices.
*
*  globals used: web.dimension
*                     SDIM
*                     gauss_wt
*                     gauss_pt
*                     gpoly
*                     gpolypartial
*                     ctrl_num
*                     gauss2D_num
*
**********************************************************************/

#ifdef ANSI_DEF
void simplex_force_metric(
vertex_id *v,  /* pointer to list of vertex ID's for simplex */
                    /* order must agree with that used to set up gauss arrays */
REAL **pt,      /* coords to used (unwrapped ) */
REAL density,  /* surface tension of facet */
REAL **forces)
#else
void simplex_force_metric(v,pt,density,forces)
vertex_id *v;  /* pointer to list of vertex ID's for simplex */
                    /* order must agree with that used to set up gauss arrays */
REAL **pt;      /* coords to used (unwrapped ) */
REAL density;  /* surface tension of facet */
REAL **forces;
#endif
{ int i,j,k,mu,nu,m,n;
  REAL gpt[MAXCOORD];
  REAL area = 0.0;  /* total */
  REAL tempvec1[MAXCOORD],tempvec2[MAXCOORD];
  REAL qsum[MAXCOORD];
  REAL vv[MAXCOORD][MAXCOORD];
  MAT2D(tang,MAXCOORD,MAXCOORD);

  /* calculate integrands at integration points and accumulate */
  for ( k = 0 ; k < gauss2D_num ; k++ )
  { /* get gauss pt */
    vec_mat_mul(gpoly[k],pt,gpt,ctrl_num,SDIM);

    /* get tangents */
    mat_mult(gpolypartial[k],pt,tang,web.dimension,ctrl_num,SDIM);

    /* evaluate metric and fill in determinant */
    if ( web.conformal_flag )
    { eval_all(&web.metric[0][0],gpt,SDIM,&metric[0][0], 
             metric_partial[0][0],NULLID); 
      for (  i = 0 ; i < web.dimension ; i++ )
        for ( j = 0 ; j <= i ; j++ )
        { vv[i][j] = vv[j][i] = SDIM_dot(tang[i],tang[j]);
          det_array[i][j] = det_array[j][i] = metric[0][0]*vv[i][j];
        }
    }
    else
    { for (  i = 0 ; i < SDIM ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
          eval_all(&web.metric[i][j],gpt,SDIM,&metric[i][j],
                metric_partial[i][j],NULLID);
        for (  i = 0 ; i < web.dimension ; i++ )
          for ( j = 0 ; j <= i ; j++ )
            det_array[i][j] = det_array[j][i] =
                quadratic_form(tang[i],metric,tang[j],SDIM);
    }

    area = det_adjoint(det_array,web.dimension);
    /* det_array now has adjoint transpose */
    if ( area <= 0.0 )
    { kb_error(1581,"Zero area facet.\n",WARNING);
      for ( m = 0 ; m < ctrl_num ; m++ )
        for ( mu = 0 ; mu < SDIM ; mu++ )
          forces[m][mu] = 0.0;
      return;
    }
    area = sqrt(area);

    for (  i = 0 ; i < web.dimension ; i++ )
      for ( j = 0 ; j < web.dimension ; j++ )
      { REAL factor = 
          gauss2Dwt[k]*density*det_array[j][i]/2/area/web.simplex_factorial;

        if ( web.conformal_flag )
        { for ( m = 0 ; m < SDIM ; m++ )
          { tempvec1[m] = metric[0][0]*tang[i][m];
            tempvec2[m] = metric[0][0]*tang[j][m];
          }
          for ( n = 0 ; n < SDIM ; n++ )
            qsum[n] = metric_partial[0][0][n]*vv[i][j];
        }
        else
        { matvec_mul(metric,tang[i],tempvec1,SDIM,SDIM);
          matvec_mul(metric,tang[j],tempvec2,SDIM,SDIM);
          for ( n = 0 ; n < SDIM ; n++ )
            for ( qsum[n] = 0.0,mu = 0 ; mu < SDIM ; mu++ )
              for ( nu = 0 ; nu < SDIM ; nu++ )
                 qsum[n] += tang[i][mu]*metric_partial[mu][nu][n]*tang[j][nu];
        }

        for ( m = 0 ; m < ctrl_num ; m++ )
          for ( mu = 0 ; mu < SDIM ; mu++ )
          { forces[m][mu] -= factor* 
                              (gpolypartial[k][j][m]*tempvec1[mu]
                                  + gpolypartial[k][i][m]*tempvec2[mu]
                                  + gpoly[k][m]*qsum[mu]);
          }
      } 
    }

  return;
}

/*********************************************************************
*
*  function: metric_form_to_vector()
*
*  purpose: convert form to vector using metric tensor.
*              Does conversion in place.
*/

void  metric_form_to_vector(x,f)
REAL *x;  /* coordinates */
REAL *f;  /* form incoming, vector outgoing */
{ int i,j;
  REAL temp[MAXCOORD];
  REAL rr,rf;

  if ( klein_metric_flag )
  { /* M^-1 = (I - rxr)*(1-r^2) */
    rr = SDIM_dot(x,x);
    rf = SDIM_dot(x,f);
    for ( j = 0 ; j < SDIM ; j++ )
      f[j] = (f[j] - x[j]*rf)*(1-rr);
    return;
  }

  if ( web.conformal_flag )
  { REAL gg = eval(&web.metric[0][0],x,NULLID,NULL);
    if ( gg == 0.0 )
      kb_error(1343,"Metric evaluates to zero.\n",WARNING);
    else for ( j = 0 ; j < SDIM ; j++ )
      f[j] /= gg;
    return;
  }

  /* if here, have general metric */
  for (  i = 0 ; i < SDIM ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
         metric[i][j] = eval(&web.metric[i][j],x,NULLID,NULL);
  mat_inv(metric,SDIM);
  matvec_mul(metric,f,temp,SDIM,SDIM);
  memcpy((char*)f,(char*)temp,SDIM*sizeof(REAL));

}


/***********************************************************************
                             Metric_facet_area Method
                             General metric
                             Works for edges, facets, and simplices
***********************************************************************/

REAL metric_area_all ARGS((struct qinfo*,int));

/**********************************************************************
*
*  function: metric_area_init()
*
*  purpose: make sure general metric is in effect.
*/
void metric_area_init(mode,mi)
int mode;
struct method_instance *mi;
{  if ( web.conformal_flag || !web.metric_flag )
     kb_error(1583,"Cannot use metric_facet_area or metric_edge_length method without a general metric.\n",
      RECOVERABLE);
}

/**********************************************************************
*
*  function: metric_area_hess()
*
*  purpose:  Universal simplex metric area hess.  Does both linear
*                and quadratic models.  
*
*  globals used: web.dimension
*                     SDIM
*                     gauss_wt
*                     gauss_pt
*                     gpoly
*                     gpolypartial
*                     ctrl_num
*                     gauss2D_num
*
**********************************************************************/

REAL metric_area_all(q_info,mode)
struct qinfo *q_info;
int mode; /* gradient or hessian */
{
  REAL density=0.0;  /* surface tension of facet */
  REAL **grad = q_info->grad;
  int i,j,k,mu,nu,m,n;
  REAL *gpt;
  REAL area = 0.0;  /* total */
  REAL value = 0.0;
  struct gauss_lag *gl = (web.modeltype == LAGRANGE) ? 
       &gauss_lagrange[web.dimension][web.gauss2D_order] : NULL;
  int cpts = (web.modeltype == LAGRANGE) ? gl->lagpts : ctrl_num;
  int gpts = (web.modeltype == LAGRANGE) ? gl->gnumpts : gauss2D_num;
  REAL **gp = (web.modeltype == LAGRANGE) ? gl->gpoly : gpoly;
  REAL ***gpp = (web.modeltype == LAGRANGE) ? gl->gpolypart : gpolypartial;

  MAT4D(metric_second,MAXCOORD,MAXCOORD,MAXCOORD,MAXCOORD);
  REAL det;
  MAT2D(adjA,MAXCOORD,MAXCOORD); /* adjoint of TGT */
  REAL ****adjAdA = NULL;
  MAT2D(dA,MAXCOORD,MAXCOORD); /* derivative of TGT */
  REAL **DdetA = NULL; /* derivs of det */
  MAT2D(GT,MAXCOORD,MAXCOORD); /* GT */
  MAT3D(DGT,MAXCOORD,MAXCOORD,MAXCOORD); /* derivative of GT */
  MAT4D(TDDGT,MAXCOORD,MAXCOORD,MAXCOORD,MAXCOORD); /* 2nd derivative of TGT */
  REAL sum,tr1,tr2,dda,factor;
  int a,b;

  switch(id_type(q_info->id))
  { case EDGE: 
       if ( METH_INSTANCE(q_info->method)->flags & USE_DENSITY )
          density = get_edge_density(q_info->id);
       else density = 1.0;
       break;
    case FACET: 
       if ( METH_INSTANCE(q_info->method)->flags & USE_DENSITY )
          density = get_facet_density(q_info->id); 
       else density = 1.0;
       break;
    default: kb_error(1584,"Metric_area method only for edge and facet.\n",
       RECOVERABLE);
  }

  DdetA = dmatrix(0,cpts-1,0,MAXCOORD-1); /* derivs of det */
  adjAdA = dmatrix4(cpts,cpts,MAXCOORD,MAXCOORD); /* adjoint of TGT times dA */

  /* calculate integrands at integration points and accumulate */
  for ( k = 0 ; k < gpts ; k++ )
  { REAL **tang = q_info->sides[k];
    REAL wt = (web.modeltype == LAGRANGE) ? gl->gausswt[k] : gauss2Dwt[k];
    gpt = q_info->gauss_pt[k];

    /* evaluate metric (using symmmetry) and fill in determinant */
    for (  i = 0 ; i < SDIM ; i++ )
      for ( j = 0 ; j <= i ; j++ )
      {  switch ( mode )
         { case METHOD_VALUE:
                    metric[i][j] = eval(&web.metric[i][j],gpt,q_info->id,NULL);
                    metric[j][i] = metric[i][j];
                    break;

           case METHOD_GRADIENT:
                    eval_all(&web.metric[i][j],gpt,SDIM,&metric[i][j],
                         metric_partial[i][j],q_info->id);
                    if ( i != j )
                    { metric[j][i] = metric[i][j];
                      for ( mu = 0 ; mu < SDIM ; mu++ )
                         metric_partial[j][i][mu] = metric_partial[i][j][mu];
                    }
                    break;

           case METHOD_HESSIAN:
                    eval_second(&web.metric[i][j],gpt,SDIM,&metric[i][j],
                        metric_partial[i][j],metric_second[i][j],q_info->id);
                    if ( i != j )
                    { metric[j][i] = metric[i][j];
                      for ( mu = 0 ; mu < SDIM ; mu++ )
                      { metric_partial[j][i][mu] = metric_partial[i][j][mu];
                         for ( nu = 0 ; nu < SDIM ; nu++ )
                            metric_second[j][i][mu][nu] = metric_second[i][j][mu][nu];
                      }
                    }
                    break;
         }
       }

      for (  i = 0 ; i < web.dimension ; i++ )
        for ( j = 0 ; j <= i ; j++ )
           adjA[i][j] = adjA[j][i] =
                    quadratic_form(tang[i],metric,tang[j],SDIM);

      det = det_adjoint(adjA,web.dimension);
      if ( det <= 0.0 ) continue;
      area = sqrt(det);
      value += wt*area;
      if ( mode == METHOD_VALUE ) continue;

      /* adjA now has adjoint transpose (but assuming symmetry) */

      /* get some useful products */
      for ( i = 0 ; i < SDIM ; i++ )
        for ( a = 0 ; a < web.dimension ; a++ )
           GT[i][a] = SDIM_dot(metric[i],tang[a]);
      for ( i = 0 ; i < SDIM ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
          for ( a = 0 ; a < web.dimension ; a++ )
          { for ( n = 0, sum = 0.0 ; n < SDIM ; n++ )
                sum += metric_partial[i][n][j]*tang[a][n];
             DGT[i][j][a] = sum;
          }

      factor = wt*density/2/area/web.simplex_factorial;
      /* get determinant gradients with respect to control pt coords */
      for ( m = 0 ; m < cpts ; m++ )
        for ( i = 0 ; i < SDIM ; i++ )
        {  for ( a = 0 ; a < web.dimension ; a++ )
             for ( b = 0 ; b <= a ; b++ )
             { sum = 0.0;
               for ( mu = 0 ; mu < SDIM ; mu++ )
                  sum += tang[a][mu]*DGT[mu][i][b];
               dA[a][b] = dA[b][a] = sum*gp[k][m]
                     + gpp[k][a][m]*GT[i][b]
                     + gpp[k][b][m]*GT[i][a];
             }
             mat_mult(adjA,dA,adjAdA[m][i],web.dimension,web.dimension,
                 web.dimension);
             /* gradient is trace */
             for ( a = 0, sum = 0.0 ; a < web.dimension ; a++ )
                sum += adjAdA[m][i][a][a];
             DdetA[m][i] = sum;
             grad[m][i] += factor*sum;
          }

      if ( mode == METHOD_GRADIENT ) continue;

      /* now, second derivatives and Hessians */    
        for ( i = 0 ; i < SDIM ; i++ )
         for ( j = 0 ; j < SDIM ; j++ )
          for ( a = 0 ; a < web.dimension ; a++ )
            for ( b = 0 ; b < web.dimension ; b++ )
             { sum = 0.0;
               for ( mu = 0 ; mu < SDIM ; mu++ )
                 for ( nu = 0 ; nu < SDIM ; nu++ )
                    sum += tang[a][mu]*metric_second[mu][nu][i][j]*tang[b][nu];
               TDDGT[a][i][j][b] = sum;
             }
        factor = wt*density/4/area/det/web.simplex_factorial;
        for ( m = 0 ; m < cpts ; m++ )
         for ( i = 0 ; i < SDIM ; i++ )
          for ( n = 0 ; n <= m ; n++ )
            for ( j = 0 ; j < SDIM ; j++ )
            { tr1 = 0.0;
              tr2 = 0.0;
              for ( a = 0 ; a < web.dimension ; a++ )
                for ( b = 0 ; b < web.dimension ; b++ )
                { dda  = gpp[k][a][m]*gp[k][n]*DGT[i][j][b];
                  dda += gpp[k][a][n]*gp[k][m]*DGT[j][i][b];
                  dda += gpp[k][b][n]*gp[k][m]*DGT[j][i][a];
                  dda += gpp[k][b][m]*gp[k][n]*DGT[i][j][a];
                  dda += gpp[k][a][m]*gpp[k][b][n]*metric[i][j];
                  dda += gpp[k][a][n]*gpp[k][b][m]*metric[j][i];
                  dda += gp[k][m]*gp[k][n]*TDDGT[a][i][j][b];
                  tr2 += adjA[a][b]*dda;
                  tr1 += adjAdA[m][i][a][b]*adjAdA[n][j][b][a];
                }
              q_info->hess[m][n][i][j] += factor*(DdetA[m][i]*DdetA[n][j]
                      - 2*tr1 + 2*det*tr2);
            }     
     } /* end gauss loop */

  /* fill in symmetric part */
  if ( mode == METHOD_HESSIAN )
    for ( m = 0 ; m < cpts ; m++ )
      for ( i = 0 ; i < SDIM ; i++ )
        for ( n = m+1 ; n < cpts ; n++ )
          for ( j = 0 ; j < SDIM ; j++ )
            q_info->hess[m][n][i][j] = q_info->hess[n][m][j][i];

  area = value/web.simplex_factorial;
  if ( quantities_only_flag )
  { if ( web.representation == STRING )
      set_edge_length(q_info->id,area);
     else set_facet_area(q_info->id,area);
#ifdef SHARED_MEMORY
     if ( nprocs > 1 ) 
      proc_total_area[GET_THREAD_ID] += area;
     else
#endif
     binary_tree_add(web.total_area_addends,area);
  }

  free_matrix4(adjAdA);
  free_matrix(DdetA);

  return density*area;
} /* end metric_area_all() */

REAL metric_area_value(q_info)
struct qinfo *q_info;
{ return metric_area_all(q_info,METHOD_VALUE);
}

REAL metric_area_grad(q_info)
struct qinfo *q_info;
{ return metric_area_all(q_info,METHOD_GRADIENT);
}

REAL metric_area_hess(q_info)
struct qinfo *q_info;
{ return metric_area_all(q_info,METHOD_HESSIAN);
}
