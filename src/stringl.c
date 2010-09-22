/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/************************************************************************
*
*  File:  stringl.c
*
*  Contents: Functions to calculate area, energy and their gradients
*                according to the LINEAR STRING model.
*/

#include "include.h"


/************************************************************************
*
*  Returns energy due to one edge.
*  Find edge's contribution to total length and facet areas.
*  Areas with respect to origin are calculated 
*  and then oriented contributions added for each facet. 
*
*/

void edge_energy_l(e_id)
edge_id e_id;
{
  REAL energy;
  vertex_id v[2];
  int i;
  REAL *x[2];
  REAL xx[MAXCOORD];
  REAL side[MAXCOORD];

  v[0] = get_edge_tailv(e_id);
  v[1] = get_edge_headv(e_id);
  x[0] = get_coord(v[0]);
  x[1] = get_coord(v[1]);
  if ( web.symmetry_flag )
  { (*sym_wrap)(x[1],xx,get_edge_wrap(e_id));
    x[1] = xx;
  }
  for ( i = 0 ; i < SDIM ; i++ ) side[i] = x[1][i] - x[0][i];

  /* energy due to linear tension */
  if ( web.metric_flag )
  { if ( klein_metric_flag )
       energy = klein_length(x[0],x[1]);
    else
       energy = simplex_energy_metric(v,x);
  }
  else
  { /* calculate length energy */
    energy = sqrt(SDIM_dot(side,side));
    set_edge_length(e_id,energy);
    if ( web.wulff_flag ) 
    { REAL wulff[MAXCOORD];    /* energy covector to side vector */
      (*get_wulff)(side,wulff);
      energy = SDIM_dot(side,wulff);
    }
  }
  if ( web.representation == STRING )
      web.total_area    += energy;    /* don't count triple junction as area */

  energy *= get_edge_density(e_id);

  /* calculate gravitational energy */
  if ( web.gravflag && !(get_eattr(e_id) & NONCONTENT) )
  { REAL *t,*h;
    REAL grav_e;
    REAL density = edge_grav_density(e_id);

    if ( density != 0.0 )
    { t = get_coord(v[0]);
      h = get_coord(v[1]);
      grav_e = (t[0]-h[0])*(t[1]*t[1] + t[1]*h[1] + h[1]*h[1])/6;
      energy += density*grav_e;
    }
  }                    

  web.total_energy = web.total_energy + energy;

}

/************************************************************************
*
*  Calculates all forces on control points due to edge and
*  accumulates them at each control point.
*/

void edge_force_l(e_id)
edge_id e_id;
{
  REAL len;
  REAL side[MAXCOORD];
  int i,j;
  vertex_id hv,tv;
  REAL density = get_edge_density(e_id);
  REAL forces[2][MAXCOORD];  /* total forces from this facet */
  REAL *forceptr[2];    /* pointers to forces */
  REAL *force[2];  /* vertex forces */
  WRAPTYPE wraps[2];
  REAL xx[MAXCOORD];
  REAL *x[2];
  vertex_id v[2];

  v[0] = tv = get_edge_tailv(e_id);
  v[1] = hv = get_edge_headv(e_id);
  x[0] = get_coord(v[0]);
  x[1] = get_coord(v[1]);
  for ( i = 0 ; i < 2 ; i++ ) forceptr[i] = forces[i];
  if ( web.symmetry_flag )
  { (*sym_wrap)(x[1],xx,get_edge_wrap(e_id));
    x[1] = xx;
  }
  for ( i = 0 ; i < SDIM ; i++ ) side[i] = x[1][i] - x[0][i];

  memset((char*)forces,0,sizeof(forces));  /* set to 0 */

  add_vertex_valence(tv,1);
  add_vertex_valence(hv,1);

  /* force due to linear tension */
  if ( web.metric_flag )
  { if ( klein_metric_flag )
    { len = klein_length(x[0],x[1]);
      klein_length_grad(x[0],x[1],forces[0],forces[1]);
    }
    else
    { len = simplex_energy_metric(v,x);
      simplex_force_metric(v,x,density,forceptr);
    }
  }
  else len = sqrt(SDIM_dot(side,side));

  set_edge_length(e_id,len);
  /* add to endpoint stars */
  add_vertex_star(tv,len);
  add_vertex_star(hv,len);

  if ( !web.metric_flag ) 
    if ( len != 0.0 ) 
     for ( i = 0 ; i < SDIM ; i++ )
     { forces[0][i] += density*side[i]/len;
       forces[1][i] -= density*side[i]/len;
     }

  /* calculate gravitational forces */
  if ( web.gravflag && !(get_eattr(e_id) & NONCONTENT) )
  { REAL *t,*h;
    REAL gdensity = edge_grav_density(e_id);

    if ( gdensity != 0.0 )
    { t = get_coord(tv);
      h = get_coord(hv);
      forces[0][0] += -gdensity*(t[1]*t[1] + t[1]*h[1] + h[1]*h[1])/6;
      forces[1][0] +=  gdensity*(t[1]*t[1] + t[1]*h[1] + h[1]*h[1])/6;
      forces[0][1] += -gdensity*(t[0]-h[0])*(2*t[1] + h[1])/6;
      forces[1][1] += -gdensity*(t[0]-h[0])*(t[1] + 2*h[1])/6;
    }
  }                    



  force[0] = get_force(tv);
  force[1] = get_force(hv);
  if ( web.symmetry_flag )
  { REAL wforce[MAXCOORD];  /* unwrapped forces */
    REAL *t = get_coord(tv);

    for ( j = 0 ; j < SDIM ; j++ )
      force[0][j] += forces[0][j];
    wraps[1] = get_edge_wrap(e_id);
    (*sym_form_pullback)(t,wforce,forces[1],wraps[1]);
    for ( j = 0 ; j < SDIM ; j++ )
      force[1][j] += wforce[j];
  }
  else
    for ( i = 0 ; i < 2 ; i++ )
      for ( j = 0 ; j < SDIM ; j++ )
              force[i][j] += forces[i][j];
}

/******************************************************************
*
*  Function: edge_grav_density()
*
*  Purpose: Finds gravitational energy density multiplier across
*              an edge in the string model. Includes grav const.
*/

REAL edge_grav_density(e_id)
edge_id e_id;
{
  facetedge_id fe,first_fe;
  body_id  b_id;
  REAL density = 0.0;
  facet_id f_id;

  /* if body is on facet agreeing with edge orientation,
      then pressure is positive. */

  fe = first_fe = get_edge_fe(e_id);
  while ( valid_id(fe) )
  { f_id = get_fe_facet(fe);
    b_id = get_facet_body(f_id);
    if ( (valid_id(b_id)) && (get_battr(b_id) & DENSITY) )
       density += get_body_density(b_id);
    b_id = get_facet_body(inverse_id(f_id));
    if ( (valid_id(b_id)) && (get_battr(b_id) & DENSITY) )
       density -= get_body_density(b_id);
    fe = get_next_facet(fe);
    if ( equal_id(fe,first_fe) ) break;
  }

  return web.grav_const*density;
}
 
/**********************************************************************
*
*  Adds contribution of edge to areas of neighboring facets, via
*  Green's theorem.    x-y plane only.
*
*/

void edge_area_l(fe_id)
facetedge_id fe_id;
{
  REAL *xt,*xh;
  REAL area;
  body_id b_id1,b_id2; /* bodies to add area to */
  facet_id f_id1,f_id2; /* facets to add area to */
  edge_id e_id = get_fe_edge(fe_id);

  if ( get_eattr(e_id) & NONCONTENT ) return;

  if ( inverted(e_id) ) { fe_id = inverse_id(fe_id); e_id = inverse_id(e_id); }

  f_id1 = get_fe_facet(fe_id);
  f_id2 = facet_inverse(get_fe_facet(fe_id));
  if ( !valid_id(f_id1) && !valid_id(f_id2) ) return; /* no facets */
  b_id1 = get_facet_body(f_id1);
  b_id2 = get_facet_body(f_id2);

  if ( web.torus_flag )
  { REAL v1,v2,v3,v4;
    int wx,wy; /* wraps */
    WRAPTYPE wrap = get_edge_wrap(e_id);
  
     /* new way, accurate modulo unit cell area */

    area = 0.0;
    xt = get_coord(get_edge_tailv(e_id));     
    xh = get_coord(get_edge_headv(e_id));     
    v1 = dot(web.inverse_periods[0],xt,2);
    v2 = dot(web.inverse_periods[0],xh,2);
    v3 = dot(web.inverse_periods[1],xt,2);
    v4 = dot(web.inverse_periods[1],xh,2);

    wx = WRAPNUM(wrap & WRAPMASK);
    wy = WRAPNUM((wrap>>TWRAPBITS) & WRAPMASK);
    area += wy*(v2+wx);
     
    area -= ((v2+wx - v1)*(v4+wy + v3)/2 );
    area *= web.torusv;
  }  /* end torus */
  else
  { 
    xt = get_coord(get_edge_tailv(e_id));     
    xh = get_coord(get_edge_headv(e_id));     
     
    /* calculate area above x1 axis */ 
    area = (xt[0] - xh[0])*(xt[1] + xh[1])/2;
  }

  /* add to cell areas */
  if ( valid_id(f_id1) )
     add_facet_area(f_id1,area);
  
  if ( valid_id(b_id1) )
     add_body_volume(b_id1,area);
  
  if ( valid_id(b_id2) )
     add_body_volume(b_id2,-area);
}

/***********************************************************************
*
*  Function: string_grad_l()
*
*  Purpose:  construct vertex area gradients
*            Note: force is local, so toroidal wrappings don't matter,
*            as long as we get the edges right, which get_edge_side() does.
*
*/

void string_grad_l()
{
  body_id bi_id;  /* identifier for body i */
  facetedge_id fe_id;
  REAL side[MAXCOORD];
  volgrad *hvgptr,*tvgptr;
  vertex_id headv,tailv;
  facet_id f_id;
  edge_id e_id;
  REAL *xt,*xh;
  int i;

  FOR_ALL_FACETEDGES(fe_id)
  {
    /* see which side has body, if any */
    f_id = get_fe_facet(fe_id);
    bi_id = get_facet_body(f_id);
    if ( !valid_id(bi_id) ) 
    { invert(fe_id);
      bi_id = get_facet_body(get_fe_facet(fe_id));
    }
    if ( !valid_id(bi_id) ) continue;  /* nothing here */
    if ( !(get_battr(bi_id) & (FIXEDVOL|PRESSURE) ) ) continue;
   
	e_id = get_fe_edge(fe_id);
    if ( get_eattr(e_id) & NONCONTENT ) continue;

    get_edge_side(e_id,side);

    headv = get_fe_headv(fe_id);
    tailv = get_fe_tailv(fe_id);
    xt = get_coord(tailv);
    xh = get_coord(headv);

    /* gradient due to edge */

    hvgptr = get_bv_new_vgrad(get_body_fixnum(bi_id),headv);
    hvgptr->bb_id = bi_id;
    tvgptr = get_bv_new_vgrad(get_body_fixnum(bi_id),tailv);
    tvgptr->bb_id = bi_id;

    if ( web.torus_flag )
    { REAL v1,v2,v3,v4;
      int wx=0,wy=0; /* wraps */
      WRAPTYPE wrap = get_edge_wrap(e_id);
      REAL hgrad[MAXCOORD];
      REAL tgrad[MAXCOORD];
  
     /* new way, accurate modulo unit cell area */

      for ( i = 0 ; i < SDIM ; i++ ) hgrad[i] = tgrad[i] = 0.0;
      xt = get_coord(get_edge_tailv(e_id));     
      xh = get_coord(get_edge_headv(e_id));     
      v1 = dot(web.inverse_periods[0],xt,2);
      v2 = dot(web.inverse_periods[0],xh,2);
      v3 = dot(web.inverse_periods[1],xt,2);
      v4 = dot(web.inverse_periods[1],xh,2);

      wx = WRAPNUM(wrap & WRAPMASK);
      wy = WRAPNUM((wrap>>TWRAPBITS) & WRAPMASK);
      if ( wy )
        for ( i = 0 ; i < SDIM ; i++ )
          hgrad[i] += wy*web.inverse_periods[0][i];
     
      for ( i = 0 ; i < SDIM ; i++ )
      { hgrad[i] -= (v4+wy + v3)/2*web.inverse_periods[0][i];
        tgrad[i] += (v4+wy + v3)/2*web.inverse_periods[0][i];
        hgrad[i] -= (v2+wx - v1)/2*web.inverse_periods[1][i];
        tgrad[i] -= (v2+wx - v1)/2*web.inverse_periods[1][i];
      }
      for ( i = 0 ; i < SDIM ; i++ ) 
      { hvgptr->grad[i] += web.torusv*hgrad[i];
        tvgptr->grad[i] += web.torusv*tgrad[i];
      }
    }  /* end torus */
    else
    { /* grads of  area = (xt[0] - xh[0])*(xt[1] + xh[1])/2 */
      hvgptr->grad[0] += -(xt[1] + xh[1])/2;
      hvgptr->grad[1] +=  (xt[0] - xh[0])/2;
      tvgptr->grad[0] +=  (xt[1] + xh[1])/2;
      tvgptr->grad[1] +=  (xt[0] - xh[0])/2;
    }
  }

}

/*******************************************************************
*
*  Function: string_bdry_grad()
*
*  Purpose: Add cell area gradients due to boundary integrals.
*/

void string_bdry_grad()
{
  vertex_id v_id;
  REAL dummy,partial[MAXCOORD]; /* for eval_all */
  int i;

  FOR_ALL_VERTICES(v_id)
  {
    volgrad *vgptri;
    struct boundary *bdry;
    ATTR attr = get_vattr(v_id);
    edge_id e_id, start_e, next_e;

    if ( attr & FIXED ) continue;
    if ( !(attr & BOUNDARY) ) continue;
    if ( !(attr & BDRY_CONTENT) ) continue;

    bdry = get_boundary(v_id);
    if ( bdry->pcount != 1 ) return ;

    start_e = get_vertex_edge(v_id);
    if ( !valid_id(start_e) ) continue;
    next_e = start_e; 

    do
    { facetedge_id fe_id0,fe_id;
      facet_id f_id;
      body_id bf;
      int sign;

      e_id = next_e;
      fe_id0 = get_edge_fe(e_id);
      next_e = get_next_tail_edge(e_id);
      if ( !valid_id(fe_id0) ) continue;
      if ( get_eattr(e_id) & NONCONTENT ) continue;

      eval_all(bdry->convect[0],get_param(v_id),bdry->pcount,&dummy,
         partial,v_id);

      vgptri = get_vertex_vgrad(v_id);
      do
      {
        sign = 0;
        fe_id = fe_id0;
        do
        { f_id = get_fe_facet(fe_id);
          bf = get_facet_body(f_id);
          if ( equal_id(bf,vgptri->bb_id) )
          { sign = 1; break; }
          else
          { facetedge_id fe_ida = inverse_id(fe_id);
            f_id = get_fe_facet(fe_ida);
            bf = get_facet_body(f_id);
            if ( equal_id(bf,vgptri->bb_id) )
            { sign = -1; break; }
          }
          fe_id = get_next_facet(fe_id);  
        }
        while ( fe_id != fe_id0 );

        if ( sign ) 
          for ( i = 0 ; i < bdry->pcount ; i++ )
            vgptri->grad[i] += partial[i];  
        vgptri = vgptri->chain;
      } while ( vgptri );
    } while ( !equal_id(next_e,start_e) );
  }
}

/*******************************************************************
*
*  Function: string_constr_grad()
*
*  Purpose: Add cell area gradients due to constraint integrals.
*/

void string_constr_grad()
{
  vertex_id v_id;
  edge_id e_id, next_e, start_e;

  FOR_ALL_VERTICES(v_id)
  {
    struct volgrad *vgptri;
    struct constraint *constr;
    facetedge_id fe_id,fe_id0;
    ATTR attr = get_vattr(v_id);

    if ( attr & FIXED ) continue;
    if ( !(attr & CONSTRAINT) ) continue;
    if ( !(attr & BDRY_CONTENT) ) continue;

    start_e = get_vertex_edge(v_id);
    if ( !valid_id(start_e) ) continue;
    e_id = next_e = start_e; 

    do
    { e_id = next_e;
      fe_id0 = get_edge_fe(e_id);
      next_e = get_next_tail_edge(e_id);
      if ( !valid_id(fe_id0) ) continue;
      if ( get_eattr(e_id) & NONCONTENT ) continue;

      vgptri = get_vertex_vgrad(v_id);
      while ( vgptri )
      {
        int sign=0,i,j;
        conmap_t * conmap = get_v_constraint_map(v_id);
        facet_id f_id;
        body_id bf;

        fe_id = fe_id0;
        do
        { f_id = get_fe_facet(fe_id);
          bf = get_facet_body(f_id);
          if ( equal_id(bf,vgptri->bb_id) )
          { sign = 1; break; }
          else
          { facetedge_id fe_ida = inverse_id(fe_id);
            f_id = get_fe_facet(fe_ida);
            bf = get_facet_body(f_id);
            if ( equal_id(bf,vgptri->bb_id) )
             { sign = -1; break; }
          }
          fe_id = get_next_facet(fe_id);  
        }
        while ( fe_id != fe_id0 );
        if ( sign ) 
          for ( j = 1 ; j <= (int)conmap[0] ; j++ )
          { REAL val,derivs[MAXCOORD];
            constr = get_constraint(conmap[j]);
            if ( !(constr->attr & CON_CONTENT) ) continue;
            eval_all(constr->convect[0],get_coord(v_id),SDIM,&val,derivs,v_id);
            for ( i = 0 ; i < SDIM ; i++ )
              vgptri->grad[i] -= sign*derivs[i];  
          }
        vgptri = vgptri->chain;
      }
    }
    while ( !equal_id(next_e,start_e) );
  }
}


/*********************************************************************

                    edge_general method, linear and quadratic

*********************************************************************/

/*********************************************************************
*
* function: edge_general_init()
*
* purpose:  check dimension of edge
*
*/

void edge_general_init(mode,mi)
int mode;
struct method_instance *mi;
{
  if ( web.dimension > 2 ) 
     kb_error(2189,
        "Can only do edge_general_integral if edges are 1 dimensional.\n",
        RECOVERABLE);
}

/*********************************************************************
*
* function: edge_general_value()
*
* purpose:  method value
*
*/

REAL edge_general_value(e_info)
struct qinfo *e_info;
{ int m,k;
  REAL value = 0.0;
  REAL z[2*MAXCOORD+1];  /*  pointers to coord and tangent */
  REAL sign = (get_eattr(e_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;

  if ( web.modeltype == LAGRANGE ) return edge_general_value_lagrange(e_info); 
  for ( m = 0 ; m < gauss1D_num ; m++ )
  { REAL weight = gauss1Dwt[m];
     for ( k = 0 ; k < SDIM ; k++ )
     { z[k] = e_info->gauss_pt[m][k]; 
        z[SDIM+k] = sign*e_info->sides[m][0][k];
     }
     z[2*SDIM] = m; /* kludge for attr interp */
     value += weight*eval(METH_INSTANCE(e_info->method)->expr[0],z,e_info->id,NULL);
  }
  return value;
}

/*********************************************************************
*
* function: edge_general_grad()
*
* purpose:  method gradient
*
*/


REAL edge_general_grad(e_info)
struct qinfo *e_info;
{ int m,j,k;
  REAL value = 0.0;
  REAL val;
  REAL derivs[2*MAXCOORD];
  REAL z[2*MAXCOORD+1];  /*  pointers to coord and tangent */
  REAL sign = (get_eattr(e_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;

  if ( web.modeltype == LAGRANGE ) return edge_general_grad_lagrange(e_info); 
  for ( m = 0 ; m < gauss1D_num ; m++ )
  { REAL weight = gauss1Dwt[m];
     for ( k = 0 ; k < SDIM ; k++ )
     { z[k] = e_info->gauss_pt[m][k]; 
        z[SDIM+k] = sign*e_info->sides[m][0][k];
     }
     z[2*SDIM] = m; /* kludge for attr interp */
     eval_all(METH_INSTANCE(e_info->method)->expr[0],z,2*SDIM,&val,derivs,e_info->id);
     value += weight*val;
     for ( k = 0 ; k < edge_ctrl ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
         e_info->grad[k][j] += weight*(gauss1poly[k][m]*derivs[j]
                                        + gauss1polyd[k][m]*derivs[j+SDIM]);
  }

  return value;
}

/*********************************************************************
*
* function: edge_general_hess()
*
* purpose:  method hessian
*
*/

REAL edge_general_hess(e_info)
struct qinfo *e_info;
{ int m,j,jj,k,kk;
  REAL value = 0.0;
  REAL val;
  REAL derivs[2*MAXCOORD];
  REAL z[2*MAXCOORD+1];  /*  pointers to coord and tangent */
  MAT2D(second,2*MAXCOORD,2*MAXCOORD); /* second derivatives */
  REAL sign = (get_eattr(e_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;

  if ( web.modeltype == LAGRANGE ) return edge_general_hess_lagrange(e_info); 
  for ( m = 0 ; m < gauss1D_num ; m++ )
  { REAL weight = gauss1Dwt[m];
     for ( k = 0 ; k < SDIM ; k++ )
     { z[k] = e_info->gauss_pt[m][k]; 
       z[SDIM+k] = sign*e_info->sides[m][0][k];
     }
     z[2*SDIM] = m; /* kludge for attr interp */
     eval_second(METH_INSTANCE(e_info->method)->expr[0],z,2*SDIM,&val,derivs,second,e_info->id);
     value += weight*val;
     for ( k = 0 ; k < edge_ctrl ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
         e_info->grad[k][j] += weight*(gauss1poly[k][m]*derivs[j]
                                        + gauss1polyd[k][m]*derivs[j+SDIM]);
     for ( k = 0 ; k < edge_ctrl ; k++ )
      for ( kk = 0 ; kk < edge_ctrl ; kk++ )
        for ( j = 0 ; j < SDIM ; j++ )
         for ( jj = 0 ; jj < SDIM ; jj++ )
            e_info->hess[k][kk][j][jj] += weight*
             (gauss1poly[k][m]*gauss1poly[kk][m]*second[j][jj] +
              gauss1polyd[k][m]*gauss1poly[kk][m]*second[j+SDIM][jj] +
              gauss1poly[k][m]*gauss1polyd[kk][m]*second[j][jj+SDIM] +
              gauss1polyd[k][m]*gauss1polyd[kk][m]*second[j+SDIM][jj+SDIM]);
  }
  return value;
}

/*********************************************************************
*
* function: edge_general_value_lagrange()
*
* purpose:  method value
*
*/

REAL edge_general_value_lagrange(e_info)
struct qinfo *e_info;
{ int m,k;
  REAL value = 0.0;
  REAL z[2*MAXCOORD+1];  /*  pointers to coord and tangent */
  REAL sign = (get_eattr(e_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  struct gauss_lag *gl = &gauss_lagrange[1][web.gauss1D_order];
 
  for ( m = 0 ; m < gl->gnumpts ; m++ )  /*  integration point number */
  { for ( k = 0 ; k < SDIM ; k++ )
     { z[k] = e_info->gauss_pt[m][k]; 
       z[SDIM+k] = sign*e_info->sides[m][0][k];
     }
     z[2*SDIM] = m; /* kludge for attr interp */
     value += gl->gausswt[m]*eval(METH_INSTANCE(e_info->method)->expr[0],z,e_info->id,NULL);
  }
  return value;
}

/*********************************************************************
*
* function: edge_general_grad_lagrange()
*
* purpose:  method gradient
*
*/

REAL edge_general_grad_lagrange(e_info)
struct qinfo *e_info;
{ int m,j,k;
  REAL value = 0.0;
  REAL val;
  REAL derivs[2*MAXCOORD];
  REAL z[2*MAXCOORD+1];  /*  pointers to coord and tangent */
  REAL sign = (get_eattr(e_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  struct gauss_lag *gl = &gauss_lagrange[1][web.gauss1D_order];

  for ( m = 0 ; m < gl->gnumpts ; m++ )  /*  integration point number */
  { REAL weight = gl->gausswt[m];
     for ( k = 0 ; k < SDIM ; k++ )
     { z[k] = e_info->gauss_pt[m][k]; 
       z[SDIM+k] = sign*e_info->sides[m][0][k];
     }
     z[2*SDIM] = m; /* kludge for attr interp */
     eval_all(METH_INSTANCE(e_info->method)->expr[0],z,2*SDIM,&val,derivs,e_info->id);
     value += weight*val;
     for ( k = 0 ; k < gl->lagpts ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
         e_info->grad[k][j] += weight*(gl->gpoly[m][k]*derivs[j]
                                   + gl->gpolypart[m][0][k]*derivs[j+SDIM]);
  }
  return value;
}

/*********************************************************************
*
* function: edge_general_hess_lagrange()
*
* purpose:  method hessian
*
*/

REAL edge_general_hess_lagrange(e_info)
struct qinfo *e_info;
{ int m,j,jj,k,kk;
  REAL value = 0.0;
  REAL val;
  REAL derivs[2*MAXCOORD];
  REAL z[2*MAXCOORD+1];  /*  pointers to coord and tangent */
  MAT2D(second,2*MAXCOORD,2*MAXCOORD); /* second derivatives */
  REAL sign = (get_eattr(e_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  struct gauss_lag *gl = &gauss_lagrange[1][web.gauss1D_order];

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL weight = gl->gausswt[m];
     for ( k = 0 ; k < SDIM ; k++ )
     { z[k] = e_info->gauss_pt[m][k]; 
       z[SDIM+k] = sign*e_info->sides[m][0][k];
     }
     z[2*SDIM] = m; /* kludge for attr interp */
     eval_second(METH_INSTANCE(e_info->method)->expr[0],z,2*SDIM,&val,derivs,second,e_info->id);
     value += weight*val;
     for ( k = 0 ; k < gl->lagpts ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
         e_info->grad[k][j] += weight*(gl->gpoly[m][k]*derivs[j]
                                        + gl->gpolypart[m][0][k]*derivs[j+SDIM]);
     for ( k = 0 ; k < gl->lagpts ; k++ )
      for ( kk = 0 ; kk < gl->lagpts ; kk++ )
        for ( j = 0 ; j < SDIM ; j++ )
         for ( jj = 0 ; jj < SDIM ; jj++ )
            e_info->hess[k][kk][j][jj] += weight*
             (gl->gpoly[m][k]*gl->gpoly[m][kk]*second[j][jj] +
              gl->gpolypart[m][0][k]*gl->gpoly[m][kk]*second[j+SDIM][jj] +
              gl->gpoly[m][k]*gl->gpolypart[m][0][kk]*second[j][jj+SDIM] +
              gl->gpolypart[m][0][k]*gl->gpolypart[m][0][kk]*second[j+SDIM][jj+SDIM]);
  }
  return value;
}
