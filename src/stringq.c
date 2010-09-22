/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/************************************************************************
*
*  File: stringq.c
*
*  Purpose:
*  Functions to calculate area, length, and energy according to
*  the QUADRATIC STRING model.
*/

#include "include.h"

/* combining coefficients for coordinates of interpolation pts */
/* initialized in scoeff_init() */
REAL gcombo[EDGE_CTRL][EDGE_INTERP];

/* antisymmetric coefficients for area integral */
/* with diagonal elements to give area over x-axis for torus ease */
REAL scoeff[EDGE_CTRL][EDGE_CTRL] = {
      { 0.5, -2.0/3, 1.0/6 },{2.0/3, 0.0, -2.0/3},{-1.0/6, 2.0/3, -0.5} } ;

/* length gradient coefficients for string forces */
/* sdip[control][eval] - eval is integration index
                                 control is control point index for grad */

REAL sdip[EDGE_CTRL][EDGE_INTERP];

/* integration coefficients for string forces */
/* ssimp[control][eval]                              */
REAL ssimp[EDGE_CTRL][EDGE_INTERP];

/* three-point gaussian integration on [0,2] */
REAL gauss2pt[EDGE_INTERP] = { 0.225403330758517, 1.0, 1.774596669241483 };
REAL gauss2wt[EDGE_INTERP] = { 5.0/9, 8.0/9, 5.0/9 };

REAL interpoly(k,u)
int  k;
REAL u;
{
  switch ( k )
    {
      case  0:  return (1 - u)*(2 - u)/2;
      case  1:  return u*(2 - u);
      case  2:  return u*(u - 1)/2;
    }

  return  0.0;  /* error return */
}

REAL interpolyderiv(k,u)
int  k;
REAL u;
{
  switch ( k )
    {
      case  0:  return u - 1.5;
      case  1:  return 2 - 2*u;
      case  2:  return u - 0.5;
    }

  return  0.0;  /* error return */
}

void scoeff_init()
{
    int i,j;

    for ( i = 0 ; i < EDGE_CTRL ;  i++ )
      for ( j = 0 ; j < EDGE_INTERP ; j++ )
         {
            gcombo[i][j] = interpoly(i,gauss2pt[j]);
            sdip[i][j] = interpolyderiv(i,gauss2pt[j]);
            ssimp[i][j] = gauss2wt[j]*sdip[i][j];
         }
}

/************************************************************************
*
*  Calculates all forces on control points due to edge and
*  accumulates them at each control point.
*  Quadratic version.
*/

void edge_force_q(e_id)
edge_id e_id;
{
  REAL *x[EDGE_CTRL],*force[EDGE_CTRL],etang[EDGE_INTERP][MAXCOORD];
  REAL norm[MAXCOORD];
  REAL len;
  REAL density = get_edge_density(e_id);
  vertex_id hv = get_edge_headv(e_id);
  vertex_id mv = get_edge_midv(e_id);
  vertex_id tv = get_edge_tailv(e_id);
  REAL torusx[MAXCOORD];  /* for unwrapping head in torus */

  int i,j,k;

  x[0] = get_coord(tv);
  x[1] = get_coord(mv);
  x[2] = get_coord(hv);
  force[0] = get_force(tv);
  force[1] = get_force(mv);
  force[2] = get_force(hv);
  if ( web.torus_flag ) 
    { /* unwrap head */
      (*sym_wrap)(x[2],torusx,get_edge_wrap(e_id));
      x[2] = torusx;
    }
  for ( i = 0 ; i < EDGE_INTERP ; i++ )
     {
        for ( j = 0 ; j < SDIM ; j ++ )
          {
             etang[i][j] = 0.0;
             for ( k = 0 ; k < EDGE_CTRL ; k++ )
                etang[i][j] += sdip[k][i]*x[k][j];
          }
        norm[i] = sqrt(SDIM_dot(etang[i],etang[i]));
        if ( norm[i] > 0.0 ) 
         for ( j = 0 ; j < SDIM ; j ++ )
          etang[i][j] /= norm[i];
     }
  for ( i = 0 ; i < EDGE_CTRL ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
        for ( k = 0 ; k < EDGE_INTERP ; k++ )
          force[i][j] -= density*ssimp[i][k]*etang[k][j];

  len = 0.0;
  for ( i = 0 ; i < EDGE_INTERP ; i++ )
     len += gauss2wt[i]*norm[i];

  set_edge_length(e_id,len);
  if ( web.area_norm_flag )
  { add_vertex_star(hv,len);
    add_vertex_star(mv,len);
    add_vertex_star(tv,len);
  }

  /* calculate gravitational forces */
  if ( web.gravflag && !( get_eattr(e_id) & NONCONTENT) )
  { REAL z;
    REAL gdensity = edge_grav_density(e_id);

    if ( gdensity != 0.0 )
    { for ( i = 0 ; i < EDGE_INTERP ; i++ )
      { z = 0.0;
        for ( j = 0 ; j < EDGE_CTRL ; j++ )
          z += gcombo[j][i]*x[j][1];
        for ( j = 0 ; j < EDGE_CTRL ; j++ )
        { force[j][1] -= -gdensity*z*etang[i][0]*gcombo[j][i];
          force[j][0] -= -0.5*gdensity*z*z*sdip[j][i];
        }
      }                  
    }
  }                    

}

/************************************************************************
*
*  Returns energy due to one edge.
*
*  Quadratic version.
*/

void edge_energy_q(e_id)
edge_id e_id;
{
  REAL *x[EDGE_CTRL];
  REAL etang[MAXCOORD];
  vertex_id v[EDGE_CTRL];
  REAL len;
  int i,j,k;
  REAL z;
  REAL density = edge_grav_density(e_id);
  REAL torusx[MAXCOORD];  /* for unwrapping head in torus */
                  
  v[0] = get_edge_tailv(e_id);
  v[1] = get_edge_midv(e_id);
  v[2] = get_edge_headv(e_id);
  for ( i = 0 ; i < EDGE_CTRL ; i++ )
    x[i] = get_coord(v[i]);
             
  if ( web.torus_flag ) 
  { /* unwrap head */
    (*sym_wrap)(x[2],torusx,get_edge_wrap(e_id));
    x[2] = torusx;
  }
     
  /* calculate tangents at integration points and accumulate */
  len = 0.0;
  for ( i = 0 ; i < EDGE_INTERP ; i++ )
  { for ( j = 0 ; j < SDIM ; j ++ )
    { etang[j] = 0.0;
      for ( k = 0 ; k < EDGE_CTRL ; k++ )
        etang[j] += sdip[k][i]*x[k][j];
    }
    len += gauss2wt[i]*sqrt(SDIM_dot(etang,etang));

    /* calculate gravitational energy */
    if ( web.gravflag &&  (density != 0.0) && !(get_eattr(e_id) & NONCONTENT) )
    { for ( i = 0 ; i < EDGE_INTERP ; i++ )
      { z = 0.0;
        for ( j = 0 ; j < EDGE_CTRL ; j++ )
        z += gcombo[j][i]*x[j][1];
        binary_tree_add(web.total_energy_addends,-0.5*density*z*z*etang[0]);
      }                  
    }
  }
  set_edge_length(e_id,len);

  /* following three lines for area normalization option */
  add_vertex_star(v[0],len);
  add_vertex_star(v[1],len);
  add_vertex_star(v[2],len);
     
  binary_tree_add(web.total_energy_addends,len*get_edge_density(e_id));
  if ( web.representation == STRING ) /* don't count triple junction as area */
      web.total_area    += len;
}
 
/**********************************************************************
*
*  Adds contribution of edge to areas of neighboring facets, via
*  Green's theorem.    x-y plane only.
*
*/

void edge_area_q(fe_id)
facetedge_id fe_id;
{
  body_id b_id1,b_id2;
  facet_id f_id1,f_id2;
  REAL *x[EDGE_CTRL];
  REAL area = 0.0;
  vertex_id v[EDGE_CTRL];
  int i,j;
  edge_id e_id = get_fe_edge(fe_id);
                  
  if ( get_eattr(e_id) & NONCONTENT ) return;

  /* be sure have original orientation of edge so torus midpoints
      have same wrap as tail */
  if ( inverted(e_id) )
  { invert(e_id); invert(fe_id); }

  f_id1 = get_fe_facet(fe_id);
  f_id2 = facet_inverse(get_fe_facet(fe_id));
  if ( !valid_id(f_id1) && !valid_id(f_id2) ) return; /* no facets */
  b_id1 = get_facet_body(f_id1);
  b_id2 = get_facet_body(f_id2);

  v[0] = get_edge_tailv(e_id);
  v[1] = get_edge_midv(e_id);
  v[2] = get_edge_headv(e_id);
  for ( i = 0 ; i < EDGE_CTRL ; i++ )
     x[i] = get_coord(v[i]);
             
  if ( web.torus_flag ) 
  { MAT2D(u,EDGE_CTRL,MAXCOORD);
    int wy; /* wraps */
    WRAPTYPE wrap = get_edge_wrap(e_id);
    mat_mul_tr(x,web.inverse_periods,u,EDGE_CTRL,SDIM,SDIM);
    for ( i = 0 ; i < EDGE_CTRL ; i++ )
      for ( j = 0 ; j < EDGE_CTRL ; j ++ )
         area += scoeff[i][j]*u[i][1]*u[j][0];
    wy = WRAPNUM((wrap>>TWRAPBITS) & WRAPMASK);
    area += wy*u[2][0];
    area *= web.torusv;
  }
  else 
   for ( i = 0 ; i < EDGE_CTRL ; i++ )
    for ( j = 0 ; j < EDGE_CTRL ; j ++ )
       area += scoeff[i][j]*x[i][1]*x[j][0];

  /* add to body areas */
  if ( valid_id(f_id1 ) )
     add_facet_area(f_id1, area);
  
  if ( valid_id(b_id1 ) )
     add_body_volume(b_id1, area);
  
  if ( valid_id(b_id2 ) )
     add_body_volume(b_id2, -area);
}

/*****************************************************************
*
*  Function: string_grad_q()
*
*  Purpose:  Calculate area gradients at vertices.
*/

void string_grad_q()
{
  body_id bi_id;  /* identifier for body i */
  body_id bj_id;  /* identifier for body j */
  facetedge_id fe_id;
  REAL *x[EDGE_CTRL];
  int i,j,k;
  vertex_id v[EDGE_CTRL];
  volgrad *vgptr;
  MAT2D(u,EDGE_CTRL,MAXCOORD); /* affine coordinates of vertices */
  MAT2D(g,EDGE_CTRL,MAXCOORD);
  MAT2D(grad,EDGE_CTRL,MAXCOORD);
  int wrap,wrapnum;
  

  FOR_ALL_FACETEDGES(fe_id)
  {
    edge_id e_id = get_fe_edge(fe_id);
                  
    if ( get_eattr(e_id) & NONCONTENT ) continue;

    /* be sure have original orientation of edge so torus midpoints
       have same wrap as tail */
    if ( inverted(e_id) )
    { invert(e_id); invert(fe_id); }

    bi_id = get_facet_body(get_fe_facet(fe_id));
    bj_id = get_facet_body(get_fe_facet(get_next_facet(fe_inverse(fe_id))));
    if ( !(valid_id(bi_id) && (get_battr(bi_id) & (FIXEDVOL|PRESSURE))) 
       && !(valid_id(bj_id) && (get_battr(bj_id) & (FIXEDVOL|PRESSURE))) )
         continue;

    v[0] = get_fe_tailv(fe_id);
    v[1] = get_fe_midv(fe_id);
    v[2] = get_fe_headv(fe_id);
    for ( i = 0 ; i < EDGE_CTRL ; i++ )
      x[i] = get_coord(v[i]);

    if ( web.torus_flag ) 
    { /* get affine coordinates of vertices */
      mat_mul_tr(x,web.inverse_periods,u,edge_ctrl,SDIM,SDIM);
      /* main integral over edge */
      for ( i = 0 ; i < EDGE_CTRL ; i++ )
        for ( j = 0 ; j < EDGE_CTRL ; j++ )
        { REAL v = scoeff[j][i];
          if ( v == 0.0 ) continue;
          g[i][0] += v*u[j][1];
          g[j][1] += v*u[i][0];
        }

      /* wrap correction */
      wrap = (get_edge_wrap(e_id)>>TWRAPBITS) & WRAPMASK;
      wrapnum = WRAPNUM(wrap);
      g[2][0] += wrapnum;

      for ( k = 0 ; k < EDGE_CTRL ; k++ )
       for ( j = 0 ; j < SDIM ; j++ )
        g[k][j] *= web.torusv;
      mat_mult(g,web.inverse_periods,grad,EDGE_CTRL,SDIM,SDIM);
    }
    else
    { for ( i = 0 ; i < EDGE_CTRL ; i++ )
      { grad[i][0] = grad[i][1] = 0.0;
        for ( k = 0 ; k < EDGE_CTRL ; k++ )
        { grad[i][0] +=  scoeff[k][i]*x[k][1];
          grad[i][1] +=  scoeff[i][k]*x[k][0];
        }
      }
    }

    if ( valid_id(bi_id) && (get_battr(bi_id) & (FIXEDVOL|PRESSURE)) )
    { for ( i = 0 ; i < EDGE_CTRL ; i++ )
      { vgptr = get_bv_new_vgrad(get_body_fixnum(bi_id),v[i]);
        vgptr->bb_id = bi_id;
        vgptr->grad[0] +=  grad[i][0];
        vgptr->grad[1] +=  grad[i][1];        
      }
    }

    if ( valid_id(bj_id) && !equal_id(bj_id,bi_id) && 
                          (get_battr(bj_id) & (FIXEDVOL|PRESSURE)) )
    { for ( i = 0 ; i < EDGE_CTRL ; i++ )
      { 
        vgptr = get_bv_new_vgrad(get_body_fixnum(bj_id),v[i]);
        vgptr->bb_id = bj_id;
        vgptr->grad[0] -=  grad[i][0];
        vgptr->grad[1] -=  grad[i][1];
      }
    }
  }
}
