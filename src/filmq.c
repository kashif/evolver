/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/************************************************************************
*
*     file:        filmq.c
*
*     contents:  Functions for calculating energy, volume,
*                    and their gradients for the QUADRATIC
*                    SOAPFILM model.  Also utility functions
*                    for quadratic interpolation and Gaussian
*                    integration.
*/

/************************************************************************
*
*  Functions to calculate area, volume, and energy according to
*  the tessellation model.
* 
*  Quadratic patch version, using n-point facet cubature of integral_order.
*/

#include "include.h"

/* volume coefficients, used only for quadratic volume */
REAL vcoeff[FACET_CTRL][FACET_CTRL][FACET_CTRL];

/* interpolation polynomial second partials */
REAL poly2partial[FACET_CTRL][2][2] = { { {1.0, 1.0}, {1.0, 1.0} },
     {{-2.0, -1.0}, {-1.0, 0.0} },{ {1.0,0.0},{0.0,0.0} }, 
     { {0.0,1.0},{1.0,0.0} }, { {0.0,0.0},{0.0,1.0} },
     { {0.0,-1.0},{-1.0,2.0} } };

/************************************************************************
*
*  Calculates all forces on control points due to facet and
*  accumulates them at each control point.
*  Quadratic version.
*/

void facet_force_q(f_id)
facet_id f_id;
{
  vertex_id v_id[FACET_CTRL];
  MAT2D(x,FACET_CTRL,MAXCOORD);
  REAL normal[MAXCOORD];
  int i,j,k,n;
  REAL norm,summer[2][MAXCOORD];
  facetedge_id fe_id;
  REAL density = get_facet_density(f_id);
  REAL area = 0.0;
  REAL gdensity = 0.0;
  REAL z;
  body_id b_id;
  REAL forces[FACET_CTRL][MAXCOORD];  /* total forces from this facet */
  REAL *forceptr[FACET_CTRL];    /* pointers to forces */
  WRAPTYPE wraps[FACET_CTRL];
  MAT2D(tang,MAXCOORD,MAXCOORD);

  memset((char*)forces,0,sizeof(forces));  /* set to 0 */
  /* get control points */
  fe_id = get_facet_fe(f_id);
  for ( i = 0, j = 0 ; i < FACET_EDGES ; i++ )
  { v_id[j++] = get_fe_tailv(fe_id);
    v_id[j++] = get_fe_midv(fe_id);
    fe_id = get_next_edge(fe_id);
  }
     
  for ( j = 0 ; j < FACET_CTRL ; j++ )
    forceptr[j] = forces[j];

  get_facet_verts(f_id,x,wraps);

  if ( web.metric_flag )
  { simplex_force_metric(v_id,x,density,forceptr);
    goto cumforces;  /* assume no gravity */
  }

  if ( web.gravflag && !(get_fattr(f_id) & NONCONTENT) )
  { b_id = get_facet_body(f_id);
    if ( valid_id(b_id) )
       gdensity += get_body_density(b_id)*web.grav_const;
    b_id = get_facet_body(facet_inverse(f_id));
    if ( valid_id(b_id) )
       gdensity -= get_body_density(b_id)*web.grav_const;
  }
  for ( i = 0 ; i < gauss2D_num ; i++ )  /*  integration point number */
  { /* calculate tangents and normals */ 
    mat_mult(gpolypartial[i],x,tang,web.dimension,ctrl_num,SDIM);
    cross_prod(tang[0],tang[1],normal);
    norm = sqrt(SDIM_dot(normal,normal));
    for ( n = 0 ; n < 2 ; n++ ) /* parameter number */
      cross_prod(normal,tang[n],summer[n]);

    area += density*gauss2Dwt[i]/2*norm;
    for ( k = 0 ; k < FACET_CTRL ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
        forces[k][j] -= density*gauss2Dwt[i]/2/norm*
           (gpolypartial[i][1][k]*summer[0][j] 
               - gpolypartial[i][0][k]*summer[1][j]);

    /* add gravitational force */
    if ( web.gravflag && !(get_fattr(f_id) & NONCONTENT) )
    { z = 0.0;
      for ( j = 0 ; j < FACET_CTRL ; j++ )
         z += gpoly[i][j]*x[j][2];
      for ( j = 0 ; j < FACET_CTRL ; j++ )
      { forces[j][0] -= 0.5*z*z*gauss2Dwt[i]/2*gdensity
                          *(gpolypartial[i][0][j]*tang[1][1] -
                                tang[0][1]*gpolypartial[i][1][j]);
        forces[j][1] -= 0.5*z*z*gauss2Dwt[i]/2*gdensity
                          *(tang[0][0]*gpolypartial[i][1][j] -
                                gpolypartial[i][0][j]*tang[1][0]);
        forces[j][2] -= z*gpoly[i][j]*gauss2Dwt[i]/2*gdensity
                          *(tang[0][0]*tang[1][1] - tang[0][1]*tang[1][0]);
      } 
    }
  }


cumforces:
  /* add to totals, unwrapping if necessary */
  for ( i = 0 ; i < FACET_CTRL ; i++ )  /* vertex loop */
  { REAL *f; 
    REAL wforce[MAXCOORD];  /* unwrapped forces */

    f= get_force(v_id[i]);
    if ( web.symmetry_flag )
    { (*sym_form_pullback)(get_coord(v_id[i]),wforce,forces[i],wraps[i]);
      for ( j = 0 ; j < SDIM ; j++ )
        f[j] += wforce[j];
    }
    else
      for ( j = 0 ; j < SDIM ; j++ )
        f[j] += forces[i][j];
  }

  set_facet_area(f_id,area);
} /* end facet_force_q() */

/*********************************************************************
*
* Function: facet_energy_q()
*
* Purpose: Returns energy due to facet.  Quadratic version.
*/

void facet_energy_q(f_id,mode)
facet_id f_id;
int mode; /* AREA_ONLY or ALL_ENERGY */
{
  REAL energy = 0.0;
  body_id b_id;
  MAT2D(x,FACET_CTRL,MAXCOORD);
  vertex_id v_id[FACET_CTRL];
  REAL normal[MAXCOORD];
  int i,j;
  REAL norm;
  facetedge_id fe_id;
  REAL u = 0.0;  /* gravitational integral */
  REAL z;
  WRAPTYPE wraps[FACET_CTRL];
  MAT2D(tang,MAXCOORD,MAXCOORD);

  /* get control points */
  get_facet_verts(f_id,x,wraps);

  if ( web.metric_flag )
  { energy = simplex_energy_metric(v_id,x);
    goto skip_from_metric;
  }

  for ( i = 0 ; i < gauss2D_num ; i++ )  /*  integration point number */
  {
    /* calculate tangents and normals */ 
    mat_mult(gpolypartial[i],x,tang,web.dimension,ctrl_num,SDIM);
    cross_prod(tang[0],tang[1],normal);
    norm = sqrt(SDIM_dot(normal,normal));
    energy += gauss2Dwt[i]*norm/2;

    if ( web.gravflag && !(get_fattr(f_id) & NONCONTENT) )
    { z = 0.0;
      for ( j = 0 ; j < FACET_CTRL ; j++ )
        z += gpoly[i][j]*x[j][2];
      u += 0.5*z*z*gauss2Dwt[i]/2*(tang[0][0]*tang[1][1] -
                                                    tang[0][1]*tang[1][0]);
    }
  }

skip_from_metric:
  set_facet_area(f_id,energy);

  if ( mode == AREA_ONLY ) return;

  binary_tree_add(web.total_area_addends,energy);
     
  /* apportion area to vertices and midpoints to scale motion */
  /* Note apportionment ratios depend on equivalent test        */
  /* function used to get curvature; here linear used            */
  { fe_id = get_facet_fe(f_id);
    for ( i = 0 ; i < FACET_EDGES ; i++, fe_id = get_next_edge(fe_id) )
    { vertex_id vv_id;
      edge_id e_id = get_fe_edge(fe_id);
      vv_id = get_edge_headv(e_id);
      add_vertex_star(vv_id,energy);
      vv_id = get_edge_midv(e_id);
      add_vertex_star(vv_id,energy);
    }
  }

  /* calculate surface energy */
  energy *= get_facet_density(f_id);
     
  /* add gravitational energy, vector potential z*z/2*k  */
  if ( web.gravflag && !(get_fattr(f_id) & NONCONTENT) )
  { b_id = get_facet_body(f_id);
    if ( valid_id(b_id) )
       energy += u*get_body_density(b_id)*web.grav_const;
    b_id = get_facet_body(facet_inverse(f_id));
    if ( valid_id(b_id) )
       energy -= u*get_body_density(b_id)*web.grav_const;
  }

  web.total_energy = web.total_energy + energy;

}  /* end facet_energy_q() */

/**********************************************************************
*
*  Function: facet_volume_q()
*
*  Purpose: Find triangle's contribution to volumes of neighboring bodies.
*/

void facet_volume_q(f_id)
facet_id f_id;
{ 
  body_id b_id0,b_id1;
  REAL vol = 0.0;
  REAL *x[FACET_CTRL];
  vertex_id v_id[FACET_CTRL];
  int i,j,k;
  facetedge_id fe_id;

  if ( get_fattr(f_id) & NONCONTENT ) return;

  b_id0 = get_facet_body(f_id);
  b_id1 = get_facet_body(facet_inverse(f_id));
     
  if ( !valid_id(b_id0) && !valid_id(b_id1) ) return;

  if ( web.symmetric_content && !everything_quantities_flag )
     kb_error(1038,
      "Do convert_to_quantities for symmetric content for quadratic model.\n",
         RECOVERABLE);


  /* get control points */
  fe_id = get_facet_fe(f_id);
  for ( i = 0, j = 0 ; i < FACET_EDGES ; i++ )
  { v_id[j++] = get_fe_tailv(fe_id);
    v_id[j++] = get_fe_midv(fe_id);
    fe_id = get_next_edge(fe_id);
  }
  for ( j = 0 ; j < FACET_CTRL ; j++ )
     x[j] = get_coord(v_id[j]);

  /* volume, integral of z dx dy */
  for ( i = 0 ; i < FACET_CTRL ; i++ )
     for ( j = 0 ; j < FACET_CTRL ; j++ )
        for ( k = 0 ; k < FACET_CTRL ; k++ )
          vol += vcoeff[i][j][k]*x[i][2]*(x[j][0]*x[k][1]-x[j][1]*x[k][0]);

  /* add to body volumes */
  if ( valid_id(b_id0) )
     add_body_volume(b_id0,vol);
  if ( valid_id(b_id1) )
     add_body_volume(b_id1,-vol);
} /* end facet_volume_q() */

/****************************************************************
*
*  Function:  film_grad_q()
*
*  Purpose:    Calculate volume gradients in SOAPFILM quadratic
*                  model.
*/

void film_grad_q()
{
  body_id bi_id;  /* identifier for body i */
  body_id bj_id;  /* identifier for body j */
  facetedge_id fe_id;
  facet_id f_id;
  REAL g[MAXCOORD];
  vertex_id v_id[FACET_CTRL];
  REAL *x[FACET_CTRL];
  volgrad *vgptr;
  int i,j,k,n;

  FOR_ALL_FACETS(f_id)
  { if ( get_fattr(f_id) & NONCONTENT ) continue;
    bi_id = get_facet_body(f_id);
    bj_id = get_facet_body(facet_inverse(f_id));
                    
    /* get control points */
    fe_id = get_facet_fe(f_id);
    for ( i = 0, j = 0 ; i < FACET_EDGES ; i++ )
    { v_id[j++] = get_fe_tailv(fe_id);
      v_id[j++] = get_fe_midv(fe_id);
      fe_id = get_next_edge(fe_id);
    }
    for ( j = 0 ; j < FACET_CTRL ; j++ )
      x[j] = get_coord(v_id[j]);

    for ( i = 0 ; i < FACET_CTRL ; i++ )
    { g[0] = g[1] = g[2] = 0.0;
      for ( j = 0 ; j < FACET_CTRL ; j++ ) 
        for ( k = 0 ; k < FACET_CTRL  ; k++ )
        { g[0] += vcoeff[k][i][j]*x[j][1]*x[k][2] 
                  - vcoeff[j][k][i]*x[j][2]*x[k][1];
          g[1] += vcoeff[j][k][i]*x[j][2]*x[k][0] 
                  - vcoeff[k][i][j]*x[j][0]*x[k][2];
          g[2] += vcoeff[i][j][k]*(x[j][0]*x[k][1] - x[j][1]*x[k][0]);
        }

      if ( valid_id(bi_id) && (get_battr(bi_id) & (PRESSURE|FIXEDVOL)) )
      { vgptr = get_bv_new_vgrad(get_body_fixnum(bi_id),v_id[i]);
        vgptr->bb_id = bi_id;
        for ( n = 0 ; n < SDIM ; n++ )
          vgptr->grad[n] += g[n];
      }

      if ( valid_id(bj_id) && (get_battr(bj_id) & (FIXEDVOL|PRESSURE)) )
      { vgptr = get_bv_new_vgrad(get_body_fixnum(bj_id),v_id[i]);
        vgptr->bb_id = bj_id;
        for ( n = 0 ; n < SDIM ; n++ )
          vgptr->grad[n] -= g[n];
      }
    }
  }  
}  /* end film_grad_q() */

/*************************************************************************
*
*  Function: triangle_integral()
*
*  Purpose: Used by vcoeff_init() to calculate volume coefficients.
*
*/
REAL triangle_integral ARGS(( REAL(*)(REAL,REAL)));
REAL triangle_integral(f)  /* on side 2 triangle, 5th degree */
#ifdef NOPROTO
REAL (*f)();
#else
REAL (*f)(REAL,REAL);
#endif
{
  int i;
  REAL sum = 0.0;

  for ( i = 0 ; i < 7 ; i++ )
     sum += gauss2Dwt5[i]*(*f)(2*gauss2Dpt5[i][0],2*gauss2Dpt5[i][1]);

  return 2*sum;
}

/* Interpolation polynomials for 6 point interpolation on triangle.

    Triangle is: u > 0, v > 0, u + v < 2.

    Point indexing (counterclockwise):
        4
        5  3
        0  1  2

*/

/**********************************************************************
*
*  Function: intpoly6()
*
*  Purpose: interpolation polynomial evaluation
*/

REAL intpoly6(k,u,v)
int k;       /* polynomial number */
REAL u,v;    /* evaluation point */
{
  switch ( k )
    {
      case 0: return (1 - (u + v))*(2 - (u + v))/2.0;
      case 1: return u*(2 - (u + v));
      case 2: return u*(u - 1)/2.0;
      case 3: return u*v;
      case 4: return v*(v - 1)/2.0;
      case 5: return v*(2 - (u + v));
    }

  /* bad index */
  return 0.0;
}

/************************************************************************
*
* Function: inpoly6part()
*
* Purpose: partials of interpolation polynomials 
*/

REAL intpoly6part(k,i,u,v)
int k;  /* which polynomial (control point index) */
int i;  /* partial number: 0 for u, 1 for v */
REAL u,v;    /* evaluation point */
{
  if ( i == 0 )
    switch ( k )
     {
        case 0: return u + v - 1.5;
        case 1: return 2 - 2*u - v;
        case 2: return u - 0.5;
        case 3: return v;
        case 4: return 0.0;
        case 5: return -v;
     }
  else if ( i == 1 )
    switch ( k )
     {
        case 0: return u + v - 1.5;
        case 1: return -u;
        case 2: return 0.0;
        case 3: return u;
        case 4: return v - 0.5;
        case 5: return 2 - u - 2*v;
     }

  /* bad index */
  return 0.0;
}


/**********************************************************************
*
* Function: vcoeff_init()
*
* Purpose: initializing coefficients for quadratic volume calculations.
*/

/* integrand function */
static int al,be,ga;
REAL vintzf(u,v)
REAL u,v;
{ 
  return 
      intpoly6(al,u,v)*intpoly6part(be,0,u,v)*intpoly6part(ga,1,u,v);
}

void vcoeff_init()
{
  REAL t;

  for ( al = 0 ; al < FACET_CTRL ; al++ )
    for ( be = 0; be < FACET_CTRL ; be++ )
      for ( ga = 0 ; ga < FACET_CTRL ; ga++ )
      { t = triangle_integral(vintzf);
        /* round to nearest 90th */
        t = (int)(t*90.001)/90.0;
        vcoeff[al][be][ga] = t;
      }

}


/*********************************************************************
**********************************************************************
                     Quadratic film quantities
**********************************************************************
**********************************************************************/

/*********************************************************************
                     Film area quantity

    Uses Gaussian integration.  Not known to be upper bound on area.

**********************************************************************/

/*********************************************************************
*
*  Function: q_facet_tension_q()
*
*  Purpose: Returns energy due to facet.
*  Quadratic version.
*/

REAL q_facet_tension_q(f_info)
struct qinfo *f_info;
{
  REAL value = 0.0;
  int m;
  REAL det;
  REAL st,ss,tt;
  REAL **tang;
     
  for ( m = 0 ; m < gauss2D_num ; m++ )  /*  integration point number */
  { tang = f_info->sides[m];
    ss = SDIM_dot(tang[0],tang[0]);
    st = SDIM_dot(tang[0],tang[1]);
    tt = SDIM_dot(tang[1],tang[1]);
    det = ss*tt - st*st;
    value += gauss2Dwt[m]*sqrt(det);
  }

  value /= 2; /* triangle factor */
  if ( quantities_only_flag )
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

} /* end q_facet_tension_q() */

/*********************************************************************
*
*  Function: q_facet_tension_grad_q()
*
*  Returns gradient and energy due to facet.
*  Quadratic version.
*/

REAL q_facet_tension_q_grad(f_info)
struct qinfo *f_info;
{
  REAL value = 0.0;
  int m,j,k;
  REAL det,norm;
  REAL st,ss,tt;
  REAL density,fudge;

  if ( METH_INSTANCE(f_info->method)->flags & USE_DENSITY )
      density = get_facet_density(f_info->id);
  else density = 1.0;

  for ( k = 0 ; k < FACET_CTRL ; k++ )
     for ( j = 0 ; j < SDIM ; j++ )
        f_info->grad[k][j] = 0.0;

  for ( m = 0 ; m < gauss2D_num ; m++ )  /*  integration point number */
  { REAL **tang = f_info->sides[m];
     /* calculate tangents and det */ 
     ss = SDIM_dot(tang[0],tang[0]);
     st = SDIM_dot(tang[0],tang[1]);
     tt = SDIM_dot(tang[1],tang[1]);
     det = ss*tt - st*st;
     norm = sqrt(det);
     value += gauss2Dwt[m]*norm/2;
     /* gradients */
     fudge = density*gauss2Dwt[m]/norm/2;
     for ( k = 0 ; k < FACET_CTRL ; k++ )
        for ( j = 0 ; j < SDIM ; j++ )
          f_info->grad[k][j] += fudge*
             (  tang[0][j]*gpolypartial[m][0][k]*tt
              - tang[0][j]*gpolypartial[m][1][k]*st
              - tang[1][j]*gpolypartial[m][0][k]*st
              + tang[1][j]*gpolypartial[m][1][k]*ss);
  }

  return density*value;

} /* end q_facet_tnesion_grad_q() */

/*********************************************************************
*
*  Function: q_facet_tension_q_hess()
*
*  Purpose: Returns hessian, gradient and energy due to facet.
*  Quadratic version.
*/

REAL q_facet_tension_q_hess(f_info)
struct qinfo *f_info;
{
  REAL value = 0.0;
  int m,j,jj,k,kk,p,q;
  REAL det,norm;
  REAL st,ss,tt;
  REAL tr[FACET_CTRL][MAXCOORD];  /* traces */
  MAT2D(at,2,2);
  MAT2D(ainv,2,2);
  MAT4D(aa,FACET_CTRL,MAXCOORD,2,2);
  REAL density,fudge;

  if ( METH_INSTANCE(f_info->method)->flags & USE_DENSITY )
      density = get_facet_density(f_info->id);
  else density = 1.0;

  for ( m = 0 ; m < gauss2D_num ; m++ )  /*  integration point number */
  { REAL **tang = f_info->sides[m];
     /* calculate tangents and det */ 
     ss = SDIM_dot(tang[0],tang[0]);
     st = SDIM_dot(tang[0],tang[1]);
     tt = SDIM_dot(tang[1],tang[1]);
     det = ss*tt - st*st;
     if ( det <= 0.0 ) continue;
     norm = sqrt(det);
     value += gauss2Dwt[m]*norm/2;

     ainv[0][0] = tt/det;
     ainv[0][1] = ainv[1][0] = -st/det;
     ainv[1][1] = ss/det;

     /* gradients */
     fudge = density*gauss2Dwt[m]*norm/4;
     for ( k = 0 ; k < FACET_CTRL ; k++ )
        for ( j = 0 ; j < SDIM ; j++ )
        { for ( p = 0 ; p < 2 ; p++ )
            for ( q = 0 ; q < 2; q++ )
              at[p][q] = tang[p][j]*gpolypartial[m][q][k] +
                  tang[q][j]*gpolypartial[m][p][k];
          mat_mult(at,ainv,aa[k][j],2,2,2);

          tr[k][j] = aa[k][j][0][0] + aa[k][j][1][1];
          f_info->grad[k][j] += fudge*tr[k][j];
        }

     /* hessians */
     for ( k = 0 ; k < FACET_CTRL ; k++ )
      for ( kk = 0 ; kk < FACET_CTRL ; kk++ )
        for ( j = 0 ; j < SDIM ; j++ )
         for ( jj = 0 ; jj < SDIM ; jj++ )
             f_info->hess[k][kk][j][jj] += fudge*
                ( 0.5*tr[k][j]*tr[kk][jj]  
                 - (aa[k][j][0][0]*aa[kk][jj][0][0] +
                      aa[k][j][0][1]*aa[kk][jj][1][0] +
                      aa[k][j][1][0]*aa[kk][jj][0][1] +
                      aa[k][j][1][1]*aa[kk][jj][1][1]) 
                  + ( (j==jj)? 
                  2*(gpolypartial[m][0][k]*gpolypartial[m][0][kk]*ainv[0][0]
                    + gpolypartial[m][0][k]*gpolypartial[m][1][kk]*ainv[0][1]
                    + gpolypartial[m][1][k]*gpolypartial[m][0][kk]*ainv[1][0]
                    + gpolypartial[m][1][k]*gpolypartial[m][1][kk]*ainv[1][1])
                    : 0.0)
                );
  } /* end gauss pt loop */

  return density*value;

} /* end q_facet_tension_q_hess() */


/*********************************************************************
          Upper bound on facet quadratic area 

 Good way is to do sqrt(gaussian int of det) which gives upper
    bound by Cauchy-Schwarz inequality for 7-pt integration, since
    det is order 4 polynomial.
***********************************************************************/

/*********************************************************************
*
*  Returns energy due to facet.
*  Quadratic version.
*/

REAL q_facet_tension_uq(f_info)
struct qinfo *f_info;
{
  REAL value = 0.0;
  int m;
  REAL det;
  REAL st,ss,tt;

  for ( m = 0 ; m < gauss2D_num ; m++ )  /*  integration point number */
  { REAL **tang = f_info->sides[m];
    ss = SDIM_dot(tang[0],tang[0]);
    st = SDIM_dot(tang[0],tang[1]);
    tt = SDIM_dot(tang[1],tang[1]);
    det = ss*tt - st*st;
    value += gauss2Dwt[m]*det;
  }
  value = sqrt(value)/2;
  if ( quantities_only_flag )
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
*  Quadratic version.
*/

REAL q_facet_tension_uq_grad(f_info)
struct qinfo *f_info;
{
  REAL value = 0.0;
  int m,j,k;
  REAL det;
  REAL norm;
  REAL st,ss,tt;
  REAL detgrad[FACET_CTRL][MAXCOORD];
  REAL density = 1.0;

  if ( METH_INSTANCE(f_info->method)->flags & USE_DENSITY )
      density = get_facet_density(f_info->id);

  for ( k = 0 ; k < FACET_CTRL ; k++ )
     for ( j = 0 ; j < SDIM ; j++ )
        detgrad[k][j] = 0.0;

  for ( m = 0 ; m < gauss2D_num ; m++ )  /*  integration point number */
  { REAL **tang = f_info->sides[m]; 
     /* calculate tangents and det */ 
     ss = SDIM_dot(tang[0],tang[0]);
     st = SDIM_dot(tang[0],tang[1]);
     tt = SDIM_dot(tang[1],tang[1]);
     det = ss*tt - st*st;
     value += gauss2Dwt[m]*det;

     /* gradients */
     for ( k = 0 ; k < FACET_CTRL ; k++ )
        for ( j = 0 ; j < SDIM ; j++ )
          detgrad[k][j] += gauss2Dwt[m]*
             (  tang[0][j]*gpolypartial[m][0][k]*tt
              - tang[0][j]*gpolypartial[m][1][k]*st
              - tang[1][j]*gpolypartial[m][0][k]*st
              + tang[1][j]*gpolypartial[m][1][k]*ss);
  }

  norm = density/sqrt(value*4);
  for ( k = 0 ; k < FACET_CTRL ; k++ )
     for ( j = 0 ; j < SDIM ; j++ )
        f_info->grad[k][j] = norm*detgrad[k][j];

  return density*sqrt(value)/2;

}

/*********************************************************************
*
*  Returns hessian, gradient and energy due to facet.
*  Quadratic version.
*/

REAL q_facet_tension_uq_hess(f_info)
struct qinfo *f_info;
{
  REAL value = 0.0;
  int m,j,jj,k,kk,r,q;
  REAL det,norm;
  REAL st,ss,tt;
  MAT2D(at,2,2);
  MAT2D(ainv,2,2);
  MAT4D(aa,FACET_CTRL,MAXCOORD,2,2);
  REAL detgrad[FACET_CTRL][MAXCOORD];
  REAL dethess[FACET_CTRL][FACET_CTRL][MAXCOORD][MAXCOORD];
  REAL density = 1.0;

  if ( METH_INSTANCE(f_info->method)->flags & USE_DENSITY )
      density = get_facet_density(f_info->id);
     
  for ( k = 0 ; k < FACET_CTRL ; k++ )
     for ( j = 0 ; j < SDIM ; j++ )
        detgrad[k][j] = 0.0;

  for ( k = 0 ; k < FACET_CTRL ; k++ )
     for ( kk = 0 ; kk < FACET_CTRL ; kk++ )
        for ( j = 0 ; j < SDIM ; j++ )
         for ( jj = 0 ; jj < SDIM ; jj++ )
            dethess[k][kk][j][jj] = 0.0;

  for ( m = 0 ; m < gauss2D_num ; m++ )  /*  integration point number */
  { REAL **p = gpolypartial[m];
     REAL **tang = f_info->sides[m];
     /* calculate tangents and det */ 
     ss = SDIM_dot(tang[0],tang[0]);
     st = SDIM_dot(tang[0],tang[1]);
     tt = SDIM_dot(tang[1],tang[1]);
     det = ss*tt - st*st;
     value += gauss2Dwt[m]*det;

     ainv[0][0] = tt/det;
     ainv[0][1] = ainv[1][0] = -st/det;
     ainv[1][1] = ss/det;

     /* gradients */
     for ( k = 0 ; k < FACET_CTRL ; k++ )
        for ( j = 0 ; j < SDIM ; j++ )
        { for ( r = 0 ; r < 2 ; r++ )
            for ( q = 0 ; q < 2; q++ )
              at[r][q] = tang[r][j]*gpolypartial[m][q][k] +
                  tang[q][j]*gpolypartial[m][r][k];
          mat_mult(at,ainv,aa[k][j],2,2,2);

          detgrad[k][j] += gauss2Dwt[m]
             *(p[0][k]*tang[0][j]*tt +  ss*p[1][k]*tang[1][j]
                  - st*(p[0][k]*tang[1][j] + tang[0][j]*p[1][k]));
        }

     /* hessians */
     for ( k = 0 ; k < FACET_CTRL ; k++ )
      for ( kk = 0 ; kk < FACET_CTRL ; kk++ )
        for ( j = 0 ; j < SDIM ; j++ )
         for ( jj = 0 ; jj < SDIM ; jj++ )
         {  dethess[k][kk][j][jj] += gauss2Dwt[m]*
            ( 4*p[0][k]*tang[0][j]*tang[1][jj]*p[1][kk]
             + 4*tang[0][jj]*p[0][kk]*p[1][k]*tang[1][j]
             - 2*(p[0][kk]*tang[1][jj]+tang[0][jj]*p[1][kk])
                 *(p[0][k]*tang[1][j]+tang[0][j]*p[1][k]));
             if ( j == jj )
              dethess[k][kk][j][jj] += gauss2Dwt[m]*
              ( 2*p[0][k]*p[0][kk]*tt + 2*ss*p[1][k]*p[1][kk]
                 - 2*st*(p[0][k]*p[1][kk] + p[0][kk]*p[1][k]) );
          }
  } /* end gauss pt loop */

  /* gradients */
  norm = density/sqrt(value*4);
  for ( k = 0 ; k < FACET_CTRL ; k++ )
     for ( j = 0 ; j < SDIM ; j++ )
        f_info->grad[k][j] = norm*detgrad[k][j];

  /* hessians */
  for ( k = 0 ; k < FACET_CTRL ; k++ )
    for ( kk = 0 ; kk < FACET_CTRL ; kk++ )
      for ( j = 0 ; j < SDIM ; j++ )
         for ( jj = 0 ; jj < SDIM ; jj++ )
             f_info->hess[k][kk][j][jj] = norm*
             (0.5*dethess[k][kk][j][jj] - detgrad[k][j]*detgrad[kk][jj]/value);

  return density*sqrt(value)/2; 

}

/**********************************************************************
                    Quadratic volume quantity
**********************************************************************/

/**********************************************************************
*
*  function: q_facet_volume_q()
*
*  purpose: value of volume integral on quadratic facet
*/

REAL q_facet_volume_q(f_info)
struct qinfo *f_info;
{ 
  REAL vol = 0.0;
  int m,i;

  /* volume, integral of z dx dy */
  for ( m = 0 ; m < gauss2D_num ; m++ )
  { REAL **t = f_info->sides[m];
     REAL z;
     for ( z = 0., i = 0 ; i < FACET_CTRL ; i++ ) 
        z += gpoly[m][i]*f_info->x[i][2];
     vol += gauss2Dwt[m]*z*(t[0][0]*t[1][1]-t[0][1]*t[1][0]);
  }
  return vol/2;

} /* end q_facet_volume_q() */

/**********************************************************************
*
*  function: q_facet_volume_q_grad()
*
*  purpose: value and gradient of volume integral on quadratic facet
*/

REAL q_facet_volume_q_grad(f_info)
struct qinfo *f_info;
{ 
  REAL vol = 0.0;
  int i,j,k;
  REAL **x = f_info->x;
  REAL v;

  /* volume, integral of z dx dy */
  for ( i = 0 ; i < FACET_CTRL ; i++ )
     for ( j = 0 ; j < FACET_CTRL ; j++ )
        for ( k = 0 ; k < FACET_CTRL ; k++ )
        { v = vcoeff[i][j][k];
          if ( v == 0.0 ) continue;
          vol += v*x[i][2]*(x[j][0]*x[k][1]-x[j][1]*x[k][0]);
        }

  /* gradients */
  for ( k = 0 ; k < FACET_CTRL ; k++ )
     for ( j = 0 ; j < SDIM ; j++ )
        f_info->grad[k][j] = 0.0;

  for ( i = 0 ; i < FACET_CTRL ; i++ )
    for ( j = 0 ; j < FACET_CTRL ; j++ )
     for ( k = 0 ; k < FACET_CTRL ; k++ )
     { v = vcoeff[i][j][k];
        if ( v == 0.0 ) continue;
        f_info->grad[i][2] += v*(x[j][0]*x[k][1]-x[j][1]*x[k][0]);
        f_info->grad[j][0] += v*x[i][2]*x[k][1];
        f_info->grad[k][1] += v*x[i][2]*x[j][0];
        f_info->grad[j][1] -= v*x[i][2]*x[k][0];
        f_info->grad[k][0] -= v*x[i][2]*x[j][1];
     }
  return vol;
} /* end q_facet_volume_grad_q() */

/**********************************************************************
*
*  function: q_facet_volume_q_hess()
*
*  purpose: hessian, value, and gradof volume integral on quadratic facet
*/

REAL q_facet_volume_q_hess(f_info)
struct qinfo *f_info;
{ 
  REAL vol = 0.0;
  int i,j,k;
  REAL v;
  REAL **x;
  REAL ****h;

  x = f_info->x;

  /* volume, integral of z dx dy */
  for ( i = 0 ; i < FACET_CTRL ; i++ )
     for ( j = 0 ; j < FACET_CTRL ; j++ )
        for ( k = 0 ; k < FACET_CTRL ; k++ )
          vol += vcoeff[i][j][k]*x[i][2]*(x[j][0]*x[k][1]-x[j][1]*x[k][0]);

  /* gradients */
  for ( k = 0 ; k < FACET_CTRL ; k++ )
     for ( j = 0 ; j < SDIM ; j++ )
        f_info->grad[k][j] = 0.0;

  for ( i = 0 ; i < FACET_CTRL ; i++ )
    for ( j = 0 ; j < FACET_CTRL ; j++ )
     for ( k = 0 ; k < FACET_CTRL ; k++ )
     { v = vcoeff[i][j][k];
        f_info->grad[i][2] += v*(x[j][0]*x[k][1]-x[j][1]*x[k][0]);
        f_info->grad[j][0] += v*x[i][2]*x[k][1];
        f_info->grad[k][1] += v*x[i][2]*x[j][0];
        f_info->grad[j][1] -= v*x[i][2]*x[k][0];
        f_info->grad[k][0] -= v*x[i][2]*x[j][1];
     }

  /* hessian */
  h = f_info->hess;
  for ( i = 0 ; i < FACET_CTRL ; i++ )
    for ( j = 0 ; j < FACET_CTRL ; j++ )
     for ( k = 0 ; k < FACET_CTRL ; k++ )
     { v = vcoeff[i][j][k];
        h[i][j][2][0] += v*x[k][1];
        h[j][i][0][2] += v*x[k][1];
        h[i][k][2][1] += v*x[j][0];
        h[k][i][1][2] += v*x[j][0];
        h[j][k][0][1] += v*x[i][2];
        h[k][j][1][0] += v*x[i][2];
        h[i][j][2][1] -= v*x[k][0];
        h[j][i][1][2] -= v*x[k][0];
        h[i][k][2][0] -= v*x[j][1];
        h[k][i][0][2] -= v*x[j][1];
        h[j][k][1][0] -= v*x[i][2];
        h[k][j][0][1] -= v*x[i][2];
     }
  return vol;
} /* end q_facet_volume_q_hess() */

