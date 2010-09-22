/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/**********************************************************************
*
*  File: gauss.c
*
*  Purpose: Does calculations needed for including gauss curvature
*              integral named method. Linear model only.
*              Implemented as total angle deficit of boundary vertices.
*              Uses general quantity interface.
*/

#include "include.h"

void sqgauss_method_cleanup ARGS((void));
REAL levine_energy_all ARGS(( struct qinfo *, int )); 

/*********************************************************************
*
* function: gauss_integral_init()
*
* purpose:  Check illegalities, and sees if attributes defined for
*           designating boundaries.
*
*/

#define GAUSS_BDRY_V_NAME "gauss_bdry_v"
#define GAUSS_BDRY_E_NAME "gauss_bdry_e"
int gauss_bdry_v_attr,gauss_bdry_e_attr;

void gauss_integral_init(mode,mi)
int mode;
struct method_instance *mi;
{
  if ( web.dimension != 2 )
     kb_error(1590,"gauss_integral method only for 2D facets.\n",RECOVERABLE);

  if ( SDIM != 3 )
     kb_error(1591,"gauss_integral method only for 3D space.\n",RECOVERABLE);

  if ( web.modeltype != LINEAR )
     kb_error(1592,"gauss_integral method only for LINEAR model.\n",RECOVERABLE);

    gauss_bdry_v_attr = find_attribute(VERTEX,GAUSS_BDRY_V_NAME);
    gauss_bdry_e_attr = find_attribute(EDGE,GAUSS_BDRY_E_NAME);
}

/********************************************************************
*
* function: gauss_int_energy()
*
* purpose: single facet contribution to total angle of boundary
*             vertices.
*
*/

REAL gauss_int_energy(f_info)
struct qinfo *f_info;
{ REAL s1[MAXCOORD],s2[MAXCOORD];
  facetedge_id fe;
  vertex_id v_id;
  int k;
  REAL energy = 0.0;
  REAL a;

  fe = get_facet_fe(f_info->id);
  for ( k = 0 ; k < FACET_VERTS ; k++, fe = get_next_edge(fe) )
  {  edge_id e_id = get_fe_edge(fe);

     if ( gauss_bdry_e_attr >= 0 )
     { if ( *(int*)get_extra(e_id,gauss_bdry_e_attr) )
        energy -= M_PI;  /* normalization */
     } else
       if ( get_eattr(e_id) & (BOUNDARY|FIXED) )
        energy -= M_PI;  /* normalization */

     v_id = get_fe_tailv(fe);
     if ( gauss_bdry_v_attr >= 0 )
     { if ( *(int*)get_extra(v_id,gauss_bdry_v_attr) == 0 )
        continue;
     }
     else if ( !(get_vattr(v_id) & (BOUNDARY|FIXED) ) ) continue;

     get_fe_side(fe,s1);
     get_fe_side(get_prev_edge(fe),s2);
     a = -SDIM_dot(s1,s2)/sqrt(SDIM_dot(s1,s1)*SDIM_dot(s2,s2));
     energy += acos(a);
  }
  return energy;
}


/********************************************************************
*
* function: gauss_int_gradient()
*
* purpose: single facet contribution to gradient of  total angle of 
*             boundary vertices.
*
*/

REAL gauss_int_gradient(f_info)
struct qinfo *f_info;
{ REAL s1[MAXCOORD],s2[MAXCOORD];
  facetedge_id fe;
  vertex_id v_id;
  int j,k;
  REAL energy = 0.0;
  REAL ss1,ss2,ss12;
  REAL a,b;

  fe = get_facet_fe(f_info->id);
  for ( k = 0 ; k < FACET_VERTS ; k++ )
     memset((char*)(f_info->grad[k]),0,SDIM*sizeof(REAL));
  for ( k = 0 ; k < FACET_VERTS ; k++, fe = get_next_edge(fe) )
  {  edge_id e_id = get_fe_edge(fe);

     if ( gauss_bdry_e_attr >= 0 )
     { if ( *(int*)get_extra(e_id,gauss_bdry_e_attr) )
        energy -= M_PI;  /* normalization */
     } else
     if ( get_eattr(e_id) & (BOUNDARY|FIXED) )
        energy -= M_PI;  /* normalization */

     v_id = get_fe_tailv(fe);
     if ( gauss_bdry_v_attr >= 0 )
     { if ( *(int*)get_extra(v_id,gauss_bdry_v_attr) == 0 )
        continue;
     }
     else if ( !(get_vattr(v_id) & (BOUNDARY|FIXED) ) ) continue; 

     get_fe_side(fe,s1);
     get_fe_side(get_prev_edge(fe),s2);
     ss1 = SDIM_dot(s1,s1);
     ss2 = SDIM_dot(s2,s2);
     ss12 = SDIM_dot(s1,s2);
     a = -ss12/sqrt(ss1*ss2);
     energy += acos(a);
     b = sqrt(1-a*a)*ss1*sqrt(ss1*ss2);
     for ( j = 0 ; j < SDIM ; j++ )
        f_info->grad[(k+1)%3][j] += (ss1*s2[j] - ss12*s1[j])/b;
     b = sqrt(1-a*a)*ss2*sqrt(ss1*ss2);
     for ( j = 0 ; j < SDIM ; j++ )
        f_info->grad[(k+2)%3][j] += -(ss2*s1[j] - ss12*s2[j])/b;

  }
  return energy;
}


/********************************************************************
*
*  Function: sqgauss_energy()
*
*  Purpose:  Does square gauss curvature energy calculation for vertices.
*
*/

void sqgauss_energy()
{ 
  REAL modulus = globals(sqgauss_param)->value.real;
  vertex_id v_id,v[3];
  edge_id e_id;
  facet_id f_id;
  facetedge_id fe;
  int fixcount;
  int i;
  REAL area;
  REAL side[3][MAXCOORD];
  REAL ss[3];
  REAL st[3];
  REAL angle;
  REAL c;
  REAL gc;  /* gaussian curvarture */
  struct gvert { REAL angle;
                 REAL area;
                 REAL star_area;
                } *gverts,*gv;

  gverts = (struct gvert*)temp_calloc(web.skel[VERTEX].max_ord+1,
                                            sizeof(struct gvert));

  /* accumulate angles around vertices */
  FOR_ALL_FACETS(f_id)
    { fe = get_facet_fe(f_id);
      fixcount = 0;
      for ( i = 0; i < FACET_VERTS ; i++,fe=get_next_edge(fe) )
        { e_id = get_fe_edge(fe);
          get_edge_side(e_id,side[i]);
          v[i] = get_edge_tailv(e_id);
          if ( get_vattr(v[i]) & (FIXED|BOUNDARY) ) fixcount++;
        }
        for ( i = 0 ; i < FACET_VERTS ; i++ )
         { ss[i] = SDIM_dot(side[i],side[i]);
            st[i] = SDIM_dot(side[i],side[(i+2)%3]);
         }
        area = 0.5*sqrt(ss[0]*ss[1]-st[1]*st[1]);
        for ( i = 0 ; i < FACET_VERTS ; i++ )
        { c = -st[i]/sqrt(ss[i]*ss[(i+2)%3]);
          angle = acos(c);
          gv = gverts + loc_ordinal(v[i]);
          gv->angle += angle;
          gv->area  += area/3;
          gv->star_area  += area/(3-fixcount);
        }
    }
  
  /* calc square gauss curvature at each vertex */
  FOR_ALL_VERTICES(v_id)
    { struct gvert *vg = gverts + loc_ordinal(v_id);
      if ( get_vattr(v_id) & (FIXED|BOUNDARY) ) continue;
      gc = (wedge_angle(v_id) - vg->angle)/vg->area;
      binary_tree_add(web.total_energy_addends,modulus*gc*gc*vg->star_area);
    }


  temp_free((char*)gverts);
}

/*************************************************************************
*
* function: wedge_angle()
*
* purpose: find topological angle around vertex:
*             2*pi for nonconstrained vertex
*             pi for one constraint
*             calculate angle between 2 constraints
*/

REAL wedge_angle(v_id)
vertex_id v_id;
{ int concount;
  conmap_t *conmap = get_v_constraint_map(v_id);
  REAL normal[2][MAXCOORD];
  REAL c;
  int j;
  struct constraint *con;
  REAL fval;

  concount = conmap[0];

  if ( concount == 0 ) return 2*M_PI;
  if ( concount == 1 ) return M_PI;

  /* two constraints, so find angle between */

  if ( concount > 2 ) 
  { sprintf(errmsg,
     "gauss_curvature_integral: More than two constraints on vertex %s.\n",
          ELNAME(v_id));
    kb_error(1593,errmsg,RECOVERABLE);
  }
  for ( j = 1, concount = 0 ; j <= (int)conmap[0] ; j++ ) 
  { 
    con = get_constraint(conmap[j]);
    eval_all(con->formula,get_coord(v_id),SDIM,&fval,normal[concount],v_id); 
    concount++;
  }
  c = SDIM_dot(normal[0],normal[1])
        /sqrt(SDIM_dot(normal[0],normal[0])) 
        /sqrt(SDIM_dot(normal[1],normal[1]));
  return M_PI - acos(c);
}


/********************************************************************
*
*  Function: sqgauss_force()
*
*  Purpose:  Does square gauss curvature force calculation for vertices.
*
*/

void sqgauss_force()
{ 
  REAL modulus = globals(sqgauss_param)->value.real;
  vertex_id v_id,v[3];
  edge_id e_id,e[3];
  facet_id f_id;
  facetedge_id fe;
  int fixcount;
  REAL side[3][MAXCOORD];
  REAL ss[3];
  REAL st[3];
  int j,i;
  REAL area,angle,c,c1,c2;
  REAL *f;
  REAL gc;  /* gaussian curvarture */
  struct gvert { REAL angle;
                 REAL angle_grad[MAXCOORD];
                 REAL area;
                 REAL area_grad[MAXCOORD];
                 REAL star_area;
                 REAL star_area_grad[MAXCOORD];
                 REAL gc;  /* gaussian curvature */
                 } *gverts;
  struct gedge { REAL area_grad[2][MAXCOORD];
                 REAL star_area_grad[2][MAXCOORD];
                 REAL angle_grad[2][MAXCOORD];
                 /* dhead/dtail, dtail/dhead */
               } *gedges;
  gverts = (struct gvert*)temp_calloc(web.skel[VERTEX].max_ord+1,
                                            sizeof(struct gvert));
  gedges = (struct gedge*)temp_calloc(web.skel[EDGE].max_ord+1,
                                            sizeof(struct gedge));

  /* accumulate angles around vertices */
  FOR_ALL_FACETS(f_id)
    { fe = get_facet_fe(f_id);
      fixcount = 0;
      for ( i = 0; i < FACET_VERTS ; i++,fe=get_next_edge(fe) )
        { e[i] = get_fe_edge(fe);
          get_edge_side(e[i],side[i]);
          v[i] = get_edge_tailv(e[i]);
          if ( get_vattr(v[i]) & (FIXED|BOUNDARY) ) fixcount++;
        }
        for ( i = 0 ; i < FACET_VERTS ; i++ )
         { ss[i] = SDIM_dot(side[i],side[i]);
           st[i] = SDIM_dot(side[i],side[(i+2)%3]);
         }
        area = 0.5*sqrt(ss[0]*ss[1]-st[1]*st[1]);
        for ( i = 0 ; i < FACET_VERTS ; i++ )
        { int i1 = (i+1)%3;
          int i2 = (i+2)%3;
          struct gedge *ge1,*ge2;
          struct gvert *gv;
          int jj1,jj2;

          gv = gverts + loc_ordinal(v[i]);
          ge1 = gedges + loc_ordinal(e[i]);
          ge2 = gedges + loc_ordinal(e[i2]);
          jj1 =  inverted(e[i]) ? 1 : 0;
          jj2 =  inverted(e[i2]) ? 0 : 1;

          c = -st[i]/sqrt(ss[i]*ss[i2]);
          angle = acos(c);
          gv->angle += angle;
          c1 = (1+st[i]/ss[i])/area/2;
          c2 = (1+st[i]/ss[i2])/area/2;
          for ( j = 0 ; j < SDIM ; j++ )
             gv->angle_grad[j] += c1*side[i][j] - c2*side[i2][j];
          c1 = st[i]/ss[i]/area/2;
          c2 = 1/area/2;
          for ( j = 0 ; j < SDIM ; j++ )
             ge1->angle_grad[jj1][j] -= c1*side[i][j] - c2*side[i2][j];
          c1 = 1/area/2;
          c2 = st[i]/ss[i2]/area/2;
          for ( j = 0 ; j < SDIM ; j++ )
             ge2->angle_grad[jj2][j] -= c1*side[i][j] - c2*side[i2][j];
          gv->area  += area/3;
          gv->star_area  += area/(3-fixcount);
          c1 = ss[i1]/area/4;
          c2 = st[i1]/area/4;
          for ( j = 0 ; j < SDIM ; j++ )
            { c = c2*side[i1][j] - c1*side[i][j];
              gv->area_grad[j] += c/3;
              gv->star_area_grad[j] += c/(3-fixcount);
              ge1->area_grad[jj1][j] += c/3;
              ge2->area_grad[jj2][j] += c/3;
              ge1->star_area_grad[jj1][j] += c/(3-fixcount);
              ge2->star_area_grad[jj2][j] += c/(3-fixcount);
            }
        }
    }
  
  /* calc square gauss curvature at each vertex */
  FOR_ALL_VERTICES(v_id)
    { struct gvert *gv = gverts + loc_ordinal(v_id);
      if ( get_vattr(v_id) & (FIXED|BOUNDARY) ) continue;
      gc = (wedge_angle(v_id) - gv->angle)/gv->area;
      binary_tree_add(web.total_energy_addends,modulus*gc*gc*gv->star_area);
      gv->gc = gc;

      /* now self terms in derivative */
      f = get_force(v_id);
      for ( j = 0 ; j < SDIM ; j++ )
         f[j] -= (2*gc*gv->star_area*(-gv->angle_grad[j]/gv->area 
                         - gc*gv->area_grad[j]/gv->area)
                         + gc*gc*gv->star_area_grad[j])*modulus;
    }
  
  /* now cross terms from edges */
  FOR_ALL_EDGES(e_id)
    { struct gvert *gv1 = gverts + loc_ordinal(get_edge_tailv(e_id));
      struct gvert *gv2 = gverts + loc_ordinal(get_edge_headv(e_id));
      struct gedge *ge = gedges + loc_ordinal(e_id);

      /* head energy as function of tail */
      if ( !(get_vattr(get_edge_headv(e_id)) & (FIXED|BOUNDARY) ) )
        { f = get_force(get_edge_tailv(e_id));
          for ( j = 0 ; j < SDIM ; j++ )
             f[j] -= (2*gv2->gc*gv2->star_area*(-ge->angle_grad[0][j]/gv2->area
                        - gv2->gc*ge->area_grad[0][j]/gv2->area)
                        + gv2->gc*gv2->gc*ge->star_area_grad[0][j])*modulus;
        }
      /* tail energy as function of head */
      if ( !(get_vattr(get_edge_tailv(e_id)) & (FIXED|BOUNDARY) ) )
        { f = get_force(get_edge_headv(e_id));
          for ( j = 0 ; j < SDIM ; j++ )
             f[j] -= (2*gv1->gc*gv1->star_area*(-ge->angle_grad[1][j]/gv1->area
                        - gv1->gc*ge->area_grad[1][j]/gv1->area)
                        + gv1->gc*gv1->gc*ge->star_area_grad[1][j])*modulus;
        }
    }

  temp_free((char*)gverts);
  temp_free((char*)gedges);
}


/********************************************************************

                     sqgauss integral as method

*********************************************************************/

static  struct gvert { REAL angle;
                       REAL angle_grad[MAXCOORD];
                       REAL area;
                       REAL area_grad[MAXCOORD];
                       REAL star_area;
                       REAL star_area_grad[MAXCOORD];
                       REAL gc;  /* gaussian curvature */
                     } *gverts,*gv;
static  struct gedge { REAL area_grad[2][MAXCOORD];
                       REAL star_area_grad[2][MAXCOORD];
                       REAL angle_grad[2][MAXCOORD];
                       /* dhead/dtail, dtail/dhead */
                     } *gedges;

/*************************************************************************
*
* function: sqgauss_method_init()
*
* purpose: gather data for sqgauss method.
*             Should really be replaced by local gathering.
*/

void sqgauss_method_init(mode,mi)
int mode;
struct method_instance *mi;
{
  vertex_id v[3];
  edge_id e_id,e[3];
  facet_id f_id;
  facetedge_id fe;
  int fixcount;
  REAL side[3][MAXCOORD];
  REAL ss[3];
  REAL st[3];
  int j,i;
  REAL area,angle,c,c1,c2;

  if ( web.modeltype != LINEAR )
    kb_error(2866,"Method sq_gauss_curvature only for LINEAR model.\n",
       RECOVERABLE);

  gverts = (struct gvert*)temp_calloc(web.skel[VERTEX].max_ord+1,
                                        sizeof(struct gvert));

  /* accumulate angles around vertices */
  if ( mode == METHOD_VALUE )
  FOR_ALL_FACETS(f_id)
    { fe = get_facet_fe(f_id);
      fixcount = 0;
      for ( i = 0; i < FACET_VERTS ; i++,fe=get_next_edge(fe) )
        { e_id = get_fe_edge(fe);
          get_edge_side(e_id,side[i]);
          v[i] = get_edge_tailv(e_id);
          if ( get_vattr(v[i]) & (FIXED|BOUNDARY) ) fixcount++;
        }
        for ( i = 0 ; i < FACET_VERTS ; i++ )
         { ss[i] = SDIM_dot(side[i],side[i]);
           st[i] = SDIM_dot(side[i],side[(i+2)%3]);
         }
        area = 0.5*sqrt(ss[0]*ss[1]-st[1]*st[1]);
        for ( i = 0 ; i < FACET_VERTS ; i++ )
        { c = -st[i]/sqrt(ss[i]*ss[(i+2)%3]);
          angle = acos(c);
          gv = gverts + loc_ordinal(v[i]);
          gv->angle += angle;
          gv->area  += area/3;
          gv->star_area  += area/(3-fixcount);
        }
    }
  else if ( mode == METHOD_GRADIENT )
  {
    gedges = (struct gedge*)temp_calloc(web.skel[EDGE].max_ord+1,
                                            sizeof(struct gedge));

    /* accumulate angles around vertices */
    FOR_ALL_FACETS(f_id)
    { fe = get_facet_fe(f_id);
      fixcount = 0;
      for ( i = 0; i < FACET_VERTS ; i++,fe=get_next_edge(fe) )
        { e[i] = get_fe_edge(fe);
          get_edge_side(e[i],side[i]);
          v[i] = get_edge_tailv(e[i]);
          if ( get_vattr(v[i]) & (FIXED|BOUNDARY) ) fixcount++;
        }
        for ( i = 0 ; i < FACET_VERTS ; i++ )
         { ss[i] = SDIM_dot(side[i],side[i]);
            st[i] = SDIM_dot(side[i],side[(i+2)%3]);
         }
        area = 0.5*sqrt(ss[0]*ss[1]-st[1]*st[1]);
        for ( i = 0 ; i < FACET_VERTS ; i++ )
        { int i1 = (i+1)%3;
          int i2 = (i+2)%3;
          struct gedge *ge1,*ge2;
          int jj1,j2;

          gv = gverts + loc_ordinal(v[i]);
          ge1 = gedges + loc_ordinal(e[i]);
          ge2 = gedges + loc_ordinal(e[i2]);
          jj1 =  inverted(e[i]) ? 1 : 0;
          j2 =  inverted(e[i2]) ? 0 : 1;

          c = -st[i]/sqrt(ss[i]*ss[i2]);
          angle = acos(c);
          gv->angle += angle;
          c1 = (1+st[i]/ss[i])/area/2;
          c2 = (1+st[i]/ss[i2])/area/2;
          for ( j = 0 ; j < SDIM ; j++ )
             gv->angle_grad[j] += c1*side[i][j] - c2*side[i2][j];
          c1 = st[i]/ss[i]/area/2;
          c2 = 1/area/2;
          for ( j = 0 ; j < SDIM ; j++ )
             ge1->angle_grad[jj1][j] -= c1*side[i][j] - c2*side[i2][j];
          c1 = 1/area/2;
          c2 = st[i]/ss[i2]/area/2;
          for ( j = 0 ; j < SDIM ; j++ )
             ge2->angle_grad[j2][j] -= c1*side[i][j] - c2*side[i2][j];
          gv->area  += area/3;
          gv->star_area  += area/(3-fixcount);
          c1 = ss[i1]/area/4;
          c2 = st[i1]/area/4;
          for ( j = 0 ; j < SDIM ; j++ )
            { c = c2*side[i1][j] - c1*side[i][j];
              gv->area_grad[j] += c/3;
              gv->star_area_grad[j] += c/(3-fixcount);
              ge1->area_grad[jj1][j] += c/3;
              ge2->area_grad[j2][j] += c/3;
              ge1->star_area_grad[jj1][j] += c/(3-fixcount);
              ge2->star_area_grad[j2][j] += c/(3-fixcount);
            }
        }
    }
  } 
}

/************************************************************************
*
* function: sqgauss_method_cleanup()
*
* purpose: free memory allocated in initialization.
*/

void sqgauss_method_cleanup()
{
  temp_free((char*)gverts);
  temp_free((char*)gedges);
}

/********************************************************************
*
*  Function: sqgauss_method_value()
*
*  Purpose:  Does square gauss curvature energy calculation for vertices.
*
*/

REAL sqgauss_method_value(v_info)
struct qinfo *v_info;
{ 
  REAL modulus = sqgauss_flag ? globals(sqgauss_param)->value.real : 0.0;
  REAL gc;  /* gaussian curvarture */

  /* calc square gauss curvature at each vertex */
  struct gvert *vg = gverts + loc_ordinal(v_info->id);
  if ( get_vattr(v_info->id) & (FIXED|BOUNDARY) ) return 0.0;
  gc = (wedge_angle(v_info->id) - vg->angle)/vg->area;
  return modulus*gc*gc*vg->star_area;
}


/********************************************************************
*
*  Function: sqgauss_method_grad()
*
*  Purpose:  Does square gauss curvature force calculation for vertices.
*
*/

REAL sqgauss_method_grad(v_info)
struct qinfo *v_info;
{ 
  REAL modulus = sqgauss_flag ? globals(sqgauss_param)->value.real : 0.0;
  vertex_id v_id;
  edge_id e_id,start_e;
  int j;
  REAL gc;  /* gaussian curvarture */
  REAL energy = 0.0;

  /* calc square gauss curvature at each vertex */
  v_id = v_info->id;
  gv = gverts + loc_ordinal(v_id);
  if ( get_vattr(v_id) & (FIXED|BOUNDARY) ) return 0.0;
  gc = (wedge_angle(v_id) - gv->angle)/gv->area;
  energy += modulus*gc*gc*gv->star_area;
  gv->gc = gc;

  /* now self terms in derivative */
  for ( j = 0 ; j < SDIM ; j++ )
     v_info->grad[0][j] += (2*gc*gv->star_area*(-gv->angle_grad[j]/gv->area 
                         - gc*gv->area_grad[j]/gv->area)
                         + gc*gc*gv->star_area_grad[j])*modulus;
  
  /* now cross terms from edges */
  start_e = e_id = get_vertex_edge(v_id);
  if ( !(get_vattr(v_id) & (FIXED|BOUNDARY) ) )
    do
    { 
      struct gvert *gv2 = gverts + loc_ordinal(get_edge_headv(e_id));
      struct gedge *ge = gedges + loc_ordinal(e_id);

      /* head energy as function of tail */
        for ( j = 0 ; j < SDIM ; j++ )
             v_info->grad[0][j] += 
                    (2*gv2->gc*gv2->star_area*(-ge->angle_grad[0][j]/gv2->area
                        - gv2->gc*ge->area_grad[0][j]/gv2->area)
                        + gv2->gc*gv2->gc*ge->star_area_grad[0][j])*modulus;

      e_id = get_next_tail_edge(e_id);
    } while ( !equal_element(start_e,e_id) );

 return energy;
}

/*****************************************************************************

          Gaussian curvature at vertices (star model)

******************************************************************************/

void star_gauss_method_init(mode,mi)
int mode;
struct method_instance *mi;
{
  if ( web.modeltype != LINEAR )
    kb_error(2869,"Method gauss_curvature only for LINEAR model.\n",
       RECOVERABLE);
}

REAL star_gauss_method_all(v_info,mode)
struct qinfo *v_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */
{ REAL deficit = 2*M_PI;
  int k,i,j;
  int fudge = (v_info->flags & INCOMPLETE_STAR) ? 1 : 0;

  if ( v_info->vcount <= 1 ) return 0.0;

  for ( k = 1 ; k < v_info->vcount-fudge ; k++ )
  { REAL ss1,ss2,s1s2;
    int kk = (!fudge && k==(v_info->vcount-1)) ? 1 : k+1;
    REAL *s1 = v_info->sides[0][k-1],*s2 = v_info->sides[0][kk-1];
    REAL denom;

    ss1 = SDIM_dot(s1,s1);
    ss2 = SDIM_dot(s2,s2);
    s1s2 = SDIM_dot(s1,s2);
    deficit -= acos(s1s2/sqrt(ss1*ss2));

    if ( mode == METHOD_VALUE )
      continue;

    denom = sqrt(ss1*ss2 - s1s2*s1s2);
    for ( i = 0 ; i < SDIM ; i++ )
    { REAL ddefds1,ddefds2;
      ddefds1 = (s2[i] - s1s2/ss1*s1[i])/denom;
      ddefds2 = (s1[i] - s1s2/ss2*s2[i])/denom;

      v_info->grad[k][i] += ddefds1;
      v_info->grad[0][i] -= ddefds1;
      v_info->grad[kk][i] += ddefds2;
      v_info->grad[0][i] -= ddefds2;

      if ( mode == METHOD_GRADIENT )
        continue;
  
      for ( j = 0 ; j < SDIM ; j++ )
      { REAL dddefds1ds1,dddefds1ds2,dddefds2ds1,dddefds2ds2;
  
        dddefds1ds1 = (-s2[j]/ss1*s1[i] + s1s2/ss1/ss1*2*s1[j]*s1[i]
                 - (i==j ? s1s2/ss1 : 0))/denom
          - ddefds1/denom*0.5/denom*(2*ss2*s1[j] - 2*s1s2*s2[j]); 

        dddefds1ds2 = ((i==j ? 1.0 : 0.0) - s1[j]/ss1*s1[i])/denom
          - ddefds1/denom*0.5/denom*(2*ss1*s2[j] - 2*s1s2*s1[j]);

        dddefds2ds1 = ((i==j ? 1.0 : 0.0) - s2[j]/ss2*s2[i])/denom
          - ddefds2/denom*0.5/denom*(2*ss2*s1[j] - 2*s1s2*s2[j]);

        dddefds2ds2 = (-s1[j]/ss2*s2[i] + s1s2/ss2/ss2*2*s2[j]*s2[i]
                 - (i==j ? s1s2/ss2 : 0))/denom
          - ddefds2/denom*0.5/denom*(2*ss1*s2[j] - 2*s1s2*s1[j]);

        v_info->hess[k][k][i][j] += dddefds1ds1;
        v_info->hess[0][k][i][j] -= dddefds1ds1;
        v_info->hess[k][0][i][j] -= dddefds1ds1;
        v_info->hess[0][0][i][j] += dddefds1ds1;

        v_info->hess[k][kk][i][j] += dddefds1ds2;
        v_info->hess[0][kk][i][j] -= dddefds1ds2;
        v_info->hess[k][0][i][j] -= dddefds1ds2;
        v_info->hess[0][0][i][j] += dddefds1ds2;

        v_info->hess[kk][k][i][j] += dddefds2ds1;
        v_info->hess[0][k][i][j] -= dddefds2ds1;
        v_info->hess[kk][0][i][j] -= dddefds2ds1;
        v_info->hess[0][0][i][j] += dddefds2ds1;

        v_info->hess[kk][kk][i][j] += dddefds2ds2;
        v_info->hess[0][kk][i][j] -= dddefds2ds2;
        v_info->hess[kk][0][i][j] -= dddefds2ds2;
        v_info->hess[0][0][i][j] += dddefds2ds2;

      }
        
    }

  }


  return deficit;
}

REAL star_gauss_method_value(v_info)
struct qinfo *v_info;
{ return star_gauss_method_all(v_info,METHOD_VALUE);
}
REAL star_gauss_method_grad(v_info)
struct qinfo *v_info;
{ return star_gauss_method_all(v_info,METHOD_GRADIENT);
}
REAL star_gauss_method_hess(v_info)
struct qinfo *v_info;
{ return star_gauss_method_all(v_info,METHOD_HESSIAN);
}

/*****************************************************************************

          Square Gaussian curvature integral at vertices (star model)

******************************************************************************/


void star_sqgauss_method_init(mode,mi)
int mode;
struct method_instance *mi;
{
  if ( web.modeltype != LINEAR )
    kb_error(2867,"Method sq_gauss_curvature only for LINEAR model.\n",
       RECOVERABLE);
}

REAL star_sqgauss_method_all(v_info,mode)
struct qinfo *v_info;
int mode; /* METHOD_VALUE, etc. */
{ REAL deficit = 2*M_PI;
  REAL area = 0.0;
  int k,kk,i,j;
  REAL **dadv=NULL,**ddefdv=NULL; /* gradient storage */
  int pairs,final_edge;

  if ( v_info->vcount <= 1 ) return 0.0;
  pairs = (v_info->vcount - 1);
  if ( pairs <= 0 ) return 0.0;
  if ( v_info->flags & INCOMPLETE_STAR )
  { pairs--;
    final_edge = pairs;
  }
  else 
    final_edge = 0;

  if ( mode == METHOD_HESSIAN )
  { dadv = temp_dmatrix(0,v_info->vcount,0,SDIM-1);
    ddefdv = temp_dmatrix(0,v_info->vcount,0,SDIM-1);
  } 

  for ( k = 1 ; k < v_info->vcount ; k++ )
  { REAL ss1,ss2,s1s2;
    int kk = (k==(v_info->vcount-1)) ? 0 : k;
    ss1 = SDIM_dot(v_info->sides[0][k-1],v_info->sides[0][k-1]);
    ss2 = SDIM_dot(v_info->sides[0][kk],v_info->sides[0][kk]);
    s1s2 = SDIM_dot(v_info->sides[0][k-1],v_info->sides[0][kk]);
    deficit -= acos(s1s2/sqrt(ss1*ss2));
    area += sqrt(ss1*ss2 - s1s2*s1s2)/6;
  }

  if ( mode == METHOD_VALUE )
    return deficit*deficit/area;

  /* now individual vertex gradients */
  for ( k = 1 ; k < v_info->vcount ; k++ )
  {  REAL ss1,ss2,s1s2;
     int kk = (k==(v_info->vcount-1)) ? 1 : k+1;
     REAL *s1 = v_info->sides[0][k-1],*s2 = v_info->sides[0][kk-1];
     REAL denom;

     ss1 = SDIM_dot(s1,s1);
     ss2 = SDIM_dot(s2,s2);
     s1s2 = SDIM_dot(s1,s2);
     
     denom = sqrt(ss1*ss2 - s1s2*s1s2);
     for ( i = 0 ; i < SDIM ; i++ )
     { REAL ddefds1,ddefds2,dads1,dads2,dsqds1,dsqds2;
       int j;
        ddefds1 = (s2[i] - s1s2/ss1*s1[i])/denom;
        ddefds2 = (s1[i] - s1s2/ss2*s2[i])/denom;
        dads1 = (s1[i]*ss2 - s1s2*s2[i])/denom/6;
        dads2 = (s2[i]*ss1 - s1s2*s1[i])/denom/6;
        dsqds1 = 2*deficit*ddefds1/area - deficit*deficit/area/area*dads1;
        dsqds2 = 2*deficit*ddefds2/area - deficit*deficit/area/area*dads2;
        v_info->grad[k][i] += dsqds1;
        v_info->grad[0][i] -= dsqds1;
        v_info->grad[kk][i] += dsqds2;
        v_info->grad[0][i] -= dsqds2;

        if ( mode == METHOD_GRADIENT )
          continue;

        /* record gradients for later */
        if ( dadv )
        {
          ddefdv[k][i] += ddefds1;
          ddefdv[kk][i] += ddefds2;
          dadv[k][i] += dads1;
          dadv[kk][i] += dads2;
        }
        
        /* add in pure second partial terms */
        for ( j = 0 ; j < SDIM ; j++ )
        { REAL dddefds1ds1,dddefds1ds2,dddefds2ds1,dddefds2ds2;
          REAL ddads1ds1,ddads1ds2,ddads2ds1,ddads2ds2;
          REAL ddenomds1j,ddenomds2j;
          REAL ddsqds1ds1,ddsqds1ds2,ddsqds2ds1,ddsqds2ds2;     
  
          /*     denom = sqrt(ss1*ss2 - s1s2*s1s2);   */
          ddenomds1j = 1.0/denom*(s1[j]*ss2 - s1s2*s2[j]);
          ddenomds2j = 1.0/denom*(s2[j]*ss1 - s1s2*s1[j]);
  
          ddads1ds1 = ((i==j ? 1:0)*ss2 - s2[j]*s2[i])/denom/6
                   - (s1[i]*ss2 - s1s2*s2[i])/denom/6/denom*ddenomds1j;

          ddads1ds2 = (s1[i]*2*s2[j] - s1[j]*s2[i] - s1s2*(i==j?1.0:0.))/denom/6
                   - (s1[i]*ss2 - s1s2*s2[i])/denom/6/denom*ddenomds2j;
  
          ddads2ds1 = (s2[i]*2*s1[j] - s2[j]*s1[i] - s1s2*(i==j?1:0))/denom/6
                   - (s2[i]*ss1 - s1s2*s1[i])/denom/6/denom*ddenomds1j;
  
          ddads2ds2 = ((i==j?1.0:0.0)*ss1 - s1[j]*s1[i])/denom/6
                   - (s2[i]*ss1 - s1s2*s1[i])/denom/6/denom*ddenomds2j;
  
          dddefds1ds1 = (-s2[j]/ss1*s1[i] + s1s2/ss1/ss1*2*s1[j]*s1[i]
                   - (i==j ? s1s2/ss1 : 0))/denom - ddefds1/denom*ddenomds1j; 
  
          dddefds1ds2 = ((i==j ? 1.0 : 0.0) - s1[j]/ss1*s1[i])/denom
                   - ddefds1/denom*ddenomds2j;
  
          dddefds2ds1 = ((i==j ? 1.0 : 0.0) - s2[j]/ss2*s2[i])/denom
                   - ddefds2/denom*ddenomds1j;
  
          dddefds2ds2 = (-s1[j]/ss2*s2[i] + s1s2/ss2/ss2*2*s2[j]*s2[i]
                   - (i==j ? s1s2/ss2 : 0))/denom - ddefds2/denom*ddenomds2j;
  
          ddsqds1ds1 = 2*deficit*dddefds1ds1/area                                        
                     - deficit*deficit/area/area*ddads1ds1;
  
          ddsqds1ds2 = 2*deficit*dddefds1ds2/area                    
                     - deficit*deficit/area/area*ddads1ds2;
  
          ddsqds2ds1 = 2*deficit*dddefds2ds1/area 
                     - deficit*deficit/area/area*ddads2ds1;
  
          ddsqds2ds2 = 2*deficit*dddefds2ds2/area 
                     - deficit*deficit/area/area*ddads2ds2;
   
          v_info->hess[k][k][i][j] += ddsqds1ds1;
          v_info->hess[0][k][i][j] -= ddsqds1ds1;
          v_info->hess[k][0][i][j] -= ddsqds1ds1;
          v_info->hess[0][0][i][j] += ddsqds1ds1;
  
          v_info->hess[k][kk][i][j] += ddsqds1ds2;
          v_info->hess[0][kk][i][j] -= ddsqds1ds2;
          v_info->hess[k][0][i][j] -= ddsqds1ds2;
          v_info->hess[0][0][i][j] += ddsqds1ds2;
  
          v_info->hess[kk][k][i][j] += ddsqds2ds1;
          v_info->hess[0][k][i][j] -= ddsqds2ds1;
          v_info->hess[kk][0][i][j] -= ddsqds2ds1;
          v_info->hess[0][0][i][j] += ddsqds2ds1;
  
          v_info->hess[kk][kk][i][j] += ddsqds2ds2;
          v_info->hess[0][kk][i][j] -= ddsqds2ds2;
          v_info->hess[kk][0][i][j] -= ddsqds2ds2;
          v_info->hess[0][0][i][j] += ddsqds2ds2;
  
        }
     }
  }

  if ( mode == METHOD_HESSIAN )
  { /* add in product of gradient terms */
    for ( k = 1 ; k < v_info->vcount ; k++ )
      for ( kk = 1 ; kk < v_info->vcount ; kk++ )
        for ( i = 0 ; i < SDIM ; i++ )
          for ( j = 0 ; j < SDIM ; j++ )
          { REAL term;
            term = 2/area*ddefdv[k][i]*ddefdv[kk][j]
                 - 2*deficit/area/area*dadv[k][i]*ddefdv[kk][j]
                 - 2*deficit/area/area*ddefdv[k][i]*dadv[kk][j]
                 + 2*deficit*deficit/area/area/area*dadv[k][i]*dadv[kk][j];

            v_info->hess[k][kk][i][j] += term;
            v_info->hess[0][kk][i][j] -= term;
            v_info->hess[k][0][i][j] -= term;
            v_info->hess[0][0][i][j] += term;
         }
    free_temp_matrix(dadv);
    free_temp_matrix(ddefdv);
  }
   
  return deficit*deficit/area;
}


REAL star_sqgauss_method_value(v_info)
struct qinfo *v_info;
{ return star_sqgauss_method_all(v_info,METHOD_VALUE);
}
REAL star_sqgauss_method_grad(v_info)
struct qinfo *v_info;
{ return star_sqgauss_method_all(v_info,METHOD_GRADIENT);
}
REAL star_sqgauss_method_hess(v_info)
struct qinfo *v_info;
{ return star_sqgauss_method_all(v_info,METHOD_HESSIAN);
}


/**************************************************************************

                levine_energy

       A curvature-dependent energy requested by Dov Levine,
                levine@techunix.technion.ac.il

     Depends on two variables, levine_a and levine_t, to be set by user.

     E = Integral ( K1*K2*t/a + 1/4/a*(K1 - K2)*log((1-K2*t)/(1-K1*t)) dA

     where K1 and K2 are the principal curvatures.

****************************************************************************/

static REAL levine_a=1.0,levine_t=1.0,levine_c=1.0;

void levine_energy_init(mode,meth)
int mode;
struct method_instance *meth;
{ int k;

  /* find the two variables */

  k = lookup_global("levine_a");
  if ( k >= 0 )
      levine_a = globals(k)->value.real;
  else levine_a = 1.0;

  k = lookup_global("levine_t");
  if ( k >= 0 )
      levine_t = globals(k)->value.real;
  else levine_t = 1.0;

  k = lookup_global("levine_c");  /* coeff of log part, for test */
  if ( k >= 0 )
      levine_c = globals(k)->value.real;
  else levine_c = 1.0;
}

REAL levine_energy_all(v_info,mode)
struct qinfo *v_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */ 
{ REAL deficit = 2*M_PI;
  REAL area = 0.0;
  REAL coeff = levine_c*levine_t/levine_a;
  int variety; /* PLAIN_SQ, EFF_SQ, NORMAL_SQ */
  int pairs;
  int i,j,k;
  REAL energy,ff,fn,g;
  REAL dAdv[MAXCOORD], *a, *d, *s1s1, *s1s2, *s2s2;
  REAL vnorm[MAXCOORD];
  REAL **dAdv1,**dAdv2,***dvnorm1,***dvnorm2,**ds2;
  REAL ***ddAdv1dv1,***ddAdv2dv1,***ddAdv1dv2,***ddAdv2dv2;
  REAL ***ddss12,***ddss21,***ddss22;
  REAL **dfndv1,**dfndv2,**dffdv1,**dffdv2,**dnndv1,**dnndv2;
  REAL ***dfdv1,***dfdv2;
  REAL *s1,*s2;
  REAL **s = v_info->sides[0];
  REAL temp[MAXCOORD];
#define MAXV 10
  REAL aa[MAXV*5];
  MAT2D(ds1,12*MAXV,MAXCOORD);
  MAT3D(ddss11,16*MAXV,MAXCOORD,MAXCOORD);
  REAL hh0;  /* curvature adjusted for h-h0 */
  REAL gc,root,kappa1,kappa2;
  REAL dgc[MAXV][MAXCOORD];
  REAL dhh0[MAXV][MAXCOORD];
  REAL darea[MAXV][MAXCOORD];
  REAL ddef[MAXV][MAXCOORD];

  variety = NORMAL_SQ;

  if ( v_info->vcount <= 1 ) return 0.0;

  /* gaussian curvature */
  for ( k = 1 ; k < v_info->vcount ; k++ )
  { REAL ss1,ss2,s1s2;
    int kk = (k==(v_info->vcount-1)) ? 0 : k;
    ss1 = SDIM_dot(v_info->sides[0][k-1],v_info->sides[0][k-1]);
    ss2 = SDIM_dot(v_info->sides[0][kk],v_info->sides[0][kk]);
    s1s2 = SDIM_dot(v_info->sides[0][k-1],v_info->sides[0][kk]);
    deficit -= acos(s1s2/sqrt(ss1*ss2));
    area += sqrt(ss1*ss2 - s1s2*s1s2)/6;
  }
  gc = deficit/area;

  /* mean curvature */
  pairs = (v_info->vcount - 1);
  if ( pairs <= 0 ) return 0.0;

  if ( v_info->vcount > MAXV )
  { a = (REAL*)mycalloc(5*pairs,sizeof(REAL));
    ds1 = dmatrix(0,12*v_info->vcount,0,SDIM-1);
  } else
  { memset((char*)aa,0,sizeof(REAL)*5*pairs);
    a = aa;
    memset((char*)ds1[0],0,sizeof(REAL)*12*v_info->vcount*MAXCOORD);
  }
  d = a+pairs; s1s1 = d + pairs; s1s2 = s1s1 + pairs; s2s2 = s1s2 + pairs;
  ds2 = ds1 + pairs; dAdv1 = ds2 + pairs; dAdv2 = dAdv1 + pairs;
  dfndv1 = dAdv2 + pairs; dfndv2 = dfndv1 + pairs; dffdv1 = dfndv2 + pairs;
  dffdv2 = dffdv1 + pairs; dnndv1 = dffdv2 + pairs; dnndv2 = dnndv1 + pairs;

  /* basic dot products */
  /* area = 0.0; */
  for ( j = 0 ; j < SDIM ; j++ ) dAdv[j] = vnorm[j] = 0.0;
  for ( k = 0 ; k < pairs ; k++ )
  { s1 = s[k]; s2 = s[(k+1==pairs)?0:k+1];
    s1s1[k] = SDIM_dot(s1,s1);
    s1s2[k] = SDIM_dot(s1,s2);
    s2s2[k] = SDIM_dot(s2,s2);
    d[k] = s1s1[k]*s2s2[k] - s1s2[k]*s1s2[k];
    a[k] = 0.5*sqrt(d[k]);
    for ( j = 0 ; j < SDIM ; j++ )
    { ds1[k][j] = 2*(s2s2[k]*s1[j] - s1s2[k]*s2[j]);
      ds2[k][j] = 2*(s1s1[k]*s2[j] - s1s2[k]*s1[j]);
      dAdv1[k][j] = 0.125/a[k]*ds1[k][j];
      dAdv2[k][j] = 0.125/a[k]*ds2[k][j];
      dAdv[j] -= dAdv1[k][j] + dAdv2[k][j];
    }
    if ( variety != PLAIN_SQ )
    { cross_prod(s1,s2,temp);
      for ( j = 0 ; j < SDIM ; j++ )
        vnorm[j] += 0.5*temp[j]/3;
    }
  }

  /* energy */
  ff = SDIM_dot(dAdv,dAdv);
  fn = SDIM_dot(dAdv,vnorm);
  if ( fn == 0.0 ) 
  { ff = 0.0; fn = 1e-6; }
  hh0 = ff/fn/2;  /* mean curvature, so divide by 2 */
  
  if ( hh0*hh0 < gc ) 
    root = 0.0;
  else 
    root = sqrt(hh0*hh0 - gc);
  kappa1 = hh0 + root;
  kappa2 = hh0 - root;
#define APPROX_LEVINE_XX
#ifdef APPROX_LEVINE
  /* express in terms of hh0 and gc without sqrt to keep smooth */
  energy = coeff*deficit 
            + 4*(hh0*hh0 - gc)/4/levine_a*levine_t*(1 + 2*hh0*levine_t)*area;
#else
  energy = coeff*deficit 
            + (2*root)/4/levine_a*
               log((1-kappa2*levine_t)/(1-kappa1*levine_t))*area;
#endif

  if ( !is_finite(energy) )
      kb_error(2586,"Levine energy infinite.\n",WARNING);
  if ( mode == METHOD_VALUE )
    return energy;

  /* now individual vertex gradients */
  for ( k = 0 ; k < v_info->vcount ; k++ )
   for ( i = 0 ; i < SDIM ; i++ )
   { dgc[k][i] = dhh0[k][i] = darea[k][i] = ddef[k][i] = 0.0;
   }

  /* gaussian part */
  for ( k = 1 ; k < v_info->vcount ; k++ )
  {  REAL ss1,ss2,s1s2;
     int kk = (k==(v_info->vcount-1)) ? 0 : k;
     REAL *s1 = v_info->sides[0][k-1],*s2 = v_info->sides[0][kk];
     REAL denom;

     ss1 = SDIM_dot(s1,s1);
     ss2 = SDIM_dot(s2,s2);
     s1s2 = SDIM_dot(s1,s2);
     
     denom = sqrt(ss1*ss2 - s1s2*s1s2);
   
     for ( i = 0 ; i < SDIM ; i++ )
     { REAL ddefds1,ddefds2,dads1,dads2;
       ddefds1 = (s2[i] - s1s2/ss1*s1[i])/denom;
       ddefds2 = (s1[i] - s1s2/ss2*s2[i])/denom;
       dads1 = (s1[i]*ss2 - s1s2*s2[i])/denom/6;
       dads2 = (s2[i]*ss1 - s1s2*s1[i])/denom/6;

       dgc[k][i] += ddefds1/area - deficit/area/area*dads1;
       dgc[0][i] -= ddefds1/area - deficit/area/area*dads1;
       dgc[kk?k+1:1][i] += ddefds2/area - deficit/area/area*dads2;
       dgc[0][i] -= ddefds2/area - deficit/area/area*dads2;

       ddef[k][i] += ddefds1;
       ddef[0][i] -= ddefds1;
       ddef[kk?k+1:1][i] += ddefds2;
       ddef[0][i] -= ddefds2;

       darea[k][i] += dads1;
       darea[0][i] -= dads1;
       darea[kk?k+1:1][i] += dads2;
       darea[0][i] -= dads2;
     }
  }


  /* other part */
    if ( v_info->vcount > MAXV )
      ddss11 = dmatrix3(16*pairs,SDIM,SDIM);
    else memset((char*)ddss11[0][0],0,sizeof(REAL)*16*pairs*MAXCOORD*MAXCOORD);
    ddss12 = ddss11 + pairs; ddss21 = ddss12 + pairs; ddss22 = ddss21 + pairs;
    ddAdv1dv1 = ddss22 + pairs; ddAdv1dv2 = ddAdv1dv1 + pairs;
    ddAdv2dv1 = ddAdv1dv2 + pairs; ddAdv2dv2 = ddAdv2dv1 + pairs;
    dvnorm1 = ddAdv2dv2 + pairs; dvnorm2 = dvnorm1 + pairs;
    dfdv1 = dvnorm2 + pairs; dfdv2 = dfdv1 + pairs;

    /* first, some more common terms */
    for ( k = 0 ; k < pairs ; k++ )
    { s1 = s[k]; s2 = s[(k+1==pairs)?0:k+1];
      for ( i = 0 ; i < SDIM ; i++ )
      { ddss11[k][i][i] = 2*s2s2[k];
        ddss12[k][i][i] = -2*s1s2[k];
        ddss21[k][i][i] = -2*s1s2[k];
        ddss22[k][i][i] = 2*s1s1[k];
        for ( j = 0 ; j < SDIM ; j++ )
        { ddss11[k][i][j] -= 2*s2[i]*s2[j];
          ddss12[k][i][j] += 4*s1[i]*s2[j] - 2*s2[i]*s1[j];
          ddss21[k][i][j] += 4*s2[i]*s1[j] - 2*s1[i]*s2[j];
          ddss22[k][i][j] -= 2*s1[i]*s1[j];

          ddAdv1dv1[k][i][j] = -0.125/a[k]/a[k]*dAdv1[k][i]*ds1[k][j]
                      + 0.125/a[k]*ddss11[k][i][j];
          ddAdv1dv2[k][i][j] = -0.125/a[k]/a[k]*dAdv1[k][i]*ds2[k][j]
                      + 0.125/a[k]*ddss12[k][i][j];
          dfdv1[k][i][j] = -ddAdv1dv1[k][i][j] - ddAdv1dv2[k][i][j];
          ddAdv2dv1[k][i][j] = -0.125/a[k]/a[k]*dAdv2[k][i]*ds1[k][j]
                      + 0.125/a[k]*ddss21[k][i][j];
          ddAdv2dv2[k][i][j] = -0.125/a[k]/a[k]*dAdv2[k][i]*ds2[k][j]
                      + 0.125/a[k]*ddss22[k][i][j];
          dfdv2[k][i][j] = -ddAdv2dv1[k][i][j] - ddAdv2dv2[k][i][j];
        }
        dffdv1[k][i] = 2*SDIM_dot(dfdv1[k][i],dAdv);
        dffdv2[k][i] = 2*SDIM_dot(dfdv2[k][i],dAdv);
      }

      if ( variety == PLAIN_SQ ) continue;
      dvnorm1[k][0][1] = -0.5*s2[2]/3;
      dvnorm1[k][0][2] =  0.5*s2[1]/3;
      dvnorm1[k][1][0] =  0.5*s2[2]/3;
      dvnorm1[k][1][2] = -0.5*s2[0]/3;
      dvnorm1[k][2][0] = -0.5*s2[1]/3;
      dvnorm1[k][2][1] =  0.5*s2[0]/3;
      dvnorm2[k][0][1] =  0.5*s1[2]/3;
      dvnorm2[k][0][2] = -0.5*s1[1]/3;
      dvnorm2[k][1][0] = -0.5*s1[2]/3;
      dvnorm2[k][1][2] =  0.5*s1[0]/3;
      dvnorm2[k][2][0] =  0.5*s1[1]/3;
      dvnorm2[k][2][1] = -0.5*s1[0]/3;
      for ( i = 0 ; i < SDIM ; i++ )
      { dfndv1[k][i] = SDIM_dot(dfdv1[k][i],vnorm);
        dfndv1[k][i] += SDIM_dot(dAdv,dvnorm1[k][i]);
        dfndv2[k][i] = SDIM_dot(dfdv2[k][i],vnorm);
        dfndv2[k][i] += SDIM_dot(dAdv,dvnorm2[k][i]);
        dnndv1[k][i] = 2*SDIM_dot(vnorm,dvnorm1[k][i]);
        dnndv2[k][i] = 2*SDIM_dot(vnorm,dvnorm2[k][i]);
      }
    }

  /* now, the actual gradients */

  if ( fn != 0.0 )
    for ( k = 0 ; k < pairs ; k++ )
    { 
      for ( i = 0 ; i < SDIM ; i++ )
      { 
        g = dffdv1[k][i]/fn - ff/fn/fn*dfndv1[k][i];
        dhh0[k+1][i] += g/2;
        dhh0[0][i] -= g/2;

        g = dffdv2[k][i]/fn - ff/fn/fn*dfndv2[k][i];
        dhh0[(k+1==pairs)?1:k+2][i] += g/2;
        dhh0[0][i] -= g/2;

      }
    }


#ifdef APPROX_LEVINE
  for ( k = 0 ; k < v_info->vcount ; k++ )
   for ( i = 0 ; i < SDIM ; i++ )
   { REAL droot = root==0.0 ? 0.0 : 0.5/root*(2*hh0*dhh0[k][i] - dgc[k][i]);
     REAL dkappa1 = dhh0[k][i] + droot;
     REAL dkappa2 = dhh0[k][i] - droot;
     v_info->grad[k][i] += coeff*ddef[k][i] 
      + (2*hh0*dhh0[k][i]-dgc[k][i])/levine_a*levine_t*(1+2*hh0*levine_t)*area
         + (hh0*hh0 - gc)/levine_a*levine_t*(2*dhh0[k][i]*levine_t)*area
         + (hh0*hh0 - gc)/levine_a*levine_t*(1 + 2*hh0*levine_t)*darea[k][i];
   } 
#else
  for ( k = 0 ; k < v_info->vcount ; k++ )
   for ( i = 0 ; i < SDIM ; i++ )
   { REAL droot = root==0.0 ? 0.0 : 0.5/root*(2*hh0*dhh0[k][i] - dgc[k][i]);
     REAL dkappa1 = dhh0[k][i] + droot;
     REAL dkappa2 = dhh0[k][i] - droot;
     v_info->grad[k][i] += coeff*ddef[k][i] 
         + (
           droot/2/levine_a*
               (log(1-kappa2*levine_t)-log(1-kappa1*levine_t))*area
         + root/2/levine_a*
               (1/(1-kappa2*levine_t)*(-dkappa2*levine_t)
                    -1/(1-kappa1*levine_t)*(-dkappa1*levine_t))*area
         + root/2/levine_a*
               (log(1-kappa2*levine_t)-log(1-kappa1*levine_t))*darea[k][i]);
   } 
#endif

  if ( mode == METHOD_GRADIENT )  
    return energy;

  /* No hessian yet */
  return energy;
}

REAL levine_energy_value(v_info)
struct qinfo *v_info;
{ return levine_energy_all(v_info,METHOD_VALUE);
}

REAL levine_energy_grad(v_info)
struct qinfo *v_info;
{ return levine_energy_all(v_info,METHOD_GRADIENT);
}


