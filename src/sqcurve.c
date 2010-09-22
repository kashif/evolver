/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/**********************************************************************
*
*  File: sqcurve.c
*
*  Purpose: Does calculations needed for including square curvature
*              in energy. Linear model only.
*/

#include "include.h"
REAL total_sqcurve;  /* total square curvature integral */
REAL total_length;

int h0_flag; /* set to use (H - H_0)^2 */
REAL h0_value;  /* value of H_0 */

REAL selfsim_coeff; /* self similarity */

/***********************************************************************
*
*  Function: sqcurve_energy_init()
*
*  Purpose: Initializes data structures for square curvature.
*           Call before doing facet loop in calc_energy or calc_force.
*/

void sqcurve_energy_init()
{ int k;
  int eltype;

  total_sqcurve = 0.0;
  total_length  = 0.0;
  if ( web.representation != SOAPFILM ) return; /* not needed for string */

  v_curve = (struct v_curve_t *)temp_calloc(web.skel[VERTEX].max_ord+1,
                         sizeof(struct v_curve_t));

  /* see if using (H - H_0)^2 adjustment */
  h0_flag = 0;
  h0_attr = find_extra("h_zero",&eltype);
  if ( h0_attr >= 0 )
    h0_flag = H0_IN_ATTR; 
  else
  { k = lookup_global("h_zero");
    if ( k >= 0 ) 
    { h0_flag = H0_IN_GLOBAL;
      h0_value = globals(k)->value.real;
    }
  }
  if ( self_similar_flag )
  { int param = lookup_global(SELFSIM_NAME);
    if ( param < 0 ) /* missing, so add */
    { param = add_global(SELFSIM_NAME);
      globals(param)->value.real = 1.0;  /* default */
      globals(param)->flags |=  ORDINARY_PARAM | RECALC_PARAMETER | ALWAYS_RECALC;
    }
    selfsim_coeff = globals(param)->value.real/6; /* area and volume factor */
    if ( h0_flag == 0 ) { h0_flag = H0_IN_GLOBAL; h0_value = 0.0; }
  }
}

/***********************************************************************
*
*  Function: sqcurve_force_init()
*
*  Purpose: Initializes data structures for square curvature.
*              Call before doing facet loop in calc_energy or calc_force.
*/

void sqcurve_force_init()
{ int k;
  int eltype;
  
  if ( web.representation != SOAPFILM ) return; /* not needed for string */

  v_curve = (struct v_curve_t *)temp_calloc(web.skel[VERTEX].max_ord+1,
                         sizeof(struct v_curve_t));
  e_curve = (struct e_curve_t *)temp_calloc(web.skel[EDGE].max_ord+1,
                         sizeof(struct e_curve_t));

  /* see if using (H - H_0)^2 adjustment */
  h0_flag = 0;
  h0_attr = find_extra("h_zero",&eltype);
  if ( h0_attr >= 0 )
    h0_flag = H0_IN_ATTR; 
  else
  { k = lookup_global("h_zero");
    if ( k >= 0 ) 
    { h0_flag = H0_IN_GLOBAL;
      h0_value = globals(k)->value.real;
    }
  }

  if ( self_similar_flag )
  { int param = lookup_global(SELFSIM_NAME);
    if ( param < 0 ) /* missing, so add */
    { param = add_global(SELFSIM_NAME);
      globals(param)->value.real = 1.0;  /* default */
      globals(param)->flags |=  ORDINARY_PARAM | RECALC_PARAMETER;
    }
    selfsim_coeff = globals(param)->value.real/6; /* area and volume factor */
    if ( h0_flag == 0 ) { h0_flag = H0_IN_GLOBAL; h0_value = 0.0; }
  }
}

/********************************************************************
*
*  Function: sqcurve_energy()
*
*  Purpose:  Does square curvature energy calculation for a facet.
*
*/

void sqcurve_energy(v_id,side)
vertex_id *v_id;  /* vertex list for facet */
REAL (*side)[MAXCOORD];  /* side vectors */
{ 
  REAL t1t1,t1t2,t2t2;
  REAL det;
  struct v_curve_t *vc[FACET_VERTS];
  int i,j;
  REAL area;

  t1t1 = SDIM_dot(side[0],side[0]);
  t1t2 = SDIM_dot(side[0],side[1]);
  t2t2 = SDIM_dot(side[1],side[1]);

  det = t1t1*t2t2 - t1t2*t1t2;

  area = sqrt(det)/2;
  for ( i = 0 ; i < FACET_VERTS ; i++ )
  { vc[i] = v_curve + loc_ordinal(v_id[i]);
    vc[i]->area += area;
  }
  if ( boundary_curvature_flag ) /* apportion area differently */
  { int fixcount = 0;
    for ( i = 0 ; i < FACET_VERTS ; i++ )
      if ( get_vattr(v_id[i]) & (BOUNDARY|FIXED) ) fixcount++;
    for ( i = 0 ; i < FACET_VERTS ; i++ )
    { vc[i] = v_curve+loc_ordinal(v_id[i]);
      if ( !(get_vattr(v_id[i]) & (BOUNDARY|FIXED)) )
        vc[i]->a += 3*area/(3-fixcount);
    }
  }

  if ( area > 0.0 )
    for ( i = 0 ; i < SDIM ; i++ )
    { vc[0]->force[i] -= (t2t2*side[0][i]-t1t2*side[1][i])/4/area;
      vc[1]->force[i] += (t2t2*side[0][i]-t1t2*side[1][i])/4/area;
      vc[2]->force[i] += (t1t1*side[1][i]-t1t2*side[0][i])/4/area;
      vc[1]->force[i] -= (t1t1*side[1][i]-t1t2*side[0][i])/4/area;
    }

  if ( effective_area_flag || normal_curvature_flag || div_normal_curvature_flag 
            || self_similar_flag || h0_flag ) 
  /* accumulate normal vector at each vertex */
  { REAL normal[MAXCOORD];
    cross_prod(side[0],side[1],normal);
    for ( i = 0 ; i < 3 ; i ++ )
      for ( j = 0 ; j < SDIM ; j++ )
        vc[i]->normal[j] += normal[j];
  }
}


/************************************************************************
*
*  Function: sqcurve_force()
*
*  Purpose:  Does square curvature force calculation for a facet.
*
*/

void sqcurve_force(v_id,e_id,side)
vertex_id *v_id; /* vertex list of facet */
edge_id *e_id;     /* edge list */
REAL (*side)[MAXCOORD];  /* side vectors */
{ 
  REAL det;
  struct v_curve_t *vc[FACET_VERTS];
  int i,j,k;
  REAL force[FACET_VERTS][MAXCOORD];
  REAL tt[FACET_VERTS][FACET_VERTS]; /* side dot products */
  REAL area;
  struct e_curve_t *ec[FACET_EDGES];
  int fixcount=0;

  for ( j = 0 ; j < FACET_VERTS ; j++ )
     for ( k = 0 ; k <= j ; k++ )
        tt[j][k] = tt[k][j] = SDIM_dot(side[j],side[k]);

  det = tt[0][0]*tt[1][1] - tt[0][1]*tt[0][1];

  area = sqrt(det)/2;
  for ( i = 0 ; i < FACET_VERTS ; i++ )
     { vc[i] = v_curve + loc_ordinal(v_id[i]);
        vc[i]->area += area;
     }
  if ( boundary_curvature_flag ) /* apportion area differently */
     { fixcount = 0;
        for ( i = 0 ; i < FACET_VERTS ; i++ )
          if ( get_vattr(v_id[i]) & (BOUNDARY|FIXED) ) fixcount++;
        for ( i = 0 ; i < FACET_VERTS ; i++ )
          if ( !(get_vattr(v_id[i]) & (BOUNDARY|FIXED)) )
             vc[i]->a += 3*area/(3-fixcount);
     }
  for ( i = 0 ; i < FACET_EDGES ; i++ )
     ec[i] = e_curve + loc_ordinal(e_id[i]);

  memset((char*)force,0,sizeof(force));
  for ( j = 0 ; j < FACET_VERTS ; j++ )
  { int i1 = (j+1)%FACET_VERTS;
    int i2 = (j+2)%FACET_VERTS;
    for ( i = 0 ; i < SDIM ; i++ )
      force[j][i] = (tt[i1][i1]*side[i2][i] - tt[i1][i2]*side[i1][i])/4/area;
   }

  /* first and second derivatives at vertices */
  for ( i = 0 ; i < FACET_VERTS ; i++ )
     { int ii = (i+1)%FACET_VERTS; /* opposite side from vertex i */
        for ( j = 0 ; j < SDIM ; j++ )
          vc[i]->force[j] += force[i][j];
        if ( boundary_curvature_flag && (fixcount != 3) )
         for ( j = 0 ; j < SDIM ; j++ )
          vc[i]->star_force[j] += 3*force[i][j]/(3-fixcount);
        for ( j = 0 ; j < SDIM ; j++ )
          { vc[i]->deriv2[j][j] +=  tt[ii][ii]/4/area;
             for ( k = 0 ; k < SDIM ; k++ )
                vc[i]->deriv2[j][k] -= (force[i][j]*force[i][k]+0.25*side[ii][j]
                                                *side[ii][k])/area;
          }
     }

  /* now first and second derivatives on edges */
  for ( i = 0 ; i < FACET_EDGES ; i++ )
  { int i1 =  (i+1)%FACET_EDGES;
    int i2 =  (i+2)%FACET_EDGES;
    for ( j = 0 ; j < SDIM ; j++ )
    { ec[i]->deriv2[j][j] += tt[i1][i2]/4/area;
      for ( k = 0 ; k < SDIM ; k++ )
        if ( inverted(e_id[i]) )
           ec[i]->deriv2[k][j] += (-side[i1][j]*side[i2][k]/2
                                   + side[i2][j]*side[i1][k]/4
                                   -force[i1][j]*force[i][k])/area;
         else
           ec[i]->deriv2[j][k] += (-side[i1][j]*side[i2][k]/2
                                   + side[i2][j]*side[i1][k]/4
                                   -force[i1][j]*force[i][k])/area;
      if ( boundary_curvature_flag 
                     && (get_vattr(v_id[i2]) & (FIXED|BOUNDARY)) )
      { if ( inverted(e_id[i]) )
        { ec[i]->aderiv[0][j] += 0.5*force[i1][j];
          ec[i]->aderiv[1][j] += 0.5*force[i][j];
        }
        else
        { ec[i]->aderiv[0][j] += 0.5*force[i][j];
          ec[i]->aderiv[1][j] += 0.5*force[i1][j];
        }
      }

      if ( inverted(e_id[i]) )
      { ec[i]->deriv[0][j] += force[i1][j];
        ec[i]->deriv[1][j] += force[i][j];
      }
      else
      { ec[i]->deriv[0][j] += force[i][j];
        ec[i]->deriv[1][j] += force[i1][j];
      }
    }
    if ( self_similar_flag )
    { REAL cp[MAXCOORD];
      int ii;
      cross_prod(get_coord(get_edge_tailv(e_id[i])),
                            get_coord(get_edge_headv(e_id[i])), cp);
      ii =  inverted(e_id[i1]) ? 0 : 1;
      for ( j = 0 ; j < SDIM ; j++ ) ec[i1]->volderiv[ii][j] += cp[j];
      ii =  inverted(e_id[i2]) ? 1 : 0;
      for ( j = 0 ; j < SDIM ; j++ ) ec[i2]->volderiv[ii][j] += cp[j];
    }
  } 
  
  if ( effective_area_flag || normal_curvature_flag || div_normal_curvature_flag
            || self_similar_flag || h0_flag ) 
  /* accumulate normal vector at each vertex */
    { REAL normal[MAXCOORD];
      cross_prod(side[0],side[1],normal);
      for ( i = 0 ; i < 3 ; i ++ )
         for ( j = 0 ; j < SDIM ; j++ )
            vc[i]->normal[j] += normal[j];
    }
 }

/*************************************************************************
*
*  function: sqcurve_energy_end()
*
*  purpose:  Convert square curvature data into energy.
*                Call at end of facet loop in calc_energy.
*
*/

void sqcurve_energy_end()
{
  vertex_id v_id;
  REAL modulus = globals(square_curvature_param)->value.real;
  REAL energy = 0.0;
  REAL denom;
  facet_id f_id;

  if ( web.representation == STRING )
  { binary_tree_add(web.total_energy_addends,total_sqcurve);
    return; /* rest not needed for string */
  }
 
  if ( div_normal_curvature_flag )
  { FOR_ALL_FACETS(f_id)
    { REAL *norm[3];  /* vertex normals */
      REAL side[3][3]; /* facet sides */
      REAL normal[3];  /* facet normal */
      int i;
      REAL h;
      facetedge_id fe;

      for ( i = 0, fe = get_facet_fe(f_id) ; i < 3 ; i++ )
        { v_id = get_fe_tailv(fe);
          get_fe_side(fe,side[i]);
          norm[i] = v_curve[loc_ordinal(v_id)].normal;
          fe = get_next_edge(fe);
        }
      cross_prod(side[0],side[1],normal);      
      denom = dot(normal,normal,3);
      for ( i = 0, h = 0.0 ; i < 3 ; i++ )
         h += triple_prod(normal,side[(i+1)%3],norm[i])/
                  sqrt(dot(norm[i],norm[i],3))/denom;
      energy += h*h*sqrt(denom)/2;
    }
    binary_tree_add(web.total_energy_addends,energy/4);/* mean factor */
    return;
  }

  FOR_ALL_VERTICES(v_id)
  { REAL venergy = vertex_sq_mean_curvature(v_id);
    int ordv = loc_ordinal(v_id);
    struct v_curve_t *vc = v_curve + ordv;

    energy += venergy*vc->a/3;

  }
  binary_tree_add(web.total_energy_addends,modulus*energy);

  temp_free((char*)v_curve);
  v_curve = NULL;
}

/*************************************************************************
*
*  function: vertex_sq_mean_curvature()
*
*  purpose: calculate squared mean curvature of given vertex
*              from data generated by sqcurve_energy.
*
*/

REAL vertex_sq_mean_curvature(v_id)
vertex_id v_id;
{ REAL h,venergy;
  ATTR attr = get_vattr(v_id);
  int ordv = loc_ordinal(v_id);
  struct v_curve_t *vc;
  REAL denom,f;
  REAL area; /* curvature normalization area */
  REAL h0_val=0.0;

  if ( !v_curve ) 
     sqcurve_method_init(METHOD_VALUE,NULL);
     
  vc = v_curve + ordv;
  if ( (attr & (FIXED|BOUNDARY)) ) return 0.0;
  if ( vc->area == 0.0 ) return 0.0;

  switch ( h0_flag )
  { case H0_IN_GLOBAL: h0_val = h0_value; break;
    case H0_IN_ATTR:   h0_val = *VREAL(v_id,h0_attr); break;
  }
  if ( !boundary_curvature_flag )
     { vc->a = vc->area; area = vc->area/3; } 
  else { area = vc->area/3; }
  if ( attr & CONSTRAINT )
  {
    conmap_t* conmap = get_v_constraint_map(v_id);
    int j,oncount = 0;
    struct constraint *con[MAXCONPER];
    REAL perp[MAXCOORD];

    for ( j = 1 ; j <= (int)conmap[0] ; j++ )
      if ( conmap[j] & CON_HIT_BIT ) 
         con[oncount++] = get_constraint(conmap[j]);

    constr_proj(TANGPROJ,oncount,con,get_coord(v_id),
                            vc->force,perp,NULL,NO_DETECT,v_id);
    for ( j = 0 ; j < SDIM ; j++ )
       vc->force[j] -= perp[j];
    if ( effective_area_flag || normal_curvature_flag )
    { constr_proj(TANGPROJ,oncount,con,get_coord(v_id),
                                 vc->normal,perp,NULL,NO_DETECT,v_id);
      for ( j = 0 ; j < SDIM ; j++ )
        vc->normal[j] -= perp[j];
    }
  } 
  if ( normal_curvature_flag )
  { f = SDIM_dot(vc->force,vc->force);
    denom = SDIM_dot(vc->force,vc->normal);
    if ( denom == 0.0 )  h = 0.0;
    else h = 3*f/denom;  /* mean, and  normal was twice area */
    if ( h0_flag ) h -= h0_val;
    if ( mean_curv_int_flag )
       venergy = h;
    else /* squared curvature */
       venergy = h*h;
  }
  else if ( h0_flag )
  { REAL term,sim;
    vc->h = h = SDIM_dot(vc->force,vc->normal)/
                          SDIM_dot(vc->normal,vc->normal)*3;
                            /* since vc->normal = 6*volgrad */
    term = h - h0_val;
    if ( self_similar_flag )
    { vc->vol = SDIM_dot(get_coord(v_id),vc->normal);
      sim = selfsim_coeff*vc->vol;
      term -= sim;
    }
    venergy = term*term;
   }
   else if ( mean_curv_int_flag ) /* unsquared curvature */
       venergy = sqrt(SDIM_dot(vc->force,vc->force))/2/area;
   else if ( effective_area_flag )
   { f = SDIM_dot(vc->force,vc->force);
     denom = SDIM_dot(vc->normal,vc->normal);
     if ( denom == 0.0 ) venergy = 0.0;
     else
        venergy = 9*f/denom;  /* 9 = 36/4 */
   }
   else /* plain squared curvature */
     venergy = SDIM_dot(vc->force,vc->force)/area/area/4;

  return venergy;
}

/*************************************************************************
*
*  function: sqcurve_force_end()
*
*  purpose:  Convert square curvature data into forces.
*
*/

void sqcurve_force_end()
{
  vertex_id v_id;
  edge_id e_id;
  int i,j;
  REAL modulus = globals(square_curvature_param)->value.real;
  REAL e,e1,e2,denom;
  REAL f[MAXCOORD];
  REAL fudge1,fudge2,fudge3; /* combinations of values */
  REAL fudge11=0.0,fudge12=0.0,fudge13=0.0; /* combinations of values */
  REAL fudge21=0.0,fudge22=0.0,fudge23=0.0; /* combinations of values */
  REAL h; /* curvature */
  REAL area; /* curvature normalization area */
  REAL a;     /* integral area allocation */

  if ( div_normal_curvature_flag ) 
      kb_error(1646,"Force not implemented yet for div_normal_curvature.\n",
         RECOVERABLE );

  if ( web.representation != SOAPFILM )
  { sqcurve_force_string_end();
    return;
  }

  FOR_ALL_VERTICES(v_id)
  { REAL *force = get_force(v_id);
    struct v_curve_t *vc = v_curve + loc_ordinal(v_id);
    ATTR attr = get_vattr(v_id);
    REAL  ad[MAXCOORD];
    REAL h0_val=0.0;

    switch ( h0_flag )
    { case H0_IN_GLOBAL: h0_val = h0_value; break;
      case H0_IN_ATTR:   h0_val = *VREAL(v_id,h0_attr); break;
    }

    if ( (attr & (FIXED|BOUNDARY))  ) continue;
    if ( vc->area == 0.0 ) continue;
    if ( !boundary_curvature_flag )
    { vc->a = vc->area; area = a = vc->a/3; } 
    else { a = vc->a/3; area = vc->area/3; }
    for ( i = 0 ; i < SDIM  ;i++ ) /* alloc area deriv */
        if ( boundary_curvature_flag ) ad[i] = vc->star_force[i]/3;
        else ad[i] = vc->force[i]/3;
    if ( attr & CONSTRAINT )
    { conmap_t *conmap = get_v_constraint_map(v_id);
      int oncount = 0;
      struct constraint *con[MAXCONPER];
      REAL perp[MAXCOORD];

      for ( j = 1 ; j <= (int)conmap[0] ; j++ )
      { if (conmap[j] & CON_HIT_BIT)
          con[oncount++] = get_constraint(conmap[j]);
      }

      constr_proj(TANGPROJ,oncount,con,get_coord(v_id),
                                 vc->force,perp,NULL,NO_DETECT,v_id);
      for ( j = 0 ; j < SDIM ; j++ )
        vc->force[j] -= perp[j];

      if ( effective_area_flag || normal_curvature_flag )
      { constr_proj(TANGPROJ,oncount,con,get_coord(v_id),
                              vc->normal,perp,NULL,NO_DETECT,v_id);
        for ( j = 0 ; j < SDIM ; j++ )
          vc->normal[j] -= perp[j];
      }
    }

    /* vertex self-second derivatives */
    if ( mean_curv_int_flag ) /* unsquared curvature */
    { e = sqrt(SDIM_dot(vc->force,vc->force));
      if ( e == 0.0 ) continue; /* no curvature */
        for ( i = 0 ; i < SDIM ; i++ )
          f[i] = SDIM_dot(vc->force,vc->deriv2[i])/e/2;
    }
    else if ( normal_curvature_flag )
    { e = SDIM_dot(vc->force,vc->force);
      denom = SDIM_dot(vc->force,vc->normal);
      if ( denom != 0.0 )
      { h = 3*e/denom;
        if ( h0_flag ) h -= h0_val;
        if ( mean_curv_int_flag )
        { fudge1 = 2/denom*vc->area;
          fudge2 = fudge1*e/denom/2;
          fudge3 = h/6;
          for ( i = 0 ; i < SDIM ; i++ )
            f[i] = fudge1*dot(vc->force,vc->deriv2[i],SDIM)
                   - fudge2*dot(vc->normal,vc->deriv2[i],SDIM)
                   + fudge3*(boundary_curvature_flag ? vc->star_force[i] :
                                  vc->force[i]);
/* Note: Convex Exemplar cc 6.2.1 chokes on SDIM_dot here */
        }
        else /* squared curvature */
        { fudge1 = 4*h/denom*vc->area;
          fudge2 = fudge1*e/denom/2;
          fudge3 = h*h/3;
          for ( i = 0 ; i < SDIM ; i++ )
            f[i] = fudge1*dot(vc->force,vc->deriv2[i],SDIM)
                  - fudge2*dot(vc->normal,vc->deriv2[i],SDIM)
                     + fudge3*(boundary_curvature_flag ? vc->star_force[i] :
                              vc->force[i]);
        }
      }
      else 
        for ( i = 0 ; i < SDIM ; i++ ) f[i] =0.0;
    }
    else if ( effective_area_flag )
    { e = SDIM_dot(vc->force,vc->force);
      denom = SDIM_dot(vc->normal,vc->normal);
      if ( denom != 0.0 )
        for ( i = 0 ; i < SDIM ; i++ )
          f[i] = 18*SDIM_dot(vc->force,vc->deriv2[i])/denom*a
                + 9*e/denom*ad[i];
      else
         for ( i = 0 ; i < SDIM ; i++ ) f[i] =0.0;
    }
    else /* squared curvature */
    { e = SDIM_dot(vc->force,vc->force)/vc->area*3.0/4;
      for ( i = 0 ; i < SDIM ; i++ )
        f[i] = (2*SDIM_dot(vc->force,vc->deriv2[i])
               - 4/3.0*e*vc->force[i])/vc->area*3.0/4;
    }

    if ( h0_flag  && !normal_curvature_flag)
    { REAL net,sim;
      REAL fd[MAXCOORD],simd[MAXCOORD];
      REAL aread[MAXCOORD];
      vc->norm = SDIM_dot(vc->normal,vc->normal);
      vc->f = SDIM_dot(vc->force,vc->normal);
      vc->h = h = vc->f/vc->norm*3;
      for ( i = 0 ; i < SDIM  ;i++ )
        fd[i] = SDIM_dot(vc->normal,vc->deriv2[i])/vc->norm*3;
      for ( i = 0 ; i < SDIM  ;i++ )
      { aread[i] = vc->force[i]/3;
        if ( boundary_curvature_flag ) ad[i] = vc->star_force[i]/3;
        else ad[i] = aread[i];
      }
      vc->term = h - h0_val;
      if ( self_similar_flag )
      { vc->vol = SDIM_dot(get_coord(v_id),vc->normal);
        sim = selfsim_coeff*vc->vol/area;
        vc->term -= sim/area;
        for ( i = 0 ; i < SDIM  ;i++ )
           { simd[i] = selfsim_coeff*vc->normal[i];
              force[i] -= modulus*2*vc->term* (- simd[i])*a;
           }
      }
      for ( i = 0  ; i < SDIM ; i++ )
      { net = 2*vc->term*fd[i]*a + vc->term*vc->term*ad[i];
        force[i] -= modulus*net;
      }
    }
    else
      for ( i = 0 ; i < SDIM ; i++ )
        force[i] -= modulus*f[i];
  }

  FOR_ALL_EDGES(e_id)
  {
     vertex_id headv = get_edge_headv(e_id);
     vertex_id tailv = get_edge_tailv(e_id);
     struct e_curve_t *ec = e_curve + loc_ordinal(e_id);
     struct v_curve_t *vc1 = v_curve + loc_ordinal(tailv);
     struct v_curve_t *vc2 = v_curve + loc_ordinal(headv);
     REAL *force1 = get_force(tailv); 
     REAL *force2 = get_force(headv); 
     REAL s[MAXCOORD],cross1[MAXCOORD],cross2[MAXCOORD];
     REAL denom1=0.0,denom2=0.0;
     REAL s0[MAXCOORD];
     REAL wa[MAXCOORD],wb[MAXCOORD],w[MAXCOORD],aw[MAXCOORD],vw[MAXCOORD];
     facetedge_id fe;
     facet_id f_id;

     
     if ( (vc1->area == 0.0) || (vc2->area == 0.0) ) continue;
     if ( mean_curv_int_flag )  /* unsquared curvature */
      { e1 = sqrt(SDIM_dot(vc1->force,vc1->force));
        e2 = sqrt(SDIM_dot(vc2->force,vc2->force));
      }
     else if ( normal_curvature_flag )
      {
         facetedge_id fe_a;
         facetedge_id fe_b;
         REAL sa[MAXCOORD],sb[MAXCOORD];

         fe = get_edge_fe(e_id);
         fe_a = get_prev_edge(fe);
         fe_b = get_prev_edge(get_next_facet(fe));
         get_edge_side(e_id,s0);

         get_edge_side(get_fe_edge(fe_a),sa);
         cross_prod(vc1->force,sa,cross1);
         if ( (assume_oriented_flag && inverted(get_fe_facet(fe_a))) 
 || (!assume_oriented_flag && triple_prod(vc1->normal,sa,s0) < 0.0 ) )
         { for ( i = 0 ; i < SDIM ; i++ ) cross1[i] = -cross1[i];
         }
         if ( fe_b != fe_a ) /* check for single edge on constraint */
           { get_edge_side(get_fe_edge(fe_b),sb);
              cross_prod(vc1->force,sb,s);
              if ( (assume_oriented_flag && inverted(get_fe_facet(fe_b))) 
 || (!assume_oriented_flag && triple_prod(vc1->normal,sb,s0) < 0.0 ) )
              {  for ( i = 0 ; i < SDIM ; i++ ) cross1[i] -= s[i];
              }
              else  for ( i = 0 ; i < SDIM ; i++ ) cross1[i] += s[i];
           }

         e1 = SDIM_dot(vc1->force,vc1->force);
         denom1 = SDIM_dot(vc1->force,vc1->normal);
         if ( denom1 != 0.0 )
         { h = 3*e1/denom1;
           switch ( h0_flag )
           { case H0_IN_GLOBAL: h -= h0_value; break;
             case H0_IN_ATTR:   h -= *VREAL(tailv,h0_attr); break;
           }
           if ( mean_curv_int_flag )
           { fudge11 = 2/denom1*vc1->area;
             fudge12 = fudge11*e1/denom1/2;
             fudge13 = h/6;
           }
           else
           { fudge11 = 4*h/denom1*vc1->area;
             fudge12 = fudge11*e1/denom1/2;
             fudge13 = h*h/3;
           }
         }
         else  fudge11 = fudge12 = fudge13 = 0.0; 

         e2 = SDIM_dot(vc2->force,vc2->force);
         denom2 = SDIM_dot(vc2->force,vc2->normal);
         if ( denom2 != 0.0 )
         { h = 3*e2/denom2;
           switch ( h0_flag )
           { case H0_IN_GLOBAL: h -= h0_value; break;
             case H0_IN_ATTR:   h -= *VREAL(headv,h0_attr); break;
           }
           if ( mean_curv_int_flag )
           { fudge21 = 2/denom2*vc2->area;
             fudge22 = fudge21*e2/denom2/2;
             fudge23 = h/6;
           }
           else
           { fudge21 = 4*h/denom2*vc2->area;
             fudge22 = fudge21*e2/denom2/2;
             fudge23 = h*h/3;
           }
         }
         else  fudge21 = fudge22 = fudge23 = 0.0; 

         cross_prod(vc2->force,sa,cross2);
         if ( (assume_oriented_flag && !inverted(get_fe_facet(fe_a))) 
 || (!assume_oriented_flag && triple_prod(vc2->normal,sa,s0) > 0.0 ) )
         { for ( i = 0 ; i < SDIM ; i++ ) cross2[i] = -cross2[i];
         }
         if ( fe_b != fe_a ) /* check for single edge on constraint */
         { cross_prod(vc2->force,sb,s);
           if ( (assume_oriented_flag && !inverted(get_fe_facet(fe_b))) 
 || (!assume_oriented_flag && triple_prod(vc2->normal,sb,s0) > 0.0 ) )
           {
              for ( i = 0 ; i < SDIM ; i++ ) cross2[i] -= s[i];
           }
           else  for ( i = 0 ; i < SDIM ; i++ ) cross2[i] += s[i];
         }
      }
     else if ( effective_area_flag )
      { 
         facetedge_id fe_a;
         facetedge_id fe_b;
         REAL sa[MAXCOORD],sb[MAXCOORD];

         fe = get_edge_fe(e_id);
         fe_a = get_prev_edge(fe);
         fe_b = get_prev_edge(get_next_facet(fe));
         get_edge_side(e_id,s0);

         get_edge_side(get_fe_edge(fe_a),sa);
         cross_prod(vc1->normal,sa,cross1);
         if ( (assume_oriented_flag && inverted(get_fe_facet(fe_a))) 
 || (!assume_oriented_flag &&  SDIM_dot(cross1,s0) < 0.0 ) )
           for ( i = 0 ; i < SDIM ; i++ ) cross1[i] = -cross1[i];
         if ( fe_b != fe_a ) /* check for single edge on constraint */
           { get_edge_side(get_fe_edge(fe_b),sb);
              cross_prod(vc1->normal,sb,s);
              if ( (assume_oriented_flag && inverted(get_fe_facet(fe_b))) 
 || (!assume_oriented_flag && SDIM_dot(s,s0) < 0.0 ) )
                 for ( i = 0 ; i < SDIM ; i++ ) cross1[i] -= s[i];
              else  for ( i = 0 ; i < SDIM ; i++ ) cross1[i] += s[i];
           }

         e1 = SDIM_dot(vc1->force,vc1->force);
         denom1 = SDIM_dot(vc1->normal,vc1->normal);
         e2 = SDIM_dot(vc2->force,vc2->force);
         denom2 = SDIM_dot(vc2->normal,vc2->normal);

         cross_prod(vc2->normal,sa,cross2);
         if ( (assume_oriented_flag && !inverted(get_fe_facet(fe_a))) 
 || (!assume_oriented_flag &&  SDIM_dot(cross2,s0) > 0.0 ) )
           for ( i = 0 ; i < SDIM ; i++ ) cross2[i] = -cross2[i];
         if ( fe_b != fe_a ) /* check for single edge on constraint */
           { cross_prod(vc2->normal,sb,s);
              if ( (assume_oriented_flag && !inverted(get_fe_facet(fe_b))) 
 || (!assume_oriented_flag &&  SDIM_dot(s,s0) > 0.0 ) )
                 for ( i = 0 ; i < SDIM ; i++ ) cross2[i] -= s[i];
              else  for ( i = 0 ; i < SDIM ; i++ ) cross2[i] += s[i];
           }
      }
     else  /* squared curvature */
      { e1 = SDIM_dot(vc1->force,vc1->force)/vc1->area*3.0/4;
         e2 = SDIM_dot(vc2->force,vc2->force)/vc2->area*3.0/4;
      }

     if ( !(get_vattr(tailv) & (FIXED|BOUNDARY)) )
       { /* force on head due to curvature at tail */
          if ( mean_curv_int_flag )  /* unsquared curvature */
             { if ( e1 == 0.0 ) break;
               for ( i = 0 ; i < SDIM ; i++ )
                  f[i] = SDIM_dot(vc1->force,ec->deriv2[i])/e1/2;
             }
          else if ( normal_curvature_flag )
           for ( i = 0 ; i < SDIM ; i++ )
             { f[i] = fudge11*SDIM_dot(vc1->force,ec->deriv2[i])
                  - fudge12*(cross1[i]+SDIM_dot(vc1->normal,ec->deriv2[i]))
                  + fudge13*ec->deriv[1][i] ;
             }
          else if ( effective_area_flag )
             for ( i = 0 ; i < SDIM ; i++ )
             { if ( denom1 != 0.0 )
                   f[i] = 6*SDIM_dot(vc1->force,ec->deriv2[i])/denom1*vc1->area
                     + 3*e1/denom1*ec->deriv[1][i] 
                     - 6*e1/denom1/denom1*cross1[i]*vc1->area;
               else f[i] = 0.0;
             }
          else  /* squared curvature */
           for ( i = 0 ; i < SDIM ; i++ )
             f[i] = (- 4/3.0*e1*ec->deriv[1][i]
             + 2*SDIM_dot(vc1->force,ec->deriv2[i]))/vc1->area*3.0/4;
          if ( h0_flag && !normal_curvature_flag )
           {REAL fd[MAXCOORD],ad[MAXCOORD],net;
             fe = get_edge_fe(e_id);
/* Gotta pay attention to wrapping
             wa = get_coord(get_fe_headv(get_next_edge(fe)));
             wb = get_coord(get_fe_headv(get_next_edge(get_next_facet(fe))));
*/
             get_edge_side(get_fe_edge(get_next_edge(fe)),wa);
             get_edge_side(get_fe_edge(get_next_edge(get_next_facet(fe))),wb);
             f_id = get_fe_facet(fe);
             if ( inverted(f_id) )
               for ( i = 0 ; i < SDIM ; i++ ) w[i] = wb[i] - wa[i];
             else for ( i = 0 ; i < SDIM ; i++ ) w[i] = wa[i] - wb[i];
             cross_prod(w,vc1->force,aw);
             cross_prod(w,vc1->normal,vw);
             for ( i = 0 ; i < SDIM ; i++ )
             { fd[i] = (SDIM_dot(vc1->normal,ec->deriv2[i])/vc1->norm
                           + aw[i]/vc1->norm - vc1->f*2*vw[i]/vc1->norm/vc1->norm)*3;
               ad[i] = ec->deriv[1][i]/3;
             }
             area = vc1->area/3;
             a = vc1->a/3;
             for ( i = 0 ; i < SDIM ; i++ )
             {
               net = 2*vc1->term*fd[i]*a 
                       + vc1->term*vc1->term*(ad[i]+ec->aderiv[1][i]/3);
               if ( self_similar_flag )
                 net += 2*vc1->term*(-selfsim_coeff*ec->volderiv[1][i])*a;
               force2[i] -= modulus*net;
             }
           }
          else
           for ( i = 0 ; i < SDIM ; i++ )
             force2[i] -= modulus*f[i];
       }

     if ( !(get_vattr(headv) & (FIXED|BOUNDARY)) )
       { /* force on tail due to curvature at head */
          if ( mean_curv_int_flag )  /* unsquared curvature */
           { if ( e2 == 0.0 ) break;
               for ( i = 0 ; i < SDIM ; i++ )
                  for ( j = 0,f [i] = 0.0 ; j < SDIM ; j++ )
                       f[i] += vc2->force[j]*ec->deriv2[j][i]/e2/2;
           }
          else if ( normal_curvature_flag )
           for ( i = 0 ; i < SDIM ; i++ )
             { f[i] = fudge23*ec->deriv[0][i] - fudge22*cross2[i];
               for ( j = 0 ; j < SDIM ; j++ )
                      f[i] += fudge21*vc2->force[j]*ec->deriv2[j][i]
                               - fudge22*vc2->normal[j]*ec->deriv2[j][i];
             }
          else if ( effective_area_flag )
           for ( i = 0 ; i < SDIM ; i++ )
             { if ( denom2 != 0.0 )
                 { f[i] =    3*e2/denom2*ec->deriv[0][i] 
                               - 6*e2/denom2/denom2*cross2[i]*vc2->area;
                   for ( j = 0 ; j < SDIM ; j++ )
                      f[i] += 6*vc2->force[j]*ec->deriv2[j][i]/denom2*vc2->area;
                 }
               else f[i] = 0.0;
             }
          else  /* squared curvature */
           for ( i = 0 ; i < SDIM ; i++ )
           { f[i] = -4/3.0*e2*ec->deriv[0][i]/vc2->area*3.0/4;
              for ( j = 0 ; j < SDIM ; j++ )
                  f[i] += 2*vc2->force[j]*ec->deriv2[j][i]/vc2->area*3.0/4;
           }

          if ( h0_flag && !normal_curvature_flag )
            {REAL fd[MAXCOORD],ad[MAXCOORD],net;
             fe = get_edge_fe(e_id);
             get_edge_side(get_fe_edge(get_next_edge(fe)),wa);
             get_edge_side(get_fe_edge(get_next_edge(get_next_facet(fe))),wb);
             f_id = get_fe_facet(fe);
             if ( inverted(f_id) )
               for ( i = 0 ; i < SDIM ; i++ ) w[i] = wb[i] - wa[i];
             else for ( i = 0 ; i < SDIM ; i++ ) w[i] = wa[i] - wb[i];
             cross_prod(vc2->force,w,aw);
             cross_prod(vc2->normal,w,vw);
             for ( i = 0 ; i < SDIM ; i++ )
             { for ( j = 0, fd[i] = 0.0 ; j < SDIM ; j++ )
                  fd[i] += vc2->normal[j]*ec->deriv2[j][i];
               fd[i] = (fd[i]/vc2->norm
                           + aw[i]/vc2->norm - vc2->f*2*vw[i]/vc2->norm/vc2->norm)*3;
               ad[i] = ec->deriv[0][i]/3;
             }
             area = vc2->area/3;
             a = vc2->a/3;
             for ( i = 0 ; i < SDIM ; i++ )
             {
               net = 2*vc2->term*fd[i]*a 
                       + vc2->term*vc2->term*(ad[i]+ec->aderiv[0][i]/3);
               if ( self_similar_flag )
                 net += 2*vc2->term*(-selfsim_coeff*ec->volderiv[0][i])*a;
               force1[i] -= modulus*net;
             }
           }
          else
           for ( i = 0 ; i < SDIM ; i++ )
             force1[i] -= modulus*f[i];
       }
  }

  temp_free((char*)e_curve);  e_curve = NULL;
  temp_free((char*)v_curve);  v_curve = NULL;
}

REAL curve_power;
int curve_power_param;

/************************************************************************
*
*  function: sqcurve_energy_string_init()
*
*/

void sqcurve_energy_string_init()
{
  curve_power_param = lookup_global(CURVE_POWER_NAME);
  if ( curve_power_param < 0 ) /* missing, so add */
  { curve_power_param = add_global(CURVE_POWER_NAME);
    globals(curve_power_param)->value.real = 2.0;  /* default */
    globals(curve_power_param)->flags |=  ORDINARY_PARAM;
  }
  curve_power = globals(curve_power_param)->value.real;
}

/************************************************************************
*
*  function: sqcurve_energy_string()
*
*  purpose:  Calculate square curvature energy for string model
*                Works locally vertex by vertex. Requires edges to be
*                on a facet, assumes two edges per vertex.
*
*/

void sqcurve_energy_string(v_id)
vertex_id v_id;
{
  REAL s1,s2,s1s2;  /* edge lengths */
  REAL side1[MAXCOORD],side2[MAXCOORD];
  edge_id e1;
  edge_id e2;
  REAL energy;
  REAL a1,a2;

  e2 = get_vertex_edge(v_id);
  e1 = get_next_tail_edge(e2);;
  e1 = inverse_id(e1);

  get_edge_side(e1,side1);
  get_edge_side(e2,side2);
  s1 = sqrt(SDIM_dot(side1,side1));
  s2 = sqrt(SDIM_dot(side2,side2));
  s1s2 = SDIM_dot(side1,side2);

  a1 = 1 - s1s2/s1/s2;
  a2 = (s1 + s2)/2;
  total_length += a2;
  if ( a1 <= 0.0 ) return;

  energy = pow(a1,curve_power/2)/pow(a2,curve_power-1);
  energy *= globals(square_curvature_param)->value.real;

  /* fudge factor to agree with def */
  total_sqcurve += 2*energy;

}

/************************************************************************
*
*  function: sqcurve_force_string()
*
*  purpose:  Calculate square curvature force for string model
*                Works locally vertex by vertex. Requires edges to be
*                on a facet, assumes two edges per vertex.
*
*/

void sqcurve_force_string(v_id)
vertex_id v_id;
{
  REAL s1,s2,s1s2;  /* edge lengths */
  REAL side1[MAXCOORD],side2[MAXCOORD];
  edge_id e1;
  edge_id e2;
  REAL *tforce,*vforce,*hforce;
  int  i;
  REAL c1,c2;
  REAL modulus = globals(square_curvature_param)->value.real;
  REAL a1,a2,energy;

  e2 = get_vertex_edge(v_id);
  e1 = get_next_tail_edge(e2);
  e1 = inverse_id(e1);
 
  vforce = get_force(v_id);
  tforce = get_force(get_edge_tailv(e1));
  hforce = get_force(get_edge_headv(e2));

  get_edge_side(e1,side1);
  get_edge_side(e2,side2);
  s1 = sqrt(SDIM_dot(side1,side1));
  s2 = sqrt(SDIM_dot(side2,side2));
  s1s2 = SDIM_dot(side1,side2);

  a1 = 1 - s1s2/s1/s2;
  if ( a1 <= 0.0 ) return;

  a2 = (s1 + s2)/2;
  energy = pow(a1,curve_power/2)/pow(a2,curve_power-1);

  c1 = -modulus*2*( curve_power/2*energy/a1*(s1s2/s1/s1/s1/s2)
            - (curve_power - 1)*energy/a2/s1/2);
  c2 = -modulus*2*( curve_power/2*energy/a1*(-1/s1/s2));
  for ( i = 0 ; i < SDIM ; i++ )
     { tforce[i] -= c1*side1[i] + c2*side2[i];
        vforce[i] += c1*side1[i] + c2*side2[i];
     }

  c1 = -modulus*2*( curve_power/2*energy/a1*(s1s2/s1/s2/s2/s2)
            - (curve_power - 1)*energy/a2/s2/2);
  c2 = -modulus*2*( curve_power/2*energy/a1*(-1/s1/s2));
  for ( i = 0 ; i < SDIM ; i++ )
     { hforce[i] += c2*side1[i] + c1*side2[i];
        vforce[i] -= c2*side1[i] + c1*side2[i];
     }
}

/*************************************************************
*
*  function: sqcurve_force_string_end()
*
*  purpose: nothing needed
*
*/

void sqcurve_force_string_end()
{ return;
}


/***************************************************************************/
/***************************************************************************/
/*   Stokes2D - method for calculating flat Willmore energy, which is 
 *              minimized for solutions of the 2D Stokes equation for
 *              steady state slow viscous flow. 
 *     Flat willmore energy for the graph of a function is the area integral 
 *     of the square of the trace of the Hessian of the function.
 *
 *  Boundary treatment: for use without boundary plateaus.  Multiplies
 *     Laplacian by two at fixed boundary points to simulate mirrored
 *     stream function.
 */
#define STOKES_TYPE_NAME "stokes_type"
#define STOKES_NONWALL 0
#define STOKES_SLIPWALL 1
#define STOKES_NONSLIPWALL 2
#define STOKES_VELOCITY_NAME "stokes_velocity"
REAL stokes2d_all ARGS(( struct qinfo *, int));
int stokes_type_attr;
int stokes_velocity_attr;

/* special mode value for Laplacian only */
#define STOKES_LAPLACIAN   0x4757737

void stokes2d_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{
 /* find parameters */ 
 stokes_type_attr = find_attribute(VERTEX,STOKES_TYPE_NAME);
 stokes_velocity_attr = find_attribute(VERTEX,STOKES_VELOCITY_NAME);
 if ( stokes_velocity_attr >= 0 )
 { if ( EXTRAS(VERTEX)[stokes_velocity_attr].array_spec.sizes[0] < 2 )
     kb_error(3674,"Dimension of stokes_velocity must be 2.\n",RECOVERABLE);
 } 
 else /* create */
 { int two = SDIM;
   stokes_velocity_attr = add_attribute(VERTEX,STOKES_VELOCITY_NAME,
       REAL_TYPE,1,&two,0,NULL);
 }

}

REAL stokes2d_all(v_info,mode)
struct qinfo *v_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */
{ int n,nn,maxn;
  REAL h,H,x1,y1,x2,y2,u1,u2,det,sumarea,value;
  REAL wedge; /* angle around vertex, to detect boundary or corner point for kludge */
  int stokes_type = (stokes_type_attr >= 0) ? *VINT(v_info->id,stokes_type_attr)
                        : STOKES_NONWALL ;
  REAL * stokes_velocity = (stokes_velocity_attr >= 0) ?
      VREAL(v_info->id,stokes_velocity_attr) : NULL ;
  REAL vgx,vgy,hinc;

  if ( stokes_type == STOKES_SLIPWALL )
	  return 0;

  /* Calculate H as gradient of integral of square of gradient */
  maxn = v_info->flags & INCOMPLETE_STAR ? v_info->vcount-2 : v_info->vcount-1;
  h = 0;
  sumarea = 0;
  wedge = 0;
  for ( n = 0 ; n < maxn ; n++ )  /* nth facet contribution */
  { int nn = (n == maxn-1) ? (v_info->flags & INCOMPLETE_STAR ? n+1 : 0) : n+1;
    x1 = v_info->sides[0][n][0];
    y1 = v_info->sides[0][n][1];
    u1 = v_info->sides[0][n][2];
    x2 = v_info->sides[0][nn][0];
    y2 = v_info->sides[0][nn][1];
    u2 = v_info->sides[0][nn][2];
    det = x1*y2 - x2*y1;  
    sumarea += fabs(det)/2;
    wedge += acos((x1*x2+y1*y2)/sqrt((x1*x1+y1*y1)*(x2*x2+y2*y2)));
/*  Not going to use the value, but the formula is here for easy reference.
    sqgradint = 
      1./2./fabs(det)*((y2*u1-y1*u2)*(y2*u1-y1*u2)+(x1*u2-x2*u1)*(x1*u2-x2*u1));
*/
    /* h is derivative wrt u0 only, with extra half factor since Laplacian
       is half of gradient of Dirichlet energy */
    hinc =  
      1./4./fabs(det)*(2*(y2*u1-y1*u2)*(-y2+y1)+2*(x1*u2-x2*u1)*(-x1+x2));
	h += hinc;
    if ( stokes_type == STOKES_NONSLIPWALL )
    { if ( stokes_velocity == NULL )
        kb_error(3833,"stokes_nonslipwall needs stokes_velocity attribute.\n",
           RECOVERABLE);
      /* contribution of virtual continuation across boundary, assuming
         boundary is a streamline.  Extended slope taken to be
         2*speed - slope */
      vgx = -2*stokes_velocity[1]*det - (y2*u1-y1*u2);
      vgy =  2*stokes_velocity[0]*det - (x1*u2-x2*u1);
      /* sqgradint just for reference here 
      sqgradint = 1./2./fabs(det)*(vgx*vgx + vgy*vgy);
      */
      hinc = 1./4./fabs(det)*(2*vgx*(y2-y1) + 2*vgy*(x1-x2));
	  h += hinc;
    }
    if ( (mode == METHOD_GRADIENT) || (mode == METHOD_HESSIAN) )
    { /* using grads to store grads of h */
      v_info->grad[0][2] +=
        1./4./fabs(det)*(2*(-y2+y1)*(-y2+y1)+2*(-x1+x2)*(-x1+x2));
      v_info->grad[n+1][2] += 
        1./4./fabs(det)*(2*(y2)*(-y2+y1)+2*(-x2)*(-x1+x2));
      v_info->grad[nn+1][2] += 
        1./4./fabs(det)*(2*(-y1)*(-y2+y1)+2*(x1)*(-x1+x2));
   
      if ( stokes_type == STOKES_NONSLIPWALL )
      { v_info->grad[0][2] +=
          1./4./fabs(det)*(2*(-y2+y1)*(-y2+y1)+2*(-x1+x2)*(-x1+x2));
        v_info->grad[n+1][2] += 
          1./4./fabs(det)*(2*(y2)*(-y2+y1)+2*(-x2)*(-x1+x2));
        v_info->grad[nn+1][2] += 
          1./4./fabs(det)*(2*(-y1)*(-y2+y1)+2*(x1)*(-x1+x2));
      }
	}

	
    /* note that hessian of h is identically 0 */

    /* NOTE: for slipping boundary, h = 0 (not implemented yet) */
   }
  if ( sumarea == 0.0 ) 
     return 0.0;  /* nothing there */

  if ( stokes_type == STOKES_NONSLIPWALL )
  { h /= 2;
	if ( (mode == METHOD_GRADIENT) || (mode == METHOD_HESSIAN) )
	  for ( n = 0 ; n < v_info->vcount ; n++ )
	    v_info->grad[n][2] /=2;
  }

  /* Kludge to try to get the Laplacian to come out more even on the bdry */
  if ( get_vattr(v_info->id) & FIXED )
  { 
    if ( wedge < 3.3 ) /* boundary point */
    {  sumarea *= 3./maxn*wedge/M_PI;
    }
  }

  /* normalize over area */
  H = h/(sumarea/3);
  if ( mode == STOKES_LAPLACIAN )
    return H;

  /* integral of H^2 */
  value = H*H*sumarea/3;  /* or value = h*h*3/sumarea */
  if ( mode == METHOD_VALUE ) 
    return value;

  if ( mode == METHOD_HESSIAN )
  { /* use h gradients while we still have them */
    for ( n = 0 ; n < v_info->vcount ; n++ )
      for ( nn = 0 ; nn < v_info->vcount ; nn++ )
        v_info->hess[n][nn][2][2] = 
             6/sumarea*v_info->grad[n][2]*v_info->grad[nn][2];
  }

  for ( n = 0 ; n < v_info->vcount ; n++ )
    v_info->grad[n][2] *= 6*h/sumarea;
  if ( mode == METHOD_GRADIENT ) 
    return value;

  /* nothing more to do for hessian, so ... */
  return value;
 
}

REAL stokes2d_value(v_info)
struct qinfo *v_info;
{ return stokes2d_all(v_info,METHOD_VALUE);
}

REAL stokes2d_grad(v_info)
struct qinfo *v_info;
{ return stokes2d_all(v_info,METHOD_GRADIENT);
}

REAL stokes2d_hess(v_info)
struct qinfo *v_info;
{ return stokes2d_all(v_info,METHOD_HESSIAN);
}

REAL stokes2d_laplacian(v_info)
struct qinfo *v_info;
{ return stokes2d_all(v_info,STOKES_LAPLACIAN);
}

