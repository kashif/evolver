/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/**********************************************************************
*
*  File: sqcurve2.c
*
*  Purpose: Does calculations needed for including square curvature
*              in energy. Linear model only.
*              Named quantity methods.
*/

#include "include.h"

static int h0_flag; /* set to use (H - H_0)^2 */
static REAL h0_value;  /* value of H_0 */
static REAL sq_mean_mix; /* proportion for mix_sq_mean_curvature */
static REAL selfsim_coeff; /* self similarity */

long sqcurve_init_timestamp; /* when last initialized */
long sqcurve_grad_init_timestamp; /* when last initialized */

/* local prototypes */
void sqcurve_energy_precalc ARGS((vertex_id *,REAL (*)[MAXCOORD],WRAPTYPE*));
void sqcurve_grad_precalc ARGS((vertex_id *,edge_id *,REAL (*)[MAXCOORD],
    WRAPTYPE*));

/***********************************************************************
*
*  Function: sqcurve_method_init()
*
*  Purpose: Initializes data structures for square curvature.
*/

void sqcurve_method_init(mode,mi)
int mode; /* METHOD_VALUE or METHOD_GRADIENT */
struct method_instance *mi;
{ int k,n;
  facet_id f_id;
  struct gen_quant_method *gm;
  MAT2D(x,MAXCOORD,MAXCOORD);
  vertex_id vv_id;
  WRAPTYPE wraps[FACET_VERTS];
  int eltype;

  if ( web.modeltype != LINEAR )
     kb_error(1758,"sq_mean_curvature method method only for LINEAR model.\n",
          RECOVERABLE);

 
  if ( everything_quantities_flag && square_curvature_flag)
     GEN_QUANT(sq_mean_curv_quantity_num)->modulus = 
        globals(square_curvature_param)->value.real;
  if ( mi )
  {
  if ( sq_mean_curvature_mi < 0 )
  { /* see what method indices correspond to what methods */
    for ( n=0,gm = basic_gen_methods ; gm->name[0] != ' ' ; gm++,n++ )
    { if ( stricmp(gm->name,"sq_mean_curvature") == 0 ) 
           sq_mean_curvature_mi = n;
      if ( stricmp(gm->name,"eff_area_sq_mean_curvature") == 0 ) 
           eff_area_sq_mean_curvature_mi = n;
      if ( stricmp(gm->name,"normal_sq_mean_curvature") == 0 ) 
           normal_sq_mean_curvature_mi = n;
      if ( stricmp(gm->name,"mix_sq_mean_curvature") == 0 ) 
           mix_sq_mean_curvature_mi = n;
    }
  }

  if ( mi->gen_method == eff_area_sq_mean_curvature_mi )
    if ( SDIM != 3 )
      kb_error(1759,"eff_area_sq_mean_curvature method only for 3D space.\n",
                RECOVERABLE);
  if ( mi->gen_method == normal_sq_mean_curvature_mi )
    if ( SDIM != 3 )
      kb_error(1760,"normal_sq_mean_curvature method only for 3D space.\n",
                RECOVERABLE);
  if ( mi->gen_method == normal_sq_mean_curvature_mi )
    if ( SDIM != 3 )
      kb_error(1450,"mix_sq_mean_curvature method only for 3D space.\n",
                RECOVERABLE);

  /* get mix coefficient */
  if ( mi->gen_method == normal_sq_mean_curvature_mi )
  { k = lookup_global("sq_mean_mix");
    if ( k >= 0 ) sq_mean_mix = globals(k)->value.real;
    else sq_mean_mix = 0.0;
  }
  } /* end if ( mi ) */

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

  /* necessary precalculations, if anything changed */
  if ( ( mode == METHOD_VALUE )
     && ( sqcurve_init_timestamp < global_timestamp || !v_curve) ) 
  {
     v_curve = (struct v_curve_t *)temp_calloc(web.skel[VERTEX].max_ord+1,
                         sizeof(struct v_curve_t));

     FOR_ALL_FACETS(f_id)
     { /* get side vectors */
        int i,j;
        vertex_id v_id[FACET_VERTS];
        facetedge_id fe_id;
        REAL side[FACET_EDGES][MAXCOORD];

        fe_id = get_facet_fe(f_id);
        for ( i = 0 ; i < FACET_EDGES ; i++ )
        {
          v_id[i] = get_fe_tailv(fe_id);
          fe_id = get_next_edge(fe_id);
        }
        get_facet_verts(f_id,x,wraps);  /* in tail order */
        for ( i = 0 ; i < FACET_EDGES ; i++ )
        { int ii = (i+1)%FACET_EDGES;
          for ( j = 0 ; j < SDIM ; j++ )
             side[i][j] = x[ii][j] - x[i][j];
        }
        sqcurve_energy_precalc(v_id,side,wraps);
     }

     sqcurve_init_timestamp = global_timestamp; 
  }

  if ( ( mode == METHOD_GRADIENT )
     && ( sqcurve_grad_init_timestamp < global_timestamp || !v_curve )  ) 
  {
     MAT2D(a,MAXCOORD,MAXCOORD);
     REAL af[MAXCOORD], gaf[MAXCOORD];
     MAT2D(grad,MAXCOORD,MAXCOORD);
     MAT2D(adft,MAXCOORD,MAXCOORD);
     MAT3D(seconds,MAXCOORD,MAXCOORD,MAXCOORD);

     v_curve = (struct v_curve_t *)temp_calloc(web.skel[VERTEX].max_ord+1,
                         sizeof(struct v_curve_t));
     e_curve = (struct e_curve_t *)temp_calloc(web.skel[EDGE].max_ord+1,
                         sizeof(struct e_curve_t));
      

     FOR_ALL_FACETS(f_id)
     { /* get side vectors */
        int i,j;
        vertex_id v_id[FACET_VERTS];
        edge_id e_id[FACET_VERTS];
        facetedge_id fe_id;
        REAL side[FACET_EDGES][MAXCOORD];

        fe_id = get_facet_fe(f_id);
        for ( i = 0 ; i < FACET_EDGES ; i++ )
        {
          v_id[i] = get_fe_tailv(fe_id);
          e_id[i] = get_fe_edge(fe_id);
          fe_id = get_next_edge(fe_id);
        }
        get_facet_verts(f_id,x,wraps);  /* in tail order */
        for ( i = 0 ; i < FACET_EDGES ; i++ )
        { int ii = (i+1)%FACET_EDGES;
          for ( j = 0 ; j < SDIM ; j++ )
             side[i][j] = x[ii][j] - x[i][j];
        }
        sqcurve_grad_precalc(v_id,e_id,side,wraps);
     }

    FOR_ALL_VERTICES(vv_id) 
    { struct v_curve_t *vc = v_curve + loc_ordinal(vv_id);
      if ( !boundary_curvature_flag ) vc->a = vc->area; 
      if ( (get_vattr(vv_id) & CONSTRAINT) && !(sqcurve_ignore_constr) )
      { conmap_t * conmap = get_v_constraint_map(vv_id);
        int i,j,oncount = 0;
         struct constraint *con[MAXCONPER];
         REAL perp[MAXCOORD];
         REAL dummy;

         for ( j = 1 ; j <= (int)conmap[0] ; j++ )
          if ( conmap[j] & CON_HIT_BIT )
                con[oncount++] = get_constraint(conmap[j]);

         /* stuff for gradient of projection operator */
         for ( j = 0 ; j < oncount ; j++ )
             eval_second(con[j]->formula,get_coord(vv_id),SDIM,&dummy,
                  grad[j],seconds[j],vv_id);

         /* construct matrix A */
         for ( i = 0 ; i < oncount ; i++ )
            for ( j = 0 ; j < oncount ; j++ )
              a[i][j] = SDIM_dot(grad[i],grad[j]);

         /* invert */
         mat_inv(a,oncount);

         mat_mult(a,grad,adft,oncount,oncount,SDIM);
         matvec_mul(adft,vc->force,af,oncount,SDIM);
         vec_mat_mul(af,grad,gaf,oncount,SDIM);
         for ( k = 0 ; k < SDIM ; k++ )
         { REAL sum = 0.0;
           for ( i = 0 ; i < oncount ; i++ )
             for ( j = 0 ; j < SDIM ; j++ )
             { sum += af[i]*seconds[i][k][j]*gaf[j];
               sum += vc->force[j]*seconds[i][k][j]*af[i];
             }
           vc->fpgradf[k] = -2*sum;
         } 
  
         constr_proj(TANGPROJ,oncount,con,get_coord(vv_id),
                                 vc->force,perp,NULL,NO_DETECT,NULLID);
         for ( j = 0 ; j < SDIM ; j++ )
           vc->force[j] -= perp[j];

         constr_proj(TANGPROJ,oncount,con,get_coord(vv_id),
                                 vc->normal,perp,NULL,NO_DETECT,NULLID);
         for ( j = 0 ; j < SDIM ; j++ )
                  vc->normal[j] -= perp[j];
       }

       if ( h0_flag ) 
       { vc->norm = SDIM_dot(vc->normal,vc->normal);
         vc->f = SDIM_dot(vc->force,vc->normal);
         vc->h = vc->f/vc->norm*3;
         switch ( h0_flag )
         { case H0_IN_GLOBAL: vc->term = vc->h - h0_value; break;
           case H0_IN_ATTR:   vc->term = vc->h - *VREAL(vv_id,h0_attr); break;
         }
       }
     }
     sqcurve_grad_init_timestamp = global_timestamp; 
  }
}

/********************************************************************
*
*  Function: sqcurve_energy_precalc()
*
*  Purpose:  Does square curvature energy calculation for a facet.
*
*/

void sqcurve_energy_precalc(v_id,side,wraps)
vertex_id *v_id;  /* vertex list for facet */
REAL (*side)[MAXCOORD];  /* side vectors */
WRAPTYPE *wraps; /* in case of symmetry group */
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
  { if ( web.symmetry_flag )
    { REAL f[FACET_VERTS][MAXCOORD];
      REAL wforce[MAXCOORD];  /* unwrapped forces */
      for ( i = 0 ; i < SDIM ; i++ )
      { f[0][i] = -(t2t2*side[0][i]-t1t2*side[1][i])/4/area;
         f[1][i] = (t2t2*side[0][i]-t1t2*side[1][i])/4/area;
         f[2][i] = (t1t1*side[1][i]-t1t2*side[0][i])/4/area;
         f[1][i] -= (t1t1*side[1][i]-t1t2*side[0][i])/4/area;
      }
      for ( i = 0 ; i < FACET_VERTS ; i++ )  /* vertex loop */
      { (*sym_form_pullback)(get_coord(v_id[i]),wforce,f[i],wraps[i]);
         for ( j = 0 ; j < SDIM ; j++ )
            vc[i]->force[j] += wforce[j];
      }
    }
    else for ( i = 0 ; i < SDIM ; i++ )
    { vc[0]->force[i] -= (t2t2*side[0][i]-t1t2*side[1][i])/4/area;
      vc[1]->force[i] += (t2t2*side[0][i]-t1t2*side[1][i])/4/area;
      vc[2]->force[i] += (t1t1*side[1][i]-t1t2*side[0][i])/4/area;
      vc[1]->force[i] -= (t1t1*side[1][i]-t1t2*side[0][i])/4/area;
    }
  }

  /* accumulate normal vector at each vertex;  should pullback */
  { REAL normal[MAXCOORD];
    cross_prod(side[0],side[1],normal);
    for ( i = 0 ; i < FACET_VERTS ; i ++ )
    { if ( web.symmetry_flag && wraps[i] )
      { REAL wnorm[MAXCOORD];
        (*sym_form_pullback)(get_coord(v_id[i]),wnorm,normal,wraps[i]);
        for ( j = 0 ; j < SDIM ; j++ )
        vc[i]->normal[j] += wnorm[j];
      }
      else
        for ( j = 0 ; j < SDIM ; j++ )
          vc[i]->normal[j] += normal[j];
    }
  }
}


/************************************************************************
*
*  Function: sqcurve_grad_precalc()
*
*  Purpose:  Does square curvature grad calculation for a facet.
*
*/

void sqcurve_grad_precalc(v_id,e_id,side,wraps)
vertex_id *v_id; /* vertex list of facet */
edge_id *e_id;     /* edge list */
REAL (*side)[MAXCOORD];  /* side vectors */
WRAPTYPE *wraps;
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
          force[j][i] = (tt[i1][i1]*side[i2][i] - tt[i1][i2]*side[i1][i])
                                    /4/area;
     }

  /* first and second derivatives at vertices; should add pullback */
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

  /* now first and second derivatives on edges; should add pullback */
  for ( i = 0 ; i < FACET_EDGES ; i++ )
  { 
    int i1 =  (i+1)%FACET_EDGES;
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
  
  /* accumulate normal vector at each vertex */
    { REAL normal[MAXCOORD];
      cross_prod(side[0],side[1],normal);
      for ( i = 0 ; i < 3 ; i ++ )
         for ( j = 0 ; j < SDIM ; j++ )
            vc[i]->normal[j] += normal[j];
    }
}

/***********************************************************************
*
*  Function: sqcurve_method_cleanup()
*
*  Purpose: Initializes data structures for square curvature.
*/

void sqcurve_method_cleanup()
{
  if ( v_curve ) { temp_free((char*)v_curve); v_curve = NULL; }
  if ( e_curve ) { temp_free((char*)e_curve); e_curve = NULL; }
}

/*************************************************************************
*
*  function: sqcurve_method_value()
*
*  purpose: Calculate squared mean curvature of given vertex
*           from precalculated data. Does normal and eff_area versions also.
*
*/

REAL sqcurve_method_value(v_info)
struct qinfo *v_info;
{
  vertex_id v_id = v_info->v[0];
  REAL h,venergy;
  ATTR attr = get_vattr(v_id);
  int ordv = loc_ordinal(v_id);
  struct v_curve_t *vc;
  REAL denom,f;
  REAL area; /* curvature normalization area */

  /* kludge check here, needed for info_only individual attributes */
  if ( sqcurve_init_timestamp < global_timestamp ) 
    sqcurve_method_init(METHOD_VALUE,METH_INSTANCE(v_info->method));

  if ( v_curve == NULL )
    sqcurve_method_init(METHOD_VALUE,NULL);

  vc = v_curve + ordv;
  if ((attr & BOUNDARY) && 
         !(METH_INSTANCE(v_info->method)->flags & IGNORE_CONSTR))
     return 0.0;
  if ((attr & FIXED) && !(METH_INSTANCE(v_info->method)->flags & IGNORE_FIXED))
     return 0.0;
  if ( vc->area == 0.0 ) return 0.0;

  if ( METH_INSTANCE(v_info->method)->gen_method != sq_mean_curvature_mi )
  { /* need to check facet orientation consistency */
    edge_id eid,starteid;
    eid = starteid = get_vertex_edge(v_id);
    do
    { facetedge_id fa,fb;
      fa = get_edge_fe(eid);
      fb = get_next_facet(fa);
      if ( (fa != fb) && 
               (inverted(get_fe_facet(fa))==inverted(get_fe_facet(fb))) )
      { sprintf(errmsg,
           "Inconsistent orientation of \nfacets %s and %s at vertex %s.\n",
            ELNAME(get_fe_facet(fa)),ELNAME1(get_fe_facet(fb)),
               ELNAME(eid));
        strcat(errmsg,"eff_area_sq_mean_curvature and normal_sq_mean_curvature need consistency.\n"); 
        kb_error(2183,errmsg,RECOVERABLE);
      }
      eid = get_next_tail_edge(eid); 
    } while ( !equal_element(eid,starteid) );
  }

  if ( !boundary_curvature_flag )
  { vc->a = vc->area; area = vc->area/3; } 
  else { area = vc->area/3; }
  if ( (attr & CONSTRAINT) && 
       !(METH_INSTANCE(v_info->method)->flags & IGNORE_CONSTR) )
  { conmap_t * conmap = get_v_constraint_map(v_id);
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

    constr_proj(TANGPROJ,oncount,con,get_coord(v_id),
                                 vc->normal,perp,NULL,NO_DETECT,v_id);
    for ( j = 0 ; j < SDIM ; j++ )
                  vc->normal[j] -= perp[j];
  }
  if ( METH_INSTANCE(v_info->method)->gen_method == 
                  normal_sq_mean_curvature_mi )
  { f = SDIM_dot(vc->force,vc->force);
    denom = SDIM_dot(vc->force,vc->normal);
    if ( denom == 0.0 )  h = 0.0;
    else h = 3*f/denom;  /* mean, and  normal was twice area */
    switch ( h0_flag )
    { case H0_IN_GLOBAL: h -= h0_value; break;
      case H0_IN_ATTR:   h -= *VREAL(v_id,h0_attr); break;
    }
    venergy = h*h;
  }
  else 
   if ( METH_INSTANCE(v_info->method)->gen_method==mix_sq_mean_curvature_mi )
   { /* normal_sq_mean part */
     f = SDIM_dot(vc->force,vc->force);
     denom = SDIM_dot(vc->force,vc->normal);
     if ( denom == 0.0 )  h = 0.0;
     else h = 3*f/denom;  /* mean, and  normal was twice area */

     /* add other part */
     vc->h = h = h*sq_mean_mix + SDIM_dot(vc->force,vc->normal)/
                          SDIM_dot(vc->normal,vc->normal)*3*(1-sq_mean_mix);

     switch ( h0_flag )
     { case H0_IN_GLOBAL: h -= h0_value; break;
       case H0_IN_ATTR:   h -= *VREAL(v_id,h0_attr); break;
     }
     venergy = h*h;
   }
   else if ( h0_flag )
   { REAL term=0.0,sim;
     vc->h = h = SDIM_dot(vc->force,vc->normal)/
                          SDIM_dot(vc->normal,vc->normal)*3;
                            /* since vc->normal = 6*volgrad */
     switch ( h0_flag )
     { case H0_IN_GLOBAL: term = h - h0_value; break;
       case H0_IN_ATTR:   term = h - *VREAL(v_id,h0_attr); break;
     }
     if ( self_similar_flag )
     { vc->vol = SDIM_dot(get_coord(v_id),vc->normal);
       sim = selfsim_coeff*vc->vol;
       term -= sim;
     }
     venergy = term*term;
   }
   else if ( METH_INSTANCE(v_info->method)->gen_method 
                 == eff_area_sq_mean_curvature_mi )
   { f = SDIM_dot(vc->force,vc->force);
     denom = SDIM_dot(vc->normal,vc->normal);
     if ( denom == 0.0 ) venergy = 0.0;
     else
        venergy = 9*f/denom;  /* 9 = 36/4 */
   }
   else /* plain squared curvature */
     venergy = SDIM_dot(vc->force,vc->force)/area/area/4;

   return venergy*vc->a/3;
}

/*************************************************************************
*
*  function: sqcurve_method_grad()
*
*  purpose:  Convert square curvature data into forces for a vertex.
*
*/

REAL sqcurve_method_grad(v_info)                
struct qinfo *v_info;
{
  vertex_id v_id = v_info->v[0];
  edge_id e_id,ee_id,eee_id;
  int i,j;
  REAL e,e1,e2,denom;
  REAL f[MAXCOORD];
  REAL fudge1,fudge2,fudge3; /* combinations of values */
  REAL fudge11=0.0,fudge12=0.0,fudge13=0.0; /* combinations of values */
  REAL fudge21=0.0,fudge22=0.0,fudge23=0.0; /* combinations of values */
  REAL h; /* curvature */
  REAL area; /* curvature normalization area */
  REAL a;     /* integral area allocation */
  struct v_curve_t *vc = v_curve + loc_ordinal(v_id);
  ATTR attr = get_vattr(v_id);
  REAL  ad[MAXCOORD];
  REAL venergy = 0.0;
  REAL *grad = v_info->grad[0]; 

  if ( div_normal_curvature_flag ) 
      kb_error(1761,"Force not implemented yet for div_normal_curvature.\n",
         RECOVERABLE );

  for ( i = 0 ; i < SDIM ; i++ ) grad[i] = 0.0;

  if ((attr & BOUNDARY) && !(METH_INSTANCE(v_info->method)->flags & IGNORE_CONSTR))
         return 0.0;
  if ((attr & FIXED) && !(METH_INSTANCE(v_info->method)->flags & IGNORE_FIXED))
         return 0.0;
  if ( vc->area == 0.0 ) return 0.0;
  if ( !boundary_curvature_flag ) {  area = a = vc->a/3; } 
  else { a = vc->a/3; area = vc->area/3; }
  for ( i = 0 ; i < SDIM  ;i++ ) /* alloc area deriv */
    if ( boundary_curvature_flag ) 
       ad[i] = vc->star_force[i]/3;
    else ad[i] = vc->force[i]/3;

  /* vertex self-second derivatives */
  if ( METH_INSTANCE(v_info->method)->gen_method==normal_sq_mean_curvature_mi )
  { e = SDIM_dot(vc->force,vc->force);
    denom = SDIM_dot(vc->force,vc->normal);
    if ( denom != 0.0 )
    {
       h = 3*e/denom;
       switch ( h0_flag )
       { case H0_IN_GLOBAL: h -= h0_value; break;
         case H0_IN_ATTR:   h -= *VREAL(v_id,h0_attr); break;
       }
       venergy = h*h;
       fudge1 = 4*h/denom*vc->area;
       fudge2 = fudge1*e/denom/2;
       fudge3 = h*h/3;
       for ( i = 0 ; i < SDIM ; i++ )
       f[i] = fudge1*SDIM_dot(vc->force,vc->deriv2[i])
                   - fudge2*SDIM_dot(vc->normal,vc->deriv2[i])
                   + fudge3*(boundary_curvature_flag ? vc->star_force[i] :
                        vc->force[i]);
    }
    else 
       for ( i = 0 ; i < SDIM ; i++ ) f[i] =0.0;
  }
  else
  if ( METH_INSTANCE(v_info->method)->gen_method==mix_sq_mean_curvature_mi )
  {
    /* normal_sq_mean part */
    e = SDIM_dot(vc->force,vc->force);
    denom = SDIM_dot(vc->force,vc->normal);
    h = 0.0;
    if ( denom != 0.0 )
    {
       h = 3*e/denom;
       switch ( h0_flag )
       { case H0_IN_GLOBAL: h -= h0_value; break;
         case H0_IN_ATTR:   h -= *VREAL(v_id,h0_attr); break;
       }

       fudge1 = 4/denom;
       fudge2 = fudge1*e/denom/2;

       fudge3 = h*h/3;
       for ( i = 0 ; i < SDIM ; i++ )
       f[i] = fudge1*SDIM_dot(vc->force,vc->deriv2[i])
                   - fudge2*SDIM_dot(vc->normal,vc->deriv2[i]);

    }
    else 
       for ( i = 0 ; i < SDIM ; i++ ) f[i] =0.0;

      /* add other part */
    {
      REAL net;
      REAL fd[MAXCOORD];
      REAL aread[MAXCOORD];
      for ( i = 0 ; i < SDIM  ;i++ )
        fd[i] = SDIM_dot(vc->normal,vc->deriv2[i])/vc->norm*3;
      for ( i = 0 ; i < SDIM  ;i++ )
      { aread[i] = vc->force[i]/3;
        if ( boundary_curvature_flag ) ad[i] = vc->star_force[i]/3;
        else ad[i] = aread[i];
      }
      h = (h*sq_mean_mix+(1-sq_mean_mix)*vc->term);  /* make combination */
      for ( i = 0  ; i < SDIM ; i++ )
      { net = 2*h*(f[i]*sq_mean_mix+(1-sq_mean_mix)*fd[i])*a + h*h*ad[i];
        grad[i] += net;
      }
    }
    venergy = h*h;
  }
  else if ( METH_INSTANCE(v_info->method)->gen_method == eff_area_sq_mean_curvature_mi )
  { e = SDIM_dot(vc->force,vc->force);
    denom = SDIM_dot(vc->normal,vc->normal);
    if ( denom != 0.0 )
    { for ( i = 0 ; i < SDIM ; i++ )
        f[i] = 18*SDIM_dot(vc->force,vc->deriv2[i])/denom*a
          + 9*e/denom*ad[i];
      venergy = 9*e/denom;  /* 9 = 36/4 */
    }
    else
       for ( i = 0 ; i < SDIM ; i++ ) f[i] =0.0;
  }
  else /* squared curvature */
  { e = SDIM_dot(vc->force,vc->force)/vc->area*3.0/4;
    for ( i = 0 ; i < SDIM ; i++ )
    { f[i] = (2*SDIM_dot(vc->force,vc->deriv2[i])
            - 4/3.0*e*vc->force[i])/vc->area*3.0/4;
      f[i] += vc->fpgradf[i]/vc->area*3.0/4;
    }
    venergy = e*3/vc->area/4;
  }

  if ( h0_flag  && 
    !(METH_INSTANCE(v_info->method)->gen_method==normal_sq_mean_curvature_mi) )
    {
      REAL net,sim;
      REAL fd[MAXCOORD],simd[MAXCOORD];
      REAL aread[MAXCOORD];
      for ( i = 0 ; i < SDIM  ;i++ )
        fd[i] = SDIM_dot(vc->normal,vc->deriv2[i])/vc->norm*3;
      for ( i = 0 ; i < SDIM  ;i++ )
      { aread[i] = vc->force[i]/3;
        if ( boundary_curvature_flag ) ad[i] = vc->star_force[i]/3;
        else ad[i] = aread[i];
      }
      if ( self_similar_flag )
      { vc->vol = SDIM_dot(get_coord(v_id),vc->normal);
        sim = selfsim_coeff*vc->vol/area;
        vc->term -= sim/area;
        for ( i = 0 ; i < SDIM  ;i++ )
        { simd[i] = selfsim_coeff*vc->normal[i];
          grad[i] += 2*vc->term* (- simd[i])*a;
        }
      }
      for ( i = 0  ; i < SDIM ; i++ )
      { net = 2*vc->term*fd[i]*a + vc->term*vc->term*ad[i];
        grad[i] += net;
      }
    }
  else
    for ( i = 0 ; i < SDIM ; i++ )
       grad[i] += f[i];
    

  ee_id = get_vertex_edge(v_id);
  eee_id = ee_id;
  if ( valid_id(eee_id) ) do
  {
    vertex_id headv;
    vertex_id tailv;
    struct e_curve_t *ec;
    struct v_curve_t *vc1;
    struct v_curve_t *vc2;
    REAL s[MAXCOORD],cross1[MAXCOORD],cross2[MAXCOORD];
    REAL denom1=0.0,denom2=0.0;
    REAL s0[MAXCOORD];
    REAL wa[MAXCOORD],wb[MAXCOORD],w[MAXCOORD],aw[MAXCOORD],vw[MAXCOORD];
    facetedge_id fe;
    facet_id f_id;
  
    if ( inverted(eee_id) ) e_id = inverse_id(eee_id);
    else e_id = eee_id;
    headv = get_edge_headv(e_id);
    tailv = get_edge_tailv(e_id);
    ec = e_curve + loc_ordinal(e_id);
    vc1 = v_curve + loc_ordinal(tailv);
    vc2 = v_curve + loc_ordinal(headv);
    
    if ( (vc1->area == 0.0) || (vc2->area == 0.0) ) continue;
    if ( METH_INSTANCE(v_info->method)->gen_method == normal_sq_mean_curvature_mi )
     { facetedge_id fe_a;
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
          for ( i = 0 ; i < SDIM ; i++ ) cross1[i] = -cross1[i];
        if ( fe_b != fe_a ) /* check for single edge on constraint */
        { get_edge_side(get_fe_edge(fe_b),sb);
          cross_prod(vc1->force,sb,s);
          if ( (assume_oriented_flag && inverted(get_fe_facet(fe_b)))
           || (!assume_oriented_flag && triple_prod(vc1->normal,sb,s0) < 0.0 ) )
             for ( i = 0 ; i < SDIM ; i++ ) cross1[i] -= s[i];
          else  for ( i = 0 ; i < SDIM ; i++ ) cross1[i] += s[i];
        }
  
        e1 = SDIM_dot(vc1->force,vc1->force);
        denom1 = SDIM_dot(vc1->force,vc1->normal);
        if ( denom1 != 0.0 )
        { h = 3*e1/denom1;
          switch ( h0_flag )
          { case H0_IN_GLOBAL: h -= h0_value; break;
            case H0_IN_ATTR:   h -= *VREAL(headv,h0_attr); break;
          }
          fudge11 = 4*h/denom1*vc1->area;
          fudge12 = fudge11*e1/denom1/2;
          fudge13 = h*h/3;
        }
        else  fudge11 = fudge12 = fudge13 = 0.0; 
  
        e2 = SDIM_dot(vc2->force,vc2->force);
        denom2 = SDIM_dot(vc2->force,vc2->normal);
        if ( denom2 != 0.0 )
        { h = 3*e2/denom2;
          switch ( h0_flag )
          { case H0_IN_GLOBAL: h -= h0_value; break;
            case H0_IN_ATTR:   h -= *VREAL(tailv,h0_attr); break;
          }
          fudge21 = 4*h/denom2*vc2->area;
          fudge22 = fudge21*e2/denom2/2;
          fudge23 = h*h/3;
        }
        else  fudge21 = fudge22 = fudge23 = 0.0; 
  
        cross_prod(vc2->force,sa,cross2);
        if ( (assume_oriented_flag && !inverted(get_fe_facet(fe_a)))
           || (!assume_oriented_flag && triple_prod(vc2->normal,sa,s0) > 0.0 ) )
          for ( i = 0 ; i < SDIM ; i++ ) cross2[i] = -cross2[i];
        if ( fe_b != fe_a ) /* check for single edge on constraint */
        { cross_prod(vc2->force,sb,s);
          if ( (assume_oriented_flag && !inverted(get_fe_facet(fe_b)))
           || (!assume_oriented_flag && triple_prod(vc2->normal,sb,s0) > 0.0 ) )
              for ( i = 0 ; i < SDIM ; i++ ) cross2[i] -= s[i];
          else  for ( i = 0 ; i < SDIM ; i++ ) cross2[i] += s[i];
        }
     }
    else if ( METH_INSTANCE(v_info->method)->gen_method 
                  == eff_area_sq_mean_curvature_mi )
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
  
    if ( inverted(eee_id) )
    { int tattr = get_vattr(tailv);
      if ( (!(tattr & FIXED) || 
                    (METH_INSTANCE(v_info->method)->flags & IGNORE_FIXED))
           && (!(tattr & BOUNDARY) || 
                    (METH_INSTANCE(v_info->method)->flags & IGNORE_CONSTR))
      )
      { /* force on head due to curvature at tail */
        if ( METH_INSTANCE(v_info->method)->gen_method 
                      == normal_sq_mean_curvature_mi )
          for ( i = 0 ; i < SDIM ; i++ )
          { f[i] = fudge11*SDIM_dot(vc1->force,ec->deriv2[i])
                 - fudge12*(cross1[i]+SDIM_dot(vc1->normal,ec->deriv2[i]))
                 + fudge13*ec->deriv[1][i] ;
          }
        else if (METH_INSTANCE(v_info->method)->gen_method
          ==eff_area_sq_mean_curvature_mi )
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
         if ( h0_flag && !(METH_INSTANCE(v_info->method)->gen_method 
                == normal_sq_mean_curvature_mi) )
          { REAL fd[MAXCOORD],net;
            fe = get_edge_fe(e_id);
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
              grad[i] += net;
            }
          }
         else
          for ( i = 0 ; i < SDIM ; i++ )
            grad[i] += f[i];
         }
      }
      else /* not inverted */
      if ( (!(get_vattr(headv) & FIXED) || 
                    (METH_INSTANCE(v_info->method)->flags & IGNORE_FIXED))
     && (!(get_vattr(headv) & BOUNDARY) || 
                    (METH_INSTANCE(v_info->method)->flags & IGNORE_CONSTR))
      )
      { /* force on tail due to curvature at head */
         if ( METH_INSTANCE(v_info->method)->gen_method == normal_sq_mean_curvature_mi )
          for ( i = 0 ; i < SDIM ; i++ )
          { f[i] = fudge23*ec->deriv[0][i] - fudge22*cross2[i];
            for ( j = 0 ; j < SDIM ; j++ )
              f[i] += fudge21*vc2->force[j]*ec->deriv2[j][i]
                              - fudge22*vc2->normal[j]*ec->deriv2[j][i];
         }
         else if ( METH_INSTANCE(v_info->method)->gen_method == eff_area_sq_mean_curvature_mi )
          for ( i = 0 ; i < SDIM ; i++ )
          { if ( denom2 != 0.0 )
            { f[i] = 3*e2/denom2*ec->deriv[0][i] 
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
  
         if ( h0_flag &&
            !(METH_INSTANCE(v_info->method)->gen_method == normal_sq_mean_curvature_mi) )
          { REAL fd[MAXCOORD],net;
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
              grad[i] += net;
            }
          }
         else
          for ( i = 0 ; i < SDIM ; i++ )
            grad[i] += f[i];
      }
    eee_id = get_next_tail_edge(eee_id);
  } while ( !equal_id(eee_id,ee_id) );
    return venergy*vc->a/3;
}  /* end sqcurve_method_grad */


/*************************************************************************
              square mean curvature for string

              With optional power in variable curvature_power 

**************************************************************************/
/************************************************************************
*
*  function: sqcurve_string_init()
*
*  purpose:  
*/
void sqcurve_string_init(mode,mi)
int mode;
struct method_instance *mi;
{ 
  if ( web.modeltype != LINEAR )
    kb_error(2864,"Method sqcurve_string only for LINEAR model.\n",
       RECOVERABLE);

  sqcurve_energy_string_init(); /* sets up curve_power */
}

void sqcurve_string_marked_init(mode,mi)
int mode;
struct method_instance *mi;
{ int eltype;

  if ( web.modeltype != LINEAR )
    kb_error(2865,"Method sqcurve_string only for LINEAR model.\n",
       RECOVERABLE);

  marked_edge_attr = find_extra(MARKED_EDGE_ATTR_NAME,&eltype);
  if ( eltype != EDGE )
      kb_error(2184,"sqcurve_string_mark should be an edge attribute.\n",
        RECOVERABLE);
}

/************************************************************************
*
*  function: sqcurve_string_value()
*
*  purpose:  Calculate square curvature energy for string model
*            Works locally vertex by vertex. Assumes two edges per vertex.
*
*/

REAL sqcurve_string_value(v_info)
struct qinfo *v_info;
{
  REAL s1,s2,s1s2;  /* edge lengths */
  REAL *side1 = v_info->sides[0][0],*side2 = v_info->sides[0][1];
  REAL energy;
  REAL hh,a1,a2;
  REAL power;

  if ( v_info->vcount < 3 ) return 0.;

  if ( METH_INSTANCE(v_info->method)->flags & METH_PARAMETER_1 )
     power = METH_INSTANCE(v_info->method)->parameter_1;
  else power = curve_power;
  s1 = sqrt(SDIM_dot(side1,side1));
  s2 = sqrt(SDIM_dot(side2,side2));
  s1s2 = SDIM_dot(side1,side2);

  a1 = 1 + s1s2/s1/s2;
  a2 = (s1 + s2)/2;
  if ( (a1 <= 0.0) || (a2 == 0.0) ) return 0.0;

  hh = (2*a1+12*a1*a1/36)/a2/a2;
  energy = a2*pow(hh,power/2);

  return energy;
}

/************************************************************************
*
*  function: sqcurve_string_grad()
*
*  purpose:  Calculate square curvature force for string model
*                Works locally vertex by vertex. 
*
*/

REAL sqcurve_string_grad(v_info)
struct qinfo *v_info;
{
  REAL s1,s2,s1s2;  /* edge lengths */
  REAL *side1 = v_info->sides[0][0],*side2 = v_info->sides[0][1];
  int  i,k;
  REAL hh,a1,a2,energy;
  REAL a1g[2][MAXCOORD],a2g[2][MAXCOORD];
  REAL s[2];
  REAL *side[2];
  REAL term; /* common factor */
  REAL power;

  if ( v_info->vcount < 3 ) return 0.0;

  if ( METH_INSTANCE(v_info->method)->flags & METH_PARAMETER_1 )
     power = METH_INSTANCE(v_info->method)->parameter_1;
  else power = curve_power;

  s1 = sqrt(SDIM_dot(side1,side1));
  s2 = sqrt(SDIM_dot(side2,side2));
  s1s2 = SDIM_dot(side1,side2);

  a1 = 2 + 2*s1s2/s1/s2;
  a2 = (s1 + s2)/2;
  if (a2 == 0.0) return 0.0;

  hh = (a1+3*a1*a1/36)/a2/a2;
  if ( hh <= 0.0 )
  { hh = 0.0;
    energy = 0.0;
  }
  else
    energy = a2*pow(hh,power/2);

  side[0] = side1; side[1] = side2; s[0] = s1; s[1] = s2;
  for ( k = 0 ; k < 2 ; k++ )
  { REAL coeff = 2*s1s2/s[k]/s[k]/s[k]/s[1-k];
     for ( i = 0 ; i < SDIM ; i++ )
        { a1g[k][i] = 2*side[1-k][i]/s1/s2 - coeff*side[k][i];
          a2g[k][i] = 0.5*side[k][i]/s[k];
        }
  }
  for ( i = 0 ; i < SDIM ; i++ ) v_info->grad[0][i] = 0.0;
  term = a2*pow(hh,power/2-1); 
  for ( k = 0 ; k < 2 ; k++ )
     for ( i = 0 ; i < SDIM ; i++ )
     { REAL f;
        f = a2g[k][i]*energy/a2 + term*power/2
              *(a1g[k][i]*(1+6*a1/36)/a2/a2 - (2*a1+6*a1*a1/36)/a2/a2/a2*a2g[k][i]);
        v_info->grad[k+1][i] = f; 
        v_info->grad[0][i] += -f; 
     }
  return energy;

}


/************************************************************************
*
*  function: sqcurve_string_hess()
*
*  purpose:  Calculate square curvature hessian for string model
*                Works locally vertex by vertex. 
*
*/

REAL sqcurve_string_hess(v_info)
struct qinfo *v_info;
{
  REAL s1,s2,s1s2;  /* edge lengths */
  REAL *side1 = v_info->sides[0][0],*side2 = v_info->sides[0][1];
  REAL *side[2];
  int  i,k,kk,ii;
  REAL a1,a2,energy;
  REAL a1g[2][MAXCOORD],a2g[2][MAXCOORD];
  REAL a1h[2][2][MAXCOORD][MAXCOORD],a2h[2][2][MAXCOORD][MAXCOORD];
  REAL s[2];
  REAL ****h = v_info->hess;
  REAL term; /* common factor */
  REAL power;

  if ( v_info->vcount < 3 ) return 0.0;

  if ( METH_INSTANCE(v_info->method)->flags & METH_PARAMETER_1 )
     power = METH_INSTANCE(v_info->method)->parameter_1;
  else power = curve_power;

  if ( power != 2 )
     kb_error(1762,"Hessian not implemented for curvature_power != 2.\n",
        RECOVERABLE);

  s1 = sqrt(SDIM_dot(side1,side1));
  s2 = sqrt(SDIM_dot(side2,side2));
  s1s2 = SDIM_dot(side1,side2);

  a1 = 1 + s1s2/s1/s2;
  a2 = (s1 + s2)/2;
  if ( a2 == 0.0 ) return 0.0;

  energy = 2*(a1+6*a1*a1/36)/a2;

  side[0] = side1; side[1] = side2; s[0] = s1; s[1] = s2;
  for ( k = 0 ; k < 2 ; k++ )
  { term = s1s2/s[k]/s[k]/s[k]/s[1-k];
     for ( i = 0 ; i < SDIM ; i++ )
        { a1g[k][i] = side[1-k][i]/s1/s2 - term*side[k][i];
          a2g[k][i] = 0.5*side[k][i]/s[k];
        }
  }
  for ( i = 0 ; i < SDIM ; i++ ) v_info->grad[0][i] = 0.0;
  term = (a1+6*a1*a1/36)/a2/a2;
  for ( k = 0 ; k < 2 ; k++ )
     for ( i = 0 ; i < SDIM ; i++ )
     { REAL f = 2*(a1g[k][i]*(1+12*a1/36)/a2 - term*a2g[k][i]);
        v_info->grad[k+1][i] = f; 
        v_info->grad[0][i] += -f; 
     }

  /* hessian */
  memset((char*)a1h,0,sizeof(a1h));
  memset((char*)a2h,0,sizeof(a2h));
  for ( k = 0 ; k < 2 ; k++ )
  { REAL terma = 3*s1s2/s[k]/s[k]/s[k]/s[k]/s[k]/s[1-k];
     REAL termb = s1s2/s[k]/s[k]/s[k]/s[1-k]/s[1-k]/s[1-k];
     REAL termc = 1/s[k]/s[1-k]/s[1-k]/s[1-k];
     term = 1/s[k]/s[k]/s[k]/s[1-k];
     for ( i = 0 ; i < SDIM ; i++ )
     { a1h[k][1-k][i][i] += 1/s1/s2;
        for ( ii = 0 ; ii < SDIM ; ii++ )
        { a1h[k][k][i][ii] += -side[1-k][i]*term*side[k][ii];
          a1h[k][1-k][i][ii] += 
              -side[1-k][i]*termc*side[1-k][ii];
        }
        for ( ii = 0 ; ii < SDIM ; ii++ )
        { a1h[k][k][i][ii] += -side[1-k][ii]*term*side[k][i];
          a1h[k][1-k][i][ii] += -side[k][ii]*term*side[k][i];
          a1h[k][k][i][ii] += terma*side[k][i]*side[k][ii];
          a1h[k][1-k][i][ii] += termb*side[k][i]*side[1-k][ii];
        }
        a1h[k][k][i][i] += - s1s2*term;
        a2h[k][k][i][i] += 0.5/s[k];
        for ( ii = 0 ; ii < SDIM ; ii++ )
          a2h[k][k][i][ii] += -0.5*side[k][i]/s[k]/s[k]/s[k]*side[k][ii];

     }
  }
  for ( k = 0 ; k < 2 ; k++ )
    for ( kk = 0 ; kk < 2 ; kk++ )
     for ( i = 0 ; i < SDIM ; i++ )
      for ( ii = 0 ; ii < SDIM ; ii++ )
      { REAL f = 2*((a1h[k][kk][i][ii]*(1+12*a1/36)+12*a1g[k][i]*a1g[kk][ii]/36)/a2
                         - a1g[k][i]*(1+12*a1/36)*a2g[kk][ii]/a2/a2
                         - a1g[kk][ii]*(1+12*a1/36)*a2g[k][i]/a2/a2
                         - (a1+6*a1*a1/36)*a2h[k][kk][i][ii]/a2/a2
                         + 2*(a1+6*a1*a1/36)*a2g[k][i]*a2g[kk][ii]/a2/a2/a2);
         h[k+1][kk+1][i][ii] += f;
         h[k+1][0][i][ii] += -f;
         h[0][kk+1][i][ii] += -f;
         h[0][0][i][ii] += f;
      }
  return energy;
}

/*************************************************************************
    Square curvature for string with intrinsic curvature
    Plane only; intrinsic curvature in vertex attribute 
      "h_zero" or variable h_zero.  Edges assumed to all be oriented
    in the same direction.


**************************************************************************/
/************************************************************************
*
*  function: sqcurve2_string_init()
*
*  purpose:  
*/
void sqcurve2_string_init(mode,mi)
int mode;
struct method_instance *mi;
{ int eltype;
  int k;
  
  if ( web.modeltype != LINEAR )
    kb_error(3211,"Method sqcurve_string only for LINEAR model.\n",
       RECOVERABLE);

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

}

/************************************************************************
*
*  function: sqcurve2_string_value()
*
*  purpose:  Calculate square curvature energy for string model
*            Works locally vertex by vertex. Assumes two edges per vertex.
*
*/

REAL sqcurve2_string_value(v_info)
struct qinfo *v_info;
{
  REAL s1,s2,s1xs2;  /* edge lengths */
  REAL *side1 = v_info->sides[0][0],*side2 = v_info->sides[0][1];
  REAL energy;
  REAL h,h0,hh,a1,a2;

  if ( v_info->vcount < 3 ) return 0.;

  s1 = sqrt(SDIM_dot(side1,side1));
  s2 = sqrt(SDIM_dot(side2,side2));
  s1xs2 = side1[0]*side2[1] - side1[1]*side2[0];

  a1 = s1xs2/s1/s2;  /* sin(theta) */
  a2 = (s1 + s2)/2;
  if ( a2 == 0.0 ) return 0.0;

  h = a1/a2;  /* curvature */
  if ( h0_flag == H0_IN_ATTR )
    h0 = *VREAL(v_info->id,h0_attr);
  else
    h0 = h0_value;
  hh = (h - h0)*(h - h0);  /* square net curvature */
  energy = a2*hh;

  return energy;
}

/************************************************************************
*
*  function: sqcurve2_string_grad()
*
*  purpose:  Calculate square curvature force for string model
*                Works locally vertex by vertex. 
*
*/

REAL sqcurve2_string_grad(v_info)
struct qinfo *v_info;
{
  REAL s1,s2,s1xs2;  /* edge lengths */
  REAL *side1 = v_info->sides[0][0],*side2 = v_info->sides[0][1];
  int  i,k;
  REAL hh,h,h0,a1,a2,energy;
  REAL a1g[2][MAXCOORD],a2g[2][MAXCOORD];
  REAL s[2];
  REAL *side[2];

  if ( v_info->vcount < 3 ) return 0.;

  s1 = sqrt(SDIM_dot(side1,side1));
  s2 = sqrt(SDIM_dot(side2,side2));
  s1xs2 = side1[0]*side2[1] - side1[1]*side2[0];

  a1 = s1xs2/s1/s2;  /* sin(theta) */
  a2 = (s1 + s2)/2;
  if ( a2 == 0.0 ) return 0.0;

  h = a1/a2;  /* curvature */
  if ( h0_flag == H0_IN_ATTR )
    h0 = *VREAL(v_info->id,h0_attr);
  else
    h0 = h0_value;
  hh = (h - h0)*(h - h0);  /* square net curvature */
  energy = a2*hh;


  side[0] = side1; side[1] = side2; s[0] = s1; s[1] = s2;
  for ( k = 0 ; k < 2 ; k++ )
  { REAL coeff = s1xs2/s[k]/s[k]/s[k]/s[1-k];
    for ( i = 0 ; i < SDIM ; i++ )
    { a1g[k][i] = (k==i?1:-1)*side[1-k][1-i]/s1/s2 - coeff*side[k][i];
      a2g[k][i] = 0.5*side[k][i]/s[k];
    }
  }
  for ( i = 0 ; i < SDIM ; i++ ) v_info->grad[0][i] = 0.0;
  for ( k = 0 ; k < 2 ; k++ )
    for ( i = 0 ; i < SDIM ; i++ )
    { REAL f;
      f = a2g[k][i]*hh + a2*2*(h - h0)*(a1g[k][i]/a2 - a1/a2/a2*a2g[k][i]);
      v_info->grad[k+1][i] = f; 
      v_info->grad[0][i] += -f; 
    }
  return energy;

}

/*************************************************************************
              square mean curvature for string, version 3

              Uses (1/L1 + 1/L2)/2 instead of 1/(L1+L2) for inverse
              length to encourage equal size edges.
**************************************************************************/
/************************************************************************
*
*  function: sqcurve3_string_init()
*
*  purpose:  
*/
void sqcurve3_string_init(mode,mi)
int mode;
struct method_instance *mi;
{ 
  if ( web.modeltype != LINEAR )
    kb_error(4864,"Method sqcurve3_string only for LINEAR model.\n",
       RECOVERABLE);
}

/************************************************************************
*
*  function: sqcurve3_string_all()
*
*  purpose:  Calculate square curvature energy etc. for string model
*            Works locally vertex by vertex. Assumes two edges per vertex.
*
*/

REAL sqcurve3_string_all(v_info,mode)
struct qinfo *v_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, METHOD_HESSIAN */
{
  REAL s1,s2,s1s2;  /* edge lengths */
  REAL *side1 = v_info->sides[0][0],*side2 = v_info->sides[0][1];
  REAL energy;
  REAL a1,a2;
  REAL da1[3][MAXCOORD],da2[3][MAXCOORD];
  REAL dda1[3][3][MAXCOORD][MAXCOORD],dda2[3][3][MAXCOORD][MAXCOORD];
  int i,j,k,kk;


  if ( v_info->vcount != 3 ) return 0.;

  s1 = sqrt(SDIM_dot(side1,side1));
  s2 = sqrt(SDIM_dot(side2,side2));
  s1s2 = SDIM_dot(side1,side2);
  if ( (s1 == 0.0) || (s2 == 0.0) ) return 0.0;

  a1 = 1 + s1s2/s1/s2;  /* 1 - cos(theta)^2 */
  a2 = (1/s1 + 1/s2)/2;  /* inverse length approximation */

  energy = (2*a1+a1*a1/3)*a2;

  if ( mode == METHOD_VALUE )
  return energy;

  /* Gradient */
  for ( i = 0 ; i < SDIM ; i++ )
  { da1[1][i] = side2[i]/s1/s2 - s1s2/s1/s1/s1/s2*side1[i];
    da1[2][i] = side1[i]/s1/s2 - s1s2/s1/s2/s2/s2*side2[i];
    da1[0][i] = -da1[1][i] - da1[2][i];
    da2[1][i] = -1./2/s1/s1/s1*side1[i];
    da2[2][i] = -1./2/s2/s2/s2*side2[i];
    da2[0][i] = -da2[1][i] - da2[2][i];
  }
  for ( k = 0 ; k < v_info->vcount ; k++ )
    for ( i = 0 ; i < SDIM ; i++ )
      v_info->grad[k][i] = (2 + 2*a1/3)*da1[k][i]*a2 + (2*a1+a1*a1/3)*da2[k][i];

  if ( mode == METHOD_GRADIENT )
  return energy;

  /* Hessian */
  for ( i = 0 ; i < SDIM ; i++ )
  {
    for ( j = 0 ; j < SDIM ; j++ )
    { 
      dda1[1][1][i][j] = -side2[i]/s1/s1/s1/s2*side1[j]
                          - side2[j]/s1/s1/s1/s2*side1[i]
                          + 3*s1s2/s1/s1/s1/s1/s1/s2*side1[i]*side1[j]
                          + (j==i ? -s1s2/s1/s1/s1/s2 : 0.0);
      dda1[1][2][i][j] = -side2[i]/s1/s2/s2/s2*side2[j]
                          - side1[j]/s1/s1/s1/s2*side1[i]
                          + s1s2/s1/s1/s1/s2/s2/s2*side1[i]*side2[j]
                          + (j==i ? 1/s1/s2 : 0.0);
      dda1[2][1][i][j] = -side1[i]/s1/s1/s1/s2*side1[j]
                          - side2[j]/s1/s2/s2/s2*side2[i]
                          + s1s2/s1/s1/s1/s2/s2/s2*side2[i]*side1[j]
                          + (j==i ? 1/s1/s2 : 0.0);
      dda1[2][2][i][j] = -side1[i]/s1/s2/s2/s2*side2[j]
                          - side1[j]/s1/s2/s2/s2*side2[i]
                          + 3*s1s2/s1/s2/s2/s2/s2/s2*side2[i]*side2[j]
                          + (j==i ? -s1s2/s1/s2/s2/s2 : 0.0);
      dda1[0][1][i][j] = -dda1[1][1][i][j] - dda1[2][1][i][j];
      dda1[0][2][i][j] = -dda1[1][2][i][j] - dda1[2][2][i][j];
      dda1[0][0][i][j] = -dda1[0][1][i][j] - dda1[0][2][i][j];
      
      dda2[1][1][i][j] = 3./2/s1/s1/s1/s1/s1*side1[j]*side1[i]
                           + (j==i ? -1./2/s1/s1/s1 : 0.0 );
      dda2[1][2][i][j] = 0.0;
      dda2[2][1][i][j] = 0.0;
      dda2[2][2][i][j] = 3./2/s2/s2/s2/s2/s2*side2[j]*side2[i]
                           + (j==i ? -1./2/s2/s2/s2 : 0.0 );
      dda2[0][1][i][j] = -dda2[1][1][i][j] - dda2[2][1][i][j];
      dda2[0][2][i][j] = -dda2[1][2][i][j] - dda2[2][2][i][j];
      dda2[0][0][i][j] = -dda2[0][1][i][j] - dda2[0][2][i][j];
    }
  }

  for ( k = 0 ; k < v_info->vcount ; k++ )
    for ( kk = 0 ; kk < v_info->vcount ; kk++ )
      for ( i = 0 ; i < SDIM ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
          v_info->hess[k][kk][i][j] = (2*da1[kk][j]/3)*da1[k][i]*a2
                + (2 + 2*a1/3)*dda1[k][kk][i][j]*a2
                + (2 + 2*a1/3)*da1[k][i]*da2[kk][j]
                + (2 + 2*a1/3)*da1[kk][j]*da2[k][i]
                + (2*a1+a1*a1/3)*dda2[k][kk][i][j];
       
  return energy;
}

REAL sqcurve3_string_value(v_info)
struct qinfo *v_info;
{ return sqcurve3_string_all(v_info,METHOD_VALUE);
}

REAL sqcurve3_string_grad(v_info)
struct qinfo *v_info;
{ return sqcurve3_string_all(v_info,METHOD_GRADIENT);
}

REAL sqcurve3_string_hess(v_info)
struct qinfo *v_info;
{ return sqcurve3_string_all(v_info,METHOD_HESSIAN);
}
/* end sqcurve3_string method  */

/*************************************************************************
              Square mean curvature for cylinder in string model

              Cylindrical axis is x axis.
              Includes 2*pi factor.
              Includes the 1/2 factor in the definition of mean curvature.

              Functions can actually do any power of (h-h0),
                so could set up other methods easily.
**************************************************************************/

REAL sq_mean_curv_cyl_all ARGS(( struct qinfo *, int, REAL ));

/************************************************************************
*
*  function: sq_mean_curv_cyl_init()
*
*  purpose:  
*/
void sq_mean_curv_cyl_init(mode,mi)
int mode;
struct method_instance *mi;
{ int k;
  int eltype;

  if ( web.modeltype != LINEAR )
     kb_error(2486,"sq_mean_curv_cyl method only for LINEAR model.\n",
        RECOVERABLE);

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


}

REAL sq_mean_curv_cyl_all(v_info,mode,power)
struct qinfo *v_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, METHOD_HESSIAN */
REAL power;
{
  REAL s1,s2;  /* edge lengths */
  REAL *side1 = v_info->sides[0][0],*side2 = v_info->sides[0][1];
  REAL energy;
  REAL area;  /* of entire pair of frustrums */
  REAL h,hh;
  REAL r0,r1,r2;  /* radii; r1 is middle radius, at varying vertex */
  REAL dadx[3],dadr[3]; /* index refers to v_info order */
  REAL ddadxdx[3][3],ddadxdr[3][3],ddadrdr[3][3],ddadrdx[3][3],dhdx[3],dhdr[3];
  REAL ddhdxdx[3][3],ddhdxdr[3][3],ddhdrdr[3][3],ddhdrdx[3][3];
  REAL dddadxdxdx[3][3][3];
  REAL dddadxdxdr[3][3][3];
  REAL dddadxdrdx[3][3][3];
  REAL dddadxdrdr[3][3][3];
  REAL dddadrdxdx[3][3][3];
  REAL dddadrdxdr[3][3][3];
  REAL dddadrdrdx[3][3][3];
  REAL dddadrdrdr[3][3][3];
  REAL root;
  int i,j;
  int sign; /* of curvature */


  if ( (v_info->vcount == 2)  &&
          (fabs(v_info->x[0][1]) < 1e-10) )
  { /* assume this is an endpoint on the axis, so we do special
       concentrated squared curvature */
    edge_id e_id = get_vertex_edge(v_info->id);
    int sign = inverted(e_id) ? 1 : -1; /* in case of h0 */
    REAL ss = SDIM_dot(side1,side1);
    REAL dx = v_info->x[1][0] - v_info->x[0][0];
    REAL prod; /* full power */
    REAL hterm; /* h - h0 */
    REAL gradprod=0.0; /* one less power, for grad */
    REAL hessprod=0.0; /* two less power, for hess */
    int k;
    REAL dx_dx0=-1,dx_dx1=1,dx_dy0=0,dx_dy1=0;
    REAL dss_dx0,dss_dx1,dss_dy0,dss_dy1;
    REAL dhterm_dx0,dhterm_dx1,dhterm_dy0,dhterm_dy1;
    REAL dprod_dx0,dprod_dy0,dprod_dx1,dprod_dy1;
    
    REAL dss_dx0_dx0,dss_dx0_dx1,dss_dx0_dy0=0,dss_dx0_dy1=0;
    REAL dss_dx1_dx0,dss_dx1_dx1,dss_dx1_dy0=0,dss_dx1_dy1=0;
    REAL dss_dy0_dx0=0,dss_dy0_dx1=0,dss_dy0_dy0,dss_dy0_dy1;
    REAL dss_dy1_dx0=0,dss_dy1_dx1=0,dss_dy1_dy0,dss_dy1_dy1;
   
    REAL dhterm_dx0_dx0,dhterm_dx0_dx1,dhterm_dx0_dy0,dhterm_dx0_dy1;
    REAL dhterm_dx1_dx0,dhterm_dx1_dx1,dhterm_dx1_dy0,dhterm_dx1_dy1;
    REAL dhterm_dy0_dx0,dhterm_dy0_dx1,dhterm_dy0_dy0,dhterm_dy0_dy1;
    REAL dhterm_dy1_dx0,dhterm_dy1_dx1,dhterm_dy1_dy0,dhterm_dy1_dy1;
    
    REAL dprod_dx0_dx0,dprod_dx0_dx1,dprod_dx0_dy0,dprod_dx0_dy1;
    REAL dprod_dx1_dx0,dprod_dx1_dx1,dprod_dx1_dy0,dprod_dx1_dy1;
    REAL dprod_dy0_dx0,dprod_dy0_dx1,dprod_dy0_dy0,dprod_dy0_dy1;
    REAL dprod_dy1_dx0,dprod_dy1_dx1,dprod_dy1_dy0,dprod_dy1_dy1;
       
    hterm = sign*2*dx/ss - h0_value;
    for ( prod = 1.0, k = 0 ; k < power ; k++ )
    { hessprod = gradprod;
      gradprod = prod;
      prod *= hterm;
    }
    energy = prod*M_PI*ss/4;
    if ( mode == METHOD_VALUE )
      return energy;

    /* gradient */
    dss_dx0 = -2*side1[0];
    dss_dx1 =  2*side1[0];
    dss_dy0 = -2*side1[1];
    dss_dy1 =  2*side1[1];
    dhterm_dx0 = sign*2*(-1/ss - dx/ss/ss*dss_dx0);
    dhterm_dy0 = sign*2*(      - dx/ss/ss*dss_dy0);
    dhterm_dx1 = sign*2*( 1/ss - dx/ss/ss*dss_dx1);
    dhterm_dy1 = sign*2*(      - dx/ss/ss*dss_dy1);
    dprod_dx0 = power*gradprod*dhterm_dx0;
    dprod_dx1 = power*gradprod*dhterm_dx1;
    dprod_dy0 = power*gradprod*dhterm_dy0;
    dprod_dy1 = power*gradprod*dhterm_dy1;
    v_info->grad[0][0] = dprod_dx0*M_PI*ss/4 +prod*M_PI*dss_dx0/4;
    v_info->grad[0][1] = 0.0; /* assuming constrained to axis */
    v_info->grad[1][0] = dprod_dx1*M_PI*ss/4 +prod*M_PI*dss_dx1/4;
    v_info->grad[1][1] = dprod_dy1*M_PI*ss/4 +prod*M_PI*dss_dy1/4;
    if ( mode == METHOD_GRADIENT )
      return energy;

    /* hessian */
    dss_dx0_dx0 = 2;
    dss_dx0_dx1 = -2;
    dss_dx1_dx0 = -2;
    dss_dx1_dx1 = 2;
    dss_dy0_dy0 = 2;
    dss_dy0_dy1 = -2;
    dss_dy1_dy0 = -2;
    dss_dy1_dy1 = 2;
     
    dhterm_dx0_dx0 = sign*2*(1/ss/ss*dss_dx0 - dx_dx0/ss/ss*dss_dx0
                         +2*dx/ss/ss/ss*dss_dx0*dss_dx0 - dx/ss/ss*dss_dx0_dx0);
    dhterm_dx0_dx1 = sign*2*(1/ss/ss*dss_dx1 - dx_dx1/ss/ss*dss_dx0
                         +2*dx/ss/ss/ss*dss_dx1*dss_dx0 - dx/ss/ss*dss_dx0_dx1);
    dhterm_dx0_dy0 = sign*2*(1/ss/ss*dss_dy0 - dx_dy0/ss/ss*dss_dx0
                         +2*dx/ss/ss/ss*dss_dy0*dss_dx0 - dx/ss/ss*dss_dx0_dy0);
    dhterm_dx0_dy1 = sign*2*(1/ss/ss*dss_dy1 - dx_dy1/ss/ss*dss_dx0
                         +2*dx/ss/ss/ss*dss_dy1*dss_dx0 - dx/ss/ss*dss_dx0_dy1);
                         
    dhterm_dy0_dx0 = sign*2*(-dx_dx0/ss/ss*dss_dy0 +2*dx/ss/ss/ss*dss_dx0*dss_dy0
                                  - dx/ss/ss*dss_dy0_dx0);
    dhterm_dy0_dx1 = sign*2*(-dx_dx1/ss/ss*dss_dy0 +2*dx/ss/ss/ss*dss_dx1*dss_dy0
                                  - dx/ss/ss*dss_dy0_dx1);
    dhterm_dy0_dy0 = sign*2*(-dx_dy0/ss/ss*dss_dy0 +2*dx/ss/ss/ss*dss_dy0*dss_dy0
                                  - dx/ss/ss*dss_dy0_dy0);
    dhterm_dy0_dy1 = sign*2*(-dx_dy1/ss/ss*dss_dy0 +2*dx/ss/ss/ss*dss_dy1*dss_dy0
                                  - dx/ss/ss*dss_dy0_dy1);
   
     
    dhterm_dx1_dx0 = sign*2*(-1/ss/ss*dss_dx0 - dx_dx0/ss/ss*dss_dx1
                         +2*dx/ss/ss/ss*dss_dx0*dss_dx1 - dx/ss/ss*dss_dx1_dx0);
    dhterm_dx1_dx1 = sign*2*(-1/ss/ss*dss_dx1 - dx_dx1/ss/ss*dss_dx1
                         +2*dx/ss/ss/ss*dss_dx1*dss_dx1 - dx/ss/ss*dss_dx1_dx1);
    dhterm_dx1_dy0 = sign*2*(-1/ss/ss*dss_dy0 - dx_dy0/ss/ss*dss_dx0
                         +2*dx/ss/ss/ss*dss_dy0*dss_dx1 - dx/ss/ss*dss_dx1_dy0);
    dhterm_dx1_dy1 = sign*2*(-1/ss/ss*dss_dy1 - dx_dy1/ss/ss*dss_dx0
                         +2*dx/ss/ss/ss*dss_dy1*dss_dx1 - dx/ss/ss*dss_dx1_dy1);
    
    
    
    dhterm_dy1_dx0 = sign*2*(-dx_dx0/ss/ss*dss_dy1 +2*dx/ss/ss/ss*dss_dx0*dss_dy1
                                  - dx/ss/ss*dss_dy1_dx0);
    dhterm_dy1_dx1 = sign*2*(-dx_dx1/ss/ss*dss_dy1 +2*dx/ss/ss/ss*dss_dx1*dss_dy1
                                  - dx/ss/ss*dss_dy1_dx1);
    dhterm_dy1_dy0 = sign*2*(-dx_dy0/ss/ss*dss_dy1 +2*dx/ss/ss/ss*dss_dy0*dss_dy1
                                  - dx/ss/ss*dss_dy1_dy0);
    dhterm_dy1_dy1 = sign*2*(-dx_dy1/ss/ss*dss_dy1 +2*dx/ss/ss/ss*dss_dy1*dss_dy1
                                  - dx/ss/ss*dss_dy1_dy1);
   
    
    dprod_dx0_dx0 = power*(power-1)*hessprod*dhterm_dx0*dhterm_dx0
        + power*gradprod*dhterm_dx0_dx0;
    dprod_dx0_dx1 = power*(power-1)*hessprod*dhterm_dx0*dhterm_dx1
        + power*gradprod*dhterm_dx0_dx1;
    dprod_dx0_dy0 = power*(power-1)*hessprod*dhterm_dx0*dhterm_dy0
        + power*gradprod*dhterm_dx0_dy0;
    dprod_dx0_dy1 = power*(power-1)*hessprod*dhterm_dx0*dhterm_dy1
        + power*gradprod*dhterm_dx0_dy1;
   
    dprod_dx1_dx0 = power*(power-1)*hessprod*dhterm_dx1*dhterm_dx0
        + power*gradprod*dhterm_dx1_dx0;
    dprod_dx1_dx1 = power*(power-1)*hessprod*dhterm_dx1*dhterm_dx1
        + power*gradprod*dhterm_dx1_dx1;
    dprod_dx1_dy0 = power*(power-1)*hessprod*dhterm_dx1*dhterm_dy0
        + power*gradprod*dhterm_dx1_dy0;
    dprod_dx1_dy1 = power*(power-1)*hessprod*dhterm_dx1*dhterm_dy1
        + power*gradprod*dhterm_dx1_dy1;
      
    dprod_dy0_dx0 = power*(power-1)*hessprod*dhterm_dy0*dhterm_dx0
        + power*gradprod*dhterm_dy0_dx0;
    dprod_dy0_dx1 = power*(power-1)*hessprod*dhterm_dy0*dhterm_dx1
        + power*gradprod*dhterm_dy0_dx1;
    dprod_dy0_dy0 = power*(power-1)*hessprod*dhterm_dy0*dhterm_dy0
        + power*gradprod*dhterm_dy0_dy0;
    dprod_dy0_dy1 = power*(power-1)*hessprod*dhterm_dy0*dhterm_dy1
        + power*gradprod*dhterm_dy0_dy1;
    
    dprod_dy1_dx0 = power*(power-1)*hessprod*dhterm_dy1*dhterm_dx0
        + power*gradprod*dhterm_dy1_dx0;
    dprod_dy1_dx1 = power*(power-1)*hessprod*dhterm_dy1*dhterm_dx1
        + power*gradprod*dhterm_dy1_dx1;
    dprod_dy1_dy0 = power*(power-1)*hessprod*dhterm_dy1*dhterm_dy0
        + power*gradprod*dhterm_dy1_dy0;
    dprod_dy1_dy1 = power*(power-1)*hessprod*dhterm_dy1*dhterm_dy1
        + power*gradprod*dhterm_dy1_dy1;
  
    v_info->hess[0][0][0][0] = M_PI/4*(dprod_dx0_dx0*ss + dprod_dx0*dss_dx0
            + dprod_dx0*dss_dx0 + prod*dss_dx0_dx0);
    v_info->hess[0][0][0][1] = 0.0;
    v_info->hess[0][1][0][0] =  -v_info->hess[0][0][0][0];
    v_info->hess[0][1][0][1] =  M_PI/4*(dprod_dx0_dy1*ss + dprod_dx0*dss_dy1
            + dprod_dy1*dss_dx0 + prod*dss_dx0_dy1);

    v_info->hess[0][0][1][0] = 0.0; /* assuming constrained to axis */
    v_info->hess[0][0][1][1] = 0.0; /* assuming constrained to axis */
    v_info->hess[0][1][1][0] = 0.0; /* assuming constrained to axis */
    v_info->hess[0][1][1][1] = 0.0; /* assuming constrained to axis */

    v_info->hess[1][0][0][0] = v_info->hess[0][1][0][0];
    v_info->hess[1][0][0][1] = 0.0;
    v_info->hess[1][1][0][0] = -v_info->hess[1][0][0][0];
    v_info->hess[1][1][0][1] = M_PI/4*(dprod_dx1_dy1*ss + dprod_dx1*dss_dy1
               + dprod_dy1*dss_dx1 + prod*dss_dx1_dy1);
    
    v_info->hess[1][0][1][0] = v_info->hess[0][1][0][1];
    v_info->hess[1][0][1][1] = v_info->hess[0][1][1][1];
    v_info->hess[1][1][1][0] = v_info->hess[1][1][0][1];
    v_info->hess[1][1][1][1]=  M_PI/4*(dprod_dy1_dy1*ss + dprod_dy1*dss_dy1
                 + dprod_dy1*dss_dy1 + prod*dss_dy1_dy1);

    return energy;
  } 

  if ( v_info->vcount < 3 ) return 0.;

  /* now regular case */

  r0 = v_info->x[1][1];
  r1 = v_info->x[0][1];
  r2 = v_info->x[2][1];
  s1 = sqrt(SDIM_dot(side1,side1));
  s2 = sqrt(SDIM_dot(side2,side2));
  area = s1*(r0 + r1)/2 + s2*(r1 + r2)/2;

  dadx[0] = -side1[0]*(r0+r1)/2/s1 - side2[0]*(r1+r2)/2/s2;
  dadr[0] = (r1*r1 - r0*r0)/s1/2 + s1/2 + (r1*r1 - r2*r2)/s2/2 + s2/2;

  sign = ( dadr[0] < 0 ) ? -1 : 1;
  root = sqrt(dadx[0]*dadx[0] + dadr[0]*dadr[0]); 
  h = sign*root/area; /* half factor included here */

  switch ( h0_flag )
  { case H0_IN_GLOBAL: hh = h - h0_value; break;
    case H0_IN_ATTR:   hh = h - *VREAL(v_info->id,h0_attr); break;
    default: hh = h;
  }

  if ( power == 2.0 ) energy = 2*M_PI*hh*hh*area/2;
  else if ( power == 1.0 ) energy = 2*M_PI*hh*area/2;
  else energy = 2*M_PI*area/2*pow(fabs(hh),power);

  if ( mode == METHOD_VALUE )
    return energy;

  /* Gradient */
  /* Note: v_info->grad[i][j] is component j of gradient at vertex i */

  dadx[1] = side1[0]*(r0+r1)/2/s1;
  dadr[1] = -(r1*r1 - r0*r0)/s1/2 + s1/2;

  dadx[2] = side2[0]*(r1+r2)/2/s2;
  dadr[2] = -(r1*r1 - r2*r2)/s2/2 + s2/2;

  ddadxdx[0][1] = -(r0+r1)/2/s1 + side1[0]*(r0+r1)/2/s1/s1/s1*side1[0];
  ddadxdx[0][2] = -(r1+r2)/2/s2 + side2[0]*(r1+r2)/2/s2/s2/s2*side2[0];
  ddadxdx[0][0] = -ddadxdx[0][1] - ddadxdx[0][2];

  /* dadx[0] = -side1[0]*(r0+r1)/2/s1 - side2[0]*(r1+r2)/2/s2; */
  ddadxdr[0][1] = -side1[0]/2/s1 - side1[0]*(r1*r1-r0*r0)/s1/s1/s1/2;
  ddadxdr[0][2] = -side2[0]/2/s2 - side2[0]*(r1*r1-r2*r2)/s2/s2/s2/2;
  ddadxdr[0][0] = -side1[0]/2/s1 + side1[0]*(r1*r1-r0*r0)/s1/s1/s1/2 
                  -side2[0]/2/s2 + side2[0]*(r1*r1-r2*r2)/s2/s2/s2/2;

  ddadrdx[0][1] = ((r1*r1-r0*r0)/2*(-1)/s1/s1 + 1/2.)*(side1[0]/s1);
  ddadrdx[0][2] = ((r1*r1-r2*r2)/2*(-1)/s2/s2 + 1/2.)*(side2[0]/s2);
  ddadrdx[0][0] = -ddadrdx[0][1]-ddadrdx[0][2];

  ddadrdr[0][0] = r1/s1 + ((r1*r1-r0*r0)/2*(-1)/s1/s1 + 0.5)/s1*(r1-r0)
                + r1/s2 + ((r1*r1-r2*r2)/2*(-1)/s2/s2 + 0.5)/s2*(r1-r2);  
  ddadrdr[0][1] = -r0/s1 +((r1*r1-r0*r0)/2*(-1)/s1/s1 + 0.5)/s1*(r0-r1);
  ddadrdr[0][2] = -r2/s2 + ((r1*r1-r2*r2)/2*(-1)/s2/s2 + 0.5)/s2*(r2-r1);

  for ( i = 0 ; i < 3 ; i++ )
  { dhdx[i] = sign*(.5/root/area*
         (2*dadx[0]*ddadxdx[0][i] + 2*dadr[0]*ddadrdx[0][i]))
         - h/area*dadx[i];
    dhdr[i] = sign*(.5/root/area*
         (2*dadx[0]*ddadxdr[0][i] + 2*dadr[0]*ddadrdr[0][i]))
         - h/area*dadr[i];
  }
  
  if ( power == 2.0 ) 
  { for ( i = 0 ; i < 3 ; i++ )
    { v_info->grad[i][0] = 2*M_PI*(2*hh*dhdx[i]*area + hh*hh*dadx[i])/2;
      v_info->grad[i][1] = 2*M_PI*(2*hh*dhdr[i]*area + hh*hh*dadr[i])/2;
    }
  }
  else if ( power == 1.0 ) 
  { for ( i = 0 ; i < 3 ; i++ )
    { v_info->grad[i][0] = 2*M_PI*(dhdx[i]*area + hh*dadx[i])/2;
      v_info->grad[i][1] = 2*M_PI*(dhdr[i]*area + hh*dadr[i])/2;
    }
  }
  else
  { REAL pp = pow(fabs(hh),power-1);
    for ( i = 0 ; i < 3 ; i++ )
    { v_info->grad[i][0] = 2*M_PI*(power*pp*dhdx[i]*area + pp*hh*dadx[i])/2;
      v_info->grad[i][1] = 2*M_PI*(power*pp*dhdr[i]*area + pp*hh*dadr[i])/2;
    }
  }

  if ( mode == METHOD_GRADIENT )
    return energy;

  /*  Hessian */
  /*  Note: v_info->hess[i][ii][j][jj] is component j of vertex i and 
      component jj of vertex ii */

  /* dadx[1] = side1[0]*(r0+r1)/2/s1; */

  ddadxdx[1][1] =  (r0+r1)/2/s1 - side1[0]*(r0+r1)/2/s1/s1/s1*side1[0];
  ddadxdx[1][2] =  0;
  ddadxdx[1][0] =  -ddadxdx[1][1];

  ddadxdr[1][1] =  side1[0]/2/s1 + side1[0]*(r0+r1)/s1/s1/s1/2*(r1-r0);
  ddadxdr[1][2] =  0;
  ddadxdr[1][0] =  side1[0]/2/s1 - side1[0]*(r0+r1)/s1/s1/s1/2*(r1-r0);

  /* dadr[1] = -(r1*r1 - r0*r0)/s1/2 + s1/2; */

  ddadrdx[1][1] = (-(r1*r1-r0*r0)/2*(-1)/s1/s1 + 1/2.)*(side1[0]/s1);
  ddadrdx[1][2] = 0;
  ddadrdx[1][0] = -ddadrdx[1][1];

  ddadrdr[1][0] = -r1/s1 + (-(r1*r1-r0*r0)/2*(-1)/s1/s1 + 0.5)/s1*(r1-r0);
  ddadrdr[1][1] =  r0/s1 + (-(r1*r1-r0*r0)/2*(-1)/s1/s1 + 0.5)/s1*(r0-r1);
  ddadrdr[1][2] = 0;


  /* dadx[2] = side2[0]*(r1+r2)/2/s2; */

  ddadxdx[2][1] = 0;
  ddadxdx[2][2] = (r1+r2)/2/s2 - side2[0]*(r1+r2)/2/s2/s2/s2*side2[0];
  ddadxdx[2][0] = -ddadxdx[2][2];

  ddadxdr[2][1] = 0;
  ddadxdr[2][2] =  side2[0]/2/s2 + side2[0]*(r2+r1)/s2/s2/s2/2*(r1-r2);
  ddadxdr[2][0] =  side2[0]/2/s2 - side2[0]*(r2+r1)/s2/s2/s2/2*(r1-r2);

  /* dadr[2] = -(r1*r1 - r2*r2)/s2/2 + s2/2; */

  ddadrdx[2][1] = 0;
  ddadrdx[2][2] = (-(r1*r1-r2*r2)/2*(-1)/s2/s2 + 1/2.)*(side2[0]/s2);
  ddadrdx[2][0] = -ddadrdx[2][2];

  ddadrdr[2][0] = -r1/s2 + (-(r1*r1-r2*r2)/2*(-1)/s2/s2 + 0.5)/s2*(r1-r2);  
  ddadrdr[2][1] = 0;
  ddadrdr[2][2] =  r2/s2 + (-(r1*r1-r2*r2)/2*(-1)/s2/s2 + 0.5)/s2*(r2-r1);

  /* ddadxdx[0][1] = -(r0+r1)/2/s1 + side1[0]*(r0+r1)/2/s1/s1/s1*side1[0]; */
  dddadxdxdx[0][1][0] = -(r0+r1)/2/s1/s1/s1/(-2)*(-2*side1[0])
        - 2*side1[0]*(r0+r1)/2/s1/s1/s1
        - 1.5*side1[0]*side1[0]*(r0+r1)/2/s1/s1/s1/s1/s1*(-2*side1[0]);
  dddadxdxdx[0][1][1] = -dddadxdxdx[0][1][0];
  dddadxdxdx[0][1][2] = 0;
  dddadxdxdr[0][1][0] = -1./2/s1 + side1[0]/2/s1/s1/s1*side1[0]
      + (-(r0+r1)/2*(-1)/s1/s1 + side1[0]*(r0+r1)/2*(-3)/s1/s1/s1/s1*side1[0])
       *(r1-r0)/s1;
  dddadxdxdr[0][1][1] = -1./2/s1 + side1[0]/2/s1/s1/s1*side1[0]
      + (-(r0+r1)/2*(-1)/s1/s1 + side1[0]*(r0+r1)/2*(-3)/s1/s1/s1/s1*side1[0])
       *(r0-r1)/s1;
  dddadxdxdr[0][1][2] = 0;

  /* ddadxdx[0][2] = -(r1+r2)/2/s2 + side2[0]*(r1+r2)/2/s2/s2/s2*side2[0]; */

  dddadxdxdx[0][2][0] = + 2*side2[0]*(-1)*(r1+r2)/2/s2/s2/s2
      + (-(r1+r2)/2*(-1)/s2/s2 + side2[0]*(r1+r2)/2*(-3)/s2/s2/s2/s2*side2[0])
             *(-side2[0])/s2;
  dddadxdxdx[0][2][1] = 0;
  dddadxdxdx[0][2][2] = -dddadxdxdx[0][2][0];
  dddadxdxdr[0][2][0] = -1./2/s2 + side2[0]/2/s2/s2/s2*side2[0] +
      (-(r1+r2)/2*(-1)/s2/s2 + side2[0]*(r1+r2)/2*(-3)/s2/s2/s2/s2*side2[0])
         *(r1-r2)/s2;
  dddadxdxdr[0][2][1] = 0;
  dddadxdxdr[0][2][2] = -1./2/s2 + side2[0]/2/s2/s2/s2*side2[0] +
      (-(r1+r2)/2*(-1)/s2/s2 + side2[0]*(r1+r2)/2*(-3)/s2/s2/s2/s2*side2[0])
         *(r2-r1)/s2;

  /* ddadxdx[0][0] = -ddadxdx[0][1] - ddadxdx[0][2]; */
  dddadxdxdx[0][0][0] = -dddadxdxdx[0][1][0] - dddadxdxdx[0][2][0]; 
  dddadxdxdx[0][0][1] = -dddadxdxdx[0][1][1] - dddadxdxdx[0][2][1];
  dddadxdxdx[0][0][2] = -dddadxdxdx[0][1][2] - dddadxdxdx[0][2][2];
  dddadxdxdr[0][0][0] = -dddadxdxdr[0][1][0] - dddadxdxdr[0][2][0]; 
  dddadxdxdr[0][0][1] = -dddadxdxdr[0][1][1] - dddadxdxdr[0][2][1];
  dddadxdxdr[0][0][2] = -dddadxdxdr[0][1][2] - dddadxdxdr[0][2][2];


  /* ddadxdr[0][1] = -side1[0]/2/s1 - side1[0]*(r1*r1-r0*r0)/s1/s1/s1/2; */
  dddadxdrdx[0][1][0] = dddadxdxdr[0][0][1];
  dddadxdrdx[0][1][1] = dddadxdxdr[0][1][1];
  dddadxdrdx[0][1][2] = dddadxdxdr[0][2][1]; 
  dddadxdrdr[0][1][0] = -side1[0]*2*r1/s1/s1/s1/2 +
       (-side1[0]/2*(-1)/s1/s1 - side1[0]*(r1*r1-r0*r0)*(-3)/s1/s1/s1/s1/2)
        *(r1-r0)/s1; 
  dddadxdrdr[0][1][1] = -side1[0]*(-2)*r0/s1/s1/s1/2 +
       (-side1[0]/2*(-1)/s1/s1 - side1[0]*(r1*r1-r0*r0)*(-3)/s1/s1/s1/s1/2)
        *(r0-r1)/s1; 
  dddadxdrdr[0][1][2] = 0;

  /* ddadxdr[0][2] = -side2[0]/2/s2 - side2[0]*(r1*r1-r2*r2)/s2/s2/s2/2 */;
  dddadxdrdx[0][2][0] = dddadxdxdr[0][0][2];
  dddadxdrdx[0][2][1] = dddadxdxdr[0][1][2];
  dddadxdrdx[0][2][2] = dddadxdxdr[0][2][2]; 
  dddadxdrdr[0][2][0] = -side2[0]*2*r1/s2/s2/s2/2 +
     + (-side2[0]/2*(-1)/s2/s2 - side2[0]*(r1*r1-r2*r2)*(-3)/s2/s2/s2/s2/2) *
        (r1-r2)/s2;
  dddadxdrdr[0][2][1] = 0;
  dddadxdrdr[0][2][2] = -side2[0]*(-2)*r2/s2/s2/s2/2 +
     + (-side2[0]/2*(-1)/s2/s2 - side2[0]*(r1*r1-r2*r2)*(-3)/s2/s2/s2/s2/2) *
        (r2-r1)/s2;



  /* ddadxdr[0][0] = -side1[0]/2/s1 + side1[0]*(r1*r1-r0*r0)/s1/s1/s1/2 
                  -side2[0]/2/s2 + side2[0]*(r1*r1-r2*r2)/s2/s2/s2/2;   */
  dddadxdrdx[0][0][0] = dddadxdxdr[0][0][0];
  dddadxdrdx[0][0][1] = dddadxdxdr[0][1][0];
  dddadxdrdx[0][0][2] = dddadxdxdr[0][2][0]; 
  dddadxdrdr[0][0][0] = side1[0]*2*r1/s1/s1/s1/2 + side2[0]*2*r1/s2/s2/s2/2
    + (side1[0]/2/s1/s1 + side1[0]*(r1*r1-r0*r0)*(-3)/s1/s1/s1/s1/2)*(r1-r0)/s1
    + (side2[0]/2/s2/s2 + side2[0]*(r1*r1-r2*r2)*(-3)/s2/s2/s2/s2/2)*(r1-r2)/s2;
  dddadxdrdr[0][0][1] = side1[0]*(-2)*r0/s1/s1/s1/2 
    + (side1[0]/2/s1/s1 + side1[0]*(r1*r1-r0*r0)*(-3)/s1/s1/s1/s1/2)*(r0-r1)/s1;
  dddadxdrdr[0][0][2] = side2[0]*(-2)*r2/s2/s2/s2/2
    + (side2[0]/2/s2/s2 + side2[0]*(r1*r1-r2*r2)*(-3)/s2/s2/s2/s2/2)*(r2-r1)/s2;

  /* ddadrdx[0][1] = ((r1*r1-r0*r0)/2*(-1)/s1/s1 + 1/2.)*(side1[0]/s1); */
  dddadrdxdx[0][1][0] = ((r1*r1-r0*r0)/2*(-1)/s1/s1 + 1/2.)*(-1/s1)
    + side1[0]*((r1*r1-r0*r0)/2*(-1)*(-3)/s1/s1/s1/s1 + 1/2.*(-1)/s1/s1)
        *(-side1[0])/s1;
  dddadrdxdx[0][1][1] = ((r1*r1-r0*r0)/2*(-1)/s1/s1 + 1/2.)*(1/s1)
    + side1[0]*((r1*r1-r0*r0)/2*(-1)*(-3)/s1/s1/s1/s1 + 1/2.*(-1)/s1/s1)
        *(side1[0])/s1;
  dddadrdxdx[0][1][2] = 0;
  dddadrdxdr[0][1][0] = 2*r1/2*(-1)/s1/s1*(side1[0]/s1) +
         side1[0]*((r1*r1-r0*r0)/2*(-1)*(-3)/s1/s1/s1/s1 
         + 1/2.*(-1)/s1/s1)*(r1-r0)/s1;
  dddadrdxdr[0][1][1] = -2*r0/2*(-1)/s1/s1*(side1[0]/s1) +
          side1[0]*((r1*r1-r0*r0)/2*(-1)*(-3)/s1/s1/s1/s1 
         + 1/2.*(-1)/s1/s1)*(r0-r1)/s1;
  dddadrdxdr[0][1][2] = 0;

  /* ddadrdx[0][2] = ((r1*r1-r2*r2)/2*(-1)/s2/s2 + 1/2.)*(side2[0]/s2); */
  dddadrdxdx[0][2][0] = ((r1*r1-r2*r2)/2*(-1)/s2/s2 + 1/2.)*(-1/s2)
    + side2[0]*((r1*r1-r2*r2)/2*(-1)*(-3)/s2/s2/s2/s2 + 1/2.*(-1)/s2/s2)
        *(-side2[0])/s2;
  dddadrdxdx[0][2][1] = 0;
  dddadrdxdx[0][2][2] = ((r1*r1-r2*r2)/2*(-1)/s2/s2 + 1/2.)*(1/s2)
    + side2[0]*((r1*r1-r2*r2)/2*(-1)*(-3)/s2/s2/s2/s2 + 1/2.*(-1)/s2/s2)
        *(side2[0])/s2;
  dddadrdxdr[0][2][0] = 2*r1/2*(-1)/s2/s2*(side2[0]/s2) +
         side2[0]*((r1*r1-r2*r2)/2*(-1)*(-3)/s2/s2/s2/s2 
         + 1/2.*(-1)/s2/s2)*(r1-r2)/s2;
  dddadrdxdr[0][2][1] = 0;
  dddadrdxdr[0][2][2] = -2*r2/2*(-1)/s2/s2*(side2[0]/s2) +
          side2[0]*((r1*r1-r2*r2)/2*(-1)*(-3)/s2/s2/s2/s2 
         + 1/2.*(-1)/s2/s2)*(r2-r1)/s2;


  /* ddadrdx[0][0] = -ddadrdx[0][1]-ddadrdx[0][2]; */
  dddadrdxdx[0][0][0] = -dddadrdxdx[0][1][0]-dddadrdxdx[0][2][0];
  dddadrdxdx[0][0][1] = -dddadrdxdx[0][1][1]-dddadrdxdx[0][2][1];
  dddadrdxdx[0][0][2] = -dddadrdxdx[0][1][2]-dddadrdxdx[0][2][2];
  dddadrdxdr[0][0][0] = -dddadrdxdr[0][1][0]-dddadrdxdr[0][2][0];
  dddadrdxdr[0][0][1] = -dddadrdxdr[0][1][1]-dddadrdxdr[0][2][1];
  dddadrdxdr[0][0][2] = -dddadrdxdr[0][1][2]-dddadrdxdr[0][2][2];

  /* ddadrdr[0][0] = r1/s1 + (-(r1*r1-r0*r0)/s1/s1/s1 + 1/s1)*(r1-r0)/2
                + r1/s2 + (-(r1*r1-r2*r2)/s2/s2/s2 + 1/s2)*(r1-r2)/2;   */
  dddadrdrdx[0][0][0] = dddadrdxdr[0][0][0];
  dddadrdrdx[0][0][1] = dddadrdxdr[0][1][0];
  dddadrdrdx[0][0][2] = dddadrdxdr[0][2][0];
  dddadrdrdr[0][0][0] = 1/s1 - 2*r1/s1/s1/s1*(r1-r0)/2
           + (-(r1*r1-r0*r0)/s1/s1/s1 + 1/s1)/2
          + (-r1/s1/s1 + (-(r1*r1-r0*r0)*(-3)/s1/s1/s1/s1 - 1/s1/s1)*(r1-r0)/2)
               *(r1-r0)/s1
                   +  1/s2 - 2*r1/s2/s2/s2*(r1-r2)/2
           + (-(r1*r1-r2*r2)/s2/s2/s2 + 1/s2)/2
          + (-r1/s2/s2 + (-(r1*r1-r2*r2)*(-3)/s2/s2/s2/s2 - 1/s2/s2)*(r1-r2)/2)
               *(r1-r2)/s2;
  dddadrdrdr[0][0][1] =   2*r0/s1/s1/s1*(r1-r0)/2
           - (-(r1*r1-r0*r0)/s1/s1/s1 + 1/s1)/2
          + (-r1/s1/s1 + (-(r1*r1-r0*r0)*(-3)/s1/s1/s1/s1 - 1/s1/s1)*(r1-r0)/2)
               *(r0-r1)/s1;
  dddadrdrdr[0][0][2] =  2*r2/s2/s2/s2*(r1-r2)/2
           - (-(r1*r1-r2*r2)/s2/s2/s2 + 1/s2)/2
          + (-r1/s2/s2 + (-(r1*r1-r2*r2)*(-3)/s2/s2/s2/s2 - 1/s2/s2)*(r1-r2)/2)
               *(r2-r1)/s2;


  /* ddadrdr[0][1] = -r0/s1 +(-(r1*r1-r0*r0)/s1/s1/s1 + 1/s1)*(r0-r1)/2; */
  dddadrdrdx[0][1][0] = dddadrdxdr[0][0][1];
  dddadrdrdx[0][1][1] = dddadrdxdr[0][1][1];
  dddadrdrdx[0][1][2] = dddadrdxdr[0][2][1];
  dddadrdrdr[0][1][0] = dddadrdrdr[0][0][1];
  dddadrdrdr[0][1][1] = -1/s1 + 2*r0/s1/s1/s1*(r0-r1)/2 
          + (-(r1*r1-r0*r0)/s1/s1/s1 + 1/s1)/2
        + (r0/s1/s1 + (-(r1*r1-r0*r0)*(-3)/s1/s1/s1/s1 + (-1)/s1/s1)*(r0-r1)/2)
          *(r0-r1)/s1;
  dddadrdrdr[0][1][2] = 0;


  /* ddadrdr[0][2] = -r2/s2 + ((r1*r1-r2*r2)/2*(-1)/s2/s2 + 0.5)/s2*(r2-r1); */
  dddadrdrdx[0][2][0] = dddadrdxdr[0][0][2];
  dddadrdrdx[0][2][1] = dddadrdxdr[0][1][2];
  dddadrdrdx[0][2][2] = dddadrdxdr[0][2][2];
  dddadrdrdr[0][2][0] = dddadrdrdr[0][0][2];
  dddadrdrdr[0][2][1] = dddadrdrdr[0][1][2];
  dddadrdrdr[0][2][2] = -1/s2 + 2*r2/s2/s2/s2*(r2-r1)/2 
          + (-(r1*r1-r2*r2)/s2/s2/s2 + 1/s2)/2
        + (r2/s2/s2 + (-(r1*r1-r2*r2)*(-3)/s2/s2/s2/s2 + (-1)/s2/s2)*(r2-r1)/2)
          *(r2-r1)/s2;


  for ( i = 0 ; i < 3 ; i++ )
    for ( j = 0 ; j < 3 ; j++ )
    { ddhdxdx[i][j] = sign*(
      (-.5)*.5/root/root/root/area*
         (2*dadx[0]*ddadxdx[0][i] + 2*dadr[0]*ddadrdx[0][i])
        *(2*dadx[0]*ddadxdx[0][j] + 2*dadr[0]*ddadrdx[0][j])
   + (-1)*.5/root/area/area*dadx[j]*
         (2*dadx[0]*ddadxdx[0][i] + 2*dadr[0]*ddadrdx[0][i])
   + .5/root/area*
         (2*ddadxdx[0][j]*ddadxdx[0][i] + 2*ddadrdx[0][j]*ddadrdx[0][i])
   + .5/root/area*
         (2*dadx[0]*dddadxdxdx[0][i][j] + 2*dadr[0]*dddadrdxdx[0][i][j])
     )
         - dhdx[j]/area*dadx[i]
         - (-1)*h/area/area*dadx[j]*dadx[i]
         - h/area*ddadxdx[i][j];

      ddhdxdr[i][j] = sign*(
      (-.5)*.5/root/root/root/area*
         (2*dadx[0]*ddadxdx[0][i] + 2*dadr[0]*ddadrdx[0][i])
        *(2*dadx[0]*ddadxdr[0][j] + 2*dadr[0]*ddadrdr[0][j])
   + (-1)*.5/root/area/area*dadr[j]*
         (2*dadx[0]*ddadxdx[0][i] + 2*dadr[0]*ddadrdx[0][i])
   + .5/root/area*
         (2*ddadxdr[0][j]*ddadxdx[0][i] + 2*ddadrdr[0][j]*ddadrdx[0][i])
   + .5/root/area*
         (2*dadx[0]*dddadxdxdr[0][i][j] + 2*dadr[0]*dddadrdxdr[0][i][j])
     )
         - dhdr[j]/area*dadx[i]
         - (-1)*h/area/area*dadr[j]*dadx[i]
         - h/area*ddadxdr[i][j];

      ddhdrdx[i][j] = sign*(
      (-.5)*.5/root/root/root/area*
         (2*dadx[0]*ddadxdr[0][i] + 2*dadr[0]*ddadrdr[0][i])
        *(2*dadx[0]*ddadxdx[0][j] + 2*dadr[0]*ddadrdx[0][j])
   + (-1)*.5/root/area/area*dadx[j]*
         (2*dadx[0]*ddadxdr[0][i] + 2*dadr[0]*ddadrdr[0][i])
   + .5/root/area*
         (2*ddadxdx[0][j]*ddadxdr[0][i] + 2*ddadrdx[0][j]*ddadrdr[0][i])
   + .5/root/area*
         (2*dadx[0]*dddadxdrdx[0][i][j] + 2*dadr[0]*dddadrdrdx[0][i][j])
     )
         - dhdx[j]/area*dadr[i]
         - (-1)*h/area/area*dadx[j]*dadr[i]
         - h/area*ddadrdx[i][j];

      ddhdrdr[i][j] = sign*(
      (-.5)*.5/root/root/root/area*
         (2*dadx[0]*ddadxdr[0][i] + 2*dadr[0]*ddadrdr[0][i])
        *(2*dadx[0]*ddadxdr[0][j] + 2*dadr[0]*ddadrdr[0][j])
   + (-1)*.5/root/area/area*dadr[j]*
         (2*dadx[0]*ddadxdr[0][i] + 2*dadr[0]*ddadrdr[0][i])
   + .5/root/area*
         (2*ddadxdr[0][j]*ddadxdr[0][i] + 2*ddadrdr[0][j]*ddadrdr[0][i])
   + .5/root/area*
         (2*dadx[0]*dddadxdrdr[0][i][j] + 2*dadr[0]*dddadrdrdr[0][i][j])
     )
         - dhdr[j]/area*dadr[i]
         - (-1)*h/area/area*dadr[j]*dadr[i]
         - h/area*ddadrdr[i][j];
  } 

  if ( power == 2.0 ) 
  { for ( i = 0 ; i < 3 ; i++ )
     for ( j = 0 ; j < 3 ; j++ )
     { v_info->hess[i][j][0][0] = M_PI*( 2*dhdx[j]*dhdx[i]*area 
             + 2*dhdx[j]*hh*dadx[i] + 2*hh*ddhdxdx[i][j]*area 
             + hh*hh*ddadxdx[i][j] + 2*hh*dhdx[i]*dadx[j]);
       v_info->hess[i][j][0][1] = M_PI*( 2*dhdr[j]*dhdx[i]*area 
             + 2*dhdr[j]*hh*dadx[i] + 2*hh*ddhdxdr[i][j]*area 
             + hh*hh*ddadxdr[i][j] + 2*hh*dhdx[i]*dadr[j]);
       v_info->hess[i][j][1][0] = M_PI*( 2*dhdx[j]*dhdr[i]*area 
             + 2*dhdx[j]*hh*dadr[i] + 2*hh*ddhdrdx[i][j]*area 
             + hh*hh*ddadrdx[i][j] + 2*hh*dhdr[i]*dadx[j]);
       v_info->hess[i][j][1][1] = M_PI*( 2*dhdr[j]*dhdr[i]*area 
             + 2*dhdr[j]*hh*dadr[i] + 2*hh*ddhdrdr[i][j]*area 
             + hh*hh*ddadrdr[i][j] + 2*hh*dhdr[i]*dadr[j]);
     }
  }
  else if ( power == 1.0 ) 
  { for ( i = 0 ; i < 3 ; i++ )
     for ( j = 0 ; j < 3 ; j++ )
     { v_info->hess[i][j][0][0] = M_PI*(ddhdxdx[i][j]*area + dhdx[j]*dadx[i]
                               + dhdx[i]*dadx[j] + hh*ddadxdx[i][j]);
       v_info->hess[i][j][0][1] = M_PI*(ddhdxdr[i][j]*area + dhdr[j]*dadx[i]
                               + dhdx[i]*dadr[j] + hh*ddadxdr[i][j]);
       v_info->hess[i][j][1][0] = M_PI*(ddhdxdr[i][j]*area + dhdx[j]*dadr[i]
                               + dhdr[i]*dadx[j] + hh*ddadxdr[i][j]);
       v_info->hess[i][j][1][1] = M_PI*(ddhdrdr[i][j]*area + dhdr[j]*dadr[i]
                               + dhdr[i]*dadr[j] + hh*ddadrdr[i][j]);
     }
  }
  else 
  { REAL pp = pow(fabs(hh),power-1);
    for ( i = 0 ; i < 3 ; i++ )
     for ( j = 0 ; j < 3 ; j++ )
     { v_info->hess[i][j][0][0] = M_PI*(
           power*(power-1)*pp/hh*dhdx[j]*dhdx[i]*area + power*pp*dhdx[j]*dadx[i]
         + power*pp*ddhdxdx[i][j]*area + pp*hh*ddadxdx[i][j]
         + power*pp*dhdx[i]*dadx[j]);
       v_info->hess[i][j][0][1] = M_PI*(
           power*(power-1)*pp/hh*dhdr[j]*dhdx[i]*area + power*pp*dhdr[j]*dadx[i]
         + power*pp*ddhdxdr[i][j]*area + pp*hh*ddadxdr[i][j]
         + power*pp*dhdx[i]*dadr[j]);
       v_info->hess[i][j][1][0] = M_PI*(
           power*(power-1)*pp/hh*dhdx[j]*dhdr[i]*area + power*pp*dhdx[j]*dadr[i]
         + power*pp*ddhdxdr[i][j]*area + pp*hh*ddadxdr[i][j]
         + power*pp*dhdr[i]*dadx[j]);
       v_info->hess[i][j][1][1] = M_PI*(
           power*(power-1)*pp/hh*dhdr[j]*dhdr[i]*area + power*pp*dhdr[j]*dadr[i]
         + power*pp*ddhdrdr[i][j]*area + pp*hh*ddadrdr[i][j]
         + power*pp*dhdr[i]*dadr[j]);
     }
  }

  return energy;

}

/************************************************************************
*
*  function: sq_mean_curv_cyl_value()
*
*  purpose:  Calculate square curvature energy for string model
*            Works locally vertex by vertex. Assumes two edges per vertex.
*
*/

REAL sq_mean_curv_cyl_value(v_info)
struct qinfo *v_info;
{
  return sq_mean_curv_cyl_all(v_info,METHOD_VALUE,2.0);
}

/************************************************************************
*
*  function: sq_mean_curv_cyl_grad()
*
*  purpose:  Calculate square curvature gradient for string model
*            Works locally vertex by vertex. Assumes two edges per vertex.
*
*/

REAL sq_mean_curv_cyl_grad(v_info)
struct qinfo *v_info;
{
  return sq_mean_curv_cyl_all(v_info,METHOD_GRADIENT,2.0);
}

/************************************************************************
*
*  function: sq_mean_curv_cyl_hess()
*
*  purpose:  Calculate square curvature hessian for string model
*            Works locally vertex by vertex. Assumes two edges per vertex.
*
*/

REAL sq_mean_curv_cyl_hess(v_info)
struct qinfo *v_info;
{
  return sq_mean_curv_cyl_all(v_info,METHOD_HESSIAN,2.0);
}

/*************************************************************************
   Square gaussian curvature for surface of revolution in string model.
   Contributed by John Frank, jrf@mit.edu

   Cylindrical axis is x axis.
   Includes 2*pi factor.
**************************************************************************/

REAL sq_gauss_curv_cyl_all ARGS(( struct qinfo *, int));

/************************************************************************
*
*  function: sq_gauss_curv_cyl_init()
*
*  purpose:  Any overall initialization needed each time the method
*            is evaluated.
*/
void sq_gauss_curv_cyl_init(mode,mi)
int mode;
struct method_instance *mi;
{ 
  if ( web.modeltype != LINEAR )
     kb_error(4486,"sq_gaussian_curv_cyl method only for LINEAR model.\n",
        RECOVERABLE);

}

/************************************************************************
*
* function: sq_gauss_curv_cyl_all()
*
* purpose: Combined calculations for one vertex for sq_gaussian_curv_cyl
*          method.
*/

REAL sq_gauss_curv_cyl_all(v_info,mode)
struct qinfo *v_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */
{ REAL energy = 0.0;

  /* Notes on incoming data:
       v_info->vcount is number of vertices in v_info; it will be 3
           for an interior vertex and 2 for an endpoint.
       v_info->x[0][] is the coordinates of the vertex in question.
       v_info->x[1][] is the coordinates of one neighbor vertex.
       v_info->x[2][] is the coordinates of the other neighbor vertex.
       v_info->sides[0][0][] is the vector from vertex 0 to vertex 1.
       v_info->sides[0][1][] is the vector from vertex 0 to vertex 2.
  */

  /* calculate value for single vertex here */

  if ( mode == METHOD_VALUE )
    return energy;

  /* Gradient */

  /* calculate gradient for the energy of the vertex here */

  if ( mode == METHOD_GRADIENT )
    return energy;

  /*  Hessian */

  /* calculate hessian for the energy of the vertex here */

  return energy;

}

/************************************************************************
*
*  function: sq_gauss_curv_cyl_value()
*
*  purpose:  Calculate square gaussian curvature energy for surface
*            of revolution in string model.
*            Works locally vertex by vertex. Assumes two edges per vertex.
*
*/

REAL sq_gauss_curv_cyl_value(v_info)
struct qinfo *v_info;
{
  return sq_gauss_curv_cyl_all(v_info,METHOD_VALUE);
}

/************************************************************************
*
*  function: sq_gauss_curv_cyl_grad()
*
*  purpose:  Calculate square gaussian curvature gradient for 
*            surface of revolution in string model.
*
*/

REAL sq_gauss_curv_cyl_grad(v_info)
struct qinfo *v_info;
{
  return sq_gauss_curv_cyl_all(v_info,METHOD_GRADIENT);
}

/************************************************************************
*
*  function: sq_gauss_curv_cyl_hess()
*
*  purpose:  Calculate square curvature hessian for surface of revolution
*            in string model.
*
*/

REAL sq_gauss_curv_cyl_hess(v_info)
struct qinfo *v_info;
{
  return sq_gauss_curv_cyl_all(v_info,METHOD_HESSIAN);
}
