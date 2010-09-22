/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*********************************************************************
*
* file:    filml.c
*
* Contents:  Functions calculating energy, volume, and their
*            gradients for the LINEAR SOAPFILM model.
*/

#include "include.h"
REAL wee_area = 0.0;

/*********************************************************************
*
*  Function: facet_energy_l()
*
*  Purpose:  Calculates energy due to facet for LINEAR SOAPFILM.
*        Also does facet quantity integrands.
*/

void facet_energy_l(f_id,mode)
facet_id f_id;
int mode; /* AREA_ONLY or ALL_ENERGY */
{
  int i,j;
  REAL side[FACET_EDGES][MAXCOORD];
  REAL normal[MAXCOORD];
  REAL wulff[MAXCOORD];    /* energy covector to area normal vector */
  REAL energy;
  body_id b_id;
  REAL *x[FACET_VERTS];
  REAL unwrap_x[FACET_VERTS][MAXCOORD];
  REAL z[FACET_VERTS];  /* for calculating average z*z */
  REAL zz;     /* for average z*z, for gravity */
  REAL u;      /* gravitational energy */
  facetedge_id fe_id;
  vertex_id v_id[FACET_VERTS];
  REAL ss,st,tt;
  REAL det;
#ifdef THREADS
  struct thread_data *data = GET_THREAD_DATA;
#endif
  int_val = ordinal(get_original(f_id))+1;  /* for eval  of file parameters */

  /* get side vectors */
  fe_id = get_facet_fe(f_id);
  for ( i = 0 ; i < FACET_EDGES ; i++ )
  { 
    v_id[i] = get_fe_tailv(fe_id);
    x[i] = unwrap_x[i];
    fe_id = get_next_edge(fe_id);
  }

  get_facet_verts(f_id,x,NULL);  /* in tail order */
  for ( i = 0 ; i < FACET_EDGES ; i++ )
  { int ii = (i+1)%FACET_EDGES;
    for ( j = 0 ; j < SDIM ; j++ )
      side[i][j] = x[ii][j] - x[i][j];
    z[i] = x[i][2];
  } 
     
  if ( web.metric_flag )
  { if ( klein_metric_flag )
      energy = klein_area(x);
    else energy = simplex_energy_metric(v_id,x);
    set_facet_area(f_id,energy);
    goto skip_from_metric;
  }

  /* calculate surface tension energy */
  ss = SDIM_dot(side[0],side[0]);
  st = SDIM_dot(side[0],side[1]);
  tt = SDIM_dot(side[1],side[1]);
  det = ss*tt-st*st;
  if ( det > 0.0 ) energy = sqrt(det)/2;
  else energy = 0.0;
  set_facet_area(f_id,energy);

  if ( mode == AREA_ONLY ) return;

  if ( web.wulff_flag ) 
  {
    /* calculate normal */ 
    cross_prod(side[0],side[1],normal);
    (*get_wulff)(normal,wulff);
    energy = SDIM_dot(wulff,normal)/2;
  }

  /* do square curvature if wanted */
  if ( ((square_curvature_flag | mean_curv_int_flag) & EVALUATE )
        && !kusner_flag && !conf_edge_curv_flag )
     sqcurve_energy(v_id,side);


skip_from_metric:

#ifdef THREADS
  if ( threadflag )
    data->total_area = data->total_area + energy; /* Borland C bug */
  else
#endif
  binary_tree_add(web.total_area_addends,energy);

  if ( get_fattr(f_id) & DENSITY )
     energy = energy * get_facet_density(f_id);  /* Borland C bug */
      
  /* add gravitational energy, vector potential z*z/2*k  */
  if ( web.gravflag && !(get_fattr(f_id) & NONCONTENT) )
    {
      zz = (z[0]*z[0]+z[1]*z[1]+z[2]*z[2]+z[0]*z[1]+z[1]*z[2]+z[0]*z[2])/6;
      u = zz*(side[0][0]*side[1][1]-side[0][1]*side[1][0])/2/2;  
          /* half for area, half from potential */
      b_id = get_facet_body(f_id);
      if ( valid_id(b_id) )
      energy += u*get_body_density(b_id)*web.grav_const;
      b_id = get_facet_body(facet_inverse(f_id));
      if ( valid_id(b_id) )
      energy -= u*get_body_density(b_id)*web.grav_const;
    }

#ifdef THREADS
  if ( threadflag )
    data->total_energy = data->total_energy + energy; /* Borland C bug */
  else
#endif
  binary_tree_add(web.total_energy_addends,energy); 

} /* end facet_energy_l() */
 

/************************************************************************
*
*  Function: facet_force_l()
*
*  Purpose: Calculates all forces on control points due to facet and
*  accumulates them at each control point.
*/

void facet_force_l(f_id)
facet_id f_id;
{
  REAL side[FACET_EDGES][MAXCOORD];
  REAL normal[MAXCOORD];
  REAL wulff[MAXCOORD]; /* area vector covector for energy */
  REAL temp[MAXCOORD];
  int i,j;
  REAL area;
  facetedge_id fe_id;
  REAL *x[FACET_VERTS];
  REAL z[FACET_VERTS];  /* for gravitational forces */
  body_id b_id;
  REAL density = get_facet_density(f_id);
  vertex_id v_id[FACET_VERTS];
  edge_id    e_id[FACET_EDGES];
  REAL unwrap_x[FACET_VERTS][MAXCOORD];
  WRAPTYPE wraps[FACET_VERTS];
  REAL forces[FACET_VERTS][MAXCOORD];  /* total forces from this facet */
  REAL *forceptr[FACET_VERTS];    /* pointers to forces */
  REAL ss,st,tt;

  memset((char*)forces,0,sizeof(forces));  /* set to 0 */
  int_val = ordinal(get_original(f_id))+1;  /* for eval  of file parameters */
  /* get side vectors */
  fe_id = get_facet_fe(f_id);
  for ( i = 0 ; i < FACET_EDGES ; i++ )
  { 
    e_id[i] = get_fe_edge(fe_id);
    v_id[i] = get_edge_tailv(e_id[i]);
    x[i] = unwrap_x[i];
    forceptr[i] = forces[i];
    fe_id = get_next_edge(fe_id);
  }
  get_facet_verts(f_id,x,wraps);  /* verts in tail order */
  for ( i = 0 ; i < FACET_EDGES ; i++ )
  { int ii = (i+1)%FACET_EDGES;
    for ( j = 0 ; j < SDIM ; j++ )
      side[i][j] = x[ii][j] - x[i][j];
    z[i] = x[i][2];
  } 
  if ( web.metric_flag )
  { if ( klein_metric_flag )
    { klein_area_grad(x,forceptr);
        for ( i = 0 ; i < FACET_VERTS ; i++ )
          for ( j = 0 ; j < SDIM ; j++ )
              forceptr[i][j] *= density;
         area = klein_area(x);
    }
    else
      { simplex_force_metric(v_id,x,density,forceptr);
        area = simplex_energy_metric(v_id,x);
      }
    goto end_euclidean;
  }

  if ( ((square_curvature_flag | mean_curv_int_flag) & EVALUATE)
        && !kusner_flag && !conf_edge_curv_flag ) 
     sqcurve_force(v_id,e_id,side);

  /* calculate area */ 
  ss = SDIM_dot(side[0],side[0]);
  st = SDIM_dot(side[0],side[1]);
  tt = SDIM_dot(side[1],side[1]);
  area = ss*tt-st*st;
  if ( area < 0.0 ) area = 0.0; /* stupid inaccurate computers! */
  area = sqrt(area)/2;
  set_facet_area(f_id,area);

  /* an error check, and accommodation for possibly deliberately
      degenerate triangles on boundary */
  if ( area <= wee_area )
  { facetedge_id ffe;
      
    ffe = fe_id;
    sprintf(errmsg,"WARNING! Zero area for facet %s.\n",ELNAME(f_id));
    outstring(errmsg);
    if ( itdebug )
    {
      outstring("Facet-edges and side vectors: \n");
      for ( i = 0 ; i < FACET_EDGES ; i++, ffe = get_next_edge(ffe) )
      { 
         sprintf(msg," %8lX    %18.15f %18.15f %18.15f\n",ffe,
             (DOUBLE)side[i][0],(DOUBLE)side[i][1],(DOUBLE)side[i][2]);
         outstring(msg);
      }
      prompt("Hit RETURN to continue.",msg,msgmax);
    }
    return;
  }

  /* get energy covector */
  if ( web.wulff_flag )
  {
     /* calculate normal */ 
     cross_prod(side[0],side[1],normal);
     (*get_wulff)(normal,wulff);
     if ( get_fattr(f_id) & DENSITY )
       for ( i = 0 ; i < SDIM ; i++ )
        wulff[i] *= density;
     /* force on each vertex */
     for ( i = 0 ; i < FACET_VERTS ; i++ )  /* vertex loop */
      { int k;
        j = (i+1)%FACET_EDGES;  /* opposite side */
        cross_prod(side[j],wulff,temp);
        for ( k = 0 ; k < SDIM ; k++ ) forces[i][k] += temp[k]/2;
      }
  }
  else
  { REAL coeff = density/4/area;
     int k;
     for ( k = 0 ; k < SDIM ; k++ )
     { forces[0][k] += coeff*(side[0][k]*tt - st*side[1][k]);
       forces[1][k] -= coeff*(side[0][k]*tt - st*side[1][k]);
       forces[1][k] += coeff*(ss*side[1][k] - st*side[0][k]);
       forces[2][k] -= coeff*(ss*side[1][k] - st*side[0][k]);
     }
  }
  
  /* gravity forces, negative of gravity energy gradient */
  if ( web.gravflag && !(get_fattr(f_id) & NONCONTENT) )
  {  REAL zz;     /* average z*z          */
     REAL gdensity;  /* net density difference of bodies across facet */
     REAL normz;     /* facet normal component in z direction */
     zz = (z[0]*z[0]+z[1]*z[1]+z[2]*z[2]+z[0]*z[1]+z[1]*z[2]+z[0]*z[2])/6;
     b_id = get_facet_body(f_id);
     gdensity = 0.0;
     if ( valid_id(b_id) )
      gdensity += get_body_density(b_id);
     b_id = get_facet_body(facet_inverse(f_id));
     if ( valid_id(b_id) )
      gdensity -= get_body_density(b_id);
     normz = side[0][0]*side[1][1] - side[0][1]*side[1][0];
     for ( i = 0 ; i < FACET_VERTS ; i++ )  /* vertex loop */
     {
       j = (i+1)%FACET_EDGES;  /* opposite side */
       forces[i][0] += web.grav_const*gdensity*side[j][1]*zz/4;
       forces[i][1] -= web.grav_const*gdensity*side[j][0]*zz/4;
       forces[i][2] -= web.grav_const*gdensity*normz*(z[i]+z[0]+z[1]+z[2])/24;
     }
  }

end_euclidean:
  /* accumulate star area around each vertex and edge */
   fe_id = get_facet_fe(f_id);
   for ( i = 0 ; i < FACET_VERTS ; i++ )  /* vertex loop */
     { edge_id ee_id;
       vertex_id vv_id;
       ee_id = get_fe_edge(fe_id);
       vv_id = get_edge_headv(ee_id);
       add_vertex_star(vv_id,area);
       add_edge_star(ee_id,area);
       fe_id = get_next_edge(fe_id);
     }

  /* add to totals, unwrapping if necessary */
  for ( i = 0 ; i < FACET_VERTS ; i++ )  /* vertex loop */
  { REAL *force; 
    REAL wforce[MAXCOORD];  /* unwrapped forces */

    force = get_force(v_id[i]);
    if ( web.symmetry_flag )
    { (*sym_form_pullback)(get_coord(v_id[i]),wforce,forces[i],wraps[i]);
      for ( j = 0 ; j < SDIM ; j++ )
        force[j] += wforce[j];
    }
    else
    { for ( j = 0 ; j < SDIM ; j++ )
        force[j] += forces[i][j];
    }
  }

}  /* end facet_force_l() */


/**********************************************************************
*
*  Function: facet_volume_l()
*
*  Purpose: Find triangle's contribution to volumes of neighboring bodies.
*  Volumes with respect to origin are calculated for each
*        face, and then oriented contributions added for each body. 
*/

void facet_volume_l(f_id)
facet_id f_id;
{ 
  body_id b_id0,b_id1;
  facetedge_id fe_id;
  REAL vol;
     
  if ( get_fattr(f_id) & NONCONTENT ) return;

  int_val = ordinal(get_original(f_id))+1;  /* for eval  of file parameters */
  b_id0 = get_facet_body(f_id);
  b_id1 = get_facet_body(facet_inverse(f_id));
  if ( !valid_id(b_id0) && !valid_id(b_id1) ) return;
     
  if ( web.symmetric_content )
  { vertex_id v1,v2,v3;
    facetedge_id next_fe;
    fe_id = get_facet_fe(f_id);
    next_fe = get_next_edge(fe_id);
    v1 = get_fe_tailv(fe_id);
    v2 = get_fe_headv(fe_id);
    v3 = get_fe_headv(next_fe);
    vol = triple_prod(get_coord(v1), get_coord(v2), get_coord(v3))/6;
  }
  else
  { MAT2D(x,MAXCOORD,MAXCOORD);
    get_facet_verts(f_id,x,NULL);
    vol = (x[0][2]+x[1][2]+x[2][2])/6*
     ((x[1][0]-x[0][0])*(x[2][1]-x[0][1])-(x[1][1]-x[0][1])*(x[2][0]-x[0][0]));
  }
     
  /* add to body volumes */
  if ( valid_id(b_id0) ) 
    add_body_volume(b_id0,vol);
  
  if ( valid_id(b_id1) ) 
    add_body_volume(b_id1,-vol);

} /* end facet_volume_l() */

/******************************************************************
*    
*  Function: film_grad_l()
*
*  Purpose: Compute volume gradients for vertices on facets.
*          Also fixed quantity gradients.
*/

#define NEWTORVOL

void film_grad_l()
{
  body_id bi_id;  /* identifier for body i */
  body_id bj_id;  /* identifier for body j */
  facetedge_id fe;
  vertex_id v_id;
  facet_id f_id;
  facetedge_id fe_id;
  int i,k;
  volgrad *vgptr;
  REAL z;
  REAL side[FACET_EDGES][MAXCOORD];
  REAL normal[MAXCOORD];
  REAL *x[FACET_VERTS];  /* coordinates of vertices */
  REAL unwrap_x[FACET_VERTS][MAXCOORD];

#ifdef NEWTORVOL
  struct qinfo f_info; /* for calling q_facet_torus_volume */
  if ( web.torus_flag )
    q_info_init(&f_info,METHOD_GRADIENT);
#endif


  if ( sym_flags & NEED_FORM_UNWRAPPING )
     kb_error(1035,"Have to do convert_to_quantities with this symmetry group.\n",RECOVERABLE);

  FOR_ALL_FACETS(f_id)
  { 
    WRAPTYPE wraps[3];
    int_val = ordinal(get_original(f_id))+1;  /* for eval  of file parameters */
    bi_id = get_facet_body(f_id);
    bj_id = get_facet_body(facet_inverse(f_id));
    if ( (!valid_id(bi_id) || !(get_battr(bi_id) & (FIXEDVOL|PRESSURE)) )
     && (!valid_id(bj_id) || !(get_battr(bj_id) & (FIXEDVOL|PRESSURE)) ) )
      continue;

    if ( web.torus_flag )
    { 
#ifdef NEWTORVOL
      int n;
      vertex_id v_ids[FACET_EDGES];
      
      /* get basic info */ 
      fe = get_facet_fe(f_id);
      for ( i = 0 ; i < FACET_EDGES ; i ++ )
      { v_ids[i] = get_fe_tailv(fe);
        fe = get_next_edge(fe);
      }

      f_info.id = f_id;
      q_facet_setup(NULL,&f_info,NEED_SIDE|TORUS_MODULO_MUNGE|ORIENTABLE_METHOD);
      q_facet_torus_volume_grad(&f_info);

      if ( valid_id(bi_id) && (get_battr(bi_id) & (FIXEDVOL|PRESSURE)) )
        for ( i = 0 ; i < FACET_VERTS ; i++ )
        { vgptr = get_bv_new_vgrad(get_body_fixnum(bi_id),v_ids[i]);
          vgptr->bb_id = bi_id;
          for ( n = 0 ; n < SDIM ; n++ )
             vgptr->grad[n] += f_info.grad[i][n];
        }

       if ( valid_id(bj_id) && (get_battr(bj_id) & (FIXEDVOL|PRESSURE)) )
        for ( i = 0 ; i < FACET_VERTS ; i++ )
        { vgptr = get_bv_new_vgrad(get_body_fixnum(bj_id),v_ids[i]);
          vgptr->bb_id = bj_id;
          for ( n = 0 ; n < SDIM ; n++ )
            vgptr->grad[n] -= f_info.grad[i][n];
        }
#else
      /* kludge copy from torvol_project, so could ditch torvol_project */
      REAL *v[FACET_VERTS];  /* pointers to three vertex coordinates */
      int j;
      REAL adj[FACET_EDGES][MAXCOORD];  /* torus wrap adjustments for edge */ 
      vertex_id v_ids[FACET_VERTS];
      int n;
      REAL g[MAXCOORD];


      /* get basic info */
      fe = get_facet_fe(f_id);
      for ( i = 0 ; i < FACET_EDGES ; i ++ )
      {
         v_ids[i] = get_fe_tailv(fe);
         v[i] = get_coord(get_fe_tailv(fe));
         get_edge_adjust(get_fe_edge(fe),adj[i]);
         fe = get_next_edge(fe);
      }

      for ( i = 0 ; i < FACET_EDGES ; i++ )
      {
         int m;
         REAL ga[MAXCOORD],gb[MAXCOORD],gc[MAXCOORD],gd[MAXCOORD],

            ge[MAXCOORD],gf[MAXCOORD]; /* gradient parts */
         j = (i+1)%FACET_EDGES;
         k = (i+2)%FACET_EDGES;

         /* basic tetrahedron */
          cross_prod(v[j],v[k],ga);

         /* torus wrap corrections */
         /* two-vertex term */
         cross_prod(adj[j],v[j],gb);  /* - */
         cross_prod(adj[i],v[k],gc);  /* + */
         cross_prod(adj[k],v[j],gd);  /* + */
         cross_prod(adj[j],v[k],ge);  /* - */

         /* one-vertex term */
         cross_prod(adj[k],adj[i],gf);

         /* add parts to existing gradient */
         for ( m = 0 ; m < SDIM ; m++ )
            g[m] = (ga[m] + (-gb[m]+gc[m]+gd[m]-ge[m])/2 + gf[m])/6;

         if ( valid_id(bi_id) && (get_battr(bi_id) & (FIXEDVOL|PRESSURE)) )
          {
             vgptr = get_bv_new_vgrad(get_body_fixnum(bi_id),v_ids[i]);
             vgptr->bb_id = bi_id;
             for ( n = 0 ; n < SDIM ; n++ )
               vgptr->grad[n] += g[n];
          }

         if ( valid_id(bj_id) && (get_battr(bj_id) & (FIXEDVOL|PRESSURE)) )
          {
            vgptr = get_bv_new_vgrad(get_body_fixnum(bj_id),v_ids[i]);
            vgptr->bb_id = bj_id;
            for ( n = 0 ; n < SDIM ; n++ )
              vgptr->grad[n] -= g[n];
          }
      }
#endif
    }
    else
    {
      vertex_id v[3];
      /* get side vectors */
      fe_id = get_facet_fe(f_id);
      for ( i = 0 ; i < FACET_EDGES ; i++ )
      { 
        get_fe_side(fe_id,side[i]);
        v[i] = get_fe_headv(fe_id);
        x[i] = get_coord(v[i]);
        fe_id = get_next_edge(fe_id);
      }

      if ( web.symmetry_flag )
      {
         for ( i = 0 ; i < FACET_VERTS ; i++ ) x[i] = unwrap_x[i];
         get_facet_verts(f_id,x,wraps);
         /* have to readjust indexing for agreement with nonsym way..
         kludge, but don't want to risk massive changes */
         for ( i = 0 ; i < FACET_VERTS ; i++ ) x[i] = unwrap_x[(i+1)%3]; 
         for ( i = 0 ; i < SDIM ; i++ )
         { side[0][i] = x[0][i] - x[2][i]; 
           side[1][i] = x[1][i] - x[0][i];
           side[2][i] = x[2][i] - x[1][i];
         }
      }

      if ( web.symmetric_content )
      {
         /* do each of the three vertices */
         fe = get_facet_fe(f_id);
         for ( k = 0 ; k < FACET_VERTS ; k++, fe = get_next_edge(fe) )
         {
           fe_id = get_next_edge(fe);
           v_id = get_fe_headv(fe_id);
           if ( get_vattr(v_id) & FIXED ) continue;

           cross_prod(get_coord(get_fe_tailv(fe)),
               get_coord(get_fe_headv(fe)),normal);

           if ( valid_id(bi_id) && (get_battr(bi_id) & (PRESSURE|FIXEDVOL)) )
           { 
             vgptr = get_bv_new_vgrad(get_body_fixnum(bi_id),v_id);
             vgptr->bb_id = bi_id;
             for ( i = 0 ; i < SDIM ; i++ )
                vgptr->grad[i] +=  normal[i]/6.0;
           }

           if ( valid_id(bj_id) && (get_battr(bj_id) & (PRESSURE|FIXEDVOL)) )
           { 
             vgptr = get_bv_new_vgrad(get_body_fixnum(bj_id),v_id);
             vgptr->bb_id = bj_id;
             for ( i = 0 ; i < SDIM ; i++ )
                vgptr->grad[i] -=  normal[i]/6.0;
           }
        }
     }
     else
     {  /* content from integrating z dx dy */
     
        /* get centroid z (divide by 3 later) */
        for ( i = 0,z = 0.0; i < FACET_EDGES ; i++ ) z += x[i][2];
          
        /* calculate normal */ 
        cross_prod(side[0],side[1],normal);

        /* now do each of the three vertices */
     
        fe = get_facet_fe(f_id);
        for ( k = 0 ; k < FACET_VERTS ; k++, fe = get_next_edge(fe) )
        {
          fe_id = get_next_edge(fe);
          v_id = get_fe_headv(fe_id);
          if ( get_vattr(v_id) & FIXED ) continue;

          if ( valid_id(bi_id) && (get_battr(bi_id) & (PRESSURE|FIXEDVOL)) )
          { 
            vgptr = get_bv_new_vgrad(get_body_fixnum(bi_id),v_id);
            vgptr->bb_id = bi_id;
            vgptr->grad[0] += -side[k][1]*z/6.0;
            vgptr->grad[1] +=  side[k][0]*z/6.0;
            vgptr->grad[2] +=  normal[2]/6.0;
          }

          if ( valid_id(bj_id) && (get_battr(bj_id) & (PRESSURE|FIXEDVOL)) )
          { 
            vgptr = get_bv_new_vgrad(get_body_fixnum(bj_id),v_id);
            vgptr->bb_id = bj_id;
            vgptr->grad[0] -= -side[k][1]*z/6.0;
            vgptr->grad[1] -=  side[k][0]*z/6.0;
            vgptr->grad[2] -=  normal[2]/6.0;
          }
        }
      }  

    } /* end torus/nontorus */

  } /* end facet loop */
  
#ifdef NEWTORVOL
  if ( web.torus_flag )
    q_info_free(&f_info);
#endif
} /* end film_grad_l() */

/*******************************************************************
*
*  Function: film_constr_grad()
*
*  Purpose: Add cell volume gradients due to constraint integrals.
*          And quantity integrals.
*/

void film_constr_grad()
{
  edge_id e_id;
  REAL side[MAXCOORD];
  REAL tgrad[MAXCOORD];  
  REAL hgrad[MAXCOORD];  
  REAL grad;  
  int i,j,k,m,sign,bodysign=0;
  REAL green[MAXCOORD];
  REAL green_deriv[MAXCOORD][MAXCOORD];
  REAL midpt[MAXCOORD];
  REAL *tcoord,*hcoord;
  struct constraint *constr;
  vertex_id headv,tailv;
  facetedge_id fe,first_fe;

  FOR_ALL_EDGES(e_id)
  {
    struct volgrad *vgptri;
    facet_id f_id;
    ATTR attr = get_eattr(e_id);
    conmap_t *conmap = get_e_constraint_map(e_id); /* only hit constraints */

    if ( attr & FIXED ) continue;
    if ( !(attr & CONSTRAINT) ) continue;

    headv = get_edge_headv(e_id);
    tailv = get_edge_tailv(e_id);
    tcoord = get_coord(tailv);
    hcoord = get_coord(headv);
    int_val = ordinal(get_original(e_id))+1;  /* for eval  of file parameters */
    for ( j = 0 ; j < SDIM ; j++ )
         side[j] = hcoord[j] - tcoord[j];
    if ( !(attr & BDRY_CONTENT) ) continue;

    if ( web.modeltype == QUADRATIC ) 
    { constr_vol_grad_q(e_id);
      continue;
    }
    else if ( web.modeltype == LAGRANGE )
      kb_error(1036,"constr_vol_grad(): Cannot do LAGRANGE model.\n",RECOVERABLE);

    if ( attr & NEGBOUNDARY ) sign = -1;
    else sign = 1;
    if ( inverted(e_id) ) sign = -sign;

    for ( i = 0 ; i < SDIM ; i++ )
      tgrad[i] = hgrad[i] = 0.0;

    for ( j = 1 ; j <= (int)conmap[0] ; j++ )
    {
      constr = get_constraint(conmap[j]);
      if ( constr->compcount != SDIM ) continue;
      if ( !(constr->attr & CON_CONTENT) ) continue;
      for ( m = 0 ; m <  gauss1D_num ; m++ )
      {
        for ( i = 0 ; i < SDIM ; i++ )
           midpt[i] = gauss1Dpt[m]*hcoord[i] + (1 - gauss1Dpt[m])*tcoord[i];
        for ( i = 0 ; i < SDIM ; i++ )
           eval_all(constr->convect[i],midpt,SDIM,&green[i],
                                     green_deriv[i],e_id);
        for ( i = 0 ; i < SDIM ; i++ )
        { 
          for ( k = 0,grad = 0.0 ; k < SDIM ; k++ )
            grad += side[k]*green_deriv[k][i];
          tgrad[i] += sign*gauss1Dwt[m]*((1-gauss1Dpt[m])*grad - green[i]);
          hgrad[i] += sign*gauss1Dwt[m]*(gauss1Dpt[m]*grad + green[i]);
        }
      }
    }

    fe = first_fe = get_edge_fe(e_id);
    if ( valid_id(fe) ) do
    { f_id = get_fe_facet(fe);
      vgptri = get_vertex_vgrad(tailv);
      for  ( ; vgptri ; vgptri = vgptri->chain )
      {
        if ( !valid_id(vgptri->bb_id) ) continue; /* skip quantities */
        if ( !equal_id(get_facet_body(f_id),vgptri->bb_id) ) 
        { if ( !equal_id(get_facet_body(inverse_id(f_id)),vgptri->bb_id) )
            continue;
          else  bodysign = -sign;
        }
        else bodysign = sign;
        for ( i = 0 ; i < SDIM ; i++ )
        vgptri->grad[i] += bodysign*tgrad[i];  
      }

      vgptri = get_vertex_vgrad(headv);
      for  ( ; vgptri ; vgptri = vgptri->chain )
      {
        if ( !valid_id(vgptri->bb_id) ) continue; /* skip quantities */
        if ( !equal_id(get_facet_body(f_id),vgptri->bb_id) ) 
        { if ( !equal_id(get_facet_body(inverse_id(f_id)),vgptri->bb_id) )
             continue;
          else  bodysign = -sign;
        }
        else bodysign = sign;
        for ( i = 0 ; i < SDIM ; i++ )
        vgptri->grad[i] += bodysign*hgrad[i];  
      }
      fe = get_next_facet(fe);
    } while ( valid_id(fe) && !equal_id(fe,first_fe) );
  }
}  /* end film_constr_grad() */

