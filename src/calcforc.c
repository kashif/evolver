/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/


/*************************************************************
*
*    file:        calcforc.c
*
*    Contents:  Functions calculating energy, volume, and 
*                  their gradients at vertices.
*/

#include "include.h"

#ifdef THREADS
/******************************************************************
*
* function: thread_calc_facet_energy()
*
* purpose: thread-friendly calculation of classic facet energy.
*/

void thread_calc_facet_energy()
{ 
#ifdef OLDTHNEXT
  facet_id f_id;
  THREAD_FOR_ALL_FACETS(f_id)
     (*calc_facet_energy)(f_id,ALL_ENERGIES);
#else
  THREAD_FOR_ALL_NEW(FACET, {(*calc_facet_energy)(*idptr,ALL_ENERGIES);});
#endif
  
}

/******************************************************************
*
* function: thread_calc_facet_forces()
*
* purpose: thread-friendly calculation of classic facet forces.
*/

void thread_calc_facet_forces()
{ 
  
#ifdef OLDTHNEXT
  facet_id f_id;
  THREAD_FOR_ALL_FACETS(f_id)
  {  
     (*calc_facet_forces)(f_id);
  }
#else
  THREAD_FOR_ALL_NEW(FACET,(*calc_facet_forces)(*idptr))
#endif
}
#endif
 /* THREADS */

/*******************************************************************
*
*  Function: calc_energy()
*
*  Purpose:  Wrapper for total energy calculation.
*    
* 
*/

void calc_energy() 
{
#ifdef MPI_EVOLVER
  mpi_calc_energy();
#else
  local_calc_energy();
#endif
}

/*******************************************************************
*
*  Function: local_calc_energy()
*
*  Purpose: Finds total energy of configuration.
*           Requires volumes set to calculate pressure energy.
*
*/

void local_calc_energy() 
{
  int i;
  body_id    b_id;
  edge_id e_id;

  web.total_energy = 0.0;
  web.total_area    = 0.0;
  for ( i = 0 ; i < MAXADDENDS ; i++ ) 
  { web.total_area_addends[i] = 0.0;
    web.total_energy_addends[i] = 0.0;
  }
  web.spring_energy = 0.0;
  euclidean_area = 0.0;

  if ( web.modeltype == LAGRANGE && !quantities_only_flag )
  { if ( auto_convert_flag ) convert_to_quantities();
    else
     kb_error(1774,"LAGRANGE model needs convert_to_quantities.\n",RECOVERABLE);
  }

  if ( min_square_grad_flag ) { web.total_energy = square_grad(); return; }
  if ( quantities_only_flag ) goto quantities_only;

  if ( square_curvature_flag )
  { if ( globals(square_curvature_param)->value.real == 0.0 )
      square_curvature_flag &= ~EVALUATE;
    else square_curvature_flag |= EVALUATE;
  }

  if ( mean_curv_int_flag )
  { if ( globals(square_curvature_param)->value.real == 0.0 )
      mean_curv_int_flag &= ~EVALUATE;
    else mean_curv_int_flag |= EVALUATE;
  }

  if ( sqgauss_flag )
  { if ( globals(sqgauss_param)->value.real == 0.0 )
      sqgauss_flag &= ~EVALUATE;
    else sqgauss_flag |= EVALUATE;
  }

  if ( ((square_curvature_flag | mean_curv_int_flag) & EVALUATE)
          && !kusner_flag && !conf_edge_curv_flag ) 
     sqcurve_energy_init();

  if ( web.representation == SIMPLEX )
  { facet_id f_id;
    FOR_ALL_FACETS(f_id)
      calc_simplex_energy(f_id);
    FOR_ALL_EDGES(e_id)
      calc_simplex_edge_energy(e_id);
  }
  else if ( web.representation == STRING )
  { vertex_id v_id;
            
    FOR_ALL_EDGES(e_id)
    { ATTR attr = get_eattr(e_id);
         
      (*calc_edge_energy)(e_id);
      if ( attr & BDRY_ENERGY )
            calc_constr_energy_e(e_id);  /* substitute surface energy */
    }

    /* boundary energy  and square curvature */
    if ( ((square_curvature_flag | mean_curv_int_flag) & EVALUATE)
                    && !kusner_flag  && !conf_edge_curv_flag ) 
          sqcurve_energy_string_init();
    FOR_ALL_VERTICES(v_id)
    { ATTR attr = get_vattr(v_id);
      if ( ((square_curvature_flag | mean_curv_int_flag) & EVALUATE)
                    && !kusner_flag  && !conf_edge_curv_flag ) 
           sqcurve_energy_string(v_id);
      if ( attr & BDRY_ENERGY )
      { if ( attr & CONSTRAINT )
            calc_constr_energy_v(v_id);
      }
    }
  } /* end STRING */

  else /* web.representation == SOAPFILM */
  {  facet_id  f_id;

    if ( threadflag )
      thread_launch(TH_CALC_FACET_ENERGY,FACET);
    else
    { FOR_ALL_FACETS(f_id)
       (*calc_facet_energy)(f_id,ALL_ENERGIES);
    }

    FOR_ALL_EDGES(e_id)
    { ATTR attr = get_eattr(e_id);
      if ( attr & BDRY_ENERGY ) 
        { if ( attr & CONSTRAINT )
              calc_constr_energy_e(e_id);
        }
      if ( attr & DENSITY )
        (*calc_edge_energy)(e_id);  /* for triple line energies */
    }
  } /* end SOAPFILM */

  if ( ((square_curvature_flag | mean_curv_int_flag) & EVALUATE)
          && !kusner_flag && !conf_edge_curv_flag ) 
     sqcurve_energy_end();

  if ( (square_curvature_flag & EVALUATE) && kusner_flag ) 
      kusner_energy();
  if ( (square_curvature_flag & EVALUATE) && conf_edge_curv_flag ) 
      conf_edge_curv_energy();

  if ( sqgauss_flag & EVALUATE ) sqgauss_energy();

  /* Add kludge forces on boundary edges to prevent pulling away */
  /* Could use more exact test to see if worth calling these  */
  if ( (web.modeltype == LINEAR) && (web.convex_flag) )
  {  
    if ( web.bdrymax > 0 )
     FOR_ALL_EDGES(e_id)
      bdry_spring_energy(e_id);
    if ( web.maxcon > 0 )
     FOR_ALL_EDGES(e_id)
      constr_spring_energy(e_id);
  }

quantities_only:
  FOR_ALL_BODIES(b_id)
  { REAL fix,vol;

    fix = get_body_fixvol(b_id);
    vol = get_body_volume(b_id);
    if ( web.pressure_flag && (get_battr(b_id) & FIXEDVOL) 
      && !quantities_only_flag )
    {
      if ( !equal_id(b_id,web.outside_body) )
      { binary_tree_add(web.total_energy_addends,
           -web.pressure*(fix*log(vol/fix)-(vol-fix)));
        if ( valid_id(web.outside_body) )
          binary_tree_add(web.total_energy_addends,web.pressure*vol);
        set_body_pressure(b_id,web.pressure*fix/vol);     
      }
    }
    else if ( (get_battr(b_id) & PRESSURE) && !quantities_only_flag )
       binary_tree_add(web.total_energy_addends,-get_body_pressure(b_id)*vol);
  }

  if ( gen_quant_count )
     web.total_energy += calc_quants(Q_ENERGY);

  /* finalize web.total_area */
  for ( i = 0 ; i < MAXADDENDS ; i++ )
  { web.total_area += web.total_area_addends[i];
    web.total_energy += web.total_energy_addends[i];
  }

  extrap_val[reflevel] = web.total_energy;

} /* end calc_energy() */


/****************************************************************
*
*  Function: calc_force()
*
*  Purpose: Calls proper energy gradient calculator.
*
*/

void calc_force()  
{
  #ifdef MPI_EVOLVER
  mpi_calc_force();
  #else
  local_calc_force();
  #endif
}

/****************************************************************
*
*  Function: calc_force()
*
*  Purpose: calculates net force at triangulation vertices due
*           to surface tension and constraints
*
*/

void local_calc_force()  
{
  vertex_id v_id;
  facet_id f_id;
  edge_id e_id;
  int i;

  /* zero out vertex cumulative quantities */
  MFOR_ALL_VERTICES(v_id)
  { REAL *f = get_force(v_id);
    for ( i = 0 ; i < SDIM ; i ++ ) f[i] = 0.0;
    set_vertex_star(v_id,0.0);             
    set_vertex_valence(v_id,0);
  }

  if ( min_square_grad_flag ) { square_grad_forces(); return; }
  if ( quantities_only_flag ) goto grad_quantities_only;
 
  if ( square_curvature_flag )
  { if ( globals(square_curvature_param)->value.real == 0.0 )
      square_curvature_flag &= ~EVALUATE;
    else square_curvature_flag |= EVALUATE;
  }

  if ( mean_curv_int_flag )
  { if ( globals(mean_curvature_param)->value.real == 0.0 )
      mean_curv_int_flag &= ~EVALUATE;
    else mean_curv_int_flag |= EVALUATE;
  }

  if ( sqgauss_flag )
  { if ( globals(sqgauss_param)->value.real == 0.0 )
      sqgauss_flag &= ~EVALUATE;
    else sqgauss_flag |= EVALUATE;
  }

  if ( ((square_curvature_flag | mean_curv_int_flag) & EVALUATE) 
            && !kusner_flag && !conf_edge_curv_flag ) 
     sqcurve_force_init();

  if ( unit_normal_flag )  /* for Dennis DeTurck */
  {  
     FOR_ALL_VERTICES(v_id)
     { REAL *f = get_force(v_id);
       facetedge_id fe = get_vertex_fe(v_id);
       if ( inverted(get_fe_facet(fe)) ) fe = get_next_facet(fe);
       calc_vertex_normal(v_id,fe,f);
       for ( i = 0 ; i < SDIM ; i++ )
          f[i] *= deturck_factor;
     }
     /* continue with other forces */
  }

  /* boundary and constraint forces */
  if ( web.representation == SIMPLEX )
  { FOR_ALL_EDGES(e_id)
       calc_simplex_edge_force(e_id);
  }
  else if ( web.representation == STRING )
  { if ( ((square_curvature_flag | mean_curv_int_flag) & EVALUATE)
                    && !kusner_flag && !conf_edge_curv_flag ) 
        sqcurve_energy_string_init();
    FOR_ALL_VERTICES(v_id)
    { ATTR attr = get_vattr(v_id);
      if ( attr &  FIXED ) continue;
      if ( ((square_curvature_flag | mean_curv_int_flag) & EVALUATE)
                    && !kusner_flag && !conf_edge_curv_flag ) 
          sqcurve_force_string(v_id);
      if ( !(attr & BDRY_ENERGY) ) continue;
      if ( attr & CONSTRAINT )
         calc_constr_force_v(v_id);
    }
  }
  else /* SOAPFILM */
  { FOR_ALL_EDGES(e_id)
    { ATTR attr = get_eattr(e_id);
     if ( attr & DENSITY )
       (*calc_edge_forces)(e_id);  /* for triple line energies */
     if ( !(attr & BDRY_ENERGY) ) continue;
     if ( attr & CONSTRAINT )
       calc_constr_force_e(e_id);
    }
  }

  if ( web.representation == SIMPLEX )
  { FOR_ALL_FACETS(f_id)
      calc_simplex_forces(f_id);
  }
  else if ( web.representation == STRING )
  { /* tension forces */
    /* add each edge's contribution to its endpoints */
    FOR_ALL_EDGES(e_id)
    { ATTR attr = get_eattr(e_id);
      (*calc_edge_forces)(e_id);
      if ( attr & BDRY_ENERGY )
         calc_constr_force_e(e_id);  /* substitute surface energy */
    }
  }
  else /* get here only for SOAPFILM */
  { /* tension forces */
    /* find each triangle's contribution to forces on its vertices */
    if ( threadflag )
      thread_launch(TH_CALC_FACET_FORCES,FACET);
    else
      FOR_ALL_FACETS(f_id)
        (*calc_facet_forces)(f_id);
  }

  if ( ((square_curvature_flag | mean_curv_int_flag) & EVALUATE )
          && !kusner_flag && !conf_edge_curv_flag ) 
     sqcurve_force_end();

  if ( (square_curvature_flag & EVALUATE) && kusner_flag ) 
     kusner_force();
  if ( (square_curvature_flag & EVALUATE) && conf_edge_curv_flag) 
     conf_edge_curv_force();
  if ( sqgauss_flag & EVALUATE )
     sqgauss_force();

  /* Add kludge forces on boundary edges to prevent pulling away */
  if ( (web.modeltype == LINEAR) & web.convex_flag )
  { FOR_ALL_EDGES(e_id)
    { bdry_force(e_id);
      constr_springs(e_id);
    }
  }

grad_quantities_only:
  
  /* general quantity forces */
  calc_quant_grads(Q_ENERGY);

  /* derivatives with respect to optimizing parameters */
  if ( optparamcount > 0 )
  { struct oldcoord csaved;
     csaved.coord = NULL;
     save_coords(&csaved,SAVE_SEPARATE);
     for ( i = 0 ; i < optparamcount ; i++ )
     { REAL dp;
        REAL emid = web.total_energy;
        REAL eleft,eright;

        dp = globals(optparam[i].pnum)->attr.varstuff.delta;

        /* right difference */
        globals(optparam[i].pnum)->value.real += dp;
        project_all(0, TEST_MOVE);
        if ( fixed_constraint_flag || web.pressure_flag || web.pressflag )
          { calc_content(Q_FIXED);
             /*volume_restore();*/
             /*calc_pressure();*/
          }
        calc_energy();  /* energy after motion */
        eright = web.total_energy;
        restore_coords(&csaved,SAVE_SEPARATE);  /* also restores opt params */

        /* left difference */
        globals(optparam[i].pnum)->value.real -= dp;
        project_all(0, TEST_MOVE);
        if ( fixed_constraint_flag || web.pressure_flag || web.pressflag )
          { calc_content(Q_FIXED);
             /*volume_restore(); */
             /*calc_pressure(); */
          }
        calc_energy();  /* energy after motion */
        eleft = web.total_energy;

        restore_coords(&csaved,SAVE_SEPARATE);  /* also restores opt params */
        web.total_energy = emid; /* restore */

        optparam[i].grad = (eright - eleft)/2/dp;
     }
     unsave_coords(&csaved,SAVE_SEPARATE);
  }

} /* end calc_force() */

/*********************************************************************
*  
*  function force_normalization()
*
*  purpose: now have total energy gradient (which is a covector) and
*      need to convert to a velocity vector, which requires
*      multiplication by the inverse of a metric matrix.
*      Default is Euclidean (identity matrix).
*/

void force_normalization()
{
  vertex_id v_id;
  int i,j;
  MAT2D(a,MAXPARAM,MAXCOORD); 

  if ( web.metric_flag && metric_convert_flag )
    FOR_ALL_VERTICES(v_id)
    { if ( !(get_attr(v_id) & FIXED) )
      metric_form_to_vector(get_coord(v_id),get_force(v_id));
    }

  /* do area normalization */
  if ( web.area_norm_flag && !approx_curve_flag) 
  { FOR_ALL_VERTICES(v_id)
    { REAL area = ((web.representation==STRING)?get_vertex_length_star(v_id):
                                 get_vertex_area_star(v_id))/star_fraction;
      REAL *force = get_force(v_id);
      REAL ff;

      if ( get_vattr(v_id) & FIXED ) continue;
      if ( effective_area_flag && (web.representation == STRING) )
      { /* calculate effective area */
        REAL d;
        int valence = get_vertex_valence(v_id);
        ff = SDIM_dot(force,force);
        if ( ff == 0.0 ) continue;
        d = get_edge_density(get_vertex_edge(v_id));
        ff /= d*d;
        if ( valence == 2 )
        { REAL f2;
          f2 = sqrt(ff)/2;
          area *= sqrt(1 - ff/4)*f2/asin(f2);
        }
        else if ( valence == 1 )
        { if ( !(get_vattr(v_id) & (FIXED|CONSTRAINT|BOUNDARY) ) )
          { edge_id e_id = get_vertex_edge(v_id);
            vertex_id other_v = get_edge_headv(e_id);
            eliminate_edge(e_id);
            free_element(e_id);
            add_vertex_valence(other_v,-1);
            continue;
          }
          area *= sqrt(1 - ff/4);
        }
        else if ( valence == 0 )
          area = 1.0;  /* disconnected pt; no force anyway */
        else if ( (valence == 3) && !old_area_flag )
        { /* for density 1 edges only */
          edge_id e_id;
          REAL ss[3],f[MAXCOORD],side[3][MAXCOORD];
          REAL ang12,ang13,det,leg[2][MAXCOORD];
          area = 0.0;
          e_id = get_vertex_edge(v_id);
          for ( i = 0 ; i < 3 ; i++ )
          { get_edge_side(e_id,side[i]);
            ss[i] = sqrt(SDIM_dot(side[i],side[i]));
            e_id = get_next_tail_edge(e_id);
          }
          ang12 = acos(SDIM_dot(side[0],side[1])/ss[0]/ss[1]) - 2*M_PI/3;
          ang13 = acos(SDIM_dot(side[0],side[2])/ss[0]/ss[2]) - 2*M_PI/3;
          for ( j = 0 ; j < SDIM ; j++ )
          { leg[0][j] = side[1][j] - side[0][j];
            leg[1][j] = side[0][j] - side[2][j];
          } 
          det = fabs(leg[0][0]*leg[1][1] - leg[0][1]*leg[1][0]);
          if ( det != 0.0 )
          { f[0] = (leg[1][0]*ang12 - leg[0][0]*ang13)/det;
            f[1] = (leg[1][1]*ang12 - leg[0][1]*ang13)/det;
            area = -0.5*(f[0]*force[0]+f[1]*force[1])/ (f[0]*f[0]+f[1]*f[1]);
          } 
          else area = f[0] = f[1] = 0.0;
          set_vertex_star(v_id,star_fraction*area);
          for ( i = 0 ; i < 2 ; i++ )
            force[i] = -2*f[i];
          continue;
        }
        else /* triple point at least */
        { edge_id e_id,start_e;
          REAL ss,fs,side[MAXCOORD];
          area = 0.0;
          e_id = start_e = get_vertex_edge(v_id);
          do
          { get_edge_side(e_id,side);
            ss = SDIM_dot(side,side);
            fs = SDIM_dot(force,side);
            ff = SDIM_dot(force,force);
            area += 0.5*sqrt(ff*ss - fs*fs);
            e_id = get_next_tail_edge(e_id);
          }
          while ( !equal_id(e_id,start_e) );
        }
        set_vertex_star(v_id,star_fraction*area);
      }
      else if ( effective_area_flag && (web.representation == SOAPFILM) )
      { /* crude correction for triple edges and tetra points */
        if ( get_vattr(v_id) & TRIPLE_PT )
           area /= sqrt(3.);
        else if ( get_vattr(v_id) & TETRA_PT )
           area /= sqrt(6.);
      }
      if ( area == 0.0 )  
      { sprintf(errmsg,"Zero area around vertex %s.\n",ELNAME(v_id));
        kb_error(1453,errmsg,RECOVERABLE);
      }
      for ( i = 0 ; i < SDIM ; i++ )
        force[i] /= area;
    }
  } /* end web.area_norm_flag && !approx_curve_flag */ 

  /* project to parameter space for boundary points */
  FOR_ALL_VERTICES(v_id)
  { int pcount;
    REAL *f = get_force(v_id);
    REAL tmp[MAXCOORD];
    struct boundary *bdry;
    int m;

    if ( get_vattr(v_id) & FIXED ) continue;
    if ( !(get_vattr(v_id) & BOUNDARY) ) continue;
    bdry = get_boundary(v_id);
    pcount = bdry->pcount;
    b_proj(bdry,get_param(v_id),a,PARAMPROJ,v_id);
    matvec_mul(a,f,tmp,pcount,SDIM);
    for ( m = 0 ; m < pcount ; m++ ) f[m] = tmp[m];
    for ( m = pcount ; m < SDIM ; m++ ) f[m] = 0.0;
  }

  return;
} /* end force_normalization */

/*************************************************************
*
*  Function: calc_content()
*
*  Purpose: wrapper for local_calc_content,  and
*       calculates body volumes diffs
*
*  Return value: total difference from fixed value constraints
*/
REAL calc_content ARGS1((mode),
int mode)
{ body_id b_id,bi_id;
  int k;
  REAL diff = 0.0;
  struct gen_quant *gq;
  facet_id f_id;

  /* starting body volumes */
  FOR_ALL_BODIES(b_id)
  { int attr = get_battr(b_id);
    if ( !everything_quantities_flag || ((attr & FIXEDVOL) && (mode & Q_FIXED))
       || ((attr & PRESSURE) && (mode & (Q_ENERGY|Q_INFO)) )
       || (!(attr & (FIXEDVOL|PRESSURE)) && (mode & Q_INFO)) )
     { set_body_volume(b_id,get_body_volconst(b_id),NOSETSTAMP);
       if ( web.representation == STRING )
       { /* kludge for calculating separate facet areas */
         facet_id start_f = get_body_facet(b_id);
         f_id = start_f;
         if ( valid_id(f_id) )
         { do
           { set_facet_area(f_id,0.0);
             f_id = get_next_body_facet(f_id);
           } while ( !equal_id(f_id,start_f));
         } 
       }
     }
  }

  #ifdef MPI_EVOLVER
  if ( this_task == MASTER_TASK )
	  mpi_calc_content(mode);
  else
  #endif
  local_calc_content(mode);

  if ( web.torus_flag )
  { /* do volume adjust to get continuity with old volume, or fixed volume */
     FOR_ALL_BODIES(b_id)
     { REAL vol = get_body_volume(b_id);
       REAL dvol = 0.0;
       REAL new_vc;
       if ( (get_battr(b_id) & FIXEDVOL) && !web.pressure_flag )
       { REAL ovol = get_body_fixvol(b_id);
         dvol = web.torusv*floor((ovol-vol)/web.torusv+0.5);
         add_body_volume_plain(b_id,dvol);   
         save_body_volume(b_id);
       }
       else if (mode & Q_RENORMALIZE) /* set between 0 and 1 */
       { dvol = -web.torusv*floor(vol/web.torusv);
         add_body_volume_plain(b_id,dvol);
         save_body_volume(b_id);  
       }
       else  /* enforce continuity */
       { REAL ovol = get_body_oldvolume(b_id);
         dvol = web.torusv*floor((ovol-vol)/web.torusv+0.5);
         add_body_volume_plain(b_id,dvol); 
       }
       new_vc = get_body_volconst(b_id)+dvol;
       set_body_volconst(b_id,new_vc);
  
       if ( (web.representation == STRING) && (new_vc != 0.0) )
       { /* apply volconst to most worthy facet */
         facet_id f_id = get_body_facet(b_id);
         facet_id start_f = f_id;
         REAL worst_a = new_vc > 0.0 ? 1e30 : -1e30;
         facet_id best_f = NULLID;
         if ( valid_id(f_id) )
         do
         { REAL a = get_facet_area(f_id);
           if ( inverted(f_id) ) 
             a = -a;
           if ( ((new_vc > 0.0) && (a < worst_a))
              || ((new_vc < 0.0) && (a > worst_a)) )
           { best_f = f_id;
             worst_a = a;
           }
           f_id = get_next_body_facet(f_id);
         } while ( !equal_id(f_id,start_f) );
         if ( valid_id(best_f) )
           add_facet_area(best_f,new_vc);
       }
     }
  }

  if ( mode & Q_RENORMALIZE )
  { /* take care of bodies with declared actual volume */
    FOR_ALL_BODIES(b_id)
      if ( get_battr(b_id) & ACTUALVOL )
      { REAL calcvol = get_body_volume(b_id);
        REAL actual  = get_body_actualvolume(b_id);
        set_body_volume(b_id,actual,SETSTAMP);
        set_body_volconst(b_id,get_body_volconst(b_id)+actual-calcvol); 
        save_body_volume(b_id);
      }
  }

  web.vol_flag = 1;


  /* get total deviation from constraints */
  if ( !everything_quantities_flag && !web.pressure_flag ) 
    FOR_ALL_BODIES(bi_id)
    { REAL absvol = get_body_abstotal(b_id);
      REAL target = get_body_fixvol(bi_id);
      REAL actual = get_body_volume(bi_id);

      if ( !(get_battr(bi_id) & FIXEDVOL) ) continue;
      diff += fabs(target - actual)/
                        (web.target_tolerance*(absvol?absvol:1.0));
    }
 
  for ( k = 0 ; k < gen_quant_count ; k++ )
  { gq = GEN_QUANT(k);
    if ( !(gq->flags & Q_FIXED) ) continue;
    if ( gq->tolerance > 0.0 )
      diff += fabs(gq->target - gq->value)/(gq->tolerance*gq->abstotal);
    else
      diff += fabs(gq->target - gq->value)/(web.target_tolerance*gq->abstotal);
  }
  
  if ( mode & Q_FIXED )
    fixed_volume_timestamp = global_timestamp;
  if ( mode & Q_INFO )
    info_volume_timestamp = global_timestamp;
  return diff;

}

/*************************************************************
*
*  Function: local_calc_content()
*
*  Purpose: calculates body volumes  on local piece of surface.
*
*/

void  local_calc_content ARGS1((mode),
int mode) /* Q_FIXED | Q_INFO | Q_RENORMALIZE */
{
  facet_id  f_id;
  facetedge_id fe_id;
  body_id b_id;

  FOR_ALL_BODIES(b_id)
  { /* initialize binary tree add */
    struct body *ptr;
	int k;

	ptr = bptr(b_id);
    for ( k = 0 ; k < MAXADDENDS ; k++ )
       ptr->volume_addends[k] = 0;
  }

  if ( gen_quant_count )
     calc_quants(mode);

  if ( quantities_only_flag ) return;
  if ( web.bodycount == 0 ) return;

  if ( web.representation == SIMPLEX )
  { FOR_ALL_FACETS(f_id)
      calc_simplex_volume(f_id);
  }
  else if ( web.representation == STRING ) 
  { vertex_id v_id;

    FOR_ALL_FACETEDGES(fe_id)
      (*calc_edge_area)(fe_id);

    FOR_ALL_VERTICES(v_id)
    { ATTR attr = get_vattr(v_id);
      if ( !(attr & BDRY_CONTENT) ) continue;
      if ( attr & CONSTRAINT )
         calc_constr_content_v(v_id);
    }
  }

  else if ( web.representation == SOAPFILM ) 
  { if ( web.torus_flag ) torvol();
    else
    { edge_id e_id;

      FOR_ALL_FACETS(f_id)
        (*calc_facet_volume)(f_id);

      FOR_ALL_EDGES(e_id)
      { ATTR attr = get_eattr(e_id);
        if ( !(attr & BDRY_CONTENT) ) continue;
        if ( attr & CONSTRAINT )
          calc_constr_content_e(e_id);
      }
    }
  }

  /* finish binary tree accumulation */
  FOR_ALL_BODIES(b_id)
  { REAL sum = 0.0; 
    struct body *ptr = bptr(b_id);
	int k;
    for ( k = 0 ; k < MAXADDENDS ; k++ )
      sum += ptr->volume_addends[k];
    add_body_volume_plain(b_id,sum);
  }

} /* end local_calc_content() */

/*************************************************************
*
*  Function: calc_pressure()
*
*  Purpose: calculates body volumes and pressures on faces 
*/

void calc_pressure()  
{
  body_id    b_id;

  if ( !web.pressure_flag ) return;
  calc_content(Q_FIXED);

  /* now compute pressure in each body */
  FOR_ALL_BODIES(b_id)
  { 
    REAL p;
  
    if ( equal_id(b_id,web.outside_body) )
      p = web.pressure;
    else if ( get_battr(b_id) & FIXEDVOL )
    { if ( get_body_volume(b_id) > 0.0 )
        p = web.pressure*get_body_fixvol(b_id)/get_body_volume(b_id);
      else 
      { sprintf(errmsg,"Body %s has volume %f\n",ELNAME(b_id),
                               (DOUBLE)get_body_volume(b_id));
        kb_error(1776,errmsg,WARNING);
        p = 0.0;
      }
    }
    else
    { p = 0.0;
    }

    set_body_pressure(b_id,p);
  }

} /* end calc_pressure() */
