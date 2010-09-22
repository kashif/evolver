/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*************************************************************
*
*     file:        iterate.c
*
*     Purpose:    Do one iteration of motion.
*/

#include "include.h"

int volgrads_changed_flag; /* whether volgrads recalced during move */

/*************************************************************
*
*  Function: iterate()
*
*  Purpose:  calculate and make one move of vertices
*/

void iterate()
{
  REAL energy0,energy1,energy2=0.0;
  int seekcount = 0;  /* to prevent infinite loop */
  REAL denom;  /* denominator of interpolation expression */
  REAL scale0,scale1=0.0,scale2=0.0;  /* for interpolation */
  int old_flag = iterate_flag;
  REAL old_energy = web.total_energy; /* for estimate */

#ifndef MPI_EVOLVER
  if ( web.skel[VERTEX].count == 0 )
  { kb_error(1051,"No vertices. Did you forget to load a surface?\n",WARNING);
    return;
  }
#endif

  iterate_flag = 1;  /* for interrupt handler */

  if ( web.diffusion_flag ) 
     diffuse();
  
  energy0 = web.total_energy;
  if ( check_pinning_flag ) check_pinning();
  if ( normal_motion_flag ) begin_normal_motion();
  calc_all_grads(CALC_FORCE|CALC_VOLGRADS);
  volgrads_changed_flag = 0;

  save_coords(&saved,SAVE_IN_ATTR); /* in case of disaster */
  if ( conj_grad_flag )
  { cg_calc_gamma(); /* find adjustment factor  */
    cg_direction();  /* fix up motion direction */
  }

  ENTER_GRAPH_MUTEX;

  if ( !web.motion_flag )  /* want to seek minimum energy along gradient */
  { REAL tempscale;
     
    if ( web.scale > web.maxscale ) web.scale = web.maxscale;
    tempscale = web.scale <= 0.0 ? web.maxscale*1e-6 : web.scale;

    if ( tempscale <= 0.0  ) web.scale = web.maxscale*1e-6;
    else web.scale = tempscale;
    if ( itdebug ) printf("First move, scale %g\n",(double)web.scale);
    move_vertices(TEST_MOVE,web.scale);  /* moving by current scale */
    energy1 = web.total_energy;
    if ( !is_finite(energy1) ) energy1 = MAXDOUBLE;
    scale1 = web.scale;
    if ( itdebug )
    { printf("first move:\nscale1 %g energy1 %20.15g \n",
          (DOUBLE)scale1,(DOUBLE)energy1);
      if ( web.skel[BODY].count < 10  ) show_volumes();
      printf("\n");
    }

    restore_coords(&saved,SAVE_IN_ATTR);
    if ( volgrads_changed_flag )
    {  calc_all_grads(CALC_VOLGRADS);
       calc_leftside();
       volgrads_changed_flag = 0;
    }
    web.scale = 0.0; /* to do restoring force */
    move_vertices(TEST_MOVE,web.scale);  /* moving by current scale */
    energy0 = web.total_energy;
    if ( itdebug )
    { printf("0th move:\nscale1 %g energy1 %20.15g \n",0.0,(DOUBLE)energy0);
      if ( web.skel[BODY].count < 10 )
        show_volumes();
      printf("\n");
    }

    if ( !is_finite(energy0) )
      { kb_error(1863,"Infinite energy. Restoring coordinates.\n",WARNING);
        goto iterate_error_exit;
      }
    scale0 = 0.0;
    restore_coords(&saved,SAVE_IN_ATTR); 
    if ( volgrads_changed_flag )
    {  calc_all_grads(CALC_VOLGRADS);
       calc_leftside();
       volgrads_changed_flag = 0;
    }

    web.scale = tempscale;
 
    if ( energy1 < energy0 )
    { do
      {
        web.scale *= 2;
        if ( itdebug ) printf("Doubling scale, scale %g\n",(double)web.scale);
        move_vertices(TEST_MOVE,web.scale);
        energy2 = web.total_energy;
        scale2 = web.scale;
        if ( itdebug )
        { printf("scale2 %g energy2 %20.15g \n",(DOUBLE)scale2,(DOUBLE)energy2);
          if ( web.skel[BODY].count < 10 ) show_volumes();
          printf("\n");
        }
        restore_coords(&saved,SAVE_IN_ATTR);
        if ( volgrads_changed_flag )
        {  calc_all_grads(CALC_VOLGRADS);
           calc_leftside();
           volgrads_changed_flag = 0;
        }
        if ( !is_finite(energy2) ) 
           { web.scale /= 2; goto have_scale;} /* use finite motion */ 
        if ( energy2 > energy1 ) 
           { web.scale = web.scale/2; break; }
        energy1 = energy2;  scale1 = scale2;
      }
      while ( (web.scale < web.maxscale)  /* || conj_grad_flag */ );
    }
    else /* have to come down in scale */
    {
      do
      { 
         seekcount++;
         if ( seekcount > 20 ) /* looks like energy won't decrease */
            { web.scale = 0.0; break; }
         energy2 = energy1; scale2 = scale1;
         web.scale /= 2;
         if ( itdebug ) printf("Halving scale, scale %g\n",(double)web.scale);
         if  ( web.scale < 1e-12*web.maxscale ) { web.scale = 0.0; break; }
         move_vertices(TEST_MOVE,web.scale);
         energy1 = web.total_energy;
         if ( !is_finite(energy1) ) energy1 = MAXDOUBLE;
         scale1 = web.scale;
         if ( itdebug ) 
         { printf("scale1 %g energy1 %20.15g \n",(DOUBLE)scale1,(DOUBLE)energy1);
           if ( web.skel[BODY].count < 10 ) show_volumes();
           printf("\n");
         }
         restore_coords(&saved,SAVE_IN_ATTR);
         if ( volgrads_changed_flag )
         { calc_all_grads(CALC_VOLGRADS);
           calc_leftside();
           volgrads_changed_flag = 0;
         }
      }
      while ( energy1 > energy0 );
      web.scale *= 2;
    }

    if ( web.scale > web.maxscale ) 
      web.scale = web.maxscale;
    else if ( web.scale > 0.0 )
    {
      /* now quadratic interpolation for minimum energy */
      denom = energy0*(scale1-scale2)+energy1*(scale2-scale0)
                     + energy2*(scale0 - scale1);
      if ( denom == 0.0 ) web.scale = 0.0;
      else
      { web.scale = ((energy0-energy2)*scale1*scale1
                +(energy1-energy0)*scale2*scale2
                +(energy2-energy1)*scale0*scale0)/2/denom;
      }
      /* else leave scale as is */
    }
  }
  else if ( runge_kutta_flag )
  { /* only for fixed scale */
    runge_kutta(); 
  }

have_scale:
  if ( !web.motion_flag )
    if ( web.scale > web.maxscale ) 
      web.scale = web.maxscale; /* max on movement */

  if ( itdebug ) printf("Final scale: %g\n",(double)web.scale);
  move_vertices(ACTUAL_MOVE,web.scale_scale*web.scale);


  if ( web.jiggle_flag )  jiggle(); 
  if( autopop_flag || autochop_flag ) 
  { 
    autopop_detect(web.scale);
    if ( autopop_count || autochop_count )
    { autopop_pop();
      autochop_chop();
    }
    autopop_cleanup();   
  }

  if ( fixed_constraint_flag ) calc_content(Q_FIXED);

  if ( !is_finite(web.total_energy ))
  { kb_error(1864,
     "Motion would cause infinite energy. Restoring coordinates.\n",WARNING);
        goto iterate_error_exit;
     }     
  if ( check_increase_flag && (web.total_energy > energy0) )
  { kb_error(1865,
     "Motion would have increased energy. Restoring coordinates.\n",WARNING);
        goto iterate_error_exit;
  }
  if ( !web.motion_flag && (web.total_energy > energy0) )
  { /* go back and use scale1 */
    restore_coords(&saved,SAVE_IN_ATTR);
    web.scale = scale1; /* known to decrease energy */
    move_vertices(ACTUAL_MOVE,web.scale);
    if ( cg_hvector ) /* restart conjugate gradient */
       cg_restart();
  }
  total_time += web.scale;
  if ( web.area_norm_flag && web.norm_check_flag &&
               (web.representation==SOAPFILM) )
  { REAL delta = normal_change_check();
    if ( delta  > web.norm_check_max )
    { sprintf(msg,"Max normal change: %f.  Restoring coordinates.\n",
                            (DOUBLE) delta);
      kb_error(1866,msg,WARNING);
      goto iterate_error_exit;
    }
  }
     
/* if chop here, then instabilities have a chance to get out of control */
/*  if ( autopop_flag ) autopop_pop();
  if ( autochop_flag ) autochop_chop();
*/
/* following good for debugging to see if energy gradients really give
    change in energy.  Estimated decrease should be approx exact for
    scale much less than optimum; at optimum scale, estimate is twice
    actual (if energy shape true parabola )
    But note estimate does not work if form-to-vector has messed things up.
*/

  if ( estimate_flag )
     {
#ifdef LONGDOUBLE
        sprintf(msg,"Estimated energy change: %#*.*Lg\n",DWIDTH,DPREC,
            estimate_decrease());
        outstring(msg);
        sprintf(msg,"Actual energy change   : %#*.*Lg\n",DWIDTH,DPREC,
            web.total_energy-old_energy);
        outstring(msg);
#else
        sprintf(msg,"Estimated energy change: %#20.15g\n",estimate_decrease());
        outstring(msg);
        sprintf(msg,"Actual energy change   : %#20.15g\n",
            web.total_energy-old_energy);
        outstring(msg);
#endif
     }

#ifdef LONGDOUBLE
  sprintf(msg,"%3d. energy: %#*.*Lg  scale: %#Lg\n",gocount,DWIDTH,DPREC,
                web.total_energy,web.scale);
#else
  sprintf(msg,"%3d. %s: %#17.15g energy: %#17.15g  scale: %#g\n",gocount,
                areaname,web.total_area,web.total_energy,web.scale);
#endif
  if ( !quiet_go_flag ) outstring(msg);

  vgrad_end();
  unsave_coords(&saved,SAVE_IN_ATTR);
  update_display();
  iterate_flag = old_flag; /* for interrupt handler */
  goto iterate_exit;

iterate_error_exit:
  /* in case optimizing scale didn't work */

  vgrad_end();

  restore_coords(&saved,SAVE_IN_ATTR);
  if ( count_fixed_vol() || web.pressure_flag ) calc_content(Q_FIXED);
  calc_pressure();
  calc_energy();  /* energy after motion */
  unsave_coords(&saved,SAVE_IN_ATTR);
  iterate_flag = old_flag; /* for interrupt handler */
  update_display();
  breakflag = BREAKREPEAT; /* break repeat loop */

iterate_exit:
  LEAVE_GRAPH_MUTEX;
}  /* end iterate() */

/**************************************************************************
*
* function: calc_all_grads()
*
* purpose: recalculate forces and/or constraint gradients.
*
*/

void calc_all_grads(mode)
int mode; /* bits for CALC_FORCE and CALC_VOLGRADS */
{ find_fixed();
 

  if ( mode & CALC_VOLGRADS ) 
    calc_volgrads(DO_OPTS);
  if ( mode & CALC_FORCE )
  { calc_force();
    pressure_forces();
  }

  partner_shift_grads(mode);  /* in case doing partners */

  /* Now have energy and constraint gradients as forms. 
      Next convert to vectors using current metric. */
/* lagrange_recalc: */
  convert_forms_to_vectors(mode);  /* using current metric */
  /*one_sided_adjust(mode);*/ /* modify forces and grads on one-sided constraints */

  if ( mode & CALC_FORCE )
  { calc_lagrange();     /* calculate Lagrange multipliers */
    lagrange_adjust();   /* adjust forces using Lagrange multipliers */
    pressure_set_flag = 1;

  }
} /* end calc_all_grads() */
 
/******************************************************************
*
*  function: burchard()
*
*  purpose: implement Paul Burchard's scheme for accelerating
*  motion by ramping scale factor so instabilities are damped.
*
*  Starts with current scale factor and ramps up, so use 'm' 
*  command to set small starting scale.  Will reset scale at
*  end to original small value.
*/

void burchard(maxsteps)
int maxsteps;
{
  REAL old_scale = web.scale;
  int old_motion_flag = web.motion_flag;
  int i;

  web.motion_flag = 1;
  for ( i = 0 ; i < maxsteps ; i++ )
    { web.scale =old_scale /(1 - (i*i)/(REAL)(maxsteps*maxsteps));
      iterate();
    }
  web.scale = old_scale;
  web.motion_flag = old_motion_flag;
}

/***************************************************************
*
*    Function: fix_vertices()
*
*    Purpose:  Fixes vertices by zeroing out force.  Also projects
*                 force to be tangent to boundary and constraints.
*
*   only current use is in square_grad()
*/

void fix_vertices()
{
  vertex_id v_id;
  REAL *force;
  int i,j;

  if ( itdebug )
    outstring("fix_vertices(): project forces to level-set constraints\n");

  if ( check_pinning_flag )
  { edge_id e_id;
    /* check for vertices that can't move because adjacent
       vertices are not on same constraint when they could be */
    FOR_ALL_VERTICES(v_id) /* clear pinning flag */
      vptr(v_id)->attr &= ~PINNED_V;
    FOR_ALL_EDGES(e_id)
    { 
      vertex_id headv = get_edge_headv(e_id);
      vertex_id tailv = get_edge_tailv(e_id);
      conmap_t * hstat = get_v_constraint_map(headv);
      conmap_t * tstat = get_v_constraint_map(tailv);
      for ( i=1, j=0 ;  i <= (int)hstat[0] ; i++ ) 
         if (hstat[i] & CON_HIT_BIT) j++;
      if ( j == 1 )
             set_attr(tailv,PINNED_V);
      for ( i=1, j=0 ;  i <= (int)tstat[0] ; i++ ) 
         if (tstat[i] & CON_HIT_BIT) j++;
      if ( j == 1 )
         set_attr(headv,PINNED_V);
    }
    FOR_ALL_VERTICES(v_id) /* see if 2 con vertex has 2 con nbrs */
    { conmap_t * hit = get_v_constraint_map(v_id);
      for ( i = 1, j = 0 ; i <= (int)hit[0] ; i++ ) 
        if ( hit[i] & CON_HIT_BIT ) j++;
      if ( j != 2 )
         set_attr(v_id,PINNED_V);
    }
  }


  FOR_ALL_VERTICES(v_id)
  {
    ATTR attr = get_vattr(v_id);

    force = get_force(v_id);
    if ( attr & FIXED )
    { memset((char*)force,0,SDIM*sizeof(REAL));
      continue;
    }
        
    if ( mobility_flag )
    { if ( mobility_tensor_flag )
      { REAL new_force[MAXCOORD];
        REAL *x = get_coord(v_id);
        for ( i = 0 ; i < SDIM ; i++ )
          for ( j = 0, new_force[i] = 0.0 ; j < SDIM ; j++ ) 
            new_force[i] += force[j]*eval(&mobility_tensor[i][j],x,v_id,NULL);
        for ( j = 0 ; j < SDIM ; j++ )
          force[j] = new_force[j];
      }
      else
      { REAL mobility = eval(&mobility_formula,get_coord(v_id),v_id,NULL);
        for ( j = 0 ; j < SDIM ; j++ )
        force[j] *= mobility;
      }
    }

    if ( normal_motion_flag )
    { /* project to normal */
      REAL d;
      int vnum = loc_ordinal(v_id);
      REAL *normal = vertex_normals[vnum];

      d = SDIM_dot(force,normal);
      for ( j = 0 ; j < SDIM ; j++ )
         force[j] = d*normal[j];
    }

    if ( (attr & CONSTRAINT) && (!check_pinning_flag || (attr & PINNED_V)) )
    {
      conmap_t * conmap = get_v_constraint_map(v_id);
      int oncount = 0;
      struct constraint *con[MAXCONPER];
      int conlist[MAXCONPER];
      REAL perp[MAXCOORD];

      for ( j = 1 ; j <= (int)conmap[0] ; j++ )
      { 
        if ( conmap[j] & CON_HIT_BIT )
        { conlist[oncount] = conmap[j] & CONMASK;
          con[oncount] = get_constraint(conmap[j]);
/*          if ( !(con[oncount]->attr & (NONNEGATIVE|NONPOSITIVE) ) ) */
            oncount++;  
        }
      }

      if ( oncount > SDIM ) 
      { sprintf(errmsg,
        "Vertex %s is on more constraints than the dimension of space.\n",
           ELNAME(v_id));
        kb_error(2084,errmsg,WARNING);
        oncount = SDIM;
      }
      if ( oncount )
      { constr_proj(TANGPROJ,oncount,con,get_coord(v_id),
                               force,perp,conlist,DETECT,v_id);
        for ( j = 0 ; j < SDIM ; j++ )
          force[j] -= perp[j];
        if ( web.area_norm_flag && (web.representation==STRING) )
        { /* correction factor for moving vertex along constraint
             is to divide projected force by sin^2 of contact angle,
             here crudely estimated. */
          REAL sinsq = SDIM_dot(perp,perp);
          if ( (sinsq != 0.0) && (sinsq < 1.0) )
            for ( j = 0 ; j < SDIM ; j++ )
              force[j] /= sinsq;
        }
      }
    }
  }
  if ( web.h_inverse_metric_flag ) apply_h_inverse_metric();
} /* end fix_vertices() */

/***************************************************************
*
*    Function: move_vertices()
*
*    Purpose: handle possible MPI invocation of local_move_vertices
*/
void move_vertices(mode,scale)
int mode; /* TEST_MOVE or ACTUAL_MOVE */
REAL scale;
{
  if ( itdebug )
    outstring("move_vertices(): by scale factor times velocity\n");

  #ifdef MPI_EVOLVER
  if ( this_task != 0 ) return;
  mpi_move_vertices(mode,scale);
  #else
  local_move_vertices(mode,scale);
  #endif
  
  if ( web.homothety )
     homothety();

  global_timestamp++;

  partner_move(); /* in case doing partners */

  project_all(1, mode);
 

  if ( fixed_constraint_flag || web.pressure_flag || web.pressflag ) 
     calc_pressure();

  calc_energy();  /* energy after motion */

}
/***************************************************************
*
*    Function: local_move_vertices()
*
*    Purpose:  moves all unfixed vertices by current scale
*                 factor and force.
*
*    Input:     uses global variable for scale
*/

REAL thread_scale; /* for passing scale argument to threads */

void local_move_vertices(mode,scale)
int mode; /* TEST_MOVE or ACTUAL_MOVE */
REAL scale;
{
  REAL *velocity;
  REAL *x;
  int i;
  vertex_id v_id;
  int dim = ackerman_flag ? 2*SDIM : SDIM;

  if ( optparamcount > 0 )
  { for ( i = 0 ; i < optparamcount ; i++ )
      globals(optparam[i].pnum)->value.real -= scale*optparam[i].velocity;
    project_all(0,TEST_MOVE);  /* force vertices to constraints */
  }

  /* move by multiple of velocity */
  if ( threadflag )
  { thread_scale = scale;
    thread_launch(TH_MOVE_VERTICES,VERTEX);
  }
  else
  FOR_ALL_VERTICES(v_id)
  {
    if ( get_vattr(v_id) & FIXED ) continue ;
    velocity = get_velocity(v_id);
    x = get_coord(v_id);
    if ( get_vattr(v_id) & BOUNDARY )
    {
      struct boundary *boundary = get_boundary(v_id);
      int pcount = boundary->pcount;
      REAL *param = get_param(v_id);

      for ( i = 0 ; i < pcount ; i++ )
        param[i] += scale*velocity[i];
    }
    else
    {
      for ( i = 0 ; i < dim ; i++ )
        x[i] += scale*velocity[i];
    }
  }
  
  return;
} /* end local_move_vertices() */

#ifdef THREADS
/*****************************************************************
*
* function: thread_move_vertices()
*
* purpose: one thread's part of move_vertices()
*/

void thread_move_vertices()
{ int i; 
  int dim = ackerman_flag ? 2*SDIM : SDIM;
  
  THREAD_FOR_ALL_NEW(VERTEX,  /* following block is macro argument */
  { vertex_id v_id = *idptr;
    REAL *velocity;
    REAL *x;

    if ( get_vattr(v_id) & FIXED ) continue ;
    velocity = get_velocity(v_id);
    x = get_coord(v_id);
    if ( get_vattr(v_id) & BOUNDARY )
    {
      struct boundary *boundary = get_boundary(v_id);
      int pcount = boundary->pcount;
      REAL *param = get_param(v_id);

      for ( i = 0 ; i < pcount ; i++ )
        param[i] += thread_scale*velocity[i];
    }
    else
    {
      for ( i = 0 ; i < dim ; i++ )
        x[i] += thread_scale*velocity[i];
    }
  }
 ) /* end of macro argument */
}  
#endif
/* THREADS */

/***************************************************************************
*
* function: project_all()
*
* purpose: projection to all constraints and boundaries after moving.
*/

void project_all(mode,mode2)
int mode;  /* 1 for extensive constraints */
int mode2; /* TEST_MOVE or ACTUAL_MOVE */
{ vertex_id v_id;
  int one_sided_mode = (mode2==TEST_MOVE) ? KEEP_ONESIDEDNESS : RESET_ONESIDEDNESS;
  /* project to constraints and boundaries */

  if ( itdebug )
    outstring("project_all(): to level set and extensive constraints\n");

  if ( threadflag )
  { int task = mode2==TEST_MOVE ? TH_PROJECT_ALL_TEST : TH_PROJECT_ALL_ACTUAL;
    thread_launch(task,VERTEX);
  }
  else
  FOR_ALL_VERTICES(v_id)
  { int attr = get_vattr(v_id);
    if ( attr & CONSTRAINT )
       project_v_constr(v_id,mode2,one_sided_mode);
    else if ( attr & BOUNDARY )
    { int i;
      struct boundary *boundary = get_boundary(v_id);
      REAL *param = get_param(v_id);
      REAL *x = get_coord(v_id);

      for ( i = 0 ; i < SDIM ; i++ )
         x[i] = eval(boundary->coordf[i],param,v_id,NULL);
    }
  }

  /* TESTING new feature */
  detect_bdry_hits();
 
  /* enforce volume constraints also; keep conjugate gradient under control */
  if ( mode && fixed_constraint_flag ) 
  { int calc_count;
    REAL orig_diff = calc_content(Q_FIXED);
    REAL old_diff = orig_diff;
    REAL diff=old_diff;  /* total relative constraint diff */
    REAL stepsize = 1.0;
    struct oldcoord psaved;
    calc_count = 1;
    psaved.coord = NULL;
    save_coords(&psaved,SAVE_SEPARATE);
    do 
    { if ( diff == 0.0 ) break; 
      if ( itdebug ) printf("Diff: %g    Old_diff: %g\n",
           (double)diff,(double)old_diff);
      if ( diff > 1.01*old_diff )
      { 
        if ( itdebug ) printf("Diff increased. Restoring coords.\n");
        restore_coords(&psaved,SAVE_SEPARATE); 
        if ( diff > 100*old_diff )
        { sprintf(msg,"Total constraint difference would increase by factor of %f.\nAborting constraint adjustment.\n",fabs(diff/old_diff) );
          outstring(msg);
          break;
        }
       /*   calc_content(Q_FIXED); taken care of by restore_coords */
          stepsize *= 0.5;
      }
      else stepsize = 1.0;
      if ((volgrads_every_flag || (diff > 0.5*old_diff)) && (calc_count > 1))
      { /* not good improvement, so recalculate */
        if ( itdebug ) printf("Recalculating volgrads.\n");
        calc_all_grads(CALC_VOLGRADS);
        calc_leftside();
        volgrads_changed_flag = 1;
      }
      volume_restore(stepsize,mode2);
      global_timestamp++;
      if ( calc_count++ > 10 ) 
      { 
        sprintf(errmsg,"Volume or quantity constraints don't converge in 10 projections.\nTotal difference %g times tolerance\n",(DOUBLE)diff);
        if ( mode2 == ACTUAL_MOVE )
           kb_error(1055,errmsg,WARNING);
        reset_conj_grad();
        break;
      }
      old_diff = diff;
      diff = calc_content(Q_FIXED);
      if ( itdebug ) printf("Next diff: %g\n",(double)diff);
    } while ( mode && (diff > 1) ); 

    unsave_coords(&psaved,SAVE_SEPARATE);
  } /* end enforcing volume constraints */

  global_timestamp++;

  return;
} /* end project_all() */

#ifdef THREADS
/***************************************************************************
*
* function thread_project_all()
*
* purpose: thread-friendly first part of project_all
*
*/

void thread_project_all(int mode2)
{ int one_sided_mode = (mode2==TEST_MOVE) ? KEEP_ONESIDEDNESS : RESET_ONESIDEDNESS;
  /* project to constraints and boundaries */ 
  THREAD_FOR_ALL_NEW(VERTEX,
    { int attr = get_vattr(*idptr);
      if ( attr & CONSTRAINT )
         project_v_constr(*idptr,mode2,one_sided_mode);
      else if ( attr & BOUNDARY )
      { int i;
        struct boundary *boundary = get_boundary(*idptr);
        REAL *param = get_param(*idptr);
        REAL *x = get_coord(*idptr);

        for ( i = 0 ; i < SDIM ; i++ )
           x[i] = eval(boundary->coordf[i],param,*idptr,NULL);
      }
    }
  ) /* end of THREAD_FOR_ALL macro  */
}
#endif
/* THREADS */

/*************************************************************************
*
* Function: save_coords()
*
* Purpose: wrapper for local_save_coords()
*   NOTE: relies on mode to determine where to save, rather than saver
*/
void save_coords(saver,mode)
struct oldcoord *saver;  
int mode; /* SAVE_IN_ATTR if use vertex attribute __oldx  */
          /* SAVE_SEPARATE for separate memory allocation */
{
  #ifdef MPI_EVOLVER
  if ( this_task != 0 ) return;
  mpi_save_coords(mode);
  #else
  local_save_coords(saver,mode);
  #endif
  
}


/****************************************************************
*
*  Function: local_save_coords()
*
*  Purpose: Save current coordinates so they can be restored
*              after a trial motion.
*
*/

void local_save_coords(saver,mode)
struct oldcoord *saver;  
int mode; /* SAVE_IN_ATTR if use vertex attribute __oldx  */
          /* SAVE_SEPARATE for separate memory allocation */
{
  vertex_id v_id;
  body_id b_id;
  int n;

  if ( mode == SAVE_SEPARATE ) 
  {
    if ( saver->coord )
      temp_free((char *)saver->coord);    /* in case somebody forgot */
    saver->coord = (REAL (*)[MAXCOORD])temp_calloc(web.skel[VERTEX].max_ord+1,
                                                  sizeof(REAL)*MAXCOORD);

    FOR_ALL_VERTICES(v_id)
    { if ( get_vattr(v_id) & BOUNDARY )
        memcpy((char *)(saver->coord+loc_ordinal(v_id)),(char *)get_param(v_id),
                                           sizeof(REAL)*web.maxparam);
      else
        memcpy((char *)(saver->coord+loc_ordinal(v_id)),(char *)get_coord(v_id),
                                           sizeof(REAL)*SDIM);
    }

  }
  else
  {
    FOR_ALL_VERTICES(v_id)
    { if ( get_vattr(v_id) & BOUNDARY )
        memcpy((char *)(get_oldcoord(v_id)),(char *)get_param(v_id),
                                           sizeof(REAL)*web.maxparam);
      else
        memcpy((char *)(get_oldcoord(v_id)),(char *)get_coord(v_id),
                                           sizeof(REAL)*SDIM);
    }

  }

  /* also save old energy and quantities */
  /* always saving separately since in-structure save being used elsewhere */
  saver->energy = web.total_energy;
  saver->bod = (REAL*)temp_calloc(web.skel[BODY].max_ord+1,3*sizeof(REAL));
  FOR_ALL_BODIES(b_id) 
  { int spot = 3*loc_ordinal(b_id);
    saver->bod[spot] = get_body_volume(b_id);
    saver->bod[spot+1] = get_body_pressure(b_id);
    saver->bod[spot+2] = get_body_volconst(b_id);
  }
  saver->meth = (REAL*)temp_calloc(meth_inst_count,sizeof(REAL));
  for ( n = LOW_INST ; n < meth_inst_count ; n++ )
     saver->meth[n] = METH_INSTANCE(n)->value;
  saver->quant = (REAL*)temp_calloc(gen_quant_count,2*sizeof(REAL));
  for ( n = 0 ; n < gen_quant_count ; n++ )
  { saver->quant[2*n] = GEN_QUANT(n)->value;
    saver->quant[2*n+1] = GEN_QUANT(n)->pressure;
  }

  /* save optimizing parameters */
  for ( n = 0 ; n < optparamcount ; n++ )
     saver->optparam_values[n] = globals(optparam[n].pnum)->value.real;

} /* end save_coords() */

/****************************************************************
*
*  Function: restore_coords()
*
*  Purpose: wrapper for local_restore_coords()
*
*/
void restore_coords(saver,mode)
struct oldcoord *saver;
int mode;
{
  #ifdef MPI_EVOLVER
  if ( this_task != 0 ) return;
  mpi_restore_coords(mode);
  #else
  local_restore_coords(saver,mode);
  #endif
}

/****************************************************************
*
*  Function: local_restore_coords()
*
*  Purpose: Restore current coordinates after a trial motion.
*
*/

void local_restore_coords(saver,mode)
struct oldcoord *saver;
int mode;
{
  vertex_id v_id;
  body_id b_id;
  int n;

  if ( mode == SAVE_SEPARATE )
  { if ( saver->coord == NULL )
     kb_error(1056,
    "Internal error: Cannot restore old coordinates since there aren't any!\n",
                        RECOVERABLE);
  }

  /* restore optimizing parameters */
  for ( n = 0 ; n < optparamcount ; n++ )
     globals(optparam[n].pnum)->value.real = saver->optparam_values[n];

  FOR_ALL_VERTICES(v_id)
     restore_vertex(v_id,saver,mode);

  web.total_energy = saver->energy;

    FOR_ALL_BODIES(b_id) 
    { int spot = 3*loc_ordinal(b_id);
      set_body_volume(b_id,saver->bod[spot],NOSETSTAMP);
      set_body_pressure(b_id,saver->bod[spot+1]);
      set_body_volconst(b_id,saver->bod[spot+2]);
    }
    for ( n = LOW_INST ; n < meth_inst_count ; n++ )
       METH_INSTANCE(n)->value = saver->meth[n];
    for ( n = 0 ; n < gen_quant_count ; n++ )
    { GEN_QUANT(n)->value = saver->quant[2*n];
      GEN_QUANT(n)->pressure = saver->quant[2*n+1];
    }

  global_timestamp++;
} /* end restore_coords() */

/******************************************************************
*
*  Function: restore_vertex()
*
*  Purpose:  Put a vertex back where it was.
*/

void restore_vertex(v_id,saver,mode)
vertex_id v_id;
struct oldcoord *saver;
int mode; /* whether to use _oldx */
{
  int i;
  REAL *p,*x;
  struct boundary *bdry;

  if ( get_vattr(v_id) & BOUNDARY )
  { p = get_param(v_id);
    x = get_coord(v_id);
    bdry = get_boundary(v_id);
    if ( mode == SAVE_SEPARATE )
      memcpy((char *)p,(char *)(saver->coord+loc_ordinal(v_id)),
                                  sizeof(REAL)*web.maxparam);
    else
      memcpy((char *)p,(char *)(get_oldcoord(v_id)),
                                  sizeof(REAL)*web.maxparam);
    for ( i = 0 ; i < SDIM ; i++ )
       x[i] = eval(bdry->coordf[i],p,v_id,NULL);
  }
  else
  { if ( mode == SAVE_SEPARATE )
      memcpy((char *)get_coord(v_id),(char *)(saver->coord+loc_ordinal(v_id)),
                                                         sizeof(REAL)*SDIM);
    else
      memcpy((char *)get_coord(v_id),(char *)(get_oldcoord(v_id)),
                                                         sizeof(REAL)*SDIM);
  }
} /* end restore_vertex() */

/****************************************************************
*
*  Function: unsave_coords()
*
*  Purpose: wrapper for local_unsave_coords()
*
*/
void unsave_coords(saver,mode)
struct oldcoord *saver;
int mode;
{
  #ifdef MPI_EVOLVER
  if ( this_task != 0 ) return;
  mpi_unsave_coords(mode);
  #else
  local_unsave_coords(saver,mode);
  #endif
}
/********************************************************************
*
*  Function: local_unsave_coords()
*
*  Purpose: Clean up after all trial motions done.
*/

void local_unsave_coords(saver,mode)
struct oldcoord *saver;
int mode;
{
    if ( saver->coord )
      temp_free( (char *)saver->coord );
    saver->coord = NULL;
    if ( saver->bod )
      temp_free( (char *)saver->bod );
    saver->bod = NULL;
    if ( saver->quant )
      temp_free( (char *)saver->quant );
    saver->quant = NULL;
    if ( saver->meth )
      temp_free( (char *)saver->meth );
    saver->meth = NULL;
  
}


/****************************************************************
*
*  Function:  jiggle()
*
*  Purpose:    Move each vertex a little bit at random to get
*                 away from metastable equilibria and crystalline
*                 integrand hangups.
*
*  Input:      The global variable temperature scales the size
*                 of the jiggles.  They are taken to be spherically
*                 symmetric Gaussian distribution with mean size
*                 temperature*max_length.
*/

void jiggle()
{
  vertex_id v_id;
  REAL *x;
  int j;

  if ( web.max_len == 0.0 ) web.max_len = .1;
  if ( overall_size == 0.0 ) overall_size = 1.0;
  FOR_ALL_VERTICES(v_id)
     { if ( get_vattr(v_id) & FIXED ) continue;
        x = get_coord(v_id);
        for ( j = 0 ; j < SDIM ; j++ )
          x[j] += gaussian()*web.temperature*web.max_len*overall_size;
     }
  outstring("One jiggle done.\n");
}


/****************************************************************
*
*  Function:  long_jiggle()
*
*  Purpose:    Move each vertex with a long wavelenth perturbation
*                 away from metastable equilibria and crystalline
*                 integrand hangups.
*
*  Input:      The global variable temperature scales the size
*                 of the jiggles.  The perturbation has a random
*                 amplitude vector chosen from sphere and random
*                 wavevector likewise chosen at random.  Amplitude
*                 is multiplied by current temperature.
*/

/* remembered parameters */
static REAL wavev[MAXCOORD];
static REAL amp[MAXCOORD];
static REAL phase;

void long_jiggle()
{ REAL mag;
  REAL ww; 
  int j;
  vertex_id v_id;
  REAL *x;
  char response[100];

  /* get wave vector */
get_wv:
#ifdef LONGDOUBLE
  sprintf(msg,"Enter wave vector (%Lf,%Lf,%Lf;r): ",wavev[0],wavev[1],wavev[2]);
#else
  sprintf(msg,"Enter wave vector (%f,%f,%f;r): ",wavev[0],wavev[1],wavev[2]);
#endif
  prompt(msg,response,sizeof(response));
  if ( logfd ) fprintf(logfd,"%s\n",response);
  if ( response[0] == 'r' )
    { /* random */
      prompt(msg,response,sizeof(response));
      if ( logfd ) fprintf(logfd,"%s\n",response);
      if ( atoi(response) != 0 ) srand(atoi(response));

      /* pick random wavelength in unit sphere */
      do 
         { for ( j = 0 ; j < SDIM ; j++ )
              wavev[j] = 1 - 2*(REAL)(rand()&0x7FFF)/0x7FFFL;
              ww = SDIM_dot(wavev,wavev);
         }
      while ( ww > 1.0 );
      /* invert to wavevector and scale to surface size */
      for ( j = 0 ; j < SDIM ; j++ )
         wavev[j] /= ww*overall_size;
    }
  else if ( isalpha(response[0]) ) return; /* escape without jiggle */
  else if ( response[0] )
    { REAL val[MAXCOORD];
      cmdptr = response;
      for ( j = 0 ; j < SDIM ; j++ )
         if ( read_const(val+j) <= 0 )
         { outstring("Wrong number of components.\n");
            goto get_wv;
         }
      for ( j = 0 ; j < SDIM ; j++ ) wavev[j] = val[j];
    }

  /* pick random phase */
get_phase:
  sprintf(msg,"Enter phase (%f;r): ",(DOUBLE)phase);
  prompt(msg,response,sizeof(response));
  if ( logfd ) fprintf(logfd,"%s\n",response);
  if ( response[0] == 'r' )
     phase = 2*M_PI*(REAL)(rand()&0x7FFF)/0x7FFFL;
  else if ( isalpha(response[0]) ) return; /* escape without jiggle */
  else if ( response[0] )
     { REAL val;
        if ( const_expr(response,&val) <= 0 )
          goto get_phase;
        phase = val;
     }

  /* amplitude */  
get_amp:
  sprintf(msg,"Enter amplitude (%f,%f,%f;r): ",(DOUBLE)amp[0],(DOUBLE)amp[1],(DOUBLE)amp[2]);
  prompt(msg,response,sizeof(response));
  if ( logfd ) fprintf(logfd,"%s\n",response);
  if ( response[0] == 'r' )
    { /* random */
      /* pick random amplitude */
      do 
         for ( j = 0 ; j < SDIM ; j++ )
            amp[j] = 1 - 2*(REAL)(rand()&0x7FFF)/0x7FFFL;
      while ( SDIM_dot(amp,amp) > 1.0 );
      for ( j = 0 ; j < SDIM ; j++ )
         amp[j] *= web.temperature*overall_size;
    }  
  else if ( isalpha(response[0]) ) return; /* escape without jiggle */
  else if ( response[0] )
    { REAL val[MAXCOORD];
      cmdptr = response;
      for ( j = 0 ; j < SDIM ; j++ )
         if ( read_const(val+j) <= 0 )
         { outstring("Wrong number of components.\n");
            goto get_amp;
         }
      for ( j = 0 ; j < SDIM ; j++ ) amp[j] = val[j];
    }

  /* move vertices */
  FOR_ALL_VERTICES(v_id)
     { if ( get_vattr(v_id) & FIXED ) continue;
        x = get_coord(v_id);
        mag = sin(SDIM_dot(wavev,x) + phase);
        for ( j = 0 ; j < SDIM ; j++ )
          x[j] += amp[j]*mag;
     }
  outstring("One long jiggle done.\n");
} /* end long_jiggle() */

/****************************************************************
*
*  Function: gaussian()
*
*  Purpose:  generate gaussian random variables with variance 1.
*
*/

REAL gaussian()
{
  int k;
  REAL sum = 0.0;

  for ( k = 0 ; k < 5 ; k++ ) sum += (REAL)(rand()&0x7FFF);
  return (sum/0x7FFFL - 2.5)/5*sqrt(12.0/5);
}


/***************************************************************
*
*    Function: estimate_decrease()
*
*    Purpose:  Estimates energy decrease from gradients of all 
*                 unfixed vertices using current scale factor and force.
*                 Does not include volume retoration.
*
*    Input:     uses global variable for scale
*/

REAL estimate_decrease()
{ int i;
  REAL change = 0.0;

  for ( i = 0 ; i < optparamcount ; i++ ) 
    change += web.scale*optparam[i].grad*optparam[i].velocity;

#ifdef MPI_EVOLVER
  change += mpi_v_estimate();
#else
  change += v_estimate();
#endif

  estimated_change = -change;  /* for estimated_change internal variable */
  return -change;  /* negative since forces are opposite gradients */
  
}

/**************************************************************************
*
* function: v_estimate()
*
* purpose: Calculate vertex part of estimated energy change.
*/

REAL v_estimate()
{
  vertex_id v_id;
  REAL *force;
  REAL *velocity;
  int i,j;
  REAL change = 0.0;
  REAL delta;

  FOR_ALL_VERTICES(v_id)
  { if ( get_vattr(v_id) & FIXED ) continue;
    force = get_force(v_id);
    velocity = get_velocity(v_id);
    delta = 0.0;
    if ( get_vattr(v_id) & BOUNDARY )
    { REAL temp[MAXCOORD],dummy;
      struct boundary *boundary = get_boundary(v_id);
      int pcount = boundary->pcount;
      REAL *param = get_param(v_id);
      for ( j = 0 ; j < SDIM ; j++ )
      { eval_all(boundary->coordf[j],param,pcount,&dummy,temp,v_id);
        delta += web.scale*force[j]*dot(temp,velocity,pcount);
      }
    }
    else
    { for ( i = 0 ; i < SDIM ; i++ )
        delta += web.scale*force[i]*velocity[i];
    }
    change += delta;
  }
  return change;
} /* end v_estimate() */

/********************************************************************
*
*  Function: homothety()
*
*  Purpose: homothety to scale to fixed area or volume.
*              Normalizes total volume of all bodies to 1.
*/

void homothety()
{
  body_id b_id;
  vertex_id v_id;
  REAL vol = 0.0;
  REAL *x;
  REAL scale;
  int i;

  if ( square_curvature_flag )
     scale = 1/pow(web.total_area/homothety_target,1.0/web.dimension);
  else
  {
    /* be sure to have current volumes */
    calc_content(Q_FIXED); 
    FOR_ALL_BODIES(b_id)
        vol += get_body_volume(b_id);
    if ( web.representation == STRING ) scale = 1/sqrt(vol/homothety_target);
    else scale = 1/pow(vol/homothety_target,1/3.0);
  }
  FOR_ALL_VERTICES(v_id)
  {  x = get_coord(v_id);
     for ( i = 0 ; i < SDIM ; i++ ) x[i] *= scale;
  }
}

/***********************************************************************
*
*  function: cg_calc_gamma()
*
*  purpose:  Calculate conjugate gradient direction adjustment factor.
*
*/

void cg_calc_gamma()
{

  REAL sum = 0.0;
  REAL rsum = 0.0;
  int i;

  if ( ribiere_flag )
  { 
#ifdef MPI_EVOLVER
    rsum = mpi_ribiere_calc();
#else
    rsum = ribiere_calc();
#endif

    for ( i = 0 ; i < optparamcount ; i++ ) 
    { rsum += optparam[i].grad*optparam[i].oldgrad;
      optparam[i].oldgrad = optparam[i].grad;
    }
  }

#ifdef MPI_EVOLVER
    sum = mpi_cg_sum_calc();
#else
    sum = cg_sum_calc();
#endif

  for ( i = 0 ; i < optparamcount ; i++ ) 
      sum += optparam[i].velocity*optparam[i].grad;

  if ( cg_oldsum >= 1e-15 )
     cg_gamma = (sum-rsum)/cg_oldsum;

  if ( cg_gamma > 10.0 ) /* something probably changed too much */
      cg_gamma = 0.0;

  /* guarantee convergence, (Shewchuk notes, p. 42)  */
  if ( ribiere_flag && (cg_gamma < 0.0) ) cg_gamma = 0.0;

  cg_oldsum = sum; return;
} /* end cg_calc_gamma() */

/**************************************************************************
*
* function: ribiere_calc()
*
* purpose: calculate vertex contributions to Ribiere conjugate gradient sum.
*/
REAL ribiere_calc()
{ int i;
  REAL rsum = 0.0;
  vertex_id v_id;

  int r_attr = find_attribute(VERTEX,RIBIERE_ATTR_NAME);
  if ( r_attr < 0 )
      kb_error(1058,"Internal error: Forces not saved for Ribiere CG.\n",
          RECOVERABLE);

  FOR_ALL_VERTICES(v_id)
  { REAL *f = get_force(v_id);
    REAL *v = get_velocity(v_id);
    REAL *g = (REAL*)get_extra(v_id,r_attr);

    if ( get_vattr(v_id) & (FIXED|BOUNDARY) ) continue;
    rsum += SDIM_dot(v,g);
    for ( i = 0 ; i < SDIM ; i++ ) g[i] = f[i]; 
  }
  return rsum;
}

/**************************************************************************
*
* function: cg_sum_calc()
*
* purpose: calculate vertex contributions to Ribiere conjugate gradient sum.
*/
REAL cg_sum_calc()
{ vertex_id v_id; 
  REAL sum = 0.0;
  FOR_ALL_VERTICES(v_id)
  { REAL *f = get_force(v_id);
    REAL *v = get_velocity(v_id);
    if ( get_vattr(v_id) & (FIXED |BOUNDARY) ) continue;
    sum += SDIM_dot(v,f);
  }
  return sum;
}

/***********************************************************************
*
*  function: cg_direction()
*
*  purpose:  Adjusts direction of motion for conjugate gradient.
*
*/

void cg_direction()
{
  int i;

#ifdef MPI_EVOLVER
  mpi_cg_direction();  /* do vertices */
#else
  cg_direction_local();
#endif

  for ( i = 0 ; i < optparamcount ; i++ ) 
  { optparam[i].grad += cg_gamma*optparam[i].cg;
    optparam[i].cg = optparam[i].grad;
  }
}

/**********************************************************************
*
* function: cg_direction_local()
*
* purpose: make conjugate gradient adjustments on vertices.
*/
void cg_direction_local()
{ int i;
  vertex_id v_id;
  REAL *v,*h;

  if ( cg_hvector == NULL )
  { /* reinitialize */
    cg_hvector = (REAL (*)[MAXCOORD])mycalloc(web.skel[VERTEX].max_ord+1,
                                         sizeof(REAL [MAXCOORD]));
    for ( i = 0 ; i < optparamcount ; i++ ) optparam[i].cg = 0.0;
  }

  FOR_ALL_VERTICES(v_id)
  { if ( get_vattr(v_id) & (FIXED|BOUNDARY) ) continue;
    h = cg_hvector[loc_ordinal(v_id)];
    v = get_velocity(v_id);
    for ( i = 0 ; i < SDIM ; i++ )
      h[i] = v[i] += cg_gamma*h[i];
  }
}

/**********************************************************************
*
* function: cg_restart()
*
* purpose: reset conjugate gradient
*/

void cg_restart()
{
#ifdef MPI_EVOLVER
   if ( this_task == MASTER_TASK )
   { struct mpi_command message;
     message.cmd = mpi_CG_RESTART;
     MPI_Bcast(&message,sizeof(struct mpi_command),MPI_BYTE,MASTER_TASK,
         MPI_COMM_WORLD);
   }
#endif
   { myfree((char *)cg_hvector); cg_hvector = NULL; cg_oldsum = 0.0;}
}

/*********************************************************************
*
*  function: runge_kutta()
*
*  purpose: do runge-kutta method for calculating motion.
*              Assumes first force evaluation already done.
*              and vertices saved.  Calculates final motion,
*              but does not actually do it, rather saves in 
*              vertex force array, so that final move can
*              be done in regular flow of control.
*
*/

void runge_kutta()
{
  REAL **k1,**k2,**k3, **k4;  /* saved motions */
  int i;
  vertex_id v_id;

  k1 = dmatrix(0,web.skel[VERTEX].max_ord,0,SDIM-1);
  k2 = dmatrix(0,web.skel[VERTEX].max_ord,0,SDIM-1);
  k3 = dmatrix(0,web.skel[VERTEX].max_ord,0,SDIM-1);
  k4 = dmatrix(0,web.skel[VERTEX].max_ord,0,SDIM-1);

  /* save first motion */
  FOR_ALL_VERTICES(v_id)
     { int k = loc_ordinal(v_id);
        REAL *f = get_force(v_id);
        for ( i = 0 ; i < SDIM ; i++ ) k1[k][i] = f[i];
     }

  /* second motion */
  web.scale /= 2;
  move_vertices(ACTUAL_MOVE,web.scale);
  calc_all_grads(CALC_FORCE|CALC_VOLGRADS);
  volume_restore(1.0,ACTUAL_MOVE);
  FOR_ALL_VERTICES(v_id)
     { int k = loc_ordinal(v_id);
        REAL *f = get_force(v_id);
        for ( i = 0 ; i < SDIM ; i++ ) k2[k][i] = f[i];
     }

  /* third motion */
  restore_coords(&saved,SAVE_IN_ATTR);
  move_vertices(ACTUAL_MOVE,web.scale);
  calc_all_grads(CALC_FORCE|CALC_VOLGRADS);
  volume_restore(1.0,ACTUAL_MOVE);
  FOR_ALL_VERTICES(v_id)
  { int k = loc_ordinal(v_id);
    REAL *f = get_force(v_id);
    for ( i = 0 ; i < SDIM ; i++ ) k3[k][i] = f[i];
  }

  /* fourth motion */
  restore_coords(&saved,SAVE_IN_ATTR);
  web.scale *= 2;
  move_vertices(ACTUAL_MOVE,web.scale);
  calc_all_grads(CALC_FORCE|CALC_VOLGRADS);
  volume_restore(1.0,ACTUAL_MOVE);

  FOR_ALL_VERTICES(v_id)
  { int k = loc_ordinal(v_id);
    REAL *f = get_force(v_id);
    for ( i = 0 ; i < SDIM ; i++ ) k4[k][i] = f[i];
  }

  /* runge-kutta combination */
  FOR_ALL_VERTICES(v_id)
     { int k = loc_ordinal(v_id);
        REAL *f = get_force(v_id);
        for ( i = 0 ; i < SDIM ; i++ )
          f[i] = (k1[k][i] + 2*k2[k][i] + 2*k3[k][i] + k4[k][i])/6;
     }
  restore_coords(&saved,SAVE_IN_ATTR);

  free_matrix(k1);
  free_matrix(k2);
  free_matrix(k3);
  free_matrix(k4);
} /* end runge_kutta() */

/************************************************************************
*
* function: apply_h_inverse_metric();
*
* purpose: Replace force by laplacian of force.  For doing motion
* by laplacian of mean curvature.
*
*/

void apply_h_inverse_metric()
{ REAL *scalarh;  /* scalar curvature in normal direction */
  REAL **normals; /* for soapfilm model */
  REAL *stars;     /* vertex areas */
  edge_id e_id;
  vertex_id v_id;
  int i;
  REAL normal[MAXCOORD];

  if ( (web.representation != STRING)  && (web.representation != SOAPFILM) )
     kb_error(2085,"Can do h_inverse_metric only in STRING and SOAPFILM models.\n",
         RECOVERABLE);

  scalarh = (REAL*)temp_calloc(web.skel[VERTEX].max_ord+5,sizeof(REAL));
  stars = (REAL*)temp_calloc(web.skel[VERTEX].max_ord+5,sizeof(REAL));

  if ( web.representation == STRING )
  {
     /* gather scalar curvatures */
     FOR_ALL_VERTICES(v_id)
     { REAL *f = get_force(v_id);
        REAL *vel = scalarh+loc_ordinal(v_id);
        REAL len,star;
        e_id = get_vertex_edge(v_id);
        len = get_edge_length(e_id)/2;
        e_id = get_next_tail_edge(e_id);
        len += get_edge_length(e_id)/2;
        /* *vel = sqrt(SDIM_dot(f,f))/len; */
        star = calc_vertex_normal(v_id,NULLID,normal)/2;
        if ( effective_area_flag ) len = star;
        stars[loc_ordinal(v_id)] = len;
        *vel = -SDIM_dot(f,normal)/len;
     }

     /* compute Laplacian and multiply by normal */
     FOR_ALL_VERTICES(v_id)
     { REAL *f = get_velocity(v_id);
        REAL len1,len2;
        vertex_id v1,v2;
        REAL h1,h,h2;
        REAL lap;
        h = scalarh[loc_ordinal(v_id)];
        e_id = get_vertex_edge(v_id);
        v1 = get_edge_headv(e_id);
        len1 = get_edge_length(e_id);
        h1 = scalarh[loc_ordinal(v1)];
        e_id = get_next_tail_edge(e_id);
        v2 = get_edge_headv(e_id);
        len2 = get_edge_length(e_id);
        h2 = scalarh[loc_ordinal(v2)];
        lap = ((h1-h)/len1 + (h2-h)/len2)/stars[loc_ordinal(v_id)];

        /* now multiply be normal */
        calc_vertex_normal(v_id,NULLID,normal);
        for ( i = 0 ; i < SDIM ; i++ )
          f[i] = lap*normal[i];
     }
  }

  if ( web.representation == SOAPFILM )
  {
     normals = dmatrix(0,web.skel[VERTEX].max_ord+1,0,MAXCOORD-1);
     /* gather scalar curvatures */
     FOR_ALL_VERTICES(v_id)
     { REAL *f = get_force(v_id);
        REAL *vel = scalarh+loc_ordinal(v_id);
        REAL star;
        REAL *norm = normals[loc_ordinal(v_id)];
        star = calc_vertex_normal(v_id,get_vertex_fe(v_id),norm)/3;
        if ( !effective_area_flag )
              star = get_vertex_area_star(v_id)/3;
        *vel = SDIM_dot(f,norm)/star;
        stars[loc_ordinal(v_id)] = star;
     }

     /* compute Laplacian and multiply by normal */
     FOR_ALL_VERTICES(v_id)
     { REAL *f = get_velocity(v_id);
        REAL *norm = normals[loc_ordinal(v_id)];
        REAL h1,h,h2;
        REAL lap;
        REAL side1[MAXCOORD],side2[MAXCOORD];
        facetedge_id fe,start_fe,next_fe;
        edge_id e1,e2;

        h = scalarh[loc_ordinal(v_id)];

        lap = 0.0;
        fe = start_fe = get_vertex_fe(v_id);
        do
        { REAL s11,s12,s22,det;
          next_fe = inverse_id(get_prev_edge(fe));
          e1 = get_fe_edge(fe);
          get_edge_side(e1,side1);
          h1 = scalarh[loc_ordinal(get_edge_headv(e1))];

          e2 = get_fe_edge(next_fe);
          get_edge_side(e2,side2);
          h2 = scalarh[loc_ordinal(get_edge_headv(e2))];
          s11 = SDIM_dot(side1,side1);
          s12 = SDIM_dot(side1,side2);
          s22 = SDIM_dot(side2,side2);
          det = sqrt(s11*s22 - s12*s12);
          lap += ((s11-2*s12+s22)*h + (s12-s22)*h1 + (s12-s11)*h2)/det;
          fe = get_next_facet(next_fe);
        } while ( !equal_id(fe,start_fe) );
        lap /= 2*stars[loc_ordinal(v_id)];

        /* now multiply by unit normal */
        for ( i = 0 ; i < SDIM ; i++ )
          f[i] = lap*norm[i];
     }
     free_matrix(normals);
  }

  temp_free((char*)scalarh);
  temp_free((char*)stars);
} /* end apply_h_inverse_metric() */


/***************************************************************
*
*    Function: convert_forms_to_vectors()
*
*    Purpose: wrapper for local_convert_forms_to_vectors()
*/
void convert_forms_to_vectors(mode)
int mode; /* bits for CALC_FORCE and CALC_VOLGRADS */
{
  if ( itdebug ) 
  { sprintf(msg,"convert_forms_to_vectors(%s %s)\n",
       (mode&CALC_FORCE)?"CALC_FORCE":"",(mode&CALC_VOLGRADS)?"CALC_VOLGRADS":"");
    outstring(msg);
  }

  #ifdef MPI_EVOLVER
  if ( this_task != 0 ) return;
  mpi_convert_forms_to_vectors(mode);
  #else
  local_convert_forms_to_vectors(mode);
  #endif
}

/***************************************************************
*
*    Function: local_convert_forms_to_vectors()
*
*    Purpose: Converts energy and constraint gradients to vector
*                form, using current metric or mobility, and
*                projects vectors to pointwise constraints.
*
*/

void local_convert_forms_to_vectors(mode)
int mode; /* bits for CALC_FORCE and CALC_VOLGRADS */
{
  vertex_id v_id;
  REAL *force,*velocity;
  int i,j,k,m;
  REAL **weights=NULL;
  REAL zener_coeff=0.0;
  MAT2D(a,MAXPARAM,MAXCOORD); 
  int vgrad_attr,vvelocity_attr; /* for debug save of vgrads to attr */
  int eltype;

  if ( zener_drag_flag )
  { int zd;
    zd = lookup_global(ZENER_COEFF_NAME);
    if ( zd >= 0 ) zener_coeff = globals(zd)->value.real;
  }
  if ( approx_curve_flag )
  { /* approx_curvature();*/  /* Dzuik and Schmidt version */
    mobility_setup();
    approx_curv_calc(mode); /* convert form to vector */
    mobility_cleanup();
    return;
  }

  if ( (web.modeltype == LAGRANGE) && web.area_norm_flag )
  { /* set up weight matrix and invert */
     int ctrl;
     struct gauss_lag *gl;
     edge_id e_id;
     facet_id f_id;

     /* Notes: This tries to approximate the inverse of the linear interpolation
         metric.    The inverse metric used here has diagonal sum of blocks which
         are the inverses of the single element metric, with premultiplication
         of the form by diagonal 1/star, and postmultiplication of the vector
         by 1/valence. This results in uniform volume gradient for a flat
         surface, regardless of triangulation.  Drawback is that inverse metric
         is not symmetric if elements not equal size.  But probably still 
         positive definite (in sense of guaranteeing downhill in energy)
     */

     if ( web.representation == STRING )
     { gl = &gauss_lagrange[web.dimension][web.gauss1D_order];
       ctrl = web.skel[EDGE].ctrlpts;
     } else
     { gl = &gauss_lagrange[web.dimension][web.gauss2D_order];
       ctrl = web.skel[FACET].ctrlpts;
     }
     weights = dmatrix(0,ctrl,0,ctrl);
     for ( i = 0 ; i < ctrl ; i++ )
       for ( j = 0 ; j < ctrl ; j++ ) 
       { weights[i][j] = 0.0;
         for ( m = 0 ; m < gl->gnumpts ; m++ )
            weights[i][j] += gl->gausswt[m]*gl->gpoly[m][i]*gl->gpoly[m][j];
       }
     mat_inv(weights,ctrl);

     if ( web.representation == STRING )
     { FOR_ALL_EDGES(e_id)
        { vertex_id *v_ptr = get_edge_vertices(e_id);
          ctrl = web.skel[EDGE].ctrlpts;
          for ( i = 0 ; i < ctrl ; i++ )
          { REAL area = get_vertex_length_star(v_ptr[i]);
             REAL *f = get_force(v_ptr[i]);
             volgrad *vgptr0 = get_vertex_vgrad(v_ptr[i]);
             volgrad *vgptr;
             for ( j = 0 ; j < ctrl ; j++ )
             { REAL *vel = get_velocity(v_ptr[j]);
                REAL fudge = 1.0/get_vertex_evalence(v_ptr[j]);
                volgrad *vgptri;
                volgrad *vgptri0 = get_vertex_vgrad(v_ptr[j]);
                if ( mode & CALC_FORCE )
                 for ( k = 0 ; k < SDIM ; k++ )
                    vel[k] += weights[i][j]*f[k]/area;
                if ( mode & CALC_VOLGRADS )
                 for ( vgptri=vgptri0; vgptri ; vgptri = vgptri->chain )
                  for ( vgptr=vgptr0; vgptr ; vgptr = vgptr->chain )
                  { if ( vgptr->fixnum == vgptri->fixnum )
                      for ( k = 0 ; k < SDIM ; k++ )
                        vgptri->velocity[k] += fudge*weights[i][j]*vgptr->grad[k]/area;
                  }
              }
            }
         }
      } /* end string */
     else /* soapfilm */
     { FOR_ALL_FACETS(f_id)
        { vertex_id *v_ptr = get_facet_vertices(f_id);
          ctrl = web.skel[FACET].ctrlpts;
          for ( i = 0 ; i < ctrl ; i++ )
          { REAL area = get_vertex_area_star(v_ptr[i]);
            REAL *f = get_force(v_ptr[i]);
            volgrad *vgptr0 = get_vertex_vgrad(v_ptr[i]);
            volgrad *vgptr;
            for ( j = 0 ; j < ctrl ; j++ )
            { REAL *vel = get_velocity(v_ptr[j]);
              REAL fudge = 1.0/get_vertex_fvalence(v_ptr[j]);
              volgrad *vgptri;
              volgrad *vgptri0 = get_vertex_vgrad(v_ptr[j]);
              if ( mode & CALC_FORCE )
                for ( k = 0 ; k < SDIM ; k++ )
                  vel[k] += weights[i][j]*f[k]/area;
              if ( mode & CALC_VOLGRADS )
                for ( vgptri=vgptri0; vgptri ; vgptri = vgptri->chain )
                 for ( vgptr=vgptr0; vgptr ; vgptr = vgptr->chain )
                 { if ( vgptr->fixnum == vgptri->fixnum )
                     for ( k = 0 ; k < SDIM ; k++ )
                        vgptri->velocity[k] += fudge*weights[i][j]*vgptr->grad[k]/area;
                 }
            }
          }
        }
      } /* end soapfilm */
    }  /* end Lagrange area_normalization */

  FOR_ALL_VERTICES(v_id)
  { ATTR attr = get_vattr(v_id);
    volgrad *vgptr0 = get_vertex_vgrad(v_id);
    volgrad *vgptr;

    force = get_force(v_id);
    velocity = get_velocity(v_id);
    if ( mode & CALC_FORCE )
      if ( attr & FIXED )
      { memset((char*)velocity,0,SDIM*sizeof(REAL));
        for ( vgptr=vgptr0 ; vgptr ; vgptr = vgptr->chain )
          for ( i = 0 ; i < SDIM ; i++ )
            vgptr->velocity[i] = 0.0;
             continue;
      }
        
    /* convert form to vector */

    if ( web.metric_flag && metric_convert_flag )
      FOR_ALL_VERTICES(v_id)
      { REAL *x = get_coord(v_id);
        REAL rr,rf;

        if ( klein_metric_flag )
        { /* M^-1 = (I - rxr)*(1-r^2) */
          rr = SDIM_dot(x,x);
          rf = SDIM_dot(x,force);
          if ( mode & CALC_FORCE )
             for ( j = 0 ; j < SDIM ; j++ )
               velocity[j] = (force[j] - x[j]*rf)*(1-rr);
          if ( mode & CALC_VOLGRADS )
            for ( vgptr=vgptr0; vgptr ; vgptr = vgptr->chain )
            { rf = SDIM_dot(x,vgptr->grad);
              for ( i = 0 ; i < SDIM ; i++ )
                vgptr->velocity[i] = (vgptr->grad[i] - x[i]*rf)*(1-rr);
            }
         }
         else if ( web.conformal_flag )
         { REAL gg = eval(&web.metric[0][0],x,NULLID,NULL);
           if ( gg == 0.0 )
             kb_error(1582,"Conformal metric evaluates to zero.\n",WARNING);
           else 
           { if ( mode & CALC_FORCE )
               for ( j = 0 ; j < SDIM ; j++ )
                 velocity[j] = force[j]/gg;
             if ( mode & CALC_VOLGRADS )
               for ( vgptr=vgptr0; vgptr ; vgptr = vgptr->chain )
                 for ( j = 0 ; j < SDIM ; j++ )
                   vgptr->velocity[j] = vgptr->grad[j]/gg;
           }
         }
         else
         { /* if here, have general metric */
           for (  i = 0 ; i < SDIM ; i++ )
             for ( j = 0 ; j < SDIM ; j++ )
                metric[i][j] = eval(&web.metric[i][j],x,NULLID,NULL);
           mat_inv(metric,SDIM);
           if ( mode & CALC_FORCE )
              matvec_mul(metric,force,velocity,SDIM,SDIM);
           if ( mode & CALC_VOLGRADS )
              for ( vgptr=vgptr0; vgptr ; vgptr = vgptr->chain )
                matvec_mul(metric,vgptr->grad,vgptr->velocity,SDIM,SDIM);
         }

      }
      else
      if ( web.area_norm_flag && !approx_curve_flag) 
        switch ( web.modeltype )
        { case LINEAR:
          { REAL area=((web.representation==STRING)?get_vertex_length_star(v_id):
                              get_vertex_area_star(v_id))/star_fraction;
            REAL ff;
            if ( get_vattr(v_id) & FIXED ) continue;
            if ( effective_area_flag && (web.representation == STRING) )
            { /* calculate effective area */
              int valence = get_vertex_evalence(v_id);
              if ( (valence == 2) || (valence==1) )
              { REAL dummy[MAXCOORD];;
                area = calc_vertex_normal(v_id,get_vertex_fe(v_id),dummy)/2;
              }
              else if ( valence == 0 )
                area = 1.0;  /* disconnected pt; no force anyway */
              else /* triple point at least */
              { edge_id e_id,start_e;
                REAL ss,fs,side[MAXCOORD];
                area = 0.0;
                e_id = start_e = get_vertex_edge(v_id);
                do
                { double det;
                  get_edge_side(e_id,side);
                  ss = SDIM_dot(side,side);
                  fs = SDIM_dot(force,side);
                  ff = SDIM_dot(force,force);
                  det = ff*ss - fs*fs;
                  if ( det > 0.0 )
                     area += 0.5*sqrt(det)/sqrt(ff);
                   e_id = get_next_tail_edge(e_id);
                 }
                 while ( !equal_id(e_id,start_e) );
               }
               set_vertex_star(v_id,star_fraction*area);
             }
             else if ( effective_area_flag && (web.representation == SOAPFILM) )
             { /* crude correction for triple edges and tetra points */
                REAL dummy[MAXCOORD];
                if ( get_vattr(v_id) & TRIPLE_PT )
                { area /= sqrt(3.);
                }
                else if ( get_vattr(v_id) & TETRA_PT )
                { area /= sqrt(6.);
                }
                else area = calc_vertex_normal(v_id,get_vertex_fe(v_id),dummy)
                                              /star_fraction;
             }
             if ( area == 0.0 )  
             { 
               if ( mode & CALC_FORCE )
                for ( i = 0 ; i < SDIM ; i++ )
                  velocity[i] = 0.0;
               if ( mode & CALC_VOLGRADS )
                for ( vgptr=vgptr0; vgptr ; vgptr = vgptr->chain )
                   for ( i = 0 ; i < SDIM ; i++ )
                      vgptr->velocity[i] = 0.0;

               sprintf(errmsg,"Zero area star around vertex %s.\n",
                 ELNAME(v_id));
               kb_error(1775,errmsg,WARNING);
             }
             else
             {
               if ( mode & CALC_FORCE )
                for ( i = 0 ; i < SDIM ; i++ )
                  velocity[i] = force[i]/area;
               if ( mode & CALC_VOLGRADS )
                for ( vgptr=vgptr0; vgptr ; vgptr = vgptr->chain )
                   for ( i = 0 ; i < SDIM ; i++ )
                      vgptr->velocity[i] = vgptr->grad[i]/area;
             }

            }
            break;

            case QUADRATIC:
            /* area weights carefully chosen to make volume grad vector 
                constant for flat surface */
              { edge_id e_id, start_e;

                 if ( get_vattr(v_id) & Q_MIDPOINT )
                 { REAL area = (web.representation==STRING) ?
                                get_vertex_length_star(v_id)*2/3.:
                                get_vertex_area_star(v_id)*2/3.;
                    if ( mode & CALC_FORCE )
                     for ( i = 0 ; i < SDIM ; i++ )
                      velocity[i] = force[i]/area;
                    if ( mode & CALC_VOLGRADS )
                     for ( vgptr=vgptr0; vgptr ; vgptr = vgptr->chain )
                      for ( i = 0 ; i < SDIM ; i++ )
                         vgptr->velocity[i] = vgptr->grad[i]/area;
                 }
                 else /* corner vertex */
                 { REAL area;
                 
                    /* first, self term */
                    area = (web.representation==STRING) ?
                                get_vertex_length_star(v_id)/6:
                                get_vertex_area_star(v_id)/5.;
                    if ( mode & CALC_FORCE )
                     for ( i = 0 ; i < SDIM ; i++ )
                      velocity[i] = force[i]/area;
                    if ( mode & CALC_VOLGRADS )
                     for ( vgptr=vgptr0; vgptr ; vgptr = vgptr->chain )
                      for ( i = 0 ; i < SDIM ; i++ )
                         vgptr->velocity[i] = vgptr->grad[i]/area;

                    /* next, neighboring midpoints */
                    if ( web.representation == SOAPFILM )
                    { start_e = e_id = get_vertex_edge(v_id);
                      area = get_vertex_area_star(v_id)*4/3;
                      do 
                      { vertex_id vv_id = get_edge_midv(e_id);
                         REAL *ff = get_force(vv_id);
                         volgrad *vgptri;
                         volgrad *vgptri0 = get_vertex_vgrad(vv_id);

                         if ( mode & CALC_FORCE )
                          for ( i = 0 ; i < SDIM ; i++ )
                            velocity[i] += ff[i]/area;
                         if ( mode & CALC_VOLGRADS )
                          for ( vgptri=vgptri0; vgptri ; vgptri = vgptri->chain )
                            for ( vgptr=vgptr0; vgptr ; vgptr = vgptr->chain )
                            { if ( vgptr->fixnum == vgptri->fixnum )
                                 for ( i = 0 ; i < SDIM ; i++ )
                                    vgptr->velocity[i] += vgptri->grad[i]/area;
                            }
                         e_id = get_next_tail_edge(e_id);
                      } while ( !equal_id(start_e,e_id) );
                    }
                 }
              }
              break;

            case LAGRANGE: /* did earlier */
            break;

          } /* end area_normalization */

          else /* not any special mobility */
          {
             if ( mode & CALC_FORCE )
              for ( i = 0 ; i < SDIM ; i++ )
                velocity[i] = force[i]; /* identity metric */
             if ( mode & CALC_VOLGRADS )
              for ( vgptr=vgptr0; vgptr ; vgptr = vgptr->chain )
                for ( i = 0 ; i < SDIM ; i++ )
                  vgptr->velocity[i] = vgptr->grad[i];
          }


    /* apply mobility */
    if ( mobility_flag )
    { if ( mobility_tensor_flag )
      { REAL *x = get_coord(v_id);
        REAL newv[MAXCOORD];
        REAL mob[MAXCOORD][MAXCOORD];

        for ( i = 0 ; i < SDIM ; i++ )
          for ( j = 0 ; j < SDIM ; j++ ) 
            mob[i][j] = eval(&mobility_tensor[i][j],x,v_id,NULL);

        if ( mode & CALC_FORCE )
        { for ( i = 0 ; i < SDIM ; i++ )
            for ( newv[i] = 0.0, j = 0; j < SDIM ; j++ )
              newv[i] += mob[i][j]*velocity[j];
          for ( i = 0 ; i < SDIM ; i++ )
            velocity[i] = newv[i];
        }

        if ( mode & CALC_VOLGRADS )
          for ( vgptr=vgptr0; vgptr ; vgptr = vgptr->chain )
          { for ( i = 0 ; i < SDIM ; i++ )
              for ( newv[i] = 0.0, j = 0; j < SDIM ; j++ )
                newv[i] += mob[i][j]*vgptr->velocity[j];
            for ( i = 0 ; i < SDIM ; i++ )
              vgptr->velocity[i] = newv[i];
          }
      } /* end mobility_tensor */
      else
      { REAL mobility = eval(&mobility_formula,get_coord(v_id),v_id,NULL);
        if ( mode & CALC_FORCE )
          for ( j = 0 ; j < SDIM ; j++ )
            velocity[j] *= mobility;
        if ( mode & CALC_VOLGRADS )
          for ( vgptr=vgptr0; vgptr ; vgptr = vgptr->chain )
            for ( i = 0 ; i < SDIM ; i++ )
              vgptr->velocity[i] *= mobility;
      }
    } /* end mobility */
    


        /* project vector to pointwise constraints */

        if ( normal_motion_flag && !(attr & BOUNDARY) )  
        { /* project to normal */
          REAL d;
          REAL *normal = vertex_normals[loc_ordinal(v_id)];
          REAL *h;

          if ( mode & CALC_FORCE )
          { d = SDIM_dot(velocity,normal);
            for ( j = 0 ; j < SDIM ; j++ )
              velocity[j] = d*normal[j];

            if ( cg_hvector ) /* conjugate gradient history vector also */
            { h = cg_hvector[loc_ordinal(v_id)];
              d = SDIM_dot(h,normal);
              for ( j = 0 ; j < SDIM ; j++ )
                h[j] = d*normal[j];
            }
          }
          if ( mode & CALC_VOLGRADS )
            for ( vgptr=vgptr0; vgptr ; vgptr = vgptr->chain )
            { d = SDIM_dot(vgptr->velocity,normal);
              for ( i = 0 ; i < SDIM ; i++ )
                 vgptr->velocity[i] = d*normal[i];
            }
        } /* end normal motion */

        if ( (attr & CONSTRAINT) && (!check_pinning_flag || (attr & PINNED_V)) )
        { conmap_t * conmap = get_v_constraint_map(v_id);
          int oncount = 0;
          struct constraint *con[MAXCONPER];
          int conlist[MAXCONPER];
          REAL perp[MAXCOORD];
          int one_sided_flag = 0;

          for ( j = 1 ; j <= (int)conmap[0] ; j++ )
          { if ( conmap[j] & CON_HIT_BIT )
            { conlist[oncount] = conmap[j] & CONMASK;
              con[oncount] = get_constraint(conmap[j]);
              if ( con[oncount]->attr & (NONPOSITIVE|NONNEGATIVE) )
                one_sided_flag = 1;
              oncount++; 
            }
          }

          if ( oncount > SDIM ) 
          { sprintf(errmsg,
              "Vertex %s is on more constraints than the dimension of space.\n",
                      ELNAME(v_id));
            kb_error(2086,errmsg,WARNING);
            oncount = SDIM;
          }

          if ( one_sided_flag )
          { REAL *raw_velocity = VREAL(v_id,raw_velocity_attr);
            for ( j = 0 ; j < SDIM ; j++ )
              raw_velocity[j] = velocity[j];
            for ( vgptr=vgptr0; vgptr ; vgptr = vgptr->chain )
              for ( j = 0 ; j < SDIM ; j++ )
                vgptr->raw_velocity[j] = vgptr->velocity[j];
          }

          if ( oncount )
          { if ( mode & CALC_FORCE )
            { constr_proj(TANGPROJ,oncount,con,get_coord(v_id),
                                 velocity,perp,conlist,DETECT,v_id);
              for ( j = 0 ; j < SDIM ; j++ )
                velocity[j] -= perp[j];

              if ( web.area_norm_flag && (web.representation==STRING) )
              { /* correction factor for moving vertex along constraint
                   is to divide projected force by sin^2 of contact angle,
                   here crudely estimated. */
                REAL sinsq = SDIM_dot(perp,perp);
                if ( (sinsq != 0.0) && (sinsq < 1.0) )
                for ( j = 0 ; j < SDIM ; j++ )
                  velocity[j] /= sinsq;
              }
            }

            if ( mode & CALC_VOLGRADS )
              for ( vgptr=vgptr0; vgptr ; vgptr = vgptr->chain )
              { REAL perp[MAXCOORD];
               constr_proj(TANGPROJ,oncount,con,get_coord(v_id),
                               vgptr->velocity,perp,NULL,NO_DETECT,v_id);
                for ( j = 0 ; j < SDIM ; j++ )
                  vgptr->velocity[j] -= perp[j];
              }

           }
         }
     } /* end for all vertices */

  for ( k = 0 ; k < optparamcount ; k++ )
    optparam[k].velocity = 
        globals(optparam[k].pnum)->attr.varstuff.pscale*optparam[k].grad;

  if ( zener_drag_flag )
  { FOR_ALL_VERTICES(v_id) 
    { REAL *v = get_velocity(v_id);
      REAL mag = sqrt(SDIM_dot(v,v));
      for ( k = 0 ; k < SDIM ; k++ )
        if ( mag > zener_coeff )
           v[k] = (mag - zener_coeff)*v[k]/mag;
        else v[k] = 0.0;
    }
  }

  if ( web.h_inverse_metric_flag ) apply_h_inverse_metric();

  /* project to parameter space for boundary points */   
  if ( (mode & CALC_FORCE) && web.bdrymax )
   FOR_ALL_VERTICES(v_id)
   {
     int pcount;
     REAL *v = get_velocity(v_id);
     REAL tmp[MAXCOORD];
     struct boundary *bdry;
     int m;
     ATTR attr = get_vattr(v_id);

     if ( attr & FIXED ) continue;
     if ( !(attr & BOUNDARY) ) continue;
     bdry = get_boundary(v_id);
     pcount = bdry->pcount;
     b_proj(bdry,get_param(v_id),a,PARAMPROJ,v_id);
     matvec_mul(a,v,tmp,pcount,SDIM);
     for ( m = 0 ; m < pcount ; m++ ) v[m] = tmp[m];
     for ( m = pcount ; m < SDIM ; m++ ) v[m] = 0.0;    
   }

  /* for debugging, transfer volgrads to vertex attribute */
  vgrad_attr = find_extra("__volgrad",&eltype);
  vvelocity_attr = find_extra("__volvelocity",&eltype);
  if ( (vgrad_attr >= 0) || (vvelocity_attr >= 0) )
  { /* user should expand so each vertex has volgrad for each body */
    struct extra *gradex = EXTRAS(VERTEX)+vgrad_attr;
    int maxgradbody = gradex->array_spec.sizes[0];
    int maxgraddim = gradex->array_spec.sizes[1];
    struct extra *velex = EXTRAS(VERTEX)+vvelocity_attr;
    int maxvelbody = velex->array_spec.sizes[0];
    int maxveldim = gradex->array_spec.sizes[1];
    if ( maxveldim > SDIM ) maxveldim = SDIM;
    if ( maxgraddim > SDIM ) maxgraddim = SDIM;
    FOR_ALL_VERTICES(v_id)
    { struct volgrad *vgptr;
      REAL *vg = (vgrad_attr >= 0 ) ? VREAL(v_id,vgrad_attr) : NULL;
      REAL *vv = (vvelocity_attr >= 0 ) ? VREAL(v_id,vvelocity_attr) : NULL;
      if ( vgrad_attr >= 0 )
         memset((char*)vg,0,maxgradbody*maxgraddim*sizeof(REAL));
      if ( vvelocity_attr >= 0 )
         memset((char*)vv,0,maxvelbody*maxveldim*sizeof(REAL));
      for ( vgptr=get_vertex_vgrad(v_id) ; vgptr ; vgptr = vgptr->chain )
      { if ( valid_id(vgptr->bb_id) ) /* just doing bodies */
        { int bnum = loc_ordinal(vgptr->bb_id);
          if ( (vgrad_attr >= 0) && (bnum < maxgradbody) )
          { 
            for ( i = 0 ; i < maxgraddim ; i++ )
              vg[bnum*gradex->array_spec.sizes[1]+i] = vgptr->grad[i];
          }
          if ( (vvelocity_attr >= 0) && (bnum < maxvelbody) )
          { 
            for ( i = 0 ; i < maxveldim ; i++ )
              vv[bnum*velex->array_spec.sizes[1]+i] = vgptr->velocity[i];
          }
        }
      }
    }
  }

  if ( weights ) free_matrix(weights);
} /* end convert_forms_to_vectors() */

/*************************************************************************
*
* function: check_pinning()
*
* purpose:  check for vertices that can't move because adjacent
*           vertices are not on same constraint when they could be 
*/

void check_pinning()
{ edge_id e_id;
  vertex_id v_id;
  int i,j;

  FOR_ALL_VERTICES(v_id) /* clear pinning flag */
     vptr(v_id)->attr &= ~PINNED_V;
  FOR_ALL_EDGES(e_id)
  { 
    vertex_id headv = get_edge_headv(e_id);
    vertex_id tailv = get_edge_tailv(e_id);
    conmap_t * hstat = get_v_constraint_map(headv);
    conmap_t * tstat = get_v_constraint_map(tailv);
    for ( i=1, j=0 ;  i <= (int)hstat[0] ; i++ ) 
       if (hstat[i] & CON_HIT_BIT) j++;
    if ( j == 1 )
       set_attr(tailv,PINNED_V);
    for ( i=1, j=0 ;  i <= (int)tstat[0] ; i++ ) 
       if (tstat[i] & CON_HIT_BIT) j++;
    if ( j == 1 )
       set_attr(headv,PINNED_V);
  }
  FOR_ALL_VERTICES(v_id) /* see if 2 con vertex has 2 con nbrs */
    { conmap_t * hit = get_v_constraint_map(v_id);
      for ( i = 1, j = 0 ; i <= (int)hit[0] ; i++ ) 
         if ( hit[i] & CON_HIT_BIT ) j++;
      if ( j != 2 )
         set_attr(v_id,PINNED_V);
    }
} /* end check_pinning() */

