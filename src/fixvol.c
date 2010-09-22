/*************************************************************
*  This file is part of the Surface Evolver source code      *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/******************************************************
*
*  File: fixvol.c
*
*  Contents: Routines for projecting force on vertices
*        to perpendicular of all body volume gradients.
*        Does constrained quantities also.
*
*/

#include "include.h"

static  int maxquants;  /* total number of constraints */
REAL *vpressures = NULL; /* so global pressures not messed */
static  int fixcount = 0;  /* number of constrained quantities */
/* for numbering various types of constraints */
static  int gen_quant_start;
static  int one_sided_start_fixcount;
static  struct linsys LS;  /* for sparse constraints */

struct fixcount_list { vertex_id v_id; int connum; };
static struct fixcount_list *one_sided_fixcount_list;

REAL **vgev = NULL;  /* vector vol grads, for approx curvature */
REAL **vgef = NULL;  /* form vol grads, for approx curvature */

/*************************************************************************
*
* Function: calc_volgrads()
*
* purpose: wrapper for local_calc_volgrads()
*
*/
void calc_volgrads(mode)
int mode; 
{
   if ( itdebug)
    outstring("Calculating volgrads.\n");
 
   
  #ifdef MPI_EVOLVER
   mpi_calc_volgrads(mode);
  #else
   local_calc_volgrads(mode);
  #endif

#ifdef _DEBUGXXX
  if ( itdebug ) /* dump vgrads */
  { vertex_id v_id;
    FOR_ALL_VERTICES(v_id)
    { volgrad  *vgptr;

      vgptr = get_vertex_vgrad(v_id);
      while ( vgptr )
      { printf("v %3s   b %3s  grad %f %f %f\n",ELNAME(v_id),
         ELNAME1(vgptr->bb_id),vgptr->grad[0],vgptr->grad[1],vgptr->grad[2]);
        vgptr = vgptr->chain;
      }
    }
  }
#endif
}

/*************************************************************************
*
*  Function:  local_calc_volgrads()
*
*  purpose: calculate gradients of constrained volumes and quantities.
*          Leaves them in vgptr... array.
*        
*          Also does variable_parameter constraint gradients.
*/

void local_calc_volgrads(mode)
int mode; /* DO_OPTS or NO_OPTS */
{
  body_id bi_id;  /* identifier for body i */
  vertex_id v_id;
  int i,k,n;
  struct boundary *bdry;
  int qfixed = 0;
  struct gen_quant *gq;
  MAT2D(a,MAXCOORD,MAXCOORD);
 
  /* for numbering various types of constraints */
  gen_quant_start = web.skel[BODY].max_ord + 1;
 
  /* see if anything needs to be done */
  fixed_constraint_flag = 0;
  if ( !web.pressure_flag && !everything_quantities_flag )
    FOR_ALL_BODIES(bi_id)
     if ( get_battr(bi_id) & (FIXEDVOL|PRESSURE) ) fixed_constraint_flag = 1;
  for ( k = n = 0 ; n < gen_quant_count ; n++ )
  { gq = GEN_QUANT(n);
     if ( gq->flags & (Q_FIXED|Q_CONSERVED) )
     { if ( !valid_id(gq->b_id) || !web.pressure_flag ) 
       { fixed_constraint_flag = 1; qfixed++;
         gq->vol_number = gen_quant_start + k++;
       }
       else 
         if ( web.pressure_flag )
           gq->vol_number = gen_quant_start + k++;
     }
  }
  maxquants = gen_quant_start + k;
  if ( !web.pressure_flag && !fixed_constraint_flag && !one_sided_present )
    return;

  /* allocate space to hold vertex body volume gradients */
  vgrad_init(qfixed);

  /* calculate body volume gradients at all control points 
      due to free surfaces */
  if ( !quantities_only_flag )
  { if ( web.representation == SIMPLEX ) 
      simplex_grad_l();
    else if ( web.representation == STRING )
      (*string_grad)();
    else /* web.representation == SOAPFILM */
      (*film_grad)();
  }
  calc_quant_grads(Q_FIXED|Q_CONSERVED);

  // calc_one_sided_grads();  /* no, doing one-sided lagranges after others */

  /* calculate optimizing_parameter gradients by finite differences */
  if ( (mode == DO_OPTS) && (optparamcount > 0) )
  { REAL **convalues; 
     struct oldcoord osaved;

     if ( optparam_congrads ) free_matrix(optparam_congrads);
     optparam_congrads = dmatrix(0,optparamcount,0,maxquants);
     convalues = dmatrix(0,maxquants,0,2);
     osaved.coord = NULL;
     save_coords(&osaved,SAVE_SEPARATE);
     /* save values */
     if ( !web.pressure_flag && !everything_quantities_flag )
      FOR_ALL_BODIES(bi_id)
        if ( get_battr(bi_id) & FIXEDVOL ) 
          convalues[loc_ordinal(bi_id)][0] = get_body_volume(bi_id);
     for ( k = n = 0 ; n < gen_quant_count ; n++ )
     { gq = GEN_QUANT(n);
       if ( gq->flags & Q_FIXED )
         if ( !valid_id(gq->b_id) || !web.pressure_flag ) 
           convalues[gq->vol_number][0] = gq->value;
     }
     for ( i = 0 ; i < optparamcount ; i++ )
     { REAL dp;

       dp = globals(optparam[i].pnum)->attr.varstuff.delta;

       /* right difference */
       globals(optparam[i].pnum)->value.real += dp;
       project_all(0, TEST_MOVE);
       calc_content(Q_FIXED);
       /* save values */
       if (!web.pressure_flag &&  !everything_quantities_flag )
         FOR_ALL_BODIES(bi_id)
           if ( get_battr(bi_id) & FIXEDVOL ) 
             convalues[loc_ordinal(bi_id)][1] = get_body_volume(bi_id);
       for ( k = n = 0 ; n < gen_quant_count ; n++ )
       { gq = GEN_QUANT(n);
         if ( gq->flags & Q_FIXED )
           if ( !valid_id(gq->b_id) || !web.pressure_flag )
             convalues[gq->vol_number][1] = gq->value;
       }
       restore_coords(&osaved,SAVE_SEPARATE);  /* also fixes opt params */

       /* left difference */
       globals(optparam[i].pnum)->value.real -= dp;
       project_all(0, TEST_MOVE);
       if ( fixed_constraint_flag || web.pressure_flag || web.pressflag )
       calc_content(Q_FIXED);
       /* save values */
       if (!web.pressure_flag &&  !everything_quantities_flag )
         FOR_ALL_BODIES(bi_id)
           if ( get_battr(bi_id) & FIXEDVOL ) 
             convalues[loc_ordinal(bi_id)][2] = get_body_volume(bi_id);
       for ( k = n = 0 ; n < gen_quant_count ; n++ )
       { gq = GEN_QUANT(n);
         if ( gq->flags & Q_FIXED )
          if ( !valid_id(gq->b_id) || !web.pressure_flag )
            convalues[gq->vol_number][2] = gq->value;
       }
       restore_coords(&osaved,SAVE_SEPARATE);  /* also fixes opt params */
   
       /* calculate gradients */
       if (!web.pressure_flag &&  !everything_quantities_flag )
         FOR_ALL_BODIES(bi_id)
           if ( get_battr(bi_id) & FIXEDVOL ) 
             optparam_congrads[i][loc_ordinal(bi_id)] =
               (convalues[loc_ordinal(bi_id)][1] -
                  convalues[loc_ordinal(bi_id)][2])/2/dp;
       for ( k = n = 0 ; n < gen_quant_count ; n++ )
       { gq = GEN_QUANT(n);
         if ( gq->flags & Q_FIXED )
          if ( !valid_id(gq->b_id) || !web.pressure_flag )
           optparam_congrads[i][gq->vol_number] =
             (convalues[gq->vol_number][1]-convalues[gq->vol_number][2])/2/dp;
       }
      }
      /* restore values */
      if (!web.pressure_flag &&  !everything_quantities_flag )
      FOR_ALL_BODIES(bi_id)
        if ( get_battr(bi_id) & FIXEDVOL ) 
           set_body_volume(bi_id, convalues[loc_ordinal(bi_id)][0],SETSTAMP);
      for ( k = n = 0 ; n < gen_quant_count ; n++ )
      { gq = GEN_QUANT(n);
        if ( gq->flags & Q_FIXED )
          if ( !valid_id(gq->b_id) || !web.pressure_flag )
            gq->value = convalues[gq->vol_number][2];
      }
      free_matrix(convalues);
      unsave_coords(&osaved,SAVE_SEPARATE);
  } /* end optimizing parameters */

  /* add on gradients due to constraint integrals */
  if ( !quantities_only_flag )
  { if ( web.representation == STRING )
     { 
       string_constr_grad();
       film_constr_grad();  /* for quantities */
     }
     else /* web.representation == SOAPFILM */
     { 
       film_constr_grad();
     }
  } 
  /* project to parameter space for boundary points */
  FOR_ALL_VERTICES(v_id)
  { REAL  dummy;
    REAL temp[MAXCOORD];
    int pcount,j;
    volgrad *vgptri;

    if ( get_vattr(v_id) & FIXED ) continue;
    if ( !(get_vattr(v_id) & BOUNDARY) ) continue;
    bdry = get_boundary(v_id);
    pcount = bdry->pcount;
    for ( j = 0 ; j < SDIM ; j++ )
    { eval_all(bdry->coordf[j],get_param(v_id),pcount,&dummy,temp,v_id);
      for ( i = 0 ; i < pcount ; i++ )
        a[i][j] = temp[i];
    }

    vgptri = get_vertex_vgrad(v_id);
    while ( vgptri )
      { REAL tmp[MAXCOORD];
         int m;
         matvec_mul(a,vgptri->grad,tmp,pcount,SDIM);
         for ( m = 0 ; m < pcount ; m++ ) vgptri->grad[m] = tmp[m];
         for ( m = pcount ; m < SDIM ; m++ )
           vgptri->grad[m] = 0.0;
         vgptri = vgptri->chain;
      }
  }

} /* end local_calc_volgrads() */

/*************************************************************************
*
* function: calc_one_sided_grads()
*
* purpose: calculate one-sided constraint gradients for vertices
*          hitting constraints, so can calculate Lagrange multipliers
*          so can see if they want to leave constraint.
*/

void calc_one_sided_grads()
{ vertex_id v_id;
  int list_alloc = 2*web.skel[VERTEX].count;
  int list_spot = 0;

  if ( itdebug ) 
    outstring("calc_one_sided_grads() - adding onesided constraints to constraint matrix\n");

  one_sided_start_fixcount = fixcount;

  one_sided_fixcount_list = (struct fixcount_list *)
     temp_calloc(list_alloc,sizeof(struct fixcount_list));

  FOR_ALL_VERTICES(v_id)
  { conmap_t *conmap;
    int j;
    if ( get_vattr(v_id) & FIXED )
       continue;
    conmap = get_v_constraint_map(v_id);
    for ( j = 1 ; j <= (int)conmap[0] ; j++ )
    { struct constraint *thiscon;
      thiscon = get_constraint(conmap[j]);
      if ( (thiscon->attr & (NONNEGATIVE|NONPOSITIVE)) && 
         (conmap[j] & CON_HIT_BIT) )
      { /* need this one */
        REAL fval;
        REAL *x = get_coord(v_id);
        volgrad *vgptr;

        if ( list_spot >= list_alloc )
        { list_alloc *= 2;
          one_sided_fixcount_list = (struct fixcount_list *)
             realloc((char*)one_sided_fixcount_list,
                list_alloc*sizeof(struct fixcount_list));
        }

        one_sided_fixcount_list[list_spot].v_id = v_id;
        one_sided_fixcount_list[list_spot].connum = conmap[j] & CONMASK;
        list_spot++; 
        
        vgptr = get_bv_new_vgrad(fixcount,v_id); 
        vgptr->qnum = conmap[j] & CONMASK;
        vgptr->bb_id = v_id;
        vgptr->fixnum = fixcount;
        eval_all(thiscon->formula,x,SDIM,&fval,vgptr->grad,v_id);
        
        fixcount++;
      }
    } /* end j loop */
  } /* end all vertices */

  /* free extra space */
  one_sided_fixcount_list = (struct fixcount_list *)
     temp_realloc((char*)one_sided_fixcount_list,
        list_spot*sizeof(struct fixcount_list));


  maxquants = fixcount;

} /* end calc_one_sided_grads() */

/**********************************************************************
*
* function: pressure_forces()
*
* purpose: add forces due to dynamic pressure
*/

void pressure_forces()
{ vertex_id v_id;
  int k;
  body_id b_id;
  volgrad *vgptr;
  int to_do = 0;

  if ( everything_quantities_flag ) return; 

  if ( itdebug ) 
      outstring("pressure_forces(): adding forces due to prescribed pressure.\n");

  if ( web.pressure_flag )
  {
    /* add forces due to dynamic pressure */
    FOR_ALL_VERTICES(v_id)
    { REAL *f;
     
      if ( get_vattr(v_id) & FIXED ) continue;
      f = get_force(v_id);
      for ( vgptr = get_vertex_vgrad(v_id) ; vgptr ; vgptr = vgptr->chain )
        {
          if ( valid_id(vgptr->bb_id) ) 
          if ( (get_battr(vgptr->bb_id) & FIXEDVOL) ) 
          { REAL p = get_body_pressure(vgptr->bb_id);
             for ( k = 0 ; k < SDIM ; k++ )
                f[k] += (p - web.pressure)*vgptr->grad[k];
          }
        }
     
    }
    return;
  }

  /* add prescribed pressure forces */
  /* first, see if there are any */
  FOR_ALL_BODIES(b_id)
     if ( get_battr(b_id) & PRESSURE ) to_do = 1;
  
  if ( to_do == 0 ) return;

  FOR_ALL_VERTICES(v_id)
  { REAL *f;

    if ( get_vattr(v_id) & FIXED ) continue;
    f = get_force(v_id);
    for ( vgptr = get_vertex_vgrad(v_id) ; vgptr ; vgptr = vgptr->chain )
    {
      if ( valid_id(vgptr->bb_id) )  /* check for real bodies */
      if ( get_battr(vgptr->bb_id) & PRESSURE )
      for ( k = 0 ; k < SDIM ; k++ )
        f[k] += get_body_pressure(vgptr->bb_id)*vgptr->grad[k];
    }
  }
} /* end pressure_forces() */

/************************************************************************
*
* function: one_sided_adjust()
*
* purpose: see which vertices need to have velocity and vgrads
*         restricted by one-sided constraints. Uses old
*         Lagrange multipliers to calculate force, then sees
*         which constraints violated, and projects tangent
*         to violated constraints and regular constraints.
*/

void one_sided_adjust(mode)
int mode; /* CALC_FORCE and/or CALC_VOLGRADS */
{ vertex_id v_id;
  int i,j,k;
  REAL vel[MAXCOORD];
  int flag;

  /* see if we need to do anything */
  if ( !fixed_constraint_flag ) return;
  for ( i = 0, flag = 0 ; i < web.maxcon ; i++ )
    { struct constraint *con = get_constraint(i);
      if ( con->attr & (NONNEGATIVE|NONPOSITIVE) )
        { flag = 1;  break; }
    }
  if ( flag == 0 ) return;
  
  if ( itdebug ) 
      outstring("one_sided_adjust(): see which grads violate 1-sided constraints \n");

  if ( !pressure_set_flag ) calc_lagrange(); /* for first time only */

  FOR_ALL_VERTICES(v_id)
  {
    volgrad *vgptr;
    int ord = loc_ordinal(v_id);
    REAL *f;
    conmap_t * conmap = get_v_constraint_map(v_id);
    int oncount = 0;
    struct constraint *con[MAXCONPER];
    REAL *x;
    REAL perp[MAXCOORD];
    REAL fval;
    REAL grad[MAXCOORD];
    REAL fp;

    if ( get_vattr(v_id) & FIXED ) continue;
    if ( !(get_vattr(v_id) & CONSTRAINT) ) continue;
    x = get_coord(v_id);

    f = get_velocity(v_id);
    for ( i = 0 ; i < SDIM ; i++ ) vel[i] = f[i];
    for ( vgptr = get_vertex_vgrad(v_id) ; vgptr ; vgptr = vgptr->chain )
    { REAL p; /* Lagrange multiplier */
      if ( valid_id(vgptr->bb_id) )
      p = get_body_pressure(vgptr->bb_id);
      else  /* for quantities */
      p = GEN_QUANT(vgptr->qnum)->pressure;
      if ( approx_curve_flag )
        for ( k = 0 ; k < SDIM ; k++ )
            vel[k] += p*vgev[vgptr->fixnum][SDIM*ord + k];
      else
        for ( k = 0 ; k < SDIM ; k++ )
            vel[k] += p*vgptr->velocity[k];
      /* Note: positive signs since pressure is negative of Lagrange multiplier */
    }

    for ( j = 1,oncount = 0 ; j <= (int)conmap[0] ; j++ )
      {
         if ( conmap[j] & CON_HIT_BIT )
         { struct constraint *cc = get_constraint(conmap[j]);
           if ( cc->attr & (NONNEGATIVE | NONPOSITIVE) )
           { /* check for violation */
             eval_all(cc->formula,x,SDIM,&fval,grad,v_id);
             fp = SDIM_dot(vel,grad);
             if ( (cc->attr & NONNEGATIVE) && (fp < 0.0) )
                con[oncount++] = cc;
             else if ( (cc->attr & NONPOSITIVE) && (fp > 0.0) )
                con[oncount++] = cc;
           }
         }
      }
    if ( mode & CALC_VOLGRADS )
    for ( vgptr = get_vertex_vgrad(v_id) ; vgptr ; vgptr = vgptr->chain )
    { constr_proj(TANGPROJ,oncount,con,x,vgptr->velocity,perp,NULL,NO_DETECT,v_id);
      for ( j = 0 ; j < SDIM ; j++ )
          vgptr->velocity[j] -= perp[j]; 
    }
    if ( mode & CALC_FORCE )
    { constr_proj(TANGPROJ,oncount,con,x,f,perp,NULL,NO_DETECT,v_id);
      for ( j = 0 ; j < SDIM ; j++ )
          f[j] -= perp[j]; 
    }
  }
}  /* one_sided_adjust() */

/***************************************************************************
*
*  function: find_fixed()
*
*  purpose: find which bodies and quantities are fixed and assign them
*           their fixnum.  Sets file static variable fixcount, and returns
*           fixcount.
*/

int find_fixed()
{ int i,k;
  struct gen_quant *q;
  int redundant_bi = -1; /* for torus_filled */
  body_id b_id;

  if ( itdebug ) 
      outstring("find_fixed(): count fixed bodies and constraints\n");

  /* figure out which quantities are fixed */
  fixcount = 0;
  if ( !everything_quantities_flag )
    MFOR_ALL_BODIES(b_id)
      { if ( get_battr(b_id) & (FIXEDVOL/*|PRESSURE*/) ) 
        { if ( web.full_flag && (redundant_bi < 0) )
          { set_body_fixnum(b_id,-1);
            redundant_bi = 1;
          }
          else
          { set_body_fixnum(b_id,fixcount);
            fixcount++;
          }
        } 
        else set_body_fixnum(b_id,-1);
      }
   for ( i = 0,k=0 ; i < gen_quant_count ; i++ )
   { q = GEN_QUANT(i);
      if ( q->flags & (Q_FIXED|Q_CONSERVED) )
      { if ( web.full_flag && valid_id(q->b_id) && redundant_bi < 0 )
        { q->fixnum = -1; 
          redundant_bi = 1;
        }
        else
        { q->fixnum = fixcount++;
          k++;
        }
      }
      else q->fixnum = -1;
   }
  gen_quant_start = web.skel[BODY].max_ord + 1;
  maxquants = gen_quant_start + k;
  fixed_constraint_flag = fixcount;
  return fixcount;
}

/**************************************************************************
*
* function: calc_leftside()
*
* purpose: set up DV^T DV
*/
static int degfree; /* for check there is sufficient degrees of freedom */
static int calc_leftside_hash_count; /* for estimating need */

void calc_leftside()
{ int i,j,k;
  int bi,bj;

  if ( itdebug ) 
      outstring("calc_leftside(): for finding Lagrange multipliers\n");

  degfree = 0;

  /* set up matrices for DV^T DV */
  if ( sparse_constraints_flag )
  { memset(&LS,0,sizeof(LS));
    sp_hash_init(&LS,calc_leftside_hash_count);
  }
  else
  {
    rleftside = dmatrix(0,fixcount,0,fixcount);
  }

  #ifdef MPI_EVOLVER
  if ( sparse_constraints_flag ) 
    kb_error(3381,"Cannot do sparse constraints yet in MPI.\n",RECOVERABLE);
  mpi_calc_leftside(fixcount);
  #else
  local_calc_leftside();
  #endif

  if ( approx_curve_flag )
  { int NV = SDIM*(1+web.skel[VERTEX].max_ord);

    /*  dot products */
    for ( bi = 0 ; bi <= maxquants ; bi++ )
    { int fixi = GEN_QUANT(bi)->fixnum;
      if ( fixi < 0 ) continue;
      /* self product */ 
      if ( sparse_constraints_flag ) 
       { sp_hash_search(&LS,fixi,fixi,dot(vgev[bi],vgef[bi],NV)); 
       }
      else
         rleftside[fixi][fixi] += dot(vgev[fixi],vgef[fixi],NV); 
      for ( bj = bi+1 ; bj <= maxquants ; bj++ ) /* other products */
      { int fixj = GEN_QUANT(bj)->fixnum;
        REAL tmp;
        if ( fixj < 0 ) continue;
        tmp = dot(vgev[bi],vgef[bj],NV);
         if ( sparse_constraints_flag )
         {  if ( fixi < fixj ) sp_hash_search(&LS,fixi,fixj,tmp);
            else sp_hash_search(&LS,fixj,fixi,tmp);
         }
         else
         { rleftside[fixi][fixj] += tmp;
           rleftside[fixj][fixi] += tmp;
         }
       }
     }
     degfree += NV;
  }  /* end approx_curve_flag */

  /* variable_parameter contributions */
  if ( optparamcount )
  for ( i = 0 ; i < gen_quant_count ; i++ )
  { int fixi = GEN_QUANT(i)->fixnum;
    int voli = GEN_QUANT(i)->vol_number;
    if ( fixi < 0 ) continue;
    for ( j = 0 ; j < gen_quant_count ; j++ )
    { int fixj = GEN_QUANT(j)->fixnum;
      int volj = GEN_QUANT(j)->vol_number;
      if ( fixj < 0 ) continue;
      for ( k = 0 ; k < optparamcount ; k++ )
      { REAL tmp = optparam_congrads[k][voli]
            *globals(optparam[k].pnum)->attr.varstuff.pscale*optparam_congrads[k][volj];
        if ( sparse_constraints_flag )
        {  if ( fixi < fixj ) 
             sp_hash_search(&LS,fixi,fixj,tmp);
           else if ( fixi == fixj )
           { sp_hash_search(&LS,fixi,fixi,tmp); }
        }
        else rleftside[fixi][fixj] += tmp;
      }
    }
  }
  degfree += optparamcount;
  
  #ifndef MPI_EVOLVER
  if ( degfree < fixcount ) 
  { sprintf(errmsg,
       "Degrees of freedom, %d, is less than number of constraints, %d\n",
       degfree,fixcount);
    if ( degfree == 0 )
      strcat(errmsg,"Perhaps constraint is not applied to any elements?\n");
    kb_error(3004,errmsg,RECOVERABLE);
  }
  #endif
  
  /* solve for coefficients */
  if ( sparse_constraints_flag )
  { REAL old_hessian_epsilon = hessian_epsilon;
    calc_leftside_hash_count = sp_hash_end(&LS,fixcount,fixcount,A_OFF);
    LS.N = fixcount; 
    hessian_epsilon = 0.0;
    ysmp_factor(&LS);  /* since mindeg uses vertex info at the moment */
    hessian_epsilon = old_hessian_epsilon;
  }
  else
  { for ( k = 0 ; k < fixcount ; k++ )
      if ( rleftside[k][k] == 0.0 ) 
        rleftside[k][k] = 1.0; /* for invertibility */
    LD_factor(rleftside,fixcount);   
  }

} /* end calc_leftside() */

/*************************************************************************
*
* function: local_calc_leftside()
*
* purpose: Do local dot products of constraint gradients
*/

void local_calc_leftside()
{ vertex_id v_id;
  int bi,bj;
  
  /* generate  DV^T DV */
  if ( !approx_curve_flag )
    FOR_ALL_VERTICES(v_id)
    {
      volgrad *vgptri,*vgptrj;
      ATTR attr = get_vattr(v_id);

      if ( attr & FIXED ) continue;

      for ( vgptri = get_vertex_vgrad(v_id); vgptri ; vgptri = vgptri->chain )
      { REAL tmp;
        bi = vgptri->fixnum;
        if ( (bi < 0) || (bi >= fixcount) ) continue;
        degfree++;
        tmp = SDIM_dot(vgptri->velocity,vgptri->grad);
         
 /*      if ( !(attr & HIT_WALL) )    */
        { if ( sparse_constraints_flag )
          { sp_hash_search(&LS,bi,bi,tmp); }
          else
            rleftside[bi][bi] += tmp;
        }
        for ( vgptrj = vgptri->chain ; vgptrj ; vgptrj = vgptrj->chain )
        { tmp = SDIM_dot(vgptri->grad,vgptrj->velocity);
          bj = vgptrj->fixnum;
          if ( (bj < 0) || (bj >= fixcount) ) continue;
/*        if ( !(attr & HIT_WALL) ) */
          {
            if ( sparse_constraints_flag )
            { if ( bi < bj ) sp_hash_search(&LS,bi,bj,tmp);
              else sp_hash_search(&LS,bj,bi,tmp);
            }
            else
            { rleftside[bi][bj] += tmp;
              rleftside[bj][bi] += tmp;
            }
          }
        }
      }
    }
}

/************************************************************************
*
* function: calc_lagrange()
*
* purpose: calculate Lagrange multipliers
*
*/

void calc_lagrange()
{ int i,k;
  body_id b_id;
  struct gen_quant *gq;

  if ( fixcount == 0 ) return;

  if ( itdebug ) 
    outstring("calc_lagrange(): find Lagrange multipliers\n");

  calc_leftside();  /* set up rleftside in factored form */

  rightside = (REAL*)temp_calloc(maxquants,sizeof(REAL));
  vpressures = (REAL*)temp_calloc(maxquants,sizeof(REAL));

  #ifdef MPI_EVOLVER
  mpi_calc_rightside(maxquants);
  #else
  local_calc_rightside();
  #endif

  /* optimizing_parameter contributions */
  if ( optparamcount )
  for ( i = 0 ; i < gen_quant_count ; i++ )
  { int fixi = GEN_QUANT(i)->fixnum;
    int voli = GEN_QUANT(i)->vol_number;
    if ( fixi < 0 ) continue;
    for ( k = 0 ; k < optparamcount ; k++ )
      rightside[fixi] -= optparam_congrads[k][voli]*optparam[k].velocity;
      /* negative since using grad, not force */ 
  }

  /* solve for coefficients */
  if ( sparse_constraints_flag )
  {ysmp_solve(&LS,rightside,vpressures);
    free_system(&LS);
  }
  else  
    LD_solve(rleftside,rightside,vpressures,fixcount);

  /* check for singular matrix */
  for ( i = 0 ; i < fixcount ; i++ )
  { if ( !is_finite(vpressures[i]) )
    {
#ifdef used_calc_one_sided_grads      
      if ( i >= one_sided_start_fixcount )
      { int j = i-one_sided_start_fixcount;
        element_id v_id = one_sided_fixcount_list[j].v_id;
        int connum = one_sided_fixcount_list[j].connum;
        struct constraint *con = get_constraint(connum);  
        sprintf(errmsg,"Singular one-sided constraint matrix, vertex %s, constraint %s.\n",
           ELNAME(v_id),con->name);
      }
      else
#endif
        sprintf(errmsg,"Constraint adjustment matrix singular. \nMore constraints than degrees of freedom?\nConstraints with no elements?");
      kb_error(3005,errmsg,RECOVERABLE);
    }
  } 

  

  /* install pressures into body structures */
  if ( !web.pressure_flag )
    MFOR_ALL_BODIES(b_id)
    { if ( get_battr(b_id) & FIXEDVOL )
      { 
        if ( everything_quantities_flag )
        { int fixi;
          gq = GEN_QUANT(get_body_volquant(b_id));
          fixi = gq->fixnum;
          set_body_pressure(b_id,(fixi<0)?0.0 : -vpressures[fixi]);
        }
        else
        { int fixi = get_body_fixnum(b_id);
          set_body_pressure(b_id,(fixi<0)?0.0:-vpressures[fixi]);
        }
      }
    }
    for ( k = 0 ; k < gen_quant_count ; k++ )
    { gq = GEN_QUANT(k);
      if ( gq->flags & (Q_FIXED|Q_CONSERVED) )
        if ( !valid_id(gq->b_id) || !web.pressure_flag )
          gq->pressure = -vpressures[gq->fixnum];
    }

#ifdef ZZZZZZZZ
    /* Check for loosening, and record */
    /* one-sided constraint lagrange multipliers, if user wants */
    { int one_sided_lagrange_attr = 
            find_attribute(VERTEX,ONE_SIDED_LAGRANGE_ATTR_NAME);
      volgrad *vgptri;

      for ( k = one_sided_start_fixcount, j = 0 ; k < fixcount ; k++,j++ )
      { element_id v_id = one_sided_fixcount_list[j].v_id;
        int connum = one_sided_fixcount_list[j].connum;
        struct constraint *con = get_constraint(connum);
        if ( ((con->attr & NONNEGATIVE) && (vpressures[k] > 0))
             || ((con->attr & NONPOSITIVE) && (vpressures[k] < 0)) )
        { unset_v_constraint_status(v_id,connum);
          /* also have to kill vgrad so velocity not chopped */
          for ( vgptri = get_vertex_vgrad(v_id); vgptri ; vgptri = vgptri->chain )
            if ( vgptri->fixnum == k )
                vgptri->fixnum = -1;
        }

        if ( one_sided_lagrange_attr >= 0 )
        { REAL *v = (REAL*)get_extra(v_id,one_sided_lagrange_attr);
          for ( i = 0 ; i < osl_size ; i++ )
            if ( v[i] == 0.0 )  
            { v[i] = vpressures[k];       
              break;
            }
        }
      }
    }
#endif

} /* end calc_lagrange() */

/**************************************************************************
*
* function: local_calc_rightside()
*
* purpose: local part of calculation of Lagrange multiplier right side.
*/
void local_calc_rightside()
{ int bi;
  vertex_id v_id;

  if ( itdebug ) 
    outstring("calc_rightside(): for finding Lagrange multipliers\n");
  /* generate right side of matrix equation */
  if ( !approx_curve_flag )
    FOR_ALL_VERTICES(v_id)
    {
      volgrad *vgptri;
      ATTR attr = get_vattr(v_id);
      REAL *f;

      if ( attr & FIXED ) continue;
      f = get_velocity(v_id);

      for ( vgptri = get_vertex_vgrad(v_id); vgptri ; vgptri = vgptri->chain )
      { 
        bi = vgptri->fixnum;
        if ( (bi < 0) || (bi >= fixcount) ) continue;
        if (valid_id(vgptri->bb_id) && !web.pressure_flag && !everything_quantities_flag )
        { if ( (id_type(vgptri->bb_id) == BODY) && !(get_battr(vgptri->bb_id)&FIXEDVOL) ) 
             continue;
        }
        rightside[bi] += SDIM_dot(f,vgptri->grad);
      }
    }

  if ( approx_curve_flag )
  { REAL *f;

    /* calculate right side */
    FOR_ALL_VERTICES(v_id)
     {
        volgrad *vgptri;
        ATTR attr = get_vattr(v_id);

        if ( attr & FIXED ) continue;

        f = get_velocity(v_id);
        for ( vgptri=get_vertex_vgrad(v_id); vgptri ; vgptri = vgptri->chain )
        {
          bi = vgptri->fixnum;
          if ( (bi < 0) || (bi >= fixcount) ) continue;
          if (valid_id(vgptri->bb_id) &&!web.pressure_flag &&  !everything_quantities_flag )
            { if ( !(get_battr(vgptri->bb_id)&FIXEDVOL) ) continue;
            }
          rightside[bi] += SDIM_dot(f,&vgef[bi][SDIM*loc_ordinal(v_id)]);
        }
      }
  }  /* end approx_curve_flag */
}

/*************************************************************************
*
* function: lagrange_adjust()
*
* purpose: wrapper for local_lagrange_adjust()
*/
void lagrange_adjust()
{ int i,k;

  if ( !fixed_constraint_flag ) return;

  if ( itdebug ) 
    outstring("lagrange_adjust(): subtract multiples of volume gradients from force\n");

  #ifdef MPI_EVOLVER
  mpi_lagrange_adjust(maxquants);
  #else
  local_lagrange_adjust();
  #endif

  /* optimizing parameter adjust */
  if ( optparamcount )
    for ( i = 0 ; i < optparamcount ; i++ )
      for ( k = 0 ; k < gen_quant_count ; k++ )
      { int fixi = GEN_QUANT(k)->fixnum;
        int volk = GEN_QUANT(k)->vol_number;
        if ( fixi >= 0 )
          optparam[i].velocity += vpressures[fixi]*
              globals(optparam[i].pnum)->attr.varstuff.pscale*
                 optparam_congrads[i][volk];
      }

} /* end lagrange_adjust() */

/*********************************************************************
*
* function: local_lagrange_adjust()
*
* purpose: adjust forces by Lagrange multipliers of constraint grads.
*
* return value: number of vertices loosened from one-sided constraints.
*
*/

int local_lagrange_adjust()
{ vertex_id v_id;
  int k;
  int loosened = 0;
  
  one_sided_lagrange_attr = find_attribute(VERTEX,ONE_SIDED_LAGRANGE_ATTR_NAME);
  /* clear one_sided_lagrange */
  if ( one_sided_lagrange_attr >= 0 )
  { struct extra *ex = EXTRAS(VERTEX)+one_sided_lagrange_attr;
    vertex_id v_id;
    int i;
    int osl_size = ex->array_spec.datacount;

    FOR_ALL_VERTICES(v_id)
    { REAL *v = (REAL*)get_extra(v_id,one_sided_lagrange_attr);
      for ( i = 0 ; i < osl_size ; i++ )
        v[i] = 0.0;
    }
  }

  /* subtract multiples of volume gradients from force */

  FOR_ALL_VERTICES(v_id)
  {
    volgrad *vgptr;
    int ord = loc_ordinal(v_id);
    REAL *f;
    int bi; /* row number for constraint */

    if ( get_vattr(v_id) & FIXED ) continue;
    f = get_velocity(v_id);
    for ( vgptr = get_vertex_vgrad(v_id) ; vgptr ; vgptr = vgptr->chain )
    { bi = vgptr->fixnum;
      if ( (bi < 0) || (bi >= fixcount) ) 
          continue;  /* bi = -1 for killed one-sided constraints */

      if ( approx_curve_flag )
         for ( k = 0 ; k < SDIM ; k++ )
            f[k] -= vpressures[bi]*vgev[bi][SDIM*ord +k];
      else
         for ( k = 0 ; k < SDIM ; k++ )
            f[k] -= vpressures[bi]*vgptr->velocity[k];
    }

    if ( one_sided_present )
      loosened += one_sided_lagrange_adjust(v_id);
  }

  return loosened;
} /* end local_lagrange_adjust() */
        
/******************************************************************
* 
* function: one_sided_lagrange_adjust()
*
* purpose; Find Lagrange multipliers for one-sided constraints
*          for a vertex.  Callde from local_lagrange_adjust().
*
* return value; 1 if vertex loosened from a one-sided constraint.
*/
int one_sided_lagrange_adjust(v_id)
vertex_id v_id;
{
    REAL *raw_velocity = VREAL(v_id,raw_velocity_attr);
    volgrad *vgptr;
    int bi; /* row number for constraint */

     conmap_t * conmap = get_v_constraint_map(v_id);
     int oncount = 0;
     int one_sided_count = 0;
     struct constraint *con[MAXCONPER];
     int conlist[MAXCONPER];
     MAT2D(grads,MAXCOORD,MAXCOORD);
     MAT2D(ss,MAXCOORD,MAXCOORD);
     REAL lagmul[MAXCOORD];
     REAL *coord;
     int i,j,k,n;
     REAL value;
     int loosened = 0;

     for ( j = 1 ; j <= (int)conmap[0] ; j++ )
     {
       if ( conmap[j] & CON_HIT_BIT )
       { conlist[oncount] = conmap[j] & CONMASK;
         con[oncount] = get_constraint(conmap[j]);
         if ( con[oncount]->attr & (NONNEGATIVE|NONPOSITIVE) )
           one_sided_count++;
         oncount++;
       }
     }
    
     if ( one_sided_count == 0 ) 
       return 0; /* nothing to do here */

    /* adjust raw velocity for global constraints */
    for ( vgptr = get_vertex_vgrad(v_id) ; vgptr ; vgptr = vgptr->chain )
    { bi = vgptr->fixnum;
      if ( (bi < 0) || (bi >= fixcount) ) 
          continue;  /* bi = -1 for killed one-sided constraints */

       for ( k = 0 ; k < SDIM ; k++ )
         raw_velocity[k] -= vpressures[bi]*vgptr->raw_velocity[k];
    }

    /* get local constraint gradients */
    coord = get_coord(v_id);
    for ( i = 0 ; i < oncount ; i++ )
      eval_all(con[i]->formula,coord,SDIM,&value,grads[i],v_id);
    matvec_mul(grads,raw_velocity,lagmul,oncount,SDIM);
    mat_mul_tr(grads,grads,ss,oncount,SDIM,oncount);
    mat_approx_solve(ss,oncount,lagmul);

    /* now inspect lagrange multipliers */
     for ( n = 0 ; n < oncount ;  )
     {
         if ( one_sided_lagrange_attr >= 0 )
         { REAL *v = (REAL*)get_extra(v_id,one_sided_lagrange_attr);
           struct extra *ex = EXTRAS(VERTEX)+one_sided_lagrange_attr;
           int osl_size = ex->array_spec.datacount;
           for ( i = 0 ; i < osl_size ; i++ )
             if ( v[i] == 0.0 )  
             { v[i] = lagmul[n];    /* sign so acts like pressure */  
               break;
             }
         }

         if ( ((con[n]->attr & NONPOSITIVE) && ( lagmul[n] < 0.0 )) ||
              ((con[n]->attr & NONNEGATIVE) && ( lagmul[n] > 0.0 )) )         
           { /* loosen */
             unset_v_constraint_status(v_id,conlist[n]);
             oncount--; /* replace with last in list */
             conlist[n] = conlist[oncount];
             con[n] = con[oncount];
             lagmul[n] = lagmul[oncount];
             loosened++;
             continue; /* don't increment n */
           }
 
         n++;
     }


     if ( loosened )
     { /* recalculate projected velocity */
       REAL perp[MAXCOORD];
       REAL *velocity = get_velocity(v_id);
       constr_proj(TANGPROJ,oncount,con,coord,raw_velocity,perp,NULL,NO_DETECT,v_id);
       for ( j = 0 ; j < SDIM ; j++ )
          velocity[j] = raw_velocity[j] - perp[j]; 
     }

  return loosened;
}

/*******************************************************************
*
* function: volume_restore()
*
* purpose: adjust surface to global constraints.
*         Assumes quantities and gradients already calculated.
*
*/

void volume_restore ARGS2((stepsize,mode),
REAL stepsize, /* multiplier for motion; usually 1 */
int mode)/* TEST_MOVE or ACTUAL_MOVE */
{ body_id bi_id;
  struct gen_quant *gq;
  int i,k;
  int fixi;

  if ( itdebug ) 
    outstring("volume_restore()\n");

  if ( !fixed_constraint_flag ) return;

  vol_deficit = (REAL*)temp_calloc(maxquants,sizeof(REAL));
  vol_restore = (REAL*)temp_calloc(maxquants,sizeof(REAL));

  /* gather differences from targets */
  if ( !web.pressure_flag && !everything_quantities_flag )
    FOR_ALL_BODIES(bi_id)
     {
       if ( !(get_battr(bi_id) & FIXEDVOL) ) continue;
       fixi = get_body_fixnum(bi_id);
       if ( fixi < 0 ) continue;
       vol_deficit[fixi] = get_body_fixvol(bi_id) - get_body_volume(bi_id);
     }
  for ( k = 0 ; k < gen_quant_count ; k++ )
  { gq = GEN_QUANT(k);
    if ( !(gq->flags & Q_FIXED) ) continue;
    if ( valid_id(gq->b_id) && web.pressure_flag ) continue;
    fixi = gq->fixnum;
    if ( fixi < 0 ) continue;
    vol_deficit[fixi] = gq->target - gq->value;
  }

  /* solve for volume restoration coefficients */
  if ( sparse_constraints_flag )
  { if ( LS.A == NULL ) calc_leftside(); 
    ysmp_solve(&LS,vol_deficit,vol_restore);
    free_system(&LS);
  }
  else
  { if ( rleftside == NULL ) calc_leftside();
    LD_solve(rleftside,vol_deficit,vol_restore,fixcount);
  }
  
  /* subtract multiples of volume gradients from force */
  /* and combine multiples of gradients for restoring motion */

  /* set restoring motion */

  /* optimizing parameter adjust */
  if ( optparamcount )
    for ( i = 0 ; i < optparamcount ; i++ )
     for ( k = 0 ; k < gen_quant_count ; k++ )
     { int fixk = GEN_QUANT(k)->fixnum;
       int volk = GEN_QUANT(k)->vol_number;
       if ( fixk >= 0 )
          globals(optparam[i].pnum)->value.real += vol_restore[fixk]
            *globals(optparam[i].pnum)->attr.varstuff.pscale*optparam_congrads[i][volk];
     }

  #ifdef MPI_EVOLVER
  mpi_volume_restore(maxquants,stepsize,mode);
  #else
  local_volume_restore(stepsize,mode);
  #endif

  partner_move();  /* in case doing partners */
  
  if ( vol_deficit ) temp_free((char *)vol_deficit);  vol_deficit = NULL;
  if ( vol_restore ) temp_free((char *)vol_restore);  vol_restore = NULL;

} /* end volume_restore() */

  
/***************************************************************************
*
* function: local_volume_restore()
*
* purpose: apply volume restoration to local part of surface
*/
void local_volume_restore(stepsize,mode)
REAL stepsize;
int mode;
{ vertex_id v_id;
  int bi,i,k;
  
  /* vertices */
  FOR_ALL_VERTICES(v_id)
  { REAL *x;
    volgrad *vgptr;
    int ord = loc_ordinal(v_id);
    int attr = get_vattr(v_id);

	  if ( (attr & CONSTRAINT) && one_sided_present )
      one_sided_volume_adjust(v_id);

    x = get_coord(v_id);
    if ( attr & BOUNDARY )
    { REAL *p = get_param(v_id);
      struct boundary *boundary = get_boundary(v_id);
      if ( !(attr & FIXED) ) 
        for ( vgptr = get_vertex_vgrad(v_id) ; vgptr ; vgptr = vgptr->chain )
        { bi = vgptr->fixnum;
          if ( ( bi < 0 ) || (bi >= fixcount) ) continue;
          if ( approx_curve_flag )
            for ( k = 0 ; k < SDIM ; k++ )
              p[k] += stepsize*vol_restore[bi]*vgev[bi][SDIM*ord+k];
          else
            for ( k = 0 ; k < boundary->pcount ; k++ )
              p[k] += stepsize*vol_restore[bi]*vgptr->velocity[k];
        }
      for ( i = 0 ; i < SDIM ; i++ )
        x[i] = eval(boundary->coordf[i],p,v_id,NULL);

    }
    else
    { 
      if ( !(attr & FIXED) ) 
      for ( vgptr = get_vertex_vgrad(v_id) ; vgptr ; vgptr = vgptr->chain )
      { bi = vgptr->fixnum;
        if ( ( bi < 0 ) || (bi >= fixcount) ) continue;
        if ( approx_curve_flag )
          for ( k = 0 ; k < SDIM ; k++ )
             x[k] += stepsize*vol_restore[bi]*vgev[bi][SDIM*ord+k];
        else
        { for ( k = 0 ; k < SDIM ; k++ )
             x[k] += stepsize*vol_restore[bi]*vgptr->velocity[k];
        }
           
      }
      if ( attr & CONSTRAINT ) project_v_constr(v_id,mode,KEEP_ONESIDEDNESS);
    }
  }
}

/*******************************************************************
* function: one_sided_volume_adjust()
*
* purpose: to see if volume correction velocity at a vertex
*          will detach it from one-sided constraints.  If so,
*          raw volume adjust velocities are re-projected.
*/

void one_sided_volume_adjust ARGS1((v_id),
vertex_id v_id)
{
    REAL raw_velocity[MAXCOORD];
    volgrad *vgptr;
    int bi; /* row number for constraint */

     conmap_t * conmap = get_v_constraint_map(v_id);
     int oncount = 0;
     int one_sided_count = 0;
     struct constraint *con[MAXCONPER];
     int conlist[MAXCONPER];
     MAT2D(grads,MAXCOORD,MAXCOORD);
     MAT2D(ss,MAXCOORD,MAXCOORD);
     REAL lagmul[MAXCOORD];
     REAL *coord;
     int i,j,k,n;
     REAL value;
     int loosened = 0;

     for ( j = 1 ; j <= (int)conmap[0] ; j++ )
     {
       if ( conmap[j] & CON_HIT_BIT )
       { conlist[oncount] = conmap[j] & CONMASK;
         con[oncount] = get_constraint(conmap[j]);
         if ( con[oncount]->attr & (NONNEGATIVE|NONPOSITIVE) )
           one_sided_count++;
         oncount++;
       }
     }
    
     if ( one_sided_count == 0 ) 
       return; /* nothing to do here */

    /* form volume adjust net velocity */
    for ( i = 0 ; i < SDIM ; i++ )
      raw_velocity[i] = 0.0;
    for ( vgptr = get_vertex_vgrad(v_id) ; vgptr ; vgptr = vgptr->chain )
    { bi = vgptr->fixnum;
      if ( (bi < 0) || (bi >= fixcount) ) 
          continue;  /* bi = -1 for killed one-sided constraints */

       for ( k = 0 ; k < SDIM ; k++ )
         raw_velocity[k] += vol_restore[bi]*vgptr->raw_velocity[k];
    }

    /* get local constraint gradients */
    coord = get_coord(v_id);
    for ( i = 0 ; i < oncount ; i++ )
      eval_all(con[i]->formula,coord,SDIM,&value,grads[i],v_id);
    matvec_mul(grads,raw_velocity,lagmul,oncount,SDIM);
    mat_mul_tr(grads,grads,ss,oncount,SDIM,oncount);
    mat_approx_solve(ss,oncount,lagmul);

    /* now inspect lagrange multipliers */
     for ( n = 0 ; n < oncount ;  )
     {
         if ( ((con[n]->attr & NONPOSITIVE) && ( lagmul[n] < 0.0 )) ||
              ((con[n]->attr & NONNEGATIVE) && ( lagmul[n] > 0.0 )) )         
           { /* loosen */
             unset_v_constraint_status(v_id,conlist[n]);
             oncount--; /* replace with last in list */
             conlist[n] = conlist[oncount];
             con[n] = con[oncount];
             lagmul[n] = lagmul[oncount];
             loosened++;
             continue; /* don't increment n */
           }
 
         n++;
     }


     if ( loosened )
     { /* recalculate projected velocity */
       REAL perp[MAXCOORD];
       for ( vgptr = get_vertex_vgrad(v_id) ; vgptr ; vgptr = vgptr->chain )
       { constr_proj(TANGPROJ,oncount,con,coord,vgptr->raw_velocity,perp,NULL,NO_DETECT,v_id);
         for ( j = 0 ; j < SDIM ; j++ )
          vgptr->velocity[j] = vgptr->raw_velocity[j] - perp[j]; 
       }
     }

}

/*******************************************************************
*
*  function: vgrad_init()
*
*  purpose:  allocates storage for vertex volume gradients
*
*/

char *vgrad_attr_name = "__vgrad_head";
int vgrad_attr;

void vgrad_init(qfixed)
int qfixed; /* how many fixed quantities (aside from body volumes)*/
{ int one = 1;
  vertex_id v_id;
  int i;
  int stride;
  
  vgrad_end();  /* take care of any leftovers */
  
  /* allocate chain start for each vertex */
  vgrad_attr = find_attribute(VERTEX,vgrad_attr_name);
  if ( vgrad_attr < 0 )
    vgrad_attr = add_attribute(VERTEX,vgrad_attr_name,PTR_TYPE,0,&one,0,NULL);
  MFOR_ALL_VERTICES(v_id)
    VPTR(v_id,vgrad_attr)[0] = NULL;

  /* allocate initial block of structures, using info from last time
     around, plus a little margin for growth. 
  */
  vgradbase = (struct vgradblock *)temp_calloc(1,sizeof(struct vgradblock));
  vgradbase->max = vgradlastused ? (vgradlastused + vgradlastused/10) :
                     web.skel[VERTEX].count;
  vgradbase->base = (volgrad*)temp_calloc(vgradbase->max,sizeof(volgrad));
  stride = one_sided_present?3:2;
  vgradbase->values = (REAL*)temp_calloc(vgradbase->max,stride*SDIM*sizeof(REAL));
  for ( i = 0 ; i < vgradbase->max ; i++ )
  { vgradbase->base[i].grad = vgradbase->values + stride*i*SDIM;
    vgradbase->base[i].velocity = vgradbase->values + (stride*i+1)*SDIM;
    if ( one_sided_present )
      vgradbase->base[i].raw_velocity = vgradbase->values + (stride*i+2)*SDIM;
  }
  vgradbase->top = 0;
  vgradbase->next = NULL;
  vgradtop = 0;
  vgradmax = 0;
  
} /* end vgrad_init() */

/*********************************************************************
*
* function: vgrad_end()
*
* purpose: Deallocate constraint gradients structures.
*
*/
void vgrad_end()
{ struct vgradblock *block,*next;

  vgradlastused = vgradtop;
  for ( block = vgradbase ; block ; block = next )
  { temp_free((char *)block->base);
    temp_free((char *)block->values);
    next = block->next;
    temp_free((char *)block);
  }
  vgradbase = NULL; 
  
  vgradmax = 0;
  if ( sparse_constraints_flag )
  { free_system(&LS);
    memset(&LS,0,sizeof(LS));
  }
  else
    if ( rleftside ) free_matrix(rleftside);  rleftside = NULL;
  if ( rightside ) temp_free((char *)rightside);  rightside = NULL;
  if ( vpressures ) temp_free((char *)vpressures); vpressures = NULL;
  if ( optparam_congrads ) free_matrix(optparam_congrads);
  optparam_congrads = NULL;

  #ifdef MPI_EVOLVER
  if ( this_task == 0 )
    mpi_vgrad_end(); 
  #endif

} /* end vgrad_end() */

/********************************************************************
*
* function: get_vertex_vgrad()
*
* purpose: return pointer to first vgrad structure in vertex's chain
*/

volgrad *get_vertex_vgrad(v_id)
vertex_id v_id;
{ if ( !vgradbase ) return NULL;
  return (volgrad*)VPTR(v_id,vgrad_attr)[0];
}

/********************************************************************
*
* function: set_vertex_vgrad()
*
* purpose: set pointer to first vgrad structure in vertex's chain
*/

void set_vertex_vgrad(v_id,vgptr)
vertex_id v_id;
volgrad *vgptr;
{
  VPTR(v_id,vgrad_attr)[0] = (char*)vgptr;
}

/**********************************************************************
*
* function: new_vgrad()
*
* purpose: Allocate a new vgrad structure from pool. Returns index.
*          Allocates more space if necessary; possibility of reallocation
*          is the reason for using an index instead of a pointer.
*
*/

struct volgrad * new_vgrad()
{ struct volgrad * vg;
  struct vgradblock *newblock;

LOCK_WEB;
  if ( vgradbase->top >= vgradbase->max ) 
  { int i,stride;
    /* need new block */
    newblock = (struct vgradblock *)temp_calloc(1,sizeof(struct vgradblock));
    newblock->max = vgradtop/3+100;
    newblock->base = (volgrad*)temp_calloc(newblock->max,sizeof(volgrad));
    stride = one_sided_present ? 3 : 2;
    newblock->values = (REAL*)temp_calloc(newblock->max,stride*SDIM*sizeof(REAL));
    for ( i = 0 ; i < newblock->max ; i++ )
    { newblock->base[i].grad = newblock->values + stride*i*SDIM;
      newblock->base[i].velocity = newblock->values + (stride*i+1)*SDIM;
      if ( one_sided_present )
        newblock->base[i].raw_velocity = newblock->values + (stride*i+2)*SDIM;
    }
    newblock->top = 0;
    newblock->next = vgradbase;
    vgradbase = newblock;
  }

  vg = vgradbase->base + vgradbase->top++;
  vgradtop++;
UNLOCK_WEB;

  return vg;
}

/******************************************************************
*
* function: get_bv_vgrad()
*
* purpose: return pointer to vgrad structure of given body at
*         given vertex. NULL if none.
*/
    
volgrad *get_bv_vgrad(fixnum,v_id)
int fixnum; /* which constrained quantity */
vertex_id v_id;
{
  volgrad  *vgptr;

  vgptr = get_vertex_vgrad(v_id);
  while ( vgptr )
    if ( vgptr->fixnum == fixnum ) break;
    else vgptr = vgptr->chain;

  return vgptr;    /* null if not found */
}
    
/******************************************************************
*
* function: get_bv_new_vgrad()
*
* purpose: return pointer to vgrad structure of given body at
*         given vertex. Allocates if none.
*/

volgrad *get_bv_new_vgrad(fixnum,v_id)
int fixnum;
vertex_id v_id;
{
  volgrad  **ptrptr;  /* pointer to pointer so can update if need be */

  ptrptr = (volgrad**)VPTR(v_id,vgrad_attr);

  while ( *ptrptr )
    if ( (*ptrptr)->fixnum == fixnum ) return *ptrptr;
    else ptrptr = &(*ptrptr)->chain;

  /* need to get new structure */
  *ptrptr = new_vgrad();
  (*ptrptr)->fixnum = fixnum;

  return *ptrptr;
}


/**********************************************************************
*
*  function: approx_curv_calc()
*
*  purpose: converts energy gradients to approximate curvature
*
*/

void approx_curv_calc(mode)
int mode; /* bits for CALC_FORCE and CALC_VOLGRADS */
{
  int j;
  REAL *B;
  vertex_id v_id;

  if ( mode & CALC_FORCE )
  { B  = (REAL *)temp_calloc(SDIM*(web.skel[VERTEX].max_ord+1),
             sizeof(REAL));  

    /* each coordinate gives a right side */
    FOR_ALL_VERTICES(v_id)
      { 
        int vnum = SDIM*loc_ordinal(v_id);
        for ( j = 0 ; j < SDIM ; j++ )
           B[vnum+j] = get_force(v_id)[j];
      }
    mobility_mult(B);
    FOR_ALL_VERTICES(v_id)
      { REAL *vel = get_velocity(v_id);
        int vnum = SDIM*loc_ordinal(v_id);
        for ( j = 0 ; j < SDIM ; j++ )
          vel[j] = B[vnum+j];
      }

     temp_free((char *)B);
  }

  if ( mode & CALC_VOLGRADS )
  /* constraint gradients */
  { int NV = SDIM*(1+web.skel[VERTEX].max_ord);
    int bi;

    vgef = dmatrix(0,maxquants+1,0,NV);
    vgev = dmatrix(0,maxquants+1,0,NV);
    /* load volume gradients */
    FOR_ALL_VERTICES(v_id)
     {
        volgrad *vgptri;
        ATTR attr = get_vattr(v_id);
        int ord = loc_ordinal(v_id);

        if ( attr & FIXED ) continue;

        for ( vgptri=get_vertex_vgrad(v_id); vgptri ; vgptri = vgptri->chain )
        { bi = vgptri->fixnum;
          if (valid_id(vgptri->bb_id) && !web.pressure_flag && !everything_quantities_flag )
            { if ( !(get_battr(vgptri->bb_id)&FIXEDVOL) ) continue;
            }
          for ( j = 0 ; j < SDIM ; j++ )
            vgef[bi][SDIM*ord+j] = vgptri->grad[j];
        }
     }

      /* convert gradients to vectors */
     for ( bi = 0 ; bi <= maxquants ; bi++ )
     { memcpy((char*)vgev[bi],(char*)vgef[bi],NV*sizeof(REAL));
       mobility_mult(vgev[bi]); /* to vector */
     }

  }  

} /* end approx_curv_calc() */



  
  
