/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/****************************************************************
*
*  File: cnstrnt.c
*
*  Contents:  Functions dealing with boundary constraints.
*/

#include "include.h"

/*******************************************************************
*
*  function: constraint_init()
*
*  purpose:  allocate expnode structures for a constraint
*
*/

void constraint_init(con)
struct constraint *con;
{ int i;
  con->formula = (struct expnode *)mycalloc(1+2*MAXCONCOMP,
      sizeof(struct expnode));

  for ( i = 0 ; i < MAXCONCOMP ; i++ )
  { con->envect[i] = con->formula + 1 + i;
    con->convect[i] = con->formula + 1 + MAXCONCOMP + i;
  }
}

/*******************************************************************
*
*  function: constraint_free()
*
*  purpose:  free expnode structures and expressions for a constraint
*
*/

void constraint_free(con)
struct constraint *con;
{ int j;

  if ( con->formula )
  { for ( j = 0 ; j < MAXCONCOMP ; j++ )
    { free_expr(con->envect[j]);
      free_expr(con->convect[j]);
    }
    free_expr(con->formula);
    myfree((char*)con->formula);
  }
  
}

/******************************************************************
*
*  Function: constr_proj()
*
*  Purpose:  projection on level-set constraints.
*
*  Explanation:  Projecting a point or vector onto constraints
*      requires finding an appropriate linear combination of
*      constraint gradients c[i]*grad(f[i]) where c[i] = (inv A)*B,
*      where B is an appropriate vector (input) and 
*                A[i][j] = <grad(f[i]),grad(f[j])>.
*      The linear combination is returned.
*      Function return value is number of hit constraints. 
*/


int constr_proj(mode,concount,constr,coord,b,combo,conlist,detect_flag,v_id)
int mode;  /* TANGPROJ for vector projection of b to tangent plane,
                        (note return is normal component of b!)
              PLAINPROJ for using input b as right side 
                        (note return is offset from constraints!)*/
int concount;  /* how many constraints */
struct constraint **constr;  /* array of pointers to constraints */
REAL *coord;  /* coordinates of point for evaluating functions */
REAL *b;        /* vector to project */
REAL *combo;  /* desired linear combination */
int *conlist;  /* list of constraint numbers; for returning hits */
int detect_flag; /* set if want to detect one-sided constraints */
vertex_id v_id; /* vertex for DETECT mode, or passing on to constraint eval */
{
/* maximum number of hit constraints */
#define MAXCONSTR MAXCOORD
  int i,j;
  REAL grad[MAXCONSTR][MAXCOORD];     /* gradients of constraints */
  REAL c[MAXCONSTR];                  /* combination coefficients */
  REAL r[MAXCONSTR];                  /* constructed right side B */
  REAL fval;                          /* value of constraint      */
  REAL *bb;                           /* pointer to right side    */
  MAT2D(a,MAXCONSTR,MAXCONSTR);       /* matrix for projection    */

  if ( concount > MAXCONSTR ) 
  { sprintf(errmsg,
     "Trying to project vertex %s on more constraints, %d, than allowed, %d.",
     ELNAME(v_id),concount,MAXCONSTR);  
     if ( conlist )
     { sprintf(errmsg+strlen(errmsg),"    Projecting on constraints ");
        for ( i = 0 ; i < concount ; i++ )
        sprintf(errmsg+strlen(errmsg)," %s",get_constraint(conlist[i])->name);
        strcat(errmsg,".  Maybe redundant one-sided constraints?");
     }
     strcat(errmsg,"\n");
     kb_error(1791,errmsg,RECOVERABLE);
  }

  if ( concount <= 0 ) 
  { for ( j = 0 ; j < SDIM ; j++ )
       combo[j] = 0.0;     
    return concount;
  }

  /* calculate gradients */
  for ( i = 0 ; i < concount ; i++ )     
      eval_all(constr[i]->formula,coord,SDIM,&fval,grad[i],v_id);

  /* maybe construct right side for vector projection */
  if ( mode == TANGPROJ )
  { for ( i = 0 ; i < concount ; i++ )
      r[i] = SDIM_dot(b,grad[i]);
    bb = r;
  }
  else bb = b;

  /* construct matrix A */  
  for ( i = 0 ; i < concount ; i++ )     
     for ( j = 0 ; j < concount ; j++ )
        a[i][j] = SDIM_dot(grad[i],grad[j]);

#define BBOLDWAY
#ifdef OLDWAY
  /* invert */
  mat_inv(a,concount);

  /* combination coefficients */
  matvec_mul(a,bb,c,concount>SDIM?SDIM:concount,concount>SDIM?SDIM:concount);
#else

   for ( i = 0 ; i < concount ; i++ )
     c[i] = bb[i];
   mat_approx_solve(a,concount,c);
#endif

  /* form combination */
  for ( i = 0 ; i < SDIM ; i++ )
  { combo[i] = 0.0;
    for ( j = 0 ; j < concount ; j++ )
      combo[i] += c[j]*grad[j][i];
  }
  
  return concount;

} /* end constr_proj() */


/******************************************************************
*
*  Function: constr_proj_matrix()
*
*  Purpose:  Find matrix for projection onto constraint tangents.
*
*  Explanation:  Projecting a point or vector onto constraints
*      requires finding an appropriate linear combination of
*      constraint gradients c[i]*grad(f[i]) where c[i] = (inv A)*B,
*      where B is an appropriate vector (input) and 
*                A[i][j] = <grad(f[i]),grad(f[j])>.
*/


int constr_proj_matrix(v_id,mat)
vertex_id v_id; /* vertex for DETECT mode, or passing on to constraint eval */
REAL **mat;     /* projection matrix */
{
/* maximum number of hit constraints */
#define MAXCONSTR MAXCOORD
  int i,j;
  MAT2D(grad,MAXCONSTR,MAXCOORD);     /* gradients of constraints */
  REAL fval;                          /* value of constraint */
  int concount=0;
  conmap_t * conmap = get_v_constraint_map(v_id);
  struct constraint *constr[MAXCONSTR];
  for ( j = 1 ; j <= (int)conmap[0] ; j++ )
   if ( conmap[j] & CON_HIT_BIT )
     constr[concount++] = get_constraint(conmap[j]);

  if ( concount > MAXCONSTR ) 
  { sprintf(errmsg,
     "Vertex %s has more constraints, %d, than allowed, %d.",
        ELNAME(v_id),concount,SDIM);  
 
     sprintf(errmsg+strlen(errmsg),"    Projecting on constraints ");
     for ( i = 0 ; i < concount ; i++ )
        sprintf(errmsg+strlen(errmsg)," %s",constr[i]->name);
     strcat(errmsg,".  Maybe redundant one-sided constraints?");
     
     strcat(errmsg,"\n");
     kb_error(3515,errmsg,RECOVERABLE);
  }

  if ( concount > 0 )
  { REAL *coord = get_coord(v_id);
  
    /* calculate gradients */
    for ( i = 0 ; i < concount ; i++ )     
      eval_all(constr[i]->formula,coord,SDIM,&fval,grad[i],v_id);
  
    /* orthonormalize */
    concount = gram_schmidt(grad,concount,SDIM);

    /* form projection matrix */
    tr_mat_mul(grad,grad,mat,concount,SDIM,SDIM);
    for ( i = 0 ; i < SDIM ; i++ )
    { for ( j = 0 ; j < SDIM ; j++ )
        mat[i][j] *= -1;
      mat[i][i] += 1.0;
    }
  }
  /* else identity */
  else
  { for ( i = 0 ; i < SDIM ; i++ )
      for ( j = 0 ; j < SDIM ; j++ )
        mat[i][j] = (i==j) ? 1.0 : 0.0;
  }

  return concount;
} /* end constr_proj_matrix */

/******************************************************************
*
*  Function: constr_proj_matrix_wall()
*
*  Purpose:  Find matrix for projection onto constraint tangents,
*            but ignoring nonwall constraints.
*
*  Explanation:  Projecting a point or vector onto constraints
*      requires finding an appropriate linear combination of
*      constraint gradients c[i]*grad(f[i]) where c[i] = (inv A)*B,
*      where B is an appropriate vector (input) and 
*                A[i][j] = <grad(f[i]),grad(f[j])>.
*/


int constr_proj_matrix_wall(v_id,mat)
vertex_id v_id; /* vertex for DETECT mode, or passing on to constraint eval */
REAL **mat;  /* projection matrix */
{
/* maximum number of hit constraints */
#define MAXCONSTR MAXCOORD
  int i,j;
  MAT2D(grad,MAXCONSTR,MAXCOORD);     /* gradients of constraints */
  REAL fval;                                 /* value of constraint */
  int concount=0;
  conmap_t * conmap = get_v_constraint_map(v_id);
  struct constraint *constr[MAXCONSTR];
  for ( j = 1 ; j <= (int)conmap[0] ; j++ )
   if ( conmap[j] & CON_HIT_BIT )
   { struct constraint *con = get_constraint(conmap[j]);
     if ( !(con->attr & NONWALL) )
       constr[concount++] = con;
   }

  if ( concount > SDIM ) 
  { sprintf(errmsg,
     "Vertex %s has more constraints, %d, than space dimensions, %d.",
        ELNAME(v_id),concount,SDIM);  
 
     sprintf(errmsg+strlen(errmsg),"    Projecting on constraints ");
     for ( i = 0 ; i < concount ; i++ )
        sprintf(errmsg+strlen(errmsg)," %s",constr[i]->name);
     strcat(errmsg,".  Maybe redundant one-sided constraints?");
     
     strcat(errmsg,"\n");
     kb_error(3028,errmsg,RECOVERABLE);
  }

  if ( concount > 0 )
  { REAL *coord = get_coord(v_id);
  
    /* calculate gradients */
    for ( i = 0 ; i < concount ; i++ )     
      eval_all(constr[i]->formula,coord,SDIM,&fval,grad[i],v_id);
  
    /* orthonormalize */
    concount = gram_schmidt(grad,concount,SDIM);

    /* form projection matrix */
    tr_mat_mul(grad,grad,mat,concount,SDIM,SDIM);

    for ( i = 0 ; i < SDIM ; i++ )
    { for ( j = 0 ; j < SDIM ; j++ )
        mat[i][j] *= -1;
      mat[i][i] += 1.0;
    }
  }
  /* else identity */
  else
  { for ( i = 0 ; i < SDIM ; i++ )
      for ( j = 0 ; j < SDIM ; j++ )
        mat[i][j] = (i==j) ? 1.0 : 0.0;
  }

  return concount;
} /* end constr_proj_matrix */

/******************************************************************
*
*  Function:  project_v_constr()
*
*  Purpose:  project a vertex onto its constraints.
*  Return:    whether hitting a one-sided constraint
*                for the first time.
*/

int project_v_constr(v_id,mode,one_sided_reset)
vertex_id v_id;
int mode; /* TEST_MOVE or ACTUAL_MOVE */
int one_sided_reset; /* whether to reset one-sidedness */
{
  REAL *x;
  conmap_t *conmap;
  struct constraint *con[MAXCONPER],*thiscon;
  int oncount = 0;
  REAL f[MAXCONPER],delta[MAXCOORD];
  int j,itercount = 0;
  REAL diff,totaldiff;
  int walls = 0;  /* total number of constraints vertex is on */
  REAL maxdiff;
  int dcon=0; /* which constraint has largest difference */
  /*int hit_one_side;*/ /* if hits a one-sided constraint */
  int new_hits = 0; /* total new hits */

//  if ( one_sided_reset == RESET_ONESIDEDNESS )
//    clear_v_constraint_status(v_id);  // moved from inside loop, so 1-sided con is sticky
  x = get_coord(v_id);
  conmap = get_v_constraint_map(v_id);
  do
  {
    walls = 0; 
    /*hit_one_side = 0; */
    new_hits = 0;
    totaldiff = maxdiff = 0.0;

    for ( j = 1, oncount = 0 ; j <= (int)conmap[0] ; j++ )
    {
      thiscon = get_constraint(conmap[j]);
      diff = eval(thiscon->formula,x,v_id,NULL);
      if ( ((thiscon->attr & NONNEGATIVE) && !(conmap[j] & CON_HIT_BIT) && 
               ( diff > web.tolerance ))
       || ((thiscon->attr & NONPOSITIVE) && !(conmap[j] & CON_HIT_BIT) && 
               ( diff < -web.tolerance )) )
      {
        continue;
      }
      if ( thiscon->attr & (NONNEGATIVE|NONPOSITIVE) )
      { /* hit_one_side = 1; */
        if ( !(conmap[j] & CON_HIT_BIT) ) new_hits++;
        set_attr(v_id,HIT_ONE_SIDED);
      }
      f[oncount] = -diff;
      con[oncount++] = thiscon;
      if ( fabs(diff) > maxdiff ) 
      { maxdiff = fabs(diff); dcon = conmap[j]&CONMASK; }
      totaldiff += fabs(diff);
      if ( one_sided_reset == RESET_ONESIDEDNESS )
          set_v_constraint_status(v_id,conmap[j]);
      walls++;
    }
    if ( totaldiff < web.tolerance ) break;

    constr_proj(PLAINPROJ,oncount,con,x,f,delta,NULL,NO_DETECT,v_id);
    for ( j = 0 ; j < SDIM ; j++ )
        x[j] += delta[j];
    itercount++;
  }
  while ( itercount < MAXCONITER );

  if ( (itercount >= MAXCONITER) && (mode == ACTUAL_MOVE) )
  { struct constraint *con = get_constraint(dcon);
    sprintf(msg,
  "Vertex %s doesn't converge to constraint %s after %d iterations. \n maxdiff = %g, constraint_tolerance %g\n",
         ELNAME(v_id),con->name,MAXCONITER,(DOUBLE)totaldiff,web.tolerance);
     kb_error(1792,msg,WARNING);
  }

/*  if ( mode == ACTUAL_MOVE ) */
  { if ( walls ) set_attr(v_id,HIT_WALL);
     else unset_attr(v_id,HIT_WALL);
/*
     if ( hit_one_side ) set_attr(v_id,HIT_ONE_SIDED);
     else unset_attr(v_id,HIT_ONE_SIDED);
*/
  }
  return new_hits;
}

/*****************************************************************
*
*  Function: calc_constr_force_v()
*
*  Purpose: calculate force on vertex due to constraint energy. (string model)
*/

void calc_constr_force_v(v_id)
vertex_id v_id;
{
  REAL *f,*coord;
  struct constraint *constr;
  int i,j;
  conmap_t * conmap;
  int sign;
  REAL fval,deriv[MAXCOORD];

  int_val = ordinal(get_original(v_id))+1;  /* for eval  of file parameters */
  f = get_force(v_id);
  coord = get_coord(v_id);
  if ( get_vattr(v_id) & NEGBOUNDARY ) sign = -1;
  else sign = 1;
  conmap = get_v_constraint_map(v_id);
  for ( j = 1 ; j <= (int)conmap[0] ; j++ )
  { constr = get_constraint(conmap[j]);
    if ( !(constr->attr & CON_ENERGY) || (constr->compcount != 1) )
      continue;
    eval_all(constr->envect[0],coord,SDIM,&fval,deriv,v_id);      
    for ( i = 0 ; i < SDIM ; i++ )
      f[i] += -sign*deriv[i]; /* force by constraint */
  }

  return;
}

/**************************************************************************
*
*  Function: calc_constr_force_e()
*
*  Purpose: calculate force on endpoints of edge due to constraint energy.
*/

void calc_constr_force_e(e_id)
edge_id e_id;
{
  REAL *tcoord,*hcoord;
  REAL *tforce,*hforce;
  struct constraint *constr;
  int i,k,m;
  REAL side[MAXCOORD];
  REAL green[MAXCOORD];
  REAL green_deriv[MAXCOORD][MAXCOORD];
  int j,sign;
  REAL midpt[MAXCOORD];
  REAL grad;
  vertex_id headv,tailv;
  conmap_t *conmap;

  int_val = ordinal(get_original(e_id))+1;  /* for eval  of file parameters */
  if ( web.modeltype == QUADRATIC )
  { constr_edge_force_q(e_id);
    return;
  }
  else if ( web.modeltype == LAGRANGE )
     kb_error(1793,"calc_constr_force_e(): Cannot do LAGRANGE model.\n",RECOVERABLE);

  conmap = get_e_constraint_map(e_id);
  if ( get_eattr(e_id) & NEGBOUNDARY ) sign = -1;
  else sign = 1;
  if ( inverted(e_id) ) sign = -sign;

  headv = get_edge_headv(e_id);
  tailv = get_edge_tailv(e_id);

  tcoord = get_coord(tailv);
  hcoord = get_coord(headv);
  for ( j = 0 ; j < SDIM ; j++ )
    side[j] = hcoord[j] - tcoord[j];

  tforce = get_force(get_edge_tailv(e_id));
  hforce = get_force(get_edge_headv(e_id));
  for ( j = 1 ; j <= (int)conmap[0] ; j++ )
  { constr = get_constraint(conmap[j]);
    if ( !(constr->attr & CON_ENERGY) || (constr->compcount != SDIM) )
       continue;
    for ( m = 0 ; m < gauss1D_num ; m++ )
    {
      for ( i = 0 ; i < SDIM ; i++ )
        midpt[i] = gauss1Dpt[m]*hcoord[i] + (1 - gauss1Dpt[m])*tcoord[i];
      for ( i = 0 ; i < SDIM ; i++ )
        eval_all(constr->envect[i],midpt,SDIM,&green[i],green_deriv[i],e_id);
      for ( i = 0 ; i < SDIM ; i++ )
      { for ( grad = 0.0, k = 0 ; k < SDIM ; k++ )
          grad += side[k]*green_deriv[k][i];
        tforce[i] -= sign*gauss1Dwt[m]*((1-gauss1Dpt[m])*grad - green[i]);
        hforce[i] -= sign*gauss1Dwt[m]*(gauss1Dpt[m]*grad + green[i]);
      }
    }
  }

  return;
}

/*****************************************************************
*
*  Function: calc_constr_energy_v()
*
*  Purpose: calculate constraint energy due to vertex. (string model)
*/

void calc_constr_energy_v(v_id)
vertex_id v_id;
{
  REAL e;
  int j;
  conmap_t *conmap = get_v_constraint_map(v_id);
  struct constraint *constr;

  int_val = ordinal(get_original(v_id))+1;  /* for eval  of file parameters */
  for ( j = 1 ;  j <= (int)conmap[0] ; j++ )
  { constr = get_constraint(conmap[j]);
    if ( !(constr->attr & CON_ENERGY) || (constr->compcount != 1) ) continue;
    e = eval(constr->envect[0],get_coord(v_id),v_id,NULL);  
    if ( get_vattr(v_id) & NEGBOUNDARY )
      binary_tree_add(web.total_energy_addends,-e);
    else
      binary_tree_add(web.total_energy_addends,e);
  }
}

/*****************************************************************
*
*  Function: calc_constr_energy_e()
*
*  Purpose: calculate energy due to edge on constraint.
*              Also contributions to quantities from constraints.
*/

void calc_constr_energy_e(e_id)
edge_id e_id;
{
  REAL *tcoord,*hcoord;
  struct constraint *constr;
  int i,j,k;
  REAL energy = 0.0;
  REAL side[MAXCOORD];
  REAL green[MAXCOORD];
  int sign;
  REAL midpt[MAXCOORD];
  vertex_id headv,tailv;
  conmap_t *conmap;

  int_val = ordinal(get_original(e_id))+1;  /* for eval  of file parameters */
  if ( web.modeltype == QUADRATIC )
  { constr_edge_energy_q(e_id);
    return;
  }
  else if ( web.modeltype == LAGRANGE )
     kb_error(1794,"calc_constr_energy_e(): Cannot do LAGRANGE model.\n",RECOVERABLE);

  conmap = get_e_constraint_map(e_id);
  if ( get_eattr(e_id) & NEGBOUNDARY ) sign = -1;
  else sign = 1;
  if ( inverted(e_id) ) sign = -sign;

  headv = get_edge_headv(e_id);
  tailv = get_edge_tailv(e_id);
        
  tcoord = get_coord(tailv);
  hcoord = get_coord(headv);
  for ( j = 0 ; j < SDIM ; j++ )
    side[j] = hcoord[j] - tcoord[j];

  for ( j = 1 ; j <= (int)conmap[0] ; j++ )
  { constr = get_constraint(conmap[j]);
    if ( !(constr->attr & CON_ENERGY) ) continue;
    if ( constr->compcount != SDIM ) continue;
    for ( k = 0 ; k < gauss1D_num ; k++ )
    { for ( i = 0 ; i < SDIM ; i++ )
        midpt[i] = gauss1Dpt[k]*hcoord[i] + (1 - gauss1Dpt[k])*tcoord[i];
      if ( constr->attr & CON_ENERGY )
      { for ( i = 0 ; i < SDIM ; i++ )
          green[i] = eval(constr->envect[i],midpt,e_id,NULL); 
        energy += sign*gauss1Dwt[k]*SDIM_dot(side,green);
      }
    }

  }

  binary_tree_add(web.total_energy_addends,energy);

}

/*****************************************************************
*
*  Function: calc_constr_content_v()
*
*  Purpose: calculate interior content due to vertex. (string model)
*/

void calc_constr_content_v(v_id)
vertex_id v_id;
{
  REAL e=0.0;
  int j;
  conmap_t * conmap;
  struct constraint *constr;
  body_id b_id;
  facetedge_id fe_id;
  facet_id f_id;
  edge_id  e_id = get_vertex_edge(v_id);
  edge_id  start_e = e_id;
  edge_id  next_e;
  int min_rank,max_rank;

  if ( !valid_id(e_id) ) return;  /* not on a cell */

  int_val = ordinal(get_original(v_id))+1;  /* for eval  of file parameters */
  conmap = get_v_constraint_map(v_id); /* only hit constraints */
  
  min_rank = MAXINT; max_rank = 0;
  for ( j = 1 ; j <= (int)conmap[0] ; j++ )
  { if ( !(conmap[j] & CON_HIT_BIT) ) continue;
    constr = get_constraint(conmap[j]);
    if ( constr->content_rank < min_rank ) min_rank = constr->content_rank;
    if ( constr->content_rank > max_rank ) max_rank = constr->content_rank;
  }
    
  for ( j = 1 ; j <= (int)conmap[0] ; j++ )
  { if ( !(conmap[j] & CON_HIT_BIT) ) continue;
    constr = get_constraint(conmap[j]);
    if ( !(constr->attr & CON_CONTENT) ) continue;
    if ( constr->compcount != 1 ) continue;
    
    if ( get_vattr(v_id) & NEGBOUNDARY )
      e = -eval(constr->convect[0],get_coord(v_id),v_id,NULL);  
    else
      e = eval(constr->convect[0],get_coord(v_id),v_id,NULL);  
  
    next_e = e_id;
    do
    { facetedge_id start_fe;
      
      e_id = next_e;
      next_e = get_next_tail_edge(e_id);
      if ( get_eattr(e_id) & NONCONTENT ) continue;
  
      /* cell on one side of edge */
      start_fe = fe_id = get_edge_fe(e_id);    
      if ( !valid_id(fe_id) ) continue;
      
      do 
      {
        if ( !valid_id(get_prev_edge(fe_id)) )
        { 
          f_id = get_fe_facet(fe_id);
          if ( valid_id(f_id) && 
           ( (!inverted(f_id) && constr->content_rank >= max_rank) 
               || (inverted(f_id) && constr->content_rank <= min_rank)) )
          {
            add_facet_area(f_id,-e);
            
            b_id = get_facet_body(f_id);
            if ( valid_id(b_id) )
              add_body_volume(b_id,-e);
            b_id = get_facet_body(inverse_id(f_id));
            if ( valid_id(b_id) )
              add_body_volume(b_id,e);
          }
        }
        fe_id = get_next_facet(fe_id);
      } while (!equal_id(fe_id,start_fe));
  
    } while ( !equal_id(next_e,start_e) );
  
  }
}

/*****************************************************************
*
*  Function: calc_constr_content_e()
*
*  Purpose: calculate interior content due to edge. (film model)
*/

void calc_constr_content_e(e_id)
edge_id e_id;
{
  REAL *tcoord,*hcoord;
  struct constraint *constr;
  int i,k;
  REAL content = 0.0;
  REAL midpt[MAXCOORD];
  REAL side[MAXCOORD];
  REAL green[MAXCOORD];
  body_id b_id;
  facetedge_id fe_id;
  facet_id f_id;
  int j;
  conmap_t *conmap;
  int sign;
  vertex_id headv,tailv;
  facetedge_id first_fe;
  int min_rank, max_rank;

  if ( web.modeltype == QUADRATIC )
  { constr_edge_content_q(e_id);
    return;
  }
  else if ( web.modeltype == LAGRANGE )
     kb_error(1795,"calc_constr_content_e(): Cannot do LAGRANGE model.\n",
        RECOVERABLE);


  int_val = ordinal(get_original(e_id))+1;  /* for eval  of file parameters */
  conmap = get_e_constraint_map(e_id); 
  if ( get_eattr(e_id) & NEGBOUNDARY ) sign = -1;
  else sign = 1;
  if ( inverted(e_id) ) sign = -sign;

  headv = get_edge_headv(e_id);
  tailv = get_edge_tailv(e_id);

  tcoord = get_coord(tailv);
  hcoord = get_coord(headv);
  for ( j = 0 ; j < SDIM ; j++ )
    side[j] = hcoord[j] - tcoord[j];


  min_rank = MAXINT; max_rank = 0;
  for ( j = 1 ; j <= (int)conmap[0] ; j++ )
  { 
    constr = get_constraint(conmap[j]);
    if ( constr->content_rank < min_rank ) min_rank = constr->content_rank;
    if ( constr->content_rank > max_rank ) max_rank = constr->content_rank;
  }
    
  for ( j = 1 ; j <= (int)conmap[0] ; j++ )
  {
    constr = get_constraint(conmap[j]);
    if ( !(constr->attr & CON_CONTENT) ) continue;
    if ( constr->compcount != SDIM ) continue;
    content = 0.0;
    for ( k = 0 ; k < gauss1D_num ; k++ )
    {
      for ( i = 0 ; i < SDIM ; i++ )
        midpt[i] = gauss1Dpt[k]*hcoord[i] + (1 - gauss1Dpt[k])*tcoord[i];
      for ( i = 0 ; i < SDIM ; i++ )
        green[i] = eval(constr->convect[i],midpt,e_id,NULL);
      content += sign*gauss1Dwt[k]*SDIM_dot(side,green);
    }

    fe_id = first_fe = get_edge_fe(e_id);
    if ( valid_id(fe_id) ) do
    { /* cell on plus side of edge */
      f_id = get_fe_facet(fe_id);
      if ( valid_id(f_id) && !(get_fattr(f_id) & NONCONTENT)
        && (constr->content_rank >= max_rank) ) 
      {
        b_id = get_facet_body(f_id);
        if ( valid_id(b_id) )
          add_body_volume(b_id,content);
      }
  
      /* cell on other side of edge */
      f_id = get_fe_facet(inverse_id(fe_id));
      if ( valid_id(f_id) && !(get_fattr(f_id) & NONCONTENT)
         && (constr->content_rank <= min_rank) ) 
      {
        b_id = get_facet_body(f_id);
        if ( valid_id(b_id) )
          add_body_volume(b_id,-content);
      }
      fe_id = get_next_facet(fe_id);
    } while ( valid_id(fe_id) && !equal_id(fe_id,first_fe) );

  }
  return;
}


/*****************************************************************
*
*  Function: constr_spring_energy()
*
*  Purpose:  Calculate energy of kludge constraint force.
*                Constant factor included to make it best approx
*                of true area.
*/

void constr_spring_energy(e_id)
edge_id e_id;
{
  REAL sprenergy = 0.0;
  REAL s[MAXCOORD];
  REAL ss;
  struct constraint *constr[MAXCONPER];
  int concount;
  conmap_t * conmap;
  vertex_id tail,head;
  int i,j; 

  if ( get_eattr(e_id) & FIXED ) return;
  if ( !(get_eattr(e_id) & CONSTRAINT) ) return;

  tail = get_edge_tailv(e_id);
  head = get_edge_headv(e_id);

  /* find which constraints have CONVEX attribute */
  conmap = get_e_constraint_map(e_id);
  for ( j = 1,i = 0 ; j <= (int)conmap[0] ; j++ )
  { constr[i] = get_constraint(conmap[j]);
    if ( constr[i]->attr & B_CONVEX ) 
      i++;    /* keep this one */
  }
  if ( i == 0 ) return;
  concount = i;  

  /* now the calculation */
  get_edge_side(e_id,s);
  ss = SDIM_dot(s,s);

#ifndef OLDWAY
  for ( i = 0 ; i < concount ; i++ )
  { REAL *coord,ff,fs;
    REAL fval,grad[MAXCOORD];
    coord = get_coord(tail);
    eval_all(constr[i]->formula,coord,SDIM,&fval,grad,e_id);
    ff = SDIM_dot(grad,grad);
    if ( ff <= 0.0 )
    { sprintf(errmsg,
   "Gap energy error: Vertex %s is on a convex constraint at zero gradient.\n",
             ELNAME(tail));
       kb_error(2001,errmsg,WARNING);
       ff = 1.0;
    }
    fs = SDIM_dot(s,grad);
    sprenergy += fabs(fs)*sqrt(ss/ff)/12;

    coord = get_coord(head);
    eval_all(constr[i]->formula,coord,SDIM,&fval,grad,e_id);
    ff = SDIM_dot(grad,grad);
    if ( ff <= 0.0 )
    { sprintf(errmsg,
  "Gap energy error: Vertex %s is on a convex constraint at zero gradient.\n",
             ELNAME(head));
      kb_error(2002,errmsg,WARNING);
      ff = 1.0;
    }
    fs = SDIM_dot(s,grad);
    sprenergy += fabs(fs)*sqrt(ss/ff)/12;
  }
#endif

#ifdef OLDWAYX

  constr_proj(TANGPROJ,concount,constr,get_coord(tail),s,q,NULL,
      NO_DETECT,tail);
  qq = SDIM_dot(q,q);
  sprenergy = ss*sqrt(qq/(ss - qq))/12;

  constr_proj(TANGPROJ,concount,constr,get_coord(head),s,q,NULL,
      NO_DETECT,head);
  qq = SDIM_dot(q,q);
  sprenergy += ss*sqrt(qq/(ss - qq))/12;
#endif

  sprenergy *= web.spring_constant;
  binary_tree_add(web.total_energy_addends,sprenergy);
  web.spring_energy += sprenergy;
}

/****************************************************************
*
*  Function: constr_springs()
*
*  Purpose:  Since only vertices are actually confined to constraints,
*                edges and faces supposedly on constraints can pull
*                away from convex constraints, and in fact do, since
*                a long edge short-cuts the constraints.  To prevent
*                this and encourage equal-length constraint edges, an
*                energy penalty is inflicted for an edge angling away
*                from its constraint.  
*                The energy penalty is 2/3 of the area of the right
*                triangle whose base is half the side and whose hypoteneuse
*                lies on the constraint tangent.  This is done for both
*                ends of the side.
*/


void  constr_springs(e_id)
edge_id e_id;
{
  REAL s[MAXCOORD],*fh,*ft;
  REAL ss; /* square lengths */
  struct constraint *constr[MAXCONPER];
  int concount;
  conmap_t * conmap;
  vertex_id head,tail;
  int i,j; 
  MAT2D(second,MAXCOORD,MAXCOORD); /* for second partials */

  if ( get_eattr(e_id) & FIXED ) return;
  if ( !(get_eattr(e_id) & CONSTRAINT) ) return;

  tail = get_edge_tailv(e_id);
  head = get_edge_headv(e_id);

  /* find which constraints have CONVEX attribute */
  conmap = get_e_constraint_map(e_id);
  for ( j = 1,i=0 ; j <= (int)conmap[0] ; j++ )
  { constr[i] = get_constraint(conmap[j]);
    if ( constr[i]->attr & B_CONVEX ) i++;    /* keep this one */
   }
  if ( i == 0 ) return;
  concount = i;  

  /* now the calculation */
  get_edge_side(e_id,s);
  ss = SDIM_dot(s,s);
  ft = get_force(tail);
  fh = get_force(head);

#ifndef OLDWAYX
  for ( i = 0 ; i < concount ; i++ )
  { REAL *coord;
    REAL fval,grad[MAXCOORD];
    REAL ff,fs,t;

    coord = get_coord(tail);
    eval_second(constr[i]->formula,coord,SDIM,&fval,grad,second,tail);
    ff = SDIM_dot(grad,grad);
    if ( ff <= 0.0 ) ff = 1;
    fs = SDIM_dot(s,grad);
    t = sqrt(ss/ff);
    if ( fs < 0.0 ) t = -t;  /* to take care of fabs() */
    for ( j = 0 ; j < SDIM ; j++ )
    { REAL g;
      g = -t*grad[j] + t*SDIM_dot(s,second[j]) 
                    + fs/t*(-s[j]/ff - ss/ff/ff*SDIM_dot(grad,second[j]));
      ft[j] -= web.spring_constant*g/12;
      g = t*grad[j] + fs/t/ff*s[j];
      fh[j] -= web.spring_constant*g/12;
    }
    coord = get_coord(head);
    eval_second(constr[i]->formula,coord,SDIM,&fval,grad,second,head);
    ff = SDIM_dot(grad,grad);
    if ( ff <= 0.0 ) ff = 1;
    fs = SDIM_dot(s,grad);
    t = sqrt(ss/ff);
    if ( fs < 0.0 ) t = -t;  /* to take care of fabs() */
    for ( j = 0 ; j < SDIM ; j++ )
    { REAL g;
      g = t*grad[j] + t*SDIM_dot(s,second[j]) 
                   + fs/t*(s[j]/ff - ss/ff/ff*SDIM_dot(grad,second[j]));
      fh[j] -= web.spring_constant*g/12;
      g = -t*grad[j] - fs/t/ff*s[j];
      ft[j] -= web.spring_constant*g/12;
    }
  }
#endif

#ifdef OLDWAYX

  /*tail*/
  constr_proj(TANGPROJ,concount,constr,get_coord(tail),s,q,NULL,
      NO_DETECT,tail);
  for ( i = 0 ; i < SDIM ; i++ )
     q[i] = s[i] - q[i];    /* get tangent side */
  qq = SDIM_dot(q,q);
  norm = (1 + (ss-qq)/3/qq)*sqrt(fabs(ss-qq)/qq)/2; /* fabs due to machine inaccuracy */
  for ( i = 0 ; i < SDIM ; i++ )
     ft[i] += web.spring_constant*q[i]*norm;


  /* head */
  constr_proj(TANGPROJ,concount,constr,get_coord(head),s,q,NULL,
      NO_DETECT,head);
  for ( i = 0 ; i < SDIM ; i++ )
     q[i] = s[i] - q[i];    /* get tangent side */
  qq = SDIM_dot(q,q);
  norm = (1 + (ss-qq)/3/qq)*sqrt(fabs(ss-qq)/qq)/2;
  for ( i = 0 ; i < SDIM ; i++ )
     fh[i] -= web.spring_constant*q[i]*norm;
#endif 

}

/*************************************************************************
 
  Following are quadratic model version of constraint integral routines.
      
*************************************************************************/



/************************************************************************
*
*  Function: constr_edge_energy_q()
*
*  Purpose:  Returns energy due to one edge on constraint.
*
*  Quadratic version.
*/

void constr_edge_energy_q(e_id)
edge_id e_id;
{
  REAL x[EDGE_CTRL][MAXCOORD];
  REAL *pt[EDGE_CTRL];
  REAL etang[MAXCOORD];
  vertex_id v[EDGE_CTRL];
  int i,j,k;
  struct constraint *constr;
  REAL energy = 0.0;
  REAL side[MAXCOORD];
  REAL green[MAXCOORD];
  conmap_t *conmap;
  int sign;
  REAL gpt[MAXCOORD];
          
  int_val = ordinal(get_original(e_id))+1;  /* for eval  of file parameters */
  conmap = get_e_constraint_map(e_id);
  get_edge_side(e_id,side);
  if ( get_eattr(e_id) & NEGBOUNDARY ) sign = -1;
  else sign = 1;
                  
  v[0] = get_edge_tailv(e_id);
  v[1] = get_edge_midv(e_id);
  v[2] = get_edge_headv(e_id);
  for ( i = 0 ; i < EDGE_CTRL ; i++ )
    pt[i] = get_coord(v[i]);

  for ( i = 0 ; i < EDGE_CTRL ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
        x[i][j] = pt[i][j];
             
     
  /* calculate tangents at integration points and accumulate */
  for ( i = 0 ; i < EDGE_INTERP ; i++ )
  { for ( j = 0 ; j < SDIM ; j ++ )
    { etang[j] = 0.0;
      gpt[j] = 0.0;
      for ( k = 0 ; k < EDGE_CTRL ; k++ )
      { etang[j] += sdip[k][i]*x[k][j];
        gpt[j] += gcombo[k][i]*x[k][j];
      }
    }
    for ( j = 1 ; j <= (int)conmap[0] ; j++ )
    { constr = get_constraint(conmap[j]);
      if ( !(constr->attr & CON_ENERGY) ) continue;
      if ( constr->compcount != SDIM ) continue;
      for ( k = 0 ; k < SDIM ; k++ )
        green[k] = eval(constr->envect[k],gpt,e_id,NULL); 
      energy += sign*gauss2wt[i]*SDIM_dot(etang,green);
    }
  }
     
  binary_tree_add(web.total_energy_addends,energy);

}
 


/************************************************************************
*
*  Function: constr_edge_force_q()
*
*  Purpose:  Calculates force due to one edge on constraint.
*
*  Quadratic version.
*/

void constr_edge_force_q(e_id)
edge_id e_id;
{
  REAL x[EDGE_CTRL][MAXCOORD];
  REAL *pt[EDGE_CTRL];
  REAL etang[MAXCOORD];
  vertex_id v[EDGE_CTRL];
  int i,j,k,m,n;
  struct constraint *constr;
  REAL side[MAXCOORD];
  REAL green[MAXCOORD];
  REAL green_deriv[MAXCOORD][MAXCOORD];
  conmap_t *conmap;
  int sign;
  REAL gpt[MAXCOORD];
  REAL *force[EDGE_CTRL];
          
  int_val = ordinal(get_original(e_id))+1;  /* for eval  of file parameters */
  conmap = get_e_constraint_map(e_id);
  get_edge_side(e_id,side);
  if ( get_eattr(e_id) & NEGBOUNDARY ) sign = -1;
  else sign = 1;
                  
  v[0] = get_edge_tailv(e_id);
  v[1] = get_edge_midv(e_id);
  v[2] = get_edge_headv(e_id);
  for ( i = 0 ; i < EDGE_CTRL ; i++ )
  { pt[i] = get_coord(v[i]);
    force[i] = get_force(v[i]);
  }
  for ( i = 0 ; i < EDGE_CTRL ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
        x[i][j] = pt[i][j];
             
  /* calculate tangents at integration points and accumulate */
  for ( i = 0 ; i < EDGE_INTERP ; i++ )
  { for ( j = 0 ; j < SDIM ; j ++ )
    { etang[j] = 0.0;
      gpt[j] = 0.0;
      for ( k = 0 ; k < EDGE_CTRL ; k++ )
      { etang[j] += sdip[k][i]*x[k][j];
        gpt[j] += gcombo[k][i]*x[k][j];
      }
    }
    for ( j = 1 ; j <= (int)conmap[0] ; j++ )
    { REAL tangdot[MAXCOORD];
      constr = get_constraint(conmap[j]);
      if ( !(constr->attr & CON_ENERGY) ) continue;
      if ( constr->compcount != SDIM ) continue;
      for ( m = 0 ; m < SDIM ; m++ )
        eval_all(constr->envect[m],gpt,SDIM,&green[m],green_deriv[m],e_id);
      for ( n = 0 ; n < SDIM ; n++ )
        for ( m = 0, tangdot[n] = 0.0 ; m < SDIM ; m++ )
          tangdot[n] += etang[m]*green_deriv[m][n];
      for ( k = 0 ; k < EDGE_CTRL ; k++ )
      { for ( m = 0 ; m < SDIM ; m++ )
          force[k][m] -= sign*gauss2wt[i]*sdip[k][i]*green[m];
        for ( n = 0 ; n < SDIM ; n++ )
          force[k][n] -= sign*gauss2wt[i]*gcombo[k][i]*tangdot[n];
      }              
    }
  }
}
 


/************************************************************************
*
*  Function: constr_edge_content_q()
*
*  Purpose:  Finds volume due to one edge on a constraint.
*
*  Quadratic version.
*/

void constr_edge_content_q(e_id)
edge_id e_id;
{
  REAL x[EDGE_CTRL][MAXCOORD];
  REAL *pt[EDGE_CTRL];
  REAL etang[MAXCOORD];
  vertex_id v[EDGE_CTRL];
  int i,j,k;
  struct constraint *constr;
  REAL content = 0.0;
  REAL side[MAXCOORD];
  REAL green[MAXCOORD];
  conmap_t * conmap;
  int sign;
  REAL gpt[MAXCOORD];
  body_id b_id;
  facet_id f_id;
  facetedge_id fe_id = get_edge_fe(e_id);
  facetedge_id first_fe;
          
  int_val = ordinal(get_original(e_id))+1;  /* for eval  of file parameters */
  conmap = get_e_constraint_map(e_id);
  get_edge_side(e_id,side);
  if ( get_eattr(e_id) & NEGBOUNDARY ) sign = -1;
  else sign = 1;
                  
  v[0] = get_edge_tailv(e_id);
  v[1] = get_edge_midv(e_id);
  v[2] = get_edge_headv(e_id);
  for ( i = 0 ; i < EDGE_CTRL ; i++ )
    pt[i] = get_coord(v[i]);

  for ( i = 0 ; i < EDGE_CTRL ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
        x[i][j] = pt[i][j];
             
     
  /* calculate tangents at integration points and accumulate */
  for ( i = 0 ; i < EDGE_INTERP ; i++ )
  { for ( j = 0 ; j < SDIM ; j ++ )
    { etang[j] = 0.0;
      gpt[j] = 0.0;
      for ( k = 0 ; k < EDGE_CTRL ; k++ )
      { etang[j] += sdip[k][i]*x[k][j];
        gpt[j] += gcombo[k][i]*x[k][j];
      }
    }
    for ( j = 1 ; j <= (int)conmap[0] ; j++ )
    { constr = get_constraint(conmap[j]);
      if ( !(constr->attr & CON_CONTENT) ) continue;
      if ( constr->compcount != SDIM ) continue;
        for ( k = 0 ; k < SDIM ; k++ )
          green[k] = eval(constr->convect[k],gpt,e_id,NULL);
      content += sign*gauss2wt[i]*SDIM_dot(etang,green);
    }
  }
     
  fe_id = first_fe = get_edge_fe(e_id);
  if ( valid_id(fe_id) ) do
  { /* cell on plus side of edge */
    f_id = get_fe_facet(fe_id);
    if ( valid_id(f_id) && !(get_fattr(f_id) & NONCONTENT) ) 
    {
      b_id = get_facet_body(f_id);
      if ( valid_id(b_id) )
        add_body_volume(b_id,content);
    }

    /* cell on other side of edge */
    f_id = get_fe_facet(inverse_id(fe_id));
    if ( valid_id(f_id) && !(get_fattr(f_id) & NONCONTENT) ) 
    {
      b_id = get_facet_body(f_id);
      if ( valid_id(b_id) )
        add_body_volume(b_id,-content);
    }
    fe_id = get_next_facet(fe_id);
  } while ( valid_id(fe_id) && !equal_id(fe_id,first_fe) );

  return;
}


/************************************************************************
*
*  Function: constr_vol_grad_q()
*
*  Purpose:  Calculates volume gradients due to one edge on constraint.
*
*  Quadratic version.
*/

void constr_vol_grad_q(e_id)
edge_id e_id;
{
  REAL x[EDGE_CTRL][MAXCOORD];
  REAL *pt[EDGE_CTRL];
  REAL etang[MAXCOORD];
  vertex_id v[EDGE_CTRL];
  int i,j,k;
  REAL green[MAXCOORD];
  REAL green_deriv[MAXCOORD][MAXCOORD];
  struct constraint *constr;
  int m,n;
  REAL grad[EDGE_CTRL][MAXCOORD];
  conmap_t * conmap;
  int sign,bodysign=1;
  REAL gpt[MAXCOORD];
  struct volgrad *vgptri;
  facet_id f_id;
  facetedge_id fe,start_fe;

  int_val = ordinal(get_original(e_id))+1;  /* for eval  of file parameters */
  /* get basic edge data */          
  conmap = get_e_constraint_map(e_id);
  if ( get_eattr(e_id) & NEGBOUNDARY ) sign = -1;
  else sign = 1;
                  
  v[0] = get_edge_tailv(e_id);
  v[1] = get_edge_midv(e_id);
  v[2] = get_edge_headv(e_id);
  for ( i = 0 ; i < EDGE_CTRL ; i++ )
     pt[i] = get_coord(v[i]);
  for ( i = 0 ; i < EDGE_CTRL ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
        x[i][j] = pt[i][j];
             
  memset((char *)grad,0,sizeof(grad));

  /* find content integral gradients */     
  for ( i = 0 ; i < EDGE_INTERP ; i++ )
  { /* calculate coords and tangents at integration point */
    for ( j = 0 ; j < SDIM ; j ++ )
    { etang[j] = 0.0;
      gpt[j] = 0.0;
      for ( k = 0 ; k < EDGE_CTRL ; k++ )
      { etang[j] += sdip[k][i]*x[k][j];
        gpt[j] += gcombo[k][i]*x[k][j];
      }
    }
 
    /* accumulate gradients due to this integration point */
    for ( j = 1 ; j <= (int)conmap[0] ; j++ )
    { REAL tangdot[MAXCOORD];

      constr = get_constraint(conmap[j]);
      if ( !(constr->attr & CON_CONTENT) ) continue;
      if ( constr->compcount != SDIM ) continue;
      for ( m = 0 ; m < SDIM ; m++ )
        eval_all(constr->convect[m],gpt,SDIM,&green[m],green_deriv[m],e_id); 
      for ( n = 0 ; n < SDIM ; n++ )
        for ( m = 0, tangdot[n] = 0.0 ; m < SDIM ; m++ )
          tangdot[n] += etang[m]*green_deriv[m][n];
      for ( k = 0 ; k < EDGE_CTRL ; k++ )
      { for ( m = 0 ; m < SDIM ; m++ )
          grad[k][m] += sign*gauss2wt[i]*sdip[k][i]*green[m];
        for ( n = 0 ; n < SDIM ; n++ )
          grad[k][n] += sign*gauss2wt[i]*gcombo[k][i]*tangdot[n];
      }              
    }
  }

  /* now add gradients to proper bodies */
  fe = start_fe = get_edge_fe(e_id);
  if ( valid_id(start_fe) )
    do
    { f_id = get_fe_facet(fe);
      fe = get_next_facet(fe);
      if ( get_fattr(f_id) & NONCONTENT ) continue;
      
      for ( m = 0 ; m < EDGE_CTRL ; m++ )
      { vgptri = get_vertex_vgrad(v[m]);
        for  ( ; vgptri ; vgptri = vgptri->chain )
        { 
          if ( !valid_id(vgptri->bb_id) ) continue; /* skip quantities */
          if ( !equal_id(get_facet_body(f_id),vgptri->bb_id) ) 
          { if ( !equal_id(get_facet_body(inverse_id(f_id)),vgptri->bb_id) )
                continue; 
            else  bodysign = -sign;
          }
          else bodysign = sign;
          
          for ( k = 0 ; k < SDIM ; k++ )
            vgptri->grad[k] += bodysign*grad[m][k];
        }
      }
    } while ( !equal_id(fe,start_fe));
}


/*****************************************************************
*
*  Function: constr_basis()
*
*  Purpose: calculate basis of constraint tangent.
*/

int constr_basis(v_id,basis)
vertex_id v_id;
REAL **basis;  /* for return */
{
  conmap_t *conmap;
  struct constraint *con[MAXCONPER];
  int oncount = 0;
  REAL ggrad[MAXCOORD][MAXCOORD];
  REAL *grad[MAXCOORD]; /* for proper matrix form */
  REAL fval;
  int i,j;
  MAT2D(bas,MAXCOORD,MAXCOORD);
  int nullity;

  conmap = get_v_constraint_map(v_id);
  for ( j = 1 , oncount = 0; j <= (int)conmap[0] ; j++ )
        con[oncount++] = get_constraint(conmap[j]);

  /* first calc constraint gradients */
  for ( i = 0 ; i < oncount ; i++ )     
  { grad[i] = ggrad[i];
    eval_all(con[i]->formula,get_coord(v_id),SDIM,&fval,grad[i],v_id);
  }

  /* now get basis */
  nullity = kernel_basis(grad,bas,oncount,SDIM);

  /* transpose */
  for ( i = 0 ; i < nullity ; i++ )
    for ( j = 0 ; j < SDIM ; j++ )
      basis[i][j] = bas[j][i];

  return nullity;
}

/***************************************************************************
*
* function: force_project()
*
* purpose: project vector to constraint plane at vertex.
*
*/

void force_project(fin,v_id,fout)
REAL *fin;
vertex_id v_id;
REAL *fout;
{ int attr = get_vattr(v_id);
  int i,j;
  if ( attr & CONSTRAINT )
  { conmap_t * conmap = get_v_constraint_map(v_id);
    int oncount = 0;
    struct constraint *con[MAXCONPER];
    int conlist[MAXCONPER];
    REAL perp[MAXCOORD];

    for ( j = 1 ; j <= (int)conmap[0] ; j++ )
    { if ( conmap[j] & CON_HIT_BIT )
      { conlist[oncount] = conmap[j] & CONMASK;
        con[oncount] = get_constraint(conmap[j]);
        oncount++;
      }
      if ( oncount )
      { constr_proj(TANGPROJ,oncount,con,get_coord(v_id),
                                 fin,perp,conlist,0,v_id);
        for ( j = 0 ; j < SDIM ; j++ )
          fout[j] = fin[j] - perp[j];
        return;
      }
    }
  }
  else if ( attr & BOUNDARY )
  { MAT2D(a,MAXCOORD,MAXCOORD);
    b_proj(get_boundary(v_id),get_param(v_id),a,TANGPROJ,v_id);
    matvec_mul(a,fin,fout,SDIM,SDIM);
    return;
  }

  /* no projecting to do */
  for ( i = 0 ; i < SDIM ; i++ )
    fout[i] = fin[i];
}

 
