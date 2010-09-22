/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*************************************************************
*
*  File: hessian.c 
*
*  Purpose: Main file for hessian commands.
*
*/

#define GDEBUG 
int hess_debug;  /* debugging flag */
/* use ysmp */


#include "include.h"
#include "f2c.h"
          
int hmode; /* mode of motion */
struct hess_verlist *vhead = NULL;  /* main vertex list */
int vhead_count;          /* total number of vertices */
#ifdef XXX
REAL *rhs = NULL;                 /* right side of augmented matrix  */
REAL *X = NULL;                 /* solution to augmented matrix  */
#endif
REAL *conrhs = NULL;    /* right side of augmented matrix for constraints */
int vertex_rows; /* number of rows for vertex degrees of freedom */

REAL **vproj_base; /* vertex projection matrix row pointers */
REAL *vproj_space; /* vertex projection matrix arena */
REAL ***conhess_base; /* vertex constraint hessian arena */
REAL rhs_norm;

static int ritz_done_flag;
REAL **ritzvecs;
static int ritzdim;
void optparamhess ARGS((struct linsys *,REAL *));  /* right side */

#ifdef OLDHASH
/********************************************************************
* 
* function: hess_hash_search()
*
* purpose: Finds existing entry or allocates entry.
*             Installs key values, and adds hessian value.
* return:  Pointer to entry.
*/
void hess_hash_search(col,row,value)
int row,col;  /* meant to do upper triangle */
REAL value;  /* value to add */
{
  struct hess_entry *e;
  int spot;

  if ( row > col ) return;
  if ( (value == 0.0) && (row != col) ) return;   

  if ( col >=  S->A_rows ) /* a constraint grad */
  { if ( col == goner ) return;
    if ( !array[col].u.congrad ) 
        array[col].u.congrad = (REAL *)mycalloc(S->A_rows,sizeof(REAL));
    e = (struct hess_entry *)(array[col].u.congrad+row);
    e->value += value;
    if ( !sparse_constraints_flag )
      return;
  }


  if ( hashcount >= max_fill ) 
     hess_hash_expand();

  /* search hash table */
  spot = hash(row,col) % table_size;
  e = hashtable + spot;
  while ( e->row != HASHEMPTY )
  { if ( (e->row == row) && (e->col == col) )
     { e->value += value; return; 
     }
     spot++;
     if ( spot >= table_size ) spot -= table_size;
     e = hashtable + spot;
     hash_extraprobes++;
  }
  /* if here, then have empty slot and need to insert */
  e->col = col; e->row = row;  hashcount++; 
  e->value = value;
}
#endif

/***********************************************************************
* 
* function: fill_grad()
*
* purpose: process gradient of function at constraint 
*/

void fill_grad(S,v,grad,rhs)
struct linsys *S;
struct hess_verlist *v;
REAL *grad;
REAL *rhs;
{ REAL g[MAXCOORD];
  int k,a,b;

  if ( rhs_flag )
  { if ( v->proj )
    { vec_mat_mul(grad,v->proj,g,SDIM,v->freedom);
      for ( k = 0 ; k < v->freedom ; k++ )
          rhs[v->rownum+k] -= g[k];
    }
    else
      for ( k = 0 ; k < v->freedom ; k++ )
         rhs[v->rownum+k] -= grad[k];
  }

  if ( hess_flag && v->conhess )
  { 
    for ( a = 0 ; a < v->freedom ; a++ )
     for ( b = 0 ; b <= a ; b++ )
       sp_hash_search(S,v->rownum+b,v->rownum+a,
          SDIM_dot(grad,v->conhess[a][b]));
  }
}

/*************************************************************************
*
* function: fill_self_entry()
*
* purpose: fill proper hessian matrix spot for single vertex second derivs
*/

void fill_self_entry(S,v_id,self)
struct linsys *S;
vertex_id v_id;
REAL **self; /* full dim values */
{ int j,k;
  struct hess_verlist *v;

  v = get_vertex_vhead(v_id);

  if ( v->proj )
  { MAT2D(temp_mat,MAXCOORD,MAXCOORD);
    MAT2D(temp_mat2,MAXCOORD,MAXCOORD);
    tr_mat_mul(v->proj,self,temp_mat,SDIM,v->freedom,SDIM);
    mat_mult(temp_mat,v->proj,temp_mat2,v->freedom,SDIM,v->freedom);
    for ( j = 0 ; j < v->freedom ; j++ )
      for ( k = 0 ; k <= j ; k++ )
         sp_hash_search(S,v->rownum+k,v->rownum+j, temp_mat2[j][k]);
  }
  else 
  { for ( j = 0 ; j < v->freedom ; j++ )
      for ( k = 0 ; k <= j ; k++ )
        sp_hash_search(S,v->rownum+k,v->rownum+j,self[j][k]);
  }
}


/*************************************************************************
*
* function: fill_mixed_entry()
*
* purpose: fill proper hessian matrix spot for mixed vertex second derivs
*/

void fill_mixed_entry(S,v_id1,v_id2,mixed)
struct linsys *S;
vertex_id v_id1,v_id2;
REAL **mixed; /* full dim values */
{ int k,j;
  REAL **oo;
  MAT2D(temp_mat,MAXCOORD,MAXCOORD);
  MAT2D(temp_mat2,MAXCOORD,MAXCOORD);
  struct hess_verlist *v1,*v2;
  
  if ( equal_id(v_id1,v_id2) )
  { fill_self_entry(S,v_id1,mixed);
    return;
  }

  v1 = get_vertex_vhead(v_id1); 
  v2 = get_vertex_vhead(v_id2);
  if ( v1->proj )
  { tr_mat_mul(v1->proj,mixed,temp_mat,SDIM,v1->freedom,SDIM);
    oo = temp_mat;
  }
  else oo = mixed;
  if ( v2->proj )
  { mat_mult(oo,v2->proj,temp_mat2,v1->freedom,SDIM,v2->freedom);
    oo = temp_mat2;
  }

  if ( v1->rownum < v2->rownum )
    for ( j = 0 ; j < v1->freedom ; j++ )
     for ( k = 0 ; k < v2->freedom ; k++ )
        sp_hash_search(S,v1->rownum+j,v2->rownum+k,oo[j][k]);
  else 
    for ( j = 0 ; j < v1->freedom ; j++ )
      for ( k = 0 ; k < v2->freedom ; k++ )
        sp_hash_search(S,v2->rownum+k,v1->rownum+j,oo[j][k]);

}

#ifdef OLDHASH
/********************************************************************
* 
* function: hess_hash_sort()
*
* purpose: Create sorted list of hessian entries, row major order
*             Does 2-digit radix sort on row,col with count sort on each.
* input:    total_rows - upper bound for row,col.
* output:  *nptr - number of entries
*             **listptr - allocated list (temp_calloc)
*/
void hess_hash_sort(hessian_rows,nptr,listptr)
int hessian_rows;  /* rows in hessian */
int *nptr; /* for return of total entries */
struct hess_entry **listptr; /* list in, for return of sorted list */
{ int i;
  struct hess_entry *e;
  int *counts;
  int *starts;
  int sum,oldsum;

  counts = (int *)temp_calloc(2*hessian_rows+1,sizeof(int));
  starts = counts + hessian_rows;

  /* count entries in row */
  for ( i = 0, e = hashtable ; i < table_size ; i++, e++ )
     if ( e->row != HASHEMPTY ) counts[e->row]++;
  for ( i = 0 ; i < hessian_rows ; i++ )
     array[i].count = counts[i];

  /* get starting points of each row */
  for ( i = 0, sum = 0 ; i < hessian_rows  ; i++ )
  { oldsum = sum;
     sum += counts[i];
     counts[i] = starts[i] = oldsum;
  } 
  starts[hessian_rows] = sum;

  /* sort into row bins */
  for ( i = 0, e = hashtable ; i < table_size ; i++,e++ )
  { 
     struct hess_entry eorig;
     struct hess_entry etmp;
     if ( e->row == HASHEMPTY ) continue;
     if ( i < counts[e->row] ) continue;
     eorig = *e;
     e->row = HASHEMPTY;
     do
        { /* follow chain of replacements */
          int spot;
          int k;
          spot = counts[eorig.row];
          etmp = hashtable[spot];
          /* insertion sort into row */
          for ( k = spot ; k > starts[eorig.row] ; k-- )
          { if ( hashtable[k-1].col > eorig.col )
                hashtable[k] = hashtable[k-1];
             else break;
          }
          hashtable[k] = eorig;
          counts[eorig.row]++;
          eorig = etmp;
        }
        while ( eorig.row != HASHEMPTY ); /* stop when get back to empty */ 
    }
  *listptr = hashtable;
  *nptr = hashcount;
  temp_free((char*)counts);
  hash_per_row = 1 + (5*hashcount)/(4*S->A_rows+1); /* for next time */

  if ( !hessian_quiet_flag )
  { sprintf(msg,"Hessian entries: %d  Final hashtable size: %d next hash_per_row: %d.\n",hashcount, table_size,hash_per_row);
     outstring(msg);
     sprintf(msg,"Hash extra probes: %d\n",hash_extraprobes);
     outstring(msg);
  }
}


/********************************************************************
* 
* function: hess_hash_end()
*
* purpose: Deallocate hessian hash table.
*/
void hess_hash_end()
{
  if ( hashtable )  myfree((char*)hashtable);
  hashtable = NULL;
}
#endif
/********************************************************************
     End Hessian hash routines.
********************************************************************/


/**************************************************************
*
*  Function: hessian_auto()
*
*  Purpose:  Global minimization of energy by finding
*            critical point of quadratic approximation
*            to total energy function.
*            Using only lower left triangle of symmetric matrix.
*            Uses whatever sparse matrix solver is in effect at the moment.
*
*/

void hessian_auto() /* automatic hessian */
{ REAL old_energy = web.total_energy;
  REAL *rhs = NULL;
  REAL *X = NULL;
  struct linsys S;

  hmode = hessian_normal_flag;
  hessian_cleanup(); /* cleanup from previous round */
  hessian_init(&S,&rhs);
  hess_flag = 1; rhs_flag = 1; 
  hessian_fill(&S,&rhs); 
  sp_Hessian_solver(&S,rhs,&X);
  hessian_move((REAL)1.0,ACTUAL_MOVE,X);

  if ( check_increase_flag && 
         (web.total_energy > (1+100*machine_eps)*old_energy) )
  { kb_error(1596,"Hessian move would have raised energy.  Restoring coords.\n",
          WARNING);
    restore_coords(&saved,SAVE_IN_ATTR);
  }
  hessian_exit(X);
  free_system(&S);
  temp_free((char*)rhs);
  temp_free((char*)X); 
}

/**************************************************************
*
*  Function: hessian_seek()
*
*  Purpose: Same as hessian_auto(), but uses direction of minimum
*           as seek direction, and moves down it.
*
*  Return: best stepsize
*/

REAL hessian_seek(maxscale) /* automatic hessian */
REAL maxscale;
{ REAL best_scale;
  REAL *rhs=NULL,*X=NULL;
  struct linsys S;
 
  hmode = hessian_normal_flag;
  hessian_init(&S,&rhs); 
  hess_flag = 1; rhs_flag = 1; 
  hessian_fill(&S,&rhs); 
  if ( hessian_linear_metric_flag )
    linear_metric_setup(&S,&Met);
  else if ( web.area_norm_flag )
    star_metric_setup(&S,&Met);
  sp_Hessian_solver(&S,rhs,&X);
  best_scale = hessian_line_seek(&S,maxscale,X);
  hessian_exit(X);
  free_system(&S);
  if ( hessian_linear_metric_flag || web.area_norm_flag ) free_system(&Met);
  temp_free((char*)rhs);
  temp_free((char*)X);
  return best_scale;
}

/*****************************************************************************
*
* function: square_grad()
*
* purpose: calculates gradients for rhs and returns square of gradient 
*
*/

REAL square_grad()
{ REAL sum;
  vertex_id v_id;
  REAL *f;
  int i;
  int oldflag = min_square_grad_flag;
 
  calc_all_grads(CALC_VOLGRADS);
  volume_restore(1.,ACTUAL_MOVE);
  min_square_grad_flag = 0; /* so calc_force does regular forces */
  calc_force();
  min_square_grad_flag = oldflag;
  pressure_forces();
  fix_vertices(); /* project forces to constraints, etc */
  calc_lagrange();
  lagrange_adjust();
  vgrad_end();
  sum = 0.0;
  FOR_ALL_VERTICES(v_id)
  { REAL star;
     f = get_force(v_id);
     if ( hessian_linear_metric_flag )
        star = Met.apinv[get_vertex_vhead(v_id)->rownum];
     else star = 1.0;
     if ( hessian_normal_flag )
     { struct hess_verlist *v;              /* current  vertex */
        REAL r[MAXCOORD];
        REAL *rr = r;
        v = get_vertex_vhead(v_id);
        vec_mat_mul(f,v->proj,rr,SDIM,v->freedom);
        sum += dot(r,r,v->freedom)*star;
     }
     else sum += SDIM_dot(f,f)*star;
  }
  for ( i = 0 ; i < optparamcount ; i++ )
     sum += optparam[i].grad*optparam[i].grad;
  return sum;
}


/*****************************************************************************
*
* function: square_grad_forces()
*
* purpose: calculates gradients for rhs and returns square of gradient 
*          and calculates forces as hessian times rhs.
*
*/

void square_grad_forces()
{ REAL *rhs=NULL,*X=NULL;
  struct linsys S;
 
  hmode = hessian_normal_flag;
  hessian_init(&S,&rhs);
  hess_flag = 1; rhs_flag = 1; 
  hessian_fill(&S,&rhs); 

  /* ?
  if ( hessian_linear_metric_flag )
    linear_metric_setup(&S,&Met);
  else if ( web.area_norm_flag )
    star_metric_setup(&S,&Met);
  */

  X = (REAL*)temp_calloc(S.N,sizeof(REAL));
  sp_hessian_mult(&S,rhs,X);
  hessian_move(0,SET_VELOCITY,X);

  hessian_exit(X);
  free_system(&S);
  if ( hessian_linear_metric_flag || web.area_norm_flag ) free_system(&Met);
  temp_free((char*)rhs);
  temp_free((char*)X);
}

/*****************************************************************************
*
* function: hessian_line_seek()
*
* purpose: seek optimum motion in downhill direction, after downhill 
*  found.
*  save_coords() assumed to have been done in hessian_init();
*  step size bounded by web.max_scale.
*  If input parameter maxscale is nonzero, will obey.
*
* return: optimal stepsize
*/

REAL hessian_line_seek(S,maxscale,X)
struct linsys *S;
REAL maxscale;
REAL *X;
{ REAL scale0=0.0,scale1,scale2=0.0;
  REAL energy0,energy1,energy2=0.0;
  int seekcount = 0;
  int dirflag = 1; /* for positive or negative direction */
  REAL stepsize;
  REAL typescale;
  
  if ( maxscale <= 0.0 ) return 0.0;

  typescale = maxscale/100;
  
  ENTER_GRAPH_MUTEX;
 
  /* base energy; may be volume adjustments, so can't just take current values */
  hessian_move(0.0,ACTUAL_MOVE/*TEST_MOVE*/,X); 
  energy0 = min_square_grad_flag ? square_grad(): web.total_energy;
  restore_coords(&saved,SAVE_IN_ATTR);
  if ( itdebug )
  { sprintf(msg,"stepsize 0   energy0 %18.15g\n",energy0);
    outstring(msg);
  }

  hessian_move(typescale*1e-3,ACTUAL_MOVE/*TEST_MOVE*/,X);
  energy1 = min_square_grad_flag ? square_grad(): web.total_energy;
  restore_coords(&saved,SAVE_IN_ATTR);
  if ( itdebug )
  { sprintf(msg,"stepsize %f   energy1 %18.15g\n",typescale*1e-3,energy1);
    outstring(msg);
  }

  hessian_move(-typescale*1e-3,ACTUAL_MOVE/*TEST_MOVE*/,X);
  energy2 = min_square_grad_flag ? square_grad(): web.total_energy;
  restore_coords(&saved,SAVE_IN_ATTR);
  if ( itdebug )
  { sprintf(msg,"stepsize %f   energy2 %18.15g\n",-typescale*1e-3,energy2);
    outstring(msg);
  }
  LEAVE_GRAPH_MUTEX;

  if ( (energy1 >= energy0) && (energy2 >= energy0) )
     return 0.0;
  if ( energy1 > energy2 ) dirflag = -1;
  else dirflag = 1;

  ENTER_GRAPH_MUTEX;

  stepsize = scale1 = 0.499999999*typescale;
  hessian_move(dirflag*stepsize,ACTUAL_MOVE/*TEST_MOVE*/,X);
  energy1 = min_square_grad_flag ? square_grad(): web.total_energy;
  restore_coords(&saved,SAVE_IN_ATTR);
  if ( itdebug )
  { sprintf(msg,"stepsize %f   energy1 %18.15g\n",dirflag*stepsize,energy1);
    outstring(msg);
  }
  if ( energy1 < energy0 )
  { do
     { stepsize *= 2;
        if ( stepsize > maxscale ) { stepsize = maxscale; /*break;*/ }
        hessian_move(dirflag*stepsize,ACTUAL_MOVE/*TEST_MOVE*/,X);
        energy2 = min_square_grad_flag ? square_grad(): web.total_energy;
        scale2 = stepsize;
        restore_coords(&saved,SAVE_IN_ATTR);
        if ( itdebug )
        { sprintf(msg,"stepsize %f   energy2 %18.15g\n",dirflag*stepsize,energy2);
          outstring(msg);
        }
        if ( energy2 > energy1 )
        { stepsize /= 2; break; }
        else 
        { energy0 = energy1; scale0 = scale1;
          energy1 = energy2; scale1 = scale2;
        }
     } while ( stepsize < maxscale );
  }
  else /* energy1 >= energy0 */
  { seekcount = 0;
     do
     { if ( seekcount++ > 20 ) { stepsize = 0.0; break; }
        energy2 = energy1; scale2 = scale1;
        stepsize = scale2/2;
        hessian_move(dirflag*stepsize,ACTUAL_MOVE/*TEST_MOVE*/,X);
        energy1 = min_square_grad_flag ? square_grad(): web.total_energy;
        scale1 = stepsize;
        restore_coords(&saved,SAVE_IN_ATTR);
        if ( itdebug )
        { sprintf(msg,"stepsize %f   energy1 %18.15g\n",dirflag*stepsize,energy1);
          outstring(msg);
        }
     } while ( energy1 > energy0 );
  }
  if ( (stepsize > 0.0) && (energy2 > energy1) /*(stepsize < maxscale)*/ )
  { REAL denom;
    /* now quadratic interpolation for minimum energy */
    denom = energy0*(scale1-scale2)+energy1*(scale2-scale0)
                 + energy2*(scale0 - scale1);
    if ( denom == 0.0 ) stepsize = 0.0;
    else
    { stepsize = ((energy0-energy2)*scale1*scale1
            +(energy1-energy0)*scale2*scale2
            +(energy2-energy1)*scale0*scale0)/2/denom;
    }
    if ( stepsize < scale0 ) stepsize = scale0;
    if ( stepsize > scale2 ) stepsize = scale2;
  }
  else if ( stepsize < 0.0 ) stepsize = 0.0;
  else if ( stepsize > maxscale ) stepsize = maxscale;
  hessian_move(dirflag*stepsize,ACTUAL_MOVE,X);

  LEAVE_GRAPH_MUTEX;

  if ( min_square_grad_flag )
  { energy1 =  square_grad();
#ifdef LONGDOUBLE
     sprintf(msg,"square gradient: %3.*Lg\n",DPREC,energy1);
#else
     sprintf(msg,"square gradient: %3.15g\n",energy1);
#endif 
     outstring(msg);
  }

  return dirflag*stepsize;
} /* end hessian_line_seek() */

/***************************************************************************
*
* function: hessian_saddle()
*
* purpose: move downhill optimally if at saddle point
*/

void hessian_saddle(maxscale)
REAL maxscale;
{ REAL low;
  REAL stepsize;
  REAL *rhs=NULL,*X=NULL;
  struct linsys S;

  hmode = hessian_normal_flag;
  hessian_init(&S,&rhs);
  hess_flag = 1; rhs_flag = 1; 
  hessian_fill(&S,&rhs); 
  (*sp_AIJ_setup_func)(S.A_rows,&S);
  (*sp_constraint_setup_func)
         (web.skel[BODY].max_ord+1 + gen_quant_count,&S);
  if ( sp_ordering_func ) (*sp_ordering_func)(&S);
  if ( hessian_linear_metric_flag )
     { 
       linear_metric_setup(&S,&Met);
     }
  else if ( web.area_norm_flag )
     { 
       star_metric_setup(&S,&Met);
     }

  X = (REAL*)temp_calloc(S.N+S.concount,sizeof(REAL));
  low =  lowest_eigenpair(&S,X);
  if ( low >= 0.0 )
     { 
#ifdef LONGDOUBLE
        sprintf(msg,"Lowest eigenvalue %2.*Lg. Not a saddle point.\n",DPREC,low); 
#else
        sprintf(msg,"Lowest eigenvalue %2.15g. Not a saddle point.\n",low); 
#endif 
        outstring(msg);
        goto saddle_exit;
     }
#ifdef LONGDOUBLE
  sprintf(msg,"Lowest eigenvalue %2.*Lg\n",DPREC,low);
#else
  sprintf(msg,"Lowest eigenvalue %2.15g\n",low);
#endif 
  outstring(msg);
  stepsize = hessian_line_seek(&S,maxscale,X);
  sprintf(msg,"stepsize %g\n",(DOUBLE)stepsize);
  outstring(msg);
#ifdef LONGDOUBLE
  sprintf(msg,"1.  energy: %17.*Lg \n", DPREC,web.total_energy);
#else
  sprintf(msg,"1. %s: %17.15g energy: %17.15g \n",
                areaname,web.total_area,web.total_energy);
#endif 
  outstring(msg);
  update_display();
  reset_conj_grad();
saddle_exit:
  hessian_exit(X); /* cleanup */
  free_system(&S);
  if ( hessian_linear_metric_flag || web.area_norm_flag ) free_system(&Met);
  temp_free((char*)rhs);
  temp_free((char*)X);
} /* end hessian_saddle() */

/***************************************************************************
* 
* function: print_hessian_menu()
*
* purpose: Prints full menu in hessian_menu command.
*/
void print_hessian_menu()
{
  outstring("\n");
  outstring("1. Fill in hessian matrix.\n");
  outstring("2. Fill in right side. (Do 1 first)\n");
  outstring("3. Solve. (Do 2 first)\n");
  outstring("4. Move. (Do 3,V,E,F,C,B or X  first)\n");
  outstring("7. Restore original coordinates.\n");
  outstring("9. Toggle debugging. (Don't do this!)\n");
  outstring("B. Chebychev (For Hessian solution ).\n");
  outstring("C. Chebychev (For most negative eigenvalue eigenvector).\n");
  outstring("E. Lowest eigenvalue. (By factoring. Do 1 first)\n");
  outstring("F. Lowest eigenvalue. (By conjugate gradient. Do 1 first)\n");
  outstring("L. Lanczos. (Finds eigenvalues near probe value. )\n");
  outstring("R. Lanczos with selective reorthogonalization.\n");
  outstring("Z. Ritz subspace iteration for eigenvalues. (Do 1 first)\n");
  outstring("X. Pick Ritz vector for motion. (Do Z first)\n");
  outstring("P. Eigenvalue probe. (By factoring. Do 1 first)\n");
  outstring("V. Eigenvalue probe with eigenvector. (By factoring. Do 1 first)\n");
  outstring("S. Seek along direction. (Do 3,V,E,F,C,B or X first)\n");
  outstring("Y. Toggle YSMP/alternate minimal degree factoring.\n");
  outstring("U. Toggle Bunch-Kaufman version of min deg.\n");
  outstring("M. Toggle projecting to global constraints in move. (Leave this alone!)\n");
  outstring("G. Toggle minimizing square gradient in seek.\n");
  outstring("D. Dump Hessian to text file.\n");
  outstring("=. Execute Evolver commands in subprompt.\n");
#ifdef MPI_EVOLVER
  outstring("A. Build corona (MPI version only).\n");
  outstring("J. Dissolve corona (MPI version only).\n");
  outstring("I. Free discard lists (MPI version only).\n");
#endif
  outstring("0. Exit hessian.\n");
}

/***********************************************************************
*
*  function: hessian_menu()
*
*  purpose: give user more interactive control over Hessian move
*     and debugging.
*/

void hessian_menu()
{   char response[30];
    int left_flag = 0; /* whether hessian filled */
    int right_flag = 0; /* whether rhs filled in */
    int dir_flag = 0 ; /* whether direction filled in */
    REAL stepsize;
    REAL *rhs = NULL,*X = NULL;
    struct linsys S;
    
    hessian_legal();
    save_coords(&saved,SAVE_IN_ATTR);

    memset(&S,0,sizeof(struct linsys)); 
    hess_debug = 0;
    hmode = hessian_normal_flag;
    for(;;)
     {
#ifdef MPI_EVOLVER
        prompt("Choice(?,1,2,3,4,7,9,B,C,E,F,L,R,Z,X,P,V,S,Y,U,M,G,D,A,=,0,q): ",
		 response,sizeof(response));
#else
        prompt("Choice(?,1,2,3,4,7,9,B,C,E,F,L,R,Z,X,P,V,S,Y,U,M,G,D,=,0,q): ",
		 response,sizeof(response));
#endif
        switch ( toupper(response[0]) )
          { case '?': print_hessian_menu(); break;
             case '1': 
                 hessian_cleanup(); /* cleanup from previous round */
                 hessian_init(&S,&rhs);
                 hess_flag = 1; rhs_flag = 1; 
                 hessian_fill(&S,&rhs); 
                 X = (REAL*)temp_calloc(S.N,sizeof(REAL));
                 (*sp_AIJ_setup_func)(S.A_rows,&S);
                 (*sp_constraint_setup_func) (
                           web.skel[BODY].max_ord+1 + gen_quant_count,&S);
                 if ( sp_ordering_func ) (*sp_ordering_func)(&S);
                 if ( hessian_linear_metric_flag )
                 {
                    linear_metric_setup(&S,&Met);
                 }
                 else if ( web.area_norm_flag )
                 { 
                    star_metric_setup(&S,&Met);
                 }
                 left_flag = 1;
                 break;

             case '2': 
                  if ( left_flag == 0 )
                     { outstring("Error: Must do step 1 first.\n"); break; }
                  if ( hess_debug )
                  { int i;
                    for ( i = 0 ; i < S.N ; i++ )
                      printf("B[%d]  %20.15f\n",i,(DOUBLE)(rhs[i]));
                  }
                  rhs_norm = dot(rhs,rhs,S.N);
#ifdef LONGDOUBLE
                  sprintf(msg,"RHS norm: %20.*Lg\n",DPREC, rhs_norm);
#else
                  sprintf(msg,"RHS norm: %20.15g\n", rhs_norm);
#endif 
                  outstring(msg);
                  right_flag = 1;
                  break;
                
             case '3': 
                 if ( left_flag == 0 )
                     { outstring("Error: Must do step 1 first.\n"); break; }
                 if ( right_flag == 0 )
                     { outstring("Error: Must do step 2 first.\n"); break; }
                 if ( !(S.flags & S_FACTORED) ) sp_factor(&S);
                 if ( !(S.flags & S_PROJECTED)) (*sp_hess_project_setup_func)(&S);
                 sp_hessian_solve(&S,rhs,X,SET_PRESSURE);
                 outstring("Motion found.  Pressures adjusted.\n");
                 dir_flag = 1;
                 break;

             case '4': 
                  if ( dir_flag == 0 )
                  { outstring("Error: Must do step 3,V,E,F,C,B or X first.\n"); 
                    break; 
                  }
                 prompt("Step size: ",response,sizeof(response));
                 stepsize = atof(response);
                 hessian_move(stepsize,ACTUAL_MOVE,X);
#ifdef LONGDOUBLE
                 sprintf(msg,"1. energy: %*.*Lg \n",DWIDTH,DPREC,web.total_energy);
#else
                 sprintf(msg,"1. %s: %17.15f energy: %17.15f \n",
                        areaname,web.total_area,web.total_energy);
#endif 
                 outstring(msg);
                 update_display();
                 break;

             case '7': restore_coords(&saved,SAVE_IN_ATTR); update_display(); break;

             case '8': 
                       if ( left_flag == 0 )
                         { outstring("Error: Must do step 1 first.\n"); break; }
                       write_hessian(&S); break;

             case '=': 
                   hessian_subshell_flag = 1;
                   subshell_depth++;
                   setjmp(jumpbuf[subshell_depth]);
                   /* command read and execute loop */
                   exec_commands(NULL,"hessian_menu subcommand: "); 
                   subshell_depth-- ;
                   hessian_subshell_flag = 0;
                   break;

             case 'Q': case '0': case 0: 
                   hessian_exit(X); 
                   free_system(&S);
                   if ( hessian_linear_metric_flag || web.area_norm_flag )
                       free_system(&Met);
                   if ( rhs ) temp_free((char*)rhs);
                   if ( X ) temp_free((char*)X);
                   return;
             case '9': hess_debug = !hess_debug;
                       if ( hess_debug ) 
                             outstring("Hessian debug ON.\n");
                       else outstring("Hessian debug OFF.\n");
                       break;

             case 'K': /* experimdntal multigrid */
                 if ( left_flag == 0 )
                     { outstring("Error: Must do step 1 first.\n"); break; }
                 if ( right_flag == 0 )
                     { outstring("Error: Must do step 2 first.\n"); break; }
                 do_multigrid(&S);
                 break;

             case 'Z': /* ritz */
              { 
                 REAL probe;
                 if ( left_flag == 0 )
                     { outstring("Error: Must do step 1 first.\n"); break; }
                 prompt("Ritz probe value: ",response,sizeof(response));
                 probe = atof(response);
                 prompt("Number of eigenvalues: ",response,sizeof(response));
                 ritzdim = atoi(response);
                 if ( ritzvecs ) free_matrix(ritzvecs);
                 ritzvecs = dmatrix(0,ritzdim-1,0,S.N+S.concount);
                 do_ritz(&S,probe,ritzdim,ritzvecs);
                 ritz_done_flag = 1;
                 break;
              }
             case 'X': /* pick ritz vector for motion */
              { int ritznum;
                char *spot;
                 int i;
                 if ( ritz_done_flag == 0 )
                     { outstring("Error: Must do step Z first.\n"); break; }
                 sprintf(msg,"Eigenvector number (1 to %d): ",ritzdim);
                 prompt(msg,response,sizeof(response));
                 /* parse response for possible linear combo of vectors */
                 for ( spot = response ; *spot ; spot++ ) 
                   *spot = tolower(*spot);
                 if ( strchr(response,'v') )
                 { spot = response;
                   memset((char*)X,0,S.N*sizeof(REAL));
                   while ( strchr(spot,'v') )
                   { REAL coeff;
                     int sign = 1;
                     while ( *spot==' ' || *spot=='+' || *spot=='-' )
                     { if ( *spot=='-' ) sign = -sign;
                       spot++;
                     }
                     coeff = sign*atof(spot);
                     spot  = strchr(spot,'v')+1;
                     ritznum = atoi(spot); 
                     while ( isdigit(*spot) ) spot++;
                     if ( (ritznum < 0) || (ritznum > ritzdim) )
                     { outstring("Error: Invalid eigenvector number.\n"); 
                       break; 
                     }
                     ritznum--;
                     for ( i = 0 ; i < S.N ; i++ )
                       X[i] += coeff*ritzvecs[ritznum][i];
                   }
                   
                 }
                 else
                 { ritznum = atoi(response);
                   if ( (ritznum < 0) || (ritznum > ritzdim) )
                   { outstring("Error: Invalid eigenvector number.\n"); break; }
                   ritznum--;
                   for ( i = 0 ; i < S.N ; i++ ) X[i] = ritzvecs[ritznum][i];
                 }
                 dir_flag = 1;
                 break;
              }
             case 'L': 
             { int nprint,i;
                REAL evalues[NPRINT];
                 if ( left_flag == 0 )
                     { outstring("Error: Must do step 1 first.\n"); break; }
                 if ( !(S.flags & S_FACTORED) ) sp_factor(&S);
                 if ( !(S.flags & S_PROJECTED)) (*sp_hess_project_setup_func)(&S);
                 nprint = lanczos(&S,KRYLOVDIM,evalues,NPRINT);
                 /* list, ones near probe value */
                 for ( i = 0 ; i < nprint ; i++ )
                 {
#ifdef LONGDOUBLE
                    sprintf(msg,"%d  %*.*Lf\n",i+1,DWIDTH,DPREC,evalues[i]);
#else
                    sprintf(msg,"%d  %20.15f\n",i+1,evalues[i]);
#endif 
                    outstring(msg);
                 }
              }
                 break;

             case 'R': 
             { int nprint,i;
                REAL evalues[NPRINT];
                 if ( left_flag == 0 )
                     { outstring("Error: Must do step 1 first.\n"); break; }
                 if ( !(S.flags & S_FACTORED) ) sp_factor(&S);
                 if ( !(S.flags & S_PROJECTED)) (*sp_hess_project_setup_func)(&S);
                 nprint = selective_lanczos(&S,KRYLOVDIM,evalues,NPRINT);
                 /* list, ones near probe value */
                 for ( i = 0 ; i < nprint ; i++ )
                 { 
#ifdef LONGDOUBLE
                    sprintf(msg,"%d  %*.*Lf\n",i+1,DWIDTH,DPREC,evalues[i]);
#else
                    sprintf(msg,"%d  %20.15f\n",i+1,evalues[i]);
#endif 
                    outstring(msg);
                 }
              }
              break;

             case 'C': 
                 if ( left_flag == 0 )
                     { outstring("Error: Must do step 1 first.\n"); break; }
                 chebychev_ev(&S);
                 dir_flag = 1;
                 break;

             case 'B': 
                 if ( left_flag == 0 )
                     { outstring("Error: Must do step 1 first.\n"); break; }
                 chebychev_hess(&S);
                 dir_flag = 1;
                 break;

             case 'P':  /* probe for eigenvalues */
                 if ( left_flag == 0 )
                     { outstring("Error: Must do step 1 first.\n"); break; }
                 bk_eigenprobe(&S);
                 break;

             case 'V':  /* probe for eigenvalues, with eigenvector */
                 if ( left_flag == 0 )
                     { outstring("Error: Must do step 1 first.\n"); break; }
                 bk_inverse_it(&S,X);
                 dir_flag = 1;
                 break;

             case 'E':  /* lowest eigenvalue */ 
              { REAL low;
                 if ( left_flag == 0 )
                     { outstring("Error: Must do step 1 first.\n"); break; }
                 low = lowest_eigenpair(&S,X); 
#ifdef LONGDOUBLE
                 sprintf(msg,"Lowest eigenvalue %2.*Lg\n",DPREC,low);
#else
                 sprintf(msg,"Lowest eigenvalue %2.15g\n",low);
#endif 
                 outstring(msg);
                 dir_flag = 1;
                 break;
              }
             case 'd':  /* lowest eigenvalue, by old conjugate gradient */ 
              { REAL low;
                 if ( left_flag == 0 )
                     { outstring("Error: Must do step 1 first.\n"); break; }
                 low = old_cg_lowest_eigenpair(&S,X); 
#ifdef LONGDOUBLE
                 sprintf(msg,"Lowest eigenvalue %2.*Lg\n",DPREC,low);
#else
                 sprintf(msg,"Lowest eigenvalue %2.15g\n",low);
#endif 
                 outstring(msg);
                 dir_flag = 1;
                 break;
              }
             case 'F':  /* lowest eigenvalue, by conjugate gradient */ 
              { REAL low;
                 if ( left_flag == 0 )
                     { outstring("Error: Must do step 1 first.\n"); break; }
                 low = cg_lowest_eigenpair(&S,X); 
#ifdef LONGDOUBLE
                 sprintf(msg,"Lowest eigenvalue %2.*Lg\n",DPREC,low);
#else
                 sprintf(msg,"Lowest eigenvalue %2.15g\n",low);
#endif 
                 outstring(msg);
                 dir_flag = 1;
                 break;
              }
             case 'S':
                 if ( dir_flag == 0 )
                     { outstring("Error: Must get direction first.\n"); break; }
                 stepsize = hessian_line_seek(&S,10.0,X);
                 sprintf(msg,"stepsize %g\n",(DOUBLE)stepsize);
                 outstring(msg);
#ifdef LONGDOUBLE
                 sprintf(msg,"1.  energy: %*.*Lg \n",DWIDTH,DPREC,web.total_energy);
#else
                 sprintf(msg,"1. %s: %17.15g energy: %17.15g \n",
                        areaname,web.total_area,web.total_energy);
#endif 
                 outstring(msg);
                 update_display();
                 break;

             case 'Y': /* toggle YSMP */
                 if ( ysmp_flag != MINDEG_FACTORING )
                 { ysmp_flag = MINDEG_FACTORING;
                    sp_mul_func = bk_mul;
                    sp_AIJ_setup_func= bk_AIJ_setup;
                    sp_constraint_setup_func = bk_constraint_setup;
                    sp_hess_project_setup_func= BK_hess_project_setup;
                    sp_factor_func = xmd_factor;
                    sp_solve_func = xmd_solve;
                    sp_solve_multi_func = xmd_solve_multi;
                    sp_ordering_func = NULL;
                    sp_CHinvC_func = sp_CHinvC;
                    outstring("Using alternate minimal degree factoring.\n");
                    if ( left_flag ) outstring("Must do step 1 again.\n");
                    dir_flag = left_flag = right_flag = 0;
                 }
                 else
                 { ysmp_flag = YSMP_FACTORING;
                    sp_mul_func = bk_mul;
                    sp_AIJ_setup_func= bk_AIJ_setup;
                    sp_constraint_setup_func = bk_constraint_setup;
                    sp_hess_project_setup_func= BK_hess_project_setup;
                    sp_factor_func = ysmp_factor;
                    sp_CHinvC_func = NULL;
                    sp_solve_func = ysmp_solve;
                    sp_solve_multi_func = ysmp_solve_multi;
                    sp_ordering_func = NULL;
                    outstring("Using YSMP.\n");
                 }
                 break;

             case 'U': /* toggle BK version of min degree */
                 BK_flag = !BK_flag;
                 if ( BK_flag )
                    outstring("Using Bunch-Kaufman version of minimal degree.\n");
                 else
                 outstring("Using non-Bunch-Kaufman version of minimal degree.\n");
                 break;

             case 'M':
                 hess_move_con_flag = !hess_move_con_flag;
                 if ( hess_move_con_flag )
                    outstring("Projecting global constraints in move.\n");
                 else
                 outstring("Not projecting global constraints in move.\n");
                 break;
             case 'G':
                 min_square_grad_flag = !min_square_grad_flag;
                 if ( min_square_grad_flag )
                    outstring("Minimizing square gradient in seek.\n");
                 else
                    outstring("Minimizing regular energy in seek.\n");
                 break;
             case 'D': /* dump Hessian, 1-based indexing */
                 { FILE *fd;
                   char hname[200];
                   size_t baselength;
                   int i,j,k;
gethname:
                   outstring("Hessian dump. Note indexing is 1-based.\n");
                   prompt("Enter path and base name of Hessian dump files: ",
                      hname,200);
                   if ( hname[0] == 0 ) break;
                   baselength = strlen(hname);
                   strcat(hname,"_H.dat");
                   fd = fopen(hname,"w");
                   if ( fd == NULL )
                   { perror(hname); goto gethname; }
                   sprintf(msg,
            "Dumping Hessian upper triangle in row col value format to %s.\n",
                       hname);
                   outstring(msg);
                   for ( i = 0 ; i < S.N ; i++ )
                     for ( j = S.IA[i]-A_OFF ; j < S.IA[i+1]-A_OFF ; j++ )
                     { fprintf(fd,"%6d %6d %20.15f\n",i+1,S.JA[j]-A_OFF+1,S.A[j]);
                     }
                   fclose(fd);
                   if ( ysmp_flag != MINDEG_FACTORING )
                   { outstring("Further dumping of factors in minimal degree mode (ysmp off) only.\n");
                     break;
                   }
                   if ( !S.LA )
                   { outstring("Need to do step 3, P, V, or Z before dumping factors.\n");
                     break;
                   }
                   strcpy(hname+baselength,"_L.dat");
                   sprintf(msg,
            "Dumping L factor in row col value format to %s.\n",hname);
                   outstring(msg);
                   if ( hname[0] == 0 ) break;
                   fd = fopen(hname,"w");
                   if ( fd == NULL )
                   { perror(hname); break; }
                   for ( i = 0 ; i < S.N ; i++ )
                   { fprintf(fd,"%6d %6d %20.15f\n",i+1,i+1,1.0);
                     for ( j = S.LIA[i]+S.psize[i], k = S.LIJA[i]+S.psize[i] ;
                        j < S.LIA[i+1] ; j++,k++ )
                     { fprintf(fd,"%6d %6d %20.15f\n",i+1,S.LJA[k]+1,S.LA[j]);
                     }
                   }
                   fclose(fd);
                   strcpy(hname+baselength,"_D.dat");
                   sprintf(msg,
            "Dumping diagonal factor in row col value format to %s.\n",hname);
                   outstring(msg);
                   if ( hname[0] == 0 ) break;
                   fd = fopen(hname,"w");
                   if ( fd == NULL )
                   { perror(hname); break; }
                   for ( i = 0 ; i < S.N ; i++ )
                   { j = S.LIA[i]; k = S.LIJA[i]; 
                     if ( S.psize[i] == 1 )
                     { fprintf(fd,"%6d %6d %20.15f\n",i+1,S.LJA[k]+1,S.LA[j]);
                     }
                     else
                     { fprintf(fd,"%6d %6d %20.15f\n",i+1,S.LJA[k]+1,S.LA[j]);
                       fprintf(fd,"%6d %6d %20.15f\n",i+1,S.LJA[k+1]+1,S.LA[j+1]);
                       fprintf(fd,"%6d %6d %20.15f\n",S.LJA[k+1]+1,i+1,S.LA[j+1]);
                       i++;
                       j = S.LIA[i]; k = S.LIJA[i]; 
                       fprintf(fd,"%6d %6d %20.15f\n",i+1,S.LJA[k]+1,S.LA[j]);
                     }
                   }
                   fclose(fd);
                   if ( !augmented_hessian_mode )
                   { strcpy(hname+baselength,"_C.dat");
                     sprintf(msg,
            "Dumping constraint matrix in row col value format to %s.\n",hname);
                     outstring(msg);
                     fd = fopen(hname,"w");
                     if ( fd == NULL )
                     { perror(hname); break; }
                     for ( i = 0 ; i < S.CN ; i++ )
                     { if ( S.CIA )
                       for ( j = S.CIA[i] ; j < S.CIA[i+1] ; j++ )
                       { fprintf(fd,"%6d %6d %20.15f\n",i+1,S.CJA[j]+1,S.CA[j]);
                       }
                       else /* dense */
                       for ( j = 0 ; j < S.N ; j++ )
                         if ( S.C[i][j] )
                           fprintf(fd,"%6d %6d %20.15f\n",i+1,j+1,S.C[i][j]);
                     }
                     fclose(fd);
                   }
                   strcpy(hname+baselength,"_P.dat");
                   sprintf(msg,
            "Dumping permutation in one line format to %s.\n",hname);
                   outstring(msg);
                   fd = fopen(hname,"w");
                   if ( fd == NULL )
                   { perror(hname); break; }
                   for ( i = 0 ; i < S.N ; i++ )
                   { fprintf(fd,"%6d",S.P[i]+1);
                   }
                   fprintf(fd,"\n");
                   fclose(fd);
                 }
               break;
#ifdef MPI_EVOLVER
             case 'A' : 
   				 mpi_hessian_fill_distrib(Mpi_RHS_MOVE); 
   		printf("Done fill.  Enter shift: ");
              scanf("%lf",&S.lambda);
   				 mpi_hessian_factor_distrib(&S);
   				 printf("Done factoring. Inertia: pos %d, neg %d, zero %d\n", 
                   S.pos,S.neg,S.zero); 

   				 mpi_solve_distrib(Mpi_LINSYS_HESSIAN, Mpi_RHS_MOVE, Mpi_SOLUTION_MOVE);
                 mpi_hessian_distrib_move(1.0,ACTUAL_MOVE);
#ifdef RANDOMRHS
   				 mpi_create_random_rhs();
   				 printf("Created random rhs\n");
   				 mpi_solve_distrib(Mpi_LINSYS_HESSIAN, Mpi_RHS_RANDOM, Mpi_SOLUTION_RANDOM);
                 printf("Solved random rhs\n");
                 if ( S.lambda == 0.0 )
                 { mpi_mult_hess_distrib(); /* check */
                   printf("Multiplied solution by original\n");
                 }
#endif
                 
   				 break;
#endif
             default : outstring("Illegal choice.\n");
          }
     }
} /* end hessian_menu() */

/**************************************************************************
*
*  function: hessian_legal()
*
*  purpose: see if Hessian is legal for this surface.
*/
void hessian_legal()
{ int i;
  body_id b_id;

  if ( !quantities_only_flag )
  {
     if ( square_curvature_flag )
     { if ( auto_convert_flag ) convert_to_quantities(); else
       kb_error(2077,"The type of squared mean curvature in effect does not permit Hessian.\n", RECOVERABLE);
     }

     if ( optparamcount > 0 ) 
     { if ( auto_convert_flag ) convert_to_quantities(); else
     kb_error(2078,
        "Hessian for optimizing parameters requires convert_to_quantities.\n",
        RECOVERABLE);
     }

     if ( web.representation == SIMPLEX ) 
     {  if ( auto_convert_flag ) convert_to_quantities(); else
        kb_error(1597,"Hessian for simplex model requires convert_to_quantities.\n", RECOVERABLE);
     }

    for ( i = 0 ; i < web.maxcon ; i++ )
     if ( get_constraint(i)->attr & CON_CONTENT )
     { if ( auto_convert_flag ) { convert_to_quantities(); break; }
        else kb_error(1843,"Hessian for constraint content requires convert_to_quantities.\n",RECOVERABLE);
     }
 
    if ( web.symmetry_flag )
    FOR_ALL_BODIES(b_id)
    { if ( get_battr(b_id) & FIXEDVOL )
      { if ( auto_convert_flag ) { convert_to_quantities(); break; }
         else 
            kb_error(1844,"Cannot do torus body volumes with hessian. Do convert_to_quantities.\n",RECOVERABLE);
      }
    }

     if ( web.modeltype != LINEAR )
     { 
          if ( auto_convert_flag ) convert_to_quantities(); else
          kb_error(1598,"Hessian for non-LINEAR models requires convert_to_quantities.\n",
            RECOVERABLE);
     }
     if ( web.representation == STRING ) 
     {  if ( auto_convert_flag ) convert_to_quantities(); else
        kb_error(1599,
          "Hessian for string model requires convert_to_quantities.\n", 
             RECOVERABLE);
     }

     if ( web.gravflag && (web.grav_const != 0.0))
     {  if ( auto_convert_flag ) convert_to_quantities(); else
        kb_error(1601,
         "Hessian method for gravity requires convert_to_quantities.\n",
          RECOVERABLE);
     }
     if ( web.pressflag ||  web.pressure_flag )
     {  if ( auto_convert_flag ) convert_to_quantities(); else
        kb_error(1602,"Hessian with pressure in energy requires convert_to_quantities.\n", RECOVERABLE);
     }

     if ( web.wulff_flag )
     kb_error(1603,"Cannot do Hessian method with crystalline integrand.\n", RECOVERABLE);

     if ( web.convex_flag )
        kb_error(1604,"Cannot do gap energy Hessian, since it would need third derivatives.\n", RECOVERABLE);

     if ( web.metric_flag )
     {  if ( auto_convert_flag ) convert_to_quantities(); else
        kb_error(1605,"Hessian with Riemannian metric requires convert_to_quantities.\n", RECOVERABLE);
     }
   }

  if ( count_fixed_vol() && !pressure_set_flag && !web.pressure_flag )
      kb_error(1606,"Pressures invalid. Do regular iteration before Hessian to set pressures.\n",
        RECOVERABLE);

}

/*****************************************************************************
*
* function: hessian_init()
*
* purpose: Allocate Hessian
*/

void hessian_init(S,rhs)
struct linsys *S;
REAL **rhs;  /* right side, allocated by hessian_init */
{
  vertex_id v_id;
  int j,i,gam,a,b,m,n;
  struct hess_verlist *v;              /* current  vertex */
  int currow = 0;
  MAT2D(grad,MAXCOORD,MAXCOORD);
  MAT2D(gradcopy,MAXCOORD,MAXCOORD);
  MAT2D(cc,MAXCOORD,MAXCOORD);
  MAT2D(pp,MAXCOORD,MAXCOORD);
  MAT2D(qq,MAXCOORD,MAXCOORD);
  MAT2D(gg,MAXCOORD,MAXCOORD);
  MAT3D(second,MAXCOORD,MAXCOORD,MAXCOORD);
  REAL dummy;
  int total_proj,vproj_count,total_conhess,vproj_alloc,conhess_alloc;
  int vproj_spot,vcount;
  REAL ***v_normal = NULL;

  memset(S,0,sizeof(struct linsys));

  hessian_legal();

  /* in case this doesn't work */
  if ( saved.coord == NULL ) 
	  local_save_coords(&saved,SAVE_IN_ATTR);  

  /* vertex attribute to hold vhead index */
  vhead_attr = find_attribute(VERTEX,VHEAD_ATTR_NAME);
  if ( vhead_attr < 0 )
  { int one = 1;
    vhead_attr = add_attribute(VERTEX,VHEAD_ATTR_NAME,INTEGER_TYPE,
                    0,&one,0,NULL);
  }

  vhead_count = web.skel[VERTEX].max_ord+1;
  vhead_count += optparamcount;
  vhead_count += gen_quant_count;
  if ( !everything_quantities_flag ) 
  { vhead_count += web.skel[BODY].max_ord+1;
    bhead_attr = find_attribute(BODY,BHEAD_ATTR_NAME);
    if ( bhead_attr < 0 )
    { int one = 1;
      bhead_attr = add_attribute(BODY,BHEAD_ATTR_NAME,INTEGER_TYPE,
                      0,&one,0,NULL);
    }
  }


  vhead = (struct hess_verlist*)temp_calloc(vhead_count,
        sizeof(struct hess_verlist));
  if ( hmode == NORMAL_MOTION ) v_normal = dmatrix3(vhead_count,SDIM,SDIM);
      /* v_normal will use vhead_attr as index */

  /* populate vertex list and count degrees of freedom */
  total_proj = 0;    /* count columns needed */
  vproj_count = 0;  /* number of vertices needing projection */
  total_conhess = 0;    /* count rows needed */
  vcount = 0;
  MFOR_ALL_VERTICES(v_id)
  { conmap_t * conmap;
    int oncount;
    int kk;

    set_vertex_vhead(v_id,vcount); /* new way */
    vcount++;
    if ( get_vattr(v_id) & FIXED ) continue;
    v = get_vertex_vhead(v_id);   /* old way */
    v->v_id = v_id;

    if ( hmode == SINGLE_DEGREE )
        v->freedom = 1;
    else if ( get_vattr(v_id) & BOUNDARY )
    { struct boundary *bdry = get_boundary(v_id);
      v->freedom = bdry->pcount;
      total_proj += v->freedom;
      vproj_count++;
      total_conhess += v->freedom;
    }
    else /* possible level set constraints */
    { 
      if ( hmode == NORMAL_MOTION ) 
      { REAL **norm = get_vertex_v_normal(v_id);
        kk = new_calc_vertex_normal(v_id,norm);
        kk = project_vertex_normals(v_id,norm,kk);
        if ( kk < SDIM )
        { v->freedom = kk;
          total_proj += kk;
          vproj_count++;
        }
        else v->freedom = SDIM;
      }
      else /* v-proj still identity */
        v->freedom = SDIM;

      conmap = get_v_constraint_map(v_id);
      if ( conmap[0] )
      { /* calculate size of projection matrix to fewer degrees of freedom */
        for ( j = 1, oncount = 0; j <= (int)conmap[0] ; j++ )
        { if ( !(conmap[j] & CON_HIT_BIT) ) continue;
          oncount++;
        }

        if ( v->freedom + oncount > SDIM ) 
        { 
          total_proj += v->freedom;
          vproj_count++;
        }
        /* now constraint hessian */
        if ( oncount ) total_conhess += v->freedom;
      }
    }
  } /* end FOR_ALL_VERTICES first time through */


  vproj_base = (REAL**)mycalloc(vproj_count*SDIM,sizeof(REAL*));
  vproj_space = (REAL*)mycalloc(total_proj*SDIM,sizeof(REAL));;
  conhess_base = dmatrix3(total_conhess,SDIM-1,SDIM);
  vproj_spot = vproj_alloc = conhess_alloc = 0;

  /* populate vertex list vproj and conhess */
  MFOR_ALL_VERTICES(v_id)
  { conmap_t * conmap;
    int oncount;

    v = get_vertex_vhead(v_id);
/*    v->rownum  = currow; */
    v->slant = 1.0;
    if ( v->freedom == 0 ) continue;
    if ( hmode == SINGLE_DEGREE )
     {} 
    else if ( get_vattr(v_id) & BOUNDARY )
    { struct boundary *bdry = get_boundary(v_id);
      v->proj = vproj_base + vproj_alloc; vproj_alloc += SDIM;
      for ( j = 0 ; j < SDIM ; j++ ) 
      { v->proj[j] = vproj_space + vproj_spot;
        vproj_spot+= v->freedom;
      }
      v->conhess = conhess_base + conhess_alloc;
      conhess_alloc += v->freedom;
      for ( j = 0 ; j < SDIM ; j++ )
      { eval_second(bdry->coordf[j],get_param(v_id),bdry->pcount,&dummy,
            v->proj[j],second[j],v_id);
        for ( m = 0 ; m < bdry->pcount ; m++ )
          for ( n = 0 ; n < bdry->pcount ; n++ )
             v->conhess[m][n][j] = second[j][m][n];
      }
    }
    else /* possible level set constraints */
    { 
      if (v->freedom < SDIM)  /* copy over normal basis */
      { REAL **vn = get_vertex_v_normal(v_id);
        v->proj = vproj_base + vproj_alloc; vproj_alloc += SDIM;
        for ( j = 0 ; j < SDIM ; j++ ) 
        { v->proj[j] = vproj_space + vproj_spot;
          vproj_spot += v->freedom;
        }
        for ( j = 0 ; j < SDIM ; j++ )
          for ( i = 0 ; i < v->freedom ; i++ )
            v->proj[j][i] = vn[i][j];
      }
      conmap = get_v_constraint_map(v_id);     
      oncount = 0;
      if ( conmap[0] )
      { /* calculate projection matrix to fewer degrees of freedom */
        for ( j = 1; j <= (int)conmap[0] ; j++ )
        { struct constraint *constr;
          if ( !(conmap[j] & CON_HIT_BIT) ) continue;
          constr = get_constraint(conmap[j]);
          eval_second(constr->formula,get_coord(v_id),SDIM,
            &dummy, grad[oncount],second[oncount],v_id);  
           /* constraint value and derivs and hessian */
          oncount++;
        }
        oncount = gram_schmidt(grad,oncount,SDIM);
        if ( (v->freedom < SDIM) && (oncount > 0) ) /* normal or something */
        { 
          /* project basis to constraints */
          mat_mul_tr(grad,grad,gg,oncount,SDIM,oncount);
          mat_inv(gg,oncount);
          mat_mult(grad,v->proj,pp,oncount,SDIM,v->freedom);
          mat_mult(gg,pp,qq,oncount,oncount,v->freedom);
          tr_mat_mul(grad,qq,pp,oncount,SDIM,v->freedom);
          for ( i = 0 ; i < SDIM ; i++ )
           for ( j = 0 ; j < v->freedom ; j++ )
             v->proj[i][j] -= pp[i][j];
        
        }
        else if ( oncount > 0 )
        { v->freedom = SDIM - oncount;
          if ( v->freedom <= 0 ) continue;
          v->proj = vproj_base + vproj_alloc; vproj_alloc += SDIM;
          for ( j = 0 ; j < SDIM ; j++ ) 
          { v->proj[j] = vproj_space + vproj_spot;
            vproj_spot += v->freedom;
          }
          for ( m = 0 ; m < oncount ; m++ )
            for ( n = 0 ; n < SDIM ; n++ ) gradcopy[m][n] = grad[m][n];
          kernel_basis(gradcopy,v->proj,oncount,SDIM); 
        }
      }
      /* orthonormalize proj */
      if ( v->proj )
      { for ( i = 0 ; i < v->freedom ; i++ )
        { /* subtract previous components */
          REAL sum;
          for ( j = 0 ; j < i ; j++ ) 
          { for ( n = 0, sum = 0.0 ; n < SDIM ; n++ ) 
               sum += v->proj[n][j]*v->proj[n][i];
            for ( n = 0 ; n < SDIM ; n++ ) 
               v->proj[n][i] -= sum*v->proj[n][j];
          }
          /* normalize */
          for ( n = 0, sum = 0.0 ; n < SDIM ; n++ ) 
             sum += v->proj[n][i]*v->proj[n][i];
          sum = sqrt(sum);
          if ( sum > hessian_epsilon )
             for ( n = 0 ; n < SDIM ; n++ ) v->proj[n][i] /= sum;
          else /* have to skip this direction */
          { for ( n = 0 ; n < SDIM ; n++ )
              v->proj[n][i] = v->proj[n][v->freedom-1];
            v->freedom--;
            i--;
          }
        }
        if ( (hessian_normal_flag && (v->freedom == 1)) || hessian_double_normal_flag)
        { /* get cosine of normal and degree of freedom */
          REAL *nor = get_vertex_v_normal(v_id)[0];
          v->slant = 0.0;
          for ( i = 0 ; i < SDIM ; i++ )
             v->slant += v->proj[i][0]*nor[i];
          v->slant /= sqrt(SDIM_dot(nor,nor));
          if ( v->slant < hessian_slant_cutoff ) 
                v->freedom = 0;
        }
      }
        
      /* now constraint hessian */
      if ( oncount )
      { v->conhess = conhess_base + conhess_alloc;
        conhess_alloc += v->freedom;
        mat_tsquare(grad,cc,oncount,SDIM);
        mat_inv(cc,oncount);
        for ( gam = 0 ; gam < SDIM ; gam++ )
         for ( a = 0 ; a < v->freedom ; a++ )
          for ( b = 0 ; b < v->freedom ; b++ )
          { REAL sum;
            for ( sum = 0.0, i = 0 ; i < oncount ; i++ )
             for ( j = 0 ; j < oncount ; j++ )
               for ( m = 0 ; m < SDIM ; m++ )
                for ( n = 0 ; n < SDIM ; n++ )
                  sum += grad[j][gam]*cc[j][i]*v->proj[m][a]*second[i][m][n]
                              *v->proj[n][b];
            v->conhess[a][b][gam] = -sum;
          }
       }
    } /* end level set constraints */
 
    if ( hessian_double_normal_flag && v->proj )
    { REAL sum;
      int k;
      /* special kludge to give double-dimension perturbations the
         same normal space as regular dimensions. See spin4d.fe
         for example of use. */
      for ( i =  0 ; i < v->freedom ; i++ )
      { /* assume low dim normals are first */
        for ( j = 0, sum = 0.0 ; j < SDIM/2 ; j++ )
           sum += v->proj[j][i]*v->proj[j][i];
        if ( sum < 1e-6 ) break;
      }
      /* copy over low dim basis to high dim, with transform */
      for ( k = 0 ; k < i ; k++ )
         for ( j = 0 ; j < SDIM/2 ; j++ ) 
         { v->proj[j][i+k] = v->proj[j+SDIM/2][k];
           v->proj[j+SDIM/2][i+k] = v->proj[j][k];
         } 
      v->freedom = 2*i;  
    } 

    if ( hess_debug && v->proj )
    { printf("v_id %s proj\n",ELNAME(v_id));
      for ( j = 0 ; j < v->freedom ; j++ )
      { for ( m = 0 ; m < SDIM ; m++ ) printf("%f ",(DOUBLE)v->proj[m][j]);
        printf("\n");
      }
    }

  } /* end FOR_ALL_VERTICES second time through */

  if ( v_normal ) free_matrix3(v_normal); 

  /* assign row numbers */
  for ( i = 0, v = vhead, currow = 0 ; i < vhead_count ; i++,v++ )
  { v->rownum = currow;
    currow += v->freedom;
  }

  /* check */
  if ( (vproj_alloc > vproj_count*SDIM) || (vproj_spot > total_proj*SDIM) ||
      (conhess_alloc > total_conhess) )
     kb_error(1608,"Internal error, hessian_init(): Overflow of vproj or conhess arena.\n",RECOVERABLE);


  /* low_rank update stuff */
  if ( quantity_function_sparse_flag )
  {
    for ( i = LOW_INST ; i < meth_inst_count ; i++ )
    { struct method_instance *mi = METH_INSTANCE(i);
      if ( mi->flags & Q_COMPOUND )
        if ( GEN_QUANT(mi->quant)->flags & (Q_ENERGY|Q_FIXED|Q_CONSERVED) )
        {
          mi->global_low_rank = S->low_rank;
          S->low_rank++;
        }
    }
    S->low_rank_vecsize = currow;
    S->low_rank_vectors = dmatrix(0,S->low_rank,0,currow);
    S->low_rank_form = dmatrix(0,S->low_rank,0,S->low_rank);
    S->low_rank_inverse_form = dmatrix(0,S->low_rank,0,S->low_rank);
  }

  /* rows for optimizing parameters */
  S->optparamrowstart = vertex_rows = currow;
  for ( i = 0 ; i < optparamcount ; i++)
  { 
    optparam[i].vhead_index = i + web.skel[VERTEX].max_ord+1;
    v = vhead + optparam[i].vhead_index;
    v->rownum = currow;
    v->freedom = 1;
    optparam[i].rownum = currow++;
  }

  /* allocate matrix */
  S->A_rows = S->bodyrowstart = currow; 
  /* S->A_rows is variables, before constraints */

  if ( everything_quantities_flag )
    S->quanrowstart = currow;
  else
    S->quanrowstart = currow + web.skel[BODY].max_ord+1;
  S->total_rows = S->quanrowstart + gen_quant_count;

  /* take care of torus_filled */
  if ( web.full_flag )  /* pretend one body info only */
  { body_id b_id;
    int found = 0;
    FOR_ALL_BODIES(b_id)
    { if ( (get_battr(b_id) & FIXEDVOL) && !found )
      { if ( everything_quantities_flag )
          GEN_QUANT(get_body_volquant(b_id))->flags |= Q_REDUNDANT;
        else set_attr(b_id,REDUNDANT_BIT);
        found = 1; 
      }
      else
      { if ( everything_quantities_flag )
          GEN_QUANT(get_body_volquant(b_id))->flags &= ~Q_REDUNDANT;
        else unset_attr(b_id,REDUNDANT_BIT);
      }
    }
  }

  /* figure out which bodies and quantities are constraints */
  S->concount = web.skel[BODY].max_ord+1 + gen_quant_count;
  S->coninx = (int*)temp_calloc(S->concount,sizeof(int));
  S->coninxinv = (int*)temp_calloc(S->concount,sizeof(int));
  S->CN = 0;
  { if ( !everything_quantities_flag )
    { body_id b_id;
      FOR_ALL_BODIES(b_id)
      { int a = get_battr(b_id);
      
        set_body_vhead(b_id,web.skel[VERTEX].max_ord+1 + loc_ordinal(b_id));
        v = get_body_vhead(b_id);
        if ( (a & (FIXEDVOL|PRESSURE)) && !(a & REDUNDANT_BIT)  )
        { 
          v->v_id = b_id;
          v->freedom = 1;
          S->coninx[loc_ordinal(b_id)] = S->CN;
          S->coninxinv[S->CN] = loc_ordinal(b_id);
          S->CN++;
        }
        else S->coninx[loc_ordinal(b_id)] = -1;
      }
      FOR_ALL_BODIES(b_id)
      { v = get_body_vhead(b_id);
        v->rownum = currow;
        currow += v->freedom;      
      }
    }
    else  S->concount = gen_quant_count;
    for ( n = 0 ; n < gen_quant_count ; n++,v++ )
    { int nn = everything_quantities_flag ? n : web.skel[BODY].max_ord+1 + n ;
      GEN_QUANT(n)->vhead_index = web.skel[VERTEX].max_ord+1+optparamcount+nn;
      v = vhead + GEN_QUANT(n)->vhead_index;
      v->rownum = currow;
      v->v_id = n;
      if ( (GEN_QUANT(n)->flags & (Q_FIXED|Q_CONSERVED) ) &&
         !(GEN_QUANT(n)->flags & Q_REDUNDANT)  )
      { 
        v->freedom = 1;
        currow++;
        S->coninx[nn] = S->CN;
        S->coninxinv[S->CN] = nn;
        S->CN++;
      }
      else S->coninx[nn] = -1;
    }
  }

  /* figure whether to do constraints as augmentation or separately */
  if ( augmented_hessian_flag < 0 )
  { augmented_hessian_mode = (sparse_constraints_flag && (S->CN >= 50)) ? 1 : 0;
  }
  else augmented_hessian_mode = augmented_hessian_flag;

  pressures = (REAL*)temp_calloc(web.skel[BODY].max_ord+1+gen_quant_count,
                sizeof(REAL));
  conrhs = (REAL*)temp_calloc(web.skel[BODY].max_ord+1+gen_quant_count,
                sizeof(REAL));

  /* allocate right hand side and solution of augmented matrix */
  if ( rhs ) *rhs = (REAL *)temp_calloc(S->total_rows,sizeof(REAL));

  S->P = (int *)temp_calloc(S->total_rows,sizeof(int));
  S->IP = (int *)temp_calloc(S->total_rows,sizeof(int));

  /* initialize hash table */
  sp_hash_init(S,0);

  /* tell solvers element data is appropriate */
  S->flags |= S_USE_ELEMENTS;
} /* end hessian_init */

/*************************************************************************
*
* function: hessian_fill()
*
* purpose: call routines that calculate hessian.
*/
void hessian_fill(S,rhsptr)
struct linsys *S;
REAL **rhsptr;
{ int k;
  body_id b_id;
  REAL *rhs = rhsptr ? *rhsptr : NULL;

  #ifdef MPI_EVOLVER
  if ( this_task == 0 )
  { mpi_hessian_fill(S,rhsptr);
    temp_free((char*)S->hashtable); /* since not calling hash_end() */
    rhs = rhsptr ? *rhsptr : NULL;  /* mpi_hessian_fill reallocates it */
    goto rhs_constraints;
  }
  #endif
 
  /* clear entries */
  if ( rhs_flag ) memset((char*)rhs,0,S->total_rows*sizeof(REAL));

  /* fill in hessian */
  if ( hessian_by_diff_flag )
  { /* hessian by crude numeric difference formulas */
    difference_hessian(S,vhead,rhs);
    if ( optparamcount ) optparamhess(S,rhs);
  }
  else /* hessian by explicit formulas */
  {
    if ( !quantities_only_flag )
    { 
      if ( web.symmetry_flag && !web.torus_flag )
      { if ( auto_convert_flag ) {convert_to_quantities(); goto quantplace;}
        else
        kb_error(2079,
          "Must do convert_to_quantities to do Hessian with symmetry group.\n",
                    RECOVERABLE);
      }
      if ( web.representation == SIMPLEX )
         simplex_hessian(S,rhs);
      else 
      { if ( web.representation == SOAPFILM ) 
          area_hessian(S,rhs);
        edge_energy_hessian(S,rhs);
      }

    }
quantplace:
    if ( gen_quant_count )
       calc_quant_hess(S,rhs_flag,hess_flag,rhs);
        
    if ( !quantities_only_flag )
       if ( web.bodycount > web.full_flag  )
          body_hessian(S,rhs);

    if ( optparamcount ) optparamhess(S,rhs);
  } 

  if ( hess_flag )
  {
    /* extract stuff from hash table */
    if ( sparse_constraints_flag )
      sp_hash_end(S,S->total_rows,S->total_rows,A_OFF);
    else
      sp_hash_end(S,S->A_rows,S->total_rows,A_OFF);

  }

#ifdef MPI_EVOLVER
rhs_constraints:
#endif

  /* rhs for body constraint rows */
  if ( rhs && !everything_quantities_flag )
  FOR_ALL_BODIES(b_id)
  { REAL *current = rhs + S->bodyrowstart + loc_ordinal(b_id);
    if ( get_battr(b_id) & FIXEDVOL )
         *current = -(get_body_fixvol(b_id) - get_body_volume(b_id));
  }

  /* rhs for quantity constraint rows */
  if ( rhs )
  for ( k = 0 ; k < gen_quant_count ; k++ )
  { struct gen_quant *q = GEN_QUANT(k);
    if ( q->flags & Q_FIXED )
      rhs[S->quanrowstart + k] = -(q->target - q->value);
  }

}  /* end hessian_fill() */


/***********************************************************************
*
* function: hessian_move()
*
* purpose: move vertices by given stepsize times current vector
*/

void hessian_move(stepsize,mode,X)
REAL stepsize;
int mode; /* ACTUAL_MOVE or TEST_MOVE or SET_VELOCITY */
REAL *X;  /* direction */
{
  int i,j;
  struct hess_verlist *vc;              /* current  vertex */
  vertex_id v_id;

  last_hessian_scale = stepsize;

  /* change optimizing parameters */
  for ( i = 0 ; i < optparamcount ; i++ )
  { optparam[i].velocity = X[optparam[i].rownum];
    if ( mode != SET_VELOCITY )
      globals(optparam[i].pnum)->value.real += stepsize*X[optparam[i].rownum];
  }
  /* move vertices */
  #ifdef MPI_EVOLVER
  if ( this_task == 0 ) 
    mpi_hessian_move(stepsize,mode,X);
  else
  #endif 

  FOR_ALL_VERTICES(v_id)
  {
     REAL *coord;
     REAL *velocity;

     vc = get_vertex_vhead(v_id);
     if ( vc->freedom <= 0 ) continue;
     coord = get_coord(v_id);
     velocity = get_velocity(v_id);
     if ( get_vattr(v_id) & BOUNDARY )
     { struct boundary *boundary = get_boundary(v_id);
       int pcount = boundary->pcount;
       REAL *param = get_param(v_id);

       for ( i = 0 ; i < pcount ; i++ )
       { velocity[i] = X[vc->rownum + i];
         if ( mode != SET_VELOCITY )
           param[i] += stepsize*X[vc->rownum + i];
       }
       if ( mode != SET_VELOCITY )
        for ( i = 0 ; i < SDIM ; i++ )
         coord[i] = eval(boundary->coordf[i],param,v_id,NULL);
     }
     else
     { for ( j = 0 ; j < SDIM ; j++ )
         if ( vc->proj )
         { velocity[j] = dot(vc->proj[j],X+vc->rownum,vc->freedom);
           if ( mode != SET_VELOCITY )
             coord[j] += stepsize*dot(vc->proj[j],X+vc->rownum,vc->freedom);
         }
         else
         { velocity[j] = X[vc->rownum + j]; 
           if ( mode != SET_VELOCITY )
             coord[j] += stepsize*X[vc->rownum + j];
         }
     }
  }

  #ifdef MPI_EVOLVER
  if ( this_task != 0 ) return;
  #endif

  if ( mode == SET_VELOCITY ) return;
  if ( hess_move_con_flag )
  {
     calc_all_grads(CALC_VOLGRADS);
     project_all(hess_move_con_flag /* all constraints */,mode);
     vgrad_end();
  }
  else
     project_all(hess_move_con_flag /* all constraints ? */,mode);

  global_timestamp++;
  calc_energy();  /* energy after motion */
}  /* end hessian_move() */


/**************************************************************************
*
* function: hessian_exit()
* 
* purpose: clean up after hessian stuff.
*/

void hessian_exit(X)
REAL *X; /* direction, if any */
{ int j;
  struct hess_verlist *vc;              /* current  vertex */
  vertex_id v_id;

  hess_debug = 0;
  
  /* store move in vertex force */
  if ( X && vhead )
    FOR_ALL_VERTICES(v_id)
    {
      REAL *f;
      vc = get_vertex_vhead(v_id);
      if ( vc->freedom <= 0 ) continue;
      f = get_force(v_id);
      if ( get_vattr(v_id) & BOUNDARY )
      { struct boundary *boundary = get_boundary(v_id);
        int pcount = boundary->pcount;

        for ( j = 0 ; j < pcount ; j++ )
          f[j] = X[vc->rownum + j];
      }
      else
      { for ( j = 0 ; j < SDIM ; j++ )
          if ( vc->proj )
             f[j] = dot(vc->proj[j],X+vc->rownum,vc->freedom);
          else
        f[j] = X[vc->rownum + j];
      }
    }
  hessian_cleanup();
}

/************************************************************************
*
* function: sp_hessian_mult
*
* purpose: multiply Hessian times vector
*/

void sp_hessian_mult(S,B,C)
struct linsys *S;
REAL *B; /* incoming vector */
REAL *C; /* product */
{ int i,j;


  if ( S->lambda != 0.0 )
  {
    if ( hessian_linear_metric_flag )
    { for ( j = 0 ; j < Met.N ; j++ ) C[j] = 0.0;
      for ( i = 0 ; i < Met.N ; i++ )
      { for ( j = Met.IA[i] ; j < Met.IA[i+1] ; j++ )
        { int jj = Met.JA[j-A_OFF] - A_OFF;
          C[i] -= S->lambda*Met.A[j-A_OFF]*B[jj];
          if ( i != jj )
             C[jj] -= S->lambda*Met.A[j-A_OFF]*B[i];
        }
      }
    }
    else if ( augmented_hessian_mode )
    { for ( i = 0 ; i < S->A_rows ; i++ )
        C[i] = -S->lambda*B[i];
      for (  ; i < S->N ; i++ )
        C[i] = 0.0;
    }
    else
     for ( i = 0 ; i < S->N ; i++ )
      C[i] = -S->lambda*B[i];
    }

  for ( i = 0 ; i < S->N ; i++ )
  { for ( j = S->IA[i] ; j < S->IA[i+1] ; j++ )
    { int jj = S->JA[j-A_OFF] - A_OFF;
      C[i] += S->A[j-A_OFF]*B[jj];
      if ( i != jj )
         C[jj] += S->A[j-A_OFF]*B[i];
    }
  }

  /* Add low-rank stuff */
  if ( S->low_rank )
  { REAL *Y = (REAL*)temp_calloc(S->low_rank,sizeof(REAL));
    REAL *W = (REAL*)temp_calloc(S->low_rank,sizeof(REAL));
    REAL *Z = (REAL*)temp_calloc(S->N,sizeof(REAL));
    matvec_mul(S->low_rank_vectors,B,Y,S->low_rank,S->low_rank_vecsize);
    matvec_mul(S->low_rank_form,Y,W,S->low_rank,S->low_rank);
    vec_mat_mul(W,S->low_rank_vectors,Z,S->low_rank,S->low_rank_vecsize);
    for ( i = 0 ; i < S->low_rank_vecsize ; i++ )
      C[i] += Z[i];
    temp_free((char*)Y);
    temp_free((char*)W);
    temp_free((char*)Z);
  }
}

/*********************************************************************
*
* function: hessian_cleanup()
*
* purpose: free memory allocated for hessian stuff.
*
*/

void hessian_cleanup()
{
  #ifdef MPI_EVOLVER
  if ( this_task == 0 )
  { mpi_hessian_cleanup();
  }
  #endif
  
  if ( vhead ) temp_free((char *)vhead); vhead = NULL;
  if ( vproj_base ) myfree((char*)vproj_base); vproj_base = NULL;
  if ( vproj_space ) myfree((char*)vproj_space); vproj_space = NULL;
  if ( conhess_base ) free_matrix3(conhess_base); conhess_base = NULL;
  local_unsave_coords(&saved,SAVE_IN_ATTR);
  if ( pressures ) temp_free((char*)pressures); pressures = NULL;
  if ( conrhs ) temp_free((char*)conrhs); conrhs = NULL;
  if ( ritzvecs ) free_matrix(ritzvecs); ritz_done_flag = 0; ritzvecs = NULL;
  return;

}

/********************************************************************
*
* function: write_hessian()
*
* purpose: Write hessian to file in format suitable for
*             wet cone investigations.
*/

void write_hessian(S)
struct linsys *S;
{
  FILE *fd;
  char name[100];
  int i,j,n,row;
  struct hess_verlist *v;
  vertex_id v_id;

  prompt("Enter hessian file name: ",name,sizeof(name));
  fd = fopen(name,"w");
  if ( fd == NULL )
  { perror(name); return; }

  /* size info */
  fprintf(fd,"%d space dimension\n",SDIM);
  fprintf(fd,"%d rows\n",S->N);
  fprintf(fd,"%d entries\n",S->IA[S->N]-A_OFF);
  if ( hessian_linear_metric_flag )
  {  fprintf(fd,"%d metric rows\n",Met.N);
      fprintf(fd,"%d metric entries\n",Met.IA[S->N]-A_OFF);
  }
  else
  {  fprintf(fd,"%d metric rows\n",0);
      fprintf(fd,"%d metric entries\n",0);
  }
  fprintf(fd,"%d constraints\n",S->CN);

  /* per row info:
      point coords, basis vectors for each degree of freedom,
   */
  
  FOR_ALL_VERTICES(v_id)
  { REAL *x = get_coord(v_id);
    v = get_vertex_vhead(v_id);
    for ( j = 0 ; j < v->freedom ; j++ )
    { fprintf(fd,"%d ",v->rownum+j);
      for ( n = 0 ; n < SDIM ; n++ ) fprintf(fd," %21.15e",(DOUBLE)x[n]);
      if ( v->proj )
        for ( n = 0 ; n < SDIM ; n++ ) 
          fprintf(fd," %21.15e",(DOUBLE)v->proj[n][j]);
      else
        for ( n = 0 ; n < SDIM ; n++ ) fprintf(fd," %d",n==j);
      fprintf(fd,"\n");
    }
  }

  /* the Hessian */
  FOR_ALL_VERTICES(v_id)
  { 
    v = get_vertex_vhead(v_id);
    if ( v->freedom == 0 ) continue;
    for ( row = v->rownum ; row < v->rownum + v->freedom ; row++ )
    { int start = S->IA[row]-A_OFF;
      int end = S->IA[row+1] - A_OFF;
      for ( j = start ; j < end ; j++ )
        fprintf(fd,"%d %d %21.15e\n",row,S->JA[j]-A_OFF,(DOUBLE)S->A[j]);
    }
  }
  /* the metric */
  if (hessian_linear_metric_flag)
   FOR_ALL_VERTICES(v_id)
   { 
     v = get_vertex_vhead(v_id);
     if ( v->freedom == 0 ) continue;
     for ( row = v->rownum ; row < v->rownum + v->freedom ; row++ )
     { int start = Met.IA[row]-A_OFF;
        int end = Met.IA[row+1] - A_OFF;
        for ( j = start ; j < end ; j++ )
          fprintf(fd,"%d %d %21.15e\n",row,Met.JA[j]-A_OFF,(DOUBLE)Met.A[j]);
     }
   }

  /* constraints */
  for ( i = 0 ; i < S->CN ; i++ )
     for ( j = 0 ; j < S->N ; j++ )
        fprintf(fd,"%d %d %21.15e\n",i,j,(DOUBLE)S->C[i][j]);
  fclose(fd);
}

/**********************************************************************
*
* function: optparamhess()
*
* purpose: fill in hessian and right side for optimizing parameter rows.
*
* method: finite differences
*/

void optparamhess(S,rhs)
struct linsys *S;
REAL *rhs;  /* right side */
{ int i,j,m;
  struct oldcoord csaved;
  REAL *fake_rhs;    /* for grad differences */
  REAL *temprow;     /* for accumulating row values */
  struct gen_quant *q;

  if ( optparamcount <= 0 ) return;
  if ( !everything_quantities_flag ) 
     kb_error(2080,"Hessian for optimizing parameters requires convert_to_quantities.\n",
        RECOVERABLE);

  fake_rhs = (REAL*)temp_calloc(S->total_rows,sizeof(REAL));
  temprow  = (REAL*)temp_calloc(S->total_rows,sizeof(REAL));

  csaved.coord = NULL;
  save_coords(&csaved,SAVE_SEPARATE);
  for ( i = 0 ; i < optparamcount ; i++ )
  { REAL dp;
    REAL emid = web.total_energy;
    REAL eleft,eright;

    restore_coords(&csaved,SAVE_SEPARATE);
    calc_energy(); calc_content(Q_FIXED);
    emid = web.total_energy;

    dp = globals(optparam[i].pnum)->attr.varstuff.delta;

    memset((char*)fake_rhs,0,S->total_rows*sizeof(REAL));
    memset((char*)temprow ,0,S->total_rows*sizeof(REAL));

    /* for optimizing parameter self hessian */
    for ( m = 0 ; m < gen_quant_count ; m++ )
    { q = GEN_QUANT(m);
      if ( q->flags & Q_FIXED )
      { sp_hash_search(S,optparam[i].rownum,optparam[i].rownum,
            -q->pressure*( - 2*q->value )/dp/dp);
      }             
    }

    /* right difference */
    globals(optparam[i].pnum)->value.real += dp;
    project_all(0, TEST_MOVE);
    if ( fixed_constraint_flag || web.pressure_flag || web.pressflag )
        calc_content(Q_FIXED);
    calc_energy();  /* energy after motion */
    calc_quant_hess(S,1,0,temprow);
    eright = web.total_energy;
    calc_volgrads(NO_OPTS); /* global constraint hessians */

    /* for global constraints */
    for ( m = 0 ; m < gen_quant_count ; m++ )
    { q = GEN_QUANT(m);
      if ( q->flags & Q_FIXED )
      { sp_hash_search(S,optparam[i].rownum,optparam[i].rownum,
            -q->pressure*( q->value )/dp/dp);
        sp_hash_search(S,optparam[i].rownum,S->quanrowstart+m,
             q->value/dp/2);
         if ( rhs ) rhs[optparam[i].rownum] += q->pressure*q->value/dp/2;
      } 
    }
    restore_coords(&csaved,SAVE_SEPARATE);  /* also restores opt params */

    /* left difference */
    globals(optparam[i].pnum)->value.real -= dp;
    project_all(0, TEST_MOVE);
    if ( fixed_constraint_flag || web.pressure_flag || web.pressflag )
        calc_content(Q_FIXED);
    calc_energy();  /* energy after motion */
    calc_quant_hess(S,1,0,fake_rhs);
    eleft = web.total_energy;
    calc_volgrads(NO_OPTS); /* global constraint hessians */

    /* for global constraints */
    for ( m = 0 ; m < gen_quant_count ; m++ )
    { q = GEN_QUANT(m);
      if ( q->flags & Q_FIXED )
      { sp_hash_search(S,optparam[i].rownum,optparam[i].rownum,
            -q->pressure*( q->value )/dp/dp);
        sp_hash_search(S,optparam[i].rownum,S->quanrowstart+m,
             -q->value/dp/2);
         if ( rhs )  rhs[optparam[i].rownum] -= q->pressure*q->value/dp/2;
      } 
    }
 
    vgrad_end();
    restore_coords(&csaved,SAVE_SEPARATE);  /* also restores opt params */
    web.total_energy = emid; /* restore */

    if ( rhs )
      rhs[optparam[i].rownum] += -(eright - eleft)/2/dp; /* rhs is neg grad */

    /* vertex-param cross terms */
    for ( j = 0 ; j < vertex_rows ; j++ )
      sp_hash_search(S,j,optparam[i].rownum,(fake_rhs[j]-temprow[j])/2/dp);
    sp_hash_search(S,optparam[i].rownum,optparam[i].rownum,
            (eright - 2*emid + eleft)/dp/dp);
  }
  temp_free((char*)fake_rhs);
  temp_free((char*)temprow);

  /* optimizing parameter mixed terms */
  for ( i = 0 ; i < optparamcount ; i++ )
    for ( j = 0 ; j < i ; j++ )
      { REAL dp;
        REAL vmid = globals(optparam[i].pnum)->value.real;
        REAL emid = web.total_energy;
        REAL ehihi,ehilo,elohi,elolo;

        if ( fabs(vmid) > 1. ) dp = fabs(vmid)*1e-3;
        else dp = 1e-3;

        /* + + difference */
        globals(optparam[i].pnum)->value.real += dp;
        globals(optparam[j].pnum)->value.real += dp;
        project_all(0, TEST_MOVE);
        if ( fixed_constraint_flag || web.pressure_flag || web.pressflag )
            calc_content(Q_FIXED);
        calc_energy();  /* energy after motion */
        ehihi = web.total_energy;
        for ( m = 0 ; m < gen_quant_count ; m++ )
        { q = GEN_QUANT(m);
          if ( q->flags & Q_FIXED )
             sp_hash_search(S,optparam[j].rownum,optparam[i].rownum,
                -q->pressure*( q->value )/dp/dp/4);
        } 
        restore_coords(&csaved,SAVE_SEPARATE);  /* also restores opt params */

        /* + - difference */
        globals(optparam[i].pnum)->value.real += dp;
        globals(optparam[j].pnum)->value.real -= dp;
        project_all(0, TEST_MOVE);
        if ( fixed_constraint_flag || web.pressure_flag || web.pressflag )
            calc_content(Q_FIXED);
        calc_energy();  /* energy after motion */
        ehilo = web.total_energy;
        for ( m = 0 ; m < gen_quant_count ; m++ )
        { q = GEN_QUANT(m);
          if ( q->flags & Q_FIXED )
             sp_hash_search(S,optparam[j].rownum,optparam[i].rownum,
                -q->pressure*(-q->value )/dp/dp/4);
        }
        restore_coords(&csaved,SAVE_SEPARATE);  /* also restores opt params */

        /* - + difference */
        globals(optparam[i].pnum)->value.real -= dp;
        globals(optparam[j].pnum)->value.real += dp;
        project_all(0, TEST_MOVE);
        if ( fixed_constraint_flag || web.pressure_flag || web.pressflag )
            calc_content(Q_FIXED);
        calc_energy();  /* energy after motion */
        elohi = web.total_energy;
        for ( m = 0 ; m < gen_quant_count ; m++ )
        { q = GEN_QUANT(m);
          if ( q->flags & Q_FIXED )
             sp_hash_search(S,optparam[j].rownum,optparam[i].rownum,
                -q->pressure*(-q->value )/dp/dp/4);
        }
        restore_coords(&csaved,SAVE_SEPARATE);  /* also restores opt params */

        /* - - difference */
        globals(optparam[i].pnum)->value.real -= dp;
        globals(optparam[j].pnum)->value.real -= dp;
        project_all(0, TEST_MOVE);
        if ( fixed_constraint_flag || web.pressure_flag || web.pressflag )
            calc_content(Q_FIXED);
        calc_energy();  /* energy after motion */
        elolo = web.total_energy;
        for ( m = 0 ; m < gen_quant_count ; m++ )
        { q = GEN_QUANT(m);
          if ( q->flags & Q_FIXED )
             sp_hash_search(S,optparam[j].rownum,optparam[i].rownum,
                -q->pressure*( q->value )/dp/dp/4);
        }
        restore_coords(&csaved,SAVE_SEPARATE);  /* also restores opt params */

        web.total_energy = emid; /* restore */

        sp_hash_search(S,optparam[j].rownum,optparam[i].rownum,
                (ehihi-ehilo-elohi+elolo)/dp/dp/4);
     }

  unsave_coords(&csaved,SAVE_SEPARATE);
} /* end optparamhess() */
