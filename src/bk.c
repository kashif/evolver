/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/************************************************************************/  
/*    File: bk.c                                                        */
/*                                                                      */
/*     Bunch-Kaufman factoring                                          */
/*     and general linear system mongering                              */
/************************************************************************/  

#include "include.h"

/**************************************************************************/

#define RSWAP(a,b) {REAL rtmp = a; a = b ; b = rtmp; }
#define RPTRSWAP(a,b) {REAL *rtmp = a; a = b ; b = rtmp; }
#define ISWAP(a,b) {int rtmp = a; a = b ; b = rtmp; }

/* local prototypes to keep certain compilers happy */
struct xsorting { int inx;  /* original row number */
         REAL x; /* x value of vertex */
         int ord;  /* ordinal of vertex */
         int deg;  /* degree of freedom */
         };

void sp_aug_solve ARGS(( struct linsys *,REAL *,REAL *,REAL *,REAL *,REAL *));
int xcomp ARGS((struct xsorting *, struct xsorting*));
void BK_hess_project ARGS(( struct linsys *,REAL *,REAL *x));
/***************************************************************************
*
* function: sp_aug_solve()
*
* purpose: common steps in Hessian solving.
*
*/

void sp_aug_solve(S,X,B,T1,T2,T3)
struct linsys *S;
REAL *X;           /* incoming */
REAL *B;           /* incoming rhs, possibly null */
REAL *T1,*T2,*T3;  /* intermediates and final, preallocated */
{
  if ( sparse_constraints_flag )
  { int i,j;
    memset(T1,0,S->CN*sizeof(REAL));
    for ( i = 0 ; i < S->CN ; i++ )
      for ( j = S->CIA[i] ; j < S->CIA[i+1] ; j++ )
         T1[i] += S->CA[j]*X[S->CJA[j]];
    if ( B )
      for ( i = 0 ; i < S->CN ; i++ )
        T1[i] += B[i]; /* corrections */
#ifdef BLAS
    if ( blas_flag )
      LD_block_solve(S->CHinvCinv,T1,T2,S->CN);
    else
#endif
    matvec_mul(S->CHinvCinv,T1,T2,S->CN,S->CN);
    memset(T3,0,S->N*sizeof(REAL));
    for ( i = 0 ; i < S->CN ; i++ )
      for ( j = S->CIA[i] ; j < S->CIA[i+1] ; j++ )
        T3[S->CJA[j]] += S->CA[j]*T2[i];
    sp_solution(S,T3,T3);
  }
  else
  { int i;
    matvec_mul(S->C,X,T1,S->CN,S->N);
    if ( B )
      for ( i = 0 ; i < S->CN ; i++ )
        T1[i] += B[i]; /* corrections */
#ifdef BLAS
    if ( blas_flag )
      LD_block_solve(S->CHinvCinv,T1,T2,S->CN);
    else
#endif
    matvec_mul(S->CHinvCinv,T1,T2,S->CN,S->CN); /* Lagrange multipliers */
    vec_mat_mul(T2,S->HinvC,T3,S->CN,S->N);
  }
}

/*******************************************************************
* 
* function: bk_mul()
*
* purpose: multiply vector by original sparse matrix
*/
void bk_mul(S,w,v)
struct linsys  *S;
REAL *w; /* in */ 
REAL *v; /* out */
{ int row,col,i;

  PROF_START(hessian_mul);

  memset((char*)v,0,S->N*sizeof(REAL));
  for ( row = 0 ; row < S->N ; row++ )
  { for ( i = S->IA[row]-A_OFF ; i < S->IA[row+1]-A_OFF ; i++ )
    { col = S->JA[i] - A_OFF;
      v[row] += S->A[i]*w[col];
      if ( col != row ) v[col] += S->A[i]*w[row];
    }
  }
  if ( (S->CN > 0) && S->CHinvCinv )
  { /* project back */
     REAL *tempv = (REAL*)temp_calloc(2*S->N,sizeof(REAL));
     REAL *tempvv = tempv + S->N;
     sp_aug_solve(S,v,NULL,tempv,tempvv,tempv);
     for ( i = 0 ; i < S->N ; i++ ) v[i] -= tempv[i];
     temp_free((char*)tempv);
  }

  PROF_FINISH(hessian_mul);
} /* end bk_mul() */


/*******************************************************************
* 
* function: free_system()
*
* purpose: free all memory allocated to linear system structure.
*/

void free_system(S)
struct linsys *S;
{ 
  if ( S->IA ) temp_free((char*)S->IA);
  if ( S->JA && !(S->flags & S_JA_INCLUDED) ) temp_free((char*)S->JA);
  if ( S->A ) temp_free((char*)S->A);
  if ( S->pIA ) temp_free((char*)S->pIA);
  if ( S->pJA ) temp_free((char*)S->pJA);
  if ( S->pA ) temp_free((char*)S->pA);
  if ( S->P ) temp_free((char*)S->P);
  if ( S->IP ) temp_free((char*)S->IP);
  if ( S->ISP ) temp_free((char*)S->ISP);
  if ( S->psize ) temp_free((char*)S->psize);
  if ( S->LIA ) temp_free((char*)S->LIA);
  if ( S->LJA ) temp_free((char*)S->LJA);
  if ( S->LIJA ) temp_free((char*)S->LIJA);
  if ( S->LA ) temp_free((char*)S->LA);
  if ( S->CIA ) temp_free((char*)S->CIA);
  if ( S->CJA ) temp_free((char*)S->CJA);
  if ( S->CA ) temp_free((char*)S->CA);
  if ( S->coninx ) temp_free((char*)S->coninx);
  if ( S->coninxinv ) temp_free((char*)S->coninxinv);
  if ( S->rowmag ) temp_free((char*)S->rowmag);
  if ( S->apinv ) temp_free((char*)S->apinv);
  if ( S->HinvC ) free_matrix(S->HinvC);
  if ( S->CHinvCinv ) 
  { if ( blas_flag )
    { temp_free((char*)S->CHinvCinv[0]); temp_free((char*)S->CHinvCinv); }
    else free_matrix(S->CHinvCinv);
  }
  if ( S->C ) { temp_free((char *)S->C[0]); temp_free((char*)S->C);}
  if ( S->stree )
  { int k;
    for (  k = 0 ; k <= S->streemax ; k++ )
    { if ( S->stree[k].u.info.mat) 
      { temp_free((char*)S->stree[k].u.info.mat);
        temp_free((char*)S->stree[k].u.info.vlist);
      }
    }
  }

  if ( S->low_rank_vectors ) free_matrix(S->low_rank_vectors);
  if ( S->low_rank_form ) free_matrix(S->low_rank_form);
  if ( S->low_rank_inverse_form ) free_matrix(S->low_rank_inverse_form);

  memset((char*)S,0,sizeof(struct linsys));
} /* end free_system() */

/************************************************************************
*
*  function: find_ordering()
*
*  purpose: find order of vertex degree of freedom variables.
*      Currently in x coordinate order. Not very useful.
*  in:  array    Hessian data
*          n     total degrees
*  out:    P     permutation vector, P[0] is first variable to do.
*/

/* comparison function for sorting */
int xcomp(a,b)
struct xsorting *a,*b;
{ if ( a->x < b->x ) return -1;
  if ( a->x > b->x ) return  1;
  if ( a->ord < b->ord ) return -1;
  if ( a->ord > b->ord ) return  1;
  if ( a->deg < b->deg ) return -1;
  if ( a->deg > b->deg ) return  1;
  return 0;
} /* end xcomp() */

void find_ordering(verlist,n,P)
struct hess_verlist *verlist;     /* pointers to rows */
int n;        /* size of system */
int *P;       /* preallocated for return of permutation */
{
  int i,j;
  struct xsorting *xlist,*Bptr;
  vertex_id v_id;

  /* sort vertices in x order */
  xlist = (struct xsorting *)temp_calloc(n,sizeof(struct xsorting));
  for ( i = 0, Bptr = xlist ; i < n ; i++,Bptr++ ) Bptr->inx = i;
  FOR_ALL_VERTICES(v_id)
  { struct hess_verlist *v;      /* current  vertex */
    v = verlist + loc_ordinal(v_id);
    Bptr = xlist + v->rownum;
    for ( j = 0 ; j < v->freedom ; j++, Bptr++ )
    {
      Bptr->ord = loc_ordinal(v->v_id);
      Bptr->deg = j;
      Bptr->x = get_coord(v->v_id)[0];
    }
  }
  qsort((char*)xlist,n,sizeof(struct xsorting),FCAST xcomp);
  for ( i = 0 ; i < n ; i++ ) 
  { P[i] = xlist[i].inx + A_OFF;
  }
  temp_free((char*)xlist);
} /* end find_ordering() */

/**********************************************************************
*
* function: bk_AIJ_setup()
*
* purpose: convert raw Hessian data to standard sparse format
*
*/

void bk_AIJ_setup(N,S)
int N;        /* size of system */
struct linsys *S;  /* pointer to empty structure */
{

#ifdef OLDHASH
  int i,j,n;
  int  total = 0;
  struct hess_entry *e;
  int spot;
  int *ptr,*cptr,*dptr;
  int isize,dsize;

  PROF_START(hessian_AIJ_setup);

  S->flags &= ~ S_ODRV_REORDERED; 
  S->flags |= S_JA_INCLUDED;  /* S->JA not separately allocated */.
  if ( augmented_hessian_mode )
    N = total_rows;
  S->N = N;

  for ( i = 0 ; i < N ; i++ ) total += array[i].count;

  /* incoming list of hess_entry structs is compacted in place */
  isize = sizeof(int);
  dsize = sizeof(REAL);
  /* delete row numbers */
  e = hashtable;
  ptr = (int*)e;  
  if ( 2*isize == dsize )
   for ( n = 0; n < total; n++,e++,ptr+=3 )
   { ptr[0] = ((int*)e)[0];
     ptr[1] = ((int*)e)[1];
     ptr[2] = e->col;
   }
  else if ( isize == dsize )
   for ( n = 0; n < total; n++,e++,ptr+=2 )
   { ptr[0] = ((int*)e)[0];
     ptr[1] = e->col;
   }
  else if ( 4*isize == dsize )
   for ( n = 0; n < total; n++,e++,ptr+=5 )
   { ptr[0] = ((int*)e)[0];
     ptr[1] = ((int*)e)[1];
     ptr[2] = ((int*)e)[2];
     ptr[3] = ((int*)e)[3];
     ptr[4] = e->col;
   }
  else  /* i.e. 10-byte long double */
  { char *p = (char*)ptr;
    for ( n = 0; n < total; n++,e++,p+=dsize+isize )
     { *(REAL *)p = e->value;
       *(int *)(p+dsize)  = e->col;
     }
    ptr = (int *)p;
  }
  /* now extract cols to room at the end */
  S->JA = cptr = ptr;
  S->A = (REAL*)(hashtable);
  hashtable = NULL;
  ptr = (int*)(S->A);
  dptr = ptr;
  if ( 2*isize == dsize )
   for ( n = 0 ; n < total; n++,dptr+=3,ptr+=2,cptr++)
     { ptr[0] = dptr[0];
       ptr[1] = dptr[1];
       *cptr =  dptr[2]+A_OFF;
     }
  else if ( isize == dsize )
     for ( n = 0 ; n < total; n++,dptr+=2,ptr+=1,cptr++)
     { ptr[0] = dptr[0];
       *cptr =  dptr[1]+A_OFF;
     }
  else if ( 4*isize == dsize )
     for ( n = 0 ; n < total; n++,dptr+=5,ptr+=4,cptr++)
     { ptr[0] = dptr[0];
       ptr[1] = dptr[1];
       ptr[2] = dptr[2];
       ptr[3] = dptr[3];
       *cptr =  dptr[4]+A_OFF;
     }
  else
  { char *p = (char *)ptr;
    REAL *d = (REAL *)ptr;
    for ( n = 0 ; n < total; n++,p+=isize+dsize,d++,cptr++ )
    { *d = *(REAL*)p;
      *cptr = *(int*)(p+dsize)+A_OFF; 
    }
  }

  /* allocate other bits */
  S->IA = (int *)temp_calloc(N+1,sizeof(int));
  if ( S->P == NULL ) S->P = (int *)temp_calloc(N,sizeof(int));
  if ( S->IP == NULL ) S->IP = (int *)temp_calloc(N,sizeof(int));
  S->flags &= ~ S_ODRV_REORDERED; 

  for ( i = 0, spot = 0 ; i < N ; i++ )
  { S->IA[i] = spot + A_OFF;
    spot +=  harray[i].count; 
  }
  S->IA[N] = spot + A_OFF;

  /* test for NaN's */
  for ( i = 0 ; i < S->IA[S->N]-A_OFF ; i++ )
  if ( !is_finite(S->A[i]) )
  { kb_error(1822,"NaNs in Hessian. Replacing with big value.\n",WARNING);
    S->A[i] = 1e30; 
  }

  PROF_FINISH(hessian_AIJ_setup);
#endif
} /* end bk_AIJ_setup() */


/**********************************************************************
*
* function: bk_constraint_setup()
*
* purpose: get matrices needed for handling constraints
*
*/

void bk_constraint_setup(concount,S)
int concount;  /* number of constraints */
struct linsys *S; 
{ int i,j;
 
  PROF_START(hessian_constraint_setup);

  if ( S->CN == 0 ) 
  { S->N = S->A_rows; goto debug_print; }

  if ( S->A_rows < S->CN )
  { sprintf(errmsg,"Degrees of freedom, %d, fewer than constraints, %d.\n",
       S->A_rows,S->CN);
    kb_error(3382,errmsg,RECOVERABLE);
  }
  if ( augmented_hessian_mode )
  { /* Keeping constraints in sparse augmented hessian. */
    /* So not much to do, except eliminate unused constraints,  */
    /* building constraint conversion index in the process,   */
    /* and make sure zero elements exist on diagonal for constraints. */

    if ( S->A_rows + S->CN > S->maxN )
      kb_error(2549,"Internal error: S->N got bigger than expected.\n",
        RECOVERABLE);
    S->N = S->A_rows + S->CN;

    /* convert entries in S->JA */
    for ( i = 0 ; i < S->IA[S->A_rows]-A_OFF ; i++ )
      if ( S->JA[i]-A_OFF >= S->A_rows )
        S->JA[i] = S->A_rows + S->coninx[S->JA[i]-A_OFF-S->A_rows] + A_OFF;

    /* create diagonal for constraints */
    for ( i = 1 ; i <= S->CN ; i++ )
    { S->IA[i+S->A_rows] = S->IA[i+S->A_rows-1] + 1;
      S->JA[S->IA[i+S->A_rows-1]-A_OFF] = i + S->A_rows - 1 + A_OFF;
      S->A[S->IA[i+S->A_rows-1]-A_OFF] = 0.0;
    }

    if ( S->IA[S->N] > S->maxA )
      kb_error(2598,"Internal error: S->A got bigger than expected.\n",
             RECOVERABLE);
  }
  else if ( sparse_constraints_flag )
  { 
    /* Have to strip constraint gradients from A and put in CA */
    int k,kk;

    /* First, count entries for each constraint */
    S->CIA = (int *)temp_calloc(concount+1,sizeof(int));
    for ( i = S->A_rows ; i > 0 ; i-- )
    { for ( j = S->IA[i]-A_OFF - 1 ; j > S->IA[i-1]-A_OFF ; j-- )
        if ( S->JA[j]-A_OFF >= S->A_rows )
          S->CIA[S->JA[j]-A_OFF - S->A_rows]++;
        else break;
    }
    /* Running totals to S->CIA */
    for ( i = 0, j = 0 ; i < concount ; i++ )
    { int tmp = S->CIA[i];
      S->CIA[i] = j;
      j += tmp;
    }
    S->CJA = (int *)temp_calloc(j,sizeof(int));
    S->CA  = (REAL*)temp_calloc(j,sizeof(REAL));
    for ( i = concount ; i > 0 ; i-- )
      S->CIA[i] = S->CIA[i-1];
    /* place constraint gradients and compact A,JA */
    for ( i = 1, kk = 0, j = 0 ; i <= S->A_rows ; i++ )
    { for ( ; j < S->IA[i]-A_OFF ; j++ )
        if ( S->JA[j]-A_OFF >= S->A_rows )
        { k = S->CIA[S->JA[j]-A_OFF-S->A_rows+1]++;
          S->CJA[k] = i-1;
          S->CA[k]  = S->A[j];
        }
        else 
        { S->JA[kk] = S->JA[j];
          S->A[kk]  = S->A[j];
          kk++;
        }
      S->IA[i] = kk+A_OFF;
    }
    S->N = S->A_rows;    

    /* check the constraints we thought were going to be used actually are */
    for ( i = 0 ; i < S->concount ; i++ )
    { /* a check on if any fixed constraints are null */
      if ( (S->CIA[i] != S->CIA[i+1]) && (S->coninx[i] == -1) )
      { struct hess_verlist *vh = vhead + web.skel[VERTEX].max_ord+1+
                  optparamcount + i;
        if ( valid_id(vh->v_id) )
          sprintf(errmsg,
          "Internal error: hessian setup skipped fixed body %s.\n",
                ELNAME(vh->v_id));
        else 
          sprintf(errmsg,
          "Internal error: hessian setup skipped fixed quantity %s.\n",
                GEN_QUANT(vh->v_id)->name);
        kb_error(2552,errmsg,RECOVERABLE);
      }
      if ( (S->CIA[i] == S->CIA[i+1]) && (S->coninx[i] != -1) )
      { struct hess_verlist *vh = vhead + web.skel[VERTEX].max_ord+1+
                  optparamcount + i;
        if ( valid_id(vh->v_id) )
          sprintf(errmsg,
            "Fixed body %s is not connected to any mobile vertices.\n",
                ELNAME(vh->v_id));
        else 
          sprintf(errmsg,
            "Fixed quantity %s is not connected to any mobile vertices.\n",
                GEN_QUANT(vh->v_id)->name);
        kb_error(3362,errmsg,RECOVERABLE);
      }
    }
    /* Strip out empty constraints */
    for ( i = 0 ; i < S->CN ; i++ )
      S->CIA[i+1] = S->CIA[S->coninxinv[i]+1];
  }
  else   /* dense constraints */
  { int startj,newj;
    S->C = (REAL**)temp_calloc(S->CN,sizeof(REAL*));
    S->C[0] = (REAL*)temp_calloc(S->CN*S->N,sizeof(REAL));
    for ( i = 1 ; i < S->CN ; i++ ) S->C[i] = S->C[0] + i*S->N;
    for ( i = 0, startj = 0, newj = 0 ; i < S->N ; i++ )
    { /* fill in C  (really C transpose) */
      /* and compact JA, A */
      for ( j = startj ; j < S->IA[i+1]-A_OFF ; j++ )
        if ( S->JA[j]-A_OFF >= S->A_rows )
        { int row = S->coninx[S->JA[j]-A_OFF - S->A_rows];
          S->C[row][i] = S->A[j];
        }
        else /* keep in A but move it down */
        { S->A[newj] = S->A[j];
          S->JA[newj] = S->JA[j];
          newj++;
        }
      startj = S->IA[i+1] - A_OFF;  /* for next loop */
      S->IA[i+1] = newj + A_OFF;
    }
   
  }

debug_print:
  /* some debug printing */
  if ( hess_debug )
  { vertex_id v_id;
  
    printf("N = %d\n",S->N);
    printf("IA: ");
    for ( i = 0 ; i <= S->N ; i++ ) printf(" %d",S->IA[i]);
    printf("\nJA: ");
    for ( i = 0 ; i < S->IA[S->N]-A_OFF ; i++ ) printf(" %d",S->JA[i]);
    printf("\n");
    /* column labels */
    MFOR_ALL_VERTICES(v_id)
    { struct hess_verlist *vh = get_vertex_vhead(v_id);
      for ( j = 0 ; j < vh->freedom ; j++ )
      { sprintf(msg,"v%s.%d",ELNAME(v_id),j+1); 
        printf("%10s",msg);
      }
    }
    for ( i = 0 ; i < optparamcount ; i++ )
      printf("%10s",globals(optparam[i].pnum)->name);
    if ( augmented_hessian_mode )
    { if ( !everything_quantities_flag )
      { for ( i = 0 ; i <= web.skel[BODY].max_ord ; i++ )
          if ( S->coninx[i] >= 0 )
          { sprintf(msg,"b%d",i+1); printf("%10s",msg); }
        for ( i = 0 ; i < gen_quant_count ; i++ )
          if ( S->coninx[i + web.skel[BODY].max_ord+1] >= 0 )
          { sprintf(msg,"q%d",i+1); printf("%10s",msg); }
       }
       else /* just the quantities */
       for ( i = 0 ; i < gen_quant_count ; i++ )
          if ( S->coninx[i] >= 0 )
          { sprintf(msg,"q%d",i+1); printf("%10s",msg); }
    }
    printf("\n");

    /* now the body of the sparse part of the hessian, just upper triangle */
    for ( i = 0 ; i < S->N ; i++ ) 
      { int k,m;
        for ( m = 0 ; m < i ; m++ ) printf("          ");
        for ( m = i,  k = S->IA[i]-A_OFF ; m < S->N ; m++ )
          if ( (m == S->JA[k]-A_OFF) && (k < S->IA[i+1]-A_OFF) )
            { printf(" %9.6f",(DOUBLE)S->A[k]); k++; }
          else printf(" %9.6f",0.0);
        printf("\n");
     }

    /* Add on low-rank update */
    if ( S->low_rank )
    {
      printf("With low-rank update: \n");
      for ( i = 0 ; i < S->N ; i++ ) 
      { int k,m,ii,jj;
        for ( m = 0 ; m < i ; m++ ) printf("          ");
        for ( m = i,  k = S->IA[i]-A_OFF ; m < S->N ; m++ )
        { REAL val;
          if ( (m == S->JA[k]-A_OFF) && (k < S->IA[i+1]-A_OFF) )
          { val = S->A[k]; k++; }
          else val = 0.0;
          /* add low-rank stuff */
          if ( m < S->low_rank_vecsize )
            for ( ii = 0 ; ii < S->low_rank ; ii++ )
              for ( jj = 0 ; jj < S->low_rank ; jj++ )
                val += S->low_rank_vectors[ii][i]*S->low_rank_form[ii][jj]*
                         S->low_rank_vectors[jj][m];
          printf(" %9.6f",(DOUBLE)val);
        }
        printf("\n");
      }
    } 

    if ( !augmented_hessian_mode )
    {
      printf("Constraints:\n");
      if ( sparse_constraints_flag )
      { for ( i = 0 ; i < S->CN ; i++ )
        { int k = 0;
          for ( j = S->CIA[i] ; j < S->CIA[i+1] ; j++ ) 
          { for ( ; k < S->CJA[j]-1 ; k++ ) printf("%9.6f ",0.0);
            printf("%9.6f ",S->CA[j]); 
            k = S->CJA[j];
          }
        }
      }
      else
      { 
        for ( i = 0 ; i < S->CN ; i++ )
        { printf("C%d:",i+1);
          for ( j = 0 ; j < S->N ; j++ ) printf(" %9.6f",(double)(S->C[i][j]));
            printf("\n");
        }
      }
      printf("\n");
    }
   } /* end hess_debug */

  PROF_FINISH(hessian_constraint_setup);
} /* end bk_constraint_setup */

/*********************************************************************
*
* function: BK_hess_project_setup()
*
* purpose:  Set up projection to constraints using hessian metric.
*           Forms S->HinvC and CHinvCinv, unless augmented_hessian
*           is in effect.
*/

void BK_hess_project_setup(S)
struct linsys *S;
{ int i;
  int con_index=0;

  PROF_START(hessian_project_setup);

  if ( S->CN == 0 ) goto set_counts;
  if ( augmented_hessian_mode ) goto adjust_index;

  if ( !sparse_constraints_flag )
  {
    /* HinvC */
    if ( S->HinvC ) free_matrix(S->HinvC);
    S->HinvC = dmatrix(0,S->CN-1,0,S->N-1);
    for ( i = 0 ; i < S->CN ; i++ )
       sp_solution(S,S->C[i],S->HinvC[i]);
  }

  if ( S->CHinvCinv ) free_matrix(S->CHinvCinv);

#ifdef BLAS
  if ( blas_flag )
  { int fillsize = ((S->CN+BLAS_BLOCKSIZE)*(S->CN+BLAS_BLOCKSIZE+1))/2;
    S->CHinvCinv = (REAL**)temp_calloc(S->CN,sizeof(REAL*));
    S->CHinvCinv[0] = (REAL*)temp_calloc(fillsize,sizeof(REAL));
    for ( i = 0 ; i < S->CN ; i += BLAS_BLOCKSIZE )
    { int j;
      for ( j = i ; (j < i+BLAS_BLOCKSIZE) && (j < S->CN-1) ; j++ )
      { 
        S->CHinvCinv[j+1] = S->CHinvCinv[j] + i + BLAS_BLOCKSIZE;
      }
    }

  }
  else
#endif
  S->CHinvCinv = dmatrix(0,S->CN-1,0,S->CN-1);

  if ( sparse_constraints_flag )
  {
    if ( sp_CHinvC_func == NULL )
       kb_error(2445,"sparse_constraints need ysmp off (mindeg mode).\n",
         RECOVERABLE);
    (*sp_CHinvC_func)(S);    
  }
  else
  {
    /* CHinvCinv */
#ifdef BLAS
    if ( blas_flag )
    { for ( i = 0 ; i <  S->CN ; i += BLAS_BLOCKSIZE )
      { int rowcount = (S->CN-i < BLAS_BLOCKSIZE) ? S->CN-i : BLAS_BLOCKSIZE;
        int colcount = (S->CN < i+BLAS_BLOCKSIZE) ? S->CN : i+BLAS_BLOCKSIZE;

        int transa = 'T';
        int transb = 'N';
        REAL alpha = 1.0;
        REAL beta  = 0.0;
        int stridea = i+BLAS_BLOCKSIZE; 
        int strideb = S->C[1]-S->C[0];  /* WHoops! rows individually alloc! */
        int stridec = S->CHinvCinv[1] - S->CHinvCinv[0];

        mat_mul_tr(S->HinvC+i,S->C,S->CHinvCinv+i,rowcount,S->N,colcount); 

/*      DGEMM(&transa,&transb,&rowcount,&colcount,&S->N,&alpha,S->HinvC[i],
               &stridea,S->C[0],&strideb,&beta,S->CHinvCinv[i],&stridec);
*/
      }
    }    
    else 
#endif
      mat_mul_tr(S->HinvC,S->C,S->CHinvCinv,S->CN,S->N,S->CN);
  }

#ifdef BLAS
  if ( blas_flag )
    con_index = LD_block_factor(S->CHinvCinv,S->CN);
  else
#endif

/* Kludge here since mat_inv() doesn't calculate index correctly */
/*
  con_index = matrix_index(S->CHinvCinv,S->CN);
  mat_inv(S->CHinvCinv,S->CN);
*/
  con_index = mat_inv_sym(S->CHinvCinv,S->CN);

  if ( con_index < 0 )
     kb_error(1823,"Constraints not independent, or Hessian too singular.\n",
         RECOVERABLE);

adjust_index:
  /* adjust index for constraints */
  if ( augmented_hessian_mode )
  { S->neg -= (S->CN - S->degencon);
    S->pos -= (S->CN - S->degencon);
    S->zero -= S->degencon;
  }
  else 
  { S->neg -= con_index;
    S->pos -= S->CN - con_index;
  }
set_counts:
  eigen_neg = S->neg; eigen_pos = S->pos;

  PROF_FINISH(hessian_project_setup);
} /* end BK_hess_project_setup */

/*************************************************************************
*
* function: BK_hess_project()
* 
* purpose: project vector onto constraints of system using Hessian metric
*
*/

void BK_hess_project(S,B,x)
struct linsys *S; /* factored and constrained system */
REAL *B;    /* incoming vector */
REAL *x;    /* solution, may be incoming */
{ REAL *T1,*T2,*T3;
  int i;
  if ( S->CN == 0 ) return;
  T1 = (REAL *)temp_calloc(2*S->CN+S->N,sizeof(REAL));
  T2 = T1 + S->CN;
  T3 = T2 + S->CN;
  sp_aug_solve(S,B,NULL,T1,T2,T3);
  for ( i = 0 ; i < S->N ; i++ )
     x[i] = B[i] - T3[i];
  temp_free((char*)T1);
} /* end BK_hess_project */

/***********************************************************************
*
*  function: lowest_eigenpair()
*
*  purpose: find lowest eigenvalue and corresponding eigenvalue
*
*  return: lowest eigenvalue;
*/

REAL lowest_eigenpair(S,v)
struct linsys *S;
REAL *v; /* eigenvector, preallocated */
{ REAL lo;
  int old_quiet;

#ifdef XXX
  REAL old_ev,new_ev;
  int i;
  int krydim = 20;
  int nlook = 2;
  REAL evalues[5];
  REAL *vtmp = (REAL*)temp_calloc(S->N,sizeof(REAL));
#endif

  /* find lower bound on lowest eigenvalue */
  lo = -0.01;
  S->lambda = lo; 
  sp_factor(S);
  (*sp_hess_project_setup_func)(S);
  while ( S->neg > 0)
  { 
    S->lambda *= 10;
    sp_factor(S);
    (*sp_hess_project_setup_func)(S);
  }
  
  old_quiet = quiet_flag;
  quiet_flag = 1;
  do_ritz(S,S->lambda,1,&v);
  quiet_flag = old_quiet;
  return last_eigenvalue;
  
  #ifdef XXX
  //lanczos(S,krydim,evalues,nlook);

  /* inverse iteration to find eigenvector */
  S->lambda = evalues[0] - .0001;
  sp_factor(S);
  (*sp_hess_project_setup_func)(S);
  for ( i = 0 ; i < S->N ; i++ ) v[i] = drand48();
  old_ev = new_ev = 11231.0;  /* weird number, nonzero */
  do
  { sp_hessian_solve(S,v,vtmp,NO_SET_PRESSURE);
    old_ev = new_ev;
    new_ev = sqrt(dot(vtmp,vtmp,S->N));
    for ( i = 0 ; i < S->N ; i++ ) v[i] = vtmp[i]/new_ev;
  } while ( fabs(1/old_ev-1/new_ev) > 1000*machine_eps );
  temp_free((char*)vtmp);

  if ( S->zero != 0 ) last_eigenvalue = S->lambda;
  else
  last_eigenvalue = S->lambda + 1/new_ev;
  return S->lambda + 1/new_ev;
  #endif
} /* end lowest_eigenpair */


/***********************************************************************
*
*  function: cg_lowest_eigenpair()
*
*  purpose: find lowest eigenvalue and corresponding eigenvalue
*      Uses conjugate gradient on sphere to minimize XHX
*      Hessian_metric used.
*
*  return: lowest eigenvalue;
*/

REAL old_cg_lowest_eigenpair(S,x)
struct linsys *S;
REAL *x; /* eigenvector, preallocated */
{ REAL norm_inv;
  int i;
  REAL *h=NULL; /* search direction */
  REAL *ah=NULL; 
  REAL *f=NULL,*If=NULL; /* gradient of XAX, form and vector */
  REAL *mx=NULL; /* MX */
  REAL *mh=NULL; /* MH */
  REAL cgamma; /* cg coefficient */
  REAL xax,old_xax;
  int count;
  int maxcount;
  char response[100];
  REAL **CCinv=NULL;
  REAL *Cf=NULL,*Gf=NULL;

  h = (REAL *)temp_calloc(S->N,sizeof(REAL));
  ah = (REAL *)temp_calloc(S->N,sizeof(REAL));
  f = (REAL *)temp_calloc(S->N,sizeof(REAL));
  if ( web.area_norm_flag || hessian_linear_metric_flag ) 
    { mx = (REAL *)temp_calloc(S->N,sizeof(REAL));
      mh = (REAL *)temp_calloc(S->N,sizeof(REAL));
    }
  if ( S->CN )
    { CCinv = dmatrix(0,S->CN,0,S->CN);
      mat_mul_tr(S->C,S->C,CCinv,S->CN,S->N,S->CN);
      mat_inv(CCinv,S->CN);
      Cf = (REAL*)temp_calloc(S->CN,sizeof(REAL));
      Gf = (REAL*)temp_calloc(S->CN,sizeof(REAL));
    }

  /* initial random guess */
  for ( i = 0 ; i < S->N ; i++ ) x[i] = drand48() - .5;
  /* project to constraints */
  if ( S->CN )
  { matvec_mul(S->C,x,Cf,S->CN,S->N);
    matvec_mul(CCinv,Cf,Gf,S->CN,S->CN);
    for ( i = 0 ; i < S->CN ; i++ )
       vector_add_smul(x,S->C[i],-Gf[i],S->N);
  }
  if ( web.area_norm_flag || hessian_linear_metric_flag ) 
      norm_inv = 1/sqrt(sparse_metric_dot(x,x,&Met));
  else norm_inv = 1/sqrt(dot(x,x,S->N));
  for ( i = 0 ; i < S->N ; i++ ) x[i] *= norm_inv;

  xax = 1e30; /* silly value for start of convergence test */
  for(;;)
  { prompt("Enter max iterations: ",response,sizeof(response)); 
    maxcount = atoi(response);
    if ( maxcount == 0 ) break;
    if ( maxcount < 0 ) { count = 0 ; maxcount = -maxcount; }
    else count = 0;

    do
    { REAL a,b,c,evalue,denom,q2,q1comp,hnorm,hnorm_inv;
      old_xax = xax;

      count++;
      /* get cg search direction */
      (*sp_mul_func)(S,x,f);  /* AX */
      xax = dot(x,f,S->N);
      if ( web.area_norm_flag || hessian_linear_metric_flag ) 
          (*sp_mul_func)(&Met,x,mx);
      else mx = x;
      for ( i = 0 ; i < S->N ; i++ ) f[i] -= xax*mx[i];

      /* convert form f to vector If with cg metric I */
      If = f;
      /* project to constraints */
      if ( S->CN )
      { matvec_mul(S->C,If,Cf,S->CN,S->N);
        matvec_mul(CCinv,Cf,Gf,S->CN,S->CN);
        for ( i = 0 ; i < S->CN ; i++ )
          vector_add_smul(If,S->C[i],-Gf[i],S->N);
      }
      /* project If tangent to Steifel manifold */
      if ( web.area_norm_flag || hessian_linear_metric_flag ) 
         c = sparse_metric_dot(x,If,&Met);
      else c = dot(x,If,S->N);
      for ( i = 0 ; i < S->N ; i++ ) If[i] -= c*x[i];

      /* compute Ah */
      /* proper Hessian is A - XAX M */
      (*sp_mul_func)(S,h,ah);
      if ( web.area_norm_flag || hessian_linear_metric_flag ) 
          (*sp_mul_func)(&Met,h,mh);
      else mh = h;
      for ( i = 0 ; i < S->N ; i++ ) ah[i] -= xax*mh[i];

      /* compute gamma */
      if ( count <= 1 ) cgamma = 0.0;
      else
      { cgamma = -dot(If,ah,S->N)/dot(h,ah,S->N);
      }

      /* compute search direction */
      for ( i = 0 ; i < S->N ; i++ ) h[i] = If[i] + cgamma*h[i];

      if ( web.area_norm_flag || hessian_linear_metric_flag ) 
         hnorm = sqrt(sparse_metric_dot(h,h,&Met));
      else hnorm = sqrt(dot(h,h,S->N));
      hnorm_inv = 1/hnorm;

      /* recompute Ah */
      (*sp_mul_func)(S,h,ah); 
#ifdef XXX
projecting off XAX M seems to hurt 
      if ( web.area_norm_flag || hessian_linear_metric_flag ) 
    (*sp_mul_func)(&Met,h,mh);
      else mh = h;
      for ( i = 0 ; i < S->N ; i++ ) ah[i] -= xax*mh[i];
#endif

      /* find minimum along geodesic */
      a = xax;
      b = dot(x,ah,S->N)/hnorm;
      c = dot(h,ah,S->N)/hnorm/hnorm;
      evalue = 0.5*(a+c-sqrt((a-c)*(a-c)+4*b*b));  /* smallest ev */
      denom = sqrt(b*b + (evalue - a)*(evalue - a));
      if ( b < 0.0 ) denom = -denom;
      q2 = (evalue - a)/denom;
      q1comp = (evalue-a)*(evalue-a)/denom/(denom+b);
      /* rotate h and x */
      for ( i = 0 ; i < S->N ; i++ )
      { REAL xtmp = x[i]*hnorm;
        REAL htmp = h[i]*hnorm_inv;
        x[i] += -q1comp*x[i] + q2*htmp;
        h[i] += -q2*xtmp  -q1comp*h[i]; 
      }
      if ( (count < maxcount) && (maxcount > 5) && (count % (maxcount/5) == 0) )
      { 
#ifdef LONGDOUBLE
     sprintf(msg,"%3d.    %3.*Lg\n",count,DPREC,xax);
#else
     sprintf(msg,"%3d.    %3.17g\n",count,xax);
#endif 
     outstring(msg);
      }
    } while ( (fabs(xax - old_xax) > 10*machine_eps*fabs(xax)) && (count<maxcount) );
    if ( count < maxcount )
      { 
#ifdef LONGDOUBLE
     sprintf(msg,"%3d.    %3.*Lg  converged\n",count,DPREC,xax);
#else
     sprintf(msg,"%3d.    %3.17g  converged\n",count,xax);
#endif 
     outstring(msg);
      }
    else  { 
#ifdef LONGDOUBLE
     sprintf(msg,"%3d.    %3.*Lg  max iterations\n",count,DPREC,xax);
#else
     sprintf(msg,"%3d.    %3.17g  max iterations\n",count,xax);
#endif 
     outstring(msg);
      }
  }


  temp_free((char*)h);
  temp_free((char*)ah);
  temp_free((char*)f);
  if ( web.area_norm_flag || hessian_linear_metric_flag ) 
  { temp_free((char*)mx);
    temp_free((char*)mh);
  }
  if ( S->CN )
  { temp_free((char*)Gf);
    temp_free((char*)Cf);
    free_matrix(CCinv);
  }
  last_eigenvalue = xax;
  return xax;
} /* end old_cg_lowest_eigenpair */

/***********************************************************************
*
*  function: cg_lowest_eigenpair()
*
*  purpose: find lowest eigenvalue and corresponding eigenvalue
*      Uses conjugate gradient on sphere to minimize XHX
*      Hessian_metric used.
*
*  return: lowest eigenvalue;
*/

REAL cg_lowest_eigenpair(S,x)
struct linsys *S;
REAL *x; /* eigenvector, preallocated */
{ REAL norm_inv;
  int i,j;
  REAL *h; /* search direction */
  REAL *ah;  /* (A-XAX M)H */
  REAL *ax; /* AX */
  REAL *mx; /* MX */
  REAL *mh; /* MH */
  REAL xax,old_xax;
  int count;
  int maxcount;
  char response[100];
  REAL *Cf,*Gf;
  REAL **P; /* matrix of coeff for Lagrange mult */
  REAL **Pinv; /* inverse of P */

  h = (REAL *)temp_calloc(S->N,sizeof(REAL));
  ah = (REAL *)temp_calloc(S->N,sizeof(REAL));
  ax = (REAL *)temp_calloc(S->N,sizeof(REAL));
  if ( web.area_norm_flag || hessian_linear_metric_flag ) 
    { mx = (REAL *)temp_calloc(S->N,sizeof(REAL));
      mh = (REAL *)temp_calloc(S->N,sizeof(REAL));
    }
  else { mx = x; mh = h; }
  P = dmatrix(0,S->CN+1,0,S->CN+1);
  Pinv = dmatrix(0,S->CN+1,0,S->CN+1);
  Cf = (REAL*)temp_calloc(S->CN+2,sizeof(REAL));
  Gf = (REAL*)temp_calloc(S->CN+2,sizeof(REAL));
  if ( S->CN )
    { /* CGC goes in upper left corner of P */
      mat_mul_tr(S->C,S->C,P,S->CN,S->N,S->CN);
    }

  /* initial random guess */
  for ( i = 0 ; i < S->N ; i++ ) x[i] = drand48() - .5;
  /* project to constraints */
  if ( S->CN )
    { matvec_mul(S->C,x,Cf,S->CN,S->N);
      matcopy(Pinv,P,S->CN,S->CN);
      mat_inv(Pinv,S->CN);
      matvec_mul(Pinv,Cf,Gf,S->CN,S->CN);
      for ( i = 0 ; i < S->CN ; i++ )
      vector_add_smul(x,S->C[i],-Gf[i],S->N);
    }
  if ( web.area_norm_flag || hessian_linear_metric_flag ) 
      norm_inv = 1/sqrt(sparse_metric_dot(x,x,&Met));
  else norm_inv = 1/sqrt(dot(x,x,S->N));
  for ( i = 0 ; i < S->N ; i++ ) x[i] *= norm_inv;

  xax = 1e30; /* silly value for start of convergence test */
  for(;;)
  { prompt("Enter max iterations: ",response,sizeof(response)); 
    maxcount = atoi(response);
    if ( maxcount == 0 ) break;
    if ( maxcount < 0 ) { count = 0 ; maxcount = -maxcount; }
    else count = 0;

    do
    { REAL a,b,c,evalue,denom,q2,q1comp,hnorm,hnorm_inv;
      int pdim;

      old_xax = xax;

      count++;

      /* get cg search direction */

      /* calculate needed vectors */

      /* MX */
      if ( web.area_norm_flag || hessian_linear_metric_flag ) 
         (*sp_mul_func)(&Met,x,mx);

      /* MH */
      if ( web.area_norm_flag || hessian_linear_metric_flag ) 
         (*sp_mul_func)(&Met,h,mh);
      
      /* (A - XAX M)X */
      (*sp_mul_func)(S,x,ax); 
      xax = dot(x,ax,S->N);
      for ( i = 0 ; i < S->N ; i++ ) ax[i] -= xax*mx[i];

      /* (A - XAX M)H */
      (*sp_mul_func)(S,h,ah); 
      for ( i = 0 ; i < S->N ; i++ ) ah[i] -= xax*mh[i];

      /* fill in P, which already has CGC in upper left */
      if ( S->CN > 0 )
      { matvec_mul(S->C,mx,P[S->CN],S->CN,S->N);
        matvec_mul(S->C,ah,P[S->CN+1],S->CN,S->N);
      }
      P[S->CN][S->CN] = dot(mx,mx,S->N);
      P[S->CN+1][S->CN] = dot(ah,mx,S->N);

#ifdef SYMMETRICWAY
      /* this way takes twice as long */
      P[S->CN+1][S->CN+1] = dot(ah,ah,S->N);
      for ( i = 0 ; i <= S->CN ; i++ )
        for ( j = S->CN ; j < S->CN+2 ; j++ )
           P[i][j] = P[j][i];
#else
      P[S->CN+1][S->CN+1] = dot(ah,h,S->N);
      P[S->CN][S->CN+1] = dot(mx,h,S->N);
      for ( j = 0 ; j < S->CN ; j++ )
         P[j][S->CN+1] = dot(h,S->C[j],S->N);
#endif
      
      /* fill in right side */
      if ( S->CN > 0 ) matvec_mul(S->C,ax,Cf,S->CN,S->N);
      Cf[S->CN] = dot(mx,ax,S->N);
      Cf[S->CN+1] = dot(ah,ax,S->N);

      /* solve */ 
      pdim = S->CN + ( count<=1 ? 1 : 2 );
      matcopy(Pinv,P,pdim,pdim);
      mat_inv(Pinv,pdim);
      matvec_mul(Pinv,Cf,Gf,pdim,pdim);

      /* compute new search direction h */
      for ( i = 0 ; i < S->N ; i++ )
      { 
#ifdef SYMMETRICWAY
        if ( count > 1 ) h[i] = -Gf[S->CN+1]*ah[i]; else h[i] = 0.0;
#else
        if ( count > 1 ) h[i] = -Gf[S->CN+1]*h[i]; else h[i] = 0.0;
#endif
        h[i] += ax[i] - Gf[S->CN]*mx[i];
        for ( j = 0 ; j < S->CN ; j++ )
          h[i] -= Gf[j]*S->C[j][i];
      }

      /* recompute Ah */
      (*sp_mul_func)(S,h,ah); 
      if ( web.area_norm_flag || hessian_linear_metric_flag ) 
        hnorm = sqrt(sparse_metric_dot(h,h,&Met));
      else hnorm = sqrt(dot(h,h,S->N));
      hnorm_inv = 1/hnorm;

      /* find minimum along geodesic */
      a = xax;
      b = dot(x,ah,S->N)/hnorm;
      c = dot(h,ah,S->N)/hnorm/hnorm;
      evalue = 0.5*(a+c-sqrt((a-c)*(a-c)+4*b*b));  /* smallest ev */
      denom = sqrt(b*b + (evalue - a)*(evalue - a));
      if ( b < 0.0 ) denom = -denom;
      q2 = (evalue - a)/denom;
      q1comp = (evalue-a)*(evalue-a)/denom/(denom+b); 
      /* rotate h and x */
      for ( i = 0 ; i < S->N ; i++ )
      { REAL xtmp = x[i]*hnorm;
        REAL htmp = h[i]*hnorm_inv;
        x[i] += -q1comp*x[i] + q2*htmp;
        h[i] += -q2*xtmp  -q1comp*h[i]; 
      }
      if ( (count < maxcount) && (maxcount > 5) && (count % (maxcount/5) == 0) )
      { 
#ifdef LONGDOUBLE
     sprintf(msg,"%3d.    %3.*Lg\n",count,DPREC,xax);
#else
     sprintf(msg,"%3d.    %3.17g\n",count,xax);
#endif 
     outstring(msg);
      }
    } while ( (fabs(xax - old_xax) > 10*machine_eps*fabs(xax)) && (count<maxcount) );
    if ( count < maxcount )
      { 
#ifdef LONGDOUBLE
     sprintf(msg,"%3d.    %3.*Lg  converged\n",count,DPREC,xax);
#else
     sprintf(msg,"%3d.    %3.17g  converged\n",count,xax);
#endif 
     outstring(msg);
      }
    else  { 

#ifdef LONGDOUBLE
     sprintf(msg,"%3d.    %3.*Lg  max iterations\n",count,DPREC,xax);
#else
     sprintf(msg,"%3d.    %3.17g  max iterations\n",count,xax);
#endif 
     outstring(msg);
      }
  }


  temp_free((char*)h);
  temp_free((char*)ah);
  temp_free((char*)ax);
  if ( web.area_norm_flag || hessian_linear_metric_flag ) 
  { temp_free((char*)mx);
    temp_free((char*)mh);
  }
  temp_free((char*)Gf);
  temp_free((char*)Cf);
  free_matrix(P);
  free_matrix(Pinv);
  last_eigenvalue = xax;
  return xax;
} /* end cg_lowest_eigenpair */

/***********************************************************************
*
*  function: cg_ritz()
*
*  purpose: find lowest eigenvalues and corresponding eigenvalues
*      Uses conjugate gradient on XMX=I to minimize Tr(XHX)
*      Hessian_metric M used.
*
*/

void cg_ritz(S,n,x,ev)
struct linsys *S;
int n;  /* number of eigenpairs desired */
REAL **x; /* eigenvectors, preallocated */
REAL *ev; /* eigenvalues, preallocated */
{ 
  int i,j,k;
  REAL **h=NULL; /* search direction */
  REAL **ah=NULL; 
  REAL **f=NULL,**If=NULL; /* gradient of XAX, form and vector */
  REAL **mx=NULL; /* MX */
  REAL **mh=NULL; /* MH */
  REAL cgamma; /* cg coefficient */
  REAL **xax=NULL;
  REAL trxax,old_trxax; /* objective */
  int count;
  int maxcount;
  char response[100];
  REAL **CCinv=NULL;
  REAL **Cf=NULL,**Gf=NULL;
  struct linsys *M;  /* Hessian metric to use */
  REAL **L,**Linv; /* for LQ decomp */
  REAL **Q;
  REAL **hax,**xah,**hah;
  REAL **B;  /* 2n x 2n matrix to find low eigenvalues for */
  REAL **hx,**xh;
  REAL **evectors,*evalues;
  REAL **NN; /* temp */
  REAL *work;

  if ( web.area_norm_flag || hessian_linear_metric_flag ) M =  &Met;
  else M = NULL;

  if ( n <= 0 ) return;

  h = dmatrix(0,n-1,0,S->N);
  ah = dmatrix(0,n-1,0,S->N);
  f = dmatrix(0,n-1,0,S->N);
  Q = dmatrix(0,n-1,0,S->N);
  hx = dmatrix(0,2*n-1,0,S->N);
  xh = (REAL**)temp_calloc(2*n,sizeof(REAL*));
  work = (REAL*)temp_calloc(2*n,sizeof(REAL));
  for ( i = 0 ; i < n ; i++ ) { xh[i] = x[i]; xh[i+n] = Q[i]; }
  NN = dmatrix(0,n-1,0,n-1);
  L = dmatrix(0,n-1,0,n-1);
  Linv = dmatrix(0,n-1,0,n-1);
  B = dmatrix(0,2*n-1,0,2*n-1);
  xax = B;
  hax = B+n;
  xah = (REAL**)temp_calloc(n,sizeof(REAL*));
  hah = (REAL**)temp_calloc(n,sizeof(REAL*));
  for ( j = 0 ; j < n ; j++ ) { xah[j] = B[j]+n; hah[j] = B[j+n]+n; }
  evalues = (REAL *)temp_calloc(2*n,sizeof(REAL));
  evectors = dmatrix(0,2*n-1,0,2*n-1);
  if ( web.area_norm_flag || hessian_linear_metric_flag ) 
    { mx = dmatrix(0,n-1,0,S->N);
      mh = dmatrix(0,n-1,0,S->N);
    }
  if ( S->CN )
    { CCinv = dmatrix(0,S->CN,0,S->CN);
      mat_mul_tr(S->C,S->C,CCinv,S->CN,S->N,S->CN);
      mat_inv(CCinv,S->CN);
      Cf = dmatrix(0,S->CN-1,0,n-1);
      Gf = dmatrix(0,S->CN-1,0,n-1); 
    }

  /* initial random guess */
  for ( j = 0 ; j < n ; j++ )
    for ( i = 0 ; i < S->N ; i++ ) x[j][i] = drand48() - .5;
  /* project to constraints */
  if ( S->CN )
    { mat_mul_tr(S->C,x,Cf,S->CN,S->N,n);
      mat_mult(CCinv,Cf,Gf,S->CN,S->CN,n);
      for ( j = 0 ; j < n ; j++ )
       for ( i = 0 ; i < S->CN ; i++ )
        vector_add_smul(x[j],S->C[i],-Gf[i][j],S->N);
    }
  /* project to Steifel manifold XMX = I */
  LQ_decomp(x,n,S->N,x,L,M);

  trxax = 1e30; /* silly value for start of convergence test */
  for(;;)
  { prompt("Enter max iterations: ",response,sizeof(response)); 
    maxcount = atoi(response);
    if ( maxcount == 0 ) break;
    if ( maxcount < 0 ) { count = 0 ; maxcount = -maxcount; }
    else count = 0;

    do
    { REAL denom;
      old_trxax = trxax;

      count++;
      /* get cg search direction */
      for ( j = 0 ; j < n ; j++ )
        (*sp_mul_func)(S,x[j],f[j]);  /* AX */
      mat_mul_tr(x,f,xax,n,S->N,n);
      if ( web.area_norm_flag || hessian_linear_metric_flag ) 
        for ( j = 0 ; j < n ; j++ )
          (*sp_mul_func)(&Met,x[j],mx[j]);
      else mx = x;

      mat_mult(xax,mx,ah,n,n,S->N);  /* ah just used as temp */
      for ( j = 0 ; j < n ; j++ )
        for ( i = 0 ; i < S->N ; i++ ) f[j][i] -= ah[j][i];

      /* convert form f to vector If with cg metric I */
      If = f;
      /* project to constraints */
      if ( S->CN )
      { mat_mul_tr(S->C,If,Cf,S->CN,S->N,n);
        mat_mult(CCinv,Cf,Gf,S->CN,S->CN,n);
        for ( j = 0 ; j < n ; j++ )
         for ( i = 0 ; i < S->CN ; i++ )
          vector_add_smul(If[j],S->C[i],-Gf[i][j],S->N);
      }
      /* project If tangent to Steifel manifold */
      mat_mul_tr(mx,If,NN,n,S->N,n); 
      tr_mat_mul(NN,x,ah,n,n,S->N);
      for ( j = 0 ; j < n ; j++ )
        for ( i = 0 ; i < S->N ; i++ ) If[j][i] -= ah[j][i];

      if ( count <= 1 ) cgamma = 0.0;
      else
      { /* proper Hessian is A - XAX M */
        REAL numer;
        for ( j = 0 ; j < n ; j++ )
          (*sp_mul_func)(S,h[j],ah[j]);
        if ( web.area_norm_flag || hessian_linear_metric_flag ) 
         for ( j = 0 ; j < n ; j++ )
          (*sp_mul_func)(&Met,h[j],mh[j]);
        else mh = h;
        for ( j = 0 ; j < n ; j++ )
         for ( i = 0 ; i < S->N ; i++ ) 
          for ( k = 0 ; k < n ; k++ ) ah[j][i] -= xax[j][k]*mh[k][i];
           for ( j = 0, numer = denom = 0.0 ; j < n ; j++ )
           { numer += dot(If[j],ah[j],S->N);
             denom += dot(h[j],ah[j],S->N);
           }
        cgamma = -numer/denom;
      }
      for ( j = 0 ; j < n ; j++ )
        for ( i = 0 ; i < S->N ; i++ ) h[j][i] = If[j][i] + cgamma*h[j][i];

      /* normalize h to be orthonormal wrt M, hMh = I */
      LQ_decomp(h,n,S->N,Q,L,M); 
      matcopy(Linv,L,n,n);
      mat_inv(Linv,n);

      /* compute Ah */
      for ( j = 0 ; j < n ; j++ )
       (*sp_mul_func)(S,h[j],ah[j]);

      /* load up matrix */
      /* B = | xax xah |  */
      /*      | hax hah |  */
      mat_mul_tr(h,ah,hah,n,S->N,n);
      mat_mult(Linv,hah,NN,n,n,n);
      mat_mul_tr(NN,Linv,hah,n,n,n);
      mat_mul_tr(Q,f,hax,n,S->N,n);
      for ( i = 0 ; i < n ; i++ )
       for ( j = 0 ; j < n ; j++ )
         xah[i][j] = hax[j][i];
      jacobi_eigenpairs(B,2*n,evalues,evectors,work);
      /* returns eigenvectors in columns of evectors, in descending order */

      /* use lowest n eigenvectors */
      for ( j = n, trxax = 0.0; j < 2*n ; j++ ) trxax += evalues[j];

      /* rotate h and x */
      tr_mat_mul(evectors,xh,hx,2*n,2*n,S->N);
      matcopy(x,hx+n,n,S->N);
      mat_mult(L,hx,h,n,n,S->N);

      if ( (count < maxcount) && (maxcount > 5) && (count % (maxcount/5) == 0) )
      { sprintf(msg,"%3d.  ",count);
          for ( j = 0 ; (j < 4)&&(j<n) ; j++ )
#ifdef LONGDOUBLE
              sprintf(msg+strlen(msg)," %*.*Lg",DWIDTH,DPREC,evalues[2*n-1-j]);
#else
              sprintf(msg+strlen(msg)," %18.15g",evalues[2*n-1-j]);
#endif 
        strcat(msg,"\n");
        outstring(msg);
      }
    } while ( (fabs(trxax-old_trxax) > 10*machine_eps*fabs(trxax)) && (count<maxcount) );
    if ( count < maxcount )
      { 
#ifdef LONGDOUBLE
     sprintf(msg,"%3d.    %3.*Lg  converged\n",count,DPREC,trxax);
#else
     sprintf(msg,"%3d.    %3.17g  converged\n",count,trxax);
#endif 
     outstring(msg);
      }
    else  { 
#ifdef LONGDOUBLE
     sprintf(msg,"%3d.    %3.*Lg  max iterations\n",count,DPREC,trxax);
#else
     sprintf(msg,"%3d.    %3.17g  max iterations\n",count,trxax);
#endif 
     outstring(msg);
      }
  }
  if ( S->zero != 0 ) last_eigenvalue = S->lambda;
  else
  last_eigenvalue = evalues[0];

  free_matrix(h);
  free_matrix(ah);
  free_matrix(hx);
  free_matrix(f);
  free_matrix(B);
  free_matrix(L);
  free_matrix(Q);
  free_matrix(Linv);
  temp_free((char*)hah);
  temp_free((char*)xah);
  temp_free((char*)evalues);
  temp_free((char*)work);
  free_matrix(evectors);

  if ( web.area_norm_flag || hessian_linear_metric_flag ) 
  { free_matrix(mx);
    free_matrix(mh);
  }
  if ( S->CN )
  { free_matrix(Gf);
    free_matrix(Cf);
    free_matrix(CCinv);
  }
} /* end cg_ritz() */

/************************************************************************
*
* function: sp_hessian_solve()
*
* purpose: solve hessian with constraints
*
*/

void sp_hessian_solve(S,rhs,X,set_pressure_flag)
struct linsys *S;
REAL *rhs;
REAL *X;  /* solution */
int set_pressure_flag; /* whether to set body pressures */
{ /* solve stuff */
  int i,j,ii,jj;
  REAL *T1,*T2,*T3;
  body_id b_id;
  struct gen_quant*q;
 
  if ( (S->CN > 0) && augmented_hessian_mode )
    { /* Kludge since volume deltas got in with wrong sign */
      for ( i = S->N - S->CN ; i < S->N ; i++ )
        rhs[i] = - rhs[i];
    }

  sp_solution(S,rhs,X);

  /* check decrease in residual */
  if ( itdebug || hess_debug )
  { REAL *xrhs = (REAL *)temp_calloc(S->maxN,sizeof(REAL));
    REAL rhsmag = 0.0;
    REAL resmag = 0.0;
    sp_hessian_mult(S,X,xrhs);
    
    for ( i = 0 ; i < S->N ; i++ )
    { rhsmag += rhs[i]*rhs[i];
      resmag += (rhs[i] - xrhs[i])*(rhs[i] - xrhs[i]);
    }
    if ( resmag > rhsmag/10 )
    { sprintf(errmsg,"Hessian solve only reduced residual from %g to %g.\n",
             sqrt(rhsmag),sqrt(resmag));
      kb_error(2561,errmsg,WARNING);
    }
    temp_free((char*)xrhs);
  }

  if ( S->CN > 0 ) /* have constraints */ 
  { if ( augmented_hessian_mode )
    { /* remap Lagrange multipliers*/
      for ( i = 0 ; i < S->concount ; i++ )
        if ( S->coninx[i] >= 0 )
          pressures[i] = -X[S->N - S->CN + S->coninx[i]];
    }
    else 
    { REAL *con_rhs = (REAL*)temp_calloc(S->CN,sizeof(REAL));
     for ( i = 0 ; i < S->concount ; i++ )
     { j = S->coninx[i];
       if ( j >= 0 ) con_rhs[j] = rhs[S->N+i];
     }
     T1 = (REAL*)temp_calloc(S->CN,sizeof(REAL));
     T2 = (REAL*)temp_calloc(S->CN,sizeof(REAL));
     T3 = (REAL*)temp_calloc(S->N,sizeof(REAL));

     sp_aug_solve(S,X,con_rhs,T1,T2,T3);

     for ( i = 0 ; i < S->N ; i++ )
       X[i] -= T3[i];
     for ( i = 0 ; i < S->CN ; i++ ) con_rhs[i] = -T2[i];
     
     /* remap Lagrange multipliers*/
     for ( i = 0 ; i < S->concount ; i++ )
        if ( S->coninx[i] >= 0 )
          pressures[i] = con_rhs[S->coninx[i]];
     temp_free((char*)T1);
     temp_free((char*)T2);
     temp_free((char*)T3);
     temp_free((char*)con_rhs);
    }

    if ( set_pressure_flag == SET_PRESSURE )
    {  if ( ! everything_quantities_flag )
        FOR_ALL_BODIES(b_id)
        { REAL p = get_body_pressure(b_id)+pressures[loc_ordinal(b_id)];
          set_body_pressure(b_id,p);
        }
       for ( i = 0 ; i < gen_quant_count ; i++ )
       { q = GEN_QUANT(i);
         if ( q->flags & (Q_FIXED|Q_CONSERVED) )
         { q->pressure += pressures[everything_quantities_flag ? i :
                     web.skel[BODY].max_ord+1+i];
           if ( valid_id(q->b_id) )
           set_body_pressure(q->b_id,q->pressure);
         }
       }
    }
  }
  if ( hess_debug )
  { 
    #ifdef MPI_EVOLVER
    for ( ii = 0 ; ii < S->N ; ii++ )
      printf("    %20.15f  X[%d] \n",(DOUBLE)X[ii],ii);
    #else
    vertex_id v_id;
    MFOR_ALL_VERTICES(v_id)
    { struct hess_verlist *vh = get_vertex_vhead(v_id);
      for ( j = 0 ; j < vh->freedom ; j++ )
      { ii = vh->rownum+j;
        printf("    %20.15f  X[%d] v%d.%d\n",(DOUBLE)X[ii],ii,ordinal(v_id)+1,j+1);
      } 
    }
    #endif
    for ( i = 0 ; i < optparamcount ; i++ )
    { ii = optparam[i].pnum;
      jj = optparam[i].rownum;
      printf("    %f  X[%d] %s\n",(DOUBLE)X[jj],jj,globals(ii)->name);
    }
  }
} /* end sp_hessian_solve */

/************************************************************************
*
* function: sp_hessian_solve_multi()
*
* purpose: solve hessian with constraints for multiple rhs
*
*/

void sp_hessian_solve_multi(S,rhs,X,rk)
struct linsys *S;
REAL **rhs;
REAL **X;  /* solution */
int rk;    /* number of right sides */
{ /* solve stuff */
  int k;
 
  for ( k = 0 ; k < rk ; k++ )
    sp_hessian_solve(S,rhs[k],X[k],0);

#ifdef oldstuff
  (*sp_solve_multi_func)(S,rhs,X,rk);

  if ( S->CN > 0 ) /* have constraints */
  { REAL **con_rhs = dmatrix(0,rk-1,0,S->CN);
     for ( i = 0 ; i < S->concount ; i++ )
     { int j = S->coninx[i];
       if ( j >= 0 ) 
         for ( k = 0 ; k < rk ; k++ ) con_rhs[k][j] = rhs[k][S->N+i];
     }
     T1 = (REAL*)temp_calloc(S->CN,sizeof(REAL));
     T2 = (REAL*)temp_calloc(S->CN,sizeof(REAL));
     T3 = (REAL*)temp_calloc(S->N,sizeof(REAL));

     for ( k = 0 ; k < rk ; k++ )
     { 
       matvec_mul(S->C,X[k],T1,S->CN,S->N);
       for ( i = 0 ; i < S->CN ; i++ )
         T1[i] += con_rhs[k][i]; /* corrections */
#ifdef BLAS
       if ( blas_flag )
          LD_block_solve(S->CHinvCinv,T1,T2,S->CN);
       else
#endif
       matvec_mul(S->CHinvCinv,T1,T2,S->CN,S->CN); /* Lagrange multipliers */
       vec_mat_mul(T2,S->HinvC,T3,S->CN,S->N);
       for ( i = 0 ; i < S->N ; i++ )
         X[k][i] -= T3[i];
       for ( i = 0 ; i < S->CN ; i++ ) con_rhs[k][i] = -T2[i];
     }
     /* conrhs not used?? */

     temp_free((char*)T1);
     temp_free((char*)T2);
     temp_free((char*)T3);
     free_matrix(con_rhs);
  }
#endif

} /* end sp_hessian_solve_multi */


/*******************************************************************************
*
*  function: bk_eigenprobe()
*
*  purpose: find number of eigenvalues less than, equal, or greater than
*      desired value.  Asks user for probe values.
*/

void bk_eigenprobe(S)
struct linsys *S;
{ 
  for(;;)
  { char response[100];
    prompt("Enter probe value: ",response,sizeof(response));
    if ( (response[0] == 0) || (response[0] == 'q') ) break;
    S->lambda = atof(response);
    sp_factor(S);
    (*sp_hess_project_setup_func)(S);
    sprintf(msg,"Eigencounts:    %d <,  %d ==,  %d > \n",S->neg,S->zero,S->pos);
    outstring(msg);
  }
  return;
} /* end bk_eigenprobe */

/*******************************************************************************
*
*  function: bk_inverse_it()
*
*  purpose: Find eigenvector near probe value.
*/

void bk_inverse_it(S,V)
struct linsys *S;
REAL *V; /* for eigenvector return */
{ int i,ii,k,its;
  REAL t,oldt;
  char response[100];
  REAL *W = NULL, *WW = NULL; /* for use when metric is needed */

  prompt("Enter probe value: ",response,sizeof(response));
  if ( (response[0] == 0) || (response[0] == 'q') ) return;
  S->lambda = atof(response);
  sp_factor(S);
  (*sp_hess_project_setup_func)(S);
  sprintf(msg,"Eigencounts:    %d <,  %d ==,  %d > \n",S->neg,S->zero,S->pos);
  outstring(msg);

  if ( web.area_norm_flag || hessian_linear_metric_flag ) 
     W = (REAL*)temp_calloc(S->N,sizeof(REAL));
  /* inverse iteration */
  /* random starting vector */
  for ( i = 0 ; i < S->N ; i++ ) V[i] = drand48();
  t = 1/sqrt(dot(V,V,S->N));
  for ( i = ii = 0 ; i < S->N ; i++,ii++ ) V[i] *= t;
  prompt("Enter maximum iterations: ",response,sizeof(response));
  its = atoi(response);
  if ( its == 0 ) goto afterits;
  oldt = 1e30; /* for convergence test */
  for ( k = 0, ii=0 ; k < its ; k++,ii++ )
  { REAL oldv0=1.0; /* for sign */
     int oldvi=0;     /* index of nonzero component */
     REAL eps = 1/sqrt((REAL)(S->N))/2;
     for ( i = 0, oldv0 = 0.0 ; i < S->N ; i++ ) 
       if ( fabs(V[i]) > eps ) { oldvi = i; oldv0 = V[i]; break; }
     if ( web.area_norm_flag || hessian_linear_metric_flag ) 
     {  (*sp_mul_func)(&Met,V,W); WW = W; }
     else WW = V;
     sp_hessian_solve(S,WW,V,NO_SET_PRESSURE);
     if ( web.area_norm_flag || hessian_linear_metric_flag ) 
       t = 1/sqrt(sparse_metric_dot(V,V,&Met));
     else t = 1/sqrt(dot(V,V,S->N));
     if ( V[oldvi]*oldv0 < 0. ) t = -t;  /* get sign right */
#ifdef LONGDOUBLE
     if ( k % 10 == 0 ) printf("%d  ev = %*.*Lf\n",k,DWIDTH,DPREC,S->lambda+t);
#else
     if ( k % 10 == 0 ) printf("%d  ev = %20.15f\n",k,S->lambda+t);
#endif 
     for ( i = 0 ; i < S->N ; i++ ) V[i] *= t;
     if ( fabs(t-oldt) <= 100*machine_eps*fabs(t) ) 
      { /* does twice as many iterations as needed to get t converged */
        /* so eigenvector gets fully converged also */
        ii -= 2;
        if ( ii <= 0 )break;
      }
     oldt = t;
  }
#ifdef LONGDOUBLE
  printf("%d  ev = %*.*Lf\n",k,DWIDTH,DPREC,S->lambda+t);
#else
  printf("%d  ev = %20.15f\n",k,S->lambda+t);
#endif 
afterits:
  if ( web.area_norm_flag || hessian_linear_metric_flag ) 
      temp_free((char*)W);
  return;
} /* end bk_inverse_it() */

/*************************************************************************
*
* function: sp_Hessian_solver()
*
* purpose;  Set up and solve sparse Hessian with given right hand side.
*/
void sp_Hessian_solver(S,rhs,Xptr)
struct linsys *S;  /* system */
REAL *rhs; /* right side */
REAL **Xptr; /* returned vector */
{
  REAL *X;
  
  if ( *Xptr == NULL )
    *Xptr = (REAL*)temp_calloc(S->N,sizeof(REAL));
  X = *Xptr;

  (*sp_AIJ_setup_func)(S->A_rows,S);
  (*sp_constraint_setup_func)
     (web.skel[BODY].max_ord+1 + gen_quant_count,S);
  if ( sp_ordering_func ) (*sp_ordering_func)(S);
  sp_factor(S);
  (*sp_hess_project_setup_func)(S);

  if ( hess_debug )
  { int i;
    puts("rhs:");
    for ( i = 0 ; i < S->N ; i++ ) printf("%g \n",(DOUBLE)rhs[i]);
  }

  sp_hessian_solve(S,rhs,X,SET_PRESSURE);
  if ( S->neg > 0 )
    { sprintf(msg,"Hessian not positive definite. Index: %d\n",S->neg);
      kb_error(1825,msg,WARNING);
    } 

  if ( hess_debug )
  { int i;
    REAL *out = (REAL*)temp_calloc(S->N,sizeof(REAL));
    puts("X:");
    for ( i = 0 ; i < S->N ; i++ ) printf("%g \n",(DOUBLE)X[i]);
    bk_mul(S,X,out);
    puts("check:");
    for ( i = 0 ; i < S->N ; i++ ) printf("%g \n",(DOUBLE)out[i]);
    temp_free((char*)out);
  }
} /* end sp_hessian_solver() */

/**************************************************************************
*
* function: sparse_metric_dot()
*
* purpose: Take dot product of vectors when metric is sparse linear system.
*     Metric sparse storage has diagonal element first in each row.
*
*/

REAL sparse_metric_dot(u,v,M)
REAL *u,*v; /* the vectors to be dotted */
struct linsys *M;  /* the metric */
{ int i,j,end;
  REAL sum;

  sum = 0.0;
  for ( i = 0 ; i < M->N ; i++ )
  { j = M->IA[i]-A_OFF;
    end = M->IA[i+1]-A_OFF;
    sum += M->A[j]*u[i]*v[i];
    for ( j++  ; j < end  ; j++ )
     { int ii = M->JA[j]-A_OFF;
       sum += M->A[j]*(u[i]*v[ii] + u[ii]*v[i]);
     }
  }
  return sum;
} /* end sparse_metric_dot() */


/*************************************************************************
*
* function: sp_CHinvC()
* 
* purpose: Find inner product of matrix using hessian inverse as metric.
*          C stored sparsely in S.
*
*          Want to use sparsity in case of large CN.  So solve H X = C
*          for one constraint at a time, and sparse-multiply the result
*          by the rest of the constraint rows.  Avoids forming full
*          CN x N matrix.
*/

void sp_CHinvC(S)
struct linsys *S; /* factored system */
{
  int n; /* row index */
  int i,j,k;
  REAL *BB,*Y;


  if ( S->P == NULL )
   kb_error(2530,"Internal error: Must call sp_factor before sp_CHinvC.\n",
     RECOVERABLE);

  BB = (REAL*)temp_calloc(S->N,sizeof(REAL));  /* expanded rhs */
  Y = (REAL*)temp_calloc(S->N,sizeof(REAL));  /* solution */

  for ( i = 0 ; i < S->CN ; i++ )
  {
    /* solve H Y = C */
    memset(BB,0,S->N*sizeof(REAL));
    for ( n = S->CIA[i] ; n < S->CIA[i+1] ; n++ ) 
      BB[S->CJA[n]] = S->CA[n]; 
    sp_solution(S,BB,Y);

    /* multiply by rows of C */
    for ( j = 0 ; j <= i ; j++ )
    { REAL sum;
      for ( k = S->CIA[j] , sum = 0.0 ; k < S->CIA[j+1] ; k++ )
        sum += S->CA[k]*Y[S->CJA[k]];
      S->CHinvCinv[i][j] = sum;
      if ( ! blas_flag )
        S->CHinvCinv[j][i] = sum; /* symmetric */
    }
  }

  temp_free((char*)BB);
  temp_free((char*)Y);
}

/*************************************************************************
*
*  function: sp_solution()
*
*  purpose: solve system possibly in sparse + low rank update
*/

void sp_solution(S,rhs,X)
struct linsys *S;  /* factored sparse matrix */
REAL *rhs; /* right side */
REAL *X;   /* for solution */
{
  REAL *Y; /* working full-size vector */
  REAL *Z; /* low rank working vector */
  REAL *W; /* low rank working vector */
  int i;

  if ( S->low_rank == 0 )
  { /* no low-rank update */
    (*sp_solve_func)(S,rhs,X);
    return;
  }

  /* rest is solution of system with low rank update */
  Y = (REAL*)temp_calloc(S->N,sizeof(REAL));
  Z = (REAL*)temp_calloc(S->low_rank, sizeof(REAL));
  W = (REAL*)temp_calloc(S->low_rank, sizeof(REAL));
  (*sp_solve_func)(S,rhs,Y);
  matvec_mul(S->low_rank_vectors,Y,Z,S->low_rank,S->low_rank_vecsize);
  matvec_mul(S->low_rank_form,Z,W,S->low_rank,S->low_rank);
  matvec_mul(S->low_rank_inverse_form,W,Z,S->low_rank,S->low_rank);
  vec_mat_mul(Z,S->low_rank_vectors,Y,S->low_rank,S->low_rank_vecsize);
  for ( i = 0 ;  i < S->low_rank_vecsize ; i++ )
    Y[i] = rhs[i] - Y[i];
  for ( ; i < S->N ; i++ )
    Y[i] = rhs[i];
  (*sp_solve_func)(S,Y,X);

  temp_free((char*)Y);
  temp_free((char*)Z);
  temp_free((char*)W);
}
  
/**************************************************************************
*
* function: sp_factor()
*
* purpose: factor sparse matrix, and set up low-rank corrections if present.
*
*/

void sp_factor(S)
struct linsys *S;
{ 

  (*sp_factor_func)(S);  /* factor just the matrix */

  if ( S->low_rank )
  { REAL **W = dmatrix(0,S->low_rank-1,0,S->N-1);
    REAL **Z = dmatrix(0,S->low_rank-1,0,S->low_rank-1);
    int i;

    (*sp_solve_multi_func)(S,S->low_rank_vectors,W,S->low_rank);
    mat_mul_tr(S->low_rank_vectors,W,Z,S->low_rank,S->low_rank_vecsize,
         S->low_rank);
    mat_mult(S->low_rank_form,Z,S->low_rank_inverse_form,S->low_rank,
         S->low_rank,S->low_rank);
    for ( i = 0 ; i < S->low_rank ; i++ )
      S->low_rank_inverse_form[i][i] += 1;
    mat_inv(S->low_rank_inverse_form,S->low_rank);
    free_matrix(W);
    free_matrix(Z);
  }
}  /* end sp_factor() */
