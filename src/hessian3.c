/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*****************************************************************
*
*  file: hessian3.c
*
*  Contents: hessian related routines.
*                Old hessian calculators.
*                Some YSMP drivers.
*                Metric setup routines.
*/

#include "include.h"

/********************************************************************
*
* function: area_hessian()
*
* purpose: fill in hessian matrix with area derivatives
*             also does volume constraints
*
*/

void area_hessian(S,rhs)
struct linsys *S;
REAL *rhs;
{
  int i,j,k;
  facet_id f_id;
  MAT2D(self2,MAXCOORD,MAXCOORD);
  MAT2D(otherD,MAXCOORD,MAXCOORD);

  /* fill in sparse matrix rows and volume constraint rows */

  FOR_ALL_FACETS(f_id)
  {
    REAL side[FACET_EDGES][MAXCOORD];    /* 3 sides of facet */
    REAL ss[FACET_EDGES];      /* squares of sides */
    REAL sd[FACET_EDGES];    /* dot of side with next side */
    facetedge_id fe_id;         
    struct hess_verlist *v[FACET_VERTS];
    REAL two_area;    /* twice area of facet */
    REAL first[FACET_VERTS][MAXCOORD]; /* first partials */
    REAL density = get_facet_density(f_id);
    vertex_id v_id[FACET_VERTS];

    if ( density == 0.0 ) continue;
    fe_id = get_facet_fe(f_id);
    for ( i = 0 ; i < FACET_EDGES ; i++ )
    { v_id[i] = get_fe_tailv(fe_id);
      v[i] = get_vertex_vhead(v_id[i]);
      get_fe_side(fe_id,side[i]);
      fe_id = get_next_edge(fe_id);
    }

    for ( i = 0 ; i < FACET_EDGES ; i++ )
    { ss[i] = SDIM_dot(side[i],side[i]);
      sd[i] = -SDIM_dot(side[(i+1)%FACET_EDGES],side[(i+2)%FACET_EDGES]);
    }
    two_area = sqrt(ss[1]*ss[2] - sd[0]*sd[0]);

    /* first derivatives of area */
    for ( i = 0 ; i < FACET_EDGES ; i++ )
    {
      int ii,jj;
      REAL grad[MAXCOORD];

      if ( v[i]->freedom <= 0 ) continue;
      ii = (i+2)%FACET_EDGES;  /* side previous vertex i */
      jj = (i+1)%FACET_EDGES;  /* side opposite vertex i, next vertex */
      for ( k = 0 ; k < SDIM ; k++ )
      { first[i][k] = (side[ii][k]*ss[jj] - sd[i]*(-side[jj][k]))/2/two_area;
        grad[k] = density*first[i][k];
      }
      fill_grad(S,v[i],grad,rhs);
    }

    /* second derivatives */
    if ( hess_flag )
     for ( i = 0 ; i < FACET_EDGES ; i++ )
     { REAL self,other;
       int ii,jj;

       if ( v[i]->freedom <= 0 ) continue;

       ii = (i+2)%FACET_EDGES;  /* side previous vertex i */
       jj = (i+1)%FACET_EDGES;  /* side opposite vertex i, next vertex */

       /* for now, all degrees of freedom are coordinate */
       for ( j = 0 ; j < SDIM ; j++ )
          for ( k = 0 ; k < SDIM ; k++ )
          { self = -(side[jj][j]*side[jj][k])/2;
            if ( j == k ) self += ss[jj]/2;
            self2[j][k] = 
                      density*(self - 2*first[i][j]*first[i][k])/two_area;
            if ( v[jj]->freedom <= 0 ) continue;
            other = -side[ii][j]*side[jj][k] - side[jj][j]*(-side[ii][k])/2;
            if ( j == k ) other -= sd[i]/2;
            otherD[j][k] = 
                    density*(other - 2*first[i][j]*first[jj][k])/two_area;
          }
          fill_self_entry(S,v_id[i],self2);
          if ( v[jj]->freedom > 0 ) 
            fill_mixed_entry(S,v_id[i],v_id[jj],otherD);
        }
  }
}  /* end area_hessian() */


/********************************************************************
*
* function: edge_energy_hessian()
*
* purpose:  edge constraint hessian
*
*/

void edge_energy_hessian(S,rhs)
struct linsys *S;
REAL *rhs;
{ int i,j;
  int head,tail;
  edge_id e_id;
  MAT2D(first,2,MAXCOORD);
  MAT4D(second,2,2,MAXCOORD,MAXCOORD);

  /* do edges */
    FOR_ALL_EDGES(e_id)
     {  
        struct hess_verlist *v[MAXCOORD];
        vertex_id *v_id = get_edge_vertices(e_id);

        if ( ! get_e_constraint_map(e_id)[0] ) continue;

        for ( i = 0 ; i < 2 ; i++ )
          v[i] = get_vertex_vhead(v_id[i]);

        memset((char*)first[0],0,sizeof(REAL)*2*MAXCOORD);
        memset((char*)second[0][0][0],0,sizeof(REAL)*2*2*MAXCOORD*MAXCOORD);

        edge_constr_hessian(S,e_id,first,second);
    
        /* first derivatives on right hand side */
        for ( i = 0 ; i < 2 ; i++ )
         fill_grad(S,v[i],first[i],rhs);
        /* second derivatives */
        if ( hess_flag )
         for ( i = 0 ; i < 2 ; i++ )
          for ( j = i ; j < 2 ; j++ )
          {
             if ( (v[i]->freedom==0) || (v[j]->freedom==0) ) continue;
             if ( loc_ordinal(v_id[i]) > loc_ordinal(v_id[j]) )
                 { tail = i; head = j; }
             else { tail = j; head = i; }

             fill_mixed_entry(S,v_id[tail],v_id[head],second[tail][head]);
            }
      }
} /* end edge_energy_hessian() */

/********************************************************************
*
* function: edge_constr_hessian()
*
* purpose: fill in hessian matrix for constraint energy of one edge
*
*/

void  edge_constr_hessian(S,e_id,first,second)
struct linsys *S;
edge_id e_id;
REAL **first;
REAL ****second;
{
  REAL *tcoord,*hcoord;
  struct constraint *constr;
  int i,m;
  REAL side[MAXCOORD];
  REAL green[MAXCOORD];
  REAL green_deriv_space[MAXCOORD][MAXCOORD];
  REAL *green_deriv[MAXCOORD];
  int j,sign;
  conmap_t *conmap;
  REAL midpt[MAXCOORD];
  REAL grad[MAXCOORD];

  conmap = get_e_constraint_map(e_id);
  if ( !conmap[0] ) return;
  if ( get_eattr(e_id) & NEGBOUNDARY ) sign = -1;
  else sign = 1;
  if ( inverted(e_id) ) sign = -sign;

  tcoord = get_coord(get_edge_tailv(e_id));
  hcoord = get_coord(get_edge_headv(e_id));
  for ( j = 0 ; j < SDIM ; j++ )
     { 
        side[j] = hcoord[j] - tcoord[j];
        green_deriv[j] = green_deriv_space[j];
     }

  for ( j = 1 ; j <= (int)conmap[0] ; j++ )
   {
     constr = get_constraint(conmap[j]);
     if ( !(constr->attr & CON_ENERGY) 
              || (constr->compcount != SDIM) ) continue;
     for ( m = 0 ; m < gauss1D_num ; m++ )
      {
         for ( i = 0 ; i < SDIM ; i++ )
            midpt[i] = gauss1Dpt[m]*hcoord[i] + (1 - gauss1Dpt[m])*tcoord[i];
         for ( i = 0 ; i < SDIM ; i++ )
            eval_all(constr->envect[i],midpt,SDIM,&green[i],green_deriv[i],e_id);
         vec_mat_mul(side,green_deriv,grad,SDIM,SDIM);
         for ( i = 0 ; i < SDIM ; i++ )
            { int jj;

              first[0][i] += sign*gauss1Dwt[m]*((1-gauss1Dpt[m])*grad[i]-green[i]);
              first[1][i] += sign*gauss1Dwt[m]*(gauss1Dpt[m]*grad[i] + green[i]);

              if ( hess_flag )
                for ( jj = 0 ; jj < SDIM ; jj++ )
                { second[0][0][i][jj] -= sign*gauss1Dwt[m]*
                    (green_deriv[i][jj]+green_deriv[jj][i])*(1-gauss1Dpt[m]);
                  second[0][1][i][jj] += sign*gauss1Dwt[m]*
                    (-green_deriv[i][jj]*(1-gauss1Dpt[m])
                        + green_deriv[jj][i]*gauss1Dpt[m]);
                  second[1][0][i][jj] += sign*gauss1Dwt[m]*
                    (-green_deriv[jj][i]*(1-gauss1Dpt[m])
                        + green_deriv[i][jj]*gauss1Dpt[m]);
                  second[1][1][i][jj] += sign*gauss1Dwt[m]*
                    (green_deriv[i][jj]+green_deriv[jj][i])*gauss1Dpt[m];
                }
            }
        }
    }
} /* end edge_constr_hessian() */

/********************************************************************
*
* function: body_hessian()
*
* purpose: fill in hessian matrix with body volume constraints
*
* return: number of bodies that have volume constraints
*/

int body_hessian(S,rhs)
struct linsys *S;
REAL *rhs;
{
  int i,j;
  int count = 0;
  facet_id f_id;
  body_id b_id;
  REAL *Z;
  REAL coe;
  MAT2D(otherD,MAXCOORD,MAXCOORD);
  MAT2D(self,MAXCOORD,MAXCOORD);
  REAL zsum,ssum;

  Z = (REAL *)temp_calloc(web.skel[BODY].max_ord+1,sizeof(REAL));
  FOR_ALL_BODIES(b_id) 
     if ( get_battr(b_id) & FIXEDVOL )
        Z[loc_ordinal(b_id)] = get_body_pressure(b_id);
  /* Z is vector of coefficients for adding constraint hessians */

  if ( hess_debug ) 
  { printf("Z vector: \n");
    for ( i = 0 ; i < web.skel[BODY].max_ord+1 ; i++ ) 
       printf("%d    %g\n",i,(DOUBLE)Z[i]);
  }

  /* rhs for body constraint rows */
  if ( rhs_flag && !everything_quantities_flag )
  FOR_ALL_BODIES(b_id)
  { REAL *current = rhs + S->bodyrowstart + loc_ordinal(b_id);
    if ( get_battr(b_id) & FIXEDVOL )
         *current = -(get_body_fixvol(b_id) - get_body_volume(b_id));
  }

  /* body volume constraints, linear part */
  FOR_ALL_FACETS(f_id)
  {
    REAL side[FACET_EDGES][MAXCOORD];    /* 3 sides of facet */
    facetedge_id fe_id;         
    struct hess_verlist *v[FACET_VERTS];
    REAL *x[FACET_VERTS];  /* for volume calculation */
    body_id bb_id;
    int do_b, do_bb; /* flags for whether bodies fixed */
    int a;

    if ( get_attr(f_id) & NONCONTENT ) continue;

    b_id = get_facet_body(f_id);
    if ( valid_id(b_id) )
    { a = get_battr(b_id);
      do_b =  (a & FIXEDVOL) && !(a & REDUNDANT_BIT);
    } else do_b = 0;

    bb_id = get_facet_body(inverse_id(f_id));
    if ( valid_id(bb_id) )
    { a = get_battr(bb_id);
      do_bb =  (a & FIXEDVOL) && !(a & REDUNDANT_BIT);
    } else do_bb = 0;

    if ( !do_b && !do_bb ) continue;

    fe_id = get_facet_fe(f_id);
    for ( i = 0 ; i < FACET_EDGES ; i++ )
    { v[i] = get_vertex_vhead(get_fe_tailv(fe_id));
      x[i] = get_coord(get_fe_tailv(fe_id));
      get_fe_side(fe_id,side[i]);
      fe_id = get_next_edge(fe_id);
    }

    coe = 0.0;
    if ( valid_id(b_id) )
       coe += Z[loc_ordinal(b_id)];
    if ( valid_id(bb_id) )
       coe -= Z[loc_ordinal(bb_id)];

    zsum = (x[0][2]+x[1][2]+x[2][2]);
    ssum = side[0][0]*side[1][1] - side[0][1]*side[1][0];

    for ( i = 0 ; i < FACET_VERTS ; i++ )
    {
      int currentrow;
      REAL g[MAXCOORD],gg[MAXCOORD],*ggg;
      REAL grad[MAXCOORD];
      int ii = (i+1)%3;
      int iii = (i+2)%3;

      if ( web.symmetric_content )
          cross_prod(x[ii],x[iii],g);
      else
      { g[0] = zsum*(x[ii][1]-x[iii][1]);
        g[1] = zsum*(x[iii][0]-x[ii][0]);
        g[2] = ssum;
      }

      for ( j = 0 ; j < SDIM ; j++ )
         grad[j] = -coe*g[j]/6;
      fill_grad(S,v[i],grad,rhs);

      if ( !hess_flag ) continue;

      if ( v[i]->proj )
      { vec_mat_mul(g,v[i]->proj,gg,SDIM,v[i]->freedom);
        ggg = gg;
      }
      else ggg = g;

      if ( do_b )
      {
        currentrow = S->bodyrowstart + loc_ordinal(b_id);
        for ( j = 0 ; j < v[i]->freedom ; j++ )
        { sp_hash_search(S,v[i]->rownum+j,currentrow,ggg[j]/6);
        }
      }
         
      if ( do_bb )
      {
        currentrow = S->bodyrowstart + loc_ordinal(bb_id);
        for ( j = 0 ; j < v[i]->freedom ; j++ )
        { sp_hash_search(S,v[i]->rownum+j,currentrow,-ggg[j]/6);
        }
      }
    }
  }

  /* Now quadratic part */

  otherD[0][0] = otherD[1][1] = otherD[2][2] = 0.0;
  self[0][0] = self[1][1] = self[2][2] = 0.0;
  self[0][1] = self[1][0] = 0.0;
  /* now add constraint hessians to energy hessian */
  if ( hess_flag )
    FOR_ALL_FACETS(f_id)
     {
        facetedge_id fe_id;         
        struct hess_verlist *v[FACET_VERTS];
        REAL *x[FACET_VERTS];  /* for volume calculation */
        vertex_id v_id[FACET_VERTS];

        if ( get_attr(f_id) & NONCONTENT ) continue;

        coe = 0.0;
        b_id = get_facet_body(f_id);
        if ( valid_id(b_id) )
          coe += Z[loc_ordinal(b_id)];
        b_id = get_facet_body(inverse_id(f_id));
        if ( valid_id(b_id) )
          coe -= Z[loc_ordinal(b_id)];
        coe /= 6; /* tetrahedron factor */
        if ( coe == 0.0 ) continue;

        fe_id = get_facet_fe(f_id);
        for ( i = 0 ; i < FACET_EDGES ; i++ )
        { v_id[i] = get_fe_tailv(fe_id);
          v[i] = get_vertex_vhead(v_id[i]);
          x[i] = get_coord(v_id[i]);
          fe_id = get_next_edge(fe_id);
        }
        zsum = x[0][2] + x[1][2] + x[2][2];

        /* second derivatives */
        if ( hess_flag )
         for ( i = 0 ; i < FACET_EDGES ; i++ )
          { int ii,jj;

             jj = (i+1)%FACET_EDGES;  /* next vertex */
             ii = (i+2)%FACET_EDGES;  /* vertex previous vertex i */

             if ( v[i]->freedom <= 0 ) continue;
             if ( !web.symmetric_content )
             {
                self[0][2] = self[2][0] = coe*(x[jj][1]-x[ii][1]);
                self[1][2] = self[2][1] = coe*(x[ii][0]-x[jj][0]);
                fill_self_entry(S,v_id[i],self); 
             }
             if ( v[jj]->freedom <= 0 ) continue;

             /* for now, all degrees of freedom are coordinate */
             if ( web.symmetric_content )
             { otherD[0][1] = -coe*x[ii][2];
               otherD[1][0] =  coe*x[ii][2];
               otherD[0][2] =  coe*x[ii][1];
               otherD[2][0] = -coe*x[ii][1];
               otherD[2][1] =  coe*x[ii][0];
               otherD[1][2] = -coe*x[ii][0];
             } 
             else
             { otherD[0][1] = -coe*zsum;
               otherD[1][0] =  coe*zsum;
               otherD[0][2] = -coe*(x[jj][1]-x[ii][1]);
               otherD[2][0] = -coe*(x[ii][1]-x[i ][1]);
               otherD[1][2] = -coe*(x[ii][0]-x[jj][0]);
               otherD[2][1] = -coe*(x[i ][0]-x[ii][0]);
             }
             fill_mixed_entry(S,v_id[i],v_id[jj],otherD); 
          }
      }
  temp_free((char*)Z);

  count = 0;
  if ( !everything_quantities_flag )
  FOR_ALL_BODIES(b_id)
    { if ( get_battr(b_id) & FIXEDVOL )
          count++;
    }
  return count;
} /* end body_hessian() */

/*****************************************************************************/

#include "f2c.h"

/*  C declarations of the YSMP routines  */

int odrv_ ARGS(( integer *, integer *,integer *,REAL *, integer *,integer *,
integer *,integer *, integer *, integer *));

int sdrvmd_ ARGS(( integer *, integer *,integer *, integer *,integer *,REAL *,
          REAL *,REAL *, integer *,integer *,REAL *,integer *,
          integer *, integer *, REAL *));
                                                     
void sdrv_flag_check ARGS((integer , integer , integer ));
void odrv_flag_check ARGS((integer , integer ));

/*************************************************************************
*
* function: ysmp_factor()
*
* purpose: Do YSMP factoring in linear system
*
*/
void ysmp_factor(S)
struct linsys *S;
{ int i,j;
  int PATH,FLAG=0,ESP=0;
  REAL *RSP,EMAX;

  PROF_START(hessian_factor);

  /* need just enough at first to do symbolic factorization */
  if ( S->NSP == 0 )
  { S->NSP = (4 + (int)(log(S->N+1)))*(S->N + S->IA[S->N]);
    S->ISP = (int *)temp_calloc(S->NSP,sizeof(REAL));
  }
  else memset((char*)S->ISP,0,S->NSP*sizeof(REAL));

  if ( S->N == 0 ) return; /* empty hessian */

  /* subtract lambda from diagonal */
  if ( S->lambda != 0.0 )
  for ( i = 0 ; i < S->A_rows ; i++ )
  { for ( j = S->IA[i]-A_OFF ; j < S->IA[i+1]-A_OFF ; j++ )
      if ( S->JA[j]-A_OFF == i )
         { S->A[j] -= S->lambda; break; }
  }
  /*  Call ODRV to perform the reordering on A, if not done yet */
  if ( !(S->flags & S_ODRV_REORDERED) )
  { PATH = (S->flags & S_ORDERFOUND) ? 3 : 2; 
    if ( S->P == NULL ) S->P = (int *)temp_calloc(S->N,sizeof(int));
    if ( S->IP == NULL ) S->IP = (int *)temp_calloc(S->N,sizeof(int));
    odrv_( &S->N, S->IA,S->JA,S->A, S->P,S->IP, &S->NSP,S->ISP,&PATH,&FLAG );
    odrv_flag_check(FLAG,S->N);
    S->flags |= S_ODRV_REORDERED;
  }
  


  /*  Call SDRVMD to factor symbolically and numerically */
  PATH = 4; /* symbolic factor */
  RSP = (REAL *) S->ISP;
  sdrvmd_(&S->N,S->P,S->IP,S->IA,S->JA,S->A,NULL,NULL,&S->NSP,
            S->ISP,RSP,&ESP, &PATH,&FLAG,&EMAX);
 
 sdrv_flag_check(0,FLAG,S->N);
  PATH = 6; /* numeric factor */
  if ( !hessian_quiet_flag )
  { if ( ESP < 0 )
     sprintf(msg,"YSMP allocation at %p of %d*8 short by %d*8. Expanding.\n",
               S->ISP,S->NSP,-ESP);
    else
     sprintf(msg,"YSMP allocation at %p of %d*8 over by %d*8. Reducing.\n",
               S->ISP,S->NSP,ESP);
    outstring(msg);
  }
  S->ISP = (int*)temp_realloc((char*)(S->ISP),
          (S->NSP-ESP+10)*sizeof(REAL));
  if ( !hessian_quiet_flag )
  { sprintf(msg,"New allocation at %p.\n",S->ISP);  outstring(msg); }
  S->NSP += -ESP + 10;
  RSP = (REAL *) S->ISP;
  sdrvmd_(&S->N,S->P,S->IP,S->IA,S->JA,S->A,NULL,NULL,&S->NSP,
            S->ISP,RSP,&ESP, &PATH,&FLAG,&EMAX);
  sdrv_flag_check(ESP,FLAG,S->N);
  S->neg = mat_index;
  S->zero = mat_null;
  S->pos = S->N - mat_index - mat_null;

  /* add lambda back to diagonal */
  if ( S->lambda != 0.0 )
    for ( i = 0 ; i < S->A_rows ; i++ )
      { for ( j = S->IA[i]-A_OFF ; j < S->IA[i+1]-A_OFF ; j++ )
            if ( S->JA[j]-A_OFF == i )
            { S->A[j] += S->lambda; break; }
      }

  PROF_FINISH(hessian_factor);
}  /* end ysmp_factor() */

/*************************************************************************
*
* function: ysmp_solve()
*
* purpose: Do YSMP solve in linear system
*
*/
void ysmp_solve(S,b,x)
struct linsys *S;
REAL *b; /* rhs */
REAL *x; /* solution, may be B */
{ int PATH,FLAG=0,ESP;
  REAL *RSP,EMAX;

  PROF_START(hessian_factor);

  if ( S->N == 0 ) return;
  RSP = (REAL *) S->ISP;
  PATH = 3;  /* solution only */
  sdrvmd_(&S->N,S->P,S->IP,S->IA,S->JA,S->A,b,x,&S->NSP,S->ISP,RSP,&ESP, 
        &PATH,&FLAG,&EMAX);
  sdrv_flag_check(ESP,FLAG,S->N);

  PROF_FINISH(hessian_factor);
}

/**********************************************************************
*
* function: ysmp_solve_multi()
*
* purpose: solve for multiple right hand sides.
*
*/

void ysmp_solve_multi(S,b,x,rk)
struct linsys *S;
REAL **b; /* rhs */
REAL **x; /* solution, may be B */
int rk; /* number of rhs */
{ int k;
  for ( k = 0 ; k < rk ; k++ ) ysmp_solve(S,b[k],x[k]);
}

/**************************************************************************
*
* function: odrv_flag_check()
* 
* purpose: check error return from odrv().
*/
void odrv_flag_check(FLAG, N)
integer FLAG,N;
{
  if (!FLAG) return;

  /*  Fatal error */
  sprintf(errmsg,"Internal error after ODRV:  N = %d  FLAG = %d -- ",N,FLAG);
  if ( 9*N<FLAG && FLAG<=10*N )
    sprintf(errmsg+strlen(errmsg),"Internal error: Insufficient storage in MD, K = %d\n",FLAG-9*N);
  else if ( FLAG == 10*N + 1 )
    strcat(errmsg,"Internal error: Insufficient storage in ODRV\n");
  else if ( FLAG == 11*N + 1 )
    strcat(errmsg,"Internal error: Illegal path specification in ORDV\n");
  else
    sprintf(errmsg,"Internal error: Mysterious value of FLAG in ORDV: %d\n",FLAG);
  kb_error(1845,errmsg,RECOVERABLE);
}

/**************************************************************************
*
* function: sdrv_flag_check()
* 
* purpose: check error return from sdrv().
*
*/

void sdrv_flag_check(ESP, FLAG, N)
integer ESP,FLAG,N;
{ char *c;

  if (ESP<0)
  { sprintf(errmsg,"Internal error:  SDRVMD:  Storage shortage:  ESP = %d\n",ESP);
     kb_error(1846,errmsg,RECOVERABLE); 

  }
  if (FLAG==0) return;
  if (FLAG<0)
  { if ( !pos_def_warning_flag && !hessian_quiet_flag )
      { kb_error(1847,"SDRVMD: Hessian not positive definite.\n",WARNING);
         pos_def_warning_flag = 1;
      }
     negdiag = -FLAG-1; 
     return;
  }

  /*  Fatal error:  print message and abort */
  sprintf(errmsg,"Internal error: after SDRVMD:  N = %d  FLAG = %d -- ",N,FLAG);
  c = errmsg + strlen(errmsg);
  if ( 2*N < FLAG && FLAG <= 3*N )
    sprintf(c,"Internal error, SDRV: Duplicate entry in A at row %d\n",FLAG-2*N);
  else if ( 6*N < FLAG && FLAG <= 7*N )
    sprintf(c,"Internal error, SDRV: Insufficient storage in SSF at row %d\n",FLAG-6*N);
  else if ( FLAG == 7*N + 1 )
    sprintf(c,"Internal error, SDRV: Insufficient storage in SNF\n");
  else if ( 8*N < FLAG && FLAG <= 9*N )
    sprintf(c,"Internal error, SDRV: Zero pivot at row %d\n",FLAG-8*N);
  else if ( FLAG == 10*N + 1 )
    sprintf(c,"Internal error, SDRV: Insufficient storage in SDRV\n");
  else if ( FLAG == 11*N + 1 )
    sprintf(c,"Internal error, SDRV: Illegal path specification\n");
  else printf(c,"Internal error, SDRV: Mysterious value of FLAG: %d\n",FLAG);
  kb_error(1848,errmsg,RECOVERABLE);
} /* end sdrv_flag_check() */


/************************************************************************
*
* function: star_metric_setup()
*
* purpose:  set up vector-to-form metric sparse matrix for local
*              vertex star area metric.
*/

void star_metric_setup(S,M)
struct linsys *S;  /* hessian to set up metric for */
struct linsys *M;  /* pointer to empty structure */
{
  int i,j,k;
  int  Total_entries = 0;
  struct hess_verlist *vh;
  vertex_id v_id;

  if ( web.lagrange_order > 1 ) { linear_metric_setup(S,M); return; }

  if ( ysmp_flag != MINDEG_FACTORING )
  { ysmp_flag = MINDEG_FACTORING;
    sp_mul_func = bk_mul;
    sp_AIJ_setup_func = bk_AIJ_setup;
    sp_constraint_setup_func = bk_constraint_setup;
    sp_hess_project_setup_func= BK_hess_project_setup;
    sp_factor_func = xmd_factor;
    sp_solve_func = xmd_solve;
    sp_solve_multi_func = xmd_solve_multi;
    sp_ordering_func = NULL;
    sp_CHinvC_func = sp_CHinvC;
    outstring("Using alternate minimal degree with area normalization.\n");
  }
  M->N = S->N;
  M->flags &= ~ S_ODRV_REORDERED; 

  Total_entries = S->N;  /* pure diagonal */

  /* allocate storage for arrays */
  M->IA = (int *)temp_calloc(M->N+1,sizeof(int));
  M->JA = (int *)temp_calloc(Total_entries,sizeof(int));
  M->A  = (REAL *)temp_calloc(Total_entries,sizeof(REAL));
  if ( M->P == NULL ) M->P = (int *)temp_calloc(M->N,sizeof(int));
  if ( M->IP == NULL ) M->IP = (int *)temp_calloc(M->N,sizeof(int));

  /* go through, vertex by vertex */
  FOR_ALL_VERTICES(v_id)
  { 
    REAL area;
    vh = get_vertex_vhead(v_id);

    if ( vh->freedom == 0 ) continue;

    area = ((web.representation==STRING)?get_vertex_length_star(v_id):
                                    get_vertex_area_star(v_id))/star_fraction;

    for ( j = 0, k = vh->rownum  ; j < vh->freedom ; j++,k++ )
    { M->IA[k] = M->JA[k] = k + A_OFF;
      M->A[k] = area;
      M->IP[k] = M->P[k] = k;    /* unpermuted */
    }
  }

  for ( i = S->optparamrowstart ; i < S->bodyrowstart ; i++ )
  { /* metric 1 for optimizing parameters */
    M->IA[i] = M->JA[i] = i + A_OFF;
    M->A[i] = 1.0;
    M->IP[i] = M->P[i] = i;    /* unpermuted */
  }
  for ( i = S->bodyrowstart ; i < S->N ; i++ )
  { /* make sure rest of diagonal exists, but no shift */
    M->IA[i] = M->JA[i] = i + A_OFF;
    M->A[i] = 0.0;
    M->IP[i] = M->P[i] = i;    /* unpermuted */
  }


  M->IA[M->N] = M->N + A_OFF;

  M->apinv = (REAL *)temp_calloc(M->N,sizeof(REAL));
  /* sums of rows */
  for ( i = 0 ; i < M->N ; i++ )
  { int start = M->IA[i] - A_OFF;
    int end = M->IA[i+1] - A_OFF;
    M->apinv[i] = M->A[start];
    for ( j = start+1 ; j < end ; j++ )
    { M->apinv[i] += M->A[j];
      M->apinv[M->JA[j]] += M->A[j];
    }
  }
  for ( i = 0 ; i < M->N ; i++ ) 
     M->apinv[i] = (M->apinv[i]==0.0) ? 0.0 : 1/M->apinv[i];
} /* end star_metric_setup() */

/************************************************************************
*
* function: linear_metric_setup()
*
* purpose:  set up vector-to-form metric sparse matrix for local
*              linear interpolation metric.
*/

void linear_metric_setup(S,M)
struct linsys *S;  /* hessian to build metric for */
struct linsys *M;  /* pointer to empty structure */
{
  int i,jj,k,kk,m;
  size_t j;
  int sum;
  edge_id e_id;
  vertex_id v_id;
  int ti=0,hi=0;
  REAL len;
  MAT2D(weights,MAXCOORD+1,MAXCOORD+1);
  MAT2D(temp_mat,MAXCOORD,MAXCOORD);
  REAL **mat;
  int col,end;
  int ii[MAXCOORD+1];
  int dim;

if ( optparamcount > 0 )
  kb_error(2444,"Sorry; linear_metric not working with optimizing parameters.\n",RECOVERABLE);

  if ( web.modeltype == QUADRATIC )
  { linear_metric_setup_quadratic(S,M); goto apinv_setup; }
  if ( web.modeltype == LAGRANGE )
  { linear_metric_setup_lagrange(S,M); goto apinv_setup; }

  if ( ysmp_flag != MINDEG_FACTORING )
  { ysmp_flag = MINDEG_FACTORING;
    sp_mul_func = bk_mul;
    sp_AIJ_setup_func = bk_AIJ_setup;
    sp_constraint_setup_func = bk_constraint_setup;
    sp_hess_project_setup_func = BK_hess_project_setup;
    sp_factor_func = xmd_factor;
    sp_solve_func = xmd_solve;
    sp_solve_multi_func = xmd_solve_multi;
    sp_ordering_func = NULL;
    sp_CHinvC_func = sp_CHinvC;
    outstring(
        "Using alternate minimal degree with linear interpolation metric.\n");
  }
  M->N = S->N;
  M->flags &= ~ S_ODRV_REORDERED; 

  /* allocate storage for arrays */
  M->IA = (int *)temp_calloc(M->N+1,sizeof(int));
  if ( M->P == NULL ) M->P = (int *)temp_calloc(M->N,sizeof(int));
  if ( M->IP == NULL ) M->IP = (int *)temp_calloc(M->N,sizeof(int));

  /* have to count number of higher-ordinal neighbors of each vertex */
  if ( web.representation == SIMPLEX )
  { vertex_id *nbrlist;
    int nbralloc = 1000;
    int nbrcount;
    facet_id f_id,start_f;
    vertex_id *v;
    int m;

    nbrlist = (vertex_id *)temp_calloc(nbralloc,sizeof(vertex_id));
    FOR_ALL_VERTICES(v_id)
    { struct hess_verlist *vh = get_vertex_vhead(v_id);
      i = loc_ordinal(v_id);
      nbrcount = 1;
      nbrlist[0] = v_id;
      f_id = start_f = get_vertex_first_facet(v_id);
      do
      { v = get_facet_vertices(f_id);
        for ( k = 0 ; k <= web.dimension ; k++ )
          if ( loc_ordinal(v[k]) > i )
          { for ( m = 0 ; m < nbrcount ; m++ )
              if ( nbrlist[m] == v[k] ) break;
            if ( m == nbrcount )
            { if ( nbrcount >= nbralloc )
                 kb_error(1849,"Too many neighbors in linear_metric_setup()\n",
                      RECOVERABLE);
               nbrlist[nbrcount++] = v[k];
            }
          }
          f_id = get_next_vertex_facet(v_id,f_id);
      } while ( !equal_element(f_id,start_f));
      /* now count total degrees of freedom */
      for ( j = vh->rownum, jj = 0 ; jj < vh->freedom ; jj++,j++ )
        M->IA[j] += vh->freedom-jj;
      for ( m = 1 ; m < nbrcount ; m++ )
      { struct hess_verlist *vhi = get_vertex_vhead(nbrlist[m]);
        for ( j = vh->rownum, jj = 0 ; jj < vh->freedom ; j++,jj++ )
           M->IA[j] += vhi->freedom;
      }
    }
  }
  else 
  { /* fe model */
    FOR_ALL_VERTICES(v_id)
    { struct hess_verlist *vh = get_vertex_vhead(v_id);
      for ( j = vh->rownum, jj = 0 ; jj < vh->freedom ; jj++,j++ )
        M->IA[j] += vh->freedom-jj;      
    }
    FOR_ALL_EDGES(e_id)
    { vertex_id tailv = get_edge_tailv(e_id);
      vertex_id headv = get_edge_headv(e_id);
      struct hess_verlist *vt = get_vertex_vhead(tailv),*vhtmp;
      struct hess_verlist *vh = get_vertex_vhead(headv);
      ti = loc_ordinal(tailv);
      hi = loc_ordinal(headv);
      if ( ti > hi ) 
      { int tt = ti; ti = hi ; hi = tt; 
        vhtmp = vh; vh = vt; vt = vhtmp;
      }
      if ( vh->freedom == 0 ) continue;
      for ( j = vt->rownum, jj = 0 ; jj < vt->freedom ; j++,jj++ )
         M->IA[j] += vh->freedom;
    }
  }
  for ( i = S->optparamrowstart ; i < S->N ; i++ )
    M->IA[i] = 1;  /* diagonal elements */

  for ( i = 0, sum = 0 ; i < M->N ; i++ ) /* add to running totals */
  { int tmp = sum; 
    sum += M->IA[i] + optparamcount;
    M->IA[i] = tmp + A_OFF;
  }
  M->IA[M->N] = sum + A_OFF;

  /* now can allocate space */
  M->JA = (int *)temp_calloc(sum,sizeof(int));
  M->A  = (REAL *)temp_calloc(sum,sizeof(REAL));

  for ( i = 0 ; i < sum ; i++ ) M->JA[i] = -1;  /* vacant */
  for ( i = 0 ; i < M->N ; i++ )
  { M->JA[M->IA[i]-A_OFF] = i + A_OFF; /* diagonal entry at start */
    M->IP[i] = M->P[i] = i;    /* unpermuted */
  }


  /* set up interpolation weight matrix */

  for ( i = 0 ; i < optparamcount ; i++ )
    M->A[M->IA[S->optparamrowstart+i]-A_OFF] = 1.0;
  /* leave rest of diagonal zero in case of augmented hessian */

  dim = web.dimension;
  for ( i = 0 ; i <= dim ; i++ )  /* index for vertices of element */
    for ( m = 0 ; m <= dim ; m++ )
    { if ( i == m )
         weights[i][m] = linear_metric_mix*2.0/(dim+1.0)/(dim+2.0)
              + (1.0 - linear_metric_mix)/(dim+1.0);
      else weights[i][m] = linear_metric_mix/(dim+1.0)/(dim+2.0);
    }

  if ( web.representation == STRING )
  { /* edge by edge */
    FOR_ALL_EDGES(e_id)
    { struct hess_verlist *vh[2];
      vertex_id tailv = get_edge_tailv(e_id);
      vertex_id headv = get_edge_headv(e_id);
      if ( loc_ordinal(tailv) > loc_ordinal(headv) )
      { vh[0] = get_vertex_vhead(headv);
        vh[1] = get_vertex_vhead(tailv);
      }
      else
      { vh[0] = get_vertex_vhead(tailv);
        vh[1] = get_vertex_vhead(headv);
      }
      len = get_edge_length(e_id);
      for ( i = 0 ; i < 2 ; i++ )
      { struct hess_verlist *v = vh[i];
        REAL fudge = v->slant*v->slant;
         
        /* self */
        if ( v->proj )
        { 
          tr_mat_mul(v->proj,v->proj,temp_mat,SDIM,v->freedom,v->freedom);
          mat = temp_mat;
        }
        else mat = identmat;
        for ( jj = 0 ; jj < v->freedom ; jj++ )
          for ( k = jj ; k < v->freedom ; k++ )
          { col = v->rownum + k + A_OFF;
            end = M->IA[v->rownum+jj+1]-A_OFF;
            for ( m = M->IA[v->rownum+jj]-A_OFF ; m < end  ; m++ )
              if ( (M->JA[m] < A_OFF) || (M->JA[m] == col) )
              { M->JA[m] = col;
                M->A[m] += fudge*(3-linear_metric_mix)*len/6*mat[jj][k];
                break;
              }
            if ( m == end )
               kb_error(1851,"Internal error in linear_metric_setup.\n",RECOVERABLE);
          }

          /* cross terms */
          for ( k = i+1 ; k < 2 ; k++ )
          { struct hess_verlist *vv = vh[k];
            REAL cfudge = v->slant*vv->slant;
            if ( v->proj && vv->proj )
            { tr_mat_mul(v->proj,vv->proj,temp_mat,SDIM,v->freedom,
                    vv->freedom);
              mat = temp_mat;
            }
            else if ( v->proj )
            { tr_mat_mul(v->proj,identmat,temp_mat,SDIM,v->freedom,
                  vv->freedom);
              mat = temp_mat;
            }
            else if ( vv->proj ) mat = vv->proj;
            else mat = identmat;

            for ( jj = 0 ; jj < v->freedom ; jj++ )  
              for ( kk = 0 ; kk < vv->freedom ; kk++ )  
              { col = vv->rownum + kk + A_OFF ;
                end = M->IA[v->rownum+jj+1]-A_OFF;
                for ( j = M->IA[v->rownum+jj]+1-A_OFF ; (int)j < end ; j++ )
                  if ( (M->JA[j] < A_OFF) || (M->JA[j] == col)  )
                  { M->JA[j] = col; 
                    M->A[j] += cfudge*mat[jj][kk]*len/6*linear_metric_mix;
                    break;
                  }
                if ( j == end )
                    kb_error(1852,"Internal error in linear_metric_setup.\n",RECOVERABLE);
              }
          }
        }
     }
  }
  else 
  { facet_id f_id;
    REAL area;

    /* facet by facet */
    FOR_ALL_FACETS(f_id)
    { vertex_id ix[FACET_VERTS];
      facetedge_id fe;
      REAL density = get_facet_density(f_id); 
      REAL normal[MAXCOORD];
      struct hess_verlist *vh[MAXCOORD+1];

      area = get_facet_area(f_id);
      if ( density != 0.0 ) area *= density;
      if ( hessian_normal_perp_flag ) 
      { REAL d;
        get_facet_normal(f_id,normal);
        d = sqrt(SDIM_dot(normal,normal));
        for ( i = 0 ; i < SDIM ; i++ ) normal[i] /= d;
      }

      fe = get_facet_fe(f_id);
      ix[0] = get_fe_tailv(fe);
      ix[1] = get_fe_headv(fe);
      fe = get_next_edge(fe);
      ix[2] = get_fe_headv(fe);
      
      for ( i = 0 ; i <= dim ; i++ )  /* bubble sort vertices */
      { j = loc_ordinal(ix[i]);
        for ( k = i ; k > 0 ; k-- )
          if ( ii[k-1] > (int)j ) 
          { ii[k] = ii[k-1];
            vh[k] = vh[k-1];
          }
          else break;
        ii[k] = (int)j;
        vh[k] = get_vertex_vhead(ix[i]);
      }
      for ( i = 0 ; i <= dim ; i++ )
      { struct hess_verlist *v = vh[i];
        REAL fudge = v->slant*v->slant;
        /* self */
        if ( v->proj )
        { 
          if ( hessian_normal_perp_flag )
          { REAL w[MAXCOORD], *wp=w;
            vec_mat_mul(normal,v->proj,w,SDIM,v->freedom);
            tr_mat_mul(&wp,&wp,temp_mat,1,v->freedom,v->freedom);
            fudge = 1.0;
          }
          else
          {
            tr_mat_mul(v->proj,v->proj,temp_mat,SDIM,v->freedom,v->freedom);
          }
          mat = temp_mat;
        }
        else mat = identmat;

        for ( jj = 0 ; jj < v->freedom ; jj++ )
          for ( k = jj ; k < v->freedom ; k++ )
          { col = v->rownum + k + A_OFF;
            end = M->IA[v->rownum+jj+1]-A_OFF;
            for ( j = M->IA[v->rownum+jj]-A_OFF ; (int)j < end  ; j++ )
              if ( (M->JA[j] < A_OFF) || (M->JA[j] == col) )
              { M->JA[j] = col;
                M->A[j] += fudge*weights[i][i]*area*mat[jj][k];
                break;
              }
              if ( j == end )
                 kb_error(1853,"Internal error in linear_metric_setup.\n",RECOVERABLE);
          }

        /* cross terms */
        for ( k = i+1 ; k <= dim ; k++ )
        { struct hess_verlist *vv = vh[k];
          REAL cfudge = v->slant*vv->slant;
          if ( v->proj && vv->proj )
          { 
            if ( hessian_normal_perp_flag )
            { REAL w[MAXCOORD],ww[MAXCOORD],*wp=w,*wwp=ww;
              vec_mat_mul(normal,v->proj,w,SDIM,v->freedom);
              vec_mat_mul(normal,vv->proj,ww,SDIM,vv->freedom);
              tr_mat_mul(&wp,&wwp,temp_mat,1,v->freedom,vv->freedom);
              cfudge = 1.0;
            }
            else 
              tr_mat_mul(v->proj,vv->proj,temp_mat,SDIM,v->freedom,vv->freedom);
            mat = temp_mat;
          }
          else if ( v->proj )
          { 
            if ( hessian_normal_perp_flag )
            { REAL w[MAXCOORD],*wp=w,*np=normal;
              vec_mat_mul(normal,v->proj,w,SDIM,v->freedom);
              tr_mat_mul(&wp,&np,temp_mat,1,v->freedom,vv->freedom);
              cfudge = 1.0;
            }
            else 
              tr_mat_mul(v->proj,identmat,temp_mat,SDIM,v->freedom,vv->freedom);
              mat = temp_mat;
          }
          else if ( vv->proj )
          {
            if ( hessian_normal_perp_flag )
            { REAL ww[MAXCOORD],*wwp=ww,*np=normal;
              vec_mat_mul(normal,vv->proj,ww,SDIM,vv->freedom);
              tr_mat_mul(&np,&wwp,temp_mat,1,v->freedom,vv->freedom);
              cfudge = 1.0;
              mat = temp_mat;
            }
            else 
              mat = vv->proj;
          }
          else
          {
            if ( hessian_normal_perp_flag )
            { REAL *np=normal;
              tr_mat_mul(&np,&np,temp_mat,1,v->freedom,vv->freedom);
              cfudge = 1.0;
              mat = temp_mat;
            }
            else 
            mat = identmat;
          }
          for ( jj = 0 ; jj < v->freedom ; jj++ )  
           for ( kk = 0 ; kk < vv->freedom ; kk++ )  
           { col = vv->rownum + kk + A_OFF ;
             end = M->IA[v->rownum+jj+1]-A_OFF;
             for ( j = M->IA[v->rownum+jj]+1-A_OFF ; (int)j < end ; j++ )
               if ( (M->JA[j] < A_OFF) || (M->JA[j] == col)  )
               { M->JA[j] = col; 
                 M->A[j] += cfudge*mat[jj][kk]*weights[i][k]*area;
                 break;
               }
             if ( j == end )
               kb_error(1854,"Internal error: Bad j in linear_metric_setup().\n",RECOVERABLE);
           }
         }
       }
     }
  }



  /* compact matrix by looking for -1 in M->JA */
  for ( i = 0 ; i < M->IA[M->N]-A_OFF ; i++ )
     if ( M->JA[i] < A_OFF ) break;
  if ( i < M->IA[M->N]-A_OFF )
  { int *ja_to_spot = M->JA,*ja_from_spot = M->JA;
    REAL *a_to_spot= M->A,*a_from_spot= M->A;
    for ( i = 0 ; i < M->N ; i++ )
    { end = M->IA[i+1]-A_OFF;
      for ( j = ja_from_spot - M->JA ; (int)j < end ; j++,ja_from_spot++,a_from_spot++ )
      { if ( *ja_from_spot >= A_OFF )
        { *ja_to_spot = *ja_from_spot;
          *a_to_spot  = *a_from_spot;
          a_to_spot++; ja_to_spot++;
        }
      }
      M->IA[i+1] = (int)(ja_to_spot - M->JA + A_OFF);
    }
  }

apinv_setup:
  /* approximate inverse, as diagonal inverse sum of row */
  M->apinv = (REAL *)temp_calloc(M->N,sizeof(REAL));
  /* sums of rows */
  for ( i = 0 ; i < M->N ; i++ )
  { int start = M->IA[i] - A_OFF;
    end = M->IA[i+1] - A_OFF;
    M->apinv[i] += M->A[start];
    for ( j = start+1 ; (int)j < end ; j++ )
    { M->apinv[i] += M->A[j];
      M->apinv[M->JA[j]-A_OFF] += M->A[j];
    }
  }
  for ( i = 0 ; i < M->N ; i++ ) 
     M->apinv[i] = (M->apinv[i]==0.0) ? 0.0 : 1/M->apinv[i];

if ( hess_debug) 
{ printf("Metric matrix:\n");
  printf("IA: ");
  for ( i = 0 ; i <= M->N ; i++ ) printf("%d ",M->IA[i]);
  printf("\nJA: ");
  for ( i = 0 ; i < M->IA[M->N]-A_OFF ; i++ ) printf("%d ",M->JA[i]);
  printf("\nA: \n");
  for ( i = 0 ; i < M->N ; i++ )
  { int k,m;
    for ( m = 0 ; m < i ; m++ ) printf("          ");
    for ( m = i,  k = M->IA[i]-A_OFF ; m < M->N ; m++ )
      if ( (m == M->JA[k]-A_OFF) && (k < M->IA[i+1]-A_OFF) )
        { printf(" %9.6f",(DOUBLE)M->A[k]); k++; }
      else printf(" %9.6f",0.0);
    printf("\n");
  }
  printf("\n");
}

} /* end linear_metric_setup() */

/************************************************************************
*
* function: linear_metric_setup_quadratic()
*
* purpose:  set up vector-to-form metric sparse matrix for local
*              linear interpolation metric.  Quadratic mode.
*/

void linear_metric_setup_quadratic(S,M)
struct linsys *S;  /* hessian to set up metric for */
struct linsys *M;  /* pointer to empty structure */
{
  int i,j,jj,k,kk,m,n;
  int sum;
  edge_id e_id;
  vertex_id v_id;
  facet_id f_id;
  int hi;
  REAL len;
  MAT2D(weights,FACET_CTRL,FACET_CTRL);
  MAT2D(temp_mat,MAXCOORD,MAXCOORD);
  REAL **mat;
  int col,end;
  int ii[FACET_CTRL];
  vertex_id vv[FACET_CTRL];
  int lo;

  if ( ysmp_flag != MINDEG_FACTORING )
  { ysmp_flag = MINDEG_FACTORING;
    sp_mul_func = bk_mul;
    sp_AIJ_setup_func = bk_AIJ_setup;
    sp_constraint_setup_func = bk_constraint_setup;
    sp_hess_project_setup_func= BK_hess_project_setup;
    sp_factor_func = xmd_factor;
    sp_solve_func = xmd_solve;
    sp_solve_multi_func = xmd_solve_multi;
    sp_ordering_func = NULL;
    sp_CHinvC_func = sp_CHinvC;
    outstring(
       "Using alternate minimal degree with linear interpolation metric.\n");
  }
  M->N = S->N;
  M->flags &= ~ S_ODRV_REORDERED; 

  /* allocate storage for arrays */
  M->IA = (int *)temp_calloc(M->N+1,sizeof(int));
  if ( M->P == NULL ) M->P = (int *)temp_calloc(M->N,sizeof(int));
  if ( M->IP == NULL ) M->IP = (int *)temp_calloc(M->N,sizeof(int));

  for ( i = S->optparamrowstart ; i < S->N ; i++ )
  { /* make sure rest of diagonal exists, but no shift */
    M->IA[i] = 1;
  }

  /* have to count number of higher-ordinal neighbors of each vertex */
  FOR_ALL_VERTICES(v_id)
  { struct hess_verlist *vh = get_vertex_vhead(v_id);
    for ( j = vh->rownum, jj = 0 ; jj < vh->freedom ; jj++,j++ )
       M->IA[j] += vh->freedom-jj;
  }
  FOR_ALL_EDGES(e_id)
  { vertex_id v[3];
    struct hess_verlist *vh[3];
    v[0] = get_edge_tailv(e_id);
    v[1] = get_edge_headv(e_id);
    v[2] = get_edge_midv(e_id);
    for ( i = 0 ; i < 3 ; i ++ )
      vh[i] = get_vertex_vhead(v[i]);
    for ( i = 0 ; i < 3 ; i ++ )
      for ( k = 0 ; k < 3 ; k++ )
        if ( loc_ordinal(v[i]) < loc_ordinal(v[k]) )
           for ( j = vh[i]->rownum, jj = 0 ; jj < vh[i]->freedom ; j++,jj++ )
              M->IA[j] += vh[k]->freedom;
  }
  if ( web.representation == SOAPFILM )
  FOR_ALL_FACETS(f_id)
  { vertex_id vv[FACET_CTRL];
    struct hess_verlist *vh[FACET_CTRL];
    facetedge_id  fe_id = get_facet_fe(f_id);
    int v[FACET_CTRL];

    for ( i = 0 ; i < FACET_EDGES ; i++ )
    {
       vv[i] = get_fe_tailv(fe_id);
       vv[i+3] = get_fe_midv(fe_id);
       fe_id = get_next_edge(fe_id);
    }

    for ( i = 0 ; i < FACET_CTRL ; i++ ) 
    { v[i] = loc_ordinal(vv[i]);
      vh[i] = get_vertex_vhead(v[i]);
    }

#define ADDON(a,b) \
    if ( v[a] > v[b] ) {lo = b; hi = a; } else { lo= a; hi=b;}\
    for ( j = vh[lo]->rownum, jj = 0 ; jj < vh[lo]->freedom ; j++,jj++ )\
        M->IA[j] += vh[hi]->freedom;

    ADDON(0,4); ADDON(1,5); ADDON(2,3); ADDON(4,5); ADDON(3,4); ADDON(3,5);
  }
  for ( i = 0, sum = 0 ; i < M->N ; i++ ) /* add to running totals */
  { int tmp = sum; 
    sum += M->IA[i];
    M->IA[i] = tmp + A_OFF;
  }
  M->IA[M->N] = sum + A_OFF;

  /* now can allocate space */
  M->JA = (int *)temp_calloc(sum,sizeof(int));
  M->A  = (REAL *)temp_calloc(sum,sizeof(REAL));

  for ( i = 0 ; i < sum ; i++ ) M->JA[i] = -1;  /* vacant */
  for ( i = 0 ; i < M->N ; i++ )
  { M->JA[M->IA[i]-A_OFF] = i + A_OFF; /* diagonal entry at start */
    M->IP[i] = M->P[i] = i;    /* unpermuted */
  }

  /* set up interpolation weight matrix */
  for ( i = 0 ; i < optparamcount ; i++ )
    M->A[M->IA[S->optparamrowstart+i]-A_OFF] = 1.0;
  /* leave rest of diagonal zero in case of augmented hessian */


  if ( web.representation == STRING )
  { MAT2D(x,EDGE_CTRL,MAXCOORD);
    REAL tang[MAXCOORD];

    /* edge by edge */
    FOR_ALL_EDGES(e_id)
    { 
      vv[0] = get_edge_tailv(e_id);
      vv[1] = get_edge_midv(e_id);
      vv[2] = get_edge_headv(e_id);
      ii[0] = loc_ordinal(vv[0]);
      ii[1] = loc_ordinal(vv[1]);
      ii[2] = loc_ordinal(vv[2]);
      get_edge_verts(e_id,x,NULL);
      len = get_edge_length(e_id);

      /* weight matrix */
      for ( i = 0 ; i < edge_ctrl ; i++ )
        for ( j = 0 ; j < edge_ctrl ; j++ ) weights[i][j] = 0.0;

      for ( m = 0 ; m < gauss1D_num ; m++ )
      { REAL value;
        for ( n = 0 ; n < SDIM ; n ++ )
        { tang[n] = 0.0;
          for ( k = 0 ; k < edge_ctrl ; k++ )
          tang[n] += gauss1polyd[k][m]*x[k][n];
        }
        value = gauss1Dwt[m]*sqrt(SDIM_dot(tang,tang));
        for ( i = 0 ; i < edge_ctrl ; i++ )
        { for ( j = 0 ; j < edge_ctrl ; j++ )
            weights[i][j] += quadratic_metric_mix*value
                             *gauss1poly[i][m]*gauss1poly[j][m];
        }
      }
      /* simple vertex weighting part */
      for ( i = 0 ; i < edge_ctrl ; i++ )
        weights[i][i] += (1-quadratic_metric_mix)*((i==1) ? 0.5*len : 0.25*len);

      /* fill in metric matrix */
      for ( i = 0 ; i < 3 ; i++ )
      { struct hess_verlist *vh = get_vertex_vhead(vv[i]);
         
        /* self */
        if ( vh->proj )
        { 
          tr_mat_mul(vh->proj,vh->proj,temp_mat,SDIM,vh->freedom,vh->freedom);
          mat = temp_mat;
        }
        else mat = identmat;
        for ( jj = 0 ; jj < vh->freedom ; jj++ )
          for ( k = jj ; k < vh->freedom ; k++ )
          { col = vh->rownum + k + A_OFF;
            end = M->IA[vh->rownum+jj+1]-A_OFF;
            for ( j = M->IA[vh->rownum+jj]-A_OFF ; j < end  ; j++ )
              if ( (M->JA[j] < A_OFF) || (M->JA[j] == col) )
              { M->JA[j] = col;
                M->A[j] += vh->slant*vh->slant*weights[i][i]*mat[jj][k];
                break;
              }
              if ( j == end )
                 kb_error(1856,"Internal error in linear_metric_setup.\n",
                    RECOVERABLE);
          }

        /* cross terms */
        for ( k = 0 ; k < 3 ; k++ )
        { struct hess_verlist *vvh = get_vertex_vhead(vv[k]);
          if ( ii[k] <= ii[i] ) continue;
          if ( vh->proj && vvh->proj )
          { tr_mat_mul(vh->proj,vvh->proj,temp_mat,SDIM,vh->freedom,
               vvh->freedom);
            mat = temp_mat;
          }
          else if ( vh->proj )
          { tr_mat_mul(vh->proj,identmat,temp_mat,SDIM,vh->freedom,vvh->freedom);
            mat = temp_mat;
          }
          else if ( vvh->proj ) mat = vvh->proj;
          else mat = identmat;

          for ( jj = 0 ; jj < vh->freedom ; jj++ )  
            for ( kk = 0 ; kk < vvh->freedom ; kk++ )  
            { col = vvh->rownum + kk + A_OFF ;
              end = M->IA[vh->rownum+jj+1]-A_OFF;
              for ( j = M->IA[vh->rownum+jj]+1-A_OFF ; ; j++ )
                if ( (M->JA[j] < A_OFF) || (M->JA[j] == col)  )
                { M->JA[j] = col; 
                  M->A[j] += vh->slant*vvh->slant*mat[jj][kk]*weights[i][k];
                  break;
                }
              if ( j == end )
                 kb_error(1857,"Internal error in linear_metric_setup.\n",
                   RECOVERABLE);
            }
         }
       }
    }
  }
  else 
  { 
     REAL area;
     MAT2D(tang,2,MAXCOORD);
     MAT2D(x,FACET_CTRL,MAXCOORD);

     /* facet by facet */
     FOR_ALL_FACETS(f_id)
     { vertex_id ix[FACET_CTRL];
       REAL density = get_facet_density(f_id);
       facetedge_id  fe_id = get_facet_fe(f_id);

       if ( density == 0.0 ) density = 1.0;

       for ( i = 0 ; i < FACET_EDGES ; i++ )
       {
         ix[2*i] = get_fe_tailv(fe_id);
         ix[2*i+1] = get_fe_midv(fe_id);
         fe_id = get_next_edge(fe_id);
       }
       area = get_facet_area(f_id);
       for ( i = 0 ; i < FACET_CTRL ; i++ ) 
          ii[i] = loc_ordinal(ix[i]);

       /* weight matrix */
       get_facet_verts(f_id,x,NULL);
       for ( i = 0 ; i < FACET_CTRL ; i++ )
         for ( j = 0 ; j < FACET_CTRL ; j++ ) weights[i][j] = 0.0;
       /* simple vertex weighting part */
       for ( i = 0 ; i < 3 ; i++ )
       { weights[2*i][2*i] = (1-quadratic_metric_mix)/12*area; 
         weights[2*i+1][2*i+1] = (1-quadratic_metric_mix)/4*area;
       }

       for ( m = 0 ; m < gauss2D_num ; m++ )
       { REAL value,ss,st,tt,det;
         mat_mult(gpolypartial[m],x,tang,web.dimension,FACET_CTRL,SDIM);
         ss = SDIM_dot(tang[0],tang[0]);
         st = SDIM_dot(tang[0],tang[1]);
         tt = SDIM_dot(tang[1],tang[1]);
         det = ss*tt - st*st;
         value = gauss2Dwt[m]*sqrt(det)/2; /* with triangle factor */
         for ( i = 0 ; i < FACET_CTRL ; i++ )
         { for ( j = 0 ; j < FACET_CTRL ; j++ )
            weights[i][j] += quadratic_metric_mix*value*gpoly[m][i]*gpoly[m][j];
         }
       }
       for ( i = 0 ; i < FACET_CTRL ; i++ )
         for ( j = 0 ; j < FACET_CTRL ; j++ ) 
           weights[i][j] *= density;

       for ( i = 0 ; i < FACET_CTRL ; i++ )
       { struct hess_verlist *v = get_vertex_vhead(ix[i]);

         /* self */
         if ( v->proj )
         { 
           tr_mat_mul(v->proj,v->proj,temp_mat,SDIM,v->freedom,v->freedom);
           mat = temp_mat;
         }
         else mat = identmat;

         for ( jj = 0 ; jj < v->freedom ; jj++ )
           for ( k = jj ; k < v->freedom ; k++ )
           { col = v->rownum + k + A_OFF;
             end = M->IA[v->rownum+jj+1]-A_OFF;
             for ( j = M->IA[v->rownum+jj]-A_OFF ; j < end  ; j++ )
               if ( (M->JA[j] < A_OFF) || (M->JA[j] == col) )
               { M->JA[j] = col;
                 M->A[j] += v->slant*v->slant*weights[i][i]*mat[jj][k];
                 break;
               }
               if ( j == end )
                  kb_error(1858,"Internal error in linear_metric_setup.\n",
                      RECOVERABLE);
           }

         /* cross terms */
         for ( k = 0 ; k < FACET_CTRL ; k++ )
         { struct hess_verlist *vv = get_vertex_vhead(ix[k]);
            
           if ( ii[k] <= ii[i] ) continue;

            if ( v->proj && vv->proj )
            { tr_mat_mul(v->proj,vv->proj,temp_mat,SDIM,v->freedom,vv->freedom);
              mat = temp_mat;
            }
            else if ( v->proj )
            { tr_mat_mul(v->proj,identmat,temp_mat,SDIM,v->freedom,vv->freedom);
              mat = temp_mat;
            }
            else if ( vv->proj ) mat = vv->proj;
            else mat = identmat;


            for ( jj = 0 ; jj < v->freedom ; jj++ )  
              for ( kk = 0 ; kk < vv->freedom ; kk++ )  
              { col = vv->rownum + kk + A_OFF ;
                end = M->IA[v->rownum+jj+1]-A_OFF;
                for ( j = M->IA[v->rownum+jj]+1-A_OFF ; j < end ; j++ )
                  if ( (M->JA[j] < A_OFF) || (M->JA[j] == col)  )
                  { M->JA[j] = col; 
                    M->A[j] += v->slant*vv->slant*mat[jj][kk]*weights[i][k];
                    break;
                  }
                if ( j == end )
                   kb_error(1859,
     "Internal error: Bad j in linear_metric_setup_quadratic().\n",RECOVERABLE);

              }
           }
        }
     }
  }

  /* compact matrix by looking for -1 in M->JA */
  for ( i = 0 ; i < M->IA[M->N]-A_OFF ; i++ )
     if ( M->JA[i] < A_OFF )
     { kb_error(1860,"Internal error: Empty spot in Met.\n",WARNING);
        break;
     }
  if ( i < M->IA[M->N]-A_OFF )
  { int *ja_to_spot = M->JA,*ja_from_spot = M->JA;
     REAL *a_to_spot= M->A,*a_from_spot= M->A;
     for ( i = 0 ; i < M->N ; i++ )
     { end = M->IA[i+1]-A_OFF;
        for ( j = M->IA[i]-A_OFF ; j < end ; j++,ja_from_spot++,a_from_spot++ )
        { if ( M->JA[j] >= A_OFF )
          { *ja_to_spot = *ja_from_spot;
             *a_to_spot  = *a_from_spot;
             a_to_spot++; ja_to_spot++;
          }
        }
     }
  }

} /* end linear_metric_setup_quadratic() */

/************************************************************************
*
* function: linear_metric_setup_lagrange()
*
* purpose:  set up vector-to-form metric sparse matrix for local
*              linear interpolation metric.  Lagrange model.
*/

void linear_metric_setup_lagrange(S,M)
struct linsys *S;  /* hessian to set up metric for */
struct linsys *M;  /* pointer to empty structure */
{
  int i,j,jj,k,kk,m,n;
  edge_id e_id;
  facet_id f_id;
  MAT2D(temp_mat,MAXCOORD,MAXCOORD);
  REAL **mat=NULL;
  int col,row;
  int *ii;
  REAL **weights=NULL;
  REAL **x = NULL;
  struct mentry { int row; int col; REAL val; } *temp,*mlist;
  int maxcount;
  int count,total;

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
     outstring(
        "Using alternate minimal degree with Lagrange linear interpolation metric.\n");
  }
  M->N = S->N;
  M->flags &= ~ S_ODRV_REORDERED; 

  /* allocate storage for arrays */
  M->IA = (int *)temp_calloc(M->N+1,sizeof(int));
  if ( M->P == NULL ) M->P = (int *)temp_calloc(M->N,sizeof(int));
  if ( M->IP == NULL ) M->IP = (int *)temp_calloc(M->N,sizeof(int));

  for ( i = 0 ; i < M->N ; i++ )
     M->IP[i] = M->P[i] = i;    /* unpermuted */


  if ( web.representation == STRING )
  { REAL tang[MAXCOORD];
    int ctrl = web.skel[EDGE].ctrlpts;
    struct gauss_lag *gl = &gauss_lagrange[web.dimension][web.gauss1D_order];

    ii = (int*)temp_calloc(ctrl,sizeof(int));
    weights = dmatrix(0,ctrl,0,ctrl);
    x = dmatrix(0,ctrl,0,SDIM);
    maxcount = web.skel[EDGE].count*ctrl*(ctrl+1)/2*SDIM*SDIM;
    maxcount += M->N - M->optparamrowstart + 10;
    mlist = (struct mentry *)temp_calloc(maxcount,sizeof(struct mentry));
    count = 0;
    /* edge by edge */
    FOR_ALL_EDGES(e_id)
    { vertex_id *v = get_edge_vertices(e_id);
      for ( i = 0 ; i < ctrl ; i++ ) ii[i] = loc_ordinal(v[i]);
      get_edge_verts(e_id,x,NULL);

      /* weight matrix */
      for ( i = 0 ; i < ctrl ; i++ )
        for ( j = 0 ; j < ctrl ; j++ ) weights[i][j] = 0.0;

      for ( m = 0 ; m < gl->gnumpts ; m++ )
      { REAL value;
        for ( n = 0 ; n < SDIM ; n ++ )
        { tang[n] = 0.0;
          for ( k = 0 ; k < ctrl ; k++ )
            tang[n] += gl->gpolypart[m][0][k]*x[k][n];
        }
        value = gl->gausswt[m]*sqrt(SDIM_dot(tang,tang));
        for ( i = 0 ; i < ctrl ; i++ )
        { for ( j = 0 ; j < ctrl ; j++ )
            weights[i][j] += quadratic_metric_mix*value
                   *gl->gpoly[m][i]*gl->gpoly[m][j];
        }
      }
      /* simple vertex weighting part */
      weights[0][0] += (1-quadratic_metric_mix)*.5/(ctrl-1);
      weights[ctrl][ctrl] = (1-quadratic_metric_mix)*.5/(ctrl-1);
      for ( i = 1 ; i < edge_ctrl-1 ; i++ )
        weights[i][i] += (1-quadratic_metric_mix)/(ctrl-1);

      /* fill in metric matrix */
      for ( i = 0 ; i < ctrl ; i++ )
      { struct hess_verlist *vh = get_vertex_vhead(v[i]);
        
        /* self */
        if ( vh->proj )
        { 
          tr_mat_mul(vh->proj,vh->proj,temp_mat,SDIM,vh->freedom,vh->freedom);
          mat = temp_mat;
        }
        else mat = identmat;
        for ( jj = 0 ; jj < vh->freedom ; jj++ )
          for ( k = jj ; k < vh->freedom ; k++ )
          { col = vh->rownum + k;
            row = vh->rownum + jj;
            mlist[count].col = col;
            mlist[count].row = row;
            mlist[count++].val = vh->slant*vh->slant*weights[i][i]*mat[jj][k];
          }

          /* cross terms */
          for ( k = 0 ; k < ctrl ; k++ )
          { struct hess_verlist *vv = get_vertex_vhead(v[k]);
            if ( ii[k] <= ii[i] ) continue;
            if ( vh->proj && vv->proj )
            { tr_mat_mul(vh->proj,vv->proj,temp_mat,SDIM,vh->freedom,vv->freedom);
              mat = temp_mat;
            }
            else if ( vh->proj )
            { tr_mat_mul(vh->proj,identmat,temp_mat,SDIM,vh->freedom,vv->freedom);
              mat = temp_mat;
            }
            else if ( vv->proj ) mat = vv->proj;
            else mat = identmat;

            for ( jj = 0 ; jj < vh->freedom ; jj++ )  
              for ( kk = 0 ; kk < vv->freedom ; kk++ )  
                { col = vv->rownum + kk;
                  row = vh->rownum+jj;
                  mlist[count].col = col;
                  mlist[count].row = row;
                  mlist[count++].val = vh->slant*vv->slant*mat[jj][kk]*weights[i][k];
                }
          }
        }
     }
  }
  else /* facets */
  { 
    MAT2D(tang,MAXCOORD,MAXCOORD);
    int ctrl = web.skel[FACET].ctrlpts;
    struct gauss_lag *gl = &gauss_lagrange[web.dimension][web.gauss2D_order];
    MAT2D(matt,MAXCOORD,MAXCOORD);

    ii = (int*)temp_calloc(ctrl,sizeof(int));
    weights = dmatrix(0,ctrl,0,ctrl);
    x = dmatrix(0,ctrl,0,SDIM);
    maxcount = web.skel[FACET].count*ctrl*(ctrl+1)/2*SDIM*SDIM;
    maxcount += M->N - M->optparamrowstart + 10;
    count = 0;
    mlist = (struct mentry *)temp_calloc(maxcount,sizeof(struct mentry));

    /* facet by facet */
    FOR_ALL_FACETS(f_id)
    { vertex_id *ix = get_facet_vertices(f_id);
      REAL density = get_facet_density(f_id);

      for ( i = 0 ; i < ctrl ; i++ ) ii[i] = loc_ordinal(ix[i]);
      /* weight matrix */
      get_facet_verts(f_id,x,NULL);
      for ( i = 0 ; i < ctrl ; i++ )
        for ( j = 0 ; j < ctrl ; j++ ) weights[i][j] = 0.0;
      /* simple vertex weighting part */
      for ( i = 0 ; i < ctrl ; i++ ) /* not very precise */
      { int attr = get_vattr(ix[i]);
        if ( attr & Q_MIDFACET )
            weights[i][i] = (1-quadratic_metric_mix)/ctrl; 
        else if ( attr & Q_MIDEDGE )
            weights[i][i] = (1-quadratic_metric_mix)/ctrl;
        else
            weights[i][i] = (1-quadratic_metric_mix)/ctrl;
      }

      for ( m = 0 ; m < gl->gnumpts ; m++ )
      { REAL value,det;
        mat_mult(gl->gpolypart[m],x,tang,web.dimension,ctrl,SDIM);
        for ( i = 0 ; i < web.dimension ; i++ )
          for ( j = 0 ; j <= i ; j++ )
            matt[i][j] = matt[j][i] = SDIM_dot(tang[i],tang[j]);
        det = det_adjoint(matt,web.dimension);
        if ( det <= 0.0 ) value = 0.0;
        else value = gl->gausswt[m]*sqrt(det)/factorial[web.dimension];
        for ( i = 0 ; i < ctrl ; i++ )
        { for ( j = 0 ; j < ctrl ; j++ )
            weights[i][j] += quadratic_metric_mix*value
                          *gl->gpoly[m][i]*gl->gpoly[m][j];
        }
      }
      for ( i = 0 ; i < ctrl ; i++ )
        for ( j = 0 ; j < ctrl ; j++ ) weights[i][j] *= density;

      for ( i = 0 ; i < ctrl ; i++ )
      { struct hess_verlist *v = get_vertex_vhead(ix[i]);

        /* self */
        if ( v->proj )
        { 
          tr_mat_mul(v->proj,v->proj,temp_mat,SDIM,v->freedom,v->freedom);
          mat = temp_mat;
        }
        else mat = identmat;

        for ( jj = 0 ; jj < v->freedom ; jj++ )
          for ( k = jj ; k < v->freedom ; k++ )
          { col = v->rownum + k;
            row = v->rownum+jj;
            mlist[count].col = col;
            mlist[count].row = row;
            mlist[count++].val = v->slant*v->slant*weights[i][i]*mat[jj][k];
          }

        /* cross terms */
        for ( k = 0 ; k < ctrl ; k++ )
        { struct hess_verlist *vv = get_vertex_vhead(ix[k]);
            
          if ( ii[k] <= ii[i] ) continue;

          if ( v->proj && vv->proj )
          { tr_mat_mul(v->proj,vv->proj,temp_mat,SDIM,v->freedom,vv->freedom);
            mat = temp_mat;
          }
          else if ( v->proj )
          { tr_mat_mul(v->proj,identmat,temp_mat,SDIM,v->freedom,vv->freedom);
            mat = temp_mat;
          }
          else if ( vv->proj ) mat = vv->proj;
          else mat = identmat;

          for ( jj = 0 ; jj < v->freedom ; jj++ )  
            for ( kk = 0 ; kk < vv->freedom ; kk++ )  
            { col = vv->rownum + kk;
              row = v->rownum+jj;
              mlist[count].col = col;
              mlist[count].row = row;
              mlist[count++].val = 
                 v->slant*vv->slant*mat[jj][kk]*weights[i][k];
            }
         }
       }
     }
  }
  if ( count > maxcount ) 
  {kb_error(1869,
     "Internal error in linear_metric setup Lagrange: count > maxcount",
    RECOVERABLE);
  }

  /* non-element stuff */
  for ( i = 0 ; i < optparamcount ; i++ )
  { mlist[count].row = S->optparamrowstart+i;
    mlist[count].col = S->optparamrowstart+i;
    mlist[count].val = 1.0;
    count++;
  }
  /* leave rest of diagonal zero in case of augmented hessian */

  for ( i = S->optparamrowstart ; i < S->N ; i++ )
  { /* make sure rest of diagonal exists, but no shift */
    mlist[count].row = i;
    mlist[count].col = i;
    mlist[count].val = 0.0;
    count++;
  }


  /* sort and combine entries */
  temp = (struct mentry *)temp_calloc(count,sizeof(struct mentry));
  /* radix sort, first on columns */
  /* count columns */
  for ( i = 0 ; i < count ; i++ ) M->IA[mlist[i].col]++;
  for ( n = 0, total = 0 ; n < M->N ; n++ )
  { int tmp = M->IA[n];
    M->IA[n] = total;
    total += tmp;
  }
  M->IA[M->N] = total;
  /* sort into temp */
  for ( i = 0 ; i < count ; i++ )
     temp[M->IA[mlist[i].col]++] = mlist[i];
  /* now sort on rows */
  for ( n = 0; n < M->N ; n++ )M->IA[n] = 0;
  for ( i = 0 ; i < count ; i++ ) M->IA[temp[i].row]++;
  for ( n = 0, total = 0 ; n < M->N ; n++ )
  { int tmp = M->IA[n];
    M->IA[n] = total;
    total += tmp;
  }
  M->IA[M->N] = total;
  /* sort into mlist */
  for ( i = 0 ; i < count ; i++ )
     mlist[M->IA[temp[i].row]++] = temp[i];
  /* compress */
  for ( i = 1, j = 0 ; i < count ; i++ )
  { if ( (mlist[j].row == mlist[i].row) && (mlist[j].col == mlist[i].col) )
         mlist[j].val += mlist[i].val;
    else { mlist[++j] = mlist[i]; }
  }

  /* put into M structure */
  count = j+1;
  M->JA = (int*)temp_calloc(count,sizeof(int));
  M->A = (REAL *)temp_calloc(count,sizeof(REAL));
  M->IA[0] = A_OFF;
  for ( j = 0 ; j < count ; j++ )
  { M->IA[mlist[j].row+1] = j + 1 + A_OFF;
    M->JA[j] = mlist[j].col + A_OFF;
    M->A[j] = mlist[j].val;
  }
  M->IA[M->N] = count + A_OFF;

  /* free working space */
  temp_free((char*)temp);
  temp_free((char*)mlist);
  if ( ii ) temp_free((char*)ii);
  if ( weights ) free_matrix(weights);
  if ( x ) free_matrix(x);

}  /* linear_metric_setup_lagrange() */
