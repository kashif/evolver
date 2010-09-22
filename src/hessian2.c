/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*********************************************************************
*
* FILE:  hessian2.c 
*
* Contents: more hessian routines 
*/

#include "include.h"

REAL difference_lagrangian ARGS((void));
int sturm ARGS((int,REAL*,REAL*,REAL));
REAL ev_bisect ARGS((REAL,REAL,int,int,int,REAL*,REAL*));

/***************************************************************************
*
* function: difference_lagrangian()
*
* purpose: calculate lagrangian E - \lambda Q for difference_hessian,
*             where E is energy and Q is constraint with lagrange multiplier
*             \lambda
*/


REAL difference_lagrangian()
{ REAL sum;
  body_id b_id;
  struct gen_quant *q;
  int k;

  sum = web.total_energy;
  FOR_ALL_BODIES(b_id)
    if ( get_battr(b_id) & FIXEDVOL )
      sum -= get_body_pressure(b_id)*get_body_volume(b_id);
  for ( k = 0 ; k < gen_quant_count ; k++ )
  { q = GEN_QUANT(k);
    if ( q->flags & Q_FIXED )
      sum -= q->pressure*q->value;
  }
  return sum;
}


/**************************************************************************
*
* function: difference_hessian()
*
* purpose: calculation of hessian by differences in energies.
*             meant only for checking other hessian calculations.
*
*/

void difference_hessian(S,verhead,rhs)
struct linsys *S;
struct hess_verlist *verhead;  /* main vertex list */
REAL *rhs;
{ REAL dx = 0.00001;  /* wiggle size */
  REAL first[MAXCOORD];
  MAT2D(second,MAXCOORD,MAXCOORD);
  REAL e1,e2,e3,e4;
  vertex_id v_id,vv_id;
  int i,j;
  REAL energyuu,energyud,energydu,energydd; /* really Lagrangians */

  /* right hand side and self second derivatives */
  /* move vertices 1 by 1 */
  FOR_ALL_VERTICES(v_id)
  {
     int ord = loc_ordinal(v_id);
     struct hess_verlist *v = verhead+ord;
     REAL *x = get_coord(v_id);

     if ( v->freedom == 0 ) continue;
     for ( i = 0 ; i < SDIM ; i++ )
      for ( j = 0 ; j <= i ; j++ )
      { 
         x[i] += dx; 
         x[j] += dx; global_timestamp++; calc_energy(); calc_content(Q_FIXED);
         energyuu = difference_lagrangian();
         x[j] -= 2*dx; global_timestamp++; calc_energy(); calc_content(Q_FIXED);
         energyud = difference_lagrangian();
         x[i] -= 2*dx; global_timestamp++; calc_energy(); calc_content(Q_FIXED);
         energydd = difference_lagrangian();
         x[j] += 2*dx; global_timestamp++; calc_energy(); calc_content(Q_FIXED);
         energydu = difference_lagrangian();
         x[i] += dx; x[j] -= dx; /* back to normal */
         first[i] = (energyuu - energydu + energyud - energydd)/4/dx;
         second[i][j] = second[j][i] =
            (energyuu - energyud - energydu + energydd)/4/dx/dx;
      }
     fill_grad(S,v,first,rhs);
     if ( hess_flag )
        fill_self_entry(S,v_id,second);
  }

  /* second derivatives, go through all pairs of vertices */
  if ( hess_flag )
  FOR_ALL_VERTICES(v_id)
  {
     int ord = loc_ordinal(v_id);
     struct hess_verlist *v = verhead+ord;
     REAL *x = get_coord(v_id);

     if ( v->freedom == 0 ) continue;
     
     FOR_ALL_VERTICES(vv_id)
     {
        int ordd = loc_ordinal(vv_id);
        struct hess_verlist *vv = verhead+ordd;
        REAL *xx = get_coord(vv_id);

        if ( vv->freedom == 0 ) continue;
        if ( ord <= ordd ) continue; /* hessian is lower triangular */ 
      
        for ( i = 0 ; i < SDIM ; i++ )
          for ( j = 0 ; j < SDIM ; j++ )
             {
                x[i] += dx; 
                xx[j] += dx; global_timestamp++; calc_energy(); calc_content(Q_FIXED);
                e1 = difference_lagrangian();
                x[i] -= 2*dx; global_timestamp++; calc_energy(); calc_content(Q_FIXED);
                e2 = difference_lagrangian();
                xx[j] -= 2*dx; global_timestamp++; calc_energy(); calc_content(Q_FIXED);
                e3 = difference_lagrangian();
                x[i] += 2*dx; global_timestamp++; calc_energy(); calc_content(Q_FIXED);
                e4 = difference_lagrangian();
                second[i][j] = (e1 - e2 + e3 - e4)/4/dx/dx;
                x[i] -= dx; xx[j] += dx;
             }
        fill_mixed_entry(S,v_id,vv_id,second);
     }
  }
  calc_energy(); calc_content(Q_FIXED); /* cleanup */
} /* end difference_hessian */

/******************************************************************
*
*  function: dirichlet()
*
*  Purpose:  Minimization via Dirichlet integral,
*                as done by Pinkall and Polthier.
*
*  Argument: 0 for stepsize 1 (to minimum); 1 for seek minimum
*
*  Return:    stepsize
*/

/* Notes on method:

    Idea is to minimize Dirichlet energy of image of current
    surface.  Dirichlet energy of image of triangle is
    0.25*\sum_i cot(\alpha_i)*a_i^2
    where \alpha_i is angle of vertex in facet in domain
    and a_i is length of opposite edge in image.  Hence energy
    is pure quadratic and susceptible to exact solution.
    When equilibrium is reached, identity map is conformal
    so area is minimized.
    Numerical solution uses mechanisms of hessian minimization.
*/

REAL dirichlet(seekmode)
int seekmode; /* 1 if want to search in direction of min */
{
  REAL side[FACET_EDGES][MAXCOORD];
  REAL *x,*y;
  int r,s;
  REAL area;
  REAL cota;
  int i,j;
  facet_id f_id;
  edge_id e_id;
  vertex_id v[3];
  facetedge_id fe;
  REAL ss[FACET_VERTS],sd[FACET_VERTS];
  int ii,iii;
  REAL **rs;  /* one right side for each coordinate */
  REAL **xx;  /* one solution vector for each coordinate */
  struct hess_verlist *vc;              /* current  vertex */
  struct hess_verlist *rv,*sv;
  REAL scale;
  REAL best_scale;
  struct linsys S;
  vertex_id v_id;

  if ( fixed_constraint_flag ) 
     kb_error(2081,"Cannot do dirichlet with constraints.\n",RECOVERABLE);
  hmode = SINGLE_DEGREE; /* same hessian for each coordinate */
  hessian_init(&S,NULL);
  rs = dmatrix(0,SDIM-1,0,S.A_rows); /* right side for each coord */
  xx = dmatrix(0,SDIM-1,0,S.A_rows); /* solution vector for each coord */
                      /*  differences from current */
                                      
  /* fill in */
  FOR_ALL_FACETS(f_id)
    { fe = get_facet_fe(f_id);
      for ( i = 0 ; i < FACET_VERTS ; i++ )
         { e_id = get_fe_edge(fe);
            get_edge_side(e_id,side[i]);
            v[i] = get_edge_tailv(e_id);
            fe = get_next_edge(fe);
         }

      for ( i = 0 ; i < FACET_VERTS ; i++ )
        { ss[i] = SDIM_dot(side[i],side[i]);
          sd[i] = SDIM_dot(side[i],side[(i+2)%FACET_VERTS]);
        }

      area = sqrt(ss[0]*ss[2] - sd[0]*sd[0]);
      for ( i = 0 ; i < FACET_VERTS ; i++ )
        { ii = (i+1)%FACET_VERTS; iii = (i+2)%FACET_VERTS;
          x = get_coord(v[ii]);
          y = get_coord(v[iii]);
          rv = get_vertex_vhead(v[ii]);
          sv = get_vertex_vhead(v[iii]);
          r = rv->rownum;
          s = sv->rownum;
          cota = -sd[i]/area;
          if ( get_vattr(v[ii]) & FIXED )
             if ( get_vattr(v[iii]) & FIXED )
                continue;
             else /* rhs contribution */
             { 
                sp_hash_search(&S,sv->rownum,sv->rownum,0.5*cota);
                for ( j = 0 ; j < SDIM ; j++ )
                  rs[j][s] += 0.5*cota*(y[j] - x[j]);
             }
          else
             if ( get_vattr(v[iii]) & FIXED )
             { /* rhs contribution */
                sp_hash_search(&S,rv->rownum,rv->rownum,0.5*cota);
                for ( j = 0 ; j < SDIM ; j++ )
                  rs[j][r] += 0.5*cota*(x[j] - y[j]);
              }
             else /* main hessian contribution */
              {
                 sp_hash_search(&S,rv->rownum,rv->rownum,0.5*cota);
                 sp_hash_search(&S,sv->rownum,sv->rownum,0.5*cota);
                 if ( sv->rownum < rv->rownum )
                   sp_hash_search(&S,sv->rownum,rv->rownum,-0.5*cota);
                 else
                   sp_hash_search(&S,rv->rownum,sv->rownum,-0.5*cota);
                 for ( j = 0 ; j < SDIM ; j++ )
                    { rs[j][r] += cota/2*(x[j] - y[j]);
                      rs[j][s] += cota/2*(y[j] - x[j]);
                    }

              }
         }
     }
     
  /* extract stuff from hash table */
  sp_hash_end(&S,S.A_rows,S.total_rows,A_OFF);

  /* solve system */
  memset((char*)&S,0,sizeof(struct linsys));
  (*sp_AIJ_setup_func)(S.A_rows,&S);
  (*sp_constraint_setup_func)
            (web.skel[BODY].max_ord+1 + gen_quant_count,&S);
  if ( sp_ordering_func ) (*sp_ordering_func)(&S);
  sp_factor(&S);
  (*sp_hess_project_setup_func)(&S);
  for ( i = 0 ; i < SDIM ; i++ )
      sp_hessian_solve(&S,rs[i],xx[i],NO_SET_PRESSURE);

  /* move vertices */
  if ( seekmode == 0 ) { best_scale = 1.0; }
  else
  { REAL energies[3]; 
    REAL denom;
    int k;
    vertex_id v_id;
    
     save_coords(&saved,SAVE_IN_ATTR);  /* in case this doesn't work */
     energies[0] = web.total_energy;
     for ( scale = 1.0, k = 1 ; k < 3 ; scale += 1.0, k++ ) 
     { 
       FOR_ALL_VERTICES(v_id)
        { REAL *coord;

          vc = get_vertex_vhead(v_id);
          if ( vc->freedom == 0 ) continue;
          coord = get_coord(v_id);
          for ( j = 0 ; j < SDIM ; j++ )
             coord[j] = coord[j] - scale*xx[j][vc->rownum];
        }
        global_timestamp++;
        calc_energy();  /* energy after motion */
        if ( hess_debug || itdebug )
#ifdef LONGDOUBLE
          printf("scale %f energy %*.*Lf\n",(DOUBLE)scale,DWIDTH,DPREC,web.total_energy);
#else
          printf("scale %f energy %20.15f\n",(DOUBLE)scale,web.total_energy);
#endif 
        energies[k] = web.total_energy;
        restore_coords(&saved,SAVE_IN_ATTR);
     }
     denom = energies[0] - 2*energies[1] + energies[2];
     if ( denom <= 0.0 ) 
        best_scale = 1.0;  /* energy not convex */
     else best_scale = 0.5*(3*energies[0]-4*energies[1]+energies[2])/denom;
            
  }
  scale = best_scale;
  FOR_ALL_VERTICES(v_id)
  { REAL *coord;
    vc = get_vertex_vhead(v_id);
    if ( vc->freedom == 0 ) continue;
    coord = get_coord(vc->v_id);
    for ( j = 0 ; j < SDIM ; j++ )
      coord[j] = coord[j] - scale*xx[j][vc->rownum];
  }

  free_matrix(rs);
  free_matrix(xx);
  free_system(&S);
  hessian_exit(NULL);

  return best_scale;
} /* end dirichlet() */

/******************************************************************
*
*  function: sobolev()
*
*  Purpose:  Minimization via Sobolev inner product,
*                as done by Renka and Neuberger.
*
*  Argument: seekmode - 0 for jump to quadratic min, 1 to seek
*
*  Return:    stepsize
*/

/* Notes on method:

    R & N's method of using a Sobolev inner product to calculate
    the gradient is equivalent to using a positive definite
    quadratic form approximation for the area, and minimizing
    the quadratic form.
    Numerical solution uses mechanisms of hessian minimization.
*/

REAL sobolev(seekmode)
int seekmode;  /* 1 if seek for best stepsize */
{
  REAL side[FACET_EDGES][MAXCOORD];
  int r;
  REAL area;
  int i,j,k,n;
  facet_id f_id;
  edge_id e_id;
  vertex_id v[3];
  vertex_id v_id;
  facetedge_id fe;
  REAL ss[FACET_VERTS],sd[FACET_VERTS];
  int ii,iii;
  REAL *rs,*rstmp;  /* one right side for each coordinate */
  REAL *rs_save;
  struct hess_verlist *vc;              /* current  vertex */
  struct hess_verlist *rv,*sv;
  REAL scale;
  REAL best_scale;
  struct linsys S;

  if ( fixed_constraint_flag ) 
     kb_error(2082,"Cannot do sobolev with constraints.\n",RECOVERABLE);

kb_error(2083,"Sobolev not working properly.\n",WARNING);

  hmode = UNRESTRICTED; /* full hessian needed */
  hessian_init(&S,NULL);
  rs = (REAL*)temp_calloc(S.A_rows,sizeof(REAL)); /* right side for each coord */
  rs_save = (REAL*)temp_calloc(S.A_rows,sizeof(REAL)); /* right side for each coord */
  rstmp = (REAL*)temp_calloc(S.A_rows,sizeof(REAL)); /* right side for each coord */

  /* fill in */
  FOR_ALL_FACETS(f_id)
  { fe = get_facet_fe(f_id);
    for ( i = 0 ; i < FACET_VERTS ; i++ )
    { e_id = get_fe_edge(fe);
      get_edge_side(e_id,side[i]);
      v[i] = get_edge_tailv(e_id);
      fe = get_next_edge(fe);
    }

    for ( i = 0 ; i < FACET_VERTS ; i++ )
    { ss[i] = SDIM_dot(side[i],side[i]);
      sd[i] = SDIM_dot(side[i],side[(i+2)%FACET_VERTS]);
    }

    area = sqrt(ss[0]*ss[2] - sd[0]*sd[0]);
    for ( i = 0 ; i < FACET_VERTS ; i++ )
    { ii = (i+1)%FACET_VERTS; iii = (i+2)%FACET_VERTS;
      rv = get_vertex_vhead(v[ii]);
      sv = get_vertex_vhead(v[iii]);
      r = rv->rownum;
      /* vertex self-term */
      if ( !(get_vattr(v[iii]) & FIXED) )
      { 
        for ( j = 0 ; j < SDIM ; j++ )
        {
          sp_hash_search(&S,sv->rownum+j,sv->rownum+j,ss[i]/area);
          for ( k = j+1 ; k < SDIM ; k++ )
             sp_hash_search(&S,sv->rownum+j,sv->rownum+k,
                -side[i][j]*side[i][k]/area);
         }
      }
      /* edge cross-term */
      if ( !(get_vattr(v[iii]) & FIXED) && !(get_vattr(v[ii]) & FIXED) )
        for ( j = 0 ; j < SDIM ; j++ )
        { 
          if ( rv->rownum > sv->rownum )
          { sp_hash_search(&S,sv->rownum+j,rv->rownum+j,sd[i]/area);
            for ( k = 0 ; k < SDIM ; k++ )
               sp_hash_search(&S,sv->rownum+k,rv->rownum+j,
                   -side[i][j]*side[iii][k]/area);
          }
          else 
          {
            for ( k = 0 ; k < SDIM ; k++ )
              sp_hash_search(&S,rv->rownum+k,sv->rownum+j,
                  -side[i][j]*side[iii][k]/area);
          }
        }
      /* right side */ 
      if ( !(get_vattr(v[ii]) & FIXED) )
        for ( j = 0 ; j < SDIM ; j++ )
          rs[r+j] -= (sd[i]*side[iii][j] - ss[iii]*side[i][j])/area;
    }
  }
     
if ( hess_debug )
{ printf("Gradient: \n");
  FOR_ALL_VERTICES(v_id)
  { vc = get_vertex_vhead(v_id);
    if ( vc->freedom == 0 ) continue;
    printf("n: %s  ",ELNAME(v_id));
    for ( j = 0 ; j < SDIM ; j++ ) printf ("%f ",(DOUBLE)rs[vc->rownum+j]);
    printf("\n");
  }
}

  /* extract stuff from hash table */
  sp_hash_end(&S,S.A_rows,S.total_rows,A_OFF);

  /* solve system */
  memcpy((char*)rs_save,(char*)rs,sizeof(REAL)*S.A_rows);
  (*sp_AIJ_setup_func)(S.A_rows,&S);
  (*sp_constraint_setup_func)
            (web.skel[BODY].max_ord+1 + gen_quant_count,&S);
  if ( sp_ordering_func ) (*sp_ordering_func)(&S);
  sp_factor(&S);
  (*sp_hess_project_setup_func)(&S);
  sp_hessian_solve(&S,rs,rstmp,NO_SET_PRESSURE);
  for ( j = 0 ; j < S.N ; j++ ) rs[j] = rstmp[j];

if ( hess_debug )
{ printf("Motion: \n");
  for ( n = 0, vc = vhead ; n < vhead_count ; n++, vc++ )
  { if ( vc->freedom == 0 ) continue;
     printf("n: %d  ",n);
     for ( j = 0 ; j < SDIM ; j++ ) printf ("%f ",(DOUBLE)rs[vc->rownum+j]);
     printf("\n");
  }
  printf("Dot with gradient: %f\n",(DOUBLE)dot(rs,rs_save,S.A_rows));
}
  /* move vertices */
  if ( seekmode == 0 ) { best_scale = 1.0; }
  else
  { REAL energies[3]; 
    REAL denom;
    int k;

     save_coords(&saved,SAVE_IN_ATTR);  /* in case this doesn't work */
     energies[0] = web.total_energy;
     for ( scale = 1.0, k = 1 ; k < 3 ; scale += 1.0, k++ ) 
     { FOR_ALL_VERTICES(v_id)
       { REAL *coord;
         vc = get_vertex_vhead(v_id);
         if ( vc->freedom == 0 ) continue;
         coord = get_coord(v_id);
         for ( j = 0 ; j < SDIM ; j++ )
             coord[j] = coord[j] - scale*rs[vc->rownum+j];
       }
       global_timestamp++;
       calc_energy();  /* energy after motion */
       if ( hess_debug || itdebug )
#ifdef LONGDOUBLE
          printf("scale %f energy %*.*Lf\n",(DOUBLE)scale,DWIDTH,DPREC,web.total_energy);
#else
          printf("scale %f energy %20.15f\n",(DOUBLE)scale,web.total_energy);
#endif 
         energies[k] = web.total_energy;
        restore_coords(&saved,SAVE_IN_ATTR);
     }
     denom = energies[0] - 2*energies[1] + energies[2];
     if ( denom <= 0.0 ) 
        best_scale = 1.0;  /* energy not convex */
     else best_scale = 0.5*(3*energies[0]-4*energies[1]+energies[2])/denom;
  }
  scale = best_scale;
  FOR_ALL_VERTICES(v_id)
  { REAL *coord;
    vc = get_vertex_vhead(v_id);
    if ( vc->freedom == 0 ) continue;
    coord = get_coord(v_id);
    for ( j = 0 ; j < SDIM ; j++ )
      coord[j] = coord[j] - scale*rs[vc->rownum+j];
  }

  temp_free((char*)rs);
  temp_free((char*)rs_save);
  temp_free((char*)rstmp);
  free_system(&S);
  hessian_exit(NULL);

  return best_scale;
}

/***************************************************************************

    Lanczos routines, for solving hessian for motion or eigenvectors
    without factoring.

***************************************************************************/

/********************************************************************
*
* function: householder()
*
* purpose: perform Householder reduction.
*          returns b so (I-bvv^T)x = 0 mostly 
*/

REAL householder ARGS((REAL*,REAL*,int));

REAL householder(x,v,n)  
REAL *x;  /* vector to be zeroed, except first element */
REAL *v;  /* vector used to generate transformation */
int  n;    /* size of x,v */
{ REAL maxx;
  REAL a = 0.0,b;
  int i;

  for ( maxx = 0.0, i = 0 ; i < n ; i++ )
     if ( fabs(x[i]) > maxx ) maxx = fabs(x[i]);
  for ( i = 0 ; i < n ; i++ )
  { v[i] = x[i]/maxx;
    a += v[i]*v[i];
  }
  a = sqrt(a);
  b = 1/a/(a+fabs(v[0]));
  v[0] += v[0]<0.0 ? -a : a;
  return b;
}

/******************************************************************
*
* function: sturm()
*
* purpose:  Sturm sequence for number of eigenvalues below x 
*           for tridiagonal matrix 
*/

int sturm(n,alpha,beta,x) 
int n; /* size of matrix */
REAL *alpha; /* main diagonal, size n */
REAL *beta;  /* subdiagonal, size n-1 */
REAL x;     /* test eigenvalue */
{ int i;
  int changes;
  REAL p,pp,t;

  /* Sturm sequence for number of eigenvalues below x */ 
  pp = 1.0;
  p = alpha[0] - x;
  changes = p < 0.0 ? 1 : 0;
  for ( i = 1 ; i < n ; i++ )
  { t = (alpha[i] - x)*p - beta[i-1]*beta[i-1]*pp;
     if ( t*p < 0.0 ) changes++;
     pp = p; p = t;
  }
  return changes;
}

/*****************************************************************
*
*  function: ev_bisect()
*
*  purpose: returns smallest eigenvalue in interval 
*
*/

REAL ev_bisect(left,right,cleft,cright,n,alpha,beta)
REAL left;  /* left endpoint of interval */
REAL right; /* right endpoint of interval */
int cleft;  /* sturm changes at left endpoint of interval */
int cright; /* sturm changes at right endpoint of interval */
int n; /* size of matrix */
REAL *alpha; /* main diagonal, size n */
REAL *beta;  /* subdiagonal, size n-1 */
{ int cmid;
  REAL mid;
  REAL lowev = 0.0;

  mid = (left+right)/2;
  if ( right-left < 3*machine_eps )
  { 
#ifdef LONGDOUBLE
     sprintf(msg,"%d at %2.*Lg\n",DPREC,cright-cleft,mid);
#else
     sprintf(msg,"%d at %2.15g\n",cright-cleft,mid);
#endif 
     outstring(msg);
     return mid;
  }
  cmid = sturm(n,alpha,beta,mid);
  if ( cmid < cright )
     lowev = ev_bisect(mid,right,cmid,cright,n,alpha,beta);
  if ( cleft < cmid )
     lowev = ev_bisect(left,mid,cleft,cmid,n,alpha,beta);
  return lowev; 
}

/**************************************************************************
*
* function: chebychev_ev()
*
* purpose: use Chebychev iteration to solve for eigenvalue and 
* eigenvector.
*
*/

void chebychev_ev(S)
struct linsys *S;
{
  REAL *v,*w;    /* working vectors  */
  REAL *r;
  REAL t;      /* temp */
  int i,k;
  REAL maxev; /* eigenvalue upper bound */
  REAL minev; /* eigenvalue lower bound */
  REAL nextev;
  REAL coeffA,coeffc;
  REAL rayleigh=1.0;  /* Rayleigh quotient */
  int maxk;
  char response[20];
  REAL *X;

  v = (REAL *)temp_calloc(S->N+S->concount+1,sizeof(REAL));
  w = (REAL *)temp_calloc(S->N+S->concount+1,sizeof(REAL));
  r = (REAL *)temp_calloc(S->N+S->concount+1,sizeof(REAL));
  X = (REAL *)temp_calloc(S->N+S->concount+1,sizeof(REAL));

  /* first, need to find upper bound on eigenvalues */
  maxev = 1.0;
  nextev = 0.0;
  minev = 0.0;
  do
  {
     prompt("Max iterations: ",response,sizeof(response)); 
     maxk = atoi(response);
     do
     {
       coeffA = 2/(maxev-nextev);
       coeffc = -2*nextev/(maxev-nextev) - 1;
       if ( minev >= 0.0 )
          for ( i = 0 ; i < S->N ; i++ ) w[i] = drand48() - .5;
       (*sp_mul_func)(S,w,v);
       for ( i = 0 ; i < S->N ; i++ )
          r[i] = coeffA*v[i] + coeffc*w[i];
       coeffA *= 2; coeffc *= 2;
       for ( k = 0 ; k < maxk ; k++ )
       { REAL *tmp;
         REAL norm;
         tmp = r; r = w; w = tmp;
         (*sp_mul_func)(S,w,v);
         for ( i = 0 ; i < S->N ; i++ )
           r[i] = coeffA*v[i] + coeffc*w[i] - r[i];
         /* periodically check Rayleigh quotient to see if bound needs raising */
         norm = dot(w,w,S->N);
         rayleigh = dot(w,v,S->N)/norm;
         if ( rayleigh > maxev ) 
         { maxev = 1.2*rayleigh + .5; break; }
         /* just some checking */
         if ( (k % 50)==0 )
           printf("%4d.  rayleigh %2.15g  maxev %g\n",k,(DOUBLE)rayleigh,(DOUBLE)maxev);
         /* renormalize occasionally */
         if ( (norm > 25.0) || (norm < .04) )
         { t = 1/sqrt(norm);
           for ( i = 0 ; i < S->N ; i++ )
              { w[i] *= t; r[i] *= t; }
         }
       }
       if ( rayleigh < minev-.00000001 ) 
       { minev = rayleigh; nextev = 0.99*rayleigh; }
     } while ( k < maxk );
  } while ( k > 0 );

  printf("%4d.  rayleigh %2.15g\n",k,(DOUBLE)rayleigh);
  last_eigenvalue = rayleigh;

  /* else try again with increased maxev */

  /* now have got eigenvector for neg eigenvalue */
  t = 1/sqrt(dot(r,r,S->N));
  for ( i = 0 ; i < S->N ; i++ ) X[i] = t*r[i];    /* normalized */

  temp_free((char*)w);
  temp_free((char*)v);
  temp_free((char*)r);
  temp_free((char*)X);
} /* end chebychev_ev() */


/**************************************************************************
*
* function: chebychev_hess()
*
* purpose: use Chebychev iteration to solve for Hessian minimum.
*  Requires semidefinite hessian.
*
*/

void chebychev_hess(S)
struct linsys *S;
{
  REAL *v,*w;    /* working vectors  */
  REAL *r;
  REAL t;      /* temp */
  int i,k;
  REAL maxev; /* eigenvalue upper bound */
  REAL minev; /* eigenvalue lower bound */
  REAL nextev;
  REAL coeffA,coeffc;
  REAL rayleigh=0.0;  /* Rayleigh quotient */
  REAL *X;

  v = (REAL *)temp_calloc(S->N+1,sizeof(REAL));
  w = (REAL *)temp_calloc(S->N+1,sizeof(REAL));
  r = (REAL *)temp_calloc(S->N+1,sizeof(REAL));
  X = (REAL *)temp_calloc(S->N+1,sizeof(REAL));

  /* first, need to find upper bound on eigenvalues */
  maxev = 1.0;
  nextev = 0.0;
  minev = 0.0;
  do
  {
     coeffA = 2/(maxev-nextev);
     coeffc = -2*nextev/(maxev-nextev) - 1;
     if ( minev >= 0.0 )
        for ( i = 0 ; i < S->N ; i++ ) w[i] = drand48() - .5;
     (*sp_mul_func)(S,w,v);
     for ( i = 0 ; i < S->N ; i++ )
        r[i] = coeffA*v[i] + coeffc*w[i];
     coeffA *= 2; coeffc *= 2;
     for ( k = 0 ; k < 200 ; k++ )
     { REAL *tmp;
        REAL norm;
        tmp = r; r = w; w = tmp;
        (*sp_mul_func)(S,w,v);
        for ( i = 0 ; i < S->N ; i++ )
          r[i] = coeffA*v[i] + coeffc*w[i] - r[i];
        /* periodically check Rayleigh quotient to see if bound needs raising */
        norm = dot(w,w,S->N);
        rayleigh = dot(w,v,S->N)/norm;
        if ( rayleigh > maxev ) 
        { maxev = rayleigh + .5; break; }
        /* just some checking */
        if ( (k % 50)==0 )
          printf("%4d.  rayleigh %2.15g  maxev %g\n",k,(DOUBLE)rayleigh,(DOUBLE)maxev);
        /* renormalize occasionally */
        if ( (k&0xFFF0) == 0 )
        { t = 1/sqrt(norm);
          for ( i = 0 ; i < S->N ; i++ )
              { w[i] *= t; r[i] *= t; }
        }
     }
     if ( rayleigh < minev-.00000001 ) 
     { minev = rayleigh; nextev = 0.9*rayleigh; k = 0; continue; }
  } while ( k < 200 );
  printf("%4d.  rayleigh %2.15g\n",k,(DOUBLE)rayleigh);
  /* else try again with increased maxev */

  /* now have got eigenvector for neg eigenvalue */
  t = 1/sqrt(dot(r,r,S->N));
  for ( i = 0 ; i < S->N ; i++ ) X[i] = t*r[i];    /* normalized */

  temp_free((char*)w);
  temp_free((char*)v);
  temp_free((char*)r);
  temp_free((char*)X);
} /* end chebychev_hess() */


/****************************************************************************
*
* function: eigenprobe_command()
*
* purpose: handle "eigenprobe lambda" command from command line
*
*/

void eigenprobe_command(lambda,iters)
REAL lambda;
int iters;  /* iteration bound */
{ struct linsys S;
  int i,k;
  REAL t,oldt;
  REAL *W = NULL; /* for use when metric is needed */
  REAL *X = NULL; /* for eigenvector */
  hmode = hessian_normal_flag;
  hessian_init(&S,NULL);
  hess_flag = 1; rhs_flag = 0; 
  hessian_fill(&S,NULL); 
  S.lambda = lambda;
   

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

 sp_factor(&S);

 (*sp_hess_project_setup_func)(&S);

  sprintf(msg,"Eigencounts:    %d <,  %d ==,  %d > \n",S.neg,S.zero,S.pos);
  outstring(msg);
  
  if ( iters <= 0 ) goto ep_exit;

  if ( web.area_norm_flag || hessian_linear_metric_flag )
     W = (REAL*)temp_calloc(S.N,sizeof(REAL));
  X = (REAL*)temp_calloc(S.N,sizeof(REAL));
  /* inverse iteration */
  /* random starting vector */
  for ( i = 0 ; i < S.N ; i++ ) X[i] = drand48();
  t = 1/sqrt(dot(X,X,S.N));
  for ( i = 0 ; i < S.N ; i++ ) X[i] *= t;
  oldt = 1e30; /* for convergence test */
  for ( k = 0 ; k < iters ; k++ )
  { REAL oldv0=1.0; /* for sign */
     int oldvi=0;     /* index of nonzero component */
     REAL eps = 1/sqrt((REAL)(S.N))/2;
     for ( i = 0, oldv0 = 0.0 ; i < S.N ; i++ )
        if ( fabs(X[i]) > eps ) { oldvi = i; oldv0 = X[i]; break; }
     if ( web.area_norm_flag || hessian_linear_metric_flag )
         (*sp_mul_func)(&Met,X,W);
     else W = X;
     sp_hessian_solve(&S,W,X,NO_SET_PRESSURE);
     if ( web.area_norm_flag || hessian_linear_metric_flag )
        t = 1/sqrt(sparse_metric_dot(X,X,&Met));
     else t = 1/sqrt(dot(X,X,S.N));
     if ( X[oldvi]*oldv0 < 0. ) t = -t;  /* get sign right */
#ifdef LONGDOUBLE
     if ( k % 10 == 0 ) printf("%d  ev = %*.*Lf\n",k,DWIDTH,DPREC,S.lambda+t);
#else
     if ( k % 10 == 0 ) printf("%d  ev = %20.15f\n",k,S.lambda+t);
#endif 
     for ( i = 0 ; i < S.N ; i++ ) X[i] *= t;
     if ( fabs(t-oldt) <= 100*machine_eps*fabs(t) )
      { 
         break;
      }
     oldt = t;
  }
#ifdef LONGDOUBLE
  printf("%d  ev = %*.*Lf\n",k,DWIDTH,DPREC,S.lambda+t);
#else
  printf("%d  ev = %20.15f\n",k,S.lambda+t);
#endif 
  if ( S.zero != 0 ) last_eigenvalue = S.lambda;
  else
  last_eigenvalue = S.lambda+t;
  if ( web.area_norm_flag || hessian_linear_metric_flag )
      temp_free((char*)W);
  
ep_exit:
  free_system(&S);
  if ( hessian_linear_metric_flag || web.area_norm_flag ) free_system(&Met);
  hessian_exit(X);
  temp_free((char*)X);
} /* end eigenprobe_command() */

/****************************************************************************
*
* function: lanczos_command()
*
* purpose: handle "lanczos lambda" command from command line
*
*/
void lanczos_command(lambda,krydim)
REAL lambda;
int krydim;  /* dimension of Krylov subspace */
{ struct linsys S;
  REAL evalues[NPRINT];
  int i,nprint;
  
  hmode = hessian_normal_flag;
  hessian_init(&S,NULL);
  hess_flag = 1; rhs_flag = 0; 
  hessian_fill(&S,NULL); 
  S.lambda = lambda;
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
  sp_factor(&S);
  (*sp_hess_project_setup_func)(&S);
  sprintf(msg,"Eigencounts:    %d <,  %d ==,  %d > \n",S.neg,S.zero,S.pos);
  outstring(msg);
  nprint = lanczos(&S,krydim,evalues,NPRINT);
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
  free_system(&S);
  if ( hessian_linear_metric_flag || web.area_norm_flag ) free_system(&Met);
  hessian_exit(NULL);
} /* end lanczos_command() */

/*****************************************************************************
*
* function: lanczos()
*
* purpose: find eigenvalues near probe value
*             returns number distinct ones found
*
*/

int realabs_comp ARGS((REAL*,REAL*));

int realabs_comp(a,b)
REAL *a,*b;
{if(fabs(*a)<fabs(*b))return-1;else if (fabs(*a)>fabs(*b))return 1;return 0;}

int lanczos(S,krydim,evalues,nprint)
struct linsys *S;
int krydim;  /* dimension of krylov space to use */
REAL *evalues; /* for return of eigenvalues */
int nprint;  /* number to return */
{ REAL *diag;  /* main diagonal of tridiagonal */
  REAL *subdiag;    /* subdiagonal */
  REAL *v,*w,*mw;    /* working vectors; w is Lanczos vector  */
  REAL *r;
  REAL t;      /* temp */
  int i,j;

  if ( krydim > S->N-S->CN ) krydim = S->N-S->CN;
  if ( nprint > krydim ) nprint = krydim;

  diag = (REAL *)temp_calloc(krydim+1,sizeof(REAL));
  subdiag = (REAL *)temp_calloc(krydim+1,sizeof(REAL));
  v = (REAL *)temp_calloc(S->N+S->concount+1,sizeof(REAL));
  w = (REAL *)temp_calloc(S->N+S->concount+1,sizeof(REAL));
  r = (REAL *)temp_calloc(S->N+S->concount+1,sizeof(REAL));
  if ( web.area_norm_flag || hessian_linear_metric_flag ) 
     mw = (REAL *)temp_calloc(S->N+S->concount+1,sizeof(REAL));
  else mw = NULL;

  /* Lanczos iteration to tridiagonal, starting at index 1 in diag and subdiag */
  /* not doing any re-orthogonalization, so multiple eigenvalues can
      let large eigenvalues creep in spuriously multiple */


  for ( i = 0 ; i < S->N ; i++ ) w[i] = drand48() - .5;
  if ( web.area_norm_flag || hessian_linear_metric_flag ) 
      t = 1/sqrt(sparse_metric_dot(w,w,&Met));
  else t = 1/sqrt(dot(w,w,S->N));
  for ( i = 0 ; i < S->N ; i++ ) w[i] *= t;
  subdiag[0] = 1.0;
  for ( j = 0 ; j < krydim ;  )
  { if ( subdiag[j] == 0.0 )
        { outstring("premature subdiag = 0\n"); break; }
     if ( j > 0 ) 
     { for ( i = 0 ; i < S->N ; i++ )
        { t = w[i]; w[i] = v[i]/subdiag[j]; v[i] = -subdiag[j]*t; }
     }
     
     /* v = Aw + v */
     if ( web.area_norm_flag || hessian_linear_metric_flag ) 
     { bk_mul(&Met,w,mw);
        sp_hessian_solve(S,mw,r,NO_SET_PRESSURE);
     }
     else sp_hessian_solve(S,w,r,NO_SET_PRESSURE);
     for ( i = 0 ; i < S->N ; i++ ) v[i] += r[i];

     j = j + 1;
     if ( web.area_norm_flag || hessian_linear_metric_flag ) 
         diag[j] = sparse_metric_dot(w,v,&Met);
     else diag[j] = dot(w,v,S->N);
     for ( i = 0 ; i < S->N ; i++ ) v[i] -= diag[j]*w[i];
     if ( web.area_norm_flag || hessian_linear_metric_flag ) 
         subdiag[j] = sqrt(sparse_metric_dot(v,v,&Met));
     else subdiag[j] = sqrt(dot(v,v,S->N));
  }

  /* solve tridiagonal */
  tridiag_QL(diag+1,subdiag+1,krydim);

  /* sort eigenvalues */
  qsort((char*)(diag+1),krydim,sizeof(REAL),FCAST realabs_comp);

  /* unshift */
  for ( i = 1 ; i <= krydim ; i++ )
     if ( diag[i] == 0.0 ) diag[i] = S->lambda;
     else diag[i] = S->lambda + 1/diag[i];

  /* uniqify */
  for ( i = 2, j = 1; i <= krydim ; i++ )
  { if ( fabs(diag[j] - diag[i]) < 100*machine_eps ) continue;
     diag[++j] = diag[i];
  }
  krydim = j;
  if ( nprint > krydim ) nprint = krydim;

  /* list, ones near probe value */
  for ( i = 0 ; i < nprint ; i++ ) evalues[i] = diag[krydim-i];

  temp_free((char*)diag);
  temp_free((char*)subdiag);
  temp_free((char*)v);
  temp_free((char*)w);
  temp_free((char*)r);
  if ( mw ) temp_free((char*)mw);

  return nprint;
} /* end lanczos() */

/*****************************************************************************
*
* function: selective_lanczos()
*
* purpose: find eigenvalues near probe value using selective 
*          reorthogonalization, i.e. saves basis vectors for which Av
*          grows significantly.  Does not do uniqifying of eigenvalues.
*          returns number found
*
*/

int selective_lanczos(S,krydim,evalues,nprint)
struct linsys *S;
int krydim;  /* dimension of krylov space to use */
REAL *evalues; /* for return of eigenvalues */
int nprint;  /* number to return */
{ REAL *diag;  /* main diagonal of tridiagonal */
  REAL *subdiag;    /* subdiagonal */
  REAL *v,*w,*mw;    /* working vectors; w is Lanczos vector  */
  REAL *r;
  REAL t;      /* temp */
  int i,j,k;
  REAL **basis; /* for saving basis vectors for reortho */
  REAL *rayleigh; /* rayleigh quotients associated to basis vectors */
  int basis_count = 0;  /* how many basis vectors saved */
  REAL growth;

  if ( krydim > S->N ) krydim = S->N;
  if ( nprint > krydim ) nprint = krydim;

  diag = (REAL *)temp_calloc(krydim+1,sizeof(REAL));
  subdiag = (REAL *)temp_calloc(krydim+1,sizeof(REAL));
  v = (REAL *)temp_calloc(S->N+S->concount+1,sizeof(REAL));
  w = (REAL *)temp_calloc(S->N+S->concount+1,sizeof(REAL));
  r = (REAL *)temp_calloc(S->N+S->concount+1,sizeof(REAL));
  if ( web.area_norm_flag || hessian_linear_metric_flag ) 
      mw = (REAL *)temp_calloc(S->N+S->concount+1,sizeof(REAL));
  else mw = NULL;
  basis = dmatrix(0,nprint-1,0,S->N-1);
  rayleigh = (REAL*)temp_calloc(nprint,sizeof(REAL));

  /* Lanczos iteration to tridiagonal, starting at index 1 in diag and subdiag */

  for ( i = 0 ; i < S->N ; i++ ) w[i] = drand48() - .5;
  if ( web.area_norm_flag || hessian_linear_metric_flag ) 
      t = 1/sqrt(sparse_metric_dot(w,w,&Met));
  else t = 1/sqrt(dot(w,w,S->N));
  for ( i = 0 ; i < S->N ; i++ ) w[i] *= t;
  subdiag[0] = 1.0;
  for ( j = 0 ; j < krydim ;  )
  { if ( subdiag[j] == 0.0 )
        { outstring("premature subdiag = 0\n"); break; }
     if ( j > 0 ) 
     { for ( i = 0 ; i < S->N ; i++ )
        { t = w[i]; w[i] = v[i]/subdiag[j]; v[i] = -subdiag[j]*t; }
     }
     
     /* v = Aw + v */
     if ( web.area_norm_flag || hessian_linear_metric_flag ) 
     { bk_mul(&Met,w,mw);
        sp_hessian_solve(S,mw,r,NO_SET_PRESSURE);
     }
     else sp_hessian_solve(S,w,r,NO_SET_PRESSURE);
     for ( i = 0 ; i < S->N ; i++ ) v[i] += r[i];

     j = j + 1;
     if ( web.area_norm_flag || hessian_linear_metric_flag ) 
        diag[j] = sparse_metric_dot(w,v,&Met);
     else diag[j] = dot(w,v,S->N);
     for ( i = 0 ; i < S->N ; i++ ) v[i] -= diag[j]*w[i];
     /* reorthogonalize */
     for ( k = 0 ; k < basis_count ; k++ )
     { REAL d;
        REAL *p = basis[k];
        if ( web.area_norm_flag || hessian_linear_metric_flag ) 
            d = sparse_metric_dot(basis[k],v,&Met);
        else d = dot(basis[k],v,S->N);
        for ( i = 0 ; i < S->N ; i++,p++ ) v[i] -= d*(*p);
     }
     if ( web.area_norm_flag || hessian_linear_metric_flag ) 
         subdiag[j] = sqrt(sparse_metric_dot(v,v,&Met));
     else subdiag[j] = sqrt(dot(v,v,S->N));

     /* maybe save this basis vector for orthogonalization */
     growth = 
        sqrt(subdiag[j]*subdiag[j]+diag[j]*diag[j]+subdiag[j-1]*subdiag[j-1]);
     if ( growth > 1.0 )
     { /* save */
        int which = -1; /* which to replace, with non-replace initial value */
        if ( basis_count < nprint ) which = basis_count++;
        else  /* maybe replace smaller growth vector */
        { REAL diff = 0.0;
          for ( i = 0 ; i < basis_count ; i++ )
             if ( growth - rayleigh[i] > diff ) 
                { diff = growth - rayleigh[i]; which = i; }
        }
        if ( which >= 0 )
        { rayleigh[which] = growth;
          memcpy((char*)(basis[which]),(char*)w,S->N*sizeof(REAL));
        }
     }
  }

  /* solve tridiagonal */
  tridiag_QL(diag+1,subdiag+1,krydim);

  /* sort eigenvalues */
  qsort((char*)(diag+1),krydim,sizeof(REAL),FCAST realabs_comp);

  /* unshift */
  for ( i = 1 ; i <= krydim ; i++ )
  diag[i] = S->lambda + 1/diag[i];

  /* list, ones near probe value */
  for ( i = 0 ; i < nprint ; i++ ) evalues[i] = diag[krydim-i];

  temp_free((char*)diag);
  temp_free((char*)subdiag);
  temp_free((char*)v);
  temp_free((char*)w);
  if ( mw ) temp_free((char*)mw);
  temp_free((char*)r);
  temp_free((char*)rayleigh);
  free_matrix(basis);
  return nprint;
} /* end selective_lanczos() */


/***************************************************************************
* 
* function: tridiag_QL()
*
* purpose: QL algorithm with implicit shifts to determine eigenvalues
*             of a tridiagonal matrix.  From Numerical Recipes in C.
*/

#define SIGN(a,b)  ((b)<0.0 ? -fabs(a) : fabs(a))

void tridiag_QL(d,e,n)
REAL *d;  /* n diagonal elements, d[0] in a[0][0] */
REAL *e;  /* n-1 subdiagonal, e[0] in a[1][0], plus an extra! */
int n;  /* size */
{ int m,l,iter,i;
  REAL s,r,p,g,f,dd,c,b;

  d--;e--;  /* to agree with Numerical Recipes indexing */

  e[n] = 0.0;
  for ( l = 1 ;l <= n ; l++ )
  { iter = 0;
     do 
     { for ( m=l ; m <= n-1 ; m++ )    /* test matrix splitting */
        { dd = fabs(d[m]) + fabs(d[m+1]);
          if ( fabs(e[m]) + dd == dd ) break;
        }
        if ( m != l )
        { if ( iter++ >= 30 ) 
             kb_error(1654,"Over 30 iterations in QL algorithm solving for eigenvalues.\n",RECOVERABLE); 
          g = (d[l+1]-d[l])/(2*e[l]);    /* form shift */
          r = sqrt(g*g+1.0);
          g = d[m] - d[l] + e[l]/(g + SIGN(r,g));
          s = c = 1.0;
          p = 0.0;
          for ( i = m-1 ; i >= l ; i-- )
          { f = s*e[i];
             b = c*e[i];
             if ( fabs(f) >= fabs(g) ) 
             { c = g/f;
                r = sqrt(c*c + 1.0);
                e[i+1] = f*r;
                c *= (s = 1/r);
             }
             else
             { s = f/g;
                r = sqrt(s*s + 1.0);
                e[i+1] = g*r;
                s *= (c = 1/r);
             }
             g = d[i+1] - p;
             r = (d[i] - g)*s + 2*c*b;
             p = s*r;
             d[i+1] = g + p;
             g = c*r - b;
          }
          d[l] -= p;
          e[l] = g;
          e[m] = 0.0;
        }
     } while ( m != l );
  }
} /* end tridiagonal_QL () */

/****************************************************************************
*
* function: LQ_decomp()
*
* purpose: orthonormalize rows of a rectangular matrix A by Gram-Schmidt and
*             return factorizion into A = LQ where L is lower
*             triangular and Q is orthonormal, Q M Q^T = I.
*/
void  LQ_decomp(A,rows,cols,Q,L,M)
REAL **A; /* input */
int rows; /* number of rows in A */
int cols; /* number of columns in A */
REAL **Q; /* orthonormal rows, size rows * cols */
REAL **L; /* factor, size rows * rows */
struct linsys *M;  /* metric to use; NULL if Euclidean */
{ int i,j,k;
  REAL t;

  for ( i = 0 ; i < rows ; i++ )
  { if ( Q != A )/* copy over */
      for ( k = 0 ; k < cols ; k++ ) 
        Q[i][k] = A[i][k];
     for ( j = 0 ; j < i ; j++ )
     { t = M ? sparse_metric_dot(Q[j],Q[i],M) :  dot(Q[j],Q[i],cols);
        L[i][j] = t;
        for ( k = 0 ; k < cols ; k++ )
          Q[i][k] -= t*Q[j][k];
     }
     t = M ? sparse_metric_dot(Q[i],Q[i],M) :  dot(Q[i],Q[i],cols);
     if ( t != 0.0 )
     { L[i][i] = t = sqrt(t);
        t = 1/t;
        for ( k = 0 ; k < cols ; k++ )
          Q[i][k] *= t;
     }
  }
} /* end LQ_decomp() */

/****************************************************************************
*
* function: QR_full()
*
* purpose:  QR algorithm for eigenvalues of dense symmetric matrix
*              Not too good, since doesn't shift.  Has problem with
*              multiple eigenvalues.
*
*/
void QR_full ARGS((REAL **,REAL *,int, struct linsys *));

void QR_full(A,evalues,n,M)
REAL **A; /* input, destroyed */
REAL *evalues; /* for reporting eigenvalues */
int n; /* size of system */
struct linsys *M; /* metric, NULL for Euclidean */
{ REAL **Q,**L;
  int i,j;
  REAL resid;
  int count = 0;
 
  Q = dmatrix(0,n-1,0,n-1);
  L = dmatrix(0,n-1,0,n-1);
  do 
  { LQ_decomp(A,n,n,Q,L,M);
     mat_mult(Q,L,A,n,n,n);
     for ( i = 0, resid = 0.0 ; i < n ; i++ )
      for ( j = 0 ; j < i ; j++ )
         resid += fabs(A[i][j]);
#ifdef LONGDOUBLE
     sprintf(msg,"QR residual %*.*Lf\n",DWIDTH,DPREC,resid);
#else
     sprintf(msg,"QR residual %20.15f\n",resid);
#endif 
     outstring (msg);
  } while ( (resid > 1000*machine_eps) && (count++ < 20) );
  for ( i = 0 ; i < n ; i++ ) evalues[i] = A[i][i];
} /* end QR_full() */

/****************************************************************************
*
* function: ritz_command()
*
* purpose: handle "ritz(lambda,eigencount)" command from command line
*
*/

void ritz_command(lambda,ritzdim)
REAL lambda; /* probe value */
int ritzdim;  /* dimension of Rayleigh-Ritz subspace */
{ struct linsys S;
  REAL *rhs;
  memset((char*)&S,0,sizeof(struct linsys)); 
  
  hmode = hessian_normal_flag;
  hessian_init(&S,&rhs);
  hess_flag = 1; rhs_flag = 0; 
  hessian_fill(&S,&rhs); 
  if ( sparse_constraints_flag )
    (*sp_AIJ_setup_func)(S.total_rows,&S);
  else
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
  do_ritz(&S,lambda,ritzdim,NULL);

  free_system(&S);
  if ( hessian_linear_metric_flag || web.area_norm_flag ) free_system(&Met);
  temp_free((char*)rhs);
  hessian_exit(NULL);
} /* end ritz_command() */


/**************************************************************************
*
*  function: do_ritz()
*
*  purpose: Do Ritz subspace iteration 
*              Hessian already set up.
*  Input: If ritzdim over 1000, ritzdim/1000 is the number of extra
*         vectors to include in the Ritz subspace, in hopes that
*         including a few extra will find an eigenvalue gap big enough
*         to speed convergence.
*/
  REAL *evalues;

void do_ritz(S,lambda,ritzdim,ritzvec)
struct linsys *S;  /* Hessian */
REAL lambda;    /* shift value */
int ritzdim;     /* dimension of subspace */
REAL **ritzvec; /* non-null if want eigenvectors returned */
{
  int i,j;
  REAL **Lmat,**rbasis,**Arbasis;
  REAL resid;
  REAL **A;
  REAL *oldevalues;
  REAL **evectors;
  int converged; /* number of eigenvectors converged */
  int iter;
  REAL diff = 1e30,olddiff=1e30;
  REAL **mw;
  struct linsys *M; /* metric to use */
  REAL *work;
  int ritzwanted;
  int maxritz;
  int old_breakflag;

  /* see if user wants extra ritz vectors */
  ritzwanted = ritzdim % 1000;
  ritzdim    = (ritzdim/1000) +  ritzwanted;

  if ( ritzdim < 0 ) 
    { outstring("Ritz dimension cannot be negative.\n"); return; }
  S->lambda = lambda;
  sp_factor(S);
  (*sp_hess_project_setup_func)(S);
  sprintf(msg,"Eigencounts:    %d <,  %d ==,  %d > \n",S->neg,S->zero,S->pos);
  outstring(msg);

  if ( ritzdim == 0 ) return;

  maxritz = S->N - (augmented_hessian_mode ? 2 : 1)* S->CN; 
  if ( ritzdim > maxritz )
    ritzdim = maxritz;
  if ( ritzwanted > ritzdim ) ritzwanted = ritzdim;

  /* now find near-invariant subspace by power iteration and orthog */
  Lmat = dmatrix(0,ritzdim-1,0,ritzdim-1); /* for QR decomp */
  if ( ritzvec ) rbasis = ritzvec;
  else
      rbasis = dmatrix(0,ritzdim-1,0,S->N-1+S->concount);
  Arbasis = dmatrix(0,ritzdim-1,0,S->N-1+S->concount); /* multiplied basis */
  A = dmatrix(0,ritzdim-1,0,ritzdim-1);
  mw = dmatrix(0,ritzdim-1,0,S->N+S->concount);
  evalues = (REAL*)realloc(evalues,ritzdim*sizeof(REAL));

  oldevalues = (REAL*)temp_calloc(ritzdim,sizeof(REAL));
  work = (REAL*)temp_calloc(2*S->N,sizeof(REAL));
  for ( i = 0 ; i < ritzdim ; i++ ) oldevalues[i] = -1e18;  /* unlikely value */ 

  evectors = dmatrix(0,ritzdim-1,0,ritzdim-1);
  /* for augmented hessian, want augmented part of rhs 0 always */
  for ( i = 0 ; i < ritzdim ; i++ )
     for ( j = 0 ; j < S->A_rows ; j++ ) rbasis[i][j] = drand48() - .5;
  iter = 0;
  converged = 0;
  iterate_flag = 1; /* for politer interrupt message */
  if ( web.area_norm_flag || hessian_linear_metric_flag ) M =  &Met;
  else M = NULL;
  old_breakflag = breakflag;
  do
  {
    if ( web.area_norm_flag || hessian_linear_metric_flag )
    { for ( i = converged ; i < ritzdim ; i++ )
        bk_mul(&Met,rbasis[i],mw[i]);
      sp_hessian_solve_multi(S,mw+converged,Arbasis+converged,
         ritzdim-converged);
    }
    else sp_hessian_solve_multi(S,rbasis+converged,Arbasis+converged,
          ritzdim-converged);
    if  ( augmented_hessian_mode )
    { /* set augmented part to 0 */
      for ( i = converged ; i < ritzdim ; i++ )
        for ( j = S->A_rows ; j < S->N ; j++ )
          Arbasis[i][j] = 0.0;
    }
    LQ_decomp(Arbasis,ritzdim,S->A_rows,rbasis,Lmat,M);
    iter ++;
    /* add up off-diagonal residuals */
    for ( i = 0, resid = 0.0 ; i < ritzdim ; i++ )
      for ( j = 0 ; j < i ; j++ )
        resid += fabs(Lmat[i][j]);

    /* get matrix in Rayleigh-Ritz subspace */
    for ( i = converged ; i < ritzdim ; i++ )
    { if ( web.area_norm_flag || hessian_linear_metric_flag )
      { bk_mul(&Met,rbasis[i],mw[i]);
        memcpy((char*)(rbasis[i]),(char*)(mw[i]),Met.N*sizeof(REAL));
      }
    }
    sp_hessian_solve_multi(S,rbasis+converged,Arbasis+converged,
              ritzdim-converged);
    if ( augmented_hessian_flag )
    { /* set augmented part to 0 */
      for ( i = converged ; i < ritzdim ; i++ )
        for ( j = S->A_rows ; j < S->N ; j++ )
          Arbasis[i][j] = 0.0;
    }
    mat_mul_tr(rbasis+converged,Arbasis+converged,A,ritzdim-converged,
                      S->A_rows,ritzdim-converged);
    jacobi_eigenpairs(A,ritzdim-converged,evalues+converged,evectors+converged,work);
    msg[0] = 0 ;

    /* get eigenvector basis for subspace */
    tr_mat_mul(evectors+converged,Arbasis+converged,rbasis+converged,
         ritzdim-converged,ritzdim-converged,S->A_rows);

    for ( i = converged ; i < ritzdim ; i++ )
      if ( evalues[i] == oldevalues[i] )
      { converged++;
#ifdef LONGDOUBLE
        sprintf(msg,"%3d. %*.*Lf\n",converged,DWIDTH,DPREC,1/evalues[i]+lambda);
#else
        sprintf(msg,"%3d. %20.15f\n",converged,1/evalues[i]+lambda);
#endif 
        outstring(msg);
      }
      else break; 
    olddiff = diff;
    for ( diff = 0.0 ; i < ritzwanted ; i++ )
    { diff += fabs(evalues[i] - oldevalues[i]);
      oldevalues[i] = evalues[i];
    }

  } while ( (iter<1000) && ((diff < olddiff)||(diff > 10*S->N*machine_eps))
       && (diff > 100*machine_eps) && !breakflag );
  breakflag = old_breakflag;

  /* normalize eigenvectors */
  for ( i = 0 ; i < ritzdim ; i++ )
  { REAL mag;
     if ( M ) mag = 1/sqrt(sparse_metric_dot(rbasis[i],rbasis[i],M));
     else mag = 1/sqrt(dot(rbasis[i],rbasis[i],S->A_rows));
     for ( j = 0 ; j < S->A_rows ; j++ ) rbasis[i][j] *= mag;
  }

  /* list, ones near probe value */
  
  /* convert and insertion sort */
  for ( i = 0 ; i < ritzwanted ; i++ )
  { REAL eig;
     REAL *rtmp;
     if ( evalues[i] == 0.0 ) eig = 1e30;
     else eig = 1/evalues[i] + lambda;
     rtmp = rbasis[i];
     for ( j = i ; j > converged ; j-- )
     { if ( evalues[j-1] > eig ) 
        { evalues[j] = evalues[j-1]; rbasis[j] = rbasis[j-1]; }
        else break;
     }
     evalues[j] = eig;
     rbasis[j] = rtmp;
  }

  for ( i = converged ; i < ritzwanted ; i++ )
  { if ( evalues[i] > 1e29 ) sprintf(msg,"%3d. Singular??\n",i+1);
#ifdef LONGDOUBLE
     else sprintf(msg,"%3d. %*.*Lf\n",i+1,DWIDTH,DPREC,evalues[i]);
#else
     else sprintf(msg,"%3d. %18.13f\n",i+1,evalues[i]);
#endif 
     outstring(msg);
  }
  sprintf(msg,
     "Iterations: %d. Total eigenvalue changes in last iteration: %10.8g\n",
        iter,(DOUBLE)diff);
  outstring(msg);
  if ( S->zero != 0 ) last_eigenvalue = S->lambda;
  else
  last_eigenvalue = evalues[0];
  set_eigenvalue_list_global(evalues,ritzdim);

  free_matrix(Lmat);
  free_matrix(A);
  if ( ritzvec == NULL ) free_matrix(rbasis);
  free_matrix(Arbasis);
  free_matrix(evectors);
  temp_free((char*)oldevalues);
  temp_free((char*)work);
  free_matrix(mw);
} /* end do_ritz() */

