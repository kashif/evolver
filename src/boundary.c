/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/**************************************************************
*
*  File: boundary.c
*
*  Purpose: Handle boundary computations.
*/

#include "include.h"

/***************************************************************
*
*  Function: b_proj()
*
*  Purpose:  Construct projection matrices from R^n vectors
*                to parameter tangent space or to boundary tangent
*                space.
*
*  Mathematical background:
*      Let T[i] be the boundary tangent along parameter i, and
*  let V be the vector in R^n.  Let C be the linear combination
*  coefficients that give the projection of V:
*         (proj V)[j] = C[i]*T[i][j]        (sum on repeated indices)
*  The projection is defined by having identical dot products with
*  the tangents as does V:
*         C[i]*T[i][j]*T[k][j] = V[j]*T[k][j]  for all k.
*  Define the matrix A by A[i][k] = T[i][j]*T[k][j], and let B be
*  its inverse.  Note A and B are symmetric. So
*         A[k][i]*C[i] = T[k][j]*V[j]
*  and
*         C[i] = B[i][k]*T[k][j]*V[j].
*  So the coefficient projection matrix is D[i][j] = B[i][k]*T[k][j].
*  (which is returned for PARAMPROJ)
*  And
*        (proj V)[m] = C[i]*T[i][m] = T[i][m]*B[i][k]*T[k][j]*V[j]
*  so the tangent projection matrix is
*         E[m][j] = T[i][m]*B[i][k]*T[k][j].
*  (which is returned for TANJPROJ)
*/


void b_proj(bdry,param,a,type,v_id)
struct boundary *bdry;  /* boundary involved */
REAL *param;            /* parameter values to use */
REAL **a;               /* returned matrix */
int type;               /* PARAMPROJ or TANGPROJ */
vertex_id v_id;  /* so can use extra attributes */
{
  int pcount = bdry->pcount;
  int i,j,k,m;
  REAL dummy;  /* for eval_all function value */
  REAL temp[MAXCOORD];
  MAT2D(B,3,3); /* both A and B above */
  MAT2D(T,MAXCOORD,MAXCOORD);

  for ( j = 0 ; j < SDIM ; j++ )
  { eval_all(bdry->coordf[j],param,pcount,&dummy,temp,v_id);
    for ( i = 0 ; i < pcount ; i++ )
      T[i][j] = temp[i];
  }
  for ( i = 0 ; i < pcount ; i++ )
     for ( j = 0 ; j < pcount ; j++ )
        B[i][j] = SDIM_dot(T[i],T[j]);
  mat_inv(B,pcount);

  if ( type == PARAMPROJ )
  { for ( i = 0 ; i < pcount ; i++ )
      for ( j = 0 ; j < SDIM ; j++ )
      { a[i][j] = 0.0;
        for ( k = 0 ; k < pcount ; k++ )
           a[i][j] += B[i][k]*T[k][j];
      }
  }
  else if ( type == TANGPROJ )
  { for ( m = 0 ; m < SDIM ; m++ )
      for ( j = 0 ; j < SDIM ; j++ )
      { a[m][j] = 0.0;
        for ( i = 0 ; i < pcount ; i++ )
          for ( k = 0 ; k < pcount ; k++ )
            a[m][j] += T[i][m]*B[i][k]*T[k][j];
      }
  }
  else if ( type == GRADPROJ )
  { for ( i = 0 ; i < SDIM ; i++ )
      for ( j = 0 ; j < pcount ; j++ )
      { a[i][j] = 0.0;
        for ( k = 0 ; k < pcount ; k++ )
          a[i][j] += T[k][i]*B[k][j];
      }
  }
} /* end b_proj() */

/**********************************************************************
*
*  Function: b_extrapolate()
*
*  Purpose:  Find projection of point on boundary, starting at
*                given boundary location and projecting several
*                times on tangent.
*/

void b_extrapolate(bdry,base_x,point_x,new_x,base_param,new_param,v_id)
struct boundary *bdry;    /* boundary involved (in) */
REAL *base_x;      /* coordinates of base point (in) */
REAL *point_x;     /* coordinates of point to project (in) */
REAL *new_x;       /* projected coordinates (out) */
REAL *base_param;  /* base point parameters (in) */
REAL *new_param;   /* projected parameters (out) */
vertex_id v_id;    /* so can use vertex attributes */
{
  int pcount;
  REAL co[MAXCOORD];
  int i,k;
  REAL x[MAXCOORD],s[MAXCOORD];
  MAT2D(a,MAXPARAM,MAXCOORD);

  pcount = bdry->pcount;
  for ( k = 0 ; k < SDIM ; k++ )
  { x[k] = base_x[k];              /* start at base point */
    new_param[k] = base_param[k];
  }

#ifdef OLDWAY
  /* iterate projection on tangent till get close to desired point */
  for ( i = 0 ; i < 5 ; i++ )
  {
    for ( k = 0 ; k < SDIM ; k++ )
      s[k] = point_x[k] - x[k];
    b_proj(bdry,new_param,a,PARAMPROJ,v_id);
    matvec_mul(a,s,co,pcount,SDIM);
    for ( k = 0 ; k < pcount ; k++ )
      new_param[k] += co[k];
    for ( k = 0 ; k < SDIM ; k++ )
      x[k] = eval(bdry->coordf[k],new_param,NULLID,NULL);
  }
#else
  /* from Laszlo Csirmaz, being much more careful */
  for ( i=0; i<5; i++)
  { REAL lambda, oldsq,newsq,newx; REAL pp[MAXPARAM];
    int iter;
    lambda=1.0; oldsq=0.0;
    for(k=0;k<SDIM;k++){ s[k]=point_x[k]-x[k]; oldsq += s[k]*s[k]; }
    if( oldsq > 1e-10 )
    {
      b_proj(bdry,new_param,a,PARAMPROJ,v_id);
      matvec_mul(a,s,co,pcount,SDIM); /* the correcting vector is in co */
      for(iter=0;iter<10;iter++)
      {
        // the goal is  x + lambda*(point_x - x)
        // the distance is lambda*sqrt(oldsq);
        for( k=0;k<pcount;k++){ pp[k]= new_param[k]+lambda*co[k]; }
        // let's see how close we get
        newsq=0.0; newx=0.0;
        for(k=0;k<SDIM;k++)
        { REAL tmp;
          s[k]=eval(bdry->coordf[k],pp,NULLID,NULL);
          tmp = x[k]-s[k]; newx += tmp*tmp;
          tmp += lambda*(point_x[k]-x[k]);
          newsq+= tmp*tmp;
        }
        // is it close enough? 
        if( newsq <= 0.5*oldsq || (newsq <= oldsq+1e-10 &&
              (newx <= 0.5*oldsq || newsq <= 1e-10 || newx <= 1e-10 ))) break;
        lambda *= 0.5;
        oldsq  *= 0.25;
      }
      if(iter== 10 )
      {
         kb_error(9000,"Extrapolate does not converge in 10 steps: boundary is not smooth enough.\n",RECOVERABLE);
      }
      for(k=0;k<SDIM;k++) x[k]=s[k];
      for(k=0;k<pcount;k++) new_param[k]=pp[k];
    }
  }
#endif

  for ( i = 0 ; i < SDIM ; i++ ) new_x[i] = x[i];
} /* end b_extrapolate() */

/****************************************************************
*
*  Function: bdry_force()
*
*  Purpose:  Since only vertices are actually confined to boundaries,
*            edges and faces supposedly on boundaries can pull
*            away from convex boundaries, and in fact do, since
*            a long edge short-cuts the boundary.  To prevent
*            this and encourage equal-length boundary edges, an
*            energy penalty is inflicted for an edge angling away
*            from its boundary.  If S is the edge vector and P is the
*            projection matrix on the boundary tangent at the tail
*            of the edge, then the energy is the  area of the right
*            triangle formed by S and P*S,
*                  E = |S x P*S|/2
*            and the force on the head is perpendicular to the base
*            and of magnitude proportional to the length of the base,
*                  F = -|P*S|*(S - P*S)/|S - P*S|.
*            Recall force will later be projected to tangent.
*/

void bdry_force(e_id)
edge_id e_id;
{
  REAL s[MAXCOORD],q[MAXCOORD],*f;
  struct boundary *bdry;
  vertex_id head,tail;
  REAL norm;
  int i; 
  REAL qq,ss;
  MAT2D(a,3,3);

  if ( get_eattr(e_id) & FIXED ) return;

  /* see if edge and tail are on same boundary */
  bdry = get_edge_boundary(e_id);
  if ( !bdry ) return;
  if ( !(bdry->attr & B_CONVEX) ) return;
  tail = get_edge_tailv(e_id);
  if ( bdry != get_boundary(tail) )
     { invert(e_id);
       tail = get_edge_tailv(e_id);
       if ( bdry != get_boundary(tail) ) return;
     }
  head = get_edge_headv(e_id);

  /* now the calculation */
  get_edge_side(e_id,s);
  ss = SDIM_dot(s,s);

  
  if ( bdry->pcount == 1 )
  { REAL t1[MAXCOORD],t2[MAXCOORD], ac,t1t1,t2t2,st1,st2,t1u1,t2u2,ar1,ar2,ep;
    REAL *accel = &ac,*params,t1t2,val,su1,su2;

     /* tail vertex */
     params = get_param(tail);
     su1 = t1t1 = st1 = t1u1 = 0.0;
     for ( i = 0 ; i < SDIM ; i++ )
     {
        eval_second(bdry->coordf[i],params,bdry->pcount,&val,t1+i,&accel,tail);
        t1t1 += t1[i]*t1[i];
        st1 += s[i]*t1[i];
        t1u1 += t1[i]*ac;
        su1 += s[i]*ac;
     }
     ar1 = sqrt(ss*t1t1/st1/st1 - 1.0);

     /* head vertex */
     params = get_param(head);
     su2 = t2t2 = st2 = t2u2 = t1t2 = 0.0;
     for ( i = 0 ; i < SDIM ; i++ )
     {
        eval_second(bdry->coordf[i],params,bdry->pcount,&val,t2+i,&accel,tail);
        t2t2 += t2[i]*t2[i];
        t1t2 += t1[i]*t2[i];
        st2 += s[i]*t2[i];
        t2u2 += t2[i]*ac;
        su2 += s[i]*ac;
     }
     ar2 = sqrt(ss*t2t2/st2/st2 - 1.0);

     ep = -st1/6*(ar1+ar2);
     ep += ss/12/ar1*((-st1*t1t1+ss*t1u1) - ss*t1t1*(-t1t1+su1)/st1)/st1/st1;
     ep += ss/12/ar2*(-st1*t2t2 + ss*t2t2*t1t2/st2)/st2/st2;
     f = get_force(tail);
     for ( i = 0 ; i < SDIM ; i++ )
        f[i] -= ep*t1[i]/t1t1;

     ep = st2/6*(ar1+ar2);
     ep += ss/12/ar2*((st2*t2t2+ss*t2u2) - ss*t2t2*(t2t2+su2)/st2)/st2/st2;
     ep += ss/12/ar1*(st2*t1t1 - ss*t1t1*t1t2/st1)/st1/st1;
     f = get_force(head);
     for ( i = 0 ; i < SDIM ; i++ )
        f[i] -= ep*t2[i]/t2t2;

  }
  else  /* 2 or more parameters */
  { b_proj(bdry,get_param(tail),a,TANGPROJ,tail);
     matvec_mul(a,s,q,SDIM,SDIM);
     qq = SDIM_dot(q,q);
     norm = web.spring_constant*(1 + (ss-qq)/3/qq)*sqrt(fabs(ss-qq)/qq)/2; 
     f = get_force(tail);
     for ( i = 0 ; i < SDIM ; i++ )
        f[i] += norm*q[i];

     b_proj(bdry,get_param(head),a,TANGPROJ,head);
     matvec_mul(a,s,q,SDIM,SDIM);
     qq = SDIM_dot(q,q);
     norm = web.spring_constant*(1 + (ss-qq)/3/qq)*sqrt(fabs(ss-qq)/qq)/2; 
     f = get_force(head);
     for ( i = 0 ; i < SDIM ; i++ )
        f[i] -= norm*q[i];
  }
} /* end bdry_force */

/*****************************************************************
*
*  Function: bdry_spring_energy()
*
*  Purpose:  Calculate energy of kludge boundary force.
*/

void bdry_spring_energy(e_id)
edge_id e_id;
{
  REAL s[MAXCOORD],q[MAXCOORD];
  struct boundary *bdry;
  vertex_id tail,head;
  REAL ss,qq,sprenergy;
  MAT2D(a,3,3);

  if ( get_eattr(e_id) & FIXED ) return;
  if ( !(get_eattr(e_id) & BOUNDARY) ) return;

  /* see if edge and tail are on same boundary */
  bdry = get_edge_boundary(e_id);
  if ( !bdry ) return;
  if ( !(bdry->attr & B_CONVEX) ) return;
  tail = get_edge_tailv(e_id);
  head = get_edge_headv(e_id);
  if ( bdry != get_boundary(tail) )
     { 
        invert(e_id);
        tail = get_edge_tailv(e_id);
        head = get_edge_headv(e_id);
        if ( bdry != get_boundary(tail) ) return;
     }

  /* now the calculation */
  get_edge_side(e_id,s);
  ss = SDIM_dot(s,s);

  b_proj(bdry,get_param(tail),a,TANGPROJ,tail);
  matvec_mul(a,s,q,SDIM,SDIM);
  qq = SDIM_dot(q,q);
  sprenergy = web.spring_constant*ss*sqrt((ss - qq)/qq)/12;

  b_proj(bdry,get_param(head),a,TANGPROJ,head);
  matvec_mul(a,s,q,SDIM,SDIM);
  qq = SDIM_dot(q,q);
  sprenergy += web.spring_constant*ss*sqrt((ss - qq)/qq)/12;

  binary_tree_add(web.total_energy_addends,sprenergy);
  web.spring_energy += sprenergy;
} /* end bdry_spring_energy */


/*****************************************************************
*
*  Function: bdry_basis()
*
*  Purpose: calculate basis of boundary tangent plane.
*/

int bdry_basis(v_id,basis)
vertex_id v_id;
REAL **basis;  /* for return */
{
  struct boundary *b = get_boundary(v_id);
  int j,i;
  REAL dummy;  /* for eval_all function value */
  REAL temp[MAXCOORD];

  if ( !b ) return 0;

  for ( j = 0 ; j < SDIM ; j++ )
   {
     eval_all(b->coordf[j],get_param(v_id),b->pcount,&dummy,temp,v_id);
     for ( i = 0 ; i < b->pcount ; i++ )
        basis[i][j] = temp[i];
   }

  return b->pcount;
} /* end bdry_basis() */

/*****************************************************************************
* 
*              VERTEX-VERTEX HITTING ON ONE-PARAMETER BOUNDARY
*
******************************************************************************/

#define BDRYHIT_PARTNER_NAME "bdryhit_partner"

/******************************************************************************
*
* function: detect_bdry_hits()
*
* purpose: detect when two vertices along a BOUNDARY_HITTING boundary
*          collide and setting positions to average thereof.
*/

void detect_bdry_hits()
{ vertex_id v_id,vv_id;
  struct boundary *bdry;
  int i,exnum,partner;
  REAL p1,pp1;
  int eltype;
  
  exnum = find_extra(BDRYHIT_PARTNER_NAME,&eltype);
  if ( exnum < 0 ) 
    return;

  FOR_ALL_VERTICES(v_id)
  { if ( !(get_vattr(v_id) & BOUNDARY) ) continue;
    bdry = get_boundary(v_id);
    if ( !(bdry->attr & PARTNER_HITTING) ) continue;
    partner = *VINT(v_id,exnum);
    if ( partner == 0 ) continue;
    vv_id = get_ordinal_id(VERTEX,abs(partner)-1);
    if ( !valid_id(vv_id) ) continue;

    if ( abs(*VINT(vv_id,exnum)) != loc_ordinal(v_id)+1 )
    { sprintf(errmsg,"Partner of %s is %d, but partner of %d is %d.\n",
        ELNAME(v_id),partner,partner,abs(*VINT(vv_id,exnum)));
      kb_error(3378,errmsg,RECOVERABLE);
    }
    p1 = get_param(v_id)[0];
    pp1 = get_param(vv_id)[0];
    if ( ((partner > 0) && (pp1 < p1 )) || ((partner < 0) && (pp1 > p1)) )
    { REAL *param = get_param(v_id);
      REAL *x = get_coord(v_id);
      param[0] = (p1+pp1)/2;
      for ( i = 0 ; i < SDIM ; i++ )
         x[i] = eval(bdry->coordf[i],param,v_id,NULL);
      param = get_param(vv_id);
      x = get_coord(vv_id);
      param[0] = (p1+pp1)/2;
      for ( i = 0 ; i < SDIM ; i++ )
         x[i] = eval(bdry->coordf[i],param,vv_id,NULL);
      set_attr(v_id,HIT_PARTNER);
      set_attr(vv_id,HIT_PARTNER);
    }
  }
}


/******************************************************************************
*
* function: partner_hit_velocity_fix()
*
* purpose: For vertices hitting on a PARTNER_HITTING boundary
*          set force on each to average.
*/

void partner_hit_velocity_fix()
{ vertex_id v_id,vv_id;
  struct boundary *bdry;
  int i,exnum,partner,eltype;
  REAL *v,*vv;
 
  exnum = find_extra(BDRYHIT_PARTNER_NAME,&eltype);
  if ( exnum < 0 ) 
    return;

  FOR_ALL_VERTICES(v_id)
  { if ( !(get_vattr(v_id) & HIT_PARTNER) ) continue;
    bdry = get_boundary(v_id);
    if ( !(bdry->attr & PARTNER_HITTING) ) continue;
    partner = *VINT(v_id,exnum);
    if ( partner == 0 ) continue;
    vv_id = get_ordinal_id(VERTEX,abs(partner)-1);
    v = get_velocity(v_id);
    vv = get_velocity(vv_id);
    for ( i = 0 ; i < SDIM ; i++ )
    { REAL avev = (v[i]+vv[i])/2;
      v[i] = vv[i] = avev;
    }
  }
}

/******************************************************************************
*
* function: partner_hit_volgrad_fix()
*
* purpose: For vertices hitting on a PARTNER_HITTING boundary
*          set volgrad velocity on each to average.
*/

void partner_hit_volgrad_fix()
{ vertex_id v_id,vv_id;
  struct boundary *bdry;
  int i,exnum,partner,eltype;
 
  exnum = find_extra(BDRYHIT_PARTNER_NAME,&eltype);
  if ( exnum < 0 ) 
    return;

  FOR_ALL_VERTICES(v_id)
  { struct volgrad *vgptr,*vvgptr;
    if ( !(get_vattr(v_id) & HIT_PARTNER) ) continue;
    bdry = get_boundary(v_id);
    if ( !(bdry->attr & PARTNER_HITTING) ) continue;
    partner = *VINT(v_id,exnum);
    if ( partner == 0 ) continue;
    vv_id = get_ordinal_id(VERTEX,abs(partner)-1);
    for ( vgptr = get_vertex_vgrad(v_id) ; vgptr ; 
                                                   vgptr = vgptr->chain )
      for ( vvgptr = get_vertex_vgrad(vv_id) ; vvgptr ; 
                                                   vvgptr = vvgptr->chain )
        if ( vgptr->fixnum == vvgptr->fixnum )
        { for ( i = 0 ; i < SDIM ; i++ )
          { REAL avev = (vgptr->velocity[i]+vvgptr->velocity[i])/2;
            vgptr->velocity[i] = vvgptr->velocity[i] = avev;
          }
        }
  }
}

/***************************************************************************
*
* function: partner_shift_grads()
*
* purpose: For vertices with partners, shift all grads to vertex with
*          lower id.
*/

void partner_shift_grads(mode)
int mode; /* bits for CALC_FORCE and CALC_VOLGRADS */
{ vertex_id v_id,vv_id;
  struct boundary *bdry;
  int i,exnum,partner,eltype;
 
  exnum = find_extra(BDRYHIT_PARTNER_NAME,&eltype);
  if ( exnum < 0 ) 
    return;

  FOR_ALL_VERTICES(v_id)
  { struct volgrad *vgptr,*vvgptr,*prev,temp;
    REAL *x,*xx;
    if ( !(get_vattr(v_id) & HIT_PARTNER) ) continue;
    bdry = get_boundary(v_id);
    if ( !(bdry->attr & PARTNER_HITTING) ) continue;
    partner = *VINT(v_id,exnum);
    if ( partner == 0 ) continue;
    if ( abs(partner) >= ordinal(v_id)+1 ) continue;

    vv_id = get_ordinal_id(VERTEX,abs(partner)-1);  /* destination */
    if ( abs(*VINT(vv_id,exnum)) != ordinal(v_id)+1 )
    { sprintf(errmsg,"Partner of %d is %d, but partner of %d is %d.\n",
        ordinal(v_id)+1,partner,partner,abs(*VINT(vv_id,exnum)));
      kb_error(3383,errmsg,RECOVERABLE);
    }

    if ( mode & CALC_FORCE )
    { x = get_force(v_id);
      xx = get_force(vv_id);
      for ( i = 0 ; i < SDIM ; i++ )
      { xx[i] += x[i];
        x[i] = 0.0;
      }
    }

    if ( (mode & CALC_VOLGRADS) && vgradbase )
    { prev = NULL;
      for ( vgptr = get_vertex_vgrad(v_id) ; vgptr ; prev = vgptr,
                                                   vgptr = vgptr->chain )
      { int found = 0;
        for ( vvgptr = get_vertex_vgrad(vv_id) ; vvgptr ; 
                                                   vvgptr = vvgptr->chain )
          if ( vgptr->fixnum == vvgptr->fixnum )
          { for ( i = 0 ; i < SDIM ; i++ )
            { vvgptr->grad[i] += vgptr->grad[i];
              vgptr->grad[i] = 0.0;
            }
            found = 1;
            break;
          }
  
        if ( !found ) /* move whole structure over to vv_id */
        { vvgptr = get_vertex_vgrad(vv_id);            
          temp.chain = vgptr->chain;
          set_vertex_vgrad(vv_id,vgptr);
          if ( prev )
            prev->chain = vgptr->chain;
          else set_vertex_vgrad(v_id,vgptr->chain); 
          vgptr->chain = vvgptr;
          vgptr = &temp;
        }
      }
    }
  }

}

/***************************************************************************
*
* function: partner_move()
*
* purpose: For vertices with partners, shift position to vertex with
*          lower id.
*/

void partner_move()
{ vertex_id v_id,vv_id;
  struct boundary *bdry;
  int i,exnum,partner,eltype;
 
  exnum = find_extra(BDRYHIT_PARTNER_NAME,&eltype);
  if ( exnum < 0 ) 
    return;

  FOR_ALL_VERTICES(v_id)
  { REAL *x,*xx,*p,*pp;
    
    if ( !(get_vattr(v_id) & HIT_PARTNER) ) continue;
    bdry = get_boundary(v_id);
    if ( !(bdry->attr & PARTNER_HITTING) ) continue;
    partner = *VINT(v_id,exnum);
    if ( partner == 0 ) continue;
    if ( abs(partner) >= ordinal(v_id)+1 ) continue;

    vv_id = get_ordinal_id(VERTEX,abs(partner)-1);  /* destination */
    if ( !valid_id(vv_id) ) continue;
    if ( abs(*VINT(vv_id,exnum)) != ordinal(v_id)+1 )
    { sprintf(errmsg,"Partner of %d is %d, but partner of %d is %d.\n",
        ordinal(v_id)+1,partner,partner,abs(*VINT(vv_id,exnum)));
      kb_error(3006,errmsg,RECOVERABLE);
    }
    p = get_param(v_id);
    pp = get_param(vv_id);
    x = get_coord(v_id);
    xx = get_coord(vv_id);
    for ( i = 0 ; i < SDIM ; i++ )
      x[i] = xx[i];
    for ( i = 0 ; i < bdry->pcount ; i++ )
      p[i] = pp[i];
    
  }
}
