/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/


/******************************************************************
*
*  File:  torvol.c
*
*  Purpose: find volumes of bodies in toroidal domain
*              and volume gradients.
*
*/

#include "include.h"

#define NEWTORVOL    /* invokes named method */

void torvol()
{
  facet_id f_id;    /* main facet iterator */
  body_id b_id;
  body_id b0_id,b1_id;    /* facet adjacent bodies */

#ifdef NEWTORVOL
struct qinfo f_info; /* for calling q_facet_torus_volume */
q_info_init(&f_info,METHOD_VALUE);
#endif

  /* adjust body volumes to the invariant constant for each */
  FOR_ALL_BODIES(b_id)
    set_body_volume(b_id,get_body_volconst(b_id),NOSETSTAMP);
  if ( web.representation == STRING )
    FOR_ALL_FACETS(f_id)
      set_facet_area(f_id,0.0);

  FOR_ALL_FACETS(f_id)
  {
    REAL t;     /* accumulator for this facet */

    if ( get_fattr(f_id) & NONCONTENT ) continue;

    /* find adjacent bodies */
    b0_id = get_facet_body(f_id);
    b1_id = get_facet_body(facet_inverse(f_id));
    if ( !valid_id(b0_id) && !valid_id(b1_id) ) continue;

#ifdef NEWTORVOL
    f_info.id = f_id;
    q_facet_setup(NULL,&f_info,NEED_SIDE|TORUS_MODULO_MUNGE|ORIENTABLE_METHOD);
    t = q_facet_torus_volume(&f_info);
    if ( valid_id(b0_id) )
        add_body_volume(b0_id,t);
    if ( valid_id(b1_id) )
        add_body_volume(b1_id,-t);
#else
    REAL *v[FACET_VERTS];  /* pointers to three vertex coordinates */
    facetedge_id fe;
    int i;
    REAL adj[FACET_EDGES][MAXCOORD];  /* torus wrap adjustments for edge */
    /* get basic info */
    fe = get_facet_fe(f_id);
    for ( i = 0 ; i < FACET_EDGES ; i ++ )
    {
      v[i] = get_coord(get_fe_tailv(fe));
      get_edge_adjust(get_fe_edge(fe),adj[i]); 
      fe = get_next_edge(fe);
    }

    /* basic tetrahedron */
    t = triple_prod(v[0],v[1],v[2]);

    /* torus wrap corrections */
    for ( i = 0 ; i < FACET_EDGES ; i++ )
    {
      /* two-vertex term */
      t += triple_prod(adj[(i+1)%FACET_EDGES],v[i],v[(i+1)%FACET_EDGES])/2;
      t -= triple_prod(adj[(i+2)%FACET_EDGES],v[i],v[(i+1)%FACET_EDGES])/2;

      /* one-vertex term */
      t += triple_prod(v[i],adj[(i+2)%FACET_EDGES],adj[i]);
    }

    if ( valid_id(b0_id) )
       add_body_volume(b0_id,t/6);
    if ( valid_id(b1_id) )
       add_body_volume(b1_id,-t/6);
#endif
        }
#ifdef NEWTORVOL
q_info_free(&f_info);
#endif
}



/**********************************************************************
                    Linear torus volume quantity
**********************************************************************/

/**********************************************************************
*
*  function: q_facet_torus_volume()
*
*  purpose: value of volume integral on facet
*/

REAL q_facet_torus_volume(f_info)
struct qinfo *f_info;
{ REAL **x;
  REAL vol;
  int i;
  REAL **dx = web.inverse_periods;
  unsigned long allwrap;
  MAT2D(u,FACET_VERTS,MAXCOORD); /* affine coordinates of vertices */
  facetedge_id fe;
  WRAPTYPE w[FACET_EDGES];

  if ( !dx )
    kb_error(3370,"Method facet_torus_volume requires torus model.\n",
       RECOVERABLE);
       
  if ( web.modeltype == QUADRATIC ) return q_facet_torus_volume_q(f_info);
  if ( web.modeltype == LAGRANGE )return q_facet_torus_volume_lagr(f_info);

  x = f_info->x;

  /* get affine coordinates of vertices */
  mat_mul_tr(x,dx,u,FACET_VERTS,SDIM,SDIM);
  /* main integral over facet */
  vol = ((u[1][0]-u[0][0])*(u[2][1]-u[0][1])
                    - (u[1][1]-u[0][1])*(u[2][0]-u[0][0]));
  vol *= (u[0][2]+u[1][2]+u[2][2])/6;

  /* add corrections due to wraps */
  fe = get_facet_fe(f_info->id);
  for ( i = 0 ; i < FACET_EDGES ; i++, fe = get_next_edge(fe) )
     w[i] = get_fe_wrap(fe);

if ( f_info->wraps[0] )
{ sprintf(errmsg,"Base vertex of facet %s has nonzero wrap in facet_torus_volume.\n",
     ELNAME(f_info->id));
  kb_error(1326,errmsg,WARNING);
}

#define WR(w,i)  (w>>((i)*TWRAPBITS) & WRAPMASK)
  allwrap = WR(w[0],2)+(WR(w[1],2)<<TWRAPBITS)+(WR(w[2],2)<<(2*TWRAPBITS));
  switch ( allwrap )
      { case 0: break;
        case (POSWRAP)+(NEGWRAP << TWRAPBITS): break;
        case (POSWRAP << TWRAPBITS) + (NEGWRAP << (2*TWRAPBITS)): break; 

        case (NEGWRAP) + (POSWRAP << (2*TWRAPBITS)):
            vol +=  ((u[1][0]+u[0][0])*(u[1][1]-u[0][1])
                      +(u[2][0]+u[1][0])*(u[2][1]-u[1][1])
                      +(u[0][0]+u[2][0])*(u[0][1]-u[2][1]))/2;
            break;

        case (POSWRAP ) + (NEGWRAP << (2*TWRAPBITS)):
            vol +=  (u[1][0]+u[2][0])*(u[1][1]-u[2][1])/2;

            switch ( WR(f_info->wraps[1],0) )
            { case POSWRAP: vol -= u[1][1]; break;
              case NEGWRAP: vol += u[1][1]; break;
            }
            switch ( WR(f_info->wraps[2],0) )
            { case POSWRAP: vol += u[2][1]; break;
              case NEGWRAP: vol -= u[2][1]; break;
            }
            break;

        case  (NEGWRAP) + (POSWRAP << TWRAPBITS):
            vol += ((u[1][0]+u[0][0])*(u[1][1]-u[0][1])
                      +(u[2][0]+u[1][0])*(u[2][1]-u[1][1]))/2;
            switch ( WR(f_info->wraps[2],0) )
            { case POSWRAP: vol -= u[2][1]; break;
              case NEGWRAP: vol += u[2][1]; break;
            }
          break;

        case (NEGWRAP << TWRAPBITS) + (POSWRAP << (2*TWRAPBITS)):
            vol +=  ((u[2][0]+u[1][0])*(u[2][1]-u[1][1])
                      +(u[0][0]+u[2][0])*(u[0][1]-u[2][1]))/2;
            switch ( WR(f_info->wraps[1],0) )
            { case POSWRAP: vol += u[1][1]; ; break;
              case NEGWRAP: vol -= u[1][1]; ; break;
            }
            break;

         default: 
           sprintf(errmsg,
        "Internal error: Bad allwrap %08lX in facet_torus_volume facet %s.\n",
             allwrap,ELNAME(f_info->id));
           kb_error(1327,errmsg, RECOVERABLE);
     }
     vol *= web.torusv;
 
  return vol;
}

/**********************************************************************
*
*  function: q_facet_torus_volume_grad()
*
*  purpose: gradient and value of volume integral on quadratic facet
*/

REAL q_facet_torus_volume_grad(f_info)
struct qinfo *f_info;
{ REAL **x;
  REAL zsum,ssum;
  int i,j;
  REAL **dx = web.inverse_periods;
  WRAPTYPE w[FACET_EDGES];
  unsigned long allwrap;
  MAT2D(u,FACET_VERTS,MAXCOORD); /* affine coordinates of vertices */
  facetedge_id fe;
  MAT2D(ugrad,FACET_VERTS,MAXCOORD);
  REAL vol;

  if ( web.modeltype == QUADRATIC ) return q_facet_torus_volume_q_grad(f_info);
  if ( web.modeltype == LAGRANGE ) 
     return q_facet_torus_volume_lagr_grad(f_info);


  x = f_info->x;

  /* get affine coordinates of vertices */
  mat_mul_tr(x,dx,u,FACET_VERTS,SDIM,SDIM);
  /* main integral over facet */
  zsum = (u[0][2]+u[1][2]+u[2][2])/6;
  ssum = (u[1][0]-u[0][0])*(u[2][1]-u[0][1]) 
            - (u[1][1]-u[0][1])*(u[2][0]-u[0][0]);
  vol = zsum*ssum;
  ugrad[0][0] = zsum*(u[1][1]-u[2][1]);
  ugrad[0][1] = zsum*(u[2][0]-u[1][0]);
  ugrad[0][2] = ssum/6;
  ugrad[1][0] = zsum*(u[2][1]-u[0][1]);
  ugrad[1][1] = zsum*(u[0][0]-u[2][0]);
  ugrad[1][2] = ssum/6;
  ugrad[2][0] = zsum*(u[0][1]-u[1][1]);
  ugrad[2][1] = zsum*(u[1][0]-u[0][0]);
  ugrad[2][2] = ssum/6;
  /* add corrections due to wraps */
  fe = get_facet_fe(f_info->id);
  for ( i = 0 ; i < FACET_EDGES ; i++, fe = get_next_edge(fe) )
     w[i] = get_fe_wrap(fe);

  allwrap = WR(w[0],2)+(WR(w[1],2)<<TWRAPBITS)+(WR(w[2],2)<<(2*TWRAPBITS));
  switch ( allwrap )
      { case 0: break;
        case (POSWRAP)+(NEGWRAP << TWRAPBITS): break;
        case (POSWRAP << TWRAPBITS) + (NEGWRAP << (2*TWRAPBITS)): break; 
        case (NEGWRAP) + (POSWRAP << (2*TWRAPBITS)):
            vol += ((u[1][0]+u[0][0])*(u[1][1]-u[0][1])
                      +(u[2][0]+u[1][0])*(u[2][1]-u[1][1])
                      +(u[0][0]+u[2][0])*(u[0][1]-u[2][1]))/2;
            ugrad[0][0] += (u[1][1]-u[2][1])/2;
            ugrad[0][1] -= (u[1][0]-u[2][0])/2;
            ugrad[1][0] += (u[2][1]-u[0][1])/2;
            ugrad[1][1] -= (u[2][0]-u[0][0])/2;
            ugrad[2][0] += (u[0][1]-u[1][1])/2;
            ugrad[2][1] -= (u[0][0]-u[1][0])/2; 
            break;
        case (POSWRAP ) + (NEGWRAP << (2*TWRAPBITS)):
            vol += (u[1][0]+u[2][0])*(u[1][1]-u[2][1])/2;
            ugrad[1][0] += (u[1][1]-u[2][1])/2;
            ugrad[2][0] += (u[1][1]-u[2][1])/2;
            ugrad[1][1] += (u[1][0]+u[2][0])/2;
            ugrad[2][1] -= (u[1][0]+u[2][0])/2;
            switch ( WR(f_info->wraps[1],0) )
            { case POSWRAP: vol -= u[1][1]; ugrad[1][1] -= 1.0; break;
              case NEGWRAP: vol += u[1][1]; ugrad[1][1] += 1.0; break;
            }
            switch ( WR(f_info->wraps[2],0) )
            { case POSWRAP: vol += u[2][1]; ugrad[2][1] += 1.0; break;
              case NEGWRAP: vol -= u[2][1]; ugrad[2][1] -= 1.0; break;
            }
            break;
        case  (NEGWRAP) + (POSWRAP << TWRAPBITS):
            vol += ((u[1][0]+u[0][0])*(u[1][1]-u[0][1])
                      +(u[2][0]+u[1][0])*(u[2][1]-u[1][1]))/2;
            ugrad[0][0] += (u[1][1]-u[0][1])/2;
            ugrad[0][1] -= (u[1][0]+u[0][0])/2;
            ugrad[1][0] += (u[2][1]-u[0][1])/2;
            ugrad[1][1] += (u[0][0]-u[2][0])/2;
            ugrad[2][0] += (u[2][1]-u[1][1])/2;
            ugrad[2][1] += (u[2][0]+u[1][0])/2;
            switch ( WR(f_info->wraps[2],0) )
            { case POSWRAP: vol -= u[2][1]; ugrad[2][1] -= 1.0; break;
              case NEGWRAP: vol += u[2][1]; ugrad[2][1] += 1.0; break;
            }
            switch ( WR(f_info->wraps[0],0) )
            { case POSWRAP: vol += u[0][1]; ugrad[0][1] += 1.0;break;
              case NEGWRAP: vol -= u[0][1]; ugrad[0][1] -= 1.0;break;
            }
            break;
        case (NEGWRAP << TWRAPBITS) + (POSWRAP << (2*TWRAPBITS)):
            vol += ((u[2][0]+u[1][0])*(u[2][1]-u[1][1])
                      +(u[0][0]+u[2][0])*(u[0][1]-u[2][1]))/2;
            ugrad[0][0] += (u[0][1]-u[2][1])/2;
            ugrad[0][1] += (u[0][0]+u[2][0])/2;
            ugrad[1][0] += (u[2][1]-u[1][1])/2;
            ugrad[1][1] -= (u[2][0]+u[1][0])/2;
            ugrad[2][0] += (u[0][1]-u[1][1])/2;
            ugrad[2][1] += (u[1][0]-u[0][0])/2;
            switch ( WR(f_info->wraps[1],0) )
            { case POSWRAP: vol += u[1][1]; ugrad[1][1] += 1.0; break;
              case NEGWRAP: vol -= u[1][1]; ugrad[1][1] -= 1.0; break;
            }
            break;
         default: kb_error(1328,"Internal error: Bad allwrap in facet_volume.\n",RECOVERABLE);
     }
     vol *= web.torusv;
     for ( i = 0 ; i < FACET_VERTS ; i++ )
        for ( j = 0 ; j < SDIM ; j++ ) 
          ugrad[i][j] *= web.torusv;
     mat_mult(ugrad,dx,f_info->grad,FACET_VERTS,SDIM,SDIM);
     return vol;
}

/**********************************************************************
*
*  function: q_facet_torus_volume_hess()
*
*  purpose: hessian, gradient and value of volume integral on quadratic facet
*/

REAL q_facet_torus_volume_hess(f_info)
struct qinfo *f_info;
{ REAL **x;
  REAL zsum,ssum;
  int i,j,ii,jj;
  REAL **dx = web.inverse_periods;
  WRAPTYPE w[FACET_EDGES];
  unsigned long allwrap;
  MAT2D(u,FACET_VERTS,MAXCOORD); /* affine coordinates of vertices */
  facetedge_id fe;
  MAT2D(ugrad,FACET_VERTS,MAXCOORD);
  MAT4D(h,FACET_VERTS,FACET_VERTS,MAXCOORD,MAXCOORD);
  MAT2D(temph,MAXCOORD,MAXCOORD);
  REAL vol;
  
  if ( !dx ) 
     kb_error(2194,"Need torus model to do facet_torus_volume.\n",RECOVERABLE);
  if ( web.modeltype == QUADRATIC ) return q_facet_torus_volume_q_hess(f_info);
  if ( web.modeltype == LAGRANGE ) return q_facet_torus_volume_lagr_hess(f_info);

  x = f_info->x;

  /* get affine coordinates of vertices */
  mat_mul_tr(x,dx,u,FACET_VERTS,SDIM,SDIM);
  /* main integral over facet */
  zsum = (u[0][2]+u[1][2]+u[2][2])/6;
  ssum = (u[1][0]-u[0][0])*(u[2][1]-u[0][1]) 
                        - (u[1][1]-u[0][1])*(u[2][0]-u[0][0]);
  vol = zsum*ssum;
  ugrad[0][0] = zsum*(u[1][1]-u[2][1]);
  ugrad[0][1] = zsum*(u[2][0]-u[1][0]);
  ugrad[0][2] = ssum/6;
  ugrad[1][0] = zsum*(u[2][1]-u[0][1]);
  ugrad[1][1] = zsum*(u[0][0]-u[2][0]);
  ugrad[1][2] = ssum/6;
  ugrad[2][0] = zsum*(u[0][1]-u[1][1]);
  ugrad[2][1] = zsum*(u[1][0]-u[0][0]);
  ugrad[2][2] = ssum/6;

  for ( i = 0 ; i < FACET_VERTS ; i++ )
     for ( ii = 0 ; ii < FACET_VERTS ; ii++ )
        for ( j = 0 ; j < SDIM ; j++ ) 
          for ( jj = 0 ; jj < SDIM ; jj++ ) 
              h[i][ii][j][jj] = 0.0;

  h[1][2][0][1] = h[1][0][1][0] = h[0][2][1][0] =  zsum;
  h[2][1][1][0] = h[0][1][0][1] = h[2][0][0][1] =  zsum;
  h[1][0][0][1] = h[0][2][0][1] = h[1][2][1][0] = -zsum;
  h[0][1][1][0] = h[2][0][1][0] = h[2][1][0][1] = -zsum;

  for ( i = 0 ; i < 3 ; i++ )
  { h[i][0][2][0] = h[0][i][0][2] = (u[1][1]-u[2][1])/6;
     h[i][0][2][1] = h[0][i][1][2] = (u[2][0]-u[1][0])/6;
     h[i][1][2][0] = h[1][i][0][2] = (u[2][1]-u[0][1])/6;
     h[i][1][2][1] = h[1][i][1][2] = (u[0][0]-u[2][0])/6;
     h[i][2][2][0] = h[2][i][0][2] = (u[0][1]-u[1][1])/6;
     h[i][2][2][1] = h[2][i][1][2] = (u[1][0]-u[0][0])/6;
  }
  /* add corrections due to wraps */
  fe = get_facet_fe(f_info->id);
  for ( i = 0 ; i < FACET_EDGES ; i++, fe = get_next_edge(fe) ) 
     w[i] = get_fe_wrap(fe);

  allwrap = WR(w[0],2)+(WR(w[1],2)<<TWRAPBITS)+(WR(w[2],2)<<(2*TWRAPBITS));
  switch ( allwrap )
      { case 0: break;
        case (POSWRAP)+(NEGWRAP << TWRAPBITS): break;
        case (POSWRAP << TWRAPBITS) + (NEGWRAP << (2*TWRAPBITS)): break; 
        case (NEGWRAP) + (POSWRAP << (2*TWRAPBITS)):
            vol += ((u[1][0]+u[0][0])*(u[1][1]-u[0][1])
                      +(u[2][0]+u[1][0])*(u[2][1]-u[1][1])
                      +(u[0][0]+u[2][0])*(u[0][1]-u[2][1]))/2;
            ugrad[0][0] += (u[1][1]-u[2][1])/2;
            ugrad[0][1] -= (u[1][0]-u[2][0])/2;
            ugrad[1][0] += (u[2][1]-u[0][1])/2;
            ugrad[1][1] -= (u[2][0]-u[0][0])/2;
            ugrad[2][0] += (u[0][1]-u[1][1])/2;
            ugrad[2][1] -= (u[0][0]-u[1][0])/2; 
            h[0][1][0][1] += 0.5;
            h[0][2][0][1] -= 0.5;
            h[0][1][1][0] -= 0.5;
            h[0][2][1][0] += 0.5;
            h[1][2][0][1] += 0.5;
            h[1][0][0][1] -= 0.5;
            h[1][2][1][0] -= 0.5;
            h[1][0][1][0] += 0.5;
            h[2][0][0][1] += 0.5;
            h[2][1][0][1] -= 0.5;
            h[2][0][1][0] -= 0.5;
            h[2][1][1][0] += 0.5;
            break;
        case (POSWRAP ) + (NEGWRAP << (2*TWRAPBITS)):
            vol += (u[1][0]+u[2][0])*(u[1][1]-u[2][1])/2;
            ugrad[1][0] += (u[1][1]-u[2][1])/2;
            ugrad[2][0] += (u[1][1]-u[2][1])/2;
            ugrad[1][1] += (u[1][0]+u[2][0])/2;
            ugrad[2][1] -= (u[1][0]+u[2][0])/2;
            h[1][1][0][1] += 0.5;
            h[1][2][0][1] -= 0.5;
            h[2][1][0][1] += 0.5;
            h[2][2][0][1] -= 0.5;
            h[1][1][1][0] += 0.5;
            h[1][2][1][0] += 0.5;
            h[2][1][1][0] -= 0.5;
            h[2][2][1][0] -= 0.5;
            switch ( WR(f_info->wraps[1],0) )
            { case POSWRAP: vol -= u[1][1]; ugrad[1][1] -= 1.0; break;
              case NEGWRAP: vol += u[1][1]; ugrad[1][1] += 1.0; break;
            }
            switch ( WR(f_info->wraps[2],0) )
            { case POSWRAP: vol += u[2][1]; ugrad[2][1] += 1.0; break;
              case NEGWRAP: vol -= u[2][1]; ugrad[2][1] -= 1.0; break;
            }
            break;
        case  (NEGWRAP) + (POSWRAP << TWRAPBITS):
            vol += ((u[1][0]+u[0][0])*(u[1][1]-u[0][1])
                      +(u[2][0]+u[1][0])*(u[2][1]-u[1][1]))/2;
            ugrad[0][0] += (u[1][1]-u[0][1])/2;
            ugrad[0][1] -= (u[1][0]+u[0][0])/2;
            ugrad[1][0] += (u[2][1]-u[0][1])/2;
            ugrad[1][1] += (u[0][0]-u[2][0])/2;
            ugrad[2][0] += (u[2][1]-u[1][1])/2;
            ugrad[2][1] += (u[2][0]+u[1][0])/2;
            h[0][1][0][1] += 0.5;
            h[0][0][0][1] -= 0.5;
            h[0][1][1][0] -= 0.5;
            h[0][0][1][0] -= 0.5;
            h[1][2][0][1] += 0.5;
            h[1][0][0][1] -= 0.5;
            h[1][0][1][0] += 0.5;
            h[1][2][1][0] -= 0.5;
            h[2][2][0][1] += 0.5;
            h[2][1][0][1] -= 0.5;
            h[2][2][1][0] += 0.5;
            h[2][1][1][0] += 0.5;
            switch ( WR(f_info->wraps[2],0) )
            { case POSWRAP: vol -= u[2][1]; ugrad[2][1] -= 1.0; break;
              case NEGWRAP: vol += u[2][1]; ugrad[2][1] += 1.0; break;
            }
            switch ( WR(f_info->wraps[0],0) )
            { case POSWRAP: vol += u[0][1]; ugrad[0][1] += 1.0;break;
              case NEGWRAP: vol -= u[0][1]; ugrad[0][1] -= 1.0;break;
            }
            break;
        case (NEGWRAP << TWRAPBITS) + (POSWRAP << (2*TWRAPBITS)):
            vol += ((u[2][0]+u[1][0])*(u[2][1]-u[1][1])
                      +(u[0][0]+u[2][0])*(u[0][1]-u[2][1]))/2;
            ugrad[0][0] += (u[0][1]-u[2][1])/2;
            ugrad[0][1] += (u[0][0]+u[2][0])/2;
            ugrad[1][0] += (u[2][1]-u[1][1])/2;
            ugrad[1][1] -= (u[2][0]+u[1][0])/2;
            ugrad[2][0] += (u[0][1]-u[1][1])/2;
            ugrad[2][1] += (u[1][0]-u[0][0])/2;
            h[0][0][0][1] += 0.5;
            h[0][2][0][1] -= 0.5;
            h[0][0][1][0] += 0.5;
            h[0][2][1][0] += 0.5;
            h[1][2][0][1] += 0.5;
            h[1][1][0][1] -= 0.5;
            h[1][2][1][0] -= 0.5;
            h[1][1][1][0] -= 0.5;
            h[2][0][0][1] += 0.5;
            h[2][1][0][1] -= 0.5;
            h[2][1][1][0] += 0.5;
            h[2][0][1][0] -= 0.5;
            switch ( WR(f_info->wraps[1],0) )
            { case POSWRAP: vol += u[1][1]; ugrad[1][1] += 1.0; break;
              case NEGWRAP: vol -= u[1][1]; ugrad[1][1] -= 1.0; break;
            }
            break;
         default: kb_error(1330,"Internal error: Bad allwrap in facet_volume.\n",RECOVERABLE);

     }
  vol *= web.torusv;
  for ( i = 0 ; i < FACET_VERTS ; i++ )
     for ( j = 0 ; j < SDIM ; j++ ) 
       ugrad[i][j] *= web.torusv;
  mat_mult(ugrad,dx,f_info->grad,FACET_VERTS,SDIM,SDIM);
  for ( i = 0 ; i < FACET_VERTS ; i++ )
     for ( ii = 0 ; ii < FACET_VERTS ; ii++ )
        { for ( j = 0 ; j < SDIM ; j++ ) 
             for ( jj = 0 ; jj < SDIM ; jj++ ) 
             {
               h[i][ii][j][jj] *= web.torusv;
              }
          mat_mult(h[i][ii],dx,temph,SDIM,SDIM,SDIM);
          tr_mat_mul(dx,temph,f_info->hess[i][ii],SDIM,SDIM,SDIM);
        }
  return vol;
}


/**********************************************************************
                    Quadratic torus volume quantity
**********************************************************************/

REAL cut_int ARGS((REAL**));
void cut_grad ARGS((REAL**,REAL**,int));
void cut_hess ARGS((REAL**,REAL**,REAL****,int));

REAL cut_int(u)
REAL **u; /* coefficients of the 3 vertices along edge */
{ REAL area = 0.0;
  int i,j;
  for ( i = 0 ; i < EDGE_CTRL ; i++ )
     for ( j = 0 ; j < EDGE_CTRL ; j ++ )
        area += scoeff[i][j]*u[i][0]*u[j][1];
  return area;
}

void cut_grad(u,ugrad,sign)
REAL **u;
REAL **ugrad; /* gradients to increment */
int sign;  /* +1 or -1 */
{
  int i,j;
  for ( i = 0 ; i < EDGE_CTRL ; i++ )
     for ( j = 0 ; j < EDGE_CTRL ; j ++ )
     { ugrad[i][0] += sign*scoeff[i][j]*u[j][1];
       ugrad[j][1] += sign*scoeff[i][j]*u[i][0];
     }
}

void cut_hess(u,ugrad,uhess,sign)
REAL **u;
REAL **ugrad; /* gradients to increment */
REAL ****uhess; /* hessians to increment */
int sign;  /* +1 or -1 */
{
  int i,j;
  for ( i = 0 ; i < EDGE_CTRL ; i++ )
     for ( j = 0 ; j < EDGE_CTRL ; j ++ )
     { ugrad[i][0] += sign*scoeff[i][j]*u[j][1];
       ugrad[j][1] += sign*scoeff[i][j]*u[i][0];
       uhess[i][j][0][1] += sign*scoeff[i][j];
       uhess[j][i][1][0] += sign*scoeff[i][j];
     }
}
/**********************************************************************
*
*  function: q_facet_torus_volume_q()
*
*  purpose: value of volume integral on facet, quadratic
*/

REAL q_facet_torus_volume_q(f_info)
struct qinfo *f_info;
{ REAL **x;
  REAL vol;
  int i,j,k;
  REAL **dx = web.inverse_periods;
  WRAPTYPE w[FACET_EDGES];
  unsigned long allwrap;
  MAT2D(u,FACET_CTRL+1,MAXCOORD); /* affine coordinates of vertices */
  facetedge_id fe;

  x = f_info->x;

  /* get affine coordinates of vertices */
  mat_mul_tr(x,dx,u,FACET_CTRL,SDIM,SDIM);
  u[FACET_CTRL] = u[0]; /* handy wrap around */

  /* main integral over facet */

  /* volume, integral of z dx dy */
  vol = 0.0;
  for ( i = 0 ; i < FACET_CTRL ; i++ )
     for ( j = 0 ; j < FACET_CTRL ; j++ )
        for ( k = 0 ; k < FACET_CTRL ; k++ )
        { REAL v = vcoeff[i][j][k];
          if ( v == 0.0 ) continue;
          vol += v*u[i][2]*(u[j][0]*u[k][1]-u[j][1]*u[k][0]);
        }

  /* add corrections due to wraps */
  fe = get_facet_fe(f_info->id);
  for ( i = 0 ; i < FACET_EDGES ; i++, fe = get_next_edge(fe) ) 
     w[i] = get_fe_wrap(fe);

  allwrap = WR(w[0],2)+(WR(w[1],2)<<TWRAPBITS)+(WR(w[2],2)<<(2*TWRAPBITS));
  switch ( allwrap )
     { case 0: break;
        case (POSWRAP)+(NEGWRAP << TWRAPBITS): break;
        case (POSWRAP << TWRAPBITS) + (NEGWRAP << (2*TWRAPBITS)): break; 
        case (NEGWRAP) + (POSWRAP << (2*TWRAPBITS)):
            vol -= cut_int(u) + cut_int(u+2) + cut_int(u+4);
            break;
        case (POSWRAP ) + (NEGWRAP << (2*TWRAPBITS)):
            vol += cut_int(u+2);
            switch ( WR(f_info->wraps[2],0) )
            { case POSWRAP: vol -= u[2][1];break;
              case NEGWRAP: vol += u[2][1];break;
            }
            switch ( WR(f_info->wraps[4],0) )
            { case POSWRAP: vol += u[4][1]; break;
              case NEGWRAP: vol -= u[4][1]; break;
            }
            break;
        case  (NEGWRAP) + (POSWRAP << TWRAPBITS):
            vol -= cut_int(u) + cut_int(u+2);
            switch ( WR(f_info->wraps[4],0) )
            { case POSWRAP: vol -= u[4][1]; break;
              case NEGWRAP: vol += u[4][1]; break;
            }
            switch ( WR(f_info->wraps[0],0) )
            { case POSWRAP: vol += u[0][1]; break;
              case NEGWRAP: vol -= u[0][1]; break;
            }
            break;
        case (NEGWRAP << TWRAPBITS) + (POSWRAP << (2*TWRAPBITS)):
            vol -= cut_int(u+2) + cut_int(u+4);
            switch ( WR(f_info->wraps[2],0) )
            { case POSWRAP: vol += u[2][1]; break;
              case NEGWRAP: vol -= u[2][1]; break;
            }
            break;
         default: kb_error(1331,"Internal error: Bad allwrap in facet_volume.\n",RECOVERABLE);

     }
  vol *= web.torusv;
 
  return vol;
}

/**********************************************************************
*
*  function: q_facet_torus_volume_q_grad()
*
*  purpose: gradient and value of volume integral on quadratic facet
*/

REAL q_facet_torus_volume_q_grad(f_info)
struct qinfo *f_info;
{ REAL **x;
  REAL vol;
  int i,j,k;
  REAL **dx = web.inverse_periods;
  WRAPTYPE w[FACET_EDGES];
  unsigned long allwrap;
  MAT2D(u,FACET_CTRL+1,MAXCOORD); /* affine coordinates of vertices */
  MAT2D(ugrad,FACET_CTRL+1,MAXCOORD);
  facetedge_id fe;

  x = f_info->x;

  /* get affine coordinates of vertices */
  mat_mul_tr(x,dx,u,FACET_CTRL,SDIM,SDIM);
  u[FACET_CTRL] = u[0]; /* handy wrap around */
  ugrad[FACET_CTRL] = ugrad[0]; /* handy wrap around */

  /* main integral over facet */

  /* volume, integral of z dx dy */
  vol = 0.0;
  for ( i = 0 ; i < FACET_CTRL ; i++ )
     for ( j = 0 ; j < FACET_CTRL ; j++ )
        for ( k = 0 ; k < FACET_CTRL ; k++ )
        { REAL v = vcoeff[i][j][k];
          if ( v == 0.0 ) continue;
          vol += v*u[i][2]*(u[j][0]*u[k][1]-u[j][1]*u[k][0]);
        }

  /* gradients */
  for ( k = 0 ; k < FACET_CTRL ; k++ )
     for ( j = 0 ; j < SDIM ; j++ )
        ugrad[k][j] = 0.0;

  for ( i = 0 ; i < FACET_CTRL ; i++ )
    for ( j = 0 ; j < FACET_CTRL ; j++ )
     for ( k = 0 ; k < FACET_CTRL ; k++ )
     { REAL v = vcoeff[i][j][k];
        if ( v == 0.0 ) continue;
        ugrad[i][2] += v*(u[j][0]*u[k][1]-u[j][1]*u[k][0]);
        ugrad[j][0] += v*u[i][2]*u[k][1];
        ugrad[k][1] += v*u[i][2]*u[j][0];
        ugrad[j][1] -= v*u[i][2]*u[k][0];
        ugrad[k][0] -= v*u[i][2]*u[j][1];
     }

  /* add corrections due to wraps */
  fe = get_facet_fe(f_info->id);
  for ( i = 0 ; i < FACET_EDGES ; i++, fe = get_next_edge(fe) ) 
     w[i] = get_fe_wrap(fe);

  allwrap = WR(w[0],2)+(WR(w[1],2)<<TWRAPBITS)+(WR(w[2],2)<<(2*TWRAPBITS));
  switch ( allwrap )
     { case 0: break;
        case (POSWRAP)+(NEGWRAP << TWRAPBITS): break;
        case (POSWRAP << TWRAPBITS) + (NEGWRAP << (2*TWRAPBITS)): break; 
        case (NEGWRAP) + (POSWRAP << (2*TWRAPBITS)):
            vol -= cut_int(u) + cut_int(u+2) + cut_int(u+4);
            cut_grad(u,ugrad,-1); 
            cut_grad(u+2,ugrad+2,-1); 
            cut_grad(u+4,ugrad+4,-1);
            break;

        case (POSWRAP ) + (NEGWRAP << (2*TWRAPBITS)):
            vol += cut_int(u+2);
            cut_grad(u+2,ugrad+2,1);
            switch ( WR(f_info->wraps[2],0) )
            { case POSWRAP: vol -= u[2][1]; ugrad[2][1] -= 1.0; break;
              case NEGWRAP: vol += u[2][1]; ugrad[2][1] += 1.0; break;
            }
            switch ( WR(f_info->wraps[4],0) )
            { case POSWRAP: vol += u[4][1]; ugrad[4][1] += 1.0; break;
              case NEGWRAP: vol -= u[4][1]; ugrad[4][1] -= 1.0; break;
            }
            break;
        case  (NEGWRAP) + (POSWRAP << TWRAPBITS):
            vol -= cut_int(u) + cut_int(u+2);
            cut_grad(u,ugrad,-1);
            cut_grad(u+2,ugrad+2,-1);
            switch ( WR(f_info->wraps[4],0) )
            { case POSWRAP: vol -= u[4][1]; ugrad[4][1] -= 1.0; break;
              case NEGWRAP: vol += u[4][1]; ugrad[4][1] += 1.0; break;
            }
            switch ( WR(f_info->wraps[0],0) )
            { case POSWRAP: vol += u[0][1]; ugrad[0][1] += 1.0;break;
              case NEGWRAP: vol -= u[0][1]; ugrad[0][1] -= 1.0;break;
            }
            break;
        case (NEGWRAP << TWRAPBITS) + (POSWRAP << (2*TWRAPBITS)):
            vol -= cut_int(u+2) + cut_int(u+4);
            cut_grad(u+2,ugrad+2,-1);
            cut_grad(u+4,ugrad+4,-1);
            switch ( WR(f_info->wraps[2],0) )
            { case POSWRAP: vol += u[2][1]; ugrad[2][1] += 1.0; break;
              case NEGWRAP: vol -= u[2][1]; ugrad[2][1] -= 1.0; break;
            }
            break;
         default: kb_error(1332,"Internal error: Bad allwrap in facet_volume.\n",RECOVERABLE);

     }
  vol *= web.torusv;
  for ( i = 0 ; i < FACET_CTRL ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
       ugrad[i][j] *= web.torusv;
  mat_mult(ugrad,dx,f_info->grad,FACET_CTRL,SDIM,SDIM);
 
  return vol;
}


/**********************************************************************
*
*  function: q_facet_torus_volume_q_hess()
*
*  purpose: gradient and value of volume integral on quadratic facet
*/

REAL q_facet_torus_volume_q_hess(f_info)
struct qinfo *f_info;
{ REAL **x;
  REAL vol;
  int i,j,k;
  REAL **dx = web.inverse_periods;
  WRAPTYPE w[FACET_EDGES];
  unsigned long allwrap;
  MAT2D(u,FACET_CTRL+1,MAXCOORD); /* affine coordinates of vertices */
  MAT2D(ugrad,FACET_CTRL+1,MAXCOORD);
  facetedge_id fe;
  int ii,jj;
  MAT4D(h,FACET_CTRL+1,FACET_CTRL+1,MAXCOORD,MAXCOORD);
  MAT2D(temph,MAXCOORD,MAXCOORD);
  REAL ***hh[FACET_CTRL+1];

  x = f_info->x;

  /* get affine coordinates of vertices */
  mat_mul_tr(x,dx,u,FACET_CTRL,SDIM,SDIM);
  u[FACET_CTRL] = u[0]; /* handy wrap around */
  ugrad[FACET_CTRL] = ugrad[0]; /* handy wrap around */
  h[FACET_CTRL] = h[0];
  for ( i = 0 ; i < FACET_CTRL ; i++ ) h[i][FACET_CTRL] = h[i][0];

  /* main integral over facet */

  /* volume, integral of z dx dy */
  vol = 0.0;
  for ( i = 0 ; i < FACET_CTRL ; i++ )
     for ( j = 0 ; j < FACET_CTRL ; j++ )
        for ( k = 0 ; k < FACET_CTRL ; k++ )
        { REAL v = vcoeff[i][j][k];
          if ( v == 0.0 ) continue;
          vol += v*u[i][2]*(u[j][0]*u[k][1]-u[j][1]*u[k][0]);
        }

  for ( i = 0 ; i < FACET_CTRL ; i++ )
         for ( j = 0 ; j < SDIM ; j++ ) 
              ugrad[i][j] = 0.0;

  for ( i = 0 ; i < FACET_CTRL ; i++ )
     for ( ii = 0 ; ii < FACET_CTRL ; ii++ )
         for ( j = 0 ; j < SDIM ; j++ ) 
            for ( jj = 0 ; jj < SDIM ; jj++ ) 
              h[i][ii][j][jj] = 0.0;

  for ( i = 0 ; i < FACET_CTRL ; i++ )
    for ( j = 0 ; j < FACET_CTRL ; j++ )
     for ( k = 0 ; k < FACET_CTRL ; k++ )
     { REAL v = vcoeff[i][j][k];
        if ( v == 0.0 ) continue;
        ugrad[i][2] += v*(u[j][0]*u[k][1]-u[j][1]*u[k][0]);
        ugrad[j][0] += v*u[i][2]*u[k][1];
        ugrad[k][1] += v*u[i][2]*u[j][0];
        ugrad[j][1] -= v*u[i][2]*u[k][0];
        ugrad[k][0] -= v*u[i][2]*u[j][1];
        h[i][j][2][0] += v*u[k][1];
        h[i][k][2][1] += v*u[j][0];
        h[i][j][2][1] -= v*u[k][0];
        h[i][k][2][0] -= v*u[j][1];
        h[j][i][0][2] += v*u[k][1];
        h[j][k][0][1] += v*u[i][2];
        h[k][i][1][2] += v*u[j][0];
        h[k][j][1][0] += v*u[i][2];
        h[j][i][1][2] -= v*u[k][0];
        h[j][k][1][0] -= v*u[i][2];
        h[k][i][0][2] -= v*u[j][1];
        h[k][j][0][1] -= v*u[i][2];
     }

  /* add corrections due to wraps */
  fe = get_facet_fe(f_info->id);
  for ( i = 0 ; i < FACET_EDGES ; i++, fe = get_next_edge(fe) ) 
     w[i] = get_fe_wrap(fe);

  allwrap = WR(w[0],2)+(WR(w[1],2)<<TWRAPBITS)+(WR(w[2],2)<<(2*TWRAPBITS));
  switch ( allwrap )
     { case 0: break;
        case (POSWRAP)+(NEGWRAP << TWRAPBITS): break;
        case (POSWRAP << TWRAPBITS) + (NEGWRAP << (2*TWRAPBITS)): break; 
        case (NEGWRAP) + (POSWRAP << (2*TWRAPBITS)):
            vol -= cut_int(u) + cut_int(u+2) + cut_int(u+4);
            for ( i = 0 ; i < EDGE_CTRL ; i++ ) hh[i] = h[i];
            cut_hess(u,ugrad,hh,-1); 
            for ( i = 0 ; i < EDGE_CTRL ; i++ ) hh[i] = h[i+2]+2;
            cut_hess(u+2,ugrad+2,hh,-1); 
            for ( i = 0 ; i < EDGE_CTRL ; i++ ) hh[i] = h[i+4]+4;
            cut_hess(u+4,ugrad+4,hh,-1);
            break;

        case (POSWRAP ) + (NEGWRAP << (2*TWRAPBITS)):
            vol += cut_int(u+2);
            for ( i = 0 ; i < EDGE_CTRL ; i++ ) hh[i] = h[i+2]+2;
            cut_hess(u+2,ugrad+2,hh,1); 
            switch ( WR(f_info->wraps[2],0) )
            { case POSWRAP: vol -= u[2][1]; ugrad[2][1] -= 1.0; break;
              case NEGWRAP: vol += u[2][1]; ugrad[2][1] += 1.0; break;
            }
            switch ( WR(f_info->wraps[4],0) )
            { case POSWRAP: vol += u[4][1]; ugrad[4][1] += 1.0; break;
              case NEGWRAP: vol -= u[4][1]; ugrad[4][1] -= 1.0; break;
            }
            break;
        case  (NEGWRAP) + (POSWRAP << TWRAPBITS):
            vol -= cut_int(u) + cut_int(u+2);
            for ( i = 0 ; i < EDGE_CTRL ; i++ ) hh[i] = h[i];
            cut_hess(u,ugrad,hh,-1); 
            for ( i = 0 ; i < EDGE_CTRL ; i++ ) hh[i] = h[i+2]+2;
            cut_hess(u+2,ugrad+2,hh,-1); 
            switch ( WR(f_info->wraps[4],0) )
            { case POSWRAP: vol -= u[4][1]; ugrad[4][1] -= 1.0; break;
              case NEGWRAP: vol += u[4][1]; ugrad[4][1] += 1.0; break;
            }
            switch ( WR(f_info->wraps[0],0) )
            { case POSWRAP: vol += u[0][1]; ugrad[0][1] += 1.0;break;
              case NEGWRAP: vol -= u[0][1]; ugrad[0][1] -= 1.0;break;
            }
            break;
        case (NEGWRAP << TWRAPBITS) + (POSWRAP << (2*TWRAPBITS)):
            vol -= cut_int(u+2) + cut_int(u+4);
            for ( i = 0 ; i < EDGE_CTRL ; i++ ) hh[i] = h[i+2]+2;
            cut_hess(u+2,ugrad+2,hh,-1); 
            for ( i = 0 ; i < EDGE_CTRL ; i++ ) hh[i] = h[i+4]+4;
            cut_hess(u+4,ugrad+4,hh,-1);
            switch ( WR(f_info->wraps[2],0) )
            { case POSWRAP: vol += u[2][1]; ugrad[2][1] += 1.0; break;
              case NEGWRAP: vol -= u[2][1]; ugrad[2][1] -= 1.0; break;
            }
            break;
         default: kb_error(1333,
            "Internal error: Bad allwrap in facet_volume.\n",RECOVERABLE);

     }
  vol *= web.torusv;
  for ( i = 0 ; i < FACET_CTRL ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
       ugrad[i][j] *= web.torusv;
  mat_mult(ugrad,dx,f_info->grad,FACET_CTRL,SDIM,SDIM);
  for ( i = 0 ; i < FACET_CTRL ; i++ )
     for ( ii = 0 ; ii < FACET_CTRL ; ii++ )
     { for ( j = 0 ; j < SDIM ; j++ ) 
          for ( jj = 0 ; jj < SDIM ; jj++ ) 
             h[i][ii][j][jj] *= web.torusv;
       mat_mult(h[i][ii],dx,temph,SDIM,SDIM,SDIM);
       tr_mat_mul(dx,temph,f_info->hess[i][ii],SDIM,SDIM,SDIM);
     }
 
  return vol;
}


/**********************************************************************
                    Lagrange torus volume quantity
**********************************************************************/

REAL lagr_cut_int ARGS((REAL**));
void lagr_cut_grad ARGS((REAL**,REAL**,int));
void lagr_cut_hess ARGS((REAL**,REAL**,REAL****,int));

REAL lagr_cut_int(u)
REAL **u; /* coefficients of the vertices along edge */
{ 
  REAL area;
  struct gauss_lag *gl = &gauss_lagrange[1][web.gauss1D_order];
  int ctrl = web.skel[EDGE].ctrlpts;
  int m,k;

  /* main integral over edge */
  area = 0.0;
  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL dy,x;
    for ( x = 0.0, dy = 0.0, k = 0 ; k < ctrl ; k++ )
    { x += gl->gpoly[m][k]*u[k][0];
      dy += gl->gpolypart[m][0][k]*u[k][1];
    }
    area -= gl->gausswt[m]*x*dy;
  }
  return area;
}

void lagr_cut_grad(u,ugrad,sign)
REAL **u;
REAL **ugrad; /* gradients to increment */
int sign;  /* +1 or -1 */
{
  int m,k;
  struct gauss_lag *gl = &gauss_lagrange[1][web.gauss1D_order];
  int ctrl = web.skel[EDGE].ctrlpts;

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL x,dy;
    for ( x = 0.0, dy = 0.0, k = 0 ; k < ctrl ; k++ )
    { x += gl->gpoly[m][k]*u[k][0];
      dy += gl->gpolypart[m][0][k]*u[k][1];
    }
    for ( k = 0 ; k < ctrl ; k++ )
    { ugrad[k][1] -= sign*gl->gausswt[m]*x*gl->gpolypart[m][0][k];
      ugrad[k][0] -= sign*gl->gausswt[m]*dy*gl->gpoly[m][k];
    }
  }
}

void lagr_cut_hess(u,ugrad,uhess,sign)
REAL **u;
REAL **ugrad; /* gradients to increment */
REAL ****uhess; /* hessians to increment */
int sign;  /* +1 or -1 */
{
  int m,k,kk;
  struct gauss_lag *gl = &gauss_lagrange[1][web.gauss1D_order];
  int ctrl = web.skel[EDGE].ctrlpts;
  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL x,dy;
    REAL wt = sign*gl->gausswt[m];
    for ( x = 0.0, dy = 0.0, k = 0 ; k < ctrl ; k++ )
    { x += gl->gpoly[m][k]*u[k][0];
      dy += gl->gpolypart[m][0][k]*u[k][1];
    }
    for ( k = 0 ; k < ctrl ; k++ )
    { ugrad[k][1] -= wt*x*gl->gpolypart[m][0][k];
      ugrad[k][0] -= wt*dy*gl->gpoly[m][k];
      for ( kk = 0 ; kk < ctrl ; kk++ )
      { uhess[k][kk][1][0] -= wt*gl->gpoly[m][kk]*gl->gpolypart[m][0][k];
        uhess[k][kk][0][1] -= wt*gl->gpolypart[m][0][kk]*gl->gpoly[m][k];
      }
    }
  }

}
/**********************************************************************
*
*  function: q_facet_torus_volume_lagr()
*
*  purpose: value of volume integral on facet, quadratic
*/

REAL q_facet_torus_volume_lagr(f_info)
struct qinfo *f_info;
{ REAL **x;
  REAL vol;
  int i,m;
  int ctrl = web.skel[FACET].ctrlpts;
  REAL **dx = web.inverse_periods;
  WRAPTYPE w[FACET_EDGES];
  unsigned long allwrap;
  REAL **u = f_info->u;
  facetedge_id fe;
  MAT2D(mat,MAXCOORD,MAXCOORD);
  int dim = web.dimension;
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];
  REAL ***uu = f_info->uu;  /* side vertex coords */
  int order = web.lagrange_order;

  x = f_info->x;

  /* get affine coordinates of vertices */
  mat_mul_tr(x,dx,u,ctrl,SDIM,SDIM);

  for ( i = 0 ; i <= web.lagrange_order ; i++ )
  { uu[0][i] = u[i];
    uu[1][i] = u[(i+1)*order - (i-1)*i/2];
    uu[2][web.lagrange_order-i] = u[i*(order+1) - i*(i-1)/2];
  }

  /* main integral over facet */

  /* volume, integral of z dx dy */
  vol = 0.0;
  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL z;
    mat_mult(gl->gpolypart[m],u,mat,dim,ctrl,SDIM);
    for ( z = 0.0, i = 0 ; i < ctrl ; i++ ) z += gl->gpoly[m][i]*u[i][dim];
    vol += gl->gausswt[m]*det_adjoint(mat,dim)*z;
  }
  vol *= sign/factorial[dim];


  /* add corrections due to wraps */
  fe = get_facet_fe(f_info->id);
  for ( i = 0 ; i < FACET_EDGES ; i++, fe = get_next_edge(fe) ) 
    w[i] = get_fe_wrap(fe);

  allwrap = WR(w[0],2)+(WR(w[1],2)<<TWRAPBITS)+(WR(w[2],2)<<(2*TWRAPBITS));
  switch ( allwrap )
     { case 0: break;
       case (POSWRAP)+(NEGWRAP << TWRAPBITS): break;
       case (POSWRAP << TWRAPBITS) + (NEGWRAP << (2*TWRAPBITS)): break; 
       case (NEGWRAP) + (POSWRAP << (2*TWRAPBITS)):
            vol -= lagr_cut_int(uu[0]);
            vol -= lagr_cut_int(uu[1]);
            vol -= lagr_cut_int(uu[2]);
            break;
       case (POSWRAP ) + (NEGWRAP << (2*TWRAPBITS)):
            vol += lagr_cut_int(uu[1]);
            switch ( WR(f_info->wraps[web.lagrange_order],0) )
            { case POSWRAP: vol -= uu[1][0][1];break;
              case NEGWRAP: vol += uu[1][0][1];break;
            }
            switch ( WR(f_info->wraps[ctrl-1],0) )
            { case POSWRAP: vol += uu[2][0][1]; break;
              case NEGWRAP: vol -= uu[2][0][1]; break;
            }
            break;
       case  (NEGWRAP) + (POSWRAP << TWRAPBITS):
            vol -= lagr_cut_int(uu[0]);
            vol -= lagr_cut_int(uu[1]);
            switch ( WR(f_info->wraps[ctrl-1],0) )
            { case POSWRAP: vol -= uu[2][0][1]; break;
              case NEGWRAP: vol += uu[2][0][1]; break;
            }
            switch ( WR(f_info->wraps[0],0) )
            { case POSWRAP: vol += uu[0][0][1]; break;
              case NEGWRAP: vol -= uu[0][0][1]; break;
            }
            break;
       case (NEGWRAP << TWRAPBITS) + (POSWRAP << (2*TWRAPBITS)):
            vol -= lagr_cut_int(uu[1]);
            vol -= lagr_cut_int(uu[2]);
            switch ( WR(f_info->wraps[web.lagrange_order],0) )
            { case POSWRAP: vol += uu[1][0][1]; break;
              case NEGWRAP: vol -= uu[1][0][1]; break;
            }
            break;
         default: kb_error(1334,"Internal error: Bad allwrap in facet_volume.\n",RECOVERABLE);

     }
  vol *= web.torusv;
 
  return vol;
}

/**********************************************************************
*
*  function: q_facet_torus_volume_lagr_grad()
*
*  purpose: gradient and value of volume integral on quadratic facet
*/

REAL q_facet_torus_volume_lagr_grad(f_info)
struct qinfo *f_info;
{ REAL **x;
  REAL vol;
  int i,j,k;
  REAL **dx = web.inverse_periods;
  WRAPTYPE w[FACET_EDGES];
  unsigned long allwrap;
  facetedge_id fe;
  REAL **ugrad = f_info->ugrad;
  REAL ***uu = f_info->uu;  /* side vertex coords */
  REAL ***uugrad = f_info->uugrad;  /* side vertex coords */
  REAL **u = f_info->u;
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  int order = web.lagrange_order;
  int m;
  int dim = web.dimension;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];
  MAT2D(mat,MAXCOORD,MAXCOORD);
  REAL z;
  int ctrl = web.skel[FACET].ctrlpts;

  x = f_info->x;

  for ( i = 0 ; i < ctrl ; i++ )
    for ( j = 0 ; j < SDIM; j++ )
      ugrad[i][j] = 0.0;

  /* get affine coordinates of vertices */
  mat_mul_tr(x,dx,u,ctrl,SDIM,SDIM);
  for ( i = 0 ; i <= web.lagrange_order ; i++ )
  { uu[0][i] = u[i];
    uugrad[0][i] = ugrad[i];

    uu[1][i] = u[(i+1)*order - (i-1)*i/2];
    uugrad[1][i] = ugrad[(i+1)*order - (i-1)*i/2];

    uu[2][web.lagrange_order-i] = u[i*(order+1) - i*(i-1)/2];
    uugrad[2][web.lagrange_order-i] = ugrad[i*(order+1) - i*(i-1)/2];
  }

  /* main integral over facet */
  /* volume, integral of z dx dy */
  vol = 0.0;
  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL weight = sign* gl->gausswt[m]/factorial[dim];
    REAL det;
    mat_mult(gl->gpolypart[m],u,mat,dim,ctrl,SDIM);
    for ( z = 0.0, i = 0 ; i < ctrl ; i++ ) z += gl->gpoly[m][i]*u[i][dim];
    det = det_adjoint(mat,dim);
    vol += weight*det*z;
    for ( k = 0 ; k < gl->lagpts; k++ )
      for ( j = 0 ; j < dim ; j++ )
        for ( i = 0 ; i < dim ; i++ )
          ugrad[k][j] += weight*z*gl->gpolypart[m][i][k]*mat[j][i];
    for ( k = 0 ; k < gl->lagpts ; k++ )
        ugrad[k][dim] += weight*gl->gpoly[m][k]*det;
  }

  /* add corrections due to wraps */
  fe = get_facet_fe(f_info->id);
  for ( i = 0 ; i < FACET_EDGES ; i++, fe = get_next_edge(fe) ) 
     w[i] = get_fe_wrap(fe);

  allwrap = WR(w[0],2)+(WR(w[1],2)<<TWRAPBITS)+(WR(w[2],2)<<(2*TWRAPBITS));
  switch ( allwrap )
     { case 0: break;
       case (POSWRAP)+(NEGWRAP << TWRAPBITS): break;
       case (POSWRAP << TWRAPBITS) + (NEGWRAP << (2*TWRAPBITS)): break; 
       case (NEGWRAP) + (POSWRAP << (2*TWRAPBITS)):
            vol -= lagr_cut_int(uu[0])+lagr_cut_int(uu[1])+lagr_cut_int(uu[2]);
            lagr_cut_grad(uu[0],uugrad[0],-1); 
            lagr_cut_grad(uu[1],uugrad[1],-1); 
            lagr_cut_grad(uu[2],uugrad[2],-1);
            break;

       case (POSWRAP ) + (NEGWRAP << (2*TWRAPBITS)):
            vol += lagr_cut_int(uu[1]);
            lagr_cut_grad(uu[1],uugrad[1],1);
            switch ( WR(f_info->wraps[order],0) )
            { case POSWRAP: vol -= uu[1][0][1]; uugrad[1][0][1] -= 1.0; break;
              case NEGWRAP: vol += uu[1][0][1]; uugrad[1][0][1] += 1.0; break;
            }
            switch ( WR(f_info->wraps[ctrl-1],0) )
            { case POSWRAP: vol += uu[2][0][1]; uugrad[2][0][1] += 1.0; break;
              case NEGWRAP: vol -= uu[2][0][1]; uugrad[2][0][1] -= 1.0; break;
            }
            break;
        case  (NEGWRAP) + (POSWRAP << TWRAPBITS):
            vol -= lagr_cut_int(uu[0]) + lagr_cut_int(uu[1]);
            lagr_cut_grad(uu[0],uugrad[0],-1);
            lagr_cut_grad(uu[1],uugrad[1],-1);
            switch ( WR(f_info->wraps[ctrl-1],0) )
            { case POSWRAP: vol -= uu[2][0][1]; uugrad[2][0][1] -= 1.0; break;
              case NEGWRAP: vol += uu[2][0][1]; uugrad[2][0][1] += 1.0; break;
            }
            switch ( WR(f_info->wraps[0],0) )
            { case POSWRAP: vol += uu[0][0][1]; uugrad[0][0][1] += 1.0;break;
              case NEGWRAP: vol -= uu[0][0][1]; uugrad[0][0][1] -= 1.0;break;
            }
            break;
        case (NEGWRAP << TWRAPBITS) + (POSWRAP << (2*TWRAPBITS)):
            vol -= lagr_cut_int(uu[1]) + lagr_cut_int(uu[2]);
            lagr_cut_grad(uu[1],uugrad[1],-1);
            lagr_cut_grad(uu[2],uugrad[2],-1);
            switch ( WR(f_info->wraps[order],0) )
            { case POSWRAP: vol += uu[1][0][1]; uugrad[1][0][1] += 1.0; break;
              case NEGWRAP: vol -= uu[1][0][1]; uugrad[1][0][1] -= 1.0; break;
            }
            break;
         default: kb_error(1202,"Internal error: Bad allwrap in facet_volume.\n",RECOVERABLE);

     }
  vol *= web.torusv;
  for ( i = 0 ; i < ctrl ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
       ugrad[i][j] *= web.torusv;
  mat_mult(ugrad,dx,f_info->grad,ctrl,SDIM,SDIM);
 
  return vol;
}


/**********************************************************************
*
*  function: q_facet_torus_volume_lagr_hess()
*
*  purpose: gradient and value of volume integral on quadratic facet
*/

REAL q_facet_torus_volume_lagr_hess(f_info)
struct qinfo *f_info;
{ REAL **x;
  REAL vol;
  int i,j,k,kk,m;
  REAL **dx = web.inverse_periods;
  WRAPTYPE w[FACET_EDGES];
  unsigned long allwrap;
  facetedge_id fe;
  int ii,jj;
  MAT2D(temph,MAXCOORD,MAXCOORD);
  int ctrl = web.skel[FACET].ctrlpts;
  REAL **ugrad = f_info->ugrad;
  REAL ***uu = f_info->uu;  /* side vertex coords */
  REAL ***uugrad = f_info->uugrad;  /* side vertex grads */
  REAL *****uuhess = f_info->uuhess;  /* side vertex hessians */
  REAL ****uhess = f_info->uhess;  /* vertex hessians */
  int order = web.lagrange_order;
  REAL **u = f_info->u;
  MAT2D(mat,MAXCOORD,MAXCOORD);
  int dim = web.dimension;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];
  MAT4D(dethess,MAXCOORD,MAXCOORD,MAXCOORD,MAXCOORD);
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;

  x = f_info->x;

  for ( i = 0 ; i < ctrl ; i++ )
   for ( j = 0 ; j < SDIM ; j++ ) 
     ugrad[i][j] = 0.0;

  for ( i = 0 ; i < ctrl ; i++ )
     for ( ii = 0 ; ii < ctrl ; ii++ )
         for ( j = 0 ; j < SDIM ; j++ ) 
            for ( jj = 0 ; jj < SDIM ; jj++ ) 
              uhess[i][ii][j][jj] = 0.0;

  /* get affine coordinates of vertices */
  mat_mul_tr(x,dx,u,ctrl,SDIM,SDIM);

  /* set up edge pointers */
  for ( i = 0 ; i <= web.lagrange_order ; i++ )
  { uu[0][i] = u[i];
    uugrad[0][i] = ugrad[i];

    uu[1][i] = u[(i+1)*order - (i-1)*i/2];
    uugrad[1][i] = ugrad[(i+1)*order - (i-1)*i/2];

    uu[2][web.lagrange_order-i] = u[i*(order+1) - i*(i-1)/2];
    uugrad[2][web.lagrange_order-i] = ugrad[i*(order+1) - i*(i-1)/2];

    for ( j = 0 ; j <= web.lagrange_order ; j++ )
    { uuhess[0][i][j] = uhess[i][j];
      uuhess[1][i][j] = uhess[(i+1)*order-(i-1)*i/2][(j+1)*order-(j-1)*j/2];
      uuhess[2][order-i][order-j] = 
          uhess[i*(order+1) - i*(i-1)/2][j*(order+1) - j*(j-1)/2];
    }
  }

  /* main integral over facet, without wraps */
  for ( m = 0, vol = 0.0 ; m < gl->gnumpts ; m++ )
  { REAL det;
    REAL z,sum;
    REAL weight = sign*gl->gausswt[m]/factorial[dim];

    mat_mult(gl->gpolypart[m],u,mat,dim,ctrl,SDIM);
    det_hess(mat,dethess,dim);
    det = det_adjoint(mat,dim);
    for ( z = 0.0, i = 0 ; i < ctrl ; i++ ) z += gl->gpoly[m][i]*u[i][dim];

    vol += weight*det*z;
     
    /* gradient */
    for ( k = 0 ; k < gl->lagpts; k++ )
     for ( j = 0 ; j < dim ; j++ ) 
       for ( i = 0 ; i < dim ; i++ )
         ugrad[k][j] += weight*z*gl->gpolypart[m][i][k]*mat[j][i];
    for ( k = 0 ; k < gl->lagpts ; k++ )
        ugrad[k][dim] += weight*gl->gpoly[m][k]*det;
     
    /* hessian */
    for ( k = 0 ; k < gl->lagpts  ; k++ )
       for ( kk = 0 ; kk < gl->lagpts ; kk++ )
       { 
          if ( dim == 2 )
          {
             uhess[k][kk][0][1]  += weight*z*
                    (gl->gpolypart[m][0][k]*gl->gpolypart[m][1][kk]
                     - gl->gpolypart[m][1][k]*gl->gpolypart[m][0][kk]);
             uhess[k][kk][1][0]  += weight*z*
                    (gl->gpolypart[m][1][k]*gl->gpolypart[m][0][kk]
                     - gl->gpolypart[m][0][k]*gl->gpolypart[m][1][kk]);
          }
          else if ( dim > 2 )
          { for ( j = 0 ; j < dim ; j++ )
             { for ( jj = 0 ; jj < dim ; jj++ )
                  for ( i = 0 ; i < dim ; i++ )
                     for ( ii = 0 ; ii < dim ; ii++ )
                { 
                  uhess[k][kk][j][jj]  += weight*z*dethess[i][j][ii][jj]
                    *gl->gpolypart[m][i][k]*gl->gpolypart[m][ii][kk];
                }
             }
          }
          for ( j = 0 ; j < dim ; j++ )
          { for ( i = 0, sum = 0.0 ; i < dim ; i++ )
                  sum += gl->gpolypart[m][i][k]*mat[j][i];
             uhess[k][kk][j][dim] += weight*gl->gpoly[m][kk]*sum;
          }
          for ( jj = 0 ; jj < dim ; jj++ )
          { for ( ii = 0,sum = 0.0 ; ii < dim ; ii++ )
                sum +=  gl->gpolypart[m][ii][kk]*mat[jj][ii];
             uhess[k][kk][dim][jj] += weight*gl->gpoly[m][k]*sum;
          }
        }
  }

  /* add corrections due to wraps */
  fe = get_facet_fe(f_info->id);
  for ( i = 0 ; i < FACET_EDGES ; i++, fe = get_next_edge(fe) ) 
     w[i] = get_fe_wrap(fe);

  allwrap = WR(w[0],2)+(WR(w[1],2)<<TWRAPBITS)+(WR(w[2],2)<<(2*TWRAPBITS));

  switch ( allwrap )
     { case 0: break;
       case (POSWRAP)+(NEGWRAP << TWRAPBITS): break;
       case (POSWRAP << TWRAPBITS) + (NEGWRAP << (2*TWRAPBITS)): break; 
       case (NEGWRAP) + (POSWRAP << (2*TWRAPBITS)):
            vol -= lagr_cut_int(uu[0])+lagr_cut_int(uu[1])+lagr_cut_int(uu[2]);
            lagr_cut_hess(uu[0],uugrad[0],uuhess[0],-1); 
            lagr_cut_hess(uu[1],uugrad[1],uuhess[1],-1); 
            lagr_cut_hess(uu[2],uugrad[2],uuhess[2],-1);
            break;

       case (POSWRAP ) + (NEGWRAP << (2*TWRAPBITS)):
            vol += lagr_cut_int(uu[1]);
            lagr_cut_hess(uu[1],uugrad[1],uuhess[1],1);
            switch ( WR(f_info->wraps[order],0) )
            { case POSWRAP: vol -= uu[1][0][1]; uugrad[1][0][1] -= 1.0; break;
              case NEGWRAP: vol += uu[1][0][1]; uugrad[1][0][1] += 1.0; break;
            }
          switch ( WR(f_info->wraps[ctrl-1],0) )
            { case POSWRAP: vol += uu[2][0][1]; uugrad[2][0][1] += 1.0; break;
              case NEGWRAP: vol -= uu[2][0][1]; uugrad[2][0][1] -= 1.0; break;
            }
            break;
       case  (NEGWRAP) + (POSWRAP << TWRAPBITS):
            vol -= lagr_cut_int(uu[0]) + lagr_cut_int(uu[1]);
            lagr_cut_hess(uu[0],uugrad[0],uuhess[0],-1);
            lagr_cut_hess(uu[1],uugrad[1],uuhess[1],-1);
            switch ( WR(f_info->wraps[ctrl-1],0) )
            { case POSWRAP: vol -= uu[2][0][1]; uugrad[2][0][1] -= 1.0; break;
              case NEGWRAP: vol += uu[2][0][1]; uugrad[2][0][1] += 1.0; break;
            }
            switch ( WR(f_info->wraps[0],0) )
            { case POSWRAP: vol += uu[0][0][1]; uugrad[0][0][1] += 1.0;break;
              case NEGWRAP: vol -= uu[0][0][1]; uugrad[0][0][1] -= 1.0;break;
            }
            break;
       case (NEGWRAP << TWRAPBITS) + (POSWRAP << (2*TWRAPBITS)):
            vol -= lagr_cut_int(uu[1]) + lagr_cut_int(uu[2]);
            lagr_cut_hess(uu[1],uugrad[1],uuhess[1],-1);
            lagr_cut_hess(uu[2],uugrad[2],uuhess[2],-1);
            switch ( WR(f_info->wraps[order],0) )
            { case POSWRAP: vol += uu[1][0][1]; uugrad[1][0][1] += 1.0; break;
              case NEGWRAP: vol -= uu[1][0][1]; uugrad[1][0][1] -= 1.0; break;
            }
            break;
       default: kb_error(1335,"Internal error: Bad allwrap in facet_volume.\n",
               RECOVERABLE);
     }

  /* pull back gradient and hessian to Euclidean coords */
  vol *= web.torusv;
  for ( i = 0 ; i < ctrl ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
       ugrad[i][j] *= web.torusv;
  mat_mult(ugrad,dx,f_info->grad,ctrl,SDIM,SDIM);
  for ( i = 0 ; i < ctrl ; i++ )
     for ( ii = 0 ; ii < ctrl ; ii++ )
     { for ( j = 0 ; j < SDIM ; j++ ) 
          for ( jj = 0 ; jj < SDIM ; jj++ ) 
          { uhess[i][ii][j][jj] *= web.torusv;
           }
       mat_mult(uhess[i][ii],dx,temph,SDIM,SDIM,SDIM);
       tr_mat_mul(dx,temph,f_info->hess[i][ii],SDIM,SDIM,SDIM);
     }
 
  return vol;
}

