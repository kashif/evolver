/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/**********************************************************
*
*    File:     trirevise.c
*
*    Purpose: Contains routines for modifying surface
*                triangulations. 
*
*/

#include "include.h"

/************************************************************************
*
* function: refine()
*
* purpose: wrapper for local_refine()
*/

void refine()
{
  #ifdef MPI_EVOLVER
  mpi_refine();
  #else
  local_refine();
  #endif
}

/****************************************************************
*
*  Function: local_refine()
*
*  Purpose:  Refines current triangulation by subdividing each
*                triangulation into four congruent triangles.
*/

void local_refine()     /* REAL the resolution of triangulation */
{
  vertex_id v_id;
  edge_id e_id;
  element_id sentinel;
  facet_id f_id;
  MAT2D(vx,FACET_CTRL,MAXCOORD);
  REAL **interp=NULL,**oldx=NULL,**oldp=NULL;
  facet_id newf[FACET_EDGES];
  vertex_id allv[MAXLAGRANGE+1][MAXLAGRANGE+1];
  WRAPTYPE wraps[MAXLAGRANGE+1][MAXLAGRANGE+1];
  WRAPTYPE w2=0,w3=0,w4=0,w5=0,w6=0;
  int N = web.lagrange_order;
  int type;
 
  /* check impediments to Lagrange model refining */
  if ( web.modeltype == LAGRANGE )
  { if ( web.representation == SIMPLEX) 
    kb_error(1338,"No refining in simplex Lagrange model yet.\n",RECOVERABLE );
    if ( web.representation == SOAPFILM )
    { FOR_ALL_EDGES(e_id)
      { int attr = get_eattr(e_id);
        if ( (attr & NO_REFINE) && !(attr & BARE_EDGE) )
          kb_error(2195,
           "Can't refine in Lagrange model with no_refine edge yet.\n",
              RECOVERABLE);
      }
    }
  }

  web.vol_flag = 0;
  top_timestamp = ++global_timestamp;

  /* clean out NEW flags */
  MFOR_ALL_VERTICES(v_id)
     unset_attr(v_id,NEWVERTEX);
  MFOR_ALL_EDGES(e_id)
     unset_attr(e_id,NEWEDGE);
  MFOR_ALL_FACETS(f_id)
     unset_attr(f_id,NEWFACET);

  if ( web.modeltype == LAGRANGE )
  { /* save in oldcoord attribute */
    MFOR_ALL_VERTICES(v_id)
    { if ( get_vattr(v_id) & BOUNDARY )
        memcpy((char *)(get_force(v_id)),(char *)get_param(v_id),
                                           sizeof(REAL)*web.maxparam);
  
        memcpy((char *)(get_oldcoord(v_id)),(char *)get_coord(v_id),
                                           sizeof(REAL)*SDIM);
    }
  }

  /* allocate room for new elements */
  for ( type = VERTEX ; type <= FACETEDGE ; type++ )
   extend(type,EXTEND_FOR_REFINE);

  if ( web.representation == SIMPLEX ) 
  { refine_all_simplices(); 
    if ( web.dimension > 2 )
         web.maxscale *= 1 << (web.dimension-2);
    return;
  }

  ENTER_GRAPH_MUTEX;

  /* first, subdivide all edges */
  e_id = NULLEDGE;
  while ( generate_all(EDGE,&e_id,&sentinel) )
  { if ( get_eattr(e_id) & (NEWEDGE|NO_REFINE) ) continue;
    edge_divide(e_id);
  }

  #ifdef MPI_EVOLVER
  mpi_refine_edge_synch();
  #endif

  if ( web.representation == STRING ) goto windup; 
  /* don't want to subdivide facets */
     
  if ( web.modeltype == LAGRANGE )
  { /* interpolation matrix */
    int newctrl = (2*N+1)*(N+1);
    int oldctrl = web.skel[FACET].ctrlpts;
    int p,q,i,j,k,a,b,c;
     
    oldx = dmatrix(0,oldctrl,0,SDIM);
    oldp = dmatrix(0,oldctrl,0,SDIM);
    if ( bezier_flag )
      interp = bezier_refine_2d[N]; 
    else /* regular lagrange */
    { interp = dmatrix(0,newctrl,0,oldctrl);
      for ( p = 1 ; p < 2*N ; p++ ) 
        for ( q = 1 ; p+q < 2*N ; q++ ) 
        { REAL x,y,z,prod;
          x = p/2.; y = q/2.; z = N - x - y;
          for ( i = 0 ; i <= N ; i++ )
            for ( j = 0 ; i+j <= N ; j++ )
            { k = N - i - j;
              prod = 1.0;
              for ( a = 0 ; a < i ; a++ ) prod *= (x-a)/(REAL)(i-a);
              for ( b = 0 ; b < j ; b++ ) prod *= (y-b)/(REAL)(j-b);
              for ( c = 0 ; c < k ; c++ ) prod *= (z-c)/(REAL)(k-c);
              interp[p+2*N*q+(3*q-q*q)/2][i+j*N+(3*j-j*j)/2] = prod;
            }
        }
    }
  }
  /* now subdivide each facet */
  f_id = NULLFACET;
  while ( generate_all(FACET,&f_id,&sentinel) )
  { facetedge_id fe;
    facetedge_id next_fe;
    int i,j,k,n;
    REAL *x[2*FACET_CTRL],*xmid;

    if ( get_fattr(f_id) & NEWFACET ) continue;
    newf[0] = newf[1] = newf[2] = NULLFACET;

    if ( web.modeltype == QUADRATIC )
    { /* will need old vertex coords for quadratic corrections */
      int wrap,fewrap;
      fe = get_facet_fe(f_id);
      if ( inverted(get_fe_edge(fe)) ) fe = get_prev_edge(fe);
      for ( i = 0,wrap=0 ; i < 6 ; i++ )
      {
         if ( get_eattr(get_fe_edge(fe)) & NO_REFINE )
         { 
            e_id = get_fe_edge(fe);
            if ( web.symmetry_flag )
            { REAL *y = get_coord(get_fe_tailv(fe));
              (*sym_wrap)(y,vx[i],wrap);
              x[i] = x[i+6] = vx[i];
              fewrap = get_edge_wrap(e_id);
              if ( inverted(e_id) ) wrap = (*sym_compose)(wrap,fewrap);
              y = get_coord(get_fe_midv(fe));
              (*sym_wrap)(y,vx[i],wrap);
              x[i] = x[i+6] = vx[i];
              if ( !inverted(e_id) ) wrap = (*sym_compose)(wrap,fewrap);
            }
            else
            { x[i] = x[i+6] = get_coord(get_fe_tailv(fe));
              x[i+1] = x[i+1+6] = get_coord(get_fe_midv(fe));
            }
            i++;
         }
         else
         {
            if ( web.symmetry_flag )
            { REAL *y = get_coord(get_fe_tailv(fe));
              (*sym_wrap)(y,vx[i],wrap);
              fewrap = get_fe_wrap(fe);
              wrap = (*sym_compose)(wrap,fewrap);
              x[i] = x[i+6] = vx[i];
            }
            else x[i] = x[i+6] = get_coord(get_fe_tailv(fe));
         }
         fe = get_next_edge(fe);
      }
    }
    else if ( web.modeltype == LAGRANGE )
    { 
      if (  web.symmetry_flag )
      { /* get some handy wraps */
        fe = get_facet_fe(f_id);
        e_id = get_fe_edge(fe);
        if ( inverted(e_id) ) fe = get_prev_edge(fe);
        w4 = get_fe_wrap(fe);
        fe = get_next_edge(fe);
        w2 = (*sym_compose)(w4,get_fe_wrap(fe));
        fe = get_next_edge(fe);
        w5 = (*sym_compose)(w2,get_fe_wrap(fe));
        fe = get_next_edge(fe);
        w3 = (*sym_compose)(w5,get_fe_wrap(fe));
        fe = get_next_edge(fe);
        w6 = (*sym_compose)(w3,get_fe_wrap(fe));
      }
    }

    /* walk around subdivided edges, cutting off points as new facets */
    fe = get_facet_fe(f_id);
    if ( inverted(get_fe_edge(fe)) )
        fe = get_prev_edge(fe);  
    if ( get_eattr(get_fe_edge(get_prev_edge(fe))) & NO_REFINE ) i = 1;
    else i = 0;
    for (  n = 0 ; i < FACET_EDGES ; n++  )
    { vertex_id headv,midv;
      edge_id next_e;

      next_fe = get_next_edge(fe);
      headv = get_fe_headv(fe);
      if ( get_vattr(headv) & NEWVERTEX )
      { cross_cut(get_prev_edge(fe),fe);
        if ( web.modeltype == QUADRATIC )
        {
          /* add quadratic correction to linear interpolation */
          next_e = get_next_edge(fe);
          midv = get_fe_midv(next_e);
          xmid = get_coord(midv);
          for ( j = 0 ; j < SDIM ; j++ )
            xmid[j] -= (x[n+2][j] - 2*x[n+3][j] + x[n+4][j])/8.0;
        }
        newf[i] = get_fe_facet(fe);
        i++;
      }
      if ( get_eattr(get_fe_edge(fe)) & NO_REFINE ) {n++,i++;}
      fe = next_fe;
    }

    if ( web.modeltype == LAGRANGE )
    { /* fill in allv array with all vertices needed */
      int oldctrl = web.skel[FACET].ctrlpts;
      vertex_id vv_id, *v,*v0,*v1,*v2,*v3;
      int facet_bdry_flag = get_fattr(f_id) & BOUNDARY;

      for ( i = 0 ; i <= 2*N ; i++ )
         for ( j = 0 ; i+j <= 2*N ; j++ )
            allv[i][j] = NULLID; 

      /* use old interior vertices, in same positions */
      v = get_facet_vertices(f_id);
      for ( i = 1 ; i < N ; i++)
         for ( j = 1; i+j < N ; j++ )
            allv[2*j][2*i] = v[i+j*N+(3*j-j*j)/2];

      /* now extract from various edges */
      fe = get_facet_fe(newf[0]);
      e_id = get_fe_edge(fe);
      v = get_edge_vertices(e_id);
      if ( inverted(e_id) )
         for ( i = 0 ; i <= N ; i++ )
            allv[0][N-i] = v[i];
      else
         for ( i = 0 ; i <= N ; i++ )
            allv[0][i] = v[i];

      fe = get_next_edge(fe);  /* crosscut edge */
      e_id = get_fe_edge(fe);
      v = get_edge_vertices(e_id);
      if ( inverted(e_id) )
         for ( i = 1 ; i < N ; i++ )
         { vv_id = allv[N-i][i];
           if ( valid_id(vv_id) )  /* use old vertex */
           { unset_attr(vv_id,Q_MIDFACET);
             set_attr(vv_id,Q_MIDEDGE);
             set_vertex_edge(vv_id,e_id);
             free_element(v[i]);
             v[i] = vv_id;
           }
           else
             allv[N-i][i] = v[i];
         }
      else
         for ( i = 1 ; i < N ; i++ )
         { vv_id = allv[i][N-i];
           if ( valid_id(vv_id) )  /* use old vertex */
           { unset_attr(vv_id,Q_MIDFACET);
             set_attr(vv_id,Q_MIDEDGE);
             set_vertex_edge(vv_id,e_id);
             free_element(v[i]);
             v[i] = vv_id;
           }
           else
             allv[i][N-i] = v[i];
         }

      fe = get_next_edge(fe);
      e_id = get_fe_edge(fe);
      v = get_edge_vertices(e_id);
      if ( inverted(e_id) )
         for ( i = 0 ; i <= N ; i++ )
            allv[i][0] = v[i];
      else
         for ( i = 0 ; i <= N ; i++ )
            allv[N-i][0] = v[i];

      fe = get_facet_fe(newf[1]);
      e_id = get_fe_edge(fe);
      v = get_edge_vertices(e_id);
      if ( inverted(e_id) )
         for ( i = 0 ; i <= N ; i++ )
            allv[N-i][N+i] = v[i];
      else
         for ( i = 0 ; i <= N ; i++ )
            allv[i][2*N-i] = v[i];

      fe = get_next_edge(fe);  /* crosscut edge */
      e_id = get_fe_edge(fe);
      v = get_edge_vertices(e_id);
      if ( inverted(e_id) )
         for ( i = 1 ; i < N ; i++ )
         { vv_id = allv[i][N];
           if ( valid_id(vv_id) )  /* use old vertex */
           { unset_attr(vv_id,Q_MIDFACET);
             set_attr(vv_id,Q_MIDEDGE);
             set_vertex_edge(vv_id,e_id);
             free_element(v[i]);
             v[i] = vv_id;
           }
           else
            allv[i][N] = v[i];
         }
      else
         for ( i = 1 ; i < N ; i++ )
         { vv_id = allv[N-i][N];
           if ( valid_id(vv_id) )  /* use old vertex */
           { unset_attr(vv_id,Q_MIDFACET);
             set_attr(vv_id,Q_MIDEDGE);
             set_vertex_edge(vv_id,e_id);
             free_element(v[i]);
             v[i] = vv_id;
           }
           else
            allv[N-i][N] = v[i];
         }

      fe = get_next_edge(fe);
      e_id = get_fe_edge(fe);
      v = get_edge_vertices(e_id);
      if ( inverted(e_id) )
         for ( i = 0 ; i <= N ; i++ )
            allv[0][2*N-i] = v[i];
      else
         for ( i = 0 ; i <= N ; i++ )
            allv[0][N+i] = v[i];
            
      fe = get_facet_fe(newf[2]);
      e_id = get_fe_edge(fe);
      v = get_edge_vertices(e_id);
      if ( inverted(e_id) )
         for ( i = 0 ; i <= N ; i++ )
            allv[N+i][0] = v[i];
      else
         for ( i = 0 ; i <= N ; i++ )
            allv[2*N-i][0] = v[i];

      fe = get_next_edge(fe);  /* crosscut edge */
      e_id = get_fe_edge(fe);
      v = get_edge_vertices(e_id);
      if ( inverted(e_id) )
         for ( i = 1 ; i < N ; i++ )
         { vv_id = allv[N][N-i];
           if ( valid_id(vv_id) )  /* use old vertex */
           { unset_attr(vv_id,Q_MIDFACET);
             set_attr(vv_id,Q_MIDEDGE);
             set_vertex_edge(vv_id,e_id);
             free_element(v[i]);
             v[i] = vv_id;
           }
           else
            allv[N][N-i] = v[i];
         }
      else
         for ( i = 1 ; i < N ; i++ )
         { vv_id = allv[N][i];
           if ( valid_id(vv_id) )  /* use old vertex */
           { unset_attr(vv_id,Q_MIDFACET);
             set_attr(vv_id,Q_MIDEDGE);
             set_vertex_edge(vv_id,e_id);
             free_element(v[i]);
             v[i] = vv_id;
           }
           else
            allv[N][i] = v[i];
         }

      fe = get_next_edge(fe);
      e_id = get_fe_edge(fe);
      v = get_edge_vertices(e_id);
      if ( inverted(e_id) )
         for ( i = 0 ; i <= N ; i++ )
            allv[2*N-i][i] = v[i];
      else
         for ( i = 0 ; i <= N ; i++ )
            allv[N+i][N-i] = v[i];

      /* unwrap coordinates of old vertices */
      v = get_facet_vertices(f_id);
      for ( i = 0 ; i < oldctrl ; i++)
      { REAL *vx = get_oldcoord(v[i]);
        for ( k = 0 ; k < SDIM ; k++ ) 
          oldx[i][k] = vx[k];
        if ( get_vattr(v[i]) & BOUNDARY )
        { REAL *vp = get_param(v[i]);
          for ( k = 0 ; k < web.maxparam ; k++ )
            oldp[i][k] = vp[k];
        }
      }
      if ( w2 ) (*sym_wrap)(get_coord(v[N]),oldx[N],w2);
      if ( w3 ) (*sym_wrap)(get_coord(v[oldctrl-1]),oldx[oldctrl-1],w3);
      if ( w4 )
          for ( i = 1 ; i < N ; i++ )
              (*sym_wrap)(get_coord(v[i]),oldx[i],w4);
      if ( w5 )
          for ( i = 1 ; i < N ; i++ )
          { j = N-i +i*N+(3*i-i*i)/2;
             (*sym_wrap)(get_coord(v[j]),oldx[j],w5);
          }
      if ( w6 )
          for ( i = 1 ; i < N ; i++ )
          { j = i*N+(3*i-i*i)/2;
             (*sym_wrap)(get_coord(v[j]),oldx[j],w6);
          }

      /* now allocate new vertices and calculate coordinates as needed */
      for ( j = 1 ; j < 2*N ; j++)
        for ( i = 1; i+j < 2*N ; i++ )
        { REAL *vx,sum;
          if ( !valid_id(allv[j][i]) )
          { allv[j][i] = new_vertex(NULL,f_id);
            set_attr(allv[j][i],Q_MIDFACET);
          }
            /* regular coordinates */
          { vx = get_coord(allv[j][i]);
            for ( n = 0 ; n < SDIM ; n++ )
            { for ( k = 0, sum = 0.0 ; k < web.skel[FACET].ctrlpts ; k++ )
                sum += interp[i+2*N*j+(3*j-j*j)/2][k]*oldx[k][n];
              vx[n] = sum;
            }
          }
         
          /* whole facet boundary parameters */
          if ( facet_bdry_flag && (get_vattr(allv[j][i]) & Q_MIDFACET) )
            { REAL * vp = get_param(allv[j][i]);
              for ( n = 0 ; n < 2 ; n++ )
              { for ( k = 0, sum = 0.0 ; k < web.skel[FACET].ctrlpts ; k++ )
                  sum += interp[i+2*N*j+(3*j-j*j)/2][k]*oldp[k][n];
                vp[n] = sum;
              }
            }

           
        }

  
      /* wrap coordinates of internal vertices */
      if ( web.symmetry_flag )
      { 
         for ( i = 1 ; i < N ; i++ )
            for ( j = 1 ; i+j < N ; j++ ) wraps[j][i] = 0;
         for ( i = N+1 ; i < 2*N ; i++ )
            for ( j = 1 ; i+j < 2*N ; j++ ) wraps[j][i] = w4;
         for ( i = 1 ; i < N ; i++ )
            for ( j = N+1 ; i+j < 2*N ; j++ ) wraps[j][i] = w6;
         for ( i = 1 ; i < N ; i++ )
            for ( j = N-i+1 ; j < N ; j++ ) wraps[j][i] = w5;
         for ( i = 1 ; i < N ; i++ ) wraps[N-i][i] = w4;
         for ( i = 1 ; i < N ; i++ ) wraps[N][i] = w6;
         for ( j = 1 ; j < N ; j++ ) wraps[j][N] = w5;
         /* now unwrap coordinates */
         for ( i = 1 ; i < 2*N ; i++ )
           for ( j = 1 ; i+j < 2*N ; j++ ) 
           { REAL newx[MAXCOORD];
             REAL *vx = get_coord(allv[j][i]);
             (*sym_wrap)(vx,newx,sym_inverse(wraps[j][i]));
             for ( k = 0 ; k < SDIM ; k++ ) vx[k] = newx[k];
           }
      }

      /* assign vertices to facets */
      v0 = get_facet_vertices(f_id);
      v1 = get_facet_vertices(newf[0]);
      v2 = get_facet_vertices(newf[1]);
      v3 = get_facet_vertices(newf[2]);
      for ( i = 0 ; i <= N ; i++ )
        for ( j = 0 ; i+j <= N ; j++ )
        { int spot = i + N*j + (3*j-j*j)/2;
          v0[spot] = allv[N-j][N-i];
          v1[spot] = allv[j][i];
          v2[spot] = allv[j][N+i];
          v3[spot] = allv[N+j][i];
          if ( (i>0) && (j>0) && (i+j<N) )
          { set_vertex_facet(v0[spot],f_id);
            set_vertex_facet(v1[spot],newf[0]);
            set_vertex_facet(v2[spot],newf[1]);
            set_vertex_facet(v3[spot],newf[2]);
          }
       }
    }

    /* adjust facet_fe so new facets similar to old */
    if ( valid_id(newf[1]) )
       set_facet_fe(newf[1],get_prev_edge(get_facet_fe(newf[1])));
    if ( valid_id(newf[2]) )
       set_facet_fe(newf[2],get_next_edge(get_facet_fe(newf[2])));

    if ( web.symmetry_flag && (sym_flags & HAS_FIXED_PTS) )
    { /* get proper wrap on edge cutting off old base point */
      edge_id prev_e,next_e;
      WRAPTYPE we,wn,wp,wi;
      fe = get_facet_fe(f_id);
      e_id = get_fe_edge(fe);
      prev_e = get_fe_edge(get_prev_edge(fe));
      next_e = get_fe_edge(get_next_edge(fe));
      we = get_edge_wrap(e_id);
      wp = get_edge_wrap(prev_e);
      wi = (*sym_compose)(wp,we);
      wn = (*sym_inverse)(wi);
      set_edge_wrap(next_e,wn);
    }

  } 

windup:
  if ( web.modeltype == LAGRANGE ) 
  { if ( !bezier_flag ) free_matrix(interp);
    free_matrix(oldx);
  } 
  if ( oldp ) free_matrix(oldp);
  if ( reflevel < MAXLEVEL-1 ) reflevel++;  /* for extrapolation */
  autochop_length /= 2;    /* keep same relative scale */
  bare_edge_count++;

  LEAVE_GRAPH_MUTEX;

} /* end refine() */



/***********************************************************
*
*  Function:  areaweed()
*
*  Purpose:    Subdivide long edges of skinny triangles.
*              Should be called after edgeweed() gets rid
*              of short triangles.  Newly created trianges
*              are marked as NEWFACET, and are not tested
*              again in this pass.
*
*  Input:      Area cutoff for skinny triangles.        
*
*  Output:    
*  weed by eliminating shortest side. eliminate obtuse vertex,
*
*  Return:     Number of edges deleted.
*/


#ifdef ANSI_DEF
int areaweed(
REAL min_area)    /* criterion for weeding out small triangles */
#else
int areaweed(min_area)
REAL min_area;    /* criterion for weeding out small triangles */
#endif
{
  facet_id f_id;  /* facet being worked on */
  facet_id sentinel;
  int weedcount = 0; /* number of facets created */

  if ( web.representation != SOAPFILM )
     kb_error(1339,"Can only do area weed on soapfilm model.\n",RECOVERABLE);

  if ( web.representation == SIMPLEX )
      kb_error(1340,"Areaweed not implemented for simplex representation.\n",
        RECOVERABLE);

  web.vol_flag = 0;

  /* first, unmark all NEWFACET attributes */
  FOR_ALL_FACETS(f_id)
      unset_attr(f_id,NEWFACET);

  ENTER_GRAPH_MUTEX;

  /* main loop sweeping over all triangles */
  f_id = NULLFACET;
  while ( generate_all(FACET,&f_id,&sentinel) )
  { REAL side[FACET_EDGES][MAXCOORD];  /* side vector */
    REAL sside[FACET_EDGES]; /* squares of side lengths */
    REAL area;      /* area of triangle */
    facetedge_id fe[FACET_EDGES]; /* edges of triangle */
    edge_id e_id;
    int i;              /* side number */
    int elimcount;
#ifdef _DEBUGXX
    if ( (f_id & 0xFFFFFF) == 0xb3b )
      break;
#endif
    /* skip already modified triangles */
    if ( get_fattr(f_id) & NEWFACET )
       continue;

    /* find sides and area */
    fe[0] = get_facet_fe(f_id);
    for ( i = 0 ; i < FACET_EDGES ; i++ ) 
    { get_fe_side(fe[i],side[i]);
      sside[i] = SDIM_dot(side[i],side[i]);
      fe[i+1] = get_next_edge(fe[i]);
    }
    
    area = get_facet_area(f_id);

    if ( area > min_area )  /* skip big triangles */
      continue;  

    /* weed by eliminating shortest side. eliminate obtuse vertex,
       the one between the two shortest sides, if possible.
       Follow with equiangulation  */
    i = (sside[0] < sside[1]) ? 0 : 1;
    i = (sside[i] < sside[2]) ? i : 2;
   
    if ( sside[(i+1)%3] < sside[(i+2)%3] )
      e_id = inverse_id(get_fe_edge(fe[i]));
    else 
      e_id = get_fe_edge(fe[i]);

    if ( sside[(i+1)%3] < sside[(i+2)%3] )
    { e_id = get_fe_edge(fe[(i+1)%3]);
      elimcount = eliminate_edge(e_id);
      if ( elimcount ) goto elimdone;
      e_id = get_fe_edge(fe[(i+2)%3]);
      elimcount = eliminate_edge(e_id);
    }
    else
    { e_id = get_fe_edge(fe[(i+2)%3]);
      elimcount = eliminate_edge(e_id);
      if ( elimcount ) goto elimdone;
      e_id = get_fe_edge(fe[(i+1)%3]);
      elimcount = eliminate_edge(e_id);
    }
elimdone:
    if ( elimcount )
    { free_element(e_id);
      if ( verbose_flag )
      { sprintf(msg,"Weeded facet %s\n",ELNAME(f_id));
        outstring(msg);
      }
      weedcount += elimcount;
    }
    else if ( verbose_flag )
    { sprintf(msg,"Couldn't weed facet %s\n",ELNAME(f_id));
      outstring(msg);
    }

  }  /* end main sweep loop */

  LEAVE_GRAPH_MUTEX;

  if ( weedcount > 0 ) top_timestamp = ++global_timestamp;
  return weedcount;
}

/***********************************************************************
*
*  Function: eliminate_facet()
*
*  Purpose: Delete facet by finding shortest edge and eliminating it.
*
*  Return: 1 if eliminated.
*/
int eliminate_facet(f_id)
facet_id f_id;
{
  REAL side[MAXCOORD];  /* side vector */
  REAL sside[FACET_EDGES]; /* squares of side lengths */
  facetedge_id fe[FACET_EDGES]; /* edges of triangle */
  edge_id e_id;
  int i;              /* side number */
  int elimcount;

  if ( web.dimension == 1 ) return string_eliminate_facet(f_id);
  if ( web.representation == SIMPLEX ) return simplex_delete_facet(f_id);

  ENTER_GRAPH_MUTEX;

  /* find sides */
  fe[0] = get_facet_fe(f_id);
  for ( i = 0 ; i < FACET_EDGES ; i++ ) 
    {
       get_fe_side(fe[i],side);
       sside[i] = SDIM_dot(side,side);
       fe[i+1] = get_next_edge(fe[i]);
    }

  /* weed by eliminating shortest side. eliminate obtuse vertex,
      the one between the two shortest sides, if possible.
    */
  i = (sside[0] < sside[1]) ? 0 : 1;
  i = (sside[i] < sside[2]) ? i : 2;
  e_id = get_fe_edge(fe[i]);
  elimcount = eliminate_edge(e_id);
  if ( elimcount ) goto elimdone;

  if ( sside[(i+1)%3] < sside[(i+2)%3] )
  { e_id = get_fe_edge(fe[(i+1)%3]);
    elimcount = eliminate_edge(e_id);
    if ( elimcount ) goto elimdone;
    e_id = get_fe_edge(fe[(i+2)%3]);
    elimcount = eliminate_edge(e_id);
  }
  else 
  { e_id = get_fe_edge(fe[(i+2)%3]);
    elimcount = eliminate_edge(e_id);
    if ( elimcount ) goto elimdone;
    e_id = get_fe_edge(fe[(i+1)%3]);
    elimcount = eliminate_edge(e_id);
  }
elimdone:
  if ( elimcount )
  { if ( verbose_flag )
    { sprintf(msg,"Deleting facet %s\n",ELNAME(f_id));
       outstring(msg);
    }
    free_element(e_id);
  }
  else if ( verbose_flag )
    { sprintf(msg,"Couldn't delete facet %s\n",ELNAME(f_id));
       outstring(msg);
    }
  LEAVE_GRAPH_MUTEX;
  return elimcount;
}


/***********************************************************************
*
*  Function: string_eliminate_facet()
*
*  Purpose: Delete facet in string model by deleting all its edges.
*
*  Return: 1 if eliminated.
*/
int string_eliminate_facet(f_id)
facet_id f_id;
{
  facetedge_id fe,start_fe,next_fe; 
  edge_id e_id,final_e=NULLID;
  int done,not_done=0;              /* side number */
  int elimcount=0;
  int loop_flag; /* whether edges form closed loop */

  /* find starting point */
  fe = get_facet_fe(f_id);
  if ( !valid_id(fe) )
  { free_element(f_id);
    return 1;
  }

  ENTER_GRAPH_MUTEX;

  loop_flag = 1;
  start_fe = fe;
  do
  { if ( equal_element(get_next_edge(fe),fe) )
    { /* end of edge chain */
      loop_flag = 0;
    }
  } while ( !equal_id(fe,start_fe) );

  if ( loop_flag == 0 )
  { /* find start of chain */
    while ( !equal_element(get_prev_edge(start_fe),start_fe) )
      start_fe = get_prev_edge(start_fe);
  }

  fe = start_fe;
  do
  { next_fe = get_next_edge(fe);
    e_id = get_fe_edge(fe);
    if ( valid_id(next_fe) )
      final_e = get_fe_edge(next_fe);
    done = eliminate_edge(e_id);
    if ( done )
    { elimcount++;
      free_element(e_id);
    }
    else not_done++;
    fe = next_fe;
  } while ( valid_element(fe) );

  // clean up dangling edge maybe left by last deletion
  if ( valid_element(final_e) )
  { done = eliminate_edge(final_e);
    if ( done )
    { elimcount++;
      free_element(final_e);
    }
  }

  LEAVE_GRAPH_MUTEX;

  if ( not_done == 0 )
  { if ( verbose_flag )
    { sprintf(msg,"Deleted facet %s\n",ELNAME(f_id));
      outstring(msg);
    }
    return 1;
  }
  else if ( verbose_flag )
  { sprintf(msg,"Couldn't entirely delete facet %s\n",ELNAME(f_id));
    outstring(msg);
  }
  return -elimcount;
}

/***********************************************************************
*
*  Function: area_histogram()
*
*  Purpose: Give user a histogram of area lengths to guide areaweeding.
*              Reports in bins of width powers of two.
*/

void area_histogram()
{
  facet_id f_id;  /* facet being worked on */
  int bincount[HISTO_BINS];
  int n;
  REAL ref_area = web.total_area/1000;

  for ( n = 0 ; n < HISTO_BINS ; n++ ) bincount[n] = 0;

  /* main loop sweeping over all triangles */
  FOR_ALL_FACETS(f_id)
     {
        REAL area;      /* area of triangle */

        area = get_facet_area(f_id);
        if ( area == 0.0 ) n = 0;
        else
          n = HISTO_BINS/2 + 1 + (int)floor(log(area/ref_area)*HISTO_BINSIZE);
        if ( n < 0 ) n = 0;
        if ( n >= HISTO_BINS ) n = HISTO_BINS - 1;
        bincount[n]++;
     }
  outstring("         area                 number\n");
  if ( bincount[0] )
     {
        sprintf(msg,"%f - %g      %6d \n",0.0,
             (DOUBLE)(ref_area*exp((-HISTO_BINS/2)/HISTO_BINSIZE)), bincount[0]);
        outstring(msg);
     }
  for ( n = 1 ; n < HISTO_BINS ; n++ )
    if ( bincount[n] )
     {
        sprintf(msg,"%g - %g      %6d\n",
                (DOUBLE)(ref_area*exp((n-HISTO_BINS/2-1)/HISTO_BINSIZE)),
                  (DOUBLE)(exp((n-HISTO_BINS/2)/HISTO_BINSIZE)*ref_area),bincount[n]);
        outstring(msg);
     }
}


/***********************************************************
*
*  Function:  skinny()
*
*  Purpose:    Subdivide long edges of skinny triangles.
*                 Newly created trianges
*                 are marked as NEWFACET, and are not tested
*                 again in this pass.
*
*  Input:      Acutest angle cutoff for skinny triangles.        
*
*  Output:     Skinny triangles have long edges subdivided.
*
*  Return:     Number of facets created.
*/

int skinny(min_angle)
REAL min_angle;    /* criterion for weeding out small triangles */
{
  facet_id f_id;  /* facet being worked on */
  facet_id sentinel;
  int weedcount = 0; /* number of facets created */

  web.vol_flag = 0;

  /* first, unmark all NEWFACET attributes */
  FOR_ALL_FACETS(f_id)
      unset_attr(f_id,NEWFACET);

  ENTER_GRAPH_MUTEX;

  /* main loop sweeping over all triangles */
  f_id = NULLFACET;
  while ( generate_all(FACET,&f_id,&sentinel) )
  { REAL side[MAXCOORD];  /* side vector */
    REAL sside[FACET_EDGES]; /* squares of side lengths */
    REAL angle;      /* area of triangle */
    facetedge_id fe[FACET_EDGES+1]; /* edges of triangle, with wrap */
    int i;              /* side number */
    int smallside, mid, big;

    /* skip already modified triangles */
    if ( get_fattr(f_id) & NEWFACET )
         continue;

    /* find sides */
    fe[0] = get_facet_fe(f_id);
    for ( i = 0 ; i < FACET_EDGES ; i++ ) 
    { get_fe_side(fe[i],side);
      sside[i] = SDIM_dot(side,side);
      fe[i+1] = get_next_edge(fe[i]);
    }

    /* find shortest side */
    smallside = (sside[0] < sside[1]) ? 0 : 1;
    smallside = (sside[smallside] < sside[2]) ? smallside : 2;
    /* find longest side */
    big = (sside[0] > sside[1]) ? 0 : 1;
    big = (sside[big] > sside[2]) ? big : 2;
    /* find middle side */
    mid = 3 - (smallside+big);
    angle = acos( (sside[mid]+sside[big]-sside[smallside])/2/
                             sqrt(sside[mid]*sside[big]) );
    if ( angle > min_angle )  /* skip fat triangles */
         continue;
    edge_refine(get_fe_edge(fe[big]));

    weedcount++;

  }  /* end main sweep loop */

  LEAVE_GRAPH_MUTEX;

  if ( weedcount > 0 ) top_timestamp = ++global_timestamp;
  return weedcount;
}


/***********************************************************************
*
*  Function: skinny_histogram()
*
*  Purpose: Give user a histogram of acute angles to guide skinnyweeding.
*              Reports in bins of width powers of two.
*/

void skinny_histogram()
{
  facet_id f_id;  /* facet being worked on */ 
  int bincount[HISTO_BINS]; 
  int n; 
 
  for ( n = 0 ; n < HISTO_BINS ; n++ ) bincount[n] = 0;
  /* main loop sweeping over all triangles */ 
  FOR_ALL_FACETS(f_id)
    {
        REAL sside[FACET_EDGES]; /* squares of side lengths */ 
        REAL angle;      /* area of triangle */
        facetedge_id fe[FACET_EDGES+1]; /* edges of triangle */
        int i;              /* side number */
        int smallside,mid,big;

        /* find sides and area */
        fe[0] = get_facet_fe(f_id);
        for ( i = 0 ; i < FACET_EDGES ; i++ ) 
          { 
             calc_edge(get_fe_edge(fe[i]));
             sside[i] = get_edge_length(get_fe_edge(fe[i]));
             fe[i+1] = get_next_edge(fe[i]);
          }
        /* find shortest side */
        smallside = (sside[0] < sside[1]) ? 0 : 1;
        smallside = (sside[smallside] < sside[2]) ? smallside : 2;
        /* find longest side */
        big = (sside[0] > sside[1]) ? 0 : 1;
        big = (sside[big] > sside[2]) ? big : 2;
        /* find middle side */
        mid = 3 - (smallside+big);
        angle = acos((sside[mid]+sside[big]-sside[smallside])/2/
                                 sqrt(sside[mid]*sside[big]) );
        if ( angle == 0.0 ) n = 0;
        else
          n = HISTO_BINS + (int)floor(log(angle/M_PI)*HISTO_BINSIZE); 
          
        if ( n < 0 ) 
          n = 0; 
        else if ( n >= HISTO_BINS ) 
          n = HISTO_BINS - 1;

        bincount[n]++;
     }

  outstring("         angle                 number\n");
  if ( bincount[0] )
     {
        sprintf(msg,"%f - %g      %6d \n",0.0,
             (DOUBLE)(M_PI*exp((-HISTO_BINS/2)/HISTO_BINSIZE)), bincount[0]);
        outstring(msg);
     }
  for ( n = 1 ; n < HISTO_BINS ; n++ )
    if ( bincount[n] )
     {
        sprintf(msg,"%g - %g      %6d\n",
                (DOUBLE)(M_PI*exp((n-HISTO_BINS)/HISTO_BINSIZE)),
                  (DOUBLE)(exp((n-HISTO_BINS+1)/HISTO_BINSIZE)*M_PI),bincount[n]);
        outstring(msg);
     }
}


/******************************************************************
*
*  Function:  edgeweed()
*
*  Purpose: eliminate all unfixed edges whose length is less
*              than min_length.  Will not remove fixed edges.
*              Only removes free boundary edges with both ends
*              on same boundary.
*
*  Input:    none
*
*  Output:  number of facets removed
*/

int edgeweed(min_length)
REAL min_length; /* minimum allowed edge length */
{
  edge_id e_id,sentinel;
  int weedcount = 0;

  if ( web.representation == SIMPLEX )
     return simplex_tiny_edges(min_length);

  web.vol_flag = 0;

  /* main loop over all edges */

  ENTER_GRAPH_MUTEX;

  e_id = NULLEDGE;
  while ( generate_all(EDGE,&e_id,&sentinel) )
  {
    REAL side_len;  /* actual side length */
    int elimcount = 0;

    calc_edge(e_id);
    side_len = get_edge_length(e_id);

    if ( side_len < min_length ) elimcount = eliminate_edge(e_id);
    if ( elimcount ) 
    { free_element(e_id);
      weedcount++ ;
    }
  }
  LEAVE_GRAPH_MUTEX;

  if ( weedcount > 0 ) top_timestamp = ++global_timestamp;
    return weedcount;
} 

/***********************************************************************
*
*  Function: edge_histogram()
*
*  Purpose: Give user a histogram of edge lengths to guide edgeweeding.
*              Reports in bins of width powers of two.
*/

void edge_histogram()
{
  edge_id e_id;
  int bincount[HISTO_BINS];
  int n;

  for ( n = 0 ; n < HISTO_BINS ; n++ ) bincount[n] = 0;

  /* main loop over all edges */

  if ( overall_size == 0.0 ) resize();
  FOR_ALL_EDGES(e_id)
     {
        REAL side_len;  /* actual side length */

        calc_edge(e_id);
        side_len = get_edge_length(e_id);
        if ( side_len <= 0.0 ) n = 0;
        else n = HISTO_BINS/2 + 1 
                        + (int)floor(log(side_len/overall_size)*HISTO_BINSIZE);
        if ( n < 0 ) n = 0;
        if ( n >= HISTO_BINS ) n = HISTO_BINS - 1;
        bincount[n]++;
     }
  outstring("     side length            number\n");
  if ( bincount[0] )
     {
        sprintf(msg,"%f - %g      %6d \n",0.0,
            (DOUBLE)(overall_size*exp((-HISTO_BINS/2)/HISTO_BINSIZE)),
                                                                  bincount[0]);
        outstring(msg);
     }
  for ( n = 1 ; n < HISTO_BINS ; n++ )
    if ( bincount[n] )
     {
        sprintf(msg,"%g - %g      %6d\n",
            (DOUBLE)(overall_size*exp((n-HISTO_BINS/2-1)/HISTO_BINSIZE)),
            (DOUBLE)(overall_size*exp((n-HISTO_BINS/2)/HISTO_BINSIZE)),bincount[n]);
        outstring(msg);
     }
}


/*******************************************************************
*
*  Function: eliminate_edge()
*
*  Purpose: delete an edge and adjacent facets (if triangles in STRING);
*           merges head of edge to tail, unless head fixed.
*           If both ends fixed, will do nothing.
*           Does not deallocate edge itself; caller must do that
*           due to use in chaining to next edge.
*           Favors keeping non-valence-2 edges in SOAPFILM model,
*               constraints and such permitting.
*
*  Input:    edge id of edge to eliminate
*
*  Output:  returns number of edges eliminated
*/

int eliminate_edge(short_edge)
edge_id short_edge;
{
  facetedge_id base_fe;  /* along edge to be eliminated */
  vertex_id headv,tailv;
  vertex_id elim_v;  /* vertex to eliminate */
  vertex_id keep_v;  /* vertex to keep */
  int edge_head_same,edge_tail_same;  /* whether same constraints and stuff */
  int head_edge_comp,tail_edge_comp,tail_head_comp;
  body_id b_id;
  facet_id f_id;
  facetedge_id first_fe;
  int retval = 0;

  if ( web.representation == SIMPLEX )
     kb_error(1341,"Cannot eliminate edge in simplex model.\n",RECOVERABLE);

  if ( web.modeltype == LAGRANGE )
     kb_error(1342,"No eliminate_edge() in Lagrange model yet.\n",RECOVERABLE );

  if ( !valid_element(short_edge) ) return 0;
  if ( get_eattr(short_edge) & FIXED )
  { if ( verbose_flag )
    { sprintf(msg,"Not deleting edge %s since it is FIXED.\n",
        ELNAME(short_edge));
      outstring(msg);
    }
    return 0;
  }
  web.vol_flag = 0;

  headv = get_edge_headv(short_edge);
  tailv = get_edge_tailv(short_edge);

  /* kludge to overcome problem with change_vertex not finding
      all edges linked to vertex */
  if ( !valid_element(headv) ) 
     unfree_element(headv);
  if ( !valid_element(tailv) ) 
     unfree_element(tailv);

  /* Figure out which vertex to eliminate.  Keep fixed vertices. */

  head_edge_comp = compare_vertex_edge_attr(headv,short_edge); 
  tail_edge_comp = compare_vertex_edge_attr(tailv,short_edge); 
  tail_head_comp = compare_vertex_attr(tailv,headv);
  edge_head_same =  ((head_edge_comp==A_SUB_B) || (head_edge_comp==A_EQ_B)) && 
                          !(get_vattr(headv) & FIXED); 
  edge_tail_same =  ((tail_edge_comp==A_SUB_B) || (tail_edge_comp==A_EQ_B)) &&
                          !(get_vattr(tailv) & FIXED);
  if ( (edge_head_same || 
    ((head_edge_comp == A_EQ_B || head_edge_comp == A_SUPER_B) &&
     (tail_head_comp == A_EQ_B || tail_head_comp == A_SUPER_B)) ) && 
        (get_boundary(headv) == get_edge_boundary(short_edge) ) 
        &&   !( get_vattr(headv) & (FIXED|AXIAL_POINT)) )
  { /* short_edge ok */
  }
  else 
  if ( (edge_tail_same ||
    ((tail_edge_comp == A_EQ_B || tail_edge_comp == A_SUPER_B) &&
     (tail_head_comp == A_EQ_B || tail_head_comp == A_SUB_B))  ) &&
       (get_boundary(tailv) == get_edge_boundary(short_edge) ) 
       &&   !( get_vattr(tailv) & (FIXED|AXIAL_POINT)) )

  { short_edge = edge_inverse(short_edge);
  }
  else 
  { if ( verbose_flag )
    { sprintf(msg,"Can't delete edge %s due to conflicting constraints, boundaries, or fixedness.\n",ELNAME(short_edge));
             outstring(msg);
    }
    return 0; /* can't safely eliminate edge */
  }

  /* check edges that would be merged */
  if ( web.representation == SOAPFILM )
  { facetedge_id fe;
    base_fe = get_edge_fe(short_edge);
    fe = base_fe;
    if ( valid_id(base_fe) )
    for (;;)
    { facetedge_id fe_a = get_prev_edge(fe);
      facetedge_id fe_b = get_next_edge(fe);
      if ( valid_id(get_fe_facet(fe)) )
      { int abcomp = compare_edge_attr(get_fe_edge(fe_a),get_fe_edge(fe_b));
        if ( abcomp == INCOMPARABLE)
        { facet_id f_id = get_fe_facet(fe);
          int acomp = compare_edge_facet_attr(get_fe_edge(fe_a),f_id);
          int bcomp = compare_edge_facet_attr(get_fe_edge(fe_b),f_id);
          if ( !(acomp==A_SUB_B || acomp==A_EQ_B || bcomp==A_SUB_B || bcomp==A_EQ_B) )
          { if ( verbose_flag )
            { sprintf(msg,
             "Can't delete edge %s due to constraints or methods on edges %s and %s.\n",ELNAME(short_edge),ELNAME1(get_fe_edge(fe_a)),ELNAME2(get_fe_edge(fe_b)));
               outstring(msg);
            }
            return 0; 
          }
        }
      }
      fe = get_next_facet(fe);
      if ( equal_id(fe,base_fe) ) break;
    }
  }

  elim_v = get_edge_headv(short_edge);
  keep_v = get_edge_tailv(short_edge);

  if ( get_vattr(keep_v) & AXIAL_POINT )
  {  sprintf(errmsg,"Not deleting edge %s due to axial point.\n",
         ELNAME(short_edge));
  /*    kb_error(2197,errmsg,WARNING);  john doesn't like warning */
     if ( verbose_flag ) outstring(errmsg);
     return 0;
  }

  /* check for multiple edges between endpoints */
  if ( web.symmetry_flag && (web.representation == SOAPFILM) )
  { edge_id e_id = short_edge;
    e_id = get_next_tail_edge(e_id);
    do
    { if ( equal_id(get_edge_headv(e_id) ,elim_v) 
          && (get_edge_wrap(e_id) != get_edge_wrap(short_edge)) )
      { if ( verbose_flag ) outstring(
                "Not deleting edge due to multiple edges between endpoints with different wraps.\n");
        return 0;  
      }
       e_id = get_next_tail_edge(e_id);
    }
    while ( !equal_id(short_edge,e_id) );
  }



  /* Go through facets around edge,  checking for stars (adjacent
     triangulated triangle) and unstarring if found.              */
  if ( web.representation == SOAPFILM )
  { int bad_flag = 0;
  
    base_fe = first_fe = get_edge_fe(short_edge);
    while ( valid_id(base_fe) )
    {
      if ( simple_unstar(base_fe) < 0 )
        bad_flag = 1;

      base_fe = get_next_facet(base_fe);
      if ( equal_id(base_fe,first_fe) ) base_fe = NULLID;
    } /* end base_fe loop */
    
    /* extra checks for bad configurations */
    if ( star_finagling )
      if( star_finagle(short_edge) < 0 )
        bad_flag = 1;

    if ( bad_flag )
    { if ( force_deletion )
      { if ( verbose_flag )
          outstring("Risky configuration, but proceeding with deletion since force_deletion is on.\n");
      } 
      else return 0;
    }


  }

  if ( verbose_flag )
  { sprintf(msg,"Deleting edge %s\n",ELNAME(short_edge));
    outstring(msg);
  }
  
 ENTER_GRAPH_MUTEX;

 /* unwrap edge */
 if ( web.torus_flag )
   torus_unwrap_edge(short_edge);
 else if ( web.symmetry_flag )
  { edge_id pos_e = positive_id(short_edge);  /* in case of quadratic */
    int wrap = get_edge_wrap(pos_e);
    if ( wrap )
      wrap_vertex(get_edge_headv(pos_e),wrap);
  }

  /* put keep_v at middle of old edge if possible */
  if ( (web.modeltype == LINEAR) || (web.modeltype == QUADRATIC) )
  { if ( edge_head_same && edge_tail_same )
    { if ( web.representation == STRING )
      { /* could eliminate either end, so keep high valence vertex */
        facetedge_id fe = get_edge_fe(short_edge);
        facetedge_id ffe;
        edge_id e1,e2; 
        if ( valid_id(fe) )
        { ffe = get_next_facet(fe);
          e1 = get_fe_edge(get_prev_edge(fe));
          e2 = get_fe_edge(get_prev_edge(ffe));
          edge_tail_same = equal_id(e1,e2);
          e1 = get_fe_edge(get_next_edge(fe));
          e2 = get_fe_edge(get_next_edge(ffe));
          edge_head_same = equal_id(e1,e2);
          if ( edge_tail_same && !edge_head_same )
          { short_edge = edge_inverse(short_edge);
            elim_v = get_edge_headv(short_edge);
            keep_v = get_edge_tailv(short_edge);
          }
        }
      }
      if ( edge_tail_same == edge_head_same )
      { REAL *tailx = get_coord(keep_v);
        REAL *headx = get_coord(elim_v); /* since unwrapped */
        int i;
        for ( i = 0 ; i < SDIM ; i++ )
          tailx[i] = (tailx[i]+headx[i])/2;
      }
    }
  }     
  
  /* change all references to the eliminated vertex to the
      kept vertex. */
  if ( !equal_id(elim_v,keep_v) )
  { edge_id e_id;
    int nn = 0;
    for (;;)
    { e_id = get_vertex_edge(elim_v);
      if ( !valid_id(e_id) ) break;
      remove_vertex_edge(elim_v,e_id);
      if ( equal_element(e_id,short_edge) ) 
         continue;
      set_edge_tailv(e_id,keep_v);
      if ( ++nn > web.skel[EDGE].count ) 
      { sprintf(errmsg,"Internal error: Bad edge loop on vertex %s.\n",
           ELNAME(elim_v));
        kb_error(1344,errmsg,WARNING); 
        break; 
      }
    }
  }
  remove_vertex_edge(keep_v,short_edge);

#ifdef MPI_EVOLVER
  mpi_note_edge_delete(short_edge,elim_v,keep_v);
#endif

  /* Go through facets around edge, adjusting facet loops
      of the merged edges, deleting facetedges, and facets */
  base_fe = first_fe = get_edge_fe(short_edge);
  while ( valid_id(base_fe) )
  {
    facetedge_id next_base;  /* to get before freeing base_fe */
    facetedge_id aa,bb;
    facetedge_id bbase_fe;
    facetedge_id next_fe,prev_fe;
    vertex_id third_v;

    next_fe = get_next_edge(base_fe);
    prev_fe = get_prev_edge(base_fe);
    third_v = get_fe_headv(next_fe);
    if ( (web.representation == STRING) && 
       ((!valid_id(get_fe_facet(base_fe)) || !valid_id(prev_fe) || 
       (!equal_id(get_next_edge(next_fe), prev_fe)) && !equal_id(prev_fe,next_fe))
        || ((web.modeltype == QUADRATIC) && (!equal_id(prev_fe,next_fe)))) ) 
    {
      /* do not eliminate facets with more than 3 edges */
      /* but might have free end */

      bbase_fe = base_fe;
  /*    do  */
      {
        next_fe = get_next_edge(bbase_fe);
        if ( equal_element(short_edge,get_fe_edge(next_fe)) )
          next_fe = get_next_edge(next_fe);

        prev_fe = get_prev_edge(bbase_fe);
        if ( equal_element(short_edge,get_fe_edge(prev_fe)) )
          prev_fe = get_prev_edge(prev_fe);

        set_next_edge(prev_fe,next_fe);
        set_prev_edge(next_fe,prev_fe);
        f_id = get_fe_facet(bbase_fe);
        if ( valid_id(f_id) && equal_id(bbase_fe,get_facet_fe(f_id)) )
        { b_id = get_facet_body(f_id);
          if ( valid_id(next_fe) )
          { set_facet_fe(f_id,next_fe);
            if ( valid_id(b_id) )
              set_body_facet(b_id,f_id);
          } else
          { set_facet_fe(f_id,prev_fe);
            if ( valid_id(b_id) )
              set_body_facet(b_id,f_id);
          }
        }
        bbase_fe = get_next_facet(bbase_fe);
      } /*while ( valid_id(bbase_fe) && (!equal_id(bbase_fe,base_fe)) );*/
    }
    else
    { /* eliminate whole facet */
      facetedge_id a=0;  /* for edge to be merged */
      facetedge_id b=0;  /* for edge to be merged with */
      facetedge_id a_next=0,a_prev=0,b_next=0,b_prev=0;
                 /* facet chain links around edges a,b */
      facetedge_id next_fea;  /* next around facet loop of a */
      edge_id  a_edge=0; /* edge of side a */
      edge_id  b_edge=0; /* edge of side b */
      facet_id  facet=0;  /* facet to be eliminated */ 
      edge_id keep_edge=0,throw_edge=0;

      /* label relevant edges and */
      /* see if we have an adjacent starred triangle which will give
          trouble if we don't unstar it */
      bbase_fe = base_fe;
      /* for(;;) */ /* may take multiple passes for unstarring */
      { int flag;
        int unstar_count = 0;
        int ktcomp;
        /* first, set up to favor deletion of valence 2 edge */
        if ( get_edge_valence(get_fe_edge(get_prev_edge(bbase_fe))) == 2 )
              bbase_fe = inverse_id(bbase_fe);
        /* now check conditions and set up for elimination */
        for (flag=0;flag<2;flag++)
        { facet = get_fe_facet(bbase_fe);
          if ( !valid_id(facet) ) break;
          a = get_next_edge(bbase_fe);
          a_edge = get_fe_edge(a);
          a_next = get_next_facet(a);
          a_prev = get_prev_facet(a);
          b = get_prev_edge(bbase_fe);
          b_edge = get_fe_edge(b);
          b_next = get_next_facet(b);
          b_prev = get_prev_facet(b);
          keep_edge = b_edge; throw_edge = a_edge; 
          
          ktcomp = compare_edge_attr(keep_edge,throw_edge);
          if ( ktcomp == A_SUB_B )
          { bbase_fe = inverse_id(bbase_fe); continue; } 
          if ( ktcomp == INCOMPARABLE )
          { facet_id f_id = get_fe_facet(bbase_fe);
            int tcomp = compare_edge_facet_attr(throw_edge,f_id);
            if ( !(tcomp==A_SUB_B || tcomp==A_EQ_B) )
            { bbase_fe = inverse_id(bbase_fe); continue; }
          } 
          
          if ( equal_element(a_edge,b_edge) ) break;
          aa = get_next_edge(a_next);
          bb = get_prev_edge(b_next);
          if ( web.representation != SOAPFILM ) break;
          if ( get_vattr(get_edge_headv(a_edge)) & AXIAL_POINT ) break;
          if ( equal_id(get_next_facet(aa),inverse_id(bb)) )
          { int ret = unstar(aa); 
            if ( ret < 0 )
            { sprintf(errmsg,
        "Edge %s not deleted due to adjacent configuration involving facet %s.\n",
                                 ELNAME(short_edge),ELNAME1(facet));

            kb_error(1345,errmsg,WARNING);
              break;
            }
            unstar_count = 1;
          }
          break;  
        }
        if ( unstar_count ) continue;  /* retry */
        if ( flag >= 2 ) 
        {
          sprintf(errmsg,"Edge amenity failed second time through on edge %s\n",
              ELNAME(short_edge));
          kb_error(2198,errmsg, RECOVERABLE);
        }
        /* break;  since not doing for loop any more */
      }

      /* put throw_edge constraints on keep_edge */
/* probably don't want to do this wholesale 
      set_attr(keep_edge,get_eattr(throw_edge));
      set_e_conmap(keep_edge,get_e_constraint_map(throw_edge));
      if ( get_eattr(throw_edge) & BOUNDARY )
        set_edge_boundary_num(keep_edge,get_edge_boundary_num(throw_edge));
*/
      if ( equal_element(a,b) )
      { /* dihedron; should be only in quadratic model */     
        facetedge_id rfe = get_next_facet(a);
        do
        { facet_id ff_id = get_fe_facet(rfe);
          facetedge_id rnext = get_next_edge(rfe);
          facetedge_id rprev = get_prev_edge(rfe);
          if ( equal_element(rfe,get_facet_fe(ff_id)) )
          { /* reset ff_id facetedge link */
            if ( equal_id(rnext,rfe) )
            { if ( equal_id(rfe,rprev) )
                set_facet_fe(ff_id,NULLID);
              else
                set_facet_fe(ff_id,rprev);
            }
            else 
              set_facet_fe(ff_id,rnext);              
          }
          set_next_edge(rprev,rnext);
          set_prev_edge(rnext,rprev);
          free_element(rfe);
          rfe = get_next_facet(rfe);
        } while ( !equal_element(a,rfe) );
        goto wasloop;   
      }
      
      if ( equal_id(a_next,a) )  /* single facet on edge a */
      { if ( equal_id(b_next,b) ) /* and on edge b */
            set_edge_fe(keep_edge,NULLID);
        else  /* have more around b */
        { set_next_facet(b_prev,b_next);
          set_prev_facet(b_next,b_prev);
          set_edge_fe(keep_edge,b_next);
        }
      }
      else if ( equal_id(b_next,b) )  /* single facet on edge b */
      { set_next_facet(a_prev,a_next);
        set_prev_facet(a_next,a_prev);
        set_edge_fe(keep_edge,fe_inverse(a_next));
      }
      else if ( equal_element(a_edge,b_edge) )
      { /* excise a,b from loop */
        set_prev_facet(a_next,a_prev);
        set_next_facet(a_prev,a_next);
        b_next = get_next_facet(b); /* in case of change */
        b_prev = get_prev_facet(b);
        if ( equal_id(b,b_next) )
              set_edge_fe(keep_edge,NULLEDGE); /* dropped only facet */
        else
        { /* excise b */
          set_prev_facet(b_next,b_prev);
          set_next_facet(b_prev,b_next);
          set_edge_fe(keep_edge,b_next);
        }
      }
      else  /* have to join chains */
      { set_prev_facet(a_next,fe_inverse(b_next));
        set_next_facet(a_prev,fe_inverse(b_prev));
        set_next_facet(b_prev,fe_inverse(a_prev));
        set_prev_facet(b_next,fe_inverse(a_next));
        set_edge_fe(keep_edge,fe_inverse(a_next));
      }
              
      /* fix edge references around deleted edge to refer to kept edge */
      next_fea = get_edge_fe(keep_edge);
      if ( valid_id(next_fea) )
        do
        { set_fe_edge(next_fea,keep_edge);
          next_fea = get_next_facet(next_fea);
        }
        while ( next_fea != get_edge_fe(keep_edge) );

      /* fix phase tension */
      if ( (web.representation == STRING) && phase_flag )
          set_e_phase_density(keep_edge);

      /* fix method instances. Oppositely oriented signed methods cancel;
         others combined.                                                   */
      { struct edge *ea_ptr = eptr(keep_edge);
        struct edge *eb_ptr = eptr(throw_edge);
        int meth_offset = get_meth_offset(EDGE);
        int *ameths = (int*)((char*)ea_ptr+meth_offset);
        int *bmeths = (int*)((char*)eb_ptr+meth_offset);
        int i,j;
        for ( i = 0 ; i < (int)eb_ptr->method_count ; i++ )
        { int m;
          int handled = 0;
  
          int orientable;
          m = abs(bmeths[i]);
          orientable = basic_gen_methods[METH_INSTANCE(m)->gen_method].flags &
                           ORIENTABLE_METHOD;
          for ( j = 0 ; j < (int)ea_ptr->method_count ; j++ )
          { int n;
            n = abs(ameths[j]);
            if ( n == m ) 
            { /* see if orientable and cancellable */
              if ( orientable )
              { /* drop from keep_edge */
                ameths[j] = ameths[--ea_ptr->method_count];
              }
              /* else leave alone */
              handled = 1;
              break;
            }
          }
          if ( handled ) continue;
          /* Not found on keep_edge, so paste onto keep_edge */
          if ( orientable && same_sign(keep_edge,throw_edge) )
            ameths[ea_ptr->method_count++] = -bmeths[i];
          else
            ameths[ea_ptr->method_count++] = bmeths[i];
       } /* end b meth loop */
      } /* end method adjust */

        
wasloop:

#ifdef MPI_EVOLVER
      mpi_note_facet_delete(facet,keep_edge,throw_edge);
#endif

      /* free structures  */
      if ( !equal_element(a_edge,b_edge) ) 
        free_element(throw_edge);
      if ( !valid_id(get_edge_fe(keep_edge)) )  /* get rid of bare edge */
        free_element(keep_edge);
      if ( !valid_id(get_vertex_edge(third_v)) ) /* get rid of bare vertex */
        free_element(third_v);
      if ( !equal_element(a,b) ) 
        free_element(a);
      else
      { /* get rid of loop edge */
        free_element(get_fe_edge(a));
      }
      free_element(b);
      
      if ( valid_id(facet) )
        free_element(facet); /*  also takes care of body facet links */

         
      if ( everything_quantities_flag && (get_eattr(keep_edge) 
             & (BOUNDARY|CONSTRAINT)) )
      { /* kludge to correct for getting edge content integral back */
        fixup_edge_content_meths(keep_edge);

      }
    
      
   }
   next_base = get_next_facet(base_fe);
   free_element(base_fe);
   base_fe = next_base;
   if ( equal_id(base_fe,first_fe) ) base_fe = NULLID;
  }  /* end facets around short edge loop */

  if ( !equal_id(elim_v,keep_v) )    /* see note above */
     free_element(elim_v);
  if ( !valid_id(get_vertex_edge(keep_v)) ) /* get rid of bare vertex */
  { free_element(keep_v);
    retval = 1; 
    goto eliminate_edge_exit;
  }


      /* note: short_edge cannot be eliminated here since caller
          has to use it to continue edge generation.  Caller 
          cannot save next edge before calling, since this routine
          frees other edges, which might include the saved one. 
      */

 if ( web.representation == STRING && everything_quantities_flag &&
    (get_vattr(keep_v) & (BOUNDARY|CONSTRAINT)) )
 { /* kludge to correct for getting edge content integral back */
    fixup_vertex_content_meths(keep_v);
 }

 if ( web.modeltype == QUADRATIC )
  { /* fix up adjacent midpoints  */
    edge_id start_e = get_vertex_edge(keep_v);
    edge_id ea = start_e;
    int bailcount = 0;

    if ( valid_id(ea) )
    { do
      { new_vertex_average(get_edge_midv(ea),VOLKEEP);
        ea = get_next_tail_edge(ea);
        if ( bailcount++ > 1000 ) break;
      } while ( !equal_id(ea,start_e) );
    }
  }
  retval = 1;

eliminate_edge_exit:
  LEAVE_GRAPH_MUTEX;

  top_timestamp = ++global_timestamp;
  return retval;
} /* end eliminate_edge() */

/**************************************************************************
*
*  Function: body_facet_fixup()
*  purpose: Fix up any body facet links to a removed facet.
*/

void body_facet_fixup(facet)
facet_id facet;
{
  body_id b_id;
  facet_id bf_id;
  facet_id f_id;

  /* fix any body references to facet */
  if ( valid_id(facet) )
  { b_id = get_facet_body(facet);
    bf_id = get_body_facet(b_id);
    if ( equal_element(facet,bf_id) )
    { /* need to reset body facet */
      set_body_facet(b_id,NULLID);  /* in case none found */
      FOR_ALL_FACETS(f_id)
      { if ( equal_element(f_id,facet) ) continue;
        if ( equal_id(get_facet_body(f_id),b_id) )
        { set_body_facet(b_id,f_id); break; }
        if ( equal_id(get_facet_body(inverse_id(f_id)),b_id) )
        { set_body_facet(b_id,inverse_id(f_id)); break; }
      }
    }
    b_id = get_facet_body(inverse_id(facet));
    bf_id = get_body_facet(b_id);
    if ( equal_element(facet,bf_id) )
    { /* need to reset body facet */
      set_body_facet(b_id,NULLID);  /* in case none found */
      FOR_ALL_FACETS(f_id)
      { invert(f_id);
        if ( equal_element(f_id,facet) ) continue;
        if ( equal_id(get_facet_body(f_id),b_id) )
        { set_body_facet(b_id,f_id); break; }
        if ( equal_id(get_facet_body(inverse_id(f_id)),b_id) )
        { set_body_facet(b_id,inverse_id(f_id)); break; }
      }
    }
  }
}


/************************************************************
*
*  Function: change_vertex()
*
*  Purpose:  Recursively goes through facetedges with tail
*            at old vertex and put tail at new vertex.
*            To seek new edges, looks at facet loop around
*            edge predecessor.
*/

void change_vertex(fe,old_v,new_v,wrap)
facetedge_id fe;  /* known to have tail at old vertex */
vertex_id  old_v; /* old vertex */
vertex_id  new_v; /* new vertex */
WRAPTYPE wrap;  /* wraps to add to edges */
{
  facetedge_id next_fe;  /* looping around current edge */
  facetedge_id pre_fe;    /* going back around facet to new edge */
  edge_id e_id;

  if ( equal_id(old_v,new_v) )
      kb_error(1346,"Internal error: change_vertex: looping edge! ",WARNING);


  e_id = get_fe_edge(fe);
  set_edge_tailv(e_id,new_v);
  if ( web.symmetry_flag )
        set_edge_wrap(e_id,(*sym_compose)(wrap,get_edge_wrap(e_id)));

  next_fe = fe;
  do
     { pre_fe = fe_inverse(get_prev_edge(next_fe));
        if ( equal_id(get_fe_tailv(pre_fe),old_v) )
          change_vertex(pre_fe,old_v,new_v,wrap);
        next_fe = get_next_facet(next_fe);
     }
  while ( !equal_id(next_fe,fe) );
}


/*******************************************************************
*
*  Function: articulate()
*
*  Purpose: Subdivides all existing edges longer than a given length, and
*              re-triangulates accordingly.
*
*  Input:    REAL max_len - upper bound for final edges
*
*  Output:  all edges less than max_len
*
*  Return value: number of facets created
*/

#ifdef ANSI_DEF
int articulate(
REAL max_len)  /* maximum allowed edge length */
#else
int articulate(max_len)
REAL max_len;  /* maximum allowed edge length */
#endif
{
  edge_id e_id,sentinel;
  int new_edge_count = 0;

  web.vol_flag = 0;

  /* first, a little error check */
  if ( max_len <= 0.0 ) 
     kb_error(1347,"Must have positive minimum length.\n",RECOVERABLE);

  if ( web.representation == SIMPLEX ) return simplex_long_edges(max_len);

  ENTER_GRAPH_MUTEX;

  /* main loop over all edges */
  e_id = NULLEDGE;
  while ( generate_all(EDGE,&e_id,&sentinel) )
  { REAL side_len;  /* actual side length */

    calc_edge(e_id);
    side_len = get_edge_length(e_id);
    if ( side_len > max_len ) 
    {
      edge_refine(e_id);
      new_edge_count++;
    }
  }

  LEAVE_GRAPH_MUTEX;

  if ( new_edge_count > 0 ) top_timestamp = ++global_timestamp;
    return new_edge_count;
} 

/*************************************************************
*
*  Function: edge_refine()
*
*  Purpose: subdivide an edge of a triangulation, and also
*              subdivide adjacent facets.
*
*  Input: edge id of edge to refine
*
*  Output:  ID of new edge half 
*/

edge_id edge_refine(e_id)
edge_id e_id;
{
  facetedge_id fe; /* for facet being subdvided */
  edge_id new_e;

  if ( !valid_element(e_id) )
    return NULLID;
    
  if ( web.modeltype == LAGRANGE )
  kb_error(2199,"Cannot refine an edge in Lagrange model. Suggest reverting to quadratic.\n",RECOVERABLE);

  ENTER_GRAPH_MUTEX;
  new_e = edge_divide(e_id);  /* e_id now tail half of old edge */

  if ( web.representation == SOAPFILM )
  { facetedge_id first_fe;
    /* now go around facet loop of edge dividing facets */
    fe = first_fe = get_edge_fe(e_id);
    if ( valid_id(fe) ) do
    { if ( get_vattr(get_edge_tailv(e_id)) & AXIAL_POINT )
      { facetedge_id ffe = get_next_edge(fe);
        facet_id f_id = get_fe_facet(fe);
        cross_cut(ffe,get_next_edge(ffe));
        if ( inverted(f_id) ) 
          set_facet_fe(inverse_id(f_id),inverse_id(get_prev_edge(fe)));
        else 
          set_facet_fe(f_id,fe);
      }
      else
        cross_cut(get_prev_edge(fe),fe);
      fe = get_next_facet(fe);
    } while ( !equal_id(fe,first_fe) );
  }
  LEAVE_GRAPH_MUTEX;

  top_timestamp = ++global_timestamp;
  return new_e;
}

/***********************************************************************
*
* function: equiangulate_edge()
*
* purpose: Test given edge for equiangulation, and swap it if it passes.
*
* return: 1 if edge swapped, 0 if not.
*/
int did_global_edge_calc = 0;  /* efficiency measure */

int equiangulate_edge(e_id)
edge_id e_id;
{ facetedge_id fe_a; /* for edge under test */
  facetedge_id fe_ai; /* other facetedge of e_id */
  REAL a;  /* length of e_id */
  facetedge_id fe_b,fe_c; /* other sides of one triangle */
  REAL b,c;  /* lengths of other sides of one triangle */
  facetedge_id fe_d,fe_e; /* other sides of other triangle */
  REAL d,e;  /* lengths of other sides of other triangle */
  facet_id f1,f2; 

  if ( get_eattr(e_id) & FIXED ) 
    return 0;

  /* test to be sure edge has exactly two adjacent facets */
  fe_a = get_edge_fe(e_id);
  if ( !valid_id(fe_a) ) 
    return 0; /* might be bare edge */
  fe_ai = get_next_facet(fe_a);
  if ( equal_id(fe_a,fe_ai) ) 
    return 0;
  if ( !equal_id(fe_ai,get_prev_facet(fe_a)) ) 
    return 0;

  f1 = get_fe_facet(fe_a);
  f2 = get_fe_facet(get_next_facet(fe_a));

  /* test for equal density */
  if ( (get_fattr(f1)&DENSITY) || (get_fattr(f2)&DENSITY) )
    if ( fabs(get_facet_density(f1) - get_facet_density(f2)) > 1e-10 )
        return 0;

  /* test for equal constraints */
  if ( !equal_constr(e_id,f1) || !equal_constr(e_id,f2) )
    return 0;
 
  /* test for equal boundary */
  if ( ( get_edge_boundary(e_id) != get_facet_boundary(f1))
      ||( get_edge_boundary(e_id) != get_facet_boundary(f2)) )
    return 0;

  /* test equiangularity */
  if ( !did_global_edge_calc ) 
    calc_edge(e_id);
  a = get_edge_length(e_id);
  fe_b = get_next_edge(fe_a);
  if ( !did_global_edge_calc ) 
    calc_edge(get_fe_edge(fe_b));
  b = get_edge_length(get_fe_edge(fe_b));
  fe_c = get_prev_edge(fe_a);
  if ( !did_global_edge_calc ) 
    calc_edge(get_fe_edge(fe_c));
  c = get_edge_length(get_fe_edge(fe_c));
  if ( b*c == 0.0 ) 
    return 0;
  fe_ai = fe_inverse(get_next_facet(fe_a));
  fe_d = get_next_edge(fe_ai); 
  if ( !did_global_edge_calc ) 
    calc_edge(get_fe_edge(fe_d));
  d = get_edge_length(get_fe_edge(fe_d));
  fe_e = get_prev_edge(fe_ai); 
  if ( !did_global_edge_calc ) 
    calc_edge(get_fe_edge(fe_e));
  e = get_edge_length(get_fe_edge(fe_e));
  if ( e*d == 0.0 ) 
    return 0;
  if ( (b*b + c*c - a*a)/b/c + (d*d + e*e - a*a)/d/e > -0.001 )
      return 0;
                                         /* -0.01 prevents cycling */

  /* test acuteness ??? (fix 0.0001 to make scale invariant)*/
/*        if ( a*a + d*d <= e*e + 0.0001 ) return 0;
        if ( a*a + e*e <= d*d + 0.0001 ) return 0;
        if ( a*a + b*b <= c*c + 0.0001 ) return 0;
        if ( a*a + c*c <= b*b + 0.0001 ) return 0;
        if ( a*a + d*d <= e*e + 0.0001 ) return 0;
*/
  /* may want to switch, but test that opposite vertices are different */
  if ( equal_id(get_fe_tailv(fe_c),get_fe_headv(fe_d)) ) return 0;
  if ( eartest(get_fe_tailv(fe_c),get_fe_headv(fe_d)) ) return 0;
   /* if we are here, we want to switch diagonals */
  return do_edgeswap(e_id);
} /* end equiangulate_edge() */ 

/*************************************************************
*
*  Function: equiangulate()
*
*  Purpose:  Switch diagonals of quadrilaterals around so
*            that the triangulation becomes more equiangular.
*            The criterion used is that for a Delauney triangulation:
*            the sum of the angles opposite a common base of
*            two triangles should be less than pi.  Only 
*            quadrilaterals within a face are examined, and
*            fixed edges are not affected.  Angles next to
*            diagonal must be acute.
*
*  Input:     Triangulation.
*
*  Output:    One pass made through all edges, and diagonals
*                switched if appropriate.
*
*  Return value: Number of edges switched.
*/

int equiangulate()
{
  int switchcount = 0;
  edge_id e_id;  /* edge being examined */
  edge_id sentinel;

  if ( web.modeltype == LAGRANGE )
     kb_error(1348,"Cannot equiangulate LAGRANGE model.\n",RECOVERABLE);

  if ( web.dimension == 1 )
     kb_error(2200,"Cannot equiangulate string model.\n",RECOVERABLE);

  if ( web.representation == SIMPLEX ) {  return simplex_equiangulate(); }
  
  web.vol_flag = 0;

  MFOR_ALL_EDGES(e_id)
    calc_edge(e_id);
  did_global_edge_calc = 1;

  /* main loop through edges */
  ENTER_GRAPH_MUTEX;
  e_id = NULLEDGE;
  while ( generate_all(EDGE,&e_id,&sentinel) )
    switchcount += equiangulate_edge(e_id);
  LEAVE_GRAPH_MUTEX;

  did_global_edge_calc = 0;
  return switchcount;
}

/*************************************************************************
*
*  function: eartest()
*
*  purpose: see if edge swapping would create an ear. Counts
*      edges already connecting proposed points.
*
*  Input: The two vertex id's
*
*  Output: 1 if ear would be created. 0 else.
*/

int eartest(v1,v2)
vertex_id v1,v2;
{ edge_id e_id,start_id;

  e_id = start_id = get_vertex_edge(v1);
  do
  { if ( equal_id(v2,get_edge_headv(e_id)) ) return 1; 
     e_id = get_next_tail_edge(e_id);
  } while ( e_id != start_id );
  return 0;
}


/*************************************************************
*
*  Function: edgeswap()
*
*  Purpose:  Switch diagonal of a quadrilateral as in equiangulation,
*                but without the test of improving equiangularity.
*
*  Input:     Edge to try to swap.
*
*  Output:    Number of edges swapped.
*
*/

int edgeswap(e_id)
edge_id e_id;
{
  facetedge_id fe_a; /* for edge under test */
  facetedge_id fe_ai; /* other facetedge of e_id */
  facetedge_id fe_c; /* other side of one triangle */
  facetedge_id fe_d; /* other side of other triangle */
  facet_id f1,f2;

  web.vol_flag = 0;
  if ( get_eattr(e_id) & FIXED ) 
  { if ( verbose_flag )
    { sprintf(msg,"Not swapping fixed edge %s\n",ELNAME(e_id));
      outstring(msg);
    }
    return 0;
  }
      
  /* test to be sure edge has exactly two adjacent facets */
  fe_a = get_edge_fe(e_id);
  if ( !valid_id(fe_a) ) /* might be bare edge */
  { if ( verbose_flag )
    { sprintf(msg,"Not swapping bare edge %s\n",ELNAME(e_id));
       outstring(msg);
    }
    return 0;
  }
  fe_ai = get_next_facet(fe_a);
  if ( equal_id(fe_a,fe_ai) ) 
  { if ( verbose_flag )
    { sprintf(msg,"Not swapping single-facet edge %s\n",ELNAME(e_id));
       outstring(msg);
    }
    return 0;
  }

  if ( !equal_id(fe_ai,get_prev_facet(fe_a)) )
  { if ( verbose_flag )
    { sprintf(msg,"Not swapping edge %s; more than two facets.\n",ELNAME(e_id));
       outstring(msg);
    }
    return 0;
  }

  f1 = get_fe_facet(fe_a);
  f2 = get_fe_facet(get_next_facet(fe_a));

  /* test for equal density */
  if ( (get_fattr(f1)&DENSITY) || (get_fattr(f2)&DENSITY) )
    if ( fabs(get_facet_density(f1) - get_facet_density(f2)) > 1e-10 )
    { if ( verbose_flag )
      { sprintf(msg,"Not swapping edge %s. Unequal facet densities.\n",
                ELNAME(e_id)); outstring(msg);
       }
       return 0;
    }

  /* test for equal no_refine and fixedness */
  if ( (get_fattr(f1)&(FIXED)) != (get_fattr(f2)&(FIXED)) )
  { if ( verbose_flag )
    { sprintf(msg,"Not swapping edge %s, facets have unequal FIXED.\n",
         ELNAME(e_id));
       outstring(msg);
    }
    return 0;
  }
  if ( (get_fattr(f1)&(NO_REFINE)) != 
                (get_fattr(f2)&(NO_REFINE)) )
  { if ( verbose_flag )
    { sprintf(msg,"Not swapping edge %s, facets have unequal NO_REFINE.\n",
         ELNAME(e_id));
       outstring(msg);
    }
    return 0;
  }

  /* test for equal constraints */
  if ( !equal_constr(e_id,f1) || !equal_constr(e_id,f2) )
    { if ( verbose_flag) 
      { sprintf(msg,"Not swapping edge %s. Unequal edge and facet constraints.\n",
                ELNAME(e_id)); outstring(msg);
      }
       return 0;
    }
 
  /* test for equal boundary */
  if ( ( get_edge_boundary(e_id) != get_facet_boundary(f1))
            ||( get_edge_boundary(e_id) != get_facet_boundary(f2)) )
    { if ( verbose_flag )
      { sprintf(msg,"Not swapping edge %s. Unequal facet and edge boundaries.\n",
                ELNAME(e_id)); outstring(msg);
      }
       return 0;
    }

  fe_c = get_prev_edge(fe_a);
  fe_ai = fe_inverse(get_next_facet(fe_a));
  fe_d = get_next_edge(fe_ai); 

  /* may want to switch, but test that opposite vertices are different */
  if ( equal_id(get_fe_tailv(fe_c),get_fe_headv(fe_d)) ) 
    { if ( verbose_flag )
      { sprintf(msg,"Not swapping edge %s. Would be a loop on vertex %s.\n",
                ELNAME(e_id),ELNAME1(get_fe_tailv(fe_c))); outstring(msg);
      }
      return 0;
    }
  if ( eartest(get_fe_tailv(fe_c),get_fe_headv(fe_d)) )
    { if (verbose_flag )
      { sprintf(msg,"Not swapping edge %s. Would create two facets with same vertices.\n",
                ELNAME(e_id)); outstring(msg);
      }
      return 0;
    }

    return do_edgeswap(e_id);
}

/***************************************************************************
*
* Function: test_axial_points()
*
* Purpose: make sure axial point at start of facet.
*/

void test_axial_points(f_id)
facet_id f_id;
{ facetedge_id fe;
  int i;

  if ( inverted(f_id) ) f_id = inverse_id(f_id);
  fe = get_facet_fe(f_id);
  for ( i = 0 ; i < FACET_EDGES ; i++ )
  { vertex_id v_id;
    v_id = get_fe_tailv(fe);
    if ( get_vattr(v_id) & AXIAL_POINT )
    { set_facet_fe(f_id,fe); return; }
    fe = get_next_edge(fe);
  }
}

/***************************************************************************
*
* function do_edgeswap()
*
* purpose: common actual edgeswapper for equiangulation and edgeswap.
*
* return: 1 if edge swapped, 0 if not.
*/

int do_edgeswap(e_id)
edge_id e_id;
{
  vertex_id v_id;
  facetedge_id fe_a; /* for edge under test */
  facetedge_id fe_ai; /* other facetedge of e_id */
  facetedge_id fe_b,fe_c; /* other sides of one triangle */
  facetedge_id fe_d,fe_e; /* other sides of other triangle */


  fe_a = get_edge_fe(e_id);
  fe_b = get_next_edge(fe_a);
  fe_c = get_prev_edge(fe_a);
  fe_ai = fe_inverse(get_next_facet(fe_a));
  fe_d = get_next_edge(fe_ai); 
  fe_e = get_prev_edge(fe_ai); 

  /* special check so axial point does not become head of swapped edge */
  if ( get_vattr(get_fe_tailv(fe_c)) & AXIAL_POINT )
  { int retval;
    if ( get_vattr(get_fe_headv(fe_d)) & AXIAL_POINT )
    { if ( verbose_flag ) 
      { sprintf(msg,"Not swapping edge %s. Would create edge with both ends axial points.\n",
                ELNAME(e_id)); outstring(msg);
      }
      return 0;
    }
    set_edge_fe(e_id,inverse_id(fe_ai)); 
    retval = edgeswap(e_id); 
    return retval;
  }

  /* if we are here, we want to switch diagonals */
  if ( verbose_flag )
  { sprintf(msg,"Swapping edge %s\n",ELNAME(e_id)); outstring(msg); 
  }
  remove_vertex_edge(get_edge_tailv(e_id),e_id);
  remove_vertex_edge(get_edge_headv(e_id),inverse_id(e_id));
  set_facet_fe(get_fe_facet(fe_a),fe_a);
  set_facet_fe(get_fe_facet(fe_ai),fe_ai);
  set_fe_facet(fe_b,get_fe_facet(fe_ai));
  set_fe_facet(fe_d,get_fe_facet(fe_a));
  v_id = get_fe_tailv(fe_c);
  set_edge_headv(e_id,v_id);
  v_id = get_fe_headv(fe_d);
  set_edge_tailv(e_id,v_id);
  set_next_edge(fe_a,fe_c);
  set_prev_edge(fe_a,fe_d);
  set_next_edge(fe_ai,fe_e);
  set_prev_edge(fe_ai,fe_b);
  set_next_edge(fe_b,fe_ai);
  set_prev_edge(fe_b,fe_e);
  set_next_edge(fe_c,fe_d);
  set_prev_edge(fe_c,fe_a);
  set_next_edge(fe_d,fe_a);
  set_prev_edge(fe_d,fe_c);
  set_next_edge(fe_e,fe_b);
  set_prev_edge(fe_e,fe_ai);
  calc_edge(e_id);
  if ( web.symmetry_flag )
  { test_axial_points(get_fe_facet(fe_a));
    test_axial_points(get_fe_facet(fe_ai));
  }

  if ( web.symmetry_flag )
  { 
    edge_id ed,ec;
    WRAPTYPE w;

    if ( get_vattr(get_fe_tailv(fe_d)) & AXIAL_POINT )
    { ed = get_fe_edge(fe_b);
      ec = get_fe_edge(fe_e);
      w = (*sym_compose)(get_edge_wrap(ec),get_edge_wrap(ed));
      set_edge_wrap(e_id,w);
    }
    else
    { ed = get_fe_edge(fe_d);
      ec = get_fe_edge(fe_c);
      w = (*sym_compose)(get_edge_wrap(ec),get_edge_wrap(ed));
      set_edge_wrap(e_id,(*sym_inverse)(w));
    }
    if ( ed && (web.modeltype == QUADRATIC) )
    { /* adjust coordinates of midpoint */
      REAL *x = get_coord(get_edge_midv(e_id));
      torus_wrap(x,x,(*sym_inverse)(get_edge_wrap(ed)));
    }
  }
  if ( web.modeltype == QUADRATIC )
    new_vertex_average(get_edge_midv(e_id),VOLKEEP);

#ifdef MPI_EVOLVER
  /* Record changed facet-edges for synchronization */
  if ( id_task(fe_a) != this_task )
    mpi_add_to_synch_list(fe_a);
  if ( id_task(fe_ai) != this_task )
    mpi_add_to_synch_list(fe_ai);
  if ( id_task(fe_b) != this_task )
    mpi_add_to_synch_list(fe_b);
  if ( id_task(fe_c) != this_task )
    mpi_add_to_synch_list(fe_c);
  if ( id_task(fe_d) != this_task )
    mpi_add_to_synch_list(fe_d);
  if ( id_task(fe_e) != this_task )
    mpi_add_to_synch_list(fe_e);
#endif
    

  top_timestamp = ++global_timestamp;
  return 1;
} /* end do_edgeswap */

/***********************************************************************
*
*  function: vertex_angle()
*
*  purpose: calculate angle at vertex in string model.
*
*/

REAL vertex_angle(v_id)
vertex_id v_id;
{
  REAL b[MAXCOORD];  /* edge vectors */
  REAL angle;
  REAL bb;
  edge_id ea,eb;
  REAL ac;
  REAL netforce[MAXCOORD];
  int n;
  int count = 0;

  for ( n = 0 ; n < SDIM ; n++ ) netforce[n] = 0.0;

  eb = ea = get_vertex_edge(v_id);
  do {
     if ( !valid_id(eb) ) return 0.0;
     get_edge_side(eb,b);
     bb = sqrt(SDIM_dot(b,b));
     if ( bb > 0.0 )
        for ( n = 0 ; n < SDIM ; n++ ) netforce[n] += b[n]/bb;
     count++;
     eb = get_next_tail_edge(eb); 
  } while ( !equal_element(ea,eb) );
  if ( count < 2 ) return 0.0;
  ac = sqrt(SDIM_dot(netforce,netforce));
  angle = (ac >= 2.0) ? M_PI : 2*asin(ac/2);
  return angle;
}
    
/***********************************************************************
*
*  function: dihedral()
*
*  purpose: calculate dihedral angle between facets on edge.
*
*  return:  angle in radians if exactly two facets
*              0 otherwise
*
*  method: uses 2-vector dot product for cosine angle
*/

REAL dihedral(e_id)
edge_id e_id;
{
  facetedge_id fe_a; /* for edge under test */
  facetedge_id fe_ai; /* other facetedge of e_id */
  facetedge_id fe_b; /* other side of one triangle */
  facetedge_id fe_d; /* other side of other triangle */
  REAL a[MAXCOORD],b[MAXCOORD],d[MAXCOORD];  /* edge vectors */
  REAL denom,costh;
  REAL aa,bb,dd,ad,ab,bd;

  /* test to be sure edge has exactly two adjacent facets */
  if ( get_vattr(get_edge_tailv(e_id)) & AXIAL_POINT )
    e_id = inverse_id(e_id);
  fe_a = get_edge_fe(e_id);
  if ( !valid_id(fe_a) )  return 0.0;
  if ( !equal_id(get_next_facet(fe_a),get_prev_facet(fe_a)) ) return 0.0;
  fe_b = fe_inverse(get_prev_edge(fe_a));
  fe_ai = fe_inverse(get_next_facet(fe_a));
  fe_d = get_next_edge(fe_ai); 
  if ( equal_id(fe_b,fe_d) ) return 0.0;

  /* test parallelism */
  get_fe_side(fe_a,a);
  get_fe_side(fe_b,b);
  get_fe_side(fe_d,d);
  /* works in general dimension */
  aa = SDIM_dot(a,a);
  bb = SDIM_dot(b,b);
  dd = SDIM_dot(d,d);
  ab = SDIM_dot(a,b);
  ad = SDIM_dot(a,d);
  bd = SDIM_dot(b,d);
  denom = (aa*bb - ab*ab)*(aa*dd - ad*ad);
  if ( denom <= 0.0 ) return 0.0;
  costh = (ab*ad - aa*bd)/sqrt(denom);
  if ( costh > 1.0 ) return 0.0;
  if ( costh < -1.0 ) return M_PI;
  return acos(costh);

}

/******************************************************************
*
*  Function:  ridge_notcher()
*
*  Purpose:    Subdivide internal edges in a face whose adjacent
*                 faces are non-parallel to a certain degree.
*
*  Input:      REAL max_angle - maximum allowed angle of deviation
*
*  Output:     offending edges subdivided, with middle vertex set
*                 to the average of the four adjacent vertices
*
*/

int ridge_notcher(max_angle)
REAL max_angle;
{
  int notchcount = 0;  /* number of edges notched */
  edge_id e_id;          /* edge to notch */
  facet_id *todo;     /* for list of facets to refine */
  int i;
  int maxfacet = web.skel[FACET].max_ord+1;
  edge_id sentinel;

  web.vol_flag = 0;

  todo = (facet_id *)temp_calloc(web.skel[FACET].max_ord+1,sizeof(facet_id));

  /* first, unmark all NEWEDGE attributes */
  FOR_ALL_EDGES(e_id)
      unset_attr(e_id,NEWEDGE);

  /* main loop over all edges */
  e_id = NULLEDGE;
  while ( generate_all(EDGE,&e_id,&sentinel) )
  { REAL angle;
    facetedge_id fe_a;
    facet_id f_id;

    if ( get_eattr(e_id) & (FIXED | BOUNDARY | NEWEDGE) )
        continue;

    angle = dihedral(e_id);
    if ( angle < max_angle )
      continue;

    /* record neighboring facets for subdivision */
    fe_a = get_edge_fe(e_id);
    f_id = get_fe_facet(fe_a);
    todo[loc_ordinal(f_id)] = f_id;
    f_id = get_fe_facet(get_next_facet(fe_a));
    todo[loc_ordinal(f_id)] = f_id;
    if ( verbose_flag )
    { sprintf(msg,"Notching edge %s\n",ELNAME(e_id));
      outstring(msg);
    }

  }

  for ( i = 0 ; i < maxfacet ; i++ )
     if ( valid_id(todo[i]) )
        { face_triangulate(todo[i],FACET_EDGES);
          notchcount++;
        }
  temp_free((char*)todo);

  if ( notchcount > 0 ) top_timestamp = ++global_timestamp;
  return notchcount;
}

/******************************************************************
*
*  Function:  ridge_histogram()
*
*  Purpose:    Calculate histogram of ridge angles.
*
*
*/

void ridge_histogram()
{
  edge_id e_id;          /* edge to notch */
  int bincount[HISTO_BINS];
  int n;

  for ( n = 0 ; n < HISTO_BINS ; n++ ) bincount[n] = 0;


  /* main loop over all edges */
  FOR_ALL_EDGES(e_id)
     { REAL angle;

        if ( get_eattr(e_id) & (FIXED | BOUNDARY ) ) continue;

        angle = dihedral(e_id);
        if ( angle == 0.0 ) n = 0;
        else
          n = HISTO_BINS/2 + 1 + (int)floor(log(angle)*HISTO_BINSIZE);
        if ( n < 0 ) n = 0;
        if ( n >= HISTO_BINS ) n = HISTO_BINS - 1;
        bincount[n]++;

     }

  /* print histogram */
  outstring("         angle                 number\n");
  if ( bincount[0] )
     {
        sprintf(msg,"%f - %f      %6d \n",0.0,
         (DOUBLE)(exp((-HISTO_BINS/2)/HISTO_BINSIZE)), bincount[0]);
        outstring(msg);
     }
  for ( n = 1 ; n < HISTO_BINS ; n++ )
    if ( bincount[n] )
     {
        sprintf(msg,"%f - %f      %6d\n",
                  (DOUBLE)(exp((n-HISTO_BINS/2-1)/HISTO_BINSIZE)),
                  (DOUBLE)(exp((n-HISTO_BINS/2)/HISTO_BINSIZE)),bincount[n]);
        outstring(msg);
     }

}


#ifdef UNDER_DEVELOPMENT
/*******************************************************************
*
*  function: merge_collapsed_facets()
*
*  purpose: Merge facets that have the same three vertices.
*
*/

void merge_collapsed_facets()
{
  facetedge_id fe,next_fe,sentinel;
  vertex_id v1,v2;

  fe = NULLFACETEDGE;
  while ( generate_all(FACETEDGE,&fe,&sentinel) )
  { next_fe = get_next_facet(fe);
    if ( equal_id(fe,next_fe) ) continue;  /* only one facet on edge */
    v1 = get_fe_headv(get_next_edge(fe));
    v2 = get_fe_headv(get_next_edge(next_fe));
    if ( equal_id(v1,v2) )
    { /* merge */
             
    }
  }
}
#endif

/***************************************************************************
 *   
 *   function: fixup_vertex_content_meths()
 *
 *   purpose: Make sure vertex has right set of content method instances.
 *            To be called during eliminate_edge().
 *
 *   algorithm: Delete old content methods, then go through adjacent 
 *              facets and look for bodies.
 */
void fixup_vertex_content_meths(v_id)
vertex_id v_id;
{ int  meth_offset = get_meth_offset(VERTEX); 
  struct vertex *v_ptr = (struct vertex*)vptr(v_id);
  int *instlist = (int*)((char*)v_ptr + meth_offset);
  facetedge_id start_fe,fe;
  conmap_t *map;
  struct boundary *bdry = get_vertex_boundary(v_id);
  int attr;
  int i,k;
  char name[200];
  
  if ( !everything_quantities_flag )
    return;

  attr = get_vattr(v_id);
  if ( !(attr & (BOUNDARY|CONSTRAINT)) )
    return;

  /* Delete old content methods. */
  for ( i = 0 ; i < (int)v_ptr->method_count ; i++ )
  { if ( METH_INSTANCE(instlist[i])->flags & BODY_INSTANCE )
      instlist[i] = instlist[--v_ptr->method_count];
  }

  map = get_v_constraint_map(v_id);

  /* Add current content methods */  
  start_fe = fe = get_vertex_fe(v_id);
  if ( valid_id(fe) )
  do
  { int sides;
    facet_id f_id = get_fe_facet(fe);
    vertex_id vv_id = v_id;
    for ( sides = 0 ; sides < 2 ; sides++ )
    { body_id b_id;
    
      b_id = get_facet_body(f_id);
      if ( valid_id(f_id) && valid_id(b_id) && !(get_fattr(f_id) & NONCONTENT) )
      { if ( attr & CONSTRAINT )
          for ( k = 1 ; k <= (int)map[0] ; k++ )
          { struct constraint *con = get_constraint(map[k]);
            if ( con->attr & CON_CONTENT )
            { if ( con->attr & NAMED_THING )
                 sprintf(name,"body_%d_%s_meth",ordinal(b_id)+1,con->name);
              else
                 sprintf(name,"body_%d_con_%d_meth",ordinal(b_id)+1,map[k]);
              apply_method(inverse_id(vv_id),name); 
            }
          }
       if ( bdry && (bdry->attr & CON_CONTENT) )
       { sprintf(name,"body_%d_bdry_%d_meth",ordinal(b_id)+1,get_vertex_boundary_num(v_id));
         apply_method(inverse_id(vv_id),name);
       }
      }
      f_id = inverse_id(f_id);
      v_id = inverse_id(vv_id);
    }
    fe = get_next_facet(fe);
  } while ( !equal_id(fe,start_fe) );
} /* end fixup_vertex_content_meths() */


/***************************************************************************
 *   
 *   function: fixup_edge_content_meths()
 *
 *   purpose: Make sure edge has right set of content method instances.
 *            To be called during eliminate_edge().
 *
 *   algorithm: Delete old content methods, then go through adjacent 
 *              facets and look for bodies.
 */
void fixup_edge_content_meths(e_id)
edge_id e_id;
{ int  meth_offset = get_meth_offset(EDGE); 
  struct edge *e_ptr = (struct edge*)eptr(e_id);
  int *instlist = (int*)((char*)e_ptr + meth_offset);
  facetedge_id start_fe,fe;
  conmap_t *map;
  struct boundary *bdry = get_edge_boundary(e_id);
  int attr;
  int i,k;
  char name[200];
  
  if ( !everything_quantities_flag )
    return;

  attr = get_eattr(e_id);
  if ( !(attr & (BOUNDARY|CONSTRAINT)) )
    return;

  /* Delete old content methods. */
  for ( i = 0 ; i < (int)e_ptr->method_count ; i++ )
  { if ( METH_INSTANCE(instlist[i])->flags & BODY_INSTANCE )
      instlist[i] = instlist[--e_ptr->method_count];
  }

  map = get_e_constraint_map(e_id);

  /* Add current content methods */  
  start_fe = fe = get_edge_fe(e_id);
  if ( valid_id(fe) )
  do
  { int sides;
    facet_id f_id = get_fe_facet(fe);
    edge_id ee_id = e_id;
    if ( inverted(f_id) ) invert(ee_id);
    for ( sides = 0 ; sides < 2 ; sides++ )
    { body_id b_id;
    
      b_id = get_facet_body(f_id);
      if ( valid_id(f_id) && valid_id(b_id) && !(get_fattr(f_id) & NONCONTENT) )
      { if ( attr & CONSTRAINT )
          for ( k = 1 ; k <= (int)map[0] ; k++ )
          { struct constraint *con = get_constraint(map[k]);
            if ( con->attr & CON_CONTENT )
            { if ( con->attr & NAMED_THING )
                 sprintf(name,"body_%d_%s_meth",ordinal(b_id)+1,con->name);
              else
                 sprintf(name,"body_%d_con_%d_meth",ordinal(b_id)+1,map[k]);
              apply_method(ee_id,name); 
            }
          }
       if ( bdry && (bdry->attr & CON_CONTENT) )
       { sprintf(name,"body_%d_bdry_%d_meth",ordinal(b_id)+1,get_edge_boundary_num(e_id));
         apply_method(ee_id,name);
       }
      }
      f_id = inverse_id(f_id);
      e_id = inverse_id(ee_id);
    }
    fe = get_next_facet(fe);
  } while ( !equal_id(fe,start_fe) );
} /* end fixup_edge_content_meths() */

