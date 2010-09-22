/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/******************************************************************
*
*  File: modify.c
*
*  Contents:  low-level triangulation modification routines:
*
*            edge_divide() - subdivide an edge
*            cross_cut() -  subdivide a facet
*
*/

#include "include.h"

/******************************************************************
*
*  Function: face_triangulate()
*
*  Purpose:  Triangulate a n-sided face by putting in central vertex.
*/

void face_triangulate(f_id,edgecount)
facet_id f_id;
int edgecount;
{
  int i,k;
  vertex_id center; 
  facetedge_id fe,pre_fe;
  vertex_id rimv;
  edge_id spoke;
  facetedge_id fe_in,fe_out,next_fe;
  REAL *centerx,*x;
  struct boundary *bdry;
  WRAPTYPE wrap = 0;
  facetedge_id first_fe;

  if ( web.modeltype == LAGRANGE ) 
     kb_error(1237,"Can't face_triangulate() in Lagrange model.\n",RECOVERABLE);

  f_id = positive_id(f_id);  /* axial points like positive orientation here */
  
  /* put a new vertex in the center */
  center = new_vertex(NULL,f_id);
  if ( get_fattr(f_id) & FIXED )
     set_attr(center,FIXED);

  /* center coordinates are average of vertices */
  centerx = get_coord(center);
  for ( i = 0 ; i < SDIM ; i++ ) centerx[i] = 0.0;
  fe = first_fe = get_facet_fe(f_id);
  if ( valid_id(fe) ) do
  { REAL w[MAXCOORD];

    x = get_coord(get_fe_tailv(fe));
    if ( web.symmetry_flag )
    { (*sym_wrap)(x,w,wrap);
      x = w;
      wrap = (*sym_compose)(wrap,get_fe_wrap(fe));
    }
    for ( i = 0 ; i < SDIM ; i++ ) 
      centerx[i] += x[i];
    fe = get_next_edge(fe);
  } while ( valid_id(fe) && !equal_id(fe,first_fe) );
  for ( i = 0 ; i < SDIM ; i++ ) centerx[i] /= edgecount;

  /* find centerpoint parameters for facet on boundary */
  if ( get_fattr(f_id) & BOUNDARY )    /* not working for torus */
  { REAL defaultp[MAXCOORD];
    REAL *paramb,*parammid,*xb;
    vertex_id base_v;
    REAL s[MAXCOORD];
    REAL midp[MAXCOORD];
    int no_interp = 0;
    int vercount = 0;

    set_attr(center,BOUNDARY);
    bdry = get_facet_boundary(f_id);
    set_boundary_num(center,bdry->num);
    for ( i = 0 ; i < bdry->pcount ; i++ ) midp[i] = 0.;

    /* center parameters extrapolate from a vertex */
    /* try to find a vertex on same boundary */
    base_v = NULLVERTEX;
    fe = first_fe = get_facet_fe(f_id);
    if ( valid_id(fe) ) do
    { base_v = get_fe_tailv(fe);
      if ( bdry == get_boundary(get_fe_tailv(fe)) )
      { REAL *p = get_param(base_v);
        for ( i = 0 ; i < bdry->pcount ; i++ ) 
          midp[i] += p[i];
      }
      else if ( extra_bdry_attr && 
         (*(int*)get_extra(base_v,extra_bdry_attr) == bdry->num) )
      { REAL *p = (REAL*)get_extra(base_v,extra_bdry_param_attr);
        for ( i = 0 ; i < bdry->pcount ; i++ ) 
          midp[i] += p[i];
      }
      else no_interp = 1;
      vercount++;
      fe = get_next_edge(fe);
    } while ( valid_id(fe) && !equal_id(fe,first_fe) );
    if ( valid_id(base_v) && (bdry == get_boundary(base_v)) )
    { paramb = get_param(base_v);
      xb = get_coord(base_v);
      for ( i = 0 ; i < SDIM ; i++ )
        s[i] = xb[i];  /* displacement vector */
    }
    else
    { paramb = defaultp;
      for ( i = 0 ; i < bdry->pcount ; i++ ) 
		 defaultp[i] = 0.0;
      for ( i = 0 ; i < SDIM ; i++ )
        s[i] = eval(bdry->coordf[i],defaultp,NULLID,NULL);
      sprintf(msg,"Could not find vertex on same boundary as facet %s.\n",
        ELNAME(f_id)); /* not sure if we want to output this or not */
    }

    parammid = get_param(center);
    if ( (bdry->pcount <  2) && (vercount == 0) )
    { for ( i = 0 ; i < bdry->pcount ; i++ )
         parammid[i] = 0;
      for ( k = 0 ; k < SDIM ; k++ )
        centerx[k] = eval(bdry->coordf[k],parammid,center,NULL);
    } 
    if ( (bdry->pcount < 2) || (interp_bdry_param && !no_interp) )
    { for ( i = 0 ; i < bdry->pcount ; i++ )
         parammid[i] = midp[i]/vercount;
      for ( k = 0 ; k < SDIM ; k++ )
        centerx[k] = eval(bdry->coordf[k],parammid,center,NULL);
    } 
    else b_extrapolate(bdry,s,centerx,centerx,paramb,parammid,center);
  }

  /* install edge from rim to center */
  fe = get_facet_fe(f_id);  /* canonical starting point */
  pre_fe = get_prev_edge(fe);
  rimv = get_fe_tailv(fe);
  spoke = new_edge(rimv,center,f_id);
  if ( get_fattr(f_id) & FIXED )
     set_attr(spoke,FIXED);
  if ( get_fattr(f_id) & NO_REFINE )
     set_attr(spoke,NO_REFINE);
     
  if ( web.symmetry_flag )
    set_edge_wrap(spoke,0);
  fe_in = new_facetedge(f_id,spoke);
  set_edge_fe(spoke,fe_in);
  set_prev_edge(fe_in,pre_fe);
  set_next_edge(pre_fe,fe_in);
  fe_out = new_facetedge(f_id,edge_inverse(spoke));
  set_prev_edge(fe_out,fe_in);
  set_next_edge(fe_in,fe_out);
  set_prev_edge(fe,fe_out);
  set_next_edge(fe_out,fe);
  set_next_facet(fe_in,fe_inverse(fe_out));
  set_prev_facet(fe_in,fe_inverse(fe_out));
  set_next_facet(fe_out,fe_inverse(fe_in));
  set_prev_facet(fe_out,fe_inverse(fe_in));
  
  if ( get_fattr(f_id) & BOUNDARY )    
  { set_attr(spoke,BOUNDARY);
    bdry = get_facet_boundary(f_id);
    set_edge_boundary_num(spoke,bdry->num);
  }
  else if ( get_fattr(f_id) & CONSTRAINT )    
  { ATTR attr = get_fattr(f_id) & (BDRY_ENERGY | BDRY_CONTENT | CONSTRAINT );
    conmap_t * conmap = get_f_constraint_map(f_id);

    set_attr(spoke,attr);
    set_attr(center,attr);
    set_e_conmap(spoke,conmap);
    set_v_conmap(center,conmap);
    project_v_constr(center,ACTUAL_MOVE,RESET_ONESIDEDNESS);
    if ( web.modeltype == QUADRATIC )
    { vertex_id mid = get_edge_midv(spoke);
      set_attr(mid,attr);
      set_v_conmap(mid,conmap);
      project_v_constr(mid,ACTUAL_MOVE,RESET_ONESIDEDNESS);
    }
  }

  /* now go around cutting off triangles */
  fe_in = get_edge_fe(spoke);
  while ( !equal_id(get_next_edge(fe),fe_in) )
  { next_fe = get_next_edge(fe);
    cross_cut(get_prev_edge(fe),fe);
    fe = next_fe;
  }    

 if ( web.symmetry_flag ) /* check for axial vertices */
 { fe = get_facet_fe(f_id);
   for ( i = 0 ; i < FACET_VERTS ; i++ )
   { if ( get_vattr(get_fe_tailv(fe)) & AXIAL_POINT )
     { set_facet_fe(f_id,fe); break; }
     fe = get_next_edge(fe);
   }
 }

  top_timestamp = ++global_timestamp;
} /* end face_triangulate */


/*****************************************************************
*
* function: unstar()
*
* purpose: Remove center vertex from known starred triangle;
*    necessary for edge removal of one side. Checks that all
*    interior edges are valence 2.  Does inner edge deletion
*    by hand rather than call eliminate_edge().
*
* return: -1 failure, 0 unremovable star, 1 success
*/

int unstar(fe_a)
facetedge_id fe_a;  /* removable vertex is at tail */
{ vertex_id v_id = get_fe_tailv(fe_a);
  facet_id fkeep = get_fe_facet(fe_a);
  facetedge_id fe_b = get_next_edge(fe_a);
  facetedge_id fe_c = get_next_edge(fe_b);
  facetedge_id fe_d = get_next_facet(fe_c);
  facetedge_id fe_e = get_next_edge(fe_d);
  facetedge_id fe_f = get_next_edge(fe_e);
  facetedge_id fe_g = get_next_facet(fe_e);
  facetedge_id fe_h = get_next_edge(fe_g);
  facetedge_id fe_i = get_next_edge(fe_h);
  facet_id f_id;
  body_id b_id;

  if ( verbose_flag )
  { sprintf(msg,"Unstarring vertex %s\n",ELNAME(v_id));
    outstring(msg);
  }

  /* some checks on legality */
  if ( get_vattr(v_id) & FIXED )  /* fixed vertex */
  { if ( verbose_flag )
    { sprintf(msg,"Unstarring failed, vertex %s is fixed.\n",
                ELNAME(v_id),get_vertex_evalence(v_id));
      outstring(msg);
    }
    return -1;
  }
  if ( get_vertex_evalence(v_id) != 3 )  /* not lonesome star */
  { if ( verbose_flag )
    { sprintf(msg,"Unstarring failed, vertex %s has valence %d\n",
                ELNAME(v_id),get_vertex_evalence(v_id));
      outstring(msg);
    }
    return -1;
  }
  if ( !equal_id(fe_c,get_next_facet(fe_d)) ) 
  { if ( verbose_flag )
    { sprintf(msg,"Unstarring failed, triple valence edge %s\n",
                ELNAME(get_fe_edge(fe_c)));
      outstring(msg);
    }
    return -1;
  }
  if ( !equal_id(fe_e,get_next_facet(fe_g))  )
  { if ( verbose_flag )
    { sprintf(msg,"Unstarring failed, triple valence edge %s\n",
             ELNAME(get_fe_edge(fe_e)));
      outstring(msg);
    }
    return -1;
  }
  if ( !equal_id(inverse_id(fe_a),get_next_facet(fe_i)) ) 
  { if ( verbose_flag )
    { sprintf(msg,"Unstarring failed, triple valence edge %s\n",
              ELNAME(get_fe_edge(fe_a)));
      outstring(msg);
    }
    return -1;
  }
  if ( !equal_id(inverse_id(fe_a),get_prev_facet(fe_i)) )
  { if ( verbose_flag )
    { sprintf(msg,"Unstarring failed, triple valence edge %s\n",
            ELNAME(get_fe_edge(fe_a)));
      outstring(msg);
    }
    return -1;
  }

  /* fix up body links */
  f_id = get_fe_facet(fe_e);
  b_id = get_facet_body(f_id);
  if ( valid_id(b_id) ) set_body_facet(b_id,f_id);
  b_id = get_facet_body(inverse_id(f_id));
  if ( valid_id(b_id) ) set_body_facet(b_id,inverse_id(f_id));

  /* fix up fe's around edges of new big facet */
  set_fe_facet(fe_f,inverse_id(fkeep));
  set_fe_facet(fe_h,fkeep);
  set_facet_fe(fkeep,fe_b);
  set_next_edge(fe_b,inverse_id(fe_f));
  set_next_edge(fe_f,inverse_id(fe_b));
  set_prev_edge(fe_f,inverse_id(fe_h));
  set_prev_edge(fe_h,inverse_id(fe_f));
  set_next_edge(fe_h,fe_b);
  set_prev_edge(fe_b,fe_h);

  /* discard */
  free_element(get_fe_facet(fe_e));
  free_element(get_fe_facet(fe_i));
  free_element(get_fe_edge(fe_a));
  free_element(get_fe_edge(fe_d));
  free_element(get_fe_edge(fe_e));
  free_element(fe_a);
  free_element(fe_c);
  free_element(fe_d);
  free_element(fe_e);
  free_element(fe_g);
  free_element(fe_i);
  free_element(v_id);

  top_timestamp = ++global_timestamp;
  return 1;
} /* end unstar() */

/************************************************************************
*
* function: simple_unstar()
*
* Purpose: Test and unstar facet next to an edge set for deletion.
*          May unstar multiple times if necessary.
*
* Return: -1 for failure, 1 for success
*/

int simple_unstar(facetedge_id base_fe)
{
      facetedge_id aa,bb;   
      facetedge_id a=0;  /* for edge to be merged */
      facetedge_id b=0;  /* for edge to be merged with */
      facetedge_id a_next=0,b_next=0;
                 /* facet chain links around edges a,b */
      edge_id  a_edge=0; /* edge of side a */
      edge_id  b_edge=0; /* edge of side b */
      facet_id  facet;  /* facet to be eliminated */ 
      int retval = 1;

      /* check we really have a facet */
      facet = get_fe_facet(base_fe);
      if ( !valid_id(facet) ) return 1;

      /* label relevant edges and */
      /* see if we have an adjacent starred triangle which will give
          trouble if we don't unstar it */
      for(;;)  /* may take multiple passes for total unstarring */
      { 
        int unstar_count = 0;

          a = get_next_edge(base_fe);
          a_edge = get_fe_edge(a);
          if ( get_vattr(get_edge_headv(a_edge)) & AXIAL_POINT ) return 1;
          a_next = get_next_facet(a);
          b = get_prev_edge(base_fe);
          b_edge = get_fe_edge(b);
          b_next = get_next_facet(b);
          if ( equal_element(a_edge,b_edge) ) return 1;
          aa = get_next_edge(a_next);
          bb = get_prev_edge(b_next);

          if ( !equal_id(get_next_facet(aa),inverse_id(bb)) )
            return 1; /* no star */
         
          retval  = unstar(aa); 
          if ( retval < 0 )
          { sprintf(errmsg,
            "Edge %s not deleted due to adjacent configuration involving facet %s.\n",
                                 ELNAME(get_fe_edge(base_fe)),ELNAME1(facet));
              kb_error(1903,errmsg,WARNING);
              return -1;
          }
            unstar_count++;
  
        } /* end for loop */
     
} /* end simple_unstar() */

/*********************************************************************
*
* function: star_finagle()
*
* Purpose: Clean up common-endpoint edges coming off an edge that
*          weren't taken care of by unstar() since the interior is
*          more complicated than a star.
*          Does this by refining common endpoint edges that aren't
*          on a facet adjacent to the eliminatable edge.
*
* return:  1 if 
*/
struct finagle { edge_id e_id; vertex_id v_id; };
int finagle_comp(a,b)
struct finagle *a,*b;
{ if ( a->v_id < b->v_id ) return -1;
  if ( a->v_id > b->v_id ) return  1;
  if ( a->e_id < b->e_id ) return -1;
  if ( a->e_id > b->e_id ) return  1;
  return 0;
}

int star_finagle(e_id)
edge_id e_id;
{ struct finagle tail_edges[100];  /* tails into e_id tail */
  int tail_valence = 0;
  struct finagle head_edges[100];  /* heads into e_id head */
  int head_valence = 0;
  int i,j;
  edge_id ee_id;
  int keeps;
  facetedge_id start_fe,fe_id;
  REAL side1[MAXCOORD],side2[MAXCOORD],side3[MAXCOORD];
  REAL aa,bb,cc,ab,ac,bc;

  /* get edge lists from the two vertices */
  ee_id = e_id;
  do
  { ee_id = get_next_tail_edge(ee_id); 
    if ( !equal_id(e_id,ee_id) && tail_valence < 99 )
    { tail_edges[tail_valence].e_id = ee_id;
      tail_edges[tail_valence++].v_id = get_edge_headv(ee_id);
    }
  } while ( !equal_id(e_id,ee_id) );

  ee_id = e_id;
  do
  { ee_id = get_next_head_edge(ee_id); 
    if ( !equal_id(e_id,ee_id) && head_valence < 99 )
    { head_edges[head_valence].e_id = ee_id;
      head_edges[head_valence++].v_id = get_edge_tailv(ee_id);
    }
  } while ( !equal_id(e_id,ee_id) );

  /* sort by other endpoint */
  qsort(tail_edges,tail_valence,sizeof(struct finagle),FCAST finagle_comp);
  qsort(head_edges,head_valence,sizeof(struct finagle),FCAST finagle_comp);

  /* keep only pairs of edges with same endpoints */
  for ( i = j = keeps = 0 ; i < tail_valence && j < head_valence ;  )
  { if ( tail_edges[i].v_id < head_edges[j].v_id )
      while ( tail_edges[i].v_id < head_edges[j].v_id ) i++;
    else
      while ( tail_edges[i].v_id > head_edges[j].v_id ) j++;
    if ( (i >= tail_valence) || (j >= head_valence) )
      break;
    if ( tail_edges[i].v_id == head_edges[j].v_id ) 
    { /* skip if nonzero wrap */
      if ( web.symmetry_flag )
      { WRAPTYPE w1 = get_edge_wrap(inverse_id(tail_edges[i].e_id));
        WRAPTYPE w2 = get_edge_wrap(e_id);
        WRAPTYPE w3 = get_edge_wrap(head_edges[j].e_id);
        WRAPTYPE netwrap = (sym_compose)(w1,w2);
        netwrap = (*sym_compose)(netwrap,w3);
        if ( netwrap != 0 )
        { i++; j++;  continue;
        }
      }
      tail_edges[keeps] = tail_edges[i]; i++;
      head_edges[keeps] = head_edges[j]; j++;
      keeps++;
    }
  }
  tail_valence = head_valence = keeps;

  /* remove edges that are facet-adjacent to elim edge */
  start_fe = fe_id = get_edge_fe(e_id);
  do
  { edge_id e_a = get_fe_edge(get_prev_edge(fe_id));
    edge_id e_b = get_fe_edge(get_next_edge(fe_id));
    for ( i = 0 ; i < tail_valence ; i++ )
    { if ( equal_element(e_a,tail_edges[i].e_id) )
      { tail_edges[i] = tail_edges[--tail_valence];
        break;
      }
    }
    for ( i = 0 ; i < head_valence ; i++ )
    { if ( equal_element(e_b,head_edges[i].e_id) )
      { head_edges[i] = head_edges[--head_valence];
        break;
      }
    }
    fe_id = get_next_facet(fe_id);
  } while ( !equal_id(fe_id,start_fe) );

  if ( head_valence != tail_valence )
  { sprintf(errmsg,"Internal error: star_finagle() edge %s: head_valence %d != tail_valence %d\n",
       ELNAME(e_id),head_valence,tail_valence);
    kb_error(3982,errmsg,RECOVERABLE);
  }
  /* Remove edges that have facets at large angle from */
  /*  triangle plane, indicating neck pinch */
  get_edge_side(e_id,side1);
  aa = dot(side1,side1,SDIM);
  for ( i = 0 ; i < head_valence ; i++ )
  { int flatflag = 0;
    /* calculate triangle normal */
    get_edge_side(tail_edges[i].e_id,side2);
    ab = dot(side1,side2,SDIM);
 
    start_fe = fe_id = get_edge_fe(tail_edges[i].e_id);
    for ( j = 0 ; j < 2 ; j++ ) /* check both head and tail edges */
    {
      do
      { REAL numer,denom;
        facetedge_id ffe = inverse_id(get_prev_edge(fe_id));
        edge_id ee_id = get_fe_edge(ffe);
        get_edge_side(ee_id,side3);
        ac = dot(side1,side3,SDIM);
        bb = dot(side2,side2,SDIM);
        bc = dot(side2,side3,SDIM);
        cc = dot(side3,side3,SDIM);
        numer = (j?-1:1)*(ac*bb - ab*bc);
        denom = sqrt(aa*bb-ab*ab)*sqrt(bb*cc-bc*bc);
        if ( numer > .8*denom ) 
          flatflag = 1;
        fe_id = get_next_facet(fe_id);
      } while ( !equal_id(fe_id,start_fe) );
      /* set up to try the head side */
      start_fe = fe_id = inverse_id(get_edge_fe(head_edges[i].e_id));
    }

    if ( !flatflag )
    { /* probably a neck, so let it be deleted */
      head_edges[i] = head_edges[--head_valence];
      tail_edges[i] = tail_edges[--tail_valence];
      i--;
    }
    
  }
  
  /* refine remaining edges */
  for ( i = 0 ; i < tail_valence ; i++ )
    edge_refine(tail_edges[i].e_id);
  for ( i = 0 ; i < head_valence ; i++ )
    edge_refine(head_edges[i].e_id);

  return tail_valence;

} /* end star_finagle */

/*********************************************************
*
*  edge_divide()
*
*  Purpose: Subdivide an edge in the first stage of 
*              refinement.  Marks the new vertex with
*              the NEWVERTEX attribute, and both new edges
*              with the NEWEDGE attribute, so they can be
*              properly skipped during refinement.
*              Also sets FIXED attribute bit of new vertex and
*              edge if the old edge is FIXED.
* 
*  Input:    edge_id e_id - the edge to subdivide
*
*  Output:  New legal configuration with new vertex.
*           New edge is head half of old edge.
*
*  Return: ID of new edge.
*/
 
edge_id edge_divide(e_id)
edge_id e_id;
{
  REAL s[MAXCOORD],*t,*mu=NULL,*mv,*h,m[MAXCOORD],q1[MAXCOORD],q3[MAXCOORD];
  edge_id  new_e;
  vertex_id divider=NULLID,old_mid=0,new_mid=0,headv,tailv;
  int i,j,k,n;
  facetedge_id new_fe,old_fe;
  int wrap = 0,wrap1=0,wrap2=0;
  REAL w[MAXCOORD];
  vertex_id *oldv=NULL,*newv;
  REAL *oldx[MAXLAGRANGE+1];
  REAL prod1[MAXLAGRANGE+1],prod2;
  facetedge_id first_fe;
  vertex_id allv[2*MAXLAGRANGE+1];
  

  if ( extra_bdry_attr && !extra_bdry_param_attr )
    kb_error(2841,
     "extra_boundary attribute defined but not extra_boundary_param.\n",
        RECOVERABLE); 

  if ( !extra_bdry_attr && extra_bdry_param_attr )
    kb_error(3367,
     "extra_boundary_param attribute defined but not extra_boundary.\n",
        RECOVERABLE);

  if ( !valid_element(e_id) ) 
     return NULLID;

  if ( web.representation == SIMPLEX )
     kb_error(1239,"Edge divide not implemented for simplex representation.\n",
        COMMAND_ERROR);

  if ( verbose_flag )
  { sprintf(msg,"Refining edge %s\n",ELNAME(e_id));
    outstring(msg);
  }

  headv = get_edge_headv(e_id);
  tailv = get_edge_tailv(e_id);
  t = get_coord(tailv);
  h = get_coord(headv);
  if ( web.symmetry_flag )
  { wrap = get_edge_wrap(e_id);
    /* always use tail as base, for predictability */
    if ( wrap ) /* see which endpoint closer to origin, to use as base */
    { /* use tail as base */
      (*sym_wrap)(get_coord(headv),w,wrap);
      h = w;
      wrap1 = 0; wrap2 = wrap;
    }
    else wrap1 = wrap2 = 0;
  }
  if ( web.modeltype == LINEAR )
  { 
    for ( k = 0 ; k < SDIM ; k++ ) m[k] = (t[k] + h[k])/2;
    divider = new_vertex(m,e_id);
    set_attr(divider,get_eattr(e_id) & (FIXED|BARE_NAKED));
  }
  else if ( web.modeltype == QUADRATIC )
  { 
    divider = get_edge_midv(e_id);  /* use old midpoint */
    unset_attr(divider,Q_MIDPOINT);
    set_vertex_edge(divider,NULLID); /* so set_vertex_edge works */
    /* get coordinates of new midpoint(s) */
    mu = get_coord(divider);
    if ( circular_arc_flag  && (SDIM == 2))
    { /* using facts that midpoint is on perpendicular bisector and
         inversions with tail center lie on a straight line */
      REAL x1 = mu[0] - t[0];
      REAL y1 = mu[1] - t[1];
      REAL x2 = h[0] - t[0];
      REAL y2 = h[1] - t[1];
      REAL det = x2*y1 - y2*x1;
      REAL xp = det > 0 ? -y1 : y1;
      REAL yp = det > 0 ?  x1 : -x1;
      REAL rr1 = x1*x1 + y1*y1;
      REAL rr2 = x2*x2 + y2*y2;
      REAL xx = x2/rr2 - x1/rr1;
      REAL yy = y2/rr2 - y1/rr1;
      REAL a = -x1*yy + y1*xx;  /* lambda^2 coeff */
      REAL b = xp*yy - yp*xx;  /* lambda coeff */
      REAL c = x1/4*yy - y1/4*xx;  /* const coeff */
      REAL lambda = (fabs(c) < fabs(b)/100000) ?
      ( (b > 0.0) ? (-2*c/(b + sqrt(b*b-4*a*c))) : (2*c/(-b + sqrt(b*b-4*a*c))) )
      : (( c > 0 ) ? (2*c/(-b + sqrt(b*b-4*a*c))) : 
                               (-2*c/(b + sqrt(b*b-4*a*c)))) ;
      q1[0] = t[0] + x1/2 + lambda*xp;
      q1[1] = t[1] + y1/2 + lambda*yp;

      /* turn it all around for other midpoint */
      x1 = mu[0] - h[0];
      y1 = mu[1] - h[1];
      x2 = t[0] - h[0];
      y2 = t[1] - h[1];
      det = x2*y1 - y2*x1;
      xp = det > 0 ? -y1 : y1;
      yp = det > 0 ?  x1 : -x1;
      rr1 = x1*x1 + y1*y1;
      rr2 = x2*x2 + y2*y2;
      xx = x2/rr2 - x1/rr1;
      yy = y2/rr2 - y1/rr1;
      a = -x1*yy + y1*xx;  /* lambda^2 coeff */
      b = xp*yy - yp*xx;  /* lambda coeff */
      c = x1/4*yy - y1/4*xx;  /* const coeff */
      lambda = (fabs(c) < fabs(b)/100000) ?
      ( (b > 0.0) ? (-2*c/(b + sqrt(b*b-4*a*c))) : (2*c/(-b + sqrt(b*b-4*a*c))) )
      : (( c > 0 ) ? (2*c/(-b + sqrt(b*b-4*a*c))) : 
                               (-2*c/(b + sqrt(b*b-4*a*c)))) ;
      q3[0] = h[0] + x1/2 + lambda*xp;
      q3[1] = h[1] + y1/2 + lambda*yp;
    }
    else
    { for ( k = 0 ; k < SDIM ; k++ ) 
      { q1[k] = 0.375*t[k] + 0.75*mu[k] - 0.125*h[k];
        m[k] = mu[k];
        q3[k] = 0.375*h[k] + 0.75*mu[k] - 0.125*t[k];
      }
    }
    if ( wrap1 )
    { (*sym_wrap)(q1,w,wrap1);
      for ( k = 0 ; k < SDIM ; k++ ) q1[k] = w[k];
    }
  }
  else if ( web.modeltype == LAGRANGE )
  { oldv = get_edge_vertices(e_id);

    /* get old vertex coordinates */
    for ( i = 0 ; i <= web.lagrange_order ; i++ )
    { if ( i < web.lagrange_order ) oldx[i] = get_coord(oldv[i]);
      else oldx[i] = h;  /* in case of unwrapping */
      for ( j = 0, prod1[i] = 1.0 ; j <= web.lagrange_order ; j++ )
        if ( i != j )  prod1[i] *= i-j;
    }

    /* figure out divider vertex */
    for ( k = 0 ; k < SDIM ; k++ ) m[k] = (t[k] + h[k])/2;
    if ( web.lagrange_order & 1 )  /* odd order */
    { divider = new_vertex(m,e_id);
      set_attr(divider,get_eattr(e_id) & (FIXED|BARE_NAKED));
    }
    else  /* even order */
    { divider = oldv[web.lagrange_order/2];
      unset_attr(divider,Q_MIDEDGE);
      set_vertex_edge(divider,NULLID); /* so set_vertex_edge works */
      oldv[web.lagrange_order/2] = new_vertex(NULL,e_id);
    }

  }

  /* make refine() work properly */
  set_attr(divider,NEWVERTEX);

  remove_vertex_edge(headv,inverse_id(e_id));
  new_e = dup_edge(e_id);
  insert_vertex_edge(headv,inverse_id(new_e));
  set_edge_tailv(new_e,divider);
  set_edge_headv(e_id,divider);
  set_attr(new_e,NEWEDGE | get_eattr(e_id));
  if ( web.symmetry_flag )
  { set_edge_wrap(e_id,wrap1);  /* new vertex in same unit cell as tail */
    set_edge_wrap(new_e,wrap2); 
  }
  if ( (web.modeltype == LINEAR) && web.metric_flag )
  { /* get midpoint in metric middle */
    REAL front,rear;  /* edge lengths */
    REAL tt[MAXCOORD],hh[MAXCOORD];
    REAL *xm = get_coord(divider);
    for(;;)  /* binary search for reasonable endpoint */ 
    {
      calc_edge(e_id);
      rear = get_edge_length(e_id);
      calc_edge(new_e);
      front = get_edge_length(new_e);
      if ( rear < 0.8*front )
      { /* bisect high end */
        t = tt;
        for ( i = 0 ; i < SDIM ; i++ )
        { t[i] = xm[i];
          xm[i] = (t[i] + h[i])/2;
        }
      }
      else if ( rear > 1.2*front )
      { /* bisect low end */
        h = hh;
        for ( i = 0 ; i < SDIM ; i++ )
        { h[i] = xm[i];
          xm[i] = (t[i] + h[i])/2;
        }
      }
      else break;
    } 
  }

     
  if ( web.modeltype == QUADRATIC )
  { /* new midpoint for old edge */
    old_mid = new_vertex(q1,e_id);
    set_edge_midv(e_id,old_mid);
    set_attr(old_mid,get_eattr(e_id) & (FIXED|BARE_NAKED));

    /* set up midpoint of new edge */
    new_mid = get_edge_midv(new_e);
    set_attr(new_mid,get_eattr(e_id) & (FIXED|BARE_NAKED));
    mv = get_coord(new_mid);
    for ( k = 0 ; k < SDIM ; k++ ) mv[k] = q3[k];

  }
  else if ( web.modeltype == LAGRANGE )
  { /* calculate new vertex coordinates */

    /* get vertices in right order */
    newv = get_edge_vertices(new_e);
    for ( n = 0 ; n < web.lagrange_order ; n++ )
    { allv[2*n] = oldv[n];
      allv[2*n+1] = newv[n];
    }
    allv[2*n] = newv[n]; /* last one */

    /* get divider back into order */
    allv[1] = allv[web.lagrange_order];
    allv[web.lagrange_order] = divider;
     
    for ( n = 1 ; n < web.lagrange_order ; n++ )
    {
      oldv[n] = allv[n];
      newv[n] = allv[web.lagrange_order + n];
      set_vertex_edge(oldv[n],e_id);
      set_vertex_edge(newv[n],new_e);
      set_attr(oldv[n],Q_MIDEDGE);
      set_attr(newv[n],Q_MIDEDGE);
    }

    /* set coordinates of new vertices */
    if ( bezier_flag )
    { 
      /* have to change all control points */
      MAT2D(oldc,MAXLAGRANGE+1,MAXCOORD);
      REAL *newx[2*MAXLAGRANGE+1];
      for ( i = 0 ; i <= web.lagrange_order ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
          oldc[i][j] = oldx[i][j]; 
      for ( i = 0 ; i <= 2*web.lagrange_order ; i++ )
        newx[i] = get_coord(allv[i]);
      mat_mult(bezier_refine_1d[web.lagrange_order],oldc,newx,
         2*web.lagrange_order+1,web.lagrange_order+1,SDIM); 
    }
    else
    for ( n = 0 ; n < web.lagrange_order ; n++ )
    { REAL coeff;
      REAL x;
      REAL *newx = get_coord(allv[2*n+1]);
      x = n + 0.5;
      for ( j = 0, prod2 = 1.0 ; j <= web.lagrange_order ; j++ )
         prod2 *= x - j;
      for ( k = 0 ; k < SDIM ; k++ ) newx[k] = 0.0;
      for ( i = 0 ; i <= web.lagrange_order ; i++ )
      { coeff = prod2/(x-i)/prod1[i];
        for ( k = 0 ; k < SDIM ; k++ )
           newx[k] += coeff*oldx[i][k];
      }
    }
  }

  /* for free boundary edges, cannot just interpolate parameters
     due to wrap-around of periodic parameters. So tangent extrapolate
     from one endpoint.
   */
  if ( get_eattr(e_id) & BOUNDARY )
  { struct boundary *bdry;
    REAL *parammid;
    REAL  defaultp[MAXCOORD];
    REAL *parama=NULL; /* tail */
    REAL *paramb=NULL; /* head */

    bdry = get_edge_boundary(e_id);
    set_edge_boundary_num(new_e,bdry->num);


    if ( get_boundary(tailv) == bdry )
      parama = get_param(tailv);
    else if ( extra_bdry_attr && extra_bdry_param_attr)
    { int bnum = *(int*)get_extra(tailv,extra_bdry_attr);
      if ( bnum == bdry->num )
         parama = (REAL*)get_extra(tailv,extra_bdry_param_attr);
    }
    
    if ( get_boundary(headv) == bdry )
        paramb = get_param(headv);
    else if ( extra_bdry_attr && extra_bdry_param_attr &&
           (*(int*)get_extra(headv,extra_bdry_attr) == bdry->num) )
        paramb = (REAL*)get_extra(headv,extra_bdry_param_attr);


    if ( (web.modeltype == LINEAR) || 
         ((web.modeltype==LAGRANGE) && (web.lagrange_order == 1)) )
    { set_attr(divider,BOUNDARY);
      set_boundary_num(divider,bdry->num);

      /* find parameters of new midpoint */
      mv = get_coord(divider);
      parammid = get_param(divider);
      if ( interp_bdry_param && parama && paramb )
      { for (  i = 0 ; i < bdry->pcount ; i++ )
          parammid[i] = (parama[i] + paramb[i])/2;
      }
      else
      if ( !parama && !paramb )
      { sprintf(errmsg,
          "Vertices %s and %s are on different boundaries from edge %s .\n",
                   ELNAME(headv),ELNAME1(tailv),ELNAME2(e_id));
        kb_error(1240,errmsg,WARNING);
        paramb = defaultp;
        defaultp[0] = defaultp[1] = defaultp[2] = 0.1;
        for ( i = 0 ; i < SDIM ; i++ )
          s[i] = eval(bdry->coordf[i],defaultp,NULLID,NULL);
        mu = s;
        /* projecting on tangent */
        b_extrapolate(bdry,mu,mv,mv,paramb,parammid,NULLID);
      }
      else
#ifdef PARAMAVG
         if ( (get_boundary(headv) == bdry) && (get_boundary(tailv) == bdry) )
              {
                 mu = get_coord(tailv);
                 parama = get_param(tailv);
                 paramb = get_param(headv);
                 /* projecting on tangent */
                 b_extrapolate(bdry,mu,mv,mv,parama,parammid,tailv);
                 /* if not wrapped, take average parameter */
                 for (  i = 0 ; i < bdry->pcount ; i++ )
                    { if ( ((parama[i] < parammid[i]) && (parammid[i] < paramb[i]))
                      || ((parama[i] > parammid[i]) && (parammid[i] > paramb[i])))
                      parammid[i] = (parama[i] + paramb[i])/2;
                    }

              }
             else
#endif
      if ( (get_boundary(headv) == bdry) && !wrap2 )
      { mu = get_coord(headv);
        paramb = get_param(headv);
        /* projecting on tangent */
        b_extrapolate(bdry,mu,mv,mv,paramb,parammid,headv);
      }
      else
      { mu = get_coord(tailv);
        paramb = get_param(tailv);
        /* projecting on tangent */
        b_extrapolate(bdry,mu,mv,mv,paramb,parammid,tailv);
      }
    }  /* end linear */
    else if ( web.modeltype == QUADRATIC )
    { REAL *paramc; 
      set_attr(old_mid,BOUNDARY);
      set_boundary_num(old_mid,bdry->num);
      set_attr(new_mid,BOUNDARY);
      set_boundary_num(new_mid,bdry->num);

      paramc = get_param(divider);

      if ( interp_bdry_param && parama )
      { 
        parammid = get_param(old_mid);             
        for ( i = 0 ; i < bdry->pcount ; i++ )
          parammid[i] = (parama[i] + paramc[i])/2;
      }
      else
      { mu = get_coord(divider);

        /* find new parameters of old edge midpoint */
        /* projecting on tangent */
        parammid = get_param(old_mid);             
        mv = get_coord(old_mid);
        b_extrapolate(bdry,mu,mv,mv,paramc,parammid,old_mid);
      }
      if ( interp_bdry_param && paramb )
      {
        parammid = get_param(new_mid);             
        for ( i = 0 ; i < bdry->pcount ; i++ )
          parammid[i] = (paramb[i] + paramc[i])/2;
      }
      else
      {
        /* find parameters of new edge midpoint */
        /* projecting on tangent */
        parammid = get_param(new_mid);             
        mv = get_coord(new_mid);
        b_extrapolate(bdry,mu,mv,mv,paramc,parammid,new_mid);
      }
    }
    else if ( web.modeltype == LAGRANGE )
    { set_attr(divider,BOUNDARY);
      set_boundary_num(divider,bdry->num);
      oldv = get_edge_vertices(e_id);
      newv = get_edge_vertices(new_e);

      /* calculate new parameter values */
      if ( interp_bdry_param && parama && paramb )
      { 
         for ( i = 1; i < 2*web.lagrange_order ; i += 2 ) /* tail half */
         { REAL *pa = (i==1) ? parama : get_param(allv[i-1]);
           REAL *pb = (i==2*web.lagrange_order-1)? get_param(allv[1])
                              : paramb;
           REAL *p  = get_param(allv[i]);
           for (  k = 0 ; k < bdry->pcount ; k++ )
               p[k] = (pa[k] + pb[k])/2;
         }
      }
      else /* interpolate */
      { oldv = get_edge_vertices(e_id);
        for ( i = 1; i < 2*web.lagrange_order ; i += 2 )
        { REAL *pa = get_param(allv[i-1]);
          REAL *p  = get_param(allv[i]);
          mu = get_coord(allv[i-1]);             
          mv = get_coord(allv[i]);
          b_extrapolate(bdry,mu,mv,mv,pa,p,allv[i]);
        }
      }

    }
  }
  else
  { ATTR attr = get_eattr(e_id) & (BDRY_ENERGY | BDRY_CONTENT | CONSTRAINT );
    conmap_t * conmap = get_e_constraint_map(e_id);

    set_attr(new_e,attr);
    set_attr(divider,attr);
    if ( conmap || attr )
    { set_e_conmap(new_e,conmap);
      set_v_conmap(divider,conmap);
      project_v_constr(divider,ACTUAL_MOVE,RESET_ONESIDEDNESS);
      if ( web.modeltype == QUADRATIC )
      { set_attr(old_mid,attr);
        set_attr(new_mid,attr);
        clear_v_conmap(old_mid);
        clear_v_conmap(new_mid);
        set_v_conmap(old_mid,conmap);
        set_v_conmap(new_mid,conmap);
        project_v_constr(old_mid,ACTUAL_MOVE,RESET_ONESIDEDNESS);
        project_v_constr(new_mid,ACTUAL_MOVE,RESET_ONESIDEDNESS);
      }
      /* this should have been taken care of for Lagrange in dup_edge */
    }
  }

  old_fe = first_fe = get_edge_fe(e_id);
  new_fe = NULLID;
  if ( valid_id(old_fe) ) do
  { /* create new facetedge and splice into edge net */
    facetedge_id next;
    new_fe = new_facetedge(get_fe_facet(old_fe),new_e);
    set_next_edge(new_fe,get_next_edge(old_fe));
    next = get_next_edge(old_fe);
    if ( valid_id(next) )
      set_prev_edge(get_next_edge(old_fe),new_fe);
    set_next_edge(old_fe,new_fe);
    set_prev_edge(new_fe,old_fe);
    if ( web.representation == STRING ) /* keep facet fe at start of arc */
    { facet_id ff_id = get_fe_facet(old_fe);
      if ( inverted(ff_id) && equal_id(old_fe,get_facet_fe(ff_id)) )
         set_facet_fe(ff_id,new_fe);
    }
    old_fe = get_next_facet(old_fe);
  } while ( valid_id(old_fe) && !equal_id(old_fe,first_fe) );
  set_edge_fe(new_e,new_fe);

  old_fe = first_fe = get_edge_fe(e_id);
  if ( valid_id(old_fe) )  do
    { /* copy over facet chain */
      facetedge_id fe,nfe;

      fe = get_next_edge(old_fe);
      nfe = get_next_facet(old_fe);
      set_next_facet(fe,get_next_edge(nfe));
      nfe = get_prev_facet(old_fe);
        set_prev_facet(fe,get_next_edge(nfe));
      old_fe = get_next_facet(old_fe);
    } while ( valid_id(old_fe) && !equal_id(old_fe,first_fe) );

  /* check for dangling ends with loopback fe */
  new_fe = first_fe = get_edge_fe(new_e);
  if ( valid_id(new_fe) )  do
  { if ( valid_id(get_next_edge(new_fe)) )
      if ( !equal_id(get_prev_edge(get_next_edge(new_fe)),new_fe ) )
         set_next_edge(new_fe,get_prev_edge(get_next_edge(new_fe)));
    new_fe = get_next_facet(new_fe);
  } while (  valid_id(new_fe) &&!equal_id(new_fe,first_fe) );

 #ifdef MPI_EVOLVER
 { unsigned short *nbr = mpi_export_eattr_ptr(e_id);
   for (i = 0 ; i < MPI_EXPORT_MAX ; i++ )
   { if ( nbr[i] == 0 ) break;
     if ( nbr[i] != this_task )
        mpi_refine_edge(e_id,divider,new_e,(int)nbr[i]);
    }
 }
 #endif


 top_timestamp = ++global_timestamp;

 return new_e;

} /* end edge_divide() */

/************************************************************
*
*    cross_cut()
*
*    Purpose: subdivides a facet into two facets by introducing
*                a new edge and a new facet.
*
*    Inputs:  facetedge_id first_fe  - first in chain defining new facet
*             facetidge_id last_fe   - last in chain;
*
*    Output:  new facet created with boundary first_fe,chain,last_fe,newedge
*                also both facets marked with NEWFACET attribute, so they
*                can be left alone during a refinement.
*
*/

void cross_cut(first_fe,last_fe)
facetedge_id first_fe,last_fe;
{
  facet_id old_f,new_f;
  edge_id  new_e;
  facetedge_id fe,new_fe_new,new_fe_old;
  int wrap;
  ATTR attr;
  vertex_id headv,tailv;
  int i,k;

  old_f = get_fe_facet(first_fe);
  attr = get_fattr(old_f);

  tailv = get_fe_headv(last_fe);
  headv = get_fe_tailv(first_fe);
  if ( get_vattr(headv) & AXIAL_POINT )
     new_e = inverse_id(new_edge(headv,tailv,old_f));
  else new_e = new_edge(tailv,headv,old_f);
  if ( attr & FIXED )
  { set_attr(new_e,FIXED);
     if ( web.modeltype == QUADRATIC ) 
        set_attr(get_edge_midv(new_e),FIXED);
     else if ( web.modeltype == LAGRANGE ) 
     { vertex_id *v = get_edge_vertices(new_e);
        for ( i = 1 ; i < web.lagrange_order ; i++ )
          set_attr(v[i],FIXED);
     }
  }
  if ( attr & NO_REFINE ) set_attr(new_e,NO_REFINE);

  /* for QUADRATIC model, midpoint of new edge is left as the 
      linear interpolation given by new_edge() since triangular
      facets may not yet be established yet */

  new_f = dup_facet(old_f); /* copy facet data */
  /* if ( inverted(old_f) ) invert(new_f);*/    /* same orientation */
  set_attr(old_f,NEWFACET);


  new_fe_new = new_facetedge(new_f,new_e);
  new_fe_old = new_facetedge(old_f,edge_inverse(new_e));
  set_edge_fe(new_e,new_fe_new);
  set_facet_body(new_f,get_facet_body(old_f));
  set_facet_body(facet_inverse(new_f),get_facet_body(facet_inverse(old_f)));
  if ( phase_flag ) set_f_phase_density(new_f);
  set_facet_fe(new_f,last_fe); /* preserves starting point of a facet */

  set_facet_fe(old_f,new_fe_old);

  /* install new facet into its facet-edges */
  /* and set torus wrap flags if needed */
  fe = first_fe;
  wrap = 0;
  for(;;)
  { set_fe_facet(fe,new_f);
    if ( web.symmetry_flag )
       wrap = (*sym_compose)(wrap,get_fe_wrap(fe));
    if ( equal_id(fe,last_fe) ) break;
    fe = get_next_edge(fe);
  }
  if ( wrap )
  { set_edge_wrap(new_e,(*sym_inverse)(wrap));
    if ( web.modeltype == QUADRATIC )
    { /* have to adjust coordinates of midpoint */
      REAL temp[MAXCOORD];
      REAL *mv,*tv,*hv;
      tv = get_coord(tailv);
      mv = get_coord(get_edge_midv(new_e));
      hv = get_coord(headv);
      (*sym_wrap)(hv,temp,(*sym_inverse)(wrap));
      for ( k = 0 ; k < SDIM ; k++ )
        mv[k] = (tv[k] + temp[k])/2;
    }
  }

  /* link up facet-edges */
  set_next_edge(get_prev_edge(first_fe),new_fe_old);
  set_prev_edge(new_fe_old,get_prev_edge(first_fe));
  set_prev_edge(get_next_edge(last_fe),new_fe_old);
  set_next_edge(new_fe_old,get_next_edge(last_fe));
  set_prev_edge(first_fe,new_fe_new);
  set_next_edge(new_fe_new,first_fe);
  set_next_edge(last_fe,new_fe_new);
  set_prev_edge(new_fe_new,last_fe);
  set_next_facet(new_fe_new,fe_inverse(new_fe_old));
  set_prev_facet(new_fe_new,fe_inverse(new_fe_old));
  set_next_facet(new_fe_old,fe_inverse(new_fe_new));
  set_prev_facet(new_fe_old,fe_inverse(new_fe_new));

  if ( attr & BOUNDARY )
  { struct boundary *bdry = get_facet_boundary(old_f);
    REAL *paramb,*parammid;
    REAL *mu,*mv;

    set_attr(new_e,BOUNDARY);
    set_edge_boundary_num(new_e,bdry->num);
    if ( web.modeltype == QUADRATIC )
    { vertex_id divider = get_edge_midv(new_e); 
      set_attr(divider,BOUNDARY);
      set_boundary_num(divider,bdry->num);

      /* find parameters of new midpoint */
      mv = get_coord(divider);
      parammid = get_param(divider);
      if ( (get_boundary(headv) != bdry) && (get_boundary(tailv) != bdry) )
      { sprintf(errmsg,
           "Vertices %s and %s are on different boundaries from edge %s.\n",
              ELNAME(headv),ELNAME1(tailv),ELNAME2(new_e));
        kb_error(1242,errmsg,RECOVERABLE);
      }
      else
#ifdef PARAMAVG
      if ( (get_boundary(headv) == bdry) && (get_boundary(tailv) == bdry) )
      { mu = get_coord(tailv);
        parama = get_param(tailv);
        paramb = get_param(headv);
        /* projecting on tangent */
        b_extrapolate(bdry,mu,mv,mv,parama,parammid,tailv);
        /* if not wrapped, take average parameter */
        for (  i = 0 ; i < bdry->pcount ; i++ )
        { if ( ((parama[i] < parammid[i]) && (parammid[i] < paramb[i]))
             || ((parama[i] > parammid[i]) && (parammid[i] > paramb[i])))
             parammid[i] = (parama[i] + paramb[i])/2;
        }
      }
      else
#endif
      if ( get_boundary(headv) == bdry )
      { mu = get_coord(headv);
        paramb = get_param(headv);
        /* projecting on tangent */
        b_extrapolate(bdry,mu,mv,mv,paramb,parammid,headv);
      }
      else
      { mu = get_coord(tailv);
        paramb = get_param(tailv);
        /* projecting on tangent */
        b_extrapolate(bdry,mu,mv,mv,paramb,parammid,tailv);
      }
    }
  }
  if ( attr & CONSTRAINT )    
  { ATTR cattr = attr & (BDRY_ENERGY | BDRY_CONTENT | CONSTRAINT );
    conmap_t * conmap = get_f_constraint_map(old_f);

    set_attr(new_e,cattr);
    set_e_conmap(new_e,conmap);
    if ( web.modeltype == QUADRATIC )
    { vertex_id mid = get_edge_midv(new_e);
      set_attr(mid,cattr);
      set_v_conmap(mid,conmap);
      project_v_constr(mid,ACTUAL_MOVE,RESET_ONESIDEDNESS);
    }
  }
 top_timestamp = ++global_timestamp;
} /* end cross_cut() */

/**************************************************************************
 * 
 *  function: merge_bodies()
 *  
 *  purpose: Merge bodies, after facet dissolve, perhaps
 *           Frees merged bodies.
 *
 *  return: Number of merged bodies.
 */

int merge_bodies()
{
  body_id *merge_body;  /* final destination of body */
  body_id b_id,bb_id;
  facetedge_id fe_id,next_fe;
  facet_id f_id,ff_id;
  int merge_count=0;

  merge_body = (body_id *)temp_calloc(web.skel[BODY].max_ord+5,sizeof(body_id));
  FOR_ALL_BODIES(b_id)
    merge_body[loc_ordinal(b_id)] = b_id;

  /* Find junctions between bodies */
  if ( web.representation == STRING )
  { /* test for distinct facets joining at vertex */
    FOR_ALL_FACETEDGES(fe_id)
    { next_fe = get_next_edge(fe_id);
      if ( equal_element(get_fe_edge(fe_id),get_fe_edge(next_fe)) )
        continue;
      f_id = get_fe_facet(fe_id);
      if ( !valid_id(f_id) ) continue;
      ff_id = get_fe_facet(next_fe);
      if ( !valid_id(ff_id) ) continue;
      if ( equal_id(f_id,ff_id)) continue;
      /* now ready to merge */
      while ( equal_id(ff_id,get_fe_facet(next_fe)) )
        set_fe_facet(next_fe,f_id);
      free_element(ff_id); 
    }
  }
  else if ( web.representation == SOAPFILM )
  { FOR_ALL_FACETEDGES(fe_id)
    { next_fe = get_next_facet(fe_id);
      if ( equal_id(fe_id,next_fe) ) continue; /* ok to disagree on valence 1 */
      f_id = get_fe_facet(fe_id);
      ff_id = get_fe_facet(next_fe);
      b_id = get_facet_body(inverse_id(f_id));
      bb_id = get_facet_body(ff_id);
      if ( !equal_id(b_id,bb_id) )
      { /* merge higher number body to lower */
        if ( loc_ordinal(b_id) > loc_ordinal(bb_id) )
          merge_body[loc_ordinal(b_id)] = bb_id;
        else
          merge_body[loc_ordinal(bb_id)] = b_id;
      }
    }
  }

  /* find net merge destinations */
  FOR_ALL_BODIES(b_id)
  { if ( b_id != merge_body[loc_ordinal(b_id)] )
    { bb_id = b_id;
      while ( bb_id != merge_body[loc_ordinal(bb_id)] )
        bb_id = merge_body[loc_ordinal(bb_id)];
      merge_body[loc_ordinal(b_id)] = bb_id;
      merge_count++;
    }
  }

  /* reset all facets appropriately */
  FOR_ALL_FACETS(f_id)
  { b_id = get_facet_body(f_id);
    bb_id = merge_body[loc_ordinal(b_id)];
    if ( !equal_id(b_id,bb_id) )
      set_facet_body(f_id,bb_id);
    b_id = get_facet_body(inverse_id(f_id));
    bb_id = merge_body[loc_ordinal(b_id)];
    if ( !equal_id(b_id,bb_id) )
      set_facet_body(inverse_id(f_id),bb_id);
  }

  /* get rid of unneeded bodies */
  FOR_ALL_BODIES(b_id)
  { if ( b_id != merge_body[loc_ordinal(b_id)] )
      free_element(b_id);
  }

  temp_free((char*)merge_body);

  return merge_count;
}

/********************************************************************
*
* function: ffcomp()
*
* purpose:  comparison for ordering facets in flist in rebody()
*
*/
 
int ffcomp(a,b)
facet_id *a,*b;
{
  if ( loc_ordinal(*a) < loc_ordinal(*b) ) return -1;
  if ( loc_ordinal(*a) > loc_ordinal(*b) ) return 1;
  if ( inverted(*a) < inverted(*b) ) return -1;
  if ( inverted(*a) > inverted(*b) ) return 1;

  return 0;
}

/************************************************************************
*
* function: rebody()
*
* purpose:  redivide bodies into connected pieces. Useful after
*                neck pinches.
*
* method:  Starting from body facet, labels all adjacent facets 
*             for body.  An unlabelled body facet is assigned new body
*             and propagated to neighbors.
*
* return:    Number of new bodies.
*/

int rebody()
{ body_id b_id,old_b;
  int faces_left;
  int new_bodies = 0;
  int i;
  struct fface { facet_id f_id; int wrapflag; } *flist, *bf;
  facet_id *stack;  /* of known faces */
  int stacktop;
  int facetop;  /* length of flist */
  facet_id f_id,ff_id;
  facetedge_id fe;

  if ( web.representation == SIMPLEX )
  { kb_error(3032,"\"rebody\" not implemented for the simplex model.\n",
       RECOVERABLE);
  }
  if ( web.representation == STRING ) return string_rebody();

  /* working list of facets */
  flist = (struct fface *)temp_calloc(2*web.skel[FACET].count,
                sizeof(struct fface));
  stack = (facet_id *)temp_calloc(2*web.skel[FACET].count,sizeof(facet_id*));
  faces_left = 0; 
  FOR_ALL_FACETS(f_id)
     { if ( valid_id(get_facet_body(f_id)))
             flist[faces_left++].f_id = f_id;
        ff_id = facet_inverse(f_id);
        if ( valid_id(get_facet_body(ff_id)) )
             flist[faces_left++].f_id = ff_id;
      }
  if ( faces_left == 0 ) return 0;
  facetop = faces_left;

  /* sort in facet order, so can find facets quickly */
  qsort((char *)flist,faces_left,sizeof(struct fface),FCAST ffcomp);

  /* initialize stack with body facets */
  stacktop = 0;
  FOR_ALL_BODIES(b_id)
    { 
      f_id = get_body_facet(b_id);
      if ( !valid_id(f_id) )
        continue;
      stack[stacktop++] = f_id;
      bf = (struct fface *)bsearch((char *)&f_id,(char *)flist,
                        facetop, sizeof(struct fface),FCAST ffcomp);
      if ( bf == NULL ) 
      { kb_error(1243,
             "Internal error: rebody() cannot find facet on stack.\n",WARNING);
        continue; 
      }

      bf->wrapflag = 1;
      faces_left--;
    }

  /* pull stuff off stack till empty */
  while (stacktop > 0 )
    { f_id = stack[--stacktop];
      b_id = get_facet_body(f_id);
      for ( i = 0, fe = get_facet_fe(f_id); i < 3 ; i++,fe = get_next_edge(fe))
         { 
            ff_id = get_fe_facet(get_prev_facet(fe));
            if ( !valid_id(ff_id) ) continue;
            ff_id = facet_inverse(ff_id);
            if ( !equal_id(get_facet_body(ff_id),b_id) ) continue;
            bf = (struct fface *)bsearch((char *)&ff_id,(char *)flist,
                        facetop, sizeof(struct fface),FCAST ffcomp);
            if ( bf == NULL ) 
              { kb_error(1244,"Internal error: rebody() cannot find facet on stack (old body).\n",WARNING);

                 continue;
              }
            if ( bf->wrapflag ) continue;
            bf->wrapflag = 1; /* mark as part of original body */
            stack[stacktop++] = ff_id;
            faces_left--;
         }
    }

  /* now have to create new bodies */
  while ( faces_left > 0 )
    { /* find undone face */
      for ( i  =  0 ; i < facetop ; i++ )
         if ( flist[i].wrapflag == 0 ) break;
      if ( i == facetop ) break;
      flist[i].wrapflag = 1;
      f_id = flist[i].f_id;
      old_b = get_facet_body(f_id);
      b_id = dup_body(b_id);
      new_bodies++;
      set_body_facet(b_id,NULLID);  /* clear dup'ed value */
      set_facet_body(f_id,b_id);  /* takes care of set_body_facet() for first one */
      stack[stacktop++] = f_id;
      faces_left--;
      /* pull stuff off stack till empty */
      while (stacktop > 0 )
        { f_id = stack[--stacktop];
          for ( i=0, fe = get_facet_fe(f_id); i < 3 ; i++,fe = get_next_edge(fe))
             { ff_id = facet_inverse(get_fe_facet(get_prev_facet(fe)));
                if (!valid_id(ff_id) || !valid_id(get_facet_body(ff_id))) continue;
                if ( !equal_id(get_facet_body(ff_id),old_b) ) continue;
                bf = (struct fface *)bsearch((char *)&ff_id,(char *)flist,
                        facetop, sizeof(struct fface),FCAST ffcomp);
                if ( bf == NULL )
                  { kb_error(1245,"Internal error: rebody() cannot find facet on stack (new body).\n",WARNING);
                     continue;
                  }
                if ( bf->wrapflag ) continue;
                bf->wrapflag = 1; /* mark as part of original body */
                set_facet_body(ff_id,b_id);
                stack[stacktop++] = ff_id;
                faces_left--;
             }
          }
     }

  temp_free((char*)flist);
  temp_free((char*)stack);
  top_timestamp = ++global_timestamp;

  FOR_ALL_BODIES(b_id)
  { if ( get_body_volconst(b_id) != 0.0 )
    { sprintf(msg,
       "Nonzero VOLCONST on body %s.  May need adjusting due to rebody.\n",
         ELNAME(b_id));
      kb_error(2171,msg,WARNING);
    }
  }
  return new_bodies;
}

/************************************************************************
*
* function: string_rebody()
*
* purpose:  redivide bodies into connected pieces. Useful after
*                neck pinches.  String model.
*
* method:  Starting from body facet edge, labels all adjacent edges 
*             for body.  An ruUnlabelled body edges is assigned new body
*             and propagated to neighbors.
*
* return:    Number of new bodies.
*/

int string_rebody()
{ body_id b_id,old_b;
  int edges_left;
  int new_bodies = 0;
  int i;
  struct bodyface *felist, *be;
  facetedge_id *stack;  /* of known faces */
  int stacktop;
  int edgetop;  /* length of flist */
  facet_id f_id,ff_id,new_f;
  edge_id e_id;
  facetedge_id fe,ffe;

  /* working list of edges */
  felist = (struct bodyface *)temp_calloc(2*web.skel[EDGE].count,
                sizeof(struct bodyface));
  stack = (facet_id *)temp_calloc(2*web.skel[EDGE].count,sizeof(edge_id*));
  edges_left = 0; 
  FOR_ALL_EDGES(e_id)
     { fe = get_edge_fe(e_id);
        if ( !valid_id(fe) ) continue;
        f_id = get_fe_facet(fe);
        if ( !valid_id(f_id) ) continue;

        if ( valid_id(get_facet_body(f_id)))
             felist[edges_left++].f_id = fe;
        ff_id = facet_inverse(f_id);
        if ( valid_id(get_facet_body(ff_id)) )
             felist[edges_left++].f_id = inverse_id(fe);

        fe = get_next_facet(fe);
        ff_id = get_fe_facet(fe);
        if ( equal_element(f_id,ff_id) ) continue;

        if ( valid_id(get_facet_body(ff_id)))
             felist[edges_left++].f_id = fe;
        ff_id = facet_inverse(ff_id);
        if ( valid_id(get_facet_body(ff_id)) )
             felist[edges_left++].f_id = inverse_id(fe);

      }
  if ( edges_left == 0 ) return 0;
  edgetop = edges_left;

  /* sort in edge order, so can find edges quickly */
  qsort((char *)felist,edges_left,sizeof(struct bodyface),FCAST ffcomp);

  /* initialize stack with body edges */
  stacktop = 0;
  FOR_ALL_BODIES(b_id)
    { fe = get_body_fe(b_id);
      stack[stacktop++] = fe;
      be = (struct bodyface *)bsearch((char *)&fe,(char *)felist,
                        edgetop, sizeof(struct bodyface),FCAST ffcomp);
      if ( be == NULL ) 
      { kb_error(1246,
              "Internal error: string_rebody() cannot find edge on stack.\n",
                 WARNING); 
        continue; 
      }

      be->wrapflag = 1;
      edges_left--;
    }

  /* pull stuff off stack till empty */
  while (stacktop > 0 )
    { fe = stack[--stacktop];
      f_id = get_fe_facet(fe);

      ffe = get_prev_edge(fe);
      if ( !valid_id(ffe) ) goto otherend;
      ff_id = get_fe_facet(ffe);
      if ( !equal_id(ff_id,f_id) ) goto otherend;
      be = (struct bodyface *)bsearch((char *)&ffe,(char *)felist,
                  edgetop, sizeof(struct bodyface),FCAST ffcomp);
      if ( be == NULL ) 
        { kb_error(1247,"Internal error: string_rebody() cannot find edge on stack (old body).\n",WARNING);

           goto otherend;
        }
      if ( !be->wrapflag )
      { be->wrapflag = 1; /* mark as part of original body */
        stack[stacktop++] = ffe;
        edges_left--;
      }
otherend:
      ffe = get_next_edge(fe);
      if ( !valid_id(ffe) ) continue;
      ff_id = get_fe_facet(ffe);
      if ( !equal_id(ff_id,f_id) ) continue;
      be = (struct bodyface *)bsearch((char *)&ffe,(char *)felist,
                  edgetop, sizeof(struct bodyface),FCAST ffcomp);
      if ( be == NULL ) 
        { kb_error(1248,"Internal error: string_rebody() cannot find edge on stack (old body).\n",WARNING);
          continue;
        }
      if ( !be->wrapflag )
      { be->wrapflag = 1; /* mark as part of original body */
        stack[stacktop++] = ffe;
        edges_left--;
      }
    }

  /* now have to create new bodies */
  while ( edges_left > 0 )
    { /* find undone edge */
      for ( i  =  0 ; i < edgetop ; i++ )
         if ( felist[i].wrapflag == 0 ) break;
      if ( i == edgetop ) break;
      felist[i].wrapflag = 1;
      fe = felist[i].f_id;
      f_id = get_fe_facet(fe);
      old_b = get_facet_body(f_id);
      b_id = dup_body(old_b);
      new_f = dup_facet(f_id);
      new_bodies++;
      set_body_facet(b_id,f_id);
      set_facet_body(new_f,b_id);
      set_facet_fe(new_f,fe);
      set_fe_facet(fe,new_f);
      stack[stacktop++] = fe;
      edges_left--;
      /* pull stuff off stack till empty */
      while (stacktop > 0 )
      { fe = stack[--stacktop];

        ffe = get_prev_edge(fe);
        if ( !valid_id(ffe) ) goto anotherend;
        ff_id = get_fe_facet(ffe);
        if ( !equal_id(ff_id,f_id) ) goto otherend;
        be = (struct bodyface *)bsearch((char *)&ffe,(char *)felist,
                    edgetop, sizeof(struct bodyface),FCAST ffcomp);
        if ( be == NULL ) 
          { kb_error(1249,"Internal error: string_rebody() cannot find edge on stack (old body).\n",WARNING);
            goto otherend;
          }
        if ( !be->wrapflag )
        { be->wrapflag = 1; /* mark as part of original body */
          stack[stacktop++] = ffe;
          edges_left--;
          set_fe_facet(ffe,new_f);
        }
anotherend:
        ffe = get_next_edge(fe);
        if ( !valid_id(ffe) ) continue;
        ff_id = get_fe_facet(ffe);
        if ( !equal_id(ff_id,f_id) ) continue;
        be = (struct bodyface *)bsearch((char *)&ffe,(char *)felist,
                    edgetop, sizeof(struct bodyface),FCAST ffcomp);
        if ( be == NULL ) 
          { kb_error(1250,"Internal error: string_rebody() cannot find edge on stack (old body).\n",WARNING);
            continue;
          }
        if ( !be->wrapflag )
        { be->wrapflag = 1; /* mark as part of original body */
          stack[stacktop++] = ffe;
          edges_left--;
          set_fe_facet(ffe,new_f);
        }
     }
  }

  temp_free((char*)felist);
  temp_free((char*)stack);
  top_timestamp = ++global_timestamp;
  return new_bodies;
}

/****************************************************************************
*
* function: dissolve_vertex()
*
* purpose:  Remove unattached vertex from surface.
*
* return: 1 if vertex removed, 0 if not because attached.
*/

int dissolve_vertex(v_id)
vertex_id v_id;
{ edge_id e_id;

  if ( !valid_element(v_id) ) return 0;
  if ( ( web.representation == SIMPLEX ) || ( web.modeltype == LAGRANGE ) )
  { if ( valid_id(get_vertex_facet(v_id)) ) 
      return 0;
  }
  e_id = get_vertex_edge(v_id);
  if ( valid_id(e_id) ) return 0;
  free_element(v_id);
  top_timestamp = ++global_timestamp;
  if ( verbose_flag )
  { sprintf(msg,"Dissolving vertex %s\n",ELNAME(v_id));
    outstring(msg);
  }

  return 1;
}

/****************************************************************************
*
* function: dissolve_edge()
*
* purpose:  Remove edge from surface if not on a facet.
*
* return: 1 if edge removed, 0 if not because attached.
*/

int dissolve_edge(e_id)
edge_id e_id;
{ facetedge_id fe,fe_id,start_fe;
  vertex_id head,tail;
  int evalence;
  facet_id f_id=NULLID;

  if ( !valid_element(e_id) ) return 0;

  evalence = get_edge_valence(e_id);

  if ( web.representation == STRING )
  {
#ifdef CODDLING
    /* check for at most one facet */
    if ( evalence > 1 )
    { if ( verbose_flag )
      { sprintf(msg,"Not dissolving edge %s since still on two facets.\n",
          ELNAME(e_id));
        outstring(msg);
      }
      return 0;
    }
#endif
  
    /* check for being at beginning or end of edge arc for all facets */
    if ( evalence > 0 )
    { start_fe = fe_id = get_edge_fe(e_id);
      do
      {   
        fe = fe_id;
        if ( valid_id(get_next_edge(fe)) && valid_id(get_prev_edge(fe)) )
        { /* check for full loop */
          do { fe = get_next_edge(fe); }
          while ( valid_id(fe) && !equal_id(fe,fe_id) );
          if ( !valid_id(fe) )
          { if ( verbose_flag )
            { sprintf(msg,
               "Not dissolving edge %s since would make facet into two arcs.\n",
                   ELNAME(e_id));
              outstring(msg);
            }
            return 0;
          }
        }
        fe_id = get_next_facet(fe_id);
      } while ( fe_id != start_fe );
      if ( evalence > 1 )
      { sprintf(msg,"Dissolving edge %s between two facets.\n",ELNAME(e_id));
        kb_error(2172,msg,WARNING);
      }
    }
  }
  else if ( evalence > 0 ) 
  { if ( verbose_flag )
    { sprintf(msg,"Not dissolving edge %s since still on a facet.\n",
         ELNAME(e_id));
      outstring(msg);
    }
    return 0;
  }

  if ( verbose_flag )
  { sprintf(msg,"Dissolving edge %s\n",ELNAME(e_id));
    outstring(msg);
  }

  fe = get_edge_fe(e_id);
  if ( !valid_id(fe) )
  { free_element(e_id); return 1; }

  head = get_edge_headv(e_id);
  tail = get_edge_tailv(e_id);

  /* reset vertex-edge links */
  remove_vertex_edge(tail,e_id);
  remove_vertex_edge(head,inverse_id(e_id));

  /* connect facetedges of stumps */
  if ( web.representation == STRING )
  { facetedge_id ffe = fe;
    do
    {
      f_id = get_fe_facet(ffe);  /* in case of STRING */
      if ( valid_id(f_id) )
      {
        if ( inverted(f_id) )
        { if ( valid_id(get_prev_edge(ffe)) )
           set_facet_fe(f_id,get_prev_edge(ffe));
          else if ( !valid_id(get_next_edge(ffe)) )
           set_facet_fe(f_id,NULLID); 
        } 
        else
        { if ( valid_id(get_next_edge(ffe)) )
           set_facet_fe(f_id,get_next_edge(ffe));
          else if ( !valid_id(get_prev_edge(ffe)) )
           set_facet_fe(f_id,NULLID); 
        }
      }
      ffe = get_next_facet(ffe);
    } while ( valid_id(ffe) && !equal_id(ffe,fe) );
  }

  for ( fe_id = fe ; ; )
  { facetedge_id  prev,next;
    prev = get_prev_edge(fe_id);
    if ( valid_id(prev) )
     { if ( valid_id(f_id) ) set_next_edge(prev,NULLID);
       else set_next_edge(prev,inverse_id(prev));
     }
    next = get_next_edge(fe_id);
    if ( valid_id(next) )
     { if ( valid_id(f_id) ) set_prev_edge(next,NULLID);
       else set_prev_edge(next,inverse_id(next));
     }

    free_element(fe_id);
    fe_id = get_next_facet(fe_id);
    if ( equal_id(fe_id,fe) ) break;
  }

  free_element(e_id);
  top_timestamp = ++global_timestamp;
  return 1;
}

/****************************************************************************
*
* function: dissolve_facet()
*
* purpose:  Remove facet from surface if not on a body.
*
* return: 1 if facet removed, 0 if not because attached.
*/

int dissolve_facet(f_id)
facet_id f_id;
{ facetedge_id fe,fe_id;
  edge_id e_id;
  body_id b1,b2;

  if ( !valid_element(f_id) ) return 0;

  b1 = get_facet_body(f_id);
  b2 = get_facet_body(inverse_id(f_id));

  if ( !equal_id(b1,b2) && valid_id(b1) && valid_id(b2) )
  { if ( verbose_flag )
    { sprintf(msg,"Not dissolving facet %s since on two different bodies\n",ELNAME(f_id));
      outstring(msg);
    }
    return 0;
  }

  if ( verbose_flag )
  { sprintf(msg,"Dissolving facet %s\n",ELNAME(f_id));
    outstring(msg);
  }

  if ( web.representation != SIMPLEX )
  { /* Identify the edges, but not the facets yet.
       delete_facet() will take care of the second 
       facet and patch up its facetedges. */
    fe = get_facet_fe(f_id);
    for ( fe_id = fe ; ; )
    { facetedge_id prev,next,next_fe;
      prev = get_prev_facet(fe_id);
      next = get_next_facet(fe_id);
      e_id = get_fe_edge(fe_id);
      next_fe = get_next_edge(fe_id);
      if ( !equal_id(prev,fe_id) )
      { set_next_facet(prev,next);
        set_prev_facet(next,prev);
        set_edge_fe(e_id,next);
        free_element(fe_id);
      }
      else
      {
        set_attr(e_id,BARE_NAKED);
        set_edge_fe(e_id,NULLID);
        free_element(fe_id);
      }
      fe_id = next_fe;
      if ( !valid_id(fe_id) ) break;  /* open facet in string model */
      if ( equal_id(fe_id,fe) ) break;
    }
  }

  set_facet_body(f_id,NULLID);
  set_facet_body(inverse_id(f_id),NULLID);

  if ( (web.modeltype == LAGRANGE) && (web.lagrange_order >= 3) )
  { vertex_id *v = get_facet_vertices(f_id);
    int i;
    for ( i = 0 ; i < web.skel[FACET].ctrlpts ; i++ )
      if ( get_vattr(v[i]) & Q_MIDFACET )
        free_element(v[i]);
  }
  free_element(f_id);
  top_timestamp = ++global_timestamp;
  return 1;
}

/****************************************************************************
*
* function: dissolve_body()
*
* purpose:  Remove body from surface.
*
* return: 1 if body removed.
*/

int dissolve_body(b_id)
body_id b_id;
{ facet_id f_id,ff_id;
  int k;
   
  if ( !valid_element(b_id) ) return 0;

  if ( verbose_flag )
  { sprintf(msg,"Dissolving body %s\n",ELNAME(b_id));
     outstring(msg);
  }

  FOR_ALL_FACETS(f_id)
  { if ( equal_id(b_id,get_facet_body(f_id)) ) 
       set_facet_body(f_id,NULLBODY);
    ff_id = inverse_id(f_id);
    if ( equal_id(b_id,get_facet_body(ff_id)) ) 
       set_facet_body(ff_id,NULLBODY);
  }
  if ( everything_quantities_flag )
  { struct method_instance *mi = METH_INSTANCE(get_body_volmeth(b_id));
    struct gen_quant *q = GEN_QUANT(mi->quant);
    mi->flags = Q_DELETED;
    q->flags = Q_DELETED;
    for ( k = LOW_INST  ; k < meth_inst_count  ; k++ )
    { struct method_instance *mm = METH_INSTANCE(k);
      if ( mm->quant == mi->quant )
        mi->flags = Q_DELETED;
    }
  }
  free_element(b_id);
  top_timestamp = ++global_timestamp;
  return 1;
}

/*****************************************************************************
*
* function: divide_quad()
*
* purpose: Divide quadrilateral into two triangles.
*
*/

void divide_quad(fe)
facetedge_id fe;
{ facetedge_id fea,feb,fec,new_fe,new_fe2;
  facet_id old_f,new_f;
  edge_id new_e;
  conmap_t *old_conmap;

  old_f = get_fe_facet(fe);
  old_conmap = get_f_constraint_map(old_f);
  if ( inverted(old_f) )
  { fe = inverse_id(fe);
    old_f = inverse_id(old_f);
  }
  fea = get_next_edge(fe);
  feb = get_next_edge(fea);
  fec = get_next_edge(feb);

  new_f = dup_facet(old_f);
  new_e = new_edge(get_fe_headv(fe),get_fe_tailv(fec),old_f);
  set_e_conmap(new_e,old_conmap);
  new_fe = new_facetedge(old_f,new_e);
  set_edge_fe(new_e,new_fe);
  set_facet_fe(old_f,new_fe);
  set_next_edge(new_fe,fec);
  set_prev_edge(fec,new_fe);
  set_next_edge(fe,new_fe);
  set_prev_edge(new_fe,fe);
  if ( web.symmetry_flag )
  { WRAPTYPE wrap = get_fe_wrap(fec);
    wrap = (*sym_compose)(wrap,get_fe_wrap(fe));
    wrap = (*sym_inverse)(wrap);
    set_edge_wrap(new_e,wrap);
  }
  new_fe2 = new_facetedge(new_f,inverse_id(new_e));
  set_facet_fe(new_f,new_fe2);
  set_next_edge(new_fe2,fea);
  set_prev_edge(fea,new_fe2);
  set_next_edge(feb,new_fe2);
  set_prev_edge(new_fe2,feb);
  set_next_facet(new_fe,inverse_id(new_fe2));
  set_next_facet(new_fe2,inverse_id(new_fe));
  set_prev_facet(new_fe,inverse_id(new_fe2));
  set_prev_facet(new_fe2,inverse_id(new_fe));
  set_fe_facet(fea,new_f);
  set_fe_facet(feb,new_f);
}

/*************************************************************************
*
* function: t1_edgeswap()
*
* purpose: Does 2D string model T1 topological transition.
*          For edge with valence 3 at each end, forcibly switches
*          orientation of the edge.  Only swaps if the edge and its
*          two endpoints are not fixed and not on constraints or boundaries.
*
* return value: Number of edges swapped.
*/

int t1_edgeswap(e_id)
edge_id e_id;
{
  facetedge_id fe_a,fe_b,fe_c,fe_d,fe_e,fe_f,fe_g,fe_h,fe_i,fe_j;
  vertex_id headv,tailv;
  facet_id f_a,f_b,f_c,f_d;
  REAL *hx,*tx,newhx[MAXCOORD],newtx[MAXCOORD];
  int i,j;

  if ( web.dimension != STRING )
  { kb_error(3665,"t1_edgeswap valid only for STRING model.\n",WARNING);
    return 0;
  }  
 
  /* check fixedness and boundary */
  headv = get_edge_headv(e_id);
  tailv = get_edge_tailv(e_id);
  if ( (get_eattr(e_id) & FIXED) 
    || (get_vattr(headv) & FIXED) 
    || (get_vattr(tailv) & FIXED) )
  { if ( verbose_flag )
    { sprintf(msg,
       "Edge %s not t1_swapped due to fixedness.\n",
          ELNAME(e_id));
      outstring(msg);
    }
    return 0;
  }
  /* check constraints */
  if ( compare_vertex_edge_attr(headv,e_id) != A_EQ_B  ||
       compare_vertex_edge_attr(tailv,e_id) != A_EQ_B ) 
  { if ( verbose_flag )
    { sprintf(msg,
       "Edge %s not t1_swapped due to differing constraints.\n",
          ELNAME(e_id));
      outstring(msg);
    }
    return 0;
  }
  /* check boundaries */
  if ( get_vertex_boundary_num(headv) != get_edge_boundary_num(e_id) ||
       get_vertex_boundary_num(tailv) != get_edge_boundary_num(e_id) ) 
  { if ( verbose_flag )
    { sprintf(msg,
       "Edge %s not t1_swapped due to differing boundaries.\n",
          ELNAME(e_id));
      outstring(msg);
    }
    return 0;
  }

  /* gather facet edges and check topology */
  fe_a = get_edge_fe(e_id);
  if ( !valid_id(fe_a) || !valid_id(get_fe_facet(fe_a)) ) 
  { if ( verbose_flag )
    { sprintf(msg,
       "Edge %s not t1_swapped since it does not have adjacent facets.\n",
          ELNAME(e_id));
      outstring(msg);
    }
    return 0;
  }
  fe_b = get_next_facet(fe_a);
  if ( equal_id(fe_a,fe_b) ) 
  { if ( verbose_flag )
    { sprintf(msg,
       "Edge %s not t1_swapped since it has only one adjacent facet.\n",
          ELNAME(e_id));
      outstring(msg);
    }
    return 0;
  }
  fe_c = get_prev_edge(fe_a);
  fe_d = get_next_edge(fe_a);
  fe_e = get_next_facet(fe_c);
  fe_f = inverse_id(get_next_edge(fe_e));
  fe_g = get_prev_edge(fe_b);
  if ( !equal_id(fe_g,get_next_facet(fe_f)) )
  { if ( verbose_flag )
    { sprintf(msg,
       "Edge %s not t1_swapped since tail vertex doesn't have valence 3.\n",
          ELNAME(e_id));
      outstring(msg);
printf("fe_f edge %08X  fe_g edge %08X\n",get_fe_edge(fe_g),get_fe_edge(fe_f));
    }
    return 0;
  }
  fe_j = get_next_facet(fe_d);
  fe_i = inverse_id(get_prev_edge(fe_j));
  fe_h = get_next_edge(fe_b);
  if ( !equal_id(fe_i,get_next_facet(fe_h)) )
  { if ( verbose_flag )
    { sprintf(msg,
       "Edge %s not t1_swapped since head vertex doesn't have valence 3.\n",
          ELNAME(e_id));
      outstring(msg);
    }
    return 0;
  }
  f_a = inverse_id(get_fe_facet(fe_a));
  f_b = get_fe_facet(fe_b);
  f_c = get_fe_facet(fe_e);
  f_d = get_fe_facet(fe_j);
  
  /* adjust wrapping so swapped edge is not wrapped */
  if ( web.torus_flag )
    torus_unwrap_edge(e_id);
  else if ( web.symmetry_flag )
  { edge_id pos_e = positive_id(e_id);  /* in case of quadratic */
    int wrap = get_edge_wrap(pos_e);
    if ( wrap ) 
      wrap_vertex(get_edge_headv(pos_e),wrap);
  }
    
  /* Now the actual swapping */

  /* kludge to get quantity body integrals attached */
  set_fe_facet(fe_a,inverse_id(f_c));
  set_fe_facet(fe_b,f_d);
  if ( equal_element(fe_a,get_facet_fe(f_a)) )
    set_facet_fe(f_a,inverse_id(fe_c));
  if ( equal_element(fe_b,get_facet_fe(f_b)) )
    set_facet_fe(f_b,fe_g);

  set_next_edge(fe_a,inverse_id(fe_e));
  set_prev_edge(inverse_id(fe_e),fe_a);
  set_prev_edge(fe_a,fe_f);
  set_next_edge(fe_f,fe_a);
  set_next_edge(fe_g,fe_h);
  set_prev_edge(fe_h,fe_g);
  set_prev_edge(fe_b,inverse_id(fe_i));
  set_prev_edge(fe_i,inverse_id(fe_b));
  set_next_edge(fe_b,fe_j);
  set_prev_edge(fe_j,fe_b);
  set_next_edge(fe_c,fe_d);
  set_prev_edge(fe_d,fe_c);
  remove_vertex_edge(tailv,get_fe_edge(inverse_id(fe_c)));
  remove_vertex_edge(headv,get_fe_edge(fe_h));
  set_edge_headv(get_fe_edge(fe_c),headv);
  set_edge_tailv(get_fe_edge(fe_h),tailv);

  /* move vertices (note that edge was unwrapped earlier */
  hx = get_coord(headv);
  tx = get_coord(tailv);
  if ( inverted(get_fe_facet(fe_a)) )
  { newtx[0] = (hx[0]+tx[0])/2 - (hx[1]-tx[1])/2;  
    newhx[0] = (hx[0]+tx[0])/2 + (hx[1]-tx[1])/2;  
    newtx[1] = (hx[1]+tx[1])/2 + (hx[0]-tx[0])/2;  
    newhx[1] = (hx[1]+tx[1])/2 - (hx[0]-tx[0])/2;  
  }
  else
  { newtx[0] = (hx[0]+tx[0])/2 + (hx[1]-tx[1])/2;  
    newhx[0] = (hx[0]+tx[0])/2 - (hx[1]-tx[1])/2;  
    newtx[1] = (hx[1]+tx[1])/2 - (hx[0]-tx[0])/2;  
    newhx[1] = (hx[1]+tx[1])/2 + (hx[0]-tx[0])/2;  
  }
  hx[0] = newhx[0];
  hx[1] = newhx[1];
  tx[0] = newtx[0];
  tx[1] = newtx[1];
  if ( web.modeltype == QUADRATIC )
  { REAL *midx = get_coord(get_edge_midv(e_id));
    for ( i = 0 ; i < SDIM ; i++ )
      midx[i] = (hx[i] + tx[i])/2;
  }
  if ( web.modeltype == LAGRANGE )
  { vertex_id *v = get_edge_vertices(e_id);
    for ( j = 1 ; j < web.lagrange_order ; j++ )
    { REAL *x = get_coord(v[j]);
      for ( i = 0 ; i < SDIM ; i++ )
        x[i] = (j*hx[i] + (web.lagrange_order-j)*tx[i])/web.lagrange_order;
    }
  }

  return 1;
}

/**************************************************************************
*
* function: merge_vertex()
*
* purpose:  merge two supposedly close vertices.  Not meant for vertices
*           connected with an edge.  Replacement for kludge of defining
*           edge between vertices and deleting edge.  Keeps first vertex.
*
*/

void merge_vertex(keepv,throwv)
edge_id keepv,throwv;
{
  edge_id e_id;
  int nn = 0;
  int i,wrap=0;
  facet_id match_f=NULLID;
  facetedge_id match_kfe=NULLID,match_tfe=NULLID;

  if ( get_vattr(keepv) & (Q_MIDPOINT|Q_MIDEDGE|Q_MIDFACET) )
  { sprintf(errmsg,"vertex_merge: Cannot merge %s since not at end of edge.\n",
       ELNAME(keepv));
    kb_error(4321,errmsg,RECOVERABLE);
  }
  if ( get_vattr(throwv) & (Q_MIDPOINT|Q_MIDEDGE|Q_MIDFACET) )
  { sprintf(errmsg,"vertex_merge: Cannot merge %s since not at end of edge.\n",
       ELNAME(throwv));
    kb_error(4322,errmsg,RECOVERABLE);
  }

  if ( (web.modeltype != LINEAR) && (web.modeltype != QUADRATIC) )
    kb_error(3913,"vertex_merge only in LINEAR or QUADRATIC model so far.\n",
        RECOVERABLE);
  
  if ( equal_id(keepv,throwv) )
  { kb_error(3882,"vertex_merge called with identical vertices.\n",WARNING);
    return;
  }
 
  if ( web.symmetry_flag && !web.torus_flag )
  { kb_error(3883,"vertex_merge can't handle wraps in non-torus symmetries.\n",
      RECOVERABLE);
  }
   

  /* no checks on constraints or fixedness; let the user beware! */

  /* maybe have wrapping in torus mode */
  if ( web.torus_flag )
  { REAL *x = get_coord(keepv);
    REAL *y = get_coord(throwv);
    REAL z[MAXCOORD];
    REAL u[MAXCOORD];
   
    for ( i = 0 ; i < SDIM ; i++ )
      z[i] = y[i] - x[i];
    matvec_mul(web.inverse_periods,z,u,SDIM,SDIM);
    for ( i = SDIM-1, wrap = 0 ; i >= 0 ; i-- )
    { wrap <<= TWRAPBITS;
      wrap |= (int)(floor(u[i]+0.5)) & WRAPMASK;      
    }
  }
  
  /* for later fixup in string model */
  if ( web.representation == STRING )
  { /* find common face four of the facetedges have in common */
    facetedge_id keep_fe;
    facetedge_id throw_fe_1,throw_fe_2;
    edge_id keep_e, throw_e, keep_e_start, throw_e_start;
    facet_id kf,tf;

    match_kfe = match_tfe = NULLID;
    match_f = NULLID;
    keep_e = keep_e_start = get_vertex_edge(keepv);
    do
    {
      throw_e = throw_e_start = get_vertex_edge(throwv);
      do
      {
        /* see if facetedges match up */
        keep_fe = get_edge_fe(keep_e);
        if ( valid_id(keep_fe) && valid_id(get_fe_facet(keep_fe)) )
        {
          throw_fe_1 = get_edge_fe(throw_e);
          if ( valid_id(throw_fe_1) && valid_id(get_fe_facet(throw_fe_1)) )
          {
            throw_fe_2 = get_next_facet(throw_fe_1);
            kf = get_fe_facet(keep_fe);
            tf = get_fe_facet(throw_fe_1);
            if ( equal_element(kf,tf) && !like_sign(kf,tf) )
            { match_f = kf;
              match_kfe = keep_fe;
              match_tfe = throw_fe_1;
              goto found_match;
            }
            tf = get_fe_facet(throw_fe_2);
            if ( equal_element(kf,tf) && !like_sign(kf,tf) )
            { match_f = kf;
              match_kfe = keep_fe;
              match_tfe = throw_fe_2;
              goto found_match;
            }
  
            keep_fe = get_next_facet(keep_fe);
            kf = get_fe_facet(keep_fe);
            tf = get_fe_facet(throw_fe_1);
            if ( equal_element(kf,tf) && !like_sign(kf,tf) )
            { match_f = kf;
              match_kfe = keep_fe;
              match_tfe = throw_fe_1;
              goto found_match;
            }
            tf = get_fe_facet(throw_fe_2);
            if ( equal_element(kf,tf) && !like_sign(kf,tf) )
            { match_f = kf;
              match_kfe = keep_fe;
              match_tfe = throw_fe_2;
              goto found_match;
            }
          }
        }
        throw_e = get_next_tail_edge(throw_e);
      } while ( !equal_id(throw_e,throw_e_start) );
      keep_e = get_next_tail_edge(keep_e);
    } while ( !equal_id(keep_e,keep_e_start) );
 found_match: ;


  }
   /* change all references to the eliminated vertex to the kept vertex. */
   for (;;)
   { e_id = get_vertex_edge(throwv);
     if ( !valid_id(e_id) ) break;
     remove_vertex_edge(throwv,e_id);
     set_edge_tailv(e_id,keepv);
     if ( web.torus_flag )
     { WRAPTYPE invwrap = (*sym_inverse)(wrap);
       set_edge_wrap(e_id,(*sym_compose)(invwrap,get_edge_wrap(e_id)));
       if ( !inverted(e_id) ) /* keep interior vertices with tail */
       { if ( web.modeltype == QUADRATIC )
         { vertex_id midv = get_edge_midv(e_id);
           REAL *midx = get_coord(midv);
           (*sym_wrap)(midx,midx,invwrap);
         }
         else if ( web.modeltype == LAGRANGE )
         { vertex_id *v = get_edge_vertices(e_id);
           for ( i = 1 ; i < web.lagrange_order ; i++ )
           { REAL *vx = get_coord(v[i]);
             (*sym_wrap)(vx,vx,invwrap);
           }
         }
       }
     }
     if ( ++nn > web.skel[EDGE].count )
     { sprintf(errmsg,"Internal error: Bad edge loop on vertex %s.\n",
          ELNAME(throwv));
       kb_error(3884,errmsg,WARNING);
       break;
     }
   }
 
   /* fix up facetedge links in string model */
   if ( (web.representation == STRING) && valid_id(match_f) )
   { /* find common face four of the facetedges have in common */
     facetedge_id prev_k = get_prev_edge(match_kfe);
     facetedge_id prev_t = get_prev_edge(match_tfe);
     facetedge_id next_fe,next_ffe;
     int found;
     int fe_counter; /* to prevent infinite loop in case of error */
     int fe_counter_max = 2*web.skel[FACETEDGE].count;

     set_next_edge(prev_k,inverse_id(prev_t));
     set_next_edge(prev_t,inverse_id(prev_k));
     set_prev_edge(match_kfe,inverse_id(match_tfe));
     set_prev_edge(match_tfe,inverse_id(match_kfe));
     /* see if we need to make a new facet */
     next_fe = match_kfe;
     found = 0;
     fe_counter = 0;
     do
     { if ( equal_element(next_fe,prev_k) )
       { found = 1;  break; }
       next_ffe = get_next_edge(next_fe);
       if ( equal_element(next_fe,next_ffe) ) 
         break;
       next_fe = next_ffe;
       if ( fe_counter++ > fe_counter_max )
               kb_error(3616,"Internal error - unclosed facetedge loop\n",
                               RECOVERABLE);
     } while ( !equal_element(next_fe,match_kfe) );
     next_fe = match_tfe; /* try the other way */
     fe_counter = 0;
     if ( !found )
     do
     { if ( equal_element(next_fe,prev_k) )
       { found = 1;  break; }
       next_ffe = get_next_edge(next_fe);
       if ( equal_element(next_fe,next_ffe) ) 
         break;
       next_fe = next_ffe;
       if ( fe_counter++ > fe_counter_max )
               kb_error(3613,"Internal error - unclosed facetedge loop\n",
                               RECOVERABLE);
     } while ( !equal_element(next_fe,match_kfe) );
     if ( !found )
     { /* have to create new facet */
       facet_id newf = dup_facet(match_f);
       set_facet_fe(newf,prev_k);
       set_facet_fe(get_fe_facet(match_tfe),match_tfe); /*make sure match_f has valid fe */
       next_fe = prev_k;
       fe_counter = 0;
       do
       { set_fe_facet(next_fe,newf);
         next_ffe = get_prev_edge(next_fe);
         if ( equal_element(next_ffe,next_fe) ) 
           break;
         next_fe = next_ffe;
         if ( fe_counter++ > fe_counter_max )
               kb_error(2873,"Internal error - unclosed facetedge loop\n",
                               RECOVERABLE);
       } while ( !equal_element(next_fe,prev_k));
       if ( !equal_element(next_fe,prev_k) )
       next_fe = inverse_id(prev_t);
       fe_counter = 0;
       do
       { set_fe_facet(next_fe,newf);
         next_ffe = get_next_edge(next_fe);
         if ( equal_element(next_ffe,next_fe) ) 
           break;
         next_fe = next_ffe;
         if ( fe_counter++ > fe_counter_max )
               kb_error(3621,"Internal error - unclosed facetedge loop\n",
                               RECOVERABLE);
       } while ( !equal_element(next_fe,prev_k));
     }
   }

 free_element(throwv);
  
}



/**************************************************************************
*
* function: merge_edge()
*
* purpose:  merge two supposedly close and parallel edges.
*           merges tail to tail and head to head.
*           In string model, first edge is on left, second on right.
*           Endpoints don't have to be distinct; i.e. can merge edges
*           with same tail.
*
*           Doesn't eliminate any facetedges, so continuity of chain
*           of edges of middle facet is maintained.  Edge winds up
*           with maybe 4 facet edges, which might confuse some things.
*
*/

void merge_edge(e_id1,e_id2)
edge_id e_id1,e_id2;
{ facetedge_id fe_start,fe,ffe,fep,ffep;
  vertex_id t1,t2,h1,h2;
  REAL side1[MAXCOORD],side2[MAXCOORD];

  if ( web.modeltype != LINEAR )
    kb_error(2894,"merge_edge only in LINEAR model so far.\n",RECOVERABLE);
  
  if ( equal_id(e_id1,e_id2) )
  { kb_error(3887,"merge_edge called with identical edges.\n",WARNING);
    return;
  }
 
  if ( web.symmetry_flag && !web.torus_flag )
  { kb_error(3888,"vertex_merge can't handle wraps in non-torus symmetries.\n",
      RECOVERABLE);
  }

  /* orient as parallel as possible */
  if ( equal_id(get_edge_tailv(e_id1),get_edge_tailv(e_id2))
    || equal_id(get_edge_headv(e_id1),get_edge_headv(e_id2)) )
  { /* ok */
  }
  else 
  if ( equal_id(get_edge_tailv(e_id1),get_edge_headv(e_id2))
    || equal_id(get_edge_headv(e_id1),get_edge_tailv(e_id2)) )
  { invert(e_id2);
  }
  else 
  { /* try geometric alignment */
    get_edge_side(e_id1,side1);
    get_edge_side(e_id2,side2);
    if ( dot(side1,side2,SDIM) < 0 )
      invert(e_id2);
  }

   
  t1 = get_edge_tailv(e_id1);
  t2 = get_edge_tailv(e_id2);
  if ( !equal_id(t1,t2) )
    merge_vertex(t1,t2);
  h1 = get_edge_headv(e_id1);
  h2 = get_edge_headv(e_id2);
  if ( !equal_id(h1,h2) )
    merge_vertex(h1,h2);

  /* now transfer facetedges */
  fe = fe_start = get_edge_fe(e_id2);
  do 
  { set_fe_edge(fe,e_id1);
    fe = get_next_facet(fe);
  } while ( !equal_id(fe,fe_start) );
  ffe = get_edge_fe(e_id1);
  fep = get_next_facet(fe);
  ffep = get_prev_facet(ffe);
  set_next_facet(fe,ffe);
  set_prev_facet(ffe,fe);
  set_next_facet(ffep,fep);
  set_prev_facet(fep,ffep);
  fe_reorder(e_id1);  /* for proper geometrical order */

  free_element(e_id2);  /* also removes from vertex lists */
}



/**************************************************************************
*
* function: merge_facet()
*
* purpose:  merge two supposedly close and parallel facets.
*           Tries to use body info to control orientation of merger.
*           picks best correspondence spatially of vertices to merge.
*
*/

void merge_facet(f_id1,f_id2)
edge_id f_id1,f_id2;
{ MAT2D(f1x,FACET_VERTS,MAXCOORD);
  MAT2D(f2x,FACET_VERTS,MAXCOORD);
  MAT2D(tempx,FACET_VERTS,MAXCOORD);
  int wrap,i;
  facetedge_id fe1,fe2;
  body_id b_id;
  REAL bestdist;
  int besti,edge_match;
  int j,k;
  body_id b1_front,b1_back,b2_front,b2_back;
  int  body_restricted = 0;  /* 1 if only one way to match bodies */
  
  if ( web.modeltype != LINEAR )
    kb_error(3881,"facet_merge only in LINEAR model so far.\n",RECOVERABLE);
  
  if ( equal_element(f_id1,f_id2) )
  { kb_error(3889,"facet_merge called with identical facets.\n",WARNING);
    return;
  }
 
  if ( web.symmetry_flag && !web.torus_flag )
  { kb_error(3915,"facet_merge can't handle wraps in non-torus symmetries.\n",
      RECOVERABLE);
  }

  /* figure best correspondence of vertices */
  /* First, see if any identical vertices */
  besti = -1;
  edge_match = 0;
  fe1 = get_facet_fe(f_id1);
  for ( i = 0 ; (i < FACET_VERTS) && !edge_match ; i++ )
  { fe2 = get_facet_fe(f_id2); 
    for ( j = 0 ; j < FACET_VERTS ; j++ )
    { if ( equal_id(get_fe_tailv(fe1),get_fe_tailv(fe2)) )
      { int newbesti =  j >= i ? j - i : j - i + 3;
        if ( besti >= 0 )
        { edge_match = 1;
          if ( besti != newbesti )
          /* have to reverse relative facet orientation */
          { invert(f_id2);
            j = (4 - j) % 3;
            besti =  j >= i ? j - i : j - i + 3;
          }
        }
        else 
           besti = newbesti;
        break;
      }
      fe2 = get_next_edge(fe2);
    }
    fe1 = get_next_edge(fe1);
  }

  /* see if body information is enough to specify direction */
  b1_front = get_facet_body(f_id1);
  b1_back = get_facet_body(inverse_id(f_id1));
  b2_front = get_facet_body(f_id2);
  b2_back = get_facet_body(inverse_id(f_id2));
  if ( !equal_id(b1_front,b2_back) )
  { if ( equal_id(b1_front,b2_front) && !edge_match ) 
    {  invert(f_id2);
       if ( !equal_id(b1_back,b2_back) )
         body_restricted = 1;
    }  
    else if ( equal_id(b1_back,b2_back) && !edge_match )
    {  invert(f_id1);
       if ( !equal_id(b1_front,b2_front) ) 
         body_restricted = 1;
    }
    else if ( equal_id(b1_back,b2_front) ) 
    {  invert(f_id1); invert(f_id2); 
       if ( besti >= 0 )
         besti = (3 - besti) % 3;
       body_restricted = 1;
    }
    else
    { sprintf(errmsg,"Cannot get body agreement for merging facets %s and %s\n",
           ELNAME(f_id1),ELNAME1(f_id2));
      kb_error(3896,errmsg,WARNING);
      return; 
    }
  }
  else if ( !equal_id(b1_back,b2_front) )
    body_restricted = 1;
    
  /* need vertex coordinates for orientation matching and vertex matching */
  get_facet_verts(f_id1,f1x,NULL);  /* follows facet orientation */
  get_facet_verts(f_id2,f2x,NULL);
  if ( web.torus_flag )
    { REAL z[MAXCOORD];
      REAL u[MAXCOORD];
  
      for ( i = 0 ; i < SDIM ; i++ )
        z[i] = f2x[0][i] - f1x[0][i];
      matvec_mul(web.inverse_periods,z,u,SDIM,SDIM);
      for ( i = SDIM-1, wrap = 0 ; i >= 0 ; i-- )
      { wrap |= (int)(floor(u[i]+0.5)) & WRAPMASK; 
        wrap <<= TWRAPBITS;
      } 
      if ( wrap )
      { for ( i = 0 ; i < FACET_VERTS ; i++ )
          torus_wrap(f1x[i],tempx[i],wrap); 
        f1x = tempx;
      }
    }

  if ( besti < 0 )
  { /* no identical vertices, so try minimizing distance */
    /* calculate sum of distances for various matchings */
    for ( i = 0, bestdist = 1e30, besti = -1 ; i < FACET_VERTS ; i++ )
    { REAL dist = 0.0;
      for ( j = 0 ; j < FACET_VERTS ; j++ )
      { REAL s;
        int cor = (i+j)%3;
        for ( k = 0, s = 0.0 ; k < SDIM ; k++ )
        { REAL d = (f1x[j][k] - f2x[cor][k]);
          s += d*d;
        }
        dist += sqrt(s);
      }
      if ( dist < bestdist ) 
      { bestdist = dist;
        besti = i;
      }
    }
  }     

  /* Merge vertices and edges.  Doing own edge merge since
   * can't pass enough info to merge_edge()
   */
  fe1 = get_facet_fe(f_id1);
  fe2 = get_facet_fe(f_id2);
  for ( i = 0 ; i < besti ; i++ )
      fe2 = get_next_edge(fe2);
  for ( i = 0 ; i < FACET_EDGES ; i++ )
  { edge_id e_id1 = get_fe_edge(fe1);
    edge_id e_id2 = get_fe_edge(fe2);
    vertex_id tail1 = get_edge_tailv(e_id1);
    vertex_id tail2 = get_edge_tailv(e_id2);
    facetedge_id fe_next;
    facetedge_id next_fe2,prev_fe1;
    if ( !equal_element(tail1,tail2) )
       merge_vertex(tail1,tail2);
    if ( !equal_id(e_id1,e_id2) )
    { fe_next = fe2;
      do
      { set_fe_edge(fe_next,e_id1);
        fe_next = get_next_facet(fe_next);
      } while ( !equal_id(fe2,fe_next) );
      next_fe2 = get_next_facet(fe2);
      prev_fe1 = get_prev_facet(fe1);
      set_next_facet(fe2,fe1);
      set_prev_facet(fe1,fe2);
      set_next_facet(prev_fe1,next_fe2);
      set_prev_facet(next_fe2,prev_fe1);
      free_element(e_id2);   
    }
    fe1 = get_next_edge(fe1);
    fe2 = get_next_edge(fe2) ;
  }

  b_id = get_facet_body(f_id2);
  set_facet_body(f_id1,b_id);
 
  set_facet_body(inverse_id(f_id2),b_id); /* so dissolve_facet() works */
  dissolve_facet(f_id2);

}  /* end merge_facet90 */


/****************************************************************************
*
* function: reverse_orientation_edge()
*
* purpose: reverse the orientation of an edge
*/

void reverse_orientation_edge(e_id)
edge_id e_id;
{ vertex_id v_id[2],*v;
  facetedge_id start_fe;
  int i;
  struct edge *e_ptr;

  v_id[0] = get_edge_tailv(e_id);
  v_id[1] = get_edge_headv(e_id);
  /* swap endpoints */
  switch ( web.modeltype )
  { case LINEAR:
       remove_vertex_edge(v_id[0],e_id);
       remove_vertex_edge(v_id[1],inverse_id(e_id));
       set_edge_headv(e_id,v_id[0]);
       set_edge_tailv(e_id,v_id[1]);
       break;
    case QUADRATIC:
       remove_vertex_edge(v_id[0],e_id);
       remove_vertex_edge(v_id[1],inverse_id(e_id));
       set_edge_headv(e_id,v_id[0]);
       set_edge_tailv(e_id,v_id[1]);
       break;
    case LAGRANGE:
       remove_vertex_edge(v_id[0],e_id);
       remove_vertex_edge(v_id[1],inverse_id(e_id));
       set_edge_headv(e_id,v_id[0]);
       set_edge_tailv(e_id,v_id[1]);
       v = get_edge_vertices(e_id);
       for ( i = 1 ; i <= web.lagrange_order/2 ; i++ )
       { vertex_id vv = v[i];
         v[i] = v[web.lagrange_order - i];
         v[web.lagrange_order - i] = vv;
       }
       break;
  }
  set_vertex_edge(v_id[1],e_id);
  set_vertex_edge(v_id[0],inverse_id(e_id));

  /* reverse id in facet-edges */
  start_fe = get_edge_fe(e_id);
  if ( valid_id(start_fe) )
  { facetedge_id fe = start_fe;
    do 
    { facet_id feprev,fenext,fe_fore,fe_aft;
      facet_id f_id = get_fe_facet(fe);
      set_fe_facet(fe,inverse_id(f_id));
      set_facet_fe(f_id,inverse_id(fe));
      fenext = get_next_facet(fe);
      feprev = get_prev_facet(fe);
      set_next_facet(fe,feprev);
      set_prev_facet(fe,fenext);
      fe_fore = get_next_edge(fe);
      fe_aft = get_prev_edge(fe);
      set_next_edge(fe,inverse_id(fe_aft));
      set_prev_edge(fe,inverse_id(fe_fore));
      set_prev_edge(fe_fore,inverse_id(fe));
      set_next_edge(fe_aft,inverse_id(fe));
      fe = fenext;
    } while ( !equal_id(fe,start_fe) );
  }

  /* reverse symmetry wraps */
  if ( web.symmetry_flag )
    set_edge_wrap(e_id,(*sym_inverse)(get_edge_wrap(e_id)));
    
   
  /* reverse "orientation" for methods and constraint integrals */
  e_ptr = eptr(e_id);
  e_ptr->attr ^= NEGBOUNDARY;
  
} /* end edge_reverse_orientation() */

/***************************************************************************
*
* Function: reverse_orientation_facet()
*
* Purpose: reverse the orientation of one facet.
*/

void reverse_orientation_facet(f_id)
facet_id f_id;
{ facetedge_id start_fe,fe,last_fe;
  body_id b_id,bb_id;
  struct facet *e_ptr;
  
   /* reverse body attachments (before reversing edges!!) */
  b_id = get_facet_body(f_id);
  bb_id = get_facet_body(inverse_id(f_id));
  set_facet_body(f_id,bb_id);
  set_facet_body(inverse_id(f_id),b_id);

  /* reverse fe */
  fe = start_fe = get_facet_fe(f_id);
  do
  { set_fe_facet(fe,inverse_id(f_id));
    last_fe = fe;
    fe = get_next_edge(fe);
  } while (valid_id(fe) && !equal_id(fe,start_fe));
  if (!valid_id(fe)) /* in case of open string facet */
    set_facet_fe(f_id,inverse_id(last_fe));
  else
    set_facet_fe(f_id,inverse_id(start_fe));
    
  /* invert Lagrange vertices */
  if ( (web.representation == SOAPFILM) && (web.modeltype == LAGRANGE) )
  { vertex_id *v = get_facet_vertices(f_id);
    int row,spot = web.lagrange_order+1;
    for ( row = 1 ; row < web.lagrange_order ; row++ )
    { int rowsize = web.lagrange_order+1-row;
      int i,mid = rowsize/2;
      for ( i = 1 ; i < mid ; i++ )
      { vertex_id vv = v[spot+i];
        v[spot+i] = v[spot+rowsize-i-1];
        v[spot+rowsize-i] = vv;
      }
      spot += rowsize;
    }
  }
  
  /* reverse method orientations */
  e_ptr = fptr(f_id);
  e_ptr->attr ^= NEGBOUNDARY;
}

