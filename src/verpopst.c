/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/******************************************************************
*
*  File:    verpopst.c
*
*  Purpose: In string model, pop vertices with 4 or more edges into
*              a configuration of vertices with only 3 edges each.
*              Works in 3-space.  No assumptions about edges bounding
*              facets or not, but does treat facets properly.
*/

#include "include.h"

/*********************************************************************
*
*  Function: verpop_str()
*
*  Purpose:  Assembles list of edges for each vertex, and calls
*            single-vertex routine for vertices with more than 3
*            edges.  List is first made as list of edge-vertex
*            pairs, then sorted on vertex.  Also deletes
*            bare, nonfixed vertices.
*
*  Return:  Number of vertices popped.
*/

int verpop_str()
{ vertex_id v_id;
  int popcount = 0;

  FOR_ALL_VERTICES(v_id)
  { int valence = get_vertex_evalence(v_id);
    if ( valence == 0 ) popcount += dissolve_vertex(v_id);
    else  if ( valence > 3 ) 
       popcount += poponest(v_id,valence);
    else if ( (valence >= 2) && (get_vattr(v_id) & (CONSTRAINT|BOUNDARY)) )
       popcount += poponest(v_id,valence);
  } 
  
  if ( popcount )
  { calc_content(Q_FIXED);
    calc_pressure();
    calc_energy();
  }

  return popcount;
}

/**************************************************************************
*
* function: pop_string_vertex()
*
* purpose: try popping given vertex; implements pop vertex in string model.
*/
int pop_string_vertex(v_id)
vertex_id v_id;
{ int valence = get_vertex_evalence(v_id);
  int popcount = 0;
  if ( valence == 0 ) popcount = dissolve_vertex(v_id);
  else  if ( valence > 3 ) 
     popcount = poponest(v_id,valence);
  else if ( (valence >= 2) && (get_vattr(v_id) & (CONSTRAINT|BOUNDARY)) )
     popcount = poponest(v_id,valence); 
  return popcount;
} 
  
/*****************************************************************************
*
*  function: poponest()
*
*  purpose: pop vertex with more that 3 edges
*
*/

/* comparison routine for qsort */
static int vecomp(a,b)
struct veredge *a,*b;
{
  if ( a->v_id < b->v_id ) return -1;
  if ( a->v_id > b->v_id ) return 1;
  return 0;
}

int poponest(v_id,edges)
vertex_id v_id; 
int edges;  /* number of edges at this vertex */
{ edge_id e_id;
  int i,j;
  vertex_id new_v;
  edge_id new_e;
  facetedge_id fe_id;
  REAL cosine,maxcos;
  REAL *x;
  int si=0,sj=0;
  struct side_x { edge_id e_id;    /* which edge */
                REAL vec[MAXCOORD];  /* side vector, from vertex */
                REAL norm;     /* length of side              */
              } *side;

  if ( valid_id ( get_vertex_fe(v_id) ) )
      return new_popverst(v_id,edges);
 
  side = (struct side_x *)temp_calloc(sizeof(struct side_x),edges);
  
  /* get edge vectors */
  e_id = get_vertex_edge(v_id);
  for ( i = 0 ; i < edges ; i++ )
  { 
    side[i].e_id = e_id;
    get_edge_tail_tangent(e_id,side[i].vec);
    side[i].norm = sqrt(SDIM_dot(side[i].vec,side[i].vec));
    e_id = get_next_tail_edge(e_id);
  }

  while ( edges > 3 )
  {
    /* find closest pair of edges */
    maxcos = -2.0;
    for ( i = 0 ; i < edges ; i++ )
    {
      for ( j = i+1 ; j < edges ; j++ )
      {
        cosine = SDIM_dot(side[i].vec,side[j].vec)/ side[i].norm/side[j].norm;
        if ( cosine > maxcos )
        { 
          /* toss in a little randomness if a choice of ways */
          /* but big problem if edges not adjacent! */
          /*  if ( (maxcos > -0.5) && (rand() & 0x0100) ) continue; */
          maxcos = cosine;
          si = i; 
          sj = j;
        }
      }
    }

    /* split off edges si and sj */
    new_v = dup_vertex(v_id);  /* new vertex in same spot */
    new_e = new_edge(v_id,new_v,NULLID);
    set_edge_color(new_e,BLACK);
    remove_vertex_edge(v_id,side[si].e_id);
    set_edge_tailv(side[si].e_id,new_v);
    remove_vertex_edge(v_id,side[sj].e_id);
    set_edge_tailv(side[sj].e_id,new_v);
    if ( get_vattr(v_id) & BOUNDARY )
         set_edge_boundary_num(new_e,get_edge_boundary(v_id)->num);
        
    /* take care of facet incidences, assuming plane configuration */
    do
    { facetedge_id first_fe;
      fe_id = first_fe = get_edge_fe(side[si].e_id);
      if ( valid_id(fe_id) )
      do
      {
        edge_id next_e;
        facetedge_id next_fe,new_fe,other_fe;
        facet_id f_id;
                  
        /* see if this facet got split */
        next_fe = get_prev_edge(fe_id);
        next_e = get_fe_edge(next_fe);
        if ( equal_id(inverse_id(side[sj].e_id),next_e) ) goto loopbottom;
          
        /* install new facetedge */
        f_id = get_fe_facet(fe_id);
        new_fe = new_facetedge(f_id,new_e);
        set_prev_edge(fe_id,new_fe);
        set_next_edge(next_fe,new_fe);
        set_prev_edge(new_fe,next_fe);
        set_next_edge(new_fe,fe_id);
        other_fe = get_edge_fe(new_e);
        if ( valid_id(other_fe) )
           {
              set_next_facet(new_fe,other_fe);
              set_prev_facet(new_fe,other_fe);
              set_prev_facet(other_fe,new_fe);
              set_next_facet(other_fe,new_fe);
           }
        else
           {
              set_edge_fe(new_e,new_fe);
              set_next_facet(new_fe,new_fe);
              set_prev_facet(new_fe,new_fe);
           }
loopbottom:
        fe_id = get_next_facet(fe_id);
      } while ( valid_id(fe_id) && !equal_id(fe_id,first_fe) );
      /* swap roles of sj and si */         
      i = sj; sj = si; si = i;
    }
    while ( sj < si ); /* exit when swapped back */

    /* adjust edge list for another round, if necessary */
    /* direction of new edge is average of old two */
    /* Also move new vertex a little ways away to avoid zero edge */
    edges --;
    x = get_coord(new_v);
    for ( i = 0 ; i < SDIM ; i++ )
    {
      side[si].vec[i] = side[si].vec[i]/side[si].norm 
                                    + side[sj].vec[i]/side[si].norm;
      x[i] += 0.000001 * side[si].vec[i];
    }
    side[si].norm = sqrt(SDIM_dot(side[si].vec,side[si].vec)); 
    side[si].e_id = new_e;
    if ( sj < edges ) /* trim top of list */
        side[sj] = side[edges];

    if ( phase_flag && (web.representation == STRING) )
       set_e_phase_density(new_e);
    else          
       set_edge_density(new_e,(get_edge_density(side[si].e_id)
           + get_edge_density(side[sj].e_id))/2);
  }
  temp_free((char *)side);
  return 1;
}


/*****************************************************************************
*
*  function: new_popverst()
*
*  purpose: pop vertex with more that 3 edges with variable density
*           Finds pair with greatest net force, allowing for force
*             of introduced edge.
*
*/

struct side_t { edge_id e_id;    /* which edge */
              facetedge_id fe;  /* canonical fe */
              REAL vec[MAXCOORD];  /* side vector, from vertex */
              REAL norm;     /* length of side              */
              REAL density;
              int degfree;  /* degrees of freedom */
            } ;

int anglecomp ARGS((struct side_t *,struct side_t *));

int anglecomp(a,b)
struct side_t *a,*b;
{ REAL aa = atan2(a->vec[1],a->vec[0]);
  REAL bb = atan2(b->vec[1],b->vec[0]);
  if ( aa < bb ) return -1;
  if ( aa > bb ) return 1;
  return 0;
}

int new_popverst(v_id,edges)
vertex_id v_id; /* vertex being popped */
int edges;  /* number of edges at this vertex */
{
  int i,j,m;
  int besti;
  vertex_id new_v;
  edge_id new_e;
  edge_id e_id,first_e;
  edge_id e1,e2; /* edges to be pulled out */
  REAL *x;
  facet_id f1,f2;
  REAL ff,f[MAXCOORD],fproj[MAXCOORD],ffmax;
  REAL new_density=0.0;
  struct side_t *side;
  REAL bestmove[MAXCOORD];
  facetedge_id fe_id,new_fe1,new_fe2,other_fe;
  int attr1,attr2,attrv;
  REAL cosa;
  facetedge_id fe;
  int degfree; /* degrees of freedom of popping vertex */
  struct boundary *vbdry; /* boundary of popping vertex */
  conmap_t *vmap,*emap;  /* constraint list of popping vertex */

  /* First, quick checks to rule out most vertices */
  edges = get_vertex_evalence(v_id);
  attrv = get_vattr(v_id);
  if ( !(attrv&FIXED) && !(attrv&(BOUNDARY|CONSTRAINT)) && (edges <= 3) )
    return 0;  /* typical interior vertex */
  if ( edges <= 1 ) return 0;

  /* Now, we have to get more detailed info */

  /* Get degrees of freedom of vertex */
  degfree = SDIM;
  if ( attrv & FIXED ) degfree = 0;
  else if ( attrv & BOUNDARY )
  { vbdry = get_boundary(v_id);
    if ( (vbdry->attr & NONWALL) )
      degfree = vbdry->pcount;
  }
  else if ( attrv & CONSTRAINT )
  { vmap = get_v_constraint_map(v_id);
    for ( i = 1; i <= (int)vmap[0] ; i++ )
     if (!((get_constraint(vmap[i])->attr)&(NONPOSITIVE|NONNEGATIVE|NONWALL)))
      degfree--;
  }

  side = (struct side_t *)temp_calloc(edges+1,sizeof(struct side_t));
  
  /* get edge vectors */
  e_id = first_e = get_vertex_edge(v_id);
  for ( i = 0 ; i < edges ; i++  )
  { 
    if  ( !valid_id(e_id) )
    { sprintf(errmsg,"Pop vertex: Invalid edge at vertex %s.\n",
             ELNAME(v_id));
      kb_error(1382,errmsg,RECOVERABLE);
    }

    /* Get degrees of freedom of edge */
    attr1 = get_eattr(e_id);
    side[i].degfree = SDIM;
    if ( attr1 & FIXED ) side[i].degfree = 0;
    else if ( attr1 & BOUNDARY )
    { if ( !(get_edge_boundary(e_id)->attr & NONWALL) )
        side[i].degfree = get_edge_boundary(e_id)->pcount;
    }
    else if ( attr1 & CONSTRAINT )
    { emap = get_e_constraint_map(e_id);
      for ( j = 1; j <= (int)emap[0] ; j++ )
       if (!((get_constraint(emap[j])->attr)&(NONPOSITIVE|NONNEGATIVE|NONWALL)))
        side[i].degfree--;
    }

    side[i].e_id = e_id;
    side[i].fe = get_edge_fe(e_id);
    if ( valid_id(side[i].fe) )
    { f1 = get_fe_facet(side[i].fe);
      if ( valid_id(f1) && inverted(f1) )
         side[i].fe = get_next_facet(side[i].fe);
    }
    get_edge_tail_tangent(side[i].e_id,side[i].vec);
    side[i].norm = sqrt(SDIM_dot(side[i].vec,side[i].vec));
    if ( side[i].norm <= 0.0 )
    { sprintf(errmsg,"verpop: edge %s length zero. \n",
        ELNAME(side[i].e_id));
      kb_error(1383,errmsg,WARNING);
      temp_free((char *)side);
      return 0;
    }
    side[i].density = get_edge_density(side[i].e_id);
    e_id = get_next_tail_edge(e_id);
    if ( equal_id(e_id,first_e) && ( i < edges-1 ) )
    { sprintf(errmsg,
             "Internal error: Expected %d edges; found %d.\n",edges,i+1);
      outstring(errmsg);
      kb_error(1384,
        "new_popverst(): Not expected number of edges around vertex\n",
           RECOVERABLE);
    }  
  }

  if ( edges==3 )
    if ( ( (side[0].degfree <= degfree) ? 1:0 ) 
        + ( (side[1].degfree <= degfree) ? 1:0 )
        + ( (side[2].degfree <= degfree) ? 1:0 ) >= 2 )
    {  temp_free((char *)side);
       return 0;
    }
        
  /* order edges around vertex geometrically */
  /*
  if ( SDIM == 2 )
     qsort((char*)side,edges,sizeof(struct side_t),FCAST anglecomp);
  else
  */
  { /* see if we can use facet info */
    facetedge_id fe;
    int top,bottom;
    int ends = 0;  /* can have at most two ends of facet fan */ 
    for ( i = 0 ; i < edges ; i++ )
    { if ( !valid_id(get_fe_facet(side[i].fe)) ) goto order_fail;    
      if ( equal_id(get_next_facet(side[i].fe),side[i].fe) ) ends++;
    }
    if ( (ends != 0) && (ends != 2) ) goto order_fail;
    if ( ends == 2 )
    { /* move an end to the first slot */
      for ( i = 0 ; i < edges ; i++ )
      { if ( equal_id(get_next_facet(side[i].fe),side[i].fe) )
        { struct side_t temp;
          if ( i == 0 ) break;
          temp = side[0];
          side[0] = side[i];
          side[i] = temp;
          break;
        }
      }
    }
    /* now trace path around facets */
    fe = side[0].fe;
    for ( i = 1 ; i < edges ; i++ )
    { facetedge_id next_fe = inverse_id(get_prev_edge(fe));
      edge_id next_e = get_fe_edge(next_fe);
      for ( j = i; j < edges ; j++ )
        if ( equal_id(side[j].e_id,next_e) )
        { struct side_t temp;
          if ( i == j ) break;
          temp = side[j];
          side[j] = side[i];
          side[i] = temp;
          break;
        }
      if ( j == edges ) goto order_fail;
      fe = get_next_facet(next_fe);
    }
    goto order_succeed;

order_fail:
    /* try geometric ordering by adding closest to existing chain */
    /* Leave first where it is, as initial link in chain */
    side[edges] = side[0];
    top = edges;
    bottom = 0;
    while ( top - bottom > 1 )
    { REAL topgap,bottomgap,topcos,bottomcos;
      int topbest=0,bottombest=0;
      bottomgap = topgap = -1e30;
      for ( i = bottom+1 ; i < top ; i++ )
      { bottomcos = SDIM_dot(side[i].vec,side[bottom].vec)/
                       side[i].norm/side[bottom].norm;
        if ( bottomcos > bottomgap )
        { bottomgap = bottomcos; bottombest = i; }
        topcos = SDIM_dot(side[i].vec,side[top].vec)/
                       side[i].norm/side[top].norm;
        if ( topcos > topgap )
        { topgap = topcos; topbest = i; }
      }
      if ( topgap > bottomgap )
      { /* add at top */
        top--;
        if ( topbest != top )
        { struct side_t temp = side[top];
          side[top] = side[topbest];
          side[topbest] = temp;
        } 
      }
      else
      { /* add at bottom */
        bottom++;
        if ( bottombest != bottom )
        { struct side_t temp = side[bottom];
          side[bottom] = side[bottombest];
          side[bottombest] = temp;
        } 
      }
    }
order_succeed: ;
  }

  side[edges] = side[0];  /* easy wraparound */

  if ( edges == 2 )
  { /* must be constraint or boundary or fixed */
    if ( (side[0].degfree <= degfree) || (side[1].degfree <= degfree ) )
    { temp_free((char *)side);
      return 0;
    }
    if ( (side[0].degfree <= 1 ) && (side[1].degfree <= 1 ) )
    { temp_free((char *)side);
      return 0;
    }
    besti = 0;
    /* find angle between edges to see which way we want to pop */
    cosa = SDIM_dot(side[0].vec,side[1].vec);
    e1 = side[0].e_id;
    e2 = side[1].e_id;
    if ( (side[0].degfree > degfree) && (side[1].degfree > degfree) &&
          (degfree >= 1) && (cosa < 0.0)  )
    { /* split apart along constraint rather than make Y */
      if ( verbose_flag )
      { sprintf(msg,"Popping vertex %s.\n",ELNAME(v_id));
        outstring(msg);
      }
      new_v = dup_vertex(v_id);  /* new vertex in same spot, to be pulled out */
      attrv = get_vattr(new_v);
      remove_vertex_edge(v_id,e2);
      set_edge_tailv(e2,new_v);
      fe = get_edge_fe(e1);
      set_prev_edge(fe,inverse_id(fe));
      fe = get_edge_fe(e2);
      set_prev_edge(fe,inverse_id(fe));
      goto newpop_exit;
    }
  }

  if ( pop_disjoin_flag && (edges == 4) )
  { /* see if exactly two opposite wedges have same body */
    facetedge_id fe_id[4];
    facet_id f_id[4];
    body_id b_id[4];
    int same02,same13;
    REAL *xnew,*xold,*x0,*x1,*x2,*x3;

    for ( i = 0 ; i < edges ; i++ )
    { /* assume positive facets on bodies */
      fe_id[i] = get_edge_fe(side[i].e_id);
      if ( !valid_id(fe_id[i]) )
      { f_id[i] = b_id[i] = NULLID;
        continue;
      }
      f_id[i] = get_fe_facet(fe_id[i]);
      if ( inverted(f_id[i]) )
      { fe_id[i] = get_next_facet(fe_id[i]);
        f_id[i] = get_fe_facet(fe_id[i]);
        if ( inverted(f_id[i]) )
        { f_id[i] = b_id[i] = fe_id[i] = NULLID;
          continue;
        }
      }
      b_id[i] = get_facet_body(f_id[i]);
    }
    same02 = equal_id(b_id[0],b_id[2]) && valid_id(b_id[0]);
    same13 = equal_id(b_id[1],b_id[3]) && valid_id(b_id[1]);
    if ( same02 != same13 )
    { /* do the disjoin */
      if ( same13 )
      { /* shuffle things so same02 */
        element_id temp;
        struct side_t tempside = side[0];
        side[0] = side[1]; side[1] = side[2]; side[2] = side[3]; 
        side[3] = tempside;
        temp = fe_id[0]; fe_id[0] = fe_id[1]; fe_id[1] = fe_id[2];
        fe_id[2] = fe_id[3]; fe_id[3] = temp;
        temp = f_id[0]; f_id[0] = f_id[1]; f_id[1] = f_id[2];
        f_id[2] = f_id[3]; f_id[3] = temp;
        temp = b_id[0]; b_id[0] = b_id[1]; b_id[1] = b_id[2];
        b_id[2] = b_id[3]; b_id[3] = temp;
      }
      /* now the real action */
      new_v = dup_vertex(v_id);  /* new vertex in same spot */
      remove_vertex_edge(v_id,side[1].e_id);
      set_edge_tailv(side[1].e_id,new_v);
      remove_vertex_edge(v_id,side[2].e_id);
      set_edge_tailv(side[2].e_id,new_v);
      if ( valid_id(f_id[0]) )
      { /* have to merge facets */
        facetedge_id fe_a = fe_id[0];
        facetedge_id fe_b = inverse_id(get_prev_edge(fe_a));
        facetedge_id fe_c = fe_id[2];
        facetedge_id fe_d = inverse_id(get_prev_edge(fe_c));
        set_prev_edge(fe_a,inverse_id(fe_d));
        set_prev_edge(fe_b,inverse_id(fe_c));
        set_prev_edge(fe_c,inverse_id(fe_b));
        set_prev_edge(fe_d,inverse_id(fe_a));
        if ( !equal_id(f_id[0],f_id[2]) )
        { /* merge f_id[2] into f_id[1] */
          facetedge_id fe = fe_c;
          do
          { set_fe_facet(fe,f_id[0]);
            fe = get_next_edge(fe);
          } while ( valid_id(fe) && !equal_element(f_id[0],get_fe_facet(fe)) );
          fe = inverse_id(fe_d);
          do
          { set_fe_facet(fe,f_id[0]);
            fe = get_prev_edge(fe);
          } while ( valid_id(fe) && !equal_element(f_id[0],get_fe_facet(fe)) );
          set_facet_body(f_id[2],NULLID);
          free_element(f_id[2]);
        } 
      }
    
      /* spread the vertices a bit */
      xold = get_coord(v_id);
      xnew = get_coord(new_v);
      x0 = side[0].vec;
      x1 = side[1].vec;
      x2 = side[2].vec;
      x3 = side[3].vec;
      for ( i = 0 ; i < SDIM ; i++ )
      { xold[i] += 0.05*x0[i] - 0.05*x1[i] - 0.05*x2[i] + 0.05*x3[i];
        xnew[i] += -0.05*x0[i] + 0.05*x1[i] + 0.05*x2[i] - 0.05*x3[i];
      }
 
      temp_free((char *)side);
      return 1;
    }
    /* else fall through and do ordinary wedge splitting */
  }

  /* If here, want to do Y pull out */
  /* find two edges that pull apart the most */
  ffmax = 0.01; /* margin to prevent looping with autopop */
  besti = -1;
  for ( i = 0 ; i < edges ; i++ )
    {
      e1 = side[i].e_id;
      e2 = side[i+1].e_id;

      /* test to see if this pair is valid */
      if ( side[i].degfree < 1 ) continue;
      if ( side[i+1].degfree < 1 ) continue;
      attr1 = get_eattr(e1);
      attr2 = get_eattr(e2);
      if ( get_edge_boundary(e1) != get_edge_boundary(e2) ) 
        continue;
      if ( (attr1 & CONSTRAINT) || (attr2 & CONSTRAINT) )
      { conmap_t *map1 = get_e_constraint_map(e1);
        conmap_t *map2 = get_e_constraint_map(e2);
        conmap_t ii,jj,found;
        found = 0;
        for ( ii = 1 ; ii <= map1[0] ; ii++ )
        { for ( jj = 1; jj <= map2[0] ; jj++ )
            if ( map1[ii] == map2[jj] ) found++;
        }
        if ( (found != map1[0]) && ( found != map2[0]) )
          continue;
      }

      if ( (edges <= 3) && (side[(6-i-i-1)%3].degfree <= degfree) )
      {  
        if ((side[i].degfree <= degfree)||(side[i+1].degfree <= degfree) )
        continue;   /* fake wall ??? */
      }

      /* check to see if better than what we have */
      for ( m = 0 ; m < SDIM ; m++ )
      { f[m] = (side[i].density*side[i].vec[m]/side[i].norm 
               + side[i+1].density*side[i+1].vec[m]/side[i+1].norm);          
      }
      if ( (degfree < SDIM) && 
          ((side[i].degfree <= degfree) || (side[i+1].degfree <= degfree)) )
      { force_project(f,v_id,fproj);
        ff = sqrt(SDIM_dot(fproj,fproj));
      }
      else ff = sqrt(SDIM_dot(f,f));
      if ( phase_flag )
      {
        f1 = get_fe_facet(get_prev_facet(side[i].fe));
        f2 = get_fe_facet(side[i+1].fe);
        new_density = phase_data[get_f_phase(f1)][get_f_phase(f2)];
      }
      else /* use minimal density of wedge edges */
      { REAL den1,den2;
        den1 = get_edge_density(e1);
        den2 = get_edge_density(e2);
        new_density = (den1 < den2) ? den1 : den2;
      }
      if ( ff - new_density > ffmax )
      { REAL mag;   /* size of new edge */
        ffmax = ff - new_density ; besti = i;
        mag = 0.1*((side[i].norm < side[i+1].norm) ?
                side[i].norm : side[i+1].norm);
        for ( m = 0 ; m < SDIM ; m++ )
           bestmove[m] = mag*f[m]/ff;
      }
    }          
  
  if ( besti == -1 ) 
  { temp_free((char *)side);
    return 0;
  }
  
  if ( verbose_flag )
  { sprintf(msg,"Popping vertex %s.\n",ELNAME(v_id));
    outstring(msg);
  }

  /* split off edges besti and besti+1 */
  e1 = side[besti].e_id;
  e2 = side[besti+1].e_id;
  attr1 = get_eattr(e1);
  attr2 = get_eattr(e2);
  new_v = dup_vertex(v_id);  /* new vertex in same spot, to be pulled out */
  attrv = get_vattr(new_v);
  /* move new vertex a little ways away to avoid zero edge */
  x = get_coord(new_v);
  for ( i = 0 ; i < SDIM ; i++ )
     x[i] += bestmove[i];

  new_e = new_edge(v_id,new_v,NULLID);
  set_edge_color(new_e,BLACK);
  remove_vertex_edge(v_id,e1);
  set_edge_tailv(e1,new_v);
  remove_vertex_edge(v_id,e2);
  set_edge_tailv(e2,new_v);
  set_edge_density(new_e,new_density);
  if ( (attr1 & NONCONTENT) || (attr2 & NONCONTENT) ) 
    set_attr(new_e,NONCONTENT);
  if ( (attr1 & NO_REFINE) || (attr2 & NO_REFINE) ) 
    set_attr(new_e,NO_REFINE);

  /* want to be careful here; new vertex and edge should get union of 
     properties of the two wedge edges */
  unset_attr(new_v,FIXED);
  if ( attrv & CONSTRAINT )
  { conmap_t *map = get_v_constraint_map(new_v);
    for ( i = map[0] ; i >= 0 ; i-- ) map[i] = 0;
    unset_attr(new_v,CONSTRAINT);
  }
  if ( attrv & BOUNDARY )
  { set_boundary_num(new_v,0);
    unset_attr(new_v,BOUNDARY);
  }
  if ( (attr1 & CONSTRAINT) || (attr2 & CONSTRAINT) )
  { conmap_t *map1 = get_e_constraint_map(e1);
    conmap_t *map2 = get_e_constraint_map(e2);
    conmap_t i,j;
    for ( i = 1 ; i <= map1[0] ; i++ )
    { set_v_constraint_map(new_v,map1[i]); 
      set_e_constraint_map(new_e,map1[i]); 
    }
    for ( j = 1 ; j <= map2[0] ; j++ )
    { set_v_constraint_map(new_v,map2[j]); 
      set_e_constraint_map(new_e,map2[j]); 
    }
  }
  else if ( ((attr1 & BOUNDARY) || (attr2 & BOUNDARY))
       && (get_vattr(v_id) & BOUNDARY) )
  { struct boundary *b1 = get_edge_boundary(e1);
    struct boundary *b2 = get_edge_boundary(e2);
    struct boundary *bv = get_boundary(v_id);
    if ( (b1 == bv) || (b2 == bv) )
    { set_edge_boundary_num(new_e,bv->num);
      set_boundary_num(new_v,bv->num);
      b_extrapolate(bv,get_coord(v_id),x,x,get_param(v_id),
            get_param(new_v),new_v);
    }
  }
        
  /* take care of facet incidences, assuming plane configuration */
  new_fe1 = NULLID;
  f1 = get_fe_facet(side[besti].fe);
  if ( valid_id(f1) )
  { /* install new facetedge */
    fe_id = side[besti].fe;
    fe_id = inverse_id(fe_id);
    other_fe = get_next_edge(fe_id);
    if ( !equal_element(side[besti+1].e_id,get_fe_edge(other_fe)) )
    { new_fe1 = new_facetedge(f1,new_e);
      set_edge_fe(new_e,new_fe1);
      set_prev_edge(other_fe,inverse_id(new_fe1));
      set_next_edge(fe_id,inverse_id(new_fe1));
      set_prev_edge(new_fe1,inverse_id(other_fe));
      set_next_edge(new_fe1,inverse_id(fe_id));
      set_next_facet(new_fe1,new_fe1);
      set_prev_facet(new_fe1,new_fe1);
    }
  }

  fe_id = get_prev_facet(side[besti].fe);
  if ( !equal_id(fe_id,side[besti].fe) )
  { f1 = get_fe_facet(fe_id);
    if ( valid_id(f1) )
    { /* install new facetedge */
      fe_id = inverse_id(fe_id);
      other_fe = get_next_edge(fe_id);
      if ( !equal_element(side[besti+1].e_id,get_fe_edge(other_fe)) )
      { 
        new_fe1 = new_facetedge(f1,new_e);
        set_edge_fe(new_e,new_fe1);
        set_prev_edge(other_fe,inverse_id(new_fe1));
        set_next_edge(fe_id,inverse_id(new_fe1));
        set_prev_edge(new_fe1,inverse_id(other_fe));
        set_next_edge(new_fe1,inverse_id(fe_id));
        set_next_facet(new_fe1,new_fe1);
        set_prev_facet(new_fe1,new_fe1);
      }
    }
  }

  f2 = get_fe_facet(side[besti+1].fe);
  if ( valid_id(f2) )
  {
    fe_id = side[besti+1].fe;
    other_fe = get_prev_edge(fe_id);
    if ( !equal_element(side[besti].e_id,get_fe_edge(other_fe)) )
    { new_fe2 = new_facetedge(f2,new_e);
      set_edge_fe(new_e,new_fe2);
      set_next_edge(other_fe,new_fe2);
      set_prev_edge(fe_id,new_fe2);
      set_next_edge(new_fe2,fe_id);
      set_prev_edge(new_fe2,other_fe);

      if ( valid_id(new_fe1) )
      { set_prev_facet(new_fe1,new_fe2);
        set_next_facet(new_fe1,new_fe2);
        set_prev_facet(new_fe2,new_fe1);
        set_next_facet(new_fe2,new_fe1);
      } else
      { set_next_facet(new_fe2,new_fe2);
        set_prev_facet(new_fe2,new_fe2);
      }
    }
  }
  fe_id = get_next_facet(side[besti+1].fe);
  if ( !equal_id(fe_id,side[besti+1].fe) )
  {
    f2 = get_fe_facet(fe_id);
    if ( valid_id(f2) )
    {
      other_fe = get_prev_edge(fe_id);
      if ( !equal_element(side[besti].e_id,get_fe_edge(other_fe)) )
      { new_fe2 = new_facetedge(f2,new_e);
        set_edge_fe(new_e,new_fe2);
        set_next_edge(other_fe,new_fe2);
        set_prev_edge(fe_id,new_fe2);
        set_next_edge(new_fe2,fe_id);
        set_prev_edge(new_fe2,other_fe);

        if ( valid_id(new_fe1) )
        { set_prev_facet(new_fe1,new_fe2);
          set_next_facet(new_fe1,new_fe2);
          set_prev_facet(new_fe2,new_fe1);
          set_next_facet(new_fe2,new_fe1);
        } else
        { set_next_facet(new_fe2,new_fe2);
          set_prev_facet(new_fe2,new_fe2);
        }
      }
    }
  }                

  /* make sure new edge has at least one facetedge structure */
  if ( !valid_id(get_edge_fe(new_e)) )
  { facetedge_id new_fe = new_facetedge(NULLID,new_e);
    set_edge_fe(new_e,new_fe);
    set_next_edge(new_fe,inverse_id(new_fe));
    set_prev_edge(new_fe,inverse_id(new_fe));
    set_next_facet(new_fe,new_fe);
    set_prev_facet(new_fe,new_fe);
  }

newpop_exit:
  temp_free((char *)side);
  if ( (edges > 4) || ((edges > 2) && (degfree < SDIM)) )
    return 1 + new_popverst(v_id,edges-1); /* recurse if need */
  return 1;
}

/******************************************************************************
    Autopop and autochop functions.
*****************************************************************************/
edge_id *autopop_list;  /* list of short edges found */
edge_id *autochop_list;  /* list of long edges found */

/*****************************************************************************
*
* Function: autopop_init()
*
* Purpose:  Allocates lists for accumulating poppable or choppable edges.
*/
void autopop_init()
{
  if ( autopop_flag )
  {
    autopop_list = (edge_id *)temp_calloc(web.skel[EDGE].count+web.skel[FACET].count,sizeof(edge_id));
  
  }
  if ( autochop_flag )
  {
    autochop_list = (edge_id *)temp_calloc(web.skel[EDGE].count,sizeof(edge_id));
  }
} /* end autopop_init() */

/**************************************************************************
*
* Function: autopop_cleanup()
*
* Purpose: deallocate lists
*/
void autopop_cleanup()
{ if ( autochop_list )
  { temp_free((char*)autochop_list);
    autochop_list = NULL;
  }
  if ( autopop_list )
  { temp_free((char*)autopop_list);
    autopop_list = NULL;
  }
} /* end autopop_cleanup() */


/********************************************************************
*
*  function: autopop_detect()
*
*  purpose: Find which edges will shrink to zero length or  beyond.
*           (half length when runge-kutta in effect)
*           Uses velocity attribute of vertices to predict motion,
*           so it does not chop growing small edges produced by 
*           topology changes.
*           Makes a list, but does not change anything.  Do
*           motion, then call autopop_pop() to eliminate edges.
*           Also detects for autochop.
*
*           Soapfilm model: also detects for facets disappearing.
*/

void autopop_detect(scale)
REAL scale;  /* scale factor for motion */
{
  edge_id e_id;
  REAL minlength = sqrt(2*scale); /* stability critical length */

  if ( autopop_quartic_flag )
    minlength = 2*sqrt(sqrt(scale));

#ifdef MPI_EVOLVER
  if ( this_task == MASTER_TASK )
  { mpi_autopop_detect(scale);
    return;
  }
#endif

  autopop_init();

  autopop_count = 0;
  autochop_count = 0;  
  FOR_ALL_EDGES(e_id)
  {
    vertex_id headv = get_edge_headv(e_id);
    REAL *headf = get_velocity(headv);
    vertex_id tailv = get_edge_tailv(e_id);
    REAL *tailf = get_velocity(tailv);
    REAL tailtan[MAXCOORD],headtan[MAXCOORD];
    REAL length,dx;

    get_edge_tail_tangent(e_id,tailtan);
    get_edge_tail_tangent(inverse_id(e_id),headtan);
    length = sqrt(SDIM_dot(tailtan,tailtan));
    if ( length > 0.0 )
      dx = -scale*(SDIM_dot(headf,headtan)
                          + SDIM_dot(tailf,tailtan))/length;
    else dx = 0.0;
    if ( autopop_flag && (dx <= 0.0) )
    { if ( (length < minlength) || 
          (length + dx <= (runge_kutta_flag ? 0.52*length : 0.0)) )
      { if ( immediate_autopop_flag )
        { int ret = eliminate_edge(e_id); 
          if ( ret ) free_element(e_id); 
          continue; /* next edge */
        }
        else /* add to list */
          autopop_list[autopop_count++] = e_id;
      }
    }
    if ( autochop_flag )
    if ( length + dx > autochop_length)
    { /* add to list */
      autochop_list[autochop_count++] = e_id;
    }
  }

  if ( web.representation == SOAPFILM )
  { facet_id f_id;

    FOR_ALL_FACETS(f_id)
    { int i;
      REAL sides[FACET_EDGES][MAXCOORD];
      REAL *velocities[FACET_VERTS];
      REAL area=0.0,area_rate;
      facetedge_id fe;
      REAL perim = 0.0;

      fe = get_facet_fe(f_id);
      for ( i = 0 ; i < FACET_VERTS ; i++ )
      { e_id = get_fe_edge(fe);
        get_edge_side(e_id,sides[i]);
        velocities[i] = get_velocity(get_edge_tailv(e_id));
        fe = get_next_edge(fe);
      }

      /* get area rate of change */
      area_rate = 0.0;
      for ( i = 0 ; i < FACET_VERTS ; i++ )
      { int ii = (i==0)?2:(i-1);
        REAL s1s1,s1s2,s2s2,s1v,s2v;
        s1s1 = SDIM_dot(sides[i],sides[i]);
        s2s2 = SDIM_dot(sides[ii],sides[ii]);
        s1s2 = -SDIM_dot(sides[i],sides[ii]);
        s1v  = SDIM_dot(sides[i],velocities[i]); 
        s2v  = -SDIM_dot(sides[ii],velocities[i]); 
        area = sqrt(s1s1*s2s2 - s1s2*s1s2);
        area_rate -= (s1v*(s2s2 - s1s2) + s2v*(s1s1 - s1s2))/area;
        perim += sqrt(s1s1);
      }
      if ( (area_rate < 0) && (area + scale*area_rate <= minlength*perim/2) )
      { if ( immediate_autopop_flag )
          eliminate_facet(f_id);
        else /* add to list */
          autopop_list[autopop_count++] = f_id;
      }
    } /* end facet loop */
  } /* end facet detect */

} /* end autopop_detect() */

/***********************************************************************
*
*  function: autopop_pop()
*
*  purpose: After motion, eliminate edges found by autopop_detect()
*           and pop any resulting bad vertices.
*           In soapfilm model, can also delete facets.
*
*/

void autopop_pop()
{
  int k;
  int popped = 0;

#ifdef MPI_EVOLVER
  if ( this_task == MASTER_TASK )
  { mpi_autopop_pop();
    return;
  }
#endif

  if ( autopop_list == NULL ) return;

#ifdef MPI_EVOLVER
  if ( this_task != MASTER_TASK )
    mpi_delete_init();
#endif
 
  for ( k = 0 ; k < autopop_count ; k++ )
  { if ( id_type(autopop_list[k]) == EDGE ) 
    { if ((get_eattr(autopop_list[k])&ALLOCATED) && 
                         eliminate_edge(autopop_list[k]))
        free_element(autopop_list[k]);
    }
    else
      eliminate_facet(autopop_list[k]);
  }
  if ( autopop_count )
  {
    if ( web.representation == STRING )
      popped = verpop_str();
    else 
      popped = popfilm();
  }
  
  if ( (autopop_count > 0) || (popped > 0) )
  { sprintf(msg,"Autopopped %d edges, %d vertices.\n",autopop_count,popped);
    outstring(msg);
  }

  temp_free((char *)autopop_list);
  autopop_list = NULL;
  autopop_count = 0;

#ifdef MPI_EVOLVER
  if ( this_task != MASTER_TASK )
    mpi_task_delete_wrapup();
#endif

  free_discards(DISCARDS_SOME); /* prevents auto pileup */

}


/***********************************************************************
*
*  function: autochop_chop()
*
*  purpose: After motion, divides edges found by autochop_detect()
*
*/

void autochop_chop()
{
  int k;

#ifdef MPI_EVOLVER
  if ( this_task == MASTER_TASK )
  { mpi_autopop_chop();
    return;
  }
#endif

  if ( autochop_list == NULL ) return;

  for ( k = 0 ; k < autochop_count ; k++ )
    { /* printf("chopping edge %s\n",ELNAME(autochop_list[k])); */
      if ( !valid_element(autochop_list[k]) )
         continue;
      edge_refine(autochop_list[k]);      
 
    }

  if ( autochop_count > 0 )
     { sprintf(msg,"Autochopped %d edges.\n",autochop_count);
       outstring(msg);
     }

  temp_free((char *)autochop_list);
  autochop_list = NULL;
  autochop_count = 0;

}

