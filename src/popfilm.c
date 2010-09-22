/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/******************************************************************
*
*  File:    popfilm.c
*
*  Purpose:  Convert all tangent cones to minimal cones in
*                soapfilm model.  
*/

#include "include.h"

/*********************************************************************
*
*  Function: popfilm()
*
*  Purpose:  Overall control of tangent cone minimization.
*                Creates sorted list of vertex-facet incidences
*                sorted on vertex.  Analyzes tangent cone for each
*                vertex and calls minimizer for the non-minimal.
*/


/* comparison routine for qsort */
static int vfcomp(a,b)
struct verfacet *a,*b;
{
  if ( a->v_id < b->v_id ) return -1;
  if ( a->v_id > b->v_id ) return 1;
  return 0;
}

int popfilm()
{
  int popped = 0;

  if ( web.counts_reported & edge_pop_count_bit )
    web.edge_pop_count = 0;
  popped = edgepop_film();
  web.counts_reported |= edge_pop_count_bit;
  sprintf(msg,"Edges popped: %d\n",popped); outstring(msg);

  popped = verpop_film();
  
  return popped;
}


/********************************************************************
*
*  Function: edgepop_film()
*
*  Purpose:  Reconfigure all edges with more than 3 facets into
*                triple edges.  Also does two or more facets with 
*                common edge on boundary or constraint.
*/

int  edgepop_film()
{
  edge_id e_id,sentinel;
  int popped = 0;

  /* Loop through all edges, popping as you go.  New edges created
     during popping will be scanned to see if they are still
     pop-worthy.
   */

  e_id = NULLEDGE;
  while ( generate_all(EDGE,&e_id,&sentinel) )
  { popped += pop_one_edge(e_id);
  }

  return popped;
}

/***************************************************************************
*
* function: vertex_degfree()
*
* purpose: find degrees of freedom left by a vertex's constraints
*
*/

int vertex_degfree(v_id)
vertex_id v_id;
{ int degfree;
  int vattr = get_vattr(v_id);
  struct boundary *vbdry;
  conmap_t *vmap;
  int i;

  /* Get degrees of freedom of vertex */
  degfree = SDIM;
  if ( vattr & FIXED ) degfree = 0;
  else if ( vattr & BOUNDARY )
  { vbdry = get_boundary(v_id);
    if ( (vbdry->attr & NONWALL) )
      degfree = vbdry->pcount;
  }
  else if ( vattr & CONSTRAINT )
  { vmap = get_e_constraint_map(v_id);
    for ( i = 1; i <= (int)vmap[0] ; i++ )
     if (!((get_constraint(vmap[i])->attr)&(NONPOSITIVE|NONNEGATIVE|NONWALL)))
      degfree--;
  }
 
  return degfree;
}

/***************************************************************************
*
* function: edge_degfree()
*
* purpose: find degrees of freedom left by a edge's constraints
*
*/

int edge_degfree(e_id)
edge_id e_id;
{ int degfree;
  int eattr = get_eattr(e_id);
  struct boundary *ebdry;
  conmap_t *emap;
  int i;

  /* Get degrees of freedom of edge */
  degfree = SDIM;
  if ( eattr & FIXED ) degfree = 0;
  else if ( eattr & BOUNDARY )
  { ebdry = get_edge_boundary(e_id);
    if ( (ebdry->attr & NONWALL) )
      degfree = ebdry->pcount;
  }
  else if ( eattr & CONSTRAINT )
  { emap = get_e_constraint_map(e_id);
    for ( i = 1; i <= (int)emap[0] ; i++ )
     if (!((get_constraint(emap[i])->attr)&(NONPOSITIVE|NONNEGATIVE|NONWALL)))
      degfree--;
  }
 
  return degfree;
}

/***************************************************************************
*
* function: facet_degfree()
*
* purpose: find degrees of freedom left by a facet's constraints
*
*/

int facet_degfree(f_id)
facet_id f_id;
{ int degfree;
  int fattr = get_fattr(f_id);
  struct boundary *fbdry;
  conmap_t *fmap;
  int i;

  /* Get degrees of freedom of edge */
  degfree = SDIM;
  if ( fattr & FIXED ) degfree = 0;
  else if ( fattr & BOUNDARY )
  { fbdry = get_facet_boundary(f_id);
    if ( (fbdry->attr & NONWALL) )
      degfree = fbdry->pcount;
  }
  else if ( fattr & CONSTRAINT )
  { fmap = get_f_constraint_map(f_id);
    for ( i = 1; i <= (int)fmap[0] ; i++ )
     if (!((get_constraint(fmap[i])->attr)&(NONPOSITIVE|NONNEGATIVE|NONWALL)))
      degfree--;
  }
 
  return degfree;
}

/********************************************************************
*
*  Function: pop_one_edge()
*
*  Purpose:  Try popping one edge.  Tests for more than 3 facets.
*                Also does two or more facets with 
*                common edge on boundary or constraint.
*/

REAL new_displacement[MAXCOORD]; /* one normal of splittin wedge */

int pop_one_edge(e_id)
edge_id e_id;
{
  int facet_count = 0;
  facetedge_id fe_id,fe,key_fe=NULLID,new_key;
  REAL side[MAXCOORD],sideA[MAXCOORD],sideB[MAXCOORD];
  REAL normA,normB,normalA[MAXCOORD],normalB[MAXCOORD];
  REAL maxcos,newcos;
  int didsplit;
  int foundwedge;
  facetedge_id first_fe;
  int eattr = get_eattr(e_id);
  facet_id f1,f2;
  int attr1,attr2;
  int i,k;
  REAL midnormal[MAXCOORD];
  int popped = 0;
  int degfree;  /* degrees of freedom of edge */
  int fdegfree,next_fdegfree;
  int popattr = -1;  /* in case of kraynikpop */
  int kraynikwedge; /* whether current wedge has kraynik preference */
  int foundkraynik;
  int maxffree;  /* maximum degrees of freedom of adjacent facets */
  int maxffreen; /* number of adjacent facets with that freedom */
  int septum_flag=1; /* whether to put in septum joining popped edges */

  /* count facets around edge */
  fe_id = first_fe = get_edge_fe(e_id);
  maxffree = 0; maxffreen = 0;
  if ( valid_id(fe_id) )
  do 
  { int ffree = facet_degfree(get_fe_facet(fe_id));
    if ( ffree > maxffree )
    { maxffree = ffree; maxffreen = 1; }
    else if ( ffree == maxffree ) maxffreen ++;
    facet_count++;
    fe_id = get_next_facet(fe_id);
  } while ( valid_id(fe_id) && !equal_id(fe_id,first_fe) );

  /* only pop if more than 3 facets, or a wall or wire */
  if ( !(eattr & (FIXED|BOUNDARY|CONSTRAINT) && (maxffreen >= 2))
       && !(maxffreen > 3) )
     return 0;

  /* Get degrees of freedom of edge */
  degfree = edge_degfree(e_id);
 
  if ( (facet_count == 3) && (degfree == SDIM) ) return 0;

  if ( kraynikpopedge_flag )
  { /* find facets that disagree on edge_pop_attribute */
    popattr = find_attribute(FACET,"edge_pop_attribute");
  }
  
  foundwedge = 0;
  foundkraynik = 0;
 
  if ( pop_disjoin_flag && (facet_count == 4) )
  { /* test for merging Plateau borders */
    facetedge_id fea,feb,fec,fed;
    body_id ba,bb,bc,bd;
    fea = get_edge_fe(e_id);
    feb = get_next_facet(fea); 
    fec = get_next_facet(feb); 
    fed = get_next_facet(fec); 
    ba = get_facet_body(get_fe_facet(fea));
    bb = get_facet_body(get_fe_facet(feb));
    bc = get_facet_body(get_fe_facet(fec));
    bd = get_facet_body(get_fe_facet(fed));
    if ( valid_id(ba) && equal_id(ba,bc) ) 
    { key_fe = feb;
      foundwedge = 1;
      septum_flag = 0;
    }
    else if ( valid_id(bb) && equal_id(bb,bd) ) 
    { key_fe = fea;
      foundwedge = 1;
      septum_flag = 0;
    }
  }

  if ( !foundwedge )
  {
    /* find narrowest wedge to pull out */
    get_edge_side(e_id,side);
    maxcos = -2.0;    /* for finding minimum angle between facets */

    /* find first facet normal */
    fe = get_edge_fe(e_id);
    get_fe_side(get_next_edge(fe),sideA);
    cross_prod(side,sideA,normalA);
    normA = sqrt(SDIM_dot(normalA,normalA));
    next_fdegfree = fdegfree = facet_degfree(get_fe_facet(fe));
    for ( i = 0 ; i < facet_count ; i++ )
    { facetedge_id next_fe;

      next_fe = get_next_facet(fe);
      fdegfree = next_fdegfree;
      next_fdegfree = facet_degfree(get_fe_facet(next_fe));

      /* test wedge for compatibility */
      if ( fdegfree < 3 ) continue;
      if ( next_fdegfree < 3 ) continue;
      f1 = get_fe_facet(fe);
      f2 = get_fe_facet(next_fe);
      attr1 = get_fattr(f1);
      attr2 = get_fattr(f2);
      if ( get_facet_boundary(f1) != get_facet_boundary(f2) )
        continue;
      if ( (attr1 & CONSTRAINT) || (attr2 & CONSTRAINT) )
      { conmap_t *map1 = get_f_constraint_map(f1);
        conmap_t *map2 = get_f_constraint_map(f2);
        conmap_t ii,jj,found;
        found = 0;
        for ( ii = 1 ; ii <= map1[0] ; ii++ )
        { for ( jj = 1; jj <= map2[0] ; jj++ )
            if ( map1[ii] == map2[jj] ) found++;
        }
        if ( (found != map1[0]) || ( found != map2[0]) )
          continue;
      }
      if ( (facet_count <= 3) &&
         ((fdegfree <= degfree)||(next_fdegfree <= degfree) ) )
        continue;  /* fake wall */

      /* get normal */
      get_fe_side(get_next_edge(next_fe),sideB);
      cross_prod(side,sideB,normalB);
      normB = sqrt(SDIM_dot(normalB,normalB));
      newcos = SDIM_dot(normalA,normalB)/normA/normB;

      /* If phases in effect, can estimate force */
      if ( phase_flag )
      { REAL tensionA = get_facet_density(f1);
        REAL tensionB = get_facet_density(f2);
        REAL tensionC; /* for the proposed interface */
        REAL force1;
        REAL sum;
        int ii = get_b_phase(get_facet_body(f1));
        int jj = get_b_phase(get_facet_body(inverse_id(f2)));
        tensionC = phase_data[ii][jj];
        for ( k = 0, sum = 0.0 ; k < SDIM ; k++ )
        { REAL term = 
             (tensionA*normalA[0]/normA + tensionB*normalB[0]/normB);
          sum += term;
        }
        force1 = sqrt(sum);
        newcos = force1 - tensionC;
      }

      if ( popattr >= 0 )   /* kraynikpop test */
      { kraynikwedge = ( *(int*)get_extra(f1,popattr) != 
                            *(int*)get_extra(f2,popattr) );
      }
      else kraynikwedge = 0;

      if ( ((newcos > maxcos) && (kraynikwedge >= foundkraynik))
               || (kraynikwedge > foundkraynik) )
      { key_fe = next_fe;
        maxcos = newcos;
        for ( k = 0 ; k < SDIM ; k++ )
           midnormal[k] = normalA[k]/normA + normalB[k]/normB;
        cross_prod(midnormal,side,new_displacement);
      }
      if ( kraynikwedge ) foundkraynik = 1;

      /* set up for next angle */
      normA = normB;
      memcpy((char *)normalA,(char *)normalB,sizeof(normalA));
      fe = next_fe;
    }
    /* now a little check just to make sure things happened as planned */
    if ( (facet_count >= 4) && (maxcos < 0.0) && !foundkraynik )
    { sprintf(errmsg,
      "Can't find poppable pair of facets on poppable edge %s.\n",
          ELNAME(e_id));
      kb_error(1299,errmsg, RECOVERABLE);
    }
  }
  if ( !valid_id(key_fe) ) return 0;

  /* check boundary and constraint compatibility of wedge */
  /* test to see if we really want to do this pop */
  f1 = get_fe_facet(key_fe);
  f2 = get_fe_facet(get_next_edge(key_fe));
  attr1 = get_fattr(f1);
  attr2 = get_fattr(f2);
  if ( (attr1 & FIXED) || (attr2 & FIXED) )
     return 0; 
  if ( get_facet_boundary(f1) != get_facet_boundary(f2) ) 
     return 0;
  if ( (attr1 & CONSTRAINT) || (attr2 & CONSTRAINT) )
  { conmap_t *map1 = get_e_constraint_map(f1);
    conmap_t *map2 = get_e_constraint_map(f2);
    conmap_t ii,jj,found;
    found = 0;
    for ( ii = 1 ; ii <= map1[0] ; ii++ )
    { 
      for ( jj = 1; jj <= map2[0] ; jj++ )
        if ( map1[ii] == map2[jj] ) found++;
    }
    if ( (found != map1[0]) || ( found != map2[0]) )
      return 0;
  }

  if ( verbose_flag )
  { sprintf(msg,"Popping edge %s\n",ELNAME(e_id));
    outstring(msg);
  }


  didsplit = 0;

  if ( (facet_count == 2) && (degfree==2) )
  { /* special treatment, since may split either way */
    popped = two_split(key_fe,septum_flag);
    if ( popped ) return popped;  /* else do Y pull-out */
  }

  /* try propagating split forward */
  new_key = key_fe;
  while ( try_prop(&new_key,key_fe,septum_flag) ) 
  { didsplit = 1;
    if ( verbose_flag )
    { sprintf(msg," Propagating pop to edge %s\n",ELNAME(get_fe_edge(new_key)));
      outstring(msg);
    }
    popped++;
  }

  /* try propagating split backward */
  new_key = inverse_id(get_prev_facet(key_fe));
  while ( try_prop(&new_key,inverse_id(get_prev_facet(key_fe)),septum_flag) ) 
  { didsplit = 1;
    if ( verbose_flag )
    { sprintf(msg," Propagating pop to edge %s\n",ELNAME(get_fe_edge(key_fe)));
      outstring(msg);
    }
    popped++;
  }

  /* if can't split forward or backward, divide edge and split */
  if ( !didsplit )
  {
    edge_refine(e_id);
    popped += pop_one_edge(e_id);  /* try again */
  }

  return popped;
} /* end pop_one_edge */


/***********************************************************************
*  
*  Function: try_prop()
*
*  Purpose:  Sees if vertex at head of *pass_key can be split.  
*                If it can, it does.  If split can be propagated
*                further, it returns the next edge to try in *pass_key.
*                If the vertex was split, it returns 1, else 0.
*                Does not split FIXED vertices. Or BOUNDARY vertices.
*/

int try_prop(pass_key,start_key,septum_flag)
facetedge_id *pass_key;
facetedge_id  start_key; /* to check for complete loop */
int septum_flag; /* whether to put septum between popped edges */
{
  int splitflag;
  int propflag;
  facetedge_id wing_fe;
  facetedge_id flip_fe;
  facetedge_id new_key=0;
  facetedge_id key_fe = *pass_key;
  edge_id e_id = get_fe_edge(key_fe);
  edge_id next_e=0;

  if ( !valid_id(*pass_key) ) return 0;

  /* if ( get_vattr(get_fe_headv(key_fe)) & FIXED ) return 0; */

  /* don't even try if valence is already <= 3 */
  /*if ( get_edge_valence(e_id) <= 3 ) return 0; */

  /* swing forward on one side until find multiple edge */
  flip_fe = inverse_id(key_fe);
  for (;;)
  {
    wing_fe = get_next_edge(inverse_id(flip_fe));
    if ( equal_id(wing_fe,start_key) )
    { /* have made complete loop around */
      splitflag = 1;
      propflag = 0;
      break;
    }
    flip_fe = get_next_facet(wing_fe);
    if ( equal_id(wing_fe,flip_fe) )
    { /* dead end on wing, so OK to split vertex maybe */
      if ( edge_degfree(get_fe_edge(wing_fe)) >= 2 )
        splitflag = 1;
      else splitflag = 0;
      propflag = 0;
      break;
    }
    if ( !equal_id(wing_fe,get_next_facet(flip_fe))
        || (get_eattr(get_fe_edge(wing_fe)) & (FIXED|BOUNDARY|CONSTRAINT)) )
    { /* have found multiple edge, or wall or wire */
      next_e = get_fe_edge(wing_fe);
      if ( !equal_element(e_id,next_e) )
      { /* have found legitimate next edge */
        splitflag = 1;
        propflag = 1;
        new_key = wing_fe;
        break;
      }
      if ( equal_element(wing_fe,get_prev_facet(key_fe)) )
      { /* have cirque, so OK to split, but no propagation */
        splitflag = 1;
        propflag = 0;
      }
      else
      { /* have cirque crossing proposed split */
        /* which ends propagation */
        splitflag = 0;
        propflag = 0;
      }
      break;
    }
  }
  if ( !splitflag ) return 0;

  /* swing forward on other side until find multiple edge */
  flip_fe = inverse_id(get_prev_facet(key_fe));
  for (;;)
  {
    wing_fe = get_next_edge(inverse_id(flip_fe));
    flip_fe = get_next_facet(wing_fe);
    if ( equal_id(wing_fe,flip_fe) )
    { /* dead end on wing, so OK to split vertex maybe */
      if ( edge_degfree(get_fe_edge(wing_fe)) < 2 )
        splitflag = 0;
      propflag = 0;
      break;
    }
    if ( !equal_id(wing_fe,get_next_facet(flip_fe)) 
        || (get_eattr(get_fe_edge(wing_fe)) & (FIXED|BOUNDARY|CONSTRAINT)) )
    { /* have found multiple edge */
      if ( !equal_id(get_fe_edge(wing_fe),next_e) )
      {
        /* can't split; have serious nonminimal vertex */
        splitflag = 0;
        propflag = 0;
        break;
      }
      else /* check normals */
      {
      }
      break;
    }
  }

  if ( !propflag ) new_key = NULLFACETEDGE;

  /* check to see if new_key is already just triple */
  { facetedge_id feb = get_next_facet(new_key);
    facetedge_id fec = get_next_facet(feb);
    facetedge_id fed = get_next_facet(fec);
    if ( equal_id(fec,new_key) )
    { /* can't split further */
      splitflag = 0;
    }
    else if ( equal_id(fed,new_key) )
    { /* already triple edge; split and stop */
      propflag = 0;
    }
  } 
  if ( splitflag ) versplit(key_fe,new_key,septum_flag);

  if ( !propflag ) new_key = NULLFACETEDGE;
  *pass_key = new_key;

  return splitflag;
} /* end try_prop */

/**********************************************************************
*
*  Function: versplit()
*
*  Purpose:  Split a wedge of facets off a vertex along two edges.
*                Old edges go with split wedge (old edges now proper
*                minimal cones) and new edges have rest of old facets
*                and await scanning for minimality.  Arguments are
*                facetedges for the two edges inside the wedge; if 
*                second is null, means there is no second edge.
*                This is essentially the reverse of eliminating an edge.
*/

void versplit(fe_a,fe_b,septum_flag)
facetedge_id fe_a,fe_b;
int septum_flag;
{
  vertex_id old_v = get_fe_headv(fe_a);
  vertex_id new_v;
  edge_id new_e=0,new_a=0,new_b=0,e_id;
  edge_id old_a = get_fe_edge(fe_a);
  edge_id old_b = get_fe_edge(fe_b);
  facetedge_id fe_aa = get_prev_facet(fe_a);
  facetedge_id fe_bb = get_prev_facet(fe_b);
  facetedge_id fe_a_old,fe_a_new,fe_a_e;
  facetedge_id fe_b_old=0,fe_b_new=0,fe_b_e=0;
  facet_id new_fa,new_fb=0;
  body_id b_id;
  int i;
  REAL *x;
  int halfflag;
  facetedge_id fe,first_fe;
  int valence = get_edge_valence(old_a);
  conmap_t *vmap,*emap,*fmap;
  facetedge_id halffe;

  /* create a new vertex, which will be split away with wedge */
  new_v = dup_vertex(old_v);
  x = get_coord(new_v);
  for ( i = 0 ; i < SDIM ; i++ )
     x[i] += 0.01*new_displacement[i];
  
  if ( septum_flag )
  { /* create new edge between vertices */
    new_e = new_edge(old_v,new_v,NULLID);
  }
  
  /* create new edges split off from wedge */
  if ( !equal_id(get_prev_facet(fe_aa),fe_a) )
  {
    new_a = dup_edge(old_a);
    insert_vertex_edge(get_edge_tailv(old_a),new_a);
    insert_vertex_edge(old_v,inverse_id(new_a));
  }

  /* fix up attributes of old edge, in case pulling off wall */
  unset_attr(old_a,FIXED);
  set_edge_boundary_num(old_a,0);
  unset_attr(old_a,BOUNDARY); 
  fmap = get_f_constraint_map(get_fe_facet(fe_a));
  emap = get_e_constraint_map(old_a);
  for ( i = 1 ; i <= (int)fmap[0] ; i++ )
    emap[i] = fmap[i]; 
  for ( ; i <= (int)emap[0] ; i++ ) emap[i] = 0;
  emap[0] = fmap[0];
  if ( emap[0] == 0 ) unset_attr(old_a,CONSTRAINT);
  else set_attr(old_a,CONSTRAINT);

  if ( valid_id(fe_b) )
  {
    new_b = dup_edge(old_b);
    insert_vertex_edge(old_v,new_b);
    insert_vertex_edge(get_edge_headv(old_b),inverse_id(new_b));
    /* fix up attributes of old edge, in case pulling off wall */
    unset_attr(old_b,FIXED);
    set_edge_boundary_num(old_b,0);
    unset_attr(old_b,BOUNDARY); 
    fmap = get_f_constraint_map(get_fe_facet(fe_b));
    emap = get_e_constraint_map(old_b);
    for ( i = 1 ; i <= (int)fmap[0] ; i++ )
      emap[i] = fmap[i]; 
    for ( ; i <= (int)emap[0] ; i++ ) emap[i] = 0;
    emap[0] = fmap[0];
    if ( emap[0] == 0 ) unset_attr(old_b,CONSTRAINT);
    else set_attr(old_b,CONSTRAINT);
  }
 
  /* reset edge endpoints coming into new_v */
  fe = fe_a;
  halfflag = 0;
  halffe = NULLID; /* for attributes of new_v */
  do
  { e_id = get_fe_edge(fe);
    remove_vertex_edge(old_v,inverse_id(e_id));
    set_edge_headv(e_id,new_v);
    if ( equal_id(fe,get_next_facet(fe)) )
    { halfflag = 1; /* need to go back and do other side of wedge */
      halffe = fe;
      break;
    }
    fe = get_next_facet(inverse_id(get_next_edge(fe)));
    if ( !valid_id(halffe) ) halffe = fe;
  } while ( !equal_id(fe,fe_a) );
  if ( halfflag )
  { fe = get_prev_facet(inverse_id(get_next_edge(fe_aa)));
    if ( !valid_id(halffe) ) halffe = fe;
    do
    {
      e_id = get_fe_edge(fe);
      remove_vertex_edge(old_v,inverse_id(e_id));
      set_edge_headv(e_id,new_v);
      if ( equal_id(fe,get_next_facet(fe)) )
        break;
      fe = get_prev_facet(inverse_id(get_next_edge(fe)));
    } while ( !equal_id(fe,fe_aa) );
  }
  /* fix up attributes of new_v, which was clone of old_v */
  unset_attr(new_v,FIXED);
  if ( valid_id(halffe) )
  { /* set attributes to same as halffe */
    e_id = get_fe_edge(halffe);
    if ( get_eattr(e_id) & BOUNDARY )
    { if ( get_boundary(new_v) != get_edge_boundary(e_id) )
        kb_error(2440,"Trying to pop vertex not on same boundary as incoming edge.",RECOVERABLE);
    }
    else
    { set_boundary_num(new_v,0);
      unset_attr(new_v,BOUNDARY);
    }
    vmap = get_v_constraint_map(new_v);
    emap = get_e_constraint_map(e_id);
    for ( i = 1 ; i <= (int)emap[0] ; i++ ) vmap[i] = emap[i];
    for ( ; i <= (int)vmap[0] ; i++ ) vmap[i] = 0;
    vmap[0] = emap[0];
    if ( vmap[0] ) set_attr(new_v,CONSTRAINT);
    else unset_attr(new_v,CONSTRAINT);
  }

  if ( septum_flag )
  {
    /* fix up attributes of new_e, which was brand new */
    if ( valid_id(halffe) )
    { /* set attributes to same as halffe */
      e_id = get_fe_edge(halffe);
      if ( get_eattr(e_id) & BOUNDARY )
      { set_edge_boundary_num(new_e,get_edge_boundary_num(e_id));
        set_attr(new_e,BOUNDARY);
      }
      if ( get_eattr(e_id) & CONSTRAINT )
      { conmap_t *eemap = get_e_constraint_map(new_e);
        emap = get_e_constraint_map(e_id);
        for ( i = 1 ; i <= (int)emap[0] ; i++ ) eemap[i] = emap[i];
        eemap[0] = emap[0];
        set_attr(new_e,CONSTRAINT);
      }
    }

    /* create two new facets */
    new_fa = dup_facet(get_fe_facet(fe_a));
    if ( valid_id(fe_b) )
    {
       new_fb = dup_facet(get_fe_facet(fe_b));
    }
  
    /* create new facet-edges */
    fe_a_e = new_facetedge(new_fa,new_e);
    fe_a_new = new_facetedge(new_fa,new_a);
    fe_a_old  = new_facetedge(new_fa,inverse_id(old_a));
    if ( valid_id(fe_b) )
    {
       fe_b_e = new_facetedge(new_fb,inverse_id(new_e));
       fe_b_new = new_facetedge(new_fb,new_b);
       fe_b_old  = new_facetedge(new_fb,inverse_id(old_b));
    }
  
    /* link elements to facet-edges */
    set_edge_fe(old_a,inverse_id(fe_a_old)); 
    set_edge_fe(new_a,fe_a_new);
    set_edge_fe(new_e,fe_a_e);
    set_facet_fe(new_fa,fe_a_new);
    if ( valid_id(fe_b) )
    {
      set_edge_fe(old_b,inverse_id(fe_b_old)); 
      set_edge_fe(new_b,fe_b_new);
      set_facet_fe(new_fb,fe_b_new);
    }
  
    /* link edges around new facets */
    set_next_edge(fe_a_e,fe_a_old);
    set_next_edge(fe_a_old,fe_a_new);
    set_next_edge(fe_a_new,fe_a_e);
    set_prev_edge(fe_a_old,fe_a_e);
    set_prev_edge(fe_a_new,fe_a_old);
    set_prev_edge(fe_a_e,fe_a_new);
    if ( valid_id(fe_b) )
    {
       set_next_edge(fe_b_e,fe_b_new);
       set_next_edge(fe_b_old,fe_b_e);
       set_next_edge(fe_b_new,fe_b_old);
       set_prev_edge(fe_b_old,fe_b_new);
       set_prev_edge(fe_b_new,fe_b_e);
       set_prev_edge(fe_b_e,fe_b_old);
    }
  
    /* link facets around edges */
    
    /* new middle edge */
    if ( valid_id(fe_b) )
    { set_next_facet(fe_a_e,inverse_id(fe_b_e));
      set_next_facet(fe_b_e,inverse_id(fe_a_e));
      set_prev_facet(fe_a_e,inverse_id(fe_b_e));
      set_prev_facet(fe_b_e,inverse_id(fe_a_e));
    }
    else
    { set_next_facet(fe_a_e,fe_a_e);
      set_prev_facet(fe_a_e,fe_a_e);
    }
      
    /* facet A old edge */
    set_next_facet(fe_a_old,inverse_id(fe_a));
    set_prev_facet(fe_a_old,inverse_id(fe_aa));
  
    /* facet A new edge */
    if ( valence >= 3 )
    { set_next_facet(fe_a_new,get_next_facet(fe_a));
      set_prev_facet(fe_a_new,get_prev_facet(fe_aa));
    }
    else
    { set_next_facet(fe_a_new,fe_a_new);
      set_prev_facet(fe_a_new,fe_a_new);
    }
  
    /* reset old links */
    set_next_facet(get_prev_facet(fe_a_old),fe_a_old);
    set_prev_facet(get_next_facet(fe_a_old),fe_a_old);
    set_next_facet(get_prev_facet(fe_a_new),fe_a_new);
    set_prev_facet(get_next_facet(fe_a_new),fe_a_new);
  
    /* reset edges of facet-edges around new edge */
    fe = first_fe = get_edge_fe(new_a);
    if ( valid_id(fe) ) do
    { set_fe_edge(fe,new_a);
      fe = get_next_facet(fe);
    } while ( valid_id(fe) && !equal_id(fe,first_fe) );
  
    if ( valid_id(fe_b) )
    { /* facet B old edge */
      set_next_facet(fe_b_old,inverse_id(fe_b));
      set_prev_facet(fe_b_old,inverse_id(fe_bb));
  
      /* facet B new edge */
      set_next_facet(fe_b_new,get_next_facet(fe_b));
      set_prev_facet(fe_b_new,get_prev_facet(fe_bb));
  
      /* reset old links */
      set_next_facet(get_prev_facet(fe_b_old),fe_b_old);
      set_prev_facet(get_next_facet(fe_b_old),fe_b_old);
      set_next_facet(get_prev_facet(fe_b_new),fe_b_new);
      set_prev_facet(get_next_facet(fe_b_new),fe_b_new);
  
      /* reset edges of facet-edges around new edge */
      fe = first_fe = get_edge_fe(new_b);
      if ( valid_id(fe) ) do
      { set_fe_edge(fe,new_b);
        fe = get_next_facet(fe);
      } while ( valid_id(fe) && !equal_id(fe,first_fe) );
    }
  
    /* set new facet bodies */
    fe = get_next_facet(fe_a_new);
    b_id = get_facet_body(get_fe_facet(fe));
    set_facet_body(inverse_id(new_fa),b_id);
    if ( valid_id(fe_b) )
       set_facet_body(inverse_id(new_fb),b_id);
    fe = inverse_id(get_prev_facet(fe_a_new));
    b_id = get_facet_body(get_fe_facet(fe));
    set_facet_body(new_fa,b_id);
    if ( valid_id(fe_b) )
       set_facet_body(new_fb,b_id);

    /* Tension, if phases */
    if ( phase_flag )
    { set_f_phase_density(new_fa);
      if ( valid_id(fe_b) )
        set_f_phase_density(new_fb); 
    }

    /* set velocity, so can tell edge is supposed to grow */
    { REAL *velocity;
      velocity = get_velocity(old_v);
      for ( i = 0 ; i < SDIM ; i++ )
         velocity[i] = -new_displacement[i];
      velocity = get_velocity(new_v);
      for ( i = 0 ; i < SDIM ; i++ )
         velocity[i] = new_displacement[i];
    }
  } /* end if septum_flag */
  else
  { /* no septum, so just reset facetedge links */
    
    facetedge_id fe_a_next = get_next_facet(fe_a);
    facetedge_id fe_aa_prev = get_prev_facet(fe_aa);
    if ( !equal_id(fe_a_next,fe_aa) )
    {
      set_next_facet(fe_a,fe_aa);
      set_prev_facet(fe_aa,fe_a);
      set_next_facet(fe_aa_prev,fe_a_next);
      set_prev_facet(fe_a_next,fe_aa_prev);
      set_fe_edge(fe_a_next,new_a);
      set_fe_edge(fe_aa_prev,new_a);
      set_edge_fe(old_a,fe_a);
      set_edge_fe(new_a,fe_a_next);
    }
    set_edge_headv(old_a,new_v);

  
    if ( valid_id(fe_b) )
    { facetedge_id fe_b_next = get_next_facet(fe_b);
      facetedge_id fe_bb_prev = get_prev_facet(fe_bb);
      set_next_facet(fe_b,fe_bb);
      set_prev_facet(fe_bb,fe_b);
      set_next_facet(fe_bb_prev,fe_b_next);
      set_prev_facet(fe_b_next,fe_bb_prev);
      set_fe_edge(fe_b_next,new_b);
      set_fe_edge(fe_bb_prev,new_b);
      set_edge_tailv(old_b,new_v);
      set_edge_fe(old_b,fe_b);
      set_edge_fe(new_b,fe_b_next);
    }
  }
}

/***************************************************************************
*
* function: two_split()
*
* purpose: Handle special case of two facets meeting at an edge on a wall,
*          with two degrees of freedom on the wall so splitting apart
*          is legal.
*
* input: facetedge of edge known to have 2 facets and 2 degrees of freedom.
*
* return:  Number of edges split.
*/

int two_split(key_fe,septum_flag)
facetedge_id key_fe;  /* starting edge */
int septum_flag; /* no septum not implemented yet */
{

  facetedge_id headsplitlist[10];
  int headsplitcount = 0;
  int headsplitflag = 0; /* whether to split end vertex */
  facetedge_id tailsplitlist[10];
  int tailsplitcount = 0;
  int tailsplitflag = 0; /* 7hether to split end vertex */
  facetedge_id fe_a,fe_aa,fe_aaa;
  int stopflag;
  int popped = 0;
  int splitcount;
  int i;

  /* See how far potential split can be traced in each direction,
     checking that Y split is not more suitable all the way.
   */

  /* Try forward */
  fe_a = key_fe;
  headsplitflag = 1;
  for (stopflag=0;!stopflag;)
  { int head_degfree;
    int aa_degfree;

    headsplitlist[headsplitcount++] = fe_a;

    /* test head vertex */
    head_degfree = vertex_degfree(get_fe_headv(fe_a));
    if ( head_degfree == 0 )
    { /* can't split */
      headsplitflag = 0;
      break;
    }
  
    /* find next wall edge forward */
    fe_aaa = inverse_id(fe_a);
    do 
    { 
      fe_aa = inverse_id(get_next_edge(fe_aaa));
      fe_aaa = get_next_facet(fe_aa);
      if ( !equal_id(fe_aa,get_next_facet(fe_aaa))  )
      { /* run into valence more than 2, can't split head */
        stopflag = 1;
        headsplitflag = 0;
        break;
      }
      if ( equal_id(fe_aa,fe_aaa) )
      { /* valence 1, could still split head */
        aa_degfree = edge_degfree(get_fe_edge(fe_aa));
        if ( aa_degfree < 2 ) headsplitflag = 0;
        else headsplitflag = 1;
        stopflag = 1;
        break;
      }
      /* now have valence 2 */
      aa_degfree = edge_degfree(get_fe_edge(fe_aa));
      if ( aa_degfree == 2 )
      { /* back to wall */
        if ( (facet_degfree(get_fe_facet(fe_aa)) != 3)
             || (facet_degfree(get_fe_facet(get_next_facet(fe_aa))) != 3) )
           stopflag = 1;
        break;
      }
      else if ( aa_degfree != 3 )
      { /* end of the line  */
        stopflag = 1;
      }
      
    } while ( aa_degfree == 3 );
  
    fe_a = fe_aa;
  } /* end of going forward */

  /* go backward */
  fe_a = inverse_id(key_fe);
  tailsplitflag = 1;
  for (stopflag=0;!stopflag;)
  { int tail_degfree;
    int aa_degfree;

    tailsplitlist[tailsplitcount++] = fe_a;

    /* test tail vertex */
    tail_degfree = vertex_degfree(get_fe_headv(fe_a));
    if ( tail_degfree == 0 )
    { /* can't split */
      tailsplitflag = 0;
      break;
    }
  
    /* find next wall edge forward */
    fe_aaa = inverse_id(fe_a);
    do 
    { fe_aa = inverse_id(get_next_edge(fe_aaa));
      fe_aaa = get_next_facet(fe_aa);
      if ( !equal_id(fe_aa,get_next_facet(fe_aaa))  )
      { /* run into valence more than 2, can't split tail */
        stopflag = 1;
        tailsplitflag = 0;
        break;
      }
      if ( equal_id(fe_aa,fe_aaa) )
      { /* valence 1, could still split tail */
        aa_degfree = edge_degfree(get_fe_edge(fe_aa));
        if ( aa_degfree < 2 ) tailsplitflag = 0;
        else tailsplitflag = 1;
        stopflag = 1;
        break;
      }
      /* now have valence 2 */
      aa_degfree = edge_degfree(get_fe_edge(fe_aa));
      if ( aa_degfree == 2 )
      { /* back to wall */
        if ( (facet_degfree(get_fe_facet(fe_aa)) != 3)
             || (facet_degfree(get_fe_facet(get_next_facet(fe_aa))) != 3) )
           stopflag = 1;
        break;
      }
      else if ( aa_degfree != 3 )
      { /* end of the line  */
        stopflag = 1;
      }
      
    } while ( aa_degfree == 3 );
  
    fe_a = fe_aa;
  } /* end of going backward */

  /* split things */

  splitcount = headsplitcount + tailsplitcount - 1;
  if ( (splitcount == 1) && !headsplitflag && !tailsplitflag )
    return 0;

  /* now get into one list for splitting */
  for ( i = headsplitcount-1 ; i >= 0 ; i-- )
   headsplitlist[i+tailsplitcount-1] = headsplitlist[i];
  for ( i = 1 ; i < tailsplitcount ; i++ )
   headsplitlist[tailsplitcount-i] = inverse_id(tailsplitlist[i]);

  /* Test to see if all edges have their facets less than 90 degrees
     apart, in which case we don't split, but let Y pull-out happen. 
   */
  for ( i = 0 ; i < splitcount ; i++ )
  { facetedge_id fe = headsplitlist[i];
    facetedge_id fe_a = get_next_facet(fe);
    REAL normal[MAXCOORD];
    REAL normala[MAXCOORD];
    get_facet_normal(get_fe_facet(fe),normal);
    get_facet_normal(get_fe_facet(fe_a),normala);
    if ( SDIM_dot(normal,normala) < 0 )
      break;  /* too wide */
  }
  if ( i == splitcount ) return 0;
  

  /* split new edge off with facetedges in list */
  for ( i = 0 ; i < splitcount ; i++ )
  { facetedge_id fe,fe_b,fe_c;
    edge_id new_e,old_e;
 
    fe = headsplitlist[i];
    old_e = get_fe_edge(fe);
    new_e = dup_edge(old_e);
    if ( verbose_flag )
    { sprintf(msg,"Popping valence 2 edge %s on a constraint.\n",ELNAME(old_e));
      outstring(msg);
    }
    insert_vertex_edge(get_edge_tailv(old_e),new_e);
    insert_vertex_edge(get_edge_headv(old_e),inverse_id(new_e));
    fe_a = get_next_facet(fe);
    set_fe_edge(fe,new_e);
    set_edge_fe(new_e,fe);
    set_edge_fe(old_e,fe_a);
    set_next_facet(fe,fe);
    set_prev_facet(fe,fe);
    set_next_facet(fe_a,fe_a);
    set_prev_facet(fe_a,fe_a);


    if ( (i > 0) || tailsplitflag )
    { vertex_id old_tail = get_edge_tailv(old_e);
      vertex_id new_tail = dup_vertex(old_tail);
      fe_b = fe;
      fe_c = NULLID;
 
      /* reconnect edges */
      for (;;)
      {
        remove_vertex_edge(old_tail,get_fe_edge(fe_b));
        set_edge_tailv(get_fe_edge(fe_b),new_tail);
        if ( equal_id(fe_b,fe_c) ) break;

        fe_c = inverse_id(get_prev_edge(fe_b));
        fe_b = get_next_facet(fe_c);

      }
    }

    if ( (i == splitcount-1) && headsplitflag )
    { vertex_id old_head = get_edge_headv(old_e);
      vertex_id new_head = dup_vertex(old_head);
      fe_b = fe;
      fe_c = NULLID;
 
      /* reconnect edges */
      for (;;)
      {
        remove_vertex_edge(old_head,inverse_id(get_fe_edge(fe_b)));
        set_edge_headv(get_fe_edge(fe_b),new_head);
        if ( equal_id(fe_b,fe_c) ) break;

        fe_c = inverse_id(get_next_edge(fe_b));
        fe_b = get_next_facet(fe_c);

      }
    }
  }

  popped = splitcount;

  return popped;
}  

/**********************************************************************

     Vertex popping.

*************************************************************************/

#define OTHERCONE      0
#define WIREBOUNDARY  1
#define PLANECONE      2
#define WIREWING        3
#define WIRECORNER     4
#define TRIPLE_EDGER  5
#define TETRAHEDRAL    6
#define KRAYNIKCONE    7
#define CUBECONE      8
#define ODD4CONE      9
static  facetedge_id *felist;  /* for cell-ordered list */


/*************************************************************************
*
*  Function: verpop_film()
*
*  Purpose:  Pop vertices with non-minimal tangent cones.  Assumes
*                all edges are at most triple edges.
*/

int verpop_film()
{
  int popcount = 0;

  while ( find_vertex_to_pop() )
  { popcount++;
  }

  return popcount;
}

/************************************************************************
*
*  function: find_vertex_to_pop
*
*  purpose:  Looks through all vertices and finds one to pop.  Whole
*                process restarts after each pop since popping makes
*                search lists obsolete.
*
*                Returns number popped (0 or 1)
*/

int find_vertex_to_pop()
{
  struct verfacet *vflist;
  facetedge_id fe;
  facet_id f_id;
  int count,maxcount;
  int spot,dups,conetype;
  int popcount = 0;  


  /* Allocate list, with room for each facet thrice */
  maxcount = 3*web.skel[FACET].count;
  vflist = (struct verfacet *)temp_calloc(sizeof(struct verfacet),maxcount);  

  /* create unsorted list */
  count = 0;
  FOR_ALL_FACETS(f_id)
  {
    vflist[count].f_id = f_id;
    fe = get_facet_fe(f_id);
    vflist[count].v_id = get_fe_tailv(fe);
    count++;
    fe = get_next_edge(fe);    
    vflist[count].f_id = f_id;
    vflist[count].v_id = get_fe_tailv(fe);
    count++;
    fe = get_next_edge(fe);    
    vflist[count].f_id = f_id;
    vflist[count].v_id = get_fe_tailv(fe);
    count++;
    if ( count > maxcount )
    {
      kb_error(1300,
   "Internal error: Not enough structures allocated for vertex-facet list.\n",
            RECOVERABLE);
      return 0;
    }
  } 

  /* sort by vertex order */
  qsort((char *)vflist,count,sizeof(struct verfacet),FCAST vfcomp);

  /* go through list and pop appropriate vertices */
  for ( spot = 0 ; spot < count ; spot += dups )
  {
    vertex_id v_id = vflist[spot].v_id;

    /* find how many successive duplicates of current vertex */
    for ( dups = 1 ; spot+dups < count ; dups++ )
      if ( !equal_id(vflist[spot+dups].v_id,v_id) ) break;

    if ( get_vattr(v_id) & FIXED ) continue;

    conetype = cone_analyze(vflist + spot,dups);
    if ( conetype == KRAYNIKCONE ) 
      popcount += kraynik_pop(v_id,dups);
    else if ( conetype == CUBECONE ) 
      popcount += cubecone_pop(v_id,dups);
    else if ( conetype == ODD4CONE )
      popcount += odd4cone_pop(v_id,dups);
    else if ( conetype == OTHERCONE )
      popcount += pop_vertex(v_id,dups);
    temp_free((char *)felist);
    if ( popcount ) break;  /* one vertex at a time */
  }
             
  temp_free((char *)vflist);
  return popcount;
}

/**************************************************************************
*
* function: pop_given_vertex()
*
* purpose: pop vertex specified by user.
*
*/

int pop_given_vertex(v_id)
vertex_id v_id;
{
  struct verfacet *vflist;
  facetedge_id fe,start_fe,loop_fe;
  facet_id f_id;
  int count,maxcount;
  int spot,dups,conetype;
  int popcount = 0;  
  int con_hits;

  if ( !valid_id(get_vertex_edge(v_id)) )
  { if ( !(get_vattr(v_id) & FIXED) )
    { if ( verbose_flag )
      { sprintf(msg,"Dissolving bare vertex %s\n",ELNAME(v_id));
        outstring(msg);
      }
      free_element(v_id); 
      return 1;
    }
  }
  
  if ( web.representation == STRING ) return pop_string_vertex(v_id);

  con_hits = v_hit_constraint_count(v_id);
  if ( con_hits ) 
    return pop_constrained_vertex(v_id);
   

  /* Allocate list, with room for each facet thrice */
  maxcount = 3*web.skel[FACET].count;
  vflist = (struct verfacet *)temp_calloc(sizeof(struct verfacet),maxcount);  

  /* create unsorted list */
  count = 0;
  start_fe = get_vertex_first_facet(v_id);
  loop_fe = start_fe;
  if ( !valid_id(loop_fe) )
    return 0;
  do
  { f_id = get_fe_facet(loop_fe);
    vflist[count].f_id = f_id;
    fe = get_facet_fe(f_id);
    vflist[count].v_id = get_fe_tailv(fe);
    count++;
    fe = get_next_edge(fe);    
    vflist[count].f_id = f_id;
    vflist[count].v_id = get_fe_tailv(fe);
    count++;
    fe = get_next_edge(fe);    
    vflist[count].f_id = f_id;
    vflist[count].v_id = get_fe_tailv(fe);
    count++;
    if ( count > maxcount )
    {
      kb_error(2526,
   "Internal error: Not enough structures allocated for vertex-facet list.\n",
            RECOVERABLE);
      return 0;
    }
    loop_fe = get_next_vertex_facet(v_id,loop_fe);
  } while ( !equal_element(loop_fe,start_fe));

  /* sort by vertex order */
  qsort((char *)vflist,count,sizeof(struct verfacet),FCAST vfcomp);

  /* go through list and pop appropriate vertices */
  for ( spot = 0 ; spot < count ; spot += dups )
  {
    if ( v_id != vflist[spot].v_id ) { dups = 1; continue; }

    /* find how many successive duplicates of current vertex */
    for ( dups = 1 ; spot+dups < count ; dups++ )
      if ( !equal_id(vflist[spot+dups].v_id,v_id) ) break;

    if ( get_vattr(v_id) & FIXED ) continue;

    conetype = cone_analyze(vflist + spot,dups);
    if ( conetype == KRAYNIKCONE ) 
      popcount += kraynik_pop(v_id,dups);
    else if ( conetype == CUBECONE ) 
      popcount += cubecone_pop(v_id,dups);
    else if ( conetype == ODD4CONE )
      popcount += odd4cone_pop(v_id,dups);
    else if ( conetype == OTHERCONE )
      popcount += pop_vertex(v_id,dups);
    temp_free((char *)felist);
    break;  /* one vertex at a time */
  }
             
  temp_free((char *)vflist);
  return popcount;
}

/**************************************************************************
*
*  function: figure_type()
*
*  purpose: figure out what type of tangent cone a vertex has.
*
*/

#define CELLMAX 300
#define ARCMAX  600

static struct cell { int start;     /* arc list start in arclist */
                  int festart;  /* starting place in felist */
                  int num;        /* number of arcs                */
                  int fenum;     /* number of facetedges */
                  REAL area;     /* external angle deficit     */
                  body_id b_id; /* which body, if any */
                } cell[CELLMAX];
static struct arc  { int start;     /* edge list start in felist */
                  int num;        /* number of edges              */
                  int valence;  /* number of arcs into head node */
/*                int headtype; */  /* type of head node */
                } arclist[ARCMAX];
static  int cells;

int figure_type ARGS((int));

int figure_type(arcs)
int arcs;
{ int k,type;

  /* Classifying cone */
  switch ( cells )
     {
        case 1: /* wire boundary */
                  type = WIREBOUNDARY;
                  break;

        case 2: switch ( arcs )
                    {
                      case 2: /* internal plane */
                         type = PLANECONE;
                         break;
                      case 4: /* two wings on wire boundary */
                         type = WIREWING;  /* minimality depends on angle */
                         break;
                      default: /* n boundary wires meet at center */
                         type = WIRECORNER;
                         break;
                    }
                  break;

        case 3: switch ( arcs ) 
                    {
                      case 6: /* triple edge */
                         type = TRIPLE_EDGER;
                         for ( k = 0 ; k < cells ; k++ )
                            if ( cell[k].num != 2 ) type = OTHERCONE;
                         break;
                      default:
                         type = OTHERCONE;
                         break;
                    }
                  break;

        case 4: switch ( arcs )
                    { 
                      case 12: /* maybe tetrahedral cone */
                      { int twos=0,threes=0,fours=0;
                         for ( k = 0 ; k < cells ; k++ )
                         { switch ( cell[k].num )
                           { case 2: twos++; break;
                             case 3: threes++; break;
                             case 4: fours++; break;
                           }
                         }
                         if ( threes == 4 )
                           type = TETRAHEDRAL;
                         else if ( twos==2 && fours==2 )
                           type = ODD4CONE;
                         else type = OTHERCONE;
                       }    
                       break;
                      default:
                         type = OTHERCONE;
                         break;
                    }
                  break;

        case 5: if ( !kraynikpopvertex_flag || (arcs != 18) ) 
                { type = OTHERCONE; break; }
                type = KRAYNIKCONE;
                for ( k = 0 ; k < cells ; k++ )
                  if ( (cell[k].num < 3) || (cell[k].num > 4) )
                    { type = OTHERCONE; break; }
                break;

         case 6: type = CUBECONE;
                 if ( arcs != 24 )
                 { type = OTHERCONE; break; }
                 for ( k = 0 ; k < cells ; k++ )
                  if ( cell[k].num != 4 )
                    { type = OTHERCONE; break; }              
                 break;

         default:
                    type = OTHERCONE;
                    break;
      }
  return type;
}

/***************************************************************************
*
* function: cone_analyze()
*
* purpose: construct tangent cone structure around vertex and 
*          return cone type from figure_type().
*
*/

int cone_analyze(vf,count)
struct verfacet *vf;
int count;
{
  int type;  /* final classification */
  facetedge_id fe,nextfe,ray,nextray;
  vertex_id v_id = vf->v_id;
  int cellstart,arcstart,cellsides;
  int arclen,valence;
  int arcs;
  int k,j;
  int nodeflag;  /* whether any interesting node has been found for cell */

  /* First, set up network structure of edges opposite central vertex */
  felist = (facetedge_id *)temp_calloc(sizeof(facetedge_id),2*count);

  /* fill with outside edges of facets */
  for ( k = 0 ; k < count ; k++ )
  {
    fe = get_facet_fe(vf[k].f_id);
    if ( equal_id(v_id,get_fe_tailv(fe)) )
      fe = get_next_edge(fe);
    else if ( equal_id(v_id,get_fe_headv(fe)) )
      fe = get_prev_edge(fe);
    felist[2*k] = fe;
    felist[2*k+1] = inverse_id(fe);
  }

  /* order into arcs and cell boundaries */
  k = 0;        /* current edge number */
  cells = 0;    /* current cell number */
  arcs = 0;     /* current arc number */
  while ( k < 2*count )
  {
CELLSTART:
    /* starting new cell */
    if ( k >= 2*count ) break;
    if ( cells >= CELLMAX )
    { sprintf(errmsg,"pop: Too many cells around vertex %s in cone_analyze().\n",
         ELNAME(v_id));
      kb_error(1301,errmsg, RECOVERABLE);
    }

    nodeflag = 0;  /* whether interesting node found on cell */
    cellstart = k;
    cellsides = 0;
    cell[cells].start = arcs;

ARCSTART:
    /* starting new arc */
    if ( arcs >= ARCMAX )
    { sprintf(errmsg,"pop: Too many arcs around vertex %s in cone_analyze().\n",
         ELNAME(v_id));
      kb_error(1302,errmsg, RECOVERABLE);
    }

    arcstart = k;
    arclist[arcs].start = arcstart;
ARCRESTART:
    arclen = 0;

    for (k = arcstart; k < 2*count ;k++)
    {
      /* starting next edge */
      arclen++;
      fe = felist[k];
      nextray = ray = get_next_edge(fe);
      for ( valence = 1 ; ; valence++ )
      { nextray = get_next_facet(nextray);
        if ( equal_id(nextray,ray) ) break;
      }
      nextray = get_next_facet(ray);
      nextfe = get_next_edge(inverse_id(nextray));
      if ( equal_id(nextfe,felist[cellstart]) )  /* have gone around */
      {
        arclist[arcs].num = arclen;
        arclist[arcs].valence = valence;
        arcs++;
        cellsides++;
        cell[cells].num = cellsides;
        cells++;
        k++;
        goto CELLSTART;
      }

      /* linear search list until nextfe found */
      for ( j = k+1 ; j < 2*count ; j++ )
        if ( equal_id(nextfe,felist[j]) ) break;
      if ( !equal_id(nextfe,felist[j]) ) 
      { sprintf(errmsg,
       "Internal error: cone_analyze vertex %s: can't find nextfe in felist.\n",
          ELNAME(v_id));
        kb_error(1303,errmsg,RECOVERABLE);
      }
      if ( j > k+1 ) /* swap into place */
      {
        felist[j] = felist[k+1];
        felist[k+1] = nextfe;
      }

      /* see if node between is interesting */
      if ( (valence != 2))
      { /* have interesting node */
        if ( nodeflag == 0 ) 
        { /* first interesting node for cell */
          /* reset felist so cell and arc start are same */
          felist[k] = felist[arcstart];
          felist[arcstart] = nextfe;
          felist[k+1] = fe;
          nodeflag = 1;
          goto ARCRESTART;
        }
        arclist[arcs].num = arclen;
        arclist[arcs].valence = valence;
        arcs++;
        cellsides++;
        k++;
        goto ARCSTART;
      }
    }
  }

  type = figure_type(arcs);  /* so stupid SUN can optimize */

  return  type;
}


/*******************************************************************
*
*  Function:  pop_vertex()
*
*  Purpose:    pops one non-minimal vertex using info found by
*                 cone_analyze().  Finds areas of each face.  All
*                 face cones are pulled out to sphere except largest
*                 area.  Special treatment for face with disjoint
*                 boundary.
*/

int pop_vertex(v_id,count)
vertex_id v_id;
int count;  /* number of entries in vflist */
{
  int i,j,k,m;
  struct arc *ar;
  int fenum;
  facetedge_id fe;
  REAL cosdef;
  REAL prevnormal[MAXCOORD],prevnorm;
  REAL thisnormal[MAXCOORD],thisnorm;
  REAL maxarea = 0.0;
  int  bigcell=0;
  REAL total_area = 0.0;
  REAL total_angle,angle;
  REAL side[MAXCOORD],ray[MAXCOORD];  
  REAL *vx,newx[MAXCOORD];
  edge_id ray_e;
  facetedge_id ray_fe;
  vertex_id new_v;
  conmap_t *old_conmap;
  int old_bdrynum = -1;
  int samecell2, othercell2;
  body_id bs1,bs2,bo1,bo2;
  REAL totside1length,totside2length;
  int flipflag = 0; /* if cones switched in widecones */
  
  if ( verbose_flag )
  { sprintf(msg,"Popping vertex %s\n",ELNAME(v_id));
    outstring(msg);
  }

  /* fix up cell structures */
  for ( i = 0 ; i < cells ; i++ )
  { cell[i].fenum = 0;
    ar = arclist + cell[i].start;
    cell[i].festart = ar->start;
    for ( j = 0 ; j < cell[i].num ; j++, ar++ )
      cell[i].fenum += ar->num;
  }

  /* check for special configuration of touching disjoint cones
     that may want to be merged rather than split
  */
  if ( (cells == 4) && (cell[0].num == 1) && (cell[1].num == 1)
          && (cell[2].num == 1) && (cell[3].num == 1) )
  { REAL cosa,bestcosa; 
    facetedge_id bestray1=NULLID, bestray2=NULLID,fe_1,fe_2;
    int samecell,othercell;
    REAL area1,area2,normal1[MAXCOORD],normal2[MAXCOORD];
    REAL totnorm1[MAXCOORD],totnorm2[MAXCOORD];
    REAL axis1[MAXCOORD], axis2[MAXCOORD];
    REAL side1[MAXCOORD], side2[MAXCOORD];
    facet_id f_1,f_2,f_septum=NULLID;
    REAL ratio;
    int middleflag = 0; /* whether to put in septum */
    facetedge_id first_fe3=NULLID, prev_fe3, newfe3;
    body_id b_1, b_2;
 
    /* first, see which cells are really the same cones */
    samecell = 0;
    for ( i = 1 ; i < cells ; i++ )
    { for ( j = 0 ; j < cell[0].fenum ; j++ )
       for ( k = 0 ; k < cell[i].fenum ; k++ )
        if (equal_element(felist[cell[0].festart+j],felist[cell[i].festart+k])) 
        { samecell = i;
          break;
        }
    }
    if ( samecell == 0 )
    { sprintf(errmsg,"Vertex %s looks like double cones but isn't.\n",
          ELNAME(v_id));
      kb_error(2890,errmsg,RECOVERABLE);
    }
    othercell = (samecell == 1) ? 2 : 1;


    /* get axis and see if cones wide enough to pop this way */
    area1 = area2 = 0.0;
    memset((char*)axis1,0,sizeof(axis1));
    memset((char*)axis2,0,sizeof(axis2));
    memset((char*)totnorm1,0,sizeof(totnorm1));
    memset((char*)totnorm2,0,sizeof(totnorm2));
    totside1length = totside2length = 0.0;
    for ( i = 0 ; i < cell[samecell].fenum ; i++ )
    { fe_1 = felist[cell[samecell].festart+i];
      f_1 = get_fe_facet(fe_1);
      get_facet_normal(f_1,normal1);
      area1 += sqrt(dot(normal1,normal1,SDIM));
      for ( j = 0 ; j < SDIM ; j++ )
        totnorm1[j] += normal1[j];
      get_fe_side(get_prev_edge(fe_1),side1);
      for ( j = 0 ; j < SDIM ; j++ )
        axis1[j] += side1[j];
      totside1length += sqrt(dot(side1,side1,SDIM));
    }
    for ( i = 0 ; i < cell[othercell].fenum ; i++ )
    { fe_2 = felist[cell[othercell].festart+i];
      f_2 = get_fe_facet(fe_2);
      get_facet_normal(f_2,normal2);
      area2 += sqrt(dot(normal2,normal2,SDIM));
      for ( j = 0 ; j < SDIM ; j++ )
        totnorm2[j] += normal2[j];
      get_fe_side(get_prev_edge(fe_2),side2);
      for ( j = 0 ; j < SDIM ; j++ )
        axis2[j] += side2[j];
      totside2length += sqrt(dot(side2,side2,SDIM));
       }

    if ( pop_disjoin_flag ) goto narrowcones;
    if ( pop_enjoin_flag ) goto widecones;
    
    /* test wideness */
    ratio = (sqrt(dot(totnorm1,totnorm1,SDIM))+sqrt(dot(totnorm2,totnorm2,SDIM)))
             /(area1 + area2);
    if ( ratio < (middleflag?0.94:0.88) )
      goto narrowcones;
    
widecones: 
    /* choose cones to get facet normals going same way */
    for ( j = 1 ; j < cells ; j++ )
        if ( (j != samecell) && (j != othercell) ) break;
      othercell2 = j;
    samecell2 = 0;

    /*
    if ( dot(axis2,totnorm2,SDIM) < 0 )
    { int tmp = othercell;
      othercell = othercell2;  
      othercell2 = tmp;
    }
    if ( dot(axis1,totnorm1,SDIM) > 0 ) 
    { samecell2 = samecell;
      samecell = 0;
    }
    */
    if ( dot(totnorm1,totnorm2,SDIM) < 0 )
    { int tmp = othercell;
      othercell = othercell2;  
      othercell2 = tmp;
      for ( i = 0 ; i < SDIM ; i++ )
      { axis2[i] *= -1;
        totnorm2[i] *= -1;
      }
    }
    if ( dot(axis1,totnorm1,SDIM)/totside1length >
            dot(axis2,totnorm1,SDIM)/totside2length )
    { int tmp = othercell; othercell = samecell; samecell = tmp;
      tmp = samecell2; samecell2 = othercell2; othercell2 = tmp;
    }

    bs1 = get_facet_body(get_fe_facet(felist[cell[samecell].festart]));
    bs2 = get_facet_body(get_fe_facet(felist[cell[samecell2].festart]));
    bo1 = get_facet_body(get_fe_facet(felist[cell[othercell].festart]));
    bo2 = get_facet_body(get_fe_facet(felist[cell[othercell2].festart]));
    if ( !equal_id(bs2,bo1) )
    { /* have to flip sides */
      int tmp;
      body_id btmp;
      if ( !equal_id(bs1,bo2) )
      { /* no matching sides.  Shouldn't happen. */
        return 0;
      }
      tmp = othercell; othercell = othercell2; othercell2 = tmp;
      btmp = bs1; bs1 = bs2; bs2 = btmp;
      tmp = samecell; samecell = samecell2; samecell2 = tmp;
      btmp = bo1; bo1 = bo2; bo2 = btmp;
    }
      
    /* test bodies to see if we want septum */
    fe_1 = felist[cell[samecell].festart];
    f_1  = get_fe_facet(fe_1);
    fe_2 = felist[cell[othercell].festart];
    f_2  = get_fe_facet(fe_2);
    b_1 = get_facet_body(f_1);
    b_2 = get_facet_body(inverse_id(f_2));
    if ( !valid_id(b_1) || !valid_id(b_2) || !equal_id(b_1,b_2) )
    { middleflag = 1;
      f_septum = dup_facet(f_1);
      set_facet_body(f_septum,b_1);
      set_facet_body(inverse_id(f_septum),b_2);
    }


    /* now find closest rays on opposite cells */
    bestcosa = -1.0;
    for ( i = 0 ; i < cell[samecell].fenum ; i++ )
    { REAL edge1[MAXCOORD],edge2[MAXCOORD];
      facetedge_id ray1 = get_prev_edge(felist[cell[samecell].festart+i]);
      get_fe_side(ray1,edge1);
      for ( j = 0 ; j < cell[othercell].fenum ; j++ )
      { facetedge_id ray2 = get_prev_edge(felist[cell[othercell].festart+j]);
        get_fe_side(ray2,edge2);
        cosa = dot(edge1,edge2,SDIM)/sqrt(dot(edge1,edge1,SDIM)*
                     dot(edge2,edge2,SDIM));
        if ( cosa > bestcosa )
        { bestray1 = ray1; bestray2 = ray2; bestcosa = cosa; }
      }
    } 
    /* get lower number of sides in first cone */
    if ( cell[samecell].fenum > cell[othercell].fenum )
    { int tmp = samecell;
      facetedge_id fetmp = bestray1;
      samecell = othercell;
      othercell = tmp;
      bestray1 = bestray2;
      bestray2 = fetmp;
      flipflag = 1;
    }

    /* go around, inserting edges */
    fe_1 = bestray1;
    fe_2 = bestray2;
    prev_fe3 = NULLID;
    for ( j = 0 ; j < cell[samecell].fenum ; j++ )
    { edge_id e_1,e_1next,e_2next,newe;
      facetedge_id fe_1next,fe_2next,newfe1,newfe2;
      facet_id f_1,f_2;
      vertex_id newv;
      REAL *x;

      e_1 = get_fe_edge(fe_1);
      fe_1next = inverse_id(get_prev_edge(fe_1)); 
      e_1next = get_fe_edge(fe_1next);
      fe_2next = inverse_id(get_prev_edge(fe_2)); 
      e_2next = get_fe_edge(fe_2next);
      f_1 = get_fe_facet(fe_1next);
      f_2 = get_fe_facet(fe_2next);
      if ( j < cell[samecell].fenum - 1 )
      { newv = dup_vertex(v_id);
        remove_vertex_edge(v_id,e_1next);
        remove_vertex_edge(v_id,e_2next);
        set_edge_tailv(e_1next,newv);
        set_edge_tailv(e_2next,newv);
      }
      else newv = v_id;
      get_edge_side(e_1next,side1);
      get_edge_side(e_2next,side2);
      x = get_coord(newv);
      for ( k = 0 ; k < SDIM ; k++ )
        x[k] += 0.25*(side1[k]+side2[k]);
      newe = new_edge(get_edge_tailv(e_1),newv,e_1);
      newfe1 = new_facetedge(f_1,newe); 
      newfe2 = new_facetedge(f_2,newe); 
      set_edge_fe(newe,newfe1);
      set_prev_facet(newfe1,newfe2);
      set_next_facet(newfe2,newfe1);
      set_prev_facet(newfe2,newfe1);
      set_next_facet(newfe1,newfe2);

      if ( middleflag )
      { newfe3 = new_facetedge(f_septum,newe);
        if ( flipflag )
        { 
          set_prev_facet(newfe3,newfe2);
          set_next_facet(newfe2,newfe3);
          set_prev_facet(newfe1,newfe3);
          set_next_facet(newfe3,newfe1);
        }
        else
        {
          set_prev_facet(newfe3,newfe1);
          set_next_facet(newfe1,newfe3);
          set_prev_facet(newfe2,newfe3);
          set_next_facet(newfe3,newfe2);
        }
        if ( valid_id(prev_fe3) )
        { set_next_edge(prev_fe3,newfe3);
          set_prev_edge(newfe3,prev_fe3);
          prev_fe3 = newfe3;
        }
        else
        { first_fe3 = newfe3;
          set_facet_fe(f_septum,first_fe3);
          prev_fe3 = newfe3;
        }
      }
      set_next_edge(newfe1,fe_1next);
      set_prev_edge(fe_1next,newfe1);
      set_prev_edge(newfe1,inverse_id(fe_1));
      set_next_edge(inverse_id(fe_1),newfe1); 
      set_next_edge(newfe2,fe_2next);
      set_prev_edge(fe_2next,newfe2);
      set_prev_edge(newfe2,inverse_id(fe_2));
      set_next_edge(inverse_id(fe_2),newfe2); 
      cross_cut(newfe1,fe_1next);
      cross_cut(newfe2,fe_2next);

      fe_1 = get_next_facet(fe_1next);
      fe_2 = get_next_facet(fe_2next);
    }
    if ( middleflag )
    { set_next_edge(prev_fe3,first_fe3);
      set_prev_edge(first_fe3,prev_fe3);
      if ( cell[samecell].fenum > 3 )
        face_triangulate(f_septum,cell[samecell].fenum);
    }
    return 1;

    narrowcones: ;  /* bailout if not poppable this way */
  }

  /* see if any cells are totally detachable */
  for ( i = 0 ; i < cells ; i++ )
  { vertex_id newv;
    ar = arclist + cell[i].start;
    if ( (cell[i].num != 1) || (ar->valence != 2) ) continue;
    /* now have one */
    newv = dup_vertex(v_id);
    for ( j = 0 ; j < ar->num ; j++ )
    { fe = felist[ar->start+j];
      ray_e = get_fe_edge(get_next_edge(fe));
      remove_vertex_edge(v_id,inverse_id(ray_e));
      set_edge_headv(ray_e,newv);
    }
    return 1;    /* safe to do only one at a time */
  }

  /* calculate areas of each cell */
  for ( i = 0 ; i < cells ; i++ )
  {
    total_angle = 0.0;
    fenum = cell[i].festart;
    fe = felist[fenum+cell[i].fenum-1];
    cell[i].b_id = get_facet_body(get_fe_facet(inverse_id(fe)));
    get_fe_side(fe,side);
    get_fe_side(get_prev_edge(fe),ray);
    cross_prod(ray,side,prevnormal);
    prevnorm = sqrt(SDIM_dot(prevnormal,prevnormal));
    for ( k = 0 ; k < cell[i].fenum ; k++,fenum++ )
    { 
      fe = felist[fenum];
      get_fe_side(fe,side);
      get_fe_side(get_prev_edge(fe),ray);
      cross_prod(ray,side,thisnormal);
      thisnorm = sqrt(SDIM_dot(thisnormal,thisnormal));
      cosdef = SDIM_dot(prevnormal,thisnormal)/prevnorm/thisnorm;
      if ( cosdef > 1.0 ) angle = 0.0;
      else if ( cosdef < -1.0 ) angle = M_PI;
      else angle = acos(cosdef);
      if ( SDIM_dot(side,prevnormal) > 0.0 )
         total_angle += angle; 
      else
         total_angle -= angle;   /* in case cell not convex */

      /* set up for next loop */
      prevnorm = thisnorm;
      memcpy((char *)prevnormal,(char *)thisnormal,sizeof(prevnormal));
    }

    cell[i].area = 2*M_PI - total_angle;
    total_area += cell[i].area;
  }                  

  /* kludge to adjust for 2*pi ambiguity in turning angle that can
     affect badly shaped cells */
  while ( total_area > 4*M_PI + 1 )
  { int maxi = -1; 
    REAL maxa = -1e30;
    for ( i = 0 ; i < cells ; i++ )
     if ( cell[i].area > maxa )
     { maxi = i; maxa = cell[i].area; }
    cell[maxi].area -= 2*M_PI;
    total_area -= 2*M_PI;
  }
  while ( total_area < 4*M_PI - 1)
  { int mini = -1; 
    REAL mina = 1e30;
    for ( i = 0 ; i < cells ; i++ )
     if ( cell[i].area < mina )
     { mini = i; mina = cell[i].area; }
    cell[mini].area += 2*M_PI;
    total_area += 2*M_PI;
  }

  if ( total_area < 4*M_PI - 0.00001 )
  { sprintf(errmsg,
      "Solid angle deficit %g found around vertex %s. Not popped.\n",
      (double)(4*M_PI-total_area), ELNAME(v_id));
     kb_error(2176,errmsg,WARNING);
     return 0;
  }
  if ( total_area > 4*M_PI + 0.00001 )
  { sprintf(errmsg,
     "Solid angle excess %g found around vertex %s. Not popped.\n",
      (double)(total_area-4*M_PI), ELNAME(v_id));
     kb_error(2177,errmsg,WARNING);
     return 0;
  }
  
  /* see which cell is the largest */
  for ( i = 0 ; i < cells ; i++ )
    if ( cell[i].area > maxarea ) 
    { bigcell = i; maxarea = cell[i].area; }

  /* Now go around covering over all cells but bigcell or cells
      bigger than hemisphere */

  /* First, put new endpoint on all edges coming into v_id */
  vx = get_coord(v_id);
  old_conmap = get_v_constraint_map(v_id);
  if ( get_vattr(v_id) & BOUNDARY )
  { old_bdrynum = get_vertex_boundary_num(v_id);
  }
  for ( j = 0 ; j < 2*count ; j++ )
  { REAL rayvec[MAXCOORD];
    fe = felist[j];
    ray_fe = get_prev_edge(fe);
    if ( !equal_id(v_id,get_fe_tailv(ray_fe)) ) continue; /* done this */
    ray_e = get_fe_edge(ray_fe);
    get_edge_side(ray_e,rayvec);
    for ( m = 0 ; m < SDIM ; m++ )
      newx[m] = 0.5*rayvec[m] + vx[m];
    new_v = new_vertex(newx,ray_e);
    remove_vertex_edge(v_id,ray_e);
    set_edge_tailv(ray_e,new_v);
    if ( old_bdrynum >= 0 )
      set_vertex_boundary_num(new_v,old_bdrynum);
    else
      set_v_conmap(new_v,old_conmap);
  }

  /*  divide up old facets */
  for ( j = 0 ; j < 2*count ; j++ )
  { facetedge_id new_fe,new_fe_next,new_fe_prev;
    facetedge_id inray_fe;
    vertex_id v1,v2;
    edge_id new_e;
 
    fe = felist[j];
    ray_fe = get_prev_edge(fe);
    inray_fe = get_next_edge(fe);
    v1 = get_fe_tailv(ray_fe);
    v2 = get_fe_headv(inray_fe);

    /*  Each facet gets covered twice. First time we put in edge;
        second time triangulate the quadrilateral.  This way we
        can substitute the new inner fe for the old outer fe
        in felist.
     */
    if ( equal_id(inray_fe,get_prev_edge(ray_fe)) )
    { /* first time around */
      
      /* put in new edge */
      new_e = new_edge(v2,v1,get_fe_facet(fe));
      new_fe = new_facetedge(get_fe_facet(fe),new_e);
      set_edge_fe(new_e,new_fe);
      set_next_edge(inray_fe,new_fe);
      set_prev_edge(new_fe,inray_fe);
      set_next_edge(new_fe,ray_fe);
      set_prev_edge(ray_fe,new_fe);
      if ( old_bdrynum >= 0 )
        set_edge_boundary_num(new_e,old_bdrynum);
      else
        set_e_conmap(new_e,old_conmap);

      /* stub fe's for cells on each side */
      new_fe_prev = new_facetedge(NULLFACET,new_e);
      new_fe_next = new_facetedge(NULLFACET,new_e);
      set_next_facet(new_fe,new_fe_next);
      set_next_facet(new_fe_next,new_fe_prev);
      set_next_facet(new_fe_prev,new_fe);
      set_prev_facet(new_fe,new_fe_prev);
      set_prev_facet(new_fe_prev,new_fe_next);
      set_prev_facet(new_fe_next,new_fe);
      
      felist[j] = inverse_id(new_fe_next);    /* new inner fe */
    }
    else
    { felist[j] = inverse_id(get_next_facet(get_next_edge(inray_fe)));
      divide_quad(fe);
    }
  }
          
        
  /* now connect up insides of cells */
  for ( i = 0 ; i < cells ; i++ )
  {
    facet_id new_f=0,old_f;
    facetedge_id prev_fe=0;  /* last in edge chain */

    fenum = cell[i].festart;
    if ( (i != bigcell) && (cell[i].area < 2*M_PI) )
    { prev_fe = felist[fenum+cell[i].fenum-1];
      old_f = facet_inverse(get_fe_facet(get_next_facet(prev_fe)));
      new_f = dup_facet(old_f);
      set_facet_body(new_f,cell[bigcell].b_id);
      set_facet_body(inverse_id(new_f),cell[i].b_id);
      set_facet_fe(new_f,prev_fe);
    }

    for ( k = 0 ; k < cell[i].fenum ; k++,fenum++ )
    { 
      fe = felist[fenum];
      if ( (i == bigcell) || (cell[i].area >= 2*M_PI) )
      { /* have to excise fe */
        set_prev_facet(get_next_facet(fe),get_prev_facet(fe));
        set_next_facet(get_prev_facet(fe),get_next_facet(fe));
        free_element(fe);
      }
      else
      { /* hook into edge loop around facet */
        set_prev_edge(fe,prev_fe);
        set_next_edge(prev_fe,fe);
        set_fe_facet(fe,new_f);
        prev_fe = fe;
      }
    }

    /* if necessary, triangulate new cell facet */
    if ( (i != bigcell) && (cell[i].area < 2*M_PI) ) 
    { if ( cell[i].fenum >= 5 )
         face_triangulate(new_f,cell[i].fenum);
      else if ( cell[i].fenum == 4 )
      divide_quad(get_facet_fe(new_f));
    } 
 }


  /* free original central vertex */
  free_element(v_id);

  return 1;
}

/*******************************************************************
*
*  Function:  kraynik_pop()
*
*  Purpose:    pops one non-minimal vertex of type KRAYNIKCONE.
*/

int kraynik_pop(v_id,count)
vertex_id v_id;
int count;  /* number of entries in vflist */
{
  int i,j,k,m,kk;
  struct arc *ar;
  facetedge_id fe;
  vertex_id newv;
  edge_id newe;
  edge_id head_triples[3],tail_triples[3];
  REAL tailside[MAXCOORD],headside[MAXCOORD];
  REAL tailforce[MAXCOORD],headforce[MAXCOORD];
  REAL mag;

  /* find the 3-arc cells */
  for ( k = 0 ; k < cells ; k++ )
     if ( cell[k].num == 3 ) break;
  for ( kk = k+1 ; kk < cells ; kk++ )
     if ( cell[kk].num == 3 ) break;
  if ( kk == cells )
    return 0; /* failure */

  for ( i = 0 ; i < SDIM ; i++ )
   tailforce[i] = headforce[i] = 0.0;

  /* get triple lines and vectors to see which way to pop */
  for ( m = 0 ; m < 3 ; m++ )
  { ar = arclist + cell[k].start + m;
    fe = felist[ar->start];
    tail_triples[m] = get_fe_edge(get_prev_edge(fe));
    get_edge_side(tail_triples[m],tailside);
    mag = sqrt(dot(tailside,tailside,SDIM));
    if ( mag != 0 )
     for ( i = 0 ; i < SDIM ; i++ )
      tailforce[i] += tailside[i]/mag;
    ar = arclist + cell[kk].start + m;
    fe = felist[ar->start];
    head_triples[m] = get_fe_edge(get_prev_edge(fe));
    get_edge_side(head_triples[m],headside);
    mag = sqrt(dot(headside,headside,SDIM));
    if ( mag != 0 )
     for ( i = 0 ; i < SDIM ; i++ )
      headforce[i] += headside[i]/mag;
      
  }
  if ( pop_to_face_flag || ( !pop_to_edge_flag &&
        (dot(headforce,headforce,SDIM)+dot(tailforce,tailforce,SDIM) < 3)) )
    return pop_vertex_to_tri(v_id,tail_triples);

  newv = dup_vertex(v_id);
  newe = new_edge(newv,v_id,NULLID);

  /* go around the 3 arcs and reconfigure */
  for ( m = 0 ; m < 3 ; m++ )
  { facetedge_id ray_fe,other_ray_fe,third_ray_fe;
    facetedge_id newfe_a,newfe_b,newfe_c;
    edge_id ray_e,newray_e;
    facet_id newf;

    ar = arclist + cell[k].start + m;
    for ( j = 0 ; j < ar->num ; j++ )
    { fe = felist[ar->start+j];
      ray_e = get_fe_edge(get_next_edge(fe));
      remove_vertex_edge(v_id,inverse_id(ray_e));
      set_edge_headv(ray_e,newv);
    }
    /* last one for splitting triple edge */
    j = ar->num-1;
    fe = felist[ar->start+j];
    ray_fe = get_next_edge(fe);
    ray_e = get_fe_edge(ray_fe);
    newray_e = dup_edge(ray_e);
    set_edge_headv(newray_e,v_id);
    set_edge_tailv(newray_e,get_edge_tailv(ray_e));
    other_ray_fe = inverse_id(get_prev_edge(felist[(ar+(m==2?-2:1))->start]));
    third_ray_fe = get_next_facet(ray_fe);
    if ( equal_id(third_ray_fe,other_ray_fe) )
       third_ray_fe = get_prev_facet(ray_fe);

    newf = dup_facet(get_fe_facet(third_ray_fe));
    newfe_a = new_facetedge(newf,newe);
    newfe_b = new_facetedge(newf,inverse_id(newray_e));
    newfe_c = new_facetedge(newf,ray_e);
    set_facet_fe(newf,newfe_a);
    if ( m == 0 ) set_edge_fe(newe,newfe_a);
    set_next_edge(newfe_a,newfe_b);
    set_next_edge(newfe_b,newfe_c);
    set_next_edge(newfe_c,newfe_a);
    set_prev_edge(newfe_a,newfe_c);
    set_prev_edge(newfe_b,newfe_a);
    set_prev_edge(newfe_c,newfe_b);

    if (  equal_id(ray_fe,get_next_facet(other_ray_fe)) )
    {
      set_next_facet(ray_fe,newfe_c); 
      set_prev_facet(newfe_c,ray_fe); 
      set_prev_facet(other_ray_fe,newfe_c); 
      set_next_facet(newfe_c,other_ray_fe);  
    }
    else
    {
      set_prev_facet(ray_fe,newfe_c); 
      set_next_facet(newfe_c,ray_fe); 
      set_next_facet(other_ray_fe,newfe_c); 
      set_prev_facet(newfe_c,other_ray_fe);  
    }
    set_edge_fe(ray_e,ray_fe);
    set_edge_fe(newray_e,third_ray_fe);

    set_next_facet(third_ray_fe,inverse_id(newfe_b)); 
    set_prev_facet(third_ray_fe,inverse_id(newfe_b));  
    set_next_facet(inverse_id(newfe_b),third_ray_fe); 
    set_prev_facet(inverse_id(newfe_b),third_ray_fe);  

    set_fe_edge(third_ray_fe,newray_e);
    set_edge_fe(newray_e,third_ray_fe);
    if ( m == 1 )
    { set_next_facet(newfe_a,get_edge_fe(newe));
      set_prev_facet(newfe_a,get_edge_fe(newe));
      set_next_facet(get_edge_fe(newe),newfe_a);
      set_prev_facet(get_edge_fe(newe),newfe_a);
    }
    else if ( m == 2 ) 
    { facetedge_id fe1 = get_edge_fe(newe);
      facetedge_id fe2 = get_next_facet(fe1);

      set_next_facet(newfe_a,fe2);
      set_prev_facet(fe2,newfe_a);

      set_prev_facet(newfe_a,fe1);
      set_next_facet(fe1,newfe_a);
    }
  }

  /* move a bit to avoid zero area facets */
  new_vertex_average(newv,RAWEST);
  new_vertex_average(v_id,RAWEST);

  return 1;  /* success */
}


/*******************************************************************
*
*  Function:  cubecone_pop()
*
*  Purpose:    pops one non-minimal vertex of type CUBECONE.
*              Inserts central square in most favorable orientation.
*/

int cubecone_pop(v_id,count)
vertex_id v_id;
int count;  /* number of entries in vflist */
{
  int i,k,m;
  struct arc *ar;
  facetedge_id fe;
  REAL side[MAXCOORD];
  REAL faceforce[MAXCOORD];
  REAL facemag,bestmag,mag;
  int bestk=0;
  edge_id triples[6][4];

  /* find mean triple edge vectors for each face */


  /* get triple lines and vectors to see which way to pop */
  bestmag = 1e20;
  for ( k = 0 ; k < 6 ; k++ )
  { for ( i = 0 ; i < SDIM ; i++ )
      faceforce[i] = 0.0;
  
    for ( m = 0 ; m < 4 ; m++ )
    { ar = arclist + cell[k].start + m;
      fe = felist[ar->start];
      triples[k][m] = get_fe_edge(get_prev_edge(fe));
      get_edge_side( triples[k][m],side);
      mag = sqrt(dot(side,side,SDIM));
      if ( mag != 0 )
       for ( i = 0 ; i < SDIM ; i++ )
        faceforce[i] += side[i]/mag;
    }
    facemag = sqrt(dot(faceforce,faceforce,SDIM));
    if ( facemag < bestmag )
    { bestmag = facemag;
      bestk = k;
    }
  }

  return pop_vertex_to_quad(v_id,triples[bestk]);

}

/********************************************************************
*
* Function: facet_force_on_vertex()
*
* Purpose: Calculate force of a facet on one of its vertices,
*          force due to surface tension with density.  For
*          determining pop directions.  Note the vector in fvec
*          is the force, i.e. the negative of the area gradient.
*/
void facet_force_on_vertex(f_id,v_id,fvec)
facet_id f_id;
vertex_id v_id;
REAL *fvec; /* returned force */
{
  REAL sides[2][MAXCOORD];
  MAT2D(vx,FACET_VERTS,MAXCOORD);
  int i,j,basei=0;
  facetedge_id fe;
  REAL s1s1,s1s2,s2s2,det;
  REAL tension = get_facet_density(f_id);
  REAL coeff;

  get_facet_verts(f_id,vx,NULL);
  fe = get_facet_fe(f_id);
  for ( i = 0 ; i < FACET_EDGES ; i++ )
  {
    if ( equal_id(get_fe_tailv(fe),v_id) )
      basei = i;
    fe = get_next_edge(fe);
  }   
  for ( j = 0 ; j < SDIM ; j++ )
  { sides[0][j] = vx[1][j] - vx[0][j];
    sides[1][j] = vx[2][j] - vx[0][j];
  }
  s1s1 = dot(sides[0],sides[0],SDIM);  
  s2s2 = dot(sides[1],sides[1],SDIM);  
  s1s2 = dot(sides[0],sides[1],SDIM);  
  det = s1s1*s2s2 - s1s2*s1s2;
  if ( det <= 0.0 )
  { for ( j = 0 ; j < SDIM ; j++ )
      fvec[j] = 0.0;
  }
  coeff = tension/2/sqrt(det);
  if ( basei == 0 )
  { for ( j = 0 ; j < SDIM  ; j++ )
      fvec[j] = coeff*((s2s2*sides[0][j] - s1s2*sides[1][j])
               + (s1s1*sides[1][j] - s1s2*sides[0][j]));
  }
  else if ( basei == 1 )
  { for ( j = 0 ; j < SDIM  ; j++ )
      fvec[j] = -coeff*(s2s2*sides[0][j] - s1s2*sides[1][j]);
  }
  else
  { for ( j = 0 ; j < SDIM  ; j++ )
      fvec[j] = -coeff*(s1s1*sides[1][j] - s1s2*sides[0][j]);
  }
} /* end facet_force_on_vertex() */

/*******************************************************************
*
*  Function:  odd4cone_pop()
*
*  Purpose:    pops one non-minimal vertex of type ODD4CONE,
*              which is two triple lines touching at the vertex.
*              Can either detach the triple lines or split into
*              two tetrahedral points with septum between.
*/

int odd4cone_pop(v_id,count)
vertex_id v_id;
int count;  /* number of entries in vflist */
{
  int i,j,k;
  facetedge_id fe_a=NULLID,fe_b=NULLID; /* triple edge fe's on septum side*/
  facetedge_id fe_c=NULLID,fe_d=NULLID; /* other pair of triple edge fe's */
  edge_id e_a, e_b, e_c, e_d; /* triple edges */
  facet_id f_a,f_b,f_c, f_d; /* septum facets */
  REAL veca[MAXCOORD],vecb[MAXCOORD],vecc[MAXCOORD],vecd[MAXCOORD];
  REAL vecab[MAXCOORD],vecac[MAXCOORD],veccd[MAXCOORD],vecbd[MAXCOORD];
  int flag;
  int split_cell=0;
  int cdswap = 0;
  REAL tripnetforce; /* force towards separate triples; pos favorable */
  REAL quadnetforce; /* force towards quad line; pos favorable */
  facetedge_id next_fe,fe;
  REAL ee,ea,eb,ec,ed,aa,bb,cc,dd,bd,ac,acac,bdbd;
  REAL eside[MAXCOORD];
  int counter;
  
  /* identify key parts */
  /* find two triple lines on one side */
  for ( i = 0, flag = 0 ; i < cells ; i++ )
  { if ( cell[i].num == 2 )
    { if ( flag )
      { fe_c = felist[arclist[cell[i].start].start]; /* outer fe */
        fe_c = get_prev_edge(fe_c); /* radial fe */
        fe_c = get_prev_facet(fe_c); /* on septum */
        fe_d = felist[arclist[cell[i].start+1].start];
        fe_d = get_prev_edge(fe_d); /* radial fe */
        fe_d = get_prev_facet(fe_d); /* on septum */
      }
      else
      { fe_a = felist[arclist[cell[i].start].start]; /* outer fe */
        fe_a = get_prev_edge(fe_a); /* radial fe */
        fe_a = get_prev_facet(fe_a); /* on septum */
        fe_b = felist[arclist[cell[i].start+1].start];
        fe_b = get_prev_edge(fe_b); /* radial fe */
        fe_b = get_prev_facet(fe_b); /* on septum */
        split_cell = i;
        flag = 1;
      }     
    }
  }

  /* get a and c on same side of vertex */
  next_fe = fe_a;
  counter = 0;
  for(;;)
  { next_fe = inverse_id(get_prev_edge(next_fe));
    if ( equal_id(next_fe,fe_c) || equal_id(next_fe,fe_d)) 
      break;
    if ( !equal_id(get_next_facet(next_fe),get_prev_facet(next_fe)) || (++counter > 100) )
    { sprintf(errmsg,"Internal: Cannot match fe_c with fe_a at vertex %s; skipping pop.\n",
        ELNAME(v_id));
      kb_error(3692,errmsg,RECOVERABLE);
      return 0;
    } 
    next_fe = get_next_facet(next_fe);
  }
  if ( equal_id(next_fe,fe_d) )
  { /* swap c and d */
    facetedge_id temp = fe_c;
    fe_c = fe_d;
    fe_d = temp;
    cdswap = 1;
  }
  /* more parts */
  e_a = get_fe_edge(fe_a);
  e_b = get_fe_edge(fe_b);
  e_c = get_fe_edge(fe_c);
  e_d = get_fe_edge(fe_d);
  f_a = get_fe_facet(fe_a);
  f_b = get_fe_facet(fe_b);
  f_c = get_fe_facet(fe_c);
  f_d = get_fe_facet(fe_d);
  get_edge_side(e_a,veca);
  get_edge_side(e_b,vecb);
  get_edge_side(e_c,vecc);
  get_edge_side(e_d,vecd);
 
  for ( i = 0 ; i < SDIM ; i++ )
  { vecab[i] = (veca[i] + vecb[i])/4;
    veccd[i] = (vecc[i] + vecd[i])/4;
    vecac[i] = (veca[i] + vecc[i])/4;
    vecbd[i] = (vecb[i] + vecd[i])/4;
  }

  /* determine which way to split */
  /* First, if pull triple lines apart*/
  tripnetforce = 0.0;
  for ( i = 0, flag = 0 ; i < cells ; i++ )
  { /* forces from existing facets */
    if ( cell[i].num == 2 )
    { for ( k = 0 ; k < cell[i].num ; k++ )
      { struct arc *a = arclist + cell[i].start + k;
        for ( j = 0 ; j < a->num ; j++ )
        { facetedge_id fe = felist[a->start + j];
          REAL force[MAXCOORD];
          facet_force_on_vertex(get_fe_facet(fe),v_id,force);
          if ( flag )
            tripnetforce += dot(veccd,force,SDIM);
          else
            tripnetforce += dot(vecab,force,SDIM);
        }
      }
      flag = 1;
    } 
  }
  /* force from introduced facets */
  ee = dot(vecab,vecab,SDIM); 
  aa = dot(veca,veca,SDIM);
  ea = dot(veca,vecab,SDIM);
  tripnetforce -= sqrt(aa*ee - ea*ea)/2*get_facet_density(f_a);
  bb = dot(vecb,vecb,SDIM);
  eb = dot(vecb,vecab,SDIM);
  tripnetforce -= sqrt(bb*ee - eb*eb)/2*get_facet_density(f_b);
  ee = dot(veccd,veccd,SDIM); 
  cc = dot(vecc,vecc,SDIM);
  ec = dot(vecc,veccd,SDIM);
  tripnetforce -= sqrt(cc*ee - ec*ec)/2*get_facet_density(f_c);
  dd = dot(vecd,vecd,SDIM);
  ed = dot(vecd,veccd,SDIM);
  tripnetforce -= sqrt(dd*ee - ed*ed)/2*get_facet_density(f_d);
  

  /* now force from expanding vertex to quad line */
  /* assuming splits at the a->num/2 edge in the side arcs */
  quadnetforce = 0.0;

  for ( i = 0, flag = 0 ; i < cells ; i++ )
  { if ( cell[i].num == 2 ) 
    { for ( k = 0 ; k < cell[i].num ; k++ )
      { struct arc *a = arclist + cell[i].start + k;
        for ( j = 0 ; j < a->num ; j++ )
        { facetedge_id fe = felist[a->start + j];
          REAL force[MAXCOORD];
          facet_force_on_vertex(get_fe_facet(fe),v_id,force);
          if ( !flag || !cdswap )
          {
            if ( ((k==0) && (j < a->num/2)) || ((k==1)&&(j >= a->num/2)) )
              quadnetforce += dot(vecac,force,SDIM);
            else
              quadnetforce += dot(vecbd,force,SDIM);
          }
          else
          { if ( ((k==0) && (j < a->num/2)) || ((k==1)&&(j >= a->num/2)) )
              quadnetforce += dot(vecbd,force,SDIM);
            else
              quadnetforce += dot(vecac,force,SDIM);
          }
        
        }
      }
      flag = 1;
    } 
  }
  /* force from old septa */
  ac = dot(veca,vecc,SDIM);
  quadnetforce += sqrt(aa*cc-ac*ac)/4*get_facet_density(f_a);
  bd = dot(vecb,vecd,SDIM);
  quadnetforce += sqrt(bb*dd-bd*bd)/4*get_facet_density(f_b);

  /* force from introduced facets */
  for ( i = 0 ; i < cells ; i++ )
  { if ( cell[i].num == 2 ) 
    { for ( k = 0 ; k < cell[i].num ; k++ )
      { struct arc *a = arclist + cell[i].start + k;
        REAL tension;
        
        fe = felist[a->start + a->num/2];
        fe = get_prev_edge(fe);
        get_edge_side(get_fe_edge(fe),eside);
        ee = dot(eside,eside,SDIM); 
        tension = get_facet_density(get_fe_facet(fe));

        acac = dot(vecac,vecac,SDIM);
        ea = dot(vecac,eside,SDIM);
        quadnetforce -= sqrt(acac*ee - ea*ea)/2*tension;
        bdbd = dot(vecbd,vecbd,SDIM);
        eb = dot(vecbd,eside,SDIM);
        quadnetforce -= sqrt(bdbd*ee - eb*eb)/2*tension;
      }
    }
  }

  if ( (tripnetforce < 0) && (quadnetforce < 0) )
  { if ( verbose_flag )
    { sprintf(msg,"Not popping touching-triple-lines vertex %s since stable.\n",
        ELNAME(v_id));
      outstring(msg);
    }
    return 0; /* no profit from splitting either way */
  }
  
  /* if separate triple lines */
  if ( tripnetforce > quadnetforce )
  { vertex_id newv = dup_vertex(v_id);
    edge_id newe;
    facetedge_id fe_aa,fe_bb,new_fe_a,new_fe_b;
    REAL *xold = get_coord(v_id);
    REAL *xnew = get_coord(newv);

    if ( verbose_flag )
    { sprintf(msg,"Separating triple lines at vertex %s.\n",
        ELNAME(v_id));
      outstring(msg);
    }
    
    /* spread vertices apart */
    for ( i = 0 ; i < SDIM ; i++ )
    { 
      xnew[i] += vecab[i];
      xold[i] += veccd[i];
    }

    /* reconnected other edges on split-off part */
    for ( i = 0 ; i < cell[split_cell].num ; i++ )
    { struct arc *a = arclist + cell[split_cell].start + i;
      for ( j = 0 ; j < a->num ; j++ )
      { facetedge_id fe = felist[a->start + j];
        edge_id e_id;
        fe = get_prev_edge(fe);
        e_id = get_fe_edge(fe);
        set_edge_tailv(e_id,newv);
      }
    }

    /* new edge and its facetedges */
    newe = new_edge(v_id,newv,v_id);
    new_fe_a = new_facetedge(f_a,newe);
    new_fe_b = new_facetedge(f_b,newe);
    set_edge_fe(newe,new_fe_a);
    set_next_facet(new_fe_a,new_fe_b);
    set_prev_facet(new_fe_a,new_fe_b);
    set_next_facet(new_fe_b,new_fe_a);
    set_prev_facet(new_fe_b,new_fe_a);
    fe_aa = get_prev_edge(fe_a);
    set_next_edge(new_fe_a,fe_a);
    set_prev_edge(fe_a,new_fe_a);
    set_prev_edge(new_fe_a,fe_aa);
    set_next_edge(fe_aa,new_fe_a);
    fe_bb = get_prev_edge(fe_b);
    set_next_edge(new_fe_b,fe_b);
    set_prev_edge(fe_b,new_fe_b);
    set_prev_edge(new_fe_b,fe_bb);
    set_next_edge(fe_bb,new_fe_b);

    /* subdivide septum quadrilaterals */
    cross_cut(new_fe_a,fe_a);
    cross_cut(new_fe_b,fe_b);

  }
  else
  { /* split vertex to form quadruple line */
    vertex_id newv = dup_vertex(v_id);
    edge_id newe;
    REAL *xold = get_coord(v_id);
    REAL *xnew = get_coord(newv);

    if ( verbose_flag )
    { sprintf(msg,"Inserting new septum at vertex %s.\n",
        ELNAME(v_id));
      outstring(msg);
    }
   
    /* spread vertices apart */
    for ( i = 0 ; i < SDIM ; i++ )
    { 
      xnew[i] += vecac[i];
      xold[i] += vecbd[i];
    }

    /* new edge and its facetedges */
    newe = new_edge(v_id,newv,v_id);

    /* reconnected other edges on split-off part */
    set_edge_tailv(e_a,newv);
    set_edge_tailv(e_c,newv);

    for ( i = 0, flag = 0 ; i < cells ; i++ )
    { if ( cell[i].num != 2 ) 
        continue;
      for ( k = 0 ; k < cell[i].num ; k++ )
      { struct arc *a = arclist + cell[i].start + k;
        int upflag = (!flag && (k==0)) || (flag && !cdswap && (k==0))
            || (flag && cdswap && (k==1)) ;
        facet_id ff;
        facetedge_id new_fe,fe_prev,e_fe;
        
        for ( j = 1 ; j < a->num ; j++ )
        { facetedge_id fe = felist[a->start + j];
          edge_id e_id;
          fe = get_prev_edge(fe);
          e_id = get_fe_edge(fe);
          if ( (upflag && (j <= a->num/2)) || (!upflag && (j >= a->num/2)) )
            set_edge_tailv(e_id,newv);
        }
        /* now expand some facets to quads */
        fe = felist[a->start + a->num/2];
        fe = get_prev_edge(fe);
        if ( !upflag )
          fe = get_next_facet(fe);
        ff = get_fe_facet(fe);
        new_fe = new_facetedge(ff,newe);
        fe_prev = get_prev_edge(fe);
        set_prev_edge(new_fe,fe_prev);
        set_next_edge(fe_prev,new_fe);
        set_next_edge(new_fe,fe);
        set_prev_edge(fe,new_fe);
        e_fe = get_edge_fe(newe);
        if ( valid_id(e_fe) )
        { fe_prev = get_prev_facet(e_fe);
          set_next_facet(fe_prev,new_fe);
          set_prev_facet(new_fe,fe_prev);
          set_prev_facet(e_fe,new_fe);
          set_next_facet(new_fe,e_fe);
        }
        else
        { set_edge_fe(newe,new_fe);
          set_next_facet(new_fe,new_fe);
          set_prev_facet(new_fe,new_fe);
        }
        cross_cut(new_fe,fe);
      } 
      flag = 1;
    }
    fe_reorder(newe);
    pop_one_edge(newe);
  }
  
  return 1;

} /* end odd4cone_pop() */

/*****************************************************************************
*
* function: pop_tri_to_edge()
*
* purpose: Topology change of small triangle to edge perpendicular to it.
*          Implements pop_tri_to_edge command.
*
* return: 1 for success, 0 for failure.
*/

int pop_tri_to_edge(f_id)
facet_id f_id;
{ edge_id ea,eb,ec,newe;
  vertex_id va,vb,vc,keepv=NULLID,newv;
  facetedge_id fe,next_fe,fa,fb,fc;
  int retval,i,j;
  int legs;
  REAL side[MAXCOORD],separation[MAXCOORD];
  edge_id trip[3];
  REAL *keepx,*newx;
  edge_id tredges[12];
  int ti;
 
  if ( (web.modeltype != LINEAR) && (web.modeltype != QUADRATIC) )
    kb_error(3366,"pop_tri_to_edge only for LINEAR or QUADRATIC models.\n",
       RECOVERABLE);

  /* Test geometry is appropriate */
  next_fe = fe = get_facet_fe(f_id);
  ti = 0;
  do
  { int triples;
    ea = get_fe_edge(next_fe);
    ec = inverse_id(get_fe_edge(get_prev_edge(next_fe)));
    if ( get_edge_valence(ea) != 3 ) 
      { if ( verbose_flag )
        { sprintf(msg,
          "pop_tri_to_edge fails on facet %s; not all its edges triple .\n",
             ELNAME(f_id));
          outstring(msg);
        }
        return 0;
      }
    va = get_fe_tailv(next_fe);
    eb = get_next_tail_edge(ea);
    triples = 1;
    while ( !equal_id(ea,eb) )
    { int val = get_edge_valence(eb);
      if ( (val < 2) || ( val > 3 ) ) 
      { if ( verbose_flag )
        { sprintf(msg,
    "pop_tri_to_edge fails on facet %s due to edge %s having valence %d.\n",
      ELNAME(f_id),ELNAME1(eb),val);
          outstring(msg);
        }
        return 0;
      }
      if ( (val == 3) && !equal_id(eb,ec) )
      { tredges[ti++] = eb;
        triples++;
      }
      eb = get_next_tail_edge(eb);
    }
    if ( triples != 3 ) 
      { if ( verbose_flag )
        { sprintf(msg,
            "pop_tri_to_edge fails on facet %s due to %d triple edges on vertex %s; need 4.\n",
               ELNAME(f_id),triples+1,ELNAME1(get_edge_tailv(eb)));
          outstring(msg);
        }
        return 0;
      }
    next_fe = get_next_edge(next_fe);
  } while ( !equal_id(fe,next_fe) );

  fe = get_facet_fe(f_id);
  ea = get_fe_edge(fe);
  eb = get_fe_edge(get_next_edge(fe));
  ec = get_fe_edge(get_prev_edge(fe));
  va = get_edge_tailv(ea);
  vb = get_edge_tailv(eb);
  vc = get_edge_tailv(ec);

  /* check for disjoint endpoints of triple edges */
  for ( i = 1 ; i < ti ; i++ )
    for ( j = 0 ; j < i ; j++ )
      if ( equal_id(get_edge_headv(tredges[i]),get_edge_headv(tredges[j])) )
      { if ( verbose_flag )
        { sprintf(msg,"pop_tri_to_edge fails on facet %s due to outer triple edges with common endpoint.\n",ELNAME(f_id));
          outstring(msg);
        }
        return 0;
      }

  if ( verbose_flag )
  { sprintf(msg,"pop_tri_to_edge on facet %s\n",ELNAME(f_id));
    outstring(msg);
  }

  retval = eliminate_facet(f_id);
  if ( retval == 0 ) return 0;

  /* now finish elimination by eliminating the remaining facet edge */
  if ( valid_element(ea) ) 
  { retval = eliminate_edge(ea); free_element(ea); }
  else if ( valid_element(eb) ) 
  { retval = eliminate_edge(eb); free_element(eb); }
  else if ( valid_element(ec) ) 
  { retval = eliminate_edge(ec); free_element(ec); }
  else retval = 0;
  if ( retval == 0 )
  { if ( verbose_flag )
    { sprintf(msg,"pop_tri_to_edge failed on facet %s; failed to delete edge remaining after deleting facet.\n",ELNAME(f_id));
      outstring(msg);
    }
    return 0;
  }

  if ( valid_element(va) ) keepv = va;
  else if ( valid_element(vb) ) keepv = vb;
  else if ( valid_element(vc) ) keepv = vc;

  /* Identify the three downward triple lines */
  trip[0] = ea = tredges[0]; trip[1] = trip[2] = NULLID;
  fe = get_edge_fe(ea);
  get_edge_side(ea,side);
  for ( j = 0 ; j < SDIM ; j++ ) 
     separation[j] = side[j];
  for ( i = 0 ; i < 3 ; i++,fe=get_next_facet(fe) )
  { next_fe = inverse_id(get_prev_edge(fe));
    while ( equal_id(get_next_facet(next_fe),get_prev_facet(next_fe)) )
      next_fe = inverse_id(get_prev_edge(get_next_facet(next_fe)));
    eb = get_fe_edge(next_fe);
    get_edge_side(eb,side);
    if ( equal_id(eb,tredges[2]) || equal_id(eb,tredges[3]) ) 
    { trip[1] = eb;
      for ( j = 0 ; j < SDIM ; j++ ) 
       separation[j] += side[j];
    }
    if ( equal_id(eb,tredges[4]) || equal_id(eb,tredges[5]) ) 
    { trip[2] = eb;
      for ( j = 0 ; j < SDIM ; j++ ) 
       separation[j] += side[j];
    }
  }
  if ( !valid_id(trip[1]) || !valid_id(trip[2]) )
  { if ( verbose_flag )
    { sprintf(msg,"pop_tri_to_edge failed after deleting facet %s; didn't find triple of edges in same direction.\n",ELNAME(f_id));
      outstring(msg);
    }
    return 0;
  }
  legs = 3;

  /* Insert new vertex and edge */
  newv = dup_vertex(keepv);
  newe = new_edge(keepv,newv,NULLID);
 
  keepx = get_coord(keepv);
  newx  = get_coord(newv);
  for ( j = 0 ; j < SDIM ; j++ )
  { newx[j] = keepx[j] + separation[j]/12;
    keepx[j] -= separation[j]/12;
  }

  /* Reconnect lower edge endpoints */
  for ( i = 0 ; i < legs ; i++ )  /* lower triple edges */
  { facetedge_id next_fe,start_fe;
    remove_vertex_edge(keepv,trip[i]);
    set_edge_tailv(trip[i],newv);
    fe = start_fe = get_edge_fe(trip[i]);
    do  /* around the multiple edge */
    {
      next_fe = inverse_id(get_prev_edge(fe));
      for (;;)
      { /* traverse fan of edges */
        edge_id eg;
        if ( !equal_id(get_fe_tailv(next_fe),keepv) ) break;
        if ( get_next_facet(next_fe) != get_prev_facet(next_fe) )
        { /* have reached next triple edge */
          eg = get_fe_edge(next_fe);
          if ( equal_id(eg,trip[0]) || equal_id(eg,trip[1]) 
             || equal_id(eg,trip[2]) )
          { /* all lower edges, so go back and reconnect */
            next_fe = inverse_id(get_prev_edge(next_fe));
            while ( !equal_id(next_fe,fe) )
            { edge_id e_id = get_fe_edge(next_fe);
              remove_vertex_edge(keepv,e_id);
              set_edge_tailv(e_id,newv);
              next_fe = get_prev_facet(next_fe);
              next_fe = inverse_id(get_prev_edge(next_fe));
            }
          }
          else
          { /* vertical, so insert new edge above trip[i] */
            facetedge_id fea = get_prev_edge(fe);
            facet_id fa = get_fe_facet(fe);
            facetedge_id newfe = new_facetedge(fa,newe);
            facetedge_id newefe = get_edge_fe(newe);
            set_next_edge(fea,newfe);
            set_prev_edge(newfe,fea);
            set_prev_edge(fe,newfe);
            set_next_edge(newfe,fe);
            if ( valid_id(newefe) )
            { set_next_facet(newfe,newefe);
              set_prev_facet(newefe,newfe);
              set_prev_facet(newfe,get_next_facet(newefe));
              set_next_facet(get_next_facet(newefe),newfe);
            }
            else
            { set_edge_fe(newe,newfe);
              set_next_facet(newfe,newfe);
              set_prev_facet(newfe,newfe);
            }
            cross_cut(newfe,fe);  /* triangulate */
          }
          break;  /* done with this way out of triple edge */
        }
        next_fe = get_next_facet(next_fe);
        next_fe = inverse_id(get_prev_edge(next_fe));
      }
      fe = get_next_facet(fe); 
    } while ( !equal_id(fe,start_fe) );
  } 

  /* Fix up facet edge orders */
    fa = get_edge_fe(newe);
    fb = get_next_facet(fa);
    fc = get_prev_facet(fa);
    if ( equal_id(get_facet_body(get_fe_facet(fb)),
            get_facet_body(inverse_id(get_fe_facet(fc)))) )
    { /* have to switch order around edge */
      set_prev_facet(fa,fb);
      set_prev_facet(fb,fc);
      set_prev_facet(fc,fa);
      set_next_facet(fa,fc);
      set_next_facet(fc,fb);
      set_next_facet(fb,fa);
    }
  
  if ( web.modeltype == QUADRATIC )
  { /* fix up edge midpoints */
    vertex_id vv[2];
    int bailcount = 0;
    vv[0] = keepv; vv[1] = newv;
    /* fix up edge midpoints */
    for ( i = 0 ; i < 2 ; i++ )
    { edge_id start_e = get_vertex_edge(vv[i]);
      edge_id ea;
      ea = start_e;
      do
      { new_vertex_average(get_edge_midv(ea),VOLKEEP);
        ea = get_next_tail_edge(ea);
        if ( bailcount++ > 1000 ) break;
      } while ( !equal_id(ea,start_e));
    }
  }

  fe_reorder(newe);
 
  return 1;
}


/*****************************************************************************
*
* function: pop_edge_to_tri()
*
* purpose: Topology change of edge to small triangle perpendicular to it.
*          Implements pop_edge_to_tri command.
*
* return: 1 for success, 0 for failure.
*/

int pop_edge_to_tri(e_id)
edge_id e_id;
{ int triples;
  vertex_id tail_triples[3],head_triples[3];
  vertex_id headv,tailv;
  int retval;
  int i,j;
  conmap_t *hmap,*tmap;
  int thit=0,hhit=0;
  unsigned int k;

  if ( (web.modeltype != LINEAR) && (web.modeltype != QUADRATIC) )
    kb_error(2835,"pop_edge_to_tri only for LINEAR or QUADRATIC models.\n",
       RECOVERABLE);

  /* Check valences and gather info on triple edges */
  if ( get_edge_valence(e_id) != 3 ) 
  { if ( verbose_flag )
    { sprintf(msg,
        "pop_edge_to_tri fails on edge %s; it is not a triple edge.\n",
            ELNAME(e_id)+1);
      outstring(msg);
    }
    return 0;
  }

  tailv = get_edge_tailv(e_id);
  tmap = get_v_constraint_map(tailv);
  for ( k = 1 ; k <= tmap[0] ; k++ )
    if ( tmap[k] & CON_HIT_BIT )
    { thit = 1;
      break;
    }
  if ( !thit )
  { /* check for proper number of triple edges */
    edge_id eb = get_next_tail_edge(e_id);
    triples = 0;
    while ( !equal_id(e_id,eb) )
    { int val = get_edge_valence(eb);
      if ( (val < 2) || ( val > 3 ) )
      { if ( verbose_flag )
        { sprintf(msg,
         "pop_edge_to_tri fails on edge %s due to edge %s having valence %d.\n",
             ELNAME(e_id),ELNAME1(eb),val);
          outstring(msg);
        }
        return 0;
      }
      if ( val == 3 )
      { if ( triples >= 3 ) 
        { if ( verbose_flag )
          { sprintf(msg,
      "pop_edge_to_tri fails on edge %s due to too many triple edges at end.\n",
               ELNAME(e_id)+1);
            outstring(msg);
          }
          return 0;
        }
        tail_triples[triples++] = eb;
      }
      eb = get_next_tail_edge(eb);
    }
    if ( triples != 3 )
    { if ( verbose_flag )
      { sprintf(msg,
           "pop_edge_to_tri fails on edge %s due to not enough triple edges at an endpoint.\n",
              ELNAME(e_id));
        outstring(msg);
      }
      return 0;
    }
  }

  headv = get_edge_headv(e_id);
  hmap = get_v_constraint_map(headv);
  for ( k = 1 ; k <= hmap[0] ; k++ )
    if ( hmap[k] & CON_HIT_BIT )
    { hhit = 1;
      break;
    }
  if ( !hhit )
  { /* check for proper number of triples */
    edge_id eb = get_next_head_edge(e_id);
    triples = 0;
    while ( !equal_id(e_id,eb) )
    { int val = get_edge_valence(eb);
      if ( (val < 2) || ( val > 3 ) )
      { if ( verbose_flag )
        { sprintf(msg,
          "pop_edge_to_tri fails on edge %s due to edge %s having valence %d.\n",
             ELNAME(e_id),ELNAME1(eb),val);
          outstring(msg);
        }
        return 0;
      }
      if ( val == 3 )
      { if ( triples >= 3 ) 
        { if ( verbose_flag )
          { sprintf(msg,"pop_edge_to_tri fails on edge %s due to too many  triple edges at one endpoint.\n",ELNAME(e_id));
            outstring(msg);
          }
          return 0;
        }
        head_triples[triples++] = inverse_id(eb);
      }
      eb = get_next_head_edge(eb);
    }
    if ( triples != 3 ) 
    { if ( verbose_flag )
      { sprintf(msg,"pop_edge_to_tri fails on edge %s due to not enough triple edges at one endpoint.\n",ELNAME(e_id));
        outstring(msg);
      }
      return 0;
    }
  }

  if ( thit && hhit )
  { if ( verbose_flag )
    { sprintf(msg,"pop_edge_to_tri fails on edge %s due to both endpoints on constraints.\n",ELNAME(e_id));
      outstring(msg);
    }
    return 0;
  }
  if ( thit )
    return pop_edge_to_tri_con(e_id);
  if ( hhit )
    return pop_edge_to_tri_con(inverse_id(e_id));


  /* check for common endpoints on triples */
  for ( i = 0 ; i < 3 ; i++ )
    for ( j = 0 ; j < 3 ; j++ )
      if ( equal_id(get_edge_headv(tail_triples[i]),
                    get_edge_headv(head_triples[j]))  )
      { if ( verbose_flag )
        { sprintf(msg,"pop_edge_to_tri fails on edge %s due to outer triple edges with common endpoint.\n",ELNAME(e_id));
          outstring(msg);
        }
        return 0;
      }

  if ( verbose_flag )
  { sprintf(msg,"pop_edge_to_tri on edge %s\n",ELNAME(e_id));
    outstring(msg);
  }

  /* Delete the edge */
  retval = eliminate_edge(e_id);
  if ( retval == 0 ) return 0;
  free_element(e_id);

  return pop_vertex_to_tri((valid_element(headv) ? headv : tailv),tail_triples);
}

/**************************************************************************
*
* function: pop_vertex_to_tri()
*
* purpose: main work for pop_edge_to_tri and kraynik pop.
*/

int pop_vertex_to_tri(v_id,tail_triples)
vertex_id v_id;
edge_id *tail_triples;
{ int i,j;
  vertex_id newv[3],keepv;
  edge_id newe[3];
  facet_id newf;
  facetedge_id newfe[3],fe,fa,fb,fc;
  edge_id ea,eb,ec,ee,head_triples[3];
 
  /* Create new vertices, edges, and facet */
  newv[0] = v_id;
  newv[1] = dup_vertex(newv[0]);
  newv[2] = dup_vertex(newv[0]);
  newe[0] = new_edge(newv[0],newv[1],NULLID);
  newe[1] = new_edge(newv[1],newv[2],NULLID);
  newe[2] = new_edge(newv[2],newv[0],NULLID);
  fe = get_vertex_first_facet(newv[0]);
  newf    = dup_facet(get_fe_facet(fe));
  set_facet_body(newf,NULLID);
  set_facet_body(inverse_id(newf),NULLID);
  newfe[0] = new_facetedge(newf,newe[0]); set_edge_fe(newe[0],newfe[0]);
  newfe[1] = new_facetedge(newf,newe[1]); set_edge_fe(newe[1],newfe[1]);
  newfe[2] = new_facetedge(newf,newe[2]); set_edge_fe(newe[2],newfe[2]);
  set_next_edge(newfe[0],newfe[1]);
  set_next_edge(newfe[1],newfe[2]);
  set_next_edge(newfe[2],newfe[0]);
  set_prev_edge(newfe[0],newfe[2]);
  set_prev_edge(newfe[1],newfe[0]);
  set_prev_edge(newfe[2],newfe[1]);
  set_facet_fe(newf,newfe[0]);

  /* Reconnect things */
  keepv = newv[0];
  for ( i = 0 ; i < 3 ; i++ )  /* lower triple edges */
  { facetedge_id next_fe;
    remove_vertex_edge(keepv,tail_triples[i]);
    set_edge_tailv(tail_triples[i],newv[i]);
    fe = get_edge_fe(tail_triples[i]);
    for ( j = 0 ; j < 3 ; j++ )  /* around the triple edge */
    { fe = get_next_facet(fe);
      next_fe = inverse_id(get_prev_edge(fe));
      for (;;)
      { /* traverse fan of edges */
        vertex_id vv_id;
        edge_id eg,ee;
        eg = get_fe_edge(next_fe);
        if ( equal_element(eg,newe[0]) ) break;
        if ( equal_element(eg,newe[1]) ) break;
        if ( equal_element(eg,newe[2]) ) break;
        vv_id = get_edge_tailv(eg);
        if ( !equal_id(vv_id,keepv) && !equal_id(vv_id,newv[i]) )   
        { sprintf(errmsg,
           "pop_edge_to_tri bug: edge %s tail vertex is %s instead of %s.\n",
             ELNAME(eg),ELNAME1(get_edge_tailv(eg)),ELNAME2(keepv));
          kb_error(2834,errmsg,RECOVERABLE);
          break;
        }
        if ( get_next_facet(next_fe) != get_prev_facet(next_fe) )
        { /* have reached next triple edge */
          if ( equal_id(eg,tail_triples[(i+2)%3]) ) break;
          else if ( equal_id(eg,tail_triples[(i+1)%3]) )
          { /* all lower edges, so go back and split out to triangle edge */
            facetedge_id feb = inverse_id(get_prev_edge(fe));
            facetedge_id fea = get_prev_edge(fe);
            facet_id fa = get_fe_facet(fe);
            facetedge_id newfe = new_facetedge(fa,inverse_id(newe[i]));
            facetedge_id newefe = get_edge_fe(inverse_id(newe[i]));

            while ( !equal_id(feb,next_fe) )
            { ee = get_fe_edge(feb);
              remove_vertex_edge(keepv,ee);
              set_edge_tailv(ee,newv[(i+1)%3]);
              feb = get_next_facet(feb);
              feb = inverse_id(get_prev_edge(feb));
            }

            set_next_edge(fea,newfe);
            set_prev_edge(newfe,fea);
            set_prev_edge(fe,newfe);
            set_next_edge(newfe,fe);
            set_next_facet(newfe,newefe);
            set_prev_facet(newefe,newfe);
            set_next_facet(newefe,newfe);
            set_prev_facet(newfe,newefe);
            cross_cut(newfe,fe);  /* triangulate */
          }
          else /* hit one of the head_triples */
          { /* vertical wall, so reconnect */ 
            head_triples[i] = eg;
            next_fe = inverse_id(get_prev_edge(next_fe));
            while ( !equal_id(next_fe,fe) )
            { edge_id ee = get_fe_edge(next_fe);
              remove_vertex_edge(keepv,ee);
              set_edge_tailv(ee,newv[i]);
              next_fe = get_prev_facet(next_fe);
              next_fe = inverse_id(get_prev_edge(next_fe));
            }
          }
          break;  /* done with this way out of triple edge */
        }
        next_fe = get_next_facet(next_fe);
        next_fe = inverse_id(get_prev_edge(next_fe));
      }
    }
  } 

  /* Now go around head end and reconnect, head_triples having been
     put in proper correspondence above                                 */
  for ( i = 0 ; i < 3 ; i++ )  /* upper triple edges */
  { facetedge_id next_fe;
    remove_vertex_edge(keepv,head_triples[i]);
    set_edge_tailv(head_triples[i],newv[i]);
    fe = get_edge_fe(head_triples[i]);
    for ( j = 0 ; j < 3 ; j++ )  /* around the triple edge */
    { fe = get_next_facet(fe);
      next_fe = inverse_id(get_prev_edge(fe));
      for (;;)
      { /* traverse fan of edges */
        edge_id eg;
        if ( !equal_id(get_fe_tailv(next_fe),keepv) ) break;
        eg = get_fe_edge(next_fe);
        if ( get_next_facet(next_fe) != get_prev_facet(next_fe) )
        { /* have reached next triple edge */
          if ( equal_id(eg,head_triples[(i+2)%3]) ) break;
          if ( equal_element(eg,newe[0]) ) break;
          if ( equal_element(eg,newe[1]) ) break;
          if ( equal_element(eg,newe[2]) ) break;
          else if ( equal_id(eg,head_triples[(i+1)%3]) )
          { /* all upper edges, so go back and split out to triangle edge */
            facetedge_id feb = inverse_id(get_prev_edge(fe));
            facetedge_id fea = get_prev_edge(fe);
            facet_id fa = get_fe_facet(fe);
            facetedge_id newfe = new_facetedge(inverse_id(fa),newe[i]);
            facetedge_id newefe = get_edge_fe(newe[i]);

            while ( !equal_id(feb,next_fe) )
            { ee = get_fe_edge(feb);
              remove_vertex_edge(keepv,ee);
              set_edge_tailv(ee,newv[(i+1)%3]);
              feb = get_next_facet(feb);
              feb = inverse_id(get_prev_edge(feb));
            }

            set_next_edge(fea,inverse_id(newfe));
            set_prev_edge(inverse_id(newfe),fea);
            set_prev_edge(fe,inverse_id(newfe));
            set_next_edge(inverse_id(newfe),fe);
            set_next_facet(newfe,newefe);
            set_prev_facet(newefe,newfe);
            set_prev_facet(newfe,get_next_facet(newefe));
            set_next_facet(get_next_facet(newefe),newfe);

            cross_cut(inverse_id(newfe),fe);  /* triangulate */
          }
          else /* hit one of the head_triples */
          { /* vertical wall, but those already reconnected */ 
          }
          break;  /* done with this way out of triple edge */
        }
        next_fe = get_next_facet(next_fe);
        next_fe = inverse_id(get_prev_edge(next_fe));
      }
    }
  } 

  /* Spread new vertices */
  for ( i = 0 ; i < 3 ; i++ ) /* the vertices */
  { REAL *x,side1[MAXCOORD],side2[MAXCOORD];
    x = get_coord(newv[i]);
    get_edge_side(tail_triples[i],side1);
    get_edge_side(head_triples[i],side2);
    for ( j = 0 ; j < SDIM ; j++ )
      x[j] += (side1[j]+side2[j])/6;
  }

  /* Fix up facet edge orders */
  for ( i = 0 ; i < 3 ; i++ )
  { facetedge_id next_fe;
    int count;
    vertex_id base_v;
    fa = newfe[i];
    fb = get_next_facet(fa);
    fc = get_prev_facet(fa);
    ea = get_fe_edge(fa);
    eb = get_fe_edge(inverse_id(get_prev_edge(fa)));
    base_v = get_edge_tailv(ea);
    next_fe = inverse_id(get_prev_edge(fb));
    for(count=0;count < 10000 ;count++)
    { ec = get_fe_edge(next_fe);
      if ( !equal_id(get_edge_tailv(ec),base_v) )
        kb_error(2833,"pop_tri_to_edge failure.\n",RECOVERABLE);
      if ( equal_id(ec,ea) )
        break; /* ok */
      if ( equal_id(ec,eb) )
      { /* have to switch order around edge */
        set_prev_facet(fa,fb);
        set_prev_facet(fb,fc);
        set_prev_facet(fc,fa);
        set_next_facet(fa,fc);
        set_next_facet(fc,fb);
        set_next_facet(fb,fa);
        break;
      }
      next_fe = inverse_id(get_prev_edge(get_prev_facet(next_fe)));
    }
    if ( count >= 10000 ) 
    { sprintf(errmsg,"Internal error after pop_vertex_to_tri edge %s, bad topology around vertex %s.\n",ELNAME(v_id),ELNAME1(get_edge_tailv(ea)));
      kb_error(2832,errmsg,RECOVERABLE);
    }
  }
 

  /* Fix up bodies on new facet */
  fe = newfe[0];
  set_facet_body(newf,
     get_facet_body(inverse_id(get_fe_facet(get_prev_facet(fe)))));
  set_facet_body(inverse_id(newf),
     get_facet_body(get_fe_facet(get_next_facet(fe))));

  if ( web.modeltype == QUADRATIC )
  { /* fix up edge midpoints */
    int bailcount = 0;
    for ( i = 0 ; i < 3 ; i++ )
    { edge_id start_e = get_vertex_edge(newv[i]);
      edge_id ea;
      ea = start_e;
      do
      { new_vertex_average(get_edge_midv(ea),VOLKEEP);
        ea = get_next_tail_edge(ea);
        if ( bailcount++ > 1000 ) break;
      } while ( !equal_id(ea,start_e));
    }
  }

  return 1;

}  /* end pop_edge_to_tri */

/**************************************************************************
*
* function: pop_vertex_to_quad()
*
* purpose: main work for pop_cubecone.
*/

int pop_vertex_to_quad(v_id,tail_triples)
vertex_id v_id;
edge_id *tail_triples;
{ int i,j;
  vertex_id newv[4],keepv;
  edge_id newe[4];
  facet_id newf;
  facetedge_id newfe[4],fe,fa,fb,fc;
  edge_id ea,eb,ec,ee,head_triples[4];
 
  /* Create new vertices, edges, and facet */
  newv[0] = v_id;
  newv[1] = dup_vertex(newv[0]);
  newv[2] = dup_vertex(newv[0]);
  newv[3] = dup_vertex(newv[0]);
  newe[0] = new_edge(newv[0],newv[1],NULLID);
  newe[1] = new_edge(newv[1],newv[2],NULLID);
  newe[2] = new_edge(newv[2],newv[3],NULLID);
  newe[3] = new_edge(newv[3],newv[0],NULLID);
  fe = get_vertex_first_facet(newv[0]);
  newf    = dup_facet(get_fe_facet(fe));
  set_facet_body(newf,NULLID);
  set_facet_body(inverse_id(newf),NULLID);
  newfe[0] = new_facetedge(newf,newe[0]); set_edge_fe(newe[0],newfe[0]);
  newfe[1] = new_facetedge(newf,newe[1]); set_edge_fe(newe[1],newfe[1]);
  newfe[2] = new_facetedge(newf,newe[2]); set_edge_fe(newe[2],newfe[2]);
  newfe[3] = new_facetedge(newf,newe[3]); set_edge_fe(newe[3],newfe[3]);
  set_next_edge(newfe[0],newfe[1]);
  set_next_edge(newfe[1],newfe[2]);
  set_next_edge(newfe[2],newfe[3]);
  set_next_edge(newfe[3],newfe[0]);
  set_prev_edge(newfe[0],newfe[3]);
  set_prev_edge(newfe[1],newfe[0]);
  set_prev_edge(newfe[2],newfe[1]);
  set_prev_edge(newfe[3],newfe[2]);
  set_facet_fe(newf,newfe[0]);

  /* Reconnect things */
  keepv = newv[0];
  for ( i = 0 ; i < 4 ; i++ )  /* lower triple edges */
  { facetedge_id next_fe;
    remove_vertex_edge(keepv,tail_triples[i]);
    set_edge_tailv(tail_triples[i],newv[i]);
    fe = get_edge_fe(tail_triples[i]);
    for ( j = 0 ; j < 3 ; j++ )  /* around the triple edge */
    { fe = get_next_facet(fe);
      next_fe = inverse_id(get_prev_edge(fe));
      for (;;)
      { /* traverse fan of edges */
        vertex_id vv_id;
        edge_id eg,ee;
        eg = get_fe_edge(next_fe);
        if ( equal_element(eg,newe[0]) ) break;
        if ( equal_element(eg,newe[1]) ) break;
        if ( equal_element(eg,newe[2]) ) break;
        if ( equal_element(eg,newe[3]) ) break;
        vv_id = get_edge_tailv(eg);
        if ( !equal_id(vv_id,keepv) && !equal_id(vv_id,newv[i]) )   
        { sprintf(errmsg,
           "pop_vertex_to_quad bug: edge %s tail vertex is %s instead of %s.\n",
             ELNAME(eg),ELNAME1(get_edge_tailv(eg)),ELNAME2(keepv));
          kb_error(3930,errmsg,RECOVERABLE);
          break;
        }
        if ( get_next_facet(next_fe) != get_prev_facet(next_fe) )
        { /* have reached next triple edge */
          if ( equal_id(eg,tail_triples[(i+3)%4]) ) break;
          else if ( equal_id(eg,tail_triples[(i+1)%4]) )
          { /* all lower edges, so go back and split out to triangle edge */
            facetedge_id feb = inverse_id(get_prev_edge(fe));
            facetedge_id fea = get_prev_edge(fe);
            facet_id fa = get_fe_facet(fe);
            facetedge_id newfe = new_facetedge(fa,inverse_id(newe[i]));
            facetedge_id newefe = get_edge_fe(inverse_id(newe[i]));

            while ( !equal_id(feb,next_fe) )
            { ee = get_fe_edge(feb);
              remove_vertex_edge(keepv,ee);
              set_edge_tailv(ee,newv[(i+1)%4]);
              feb = get_next_facet(feb);
              feb = inverse_id(get_prev_edge(feb));
            }

            set_next_edge(fea,newfe);
            set_prev_edge(newfe,fea);
            set_prev_edge(fe,newfe);
            set_next_edge(newfe,fe);
            set_next_facet(newfe,newefe);
            set_prev_facet(newefe,newfe);
            set_next_facet(newefe,newfe);
            set_prev_facet(newfe,newefe);
            cross_cut(newfe,fe);  /* triangulate */
          }
          else /* hit one of the head_triples */
          { /* vertical wall, so reconnect */ 
            head_triples[i] = eg;
            next_fe = inverse_id(get_prev_edge(next_fe));
            while ( !equal_id(next_fe,fe) )
            { edge_id ee = get_fe_edge(next_fe);
              remove_vertex_edge(keepv,ee);
              set_edge_tailv(ee,newv[i]);
              next_fe = get_prev_facet(next_fe);
              next_fe = inverse_id(get_prev_edge(next_fe));
            }
          }
          break;  /* done with this way out of triple edge */
        }
        next_fe = get_next_facet(next_fe);
        next_fe = inverse_id(get_prev_edge(next_fe));
      }
    }
  } 

  /* Now go around head end and reconnect, head_triples having been
     put in proper correspondence above                                 */
  for ( i = 0 ; i < 4 ; i++ )  /* upper triple edges */
  { facetedge_id next_fe;
    remove_vertex_edge(keepv,head_triples[i]);
    set_edge_tailv(head_triples[i],newv[i]);
    fe = get_edge_fe(head_triples[i]);
    for ( j = 0 ; j < 3 ; j++ )  /* around the triple edge */
    { fe = get_next_facet(fe);
      next_fe = inverse_id(get_prev_edge(fe));
      for (;;)
      { /* traverse fan of edges */
        edge_id eg;
        if ( !equal_id(get_fe_tailv(next_fe),keepv) ) break;
        eg = get_fe_edge(next_fe);
        if ( get_next_facet(next_fe) != get_prev_facet(next_fe) )
        { /* have reached next triple edge */
          if ( equal_id(eg,head_triples[(i+3)%4]) ) break;
          if ( equal_element(eg,newe[0]) ) break;
          if ( equal_element(eg,newe[1]) ) break;
          if ( equal_element(eg,newe[2]) ) break;
          if ( equal_element(eg,newe[3]) ) break;
          else if ( equal_id(eg,head_triples[(i+1)%4]) )
          { /* all upper edges, so go back and split out to triangle edge */
            facetedge_id feb = inverse_id(get_prev_edge(fe));
            facetedge_id fea = get_prev_edge(fe);
            facet_id fa = get_fe_facet(fe);
            facetedge_id newfe = new_facetedge(inverse_id(fa),newe[i]);
            facetedge_id newefe = get_edge_fe(newe[i]);

            while ( !equal_id(feb,next_fe) )
            { ee = get_fe_edge(feb);
              remove_vertex_edge(keepv,ee);
              set_edge_tailv(ee,newv[(i+1)%4]);
              feb = get_next_facet(feb);
              feb = inverse_id(get_prev_edge(feb));
            }

            set_next_edge(fea,inverse_id(newfe));
            set_prev_edge(inverse_id(newfe),fea);
            set_prev_edge(fe,inverse_id(newfe));
            set_next_edge(inverse_id(newfe),fe);
            set_next_facet(newfe,newefe);
            set_prev_facet(newefe,newfe);
            set_prev_facet(newfe,get_next_facet(newefe));
            set_next_facet(get_next_facet(newefe),newfe);

            cross_cut(inverse_id(newfe),fe);  /* triangulate */
          }
          else /* hit one of the head_triples */
          { /* vertical wall, but those already reconnected */ 
          }
          break;  /* done with this way out of triple edge */
        }
        next_fe = get_next_facet(next_fe);
        next_fe = inverse_id(get_prev_edge(next_fe));
      }
    }
  } 

  /* Spread new vertices */
  for ( i = 0 ; i < 4 ; i++ ) /* the vertices */
  { REAL *x,side1[MAXCOORD],side2[MAXCOORD];
    x = get_coord(newv[i]);
    get_edge_side(tail_triples[i],side1);
    get_edge_side(head_triples[i],side2);
    for ( j = 0 ; j < SDIM ; j++ )
      x[j] += (side1[j]+side2[j])/6;
  }

  /* Fix up facet edge orders */
  for ( i = 0 ; i < 4 ; i++ )
  { facetedge_id next_fe;
    int count;
    vertex_id base_v;
    fa = newfe[i];
    fb = get_next_facet(fa);
    fc = get_prev_facet(fa);
    ea = get_fe_edge(fa);
    eb = get_fe_edge(inverse_id(get_prev_edge(fa)));
    base_v = get_edge_tailv(ea);
    next_fe = inverse_id(get_prev_edge(fb));
    for(count=0;count < 10000 ;count++)
    { ec = get_fe_edge(next_fe);
      if ( !equal_id(get_edge_tailv(ec),base_v) )
        kb_error(4833,"pop_vertex_to_quad failure.\n",RECOVERABLE);
      if ( equal_id(ec,ea) )
        break; /* ok */
      if ( equal_id(ec,eb) )
      { /* have to switch order around edge */
        set_prev_facet(fa,fb);
        set_prev_facet(fb,fc);
        set_prev_facet(fc,fa);
        set_next_facet(fa,fc);
        set_next_facet(fc,fb);
        set_next_facet(fb,fa);
        break;
      }
      next_fe = inverse_id(get_prev_edge(get_prev_facet(next_fe)));
    }
    if ( count >= 10000 ) 
    { sprintf(errmsg,"Internal error after pop_vertex_to_quad edge %s, bad topology around vertex %s.\n",ELNAME(v_id),ELNAME1(get_edge_tailv(ea)));
      kb_error(4832,errmsg,RECOVERABLE);
    }
  }
 

  /* Fix up bodies on new facet */
  fe = newfe[0];
  set_facet_body(newf,
     get_facet_body(inverse_id(get_fe_facet(get_prev_facet(fe)))));
  set_facet_body(inverse_id(newf),
     get_facet_body(get_fe_facet(get_next_facet(fe))));

  if ( web.modeltype == QUADRATIC )
  { /* fix up edge midpoints */
    int bailcount = 0;
    for ( i = 0 ; i < 4 ; i++ )
    { edge_id start_e = get_vertex_edge(newv[i]);
      edge_id ea;
      ea = start_e;
      do
      { new_vertex_average(get_edge_midv(ea),VOLKEEP);
        ea = get_next_tail_edge(ea);
        if ( bailcount++ > 1000 ) break;
      } while ( !equal_id(ea,start_e));
    }
  }

  /* divide central square */
  cross_cut(newfe[0],newfe[1]);

  return 1;

}  /* end pop_vertex_to_quad */


/*****************************************************************************
*
* function: pop_quad_to_quad()
*
* purpose: Topology change of narrow quadrilateral to quad perpendicular to it.
*          Implements pop_quad_to_quad command.
*
* return: 1 for success, 0 for failure.
*/

int pop_quad_to_quad(f_id)
facet_id f_id; /* one facet of the quadrilateral */
{ facetedge_id fe,next_fe,start_fe,quad_triples[4],newfe[4],fea,keepfe;
  edge_id e_id,ee_id,eee_id,other_triples[4][2],ea,eb,ec,keepe,newe[4];
  vertex_id newv[4];
  facet_id newf,fa,fb,fc;
  body_id b_id;
  int i,j,k,ii,jj;
  REAL len[4];
  REAL sides[4][2][MAXCOORD];
  REAL *x[4];
#define MAXQF 100
  facet_id quadfacets[MAXQF];
  int qfcount;

  if ( (web.modeltype != LINEAR) && (web.modeltype != QUADRATIC) )
    kb_error(2836,"pop_quad_to_quad only for LINEAR or QUADRATIC models.\n",
       RECOVERABLE);

  /* quad could be four facets or two, or more */

  /* Check geometry and gather info about triple edges */

  /* find triple edge on original facet */
  fe = get_facet_fe(f_id);
  for ( i = 0 ; i < 3 ; i++ )
  { edge_id e_id = get_fe_edge(fe);
    if ( get_edge_valence(e_id) == 3 ) break;
    fe = get_next_edge(fe);
  }
  if ( i == 3 )   /* couldn't find a triple edge */
  { if ( verbose_flag )
    { sprintf(msg,"pop_quad_to_quad fails on facet %s since it doesn't have a triple edge.\n",ELNAME(f_id));
      outstring(msg);
    }
    return 0;
  }

  /* march around inner quad */
  start_fe = fe;
  for ( i = 0 ; i < 4 ; i++ )
  {
    for(;;) /* seek next triple */ 
    { fe = inverse_id(get_prev_edge(fe));
      if ( equal_id(fe,get_next_facet(fe)) ) /* valence 1 edge */
      { if ( verbose_flag )
        { sprintf(msg,"pop_quad_to_quad fails on facet %s; edge %s has valence 1.\n",ELNAME(f_id),ELNAME1(get_fe_edge(fe)));
          outstring(msg);
        }
        return 0;
      }
      if ( !equal_id(get_next_facet(fe),get_prev_facet(fe)) )
        break;  /* found triple */
      fe = get_next_facet(fe);
    }
    /* now have next triple */
    e_id = get_fe_edge(fe);
    if ( get_edge_valence(e_id) != 3 ) 
    { if ( verbose_flag )
      { sprintf(msg,"pop_quad_to_quad fails on facet %s; edge %s valence too high.\n",ELNAME(f_id),ELNAME1(e_id));
        outstring(msg);
      }
      return 0;
    }
    quad_triples[i] = fe;

    fe = inverse_id(fe);   /* get ready for next triple line search */
  }
  if ( !equal_id(start_fe,fe) )
  { if ( verbose_flag )
    { sprintf(msg,"pop_quad_to_quad fails on facet %s; didn't find quadrilateral of triple edges.\n",ELNAME(f_id));
      outstring(msg);
    }
    return 0;
  }

  /* Find shorter pair of opposite edges, and swap if necessary
     so quad_triples[1,3] has short edges */
  for ( i = 0 ; i < 4 ; i++ ) 
  { e_id = get_fe_edge(quad_triples[i]);
    calc_edge(e_id);
    len[i] = get_edge_length(e_id);
  }
  if ( (len[0]+len[2]) < (len[1]+len[3]) )
  { facetedge_id temp_fe = quad_triples[0];
    quad_triples[0] = quad_triples[1]; 
    quad_triples[1] = quad_triples[2]; 
    quad_triples[2] = quad_triples[3]; 
    quad_triples[3] = temp_fe; 
  }

  /* Find the other triple lines at each corner */
  for ( i = 0 ; i < 4 ; i++ )
  { edge_id ee_id;
    int val;

    e_id = get_fe_edge(quad_triples[i]);
    ee_id = e_id;
    j = 0;
    do
    { ee_id = get_next_tail_edge(ee_id);
      val = get_edge_valence(ee_id);
      if ( (val < 2) || (val > 3) ) 
      { if ( verbose_flag )
        { sprintf(msg,
          "pop_quad_to_quad fails on facet %s since edge %s has valence %d.\n",
              ELNAME(f_id),ELNAME1(ee_id),val);
          outstring(msg);
        }
        return 0;
      }
      if ( val == 3 )
      { if ( equal_element(ee_id,e_id) ) continue;
        if ( equal_element(ee_id,get_fe_edge(quad_triples[(i+3)%4])) )
          continue;
        if ( j >= 2 ) 
        { if ( verbose_flag )
          { sprintf(msg,"pop_quad_to_quad fails on facet %s; too many triple edges on vertex %s.\n",ELNAME(f_id),ELNAME1(get_edge_tailv(ee_id)));
            outstring(msg);
          }
          return 0;
        }
        other_triples[i][j++] = ee_id;
      }
    } while ( !equal_id(e_id,ee_id));
    if ( j != 2 )
    { if ( verbose_flag )
      { sprintf(msg,"pop_quad_to_quad fails on facet %s since vertex %s doesn't have enough triple edges.\n",ELNAME(f_id),ELNAME1(get_edge_tailv(e_id)));
        outstring(msg);
      }
      return 0;
    }
  }

  /* check outer triples for common endpoints, due to triangular
   * prisms and pyramids.
   */
  for ( i = 0 ; i < 4 ; i++ )
   for ( j = 0 ; j < 2 ; j++ )
    for ( ii = i ; ii < 4 ; ii++ )
     for ( jj = 0 ; jj < 2 ; jj++ )
     { if ( (ii == i) && (jj == j) ) continue;
       if ( equal_id(get_edge_headv(other_triples[i][j]),
             get_edge_headv(other_triples[ii][jj])) ) 
       { sprintf(msg,"Pop_quad_to_quad fails on facet %s\n",ELNAME(f_id));
         sprintf(msg+strlen(msg),
            "   since triple edges %s and %s have common far endpoint.\n",
              ELNAME(other_triples[i][j]),ELNAME1(other_triples[ii][jj]));
         strcat(msg,"   Probably a face of a pyramid or triangular prism.\n");
         outstring(msg);
         return 0;
       }
     }


  if ( verbose_flag )
  { sprintf(msg,"pop_quad_to_quad on facet %s\n",ELNAME(f_id));
    outstring(msg);
  }

  ea = get_fe_edge(quad_triples[0]); /* for identifying kept edge */
  eb = get_fe_edge(quad_triples[2]);

  /* use old facet to get attributes for new before deleting old */
  /*  newf = new_facet(); */
  newf = dup_facet(f_id);
  set_facet_body(newf,NULLID); 
  set_facet_body(inverse_id(newf),NULLID);

  /* get list of the quadrilateral's facets, in case quad is subdivided
     too much for simple edge deletion to take care of 
   */
  quadfacets[0] = f_id;
  qfcount = 1;
  for ( i = 0 ; i < qfcount ; i++ )
  { fa = quadfacets[i];
    fea = get_facet_fe(fa);
    for ( j = 0 ; j < 3 ; j++, fea = get_next_edge(fea) )
    { if ( equal_id(get_next_facet(fea),get_prev_facet(fea)) )
      { /* add neighbor facet to list if not already there */
        fb = get_fe_facet(get_next_facet(fea));
        for ( k = 0 ; k < qfcount ; k++ )
        { if ( equal_element(fb,quadfacets[k]) )
            break;
        }
        if ( k == qfcount ) /* not found, so add to list */
        { if ( qfcount >= MAXQF )
          { kb_error(2840,"pop_quad_to_quad: over 100 facets in quadrilateral.\n", RECOVERABLE);
          }
          quadfacets[qfcount++] = fb;
        }
      }
    }
  }

  /* Delete original quadrilateral by deleting the short edges */
  keepfe = get_next_facet(quad_triples[0]);
  e_id = get_fe_edge(quad_triples[1]);
  eliminate_edge(e_id);
  free_element(e_id);
  e_id = get_fe_edge(quad_triples[3]);
  eliminate_edge(e_id);
  free_element(e_id);

  /* now delete any leftover quadrilateral facets */
  for ( i = 0 ; i < qfcount ; i++ )
    if ( valid_element(quadfacets[i]) )
      eliminate_facet(quadfacets[i]);

  /* make sure all the triple lines are left */
  for ( i = 0 ; i < 4 ; i++ )
    for ( j = 0 ; j < 2 ; j++ )
      if ( !valid_element(other_triples[i][j]) )
      { sprintf(errmsg,
          "Unanticipated geometry after eliminating quad for facet %s.\n",
             ELNAME(f_id));
        strcat(errmsg,"New quad not created.\n");
        kb_error(2828,errmsg,WARNING);
      }
 
  /* Identify the remaining edge */
  keepe = get_fe_edge(keepfe);

  /* Create new quad */
  newv[0] = get_edge_tailv(keepe);
  newv[1] = get_edge_headv(keepe);
  newv[2] = dup_vertex(newv[1]);
  newv[3] = dup_vertex(newv[0]);
  newe[0] = keepe;
  newe[1] = dup_edge(keepe); 
  set_edge_tailv(newe[1],newv[1]);
  set_edge_headv(newe[1],newv[2]);
  newe[2] = dup_edge(keepe); 
  set_edge_tailv(newe[2],newv[2]);
  set_edge_headv(newe[2],newv[3]);
  newe[3] = dup_edge(keepe); 
  set_edge_tailv(newe[3],newv[3]);
  set_edge_headv(newe[3],newv[0]);
  if ( web.symmetry_flag )
  { set_edge_wrap(newe[1],0);
    set_edge_wrap(newe[2],(*sym_inverse)(get_edge_wrap(newe[0])));
    set_edge_wrap(newe[3],0);
  }
  for ( i = 0 ; i < 4 ; i++ ) 
  { newfe[i] = new_facetedge(newf,newe[i]);
    set_next_facet(newfe[i],newfe[i]);
    set_prev_facet(newfe[i],newfe[i]);
    set_edge_fe(newe[i],newfe[i]);
  }
  set_facet_fe(newf,newfe[0]);
  for ( i = 0 ; i < 4 ; i++ ) 
  { set_next_edge(newfe[i],newfe[(i+1)%4]);
    set_prev_edge(newfe[i],newfe[(i+3)%4]);
  }
  cross_cut(newfe[0],newfe[1]);

  /* Connect in with old stuff */

  /* start with first corner */
  e_id = other_triples[0][0];
  fe = get_edge_fe(e_id);
  for ( k = 0 ; k < 3 ; k++ )
  { /* traverse fan in all three directions */
    fe = get_next_facet(fe);
    next_fe = inverse_id(get_prev_edge(fe));
    for (;;)
    { if ( !equal_id(get_next_facet(next_fe),get_prev_facet(next_fe)) )
      { /* have found next triple edge */
        ee_id = get_fe_edge(next_fe);
        if ( equal_element(ee_id,keepe) )
        { /* everything already ok, but adjust facetedges */

          fea = newfe[0];
          set_next_facet(next_fe,fea);
          set_prev_facet(fea,next_fe);
          set_prev_facet(next_fe,fea);
          set_next_facet(fea,next_fe);

          /* and line up next corner */
          for(;;)
          { next_fe = get_next_edge(next_fe);
            if ( !equal_id(get_next_facet(next_fe),get_prev_facet(next_fe)) )
            { /* have found next triple edge */
              eee_id = get_fe_edge(next_fe);
              if ( equal_element(eee_id,other_triples[1][0]) )
              { /* all ok */
              }
              else if ( equal_element(eee_id,other_triples[1][1]) )
              { /* swap */
                edge_id tempe = other_triples[1][0];
                other_triples[1][0] = other_triples[1][1];
                other_triples[1][1] = tempe;
              }
              break;
            }
            next_fe = inverse_id(get_next_facet(next_fe));
          }
        }
        else if ( equal_id(ee_id,other_triples[0][1]) )
        { /* spread out with new edge */

            facetedge_id feb = inverse_id(get_prev_edge(fe));
            facetedge_id fea = get_prev_edge(fe);
            facet_id fa = get_fe_facet(fe);
            facetedge_id newfe = new_facetedge(fa,newe[3]);
            facetedge_id newefe = get_edge_fe(newe[3]);

            while ( !equal_id(feb,next_fe) )
            { eee_id = get_fe_edge(feb);
              remove_vertex_edge(newv[0],eee_id);
              set_edge_tailv(eee_id,newv[3]);
              feb = get_next_facet(feb);
              feb = inverse_id(get_prev_edge(feb));
            }

            set_next_edge(fea,newfe);
            set_prev_edge(newfe,fea);
            set_prev_edge(fe,newfe);
            set_next_edge(newfe,fe);
            set_next_facet(newfe,newefe);
            set_prev_facet(newefe,newfe);
            set_next_facet(newefe,newfe);
            set_prev_facet(newfe,newefe);
            cross_cut(newfe,fe);

        }
        else if ( equal_id(ee_id,other_triples[3][0]) ) 
        { /* everything ok */
        }
        else if ( equal_id(ee_id,other_triples[3][1]) ) 
        { /* swap */
          edge_id tempe = other_triples[3][0];
          other_triples[3][0] = other_triples[3][1];
          other_triples[3][1] = tempe;
        }
        else /* shouldn't get here */
           kb_error(2820,"Aborted pop_quad_to_quad halfway through.\n",
              RECOVERABLE); 

        break;
      }
      next_fe = get_next_facet(next_fe);
      next_fe = inverse_id(get_prev_edge(next_fe));
    }
  }

  /* other triple out of first corner */
  e_id = other_triples[3][0];
  fe = get_edge_fe(e_id);
  for ( k = 0 ; k < 3 ; k++ )
  { /* traverse fan in all three directions */
    fe = get_next_facet(fe);
    next_fe = inverse_id(get_prev_edge(fe));
    for (;;)
    { if ( !equal_id(get_next_facet(next_fe),get_prev_facet(next_fe)) )
      { /* have found next triple edge */
        ee_id = get_fe_edge(next_fe);
        if ( equal_element(ee_id,keepe) )
        { /* everything already ok, adjust facetedges */

          fea = newfe[0];
          set_next_facet(next_fe,fea);
          set_prev_facet(fea,next_fe);
          set_prev_facet(next_fe,get_next_facet(fea));
          set_next_facet(get_next_facet(fea),next_fe);

          /* and line up next corner */
          for(;;)
          { next_fe = get_next_edge(next_fe);
            if ( !equal_id(get_next_facet(next_fe),get_prev_facet(next_fe)) )
            { /* have found next triple edge */
              eee_id = get_fe_edge(next_fe);
              if ( equal_element(eee_id,other_triples[2][0]) )
              { /* all ok */
              }
              else if ( equal_element(eee_id,other_triples[2][1]) )
              { /* swap */
                edge_id tempe = other_triples[2][0];
                other_triples[2][0] = other_triples[2][1];
                other_triples[2][1] = tempe;
              }
              break;
            }
            next_fe = inverse_id(get_next_facet(next_fe));
          }
        }
        else if ( equal_id(ee_id,other_triples[3][1]) )
        { /* spread out with new edge */

            facetedge_id feb = inverse_id(get_prev_edge(fe));
            facetedge_id fea = get_prev_edge(fe);
            facet_id fa = get_fe_facet(fe);
            facetedge_id newfe = new_facetedge(fa,newe[3]);
            facetedge_id newefe = get_edge_fe(newe[3]);

            while ( !equal_id(feb,next_fe) )
            { eee_id = get_fe_edge(feb);
              remove_vertex_edge(newv[0],eee_id);
              set_edge_tailv(eee_id,newv[3]);
              feb = get_next_facet(feb);
              feb = inverse_id(get_prev_edge(feb));
            }

            set_next_edge(fea,newfe);
            set_prev_edge(newfe,fea);
            set_prev_edge(fe,newfe);
            set_next_edge(newfe,fe);
            set_next_facet(newfe,newefe);
            set_prev_facet(newefe,newfe);
            set_next_facet(get_next_facet(newefe),newfe);
            set_prev_facet(newfe,get_next_facet(newefe));
            cross_cut(newfe,fe);

        }
        else if ( equal_id(ee_id,other_triples[0][0]) ) 
        { /* everything ok */
        }
        else /* shouldn't get here */
           kb_error(2821,"Aborted pop_quad_to_quad halfway through.\n",
              RECOVERABLE); 

        break;
      }
      next_fe = get_next_facet(next_fe);
      next_fe = inverse_id(get_prev_edge(next_fe));
    }
  }
  
  /* another corner, at end of short edge from previous */
  e_id = other_triples[0][1];
  fe = get_edge_fe(e_id);
  for ( k = 0 ; k < 3 ; k++ )
  { /* traverse fan in all three directions */
    fe = get_next_facet(fe);
    next_fe = fe;
    for (;;)
    { ee_id = get_fe_edge(next_fe);

      if ( !equal_id(get_edge_tailv(ee_id),newv[3]) )
      { remove_vertex_edge(newv[0],ee_id);
        set_edge_tailv(ee_id,newv[3]);
      }

      next_fe = inverse_id(get_prev_edge(next_fe));
      if ( !equal_id(get_next_facet(next_fe),get_prev_facet(next_fe)) )
      { /* have found next triple edge */
        ee_id = get_fe_edge(next_fe);
        if ( equal_element(ee_id,newe[0]) )
        { /* move fe to new edge */

          set_fe_edge(next_fe,inverse_id(newe[2]));
          fea = inverse_id(newfe[2]);
          set_next_facet(next_fe,fea);
          set_prev_facet(fea,next_fe);
          set_prev_facet(next_fe,fea);
          set_next_facet(fea,next_fe);

        }
        else if ( equal_element(ee_id,newe[3]) )
        { /* all ok */
        }
        else if ( equal_id(ee_id,other_triples[3][1]) ) 
        { /* everything ok */
        }
        else /* shouldn't get here */
           kb_error(2822,"Aborted pop_quad_to_quad halfway through.\n",
              WARNING); 

        break;
      }
      next_fe = get_next_facet(next_fe);
    }
  }
  
  
  /* other part of same corner, at end of short edge from previous */
  e_id = other_triples[3][1];
  fe = get_edge_fe(e_id);
  for ( k = 0 ; k < 3 ; k++ )
  { /* traverse fan in all three directions */
    fe = get_next_facet(fe);
    next_fe = fe;
    for (;;)
    { ee_id = get_fe_edge(next_fe);

      if ( !equal_id(get_edge_tailv(ee_id),newv[3]) )
      { remove_vertex_edge(newv[0],ee_id);
        set_edge_tailv(ee_id,newv[3]);
      }

      next_fe = inverse_id(get_prev_edge(next_fe));
      if ( !equal_id(get_next_facet(next_fe),get_prev_facet(next_fe)) )
      { /* have found next triple edge */
        ee_id = get_fe_edge(next_fe);
        if ( equal_element(ee_id,newe[0]) )
        { /* move fe to new edge */

          set_fe_edge(next_fe,inverse_id(newe[2]));
          fea = inverse_id(newfe[2]);
          set_next_facet(next_fe,get_prev_facet(fea));
          set_prev_facet(get_prev_facet(fea),next_fe);
          set_prev_facet(next_fe,fea);
          set_next_facet(fea,next_fe);


        }
        else if ( equal_id(ee_id,other_triples[0][1]) )
        { /* all ok */
        }
        else if ( equal_element(ee_id,newe[3]) ) 
        { /* everything ok */
        }
        else /* shouldn't get here */
           kb_error(2823,"Aborted pop_quad_to_quad halfway through.\n",
              WARNING); 

        break;
      }
      next_fe = get_next_facet(next_fe);
    }
  }
  
  /* corner at other end of long edge from first corner */
  e_id = other_triples[1][0];
  fe = get_edge_fe(e_id);
  for ( k = 0 ; k < 3 ; k++ )
  { /* traverse fan in all three directions */
    fe = get_next_facet(fe);
    next_fe = inverse_id(get_prev_edge(fe));
    for (;;)
    { if ( !equal_id(get_next_facet(next_fe),get_prev_facet(next_fe)) )
      { /* have found next triple edge */
        ee_id = get_fe_edge(next_fe);
        if ( equal_element(ee_id,keepe) )
        { /* everything already ok, but adjust facetedges */
        }
        else if ( equal_id(ee_id,other_triples[1][1]) )
        { /* spread out with new edge */

            facetedge_id feb = inverse_id(get_prev_edge(fe));
            facetedge_id fea = get_prev_edge(fe);
            facet_id fa = get_fe_facet(fe);
            facetedge_id newfe = new_facetedge(fa,inverse_id(newe[1]));
            facetedge_id newefe = get_edge_fe(inverse_id(newe[1]));

            while ( !equal_id(feb,next_fe) )
            { eee_id = get_fe_edge(feb);
              remove_vertex_edge(newv[1],eee_id);
              set_edge_tailv(eee_id,newv[2]);
              feb = get_next_facet(feb);
              feb = inverse_id(get_prev_edge(feb));
            }

            set_next_edge(fea,newfe);
            set_prev_edge(newfe,fea);
            set_prev_edge(fe,newfe);
            set_next_edge(newfe,fe);
            set_next_facet(newfe,newefe);
            set_prev_facet(newefe,newfe);
            set_next_facet(newefe,newfe);
            set_prev_facet(newfe,newefe);
            cross_cut(newfe,fe);

        }
        else if ( equal_id(ee_id,other_triples[2][0]) ) 
        { /* everything ok */
        }
        else /* shouldn't get here */
           kb_error(2824,"Aborted pop_quad_to_quad halfway through.\n",
              WARNING); 

        break;
      }
      next_fe = get_next_facet(next_fe);
      next_fe = inverse_id(get_prev_edge(next_fe));
    }
  }
  
  /* other edge at corner at other end of long edge from first corner */
  e_id = other_triples[2][0];
  fe = get_edge_fe(e_id);
  for ( k = 0 ; k < 3 ; k++ )
  { /* traverse fan in all three directions */
    fe = get_next_facet(fe);
    next_fe = inverse_id(get_prev_edge(fe));
    for (;;)
    { if ( !equal_id(get_next_facet(next_fe),get_prev_facet(next_fe)) )
      { /* have found next triple edge */
        ee_id = get_fe_edge(next_fe);
        if ( equal_element(ee_id,keepe) )
        { /* everything already ok, but adjust facetedges */
        }
        else if ( equal_id(ee_id,other_triples[2][1]) )
        { /* spread out with new edge */

            facetedge_id feb = inverse_id(get_prev_edge(fe));
            facetedge_id fea = get_prev_edge(fe);
            facet_id fa = get_fe_facet(fe);
            facetedge_id newfe = new_facetedge(fa,inverse_id(newe[1]));
            facetedge_id newefe = get_edge_fe(inverse_id(newe[1]));

            while ( !equal_id(feb,next_fe) )
            { eee_id = get_fe_edge(feb);
              remove_vertex_edge(newv[1],eee_id);
              set_edge_tailv(eee_id,newv[2]);
              feb = get_next_facet(feb);
              feb = inverse_id(get_prev_edge(feb));
            }

            set_next_edge(fea,newfe);
            set_prev_edge(newfe,fea);
            set_prev_edge(fe,newfe);
            set_next_edge(newfe,fe);
            set_next_facet(newfe,get_prev_facet(newefe));
            set_prev_facet(get_prev_facet(newefe),newfe);
            set_next_facet(newefe,newfe);
            set_prev_facet(newfe,newefe);
            cross_cut(newfe,fe);

        }
        else if ( equal_id(ee_id,other_triples[1][0]) ) 
        { /* everything ok */
        }
        else /* shouldn't get here */
           kb_error(2825,"Aborted pop_quad_to_quad halfway through.\n",
              WARNING); 

        break;
      }
      next_fe = get_next_facet(next_fe);
      next_fe = inverse_id(get_prev_edge(next_fe));
    }
  }
  
  /* final corner */
  e_id = other_triples[1][1];
  fe = get_edge_fe(e_id);
  for ( k = 0 ; k < 3 ; k++ )
  { /* traverse fan in all three directions */
    fe = get_next_facet(fe);
    next_fe = fe;
    for (;;)
    { ee_id = get_fe_edge(next_fe);

      if ( !equal_id(get_edge_tailv(ee_id),newv[2]) )
      { remove_vertex_edge(newv[1],ee_id);
        set_edge_tailv(ee_id,newv[2]);
      }

      next_fe = inverse_id(get_prev_edge(next_fe));
      if ( !equal_id(get_next_facet(next_fe),get_prev_facet(next_fe)) )
      { /* have found next triple edge */
        break;
      }
      next_fe = get_next_facet(next_fe);
    }
  }

  /* other part of final corner */
  e_id = other_triples[2][1];
  fe = get_edge_fe(e_id);
  for ( k = 0 ; k < 3 ; k++ )
  { /* traverse fan in all three directions */
    fe = get_next_facet(fe);
    next_fe = fe;
    for (;;)
    { ee_id = get_fe_edge(next_fe);

      if ( !equal_id(get_edge_tailv(ee_id),newv[2]) )
      { remove_vertex_edge(newv[1],ee_id);
        set_edge_tailv(ee_id,newv[2]);
      }

      next_fe = inverse_id(get_prev_edge(next_fe));
      if ( !equal_id(get_next_facet(next_fe),get_prev_facet(next_fe)) )
      { /* have found next triple edge */
        break;
      }
      next_fe = get_next_facet(next_fe);
    }
  }

  /* move vertices apart */
  for ( i = 0 ; i < 4 ; i++ )
  { x[i] = get_coord(newv[i]);
    for ( j = 0 ; j < 2 ; j++ )
      get_edge_side(other_triples[i][j],sides[i][j]);
  }
  for ( j = 0 ; j < SDIM ; j++ )
  { REAL d;
    d = sides[0][0][j]+sides[1][0][j]+sides[2][0][j]+sides[3][0][j];
    x[0][j] += d/12;
    x[1][j] += d/12;
    d = sides[0][1][j]+sides[1][1][j]+sides[2][1][j]+sides[3][1][j];
    x[2][j] += d/12;
    x[3][j] += d/12;
  }


  /* Fix up facet edge orders */
  for ( i = 0 ; i < 4 ; i++ )
  { facetedge_id next_fe;
    int count;
    fa = newfe[i];
    fb = get_next_facet(fa);
    fc = get_prev_facet(fa);
    ea = newe[i];
    eb = newe[(i+3)%4];
    next_fe = inverse_id(get_prev_edge(fb));
    for(count=0;count < 10000 ;count++)
    { ec = get_fe_edge(next_fe);
      if ( equal_element(ec,ea) )
        break; /* ok */
      if ( equal_element(ec,eb) )
      { /* have to switch order around edge */
        set_prev_facet(fa,fb);
        set_prev_facet(fb,fc);
        set_prev_facet(fc,fa);
        set_next_facet(fa,fc);
        set_next_facet(fc,fb);
        set_next_facet(fb,fa);
        break;
      }
      next_fe = inverse_id(get_prev_edge(get_prev_facet(next_fe)));
    }
    if ( count >= 10000 ) 
    { sprintf(errmsg,"Internal error after pop_quad_to_quad facet %s, bad topology around vertex %s.\n",ELNAME(f_id),ELNAME1(get_edge_tailv(ea)));
      kb_error(2831,errmsg,RECOVERABLE);
    }
  }
 
  /* fix up new facet bodies */
  fa = get_fe_facet(newfe[0]);
  fe = get_prev_facet(newfe[0]);  /* debugging */
  fb = get_fe_facet(fe); 
  b_id = get_facet_body(inverse_id(fb));
  set_facet_body(fa,b_id);
  set_facet_body(inverse_id(get_fe_facet(newfe[0])),
         get_facet_body(get_fe_facet(get_next_facet(newfe[0]))));
  set_facet_body(get_fe_facet(newfe[2]),
         get_facet_body(inverse_id(get_fe_facet(get_prev_facet(newfe[2])))));
  set_facet_body(inverse_id(get_fe_facet(newfe[2])),
         get_facet_body(get_fe_facet(get_next_facet(newfe[2]))));

  if ( web.modeltype == QUADRATIC )
  { /* fix up edge midpoints */
    int bailcount = 0;
    for ( i = 0 ; i < 4 ; i++ )
    { edge_id start_e = get_vertex_edge(newv[i]);
      edge_id ea;
      ea = start_e;
      do
      { new_vertex_average(get_edge_midv(ea),VOLKEEP);
        ea = get_next_tail_edge(ea);
        if ( bailcount++ > 1000 ) break;
      } while ( !equal_id(ea,start_e));
    }
  }

  return 1;
} /* end pop_quad_to_quad */

/*****************************************************************************
*
* function: pop_constrained_vertex()
*
* purpose: analyze and pop a vertex on at least one constraint.
*
* return: 1 if popped, 0 if not.
*/

int pop_constrained_vertex(v_id)
vertex_id v_id;
{
  conmap_t *vmap = get_v_constraint_map(v_id);
  int vhits = v_hit_constraint_count(v_id);
  int kind_counts[5][4];  /* indexed by valence, constraints */
  edge_id kind_lists[5][4][20]; /* actual edges, for easy reference */
  int toohigh=0,triples=0;
  unsigned int i,j,val;
  edge_id e_id,start_e;

  if ( vhits == 3 )
    return triple_con_pop(v_id);

  for ( i = 0; i < 5 ; i++ )
   for ( j = 0 ; j < 4 ; j++ )
     kind_counts[i][j] = 0;
 
  /* detect types of edges */
  e_id = start_e = get_vertex_edge(v_id);
  do
  { int valence = get_edge_valence(e_id);
    conmap_t *emap = get_e_constraint_map(e_id);
    int hits = 0;

    /* see if constraints are at most those of v_id */
    for ( i = 1 ; i <= emap[0] ; i++ )
    { int found = 0;
      for ( j = 1 ; j <= vmap[0] ; j++ )
        if ( emap[i] == (vmap[j] & ~CON_HIT_BIT) )
        { if ( vmap[j] & CON_HIT_BIT )
            hits++;
          found = 1;
          break;
        }
      if ( !found )
      { if ( verbose_flag )
        { sprintf(msg,"Pop vertex %s fails since constraints of edge %s are not a subset of those of the vertex.\n",ELNAME(v_id),ELNAME1(e_id));
          outstring(msg);
        }
        return 0;
      }
    }
    if ( valence > 3 ) { valence = 4; toohigh++; }
    if ( valence == 3 ) triples++;
    if ( hits > 3 ) hits = 4;
    if ( kind_counts[valence][hits] > 20 )
    { sprintf(errmsg,"Too many edges around vertex %s for pop to handle.\n",
        ELNAME(v_id));
      kb_error(4001,errmsg,WARNING);
      return 0;
    }
	kind_lists[valence][hits][kind_counts[valence][hits]++] = e_id;
    e_id = get_next_tail_edge(e_id);
  } while ( !equal_id(e_id,start_e) );

  /* see what we have */
  if ( toohigh )
  { if ( verbose_flag )
    { sprintf(msg,
       "Vertex %s has edges of valence more than 3, so pop edges first.\n",
          ELNAME(v_id));
      outstring(msg);
    }
  }

  if ( (vhits == 2) && (triples == 1))
    return double_con_pop(v_id,kind_lists[3][0][0],kind_lists[1][1]);

  val = get_vertex_evalence(v_id);
  if ( (vhits == 1) && (kind_counts[1][1]==4) &&
             (kind_counts[2][0] == val-4) )
    return one_con_pop_4(v_id,kind_lists[1][1],POP_TO_BETTER);

  /* test for ok configurations */
  if ( (vhits <= 2) && (triples == 0) )
    return 0;

  if ( triples <= 1 ) 
    return 0;

  /* so now have one constraint and at least two triple lines */
  if ( (triples == 2) && (kind_counts[3][0] == 2) && (kind_counts[1][1]==4) )
  { /* common case of two unconstrained triple lines with 4 constrained edges */
    return one_con_pop_2(v_id,kind_lists[3][0],kind_lists[1][1],POP_TO_BETTER);
  }
  if ( (triples >= 3) && (kind_counts[3][0] == triples) 
        && (kind_counts[1][1]==triples) )
  { /* common case of N unconstrained triple lines with N constrained edges */
    return one_con_pop_3(v_id,triples,kind_lists[3][0],kind_lists[1][1],
      POP_TO_BETTER);
  }

  return 0;
}

/*****************************************************************************
*
* function: triple_con_pop()
*
* purpose: Move film out of corner where three constraints meet.
*          Does nothing if one adjacent edge is on two constraints.
*          Just frees vertex from constraint not used by edges,
*          if not a one-sided constraint.  Doesn't actually move vertex.
*
* return: 1 if successful, 0 if not
*/

int triple_con_pop(v_id)
vertex_id v_id;
{
  conmap_t *vmap = get_v_constraint_map(v_id);
  edge_id e_id, start_e;
/*  edge_id conedges[2]; */
  conmap_t cons[2];
  unsigned int ehits=0; /* number of adjacent edges on constraints */
  unsigned int i,j;

  /* see if any adjacent edge is on two constraints */
  /* and find out what constraints are involved */
  e_id = start_e = get_vertex_edge(v_id);
  do
  { conmap_t *emap = get_e_constraint_map(e_id);
    int this_hits = 0;


    for ( i = 1 ; i <= emap[0] ; i++ )
    { 
      for ( j = 0 ;  j <= vmap[0] ; j++ )
      { if ( (emap[j]==(vmap[j]&~CON_HIT_BIT)) && (vmap[j] & CON_HIT_BIT) )
        { if ( this_hits )
          { /* now second constraint on edge */
            return 0;
          }
        /*  conedges[ehits] = e_id; */
          cons[ehits] = emap[j];
          ehits++;
          this_hits++;
        }
      }
    }
    e_id = get_next_tail_edge(e_id);
  } while ( !equal_id(e_id,start_e) );
  
  
  /* see if constraint not used by edges is not one-sided */
  for ( i = 1 ; i <= vmap[0] ; i++ )
  { if ( !(vmap[i] & CON_HIT_BIT) ) continue;
    if ( get_constraint(vmap[i])->attr & (NONPOSITIVE|NONNEGATIVE) ) continue;
    for  ( j = 0 ; j < ehits ; j++ )
      if ( cons[j] == (vmap[i] & ~CON_HIT_BIT) )
        continue;
    /* now have freeable constraint */
    unset_v_constraint_map(v_id,vmap[i]);
    return 1;
  }

  return 0;
}

/*****************************************************************************
*
* function: double_con_pop()
*
* purpose: Pop triple line coming into junction of two constraints.
*          Moves triple line end to the constraint it is most
*          parallel to.  Presumably triple edge got there by deletion
*          of a short edge, so probably wants to go on the constraint
*          with the single edge.
*
* return: 1 if successful, 0 if not
*/

int double_con_pop(v_id,triple,con_edges)
vertex_id v_id;
edge_id triple;  /* the triple edge */
edge_id *con_edges;  /* the edges on constraints */
{
  conmap_t *vmap = get_v_constraint_map(v_id);
  conmap_t cons[2];
  conmap_t concounts[2];
  unsigned int count;
  unsigned int i,j,k;
  edge_id cedges[2][2];
  REAL tripvec[MAXCOORD];
  REAL con_normal[2][MAXCOORD];
  REAL dummy;

  if ( (get_vattr(v_id) & FIXED) || (get_eattr(con_edges[0]) & FIXED)
     || (get_eattr(con_edges[1]) & FIXED) || (get_eattr(con_edges[2]) & FIXED))
    return 0;

  concounts[0] = concounts[1] = count = 0;

  /* find which constraint used twice */
  for ( i = 0 ; i < 3 ; i++ )
  { conmap_t *emap = get_e_constraint_map(con_edges[i]);
    for ( j = 1 ; j <= emap[0] ; j++ )
    { /* see if hit by vertex */
      for ( k = 1 ; k <= vmap[0] ; k++ )
        if ( (emap[j] == (vmap[k] & ~CON_HIT_BIT)) && (vmap[k] & CON_HIT_BIT) )
          break;
      if ( k > vmap[0] ) continue;

      for ( k = 0 ; k < count ; k++ )
      { if ( emap[j] == cons[k] )
        { 
          cedges[k][concounts[k]++] = con_edges[i];
          break;
        }
      }
      if ( k == count )
      { cons[k] = emap[j];
        concounts[k] = 1;
        cedges[k][0] = con_edges[i];
        count++;
      }
    }
  }

  if ( concounts[0] == 1 )
  { /* swap */
    edge_id tempe;
    conmap_t temp;
    tempe = cedges[0][0];
    cedges[0][0] = cedges[1][0];
    cedges[0][1] = cedges[1][1];
    cedges[1][0] = tempe;
    temp = cons[0];
    cons[0] = cons[1];
    cons[1] = temp;
    concounts[0] = 2;
    concounts[1] = 1;
  }
 
  /* calculate gradients */
  for ( i = 0 ; i < 2 ; i++ )
    eval_all(get_constraint(cons[i])->formula,get_coord(v_id),SDIM,
      &dummy,con_normal[i],v_id);

  if (fabs(dot(tripvec,con_normal[0],SDIM)) < fabs(dot(tripvec,con_normal[1],SDIM)))
  { /* v_id goes to single-edge constraint */
    /* generated new vertices on junction by refining edges */
    edge_refine(cedges[0][0]);
    edge_refine(cedges[0][1]);
    /* old edge is the one attached to v_id */
    set_v_constraint_map(get_edge_headv(cedges[0][0]),cons[1]);
    set_v_constraint_map(get_edge_headv(cedges[0][1]),cons[1]);
    unset_v_constraint_map(v_id,cons[0]);
    unset_e_constraint_map(cedges[0][0],cons[0]);
    unset_e_constraint_map(cedges[0][1],cons[0]);
    set_e_constraint_map(cedges[0][0],cons[1]);
    set_e_constraint_map(cedges[0][1],cons[1]);
  }
  else
  { /* move to double-edge constraint */
    edge_refine(cedges[1][0]);
    /* old edge is the one attached to v_id */
    set_v_constraint_map(get_edge_headv(cedges[1][0]),cons[0]);
    unset_v_constraint_map(v_id,cons[1]);
    unset_e_constraint_map(cedges[1][0],cons[1]);
    set_e_constraint_map(cedges[1][0],cons[0]);
  }

  return 1;
}

/***************************************************************************
*
* function: one_con_pop_2()
*
* purpose: pop a specific configuration of two triple edges on vertex
*          with one constraint.  Favors simple pull apart of triple edges.
*
* return: 1 if successful
*/

int one_con_pop_2(v_id,triples,con_edges,mode)
vertex_id  v_id;
edge_id *triples;
edge_id *con_edges;
int mode; /* POP_TO_OPEN or POP_TO_TWIST or POP_BETTER */
{ vertex_id newv;
  edge_id newe,e_id;
  facetedge_id fe,start_fe,newfe;
  unsigned int i,j,k;
  REAL sides[2][2][MAXCOORD];
  REAL mag[2][2];
  REAL *x;
  edge_id attached[2][2];  /* which constraint edges attached to which trip */
  int atcount[2];
  conmap_t con=0,*vmap;  /* which constraint involved */
  int n;
  REAL pull[2][MAXCOORD];  /* separating forces for the two ways */

  if ( get_vattr(v_id) & FIXED ) return 0;
 
  /* see which constraint edges attached to which triple, and get in order */
  atcount[0] = atcount[1] = 0;
  for ( k = 0 ; k < 2 ; k++ )   /* for each triple edge */
  { int triple_second_flag = 0;
    start_fe = get_edge_fe(triples[k]);
    for ( i = 0 ; i < 3 ; i++ )
    {
      fe = inverse_id(get_prev_edge(start_fe));
      for(;;)
      {
        if ( equal_id(triples[1-k],get_fe_edge(fe)) )
        { if ( i == 1 )
            triple_second_flag = 1;
          break;
        }
        e_id = get_fe_edge(fe); 
        if ( equal_id(fe,get_next_facet(fe)) )
        { /* found valence 1 edge */
          attached[k][atcount[k]++] = e_id;    
          break;
        }
        fe = inverse_id(get_prev_edge(get_next_facet(fe)));
      } 
      start_fe = get_next_facet(start_fe);
    } 
    if ( triple_second_flag )
    { /* swap into standard order */
      edge_id temp = attached[k][0];
      attached[k][0] = attached[k][1];
      attached[k][1] = temp;
    }
    for ( i = 0  ; i < 2 ; i++ )
      get_edge_side(attached[k][i],sides[k][i]);
  }

  /* see which constraint involved */
  vmap = get_v_constraint_map(v_id);
  for ( i = 1; i <= vmap[0] ; i++ )
    if ( vmap[i] & CON_HIT_BIT )
    { con = vmap[i] & ~CON_HIT_BIT;
      break;
    }


  /* figure out which way it should pop according to edges on constraints */
  if ( mode == POP_TO_BETTER )
  { for ( i = 0 ; i < 2 ; i++ )
      for ( j = 0 ; j < 2 ; j++ )
        mag[i][j] = dot(sides[i][j],sides[i][j],SDIM);
    for ( n = 0 ; n < SDIM ; n++ )
    { pull[0][n] = sides[0][0][n]/mag[0][0]+sides[1][1][n]/mag[1][1]
                        -sides[0][1][n]/mag[0][1]-sides[1][0][n]/mag[1][0];
      pull[1][n] = sides[0][0][n]/mag[0][0]+sides[0][1][n]/mag[0][1]
                        -sides[1][0][n]/mag[1][0]-sides[1][1][n]/mag[1][1];
    }
    if ( dot(pull[0],pull[0],SDIM) < 1.2*dot(pull[1],pull[1],SDIM) )
      mode = POP_TO_OPEN;
    else
      mode = POP_TO_TWIST;
  }

  if ( mode == POP_TO_OPEN )
  { /* simple pull two triple edges apart */
  
    newv = dup_vertex(v_id);
    newe = new_edge(v_id,newv,NULLID);
    set_e_conmap(newe,get_v_constraint_map(v_id));  
  
    /* move stuff around second triple edge to newv */
    remove_vertex_edge(v_id,triples[1]);
    set_edge_tailv(triples[1],newv);
    start_fe = get_edge_fe(triples[1]);
    for ( i = 0 ; i < 3 ; i++ )
    {
      fe = inverse_id(get_prev_edge(start_fe));
      for(;;)
      {
        if ( equal_id(triples[0],get_fe_edge(fe)) )
        { /* take care of splitting facet(s) between triples */
          facet_id f_id = inverse_id(get_fe_facet(fe));
          newfe = new_facetedge(f_id,newe);
  	      set_edge_fe(newe,newfe);
          set_next_facet(newfe,newfe);
          set_prev_facet(newfe,newfe);
          set_prev_edge(newfe,inverse_id(fe));
          set_next_edge(newfe,inverse_id(get_prev_edge(fe)));
          set_prev_edge(inverse_id(get_prev_edge(fe)),newfe);
          set_next_edge(inverse_id(fe),newfe);
          cross_cut(inverse_id(fe),newfe);
          break;
        }
        e_id = get_fe_edge(fe); 
        remove_vertex_edge(v_id,e_id);
        set_edge_tailv(e_id,newv); 
        if ( equal_id(fe,get_next_facet(fe)) )
        { /* found valence 1 edge */
          break;
        }
        fe = inverse_id(get_prev_edge(get_next_facet(fe)));
      } 
      start_fe = get_next_facet(start_fe);
    } 
  
    /* move vertices apart a bit */
  
    x = get_coord(newv);
    for ( n = 0 ; n < SDIM ; n++ )
      x[n] += sides[0][0][n]/6 + sides[0][1][n]/6 + sides[1][0][n]/3 
             + sides[1][1][n]/3;
  
    x = get_coord(v_id);
    for ( n = 0 ; n < SDIM ; n++ )
      x[n] += sides[0][0][n]/3 + sides[0][1][n]/3 + sides[1][0][n]/6 
             + sides[1][1][n]/6;
  }
  else
  { /* create twist facet in pull-apart */
    vertex_id newv1,newv2;
    edge_id newe1,newe2,newe3;
    facet_id newf;
    facetedge_id fe,prevfe,newfe1,newfe2,newfe3,newfe4,newfe5;
    REAL tsides[2][MAXCOORD];

    /* v_id will be released from constraint and moved off constraint */
    newv1 = dup_vertex(v_id);
    newv2 = dup_vertex(v_id);
    newe1 = new_edge(v_id,newv1,triples[0]);
    newe2 = new_edge(v_id,newv2,triples[0]);
    newe3 = new_edge(newv1,newv2,NULLID);
    set_e_conmap(newe3,get_v_constraint_map(v_id));  
    unset_v_constraint_map(v_id,con);

    /* create twist facet */
    newf  = new_facet();
    newfe1 = new_facetedge(newf,newe1);
    newfe2 = new_facetedge(inverse_id(newf),newe2);
    newfe3 = new_facetedge(newf,newe3);
	set_edge_fe(newe1,newfe1);
	set_edge_fe(newe2,newfe2);
	set_edge_fe(newe3,newfe3);
    set_facet_fe(newf,newfe1);
    set_next_edge(newfe1,newfe3);
    set_next_edge(newfe2,inverse_id(newfe3));
    set_next_edge(newfe3,inverse_id(newfe2));
    set_prev_edge(newfe1,inverse_id(newfe2));
    set_prev_edge(newfe2,inverse_id(newfe1));
    set_prev_edge(newfe3,newfe1);
	set_next_facet(newfe3,newfe3);
	set_prev_facet(newfe3,newfe3);

    /* reconnect wings */
    remove_vertex_edge(v_id,attached[0][0]);
    remove_vertex_edge(v_id,attached[0][1]);
    remove_vertex_edge(v_id,attached[1][0]);
    remove_vertex_edge(v_id,attached[1][1]);
    set_edge_tailv(attached[0][0],newv2); 
    set_edge_tailv(attached[0][1],newv1); 
    set_edge_tailv(attached[1][0],newv1); 
    set_edge_tailv(attached[1][1],newv2); 
    
    fe = get_edge_fe(attached[0][0]);
    prevfe = get_prev_edge(fe);
    newfe4 = new_facetedge(get_fe_facet(fe),newe2);
    set_next_edge(prevfe,newfe4);
    set_prev_edge(newfe4,prevfe);
    set_next_edge(newfe4,fe);
    set_prev_edge(fe,newfe4);
    set_next_facet(newfe2,newfe4);
    set_prev_facet(newfe4,newfe2);
    cross_cut(newfe4,fe);

    fe = get_edge_fe(attached[1][1]);
    prevfe = get_prev_edge(fe);
    newfe5 = new_facetedge(get_fe_facet(fe),newe2);
    set_next_edge(prevfe,newfe5);
    set_prev_edge(newfe5,prevfe);
    set_next_edge(newfe5,fe);
    set_prev_edge(fe,newfe5);
    set_next_facet(newfe5,newfe2);
    set_prev_facet(newfe2,newfe5);
    set_next_facet(newfe4,newfe5);
    set_prev_facet(newfe5,newfe4);
    cross_cut(newfe5,fe);

    fe = get_edge_fe(attached[1][0]);
    prevfe = get_prev_edge(fe);
    newfe4 = new_facetedge(get_fe_facet(fe),newe1);
    set_next_edge(prevfe,newfe4);
    set_prev_edge(newfe4,prevfe);
    set_next_edge(newfe4,fe);
    set_prev_edge(fe,newfe4);
    set_next_facet(newfe1,newfe4);
    set_prev_facet(newfe4,newfe1);
    cross_cut(newfe4,fe);

    fe = get_edge_fe(attached[0][1]);
    prevfe = get_prev_edge(fe);
    newfe5 = new_facetedge(get_fe_facet(fe),newe1);
    set_next_edge(prevfe,newfe5);
    set_prev_edge(newfe5,prevfe);
    set_next_edge(newfe5,fe);
    set_prev_edge(fe,newfe5);
    set_next_facet(newfe5,newfe1);
    set_prev_facet(newfe1,newfe5);
    set_next_facet(newfe4,newfe5);
    set_prev_facet(newfe5,newfe4);
    cross_cut(newfe5,fe);

    set_facet_body(newf,get_facet_body(inverse_id(get_fe_facet(newfe5))));
    set_facet_body(inverse_id(newf),get_facet_body(get_fe_facet(newfe4)));

    /* move vertices */
    x = get_coord(newv1);
    for ( n = 0 ; n < SDIM ; n++ )
      x[n] += sides[1][0][n]/3 + sides[0][1][n]/3 + sides[0][0][n]/6 
             + sides[1][1][n]/6;
  
    x = get_coord(newv2);
    for ( n = 0 ; n < SDIM ; n++ )
      x[n] += sides[1][0][n]/6 + sides[0][1][n]/6 + sides[0][0][n]/3 
             + sides[1][1][n]/3;

    x = get_coord(v_id);
    get_edge_side(triples[0],tsides[0]);
    get_edge_side(triples[1],tsides[1]);
    for ( n = 0 ; n < SDIM ; n++ )
      x[n] += 0.15*tsides[0][n] + 0.15*tsides[1][n];

  }
  return 1;

}


/***************************************************************************
*
* function: one_con_pop_3()
*
* purpose: pop a specific configuration of N triple edges on vertex
*          with one constraint.  Each triple edge is on a fin with
*          one constrained edge.
*
* return: 1 if successful
*/

#define MAXTRIPS 20

int one_con_pop_3(v_id,tripcount,triples,con_edges,mode)
vertex_id  v_id;
int tripcount;
edge_id *triples;
edge_id *con_edges;
int mode; /* POP_TO_OPEN or POP_TO_TRIPLE or POP_BETTER */
{
  edge_id e_id;
  unsigned int i;
  REAL sides[MAXTRIPS][MAXCOORD];
  REAL tsides[MAXTRIPS][MAXCOORD];
  REAL mag[MAXTRIPS];
  REAL net[MAXCOORD];
  REAL *x;
  conmap_t con=0,*vmap;  /* which constraint involved */
  int j,n,m;
  facetedge_id fe;
  
  if ( get_vattr(v_id) & FIXED ) return 0;
  
   /* see which constraint involved */
  vmap = get_v_constraint_map(v_id);
  for ( i = 1; i <= vmap[0] ; i++ )
    if ( vmap[i] & CON_HIT_BIT )
    { con = vmap[i] & ~CON_HIT_BIT;
      break;
    }

  /* get triple edges and constrained edges lined up */
  fe = get_edge_fe(con_edges[0]);
  fe = inverse_id(get_prev_edge(fe));
  while ( equal_id(get_next_facet(fe),get_prev_facet(fe)) )
    fe = inverse_id(get_prev_edge(get_next_facet(fe)));
  fe = get_prev_facet(fe);
  for ( m = 0 ; m < tripcount ; m++ )
  { facetedge_id ffe;
    triples[m] = get_fe_edge(fe);
    /* seek down to corresponding constrained edge */
    ffe = get_next_facet(fe);
    ffe = inverse_id(get_prev_edge(ffe));
    while ( !equal_id(get_next_facet(ffe),ffe) )
      ffe = inverse_id(get_prev_edge(get_next_facet(ffe)));
    con_edges[m] = get_fe_edge(ffe);
    /* now over to next triple */
    fe = get_prev_facet(fe);
    fe = inverse_id(get_prev_edge(fe));
    while ( equal_id(get_next_facet(fe),get_prev_facet(fe)) )
      fe = inverse_id(get_prev_edge(get_next_facet(fe)));
  }

  for ( m = 0  ; m < tripcount ; m++ )
    get_edge_side(triples[m],tsides[m]);

  for ( m = 0  ; m < tripcount ; m++ )
  { get_edge_side(con_edges[m],sides[m]);
    mag[m] = sqrt(dot(tsides[m],tsides[m],SDIM));
  }

  /* figure out which way it should pop according to edges on constraints */
  if ( mode == POP_TO_BETTER )
  { for ( n = 0 ; n < SDIM ; n++ )
      for ( m = 0, net[n] = 0.0 ; m < tripcount ; m++ )
         net[n] += tsides[m][n]/mag[m];
    if ( sqrt(dot(net,net,SDIM)) < 0.7*tripcount )
       mode = POP_TO_OPEN;
    else 
       mode = POP_TO_TRIPLE;
  }

  /* do the pop */
  if ( mode == POP_TO_OPEN )
  { /* make open triangle */
    vertex_id newv[MAXTRIPS];
    edge_id newe[MAXTRIPS];
    facetedge_id next_fe,fe,start_fe,newfe;
  
    newv[0] = v_id;
    for ( m = 1 ; m < tripcount ; m++ )
      newv[m] = dup_vertex(v_id);
    for ( m = 0 ; m < tripcount ; m++ )
    { newe[m] = new_edge(newv[m],newv[(m+1)%tripcount],NULLID);
      set_e_conmap(newe[m],get_v_constraint_map(v_id));  
    }
  
    /* move stuff around last two constraint edges to new vertices */
    /* and putting in new edges */
    for ( m = 0 ; m < tripcount ; m++ )
    { int found_triple;
      if ( m > 0 )
      { remove_vertex_edge(v_id,con_edges[m]);
        set_edge_tailv(con_edges[m],newv[m]);
      }
      start_fe = get_edge_fe(con_edges[m]);
      fe = inverse_id(get_prev_edge(start_fe));
      for( found_triple = 0 ; !found_triple ; fe = next_fe )
        {
          next_fe = inverse_id(get_prev_edge(get_next_facet(fe))); 
          if ( !equal_id(get_next_facet(fe),get_prev_facet(fe)) )
          { /* take care of splitting facet(s) between triples */
            facet_id f_id;
            facetedge_id ffe,prevfe;
            ffe = get_next_facet(fe);
            f_id = inverse_id(get_fe_facet(ffe));
            newfe = new_facetedge(f_id,newe[m]);
    		set_edge_fe(newe[m],newfe);
    		set_next_facet(newfe,newfe);
    		set_prev_facet(newfe,newfe);
            prevfe = inverse_id(get_prev_edge(ffe));
            set_prev_edge(newfe,inverse_id(ffe));
            set_next_edge(newfe,prevfe);
            set_prev_edge(prevfe,newfe);
            set_next_edge(inverse_id(ffe),newfe);
            /* continue on, resetting endpoints on crossing face */
            if ( m != tripcount-1 )
            while (equal_id(get_next_facet(prevfe),get_prev_facet(prevfe)))
            { e_id = get_fe_edge(prevfe);
              remove_vertex_edge(v_id,e_id);
              set_edge_tailv(e_id,newv[m+1]); 
              prevfe = inverse_id(get_prev_edge(get_next_facet(prevfe)));
            } 
            cross_cut(newfe,get_next_edge(newfe));
            found_triple = 1;
          }
          e_id = get_fe_edge(fe); 
          if ( m > 0 )
          { remove_vertex_edge(v_id,e_id);
            set_edge_tailv(e_id,newv[m]); 
          }
        } 
       
    }
  
    /* move vertices apart a bit */
    for ( m = 0 ; m < tripcount ; m++ )
    { x = get_coord(newv[m]);
      for ( n = 0 ; n < SDIM ; n++ )
      { x[n] += 0.5*sides[m][n];
        for ( j = 0 ; j < tripcount ; j++ )
          x[n] += 0.5*sides[j][n]/tripcount;
      }
    }
  }
  else
  { /* create pulled-out triple edge */
    vertex_id newv;
    edge_id newe;
    facetedge_id fe,prevfe,newfe[MAXTRIPS];

    /* v_id will be released from constraint and moved off constraint */
    newv = dup_vertex(v_id);
    newe = new_edge(v_id,newv,triples[0]);
    unset_v_constraint_map(v_id,con);

    /* reconnect wings */
    for ( m = 0 ; m < tripcount ; m++ )
    { remove_vertex_edge(v_id,con_edges[m]);
      set_edge_tailv(con_edges[m],newv); 
      fe = get_edge_fe(con_edges[m]);
      prevfe = get_prev_edge(fe);
      newfe[m] = new_facetedge(get_fe_facet(fe),newe);
      set_next_edge(prevfe,newfe[m]);
      set_prev_edge(newfe[m],prevfe);
      set_next_edge(newfe[m],fe);
      set_prev_edge(fe,newfe[m]);
      cross_cut(newfe[m],fe);
    }
    set_edge_fe(newe,newfe[0]);

    for ( m = 0 ; m < tripcount ; m++ )
    { set_next_facet(newfe[m],newfe[(m+1)%tripcount]);
      set_prev_facet(newfe[(m+1)%tripcount],newfe[m]);
    }
    fe_reorder(newe); /* make sure in proper geometric order */


    /* move vertex */
    x = get_coord(v_id);
    for ( n = 0 ; n < SDIM ; n++ )
    {  for ( m= 0 ; m < tripcount ; m++ )
         x[n] += 0.3*tsides[m][n]/tripcount;
    }

  }
  return 1;

}


/***************************************************************************
*
* function: one_con_pop_4()
*
* purpose: pop a specific configuration of two films meeting at a
*          point on a constraint, i.e. parting mounds.  
*          Or joining mounds that are touching at one vertex.
*
* return: 1 if successful
*/

int one_con_pop_4(v_id,con_edges,mode)
vertex_id  v_id;
edge_id *con_edges;
int mode; /* POP_TO_OPEN or POP_TO_TRIANGLE or POP_TO_BETTER */
{
  vertex_id newv[2];
  int i;
  REAL sides[4][MAXCOORD];
  REAL mag[4];
  facetedge_id fe,next_fe;
  
  if ( get_vattr(v_id) & FIXED ) return 0;
  
  /* see which constraint edges belong together */
  fe = get_edge_fe(con_edges[0]); 
  do 
  { fe = inverse_id(get_prev_edge(fe));
    next_fe = get_next_facet(fe);
    if ( equal_id(fe,next_fe) )
    { /* found it */
      edge_id other_e = get_fe_edge(fe);
      for ( i = 2 ; i < 4 ; i++ )  /* get in second spot */
        if ( equal_id(other_e,con_edges[i]) )
        { con_edges[i] = con_edges[1];
          con_edges[1] = other_e;
        }
      break;
    }
    fe = next_fe;
  } while (1);


  /* get side vectors */
  for ( i = 0 ; i < 4 ; i++ )
  { get_edge_side(con_edges[i],sides[i]);
    mag[i] = sqrt(dot(sides[i],sides[i],SDIM));
  }

  /* figure out which way it should pop according to edges on constraints */
  if ( mode == POP_TO_BETTER )
  { if ( pop_disjoin_flag )
       mode = POP_TO_OPEN;
    /* if included angles bigger than exterior, then merge */
    else if ( acos(dot(sides[0],sides[1],SDIM)/mag[0]/mag[1])
       + acos(dot(sides[2],sides[3],SDIM)/mag[2]/mag[3]) > M_PI )
       mode = POP_TO_TRIANGLE;
    else 
       mode = POP_TO_OPEN;
  }

  /* do the pop */
  if ( mode == POP_TO_OPEN )
  { /* make separate vertices */
    edge_id other_e;

    newv[0] = v_id;
    newv[1] = dup_vertex(v_id);
   
    /* reconnect edges */
    next_fe = get_edge_fe(con_edges[2]); 
    do 
    { 
      fe = next_fe;
      other_e = get_fe_edge(fe);
      remove_vertex_edge(v_id,other_e);
      set_edge_tailv(other_e,newv[1]);
      fe = inverse_id(get_prev_edge(fe));
      next_fe = get_next_facet(fe);
    } while ( !equal_id(other_e,con_edges[3]) );
    set_vertex_edge(newv[0],con_edges[0]);
    set_vertex_edge(newv[1],con_edges[2]);
  
    /* no reason to move vertices apart a bit */
  }
  else
  { /* create pulled-out triangle */
    facetedge_id fe1,fe2,newfe1,newfe2,prev1,prev2;
    REAL *xold,*xnew;
    edge_id newe;
    facet_id f1,f2;
    facetedge_id fe_a,fe_b,cc_fe;
    body_id b1f,b1b,b2f,b2b;
    
    /* get con_edges in canonical order */
    if ( dot(sides[0],sides[3],SDIM)*mag[1]*mag[2]
        + dot(sides[1],sides[2],SDIM)*mag[0]*mag[3]  
          < dot(sides[0],sides[2],SDIM)*mag[1]*mag[3]
        + dot(sides[1],sides[3],SDIM)*mag[0]*mag[2])
      { edge_id tmpe = con_edges[2]; con_edges[2] = con_edges[3];
        con_edges[3] = tmpe;
        for ( i = 0 ; i < SDIM ; i++ )
        { REAL t = sides[2][i]; sides[2][i] = sides[3][i]; sides[3][i] = t; }
      }
    

    /* first, split vertex and put edge between */
    newv[0] = v_id;
    newv[1] = dup_vertex(v_id);
    newe = new_edge(newv[0],newv[1],NULLID);
    f1 = get_fe_facet(get_edge_fe(con_edges[1]));
    f2 = get_fe_facet(get_edge_fe(con_edges[2]));
    newfe1 = new_facetedge(f1,newe);
    newfe2 = new_facetedge(f2,newe);
    set_edge_fe(newe,newfe1);
    set_next_facet(newfe1,newfe2);
    set_prev_facet(newfe1,newfe2);
    set_next_facet(newfe2,newfe1);
    set_prev_facet(newfe2,newfe1);

    /* reconnect edges */
    remove_vertex_edge(v_id,con_edges[1]);
    set_edge_tailv(con_edges[1],newv[1]);
    remove_vertex_edge(v_id,con_edges[2]);
    set_edge_tailv(con_edges[2],newv[1]);
    fe1 = get_edge_fe(con_edges[1]);
    prev1 = get_prev_edge(fe1);
    set_prev_edge(newfe1,prev1); 
    set_next_edge(prev1,newfe1);
    set_next_edge(newfe1,fe1);
    set_prev_edge(fe1,newfe1);
    fe2 = get_edge_fe(con_edges[2]);
    prev2 = get_prev_edge(fe2);
    set_prev_edge(newfe2,prev2); 
    set_next_edge(prev2,newfe2);
    set_next_edge(newfe2,fe2);
    set_prev_edge(fe2,newfe2);

    cross_cut(prev1,newfe1);
    cross_cut(prev2,newfe2);

    /* move vertices apart a bit */
    xold = get_coord(v_id);
    xnew = get_coord(newv[1]);
    for ( i = 0 ; i < SDIM ; i++ )
    { xold[i] += 0.1 * sides[0][i] + 0.1 * sides[3][i]; 
      xnew[i] += 0.1 * sides[1][i] + 0.1 * sides[2][i]; 
    }

    /* refine the new edge */
    cc_fe = get_next_edge(newfe1); /* need to save for later */
    edge_refine(newe);

    /* Septum if bodies disagree */
    b1f = get_facet_body(f1);
    b1b = get_facet_body(inverse_id(f1));
    b2f = get_facet_body(f2);
    b2b = get_facet_body(inverse_id(f2));
    /* figure out which side needs the septum, if any */
    fe_a = fe_b = NULLID;
    if ( !equal_id(b1b,b2f) )
    { fe_a = newfe1;
      fe_b = get_prev_edge(cc_fe);
    }
    else if ( !equal_id(b1f,b2b) )
    { fe_a = inverse_id(get_prev_edge(cc_fe));
      fe_b = inverse_id(newfe1);
    }
    if ( valid_id(fe_a) )
    { /* put in new facet */
      edge_id span_e = new_edge( get_fe_headv(fe_b),get_fe_tailv(fe_a),v_id);
      facet_id span_f = new_facet();
      facetedge_id span_fe = new_facetedge(span_f,span_e);
      facetedge_id fe_a_new = new_facetedge(span_f,get_fe_edge(fe_a));
      facetedge_id fe_b_new = new_facetedge(span_f,get_fe_edge(fe_b));
      facetedge_id fe_c,fe_d;
      
      set_facet_fe(span_f,span_fe);
      set_edge_fe(span_e,span_fe);
      set_facet_density(span_f,1.0);
      set_next_edge(span_fe,fe_a_new);
      set_next_edge(fe_a_new,fe_b_new);
      set_next_edge(fe_b_new,span_fe);
      set_prev_edge(span_fe,fe_b_new);
      set_prev_edge(fe_b_new,fe_a_new);
      set_prev_edge(fe_a_new,span_fe);
      set_next_facet(span_fe,span_fe);
      set_prev_facet(span_fe,span_fe);
      fe_c = get_next_facet(fe_a);
      fe_d = get_next_facet(fe_b);
      
      set_next_facet(fe_a,fe_a_new);
      set_next_facet(fe_a_new,fe_c);
      set_prev_facet(fe_c,fe_a_new);
      set_prev_facet(fe_a_new,fe_a);
      
      set_next_facet(fe_b,fe_b_new);
      set_next_facet(fe_b_new,fe_d);
      set_prev_facet(fe_d,fe_b_new);
      set_prev_facet(fe_b_new,fe_b);
      
      set_facet_body(span_f,get_facet_body(inverse_id(get_fe_facet(fe_a))));
      set_facet_body(inverse_id(span_f),get_facet_body(get_fe_facet(fe_c)));
      
    }
    
    
  } /* end pulled-out triangle */
  return 1;

}

/**************************************************************************
*
* function: pop_tri_to_edge_con()
*
* purpose: Implement pop_tri_to_edge for triple edge with tail on constraint.
*          Algorithm: deletes edge, calls one_con_pop_3 in proper mode.
*          Called from pop_tri_to_edge(), which has checked valence and
*          number of triple edges at head.
*
* return: 1 if successful, 0 if not.
*/
int pop_edge_to_tri_con(e_id)
edge_id e_id;
{ vertex_id v_id = get_edge_tailv(e_id);
  edge_id ee_id, start_e;
  int retval;
  int concount = 0,tripcount = 0;
  edge_id triples[20];
  edge_id con_edges[20];

  retval = eliminate_edge(e_id);
  if ( retval == 0 ) 
    return 0;
  free_element(e_id); /* quirk of eliminate_edge */

  /* gather data for one_con_pop_3 */
  ee_id = start_e = get_vertex_edge(v_id);
  do
  { int valence = get_edge_valence(ee_id); 
    if ( valence == 3 )
      triples[tripcount++] = ee_id;
    if ( valence == 1 )
      con_edges[concount++] = ee_id;
    ee_id = get_next_tail_edge(ee_id);
    if ( tripcount >= 20 || concount >= 20 )
    { if ( verbose_flag )
      { sprintf(msg,"Can't handle over 20-valence edge %s! Sorry.\n",
           ELNAME(e_id)); 
        outstring(msg);
      }
      return 0;
    }
  } while ( !equal_id(ee_id,start_e) );
  if ( tripcount != concount )
  { if ( verbose_flag )
    { sprintf(msg,"Pop_edge_to_tri not applicable to edge %s.\n",ELNAME(e_id));
      outstring(msg);
    }
    return 0;
  }
  return one_con_pop_3(v_id,tripcount,triples,con_edges,POP_TO_OPEN);
}
