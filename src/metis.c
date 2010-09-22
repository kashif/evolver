/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/


/**********************************************************
*
*  File: metis.c
*
*  Contents:  Interface to MeTiS library of George Karypis
*  and Vipin Kumar, U Minnesota, karypis@cs.umn.edu, kumar@cs.umn.edu
*  Define -DMETIS on compiler command line and link libmetis.a
*  if you want to use these routines.
*/

#include "include.h"

#ifndef METIS
void metis_partition(parts,algorithm)
int parts;
int algorithm; /* METIS_ or KMETIS_ */
{ kb_error(1623,"This Evolver not compiled with METIS library.\n",RECOVERABLE);

}
void metis_partition_dual(parts,algorithm)
int parts;
int algorithm; /* METIS_ or KMETIS_ */
{ kb_error(1624,"This Evolver not compiled with METIS library.\n",RECOVERABLE);

}
void metis_partition_body(parts,algorithm)
int parts;
int algorithm; /* METIS_ or KMETIS_ */
{ kb_error(3778,"This Evolver not compiled with METIS library.\n",RECOVERABLE);

}
void metis_order(S)
struct linsys *S;
{ kb_error(1625,"This Evolver not compiled with METIS library.\n",RECOVERABLE);

}
void metis_vertex_order(parts)
int parts;
{ kb_error(1626,"This Evolver not compiled with METIS library.\n",RECOVERABLE);
}
void metis_partition_plain(parts,algorithm)
int parts;
int algorithm; /* METIS_ or KMETIS_ */
{ kb_error(6345,"This Evolver not compiled with METIS library.\n",RECOVERABLE);
}
#else

#include "ytab.h"
#include "metis.h"

/************************************************************************
*
* function: metis_partition_plain()
*
* purpose: Use metis to partition vertices. Passes actual vertex-edge
*          graph to metis.  Only does end-edge vertices.  Leaves
*          partition number in vertex attribute vpart.
*/
void metis_partition_plain(parts,algorithm)
int parts;
int algorithm; /* METIS_ or KMETIS_ */
{
  int i;
  GraphType graph;
  int *partition, options[10], edgecut, nparts, numbering;
  vertex_id v_id;
  edge_id e_id,start_e;
  int j,k;
  int vpart;
  int weightflag = 0;

  if ( web.representation == SIMPLEX )
     kb_error(1627,"Cannot do Metis on simplex model.\n",RECOVERABLE);

  if ( web.skel[VERTEX].count != web.skel[VERTEX].max_ord+1 )
     kb_error(1628,"Need packed vertex numbering to do Metis.\n",RECOVERABLE);

  /* use partition attribute for contiguous numbering */
  vpart = find_attribute(VERTEX,"vpart");
  if ( vpart < 0 ) 
  { int one = 1;
    vpart = add_attribute(VERTEX,"vpart",INTEGER_TYPE,0,&one,1,NULL);
  }
  k = 0;
  FOR_ALL_VERTICES(v_id)
    if ( !(get_vattr(v_id) & (Q_MIDPOINT|Q_MIDEDGE|Q_MIDFACET)) )
      ((int*)get_extra(v_id,vpart))[0] = k++;

  nparts = parts;
  memset((char*)&graph,0,sizeof(graph));
  
  /* construct graph data */
  graph.xadj = (int*)temp_calloc(web.skel[VERTEX].count+1,sizeof(int));
  graph.adjncy = (int*)temp_calloc(2*web.skel[EDGE].count,sizeof(int));
  j = 0; i = 0;
  FOR_ALL_VERTICES(v_id)
    if ( !(get_vattr(v_id) & (Q_MIDPOINT|Q_MIDEDGE|Q_MIDFACET)) )
    { e_id = start_e = get_vertex_edge(v_id);
      do 
      { vertex_id vv_id = get_edge_headv(e_id); 
        graph.adjncy[j++] = ((int*)get_extra(vv_id,vpart))[0];
        e_id = get_next_tail_edge(e_id);
      } while ( !equal_element(e_id,start_e) );
      graph.xadj[++i] = j;
    }

  /* do partition */
  options[0] = 0;  /* take defaults */
  numbering = 0;
  graph.nvtxs = web.skel[VERTEX].count;
  partition = (int*)temp_calloc(graph.nvtxs,sizeof(int));
  METIS_PartGraphKway(&graph.nvtxs, graph.xadj, graph.adjncy, NULL, NULL, 
        &weightflag,&numbering, &nparts, options, &edgecut, partition);

  /* label things */

  FOR_ALL_VERTICES(v_id)
    if ( !(get_vattr(v_id) & (Q_MIDPOINT|Q_MIDEDGE|Q_MIDFACET)) )
    { int *vspot = ((int*)get_extra(v_id,vpart));
      *vspot = partition[*vspot];
    }

  /* free storage */
  temp_free((char*)graph.xadj);
  temp_free((char*)graph.adjncy);
  temp_free((char*)partition);
}

/************************************************************************
*
* function: metis_partition_dual()
*
* purpose: Use metis to partition facets. Passes dual facet-edge
*             graph to metis.
*             For string model, treats vertices and edges as graph
*             nodes.
*/
void metis_partition_dual(parts,algorithm)
int parts;
int algorithm; /* METIS_ or KMETIS_ */
{
  int i,k;
  GraphType graph;
  int *partition, options[10], edgecut, nparts, numbering;
  vertex_id v_id;
  edge_id e_id;
  int j;
  int weightflag = 0;

  if ( web.representation == SIMPLEX )
     kb_error(1629,"Cannot do Metis on simplex model.\n",RECOVERABLE);

  if ( parts < 2 )
     kb_error(3632,"Number of METIS partitions must be at least 2.\n",
         RECOVERABLE);

  
  nparts = parts;
  memset((char*)&graph,0,sizeof(graph));
  
  if ( web.representation == STRING )
  {
     int epart;
     int adj; /* total adjacencies */
     edge_id ee;

     /* get partition attribute */
     epart = find_attribute(EDGE,"epart");
     if ( epart < 0 ) 
     { int one = 1;
       epart = add_attribute(EDGE,"epart",INTEGER_TYPE,0,&one,1,NULL);
     }

     /* use partition attribute for contiguous numbering */
     k = 0;
     FOR_ALL_EDGES(e_id)
        ((int*)get_extra(e_id,epart))[0] = k++;

     /* count adjacencies */
     adj = 0;
     FOR_ALL_VERTICES(v_id)
     { int valence = 0;
        e_id = ee = get_vertex_edge(v_id);
        do{
          valence++;
          ee = get_next_tail_edge(ee);
          } while ( !equal_id(ee,e_id) );
        adj += (valence-1)*valence;
     }
     /* construct graph data */
     graph.xadj = (int*)temp_calloc(web.skel[EDGE].count+1,sizeof(int));
     graph.adjncy = (int*)temp_calloc(adj,sizeof(int));
     j = 0; i = 0;
     FOR_ALL_EDGES(e_id)
     { 
        ee = get_next_tail_edge(e_id);
        while ( !equal_id(ee,e_id) )
        { graph.adjncy[j++] = ((int*)get_extra(ee,epart))[0];
          ee = get_next_tail_edge(ee);
        }
        ee = get_next_head_edge(e_id);
        while ( !equal_id(ee,e_id) )
        { graph.adjncy[j++] = ((int*)get_extra(ee,epart))[0];
          ee = get_next_head_edge(ee);
        }
        graph.xadj[++i] = j;
     }

     /* do partition */
     options[0] = 0; /* take defaults */
     numbering = 0;
     graph.nvtxs = web.skel[EDGE].count;
     partition = (int*)temp_calloc(graph.nvtxs,sizeof(int));
     METIS_PartGraphKway(&graph.nvtxs, graph.xadj, graph.adjncy, NULL, NULL, 
          &weightflag,&numbering, &nparts, options, &edgecut, partition);
     /* label things */
     FOR_ALL_EDGES(e_id)
     { int *espot = ((int*)get_extra(e_id,epart));
       *espot = partition[*espot];
     }
  }
  else /* SOAPFILM */
  { 
     int fpart;
     int adj; /* total adjacencies */
     facet_id f_id;

     /* get facet partition attribute */
     fpart = find_attribute(FACET,"fpart");
     if ( fpart < 0 ) 
     { int one = 1;
       fpart = add_attribute(FACET,"fpart",INTEGER_TYPE,0,&one,1,NULL);
     }

     /* use partition attribute to hold contiguous ordinals */
     k = 0;
     FOR_ALL_FACETS(f_id)
        ((int*)get_extra(f_id,fpart))[0] = k++;

     /* count adjacencies */
     adj = 0;
     FOR_ALL_EDGES(e_id)
     { int valence = get_edge_valence(e_id);
       adj += (valence-1)*valence;
     }
     /* construct graph data */
     graph.xadj = (int*)temp_calloc(web.skel[FACET].count+1,sizeof(int));
     graph.adjncy = (int*)temp_calloc(adj,sizeof(int));
     j = 0; i = 0;
     FOR_ALL_FACETS(f_id)
     { facetedge_id start_fe = get_facet_fe(f_id);
       facetedge_id fe;
        
       for ( k = 0 ; k < FACET_EDGES ; k++ )
       { fe = get_next_facet(start_fe);
         while ( !equal_id(fe,start_fe) )
         { facet_id ff_id = get_fe_facet(fe); 
           graph.adjncy[j++] = ((int*)get_extra(ff_id,fpart))[0];
           fe = get_next_facet(fe);
         }
         start_fe = get_next_edge(start_fe);
       }
       graph.xadj[++i] = j;
     }

     /* do partition */
     options[0] = 0; /* take defaults */
     numbering = 0;
     graph.nvtxs = web.skel[FACET].count;
     partition = (int*)temp_calloc(graph.nvtxs,sizeof(int));
     METIS_PartGraphKway(&graph.nvtxs, graph.xadj, graph.adjncy, NULL, NULL, 
          &weightflag,&numbering, &nparts, options, &edgecut, partition);
     /* label things */
     FOR_ALL_FACETS(f_id)
     { int *fspot = ((int*)get_extra(f_id,fpart));
       *fspot = partition[*fspot];
     }
  }

  /* free storage */
  temp_free((char*)graph.xadj);
  temp_free((char*)graph.adjncy);
  temp_free((char*)partition);

} /* end metis_partition_dual */

/************************************************************************
*
* function: metis_partition_body()
*
* purpose: Use metis to partition bodies. Passes dual body-body
*             graph to metis.
*             For string model, treats vertices and edges as graph
*             nodes.
*/
struct bb_adj { int b1,b2; int weight; };

int adjcomp(struct bb_adj *a, struct bb_adj *b)
{ if ( a->b1 < b->b1 ) return -1;
  if ( a->b1 > b->b1 ) return  1;
  if ( a->b2 < b->b2 ) return -1;
  if ( a->b2 > b->b2 ) return  1;
  return 0;
}
void metis_partition_body(parts,algorithm)
int parts;
int algorithm; /* METIS_ or KMETIS_ */
{
  int i,m;
  GraphType graph;
  int *partition, options[10], edgecut, nparts, numbering;
  int adjcount; /* total adjacencies */
  facet_id f_id;
  int *bdy_to_inx;
  body_id *inx_to_bdy;
  int bcount = web.skel[BODY].count;
  int weightflag;
  struct bb_adj *adjlist;
  body_id b_id;
  int bod;
  int bpart;
  int inx,spot;

  if ( web.representation == STRING )
     kb_error(3770,"Cannot do body Metis in string model. \n",RECOVERABLE);

  if ( parts < 2 )
     kb_error(3771,"Number of METIS partitions must be at least 2.\n",
         RECOVERABLE);

  
  nparts = parts;
  memset((char*)&graph,0,sizeof(graph));

  bpart = find_attribute(BODY,"bpart");
  if ( bpart < 0 ) 
  { int one = 1;
    bpart = add_attribute(BODY,"bpart",INTEGER_TYPE,0,&one,1,NULL);
  }
  
  /* get contiguous indexes for bodies */
  bdy_to_inx = (int*)temp_calloc(web.skel[BODY].max_ord+1,sizeof(int));
  inx_to_bdy = (body_id*)temp_calloc(bcount,sizeof(body_id));
  inx = 0;
  FOR_ALL_BODIES(b_id)
  { int ord = ordinal(b_id);
    bdy_to_inx[ord] = inx;
    inx_to_bdy[inx] = b_id;
    inx++;
  }
  if ( inx != bcount )
    kb_error(3772,"internal error - body count disagreement \n",RECOVERABLE);
    

  /* gather adjacencies */
  adjlist = (struct bb_adj *)temp_calloc(2*web.skel[FACET].count+1,
                                    sizeof(struct bb_adj));
  adjcount = 0;
  FOR_ALL_FACETS(f_id)
  { body_id b_id = get_facet_body(f_id);
    body_id bb_id = get_facet_body(inverse_id(f_id));
    int ord1,ord2;

    if ( !valid_id(b_id) || !valid_id(bb_id) ) continue;
    ord1 = ordinal(b_id);
    ord2 = ordinal(bb_id);
    if ( ord1 == ord2 ) continue;
    /* metis wants adjacencies in both directions */
    adjlist[adjcount].b1 = bdy_to_inx[ord1];
    adjlist[adjcount].b2 = bdy_to_inx[ord2];
    adjlist[adjcount].weight = 1;
    adjcount++;
    adjlist[adjcount].b1 = bdy_to_inx[ord2];
    adjlist[adjcount].b2 = bdy_to_inx[ord1];
    adjlist[adjcount].weight = 1;
    adjcount++;
  }
  /* sort */
  qsort((void*)adjlist,adjcount,sizeof(struct bb_adj),FCAST adjcomp);
    
  /* uniquify and gather number of facets as weights */
  for ( i = 1, m = 0 ; i < adjcount ; i++ )
  { if ( (adjlist[i].b1 != adjlist[m].b1) || (adjlist[i].b2 != adjlist[m].b2) )
    { m++;
      adjlist[m] = adjlist[i];
    } 
    else
     adjlist[m].weight += adjlist[i].weight;
  }
  adjcount = m;
  adjlist = (struct bb_adj*)temp_realloc((char*)adjlist,
                   adjcount*sizeof(struct bb_adj));


  /* construct graph data */
  graph.xadj = (idxtype*)temp_calloc(bcount+1,sizeof(idxtype));
  graph.vwgt = (idxtype*)temp_calloc(bcount,sizeof(idxtype));
  graph.adjncy = (idxtype*)temp_calloc(adjcount,sizeof(idxtype));
  graph.adjwgt = (idxtype*)temp_calloc(adjcount,sizeof(idxtype));
  for ( bod = 0, spot = 0 ; spot < adjcount ; spot++ )
  { while ( adjlist[spot].b1 != bod )
    { bod++;
      graph.xadj[bod] = spot;
    }
    graph.adjncy[spot] = adjlist[spot].b2;
    graph.adjwgt[spot] = adjlist[spot].weight;
    graph.vwgt[bod] += adjlist[spot].weight;
  }
  bod++;
  graph.xadj[bod] = adjcount;
  temp_free((char*)adjlist);

  /* do partition */
  options[0] = 0; /* take defaults */
  numbering = 0;  /* C-style 0-based indexing */
  graph.nvtxs = bcount;
  weightflag = 3; /* both weights */
  partition = (int*)temp_calloc(graph.nvtxs,sizeof(int));
  METIS_PartGraphKway(&graph.nvtxs, graph.xadj, graph.adjncy, graph.vwgt, 
         graph.adjwgt, 
          &weightflag,&numbering, &nparts, options, &edgecut, partition);

  /* label things */
  for ( i = 0 ; i < bcount ; i++ )
   ((int*)get_extra(inx_to_bdy[i],bpart))[0] = partition[i];

  /* free storage */
  temp_free((char*)graph.xadj);
  temp_free((char*)graph.vwgt);
  temp_free((char*)graph.adjncy);
  temp_free((char*)graph.adjwgt);
  temp_free((char*)partition);
  temp_free((char*)bdy_to_inx);
  temp_free((char*)inx_to_bdy);
}

/************************************************************************
*
* function: metis_vertex_order()
*
* purpose: Use metis to order vertices for sparse factoring. 
*     Passes actual vertex-edge graph to metis.
*     Just demo.
*/
void metis_vertex_order(mmdswitch)
int mmdswitch;  /* size of subgraph to stop at */
{
  int i;
  GraphType graph;
  int  options[10], numbering;
  vertex_id v_id;
  edge_id e_id,start_e;
  int j;
  int *perm,*iperm;

  if ( web.representation == SIMPLEX )
     kb_error(1634,"Cannot do Metis on simplex model.\n",RECOVERABLE);

  if ( web.skel[VERTEX].count != web.skel[VERTEX].max_ord+1 )
     kb_error(1635,"Need packed vertex numbering to do Metis.\n",RECOVERABLE);
  memset((char*)&graph,0,sizeof(graph));

  /* construct graph data */
  graph.xadj = (int*)temp_calloc(web.skel[VERTEX].count+1,sizeof(int));
  graph.adjncy = (int*)temp_calloc(2*web.skel[EDGE].count,sizeof(int));
  j = 0; i = 0;
  FOR_ALL_VERTICES(v_id)
  { e_id = start_e = get_vertex_edge(v_id);
     do 
     { graph.adjncy[j++] = loc_ordinal(get_edge_headv(e_id)); 
        e_id = get_next_tail_edge(e_id);
     } while ( !equal_element(e_id,start_e) );
     graph.xadj[++i] = j;
  }

  /* do partition */
  numbering = 0;
  graph.nvtxs = web.skel[VERTEX].count;
  perm = (int*)temp_calloc(graph.nvtxs,sizeof(int));
  iperm = (int*)temp_calloc(graph.nvtxs,sizeof(int));

#ifndef METIS2
  options[0] = 1; /* enable options, METIS 4.0 version */
  options[1] = 3;  /* Sorted Heavy-Edge Matching */
  options[2] = 1;  /* Edge-based region growing */
  options[3] = 2;    /* One-sided node FM refinement */
  options[4] = 0;    /* debugging only */
  options[5] = 0;   /* no compression */
  options[6] = 0;   /* don't order dense columns last */
  options[7] = 1;   /* number of separators to find each step */
  METIS_NodeND(&graph.nvtxs, graph.xadj, graph.adjncy,&numbering,options,perm,iperm);
  puts("No tree available in METIS-4\n");
#else
  /* metis 2 with my modifications */
  options[0] = 1; /* enable options, METIS 2.0 version */
  options[1] = 100;  /* coarsen to */
  options[2] = 21;  /* MType SHEM */
  options[3] = 2;    /* IPType GGGP */
  options[4] = 13;    /* RType BGKLR */
  OMETIS(&graph.nvtxs, graph.xadj, graph.adjncy,&options,&numbering,
     perm,iperm,&mmdswitch);
  /* print separation tree (just a temporary thing */
  puts("node  subtree size         separator");
  for ( i = 0 ; i < graph.nvtxs ; i++ )
     if ( stree[i].nvtxs > 0 )
        printf("%4d.      %d        %d-%d \n",i,stree[i].nvtxs,stree[i].lo,
            stree[i].hi);
  myfree((char*)stree);
#endif
  /* free storage */
  temp_free((char*)graph.xadj);
  temp_free((char*)graph.adjncy);
  temp_free((char*)perm);
  temp_free((char*)iperm);

}


/************************************************************************
*
* function: metis_order()
*
* purpose: Use metis to order linear system for sparse factoring. 
*          Passes actual vertex-edge graph to metis.
*          Returns A_OFF-based permutation for ysmp.
*/
void metis_order(S)
struct linsys *S;  /* system to order */
{
  int i,k;
  GraphType graph;
  int  options[10], numbering;
  int j;
  int tot;

  memset((char*)&graph,0,sizeof(graph));

  /* construct graph data */
  /* metis requires full graph matrix, not just symmetric part */
  graph.xadj = (int*)temp_calloc(S->N+1,sizeof(int));
  graph.adjncy = (int*)temp_calloc(2*S->IA[S->N],sizeof(int));
  /* first have to count neighbors into graph.xadj */
  for ( i = 0 ; i < S->N ; i++ ) 
  { int end;
     graph.xadj[i] += S->IA[i+1] - S->IA[i] - 1;  /* not self */
     end = S->IA[i+1] - A_OFF;
     for ( j = S->IA[i]+1 - A_OFF ; j < end ; j++ )
         graph.xadj[S->JA[j] - A_OFF]++;
  }
  /* reset xadj to starts of intervals */
  for ( tot = 0, i = 0 ; i < S->N ; i++ )
  { int num = graph.xadj[i];
     graph.xadj[i] = tot;
     tot += num;
  }
  graph.xadj[S->N] = tot;
  /* insert adjacencies */
  for ( i = 0 ; i < S->N ; i++ )
  { int end = S->IA[i+1] - A_OFF;
     for ( j = S->IA[i]+1 - A_OFF ; j < end ; j++ )
     { graph.adjncy[graph.xadj[i]++] = S->JA[j] - A_OFF;
       graph.adjncy[graph.xadj[S->JA[j] - A_OFF]++] = i;
     }
  }
  /* reset xadj */
  for ( i = S->N  ; i > 0 ; i-- )
      graph.xadj[i] = graph.xadj[i-1];
  graph.xadj[0] = 0;

  /* do partition */
  options[0] = 0; /* default options */
  memset(options,0,sizeof(options));
  numbering = 0;
  graph.nvtxs = S->N;
  if ( !S->P )
     S->P = (int*)temp_calloc(graph.nvtxs,sizeof(int));
  if ( !S->IP )
     S->IP = (int*)temp_calloc(graph.nvtxs,sizeof(int));

  METIS_NodeND(&graph.nvtxs, graph.xadj, graph.adjncy,&numbering,options,
        S->P,S->IP);

  /* adjust zero based indexing */
  /* ysmp wants 1 based */
  if ( numbering == 0 )
    for ( k = 0 ; k < S->N ; k++ ) { S->P[k]++; S->IP[k]++; }

  S->flags |= S_ORDERFOUND;

#ifdef METIS2
  /* find length of stree list and clean up stree fields */
  S->maxsepsize = 0;
  for ( k = 0, S->streemax = 1, s = S->stree ; k <= S->streemax ; k++,s++ )
  { if ( s->nvtxs < 0 )
     { s->lo = s->hi = s->isleaf = 0;}
     s->u.info.size = 0;
     s->u.info.vlist = NULL;
     s->u.info.mat = NULL;
     if ( S->maxsepsize < (s->hi-s->lo) )
         S->maxsepsize = (s->hi-s->lo);
     if ( (s->nvtxs > 0) && !(s->isleaf & 1) ) 
        S->streemax = 2*k+1;
  }
#endif

#ifdef PRINTSEPTREE 
  /* print separation tree (just a temporary thing */
  for ( i = 0 ; i <= S->streemax ; i++ )
     if ( S-> [i].nvtxs > 0 )
        printf("%4d.  %d    %d-%d \n",i,S->stree[i].nvtxs,S->stree[i].lo,
            S->stree[i].hi);
#endif

  /* free storage */
  temp_free((char*)graph.xadj);
  temp_free((char*)graph.adjncy);

/*
  if ( !hessian_quiet_flag )
    tree_analyze(S);
*/

}

#endif

/***************************************************************************
*
* function: do_tree_factor()
*
* purpose: do actual factoring of matrix.  
*             Meant to be run in parallel in shared memory.
*             Each proc starts at end of stree and works back
*             by nproc-size steps, waiting until sons done.
*             Unfortunately, supernode blocks are not necessarily
*             dense in A, just the fill is.
*/

void do_tree_factor(S)
struct linsys *S;
{ int me = GET_THREAD_ID;
  int spot; /* stree index */
  int blocks;
  int i,j;
  size_t k;
  REAL *mat;
  int *work;  /* some workspace */
  size_t sepsize; /* size of separator */
  size_t heapsize; /* number of lists to merge */
  struct hp { int row;  /* which row entry from */
              int index; /* index into pJA */
              int col;  /* column, from pJA or sentinel */
            } *heap;

  work = (int*)temp_calloc(S->N*sizeof(int)+(S->maxsepsize+5)*sizeof(struct hp),1);

  blocks = S->streemax/nprocs;
  spot = blocks*nprocs + me;
  if ( spot > S->streemax ) spot -= nprocs;

  for ( ; spot >= 1 ; spot -= nprocs )  /* metis starts at stree[1] */
  { /* factor one node */
     SepNodeType *stree = S->stree + spot;  /* current node */
     SepNodeType *left = stree + spot,*right = left+1;  /* sons */
     SepNodeType *son;
     int *varlist; /* temporary variable list */
     int *vtop;      /* pointer into varlist */
     size_t varcount;
     struct hp h;
     int hspot,next;
     int kk;
     REAL *base; /* pivot row */
     int row;

     if ( stree->nvtxs <= 0 ) { stree->isleaf |= 2; continue; }
     if ( !stree->isleaf )
        /* busy wait for sons */
        while ( !left->isleaf || !right->isleaf ) ;

     sepsize = stree->hi - stree->lo;
     heapsize = stree->isleaf ? sepsize : sepsize+2;

     /* heap merge columns and sons */
     heap = (struct hp*) work;
     varlist = (int*)((char*)work+heapsize*sizeof(struct hp)); 
     /* populate heap */
     for ( k = 0 ; k < sepsize ; k++ )
     { 
       heap[k].row = (int)(stree->lo + k);
       heap[k].index = S->pIA[heap[k].row];
       heap[k].col = S->pJA[heap[k].index];
     }
     if ( !stree->isleaf)
     { size_t lsep = left->hi-left->lo;
       size_t rsep = right->hi - right->lo;
       if ( left->u.info.size > lsep )
       { heap[sepsize].row = -1;  /* left son */
         heap[sepsize].index = (int)lsep;
         heap[sepsize].col = left->u.info.vlist[lsep];
       }
       else
       { heap[sepsize].row = -3;
         heap[sepsize].index = 0;
         heap[sepsize].col = MAXINT;
       }
       if ( right->u.info.size > rsep )
       { heap[sepsize+1].row = -2;  /* right son */
         heap[sepsize+1].index = (int)rsep;
         heap[sepsize+1].col = right->u.info.vlist[rsep];
       }
       else
       { heap[sepsize+1].row = -3;
          heap[sepsize+1].index = 0;
          heap[sepsize+1].col = MAXINT;
       }
     }
     /* initial ordering */
     for ( k = 0 ; k < heapsize ; k++ )
     { int parent = (int)((k-1)>>1);
       hspot = (int)k;
       h = heap[hspot];          
       while ( hspot > 0 )
       { if ( h.col < heap[parent].col )
             { heap[hspot] = heap[parent]; 
               hspot = parent;
               parent = (parent-1)>>1;
             }
          else break;
       }
       heap[hspot] = h;
     }
     /* heap sort */
     vtop = varlist;
     *vtop = heap[0].col;
     while ( heap[0].row >= -2 )
     {
       if ( heap[0].col > *vtop )
           *(++vtop) = heap[0].col;  /* add to list */

       /* delete from heap */
       switch ( heap[0].row )
       { case -2:
             if ( heap[0].index+1 < (int)right->u.info.size )
             { /* have more */
               h.index = heap[0].index + 1;
               h.col = right->u.info.vlist[h.index];
               h.row = -2;
             }
             else
             { /* end of right son */
               h.row = -3;
               h.col = MAXINT;  /* sentinel */
             }
             break;
          case -1:
             if ( heap[0].index+1 < (int)left->u.info.size )
             { /* have more */
               h.index = heap[0].index + 1;
               h.col = left->u.info.vlist[h.index];
               h.row = -1;
             }
             else
             { /* end of left son */
               h.row = -3;
               h.col = MAXINT;  /* sentinel */
             }
             break;
          default:
           if ( heap[0].index+1 < S->pIA[heap[0].row+1] )
           { /* have more */
              h.index = heap[0].index + 1;
              h.col = S->pJA[h.index];
              h.row = heap[0].row;
           }
           else
             { /* end of left son */
               h.row = -3;
               h.col = MAXINT;  /* sentinel */
             }
             break;
       }          
       /* percolate up heap */
       hspot = 0;
       for (;;)
       { next = 2*hspot+1;
          if ( next >= (int)heapsize ) break;
          if ( (next+1 < (int)heapsize) && (heap[next+1].col < heap[next].col) )
             next++;  /* right son is lower */
          if ( h.col > heap[next].col )
          { heap[hspot] = heap[next];
             hspot = next;
             continue;
          }
          break;
       }
       heap[hspot] = h;
     }

     /* allocate variable list */
     stree->u.info.size = varcount = vtop - varlist + 1;
     stree->u.info.vlist = (int*)mycalloc(stree->u.info.size,sizeof(int));
     for ( k = 0 ; k < varcount ; k++ ) 
       stree->u.info.vlist[k] = varlist[k];
     for ( k = 0 ; k < varcount ; k++ )
       work[stree->u.info.vlist[k]] = (int)k;  /* inverse mapping */
      
     /* allocate matrix */
     stree->u.info.mat =(REAL*) 
       mycalloc(varcount*(varcount+1)/2,sizeof(REAL));

     /* fill matrix */
     base = stree->u.info.mat;
     for ( i = stree->lo ; i < stree->hi ; base+=(varcount-i-1+stree->lo),i++ )
     { for ( j = S->pIA[i] ; j < S->pIA[i+1] ; j++ )
          base[work[S->pJA[j]]] += S->pA[j];
       /* add in shift and metric */
       if ( hessian_linear_metric_flag )
       { if ( S->lambda != 0.0 )
          { int end = Met.pIA[i+1];
             for ( j = Met.pIA[i] ; j < end ; j++ )
               base[work[Met.pJA[j]]] -= S->lambda*Met.pA[j];
          }
       }
       else if ( web.area_norm_flag )
           base[i-stree->lo] -= S->lambda*Met.A[S->P[i]]; /* special metric */
       else base[i-stree->lo] -= S->lambda;
     }
     if ( !stree->isleaf )
     for ( son = left, kk = 0 ; kk < 2 ; kk++, son=right )
     { int sonsepsize = son->hi - son->lo;
       REAL *sonsrc = son->u.info.mat + sonsepsize*son->u.info.size
                              - (((sonsepsize-1)*sonsepsize)>>1);
       for ( k = sonsepsize ; k < son->u.info.size ; k++ )
       { row = work[son->u.info.vlist[k]];
          mat = stree->u.info.mat + row*varcount - (((row-1)*row)>>1) - row;
          for ( j = (int)k ; j < (int)son->u.info.size ; j++,sonsrc++ )
             mat[work[son->u.info.vlist[j]]] += *sonsrc;
       }
     }     

     /* factor */
     base = stree->u.info.mat;
     for ( row = 0 ; row < (int)sepsize ; row++, base += varcount-row )
     { REAL piv;
        if ( fabs(base[row]) <= hessian_epsilon) 
        { S->zero++; base[row] = 1.0; continue; }
        if ( base[row] > 0.0 ) 
            S->pos++; 
        else 
            S->neg++;
        piv = 1/base[row];
        mat = base + varcount;
        for ( i = row+1 ; i < (int)varcount ; i++ )
        { REAL q = base[i]*piv;
          for ( j = i ; j < (int)varcount ; j++ )
             *(mat++) -= q*base[j];
          base[i] = q;
        }
     }
     /* mark as done */
     stree->isleaf |= 2;
  }

  temp_free((char*)work);
}

/***************************************************************************
*
* function: tree_factor()
*
* purpose:  minimal degree factoring of system using tree decomp
*
*/
void tree_factor(S)
struct linsys *S;
{ 

  if ( S->N <= 0 )
  { kb_error(1638,"Empty linear system.\n",WARNING); 
    return;
  }
  S->neg = S->zero = S->pos = 0;
  S->degencon = 0;

  sparse_permute(S);
  if ( hessian_linear_metric_flag && (S->lambda != 0.0) )
  { memcpy((char*)Met.P,(char*)S->P,S->N*sizeof(int));
    memcpy((char*)Met.IP,(char*)S->IP,S->N*sizeof(int));
    sparse_permute(&Met);
  }


  /* this set up to be done in parallel */
#ifdef SGI_MULTI
  if ( mpflag == M_INACTIVE ) m_rele_procs();
  mpflag = M_ACTIVE;
  m_fork(do_tree_factor,S);
  m_park_procs();
  mpflag = M_INACTIVE;
#else
  do_tree_factor(S);
#endif

  if ( !hessian_quiet_flag )
  { /* A few statistics */
     REAL flops = 0.0;
     REAL critflops[MAXPROCS];  /* critical path flops */
     size_t fill = 0;
     size_t fillspace = 0;
     int k;

     for ( k = 0 ; k < nprocs ; k++ ) critflops[k] = 0.0;
     for ( k = 0 ; k < S->streemax ; k++ )
     { int sep = S->stree[k].hi - S->stree[k].lo;
       size_t size = S->stree[k].u.info.size;
       size_t rem = size - sep;
       size_t fl;

        if ( S->stree[k].nvtxs <= 0 ) continue;
        fillspace += size*(size+1)/2;
        fill += size*sep - sep*(sep-1)/2;
        fl = size*(size+1)*(size+2)/6 - rem*(rem+1)*(rem+2)/6;
        critflops[k % nprocs] += (REAL)fl;
        flops += (REAL)fl;
     }

     if ( nprocs > 1 )
     { REAL critpath = 0.0;
        outstring("Proc    Flops\n");
        for ( k = 0 ; k < nprocs ; k++ )
        { sprintf(msg,"%2d  %12g\n",k+1,(DOUBLE)critflops[k]); outstring(msg); }
        for ( k = 1 ; k <= S->streemax ; k *= 2 )
        { int sep = S->stree[k].hi - S->stree[k].lo;
          size_t size = S->stree[k].u.info.size;
          size_t rem = size - sep;
          critpath += (REAL)(size*(size+1)*(size+2)/6 - rem*(rem+1)*(rem+2)/6);
        }
        sprintf(msg,"Critical path flops: %12.0f\n",(DOUBLE)critpath);
        outstring(msg);
     }
     sprintf(msg,"Total fill: %d  Total flops(flop=mul+add): %g    Fillspace: %d\n",
         fill,(DOUBLE)flops,fillspace);
     outstring(msg);
  }
}

/*************************************************************************
* 
* function: do_tree_solve()
*
* purpose: nitty-gritty solution of system.
*          Uses stree; goes leaf-to-root to solve LY=B
*          then root-to-leaf to solve UX = Y.
*          NOT set up to run in parallel.
*          Parallel would need U stored columnwise instead of rowwise.
*/
void do_tree_solve(S,BB,Y)
struct linsys *S;
REAL *BB; /* incoming and outgoing */
REAL *Y;  /* intermediate */
{ SepNodeType *stree;
  int spot;
  int n;
  REAL *e;

  /* solve U^T Y = B */
  for ( spot = S->streemax; spot >= 1 ; spot-- )  /* metis starts at stree[1] */
  { stree = S->stree + spot;

     for ( n = stree->lo, e = stree->u.info.mat ; n < stree->hi ; n++ )
     { int start;
       REAL y;
       int i,*jp;

       y = Y[n] = BB[n]; 
       Y[n] /= *e; e++;  /* having saved Y[n], can divide by diag */
       start = n-stree->lo+1;
       for ( i=start, jp = stree->u.info.vlist+start ; 
                     i < (int)stree->u.info.size ; i++,e++,jp++ )
          BB[*jp] -= (*e)*y;
     }

  }

  /* solve U BB = Y */
  for ( spot = 1 ; spot <= S->streemax ; spot++ )
  { stree = S->stree + spot;
     for ( n = stree->hi-1 ; n >= stree->lo ; n-- )
     { int row,i,*jp;
        REAL y = Y[n];
        REAL *estart;
        row = n - stree->lo;
        estart = stree->u.info.mat + row*stree->u.info.size
          - ((row*(row-1))/2) - row;
        for ( i=row+1, jp=stree->u.info.vlist+i  ; 
            i < (int)stree->u.info.size ; i++,jp++ )
              y -= estart[i]*BB[*jp];
        BB[n] = y;
      }
  }
}

/*************************************************************************
*
* function: tree_solve()
* 
* purpose: solve factored system for given right hand side.
*             Factor stored in stree, permuted order
*
*/

void tree_solve(S,B,x)
struct linsys *S; /* factored system */
REAL *B;    /* incoming right hand side */
REAL *x;    /* solution, may be rhs */
{
  int n; /* row index */
  REAL *BB,*Y;


  BB = (REAL*)temp_calloc(S->N,sizeof(REAL));  /* intermediate solutions */
  Y = (REAL*)temp_calloc(S->N,sizeof(REAL));  /* intermediate solutions */

  for ( n = 0 ; n < S->N ; n++ ) BB[n] = B[S->P[n]]; /* permute */

  /* NOT set up for parallel */
  do_tree_solve(S,BB,Y);

  /* unpermute */
  for ( n = 0 ; n < S->N ; n++ )
     x[S->P[n]] = BB[n];

  temp_free((char*)Y);
  temp_free((char*)BB);
}


/*************************************************************************
*
* function: tree_solve_multi()
* 
* purpose: solve factored system for multiple right hand sides
*
*/

void tree_solve_multi(S,B,x,rk)
struct linsys *S; /* factored system */
REAL **B;    /* incoming right hand side */
REAL **x;    /* solution, may be rhs */
int rk;         /* number of right sides */
{
  int k;
  for ( k = 0 ; k < rk ; k++ )
     tree_solve(S,B[k],x[k]);
}

/************************************************************************
*
* function: tree_analyze()
*
* purpose: Try to reverse-engineer factor tree from order permutation.
*/

void tree_analyze(S)
struct linsys *S;
{ int k; /* end of block */
  int n; /* member of block */

  /* get things lined up in S->pIA and S->pJA so we can analyze */
  if ( S->pIA == NULL )
    sparse_permute(S);

  /* start at end, looking for dense blocks */
  for ( k = S->N - 1 ; k >= 0  ;  )
  { for ( n = k-1 ; n >= 0 ; n-- )
    { if ( S->pIA[n+1]-S->pIA[n] < k-n || S->pJA[S->pIA[n]+(k-n)] != k ) 
        break;
    }
    printf("Dense block %d to %d\n",n+1,k);
    k = n;
  }
}
