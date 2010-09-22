/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/
#include "include.h"

/*************************************************************
*
*     file:       mindeg.c
*
*     Purpose:    My own minimum degree sparse matrix factoring algorithm
*/

/* This file is basically the old version with compaction to take up
   less memory.
*/

/* Algorithm: at each stage, eliminated vertices fall into disjoint
   clusters, representing factored hessian of region with fixed boundary.
   Vertex of minimal degree is eliminated, sometimes resulting in 
   merging of clusters. */
 
/* clusters or regions also called cliques in the literature */

/* Nodes are gathered into supernodes, defined as having same set of
   cliques.  Supernodes are ordered by degree, and independent 
   supernodes are eliminated in degree order until reach lower bound
   estimate for next round.  Independence means not sharing region.
*/
/* This version has supernodes as persistent structures */

/* data structures */

static struct md_vertex { 
                     int supernode; /* which it belongs to */
                     int timestamp;  
                     size_t newspot; /* for merging region matrices */
                  } *vlist;

struct region { int vercount;  /* number of boundary vertices */
                int verlist;  /* vertex list spot, dynamically allocated */
                int verpnum;  /* vertex positions in parent */
                int supercount; /* number of boundary supernodes */
                int superlist; /* boundary supernode list spot */
                int timestamp; /* marking for independence */
                int merged;     /* region merged with, or self */
                int son; /* for start of merge list */ 
                int size;  /* number of vertices, incl eliminated */
#define BROTHER superlist
                /* for merging to parent */
                int next;  /* doubly linked list of actives */
                int prev;  /* doubly linked list of actives */
             } ;
static int regionstart; /* start of active list */
static  int last_region;  /* end of region chain */

/* supernode is collection of equivalent vertices */
/* defined as having same set of regions */

static struct supernode { int rcount;  /* number of regions */
                          int rlist;    /* list of regions spot */
                          int degree;
                          int flag;     /* see below */
                          float height; /* for ordering of elimination */
                          int verlist;  /* list of vertices spot */
                          int vercount; /* number of vertices */
                          int rtimestamp; /* for marking on changed region */
                          int stimestamp; /* for marking touched */
                          int next;  /* doubly linked list of actives */
                          int prev;  /* doubly linked list of actives */
                        } *slist ;
/* flag bits */
#define MD_AUG_CON 1

static int superstart; /* start of active list */

static int margin;  /* how high to go above minimum degree, command line -m */
static int total_fill;  /* total number of fill in factored matrix */
static REAL total_flops; /* total operation count, counting mul+add as 2 */
static int passes;  /* number of times through main loop */

void dsolve();
extern int mindeg_debug_level;  /*
                          0  none
                          1  print lowest degree
                          2  print supernode sizes
                          3  print vertex elim
                          4  print region absorbing
                          5  print region merging
                      */
static int minabsorb = 2; /* max size region to try to absorb */
static int vtimestamp;  /* for marking vertices */
static int stimestamp;  /* for marking supernodes */
static int rtimestamp; /* for marking regions for independence */ 
static int old_rtimestamp; /* for saving previous value */ 

extern int mindeg_margin;  /* how high to go above minimum degree, user var */
extern int mindeg_min_region_size; /* merge smaller regions with parent */

static int total_fill;  /* total number of fill in factored matrix */
static int L_total;  /* allocated spaces for final trianuglar matrix */
static int elim_count; /* number of variables eliminated so far */

/* macros for conveniently accessing working storage */
#define REG(spot) ((struct region *)(S->ISP+(spot)))
#define SNODE(spot) ((struct supernode *)(S->ISP+(spot)))
#define INT(spot)  (S->ISP+(spot))
#define CHAR(spot)  ((char*)(S->ISP+(spot)))

/* trial stuff */
int K;  /* current row number */
int *xIL;  /* next uneliminated vertex */
int *xJL;  /* lists of rows to be added to uneliminated rows: 
                  i > K => xJL(i) is first row to be added to row i,
                  i < K => xJL(i) is row following row i in some list of rows.
                  xJL(i) = -1 indicates end of list */ 

int md_alloc ARGS((struct linsys *, int));
int region_alloc ARGS((struct linsys *));
int supernode_compare ARGS((struct supernode **,struct supernode **));
int degree_compare ARGS((struct supernode *,struct supernode *));
void do_region_absorb ARGS((struct linsys *,int,int));
int exact_degree ARGS((struct linsys *,struct supernode *));
int exact_sdegree ARGS((struct linsys *,struct supernode *));
void  traverse_recur ARGS((struct linsys *,struct region *,int*,int*));
void  permute_recur ARGS((struct linsys *,struct region *));
void  factor_recur ARGS((struct linsys *,struct region *));
void  sparse_permute ARGS((struct linsys *));
void md_vertex_setup ARGS((struct linsys *));
void md_supernode_setup ARGS((struct linsys *));
void md_region_string ARGS((struct linsys *));
void md_supernode_regions ARGS((struct linsys *));
void degree_sort ARGS((struct linsys *));
void multiple_eliminate ARGS((struct linsys *));
void region_absorb ARGS((struct linsys *));
void clean_supernodes ARGS((struct linsys *));
void merge_supernodes ARGS((struct linsys *));
void traverse_region_tree ARGS((struct linsys *));
void mass_eliminate ARGS((struct linsys *,struct supernode*));
void show_mindeg_state ARGS((struct linsys *));

struct supernode **superheap;  /* for heap ordering */
static int heapcount;  /* how many in heap */
static struct supernode sentinel;

/**************************************************************************
 *
 * function: show_mindeg_state()
 * 
 * purpose: Do graphical display of current factorization state.
 *          Sets all edges between non-eliminated vertices to red,
 *          others to black.  Pauses for user response.
 */
void show_mindeg_state(S)
struct linsys *S;
{ char *marks;  /* unelim node numbers */
  edge_id e_id;
  int j;
  int rcount;
  struct region *r;
  int rnum;
  int snum;
  struct supernode *s;
  
  /* count regions */
  for ( rnum = regionstart, rcount=0 ; rnum >= 0 ; rnum = r->next )
  { r = REG(rnum);
    rcount++;
  }

  /* figure out which vertices are not eliminated */
  marks = temp_calloc(S->N,1);
  for ( snum = superstart ; snum >= 0 ; snum = s->next )
  { s = slist + snum; 
    for ( j = 0 ; j < s->vercount ; j++ )
      marks[INT(s->verlist)[j]] = 1;
  }
  FOR_ALL_EDGES(e_id)
  { struct hess_verlist *vh = get_vertex_vhead(get_edge_headv(e_id));
    struct hess_verlist *vt = get_vertex_vhead(get_edge_tailv(e_id));
    if ( marks[vh->rownum] && marks[vt->rownum] )
      set_edge_color(e_id,RED);
    else set_edge_color(e_id,WHITE);
  }
  update_display();
  sprintf(msg,"Elimcount %d of %d;  regions %d\n",elim_count,S->N,rcount);
  prompt(msg,errmsg,100);
  temp_free(marks);
}


/*****************************************************************************
*
* function: md_alloc()
*
* purpose: allocate workspace in linsys ISP. ISP[0] is amount allocated in ints.
*          No deallocation. No initialization. NSP is arena size in ints.
*          User must allocate ISP and set NSP before first call.
* return:  offset from start
*/
int md_alloc(S,bytes)
struct linsys *S;
int bytes; /* bytes wanted */
{ int spot; 
  int size; /* in ints */

  size = (bytes+3)/sizeof(int);
  if ( S->ISP[0] + size + 1 > S->NSP )
  { int newsize = (int)(1.5*S->NSP)+bytes;
    S->ISP = (int*)temp_realloc((char*)S->ISP,newsize*sizeof(int));
    S->NSP = newsize;
  }
  spot = S->ISP[0] + 1;
  S->ISP[0] += size;
  return spot;
}
/***************************************************************************
*
* function: region_alloc()
*
* purpose: allocate new region structure in workspace
*
*/
int region_alloc(S)
struct linsys *S;
{ int spot;
  struct region *r;

  spot = md_alloc(S,sizeof(struct region));
  if ( last_region >= 0 ) REG(last_region)->next = spot;
  else regionstart = spot;
  r = REG(spot);
  r->prev = last_region;
  r->next = -1;
  r->merged = spot;  /* self */
  r->son = -1;
  last_region = spot;
  return spot;
}

/*****************************************************************************
*
* function: md_vertex_setup()
*
* purpose: initialize matrix vertex list, one vertex per matrix row,
*          and establish mapping between surface elements and rows.
*             
*/
 
void md_vertex_setup(S)
struct linsys *S;
{ int i,k;
  struct hess_verlist *vh;
  vertex_id v_id;
  body_id b_id;
  
  vlist = (struct md_vertex *)temp_calloc(S->N,sizeof(struct md_vertex));

  /* set up reverse indexing from vlist to vheads */
  FOR_ALL_VERTICES(v_id)
  { vh = get_vertex_vhead(v_id);
     for ( k = 0 ; k < vh->freedom ; k++ )
       vlist[vh->rownum+k].newspot = vh - vhead;
  }
  for ( i = 0 ; i < optparamcount ; i++ )
  { vh = vhead + optparam[i].vhead_index;
    vlist[vh->rownum].newspot = vh - vhead;
  }
  if ( augmented_hessian_mode )
  { 
    if ( !everything_quantities_flag ) /* bodies */
    { FOR_ALL_BODIES(b_id)
      { vh = get_body_vhead(b_id);
        if ( vh->freedom )
          vlist[vh->rownum].newspot = vh - vhead;
      }
    }
    for ( i=0 ; i < gen_quant_count ; i++ )
    { vh = vhead + GEN_QUANT(i)->vhead_index;
      if ( vh->freedom )
        vlist[vh->rownum].newspot =  vh - vhead;
    }
  }

}

/*****************************************************************************
*
* function: md_region_string()
*
* purpose: initialize region list with one region per edge, 
*/

void md_region_string(S)
struct linsys *S;
{ int j,k,p,q;
  size_t n1,n2;
  int rcount;
  struct region *r;
  char *entrymark;  /* for tagging done entries of A */
  int size;
  int newspot,spot;  /* index into workspace */
  size_t n1_end;

  entrymark = temp_calloc(S->IA[S->N],sizeof(char));

  /* estimate regions needed */
  regionstart = md_alloc(S,0);      /* since may need realloc for vertices */
  last_region = -1;
  for ( j = 0 ; j < vhead_count ; j++ ) vhead[j].flags &= ~HV_DONE;

  /* fill in one region per edge detected in array */
  rcount = 0;
  for ( p = 0 ; p < S->N ; p++ )
  { for ( q =  S->IA[p]-A_OFF ; q < S->IA[p+1]-A_OFF ; q++ )
    { int s1,s2;

      if ( entrymark[q] ) continue;
      n1 = vlist[p].newspot;
      n2 = vlist[S->JA[q]-A_OFF].newspot;
      if ( n1 == n2 ) continue;  /* don't do just one vertex here */

      if ( n1 > n2 ) { size_t tmp = n1; n1 = n2; n2 = tmp; }

      /* need new region */
      spot = region_alloc(S);
      r = REG(spot);

      /* find its supernodes */
      s1 = vlist[vhead[n1].rownum].supernode;
      s2 = vlist[vhead[n2].rownum].supernode;
      r->supercount = (s1==s2) ? 1 : 2;
      newspot = md_alloc(S,r->supercount*sizeof(int));
      r = REG(spot);  /* in case of reallocation of workspace */
      r->superlist = newspot;
      INT(r->superlist)[0] = s1;
      if ( s1 != s2 )
        INT(r->superlist)[1] = s2;

      size = vhead[n1].freedom + vhead[n2].freedom;
      r->size = r->vercount = size;
      newspot = md_alloc(S,r->vercount*sizeof(int));
      r = REG(spot);  /* in case of reallocation of workspace */
      r->verlist = newspot;
      for ( k = 0 ; k < vhead[n1].freedom ; k++ )
          INT(r->verlist)[k] = vhead[n1].rownum + k;
      for ( k = 0 ; k < vhead[n2].freedom ; k++ )
          INT(r->verlist)[vhead[n1].freedom+k] = vhead[n2].rownum + k;

      /* the edge */
      for ( k = 0 ; k < vhead[n1].freedom ; k++ )
      { int row;
        row = vhead[n1].rownum + k;
        for ( spot = S->IA[row]-A_OFF ; spot < S->IA[row+1]-A_OFF ; spot++ )
           if ( S->JA[spot]-A_OFF >= vhead[n2].rownum ) break;
        /* entry may not exist if value 0 */
        for ( j = 0 ; (spot<S->IA[row+1]-A_OFF) && (j<vhead[n2].freedom) ; j++ )
        {
          if ( S->JA[spot]-A_OFF > vhead[n2].rownum+j ) continue;
          entrymark[spot] = 'x';
          spot++;
        }
      }
      /* the vertices */
      if ( !(vhead[n1].flags & HV_DONE) )
      { for ( k = 0 ; k < vhead[n1].freedom ; k++ )
        { int row;
          row = vhead[n1].rownum + k;
          for ( spot = S->IA[row]-A_OFF ; spot < S->IA[row+1]-A_OFF ; spot++ )
            if ( S->JA[spot]-A_OFF >= vhead[n1].rownum ) break;
          for ( j=0 ; (spot<S->IA[row+1]-A_OFF) && (j<vhead[n1].freedom) ; j++ )
          { if ( S->JA[spot]-A_OFF > vhead[n1].rownum+j ) continue;
            entrymark[spot] = 'x';
            spot++;
          }
        }
        vhead[n1].flags |= HV_DONE;
      }
      if ( !(vhead[n2].flags & HV_DONE) )
      { for ( k = 0 ; k < vhead[n2].freedom ; k++ )
        { int row;
          row = vhead[n2].rownum + k;
          for ( spot = S->IA[row]-A_OFF ; spot < S->IA[row+1]-A_OFF ; spot++ )
            if ( S->JA[spot]-A_OFF >= vhead[n2].rownum ) break;
          for ( j = 0 ; (spot<S->IA[row+1]-A_OFF)&&(j<vhead[n2].freedom) ; j++ )
          { if ( S->JA[spot]-A_OFF > vhead[n2].rownum+j ) continue;
            entrymark[spot] = 'x';
            spot++;
          }
        }
        vhead[n2].flags |= HV_DONE;
      }

      rcount++;
    }
  }

  /* now see if any vertices have been missed */
  n1_end = augmented_hessian_mode ? vhead_count : 
        web.skel[VERTEX].max_ord+1 + optparamcount;
  for ( n1 = 0 ; n1 < n1_end ; n1++ )
  { if ( vhead[n1].flags & HV_DONE ) continue;
    if ( vhead[n1].freedom == 0 ) continue;

    /* need new region */
    spot = region_alloc(S);
    r = REG(spot);

    r->supercount = 1;
    newspot = md_alloc(S,r->supercount*sizeof(int));
    r = REG(spot);  /* in case of reallocation of workspace */
    r->superlist = newspot;
    INT(r->superlist)[0] = vlist[vhead[n1].rownum].supernode;
    size = vhead[n1].freedom;
    r->size = r->vercount = size;
    newspot = md_alloc(S,r->vercount*sizeof(int));
    r = REG(spot);  /* in case of reallocation of workspace */
    r->verlist = newspot;
    for ( k = 0 ; k < vhead[n1].freedom ; k++ )
      INT(r->verlist)[k] = vhead[n1].rownum + k;

    /* fill matrix */
    for ( k = 0 ; k < vhead[n1].freedom ; k++ )
    { int row;
      row = vhead[n1].rownum + k;
      for ( spot = S->IA[row]-A_OFF ; spot < S->IA[row+1]-A_OFF ; spot++ )
         if ( S->JA[spot]-A_OFF >= vhead[n1].rownum ) break;
      for ( j=0 ; (spot<S->IA[row+1]-A_OFF) && (j<vhead[n1].freedom) ; j++ )
      { if ( S->JA[spot]-A_OFF > vhead[n1].rownum+j ) continue;
        entrymark[spot] = 'x';
        spot++;
      }
    }
    vhead[n1].flags |= HV_DONE;
    rcount++;
  }

  temp_free(entrymark);
}

/*****************************************************************************
*
* function: md_region_soapfilm()
*
* purpose: initialize region list with one region per facet or edge
*             Film quadratic mode will need per facet.
*/

#ifdef FACETREGIONS
void md_region_soapfilm(S)
struct linsys *S;
{ int i,j,k,n;
  int ecount;
  struct md_vertex *v;
  struct region *r;
}
#endif

/*****************************************************************************
*
* function: md_supernode_regions()
*
* purpose: set up lists of regions per supernode
*/
void md_supernode_regions(S)
struct linsys *S;
{ int k,rnum,snum;
  struct region *r;
  struct supernode *s;

  /* count number of correspondences needed */
  for ( rnum = regionstart ; rnum >= 0 ; rnum = r->next )
  { r = REG(rnum);
     for ( k = 0 ; k < r->supercount ; k++ )
        slist[INT(r->superlist)[k]].rcount++;
  }

  for ( snum = superstart ; snum >= 0 ; snum = s->next )
  { s = slist + snum;
    s->rlist = md_alloc(S,s->rcount*sizeof(int));
    s->rcount = 0;  /* reset so can use as index in loading */
  }
  for ( rnum = regionstart; rnum >= 0 ; rnum = r->next )
  { r = REG(rnum);
    for ( k = 0 ; k < r->supercount ; k++ )
    { s = slist + INT(r->superlist)[k];
      INT(s->rlist)[s->rcount++] = rnum;
    } 
  }
}

/***************************************************************************
*
* function: md_supernode_setup()
*
* purpose: Create one supernode per vertex. Does not set regions.
*/
int supercount;

void md_supernode_setup(S)
struct linsys *S;
{ int n,i,j;
  struct supernode *s;
  int vhead_hi;
  struct hess_verlist *vh;
 
  vhead_hi = augmented_hessian_mode ? vhead_count : 
        web.skel[VERTEX].max_ord+1 + optparamcount;

  /* allocate supernode list */
  for ( n = 0, supercount = 0; n < vhead_hi ; n++ )
     if ( vhead[n].freedom > 0 ) supercount++;
  slist = (struct supernode *)temp_calloc(supercount,sizeof(struct supernode));
  s = slist;
  supercount = 0;

  /* Higher-order Lagrange edge and facet innards */
  if ( web.lagrange_order >= 3 )  /* do edge innards */
  { edge_id e_id;
    vertex_id *v;
    FOR_ALL_EDGES(e_id)
    { int df; /* total degrees of freedom on edge */
      v = get_edge_vertices(e_id);
      for ( j = 1, df = 0 ; j < web.lagrange_order ; j++ )
      { vh = get_vertex_vhead(v[j]); 
        df += vh->freedom;
        vh->flags |= SUPERNODE_DONE;
      }
      if ( df == 0 ) continue;
      s->verlist = md_alloc(S,df*sizeof(int));
      for ( j = 1 ; j < web.lagrange_order ; j++ )
      { vh = get_vertex_vhead(v[j]);
        if ( vh->freedom > 0 )
        { 
          for ( i = 0 ; i < vh->freedom ; i++ )
          { INT(s->verlist)[s->vercount+i] = vh->rownum + i;
            vlist[INT(s->verlist)[s->vercount+i]].supernode = supercount;
          }
          s->vercount += vh->freedom;
        }
      }
      s->next = supercount+1;
      s->prev = supercount-1;
      supercount++;
      s++; 
    }
  } /* end edge innards */

  /* do facet innards */
  if ( (web.lagrange_order >= 4) && (web.representation == SOAPFILM) )  
  { facet_id f_id;
    vertex_id *v;
    int ctrl = (web.lagrange_order+1)*(web.lagrange_order+2)/2;

    FOR_ALL_FACETS(f_id)
    { int df; /* total degrees of freedom on facet interior */
      v = get_facet_vertices(f_id);
      for ( j = 1, df = 0 ; j < ctrl ; j++ )
      { if ( !(get_vattr(v[j]) & Q_MIDFACET) ) continue;
        vh = get_vertex_vhead(v[j]); 
        df += vh->freedom;
        vh->flags |= SUPERNODE_DONE;
      }
      if ( df == 0 ) continue;
      s->verlist = md_alloc(S,df*sizeof(int));
      for ( j = 1 ; j < ctrl ; j++ )
      { if ( !(get_vattr(v[j]) & Q_MIDFACET) ) continue;
        vh = get_vertex_vhead(v[j]);
        for ( i = 0 ; i < vh->freedom ; i++ )
        { INT(s->verlist)[s->vercount+i] = vh->rownum + i;
          vlist[INT(s->verlist)[s->vercount+i]].supernode = supercount;
        }
        s->vercount += vh->freedom;
      }
      s->next = supercount+1;
      s->prev = supercount-1;
      supercount++;
      s++; 
    }
  } /* end facet innards */

  /* now all single vertices and miscellaneous */
  for ( n = 0 ; n < vhead_hi ; n++ )
    if ( vhead[n].freedom > 0 && !(vhead[n].flags & SUPERNODE_DONE) )
    { s->vercount = vhead[n].freedom;
      s->verlist = md_alloc(S,s->vercount*sizeof(int));
      for ( i = 0 ; i < vhead[n].freedom ; i++ )
      { INT(s->verlist)[i] = vhead[n].rownum + i;
        vlist[INT(s->verlist)[i]].supernode = supercount;
      }
      /* s->height = get_coord(vhead[n].v_id)[0];*/ /* not so good */
      s->height = (float)n; /* pretty good */
      if ( (s->vercount == 1) && (S->A[S->IA[s->verlist]-A_OFF] == 0.0) )
          s->flag |= MD_AUG_CON;  /* so don't eliminate constraints first */
      s->next = supercount+1;
      s->prev = supercount-1;
      supercount++;
      s++; 
    }
  slist[0].prev = -1;
  slist[supercount-1].next = -1;
  superstart = 0;
}

/****************************************************************************
*
* function: clean_supernodes()
*
* purpose: eliminate merged regions from supernode region lists
*/
void clean_supernodes(S)
struct linsys *S;
{ struct supernode *s;
  int i,j,k,m;
  int n;
  for ( n = superstart ; n >= 0 ; n = s->next )
  { s = slist + n;
    for ( i=0,j=0 ; i < s->rcount ; i++ )
    { int rnum = INT(s->rlist)[i];
      while ( REG(rnum)->merged != rnum ) rnum = REG(rnum)->merged;
      if ( REG(rnum)->timestamp > old_rtimestamp )
        s->rtimestamp = REG(rnum)->timestamp;  /* mark as affected */
      for ( k = 0 ; k < j ; k++ )
      { if ( INT(s->rlist)[k] == rnum ) break; /* duplicate */
        if ( INT(s->rlist)[k] > rnum ) /* insert */
           { for ( m = j ; m > k ; m-- ) INT(s->rlist)[m] = INT(s->rlist)[m-1];
                INT(s->rlist)[k] = rnum; 
                j++;
                break;
           }
      }
      if ( k == j ) { INT(s->rlist)[j++] = rnum; }
    }
    s->rcount = j;
  }
}

/****************************************************************************
*
* function: supernode_compare()
*
* purpose: lexical comparison of supernode region lists
* return  -1 for a < b, 0 for a == b, 1 for a > b
*/
struct linsys *SSS;  /* for communication with compare routine */
int supernode_compare(aa,bb)
struct supernode **aa,**bb;
{ int k;
  struct linsys *S;
  struct supernode *a = *aa, *b = *bb;
  if ( a->rcount < b->rcount ) return -1;
  if ( a->rcount > b->rcount ) return 1;
  S = SSS;
  for ( k = 0 ; k < a->rcount ; k++ )
  { if ( INT(a->rlist)[k] < INT(b->rlist)[k] ) return -1;
     if ( INT(a->rlist)[k] > INT(b->rlist)[k] ) return  1;
  }
  return 0;
}

/****************************************************************************
*
* function: merge_supernodes()
*
* purpose: merge supernodes with same regions
*/
void merge_supernodes(S)
struct linsys *S;
{
  struct supernode **sslist,*s,*ss;
  int n,k,j,i;
  int count;
  int newverlist;

  if ( superstart < 0 ) { supercount = 0; return; } /* all done */

  /* make list to sort */
  sslist = (struct supernode **)temp_calloc(supercount,sizeof(struct supernode *));
  /* fill list */
  for ( n = superstart, count = 0 ; n >= 0 ; n = s->next )
  { s = slist + n;  /* only do touched supernodes */
     if ( s->rtimestamp > old_rtimestamp ) sslist[count++] = s;
  }
  /* sort */
  SSS = S; /* for communication with compare routine */
  qsort((char*)sslist,count,sizeof(struct supernode *),FCAST supernode_compare);
  /* merge */
  for ( n = 1, k = 0 ; n < count ; n++ )
  { s = sslist[k];
    if ( supernode_compare(sslist+n ,&s) == 0 )
    { /* merge */
      ss = sslist[n];
      if ( mindeg_debug_level > 7 )
        printf("Merging supernode %d into %d.\n",ss-slist,s-slist);
      newverlist = md_alloc(S,(s->vercount+ss->vercount)*sizeof(int));
      memcpy(CHAR(newverlist),CHAR(s->verlist),s->vercount*sizeof(int));
      memcpy(CHAR(newverlist+s->vercount),CHAR(ss->verlist),
                ss->vercount*sizeof(int));
      s->verlist = newverlist;
      s->vercount += ss->vercount;
      s->flag = 0;
      if ( s->height < ss->height ) s->height = ss->height;
      ss->verlist = -1;
      ss->vercount = 0;
      ss->flag = 0;
      if ( ss->next >= 0 ) slist[ss->next].prev = ss->prev;
      if ( ss->prev >= 0 ) slist[ss->prev].next = ss->next;
      else superstart = ss->next;
      supercount--;

      /* remove from region supernode list */
      for ( j = 0 ; j < ss->rcount ; j++ )
      { struct region *r = REG(INT(ss->rlist)[j]);
        for ( i = 0 ; i < r->supercount ; i++ )
           if ( INT(r->superlist)[i] == (sslist[n]-slist) ) break;
        r->supercount--;
        for ( ; i < r->supercount ; i++ )
           INT(r->superlist)[i] = INT(r->superlist)[i+1];
      }
    }
    else /* keep supernode */
    { k = n;
    }
  }
  temp_free((char*)sslist);
}

/****************************************************************************
*
* function do_region_absorb(keeper,goner)
*
* purpose: absorb one region into another
*
*/
void do_region_absorb(S,keeper,goner)
struct linsys *S;
int keeper,goner; /* the regions */
{ int n;
  struct region *rk = REG(keeper),*rg = REG(goner);

  /* find spots in keeper matrix */
  for ( n = 0 ; n < rk->vercount ; n++ )
     vlist[INT(rk->verlist)[n]].newspot = n;
  rg->merged = keeper;
  if ( rg->next >= 0 ) REG(rg->next)->prev = rg->prev; 
  else last_region = rg->prev;
  if ( rg->prev >= 0 ) REG(rg->prev)->next = rg->next;
  else regionstart = rg->next;
  rg->BROTHER = rk->son;
  rk->son = goner;
/*  rg->size = rg->vercount; */
}

/****************************************************************************
*
* function region_absorb()
*
* purpose: see if any regions entirely contained in others
*
*/

void region_absorb(S)
struct linsys *S;
{ int i,j,k;
  struct region *r;
  struct supernode *s;
  int absorbcount = 0;
  int maxab = 0; 

  for ( i = regionstart ; i >= 0 ; i = r->next )
  { r = REG(i);
    if ( r->supercount > minabsorb ) continue;
    /* timestamp supernodes of r */
    stimestamp++;
    for ( j = 0 ; j < r->supercount ; j++ )
       slist[INT(r->superlist)[j]].stimestamp = stimestamp;
    /* pick a supernode */
    s = slist + INT(r->superlist)[0];
    /* test regions of s */
    for ( k = 0 ; k < s->rcount ; k++ )
    { int rnum = INT(s->rlist)[k];
      struct region *rr;
      int count;
        
      while ( REG(rnum)->merged != rnum ) rnum = REG(rnum)->merged;
      rr = REG(rnum);
      if ( rr->supercount <= r->supercount ) continue;
      for ( j = 0, count = 0 ; j < rr->supercount ; j++ )
        count += (slist[INT(rr->superlist)[j]].stimestamp == stimestamp);
      if ( count == r->supercount ) /* have it */
      {
        if ( r->supercount > maxab ) maxab = r->supercount;
        do_region_absorb(S,rnum,i);
        if ( mindeg_debug_level >= 5 )
           printf("Absorbing region %d with %d.\n",i,rnum);
        absorbcount++;
        break;
      }
    }
  }
  if ( mindeg_debug_level > 1 ) 
      printf("Absorbed %d; max size %d\n",absorbcount,maxab);
}

/****************************************************************************
*
* function: degree_compare()
*
* purpose: compare supernode degrees, breaking ties by verlist spot.
*/

int degree_compare(s1,s2)
struct supernode *s1,*s2;
{ int diff = s1->degree - s2->degree;

  if ( s1->flag & MD_AUG_CON )
  { if ( s2->flag & MD_AUG_CON ) 
      return (s1->height < s2->height) ? -1 : 1;  
    else return 1;
  }
  if ( s2->flag & MD_AUG_CON ) return -1;
  if ( diff ) return diff;
  return (s1->height < s2->height) ? -1 : 1;  
  /* high order, since high first */
}

/**************************************************************************
*
* function: exact_degree()
*
* purpose: calculate exact external degree of supernode as union of
*             component regions.  Union is calculated by timestamping
*             vertices.
* return:  number of vertices in union.
*/

int exact_degree(S,s)
struct linsys *S;
struct supernode *s;
{ int i,j,degree;
  struct region *r;
  struct supernode *ss;

  stimestamp += 2; /* for current supernode */
  SSS = S;
  for ( i = 0, degree = 0 ; i < s->rcount ; i++ )
  { r = REG(INT(s->rlist)[i]);
    for ( j = 0 ; j < r->supercount ; j++ )
    { ss = slist + INT(r->superlist)[j];
      if ( ss->stimestamp < stimestamp )
      { degree += ss->vercount ; ss->stimestamp = stimestamp; }
    }
  }
  return degree - s->vercount;
}


/**************************************************************************
*
* function: exact_sdegree()
*
* purpose: calculate exact supernode degree of supernode as union of
*             component regions.  Union is calculated by timestamping
*             supernode.
* return:  number of supernodes in union, including self.
*/

int exact_sdegree(S,s)
struct linsys *S;
struct supernode *s;
{ int i,j,degree;
  struct region *r;
  struct supernode *ss;

  stimestamp++; /* for current supernode */
  for ( i = 0, degree = 0 ; i < s->rcount ; i++ )
  { r = REG(INT(s->rlist)[i]);
    for ( j = 0 ; j < r->supercount ; j++ )
    { ss = slist + INT(r->superlist)[j];
      if ( ss->stimestamp < stimestamp )
      { degree++ ; ss->stimestamp = stimestamp; }
    }
  }
  return degree;
}

/****************************************************************************
*
* function: degree_sort()
*
* purpose: order supernodes by degree. Degree is external degree, the number
* of nodes bordering the region left after removing the supernode.
*/

void degree_sort(S)
struct linsys *S;
{ int n;
  struct supernode *s;
  int spot;

  heapcount = 0; 
  for ( n = superstart ; n >= 0 ; n = s->next )
  { s = slist + n;
    if ( s->rcount == 0 ) continue; /* empty */
    /* calculate degree */
    if ( s->rtimestamp > old_rtimestamp ) 
      s->degree = exact_degree(S,s);
    /* insert in heap */
    spot = ++heapcount;    /* using heap[1] as root for convenience */
    while ( spot>1 ) /* filter down */
    { 
      if ( degree_compare(s,superheap[spot>>1]) < 0 )
        superheap[spot] = superheap[spot>>1];
      else break;
      spot >>= 1;
    }
    superheap[spot] = s;
  }
  if ( heapcount != supercount )
   kb_error(2467,"Internal error: degree_sort() has heapcount != supercount.\n",
     RECOVERABLE);
}

/******************************************************************************
*
* function: mass_eliminate()
*
* purpose: eliminate one supernode, if independent
*/
void mass_eliminate(S,s)
struct linsys *S;
struct supernode *s;
{ int i,k;
  struct region *r,*rk;
  struct md_vertex *v;
  int newvlist;
  int newslist;
  int vercount;
  int size,ssize;
  int scount;
  int rkeep;
  int sontotal = 0; /* of elim nodes of sons */

  /* check independence and gather son elim totals */
  for ( i = 0 ; i < s->rcount ; i++ )
  { int rnum = INT(s->rlist)[i];
    r = REG(rnum);
    if ( r->timestamp == rtimestamp ) return; /* not independent */
    if ( r->son < 0 )
      sontotal += r->size - r->vercount;
  }
  if ( mindeg_debug_level >= 2 )
     printf("Eliminating supernode %d, size %d.\n",s-slist,s->vercount);
  /* mark timestamps */
  for ( i = 0 ; i < s->rcount ; i++ )
  { r = REG(INT(s->rlist)[i]);
    r->timestamp = rtimestamp;
  }

  /* create merged supernode and vertex lists */
  /* with eliminated supernode vertices at end */
  if ( sontotal > mindeg_min_region_size )
    sontotal = 0; /* don't merge with sons */
  size = s->degree + s->vercount + sontotal;
  newvlist = md_alloc(S,size*sizeof(int));
  ssize = exact_sdegree(S,s);
  newslist = md_alloc(S,ssize*sizeof(int));

  /* first, elim supernode at end */
  vtimestamp++; /* new round */
  stimestamp++; /* new round */
  INT(newslist)[ssize-1] = s-slist;
  s->stimestamp = stimestamp;
  vercount = s->degree;
  for ( k = 0 ; k < s->vercount ; k++ )
  { INT(newvlist)[vercount++] = INT(s->verlist)[k];
    vlist[INT(s->verlist)[k]].timestamp = vtimestamp;
    S->P[elim_count+k] = INT(s->verlist)[k];
    S->IP[INT(s->verlist)[k]] = elim_count+k;
  }
  /* and sons, if merging */
  if ( sontotal > 0 )
    for ( i = 0 ; i < s->rcount ; i++ )
    { int j;
      r = REG(INT(s->rlist)[i]);
      if ( r->son >= 0 ) continue;
      for ( j = r->vercount ; j < r->size ; j++ )
        INT(newvlist)[vercount++] = INT(r->verlist)[j];
    }
  
  vercount = 0;
  scount = 0;
  for ( i = 0 ; i < s->rcount ; i++ )
  { struct supernode *ss;
    r = REG(INT(s->rlist)[i]);
    for ( k = 0 ; k < r->supercount ; k++ )
    { ss = slist + INT(r->superlist)[k];
      if ( ss->stimestamp == stimestamp ) continue;
      INT(newslist)[scount++] = INT(r->superlist)[k];
      ss->stimestamp = stimestamp;
      ss->rtimestamp = rtimestamp;
    }
    for ( k = 0 ; k < r->vercount ; k++ )
    { v = vlist + INT(r->verlist)[k];
      if ( v->timestamp == vtimestamp ) continue;
      INT(newvlist)[vercount++] = INT(r->verlist)[k];
      v->timestamp = vtimestamp;
    }
  }

  /* set up new region */
  rkeep = region_alloc(S);
  rk = REG(rkeep);
  rk->verlist = newvlist;
  rk->vercount = vercount;
  rk->superlist = newslist;
  rk->supercount = scount;
  rk->son = -1;
  if ( vercount != s->degree ) 
     printf("vercount %d != degree %d, supernode %d\n",vercount,s->degree,s-slist);
     
  /* mark all old regions as merged and remove from active list */
  for ( i = 0 ; i < s->rcount ; i++ )
  {  int rnum = INT(s->rlist)[i]; 
     r = REG(rnum);
     r->merged = rkeep;
     if ( (r->size > r->vercount) || ( r->son >= 0 ) )
     { /* keep nonleaf regions */
       r->BROTHER = rk->son; 
       rk->son = rnum;
     }
     else if ( mindeg_debug_level >= 5 )
     { if (r->size == r->vercount)
         printf("Dropping nodeless region %d\n",rnum);
       else
         printf("Incorporating small region %d into region %d\n",rnum,rkeep);
     }
     if ( r->next >= 0 ) REG(r->next)->prev = r->prev; 
     else last_region = r->prev;
     if ( r->prev >= 0 ) REG(r->prev)->next = r->next;
     else regionstart = r->next;
     if ( mindeg_debug_level >= 5 )
        printf("Merging region %d with %d.\n",INT(s->rlist)[i],rkeep);
  }

  /* eliminate nodes */
  rk = REG(rkeep);
  rk->size = size;

  if ( mindeg_debug_level >= 5 )
     printf("New region %d size %d vercount %d son %d.\n",
          rkeep,rk->size,rk->vercount,rk->son);
  if ( mindeg_debug_level >= 7 )
  { printf("Border: ");
    for ( i = 0 ; i < rk->vercount ; i++ )
       printf("%d ",INT(rk->verlist)[i]);
    printf("   elim: ");
    for ( ; i < rk->size ; i++ )
       printf("%d ",INT(rk->verlist)[i]);
  }
  elim_count += s->vercount;

  /* delete this supernode */
  if ( s->next >= 0 ) slist[s->next].prev = s->prev;
  if ( s->prev >= 0 ) slist[s->prev].next = s->next;
  else superstart = s->next;
  s->verlist = -1;
  supercount--;

  s->rcount = 0; /* inactivate */
}



/******************************************************************************
*
*  function: groom_regions_recur()
*
*  purpose: Traverse newly created region tree, gathering stats
*           (and maybe compacting)
*/

static int final_regions;  /* number making it to factoring */
static int final_versizes;  /* number making it to factoring */

void groom_regions_recur(S,r)
struct linsys *S;
struct region *r;
{ struct region *rr;
  int son,k;
  
  for ( son = r->son ; son >= 0 ; son = rr->BROTHER )
  { rr = REG(son);
    groom_regions_recur(S,rr);
  }

  /* record fills per vertex for later factoring */
  for ( k = r->vercount ; k < r->size ; k++ )
  { int v = INT(r->verlist)[k];
    S->LIA[v] += r->size - (k - r->vercount);
  }

  /* gather stats */
  { int vn = r->vercount,vd = r->size;
    total_fill += (vd*(vd+1) - vn*(vn+1))/2;
    total_flops += ((vd+1.)*vd*(vd+2.) - (vn+1.)*vn*(vn+2.))/3;
  }
  final_regions++;
  final_versizes += r->size;
}

/******************************************************************************
 *
 *  function: compact_recur()
 *  
 *  purpose: Compact one region information to free up large amounts
 *           of storage before numerical factoring. Recursive.
 *
 *  return:  Offset (in ints) of new location in new_ISP
 */
static int *new_ISP;  /* compacted storage */
static int new_ISP_spot;

int compact_recur(S,r)
struct linsys *S;
struct region *r;
{ struct region *rr;
  int son;
  int *v;
  int retval;
  int *rnumptr;
  int i;

  rnumptr = &(r->son);
  for ( son = r->son ; son >= 0 ; son = rr->BROTHER )
  { rr = REG(son);
    *rnumptr = compact_recur(S,rr);
    rnumptr = &(((struct region*)(new_ISP + *rnumptr))->BROTHER);
  }

  /* now do this region */
  retval = new_ISP_spot;
  rr = (struct region *)(new_ISP + new_ISP_spot);
  *rr = *r;
  new_ISP_spot += sizeof(struct region)/sizeof(int);
  v = INT(r->verlist);
  rr->verlist = new_ISP_spot;
  for ( i = 0 ; i < r->size ; i++ )
    new_ISP[new_ISP_spot++] = v[i];

  return retval;

}

/******************************************************************************
 *
 *  function: compact_regions()
 *  
 *  purpose: Compact region information to free up large amounts
 *           of storage before numerical factoring.
 */
void compact_regions(S)
struct linsys *S;
{ int rnum;
  int *new_rnum;
  int new_NSP;
  struct region *r;

  new_NSP = final_regions*sizeof(struct region) + final_versizes*sizeof(int);
  new_ISP = (int*)temp_calloc(new_NSP,1);
  new_ISP_spot = 0;

  new_rnum = &regionstart;
  for ( rnum = regionstart ; rnum >= 0 ; rnum = r->next )
  { r = REG(rnum);
    *new_rnum = compact_recur(S,r);
    new_rnum = &(((struct region*)(new_ISP+*new_rnum))->next);
  }
  temp_free((char*)S->ISP);
  S->ISP = new_ISP;
  S->NSP = new_NSP;
}

/******************************************************************************
*
* function: multiple_eliminate()
*
* purpose: eliminate independent supernodes of low degree
*/

void multiple_eliminate(S)
struct linsys *S;
{ 
  int lowdegree;
  int bounddegree; /* cutoff for multiple elimination */
  int spot;

  old_rtimestamp = rtimestamp; /* save, so know who changed */
  rtimestamp++;    /* new round of independence */
  lowdegree = superheap[1]->degree;  /* so we know where we started */
  if ( mindeg_debug_level >= 1 ) printf("Low degree %d.\n",lowdegree);

  /* using higher bounddegree leads to about 1/3 as many passes through
     main loop, but not much reduction in total time */
  bounddegree = lowdegree+margin;        /* conservative to start with */
  bounddegree = mindeg_margin < lowdegree ? lowdegree+mindeg_margin : 
    2*lowdegree;

  sentinel.degree = 2+bounddegree;    /* big degree */
  sentinel.verlist = 1 << (8*sizeof(int)-2); /* big for degree_compare */
  while ( superheap[1]->degree <= bounddegree )
  { 
    mass_eliminate(S,superheap[1]);
    /* now adjust heap */
    spot = 1;  /* empty spot */
    for ( ;; )
    { if ( spot*2 > heapcount ) { superheap[spot] = &sentinel;  break; }
      if ( spot*2 == heapcount )
      { superheap[spot] = superheap[spot*2];
        superheap[spot*2] = &sentinel;
        break;
      }
      if ( degree_compare(superheap[spot*2],superheap[spot*2+1]) < 0 )
      { superheap[spot] = superheap[spot*2];
        spot *= 2;
      }
      else
      { superheap[spot] = superheap[spot*2+1];
        spot = spot*2 + 1;
      }
    }
  }
}

/*************************************************************************
*
* function: traverse_region_tree()
*
* purpose: Traverse region tree after it's constructed, doing factoring.
*             Uses remaining LA space as scratch space to assemble
*             current supernode rows, prior to compaction.
*             Everything stored in permuted order.
*/

static int treedepth;
static int IJA_base; /* number of entries in LIJA */

void traverse_recur(S,r,depth,fill)
struct linsys *S;
struct region *r;
int *depth;  /* matrix depth of r and below */
int *fill;  /* total fill of subtree */
{ int son;
  struct region *rr;
  int fillsum = 0;
  int sondepth;
  int maxdepth = 0;
  int sonfill;
  int thisfill;

  treedepth++;
  for ( son = r->son ; son >= 0 ; son = rr->BROTHER )
  { rr = REG(son);
    traverse_recur(S,rr,&sondepth,&sonfill);
    if ( sondepth > maxdepth ) maxdepth = sondepth;
    fillsum += sonfill;
  }
  thisfill = (r->size*(r->size+1) 
                  - r->vercount*(r->vercount+1))/2;
  *fill = fillsum + thisfill;
  *depth = maxdepth + (r->size*(r->size+1))/2;
{ int n; for (n=0;n<treedepth;n++)printf("  ");printf("%d\n",(r->size*(r->size+1))/2); }
treedepth--;
}

/*********************************************************************************
*
* function: permute_recur()
*
* purpose: traverse region tree to find elimnation order
*
*/
void permute_recur(S,r)
struct linsys *S;
struct region *r;
{ int k;
  struct region *rr;
  int son;

  if ( mindeg_debug_level > 5 ) 
     printf("permute_recur region %d, vercount %d rsize %d\n",
         (int*)r-S->ISP,r->vercount,r->size);
  for ( son = r->son ; son >= 0 ; son = rr->BROTHER )
  { rr = REG(son);
    permute_recur(S,rr);
  }
  for ( k = r->vercount ; k < r->size ; k++ )
  { S->P[K] = INT(r->verlist)[k];
    S->IP[S->P[K]] = K;
    K++;
  }
  IJA_base += r->size;
}


/*********************************************************************************
*
* function: sparse_permute()
*
* purpose: Permute sparse matrix structure according to permutation S->IP.
*             Does upper triangular form, with rows in order.
*             Leaves permuted sparse matrix in  S->pIA,pJA,pA.
*             Uses radix sort.
*/
void sparse_permute(S)
struct linsys *S;
{ int i,j,end,total;
  int *cIA;
  int *cJA;
  REAL *cA;

  /* allocate */

  S->pIA = (int*)temp_realloc((char*)S->pIA,(S->N+1)*sizeof(int));
  S->pJA = (int*)temp_realloc((char*)S->pJA,(S->IA[S->N]-A_OFF)*sizeof(int));
  S->pA = (REAL*)temp_realloc((char*)S->pA,(S->IA[S->N]-A_OFF)*sizeof(REAL));
  memset((char*)S->pIA,0,(S->N+1)*sizeof(int));
  memset((char*)S->pJA,0,(S->IA[S->N]-A_OFF)*sizeof(int));
  memset((char*)S->pA,0,(S->IA[S->N]-A_OFF)*sizeof(REAL));
  
  cIA = (int*)temp_calloc(S->N+1,sizeof(int));
  cJA = (int*)temp_calloc(S->IA[S->N]-A_OFF,sizeof(int));
  cA = (REAL*)temp_calloc(S->IA[S->N]-A_OFF,sizeof(REAL));

  /* sort on permuted column */
  for ( i = 0 ; i < S->N ; i++ )
  { int ii = S->IP[i];
    end = S->IA[i+1] - A_OFF;
    for ( j = S->IA[i] - A_OFF ; j < end ; j++ )
    { int m = S->IP[S->JA[j]-A_OFF];
      if ( ii < m )
         cIA[m]++;
      else cIA[ii]++;
    }
  }
  for ( i = 0, total = 0 ; i < S->N ; i++ )
  { int tmp = cIA[i]; cIA[i] = total; total += tmp; 
  }
  cIA[S->N] = total;
  for ( i = 0 ; i < S->N ; i++ )
  { int ii = S->IP[i];
    end = S->IA[i+1] - A_OFF;
    for ( j = S->IA[i] - A_OFF ; j < end ; j++ )
    { int m = S->IP[S->JA[j]-A_OFF];
      if ( ii < m )
      { cA[cIA[m]] = S->A[j];  cJA[cIA[m]++] = ii; }
      else { cA[cIA[ii]] = S->A[j]; cJA[cIA[ii]++] = m; }
    }
  }
  for ( i = S->N-1 ; i > 0 ; i-- ) cIA[i] = cIA[i-1];
  cIA[0] = 0;
  /* now sort on permuted row */
  for ( i = 0 ; i < S->N ; i++ )
  { end = cIA[i+1];
    for ( j = cIA[i] ; j < end ; j++ )
         S->pIA[cJA[j]]++;
  }
  for ( i = 0, total = 0 ; i < S->N ; i++ )
  { int tmp = S->pIA[i]; S->pIA[i] = total; total += tmp; 
  }
  S->pIA[S->N] = total;
  for ( i = 0 ; i < S->N ; i++ )
  { end = cIA[i+1];
    for ( j = cIA[i] ; j < end ; j++ )
    { S->pA[S->pIA[cJA[j]]] = cA[j]; S->pJA[S->pIA[cJA[j]]++] = i; }
  }
  for ( i = S->N-1 ; i > 0 ; i-- ) S->pIA[i] = S->pIA[i-1];
  S->pIA[0] = 0;
  temp_free((char*)cIA);
  temp_free((char*)cJA);
  temp_free((char*)cA); 
}

/********************************************************************************
*
* function: factor_recur()
*
* purpose: Traverse region tree, factoring supernode at each.
*/

int vcompare ARGS((int*,int*));

int vcompare(a,b)
int *a,*b;
{ return *a-*b;
}

void factor_recur(S,r)
struct linsys *S;
struct region *r;
{ int to_elim = r->size - r->vercount;
  REAL *base;  /* scratch row start in LA */
  int i,j,m,ii,ii_next;
  int end;
  REAL pivot;
  int *jspot;
  struct region *reg;
  int son;

  /* first, do sons */
  for ( son = r->son ; son >= 0 ; son = reg->BROTHER )
  { reg = REG(son);
    factor_recur(S,reg);
  }

  if ( to_elim == 0 ) return;

  /* get verlist in proper order with permuted numbers */
  for ( j = 0 ; j < r->size ; j++ )
    INT(r->verlist)[j] = S->IP[INT(r->verlist)[j]];
  qsort((char*)(INT(r->verlist)),r->size,sizeof(int), FCAST vcompare);

  /* fill in full rows in scratch space */
  for ( i = 0, base = S->LA + S->LIA[K] ; i < to_elim ; i++ )
  { REAL pa,pb,p11,p12,p21,p22;
    int jj;

    /* first, fill from original matrix */
    end = S->pIA[K+i+1];
    for ( j = S->pIA[K+i] ; j < end ; j++ )
       base[S->pJA[j]-(K+i)] = S->pA[j];

    /* incorporate the shift */
    if ( S->lambda != 0.0 )
    {
      if ( hessian_linear_metric_flag )
      { if ( S->lambda != 0.0 )
        { end = Met.pIA[K+i+1];
          for ( j = Met.pIA[K+i] ; j < end ; j++ )
             base[Met.pJA[j]-(K+i)] -= S->lambda*Met.pA[j];
        }
      }
      else if ( web.area_norm_flag )
         base[0] -= S->lambda*Met.A[S->P[K+i]]; /* special metric matrix */
      else if ( augmented_hessian_mode )
      { if ( S->P[K+i] < S->A_rows )   /* do not shift constraints! */
          base[0] -= S->lambda;
      }
      else base[0] -= S->lambda;
    }

    /* next, add in previous rows that have entry in col K */
    ii_next = 0;
    for ( ii = xJL[K+i] ; ii >= 0 ; ii = ii_next )
    { REAL *subbase = base - (K+i);
      REAL *la;
      int *ja;
      int  IJdiff = S->LIA[ii] - S->LIJA[ii];
      switch ( S->psize[ii] )
      { case ONEBYONE:
            pivot = S->LA[S->LIA[ii]]*S->LA[xIL[ii]];
            end = S->LIA[ii+1];
            for ( j = xIL[ii],ja = S->LJA+j-IJdiff,la=S->LA+j ;
                             j < end ; j++,la++,ja++ )
                     subbase[*ja] -= pivot*(*la);  /* big improvement */
            ii_next = xJL[ii];
            if ( ++(xIL[ii]) < end )
            { m = S->LJA[xIL[ii]-IJdiff];
              xJL[ii] = xJL[m]; xJL[m] = ii;
            }
          break;
        case ZEROPIVOT:  /* nothing to add */
            ii_next = xJL[ii];
            if ( ++(xIL[ii]) < end )
            { m = S->LJA[xIL[ii]-IJdiff];
              xJL[ii] = xJL[m]; xJL[m] = ii;
            }
            break;
        case SECONDOFPAIR:  /* will handle under FIRSTOFPAIR */
            ii_next = xJL[ii];
            break;
        case FIRSTOFPAIR:
            p11 = S->LA[S->LIA[ii]]; p21 = p12 = S->LA[S->LIA[ii]+1];
            p22 = S->LA[S->LIA[ii+1]];
            pa = p11*S->LA[xIL[ii]] + p12*S->LA[xIL[ii+1]];
            pb = p21*S->LA[xIL[ii]] + p22*S->LA[xIL[ii+1]];
            end = S->LIA[ii+1];
            for ( j = xIL[ii], jj = xIL[ii+1] ; j < end ; j++,jj++ )
                base[S->LJA[j-IJdiff]-(K+i)] -= pa*S->LA[j] + pb*S->LA[jj];
            ii_next = xJL[ii];
            if ( ++(xIL[ii]) < end )
            { m = S->LJA[xIL[ii]-IJdiff];
              xJL[ii] = xJL[m]; xJL[m] = ii;
            }
            if ( ii_next == ii+1 ) ii_next = xJL[ii+1];
            if ( ++(xIL[ii+1]) < end )
            { m = S->LJA[xIL[ii+1]-(S->LIA[ii+1]-S->LIJA[ii+1])];
              xJL[ii+1] = xJL[m]; xJL[m] = ii+1;
            }

             break;
        default: kb_error(1835,"Internal error: Illegal case of psize\n",
                          RECOVERABLE);

       }
     }

     /* compress to dense form */ 
     for ( j = to_elim ; j < r->size ; j++ )
     { REAL *to =  base + j - i;
       REAL *from = base + INT(r->verlist)[j] - (K+i);
       if ( from != to )
       { *to = *from;
         *from = 0.0;  /* clear for next round */
       }
     }
     S->LIA[K+i+1] = S->LIA[K+i] + r->size - i;

     base += r->size - i;
  }

  /* fill in LJA */
  for ( i = 0 ; i < to_elim ; i++ )
        S->LIJA[K+i] =  IJA_base + i;
  jspot = S->LJA + S->LIJA[K];
  for ( m = 0 ; m < r->size ; m++,jspot++ ) 
          *jspot = INT(r->verlist)[m];
  IJA_base += r->size;

  /* factor */
  for ( i = 0 ; i < to_elim ; i++ )  /* pivot row */
  { REAL *pivrow = S->LA + S->LIA[K+i];
    int p;
    REAL big,sigma,critsize;
    int br;

    /* first, decide on 1x1 or 2x2 pivot */
    p = ONEBYONE; /* default pivot size 1 */
    /* find max element in pivot row (same as pivot col) */
    /* but only considering current supernode */
    for ( j=i+1,big=-1.0,br = 0  ; j < to_elim ; j++ )
      if ( fabs(pivrow[j-i]) > big )
      { big = fabs(pivrow[j-i]); br = j; }
    critsize = hessian_epsilon * S->rowmag[S->P[K+i]];
    if ( ((big <= critsize)||(!BK_flag))
                    && (fabs(pivrow[0]) <= critsize) )
       p = ZEROPIVOT;
    else if ( (big*BKalpha > fabs(pivrow[0])) && BK_flag )
    { /* find max in row/col r */
      REAL *rr;
      for ( j = i, sigma = -1.0; j<br ; j++,rr++ )
      { rr = S->LA + S->LIA[K+j] + br - j;
        if ( fabs(*rr) > sigma ) sigma = fabs(*rr);
      }
      for ( rr = S->LA + S->LIA[K+br], j = br ; j < to_elim; rr++, j++ )
       if ( fabs(*rr) > sigma ) sigma = fabs(*rr);
      if ( BKalpha*big*big > sigma*fabs(pivrow[0]) )
      { p = FIRSTOFPAIR; }
    }

    if ( p == ZEROPIVOT )
    { S->psize[K+i] = ZEROPIVOT; S->zero++;
      if ( S->P[K+i] >= S->A_rows ) S->degencon++;
      memset((char*)pivrow,0,(r->size-i)*sizeof(REAL));
      xIL[K+i] = -1;
    }
    else if ( p == ONEBYONE )
    { S->psize[K+i] = ONEBYONE;
      pivot = 1/pivrow[0];
      if ( pivot > 0.0 ) S->pos++; else S->neg++;
      for ( j = i+1; j < to_elim ; j++ ) /* row down */
      { REAL *spot = S->LA + S->LIA[K+j];
        REAL pp = pivrow[j-i]*pivot;
        REAL *pr = pivrow + j - i;

        /* this is the time-consuming inner loop */
        for ( m = j ; m < r->size ; m++ , spot++, pr++)
/* semiheavy */  /*  *spot -= pivrow[m-i]*pp; */
                    *spot -= (*pr)*pp;    /* no improvement */
      }
      for ( m = i+1, pivrow++ ; m < r->size ; m++,pivrow++ ) 
            *pivrow *= pivot;  /* pivot row */
      xIL[K+i] = S->LIA[K+i]+(to_elim-i);
      if ( xIL[K+i] < S->LIA[K+i+1] )
      { m = S->LJA[xIL[K+i]-(S->LIA[K+i]-S->LIJA[K+i])];
        xJL[K+i] = xJL[m]; xJL[m] = K+i;
      }
      S->psize[K+i] = ONEBYONE;
    }
    else /* 2x2 pivot */
    { REAL *pivrow1 = pivrow;
      REAL *pivrow2 = S->LA + S->LIA[K+i+1];
      REAL p11,p12,p22,detinv,*yy1,*yy2,pa1,pa2;
      int k;
      REAL *x;

      S->psize[K+i] = FIRSTOFPAIR;
      S->psize[K+i+1] = SECONDOFPAIR;
      if ( br != i+1 ) /* swap rows to get adjacent */
      { REAL dtmp,*rii,*rr;
        int c;

        /* swap in matrix, both A and JA */
#define DSWAP(a,b)  {dtmp=(a);(a)=(b);(b)=dtmp;}
        for ( j = 0 ; j <= i ; j++ ) /* swap columns */
        { rr = S->LA + S->LIA[K+j] - j;
          DSWAP(rr[i+1],rr[br]);
        }
        for ( j = i+2 ; j < br ; j++ )  /* across to down */
        { DSWAP(pivrow2[j-(i+1)],S->LA[S->LIA[K+j]+br-j]);
        }
        for ( j = br+1, rr=pivrow2+(br-i), rii=S->LA+S->LIA[K+br]+1 ;
                      j < r->size ; j++, rr++, rii++ )
        { DSWAP(*rr,*rii); }
        DSWAP(pivrow2[0],S->LA[S->LIA[K+br]]); /* diag elements */

        /* fix up LJA in swapped columns */
        c = S->LJA[S->LIJA[K+i+1]];
        S->LJA[S->LIJA[K+i+1]] = S->LJA[S->LIJA[K+br]];
        S->LJA[S->LIJA[K+br]] = c;
      }

      /* now, actual 2x2 pivot */
      p11 = pivrow1[0]; p12 = pivrow1[1]; p22 = pivrow2[0];
      detinv = 1/(p11*p22 - p12*p12);
      if ( detinv > 0.0 )
        { if ( p11+p22 > 0.0 ) S->pos += 2; else S->neg += 2; }
      else { S->pos++; S->neg++; }
      /* sweep through matrix */
      for ( k = i+2, x = S->LA+S->LIA[K+i+2] ; k < to_elim ; k++ ) /* row */
      { pa1 = (pivrow1[k-i]*p22 - pivrow2[k-i-1]*p12)*detinv;
        pa2 = (pivrow2[k-i-1]*p11 - pivrow1[k-i]*p12)*detinv;
        for ( j=k,yy1=pivrow1+k-i,yy2=pivrow2+k-i-1 ; j<r->size; 
                  j++ ,x++,yy1++,yy2++) /*col*/
        {  *x -=  pa1*(*yy1) + pa2*(*yy2); 
        }
        pivrow1[k-i] = pa1;
        pivrow2[k-i-1] = pa2;
      }
      for ( k = to_elim ; k < r->size ; k++ ) /* finish pivot rows */
      { pa1 = (pivrow1[k-i]*p22 - pivrow2[k-i-1]*p12)*detinv;
        pa2 = (pivrow2[k-i-1]*p11 - pivrow1[k-i]*p12)*detinv;
        pivrow1[k-i] = pa1;
        pivrow2[k-i-1] = pa2;
      }
      xIL[K+i] = S->LIA[K+i]+(to_elim-i);
      if ( xIL[K+i] < S->LIA[K+i+1] )
      { m = S->LJA[xIL[K+i]-(S->LIA[K+i]-S->LIJA[K+i])];
        xJL[K+i] = xJL[m]; xJL[m] = K+i;
      }
      i++;  /* since just did 2 rows */
      xIL[K+i] = S->LIA[K+i]+(to_elim-i);
      if ( xIL[K+i] < S->LIA[K+i+1] )
      { m = S->LJA[xIL[K+i]-(S->LIA[K+i]-S->LIJA[K+i])];
        xJL[K+i] = xJL[m]; xJL[m] = K+i;
      }
    }
  }

  K += to_elim;
}

/**************************************************************************
*
* function: traverse_region_tree()
*
* purpose: do numerical factoring.  
*          Permutes nodes to factoring order according to region tree.
*          Calls factor_recur().
*/

void traverse_region_tree(S)
struct linsys *S;
{ struct region *r;
  int rnum,n;
  int fill;

  /* allocate space */
  for ( fill = 0, n = 0 ; n < S->N ; n++ )
  { int tmp = S->LIA[n]; S->LIA[n] = fill; fill += tmp; }
  S->LIA[S->N] = fill;
  if ( fill != total_fill )
  { sprintf(errmsg,"Internal error: fill %d  !=  total_fill %d\n",fill,total_fill);
    kb_error(1836,errmsg,RECOVERABLE);
  }
  xIL = (int*)temp_calloc(S->N,sizeof(int));
  xJL = (int*)temp_calloc(S->N,sizeof(int));
  for ( n = 0 ; n < S->N ; n++ ) xJL[n] = -1;  /* list terminators */

  /* get tree traverse permutation order */
  K = 0;
  IJA_base = 0; /* for total LIJA entries */
  for ( rnum = regionstart ; rnum >= 0 ; rnum = r->next )
  { r = REG(rnum);
    permute_recur(S,r);
  }
  sparse_permute(S);
  if ( hessian_linear_metric_flag && (S->lambda != 0.0) )
  { memcpy((char*)Met.P,(char*)S->P,S->N*sizeof(int));
    memcpy((char*)Met.IP,(char*)S->IP,S->N*sizeof(int));
    sparse_permute(&Met);
  }

  /* allocate L space */
  S->Lsize = total_fill; 
  S->LJA = (int*)temp_realloc((char*)S->LJA,IJA_base*sizeof(int));
  S->LIJA = (int*)temp_realloc((char*)S->LIJA,(S->N+1)*sizeof(int));
  S->LA = (REAL*)temp_realloc((char*)S->LA,S->Lsize*sizeof(REAL));
  memset(S->LJA,0,IJA_base*sizeof(int));
  memset(S->LIJA,0,(S->N+1)*sizeof(int));
  memset(S->LA,0,S->Lsize*sizeof(REAL));
  
  K = 0;  /* current row */
  IJA_base = 0 ;
  for ( rnum = regionstart ; rnum >= 0 ; rnum = r->next )
  { r = REG(rnum);
    factor_recur(S,r);
  }
  temp_free((char*)xIL); xIL = NULL;
  temp_free((char*)xJL); xJL = NULL;

  /* test_print(S,0); */
/*debug */    /*  dsolve(S); */  
}


/*************************************************************************
*
* function: xmd_solve()
* 
* purpose: solve factored system for given right hand side
*             Factor stored as U, permuted order
*             LJA indices in LIJA, LA indices in LIA
*
*/

void xmd_solve(S,B,x)
struct linsys *S; /* factored system */
REAL *B;    /* incoming right hand side */
REAL *x;    /* solution, may be rhs */
{
  int n; /* row index */
  int i;
  int *jp;  /* pointer into LIJA */
  REAL *BB,*Y,*e;


  if ( S->psize == NULL )
     kb_error(1837,"Internal error: Must call xmd_factor before xmd_solve.\n",
         RECOVERABLE);

  /* Use existing working storage if possible */
  if ( S->NSP*sizeof(int) < 2*S->N*sizeof(REAL))
  { S->ISP = (int*)temp_realloc((char*)S->ISP,2*S->N*sizeof(REAL));
    S->NSP = (2*S->N*sizeof(REAL))/sizeof(int);
  }
  BB = (REAL*)S->ISP;           /* intermediate solutions */
  Y  = ((REAL*)S->ISP) + S->N;  /* intermediate solutions */

  /* solve U^T Y = B */
  for ( n = 0 ; n < S->N ; n++ ) BB[n] = B[S->P[n]]; /* permute */
  for ( n = 0 ; n < S->N ; n++ )
  { int start,end;
    Y[n] = BB[S->LJA[S->LIJA[n]]];  /* for BK inner permutation */
    if ( Y[n] == 0.0 ) continue;
    if ( S->psize[n] == FIRSTOFPAIR ) start = 2;
    else start = 1; 
    end = S->LIA[n+1];
    for ( i=S->LIA[n]+start, e=S->LA+i , jp=S->LJA+S->LIJA[n]+start ; 
                   i < end ; i++,e++,jp++ )
      BB[*jp] -= (*e)*Y[n];
  }

  /* solve D V = Y (will use Y to store V) */
  for ( n = 0 ; n < S->N ; n++ )
  { if ( S->psize[n] == ONEBYONE )
       Y[n] /= S->LA[S->LIA[n]];
    else if ( S->psize[n] == ZEROPIVOT ) Y[n] = 0.0;  /* generalized inverse */
    else
    { REAL piv[2][2];
      REAL pinv[2][2];
      REAL det,yy;
      piv[0][0] = S->LA[S->LIA[n]];
      piv[0][1] = piv[1][0] = S->LA[S->LIA[n]+1];
      piv[1][1] = S->LA[S->LIA[n+1]];
      det = piv[0][0]*piv[1][1] - piv[0][1]*piv[1][0];
      pinv[0][0] = piv[1][1]/det;
      pinv[1][0] = pinv[0][1] = -piv[0][1]/det;
      pinv[1][1] = piv[0][0]/det;
      yy = Y[n]*pinv[0][0] + Y[n+1]*pinv[1][0];
      Y[n+1] = Y[n]*pinv[0][1] + Y[n+1]*pinv[1][1];
      Y[n] = yy;
      n++; 
    }
  }

  /* solve U X = V */
  for ( n = S->N-1 ; n >= 0 ; n-- )
  { int start,end;
    if ( S->psize[n] == FIRSTOFPAIR ) start = 2;
    else start = 1; 
    end = S->LIA[n+1];
    for ( i=S->LIA[n]+start, e=S->LA+i, jp=S->LJA+S->LIJA[n]+start  ; 
        i < end ; i++,e++,jp++ )
         Y[n] -= (*e)*BB[*jp];
    BB[S->LJA[S->LIJA[n]]] = Y[n];
  }

  /* unpermute */
  for ( n = 0 ; n < S->N ; n++ )
     x[S->P[n]] = BB[n];

}


/*************************************************************************
*
* function: xmd_solve_multi()
* 
* purpose: solve factored system for multiple right hand sides
*             Factor stored as U, permuted order
*             LJA indices in LIJA, LA indices in LIA
*
*/

void xmd_solve_multi(S,B,x,rk)
struct linsys *S; /* factored system */
REAL **B;    /* incoming right hand side */
REAL **x;    /* solution, may be rhs */
int rk;         /* number of right sides */
{ int k; /* rhs column index */

  for ( k = 0 ; k < rk ; k++ )
    xmd_solve(S,B[k],x[k]);
  return;

#ifdef OLDXMDMULTI
/* something wrong with this */
{ int n; /* row index */
  int i;
  int *jp;  /* pointer into LIJA */
  REAL **BB,**Y,*e;

  if ( S->psize == NULL )
     kb_error(2531,"Internal error: Must call BK_factor before BK_solve.\n",RECOVERABLE);


  BB = dmatrix(0,S->N-1,0,rk-1);  /* intermediate solutions */
  Y =  dmatrix(0,S->N-1,0,rk-1);  /* intermediate solutions */

  /* solve U^T Y = B */
  for ( n = 0 ; n < S->N ; n++ ) 
    for ( k = 0 ; k < rk ; k++ )
      BB[n][k] = B[k][S->P[n]]; /* permute */

  for ( n = 0 ; n < S->N ; n++ )
  { int start,end;
    REAL *yy,*bb,ee;
    for ( yy = Y[n], k = 0 , bb = BB[S->LJA[S->LIJA[n]]]; k < rk ; k++ )
        *(yy++) = *(bb++);
    if ( S->psize[n] == FIRSTOFPAIR ) start = 2;
    else start = 1; 
    end = S->LIA[n+1];
    for ( i=S->LIA[n]+start, e=S->LA+i , jp=S->LJA+S->LIJA[n]+start ; 
                 i < end ; i++,e++,jp++ )
    { ee = *e;
      for ( bb = BB[*jp], yy = Y[n], k = 0 ; k < rk ; k++ )
         *(bb++) -= ee*(*(yy++));
    }
  }

  /* solve D V = Y (will use Y to store V) */
  for ( n = 0 ; n < S->N ; n++ )
  { if ( S->psize[n] == ONEBYONE )
    { REAL *y,pinv;
      pinv = 1.0/S->LA[S->LIA[n]];
      for ( y = Y[n], k = 0 ; k < rk ; k++ )
          *(y++) *= pinv;
    }
    else if ( S->psize[n] == ZEROPIVOT )
       for ( k = 0 ; k < rk ; k++ ) Y[n][k] = 0.0;  /* generalized inverse */
    else
    { REAL piv[2][2];
      REAL pinv[2][2];
      REAL det,yy;
      piv[0][0] = S->LA[S->LIA[n]];
      piv[0][1] = piv[1][0] = S->LA[S->LIA[n]+1];
      piv[1][1] = S->LA[S->LIA[n+1]];
      det = piv[0][0]*piv[1][1] - piv[0][1]*piv[1][0];
      pinv[0][0] = piv[1][1]/det;
      pinv[1][0] = pinv[0][1] = -piv[0][1]/det;
      pinv[1][1] = piv[0][0]/det;
      for ( k = 0 ; k < rk ; k++ )
      { yy = Y[n][k]*pinv[0][0] + Y[n+1][k]*pinv[1][0];
        Y[n+1][k] = Y[n][k]*pinv[0][1] + Y[n+1][k]*pinv[1][1];
        Y[n][k] = yy;
      }
      n++; 
    }
  }

  /* solve U X = V */
  for ( n = S->N-1 ; n >= 0 ; n-- )
  { int start,end;
    REAL *yy,*bb,ee;
    if ( S->psize[n] == FIRSTOFPAIR ) start = 2;
    else start = 1; 
    end = S->LIA[n+1];
    for ( i=S->LIA[n]+start, e=S->LA+i, jp=S->LJA+S->LIJA[n]+start  ; 
        i < end ; i++,e++,jp++ )
    { ee = *e;
      for ( yy = Y[n], bb = BB[*jp], k = 0 ; k < rk ; k++ )
         *(yy++) -= ee*(*(bb++));
    }
    for ( yy = Y[n], bb = BB[S->LJA[S->LIJA[n]]], k = 0 ; k < rk ; k++ )
      *(bb++) = *(yy++);
  }

  /* unpermute */
  for ( n = 0 ; n < S->N ; n++ )
    for ( k = 0 ; k < rk ; k++ )
     x[k][S->P[n]] = BB[n][k];

  free_matrix(Y);
  free_matrix(BB);
}
#endif
}

/*************************************************************************/

void dsolve(S)  /* solve as dense matrix */
struct linsys *S;
{ REAL **a,**u;
  int i,j,k,m,jj;

  int *xIP = (int*)temp_calloc(S->N,sizeof(int));
  for ( k = 0 ; k < S->N ; k++ )
     xIP[S->LJA[S->LIJA[k]]] = k;

  a = dmatrix(0,S->N-1,0,S->N-1);
  for ( i = 0 ; i < S->N ; i++ )
  { for ( j = S->IA[i]-A_OFF  ; j < S->IA[i+1] - A_OFF ; j++ )
     { k = xIP[S->IP[i]];
       m = xIP[S->IP[S->JA[j]-A_OFF]];
       a[k][m] = a[m][k] = S->A[j];
     }
  }
  for ( i = 0 ; i < S->N ; i++ )
  { 
     if ( S->psize[i] == ONEBYONE )
     { for ( j = i+1 ; j < S->N ; j++ )
          for ( k = i+1 ; k < S->N ; k++ )
             a[j][k] -= a[i][j]*a[i][k]/a[i][i];
        for ( k = i+1 ; k < S->N ; k++ )
          a[i][k] /= a[i][i];
     }
     else if ( S->psize[i] == ZEROPIVOT )
     { a[i][i] = 0.0;
     }
     else
     { REAL p11,p12,p22,detinv,pa1,pa2;
       p11 = a[i][i]; p12 = a[i][i+1]; p22 = a[i+1][i+1];
       detinv = 1/(p11*p22 - p12*p12);
       /* sweep through matrix */
       for ( k = i+2 ; k < S->N ; k++ ) /* row */
          { pa1 = (a[i][k]*p22 - a[i+1][k]*p12)*detinv;
             pa2 = (a[i+1][k]*p11 - a[i][k]*p12)*detinv;
             for ( j=k ; j < S->N ; j++ ) /*col*/
             {  a[k][j] -=  pa1*a[i][j] + pa2*a[i+1][j]; 
             }
             a[i][k] = pa1;
             a[i+1][k] = pa2;
          }
       i++;  /* since did 2 rows */
     }

  }
  u = dmatrix(0,S->N-1,0,S->N-1);
  for ( i = 0 ; i < S->N ; i++ )
     for ( j = S->LIA[i], jj = S->LIJA[i]; j < S->LIA[i+1] ; j++ )
        u[i][S->LJA[jj]] = S->LA[j];
  for ( i = 0 ; i < S->N ; i++ )
    for ( j = i ; j < S->N ; j++ )
      if ( fabs(a[i][j]-u[i][j]) > 100*machine_eps )
      { printf("u[%d][%d] = %20.15f    a[%d][%d] = %20.15f\n",
              i,j,(DOUBLE)u[i][j],i,j,(DOUBLE)a[i][j]);
        break;
      }

  free_matrix(a);
  free_matrix(u);
  temp_free((char*)xIP);
}
     
/***************************************************************************
*
* function: xmd_factor()
*
* purpose: experimental minimal degree factoring of system
*
*/
void xmd_factor(S)
struct linsys *S; 
{ int i,j;
  int rnum;
  struct region *r;
  
  if ( S->N <= 0 )
  { kb_error(1839,"Internal error: Empty linear system.\n",WARNING); 
    return;
  }
  total_flops = total_fill = 0;
  rtimestamp = 0;
  stimestamp = 0;
  old_rtimestamp = -1; /* less than rtimestamp first time around */
  vtimestamp = 0;
  S->neg = S->zero = S->pos = 0;
  S->degencon = 0;

  /* working storage */
  S->NSP = 4*S->IA[S->N];
  S->ISP = (int*)temp_realloc((char*)S->ISP,S->NSP*sizeof(int));
  memset(S->ISP,0,S->NSP*sizeof(int));

  /* row magnitudes for nullity detection */
  S->rowmag = (REAL *)temp_realloc((char*)S->rowmag,S->N*sizeof(REAL));
  memset(S->rowmag,0,S->N*sizeof(REAL));
  for ( i = 0 ; i < S->N ; i++ )
  { int end = S->IA[i+1] - A_OFF;
    for ( j = S->IA[i]-A_OFF ; j < end ; j++ )
    { S->rowmag[i] += S->A[j]*S->A[j];
      S->rowmag[S->JA[j]-A_OFF] += S->A[j]*S->A[j];
    }
  }
  for ( i = 0 ; i < S->N ; i++ )
   S->rowmag[i] = sqrt(S->rowmag[i]);
      

  md_vertex_setup(S);
  md_supernode_setup(S);
  md_region_string(S);
  md_supernode_regions(S);
  /* space for lists */
  superheap = (struct supernode**)temp_calloc(supercount+1,
                  sizeof(struct supernode*));
                  
  /* final lower triangular matrix storage */
  S->psize = (int*)temp_realloc((char*)S->psize,S->N*sizeof(int));
  S->LIA = (int*)temp_realloc((char*)S->LIA,(S->N+1)*sizeof(int));
  memset((char*)S->psize,0,S->N*sizeof(int));
  memset((char*)S->LIA,0,(S->N+1)*sizeof(int));
  
  L_total = 0;
  elim_count = 0;

  for(passes=0;;passes++)
  {
     if ( supercount == 0 ) break;
     degree_sort(S);
     multiple_eliminate(S);
     region_absorb(S);  /* needed to have few passes through main loop */
     clean_supernodes(S);  /* supernodes stamped */
     merge_supernodes(S);

  if ( mindeg_debug_level == -1 )
    show_mindeg_state(S);

  }
  S->LIA[elim_count] = L_total;  /* final sentinel */

  /* free temp storage */ 
  temp_free((char*)slist);
  temp_free((char*)vlist); vlist = NULL;
  temp_free((char*)superheap); superheap = NULL;

  /* gather stats */
  final_regions = 0;
  final_versizes   = 0;
  for ( rnum = regionstart ; rnum >= 0 ; rnum = r->next )
  { r = REG(rnum);
    groom_regions_recur(S,r);
  }
  compact_regions(S);

  traverse_region_tree(S); 

  eigen_pos = S->pos; 
  eigen_neg = S->neg;
  eigen_zero = S->zero;

  if ( !hessian_quiet_flag )
  {
    printf("Variables: %d  Original fill: %d\n",S->N,S->IA[S->N]);
    printf("Workspace: %d bytes\n",S->ISP[0]*sizeof(int));
    printf("Passes through main loop: %d\n",passes);
    printf("Total_fill:  %d\n",total_fill);
    printf("Total_flops: %g\n",total_flops);
  }
}



