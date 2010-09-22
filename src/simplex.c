/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*********************************************************************
*
*     file:        simplex.c
*
*     Contents:  Functions calculating energy, volume, and their
*                    gradients for the LINEAR simplex model.
*/

#include "include.h"

/**********************************************************************
*
*  function:  calc_simplex_energy()
*
*  purpose:    Calculates energy due to one simplex facet.
*                 Uses Gram determinant to find simplex area.
*
*/

void calc_simplex_energy(f_id)
facet_id f_id;
{
  vertex_id *v = get_facet_vertices(f_id);
  REAL side[MAXCOORD][MAXCOORD];
  REAL *x[MAXCOORD];
  REAL matspace[MAXCOORD][MAXCOORD];
  REAL *mat[MAXCOORD];
  REAL energy;
  REAL det;
  int k,j;

  x[0] = get_coord(v[0]);
  for ( k = 1 ; k <= web.dimension ; k++ )
  { x[k] = get_coord(v[k]);
    for ( j = 0 ; j < SDIM ; j++ )
       side[k-1][j] = x[k][j] - x[0][j];
  }
  for ( k = 0 ; k < web.dimension ; k++ )
  { mat[k] = matspace[k]; /* set up for matrix.c */
    for ( j = 0 ; j <= k ; j++ )
       mat[j][k] = mat[k][j] = SDIM_dot(side[j],side[k]);
  }
  det = determinant(mat,web.dimension);
  if ( det <= 0.0 ) energy = 0.0;
  else energy = sqrt(det)/web.simplex_factorial;

  web.total_area    += energy;

  set_facet_area(f_id,energy);
  if ( get_fattr(f_id) & DENSITY )
         energy *= get_facet_density(f_id);
  binary_tree_add(web.total_energy_addends,energy);
      
}

/**********************************************************************
*
*  function:  calc_simplex_forces()
*
*  purpose:    Calculates vertex forces due to one simplex facet.
*
*/

void calc_simplex_forces(f_id)
facet_id f_id;
{
    vertex_id *v = get_facet_vertices(f_id);
    REAL side[MAXCOORD][MAXCOORD];
    REAL *x[MAXCOORD];
    REAL *f[MAXCOORD];
    REAL matspace[MAXCOORD][MAXCOORD];
    REAL *mat[MAXCOORD];
    REAL force;
    int k,j,i;
    REAL det;
    REAL factor;

    x[0] = get_coord(v[0]);
    f[0] = get_force(v[0]);
    for ( k = 1 ; k <= web.dimension ; k++ )
      {  x[k] = get_coord(v[k]);
          f[k] = get_force(v[k]);
          for ( j = 0 ; j < SDIM ; j++ )
             side[k-1][j] = x[k][j] - x[0][j];
      }
    for ( k = 0 ; k < web.dimension ; k++ )
      { mat[k] = matspace[k]; /* set up for matrix.c */
         for ( j = 0 ; j <= k ; j++ )
            mat[j][k] = mat[k][j] = SDIM_dot(side[j],side[k]);
      }
    det = det_adjoint(mat,web.dimension);  /* leaves mat adjoint transpose */
    if ( det <= 0.0 ) return; /* degenerate triangle */

    if ( get_fattr(f_id) & DENSITY )
         factor = get_facet_density(f_id)/web.simplex_factorial/sqrt(det);
    else factor = 1.0/web.simplex_factorial/sqrt(det);

    for ( k = 0 ; k < web.dimension ; k++ )
     for ( i = 0 ; i < web.dimension ; i++ )
     { REAL *ss,*ff,*ff0, fudge =  factor*mat[k][i];
        for ( j=SDIM, ff0=f[0], ff=f[k+1],ss=side[i] ; j ; j-- )
         { 
            *(ff++) -= force = fudge*(*(ss++));
            *(ff0++) += force;
         }
     }

  /* accumulate 1/n area around each vertex to scale motion */
     { REAL energy = sqrt(det)/web.simplex_factorial;
        for ( k = 0  ; k <= web.dimension ; k++ )
          add_vertex_star(v[k],energy);
     }

}


/**********************************************************************
*
*  function:  calc_simplex_volume()
*
*  purpose:    Calculates volume due to one simplex facet.
*                 Symmetric style only.
*
*/

void calc_simplex_volume(f_id)
facet_id f_id;
{
  vertex_id *v = get_facet_vertices(f_id);
  REAL *x[MAXCOORD];
  REAL matspace[MAXCOORD][MAXCOORD];
  REAL *mat[MAXCOORD];
  int k,j;
  body_id b_id0,b_id1;
  REAL vol;
     
  if ( get_fattr(f_id) & NONCONTENT ) return;
  
  b_id0 = get_facet_body(f_id);
  b_id1 = get_facet_body(facet_inverse(f_id));
  if ( !valid_id(b_id0) && !valid_id(b_id1) ) return;
     
  for ( k = 0 ; k <= web.dimension ; k++ )
      {  x[k] = get_coord(v[k]);
          mat[k] = matspace[k];
          for ( j = 0 ; j < SDIM ; j++ )
             mat[k][j] = x[k][j];
      }
  vol = determinant(mat,web.dimension+1);
  vol /= web.simplex_factorial*(web.dimension+1);

  /* add to body volumes */
  if ( valid_id(b_id0) ) 
     add_body_volume(b_id0,vol);
  if ( valid_id(b_id1) ) 
     add_body_volume(b_id1,-vol);
}

/******************************************************************
*    
*  Function: simplex_grad_l()
*
*  Purpose: Compute volume gradients for vertices on facets.
*/

void simplex_grad_l()
{
  body_id bi_id;  /* identifier for body i */
  body_id bj_id;  /* identifier for body j */
  vertex_id *v;
  REAL *x[MAXCOORD];
  REAL matspace[MAXCOORD][MAXCOORD];
  REAL *mat[MAXCOORD];
  int k,j,i;
  facet_id f_id;
     
  volgrad *vgptr;

  FOR_ALL_FACETS(f_id)
  { if ( get_fattr(f_id) & NONCONTENT ) return;
    bi_id = get_facet_body(f_id);
    bj_id = get_facet_body(facet_inverse(f_id));
    if ( !valid_id(bi_id) && !valid_id(bj_id) ) continue;
    v = get_facet_vertices(f_id);
    for ( k = 0 ; k <= web.dimension ; k++ )
    { x[k] = get_coord(v[k]);
      mat[k] = matspace[k];
      for ( j = 0 ; j < SDIM ; j++ )
        mat[k][j] = x[k][j];
    }
    det_adjoint(mat,web.dimension+1); /* mat adjoint transpose */

    if ( valid_id(bi_id) && (get_battr(bi_id) & (PRESSURE|FIXEDVOL)) )
    { for ( k = 0 ; k <= web.dimension ; k++ )
      { vgptr = get_bv_new_vgrad(get_body_fixnum(bi_id),v[k]);
        vgptr->bb_id = bi_id;
        for ( i = 0 ; i < SDIM ; i++ )
          vgptr->grad[i] +=  mat[i][k]/volume_factorial;
      }
    }
    if ( valid_id(bj_id) && (get_battr(bj_id) & (PRESSURE|FIXEDVOL)) )
    { for ( k = 0 ; k <= web.dimension ; k++ )
      { vgptr = get_bv_new_vgrad(get_body_fixnum(bj_id),v[k]);
        vgptr->bb_id = bj_id;
        for ( i = 0 ; i < SDIM ; i++ )
          vgptr->grad[i] -=  mat[i][k]/volume_factorial;
     }
    }
  } /* end facet loop */
}


/**************************************************************************
*
* function:  refine_simplex()
*
* purpose:
*  Recursive refining of a simplex divided at the midpoints of its edges. 
*/


void refine_simplex(dim,vlist,elist,slist)
     int dim;     /* dimension of simplex */
     int *vlist;     /* vertices of simplex  */
     struct divedge *elist; /* edges of simplex */
     struct simplex *slist; /* generated simplices */
{ int v1,v2;
  int basept;
  int numedges = (dim*(dim+1))/2;
  int snum = 0;  /* simplex number */
  int pnum;        /* point number in simplex */
  struct divedge *newelist
           = (struct divedge *)temp_calloc(numedges,sizeof(struct divedge));
  int i,j;
  int newfaces;
  int edgenum;
  int lowpt,lowpti=0;

  /* pick base point as lowest numbered midpoint on list  */
  for ( i = 0 , lowpt = 0x7FFF7FFF ; i < numedges ; i++ )
  { if ( elist[i].divpt < lowpt ) 
    { lowpt = elist[i].divpt; lowpti = i; }
  }
  v1 = elist[lowpti].endpt[0]; v2 = elist[lowpti].endpt[1];
  basept = elist[lowpti].divpt;

  /* do simple opposite simplices */
  /* one for each vertex not an endpoint on base edge */
  for ( i = 0 ; i <= dim ; i++ )
     { if ( (vlist[i] == v1) || (vlist[i] == v2) ) continue; /* on base edge */
        for ( j = 0, pnum = 0 ; j < numedges ; j++ )
          if ( (elist[j].endpt[0]==vlist[i]) || (elist[j].endpt[1]==vlist[i]))
             slist[snum].pt[pnum++] = elist[j].divpt;
        slist[snum].pt[pnum] = basept; /* add common basepoint */
        snum++;
     }
  if ( dim <= 2 ) goto exxit; /* end recursion */

  /* now do compound opposite faces */
  /* first, opposite v1 */
  /* first, get edges after base not including  v2 */
  for ( i = 0, edgenum = 0 ; i < numedges ; i++ )
     if ( (elist[i].endpt[0]!=v1) && (elist[i].endpt[1]!=v1) )
          newelist[edgenum++] = elist[i];
  /* get vertex list in good order with v1 first */
  for ( i = 0 ; i <= dim ; i++ )
     if ( v1 == vlist[i] )
        { vlist[i] = vlist[0]; vlist[0] = v1; break; }
  /* recurse */
  refine_simplex(dim-1,vlist+1,newelist,slist+snum);
  /* add base point */
  newfaces = (1<<(dim-1))-dim;
  for ( i = 0 ; i < newfaces ; i++ )
     slist[snum++].pt[dim] = basept;

  /* second, opposite v2 */
  /* first, get edges after base not including  v2 */
  for ( i = 0, edgenum = 0 ; i < numedges ; i++ )
     if ( (elist[i].endpt[0]!=v2) && (elist[i].endpt[1]!=v2) )
          newelist[edgenum++] = elist[i];
  /* get vertex list in good order with v2 first */
  for ( i = 0 ; i <= dim ; i++ )
     if ( v2 == vlist[i] )
        { vlist[i] = vlist[0]; vlist[0] = v2; break; }
  /* recurse */
  refine_simplex(dim-1,vlist+1,newelist,slist+snum);
  /* add base point */
  newfaces = (1<<(dim-1))-dim;
  for ( i = 0 ; i < newfaces ; i++ )
     slist[snum++].pt[dim] = basept;

exxit:
  temp_free((char*)newelist);
}

static struct simplex *slist;
static struct simplex *slist_edge;
static int dim;
static int count,scount;
static REAL (*vcoord)[MAXCOORD];

/* temporary explicit refinements */
static struct simplex slist3[8] = { {{0,4,5,6}}, {{1,4,8,7}}, {{2,5,7,9}},
    {{3,6,9,8}}, {{4,5,8,7}}, {{4,8,5,6}}, {{6,8,5,9}}, {{5,8,7,9}} };
static struct simplex slist2[4] = { {{0,3,4}},{{1,5,3}},{{2,4,5}},{{3,5,4}}};
static struct simplex slist1[2] = { {{0,2}}, {{2,1}}};
struct simplex *slistmake ARGS((int));

/***********************************************************************
*
* function: refine_simplex_init()
*
* purpose: set up data structures needed in simplex refining.
*/

void refine_simplex_init( )
{

  dim = web.dimension;
  if ((dim < 1) || (dim > MAXCOORD) )
     kb_error(1545,"Simplex dimension must be between 1 and MAXCOORD.\n",RECOVERABLE);

  switch ( web.dimension )
     { case 4: slist = slistmake(dim); slist_edge = slist3; break;
       case 3: slist = slist3; slist_edge = slist2; return; 
       case 2: slist = slist2; slist_edge = slist1; return;
       case 1: slist = slist1; slist_edge = slist1; return;
       default: slist = slistmake(dim); slist_edge = slistmake(dim-1);
     }
}     

/***********************************************************************
*
* function: slistmake()
*
* purpose: Make list of refined simplices of a simplex.
*/
struct simplex *slistmake(sdim)
int sdim;
{
  int vlist[MAXCOORD+1];
  int i,j;
  struct divedge *elist;
  int numedges;
  int pnum,snum=0;
  struct simplex *sslist;

  vcoord = (REAL(*)[MAXCOORD])temp_calloc((MAXCOORD+1)*(MAXCOORD+2)/2*MAXCOORD,
                  sizeof(REAL));

  /* set up initial arrays */
  /* outer vertices */
  for ( i = 0 ; i <= sdim ; i++ ) 
     { vlist[i] = i;
       if ( i > 0 ) vcoord[i][i-1] = 2.0;
     }
  /* edges */
  numedges = (sdim*(sdim+1))/2;
  elist = (struct divedge *)temp_calloc(numedges,sizeof(struct divedge));
  for ( i = 0, count = 0 ; i < sdim ; i++ )
     for ( j = i+1 ; j <= sdim ; j++ )
        { int pt,k;
          elist[count].endpt[0] = i;
          elist[count].endpt[1] = j;
          elist[count].divpt = pt = sdim+1 + count;
          count++;
          for ( k = 0 ; k < sdim ; k++ )
             vcoord[pt][k] = (vcoord[i][k] + vcoord[j][k])/2.0;
        }

  scount = (1 << sdim);
  sslist = (struct simplex *)mycalloc(scount,sizeof(struct simplex));

  /* do simplices on corners */
  for ( i = 0 ; i <= sdim ; i++ )
     { 
        for ( j = 0, pnum = 0 ; j < numedges ; j++ )
          if ( (elist[j].endpt[0]==vlist[i]) || (elist[j].endpt[1]==vlist[i]))
             sslist[snum].pt[pnum++] = elist[j].divpt;
        sslist[snum].pt[pnum] = vlist[i]; /* add common basepoint */
        snum++;
     }
  /* do interior blob */
  if ( sdim > 1 )
     refine_simplex(sdim,vlist,elist,sslist+snum);

  check_orientation(sslist,sdim);
  temp_free((char*)vcoord);
  temp_free((char*)elist);
    
  return sslist;
}

/************************************************************************
*
* function: check_orientation()
*
* purpose: check orientation of simplices in a subdivision.
*/
void check_orientation(sslist,sdim)
struct simplex *sslist;
int sdim;
{
  int i,j,k;
  MAT2D(mat,MAXCOORD,MAXCOORD);

  for ( i = 0 ; i < scount ; i++ )
     { 
        /* load matrix with side vectors of simplex */
        for ( j = 0 ; j < sdim ; j++ )
          for ( k = 0 ; k < sdim ; k++ )
             mat[j][k] = vcoord[sslist[i].pt[j+1]][k] - vcoord[sslist[i].pt[0]][k];

        /* test and exchange if necessary */
        if ( determinant(mat,sdim) < 0.0 )
          { j = sslist[i].pt[0]; sslist[i].pt[0] = sslist[i].pt[1];
             sslist[i].pt[1] = j;
          }
     }
}

/* Now come the routines for refining all the simplices.  Idea is to
go through simplices refining and keep subdivided edges in hash table
to prevent duplication */

/* hashtable stuff */
struct entry { vertex_id endpt[2], divpt; };
static struct entry *simhashtable;
static int maxload;  /* maximum entries before expanding */
static int hashentries;  /* current number of entries */
static int hashmask;      /* for getting applicable bits of hash key */
static int tablesize;     /* current table size, power of 2 */
#define hash_1(id)     (((id)*701)>>7)
 /*#define hash_2(id)     (((id)*1003)>>+7) */
#define hash_2(id) 1

void init_hash_table()
{
  hashentries = 0;
  tablesize = 64; hashmask = tablesize-1; maxload = 48;
  while ( maxload < web.skel[EDGE].count )
     { hashmask += tablesize; tablesize *= 2; maxload *= 2; }
  simhashtable = (struct entry *)mycalloc(tablesize,sizeof(struct entry));
}

void rehash()
{ struct entry *oldhashtable = simhashtable;
  int i,j;
  struct entry *h;
  unsigned int hash1,hash2,hash;
  int recount=0;

  simhashtable = (struct entry *)mycalloc(2*tablesize,sizeof(struct entry));

  /* rehash all entries */
  hashmask += tablesize;
  for ( i = 0, h = oldhashtable ; i < tablesize ; i++,h++ )
     {
        if ( !h->divpt ) continue;
        hash1 = (unsigned int)(hash_1(h->endpt[0]) + hash_1(h->endpt[1]));
        hash2 = (unsigned int)(hash_2(h->endpt[0]) + hash_2(h->endpt[1]));
        hash = hash1 % hashmask;
        for ( j = 0 ; j < hashentries ; j++ )
          {
             if ( !simhashtable[hash].divpt ) break; /* empty spot */
             hash += hash2;
             hash %= hashmask;
          }
if ( simhashtable[hash].divpt )
  kb_error(1546,"Fatal hash conflict in rehash.\n",UNRECOVERABLE);

        simhashtable[hash] = *h;  /* move in entry */
        recount++;
     }
  if ( recount != hashentries )
     { end_hash_table();
        kb_error(1547,"Hash table expansion error.\n",RECOVERABLE);

     }
  tablesize <<= 1;  /* REAL tablesize */
  maxload <<= 1;
  myfree((char*)oldhashtable);
}

void end_hash_table()
{ 
  myfree((char*)simhashtable);
  simhashtable = NULL;
}

vertex_id simplex_edge_divide(v1,v2)
vertex_id v1,v2;
{ vertex_id newv;
  REAL *x1,*x2;
  REAL newx[MAXCOORD];
  unsigned int hash1,hash2,hash;
  int i;
  conmap_t conmap[MAXCONPER];

  /* get vertices in canonical order */
  if ( v1 > v2 ) { vertex_id temp = v1; v1 = v2; v2 = temp;}

  /* look up in hash table */
  hash1 = (unsigned int)(hash_1(v1) + hash_1(v2));
  hash2 = (unsigned int)(hash_2(v1) + hash_2(v2));
  hash = hash1 % hashmask;
  for ( i = 0 ; i <= hashentries ; i++ )
     { 
        if ( !simhashtable[hash].divpt ) break; /* missing */
        if ( (simhashtable[hash].endpt[0]==v1) && (simhashtable[hash].endpt[1]==v2) )
          return simhashtable[hash].divpt; /* found! */
        hash += hash2;
        hash %= hashmask;
     }
if ( simhashtable[hash].divpt )
  kb_error(1548,"Fatal hash conflict.\n",UNRECOVERABLE);

  /* need to insert new edge and dividing point */
  simhashtable[hash].endpt[0] = v1;
  simhashtable[hash].endpt[1] = v2;
  x1 = get_coord(v1); x2 = get_coord(v2);
  for ( i = 0 ; i < SDIM ; i++ )
     newx[i] = (x1[i] + x2[i])/2;
  simhashtable[hash].divpt = newv = new_vertex(newx,NULLID);
  set_attr(newv,get_vattr(v1)&get_vattr(v2));

  get_v_common_conmap(v1,v2,conmap);
  set_v_conmap(newv,conmap);

  if ( (get_vattr(v1)&BOUNDARY) && (get_boundary(v1) == get_boundary(v2)) )
     set_boundary_num(newv,get_boundary(v1)->num);
  hashentries++;

  /* see if simhashtable needs expanding */
  if ( hashentries > maxload )
     rehash();

  return newv;
}

/***********************************************************************
*
* function: refine_all_simplices()
*
* purpose: execute global refinement in simplex model.
*
* algorithm: For each simplex, sorts vertex in geometric order
*    (for consistency in refining neighboring simplices, and 
*     for consistency in refining covering space), subdivides
*     each 1D edge (with hash list for previously created points),
*     and uses precomputed decompostion.
*/
static REAL sort_vector[10] = { 1.428573894832849893,
    0.38278949278293994, -1.91078013284098892134, 0.6836515788935513,
    0.23888983762994894, -0.8892745828781876439, 0.19926767848989298,
    2.598989877489299135, 0.799299992976615};

void refine_all_simplices()
{
  facet_id f_id;
  edge_id e_id;
  int i,j;
  vertex_id vlist[(MAXCOORD+1)*(MAXCOORD+2)/2]; /* full list of vertices */
  REAL sortval[MAXCOORD+1];
  int svcount;
  ATTR attr;
  int flip; /* inversion counter */

  init_hash_table();  /* for subdivided edges */

  FOR_ALL_FACETS(f_id)
     {
        vertex_id *v = get_facet_vertices(f_id);

        attr = get_fattr(f_id);
        if ( attr & NEWELEMENT ) continue;

        /* transfer old vertices to start of full list */
        /* sorting in geometric order, for adjacent subdivision consistency */
        for ( svcount = 0,flip=0 ; svcount <= web.dimension ; svcount++ )
        { /* insertion sort */
          REAL sortv = SDIM_dot(sort_vector,get_coord(v[svcount]))
             + 10*machine_eps * loc_ordinal(v[svcount]); 
          for ( i = svcount ; i > 0 ; i-- )
             if ( sortval[i-1] > sortv ) 
                {vlist[i] = vlist[i-1]; sortval[i] = sortval[i-1]; flip++;}
             else break;
          vlist[i] = v[svcount];
          sortval[i] = sortv;
        }

        /* go through edges, subdividing */
        for ( i = 0 ; i < web.dimension ; i++ )
          for ( j = i+1 ; j <= web.dimension ; j++ )
             vlist[svcount++] = simplex_edge_divide(vlist[i],vlist[j]);

        /* construct new simplices using pre-computed decomposition */
        /* copy relevant properties of old facet */
        set_attr(f_id,NEWELEMENT); /* so don't repeat refinement */
        for ( i = 0 ; i < (1<<web.dimension) ; i++ )
          { facet_id newf;
             if ( i == 0 ) newf = f_id;    /* re-use old facet */
             else newf = dup_facet(f_id);
             v = get_facet_vertices(newf);
             for ( j = 0 ; j <= web.dimension ; j++ )
                v[j] = vlist[slist[i].pt[j]];
             if ( flip & 1 )  /* restore orientation */
             { vertex_id tmpv = v[0]; v[0] = v[1]; v[1] = tmpv; }
          }
     }


  FOR_ALL_EDGES(e_id)
     {
        vertex_id *v = get_edge_vertices(e_id);

        attr = get_eattr(e_id);
        if ( attr & NEWELEMENT ) continue;

        /* transfer old vertices to start of full list */
        for ( svcount = 0 ; svcount < web.dimension ; svcount++ )
          vlist[svcount] = v[svcount];

        /* go through edges, subdividing */
        for ( i = 0 ; i < web.dimension-1 ; i++ )
          for ( j = i+1 ; j < web.dimension ; j++ )
             vlist[svcount++] = simplex_edge_divide(v[i],v[j]);

        /* construct new edge simplices using pre-computed decomposition */
        /* copy relevant properties of old facet */
        attr |= NEWELEMENT;  /* so don't repeat refinement */
        for ( i = 0 ; i < (1<<(web.dimension-1)) ; i++ )
          { edge_id newedge;
             if ( i == 0 ) newedge = e_id;    /* re-use old facet */
             else newedge = new_edge(NULLID,NULLID,NULLID);
             set_attr(newedge,attr);
             set_original(newedge,get_original(e_id));
             set_e_conmap(newedge,get_e_constraint_map(e_id));
             v = get_edge_vertices(newedge);
             for ( j = 0 ; j < web.dimension ; j++ )
                v[j] = vlist[slist_edge[i].pt[j]];
          }
     }

  end_hash_table();

  reflevel++;
}


/******************************************************************
*
*  function: hi_dim_graph()
*
*  purpose:  graph wire frames of hi dimension simplices projected
*                to 3D.
*/

void hi_dim_graph()
{
  struct graphdata gdata[MAXCOORD+1];
  facet_id f_id;
  int i,j,k,m;

  memset((char*)gdata,0,sizeof(gdata));
  gdata[0].flags |= LIST_FACET;
  FOR_ALL_FACETS(f_id)
  { REAL *x;
     vertex_id *v = get_facet_vertices(f_id);
     if ( breakflag ) break;

     if ( show_expr[FACET] && show_expr[FACET]->start )
       if ( !eval(show_expr[FACET],NULL,f_id,NULL) ) continue;

     gdata[0].id = f_id;
     gdata[0].color = get_facet_color(f_id);
     gdata[0].backcolor = get_facet_backcolor(f_id);
     if ( slice_view_flag )
     { /* do slices */
       int ii;
       m = 0;
       for ( i = 0 ; i < web.dimension ; i++ )
       { x = get_coord(v[i]);
         for ( ii = i+1 ; ii <= web.dimension ; ii++ )
         {
           REAL denom=0.0;
           REAL numer=slice_coeff[SDIM];
           REAL lambda;
           REAL *xx = get_coord(v[ii]);
           for ( j = 0 ; j < SDIM ; j++ )
           { numer -= slice_coeff[j]*xx[j];
             denom += slice_coeff[j]*(x[j] - xx[j]);
           }
           if ( denom == 0.0 )
             continue;
           lambda = numer/denom;
           if ( (lambda < 0.0) || (lambda > 1.0) )
             continue;
           for ( j = 0 ; j < SDIM ; j++ )
             gdata[m].x[j] = lambda*x[j] + (1-lambda)*xx[j];
           m++;
         }
       }
       for ( i = 0 ; i <= m-FACET_VERTS ; i++ )
       { gdata[i].id = f_id;
         gdata[i].color = get_facet_color(f_id);
         gdata[i].backcolor = get_facet_backcolor(f_id);
         (*graph_facet)(gdata+i,NULLID);
       }
     }
     else
     /* do all combos of 3 vertices */
     for ( i = 0 ; i <= web.dimension ; i++ ) 
     { gdata[0].v_id = v[i]; /* for those who can use it */
       x = get_coord(v[i]);
       for ( m = 0 ; m < SDIM ; m++ )
         gdata[0].x[m] = (float)x[m];
       for ( j = i+1 ; j <= web.dimension ; j++ ) 
       { gdata[1].v_id = v[j]; /* for those who can use it */
          x = get_coord(v[j]);
          for ( m = 0 ; m < SDIM ; m++ )
               gdata[1].x[m] = (float)x[m];
          if ( web.representation == STRING ) 
             (*graph_edge)(gdata,NULLID);
          else
          for ( k = j+1 ; k <= web.dimension ; k++ )
          { 
             gdata[2].v_id = v[k]; /* for those who can use it */
             x = get_coord(v[k]);
             for ( m = 0 ; m < SDIM ; m++ )
               gdata[2].x[m] = (float)x[m];
             (*graph_facet)(gdata,NULLID);
          }
       }
     }
  }
}

/******************************************************************
*
*  function: calc_simplex_edge_energy()
*
*  purpose:  find energy contribution of edge integrand
*
*/

void calc_simplex_edge_energy(e_id)
edge_id e_id;
{
  struct constraint *constr;
  int i,j,k,m;
  REAL energy = 0.0;
  REAL side[MAXCOORD][MAXCOORD];
  REAL *sides[MAXCOORD];
  REAL green[MAXCONCOMP];
  conmap_t *conmap;
  REAL midpt[MAXCOORD];  /* evaluation point for integrand */
  int sign;
  REAL kvector[MAXCONCOMP];  /* k-vector representing simplex */
  REAL *x[MAXCOORD];
  vertex_id *v;

  conmap = get_e_constraint_map(e_id);
  if ( get_eattr(e_id) & NEGBOUNDARY ) sign = -1;
  else sign = 1;
  if ( inverted(e_id) ) sign = -sign;

  v = get_edge_vertices(e_id);
  x[0] = get_coord(v[0]);
  for ( k = 1 ; k < web.dimension ; k++ )
      {  x[k] = get_coord(v[k]);
          for ( j = 0 ; j < SDIM ; j++ )
             side[k-1][j] = x[k][j] - x[0][j];
          sides[k-1] = side[k-1];  /* pointers for matrix routines */
      }
  exterior_product(sides,kvector,web.dimension-1,SDIM);

  for ( j = 1 ; j <= (int)conmap[0]; j++ )
    {
     constr = get_constraint(conmap[j]&CONMASK);
     if ( !(constr->attr & CON_ENERGY) ) continue;
     for ( k = 0 ; k < 1 /* gauss1D_num */ ; k++ )
      {
         for ( i = 0 ; i < SDIM ; i++ )
            { midpt[i] = 0.0;
              for ( m = 0 ; m < web.dimension ; m++ )
                 midpt[i] += x[m][i]/web.dimension;
            }
         for ( i = 0 ; i < constr->compcount ; i++ )
                green[i] = eval(constr->envect[i],midpt,NULLID,NULL);
         energy += sign*dot(kvector,green,constr->compcount);
      }

    }

  binary_tree_add(web.total_energy_addends,
            energy/web.simplex_factorial*web.dimension);

}


/******************************************************************
*
*  function: calc_simplex_edge_force()
*
*  purpose:  find force contribution of edge integrand
*
*/

void calc_simplex_edge_force(e_id)
edge_id e_id;
{
  struct constraint *constr;
  int i,j,k,m;
  REAL *hforce,*tforce;
  REAL side[MAXCOORD][MAXCOORD];
  REAL *sides[MAXCOORD];
  REAL green[MAXCONCOMP];
  REAL green_deriv[MAXCONCOMP][MAXCOORD];
  conmap_t * conmap;
  REAL midpt[MAXCOORD];  /* evaluation point for integrand */
  int sign;
  REAL kvector[MAXCONCOMP];  /* k-vector representing simplex */
  REAL *x[MAXCOORD];
  vertex_id *v;
  REAL fudge = web.simplex_factorial/web.dimension;
  REAL grad[MAXCOORD];

  conmap = get_e_constraint_map(e_id);
  if ( get_eattr(e_id) & NEGBOUNDARY ) sign = -1;
  else sign = 1;
  if ( inverted(e_id) ) sign = -sign;

  v = get_edge_vertices(e_id);
  x[0] = get_coord(v[0]);
  for ( k = 1 ; k < web.dimension ; k++ )
      {  x[k] = get_coord(v[k]);
          for ( j = 0 ; j < SDIM ; j++ )
             side[k-1][j] = x[k][j] - x[0][j];
          sides[k-1] = side[k-1];  /* pointers for matrix routines */
      }
  exterior_product(sides,kvector,web.dimension-1,SDIM);

  tforce = get_force(v[0]);
  for ( j = 1 ; j <= (int)conmap[0] ; j++ )
    {
     constr = get_constraint(conmap[j]&CONMASK);
     if ( !(constr->attr & CON_ENERGY) ) continue;
     for ( k = 0 ; k < 1 /* gauss1D_num */ ; k++ )
      {
         for ( i = 0 ; i < SDIM ; i++ )
            { midpt[i] = 0.0;
              for ( m = 0 ; m < web.dimension ; m++ )
                 midpt[i] += x[m][i]/web.dimension;
            }
         for ( i = 0 ; i < constr->compcount ; i++ )
            eval_all(constr->envect[i],midpt,SDIM,&green[i],
                    green_deriv[i],e_id);  /* constraint value and derivs */
          
         /* part due to motion of midpoint changing integrand */
         for ( m = 0 ; m < SDIM ; m++ )
            for ( i = 0 , grad[m] = 0.0 ; i < constr->compcount ; i++ )
              grad[m] += kvector[i]*green_deriv[i][m];
         for ( i = 0 ; i < web.dimension ; i++ )
            { hforce = get_force(v[i]);
              for ( m = 0 ; m < SDIM ; m++ )
                 hforce[m] -= grad[m]/web.dimension/fudge;
            }
         /* part due to changing kvector */
         for ( i = 0 ; i < web.dimension-1 ; i++ ) /* side by side */
         { hforce = get_force(v[i+1]);
            for ( m = 0 ; m < SDIM ; m++ )
            { REAL f;
              sides[i] = identmat[m];
              exterior_product(sides,kvector,web.dimension-1,SDIM);
              f = dot(kvector,green,constr->compcount)/fudge;
              hforce[m] -= f;
              tforce[m] += f;
            }
            sides[i] = side[i];
         }          
      }
    }
}

/******************************************************************
*
*  function: simplex_edge_hessian()
*
*  purpose:  find hessian contribution of edge integrand
*                For linear integrands only.
*
*/

void simplex_edge_hessian(S,e_id,first,second)
struct linsys *S;
edge_id e_id;
REAL **first;  /* first[vert][dim] */
REAL ****second; /* second[vert][vert][dim][dim] */
{
  struct constraint *constr;
  int i,j,k,m,n,p;
  REAL side[MAXCOORD][MAXCOORD];
  REAL *sides[MAXCOORD];
  REAL green[MAXCONCOMP];
  REAL green_deriv_space[MAXCONCOMP][MAXCOORD];
  REAL *green_deriv[MAXCONCOMP];
  conmap_t * conmap;
  REAL midpt[MAXCOORD];  /* evaluation point for integrand */
  int sign;
  REAL kvector[MAXCONCOMP];  /* k-vector representing simplex */
  REAL *x[MAXCOORD];
  vertex_id *v;
  REAL fudge = web.simplex_factorial/web.dimension;
  REAL grad[MAXCOORD];

  for ( i = 0 ; i < MAXCONCOMP ; i++ ) 
     green_deriv[i] = green_deriv_space[i];

  conmap = get_e_constraint_map(e_id);
  if ( get_eattr(e_id) & NEGBOUNDARY ) sign = -1;
  else sign = 1;
  if ( inverted(e_id) ) sign = -sign;

  v = get_edge_vertices(e_id);
  x[0] = get_coord(v[0]);
  for ( k = 1 ; k < web.dimension ; k++ )
      {  x[k] = get_coord(v[k]);
          for ( j = 0 ; j < SDIM ; j++ )
             side[k-1][j] = x[k][j] - x[0][j];
          sides[k-1] = side[k-1];  /* pointers for matrix routines */
      }
  exterior_product(sides,kvector,web.dimension-1,SDIM);

  for ( j = 1 ; j <= (int)conmap[0] ; j++ )
    {
     constr = get_constraint(conmap[j]&CONMASK);
     if ( !(constr->attr & CON_ENERGY) ) continue;
     for ( k = 0 ; k < 1 /* gauss1D_num */ ; k++ )
      {
         for ( i = 0 ; i < SDIM ; i++ )
            { midpt[i] = 0.0;
              for ( m = 0 ; m < web.dimension ; m++ )
                 midpt[i] += x[m][i]/web.dimension;
            }
         for ( i = 0 ; i < constr->compcount ; i++ )
            eval_all(constr->envect[i],midpt,SDIM,&green[i],
                    green_deriv[i],e_id);  /* constraint value and derivs */
          
         /* part due to motion of midpoint changing integrand */
         vec_mat_mul(kvector,green_deriv,grad,constr->compcount,SDIM);
         for ( i = 0 ; i < web.dimension ; i++ )
            { for ( m = 0 ; m < SDIM ; m++ )
                 first[i][m] += grad[m]/web.dimension/fudge;
            }
         /* part due to changing kvector */
         for ( i = 0 ; i < web.dimension-1 ; i++ ) /* side by side */
         { for ( m = 0 ; m < SDIM ; m++ )
            { REAL f;
              sides[i] = identmat[m];
              exterior_product(sides,kvector,web.dimension-1,SDIM);
              f = dot(kvector,green,constr->compcount)/fudge;
              first[i+1][m] += f;
              first[0][m] -= f;
              vec_mat_mul(kvector,green_deriv,grad,constr->compcount,SDIM);
              for ( n = 0 ; n < web.dimension - 1 ; n++ )
                for ( p = 0 ; p < SDIM ; p++ )
                  { f = grad[p]/web.dimension/fudge;
                     second[i+1][n+1][m][p] += f;
                     second[i+1][0][m][p] += f;
                     second[0][n+1][m][p] -= f;
                     second[0][0][m][p] -= f;
                     second[n+1][i+1][p][m] += f;
                     second[n+1][0][p][m] -= f;
                     second[0][i+1][p][m] += f;
                     second[0][0][p][m] -= f;
                  }
            }
            sides[i] = side[i];
         }          
         /* part due to changing kvector - 2 components */
         for ( i = 0 ; i < web.dimension-1 ; i++ ) /* side by side */
            for ( n = 0 ; n < web.dimension - 1 ; n++ )
             { if ( i == n ) continue;
                for ( p = 0 ; p < SDIM ; p++ )
                  for ( m = 0 ; m < SDIM ; m++ )
                    { REAL f;
                      sides[i] = identmat[m];
                      sides[n] = identmat[p];
                      exterior_product(sides,kvector,web.dimension-1,SDIM);
                      f = dot(kvector,green,constr->compcount)/fudge;
                      second[i+1][n+1][m][p] += f;
                      second[0][n+1][m][p] -= f;
                      second[i+1][0][m][p] -= f;
                      second[0][0][m][p] += f;
                      sides[i] = side[i];
                      sides[n] = side[n];
                    }
             }          
      }
    }
}

/*********************************************************************
*
*  function: simplex_long_edges()
*
*  purpose: divide long 1-D edges in simplex model.
*              Method: Make list of all edges
*                         Find long edge
*                         Put in bisecting vertex
*                         Find adjacent simplices
*                         Divide adjacent simplices in two
*
*                         Returns number of edges divided
*/

/* working list of edges and adjacent facets */
#define MAXFACET 10
struct w_edge { vertex_id v[2];  /* endpoints */
                int fcount;  /* number of adjacent simplices */
                facet_id f[MAXFACET]; /* adjacent simplices */
              };
/* initial edge list for sorting */
struct s_edge { vertex_id v[2]; /* endpoints in native order */
                facet_id f;  /* facet with edge */
              };
int se_comp ARGS((struct s_edge *,struct s_edge *));

int se_comp(a,b) /* comparison function for sorting edge list */
struct s_edge *a,*b;
{ if ( a->f < b->f ) return -1;
  if ( a->f > b->f ) return 1;
  return 0;
}

/**********************************************************************
*
* function: simplex_long_edges()
*
* purpose: refine all 1-edges greater than given length.
*/
#ifdef ANSI_DEF
int simplex_long_edges(REAL max_len)
#else
int simplex_long_edges(max_len)
REAL max_len;
#endif
{ struct s_edge *se,*se_ptr;
  int se_count; /* number of edges in se */
  struct w_edge *we,*we_ptr;
  int we_count; /* number of edges in we */
  facet_id f_id;
/* inverse index facet to edge list, indexed by facet ordinal */
  struct w_edge **fedge;
  int fedge_max; /* number of fedge pointers allocated */
  int edges_per = (web.dimension*(web.dimension+1))/2;
  int divcount = 0;  /* number of edges divided */
  int i,j,k,m,n;
  vertex_id v0,v1;
  struct w_edge **fl,**newfl;
  int top;

  /* make initial edge list */
  se_count = web.skel[FACET].count*edges_per;
  se = (struct s_edge*)temp_calloc(se_count,sizeof(struct s_edge));
  se_ptr = se;
  FOR_ALL_FACETS(f_id)
    { vertex_id *v = get_facet_vertices(f_id);
      for ( i = 0 ; i < web.dimension ; i++ ) /* tail vertex */
         for ( j = i+1 ; j <= web.dimension ; j++ ) /* head vertex */
            { se_ptr->f = f_id;
              if ( v[i] < v[j] )
                 { se_ptr->v[0] = v[i]; se_ptr->v[1] = v[j]; }
              else  { se_ptr->v[0] = v[j]; se_ptr->v[1] = v[i]; }
              se_ptr++;
            }
    }
  /* sort list */
  qsort((char*)se,se_count,sizeof(struct s_edge),FCAST se_comp);
  /* count edges and max facets per edge */
  v0 = v1 = NULLVERTEX;  /* track current edge */
  for ( k = 0, se_ptr = se, we_count = top = 0 ; k < se_count ; k++, se_ptr++ )
    { if ( (se_ptr->v[0] == v0) && (se_ptr->v[1] == v1)  ) /* same as before */
         top++;
      else  /* new */
         { if ( top > MAXFACET )
              kb_error(1549,"Edge adjacent to too many simplices.\n",RECOVERABLE);

            v0 = se_ptr->v[0]; v1 =se_ptr->v[1];
            we_count++;
         }
    }
  /* make working lists */
  we = (struct w_edge*)temp_calloc(we_count,sizeof(struct w_edge));
  fedge_max = web.skel[FACET].max_ord+100;
  fedge = (struct w_edge**)temp_calloc(fedge_max*edges_per,
                         sizeof(struct w_edge *));
  v0 = v1 = NULLVERTEX;  /* track current edge */
  for ( k = 0, se_ptr = se, we_ptr = we-1 ; k < se_count ; k++, se_ptr++ )
    { if ( (se_ptr->v[0] != v0) || (se_ptr->v[1] != v1)  ) /* new */
         { we_ptr++;
           v0 = we_ptr->v[0] = se_ptr->v[0]; 
           v1 = we_ptr->v[1] = se_ptr->v[1];
         }
      we_ptr->f[we_ptr->fcount++] = se_ptr->f;
      fl=fedge+edges_per*loc_ordinal(se_ptr->f) ; while ( *fl ) fl++;
      *fl = we_ptr; /* inverse list */
    }
  temp_free((char*)se);  /* don't need anymore */

  /* go through working list, chopping long edges */
  for ( we_ptr = we, k = 0 ; k < we_count ; k++,we_ptr++ )
     { 
        REAL *x1, *x2;
        REAL newx[MAXCOORD];
        REAL side[MAXCOORD];
        conmap_t conmap[MAXCONPER];
        vertex_id newv;
        facet_id newf;
        REAL len;

        v0 = we_ptr->v[0]; v1 = we_ptr->v[1];
        x1 = get_coord(v0); x2 = get_coord(v1);
        for ( i = 0 ;  i < SDIM ; i++ ) side[i] = x1[i] - x2[i];
        len = sqrt(SDIM_dot(side,side));  
        if ( len < max_len ) continue;  

        /* now have long edge */
        /* put vertex in middle */
        for ( i = 0 ; i < SDIM ; i++ )
          newx[i] = (x1[i] + x2[i])/2;
        newv = new_vertex(newx,NULLID);
        set_attr(newv,get_vattr(v0)&get_vattr(v1));
        get_v_common_conmap(v0,v1,conmap);
        for ( j = 1 ; j <= (int)conmap[0] ; j++ )
        { int c = conmap[j] & CONMASK;
          set_v_constraint_map(newv,c);
          if ( get_v_constraint_status(v0,c) && get_v_constraint_status(v1,c) )
             set_v_constraint_status(newv,c);
        }
        if ( get_vattr(v0) & BOUNDARY )
         if ( get_boundary(v0) == get_boundary(v1) )
          set_boundary_num(newv,get_boundary(v0)->num);
        /* construct new simplices */
        for ( n = 0 ; n < we_ptr->fcount ; n++ )
          { 
             vertex_id *v,*oldv;
             f_id = we_ptr->f[n];
             /* copy relevant properties of old facet */
             newf = dup_facet(f_id);
             v = get_facet_vertices(newf);
             oldv = get_facet_vertices(f_id);
             for ( j = 0 ; j <= web.dimension ; j++ )
             { if ( equal_id(v0,oldv[j]) ) v[j] = newv;
               else v[j] = oldv[j];
               if ( equal_id(v1,oldv[j]) ) oldv[j] = newv;
             }
             /* update working list */
             if ( web.skel[FACET].max_ord >= fedge_max )
             { fedge_max = web.skel[FACET].max_ord+100;
               fedge = (struct w_edge **)temp_realloc((char*)fedge,
                               fedge_max*edges_per*sizeof(struct w_edge *));
             }
             fl = fedge + loc_ordinal(f_id)*edges_per;
             newfl = fedge + loc_ordinal(newf)*edges_per;
             for ( m = 0 ; m<edges_per ; fl++,m++ )
                { if ( !*fl || (*fl)->v[0] == v0 ) continue; /* is ok */
                  if ( (*fl)->v[1] == v1 )
                     { /* replace f_id with newf */
                        for ( j = 0 ; j < (*fl)->fcount ; j++ )
                          if ( f_id == (*fl)->f[j] )
                             { (*fl)->f[j] = newf; break; }
                        *(newfl++) = *fl; /* transfer edge to new facet */
                        *fl = NULL;
                     }
                  else /* add newf to edge */
                     { if ( (*fl)->fcount >= MAXFACET )
                          kb_error(1550,"Edge has too many simplices.\n",RECOVERABLE);

                        (*fl)->f[(*fl)->fcount++] = newf;
                        *(newfl++) = *fl; /* add edge to new facet */
                     }
                }
          }
      divcount++;
     }
  temp_free((char*)we);
  temp_free((char*)fedge);
  return divcount;
}


/**********************************************************************
*
* function: simplex_tiny_edges()
*
* purpose: delete all 1-edges shorter than given length.
*/
#ifdef ANSI_DEF
int simplex_tiny_edges(REAL min_len)
#else
int simplex_tiny_edges(min_len)
REAL min_len;
#endif
{ 
  int delcount = 0;  /* number of edges divided */
  int i,j,k;
  facet_id f_id;

  FOR_ALL_FACETS(f_id)
  { vertex_id *fv = get_facet_vertices(f_id);
     int del;

     /* check it really exists */
     if ( !(get_fattr(f_id) & ALLOCATED) ) continue; 
     /* pairs of edges */
     del = 0;
     for ( i = 0 ; i <= web.dimension ; i++ )
     { for ( j = i+1 ; j <= web.dimension ; j++ )
          { REAL *x = get_coord(fv[i]);
             REAL *y = get_coord(fv[j]);
             REAL dist;
             for ( k = 0, dist = 0.0 ; k < SDIM ; k++ )
                dist += (x[k]-y[k])*(x[k]-y[k]);
             dist = sqrt(dist);
             if ( dist >= min_len ) continue;
              /* now delete */
             del = simplex_delete_edge(fv[i],fv[j]);
             delcount += del;
             if ( del ) break;
          }
         if ( del ) break;
      }
  }
  if ( delcount ) top_timestamp = ++global_timestamp;
  return delcount;
}

/***********************************************************************
*
* function simplex_delete_edge()
*
* purpose: delete 1-edge between two given vertices.
*             Checks for consistent boundaries and constraints.
*             Does not move kept vertex.
*             Returns 0 or 1 edges deleted.
*/
int simplex_delete_edge(v1,v2)
vertex_id v1,v2;
{ vertex_id keepv; /* vertex to keep */
  vertex_id throwv; /* vertex to delete */
  int attri,attrj;
  facet_id f_id,fstart,f_ids[100];
  int k,m,n;

  /* check attributes */
  keepv = NULLID;
  attri = get_vattr(v1); attrj = get_vattr(v2);
  if ( (attri & BOUNDARY) && (attrj & CONSTRAINT) ) return 0;
  if ( (attri & CONSTRAINT) && (attrj & BOUNDARY) ) return 0;
  if ( (attri & BOUNDARY) && (attrj & BOUNDARY)
         && (get_boundary(v1) != get_boundary(v2)) ) return 0;
  if ( attri & BOUNDARY ) keepv = v1;
  else if ( attri & BOUNDARY ) keepv = v2;
  if ( (attri & CONSTRAINT) && (attrj & CONSTRAINT) )
  { conmap_t *conmapi = get_v_constraint_map(v1);
     conmap_t *conmapj = get_v_constraint_map(v2);
     int common,kk;
     for ( k = 1, common=0 ; k <= (int)conmapi[0] ; k++ )
        for ( kk = 1 ; kk <= (int)conmapj[0] ; kk++ )
          if ( (conmapi[k]&CONMASK) == (conmapj[kk]&CONMASK) )
                        common++;
     if ( conmapi[0] < conmapj[0] )  
      { if ( common < (int)conmapi[0] ) return 0;
         keepv = v2;
      }
     else { if ( common < (int)conmapj[0] ) return 0;
              keepv = v1;
            }
    }
    else if ( attri & CONSTRAINT ) keepv = v1;
    else if ( attrj & CONSTRAINT ) keepv = v2;
    if ( keepv == NULLID ) keepv = v1;

    throwv = (keepv==v1) ? v2 : v1;

    /* change vertex in facets around throwv */
    /* and delete all facets around keepv that have throwv */
    f_id = fstart = get_vertex_first_facet(throwv);
    for ( n = 0 ; n < 100 ;  )
    { f_ids[n++] = f_id;
      f_id =  get_next_vertex_facet(throwv,f_id);
      if ( equal_id (f_id,fstart) ) break;
    }
    for ( m = 0 ; m < n ; m++ )
    { vertex_id * fv = get_facet_vertices(f_ids[m]);
      for ( k = 0 ; k <= web.dimension ; k++ )
         { if ( equal_id(fv[k],keepv) )
            { free_element(f_ids[m]); break; }
            if ( equal_id(fv[k],throwv) ) fv[k] = keepv;
         }
      } 
    free_element(throwv);
    return 1;

}

/***********************************************************************
*
* function simplex_delete_facet()
*
* purpose: Deletes facet by deleting shortest 1-edge.
*/
int simplex_delete_facet(f_id)
facet_id f_id;
{
  int j,k,m,n;
  struct sid { vertex_id v1,v2; REAL length; } sids[MAXCOORD*MAXCOORD],*ss;
  int sscount;
  vertex_id *v;
  REAL sum;

  v = get_facet_vertices(f_id);
  for ( m = 0,ss=sids,sscount = 0 ; m < web.dimension ; m++ )
     for ( n = m+1 ; m <= web.dimension ; m++,ss++,sscount++ )
     { REAL *x1 = get_coord(v[m]);
        REAL *x2 = get_coord(v[n]);
        for (  k = 0, sum = 0.0 ; k < web.dimension ; k++ )
          sum += (x1[k]-x2[k])*(x1[k]-x2[k]);
        for ( j = sscount ; j > 0 ; j-- )
          if ( sids[j-1].length < sum )  break;
          else sids[j] = sids[j-1];
        sids[j].length = sum;
        sids[j].v1 = v[m];
        sids[j].v2 = v[n];
     }
  for ( k = 0 ; k < sscount ; k++ )
     if ( simplex_delete_edge(sids[k].v1,sids[k].v2) )
        return 1;
  return 0;  
}

/********************************************************************
*
* function: simplex_hessian()
*
* purpose: Overall control for simplex hessian
*
*/

void simplex_hessian(S,rhs)
struct linsys *S;
REAL *rhs;
{ REAL **first;
  REAL ****second;
  REAL **p1,**p2;
  int i,j,k;
  int head,tail;
  edge_id e_id;
  REAL g[MAXCOORD]; /* projected to constraint */

  /* do facets */
  simplex_facet_hessian(S,rhs);

  /* do edges */
  first = dmatrix(0,web.dimension-1,0,SDIM-1);
  second = dmatrix4(web.dimension,web.dimension,SDIM,SDIM);
  p1 = dmatrix(0,MAXCOORD-1,0,MAXCOORD-1);
  p2 = dmatrix(0,MAXCOORD-1,0,MAXCOORD-1);

  FOR_ALL_EDGES(e_id)
     {  
        struct hess_verlist *v[MAXCOORD];
        vertex_id *v_id = get_edge_vertices(e_id);

        for ( i = 0 ; i < web.dimension ; i++ )
          v[i] = get_vertex_vhead(v_id[i]);

        memset((char*)first[0],0,sizeof(REAL)*web.dimension*SDIM);
        memset((char*)second[0][0][0],0,sizeof(REAL)*
            web.dimension*web.dimension*SDIM*SDIM);

        simplex_edge_hessian(S,e_id,first,second);
    
        /* first derivatives on right hand side */
        for ( i = 0 ; i < web.dimension ; i++ )
          {
             if ( v[i]->proj )
                { vec_mat_mul(first[i],v[i]->proj,g,SDIM,v[i]->freedom);
                  for ( k = 0 ; k < v[i]->freedom ; k++ )
                     rhs[v[i]->rownum+k] -= g[k];
                }
             else
                for ( k = 0 ; k < v[i]->freedom ; k++ )
                     rhs[v[i]->rownum+k] -= first[i][k];
          }
        /* second derivatives */
        for ( i = 0 ; i < web.dimension ; i++ )
         for ( j = i ; j < web.dimension ; j++ )
          {
             if ( (v[i]->freedom==0) || (v[j]->freedom==0) ) continue;
             if ( loc_ordinal(v_id[i]) > loc_ordinal(v_id[j]) )
                 { tail = i; head = j; }
             else { tail = j; head = i; }

             fill_mixed_entry(S,v_id[tail],v_id[head],second[tail][head]);
            }
      }
  if ( first ) free_matrix(first);
  if ( second ) free_matrix4(second);
  if ( p1 ) free_matrix(p1);
  if ( p2 ) free_matrix(p2);
}

/********************************************************************
*
* function: simplex_facet_hessian()
*
* purpose: fill in hessian matrix with area derivatives
*
*/

void simplex_facet_hessian(S,rhs)
struct linsys *S;
REAL *rhs;
{
  int i,j,k;
  int m;
  facet_id f_id;
  REAL **hess,**p1,**p2;
  REAL **A;  /* dots of sides matrix, and later inverse */        
  REAL **AS; /* sides x A inverse */
  REAL **SAS; /* sides x A inverse x sides */
  REAL **coord; /* vertex cooordinates */
  REAL Q;  /* determinant of A*/
  REAL coeff;  /* common factor */
  REAL **side;    /* sides of facet cone, from origin of homogenous */
  int head,tail;

  /* fill in sparse matrix rows and volume constraint rows */
  p1 = dmatrix(0,MAXCOORD-1,0,MAXCOORD-1);
  p2 = dmatrix(0,MAXCOORD-1,0,MAXCOORD-1);
  hess = dmatrix(0,MAXCOORD-1,0,MAXCOORD-1);
  A = dmatrix(0,MAXCOORD,0,MAXCOORD);
  AS = dmatrix(0,MAXCOORD-1,0,MAXCOORD-1);
  SAS = dmatrix(0,MAXCOORD-1,0,MAXCOORD-1);
  coord = dmatrix(0,MAXCOORD,0,MAXCOORD);
  side = dmatrix(0,MAXCOORD,0,MAXCOORD);

  FOR_ALL_FACETS(f_id)
     {
        struct hess_verlist *v[MAXCOORD+1];
        vertex_id *v_id = get_facet_vertices(f_id);

        get_facet_verts(f_id,coord,NULL);
        for ( i = 0 ; i < web.dimension ; i++ )
          for ( j = 0 ; j < SDIM ; j++ )
             side[i][j] = coord[i][j] - coord[web.dimension][j];

        /* matrix of inner products */
        for ( i = 0 ; i < web.dimension ; i++ )
          for ( j = i ; j < web.dimension ; j++ )
             A[i][j] = A[j][i] = SDIM_dot(side[i],side[j]);
        Q = det_adjoint(A,web.dimension);
        if ( Q == 0.0 ) continue;
        for ( i = 0 ; i < web.dimension ; i++ )
          for ( j = i ; j < web.dimension ; j++ )
             A[i][j] = A[j][i] /= Q;  /* make adjoint into inverse */
        /* now make rows and columns of expanded A add to 0 for last vertex */
        A[web.dimension][web.dimension] = 0.0;
        for ( i = 0 ; i < web.dimension ; i++ )
          { REAL sum;
             for ( j = 0, sum = 0.; j < web.dimension ; j++ )
                sum += A[i][j]; 
             A[i][web.dimension] = A[web.dimension][i] = -sum;
             A[web.dimension][web.dimension] += sum;
          }
        mat_mult(A,side,AS,web.dimension+1,web.dimension,SDIM);
        tr_mat_mul(side,AS,SAS,web.dimension,SDIM,SDIM);
        coeff = get_facet_density(v_id[i])*sqrt(Q)/web.simplex_factorial;

        for ( i = 0 ; i <= web.dimension ; i++ )
          v[i] = get_vertex_vhead(v_id[i]);

        /* first derivatives of area */
        for ( i = 0 ; i <= web.dimension ; i++ )
          {
             REAL g[MAXCOORD]; /* projected to constraint */

             if ( v[i]->freedom == 0 ) continue;
             if ( v[i]->proj )
                { vec_mat_mul(AS[i],v[i]->proj,g,SDIM,v[i]->freedom);
                  for ( k = 0 ; k < v[i]->freedom ; k++ )
                     rhs[v[i]->rownum+k] -= coeff*g[k];
                }
             else
                for ( k = 0 ; k < v[i]->freedom ; k++ )
                     rhs[v[i]->rownum+k] -= coeff*AS[i][k];
          }

        /* second derivatives */
        for ( i = 0 ; i <= web.dimension ; i++ )
         for ( j = i ; j <= web.dimension ; j++ )
          {
             if ( (v[i]->freedom==0) || (v[j]->freedom==0) ) continue;
             if ( loc_ordinal(v_id[i]) > loc_ordinal(v_id[j]) )
                 { tail = i; head = j; }
             else { tail = j; head = i; }

             for ( m = 0 ; m < SDIM ; m++ )
                for ( k = 0 ; k < SDIM ; k++ )
                  hess[m][k] = coeff*((m==k?A[tail][head]:0.0) - AS[tail][k]*
                      AS[head][m] - SAS[m][k]*A[tail][head]
                      + AS[tail][m]*AS[head][k]);
             fill_mixed_entry(S,v_id[tail],v_id[head],hess);
            }
     }
  if ( hess ) free_matrix(hess);
  if ( p1 ) free_matrix(p1);
  if ( p2 ) free_matrix(p2);
  if ( A ) free_matrix(A);
  if ( AS ) free_matrix(AS);
  if ( SAS ) free_matrix(SAS);
  if ( coord ) free_matrix(coord);
  if ( side ) free_matrix(side);
}  /* end simplex_hessian() */



/*********************************************************************

                    simplex_vector_integral  method

Integral of vectorfield over facet.  nD facet in (n+1)D only.

*********************************************************************/
REAL simplex_vector_integral_all ARGS((struct qinfo*,int));
/*********************************************************************
*
* function: simplex_vector_integral_init()
*
* purpose:  Check illegalities
*
*/

void simplex_vector_integral_init(mode,mi)
int mode;
struct method_instance *mi;
{
  if ( web.dimension != SDIM-1 )
     kb_error(1551,"simplex_vector_integral method only for N-1 D facets in N D.\n",

        RECOVERABLE);
  if ( web.modeltype != LINEAR ) 
     kb_error(1552,"simplex_vector_integral method only for LINEAR model.\n",

        RECOVERABLE);
}

/*********************************************************************
*
* function: simplex_vector_integral()
*
* purpose:  method value
*
*/

REAL simplex_vector_integral(f_info)
struct qinfo *f_info;
{ int i,m,j;
  REAL value=0.0;
  MAT2D(mat,MAXCOORD,MAXCOORD);
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;

  for ( m = 0 ; m < gauss2D_num ; m++ )
  { for ( i = 0 ; i < web.dimension ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
          mat[i][j] = f_info->sides[0][i][j];
    for ( j = 0 ; j < SDIM ; j++ )
      mat[web.dimension][j] = 
        eval(METH_INSTANCE(f_info->method)->expr[j],f_info->gauss_pt[m],NULLID,NULL);
     value += gauss2Dwt[m]*det_adjoint(mat,SDIM);
  }
  return sign*value/web.simplex_factorial; 
}

/*********************************************************************
*
* function: simplex_vector_integral_grad()
*
* purpose:  method gradient
*
*/


REAL simplex_vector_integral_grad(f_info)
struct qinfo *f_info;
{ int i,m,j,k,jj;
  REAL value = 0.0;
  REAL val[MAXCOORD];
  REAL derivs[MAXCOORD][MAXCOORD];
  MAT2D(mat,MAXCOORD,MAXCOORD);
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;

  for ( m = 0 ; m < gauss2D_num ; m++ )
  { REAL weight = sign*gauss2Dwt[m];
     for ( i = 0 ; i < web.dimension ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
          mat[i][j] = f_info->sides[0][i][j];  /* mat destroyed by det */
     for ( j = 0 ; j < SDIM ; j++ )
     { eval_all(METH_INSTANCE(f_info->method)->expr[j],f_info->gauss_pt[m],SDIM,val+j,
                                                                         derivs[j],f_info->id);
        mat[web.dimension][j] = val[j];
     }
     value += weight*det_adjoint(mat,SDIM);
     for ( k = 0 ; k < ctrl_num-1; k++ )
      for ( j = 0 ; j < SDIM ; j++ ) 
        { f_info->grad[k+1][j] += weight*mat[j][k];
          f_info->grad[0][j]    -= weight*mat[j][k];
        }
     for ( k = 0 ; k < ctrl_num ; k++ )
      for ( j = 0 ; j < SDIM ; j++ ) 
        for ( jj = 0 ; jj < SDIM ; jj++ ) 
        {
          f_info->grad[k][j] += weight
                *gpoly[m][k]*derivs[jj][j]*mat[jj][web.dimension];
        }
  }
  for ( j = 0 ; j < SDIM ; j++ ) 
     for ( k = 0 ; k < f_info->vcount ; k++ )
        f_info->grad[k][j] /= web.simplex_factorial;
  return value/web.simplex_factorial;  
}

/*********************************************************************
*
* function: simplex_vector_integral_hess()
*
* purpose:  method hessian
*
*/
REAL simplex_vector_integral_hess(f_info)
struct qinfo *f_info;
{ return simplex_vector_integral_all(f_info,METHOD_HESSIAN);
}
REAL simplex_vector_integral_all(f_info,mode)
struct qinfo *f_info;
int mode;
{ int i,m,j,k,jj,ii,kk;
  REAL sum,value = 0.0;
  REAL val[MAXCOORD];
  REAL derivs[MAXCOORD][MAXCOORD];
  MAT3D(seconds,MAXCOORD,MAXCOORD,MAXCOORD);
  REAL ****ada;
  REAL **tr;
  MAT2D(mat,MAXCOORD,MAXCOORD);
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  struct gauss_lag *gl = (web.modeltype == LAGRANGE) ? 
       &gauss_lagrange[web.dimension][web.gauss2D_order] : NULL;
  int cpts = (web.modeltype == LAGRANGE) ? gl->lagpts : ctrl_num;
  REAL **gp = (web.modeltype == LAGRANGE) ? gl->gpoly : gpoly;

  ada = dmatrix4(cpts,SDIM,SDIM,SDIM);
  tr = dmatrix(0,cpts,0,SDIM);

  for ( m = 0 ; m < gauss2D_num ; m++ )
  { REAL det;
    REAL weight = sign*gauss2Dwt[m]/web.simplex_factorial;
    for ( i = 0 ; i < web.dimension ; i++ )
       for ( j = 0 ; j < SDIM ; j++ )
          mat[i][j] = f_info->sides[0][i][j];  /* mat destroyed by det */
    for ( j = 0 ; j < SDIM ; j++ )
    { eval_second(METH_INSTANCE(f_info->method)->expr[j],f_info->gauss_pt[m],
        SDIM,val+j, derivs[j],seconds[j],f_info->id);
      mat[web.dimension][j] = val[j];
    }
    det = det_adjoint(mat,SDIM);

    value += weight*det;
    if ( mode == METHOD_VALUE ) continue;

    for ( k = 0 ; k < cpts ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
      { for ( i = 0 ; i < SDIM ; i++ )
        { for ( jj = 0; jj < SDIM ; jj++ )
            ada[k][j][i][jj] = gp[m][k]*mat[i][web.dimension]*derivs[jj][j];
        }
      }
    for ( k = 1 ; k < cpts ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
      { for ( i = 0 ; i < SDIM ; i++ )
         { ada[k][j][i][j] += mat[i][k-1];
           ada[0][j][i][j] -= mat[i][k-1];
         }
      }
    for ( k = 0 ; k < cpts ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
      { for ( i = 0,sum = 0.0 ; i < SDIM ; i++) sum += ada[k][j][i][i];
        tr[k][j] = sum;
      }
    /* gradient */
    for ( k = 0 ; k < cpts; k++ )
      for ( j = 0 ; j < SDIM ; j++ ) 
      {  f_info->grad[k][j] += weight*tr[k][j];
      }
    if ( mode == METHOD_GRADIENT ) continue;

    /* hessian */
    for ( k = 0 ; k < cpts  ; k++ )
       for ( kk = 0 ; kk < cpts ; kk++ )
         for ( j = 0 ; j < SDIM ; j++ )
           for ( jj = 0 ; jj < SDIM ; jj++ )
           { REAL h;
             h = tr[k][j]*tr[kk][jj]/det;
             for ( i = 0 ; i < SDIM ; i++ ) 
               h += gp[m][k]*gp[m][kk]*seconds[i][j][jj]*mat[i][web.dimension];
             for ( i = 0 ; i < SDIM ; i++ )
               for ( ii = 0 ; ii < SDIM ; ii++ )
                 h -= ada[k][j][i][ii]*ada[kk][jj][ii][i]/det;                
             f_info->hess[k][kk][j][jj]  += weight*h;
           }
  }

  free_matrix(tr);
  free_matrix4(ada);
  return value;  
}


/*********************************************************************

                    simplex_k_vector_integral  method

Integral of (n-k)vectorfield over k-facet. Will do edge equally well. 

*********************************************************************/
REAL simplex_k_vector_integral_all ARGS((struct qinfo*,int));
/*********************************************************************
*
* function: simplex_k_vector_integral_init()
*
* purpose:  Check illegalities
*
*/

void simplex_k_vector_integral_init(mode,mi)
int mode;
struct method_instance *mi;
{
  if ( web.modeltype == QUADRATIC ) 
     kb_error(1553,"simplex_k_vector_integral method not for QUADRATIC model.\n",
        RECOVERABLE);
}

/*********************************************************************
*
* function: simplex_k_vector_integral()
*
* purpose:  method value
*
*/

REAL simplex_k_vector_integral(f_info)
struct qinfo *f_info;
{ int i,m,j,k;
  REAL value=0.0;
  MAT2D(mat,MAXCOORD,MAXCOORD);
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  int order = METH_INSTANCE(f_info->method)->vec_order;
  
  if ( web.modeltype == LAGRANGE ) return lagrange_k_vector_integral(f_info);

  for ( m = 0 ; m < gauss2D_num ; m++ )
  { for ( i = 0 ; i < order ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
          mat[i][j] = f_info->sides[0][i][j];
     for ( k = 0 ; k+order < SDIM ; k++ )
        for ( j = 0 ; j < SDIM ; j++ )
            mat[k+order][j] = 
              eval(METH_INSTANCE(f_info->method)->expr[j+k*SDIM],
                  f_info->gauss_pt[m],NULLID,NULL);
     value += gauss2Dwt[m]*det_adjoint(mat,SDIM);
  }
  return sign*value/web.simplex_factorial; 
}

/*********************************************************************
*
* function: simplex_k_vector_integral_grad()
*
* purpose:  method gradient
*
*/

REAL simplex_k_vector_integral_grad(f_info)
struct qinfo *f_info;
{ int i,m,j,k,jj;
  REAL value = 0.0;
  REAL val[MAXCOORD];
  REAL derivs[MAXCOORD][MAXCOORD][MAXCOORD];
  MAT2D(mat,MAXCOORD,MAXCOORD);
  REAL sign = (get_fattr(f_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  int order = METH_INSTANCE(f_info->method)->vec_order;
  
  if ( web.modeltype == LAGRANGE ) 
     return lagrange_k_vector_integral_grad(f_info);

  for ( m = 0 ; m < gauss2D_num ; m++ )
  { REAL weight = sign*gauss2Dwt[m];
     for ( i = 0 ; i < order ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
          mat[i][j] = f_info->sides[0][i][j];  /* mat destroyed by det */
     for ( k = 0 ; k+order < SDIM ; k++ )
     for ( j = 0 ; j < SDIM ; j++ )
     { eval_all(METH_INSTANCE(f_info->method)->expr[j+k*SDIM],
         f_info->gauss_pt[m],SDIM,val+j,derivs[k][j],f_info->id);
       mat[k+order][j] = val[j];
     }
     value += weight*det_adjoint(mat,SDIM);
     for ( k = 0 ; k < ctrl_num-1; k++ )
      for ( j = 0 ; j < SDIM ; j++ ) 
        { f_info->grad[k+1][j] += weight*mat[j][k];
          f_info->grad[0][j]    -= weight*mat[j][k];
        }
     for ( k = 0 ; k < ctrl_num ; k++ )
      for ( j = 0 ; j < SDIM ; j++ ) 
        for ( jj = 0 ; jj < SDIM ; jj++ ) 
         for ( i = 0 ; i+order < SDIM ; i++ )
         {
           f_info->grad[k][j] += weight
                *gpoly[m][k]*derivs[i][jj][j]*mat[jj][i+order];
         }
  }
  for ( j = 0 ; j < SDIM ; j++ ) 
     for ( k = 0 ; k < f_info->vcount ; k++ )
        f_info->grad[k][j] /= web.simplex_factorial;
  return value/web.simplex_factorial;  
}

/*********************************************************************
*
* function: simplex_k_vector_integral_hess()
*
* purpose:  method hessian and  gradient 
*
*/

REAL simplex_k_vector_integral_hess(f_info)
struct qinfo *f_info;
{
  if ( web.modeltype != LAGRANGE )
     kb_error(1554,"simplex_k_vector_integral hess only for LAGRANGE.\n",
         RECOVERABLE);

  return lagrange_k_vector_integral_hess(f_info);
}

/***********************************************************************
*
* function:  simplex_vertex_normal()
*
* purpose: calculate normal to surface at vertex.
*             Current version assumes no singular points,
*             and just averages all adjacent facet normals
*             to get volume gradient.
*  
*  Note: Kludge on TRIPLE_PT vertices returns normal to constraint
*    if one, for wetcone stuff.
*
*  Output:  normal basis
*  Return value: dimension of normal space
*/
int simplex_vertex_normal(v_id,norm)
vertex_id v_id;
REAL **norm;  /* for return */
{ facet_id f_id,startf;
  MAT2D(sides,MAXCOORD,MAXCOORD);
  int i,j;

  if ( SDIM > web.dimension+1 )
  { kb_error(2182,"Can do hessian_normal only for hypersurfaces in simplex model.\n",RECOVERABLE );
  }

  if ( get_vattr(v_id) & TRIPLE_PT )
  { REAL xx[MAXCOORD],val;
    int at = find_attribute(VERTEX,"mirror");
    int connum;
    if ( at < 0 )  return 0;
    connum = *(int*)get_extra(v_id,at);
    for ( i = 0 ; i < SDIM ; i++ ) xx[i] = 0.0;
    eval_all(get_constraint(connum)->formula,xx,SDIM,&val,norm[0],v_id);
  }
  else /* regular point */
  {  
    for ( i = 0 ; i < SDIM ; i++ ) norm[0][i] = 0.0;
    startf = f_id = get_vertex_first_facet(v_id);
    do
    { get_facet_verts(f_id,sides,NULL);
      for ( i = 1 ; i <= web.dimension ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
          sides[i][j] -= sides[0][j];
      det_adjoint(sides,SDIM);
      for ( i = 0 ; i < SDIM ; i++ ) norm[0][i] += sides[i][0];
      f_id = get_next_vertex_facet(v_id,f_id);
    } while ( !equal_element(f_id,startf) );
  } /* end regular point */

  return 1;
}
