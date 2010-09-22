/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/


/*****************************************************************
*
*  File: painter.c 
*
*  Contents:  Routines to accumulate and sort facets for display
*                 back to front.  Not device specific, but calls
*                 device specific routines  for actual display.
*                 Also does transformations on edges.
*                 painter_end() uses Newell-Newell-Sancha algorithm
*                 to depth sort and display facets.
*                 This version uses a separate quadtree of depth-ordered 
*                 lists from the drawing-order list to minimize comparisons
*                 needed for large elements.
*/

/* Some timings with version 2.18j on starfish31.dmp  (2560 facets)

   Without visibility test:
   Shape     Fund.   time,sec    filesize
   Fund        1        0.093      190,829
   Cubelet    12        1.08     2,276,025
   Cube       48        4.64     9,100,293
   Rhomb      96        9.625   18,199,633

   With visibility test:
   Shape     Fund.   time,sec    filesize
   Fund        1        0.165      168,332
   Cubelet    12        1.89     1,444,778
   Cube       48        9.56     4,456,944
   Rhomb      96       24.7      7,162,980
*/

#include "include.h"

static int count;      /* number of facets */
static int maxcount;  /* number allocated */
struct tsort *trilist; /* list for depth sorting triangles */
static int gdim = 3;      /* dimension doing graphics in */

/* results of comparing depth of facets */
#define  DISJOINT      1
#define  FIRST_BACK    2
#define  SECOND_BACK   4
#define  ASPLITTINGB   8
#define  BSPLITTINGA  16
#define  NOTKNOWN     32
#define  COPLANAR     64

#define TEXTRA 100

REAL tableau[7][3];  /* simplex tableau */
void pivot ARGS((int ,int ));
int newell_split ARGS((struct tsort *,struct tsort *,struct tsort *,struct tsort *));
int backstamp;   /* for timestamping being at back of list */

int plane_test ARGS((struct tsort *,struct tsort *));
int setquadcode ARGS((struct tsort *));
void find_bbox ARGS((struct tsort *));
int separating_line ARGS((struct tsort*,struct tsort*));
int separating_plane ARGS((struct tsort*,struct tsort*,int));

static int ttcompare(t1,t2)  /* depth comparison for sort */
struct tsort **t1,**t2;
{
  /* First, compare back z */
  if ( t1[0]->mins[2] > t2[0]->mins[2] ) return 1;
  if ( t1[0]->mins[2] < t2[0]->mins[2] ) return -1;
  /* Break ties with front z */
  if ( t1[0]->maxs[2] > t2[0]->maxs[2] ) return 1;
  if ( t1[0]->maxs[2] < t2[0]->maxs[2] ) return -1;
  /* Finally, break ties with id to get consistent ordering,
     independent of how many elements are being sorted */
  if ( t1[0]->f_id > t2[0]->f_id ) return 1;
  if ( t1[0]->f_id < t2[0]->f_id ) return -1;
  return 0;

}

/* quadtree of depth lists */
struct qtree_t { struct tsort *depthhead;
                 float maxdepth; /* of all in subtree */
} *qtree;
int maxquaddepth = 8; /* maximum depth of quadtree */
int get_quadindex ARGS((unsigned int));
void qdepth_insert ARGS((struct tsort *));
struct tsort *search_subtree ARGS((int,struct tsort *,int *));

/* visibility stuff */
void visibility_stage ARGS((struct tsort *));
void visibility_end ARGS((void));
int vis_count;  /* used structures */
int vis_max;   /* allocated structures */
struct tsort *vis_list;  /* storage */


/*****************************************************************
*
*  Function: find_bbox()
*
*  Purpose: find 3D bounding box for edge or facet.
*
*/

void find_bbox(t)
struct tsort *t;
{ int n;
  REAL dx,dy,len;

  if ( (t->flag & 0xF) == EDGE )
  {
    if ( t->flag & EDGE_ARC )
    { REAL w1[MAXCOORD],w2[MAXCOORD],mag1,mag2,w1w2,center[2],radius;
      REAL det,angle1,angle2;
      int i;
      for (i = 0 ; i < SDIM ; i++ )
      { w1[i] = t->x[1][i] - t->x[0][i];
        w2[i] = t->x[2][i] - t->x[0][i];
      }
      det = w1[0]*w2[1] - w1[1]*w2[0];
      mag1 = SDIM_dot(w1,w1); mag2 = SDIM_dot(w2,w2);
      w1w2 = w1[0]*w2[0] + w1[1]*w2[1];
      for ( n = 0 ; n < gdim ; n++ )
      { /* endpoints first */
        if ( t->x[0][n] < t->x[2][n] )
         { t->maxs[n] = t->x[2][n];
           t->mins[n] = t->x[0][n];
         }
         else
         { t->maxs[n] = t->x[0][n];
           t->mins[n] = t->x[2][n];
         }
       }
      if ( 4000*det*det > mag1*mag1*mag2 + mag1*mag2*mag2 - 2*mag1*w1w2*mag2 )
      { /* circle rather that straight line */
        center[0] = t->x[0][0] + 0.5*(w2[1]*mag1 - w1[1]*mag2)/det;
        center[1] = t->x[0][1] + 0.5*(-w2[0]*mag1 + w1[0]*mag2)/det;
        radius =  sqrt((mag1*mag1*mag2+mag1*mag2*mag2-2*mag1*w1w2*mag2)
                         /4/det/det);
        angle1 = atan2(t->x[0][1]-center[1],t->x[0][0]-center[0]);
        angle2 = atan2(t->x[2][1]-center[1],t->x[2][0]-center[0]);
        if ( det < 0 )
        { REAL temp = angle1; angle1 = angle2; angle2 = temp; }
        if ( angle2 < angle1 ) angle2 += 2*M_PI;
        if ( (angle1 < 0.0 && angle2 > 0.0 ) || 
             (angle1 < 2*M_PI && angle2 > 2*M_PI) )
          t->maxs[0] = (float)(center[0] + radius);
        if ( (angle1 < M_PI && angle2 > M_PI ) || 
             (angle1 < 3*M_PI && angle2 > 3*M_PI) )
          t->mins[0] = (float)(center[0] - radius);
        if ( (angle1 < M_PI/2 && angle2 > M_PI/2 ) || 
             (angle1 < 5*M_PI/2 && angle2 > 5*M_PI/2) )
          t->maxs[1] = (float)(center[1] + radius);
        if ( (angle1 < -M_PI/2 && angle2 > -M_PI/2 ) || 
             (angle1 < 3*M_PI/2 && angle2 > 3*M_PI/2) )
          t->mins[1] = (float)(center[1] - radius);
        return;
      } 
    } /* end EDGE_ARC */
    else /* just a straight segment */
    for ( n = 0 ; n < gdim ; n++ )
    { if ( t->x[0][n] < t->x[1][n] )
      { t->maxs[n] = t->x[1][n];
        t->mins[n] = t->x[0][n];
      }
      else
      { t->maxs[n] = t->x[0][n];
        t->mins[n] = t->x[1][n];
      }
    }
    /* adjust extents for thickness */
    dx = fabs(t->x[1][0] - t->x[0][0]);
    dy = fabs(t->x[1][1] - t->x[0][1]);
    len = sqrt(dx*dx+dy*dy);
    if ( len > 0.0 )
    { t->maxs[0] += (float)(dy/len*t->width/2);
      t->mins[0] -= (float)(dy/len*t->width/2);
      t->maxs[1] += (float)(dx/len*t->width/2);
      t->mins[1] -= (float)(dx/len*t->width/2);
    }
    return;
  } /* end EDGE */

  /* facet */
  for ( n = 0 ; n < gdim ; n++ )
  { if ( t->x[0][n] < t->x[1][n] )
    { if  ( t->x[2][n] < t->x[0][n] )
      { t->maxs[n] = t->x[1][n];
        t->mins[n] = t->x[2][n];
      }
      else if ( t->x[1][n] < t->x[2][n] )
      { t->maxs[n] = t->x[2][n];
        t->mins[n] = t->x[0][n];
      }
      else
      { t->maxs[n] = t->x[1][n];
        t->mins[n] = t->x[0][n];
      }
    }
    else
    { if  ( t->x[2][n] < t->x[1][n] )
      { t->maxs[n] = t->x[0][n];
        t->mins[n] = t->x[2][n];
      }
      else if ( t->x[0][n] < t->x[2][n] )
      { t->maxs[n] = t->x[2][n];
        t->mins[n] = t->x[1][n];
      }
      else
      { t->maxs[n] = t->x[0][n];
        t->mins[n] = t->x[1][n];
      }
    }
  }
}

/* For setting quadtree code and checking if in bounding box. */
/* Coded for 32 bit ints. */
#define OUTOFBOX 0
#define INTHEBOX 1

int setquadcode(t)
struct tsort *t;
{ unsigned int q = 0;  /* the quadcode */
  unsigned int bit = 1;  /* for shifting to quad bit position */
  int n;
  REAL midx = (minclipx+maxclipx)/2;
  REAL midy = (minclipy+maxclipy)/2;
  REAL deltax = (maxclipx-minclipx)/4;
  REAL deltay = (maxclipy-minclipy)/4;

  if ( t->maxs[0] < minclipx || t->mins[0] > maxclipx || t->maxs[1] < minclipy
       || t->mins[1] > maxclipy ) return OUTOFBOX;

  for ( n = 0  ; n < 8 ; n++, deltax /= 2, deltay /= 2 )
  {
    if ( t->maxs[0] <= midx ) 
    { q |= bit; midx -= deltax; }
    else if ( t->mins[0] >= midx )
    { q |= bit<<1; midx += deltax; }
    else break;
    bit <<= 2;
    if ( t->maxs[1] <= midy ) 
    { q |= bit; midy -= deltay; }
    else if ( t->mins[1] >= midy )
    { q |= bit<<1; midy += deltay; }
    else break;
    bit <<= 2;
  }

  t->quadcode = q;
  return INTHEBOX;
}

void painter_start()
{ int dummy;
  long allocsize;

  gdim = (SDIM <= 3) ? SDIM : 3;

  /* allocate space for depth sort list */
  if ( web.representation == STRING )
  {  maxcount = web.skel[EDGE].count + 5;
     if ( web.torus_flag ) maxcount *= 2;
  }
  else
  { if ( web.torus_flag )
      if ( torus_display_mode == TORUS_CLIPPED_MODE )
        maxcount = 5*web.skel[FACET].count+ bare_edge_count + 5;
      else maxcount = 2*web.skel[FACET].count+ bare_edge_count + 5;
    else
      maxcount = web.skel[FACET].count + bare_edge_count + 5;
  }
  if ( transforms_flag ) maxcount *= transform_count;
  if ( web.dimension > 2 )
     maxcount *= web.dimension+1; /* each simplex face becomes facet */
  allocsize = (long)maxcount*sizeof(struct tsort);
  if ( allocsize >= MAXALLOC  )
     maxcount = MAXALLOC/sizeof(struct tsort);
  trilist = (struct tsort *)temp_calloc(maxcount,sizeof(struct tsort));
  count = 0;
  vis_count = 0;
  vis_list = NULL;
  vis_max = 0;
  backstamp = 0;
  ps_widthattr = find_extra(PS_WIDTHNAME,&dummy);
}


void painter_edge(gdata,e_id)
struct graphdata *gdata;
edge_id e_id; 
{
  struct tsort *t;
  int i,j;
  REAL a[MAXCOORD+1],b[MAXCOORD+1];
  REAL dx,dy,dz,mag,width;
  int ctrl_pts = gdata[0].flags & EDGE_ARC ? 3 : 2;

  if ( gdata->color == CLEAR ) return;
  if ( count >= maxcount-2 )
  { trilist = (struct tsort *)temp_realloc((char*)trilist,
         (maxcount+200)*sizeof(struct tsort));
    maxcount += 200;
  }

  t = trilist + count;
  t->flag = EDGE;
  t->flag |= (gdata->flags & (EDGE_ARC|LABEL_EDGE|LABEL_HEAD|LABEL_TAIL));
  for ( j =  SDIM ; j < HOMDIM-1 ; j++ ) a[j] = 0.0; /* filler */
  for ( i = 0 ; i < ctrl_pts ; i++ )
  {
    for ( j = 0 ; (j < SDIM) && (j < HOMDIM-1) ; j++ ) 
        a[j] = gdata[i].x[j];
    a[HOMDIM-1] = 1.0;
    matvec_mul(view,a,b,HOMDIM,HOMDIM);  /* transform */
    if ( SDIM <= 2 )
      for ( j = 0 ; j < 3 ; j++ ) t->x[i][j] = (float)b[j];
    else  for ( j = 0 ; j < 3 ; j++ ) t->x[i][j] = (float)b[(j+1)%3];
    t->x[i][2] += (float).0001;   /* bias edges in front of facets */
  }

  /* width of edge, in descending order of thickness */
  if ( ps_widthattr >= 0 )
    width = *EREAL(t->f_id,ps_widthattr);
  else if ( gdata->etype & BARE_EDGE ) width = ps_bareedgewidth;
  else if ( gdata->etype & FIXED_EDGE ) width = ps_fixededgewidth;
  else if ( gdata->etype & CONSTRAINT_EDGE ) width = ps_conedgewidth;
  else if ( gdata->etype & BOUNDARY_EDGE ) width = ps_conedgewidth;
  else if ( gdata->etype & SINGLE_EDGE ) width = ps_stringwidth;
  else if ( gdata->etype & TRIPLE_EDGE ) width = ps_tripleedgewidth;
  else width = ps_gridedgewidth; /* regular grid interior edge */
  t->width = (float)width;

  t->f_id = e_id;
  t->color = t->ecolor[0] =  gdata->ecolor;
  t->etype[0] = gdata->etype;
  /* find extents */
  find_bbox(t);

  /* normal vector, closest to z axis */
  dx = t->x[1][0] - t->x[0][0];
  dy = t->x[1][1] - t->x[0][1];
  dz = t->x[1][2] - t->x[0][2];
  t->normal[0] = (float)(dx*dz);
  t->normal[1] = (float)(dy*dz);
  t->normal[2] = -(float)(dx*dx+dy*dy);
  mag = sqrt(dotf(t->normal,t->normal,3));
  if ( mag != 0.0 )
      for ( i = 0 ; i < 3; i++ ) t->normal[i] /= (float)mag;

  if ( setquadcode(t) == OUTOFBOX ) return;
  count++;
}


void painter_facet(gdata,f_id)
struct graphdata *gdata;
facet_id f_id;
{
  int i,j;
  REAL a[MAXCOORD+1],b[FACET_VERTS][MAXCOORD+1];
  struct tsort *t;
  REAL normal[MAXCOORD],mag;

  if ( gdata[0].color == UNSHOWN )
  { /* just do edges */
    struct graphdata ggdata[2];
    facetedge_id fe=NULLID;
    if ( valid_id(f_id) )
      fe = get_facet_fe(f_id);
    for ( i = 0 ; i < FACET_EDGES ; i++ )
    { if ( (gdata[i].etype&EBITS) == INVISIBLE_EDGE ) continue;
      ggdata[0] = gdata[i]; 
      ggdata[0].color = gdata[i].ecolor;
      ggdata[1] = gdata[i==2 ? 0 : i+1];
      if ( valid_id(fe) )
      { painter_edge(ggdata,get_fe_edge(fe));
        fe = get_next_edge(fe);
      }
      else painter_edge(ggdata,NULLID);
    }
    return;
  }

  if ( count >= maxcount )
  { trilist = (struct tsort *)temp_realloc((char*)trilist,
         (maxcount+200)*sizeof(struct tsort));
    maxcount += 200;
  }
  t = trilist + count;
  t->flag = FACET | (gdata->flags&LABEL_FACET);
  t->f_id = f_id;
  t->color = gdata[0].backcolor;  /* not sure why, but works */
  for ( i = 0 ; i < FACET_EDGES ; i++ ) 
  { t->ecolor[i] = gdata[i].ecolor;
    t->etype[i] = gdata[i].etype;
    t->v_id[i] = gdata[i].v_id;
  }

  /* accumulate list of triangles to display */

  for ( j = SDIM ; j < HOMDIM-1 ; j++ ) a[j] = 0.0;
  for ( i = 0 ; i < FACET_VERTS ; i++ )
  {
    for ( j = 0 ; (j < SDIM) && (j < HOMDIM-1) ; j++ ) 
      a[j] = gdata[i].x[j];
    a[HOMDIM-1] = 1.0;
    matvec_mul(view,a,b[i],HOMDIM,HOMDIM);  /* transform */
    if ( SDIM <= 2 )
    { t->x[i][0] = (float)b[i][0];
      t->x[i][1] = (float)b[i][1];
    }
    else
    { t->x[i][0] = (float)b[i][1];
      t->x[i][1] = (float)b[i][2];
      t->x[i][2] = (float)b[i][0];
    }
  } 
  if ( SDIM <= 2 )
  { t->normal[0] = t->normal[1] = 0.0;
    t->normal[2] = 1.0;
  }
  else
  { vnormal(b[0],b[1],b[2],normal); 
    mag = sqrt(SDIM_dot(normal,normal));
    if ( mag > 0.0 )
    { t->normal[0] = (float)(normal[1]/mag); 
      t->normal[1] = (float)(normal[2]/mag); 
      t->normal[2] = (float)(normal[0]/mag);
      if ( fabs(t->normal[2]) < 1e-6 ) t->normal[2] = 0.0; 
    } else
    { t->normal[0] = 0.0f; 
      t->normal[1] = 0.0f; 
      t->normal[2] = 1.0f;
    }
  }
  if ( t->normal[2] > (float)0.0 ) /* frontward normal */
  { int c;
    vertex_id tv;
    for ( i = 0 ; i < gdim ; i++ )
    { float temp = (float)t->x[1][i];
      t->x[1][i] = t->x[2][i];
      t->x[2][i] = temp;
      t->normal[i] = -t->normal[i];
    }
    c = t->ecolor[0];
    t->ecolor[0] = t->ecolor[2];
    t->ecolor[2] = c;
    c = t->etype[0] ^ LABEL_REVERSED;
    t->etype[0] = t->etype[2] ^ LABEL_REVERSED;
    t->etype[2] = (short)c;
    t->etype[1] ^= LABEL_REVERSED;
    t->color = gdata[0].color;
    tv = t->v_id[1];
    t->v_id[1] = t->v_id[2];
    t->v_id[2] = tv;
    t->flag |= FLIPPED_FACET;
  }
  else
  { 
    if ( backcull_flag && (gdata[0].color == gdata[0].backcolor) ) return;
  }

  /* find extents */
  find_bbox(t);
  if ( setquadcode(t) == OUTOFBOX ) return;

  count++;
}
 /* stats for analyzing performance; REAL to handle large counts */
 REAL in_back_calls;
 REAL box_overlaps;
 REAL facetfacet;
 REAL facetedge;
 REAL edgeedge;
 REAL crossings;
 REAL swaps;
 REAL done;
 REAL loopbailouts;
 REAL sep_line_calls;
 REAL sep_plane_calls;

 struct tsort **tlist;
 /*struct tsort *depthhead;*/

/*************************************************************************
*
* function: search_subtree()
*
* purpose: search node and subtree of quadtree depth lists for an
*          element obscured by given element.
* return: pointer to obscured element, or NULL if none found.
*         also retval to indicate type of relationship
*/
struct tsort *search_subtree(qinx,tk,retval)
int qinx; /* index of node in quadtree */
struct tsort *tk;  /* given element */
int *retval;
{ struct tsort *tj;
 
  *retval = 0;

  /* check overall subtree max depth */
  if ( tk->maxs[2] <= qtree[qinx].maxdepth )
    return NULL;

  /* the node itself */
  for ( tj = qtree[qinx].depthhead ; tj != NULL ; tj = tj->next )
  { 
    if ( tj == tk ) continue;
    if ( tk->maxs[2] <= tj->mins[2] ) 
      break; 
    *retval = in_back(tk,tj);  
    if ( *retval & (FIRST_BACK|COPLANAR|DISJOINT) ) continue;
    return tj;
  }

  /* one child */
  tj = search_subtree(2*qinx,tk,retval);
  if ( tj ) return tj;
  
  /* other child */
  return search_subtree(2*qinx+1,tk,retval);
}

#ifdef XXXXX
/* for debugging */
void loopchecker()
{ int counter;
  int qinx;

  int i;
  for ( i = 0 ; i < count+TEXTRA ; i++ )
    if ( tlist[i]->spot != i ) 
       kb_error(2893,"Bad spot\n",RECOVERABLE);

  for ( qinx = 0 ; qinx < 0x40000 ; qinx++ )
  { struct tsort *tj;
    int q;
    struct tsort *slowboat = qtree[qinx].depthhead; /* likewise */
    struct tsort *prev = NULL;
    if ( slowboat == NULL ) continue;
      counter = 0; q = slowboat->quadcode;
      if ( qinx != get_quadindex(q) )
        kb_error(2585,"Bad qinx.\n",RECOVERABLE);
      for ( tj = qtree[qinx].depthhead ; tj != NULL ; prev=tj,tj = tj->next )
        { /* loop detection */
          if ( tj->prev != prev ) 
            kb_error(2892,"bad prev\n",RECOVERABLE);
          if ( tj->quadcode != q )
             kb_error(2891,"Bad qinx.\n",RECOVERABLE);
          if ( counter & 1) slowboat = slowboat->next;
          if ( (slowboat == tj) && (counter > 1) ) 
             kb_error(2591,"Internal error: loop in loopchecker()\n",
               RECOVERABLE);
          counter++;
        }
  }
}
#endif

/**************************************************************************
*
* function: painter_end()
*
* purpose: sort and display facets and edges from trilist.
*/

/* for debugging; just displays list as is after given number of facets */
int debug_k = 0x7FFFFFFF;

void painter_end()
{
  int k;
  int loopcount; /* for emergency loop bailout */
  struct tsort **ll,*tt;
  int quadalloc;
  int k_top;  /* top of tlist */

  struct tsort textra[TEXTRA]; /* for triangle fragments */

  in_back_calls = box_overlaps = facetfacet = facetedge = edgeedge = 
   crossings = sep_plane_calls = sep_line_calls = 0;
  loopbailouts = 0;

  if ( count > maxcount ) count = maxcount;    /* in case there was excess */

  /* find bounding box */
  if ( need_bounding_box )
  { struct tsort *t;
    bbox_minx = bbox_miny = 1e20;
    bbox_maxx = bbox_maxy = -1e20;
    for ( k = 0, t = trilist ; k < count ; k++,t++ )
    { if ( t->mins[0] < bbox_minx ) bbox_minx = (REAL)t->mins[0];
      if ( t->mins[1] < bbox_miny ) bbox_miny = (REAL)t->mins[1];
      if ( t->maxs[0] > bbox_maxx ) bbox_maxx = (REAL)t->maxs[0];
      if ( t->maxs[1] > bbox_maxy ) bbox_maxy = (REAL)t->maxs[1];
    }
  }

  (*init_graphics)();

  if ( SDIM == 2 )  /* don't bother with depth */
  { for ( k = 0 ; k < count ; k++ )
      visibility_stage(trilist+k);
    goto end_exit;
  } 

  /* now sort on min z, moving pointers instead of structures */
  /* leaving room at front of list for extra fragments */
  tlist = (struct tsort **)temp_calloc(count+TEXTRA,sizeof(struct tsort *));
  for ( k = 0, ll=tlist+TEXTRA, tt=trilist ; k < count ; k++ ) *(ll++) = tt++;
  qsort((char *)(tlist+TEXTRA),count,sizeof(struct tsort *),FCAST ttcompare); 
  for ( k = 0 ; k < TEXTRA ; k++ ) 
  { tlist[k] = textra+k; tlist[k]->spot = k; tlist[k]->flag = 0; }
  for ( k = TEXTRA ; k < TEXTRA+count ; k++ )
    tlist[k]->spot = k;

  /* quadtree of depth lists */
  maxquaddepth = 8; /* maybe make this adjustable later */
  quadalloc = 2 << (2*maxquaddepth + 1);
  qtree = (struct qtree_t *)temp_calloc(quadalloc,sizeof(struct qtree_t));
  for ( k = 0 ; k < quadalloc ; k++ )
    qtree[k].maxdepth = 1e30f;
  for ( k = count+TEXTRA-1 ; k >= TEXTRA ; k-- )
    qdepth_insert(tlist[k]);

  /* display */
  loopcount = 0;
  k_top = count+TEXTRA;
  for ( k = TEXTRA ; k < k_top ;  )
  { struct tsort *tk = tlist[k];
    struct tsort *tj;
    int sinx,qinx=0;
    int retval;

    if ( breakflag ) break;
    if ( !tk->flag ) { k++; continue; }

    /* for debugging and testing */
    if ( k > debug_k )
       goto draw_it;

    /* tk is current candidate back facet */

    /* search quadtree list for any z overlap */
    /* First, node to root  */
    qinx = get_quadindex(tk->quadcode);
    for ( sinx = qinx >> 1 ; sinx != 0 ; sinx >>= 1 )
    { for ( tj = qtree[sinx].depthhead ; tj != NULL ; tj = tj->next )
      { 
        if ( tj == tk ) continue;
        if ( tk->maxs[2] <= tj->mins[2] ) 
          break; 
        retval = in_back(tk,tj);  
        if ( retval & (FIRST_BACK|COPLANAR|DISJOINT) ) continue;
        goto have_conflict;
      }
    }

    /* now search subtree for conflicts */ 
    tj = search_subtree(qinx,tk,&retval);
    if ( tj==NULL ) goto draw_it;

have_conflict:
    /* Now have conflict, tk obscuring tj */

      /* test for possible looping, and if found, split tk */
      if ( (tj->backstamp == backstamp) )
      { int ret;

        crossings++;
        if ( ++loopcount > count ) 
        { loopbailouts++; goto draw_it; }

        /* need to split */ 
               
        if ( k < 2 ) 
        { /* not enough room, so expand tlist allocation, with free at start */
          int newsize = 2*k_top;
          int n;
          struct tsort *more = (struct tsort*)temp_calloc(k_top,sizeof(struct tsort));
          tlist = (struct tsort**)temp_realloc((char*)tlist,newsize*sizeof(struct tsort*));
          for ( n = 0 ; n < k_top ; n++ )
          { tlist[n+k_top] = tlist[n];
            tlist[n] = more+n;
          }
          for ( n = 0 ; n < newsize ; n++ )
            tlist[n]->spot = n;
          k += k_top;
          k_top = newsize;
        }
        if( retval & BSPLITTINGA )
        {
          ret = newell_split(tk,tj,tlist[k-1],tlist[k-2]);
          if ( ret )
          {
            k -= ret;
            goto repeat_tests;  /* might not have split */
          }
        }
        else if ( retval & ASPLITTINGB )
        {
          /* try splitting the other way */
          ret = newell_split(tj,tk,tlist[k-1],tlist[k-2]);
          if ( ret )
          { k -= ret;
            goto repeat_tests;
          }
        }
        else if ( ((tk->flag & 0xF) == EDGE) && ((tj->flag & 0xF) == EDGE) )
        {
          ret = newell_split(tk,tj,tlist[k-1],tlist[k-2]);
          if ( ret )
          {
            k -= ret;
            goto repeat_tests;  /* might not have split */
          }
        }
      }

      tk->backstamp = backstamp;
      /* swap tj and tk */
      tlist[k] = tj; 
      tlist[tj->spot] = tk; 
      tk->spot = tj->spot;
      tj->spot = k;  
      swaps++;
      goto repeat_tests;

draw_it:
    visibility_stage(tk);
    loopcount = 0;
    tk->flag = 0;   /* to indicate empty structure */

    /* remove from depth list */
    if ( tk == qtree[qinx].depthhead ) 
      qtree[qinx].depthhead = tk->next;
    sinx = qinx;
    /* fix up subtree maxdepths */
    while ( sinx && ( tk->mins[2] <= qtree[sinx].maxdepth ) )
    { float maxd = 1e30f;
      if ( qtree[sinx].depthhead )
         maxd = qtree[sinx].depthhead->mins[2];
      if ( sinx < (1 << (2*maxquaddepth)) )
      { if ( maxd > qtree[2*sinx].maxdepth )
          maxd = qtree[2*sinx].maxdepth;
        if ( maxd > qtree[2*sinx+1].maxdepth )
          maxd = qtree[2*sinx+1].maxdepth;
      }
      qtree[sinx].maxdepth = maxd;
      sinx >>= 1;
    }
    if ( tk->prev ) tk->prev->next = tk->next;
    if ( tk->next ) tk->next->prev = tk->prev;

repeat_tests: 
  
    continue;
  }
 
end_exit:

  if ( verbose_flag )
  {
    printf("in_back_calls:   %g\n",in_back_calls);
    printf("  facetfacet:    %g\n",facetfacet);
    printf("  facetedge:     %g\n",facetedge);
    printf("  edgeedge:      %g\n",edgeedge);
    printf("box_overlaps:    %g\n",box_overlaps);
    printf("sep_line_calls:  %g\n",sep_line_calls);
    printf("sep_plane_calls: %g\n",sep_plane_calls);
    printf("crossings:       %g\n",crossings);
    printf("swaps:           %g\n",swaps);
    printf("loop bailouts:   %g\n",loopbailouts);
  }

  if ( tlist ) temp_free((char *)tlist);
  if ( qtree ) temp_free((char *)qtree);
  temp_free((char *)trilist); trilist = NULL;

  if ( visibility_test )
    visibility_end();

  (*finish_graphics)();
} /* end old painter_end() */

/*********************************************************************
*
* function: in_back()
*
* purpose: see if one facet or edge obscures another.
*
* returns DISJOINT, FIRST_BACK, SECOND_BACK, ASPLITTINGB, BSPLITTINGA, 
*   or COPLANAR (possibly bitwise OR)
*/
int in_back(ta,tb)
struct tsort *ta,*tb;
{
  int n;
  
  int retval;
   
  if ( verbose_flag )
  {
    in_back_calls++;
    if ( (ta->flag & 0xF) == FACET  )
    { if ( (tb->flag & 0xF) == FACET  )          
        facetfacet++;
      else facetedge++;
    }
    else  
    { if ( (tb->flag & 0xF) == FACET  )          
        facetedge++;
      else edgeedge++;
    }
  }

  /* quick test with quadcodes */
  if ( ((ta->quadcode & tb->quadcode) != ta->quadcode) &&
       ((ta->quadcode & tb->quadcode) != tb->quadcode) )
    return DISJOINT;

  /* test x and y extent overlap */
  for ( n = 0 ; n < 2 ; n++ )
     if ( (tb->maxs[n] <= ta->mins[n]) || (tb->mins[n] >= ta->maxs[n]) )
        return DISJOINT;  
  if ( ta->maxs[2] <= tb->mins[2] ) return FIRST_BACK;

  box_overlaps++;   /* for verbose stats */

  if ( separating_line(ta,tb) == DISJOINT ) return DISJOINT;  

  retval = plane_test(ta,tb);
  if ( retval & (FIRST_BACK|COPLANAR|DISJOINT) ) return retval;

  /* now the nitty gritty check to see if they overlap */
#ifdef ZZZ
  if ( (ta->flag & 0xF) == FACET  )
  { if ( (tb->flag & 0xF) == FACET  ) 
      return facetfacetcompare(ta,tb);
     else 
     {    retval = edgefacetcompare(tb,ta);
        if ( retval & (FIRST_BACK|SECOND_BACK) )
           return retval ^ (FIRST_BACK|SECOND_BACK);
        else return retval;
    }
  }
  else
  { if ( (tb->flag & 0xF) == FACET  ) 
      return edgefacetcompare(ta,tb);
    else
    { retval = edgeedgecompare(tb,ta);
      if ( retval & (FIRST_BACK|SECOND_BACK) )
           return retval ^ (FIRST_BACK|SECOND_BACK);
      else return retval;
    }

  }
#endif


  return retval;
}  

/**************************************************************************
*
* function: get_quadindex()
*
* purpose: convert quadcode to index number in quadtree list.
*
*/
int get_quadindex(q)
unsigned int q;
{ int inx,k;

  inx = 1;
  for ( k = 0 ; k < 2*maxquaddepth ; k++, q >>= 2 )
  { int bits = q & 0x3;
    if ( bits == 0 ) return inx;
    inx <<= 1;
    if ( bits == 2 )
      inx++;
  }
  return inx;
}

/**************************************************************************
*
* function: qdepth_insert()
*
* purpose: insert new fragment in proper place in quadtree depth list.
*          For now, crude linear search.
*/
void qdepth_insert(tc)
struct tsort *tc;
{ struct tsort *prev,*next;
  int qinx = get_quadindex(tc->quadcode);
  int sinx;

  /* take care of maxdepths of subtrees */
  sinx = qinx;
  while ( sinx && (tc->mins[2] < qtree[sinx].maxdepth) )
  { qtree[sinx].maxdepth = tc->mins[2];
    sinx >>= 1;
  }

  prev = NULL;
  for ( next=qtree[qinx].depthhead; next != NULL; prev=next, next=next->next )
  {
    if ( tc->mins[2] < next->mins[2] )
    { tc->next = next;
      tc->prev = prev;
      if ( prev )
        prev->next = tc;
      else qtree[qinx].depthhead = tc;
      next->prev = tc;
      goto qdepth_insert_exit;
    }
  }
  /* at end of list */
  if ( prev ) prev->next = tc;
  else qtree[qinx].depthhead = tc;
  tc->next = NULL;
  tc->prev = prev;

qdepth_insert_exit:;
}

/*************************************************************************
*
* function: newell_split()
*
* purpose: split one triangle by plane of another.
*
* return: number of new elements generated.
*/

int newell_split(ta,tb,tc,td)
struct tsort *ta;  /* splittee and fragment return */
struct tsort *tb;  /* splitter */
struct tsort *tc;  /* fragment return */
struct tsort *td;  /* fragment return */
{ 
  int i;
  REAL d0,d1,d2,db;
  int retval;
  int tmpspot;

  backstamp++; /* clear loop indications */

  if ( (tb->flag & 0xF) == EDGE )
  { struct tsort *t;
    if ( (ta->flag & 0xF) == EDGE )
    { /* cut first edge in half */
      tmpspot = tc->spot; *tc = *ta; tc->spot = tmpspot;
      for ( i = 0 ; i < gdim ; i++ )
      { float mid = (ta->x[0][i] + ta->x[1][i])/2; 
        if ( ta->x[0][2] > ta->x[1][2] )  /* ta is back half */
        { ta->x[0][i] = mid; tc->x[1][i] = mid; }
        else { ta->x[1][i] = mid; tc->x[0][i] = mid; }
      }
      for ( i = 0 ; i < gdim ; i++ )
      { ta->mins[i] = (ta->x[0][i]<ta->x[1][i]) ? ta->x[0][i] : ta->x[1][i];
        tc->mins[i] = (tc->x[0][i]<tc->x[1][i]) ? tc->x[0][i] : tc->x[1][i];
        ta->maxs[i] = (ta->x[0][i]>ta->x[1][i]) ? ta->x[0][i] : ta->x[1][i];
        tc->maxs[i] = (tc->x[0][i]>tc->x[1][i]) ? tc->x[0][i] : tc->x[1][i];
      }
      /* find_bbox(tc); */
      if ( setquadcode(tc) == INTHEBOX )
      { qdepth_insert(tc);
        return 1;
      }
      else { tc->flag = 0; return 0; }
    }
    t = ta; ta = tb; tb = t; /* swap so edge first */
  }

  if ( (ta->flag & 0xF) == EDGE ) /* tb assumed to be facet */
  {
    db = dotf(tb->normal,tb->x[0],gdim);
    d0 = dotf(tb->normal,ta->x[0],gdim); 
    d1 = dotf(tb->normal,ta->x[1],gdim); 

    /* fill in fragment info */
    tmpspot = tc->spot; *tc = *ta; tc->spot = tmpspot;
    for ( i = 0 ; i < gdim ; i++ )
    { double mid = (((db-d0)*ta->x[1][i] + (d1-db)*ta->x[0][i])/(d1-d0));
      if ( ta->x[0][2] < ta->x[1][2] ) /* keep ta as back part */
        ta->x[1][i] = tc->x[0][i] = (float)mid;
      else  ta->x[0][i] = tc->x[1][i] = (float)mid;
    }
    for ( i = 0 ; i < gdim ; i++ )
    { ta->mins[i] = (ta->x[0][i]<ta->x[1][i]) ? ta->x[0][i] : ta->x[1][i];
      tc->mins[i] = (tc->x[0][i]<tc->x[1][i]) ? tc->x[0][i] : tc->x[1][i];
      ta->maxs[i] = (ta->x[0][i]>ta->x[1][i]) ? ta->x[0][i] : ta->x[1][i];
      tc->maxs[i] = (tc->x[0][i]>tc->x[1][i]) ? tc->x[0][i] : tc->x[1][i];
    }
    /* find_bbox(tc); */
    if ( setquadcode(tc) == INTHEBOX )
    { qdepth_insert(tc);
      return 1;
    }
    else { tc->flag = 0; return 0; }
  }

  /* figure out which vertices of ta on same side, and get as 0,1 */
  db = dotf(tb->normal,tb->x[0],gdim);
  d0 = dotf(tb->normal,ta->x[0],gdim); 
  d1 = dotf(tb->normal,ta->x[1],gdim); 
  d2 = dotf(tb->normal,ta->x[2],gdim); 

if ( (d0<db) && (d1<db) && (d2<db) ) /* should never happen */
{ struct tsort t;  
  t = *ta; *ta = *tb; *tb = t; return 0; }
if ( (d0>db) && (d1>db) && (d2>db) )
{ return 0; }

  retval = 0;
  if ( db == d0 ) /* split thru vertex 0 of ta */
  { 
     tmpspot = tc->spot; *tc = *ta; tc->spot = tmpspot;

     /* fill in fragment info */
     for ( i = 0 ; i < gdim ; i++ )
     { ta->x[2][i] = tc->x[1][i] = 
          (float)(((db-d1)*ta->x[2][i] + (d2-db)*ta->x[1][i])/(d2-d1));
     }
     /* internal edges invisible */
     ta->etype[2] = tc->etype[0] = SPLITTING_EDGE;
     retval = 1;
  }
  if ( db == d1 ) /* split thru vertex 1 of ta */
  { 
     tmpspot = tc->spot; *tc = *ta; tc->spot = tmpspot;

     /* fill in fragment info */
     for ( i = 0 ; i < gdim ; i++ )
     { ta->x[2][i] = tc->x[0][i] = 
          (float)(((db-d0)*ta->x[2][i] + (d2-db)*ta->x[0][i])/(d2-d0));
     }
     /* internal edges invisible */
     ta->etype[1] = tc->etype[0] = SPLITTING_EDGE;
     retval = 1;
  }
  if ( db == d2 ) /* split thru vertex 2 of ta */
  { 
    tmpspot = tc->spot; *tc = *ta; tc->spot = tmpspot;

    /* fill in fragment info */
    for ( i = 0 ; i < gdim ; i++ )
    { ta->x[1][i] = tc->x[0][i] = 
          (float)(((db-d0)*ta->x[1][i] + (d1-db)*ta->x[0][i])/(d1-d0));
    }
    /* internal edges invisible */
    ta->etype[1] = tc->etype[2] = SPLITTING_EDGE;
    retval = 1;
  }

  if ( retval == 1 )
  {
     /* set mins and maxs */
     for ( i = 0 ; i < gdim ; i++ )
     { int j;
        ta->mins[i] = tc->mins[i] = (float)1e30;
        ta->maxs[i] = tc->maxs[i] = (float)(-1e30);
        for ( j = 0 ; j < FACET_VERTS ; j++ )
        { if ( ta->x[j][i] < ta->mins[i] ) ta->mins[i] = ta->x[j][i];
          if ( tc->x[j][i] < tc->mins[i] ) tc->mins[i] = tc->x[j][i];
          if ( ta->x[j][i] > ta->maxs[i] ) ta->maxs[i] = ta->x[j][i];
          if ( tc->x[j][i] > tc->maxs[i] ) tc->maxs[i] = tc->x[j][i];
        }
     }
     if ( ta->mins[2] > tc->mins[2] )
     { struct tsort tmp;  /* get ta as back part */
       tmp = *ta;
       *ta = *tc;
       *tc = tmp;
       tmpspot = ta->spot; ta->spot = tc->spot; tc->spot = tmpspot;
     }
     /* find_bbox(tc); */
     if ( setquadcode(tc) == INTHEBOX )
     { qdepth_insert(tc);
       return 1;
     }
     else { tc->flag = 0; return 0; }
  }

  if ( (d0-db)*(d2-db) > 0.0 )
  { int c = ta->ecolor[1]; 
    short e = ta->etype[1];
    REAL d = d1;
    for ( i = 0 ; i < gdim ; i++ )
    { float temp = ta->x[1][i]; 
      ta->x[1][i] = ta->x[0][i]; ta->x[0][i] = ta->x[2][i];
      ta->x[2][i] = temp;
    }
    ta->ecolor[1] = ta->ecolor[0]; ta->ecolor[0] = ta->ecolor[2];
    ta->ecolor[2] = c;
    ta->etype[1] = ta->etype[0]; ta->etype[0] = ta->etype[2];
    ta->etype[2] = e;
    d1 = d0; d0 = d2; d2 = d;
  }
  else if ( (d1-db)*(d2-db) > 0.0 )
  { int c = ta->ecolor[1]; 
    short e = ta->etype[1];
    REAL d = d1;
    for ( i = 0 ; i < gdim ; i++ )
    { float temp = ta->x[1][i]; 
      ta->x[1][i] = ta->x[2][i]; ta->x[2][i] = ta->x[0][i];
      ta->x[0][i] = temp;
    }
    ta->ecolor[1] = ta->ecolor[2]; ta->ecolor[2] = ta->ecolor[0];
    ta->ecolor[0] = c;
    ta->etype[1] = ta->etype[2]; ta->etype[2] = ta->etype[0];
    ta->etype[0] = e;
    d1 = d2; d2 = d0; d0 = d;
  }


  /* copy all info to fragments */
  tmpspot = tc->spot; *tc = *ta; tc->spot = tmpspot;
  tmpspot = td->spot; *td = *ta; td->spot = tmpspot;
  retval = 2;

  /* fill in fragment info */
  for ( i = 0 ; i < gdim ; i++ )
  { ta->x[2][i] = tc->x[0][i] = td->x[0][i] =
        (float)(((db-d0)*td->x[2][i] + (d2-db)*ta->x[0][i])/(d2-d0));
    tc->x[2][i] = td->x[1][i] =
        (float)(((db-d1)*td->x[2][i] + (d2-db)*ta->x[1][i])/(d2-d1));
  }
  /* internal edges invisible */
  ta->etype[1] = tc->etype[0] = tc->etype[2] = td->etype[0] = SPLITTING_EDGE;

  /* set mins and maxs */
  for ( i = 0 ; i < gdim ; i++ )
  { int j;
    ta->mins[i] = tc->mins[i] = td->mins[i] = (float)1e30;
    ta->maxs[i] = tc->maxs[i] = td->maxs[i] = (float)(-1e30);
    for ( j = 0 ; j < 3 ; j++ )
    { if ( ta->x[j][i] < ta->mins[i] ) ta->mins[i] = ta->x[j][i];
      if ( tc->x[j][i] < tc->mins[i] ) tc->mins[i] = tc->x[j][i];
      if ( td->x[j][i] < td->mins[i] ) td->mins[i] = td->x[j][i];
      if ( ta->x[j][i] > ta->maxs[i] ) ta->maxs[i] = ta->x[j][i];
      if ( tc->x[j][i] > tc->maxs[i] ) tc->maxs[i] = tc->x[j][i];
      if ( td->x[j][i] > td->maxs[i] ) td->maxs[i] = td->x[j][i];
    }
  }

  /* get ta as back part */
  if ( (tc->mins[2] < ta->mins[2]) && (tc->mins[2] <= td->mins[2]) )
  { struct tsort tmp;  /* get ta as back part */
    tmp = *ta;
    *ta = *tc;
    *tc = tmp;
    tmpspot = ta->spot; ta->spot = tc->spot; tc->spot = tmpspot;
  }
  else if ( td->mins[2] < ta->mins[2] )
  { struct tsort tmp;  /* get ta as back part */
    tmp = *ta;
    *ta = *td;
    *td = tmp;
    tmpspot = ta->spot; ta->spot = td->spot; td->spot = tmpspot;
  }

  /* find_bbox(tc); */
  /* find_bbox(td); */
  if ( setquadcode(tc) == INTHEBOX )
  { qdepth_insert(tc);
    if ( setquadcode(td) == INTHEBOX )
      qdepth_insert(td);
    else { retval--; td->flag = 0; td->next = (struct tsort *)0xFF; }
  }
  else
  { /* discard tc */
    tmpspot = tc->spot;
    *tc = *td; 
    tc->spot = tmpspot;
    td->flag = 0; td->next = (struct tsort *)0xAA;
    retval--;
    if ( setquadcode(tc) == INTHEBOX )
      qdepth_insert(tc);
    else { retval--; tc->flag = 0; tc->next = (struct tsort *)0xEE; }
  }
  return retval;
}

/*********************************************************************
*
* function: separating_plane()
*
* purpose: See if two facets have a separating plane in 3D.
*             Meant to be called when known the facets overlap in 2D.
*
* returns  FIRST_BACK, SECOND_BACK, or ASPLITTINGB | BSPLITTINGA
*/
int separating_plane(ta,tb,depth)
struct tsort *ta,*tb;
int depth; /* to limit recursion to depth 2 */
{ int i,j;
  int na,nb; /* vertices on respective elements */
  int nna; /* number of vertex pairs to check on ta */
  float *a[3],*b[3];
  int retval;
  REAL da,db,d;
  int n,k,bnear=0,beq=0,bfar=0,anear=0,afar=0,aeq=0;

  sep_plane_calls++;  /* for verbose statistics */

  /* see where tb is with respect to ta plane */
  if ( (ta->flag & 0xF) == FACET  )
  { da = dotf(ta->normal,ta->x[0],gdim);
    n = ((tb->flag & 0xF) == FACET) ? 3 : 2;
    for ( k = 0 ; k < n ; k++ )
    { d = dotf(ta->normal,tb->x[k],gdim); 
      if ( d < da - 0.0001 ) { bnear++; continue; }
      if ( d < da + 0.0001 ) { beq++; continue; }
      bfar++;
    }
    if ( beq == n ) 
      return ((tb->flag&0xF)==EDGE)?FIRST_BACK:DISJOINT; /* in same plane */
    if ( bfar == 0 ) return FIRST_BACK;
  }

  /* see where ta is with respect to tb plane */
  if ( (tb->flag & 0xF) == FACET )
  { db = dotf(tb->normal,tb->x[0],gdim);
    n = ((ta->flag & 0xF) == FACET) ? 3 : 2;
    for ( k = 0 ; k < n ; k++ )
    { d = dotf(tb->normal,ta->x[k],gdim); 
      if ( d < db - 0.0001 ) { anear++; continue; }
      if ( d < db + 0.0001 ) { aeq++; continue; }
      afar++;
    }
    if ( aeq == n ) 
      return ((ta->flag&0xF)==EDGE)?SECOND_BACK:DISJOINT; /* same plane */
     if ( anear == 0 ) return FIRST_BACK;
  }
  
  na = (ta->flag & 0xF) == FACET ? 3 : 2;
  nna = (ta->flag & 0xF) == FACET ? 3 : 1;
  nb = (tb->flag & 0xF) == FACET ? 3 : 2;

  for ( i = 0 ; i < nna ; i++ )
  for ( j = 0 ; j < nb ; j++ )
  { REAL c[3],d; /* coefficients for plane */
    REAL da[3],db[3],minarea,s[3],length,dar[3],dbr[3];
    int ii,jj,jjj;

    for ( ii = 0 ; ii < na ; ii++ )
       a[ii] = ta->x[(i+ii)%na];
    for ( jj = 0 ; jj < nb ; jj++ )
       b[jj] = tb->x[(j+jj)%nb];
    for ( ii = 0 ; ii < 3 ; ii++ ) s[ii] = a[1][ii]-a[0][ii];
       length = sqrt(s[0]*s[0]+s[1]*s[1]+s[2]*s[2]);
    minarea = 1e-4*length;
    c[0] = s[1]*(b[0][2]-a[0][2]) - s[2]*(b[0][1]-a[0][1]);
    c[1] = s[2]*(b[0][0]-a[0][0]) - s[0]*(b[0][2]-a[0][2]);
    c[2] = s[0]*(b[0][1]-a[0][1]) - s[1]*(b[0][0]-a[0][0]);

    if ( c[0]*c[0] + c[1]*c[1] + c[2]*c[2] <= minarea*minarea ) 
         continue; /* degenerate */

    d = c[0]*a[0][0] + c[1]*a[0][1] + c[2]*a[0][2];

    for ( ii = 2 ; ii < na ; ii++ )
    { dar[ii] = c[0]*a[ii][0] + c[1]*a[ii][1] +c[2]*a[ii][2] - d;
      da[ii] = ( dar[ii] < -minarea ? -1.0 : ( dar[ii] > minarea ? 1.0 : 0.0));
    }
    for ( jj = 1 ; jj < nb ; jj++ )
    { dbr[jj] = c[0]*b[jj][0] + c[1]*b[jj][1] +c[2]*b[jj][2] - d;
      db[jj] = ( dbr[jj] < -minarea ? -1.0 : ( dbr[jj] > minarea ? 1.0 : 0.0));
    }

    /* test opposite sidedness */
    for ( jj = 1 ; jj < nb ; jj++ )
      for ( jjj = jj+1 ; jjj < nb ; jjj++ )
        if ( db[jj]*db[jjj] < 0.0 ) goto keeptrying;
    for ( ii = 2 ; ii < na ; ii++ )
      for ( jj = 1 ; jj < nb ; jj++ )
        if ( da[ii]*db[jj] > 0.0 ) goto keeptrying;

     /* have separating plane */
    { REAL asum,bsum;
      /* decide which is in front */
      for ( ii = 2, asum = 0.0 ; ii < na ; ii++ ) asum += c[2]*da[ii];
      for ( jj = 1, bsum = 0.0 ; jj < nb ; jj++ ) bsum += c[2]*db[jj];
      if ( asum > 0.0 || bsum < 0.0 ) return SECOND_BACK;
      else   return FIRST_BACK;
    }
keeptrying: ;
  }

  /* might have case of separating plane having two vertices on tb */
  if ( depth == 2 ) 
      return ASPLITTINGB|BSPLITTINGA;
  retval = separating_plane(tb,ta,2);
  if ( retval == FIRST_BACK ) 
      return SECOND_BACK;
  if ( retval == SECOND_BACK ) 
      return FIRST_BACK;
  return retval;

}

/*********************************************************************
*
* function: separating_line()
*
* purpose: See if two elements have a separating line in 2D.
*          To be called after bounding box tests.
*          Includes small tolerance.
*
* returns  DISJOINT or  NOTKNOWN
*/
int separating_line(ta,tb)
struct tsort *ta,*tb;
{ int i;
  int same = 0;
  int na,nb; /* vertices on respective elements */
  int nna,nnb; /* number of lines to try */
  int apos,aneg,bpos,bneg;
  float *a[3],*b[3];
  REAL width; /* thickness of separating line */ 

  sep_line_calls++;  /* for verbose statistics */

  /* get edge first, if any */
  if ( ((ta->flag & 0xF) == FACET) && ((tb->flag & 0xF) == EDGE ) )
  { struct tsort *tmp = ta; ta = tb; tb = tmp; }

  /* want to prevent overlap of facet with thick edge; not going
     to worry about edge-edge overlap, since that too weird. */
  if ( ((ta->flag & 0xF) == EDGE) && ((tb->flag & 0xF) == FACET ) )
  {
    width = ta->width/2;  /* actually need half-width */
  }
  else width = -1e-5;  /* allow slight overlap for numerical purposes */

  na = (ta->flag & 0xF) == FACET ? 3 : 2;
  nb = (tb->flag & 0xF) == FACET ? 3 : 2;
  nna = (ta->flag & 0xF) == FACET ? 3 : 1;
  nnb = (tb->flag & 0xF) == FACET ? 3 : 1;

  /* Try using edges of ta */
  for ( i = 0 ; i < nna ; i++ )
  { REAL cx,cy,d; /* coefficients for line */
    REAL dar[3],dbr[3],minarea;
    int ii,jj;

    for ( ii = 0 ; ii < na ; ii++ )
       a[ii] = ta->x[(i+ii)%na];

    for ( jj = 0 ; jj < nb ; jj++ )
       b[jj] = tb->x[jj];

     cx = a[1][1] - a[0][1]; cy = a[0][0] - a[1][0]; 
     d = cx*a[0][0] + cy*a[0][1];

     minarea = width*sqrt(cx*cx + cy*cy);

     if ( fabs(minarea) < 1e-20 ) 
      { same++;    continue; /* same point */ }

     apos = aneg = bpos = bneg = 0;
     for ( ii = 2 ; ii < na ; ii++ )
     { dar[ii] = cx*a[ii][0] + cy*a[ii][1] - d;
       if ( dar[ii] > minarea ) apos++;
       if ( dar[ii] < -minarea ) aneg++;
     }
     for ( jj = 0 ; jj < nb ; jj++ )
     { dbr[jj] = cx*b[jj][0] + cy*b[jj][1] - d;
       if ( dbr[jj] > minarea ) bpos++;
       if ( dbr[jj] < -minarea ) bneg++;
     }

     /* test opposite sidedness */
     if ( apos == (na-2) && bneg == nb ) return DISJOINT;
     if ( aneg == (na-2) && bpos == nb ) return DISJOINT;
  }

  /* Try using edges of tb */
  for ( i = 0 ; i < nnb ; i++ )
  { REAL cx,cy,d; /* coefficients for line */
    REAL dar[3],dbr[3],minarea;
    int ii,jj;

    for ( ii = 0 ; ii < nb ; ii++ )
       a[ii] = tb->x[(i+ii)%nb];

    for ( jj = 0 ; jj < na ; jj++ )
       b[jj] = ta->x[jj];

     cx = a[1][1] - a[0][1]; cy = a[0][0] - a[1][0]; 
     d = cx*a[0][0] + cy*a[0][1];

     width = -1e-5;
     minarea = width*sqrt(cx*cx + cy*cy);

     if ( fabs(minarea) < 1e-20 ) 
      { same++;    continue; /* same point */ }

     apos = aneg = bpos = bneg = 0;
     for ( ii = 2 ; ii < nb ; ii++ )
     { dar[ii] = cx*a[ii][0] + cy*a[ii][1] - d;
       if ( dar[ii] > minarea ) apos++;
       if ( dar[ii] < -minarea ) aneg++;
     }
     for ( jj = 0 ; jj < na ; jj++ )
     { dbr[jj] = cx*b[jj][0] + cy*b[jj][1] - d;
       if ( dbr[jj] > minarea ) bpos++;
       if ( dbr[jj] < -minarea ) bneg++;
     }

     /* test opposite sidedness */
     if ( apos == (nb-2) && bneg == na ) return DISJOINT;
     if ( aneg == (nb-2) && bpos == na ) return DISJOINT;
  }

  return NOTKNOWN;
 }

/*********************************************************************
*
* function: plane_test()
*
* purpose: See if one facet or edge is in front or back of element plane.
*          Suitable for Newell-Newell-Sancha algorithm.
*
* returns DISJOINT, FIRST_BACK, SECOND_BACK, ASPLITTINGB, BSPLITTINGA, or COPLANAR 
* Returns FIRST_BACK if guaranteed first does not obscure any of second.
*  Possibly bitwise OR of properties.
*/
int plane_test(ta,tb)
struct tsort *ta,*tb;
{
  REAL da,db;
  int k,n;
  int afar=0,aeq=0,anear=0;  /* count of ta vertices relative to tb plane */
  int bfar=0,beq=0,bnear=0;  /* count of tb vertices relative to ta plane */
  REAL d;
  int retval = NOTKNOWN;

  /* if two edges */
  if ( ((ta->flag & 0xF) == EDGE) && ((tb->flag & 0xF) == EDGE) )
  { REAL ab1x = tb->x[0][0] - ta->x[0][0];
    REAL ab1y = tb->x[0][1] - ta->x[0][1];
    REAL ab1z = tb->x[0][2] - ta->x[0][2];
    REAL ab2x = tb->x[1][0] - ta->x[0][0];
    REAL ab2y = tb->x[1][1] - ta->x[0][1];
    REAL ab2z = tb->x[1][2] - ta->x[0][2];
    REAL aax  = ta->x[1][0] - ta->x[0][0];
    REAL aay  = ta->x[1][1] - ta->x[0][1];
    REAL aaz  = ta->x[1][2] - ta->x[0][2];
    REAL area = ab1x*ab2y - ab1y*ab2x;
    REAL vol = (ab1x*ab2y - ab1y*ab2x)*aaz + (ab1y*ab2z - ab1z*ab2y)*aax
              + (ab1z*ab2x - ab1x*ab2z)*aay;
    if ( vol == 0.0 ) return COPLANAR;
    if ( area == 0.0 ) return DISJOINT;
    if ( area*vol > 0 ) return SECOND_BACK;
    return FIRST_BACK;
  }

  /* see where tb is with respect to ta plane */
  da = dotf(ta->normal,ta->x[0],gdim);
  n = ((tb->flag & 0xF) == FACET) ? 3 : 2;
  for ( k = 0 ; k < n ; k++ )
  {
    d = dotf(ta->normal,tb->x[k],gdim); 
    if ( d < da - 0.0001 ) { bnear++; continue; }
    if ( d < da + 0.0001 ) { beq++; continue; }
    bfar++;
  }
  if ( beq == n ) return COPLANAR; /* both in same plane */
  if ( bfar == 0 ) return FIRST_BACK; 
  if ( bnear > 0 ) 
  { if ( (ta->flag & 0xF) == FACET ) retval = ASPLITTINGB; }
  else retval = SECOND_BACK;
  
  /* see where ta is with respect to tb plane */

  db = dotf(tb->normal,tb->x[0],gdim);
  n = ((ta->flag & 0xF) == FACET) ? 3 : 2;
  for ( k = 0 ; k < n ; k++ )
  {
    d = dotf(tb->normal,ta->x[k],gdim); 
    if ( d < db - 0.0001 ) { anear++; continue; }
    if ( d < db + 0.0001 ) { aeq++; continue; }
    afar++;
  }
  if ( aeq == n ) return COPLANAR; /* both in same plane */
  if ( anear == 0 ) return FIRST_BACK;
  if ( afar > 0 )  
  { if ( (tb->flag & 0xf) == FACET ) retval |= BSPLITTINGA;   }
  else retval |= SECOND_BACK;
  
  /* might still not have properly detected order, so try this */
  /*
  if ( !(retval & (FIRST_BACK|SECOND_BACK)) )
    retval = separating_plane(ta,tb,0);
    */
    
  return retval;
  
 

} /* end plane_test() */

/************************************************************************
 ************************************************************************

 Visibility testing.  Takes output of depth sort and deletes hidden
 elements.  Uses sweep line (at random angle) to track topology.
 Much like Stuart Sechrest and Donald P. Greenberg, A Visible Polygon
 Reconstruction Algorithm, ACM Transactions on Graphics, vol 1, no 1,
 Jan 1982, pp 25-42.

 General strategy remarks:

 The image is broken down into individual polygon edges.  Each edge is
 "above" or "below" a polygon.  For now, each polygon is just a facet,
 but this could change if thick edges were added in the form of rectangles.

 The algorithm proceeds by moving a sweep line across the image, keeping
 track of "active edges" that intersect the sweep line.  The active edge
 list is altered at "events": edge starts, edge ends, and edge crossings.
 The sweep line is tilted at an arbitrary angle to prevent degenerate
 vertices, except vertices coincident in projection.  Pre-ordered event
 lists are kept for edge starts and ends, and a heap for upcoming
 crossings.

 Attached to each active edge is a list of layers of facets in the area
 immediately above it.

*************************************************************************/
int visdebuglevel;
#define VIS_TIMING  1
#define VIS_LAYERCHECK  2
#define VIS_EVENTDUMP 3
struct vis_conedge;
struct vis_vertex;

/* Heap for ordering upcoming events. */
int vis_heap_count;  /* heap spots used */
int vis_heap_max;    /* heap spots allocated */
struct vis_event *vis_heap;
void vis_insert_heap ARGS((struct vis_event *));
void vis_delete_heap ARGS((int));
void find_next_event ARGS((struct vis_conedge *));
void find_next_event2 ARGS(( struct vis_conedge *, struct vis_conedge *));
void vis_crossing ARGS((struct vis_conedge *,struct vis_conedge *)); 
int vecount;  /* number of edges in edge list */
int vis_crossing_count; /* just for info */
int add_layer ARGS((struct vis_conedge *, struct tsort *));
int delete_layer ARGS((struct vis_conedge *, struct tsort *));
void check_layers ARGS(( struct vis_conedge *, REAL , REAL));
int vvcomp ARGS(( struct vis_vertex *, struct vis_vertex *));
REAL activate_edge ARGS(( struct vis_conedge *));
void check_deactivate ARGS(( struct vis_conedge *));

/* Edge list */
#define MAXLAYERS 20
struct vis_rawedge { struct vis_vertex *v[2]; /*  endpoints */
                  struct tsort *t; /* facet it borders */
                  struct vis_conedge *conedge;
                  int flags;    /* see below */
  };
struct vis_rawedge *vis_rawedges;
struct vis_rawedge **rawplist;  /* pointers for sorting */

int vecomp ARGS((struct vis_rawedge **, struct vis_rawedge **));
/* vis_rawedge flag bits */
#define V_FACET_BOTTOM 1
#define V_FACET_TOP   2
#define V_FACET_LEFT   4
#define V_FACET_RIGHT   8
#define V_LAYER_CHECK  0x10

/* Consolidated vertices */
struct vis_vertex { REAL x[2];   /* u, v */
                    struct vis_vertex **fixup[2];  /* pre-cons reverse pointer */
    };
struct vis_vertex *vis_vertices;
int vis_vertex_max;
int vis_vertex_count;

/* Consolidated edges */
struct vis_conedge {
                  struct vis_vertex *v[2];  /* endpoints */
                  REAL m;     /* line slope */
                  int rawstart;  /* associated raw edge start */
                  int rawend;  /* last associated raw edge */
                  int use_count; /* times in use as boundary */
                  struct vis_conedge *prev_active;  /* active list pointer */
                  struct vis_conedge *next_active;  /* active list pointer */
                  int flags;    /* see below */
                  int layers;  /* number of layers above edge */
                  struct tsort **layer; /* facets "above" edge */
                  int maxlayers; /* allocated space */
                  int seqno;    /* sequence number, for debugging */
    };
struct vis_conedge *vis_conedges;
int vis_conedge_max;
int vis_conedge_count;

/* Crossing event */
struct vis_event { REAL time;  /* sweep time of event */
                   int type;
                   struct vis_conedge *e1;
                   struct vis_conedge *e2;
                   struct tsort *t;
 };
int vis_event_comp ARGS((struct vis_event *, struct vis_event *));

/* Event types, ordered in way wanted in sorting */
#define V_FACET_END 1
#define V_FACET_TOPMIDDLE 2
#define V_FACET_BOTTOMMIDDLE 3
#define V_FACET_START 4
#define V_EDGE_CROSSING 5

int wrong_middles;  /* for some debugging */

/* Margin to shorten edge ends, so don't get spurious crossings */
REAL veps = 1e-14;

/* Active edge list */
struct vis_conedge *active_edge_first;
struct vis_conedge sentinel; 
struct vis_vertex sentinelv[4]; /* for sentinel endpoints */

/* List of edges to check for top layer */
struct vis_conedge **check_list;
int check_list_count;
int check_list_max;
void check_visible ARGS((REAL));

/* random tilt coefficients for sweep line */
REAL va = 0.8432848996472634;
REAL vb = 0.5869487870825054;

REAL sweep_u;  /* current sweep position */
struct vis_event *facet_events;
int f_event_count;
int facet_start_event ARGS((struct vis_event *));
int facet_middle_event ARGS((struct vis_event *));
int facet_end_event ARGS((struct vis_event *));
/* For brute force verification */
int brute_force_flag = 1;
void brute_force_times ARGS((void));
int brutecount;  /* number of brute force times */
struct brute { REAL time;
               struct vis_conedge *e1,*e2;
               int type;
 } *brute_times;
void brute_section ARGS((REAL));
void brute_visible ARGS((REAL));
int maxbrute;
struct brute_cut { REAL v; /* height */
                   struct vis_edge *e;
 } *brute_cuts;
int brute_cut_count;

#ifdef PROFILING_ENABLED
/* Profiling cycle counters */
__int32 visibility_stage_elapsed_time[2];
__int32 visibility_end_elapsed_time[2];
__int32 visibility_end1_elapsed_time[2];
__int32 visibility_end2_elapsed_time[2];
__int32 visibility_end3_elapsed_time[2];
__int32 visibility_end4_elapsed_time[2];
__int32 visibility_end5_elapsed_time[2];
__int32 vis_insert_heap_elapsed_time[2];
__int32 vis_delete_heap_elapsed_time[2];
__int32 vis_crossing_elapsed_time[2];
__int32 facet_start_event_elapsed_time[2];
__int32 facet_middle_event_elapsed_time[2];
__int32 facet_end_event_elapsed_time[2];
__int32 add_layer_elapsed_time[2];
__int32 delete_layer_elapsed_time[2];
__int32 check_visible_elapsed_time[2];
__int32 find_next_event_elapsed_time[2];
__int32 all_visibility_elapsed_time[2];
__int32 activate_edge_elapsed_time[2];
__int32 check_deactivate_elapsed_time[2];
#endif

/************************************************************************
*
* function: visibility_stage()
*
* purpose: Adds element to list for visibility testing.
*          Enabled by visibility_test toggle.
*
*/

void visibility_stage(t)
struct tsort *t;
{
PROF_START(all_visibility)
PROF_START(visibility_stage)

  if ( !visibility_test )
  { if ( (t->flag & 0xF) == FACET )
     (*display_facet)(t);
    else (*display_edge)(t);
    return;
  }

  /* accumulate */
  if ( vis_list == NULL )
  { vis_max = maxcount;
    vis_list = (struct tsort *)temp_calloc(vis_max,sizeof(struct tsort));
  }
  else if ( vis_count >= vis_max - 1 )
  { vis_list = (struct tsort *)temp_realloc((char*)vis_list,
        2*vis_max*sizeof(struct tsort));
    vis_max *= 2;
  }

  vis_list[vis_count++] = *t;

PROF_FINISH(visibility_stage)
PROF_FINISH(all_visibility)
}

/*
   FOR DEBUGGING
*/
void active_list_check ARGS((void));

void active_list_check()
{ struct vis_conedge *e;
  
  for ( e = active_edge_first ; e != &sentinel ; e = e->next_active )
  { if ( e->next_active->prev_active != e )
      kb_error(2418,"Visibility edge active list bad.\n",RECOVERABLE);
  }
}

/************************************************************************
* 
* function: vvcomp()
*
* purpose: comparison of vertices, for consolidation
*
*/
int vvcomp(a,b)
struct vis_vertex *a,*b;
{
  if ( a->x[0] < b->x[0] ) return -1;
  if ( a->x[0] > b->x[0] ) return  1;
  if ( a->x[1] < b->x[1] ) return -1;
  if ( a->x[1] > b->x[1] ) return  1;
  return 0;
}

/************************************************************************
* 
* function: vecomp()
*
* purpose: comparison of raw edges, for consolidation
*
*/
int vecomp(a,b)
struct vis_rawedge **a,**b;
{
  if ( a[0]->v[0] < b[0]->v[0] ) return -1;
  if ( a[0]->v[0] > b[0]->v[0] ) return  1;
  if ( a[0]->v[1] < b[0]->v[1] ) return -1;
  if ( a[0]->v[1] > b[0]->v[1] ) return  1;
  return 0;
}

/************************************************************************
*
* function: visibility_end()
*
* purpose: Run visibility algorithm after accumulation of data.
*
*/
int debug_seq = 0;  /* for debugging */

void visibility_end()
{ int k,i,ii,iii,j;
  struct tsort *t;
  struct vis_rawedge *ve;
  struct vis_conedge *vc;
  struct vis_vertex *vv;
  REAL next_u;
  struct vis_event *f_ev;
  int facet_event_spot;
  int vis_display_count;
  int tops,bottoms,lefts,rights;

debug_seq = 0;

PROF_START(all_visibility)
PROF_START(visibility_end)

PROF_START(visibility_end1)
  /* List of edges to check top facet for */
  check_list_max = 1000;
  check_list = (struct vis_conedge **)temp_calloc(check_list_max,
                   sizeof(struct vis_conedge *));
  check_list_count = 0;

  /* Sorted list of facet starts, middle vertices, and ends */
  facet_events = (struct vis_event *)temp_calloc(3*vis_count,
                                       sizeof(struct vis_event));
  f_event_count = 0;
  f_ev = facet_events;

  /* Populate raw edge and vertex lists */
  vis_vertex_max = 3*vis_count;
  vis_vertices = (struct vis_vertex *)temp_calloc(vis_vertex_max,
                                              sizeof(struct vis_vertex));
  vv = vis_vertices;
  vis_vertex_count = 0;
  
  vis_rawedges = (struct vis_rawedge *)temp_calloc(3*vis_count,
                                              sizeof(struct vis_rawedge));
  ve = vis_rawedges;
  vecount = 0;
  

  for ( k = 0, t = vis_list ; k < vis_count ; k++,t++ )
  { if ( (t->flag & 0xF) == FACET )
    {
      REAL minu = 1e30, maxu = -1e30;

      if ( t->color == CLEAR )
      { t->flag |= VISIBLE; continue; }  /* kludge for now */

      /* rotate coordinates */
      for ( i = 0 ; i < FACET_VERTS ; i++ )
      { vv[i].x[0] = va*t->x[i][0] + vb*t->x[i][1];
        if ( vv[i].x[0] < minu ) minu = vv[i].x[0];
        if ( vv[i].x[0] > maxu ) maxu = vv[i].x[0];
        vv[i].x[1] = -vb*t->x[i][0] + va*t->x[i][1];
      }

      /* now, the edges */
      tops = 0; bottoms = 0; lefts = 0; rights = 0;
      for ( i = 0 ; i < FACET_VERTS ; i++ )
      { REAL area;
        ii = (i+1 >= FACET_VERTS) ? 0 : i+1;
        if ( vv[i].x[0] <= vv[ii].x[0] )  /* leftmost vertex first */
        { ve->v[0] = vv+i;
          ve->v[1] = vv+ii;
          vv[i].fixup[0] = &ve->v[0];  /* so can adjust edges after sorting vertices */
          vv[ii].fixup[1] = &ve->v[1];
        } else
        { ve->v[0] = vv+ii;
          ve->v[1] = vv+i;
          vv[i].fixup[0] = &ve->v[1];  /* so can adjust edges after sorting vertices */
          vv[ii].fixup[1] = &ve->v[0];
        }

        iii = (ii+1 >= FACET_VERTS) ? 0 : ii+1;  /* third vertex */
        area = (ve->v[1]->x[0]-ve->v[0]->x[0])*(vv[iii].x[1]-ve->v[0]->x[1])
          -(vv[iii].x[0]-ve->v[0]->x[0])*(ve->v[1]->x[1]-ve->v[0]->x[1]); 
        if ( fabs(area) < 1e-14 ) break;
        if ( area > 0 )
        { ve->flags |= V_FACET_BOTTOM; bottoms++; }
        else { ve->flags |= V_FACET_TOP; tops++; }
        if ( ve->v[0]->x[0] == minu )
        { ve->flags |= V_FACET_LEFT; lefts++; }
        if ( ve->v[1]->x[0] == maxu )
        { ve->flags |= V_FACET_RIGHT; rights++; }
        ve->t = t;
        ve++; vecount++;
      }

      /* check we successfully found things, and skip edge-on facets */
      if ( (tops==0) || (bottoms==0) || (lefts!=2) || (rights!=2) )
        continue;
      
      /* facet start event */
      f_ev->type = V_FACET_START;
      f_ev->time = minu;
      f_ev->t = t;
      for ( i = -FACET_EDGES ; i < 0 ; i++ )
      { if ( ve[i].flags & V_FACET_LEFT )
        { if ( ve[i].flags & V_FACET_BOTTOM )
            f_ev->e1 = (struct vis_conedge*)(ve + i);
          else f_ev->e2 = (struct vis_conedge*)(ve + i);
        }
      }
      if ( !f_ev->e1 || !f_ev->e2 )
      { f_ev->e1 = f_ev->e2 = NULL; continue; } /* skip edge-on */
      f_ev++; f_event_count++;

      /* facet middle event */
      f_ev->t = t;
      for ( i = -FACET_EDGES ; i < 0 ; i++ )
      { if ( ve[i].flags & V_FACET_LEFT )
        { if ( !(ve[i].flags & V_FACET_RIGHT) )
          { f_ev->e1 = (struct vis_conedge*)(ve + i);
            f_ev->time = ve[i].v[1]->x[0];
          }
        }
        if ( ve[i].flags & V_FACET_RIGHT )
        { if ( !(ve[i].flags & V_FACET_LEFT) )
            f_ev->e2 = (struct vis_conedge*)(ve + i);
        }
      }
      if ( !f_ev->e1 || !f_ev->e2 ) /* skip edge-on */
      { f_ev->e1 = f_ev->e2 = NULL; f_ev--; f_event_count--; continue; }
      if ( ((struct vis_rawedge *)f_ev->e1)->flags & V_FACET_BOTTOM ) 
           f_ev->type = V_FACET_BOTTOMMIDDLE;
      else f_ev->type = V_FACET_TOPMIDDLE;
      f_ev++; f_event_count++;

      /* facet end event */
      f_ev->type = V_FACET_END;
      f_ev->time = maxu;
      f_ev->t = t;
      for ( i = -FACET_EDGES ; i < 0 ; i++ )
      { if ( ve[i].flags & V_FACET_RIGHT )
        { if ( ve[i].flags & V_FACET_BOTTOM )
            f_ev->e1 = (struct vis_conedge*)(ve + i);
          else f_ev->e2 = (struct vis_conedge*)(ve + i);
        }
      }
      if ( !f_ev->e1 || !f_ev->e2 ) /* skip edge-on */
      { f_ev->e1 = f_ev->e2 = NULL; f_ev-=2; f_event_count-=2; continue; }
      f_ev++; f_event_count++;

      vv += FACET_VERTS; vis_vertex_count += FACET_VERTS;
    } 
    else  /* lone edge */
    { 
      /* for now, make all lone edges visible */
      t->flag |= VISIBLE;
    }
  }

  PROF_FINISH(visibility_end1)

  if ( f_event_count == 0 ) 
    goto draw_visible;

  PROF_START(visibility_end2)

  /* Consolidation of vertices and edges */
  /* First, consolidation of vertices */
  qsort(vis_vertices,vis_vertex_count,sizeof(struct vis_vertex),FCAST vvcomp);
  for ( i = -1, j = 0 ; j < vis_vertex_count ; j++ )
  { int m;
    if ( (i < 0) || (vvcomp(vis_vertices+i,vis_vertices+j) != 0) ) 
    { vis_vertices[++i] = vis_vertices[j];
    }
    /* use reverse pointers to fix up edges */
    for ( m = 0 ; m < 2 ; m++ )
     *(vis_vertices[j].fixup[m]) = vis_vertices+i;
  }
  vis_vertex_count = i+1;
  PROF_FINISH(visibility_end2)

  /* Next, consolidation of edges.  Use intermediate list of pointers,
     since events point to raw edges.
  */
  PROF_START(visibility_end3)
  rawplist = (struct vis_rawedge **)temp_calloc(vecount,
                  sizeof(struct vis_rawedge *));
  for ( i = 0 ; i < vecount ; i++ ) rawplist[i] = vis_rawedges+i;
  qsort(rawplist,vecount,sizeof(struct vis_rawedge*),FCAST vecomp);
  vis_conedge_max = vecount;
  vis_conedges = (struct vis_conedge *) temp_calloc(vis_conedge_max,
                     sizeof(struct vis_conedge) );
  vis_conedges[0].v[0] = rawplist[0]->v[0];
  vis_conedges[0].v[1] = rawplist[0]->v[1];
  vis_conedges[0].rawstart = 0;
  vis_conedges[0].rawend = 0;
  rawplist[0]->conedge = vis_conedges;
  for ( i = 0, j = 1 ; j < vecount ; j++ )
  { if ( (vis_conedges[i].v[0] != rawplist[j]->v[0]) || 
         (vis_conedges[i].v[1] != rawplist[j]->v[1]) )
    { i++;
      vis_conedges[i].v[0] = rawplist[j]->v[0];
      vis_conedges[i].v[1] = rawplist[j]->v[1];
      vis_conedges[i].rawstart = j;
      vis_conedges[i].rawend = j;
    }
    else
      vis_conedges[i].rawend++;
    rawplist[j]->conedge = vis_conedges+i;
  }
  i++;
  vis_conedges = (struct vis_conedge*)temp_realloc((char*)vis_conedges,
     i*sizeof(struct vis_conedge));
  vis_conedge_max = vis_conedge_count = i;
  PROF_FINISH(visibility_end3)

  PROF_START(visibility_end4)
  /* Initialize line coefficients */
  for ( i = 0, vc = vis_conedges; i < vis_conedge_count; i++, vc++ )
  { REAL du = vc->v[1]->x[0] - vc->v[0]->x[0];
    REAL dv = vc->v[1]->x[1] - vc->v[0]->x[1];
    vc->m = dv/du;
  }

  /* change facet events over to consolidated edges */
  for ( i = 0 ; i < f_event_count ; i++ )
  { facet_events[i].e1 = ((struct vis_rawedge*)(facet_events[i].e1))->conedge;
    facet_events[i].e2 = ((struct vis_rawedge*)(facet_events[i].e2))->conedge;
  }
  PROF_FINISH(visibility_end4)

  /* and for later use */
  brute_cuts=(struct brute_cut *)temp_calloc(vecount,sizeof(struct brute_cut));

#ifdef BRUTE
  /* do brute-force list of all event times */
  brute_force_times();
  for ( i = 0 ; i < brutecount-1 ; i++ )
  { if ( brute_times[i+1].time-brute_times[i].time > 1e-8 )
      brute_visible((brute_times[i].time+brute_times[i+1].time)/2);
  }
#endif

#define FINESSE
#ifdef FINESSE

  /* sort facet event list */
  qsort((char*)facet_events,f_event_count,sizeof(struct vis_event),
                        FCAST vis_event_comp);

  /* Initialize crossing event heap with sentinel */
  vis_heap_max = vecount > 100 ? vecount : 100;
  vis_heap = (struct vis_event *)temp_calloc(vis_heap_max,
                                          sizeof(struct vis_event));
  vis_heap[0].time = 1e30;
  vis_heap_count = 1;
  vis_crossing_count = 0;

  /* Initialize active list */
  sentinel.v[0] = sentinelv;
  sentinel.v[1] = sentinelv+1;
  sentinelv[0].x[0] = -1e20;
  sentinelv[0].x[1] = 1e20;
  sentinelv[1].x[0] = 1e20;
  sentinelv[1].x[1] = 1e20;
  sentinel.m = 0;
  sentinel.prev_active = NULL;
  sentinel.next_active = NULL;
  active_edge_first = &sentinel;

  PROF_START(visibility_end5)
  /* Sweep */
  facet_event_spot = 0;
  while ( facet_event_spot < f_event_count )
  { struct vis_event *fe = facet_events + facet_event_spot;
    int retval; /* return code from event handlers; < 0 for error */

/* print current active list and event heap */
if ( visdebuglevel >= VIS_EVENTDUMP )
{ struct vis_conedge *es;

  if ( active_edge_first->prev_active != NULL )
     printf("(%d<-) ",active_edge_first->prev_active-vis_conedges);
  for ( es = active_edge_first ; es != &sentinel ; es = es->next_active )
  { if ( es->next_active->prev_active != es )
     printf("(%d<-) ",es->next_active->prev_active-vis_conedges);
    printf("%d ",es-vis_conedges);
  }
  for ( i = 0 ; i < vis_heap_count ; i++ )
    if ( vis_heap[i].time > 1e20 ) printf("(sentinel) ");
    else
      printf("(%d %d %f)",vis_heap[i].e1-vis_conedges,vis_heap[i].e2-vis_conedges,
       vis_heap[i].time); 
  printf("\n");
} /* end debug */

    /* find which is next event and process */
    if ( (vis_heap_count <= 0) || (fe->time < vis_heap[0].time) )
    { /* do facet event */
      next_u = fe->time;
  PROF_FINISH(visibility_end)
      if ( (next_u - sweep_u) > 1e-10 )
         check_visible((next_u+sweep_u)/2); 
      sweep_u = next_u;
      switch ( fe->type )
      { case V_FACET_START: 
             retval = facet_start_event(fe); 
             if ( retval < 0 )
               goto bail_out;
             break;
        case V_FACET_TOPMIDDLE: 
             retval = facet_middle_event(fe); 
             if ( retval < 0 )
               goto bail_out;
             break;
        case V_FACET_BOTTOMMIDDLE:
             retval = facet_middle_event(fe); 
             if ( retval < 0 )
               goto bail_out;
             break;
        case V_FACET_END: 
             retval = facet_end_event(fe); 
             if ( retval < 0 )
               goto bail_out;
             break;
      } 
PROF_START(visibility_end)

      facet_event_spot++;
      continue;
    }
    else /* do crossing event */
    { struct vis_conedge *e1,*e2;
      next_u = vis_heap[0].time;
  PROF_FINISH(visibility_end)
      if ( (next_u - sweep_u) > 1e-10 )
         check_visible((next_u+sweep_u)/2); 
      e1 = vis_heap[0].e1; e2 = vis_heap[0].e2;
      vis_delete_heap(0);
      if ( visdebuglevel >= VIS_EVENTDUMP )
        printf("crossing %d %d  at %20.15f\n",e1-vis_conedges,e2-vis_conedges,(double)next_u);
      sweep_u = next_u;   
      vis_crossing(e1,e2);
PROF_START(visibility_end)

      continue;
    }
  } 
#endif

  goto draw_visible;

bail_out: /* error handling: mark all as visible */
  erroutstring("Abandoning visibility test and drawing all facets.\n");
  for ( k = 0, t = vis_list, vis_display_count = 0 ; k < vis_count ; k++,t++ )
    t->flag |= VISIBLE;

draw_visible:
  PROF_FINISH(visibility_end5)
  PROF_FINISH(visibility_end)
  PROF_FINISH(all_visibility)

  /* Display elements marked visible */
  for ( k = 0, t = vis_list, vis_display_count = 0 ; k < vis_count ; k++,t++ )
  if ( t->flag & VISIBLE )
  { if ( (t->flag & 0xF) == FACET )
     (*display_facet)(t);
    else (*display_edge)(t);
    vis_display_count++;
  }
  temp_free((char*)vis_heap);
  temp_free((char*)vis_conedges);
  temp_free((char*)vis_rawedges);
  temp_free((char*)vis_vertices);
  temp_free((char*)check_list);
  temp_free((char*)rawplist);
  temp_free((char*)vis_list);
  temp_free((char*)facet_events);
  temp_free((char*)brute_cuts);


if (visdebuglevel >= VIS_TIMING)
{
fprintf(stderr,"Visible: %d facets out of %d\n",vis_display_count,vis_count);
fprintf(stderr,"Crossing count: %d\n",vis_crossing_count);
fprintf(stderr,"Wrong middles: %d\n",wrong_middles); wrong_middles = 0;
#ifdef MSC
/* print out profile times, only for Visual Studio */
printf("CPU clock cycles in various visibility routines:\n");
PROF_PRINT(all_visibility)
PROF_PRINT(visibility_stage)
PROF_PRINT(visibility_end)
PROF_PRINT(visibility_end1)
PROF_PRINT(visibility_end2)
PROF_PRINT(visibility_end3)
PROF_PRINT(visibility_end4)
PROF_PRINT(visibility_end5)
PROF_PRINT(check_visible)
PROF_PRINT(vis_crossing)
PROF_PRINT(facet_start_event)
PROF_PRINT(facet_middle_event)
PROF_PRINT(facet_end_event)
PROF_PRINT(vis_insert_heap)
PROF_PRINT(vis_delete_heap)
PROF_PRINT(find_next_event)
PROF_PRINT(add_layer)
PROF_PRINT(delete_layer)
PROF_PRINT(activate_edge)
PROF_PRINT(check_deactivate)
#endif
}
}

/************************************************************************
*
* function: vis_event_comp()
*
* purpose: compare times of two events.  Sorts on time, then type,
*          then average slope, then edges.
*/

int vis_event_comp(a,b)
struct vis_event *a, *b;
{ REAL ma,mb;
  if ( a->time < b->time ) return -1;
  if ( a->time > b->time ) return  1;
  if ( a->type < b->type ) return -1;
  if ( a->type > b->type ) return  1;
  ma = (a->e1->m+a->e2->m);
  mb = (b->e1->m+b->e2->m);
  if ( ma < mb ) return -1;
  if ( ma > mb ) return  1;
  if ( a->e1 < b->e1 ) return -1;
  if ( a->e1 > b->e1 ) return  1;
  if ( a->e2 < b->e2 ) return -1;
  if ( a->e2 > b->e2 ) return  1;
  return 0;
}

/************************************************************************
*
* function: vis_insert_heap()
*
* purpose: Add edge event to heap list.  Not detecting duplicates;
*          leaving that to validity test when crossing is handled.
*
*/

void vis_insert_heap(e)
struct vis_event *e;
{ int k,kk;
  int result;

PROF_START(vis_insert_heap)

  if ( vis_heap_count >= vis_heap_max-1 )
  { vis_heap = (struct vis_event *)kb_realloc((char*)vis_heap,
                 2*vis_heap_max*sizeof(struct vis_event));
    vis_heap_max *= 2;
  }

  for ( k = vis_heap_count ; k > 0 ; k = kk )
  { 
    kk = (k-1)/2;
    result = vis_event_comp(e,&vis_heap[kk]); 
    if ( result < 0 )
    { 
      vis_heap[k] = vis_heap[kk];
    }
    else break;
  }
  vis_heap[k] = *e;
  vis_heap_count++;

PROF_FINISH(vis_insert_heap)
}

/***************************************************************************
*
* function: vis_delete_heap()
*
* purpose: Delete element n of heap and adjust heap.
*/

void vis_delete_heap(n)
int n;
{ int k,kk;
  int result;
  struct vis_event e;

PROF_START(vis_delete_heap)

  if ( n == vis_heap_count-1 ) 
  { vis_heap_count--;
    goto vis_delete_heap_exit;
  }

  if ( vis_heap_count == 1 )
   kb_error(2421,"vis_delete_heap trying to delete sentinel.\n",RECOVERABLE);

  /* replace with top event */
  e = vis_heap[--vis_heap_count]; 

  /* check direction to percolate */
  result = (n==0) ? 1 : vis_event_comp(&e,vis_heap+(n-1)/2);
  if ( result < 0 )
  { /* downward */
    k = n;
    kk = (n-1)/2;
    vis_heap[k] = vis_heap[kk];
    for ( k = kk ; k > 0 ; k = kk )
    { kk = (k-1)/2;
      result = vis_event_comp(&e,vis_heap+kk);
      if ( result < 0 )
      { vis_heap[k] = vis_heap[kk];
        continue;
      }
      else 
      { break;
      }
    }
    vis_heap[k] = e;
    goto vis_delete_heap_exit; 
  }
  else
  { /* upward */
    for ( k = n; 2*k+1 < vis_heap_count ; k = kk )
    { 
      if ( 2*k+2 >= vis_heap_count )
      { /* only one parent */
        kk = 2*k+1;
        result = vis_event_comp(&e,vis_heap+kk);
        if ( result > 0 )
        { vis_heap[k] = vis_heap[kk]; 
          vis_heap[kk] = e;
          goto vis_delete_heap_exit;
        }
        else 
        { vis_heap[k] = e;
          goto vis_delete_heap_exit;
        }
      }
      else
      { /* two parents */
        result = vis_event_comp(&vis_heap[2*k+1],&vis_heap[2*k+2]);
        kk = (result < 0) ? 2*k+1 : 2*k+2;
        result = vis_event_comp(&e,vis_heap+kk);
        if ( result < 0 )
        { /* done */
          vis_heap[k] = e;
          goto vis_delete_heap_exit;
        }
        else if ( result > 0 )
        { /* keep going up */
          vis_heap[k] = vis_heap[kk];
          continue;
        }
      }
    }
    vis_heap[k] = e;   /* in case at top of heap */
  }
vis_delete_heap_exit: ;
PROF_FINISH(vis_delete_heap)
}

/****************************************************************************
*
* function: add_layer()
*
* purpose: Add a facet to the layers above an active edge.
*
* return value: 1 if added, 0 if already there.
*/

int add_layer(ee,f)
struct vis_conedge *ee;
struct tsort *f;
{ int i;
  int retval;

PROF_START(add_layer)

  for ( i = 0 ; i < ee->layers ; i++ )
    if ( ee->layer[i] == f ) break;
  if ( i == ee->maxlayers )
  { int newcount = (ee->maxlayers > 10) ? 2*ee->maxlayers : ee->maxlayers + 10;
    ee->layer = (struct tsort **)temp_realloc((char*)(ee->layer),
        newcount*sizeof(struct tsort*));
    ee->maxlayers = newcount;
  }
  if ( i == ee->layers )
  { /* not already found in layer list */
    ee->layer[ee->layers++] = f;
    if ( visdebuglevel >= VIS_EVENTDUMP )
      fprintf(stderr,"Adding facet %d to edge %d layers.\n",
         f-vis_list,ee-vis_conedges);
    if ( !(ee->flags & V_LAYER_CHECK) )
    { 
      if ( check_list_count >= check_list_max )
      { check_list = (struct vis_conedge **)temp_realloc((char*)check_list,
          2*check_list_max*sizeof(struct vis_conedge *));
        check_list_max *= 2;
      }
      check_list[check_list_count++] = ee;
      ee->flags |= V_LAYER_CHECK;
    }
    retval = 1;
  }
  else retval = 0;

PROF_FINISH(add_layer)
  return retval;
}

/****************************************************************************
*
* function: delete_layer()
*
* purpose: Delete a facet from the layers above an active edge.
*          Not keeping remaining facets in depth order.
* 
* return value: 1 if found, 0 if not.
*/

int delete_layer(ee,f)
struct vis_conedge *ee;
struct tsort *f;
{ int i;
  int retval = 0;

PROF_START(delete_layer)

  for ( i = 0 ; i < ee->layers ; i++ )
    if ( ee->layer[i] == f )
    { ee->layer[i] = ee->layer[--ee->layers];
      if ( visdebuglevel >= VIS_EVENTDUMP )
        fprintf(stderr,"Deleting facet %d from edge %d layers.\n",
          f-vis_list,ee-vis_conedges);
      if ( !(ee->flags & V_LAYER_CHECK) )
      { if ( check_list_count >= check_list_max )
        { check_list = (struct vis_conedge **)temp_realloc((char*)check_list,
            2*check_list_max*sizeof(struct vis_conedge *));
          check_list_max *= 2;
        }
        check_list[check_list_count++] = ee;
        ee->flags |= V_LAYER_CHECK;
      }
      retval = 1;
      break;
    }
PROF_FINISH(delete_layer)
  return retval;
}



/*************************************************************************
*
* function: find_next_event()
*
* purpose:  Check for upcoming crossing event.  Doesn't count crossing
*           near end of edge.   Need to do very robust crossing
*           calculation, so not to be fooled by numerical glitches.
*
*/

void find_next_event(e)
struct vis_conedge *e;
{ struct vis_event ev;

PROF_START(find_next_event)
  ev.time = 1e30;
 
  /* Forward */
  if ( (e->m > e->next_active->m )  )
  { REAL u;
    u = e->v[0]->x[0] + (e->v[0]->x[1]-e->next_active->v[0]->x[1] +
         e->next_active->m*(e->next_active->v[0]->x[0]-e->v[0]->x[0]))/
              (e->next_active->m - e->m);
    if ( (u > e->v[0]->x[0]-1e-8) && (u < ev.time) && (u < e->v[1]->x[0] - 1e-10)
             && (u < e->next_active->v[1]->x[0] - 1e-10) )
    { ev.e1 = e;
      ev.e2 = e->next_active; 
      ev.time = u;
    }
  }
 
  /* Backward */
  if ( e->prev_active && (e->m < e->prev_active->m))
  { REAL u;
    u = e->v[0]->x[0] + (e->v[0]->x[1]-e->prev_active->v[0]->x[1] +
         e->prev_active->m*(e->prev_active->v[0]->x[0]-e->v[0]->x[0]))/
              (e->prev_active->m - e->m);
    if ( (u > e->v[0]->x[0]-1e-8) && (u < ev.time) && (u < e->v[1]->x[0] - 1e-10)
             && (u < e->prev_active->v[1]->x[0] - 1e-10) ) 
    { ev.e1 = e->prev_active;
      ev.e2 = e; 
      ev.time = u;
    }
  }


PROF_FINISH(find_next_event)
  if ( ev.time < 1e20 )
  { 
    if ( visdebuglevel >= VIS_EVENTDUMP )
      printf("next crossing %d %d at %20.15f\n",
        ev.e1-vis_conedges,ev.e2-vis_conedges, (double)ev.time);
    vis_insert_heap(&ev);
  }

} 

/*************************************************************************
*
* function: find_next_event2()
*
* purpose:  Check for upcoming crossing event between two given edges.  
*           Doesn't count crossing
*           near end of edge.   Need to do very robust crossing
*           calculation, so not to be fooled by numerical glitches.
*
*/

void find_next_event2(e,ee)
struct vis_conedge *e;
struct vis_conedge *ee;
{ struct vis_event ev;

  if ( !e || !ee ) return;

  ev.time = 1e30;
 
  /* Forward */
  if ( (e->m > ee->m )  )
  { REAL u;
    u = e->v[0]->x[0] + (e->v[0]->x[1]-e->next_active->v[0]->x[1] +
         e->next_active->m*(e->next_active->v[0]->x[0]-e->v[0]->x[0]))/
              (e->next_active->m - e->m);
    if ( (u > e->v[0]->x[0]-1e-8) && (u < ev.time) && (u < e->v[1]->x[0] - 1e-10)
             && (u < e->next_active->v[1]->x[0] - 1e-10) )
    { ev.e1 = e;
      ev.e2 = e->next_active; 
      ev.time = u;
    }
  }

  if ( ev.time < 1e20 )
  { 
    if ( visdebuglevel >= VIS_EVENTDUMP )
      printf("next crossing %d %d at %20.15f\n",
        ev.e1-vis_conedges,ev.e2-vis_conedges, (double)ev.time);
    vis_insert_heap(&ev);
  }
} 

#ifdef XXXXXX
/* for debugging */
/**************************************************************************
*
* function: dump_vislist()
*
* purpose: dump edge list in case of error.
*/

void dump_vislist()
{ struct vis_conedge *e;
  REAL v;
  int i;

  fprintf(stderr,"Visibility edge list debug dump at u = %18.15f; debug_seq %d\n",
      sweep_u,debug_seq);
  for ( e = active_edge_first ; e != &sentinel ; e = e->next_active )
  { if ( e == NULL ) { fprintf(stderr,"NULL next_active.\n"); break; }
    v = e->m*(sweep_u-e->v[0]->x[0]) + e->v[0]->x[1]; 
    fprintf(stderr,"%3d   %5d v: %18.15f layers:",e->seqno,e-vis_conedges,v);
    for ( i = 0 ; i < e->layers ; i++ ) 
      fprintf(stderr," %3d",ordinal(e->layer[i]->f_id)+1);
    fprintf(stderr,"\n");
 }
 fprintf(stderr,"\n");
}

void check_vislist()  /* for v in ascending sequence */
{ struct vis_conedge *e;
  REAL v,prev = -1e30;

  for ( e = active_edge_first ; e != &sentinel ; e = e->next_active )
  { if ( e == NULL ) { fprintf(stderr,"NULL next_active.\n"); break; }
    v = e->m*(sweep_u-e->v[0]->x[0]) + e->v[0]->x[1]; 
    if ( prev > v+1e-5 )
    {  dump_vislist();
       printf("Bad vislist at debug_seq %d\n",debug_seq);
    }
    prev = v;
 }
}
#endif


/***************************************************************************
*
* function: activate_edge()
*
* purpose: add edge to active edge list, if it is not there.
*          Returns insertion coordinate for debugging.
*          Does linear search, so could be made more efficient.
*
*/
REAL vis_eps = 1e-13; /* for equality detection */

REAL activate_edge(e)
struct vis_conedge *e;
{ struct vis_conedge *spot;
  REAL v,vprev;
  int seq = 0;

  v = e->m*(sweep_u-e->v[0]->x[0]) + e->v[0]->x[1]; 
  if ( e->next_active ) return v; /* already active */
  PROF_START(activate_edge)

  vprev = -1e30;
  for ( spot = active_edge_first ; spot != NULL ; spot = spot->next_active )
  { REAL spotv = spot->m*(sweep_u-spot->v[0]->x[0]) + spot->v[0]->x[1];
    if ( spotv < vprev-1e-5 )
    { sprintf(errmsg,"Internal error: visibility list out of order by %f.\n",
        vprev-spotv);
      kb_error(3509,errmsg,WARNING);
    }
    vprev = spotv; /* debugging */
    spot->seqno = seq++;
    if ( spotv > v+vis_eps ) break;
    if ( (spotv > v-vis_eps) && (spot->m > e->m) ) 
       break;
  } 
  vprev = v;
  /* Have now located insertion spot; "spot" comes after e */
  if ( active_edge_first == spot ) active_edge_first = e;
  if ( spot->prev_active ) spot->prev_active->next_active = e;
  e->prev_active = spot->prev_active;
  spot->prev_active = e;
  e->next_active = spot;
  if ( e->prev_active )
  { int i;
    e->layer = (struct tsort**)temp_calloc(
       e->prev_active->layers+4, sizeof(struct tsort*));
    e->maxlayers = e->prev_active->layers+4;
    for ( i = 0 ; i < e->prev_active->layers ; i++ )
    {
      add_layer(e,e->prev_active->layer[i]);
    }
  }
  else
  { e->layer = (struct tsort**)temp_calloc(10, sizeof(struct tsort*));
    e->maxlayers = 10;
  }

  /* update sequence numbers; time waster, but we're doing linear search
     anyway. */
  for ( spot = e ; spot != NULL ; spot = spot->next_active )
  { REAL spotv = spot->m*(sweep_u-spot->v[0]->x[0]) + spot->v[0]->x[1];
    if ( spotv < vprev-1e-5 )
    { sprintf(errmsg,"Internal error: visibility list out of order by %f.\n",
        vprev-spotv);
      kb_error(2509,errmsg,WARNING);
    }
    vprev = spotv; /* debugging */
    spot->seqno = seq++;
  }

  PROF_FINISH(activate_edge)
  return v;
}

/*****************************************************************************
*
* function: check_deactivate()
*
* purpose: see if edge can be deleted from active list, since no longer
*          separating facets.
*/
void check_deactivate(e)
struct vis_conedge *e;
{
  if ( e->use_count != 0 ) return;
  PROF_START(check_deactivate)

  if ( e->prev_active )
    e->prev_active->next_active = e->next_active;
  else active_edge_first = e->next_active;
  e->next_active->prev_active = e->prev_active;
  find_next_event2(e->prev_active,e->next_active);
  e->next_active = e->prev_active = NULL;
  temp_free((char*)e->layer);
  e->layer = NULL;
  PROF_FINISH(check_deactivate)
}

/***************************************************************************
*
* function: facet_start_event()
*
* purpose: handle starts of two edges of facet.
*
* return: -1 if error, 1 if ok.
*/

int facet_start_event(fe)
struct vis_event *fe;
{ REAL v1,v2;  /* for some debugging */ 
  struct vis_conedge *spot;

  if ( visdebuglevel >= VIS_EVENTDUMP )
    printf("start edges %d %d  facet %d at %20.15f\n",
      fe->e1-vis_conedges,fe->e2-vis_conedges,
        fe->t-vis_list, (double)sweep_u);

PROF_START(facet_start_event)
  sweep_u = fe->time;
  v1 = activate_edge(fe->e1); fe->e1->use_count++;
  v2 = activate_edge(fe->e2); fe->e2->use_count++;

  if ( v2 < v1 )
  { kb_error(2505,"Internal: Visibility list insertion out of order.\n",
        WARNING);
    return -1;
  }

  if ( fe->e1->seqno > fe->e2->seqno )
  { kb_error(2508,"Internal: Visibility list insertion out of order.\n",
        WARNING);
    return -1;
  }


  /* add this facet to layers */
  for ( spot = fe->e1 ; spot != fe->e2 ; spot = spot->next_active )
  { if ( spot == NULL )
    { kb_error(2506,"Internal: Visibility list bad in facet_start_event().\n",
          WARNING);
      return -1;
    }
    add_layer(spot,fe->t);
  }
PROF_FINISH(facet_start_event)

  find_next_event(fe->e1);
  find_next_event(fe->e2);

  return 1;
} /* end facet_start_event() */

/***************************************************************************
*
* function: facet_middle_event()
*
* purpose: handle edge transition in middle of facet
*
* return: -1 for error, 1 for ok.
*/

int  facet_middle_event(fe)
struct vis_event *fe;
{ struct vis_conedge *e;

  if ( visdebuglevel >= VIS_EVENTDUMP )
     printf("middle edges %d %d  facet %d  at %20.15f\n",
        fe->e1-vis_conedges,fe->e2-vis_conedges,
           fe->t-vis_list,(double)sweep_u);

PROF_START(facet_middle_event)
  sweep_u = fe->time;
  activate_edge(fe->e2); fe->e2->use_count++;
  if ( fe->type == V_FACET_TOPMIDDLE )
  { if ( delete_layer(fe->e2,fe->t) )
    { /* second edge got put in the right way */
      for ( e = fe->e2->next_active ; e != fe->e1 ; e = e->next_active )
      { if ( e == NULL )
        { kb_error(2575,
            "Internal: Visibility list bad in facet_middle_event().\n",
              WARNING);
          return -1;
        }
        delete_layer(e,fe->t);
      }
    }
    else /* got inserted above old edge */
    { 
      for ( e = fe->e1; e != fe->e2 ; e = e->next_active ) 
      { if ( e == NULL )
        { kb_error(2507,
           "Internal: Visibility list bad in facet_middle_event(). \n",
          WARNING);
          return -1;  
        }
        add_layer(e,fe->t);
      }
      wrong_middles++;
    }
  }
  else /* bottom middle */
  { if ( add_layer(fe->e2,fe->t) )
    { /* new edge snuck in below old */
      for ( e = fe->e2->next_active ; e != fe->e1 ; e = e->next_active )
         add_layer(e,fe->t);
      wrong_middles++;
    }
    else /* new edge got in above old */
    { for ( e = fe->e1 ; e != fe->e2 ; e = e->next_active )
         delete_layer(e,fe->t);
    }
  }
  fe->e1->use_count--;
  check_deactivate(fe->e1);

PROF_FINISH(facet_middle_event)
  /* let event crossings take care of place in active list */
  find_next_event(fe->e2);

  return 1;

} /* end facet_middle_event() */

/***************************************************************************
*
* function: facet_end_event()
*
* purpose: handle deletion of active edges at end of facet
*
* return: -1 for error, 1 for ok.
*/

int facet_end_event(fe)
struct vis_event *fe;
{ struct vis_conedge *spot;

  if ( visdebuglevel >= VIS_EVENTDUMP )
    printf("end edges %d %d  facet %d  at %20.15f\n",
      fe->e1-vis_conedges,fe->e2-vis_conedges,
        fe->t-vis_list,(double)sweep_u);

  for ( spot = fe->e1 ; spot != fe->e2 ; spot = spot->next_active )
  { if ( spot == NULL )
    { struct vis_conedge *e;
      /* Oops. Something went wrong, so delete facet from entire list */
      active_list_check();
      for ( e = active_edge_first ; e != &sentinel ; e = e->next_active )
         delete_layer(e,fe->t); 
      break;
    }
    else
      delete_layer(spot,fe->t); 
  }
PROF_START(facet_end_event)
  sweep_u = fe->time;
  fe->e1->use_count--; check_deactivate(fe->e1); 
  fe->e2->use_count--; check_deactivate(fe->e2); 

PROF_FINISH(facet_end_event)
 
 return 1;

} /* end facet_end_event() */

/***************************************************************************
*
* function: vis_crossing()
*
* purpose: Handle crossing of two edges.  Have to check to be sure 
*          switching order is needed, since crossing events can be
*          listed twice.
*
*/

void vis_crossing(ea,eb)
struct vis_conedge *ea,*eb;  /* ea below eb */
{ struct vis_conedge *te;
  struct vis_rawedge *ra,*rb;
  int i;

  if ( ea->next_active != eb ) goto vis_crossing_exit; /* not adjacent */
  if ( ea != eb->prev_active )
     kb_error(2547,"Inconsistent active list.\n",RECOVERABLE);
  vis_crossing_count++;

  /* fix up layer lists, using info back in raw edge list */
  for ( i = ea->rawstart; i <= ea->rawend ; i++ )
  { ra = rawplist[i];
    if ( ra->flags & V_FACET_BOTTOM )
      delete_layer(eb,ra->t);
    if ( ra->flags & V_FACET_TOP )
      add_layer(eb,ra->t);
  }
  for ( i = eb->rawstart; i <= eb->rawend ; i++ )
  { rb = rawplist[i];
    if ( rb->flags & V_FACET_BOTTOM )
      add_layer(ea,rb->t);
    if ( rb->flags & V_FACET_TOP )
      delete_layer(ea,rb->t);
  }
PROF_START(vis_crossing)

  /* switch order in active list */
  eb->next_active->prev_active = ea;
  if ( ea->prev_active ) ea->prev_active->next_active = eb;
  else active_edge_first = eb;
  te = ea->prev_active;
  ea->prev_active = eb;
  eb->prev_active = te;
  te = eb->next_active;
  eb->next_active = ea;
  ea->next_active = te;

PROF_FINISH(vis_crossing)

  /* Test for next events */ 
  find_next_event(ea);
  find_next_event(eb);
  
vis_crossing_exit: ;

}

/* check_layers() - Brute force check of facet layers at point. */
void check_layers(e,u,v)
struct vis_conedge *e;
REAL u,v;
{ int i,j,jj,k;
  int found=0;
  REAL *x[3];
  REAL orientation,d;

  for ( i = 0 ; i < f_event_count ; i++ )
  { struct vis_event *fe = facet_events + i;
    if ( (fe->type != V_FACET_TOPMIDDLE)
          && (fe->type != V_FACET_BOTTOMMIDDLE)) continue;
    x[0] = fe->e1->v[0]->x; 
    x[1] = fe->e1->v[1]->x; 
    x[2] = fe->e2->v[1]->x; 
    orientation = (x[1][0]-x[0][0])*(x[2][1]-x[0][1])
                     - (x[1][1]-x[0][1])*(x[2][0]-x[0][0]);
    for ( j = 0 ; j < 3 ; j++ )
    { jj = (j==2) ? 0 : j+1;
      d = (x[jj][0]-x[j][0])*(v-x[j][1])
                     - (x[jj][1]-x[j][1])*(u-x[j][0]);
      if ( d*orientation < 0.0 )
          break;
    }
    if ( j < 3 ) continue;  /* not all on proper side */

    /* now check layers */
    for ( k = 0 ; k < e->layers ; k++ )
      if ( fe->t == e->layer[k] )
         { found++; break; }
    if ( k == e->layers )
    { fprintf(stderr,"Edge %d missing facet %d at (%f,%f).\n",e-vis_conedges,
         fe->t - vis_list,u,v);
    }
  } 

  /* report */
  if ( found < e->layers ) 
    fprintf(stderr,"Edge %d extra %d layers(facet %d) at (%f,%f).\n",
       e-vis_conedges,e->layers-found,e->layer[0]-vis_list,u,v);
  
   
}

/*************************************************************************
*
* function: check_visible()
*
* purpose: Inspect layers of changed edge stacks and mark topmost facets.
*          Uses facet order from painter algorithm rather than
*          calculating local z, since that is subject to problems with
*          abutting facets. 
*
*/

void check_visible(u)
REAL u;  /* sweep time to check */
{ REAL v,vv;  /* height on sweep line of edge and next */
  int i,j;
PROF_START(check_visible)

  for ( i = 0 ; i < check_list_count ; i++ )
  { struct vis_conedge *e = check_list[i];
    struct tsort *topf;

    e->flags &= ~V_LAYER_CHECK;
    if ( e->next_active == NULL ) continue;
    v  = e->m*(u-e->v[0]->x[0]) + e->v[0]->x[1];
    vv = e->next_active->m*(u-e->next_active->v[0]->x[0]) 
              + e->next_active->v[0]->x[1];
    if ( fabs(v-vv) < 1e-10 ) continue; /* ignore cracks */
    v = (v+vv)/2;
    
    if ( visdebuglevel >= VIS_LAYERCHECK )
      check_layers(e,u,v);  /* debugging */

    topf = NULL;
    for ( j = 0 ; j < e->layers ; j++ )
    { 
      struct tsort *f = e->layer[j];
      if ( f > topf )    /* using painter facet order */
      { topf = f; }
    }
    if ( topf ) 
    { topf->flag |= VISIBLE;
      if ( visdebuglevel >= VIS_EVENTDUMP )
          printf("Marking facet %d visible.\n",topf-vis_list);
    }
  }
  check_list_count = 0;

PROF_FINISH(check_visible)
}

#ifdef BRUTE
/*************************************************************************
  Brute force visibility.
**************************************************************************/


/*************************************************************************
*
* function: brutecomp
*
* purpose: comparison function for sorting brute force event times.
*/

int brutecomp (const void *a, const void *b)
{
  struct brute *aa = (struct brute *)a;
  struct brute *bb = (struct brute *)b;

  if ( aa->time < bb->time ) return -1;
  if ( aa->time > bb->time ) return 1;
  if ( aa->e1 < bb->e1 ) return -1;
  if ( aa->e1 > bb->e1 ) return 1;
  if ( aa->e2 < bb->e2 ) return -1;
  if ( aa->e2 > bb->e2 ) return 1;
  return 0;
}

/*************************************************************************
*
* function: brutecut_comp
*
* purpose: comparison function for sorting brute force cut heights.
*/

int brute_cut_comp (const void *a, const void *b)
{
  struct brute_cut *aa = (struct brute_cut *)a;
  struct brute_cut *bb = (struct brute_cut *)b;

  if ( aa->v < bb->v ) return -1;
  if ( aa->v > bb->v ) return 1;
  if ( aa->e < bb->e ) return -1;
  if ( aa->e > bb->e ) return 1;
  return 0;
}
/***************************************************************************
*
* function: brute_section
*
* purpose: Find all edge intersections at a particular sweep position
*          by brute force, and order by height.
*/

void brute_section(u)
REAL u;  /* sweep position */
{ struct brute_cut *b;
  int i;
  struct vis_edge *e;

  brute_cut_count = 0;
  for ( i = 0 ; i < vecount ; i++ )
  { e = vis_edges + i;
    if ( e->x[0][0] > u ) continue;
    if ( e->x[1][0] < u ) continue;
    b = brute_cuts + brute_cut_count++;
    b->v = e->m*(u-e->x[0][0]) + e->x[0][1];
    b->e = e;
  }
  qsort(brute_cuts,brute_cut_count,sizeof(struct brute_cut),FCAST brute_cut_comp);
}

/*************************************************************************
*
* function: brute_visible()
*
* purpose: Mark as visible those facets along sweep line, using edge
*          order found by brute_section().
*
*/

struct vis_facet *brute_column[100]; /* list of facets at spot */

void brute_visible(u)
REAL u; /* sweep line position */
{ int i,j;
  int depth = 0; /* number of facets stacked up */
  REAL v,vv,z;

  brute_section(u);
  
  for ( i = 0 ; i < brute_cut_count-1 ; i++ )
  { struct vis_edge *e = brute_cuts[i].e;
    struct vis_edge *ee = brute_cuts[i+1].e;

    if ( e->flags & V_FACET_BOTTOM )
    { /* add to column */
      brute_column[depth++] = e->f;
    } 
    else if ( e->flags & V_FACET_TOP )
    { /* delete from column */
      for ( j = 0 ; j < depth ; j++ )
       if ( brute_column[j] == e->f )
         brute_column[j] = brute_column[--depth];
    }

    /* check halfway in between for top facet */
    v = e->m*(u-e->x[0][0]) + e->x[0][1];
    vv = ee->m*(u-ee->x[0][0]) + ee->x[0][1];
    if ( (depth > 0) && (fabs(v-vv) > 1e-8) )
    { REAL topz = -1e30;
      int topj = -1;

      v = (v+vv)/2;
      for ( j = 0 ; j < depth ; j++ )
      { struct vis_facet *f = brute_column[j];
        z = f->a*u + f->b*v + f->c;
        if ( z > topz )
        { topj = j; topz = z; }
      }
      brute_column[topj]->t->flag |= VISIBLE;
    }
  }
}
#endif    

