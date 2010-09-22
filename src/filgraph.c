/*************************************************************
*  This file is part of the Surface Evolver source code.     * 
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*****************************************************************
*
*  File: filgraph.c
*
*  Purpose:  Triangle list file output.
*             Format: One triangle per line,
*                   x1 y1 x2 y2 x3 y3 w1 w2 w3 d
*             vertex coordinates, edge widths, and face density.
*
*  Also OFF format output file.
*/

#include "include.h"

static FILE *fd;

/*****************************************************************
*
*  Function: fil_init()
*
*  Purpose:  Get file name from user.
*/

void fil_init()
{
  char file_name[200];

  for (;;)
  {
     prompt("Enter file name: ",file_name,sizeof(file_name));
     if ( file_name[0] == 0 )
     { kb_error(4006,"File aborted.\n",RECOVERABLE);
     }

     fd = fopen(file_name,"w");
     if ( fd == NULL )
     { perror(file_name);
       continue;
     }
     else return;
  }

}

/************************************************************
*
*  Function: fil_edge()
*
*  Purpose: Graphs one edge, already transformed.
*/

void fil_edge(t)
struct tsort *t;
{
  fprintf(fd,"  %f %f  ",(DOUBLE)t->x[0][0],(DOUBLE)t->x[0][1]);
  fprintf(fd,"  %f %f  ",(DOUBLE)t->x[1][0],(DOUBLE)t->x[1][1]);
  fprintf(fd,"  %f %f  ",(DOUBLE)t->x[1][0],(DOUBLE)t->x[1][1]);
  fprintf(fd,"  0.03  0.03  0.03  0.0 \n");
}


/************************************************************
*
*  Function: fil_facet()
*
*  Purpose: Graphs one facet, already transformed.
*/

void fil_facet(t)
struct tsort *t;
{
  REAL cosine;
  edge_id e_id;
  facetedge_id fe_id;
  int type;
  int i;
  
  if ( t->color == CLEAR ) return;

  fprintf(fd,"%f %f",(DOUBLE)t->x[0][0],(DOUBLE)t->x[0][1]);
  fprintf(fd," %f %f",(DOUBLE)t->x[1][0],(DOUBLE)t->x[1][1]);
  fprintf(fd," %f %f",(DOUBLE)t->x[2][0],(DOUBLE)t->x[2][1]);
  fe_id = get_facet_fe(t->f_id);
  for ( i = 0 ; i < 3 ; i++ )
     { 
        e_id = get_fe_edge(fe_id);
        if ( get_eattr(e_id) & FIXED ) type = 3;
        else if ( equal_id(get_next_facet(fe_id),fe_id) )
          type = 1;  /* edge of some sort */
        else if ( !equal_id(get_next_facet(fe_id),get_prev_facet(fe_id)) )
          type = 4;  /* triple line at least */
        else type = 0;  /* ordinary internal grid line */

        fprintf(fd," %1d",type);
        fe_id = get_next_edge(fe_id);
     }
  
  if ( valid_id(t->f_id) )
     {
        cosine = t->normal[1]/sqrt(dotf(t->normal,t->normal,3));
        if ( (REAL)t->normal[2] < 0.0 ) cosine = -cosine;
     }
  else cosine = 0.0;
  fprintf(fd," %f\n",(DOUBLE)cosine);
}

/*************************************************************
*
*  Function:  fil_finish()
*
*  Purpose:    End output.
*/

void fil_finish()
{
  fclose(fd);
}

/***************************************************************************

  OFF format file.
*/

struct OFF_vertex_t { 
    REAL x[3];
    int orig;
} *OFF_verts;
int OFF_verts_alloc;

struct OFF_edge_t {
   int v[2];
   int color;
   int orig;
} *OFF_edges;
int OFF_edges_alloc;

struct OFF_facet_t {
   int v[3];
   int color;
} *OFF_facets;
int OFF_facets_alloc;

int OFF_vertex_count;
int OFF_edge_count;
int OFF_facet_count;

REAL OFF_eps = 1e-6;
int OFF_comp(a,b)
struct OFF_vertex_t *a,*b;
{ int i;
  for ( i = 0 ; i < 3; i++ )
  { if ( a->x[i] < b->x[i] - OFF_eps ) return -1;
    if ( a->x[i] > b->x[i] + OFF_eps ) return  1;
  }
  return 0;
}

int OFF_edge_comp(a,b)
struct OFF_edge_t *a,*b;
{ int i;
  for ( i = 0 ; i < EDGE_VERTS ; i++ )
  { if ( a->v[i] < b->v[i] ) return -1;
    if ( a->v[i] > b->v[i] ) return 1;
  }
  return 0;
}

/*****************************************************************
*
*  Function: OFF_start()
*
*  Purpose:  Get file name from user.
*/

void OFF_start()
{
  char file_name[200];

  for (;;)
  {
    prompt("Enter file name: ",file_name,sizeof(file_name));
    if ( file_name[0] == 0 )
    { kb_error(4005,"File aborted.\n",RECOVERABLE);
    }

    fd = fopen(file_name,"w");
    if ( fd == NULL )
    { perror(file_name);
      kb_error(1034,"",RECOVERABLE);
    }
    else break;
  }
  OFF_vertex_count = 0;
  OFF_edge_count = 0;
  OFF_facet_count = 0;

  OFF_verts_alloc = 2*web.skel[EDGE].count + 3*web.skel[FACET].count;
  OFF_verts = (struct OFF_vertex_t *)temp_calloc(OFF_verts_alloc,
                  sizeof(struct OFF_vertex_t));
  OFF_facets_alloc = 3*web.skel[FACET].count;
  OFF_facets = (struct OFF_facet_t *)temp_calloc(OFF_facets_alloc,
                  sizeof(struct OFF_facet_t));
}

/************************************************************
*
*  Function: OFF_edge()
*
*  Purpose: Graphs one edge, already transformed.
*/

void OFF_edge(g,e_id)
struct graphdata *g;
edge_id e_id;
{
 
  int e_color;

  e_color = g[0].ecolor;
  if ( e_color == CLEAR ) return;
  if ( (e_color < 0) || (e_color >= IRIS_COLOR_MAX) )
    e_color = DEFAULT_EDGE_COLOR;
}

/******************************************************************
*
* function: OFF_facet()
*
* purpose:  graph one facet.
*/

void OFF_facet(g,f_id)
struct graphdata *g;
facet_id f_id;
{
  int i,k;

  if ( OFF_vertex_count > OFF_verts_alloc - 5 )
  { OFF_verts = (struct OFF_vertex_t*)temp_realloc((char*)OFF_verts,
        2*OFF_verts_alloc*sizeof(struct OFF_vertex_t));
    OFF_verts_alloc *= 2;
  }
  if ( OFF_facet_count > OFF_facets_alloc - 5 )
  { OFF_facets = (struct OFF_facet_t*)temp_realloc((char*)OFF_facets,
        2*OFF_facets_alloc*sizeof(struct OFF_facet_t));
    OFF_facets_alloc *= 2;
  }


  for ( i = 0 ; i < FACET_VERTS ; i++ )
  { for ( k = 0 ; k < 3 ; k++ )
      OFF_verts[OFF_vertex_count].x[k] = g[i].x[k];
    OFF_facets[OFF_facet_count].v[i] = OFF_vertex_count;
    OFF_vertex_count++;
  }
  OFF_facet_count++;
}



/*************************************************************
*
*  Function:  OFF_end()
*
*  Purpose:    End output.
*/

void OFF_end()
{ int i,j,keep;
  int *translate;

  /* unify vertices */
  for ( i = 0 ; i < OFF_vertex_count ; i++ )
    OFF_verts[i].orig = i;
  qsort((char*)OFF_verts,OFF_vertex_count,sizeof(struct OFF_vertex_t), 
      FCAST OFF_comp);
  translate = (int*)temp_calloc(OFF_vertex_count,sizeof(int));
  translate[OFF_verts[0].orig] = 0;
  for ( i = 1, keep = 0 ; i < OFF_vertex_count ; i++ )
  { if ( OFF_comp(OFF_verts+i,OFF_verts+keep) != 0 )
      OFF_verts[++keep] = OFF_verts[i];
    translate[OFF_verts[i].orig] = keep;
  }
  OFF_vertex_count = keep+1;
  for ( i = 0 ; i < OFF_facet_count ; i++ )
  { for ( j = 0 ; j < 3 ; j++ )
      OFF_facets[i].v[j] = translate[OFF_facets[i].v[j]];
    /* test for degenerate facet */
    if ( OFF_facets[i].v[0] == OFF_facets[i].v[1] ||
         OFF_facets[i].v[1] == OFF_facets[i].v[2] ||
         OFF_facets[i].v[2] == OFF_facets[i].v[0] )
    { /* found degenerate, so replace with last in list */
      OFF_facets[i] = OFF_facets[--OFF_facet_count];
      i--;
    }
  }

  fprintf(fd,"OFF\n%d %d %d\n",OFF_vertex_count,OFF_facet_count,OFF_edge_count);
  for ( i = 0 ; i < OFF_vertex_count ; i++ )
    fprintf(fd,"%f %f %f\n",OFF_verts[i].x[0],OFF_verts[i].x[1],
        OFF_verts[i].x[2]);
  for ( i = 0 ; i < OFF_facet_count ; i++ )
    fprintf(fd,"3 %d %d %d\n",OFF_facets[i].v[0],OFF_facets[i].v[1],
        OFF_facets[i].v[2]);
  fclose(fd);
  temp_free((char*)OFF_verts);
  temp_free((char*)OFF_facets);
  temp_free((char*)translate);
}


/***************************************************************************

  Binary OFF format file for evmovie.

*/


/*****************************************************************
*
*  Function: binary_OFF_start()
*
*  Purpose:  Get file name from user.
*/
char *binary_off_filename; /* set by binary_off_file command */

void binary_OFF_start()
{
 
  OFF_vertex_count = 0;
  OFF_edge_count = 0;
  OFF_facet_count = 0;

  OFF_verts_alloc = 2*web.skel[EDGE].count + 3*web.skel[FACET].count + 10;
  OFF_verts = (struct OFF_vertex_t *)temp_calloc(OFF_verts_alloc,
                  sizeof(struct OFF_vertex_t));
  OFF_edges_alloc = 2*web.skel[EDGE].count + 10;
  OFF_edges = (struct OFF_edge_t *)temp_calloc(OFF_edges_alloc,
                  sizeof(struct OFF_edge_t));
  OFF_facets_alloc = web.skel[FACET].count + 10;
  OFF_facets = (struct OFF_facet_t *)temp_calloc(OFF_facets_alloc,
                  sizeof(struct OFF_facet_t));
}

/************************************************************
*
*  Function: binary_OFF_edge()
*
*  Purpose: Graphs one edge, already transformed.
*/

void binary_OFF_edge(g,e_id)
struct graphdata *g;
edge_id e_id;
{
  int i,k;
  int e_color;

  if ( OFF_vertex_count > OFF_verts_alloc - 5 )
  { OFF_verts = (struct OFF_vertex_t*)temp_realloc((char*)OFF_verts,
        2*OFF_verts_alloc*sizeof(struct OFF_vertex_t));
    OFF_verts_alloc *= 2;
  }
  if ( OFF_edge_count > OFF_edges_alloc - 5 )
  { OFF_edges = (struct OFF_edge_t*)temp_realloc((char*)OFF_edges,
        2*OFF_edges_alloc*sizeof(struct OFF_edge_t));
    OFF_edges_alloc *= 2;
  }

  e_color = g[0].ecolor;
  if ( e_color == CLEAR ) return;
  if ( (e_color < 0) || (e_color >= IRIS_COLOR_MAX) )
    e_color = DEFAULT_EDGE_COLOR;

  for ( i = 0 ; i < EDGE_VERTS ; i++ )
  { for ( k = 0 ; k < 3 ; k++ )
      OFF_verts[OFF_vertex_count].x[k] = g[i].x[k];
    OFF_edges[OFF_edge_count].v[i] = OFF_vertex_count;
    OFF_vertex_count++;
  }
  OFF_edges[OFF_facet_count].color = e_color;
  OFF_edge_count++;
}

/******************************************************************
*
* function: binary_OFF_facet()
*
* purpose:  graph one facet.
*/

void binary_OFF_facet(gdata,f_id)
struct graphdata *gdata;
facet_id f_id;
{
  int i,k;
  struct graphdata ggdata[2];
 
  if ( OFF_vertex_count > OFF_verts_alloc - 5 )
  { OFF_verts = (struct OFF_vertex_t*)temp_realloc((char*)OFF_verts,
        2*OFF_verts_alloc*sizeof(struct OFF_vertex_t));
    OFF_verts_alloc *= 2;
  }
  if ( OFF_facet_count > OFF_facets_alloc - 5 )
  { OFF_facets = (struct OFF_facet_t*)temp_realloc((char*)OFF_facets,
        2*OFF_facets_alloc*sizeof(struct OFF_facet_t));
    OFF_facets_alloc *= 2;
  }


  for ( i = 0 ; i < FACET_VERTS ; i++ )
  { for ( k = 0 ; k < 3 ; k++ )
      OFF_verts[OFF_vertex_count].x[k] = gdata[i].x[k];
    OFF_facets[OFF_facet_count].v[i] = OFF_vertex_count;
    OFF_vertex_count++;
  }
  OFF_facets[OFF_facet_count].color = gdata[0].color;
  OFF_facet_count++;

  /* do edges */
  for ( i = 0 ; i < FACET_EDGES ; i++ )
  { if ( ((gdata[i].etype&EBITS)|REGULAR_EDGE) == REGULAR_EDGE ) continue;
    ggdata[0] = gdata[i];
    ggdata[0].color = gdata[i].ecolor;
    ggdata[1] = gdata[i==2 ? 0 : i+1];
    binary_OFF_edge(ggdata,NULLID);
  }
}

/**************************************************************************
*
*  Function:  binary_OFF_end()
*
*  Purpose:  Sort and merge vertices and edges; write output file 
*/

void binary_OFF_end()
{ int i,j,k,keep;
  int *translate;

  /* unify vertices */
  for ( i = 0 ; i < OFF_vertex_count ; i++ )
    OFF_verts[i].orig = i;
  qsort((char*)OFF_verts,OFF_vertex_count,sizeof(struct OFF_vertex_t), 
      FCAST OFF_comp);
  translate = (int*)temp_calloc(OFF_vertex_count,sizeof(int));
  translate[OFF_verts[0].orig] = 0;
  for ( i = 1, keep = 0 ; i < OFF_vertex_count ; i++ )
  { if ( OFF_comp(OFF_verts+i,OFF_verts+keep) != 0 )
      OFF_verts[++keep] = OFF_verts[i];
    translate[OFF_verts[i].orig] = keep;
  }
  OFF_vertex_count = keep+1;

  /* renumber edge and facet vertices */
  for ( i = 0 ; i < OFF_edge_count ; i++ )
  { for ( j = 0 ; j < EDGE_VERTS ; j++ )
      OFF_edges[i].v[j] = translate[OFF_edges[i].v[j]];
    /* get vertices in canonical order */
    if ( OFF_edges[i].v[0] > OFF_edges[i].v[1] )
    { int tmp = OFF_edges[i].v[0];
      OFF_edges[i].v[0] = OFF_edges[i].v[1];
      OFF_edges[i].v[1] = tmp;
    }
  }
  for ( i = 0 ; i < OFF_facet_count ; i++ )
  { for ( j = 0 ; j < FACET_VERTS ; j++ )
      OFF_facets[i].v[j] = translate[OFF_facets[i].v[j]];
  }

  /* unify edges */
  /* sort edges */
  for ( i = 0 ; i < OFF_edge_count ; i++ )
    OFF_edges[i].orig = i;
  qsort((char*)OFF_edges,OFF_edge_count,sizeof(struct OFF_edge_t), 
      FCAST OFF_edge_comp);
  translate = (int*)temp_realloc((char*)translate,OFF_edge_count*sizeof(int));
  for ( i = 1, keep = 0 ; i < OFF_edge_count ; i++ )
  { if ( OFF_edge_comp(OFF_edges+i,OFF_edges+keep) != 0 )
      OFF_edges[++keep] = OFF_edges[i];
  }
  OFF_edge_count = keep+1;

 

  if ( binary_off_filename == NULL ) /* prompt for file name */
  for (;;)
  { char file_name[200];
    prompt("Enter file name (supply your own extension): ",file_name,sizeof(file_name));
    if ( file_name[0] == 0 )
    { kb_error(4005,"File aborted.\n",RECOVERABLE);
    }

    fd = fopen(file_name,"wb");
    if ( fd == NULL )
    { perror(file_name);
      kb_error(1034,"",RECOVERABLE);
    }
    else break;
  }
  else /* use supplied name */
  {
    fd = fopen(binary_off_filename,"wb");
    if ( fd == NULL )
    { perror(binary_off_filename);
      kb_error(1034,"",RECOVERABLE);
    }
    binary_off_filename = NULL; /* for next time around */
  }
  
  #ifdef ASCII_OFF
  /* ASCII version, with color indexes for edges and facets */
  fprintf(fd,"COFF\n%d %d %d\n",OFF_vertex_count,OFF_facet_count,OFF_edge_count);
  for ( i = 0 ; i < OFF_vertex_count ; i++ )
    fprintf(fd,"%f %f %f\n",OFF_verts[i].x[0],OFF_verts[i].x[1],
        OFF_verts[i].x[2]);
  for ( i = 0 ; i < OFF_facet_count ; i++ )
    fprintf(fd,"3 %d %d %d   %d\n",OFF_facets[i].v[0],OFF_facets[i].v[1],
        OFF_facets[i].v[2],OFF_facets[i].color);
  for ( i = 0 ; i < OFF_edge_count ; i++ )
    fprintf(fd,"2 %d %d   %d\n",OFF_edges[i].v[0],OFF_edges[i].v[1],
        OFF_edges[i].color);
  #else
  
  /* Binary version */
  fprintf(fd,"OFF BINARY (by Surface Evolver, for evmovie)\n");
  fwrite(&OFF_vertex_count,sizeof(int),1,fd);
  fwrite(&OFF_facet_count,sizeof(int),1,fd);
  fwrite(&OFF_edge_count,sizeof(int),1,fd);
  for ( i = 0 ; i < OFF_vertex_count ; i++ )
  { float xx[3];
    for ( k = 0 ; k < 3 ; k++ )
      xx[k] = (float)(OFF_verts[i].x[k]);
    fwrite(xx,sizeof(float),3,fd);
  }
  for ( i = 0 ; i < OFF_facet_count ; i++ )
  { int data[5];
    data[0] = 3;
    for ( k = 0 ; k < 3 ; k++ )
      data[k+1] = OFF_facets[i].v[k];
    data[4] = OFF_facets[i].color;
    fwrite(data,sizeof(int),5,fd);
  }
  for ( i = 0 ; i < OFF_edge_count ; i++ )
  { int data[4];
    data[0] = 2;
    for ( k = 0 ; k < 2 ; k++ )
      data[k+1] = OFF_edges[i].v[k];
    data[3] = OFF_edges[i].color;
    fwrite(data,sizeof(int),4,fd);
  }
 
  #endif
  
  fclose(fd);
  temp_free((char*)OFF_verts);
  temp_free((char*)OFF_edges);
  temp_free((char*)OFF_facets);
  temp_free((char*)translate);
}

