/********************************************************************
*
*  File:  glutgraph.c
*
*  Contents:  OpenGL/GLUT display for Surface Evolver
*             For OpenGL Version 1.1
*             (modified from oglgraph.c, which used aux)
*/

/* This runs in a separate thread from the main program, so it avoids
   calling kb_error, since that would longjump to the wrong place.
   Except for WARNINGS, which are OK since kb_error() just returns.
   */

/* Some timings in various OpenGL drawing modes:
   (done with cat.fe and  gtime.cmd)(list, arrays exclude setup time)
   (done in 'u' mode for 10 drawings; time for one reported here)
   (flat shading for no arrays; doesn't matter for others)

facets   edges    no arrays  display list  arrays   indexed arrays   strips  
  6144    9132      0.2224       0.1500    0.1021      0.0691        0.0420
 24576   37056      0.8552       0.5740    0.3846      0.2544        0.1292
 98304  147840      3.4059                 1.4932      1.0154        0.4888
 98304       0      1.6513       0.6630    0.5208      0.5348        0.2223
393216       0      7.8273       3.2527    2.1140      2.1827        0.8720

To get a sense of the set-up times, here are recalc times,
flat shading except for strips.
facets   edges    no arrays  display list  arrays   indexed arrays   strips  
  6144    9132      0.32         1.2319    0.3796      0.5558        0.9143
 24576   37056      0.9164      11.1961    1.0064      1.6804        2.9993
 98304  147840      3.6503                 4.2781      6.9260       12.5481

On transforms, using srol.8.fe
Transforms   facets  edges    arrays   indexed arrays   strips
    24       196608  297216   3.3058        2.2122      1.2307
A little slower than one would expect, maybe due to normal flipping??
Or memory caching?  But at least set-up is fast.
*/


#undef DOUBLE 


/* so can use glutPostWindowRedisplay() */
#define GLUT_API_VERSION 4

#if defined(WIN32) || defined(_WIN32)

#undef FIXED
#include <process.h>   
#include <glut.h>

#else

#define __cdecl

#if defined(MAC_OS_X)
#include <glut.h>
#else
#include <GL/glut.h>

#endif

#endif

#include "include.h"

#ifdef _DEBUG
#define GL_ERROR_CHECK \
{ int err = glGetError(); \
  if ( err ) \
    fprintf(stderr,\
     "OpenGL error %08X window %d at line %d\n",err,glutGetWindow(),\
         __LINE__);\
}
#else
#define GL_ERROR_CHECK
#endif

struct stripstruct { int mode;  /* GL_TRIANGLE_STRIP or GL_LINE_STRIP */
                     int start; /* starting offset in indexarray */
                     int count; /* number of vertices in strip */
                   };
                   
/* projmode values */
#define P_ORTHO 0
#define P_PERSP 1

/* stereomode values */
#define NO_STEREO 0
#define CROSS_STEREO 1

#define NOLIST     0
#define NORMALLIST 1
#define RESETLIST  2

/* Multiple graphing windows stuff */
int graph_thread_running;  /* whether graph thread is running */
int draw_pid; /* draw process id, for signalling from main */
int main_pid;  /* to see if same as main thread */
#ifdef PTHREADS
sigset_t newset;   /* mask SIGKICK from main thread */
#define SIGKICK SIGALRM
#endif
int dup_window;  /* window for new window to duplicate */
#define GET_DATA (gthread_data + glutGetWindow())
#define MAXGRAPHWINDOWS 10
#define WINTITLESIZE 120
struct thread_data glutgraph_thread_data; /* for eval() */
struct graph_thread_data {
   int in_use;
   int new_title_flag;
   char wintitle[WINTITLESIZE];
   int newarraysflag; /* to cause rebuild of arrays */
   int arrays_timestamp; /* last arrays computed in this thread */
   int declared_timestamp; /* last arrays declared in this thread */
   long win_id;  /* GLUT graphics window id */
#ifdef WIN32
   HWND draw_hwnd;  /* handle to graphics window */
#endif
   int olddim;
   double scrx[4],scry[4];  /* screen corners */
   double aspect;  /* aspect ratio */
   REAL window_aspect; /* set by user */
   double xscale,yscale;  /* scaling factors */
   GLsizei xsize,ysize; /* window size */
   int oldx,oldy,newx,newy;  /* for tracking motion */
   int basex,basey;  /* translation to do on object */
   REAL *view[MAXCOORD+1];  /* this window's view matrix */
   REAL viewspace[MAXCOORD+1][MAXCOORD+1];
   int view_initialized;
   vertex_id focus_vertex_id;    /* rotate and zoom focus */
   REAL *to_focus[MAXCOORD+1];
   REAL to_focus_space[MAXCOORD+1][MAXCOORD+1];
   REAL *from_focus[MAXCOORD+1];
   REAL from_focus_space[MAXCOORD+1][MAXCOORD+1];
   float kb_norm[3]; /* state of normal vector */
   int projmode;    /* kind of projection to do */
   float projmat[16];  /* for saving projection matrix */
   int stereomode;
   int facetshow_flag; /* whether to show facets */
   int normflag;  /* whether normals should be calculuated by graphgen() */
   REAL linewidth;
   REAL edge_bias; /* amount edges drawn in front */
   int mouse_mode;
   int mouse_left_state; /* state of left mouse button */
   int idle_flag; /* Mac OS X kludge for eliminating excessive
                         glutPostRedisplays, set by idle_func and cleared by 
                         draw_screen */
   int resize_flag; /* whether redraw event caused by resize.
              To get around Mac OS X problem of not combining events. */
                /* not used; didn't work as expected */

   int aspect_flag; /* whether pending reshape due to aspect fixing */

   int dlistflag;  /* whether to use display list */
   int arraysflag; /* whether to use OpenGL 1.1 arrays */
   struct vercol *fullarray;
   int fullarray_original;  /* if this thread allocated fullarray */
   float *colorarray;
   int edgestart,edgecount,facetstart,facetcount;
   int vertexcount;
   struct vercol *edgearray,*facetarray;
   int edgemax;
   int facetmax; /* allocated */
   int interleaved_flag; /* whether to do arrays as interleaved */
   int indexing_flag; /* whether to use indexed arrays (smaller,but random access) */
   int *indexarray;
   int strips_flag; /* whether to do GL strips */
   int strip_color_flag; /* whether to color facets according to strip number */
   struct stripstruct *striparray;
   int stripcount;  /* size of striparray */
   int estripcount; /* number of edge strips */
   int fstripcount; /* number of facet strips */
   int *stripdata;
   int doing_lazy;  /* whether glutgraph should do transforms itself */
   int q_flag;  /* whether to print drawing stats */
   int mpi_graph_task;  /* for MPI Evolver */
 
 } gthread_data[MAXGRAPHWINDOWS];

/* end multiple graphing windows stuff */

struct vercol { float c[4]; float n[3]; float x[3]; int inx; };
static  int mainmenu, submenu;  /* menu identifiers */
static  int mpi_taskmenu;  /* for task-picking menu */
static char opengl_version[20]; /* from glGetString */
int close_flag = 0; /* whether close_show has been done */
void Ogl_close ARGS((void));
void Ogl_close_show ARGS((void));
void idle_func ARGS((void));
void make_strips(void);
void make_indexlists(void);
void set_title ARGS((struct graph_thread_data*));
#ifdef PTHREADS
void * draw_thread ARGS((void *));
#else
void __cdecl draw_thread ARGS((void *));
#endif
static int glutInit_called; /* so don't call again if close and reopen */
static REAL gleps = 1e-5; /* tolerance for identifying vertices */
static REAL imagescale = 1.0;  /* scaling factor for image */
static float rgba[16][4]; 
void declare_arrays ARGS((void));
void draw_one_image ARGS((void));
void enlarge_edge_array ARGS((struct graph_thread_data*));
void enlarge_facet_array ARGS((struct graph_thread_data*));
void pick_func ARGS((int,int));
element_id name_to_id ARGS((GLuint));
void myMenuInit ARGS((void));
void my_glLoadName ARGS(( element_id ));
void e_glColor ARGS(( struct graph_thread_data *,int ));
void f_glColor ARGS(( struct graph_thread_data *,int ));
void e_glVertex3dv ARGS(( struct graph_thread_data *,REAL * ));
void f_glVertex3dv ARGS(( struct graph_thread_data *,REAL * ));
int hashfunc ARGS (( struct vercol *));
int vercolcomp ARGS(( struct vercol *, struct vercol *));
int eecomp ARGS(( int *, int *));
int build_arrays ARGS((void));
void mpi_get_task_graphics ARGS((int));
static int initz_flag = 0;  /* z buffer */
int no_graphthread_flag = 0; /* kludge for mpi task graphics */

/*****************************************************************************
*
* function glut_text_display()
*
* purpose: display text lines in graphics window.
*/

void glut_text_display()
{ char *c;
  int i;
  struct graph_thread_data *td = GET_DATA;
 
  /* set up window coords */
  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glLoadIdentity();
  glOrtho(0.0,1.0,0.0,1.0,0.0,1.0);
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();
  
  for ( i = 0 ; i < MAXTEXTS ; i++ )
  { REAL yspot;
    if ( text_chunks[i].text == NULL )
      continue;
    yspot = text_chunks[i].start_y;
    glColor3f(0.0,0.0,0.0); /* black */
    glRasterPos3d(text_chunks[i].start_x,text_chunks[i].start_y,0.0);
    for ( c = text_chunks[i].text ; *c ; c++ )
    { if ( *c == '\n' )
      { yspot -= 18.0/td->ysize;
        glRasterPos3d(text_chunks[i].start_x,yspot,0.0);
      }
      glutBitmapCharacter(GLUT_BITMAP_HELVETICA_18,*c);
    }
  }
 
  glPopMatrix();  
  glMatrixMode(GL_PROJECTION);
  glPopMatrix();
 
}

/*****************************************************************************
*
* function: my_glLoadName()
*
* purpose: translate element id into GLuint name.
*          Needful if element_id is longer than GLuint.
*          Format: high 2 bits set for type of element.
*                  then task id bits, for MPI
*                  then element number.
*/
#define NAMETASKBITS 8
#define NAMEOFFSETBITS (8*sizeof(GLuint) - NAMETASKBITS - 2)
#define NAMEOFFSETMASK  (((GLuint)1 << NAMEOFFSETBITS) - 1 )
#define NAMETYPESHIFT  (8*sizeof(GLuint)-2)
#define NAMETYPE_MASK  (3 << NAMETYPESHIFT)
#define NAMETASKMASK (((1 << NAMETASKBITS) - 1) << NAMEOFFSETBITS)

void my_glLoadName ARGS1((id),
element_id id)
{ GLuint name;
  name = id_type(id) << NAMETYPESHIFT;
#ifdef MPI_EVOLVER
  name |= id_task(id) << NAMEOFFSETBITS;
#endif
  if ( valid_id(id) )
    name |= id & OFFSETMASK & NAMEOFFSETMASK;
  else 
    name |= NAMEOFFSETMASK;
  glLoadName(name);

}

/***************************************************************************
*
* function: name_to_id()
*
* purpose: unravel picked name to element id.
*/
element_id name_to_id ARGS1((name),
GLuint name)
{ element_id id;
  id = (element_id)((name & NAMETYPE_MASK) >> NAMETYPESHIFT) << TYPESHIFT;
  if ( (name & NAMEOFFSETMASK) != NAMEOFFSETMASK )
     id |=   VALIDMASK | (name & NAMEOFFSETMASK);
#ifdef MPI_EVOLVER
  id |= (element_id)((name & NAMETASKMASK) >> NAMEOFFSETBITS) << TASK_ID_SHIFT;
#endif
  return id;
}

/********************************************************************
*
* function: enlarge_edge_array()
*
* purpose: Expand the space for the edge list
*/

void enlarge_edge_array(td)
struct graph_thread_data *td;
{ int more = td->edgemax + 10;

  td->edgearray = (struct vercol*)realloc((char*)td->edgearray,
                (td->edgemax+more)*sizeof(struct vercol));
  td->edgemax += more;
}

/********************************************************************
*
* function: enlarge_facet_array()
*
* purpose: Expand the space for the facet list
*/

void enlarge_facet_array(td)
struct graph_thread_data *td;
{ int more = 3*web.skel[FACET].count + 10;

  td->facetarray = (struct vercol*)realloc((char*)td->facetarray,
                     (td->facetmax+more)*sizeof(struct vercol));
  td->facetmax += more;
}

/***********************************************************************
*
* function: kb_glNormal3fv() etc.
*
* purpose: Either save current normal in kb_norm[] for later list entry,
*          or pass it on to gl.
*/


void kb_glNormal3fv(struct graph_thread_data *td,float *v) /* save for edges and facets */
{ if ( !td->arraysflag ) glNormal3fv(v);
  else { td->kb_norm[0] = v[0]; td->kb_norm[1] = v[1]; td->kb_norm[2] = v[2]; }
}
void kb_glNormal3dv(struct graph_thread_data *td,REAL *v) /* save for edges and facets */
{ td->kb_norm[0] = (float)v[0]; td->kb_norm[1] = (float)v[1]; td->kb_norm[2] = (float)v[2]; 
  if ( !td->arraysflag ) glNormal3fv(td->kb_norm);
}
void kb_glAntiNormal3dv(struct graph_thread_data *td,REAL *v) /* save for edges and facets */
{ td->kb_norm[0] = -(float)v[0]; td->kb_norm[1] = -(float)v[1]; td->kb_norm[2] = -(float)v[2]; 
  if ( !td->arraysflag ) glNormal3fv(td->kb_norm);
}

/***********************************************************************
*
* function: e_glColor()
*
* purpose: Either save current edge color in er,eg,eb for later list entry,
*          or pass it on to gl.
*/
static float er,eg,eb,ea; /* current edge color */
void e_glColor ARGS2((td,c),struct graph_thread_data *td,
int c)
{ 
  if ( edge_rgb_color_attr > 0 )
  {
    if ( !td->arraysflag ) 
      glColor4ubv((const GLubyte*)&c);
    else 
    { er=(float)(((c>>24)&0xFF)/255.0); eg=(float)(((c>>16)&0xFF)/255.0); 
      eb=(float)(((c>>8)&0xFF)/255.0); ea= (float)(((c)&0xFF)/255.0); 
    }
  }
  else 
  { if ( !td->arraysflag ) 
      glColor4fv(rgba[c]);
    else 
    { er = rgba[c][0]; eg = rgba[c][1]; eb = rgba[c][2]; ea = rgba[c][3]; }
  }
}

/*********************************************************************
*
* function: e_glVertex3dv()
*
* purpose: Either save current edge data in edge list, or pass on to gl.
*/
void e_glVertex3dv ARGS2((td,x),struct graph_thread_data *td,
REAL *x)
{ if ( !td->arraysflag ) 
  { glVertex3d((GLdouble)x[0],(GLdouble)x[1],(GLdouble)x[2]); return; }
  if ( !td->edgearray ) 
     td->edgemax = 0;
  if ( td->edgecount > td->edgemax-5 ) 
     enlarge_edge_array(td);
  td->edgearray[td->edgecount].c[0] = er;
  td->edgearray[td->edgecount].c[1] = eg;
  td->edgearray[td->edgecount].c[2] = eb;
  td->edgearray[td->edgecount].c[3] = ea;
  td->edgearray[td->edgecount].x[0] = (fabs(x[0]) < 30000) ? (float)x[0] : 0.0;
  td->edgearray[td->edgecount].x[1] = (fabs(x[1]) < 30000) ? (float)x[1] : 0.0;
  td->edgearray[td->edgecount].x[2] = (fabs(x[2]) < 30000) ? (float)x[2] : 0.0;
  td->edgearray[td->edgecount].n[0] = td->kb_norm[0];
  td->edgearray[td->edgecount].n[1] = td->kb_norm[1];
  td->edgearray[td->edgecount].n[2] = td->kb_norm[2];
  td->edgecount++;
}
/***********************************************************************
*
* function: f_glColor()
*
* purpose: Either save current facet color in fr,fg,fb for later list entry,
*          or pass it on to gl.
*/
static float fr,fg,fb,fa; /* current facet color */
void f_glColor ARGS2((td,c),struct graph_thread_data *td,
int c)
{ 
  if ( facet_rgb_color_attr > 0 )
  {
    if ( !td->arraysflag ) 
       glColor4ubv((const GLubyte*)&c);
    else 
    { fr=(float)(((c>>24)&0xFF)/255.); fg=(float)(((c>>16)&0xFF)/255.);
      fb=(float)(((c>>8)&0xFF)/255.); fa=(float)(((c)&0xFF)/255.); 
    }
  }
  else 
  { if ( !td->arraysflag ) 
      glColor4fv(rgba[c]);
    else 
    { fr = rgba[c][0]; fg = rgba[c][1]; fb = rgba[c][2]; fa = rgba[c][3]; }
  }
 
}
/*********************************************************************
*
* function: f_glVertex3dv()
*
* purpose: Either save current facet data in facet list, or pass on to gl.
*/
void f_glVertex3dv ARGS2((td,x),struct graph_thread_data *td,
REAL *x)
{ if ( !td->arraysflag )
  { glVertex3f((float)x[0],(float)x[1],(float)x[2]); return; }
  if ( !td->facetarray ) 
    td->facetmax = 0;
  if ( td->facetcount > td->facetmax-5 ) 
    enlarge_facet_array(td);
  td->facetarray[td->facetcount].c[0] = fr;
  td->facetarray[td->facetcount].c[1] = fg;
  td->facetarray[td->facetcount].c[2] = fb;
  td->facetarray[td->facetcount].c[3] = fa;
  td->facetarray[td->facetcount].x[0] = (fabs(x[0]) < 30000) ? (float)x[0] : 0;
  td->facetarray[td->facetcount].x[1] = (fabs(x[1]) < 30000) ? (float)x[1] : 0;
  td->facetarray[td->facetcount].x[2] = (fabs(x[2]) < 30000) ? (float)x[2] : 0;
  td->facetarray[td->facetcount].n[0] = td->kb_norm[0];
  td->facetarray[td->facetcount].n[1] = td->kb_norm[1];
  td->facetarray[td->facetcount].n[2] = td->kb_norm[2];
  td->facetcount++;
}

/* gl matrices have vector on left! */
typedef double Matrix[4][4];
Matrix vt3 =  /* gl transform matrix */
  { {0.,0.,1.,0.},
    {1.,0.,0.,0.},
    {0.,-1.,0.,0.},
    {0.,0.,0.,1.} };
Matrix vt3p =  /* gl transform matrix, perspective version */
  { {0.,0.,1.,0.},
    {1.,0.,0.,0.},
    {0.,1.,0.,0.},
    {0.,0.,0.,1.} };

Matrix vt2 =  /* gl transform matrix */
  { {2.,0.,0.,0.},            /* can't figure why I need the 2's */
    {0.,-2.,0.,0.},
    {0.,0.,2.,0.},
    {0.,0.,0.,1.} };

Matrix flip =  /* gl transform matrix */
  { {1.,0.,0.,0.},
    {0.,1.,0.,0.},
    {0.,0.,-1.,0.},
    {0.,0.,0.,1.} };

Matrix flip2D =  /* gl transform matrix */
  { {1.,0.,0.,0.},
    {0.,-1.,0.,0.},
    {0.,0.,1.,0.},
    {0.,0.,0.,1.} };

static long prev_timestamp; /* for remembering surface version */

#define MM_ROTATE       1
#define MM_TRANSLATE   2
#define MM_SCALE       3
#define MM_SPIN        4
#define MM_SLICE       5
#define MM_SLICE_SPIN  6
static int dindex = 1;  /* display list object index */

void draw_screen ARGS((void));


void Ogl_init ARGS((void))
{ 
}

void Ogl_finish ARGS((void))
{                
}

void graph_new_surface()
{ int k;
  struct graph_thread_data *td;
 
  ENTER_GRAPH_MUTEX; 

  /* to account for global deallocation at start of new surface */
  /* but we've given that up due to synchronization problems */
  /*
  td->fullarray = NULL;
  td->colorarray = NULL;
  td->edgearray = NULL;
  td->facetarray = NULL;
  td->indexarray = NULL;
  td->striparray = NULL;
  td->stripdata = NULL;
  */
  for ( k = 0, td = gthread_data ; k < MAXGRAPHWINDOWS ; k++,td++ )
  { int win_id = td->win_id;
    td->interleaved_flag = 1; /* whether to do arrays as interleaved */
    td->indexing_flag = 1; /* whether to use indexed arrays (smaller,but random access) */
    td->kb_norm[3] = 1.0;
    td->win_id = win_id;
    td->view_initialized = 0;
    td->mouse_mode = MM_ROTATE; 
  	td->mpi_graph_task = 1;
    set_title(td);
    if ( td->to_focus[0] )
    {
        matcopy(td->to_focus,identmat,HOMDIM,HOMDIM);
        matcopy(td->from_focus,identmat,HOMDIM,HOMDIM);
    }
  }
  LEAVE_GRAPH_MUTEX; 
}

/***********************************************************************************
*
* Function: set_title()
*
* Purpose: Set title of graphics window.
*/
void set_title(td)
struct graph_thread_data *td;
{ int titlespot = (strlen(datafilename) > 60) ? (strlen(datafilename)-60):0;
  int k = td - gthread_data;

#ifdef MPI_EVOLVER

    if ( this_task == MASTER_TASK )
    {
      if ( k == 1 )
        sprintf(td->wintitle," %1.*s (task %d from task %d)  ",
           WINTITLESIZE-30, datafilename+titlespot,
            this_task,td->mpi_graph_task
           );
      else  sprintf(td->wintitle," %1.*s (task %d from task %d) - Camera %d",
         WINTITLESIZE-45,datafilename+titlespot,this_task,td->mpi_graph_task,k);
    }
    else
    {
      if ( k == 1 )
        sprintf(td->wintitle,"%1.*s (task %d)  ",WINTITLESIZE-20,
          datafilename+titlespot,this_task);
      else  sprintf(td->wintitle," %1.*s (task %d) - Camera %d",
          WINTITLESIZE-30, datafilename+titlespot,this_task,k);
    }
#else
    if ( k == 1 )
      sprintf(td->wintitle," %1.*s",WINTITLESIZE-10,datafilename+titlespot);
    else  sprintf(td->wintitle,"  %1.*s - Camera %d",WINTITLESIZE-20,
      datafilename+titlespot,k);
#endif
    td->new_title_flag = 1;
  
} /* end set_title() */

void init_Oglz ARGS((void));
 
/************************************************************************
*
* function: pick_func()
*
* purpose: To handle mouse picking of element.  Invoked by right click.
*/

#define PICKBUFLENGTH 500
GLuint pickbuf[PICKBUFLENGTH];
int pick_flag;

void pick_func ARGS2((x,y),
int x, int y)
{ struct graph_thread_data *td = GET_DATA;
  GLint hits, viewport[4];
  int i,n;
  unsigned int enearz = 0xFFFFFFFF;
  unsigned int fnearz = 0xFFFFFFFF;
  unsigned int vnearz = 0xFFFFFFFF;
  facet_id f_id = NULLID;
  edge_id e_id = NULLID;
  vertex_id v_id = NULLID;
  int count;


  glSelectBuffer(PICKBUFLENGTH,pickbuf);
  glGetIntegerv(GL_VIEWPORT,viewport);
  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glRenderMode(GL_SELECT);  /* see what picked when drawn */
  glLoadIdentity();
  gluPickMatrix(x,y,4,4,viewport);
   
  glMultMatrixf(td->projmat);
  if ( SDIM >= 3 ) glMultMatrixd(flip[0]);    /* don't know why, but need */
  else glMultMatrixd(flip2D[0]);

  pick_flag = 1;
  if ( td->arraysflag )       /* have to turn arrays off during picking */
  { td->arraysflag = 0; graph_timestamp = ++global_timestamp; 
    draw_screen();
    td->arraysflag = 1; graph_timestamp = ++global_timestamp;
  }
  else
    draw_screen();
  pick_flag = 0;

  hits = glRenderMode(GL_RENDER); /* back to ordinary drawing */
  if ( hits < 0 ) 
  { hits = PICKBUFLENGTH/4;  /* buffer overflow */
    kb_error(2173,
      "Pick buffer overflow; selected element may not be foreground element.\n",
        WARNING);
  }
  
  for ( i=0, n=0 ; (i < hits) && (n < PICKBUFLENGTH-4) ; i++, n += count + 3 )
  { element_id id = name_to_id(pickbuf[n+3]);
    count = pickbuf[n];
    switch ( id_type(id) )
    { case FACET: 
        if ( pickbuf[n+1] < fnearz ) 
        { f_id = id; fnearz = pickbuf[n+1]; }
        break;
      case EDGE:
        if ( pickbuf[n+1] < enearz )
          if ( valid_id(id) || !valid_id(e_id) )
          { e_id = id; enearz = pickbuf[n+1]; }
        break;
      case VERTEX: 
        if ( pickbuf[n+1] < vnearz )
          if ( valid_id(id) )
           { v_id = id; vnearz = pickbuf[n+1]; }
        break;
       
    }
  }
  erroutstring("\n");  /* to get to next line after prompt */
  if ( valid_id(v_id) )
    { pickvnum = ordinal(v_id) + 1;
      #ifdef MPI_EVOLVER
      sprintf(msg,"Picked vertex %d@%d\n",pickvnum,id_task(v_id));
      #else
      sprintf(msg,"Picked vertex %d\n",pickvnum);
      #endif
      erroutstring(msg); 
    }

  if ( valid_id(e_id) ) 
  { 
#ifdef OLDPICKVERTEX
    /* check for vertex in common to picked edges */
    for ( i=0, n=0 ; (i < hits) && (n < PICKBUFLENGTH-4) ; i++, n += count+3 )
    { element_id id,ee_id;
      int ii,nn,ccount;

      count = pickbuf[n];
      id = name_to_id(pickbuf[n+3]);
      if ( (id_type(id) == EDGE) && valid_id(id) )
      { vertex_id v1 = get_edge_headv(id);
        vertex_id v2 = get_edge_tailv(id);
        for ( ii = i+1, nn = n+count+3 ; (ii < hits) && (n < PICKBUFLENGTH-4) ;
           ii++, nn += ccount + 3 )
        { ccount = pickbuf[nn];
          ee_id = name_to_id(pickbuf[nn+3]);
          if ( (id_type(ee_id) == EDGE) && valid_id(ee_id)
               && !equal_element(id,ee_id) )
          { if ( v1 == get_edge_tailv(ee_id) )
            { v_id = v1; break; }
            if ( v1 == get_edge_headv(ee_id) )
            { v_id = v1; break; }
            if ( v2 == get_edge_tailv(ee_id) )
            { v_id = v2; break; }
            if ( v2 == get_edge_headv(ee_id) )
            { v_id = v2; break; }
          }
        }
      }
    }
    if ( valid_id(v_id) )
    { pickvnum = ordinal(v_id) + 1;
      #ifdef MPI_EVOLVER
      sprintf(msg,"Picked vertex %d@%d\n",pickvnum,id_task(v_id));
      #else
      sprintf(msg,"Picked vertex %d\n",pickvnum);
      #endif
      erroutstring(msg); 
    }
#endif
    pickenum = ordinal(e_id) + 1;
    #ifdef MPI_EVOLVER
    sprintf(msg,"Picked edge %d@%d\n",pickenum,id_task(e_id));
    #else
    sprintf(msg,"Picked edge %d\n",pickenum);
    #endif
    erroutstring(msg);
  }
  else 
  if ( e_id == NULLEDGE )
     erroutstring("Picked facet subdivision edge.\n");

  if ( valid_id(f_id) ) 
  { pickfnum = ordinal(f_id) + 1;
    #ifdef MPI_EVOLVER
    sprintf(msg,"Picked facet %d@%d\n",pickfnum,id_task(f_id));
    #else
    sprintf(msg,"Picked facet %d\n",pickfnum);
    #endif
    erroutstring(msg);
  }
  erroutstring(current_prompt);

  glMatrixMode(GL_PROJECTION);
  glPopMatrix();

} /* end pick_func() */


 
/************************************************************************
*
* function: my_own_pick_func()
*
* purpose: To handle mouse picking of element.  Invoked by right click.
*          This version is entirely self-contained, and does not
*          use OpenGL picking, which is horribly slow in Vista.
*/

static REAL my_own_pick_radsq;
int my_own_pick_flag;
static vertex_id my_own_pick_vertex;
static edge_id my_own_pick_edge;
static facet_id my_own_pick_facet;
static REAL my_own_pick_vertex_depth;
static REAL my_own_pick_edge_depth;
static REAL my_own_pick_facet_depth;
static REAL mopt[4][4];
static REAL *my_own_pick_transmat[4] = {mopt[0],mopt[1],mopt[2],mopt[3]};
static REAL my_own_pick_x,my_own_pick_y;

void my_own_pick_func ARGS2((x,y),
int x, int y) /* pixel coordinates of pick spot */
{ 
  int i,j,k;
  float projmat[4][4];
  float modelmat[4][4];
  GLint viewport[4];

  glGetIntegerv(GL_VIEWPORT,viewport);
  my_own_pick_x = 2*(REAL)x/viewport[2] - 1.0;
  my_own_pick_y = -(2*(REAL)y/viewport[3] - 1.0);
  my_own_pick_radsq = 10*2.0/viewport[2]*2.0/viewport[3];

  /* Get current transformation matrices; note these are transposed
     from my convention */
  glGetFloatv(GL_PROJECTION_MATRIX,projmat[0]); 
  glGetFloatv(GL_MODELVIEW_MATRIX,modelmat[0]); 

  /* get product */
  for ( i = 0 ; i < 4 ; i++ )
    for ( j = 0 ; j < 4 ; j++ )
    { REAL sum = 0;
      for ( k = 0 ; k < 4 ; k++ )
      sum += projmat[k][i]*modelmat[j][k]; 
      my_own_pick_transmat[i][j] = sum;
    }

  my_own_pick_vertex = NULLID;
  my_own_pick_edge = NULLID;
  my_own_pick_facet = NULLID;
  my_own_pick_vertex_depth = 1e30;
  my_own_pick_edge_depth = 1e30;
  my_own_pick_facet_depth = 1e30;
  my_own_pick_flag = 1;
  graphgen();
  my_own_pick_flag = 0;

  erroutstring("\n");  /* to get to next line after prompt */
  if ( valid_id(my_own_pick_vertex) )
    { pickvnum = ordinal(my_own_pick_vertex) + 1;
      sprintf(msg,"Picked vertex %s\n",ELNAME(my_own_pick_vertex));
      erroutstring(msg); 
    }

  if ( valid_id(my_own_pick_edge) ) 
  { 
    pickenum = ordinal(my_own_pick_edge) + 1;
    sprintf(msg,"Picked edge %s\n",ELNAME(my_own_pick_edge));
    erroutstring(msg);
  }

  if ( valid_id(my_own_pick_facet) ) 
  { pickfnum = ordinal(my_own_pick_facet) + 1;
    sprintf(msg,"Picked facet %s\n",ELNAME(my_own_pick_facet));
    erroutstring(msg);
  }
  erroutstring(current_prompt);

} /* end my_own_pick_func() */

/**************************************************************************
*
* function: my_own_edge_pick()
*
* purpose: See if an edge intersects pick window.
*/

void my_own_edge_pick(gtail,ghead,e_id)
struct graphdata *gtail,*ghead;
edge_id e_id;
{
  REAL tailx[4],headx[4];
  REAL px,py,qx,qy,ex,ey,lensq,dotprod,lambda,this_z,distsq;

  if ( ! valid_id(e_id) )
     return;

  /* get endpoint pixel coordinates */
  matvec_mul(my_own_pick_transmat,gtail->x,tailx,4,4);
  matvec_mul(my_own_pick_transmat,ghead->x,headx,4,4);


  px = my_own_pick_x - tailx[0];
  py = my_own_pick_y - tailx[1];
  qx = my_own_pick_x - headx[0];
  qy = my_own_pick_y - headx[1];

  /* see if endpoints in pick circle */
  if ( px*px + py*py < my_own_pick_radsq )
  { if ( valid_id(gtail->v_id) && (tailx[2] < my_own_pick_vertex_depth) )
    { my_own_pick_vertex = gtail->v_id;
      my_own_pick_vertex_depth = tailx[2];
    }
  }
  if ( qx*qx + qy*qy < my_own_pick_radsq )
  { if ( valid_id(ghead->v_id) && (headx[2] < my_own_pick_vertex_depth) )
    { my_own_pick_vertex = ghead->v_id;
      my_own_pick_vertex_depth = headx[2];
    }
  }

  
  /* now find closest point on edge, via barycentric parameter lambda */
  ex = headx[0] - tailx[0];
  ey = headx[1] - tailx[1];
  lensq = ex*ex + ey*ey;
  dotprod = px*ex + py*ey;
  lambda = dotprod/lensq;
  if ( lambda < 0.0 || lambda > 1.0 )
    return;

  distsq = px*px + py*py - lambda*lambda*(ex*ex + ey*ey);
  if ( distsq > my_own_pick_radsq )
     return;

  this_z = (1-lambda)*tailx[2] + lambda*tailx[2];
  if ( this_z < my_own_pick_edge_depth )
  { my_own_pick_edge = e_id;
    my_own_pick_edge_depth = this_z;
  }
} /* end my_own_edge_pick() */

/************************************************************************** 
*
* function: my_own_facet_pick()
*
* purpose: See if an edge intersects pick window.
*/

void my_own_facet_pick(g,f_id)
struct graphdata *g;
edge_id f_id;
{
  REAL base[4],head1[4],head2[4];
  REAL px,py,ax,ay,bx,by;
  REAL det,alpha,beta;
  REAL this_z;

  if ( ! valid_id(f_id) )
     return;

  /* get vertex pixel coordinates */
  matvec_mul(my_own_pick_transmat,g[0].x,base,4,4);
  matvec_mul(my_own_pick_transmat,g[1].x,head1,4,4);
  matvec_mul(my_own_pick_transmat,g[2].x,head2,4,4);


  px = my_own_pick_x - base[0];
  py = my_own_pick_y - base[1];
  ax = head1[0] - base[0];
  ay = head1[1] - base[1];
  bx = head2[0] - base[0];
  by = head2[1] - base[1];

  /* Calculate barycentric parameters of pick point */
  det = ax*by - ay*bx;
  if ( det == 0 ) return;
  alpha = (by*px - bx*py)/det;
  beta  = (ax*py - ay*px)/det;

  if ( alpha < 0.0 || beta < 0.0 || alpha+beta > 1.0 )
    return;

  this_z = (1-alpha-beta)*base[2] + alpha*head1[2] + beta*head2[2];
  if ( this_z < my_own_pick_facet_depth )
  { my_own_pick_facet = f_id;
    my_own_pick_facet_depth = this_z;
  }
} /* end my_own_facet_pick() */


/****************************************************************************/


/***********************************************************
*
* function: mouse_func()
*
* purpose: Called on mouse button events, records position.
*/

void mouse_func ARGS((int,int,int,int));

void mouse_func ARGS4((button,state,x,y),
int button, int state, int x, int y)
{ struct graph_thread_data *td = GET_DATA;
  switch ( button )
  { case GLUT_LEFT_BUTTON:
      switch ( state )
      { case GLUT_DOWN:  /* start tracking */
           td->oldx = x; td->oldy = y;
           td->mouse_left_state = GLUT_DOWN;
           glutIdleFunc(idle_func);
           break;
        case GLUT_UP:  /* stop tracking */
           td->basex = td->newx;
           td->basey = td->newy;
           td->mouse_left_state = GLUT_UP;
           glutPostRedisplay();
           glutIdleFunc(idle_func);
           break;
      }
      break;

    case GLUT_RIGHT_BUTTON:
      switch ( state )
      { case GLUT_DOWN: 
          // pick_func(x,y);
          my_own_pick_func(x,y);
          glutPostRedisplay();  /* get image back */  
          break;
      }
      break;
  }
}

/*******************************************************************
*
* function: mouse_loc_func()
*
* purpose: Called as mouse moves with left button down, this
*          moves surface according to current mouse_mode.
*/
void mouse_loc_func ARGS((int,int));

void mouse_loc_func ARGS2((x,y),
int x, int y)
{ struct graph_thread_data *td = GET_DATA;
  int i,j;

  td->newx = x;
  td->newy = y; 
  if ( td->mouse_left_state == GLUT_DOWN )
  { switch ( td->mouse_mode )
    {  case MM_SLICE:
         if ( slice_view_flag )
          { slice_coeff[SDIM] += (td->newx-td->oldx)*td->xscale/view[0][0];
            td->newarraysflag = 1;
            break;
          }
          else if ( clip_view_flag )
          { clip_coeff[0][SDIM] += (td->newx-td->oldx)*td->xscale/view[0][0];
            td->newarraysflag = 1;
            break;
          }
          else
          /* reset back to rotate by default and fall through */
            td->mouse_mode = MM_ROTATE;
 
      case MM_ROTATE:    
       mat_mult(td->to_focus,td->view,td->view,HOMDIM,HOMDIM,HOMDIM);
       fix_ctm(td->view,(REAL)( td->newx - td->oldx),
                       -(REAL)(td->newy - td->oldy));
       mat_mult(td->from_focus,td->view,td->view,HOMDIM,HOMDIM,HOMDIM);
       break;

      case MM_SCALE:
        mat_mult(td->to_focus,td->view,td->view,HOMDIM,HOMDIM,HOMDIM);
        for(i = 0 ; i < HOMDIM-1; i++ )
            for ( j = 0 ; j < HOMDIM ; j++ )
                td->view[i][j] *= 1.0 +0.002*(td->newx-td->oldx);
        mat_mult(td->from_focus,td->view,td->view,HOMDIM,HOMDIM,HOMDIM);
        break;

      case MM_TRANSLATE:
        if ( SDIM == 2 )
        { td->view[0][2] += (td->newx-td->oldx)*td->xscale;
          td->view[1][2] -= (td->newy-td->oldy)*td->yscale;
          td->to_focus[0][2] -= (td->newx-td->oldx)*td->xscale;
          td->to_focus[1][2] += (td->newy-td->oldy)*td->yscale;
          td->from_focus[0][2] += (td->newx-td->oldx)*td->xscale;
          td->from_focus[1][2] -= (td->newy-td->oldy)*td->yscale;
        } else
        {
          td->view[1][HOMDIM-1] += (td->newx-td->oldx)*td->xscale;
          td->view[2][HOMDIM-1] -= (td->newy-td->oldy)*td->yscale;
          td->to_focus[1][HOMDIM-1] -= (td->newx-td->oldx)*td->xscale;
          td->to_focus[2][HOMDIM-1] += (td->newy-td->oldy)*td->yscale;
          td->from_focus[1][HOMDIM-1] += (td->newx-td->oldx)*td->xscale;
          td->from_focus[2][HOMDIM-1] -= (td->newy-td->oldy)*td->yscale;
        };
        break;

      case MM_SPIN: /* about z axis */
        { MAT2D(rot,MAXCOORD+1,MAXCOORD+1);
          REAL dth;
          REAL dang; /* fourth dimension */
          for ( i = 0 ; i < HOMDIM ; i++ )
          { for ( j = 0 ; j < HOMDIM ; j++ )
              rot[i][j] = 0.0;
            rot[i][i] = 1.0;
          }
          dth = (td->newx - td->oldx)/300.0*M_PI;
          dang = (td->newy - td->oldy)/300.0*M_PI;
          if ( SDIM == 2 )
          { rot[0][0] = rot[1][1] = cos(dth);
            rot[0][1] = -(rot[1][0] = sin(dth));
          } else
          { rot[1][1] = rot[2][2] = cos(dth);
            rot[1][2] = -(rot[2][1] = sin(dth));
          } 
          mat_mult(td->to_focus,td->view,td->view,HOMDIM,HOMDIM,HOMDIM);
          mat_mult(rot,td->view,td->view,HOMDIM,HOMDIM,HOMDIM);
          mat_mult(td->from_focus,td->view,td->view,HOMDIM,HOMDIM,HOMDIM);
          }
        break;

      case MM_SLICE_SPIN:
        {
          REAL dth;
          REAL dang; 
          REAL temp1[3];
          REAL temp2[3];

          dang = (td->newx - td->oldx)/300.0*M_PI;
          dth = (td->newy - td->oldy)/300.0*M_PI;

          matvec_mul(td->from_focus,clip_coeff[0],temp1,3,3);
          temp2[0] = cos(dth)*temp1[0] - sin(dth)*temp1[2];
          temp2[1] = temp1[1];
          temp2[2] = sin(dth)*temp1[0] + cos(dth)*temp1[2];
          temp1[2] = temp2[2];
          temp1[0] = cos(dang)*temp2[0] - sin(dang)*temp2[1];
          temp1[1] = sin(dang)*temp2[0] + cos(dang)*temp2[1];
          matvec_mul(td->to_focus,temp1,clip_coeff[0],3,3);
          td->newarraysflag = 1;
          break;
        }
      }
      if ( td->idle_flag )
         glutPostRedisplay();
    }

  td->oldx = td->newx; td->oldy = td->newy;
}

/************************************************************************
*
* function: reshape_func()
*
* purpose: handle window resize messages.
*/
void reshape_func ARGS(( int, int ));

void reshape_func ARGS2(( x, y ),
int x, int y)
{ struct graph_thread_data *td = GET_DATA;

  if ( window_aspect_ratio != 0.0 )
  { /* munge x,y */
    
    /* see if moving just one side of frame */
    if ( abs(td->xsize - x) < abs(td->ysize - y) )
    { /* moving y */
      /* if ( td->ysize == y ) return; */ /* not changing */
      x = (int)(y/fabs(window_aspect_ratio));
    }
    else 
    { /* moving x */
      y = (int)(x*fabs(window_aspect_ratio));
    }
    td->window_aspect = window_aspect_ratio;
    td->aspect_flag = 0;
    glutReshapeWindow(x,y);
  }
  
  td->xsize = x; td->ysize = y;
  td->aspect = (double)y/x;
  glViewport(0,0,x,y);
  if ( td->aspect > 1 ) 
  { td->xscale = 2.8/td->aspect/x; td->yscale = 2.8/y; 
    imagescale = td->yscale*100;
  }
  else 
  { td->xscale = 2.8/x; td->yscale = 2.8*td->aspect/y; 
    imagescale = td->xscale*100;
  }
  if ( td == gthread_data+1 )
  { /* first window corresponds to printing graphics state */
    if ( x < y ) 
    { minclipx = -1.5; maxclipx = 1.5;
      minclipy = -1.5*y/x; maxclipy = 1.5*y/x;
    }
    else
    { minclipx = -1.5*x/y; maxclipx = 1.5*x/y;
      minclipy = -1.5; maxclipy = 1.5;
    }
  } 
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  
  if ( (td->projmode == P_PERSP) || td->stereomode )
  {     
     gluPerspective((float)(y/1600.*180/3.14159),1/td->aspect,1.0,20.0);
     if ( SDIM == 2 ) glMultMatrixd(vt2[0]); 
     else glMultMatrixd(vt3p[0]);  /* rotate axes */
  }
  else
  {
    if ( td->aspect >= 1.0 )
    { glOrtho(td->scrx[0],td->scrx[2],td->aspect*td->scry[0],
             td->aspect*td->scry[2],-20.0,20.0);
      if ( td == gthread_data+1 )
      { minclipx = td->scrx[0]; maxclipx = td->scrx[2]; 
        minclipy = td->aspect*td->scry[2]; maxclipy = td->aspect*td->scry[0];
      }
    }
    else
    { glOrtho(td->scrx[0]/td->aspect,td->scrx[2]/td->aspect,td->scry[0],
          td->scry[2],-20.0,20.0);
      if ( td == gthread_data+1 )
      { minclipx = td->scrx[0]/td->aspect; maxclipx = td->scrx[2]/td->aspect; 
        minclipy = td->scry[2]; maxclipy = td->scry[0];
      }
    }
 
    if ( SDIM == 2 ) glMultMatrixd(vt2[0]); /* upside down */
    else glMultMatrixd(vt3[0]);  /* upside down and rotate axes */
   }

  glGetFloatv(GL_PROJECTION_MATRIX,td->projmat); /* save */

  GL_ERROR_CHECK

#ifdef resizequick
  td->resize_flag = 1; /* So Mac OS X won't try too much redrawing */
  glutIdleFunc(idle_func);
#endif
}


/***********************************************************************
*
* Function: specialkey_func()
*
* purpose: handle special keystrokes in graphics window.
*/

void specialkey_func ARGS((int,int,int));

void specialkey_func ARGS3((key,x,y),
int key, int x, int y)
{ struct graph_thread_data *td = GET_DATA;
  switch ( key )
  { 
    case GLUT_KEY_RIGHT: /* right arrow */
      td->view[SDIM>2?1:0][HOMDIM-1] += .25; break;
    case GLUT_KEY_LEFT:  /* left arrow */ 
      td->view[SDIM>2?1:0][HOMDIM-1] -= .25; break;
    case GLUT_KEY_UP:    /* up arrow */  
      td->view[SDIM>2?2:1][HOMDIM-1] += .25; break;
    case GLUT_KEY_DOWN:  /* down arrow */
      td->view[SDIM>2?2:1][HOMDIM-1] -= .25; break;
  }
  glutPostRedisplay();  /* generate redraw message */
}

/***********************************************************************
*
* Function: key_func()
*
* purpose: handle ASCII keystrokes in graphics window.
*/

void key_func ARGS((unsigned char,int,int));

void key_func ARGS3((key,x,y),
unsigned char key,
int x, int y)
{ struct graph_thread_data *td = GET_DATA;
  int i,j;

  switch ( key )
  { 
    case 'G': /* new graphics window */
        dup_window = glutGetWindow();
        draw_thread(NULL);  /* actually, just a new window */
      break;

    case 'r':  td->mouse_mode = MM_ROTATE; break;
    case 't':  td->mouse_mode = MM_TRANSLATE; break;
    case 'z':  td->mouse_mode = MM_SCALE; break;
    case 'c':  td->mouse_mode = MM_SPIN; break;
    case 'k':  if ( !slice_view_flag && !clip_view_flag )
                 td->newarraysflag = 1;
               if ( !slice_view_flag )
                 clip_view_flag = 1;  /* turn clip_view on */ 
               td->mouse_mode = MM_SLICE_SPIN;
               break;
    case 'l':  if ( !slice_view_flag && !clip_view_flag )
                 td->newarraysflag = 1;
               if ( !slice_view_flag )
                 clip_view_flag = 1;  /* turn clip_view on */ 
               td->mouse_mode = MM_SLICE;
               break;
    case 'L': slice_view_flag = clip_view_flag = 0;
              td->newarraysflag = 1;
              td->mouse_mode = MM_ROTATE;
              break;

    case 'o': box_flag = !box_flag;
              graph_timestamp = ++global_timestamp; 
              break;

    case 'b':
        td->edge_bias -= 0.001; 
        sprintf(msg,"\nEdge front bias now %f\n",td->edge_bias); erroutstring(msg);
        erroutstring(current_prompt);
        break;

    case 'B':
        td->edge_bias += 0.001; 
        sprintf(msg,"\nEdge front bias now %f\n",td->edge_bias); erroutstring(msg);
        erroutstring(current_prompt);
        break;

    case 'R':
        resize(); 
        for ( i = 0 ; i < HOMDIM ; i++ )
          for ( j = 0 ; j < HOMDIM ; j++ )
            td->view[i][j] = view[i][j];
        matcopy(td->to_focus,identmat,HOMDIM,HOMDIM);
        matcopy(td->from_focus,identmat,HOMDIM,HOMDIM);
        break;
 
    case '-':
        if ( td->linewidth > 0.6 )
        { td->linewidth -= 0.5;  
          glLineWidth(td->linewidth);
        } 
        break;
  
    case '+':
        if ( td->linewidth < 9.9 ) 
        { td->linewidth += 0.5; 
          glLineWidth(td->linewidth);
        }
        break;

    case 'e':
        edgeshow_flag = !edgeshow_flag; 
        graph_timestamp = ++global_timestamp; 
        td->newarraysflag = 1;
        break;

    case 'f':
        td->facetshow_flag = !td->facetshow_flag; 
        graph_timestamp = ++global_timestamp; 
        td->newarraysflag = 1;
        break;

    case 'g':  /* Gourard toggle */
        td->normflag = !td->normflag; graph_timestamp = ++global_timestamp; 
        td->newarraysflag = 1;
        break;

    case 'm': /* move to middle */
         { do_gfile(0,NULL); /* get bounding box */
           if ( SDIM == 2 )
            { td->view[0][HOMDIM-1] -= (bbox_maxx+bbox_minx)/2;
              td->view[1][HOMDIM-1] -= (bbox_maxy+bbox_miny)/2;
            } else
            { td->view[1][HOMDIM-1] -= (bbox_maxx+bbox_minx)/2;
              td->view[2][HOMDIM-1] -= (bbox_maxy+bbox_miny)/2;
            }
            break;
         }

    case 'i': /* toggle interleaved elements in opengl arrays */ 
        td->interleaved_flag = !td->interleaved_flag;
        erroutstring(td->interleaved_flag?"Interleaving ON.\n":"Interleaving OFF.\n"); 
        erroutstring(current_prompt);
        td->newarraysflag = 1;
        break;

    case 'I': /* indexed arrays */
        td->indexing_flag = !td->indexing_flag;
        erroutstring(td->indexing_flag?"Array indexing ON.\n":"Array indexing OFF.\n"); 
        erroutstring(current_prompt);
        td->newarraysflag = 1;
        break;
   
    case 'S': /* toggle sending strips in arrays */
        td->strips_flag = !td->strips_flag;
        erroutstring(td->strips_flag?"Element strips ON.\n":"Element strips OFF.\n"); 
        erroutstring(current_prompt);
        td->normflag = 1; /* gourard shading */
        td->indexing_flag = 1;
        td->newarraysflag = 1;
        break;
 
    case 'Y': /* colored strips */
      { int *fcolors;
        int i;
        fcolors = (int*)temp_calloc(web.skel[FACET].maxcount,sizeof(int));
        for ( i = 0 ; i < web.skel[FACET].maxcount ; i++ )
           fcolors[i] = get_facet_color(i);
        if ( !td->strips_flag ) 
           key_func('S',0,0);  /* make sure strips on */
        td->strip_color_flag = 1;
        td->newarraysflag = 1;
        draw_screen();  /* get facets colored */
        td->newarraysflag = 1;
        draw_screen();  /* draw colored facets */
        td->strip_color_flag = 0;
        for ( i = 0 ; i < web.skel[FACET].maxcount ; i++ )
           set_facet_color(i,fcolors[i]);
        temp_free((char*)fcolors);
      }
      break;

    case 'F': /* use last pick to set rotation center */
        if ( pickvnum > 0 )
        { int i,m;
          REAL *x,xx[MAXCOORD];
          REAL focus[MAXCOORD];

          td->focus_vertex_id = get_ordinal_id(VERTEX,pickvnum-1);
          x = get_coord(td->focus_vertex_id);  
          for ( m = 0 ; m < SDIM ; m++ ) xx[m] = x[m];
          if ( torus_display_mode == TORUS_CLIPPED_MODE )
          { 
            for ( m = 0 ; m < SDIM ; m++ )
            { int wrap = (int)floor(SDIM_dot(web.inverse_periods[m],x));
              for ( i = 0 ; i < SDIM ; i++ )
                xx[i] -= wrap*web.torus_period[m][i];
            }
          }

          matvec_mul(td->view,xx,focus,HOMDIM-1,HOMDIM-1);
          for ( i = 0 ; i < SDIM ; i++ ) 
          { td->to_focus[i][HOMDIM-1] = -focus[i] - td->view[i][HOMDIM-1];
            td->from_focus[i][HOMDIM-1] = focus[i] + td->view[i][HOMDIM-1];
          }
        }
        break;

    case 'D': /* toggle display list */
        td->dlistflag = td->dlistflag ? NOLIST:RESETLIST; 
        if ( td->dlistflag )
        { erroutstring("\nOpenGL display list now ON.\n");
          if ( td->arraysflag )
          { glDisableClientState(GL_COLOR_ARRAY);
            glDisableClientState(GL_NORMAL_ARRAY);
            glDisableClientState(GL_VERTEX_ARRAY);
            erroutstring("\nOpenGL arrays now OFF.\n");
          }
          td->arraysflag = 0;
        }
        else erroutstring("\nOpenGL display list now OFF.\n");
        erroutstring(current_prompt);
        graph_timestamp = ++global_timestamp; /* force recalculate arrays */
        break;
  
    case 'a': /* toggle vertex arrays */
        if ( strcmp(opengl_version,"1.1") < 0 )
        { sprintf(errmsg,
           "Vertex arrays require OpenGL version at least 1.1. This is %s.\n",
                   opengl_version);
          kb_error(2174,errmsg,WARNING);
         return;
        }
        td->arraysflag = !td->arraysflag; 
        if ( td->arraysflag )
        { /* Workaround really bizarre line-drawing bug */
          if ( td->linewidth == 1.0 ) { td->linewidth = 0.5; glLineWidth(0.5);}
          erroutstring("\nOpenGL vertex arrays now ON.\n");
          glInterleavedArrays(GL_C4F_N3F_V3F,0,(void*)td->fullarray);
          td->newarraysflag = 1;
          td->dlistflag = 0;
        }
        else
        {
          glDisableClientState(GL_COLOR_ARRAY);
          glDisableClientState(GL_NORMAL_ARRAY);
          glDisableClientState(GL_VERTEX_ARRAY);
          erroutstring("\nOpenGL vertex_arrays now OFF.\n");
        }
        erroutstring(current_prompt);
        break;

   case 's': /* toggle stereo */
      td->stereomode = (td->stereomode==NO_STEREO) ? CROSS_STEREO:NO_STEREO; 
      reshape_func(td->xsize,td->ysize);
      break;
 
   case 'p': /* perspective mode */
      if ( td->stereomode ) 
         erroutstring("\nOpenGL projection now CROSS-EYED STEREO.\n"); 
      else if ( td->projmode==P_ORTHO ) 
      { td->projmode=P_PERSP; 
        erroutstring("\nOpenGL projection now PERSPECTIVE.\n");
      }
      else 
      { td->projmode=P_ORTHO; 
        erroutstring("\nOpenGL projection now ORTHOGONAL.\n");
      }
      erroutstring(current_prompt);
      reshape_func(td->xsize,td->ysize);
      break;
 
   case 'M': /* menu mode on right mouse button */
      glutSetMenu(mainmenu);
      glutAttachMenu(GLUT_RIGHT_BUTTON);
      break;


   case 'P': /* pick mode on right mouse button */
      glutSetMenu(mainmenu);
      glutDetachMenu(GLUT_RIGHT_BUTTON);
      break;

   case 'Q': /* toggle drawing stats printing */
      td->q_flag = !td->q_flag;
      erroutstring(td->q_flag ?
       "\nPrinting drawing stats ON\n":"\nPrinting drawing stats OFF\n");
      erroutstring(current_prompt);
      break;

   #ifdef MPI_EVOLVER
   case 'y': /* MPI version only */
      mpi_show_corona_flag = ! mpi_show_corona_flag;
      td->newarraysflag = 1;
      erroutstring(mpi_show_corona_flag ?
       "\nShowing MPI corona ON\n":"\nShowing MPI corona OFF\n");
      erroutstring(current_prompt);
      break;
   #endif

   case 'x': 
      Ogl_close();
      return;

   case 'h': case '?':
      erroutstring("\nGraphics window help:\n");
      erroutstring("Left mouse: move   Right mouse: pick\n");
      erroutstring("Graphics window keys:\n");
      erroutstring(td->mouse_mode==MM_ROTATE?
        "r  Rotate mode for left mouse button, now ACTIVE\n":
        "r  Rotate mode for left mouse button\n");
      erroutstring(td->mouse_mode==MM_TRANSLATE?
        "t  Translate mode for left mouse button, now ACTIVE\n":
        "t  Translate mode for left mouse button\n");
      erroutstring(td->mouse_mode==MM_SCALE?
        "z  Zoom mode for left mouse button, now ACTIVE\n":
        "z  Zoom mode for left mouse button\n");
      erroutstring(td->mouse_mode==MM_SPIN?
        "c  Clockwise/counterclockwise mode, now ACTIVE\n":
        "c  Clockwise/counterclockwise mode\n");
      erroutstring("W  Widen edges\n");
      erroutstring("w  Narrow edges\n");
      erroutstring("B  Increase edge front bias by 0.001\n");
      erroutstring("b  Decrease edge front bias by 0.001\n");
      erroutstring(normflag? 
        "g  Gourard shading (smooth shading) toggle, now ON\n":
        "g  Gourard shading (smooth shading) toggle, now OFF\n" );
      erroutstring("R  Reset view\n");
      erroutstring("m  Center image\n");
      erroutstring(edgeshow_flag?"e  Toggle showing all edges, now ON\n":
        "e  Toggle showing all edges, now OFF\n");
      erroutstring(td->facetshow_flag?"f  Toggle showing facets, now ON\n":
        "f  Toggle showing facets, now OFF\n");
      erroutstring(td->projmode==P_PERSP?
        "p  Toggle orthogonal/perspective projection, now perspective\n":
        "p  Toggle orthogonal/perspective projection, now orthogonal\n");
      erroutstring("s  Toggle cross-eyed stereo\n");
      erroutstring(normflag? "g  Gourard shading (smooth shading) toggle, now ON\n":
             "g  Gourard shading (smooth shading) toggle, now OFF\n" );
      erroutstring("F  Set rotate/zoom focus to last picked vertex\n");
      erroutstring("arrow keys  translate image\n");
      erroutstring("G  Another graphics window\n");
      erroutstring("H  Guru-level help items\n");
      erroutstring("x  Close graphics\n");
      erroutstring(current_prompt);
      break;

   case 'H': /* guru level help */
     erroutstring("\nFollowing for fiddling with OpenGL drawing modes:\n");
     erroutstring(td->dlistflag?"D  Toggle using display list, now ON\n":
       "D  Toggle using display list, now OFF\n");
     erroutstring(td->arraysflag?"a  Toggle using vertex and color arrays, now ON\n"
        :"a  Toggle using vertex and color arrays, now OFF\n");
     erroutstring(td->indexing_flag ? "I  Indexed vertex arrays, now ON\n"
                           : "I  Indexed vertex arrays, now OFF\n");
     erroutstring(td->interleaved_flag ? "i  Interleaved vertex arrays, now ON\n"
                           : "i  Interleaved vertex arrays, now OFF\n");
     erroutstring(td->strips_flag?
    "S  Use element strips, now ON (indexed elements automatically turned on)\n"
    :"S  Use element strips, now OFF\n");
     erroutstring("Y  One-time coloring of strips generated in S mode.\n");
     erroutstring("       Caution: assumes display facets same as real, with no gaps.\n");
     erroutstring("u  Toggle window update every graphics command, now ");
     erroutstring("Q  Toggle printing some statistics during drawing\n");
     #ifdef MPI_EVOLVER
     erroutstring("y  Toggle corona display\n");
     #endif
 
     erroutstring(current_prompt);
     break;
  }
  glutPostRedisplay();  /* generate redraw message */
} /* end of key_func() */


 void mainmenu_func ARGS((int));
 void submenu_func ARGS((int));
 void mpi_taskmenu_func ARGS((int));

 void mainmenu_func ARGS1((choice),
 int choice)
 { 
   key_func(choice,0,0); 
 }
 
 void submenu_func ARGS1((choice),
 int choice)
 { 
   key_func(choice,0,0);
 }
 
 #ifdef MPI_EVOLVER
 void mpi_taskmenu_func ( task )
 int task;
 { struct graph_thread_data *td = GET_DATA;
   td->mpi_graph_task = task;
   set_title(td);
   glutSetWindowTitle(td->wintitle);
   graph_timestamp = ++global_timestamp; 
   td->newarraysflag = 1;
   glutPostRedisplay();
 }
 #endif

 void myMenuInit()
 { mainmenu = glutCreateMenu(mainmenu_func);
   glutSetMenu(mainmenu);
   glutAddMenuEntry("Left button modes:",' ');
   glutAddMenuEntry("Rotate mode (r)",'r');
   glutAddMenuEntry("Translate mode (t)",'t');
   glutAddMenuEntry("Spin mode (c)",'c');
   glutAddMenuEntry("Scale mode (z)",'z');
   glutAddMenuEntry("Slice mode (l)",'l');
   glutAddMenuEntry("Right button modes:",' ');
   glutAddMenuEntry("Pick mode (P)",'P');
   glutAddMenuEntry("Menu mode (M)",'M');
   glutAddMenuEntry("Edges front (B)",'B');
   glutAddMenuEntry("Edges back (b)",'b');
   glutAddMenuEntry("Edges thicker (+)",'+');
   glutAddMenuEntry("Edges thinner (-)",'-');
   glutAddMenuEntry("Center object (m)",'m');
   glutAddMenuEntry("Toggle edges (e)",'e');
   glutAddMenuEntry("Toggle faces (f)",'f');
   glutAddMenuEntry("Focus on picked vertex (F)",'F');
   glutAddMenuEntry("Bounding box (o)",'o');
   glutAddMenuEntry("Reset graphics (R)",'R');
   glutAddMenuEntry("",' ');  /* skip funny entry before submenu */

   submenu = glutCreateMenu(submenu_func);
   glutSetMenu(submenu);
   glutAddMenuEntry("Another graphics window (G)",'G');
   glutAddMenuEntry("Toggle Gourard shading (g)",'g');
   glutAddMenuEntry("Toggle stereo view (s)",'s');
   glutAddMenuEntry("Toggle orthogonal/perspective view (p)",'p');
   glutAddMenuEntry("Toggle arrays (a)",'a');
   glutAddMenuEntry("Toggle interleaved arrays (i)",'i');
   glutAddMenuEntry("Toggle indexed arrays (I)",'I');
   glutAddMenuEntry("Toggle strips (S)",'S');
   glutAddMenuEntry("Toggle strip coloring (Y)",'Y');
   glutAddMenuEntry("Toggle display lists (D)",'D');
   glutAddMenuEntry("Print drawing stats (Q)",'Q');
   #ifdef MPI_EVOLVER
   glutAddMenuEntry("Toggle corona display (y)",'y');
   #endif
   glutSetMenu(mainmenu);
   glutAddSubMenu("Advanced",submenu);

   #ifdef MPI_EVOLVER
   { int task;
     mpi_taskmenu = glutCreateMenu(mpi_taskmenu_func);
     glutSetMenu(mpi_taskmenu);
     for ( task = 1 ; task < mpi_nprocs ; task++ )
     { char number[10];
       sprintf(number," %4d ",task);
       glutAddMenuEntry(number,task);
     }
     glutSetMenu(mainmenu);
     glutAddSubMenu("Pick MPI task",mpi_taskmenu);
   }
   #endif

   glutAddMenuEntry("Close graphics (x)",'x');
   glutAttachMenu(GLUT_MIDDLE_BUTTON);
 }
 

 /* lighting info for surface */
 static GLfloat mat_specular[] = {.5f,.5f,.5f,1.0f};
 static GLfloat mat_shininess[] = {10.0f};
 static GLfloat mat_diffuse[] = {1.0f,1.0f,1.0f,1.0f}; 
 static GLfloat mat_white[] = {1.0f,1.0f,1.0f,1.0f};
 static GLfloat mat_emission[] = {0.3f,0.3f,0.3f,1.0f};
#define INTENSITY1  0.5f
 static GLfloat light0_position[] = {1.0f,0.0f,1.0f,0.0f};  /* front */
 static GLfloat light0_diffuse[] = {INTENSITY1,INTENSITY1,INTENSITY1,1.0f};
 static GLfloat light0_ambient[] = {.3f,.3f,.3f,1.0f};

#define INTENSITY2  0.5f
 static GLfloat light1_position[] = {0.0f,0.0f,1.0f,0.0f};  /* above */
 static GLfloat light1_diffuse[] = {INTENSITY2,INTENSITY2,INTENSITY2,1.0f};
 static GLfloat light_none[] = {0.f,0.f,0.f,.0f};

#ifdef WIN32
 /*******************************************************************
 *
 * function: handle_func()
 *
 * purpose: callback for EnumThreadWindows()
 */

static int handle_count;

BOOL __stdcall handle_func(HWND hwnd, LPARAM lParam)
{ int i;
  for ( i = 1 ; i < MAXGRAPHWINDOWS ; i++ )
  { if ( gthread_data[i].draw_hwnd == hwnd ) return TRUE;
    if ( !gthread_data[i].draw_hwnd )
    { gthread_data[i].draw_hwnd = hwnd;
      break;
    }    
  }
  return TRUE;
}
#endif

#ifndef WIN32

 /*****************************************************************
 *
 * function: glut_catcher()
 *
 * purpose: catch signals meant to wake up thread.
 */

 void glut_catcher ARGS1((x),
 int x)
 {
   signal(SIGKICK,glut_catcher);
 }

 #endif

 /*****************************************************************
 *
 * function: draw_thread()
 *
 * purpose: Create OpenGL display thread and window.
 */
#ifdef PTHREADS
void * draw_thread ARGS1((arglist),
void *arglist)
#else
void __cdecl draw_thread ARGS1((arglist),
void *arglist)
#endif
{ int i,j;
  int argc = 1; /* to keep glutInit happy */
  char *argv = "Evolver";
  struct graph_thread_data *td;
  char wintitle[WINTITLESIZE];
  static int win_id;
  int glut_id;
  int xpixels,ypixels;

#ifdef WIN32
  HWND foregroundhwnd = GetForegroundWindow();
  draw_thread_id = GetCurrentThreadId();
  /* set per-thread data */
  TlsSetValue(thread_data_key,(void*)&glutgraph_thread_data);
 
#else
  draw_thread_id = pthread_self();  /* get thread id */
  draw_pid = getpid();
  /* set per-thread data */
  pthread_setspecific(thread_data_key,(void*)&glutgraph_thread_data);
#endif

  for ( i = 0 ; i < 16 ; i++ )
    for ( j = 0 ; j < 4 ; j++ )
      rgba[i][j] = (float)rgb_colors[i][j];

  if ( !glutInit_called )
    glutInit(&argc,&argv);
  glutInit_called = 1;
  if ( !graph_thread_running ) win_id = 0;
  win_id++;
  if ( win_id >= MAXGRAPHWINDOWS )
  { kb_error(2556,"No more graphics windows available.\n",WARNING);
  }
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH );
  glutInitWindowPosition((20*win_id)%400,(20*win_id)%400);
  if ( window_aspect_ratio )
  { xpixels = (int)(400/sqrt(fabs(window_aspect_ratio)));
    ypixels = (int)(400*sqrt(fabs(window_aspect_ratio)));
  }
  else 
  { xpixels = 400;
    ypixels = 400;
  }
  glutInitWindowSize(xpixels,ypixels); 
#ifdef MAC_OS_X
  { char title[1000];
    sprintf(title,"   %s (CTRL-click for right mouse button)",datafilename);
    glutCreateWindow(title);
  }
#else
  glutCreateWindow(datafilename);
#endif

  glutMouseFunc(mouse_func); 
  glutMotionFunc(mouse_loc_func); 
  glutKeyboardFunc(key_func);
  glutSpecialFunc(specialkey_func);
  glutReshapeFunc(reshape_func);
  glutIdleFunc(idle_func); 
  myMenuInit();
  glutShowWindow();  /* start on top */

  glut_id = glutGetWindow(); /* window identifier */
  if ( glut_id >= 10 )
     kb_error(2596,"glut window id too high.\n",RECOVERABLE);
  else td = gthread_data + glut_id;

#ifdef WIN32
  /* get window handle, do this before setting win_id to prevent
     race with main thread display() */
  handle_count = 1;
  EnumThreadWindows(draw_thread_id,handle_func,0);

  /* try to get on top */
 
  SetForegroundWindow(gthread_data[glut_id].draw_hwnd);
  SetForegroundWindow(foregroundhwnd);  /* get console back on top */

#endif
  td = GET_DATA;
  td->in_use = 1;
  td->win_id = glut_id;
  td->aspect = 1;
  td->xscale=2.8/xpixels;
  td->yscale=2.8/ypixels;
  background_color = LIGHTBLUE;
  td->xsize = xpixels; 
  td->ysize = ypixels;
  td->projmode = P_ORTHO;    /* kind of projection to do */
  td->stereomode = NO_STEREO;
  td->facetshow_flag = 1; /* whether to show facets */
  td->linewidth = 1.0;
  td->edge_bias = 0.005; /* amount edges drawn in front */
  td->mouse_left_state = GLUT_UP; /* state of left mouse button */
  td->mouse_mode = MM_ROTATE;
  if (glut_id > 1 ) 
  { if ( strlen(datafilename) > 60 )
      sprintf(wintitle,"  %1.*s - Camera %d",WINTITLESIZE-30,
         datafilename+strlen(datafilename)-60, glut_id);
    else  sprintf(wintitle,"  %1.*s - Camera %d",WINTITLESIZE-30,
         datafilename,glut_id);
#ifdef MPI_EVOLVER
    sprintf(wintitle+strlen(wintitle)," (task %d)",this_task);
#endif
    glutSetWindowTitle(wintitle);
  }

 

  ENTER_GRAPH_MUTEX; /* due to view[][] */
  if ( dup_window )
  { struct graph_thread_data *tdd = gthread_data+dup_window;
    for ( i = 0 ; i < HOMDIM ; i++ )
    { td->view[i] = td->viewspace[i];
      td->to_focus[i] = td->to_focus_space[i];
      td->from_focus[i] = td->from_focus_space[i];
      for ( j = 0 ; j < HOMDIM ; j++ )
      { td->view[i][j] = tdd->view[i][j]; 
        td->to_focus[i][j] = tdd->to_focus[i][j];
        td->from_focus[i][j] = tdd->from_focus[i][j];
      }
    }
    dup_window = 0; 
  }
  else
  { for ( i = 0 ; i < HOMDIM ; i++ )
    { td->view[i] = td->viewspace[i];
      td->to_focus[i] = td->to_focus_space[i];
      td->from_focus[i] = td->from_focus_space[i];
      for ( j = 0 ; j < HOMDIM ; j++ )
        td->view[i][j] = view[i][j];  /* initialize to global view */
    }
    matcopy(td->to_focus,identmat,HOMDIM,HOMDIM);
    matcopy(td->from_focus,identmat,HOMDIM,HOMDIM);
  }
  LEAVE_GRAPH_MUTEX;

  glDepthFunc(GL_LEQUAL);   
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_NORMALIZE); 
  glEnable(GL_COLOR_MATERIAL);
  glColorMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE);
  /*glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, mat_specular); */
  /*glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, mat_shininess); */
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, mat_white);
  /*glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, mat_emission);  */
  glEnable(GL_LIGHTING);
  glLightfv(GL_LIGHT0, GL_POSITION, light0_position);  
  glLightfv(GL_LIGHT0, GL_DIFFUSE, light0_diffuse);  
  glLightfv(GL_LIGHT0, GL_AMBIENT, light0_ambient);  
  glLightfv(GL_LIGHT1, GL_POSITION, light1_position);
  glLightfv(GL_LIGHT1, GL_DIFFUSE, light1_diffuse);
  glLightfv(GL_LIGHT0, GL_SPECULAR, light_none); 
  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE,GL_TRUE);
  
  /* screen corners */
  td->scrx[0] = td->scrx[1] = -1.4;
  td->scrx[2] = td->scrx[3] =  1.4;
  td->scry[0] = td->scry[3] =  1.4;
  td->scry[1] = td->scry[2] = -1.4;
  
  go_display_flag = 1; /* default, since fast graphics */

  /* version check */
  strncpy(opengl_version,(char*)glGetString(GL_VERSION),sizeof(opengl_version));
  if ( strcmp(opengl_version,"1.1") >= 0 ) 
     td->arraysflag = 1;

  glutDisplayFunc(draw_screen);


  GL_ERROR_CHECK
  if ( !graph_thread_running ) 
  {
  
#ifndef WIN32
   signal(SIGKICK,glut_catcher);
#endif

    graph_thread_running = 1;
    glutMainLoop();
  } 
#ifdef PTHREADS
  return NULL;
#endif
}
 
/************************************************************************
*
* function: init_Oglz()
*
* purpose: start up graphics thread 
*/

void init_Oglz()
{ 
  if ( !initz_flag )
  { initz_flag = 1;

    if ( !no_graphthread_flag )
    {
#ifdef WIN32
     _beginthread(draw_thread,0,0);
#elif defined(PTHREADS)
   { pthread_t th;
    /*
    {
      sigemptyset(&newset);
      sigaddset(&newset,SIGKICK);
      pthread_sigmask(SIG_BLOCK,&newset,NULL);
    }
    */
    main_pid = getpid();
#ifdef MAC_OS_X
    /* GLUT doesn't work as secondary thread on 10.0.2, so main draws */
    curdir = getcwd(NULL,0);  /* so command thread gets proper directory */
    pthread_create(&th,NULL,(void*(*)(void*))mac_exec_commands,NULL);
    draw_thread(NULL);
#else
    pthread_create(&th,NULL,draw_thread,NULL);
#endif
   }
#else
    kb_error(2447,"Internal error: Evolver compiled with GLUT graphics without threading.\n",RECOVERABLE);
#endif
    }
  }
}

/****************************************************************
*
*  function: Oglz_start()
*
*  purpose: To be called at the start of each Evolver-initiated redraw.
*/
void Oglz_start ARGS((void));
void Oglz_start(void)
{
  if ( initz_flag == 0 ) init_Oglz();

}    

  
/******************************************************************
*
* function: Oglz_edge()
*
* purpose:  graph one edge.
*/
void Oglz_edge ARGS((struct graphdata *,edge_id));

void Oglz_edge ARGS2((g,e_id),
struct graphdata *g,
edge_id e_id)
{ struct graph_thread_data *td = GET_DATA;
  int k;
  int e_color;

  e_color = g[0].ecolor;
  if ( e_color == CLEAR ) return;

  if ( my_own_pick_flag )
  { my_own_edge_pick(g,g+1,e_id);
    return;
  }

  if ( (e_color < 0) || (e_color >= IRIS_COLOR_MAX) )
    e_color = DEFAULT_EDGE_COLOR;

  if ( pick_flag )
    my_glLoadName(e_id); /* for picking */

  /* display */ 
  e_glColor(td,e_color);
  if ( !td->arraysflag )
        glBegin(GL_LINES);
  for ( k = 0 ; k < 2 ; k++ )
     e_glVertex3dv(td,g[k].x);
  if ( !td->arraysflag )
        glEnd();

  /* pickable vertices */
  if ( pick_flag )
  { for ( k = 0 ; k < 2 ; k++ )
    { my_glLoadName(g[k].v_id); /* for picking */
      glBegin(GL_POINTS);
        glVertex3dv(g[k].x);
      glEnd();
    }
  }

 
}

/******************************************************************
*
* function: Oglz_facet()
*
* purpose:  graph one facet.
*/


void Oglz_facet ARGS((struct graphdata *,facet_id));

void Oglz_facet ARGS2((g,f_id),
struct graphdata *g,
facet_id f_id)
{  
  int i,k;
  REAL len;
  facetedge_id fe;
  struct graph_thread_data *td = GET_DATA;
  float norm[3];
  float backnorm[3];


  /* need normal for lighting */
  for ( i = 0 ; i < 3 ; i++ )
  { int ii = (i+1)%3;
    int iii = (i+2)%3;
    norm[i] = (float)((g[1].x[ii]-g[0].x[ii])*(g[2].x[iii]-g[0].x[iii])
             -  (g[1].x[iii]-g[0].x[iii])*(g[2].x[ii]-g[0].x[ii]));
  }
  len = sqrt(dotf(norm,norm,3));
  if ( len <= 0.0 ) return;
  for ( i = 0 ; i < 3 ; i++ ) norm[i]= (float)(norm[i]/len);

  if ( web.hide_flag && (g[0].color != UNSHOWN) 
           && td->facetshow_flag )
  { if ( pick_flag ) my_glLoadName(f_id); /* for picking */
    if ( g[0].color != CLEAR )
    { if ( my_own_pick_flag )
        my_own_facet_pick(g,f_id);
      else
      {
        if ( color_flag ) f_glColor(td,g->color);
        else f_glColor(td,INDEX_TO_RGBA(WHITE));
        kb_glNormal3fv(td,norm);
        if ( !td->arraysflag )
          glBegin(GL_TRIANGLES);
        for ( k = 0 ; k < 3 ; k++ )
        { if ( td->normflag ) 
            kb_glNormal3dv(td,g[k].norm);
          f_glVertex3dv(td,g[k].x);
        }
        if ( !td->arraysflag )
          glEnd();
      }
    }
    if ( my_own_pick_flag && g->backcolor != g->color && g->backcolor != CLEAR )
      my_own_facet_pick(g,f_id);
    else
    if ( (g->color != g->backcolor) && (g->backcolor != CLEAR) )
    { REAL x[MAXCOORD];
      f_glColor(td,g->backcolor);
      for ( i = 0 ; i < 3 ; i++ ) 
        backnorm[i] = -norm[i];
      kb_glNormal3fv(td,backnorm);
      if ( !td->arraysflag )
        glBegin(GL_TRIANGLES);
      for ( k = 2 ; k >= 0 ; k-- )
      { for ( i = 0 ; i < 3 ; i++ )
        x[i] = g[k].x[i] + thickness*backnorm[i];
        if ( td->normflag ) 
           kb_glAntiNormal3dv(td,g[k].norm);
        f_glVertex3dv(td,x);
      }
      if ( !td->arraysflag )
        glEnd();
    }
  }
  
  fe = valid_id(f_id) ? get_facet_fe(f_id) : NULLID;          
  for ( k = 0 ; k < 3 ; k++, fe = valid_id(fe)?get_next_edge(fe):NULLID )
  { if ( g[k].ecolor == CLEAR ) continue;
    if ( !edgeshow_flag || (g[0].color == UNSHOWN) )
    { if ( (g[k].etype & EBITS) == INVISIBLE_EDGE ) continue;      
    }
    if ( my_own_pick_flag )
    { my_own_edge_pick(g+k,g+((k+1)%3),g[k].id);
        continue;
    }


    if ( pick_flag ) 
	    my_glLoadName(g[k].id); /* for picking */
    e_glColor(td,g[k].ecolor);
    kb_glNormal3fv(td,norm);
    if ( !td->arraysflag )
        glBegin(GL_LINES);
      if ( td->normflag ) 
        kb_glNormal3dv(td,g[k].norm);
      e_glVertex3dv(td,g[k].x);
      if ( td->normflag ) 
        kb_glNormal3dv(td,g[(k+1)%3].norm);
      e_glVertex3dv(td,g[(k+1)%3].x);
    if ( !td->arraysflag )
        glEnd();
	if ( pick_flag )
	{ my_glLoadName(g[k].v_id);
	  glBegin(GL_POINTS);
        glVertex3dv(g[k].x);
      glEnd();
	  my_glLoadName(g[(k+1)%3].v_id);
	  glBegin(GL_POINTS);
        glVertex3dv(g[(k+1)%3].x);
      glEnd();
	}
   }

} /* end Oglz_facet() */


/**********************************************************************
*
* function: Oglz_end()
*
* purpose: to be called at end of presenting data.
*/

void Oglz_end ARGS((void))
{
  prev_timestamp = graph_timestamp;
}

void Ogl_close ARGS((void))
{ int i;
  struct graph_thread_data *td = GET_DATA;

  glutDestroyWindow(td->win_id);
  memset((char*)td,0,sizeof(struct graph_thread_data));

#ifndef MAC_OS_X
 /* Mac objects to closing graphics thread */

  for ( i = 0 ; i < MAXGRAPHWINDOWS ; i++ )
  { if ( gthread_data[i].in_use != 0 ) break;
  }
  if ( i == MAXGRAPHWINDOWS )
  { /* all graph windows closed */
    glDeleteLists(dindex,1);
    go_display_flag = 0;
    init_flag = 0;
    initz_flag = 0;
    td->arrays_timestamp = 0;
    graph_thread_running = 0;
  
#ifdef WIN32
  ExitThread(0);
#elif defined(PTHREADS)
  pthread_exit(NULL);
#endif
  }
#endif

}

/***********************************************************************
*
* function: Ogl_close_show()
*
* purpose: to be called by main thread when wanting to close graphics.
*          sets close_flag to be read by threads' idle_func().
*/
void Ogl_close_show(void)
{ close_flag = 1;
}

/**********************************************************************
*
* function: vercolcomp()
*
* purpose: comparison function for sorting vertex data in case of indexed arrays.
*
*/

int vercolcomp ARGS2((a,b),
struct vercol *a, struct vercol *b)
{ 
  int i;
  for ( i = 0 ; i < 10 ; i++ )
  { REAL diff = ((float*)a)[i] - ((float*)b)[i];
    if ( diff < -gleps ) return -1;
    if ( diff > gleps ) return 1;
  }
  return 0;
}

/***********************************************************************
*
* function: eecomp()
*
* purpose: comparison function for sorting indexed edges 
*/

int eecomp ARGS2((a,b),
int *a, int *b)
{ if ( *a < *b ) return -1;
  if ( *a > *b ) return 1;
  if ( a[1] > b[1] ) return 1;
  if ( a[1] < b[1] ) return -1;
  return 0;
}

/******************************************************************************
*
* function: declare_arrays()
*
* purpose: Declare element arrays to current OpenGL context.  Each thread
*          should check before drawing if arrays have changed since last 
*          declaration.
*/
void declare_arrays()
{ struct graph_thread_data *td = GET_DATA;

  GL_ERROR_CHECK

  if ( td->arraysflag )
  {
    /* declare arrays to OpenGL */
    if ( td->interleaved_flag )
    {
     glInterleavedArrays(GL_C4F_N3F_V3F,sizeof(struct vercol),(void*)td->fullarray);
    }
    else /* indexed */
    { 
      glColorPointer(4,GL_FLOAT,0,td->colorarray);     
      glNormalPointer(GL_FLOAT,sizeof(struct vercol),td->fullarray->n);
      glVertexPointer(3,GL_FLOAT,sizeof(struct vercol),td->fullarray->x);
    }
  }
  td->declared_timestamp = td->arrays_timestamp;
  GL_ERROR_CHECK
}

/**************************************************************************
*
* function: draw_one_image()
*
* purpose: draw one image of surface. separated out so can be
*          called twice in stereo mode.
*/
void draw_one_image()
{ struct graph_thread_data *td = GET_DATA;

  if ( td->dlistflag )
    glCallList(dindex);    
  else if ( td->arraysflag )
  { int i,j,m;

  GL_ERROR_CHECK

    for ( m = 0 ; (m < transform_count) || (m < 1) ; m++ )
    { float tmat[4][4];  /* transform matrix in proper form */

      if ( transforms_flag && td->doing_lazy && view_transform_det 
                                  && (view_transform_det[m] == -1.0) )
      { /* have to flip normals */
        for ( i = 0 ; i < td->edgecount+td->facetcount ; i++ )
         for ( j = 0 ; j < 3 ; j++ )
          td->fullarray[i].n[j] *= -1.0;
      }

      if ( transforms_flag && td->doing_lazy && view_transforms )
      { int hi = (SDIM <= 3) ? SDIM : 3;
        for ( i = 0 ; i < hi; i++ )
        { for ( j = 0 ; j < hi; j++ )
            tmat[i][j] = (float)view_transforms[m][j][i];
          for ( ; j < 3 ; j++ ) tmat[i][j] = 0.0;
          tmat[i][3] = (float)view_transforms[m][SDIM][i];
        }
        for ( ; i < 3 ; i++ )
        { for ( j = 0 ; j < 4 ; j++ ) tmat[i][j] = 0.0;
          tmat[i][i] = 1.0;
        }
        for ( j = 0 ; j < hi ; j++ )
          tmat[3][j] = (float)view_transforms[m][j][SDIM];
        for ( ; j < 3 ; j++ )
          tmat[3][j] = 0.0;
        tmat[3][3] = (float)view_transforms[m][SDIM][SDIM];
      
        glMatrixMode(GL_MODELVIEW);
        glPushMatrix();
        glMultMatrixf((float*)tmat); 
      }

  GL_ERROR_CHECK

      if ( td->strips_flag )
      { int i;
        /* do facets first, then edges in front */
        for ( i = td->estripcount ; i < td->stripcount ; i++ )
          glDrawElements(td->striparray[i].mode,td->striparray[i].count,
                        GL_UNSIGNED_INT,td->stripdata+td->striparray[i].start);
        glMatrixMode(GL_PROJECTION);
        glTranslated(td->edge_bias*imagescale,0.0,0.0);
        for ( i = 0 ; i < td->estripcount ; i++ )
          glDrawElements(td->striparray[i].mode,td->striparray[i].count,
                        GL_UNSIGNED_INT,td->stripdata+td->striparray[i].start);
        glTranslated(-td->edge_bias*imagescale,0.0,0.0);
      }     
      else if ( td->indexing_flag )
      {
        glDrawElements(GL_TRIANGLES,td->facetcount,GL_UNSIGNED_INT,
           td->indexarray+td->facetstart);
        glMatrixMode(GL_PROJECTION);
        glTranslated(td->edge_bias*imagescale,0.0,0.0);
glDisable(GL_LIGHTING);
        glDrawElements(GL_LINES,td->edgecount,GL_UNSIGNED_INT,td->indexarray+td->edgestart);
glEnable(GL_LIGHTING);
        glTranslated(-td->edge_bias*imagescale,0.0,0.0);
      }
      else
      {
        glDrawArrays(GL_TRIANGLES,td->facetstart,td->facetcount);
        glMatrixMode(GL_PROJECTION);
        glTranslated(td->edge_bias*imagescale,0.0,0.0);
        glDrawArrays(GL_LINES,td->edgestart,td->edgecount);
        glTranslated(-td->edge_bias*imagescale,0.0,0.0);
      }
      if ( transforms_flag && td->doing_lazy && view_transforms )
      { glMatrixMode(GL_MODELVIEW); glPopMatrix(); }

      if ( transforms_flag &&  td->doing_lazy && view_transform_det 
                 && (view_transform_det[m] == -1.0) )
      { /* have to flip normals back */
        for ( i = 0 ; i < td->edgecount+td->facetcount ; i++ )
         for ( j = 0 ; j < 3 ; j++ )
          td->fullarray[i].n[j] *= -1.0;
      }

      if ( !td->doing_lazy  || !transforms_flag || !view_transforms ) break;
    } /* end transform loop */
  } /* end arraysflag */
  else /* not using display list or arrays */
  {
    ENTER_GRAPH_MUTEX
    normflag = td->normflag;
    graphgen();
    LEAVE_GRAPH_MUTEX
  }
  GL_ERROR_CHECK
} /* end draw_one_image() */

/**********************************************************************
*
* function: draw_screen()
* 
* purpose: Handle redraw messages from operating system.
*/

void draw_screen()
{ struct graph_thread_data *td = GET_DATA;
 
  Matrix viewf;
  int i,j;
 
  if ( td->aspect_flag )
    reshape_func(td->xsize,td->ysize);

#ifdef quickresize
  if ( td->resize_flag && !td->idle_flag ) /* Mac OS X resize recombine */
  { glutIdleFunc(idle_func);
    return; 
  }
  td->resize_flag = 0;
#endif

  td->idle_flag = 0;

  if ( td->new_title_flag )
    glutSetWindowTitle(td->wintitle);
  td->new_title_flag = 0;

#ifdef __cplusplus
  try
  {
#else
  /* Set up longjmp to return here in case of error  */
  if ( setjmp(graphjumpbuf) )
  { return; }
#endif

  GL_ERROR_CHECK
  
  /* New view loading point to try to eliminate problem with first load
     of very small surfaces. Works.  It needs to have view matrix loaded
     before setting arrays so it knows the proper rounding scale.
  */
  
  /* make sure global view matrix is same as first window */
  if ( td->win_id == 1 )
  { 
    for ( i = 0 ; i < HOMDIM ; i++ )
      for ( j = 0 ; j < HOMDIM ; j++ )
        view[i][j] = td->view[i][j];
  }


  glMatrixMode(GL_MODELVIEW);
  for ( i = 0 ; i < 4 ; i++ )
    for ( j = 0 ; j < 4 ; j++ )
      viewf[i][j] = td->view[(j<3)?j:(SDIM)][(i<3)?i:(SDIM)];
  if ( SDIM == 2 ) 
  { for ( i = 0 ; i < 3 ; i++ ) viewf[i][2] = viewf[2][i] = 0.0;
    viewf[2][2] = 1.0;
  }
  /* transpose, picking first 3 coordinates */
  if ( (td->projmode == P_PERSP) || td->stereomode ) 
  {   if ( SDIM == 2 ) viewf[3][2] -= 16;
      else viewf[3][0] -= 16.0;
  } 
  GL_ERROR_CHECK


  glLoadMatrixd(viewf[0]); 
  /* end new view load */
  
  /* build arrays if needed */
  if ( td->arraysflag && (
      ((graph_timestamp != td->arrays_timestamp) && go_display_flag )
         || td->newarraysflag || (td->dlistflag == RESETLIST))
     )
  { /* if long time since last build, block and wait */


   
    if ( build_arrays() )
    {
      declare_arrays();
   
      /* doing this here since facet_alpha_flag set in graphgen */
      if ( facet_alpha_flag ) glEnable(GL_BLEND);
      else glDisable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
      
      if ( td->indexing_flag ) 
      { 
        make_indexlists();
        if ( td->q_flag )
        { sprintf(msg,"After indexing: %d unique vertices, %d unique edges\n",
              td->vertexcount,td->edgecount/2);
          outstring(msg);
        }
        if ( td->strips_flag ) make_strips();
      }

      /* Workaround really bizarre line-drawing bug */
      if ( td->linewidth == 1.0 ) 
      { td->linewidth = (float)1.1; 
        glLineWidth(td->linewidth); 
      }
      GL_ERROR_CHECK
  
      if ( !td->interleaved_flag )
      { /* kludge for broken nVidia Detonater 2.08 driver */
        if ( td->colorarray ) free((char*)td->colorarray);
        td->colorarray = (float*)calloc(td->edgecount+td->facetcount,4*sizeof(float));
        for ( i = 0 ; i < td->edgecount+td->facetcount ; i++ )
          for ( j = 0 ; j < 4 ; j++ )
            td->colorarray[4*i+j] = td->fullarray[i].c[j];
        declare_arrays();
      }
  
    GL_ERROR_CHECK
  
  
      td->newarraysflag = 0;
      if ( td->dlistflag )
      {
        declare_arrays();
        glNewList(dindex,GL_COMPILE);
        if ( td->indexing_flag )
        {
          glDrawElements(GL_TRIANGLES,td->facetcount,GL_UNSIGNED_INT,td->indexarray+td->facetstart);
          glMatrixMode(GL_PROJECTION);
          glTranslated(td->edge_bias*imagescale,0.0,0.0);   /* edges in front */
          glDrawElements(GL_LINES,td->edgecount,GL_UNSIGNED_INT,td->indexarray+td->edgestart);
          glTranslated(-td->edge_bias*imagescale,0.0,0.0);
          glMatrixMode(GL_MODELVIEW);
        }
        else
        {
          glDrawArrays(GL_TRIANGLES,td->facetstart,td->facetcount); 
          glMatrixMode(GL_PROJECTION);
          glTranslated(td->edge_bias*imagescale,0.0,0.0);    /* edges in front */
          glDrawArrays(GL_LINES,td->edgestart,td->edgecount);
          glTranslated(-td->edge_bias*imagescale,0.0,0.0);
          glMatrixMode(GL_MODELVIEW);
       }
        glEndList();           
        td->dlistflag = NORMALLIST;
      }
      if ( td->q_flag ) outstring(current_prompt);  
     }
     else 
       glutPostRedisplay();
  }
  else
  if ( ((td->dlistflag != NORMALLIST) || 
        ((graph_timestamp != prev_timestamp) && go_display_flag ))
     )
  { 
    /* if long time since last build, block and wait */
    static REAL last_mutex_time;
    REAL now = get_internal_variable(V_CLOCK);
    int timeout = (now-last_mutex_time > .5) ? LONG_TIMEOUT : IMMEDIATE_TIMEOUT;
    if ( TRY_GRAPH_MUTEX(timeout) )
    { /* regenerate display list */
      graph_start = Oglz_start;
      graph_facet = Oglz_facet;
      graph_edge  = Oglz_edge;
      graph_end = Oglz_end;
    
      init_graphics = Ogl_init;
      finish_graphics = Ogl_finish;
      close_graphics = Ogl_close_show;
      last_mutex_time = now;
 
      if ( td->dlistflag != NOLIST )
      { glNewList(dindex,GL_COMPILE);
        td->facetcount = td->edgecount = 0;
        normflag = td->normflag;
        graphgen();
        glEndList();
        td->dlistflag = NORMALLIST;
      }
      END_TRY_GRAPH_MUTEX
    }
   else 
     glutPostRedisplay();
  }

  if ( SDIM != td->olddim )
  { reshape_func(td->xsize,td->ysize);  /* in case dimension changes */
    td->olddim = SDIM;
  }

  if ( web.sdim > 2 )
  {  glEnable(GL_LIGHT0);
     glEnable(GL_LIGHTING);
  }
  else 
     glDisable(GL_LIGHTING);

  /*glEnable(GL_LIGHT1); */

  GL_ERROR_CHECK

  /* clear screen and zbuffer */
  glClearColor(rgba[background_color][0], 
          rgba[background_color][1],
          rgba[background_color][2],    
          rgba[background_color][3]
          );
         
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );

  if ( backcull_flag ) { glEnable(GL_CULL_FACE); }
  else { glDisable(GL_CULL_FACE); }

  glDepthFunc(GL_LEQUAL); /* so later things get drawn */

#ifdef GGG
original loading of view matrix
  /* make sure global view matrix is same as first window */
  if ( td->win_id == 1 )
  { 
    for ( i = 0 ; i < HOMDIM ; i++ )
      for ( j = 0 ; j < HOMDIM ; j++ )
        view[i][j] = td->view[i][j];
  }

  glMatrixMode(GL_MODELVIEW);
  for ( i = 0 ; i < 4 ; i++ )
    for ( j = 0 ; j < 4 ; j++ )
      viewf[i][j] = td->view[(j<3)?j:(SDIM)][(i<3)?i:(SDIM)];
  if ( SDIM == 2 ) 
  { for ( i = 0 ; i < 3 ; i++ ) viewf[i][2] = viewf[2][i] = 0.0;
    viewf[2][2] = 1.0;
  }
  /* transpose, picking first 3 coordinates */
  if ( (td->projmode == P_PERSP) || td->stereomode ) 
  {   if ( SDIM == 2 ) viewf[3][2] -= 16;
      else viewf[3][0] -= 16.0;
  } 
  GL_ERROR_CHECK


  glLoadMatrixd(viewf[0]); 
  #endif
  
  /* for picking */
  if ( !td->arraysflag ) { glInitNames();  glPushName(0);
   
  /*{int depth;
    glGetIntegerv(GL_NAME_STACK_DEPTH,&depth);
    printf("OpenGL name stack depth: %d\n",depth);

  }
  */

  }

  if ( td->arraysflag )
    if ( td->declared_timestamp < td->arrays_timestamp )
      declare_arrays();

  /* Now the actual drawing */
  if ( td->stereomode )
  { int w = (SDIM==2) ? 0 : 1;
    /* Stereo mode always perspective */
    glMatrixMode(GL_MODELVIEW);
    viewf[3][w] -= 1.5; glLoadMatrixd(viewf[0]); draw_one_image();
    glMatrixMode(GL_MODELVIEW);
    viewf[3][w] += 3.0; glLoadMatrixd(viewf[0]); draw_one_image();
  }
  else draw_one_image();

  if ( display_text_count )
    glut_text_display();  /* draw text lines */ 

  glFlush();
  glutSwapBuffers();  

  temp_free_all();  /* should free just graphics thread temps */

#ifdef __cplusplus
  }
  catch(...)
  {
  }
#endif


  glutIdleFunc(idle_func);

} /* end draw_screen() */

/************************************************************************
*
* function:  idle_func()
* 
* purpose: Mac OS X kludge to prevent excessive redisplays on 
*          mouse movement.
*/
void idle_func()
{ struct graph_thread_data *td = GET_DATA;

  if ( close_flag ) 
     Ogl_close();

  td->idle_flag = 1;
#ifdef quickresize
  if ( td->resize_flag ) 
     glutPostRedisplay();
  td->resize_flag = 0;
#endif
  if ( draw_pid != main_pid ) /* can use signals */
    glutIdleFunc(NULL); 
  /* else relying on idle_func to pick up redraw messages */
  else
  {
#if (defined(SUN) || defined(LINUX)) && !defined(__STRICT_ANSI__)
    /* let the thread could sleep .02 sec; need link with -lrt */
    struct timespec delay;
    delay.tv_sec = 0; delay.tv_nsec = 20000000;
#ifdef SUN
    __nanosleep(&delay,NULL); 
#else
    nanosleep(&delay,NULL); 
#endif
#endif

#ifdef WIN32
    /* sleep until some event */
    { HANDLE pHandles[MAXIMUM_WAIT_OBJECTS];
      MsgWaitForMultipleObjects(0,pHandles,FALSE,100,QS_ALLINPUT);
    }
#endif
  }
}

/***********************************************************************
*
* function: display()
*
* purpose: called by Evolver to display on screen.
*/
void display()
{ struct graph_thread_data *td;
  int i,j;

  close_flag = 0;
  Oglz_start();
  
  /* set first screen view to anything modified by main thread */
  for ( i = 0 ; i < HOMDIM ; i++ )
  { gthread_data[1].view[i] = gthread_data[1].viewspace[i];
    for ( j = 0 ; j < HOMDIM ; j++ )
      gthread_data[1].view[i][j] = view[i][j];
  }

  for ( i = 0, td = gthread_data ; i < MAXGRAPHWINDOWS ; i++, td++ )
    if ( td->win_id )
    { if ( window_aspect_ratio != td->window_aspect )
        td->aspect_flag = 1;
 
#ifdef MAC_OS_X
      /* glutPostRedisplay() generates ugly NSEvent leak messages */
  /*      glutSetWindow(td->win_id); */
     /*   draw_screen();  Conflict with graphics thread! */
         
      glutPostWindowRedisplay(td->win_id);  /* generate redraw message */    
#else
/*   WARNING: glutSetWindow() in non-drawing thread causes problems. */
/*      glutSetWindow(td->win_id); */
 /*     glutPostRedisplay(); */  /* generate redraw message */ 
      glutPostWindowRedisplay(td->win_id);  /* generate redraw message */
#ifdef WIN32
      InvalidateRect(td->draw_hwnd,NULL,FALSE);  /* give draw thread a kick */
#else
     if ( draw_pid != main_pid )
     { 
       kill(draw_pid,SIGKICK);  /* unix version of kick */
     } 
#endif
#endif
    }

}



/****************************************************************************
* 
* function: make_strips()
*
* purpose: Converts indexed arrays of edges and triangles to strips
*          for more efficient plotting.
* Input: global array indexarray
*        global variables edgecount,facetcount
* Output:  striparray
*/ 

int ecomp ARGS2((a,b),
char *a, int *b)
{ struct graph_thread_data *td = GET_DATA;
  int ai = td->indexarray[td->edgestart+*(int*)a]; 
  int bi = td->indexarray[td->edgestart+*(int*)b]; 
  if ( ai < bi ) return -1;
  if ( bi < ai ) return 1;
  return 0;
}

struct festruct { int v[3];  /* head and tail and opposite vertices */
                  int f;     /* facet on left */
                };

int fecomp  ARGS2(( aa,bb),
void *aa, void*bb)
{
  struct festruct *a = (struct festruct *)aa;
  struct festruct *b = (struct festruct *)bb;
  if ( a->v[0] < b->v[0] ) return -1;
  if ( a->v[0] > b->v[0] ) return  1;
  if ( a->v[1] < b->v[1] ) return -1;
  if ( a->v[1] > b->v[1] ) return  1;
  return 0;
}


void make_strips()
{ int *edgeinx; /* oriented edge numbers, sorted by first vertex */
  int *evlist;  /* vertex indexed list of offsets into edgeinx */
  int v;
  int i,k,kk;
  int *estripno; /* number of current strip */
  int *fstripno; /* number of current strip */
  int stripnum;
  int dataspot;
  struct festruct *felist;
  int striplength[3];  /* for testing 3 ways from each starting facet */
  int *trialstrip;  /* for unerasing markings */
  int bestlength;
  int *bestverts;
  int *bestfacets;
  int way;
  int bestway;
  struct graph_thread_data *td = GET_DATA;

  /* strip arrays */
  if ( td->stripdata ) free((char*)td->stripdata);
  if ( td->striparray ) free((char*)td->striparray);
  td->stripdata = (int*)calloc(td->edgecount+td->facetcount+5,sizeof(int));
  td->striparray = (struct stripstruct *)calloc(td->edgecount/2+td->facetcount/3 + 10,
                                         sizeof(struct stripstruct));
  dataspot = 0; stripnum = 0;

  /* edges */
  /* make list indexed by vertex */
  /* in einx, entry is 2*edgeindex+orientationbit */
  edgeinx = (int *)temp_calloc(td->edgecount,sizeof(int));
  for ( i = 0 ; i < td->edgecount ; i++ )
  { edgeinx[i] = i;  } /* using bit for orientation */  
  qsort((void*)edgeinx,td->edgecount,sizeof(int),FCAST ecomp);
  /* find where individual vertex segments start */
  evlist = (int *)temp_calloc(td->edgecount+10,sizeof(int));
  for ( i = 0, v = 0 ; i < td->edgecount ; i++ )
  { while ( td->indexarray[td->edgestart+edgeinx[i]] > v ) evlist[++v] = i;
  }
  evlist[++v] = i;  /* last sentinel */

  /* now make strips.  start with some edge and just keep going. */
  estripno = (int*)temp_calloc(td->edgecount+5,sizeof(int));
  for ( i = 0 ; i < td->edgecount/2 ; i++ )
  { int nexte,headv;
    if ( estripno[i] ) continue;
    /* new strip */
    td->striparray[stripnum].mode = GL_LINE_STRIP;
    td->striparray[stripnum].start = dataspot;
    nexte = 2*i;
    for (;;)
    { 
      estripno[nexte>>1] = stripnum+1;
      headv = td->indexarray[td->edgestart+(nexte^1)];
      td->stripdata[dataspot++] = headv;
      for ( k = evlist[headv] ; k < evlist[headv+1] ; k++ )
      { if ( estripno[edgeinx[k]>>1] == 0 )
        { nexte = edgeinx[k]; 
          break;
        }
      }
      if ( k == evlist[headv+1] ) break; /* end of strip */
    }
    /* flip list around */
    for ( k = td->striparray[stripnum].start, kk = dataspot-1 ; k < kk ; k++,kk-- )
      { int temp = td->stripdata[k]; td->stripdata[k] = td->stripdata[kk]; 
        td->stripdata[kk] = temp;
      }
    /* now backwards */
    nexte = 2*i+1;
    for (;;)
    {
      estripno[nexte>>1] = stripnum+1; 
      headv = td->indexarray[td->edgestart+(nexte^1)];
      td->stripdata[dataspot++] = headv;
      for ( k = evlist[headv] ; k < evlist[headv+1] ; k++ )
      { if ( estripno[edgeinx[k]>>1] == 0 )
        { nexte = edgeinx[k]; 
          break;
        }
      }
      if ( k == evlist[headv+1] ) break; /* end of strip */
    }
    td->striparray[stripnum].count = dataspot - td->striparray[stripnum].start;

    stripnum++;
  }

  td->estripcount = stripnum;
  temp_free((char*)evlist);
  temp_free((char*)estripno); 
  temp_free((char*)edgeinx); 

  /* facets */

  /* make list of edges with left-hand facets */
  felist = (struct festruct *)temp_calloc(td->facetcount,sizeof(struct festruct));
  for ( i = 0, k = 0 ; i < td->facetcount ; i += 3, k++ )
  { felist[i].v[0] = td->indexarray[td->facetstart+i];
    felist[i].v[1] = td->indexarray[td->facetstart+i+1];
    felist[i].v[2] = td->indexarray[td->facetstart+i+2];
    felist[i].f = k;
    felist[i+1].v[0] = td->indexarray[td->facetstart+i+1];
    felist[i+1].v[1] = td->indexarray[td->facetstart+i+2];
    felist[i+1].v[2] = td->indexarray[td->facetstart+i];
    felist[i+1].f = k;
    felist[i+2].v[0] = td->indexarray[td->facetstart+i+2];
    felist[i+2].v[1] = td->indexarray[td->facetstart+i];
    felist[i+2].v[2] = td->indexarray[td->facetstart+i+1];
    felist[i+2].f = k;
  }
  qsort((void*)felist,td->facetcount,sizeof(struct festruct),FCAST fecomp);

  fstripno = (int *)temp_calloc(td->facetcount+5,sizeof(int));
  trialstrip = (int *)temp_calloc(td->facetcount+5,sizeof(int));
  bestverts = (int *)temp_calloc(td->facetcount+5,sizeof(int));
  bestfacets = (int *)temp_calloc(td->facetcount+5,sizeof(int));

  /* now make strips.  start with some facet and just keep going. */
  for ( i = 0 ; i < td->facetcount/3 ; i++ )
  { int nextf,va,vb,vc,whichway;
    int firstcount,secondcount; /* for checking orientation at start */
    if ( fstripno[i] ) continue;
    /* new strip */
    td->striparray[stripnum].mode = GL_TRIANGLE_STRIP;
    td->striparray[stripnum].start = dataspot;
    bestlength = 0;
    for ( way = 0 ; way < 3 ; way++)
    { int m = 0;  /* trialstrip index */
      dataspot = td->striparray[stripnum].start;
      nextf = 3*i;
      td->stripdata[dataspot++] = va = td->indexarray[td->facetstart+nextf+((1+way)%3)];
      td->stripdata[dataspot++] = vb = td->indexarray[td->facetstart+nextf+way];
      whichway = 1;
      for (;;)
      { struct festruct *fe,key;
        /* find in felist */
        if ( whichway ) { key.v[0] = va; key.v[1] = vb; }
        else { key.v[1] = va; key.v[0] = vb; }
        fe = (struct festruct *)bsearch(&key,(void*)felist,td->facetcount,
            sizeof(struct festruct), FCAST fecomp);
        if ( fe==NULL ) break;  /* done; maybe hit edge of surface */
  
        /*see if facet done yet */
        if ( fstripno[fe->f] != 0 ) break;  /* done in this direction */

        /*add opposite vertex */
        vc = fe->v[2];
        td->stripdata[dataspot++] = vc;                                 
        fstripno[fe->f] = stripnum+1;
        trialstrip[m++] = fe->f;
  
        /* ready for next time around */
        va = vb;
        vb = vc;
        whichway = !whichway;
      }
      firstcount = dataspot - td->striparray[stripnum].start;
      /* flip list around */
      for ( k = td->striparray[stripnum].start, kk = dataspot-1 ; k < kk ; k++,kk-- )
      { int temp = td->stripdata[k]; td->stripdata[k] = td->stripdata[kk]; 
        td->stripdata[kk] = temp;
      }
      /* now backwards */
      va = td->indexarray[td->facetstart+nextf+way];
      vb = td->indexarray[td->facetstart+nextf+((way+1)%3)];
      whichway = 1;
      for (;;)
      { struct festruct *fe,key;
        /*find in felist */
        if ( whichway ) { key.v[0] = va; key.v[1] = vb; }
        else { key.v[1] = va; key.v[0] = vb; }
        fe = (struct festruct*)bsearch(&key,(void*)felist,td->facetcount,
          sizeof(struct festruct), FCAST fecomp);
        if ( fe==NULL ) break;  /* done; maybe hit edge of surface */

        /*see if facet done yet */
        if ( fstripno[fe->f] != 0 ) break;  /* done in this direction */

        /*add opposite vertex */
        vc = fe->v[2];
        td->stripdata[dataspot++] = vc;
        fstripno[fe->f] = stripnum+1;
        trialstrip[m++] = fe->f;
        /* ready for next time around */
        va = vb;
        vb = vc;
        whichway = !whichway;
      }
      striplength[way] = dataspot - td->striparray[stripnum].start;
      secondcount = striplength[way] - firstcount;

      /* check orientation at start */
      if ( firstcount & 1 )
      { if ( secondcount & 1 ) 
        { striplength[way]--;  /* omit last, if necessary */
          if ( i == trialstrip[m-1] ) i--;  /* so loop doesn't skip omitted facet */
        }
        /* flip order */
        for ( k = td->striparray[stripnum].start, 
            kk = td->striparray[stripnum].start+striplength[way]-1 ; k < kk ; k++,kk-- )
        { int temp = td->stripdata[k]; td->stripdata[k] = td->stripdata[kk]; 
          td->stripdata[kk] = temp;
        }
      }

      if ( striplength[way] > bestlength )
      { bestlength = striplength[way];
        bestway = way;
        memcpy(bestverts,td->stripdata+td->striparray[stripnum].start,bestlength*sizeof(int));
        memcpy(bestfacets,trialstrip,(bestlength-2)*sizeof(int));
      }
      for ( k = 0 ; k < m ; k++ )  /* unmark */
        fstripno[trialstrip[k]] = 0;
    }  /* end ways */

    memcpy(td->stripdata+td->striparray[stripnum].start,bestverts,bestlength*sizeof(int));
    for ( k = 0 ; k < bestlength-2 ; k++ )  /* remark */
    { fstripno[bestfacets[k]] = stripnum+1;
      if ( td->strip_color_flag && (bestfacets[k] < web.skel[FACET].maxcount) ) 
        set_facet_color(bestfacets[k],(stripnum % 14) + 1);
    }

    td->striparray[stripnum].count = bestlength;
    dataspot = td->striparray[stripnum].start + bestlength;

    stripnum++;
  }

  temp_free((char*)fstripno); 
  temp_free((char*)felist); 
  temp_free((char*)trialstrip); 
  temp_free((char*)bestverts); 
  temp_free((char*)bestfacets); 

  td->stripcount = stripnum;
  td->fstripcount = td->stripcount - td->estripcount;
  /* cut down arrays to needed size */
  td->stripdata = (int*)realloc((char*)td->stripdata,dataspot*sizeof(int));
  td->striparray = (struct stripstruct *)realloc((char*)td->striparray,
                     td->stripcount*sizeof(struct stripstruct));
  if ( td->q_flag )
  { sprintf(msg,"After stripping: %d edgestrips, %d facetstrips\n",
              td->estripcount,td->fstripcount);
    outstring(msg);
  }
}

/***************************************************************************
*
* function: hashfunc()
*
* purpose: Compute hash value for vertex.
*/
int hashsize; /* size of hashtable */
int hashfunc ARGS1((a),
struct vercol *a)
{
  int h;
  int scale = 100000;
  double eps = 3.e-6;  /* to prevent coincidences */
  h = 15187*(int)(scale*a->x[0]+eps);
  h += 4021*(int)(scale*a->x[1]+eps);
  h += 2437*(int)(scale*a->x[2]+eps);
  h += 7043*(int)(scale*a->n[0]+eps);
  h += 5119*(int)(scale*a->n[1]+eps);
  h += 8597*(int)(scale*a->n[2]+eps);
  h += 1741*(int)(scale*a->c[0]+eps);
  h += 4937*(int)(scale*a->c[1]+eps);
  h += 1223*(int)(scale*a->c[2]+eps);
  h = h % hashsize;
  if ( h < 0 ) h += hashsize;
  return h;
}
/***************************************************************************
*
* function: make_indexlists()
*
* purpose: Uniquify vertex and edge lists, and create index array for
*          OpenGL.  Also needed for strips.
*/

void make_indexlists()
{ struct graph_thread_data *td = GET_DATA;
  int i,j;
  int rawcount = td->edgecount+td->facetcount;  /* number of unsorted vertices */
  struct vercol **hashlist;
  float mat[4][4];

  /* get reasonable epsilon for identifying vertices */
  glGetFloatv(GL_MODELVIEW_MATRIX,mat[0]);
  gleps = 1e-5/sqrt(mat[0][0]*mat[0][0]+mat[0][1]*mat[0][1]
                       +mat[0][2]*mat[0][2]);
  
  /* Uniquify using hash table */
  /* qsort here is a time hog */
  if ( td->indexarray ) free((char*)td->indexarray);
  td->indexarray = (int*)calloc(rawcount+10,sizeof(int));
  if ( !td->fullarray ) return;
  hashsize = 2*rawcount + 10;
  hashlist = (struct vercol**)calloc(hashsize,sizeof(struct vercol *));
  hashlist[hashfunc(td->fullarray)] = td->fullarray;
  td->indexarray[0] = 0;
  for ( i = 1, j = 1 ; i < rawcount ; i++ )
  { int h = hashfunc(td->fullarray+i);
    while ( hashlist[h] && vercolcomp(hashlist[h],td->fullarray+i) ) 
    { h++; if ( h == hashsize ) h = 0; }
    if ( hashlist[h] == NULL ) /* new one */
    { 
      td->fullarray[j] = td->fullarray[i];
      hashlist[h] = td->fullarray+j;
      j++;
    }
    td->indexarray[i] = hashlist[h]-td->fullarray;
  } 
  free((char*)hashlist);
  td->vertexcount = j;


   /* Uniquify edges */
   for ( i = td->edgestart ; i < td->edgestart+td->edgecount ; i += 2 )
   { if ( td->indexarray[i] > td->indexarray[i+1] )
     { int temp = td->indexarray[i];
       td->indexarray[i] = td->indexarray[i+1];
       td->indexarray[i+1] = temp;
     }
   }
   /* qsort here relatively minor in time */
   qsort((void*)(td->indexarray+td->edgestart),td->edgecount/2,2*sizeof(int), FCAST eecomp);
   for ( i = 2, j = 0 ; i < td->edgecount ; i += 2 )
   { if ( eecomp(td->indexarray+td->edgestart+i,td->indexarray+td->edgestart+j) != 0 )
     { j += 2;
       if ( i > j )
       { td->indexarray[td->edgestart+j] = td->indexarray[td->edgestart+i];
         td->indexarray[td->edgestart+j+1] = td->indexarray[td->edgestart+i+1];
       }
     }
   }
   if ( td->edgecount ) td->
     edgecount = j+2;
} /* end make_indexlists() */

/******************************************************************************
*
* Function: build_arrays()
*
* Purpose: Construct arrays of graphics data.
*
* Return: 1 if success, 0 if failure
*/

int build_arrays()
{  static REAL last_mutex_time;
   struct graph_thread_data *td = GET_DATA;
   REAL now = get_internal_variable(V_CLOCK);
   int timeout = (now-last_mutex_time > .5) ? LONG_TIMEOUT : IMMEDIATE_TIMEOUT;

#ifdef MPI_EVOLVER
  if ( this_task == MASTER_TASK )
  { mpi_get_task_graphics(td->mpi_graph_task);
    return 1;
  }
  else
#endif
    if ( TRY_GRAPH_MUTEX(timeout) )
    { int oldflag;
   
      
      /* see if already have current arrays in some other thread */
  /*      int i;
  for ( i = 0 ; i < MAXGRAPHWINDOWS ; i++ )
        if ( td != gthread_data + i 
        #ifdef MPI_EVOLVER
                 && td->mpi_graph_task == gthread_data[i].mpi_graph_task 
        #endif
               && td->arrays_timestamp < gthread_data[i].arrays_timestamp )
         { td->fullarray = gthread_data[i].fullarray;  
           td->edgestart = gthread_data[i].edgestart; 
           td->edgecount = gthread_data[i].edgecount;   
           td->facetstart = gthread_data[i].facetstart;   
           td->facetcount = gthread_data[i].facetcount; 
           td->arrays_timestamp = gthread_data[i].arrays_timestamp;   
           td->fullarray_original = 0; 
           return 1; 
         }    
		 */
  
      last_mutex_time = now;
      if ( td->fullarray && td->fullarray_original )
      { free((char*)td->fullarray);     
        td->fullarray = NULL;
      }
      td->edgecount = 0; 
      td->edgemax = (web.representation==SIMPLEX) ? SDIM*web.skel[FACET].count + 100 :
           4*web.skel[EDGE].count+10;   /* 2 vertices per edge, each edge twice */
      td->edgearray = (struct vercol *)calloc(td->edgemax,sizeof(struct vercol));
      td->facetcount = 0; td->facetmax = 3*web.skel[FACET].count+10; /* 3 vertices per facet */
      td->facetarray = (struct vercol *)calloc(td->facetmax,sizeof(struct vercol));
      if ( !td->edgearray || !td->facetarray )
      { erroutstring("Cannot allocate memory for graphics.\n");
        return 0;
      }
  
      graph_start = Oglz_start;
      graph_facet = Oglz_facet;
      graph_edge  = Oglz_edge;
      graph_end = Oglz_end;
      init_graphics = Ogl_init;
      finish_graphics = Ogl_finish;
      close_graphics = Ogl_close_show;
  
      /*glDisableClientState(GL_COLOR_ARRAY); */
      /*glDisableClientState(GL_NORMAL_ARRAY); */
      /*glDisableClientState(GL_VERTEX_ARRAY); */
      
      oldflag = markedgedrawflag;
      markedgedrawflag = 1;
      if ( (td->mouse_mode != MM_SLICE) && !transform_colors_flag && (SDIM <= 3)) 
         lazy_transforms_flag = 1;   /* for graphgen use */
      td->doing_lazy = lazy_transforms_flag;  /* for glutgraph use */
      td->arrays_timestamp = graph_timestamp = ++global_timestamp; /* prevent stale data */ 
      normflag = td->normflag;
      graphgen();   /* fill in arrays */
      td->doing_lazy = lazy_transforms_flag;  /* for glutgraph use, in case graphgen() changed */
      END_TRY_GRAPH_MUTEX
  
      lazy_transforms_flag = 0;
      markedgedrawflag = oldflag;
      if ( td->q_flag )
      { sprintf(msg,"\n%d edges, %d facets\n",td->edgecount/2,td->facetcount/3);
        outstring(msg);
      }
 
  
      /* unify lists */
  
      td->fullarray = (struct vercol *)realloc((char*)td->edgearray,
                 (td->edgecount+td->facetcount)*sizeof(struct vercol));       
      td->fullarray_original = 1;
      
      memcpy((char*)(td->fullarray+td->edgecount),(char*)td->facetarray,
           td->facetcount*sizeof(struct vercol));
      free((char*)td->facetarray);  td->facetarray = NULL; td->edgearray = NULL;
      td->edgestart = 0; td->facetstart = td->edgecount;      
     
      return 1; /* success */
   }
   else return 0;

}  /* end build_arrays() */

#ifdef MPI_EVOLVER

/***********************************************************************
*
* Function: mpi_get_task_graphics()
*
* Purpose: Fetch graphics data from task in array format suitable for OpenGL.
*          Called by master task graphics module.
*
*/

void mpi_get_task_graphics(task)
int task;  /* which task to get from  */
{ struct mpi_command message;
  MPI_Status status;
  struct graph_metadata metadata; 
  struct graph_thread_data *td = GET_DATA;
  int i,j;

  ENTER_MPI_MUTEX;

  message.cmd = mpi_GET_GRAPHICS;
  message.task = task;
/*
  MPI_Send(&message,sizeof(struct mpi_command),MPI_BYTE,task,GRAPHICS_TAG,
    mpi_comm_graphics);
*/
  MPI_Bcast(&message,sizeof(struct mpi_command),MPI_BYTE,MASTER_TASK,
        MPI_COMM_WORLD);
  if ( mpi_debug )
    printf("Task %d requested graphics from task %d\n",this_task,task);
  
  MPI_Recv(&metadata,sizeof(struct graph_metadata),MPI_BYTE,task,GRAPHICS_TAG,
         MPI_COMM_WORLD, &status);

  if ( mpi_debug )
    printf("Got metadata\n");

  td->edgestart = metadata.edgestart;
  td->edgecount = metadata.edgecount;
  td->facetstart = metadata.facetstart;
  td->facetcount = metadata.facetcount;
  if ( !td->view_initialized )
    for ( i = 0 ; i < 4 ; i++ )
      for ( j = 0 ; j < 4 ; j++ )
        td->view[i][j] = metadata.view[i][j];
  td->view_initialized = 1;
  
  /* allocate room for graphics data */
  td->fullarray = realloc(td->fullarray,(td->facetstart+td->facetcount)*sizeof(struct vercol)); 

  MPI_Recv(td->fullarray,(td->facetstart+td->facetcount)*sizeof(struct vercol),
     MPI_BYTE,task,GRAPHICS_TAG,MPI_COMM_WORLD,&status); 
  if ( mpi_debug )
    printf("Got graphics data, edgecount %d facetcount %d\n",td->edgecount,
       td->facetcount);

  td->arrays_timestamp = graph_timestamp;
  MPI_Barrier(MPI_COMM_WORLD);

  LEAVE_MPI_MUTEX;
}

/**********************************************************************
*
* Function: mpi_task_send_graphics()
*
* Purpose: Task handler for mpi_get_task_graphics().  Sends graphics
*          data array back to master task for display.
*/

void mpi_task_send_graphics()
{ struct graph_metadata metadata;
  int i,j;
  struct graph_thread_data *td = GET_DATA;
  
  memset(&metadata,0,sizeof(metadata));
  
  if ( mpi_debug )
    printf("Task %d got request for graphics.\n",this_task);
  gthread_data[0].facetshow_flag = 1; /* kludge */
  td->arraysflag = 1;
  color_flag = 1;
  for ( i = 0 ; i < 16 ; i++ )
    for ( j = 0 ; j < 4 ; j++ )
      rgba[i][j] = (float)rgb_colors[i][j];

  no_graphthread_flag = 1;
  build_arrays();
  no_graphthread_flag = 0;
  metadata.edgecount = td->edgecount;
  metadata.edgestart = td->edgestart;
  metadata.facetcount = td->facetcount;
  metadata.facetstart = td->facetstart;
  for ( i = 0 ; i < 4 ; i++ )
    for ( j = 0 ; j < 4 ; j++ )
      metadata.view[i][j] = view[i][j];

  if ( mpi_debug )
    printf("Sending metadata\n");
  MPI_Send(&metadata,sizeof(metadata),MPI_BYTE,MASTER_TASK,GRAPHICS_TAG,
    MPI_COMM_WORLD);
   
  if ( mpi_debug )
    printf("Sending graph data\n");
  MPI_Send(td->fullarray,(td->facetstart+td->facetcount)*sizeof(struct vercol),
    MPI_BYTE,MASTER_TASK,GRAPHICS_TAG,MPI_COMM_WORLD);

} /* end mpi_task_send_graphics() */

#endif
