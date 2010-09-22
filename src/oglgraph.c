/*********************************************************************
*
*  File:  oglgraph.c
*
*  Contents:  OpenGL display for Surface Evolver
*             For OpenGL Version 1.1
*/

/* This runs in a separate thread from the main program, so it avoids
   calling kb_error, since that would longjump to the wrong place.
   Except for WARNINGS, which are OK since kb_error just returns.
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
  
#if defined(WIN32) || defined(_WIN32)
#undef FIXED
#include <GL\glaux.h>
HWND hwnd;
#else
#include <GL/aux.h>
#define CALLBACK 
#endif

#include "include.h"

static char opengl_version[20]; /* from glGetString */
int close_flag = 0; /* whether close_show has been done */

vertex_id focus_vertex_id;    /* rotate and zoom focus */

static float rgba[16][4]; 


static long win_id;  /* graphics window id */
#ifdef TC
static void *th; /* draw_thread handle */
#else
static unsigned long th;
#endif

/* screen corners */
static double scrx[4],scry[4];
static double aspect = 1;
static double xscale=2.8/400,yscale=2.8/400;
static GLsizei xsize,ysize; /* window size */
static int initz_flag = 0;  /* z buffer */
static float projmat[16];  /* for saving projection matrix */
#define P_ORTHO 0
#define P_PERSP 1
static int projmode = P_ORTHO;    /* kind of projection to do */
#define NO_STEREO 0
#define CROSS_STEREO 1
static int stereomode = NO_STEREO;
#define NOLIST     0
#define NORMALLIST 1
#define RESETLIST  2
static int dlistflag = NOLIST;  /* whether to use display list */
static int facetshow_flag = 1; /* whether to show facets */
static GLfloat linewidth = 1.0;
static REAL edge_bias = 0.005; /* amount edges drawn in front */
static int arraysflag=0; /* whether to use OpenGL 1.1 arrays */
struct vercol { float c[4]; float n[3]; float x[3]; int inx; };
static struct vercol *fullarray;
static int amax; /* allocated size of final array */
static int edgestart,edgecount,facetstart,facetcount;
static int vertexcount;
static struct vercol *edgearray,*facetarray;
static int emax,fmax; /* allocated */
static long arrays_timestamp; /* for remembering surface version */
static int interleaved_flag = 1; /* whether to do arrays as interleaved */
static int indexing_flag = 1; /* whether to use indexed arrays (smaller,but random access) */
static int *indexarray;
static int strips_flag; /* whether to do GL strips */
static int strip_color_flag; /* whether to color facets according to strip number */
static void make_strips(void);
static void make_indexlists(void);
struct stripstruct { int mode;  /* GL_TRIANGLE_STRIP or GL_LINE_STRIP */
                     int start; /* starting offset in indexarray */
                     int count; /* number of vertices in strip */
                   };
static struct stripstruct *striparray;
static int stripcount;  /* size of striparray */
static int estripcount; /* number of edge strips */
static int fstripcount; /* number of facet strips */
static int *stripdata;
static int doing_lazy;  /* whether oglgraph should do transforms itself */
static int q_flag;  /* whether to print drawing stats */

static REAL gleps = 1e-5; /* tolerance for identifying vertices */
// Note: using indexed arrays seems not to have any particular benefits.

/********************************************************************
*
* function: enlarge_edge_array()
*
* purpose: Expand the space for the edge list
*/

void enlarge_edge_array(void)
{ int more = emax + 10;

  edgearray = (struct vercol*)kb_realloc((char*)edgearray,
                    (emax+more)*sizeof(struct vercol));
  emax += more;
}

/********************************************************************
*
* function: enlarge_facet_array()
*
* purpose: Expand the space for the facet list
*/

void enlarge_facet_array(void)
{ int more = web.skel[FACET].count + 10;

  facetarray = (struct vercol*)kb_realloc((char*)facetarray,
                     (fmax+more)*sizeof(struct vercol));
  fmax += more;
}

/***********************************************************************
*
* function: kb_glNormal3fv() etc.
*
* purpose: Either save current normal in kb_norm[] for later list entry,
*          or pass it on to gl.
*/

static float kb_norm[3] = { 0.0, 0.0, 1.0 }; /* state of normal vector */

void kb_glNormal3fv(float *v) /* save for edges and facets */
{ if ( !arraysflag ) glNormal3fv(v);
  else { kb_norm[0] = v[0]; kb_norm[1] = v[1]; kb_norm[2] = v[2]; }
}
void kb_glNormal3dv(REAL *v) /* save for edges and facets */
{ kb_norm[0] = (float)v[0]; kb_norm[1] = (float)v[1]; kb_norm[2] = (float)v[2]; 
  if ( !arraysflag ) glNormal3fv(kb_norm);
}
void kb_glAntiNormal3dv(REAL *v) /* save for edges and facets */
{ kb_norm[0] = -(float)v[0]; kb_norm[1] = -(float)v[1]; kb_norm[2] = -(float)v[2]; 
  if ( !arraysflag ) glNormal3fv(kb_norm);
}

/***********************************************************************
*
* function: e_glColor()
*
* purpose: Either save current edge color in er,eg,eb for later list entry,
*          or pass it on to gl.
*/
static float er,eg,eb,ea; /* current edge color */
void e_glColor(int c) 
{ 
  if ( edge_rgb_color_attr > 0 )
  {
    if ( !arraysflag ) glColor4ubv((const GLubyte*)&c);
    else 
    { er=(float)(((c>>24)&0xFF)/255.); eg=(float)(((c>>16)&0xFF)/255.); 
      eb=(float)(((c>>8)&0xFF)/255.); ea= (float)(((c)&0xFF)/255.); 
    }
  }
  else 
  { if ( !arraysflag ) glColor4fv(rgba[c]);
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
void e_glVertex3dv(double *x)
{ if ( !arraysflag ) { glVertex3dv(x);    return; }
  if ( edgecount > emax-5 ) enlarge_edge_array();
  edgearray[edgecount].c[0] = er;
  edgearray[edgecount].c[1] = eg;
  edgearray[edgecount].c[2] = eb;
  edgearray[edgecount].c[3] = ea;
  edgearray[edgecount].x[0] = (float)x[0];
  edgearray[edgecount].x[1] = (float)x[1];
  edgearray[edgecount].x[2] = (float)x[2];
  edgearray[edgecount].n[0] = kb_norm[0];
  edgearray[edgecount].n[1] = kb_norm[1];
  edgearray[edgecount].n[2] = kb_norm[2];
  edgecount++;
}
/***********************************************************************
*
* function: f_glColor()
*
* purpose: Either save current facet color in fr,fg,fb for later list entry,
*          or pass it on to gl.
*/
static float fr,fg,fb,fa; /* current facet color */
void f_glColor(int c) 
{ 
  if ( facet_rgb_color_attr > 0 )
  {
    if ( !arraysflag ) glColor4ubv((const GLubyte*)&c);
    else 
    { fr=(float)(((c>>24)&0xFF)/255.); fg=(float)(((c>>16)&0xFF)/255.);
      fb=(float)(((c>>8)&0xFF)/255.); fa=(float)(((c)&0xFF)/255.); 
    }
  }
  else 
  { if ( !arraysflag ) glColor4fv(rgba[c]);
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
void f_glVertex3dv(double *x)
{ if ( !arraysflag ) { glVertex3dv(x); return; }
  if ( facetcount > fmax-5 ) enlarge_facet_array();
  facetarray[facetcount].c[0] = fr;
  facetarray[facetcount].c[1] = fg;
  facetarray[facetcount].c[2] = fb;
  facetarray[facetcount].c[3] = fa;
  facetarray[facetcount].x[0] = (float)x[0];
  facetarray[facetcount].x[1] = (float)x[1];
  facetarray[facetcount].x[2] = (float)x[2];
  facetarray[facetcount].n[0] = kb_norm[0];
  facetarray[facetcount].n[1] = kb_norm[1];
  facetarray[facetcount].n[2] = kb_norm[2];
  facetcount++;
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
static int mouse_mode = MM_ROTATE;
static int newarraysflag = 0; /* to cause rebuild of arrays */
static int dindex = 1;  /* display list object index */

void CALLBACK draw_screen ARGS((void));

void Ogl_init(void)
{ 
}

void Ogl_finish(void)
{                
}

void graph_new_surface(void)
{ /* to account for global deallocation at start of new surface */
  fullarray = NULL;
  edgearray = NULL;
  facetarray = NULL;
  indexarray = NULL;
  striparray = NULL;
  stripdata = NULL;
}

void init_Oglz ARGS((void));
 
int oldx,oldy,newx,newy;  /* for tracking motion */
int basex=0,basey=0;  /* translation to do on object */

/************************************************************************
*
* function: pick_func()
*
* purpose: To handle mouse picking of element.  Invoked by right click.
*/

#define PICKBUFLENGTH 500
GLuint pickbuf[PICKBUFLENGTH];

void CALLBACK pick_func(AUX_EVENTREC *rec)
{ GLint hits, viewport[4];
  int i,n;
  unsigned int enearz = 0xFFFFFFFF;
  unsigned int fnearz = 0xFFFFFFFF;
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
  gluPickMatrix(rec->data[0],rec->data[1],4,4,viewport);
   
  glMultMatrixf(projmat);
  if ( SDIM >= 3 ) glMultMatrixd(flip[0]);    /* don't know why, but need */
  else glMultMatrixd(flip2D[0]);

  if ( arraysflag )       /* no picking during arrays */
  { arraysflag = 0; graph_timestamp = ++global_timestamp; 
    draw_screen();
    arraysflag = 1; graph_timestamp = ++global_timestamp;
  } else
  draw_screen();

  hits = glRenderMode(GL_RENDER); /* back to ordinary drawing */
  if ( hits < 0 ) 
  { hits = PICKBUFLENGTH/4;  /* buffer overflow */
    kb_error(2541,
        "Pick buffer overflow; selected element may not be foreground element.\n",
        WARNING);
  }
  for ( i=0, n=0 ; (i < hits) && (n < PICKBUFLENGTH-4) ; i++, n += count + 3 )
  { 
      count = pickbuf[n];
      switch ( id_type(pickbuf[n+3]) )
      { case FACET: 
          if ( pickbuf[n+1] < fnearz ) 
          { f_id = pickbuf[n+3]; fnearz = pickbuf[n+1]; }
          break;
        case EDGE:
          if ( pickbuf[n+1] < enearz )
            if ( (pickbuf[n+3] != NULLEDGE) || !valid_id(e_id) )
             { e_id = pickbuf[n+3]; enearz = pickbuf[n+1]; }
          break;
      }
  }
  printf("\n");
  if ( valid_id(e_id) ) 
  { /* check for vertex in common to picked edges */
    for ( i = 0, n = 0 ; (i < hits) && (n < PICKBUFLENGTH-4) ; i++, n += count+3 )
    { element_id id,ee_id;
      int ii,nn,ccount;

      count = pickbuf[n];
      id = pickbuf[n+3];
      if ( (id_type(id) == EDGE) && valid_id(id) )
      { vertex_id v1 = get_edge_headv(id);
        vertex_id v2 = get_edge_tailv(id);
        for ( ii = i+1, nn = n+count+3 ; (ii < hits) && (n < PICKBUFLENGTH-4) ; ii++, nn += ccount + 3 )
        { ccount = pickbuf[nn];
          ee_id = pickbuf[nn+3];
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
    { pickvnum = loc_ordinal(v_id) + 1;
      sprintf(msg,"Picked vertex %d\n",pickvnum);
      outstring(msg); 
    }
    pickenum = loc_ordinal(e_id) + 1;
    sprintf(msg,"Picked edge %d\n",pickenum);
    outstring(msg);
  }
  else if ( e_id == NULLEDGE )
     outstring("Picked facet subdivision edge.\n");
  if ( valid_id(f_id) ) 
  { pickfnum = loc_ordinal(f_id) + 1;
    sprintf(msg,"Picked facet %d\n",pickfnum);
    outstring(msg);
  }
  outstring(current_prompt);

  glMatrixMode(GL_PROJECTION);
  glPopMatrix();
}  /* end pick_func */

/***********************************************************
*
* function: mouse_down_func()
*
* purpose: Called on left button down, records position.
*/
void CALLBACK mouse_down_func(AUX_EVENTREC *rec)
{ oldx = (short)rec->data[0]; oldy = (short)rec->data[1];
}

/***********************************************************
*
* function: mouse_up_func()
*
* purpose: Called on left button up, records position as base.
*/
void CALLBACK mouse_up_func(AUX_EVENTREC *rec)
{
  basex = newx;
  basey = newy;
}

/*******************************************************************
*

* function: mouse_loc_func()
*
* purpose: Called as mouse moves with left button down, this
*          moves surface according to current mouse_mode.
*/
void CALLBACK mouse_loc_func(AUX_EVENTREC *rec)
{ int i,j;

  newx = (short)rec->data[0];
  newy =(short)rec->data[1]; 
  switch ( mouse_mode )
  { case MM_ROTATE:    
       mat_mult(to_focus,view,view,HOMDIM,HOMDIM,HOMDIM);
       fix_ctm(view,(double)( newx-oldx),-(double)(newy - oldy));
       mat_mult(from_focus,view,view,HOMDIM,HOMDIM,HOMDIM);
       break;
    case MM_SCALE:
        mat_mult(to_focus,view,view,HOMDIM,HOMDIM,HOMDIM);

        for(i = 0 ; i < HOMDIM-1; i++ )
            for ( j = 0 ; j < HOMDIM ; j++ )
                view[i][j] *= 1.0 +0.002*(newx-oldx);
        mat_mult(from_focus,view,view,HOMDIM,HOMDIM,HOMDIM);
        break;
    case MM_SCALE: /* slicing altitude */
        slice_coeff[SDIM] += (newx-oldx)*xscale;
        break;
    case MM_TRANSLATE:
        if ( SDIM == 2 )
        { view[0][2] += (newx-oldx)*xscale;
          view[1][2] -= (newy-oldy)*yscale;
          to_focus[0][2] -= (newx-oldx)*xscale;
          to_focus[1][2] += (newy-oldy)*yscale;
          from_focus[0][2] += (newx-oldx)*xscale;
          from_focus[1][2] -= (newy-oldy)*yscale;
        } else
        {
          view[1][HOMDIM-1] += (newx-oldx)*xscale;
          view[2][HOMDIM-1] -= (newy-oldy)*yscale;
          to_focus[1][HOMDIM-1] -= (newx-oldx)*xscale;
          to_focus[2][HOMDIM-1] += (newy-oldy)*yscale;
          from_focus[1][HOMDIM-1] += (newx-oldx)*xscale;
          from_focus[2][HOMDIM-1] -= (newy-oldy)*yscale;
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
          dth = (newx - oldx)/300.0*M_PI;
          dang = (newy - oldy)/300.0*M_PI;
          if ( SDIM == 2 )
          { rot[0][0] = rot[1][1] = cos(dth);
            rot[0][1] = -(rot[1][0] = sin(dth));
          } else 
          { rot[1][1] = rot[2][2] = cos(dth);
            rot[1][2] = -(rot[2][1] = sin(dth));
          } 
          mat_mult(to_focus,view,view,HOMDIM,HOMDIM,HOMDIM);
          mat_mult(rot,view,view,HOMDIM,HOMDIM,HOMDIM);
          mat_mult(from_focus,view,view,HOMDIM,HOMDIM,HOMDIM);
        }
        break;
    }

  oldx = newx; oldy = newy;
}

/************************************************************************
*
* function: reshape_func()
*
* purpose: handle window resize messages.
*/
void CALLBACK reshape_func ( GLsizei x, GLsizei y )
{ xsize = x; ysize = y;
  aspect = (double)y/x;
  glViewport(0,0,x,y);
  if ( aspect > 1 ) { xscale = 2.8/aspect/x; yscale = 2.8/y; }
  else { xscale = 2.8/x; yscale = 2.8*aspect/y; }
  if ( x < y ) 
  { minclipx = -1.5; maxclipx = 1.5;
    minclipy = -1.5*y/x; maxclipy = 1.5*y/x;
  }
  else
  { minclipx = -1.5*x/y; maxclipx = 1.5*x/y;
    minclipy = -1.5; maxclipy = 1.5;
  }
  
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  
  if ( (projmode == P_PERSP) || stereomode )
  {     
     gluPerspective((float)(y/1600.*180/3.14159),1/aspect,1.0,20.0);
     if ( SDIM == 2 ) glMultMatrixd(vt2[0]); 
     else glMultMatrixd(vt3p[0]);  /* rotate axes */
  }
  else
  {
    if ( aspect <= 1.0 )
    { glOrtho(scrx[0],scrx[2],aspect*scry[0],aspect*scry[2],-20.0,20.0);
      minclipx = scrx[0]; maxclipx = scrx[2]; 
      minclipy = aspect*scry[2]; maxclipy = aspect*scry[0];
    }
    else
    { glOrtho(scrx[0]/aspect,scrx[2]/aspect,scry[0],scry[2],-20.0,20.0);
      minclipx = scrx[0]/aspect; maxclipx = scrx[2]/aspect; 
      minclipy = scry[2]; maxclipy = scry[0];
    }
 
    if ( SDIM == 2 ) glMultMatrixd(vt2[0]); /* upside down */
    else glMultMatrixd(vt3[0]);  /* upside down and rotate axes */
   }

  glGetFloatv(GL_PROJECTION_MATRIX,projmat); // save
}

/***********************************************************************
*
* Functions: key callbacks
*
* purpose: handle keystrokes in graphics window.
*/
void CALLBACK up_arrow (void) { view_transform("\036"); }
void CALLBACK down_arrow (void) { view_transform("\037"); }
void CALLBACK left_arrow (void) { view_transform("\035"); }
void CALLBACK right_arrow (void) { view_transform("\034"); }
void CALLBACK r_key (void) { mouse_mode = MM_ROTATE; }
void CALLBACK t_key (void) { mouse_mode = MM_TRANSLATE; }
void CALLBACK z_key (void) { mouse_mode = MM_SCALE; }
void CALLBACK c_key (void) { mouse_mode = MM_SPIN;}
void CALLBACK l_key (void) { if ( slice_view ) mouse_mode = MM_SLICE;}
void CALLBACK b_key (void)
{ edge_bias -= 0.001; 
   sprintf(msg,"\nEdge front bias now %f\n",edge_bias); outstring(msg);
   outstring(current_prompt);
}
void CALLBACK B_key (void)
{ edge_bias += 0.001; 
   sprintf(msg,"\nEdge front bias now %f\n",edge_bias); outstring(msg);
   outstring(current_prompt);
}
void CALLBACK R_key (void) 
{ 
  resize(); 
}
void CALLBACK minus_key (void) 
{ if ( linewidth > 0.6 ) { linewidth -= 0.5; glLineWidth(linewidth);} }
void CALLBACK plus_key (void) 
{ if ( linewidth < 9.9 ) { linewidth += 0.5; glLineWidth(linewidth);}} 
void CALLBACK e_key (void) 
{ edgeshow_flag = !edgeshow_flag; graph_timestamp = ++global_timestamp; 
  newarraysflag = 1;}
void CALLBACK f_key (void) 
{ facetshow_flag = !facetshow_flag; graph_timestamp = ++global_timestamp; 
  newarraysflag = 1;}
void CALLBACK g_key (void) 
{ normflag = !normflag; graph_timestamp = ++global_timestamp; 
  newarraysflag = 1;}
void CALLBACK m_key (void) { view_transform("m"); }
void CALLBACK i_key (void) { interleaved_flag = !interleaved_flag;
   outstring(interleaved_flag ? "Interleaving ON.\n":"Interleaving OFF.\n"); 
   outstring(current_prompt);
   newarraysflag = 1;
   }
void CALLBACK I_key (void) { indexing_flag = !indexing_flag;
   outstring(indexing_flag ? "Array indexing ON.\n":"Array indexing OFF.\n"); 
   outstring(current_prompt);
   newarraysflag = 1;
   }
void CALLBACK S_key (void) { strips_flag = !strips_flag;
   outstring(strips_flag ? "Element strips ON.\n":"Element strips OFF.\n"); 
   outstring(current_prompt);
   normflag = 1; /* gourard shading */
   indexing_flag = 1;
   newarraysflag = 1;
   }
void CALLBACK Y_key (void)   /* color strips */
   { int *fcolors;
     int i;
     fcolors = (int*)temp_calloc(web.skel[FACET].maxcount,sizeof(int));
     for ( i = 0 ; i < web.skel[FACET].maxcount ; i++ )
           fcolors[i] = get_facet_color(i);
     if ( !strips_flag ) S_key();  /* make sure strips on */
     strip_color_flag = 1;
     newarraysflag = 1;
     draw_screen();  /* get facets colored */
     newarraysflag = 1;
     draw_screen();  /* draw colored facets */
     strip_color_flag = 0;
     for ( i = 0 ; i < web.skel[FACET].maxcount ; i++ )
           set_facet_color(i,fcolors[i]);
     temp_free((char*)fcolors);
   }
void CALLBACK F_key (void) 
{
  if ( pickvnum > 0 )
  { int i,m;
    REAL *x,xx[MAXCOORD];
    REAL focus[MAXCOORD];

    focus_vertex_id = get_ordinal_id(VERTEX,pickvnum-1);
    x = get_coord(focus_vertex_id);  
    for ( m = 0 ; m < SDIM ; m++ ) xx[m] = x[m];
    if ( torus_display_mode == TORUS_CLIPPED_MODE )
    { 
      for ( m = 0 ; m < SDIM ; m++ )
      { int wrap = (int)floor(SDIM_dot(web.inverse_periods[m],x));
        for ( i = 0 ; i < SDIM ; i++ )
          xx[i] -= wrap*web.torus_period[m][i];
      }
    }

    matvec_mul(view,xx,focus,HOMDIM-1,HOMDIM-1);
    for ( i = 0 ; i < SDIM ; i++ ) 
    { to_focus[i][HOMDIM-1] = -focus[i] - view[i][HOMDIM-1];
      from_focus[i][HOMDIM-1] = focus[i] + view[i][HOMDIM-1];
    }
  }
}
void CALLBACK D_key (void) 
{ dlistflag = dlistflag ? NOLIST:RESETLIST; 
  if ( dlistflag ) outstring("\nOpenGL display list now ON.\n");
  else outstring("\nOpenGL display list now OFF.\n");
  outstring(current_prompt);
  graph_timestamp = ++global_timestamp; /* force recalculate arrays */
}

void CALLBACK a_key (void) 
{ if ( strcmp(opengl_version,"1.1") < 0 )
  { sprintf(errmsg,"Vertex arrays require OpenGL version at least 1.1. This is %s.\n",
                  opengl_version);
    kb_error(2542,errmsg,WARNING);
    return;
  }
  arraysflag = !arraysflag; 
  if ( arraysflag )
  { /* Workaround really bizarre line-drawing bug */
    if ( linewidth == 1.0 ) { linewidth = 0.5; glLineWidth(0.5); }
    outstring("\nOpenGL vertex arrays now ON.\n");
    glInterleavedArrays(GL_C4F_N3F_V3F,0,(void*)fullarray);
    newarraysflag = 1;
  }
  else
  {
    glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_NORMAL_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY);
    outstring("\nOpenGL vertex_arrays now OFF.\n");
  }
  outstring(current_prompt);
}

void CALLBACK s_key (void) 
{ stereomode = (stereomode==NO_STEREO) ? CROSS_STEREO:NO_STEREO; 
  if (stereomode)
  { if ( dlistflag == NOLIST ) dlistflag = RESETLIST;}
  else if ( arraysflag ) dlistflag = NOLIST;
  reshape_func(xsize,ysize);
 
}
void CALLBACK p_key (void) 
{ if ( stereomode ) 
    outstring("\nOpenGL projection now CROSS-EYED STEREO.\n"); 
  else if ( projmode==P_ORTHO ) 
  { projmode=P_PERSP; outstring("\nOpenGL projection now PERSPECTIVE.\n");}
  else 
  { projmode=P_ORTHO; outstring("\nOpenGL projection now ORTHOGONAL.\n");}
  outstring(current_prompt);
  reshape_func(xsize,ysize);
}

void CALLBACK Q_key (void)
{ q_flag = !q_flag;
  outstring(q_flag ? "\nPrinting drawing stats ON\n":"\nPrinting drawing stats OFF\n");
  outstring(current_prompt);
}


void CALLBACK help_key (void)
{ outstring("\nGraphics window help:\n");
  outstring("Left mouse: move   Right mouse: pick\n");
  outstring("Graphics window keys:\n");
  outstring(mouse_mode==MM_ROTATE?"r  Rotate mode for left mouse button, now ACTIVE\n":
      "r  Rotate mode for left mouse button\n");
  outstring(mouse_mode==MM_TRANSLATE?"t  Translate mode for left mouse button, now ACTIVE\n":
      "t  Translate mode for left mouse button\n");
  outstring(mouse_mode==MM_SCALE?"z  Zoom mode for left mouse button, now ACTIVE\n":
      "z  Zoom mode for left mouse button\n");
  outstring(mouse_mode==MM_SPIN?"c  Clockwise/counterclockwise mode, now ACTIVE\n":
      "c  Clockwise/counterclockwise mode\n");
  outstring("W  Widen edges\n");
  outstring("w  Narrow edges\n");
  outstring("B  Increase edge front bias by 0.001\n");
  outstring("b  Decrease edge front bias by 0.001\n");
  outstring(normflag? "g  Gourard shading (smooth shading) toggle, now ON\n":
             "g  Gourard shading (smooth shading) toggle, now OFF\n" );
  outstring("R  Reset view\n");
  outstring("m  Center image\n");
  outstring(edgeshow_flag?"e  Toggle showing all edges, now ON\n":
     "e  Toggle showing all edges, now OFF\n");
  outstring(facetshow_flag?"f  Toggle showing facets, now ON\n":
     "f  Toggle showing facets, now OFF\n");
  outstring(projmode==P_PERSP?"p  Toggle orthogonal/perspective projection, now perspective\n":
  "p  Toggle orthogonal/perspective projection, now orthogonal\n");
  outstring("s  Toggle cross-eyed stereo\n");
  outstring("F  Set rotate/zoom focus to last picked vertex\n");
  outstring("arrow keys  translate image\n");
  outstring("H  Guru-level help items\n");
  outstring(current_prompt);
}
void CALLBACK H_key(void)  /* guru level help */
{
  outstring("\nFollowing for fiddling with OpenGL drawing modes:\n");
  outstring(normflag? "g  Gourard shading (smooth shading) toggle, now ON\n":
             "g  Gourard shading (smooth shading) toggle, now OFF\n" );
  outstring(dlistflag?"D  Toggle using display list, now ON\n":
    "D  Toggle using display list, now OFF\n");
  outstring(arraysflag?"a  Toggle using vertex and color arrays, now ON\n"
        :"a  Toggle using vertex and color arrays, now OFF\n");
  outstring(indexing_flag ? "I  Indexed vertex arrays, now ON\n"
                           : "I  Indexed vertex arrays, now OFF\n");
  outstring(interleaved_flag ? "i  Interleaved vertex arrays, now ON\n"
                           : "i  Interleaved vertex arrays, now OFF\n");
  outstring(strips_flag?
    "S  Use element strips, now ON (indexed elements automatically turned on)\n"
    :"S  Use element strips, now OFF\n");
  outstring("Y  One-time coloring of strips generated in S mode.\n");
  outstring("       Caution: assumes display facets same as real, with no gaps.\n");
  outstring("Q  Toggle printing some statistics during drawing\n");
 
  outstring(current_prompt);
}

 /* lighting info for surface */
 static GLfloat mat_specular[] = {.5f,.5f,.5f,1.0f};
 static GLfloat mat_shininess[] = {10.0f};
 static GLfloat mat_diffuse[] = {1.0f,1.0f,1.0f,1.0f}; 
 static GLfloat mat_white[] = {1.0f,1.0f,1.0f,1.0f};
 static GLfloat mat_emission[] = {0.3f,0.3f,0.3f,1.0f};
#define INTENSITY1  0.5f
 static GLfloat light0_position[] = {1.0f,0.0f,1.0f,0.0f};  // front
 static GLfloat light0_diffuse[] = {INTENSITY1,INTENSITY1,INTENSITY1,1.0f};
 static GLfloat light0_ambient[] = {.3f,.3f,.3f,1.0f};

#define INTENSITY2  0.5f
 static GLfloat light1_position[] = {0.0f,0.0f,1.0f,0.0f};  // above
 static GLfloat light1_diffuse[] = {INTENSITY2,INTENSITY2,INTENSITY2,1.0f};
 static GLfloat light_none[] = {0.f,0.f,0.f,.0f};

 /*****************************************************************
 *
 * function: draw_thread()
 *
 * purpose: Create OpenGL display thread and window.
 */
//unsigned long __stdcall draw_thread(void *arglist)
void __cdecl draw_thread(void *arglist)
{ int i,j;

  draw_thread_id = GetCurrentThreadId();

  for ( i = 0 ; i < 16 ; i++ )
    for ( j = 0 ; j < 4 ; j++ )
      rgba[i][j] = (float)rgb_colors[i][j];
  background_color = LIGHTBLUE;

  auxInitPosition(0,0,400,400);
  auxInitDisplayMode(AUX_DOUBLE | AUX_RGBA | AUX_DEPTH);
  auxInitWindow(datafilename);
  auxMouseFunc(AUX_LEFTBUTTON,AUX_MOUSEUP,mouse_up_func); 
  auxMouseFunc(AUX_LEFTBUTTON,AUX_MOUSELOC,mouse_loc_func); 
  auxMouseFunc(AUX_LEFTBUTTON,AUX_MOUSEDOWN,mouse_down_func);
  auxMouseFunc(AUX_RIGHTBUTTON,AUX_MOUSEDOWN,pick_func);
  auxKeyFunc(AUX_UP,up_arrow);
  auxKeyFunc(AUX_LEFT,left_arrow);
  auxKeyFunc(AUX_RIGHT,right_arrow);
  auxKeyFunc(AUX_DOWN,down_arrow);
  auxKeyFunc(AUX_t,t_key);
  auxKeyFunc(AUX_z,z_key);
  auxKeyFunc(AUX_r,r_key);
  auxKeyFunc(AUX_l,l_key);
  auxKeyFunc(AUX_R,R_key);
  auxKeyFunc(AUX_D,D_key);
  auxKeyFunc(AUX_p,p_key);
  auxKeyFunc(AUX_s,s_key);
  auxKeyFunc(AUX_S,S_key);
  auxKeyFunc(AUX_Y,Y_key);
  auxKeyFunc(AUX_e,e_key);
  auxKeyFunc(AUX_f,f_key);
  auxKeyFunc(AUX_g,g_key);
  auxKeyFunc(AUX_F,F_key);
  auxKeyFunc(AUX_c,c_key);
  auxKeyFunc(AUX_B,B_key);
  auxKeyFunc(AUX_b,b_key);
  auxKeyFunc(AUX_W,plus_key); 
  auxKeyFunc(AUX_w,minus_key);
  auxKeyFunc(AUX_h,help_key);
  auxKeyFunc(AUX_H,H_key);
  auxKeyFunc(AUX_a,a_key);
  auxKeyFunc(AUX_m,m_key);
  auxKeyFunc(AUX_i,i_key);
  auxKeyFunc(AUX_I,I_key);
  auxKeyFunc(AUX_Q,Q_key);
  auxReshapeFunc(reshape_func);
  hwnd = auxGetHWND();

  ShowWindow(hwnd,SW_SHOW);
  SetWindowPos(hwnd,HWND_NOTOPMOST ,0,0,0,0,
          SWP_NOSIZE | SWP_NOMOVE);
  SetForegroundWindow(hwnd);

  glDepthFunc(GL_LEQUAL);   
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_NORMALIZE); 
  glEnable(GL_COLOR_MATERIAL);
  glColorMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE);
  //glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, mat_specular);
  //glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, mat_shininess);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, mat_white);
  //glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, mat_emission); 
  glEnable(GL_LIGHTING);
  glLightfv(GL_LIGHT0, GL_POSITION, light0_position);  
  glLightfv(GL_LIGHT0, GL_DIFFUSE, light0_diffuse);  
  glLightfv(GL_LIGHT0, GL_AMBIENT, light0_ambient);  
  glLightfv(GL_LIGHT1, GL_POSITION, light1_position);
  glLightfv(GL_LIGHT1, GL_DIFFUSE, light1_diffuse);
  glLightfv(GL_LIGHT0, GL_SPECULAR, light_none); 
  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE,GL_TRUE);
  
  /* screen corners */
  scrx[0] = scrx[1] = -1.4;
  scrx[2] = scrx[3] =  1.4;
  scry[0] = scry[3] =  1.4;
  scry[1] = scry[2] = -1.4;
  
  go_display_flag = 1; /* default, since fast graphics */

  /* version check */
  strncpy(opengl_version,glGetString(GL_VERSION),sizeof(opengl_version));
  if ( strcmp(opengl_version,"1.1") >= 0 ) arraysflag = 1;

  auxMainLoop(draw_screen);

  //return 0;
}
 
/************************************************************************
*
* function: init_Oglz()
*
* purpose: start up graphics thread
*/

#include <process.h>   
void init_Oglz(void)
{  int n;

  if ( !initz_flag )
  { initz_flag = 1;
    if ( !to_focus )
    { to_focus = dmatrix(0,MAXCOORD,0,MAXCOORD);
      from_focus = dmatrix(0,MAXCOORD,0,MAXCOORD);
      for ( n = 0 ; n <= MAXCOORD ; n++ )
        to_focus[n][n] = from_focus[n][n] = 1.0;
    }

    //th = CreateThread(NULL,0,draw_thread,0,0,&draw_thread_id);  
    th=_beginthread(draw_thread,0,0);

  }
}

/****************************************************************
*
*  function: Oglz_start()
*
*  purpose: To be called at the start of each Evolver-initiated redraw.
*/
void Oglz_start(void)
{
  if ( initz_flag == 0 ) init_Oglz();
  SetWindowText(hwnd,datafilename);
  if ( !to_focus )
    { int n;
      to_focus = dmatrix(0,MAXCOORD,0,MAXCOORD);
      from_focus = dmatrix(0,MAXCOORD,0,MAXCOORD);
      for ( n = 0 ; n <= MAXCOORD ; n++ )
        to_focus[n][n] = from_focus[n][n] = 1.0;
    }
}    

  
/******************************************************************
*
* function: Oglz_edge()
*
* purpose:  graph one edge.
*/

void Oglz_edge(
struct graphdata *g,
edge_id e_id)
{  
  int k;
  int e_color;

  e_color = g[0].ecolor;
  if ( e_color == CLEAR ) return;
  if ( (e_color < 0) || (e_color >= IRIS_COLOR_MAX) )
    e_color = DEFAULT_EDGE_COLOR;

  glLoadName(e_id); // for picking

  /* display */
  e_glColor(e_color);
  glBegin(GL_LINES);
  for ( k = 0 ; k < 2 ; k++ )
     e_glVertex3dv(g[k].x);
  glEnd();

}

/******************************************************************
*
* function: Oglz_facet()
*
* purpose:  graph one facet.
*/

static float norm[3];

void Oglz_facet(
struct graphdata *g,
facet_id f_id)
{  
  int i,k;
  REAL len;
  facetedge_id fe = NULLID;

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
           && facetshow_flag )
     { glLoadName(f_id); // for picking
       if ( g[0].color != CLEAR )
       { if ( color_flag ) f_glColor(g->color);
         else f_glColor(INDEX_TO_RGBA(WHITE));
         kb_glNormal3fv(norm);
         glBegin(GL_TRIANGLES);
         for ( k = 0 ; k < 3 ; k++ )
         { if ( normflag ) kb_glNormal3dv(g[k].norm);
            f_glVertex3dv(g[k].x);
         }
         glEnd();
       }
       if ( (g->color != g->backcolor) && (g->backcolor != CLEAR) )
       { REAL x[MAXCOORD];
           f_glColor(g->backcolor);
           for ( i = 0 ; i < 3 ; i++ ) norm[i] = -norm[i];
           kb_glNormal3fv(norm);
           glBegin(GL_TRIANGLES);
           for ( k = 2 ; k >= 0 ; k-- )
           { for ( i = 0 ; i < 3 ; i++ )
             x[i] = g[k].x[i] + thickness*norm[i];
             if ( normflag ) kb_glAntiNormal3dv(g[k].norm);
             f_glVertex3dv(x);
           }
           glEnd();
         }
     }

    fe = valid_id(f_id) ? get_facet_fe(f_id) : NULLID;          
    for ( k = 0 ; k < 3 ; k++, fe = valid_id(fe)?get_next_edge(fe):NULLID )
    { 
      if ( g[k].ecolor == CLEAR ) continue;
      if ( !edgeshow_flag || (g[0].color == UNSHOWN) )
        { if ( (g[k].etype&EBITS) == INVISIBLE_EDGE ) continue;      
        }
      glLoadName(g[k].id); /* for picking */
      e_glColor(g[k].ecolor);
      kb_glNormal3fv(norm);
      glBegin(GL_LINES);
      if ( normflag ) kb_glNormal3dv(g[k].norm);
      e_glVertex3dv(g[k].x);
      if ( normflag ) kb_glNormal3dv(g[(k+1)%3].norm);
      e_glVertex3dv(g[(k+1)%3].x);
      glEnd();
    }
}


/**********************************************************************
*
* function: Oglz_end()
*
* purpose: to be called at end of presenting data.
*/
void Oglz_end(void)
{
  prev_timestamp = graph_timestamp;
}

void Ogl_close(void)
{ //close_flag = 1;
  //InvalidateRect(hwnd,NULL,FALSE);
    kb_error(2175,"Sorry, OpenGL version can't restart show after close, so won't close.\n",
                WARNING);
graph_timestamp = ++global_timestamp;
draw_screen();
}

void really_close(void)
{
  auxCloseWindow(); 
  glDeleteLists(dindex,1);
  init_flag = 0;
  initz_flag = 0;
  win_id = -1;
  go_display_flag = 0;
  
  ExitThread(0);
}

/**********************************************************************
*
* function: vercolcomp()
*
* purpose: comparison function for sorting vertex data in case of indexed arrays.
*
*/

int vercolcomp(struct vercol *a, struct vercol *b)
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

int eecomp(const int *a, const int *b)
{ if ( *a < *b ) return -1;
  if ( *a > *b ) return 1;
  if ( a[1] > b[1] ) return 1;
  if ( a[1] < b[1] ) return -1;
  return 0;
}

/**********************************************************************
*
* function: draw_screen()
* 
* purpose: Handle redraw messages from operating system.
*/

void CALLBACK draw_screen(void)
{ static int olddim = 0;
  Matrix viewf;
  int i,j;
 
  ENTER_GRAPH_MUTEX

  // Set up longjmp to return here in case of error 
  if ( setjmp(graphjumpbuf) )
  { return; }

  //if ( close_flag ) really_close();     can't restart

  /* build arrays if needed */
  if ( arraysflag && (
      ((graph_timestamp != arrays_timestamp) && go_display_flag )
         || newarraysflag || (dlistflag == RESETLIST)) )
  { int oldflag;

    if ( fullarray )
      { myfree((char*)fullarray);     
        fullarray = NULL;
      }
    edgecount = 0; 
    emax = (web.representation==SIMPLEX) ? SDIM*web.skel[FACET].count + 100 :
          2*web.skel[EDGE].count+10;   // 2 vertices per edge
    edgearray = (struct vercol *)mycalloc(emax,sizeof(struct vercol));
    facetcount = 0; fmax = 3*web.skel[FACET].count+10; // 3 vertices per facet
    facetarray = (struct vercol *)mycalloc(fmax,sizeof(struct vercol));
    if ( !edgearray || !facetarray )
        { erroutstring("Cannot allocate memory for graphics.\n");
          goto bailout;
        }

    graph_start = Oglz_start;
    graph_facet = Oglz_facet;
    graph_edge  = Oglz_edge;
    graph_end = Oglz_end;
    init_graphics = Ogl_init;
    finish_graphics = Ogl_finish;
    close_graphics = Ogl_close;

    //glDisableClientState(GL_COLOR_ARRAY);
    //glDisableClientState(GL_NORMAL_ARRAY);
    //glDisableClientState(GL_VERTEX_ARRAY);
    
    oldflag = markedgedrawflag;
    markedgedrawflag = 1;
    if ( !transform_colors_flag ) 
       lazy_transforms_flag = 1;   /* for graphgen use */
    doing_lazy = lazy_transforms_flag;  /* for use by oglgraph.c */
    arrays_timestamp = graph_timestamp; /* here to prevent stale data */
    graphgen();   /* fill in arrays */
    lazy_transforms_flag = 0;
    markedgedrawflag = oldflag;
    if ( q_flag )
    { sprintf(msg,"\n%d edges, %d facets\n",edgecount/2,facetcount/3);
      outstring(msg);
    }

    /* doing this here since facet_alpha_flag set in graphgen */
    if ( facet_alpha_flag ) glEnable(GL_BLEND);
    else glDisable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);

    /* unify lists */
    fullarray = (struct vercol *)kb_realloc((char*)edgearray,
               (edgecount+facetcount)*sizeof(struct vercol)); 

    memcpy((char*)(fullarray+edgecount),(char*)facetarray,facetcount*sizeof(struct vercol));
    myfree((char*)facetarray);  facetarray = NULL; edgearray = NULL;
    edgestart = 0; facetstart = edgecount;      
   
    /* Workaround really bizarre line-drawing bug */
    if ( linewidth == 1.0 ) { linewidth = (float)1.1; glLineWidth(linewidth); }

    if ( indexing_flag ) 
    { 
      make_indexlists();
      if ( q_flag )
      { sprintf(msg,"After indexing: %d unique vertices, %d unique edges\n",
            vertexcount,edgecount/2);
        outstring(msg);
      }
      if ( strips_flag ) make_strips();
    }
    /* declare arrays to OpenGL */
    if ( interleaved_flag )
      glInterleavedArrays(GL_C4F_N3F_V3F,sizeof(struct vercol),(void*)fullarray);
    else // kludge for broken nVidia Detonater 2.08 driver
    { static float *colorarray;
      if ( colorarray ) myfree((char*)colorarray);
      colorarray = (float*)mycalloc(edgecount+facetcount,4*sizeof(float));
      for ( i = 0 ; i < edgecount+facetcount ; i++ )
        for ( j = 0 ; j < 4 ; j++ )
          colorarray[4*i+j] = fullarray[i].c[j];
      glColorPointer(4,GL_FLOAT,0,colorarray);     
      glNormalPointer(GL_FLOAT,sizeof(struct vercol),fullarray->n);
      glVertexPointer(3,GL_FLOAT,sizeof(struct vercol),fullarray->x);
    }

    newarraysflag = 0;
    if ( dlistflag )
    {
      glNewList(dindex,GL_COMPILE);
      if ( indexing_flag )
      {
        glDrawElements(GL_TRIANGLES,facetcount,GL_UNSIGNED_INT,indexarray+facetstart);
        glMatrixMode(GL_PROJECTION);
        glTranslated(edge_bias,0.0,0.0);   /* edges in front */
        glDrawElements(GL_LINES,edgecount,GL_UNSIGNED_INT,indexarray+edgestart);
        glTranslated(-edge_bias,0.0,0.0);
        glMatrixMode(GL_MODELVIEW);
      }
      else
      {
        glDrawArrays(GL_TRIANGLES,facetstart,facetcount); 
        glMatrixMode(GL_PROJECTION);
        glTranslated(edge_bias,0.0,0.0);    /* edges in front */
        glDrawArrays(GL_LINES,edgestart,edgecount);
        glTranslated(-edge_bias,0.0,0.0);
        glMatrixMode(GL_MODELVIEW);
     }
      glEndList();           
      dlistflag = NORMALLIST;
    }
    if ( q_flag ) outstring(current_prompt);  
  }
  else
  if ( (dlistflag != NORMALLIST) || 
        ((graph_timestamp != prev_timestamp) && go_display_flag ) )
  { /* regenerate display list */
    graph_start = Oglz_start;
    graph_facet = Oglz_facet;
    graph_edge  = Oglz_edge;
    graph_end = Oglz_end;
  
    init_graphics = Ogl_init;
    finish_graphics = Ogl_finish;
    close_graphics = Ogl_close;
       
    if ( dlistflag != NOLIST )
    { glNewList(dindex,GL_COMPILE);
      graphgen();
      glEndList();
      dlistflag = NORMALLIST;
    }
  }

  if ( SDIM != olddim )
  { reshape_func(xsize,ysize);  /* in case dimension changes */
    olddim = SDIM;
  }
  glEnable(GL_LIGHT0);
  //glEnable(GL_LIGHT1);

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

  glMatrixMode(GL_MODELVIEW);
  for ( i = 0 ; i < 4 ; i++ )
    for ( j = 0 ; j < 4 ; j++ )
      viewf[i][j] = view[(j<3)?j:(SDIM)][(i<3)?i:(SDIM)];
  if ( SDIM == 2 ) 
  { for ( i = 0 ; i < 3 ; i++ ) viewf[i][2] = viewf[2][i] = 0.0;
    viewf[2][2] = 1.0;
  }
  /* transpose, picking first 3 coordinates */
  if ( (projmode == P_PERSP) || stereomode ) 
      if ( SDIM == 2 ) viewf[3][2] -= 16;
      else viewf[3][0] -= 16.0;
 

  glLoadMatrixd(viewf[0]); 
  
  // for picking
  if ( !arraysflag ) { glInitNames();  glPushName(0);}

  // Now the actual drawing
  if ( dlistflag )
  { if ( stereomode )
    { int w = (SDIM==2) ? 0 : 1;
      // Stereo mode always perspective
      viewf[3][w] -= 1.5; glLoadMatrixd(viewf[0]); glCallList(dindex);
      viewf[3][w] += 3.0; glLoadMatrixd(viewf[0]); glCallList(dindex);
        
     }
    else glCallList(dindex);    
  }
  else if ( arraysflag )
  { int i,j,m;

    for ( m = 0 ; (m < transform_count) || (m < 1) ; m++ )
    { float tmat[4][4];  /* transform matrix in proper form */

      if ( transforms_flag && doing_lazy && view_transform_det 
                                  && (view_transform_det[m] == -1.0) )
      { /* have to flip normals */
        for ( i = 0 ; i < edgecount+facetcount ; i++ )
         for ( j = 0 ; j < 3 ; j++ )
          fullarray[i].n[j] *= -1.0;
      }

      if ( transforms_flag && doing_lazy && view_transforms )
      { int hi = (SDIM <= 3) ? SDIM : 3;
        for ( i = 0 ; i < hi; i++ )
        { for ( j = 0 ; j < hi; j++ )
            tmat[i][j] = (float)view_transforms[m][j][i];
          for ( ; j < 3 ; j++ ) tmat[i][j] = 0.0;
          tmat[i][3] = (float)view_transforms[m][i][SDIM];
        }
        for ( ; i < 3 ; i++ )
        { for ( j = 0 ; j < 4 ; j++ ) tmat[i][j] = 0.0;
          tmat[i][i] = 1.0;
        }
        for ( j = 0 ; j < hi ; j++ )
          tmat[3][j] = (float)view_transforms[m][SDIM][j];
        for ( ; j < 3 ; j++ )
          tmat[3][j] = 0.0;
        tmat[3][3] = (float)view_transforms[m][SDIM][SDIM];
      
        glMatrixMode(GL_MODELVIEW);
        glPushMatrix();
        glMultMatrixf((float*)tmat);
      }

      if ( strips_flag )
      { int i;
        /* do facets first, then edges in front */
        for ( i = estripcount ; i < stripcount ; i++ )
         glDrawElements(striparray[i].mode,striparray[i].count,GL_UNSIGNED_INT,stripdata+striparray[i].start);
        glMatrixMode(GL_PROJECTION);
        glTranslated(edge_bias,0.0,0.0);
        for ( i = 0 ; i < estripcount ; i++ )
         glDrawElements(striparray[i].mode,striparray[i].count,GL_UNSIGNED_INT,stripdata+striparray[i].start);
        glTranslated(-edge_bias,0.0,0.0);
      }     
      else if ( indexing_flag )
      {
        glDrawElements(GL_TRIANGLES,facetcount,GL_UNSIGNED_INT,indexarray+facetstart);
        glMatrixMode(GL_PROJECTION);
        glTranslated(edge_bias,0.0,0.0);
        glDrawElements(GL_LINES,edgecount,GL_UNSIGNED_INT,indexarray+edgestart);
        glTranslated(-edge_bias,0.0,0.0);
      }
      else
      {
        glDrawArrays(GL_TRIANGLES,facetstart,facetcount);
        glMatrixMode(GL_PROJECTION);
        glTranslated(edge_bias,0.0,0.0);
        glDrawArrays(GL_LINES,edgestart,edgecount);
        glTranslated(-edge_bias,0.0,0.0);
      }
      if ( transforms_flag && doing_lazy )
      { glMatrixMode(GL_MODELVIEW); glPopMatrix(); }

      if ( transforms_flag &&  doing_lazy && view_transform_det && (view_transform_det[m] == -1.0) )
      { /* have to flip normals back */
        for ( i = 0 ; i < edgecount+facetcount ; i++ )
         for ( j = 0 ; j < 3 ; j++ )
          fullarray[i].n[j] *= -1.0;
      }

      if ( !doing_lazy  || !transforms_flag ) break;
    } /* end transform loop */
  } /* end arrays_flag */
  else /* not using display list or arrays */
  {
    graphgen();
  }
  glFlush();
  auxSwapBuffers();  

bailout:
  LEAVE_GRAPH_MUTEX;
}

/***********************************************************************
*
* function: display()
*
* purpose: called by Evolver to display on screen.
*/
void display(void)
{ close_flag = 0;
  Oglz_start();
  InvalidateRect(hwnd,NULL,FALSE);  /* generate redraw message */
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

int ecomp(const void *a, const void *b)
{ int ai = indexarray[edgestart+*(int*)a]; 
  int bi = indexarray[edgestart+*(int*)b]; 
  if ( ai < bi ) return -1;
  if ( bi < ai ) return 1;
  return 0;
}

struct festruct { int v[3];  /* head and tail and opposite vertices */
                  int f;     /* facet on left */
                };

int fecomp ( const struct festruct *a, const struct festruct *b)
{ if ( a->v[0] < b->v[0] ) return -1;
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

  /* strip arrays */
  if ( stripdata ) myfree((char*)stripdata);
  if ( striparray ) myfree((char*)striparray);
  stripdata = (int*)mycalloc(edgecount+facetcount,sizeof(int));
  striparray = (struct stripstruct *)mycalloc(edgecount/2+facetcount/3,
                                         sizeof(struct stripstruct));
  dataspot = 0; stripnum = 0;

  /* edges */
  /* make list indexed by vertex */
  /* in einx, entry is 2*edgeindex+orientationbit */
  edgeinx = (int *)temp_calloc(edgecount,sizeof(int));
  for ( i = 0 ; i < edgecount ; i++ )
  { edgeinx[i] = i;  } /* using bit for orientation */  
  qsort((void*)edgeinx,edgecount,sizeof(int),FCAST ecomp);
  /* find where individual vertex segments start */
  evlist = (int *)temp_calloc(edgecount+1,sizeof(int));
  for ( i = 0, v = 0 ; i < edgecount ; i++ )
  { while ( indexarray[edgestart+edgeinx[i]] > v ) evlist[++v] = i;
  }
  evlist[++v] = i;  /* last sentinel */

  /* now make strips.  start with some edge and just keep going. */
  estripno = (int*)temp_calloc(edgecount/2,sizeof(int));
  for ( i = 0 ; i < edgecount/2 ; i++ )
  { int nexte,headv;
    if ( estripno[i] ) continue;
    /* new strip */
    striparray[stripnum].mode = GL_LINE_STRIP;
    striparray[stripnum].start = dataspot;
    nexte = 2*i;
    for (;;)
    { 
      estripno[nexte>>1] = stripnum+1;
      headv = indexarray[edgestart+(nexte^1)];
      stripdata[dataspot++] = headv;
      for ( k = evlist[headv] ; k < evlist[headv+1] ; k++ )
      { if ( estripno[edgeinx[k]>>1] == 0 )
        { nexte = edgeinx[k]; 
          break;
        }
      }
      if ( k == evlist[headv+1] ) break; /* end of strip */
    }
    /* flip list around */
    for ( k = striparray[stripnum].start, kk = dataspot-1 ; k < kk ; k++,kk-- )
      { int temp = stripdata[k]; stripdata[k] = stripdata[kk]; 
        stripdata[kk] = temp;
      }
    /* now backwards */
    nexte = 2*i+1;
    for (;;)
    {
      estripno[nexte>>1] = stripnum+1; 
      headv = indexarray[edgestart+(nexte^1)];
      stripdata[dataspot++] = headv;
      for ( k = evlist[headv] ; k < evlist[headv+1] ; k++ )
      { if ( estripno[edgeinx[k]>>1] == 0 )
        { nexte = edgeinx[k]; 
          break;
        }
      }
      if ( k == evlist[headv+1] ) break; /* end of strip */
    }
    striparray[stripnum].count = dataspot - striparray[stripnum].start;

    stripnum++;
  }

  estripcount = stripnum;
  temp_free((char*)evlist);
  temp_free((char*)estripno); 

  /* facets */

  /* make list of edges with left-hand facets */
  felist = (struct festruct *)temp_calloc(facetcount,sizeof(struct festruct));
  for ( i = 0, k = 0 ; i < facetcount ; i += 3, k++ )
  { felist[i].v[0] = indexarray[facetstart+i];
    felist[i].v[1] = indexarray[facetstart+i+1];
    felist[i].v[2] = indexarray[facetstart+i+2];
    felist[i].f = k;
    felist[i+1].v[0] = indexarray[facetstart+i+1];
    felist[i+1].v[1] = indexarray[facetstart+i+2];
    felist[i+1].v[2] = indexarray[facetstart+i];
    felist[i+1].f = k;
    felist[i+2].v[0] = indexarray[facetstart+i+2];
    felist[i+2].v[1] = indexarray[facetstart+i];
    felist[i+2].v[2] = indexarray[facetstart+i+1];
    felist[i+2].f = k;
  }
  qsort((void*)felist,facetcount,sizeof(struct festruct),FCAST fecomp);

  fstripno = (int *)temp_calloc(facetcount/3,sizeof(int));
  trialstrip = (int *)temp_calloc(facetcount/3+10,sizeof(int));
  bestverts = (int *)temp_calloc(facetcount/3+10,sizeof(int));
  bestfacets = (int *)temp_calloc(facetcount/3,sizeof(int));

  /* now make strips.  start with some facet and just keep going. */
  for ( i = 0 ; i < facetcount/3 ; i++ )
  { int nextf,va,vb,vc,whichway;
    int firstcount,secondcount; /* for checking orientation at start */
    if ( fstripno[i] ) continue;
    /* new strip */
    striparray[stripnum].mode = GL_TRIANGLE_STRIP;
    striparray[stripnum].start = dataspot;
    bestlength = 0;
    for ( way = 0 ; way < 3 ; way++)
    { int m = 0;  /* trialstrip index */
    dataspot = striparray[stripnum].start;
    nextf = 3*i;
    stripdata[dataspot++] = va = indexarray[facetstart+nextf+((1+way)%3)];
    stripdata[dataspot++] = vb = indexarray[facetstart+nextf+way];
    whichway = 1;
    for (;;)
    { struct festruct *fe,key;
      //find in felist
      if ( whichway ) { key.v[0] = va; key.v[1] = vb; }
      else { key.v[1] = va; key.v[0] = vb; }
      fe = bsearch(&key,(void*)felist,facetcount,sizeof(struct festruct),
             FCAST fecomp);
      if ( fe==NULL ) break;  // done; maybe hit edge of surface

      //see if facet done yet
      if ( fstripno[fe->f] != 0 ) break;  // done in this direction

      //add opposite vertex
      vc = fe->v[2];
      stripdata[dataspot++] = vc;                                 
      fstripno[fe->f] = stripnum+1;
      trialstrip[m++] = fe->f;

      // ready for next time around
      va = vb;
      vb = vc;
      whichway = !whichway;
    }
    firstcount = dataspot - striparray[stripnum].start;
    /* flip list around */
    for ( k = striparray[stripnum].start, kk = dataspot-1 ; k < kk ; k++,kk-- )
      { int temp = stripdata[k]; stripdata[k] = stripdata[kk]; 
        stripdata[kk] = temp;
      }
    /* now backwards */
    va = indexarray[facetstart+nextf+way];
    vb = indexarray[facetstart+nextf+((way+1)%3)];
    whichway = 1;
    for (;;)
    { struct festruct *fe,key;
      //find in felist
      if ( whichway ) { key.v[0] = va; key.v[1] = vb; }
      else { key.v[1] = va; key.v[0] = vb; }
      fe = bsearch(&key,(void*)felist,facetcount,sizeof(struct festruct),
             FCAST fecomp);
      if ( fe==NULL ) break;  // done; maybe hit edge of surface

      //see if facet done yet
      if ( fstripno[fe->f] != 0 ) break;  // done in this direction

      //add opposite vertex
      vc = fe->v[2];
      stripdata[dataspot++] = vc;
      fstripno[fe->f] = stripnum+1;
      trialstrip[m++] = fe->f;
      // ready for next time around
      va = vb;
      vb = vc;
      whichway = !whichway;
    }
    striplength[way] = dataspot - striparray[stripnum].start;
    secondcount = striplength[way] - firstcount;

    // check orientation at start
    if ( firstcount & 1 )
    { if ( secondcount & 1 ) 
      { striplength[way]--;  /* omit last, if necessary */
        if ( i == trialstrip[m-1] ) i--;  /* so loop doesn't skip omitted facet */
      }
      /* flip order */
      for ( k = striparray[stripnum].start, 
            kk = striparray[stripnum].start+striplength[way]-1 ; k < kk ; k++,kk-- )
      { int temp = stripdata[k]; stripdata[k] = stripdata[kk]; 
        stripdata[kk] = temp;
      }
    }

    if ( striplength[way] > bestlength )
    { bestlength = striplength[way];
      bestway = way;
      memcpy(bestverts,stripdata+striparray[stripnum].start,bestlength*sizeof(int));
      memcpy(bestfacets,trialstrip,(bestlength-2)*sizeof(int));
    }
    for ( k = 0 ; k < m ; k++ )  /* unmark */
       fstripno[trialstrip[k]] = 0;
    }  /* end ways */

    memcpy(stripdata+striparray[stripnum].start,bestverts,bestlength*sizeof(int));
    for ( k = 0 ; k < bestlength-2 ; k++ )  /* remark */
    { fstripno[bestfacets[k]] = stripnum+1;
      if ( strip_color_flag && (bestfacets[k] < web.skel[FACET].maxcount) ) 
        set_facet_color(bestfacets[k],(stripnum % 14) + 1);
    }

    striparray[stripnum].count = bestlength;
    dataspot = striparray[stripnum].start + bestlength;

    stripnum++;
  }

  temp_free((char*)evlist);
  temp_free((char*)estripno); 
  temp_free((char*)fstripno); 
  temp_free((char*)trialstrip); 
  temp_free((char*)bestverts); 
  temp_free((char*)bestfacets); 

  stripcount = stripnum;
  fstripcount = stripcount - estripcount;
  /* cut down arrays to needed size */
  stripdata = (int*)kb_realloc((char*)stripdata,dataspot*sizeof(int));
  striparray = (struct stripstruct *)kb_realloc((char*)striparray,
                     stripcount*sizeof(struct stripstruct));
      if ( q_flag )
      { sprintf(msg,"After stripping: %d edgestrips, %d facetstrips\n",
              estripcount,fstripcount);
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
int hashfunc(struct vercol *a)
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
{ int i,j;
  int rawcount = edgecount+facetcount;  /* number of unsorted vertices */
  struct vercol **hashlist;
  float mat[4][4];

  /* get reasonable epsilon for identifying vertices */
  glGetFloatv(GL_MODELVIEW_MATRIX,mat[0]);
  gleps = 1e-5/sqrt(mat[0][0]*mat[0][0]+mat[0][1]*mat[0][1]
                       +mat[0][2]*mat[0][2]);

  /* Uniquify using hash table */
  /* qsort here is a time hog */
  if ( indexarray ) myfree((char*)indexarray);
  indexarray = (int*)mycalloc(rawcount,sizeof(int));
  hashlist = (struct vercol**)temp_calloc(2*rawcount,sizeof(struct vercol *));
  hashsize = 2*rawcount;
  hashlist[hashfunc(fullarray)] = fullarray;
  indexarray[0] = 0;
  for ( i = 1, j = 1 ; i < rawcount ; i++ )
  { int h = hashfunc(fullarray+i);
    while ( hashlist[h] && vercolcomp(hashlist[h],fullarray+i) ) 
    { h++; if ( h == hashsize ) h = 0; }
    if ( hashlist[h] == NULL ) /* new one */
    { 
      fullarray[j] = fullarray[i];
      hashlist[h] = fullarray+j;
      j++;
    }
    indexarray[i] = hashlist[h]-fullarray;
  } 
  temp_free((char*)hashlist);
  vertexcount = j;


   // Uniquify edges
   for ( i = edgestart ; i < edgestart+edgecount ; i += 2 )
   { if ( indexarray[i] > indexarray[i+1] )
     { int temp = indexarray[i];
       indexarray[i] = indexarray[i+1];
       indexarray[i+1] = temp;
     }
   }
   /* qsort here relatively minor in time */
   qsort((void*)(indexarray+edgestart),edgecount/2,2*sizeof(int), FCAST eecomp);
   for ( i = 2, j = 0 ; i < edgecount ; i += 2 )
   { if ( eecomp(indexarray+edgestart+i,indexarray+edgestart+j) != 0 )
     { j += 2;
       if ( i > j )
       { indexarray[edgestart+j] = indexarray[edgestart+i];
         indexarray[edgestart+j+1] = indexarray[edgestart+i+1];
       }
     }
   }
   if ( edgecount ) edgecount = j+2;
} /* end make_indexlists() */
