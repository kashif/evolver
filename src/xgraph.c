/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/************************************************************
*
* File: xgraph.c
*
* Contents: Graphics routines for display on X-Windows.
*/

/*
From bink@athena.mit.edu Wed Apr 10 16:05:07 1991
Received: from ATHENA.MIT.EDU by poincare.susqu.edu   ; Wed, 10 Apr 91 16:04:59 CDT
Received: from M4-167-10.MIT.EDU by ATHENA.MIT.EDU with SMTP
          id AA09847; Wed, 10 Apr 91 17:04:52 EDT
From: bink@athena.mit.edu
Received: by m4-167-10.MIT.EDU (5.61/4.7) id AA24288; Wed, 10 Apr 91 17:04:36 -0400
Message-Id: <9104102104.AA24288@m4-167-10.MIT.EDU>
To: brakke@poincare
Subject: xgraph.c
Date: Wed, 10 Apr 91 17:04:31 EDT
Status: R

Here is the X windows interface I mentioned.  Just a few comments about it;
the author, Tim Shepard, a graduate student in the MIT Laboratory for Computer
Science, says, "It is a two hour hack, and not the best possible job."  My
own remarks are that xgraph_edge is untested to date, because I have yet to 
use that feature.  Also, despite the author's complaints about the ugliness
of the code, it has worked very nicely for everything I've needed thus far.
Besides the following, three lines need to be added to the Makefile.  They 
are:  CFLAGS= -DGENERIC
      GRAPH= xgraph.o
      GRAPHLIB= -lX11

                                                        Livia Racz
                                                        (racz@zither.mit.edu)
*/

/*************************************************************
*  This file is for the Surface Evolver source code.         *
*  by Tim Shepard for Livia Racz (March 1991)                *
*  Modified 9-9-92 for colors and shading - K. Brakke        *
*************************************************************/

/* (based on tcgraph.c) */

/* X11 graphics module */
/* All coordinates in absolute pixels in window */

#include "include.h"
#ifndef NOPROTO
#define NeedFunctionPrototypes 1
#endif
#include <X11/Xlib.h>

static Display *dpy;
static Screen *screen;
static Window rootwin;
static Window win;
static XGCValues gcv;
static GC gc;
static XEvent event;
Colormap cmap;

static int maxx,maxy;  /* max viewport coordinates */
static REAL xscale,yscale;  /* for scaling to screen size */

#define MAXGRAY 15
#define CINDEX(color,gray)    ((gray)*(MAXGRAY+1) + (color))

unsigned long pixel_values[256];
  /* colors not alloced till needed to save colormap */
#define NOT_ALLOC 0xFFA93871
static int White,Black;  /* color indices in default colormap */ 

unsigned long get_a_color ARGS((int,int));

unsigned long get_a_color(c,g)
int c;    /* color, 0-15 */
int g;    /* gray level, 0-15 */
{
  XColor xc;
  unsigned long col;

  col = pixel_values[CINDEX(c,g)];
  if ( col != NOT_ALLOC ) return col;

  xc.red = (unsigned short)(0xffff*(g+12.)/(MAXGRAY+12.)*rgb_colors[c][0]); 
  xc.green = (unsigned short)(0xffff*(g+12.)/(MAXGRAY+12.)*rgb_colors[c][1]); 
  xc.blue = (unsigned short)(0xffff*(g+12.)/(MAXGRAY+12.)*rgb_colors[c][2]); 
  /* if (xc.blue < 0x4444) xc.blue = 0x4444; */
  xc.flags = DoRed|DoGreen|DoBlue;
  if (XAllocColor(dpy, cmap, &xc) == 0) 
    {
      xc.pixel = g>=8?WhitePixelOfScreen(screen): BlackPixelOfScreen(screen);
    }
  pixel_values[CINDEX(c,g)] = xc.pixel;
  return xc.pixel;
}
  
void init_xgraph()
{
  int g;
  int waiting_for_window = 0;

  if ( init_flag == 0 ) {
        waiting_for_window = 1;
        init_flag = 1;
        event.type = 0;

        for ( g = 0 ; g < 256 ; g++ ) pixel_values[g] = NOT_ALLOC;

        dpy = XOpenDisplay("");
        if (dpy == NULL)
          kb_error(2213,"Unable to open X display.\n", RECOVERABLE);
        screen = XDefaultScreenOfDisplay(dpy);
        if (screen == NULL)
          kb_error(2214,"Unable to get default screen of X display.\n", RECOVERABLE);
        rootwin = XRootWindowOfScreen(screen);
        if (rootwin == 0)
          kb_error(2215,"X display: Unable to get root window of screen.\n", RECOVERABLE);
        win = XCreateSimpleWindow(dpy, rootwin, 0, 0, 500, 500, 1,
                                  WhitePixelOfScreen(screen),
                                  BlackPixelOfScreen(screen));
        if (win == 0)
          kb_error(2216,"X display: Unable to create window\n", RECOVERABLE);

        cmap = DefaultColormap(dpy,/*XScreenNumberOfScreen(screen)*/0);

        XSelectInput(dpy, win, StructureNotifyMask);

        XMapRaised(dpy, win);

        White = WhitePixelOfScreen(screen);
        Black = BlackPixelOfScreen(screen);
        gcv.line_width = 0;

        gc = XCreateGC(dpy, win, GCForeground|GCLineWidth, &gcv);

        /* wait for window to appear */
        XFlush(dpy);
        while ( event.type != MapNotify ) {
          XNextEvent(dpy, &event);
          switch(event.type) {
          case ConfigureNotify:
             maxx = event.xconfigure.width;
             maxy = event.xconfigure.height;
             xscale = (maxx<maxy?maxx:maxy)/3;
             yscale = xscale;
             waiting_for_window = 0;
             break;
          default:
             break;
          }
         }
        }

  /*  Note: X events are only handled each time we get here.    This is not
        the best situation, but without trying to modify the command parser
        this is about the best we can do.
        */

  /* do this each time incase window has changed size */
  while (waiting_for_window || XPending(dpy) > 0) {
     XNextEvent(dpy, &event);
     switch(event.type) {
     case ConfigureNotify:
        maxx = event.xconfigure.width;
        maxy = event.xconfigure.height;
        if ( maxx < maxy ) 
          { minclipx = -1.5; maxclipx = 1.5;
            minclipy = -1.5*maxy/maxx; maxclipy = 1.5*maxy/maxx;
          }
        else
          { minclipx = -1.5*maxx/maxy; maxclipx = 1.5*maxx/maxy;
            minclipy = -1.5; maxclipy = 1.5;
          }
        xscale = (maxx<maxy?maxx:maxy)/3;
        yscale = xscale;
        waiting_for_window = 0;
        break;
     default:
        break;
     }
  }

  /* clear window */
     XSetWindowBackground(dpy, win, get_a_color(background_color,MAXGRAY));
  XClearWindow(dpy,win);
}

void xgraph_edge(t)
struct tsort *t;
{
  XPoint p[MAXCOORD];  /* shorts */
  int i,color;

  if ( t->etype[0]&EBITS == INVISIBLE_EDGE ) return;
  if ( t->etype[0]&EBITS == SPLITTING_EDGE ) return;
  if ( t->ecolor[0] == CLEAR ) return;

  for ( i = 0 ; i < 2 ; i++ )
  { REAL temp;
    temp = (t->x[i][0]*xscale) + maxx/2;
    if ( fabs(temp) > 32000. ) return; /* off scale */
    p[i].x = (short)temp;
    temp = maxy/2 - (t->x[i][1]*yscale);
    if ( fabs(temp) > 32000. ) return;
    p[i].y = (short)temp;
  }         

  if ( !shading_flag && !color_flag ) color = Black;
  else 
     color =  get_a_color(t->color,MAXGRAY);
  if(  gcv.foreground != color)
  { gcv.foreground = color;
    XChangeGC(dpy, gc, GCForeground, &gcv);
  }
  XDrawLine(dpy, win, gc, p[0].x, p[0].y, p[1].x, p[1].y);
}

void xgraph_facet(t)
struct tsort *t;
{
  int n = FACET_VERTS;        /* vertices in polygon */
  XPoint p[6];                  /* vertex coords         */
  int i;
  int icolor;

  for ( i = 0 ; i < FACET_VERTS ; i++ )
  { REAL temp;
    temp = (t->x[i][0]*xscale) + maxx/2;
    if ( fabs(temp) > 32000. ) return; /* off scale */
    p[i].x = (short)temp;
    temp = maxy/2 - (t->x[i][1]*yscale);
    if ( fabs(temp) > 32000. ) return;
    p[i].y = (short)temp;
  }
  p[FACET_VERTS] = p[0];  /* convenient */
  

  if ( web.hide_flag && (t->color != CLEAR) && (t->color != UNSHOWN) )  
  {
    if ( shading_flag && color_flag )
       icolor = get_a_color(t->color,(int)(MAXGRAY*gray_level(t->normal)));
    else if ( color_flag )
       icolor = get_a_color(t->color,MAXGRAY);
    else if ( shading_flag )
       icolor = get_a_color(WHITE,(int)(MAXGRAY*gray_level(t->normal)));
    else icolor = White;
        
    if (gcv.foreground != icolor) {
          gcv.foreground = icolor;
          XChangeGC(dpy, gc, GCForeground, &gcv);
    }
    XFillPolygon(dpy, win, gc, p, n, Nonconvex, CoordModeOrigin);
  }

  /* show designated edges */
  for ( i = 0 ; i < 3 ; i++ )
  { 
    if ( t->ecolor[i] == CLEAR ) continue;
    if ( t->etype[i]&EBITS & SPLITTING_EDGE ) continue;
    if ( !edgeshow_flag || (t->color == UNSHOWN) )
    { if ( t->etype[i]&EBITS == INVISIBLE_EDGE ) continue;
    }
    icolor = get_a_color(t->ecolor[i],MAXGRAY);
    if (gcv.foreground != icolor) {
      gcv.foreground = icolor;
      XChangeGC(dpy, gc, GCForeground, &gcv);
    }
    XDrawLine(dpy, win, gc, p[i].x, p[i].y, p[i+1].x, p[i+1].y);
  }

}

void finish_xgraph()
{
  XFlush(dpy);
}

void close_xgraph()
{
  XCloseDisplay(dpy);
  init_flag = 0;
}

void display()
{
  ENTER_GRAPH_MUTEX
  init_graphics = init_xgraph;
  finish_graphics = finish_xgraph;
  close_graphics = close_xgraph;
  graph_start = painter_start;
  graph_edge  = painter_edge;
  display_edge = xgraph_edge;
  graph_facet = painter_facet;
  display_facet = xgraph_facet;
  graph_end = painter_end;
  
  graphgen();
  XFlush(dpy);
  LEAVE_GRAPH_MUTEX

}


void graph_new_surface() {}

