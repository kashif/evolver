/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/* DJGPP GNU C graphics module for 32 bit DOS extender     */
/* All coordinates in absolute pixels in current viewport */
/* Uses GRX graphics */

#include "include.h"
#include <grx.h>
#include <grdriver.h>
#include <mousex.h>
#include <pc.h>

static int maxx,maxy;  /* max viewport coordinates */
static double xscale,yscale;  /* for scaling to screen size */
static int numcolors;    /* available */

static int color_perm[16] = { 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
#define MAXGRAY 15
#define CINDEX(color,gray)  \
  ((numcolors == 32768) ? ( (int)(2*gray*rgb_colors[color][0])*0x400 \
+ (int)(2*gray*rgb_colors[color][1])*0x20 + (int)(2*gray*rgb_colors[color][2]) ):\
  (shading_flag?( color_flag ? ((gray)*(MAXGRAY+1)+color_perm[color]): \
      ((color)?(gray):1) ) \
      : color_perm[color]))
/* lets index 0 be screen background */

void init_gnugraph()
{ int k;
  int c;    /* color, 0-15 */
  int g;    /* gray level, 0-15 */

  GrSetMode(GR_default_graphics);
  numcolors = GrNumColors();
  if ( (numcolors < 256) && color_flag )
      shading_flag = 0;  /* not enough for shading */
  
  /* first index background color */
  for ( c = 0 ; c < 16 ; c++ ) color_perm[c] = c;
  color_perm[background_color] = 0;
  color_perm[0] = background_color;
  
  if ( shading_flag && color_flag )
  { for ( c = 0 ; c < 16 ; c++ )
      for ( g = 0 ; g <= MAXGRAY ; g++ )
        GrSetColor(CINDEX(c,g),
         (int)(0xff*(g+12.)/(MAXGRAY+12.)*rgb_colors[c][0]), 
         (int)(0xff*(g+12.)/(MAXGRAY+12.)*rgb_colors[c][1]),
         (int)(0xff*(g+12.)/(MAXGRAY+12.)*rgb_colors[c][2]));
    }
    else if ( shading_flag ) /* shading */
    { for ( g = 0 ; g <= MAXGRAY ; g++ )
        GrSetColor(CINDEX(WHITE,g),
         (int)(0xff*(g+12.)/(MAXGRAY+12.)), 
         (int)(0xff*(g+12.)/(MAXGRAY+12.)),
         (int)(0xff*(g+12.)/(MAXGRAY+12.)));
    }
    else if ( color_flag )
    { for ( c = 0 ; c < 16 ; c++ )
        GrSetColor(CINDEX(c,MAXGRAY),
         (int)(0xff*rgb_colors[c][0]), 
         (int)(0xff*rgb_colors[c][1]),
         (int)(0xff*rgb_colors[c][2]));
    }

  if ( numcolors == 32768 )
     GrClearScreen(CINDEX(WHITE,MAXGRAY));
  else
  {  GrFreeColor(0);
      GrSetColor(0,(int)(0xff*rgb_colors[background_color][0]),
        (int)(0xff*rgb_colors[background_color][1]),
        (int)(0xff*rgb_colors[background_color][2]));
  }

  maxx = GrMaxX(); maxy = GrMaxY();
  if ( maxx/3 < maxy/3 )
    { xscale = maxx/3;
      yscale = xscale; /* assume square pixel screen */
    }
  else
    { yscale = maxy/3;
      xscale = yscale; /* assume square pixel screen */
    }
}

void gnugraph_edge(t)
struct tsort *t;
{
  int x[MAXCOORD],y[MAXCOORD];
  int i;
  
  if ( t->color == CLEAR ) return;

  for ( i = 0 ; i < 2 ; i++ )
     {
        x[i] = (int)(t->x[i][0]*xscale) + maxx/2;
        y[i] = maxy/2 - (int)(t->x[i][1]*yscale);
     }         

  GrLine(x[0],y[0],x[1],y[1],CINDEX(t->color,MAXGRAY));
}

#define swap(a,b) { tmp = a; a = b; b = tmp; }

/* line by line, for graphics without filled regions */
void draw_triangle(x1,y1,x2,y2,x3,y3,color)
int x1,y1,x2,y2,x3,y3,color;
{ int tmp;
  int y;
  double xx1,xx2,dx1,dx2;

  /* sort in y from bottom */
  if ( y2 < y1 ) { swap(x1,x2); swap(y1,y2); }
  if ( y3 < y2 ) { swap(x2,x3); swap(y2,y3); }
  if ( y2 < y1 ) { swap(x1,x2); swap(y1,y2); }

  /* bottom part */
  if ( y2 > y1 )
  { dx1 = (x2 - x1)/(double)(y2 - y1);
     dx2 = (x3 - x1)/(double)(y3 - y1);
     for ( y = y1, xx1=x1+.5, xx2=x1+.5 ; y < y2 ; y++,xx1 += dx1,xx2 += dx2 )
        GrLine((int)xx1,y,(int)xx2,y,color);
  }

  /* top part */
  if ( y3 > y2 )
  { dx1 = (x3 - x2)/(double)(y3 - y2);
     dx2 = (x3 - x1)/(double)(y3 - y1);
     for ( y = y3, xx1=x3+.5, xx2=x3+.5 ; y >= y2 ; y--,xx1 -= dx1,xx2 -= dx2 )
        GrLine((int)xx1,y,(int)xx2,y,color);
  }
}

void gnugraph_facet(t)
struct tsort *t;
{
  int n = FACET_VERTS;        /* vertices in polygon */
  int x[6][2];     /* vertex coords         */
  int i,j,k,color,icolor;

  color = t->color;
  for ( i = 0 ; i < web.sdim ; i++ )
      {
         x[i][0] = (int)(t->x[i][0]*xscale) + maxx/2;
         x[i][1] = maxy/2 - (int)(t->x[i][1]*yscale);
      }
  if (color != CLEAR)
  { 
     if ( shading_flag && color_flag )
        icolor = CINDEX(color,(int)(MAXGRAY*gray_level(t->normal)));
     else if ( color_flag )
        icolor = CINDEX(color,MAXGRAY);
     else if ( shading_flag )
        icolor = CINDEX(WHITE,(int)(MAXGRAY*gray_level(t->normal)));
          else icolor = CINDEX(WHITE,MAXGRAY);
     GrFilledConvexPolygon(3,x,icolor);
/*
     draw_triangle(x[0][0],x[0][1],x[1][0],x[1][1],x[2][0],x[2][1],icolor);
*/
  }
  if ( edgeshow_flag ) /* all grid edges */
     for ( k = 0 ; k < 3 ; k++ )
     { if ( t->etype[k]&EBITS == SPLITTING_EDGE ) continue;
        GrLine(x[k][0],x[k][1],x[(k+1)%3][0],x[(k+1)%3][1],
            CINDEX(t->ecolor[k],MAXGRAY));
     }
  else for ( k = 0 ; k < 3 ; k++ ) /* show designated edges */
     {
        if ( t->etype[k]&EBITS == INVISIBLE_EDGE ) continue; 
        if ( t->etype[k]&EBITS == SPLITTING_EDGE ) continue; 
        if ( t->ecolor[k] == CLEAR ) continue;
        GrLine(x[k][0],x[k][1],x[(k+1)%3][0],x[(k+1)%3][1],
            CINDEX(t->ecolor[k],MAXGRAY));
     }
}

void display()
{ int x,y;
  MouseEvent e;
  int c,r,g,b;

  ENTER_GRAPH_MUTEX
  init_graphics = init_gnugraph;
  finish_graphics = null_function;
  graph_edge  = painter_edge;
  display_edge = gnugraph_edge;
  graph_start = painter_start;
  graph_facet = painter_facet;
  display_facet = gnugraph_facet;
  graph_end = painter_end;
  
  graphgen();

  MouseSetColors(CINDEX(WHITE,MAXGRAY), CINDEX(BLACK,MAXGRAY));
  x = maxx/2; y = maxy/2;
  MouseWarp(x, y);

  while (1)
  {
     MouseGetEvent(M_LEFT_DOWN|M_LEFT_UP|M_KEYPRESS, &e);
     if (e.flags & M_LEFT_DOWN)
        { x = e.x, y = e.y; }
     if (e.flags & M_LEFT_UP)
        { fix_ctm(view,(double)(e.x - x),(double)(y - e.y)); 
          graphgen();
        }
     if (e.flags & M_KEYPRESS)
        break;
  }
#ifdef XXX
 { int cc;
    GR_DRIVER_MODE_ENTRY *ttable = NULL,*gtable = NULL,*gt;
    cc = GrNumColors();
    GrGetDriverModes(&ttable,&gtable);
  GrSetMode(GR_default_text);
    printf("colors: %d  maxx: %d    maxy: %d\n",cc,maxx,maxy);
    if ( gtable )
     for ( gt = gtable ; gt->width > 0 ; gt++ )
      printf("w: %d  h: %d  colors: %d    BIOS: %d\n",gt->width,gt->height,
          gt->number_of_colors,gt->BIOS_mode);
    else printf("gtable is NULL.\n");
}
#else
  GrSetMode(GR_default_text);
#endif
  LEAVE_GRAPH_MUTEX

}
