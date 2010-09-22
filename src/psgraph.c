/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*****************************************************************
*
*  File: psgraph.c
*
*  Purpose:  PostScript output.
*/

/*******************************************************************
*
* Notes on usage.
*
* The depth sort is slowed down by having long unrefined edges.
* Example: 245760 facets only: 102 sec  (starfish31.fe)
*       with long bare edges: 2151 sec
*          refined bare edges: 175 sec
*/

#include "include.h"

static FILE *fd;
int gridflag;
int ps_colorflag;
int crossingflag;
char ps_file_name[1000];
int ps_widthattr;

/* for redrawing edges on top of subsequent facets */
int edgeredrawflag; /* for extra drawing of edges on top of facets */
int *edgeredrawhead;
int  edgeredrawcount;
int  edgeredrawmax;
struct erl {double x[2]; /* edge vector */
            float  v[2]; /* vertex position */
            int color;
            int next; 
            int etype; 
         };
struct erl *edgeredrawlist;

/*****************************************************************
*
*  Function: ps_init()
*
*  Purpose:  Get file name from user, write file header.
*/

void ps_init()
{ char response[100];
  int xlo,xhi,ylo,yhi;  /* int according to PostScript manual */
  REAL extra;
  time_t t;
  REAL xsize,ysize,scale;  /* for clipping box */
  int dummy;
  int n;

  if ( (web.representation != STRING) && (gridflag < 0) )
  {
    prompt("Show grid lines? ",response,sizeof(response));
    gridflag = (toupper(response[0]) == 'Y');
    edgeredrawflag = !gridflag;  /* default */
    if (toupper(response[1]) == 'Y') edgeredrawflag = 1;
    if (toupper(response[1]) == 'N') edgeredrawflag = 0;

  }
  if ( ps_colorflag < 0 )
  { prompt("Do colors? ",response,sizeof(response));
    ps_colorflag = (toupper(response[0]) == 'Y');
  }
  if ( (web.representation == STRING) && (SDIM > 2) && (crossingflag < 0) )
  {
    prompt("Do crossings? ",response,sizeof(response));
    crossingflag = (toupper(response[0]) == 'Y');
  }
  if ( labelflag < 0 )
  { prompt("Do labels? (i for ids, o for originals) ",response,sizeof(response));
    switch ( toupper(response[0]) )
     { case 'I' :
       case 'Y' : labelflag = LABEL_ID; break;
       case 'O' : labelflag = LABEL_ORIG; break;
       default  : labelflag = NOLABELS;
     }
  }
  if ( strlen(ps_file_name) == 0 )
  {
     prompt("Enter file name (.ps will be added): ",ps_file_name,sizeof(ps_file_name));
  }
  if ( (strcmp(ps_file_name+strlen(ps_file_name)-3,".ps")!=0) && 
         (strcmp(ps_file_name+strlen(ps_file_name)-4,".eps")!=0) )
        strcat(ps_file_name,".ps");
  fd = fopen(ps_file_name,"w");
  if ( fd == NULL )
  { sprintf(errmsg,"Cannot open %s.\n",ps_file_name);
    ps_file_name[0] = 0;
    kb_error(1649,errmsg,RECOVERABLE);
  }

  edgeredrawcount = 1;
  edgeredrawhead = (int*)temp_calloc(web.skel[VERTEX].max_ord+5,sizeof(int));
  edgeredrawmax  = web.skel[EDGE].count;
  edgeredrawlist = (struct erl *)temp_calloc(edgeredrawmax,sizeof(struct erl));
  ps_widthattr = find_extra(PS_WIDTHNAME,&dummy);
  fputs("%!PS-Adobe-3.0 EPSF-3.0\n",fd);
  /* bounding box, with a little extra for line widths and labels */  
  extra = (labelflag>0) ? .066 : .002;
  xsize = maxclipx - minclipx; ysize = maxclipy - minclipy;
  if ( xsize/ysize > 8/10.5 ) scale = 8*72/xsize;
  else scale = 10.5*72/ysize;

  if ( full_bounding_box_flag )
  { xlo = 0;
    ylo = 0;
    xhi = (int)(xsize*scale);
    yhi = (int)(ysize*scale);
  }
  else /* as defined by surface */
  { xlo = (int)floor((bbox_minx - extra - minclipx)*scale); 
    if ( xlo < 0 ) xlo = 0;
    xhi = (int)ceil((bbox_maxx + extra - minclipx)*scale); 
    if ( xhi > xsize*scale ) xhi = (int)(xsize*scale);
    ylo = (int)floor((bbox_miny - extra - minclipy)*scale); 
    if ( ylo < 0 ) ylo = 0;
    yhi = (int)ceil((bbox_maxy + extra - minclipy )*scale); 
    if ( yhi > ysize*scale ) yhi = (int)(ysize*scale);
  }
  fprintf(fd,"%%%%BoundingBox: %d %d %d %d\n",xlo,ylo,xhi,yhi);
  fprintf(fd,"%%%%Title: (%s)\n",ps_file_name);
  fprintf(fd,"%%%%Creator: Surface Evolver\n");
  time(&t);
  fprintf(fd,"%%%%CreationDate: %s\n",asctime(localtime(&t)));
  fprintf(fd,"%%%%EndComments\n");
  fprintf(fd,"%% Image is in %g\" x %g\" box aligned lower left on paper.\n",
      (DOUBLE)(xhi-xlo)/72.,(DOUBLE)(yhi-ylo)/72.);
  fputs("% Change relsize to alter relative size of labels and linewidths.\n",fd);
  fprintf(fd,"/relsize %f def\n",ps_labelsize);
  fprintf(fd,"%f %f scale\n",(DOUBLE)scale,(DOUBLE)scale);
  fprintf(fd,"%f %f translate\n",(DOUBLE)(-minclipx),(DOUBLE)(-minclipy));
  fprintf(fd,"newpath %f %f moveto %f %f lineto %f %f \n",
      (DOUBLE)minclipx,(DOUBLE)minclipy,(DOUBLE)maxclipx,(DOUBLE)minclipy,
         (DOUBLE)maxclipx,(DOUBLE)maxclipy);
  fprintf(fd,"   lineto  %f %f lineto closepath clip\n",
        (DOUBLE)minclipx,(DOUBLE)maxclipy);
  fputs("1 setlinecap  1 setlinejoin\n",fd);
  if ( ps_colorflag )
    { fputs("/fa { newpath setrgbcolor  moveto lineto\n",fd);
      fputs("            lineto closepath fill } def % filled facet without edges\n\n",fd);
      fputs("/fb {setrgbcolor 6 copy newpath moveto lineto\n",fd);
      fputs("            lineto closepath fill 6 2 roll 6 copy} def % filled facet with edges\n\n",fd);
      fputs("/fc {setrgbcolor .001 relsize mul setlinewidth 6 2 roll 6 copy} def % outline only\n\n",fd);
      fputs("/edge {relsize mul setlinewidth setrgbcolor newpath moveto lineto stroke} def\n",fd);
      fputs("/arcedge {relsize mul setlinewidth setrgbcolor newpath moveto arc stroke} def\n",fd);
    }
  else
    { fputs("/fa { newpath setgray  moveto lineto\n",fd);
      fputs("            lineto closepath fill } def  % filled facet without edges \n\n",fd);
      fputs("/fb {setgray 6 copy newpath moveto lineto\n",fd);
      fputs("            lineto closepath fill 6 2 roll 6 copy} def % filled facet with edges\n\n",fd);
      fputs("/fc {setgray 0 setgray .001 relsize mul setlinewidth 6 2 roll 6 copy} def % outline only\n\n",fd);
      fputs("/edge {relsize mul setlinewidth 0 setgray newpath moveto lineto stroke} def\n",fd);
      fputs("/arcedge {relsize mul setlinewidth 0 setgray newpath moveto arc stroke} def\n",fd);
    }
  if ( labelflag>0 ) 
  { 
    fputs("/vorad .022 relsize mul def\n",fd);
    fputs("/arrowrad vorad def\n",fd); 

    if ( ps_colorflag )
      fputs("/edge { relsize mul setlinewidth setrgbcolor 4 copy  newpath",fd);
    else
      fputs("/edge { relsize mul setlinewidth 0 setgray 4 copy  newpath",fd);
     fputs(" moveto lineto stroke\n",fd);
    fputs("        /y2 exch def /x2 exch def /y1 exch def /x1 exch def\n",fd);
    fputs("        /dy y2 y1 sub def  /dx x2 x1 sub def\n",fd);
    fputs("        /mag dx dx mul dy dy mul add sqrt def\n",fd);
    fputs("        mag 0 ne {                           \n",fd);
    fputs("        /xu dx mag div def /yu dy mag div def\n",fd);
    fputs("        /angle xu yu neg atan def\n",fd);
    fputs("        newpath x2 xu vorad mul sub  y2 yu vorad mul sub moveto\n",fd);
    fputs("                x2 xu vorad mul sub  arrowrad yu mul add\n",fd);
    fputs("                y2 yu vorad mul sub  arrowrad xu mul sub\n",fd);
    fputs("                arrowrad angle angle 45 add arc\n",fd);
    fputs("               stroke\n",fd);
    fputs("        newpath x2 xu vorad mul sub  y2 yu vorad mul sub moveto\n",fd);
    fputs("                x2 xu vorad mul sub  arrowrad yu mul sub\n",fd);
    fputs("                y2 yu vorad mul sub  arrowrad xu mul add\n",fd);
    fputs("                arrowrad angle 180 sub angle 225 sub arcn\n",fd);
    fputs("               stroke\n",fd);
    fputs("        } if \n",fd);
    fputs(" } def\n",fd);

    fputs("/Helvetica findfont .03 scalefont setfont\n",fd);
    for ( n = 1 ; n <= 5 ; n++ )
    { fprintf(fd,"/vertex%d { gsave translate relsize relsize scale\n",n);
      fprintf(fd,"              newpath 0 setgray 0 0 .022 0 360 arc fill\n");
      fprintf(fd,"              newpath 1 setgray 0 0 .020 0 360 arc fill\n");
      if ( n > 2 )
        fprintf(fd,"              2 %d div 2 %d div scale\n",n,n);
      fprintf(fd,"               %6.4f -.01 moveto 0 setgray show grestore } def\n",
         .004-.01*n);

      fprintf(fd,"/edgenum%d { gsave translate relsize relsize scale\n",n);
      fprintf(fd,"     newpath  %5.3f -.016 moveto\n",-(.008+.004*n));
      fprintf(fd,"    0 setgray  %5.3f 0 rlineto 0 .032 rlineto %6.3f 0\n",
        0.022+.013*n,-(0.022+0.013*n));
      fprintf(fd,"    rlineto fill newpath %5.3f -.014 moveto\n",-(.0078+.004*n));
      fprintf(fd,"     1 setgray  %5.3f 0 rlineto 0 .03 rlineto %6.3f 0\n",
         0.020+.013*n,-(0.020+.013*n));
      fprintf(fd,
        "    rlineto fill 0 setgray %6.3f -.01 moveto show grestore } def\n",
          0.001-0.006*n);

      fprintf(fd,"/facenum%d { gsave translate relsize relsize scale\n",n);
      fputs("    newpath 1 setgray -.02 -.015 moveto\n",fd);
      fprintf(fd,"   %5.3f 0 rlineto 0 .03 rlineto %5.3f 0 rlineto fill 0 setgray \n",(n+1)*0.025,-(n+1)*0.025);
      fputs("    newpath -.019 -.01 moveto show grestore } def \n",fd);
    }
  }
  fprintf(fd,"/ew {%7.5f edge} def  %% normal string edge width\n",
    ps_stringwidth);
  fprintf(fd,"/fw {%7.5f edge} def  %% fixed edge width\n",
    ps_fixededgewidth);
  fprintf(fd,"/tw {%7.5f edge} def  %% triple edge width\n",ps_tripleedgewidth);
  fprintf(fd,"/ww {%7.5f edge} def  %% edges on walls\n",ps_conedgewidth);
  fprintf(fd,"/bw {%7.5f edge} def  %% bare edge width\n",ps_bareedgewidth);
  fprintf(fd,"/gw {%7.5f edge} def  %% grid edge width\n",ps_gridedgewidth);
  fprintf(fd,"/no {pop pop pop pop} def        %% no edge \n");
  if (crossingflag > 0) /* redefine edge and gw */
  {
     if ( ps_colorflag )
     fputs("/edge { 8 copy .002 add relsize mul setlinewidth 0 setlinecap 1 setgray pop pop pop\n",fd);
     else
     fputs("/edge { 5 copy .002 add relsize mul setlinewidth 0 setlinecap 1 setgray\n",fd);
     fputs("    newpath moveto lineto stroke setlinewidth 2 setlinecap\n",fd);
     if ( ps_colorflag )
     fputs("    setrgbcolor newpath moveto lineto stroke} def\n",fd);
     else
     fputs("    0 setgray newpath moveto lineto stroke} def\n",fd);
     fputs("/gw {0.002 edge} def\n",fd);
  }
}

/************************************************************
*
*  Function: ps_edge()
*
*  Purpose: Graphs one edge, already transformed.
*/

void ps_edge(t)
struct tsort *t;
{ int i;

  if ( t->color == CLEAR ) return;
  if ( t->flag & EDGE_ARC )
  { REAL w1[MAXCOORD],w2[MAXCOORD],mag1,mag2,w1w2,center[2],radius;
    REAL angle1,angle2,det;

     for (i = 0 ; i < SDIM ; i++ )
     { w1[i] = t->x[1][i] - t->x[0][i];
       w2[i] = t->x[2][i] - t->x[0][i];
     }
     det = w1[0]*w2[1] - w1[1]*w2[0];
     mag1 = SDIM_dot(w1,w1); mag2 = SDIM_dot(w2,w2);
     w1w2 = w1[0]*w2[0] + w1[1]*w2[1];
     if ( 4000*det*det <= mag1*mag1*mag2 + mag1*mag2*mag2 - 2*mag1*w1w2*mag2 )
     { /* practically straight line */
       for ( i = 0 ; i < SDIM ; i++ )
          t->x[1][i] = t->x[2][i];
       fprintf(fd,"  %7.4f %7.4f ",(DOUBLE)t->x[0][0],(DOUBLE)t->x[0][1]);
       fprintf(fd,"  %7.4f %7.4f ",(DOUBLE)t->x[1][0],(DOUBLE)t->x[1][1]);
       t->flag &= ~EDGE_ARC;
     }
     else
     { /* circle */
       center[0] = t->x[0][0] + 0.5*(w2[1]*mag1 - w1[1]*mag2)/det;
       center[1] = t->x[0][1] + 0.5*(-w2[0]*mag1 + w1[0]*mag2)/det;
       radius =  sqrt((mag1*mag1*mag2+mag1*mag2*mag2-2*mag1*w1w2*mag2)/4/det/det);
       angle1 = 180/M_PI*atan2(t->x[0][1]-center[1],t->x[0][0]-center[0]);
       angle2 = 180/M_PI*atan2(t->x[2][1]-center[1],t->x[2][0]-center[0]);
       if ( det > 0 )
       { fprintf(fd," %7.4f %7.4f %7.4f %7.4f %7.4f %7.4f %7.4f ",
         center[0],center[1],
         radius,angle1,angle2,(DOUBLE)t->x[0][0],(DOUBLE)t->x[0][1]);
       } else
       { fprintf(fd," %7.4f %7.4f %7.4f %7.4f %7.4f %7.4f %7.4f ",
           center[0],center[1],
         radius,angle2,angle1,(DOUBLE)t->x[2][0],(DOUBLE)t->x[2][1]);
       }
     }
  }
/*
  else if ( t->flag & SPHERE_ARC )
  {
  }
*/
  else
  {
    fprintf(fd,"  %7.4f %7.4f ",(DOUBLE)t->x[0][0],(DOUBLE)t->x[0][1]);
    fprintf(fd,"  %7.4f %7.4f ",(DOUBLE)t->x[1][0],(DOUBLE)t->x[1][1]);
  }
  /* orientation for arrow label */
  if ( labelflag && (t->etype[0] & LABEL_REVERSED) )
     fprintf(fd," 4 2 roll ");
  /* color, if wanted */
  if ( ps_colorflag > 0 )
     for ( i = 0 ; i < 3 ; i++ )
        fprintf(fd," %5.3f",(DOUBLE)rgb_colors[t->color][i]);
  /* width of edge */
  if ( t->flag & EDGE_ARC )
  { if ( ps_widthattr >= 0 ) 
      fprintf(fd," %5.3f arcedge ",*EREAL(t->f_id,ps_widthattr));
    else fprintf(fd," %5.3f arcedge\n",ps_stringwidth);
  } 
  else
  {
  if ( ps_widthattr >= 0 ) 
    fprintf(fd," %5.3f edge ",*EREAL(t->f_id,ps_widthattr));
  else if ( web.representation == STRING ) fprintf(fd," ew\n");
  else if ( t->etype[0] & BARE_EDGE ) fprintf(fd," bw\n");
  else if ( t->etype[0] & FIXED_EDGE ) fprintf(fd," fw\n");
  else if ( t->etype[0] & CONSTRAINT_EDGE ) fprintf(fd," ww\n");
  else if ( t->etype[0] & BOUNDARY_EDGE ) fprintf(fd," ww\n");
  else if ( t->etype[0] & SINGLE_EDGE ) fprintf(fd," ww\n");
  else if ( t->etype[0] & TRIPLE_EDGE ) fprintf(fd," tw\n");
  else fprintf(fd," gw\n"); /* regular grid interior edge */
  }

  if ( labelflag>0 )
  { char *hv=NULL,*tv=NULL,*en=NULL;
     switch ( labelflag )
     { case LABEL_ID:
       {  hv = ELNAME(get_edge_headv(t->f_id));
          tv = ELNAME1(get_edge_tailv(t->f_id));
          en = ELNAME2(t->f_id);
          break;
       }
        case LABEL_ORIG:
        {
          hv = ELNAME(get_original(get_edge_headv(t->f_id)));
          tv = ELNAME1(get_original(get_edge_tailv(t->f_id)));
          en = ELNAME2(get_original(t->f_id));
          break;
        }
     }
     if ( strlen(tv) > 5 ) tv[5] = 0;
     if ( strlen(hv) > 5 ) hv[5] = 0;
     if ( strlen(en) > 5 ) en[5] = 0; /* max 5 chars in labels */

     if ( (t->flag & LABEL_TAIL) && (strlen(tv) > 0) )
       fprintf(fd," (%s) %f %f vertex%1d\n",tv,
        (DOUBLE)t->x[0][0],(DOUBLE)t->x[0][1],strlen(tv));
     if ( (t->flag & LABEL_HEAD) && (strlen(hv) > 0) )
       fprintf(fd," (%s) %f %f vertex%1d\n",hv,
        (DOUBLE)t->x[1][0],(DOUBLE)t->x[1][1],strlen(hv));
     if ( (t->flag & LABEL_EDGE) && (strlen(en) > 0) )
     { if ( inverted(t->f_id) )
       fprintf(fd," (%s) %f %f edgenum%1d\n",en,
        (DOUBLE)((3*t->x[0][0]+2*t->x[1][0])/5),
        (DOUBLE)(3*t->x[0][1]+2*t->x[1][1])/5,strlen(en));
       else 
       fprintf(fd," (%s) %f %f edgenum%1d\n",en,
        (DOUBLE)((2*t->x[0][0]+3*t->x[1][0])/5),
        (DOUBLE)(2*t->x[0][1]+3*t->x[1][1])/5,strlen(en));
     }
  }
}


/***************************************************************************
*
*  Function: ps_facet()
*
*  Purpose: Graphs one facet, already transformed.
*/

void ps_facet(t)
struct tsort *t;
{ int i,j;
  char line[200];
  char *ptr = line;
  int do_flag = 0; /* whether to actually print this line */

  /* paint facet */
  /* if ( (t->color != CLEAR) && (t->color != UNSHOWN) ) */
     { REAL gray;
        REAL denom;
        denom = sqrt(dotf(t->normal,t->normal,3));
        if ( denom == 0.0 ) return;
        sprintf(ptr," %7.4f %7.4f",(DOUBLE)t->x[0][0],(DOUBLE)t->x[0][1]); 
        ptr+=strlen(ptr);
        sprintf(ptr," %7.4f %7.4f",(DOUBLE)t->x[1][0],(DOUBLE)t->x[1][1]); 
        ptr+=strlen(ptr);
        sprintf(ptr," %7.4f %7.4f",(DOUBLE)t->x[2][0],(DOUBLE)t->x[2][1]); 
        ptr+=strlen(ptr);
        if ( shading_flag )
          gray = gray_level(t->normal);
        else gray = 1.0;
        if ( ps_colorflag > 0 )
        { int c = t->color;
          if ( facet_rgb_color_attr > 0 )
            sprintf(ptr," %5.3f %5.3f %5.3f ",((c>>24)&0xFF)/255.*gray,
                  ((c>>16)&0xFF)/255.*gray, ((c>>8)&0xFF)/255.*gray);
          else
          for ( i = 0 ; i < 3 ; i++ )
          { sprintf(ptr," %5.3f",
              (DOUBLE)(rgb_colors[t->color>=0?t->color:0][i]*gray)); 
            ptr+=strlen(ptr);
          }
        }
        else
          { sprintf(ptr,"  %f ",(DOUBLE)gray); ptr += strlen(ptr); }
     }
    if ( web.hide_flag && (t->color != CLEAR) && (t->color != UNSHOWN) )
    { strcat(ptr," fb "); do_flag = 1; }
    else 
        strcat(ptr," fc ");

    /* designated edges */

    for ( i = 0 ; i < FACET_VERTS ; i++ )
      if ( (t->ecolor[i] == CLEAR) || 
      (((gridflag<=0) || (t->color==UNSHOWN)) && 
           ((t->etype[i]&EBITS) == INVISIBLE_EDGE)  ) 
      || ((t->etype[i]&EBITS) & SPLITTING_EDGE) )
          strcat(ptr," no"); 
      else 
      { int ii;
        do_flag = 1;
        /* see if need to reverse edge for arrow labelling */
        if ( labelflag && (t->etype[i] & LABEL_REVERSED) )
            strcat(line," 4 2 roll ");
        if ( ps_colorflag > 0 )
        { int c = t->ecolor[i]>=0 ? t->ecolor[i] : 0;
          strcat(ptr,"\n "); ptr += strlen(ptr);
          if ( edge_rgb_color_attr > 0 )
            sprintf(ptr," %5.3f %5.3f %5.3f ",((c>>24)&0xFF)/255.,
                  ((c>>16)&0xFF)/255., ((c>>8)&0xFF)/255.);
          else for ( j = 0 ; j < 3 ; j++ )
           { sprintf(ptr," %5.3f",(DOUBLE)rgb_colors[c][j]); 
             ptr += strlen(ptr);
           }
         }
         /* width of edge, in descending order of thickness */
         ii = (i==2)?0:(i+1);
         if ( (t->x[ii][0]-t->x[i][0])*(t->x[ii][0]-t->x[i][0]) + 
              (t->x[ii][1]-t->x[i][1])*(t->x[ii][1]-t->x[i][1])  < 1e-10 )
           strcat(line," no");
         else if ( (ps_widthattr >= 0) && valid_id(t->f_id) ) 
         { facetedge_id fe = get_facet_fe(t->f_id);
           for ( j = 0 ; j < i ; j++ ) fe = get_next_edge(fe);
           sprintf(line+strlen(line)," %7.5f edge ",
                *EREAL(get_fe_edge(fe),ps_widthattr));
         }
         else if ( t->etype[i] & BARE_EDGE ) strcat(line," bw");
         else if ( t->etype[i] & FIXED_EDGE ) strcat(line," fw");
         else if ( t->etype[i] & CONSTRAINT_EDGE ) strcat(line," ww");
         else if ( t->etype[i] & BOUNDARY_EDGE ) strcat(line," ww");
         else if ( t->etype[i] & SINGLE_EDGE ) strcat(line," ww");
         else if ( t->etype[i] & TRIPLE_EDGE ) strcat(line," tw");
         else strcat(line," gw"); /* regular grid interior edge */
      }
  strcat(line,"\n");
  if ( do_flag ) fputs(line,fd);


/* draw previous edge fragments */
if ( edgeredrawflag )
{ 
    for ( i = 0 ; i < FACET_VERTS ; i++ )
    { vertex_id v_id = t->v_id[i]; 
      if ( valid_id(v_id) )
      { int erlp = edgeredrawhead[loc_ordinal(v_id)];
         for ( ; erlp != 0 ; erlp = edgeredrawlist[erlp].next )
         { struct erl *ep = edgeredrawlist + erlp;

         if ( fabs(t->x[i][0]-ep->v[0])+fabs(t->x[i][1]-ep->v[1]) > .00001 )
           continue;
         if ( ep->x[0]*ep->x[0] + ep->x[1]*ep->x[1] < (labelflag?0.01:1e-10) )
           continue;

         if ( ps_colorflag > 0 )
         { int c = ep->color; 
           if ( edge_rgb_color_attr > 0 )
             sprintf(line,"%10.6f %10.6f %10.6f %10.6f  %5.3f %5.3f %5.3f ",
              t->x[i][0],t->x[i][1],t->x[i][0]+ep->x[0],t->x[i][1]+ep->x[1],
              ((c>>24)&0xFF)/255., ((c>>16)&0xFF)/255., ((c>>8)&0xFF)/255.);
           else
             sprintf(line,"%10.6f %10.6f %10.6f %10.6f  %5.3f %5.3f %5.3f ",
              t->x[i][0],t->x[i][1],t->x[i][0]+ep->x[0],t->x[i][1]+ep->x[1],
              (DOUBLE)rgb_colors[c][0],(DOUBLE)rgb_colors[c][1],
              (DOUBLE)rgb_colors[c][2]);
         }
         else
          sprintf(line,"%10.6f %10.6f %10.6f %10.6f ", t->x[i][0],
             t->x[i][1],t->x[i][0]+ep->x[0],t->x[i][1]+ep->x[1]);

         /* see if need to reverse edge for arrow labelling */
         if ( labelflag && (ep->etype & LABEL_REVERSED) )
            strcat(line," 4 2 roll ");

         if ( ep->etype & BARE_EDGE ) strcat(line," bw");
         else if ( ep->etype & FIXED_EDGE ) strcat(line," fw");
         else if ( ep->etype & CONSTRAINT_EDGE ) strcat(line," ww");
         else if ( ep->etype & BOUNDARY_EDGE ) strcat(line," ww");
         else if ( ep->etype & SINGLE_EDGE ) strcat(line," ww");
         else if ( ep->etype & TRIPLE_EDGE ) strcat(line," tw");
         else strcat(line," gw"); /* regular grid interior edge */
         fputs(line,fd);
         fputs("\n",fd);
      }
      }
    }
}

/* save edge end fragments for later redrawing on top of later facets
      for smoother edge line */
if ( edgeredrawflag )
{ 
    for ( i = 0 ; i < FACET_VERTS ; i++ )
    { vertex_id v_id;
      double len,fudge;
      int ii = i >= FACET_VERTS-1 ? 0 : i+1;

      if ( (t->ecolor[i] == CLEAR) || 
      (((gridflag<=0) || (t->color==UNSHOWN)) && 
           ((t->etype[i]&EBITS) == INVISIBLE_EDGE)  ) 
      || ((t->etype[i]&EBITS) & SPLITTING_EDGE) )
          continue;

      len = sqrt((t->x[ii][0]-t->x[i][0])*(t->x[ii][0]-t->x[i][0]) +
          (t->x[ii][1]-t->x[i][1])*(t->x[ii][1]-t->x[i][1]));

      fudge = (len > .005) ? .005/len : 1.0; /* max length .005 */

      if ( edgeredrawcount > edgeredrawmax-2 )
      { edgeredrawlist = (struct erl *)temp_realloc((char*)edgeredrawlist,
            2*edgeredrawmax*sizeof(struct erl));
         edgeredrawmax *= 2;
      }
      v_id = t->v_id[i];
      if ( valid_id(v_id) )
      { int didflag = 0,erlp;
         for ( erlp = edgeredrawhead[loc_ordinal(v_id)] ; erlp ; 
                    erlp = edgeredrawlist[erlp].next )
         { struct erl *ep = edgeredrawlist + erlp;
            if ( fabs(t->x[i][0]-ep->v[0])+fabs(t->x[i][1]-ep->v[1])
         + fabs(ep->x[0]-fudge*(t->x[ii][0]-t->x[i][0]))
         + fabs(ep->x[1]-fudge*(t->x[ii][1]-t->x[i][1])) < .0001 )
     {  didflag = 1; break; }
         }
         if ( didflag == 0 )
         {
            edgeredrawlist[edgeredrawcount].x[0] = fudge*(t->x[ii][0]-t->x[i][0]);
            edgeredrawlist[edgeredrawcount].x[1] = fudge*(t->x[ii][1]-t->x[i][1]);
            edgeredrawlist[edgeredrawcount].v[0] = t->x[i][0];
            edgeredrawlist[edgeredrawcount].v[1] = t->x[i][1];
            edgeredrawlist[edgeredrawcount].next = edgeredrawhead[loc_ordinal(v_id)];
            edgeredrawlist[edgeredrawcount].etype = t->etype[i];
            edgeredrawlist[edgeredrawcount].color = t->ecolor[i];
            edgeredrawhead[loc_ordinal(v_id)] = edgeredrawcount;
            edgeredrawcount++;
         }
      }
      v_id = t->v_id[ii];
      if ( valid_id(v_id) )
      {  int didflag = 0,erlp;
         for ( erlp = edgeredrawhead[loc_ordinal(v_id)] ; erlp ; 
                    erlp = edgeredrawlist[erlp].next )
         { struct erl *ep = edgeredrawlist + erlp;
           if ( fabs(t->x[ii][0]-ep->v[0])+fabs(t->x[ii][1]-ep->v[1])
                + fabs(ep->x[0]-fudge*(t->x[i][0]-t->x[ii][0]))
                + fabs(ep->x[1]-fudge*(t->x[i][1]-t->x[ii][1])) < .0001 )
           {  didflag = 1; break; }
         }
         if ( didflag == 0 )
         {
            edgeredrawlist[edgeredrawcount].x[0] = fudge*(t->x[i][0]-t->x[ii][0]);
            edgeredrawlist[edgeredrawcount].x[1] = fudge*(t->x[i][1]-t->x[ii][1]);
            edgeredrawlist[edgeredrawcount].v[0] = t->x[ii][0];
            edgeredrawlist[edgeredrawcount].v[1] = t->x[ii][1];
            edgeredrawlist[edgeredrawcount].next = edgeredrawhead[loc_ordinal(v_id)];
            edgeredrawlist[edgeredrawcount].etype = t->etype[i];
            edgeredrawlist[edgeredrawcount].color = t->ecolor[i];
            edgeredrawhead[loc_ordinal(v_id)] = edgeredrawcount;
            edgeredrawcount++;
         }
      }
    }

}

  if ( labelflag>0 )
  { facetedge_id fe;
     char * fn=NULL;
     switch ( labelflag )
     { case LABEL_ID:
          fn = ELNAME(t->f_id);
          break;
        case LABEL_ORIG:
          fn = ELNAME(get_original(t->f_id));
          break;
     }
     if ( fn ) fn[5] = 0; 
     if ( (t->color != CLEAR) && (t->color != UNSHOWN) && (t->flag&LABEL_FACET) )
     if ( strlen(fn) > 0 )
       fprintf(fd," (%c%s) %f %f facenum%d\n",
          (t->flag&FLIPPED_FACET)?'+':'-',fn,
         (DOUBLE)(t->x[0][0]+t->x[1][0]+t->x[2][0])/3,
         (DOUBLE)(t->x[0][1]+t->x[1][1]+t->x[2][1])/3,strlen(fn));
     fe = get_facet_fe(t->f_id);
     if ( valid_id(fe) )
     for ( i = 0 ; i < FACET_EDGES ; i++ , fe = get_next_edge(fe) )
     { edge_id e_id = get_fe_edge(fe);
        char *tv=NULL,*hv=NULL;
        char *en=NULL;
        REAL a;
        int ii,jj,kk;

        if ( t->flag & FLIPPED_FACET ) 
              { kk = 2 - i; ii = (3-i)%3 ; jj = (5-i)%3; }
        else { kk = i; ii = i; jj = (i+1)%3; }
        if (t->ecolor[kk] == CLEAR) continue;
        if ( (gridflag<0) &&  (((t->etype[kk]&EBITS) == INVISIBLE_EDGE)
                                          || (t->etype[kk] & SPLITTING_EDGE)))
             continue;
        switch ( labelflag )
        { case LABEL_ID:
          {  tv = ELNAME(get_edge_tailv(e_id));
             hv = ELNAME1(get_edge_headv(e_id));
             en = ELNAME2(e_id);
             break; 
          }
          case LABEL_ORIG: 
          {  
             tv = ELNAME(get_original(get_edge_tailv(e_id)));
             hv = ELNAME1(get_original(get_edge_headv(e_id)));
             en = ELNAME2(get_original(e_id));
             break;
          }
        }     
        if ( strlen(tv) > 5 ) tv[5] = 0;
        if ( strlen(hv) > 5 ) hv[5] = 0;
        if ( strlen(en) > 5 ) en[5] = 0; /* max 5 chars in labels */

        if ( tv && (strlen(tv) > 0) )
        fprintf(fd,"(%s) %f %f vertex%1d\n",tv,(DOUBLE)t->x[ii][0],
                (DOUBLE)t->x[ii][1],strlen(tv));
        if ( hv && (strlen(hv) > 0) )
        fprintf(fd,"(%s) %f %f vertex%1d\n",hv,(DOUBLE)t->x[jj][0],
                (DOUBLE)t->x[jj][1],strlen(hv));

        a = .35;
        if ( en && (strlen(en) > 0) )
        { 
          if ( inverted(e_id) )
           fprintf(fd," (%s) %f %f edgenum%1d\n",en,
            (DOUBLE)((1-a)*t->x[ii][0]+a*t->x[jj][0]),
            (DOUBLE)((1-a)*t->x[ii][1]+a*t->x[jj][1]),strlen(en));
          else
           fprintf(fd," (%s) %f %f edgenum%1d\n",en,
            (DOUBLE)(a*t->x[ii][0]+(1-a)*t->x[jj][0]),
            (DOUBLE)(a*t->x[ii][1]+(1-a)*t->x[jj][1]),strlen(en));
        }

     }
  }
}

/*************************************************************
*
*  Function:  ps_finish()
*
*  Purpose:    End PostScript output.
*/

void ps_finish()
{ int i;

  /* do text */
  fputs("\n/Helvetica findfont .10 scalefont setfont\n",fd);
  fputs("0 setgray\n",fd);
  for ( i = 0 ; i < MAXTEXTS ; i++ )
  {
    REAL xspot = -1.4+2.8*text_chunks[i].start_x;
    REAL yspot = -1.4+2.8*text_chunks[i].start_y;
    if ( text_chunks[i].text )
    { char *c;
      fprintf(fd,"%f %f moveto (",xspot,yspot);
      for ( c = text_chunks[i].text ; *c ; c++ )
      { if ( *c == '\n' )
        { fprintf(fd,") show\n");
          yspot -= .10;
          fprintf(fd,"%f %f moveto (",xspot,yspot);
        }
        else 
          fprintf(fd,"%c",*c);
      }
      fprintf(fd,") show\n");
    }
  }

  fputs("\nshowpage\n\n",fd);
  fputs("%%EOF\n",fd);
  fclose(fd);
  ps_file_name[0] = 0;
  temp_free((char*)edgeredrawhead);
  temp_free((char*)edgeredrawlist);
}
