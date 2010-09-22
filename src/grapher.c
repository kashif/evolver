/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/


/*************************************************************
*
*     file:        grapher.c
*
*     contents:  Functions for control of interactive 
*                    graphics display.
*/



#include "include.h"
#ifndef TRUE
#define  TRUE 1
#define  FALSE 0
#endif

#define dang  (M_PI/30)  /* rotation increment */
static REAL zoomfactor  = 1.2;  /* scale factor */
static REAL low[MAXCOORD],high[MAXCOORD];  /* extreme coordiates */
static REAL mid[MAXCOORD];             /* midpoint of extremes */

/* matrices */
/* image display motion done via homogeneous coordinates */
static REAL **spinl, **tipup;  /* rotation increment matrices */
static REAL **spinr, **tipdown;  /* rotation increment matrices */
static REAL **clockwise, **counterclock;  /* rotation increment matrices */
static REAL **transleft,**transright; /* translation increment matrices */
static REAL **transup,**transdown; /* translation increment matrices */
static REAL **zoom, **shrink;  /* scaling matrices */


static int showflag;

/********************************************************************
*
* function: update_display()
*
* purpose: wrapper for local_update_display()
*/
void update_display()
{
  #ifdef MPI_EVOLVER
  if ( this_task == 0 )
     mpi_update_display();
  #endif

  local_update_display();
}

/********************************************************************
*
* function: local_update_display()
*
* purpose: Check for necessity of re-displaying surface.
*
*/

void local_update_display()
{
  graph_timestamp = ++global_timestamp;  /* new surface */
  if ( go_display_flag )
  {
    if ( OOGL_flag ) UpdateOOGL();
    else
      display();
  }
}

/********************************************************************
*
* function: do_show()
*
* purpose: Handles 's' command, displays and switches to 
*          graphics command mode.   
*
*/

void do_show()
{
  char line[100]; /* for reading user commands */
  int old_flag = iterate_flag;

#ifdef MAC_OS_X
  do_show_flag++;  /* kludge so mac_exec_commands() does do_show() again */
#endif

#ifndef OPENGL
  /* to prevent unnecessary recalculation of display */
  graph_timestamp = ++global_timestamp;  /* new surface */
#endif

  if ( torus_display_mode == TORUS_DEFAULT_MODE ) ask_wrap_display();

  /* main loop */
  showflag = 1;
  do
    { 
      iterate_flag = 2;
      if ( showflag ) display();
      showflag = 1; /* default to show next time around, unless option
                             below decides otherwise */
#ifdef MOTIF
      return;
#endif
      if ( prompt("Graphics command: ",line,sizeof(line)) == EOF ) break;
     }
    while ( view_transform(line) );
    iterate_flag = old_flag;
}

/********************************************************************
*
* function: ask_wrap_display()
*
* purpose: Dialog asking for symmetry group display mode.
*
*/
void ask_wrap_display()
{
  if ( commandfd != stdin ) return;
  if ( web.torus_flag )
    {
      char response[100];
      if ( web.skel[BODY].count == 0 )
         prompt("Display raw facets or clipped cell? (0,2): ",response,sizeof(response));
      else
      prompt("Display raw facets, connected bodies or clipped cell? (0,1,2): ",
          response,sizeof(response));
      switch ( response[0] )
         {
            case '0' : torus_display_mode = TORUS_RAW_MODE;
                       web.torus_body_flag = 0; 
                       web.torus_clip_flag = 0; 
                       break;
            case '1' : 
                       if ( web.skel[BODY].count == 0 )
                          kb_error(1042,"There are no bodies to display connectedly.\n",
                                WARNING);
                       else { web.torus_body_flag = 1; 
                              web.torus_clip_flag = 0;
                              torus_display_mode = TORUS_CONNECTED_MODE;}
                       break;
            case '2' : 
                       web.torus_body_flag = 0; 
                       web.torus_clip_flag = 1; 
                       torus_display_mode = TORUS_CLIPPED_MODE;
                       break;
         }
    }

  else if ( web.symmetry_flag )
    {
      char response[100];
      if ( web.skel[BODY].count == 0 )
          { torus_display_mode = TORUS_RAW_MODE; return; }
      prompt("Display raw cell or connected bodies? (0,1): ",response,sizeof(response));
      switch ( response[0] )
         {
            case '0' : torus_display_mode = TORUS_RAW_MODE;
                       web.torus_body_flag = 0; 
                       web.torus_clip_flag = 0; 
                       break;
            case '1' : 
                       if ( web.skel[BODY].count == 0 )
                          kb_error(1043,"There are no bodies to display connectedly.\n",
                             WARNING);
                       else { web.torus_body_flag = 1; 
                                 web.torus_clip_flag = 0;  
                                 torus_display_mode = TORUS_CONNECTED_MODE;}
                       break;
         }
    }
}

/********************************************************************
*
* function: view_transform()
*
* purpose: parse and execute "graphics command" input.
*
*/

int view_transform(string)
char *string;
{
  char *c;
  size_t legal; /* number of legal characters at start of string */

  /* test for illegal characters */
  legal =  strspn(string,
          "0123456789.+-udrlcCRmzsABDxqtvwbeETH?h\034\035\036\037\033\133\n\r");
  if ( legal != strlen(string) )
  { sprintf(msg,"Illegal character in graphics command: %c",string[legal]);
    kb_error(1044,msg,WARNING);
    showflag = 0;  /* don't reshow */
    return 1;
  }

  for ( c = string ; *c ; c++ )
  { int reps = 1;  /* repetition count */
    REAL val = 0.0;  /* for arbitrary rotations */
    int decflag = 0;  /* whether have real number for angle or other */
    char *cc = c;
  
    if ( isdigit(*c) )
        reps = atoi(c);
    if ( isdigit(*c) || (*c=='.') || (*c=='-'))
    { val = atof(c);
      if ( *cc == '.' ) decflag = 1;
      cc = c+1;
      while ( isdigit(*cc) || (*cc=='.'))
           { if ( *cc == '.' ) decflag = 1;
              cc++;
           }
    }
    if ( decflag ) { reps = 1; c = cc; }
    else if ( *c == '-' ) { /* '-' for color decrement */ }
    else { c = cc;  val = 6.0;  /* default angle */ }

    while ( reps-- > 0 )
     switch ( *c )
     {
        case 0: return 1;

        case 'u':
         if ( !decflag ) val = 6.0;
         set_tipup(val*M_PI/180); 
         mat_mult(tipup,view,view,HOMDIM,HOMDIM,HOMDIM); break;  

        case 'd': 
         if ( !decflag ) val = 6.0;
         set_tipdown(val*M_PI/180); 
         mat_mult(tipdown,view,view,HOMDIM,HOMDIM,HOMDIM); break;  

        case 'r': 
         if ( !decflag ) val = 6.0;
         set_spinr(val*M_PI/180); 
         mat_mult(spinr,view,view,HOMDIM,HOMDIM,HOMDIM); break;  

        case 'l': 
         if ( !decflag ) val = 6.0;
         set_spinl(val*M_PI/180); 
         mat_mult(spinl,view,view,HOMDIM,HOMDIM,HOMDIM); break;  

        case 'c': 
         if ( !decflag ) val = 6.0;
         set_clockwise(val*M_PI/180); 
         mat_mult(clockwise,view,view,HOMDIM,HOMDIM,HOMDIM); 
         break;  

        case 'C': 
         if ( !decflag ) val = 6.0;
         set_counterclockwise(val*M_PI/180); 
         mat_mult(counterclock,view,view,HOMDIM,HOMDIM,HOMDIM); 
         break;  

        case 'z': 
         if ( !decflag ) val = 1.2;
         set_zoom(val);
         mat_mult(zoom ,view,view,HOMDIM,HOMDIM,HOMDIM); break;  

        case 's': 
         if ( !decflag ) val = 1.2;
         set_zoom(1/val);
         mat_mult(zoom,view,view,HOMDIM,HOMDIM,HOMDIM); break;  

        /* MS-DOS arrow keys for translation */
        case 30: 
         transup[SDIM>2?2:1][HOMDIM-1] = decflag ? val : 0.25;
         mat_mult(transup,  view,view,HOMDIM,HOMDIM,HOMDIM);
         break;

        case 31: 
         transdown[SDIM>2?2:1][HOMDIM-1] = decflag ? -val : -0.25;
         mat_mult(transdown,view,view,HOMDIM,HOMDIM,HOMDIM);
         break;

        case 28: 
         transright[SDIM>2?1:0][HOMDIM-1] = decflag ? val : 0.25;
         mat_mult(transright,view,view,HOMDIM,HOMDIM,HOMDIM);
         break;

        case 29: 
         transleft[SDIM>2?1:0][HOMDIM-1] = decflag ? -val : -0.25;
         mat_mult(transleft,view,view,HOMDIM,HOMDIM,HOMDIM);
         break;

        case 0x1b : /* ANSI arrow keys for translation */
          if ( *(++c) != 0x5B )
             { if ( isprint(*c) )
                 sprintf(msg,"Unrecognized character:  %c\n",*c);
                else sprintf(msg,"Unrecognized character: 0x%04X\n",*c);
                outstring(msg);
                break;
             }
          switch ( *(++c) )
            {
                case 0x41: 
                 transup[SDIM>2?2:1][HOMDIM-1] = decflag ? val : 0.25;
                 mat_mult(transup,  view,view,HOMDIM,HOMDIM,HOMDIM);
                 break;

                case 0x42: 
                 transdown[SDIM>2?2:1][HOMDIM-1] = decflag ? -val : -0.25;
                 mat_mult(transdown,view,view,HOMDIM,HOMDIM,HOMDIM);
                 break;

                case 0x43: 
                 transright[SDIM>2?1:0][HOMDIM-1] = decflag ? val : 0.25;
                 mat_mult(transright,view,view,HOMDIM,HOMDIM,HOMDIM);
                 break;

                case 0x44: 
                 transleft[SDIM>2?1:0][HOMDIM-1] = decflag ? -val : -0.25;
                 mat_mult(transleft,view,view,HOMDIM,HOMDIM,HOMDIM);
                 break;

                default:
                 if ( isprint(*c) )
                     sprintf(msg,"Unrecognized character:  %c\n",*c); 
                 else  sprintf(msg,"Unrecognized character: 0x%04X \n",*c); 
                     outstring(msg);
                     break; 
             }
          break;

        case 'R':
          if ( decflag ) /* particular scaling */
             {  int i;
                 matcopy(view,identmat,HOMDIM,HOMDIM);
                 for ( i = 0 ; i < HOMDIM-1 ; i++ ) 
                    { view[i][i] = val;
                    }
             }
          else resize(); 
          reps = 0;  graph_timestamp = ++global_timestamp;
          break;

        case 'm': /* middle, for centering */
          { do_gfile(0,NULL); /* get bounding box */
            if ( SDIM == 2 )
             { view[0][HOMDIM-1] -= (bbox_maxx+bbox_minx)/2;
               view[1][HOMDIM-1] -= (bbox_maxy+bbox_miny)/2;
             } else
             { view[1][HOMDIM-1] -= (bbox_maxx+bbox_minx)/2;
               view[2][HOMDIM-1] -= (bbox_maxy+bbox_miny)/2;
             }
             break;
          }

        case 'x':
        case 'q': return 0;      

        case 't': 
              if ( !web.symmetry_flag ) break;
              ask_wrap_display(); graph_timestamp = ++global_timestamp;
             reps = 0; break;

        case 'B': bdry_showflag = !bdry_showflag; 
                  graph_timestamp = ++global_timestamp;
                  reps = 0; break;

        case 'v': ridge_color_flag = !ridge_color_flag; reps = 0;
                   graph_timestamp = ++global_timestamp;break;

        case 'w': no_wall_flag = !no_wall_flag; reps = 0;  
                  graph_timestamp = ++global_timestamp;break;

        case 'b': box_flag = !box_flag; reps = 0; 
                  graph_timestamp = ++global_timestamp; break;

        case 'e': edgeshow_flag = !edgeshow_flag; 
                  graph_timestamp = ++global_timestamp; break;

        case 'E': triple_edgeshow_flag = !triple_edgeshow_flag; 
                  graph_timestamp = ++global_timestamp; break;

        case 'T': transforms_flag = !transforms_flag; 
                  graph_timestamp = ++global_timestamp; break;

        case '+': fillcolor++;
                  sprintf(msg,"fillcolor %d\n",fillcolor);
                  outstring(msg); 
                  graph_timestamp = ++global_timestamp;
                  reps = 0; break;

        case '-': fillcolor--;
                  sprintf(msg,"fillcolor %d\n",fillcolor);
                  outstring(msg); 
                  graph_timestamp = ++global_timestamp;
                  reps = 0; break;

        case 'H': web.hide_flag = !web.hide_flag; reps = 0; 
                  graph_timestamp = ++global_timestamp;
                  break;

        case '?': 
        case 'h':
                  graph_help();
                  showflag = 0;
                  reps = 0; break;

        case '\n': case '\r': break;
        default:  
                  if ( isprint(*c) )
                     sprintf(msg,"Unrecognized letter: %c\n",*c);
                  else  sprintf(msg,"Unrecognized character: 0x%04x \n",*c);
                  outstring(msg);
                  reps = 0; showflag = 0;
                  break;

      }
  }

  return 1;
} /* end view_transform() */

/********************************************************************
*
* function: init_view()
*
* purpose: Initialize viewing and transform matrices.
*
*/
void init_view()
{
  int i;

  view = dmatrix(0,SDIM,0,SDIM); 

  if ( identmat == NULL )
  { /* first time set-up */

    /* set up identity matrix */
    identmat = perm_matrix2(MAXCOORD+1,MAXCOORD+1); 
    for ( i = 0 ; i <= MAXCOORD ; i++ )
      identmat[i][i] = 1.0;


    to_focus = perm_matrix2(MAXCOORD+1,MAXCOORD+1); 
    from_focus = perm_matrix2(MAXCOORD+1,MAXCOORD+1); 
  
    /* set rotation matrices */
    spinr = perm_matrix2(MAXCOORD+1,MAXCOORD+1);
    matcopy(spinr,identmat,SDIM+1,SDIM+1);
    spinr[0][0] = spinr[1][1] = cos(dang);
    spinr[0][1] = -(spinr[1][0] = sin(dang));
  
    spinl = perm_matrix2(MAXCOORD+1,MAXCOORD+1);
    matcopy(spinl,identmat,SDIM+1,SDIM+1);
    spinl[0][0] = spinl[1][1] = cos(dang);
    spinl[0][1] = -(spinl[1][0] = -sin(dang));
  
    tipup = perm_matrix2(MAXCOORD+1,MAXCOORD+1);
    matcopy(tipup,identmat,SDIM+1,SDIM+1);
    tipup[0][0] = tipup[2][2] = cos(dang);
    tipup[0][2] = -(tipup[2][0] = sin(dang));
  
    tipdown = perm_matrix2(MAXCOORD+1,MAXCOORD+1);
    matcopy(tipdown,identmat,SDIM+1,SDIM+1);
    tipdown[0][0] = tipdown[2][2] = cos(dang);
    tipdown[0][2] = -(tipdown[2][0] = -sin(dang));
  
    clockwise = perm_matrix2(MAXCOORD+1,MAXCOORD+1);
    matcopy(clockwise,identmat,SDIM+1,SDIM+1);
    clockwise[1][1] = clockwise[2][2] = cos(dang);
    clockwise[1][2] = -(clockwise[2][1] = -sin(dang));
  
    counterclock = perm_matrix2(MAXCOORD+1,MAXCOORD+1);
    matcopy(counterclock,identmat,SDIM+1,SDIM+1);
    counterclock[1][1] = counterclock[2][2] = cos(dang);
    counterclock[1][2] = -(counterclock[2][1] = sin(dang));
  
    /* set magnifying matrix */
    zoom = perm_matrix2(MAXCOORD+1,MAXCOORD+1);
    matcopy(zoom,identmat,SDIM+1,SDIM+1);
    zoom[0][0] = zoom[1][1] = zoom[2][2] = zoomfactor;
  
    /* set shrink matrix */
    shrink = perm_matrix2(MAXCOORD+1,MAXCOORD+1);
    matcopy(shrink,identmat,SDIM+1,SDIM+1);
    shrink[0][0] = shrink[1][1] = shrink[2][2] = 1/zoomfactor;
  
    /* set translation matrices */
    transleft = perm_matrix2(MAXCOORD+1,MAXCOORD+1);
    transup = perm_matrix2(MAXCOORD+1,MAXCOORD+1);
    transright = perm_matrix2(MAXCOORD+1,MAXCOORD+1);  
    transdown = perm_matrix2(MAXCOORD+1,MAXCOORD+1);
  } 
 
  set_view_matrix_global();
  matcopy(to_focus,identmat,SDIM+1,SDIM+1);
  matcopy(from_focus,identmat,SDIM+1,SDIM+1);

}

/********************************************************************
*
* functions: set_*()
*
* purpose:  set key entries of transform matrices.
*
*/
void set_spinr(val)
REAL val;
{
  spinr[0][0] = spinr[1][1] = cos(val);
  spinr[0][1] = -(spinr[1][0] = sin(val));
}

void set_spinl(val)
REAL val;
{
  spinl[0][0] = spinl[1][1] = cos(val);
  spinl[0][1] = -(spinl[1][0] = -sin(val));
}

void set_tipup(val)
REAL val;
{
  tipup[0][0] = tipup[2][2] = cos(val);
  tipup[0][2] = -(tipup[2][0] = sin(val));
}

void set_tipdown(val)
REAL val;
{
  tipdown[0][0] = tipdown[2][2] = cos(val);
  tipdown[0][2] = -(tipdown[2][0] = -sin(val));
}

void set_clockwise(val)
REAL val;
{
  clockwise[1][1] = clockwise[2][2] = cos(val);
  clockwise[1][2] = -(clockwise[2][1] = -sin(val));
}

void set_counterclockwise(val)
REAL val;
{
  counterclock[1][1] = counterclock[2][2] = cos(val);
  counterclock[1][2] = -(counterclock[2][1] = sin(val));
}

void set_zoom(val)
REAL val;
{ zoom[0][0] = zoom[1][1] = val;
  if ( web.sdim >= 3 ) 
     zoom[2][2] = val;
}

/********************************************************************
*
* function: reset_view()
*
* purpose: re-initialize view matrix.
*
*/
void reset_view()
{
  HOMDIM = web.sdim + 1;
  matcopy(transleft,identmat,HOMDIM,HOMDIM);
  matcopy(transdown,identmat,HOMDIM,HOMDIM);
  matcopy(transright,identmat,HOMDIM,HOMDIM);
  matcopy(transup,identmat,HOMDIM,HOMDIM);
  
  if ( SDIM > 2 )
  {
     transright[1][HOMDIM-1] = .25;
     transleft[1][HOMDIM-1] = -.25;
     transup[2][HOMDIM-1] = .25;
     transdown[2][HOMDIM-1] = -.25;
  }
  else  /* show x-y plane */
  {
     transright[0][HOMDIM-1] = .25;
     transleft[0][HOMDIM-1] = -.25;
     transup[1][HOMDIM-1] = .25;
     transdown[1][HOMDIM-1] = -.25;
     shrink[2][2] = 1.0;
     zoom[2][2] = 1.0;
  }
}

/********************************************************************
*
* function: resize()
*
* purpose: Recalculate bounding box of surface.
*          Also initializes clip_view and slice_view if empty.
*
*/

void resize()
{
  int i,j,k;
  vertex_id v_id;
  REAL size;

  /* if domain is torus, get torus fundamental cell in view */
  if ( web.torus_flag )
  {
    for ( i = 0 ; i < SDIM ; i++ )  /* coordinate loop */
    {
      low[i] = high[i] = 0.0;
      for ( j = 0 ; j < SDIM ; j++ ) /* axis loop */
      if ( web.torus_period[j][i] < 0.0 ) low[i] += web.torus_period[j][i];
      else high[i] += web.torus_period[j][i];
    }
    if ( transforms_flag )
    { REAL x[MAXCOORD+1];
      for ( j = 0 ; j < SDIM ; j++ )
      { 
        for ( i = 0 ; i < SDIM ; i++ ) x[i] = web.torus_period[j][i];
        x[SDIM] = 1.0;
        for ( k = 0 ; k < transform_count ; k++ )
        { REAL xx,newx[MAXCOORD+1];
          matvec_mul(view_transforms[k],x,newx,SDIM+1,SDIM+1);
          for ( i = 0 ; i < SDIM ; i++ )
          { xx = newx[i]/newx[SDIM];  /* project */
            if ( xx < low[i] ) low[i] = xx;
            if ( xx > high[i] ) high[i] = xx;
          }
        }
      }
      for ( i = 0 ; i < SDIM ; i++ ) x[i] = 0;
      x[SDIM] = 1.0;
      for ( k = 0 ; k < transform_count ; k++ )
        { REAL xx,newx[MAXCOORD+1];
          matvec_mul(view_transforms[k],x,newx,SDIM+1,SDIM+1);
          for ( i = 0 ; i < SDIM ; i++ )
          { xx = newx[i]/newx[SDIM];  /* project */
            if ( xx < low[i] ) low[i] = xx;
            if ( xx > high[i] ) high[i] = xx;
          }
        }
    }
  }
  else if ( web.symmetry_flag )
  { edge_id e_id;
    for ( i = 0 ; i < SDIM ; i++ )  /* initialize */
    { low[i] = 1e30;
      high[i] = -1e30;
    }
    /* figure out how big window should be  */
    FOR_ALL_EDGES(e_id)
    { REAL *t;
      REAL x[MAXCOORD+1],y[MAXCOORD+1];
      t = get_coord(get_edge_tailv(e_id));
      for ( i = 0 ; i < SDIM ; i++ ) x[i] = t[i];
      (*sym_wrap)(get_coord(get_edge_headv(e_id)),y,get_edge_wrap(e_id));
      x[SDIM] = y[SDIM] = 1.0; /* homogeneous coord */
      for ( i = 0 ; i < SDIM ; i++ )
      { if ( x[i] < low[i] ) low[i] = x[i];
        if ( x[i] > high[i] ) high[i] = x[i];
        if ( y[i] < low[i] ) low[i] = y[i];
        if ( y[i] > high[i] ) high[i] = y[i];
      }
      if ( transforms_flag )
      for ( j = 0 ; j < transform_count ; j++ )
      { REAL xx,newx[MAXCOORD+1];
        matvec_mul(view_transforms[j],x,newx,SDIM+1,SDIM+1);
        for ( i = 0 ; i < SDIM ; i++ )
        { xx = newx[i]/newx[SDIM];  /* project */
          if ( xx < low[i] ) low[i] = xx;
          if ( xx > high[i] ) high[i] = xx;
        }
        matvec_mul(view_transforms[j],y,newx,SDIM+1,SDIM+1);
        for ( i = 0 ; i < SDIM ; i++ )
        { xx = newx[i]/newx[SDIM];  /* project */
          if ( xx < low[i] ) low[i] = xx;
          if ( xx > high[i] ) high[i] = xx;
        }
      } 
    }
  }
  else
  {  
    for ( i = 0 ; i < SDIM ; i++ )  /* initialize */
    { low[i] = 1e30;
      high[i] = -1e30;
    }
    /* figure out how big window should be  */
         
    FOR_ALL_VERTICES(v_id)
    { REAL *x = get_coord(v_id);
      if ( transform_count && transforms_flag )
      { REAL y[MAXCOORD+1];
        for ( i = 0 ; i < SDIM ; i++ ) y[i] = x[i];
        y[SDIM] = 1.0; /* homogeneous coord */
        for ( j = 0 ; j < transform_count ; j++ )
        { REAL xx,newx[MAXCOORD+1];
          matvec_mul(view_transforms[j],y,newx,SDIM+1,SDIM+1);
          if ( fabs(newx[SDIM]) < 1e-12 )
          { sprintf(errmsg,"View transform matrix %d is singular.\n",j+1);
            kb_error(1045,errmsg,WARNING);
          }
          else      
            for ( i = 0 ; i < SDIM ; i++ )
            { xx = newx[i]/newx[SDIM];;
              if ( xx < low[i] ) low[i] = xx;
              if ( xx > high[i] ) high[i] = xx;
            }
        } 
      } /* end transforms */
      else /* just plain vertices */
        for ( i = 0 ; i < SDIM ; i++ )
        { if ( x[i] < low[i] ) low[i] = x[i];
          if ( x[i] > high[i] ) high[i] = x[i];
        }

    }
  }
    
  for ( i = 0 ; i < SDIM ; i++ ) mid[i] = (low[i] + high[i])/2;
  size = high[2] - low[2];
  if ( high[1] - low[1] > size ) size = high[1] - low[1];
  if ( high[0] - low[0] > size ) size = high[0] - low[0];
  
  /* transformation matrix will be set up to scale object into
      [-1,1]^3 cube */

  matcopy(view,identmat,HOMDIM,HOMDIM);
  if ( size != 0.0 )
    for ( i = 0 ; i < HOMDIM-1 ; i++ ) 
     { view[i][i] = 2/size;
       view[i][HOMDIM-1] = -mid[i]*2/size;
     }
  if ( to_focus ) /* for oglgraph.c focus reset */
  { matcopy(to_focus,identmat,HOMDIM,HOMDIM); 
    matcopy(from_focus,identmat,HOMDIM,HOMDIM); 
  } 

  /* see if clip_view and slice_view need defaults */
  if ( (clip_coeff[0][0] == 0.0) &&
       (clip_coeff[0][1] == 0.0) &&
       (clip_coeff[0][2] == 0.0))
  { clip_coeff[0][0] = 1.0;
    clip_coeff[0][3] = mid[0];
  }
  if ( (slice_coeff[0] == 0.0) &&
       (slice_coeff[1] == 0.0) &&
       (slice_coeff[2] == 0.0))
  { slice_coeff[0] = 1.0;
    slice_coeff[3] = mid[0];
  }

  overall_size = size;  /* for anybody who wants to know how big */
  if ( !user_thickness_flag ) thickness = 0.001*size;
}

/********************************************************************
*
*  function: fix_ctm()
*
*    Rotates coordinate transformation matrix according to how mouse 
*    dragged.
*/

void fix_ctm(viewmat,dx,dy)
REAL **viewmat; /* matrix to modify */
REAL dx,dy;  /* pixels mouse dragged */
{
  MAT2D(rot,MAXCOORD+1,MAXCOORD+1);
  REAL alpha;  /* angle around axis */
  REAL theta;    /* tilt of rotation axis */
  int i,j;
  
  for ( i = 0 ; i < HOMDIM ; i++ )
  { for ( j = 0 ; j < HOMDIM ; j++ )
        rot[i][j] = 0.0;
    rot[i][i] = 1.0;
  }
  alpha = sqrt(dx*dx + dy*dy)/300;  /* one radian per 300 pixels */
  if ( dx == 0.0 ) 
  { if ( dy > 0.0 ) theta = M_PI/2;
    else if ( dy < 0.0 ) theta = -M_PI/2;
    else goto ctm_exit;  /* no change */
  }
  else 
  { theta = atan(dy/dx);
    if ( dx < 0.0 ) alpha = - alpha;
  }
  
  if ( SDIM == 2 )
  { 
    /* tilt axis */
    rot[2][2] = 1.0;
    rot[0][0] = rot[1][1] = cos(theta);
    rot[0][1] = sin(theta);
    rot[1][0] = -sin(theta);
    mat_mult(rot,viewmat,viewmat,HOMDIM,HOMDIM,HOMDIM);
  
    /* rotate */
    rot[2][2] = rot[0][0] = cos(alpha);
    rot[2][0] = -sin(alpha);
    rot[0][2] = sin(alpha);
    rot[1][1] = 1.0;
    rot[0][1] = rot[1][0] = 0.0;
    mat_mult(rot,viewmat,viewmat,HOMDIM,HOMDIM,HOMDIM);
  
    /* untilt axis */
    rot[2][2] = 1.0;
    rot[0][0] = rot[1][1] = cos(theta);
    rot[0][1] = -sin(theta);
    rot[1][0] = sin(theta);
    rot[2][0] = rot[0][2] = 0.0;
  }
  else
  {
    /* tilt axis */
    rot[0][0] = 1.0;
    rot[1][1] = rot[2][2] = cos(theta);
    rot[1][2] = sin(theta);
    rot[2][1] = -sin(theta);
    mat_mult(rot,viewmat,viewmat,HOMDIM,HOMDIM,HOMDIM);
  
    /* rotate */
    rot[0][0] = rot[1][1] = cos(alpha);
    rot[0][1] = -sin(alpha);
    rot[1][0] = sin(alpha);
    rot[2][2] = 1.0;
    rot[1][2] = rot[2][1] = 0.0;
    mat_mult(rot,viewmat,viewmat,HOMDIM,HOMDIM,HOMDIM);
  
    /* untilt axis */
    rot[0][0] = 1.0;
    rot[1][1] = rot[2][2] = cos(theta);
    rot[1][2] = -sin(theta);
    rot[2][1] = sin(theta);
    rot[0][1] = rot[1][0] = 0.0;
  }
  mat_mult(rot,viewmat,viewmat,HOMDIM,HOMDIM,HOMDIM);
  
ctm_exit: ;
} /* end fix_ctm() */

