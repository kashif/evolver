/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*************************************************************
*
*  File: display.c        
*
*  Purpose:  Overall control of graphics file output.
*/

#include "include.h"


/**************************************************************
*
*  Function: display_file()
*
*  Purpose:  Makes graphical output display file of
*                current object.
*/

void display_file(arg)
int arg; /* menu choice; -1 to invoke user dialog */
{
  char response[100];
  REAL val;

  if ( web.zoom_radius < 9000.0 ) 
     { sprintf(errmsg,"Inner clip radius (%g): ",(DOUBLE)inner_clip_rad);
        prompt(errmsg,response,sizeof(response));
        if ( const_expr(response,&val) > 0 )
          { inner_clip_rad = val;
             inner_clip_flag = 1;
          }
     }

  /* get user choice */
  if ( arg < 0 )
  {
     outstring("Choose output graphics format: \n");
     outstring("1. Pixar file \n");
     outstring("2. OOGL file\n");
     outstring("3. PostScript file\n");
     outstring("4. Triangle file \n");
     outstring("5. Softimage file \n");
     outstring("6. OFF file \n");
     outstring("7. binary OFF file (for evmovie) \n");
#if defined(OOGL) && !defined(WIN32)
     outstring("8. Start simultaneous geomview  \n");
     outstring("9. End simultaneous geomview  \n");
#endif
     outstring("A. Start OOGL pipe (you must start reader)  \n");
     outstring("B. End OOGL pipe\n");
     outstring("0. cancel  \n");
     prompt("Choice: ",response,sizeof(response));
     if ( response[0] == '3' )
     { ps_colorflag = gridflag = labelflag = crossingflag = -1; }
     do_gfile(response[0],NULL);
  }
  else 
  { if ( arg == 3 )
     { ps_colorflag = gridflag = labelflag = crossingflag = -1; }
    do_gfile('0'+arg,NULL);
  }
}

/***********************************************************************
*
* Function: do_gfile()
*
* purpose: Set up graphics function pointers and invoke proper
*          graphics drivers.
*/

void do_gfile(choice,fname)
int choice;
char *fname;
{
  /* for saving old graphics state */
  void (*old_edge)ARGS((struct tsort *)); 
  void (*old_facet)ARGS((struct tsort *));
  void (*old_gfacet)ARGS((struct graphdata *,facet_id)); 
  void (*old_gedge)ARGS((struct graphdata *,edge_id));
  void (*old_init)ARGS((void));
  void (*old_finish)ARGS((void));
  void (*old_start)ARGS((void));
  void (*old_end)ARGS((void));  

  ENTER_GRAPH_MUTEX      
  if ( torus_display_mode == TORUS_DEFAULT_MODE ) ask_wrap_display();
  switch ( toupper(choice) )
  {
    case 0: /* null file with painter, for bounding box */
       /* save old graphics functions */
       old_init = init_graphics;
       old_finish = finish_graphics;
       old_edge = display_edge;
       old_facet = display_facet;
       old_start = graph_start;
       old_end    = graph_end;
       old_gfacet = graph_facet;
       old_gedge  = graph_edge;

       /* set null graphics functions */
       init_graphics = null_function;
       finish_graphics = null_function;
       graph_start = painter_start;
       graph_edge  = painter_edge;
       display_edge = (void (*)ARGS((struct tsort *))) null_function;
       graph_facet = painter_facet;
       display_facet =  (void (*)ARGS((struct tsort *)))null_function;
       graph_end = painter_end;
       need_bounding_box = 1;
       edgewidths = otherwidths;

       /* do output */
       graphgen();

       /* restore old graphics */
       init_graphics = old_init;
       finish_graphics = old_finish;
       display_edge = old_edge;
       display_facet = old_facet;
       graph_start = old_start;
       graph_end    = old_end;
       graph_facet = old_gfacet;
       graph_edge  = old_gedge;
       need_bounding_box = 0;

       break;
 
  case '1':
  case 'P':  /* Pixar */
       if ( web.representation != SOAPFILM )
          kb_error(1001,"Will do Pixar file only for SOAPFILM model.\n",
                RECOVERABLE);
       /* fall through, since MinneView same as Pixar */
  case '2':
  case 'M':  /* OOGL file */
       if ( web.representation != SOAPFILM )
          kb_error(1002,"Will do OOGL file only for SOAPFILM model.\n",
                RECOVERABLE);
       old_start = graph_start;
       old_end    = graph_end;
       old_gfacet = graph_facet;
       graph_start = pix_start;
       graph_facet = pix_facet;
       graph_end    = pix_end;

       /* do output */
       graphgen();

       /* restore old graphics */
       graph_start = old_start;
       graph_end    = old_end;
       graph_facet = old_gfacet;
       break;
 
  case '3':
  case 'S': /* PostScript */
       /* save old graphics functions */
       old_init = init_graphics;
       old_finish = finish_graphics;
       old_edge = display_edge;
       old_facet = display_facet;
       old_start = graph_start;
       old_end    = graph_end;
       old_gfacet = graph_facet;
       old_gedge  = graph_edge;

       /* set PostScript functions */
       init_graphics = ps_init;
       finish_graphics = ps_finish;
       graph_start = painter_start;
       graph_edge  = painter_edge;
       display_edge = ps_edge;
       graph_facet = painter_facet;
       display_facet = ps_facet;
       graph_end = painter_end;
       need_bounding_box = 1;
       edgewidths = pswidths;
       graph_capabilities = web.torus_display_period ? 0 : GC_ARCS;

       /* do output */
       graphgen();

       /* restore old graphics */
       init_graphics = old_init;
       finish_graphics = old_finish;
       display_edge = old_edge;
       display_facet = old_facet;
       graph_start = old_start;
       graph_end    = old_end;
       graph_facet = old_gfacet;
       graph_edge  = old_gedge;
       need_bounding_box = 0;
       graph_capabilities = 0;

       break;
 
  case '4':
  case 'F': /* triangle file output */
       /* save old graphics functions */
       old_init = init_graphics;
       old_finish = finish_graphics;
       old_edge = display_edge;
       old_facet = display_facet;
       old_start = graph_start;
       old_end    = graph_end;
       old_gfacet = graph_facet;
       old_gedge  = graph_edge;

       /* set functions */
       init_graphics = fil_init;
       finish_graphics = fil_finish;
       if ( web.representation == STRING )
          {
            graph_start = fil_init;
            graph_edge  = painter_edge;
            display_edge = fil_edge;
            graph_end    = fil_finish;
          }
       else
          { 
            graph_start = painter_start;
            graph_facet = painter_facet;
            display_facet = fil_facet;
            graph_end = painter_end;
          }
       edgewidths = otherwidths;

       /* do output */
       graphgen();

       /* restore old graphics */
       init_graphics = old_init;
       finish_graphics = old_finish;
       display_edge = old_edge;
       display_facet = old_facet;
       graph_start = old_start;
       graph_end    = old_end;
       graph_facet = old_gfacet;
       graph_edge  = old_gedge;
       break;

    case '5': /* Softimage file */
       softimage();
       break;

    case '6': /* OFF format file */
       old_start = graph_start;
       old_end    = graph_end;
       old_gfacet = graph_facet;
       old_gedge = graph_edge;
       graph_start = OFF_start;
       graph_edge = OFF_edge;
       graph_facet = OFF_facet;
       graph_end    = OFF_end;

       /* do output */
       graphgen();

       /* restore old graphics */
       graph_start = old_start;
       graph_end    = old_end;
       graph_facet = old_gfacet;
       graph_edge = old_gedge;
       break;
 
    case '7': /* binary OFF format file, for evmovie*/
       binary_off_filename = fname;
       old_start = graph_start;
       old_end    = graph_end;
       old_gfacet = graph_facet;
       old_gedge = graph_edge;
       graph_start = binary_OFF_start;
       graph_edge = binary_OFF_edge;
       graph_facet = binary_OFF_facet;
       graph_end    = binary_OFF_end;

       /* do output */
       graphgen();

       /* restore old graphics */
       graph_start = old_start;
       graph_end    = old_end;
       graph_facet = old_gfacet;
       graph_edge = old_gedge;
       break;

    case '8': /* interactive geomview */
       Begin_geomview(fname);
       go_display_flag = 1;  /* default autodisplay */
       break;

    case '9': /* end geomview */
    case 'B': /* end geomview */
       if ( geomview_flag || geompipe_flag ) End_geomview();
       go_display_flag = 0;  /* autodisplay off */
       geompipe_flag = GEOM_TO_GEOMVIEW;
       break;

    case 'A': /* geomview to named pipe */
       geompipe_flag = GEOM_NAMED_PIPE;
       Begin_geomview(fname);
       go_display_flag = 1;  /* default autodisplay */
       break;

    case 'C': /* geomview to command */
       geompipe_flag = GEOM_PIPE_COMMAND;
       Begin_geomview(fname);
       go_display_flag = 1;  /* default autodisplay */
       break;
    case 'q':
    case '0': break;

    default: outstring("Invalid choice.\n"); break;
  }
  LEAVE_GRAPH_MUTEX 
  inner_clip_flag = 0; /* turn off inner clipping */
}

/*****************************************************************
*
*  function: gray_level()
* 
*  purpose: compute gray level for facet by normal orientation
*  return value 0 < gray < 1.  Scaling depends on internal
*  variable "brightness", which is mid-level gray.
*
*/

REAL gray_level(normal)
float *normal;
{ REAL cosine;
  REAL denom;
  denom = sqrt(dotf(normal,normal,3));
  if ( denom == 0.0 ) return 0.0;
  cosine = normal[1]/denom;
  if ( (REAL)normal[2] < 0.0 ) return brightness - 0.9*(1-brightness)*cosine;
  return brightness + 0.9*(1-brightness)*cosine;
}

/********************************************************************
*
*  function: generate_transforms
*
*  purpose:  generate viewing transforms from generators to 
*                desired depth
*/

static int trans_max = 500;
static REAL ***work_transforms;

void generate_transforms(depth)
int depth;
{ int start,stop; /* range of transforms to apply gens to */
  int i,j,k,m,n,level;
  MAT2D(temp_mat,MAXCOORD+1,MAXCOORD+1);

  /* clean up old stuff */
  view_transforms = NULL;
  transform_count = 0;
  transform_depth = depth;
  if ( depth < 1 ) return;
  trans_max = 50;

  /* new stuff */
  work_transforms = dmatrix3(depth+1,SDIM+1,SDIM+1);
  matcopy(work_transforms[0],identmat,SDIM+1,SDIM+1);

restart:
  if ( view_transforms ) free_matrix3(view_transforms);
  view_transforms = dmatrix3(trans_max,SDIM+1,SDIM+1);
  matcopy(view_transforms[0],identmat,SDIM+1,SDIM+1);

  /* initialize generation */
  for ( k = 0 ; k < transform_gen_count ; k++ )
     matcopy(view_transforms[k+1],view_transform_gens[k],SDIM+1,SDIM+1);
  transform_count = 1+transform_gen_count;

  /* apply all generators to transforms of last generation */
  stop = 1;
  for ( level = 2 ; level <= depth ; level++ )
  { start = stop; stop = transform_count;
    for ( n = start ; n < stop ; n++ )
     for ( i = 0 ; i < transform_gen_count ; i++ )
     { mat_mult(view_transform_gens[i],view_transforms[n],
          view_transforms[transform_count],SDIM+1,SDIM+1,SDIM+1);
       /* see if already have, by crude linear search */
       for ( j = 0 ; j < transform_count ; j++ )
       { /* compare */
         for ( k = 0 ; k < SDIM + 1 ; k++ )
           for ( m = 0 ; m < SDIM+1 ; m++ )
             if ( fabs(view_transforms[transform_count][k][m]
                     -view_transforms[j][k][m]) > 1e-6 )  
               goto different;
           /* same */ 
           goto same;
different: ;  /* continue search */
       }
       /* now know have new transform */
       transform_count++;
       if ( transform_count >= trans_max ) { trans_max *= 2; goto restart; }
same: ;
    }
  }
  if ( view_transform_det ) myfree((char*)view_transform_det);
  view_transform_det = (int*)mycalloc(transform_count,sizeof(int));

  set_view_transforms_global();
  allocate_transform_colors(transform_count);

  if ( transform_parity ) myfree((char*)transform_parity);
  transform_parity = (int*)mycalloc(transform_count,sizeof(int));
  for ( n = 0 ; n < transform_count ; n++ )
  {
    if ( view_transforms[n][SDIM][SDIM] < 0.0 )
    { transform_colors[n] = SWAP_COLORS;
      transform_colors_flag = 1;   
      for ( i = 0 ; i <= SDIM ; i++ )
        for ( j = 0 ; j <= SDIM ; j++ )
          view_transforms[n][i][j] *= -1;
    }
    else transform_colors[n] = SAME_COLOR;

    matcopy(temp_mat,view_transforms[n],SDIM+1,SDIM+1);
    if ( determinant(temp_mat,SDIM+1) > 0.0 )
       view_transform_det[n] = 1;
    else  view_transform_det[n] = -1;
  }
  free_matrix3(work_transforms);
} /* end generate_transforms() */


/**************************************************************************
*
*  function: transform_gen_expr()
*
*  purpose:  generate viewing transforms from generators and 
*                quasi-regular expression.
*  Expression syntax:
*      G is nonterminal, represents set of transforms
*      G ::=  letter - corresponding generator, a - z, G = {I,a}
*                       but a! generates only {a}
*      G ::=  GG      - all ordered products
*      G ::=  G|G     - union
*      G ::=  nG      - all ordered n-fold products
*      G ::=  (G)     - for precedence, same set
*      Leading ! in string means do not include identity transform, even
*             if generated by other transforms.
*          
*  Sets global variable view_transforms to matrix list allocated with dmatrix3()
*  Sets global variable transform_count
*/

struct stack {        /* parsing stack */
      int type;       /* symbol type */
      int num;        /* if number */
      int count;      /* how many matrices */
      REAL ***mats;   /* set of matrices */
      int *swapcolor; /* list of front-back swap color flags */
    };
#define SMAX 100
/* types */
#define G_START 0
#define G_NUM 1
#define G_SET 2
#define G_OR  3
#define G_LEFT 4
#define G_RIGHT 5
#define G_LBRACKET 6
#define G_SINGLE 7
 
int matcomp ARGS((REAL***,REAL***));

int matcomp(a,b)  /* lexicographic comparison of matrices */
REAL ***a,***b;
{ int i,j;
  REAL *aa,*bb;
  for ( i = 0 ; i <= SDIM ; i++ )
    for ( j = 0,aa = a[0][i],bb = b[0][i] ; j <= SDIM ; j++,aa++,bb++ )
    { if ( *aa < *bb-1e-6 ) return -1;
      else if ( *aa > *bb+1e-6 ) return 1;
    }
  return 0; /* equal, within error */
}

void transform_gen_expr(expr)
char *expr;
{   int errnum = 0;
    struct stack stack[SMAX];
    int stacktop;
    char *c;  /* next character */
    int new_count;
    REAL ***new_mats;
    int i,j,k,n,m;
    int old_count;
    REAL ***old_mats;
    REAL ***id;
    MAT2D(temp_mat,MAXCOORD+1,MAXCOORD+1);
    int no_identity = 0;

    if ( expr[0] == 0 ) /* empty transform string */
    { stacktop = 1 ; stack[1].count = 0; 
      stack[stacktop].mats = dmatrix3(1,SDIM+1,SDIM+1);
      goto skip_stuff;
    }

    stacktop = 0;
    stack[stacktop].type = G_START;
    c =  expr;
    if ( *c == '!' ) 
    { no_identity = 1; c++; }
    while ( *c || (stacktop > 1) )
    { /* test first for reduction */
      if ( stack[stacktop].type == G_SET )
      {  switch ( stack[stacktop-1].type )
         { 
             case G_SET:  /* form product */
                new_count = stack[stacktop].count*stack[stacktop-1].count;
                new_mats = dmatrix3(new_count+1,SDIM+1,SDIM+1);
                for ( m = 0, k = 0 ; k < stack[stacktop-1].count ; k++ )
                 for ( j = 0 ; j < stack[stacktop].count ; j++,m++ )
                    mat_mult(stack[stacktop-1].mats[k],stack[stacktop].mats[j],
                        new_mats[m],SDIM+1,SDIM+1,SDIM+1);
                free_matrix3(stack[stacktop-1].mats);
                free_matrix3(stack[stacktop].mats);
                stacktop--;
                stack[stacktop].count = mat_simplify(new_mats,new_count);
                stack[stacktop].mats = new_mats;
                stack[stacktop].type = G_SET;
                continue;

             case G_SINGLE:  /* single matrix in [...] */
                mat_mult(stack[stacktop-1].mats[1],stack[stacktop].mats[1],
                     temp_mat,SDIM+1,SDIM+1,SDIM+1);
                free_matrix3(stack[stacktop].mats);
                stacktop--;
                matcopy(stack[stacktop].mats[1],temp_mat,SDIM+1,SDIM+1);
                continue;
                
             case G_LBRACKET: /* convert to single */
                stack[stacktop].type = G_SINGLE;
                continue;

             case G_NUM: /* form powers */
                if ( stack[stacktop-1].num < 2 )
                  { stack[stacktop-1] = stack[stacktop]; stacktop--; continue;}
                new_mats = stack[stacktop].mats;
                new_count = stack[stacktop].count;
                for ( k = 1 ; k < stack[stacktop-1].num ; k++ )
                { old_count = new_count;
                  old_mats = new_mats;
                  new_count = new_count * stack[stacktop].count;
                  new_mats = dmatrix3(new_count+1,SDIM+1,SDIM+1);
                  for ( m =0, n = 0 ; n < old_count ; n++ )
                    for ( j = 0 ; j < stack[stacktop].count ; j++,m++ )
                     mat_mult(old_mats[n],stack[stacktop].mats[j],
                        new_mats[m],SDIM+1,SDIM+1,SDIM+1);
                  if ( k > 1 ) free_matrix3(old_mats);
                  new_count = mat_simplify(new_mats,new_count);
                }
                free_matrix3(stack[stacktop].mats);
                stacktop--;
                stack[stacktop].count = new_count;
                stack[stacktop].mats = new_mats;
                stack[stacktop].type = G_SET;
                continue;

             case G_OR: /* form union */     
                if ( *c & (*c != '|') & (*c != ')') ) break; /* go to accept next */
                new_count = stack[stacktop].count+stack[stacktop-2].count;
                new_mats = dmatrix3(new_count+1,SDIM+1,SDIM+1);
                for ( m = 0, k = 0 ; k < stack[stacktop-2].count ; k++,m++ )
                  matcopy(new_mats[m],stack[stacktop-2].mats[k],SDIM+1,SDIM+1);
                for ( j = 0 ; j < stack[stacktop].count ; j++,m++ )
                  matcopy(new_mats[m],stack[stacktop].mats[j],SDIM+1,SDIM+1);
                free_matrix3(stack[stacktop-2].mats);
                free_matrix3(stack[stacktop].mats);
                stacktop -= 2;
                stack[stacktop].count = mat_simplify(new_mats,new_count);
                stack[stacktop].mats = new_mats;
                stack[stacktop].type = G_SET;
                continue;
             } /* end switch */
         }
         else if ( stack[stacktop].type == G_RIGHT )
         { if ( ((stack[stacktop-1].type != G_SET) && 
                     (stack[stacktop-1].type != G_SINGLE)) ||
                     ((stack[stacktop-2].type != G_LEFT)  &&
                     (stack[stacktop-2].type != G_LBRACKET)) )
           { sprintf(errmsg,
                       "Transform expression has unmatched parentheses.\n");
             errnum = 2600;
             goto err_exit;
           }
           stacktop -= 2;
           stack[stacktop].type = G_SET;
           stack[stacktop].count = stack[stacktop+1].count;
           stack[stacktop].mats = stack[stacktop+1].mats;
           continue;
         }

      /* shift */
      if ( *c == 0 )
      { sprintf(errmsg,"Illegal transform expression syntax: %s\n",expr);
        errnum = 2601;
        goto err_exit;
      }
      if ( isalpha(*c) )
      { n = toupper(*c) - 'A';  /* number of generator */
        if ( n >= transform_gen_count )
        { sprintf(errmsg,"No generator '%c'.\n",*c);
          errnum = 2602;
          goto err_exit; /* clean up */
        }
        /* set up set */
        stack[++stacktop].type = G_SET;
        if ( c[1] == '!' ) /* exclude identity */
        { stack[stacktop].count = 1;
          stack[stacktop].mats = dmatrix3(1+1,SDIM+1,SDIM+1);
          matcopy(stack[stacktop].mats[0],view_transform_gens[n],SDIM+1,SDIM+1);
          c++;
        } else /* include identity */
        { stack[stacktop].count = 2;
          stack[stacktop].mats = dmatrix3(2+1,SDIM+1,SDIM+1);
          matcopy(stack[stacktop].mats[0],identmat,SDIM+1,SDIM+1);
          matcopy(stack[stacktop].mats[1],view_transform_gens[n],SDIM+1,SDIM+1);
        }

        if ( transform_gen_swap[n] )
          for ( i = 0 ; i <= SDIM ; i++ )
            for ( j = 0 ; j <= SDIM ; j++ )
              stack[stacktop].mats[1][i][j] *= -1;

        c++;
      }
      else if ( isdigit(*c) )
      { stack[++stacktop].type = G_NUM;
        stack[stacktop].num = atoi(c);
        while ( isdigit(*c) ) c++;
      }
      else if ( *c == '(' ) { stack[++stacktop].type = G_LEFT; c++; }
      else if ( *c == ')' ) { stack[++stacktop].type = G_RIGHT; c++; }
      else if ( *c == '|' ) { stack[++stacktop].type = G_OR; c++; }
      else if ( *c == '[' ) { stack[++stacktop].type = G_LBRACKET; c++; }
      else if ( *c == ']' ) { stack[++stacktop].type = G_RIGHT; c++; }
      else if ( (*c==' ')||(*c=='\t') ) c++ ;
      else { sprintf(errmsg,"Illegal character '%c' in transform expression.\n",
                    *c);
             errnum = 2603;
             goto err_exit;
           }
    }

  /* normal exit */
  if ( (stacktop != 1) || (stack[1].type != G_SET) )
  { sprintf(errmsg,"Illegal transform expression syntax: %s\n",expr);
    errnum = 2604;
    goto err_exit;
  }
  strncpy(transform_expr,expr,sizeof(transform_expr));

skip_stuff:
  /* put identity at the head of the list, if not excluded */
  id = (REAL***)bsearch((char*)&identmat,(char*)stack[1].mats,stack[1].count,
            sizeof(REAL**), FCAST  matcomp);
  if ( no_identity )
  { if ( id )
      *id = stack[1].mats[--stack[1].count];
  }
  else
  { if ( id && (*id != stack[1].mats[0]) )
    { REAL **mat = stack[1].mats[0];
      stack[1].mats[0] = *id;
      *id = mat;
    }
    else if ( !id )
    {
      for ( i = 0 ; i <= SDIM ; i++ )
        for ( j = 0 ; j <= SDIM ; j++ )
        { stack[1].mats[stack[1].count][i][j] = stack[1].mats[0][i][j];
          stack[1].mats[0][i][j] = ( i==j ) ? 1.0 : 0.0;
        }
      stack[1].count++;
    }
  }
  
  ENTER_GRAPH_MUTEX;
  transform_count = stack[1].count;
  if ( view_transforms ) free_matrix3(view_transforms);
  view_transforms = stack[1].mats;
  set_view_transforms_global();
  if ( view_transform_det ) myfree((char*)view_transform_det);
  view_transform_det = (int*)mycalloc(transform_count,sizeof(int));
  allocate_transform_colors(transform_count);
  for ( n = 0 ; n < transform_count ; n++ )
  { if ( view_transforms[n][SDIM][SDIM] < 0.0 )
    { transform_colors[n] = SWAP_COLORS;
      transform_colors_flag = 1;   
      for ( i = 0 ; i <= SDIM ; i++ )
        for ( j = 0 ; j <= SDIM ; j++ )
          view_transforms[n][i][j] *= -1;
    }
    else transform_colors[n] = SAME_COLOR; 

    matcopy(temp_mat,view_transforms[n],SDIM+1,SDIM+1);
    if ( determinant(temp_mat,SDIM+1) > 0.0 )
               view_transform_det[n] = 1;
    else  view_transform_det[n] = -1;
  }
  LEAVE_GRAPH_MUTEX;
  return;

err_exit:
  for ( i = 0 ; i <= stacktop ; i++ )
      if ( stack[i].type ==  G_SET ) free_matrix3(stack[i].mats);
  kb_error(errnum,errmsg,RECOVERABLE);

} /* end transform_gen_expr()  */


/********************************************************************
*
* function: mat_simplify()
*
* purpose: remove redundant matrices during transform_gen_expr()
*
* return value: number of distinct matrices left
*/

int mat_simplify(mats,count)
REAL ***mats;
int count;
{ int newcount;
  int i;

  qsort((char*)mats,count,sizeof(REAL**), FCAST matcomp);
  /* eliminate duplicates */
  for ( newcount = 1, i = 1 ; i < count ; i++ )
     { if ( matcomp(mats+newcount-1,mats+i) != 0 ) 
          mats[newcount++] = mats[i];
     }
  return newcount;
} /* end mat_simplify() */


