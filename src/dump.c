/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/


/*************************************************************
*
*     file:        dump.c
*
*     Contents:  Functions for making ASCII dump file of surface.
*/

char *color_names[] = {
     "clear",  
     "black",
     "blue",
     "green",
     "cyan",
     "red",
     "magenta",
     "brown",
     "lightgray",
     "darkgray",
     "lightblue",
     "lightgreen",
     "lightcyan",
     "lightred",
     "lightmagenta",
     "yellow",
     "white",
     };
#define COLORNAME(n) (((n)<-1)||((n)>WHITE)?"black":color_names[(n)+1])

#include "include.h"
#include "lex.h"
#include "ytab.h"

void list_forwards ARGS((void));

/********************************************************
*
*  Function: dump()
*
*  Purpose: Dumps configuration to ASCII file in format
*           suitable for read-in.  This is just the 
*           interactive part, which calls do_dump().
*
*/

void dump()
{
  char name[200];
  char defaultname[200];

  /* construct default name */
  strncpy(defaultname,datafilename,sizeof(defaultname)-5);
  strcat(defaultname,".dmp");
  sprintf(msg,"Enter name of dump file (%s): ",defaultname);
  prompt(msg,name,sizeof(name));
  if ( name[0] == 0 ) do_dump(defaultname);
  else do_dump(name);
}

/****************************************************************************
*
* function: print_data_value()
*
* purpose: print string representation of datatype value.
*/

void print_data_value(dest,type,src)
char *dest;
int type;
void *src;
{
    switch ( type )
    { case REAL_TYPE:
          sprintf(dest,
#ifdef LONGDOUBLE
              "%2.*Lg",DPREC,*(REAL *)src);
#else
              "%2.15g",*(REAL *)src);
#endif
          break; 
       case INTEGER_TYPE:
            sprintf(dest,"%d",*(int*)src);
            break;
       case UINT_TYPE:
            sprintf(dest,"%u",*(unsigned int*)src);
            break;
       case CHAR_TYPE:
            sprintf(dest,"%d",*(char*)src);
            break;
       case UCHAR_TYPE:
            sprintf(dest,"%u",*(unsigned char*)src);
            break;
       case SHORT_TYPE:
            sprintf(dest,"%d",*(short int*)src);
            break;
       case USHORT_TYPE:
            sprintf(dest,"%u",*(unsigned short int*)src);
            break;
       case LONG_TYPE:
            sprintf(dest,"%ld",*(long int*)src);
            break;
       case ULONG_TYPE:
            sprintf(dest,"%lu",*(unsigned long int*)src);
            break;
       case PTR_TYPE:
            sprintf(dest,"%p",*(char**)src);
            break;
       case VERTEX_TYPE:
            sprintf(dest,"vertex[%s]",ELNAME(*(element_id*)src));
            break;
       case EDGE_TYPE:
            sprintf(dest,"edge[%s]",ELNAME(*(element_id*)src));
            break;
       case FACET_TYPE:
            sprintf(dest,"facet[%s]",ELNAME(*(element_id*)src));
            break;
       case BODY_TYPE:
            sprintf(dest,"body[%s]",ELNAME(*(element_id*)src));
            break;
       case FACETEDGE_TYPE:
            sprintf(dest,"facetedge[%s]",ELNAME(*(element_id*)src));
            break;
       case BOUNDARY_TYPE:
            sprintf(dest,"%s",web.boundaries[*(int*)src].name);
            break;
       case CONSTRAINT_TYPE:
            sprintf(dest,"%s",get_constraint(*(int*)src)->name);
            break;
       case QUANTITY_TYPE:
            sprintf(dest,"%s",GEN_QUANT(*(int*)src)->name);
            break;
       case INSTANCE_TYPE:
            sprintf(dest,"%s",METH_INSTANCE(*(int*)src)->name);
            break;
       case PROCEDURE_TYPE:
            sprintf(dest,"%s",globals(*(int*)src)->name);
            break;
       default: 
            if ( type >= 0 && type < NUMDATATYPES )
             sprintf(errmsg,"Printing of type \"%s\" not implemented.\n",
              datatype_name[type]);
            else sprintf(errmsg,"Illegal datatype %d\n",type);
            kb_error(3667,errmsg,RECOVERABLE);
     }
}

/***********************************************************************
*
* function: print_array()
*
* purpose: Print array elements in bracket-delimited form.  Used in
*          top_dump and print array.
*/
void print_array(a,datastart)
struct array *a;
void *datastart; /* if not in struct array */
{ int k;
  int spots[100];
  int depth;
  char * spot;

  spot = datastart ? datastart : ((char*)a + a->datastart);

  if ( a->dim == 0 ) 
  {
    print_data_value(msg+strlen(msg),a->datatype,spot);
    strcat(msg,"\n");
    outstring(msg);
    return;
  }

  outstring("{");
  depth = 1;
  spots[depth] = 0;
  for (;;)
  { msg[0] = 0;
    if ( spots[depth] < a->sizes[depth-1] && depth < a->dim )
    { outstring("{"); depth++; spots[depth] = 0; continue; }
    if ( depth == a->dim )
    { for ( k = 0 ; k < a->sizes[depth-1] ; k++ )
      { 
        print_data_value(msg+strlen(msg),a->datatype,spot);
        spot += a->itemsize;
        if ( k < a->sizes[depth-1] - 1 ) strcat(msg,",");
        if ( (strlen(msg) > 60) && (k < a->sizes[depth-1] - 1) ) 
        { outstring(msg); 
          strcpy(msg,"\n ");
        }
      }
      outstring(msg);
    }
    outstring("}");
    depth--;
    if ( depth > 0 && (spots[depth]++ < a->sizes[depth-1]-1) )
    { int i;
      outstring(",\n");
      for ( i = 0 ; i < depth ; i++ ) outstring(" ");
    }
    if ( depth == 0 ) break;

    if ( breakflag ) break;  /* in case of runaway print, user interrupt */
  }
  outstring("\n");
} /* end print_array */


/***********************************************************************
*
* function: print_array_atttribute()
*
* purpose: Print attribute array elements in bracket-delimited form.  Used in
*          element dump.
*/
void print_array_attribute(ex,datastart)
struct extra *ex;
void *datastart; 
{ int k;
  int spots[100];
  int depth;
  char * spot = datastart;
  size_t linelength;

  if ( ex->array_spec.dim == 0 ) /* scalar */
  { print_data_value(msg,ex->type,spot);
    outstring(msg);
    return;
  }

  outstring("{");
  depth = 1;
  spots[depth] = 0;
  linelength = 0;
  for (;;)
  { msg[0] = 0;
    if ( (spots[depth] < ex->array_spec.sizes[depth-1]) && (depth < ex->array_spec.dim) )
    { outstring("{"); depth++; spots[depth] = 0; continue; }
    if ( depth == ex->array_spec.dim )
    { for ( k = 0 ; k < ex->array_spec.sizes[depth-1] ; k++ )
      { print_data_value(msg+strlen(msg),ex->type,spot);
        spot += ex->array_spec.itemsize;
        if ( k < ex->array_spec.sizes[depth-1] - 1 ) 
        { strcat(msg,",");
          if ( linelength+strlen(msg) > 70 )
          { outstring(msg);
            outstring("\n   ");
            linelength = 0;
            msg[0] = 0;
          }
        } 
      }
      outstring(msg);
      linelength += strlen(msg);
    }
    
    outstring("}");
    depth--;
    if ( depth > 0 && (spots[depth]++ < ex->array_spec.sizes[depth-1]-1) )
    { outstring(",");
      if ( linelength > 70 )
      { outstring("\n   ");
        linelength = 0;
      }
    } 
    if ( depth == 0 ) break;
  }
  outstring(" ");
} /* end print_array_attribute() */

/************************************************************************
*
* function: top_dump()
*
* purpose: Write out top part of datafile.
*/

void top_dump(fd)
FILE *fd;  /* destination file */
{
  int i,j,k;
  struct gen_quant *q;
  struct method_instance *mi;
  FILE *old_fd = outfd;
  int e_type;

  outfd = fd;

  sprintf(msg,"// datafilename: %s\n",datafilename);
  outstring(msg);
  if ( needed_version[0] )
  { sprintf(msg,"\nEvolver_version \"%s\"  // minimal version needed\n\n",
        needed_version);
    outstring(msg);
  }

  if ( warnings_suppressed_count ) 
    outstring("\n");for ( i = 0 ; i < warnings_suppressed_count ; i++ )
  { sprintf(msg,"suppress_warning %d\n",warnings_suppressed[i]);
    outstring(msg);
  }
  if ( i ) 
    outstring("\n");

  if ( keep_macros_flag )
  { outstring("keep_macros\n");
    dump_macros();
  }

  if ( volume_method_name[0] )
  { sprintf(msg,"volume_method_name \"%s\"\n",volume_method_name); 
    outstring(msg);
  }
  if ( length_method_name[0] )
  { sprintf(msg,"length_method_name \"%s\"\n",length_method_name); 
    outstring(msg);
  }
  if ( area_method_name[0] )
  { sprintf(msg,"area_method_name \"%s\"\n",area_method_name); 
    outstring(msg);
  }

#ifdef LONGDOUBLE
  sprintf(msg,"// Total energy: %2.*Lg\n",DPREC,web.total_energy);
#else
  sprintf(msg,"// Total energy: %2.15g\n",web.total_energy);
#endif 
  outstring(msg);
  /* dynamic libraries */
  for ( i = 0 ; i < MAX_DLL ; i++ )
     if ( dll_list[i].name )
     { sprintf(msg,"LOAD_LIBRARY \"%s\"\n",dll_list[i].name);
       outstring(msg);
     }
  /* model info dump */
  if ( web.representation == SIMPLEX ) outstring("SIMPLEX_REPRESENTATION\n"); 
  if ( SDIM != 3 ) 
      { sprintf(msg,"SPACE_DIMENSION %d\n",SDIM); outstring(msg); }
  if ( web.representation == STRING ) outstring("STRING\n\n");
  else if ( web.representation == SOAPFILM ) outstring("SOAPFILM\n\n");
  else { sprintf(msg,"SURFACE_DIMENSION %d\n\n",web.dimension); outstring(msg); }
  if ( web.modeltype == LINEAR ) outstring("LINEAR\n\n");
  else if ( web.modeltype == LAGRANGE ) 
  { sprintf(msg, "LAGRANGE\nlagrange_order %d\n\n",web.lagrange_order);
    outstring(msg);
  }
  else outstring("QUADRATIC\n\n");
  if ( web.symmetry_flag && !web.torus_flag )
     { sprintf(msg,"SYMMETRY_GROUP \"%s\"\n\n",symmetry_name); outstring(msg); }


  if ( web.motion_flag )  /* do scale before parameters, since scale is
                             parameter attribute */
     { sprintf(msg,"SCALE: %2.15g     FIXED\n\n",(DOUBLE)web.scale); outstring(msg); }
  else if ( web.scale != 0.1 )
     { sprintf(msg,"SCALE: %2.15g\n\n",(DOUBLE)web.scale); outstring(msg); }

  /* global variables */
  for ( i = 0 ; i < web.global_count ; i++ )
  { if ( square_curvature_flag && (i == (square_curvature_param&GLOBMASK)) )
      continue;
    if ( mean_curv_int_flag && (i == (mean_curvature_param&GLOBMASK)) )
      continue;
    if ( sqgauss_flag && (i == (sqgauss_param|GLOBMASK)) ) continue;
    if ( globals(i)->flags & GLOB_LOCALVAR ) continue;
    if ( globals(i)->flags & FILE_VALUES )
    {
      sprintf(msg,"PARAMETER %s ",globals(i)->name);
      outstring(msg); 
      if ( reflevel == 0 )
         sprintf(msg,"PARAMETER_FILE \"%s\" \n",
              globals(i)->value.file.value_file); 
      else    sprintf(msg,"PARAMETER_FILE \"%s\" \n","not dumped");
      outstring(msg); 
    }
    else if ( (globals(i)->flags & ORDINARY_PARAM) &&
          !(globals(i)->flags & (INTERNAL_NAME|QUANTITY_TYPES)) )
      { if ( globals(i)->flags & OPTIMIZING_PARAMETER )
            sprintf(msg,"OPTIMIZING_PARAMETER %s ",globals(i)->name);
        else 
            sprintf(msg,"PARAMETER %s ",globals(i)->name); outstring(msg); 
#ifdef LONGDOUBLE
         sprintf(msg,"= %2.*Lg ",DPREC,globals(i)->value.real); outstring(msg); 
#else
         sprintf(msg,"= %2.15g ",globals(i)->value.real); outstring(msg); 
#endif 
         if ( globals(i)->attr.varstuff.delta != OPTPARAM_DELTA 
           && globals(i)->attr.varstuff.delta != 0.0 )
         {
#ifdef LONGDOUBLE
            sprintf(msg,"pdelta = %2.*Lg ",DPREC,globals(i)->attr.varstuff.delta); outstring(msg); 
#else
            sprintf(msg,"pdelta = %2.15g ",globals(i)->attr.varstuff.delta); outstring(msg); 
#endif 
         }
         if ( globals(i)->attr.varstuff.pscale != 1.0 
           && globals(i)->attr.varstuff.pscale != 0.0 )
         {
#ifdef LONGDOUBLE
            sprintf(msg,"pscale = %2.*Lg ",DPREC,globals(i)->attr.varstuff.pscale); outstring(msg); 
#else
            sprintf(msg,"pscale = %2.15g ",globals(i)->attr.varstuff.pscale); outstring(msg); 
#endif 
         }
         outstring("\n"); 
      }
      else if ( globals(i)->flags & ARRAY_PARAM )        
      { struct array *a = globals(i)->attr.arrayptr;
        if ( !(globals(i)->flags & INTERNAL_NAME) )
          if ( a )  /* declaration might not have been executed */
          {
            sprintf(msg,"define %s",globals(i)->name);
            strcat(msg," ");
            strcat(msg,datatype_name[a->datatype]);
            for ( j = 0 ; j < a->dim ; j++ )
              sprintf(msg+strlen(msg),"[%d]",a->sizes[j]);
            outstring(msg);
            outstring(" =\n");
            print_array(a,NULL);
            outstring("\n");
          }
      }
   }

  list_forwards(); /* constraint names, etc. */
  list_top_procedures(LIST_FULL); /* protos, then full */
  
  if ( web.full_flag ) outstring("TORUS_FILLED\n");  
  else if ( web.torus_flag ) outstring("TORUS\n");
  if ( web.torus_flag || torus_period_expr[0][0].root )
     { outstring("PERIODS\n");
       for ( i = 0 ; i < SDIM ; i++ )
       { for ( j = 0 ; j < SDIM ; j++ )
         { outstring(" "); 
           outstring(print_express(&torus_period_expr[i][j],' '));
         }
         outstring("\n"); 
       }
       outstring("//Numerical values of periods:\n");
       for ( i = 0 ; i < SDIM ; i++ )
       { outstring("// ");
         for ( j = 0 ; j < SDIM ; j++ )
         { 
#ifdef LONGDOUBLE
           sprintf(msg," %*.*Lf ",DWIDTH,DPREC,web.torus_period[i][j]);
#else
           sprintf(msg," %18.15f ",web.torus_period[i][j]);
#endif 
           outstring(msg);
         }
         outstring("\n"); 
       }
       outstring("\n"); 
     }

  if ( torus_display_period_expr[0][0].root )
     { outstring("DISPLAY_PERIODS\n");
       for ( i = 0 ; i < SDIM ; i++ )
       { for ( j = 0 ; j < SDIM ; j++ )
         { outstring(" "); 
           outstring(print_express(&torus_display_period_expr[i][j],' '));
         }
         outstring("\n"); 
       }
       outstring("//Numerical values of periods:\n");
       for ( i = 0 ; i < SDIM ; i++ )
       { outstring("// ");
         for ( j = 0 ; j < SDIM ; j++ )
         { 
#ifdef LONGDOUBLE
           sprintf(msg," %*.*Lf ",DWIDTH,DPREC,web.torus_display_period[i][j]);
#else
           sprintf(msg," %18.15f ",web.torus_display_period[i][j]);
#endif 
           outstring(msg);
         }
         outstring("\n"); 
       }
       outstring("\n"); 

       outstring("DISPLAY_ORIGIN\n");
       for ( i = 0 ; i < SDIM  ; i++ )
       {
#ifdef LONGDOUBLE
           sprintf(msg," %*.*Lf ",DWIDTH,DPREC,web.display_origin[i]);
#else
           sprintf(msg," %18.15f ",web.display_origin[i]);
#endif 
           outstring(msg);
           outstring("\n");
       }
     }

  outstring("\n"); 
  if ( transform_gen_count > 0 )
  { int n = web.torus_flag?transform_gen_count-SDIM:transform_gen_count;
    if ( n > 0 )
    { sprintf(msg,"VIEW_TRANSFORM_GENERATORS %d \n",n); 
      outstring(msg); 
      for ( k = 0 ; k < n ; k++ )
      { if ( transform_gen_swap[k] ) 
          outstring("swap_colors\n");
        for ( i = 0 ; i <= SDIM ;i++ )
        { for ( j = 0 ; j <= SDIM ; j++ )
          { outstring(print_express(&view_transform_gens_expr[k][i][j],' ')); 
            outstring("  ");
          }
          outstring("\n"); 
        }
        outstring("\n"); 
      }
    }
  }
  else if ( transform_count > 1 )
  { sprintf(msg,"VIEW_TRANSFORMS %d \n",transform_count-1); 
    outstring(msg); 
    for ( k = 1 ; k < transform_count ; k++ )
    { if ( transform_colors[k] == SWAP_COLORS )
         outstring("swap_colors\n");
      else if ( transform_colors[k] != SAME_COLOR )
      { sprintf(msg,"color %s\n",COLORNAME(transform_colors[k]));
        outstring(msg);
      }
      for ( i = 0 ; i <= SDIM ;i++ )
      { for ( j = 0 ; j <= SDIM ; j++ )
        { sprintf(msg,"%18.15g  ",(DOUBLE)view_transforms[k][i][j]); 
          outstring(msg); 
        }
        outstring("\n"); 
      }
      outstring("\n"); 
    }
  }
  if ( web.meritfactor != 0.0 )
  { sprintf(msg,"MERIT_FACTOR: %2.15g\n\n",(DOUBLE)web.meritfactor); outstring(msg); }
  if ( web.gravflag )
#ifdef LONGDOUBLE
  { sprintf(msg,"GRAVITY_CONSTANT: %2.*Lg\n\n",DPREC,web.grav_const); outstring(msg); }
#else
  { sprintf(msg,"GRAVITY_CONSTANT: %2.15g\n\n",web.grav_const); outstring(msg); }
#endif 
  if ( web.diffusion_const != 0.0 )
#ifdef LONGDOUBLE
  { sprintf(msg,"DIFFUSION: %2.*Lg\n\n",DPREC,web.diffusion_const); outstring(msg); }
#else
  { sprintf(msg,"DIFFUSION: %2.15g\n\n",web.diffusion_const); outstring(msg); }
#endif 
  if ( autochop_flag )
  { sprintf(msg,"AUTOCHOP %2.15g\n\n",(DOUBLE)autochop_length); outstring(msg); }
  if ( web.homothety )
  { sprintf(msg,"HOMOTHETY %2.15g\n\n",(DOUBLE)homothety_target); outstring(msg); }
  if ( match_id_flag ) outstring("KEEP_ORIGINALS\n\n");
  if ( autopop_flag )
     outstring("AUTOPOP\n\n");
  if ( immediate_autopop_flag )
     outstring("IMMEDIATE_AUTOPOP\n\n");
  if ( autopop_quartic_flag )
     outstring("AUTOPOP_QUARTIC\n\n");
  if ( effective_area_flag )
     outstring("EFFECTIVE_AREA\n\n"); 
  if ( boundary_curvature_flag )
     outstring("BOUNDARY_CURVATURE\n\n"); 
  if ( normal_curvature_flag )
     outstring("NORMAL_CURVATURE\n\n");
  if ( total_time > 0.0 )
     { sprintf(msg,"TOTAL_TIME %g\n\n",(DOUBLE)total_time); outstring(msg); }
  if ( runge_kutta_flag )
     outstring("RUNGE_KUTTA\n\n"); 
  if ( square_curvature_flag )
     { sprintf(msg,"SQUARE_CURVATURE: %2.15g\n\n",
            (DOUBLE)globals(square_curvature_param)->value.real); outstring(msg); }
  if ( sqgauss_flag )
     { sprintf(msg,"SQUARE_GAUSSIAN_CURVATURE: %2.15g\n\n",
            (DOUBLE)globals(sqgauss_param)->value.real); outstring(msg); }
  if ( mean_curv_int_flag )
     { sprintf(msg,"MEAN_CURVATURE_INTEGRAL: %2.15g\n\n",
            (DOUBLE)globals(square_curvature_param)->value.real); outstring(msg); }
  if ( web.wulff_flag )
     { sprintf(msg,"WULFF:      \"%s\"\n\n",web.wulff_name); outstring(msg); }
  if ( phase_flag )
     { sprintf(msg,"PHASEFILE \"%s\"\n\n",phase_file_name); outstring(msg); }
  if ( web.spring_constant != 1.0 )
     { sprintf(msg,"GAP_CONSTANT: %2.15g\n\n",(DOUBLE)web.spring_constant); outstring(msg); }
  if ( web.area_norm_flag )
     { sprintf(msg,"AREA_NORMALIZATION \n\n"); outstring(msg); }
  if ( web.jiggle_flag )
     { sprintf(msg,"JIGGLE\n\n"); outstring(msg); }
  if ( web.temperature != 0.05 )
     { sprintf(msg,"TEMPERATURE: %2.15g\n\n",(DOUBLE)web.temperature); outstring(msg); }
  if ( web.pressure != 0.0 )
     { sprintf(msg,"PRESSURE: %2.15g\n\n",(DOUBLE)web.pressure); outstring(msg); }
  if ( web.maxscale != 1.0 )
     { sprintf(msg,"SCALE_LIMIT: %2.15g\n\n",(DOUBLE)web.maxscale); outstring(msg); }
  if ( web.gauss1D_order != 3 )
     { sprintf(msg,"INTEGRAL_ORDER_1D: %d\n\n",web.gauss1D_order); outstring(msg); }
  if ( (web.dimension > 1) && (web.gauss2D_order != 6) )
     { sprintf(msg,"INTEGRAL_ORDER_2D: %d\n\n",web.gauss2D_order); outstring(msg); }
  if ( web.maxcon )
     { sprintf(msg,"CONSTRAINT_TOLERANCE: %2.15g\n\n",(DOUBLE)web.tolerance); outstring(msg); }
  if ( web.symmetric_content )
     { sprintf(msg,"SYMMETRIC_CONTENT\n\n"); outstring(msg); }

/* Too many problems for too little use
  if ( loc_ordinal(web.zoom_v) >= 0 ) 
     { sprintf(msg,"ZOOM_VERTEX  %s\n\n",ELNAME(web.zoom_v)); outstring(msg); }
  if ( web.zoom_radius < 9000.0 )
     { sprintf(msg,"ZOOM_RADIUS  %2.15g\n\n",(DOUBLE)web.zoom_radius); outstring(msg); }
*/

  if ( klein_metric_flag ) { sprintf(msg,"KLEIN_METRIC\n\n"); outstring(msg); }
  if ( web.conformal_flag )
  { sprintf(msg,"CONFORMAL_METRIC\n"); outstring(msg); 
    /* metric expression */
    outstring(print_express(&web.metric[0][0],'X'));
    outstring("\n\n"); 
  }
  else if ( web.metric_flag && web.metric[0][0].root )
  { sprintf(msg,"METRIC\n"); outstring(msg); 
    /* metric expressions */
    for ( i = 0 ; i < SDIM ; i++ )
    { for ( j = 0 ; j < SDIM ; j++ )
      { outstring(" "); 
        outstring(print_express(&web.metric[i][j],'X'));
      }
      outstring("\n"); 
    }
    outstring("\n"); 
  }

  /* extra attributes for elements */
  for ( e_type = VERTEX ; e_type <= FACETEDGE ; e_type++ )
  { struct extra *ex;
    for ( i = 0, ex = EXTRAS(e_type) ; 
                i < web.skel[e_type].extra_count ; i++, ex++ )
     { if ( !(ex->flags & DUMP_ATTR) ) continue;
        sprintf(msg,"\ndefine %s attribute %s %s",typenames[e_type], ex->name,
          datatype_name[ex->type]);
        for ( j = 0 ; j < ex->array_spec.dim ; j++)
          sprintf(msg+strlen(msg),"[%d]",ex->array_spec.sizes[j]);
        if ( ex->code.start ) 
          sprintf(msg+strlen(msg),
            " // function definition in READ section");
        strcat(msg,"\n");
        outstring(msg);       
     }
  }

  /* view matrix */
  outstring("VIEW_MATRIX \n");
  for ( i = 0 ; i <= SDIM ;i++ )
  { for ( j = 0 ; j <= SDIM ; j++ )
    { sprintf(msg,"%18.15f  ", (DOUBLE)view[i][j]); 
      outstring(msg); 
    }
    outstring("\n"); 
  }

  /* Clipping and slicing planes */
  outstring("clip_coeff = {");
  for ( i = 0 ; i < MAXCLIPS ; i++ )
  { int nonzero = 0;
    for ( j = 0 ; j <= SDIM ; j++ )
      if ( clip_coeff[i][j] ) nonzero = 1;
    if ( nonzero )
    { if ( i > 0 ) outstring(",{");
      else outstring("{");
      for ( j = 0 ; j <= SDIM ; j++ )
      { sprintf(msg,"%8.5f",clip_coeff[i][j]);
        outstring(msg);
        if ( j < SDIM )
          outstring(",");
      }
      outstring("}");
    }
  }
  outstring("}\n\n");

  outstring("slice_coeff = {");
  for ( j = 0 ; j <= SDIM ; j++ )
  { sprintf(msg,"%8.5f",slice_coeff[j]);
    outstring(msg);
    if ( j < SDIM )
      outstring(",");
  }
  outstring("}\n\n");


  if ( mobility_flag ) /* after element attributes, since it can use them */
  { if ( mobility_tensor_flag )
    { outstring("MOBILITY_TENSOR\n"); 
      /* mobility tensor expressions */
      for ( i = 0 ; i < SDIM ; i++ )
      { for ( j = 0 ; j < SDIM ; j++ )
        { outstring(" "); 
          outstring(print_express(&mobility_tensor[i][j],'X'));
        }
        outstring("\n"); 
      }
      outstring("\n"); 
    }
    else
    { outstring("MOBILITY: ");
      outstring(print_express(&mobility_formula,'X'));
      outstring("\n\n"); 
    }
  }

  /* unattached instances, probably from compound quantities */
  for ( k = LOW_INST  ; k < meth_inst_count  ; k++ )
  { mi = METH_INSTANCE(k);
    if ( mi->quant >= 0 ) continue; /* has a quantity */ 
    if ( mi->flags & (Q_DELETED|IMPLICIT_INSTANCE|DEFAULT_INSTANCE) )
       continue;
    list_method_instance(k);
  }

  /* named quantities */
  for (  k = 0 ; k < gen_quant_count ; k++ )
  { q = GEN_QUANT(k);
    if ( q->flags & (DEFAULT_QUANTITY|Q_DELETED) ) continue;
    list_quantity(k);
   }

  if ( hessian_special_normal_expr[0].start )
  { outstring("\nhessian_special_normal_vector\n");
    for ( i = 0 ; i < SDIM ; i++ )
    { sprintf(msg,"c%d: ",i+1); outstring(msg);
      outstring(print_express(hessian_special_normal_expr+i,'x'));
      outstring("\n");
    }
    outstring("\n");
  }
  /* print boundary info */
  for ( i = 0 ; i < web.bdrymax ; i++ )
  { struct boundary * bdry = web.boundaries + i;
    if ( !(bdry->attr & IN_USE) ) continue;
    list_boundary(i);
  }


  /* print constraint info */
  for ( i = 0 ; i < web.maxcon ; i++ )
  { struct constraint *con = get_constraint(i);

    if ( !(con->attr & IN_USE) ) continue;
    list_constraint(i);

  } 

  /* windup */
  outfd = old_fd;
} /* end top_dump() */

/**********************************************************************
*
* function: dump_macros()
*
* purpose: print macros in dumpfile if keep_macros_flag
*
*/

void dump_macros()
{ int n;

  if ( !keep_macros_flag ) return;
  outstring("\n");
  for ( n = 0 ; n < macro_count ; n++ )
  { sprintf(msg,"#define %s ",macros[n].name);
     outstring(msg);
     outstring(macro_subs+macros[n].offset);
     outstring("\n");
  }
  outstring("\n");

}

/**********************************************************************
*
*  function: dump_method_specs()
*
*  purpose: list attributes of method instance
*/

void dump_method_specs(meth)
int meth;
{
  struct method_instance *mi = METH_INSTANCE(meth);
  int j;

  if ( mi->flags & GLOBAL_INST ) outstring(" global ");
  if ( mi->flags & IGNORE_CONSTR ) outstring(" ignore_constraints ");
  if ( mi->flags & IGNORE_FIXED ) outstring(" ignore_fixed ");
  if ( mi->flags & ELEMENT_MODULUS_FLAG )
  {  outstring(" element_modulus ");
     outstring(EXTRAS(mi->type)[mi->elmodulus].name);
  }
  if ( mi->modulus != 1.0 )
     { 
#ifdef LONGDOUBLE
        sprintf(msg," modulus %3.*Lg ",DPREC,mi->modulus);
#else
        sprintf(msg," modulus %3.15g ",mi->modulus);
#endif 
        outstring(msg);
     }
  if ( mi->flags & METH_PARAMETER_1 )
  { 
#ifdef LONGDOUBLE
     sprintf(msg," parameter_1 %3.*Lg ",DPREC,mi->parameter_1);
#else
     sprintf(msg," parameter_1 %3.15g ",mi->parameter_1);
#endif 
     outstring(msg);
  }
  outstring("\n");
  if ( basic_gen_methods[mi->gen_method].spec_flags & SPEC_SCALAR )
    { outstring("scalar_integrand: ");
      outstring(print_express(mi->expr[0],'X'));
      outstring("\n");
    }
  if ( basic_gen_methods[mi->gen_method].spec_flags & SPEC_VECTOR )
    { outstring("vector_integrand: \n");
      for ( j = 0 ; j < SDIM ; j++ )
        { sprintf(msg,"Q%1d: ",j+1); outstring(msg); 
          outstring(print_express(mi->expr[j],'X'));
          outstring("\n");
        }
    }
  if ( basic_gen_methods[mi->gen_method].spec_flags & SPEC_KVECTOR )
    { sprintf(msg,"k_vector_order %d\nvector_integrand: \n",mi->vec_order);
      outstring(msg);
      for ( j = 0 ; j < (SDIM-mi->vec_order)*SDIM ; j++ )
        { sprintf(msg,"Q%1d: ",j+1); outstring(msg); 
          outstring(print_express(mi->expr[j],'X'));
          outstring("\n");
        }
    }
  if ( basic_gen_methods[mi->gen_method].spec_flags & SPEC_2FORM )
    { outstring("form_integrand: \n");
      for ( j = 0 ; j < (SDIM*(SDIM-1))/2 ; j++ )
        { sprintf(msg,"Q%1d: ",j+1); outstring(msg); 
          outstring(print_express(mi->expr[j],'X'));
          outstring("\n");
        }
    }
} /* end dump_method_specs() */

/*************************************************************************
*
* function: vertex_dump()
*
* purpose: write one vertex definition to datafile
*
*/

void vertex_dump(v_id,fd)
vertex_id v_id;
FILE *fd;   /* destination file */
{ int i;
  REAL *x;
  FILE *old_fd = outfd;
  struct extra *ex;
  int attr = get_vattr(v_id);
  vertex_id orig;

  outfd = fd;
        
  if ( !valid_id(v_id) ) return;
  x = get_coord(v_id);
  if ( attr & BOUNDARY )
  { /* print boundary parameters */
    struct boundary *bdry = get_boundary(v_id);
    REAL *param = get_param(v_id);

    sprintf(msg,"%3s",ELNAME(v_id)); outstring(msg); 
    for ( i = 0 ; i < bdry->pcount ; i++ )
#ifdef LONGDOUBLE
    { sprintf(msg,"  %*.*Lg",DWIDTH,DPREC,param[i]); outstring(msg); }
#else
    { sprintf(msg,"  %17.15g",param[i]); outstring(msg); }
#endif 
    sprintf(msg,"  boundary %s ",bdry->name); 
    outstring(msg); 
    sprintf(msg," /* ("); outstring(msg); 
    for ( i = 0; i < SDIM ; i++)
#ifdef LONGDOUBLE
    { sprintf(msg," %*.*Lg",DWIDTH,DPREC,x[i]); outstring(msg); }
#else
    { sprintf(msg," %17.15g",x[i]); outstring(msg); }
#endif 
    sprintf(msg,") */"); outstring(msg); 
  } /* end BOUNDARY parameters */
  else
  { /* print regular coordinates */
    sprintf(msg,"%3s ",ELNAME(v_id)); outstring(msg); 
    for ( i = 0; i < SDIM ; i++)
#ifdef LONGDOUBLE
      { sprintf(msg," %*.*Lg",DWIDTH,DPREC,x[i]); outstring(msg); }
#else
      { sprintf(msg," %17.15g",x[i]); outstring(msg); }
#endif 
  }
  if ( attr & CONSTRAINT )
  { conmap_t * conmap = get_v_constraint_map(v_id);
    struct constraint *con;
    sprintf(msg,"  constraints "); outstring(msg); 
    for ( i = 1 ; i <= (int)conmap[0] ; i++ )
    { con = get_constraint(conmap[i]);
      if ( !(con->attr & GLOBAL) )
      { if ( con->attr & NAMED_THING )
         { sprintf(msg,"%s ",con->name); outstring(msg); }
        else
         { sprintf(msg,"%d ",conmap[i]&CONMASK); outstring(msg); }
      }
    }
  }
  if ( attr & FIXED ) { sprintf(msg, " fixed "); outstring(msg); }
  if ( attr & HIT_PARTNER ) outstring(" hit_partner ");
  if ( attr & AXIAL_POINT ) 
     { sprintf(msg, " axial_point "); outstring(msg); }
  if ( attr & BARE_NAKED )
     { sprintf(msg, " bare "); outstring(msg); }

  orig = get_original(v_id);
  if ( valid_id(orig) && !equal_element(orig,v_id) ) 
  { sprintf(msg," original %s",ELNAME(orig)); outstring(msg); }

  if ( elptr(v_id)->method_count )
  { struct method_instance *mi;
    int meth_offset = EXTRAS(VERTEX)[web.meth_attr[VERTEX]].offset;
    int *methlist = (int*)((char*)elptr(v_id) + meth_offset);
    for ( i = 0 ; i < (int)elptr(v_id)->method_count ; i++ )
      { mi = METH_INSTANCE(abs(methlist[i]));
        if ( GEN_QUANT(mi->quant)->flags & (DEFAULT_QUANTITY|Q_DELETED) )
          continue;
        if ( mi->flags & IMPLICIT_INSTANCE )
           sprintf(msg," %s ",GEN_QUANT(mi->quant)->name);
        else sprintf(msg," %s%c ",mi->name,methlist[i]<0?'-':' '); 
        outstring(msg); 
      }
  }

  /* extra attributes */
  for ( i = 0, ex = EXTRAS(VERTEX) ; i < web.skel[VERTEX].extra_count;
              i++,ex++ )
    { char *at = get_v_extra(v_id,i);
      if ( !(ex->flags & DUMP_ATTR) ) continue;
      sprintf(msg," %s ",ex->name);
      outstring(msg);
      print_array_attribute(ex,at);
    }

  sprintf(msg,"\n"); outstring(msg); 
  outfd = old_fd;
} /* end vertex_dump() */

/***************************************************************************
*
* function: edge_dump()
*
* purpose: write out one edge definition
*
*/

void edge_dump(e_id,fd)
edge_id e_id;
FILE *fd;  /* destination file */
{ int i;
  int attr;
  FILE *old_fd = outfd;
  struct extra *ex;
  edge_id orig;

  outfd = fd;

  if ( !valid_id(e_id) ) return;
  attr = get_eattr(e_id);

  sprintf(msg,"%3s     ",ELNAME(e_id)); outstring(msg); 
  if ( web.modeltype == LAGRANGE )
  {
      vertex_id *v = get_edge_vertices(e_id);
      int num = web.skel[EDGE].ctrlpts;
      for ( i = 0 ; i < num ; i++ )
        { sprintf(msg,"%3s  ",ELNAME(v[i])); outstring(msg); }
  }
  else
  {
    if ( web.representation == SIMPLEX )
     { vertex_id *v = get_edge_vertices(e_id);
        for ( i = 0 ; i <= web.dimension-1 ; i++ )
          { sprintf(msg,"%3s  ",ELNAME(v[i])); outstring(msg); }
     }
    else
    { sprintf(msg,"%3s  %3s    ", ELNAME(get_edge_tailv(e_id)),
                ELNAME1(get_edge_headv(e_id))); 
      outstring(msg); 
    }
    if ( web.modeltype == QUADRATIC )
    { sprintf(msg,"  /*midpt*/ %3s ",ELNAME(get_edge_midv(e_id))); 
      outstring(msg); 
    }
  }

  if ( web.torus_flag ) 
  { WRAPTYPE wrap = get_edge_wrap(e_id);
    WRAPTYPE w = wrap;
    int smallwrap = 1; /* whether can do with - * + */
    for ( i = 0 ; i < SDIM ; i++, w >>= TWRAPBITS )
      switch ( w & WRAPMASK  )
      {
          case  NEGWRAP: case  0:  case  POSWRAP: break;  
          default : 
             smallwrap = 0; 
      }
    if ( !smallwrap )
    { sprintf(msg," wrap 0x%06lX\n",wrap);
      outstring(msg);
    }
    else 
     for ( i = 0 ; i < SDIM ; i++, wrap >>= TWRAPBITS )
      switch ( wrap & WRAPMASK  )
      {
          case  NEGWRAP : sprintf(msg," -"); outstring(msg);   break;
          case  0       : sprintf(msg," *"); outstring(msg);   break;
          case  POSWRAP : sprintf(msg," +"); outstring(msg);   break;
          default : 
              sprintf(errmsg,"Bad wrap %lX on edge %s period %d\n",
                  wrap&WRAPMASK,ELNAME(e_id),i+1);
              kb_error(1005,errmsg,WARNING);
              sprintf(msg," bad"); outstring(msg); 
            break;
      }
  }
  else if ( web.symmetry_flag )
  { WRAPTYPE wrap = get_edge_wrap(e_id);
    sprintf(msg," wrap %ld ",wrap); outstring(msg); 
  }
  if ( (attr & DENSITY) || ((web.representation==STRING) &&
                    (get_edge_density(e_id) != 1.0)))
#ifdef LONGDOUBLE
    { sprintf(msg,"  density %1.*Lg ",DPREC,get_edge_density(e_id)); outstring(msg); }
#else
    { sprintf(msg,"  density %1.15g ",get_edge_density(e_id)); outstring(msg); }
#endif 
  if ( attr & BOUNDARY )
     { struct boundary *bdry = get_edge_boundary(e_id);
       sprintf(msg,"  boundary %s ",bdry->name); 
       outstring(msg); 
     }                    
  if ( attr & CONSTRAINT )
  { conmap_t * conmap = get_e_constraint_map(e_id);
    struct constraint *con;
    sprintf(msg,"  constraints "); outstring(msg); 
    for ( i = 1 ; i <= (int)conmap[0] ; i++ )
    { con = get_constraint(conmap[i]);
      if ( !(con->attr & GLOBAL) )
      { if ( con->attr & NAMED_THING )
         { sprintf(msg,"%s ",con->name); outstring(msg); }
        else
         { sprintf(msg,"%d ",conmap[i]&CONMASK); outstring(msg); }
      }
    }
  }

  if ( attr & NEGBOUNDARY ) outstring(" orientation -1 ");
  if ( attr & FIXED )
      { sprintf(msg,"fixed "); outstring(msg); }
  if ( attr & BARE_NAKED ) { sprintf(msg," bare "); outstring(msg); }
  if ( attr & NO_REFINE ) outstring(" no_refine ");
  if ( attr & NONCONTENT ) outstring(" noncontent ");

  if ( get_edge_color(e_id)  != DEFAULT_EDGE_COLOR )
  { sprintf(msg," color %s ",COLORNAME(get_edge_color(e_id))); outstring(msg); }
  if ( (fd == old_fd) && (web.representation != SIMPLEX) )
  { calc_edge(e_id);
    sprintf(msg," /*length %g*/ ",(DOUBLE)get_edge_length(e_id)); outstring(msg); 
  }
  if ( elptr(e_id)->method_count )
  { struct method_instance *mi;
    int meth_offset = EXTRAS(EDGE)[web.meth_attr[EDGE]].offset;
    int *methlist = (int*)((char*)elptr(e_id) + meth_offset);
    for ( i = 0 ; i < elptr(e_id)->method_count ; i++ )
      { int mm = methlist[i];
        mi = METH_INSTANCE(abs(mm));
        if ( mi->quant >= 0 && 
          (GEN_QUANT(mi->quant)->flags & (DEFAULT_QUANTITY|Q_DELETED)) ) 
          continue;
        if ( mi->quant >= 0 && (mi->flags & IMPLICIT_INSTANCE) )
           sprintf(msg,"%s%c ",GEN_QUANT(mi->quant)->name,(mm<0?'-':' '));
        else sprintf(msg,"%s%c ",mi->name,(mm<0?'-':' ')); 
        outstring(msg); 
      }
  }
  orig = get_original(e_id);
  if ( valid_id(orig) && !equal_element(orig,e_id) ) 
     { sprintf(msg," original %s",ELNAME(orig)); outstring(msg); }

  /* extra attributes */
  for ( i = 0, ex = EXTRAS(EDGE) ; i < web.skel[EDGE].extra_count;
              i++,ex++ )
    { char *at = get_e_extra(e_id,i);
      if ( !(ex->flags & DUMP_ATTR) ) continue;
      if ( ex->array_spec.datacount == 0 ) continue;
      sprintf(msg," %s ",ex->name);
      outstring(msg);
      print_array_attribute(ex,at);
    }

  outstring("\n"); 

  outfd = old_fd;
}  /* end edge_dump() */

/***************************************************************************
*
* function: facet_dump()
*
* purpose: write out one facet definition
*
*/

void facet_dump(f_id,fd)
facet_id f_id;
FILE *fd;  /* destination file */
{ int i;
  int per_line = 0; 
  edge_id e_id;
  facetedge_id fe;
  FILE *old_fd = outfd;
  struct extra *ex;
  int attr = get_fattr(f_id);
  facet_id orig;

  outfd = fd;

  if ( !valid_id(f_id) ) return;
  { sprintf(msg,"%3s  ",ELNAME(f_id)); outstring(msg); }
    if ( web.representation == SIMPLEX )
    { vertex_id *v = get_facet_vertices(f_id);
      int num;
      if (web.modeltype == LAGRANGE)
         num = binom_coeff(web.lagrange_order+web.dimension,web.dimension);
      else num = web.dimension+1;
      for ( i = 0 ; i < num ; i++ )
         { sprintf(msg," %3s",ELNAME(v[i])); outstring(msg); }
    }
    else
    { facetedge_id first_fe;
      fe = first_fe = get_facet_fe(f_id);
      if ( valid_id(fe) ) do
      { e_id = get_fe_edge(fe);
        sprintf(msg," %s",SELNAME(e_id) );
        outstring(msg); 
        per_line++;
        if ( per_line >= 10 )
        { sprintf(msg," \\\n              "); outstring(msg); 
          per_line = 0;
        }
        fe = get_next_edge(fe);
      } while ( valid_id(fe) && !equal_id(fe,first_fe) );
      if ( (web.modeltype == LAGRANGE) && (web.representation != STRING) )
      { int num = binom_coeff(web.lagrange_order+web.dimension,web.dimension);
        vertex_id *v = get_facet_vertices(f_id);
        outstring(" vertices ");
        for ( i = 0 ; i < num ; i++ )
        { sprintf(msg,"%3s ",ELNAME(v[i])); outstring(msg); }
      }
    }
    if ( attr & BOUNDARY )
      { struct boundary *bdry = get_facet_boundary(f_id);
        sprintf(msg,"  boundary %s ",bdry->name); 
        outstring(msg); 
      }                    
    if ( attr & CONSTRAINT )
      { conmap_t * conmap = get_f_constraint_map(f_id);
        struct constraint *con;
        sprintf(msg,"  constraints "); outstring(msg); 
        for ( i = 1 ; i <= (int)conmap[0] ; i++ )
        { con = get_constraint(conmap[i]);
          if ( !(con->attr & GLOBAL) )
          { if ( con->attr & NAMED_THING )
             { sprintf(msg,"%s ",con->name); outstring(msg); }
            else
             { sprintf(msg,"%d ",conmap[i]&CONMASK); outstring(msg); }
      }
        }
      }
    if ( attr & NEGBOUNDARY ) outstring(" orientation -1 ");
    if ( attr & DENSITY )
#ifdef LONGDOUBLE
    { sprintf(msg,"  density %1.*Lg ",DPREC,get_facet_density(f_id)); outstring(msg); }
#else
    { sprintf(msg,"  density %1.15g ",get_facet_density(f_id)); outstring(msg); }
#endif 
    if ( attr&FIXED )  outstring(" fixed " ); 
    if ( attr & NO_REFINE ) outstring(" no_refine ");
    if ( attr & NONCONTENT ) outstring(" noncontent ");
    if ( attr & NODISPLAY ) outstring(" nodisplay"); 
    if ( get_tag(f_id) ) 
        { sprintf(msg," tag %d ",get_tag(f_id)); outstring(msg); }
    if ( phase_flag && (web.representation == STRING) )
     { sprintf(msg," phase %d ",get_f_phase(f_id)); outstring(msg); }
    if ( get_facet_color(f_id) != DEFAULT_FACET_COLOR  )
     { sprintf(msg," color %s ",COLORNAME(get_facet_color(f_id)));
       outstring(msg); }
    if ( get_facet_backcolor(f_id) !=  get_facet_color(f_id) )
     { sprintf(msg," backcolor %s ",COLORNAME(get_facet_backcolor(f_id))); 
       outstring(msg); }
    if ( elptr(f_id)->method_count )
    { struct method_instance *mi;
      int meth_offset = EXTRAS(FACET)[web.meth_attr[FACET]].offset;
      int *methlist = (int*)((char*)elptr(f_id) + meth_offset);
      int methcount = (int)elptr(f_id)->method_count;
      for ( i = 0 ; i < methcount ; i++ )
        { int mm = methlist[i];
          mi = METH_INSTANCE(abs(mm));
          if ( mi->quant >= 0 &&
            (GEN_QUANT(mi->quant)->flags & (DEFAULT_QUANTITY|Q_DELETED)) )
             continue;
          if ( mi->quant >= 0 && (mi->flags & IMPLICIT_INSTANCE) )
             sprintf(msg," %s%c ",GEN_QUANT(mi->quant)->name,(mm<0?'-':' '));
          else sprintf(msg," %s%c ",mi->name,(mm<0?'-':' ')); 
          outstring(msg); 
        }
    }
    sprintf(msg," /*area %g*/", (DOUBLE)get_facet_area(f_id)); outstring(msg); 
    orig = get_original(f_id);
    if ( valid_id(orig) && !equal_element(orig,f_id) )
        { sprintf(msg," original %s",ELNAME(orig)); outstring(msg); }

  /* extra attributes */
  for ( i = 0, ex = EXTRAS(FACET) ; i < web.skel[FACET].extra_count;
              i++,ex++ )
    { char *at = get_f_extra(f_id,i);
      if ( !(ex->flags & DUMP_ATTR) ) continue;
      if ( ex->array_spec.datacount == 0 ) continue;
      sprintf(msg," %s ",ex->name);
      outstring(msg);
      print_array_attribute(ex,at);
    }

  outstring("\n"); 

  outfd = old_fd;
}  /* end facet_dump() */

/*************************************************************************
*
* function: body_dump()
*
* purpose: Write out definition of one body.
*
*/

void body_dump(b_id,fd)
body_id b_id;
FILE *fd;  /* destination file */
{ 
  REAL den;
  int per_line = 0;
  facet_id startf,f_id;
  int i;
  FILE *old_fd = outfd;
  struct extra *ex;
  REAL bvol;
  body_id orig;
  int maxcount = 2*web.skel[FACET].count;
  int count = 0;

  outfd = fd;

  if ( !valid_id(b_id) ) return;
  /* make_bfacet_lists(); */
  sprintf(msg,"%3s      ",ELNAME(b_id)); outstring(msg); 
  startf = f_id = get_body_facet(b_id);
  while ( valid_id(f_id) )
  { sprintf(msg," %s",SELNAME(f_id));
    outstring(msg); 
    per_line++;
    if ( per_line >= 10 )
    { sprintf(msg," \\\n          "); outstring(msg); 
      per_line = 0;
    }
    f_id = get_next_body_facet(f_id);
    if ( equal_id(f_id,startf) )
      break;
    if ( count++ > maxcount )
    { sprintf(errmsg,"Internal error: body %s facet list not closed.\n",
             ELNAME(b_id));
      kb_error(4221,errmsg,RECOVERABLE);
    }
  }
  bvol = get_body_volume(b_id);
  if ( get_battr(b_id) & FIXEDVOL )
    { sprintf(msg,
#ifdef LONGDOUBLE
        "  volume %1.*Lg  /*actual: %1.*Lg*/ lagrange_multiplier %1.*Lg ",
                     DPREC,get_body_fixvol(b_id), DPREC,
                     bvol,DPREC,get_body_pressure(b_id));
#else
        "  volume %1.15g  /*actual: %1.15g*/ lagrange_multiplier %1.15g ",
                    get_body_fixvol(b_id), 
                    bvol,get_body_pressure(b_id));
#endif 
      outstring(msg); 
    }
    if ( (get_body_volconst(b_id) != 0.0) )
#ifdef LONGDOUBLE
      { sprintf(msg, "  volconst %1.*Lg ",DPREC,get_body_volconst(b_id)); 
#else
      { sprintf(msg, "  volconst %1.15g ",get_body_volconst(b_id)); 
#endif 
        outstring(msg); }

    if ( web.torus_flag && ((bvol > web.torusv) || (bvol < 0.0)) )
#ifdef LONGDOUBLE
    { sprintf(msg, "  actual_volume %1.*Lg ",DPREC,bvol); 
#else
    { sprintf(msg, "  actual_volume %1.15g ",bvol); 
#endif 
      outstring(msg); }

    if ( get_battr(b_id) & PRESSURE ) 
#ifdef LONGDOUBLE
      { sprintf(msg,"  pressure %1.*Lg",DPREC, get_body_pressure(b_id)); outstring(msg); }
#else
      { sprintf(msg,"  pressure %1.15g", get_body_pressure(b_id)); outstring(msg); }
#endif 
    den = get_body_density(b_id);
    if ( den != 0.0 )
#ifdef LONGDOUBLE
         { sprintf(msg,"  density %1.*Lg ",DPREC,den); outstring(msg); }
#else
         { sprintf(msg,"  density %1.15g ",den); outstring(msg); }
#endif 
    if ( phase_flag && (web.representation == SOAPFILM) )
         { sprintf(msg," PHASE %d ",get_b_phase(b_id)); outstring(msg); }
    orig = get_original(b_id);
    if ( valid_id(orig) && !equal_element(orig,b_id) ) 
    { sprintf(msg," original %s",ELNAME(orig)); 
    outstring(msg); }
    if ( elptr(b_id)->method_count )
    { struct method_instance *mi;
      int meth_offset = EXTRAS(BODY)[web.meth_attr[BODY]].offset;
      int *methlist = (int*)((char*)elptr(b_id) + meth_offset);
      for ( i = 0 ; i < (int)elptr(b_id)->method_count ; i++ )
      { mi = METH_INSTANCE(abs(methlist[i]));
        if ( mi->flags & IMPLICIT_INSTANCE )
          sprintf(msg,"%s ",GEN_QUANT(mi->quant)->name);
        else sprintf(msg,"%s%c ",mi->name,methlist[i]>0?' ':'-'); 
        outstring(msg); 
      }
    }

  /* extra attributes */
  for ( i = 0, ex = EXTRAS(BODY) ; i < web.skel[BODY].extra_count;
              i++,ex++ )
    { char *at = get_b_extra(b_id,i);
      if ( !(ex->flags & DUMP_ATTR) ) continue;
      if ( ex->array_spec.datacount == 0 ) continue;
      sprintf(msg," %s ",ex->name);
      outstring(msg);
      print_array_attribute(ex,at);
    }

  outstring("\n"); 

  outfd = old_fd;
} /* end body_dump */

static int readflag; /* whether READ section has been started */
static FILE *dumpfd;

/**************************************************************************
*
* function: toggle_save()
*
* purpose: write ON toggle state to standard output (which has been 
*          redirected for dump purposes)
*
*/

void toggle_save(togglename)
char *togglename;
{ if ( readflag == 0 ) { outstring("\nread\n"); readflag = 1; }
  sprintf(msg,"%s on\n",togglename);
  outstring(msg);
}

/**************************************************************************
*
* function: toggle_save_off()
*
* purpose: write OFF toggle state to standard output (which has been 
*          redirected for dump purposes)
*
*/

void toggle_save_off(togglename) /* for default on toggles */
char *togglename;
{ if ( readflag == 0 ) { outstring("\nread\n"); readflag = 1; }
  sprintf(msg,"%s off\n",togglename);
  outstring(msg);
}

/***********************************************************************
*
* function: do_dump()
*
* purpose: dump whole datafile.  Open file, call section dumps.
*
*/
void do_dump(name)
char *name;
{
  vertex_id v_id;
  edge_id e_id;
  facet_id f_id;
  body_id b_id;
  char defaultname[100];
  int old_quiet = quiet_flag;

  quiet_flag = 0;  /* so outstring actually produces output */

  if ( name == NULL )
    { /* construct default name */
      strncpy(defaultname,datafilename,sizeof(defaultname)-5);
#ifdef MPI_EVOLVER
      sprintf(defaultname+strlen(defaultname),".task%d",this_task);
#endif
      strcat(defaultname,".dmp");
      name = defaultname;
    }

  sprintf(msg,"Dumping to %s.\n",name);
  outstring(msg);
  dumpfd = fopen(name,"w");

  if ( dumpfd == NULL )
    { sprintf(errmsg,"Cannot open file %s.\n",name);
      kb_error(1006,errmsg,RECOVERABLE);
    }


  fprintf(dumpfd,"// %s: Dump of structure.\n\n",name); 

  top_dump(dumpfd);

  /* vertex dump */
  fputs("\nvertices        /*  coordinates  */    \n",dumpfd);
  MFOR_ALL_VERTICES(v_id)
     vertex_dump(v_id,dumpfd);
     
  /* edge dump */
  fputs("\nedges  \n",dumpfd);
  MFOR_ALL_EDGES(e_id)
     edge_dump(e_id,dumpfd);

  /* facet dump */
  if ( web.representation == SIMPLEX )
     fputs("\nfaces    /* vertex set */      \n",dumpfd);
  else
     fputs("\nfaces    /* edge loop */      \n",dumpfd);
  MFOR_ALL_FACETS(f_id)
     facet_dump(f_id,dumpfd);

  /* body dump */
  #ifndef MPI_EVOLVER
  calc_content(Q_FIXED|Q_INFO);
  #endif
  fputs("\nbodies  /* facets */\n",dumpfd);
  MFOR_ALL_BODIES(b_id)
     body_dump(b_id,dumpfd);

  bottom_dump(dumpfd);

  fclose(dumpfd);
  quiet_flag = old_quiet;
} /* end do_dump() */

/********************************************************************
*
* function: list_proc()
*
* purpose: list one procedure, recursing if necessary for
*             dependencies.
*/
void list_proc(g,level)
struct global *g;     /* global variable of procedure */
int level; /* for recursion depth check */
{ struct expnode *ex = &g->value.proc;
  struct treenode *node;
  int pp;

  g->attr.procstuff.proc_timestamp = proc_timestamp;
  if ( level > web.global_count ) 
  { sprintf(errmsg,"'%s' involved in cyclic definition.\n",g->name);
     kb_error(1007,errmsg,WARNING);
  }
  else
  {
    /* check dependencies */
    for ( node = ex->start ; node != ex->root ; node ++  )
      if ( node->type == PROCEDURE_ )
      { pp = node->op1.name_id;
        if ( globals(pp)->attr.procstuff.proc_timestamp < proc_timestamp )
            list_proc(globals(pp),level+1);
      }
  }

  /* print */
  if ( g->flags & PERMANENT )
    sprintf(msg,"%s ::= ",g->name);
  else sprintf(msg,"%s := ",g->name);
  outstring(msg);
  if ( g->value.proc.root==NULL )
      outstring("{}");
  else
  { if ( g->value.proc.root->type != COMMAND_BLOCK_ )
      outstring("{"); 
    outstring(print_express(&g->value.proc,'X'));
    if ( g->value.proc.root->type != COMMAND_BLOCK_ )
      outstring("}");
  }
  outstring("\n");
  g->attr.procstuff.proc_timestamp = proc_timestamp;
} /* end list_proc() */

/********************************************************************
*
* function: list_function()
*
* purpose: list one procedure, recursing if necessary for
*             dependencies.
*/
void list_function(g)
struct global *g;
{ 
  if ( g->value.proc.root == NULL )
  { outstring("// ");
    outstring(g->name);
    outstring(" body definition has not been executed yet.\n\n");
  }
  else
  { outstring(print_express(&g->value.proc,'X'));
    outstring("\n\n");
  }
} /* end list_function() */

/********************************************************************
*
* function: list_procedure()
*
* purpose: list one procedure,
*/
void list_procedure(g)
struct global *g;
{ if ( g->value.proc.root == NULL )
  { outstring("// ");
    outstring(g->name);
    outstring(" body definition has not been executed yet.\n\n");
  }
  else
  { outstring(print_express(&g->value.proc,'X'));
    outstring("\n\n");
  }
} /* end list_procedure() */

/********************************************************************
*
* function: list_function_proto()
*
* purpose: list one function prototype
*/
void list_function_proto(g)
struct global *g;
{ 
  struct expnode ex = g->value.proc;  /* copy, since will modify */
  if ( !ex.root ) 
      return;
  outstring("function ");
  outstring(datatype_name[ex.root->op4.ret_type]);
  outstring(" ");
  outstring(g->name);
   if ( ex.root == NULL )
    outstring("();  // definition has not been executed yet.\n");
  else
  { outstring("(");
    ex.root += ex.root->left; /* so just does function head */
    outstring(print_express(&ex,'X'));
    outstring(");\n");
  }
} /* end list_function() */

/********************************************************************
*
* function: list_procedure_proto()
*
* purpose: list one procedure prototype.
*/
void list_procedure_proto(g)
struct global *g;
{ 
  struct expnode ex = g->value.proc;
  outstring("procedure ");
  outstring(g->name);
  if ( g->attr.procstuff.argcount == 0 ) 
  { if ( g->flags & PROCEDURE_NAME )
       outstring("();\n"); 
    else outstring("\n");
    return;
  }
  if ( ex.root == NULL )
    outstring("(); // definition has not been executed yet.\n");
  else
  { outstring("(");
    ex.root += ex.root->left; /* so just does procedure head */
    outstring(print_express(&ex,'X'));
    outstring(");\n");
  }
} /* end list_procedure_proto() */

/*********************************************************************
*
* function: list_top_procedures()
*
* purpose: list argument-style functions and procedures in top of datafile. 
*          Note this lists globals of type FUNCTION_NAME and PROCEDURE_NAME.
*          Only fully does those originally declared in top section.
*          For SUBROUTINE, see list_procedures().
*
*/
void list_top_procedures(mode)
int mode; /* LIST_PROTO or LIST_FULL */
{ int i; 
  int protoflag = 0; /* whether printed prototype comment */

  proc_timestamp++;

  /* prototypes */
  for ( i = 0 ; i < web.global_count ; i++ )
  {
    if ( globals(i)->flags & FUNCTION_NAME ) 
    { if ( protoflag == 0 )
      { outstring("\n// Function and procedure prototypes\n");
        protoflag = 1;
      }
      list_function_proto(globals(i));
    }
    if ( globals(i)->flags & PROCEDURE_NAME ) 
    { if ( protoflag == 0 )
      { outstring("\n// Function and procedure prototypes\n");
        protoflag = 1;
      }
      list_procedure_proto(globals(i));
    }
  }
  if ( protoflag )
    outstring("// End prototypes\n\n");

  if ( mode == LIST_PROTO )
    return;

  /* full function definitions */
  for ( i = 0 ; i < web.global_count ; i++ )
  { int flags = globals(i)->flags;
    if ( (flags & FUNCTION_NAME) && (flags & IN_DATAFILE_TOP) ) 
        list_function(globals(i));
    if ( (flags & PROCEDURE_NAME) && (flags & IN_DATAFILE_TOP) ) 
        list_procedure(globals(i));
  }

} /* end list_top_procedures() */

/*********************************************************************
*
* function: list_procedures()
*
* purpose: list defined procedures 
*          Note this lists globals of type SUBROUTINE.
*
*/
void list_procedures(mode)
int mode; /* LIST_PROTO or LIST_FULL */
{ int i; 
  int perm_comment_flag = 0;
  int witharg_comment_flag = 0;
  int without_comment_flag = 0;
  int forward_comment_flag = 0;
  int redefheader;

  proc_timestamp++;

  /* redefined single letters dummy defs (i.e. forward decls) */
  redefheader = 0;
  if ( mode != LIST_PROTO )
   for ( i = 0 ; i < 128 ; i++ )
     if ( single_redefine[i].start )
     { 
        if ( !redefheader )
        { outstring("// Redefined single letter commands forward declarations: \n");
          redefheader = 1;
        }
        sprintf(msg,"%c :::= {} \n",i);
        outstring(msg);
     }


  if ( mode != LIST_PROTO )
  { 
    /* function definition forward decls */
    for ( i = 0 ; i < web.global_count ; i++ )
    { if ( globals(i)->flags & SUBROUTINE ) 
      { 
        if ( !forward_comment_flag )
        { outstring("//Procedures and functions forward declarations:\n");
          forward_comment_flag = 1;
        }
        sprintf(msg,"%s := {}\n",globals(i)->name);
        outstring(msg);
      }
     
    }
  
    for ( i = 0 ; i < web.perm_global_count ; i++ )
    { if ( perm_globals(i)->flags & SUBROUTINE ) 
      { if ( !perm_comment_flag )
        { outstring("// Permanent procedures and functions:\n");
          perm_comment_flag = 1;
        }
        sprintf(msg,"%s ::= {}\n",perm_globals(i)->name);
        outstring(msg);
      }
    }
  }

  /* full function definitions */
   for ( i = 0 ; i < web.global_count ; i++ )
  { if ( globals(i)->flags & SUBROUTINE ) 
      if ( globals(i)->attr.procstuff.proc_timestamp < proc_timestamp )
        { 
          if ( !without_comment_flag )
          { outstring("// Procedures without arguments:\n");
            without_comment_flag = 1;
          }
          if ( mode == LIST_PROTO )
            list_procedure_proto(globals(i)); 
          else
             list_proc(globals(i),0);
        }
  }

  for ( i = 0 ; i < web.perm_global_count ; i++ )
  { if ( perm_globals(i)->flags & SUBROUTINE ) 
    { if ( !perm_comment_flag )
      { outstring("// Permanent procedures and functions:\n");
        perm_comment_flag = 1;
      }
      if ( mode == LIST_PROTO )
        list_procedure_proto(perm_globals(i));
      else 
        list_proc(perm_globals(i),0);
    }
  }

  /* full function definitions */
  for ( i = 0 ; i < web.global_count ; i++ )
  { int flags = globals(i)->flags; 
    if ( (flags & FUNCTION_NAME) && !(flags & IN_DATAFILE_TOP) ) 
    { 
      if ( !witharg_comment_flag )
      { outstring("// Procedures and functions with argument lists:\n");
        witharg_comment_flag = 1;
      }
      if ( mode == LIST_PROTO )
        list_function_proto(globals(i));
      else 
        list_function(globals(i));
    }
    if ( (flags & PROCEDURE_NAME) && !(flags & IN_DATAFILE_TOP) ) 
    { 
      if ( !witharg_comment_flag )
      { outstring("// Procedures and functions with argument lists:\n");
        witharg_comment_flag = 1;
      }
      if ( mode == LIST_PROTO )
        list_procedure_proto(globals(i));
      else 
        list_procedure(globals(i));
    }
  }

  /* redefined single letters */
  redefheader = 0;
  for ( i = 0 ; i < 128 ; i++ )
     if ( single_redefine[i].start )
     { 
        if ( !redefheader )
        { outstring("// Redefined single letter commands: \n");
          redefheader = 1;
        }
        if ( mode == LIST_PROTO )
        { sprintf(msg,"%c",i);
          outstring(msg);
        }
        else
        { sprintf(msg,"%c :::= ",i);
          outstring(msg);
          if ( single_redefine[i].root->type != COMMAND_BLOCK_ )
            outstring("{"); 
          outstring(print_express(&single_redefine[i],'X'));
          if ( single_redefine[i].root->type != COMMAND_BLOCK_ )
            outstring("}"); 
        }
        outstring("\n");
     }
} /* end list_procedures() */

/****************************************************************************
*
* function: bottom_dump()
*
* purpose: print info at bottom of fe dump file.
*          Called also by bottominfo command.
*/

void bottom_dump(fd)
FILE *fd;
{ int i;
  FILE *old_fd = outfd;
  int e_type;

  outfd = fd;
  readflag = 0;

  outstring("\nread\n"); readflag = 1;

  /* string variables */
  for ( i = 0 ; i < web.global_count ; i++ )
  { if ( globals(i)->flags & STRINGVAL )
    { sprintf(msg,"%s := ",globals(i)->name);
      convert_string(globals(i)->value.string,msg+strlen(msg));
      strcat(msg,"\n");
      outstring(msg);
    }
  }  
  /* extra attributes with function definitions */
  for ( e_type = VERTEX ; e_type <= FACETEDGE ; e_type++ )
  { struct extra *ex;
     for ( i = 0, ex = EXTRAS(e_type) ; 
                i < web.skel[e_type].extra_count ; i++, ex++ )
     { int k;
       if ( !(ex->flags & DUMP_ATTR) ) continue;
       if ( !ex->code.start )  continue;
        sprintf(msg,"\ndefine %s attribute %s %s",typenames[e_type], ex->name,
          datatype_name[ex->type]);
        for ( k = 0 ; k < ex->array_spec.dim ; k++ )
          sprintf(msg+strlen(msg),"[%d]",ex->array_spec.sizes[k]);
        outstring(msg);
        outstring(" function \n  { ");
        outstring(print_express(&ex->code,'X'));
        outstring(" }\n");
     }
  }
  list_procedures(LIST_FULL);

  if ( show_expr[EDGE] ) 
    { 
      outstring(print_express(show_command+EDGE,'X'));
      outstring("\n");
    }
  if ( show_expr[FACET] ) 
    { 
      outstring(print_express(show_command+FACET,'X'));
      outstring("\n");
    }

  /* misc state saving */
  outstring("\n");
  switch(torus_display_mode)
  { case TORUS_RAW_MODE: outstring("raw_cells\n"); break;
    case TORUS_CLIPPED_MODE: outstring("clipped\n"); break;
    case TORUS_CONNECTED_MODE: outstring("connected\n"); break;
  }
  if ( window_aspect_ratio != 0.0 )
  { sprintf(msg,"window_aspect_ratio := %18.15f\n",window_aspect_ratio); 
    outstring(msg);
  }
  if ( clip_view_flag )
    toggle_save("clip_view");
  
  if ( slice_view_flag ) toggle_save("slice_view");
  if ( visibility_test ) toggle_save("visibility_test");
  if ( full_bounding_box_flag ) toggle_save("full_bounding_box");
  if ( gridflag > 0 ) toggle_save("gridflag");
  if ( gridflag == 0 ) toggle_save_off("gridflag");
  if ( user_thickness_flag )
  { sprintf(msg,"thickness := %g\n",(double)thickness); outstring(msg); }
  if ( hessian_slant_cutoff != 0.0 ) 
  { sprintf(msg,"hessian_slant_cutoff := %g\n",(double)hessian_slant_cutoff); outstring(msg); }
  if ( everything_quantities_flag ) outstring("convert_to_quantities\n");
  if ( bezier_flag ) outstring("bezier_basis on\n");
  if ( conj_grad_flag ) toggle_save("conj_grad");
  if ( quantities_only_flag && !everything_quantities_flag) 
      toggle_save("quantities_only");
  if ( !ribiere_flag ) toggle_save_off("ribiere");
  if ( assume_oriented_flag ) toggle_save("assume_oriented");
  if ( view_4D_flag ) toggle_save("view_4D");
  if ( kusner_flag ) toggle_save("kusner");
  if ( estimate_flag ) toggle_save("estimate");
  if ( unit_normal_flag ) toggle_save("deturck");
  if ( sqgauss_flag ) toggle_save("sqgauss");
  if ( autopop_flag ) toggle_save("autopop");
  if ( autochop_flag ) toggle_save("autochop");
  if ( circular_arc_flag ) toggle_save("circular_arc_draw");
  if ( rgb_colors_flag ) toggle_save("rgb_colors");
  if ( kraynikpopedge_flag ) toggle_save("kraynikpopedge");
  if ( !kraynikpopvertex_flag ) toggle_save("kraynikpopvertexedge");
  
  if ( quiet_flag ) toggle_save("quiet");
  if ( augmented_hessian_flag > 0 ) toggle_save("augmented_hessian");
  if ( augmented_hessian_flag == 0 ) toggle_save_off("augmented_hessian");
  if ( hessian_special_normal_flag ) toggle_save("hessian_special_normal");
  if ( !hessian_quiet_flag ) toggle_save_off("hessian_quiet");
  if ( quiet_go_flag ) toggle_save("quietgo");
  if ( old_area_flag ) toggle_save("old_area");
  if ( approx_curve_flag ) toggle_save("approx_curv");
  if ( runge_kutta_flag ) toggle_save("runge_kutta");
  if ( check_increase_flag ) toggle_save("check_increase");
  if ( web.area_norm_flag ) toggle_save("area_normalization");
  if ( conf_edge_curv_flag ) toggle_save("conf_edge");
  if ( effective_area_flag ) toggle_save("effective_area");
  if ( web.torus_body_flag ) toggle_save("connected");
  if ( web.torus_clip_flag ) toggle_save("clipped");
  if ( thickenflag ) toggle_save("thicken");
  if ( !innerflag ) toggle_save_off("show_inner");
  if ( !outerflag ) toggle_save_off("show_outer");
  if ( colorflag )
  { toggle_save("colormap");
    sprintf(msg,"colorfile := \"%s\"\n",cmapname);
  }
  if ( dirichlet_flag ) toggle_save("dirichlet_mode");
  if ( sobolev_flag ) toggle_save("sobolev_mode");
  if ( pop_disjoin_flag ) toggle_save("pop_disjoin");
  if ( pop_enjoin_flag ) toggle_save("pop_enjoin");
  if ( pop_to_face_flag ) toggle_save("pop_to_face");
  if ( pop_to_edge_flag ) toggle_save("pop_to_edge");
  if ( ps_colorflag > 0 ) toggle_save("pscolorflag");
  if ( ps_colorflag == 0 ) toggle_save_off("pscolorflag");
  if ( labelflag == 0 ) toggle_save_off("labelflag");
  if ( labelflag > 0  ) toggle_save("labelflag");
  if ( crossingflag > 0 ) toggle_save("crossingflag");
  if ( crossingflag == 0 ) toggle_save_off("crossingflag");
  if ( smooth_graph_flag ) toggle_save("smooth_graph");
  if ( hessian_by_diff_flag ) toggle_save("hessian_diff");
  if ( !hessian_normal_flag ) toggle_save("hessian_normal");
  if ( hessian_normal_perp_flag ) toggle_save("hessian_normal_perp");
  if ( hessian_normal_one_flag ) toggle_save("hessian_normal_one");
  if ( hessian_double_normal_flag ) toggle_save("hessian_double_normal");
  if ( post_project_flag ) toggle_save("post_project");
  if ( mean_curv_int_flag ) toggle_save("mean_curvature_integral");
  if ( normal_curvature_flag ) toggle_save("normal_curvature");
  if ( div_normal_curvature_flag ) toggle_save("div_normal_curvature");
  if ( !shading_flag ) toggle_save_off("shading");
  if ( !color_flag ) toggle_save_off("facet_colors");
  if ( boundary_curvature_flag ) toggle_save("boundary_curvature");
  if ( normal_motion_flag ) toggle_save("normal_motion");
  if ( check_pinning_flag ) toggle_save("pinning");
  if ( !metric_convert_flag ) toggle_save_off("metric_conversion");
  if ( autorecalc_flag ) toggle_save("autorecalc");
  if ( make_pos_def_flag ) toggle_save("force_pos_def");
  if ( !gv_binary_flag ) toggle_save_off("gv_binary");
  if ( self_similar_flag ) toggle_save("self_similar");
  if ( interp_bdry_param ) toggle_save("interp_bdry_param");
  if ( web.bodycount && !web.gravflag ) toggle_save_off("gravity");
  if ( hessian_linear_metric_flag )
  { toggle_save("linear_metric");
#ifdef LONGDOUBLE
     sprintf(msg,"linear_metric_mix := %2.*Lg\n",DPREC,linear_metric_mix);
#else
     sprintf(msg,"linear_metric_mix := %2.15g\n",(DOUBLE)linear_metric_mix);
#endif 
     outstring(msg);
  }
  if ( transform_expr[0] )
     { 
        sprintf(msg,"transform_expr \"%s\"\n",transform_expr);
        outstring(msg);
     }
  if ( !transforms_flag && (transform_count>1)) toggle_save_off("transforms");
  outstring("\n\n");
  if ( web.target_tolerance != DEFAULT_TARGET_TOLERANCE )
  { sprintf(msg,"target_tolerance := %2.15g\n",(DOUBLE)web.target_tolerance);
    outstring(msg);
  }
  if ( brightness != DEFAULT_BRIGHTNESS )
  { sprintf(msg,"brightness := %2.15g\n",(DOUBLE)brightness);
    outstring(msg);
  }
  /* windup */
  outfd = old_fd;
} /* end bottom_dump */

/*********************************************************************
*
* function: facetedge_dump()
*
* purpose:  List facetedge with facet and edge links.  Meant for
*           screen display for debugging, not standard dump file.
*
*/
void facetedge_dump(fe,fd)
facetedge_id fe;
FILE *fd;
{ edge_id e_id;
  facet_id f_id;
  facetedge_id nf = get_next_facet(fe);
  facetedge_id pf = get_prev_facet(fe);
  facetedge_id ne = get_next_edge(fe);
  facetedge_id pe = get_prev_edge(fe);
  FILE *old_fd = outfd;
  int i; 
  struct extra *ex;
  outfd = fd;

  e_id = get_fe_edge(fe);
  f_id = get_fe_facet(fe);
  sprintf(msg,"%6s      %5s    %5s  %8s %8s     %8s %8s",
      ELNAME(fe), SELNAME1(e_id), SELNAME2(f_id),
      (valid_id(pe) ? SELNAME3(pe) : "0"), 
      (valid_id(ne) ? SELNAME4(ne) : "0"), 
      (valid_id(pf) ? SELNAME5(pf) : "0"), 
      (valid_id(nf) ? SELNAME6(nf) : "0")); 
  outstring(msg); 

  /* extra attributes */
  for ( i = 0, ex = EXTRAS(FACETEDGE) ; 
                i < web.skel[FACETEDGE].extra_count; i++,ex++ )
    { char *at = get_extra(fe,i);
      if ( !(ex->flags & DUMP_ATTR) ) continue;
      if ( ex->array_spec.datacount == 0 ) continue;
      sprintf(msg," %s ",ex->name);
      print_array_attribute(ex,at);
    }
  outstring("\n");
  outfd = old_fd;
} /* end facetedge_dump() */


/*********************************************************************
*
*  Function:  dump_force()
*
*  Purpose:    List forces at vertices for debugging purposes.
*/

void dump_force()
{
  char name[400];

  FILE *fd;
  vertex_id v_id;
  int i;
  FILE *old_fd = outfd;

  prompt("Enter name of dump file: ",name,sizeof(name));
  if ( name[0] == '\0' ) return;

  fd = fopen(name,"w");

  if ( fd == NULL )
  { sprintf(errmsg,"Cannot open file %s.\n",name);
    kb_error(1008,errmsg,RECOVERABLE);
  }

  outfd = fd;
  sprintf(msg,"%s: Dump of force.\n\n",name); outstring(msg); 

  /* vertex dump */
  outstring("\nvertices              coordinates      \n");
  FOR_ALL_VERTICES(v_id)
  { REAL *x,*f,mag;
    x = get_coord(v_id);
    switch(SDIM)
        { case 2:
#ifdef LONGDOUBLE
          sprintf(msg,"%3s    %17.*Lf %17.*Lf  |x| = %17.*Lf ",
             ELNAME(v_id),DPREC,x[0],DPREC,x[1],DPREC,sqrt(SDIM_dot(x,x))); 
#else
          sprintf(msg,"%3s    %17.15f %17.15f  |x| = %17.15f ",
             ELNAME(v_id),x[0],x[1],sqrt(SDIM_dot(x,x))); 
#endif 
             outstring(msg); 
            break;
          case 3:
#ifdef LONGDOUBLE
          sprintf(msg,"%3s  %17.*Lf %17.*Lf %17.*Lf  |x| = %17.*Lf ",
             ELNAME(v_id),DPREC,x[0],DPREC,x[1],DPREC,x[2],DPREC,sqrt(SDIM_dot(x,x))); 
#else
          sprintf(msg,"%3s  %17.15f %17.15f %17.15f  |x| = %17.15f ",
             ELNAME(v_id),x[0],x[1],x[2],sqrt(SDIM_dot(x,x))); 
#endif 
             outstring(msg); 
            break;
         }
    if ( get_vattr(v_id) & CONSTRAINT )
    { conmap_t *conmap = get_v_constraint_map(v_id);
      struct constraint *con;
      sprintf(msg,"  constraints "); outstring(msg); 
      for ( i = 1 ; i <= (int)conmap[0] ; i++ )
      { con = get_constraint(conmap[i]);
        if ( !(con->attr & GLOBAL) )
        { if ( con->attr & NAMED_THING )
           { sprintf(msg,"%s ",con->name); outstring(msg); }
          else
           { sprintf(msg,"%d ",conmap[i]&CONMASK); outstring(msg); }
    }
      }
    }
    if ( get_vattr(v_id)&FIXED ) { sprintf(msg, "  fixed "); outstring(msg); }
    sprintf(msg,"\n"); outstring(msg); 

    f = get_force(v_id);
    mag = sqrt(SDIM_dot(f,f));
    switch(SDIM)
    { case 2:
#ifdef LONGDOUBLE
          sprintf(msg,"f:    %17.*Lf %17.*Lf |f| = %g\n\n",
             DPREC,f[0],DPREC,f[1],(DOUBLE)mag); outstring(msg); 
#else
          sprintf(msg,"f:    %17.15f %17.15f |f| = %g\n\n",
             f[0],f[1],(DOUBLE)mag); outstring(msg); 
#endif 
          break;
      case 3:
#ifdef LONGDOUBLE
          sprintf(msg,"f:    %17.*Lf %17.*Lf %17.*Lf  |f| = %g\n\n",
             DPREC,f[0],DPREC,f[1],DPREC,f[2],(DOUBLE)mag); outstring(msg); 
#else
          sprintf(msg,"f:    %17.15f %17.15f %17.15f  |f| = %g\n\n",
             f[0],f[1],f[2],(DOUBLE)mag); outstring(msg); 
#endif 
      break;
    }
  }

  fclose(fd);

  outfd = old_fd;
} /* end dump_force */

/***********************************************************************************
*
* function: list_attributes()
*
* purpose: List all defined attributes of elements (the ones with entries
*     in attribute table, not flags like FIXED).
*
*/

void list_attributes()
{ int n,k,j;
  struct extra *ex;

  outstring(
     "Extra attributes (and dynamically allocated internal attributes)\n");
  for ( n = 0 ; n < NUMELEMENTS ; n++ )
  {  
      sprintf(msg,"%s attributes:\n",typenames[n]); outstring(msg);
      for ( k = 0 ; k < web.skel[n].extra_count ; k++ )
      { ex = EXTRAS(n)+k;       
        sprintf(msg,"%32s %s",ex->name,datatype_name[ex->type]);
        for ( j = 0 ; j < ex->array_spec.dim ; j++ )
          sprintf(msg+strlen(msg),"[%d]",ex->array_spec.sizes[j]);
        strcat(msg,"\n");
        outstring(msg);       
      }
  }
} /* end list_attributes() */

/**************************************************************************
*
* Function: list_quantity()
* 
* Purpose: List quantity in top of datafile, and for "list quantity" 
*          command.
*/
void list_quantity(k)
int k;  /* quantity number */
{ struct gen_quant *q;
  int i;
  struct method_instance *mi;

  q = GEN_QUANT(k);
 
  /* first, its methods */
  outstring("\n");
  for ( i = 0 ; i < q->method_count ; i++ )
  { mi = METH_INSTANCE(q->meth_inst[i]); 
    if ( mi->flags & (IMPLICIT_INSTANCE|DEFAULT_INSTANCE) ) continue;
    outstring("METHOD_INSTANCE "); 
    outstring(mi->name);
    outstring(" METHOD ");
    outstring(basic_gen_methods[mi->gen_method].name);
    dump_method_specs(q->meth_inst[i]);
  }
  switch ( q->flags & (Q_INFO|Q_FIXED|Q_ENERGY|Q_CONSERVED) )
  { case Q_INFO:
      sprintf(msg,"QUANTITY %s INFO_ONLY ",q->name); outstring(msg); 
      break;
    case Q_CONSERVED:
#ifdef LONGDOUBLE
        sprintf(msg,"QUANTITY %s CONSERVED  lagrange_multiplier %2.*Lg",
                  q->name,DPREC,q->pressure); 
#else
        sprintf(msg,"QUANTITY %s CONSERVED  lagrange_multiplier %2.15g",
                  q->name,q->pressure); 
#endif 
        outstring(msg); 
        break;
    case Q_FIXED:
#ifdef LONGDOUBLE
        sprintf(msg,"QUANTITY %s FIXED = %2.*Lg  lagrange_multiplier %2.*Lg",
                q->name,DPREC,q->target,DPREC,q->pressure); 
#else
        sprintf(msg,"QUANTITY %s FIXED = %2.15g  lagrange_multiplier %2.15g",
                  q->name,q->target,q->pressure); 
#endif 
        outstring(msg); 
        break;
    case Q_ENERGY:
        sprintf(msg,"QUANTITY %s ENERGY ",q->name); outstring(msg); 
        break;
   }
   if ( q->tolerance >= 0.0 ) 
#ifdef LONGDOUBLE
   { sprintf(msg," tolerance %2.*Lg ",DPREC,q->tolerance); outstring(msg); }
#else
   { sprintf(msg," tolerance %2.15g ",q->tolerance); outstring(msg); }
#endif 
   if ( q->modulus != 1.0 ) 
#ifdef LONGDOUBLE
   { sprintf(msg," modulus %2.*Lg ",DPREC,q->modulus); outstring(msg); }
#else
   { sprintf(msg," modulus %2.15g ",q->modulus); outstring(msg); }
#endif 
   if ( q->volconst != 0.0 ) 
#ifdef LONGDOUBLE
   { sprintf(msg," volconst %2.*Lg ",DPREC,q->volconst); outstring(msg); }
#else
   { sprintf(msg," volconst %2.15g ",q->volconst); outstring(msg); }
#endif 
   if ( q->flags & Q_COMPOUND )
   { outstring(" function ");
     outstring(print_express(&q->expr,'\0'));
   }
   else for ( i = 0 ; i < q->method_count ; i++ )
   { mi = METH_INSTANCE(q->meth_inst[i]); 
     outstring(" method ");
     if ( mi->flags & (IMPLICIT_INSTANCE|DEFAULT_INSTANCE) )
     { outstring(basic_gen_methods[mi->gen_method].name);
       dump_method_specs(q->meth_inst[i]);
     }
     else outstring(mi->name);
   }
   outstring("\n"); 
}

/**************************************************************************
*
* Function: list_method_instance()
* 
* Purpose: List method instance in top of datafile, and implement 
*          "list method_instance" command.
*/
void list_method_instance(k)
int k;
{ struct method_instance *mi = METH_INSTANCE(k);
 
  outstring("METHOD_INSTANCE "); 
  outstring(mi->name);
  outstring(" METHOD ");
  outstring(basic_gen_methods[mi->gen_method].name);
  dump_method_specs(k);
}

/**************************************************************************
*
* Function: list_constraint()
* 
* Purpose: List constraint in top of datafile, and implement 
*          "list constraint" command.
*/
void list_constraint(cnum)
int cnum;
{ struct constraint *con = get_constraint(cnum);
  int j;
  
  if ( con->attr & NAMED_THING )
     sprintf(msg,"\nCONSTRAINT %s  ",con->name);
  else sprintf(msg,"\nCONSTRAINT %d  ",cnum); 
  outstring(msg); 
  if ( con->attr & NONWALL     )  outstring("   NONWALL "); 
  if ( con->attr & B_CONVEX    )  outstring("   CONVEX "); 
  if ( con->attr & NONNEGATIVE )  outstring("  NONNEGATIVE"); 
  if ( con->attr & NONPOSITIVE )  outstring("  NONPOSITIVE"); 
  if ( con->attr & GLOBAL      )  outstring("  GLOBAL"); 
  if ( con->content_rank ) 
  { sprintf(msg," content_rank %d\n",con->content_rank);
    outstring(msg);
  }

  outstring("\nFUNCTION:  ");
  outstring(print_express(con->formula,'X'));
  outstring("\n"); 

  if ( (con->attr & CON_ENERGY)  )
  { outstring("ENERGY \n");
    for ( j = 0 ; j < con->compcount ; j++ )
    { sprintf(msg,"E%1d: ",j+1); outstring(msg); 
      outstring(print_express(con->envect[j],'X'));
      outstring("\n"); 
    }
  }
  if ( con->attr & CON_CONTENT )
  { outstring("CONTENT \n");
    for ( j = 0 ; j < con->compcount ; j++ )
    { sprintf(msg,"C%1d: ",j+1); outstring(msg); 
      outstring(print_express(con->convect[j],'X'));
      outstring("\n"); 
    }
  }
}

/**************************************************************************
*
* Function: list_boundary()
* 
* Purpose: List boundary in top of datafile, and implement 
*          "list boundary" command.
*/
void list_boundary(bnum)
int bnum;
{ struct boundary * bdry = web.boundaries + bnum;
  int j;

  if ( bdry->attr & NAMED_THING )
     sprintf(msg,"\nBOUNDARY %s  PARAMETERS %d",bdry->name,bdry->pcount); 
  else sprintf(msg,"\nBOUNDARY %d  PARAMETERS %d",bnum,bdry->pcount); 
  outstring(msg); 
  if ( bdry->attr & B_CONVEX ) outstring(" CONVEX");
  if ( bdry->attr & NONWALL  ) outstring(" NONWALL ");
  if ( bdry->attr & PARTNER_HITTING ) outstring(" PARTNER_HITTING ");
  if ( bdry->content_rank ) 
  { sprintf(msg," content_rank %d\n",bdry->content_rank);
    outstring(msg);
  }
  outstring("\n"); 
  for ( j = 0 ; j < SDIM ; j++ )
  { sprintf(msg,"X%1d: ",j+1); outstring(msg); 
    outstring(print_express(bdry->coordf[j],'P'));
    outstring("\n");
  }

  if ( (bdry->attr & CON_ENERGY)  )
  { outstring("ENERGY \n");
    for ( j = 0 ; j < bdry->compcount ; j++ )
    { sprintf(msg,"E%1d: ",j+1); outstring(msg); 
      outstring(print_express(bdry->envect[j],'X'));
      outstring("\n"); 
    }
  }
  if ( bdry->attr & CON_CONTENT )
  { outstring("CONTENT \n");
    for ( j = 0 ; j < bdry->compcount ; j++ )
    { sprintf(msg,"C%1d: ",j+1); outstring(msg); 
      outstring(print_express(bdry->convect[j],'X'));
      outstring("\n"); 
    }
  }

  outstring("\n");
} /* end list_boundaryconstraint() */

/*************************************************************************
*
* Function: list_forwards()
*
* Purpose: list forward declared names
*/
void list_forwards()
{ int i;
  int title_flag = 0;


  /* constraints */
  for ( i = 0 ; i < web.maxcon ; i++ )
  { struct constraint *con = get_constraint(i);
    if ( !(con->attr & IN_USE) ) 
      continue;
    if ( isdigit(con->name[0]) )
      continue;
    if ( !title_flag )  outstring("\n// Forward declarations\n");
    title_flag = 1;
    outstring("constraint ");
    outstring(con->name);
    outstring(";\n");
  }

  /* boundaries */
  for ( i = 0 ; i < web.bdrymax ; i++ )
  { struct boundary *bdry = web.boundaries + i;
    if ( !(bdry->attr & IN_USE) ) 
      continue;
    if ( isdigit(bdry->name[0]) )
      continue;
    if ( !title_flag )  outstring("\n// Forward declarations\n");
    title_flag = 1;
    outstring("boundary ");
    outstring(bdry->name);
    outstring(";\n");
  }

  /* quantities */
  for ( i = 0 ; i < gen_quant_count ; i++ )
  { 
    if ( GEN_QUANT(i)->flags & DEFAULT_QUANTITY )
       continue;
    if ( !title_flag )  outstring("\n// Forward declarations\n");
    title_flag = 1;
    outstring("quantity ");
    outstring(GEN_QUANT(i)->name);
    outstring(";\n");
  }

  /* method_instances */
  for ( i = LOW_INST ; i < meth_inst_count ; i++ )
  { 
    if ( METH_INSTANCE(i)->flags & (IMPLICIT_INSTANCE|DEFAULT_INSTANCE) )
      continue;
    if ( !title_flag )  outstring("\n// Forward declarations\n");
    title_flag = 1;
    outstring("method_instance ");
    outstring(METH_INSTANCE(i)->name);
    outstring(";\n");
  }

  if ( title_flag )
     outstring("// End forward declarations\n\n");

} /* end list_forwards() */
