/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*************************************************************
*
*     file:    command.c
*
*     Purpose:  Interactive user interface command interpreter.
*/

#include "include.h"

/*************************************************************
*
*  function: recalc()
*
*  purpose:  get everything recalculated and redisplayed
*            after user changes surface.
*/

void recalc()
{ 
  global_timestamp++;  /* so INFO quantities will recalc later */
  if ( need_fe_reorder_flag )
  { /* straighten out facet order around edges */
    if ( web.representation == SOAPFILM )
    { edge_id e_id;
       FOR_ALL_EDGES(e_id)
          fe_reorder(e_id);
    }
    need_fe_reorder_flag = 0;
  }
  reset_rot_order();
  if ( web.torus_flag ) calc_periods(ADJUST_VOLUMES);
  else if ( web.torus_period ) calc_periods(NO_ADJUST_VOLUMES);
  if ( transform_expr[0] ) 
  { calc_view_transform_gens();
    transform_gen_expr(transform_expr);
  }
  recalc_verts();
  update_display();

  if ( phase_flag )
  {
    if ( web.representation == STRING )
    { edge_id e_id;
      FOR_ALL_EDGES(e_id)
        set_e_phase_density(e_id);
    }
    else
    { facet_id f_id;
      FOR_ALL_FACETS(f_id)
        set_f_phase_density(f_id);
    }
  }

  #ifdef MPI_EVOLVER
  if ( this_task == 0 )
  #endif
  {
    calc_content(Q_FIXED);
    /* if ( web.torus_flag ) fix_volconst(); */
    calc_pressure();
    calc_energy();
    change_flag = 0;
  }
  reset_conj_grad();
  if ( normal_motion_flag ) begin_normal_motion();
} /* end recalc() */

/******************************************************************
*
* function: reset_conj_grad()
*
* purpose: re-initialize conjugate gradient.
*
*/

void reset_conj_grad()
{ int i;
  vertex_id v_id;
  int size = SDIM;

#ifdef MPI_EVOLVER
  if ( this_task == MASTER_TASK )
  { struct mpi_command message;
    message.cmd = mpi_RESET_CONJ_GRAD;
    message.mode = ribiere_flag;
    MPI_Bcast(&message,sizeof(struct mpi_command),MPI_BYTE,MASTER_TASK,
          MPI_COMM_WORLD);
  }
#endif
  /* reset conjugate gradient */
  if ( cg_hvector ) myfree((char *)cg_hvector);
  cg_hvector = NULL;
  cg_oldsum = 0.0;
  if ( conj_grad_flag && ( ribiere_flag ) )
  { int r_attr = find_attribute(VERTEX,RIBIERE_ATTR_NAME);
     if ( r_attr == -1 )
     { add_attribute(VERTEX,RIBIERE_ATTR_NAME,REAL_TYPE,1,&size,0,NULL);
       r_attr = find_attribute(VERTEX,RIBIERE_ATTR_NAME);
     }
     FOR_ALL_VERTICES(v_id)
     { REAL *g = (REAL*)get_extra(v_id,r_attr);
       for ( i = 0 ; i < SDIM ; i++ ) g[i] = 0.0;
     }
  }
} /* end reset_conj_grad() */  

/*************************************************************
*
*  function: old_menu()
*
*  purpose: Handle one command.  Useful for event loopers.
*/

int old_menu ARGS1((text),
char *text)
{ int retval = 0;
  if ( text[0] == '!' ) 
   retval = old_history(text); /* history list */
  else 
   retval = command(text,ADD_TO_HISTORY);

  if ( change_flag )
       recalc();

  return retval;
}

/*************************************************************
*
*  function: letter_commands()
*
*  purpose:  handle the single-letter commands.
*
*/

void report_times ARGS((void));

void letter_command ARGS1((c),
int c)
{
  char response[140]; 
  body_id b_id;
  REAL val;  /* for scanf */
  int i,n,k;
  int znum;
  int old;

  switch ( c ) 
  {
     /* Reporting */
     case 'C':  run_checks();
            outstring("Checks completed.\n");
            break;

     case 'c': /* report count of elements and status */
            memory_report();
            break;

     case 'E': dump_force(); break;

     case 'e' : extrapolate(); break;

     case 'i':  /* i for information */
            information();
            break;

     case 'v' : /* show volumes and quantities */ 
            show_volumes();
            break;

     case 'z' : /* curvature test */
            if ( web.representation == SIMPLEX )
              outstring("Not implemented for simplex representation.\n");
            curtest();
            break;

     case 'X' : /* extra attributes */
        { 
            outstring(
 "  Element                      Attribute      Type   Offset  Bytes  Dimensions\n");
            for ( i = 0 ; i < NUMELEMENTS; i++ )
            { for ( k = 0 ; k < web.skel[i].extra_count ; k++ )
              { int j;
                sprintf(msg,"%9s %32s %10s %5d  %5d  ",typenames[i],
                EXTRAS(i)[k].name,
                datatype_name[EXTRAS(i)[k].type],               
                EXTRAS(i)[k].offset,
                datatype_size[EXTRAS(i)[k].type]*EXTRAS(i)[k].array_spec.datacount);
                for ( j = 0 ; j < EXTRAS(i)[k].array_spec.dim ; j++ )
                  sprintf(msg+strlen(msg),"[%d]",EXTRAS(i)[k].array_spec.sizes[j]);
                if ( EXTRAS(i)[k].array_spec.dim == 0 ) strcat(msg," scalar ");
                outstring(msg);
                outstring("\n");
              }
            }
         }
         break;
              

     /* Controlling model characteristics */

     case 'A' : /* set adjustable parameters */
            if ( set_parameters() )
                  recalc();
            break;


     case 'a' : /* toggle area normalization of force */
            old = web.area_norm_flag;
            web.area_norm_flag = !web.area_norm_flag;
            web.norm_check_flag = 0; /* default OFF */
            if ( web.area_norm_flag ) 
            { outstring("Area normalization ON.");
              if ( old ) outstring(" (was on)\n");
	      else outstring(" (was off)\n");
              prompt("If you want to check normal change, enter ratio: ",response,sizeof(response));
              if ( logfd ) fprintf(logfd,"%s\n",response);
              if ( const_expr(response,&val) > 0 )
                if ( val > 0.0000001 )
                    { web.norm_check_flag = 1;
                      web.norm_check_max = val;
                    }
              calc_energy();  /* to make sure vertex areas set */
            }
            else
            { outstring("Area normalization OFF.");
              if ( old ) outstring(" (was on)\n");
	      else outstring(" (was off)\n");
            }
            break;

     case 'b' : /* set body volumes and pressures */
       if ( web.skel[BODY].count == 0 )
       { outstring("No bodies.\n");
         break;
       }
       { REAL pp;

         FOR_ALL_BODIES(b_id)
         { 
           if ( get_battr(b_id) & PRESSURE )
           {
             pp = get_body_pressure(b_id);
             sprintf(msg,"Body %s. Current pressure %f.  Enter new: ",
                         ELNAME(b_id),(DOUBLE)pp);
             prompt(msg,response,sizeof(response));
             if ( logfd ) fprintf(logfd,"%s\n",response);
             if ( const_expr(response,&pp) > 0 )
             { set_body_pressure(b_id,pp);
               reset_conj_grad();
               if ( everything_quantities_flag )
               { struct gen_quant *q = GEN_QUANT(bptr(b_id)->volquant);
                 if ( pp == 0.0 )
                 { q->modulus = 1.0;
                   q->flags &= ~(Q_FIXED|Q_ENERGY|Q_CONSERVED);
                   q->flags |= Q_INFO;
                 }
                 else 
                 { q->modulus = -pp;
                   q->flags &= ~(Q_FIXED|Q_INFO|Q_CONSERVED);
                   q->flags |= Q_ENERGY;
                 }
              }
            }
          } /* end PRESSURE */
          else /* edit volumes */
          { 
            pp = get_body_fixvol(b_id);
            sprintf(msg,"Body %s. Current target volume %g.  Enter new: ",
                        ELNAME(b_id),(DOUBLE)pp);
            prompt(msg,response,sizeof(response));
            if ( logfd ) fprintf(logfd,"%s\n",response);
            cmdptr = response;
            if ( const_expr(response,&pp) > 0 )
            { set_body_fixvol(b_id,pp);
              reset_conj_grad();
              if ( pp == 0.0 )
              { if ( get_attr(b_id) & FIXEDVOL)
                { unset_attr(b_id,FIXEDVOL);
                  if ( everything_quantities_flag )
                  { struct gen_quant *q = GEN_QUANT(bptr(b_id)->volquant);
                    q->target = 0.0;
                    q->flags &= ~(Q_FIXED|Q_ENERGY|Q_CONSERVED);
                    q->flags |= Q_INFO;
                  }
                }
              }
              else
              { 
                set_attr(b_id,FIXEDVOL);
                if ( everything_quantities_flag )
                { struct gen_quant *q = GEN_QUANT(bptr(b_id)->volquant);
                  q->target = pp;
                  q->flags &= ~(Q_INFO|Q_ENERGY|Q_CONSERVED);
                  q->flags |= Q_FIXED;
                }
              } 
            }
          } 
        }  /* end BODIES */
      } /* end block */
     recalc();
     break; 
 
         case 'f' : /* Set diffusion */
                sprintf(msg,"Diffusion constant is %f.  Enter new: ",
                 (DOUBLE)web.diffusion_const); 
                prompt(msg,response,sizeof(response));
                if ( logfd ) fprintf(logfd,"%s\n",response);
                if ( const_expr(response,&val) > 0 )
                  web.diffusion_const = val;
                if ( web.diffusion_const == 0.0 )
                web.diffusion_flag = 0;
                else web.diffusion_flag = 1;
                break;

         case 'G' : /* control gravity */
                if ( web.representation == SIMPLEX )
                  { outstring(
                      "Gravity not implemented for simplex representation.\n");
                    break;
                  }
                if ( web.gravflag )
                  sprintf(msg,"Gravity is now ON with gravitational constant %f.\n",
                  (DOUBLE)web.grav_const);
                else sprintf(msg,"Gravity is now OFF.\n");
                outstring(msg);
                prompt("Enter new constant (0 for OFF): ",response,sizeof(response));
                if ( logfd ) fprintf(logfd,"%s\n",response);
                if ( const_expr(response,&val) > 0 )
                { web.grav_const = val;
                  if ( web.grav_const != 0.0 ) web.gravflag = 1;
                  else web.gravflag = 0;
                }
                if (gravity_quantity_num >= 0 )
                  GEN_QUANT(gravity_quantity_num)->modulus =
                  web.gravflag ? web.grav_const : 0.0;
                recalc();
                break;

         case 'J' : /* toggle jiggling on every move */
                web.jiggle_flag = !web.jiggle_flag;
                if ( web.jiggle_flag ) 
                  { outstring("Now jiggling on every move.\n");
                    sprintf(msg,
                    "Enter temperature for jiggling (default %f): ",
                      (DOUBLE)web.temperature);
                    prompt(msg,response,sizeof(response));
                    if ( logfd ) fprintf(logfd,"%s\n",response);
                    if ( const_expr(response,&val) > 0 )
                    web.temperature = val;
                  }
                else outstring("Jiggling on every move disabled.\n");
                break;

         case 'k' : /* Magnitude of force opposing boundary short-circuiting */
                sprintf(msg,"Gap constant is %f.  Enter new: ",
                    (DOUBLE)web.spring_constant); 
                prompt(msg,response,sizeof(response));
                if ( logfd ) fprintf(logfd,"%s\n",response);
                if ( const_expr(response,&val) > 0 )
                  web.spring_constant = val;
                if ( everything_quantities_flag )
                GEN_QUANT(gap_quantity_num)->modulus = val;
                if ( web.spring_constant == 0.0 )
                  web.convex_flag = 0;
                else
                  web.convex_flag = 1;
                recalc();
                break;

         case 'M' : /* change model type */
                if ( web.representation == SIMPLEX )
                  { outstring(
                    "Higher-order models not implemented for simplex representation.\n");
                    break;
                  }
                change_model();
                recalc();
                break;

         case 'm' : /* Setting motion scale factor */
              web.motion_flag = !web.motion_flag;
              if ( web.motion_flag )
              {
                sprintf(msg,"Enter scale factor (%g): ",(DOUBLE)web.scale);
                prompt(msg,response,sizeof(response));
                if ( logfd ) fprintf(logfd,"%s\n",response);
                if ( const_expr(response,&val) > 0 )
                  web.scale = val;
              }
              else
              { energy_init = 0;
                sprintf(msg,"Scale optimizing. Enter scale limit (%g): ",(DOUBLE)web.maxscale);
                prompt(msg,response,sizeof(response));
                if ( logfd ) fprintf(logfd,"%s\n",response);
                if ( const_expr(response,&val) > 0 )
                  web.maxscale = val;
                if ( web.scale > web.maxscale )
                  web.scale = web.maxscale;
              }
              break;

         case 'p' : /* Ambient pressure for compressible volumes */ 
                if ( web.bodycount == 0 )
                  { outstring("Can't do pressure without bodies.\n");
                    break;
                  }
                sprintf(msg,"Pressure now %f\n",(DOUBLE)web.pressure);
                outstring(msg);
                prompt("Enter pressure (0 for rigid volumes): ",response,sizeof(response));
                if ( logfd ) fprintf(logfd,"%s\n",response);
                if ( const_expr(response,&val) > 0 )
                  web.pressure = val;
                if ( web.pressure > 0.00000001 )
                {
                  if ( !web.full_flag && !valid_id(web.outside_body) )
                    add_outside();
                  web.projection_flag = 0;
                  web.pressure_flag = 1;
                  if ( everything_quantities_flag )
                    FOR_ALL_BODIES(b_id) create_pressure_quant(b_id);
                }
                else
                {
                  web.projection_flag = 1;
                  web.pressure_flag = 0;
                }
                recalc();
                break;

          case 'Q': /* quantities */
                report_quantities();
                break;

          case 'T': /* profiling times */
                report_times();
                break;

          case 'W': /* homothety toggle */
                web.homothety = !web.homothety;
                sprintf(msg,"Homothety adjustment is %s.\n",
                  web.homothety ? "ON" : "OFF");
                outstring(msg);
                if ( web.homothety )
                { sprintf(msg,"Enter target size (%g): ",
                     (DOUBLE)homothety_target);
                  prompt(msg,response,sizeof(response));
                  const_expr(response,&homothety_target);
                  if ( logfd ) fprintf(logfd,"%f\n",(DOUBLE)homothety_target);
                }
                break;

         /* Surface modification */

         case 'g' :  /* one gradient descent iteration */
                 if ( breakflag ) break;
                 iterate();
             
                 break;

        case 'j' : /* jiggling */
            sprintf(msg,"Enter temperature for jiggling (default %f): ",
                 (DOUBLE)web.temperature);
            prompt(msg,response,sizeof(response));
            if ( logfd ) fprintf(logfd,"%s\n",response);
            if ( const_expr(response,&val) > 0 )
              web.temperature = val;
            jiggle();
            recalc();
            break;

         case 'K' : /* skinny triangle long edge subdivide */
                if ( web.representation == SIMPLEX )
                { outstring(
                  "Not implemented for simplex representation.\n");
                  break;
                }
                for (;;)
                {
                  prompt("Enter minimum acute angle desired(h for histogram): ",
                       response,sizeof(response));
                  if ( logfd ) fprintf(logfd,"%s\n",response);
                  if ( tolower(response[0]) == 'h' )
                     skinny_histogram();
                  else break;
                }
                if ( const_expr(response,&val) > 0 )
                { 
                  if ( web.counts_reported & facet_refine_count_bit )
                     web.facet_refine_count = 0;
                  sprintf(msg,"Edges refined: %d\n",
                     web.facet_refine_count += n = skinny(val));
                  web.counts_reported |= facet_refine_count_bit; 
                  outstring(msg);
                  recalc();
                }
                break;


         case 'l' : /* long edge subdivide */
              for (;;)
               {
                 prompt("Enter maximum edge length desired(h for histogram): ",response,sizeof(response));
                 if ( logfd ) fprintf(logfd,"%s\n",response);
                 if ( tolower(response[0]) == 'h' )
                    edge_histogram();
                 else break;
               }
              if ( const_expr(response,&val) > 0 )
               { web.max_len = val;
                 if ( web.counts_reported & edge_refine_count_bit )
                   web.equi_count = 0;
                 sprintf(msg,"Edges refined: %d\n",
                     web.edge_refine_count += n = articulate(web.max_len));
                 outstring(msg);
                 web.counts_reported |= edge_refine_count_bit;
                 if ( n > 0 ) recalc();
               }
              break;

         case 'N' : /* Normalize target volumes to current volumes */
                FOR_ALL_BODIES(b_id)
                  set_body_fixvol(b_id,get_body_volume(b_id));
                outstring("Target volumes adjusted.\n");
                break;

         case 'n' : /* notching ridges and valleys */
                if ( web.representation == SIMPLEX )
                 { outstring(
                     "Not implemented for simplex representation.\n");
                   break;
                 }
                for (;;)
                {
                  prompt("Enter maximum angle(radians) between normals(h for histogram): ",response,sizeof(response)); 
                  if ( logfd ) fprintf(logfd,"%s\n",response);
                  if ( tolower(response[0]) == 'h' )
                    { if ( web.representation == STRING )
                        command("histogram(vertex,dihedral)",NO_HISTORY);
                      else ridge_histogram();
                    }
                  else break;
                }
                if ( const_expr(response,&val) > 0 )
                  { if ( web.representation == STRING )
                    { sprintf(msg,
                        "refine edge ee where max(ee.vertex,dihedral) > %f",
                           (DOUBLE)val);
                      command(msg,NO_HISTORY); 
					  web.notch_count = web.where_count;
                    }
                    else
                    { web.max_angle = val;
                      if ( web.max_angle <= 0.0 ) break;
                      if ( web.counts_reported & notch_count_bit )
                        web.equi_count = 0;
                      sprintf(msg,"Number of edges notched: %d\n",
                      web.notch_count += n = ridge_notcher(web.max_angle));
                      outstring(msg);
                      web.counts_reported |= notch_count_bit;
                      if ( n > 0 ) recalc();
                    }
                  }
                break;

         case 'O' : /* pop nonminimal edges */
                if ( web.representation == SIMPLEX )
                { outstring(
                    "Edge popping not implemented for simplex representation.\n");
                  break;
                }
                if ( web.representation == STRING )
                { 
                  if ( web.counts_reported & vertex_pop_count_bit )
                    web.vertex_pop_count = 0;
                  web.vertex_pop_count += n = verpop_str();
                  sprintf(msg,"Vertices popped: %d\n",web.vertex_pop_count);
                  web.counts_reported |= vertex_pop_count_bit;
                }
                else
                { 
                  if ( web.counts_reported & edge_pop_count_bit )
                    web.edge_pop_count = 0;
                  web.edge_pop_count = n = edgepop_film();
                  sprintf(msg,"Edges popped: %d\n",web.edge_pop_count);
                  web.counts_reported |= edge_pop_count_bit;
                }
                if ( n > 0 ) recalc();
                break;

         case 'o' : /* pop nonminimal edges and vertices */
                if ( web.representation == SIMPLEX )
                { outstring(
                   "Vertex popping not implemented for simplex representation.\n");
                  break;
                }
                if ( web.representation == STRING )
                  web.vertex_pop_count = n = verpop_str();
                else
                  web.vertex_pop_count = n = popfilm();
                if ( web.counts_reported & vertex_pop_count_bit )
                  web.edge_pop_count = 0;
                sprintf(msg,"Vertices popped: %d\n",web.vertex_pop_count);
                outstring(msg);
                web.counts_reported |= vertex_pop_count_bit;
                if ( n > 0 ) recalc();
                break;

         case 'r' : refine(); energy_init = 0;
                memory_report();
                web.min_area /= 4;
                web.max_len /= 2;
                web.min_length /= 2;
                recalc();
                break;

         case 't' : /* tiny edge subdivide */
               for (;;)
               {
                 prompt("Enter minimum edge length desired(h for histogram): ",response,sizeof(response));
                 if ( logfd ) fprintf(logfd,"%s\n",response);
                 if ( tolower(response[0]) == 'h' )
                    edge_histogram();
                 else break;
               }
               web.min_length = 0.0;
               if ( const_expr(response,&val) > 0 )
                  web.min_length = val;
               else break;
               if ( web.min_length <= 0.0 ) break;
               if ( web.counts_reported & edge_delete_count_bit )
                   web.edge_delete_count = 0;
               sprintf(msg,"Deleted edges: %d\n",
                  web.edge_delete_count += n = edgeweed(web.min_length));
               outstring(msg);
               web.counts_reported |= edge_delete_count_bit;
               if ( n > 0 ) recalc();
               break;

         case 'U' : /* conjugate gradient */
               old = conj_grad_flag;
               if ( conj_grad_flag )
               { if ( cg_hvector ) myfree((char *)cg_hvector);
                 cg_hvector = NULL;
                 cg_oldsum = 0.0;
                 conj_grad_flag = 0;
                 outstring("Conjugate gradient now OFF.");
                 if ( old ) outstring(" (was on)\n");
	         else outstring(" (was off)\n");
               }
               else
               { conj_grad_flag = 1;
                 outstring("Conjugate gradient now ON.");
                 if ( old ) outstring(" (was on)\n");
	         else outstring(" (was off)\n");
                 reset_conj_grad();
                 if ( web.motion_flag )
                    kb_error(1639,"Fixed scale is ON!  Probably not a good idea with conjugate gradient.\n",WARNING);

               }
               break;

         case 'u' : /* equiangulate */
                if ( web.counts_reported & equi_count_bit )
                   web.equi_count = 0;
                sprintf(msg,"Edges switched in equiangulation: %d\n",
                  web.equi_count += n = equiangulate() );
                web.counts_reported |= equi_count_bit;
                outstring(msg);
                if ( n > 0 )
                { recalc();
                  if ( web.torus_flag )
                    fix_volconst();
                }
                break;

         case 'V' : /* move vertex to average of neighbors */
                vertex_average(VOLKEEP);
                outstring("Vertex averaging done.\n");
                recalc();
                break;

         case 'w' : /* weed small triangles */
                if ( web.representation == SIMPLEX )
                { outstring(
            "Triangle weeding not implemented for simplex representation.\n");
                  break;
                }
                for (;;)
                {
                  prompt("Enter minimum area desired(h for histogram): ",
                     response,sizeof(response));
                  if ( logfd ) fprintf(logfd,"%s\n",response);
                  if ( tolower(response[0]) == 'h' )
                    area_histogram();
                  else break;
                }
                web.min_area = 0.0;
                if ( const_expr(response,&val) > 0 )
                  web.min_area = val;
                else break; 
                if ( web.min_area <= 0.0 ) break;
                if ( web.counts_reported & facet_delete_count_bit )
                   web.facet_delete_count = 0;
                sprintf(msg,"Skinny triangles weeded: %d\n",
                   web.facet_delete_count += n = areaweed(web.min_area));
                outstring(msg);
                web.counts_reported |= facet_delete_count_bit;
                if ( n > 0 ) recalc();
                break;

         case 'y' : /* torus duplication */
                if ( web.representation == SIMPLEX )
                { outstring(
                     "Torus duplication not implemented for simplex representation.\n");
                  break;
                }
                if ( ! web.torus_flag )
                { outstring("Torus model not in effect.\n");
                  break;
                }
                if ( SDIM == 2 )
                  prompt("Duplicate which period(1,2)? ",response,sizeof(response));
                else
                  prompt("Duplicate which period(1,2,3)? ",response,sizeof(response));
                if ( logfd ) fprintf(logfd,"%s\n",response);
                i = atoi(response);
                if ( i < 1 || i > SDIM )
                  outstring("Improper period number.\n");
                else tordup(i-1);
                recalc();
                break;            
                
         case 'Z' : /* zooming in on vertex */
            znum = loc_ordinal(web.zoom_v) + 1;
            sprintf(msg,"Enter zoom vertex number (%d): ",znum);
            prompt(msg,response,sizeof(response));
            if ( logfd ) fprintf(logfd,"%s\n",response);
            sscanf(response,"%d",&znum);
            /* check vertex still exists */
            { vertex_id v_id;
              int found = 0;
              FOR_ALL_VERTICES(v_id)
              { if ( znum == (loc_ordinal(v_id)+1) )
                { web.zoom_v = v_id; found = 1; break; }
              }
              if ( !found )
              { kb_error(1640,"Zoom vertex not found.\n",WARNING);
                break;
              }
            }
            sprintf(msg,"Enter cut-off radius (%f): ",(DOUBLE)web.zoom_radius);
            prompt(msg,response,sizeof(response));
            if ( logfd ) fprintf(logfd,"%s\n",response);
            if ( const_expr(response,&val) > 0 )
              web.zoom_radius = val;
            zoom_vertex(web.zoom_v,web.zoom_radius);
            recalc();
            break;

         /* Graphical output */

         case 'D' : old = go_display_flag;
                go_display_flag = !go_display_flag; 
                if ( go_display_flag )
                { outstring("Automatic display ON.");
                  update_display();
                }
                else
                  outstring("Automatic display OFF.");
                if ( old ) outstring(" (was on)\n");
	        else outstring(" (was off)\n");
                break;

         case 'd' : 
                dump(); 
                break;

         case 'P' : /* create graphics data file */
                display_file(-1);
                break;

         case 's' : /* Show image */ 
                if ( !go_display_flag && !dont_resize_flag )
                  resize();
                go_display_flag = 1;
                do_show();
                break;


         /* Process control */

         case 'F' : /* log commands */
               if ( logfd ) 
               { fclose(logfd);
                 logfd = NULL;
                 outstring("Command logging OFF.\n");
                 break;
               }
               prompt("Name of log file: ",response,sizeof(response));
               if ( response[0] == '\0' ) break;
               logfd = fopen(response,"w");
               if ( logfd == NULL )
                  { perror(response); break; }
               break;      

         case 'S' : /* save binary form */
                outstring("Binary save not currently functional.\nUse 'd' Ascii dump.\n"); 
                break;

         case 'R' :  /* restore(NULL);  */
                outstring("R (restore) command discontinued.\n");
                break;

         case 'x' : 
         case 'q' : /* Exit */

			 if ( logfd ) 
               { fclose(logfd);
                 logfd = NULL;
                 outstring("Command logging OFF.\n");
               }
               if ( subshell_depth )
               { exit_flag = 1; 
                 break;
               } 
#ifdef __cplusplus
               loadfilename[0] = 0;
               loadstub();
#else
               startup(NULL);
#ifdef MPI_EVOLVER
             /*  MPI_Barrier(MPI_COMM_WORLD); */ /* extra kludge needed */
#endif
               longjmp(jumpbuf[0],1); /* kludge, but current command
                                      list just got deallocated */
#endif
               break;

         case '?' :
         case 'H' :
         case 'h' : /* help screen */
                main_help();
                break;
         case ' ' : break;
         case '\t' : break;
         case '\b' : break;
         case '\r' : break;
         case '\n' : break;
         default  : if ( c != '\0' )
          { sprintf(msg,"Illegal command: %c. Type h for help.\n", c);
            outstring(msg);
          }
          break;
   } /* end switch */
} /* end letter_command() */

/********************************************************************
*
*  Function: extrapolate()
*
*  Purpose:  prints extrapolation of results to infinite resolution 
*
*/

void extrapolate()
{
  int m;
  REAL d1,d2,ext;

  for ( m = 0 ; m <= reflevel ; m++ )
  { 
#ifdef LONGDOUBLE
    sprintf(msg,"refinement: %1d  energy: %*.*Lg  ",m,DWIDTH,DPREC,extrap_val[m]);
#else
    sprintf(msg,"refinement: %1d  energy: %19.15f  ",m,extrap_val[m]);
#endif
    outstring(msg);
    if ( m > 1 ) /* can extrapolate */
      { d1 = extrap_val[m-1] - extrap_val[m-2];
         d2 = extrap_val[m] - extrap_val[m-1];
         ext = extrap_val[m] - d2*d2/(d2 - d1);
#ifdef LONGDOUBLE
         sprintf(msg,"extrapolation: %*.*Lg\n",DWIDTH,DPREC,ext);
#else
         sprintf(msg,"extrapolation: %19.15f\n",ext);
#endif
         outstring(msg);
      }
    else outstring("\n");
  }
}

/*****************************************************************
*
*  function: recalc_verts()
*
*  purpose:  recalculate vertex coordinates after boundaries or
*        constraints changed.
*
*/

void recalc_verts()
{
  vertex_id v_id;

  if ( threadflag )
    thread_launch(TH_PROJECT_ALL_ACTUAL,VERTEX);
  else
  FOR_ALL_VERTICES(v_id)
  {
    if ( get_vattr(v_id) & BOUNDARY )
     { struct boundary *boundary = get_boundary(v_id);
        REAL *param = get_param(v_id);
        REAL *x = get_coord(v_id);
        int j;
        
        for ( j = 0 ; j < SDIM ; j++ )
         x[j] = eval(boundary->coordf[j],param,v_id,NULL);
     }
    if ( get_vattr(v_id) & CONSTRAINT )
         project_v_constr(v_id,ACTUAL_MOVE,RESET_ONESIDEDNESS);
  }
}

/*****************************************************************
*
*  function: information()
*
*  purpose:  prints information for 'i' command
*
*/

void information()
{
    REAL total_volume;
    body_id b_id;

    sprintf(msg,"Datafile: %s\n",datafilename);
    outstring(msg);
    if ( web.area_norm_flag )
    { sprintf(msg,"Total time: %f\n",(DOUBLE)total_time);
      outstring(msg);
    }
#ifdef LONGDOUBLE
    sprintf(msg,"Total energy: %*.*Lg\n",DWIDTH,DPREC,web.total_energy);
#else
    sprintf(msg,"Total energy: %17.15g\n",web.total_energy);
#endif
    outstring(msg);
    if ( web.spring_energy != 0.0 )
      {
#ifdef LONGDOUBLE
         sprintf(msg,"Gap energy: %*.*Lg\n",DWIDTH,DPREC,web.spring_energy);
#else
         sprintf(msg,"Gap energy: %17.15g\n",web.spring_energy);
#endif
         outstring(msg);
      }
#ifdef LONGDOUBLE
    sprintf(msg,"Total %s:   %*.*Lg\n",areaname,DWIDTH,DPREC,web.total_area);
#else
    sprintf(msg,"Total %s:   %17.15g\n",areaname,web.total_area);
#endif
    outstring(msg);
    if ( web.conformal_flag )
      { sprintf(msg,"Euclidean measure: %17.15f\n",
         (DOUBLE)euclidean_area);
         outstring(msg);
      }
    if ( web.modeltype == LAGRANGE )
    { if ( bezier_flag )
        sprintf(msg,"Lagrange order %d (Bezier basis polynomials)\n",
           web.lagrange_order);
      else
        sprintf(msg,"Lagrange order %d\n",web.lagrange_order);
      outstring(msg);
    }
    sprintf(msg,"Integral order 1D: %d    2D: %d\n",web.gauss1D_order,
        web.gauss2D_order);
    outstring(msg);
    memory_report();
    if ( web.area_norm_flag ) 
      { outstring("Area normalization ON.");
         if ( web.norm_check_flag )
        { sprintf(msg,"  Max normal change %4.2f\n",(DOUBLE)web.norm_check_max);
          outstring(msg);
        }
         else outstring("  Normal change checking OFF.\n");
      }

    if ( web.modeltype ==  LINEAR )
      outstring("Representation: LINEAR\n");
    else if ( web.modeltype ==  QUADRATIC )
      outstring("Representation: QUADRATIC\n");
    else if ( web.modeltype ==  LAGRANGE )
      outstring("Representation: LAGRANGE\n");

    if ( web.homothety )
      { sprintf(msg,"Homothety ON, target size %g\n",
         (DOUBLE)homothety_target);
         outstring(msg);
      }

    if ( post_project_flag )
      outstring("Post-projection ON.\n");

    if ( conj_grad_flag )
      outstring("Conjugate gradient ON.\n");

    if ( web.motion_flag )
      sprintf(msg,"Motion scale factor fixed at %g\n",
        (DOUBLE)web.scale);
    else
      sprintf(msg,"Motion scale factor optimizing at %g; upper bound %f\n",
        (DOUBLE)web.scale,(DOUBLE)web.maxscale);
    outstring(msg);

    if ( self_similar_flag )
      { sprintf(msg,"Self similar motion is ON; coefficient %f\n,",  
        (DOUBLE)globals(lookup_global(SELFSIM_NAME))->value.real);
         outstring(msg);
      }

    if ( web.diffusion_flag )
      { sprintf(msg,"Diffusion is ON; diffusion constant: %f\n,",  
         (DOUBLE)web.diffusion_const);
         outstring(msg);
      }

    if ( web.gravflag )
      { sprintf(msg,"Gravity is ON; gravitational constant %f.\n",
          (DOUBLE)web.grav_const);
         outstring(msg);
      }

    if ( autochop_flag )
      { sprintf(msg,"Autochopping is ON; cutoff length %g.\n",
          (DOUBLE)autochop_length);
         outstring(msg);
      }
    if ( autopop_flag )
        outstring("Autopopping is ON.\n");
    if ( normal_curvature_flag )
        outstring("Normal_curvature is ON.\n");
    if ( effective_area_flag )
        outstring("Effective_area is ON.\n");
    if ( mean_curv_int_flag )
        { sprintf(msg,"Integral mean curvature is ON, modulus %g.\n",
             (DOUBLE)globals(square_curvature_param)->value.real);
          outstring(msg);
        }
    if ( square_curvature_flag )
        { sprintf(msg,"Square curvature is ON, modulus %g.\n",
             (DOUBLE)globals(square_curvature_param)->value.real);
          outstring(msg);
          if ( kusner_flag ) outstring("     Kusner edge curvature mode.\n");
          if ( conf_edge_curv_flag )
          outstring("     Conformal edge curvature mode.\n");
        }
    if ( boundary_curvature_flag )
      outstring("Boundary curvature ON.\n");
    if ( normal_motion_flag )
      outstring("Normal motion ON.\n");

    if ( web.jiggle_flag )
      { sprintf(msg,"Jiggling is ON; temperature is %f.\n",
          (DOUBLE)web.temperature);
         outstring(msg);
      }

    if ( web.convex_flag )
      {
         sprintf(msg,"Convexity gap constant is %f.\n",
          (DOUBLE)web.spring_constant); 
         outstring(msg);
      }

    if ( web.pressure_flag )
      { sprintf(msg,"Ambient pressure: %f\n",(DOUBLE)web.pressure);
         outstring(msg);
      }

    if ( klein_metric_flag )
      outstring("Klein metric.\n");

    total_volume = 0.0;
    FOR_ALL_BODIES(b_id)
      { if ( equal_id(b_id,web.outside_body) ) continue;
         total_volume += get_body_volume(b_id);
      }

    if ( web.meritfactor && (total_volume != 0.0) )
      { sprintf(msg,"Area^3/volume^2 figure of merit: %17.15f\n\n",
          (DOUBLE)(web.meritfactor*pow(web.total_energy,3.0)/
          total_volume/total_volume));
         outstring(msg);
      }

} /* end information() */

/*****************************************************************
*
*  function: show_volumes()
*
*  purpose:  prints information for 'v' command
*
*/

void show_volumes()
{ body_id b_id;
  int k;
  struct gen_quant *quan;
  int headerflag = 0;
  int recalc_flag = 0;

  if ( (web.bodycount == 0) && !gen_quant_count)
  { outstring("No bodies or quantities.\n");
    return;
  }

  if ( (fixed_volume_timestamp < global_timestamp)
      || ( info_volume_timestamp < global_timestamp) )
    recalc_flag = 1;

   for ( k=0 ; k < gen_quant_count; k++ )
  { quan = GEN_QUANT(k);
    if ( (quan->flags & DEFAULT_QUANTITY) && !show_all_quantities )
         continue;
    if (quan->flags & Q_DELETED ) continue;
	if ( !(quan->flags  & (Q_FIXED|Q_INFO|Q_ENERGY)) ) continue;
    if ( (quan->timestamp < global_timestamp) 
                 || (quan->timestamp < global_timestamp) )
    { 
#ifdef MPI_EVOLVER
      if ( this_task == MASTER_TASK )
#endif
	  { outstring("recalculating...\n");
        recalc_flag = 1;
	    break;
	  }
	  
    }
  }
  if ( recalc_flag )
       calc_content(Q_FIXED|Q_INFO|Q_ENERGY);
  if ( web.bodycount )
  {
    outstring("Body             target volume        actual volume        pressure\n");
    FOR_ALL_BODIES(b_id)
    { if ( equal_id(b_id,web.outside_body) ) continue;
#ifdef LONGDOUBLE
      if ( get_battr(b_id) & FIXEDVOL )
        sprintf(msg,"%3s  %*.*Lg      %*.*Lg    %*.*Lg\n",
             ELNAME(b_id),DWIDTH,DPREC,get_body_fixvol(b_id),
            DWIDTH,DPREC, get_body_volume(b_id),DWIDTH,DPREC,get_body_pressure(b_id));
      else if ( get_battr(b_id) & PRESSURE )
        sprintf(msg,"%3s         ------------     %*.*Lg    %*.*Lg\n",
             ELNAME(b_id),
             DWIDTH,DPREC,get_body_volume(b_id),DWIDTH,DPREC,get_body_pressure(b_id));
      else /* not a constraint */
          sprintf(msg,"%3s         ------------     %*.*Lg \n",
             ELNAME(b_id), DWIDTH,DPREC,get_body_volume(b_id));
#else
      if ( get_battr(b_id) & FIXEDVOL )
        sprintf(msg,"%3s         %17.15g      %17.15g    %17.15g\n",
             ELNAME(b_id),get_body_fixvol(b_id),
             get_body_volume(b_id),get_body_pressure(b_id));
      else if ( get_battr(b_id) & PRESSURE )
        sprintf(msg,"%3s            ------------     %17.15g    %17.15g\n",
             ELNAME(b_id),
             get_body_volume(b_id),get_body_pressure(b_id));
      else /* not a constraint */
        sprintf(msg,"%3s            ------------     %17.15g \n",
             ELNAME(b_id), get_body_volume(b_id));
#endif
      outstring(msg);
    }
  }
  if (  gen_quant_count )
  {
     for ( k=0 ; k < gen_quant_count; k++ )
     { quan = GEN_QUANT(k);
       if ( (quan->flags & DEFAULT_QUANTITY) && !show_all_quantities )
         continue;
       if ( quan->flags & Q_DELETED ) continue;
       if ( !headerflag )
         outstring(
  "          Quantity      target value     actual value          pressure\n");
       headerflag = 1;
#ifdef LONGDOUBLE
       if ( quan->flags & Q_CONSERVED )
         sprintf(msg,"%20s    (conserved)      ----------------- %*.*Lg\n",
              quan->name, DWIDTH,DPREC,quan->pressure);
       else if ( quan->flags & Q_FIXED )
         sprintf(msg,"%20s  %*.*Lg  %*.*Lg  %*.*Lg\n",
              quan->name,DWIDTH,DPREC,quan->target,DWIDTH,DPREC,
              quan->value,DWIDTH,DPREC,quan->pressure);
       else
         sprintf(msg,"%20s         ---------  %*.*Lg\n",
               quan->name,DWIDTH,DPREC,quan->value);
#else
       if ( quan->flags & Q_CONSERVED )
         sprintf(msg,"%20s     (conserved)     ----------------   %17.15g\n",
              quan->name,quan->pressure);
       else if ( quan->flags & Q_FIXED )
         sprintf(msg,"%20s  %17.15g  %17.15g  %17.15g\n",
              quan->name,quan->target,quan->value,quan->pressure);
       else
         sprintf(msg,"%20s         ---------  %17.15g\n",
                quan->name,quan->value);
#endif
       outstring(msg);
     }
   }
}  /* end show_volumes() */

/*****************************************************************
*
*  function: set_parameters()
*
*  purpose:  lets user change parameters for 'A' command
*
*  return:  1 if any changed, 0 if not.
*/

int set_parameters()
{ int n;
  REAL val;
  char response[100];
  int pchange_flag = 0;
  int showcount = 0;
 
  outstring("Variables: \n");
  for ( n = 0 ; n < web.global_count ; n++ )
  { if ( globals(n)->flags & SUBROUTINE ) continue;
    if ( !globals(n)->flags ) continue;
    if ( globals(n)->flags & ORDINARY_PARAM )
    { 
#ifdef LONGDOUBLE
      sprintf(msg,"%2d. %31.31s  %-#*.*Lg\n",
        n+1,globals(n)->name,DWIDTH,DPREC,globals(n)->value.real);
#else
      sprintf(msg,"%2d. %31.31s  %-#21.15g\n",
        n+1,globals(n)->name,globals(n)->value.real);
#endif
      if ( globals(n)->flags & OPTIMIZING_PARAMETER )
         strcpy(msg+strlen(msg)-1,"  optimizing_parameter\n");
    }
    else if ( globals(n)->flags & STRINGVAL )
     sprintf(msg,"%2d. %31.31s  %1.40s\n",
        n+1,globals(n)->name,globals(n)->value.string);
    else continue;
    outstring(msg);
    showcount++;
  }
  if ( web.perm_global_count )
  { int titleflag = 0;
    for ( n = 0 ; n < web.perm_global_count ; n++ )
    { if ( perm_globals(n)->flags & SUBROUTINE ) continue;
      if ( !perm_globals(n)->flags ) continue;
      if ( perm_globals(n)->flags & ORDINARY_PARAM )
      { 
#ifdef LONGDOUBLE
        sprintf(msg,"%2d. %31.31s  %-#*.*Lg\n", web.global_count +
          n+1,perm_globals(n)->name,DWIDTH,DPREC,perm_globals(n)->value.real);
#else
        sprintf(msg,"%2d. %31.31s  %-#21.15g\n", web.global_count +
          n+1,perm_globals(n)->name,perm_globals(n)->value.real);
#endif
        if ( globals(n)->flags & OPTIMIZING_PARAMETER )
           strcpy(msg+strlen(msg)-1,"  optimizing_parameter\n");
      }
      else if ( perm_globals(n)->flags & STRINGVAL )
       sprintf(msg,"%2d. %31.31s  %1.40s\n", web.global_count +
          n+1,perm_globals(n)->name,perm_globals(n)->value.string);
      else continue;
      if ( !titleflag )
      { outstring("Permanent variables: \n");
        titleflag = 1;
      }
      outstring(msg);
      showcount++;
    }
  }
  if ( showcount == 0 )
    outstring("   Nothing to report.\n");
  else
  /* ask for new values */
  for(;;)
  { int retval;
    char *c;
    prompt("Number and new value: ",response,sizeof(response));
    retval = sscanf(response,"%d",&n);
    if ( retval <= 0 ) break;    /* no more responses */
    strtok(response," \t");
    c = strtok(NULL,"");
    if ( const_expr(c,&val) > 0 )
    { if ( (n > 0) && (n <= web.global_count) )
       {
         if ( globals(n-1)->flags & ORDINARY_PARAM )
            globals(n-1)->value.real = val;
         else
            outstring("Bad parameter number. \n");
         if ( logfd ) 
#ifdef LONGDOUBLE
            fprintf(logfd,"%d %*.*Lg\n",n,DWIDTH,DPREC,val);
#else
            fprintf(logfd,"%d %17.15g\n",n,val);
#endif
         pchange_flag = 1;
       }
       else if ( (n >= web.global_count ) && 
              (n <= web.global_count+web.perm_global_count) )
       { n -= web.global_count;
         if ( globals(n-1)->flags & ORDINARY_PARAM )
            globals(n-1)->value.real = val;
         else
            outstring("Bad parameter number. \n");
       }
       else
         outstring("Bad parameter number.");
    }
    else
      outstring("Need number AND value.\n");
  }
  if ( logfd ) fprintf(logfd,"\n");
  return pchange_flag;
} /* end set_parameters() */
 
/*************************************************************************
*
* function: report_quantities()
*
* purpose: Implements 'Q' command; lists quantities, their instances, etc.
*          Hidden quantities shown if show_all_quantities flag set.
*
*/

void report_quantities()
{
  struct gen_quant *q;
  int k,j;

  outstring("Quantities and instances:\n");
  if ( everything_quantities_flag )
  { if ( show_all_quantities )
    outstring(
"(showing internal quantities also; to suppress, do \"show_all_quantities off\")\n");
    else outstring(
   "(not showing internal quantities; to show, do \"show_all_quantities\")\n");
  }
  for ( k=0 ; k < gen_quant_count ; k++ )
  { q = GEN_QUANT(k);
     if ( (q->flags & DEFAULT_QUANTITY) && !show_all_quantities ) continue;
     if ( q->flags & Q_DELETED ) continue;
     if ( !(q->flags & (Q_INFO|Q_ENERGY|Q_FIXED)) ) continue;
     if ( (q->timestamp < graph_timestamp) || (q->timestamp<web_timestamp) )
         calc_content(Q_INFO|Q_ENERGY|Q_FIXED);

     /* name and value */
#ifdef LONGDOUBLE
      sprintf(msg,"%2d. %-31.31s  %#*.*Lg",k+1,q->name,DWIDTH,DPREC,q->value);
#else
      sprintf(msg,"%2d. %-31.31s  %#21.15g",k+1,q->name,q->value);
#endif
      if ( q->flags & Q_FIXED ) strcat(msg,"  fixed quantity\n");
      else if ( q->flags & Q_ENERGY ) strcat(msg,"  energy quantity\n");
      else if ( q->flags & Q_CONSERVED ) strcat(msg,"  conserved quantity\n");
      else  strcat(msg,"  info_only quantity\n");
      outstring(msg);

      if ( q->flags & Q_FIXED )
      {
#ifdef LONGDOUBLE
        sprintf(msg,"    %31.31s  %#*.*Lg\n","target",DWIDTH,DPREC,
          q->target);
#else
        sprintf(msg,"    %31.31s  %#21.15g\n","target",q->target);
#endif
        outstring(msg);
      }

     /* modulus */
#ifdef LONGDOUBLE
      sprintf(msg,"    %31.31s  %#*.*Lg\n","modulus",DWIDTH,
         DPREC, q->modulus); 
#else
      sprintf(msg,"    %31.31s  %#21.15g\n","modulus",q->modulus); 
#endif
      outstring(msg);

     /* volconst */
     if ( q->volconst != 0.0 )
     {
#ifdef LONGDOUBLE
        sprintf(msg,"    %31.31s  %#*.*Lg\n","volconst",DWIDTH,
         DPREC, q->modulus); 
#else
        sprintf(msg,"    %31.31s  %#21.15g\n","volconst",q->volconst); 
#endif
        outstring(msg);
      }

     /* instances */
     for ( j = 0 ; j < q->method_count ; j++ )
     { struct method_instance *mi = METH_INSTANCE(q->meth_inst[j]);
       if ( !verbose_flag && (mi->flags & (IMPLICIT_INSTANCE|Q_DELETED)) )
          continue;  /* don't clutter up things */

       /* name and value */
#ifdef LONGDOUBLE
      sprintf(msg,"    %-31.31s  %#*.*Lg",mi->name,DWIDTH,DPREC,mi->value);
 #else
      sprintf(msg,"    %-31.31s  %#21.15g",mi->name,mi->value);
#endif
      strcat(msg,"  method instance\n");
      outstring(msg);

      /* modulus */
#ifdef LONGDOUBLE
      sprintf(msg,"    %31.31s  %#*.*Lg\n","modulus",
             DWIDTH, DPREC, mi->modulus);
#else
      sprintf(msg,"    %31.31s  %#21.15g\n","modulus",mi->modulus);
#endif
      outstring(msg);
#ifdef PROFILING_ENABLED
      /* timings */
      if ( verbose_flag )
      { sprintf(msg,"    %31.31s  %#21.15f  %10d calls\n",
        "value elapsed time, sec",mi->value_elapsed_time,mi->value_call_count);
        outstring(msg);
        sprintf(msg,"    %31.31s  %#21.15f  %10d calls\n",
        "gradient elapsed time, sec",mi->grad_elapsed_time,mi->grad_call_count);
        outstring(msg);
        sprintf(msg,"    %31.31s  %#21.15f  %10d calls\n",
        "hessian elapsed time, sec",mi->hess_elapsed_time,mi->hess_call_count);
        outstring(msg);
      }
#endif
    }
  }

}
 
/**************************************************************************
*
* function: report_times()
*
* purpose: report profiling times on systems with that enabled.
*/

void report_times()
{
#ifdef PROFILING_ENABLED
    sprintf(msg,"Method instance element setup time:   %15.10f\n",
		     PROF_ELAPSED(element_setup));
    outstring(msg);

    sprintf(msg,"Overall calc_quants time:             %15.10f\n",
		     PROF_ELAPSED(calc_quants));
    outstring(msg);

    sprintf(msg,"Overall calc_quant_grads time:        %15.10f\n",
		     PROF_ELAPSED(calc_quant_grads));
    outstring(msg);

    sprintf(msg,"Overall calc_quant_hess time:         %15.10f\n",
		     PROF_ELAPSED(calc_quant_hess));
    outstring(msg);

    sprintf(msg,"Hessian solution time:                %15.10f\n",
		     PROF_ELAPSED(hessian_AIJ_setup)
		   + PROF_ELAPSED(hessian_constraint_setup)
		   + PROF_ELAPSED(hessian_project_setup)
		   + PROF_ELAPSED(hessian_factor)
		   + PROF_ELAPSED(hessian_CHinvC)
		   + PROF_ELAPSED(hessian_solve)
		   + PROF_ELAPSED(hessian_mul)
                     );
    outstring(msg);

    if ( verbose_flag )
    {
      sprintf(msg,"    Hessian AIJ setup time:               %15.10f\n",
		     PROF_ELAPSED(hessian_AIJ_setup));
      outstring(msg);
      sprintf(msg,"    Hessian constraint setup time:        %15.10f\n",
		     PROF_ELAPSED(hessian_constraint_setup));
      outstring(msg);
      sprintf(msg,"    Hessian project setup time:           %15.10f\n",
		     PROF_ELAPSED(hessian_project_setup));
      outstring(msg);
      sprintf(msg,"    Hessian factor time:                  %15.10f\n",
		     PROF_ELAPSED(hessian_factor));
      outstring(msg);
      sprintf(msg,"    Hessian CHinvC time:                  %15.10f\n",
		     PROF_ELAPSED(hessian_CHinvC));
      outstring(msg);
      sprintf(msg,"    Hessian solve time:                   %15.10f\n",
		     PROF_ELAPSED(hessian_solve));
      outstring(msg);
      sprintf(msg,"    Hessian multiplication time:          %15.10f\n",
		     PROF_ELAPSED(hessian_mul));
      outstring(msg);
    }


    sprintf(msg,"exparse time:                         %15.10f\n",
		     PROF_ELAPSED(exparse));
    outstring(msg);

    sprintf(msg,"yyparse time:                         %15.10f\n",
		     PROF_ELAPSED(yyparse));
    outstring(msg);

    sprintf(msg,"yylex+kblex total time:               %15.10f\n",
		     PROF_ELAPSED(yylex));
    outstring(msg);

    sprintf(msg,"kblex time:                           %15.10f\n",
		     PROF_ELAPSED(kblex));
    outstring(msg);

    sprintf(msg,"Using processor speed of %g Hz\n",cpu_speed);
    outstring(msg);
   
#else
  outstring("Profiling not enabled on this system.\n");
#endif
}

