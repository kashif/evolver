/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*************************************************************
*
*     file:        lexinit.c
*
*     Purpose:    Reads in ASCII initial data files.
*
*      lexical analyzer version, using lex defs in datafile.lex
*/

#include "include.h"
#include "lex.h"
#include "ytab.h"
#define yylex() kb_yylex(NULL)

/***********************************************************
*
*  Function: reset_web()
*
*  Purpose: Zero out and initialize web storage structures
*
*/

void reset_web()
{
  int i,j;
  int cflag = web.torus_clip_flag;
  int bflag = web.torus_body_flag;

  
  unload_libraries(); /* unload dynamic libraries */
  clear_symtable();
  clear_globals();  /* need this to reset global name hash table */
  dymemsize = 0;
  dymem = NULL; 
  token_count = 0;
  display_text_count = 0;
  memset(text_chunks,0,sizeof(text_chunks));
 
  i = 1;
  if ( ((char*)&i)[0] ) little_endian_flag = 1;
  else big_endian_flag = 1;

  /* cut down overgrown eval stacks */
  for ( i = 0 ; i < nprocs ; i++ )
  { struct thread_data *td = thread_data_ptrs[i];
    int stack_used = td->stack_top - td->eval_stack;
    if ( td->eval_stack_size > stack_used + 1000 )
    { td->eval_stack_size = stack_used + 1000;
      td->eval_stack = 
          (REAL*)realloc(td->eval_stack,td->eval_stack_size*sizeof(REAL));
      td->stack_top = td->eval_stack + stack_used;
      td->eval_stack[td->eval_stack_size-1] = STACKMAGIC;
    }
    td->stack_top = td->eval_stack;
  }   



  /* reset nonzero flags and stuff */
  raw_velocity_attr = -1;
  lmc_mc_attr = -1;
  lmc_mobility_attr = -1;
  quarter_turn_var = -1;
  edge_diffusion_attr = -1;
  facet_diffusion_attr = -1;
  window_aspect_ratio = 0.0;  /* signifying not used */
  transform_gen_count = 0;
  view_transform_gens = NULL;
  view_transform_gens_expr = NULL;
  view_transforms = NULL;
  if ( !bflag && !cflag )
    torus_display_mode = TORUS_DEFAULT_MODE; 
  slice_view_flag = 0;
  clip_view_flag = 0;
  quantity_function_sparse_flag = 1;
  one_sided_present = 0;
  one_sided_lagrange_attr = -1;

  memset(slice_coeff,0,sizeof(slice_coeff));
  memset(clip_coeff,0,sizeof(clip_coeff));
  clip_coeff[0][0] = 1.0;

  list_free_all(PERM_BLOCK);  /* get rid of everything */
  reset_skeleton();  /* cleans out web and resets to 0 */
#ifdef MPI_EVOLVER
  mpi_reset();
#endif

#ifdef MPI_EVOLVER
 random_seed = this_task;  
#else
 random_seed = 1;  
#endif
  srand(random_seed); srand48(random_seed);
 
  file_no = 0;
  file_no_max = 12;
  file_names = (char**)mycalloc(file_no_max,sizeof(char**));
  file_names[0] = "stdin";
  file_no_used = 1;

  addload_flag = 0;
  verbose_flag = 0;
  estimate_flag = 0;
  warning_messages = NULL;
  warning_messages_max = 0;
  warning_messages_new = 0;
  memset((char*)pbase,0,sizeof(pbase));
  memset((char*)pmax,0,sizeof(pmax));
  vhead = NULL;
  vproj_base = NULL; 
  vproj_space = NULL;
  conhess_base = NULL;
  pressures = NULL;
  conrhs = NULL;
  optparam_congrads = NULL;
  rleftside = NULL;
  ritzvecs = NULL;
  tgverts = NULL;
  thread_stages = NULL;
  mpi_show_corona_flag = 0;
  edgeshow_flag = 1;
  
  /* Following PostScript line width variables relative to page size */
  ps_labelsize = 3.0;
  ps_stringwidth = 0.004;  /* default edge width */
  ps_fixededgewidth = 0.004;
  ps_tripleedgewidth = 0.003;
  ps_conedgewidth = 0.004;
  ps_bareedgewidth = 0.005;
  ps_gridedgewidth = 0.002;

  for ( i = 0 ; i < MAXLAGRANGE ; i++ )
  { bezier1invert[i] = NULL;
    bezier1revert[i] = NULL;
    bezier_refine_1d[i] = NULL;
    bezier2invert[i] = NULL;
    bezier2revert[i] = NULL;
    bezier_refine_2d[i] = NULL;
  }
  string_curve_tolerance = 2.0; /* degrees */

  dont_resize_flag = 0;
  local_nest_depth = 0;
  extra_bdry_attr = 0;
  extra_bdry_param_attr = 0;
  memset((char*)show_command,0,sizeof(show_command));
  macro_subs = NULL;
  macros = NULL;
  vpicklist = NULL;
  fpicklist = NULL;
  cg_hvector = NULL;
  f_sums = NULL;
  bi_sums = NULL;
  listmax = 0; list = NULL; permlist = NULL;
  if ( option_q == 2 ) option_q = 0;
  area_method_name[0] = 0;
  length_method_name[0] = 0;
  volume_method_name[0] = 0;
  uminus_flag = 0;
  inputsave_flag = 0;
  check_count = 0;
  hessian_slant_cutoff = 0.0;
  hessian_epsilon = hessian_epsilon_default;
  #ifdef MPI_EVOLVER
  sparse_constraints_flag = 0;
  augmented_hessian_flag = 0;
  #else
  sparse_constraints_flag = 1;
  augmented_hessian_flag = 1;
  #endif
  blas_flag = 0;
  last_error = 0;
  check_increase_flag = 0;
  brightness = DEFAULT_BRIGHTNESS;
  volgrads_every_flag = 0;
  zener_drag_flag = 0 ;
  pickvnum = pickenum = pickfnum = 0;
  keep_macros_flag = 0;
  needed_version[0] = 0;
  new_vertex_id = new_edge_id = new_facet_id = new_body_id = 0;
  sqcurve_ignore_constr = 0;
  ackerman_flag = 0;
  ps_colorflag = -1;
  boundary_expr_flag = 0;
  gridflag = -1;
  crossingflag = -1;
  labelflag = -1;
  web.target_tolerance = DEFAULT_TARGET_TOLERANCE;
  web.highcon = -1;
  web.highbdry = -1;
  autorecalc_flag = 1;
  circular_arc_flag = 0;
  spherical_arc_flag = 0;
  rgb_colors_flag = 0;
  kraynikpopedge_flag = 0;
  kraynikpopvertex_flag = 1;
  interp_bdry_param = 0;
  web.headvnum = 1;
  thickness = .001;
  user_thickness_flag = 0;
  last_eigenvalue = 0.0;
  last_hessian_scale = 0.0;
  optparamcount = 0;
  thickenflag = 0;
  rotorder_var = -1;
  quantities_only_flag = everything_quantities_flag = 0;
  linear_metric_mix = .50;
  min_square_grad_flag = 0;
  hessian_linear_metric_flag = 0;
  hess_move_con_flag = 1;
  eigen_neg = eigen_pos = eigen_zero = 0;
  innerflag = outerflag = 1;
  normal_sq_mean_curvature_mi = -1;
  eff_area_sq_mean_curvature_mi = -1;
  sq_mean_curvature_mi = -1;
  mix_sq_mean_curvature_mi = -1;
  star_normal_sq_mean_curvature_mi = -1;
  star_eff_area_sq_mean_curvature_mi = -1;
  star_sq_mean_curvature_mi = -1;
  gravity_quantity_num = -1;
  sq_mean_curv_quantity_num = -1;
  mean_curv_int_quantity_num = -1;
  view_4D_flag = 0;
  hessian_normal_flag = 1;
  hessian_special_normal_flag = 0;
  memset((char*)hessian_special_normal_expr,0,MAXCOORD*sizeof(struct expnode));
  hessian_normal_perp_flag = 0;
  hessian_double_normal_flag = 0;
  hessian_normal_one_flag = 0;
  hessian_quiet_flag = 1;
  hessian_by_diff_flag = 0;
  hess_debug = 0;
  mindeg_debug_level = 0;
  dirichlet_flag = 0;
  sobolev_flag = 0;
  quiet_go_flag = 0;
  quiet_flag = 0;
  pressure_set_flag = 1;
  self_similar_flag = 0;
  make_pos_def_flag = 0;
  datafile_view_flag = 0;
  post_project_flag = 0;
  read_command_flag = 0;
  ribiere_flag = 1;
  assume_oriented_flag = 0;
  conj_grad_flag = 0;
  mobility_flag = mobility_tensor_flag = 0;
  old_area_flag = 0;
  area_fixed_flag = 0;
  sqgauss_flag = 0;
  square_curvature_flag = 0;
  mean_curv_int_flag = 0;
  boundary_curvature_flag = 0;
  normal_curvature_flag = 0;
  approx_curve_flag = 0;
  kusner_flag = 0;
  klein_metric_flag = 0;
  conf_edge_curv_flag = 0;
  effective_area_flag = 0;
  autochop_flag = 0; 
  autopop_flag = 0;
  immediate_autopop_flag = 0;
  autopop_quartic_flag = 0;
  pop_disjoin_flag = 0;
  pop_enjoin_flag = 0;
  pop_to_edge_flag = 0;
  pop_to_face_flag = 0;
  runge_kutta_flag = 0;
  web.dimension = 2;
  web.representation = SOAPFILM;
  web.skel[VERTEX].ctrlpts = 1;
  web.skel[EDGE].ctrlpts = 2;
  web.skel[FACET].ctrlpts = 3;
  star_fraction = web.dimension + 1.0;
  areaname = "area";
  web.sdim = DEFAULT_SDIM;
  total_time = 0.0;
  homothety_target = 1.0;
  sym_flags = 0;
  symmetry_name = NULL;
  sym_wrap = identity_wrap;
  sym_form_pullback = identity_form_pullback;
  sym_inverse = identity_inverse;
  sym_compose = identity_compose;
  web.modeltype = LINEAR;
  web.lagrange_order = 1;
  bezier_flag = 0;
  web.hide_flag = 1;
  web.torus_clip_flag = cflag;
  web.torus_body_flag = bflag;
  web.torus_flag = 0;
  web.full_flag = 0;
  web.meritfactor = 0.0;
  web.grav_const = 1.0;
  web.symmetric_content = 0;
  web.pressure_flag = 0;
  web.projection_flag = 0;
  web.area_norm_flag = 0;
  web.vol_flag = 0;
  web.jiggle_flag = 0;
  web.temperature = 0.05;
  web.total_area = 0.0;
  web.total_facets = 0;
  web.scale = 0.1;
  web.scale_scale = 1.0;
  web.maxscale = 1.0;
  web.pressure = 0.0;
  web.bodycount = 0;
  web.wulff_count = web.wulff_flag = 0;
  web.min_area = 0.1;
  web.min_length = 0.25;
  web.max_len = 1.4;
  web.max_angle = 0.1;
  web.spring_constant = 1.0;
  web.gauss1D_order = 3;  /* can do 3  degree exactly (need default 7 in quadratic)*/
  set_by_user_gauss_1D = 0;
  web.gauss2D_order = 6; /* can do 6th degree poly */ 
  set_by_user_gauss_2D = 0;
  memset(gauss_lagrange,0,sizeof(gauss_lagrange));
  memset(maxgaussorder,0,sizeof(maxgaussorder));
  web.tolerance = DEFAULT_TOLERANCE;
  reflevel = 0;
  no_refine = 0;

  globals(view_matrix_global)->attr.arrayptr->sizes[0] = 0;
  globals(torus_periods_global)->attr.arrayptr->sizes[0] = 0;
  globals(inverse_periods_global)->attr.arrayptr->sizes[0] = 0;
  memset(torus_period_expr,0,sizeof(torus_period_expr));
  memset(torus_display_period_expr,0,sizeof(torus_display_period_expr));
  zoom_number = 1;
  web.zoom_v = NULLVERTEX;
  web.zoom_radius = 99999.0;
  transform_count = 0;
  view_transforms = NULL;
  set_view_transforms_global();
  transforms_flag = 1;
  transform_expr[0] = '\0';
  view_transform_det = NULL; 
  transform_colors = NULL; /* special kludge so allocate... doesn't free */
  allocate_transform_colors(0); 
  transform_gen_swap = NULL;
  transform_colors_flag = 0;
  vertex_normals = NULL;
  conical_x = NULL;
  conical_w = NULL;
  for ( j = 0 ; j < NUMELEMENTS ; j++ ) el_list[j] = NULL;
  v_procnum = NULL;
  phase_flag = 0;
  metric_convert_flag = 0;
  end_geomview_object();
  end_normal_motion();
  userfunc_init();
  for ( j = 0 ; j < NUMELEMENTS ; j++ )
  {
    show_expr[j] = NULL;
  }
  memset((char*)show_expr_table,0,sizeof(show_expr_table));
  memset((char*)single_redefine,0,128*sizeof(struct expnode));
  gauss1poly = NULL; 
  gauss1polyd = NULL; 
  gpoly = NULL; 
  gpolypartial = NULL; 

  expand_global_hash();  /* to get permanent variables on board */

  /* just in case no surface read in, can take commands */
  { int sdim = SDIM;
    expand_attribute(VERTEX,V_COORD_ATTR,&sdim);
    expand_attribute(VERTEX,V_OLDCOORD_ATTR,&sdim);
    expand_attribute(VERTEX,V_FORCE_ATTR,&sdim);
    expand_attribute(VERTEX,V_VELOCITY_ATTR,&sdim);
    expand_attribute(EDGE,E_VERTICES_ATTR,&web.skel[EDGE].ctrlpts);
    expand_attribute(FACET,F_VERTICES_ATTR,&web.skel[FACET].ctrlpts);
  }

  warnings_suppressed_count = 0;
  reset_counts();
}

/******************************************************************
*
*  Function: initialize()
*
*  Purpose:  read in data file and create initial triangulation.
*
*/


#define MAXLIST 100
#define LINESIZE 1000


/* temporary lists of elements */
  vertex_id *vlist = NULL;
  int vmaxlist;
  edge_id *elist = NULL;
  int emaxlist;
  facet_id *flist = NULL;
  int fmaxlist;
  int facecount; 
  body_id *blist = NULL;
  int bmaxlist;

void initialize()  /* initialize from data file */
{
  int f;
  facetedge_id fe;
  int i,j,k;
  int dataflag;
  int esize;
  REAL modulus;

  yylex_init();  /* reset lex */
  macro_init();  /* reset macros in parser */
  line_no = 1;
  parse_error_flag = 0;
  parse_errors = 0;
  recovery_flag = 0;
  facecount = 0;
  bare_edge_count = 0;
  verb_flag = 0;
  lists_flag = LISTS_OFF;
  vlist = NULL;
  elist = NULL;
  flist = NULL;
  blist = NULL;
  if ( !addload_flag )
    quantity_init(); 

  reset_timestamp = top_timestamp = ++global_timestamp;
  graph_timestamp = ++global_timestamp;
  /* read in any header information */
  topflag = 1;
  tok = yylex();
  while ( (tok != 0) && topflag )
  { switch ( tok )
     { 
        case SUPPRESS_WARNING_:
             if ( (tok = gettok(INTEGER_)) != INTEGER_ )
             { kb_error(4531,"Missing suppressed warning number.\n",WARNING);
               break;
             }
             if ( warnings_suppressed_count < MAXSUPPRESS )
               warnings_suppressed[warnings_suppressed_count++] = yylval.i;
             else
               kb_error(4532,"Too many warnings suppressed.\n",WARNING);
             
             tok = yylex();  /* eat the number */
             break;
      
       case UNSUPPRESS_WARNING_:
             if ( (tok = gettok(INTEGER_)) != INTEGER_ )
             { kb_error(4533,"Missing unsuppressed warning number.\n",WARNING);
               break;
             }
             for ( i = 0 ; i < warnings_suppressed_count ; i++ )
               if ( warnings_suppressed[i] == yylval.i )
               { warnings_suppressed[i] = warnings_suppressed[--warnings_suppressed_count];
                 break;
               }          
             tok = yylex();  /* eat the number */
             break;


        case HESSIAN_SPECIAL_NORMAL_VECTOR_:
             tok = yylex();
             for ( i = 0 ; i < SDIM ; i++ )
             { 
               if ( (tolower(yytext[0]) != 'c') || (yytext[1] != '1' + i) )
                  break;
               esize = exparse(SDIM,hessian_special_normal_expr+i,USERCOPY);
               sprintf(hessian_special_normal_expr[i].name,
                  "hessian_special_normal component %d",i);
               tok = yylex();
               if ( esize <= 0 )
               { sprintf(errmsg,
                 "Bad content component %d definition for hessian_special_normal_vector.\n",
                     i+1);
                 kb_error(2095,errmsg,DATAFILE_ERROR);
                 return;
               }
             }
             break;

        case LENGTH_METHOD_NAME_:   
             tok = yylex();
             if ( tok == QUOTATION_ )
             { strncpy(length_method_name,yytext,sizeof(length_method_name)-1);
               if ( strcmp(yytext,"circular_arc_length")==0 )
                 circular_arc_flag = 1;
               else if ( strcmp(yytext,"spherical_arc_length")==0 )
                 spherical_arc_flag = 1;
               tok = yylex();   
               if ( !option_q) option_q = 2;  /* convert_to_quantities */
             } 
             else
               kb_error(2485,"length_method_name needs quoted string.\n",
                      DATAFILE_ERROR);  
             break;

        case AREA_METHOD_NAME_:   
             tok = yylex();
             if ( tok == QUOTATION_ )
             { strncpy(area_method_name,yytext,sizeof(area_method_name)-1);
               tok = yylex();   
               if ( !option_q) option_q = 2;  /* convert_to_quantities */
             } 
             else
               kb_error(2590,"area_method_name needs quoted string.\n",
                      DATAFILE_ERROR);  
             break;

        case VOLUME_METHOD_NAME_:   
             tok = yylex();
             if ( tok == QUOTATION_ )
             { if ( SDIM == 2 ) 
                strncpy(area_method_name,yytext,sizeof(area_method_name)-1);
               else
                strncpy(volume_method_name,yytext,sizeof(volume_method_name)-1);
               tok = yylex();   
               if ( !option_q) option_q = 2;  /* convert_to_quantities */
             } 
             else
               kb_error(2096,"volume_method_name needs quoted string.\n",
                      DATAFILE_ERROR);  
             break;

        case KEEP_MACROS_ : keep_macros_flag = 1; tok = yylex();  break;
        case VERSION_:
             tok = yylex();
             strcpy(needed_version,yytext);
             tok = yylex();
             if ( strcmp(needed_version,evolver_version) > 0 )
             { sprintf(errmsg,"\nDatafile %s needs Evolver version at least %s.  This is version %s.\n\n",datafilename,needed_version,evolver_version);
               kb_error(2097,errmsg,RECOVERABLE);
             }
             break;
        case LOAD_LIBRARY_: 
             tok = yylex();
             if ( tok == QUOTATION_ )
             {  load_library(yytext);
                tok = yylex();
             }
             else
              kb_error(2098,"LOAD_LIBRARY file name missing.\n",DATAFILE_ERROR);
             recovery_flag = 0;
             break;

        case INTERP_BDRY_PARAM_:
             interp_bdry_param = 1; tok = yylex(); break;

        case EVERYTHING_QUANTITIES_:
             /*everything_quantities_flag = quantities_only_flag = 1;*/
             if ( !option_q) option_q = 2;
             tok = yylex();
             /* should warn if anything else done yet */
             break;

        case KEEP_ORIGINALS_: match_id_flag = 1; tok = yylex(); break;

        case SPACE_DIMENSION_:
             if ( (tok = gettok(INTEGER_)) != INTEGER_ )
             { kb_error(1060,"Dimension of space missing.\n",DATAFILE_ERROR);
               break;
             }
             if ( yylval.i > MAXCOORD )
             { sprintf(msg,"Space dimension too high.  Recompile with -DMAXCOORD=%d as compiler option.\n",yylval.i);
                kb_error(1061,msg,RECOVERABLE);
             }
             web.sdim = yylval.i;
             if ( web.sdim != DEFAULT_SDIM ) init_view();
             web.skel[BODY].dimension = yylval.i;
             if ( web.sdim != SDIM )
             { sprintf(errmsg,
                  "This Evolver compiled strictly for space dimension %d.\n",
                      SDIM);
               kb_error(1062,errmsg,RECOVERABLE);
             }
             tok = yylex();
             break;

        case SURFACE_DIMENSION_:
             if ( (tok = gettok(INTEGER_)) != INTEGER_ )
             { kb_error(1063,"Dimension of surface missing.\n",DATAFILE_ERROR);
               break;
             }
             web.dimension = yylval.i;
             if ( yylval.i > web.sdim )
                kb_error(1064,"Surface dimension higher than space dimension.\n",RECOVERABLE);

             star_fraction = web.dimension + 1.0;
             if ( web.representation == SIMPLEX )
             { web.skel[EDGE].ctrlpts = web.dimension;
                web.skel[EDGE].dimension = web.dimension - 1;
                web.skel[FACET].ctrlpts = web.dimension+1;
                web.skel[FACET].dimension = web.dimension;
             }
             else if ( web.dimension == 1 ) web.representation = STRING;
             tok = yylex();
             break;

        case SIMPLEX_REP_:
             web.representation = SIMPLEX;
             web.skel[EDGE].ctrlpts = web.dimension;
             web.skel[EDGE].dimension = web.dimension - 1;
             web.skel[FACET].ctrlpts = web.dimension+1;
             web.skel[FACET].dimension = web.dimension;
             tok = yylex();
             break;

        case DEFINE_:  /* extra attribute definition */
           { int dim,e_type=0,attr_type=0,anum;
             int sizes[MAXARRAYDIMS];
             char name[ATTR_NAME_SIZE+1];
             tok = yylex();
             switch ( tok )
             { case NEWIDENT_ : define_array(); goto define_exit;
               case ARRAYIDENT_ : define_array(); goto define_exit;
               case VERTICES_: e_type = VERTEX; break;
               case EDGES_:     e_type = EDGE; break;
               case FACES_:     e_type = FACET; break;
               case BODIES_:    e_type = BODY;  break;
               case FACETEDGES_: e_type = FACETEDGE; break;
               default: kb_error(1065,"Bad element type in 'define'.\n",
                  DATAFILE_ERROR);
             };
             tok = yylex();
             if ( tok != ATTRIBUTE_ )
                kb_error(1066,"Need ATTRIBUTE keyword.\n",DATAFILE_ERROR);

             tok = yylex();
             if ( (tok != NEWIDENT_) && !addload_flag )
                kb_error(1067,"Need new identifier. \n",DATAFILE_ERROR);

             strncpy(name,yytext,ATTR_NAME_SIZE);
             tok = yylex();
             if ( tok == DATATYPE_ )
             { attr_type = yylval.datatype;
             }
             else
             { kb_error(1068,"Need attribute datatype.\n",DATAFILE_ERROR);
               attr_type = REAL_TYPE; /* reasonable default */
             }
             tok = yylex();
             dim = 0; sizes[0] = 1;
             while ( tok == '[' )
             {  dim += 1;
                if ( dim > MAXARRAYDIMS )
                { sprintf(errmsg,
                "Extra attribute %s has more dimensions than the allowed %d.\n",
                    name,MAXARRAYDIMS);
                  kb_error(2566,errmsg,RECOVERABLE);
                } 
                tok = yylex();
                if ( tok != INTEGER_ )
                  kb_error(1069,"Need dimension number.\n",DATAFILE_ERROR);
                sizes[dim-1] = yylval.i;
                if ( sizes[dim-1] < 0 )
                  kb_error(1070,"Attribute dimension must be at least 0.\n",
                     DATAFILE_ERROR);
                tok = yylex();  
                tok = yylex(); /* eat ] */
             }
             anum = add_attribute(e_type,name,attr_type,dim,sizes,DUMP_ATTR,NULL);
             if ( (tok == FUNCTION_) && (dim == 0) )
             { /* have calculable attribute */
               struct extra *ext = EXTRAS(e_type) + anum;
               esize = exparse(SDIM,&(ext->code),USERCOPY);
               sprintf(ext->code.name,"attribute '%s' formula",name);
               tok = yylex();
               if ( esize <= 0 )
               { sprintf(errmsg,
                 "Bad function definition for attribute %s.\n",name);
                 kb_error(2099,errmsg,DATAFILE_ERROR);
               }
               ext->flags |= FUNCTION_ATTR;
             }
            
            } /* end DEFINE */
define_exit:
            break;
          
        case CONFORMAL_:  /* have background metric on domain */
             web.metric_flag = 1;
             web.conformal_flag = 1;
             recovery_flag = 0;
             uminus_flag = 0;
             if (exparse(SDIM,&web.metric[0][0],USERCOPY) <= 0 )
             { sprintf(errmsg,"Bad conformal metric definition.\n");
               kb_error(1071,errmsg,DATAFILE_ERROR);
             }
             sprintf(web.metric[0][0].name,"conformal metric"); 
             metric = dmatrix(0,SDIM-1,0,SDIM-1);
             metric_partial = dmatrix3(SDIM,SDIM,SDIM);
             det_array = dmatrix(0,web.dimension-1,0,web.dimension-1);
             tok = yylex();
             break;
             
        case KLEIN_METRIC_:
             klein_metric_flag = 1;
             web.metric_flag = 1;
             tok = yylex();
             break;
          
        case METRIC_:  /* have background metric on domain */
             web.metric_flag = 1;
             recovery_flag = 0;
             lists_flag = LISTS_SOME;
             uminus_flag = 1;
             for ( i = 0 ; i < SDIM ; i++ )
                for ( j = 0 ; j < SDIM ; j++ )
                { 
                  esize = exparse(SDIM,&web.metric[i][j],USERCOPY);
                  if ( esize <= 0 )
                  { sprintf(errmsg,
                        "Bad metric g[%d][%d] definition.\n",i,j);
                    kb_error(1072,errmsg,DATAFILE_ERROR);
                  }
                  sprintf(web.metric[i][j].name,"metric component [%d][%d]",
                      i+1,j+1);
                }
             metric = dmatrix(0,SDIM-1,0,SDIM-1);
             metric_partial = dmatrix3(SDIM,SDIM,SDIM);
             det_array = dmatrix(0,web.dimension-1,0,web.dimension-1);
             lists_flag = LISTS_OFF;
             tok = yylex();
             break;
             
        case SYMMETRY_GROUP_:
             web.symmetry_flag = 1;
             tok = yylex();
             if ( tok == QUOTATION_ )
             { struct sym_registry *reg;
               for ( reg = sym_register ; reg->name != NULL ; reg++ )
               if ( stricmp(yytext,reg->name) == 0 )
               { symmetry_name = reg->name;
                 sym_wrap = reg->wrapper;
                 sym_form_pullback = reg->pullback;
                 sym_inverse = reg->inverse;
                 sym_compose = reg->compose;
                 sym_flags = reg->flags;
                 break;
               }
               if ( reg->name == NULL ) /* search failed */
               { sprintf(errmsg,
                   "Symmetry name '%s' not found in registry.c \n", yytext);
                 kb_error(1073,errmsg,DATAFILE_ERROR);
               }
               if ( stricmp(yytext,"torus")==0 ) torus_period_init();
               tok = yylex();
             }
             else
               kb_error(1074,"Missing symmetry group name.\n",DATAFILE_ERROR);
             recovery_flag = 0;
             break;


        case TORUS_:
             web.torus_flag = 1;
             web.symmetry_flag = 1;
             sym_wrap = torus_wrap;
             sym_form_pullback = torus_form_pullback;
             sym_inverse = torus_inverse;
             sym_compose = torus_compose;
             torus_period_init();
             sym_flags = 0;
             tok = yylex();
             recovery_flag = 0;
             break;

        case TORUS_FILLED_:
             web.torus_flag = 1;
             web.symmetry_flag = 1;
             sym_wrap = torus_wrap;
             sym_form_pullback = torus_form_pullback;
             sym_inverse = torus_inverse;
             sym_compose = torus_compose;
             sym_flags = 0;
             torus_period_init();
             web.full_flag = 1;
             tok = yylex();
             recovery_flag = 0;
             break;

        case STRING_:
             web.dimension = 1;
             web.representation = STRING;
             web.skel[EDGE].dimension = 1;
             web.skel[FACET].dimension = 2;
             star_fraction = web.dimension + 1.0;
             edgeshow_flag = 0; /* for suppressing dummy facet edges */
             tok = yylex();
             recovery_flag = 0;
             break;

        case SOAPFILM_:
             web.dimension = 2;
             web.representation = SOAPFILM;
             star_fraction = web.dimension + 1.0;
             tok = yylex();
             recovery_flag = 0;
             break;

        case LINEAR_:
             if ( addload_flag )
             { if ( web.modeltype != LINEAR )
                 kb_error(5342,"addload datafile not in linear mode.\n",
                      RECOVERABLE);
             }
             web.modeltype = LINEAR;
             tok = yylex();
             recovery_flag = 0;
             break;
              
        case QUADRATIC_:
             if ( addload_flag )
             { if ( web.modeltype != QUADRATIC )
                 kb_error(5343,"addload datafile not in quadratic mode.\n",
                      RECOVERABLE);
             }
             else
               linear_to_quad();
             tok = yylex();
             recovery_flag = 0;
             break;

        case LAGRANGE_:
             switch ( web.modeltype )
             { case LINEAR: linear_to_lagrange(1); break;
               case QUADRATIC: quad_to_lagrange(1); break;
               case LAGRANGE:  lagrange_to_lagrange(1); break;
             }
             tok = yylex();
             recovery_flag = 0;
             break;

        case LAGRANGE_ORDER_:
             if ( (tok = gettok(INTEGER_)) != INTEGER_ )
             { kb_error(2100,"Lagrange order missing.\n",DATAFILE_ERROR);
               break;
             }
             if ( addload_flag )
             { if ( web.modeltype != LAGRANGE )
                 kb_error(5345,"addload datafile not in Lagrange mode.\n",
                      RECOVERABLE);
               if ( web.lagrange_order != yylval.i )
               { sprintf(errmsg,
           "addload datafile has different Lagrange order %d, should be %d.\n",
		           yylval.i,web.lagrange_order);
                 kb_error(5346,errmsg, RECOVERABLE);
               }
             }
             switch ( web.modeltype )
             { case LINEAR: linear_to_lagrange(yylval.i); break;
               case QUADRATIC: quad_to_lagrange(yylval.i); break;
               case LAGRANGE:  lagrange_to_lagrange(yylval.i); break;
             }
             tok = yylex();
             recovery_flag = 0;
             break;
              
        case SYMMETRIC_CONTENT_:
             web.symmetric_content = 1;
             tok = yylex();
             recovery_flag = 0;
             break;
              
        case MEAN_CURV_:
             web.area_norm_flag = 1;
             tok = yylex();
             recovery_flag = 0;
             break;
  
        case BOUNDARY_CURVATURE_:
             boundary_curvature_flag = 1;
             tok = yylex();
             recovery_flag = 0;
             break;

        case EFFECTIVE_AREA_:
             effective_area_flag = 1;
             tok = yylex();
             recovery_flag = 0;
             break;
              
        case JIGGLE_:
             web.jiggle_flag = 1;
             tok = yylex();
             recovery_flag = 0;
             break;
              

        case WULFF_: 
             tok = yylex();
             if ( tok == QUOTATION_ )
             { wulff_initialize(yytext);
               tok = yylex();
             }
             else
               kb_error(1076,"Wulff file name missing.\n",DATAFILE_ERROR);
             recovery_flag = 0;
             break;

        case PHASEFILE_: 
             tok = yylex();
             if ( tok == QUOTATION_ )
             { phase_initialize(yytext);
               tok = yylex();
             }
             else
               kb_error(1077,"Cannot find phasefile name.\n",DATAFILE_ERROR);

             recovery_flag = 0;
             break;

        case TORUS_PERIODS_:
        case PERIODS_:  recovery_flag = 0; 
                lists_flag = LISTS_SOME;
                uminus_flag = 1;
                read_periods();
                lists_flag = LISTS_OFF;
                tok = yylex(); /* lookahead */ 
                break;

        case DISPLAY_PERIODS_:  recovery_flag = 0; 
                lists_flag = LISTS_SOME;
                uminus_flag = 1;
                read_display_periods();
                lists_flag = LISTS_OFF;
                tok = yylex(); /* lookahead */ 
                break;

        case DISPLAY_ORIGIN_: 
          {  int n;
             struct global *g ;
             struct array *a;

             recovery_flag = 0;
                              
             n = lookup_global("display_origin");
             if ( n < 0 )
               n = add_global("display_origin");
             g = globals(n);
             g->flags |= INTERNAL_NAME|ARRAY_PARAM|RECALC_PARAMETER|ALWAYS_RECALC;
             a = g->attr.arrayptr = (struct array*)mycalloc(1,
                     sizeof(struct array)+sizeof(int));
             a->dim = 1;
             a->itemsize = sizeof(REAL);
             a->datatype = REAL_TYPE;
             a->datacount = SDIM*sizeof(REAL);
             a->sizes[0] = SDIM;
             a->datastart = (char*)web.display_origin-(char*)a;

             lists_flag = LISTS_SOME;
             uminus_flag = 1;
             lists_flag = LISTS_OFF;
             for ( i = 0 ; i < SDIM ; i++ )
             { if ( read_const(&web.display_origin[i]) <= 0 )
                 { kb_error(3861,"Not enough values for display_origin.\n",
                     DATAFILE_ERROR);
                   break;
                 }
             }
             if ( i >= SDIM ) tok = yylex();  /* lookahead */
          }
             break;

        case VIEW_MATRIX_: recovery_flag = 0; 
             lists_flag = LISTS_SOME;
             uminus_flag = 1;
             for ( i = 0 ; i <= SDIM ; i++ )
             { for ( j = 0 ; j <= SDIM ; j++ ) 
               { if ( read_const(&view[i][j]) <= 0 )
                 { kb_error(1861,"Not enough values for view matrix.\n",
                     DATAFILE_ERROR);
                   break;
                 }
               }     
             }
             datafile_view_flag = 1;
             lists_flag = LISTS_OFF;
             if ( i > SDIM ) tok = yylex();  /* lookahead */
             break;

        case VIEW_TRANSFORMS_: recovery_flag = 0; uminus_flag = 1; 
             read_transforms(0); break;

        case VIEW_TRANSFORM_GENS_: recovery_flag = 0; 
             uminus_flag = 1;
             read_transform_generators(0); break;

        case CLIP_COEFF:
             tok = yylex();  /* eat clip_coeff */
             if ( tok != '=' && tok != ASSIGN_ )
               kb_error(3441,"clip_coeff initializer missing '='.\n",
                  DATAFILE_ERROR);
             read_array_initializer(perm_globals(clip_coeff_global)->attr.arrayptr); 
             break;

        case SLICE_COEFF:
             tok = yylex();  /* eat clip_coeff */
             if ( tok != '=' && tok != ASSIGN_ )
               kb_error(3441,"clip_coeff initializer missing '='.\n",
                  DATAFILE_ERROR);
             read_array_initializer(perm_globals(slice_coeff_global)->attr.arrayptr); 
             break;


        case PARAMETERS_:  recovery_flag = 0; uminus_flag = 0; 
             read_parameter(); break;

        case OPTIMIZING_PARAMETER_:  recovery_flag = 0; uminus_flag = 0; 
             read_parameter(); break;

        case FUNCTION_:
             datafile_flag = 0; 
             function_kludge_flag = 1;
             command("function _anti_line_no_",NO_HISTORY);
             function_kludge_flag = 0;
             datafile_flag = 1;
             verb_flag = 0;
             tok = yylex();
             break;

        case PROCEDURE_WORD_: 
             datafile_flag = 0; 
             function_kludge_flag = 1;
             command("procedure _anti_line_no_",NO_HISTORY);
             function_kludge_flag = 0;
             datafile_flag = 1;
             verb_flag = 0;
             tok = yylex();
             break;

        case BOUNDARY_:  recovery_flag = 0; uminus_flag = 0; 
             read_boundary(); break;

        case CONSTRAINT_:  recovery_flag = 0; uminus_flag = 0; 
             read_constraint(); break;

        case SURFACE_ENERGY_: recovery_flag = 0; uminus_flag = 0; 
             read_surface_energy(); break;

        case QUANTITY_: recovery_flag = 0; uminus_flag = 0; 
             read_quantity(); break;

        case METHOD_INSTANCE_: recovery_flag = 0; uminus_flag = 0; 
             read_method_instance(); break;

        case AREA_FIXED_:
             kb_error(2101,
                "Area_fixed is obsolete.  Replace with named quantity.\n",
                 DATAFILE_ERROR);
             area_fixed_flag = 1;
             uminus_flag = 0;
             if ( read_const(&area_fixed_target) <= 0 )
             { kb_error(1078,"Missing fixed area value.\n",DATAFILE_ERROR);
                if ( tok == AREA_FIXED_ ) tok = yylex(); /* ensure progress */
             }
             else tok = yylex();
             recovery_flag = 0;
             break;

        case DIFFUSION_:
             web.diffusion_flag = 1;
             web.diffusion_const = 0.0;
             uminus_flag = 0;
             if ( read_const(&web.diffusion_const) <= 0 )
             { kb_error(1079,"Missing DIFFUSION value.\n",WARNING);
               if ( tok == DIFFUSION_ ) tok = yylex(); /* ensure progress */
             }
             else tok = yylex();
             recovery_flag = 0;
             break;

        case HOMOTHETY_:
             web.homothety = 1;
             uminus_flag = 0;
             if ( read_const(&homothety_target) <= 0 )
                homothety_target = 1.0; 
             else tok = yylex();
             break;

        case AUTOPOP_:
             autopop_flag = 1;
             tok = yylex();
             recovery_flag = 0;
             break;
              
        case IMMEDIATE_AUTOPOP_:
             immediate_autopop_flag = 1;
             tok = yylex();
             recovery_flag = 0;
             break;
              
        case AUTOPOP_QUARTIC_:
             autopop_quartic_flag = 1;
             tok = yylex();
             recovery_flag = 0;
             break;
              
        case AUTOCHOP_:
             autochop_flag = 1;
             autochop_length = 1.0;
             uminus_flag = 0;
             if ( read_const(&autochop_length) <= 0 )
             { kb_error(1080,"Missing AUTOCHOP length.\n",WARNING);
               if ( tok == AUTOCHOP_ ) tok = yylex(); /* ensure progress */
             }
             else tok = yylex();
             recovery_flag = 0;
             break;
             
        case APPROX_CURV_:
             approx_curve_flag = 1;
             tok = yylex();
             recovery_flag = 0;
             break;

        case CONDUCTING_KNOT_ENERGY_:
             { tok = yylex();
               if ( tok != MODULUS_ ) 
                 unput_tok();
               uminus_flag = 0;
               if (read_const(&modulus) <= 0) modulus = 1.0;
               else tok = yylex();
               add_standard_quantity("knot_energy",modulus);
               recovery_flag = 0;
               break;
             }
             
        case INSULATING_KNOT_ENERGY_:
              { tok = yylex();
                if ( tok != MODULUS_ ) 
                  unput_tok();
                uminus_flag = 0;
                if (read_const(&modulus) <= 0) modulus = 1.0;
                else tok = yylex();
                add_standard_quantity("uniform_knot_energy",modulus);
                recovery_flag = 0;
                break;
              }
             
        case MEAN_CURV_INT_:
             mean_curv_int_flag = 1;
             mean_curvature_param = lookup_global("mean_curvature_modulus");
             if ( mean_curvature_param < 0 )
                mean_curvature_param = add_global("mean_curvature_modulus");
             globals(mean_curvature_param)->flags |= 
                             ORDINARY_PARAM | RECALC_PARAMETER | ALWAYS_RECALC;
             uminus_flag = 0;
             if ( read_const(&globals(mean_curvature_param)->value.real) <= 0 )
             { kb_error(1081,"Missing integral mean curvature modulus value.\nSyntax: mean_curvature_integral: modulus",WARNING);
                if ( tok == MEAN_CURV_INT_ ) tok = yylex(); /* ensure progress */
             }
             else tok = yylex();
             if ( web.representation != SOAPFILM )
                  kb_error(1082,"Can do integral of mean curvature only in SOAPFILM model.\n",
                     DATAFILE_ERROR);
             recovery_flag = 0;
             break;

        case GAUSS_CURVATURE_:
              { tok = yylex();
                if ( tok != MODULUS_ ) 
                  unput_tok();
                uminus_flag = 0;
                if (read_const(&modulus) <= 0) modulus = 1.0;
                else tok = yylex();
                if ( (web.representation != SOAPFILM) )
                  kb_error(1083,"Can do gauss curvature only in SOAPFILM model.\n",
                     DATAFILE_ERROR);
                else add_standard_quantity("gauss_curvature_integral",modulus);
                recovery_flag = 0;
                break;
              }
             
        case SQGAUSS_:
             sqgauss_flag = 1;
             sqgauss_param = lookup_global("square_gauss_modulus");
             if (sqgauss_param < 0 )
               sqgauss_param = add_global("square_gauss_modulus");
             globals(sqgauss_param)->flags |= ORDINARY_PARAM | RECALC_PARAMETER | ALWAYS_RECALC;
             uminus_flag = 0;
             if ( read_const(&globals(sqgauss_param)->value.real) <= 0 )
             { kb_error(1084,"Missing square gaussian modulus value.\n",WARNING);
               if ( tok == SQGAUSS_ ) tok = yylex(); /* ensure progress */
             }
             else tok = yylex();
             if ( web.representation != SOAPFILM )
               kb_error(1085,
                   "Can do square gauss curvature only in SOAPFILM model.\n",
                     DATAFILE_ERROR);
             recovery_flag = 0;
             break;

        case SQUARE_CURVATURE_:
             square_curvature_flag = 1;
             square_curvature_param = lookup_global("sq_curvature_modulus");
             if ( square_curvature_param < 0 )
               square_curvature_param = add_global("sq_curvature_modulus");
             globals(square_curvature_param)->flags |= 
                                  ORDINARY_PARAM | RECALC_PARAMETER | ALWAYS_RECALC;
             uminus_flag = 0;
             if ( read_const(&globals(square_curvature_param)->value.real) <= 0 )
             { kb_error(1086,"Missing square curvature modulus value.\n",WARNING);
               if ( tok == SQUARE_CURVATURE_ ) tok = yylex(); /* ensure progress */
             }
             else tok = yylex();
             recovery_flag = 0;
             break;

        case NORMAL_CURVATURE_:
             normal_curvature_flag = 1;
             tok = yylex();
             break;

        case MOBILITY_:
             recovery_flag = 0;
             uminus_flag = 0;
             esize = exparse(SDIM,&mobility_formula,USERCOPY);
             sprintf(mobility_formula.name,"mobility formula");
             tok = yylex();
             if ( esize <= 0 )
                kb_error(1087,"Bad mobility definition.\n",DATAFILE_ERROR);
             else mobility_flag = 1; 
             break;
             
        case MOBILITY_TENSOR_:
             mobility_flag = 1;
             mobility_tensor_flag = 1;
             recovery_flag = 0;
             lists_flag = LISTS_SOME;
             uminus_flag = 1;
             for ( i = 0 ; i < SDIM ; i++ )
                for ( j = 0 ; j < SDIM ; j++ )
                { esize = exparse(SDIM,&mobility_tensor[i][j],USERCOPY);
                  if ( esize <= 0 )
                  { sprintf(errmsg,
                       "Bad mobility_tensor[%d][%d] definition.\n",i,j);
                    kb_error(1088,errmsg,DATAFILE_ERROR);
                  }
                  sprintf(mobility_tensor[i][j].name,
                    "mobility tensor component [%d][%d]",i+1,j+1);
                }
             lists_flag = LISTS_OFF;
             tok = yylex();
             break;
             
        case RUNGE_KUTTA_: runge_kutta_flag = 1; tok = yylex(); break;

        case SCALE_LIMIT_:
             uminus_flag = 0;
             if ( read_const(&web.maxscale) <= 0 )
             { kb_error(1089,"Missing SCALE_LIMIT value.\n",WARNING);
               if ( tok == SCALE_LIMIT_ ) tok = yylex(); /* ensure progress */
             }
             else tok = yylex();
             recovery_flag = 0;
             break;

        case TOTAL_TIME_:
             uminus_flag = 0;
             if ( read_const(&total_time) <= 0 )
             { kb_error(1090,"Missing TOTAL_TIME value.\n",WARNING);
                if ( tok == TOTAL_TIME_ ) tok = yylex(); /* ensure progress */
             }
             else tok = yylex();
             recovery_flag = 0;
             break;

        case ZOOM_RADIUS_:
             uminus_flag = 0;
             if ( read_const(&web.zoom_radius) <= 0 )
             { kb_error(1091,"Missing ZOOM RADIUS value.\n",WARNING);
               if ( tok == ZOOM_RADIUS_ ) tok = yylex(); /* ensure progress */
             }
             else tok = yylex();
             recovery_flag = 0;
             break;

        case ZOOM_VERTEX_:
             tok = yylex();
             if ( tok != INTEGER_ )
                kb_error(1092,"Missing ZOOM VERTEX number.\n",WARNING);
             else { zoom_number = yylval.i; tok = yylex(); }
             recovery_flag = 0;
             break;

        case V_INTEGRAL_ORDER:
             tok = yylex();
             if ( tok != INTEGER_ )
                { kb_error(1093,"Missing INTEGRAL_ORDER value.\n",WARNING);
                  break;
                }
             if ( yylval.i < 1 )
              { sprintf(errmsg,"Invalid INTEGRAL_ORDER value %d.\n",yylval.i);
                kb_error(1094,errmsg,WARNING);
              }
             else
             { web.gauss1D_order = 2*yylval.i-1; 
               set_by_user_gauss_1D = web.gauss1D_order;
               web.gauss2D_order = yylval.i;
               set_by_user_gauss_2D = web.gauss2D_order;
             }
             tok = yylex();
             recovery_flag = 0;
             break;

        case V_INTEGRAL_ORDER_1D:
             tok = yylex();
             if ( tok != INTEGER_ )
                { kb_error(1095,"Missing INTEGRAL_ORDER_1D value.\n",WARNING);
                  break;
                }
             if ( yylval.i < 1 )
              { sprintf(errmsg,"Invalid INTEGRAL_ORDER_1D value %d.\n",yylval.i);
                 kb_error(1096,errmsg,WARNING);
              }
             else {  web.gauss1D_order = yylval.i; 
                     set_by_user_gauss_1D = web.gauss1D_order;
                  }
             tok = yylex();
             recovery_flag = 0;
             break;

        case V_INTEGRAL_ORDER_2D:
             tok = yylex();
             if ( tok != INTEGER_ )
                { kb_error(1097,"Missing INTEGRAL_ORDER_2D value.\n",WARNING);
                  break;
                }
             if ( yylval.i < 1 )
              { sprintf(errmsg,"Invalid INTEGRAL_ORDER_2D value %d.\n",yylval.i);
                kb_error(1098,errmsg,WARNING);
              }
             else {  web.gauss2D_order = yylval.i; 
                     set_by_user_gauss_2D = web.gauss2D_order;
                  }
             tok = yylex();
             recovery_flag = 0;
             break;

        case CONSTRAINT_TOLERANCE_:
             uminus_flag = 0;
             if ( read_const(&web.tolerance) <= 0 )
             { kb_error(1099,"Missing CONSTRAINT_TOLERANCE value.\n",WARNING);
               if ( tok == CONSTRAINT_TOLERANCE_ ) tok = yylex(); /* ensure progress */
             }
             else tok = yylex();
             if ( web.tolerance <= 0.0 )
             { kb_error(2102,"Tolerance must be positive.\n",WARNING);
               web.tolerance = DEFAULT_TOLERANCE;
             }
             recovery_flag = 0;
             break;

        case MERITFACTOR_:
             uminus_flag = 0;
             if ( read_const(&web.meritfactor) <= 0 )
                kb_error(1100,"Missing MERIT FACTOR value.\n",WARNING);
             else tok = yylex();
             recovery_flag = 0;
             break;

        case GRAV_CONST_:
             uminus_flag = 0;
             if ( read_const(&web.grav_const) <= 0 )
             { kb_error(1101,"Missing GRAVITY_CONSTANT value.\n",WARNING);
               if ( tok == GRAV_CONST_ ) tok = yylex(); /* ensure progress */
             }
             else 
             { if ( web.grav_const != 0.0 )  web.gravflag = 1;
               tok = yylex();
             }
             recovery_flag = 0;
             break;

        case SPRING_CONSTANT_:
        case GAP_CONSTANT_:
             uminus_flag = 0;
             if ( read_const(&web.spring_constant) <= 0 )
             { kb_error(1102,"Missing GAP_CONSTANT value.\n",WARNING);
               if ( tok == SPRING_CONSTANT_ ) tok = yylex(); /* ensure progress */
             }
             else tok = yylex();
             recovery_flag = 0;
             break;

        case SCALE_:
             uminus_flag = 0;
             if ( read_const(&web.scale) <= 0 )
             { kb_error(1103,"Missing SCALE value.\n",WARNING);
               if ( tok == SCALE_ ) tok = yylex(); /* ensure progress */
             }
             else tok = yylex();
             if ( tok == FIXED_ )
             { web.motion_flag = 1;
               tok = yylex();
             }
             recovery_flag = 0;
             break;

        case TEMPERATURE_:
             uminus_flag = 0;
             if ( read_const(&web.temperature) <= 0 )
             { kb_error(1104,"Missing TEMPERATURE value.\n",WARNING);
               if ( tok == TEMPERATURE_ ) tok = yylex(); /* ensure progress */
             }
             else tok = yylex();
             recovery_flag = 0;
             break;

        case PRESSURE_:
             uminus_flag = 0;
             if ( read_const(&web.pressure) <= 0 )
             { kb_error(1105,"Missing PRESSURE value.\n",WARNING);
               if ( tok == PRESSURE_ ) tok = yylex(); /* ensure progress */
             }
             else 
             { tok = yylex(); web.pressure_flag = 1; }
             recovery_flag = 0;
             break;

        case VERTICES_: lists_flag = LISTS_FULL; topflag = 0; break;  
            /* done with top stuff */

        default: 
             if ( !recovery_flag )
                { sprintf(errmsg,"Illegal token '%s'.\n",yytext);
                  kb_error(1106,errmsg,PARSE_ERROR);
                }
             tok = yylex();
             break;
     }
  }

  /* see that all forward declarations resolved */
  check_forwards();
  
  /* set up gaussian quadrature */
  gauss_setup();
   
  if ( web.torus_flag )
     calc_periods(ADJUST_VOLUMES);  /* adjust torus volume constants */

  reset_view();   /* can do this now, since know ambient dimension */
   #ifdef HASH_ID
  elhash_bigger();
  #endif

  dataflag = 1;
  uminus_flag = 1;
  while ( (tok != 0) && dataflag )
    switch ( tok )
     { 
        case VERTICES_: 
             if ( (web.dimension > 2) && (web.representation != SIMPLEX) )
                kb_error(1107,
             "Must have simplex representation for surface dimension over 2.\n",
                UNRECOVERABLE);
             if ( elist || flist || blist )
               kb_error(2409,"Vertices list must be first element list.\n",
                          RECOVERABLE);
             recovery_flag = 0; 
             read_vertices();
             break;

        case EDGES_: 
             if (  flist || blist )
               kb_error(2410,"Edges list must be second element list.\n",
                          RECOVERABLE);
             recovery_flag = 0; 
             read_edges();
             break;

        case FACES_: 
             if (  blist )
               kb_error(2411,"Faces list must precede body list.\n",
                          RECOVERABLE);
             recovery_flag = 0; 
             read_faces();
             break;

        case BODIES_: 
             recovery_flag = 0; 
             read_bodies();
             break;

        case READ_: 
             if ( addload_flag )
             { pop_commandfd();
               lists_flag = LISTS_OFF;     
               dataflag = 0;  /* end of element data */
               break;
             }
             /* read commands from datafile */
             lists_flag = LISTS_OFF;
             read_command_flag = 1; 
             dataflag = 0;  /* end of element data */
             line_no++; /* kludge */
             break;

        default: 
             if ( !recovery_flag )
                { sprintf(errmsg,"Illegal token '%s'.\n",yytext);
                  kb_error(1108,errmsg,PARSE_ERROR);
                }
             brace_depth = parens = in_quote = 0; /* just in case */
             tok = yylex();
             break;
     }
             
  if ( parse_errors ) return;
  
  datafile_flag = 1;  /* got zeroed by pop_commandfd; need to read x1 */
  if ( option_q )
     convert_to_quantities();
  if ( optparamcount && !everything_quantities_flag )
     convert_to_quantities();
  if ( (web.modeltype == LAGRANGE) && !everything_quantities_flag )
  { outstring("Converting to named quantities for Lagrange model.\n");
    option_q = 1;  /* so convert_to_quantities doesn't try to calc */
    convert_to_quantities();
    option_q = 0;
  }
  for ( i = 0 ; i < web.bdrymax ; i++ )
    if ( (web.boundaries[i].attr & (CON_ENERGY|CON_CONTENT)) 
                 && !everything_quantities_flag  )
    { outstring("Converting to named quantities for boundary integrals.\n");
      option_q = 1;  /* so convert_to_quantities doesn't try to calc */
      convert_to_quantities();
      option_q = 0;
      break;
    }
  datafile_flag = 0;
  
  if ( web.dimension == 1 ) areaname = "length";
  else areaname = "area";

  mark_recalc_params();

#ifdef MPI_EVOLVER
  mpi_task_set_thin_corona();  /* get neighbor elements, with basic corona */
#endif

  /* create initial triangulation of each face */

  if ( web.representation == SOAPFILM )
    for ( f = 1 ; f <= facecount ; f++ )
    { facetedge_id first_fe;
      int edgecount = 0;

      if ( !valid_id(flist[f]) ) continue;

      /* see how many edges, and if we want to refine */
      fe = first_fe = get_facet_fe(flist[f]);
      if ( valid_id(fe) ) do
      { edgecount++;
        fe = get_next_edge(fe);
      } while ( valid_id(fe) && !equal_id(fe,first_fe) );
      if ( edgecount > 3 ) face_triangulate(flist[f],edgecount);  
    }

 
  /* straighten out facet order around edges */
  if ( web.representation == SOAPFILM )
  { edge_id e_id;
    MFOR_ALL_EDGES(e_id)
      fe_reorder(e_id);
  }

  /* put facet-edges on string network if no facets defined */

  if ( web.representation == STRING )
     string_fixup();


  /* phase boundary energies */
  if ( phase_flag && (web.representation == STRING) )
  { edge_id e_id;
    FOR_ALL_EDGES(e_id)
      set_e_phase_density(e_id);
  }
  if ( phase_flag && (web.representation != STRING) )
  { facet_id f_id;
    FOR_ALL_FACETS(f_id)
      set_f_phase_density(f_id);
  }

  /* run preliminary checks */
  if ( web.representation != SIMPLEX  )
     if ( facetedge_check(PRELIMCHECK) || facet_body_check() )
        kb_error(1109,"Bad data file.\n",DATAFILE_ERROR);
  if ( vlist == NULL )
     kb_error(1110,"No vertices found in datafile.\n",WARNING);
  if ( (elist == NULL) && (web.representation != SIMPLEX) )
     kb_error(1111,"No edges found in datafile.\n",WARNING);


  if ( vlist ) myfree((char *)vlist);
  if ( elist ) myfree((char *)elist);
  if ( flist ) myfree((char *)flist);
  if ( blist ) myfree((char *)blist);
         
  for ( k = 2, web.simplex_factorial = 1.0 ; k <= web.dimension ; k++ )
     web.simplex_factorial *= k;
  volume_factorial = web.simplex_factorial*SDIM;
  if ( web.representation == SIMPLEX )
  { refine_simplex_init();
  }
  else  if ( web.modeltype == LINEAR )
  { calc_facet_energy =  facet_energy_l;
    calc_facet_forces =  facet_force_l;
    calc_facet_volume = facet_volume_l;
    film_grad = film_grad_l;
    calc_edge_energy = edge_energy_l;
    calc_edge_forces  = edge_force_l;
    calc_edge_area = edge_area_l;
    string_grad = string_grad_l;
  }

  if ( transform_count == 0 )  /* set up identity transform */
  { transform_count = 1;
    view_transforms = dmatrix3(1,SDIM+1,SDIM+1);
    for ( j = 0 ; j <= SDIM ; j++ )
      view_transforms[0][j][j] = 1.0;
    set_view_transforms_global();
  }
  
  if ( autopop_flag )
  { int n;
    if ( web.representation == STRING )
      sprintf(msg,"Number of vertices popped: %d\n", n = verpop_str());
    else
      sprintf(msg,"Number of vertices popped: %d\n", n = edgepop_film());
    outstring(msg);
    if ( n > 0 ) update_display();
  }

  if ( web.homothety && (web.skel[BODY].count == 0) )
     { web.homothety = 0;
       kb_error(1112,"Cannot do homothety without bodies. Homothety OFF.\n",RECOVERABLE);
     }

  if ( sym_flags & NEED_FORM_UNWRAPPING )
     if ( auto_convert_flag ) convert_to_quantities();

}  /* end initialize */


/************************************************************************
*
* function: read_single_value()
*
* purpose: read one value of desired type from datafile
*
* return: 1 for success, 0 for failure
*/

int read_single_value(type,dest)
int type;
char *dest;
{ REAL val;

        switch (type )
        { 
          case CHAR_TYPE : 
                if ( read_const(&val) >= 0 )
                  *(char*)dest = (char)val; 
                else return 0;
                break;
          case UCHAR_TYPE : 
                if ( read_const(&val) >= 0 )
                  *(unsigned char*)dest = (unsigned char)val; 
                else return 0;
                break;
          case SHORT_TYPE : 
                if ( read_const(&val) >= 0 )
                  *(short*)dest = (short)val; 
                else return 0;
                break;
          case USHORT_TYPE : 
                if ( read_const(&val) >= 0 )
                  *(unsigned short*)dest = (unsigned short)val; 
                else return 0;
                break;
          case INTEGER_TYPE : 
                if ( read_const(&val) >= 0 )
                   *(int*)dest = (int)val; 
                else return 0;
                break;
          case REAL_TYPE : 
                if ( read_const(&val) >= 0 )
                   *(REAL*)dest = val; 
                else return 0;
                break;
          case ULONG_TYPE : 
                if ( read_const(&val) >= 0 )
                   *((unsigned long *)dest) = (unsigned long)val;
                else return 0;
                break;
          
          case STRING_TYPE:
                if ( tok == QUOTATION_ )
                { *(char**)dest = mycalloc(strlen(yytext)+1,sizeof(char));
                  strcpy(*(char**)dest,yytext);
                }
                else return 0;

          case ELEMENTID_TYPE:
                  return 0;
          case VERTEX_TYPE:
               if ( tok != VERTICES_ ) return 0;
               tok = yylex();
               if ( tok != '[' ) return 0;
               tok = yylex();
               if ( (tok != INTEGER_) && (tok != INTEGER_AT_) ) return 0;
               *(element_id*)dest = get_ordinal_id(VERTEX,yylval.i);
               tok = yylex();
               if ( tok != ']' ) return 0;
               break;
                
          case EDGE_TYPE:
               if ( tok != EDGES_ ) return 0;
               tok = yylex();
               if ( tok != '[' ) return 0;
               tok = yylex();
               if ( (tok != INTEGER_) && (tok != INTEGER_AT_) ) return 0;
               *(element_id*)dest = get_ordinal_id(EDGE,yylval.i);
               tok = yylex();
               if ( tok != ']' ) return 0;
               break;
                
          case FACET_TYPE:
               if ( tok != FACETS_ ) return 0;
               tok = yylex();
               if ( tok != '[' ) return 0;
               tok = yylex();
               if ( (tok != INTEGER_) && (tok != INTEGER_AT_) ) return 0;
               *(element_id*)dest = get_ordinal_id(FACET,yylval.i);
               tok = yylex();
               if ( tok != ']' ) return 0;
               break;
                
          case BODY_TYPE:
               if ( tok != BODIES_ ) return 0;
               tok = yylex();
               if ( tok != '[' ) return 0;
               tok = yylex();
               if ( (tok != INTEGER_) && (tok != INTEGER_AT_) ) return 0;
               *(element_id*)dest = get_ordinal_id(BODY,yylval.i);
               tok = yylex();
               if ( tok != ']' ) return 0;
               break;
                
          case FACETEDGE_TYPE:
               if ( tok != FACETEDGES_ ) return 0;
               tok = yylex();
               if ( tok != '[' ) return 0;
               tok = yylex();
               if ( (tok != INTEGER_) && (tok != INTEGER_AT_) ) return 0;
               *(element_id*)dest = get_ordinal_id(FACETEDGE,yylval.i);
               tok = yylex();
               if ( tok != ']' ) return 0;
               break;
                
          case CONSTRAINT_TYPE:
               if ( tok == CONSTRAINT_ )
               { tok = yylex();
                 if ( tok == INTEGER_ )
                   *(int *)dest = yylval.i;
                 else if ( tok == CONSTRAINT_NAME_ ) 
                   *(int *)dest = yylval.i;
                 else return 0;
               }
               else
               { if ( tok == CONSTRAINT_NAME_ ) 
                   *(int *)dest = yylval.i;
                 else return 0;
               }
               break;
          case BOUNDARY_TYPE: 
               if ( tok == BOUNDARY_ )
               { tok = yylex();
                 if ( tok == INTEGER_ )
                   *(int *)dest = yylval.i;
                 else if ( tok == BOUNDARY_NAME_ ) 
                   *(int *)dest = yylval.i;
                 else return 0;
               }
               else
               { if ( tok == BOUNDARY_NAME_ ) 
                   *(int *)dest = yylval.i;
                 else return 0;
               }
               break;

          case QUANTITY_TYPE:
               if ( tok == QUANTITY_ )
                 tok = yylex();
               if ( tok == QUANTITY_NAME_ ) 
                 *(int *)dest = yylval.i;
               else return 0;
               break;

          case INSTANCE_TYPE:
               if ( tok == METHOD_INSTANCE_ )
                 tok = yylex();
               if ( tok == METHOD_NAME_ ) 
                 *(int *)dest = yylval.i;
               else return 0;
               break;

          case PROCEDURE_TYPE:
               if ( tok == PROCEDURE_WORD_ )
                 tok = yylex();
               if ( tok == PROCEDURE_IDENT_ ) 
                 *(int *)dest = yylval.i;
               else return 0;
               break;

        } 
  return 1;
}

/************************************************************************
*
*  Function: read_attribute_value()
*
*  Purpose: read list of values for one attribute in element definition.
*/

void read_attribute_value(ex,datastart)
struct extra *ex;  /* attribute definition */
void *datastart;   /* destination for data */
{
    char *spot;
    int depth;
    int blocksize;
    char *spots[MAXARRAYDIMS];
    int items[MAXARRAYDIMS];
    int first_bracket_flag = 0; /* whether seen first bracket, 
                                   which is optional for 1 dim attribute */
    int no_first_bracket = 0;  /* record option */

    depth = 0;
    spot = datastart;
    if ( ex->array_spec.dim == 0 ) /* scalar */
    { if ( !read_single_value(ex->type,spot) )
      { sprintf(errmsg,"missing value for attribute %s.\n",ex->name);
        kb_error(2587,errmsg,DATAFILE_ERROR);
      }
      tok = yylex();  /* get lookahead */
      return;
    }
    spots[depth] = spot;
    items[depth] = 0;
    blocksize = ex->array_spec.datacount;
    for (;;)
    { 
      if ( depth == ex->array_spec.dim )
      { int k;
        for ( k = 0 ; ; k++ )
        { /* kludge with LEAD_INTEGER_ here */
          if ( tok != LEAD_INTEGER_ )
          { 
            if ( read_single_value(ex->type,spot) )
            { if ( k < ex->array_spec.sizes[depth-1] ) 
                spot += ex->array_spec.itemsize;
              else
              { sprintf(errmsg, "Too many initializers for attribute %s.\n",
                    ex->name);
                kb_error(2511,errmsg,DATAFILE_ERROR);
              }
            }
            else
            { tok = yylex();  /* get back token exparse pushed back */
              if ( tok != ',' ) 
                break;
            }
          }
          else break;
        }
      }
      if ( !first_bracket_flag ) tok = yylex();
      if ( (tok == '{') || !first_bracket_flag )  
      { if ( tok != '{' ) 
        { unput_tok(); no_first_bracket = 1; }
        first_bracket_flag = 1;
        if ( blocksize ) blocksize /= ex->array_spec.sizes[depth]; 
        spots[depth] = spot;
        items[depth]++;
        if ( (depth > 0) && (items[depth] > ex->array_spec.sizes[depth-1]) )
           kb_error(2133,"Too many initializers.\n",DATAFILE_ERROR);
        depth++; 
        items[depth] = 0;
        if ( (depth != ex->array_spec.dim) && !no_first_bracket ) tok = yylex();
      }
      else if ( tok == '}' )
      { tok = yylex();
        depth--;
        if ( depth == 0 ) return;

        blocksize *= ex->array_spec.sizes[depth];
        spots[depth] += blocksize*ex->array_spec.itemsize;
        spot = spots[depth];
      }
      else if ( tok == ',' )
      { tok = yylex();
        spots[depth] += blocksize*ex->array_spec.itemsize;
      }
      else if ( no_first_bracket )
      { /* must be end of 1-D attribute */
        if ( unput_tok_count )
           tok = yylex();  /* get lookahead back */
        return;
      }
      else
      { sprintf(errmsg,"Illegal token in initialization of attribute %s.\n",
              ex->name);
        kb_error(2134,errmsg, DATAFILE_ERROR);
      }
    }
}

/************************************************************************
*
*  Function: read_extra()
*
*  purpose: read extra attribute of element, attribute name still in yytext.
*/
void read_extra(el_id,exnum)
element_id el_id;
int exnum;
{ char *spot;
  int type = id_type(el_id);
  struct extra *ex = EXTRAS(type) + exnum;

  spot = get_extra(el_id,exnum);
  read_attribute_value(ex,spot);

}
/************************************************************************
*
*  Function: read_vertices()
*
*/

void read_vertices()
{
  int k;
  REAL c[MAXCOORD];  /* temporary number buffer */
  int cnum,bnum,pcount,qnum;
  struct boundary *bdry;
  struct constraint *constr;
  int more_attr;
  int sdim = SDIM;

  /* allocate space in structures */
  expand_attribute(VERTEX,V_COORD_ATTR,&sdim);
  expand_attribute(VERTEX,V_OLDCOORD_ATTR,&sdim);
  expand_attribute(VERTEX,V_FORCE_ATTR,&sdim);
  expand_attribute(VERTEX,V_VELOCITY_ATTR,&sdim);
  if ( web.maxparam > 0 )
  { expand_attribute(VERTEX,V_PARAM_ATTR,&web.maxparam);
  }

  /* read in vertex coordinates */
  vmaxlist = MAXLIST;
  vlist = (vertex_id *)mycalloc(sizeof(vertex_id),vmaxlist);
  if ( tok != VERTICES_  ) 
     kb_error(1115,"Cannot find VERTICES section of the datafile.\n",
          UNRECOVERABLE);

  tok = yylex();
  while ( (tok == LEAD_INTEGER_) || (tok == LEAD_INTEGER_AT_) )
  { 
    #ifdef MPI_EVOLVER
    /* test task number, for MPI */
    int task;
    if ( tok == LEAD_INTEGER_ )
       task = 1;  /* default to task 1 */
    else task = yylval.qnum;
    if ( task >= mpi_nprocs || task < 1 )
    { sprintf(errmsg,"Task number %d must be between 1 and %d, %d\n",
         task,mpi_nprocs-1);
      kb_error(5006,errmsg,RECOVERABLE);
    }
    if ( task != this_task )
    { /* skip this vertex */
      int flag = 0;
      while ( !flag ) 
      { tok = yylex();
        switch (tok)
        { case LEAD_INTEGER_: 
          case LEAD_INTEGER_AT_:
          case EDGES_:
          case FACES_:
          case BODIES_:
          case READ_:
          case 0:
            flag = 1;
            break;
        }
      }
      continue;  /* next vertex */
    }
    #endif
   
    k = yylval.i;
    if ( k < 1 ) 
       kb_error(2103,"Vertex number must be positive.\n",DATAFILE_ERROR);
    for ( pcount = 0 ; pcount < SDIM ; pcount++ )
    { if ( read_const(&c[pcount]) <= 0 ) break;
      if ( (tok == LEAD_INTEGER_) || (tok == LEAD_INTEGER_AT_) ) 
      { pcount++; break; }
    }
    tok = yylex(); /* get lookahead */


    while ( k >= vmaxlist )
    { int spot = vmaxlist; 
      vlist = (vertex_id *)kb_realloc((char *)vlist,
               (k+MAXLIST)*sizeof(vertex_id));
      vmaxlist = k + MAXLIST;
      for ( ; spot < vmaxlist ; spot ++ ) vlist[spot] = NULLID;
    }
    if ( valid_id(vlist[k]) )
    { sprintf(errmsg,"Duplicate vertex number %d\n",k);
      kb_error(1117,errmsg,DATAFILE_ERROR);
    }
    move_to_free_front(VERTEX,k); /* so id will be k */
    vlist[k] = new_vertex(c,NULLID);
    set_original(vlist[k],(k-1)|((element_id)VERTEX<<TYPESHIFT)|VALIDMASK);

    /* attributes */
    if ( web.con_global_map )
    { set_v_global(vlist[k]);
    }
    for ( more_attr = 1 ; more_attr ; )
      switch ( tok )
        {
           case EXTRA_ATTRIBUTE_:
           case ARRAY_ATTRIBUTE_:
              read_extra(vlist[k],yylval.qnum);
              break;

           case BARE_:
              set_attr(vlist[k],BARE_NAKED);
              tok = yylex();
              break;

           case AXIAL_POINT_:
              set_attr(vlist[k],AXIAL_POINT);
              tok = yylex();
              break;

           case ORIGINAL_:
              if ( (tok = gettok(INTEGER_)) != INTEGER_ )
              { kb_error(2104,"ORIGINAL number missing.\n",DATAFILE_ERROR);
                break;
              }
              set_original(vlist[k],(yylval.i-1)|((element_id)VERTEX<<TYPESHIFT)|VALIDMASK);
              tok = yylex();
              break;

           case FIXED_:
              set_attr(vlist[k],FIXED);
              tok = yylex();
              break;

           case HIT_PARTNER_:
              set_attr(vlist[k],HIT_PARTNER);
              tok = yylex();
              break;

           case METHOD_:  /* apply method instance to edge */
              tok = yylex();  /* get name */

           case METHOD_NAME_:
              qnum = yylval.i;
              tok = yylex();           
              if ( tok == '-' || tok == UMINUS_ )
              { apply_method_num(inverse_id(vlist[k]),qnum);
                tok = yylex();
              }
              else
                apply_method_num(vlist[k],qnum);
              break;

           case QUANTITY_NAME_: /* name of quantity */
              qnum = yylval.i;
              tok = yylex();
              if ( tok == '-' || tok == UMINUS_ )
              { apply_quantity(inverse_id(vlist[k]),qnum);
                tok = yylex();
              }
              else 
              apply_quantity(vlist[k],qnum);
              break;

           case IDENT_:  /* maybe method or quantity */
              { sprintf(errmsg,"Illegal use of identifier '%s'.\n",yytext);
                kb_error(1118,errmsg,DATAFILE_ERROR);
              }
              tok = yylex();
              break;


           case QUANTITY_:
              tok = yylex();
               if ( tok == IDENT_ )
                  { /* have named quantity pair */
                    char qname[32];
                    strncpy(qname,yytext,sizeof(qname));
                    if ( globals(yylval.i)->flags & QUANTITY_NAME )
                       apply_quantity(vlist[k],yylval.i);
                    else
                    { sprintf(errmsg,"Undefined quantity: %s.\n",yytext);
                      kb_error(1119,errmsg,DATAFILE_ERROR);
                    }
                    tok = yylex();
                    if ( stricmp(yytext,"method")==0 )
                       { tok = yylex();
                          kb_error(1120,"Obsolete quantity syntax.\n  Methods must be listed in quantity definition.",DATAFILE_ERROR);

                          tok = yylex();
                       }
                    continue;
                  }
               else 
               kb_error(1121,"Need quantity name and method.\n",DATAFILE_ERROR);
               break;

           case BOUNDARY_:
           case BOUNDARY_NAME_:
              {
                REAL *x,*param;
                int n;

                if ( tok == BOUNDARY_ )
                  tok = gettok(INTEGER_);
                if ( (tok != INTEGER_) && ( tok != BOUNDARY_NAME_ ) )  
                { kb_error(1122,"Need boundary number or name.\n",
                     DATAFILE_ERROR);
                  break;
                }
                if ( tok == INTEGER_ )
                { bnum = abs(yylval.i);
                  if ( (bnum >= web.bdrymax) 
                    ||  !(web.boundaries[bnum].attr & IN_USE) )
                   {
                      sprintf(errmsg,
                          "Bad boundary number %d for vertex %d.\n",bnum,k);
                      kb_error(1123,errmsg,DATAFILE_ERROR);
                      yylex();
                      break;
                   }          
                }
                else
                { bnum = globals(yylval.i)->value.bnum;
                }
                set_attr(vlist[k],BOUNDARY);
                if ( yylval.i < 0 )  set_attr(vlist[k],NEGBOUNDARY); 
                set_boundary_num(vlist[k],bnum);
                bdry = get_boundary(vlist[k]); 
                if ( pcount != bdry->pcount )
                  { sprintf(errmsg, "Wrong number of parameters for vertex %d.\n",k);
                    kb_error(1124,errmsg,DATAFILE_ERROR);
                  }          
                if ( (bdry->attr & CON_ENERGY) && (yytext[0] != '0') )
                   set_attr(vlist[k], BDRY_ENERGY);
                if ( bdry->attr & CON_CONTENT )
                   set_attr(vlist[k], BDRY_CONTENT);
                param = get_param(vlist[k]);
                x = get_coord(vlist[k]);
                for ( n = 0 ; n < web.maxparam ; n++ )
                      param[n] = x[n];
                /* initial coordinate calculation later, after all info */
                tok = yylex();
                    }
                break;

      case CONSTRAINT_:
      case CONSTRAINT_NAME_:
            if ( tok == CONSTRAINT_ )
              tok = gettok(INTEGER_);
            while ( (tok == INTEGER_) || (tok==CONSTRAINT_NAME_) )
              {
                 if ( tok == INTEGER_ )
                   cnum = abs(yylval.i);
                 else
                   cnum = globals(yylval.i)->value.cnum;
                 constr = get_constraint(cnum); 
                 if ( (cnum >= web.maxcon) || !(constr->attr & IN_USE) )
                    {
                      sprintf(errmsg,
                          "Bad constraint number %d for vertex %d.\n",cnum,k);
                      kb_error(1125,errmsg,DATAFILE_ERROR);
                      tok = yylex();
                      break;
                    }

                 set_attr(vlist[k],CONSTRAINT);
                 if ( yylval.i < 0 )  
                   set_attr(vlist[k],NEGBOUNDARY); 
                 set_v_constraint_map(vlist[k],cnum);

                 if ( (constr->attr & CON_ENERGY) && (yytext[0] != '0') )
                    set_attr(vlist[k], BDRY_ENERGY);
                 if ( constr->attr & CON_CONTENT )
                    set_attr(vlist[k], BDRY_CONTENT);
            
                 tok = gettok(INTEGER_);
              }
            /* projection later, after all vertex info read in */
            break;

            case EDGES_: case FACES_: case BODIES_: case READ_: 
            case LEAD_INTEGER_: case LEAD_INTEGER_AT_: case NO_TOKEN:
                more_attr = 0 ; break;  /* error recovery */
            case UNPUTTED_: 
                kb_error(3701,
                   "Internal error: forgot to get lookahead token.\n",
                       WARNING);
                tok = yylex();
                break;
            default: 
                sprintf(errmsg,"Unexpected token: %s\n",yytext);
                kb_error(2105,errmsg,WARNING);
                tok = yylex();
                break; 

          }

      if ((get_vattr(vlist[k])&(BOUNDARY|CONSTRAINT)) == (BOUNDARY|CONSTRAINT))
          kb_error(1126,"Cannot have constraint and boundary.",DATAFILE_ERROR);

      if ( !(get_vattr(vlist[k])&BOUNDARY) && (pcount != SDIM) )
         { sprintf(errmsg,"Wrong number of coordinates for vertex %d.\n",k);
            kb_error(1127,errmsg,WARNING);

          }
      if ( get_vattr(vlist[k]) & BOUNDARY )
      { REAL * x = get_coord(vlist[k]);
         int n;
         bdry = get_boundary(vlist[k]);
          for ( n = 0 ; n < SDIM ; n++ )
             if ( bdry->coordf[n]->root != NULL )
                 x[n] = eval(bdry->coordf[n],get_param(vlist[k]),vlist[k],NULL);
      }
      if ( get_vattr(vlist[k]) & CONSTRAINT )
         project_v_constr(vlist[k],ACTUAL_MOVE,RESET_ONESIDEDNESS);
    }
  web.zoom_v = vlist[zoom_number];  /* vertex for zooming in on */
}  /* end read_vertices() */


/************************************************************************
*
*  Function: read_edges()
*
*/

void read_edges()
{
  int i,k;
  element_id head,tail;
  int cnum,bnum;
  struct boundary *bdry;
  struct constraint *constr;
  int more_attr;
  REAL value;  /* for constant expression values */
  int compcount;  /* proper number of components for integrands */
  int numv; /* vertices to read in association with a facet */
  int edim = (web.representation==STRING) ? 1 : web.dimension - 1;
  int one = 1;
#ifdef MPI_EVOLVER
  struct element *vdummy = (struct element *)mycalloc(web.sizes[VERTEX],1);
#endif

  if ( web.representation == SIMPLEX )
     compcount = binom_coeff(SDIM,edim);
  else compcount = SDIM; 

  /* optional attributes */
  expand_attribute(EDGE,E_VERTICES_ATTR,&web.skel[EDGE].ctrlpts);
  if ( web.symmetry_flag )
     expand_attribute(EDGE,E_WRAP_ATTR,&one);

  if ( web.representation == SIMPLEX )
  { if ( web.modeltype == LAGRANGE )
        numv = binom_coeff(web.lagrange_order+edim,edim); 
     else numv = web.dimension;
  }
  else if ( web.modeltype == LAGRANGE )
  { numv = binom_coeff(web.lagrange_order+edim,edim);
  }
  else if ( web.modeltype == QUADRATIC ) numv = 3;
  else numv = 2;

  /* read in edges */
  emaxlist = MAXLIST;
  elist = (edge_id *)mycalloc(sizeof(edge_id),emaxlist);
  while ( (tok != EDGES_) && (tok != 0 ) ) 
     tok = yylex();
  if ( tok != EDGES_ ) return;
  tok = yylex();
  while ( (tok == LEAD_INTEGER_) || (tok == LEAD_INTEGER_AT_) )
  { int have_mid = 0;
    WRAPTYPE wrap = 0;

    #ifdef MPI_EVOLVER
    /* test task number, for MPI */
    int task;
    if ( tok == LEAD_INTEGER_ )
       task = 1;  /* default to task 1 */
    else task = yylval.qnum;
    if ( task >= mpi_nprocs || task < 1 )
    { sprintf(errmsg,"Task number %d must be between 1 and %d\n",
         task,mpi_nprocs-1);
      kb_error(5007,errmsg,RECOVERABLE);
    }
    if ( task != this_task )
    { /* skip this edge */
      int flag = 0;
      while ( !flag ) 
      { tok = yylex();
        switch (tok)
        { case LEAD_INTEGER_: 
          case LEAD_INTEGER_AT_:
          case FACES_:
          case BODIES_:
          case READ_:
          case 0:
            flag = 1;
            break;
        }
      }
      continue;  /* next edge */
    }
    #endif
    
    /* check edge number */
    k = yylval.i;
    if ( k < 1 ) 
        kb_error(2106,"Edge number must be positive.\n",DATAFILE_ERROR);
    while ( k >= emaxlist )
    { int spot = emaxlist; 
      elist = (edge_id *)kb_realloc((char *)elist,(k+MAXLIST)*sizeof(edge_id));
      emaxlist= k + MAXLIST;
      for ( ; spot < emaxlist ; spot ++ ) elist[spot] = NULLID;
    }
    if ( valid_id(elist[k]) )
    { sprintf(errmsg,"Duplicate edge number %d\n",k);
      kb_error(1130,errmsg,DATAFILE_ERROR);
    }

    { /* read vertex list */
          int vercount;
       vertex_id *v,*vv;

       move_to_free_front(EDGE,k); /* so id will be k */
       elist[k] = new_edge(NULLID,NULLID,NULLID);
       vv = v = get_edge_vertices(elist[k]);
       for ( vercount = 0 ; vercount < numv ; vercount++ )
       { tok = gettok(INTEGER_);
         if ( tok != INTEGER_ )
          { free_element(elist[k]);   
		    elist[k] = NULLID;
			if ( addload_flag && (vercount == 2) )
		    { for ( i = 0 ; i < vmaxlist ; i++ )
				if ( valid_element(vlist[i]) )
					free_element(vlist[i]);
			  if ( web.modeltype == QUADRATIC )
				kb_error(5437,"addload datafile is linear model, but current model is quadratic.\n",
				  RECOVERABLE);
			  else
				kb_error(5438,"addload datafile is linear model, but current model is Lagrange.\n",
				  RECOVERABLE);
		    } 
		    else
			kb_error(1131,"Too few vertices for edge.\n",DATAFILE_ERROR); 
            return; 
          }
#ifdef MPI_EVOLVER
          if ( yylval.qnum == 0 )
             yylval.qnum = 1;
          if ( yylval.qnum >= mpi_nprocs )
          { sprintf(errmsg,"Task number %d exceeds number of tasks running, %d\n",
               yylval.qnum,mpi_nprocs-1);
            kb_error(5008,errmsg,RECOVERABLE);
          }
          if ( yylval.qnum != this_task )
          { 
            *v = ((element_id)VERTEX << TYPESHIFT) | VALIDMASK | 
                (yylval.i-1) | ((element_id)yylval.qnum << TASK_ID_SHIFT);  
            /* add spaceholder to remote element list */
            memset(vdummy,0,web.sizes[VERTEX]);
            vdummy->self_id = *v;
            vdummy->attr = ALLOCATED|NEWELEMENT;
            mpi_add_remote_element(vdummy);
            v++;
          }
          else
#endif
          if ( (yylval.i >= vmaxlist) || !valid_id(vlist[yylval.i]) )
          { sprintf(errmsg,"Edge %d: vertex %d is not defined.\n",k,yylval.i);
            kb_error(1132,errmsg,DATAFILE_ERROR);
            return;
          }
          else *(v++) = vlist[yylval.i];
        }
       if ( web.modeltype == QUADRATIC )
       { head = vv[1]; tail = vv[0]; set_edge_midv(elist[k],vv[2]); }
       else { head = vv[numv-1]; tail = vv[0]; }
       set_edge_headv(elist[k],head);
       set_edge_tailv(elist[k],tail);
       if ( web.modeltype == QUADRATIC )
       { have_mid =1; /* for later */
       }
       else if ( web.modeltype == LAGRANGE )
       { for ( i = 1 ; i < numv-1 ; i++ )
          { set_attr(vv[i],Q_MIDEDGE); 
            set_vertex_edge(vv[i],elist[k]);
          } 
       }
    } 
    tok = yylex();
    if ( web.torus_flag )
    { read_wrap_flag = 1;
      for ( i = 0 ; i < SDIM  ; i++ )
      switch ( tok )
      { case WRAP_: 
            if ( read_const(&value) < 0 ) 
                kb_error(4135,"Missing wrap value.\n",DATAFILE_ERROR);
            else tok = yylex();
            wrap = (WRAPTYPE)value;
            i = SDIM;
            break;
        case '+':
            wrap += POSWRAP << (i*TWRAPBITS);
            tok = ' '; /* so won't expect more input */
            tok = yylex();
            break;

        case '*': 
            tok = ' '; /* so won't expect more input */
            tok = yylex(); 
           break;

        case '-':
        case UMINUS_:
            wrap += NEGWRAP << (i*TWRAPBITS);
            tok = ' '; /* so won't expect more input */
            tok = yylex();
            break;

        case POW: /* ** */
             i++;            
             tok = ' '; /* so won't expect more input */
             tok = yylex(); break;
        default :
          kb_error(1133,"Edge wraps must immediately follow endpoints.\n",
              WARNING);
          i = SDIM; /* assume wraps are missing */
          break;
      }
    }
    read_wrap_flag = 0;

    set_original(elist[k],(k-1)|((element_id)EDGE<<TYPESHIFT)|VALIDMASK);

    if ( web.representation == STRING )
       set_edge_density(elist[k],1.0);
    else
       set_edge_density(elist[k],0.0);
    /* check attributes */
    for ( more_attr = 1; more_attr ; )
       switch ( tok )
          {
               case EXTRA_ATTRIBUTE_:
               case ARRAY_ATTRIBUTE_:
                  read_extra(elist[k],yylval.qnum);
                  break;

               case ORIENTATION_:
                  tok = gettok(INTEGER_);
                  if ( tok != INTEGER_ )
                  { kb_error(2107,"ORIENTATION value missing.\n",DATAFILE_ERROR);
                    break;
                  }
                  if ( yylval.i < 0 ) set_attr(elist[k],NEGBOUNDARY);
                  tok = yylex();
                  break;

               case NONCONTENT_:
                  set_attr(elist[k],NONCONTENT);
                  tok = yylex();
                  break;

               case NO_REFINE_:
                  set_attr(elist[k],NO_REFINE);
                  tok = yylex();
                  break;

               case BARE_:
                  set_attr(elist[k],BARE_NAKED);
                  tok = yylex();
                  break;

               case ORIGINAL_:
                  if ( (tok = gettok(INTEGER_)) != INTEGER_ )
                  { kb_error(2108,"ORIGINAL number missing.\n",DATAFILE_ERROR);
                    break;
                  }
                  set_original(elist[k],(yylval.i-1)|((element_id)EDGE<<TYPESHIFT)|VALIDMASK);
                  tok = yylex();
                  break;

            case WRAP_:
                   if ( read_const(&value) < 0 ) 
                      kb_error(1135,"Missing wrap value.\n",DATAFILE_ERROR);
                   else tok = yylex();
                   wrap = (WRAPTYPE)value;
                   if ( !web.symmetry_flag )
                       kb_error(1134,"Cannot do wraps without torus or symmetry group.\n",
                          DATAFILE_ERROR);
                   else set_edge_wrap(elist[k],wrap);
                   break;

            case FIXED_:
                   set_attr(elist[k],FIXED);  
                   tok = yylex();
                   break;

            case EFIXED_: /* edge only, not vertices */ 
                   set_attr(elist[k],FIXED);  
                   tok = yylex();
                   break;

            case COLOR_:
                   if ( (tok = gettok(INTEGER_)) != INTEGER_ )
                   { kb_error(1136,"Color missing.\n",DATAFILE_ERROR);
                     break;
                   }
                   set_edge_color(elist[k],(short)yylval.i);
                   tok = yylex();
                   break;

            case BOUNDARY_:
            case BOUNDARY_NAME_:
                   if ( tok == BOUNDARY_ )
                     tok = gettok(INTEGER_);
                   if ( (tok != INTEGER_) && ( tok != BOUNDARY_NAME_ ) )  
                       { kb_error(2109,"Need boundary number or name.\n",
                           DATAFILE_ERROR);
                          break;
                       }
                   if ( tok == INTEGER_ )
                    { bnum = abs(yylval.i);
                      if ( (bnum >= web.bdrymax) 
                              || !(web.boundaries[bnum].attr & IN_USE) )
                       {
                          sprintf(errmsg,
                              "Bad boundary number %d for edge %d.\n",bnum,k);
                          kb_error(1137,errmsg,DATAFILE_ERROR);
                          yylex();
                          break;
                       }          
                    }
                    else
                    { bnum = globals(yylval.i)->value.bnum;
                    }
                   set_attr(elist[k],BOUNDARY);
                   if ( yylval.i < 0 )  set_attr(vlist[k],NEGBOUNDARY); 
                   set_edge_boundary_num(elist[k],bnum);
                   bdry = get_edge_boundary(elist[k]); 
                   if ( (bdry->attr & CON_ENERGY) && (yytext[0] == '0') )
                      set_attr(elist[k], BDRY_ENERGY);
                   if ( bdry->attr & CON_CONTENT )
                      set_attr(elist[k], BDRY_CONTENT);
                   tok = yylex();
                   break;

          case CONSTRAINT_:
          case CONSTRAINT_NAME_:
             if ( tok == CONSTRAINT_ )
               tok = gettok(INTEGER_);
             while ( (tok == INTEGER_) || (tok==CONSTRAINT_NAME_) )
             { struct constraint *con;
               if ( tok == INTEGER_ )
                 cnum = abs(yylval.i);
               else
                 cnum = globals(yylval.i)->value.cnum;
               con = get_constraint(cnum);
               if ( (cnum >= web.maxcon) || !(con->attr & IN_USE) )
               { sprintf(errmsg,
                        "Bad constraint number %d for edge %d.\n",cnum,k);
                 kb_error(1138,errmsg,DATAFILE_ERROR);
                 tok=yylex();
                 break;
               }
               /* check consistency of number of components */
               if ( (con->attr & CON_ENERGY) ||
                    ((con->attr & CON_CONTENT) && web.dimension==SOAPFILM) )
               { if ( web.dimension == 1 )
                 { sprintf(errmsg,"Edge %s is on constraint %s, which has an energy integral.  Probably a bad idea in the string model.\n",ELNAME(elist[k]),con->name);
                   kb_error(3918,errmsg,WARNING);
                 }
                 else if ( con->compcount != compcount )
                 { sprintf(errmsg,
"Inconsistent number of components in edge %s constraint %s content or energy integrands.\n",
                    ELNAME(elist[k]),con->name);
                   kb_error(1139,errmsg, WARNING);
                 }
               }
               set_attr(elist[k],CONSTRAINT);
               if ( yylval.i < 0 )  set_attr(elist[k],NEGBOUNDARY); 
               set_e_constraint_map(elist[k],cnum);
               constr = get_constraint(cnum); 
               if ( (constr->attr & CON_ENERGY) && (yytext[0] != '0') )
                  set_attr(elist[k], BDRY_ENERGY);
               if ( constr->attr & CON_CONTENT )
                  set_attr(elist[k], BDRY_CONTENT);
               tok = gettok(INTEGER_);
            }
          break;

          case DENSITY_:
               if ( read_const(&value) <= 0 )
                  kb_error(1140,"Missing DENSITY value.\n",WARNING);
               else tok = yylex();
               set_attr(elist[k],DENSITY);
               set_edge_density(elist[k],value);
               break;

        case QUANTITY_NAME_: /* name of quantity */ 
         { int qnum = yylval.i;
           tok = yylex();
           if ( tok == '-' || tok == UMINUS_ )
           { apply_quantity(inverse_id(elist[k]),qnum);
             tok = yylex();
           }
           else 
             apply_quantity(elist[k],qnum);
           break;
         }
          
        case IDENT_:  /* maybe method or quantity */
           if ( globals(yylval.i)->flags & METHOD_NAME )
              apply_method(elist[k],yytext);
           else if ( globals(yylval.i)->flags & QUANTITY_NAME )
              apply_quantity(elist[k],yylval.i);
           else
            { sprintf(errmsg,"Illegal use of identifier '%s'.\n",yytext);
              kb_error(1141,yytext,DATAFILE_ERROR);
            }
           tok = yylex();
           break;

        case METHOD_:  /* apply method instance to edge */
           tok = yylex();
        case METHOD_NAME_:
        { int qnum = yylval.i;      
           tok = yylex();
           if ( tok == UMINUS_  || tok == '-' )
           { apply_method_num(inverse_id(elist[k]),qnum);
             tok = yylex();
           }
           else apply_method_num(elist[k],qnum);
           break;
        }

        case QUANTITY_:
        /* see if quantity */
        tok = yylex();
        if ( tok == QUANTITY_NAME_ )
        { /* have named quantity/method pair */
          char qname[32];
          strncpy(qname,yytext,sizeof(qname));
          apply_quantity(elist[k],yylval.i);
          
          tok = yylex();
          if ( stricmp(yytext,"method")==0 )
          { tok = yylex();
            kb_error(1143,"Obsolete quantity syntax.\n  Methods must be listed in quantity definition.",DATAFILE_ERROR);
            tok = yylex();
          }
          continue;
        }
        else 
        { sprintf(errmsg,"Undefined quantity: %s\n",yytext);
          kb_error(1144,errmsg,DATAFILE_ERROR);
        }
        break;

        case ENERGY_:
        /* obsolete surface energy */
           kb_error(1147,
  "'Energy' obsolete. Implement edge integral energy with named quantity.\n",
            DATAFILE_ERROR);
           break;

          case FACES_: case BODIES_: case READ_: case LEAD_INTEGER_: 
          case LEAD_INTEGER_AT_:
          case NO_TOKEN:    more_attr = 0 ; break;  /* error recovery */
          case UNPUTTED_: 
                kb_error(3702,
                   "Internal error: forgot to get lookahead token.\n",
                       WARNING);
                tok = yylex();
                break;
          default: 
              sprintf(errmsg,"Unexpected token: %s\n",yytext);
              kb_error(2110,errmsg,WARNING);
              tok = yylex();
              break;

       }

    if ( web.symmetry_flag )
    {  set_edge_wrap(elist[k],wrap);
       if ( (web.modeltype == QUADRATIC) && !have_mid )
       { /* have to adjust midpoints */
         REAL *x = get_coord(get_edge_midv(elist[k]));
         REAL *t = get_coord(get_edge_tailv(elist[k]));
         REAL h[MAXCOORD];
         (*sym_wrap)(get_coord(get_edge_headv(elist[k])),h,wrap);
         for ( i = 0 ; i < SDIM ; i++ )
            x[i] = 0.5*(t[i] + h[i]);
       }
    }
    if ((get_eattr(elist[k])&(BOUNDARY|CONSTRAINT)) == (BOUNDARY|CONSTRAINT))
        kb_error(1148,"Cannot have constraint and boundary.",DATAFILE_ERROR);
   }

#ifdef MPI_EVOLVER
  myfree((char*)vdummy);
#endif

} /* end read_edges() */

/************************************************************************
*
*  Function: read_faces()
*
*  Purpose: Read face definition lines from datafile and construct
*           facet-edge links.
*
*           MPI note: In SOAPFILM model, facetedges go to same task
*           as facets, but go to same task as edges in STRING model.
*/

void read_faces()
{
  int i,k;
  int e;
  facetedge_id old_fe;
  facet_id this_facet_id;
  int cnum,bnum;
  struct constraint *constr;
  REAL value;  /* for constant expression values */
  int numv; /* vertices to read with facet */
#ifdef MPI_EVOLVER
  struct element *vdummy = (struct element *)mycalloc(web.sizes[VERTEX],1);
  struct element *edummy = (struct element *)mycalloc(web.sizes[EDGE],1);
  int facet_task,edge_task;
  int split_loop_flag = 0; /* string, set if task number of any edge is foreign */
#endif

  /* optional attributes */
  expand_attribute(FACET,F_VERTICES_ATTR,&web.skel[FACET].ctrlpts);

  if ( web.representation == SIMPLEX )
  { if ( web.modeltype == LAGRANGE )
        numv = binom_coeff(web.lagrange_order+web.dimension,web.dimension); 
    else numv = web.dimension;
  }
  else if ( web.modeltype == LAGRANGE )
  { numv = binom_coeff(web.lagrange_order+web.dimension,web.dimension);
  }
  else if ( web.modeltype == QUADRATIC ) numv = 0;
  else numv = 0;

  /* read in faces */
  fmaxlist = MAXLIST;
  flist = (facet_id *)mycalloc(sizeof(facet_id),fmaxlist);

  tok = yylex();
  while ( (tok == LEAD_INTEGER_) || (tok == LEAD_INTEGER_AT_) )
  { int more_attr;
    int edge_count = 0;


    #ifdef MPI_EVOLVER
    /* test task number, for MPI */
    if ( tok == LEAD_INTEGER_ )
       facet_task = 1;  /* default to task 1 */
    else facet_task = yylval.qnum;
    if ( facet_task >= mpi_nprocs || facet_task < 1 )
    { sprintf(errmsg,"Task number %d must be between 1 and %d\n",
         facet_task,mpi_nprocs-1);
      kb_error(5009,errmsg,RECOVERABLE);
    }
    if ( facet_task != this_task )
    { /* skip this facet */
      int flag = 0;
      while ( !flag ) 
      { tok = yylex();
        switch (tok)
        { case LEAD_INTEGER_: 
          case LEAD_INTEGER_AT_:
          case BODIES_:
          case READ_:
          case 0:
            flag = 1;
            break;
        }
      }
      continue;  /* next facet */
    }
    #endif
    
    k = yylval.i;
    if ( k < 1 ) 
        kb_error(2111,"Face number must be positive.\n",DATAFILE_ERROR);
    old_fe = NULLFACETEDGE;

    {
      while ( k >= fmaxlist )
      { int spot = fmaxlist;
        flist = (facet_id *)kb_realloc((char *)flist,
                              (k+MAXLIST)*sizeof(facet_id));
        fmaxlist = k + MAXLIST;
        for ( ; spot < fmaxlist ; spot++ ) flist[spot] = NULLID;
      }
      if ( valid_id(flist[k]) )
      { sprintf(errmsg,"Duplicate face number %d\n",k);
        kb_error(1152,errmsg,DATAFILE_ERROR);
      }
  
      move_to_free_front(FACET,k); /* so id will be k */
      flist[k] = new_facet();
      set_original(flist[k],(k-1)|((element_id)FACET<<TYPESHIFT)|VALIDMASK);
      this_facet_id = flist[k];
    }


    if ( web.representation == SIMPLEX )
    { /* read vertex list */
      int vercount = 0;
      vertex_id *v = get_facet_vertices(this_facet_id);
      while ( (tok = gettok(INTEGER_)) == INTEGER_ )
      { if ( vercount++ > numv )
          kb_error(1153,"Too many vertices for facet.\n",DATAFILE_ERROR);
#ifdef MPI_EVOLVER
        if ( yylval.qnum == 0 )
             yylval.qnum = 1;
        if ( yylval.qnum >= mpi_nprocs )
        { sprintf(errmsg,"Task number %d exceeds number of tasks running, %d\n",
             yylval.qnum,mpi_nprocs-1);
          kb_error(5010,errmsg,RECOVERABLE);
        }
        if ( yylval.qnum != this_task )
        { *v = ((element_id)VERTEX << TYPESHIFT) | VALIDMASK | 
             (yylval.i-1) | ((element_id)yylval.qnum << TASK_ID_SHIFT);  
          memset(vdummy,0,web.sizes[VERTEX]);
          vdummy->self_id = *v;
          vdummy->attr = ALLOCATED|NEWELEMENT;
          mpi_add_remote_element(vdummy);
          v++;
        }
        else
#endif
        if ( (yylval.i >= vmaxlist) || !valid_id(vlist[yylval.i]) )
        { sprintf(errmsg,"Facet %d: vertex %d is not defined.\n",k,yylval.i);
          kb_error(1154,errmsg,DATAFILE_ERROR);
        }
        else 
          *(v++) = vlist[yylval.i];
      }
      if ( vercount < numv )
        kb_error(1155,"Too few vertices for facet.\n",DATAFILE_ERROR);
    }
    else  /* facet_edge representation */
    { facetedge_id fe = NULLID;
      facetedge_id first_fe = NULLID;

      while ( (tok = gettok(INTEGER_)) == INTEGER_ )
      { edge_id e_id;
        facetedge_id edge_fe;
            
        edge_count++;
        e = yylval.i;

#ifdef MPI_EVOLVER
        edge_task = yylval.qnum;
        if ( edge_task == 0 )  /* for non-partitioned datafiles */
             edge_task = 1;
        if ( edge_task >= mpi_nprocs )
        { sprintf(errmsg,"Edge task number %d exceeds number of tasks running, %d\n",
             edge_task,mpi_nprocs-1);
          kb_error(5011,errmsg,RECOVERABLE);
        }
        if ( edge_task != this_task )
        { if (web.representation==SOAPFILM ) 
          {
          e_id = ((element_id)EDGE << TYPESHIFT) | VALIDMASK | (abs(e)-1) |
             ((element_id)edge_task << TASK_ID_SHIFT);  
          memset(edummy,0,web.sizes[EDGE]);
          edummy->self_id = e_id;
          edummy->attr = ALLOCATED | NEWELEMENT;
          mpi_add_remote_element(edummy);
          if ( e < 0 ) invert(e_id);
          }
          else /* string */
          { 
            split_loop_flag = 1; /* so don't try to close loop at end */
            continue; /* only doing local edges */
          }
        }
        else
#endif
        { if ( abs(e) >= emaxlist )
          { sprintf(errmsg,"Facet %d: edge %d is not defined.\n",k,abs(e));
            e = 0;
            kb_error(1156,errmsg,DATAFILE_ERROR);
            continue;
          }
          e_id =  e > 0 ? elist[e] : edge_inverse(elist[-e]);
        }

        if ( !valid_id(e_id) )
        { sprintf(errmsg,"Facet %d: edge %d is not defined.\n",k,e);
          kb_error(1157,errmsg,DATAFILE_ERROR);
          continue;
        }

        fe = new_facetedge(this_facet_id,e_id);
        if ( valid_id(old_fe) )
        { 
          #ifndef MPI_EVOLVER
		  vertex_id hh,tt;
          hh = get_fe_headv(old_fe);
          tt = get_fe_tailv(fe);
          if ( !equal_id(hh,tt) ) 
          { sprintf(msg,"Inconsistency in face %d, edge %d tail vertex disagrees with previous head.\n",
                         k,e);
            kb_error(2112,msg,DATAFILE_ERROR);
          }
          #endif
          set_next_edge(old_fe,fe);
        }
		else 
			first_fe = fe;
        set_prev_edge(fe,old_fe);
        old_fe = fe;

#ifdef MPI_EVOLVER
       if ( (web.representation == SOAPFILM) || (facet_task == this_task) )
#endif
         if ( !valid_id(get_facet_fe(this_facet_id)) )
           set_facet_fe(this_facet_id,fe);

        /* add to edge facet list, not in geometric order */
        edge_fe = get_edge_fe(e_id);
        if ( valid_id(edge_fe) )
        { /* insert in chain */
          set_next_facet(fe,get_next_facet(edge_fe));
          set_prev_facet(fe,edge_fe);
          set_prev_facet(get_next_facet(fe),fe);
          set_next_facet(edge_fe,fe);
        }
        else
        { set_next_facet(fe,fe);
          set_prev_facet(fe,fe);
          set_edge_fe(e_id,fe);      /* link edge to rest of world */
        }
      }

      if ( ((web.representation == STRING) && (edge_count < 1))
              || ((web.representation == SOAPFILM) && (edge_count < 3)) )
      { sprintf(errmsg,"Face %d has too few edges.\n",k);
        kb_error(1158,errmsg,DATAFILE_ERROR);
      }
      #ifdef MPI_EVOLVER
      if ( !split_loop_flag )
      { set_next_edge(fe,first_fe);  /* close up ring */
        set_prev_edge(first_fe,fe);
      }
      #else
	  { vertex_id hh,tt;
      tt = get_fe_tailv(first_fe);
      hh = get_fe_headv(fe);
      if ( equal_id(tt,hh) ) 
      { set_next_edge(fe,first_fe);  /* close up ring */
        set_prev_edge(first_fe,fe);
      }
      else 
      { if ( web.representation != STRING )
        { sprintf(errmsg,
             "Inconsistency in face %d first and last edges.\n",k);
          kb_error(1159,errmsg,DATAFILE_ERROR);
        }
      }
	  }
      #endif
      
      if ( (web.modeltype == LAGRANGE) && (web.representation == SOAPFILM) )
      { /* read vertex list */
        int vercount = 0;
        vertex_id *v;
        if ( tok != VERTICES_ )
          kb_error(1160,"Need facet vertices in Lagrange model.\n",RECOVERABLE);
        v = get_facet_vertices(this_facet_id);
        while ( (tok = gettok(INTEGER_)) == INTEGER_ )
        { if ( vercount++ >= numv )
             kb_error(1161,"Too many vertices for facet.\n",DATAFILE_ERROR);
          else if ( !valid_id(vlist[yylval.i]) )
          { sprintf(errmsg,"Facet %d: vertex %d is not defined.\n",k,yylval.i);
            kb_error(1162,errmsg,DATAFILE_ERROR);
          }
          else *(v++) = vlist[yylval.i];
        }
        if ( vercount < numv )
           kb_error(1163,"Too few vertices for facet.\n",DATAFILE_ERROR);
      }
    } 
    if ( (web.modeltype == LAGRANGE) && (web.modeltype == SOAPFILM) )
        { vertex_id *v = get_facet_vertices(this_facet_id);
          for ( i = 0 ; i < numv ; i++ ) 
             if ( !(get_vattr(v[i]) & Q_MIDEDGE ) )
             { set_attr(v[i],Q_MIDFACET);
               set_vertex_facet(v[i],this_facet_id);
             }
          for ( i = 0 ; i <= web.dimension ; i++ )
             unset_attr(v[web.skel[FACET].extreme[i]],Q_MIDFACET);
        }

#ifdef MPI_EVOLVER
	  if ( (web.representation == STRING) && (facet_task != this_task)) 
	  { /* skip rest of line */
        while ( (tok != LEAD_INTEGER_) && (tok != LEAD_INTEGER_AT_) &&
               (tok != READ_) && (tok != BODIES_) && (tok != 0) )
		{ tok = yylex();
        }
		continue;
	  }
#endif

      set_facet_density(this_facet_id,1.0);
      /* have attributes, maybe */
      for ( more_attr = 1 ; more_attr ; )
        switch ( tok )
         { 
            case EXTRA_ATTRIBUTE_:
            case ARRAY_ATTRIBUTE_:
                    read_extra(this_facet_id,yylval.qnum);
                    break;

            case ORIENTATION_:
                    if ( (tok = gettok(INTEGER_)) != INTEGER_ )
                    { kb_error(2113,"ORIENTATION value missing.\n",DATAFILE_ERROR);
                      break;
                    }
                    if ( yylval.i < 0 ) set_attr(this_facet_id,NEGBOUNDARY);
                    tok = yylex();
                    break;


            case ORIGINAL_:
                    if ( (tok = gettok(INTEGER_)) != INTEGER_ )
                    { kb_error(2114,"ORIGINAL value missing.\n",DATAFILE_ERROR);
                      break;
                    }
                    else 
                    { set_original(this_facet_id,(yylval.i-1)|((element_id)FACET<<TYPESHIFT)|VALIDMASK);
                      tok = yylex();
                    }
                    break;

            case DENSITY_:
                 if ( read_const(&value) <= 0 )
                    kb_error(1164,"Missing DENSITY or TENSION value.\n",WARNING);
                 else tok = yylex();
                 set_attr(this_facet_id,DENSITY);
                 set_facet_density(this_facet_id,value);
                 break;

            case NODISPLAY_:
          /* see if want not to be displayed */
                 set_attr(this_facet_id,NODISPLAY);
                 tok = yylex();
                 break;

             case NONCONTENT_:
                    set_attr(this_facet_id,NONCONTENT);
                    tok = yylex();
                    break;

             case NO_REFINE_:
                    set_attr(this_facet_id,NO_REFINE);
                    tok = yylex();
                    break;

          case FIXED_:
          /* see if fixed in place */
                 set_attr(this_facet_id,FIXED);
                 tok = yylex();
                 break;

          case PHASE_: 
                if ( !phase_flag )
                  kb_error(1165,"Phases not in effect.\n",DATAFILE_ERROR);

                if ( web.representation != STRING )
                  kb_error(1166,"Phases on facets only in STRING model.\n",DATAFILE_ERROR);

                if ( (tok = gettok(INTEGER_)) != INTEGER_ )
                { kb_error(1167,"Phase missing.\n",DATAFILE_ERROR);
                  break;
                }
                if ( (yylval.i < 0) || (yylval.i > phasemax) )
                  kb_error(1168,"Illegal phase value.\n",DATAFILE_ERROR);

                set_f_phase(this_facet_id,yylval.i);
                tok = yylex();
                break;

          case COLOR_:
                if ( (tok = gettok(INTEGER_)) != INTEGER_ )
                { kb_error(1169,"Color missing.\n",DATAFILE_ERROR);
                  break;
                }
                set_facet_color(this_facet_id,(short)yylval.i);
                tok = yylex();
                break;

          case FRONTCOLOR_:
                if ( (tok = gettok(INTEGER_)) != INTEGER_ )
                { kb_error(1170,"Frontcolor missing.\n",DATAFILE_ERROR);
                  break;
                }
                set_facet_frontcolor(this_facet_id,(short)yylval.i);
                tok = yylex();
                break;

          case BACKCOLOR_:
                if ( (tok = gettok(INTEGER_)) != INTEGER_ )
                { kb_error(1171,"Backcolor missing.\n",DATAFILE_ERROR);
                  break;
                }
                set_facet_backcolor(this_facet_id,(short)yylval.i);
                tok = yylex();
                break;

          case TAG_:
          /* optional inheritable tag */
              { int one = 1;
                F_TAG_ATTR = add_attribute(FACET,"tag",INTEGER_TYPE,0,&one,0,
                     NULL);
                if ( (tok = gettok(INTEGER_)) != INTEGER_ )
                { kb_error(1172,"Tag missing.\n",DATAFILE_ERROR);
                  break;
                }
                set_tag(this_facet_id,(tagtype)yylval.i);
                tok = yylex();
              }
              break;

          case BOUNDARY_:
          case BOUNDARY_NAME_:
          /* see if boundary facet */
                   if ( tok == BOUNDARY_ )
                     tok = gettok(INTEGER_);
                   if ( (tok != INTEGER_) && ( tok != BOUNDARY_NAME_ ) )  
                   { kb_error(1173,"Need boundary number or name.\n",
                            DATAFILE_ERROR);
                     break;
                   }
                   if ( tok == INTEGER_ )
                   { bnum = abs(yylval.i);
                     if ( (bnum >= web.bdrymax) 
                                || !(web.boundaries[bnum].attr & IN_USE) )
                     {
                       sprintf(errmsg,
                           "Bad boundary number %d for facet %d.\n",bnum,k);
                       kb_error(1174,errmsg,DATAFILE_ERROR);
                       yylex();
                       break;
                     }          
                   }
                   else
                   { bnum = globals(yylval.i)->value.bnum;
                   }
                set_attr(this_facet_id,BOUNDARY);
                set_facet_boundary_num(this_facet_id,bnum);
                tok = yylex();
                break;

          case CONSTRAINT_:
          case CONSTRAINT_NAME_:
              if ( tok == CONSTRAINT_ )
                tok = gettok(INTEGER_);
              while ( (tok == INTEGER_) || (tok==CONSTRAINT_NAME_) )
              {
                 if ( tok == INTEGER_ )
                     cnum = abs(yylval.i);
                 else
                     cnum = globals(yylval.i)->value.cnum;
                 constr = get_constraint(cnum); 
                 if ( (cnum >= web.maxcon) || !(constr->attr & IN_USE) )
                 { sprintf(errmsg,
                          "Bad constraint number %d for face %d.\n",cnum,k);
                   kb_error(1175,errmsg,DATAFILE_ERROR);
                   tok = yylex();
                   break;
                 }
                 set_attr(this_facet_id,CONSTRAINT);
/* ?? */         if ( yylval.i < 0 ) set_attr(this_facet_id,NEGBOUNDARY);
                 set_f_constraint_map(this_facet_id,cnum);
                 if ( constr->attr & CON_ENERGY )
                 set_attr(this_facet_id, BDRY_ENERGY);

                 tok = gettok(INTEGER_);
               }
                break;

          case ENERGY_:
          /* see if surface energy */
                tok = yylex();
                kb_error(1177,"Surface energies obsolete.\n",DATAFILE_ERROR);
                if ( tok == INTEGER_ ) tok = yylex();
                break;

          case QUANTITY_NAME_: /* name of quantity */
           { int qnum = yylval.i;
             tok = yylex();
             if ( tok == '-' || tok == UMINUS_ )
             { apply_quantity(inverse_id(this_facet_id),qnum);
               tok = yylex();
             }
             else
               apply_quantity(this_facet_id,qnum);
             break;
           }

          case IDENT_:  /* maybe method or quantity */
             if ( globals(yylval.i)->flags & METHOD_NAME )
                apply_method(this_facet_id,yytext);
             else if ( globals(yylval.i)->flags & QUANTITY_NAME )
                apply_quantity(this_facet_id,yylval.i);
             else 
             { sprintf(errmsg,"Illegal use of identifier '%s'.\n",yytext);
               kb_error(1178,errmsg,DATAFILE_ERROR);
             }
             tok = yylex();
             break;

          case METHOD_:  /* apply method instance to edge */
             tok = yylex(); /* fall through */
          case METHOD_NAME_:
          { char name[100];
             strcpy(name,yytext);
             tok = yylex();
             if ( (tok == UMINUS_) || (tok == '-') )
             { apply_method(inverse_id(this_facet_id),name);
               tok = yylex();
             }
             else apply_method(this_facet_id,name);
             break;
          }

          case QUANTITY_:            
                tok = yylex();
                if ( tok == IDENT_ )
                { /* have named quantity/method pair */
                  char qname[32];
                  strncpy(qname,yytext,sizeof(qname));
                  tok = yylex();
                  if ( globals(yylval.i)->flags & QUANTITY_NAME )
                  { if ( tok == '-' || tok == UMINUS_ )
                    { apply_quantity(inverse_id(this_facet_id),yylval.i);
                      tok = yylex();
                    }
                    else
                      apply_quantity(this_facet_id,yylval.i);
                  }
                  else 
                  { sprintf(errmsg,"Undefined quantity '%s'.\n",yytext);
                    kb_error(1179,errmsg,DATAFILE_ERROR);
                  }
                
                  if ( stricmp(yytext,"method")==0 )
                  { tok = yylex();
                    kb_error(1180,"Obsolete quantity syntax.\n  Methods must be listed in quantity definition.",DATAFILE_ERROR);
                    tok = yylex();
                  }
                  continue;
                }
                break;

            case BODIES_: case READ_: case LEAD_INTEGER_: case NO_TOKEN:
            case LEAD_INTEGER_AT_:
                more_attr = 0 ; break;  /* error recovery */
                
            default: 
                sprintf(errmsg,"Unexpected token: %s\n",yytext);
                kb_error(2115,errmsg,WARNING);
                tok = yylex();
                break;
         }

      if ((get_fattr(this_facet_id)&(BOUNDARY|CONSTRAINT)) == (BOUNDARY|CONSTRAINT))
          kb_error(1182,"Cannot have constraint and boundary.",DATAFILE_ERROR);


      if ( k > facecount ) facecount = k;
    }

#ifdef MPI_EVOLVER
  myfree((char*)vdummy);
  myfree((char*)edummy);
#endif

} /* end read_faces() */


/************************************************************************
*
*  Function: read_bodies()
*
*/

void read_bodies()
{
  int k;
  int f=0;
  facet_id f_id=NULLID;
  int more_attr;
  REAL value;  /* for constant expression values */
  int lagmulflag = 0;
  body_id b_id = NULLID;

  /* read in bodies */
  bmaxlist = MAXLIST;
  blist = (body_id *)mycalloc(sizeof(body_id),bmaxlist);

  tok = yylex();
  while ( (tok == LEAD_INTEGER_) || (tok == LEAD_INTEGER_AT_) )  /* body loop */
  { 
    REAL den,vol;
    int face_count = 0;
 

    #ifdef MPI_EVOLVER
    int facet_task,body_task;

    body_task = yylval.qnum;
  
    /* test task number, for MPI */
    if ( tok == LEAD_INTEGER_ )
       body_task = 1;  /* default to task 1 */
    else body_task = yylval.qnum;
    if ( body_task >= mpi_nprocs || body_task < 1 )
    { sprintf(errmsg,"Task number %d must be between 1 and %d\n",
         body_task,mpi_nprocs-1);
      kb_error(5029,errmsg,RECOVERABLE);
    }

    #endif 


    
    k = yylval.i;
    if ( k < 1 ) 
       kb_error(2116,"Body number must be positive.\n",DATAFILE_ERROR);
    while ( k >= bmaxlist )
    { int spot = bmaxlist; 
      blist = (body_id *)kb_realloc((char *)blist,(k+MAXLIST)*sizeof(body_id));
      bmaxlist = k + MAXLIST;
      for ( ; spot < bmaxlist ; spot ++ ) blist[spot] = NULLID;
    }
#ifdef MPI_EVOLVER
    if ( !mpi_local_bodies_flag || (body_task == this_task) )
#endif
    {
      if ( valid_id(blist[k]) )
      { sprintf(errmsg,"Duplicate body number %d\n",k);
        kb_error(1187,errmsg,DATAFILE_ERROR);
      }
      move_to_free_front(BODY,k); /* so id will be k */
      blist[k] = new_body();
      set_original(blist[k],(k-1)|((element_id)BODY<<TYPESHIFT)|VALIDMASK);
      b_id = blist[k];
    }
#ifdef MPI_EVOLVER
    else
      b_id =  (k-1)|((element_id)BODY<<TYPESHIFT)|VALIDMASK |
                   (((element_id)body_task) << TASK_ID_SHIFT);
#endif

    f_id = NULLID;  /* in case no facets */
    while ( (tok = gettok(INTEGER_)) == INTEGER_ )
    { 
     
      f = yylval.i;
#ifdef MPI_EVOLVER
      facet_task = yylval.qnum;
      if ( facet_task == 0 )
             facet_task = 1;
      if ( facet_task >= mpi_nprocs )
      { sprintf(errmsg,"Task number %d exceeds number of tasks running, %d\n",
           facet_task,mpi_nprocs-1);
        kb_error(5012,errmsg,RECOVERABLE);
      }
      if ( facet_task != this_task )
        continue;
      else
#endif
      { 
        face_count++;
        if ( abs(f) >= fmaxlist )
        { sprintf(errmsg,"Body %d: face %d is not defined.\n",k,f);
          kb_error(1188,errmsg,DATAFILE_ERROR);
        }
        f_id = f > 0 ? flist[f] : facet_inverse(flist[-f]);
        if ( !valid_id(f_id) )
        { sprintf(errmsg,"Body %d: face %d is not defined.\n",k,f);
          kb_error(1189,errmsg,DATAFILE_ERROR);
        }
        set_facet_body(f_id, b_id);
      }
    } 
    #ifndef MPI_EVOLVER 
    if ( (web.representation != STRING) && (face_count < 1) )
    { sprintf(errmsg,"Body %d has no faces.\n",k);
      kb_error(1190,errmsg,WARNING);
    }
    #endif

#ifdef MPI_EVOLVER
    if ( mpi_local_bodies_flag && (body_task != this_task ) )
    {  /* skip until next body */
        while ( (tok != LEAD_INTEGER_) && (tok != LEAD_INTEGER_AT_) &&
               (tok != READ_) && (tok != 0) )
		{ tok = yylex();
        }
       continue;
    }
#endif

    more_attr = 1;
    while ( more_attr )
      switch ( tok )
      {
        case EXTRA_ATTRIBUTE_:
        case ARRAY_ATTRIBUTE_:
              read_extra(blist[k],yylval.qnum);
              break;

        case ORIGINAL_:
              if ( (tok = gettok(INTEGER_)) != INTEGER_ )
              { kb_error(2117,"ORIGINAL value missing.\n",DATAFILE_ERROR);
                break;
              }
              set_original(blist[k],(yylval.i-1)|((element_id)BODY<<TYPESHIFT)|VALIDMASK);
              tok = yylex();
              break;

        case VOLUME_:
              /* have a fixed volume constraint */
              if ( read_const(&vol) <= 0 )
                kb_error(1191,"Missing VOLUME value.\n",DATAFILE_ERROR);
              else 
              { set_attr(blist[k],FIXEDVOL);
                set_body_fixvol(blist[k],vol);
                tok = yylex();
              }
              break;

        case ACTUAL_VOLUME_: 
              /* have a declared volume */
              if ( read_const(&vol) <= 0 )
                kb_error(1075,"Missing ACTUAL_VOLUME value.\n",DATAFILE_ERROR);
              else 
              { set_attr(blist[k],ACTUALVOL);
                set_body_actualvolume(blist[k],vol);
                tok = yylex();
              }
              break;


        case VOLCONST_:
              /* have a body volume adjustment */
              if ( read_const(&vol) <= 0 )
                kb_error(1192,"Missing VOLCONST value.\n",WARNING);
              else tok = yylex();
              set_body_volconst(blist[k],vol); 
              break;

        case DENSITY_:
              /* have density for gravity */
              if ( read_const(&den) <= 0 )
                kb_error(1193,"Missing DENSITY value.\n",WARNING);
              else tok = yylex();
              web.gravflag = 1;
              set_body_density(blist[k],den);
              set_attr(blist[k],DENSITY);
              break;

        case PRESSURE_:
              /* have prescribed pressure */
              web.pressflag = 1;
              set_attr(blist[k],PRESSURE);
              if ( read_const(&value) <= 0 )
                kb_error(1194,"Missing PRESSURE value.\n",WARNING);
              else 
              { set_body_pressure(blist[k],value);
                tok = yylex();
              }
              break;

        case LAGRANGE_MULTIPLIER_:
              if ( read_const(&value) <= 0 )
                kb_error(2118,"Missing lagrange_multiplier value.\n",DATAFILE_ERROR);
              else 
              { set_body_pressure(blist[k],value); lagmulflag = 1; 
                tok = yylex();
              }
              break;
              
        case PHASE_: 
              if ( !phase_flag )
                kb_error(1195,"Phases not in effect.\n",DATAFILE_ERROR);

              if ( web.representation == STRING )
                kb_error(1196,"Phases must be on facets in STRING model.\n",

                   DATAFILE_ERROR);
              if ( (tok = gettok(INTEGER_)) != INTEGER_ )
               { kb_error(1197,"Phase missing.\n",DATAFILE_ERROR);
                  break;
               }
              if ( (yylval.i < 0) || (yylval.i > phasemax) )
                kb_error(1198,"Illegal phase value.\n",DATAFILE_ERROR);

              else set_b_phase(blist[k],yylval.i);
              tok = yylex();
              break;

           case METHOD_:  /* apply method instance to edge */
              tok = yylex();
           case METHOD_NAME_:
              apply_method(blist[k],yytext);
              tok = yylex();
              break;

           case IDENT_:  /* maybe method or quantity */
              if ( globals(yylval.i)->flags & METHOD_NAME )
                apply_method(blist[k],yytext);
              else if ( globals(yylval.i)->flags & QUANTITY_NAME )
                apply_quantity(blist[k],yylval.i);
              else 
              { sprintf(errmsg,"Illegal use of identifier: %s.\n",yytext);
                kb_error(1199,errmsg,DATAFILE_ERROR);
              }
              tok = yylex();
              break;

		   case READ_: case LEAD_INTEGER_: case NO_TOKEN: case LEAD_INTEGER_AT_:
                more_attr = 0 ; break;  /* error recovery */
        case UNPUTTED_: 
                kb_error(3703,
                   "Internal error: forgot to get lookahead token.\n",
                       WARNING);
                tok = yylex();
                break;
      default: 
          sprintf(errmsg,"Unexpected token: %s\n",yytext);
          kb_error(2119,errmsg,WARNING);
          tok = yylex();
          break;

      }

    /* can't have both pressure and volume */
    if ((get_battr(blist[k]) & (FIXEDVOL|PRESSURE)) == (FIXEDVOL|PRESSURE))
        kb_error(1203,"Body can't have fixed volume and fixed pressure.\n",DATAFILE_ERROR);
  } /* end body loop */

  if ( web.bodycount > 0 )
      web.projection_flag = 1;

  if ( !lagmulflag ) pressure_set_flag = 0;

  if ( web.pressure_flag)
  {
    if ( !web.full_flag && !valid_id(web.outside_body) )
      add_outside();
  }

} /* end read_bodies() */

