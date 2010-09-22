%{
/********************************************************************
*        
*  File:  datafile.lex, lexyy.c  
*
*  Contents: lexical analyzer for evolver data files.
*            Constructed from datafile.lex by lex.
*/

/* will have my own input() and output() */

#ifdef FLEX_SCANNER
#define YY_INPUT(buf,result,max_size) \
     { \
       int c = kb_input(); \
       result = (c == 0) ? YY_NULL : (buf[0] = c, 1); \
     }
#else
/* lex scanner */
#undef input
#undef unput
#define input kb_input
#define unput rawunput
#endif

#include "include.h"
#include "lex.h"
#include "ytab.h"

static int previous_char = 0;  /* for CR conversion */
static int BUFFSIZE = 0; /* size of lex buffer */
static char *buff = NULL; /* lex buffer, expandable */
static int  spot = 0;
#define ERRBUFFSIZE 80
static char errbuff[ERRBUFFSIZE+4];  /* for reporting spot of error */
static int  errspot;
extern int ubuff_spot;

struct ckey { char *name; int token; } const_expr_keywords[] =
{
  {"pi",PI_},
  {"e",E_},
  {"g",G_}
};

void savein ARGS((int));
void get_more_input ARGS((void));
int keyword_compare ARGS(( struct ckey *, struct ckey *));

/* Wrapper for yylex() to buffer look-ahead token */
#define UNPUTMAX 10
int unputted_tok[UNPUTMAX];  /* unput token number   */
int unput_tok_count;  /* 1 if token was unput */
YYSTYPE yylval;

char kb_input_new ARGS((void));
void kb_unput ARGS((char));
int kblex ARGS((void));
#define KB_INPUT kb_input_new
#define KB_UNPUT kb_unput

/* for generation of bad tokens for error message generation */
/* for use with -E command line option */
int next_err_token = 0;  /* token in file for which to generate error */
int token_count; /* resets on file initialization */
int err_tok_gen_flag;  /* whether -E option in effect */

int kb_yylex(lvalp)
YYSTYPE *lvalp;  /* for pure parser destination for yylval */
{
  if ( lvalp )
  { PROF_FINISH(yyparse); /* exclude from yyparse time */
  }

  PROF_START(yylex);

  if ( unput_tok_count ) 
  { tok = unputted_tok[--unput_tok_count]; 
  }
  else 
  { 
    /* clean out previous data */
    /* memset(&yylval,0,sizeof(yylval)); time hog*/
    /*  tok = yylex(); */
    if ( err_tok_gen_flag )
    { if ( token_count == next_err_token )
      { tok = GEN_ERROR_TOKEN;
        if ( yytext )
          strcpy(yytext,"generated error");
        next_err_token++;
        token_count += 10000000; /* just one error per file */
        return tok;
      }
      token_count++;
    }
    tok = kblex();
  }

  switch ( tok )
  { case '(' : parens++; break;
    case ')' : parens--; break;
    case '{' : brace_depth++; break;
    case '}' : brace_depth--; break;
  }

  if ( lvalp ) 
  {
    {
      /* non-movsd substitute for     *lvalp = yylval; */
      int i;
      int *dest = (int*)lvalp;
      int *src  = (int*)&yylval;
      for ( i = 0 ; i < sizeof(yylval)/sizeof(int) ; i++ )
         *(dest++) = *(src++);
    }

    PROF_START(yyparse);
  }


  PROF_FINISH(yylex);
  return tok;
}
/* For unputting unneeded look-ahead token */
void unput_tok()
{ 
  if ( unput_tok_count >= UNPUTMAX )
    kb_error(2324,"Internal error: unputted_tok stack overflow.\n",RECOVERABLE);
  unputted_tok[unput_tok_count++] = tok;
  /* fprintf(stderr,"UNPUT %3d %s %s\n",tok,tokname(tok),yytext); */
  /* tok = UNPUTTED_; */ /* some places depend on knowing unput lookahead */
 
}

#ifndef NOPROTO
int rawinput ARGS((void));
int yyback(int *,int);
/* #define yyoutput yyout_unused */
/* #define yyunput yyunput_unused */
#endif

extern FILE *data_fd;
extern char *cmdptr;
int line_no = 1;

int macro_flag;  /* tells macro() identifier is being #define'd */
/* return values for macro() */
#define NO_MACRO  0
#define WAS_MACRO_DEF 1
#define MACRO_EXPANDED 2

char *whitespace = " \t\r,:;";   /* whitespace */

#define SUBMAX 500
int  macro_max;
int  macro_subs_top;  /* index of top of string space */
int  macro_subs_max;   /* space allocated for strings */

struct dkey { char *name; int token; } datafile_keywords[] =
{ 
  {"content_rank",CONTENT_RANK_},
  {"clip_coeff",CLIP_COEFF},
  {"slice_coeff",SLICE_COEFF},
  {"high_constraint",V_HIGH_CONSTRAINT},
  {"high_boundary",V_HIGH_BOUNDARY},
  {"suppress_warning",SUPPRESS_WARNING_},
  {"unsuppress_warning",UNSUPPRESS_WARNING_},
  {"volume_method_name",VOLUME_METHOD_NAME_},
  {"partner_hitting",PARTNER_HITTING_},
  {"procedure",PROCEDURE_WORD_},
  {"display_origin",DISPLAY_ORIGIN_},
  {"length_method_name",LENGTH_METHOD_NAME_},
  {"area_method_name",AREA_METHOD_NAME_},
  {"hessian_special_normal_vector",HESSIAN_SPECIAL_NORMAL_VECTOR_},
  {"keep_originals",KEEP_ORIGINALS_},
  {"element_modulus",ELEMENT_MODULUS_},
  {"swap_colors",SWAP_COLORS_},
  {"self",SELF_},
  {"conserved",CONSERVED_},
  {"actual_volume",ACTUAL_VOLUME_},
  {"keep_macros",KEEP_MACROS_},
  {"pdelta",DELTA_ },
  {"tolerance",TOLERANCE_},
  {"lagrange_multiplier",LAGRANGE_MULTIPLIER_ },
  {"evolver_version",VERSION_},
  {"orientation",ORIENTATION_},
  {"ignore_constraints",IGNORE_CONSTRAINTS_},
  {"ignore_fixed",IGNORE_FIXED_},
  {"load_library",LOAD_LIBRARY_},
  {"interp_bdry_param",INTERP_BDRY_PARAM_},
  {"axial_point",AXIAL_POINT_},
  {"lagrange",LAGRANGE_},
  {"lagrange_order",LAGRANGE_ORDER_},
  {"parameter_1",PARAMETER_1_},
  {"optimizing_parameter",OPTIMIZING_PARAMETER_},
  {"optimising_parameter",OPTIMIZING_PARAMETER_},
  {"everything_quantities",EVERYTHING_QUANTITIES_},
  {"value",VALUE_},
  {"target",TARGET_},
  {"define",DEFINE_},
  {"attribute",ATTRIBUTE_},
  {"method_instance",METHOD_INSTANCE_},
  {"method",METHOD_},
  {"scalar_integrand",SCALAR_INTEGRAND_},
  {"vector_integrand",VECTOR_INTEGRAND_},
  {"k_vector_order",K_VEC_ORDER_},
  {"form_integrand",FORM_INTEGRAND_},
  {"mobility",MOBILITY_},
  {"mobility_tensor",MOBILITY_TENSOR_},
  {"bare",BARE_},
  {"boundary_curvature",BOUNDARY_CURVATURE_},
  {"modulus",MODULUS_},
  {"info_only",INFO_ONLY_},
  {"global_method",GLOBAL_METHOD_},
  {"global",GLOBAL_},
  {"area_fixed",AREA_FIXED_},
  {"fixed_area",AREA_FIXED_},
  {"view_matrix",VIEW_MATRIX_},
  {"view_transforms",VIEW_TRANSFORMS_},
  {"view_transform_generators",VIEW_TRANSFORM_GENS_},
  {"homothety",HOMOTHETY_},
  {"approximate_curvature",APPROX_CURV_},
  {"approx_curvature",APPROX_CURV_},
  {"phasefile",PHASEFILE_},
  {"phase",PHASE_},
  {"autopop",AUTOPOP_},
  {"autopop_quartic",AUTOPOP_QUARTIC_},
  {"autochop",AUTOCHOP_},
  {"total_time",TOTAL_TIME_},
  {"effective_area",EFFECTIVE_AREA_},
  {"runge_kutta",RUNGE_KUTTA_},
  {"color",COLOR_},
  {"backcolor",BACKCOLOR_},
  {"frontcolor",FRONTCOLOR_},
  {"mean_curvature_integral",MEAN_CURV_INT_},
  {"normal_curvature",NORMAL_CURVATURE_},
  {"square_curvature",SQUARE_CURVATURE_},
  {"squared_curvature",SQUARE_CURVATURE_},
  {"square_gaussian_curvature",SQGAUSS_},
  {"squared_gaussian_curvature",SQGAUSS_},
  {"gauss_curvature",GAUSS_CURVATURE_},
  {"insulating_knot_energy",INSULATING_KNOT_ENERGY_},
  {"conducting_knot_energy",CONDUCTING_KNOT_ENERGY_},
  {"space_dimension",SPACE_DIMENSION_},
  {"surface_dimension",SURFACE_DIMENSION_},
  {"simplex_representation",SIMPLEX_REP_},
  {"metric",METRIC_},
  {"klein_metric",KLEIN_METRIC_},
  {"conformal_metric",CONFORMAL_},
  {"fixed",FIXED_},
  {"no_refine",NO_REFINE_},
  {"hit_partner",HIT_PARTNER_},
  {"no_display",NODISPLAY_},
  {"noncontent",NONCONTENT_},
  {"efixed",EFIXED_},
  {"symmetry_group",SYMMETRY_GROUP_},
  {"wrap",WRAP_},
  {"torus",TORUS_},
  {"torus_filled",TORUS_FILLED_},
  {"torus_periods",TORUS_PERIODS_},
  {"periods",PERIODS_},
  {"display_periods",DISPLAY_PERIODS_},
  {"string",STRING_},
  {"soapfilm",SOAPFILM_},
  {"wulff",WULFF_},
  {"boundary",BOUNDARY_},
  {"boundaries",BOUNDARY_},
  {"constraint",CONSTRAINT_},
  {"constraints",CONSTRAINT_},
  {"surface_energy",SURFACE_ENERGY_},
  {"formula",FUNCTION_},
  {"function",FUNCTION_},
  {"parameter",PARAMETERS_},
  {"parameters",PARAMETERS_},
  {"parameter_file",PARAMETER_FILE_},
  {"symmetric_content",SYMMETRIC_CONTENT_},
  {"integral_order",V_INTEGRAL_ORDER},
  {"integral_order_1d",V_INTEGRAL_ORDER_1D},
  {"integral_order_2d",V_INTEGRAL_ORDER_2D},
  {"integration_order",V_INTEGRAL_ORDER},
  {"integration_order_1d",V_INTEGRAL_ORDER_1D},
  {"integration_order_2d",V_INTEGRAL_ORDER_2D},
  {"constraint_tolerance",CONSTRAINT_TOLERANCE_ },
  {"convex",CONVEX_},
  {"nonwall",NONWALL_},
  {"nonnegative",NONNEGATIVE_},
  {"nonpositive",NONPOSITIVE_},
  {"global",GLOBAL_},
  {"energy",ENERGY_},
  {"content",CONTENT_},
  {"quadratic",QUADRATIC_},
  {"linear",LINEAR_},
  {"area_normalization",MEAN_CURV_},
  {"jiggle",JIGGLE_},
  {"diffusion",DIFFUSION_},
  {"merit_factor",MERITFACTOR_},
  {"gravity_constant",GRAV_CONST_},
  {"spring_constant",SPRING_CONSTANT_},
  {"gap_constant",GAP_CONSTANT_},
  {"scale",SCALE_},
  {"pscale",SCALE_},
  {"temperature",TEMPERATURE_},
  {"pressure",PRESSURE_},
  {"volume",VOLUME_},
  {"density",DENSITY_},
  {"tension",DENSITY_},
  {"nodisplay",NODISPLAY_},
  {"scale_limit",SCALE_LIMIT_},
  {"zoom_vertex",ZOOM_VERTEX_},
  {"zoom_radius",ZOOM_RADIUS_},
  {"quantity",QUANTITY_ },
  {"volconst",VOLCONST_ },
  {"read",READ_},
  {"and",AND_},
  {"or",OR_},
  {"not",NOT_},
  {"mod",'%'},
  {"imod",IMOD_},
  {"idiv",IDIV_},
  {"dot_product",DOT_},
  {"pi",PI_},
  {"e",E_},
  {"g",G_},
  {"tag",TAG_},
  {"original",ORIGINAL_},
  {"vertices",VERTICES_},
  {"vertex",VERTICES_},
  {"edges",EDGES_},
  {"edge",EDGES_},
  {"faces",FACES_},
  {"face",FACES_},
  {"facets",FACES_},
  {"facet",FACES_},
  {"bodies",BODIES_},
  {"body",BODIES_},
  {"facet_edges",FACETEDGES_},
  {"facet_edge",FACETEDGES_},
  {"facetedges",FACETEDGES_},
  {"facetedge",FACETEDGES_}
};


struct ckey mathfunc_keywords[] = {
  {"wrap_inverse",WRAP_INVERSE_},
  {"ellipticK",ELLIPTICK},
  {"ellipticE",ELLIPTICE},
  {"log",LOG},
  {"exp",EXP},
  {"sin",SIN},
  {"cos",COS},
  {"tan",TAN},
  {"asin",ASIN},
  {"acos",ACOS},
  {"atan",ATAN},
  {"sinh",SINH},
  {"cosh",COSH},
  {"tanh",TANH},
  {"asinh",ASINH},
  {"acosh",ACOSH},
  {"atanh",ATANH},
  {"sqrt",SQRT},
  {"sqr",SQR},
  {"ceil",CEIL_},
  {"floor",FLOOR_},
  {"abs",ABS}
};

struct ckey mathfunc2_keywords[] = { /* two arguments */
  {"wrap_compose",WRAP_COMPOSE_},
  {"atan2",ATAN2_},
  {"incompleteEllipticF",INCOMPLETE_ELLIPTICF},
  {"incompleteEllipticE",INCOMPLETE_ELLIPTICE},
  {"maximum",MAXIMUM_},
  {"minimum",MINIMUM_},
  {"pow",POW}
};

struct ckey command_keywords[] =
{ 
  {"valid_constraint",VALID_CONSTRAINT_},
  {"valid_boundary",VALID_BOUNDARY_},
  {"profiling",PROFILING_},
  {"reset_profiling",RESET_PROFILING_},
  {"suppress_warning",SUPPRESS_WARNING_},
  {"unsuppress_warning",UNSUPPRESS_WARNING_},
  {"delete_text",DELETE_TEXT_},
  {"display_text", DISPLAY_TEXT_ },
  {"simplex_to_fe", SIMPLEX_TO_FE_ },
  {"addload", ADDLOAD_ },
  {"whereami",WHEREAMI_},
  {"breakpoint",BREAKPOINT_},
  {"breakpoints",BREAKPOINT_},
  {"abort",ABORT_},
  {"subcommand",SUBCOMMAND_},
  {"global",GLOBAL_},
  {"repartition",REPARTITION_},
  {"free_discards",FREE_DISCARDS_},
  {"dump_memlist",DUMP_MEMLIST_},
  {"reverse_orientation",REVERSE_ORIENTATION_},
  {"matrix_multiply",MATRIX_MULTIPLY_},
  {"matrix_inverse",MATRIX_INVERSE_},
  {"matrix_determinant",MATRIX_DETERMINANT_},
  {"mid_edge",MID_EDGE_},
  {"mid_facet",MID_FACET_},
  {"flush_counts",FLUSH_COUNTS_},
  {"valid_element",VALID_ELEMENT_},
  {"reset_counts",RESET_COUNTS_},
  {"vertex_merge",MERGE_VERTEX_},
  {"edge_merge",MERGE_EDGE_},
  {"facet_merge",MERGE_FACET_},
  {"mpi_task",MPI_TASK_ATTR_},
  {"mean_curvature",MEAN_CURVATURE_},
  {"element_modulus",ELEMENT_MODULUS_},
  {"ignore_constraints",IGNORE_CONSTRAINTS_},
  {"ignore_fixed",IGNORE_FIXED_},
  {"global_method",GLOBAL_METHOD_},
  {"scalar_integrand",SCALAR_INTEGRAND_},
  {"vector_integrand",VECTOR_INTEGRAND_},
  {"k_vector_order",K_VEC_ORDER_},
  {"form_integrand",FORM_INTEGRAND_},
  {"info_only",INFO_ONLY_},
  {"method",METHOD_},
  {"parallel_exec",PARALLEL_EXEC_},
  {"task_exec",TASK_EXEC_},
  {"pop",POP_},
  {"pop_tri_to_edge",POP_TRI_TO_EDGE_},
  {"pop_edge_to_tri",POP_EDGE_TO_TRI_},
  {"pop_quad_to_quad",POP_QUAD_TO_QUAD_},
  {"local",LOCAL_},
  {"for",FOR_},
  {"warning_messages",WARNING_MESSAGES_},
  {"equiangulate",EQUIANGULATE_},
  {"no_refine",NO_REFINE_},
  {"hit_partner",HIT_PARTNER_},
  {"no_display",NODISPLAY_},
  {"vertexnormal",VERTEXNORMAL_},
  {"colorfile",COLORFILE_},
  {"date_and_time",DATE_AND_TIME_},
  {"exprint",EXPRINT_},
  {"energy",ENERGY_},
  {"info_only",INFO_ONLY_},
  {"conserved",CONSERVED_},
  {"exec",EXEC_},
  {"wrap_vertex",WRAP_VERTEX_},
  {"self",SELF_},
  {"is_defined",IS_DEFINED_},
  {"nodisplay",NODISPLAY_},
  {"no_display",NODISPLAY_},
  {"function",FUNCTION_},
  {"reorder_storage",REORDER_STORAGE_},
  {"renumber_all",RENUMBER_ALL_},
/*  {"view_matrix",VIEW_MATRIX_}, */
  {"pause",PAUSE_},
  {"pdelta",DELTA_ },
  {"pscale",SCALE_ },
  {"scale",SCALE_ },
  {"tolerance",TOLERANCE_},
  {"method_instance",METHOD_INSTANCE_},
  {"logfile",LOGFILE_},
  {"keylogfile",KEYLOGFILE_},
  {"frontbody",FRONTBODY_},
  {"backbody",BACKBODY_},
  {"new_vertex",NEWVERTEX_},
  {"new_edge",NEWEDGE_},
  {"new_facet",NEWFACET_},
  {"new_body",NEWBODY_},
  {"noncontent",NONCONTENT_},
/*  {"inverse_periods",INVERSE_PERIODS_}, */
  {"return",RETURN_},
  {"postscript",POSTSCRIPT_},
  {"ooglfile",OOGLFILE_},
  {"binary_off_file",BINARY_OFF_FILE_},
  {"vertex_average",VERTEX_AVERAGE_},
  {"raw_vertex_average",RAW_VERTEX_AVERAGE_},
  {"rawest_vertex_average",RAWEST_VERTEX_AVERAGE_},
  {"axial_point",AXIAL_POINT_},
  {"metis_factor",METIS_FACTOR_},
  {"lagrange",LAGRANGE_},
  {"metis",METIS_},
  {"metis_readjust",METIS_READJUST_},
  {"body_metis",BODY_METIS_},
  {"kmetis",KMETIS_},
  {"ometis",OMETIS_},
  {"sizeof",SIZEOF_},
  {"move",MOVE_},
  {"geompipe",GEOMPIPE_},
  {"convert_to_quantities",CONVERT_TO_QUANTS_},
  {"edgeswap",EDGESWAP_},
  {"t1_edgeswap",T1_EDGESWAP_},
/*  {"torus_periods",TORUS_PERIODS_}, */
  {"break",BREAK_},
  {"volfixed",FIXEDVOL_},
  {"continue",CONTINUE_},
  {"volconst",VOLCONST_ },
  {"ritz",RITZ_},
  {"orientation",ORIENTATION_},
  {"eigenprobe",EIGENPROBE_},
  {"lanczos",LANCZOS_},
  {"tetra_point",TETRA_POINT_},
  {"triple_point",TRIPLE_POINT_},
  {"value",VALUE_},
  {"target",TARGET_},
  {"datafilename",DATAFILENAME_},
  {"geomview",GEOMVIEW_},
  {"saddle",HESSIAN_SADDLE_},
  {"midv",MIDV_},
  {"define",DEFINE_},
  {"attribute",ATTRIBUTE_},
  {"attributes",ATTRIBUTE_},
  {"string",STRING_},
  {"sobolev",SOBOLEV_},
  {"sobolev_seek",SOBOLEV_SEEK_},
  {"dirichlet",DIRICHLET_},
  {"dirichlet_seek",DIRICHLET_SEEK_},
  {"wrap",WRAP_},
  {"total",TOTAL_},
  {"history",HISTORY_},
  {"fix",FIX_},
  {"unfix",UNFIX_},
  {"bare",BARE_},
  {"phase",PHASE_},
  {"foreach",FOREACH_},
  {"rebody", REBODY_},
  {"burchard", BURCHARD_},
  {"close_show",CLOSE_SHOW_},
  {"show_off",CLOSE_SHOW_},
/*  {"view_transforms",VIEW_TRANSFORMS_}, */
/*  {"view_transform_swap_colors",VIEW_TRANSFORM_SWAP_COLORS_}, */
/*  {"view_transform_parity",VIEW_TRANSFORM_PARITY_}, */
  {"transform_depth",TRANSFORM_DEPTH_},
  {"transform_expr",TRANSFORM_EXPR_},
  {"modulus",MODULUS_},
  {"boundary",BOUNDARY_},
  {"on_constraint",ON_CONSTRAINT_},
  {"hit_constraint",HIT_CONSTRAINT_},
  {"on_boundary",ON_BOUNDARY_},
  {"on_quantity",ON_QUANTITY_},
  {"on_method_instance",ON_METHOD_INSTANCE_},
  {"hessian",HESSIAN_},
  {"hessian_seek",HESSIAN_SEEK_},
  {"hessian_menu",HESSIAN_MENU_},
  {"help",HELP_},
  {"quit",QUIT_},
  {"exit",QUIT_},
  {"bye",QUIT_},
  {"dump",DUMP_},
  {"load",LOAD_},
  {"permload",PERMLOAD_},
  {"quantity",QUANTITY_},
  {"spring_constant",GAP_CONSTANT_},
  {"gap_constant",GAP_CONSTANT_},
  {"notch",NOTCH_},
  {"while",WHILE_},
  {"do",DO_},
  {"if",IF_},
  {"then",THEN_},
  {"else",ELSE_},
  {"histogram",HISTOGRAM_},
  {"loghistogram",LOGHISTOGRAM_},
  {"show_expr",SHOW_EXPR_},
  {"show_trans",SHOW_TRANS_},
  {"dihedral",DIHEDRAL_},
  {"max",MAX_},
  {"min",MIN_},
  {"avg",AVG_},
  {"sum",SUM_},
  {"count",COUNT_},
  {"print",PRINT_},
  {"eprint",EPRINT_},
  {"printf",PRINTF_},
  {"errprintf",ERRPRINTF_},
  {"sprintf",SPRINTF_},
  {"binary_printf",BINARY_PRINTF_},
  {"procedures",PROCEDURES_},
  {"procedure",PROCEDURE_WORD_},
  {"on",ON_},
  {"counts",COUNTS_},
  {"extrapolate",EXTRAPOLATE_},
  {"off",OFF_},
  {"areaweed",AREAWEED_},
  {"edgeweed",EDGEWEED_},
  {"edge_divide",EDGEDIVIDE_},
  {"alice",ALICE_},
  {"stability_test",STABILITY_TEST_},
  {"zoom",ZOOM_},
  {"utest",UTEST_},
  {"system",SYSTEM_},
  {"chdir",CHDIR_},
  {"longj",LONG_JIGGLE_},
  {"rawv",RAW_VERAVG_},
  {"rawestv",RAWEST_VERAVG_},
  {"go",GO_},
  {"refine",REFINE_},
  {"check",CHECK_},
  {"show_vol",SHOW_VOL_},
  {"id",ID_},
  {"oid",OID_},
  {"and",AND_},
  {"or",OR_},
  {"not",NOT_},
  {"mod",'%'},
  {"imod",IMOD_},
  {"idiv",IDIV_},
  {"dot_product",DOT_},
  {"pi",PI_},
  {"e",E_},
  {"g",G_},
  {"tag",TAG_},
  {"original",ORIGINAL_},
  {"fixed",FIXED_},
  {"vertices",VERTICES_},
  {"vertex",VERTICES_},
  {"edges",EDGES_},
  {"edge",EDGES_},
  {"facets",FACETS_},
  {"facet",FACETS_},
  {"faces",FACETS_},
  {"face",FACETS_},
  {"facet_edges",FACETEDGES_},
  {"facet_edge",FACETEDGES_},
  {"facetedges",FACETEDGES_},
  {"facetedge",FACETEDGES_},
  {"bodies",BODIES_},
  {"body",BODIES_},
  {"topinfo",TOPINFO_},
  {"bottominfo",BOTTOMINFO_},
  {"length",LENGTH_ },
  {"valence",VALENCE_ },
  {"area",AREA_ },
  {"volume",VOLUME_ },
  {"sqcurve",SQ_MEAN_CURV_},
  {"where",WHERE_ },
  {"list",LIST_ },
  {"show",SHOW_},
  {"showq",SHOWQ_},
  {"delete",DELETE_ },
  {"dissolve",DISSOLVE_ },
  {"refine",REFINE_ },
  {"shell",SHELL_},
  {"constraint",CONSTRAINT_},
  {"pressure",PRESSURE_},
  {"color",COLOR_},
  {"backcolor",BACKCOLOR_},
  {"frontcolor",FRONTCOLOR_},
  {"opacity",OPACITY_},
  {"volume",VOLUME_},
  {"density",DENSITY_},
  {"tension",DENSITY_},
  {"set",SET_},
  {"read",READ_},
  {"unset",UNSET_},
  {"recalc",RECALC_},
  {"optimize",OPTIMIZE_},
  {"optimise",OPTIMIZE_},
  {"autochop",AUTOCHOP_},
 }; 

struct ckey togglenames[] = {
  {"immediate_autopop",IMMEDIATE_AUTOPOP_},
  {"star_finagling",STAR_FINAGLING_},
  {"force_deletion",FORCE_DELETION_},
  {"function_quantity_sparse",FUNCTION_QUANTITY_SPARSE_},
  {"slice_view",SLICE_VIEW_},
  {"clip_view",CLIP_VIEW_},
  {"quietload",QUIETLOAD_},
  {"big_endian",BIG_ENDIAN_},
  {"little_endian",LITTLE_ENDIAN_},
  {"full_bounding_box",FULL_BOUNDING_BOX_},
  {"pop_disjoin",POP_DISJOIN_},
  {"pop_enjoin",POP_ENJOIN_},
  {"pop_to_edge",POP_TO_EDGE_},
  {"pop_to_face",POP_TO_FACE_},
  {"mpi_debug",MPI_DEBUG_},
  {"smooth_graph",SMOOTH_GRAPH_},
  {"bezier_basis",BEZIER_BASIS_},
  {"break_after_warning",BREAK_AFTER_WARNING_},
  {"blas_flag",BLAS_FLAG_},
  {"diffusion",DIFFUSION_},
  {"augmented_hessian",AUGMENTED_HESSIAN_},
  {"sparse_constraints",SPARSE_CONSTRAINTS_},
  {"visibility_test", VISIBILITY_TEST_},
  {"circular_arc_draw",CIRCULAR_ARC_DRAW_},
  {"rgb_colors",RGB_COLORS_FLAG_},
  {"kraynikpopvertex",KRAYNIKPOPVERTEX_FLAG_},
  {"kraynikpopedge",KRAYNIKPOPEDGE_FLAG_},
  {"sobolev_mode",SOBOLEV_MODE_},
  {"dirichlet_mode",DIRICHLET_MODE_},
  {"verbose",VERBOSE_},
  {"torus_filled",TORUS_FILLED_},
  {"ambient_pressure",AMBIENT_PRESSURE_},
  {"backcull",BACKCULL_},
  {"interp_normals",INTERP_NORMALS_},
  {"volgrads_every",VOLGRADS_EVERY_},
  {"zener_drag",ZENER_DRAG_},
  {"hessian_special_normal",HESSIAN_SPECIAL_NORMAL_},
  {"hessian_normal_perp",HESSIAN_NORMAL_PERP_},
  {"show_all_quantities",SHOW_ALL_QUANTITIES_},
  {"pscolorflag",PSCOLORFLAG_},
  {"ps_colorflag",PSCOLORFLAG_},
  {"ps_gridflag",GRIDFLAG_},
  {"gridflag",GRIDFLAG_},
  {"crossingflag",CROSSINGFLAG_},
  {"labelflag",LABELFLAG_},
  {"ps_labelflag",LABELFLAG_},
  {"hessian_normal_one",HESSIAN_NORMAL_ONE_},
  {"interp_bdry_param",INTERP_BDRY_PARAM_},
  {"hessian_double_normal",HESSIAN_DOUBLE_NORMAL_},
  {"h_inverse_metric",H_INVERSE_METRIC_},
  {"squared_gradient",SQUARED_GRADIENT_},
  {"linear_metric",LINEAR_METRIC_},
  {"metric_convert",METRIC_CONVERT_},
  {"quantities_only",QUANTITIES_ONLY_},
  {"ysmp",YSMP_},
  {"bunch_kaufman",BUNCH_KAUFMAN_},
  {"bunch_kauffman",BUNCH_KAUFMAN_},
  {"hessian_normal",HESSIAN_NORMAL_},
  {"jiggle",JIGGLE_TOGGLE_},
  {"assume_oriented",ASSUME_ORIENTED_},
  {"ribiere",RIBIERE_CG_},
  {"area_normalization",MEAN_CURV_},
  {"force_pos_def",FORCE_POS_DEF_},
  {"autodisplay",AUTODISPLAY_},
  {"lagrange",LAGRANGE_},
  {"quadratic",QUADRATIC_},
  {"linear",LINEAR_},
  {"show_inner",SHOW_INNER_},
  {"area_fixed",AREA_FIXED_},
  {"fixed_area",AREA_FIXED_},
  {"clipped",CLIPPED_CELLS_},
  {"clipped_cells",CLIPPED_CELLS_},
  {"raw_cells",RAW_CELLS_},
  {"connected",CONNECTED_CELLS_},
  {"connected_cells",CONNECTED_CELLS_},
  {"show_outer",SHOW_OUTER_},
  {"colormap",COLORMAP_},
  {"thicken",THICKEN_},
  {"hessian_diff",HESSIAN_DIFF_},
  {"normal_motion",NORMAL_MOTION_},
  {"runge_kutta",RUNGE_KUTTA_},
  {"deturck",DETURCK_},
  {"kusner",KUSNER_},
  {"view_4d",VIEW_4D_},
  {"conf_edge",CONF_EDGE_SQCURV_},
  {"mean_curvature_integral",MEAN_CURV_INT_},
  {"sqgauss",SQGAUSS_},
  {"autopop",AUTOPOP_},
  {"autopop_quartic",AUTOPOP_QUARTIC_},
  {"old_area",OLD_AREA_}, 
  {"approx_curv",APPROX_CURV_},
  {"check_increase",CHECK_INCREASE_},
  {"debug",DEBUG_},
  {"memdebug",MEMDEBUG_},
  {"itdebug",ITDEBUG_},
  {"gravity",GRAVITY_},
  {"effective_area",EFFECTIVE_AREA_},
  {"estimate",ESTIMATE_},
  {"post_project",POST_PROJECT_},
  {"transforms",TRANSFORMS_},
  {"quiet",QUIET_},
  {"quietgo",QUIETGO_},
  {"hessian_quiet",HESSIAN_QUIET_},
  {"conj_grad",CONJ_GRAD_},
  {"homothety",HOMOTHETY_},
  {"facet_colors",FACET_COLORS_},
  {"shading",SHADING_},
  {"div_normal_curvature",DIV_NORMAL_CURVATURE_},
  {"normal_curvature",NORMAL_CURVATURE_},
  {"boundary_curvature",BOUNDARY_CURVATURE_},
  {"self_similar",SELF_SIMILAR_},
  {"gv_binary",GV_BINARY_},
  {"metric_conversion",METRIC_CONVERSION_},
  {"autorecalc",AUTORECALC_},
  {"pinning",PINNING_}
  };
struct ckey colornames[] = {
    {"clear",CLEAR},     /* transparent */
    {"transparent",CLEAR},     /* transparent */
    {"black",BLACK},            /* dark colors */
    {"blue",BLUE},
    {"green",GREEN},
    {"cyan",CYAN},
    {"red",RED},
    {"magenta",MAGENTA},
    {"brown",BROWN},
    {"lightgray",LIGHTGRAY},
    {"lightgrey",LIGHTGRAY},
    {"gray",LIGHTGRAY},
    {"grey",LIGHTGRAY},
    {"darkgray",DARKGRAY},            /* light colors */
    {"darkgrey",DARKGRAY},            /* light colors */
    {"lightblue",LIGHTBLUE},
    {"lightgreen",LIGHTGREEN},
    {"lightcyan",LIGHTCYAN},
    {"lightred",LIGHTRED},
    {"lightmagenta",LIGHTMAGENTA},
    {"yellow",YELLOW},
    {"white",WHITE}
    };

struct ckey internal_variables[] = {
  {"autochop_length",V_AUTOCHOP_LENGTH},
  {"string_curve_tolerance",V_STRING_CURVE_TOLERANCE},
  {"corona_state",V_CORONA_STATE},
  {"mpi_maxtask",V_MPI_MAXTASK}, 
  {"this_task",V_THIS_TASK}, /* MPI task number */
  {"window_aspect_ratio",V_WINDOW_ASPECT_RATIO},
  {"mindeg_debug_level",V_MINDEG_DEBUG_LEVEL},
  {"mindeg_min_region_size",V_MINDEG_MIN_REGION_SIZE},
  {"mindeg_margin",V_MINDEG_MARGIN},
  {"fix_count",V_FIX_COUNT},
  {"unfix_count",V_UNFIX_COUNT},
  {"edge_delete_count",V_EDGE_DELETE_COUNT},
  {"facet_delete_count",V_FACET_DELETE_COUNT},
  {"vertex_dissolve_count",V_VERTEX_DISSOLVE_COUNT},
  {"edge_dissolve_count",V_EDGE_DISSOLVE_COUNT},
  {"facet_dissolve_count",V_FACET_DISSOLVE_COUNT},
  {"body_dissolve_count",V_BODY_DISSOLVE_COUNT},
  {"edge_refine_count",V_EDGE_REFINE_COUNT},
  {"facet_refine_count",V_FACET_REFINE_COUNT},
  {"vertex_pop_count",V_VERTEX_POP_COUNT},
  {"edge_pop_count",V_EDGE_POP_COUNT},
  {"pop_tri_to_edge_count",V_POP_TRI_TO_EDGE_COUNT},
  {"pop_edge_to_tri_count",V_POP_EDGE_TO_TRI_COUNT},
  {"pop_quad_to_quad_count",V_POP_QUAD_TO_QUAD_COUNT},
  {"edgeswap_count",V_EDGESWAP_COUNT},
  {"t1_edgeswap_count",V_T1_EDGESWAP_COUNT},
  {"ps_labelsize",V_PS_LABELSIZE_},
  {"ps_stringwidth",V_PS_STRINGWIDTH_},
  {"ps_fixededgewidth",V_PS_FIXEDEDGEWIDTH_},
  {"ps_tripleedgewidth",V_PS_TRIPLEEDGEWIDTH_},
  {"ps_conedgewidth",V_PS_CONEDGEWIDTH_},
  {"ps_bareedgewidth",V_PS_BAREEDGEWIDTH_},
  {"ps_gridedgewidth",V_PS_GRIDEDGEWIDTH_},
  {"scrollbuffersize",V_SCROLLBUFFERSIZE_},
  {"visibility_debug",V_VISIBILITY_DEBUG_},
  {"check_count",V_CHECK_COUNT_},
  {"breakflag",V_BREAKFLAG_},
  {"everything_quantities",EVERYTHING_QUANTITIES_},
  {"gravity_constant",GRAV_CONST_},
  {"hessian_slant_cutoff",V_HESSIAN_SLANT_CUTOFF},
  {"ambient_pressure_value",V_AMBIENT_PRESSURE},
  {"last_error",V_LAST_ERROR},
  {"memory_arena",V_MEMARENA},
  {"memory_used",V_MEMUSED},
  {"background",V_BACKGROUND},
  {"brightness",V_BRIGHTNESS},
  {"diffusion_coeff",V_DIFFUSION},
  {"transform_count",V_TRANSFORM_COUNT},
  {"scale_limit",V_SCALE_LIMIT},
  {"clock",V_CLOCK},
  {"cpu_counter",V_CPU_COUNTER},
  {"target_tolerance",V_TARGET_TOLERANCE},
  {"thickness",V_THICKNESS},
  {"spring_constant",V_GAP_CONSTANT},
  {"gap_constant",V_GAP_CONSTANT},
  {"lagrange_order",V_LAGRANGE_ORDER},
  {"last_eigenvalue",V_LAST_EIGENVALUE},
  {"last_hessian_scale",V_LAST_HESSIAN_SCALE},
  {"integral_order",V_INTEGRAL_ORDER},
  {"integral_order_1d",V_INTEGRAL_ORDER_1D},
  {"integral_order_2d",V_INTEGRAL_ORDER_2D},
  {"integration_order",V_INTEGRAL_ORDER},
  {"integration_order_1d",V_INTEGRAL_ORDER_1D},
  {"integration_order_2d",V_INTEGRAL_ORDER_2D},
  {"random_seed",V_RANDOM_SEED},
  {"random",V_RANDOM},
  {"linear_metric_mix",V_LINEAR_METRIC_MIX},
  {"quadratic_metric_mix",V_QUADRATIC_METRIC_MIX},
  {"pickvnum",V_PICKVNUM},
  {"pickenum",V_PICKENUM},
  {"pickfnum",V_PICKFNUM},
  {"eigen_pos",V_EIGENPOS},
  {"eigen_neg",V_EIGENNEG},
  {"eigen_zero",V_EIGENZERO},
  {"eigenpos",V_EIGENPOS},
  {"eigenneg",V_EIGENNEG},
  {"eigenzero",V_EIGENZERO},
  {"total_time",V_TIME},
  {"jiggle_temperature",V_JIG_TEMP},
  {"iteration_counter",V_ITER_COUNTER},
  {"scale_scale",V_SCALE_SCALE},
  {"vertex_count",V_VERTEXCOUNT},
  {"edge_count",V_EDGECOUNT},
  {"facet_count",V_FACETCOUNT},
  {"body_count",V_BODYCOUNT},
  {"facetedge_count",V_FACETEDGECOUNT},
  {"total_energy",V_ENERGY},
  {"total_area",V_AREA},
  {"total_length",V_LENGTH},
  {"scale",V_SCALE},
  {"space_dimension",V_SPACE_DIMENSION},
  {"surface_dimension",V_SURFACE_DIMENSION},
  {"torus",V_TORUS},
  {"symmetry_group",V_SYMMETRY_GROUP},
  {"simplex_representation",V_SIMPLEX},
  {"constraint_tolerance",V_TOLERANCE},
  {"hessian_epsilon",V_HESS_EPSILON},
  {"equi_count",V_EQUI_COUNT},
  {"delete_count",V_DELETE_COUNT},
  {"refine_count",V_REFINE_COUNT},
  {"notch_count",V_NOTCH_COUNT},
  {"dissolve_count",V_DISSOLVE_COUNT},
  {"pop_count",V_POP_COUNT},
  {"where_count",V_WHERE_COUNT}
 };
%}

   /* %option array */
D     [0-9]
PD    [1-9]
H     [0-9A-Fa-f]
E     [DEde][+-]?{D}+
W     [ \t\r\032]

%p 4000
%a 3000
%%
"/*"   { /* eat comment */
         register int c;
         in_comment = 1;
         for ( ; ; )
          { while ( (c = input()) != '*' && c != 0 )
          if ( c == '\n' ) line_no++; /* eat up text of comment */
            if ( c == '*' )
              { while ( (c = input()) == '*' ) ;
                if ( c == '/' ) break;    /* found the end */ 
            if ( c == '\n' ) line_no++; 
              }
            if ( c == 0 && yywrap() )
              { kb_error(2325,"End-of-file in comment\n",RECOVERABLE ); break; }
          }
         in_comment = 0; 
       }

"//".*    /* comment */ ;

\n           { line_no++; }
_anti_line_no_  { errspot -= 15; line_no--; }

^{W}*-?{D}+  { char *c = strtok(yytext,whitespace);
               yylval.r = atof(strtok(yytext,whitespace));
               yylval.i = atoi(strtok(yytext,whitespace));
               if ( lists_flag==LISTS_FULL ) return(tok=LEAD_INTEGER_); 
               else if ( uminus_flag || c[0] != '-' )  
                 return(tok = (fabs(yylval.r) > MAXINT) ? REAL_ : INTEGER_);
               else { unput_string(c+1);  c[1] = 0;
               return (tok = minus_type(c[0])); 
             }
          }
{W}+[+-]{D}+ { char *c = strtok(yytext,whitespace);
               yylval.i = atoi(c); 
               yylval.r = atof(strtok(c,whitespace));
               yylval.qnum = 0;
               if ((lists_flag!=LISTS_OFF) && uminus_flag && (parens==0))
               return(tok = (fabs(yylval.r) > MAXINT) ? REAL_ : INTEGER_);
               else { unput_string(c+1);  c[1] = 0;
               return (tok = minus_type(c[0])); 
             }
          }
{D}+      { yylval.i = atoi(yytext); 
            yylval.r = atof(strtok(yytext,whitespace));
            yylval.qnum = 0;
            return(tok = (fabs(yylval.r) > MAXINT) ? REAL_ : INTEGER_);
          }
^{W}*{D}+"@"{D}+ { yylval.i = atoi(yytext);
              yylval.qnum = atoi(strchr(yytext,'@')+1);
              return(tok = LEAD_INTEGER_AT_);
            }
{W}+-?{D}+"@"{D}+ { yylval.i = atoi(yytext);
              yylval.qnum = atoi(strchr(yytext,'@')+1);
              return(tok = INTEGER_AT_);
            }

0x{H}+      { sscanf(yytext+2,"%x",&yylval.i); 
              yylval.r = (REAL)(yylval.i);
              return(tok = INTEGER_);
            }  /* hex */
[01]+[Bb]   { char *c = yytext;  /* binary */
              yylval.i = 0;
              while ( isdigit(*c) ) { yylval.i = 2*yylval.i + *c - '0'; c++;}
              yylval.r = (REAL)(yylval.i);
              return(tok = INTEGER_);
            }

{D}+"."{D}+[a-z]+   return VERSIONTOKEN_;

^{W}*[+-]{D}+"."{D}*({E})?  |
^{W}*[+-]{D}*"."{D}+({E})?  |
^{W}*[+-]{D}+{E}            |
{W}+[+-]{D}+"."{D}*({E})?   |
{W}+[+-]{D}*"."{D}+({E})?   |
{W}+[+-]{D}+{E}   { 
             char *c = strtok(yytext,whitespace);
             yylval.r = atof(c); 
             if ((lists_flag!=LISTS_OFF) && uminus_flag && (parens==0))
               return(tok = REAL_); 
             else 
             { unput_string(c+1);   c[1] = 0;
               verb_flag = 0;  
               return (tok = minus_type(c[0])); 
             }
          }

^{W}*{D}+"."{D}*({E})?   |
^{W}*{D}*"."{D}+({E})?   |
^{W}*{D}+{E}        |
{D}+"."{D}*({E})?   |
{D}*"."{D}+({E})?   |
{D}+{E}    { yylval.r = atof(yytext);
     return(tok = REAL_); }

\"          { /* read quoted string */
              int n;
          in_quote = 1;
          for (n=0;;n++)
          { int c;
        c = input();
        if ( c == '\n' ) line_no++;
        if ( c == '"' && (n == 0 || (yytext[n-1] != '\\')) ) break; 
        if ( n < 198 ) yytext[n] = c;
        else if ( n==199 )
          kb_error(2326,"Quoted string over 198 characters.",WARNING);
              }
          yytext[n] = 0;
          reduce_string(yytext);
          in_quote = 0;
          return(tok = QUOTATION_);
        }

{W}+-{W}+  verb_flag = 0; return (tok = minus_type('-'));   
{W}+-$     verb_flag = 0; return (tok = minus_type('-'));   

{W}+-      { if (datafile_flag && uminus_flag && (parens == 0) )  
               return(tok = UMINUS_);
             else { verb_flag = 0; return ( tok = minus_type('-') );}
           }
{W}+-=     { verb_flag = 2; yylval.i = SUBASSIGN_; return (tok= ASSIGNOP_);}

^-{W}      { return ( tok = minus_type('-')); }
^-         { if (datafile_flag && uminus_flag && (parens == 0) ) 
               return ( tok = UMINUS_);
             else { verb_flag = 0; return ( tok = minus_type('-') ); }
           }
":="       { verb_flag = 2; return (tok= ASSIGN_);}
";="       { verb_flag = 2; 
             kb_error(2327,"You mistyped ';=' for ':='?\n",WARNING);
             return (tok= ASSIGN_);
           }
"+="        { verb_flag = 2; yylval.i = PLUSASSIGN_; return (tok= ASSIGNOP_);}
"-="        { verb_flag = 2; yylval.i = SUBASSIGN_; return (tok= ASSIGNOP_);}
"*="        { verb_flag = 2; yylval.i = MULTASSIGN_; return (tok= ASSIGNOP_);}
"/="        { verb_flag = 2; yylval.i = DIVASSIGN_; return (tok= ASSIGNOP_);}
"::="       { verb_flag = 2; return (tok = PERM_ASSIGN_);}
":::="      { verb_flag = 2; return (tok = REDEFINE_); }
-           { return (tok = minus_type('-'));  }
[<>+*/,.?%^\[\]]       { verb_flag = 0 ;  return(tok = yytext[0]); }
";"         { verb_flag = 1; return tok = ';'; }
"{"         {  verb_flag = 1; return(tok = yytext[0]); }
"}"         {  return(tok = yytext[0]);  }
"="         { return tok = (datafile_flag ? '=' : EQ_); }
"("         {   verb_flag = 0; return(tok = yytext[0]); }
")"         {  return(tok = yytext[0]);  }
"`"         {  return(tok = yytext[0]);  }
"**"        { return(tok = '^'); }
"=="        { return(tok = EQ_); }
"!="        { return(tok = NE_); }
"<="        { return(tok = LE_); }
">="        { return(tok = GE_); }
"||"        { return(tok = OR_); }
"&&"        { return(tok = AND_); }
"!"         { return(tok = NOT_); }
"|"         { return (tok = PIPE_); /* pipe followed by quoted string */ }
">>"        { return (tok = REDIRECT_); /* for redirection */ }
">>>"       { return (tok = REDIRECTOVER_); /* for overwrite redirection */ }
"`"" "*","  { return (tok = BACKQUOTE_COMMA_); }

#define    { if (!datafile_flag)
             kb_error(1880,"#define valid only in top of datafile.\n",WARNING);
             macro_flag = 1;
           }
#include{W}*\"[^"]*\"  { /* nested include */
             char *name = strchr(yytext,'"')+1;/* initial quote + 1 */
             *strchr(name,'"') = 0; /* replace final quote by 0 */
             push_commandfd(NULL,name);
           }

[A-Za-z_][A-Za-z0-9_]*   { strncpy(yylval.lexeme,yytext,31);
                           if ( !macro() ) return identcase(yytext); 
                         } 

"'"[A-Za-z]"'"     { yylval.i = yytext[1]; return tok = SINGLE_LETTER_; }
{W}      ;
":"     { if ( cond_expr_flag ) return tok = ':';}
.       { if ( isprint(yytext[0]) )
            sprintf(errmsg,"Illegal token: %c\n",yytext[0]);
          else
            sprintf(errmsg,"Illegal token: 0x%02X\n",yytext[0]);
          yyerror(errmsg);
          return tok=LEXERROR;
        }
%%

/* My own input() function to handle case insensitivity and macros */
/* and line counting */

/* for saving input for command definition */
int inputbuffersize = 0;
int inputbufferspot = 0;  /* where next character will go */
int inputsave_flag = 0;  /* so only save when reading command */
char *inputbuffer;
void savein(c)
int c;
{ if ( !inputsave_flag ) return;
  if ( inputbufferspot >= inputbuffersize )
  { int newsize = inputbuffersize?2*inputbuffersize:1000;
    inputbuffer = my_list_realloc(inputbuffer,newsize,ETERNAL_BLOCK);
    inputbuffersize = newsize;
  }
  inputbuffer[inputbufferspot++] = (char)c;
}
 
void reset_inputbuffer()
{ inputbufferspot = 0;
  inputsave_flag = 1;
}

#define MOREMAX 1000
char morebuff[MOREMAX+2];

int rawinput()
{
  int c=0,retval;

  if ( cmdptr ) /* read from string */
   {
     if ( *cmdptr == 0 )  /* newline at end of each line */
     { if ( previous_char != '\n' )
       { retval = previous_char = '\n';
         errbuff[errspot++] = retval; 
         savein(retval);
       }
       else retval = 0;
       goto rawreturn; 
     }
     c = *(cmdptr++);
     if ( c == MOREIN ) /* for really long input lines */
     { 
#ifdef USE_READLINE  //CSL
       prompt(CONTPROMPT,morebuff,MOREMAX);
#else
       getstring(morebuff,MOREMAX);
#endif
       cmdptr = morebuff;
       if ( !*cmdptr ) 
       { if (!in_quote && !in_comment) errbuff[errspot++] = '\n';  
         retval = '\n'; 
         savein(retval);
         goto rawexit; 
       }
       c = *(cmdptr++);
     }
   }
  else /* read from file */
   { for(;;)
     {
       if ( commandfd == NULL )  /* in case read after close */
       { retval = previous_char = 0; goto rawreturn; }
       c = getc(commandfd);
       if ( c != -1 ) break;
       pop_commandfd();  /* EOF, so pop #include stack */
       if ( !datafile_flag ) /* EOF for a command file */
       { retval =  previous_char = 0; goto rawreturn; }
     }
   }

  /* taking care of various line-ending combinations */
  if ( (c == '\n') && (previous_char == '\r') ) 
    return  rawinput();
  previous_char = c;
  if ( c == '\r' ) 
     c = '\n';

  errbuff[errspot++] = (char)c;
  retval = c;

  savein(retval);

rawexit:
  if ( errspot >= ERRBUFFSIZE ) /* need partial reset */
    { memcpy(errbuff,errbuff+ERRBUFFSIZE/2,ERRBUFFSIZE/2);
      errspot = ERRBUFFSIZE/2;
    }

rawreturn:
  return  retval;
}

void get_more_input()
{int n; 
 #ifdef USE_READLINE //CSL
  prompt(MOREPROMPT,morebuff,MOREMAX);
 #else
  if ( !topflag && (!quiet_load_flag || (commandfd == stdin)) ) 
     outstring("more> ");
  getstring(morebuff,MOREMAX);
 #endif
  n = strlen(morebuff); 
  if ( (morebuff[n-1] != MOREIN) && (morebuff[n-1] != '\n') )
  morebuff[n] = '\n'; morebuff[n+1] = 0;
  catfulltext(morebuff);
  cmdptr = morebuff;
}

int kb_input()
{
  int c;

  if ( spot > 0 ) 
    { c = buff[--spot];
      goto input_exit;
    }

retry:
  c = rawinput();

  if ( (c == '\\') )  /* line splicing */
  { /* have to do line splicing as input filter to remove 
       special character of line start */
    int cc = c;
    c = rawinput();
    if ( c == 0 ) 
    { get_more_input(); c = ' '; goto input_exit; }
    if ( c == '\n' ) 
    { int ccc;
      line_no++;
      ccc = rawinput();
      if ( ccc == 0 ) 
      { get_more_input(); c = ' '; goto input_exit; }
      rawunput(ccc); c = ' ';
    } 
    else { rawunput(c); c = cc; /* not linesplicing */ } 
  }
  if ( c == 0 && in_comment ) { get_more_input(); goto retry; }
  if ( c == 0 && in_quote ) { get_more_input(); goto retry; }
input_exit:

  return c;
}

void rawunput(c)
int c;
{
  if ( spot >= BUFFSIZE - 1 ) 
  { buff = my_list_realloc(buff,BUFFSIZE+500,ETERNAL_BLOCK);
    BUFFSIZE += 500;
  }

  buff[spot++] = (char)c;
}

void unput_string(s) 
char *s;
{ char *c;
  c = s ; while ( *c )  c++;   /* find end of token */
  while ( c != s ) { --c; unput(*c); }
}


void dump_buff(str,size) /* for error reporting */
char *str;
size_t size;
{ int place;
  strcpy(str,"  Input line so far: \n");
  size -= strlen(str)+3;
  errbuff[errspot] = 0;
  for ( place = errspot-1 ; place >= 0 ; place-- )
  { if (errbuff[place]==0 ) errbuff[place] = ' '; /* nulls creep in */ 
    if (errbuff[place]=='\n' && place < errspot-2) break;
  } 
  strncat(str,errbuff+place+1,size);
  if ( str[strlen(str)-1] != '\n' ) strcat(str,"\n");
}

int yywrap()
{ /* Called at end of input.  Return 1 if really done, 0 if not */
  spot = 0; /* clean buffer */
  if ( (parens > 0) || (brace_depth > 0) || in_quote || in_comment 
         || in_function || in_control_structure /* || loopdepth */ )
    { get_more_input(); return 0; }
  if ( !help_flag )
    switch ( tok )
    { case '+': case '-': case '*': /* might be wrap symbols */
        if ( read_wrap_flag ) return 1;
        else { get_more_input(); return 0 ; }
        break;
      case UMINUS_: case ',':
      case '=': case '?': case ':': case OR_: case AND_: case NOT_:
      case EQ_: case '>': case '<': case LE_: case GE_: case NE_:
      case '/': case '%': case '^': case ON_CONSTRAINT_:
      case HIT_CONSTRAINT_: case ON_BOUNDARY_: case DO_ : case FOR_:
      case WHILE_ : case IF_ : case ELSE_: case THEN_ : case SET_:
      case DEFINE_: case WHERE_: case ASSIGN_ : case PERM_ASSIGN_:
      case REDEFINE_ : case FUNCTION_: case PROCEDURE_WORD_:
      get_more_input(); return 0;
      break;
      default: return 1;
        }
  return 1;  /* done */
}

void macro_init()
{
  if ( macro_subs ) myfree((char *)macro_subs);
  if ( macros ) myfree((char *)macros);
  macro_subs = NULL;
  macros = NULL;
  macro_count = macro_subs_top = macro_max = macro_subs_max = 0;
}

void yylex_init()
{
  unput_tok_count = 0;
  ubuff_spot = 0;
  spot = 0;
  errspot = 0;
  yylval.i = 0;
  yylval.r = 0.0;
  in_function = brace_depth = in_quote = in_comment = 0;
#ifdef FLEX_SCANNER
  if ( yy_current_buffer) YY_FLUSH_BUFFER;
#endif
}

/*************************************************************
*
*  Function: record_macro()
*
*  Purpose:  Record macro definition.
*
*/

void record_macro()
{
  int len;
  char *mspot;
  int cont_flag;
  int backslash_spot;

  macro_flag = 0;

  if ( macro_count >= macro_max )
  { if ( macro_max == 0 )
    { macro_max = 10; 
      macros = (struct macro *)mycalloc(macro_max,sizeof(struct macro));
    }
    else
    { 
      macros = (struct macro *)kb_realloc((char *)macros,
        2*macro_max*sizeof(struct macro));
      macro_max *= 2;
    }
  }

  strncpy(macros[macro_count].name,yytext,MACRONAMESIZE);

  if ( macro_subs_max == 0 )
  { macro_subs_max = 2*SUBMAX; 
    macro_subs = (char *)mycalloc(macro_subs_max,1);
  }

  /* read in macro string */
  for ( len = 0, cont_flag = 0 ; ; len++ )
    { int c = KB_INPUT();
      if ( len == 0 && (c == ' ' || c == '\t') )
      { len--;
        continue; /* skip leading whitespace */
      }
      if ( c == '\\' )
      { cont_flag = 1;
        backslash_spot = len;
      }
      else if ( c != ' ' && c != '\t' && c != '\n' && c != 0 )
        cont_flag = 0;
      if ( (c == '\0') || (c == '\n') )
      { 
        line_no++; 
        if ( !cont_flag ) 
           break; 
        cont_flag = 0;
        len = backslash_spot;
        macro_subs[macro_subs_top+len] = 0;
        len--;
        continue;
      }
      if ( macro_subs_top+len >= macro_subs_max-2 )
      { macro_subs = (char *)kb_realloc(macro_subs,macro_subs_max+2*SUBMAX);
        macro_subs_max += 2*SUBMAX;
      }
      macro_subs[macro_subs_top+len] = (char)c;
    }
  mspot = macro_subs+macro_subs_top;

  /* strip terminal comment */
  if ( strstr(mspot,"//") )
    { *strstr(mspot,"//") = 0;
      len = strlen(mspot);
    }
  else mspot[len] = 0;
  /* strip trailing blanks */
  while ( (len > 0) && (mspot[len-1] == ' ') ) mspot[--len] = 0;
  macros[macro_count].subsize = len;
  macros[macro_count++].offset = macro_subs_top;
  macro_subs_top += len+1;
}

/*************************************************************
*
*  Function:  macro()
*
*  Purpose: See if yytext is a macro, and do substitution.
*
*  Return:  1 if macro, 0 if not.
*
*/

int macro()
{
  int n,k;
  char *c;
  
  if ( macro_flag ) { record_macro(); return WAS_MACRO_DEF; }
  if ( strcmp(yytext,"_anti_line_no_") == 0 )
  { line_no--;  /* kludge line backup */
    errspot -= 15;
    return 1;
  }

  if ( !keep_macros_flag && !datafile_flag ) return 0;  /* no macros in commands */
  /* find name, simple linear search */
  for ( n = macro_count-1 ; n >= 0 ; n-- )  /* use latest definition */
     if ( strcmp(yytext,macros[n].name) == 0 ) break;

  if ( n < 0 ) return 0;   /* not found */

  /* insert string into input in reverse order so reads ok */
  c = macro_subs+macros[n].offset;
  for ( k = macros[n].subsize - 1 ; k >= 0 ; k-- )
    KB_UNPUT(c[k]);

  KB_UNPUT(' ');  /* substitution starts with whitespace; for proper
                     preservation of unary minus leading in the macro */

  return MACRO_EXPANDED;
}

/* for deciding type of minus sign or plus sign */
int minus_type(c)
int c; /* '+' or '-' */
{ int result;
  switch ( tok )
  {  case LEAD_INTEGER_:
     case SIGNED_NUMBER_:
     case INTEGER_:
     case REAL_:
     case PI_:
     case E_:
     case G_:
     case PARAM_: case ARRAYIDENT_:
     case COORD_: case VALUE_: case BARE_:
     case IDENT_: case QUANTITY_NAME_: case WRAP_: case PERM_IDENT_:
     case LENGTH_: case DIHEDRAL_: case VALENCE_:
     case AREA_: case VOLUME_: case DENSITY_: case TAG_:
     case ID_: case ORIGINAL_: case SQ_MEAN_CURV_:
     case OID_: case INTERNAL_VARIABLE_: case COLOR_:
     case TOGGLEVALUE_: case EXTRA_ATTRIBUTE_: case PRESSURE_:
     case ARRAY_ATTRIBUTE_:
     case FRONTCOLOR_: case BACKCOLOR_: case TARGET_:
     case NO_REFINE_: case VOLCONST_: case FRONTBODY_: case NONCONTENT_:
     case BACKBODY_: case TETRA_POINT_: case TRIPLE_POINT_:
     case MIDV_: case PHASE_: case MODULUS_: 
     case METHOD_NAME_: case NODISPLAY_: case HIT_PARTNER_:
     case ']': case ')':
        result = c; 
        break; 
     default: 
        result = (c=='-' ? UMINUS_ : UPLUS_) ; 
        break;
  }
  return result; 
}

/****************************************************************************
*
*  function identcase()
*
*  purpose: handle case of identifier token, maybe macro, keyword, etc.
*/
int keyword_compare(a,b)
struct ckey *a,*b;
{ return stricmp(a->name,b->name); }

int identcase(lexeme)
char *lexeme;
{
   int k,i;
   int type;
   struct sym *s;
   static int sorted = 0; /* flag for sorting keywords */
   struct ckey *keyptr;
   struct ckey key;  /* for bsearch key */
   char *c,*p;
   dll_func_type h;

   yylval.i = 0; 

   if ( ! sorted )
   { qsort((char*)datafile_keywords,
      sizeof(datafile_keywords)/ sizeof(struct dkey),sizeof(struct dkey),
      FCAST keyword_compare);
     qsort((char*)command_keywords,
      sizeof(command_keywords)/ sizeof(struct ckey),sizeof(struct ckey),
      FCAST keyword_compare);
     qsort((char*)togglenames,
      sizeof(togglenames)/ sizeof(struct ckey),sizeof(struct ckey),
      FCAST keyword_compare);
     qsort((char*)colornames,
      sizeof(colornames)/ sizeof(struct ckey),sizeof(struct ckey),
      FCAST keyword_compare);
     qsort((char*)internal_variables,
      sizeof(internal_variables)/ sizeof(struct ckey),sizeof(struct ckey),
      FCAST keyword_compare);
     qsort((char*)mathfunc_keywords,
      sizeof(mathfunc_keywords)/ sizeof(struct ckey),sizeof(struct ckey),
      FCAST keyword_compare);
     qsort((char*)mathfunc2_keywords,
      sizeof(mathfunc2_keywords)/ sizeof(struct ckey),sizeof(struct ckey),
      FCAST keyword_compare);
     sorted = 1;
   }

   if ( strcmp(lexeme,"_command_") == 0 )  { return tok = COMMAND_START_ ; }
   if ( strcmp(lexeme,"_expr_") == 0 )   { return tok = EXPRESSION_START_; }

   /* strip whitespace from front of lexeme */
   c = strtok(lexeme,whitespace);
   if ( c != lexeme ) { p = lexeme;  while ( *c )  *(p++) = *(c++); *p = 0;}

   if ( strlen(lexeme) == 1 )
   { char ch = lexeme[0];
     if ( verb_flag == 1 || tok==PRINT_ ||
       (verb_flag == 2 && (toupper(ch) < 'W'))  )
     { yylval.i=lexeme[0]; 
       if ( single_redefine[yylval.i].start ) return tok = SINGLE_REDEFD_;
       switch ( yylval.i )
       { case 't': case 'l': case 'j': case 'P': case 'M':
         case 'w': case 'n': case 'm':  case 'b':
         case 'k': case 'K': case 'p': case 'y':
           return ( tok = SINGLE_LETTER_ARG_ );
           case 'G': if ( verb_flag ) return ( tok = SINGLE_LETTER_ARG_ );
         else return tok=G_;
         default: return ( tok = SINGLE_LETTER_ );
       } 
     }
     else 
      switch ( toupper(lexeme[0]) )
      { case 'X': case 'Y': case 'Z': case 'W':
          yylval.i = ((toupper(lexeme[0]) - 'X' + 4) % 4) + 1;
          return COORD_;
      }
    }

    if ( toupper(lexeme[0]) == 'X' )
    { char *c;
      for ( c = lexeme+1 ; isdigit(*c) ; c++ ) ;
      if ( *c == 0 )
      { yylval.i = atoi(lexeme+1);
        if ( yylval.i != 0 )
          return COORD_;
      }
    }

    if ( toupper(lexeme[0]) == 'P' && isdigit(lexeme[1]) && !lexeme[2] )
    { yylval.i = atoi(lexeme+1);
      return PARAM_;
    }

    if ( lexeme[0] == 'g' )
    { char *c;
      for ( c = lexeme+1 ; isdigit(*c) ; c++ ) ;
      if ( *c == 0 )
      { yylval.i = atoi(lexeme+1);
        return GO_COUNT_;
      }
    }

    if ( strncmp(lexeme,"usr",3) == 0 )
    { char *c;
      for ( c = lexeme+3 ; isdigit(*c) ; c++ ) ;
      if ( *c == 0 )
      { yylval.i = atoi(lexeme+3);
        return USERFUNC_;
      }
    }
 
    key.name = lexeme;

#define BSEARCH(namelist)\
    (struct ckey*)bsearch((char*)&key,(char*)(namelist),\
         sizeof(namelist)/sizeof(struct ckey),\
          sizeof(struct ckey),FCAST keyword_compare);

    keyptr = BSEARCH(colornames);
    if ( keyptr )
        { yylval.r = yylval.i = keyptr->token;
          return (tok = INTEGER_);
        }
    /* search math funcs */
    keyptr = BSEARCH(mathfunc_keywords);
    if ( keyptr ) {  yylval.i = keyptr->token; return (tok = MATHFUNC_); }

    keyptr = BSEARCH(mathfunc2_keywords);
    if ( keyptr ) {  yylval.i  = keyptr->token; return (tok = MATHFUNC2_); }

    if ( const_expr_flag )
    { for ( k=0 ; k<sizeof(const_expr_keywords)/ sizeof(struct ckey) ; k++ )
      if (stricmp(lexeme,const_expr_keywords[k].name)==0)
           return (tok = const_expr_keywords[k].token);
    }

    /* kludge for some backward compatibility */
    if ( strcmp(lexeme,"vertexnormal") == 0 )
      strcpy(lexeme,"__vertex_normal");

    if ( datafile_flag && !backquote_flag )
    { keyptr = BSEARCH(datafile_keywords);
      if ( keyptr ) {  return tok = keyptr->token;}
      keyptr = BSEARCH(command_keywords);
      if ( keyptr ) 
      { sprintf(errmsg,
         "Use of the keyword '%s' as an identifier is very ill-advised!\n",
           lexeme);
        kb_error(2328,errmsg, WARNING);
      }
    }
    else /* search keywords and internal variables */
    { 
      keyptr = BSEARCH(command_keywords);
      if ( keyptr ) 
      { if ( kb_stricmp(lexeme,"HELP") == 0 ) help_flag = 1;
        yylval.i = tok = keyptr->token;
        switch ( tok )
        { /* set verb_flag when expecting a command */
          case THEN_: case ELSE_: case DO_: verb_flag = 1; break;
          /* clear verb_flag when expecting an expression */
          case PRINT_: 
          case IF_: case WHILE_: case VIEW_TRANSFORMS_: case TRANSFORM_DEPTH_:
          case METIS_: case KMETIS_: case OMETIS_: case EDGEWEED_:
          case AREAWEED_: case EDGEDIVIDE_: case LANCZOS_: case RITZ_:
          case EIGENPROBE_: case MOVE_: case ZOOM_: case LAGRANGE_: case SET_:
          case PRINTF_: case LIST_: case DELETE_: case VERTEX_AVERAGE_:
          case BINARY_PRINTF_: 
          case DISSOLVE_: case REFINE_: case EDGESWAP_: case FIX_: case UNFIX_:
              case SPRINTF_: case EPRINT_: case HISTOGRAM_: case LOGHISTOGRAM_:
          case FOREACH_: case SHOW_EXPR_: case UNSET_:
          case HESSIAN_SADDLE_: case HESSIAN_SEEK_: case NOTCH_:
          case AUTOCHOP_: case AUTOPOP_: case OPTIMIZE_:
               verb_flag = 0; break;
          case SHOW_: if ( verb_flag ) tok = SHOWVERB_;
                      verb_flag = 0;
                      break;
          case TRANSFORM_EXPR_: if ( verb_flag ) tok = TRANSFORM_EXPR_VERB_;
                      verb_flag = 0;
                      break;
        }
         return tok;
      }
     }

    for ( i = 0 ; i < NUMDATATYPES ; i++ )
      if ( stricmp(lexeme,datatype_name[i]) == 0 )
      { yylval.i = DATATYPE_;
        yylval.datatype = i;
        return tok = DATATYPE_;
      }

    keyptr = BSEARCH(togglenames);
    if ( keyptr ) 
    { yylval.i = keyptr->token;
      if ( verb_flag != 0 ) tok = TOGGLENAME_;
      else tok = TOGGLEVALUE_;
      return tok;
    }
    keyptr = BSEARCH(internal_variables);
    if ( keyptr ) 
    { yylval.i = keyptr->token;
      return tok = INTERNAL_VARIABLE_;
    }

    /* search local variables */
    yylval.i = lookup_local_var(lexeme);
    if ( yylval.i )
    { struct global *g;
      g = globals(yylval.i); 
      if ( g->flags & SUBROUTINE )
        return ( tok = PROCEDURE_ );
      else if ( g->flags & ORDINARY_PARAM )
      { switch ( g->type )
        { case VERTEX_TYPE: 
             yylval.etype = VERTEX;
             return ( tok = ELEMENT_IDENT_ );
          case ELEMENTID_TYPE:
             yylval.etype = -1;
             return ( tok = ELEMENT_IDENT_ );
          case EDGE_TYPE:
             yylval.etype = EDGE;
             return ( tok = ELEMENT_IDENT_ );
          case FACET_TYPE:
             yylval.etype = FACET;
             return ( tok = ELEMENT_IDENT_ );
          case BODY_TYPE:
             yylval.etype = BODY;
             return ( tok = ELEMENT_IDENT_ );
          case FACETEDGE_TYPE:
             yylval.etype = FACETEDGE;
             return ( tok = ELEMENT_IDENT_ );
          default:
            return ( tok = IDENT_ );
        }
      }
      else if ( g->flags & FUNCTION_NAME )
        return ( tok = FUNCTION_IDENT_ );
      else if ( g->flags & PROCEDURE_NAME )
        return ( tok = PROCEDURE_IDENT_ );
      else if ( g->flags & STRINGVAL )
        return ( tok = STRINGGLOBAL_ );
      else if ( g->flags & QUANTITY_NAME )  /* can't happen */
        return ( tok = QUANTITY_NAME_ );
      else if ( g->flags & METHOD_NAME )  /* can't happen */
        return ( tok = METHOD_NAME_ );
      else if ( g->flags & CONSTRAINT_NAME )  /* can't happen */
        return ( tok = CONSTRAINT_NAME_ );
      else if ( g->flags & BOUNDARY_NAME )  /* can't happen */
        return ( tok = BOUNDARY_NAME_ );
      else if ( g->flags & DYNAMIC_LOAD_FUNC )  /* can't happen */
        return ( tok = DYNAMIC_LOAD_FUNC_ );
      else if ( g->flags & ARRAY_PARAM )
        return ( tok = ARRAYIDENT_ );
      else if ( g->flags & GLOB_LOCALVAR )
        return IDENT_;
      else 
        return (tok = NEWIDENT_);
    }

    /* search symbol table */
    s = symbol_lookup(lexeme);
    if ( s ) 
    { yysym = s; yylval.i = yysym-symtable; verb_flag = 0;
      return (tok = SYMBOL_ );
    }

    /* search parameter names */
    yylval.i = lookup_global_hash(lexeme,0,0,HASH_LOOK);
    if ( yylval.i != 0 )
    { int nametype = yylval.i & NAMETYPEMASK;
      struct global *g;
      yylval.i &= INDEXMASK; /* get plain index */

      if ( nametype == QUANTITYNAME )
        return tok = QUANTITY_NAME_;
      else if ( nametype == METHODNAME )
        return tok = METHOD_NAME_;
      else if ( nametype == PERM_NAME )
      { yylval.i |= PERMGLOBAL;
        if ( perm_globals(yylval.i)->flags & SUBROUTINE )
          return ( tok = PERM_PROCEDURE_ );
        else if ( perm_globals(yylval.i)->flags & STRINGVAL )
          return ( tok = PERM_STRINGGLOBAL_ );
        else if ( perm_globals(yylval.i)->flags & ARRAY_PARAM )
          return ( tok = ARRAYIDENT_ );
        else return (tok = PERM_IDENT_);
      }
      yylval.i |= EPHGLOBAL;
      g = globals(yylval.i); 
      if ( g->flags & SUBROUTINE )
        return ( tok = PROCEDURE_ );
      else if ( g->flags & FUNCTION_NAME )
        return ( tok = FUNCTION_IDENT_ );
      else if ( g->flags & PROCEDURE_NAME )
        return ( tok = PROCEDURE_IDENT_ );
      else if ( g->flags & STRINGVAL )
        return ( tok = STRINGGLOBAL_ );
      else if ( g->flags & QUANTITY_NAME )
        return ( tok = QUANTITY_NAME_ );
      else if ( g->flags & METHOD_NAME )
        return ( tok = METHOD_NAME_ );
      else if ( g->flags & CONSTRAINT_NAME )
        return ( tok = CONSTRAINT_NAME_ );
      else if ( g->flags & BOUNDARY_NAME )
        return ( tok = BOUNDARY_NAME_ );
      else if ( g->flags & DYNAMIC_LOAD_FUNC )
        return ( tok = DYNAMIC_LOAD_FUNC_ );
      else if ( g->flags & ARRAY_PARAM )
        return ( tok = ARRAYIDENT_ );
      else 
      { switch ( g->type )
        { case VERTEX_TYPE: 
             yylval.etype = VERTEX;
             return ( tok = ELEMENT_IDENT_ );
          case ELEMENTID_TYPE:
             yylval.etype = -1;
             return ( tok = ELEMENT_IDENT_ );
          case EDGE_TYPE:
             yylval.etype = EDGE;
             return ( tok = ELEMENT_IDENT_ );
          case FACET_TYPE:
             yylval.etype = FACET;
             return ( tok = ELEMENT_IDENT_ );
          case BODY_TYPE:
             yylval.etype = BODY;
             return ( tok = ELEMENT_IDENT_ );
          case FACETEDGE_TYPE:
             yylval.etype = FACETEDGE;
             return ( tok = ELEMENT_IDENT_ );
          default:
            return ( tok = IDENT_ );
        }
      }
    }

    /* search extra attributes */
    for ( type = 0 ; type < NUMELEMENTS ; type++ )
      { struct extra *ex;
        int i;

        for ( i = 0, ex = EXTRAS(type) ; 
           i < web.skel[type].extra_count ; i++ , ex++ )
        if ( stricmp(lexeme, ex->name) == 0 )
        { strncpy(idname,lexeme,sizeof(idname)); /* save text */
          yylval.qnum = i;
          yylval.etype = type;
          return tok = ex->array_spec.dim ? ARRAY_ATTRIBUTE_ : EXTRA_ATTRIBUTE_; 
        }
      }

    /* search dynamic load libraries */
    h = search_libraries(lexeme);
    if ( h )
    { yylval.i = add_global(lexeme);
      globals(yylval.i)->flags |= DYNAMIC_LOAD_FUNC;
      globals(yylval.i)->value.funcptr = h;
      return ( tok = DYNAMIC_LOAD_FUNC_ );
    }

    /* if here, then not keyword */
    strncpy(idname,lexeme,sizeof(idname)); /* save text */
    if ( strlen(lexeme) == 1 )
    { sprintf(errmsg,  
      "Use of single letter command '%c' is illegal here!\n",
      lexeme[0]);
       kb_error(2329,errmsg, WARNING);
       if ( !datafile_flag) return tok = lexeme[0];
    }

   yylval.i = 0;
   return(tok = NEWIDENT_) ;
} /* end identcase() */

/******************************************************************
*
*  function: keywordname()
*
*  purpose: find keyword of given token number. 
*
*/

char *keywordname(toknum)
int toknum;
{  int k,i,imax;

    for ( k = 0 ; k < sizeof(command_keywords)/sizeof(struct ckey) ; k++ )
      if ( toknum == command_keywords[k].token )
      { static char name[32]; 
        strncpy(name,command_keywords[k].name,31);
        return name;
      }
    for ( k = 0 ; k < sizeof(togglenames)/sizeof(struct ckey) ; k++ )
      if ( toknum == togglenames[k].token )
      { static char name[32]; 
        strncpy(name,togglenames[k].name,31);
        return name;
      }
    for ( k = 0 ; k < sizeof(internal_variables)/sizeof(struct ckey) ; k++ )
      if ( toknum == internal_variables[k].token )
      { static char name[32]; 
        strncpy(name,internal_variables[k].name,31);
        return name;
      }

    imax = sizeof(colornames)/sizeof(struct ckey);
    for ( i = 0 ; i < imax ; i++ )
      if ( colornames[i].token == toknum )
        return colornames[i].name;

    imax = sizeof(datafile_keywords)/sizeof(struct dkey);
    for ( i = 0 ; i < imax ; i++ )
      if ( datafile_keywords[i].token == toknum )
        return datafile_keywords[i].name;

    imax = sizeof(mathfunc_keywords)/sizeof(struct ckey);
    for ( i = 0 ; i < imax ; i++ )
      if ( mathfunc_keywords[i].token == toknum )
        return mathfunc_keywords[i].name;

    imax = sizeof(mathfunc2_keywords)/sizeof(struct ckey);
    for ( i = 0 ; i < imax ; i++ )
      if ( mathfunc2_keywords[i].token == toknum )
        return mathfunc2_keywords[i].name;

   /* unfound */
   return tokname(toknum);
}

/****************************************************************************
*
*  function identtype()
*
*  purpose: find type of an identifier. This version used by
*   is_variable() etc. commands.
*/

int identtype(word)
char *word;
{
   int type;
   struct sym *s;
   struct ckey *keyptr;
   struct ckey key;  /* for bsearch key */


   if ( strlen(word) == 1 )
   {
     if ( single_redefine[yylval.i].start ) return tok = SINGLE_REDEFD_;
     switch ( word[0] )
       { case 't': case 'l': case 'j': case 'P': case 'M':
         case 'w': case 'n': case 'm': case 'b':
         case 'k': case 'K': case 'p': case 'y':
           return ( tok = SINGLE_LETTER_ARG_ );
             case 'G': if ( verb_flag ) return tok = SINGLE_LETTER_ARG_;
               else return tok = G_; break;
         default: return ( tok = SINGLE_LETTER_ );
       } 
    }
    key.name = word;
    keyptr = BSEARCH(colornames);
    if ( keyptr )
          return  INTEGER_;

    /* search math funcs */
    keyptr = BSEARCH(mathfunc_keywords);
    if ( keyptr ) {  return MATHFUNC_; }

    keyptr = BSEARCH(mathfunc2_keywords);
    if ( keyptr ) {  return  MATHFUNC2_; }

    /* search keywords and internal variables */
    { 
      keyptr = BSEARCH(command_keywords);
      if ( keyptr ) 
      {  
         return tok;
      }

      keyptr = BSEARCH(togglenames);
      if ( keyptr ) 
           return tok;
      keyptr = BSEARCH(internal_variables);
      if ( keyptr ) { return  INTERNAL_VARIABLE_; }
    }

    /* search local variables */
    yylval.i = lookup_local_var(word);
    if ( yylval.i )
    { struct global *g;
      g = globals(yylval.i); 
      if ( g->flags & SUBROUTINE )
        return ( tok = PROCEDURE_ );
      else if ( g->flags & ORDINARY_PARAM )
        return ( tok = IDENT_ );
      else if ( g->flags & STRINGVAL )
        return ( tok = STRINGGLOBAL_ );
      else if ( g->flags & QUANTITY_NAME )  /* can't happen */
        return ( tok = QUANTITY_NAME_ );
      else if ( g->flags & METHOD_NAME )  /* can't happen */
        return ( tok = METHOD_NAME_ );
      else if ( g->flags & CONSTRAINT_NAME )  /* can't happen */
        return ( tok = CONSTRAINT_NAME_ );
      else if ( g->flags & BOUNDARY_NAME )  /* can't happen */
        return ( tok = BOUNDARY_NAME_ );
      else if ( g->flags & DYNAMIC_LOAD_FUNC )  /* can't happen */
        return ( tok = DYNAMIC_LOAD_FUNC_ );
      else if ( g->flags & ARRAY_PARAM )
        return ( tok = ARRAYIDENT_ );
      else if ( g->flags & GLOB_LOCALVAR )
        return IDENT_;
      else return (tok = NEWIDENT_);
    }

    /* search symbol table */
    s = symbol_lookup(word);
    if ( s ) 
    { yysym = s; yylval.i = yysym-symtable; verb_flag = 0;
      return (tok = SYMBOL_ );
    }

    /* search parameter names */
    yylval.i = lookup_perm_global(word);
    if ( yylval.i >= 0 )
      { 
        if ( perm_globals(yylval.i)->flags & SUBROUTINE )
        return ( tok = PERM_PROCEDURE_ );
        else if ( globals(yylval.i)->flags & STRINGVAL )
        return ( tok = PERM_STRINGGLOBAL_ );
        else return (tok = PERM_IDENT_);
      }

    yylval.i = lookup_global(word);
    if ( yylval.i >= 0 )
      { 
        if ( globals(yylval.i)->flags & SUBROUTINE )
          return ( tok = PROCEDURE_ );
        else if ( globals(yylval.i)->flags & STRINGVAL )
          return ( tok = STRINGGLOBAL_ );
        else if ( globals(yylval.i)->flags & QUANTITY_NAME )
          return ( tok = QUANTITY_NAME_ );
        else if ( globals(yylval.i)->flags & METHOD_NAME )
          return ( tok = METHOD_NAME_ );
        else if ( globals(yylval.i)->flags & DYNAMIC_LOAD_FUNC )
          return ( tok = DYNAMIC_LOAD_FUNC_ );
        else if ( globals(yylval.i)->flags & ARRAY_PARAM )
          return ( tok = ARRAYIDENT_ );
        else return (tok = IDENT_);
      }

    /* search extra attributes */
    for ( type = 0 ; type <= BODY ; type++ )
    { struct extra *ex;
      int i;

      for ( i = 0, ex = EXTRAS(type) ; 
         i < web.skel[type].extra_count ; i++ , ex++ )
      if ( stricmp(word, ex->name) == 0 )
      { strncpy(idname,word,sizeof(idname)); /* save text */
        yylval.qnum = i;
        yylval.etype = type;
        return tok = ex->array_spec.dim ? ARRAY_ATTRIBUTE_ : EXTRA_ATTRIBUTE_; 
      }
    }

    /* if here, then not keyword */
    yylval.i = 0;
    return(tok = NEWIDENT_) ;
} /* end identtype() */

/*********************************************************************
* 
* function: kblex()
*
* purpose: my own lexical analyzer, faster (for Evolver) and 
*          more comprehensible.
*/

/* States */
#define L_START  1
#define L_SLASH  2
#define L_LINECOMMENT 3
#define L_DOT 4
#define L_DIGITS 5
#define L_DIGITS_DOT 6
#define L_WHITESPACE 7
#define L_WHITESIGN 8
#define L_LINESTART 9
#define L_LEAD_INTEGER 10
#define L_LEAD_INTEGER_AT 11
#define L_INTEGER_AT 12
#define L_DIGITS_E 13
#define L_ALPHANUM 14
#define L_COLON 15
#define L_SEMICOLON 16
#define L_PLUS  17
#define L_MINUS 18
#define L_STAR  19
#define L_BANG  20
#define L_PIPE  21
#define L_AND   22
#define L_GREATER  23
#define L_COLONCOLON 24
#define L_SINGLE_QUOTE 25
#define L_HEXNUMBER   26
#define L_ZERO  27
#define L_GRGR  28
#define L_BACKQUOTE 29
#define L_SHARP  30
#define L_COLONCOLONCOLON 31
#define L_QUOTE 32
#define L_EQUAL 33
#define L_LESS  34
#define L_DIGITS_E_DIGITS 35
#define L_LEAD_MINUS 36
#define L_BACKSLASH 37

/* Unput buffer */
int ubuff_max = 0;
int ubuff_spot = 0;  /* index of first vacant spot in unput_buff */
char *unput_buff;

/* the token */
/*char *yytext;*/
int yytext_max;

char kb_input_new()
{ char c;

  if ( ubuff_spot )
  { c = unput_buff[--ubuff_spot];
    unput_buff[ubuff_spot] = 0;
    return c;
  }
  c = rawinput();
  while ( c == 0 )
  { if ( yywrap() ) 
      return 0;
    else
      c = rawinput();
   }
   return c;
}

void kb_unput(c)
char c;
{ if ( ubuff_spot >= ubuff_max )
  { if ( unput_buff )
    { ubuff_max *= 2;
      unput_buff = realloc(unput_buff,ubuff_max);
    }
    else
    { ubuff_max = 16000;
      unput_buff = calloc(ubuff_max,sizeof(char));
    }
  }

  unput_buff[ubuff_spot++] = c;
}

int kblex()
{ int state;
  char nextchar;
  char *yyspot; /* first empty spot in yytext */
  int retval;

  PROF_START(kblex);

  if ( !yytext )
  { yytext_max = 200;
    yytext = calloc(yytext_max,sizeof(char));
  } 

    yyspot = yytext;
    memset(&yylval,0,sizeof(yylval));
    state = tok ? L_START : L_LINESTART;
    
    for(;;)
    { int len = yyspot-yytext;
      if ( len >= yytext_max-2 )
      { yytext_max *= 2;
        yytext = realloc(yytext,yytext_max);
        yyspot = yytext + len;
      }
      *(yyspot++) = nextchar = kb_input_new();
      switch ( state )
      {
        case L_START:
           if ( nextchar == '0' )
             state = L_ZERO;
           else if ( isdigit(nextchar) )
             state = L_DIGITS;
           else if ( isalpha(nextchar) ) 
             state = L_ALPHANUM;
           else
           switch ( nextchar )
           {
             case 0:  tok=0; goto kblex_exit;
             case '\n' : line_no++; yyspot = yytext; state = L_LINESTART;
                         break;
             case ' ': case '\t':
               yyspot--; state = L_WHITESPACE; break;
             case '/':  state = L_SLASH; break;
             case '.':  state = L_DOT; break;         
             case '_':  state = L_ALPHANUM; break;
             case ':':  state = L_COLON; break;
             case ';':  state = L_SEMICOLON; break;
             case '+':  state = L_PLUS; break;
             case '-':  state = L_MINUS; break; 
             case '*':  state = L_STAR; break; 
             case '!':  state = L_BANG; break; 
             case '|':  state = L_PIPE; break; 
             case '&':  state = L_AND; break; 
             case '>':  state = L_GREATER; break;
             case '\'':  state = L_SINGLE_QUOTE; break;
             case '"':  state = L_QUOTE; break;
             case '`':  state = L_BACKQUOTE; break;
             case '=':  state = L_EQUAL; break;
             case '<':  state = L_LESS; break;
             case '\\': state = L_BACKSLASH; break;
             case '#':  state = L_SHARP; break;

             case '(': verb_flag = 0;
                *yyspot = 0;  tok = nextchar; goto kblex_exit;
             case ')':
             case '^':
             case ',':
             case '?':
             case '%':
             case '[':
             case ']':
             case '}': 
             case '@':
                *yyspot = 0;  tok = nextchar; goto kblex_exit;
             case '{': verb_flag = 1;
                *yyspot = 0; tok = nextchar; goto kblex_exit;
             default:  
               sprintf(errmsg,"Illegal character '%c'\n",nextchar);
               kb_error(3658,errmsg,RECOVERABLE);
           }
           break;

        case L_BACKSLASH:
           /* allow trailing whitespace */
           while ( nextchar == ' ' || nextchar == '\t' )
              nextchar = kb_input_new();
           if ( nextchar == '\n' )
           { yyspot = yytext;
             state = L_START;
             line_no++;
             break;
           }
           kb_error(3659,
              "Illegal backslash. Backslash is line continuation character.\n",
                 RECOVERABLE);
           break;

        case L_EQUAL:
           if ( nextchar == '=' )
           { *yyspot = 0;
             tok = EQ_;
             goto kblex_exit;
           }
           kb_unput(nextchar);
           yyspot[-1] = 0;
           tok = (datafile_flag ? '=' : EQ_);
           goto kblex_exit;
           
        case L_QUOTE:
           { /* read quoted string */
             int n;
             in_quote = 1;
             for (n=0;;n++)
             { if ( nextchar == '\n' ) line_no++;
               if ( nextchar == '"' && (n == 0 || (yytext[n-1] != '\\')) ) 
                 break;
               if ( n >= yytext_max - 2 )
               { yytext_max *= 2;
                 yytext = realloc(yytext,yytext_max);
               }
               yytext[n] = nextchar;
               nextchar = kb_input_new();
             }
             yytext[n] = 0;
             reduce_string(yytext);
             in_quote = 0;
             tok = QUOTATION_;
             goto kblex_exit;
           }
           break;
   
        case L_LESS:
           if ( nextchar == '=' )
           { *yyspot = 0;
             tok = LE_;
             goto kblex_exit;
           }
           else
           { kb_unput(nextchar);
             yyspot[-1] = 0;
             tok = '<';
             goto kblex_exit;
           }
           break;

        case L_GREATER:
           if ( nextchar == '>' )
              state = L_GRGR;
           else if ( nextchar == '=' )
           { *yyspot = 0;
             tok = GE_;
             goto kblex_exit;
           }
           else
           { kb_unput(nextchar);
             yyspot[-1] = 0;
             tok = '>';
             goto kblex_exit;
           }
           break;

        case L_GRGR:
           if ( nextchar == '>' )
           { *yyspot = 0;
             tok = REDIRECTOVER_;
             goto kblex_exit;
           } 
           else
           { kb_unput(nextchar);
             yyspot[-1] = 0;
             tok = REDIRECT_;
             goto kblex_exit;
           }
           break;

        case L_SINGLE_QUOTE:
           if ( nextchar == '\'' )
              kb_error(3577,"Empty single quotes.\n",RECOVERABLE);
           if ( nextchar == 0 || nextchar == '\n' )
              kb_error(3570,"Dangling single quote.\n",RECOVERABLE);
           yylval.i = nextchar;
           nextchar = kb_input_new();
           if ( nextchar != '\'' ) 
              kb_error(3583,"Single quote not closed after one character.\n",
                              RECOVERABLE);
           yytext[1] = 0;
           tok = SINGLE_LETTER_;
           goto kblex_exit;
           break;

        case L_BACKQUOTE:
           while ( nextchar == ' ' ) yyspot[-1] = nextchar = kb_input_new();
           if ( nextchar == ',' )
           { *yyspot = 0;
             tok = BACKQUOTE_COMMA_;
             goto kblex_exit;
           }
           yyspot[-1] = 0;
           kb_unput(nextchar);
           tok = '`';
           goto kblex_exit;
           break;

        case L_SHARP:
           while ( isalpha(nextchar) )
              *(yyspot++) = nextchar = kb_input_new();
           yyspot[-1] = 0;
           if ( strcmp(yytext,"#define") == 0 )
           { if (!datafile_flag)
             kb_error(2853,"#define valid only in top of datafile.\n",WARNING);
             macro_flag = 1;
             state = L_START;
             yyspot = yytext;
             break;
           }
           if ( strcmp(yytext,"#include") == 0 )
           { 
             while ( nextchar != '"' ) nextchar = kb_input_new();
             yyspot = yytext;
             do
               *(yyspot++) = nextchar = kb_input_new();
             while ( nextchar != '"' );
             yyspot[-1] = 0;
             push_commandfd(NULL,yytext);
             state = L_LINESTART;
             yyspot = yytext;
             break;
           }
           kb_error(3681,"Illegal # directive.\n",RECOVERABLE);
           break;

        case L_ZERO:
           if ( toupper(nextchar) == 'X' )
             state = L_HEXNUMBER;
           else if ( nextchar == '.' )
             state = L_DIGITS_DOT;
           else if ( isdigit(nextchar) )
             state = L_DIGITS;
           else
           { kb_unput(nextchar);
             yyspot[-1] = 0;
             yylval.i = 0;
             verb_flag = 0;
             tok = INTEGER_;
             goto kblex_exit;
           }
           break;

        case L_HEXNUMBER:
           if ( isdigit(nextchar) ||
                 (toupper(nextchar) >= 'A' && toupper(nextchar) <= 'F') )
              break;
           yyspot[-1] = 0;
           kb_unput(nextchar);
           sscanf(yytext+2,"%x",&yylval.i);
           yylval.r = (REAL)yylval.i;
           verb_flag = 0;
           tok = INTEGER_;
           goto kblex_exit;

        case L_WHITESPACE:
           if ( nextchar == '+' || nextchar == '-' )
           { state = L_WHITESIGN;
             break;
           }
           if ( nextchar == ' ' || nextchar == '\t' )
           { yyspot--; break;
           }
           else
           { state = L_START;
             kb_unput(nextchar);
             yyspot--;
           }
           break;

        case L_WHITESIGN:
           kb_unput(nextchar);
           if ( !isspace(nextchar) && (yytext[0] == '-') )
           { if ( datafile_flag && uminus_flag && (parens == 0) )
             { tok = UMINUS_;
               goto kblex_exit;
             }
           }
           /* else go back and treat as ordinary - */
           yyspot--;
           state = yytext[0] == '+' ? L_PLUS : L_MINUS;
           break;

        case L_LINESTART:
           if ( nextchar == 0 ) 
           { tok = 0;
             goto kblex_exit;
           }
           if ( nextchar == ' ' || nextchar == '\t' )
             yyspot--;
           else if ( isdigit(nextchar) )
             state = L_LEAD_INTEGER;
           else if ( nextchar == '-' )
             state = L_LEAD_MINUS;
           else
           { kb_unput(nextchar);
             state = L_START;
             yyspot = yytext;
           }
           break;
        
        case L_LEAD_MINUS:
           if ( isdigit(nextchar) )
             state = L_LEAD_INTEGER;
           else
           { kb_unput(nextchar);
             state = L_WHITESIGN;
             yyspot--;
           }
           break;

        case L_LEAD_INTEGER:
           if ( isdigit(nextchar) )
             break;
           if ( nextchar == '.' )
           { state = L_DIGITS_DOT;
             break;
           }
           if ( (toupper(nextchar) == 'E') || (toupper(nextchar) == 'D') )
           { state = L_DIGITS_E;
             break;
           }
           if ( nextchar == '@' )
           { state = L_LEAD_INTEGER_AT;
             break;
           }
           /* have LEAD_INTEGER_ */
           kb_unput(nextchar);
           yyspot[-1] = 0;
           yylval.r = atof(yytext);
           yylval.i = atoi(yytext);
           verb_flag = 0;
           if ( lists_flag==LISTS_FULL ) 
           { tok = LEAD_INTEGER_;
             goto kblex_exit;
           }
           else if ( uminus_flag || yytext[0] != '-' )
           { tok = (fabs(yylval.r) > MAXINT) ? REAL_ : INTEGER_;
             goto kblex_exit;
           }
           else 
           { for ( yyspot -= 2 ; yyspot != yytext ; yyspot-- ) 
               kb_unput(*yyspot);
             yytext[1] = 0;
             tok = minus_type(yytext[0]);
             goto kblex_exit;
           }
           break;

        case L_DIGITS:
           if ( isdigit(nextchar) )
             break;
           if ( nextchar == '.' )
           { state = L_DIGITS_DOT;
             break;
           }
           if ( (toupper(nextchar) == 'E') || (toupper(nextchar) == 'D') )
           { state = L_DIGITS_E;
             break;
           }
           if ( nextchar == '@' )
           { state = L_INTEGER_AT;
             break;
           }
           if ( toupper(nextchar) == 'B' )
           { char *c;
             yylval.i = 0;
             for ( c = yytext;  *c=='0' || *c=='1'  ; c++ )
               yylval.i = 2*yylval.i + (*c - '0');
             if ( c == yyspot-1 )
             { yylval.r = (REAL)yylval.i;
               verb_flag = 0;
               tok = INTEGER_;
               goto kblex_exit;
             }
             /* else fall through to ordinary integer */
           }
           /* have INTEGER_ */
           kb_unput(nextchar);
           yyspot[-1] = 0;
           yylval.r = atof(yytext);
           yylval.i = atoi(yytext);
           verb_flag = 0;
           tok = (fabs(yylval.r) > MAXINT) ? REAL_ : INTEGER_;
           goto kblex_exit;

        case L_DIGITS_DOT:
           if ( (toupper(nextchar) == 'E') || (toupper(nextchar) == 'D') )
           { state = L_DIGITS_E;
             break;
           }
           if ( isdigit(nextchar) )
             break;
           if ( isalpha(nextchar) )
           { *yyspot = 0;
             tok = VERSIONTOKEN_;
             goto kblex_exit;
           }
           /* have complete REAL */
           kb_unput(nextchar);
           yyspot[-1] = 0;
           yylval.r = atof(yytext);
           verb_flag = 0;
           tok = REAL_;
           goto kblex_exit;

        case L_DOT:
           if ( isdigit(nextchar) )
           { state = L_DIGITS_DOT;
             break;
           }
           /* else just a dot */
           kb_unput(nextchar);
           yyspot[-1] = 0;
           tok = '.';
           goto kblex_exit;

        case L_LEAD_INTEGER_AT:
           if ( isdigit(nextchar) )
             break;
           /* have token */
           kb_unput(nextchar);
           yyspot[-1] = 0;
           yylval.i = atoi(yytext);
           yylval.qnum = atoi(strchr(yytext,'@')+1);
           if ( yylval.i == 0 )
              kb_error(3554,"Missing task number.",DATAFILE_ERROR);
           verb_flag = 0;
           tok = LEAD_INTEGER_AT_;
           goto kblex_exit;

        case L_INTEGER_AT:
           if ( isdigit(nextchar) )
             break;
           /* have token */
           kb_unput(nextchar);
           yyspot[-1] = 0;
           yylval.i = atoi(yytext);
           yylval.qnum = atoi(strchr(yytext,'@')+1);
           if ( yylval.i == 0 )
              kb_error(3553,"Missing task number.",DATAFILE_ERROR);
           verb_flag = 0;
           tok = INTEGER_AT_;
           goto kblex_exit;

        case L_SLASH:
           switch ( nextchar )
           { case '*': 
              { /* eat comment */
                register int c;
                in_comment = 1;
                state = L_START;
                for ( ; ; )
                { while ( (c = kb_input_new()) != '*' && c != 0 )
                    if ( c == '\n' ) 
                    { line_no++; /* eat up text of comment */
                      state = L_LINESTART;
                    }
                  if ( c == '*' )
                  { while ( (c = kb_input_new()) == '*' ) ;
                    if ( c == '/' ) break;    /* found the end */
                    if ( c == '\n' ) 
                    { line_no++;
                      state = L_LINESTART;
                    }
                  }
                  if ( c == 0 && yywrap() )
                  { kb_error(2861,"End-of-file in comment\n",RECOVERABLE ); 
                    break; 
                  }
                }
                in_comment = 0;
                yyspot = yytext;
              }
              break;
            
            case '/': state = L_LINECOMMENT; break;
            case '=': *yyspot = 0; yylval.i = DIVASSIGN_; 
                       verb_flag = 0;
                       tok=ASSIGNOP_;
                       goto kblex_exit;
                       
            default: kb_unput(nextchar); 
                     yyspot[-1] = 0;
                     verb_flag = 0;
                     tok = '/';
                     goto kblex_exit;
         }
         break;  /* end L_SLASH */

        case L_DIGITS_E:
          if ( isdigit(nextchar) || nextchar == '+' || nextchar == '-' )
          { state = L_DIGITS_E_DIGITS;
            break; 
          }
          kb_error(3664,"Missing exponent for scientific notation.\n",
             RECOVERABLE);
          break;

        case L_DIGITS_E_DIGITS:
          if ( isdigit(nextchar) ) 
            break;
          /* have token */
          kb_unput(nextchar);
          yyspot[-1] = 0;
          yylval.r = atof(yytext);
          verb_flag = 0;
          tok = REAL_;
          goto kblex_exit;

        case L_ALPHANUM:
          if ( isalnum(nextchar) || nextchar == '_' )
            break;
          /* have token */
          kb_unput(nextchar);
          yyspot[-1] = 0;
          strncpy(yylval.lexeme,yytext,31);
          retval = macro();
          if ( retval == WAS_MACRO_DEF ) 
          { /* macro() ate up rest of line */
            state = L_LINESTART;
            yyspot = yytext;
          }
          else if ( retval == MACRO_EXPANDED )
          { state = L_START;
            yyspot = yytext;
          }
          else
          {
            tok = identcase(yytext);
            goto kblex_exit;
          }
          break;

        case L_COLON:
          if ( nextchar == '=' )
          { verb_flag = 2; yyspot[-1] = 0; 
            tok = ASSIGN_;
            goto kblex_exit;
          }
          if ( nextchar == ':' )
          { state = L_COLONCOLON;
            break;
          }
          kb_unput(nextchar);
          yyspot[-1] = 0;
          if ( cond_expr_flag )
          { tok = ':';
            goto kblex_exit;
          }
          /* else treat as whitespace */
          yyspot = yytext;
          state = L_START;
          break;

        case L_COLONCOLON:
          if ( nextchar == '=' )
          { verb_flag = 2; yyspot[-1] = 0; 
            tok = PERM_ASSIGN_;
            goto kblex_exit;
          }
          if ( nextchar == ':' )
          { state = L_COLONCOLONCOLON;
            break;
          }
          kb_error(3568,"Illegal token '::'\n",RECOVERABLE);
          break;

        case L_COLONCOLONCOLON:
          if ( nextchar == '=' )
          { verb_flag = 2; yyspot[-1] = 0; 
            tok = REDEFINE_;
            goto kblex_exit;
          }
          kb_error(3569,"Illegal token ':::'\n",RECOVERABLE);
          break;

        case L_SEMICOLON:
          if ( nextchar == '=' )
          { verb_flag = 2; 
            kb_error(2868,"You mistyped ';=' for ':='?\n",WARNING);
            tok= ASSIGN_;
            goto kblex_exit;
          }
          kb_unput(nextchar);
          yyspot[-1] = 0;
          verb_flag = 1;
          tok = ';';
          goto kblex_exit;
 
        case L_PLUS:
          verb_flag = 0;
          if ( nextchar == '=' )
          { verb_flag = 0; *yyspot = 0;
            yylval.i = PLUSASSIGN_;
            tok = ASSIGNOP_;
            goto kblex_exit;
          }
          kb_unput(nextchar);
          yyspot[-1] = 0;
          tok = '+';
          goto kblex_exit;
          
        case L_MINUS:
          verb_flag = 0;
          if ( nextchar == '=' )
          { verb_flag = 0; yylval.i = SUBASSIGN_; 
            *yyspot = 0;
            tok= ASSIGNOP_;
            goto kblex_exit;
          }
          verb_flag = 0; 
          kb_unput(nextchar);
          tok = minus_type('-');
          goto kblex_exit;

        case L_STAR:
          verb_flag = 0;
          if ( nextchar == '=' )
          { verb_flag = 0; *yyspot = 0;
            yylval.i = MULTASSIGN_;
            tok = ASSIGNOP_;
            goto kblex_exit;
          }
          if ( nextchar == '*' )
          { verb_flag = 0; *yyspot = 0; 
            tok = '^';
            goto kblex_exit;
          }
          kb_unput(nextchar);
          yyspot[-1] = 0;
          tok = '*';
          goto kblex_exit;

        case L_BANG:
           verb_flag = 0;
           if ( nextchar == '=' )
           { *yyspot = 0;
             tok = NE_;
             goto kblex_exit;
           }
           kb_unput(nextchar);
           yyspot[-1] = 0;
           tok = NOT_;
           goto kblex_exit;

        case L_PIPE:
           if ( nextchar == '|' )
           { *yyspot = 0;
             tok = OR_;
             goto kblex_exit;
           }
           kb_unput(nextchar);
           yyspot[-1] = 0;
           tok = PIPE_; 
           goto kblex_exit;

        case L_AND:
           if ( nextchar == '&' )
           { *yyspot = 0;
             tok = AND_;
             goto kblex_exit;
           }
           kb_error(3566,"Illegal token '&'\n",RECOVERABLE);
          break;

        case L_LINECOMMENT:
          if ( (nextchar == '\n') || (nextchar == 0) )
          { line_no++;
            state = L_LINESTART; 
            yyspot = yytext;
            break;
          }
          yyspot--;  /* don't save comment token */
          break;

        default: 
          fprintf(stderr,"Unhandled state: %d\n",state);
          *yyspot = 0;
          tok = 1;
          goto kblex_exit;
      }
    }
  kblex_exit:
  PROF_FINISH(kblex);
 
  return tok;
}
