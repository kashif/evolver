/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakkf, brakke@susqu.edu                 *
*************************************************************/
 
/***************************************************************
*
*  File:    variable.c
*
*  Purpose: allocate storage for global variables of evolver.
*
*/

#include "include.h"

#ifdef  __cplusplus
extern "C" {
#endif

char *evolver_version = "2.30c";
char needed_version[30]; 

#ifdef mpi_evolver 
char *version = "version 2.30c, January 15, 2008; compiled for mpi.";
#else
#ifdef SGI_MULTI
char *VERSION = "Version 2.30c, January 15, 2008; SGI multiprocessing"; 
#else
#ifdef _WIN64
char *VERSION = "Version 2.30c, January 15, 2008 Windows 64-bit, OpenGL";
#else
#ifdef WIN32
char *VERSION = "Version 2.30c, January 15, 2008; Windows 32-bit, OpenGL";
#else
char *VERSION = "Version 2.30c, January 15, 2008";
#endif
#endif
#endif
#endif


char *typenames[NUMELEMENTS] = {"vertex","edge","facet","body","facetedge"};

REAL factorial[20] = {1.,1.,2.,6.,24.,120.,720.,7*720.,8*7*720.,9*8*7*720.,
  9.*8*7*7200,9.*8*7*7200*11,9.*8*7*7200*11*12,9.*8*7*7200*11*12*13,
  9.*8*7*7200*11*12*13*14,9.*8*7*7200*11*12*13*14*15,
  9.*8*7*7200*11*12*13*14*15*16,9.*8*7*7200*11*12*13*14*15*16*17,
  9.*8*7*7200*11*12*13*14*15*16*17*18,9.*8*7*7200*11*12*13*14*15*16*17*18*19};

int broken_pipe_flag; /* so output routines will know to quit */
int function_kludge_flag; /* handling errors in function def in datafile */

int this_task = 1; /* machine identifier for MPI; really task number */
  /* 1 works as default when not using MPI */
int mpi_subtask_command_flag; /* whether command is running on task
     as subcommand from master instead of running isolated */
int mpi_local_bodies_flag; 

#ifdef MPI_EVOLVER
int match_id_flag = 1; /* so remote references to locals work */
#else
int match_id_flag = 0; /* to make id match datafile number, option -i */
#endif
int echo_flag; /* whether to echo stdin; for piped input */
struct optparam_t optparam[MAXOPTPARAM]; 
int optparamcount;  /* number thereof */
REAL **optparam_congrads;  /* constraint gradients */

/* controlling edge and facet deletion options */
int star_finagling; /* extra bad configuration detection */
int force_deletion; /* even if it seems a bad idea */

/* my own ctypes */
char kb_upper_array[256];
char kb_lower_array[256];

REAL machine_eps;  /* smallest machine resolution at 1.0  */
char loadfilename[PATHSIZE]; /* for LOAD command */
int addload_flag;  /* if doing ADDLOAD command */
jmp_buf loadjumpbuf;  /* for LOAD command */
int exit_flag;  /* whether to end this command interpreter */
int hessian_subshell_flag; /* whether in hessian subshell, so can't menu  */
int subshell_depth; /* nesting depth of subshells */

int file_no; /* number of current source file */
int file_no_used; /* number in list */
int file_no_max; /* number allocated */
char **file_names; /* names of source files */

char *curdir;  /* current directory */

/* for binary_printf byte order */
int big_endian_flag;
int little_endian_flag;

struct text_s text_chunks[MAXTEXTS];
int display_text_count;

char *current_prompt; /* prompt string being displayed */
char *cmdfilename; /* for saving command line read file */
int uminus_flag; /* whether to interpret " -" as UMINUS */
int facet_general_flag;
int everything_quantities_flag;  /* for pure quantity version */
int auto_convert_flag = 1; /* whether to automatically convert_to_quantities */
int show_all_quantities;  /* to display default quantities also */
int option_q = 0;  /* record command line option  -q, default off */
int random_seed = 1;  /* seed for random number generators */
int keep_macros_flag; /* to preserve macros after datafile. */
int macro_count;  /* number of macros defined */
char  *macro_subs;  /* string space for substitution strings */
struct macro  *macros;  /* dynamically allocated */
char *warning_messages;  /* for storing warning messages */
size_t warning_messages_max; /* allocated bytes for warning_messages */
int warning_messages_new; /* number of new warning messages since last looked */
int warnings_suppressed[MAXSUPPRESS];
int warnings_suppressed_count;
int shadow_warn_flag; /* warn if locals shadow existing names */
int do_show_flag; /* for Mac kludge graphics command prompt */

int dont_resize_flag; /* set if datafile specified view matrix */
REAL brightness = 0.65; /* midlevel gray for screen display */
int markedgedrawflag = 0; /* for single-drawing edges */

struct constraint null_constraint; /* non-constraint placeholder */

struct vgradblock *vgradbase;    /* allocated list block header start */
int    vgradtop;     /* number of first free  structure */
long      vgradmax;     /* number allocated */
long    vgradlastused;     /* number actually used last time round */
int volgrads_every_flag; /* whether recalc volgrads every projection iteration */
int zener_drag_flag; /* whether to do zener drag */

int HOMDIM = 4;
REAL **to_focus;  /* used only by oglgraph.c at present */
REAL **from_focus;
int lazy_transforms_flag;  /* let graphics display handle symmetry */

char *dymem; /* dynamic memory region */
int  dymemsize; /* size of dynamic memory region */
char *areaname;  /* length or area */
int read_command_flag;  /* whether commands at end of datafile */
int verb_flag;    /* set if lex looking for a verb */
int cond_expr_flag; /* set if parser looking for ':' in conditional expr */
int read_wrap_flag; /* set when reading wraps in datafile */
int quiet_flag; /* whether to display normal output */
int quiet_go_flag; /* whether to display normal g output */
int quiet_load_flag; /* whether to display output while loading file */
int exit_after_error; /* auto exit flag */ 
int exit_after_warning; /* auto exit flag */ 
int break_after_warning; /* auto break flag */ 
int last_error;  /* number of last error */
int change_flag;  /* set during command that changes surface */
int assigntype;  /* type of assignment operator */
char *cmdptr;    /* current command or input for parsing */
int memdebug;  /* flag for memory debugging in run_checks() */
int single_step_debugging; /* flag for debug stop at next line */
int check_count;      /* number of errors returned by run_checks() */
int itdebug;    /* flag for iteration debugging */
int nprocs = 1;     /* number of parallel processors */
int procs_requested = 1; /* number of processes desired */
REAL proc_total_area[MAXPROCS]; /* for individual processes */
int logfile_flag; /* whether logging in progress */
char logfilename[PATHSIZE];
FILE *logfilefd;
int keylogfile_flag; /* whether keylogging in progress */
char keylogfilename[PATHSIZE];
FILE *keylogfilefd;

int web_checksum = 0; /* to see if web needs sending */
int dymem_checksum = 0; /* see if dymem needs sending */
int comp_quant_vertex; /* during calc_quant_grad */
int comp_quant_vertexi; /* during calc_quant_hess */
int comp_quant_vertexj; /* during calc_quant_hess */
int comp_quant_type; /* during calc_quant_grad */
int comp_quant_stamp; /* coordination with eval_all and eval_sec */
vertex_id comp_quant_vi; /* during calc_quant_hess */
vertex_id comp_quant_vj; /* during calc_quant_hess */

int gocount = 1;          /* number of iterations left */ 
#if defined(MAC_APP) || defined(MAC_CW)
int go_display_flag = 1; /* for displaying each change */
#else
int go_display_flag = 0; /* for displaying each change */
#endif
int shading_flag = 1;  /* for facet shading by orientation */
int color_flag = 1;    /* facet coloring by user */
int rgb_colors_flag;  /* enabling rgb color scheme */
int edge_rgb_color_attr; /* number of rgb color attribute */
int facet_rgb_color_attr; /* number of rgb color attribute */
int facet_rgb_backcolor_attr; /* number of rgb color attribute */
int smooth_graph_flag; /* whether to smoothly plot in lagrange mode */
int graph_capabilities;  /* bits to describe what current graphics device
                               can do */
int edge_alpha_flag; /* whether edges have alpha channel */
int facet_alpha_flag; /* whether facets have alpha channel */
int facetback_alpha_flag; /* whether facetbacks have alpha channel */
int background_color = WHITE;  /* graphics background */
int backcull_flag = 0;  /* 3D graphics backculling */
int setting_backcull = 0; 

/* slicing view */
int slice_view_flag;
REAL slice_coeff[MAXCOORD+2];
int slice_coeff_global;

/* clipping view */
int clip_view_flag;
int clip_coeff_global;
REAL clip_coeff[MAXCLIPS][MAXCOORD+2];


#ifdef IRIS
int gv_binary_flag = 1;  /* whether to do geomview in binary */
#else
int gv_binary_flag = 0;  /* whether to do geomview in binary */
#endif
int self_similar_flag;    /* for self-similar motion */
REAL string_curve_tolerance; /* quadratic string model smoothness, deg */
int labelflag; /* whether ps doing labels */
/* Following PostScript line width variables relative to page size */
REAL ps_labelsize;  /* default label size */
REAL ps_stringwidth;  /* default edge width */
REAL ps_fixededgewidth;
REAL ps_tripleedgewidth;
REAL ps_conedgewidth;
REAL ps_bareedgewidth;
REAL ps_gridedgewidth;
int full_bounding_box_flag; /* for window bounding box */
int geomview_bug_flag; /* for geomview 1.6.1 picking bug */
int gv_pipe[2]; /* for pipe for reading geomview pick commands */
vertex_id *vpicklist; /* for geomview picking */
facet_id *fpicklist;
int pickvnum,pickenum,pickfnum,pickbnum; /* geomview picks */
int new_vertex_id,new_edge_id,new_facet_id,new_body_id; /* just created elements */
int gv_vect_start; /* vector vertex start in vpicklist */
int circular_arc_flag; /* whether to draw edges as arcs */
int spherical_arc_flag; /* whether to draw edges as spherical arcs */

element_id junk; /* for MSC bug */
#ifndef PARALLEL_MACHINE
element_id xx_id;  /* for macro temporary to prevent multiple evaluation */
element_id x1_id,x2_id,x3_id,x4_id,x5_id,x6_id,x7_id,x8_id,x9_id;
element_id xa_id,xb_id,xc_id,xd_id,xe_id,xf_id,xg_id,xh_id;
/*so nested macros don't tromp each other*/
#endif

#ifdef __WIN32__
extern unsigned _stklen = 0x3000;  /* get bigger stack */
#endif

/* dynamic load library list */
struct dll dll_list[MAX_DLL];

/* global variable stuff */
int dy_global_hash_max;      /* allocated entries */
int dy_global_hash_maxfill;  /* entries before expanding */
int dy_global_hash_used;     /* entries actually in use */
struct global *Globals; /* handy for debugging */
struct global *perm_Globals; /* handy for debugging */
int old_global_count; /* for error recovery */
int old_perm_global_count; /* for error recovery */
int proc_timestamp; /* for ordering procedure definitions */
int perm_flag;  /* for whether permanent assignment parsing in place */
int reading_comp_quant_flag;  
int cur_quant;  /* when reading compound quantity */
int compound_hess_flag; /* mode while doing compound hessian */
int quantities_only_flag; /* for using named quantities only */
int calc_quant_flag; /* set when quantity calculation underway */
int gravity_quantity_num;  /* number of quantity for default gravity */
int gap_quantity_num;  /* number of quantity for default gap energy */
int default_area_quant_num; /* number of quantity for default area */
char length_method_name[100];  /* for replacing default */
int  length_method_number;  /* for replacing default */
char area_method_name[100];  /* for replacing default */
char volume_method_name[100];  /* for replacing default */
int dirichlet_flag;  /* to do area hessian with Dirichlet hessian */
int sobolev_flag;  /* to do area hessian with Sobolev hessian */

FILE *logfd = NULL;  /* command log file */
int read_depth;  /* for nested reads */
int include_depth;  /* for #include */
FILE *commandfd = NULL;  /* command input file */
struct cmdfile cmdfile_stack[NESTDEPTH],datafile_stack[NESTDEPTH];
char *history_space;  /* for command history */
int  history_offsets[MAXHISTORY];
int  history_number;    /* number of current command */
int history_count;  /* number in list */
char fulltext[MAXCMDSIZE+5]; /* for full text of commands */
size_t  fulltextsize;              /* length of command */
char datafilename[PATHSIZE];  /* current datafile name */
char filename[PATHSIZE];  /* file name in command */
int datafile_flag;  /* 1 for datafile, 0 for command so expression
                                        parser knows what's up */
int datafile_input_flag; /* whether further input available from datafile */
int topflag;  /* 1 while in datafile top section */
int backquote_flag; /* 1 while in backquoted command; kludge */
int lists_flag;  /* set when parsing space-separated lists */
int const_expr_flag;  /* 1 for const_expr, 0 for command so expression
                                        parser knows what's up */
int boundary_expr_flag; /* so parser knows when parsing boundary */
int use_given_id; /* iterator expression should not use local, for SHOW_ */
/* for redefining single letter commands */
struct expnode single_redefine[128];

extern int reading_elements_flag; /* so parser knows attributes should not
     be accepted in expressions */
FILE *data_fd;
FILE *outfd;     /* where normal output is to go */
int estimate_flag;    /* for toggling estimate of energy decrease */
REAL estimated_change; /* stored result */
int autorecalc_flag; /* for toggling autorecalc after variable assign  */
int verbose_flag;    /* for lots of messages */
int parens;             /* level of parenthesis nesting */
int brace_depth;             /* level of brace nesting */
int in_quote;                /* toggle for lexer */
int in_function;                /* toggle for lexer */
int in_comment;                /* toggle for lexer */
int in_control_structure;                /* toggle for lexer */
int autopop_flag;     /* whether to do autopopping */
int autopop_quartic_flag; /* scale = (length)^4, say surface diffusion */
int immediate_autopop_flag; /* set if pop before motion */
int pop_disjoin_flag;  /* whether cones to be disjoined rather than merged */
int pop_enjoin_flag;  /* whether cones to be enjoined rather than disjoined */
int pop_to_edge_flag;  /* control which way popping goes */
int pop_to_face_flag;  /* control which way popping goes */
int autochop_flag;     /* whether to do autochopping */
REAL autochop_length;  /* max edge length for autochop */
int autopop_count;  /* number of edges found */
int autochop_count;  /* number of edges found */
int kraynikpopvertex_flag;  /* for special popping of a certain cone */
int kraynikpopedge_flag;  /* for special popping of a certain edges */
int effective_area_flag; /* use quadratic form for area around vertex */
int old_area_flag;     /* on for using old effective area */
int runge_kutta_flag;  /* whether to use runge-kutta method for motion */
REAL total_time;      /* total scale factor */
REAL star_fraction;  /* weighting factor for star around vertices */
int area_fixed_flag;    /* for fixed area constraint */
REAL area_fixed_target;  /* target value for fixed area */
REAL area_fixed_pressure; /* Lagrange multiplier */
int hessian_by_diff_flag; /* for crude hessian */
int hessian_quiet_flag=1;  /* 1 to suppress hessian warnings */
int hessian_normal_flag=1; /* 1 for hessian constrained to normal */
int hessian_normal_perp_flag; /* 1 for hessian metric to normal */
int hessian_special_normal_flag; /*  for user spec of Hessian direction */
struct expnode hessian_special_normal_expr[MAXCOORD];
int hessian_normal_one_flag; /* 1 for hessian constrained to 1D normal */
int hessian_double_normal_flag; /* for double dimension perturbation */
REAL hessian_slant_cutoff; /* for treating constrained vertices as
     fixed with hessian_normal */
int hessian_linear_metric_flag; /* linear interp dot product */
REAL linear_metric_mix = .50;  /* proportion of linear interp metric */
REAL quadratic_metric_mix = 1.0;  /* proportion of quadratic interp metric */
int min_square_grad_flag = 0; /* what to minimize in hessian_line_seek */
int hess_move_con_flag=1; /* whether to project to global constraints in move */
                                  /* projecting seems to be good idea */
REAL last_hessian_scale; /* from hessian_line_seek() */
REAL last_eigenvalue;    /* from eigenprobe and stuff */
struct hess_entry *hashtable;  /* the table */
int table_size;  /* hashtable size */
int hash_per_row = 5;  /* for estimating table size */
int vhead_attr;  /* number of vertex attribute for vhead index */
int bhead_attr;  /* number of body attribute for vhead index */


/* for controlling steps */
int rhs_flag;     /* filling in right hand side */
int hess_flag;    /* filling in hessian matrix */
int negdiag;      /* C index of most negative entry on diagonal */
struct linsys Met; /* vector-to-form  metric */

/* Flag for doing sparse rank update for function quantities, to 
   avoid dense hessians */
int quantity_function_sparse_flag;


#if defined(USEYSMP) || defined(MPI_EVOLVER)
int ysmp_flag=YSMP_FACTORING;  /* set if doing Yale Sparse Matrix version */
/* factor matrix */
void (*sp_factor_func)ARGS((struct linsys *)) = ysmp_factor;
/* solve given rhs */
void (*sp_solve_func)ARGS((struct linsys *,REAL *,REAL *)) = ysmp_solve;
/* solve multiple given rhs */
void (*sp_solve_multi_func)ARGS((struct linsys*,REAL**,REAL**,int)) = ysmp_solve_multi;
/* matrix inner product with hessian inverse as metric */
void (*sp_CHinvC_func)ARGS((struct linsys *)) = sp_CHinvC;
#else
/* use mindeg since doing sparse_constraints */
int ysmp_flag=MINDEG_FACTORING;  /* Hessian problem in Borland LONGDOUBLE version */
/* factor matrix */
void (*sp_factor_func)ARGS((struct linsys *)) = xmd_factor;
/* solve given rhs */
void (*sp_solve_func)ARGS((struct linsys *,REAL *,REAL *)) = xmd_solve;
/* solve multiple given rhs */
void (*sp_solve_multi_func)ARGS((struct linsys*,REAL**,REAL**,int)) = xmd_solve_multi;
/* matrix inner product with hessian inverse as metric */
void (*sp_CHinvC_func)ARGS((struct linsys *)) = sp_CHinvC;

#endif

/* for mindeg control */
int mindeg_debug_level;
int mindeg_margin = 5;
int mindeg_min_region_size;

int sparse_constraints_flag = 0; /* whether to store constraint gradients
               in sparse format */
int blas_flag; /* whether to use BLAS */
int augmented_hessian_flag = -1; /* whether to use augmented Hessian */
                                /* -1 for unset */
int augmented_hessian_mode; /* set during factoring */
REAL BKalpha = 0.525; /* single/REAL pivot ratio */
int BK_flag=0;  /* for enabling Bunch-Kaufman version of sparse factoring */
/* sparse matrix function pointers, for easy switching among algorithms */
/* multiply vector by original sparse matrix */
void (*sp_mul_func)ARGS((struct linsys *, REAL*,REAL*)) = bk_mul;
/* convert raw Hessian data to standard sparse format */
void (*sp_AIJ_setup_func)ARGS((int,struct linsys*))
    = bk_AIJ_setup;
/* set up  matrices needed for handling constraints */
void (*sp_constraint_setup_func)ARGS((int,struct linsys *)) 
    = bk_constraint_setup;
/* set up projection to constraints using hessian metric */
void (*sp_hess_project_setup_func)ARGS((struct linsys *)) 
    = BK_hess_project_setup;
/* optional ordering of vertices */
void (*sp_ordering_func)ARGS((struct linsys *)) = NULL;


int eigen_pos,eigen_neg,eigen_zero;  /* inertia of shifted hessian */
int pos_def_warning_flag; /* to suppress a proliferation of warnings */
int make_pos_def_flag;    /* force hessian to positive definiteness */
int mat_index; /* number of negatives on diagonal */
int mat_null; /* number of zeroes on diagonal */
REAL hessian_epsilon = 1e-10; /* cutoff for diagonal elements */
REAL hessian_epsilon_default = 1e-10; 
int post_project_flag;     /* project to quant constr after each motion */
int normal_motion_flag; /* for motion allowed only along normals */
pt_type *vertex_normals;  /* for storage of normals by ordinal */
struct expnode mobility_formula;
int mobility_flag; /* whether mobility in effect */
struct expnode mobility_tensor[MAXCOORD][MAXCOORD];
int mobility_tensor_flag; /* whether tensor mobility in effect */
int check_pinning_flag;  /* for vertices changing constraints */
int ackerman_flag;  /* whether doing phase space motion */
int one_sided_present; /* whether any one-sided constraints exist */
int one_sided_lagrange_attr;
int raw_velocity_attr; /* for use by one-sided constraints */

/* for Dennis DeTurck unit normal motion */
int unit_normal_flag;
REAL deturck_factor = 1.0;  /* weight for unit normal */

int int_val;
REAL real_val;
int attr_etype; /* for ATTRIBUTE parse */
int subtype; /* for INDEXED_SUBTYPE parse */
int coord_num;  /* communication from parser to makenode */ 
int tok;     /* current token from yylex */
int aggrtype;  /* aggregate type being parsed */
int aggregate_depth; /* nesting depth of aggregate loops */
int attr_kind; /* kind of attribute being parsed */
struct sym *elsym;  /* name of element during parsing */
struct sym *yysym;  /* name of identifier from lex */
char *default_name = "q_id"; /* for unnamed elements */
char last_name[50]; /* name of last element generator */
char idname[35]; /* for saving yytext */
char set_extra_name[100]; /* for saving name */

/* useful for debugging */
struct extra *Extras[NUMELEMENTS];

int F_TAG_ATTR; 
int F_PHASE_ATTR;
int B_PHASE_ATTR;
int V_BOUNDARY_ATTR;
int E_BOUNDARY_ATTR;
int F_BOUNDARY_ATTR;

struct expnode torus_period_expr[MAXCOORD][MAXCOORD];
struct expnode torus_display_period_expr[MAXCOORD][MAXCOORD];
int torus_display_mode;  /* default, raw, connected, clipped */

/* squared curvature as part of energy */
struct v_curve_t *v_curve;
struct e_curve_t *e_curve; 
int sqcurve_ignore_constr; /* set if to count fixed and constrained verts */
int square_curvature_flag;  /* set if to be counted */
int square_curvature_param;  /* which parameter for modulus */
int mean_curvature_param;    /* which parameter for modulus */
int mean_curv_int_quantity_num;    /* for everything quantities */
int sq_mean_curv_quantity_num; /* for everything quantities */
int kusner_flag;    /* set for edge square curvature */
int assume_oriented_flag; /* for orientation checking */
int boundary_curvature_flag; /* whether to include boundary vertices */
int conf_edge_curv_flag; /* set for conformal edge curvature squared */
int sqgauss_flag;  /* for squared gaussian curvature */
int sqgauss_param;    /* which parameter for modulus */
int normal_sq_mean_curvature_mi = -1;  /* which is which */
int eff_area_sq_mean_curvature_mi = -1;
int sq_mean_curvature_mi = -1;
int mix_sq_mean_curvature_mi = -1;
int star_normal_sq_mean_curvature_mi;
int star_perp_sq_mean_curvature_mi;
int star_eff_area_sq_mean_curvature_mi;
int star_sq_mean_curvature_mi;
int h0_attr;  /* attribute number for h_zero, if any */

REAL target_length;  /* for string model */
int check_increase_flag;  /* to detect blowups */
int approx_curve_flag;  /* if approximate curvature in effect */
int mean_curv_int_flag;  /* for unsquared mean curvature */
int normal_curvature_flag; /* choice of curvature formula */
int div_normal_curvature_flag; /* choice of curvature formula */
int marked_edge_attr;

REAL *f_sums;  /* facet_knot_energy, for sums to all other vertices */

/* homothety target value, set when homothety toggled on */
REAL homothety_target;

int scrollbuffersize=25; /* output console lines */
char *msg;      /* for constructing user messages */
int msgmax;     /* length allocated */
char errmsg[ERRMSGSIZE];  /*  for kb_error() routine */

jmp_buf jumpbuf[MAXCMDDEPTH];    /* for error recovery  */
jmp_buf cmdbuf;    /* for command error recovery  */
jmp_buf m_jumpbuf[MAXPROCS];    /* for multiproc error recovery  */
jmp_buf graphjumpbuf;  /* for errors during MS graphing */
#ifdef PTHREADS
pthread_t draw_thread_id; /* for graphics thread */
#else
unsigned int draw_thread_id; /* for graphics thread */
#endif
int this_thread;  /* debugging THREADBLOCK */
int parse_error_flag;  /* set when parser hits error */
int recovery_flag;      /* set while recovering from parsing error */
int parse_errors;     /* for counting errors */
int breakflag;      /* set by user interrupt */
int iterate_flag;  /* so handler knows when iteration in progress */
struct oldcoord saved; /* for old coordinates */

/* conjugate gradient stuff */
int  conj_grad_flag;  /* whether conjugate gradient in effect */
REAL cg_oldsum;  /* total grad*grad from previous step */
REAL (*cg_hvector)[MAXCOORD] = NULL;  /* saved direction vector */
REAL cg_gamma;    /* direction adjustment factor */
int  ribiere_flag; /* to do Polak-Ribiere version */

element_id *el_list[NUMELEMENTS]; /* for list of active elements */
long global_timestamp;  /* universal clock */
long web_timestamp;  /*  when stuff changed */
long graph_timestamp;  /* so graph routines know when surface changed */
long top_timestamp;    /* timestamp for topology changes */
long vedge_timestamp = -1; /* for vertex edgelist currency */
long vfacet_timestamp = -1; /* for vertex facetlist currency */
long bfacet_timestamp = -1; /* for body facetlist currency */
long fixed_volume_timestamp; /* for volume calculation */
long info_volume_timestamp; /* for volume calculation */
long reset_timestamp;  /* when surface loaded */
int need_fe_reorder_flag; /* setting causes recalc to fe_reorder */
int parallel_update_flag[NUMELEMENTS];  /* set when element info changed */
/* graphing flags */
int init_flag; /* whether graphics initialized */
int bdry_showflag = 1;  /* whether to show facets on boundary */
int no_wall_flag;        /* whether to suppress wall facets */
int normflag = 0;
int thickenflag = 0;
int innerflag = 0;
int outerflag = 0;
int colorflag = 0;
int OOGL_flag = 0;
int geomview_flag;    /* whether geomview initialized */
int geompipe_flag;    /* for pipe only */
int edgeshow_flag = 1;    /* whether to show edges of facets */
int triple_edgeshow_flag;    /* whether to show triple edges  */
REAL thickness = 0.0;  /* for thickening double-sided surfaces */
int user_thickness_flag = 0; /* whether user has specified thickness */

/* edge widths */
/* in order BARE_EDGE, FIXED_EDGE, CONSTRAINT_EDGE, BOUNDARY_EDGE,
   SINGLE_EDGE, TRIPLE_EDGE, and  other */
REAL pswidths[] = {0.005,0.004,0.004,0.004,0.004,0.003,0.002};
REAL xwidths[] = {0.001,0.001,0.001,0.001,0.001,0.001,0.001};
REAL otherwidths[] = {0.005,0.005,0.005,0.005,0.005,0.005,0.005};
REAL *edgewidths=otherwidths;

int bare_edge_count;  
REAL facet_alpha = 1.0;  /* global transparency */
int view_4D_flag = 0;  /* 0 for doing 3D xyz projection graphics, */
                                    /* 1 for outputting 4D graphics */

IColor rgb_colors[16] = {  /* rgba */ {0.0,0.0,0.0,1.},{0.0,0.0,1.,1.},
 {0.0,1.,0.0,1.},{0.0,1.,1.,1.}, {1.,0.0,0.0,1.},{1.,0.0,1.,1.},
 {1.,0.5,0.,1.},{.6,.6,.6,1.},{.3,.3,.3,1.},{.3,.8,1.,1.}, {.5,1.,.5,1.},
 {.5,1.,1.,1.},{1.,.5,.5,1.},{1.,.5,1.,1.},{1.,1.,.0,1.},{1.,1.,1.,1.} };

/* query variables */
int condition_flag;     /* whether query has condition expression */
struct expnode *show_expr[NUMELEMENTS];  /* for element show expressions */
struct expnode show_command[NUMELEMENTS];  /* for dump */
struct expnode show_expr_table[NUMELEMENTS];  /* actual expressions */
int celement;             /* type of element for query */
int query_intval;
REAL query_realval;
int set_query_type;
ATTR set_query_attr;
int query_coord;

/* graphing stuff */
char cmapname[100];  /* colormap file name */ 
maprow *colormap; /* rgba colormap, values 0 to 1 */
int fillcolor; /* current polygon fill color */
int box_flag = 0;  /* whether or not to show outline box */
int ridge_color_flag;  /* whether to differently color */
int visibility_test; /* whether to do after depth sort */
REAL  overall_size;  /* for anybody who wants to know how big */
/* bounding box, for PostScript */
REAL bbox_minx,bbox_miny,bbox_maxx,bbox_maxy;
int need_bounding_box;  /* flag to tell painter.c to calculate */
/* clip window for PostScript and painter algorithms */
REAL minclipx=-1.5,maxclipx=1.5,minclipy=-1.5,maxclipy=1.5;
 
/* 3D bounding box */
REAL bounding_box[MAXCOORD][2];

/* vertex for zooming in on; default is first one read in */
int zoom_number = 1;

/* for inner clipping for making zoom pictures */
int inner_clip_flag = 0;    /* don't clip by default */
REAL inner_clip_rad = 0.0; /* show everything by default */

REAL volume_factorial = 1.0;  /* simplex volume factor */
int subsimplex[1<<MAXCOORD][MAXCOORD];  /* for refining simplices */

/* handy identity matrix, set up in init_view */
REAL **identmat; 

/* graph function pointers */
#if defined(NOPROTO)
void (*graph_start)() = null_function;
void (*graph_end)() = null_function;
void (*graph_edge )() = null_function;  /* called to graph one triangle */
void (*graph_facet)() = null_function;
void (*display_edge )() = null_function;
void (*display_facet)() = null_function;
void (*init_graphics)() = null_function;
void (*finish_graphics)() = null_function;
void (*close_graphics)() = null_function;
#else
void (*graph_start)(void) = null_function; 
void (*graph_edge )(struct graphdata *,edge_id) = 
     (void (*)(struct graphdata *,edge_id))null_function; 
void (*graph_facet)(struct graphdata *,facet_id) = 
     (void (*)(struct graphdata *,facet_id))null_function; 
void (*graph_end)(void) = null_function;    
void (*display_edge )(struct tsort *) = 
     (void (*)(struct tsort *))null_function;
void (*display_facet)(struct tsort *) = 
     (void (*)(struct tsort *))null_function;
void (*init_graphics)(void) = null_function;
void (*finish_graphics)(void) = null_function;
void (*close_graphics)(void) = null_function;
#endif

FILE * savefd;     /* for binary dump of web */

/* general parameters */
REAL **view;  /* transformation matrix */ 
int datafile_view_flag; /* whether datafile had view matrix */
int steps = 1; 
int energy_init;      /* to keep track if we have current config energy */
int movie_init;        /* whether movie routine initialized */


int view_matrix_global; /* global id number */
int torus_periods_global;
int inverse_periods_global;
REAL window_aspect_ratio;  /* 0 if not set */
int eigenvalues_list_global; /* for accessing ritz() output list */ 


REAL wulff_vector[MAXWULFF][MAXCOORD];  /* the vector components */
#ifdef NOPROTO
void (*get_wulff)();
#else
void (*get_wulff)(REAL *,REAL *);
#endif

int interp_bdry_param; /* interp or extrap bdry param */
int extra_bdry_attr; /* when extra boundary for vertices */
int extra_bdry_param_attr; /* corresponding parameter values */

REAL **phase_data;  /* phase boundary energies */
int  phase_flag;     /* if phase boundary data in effect */
char phase_file_name[PATHSIZE];  /* for dump */
int phasemax;  /* number of phases */    

/* additional viewing transforms */
int transform_count;  /* number of transforms, including identity */
REAL ***view_transforms;
int view_transforms_global; /* global var number */
int *view_transform_det; /* to see if normals need flipping */
int transforms_flag; /* whether to show transforms */
int transform_gen_count;
expnodearray *view_transform_gens_expr;    /* generator expressions */
REAL ***view_transform_gens;    /* generator values */
char transform_expr[100];  /* save it */
int transform_depth;  /* tree depth in transform generation */
int *transform_colors;
int view_transform_swap_colors_global; /* global var number */
int *transform_parity;
int *transform_gen_swap;
int transform_colors_flag;


int fixed_constraint_flag;  /* set if restoring force valid */
REAL **leftside;  /* volume projection matrix (torus model) */
REAL **rleftside;  /* volume projection matrix (for vol restore) */
REAL *rightside;  /* right side of volume gradient constraints */
REAL *pressures; /* multiples of gradients to subtract from force */
int pressure_set_flag; /* pressures have been given values */
REAL *vol_deficit; /* bodyu volume deficits */
REAL *vol_restore; /* volume restoring gradient coefficients */
int no_refine = 0;    /* keyword NOREFINE in data file header sets this
                         to disable initial triangulation of triangular
                         initial faces. */

/* interpolation structures */
int reflevel = 0;  /* refinement level */
REAL extrap_val[MAXLEVEL]; /* energy value at refinement level */


/* model dependent function pointers */
#ifdef NOPROTO
void (*calc_facet_energy)() = facet_energy_l;
void (*calc_facet_forces)()  = facet_force_l;
void (*calc_facet_volume)() = facet_volume_l;
void (*calc_edge_energy)() = edge_energy_l;
void (*calc_edge_forces)()  = edge_force_l;
void (*calc_edge_area)() = edge_area_l;
void (*string_grad)() = string_grad_l;
void (*film_grad)() = film_grad_l;
#else
/* void (*calc_facet_energy)(facet_id) = facet_energy_l;*/
/* void (*calc_facet_forces)(facet_id) = facet_force_l; */
void (*calc_facet_energy)(facet_id,int) =  facet_energy_l;
void (*calc_facet_forces)(facet_id) = facet_force_l;
void (*calc_facet_volume)(facet_id) = facet_volume_l;
void (*calc_edge_energy)(edge_id) = edge_energy_l;
void (*calc_edge_forces)(edge_id) = edge_force_l;
void (*calc_edge_area)(edge_id) = edge_area_l;
void (*string_grad)(void) = string_grad_l;
void (*film_grad)(void) = film_grad_l;
#endif

/* gaussian integration on [0,1] */
int gauss1D_num; /* number of integration points */
REAL *gauss1Dpt;
REAL *gauss1Dwt;
REAL **gauss1poly;  /* interp polys at gauss pts */
REAL **gauss1polyd; /* and derivatives              */
                  /* values for linear or quadratic model, whichever in effect */
int  edge_ctrl=2; /* control points on edge, 2 linear, 3 quadratic */

REAL **conical_x;
REAL *conical_w;

int bezier_flag; /* whether to do Bezier basis polynomials in Lagrange */
REAL **bezier1invert[MAXLAGRANGE];
REAL **bezier1revert[MAXLAGRANGE];
REAL **bezier_refine_1d[MAXLAGRANGE];
REAL **bezier2invert[MAXLAGRANGE];
REAL **bezier2revert[MAXLAGRANGE];
REAL **bezier_refine_2d[MAXLAGRANGE];

/* new version for gauss-lagrange elements */
struct gauss_lag *gauss_lagrange[MAXCOORD];
int maxgaussorder[MAXCOORD]; /* allocated for each dimension */

int set_by_user_gauss_1D;  /* minimums set by user */
int set_by_user_gauss_2D;

/* cubature over triangles */
/* points given in barycentric coordinates */
/* weights total to 1 */

/* names end with polynomial degree done exactly */

REAL gauss2Dpt1[1][3] = { {(REAL)1/3.0, (REAL)1/3.0, (REAL)1/3.0} };
REAL gauss2Dwt1[1]     = { 1.0 };

REAL gauss2Dpt2[3][3] = { {(REAL)2/3.0, (REAL)1/6.0, (REAL)1/6.0}, 
                          {(REAL)1/6.0,(REAL)2/3.0,(REAL)1/6.0},
                          {(REAL)1/6.0, (REAL)1/6.0, (REAL)2/3.0} };
REAL gauss2Dwt2[3]     = { (REAL)1.0/3, (REAL)1.0/3, (REAL)1.0/3 };

/* point indexing for degree 5 7-point cubature:
         |\
         |2\ 
         |    \
         |4 0 5\
         |       \
         |3   6   1\
         ------------


*/

#ifdef LONGDOUBLE
#define ROOT15 3.8729833462074168851792653997824L
#else
#define ROOT15 3.8729833462074168851792653997824
#endif

/* point coordinates */
#define Q1  (9 + 2*ROOT15)/21.0
#define Q2  (9 - 2*ROOT15)/21.0
#define Q3  (6 - ROOT15)/21.0
#define Q4  (6 + ROOT15)/21.0

REAL gauss2Dpt5[7][3] = { {(REAL)1/3.0, (REAL)1/3.0, (REAL)1/3.0}, 
    {Q3,Q1,Q3}, {Q3,Q3,Q1},
    {Q1,Q3,Q3}, {Q4,Q2,Q4}, {Q2,Q4,Q4}, {Q4,Q4,Q2}};

/* point weights */
#define W1  (REAL)270/1200.0
#define W2  (155 - ROOT15)/1200.0
#define W3  (155 + ROOT15)/1200.0

REAL gauss2Dwt5[7] = { W1, W2, W2, W2, W3, W3, W3 };

#define Z0 (1-X0-Y0)
#define Z1 (1-X1-Y1)
#define Z2 (1-X2-Y2)
#define Z3 (1-X3-Y3)
#define Z4 (1-X4-Y4)
#define Z5 (1-X5-Y5)
#define Z6 (1-X6-Y6)
#define Z7 (1-X7-Y7)
#define Z8 (1-X8-Y8)
#define Z9 (1-X9-Y9)
#define Z10 (1-X10-Y10)
#define Z11 (1-X11-Y11)
#define Z12 (1-X12-Y12)

/* Following data from J. N. Lyness & D. Jespersen, Moderate Degree
Symmetric Quadrature Rules for the Triangle, J. Inst. Math Applics 15 (1975),
19-32. Table 4, p. 31-32 in particular. Note their weights must be
divided by multiplicity. */

#ifdef LONGDOUBLE

/* Degree 6, 12 point rule */

REAL gauss2Dpt6[12][3] = {
    {5.014265096581716452598916981622520e-01L,2.492867451709183298036131820303480e-01L,2.492867451709100249364951198073999e-01L},
    {2.492867451709029078515338975777059e-01L,2.492867451709029087596082200592039e-01L,5.014265096581941833888578823630887e-01L},
    {2.492867451709183270497538554128221e-01L,5.014265096581716436014191018962773e-01L,2.492867451709100293488270426909022e-01L},
    {8.738219710169944965483631057423612e-01L,6.308901449150063227060230390799104e-02L,6.308901449150487118103459034964771e-02L},
    {6.308901449150118289948940474325112e-02L,6.308901449150118062397376635675358e-02L,8.738219710169976364765368288999961e-01L},
    {6.308901449150063052654827028098232e-02L,8.738219710169944969331104478455354e-01L,6.308901449150487254034128187348069e-02L},
    {6.365024991213878806775528486102556e-01L,5.314504984482153366497370826580484e-02L,3.103524510337905856574734431239388e-01L},
    {5.314504984481301853852016593456701e-02L,3.103524510337810024162297298232822e-01L,6.365024991214059790452501042421485e-01L},
    {6.365024991214020861075296803426151e-01L,3.103524510337816253444925085740704e-01L,5.314504984481628854797781108331449e-02L},
    {5.314504984482153344934219603940619e-02L,6.365024991213878755832859576703452e-01L,3.103524510337905909673718462902471e-01L},
    {3.103524510337816190237041333136396e-01L,6.365024991214020904494434773200222e-01L,5.314504984481629052685238936633823e-02L},
    {3.103524510337810090903747435196749e-01L,5.314504984481301939183175756123539e-02L,6.365024991214059715177934989190889e-01L},
 };

REAL gauss2Dwt6[12] = {
  1.167862757263768807179050181734018e-01L,
  1.167862757263843389483416568714488e-01L,
  1.167862757263768784096221602729326e-01L,
  5.084490637020761505324985831039298e-02L,
  5.084490637020522089450596003542061e-02L,
  5.084490637020761481505460912478438e-02L,
  8.285107561837945338854543136636368e-02L,
  8.285107561836744468304776994936136e-02L,
  8.285107561837382557721317499207620e-02L,
  8.285107561837945396508366849107151e-02L,
  8.285107561837382739566637451872796e-02L,
  8.285107561836744615176431789401854e-02L,
 };
/* End degree 6, 12 point rule */

/* Degree 8, 16 point rule */

REAL gauss2Dpt8[16][3] = {
    {8.141482341455367819699534994722419e-02L,4.592925882927231954528529484765580e-01L,4.592925882927231263501517015762163e-01L},
    {4.592925882927231475552831519982072e-01L,4.592925882927231440671578886916433e-01L,8.141482341455370837755895931014647e-02L},
    {4.592925882927231895384329284653822e-01L,8.141482341455367725255260372755380e-02L,4.592925882927231332090144678070624e-01L},
    {8.989055433659380503601403587955802e-01L,5.054722831703098198068783486597387e-02L,5.054722831703096765917180633844595e-02L},
    {5.054722831703097612348075243852114e-02L,5.054722831703097747109655923190572e-02L,8.989055433659380464054226883295770e-01L},
    {5.054722831703098257424395170177590e-02L,8.989055433659380504838956492830988e-01L,5.054722831703096694186039901512568e-02L},
    {8.394777409957582885939168507211434e-03L,7.284923929554043342528491158985918e-01L,2.631128296346380828612117155941962e-01L},
    {7.284923929554042417294993455100135e-01L,2.631128296346381446917400333744423e-01L,8.394777409957613578760621115544193e-03L},
    {8.394777409957621342164678161115858e-03L,2.631128296346381119629070781745168e-01L,7.284923929554042666949282436643645e-01L},
    {7.284923929554043315683241846756702e-01L,8.394777409957581437665567662234137e-03L,2.631128296346380869940102476620962e-01L},
    {2.631128296346381047270423024925753e-01L,8.394777409957622165712741454623092e-03L,7.284923929554042731072449560528032e-01L},
    {2.631128296346381492938033402499032e-01L,7.284923929554042400931564293706084e-01L,8.394777409957610613040230379491482e-03L},
    {3.333333333333333298756845531938201e-01L,3.333333333333333295405972146583049e-01L,3.333333333333333405837182321478719e-01L},
    {6.588613844964795996881977667912005e-01L,1.705693077517601833650453623478371e-01L,1.705693077517602169467568708609624e-01L},
    {1.705693077517602184835385126924616e-01L,1.705693077517602218799588702268691e-01L,6.588613844964795596365026170806708e-01L},
    {1.705693077517601874682269645273959e-01L,6.588613844964796009415386071000160e-01L,1.705693077517602115902344283725850e-01L},
 };

REAL gauss2Dwt8[16] = {
  9.509163426728462211812551727237322e-02L,
  9.509163426728463040475131002633975e-02L,
  9.509163426728462185881148584352696e-02L,
  3.245849762319807950285367629838385e-02L,
  3.245849762319808200538225570309642e-02L,
  3.245849762319807942454185302686576e-02L,
  2.723031417443497909034662162999206e-02L,
  2.723031417443500130682974809145410e-02L,
  2.723031417443500345941207712305268e-02L,
  2.723031417443497840073343436442744e-02L,
  2.723031417443500356949877575284559e-02L,
  2.723031417443499976224748349027387e-02L,
  1.443156076777871682510911104929244e-01L,
  1.032173705347182493943310821168220e-01L,
  1.032173705347182521427179733806271e-01L,
  1.032173705347182493083255953869918e-01L,
 };
/* End degree 8, 16 point rule */

/* Degree 11, 28 point rule */

REAL gauss2Dpt11[28][3] = {
    {3.333333333333331934268876397513019e-01L,3.333333333333331885493480074000521e-01L,3.333333333333336180237643528486460e-01L},
    {9.480217181434248520796703701066877e-01L,2.598914092828746905227163743279928e-02L,2.598914092828767886805799246051302e-02L},
    {2.598914092828750250330683735768600e-02L,2.598914092828750379810174479386825e-02L,9.480217181434249936985914178484461e-01L},
    {2.598914092828747233043276628489387e-02L,9.480217181434248503391185984344709e-01L,2.598914092828767733044863528063680e-02L},
    {8.114249947041541057105114602872123e-01L,9.428750264792309152262594776976324e-02L,9.428750264792280276686259194302444e-02L},
    {9.428750264792294205688057283929199e-02L,9.428750264792293976562531942692914e-02L,8.114249947041541181774941077337804e-01L},
    {9.428750264792308535346571246913308e-02L,8.114249947041541039428774073510201e-01L,9.428750264792281070365688017984295e-02L},
    {1.072644996557223364032624816075547e-02L,4.946367750172142541655246489091777e-01L,4.946367750172135121941491029300675e-01L},
    {4.946367750172138356569288298990328e-01L,4.946367750172138492217506010880465e-01L,1.072644996557231512132056901292381e-02L},
    {4.946367750172142603206405430763140e-01L,1.072644996557223283969745611789061e-02L,4.946367750172135068396620008057998e-01L},
    {5.853132347709768611618492370226948e-01L,2.073433826145115476405318676892802e-01L,2.073433826145115911976188952880251e-01L},
    {2.073433826145112929736995849484435e-01L,2.073433826145112917691542485510629e-01L,5.853132347709774152571461665004905e-01L},
    {2.073433826145115514898445652372987e-01L,5.853132347709768556041881135437627e-01L,2.073433826145115929059673212189386e-01L},
    {1.221843885990157953731678025662522e-01L,4.389078057004918473935962589894976e-01L,4.389078057004923572332359384442463e-01L},
    {4.389078057004920432774888717532916e-01L,4.389078057004920362033436105583186e-01L,1.221843885990159205191675176883897e-01L},
    {4.389078057004918488377262692852548e-01L,1.221843885990157929582029620989605e-01L,4.389078057004923582040707686157848e-01L},
    {5.408103218906657362601826963019979e-16L,8.588702812826352260744592701671935e-01L,1.411297187173642331152188391670671e-01L},
    {8.588702812826362357651429254730832e-01L,1.411297187173635550314485531809786e-01L,2.092034085213459382564508841867072e-16L},
    {5.894249514555971772314561308509348e-16L,1.411297187173634389158630737803813e-01L,8.588702812826359716591854706224420e-01L},
    {8.588702812826352167254126247917169e-01L,5.485432261907201156555876498731513e-16L,1.411297187173642347313611844881671e-01L},
    {1.411297187173634349207716824714842e-01L,5.874425033169882478828708473648979e-16L,8.588702812826359776367250005402653e-01L},
    {1.411297187173635660995910351738468e-01L,8.588702812826362190772882187544370e-01L,2.148231207460717161826532062140323e-16L},
    {4.484167758913065515852111947696719e-02L,6.779376548825896179727440496157620e-01L,2.772206675282797268687348309072728e-01L},
    {6.779376548825898385104404272923703e-01L,2.772206675282796771900376174655809e-01L,4.484167758913048429952195524204881e-02L},
    {4.484167758913036917234558642451321e-02L,2.772206675282798439120486928988149e-01L,6.779376548825897869156057206766723e-01L},
    {6.779376548825896200572030778593289e-01L,4.484167758913065590118296194845361e-02L,2.772206675282797240416139601922167e-01L},
    {2.772206675282798453349125084658788e-01L,4.484167758913036763892763447779735e-02L,6.779376548825897870261598570563270e-01L},
    {2.772206675282796746112973376755915e-01L,6.779376548825898366894562613921071e-01L,4.484167758913048869924640093230133e-02L},
 };

REAL gauss2Dwt11[28] = {
  8.797730116223213625904988896318388e-02L,
  8.744311553736135385838049393708602e-03L,
  8.744311553736090794487689890661033e-03L,
  8.744311553736135935154623749934445e-03L,
  3.808157199393497346581425946179796e-02L,
  3.808157199393491720019759919271446e-02L,
  3.808157199393497407163683580908803e-02L,
  1.885544805613116782560199663095834e-02L,
  1.885544805613122017842534912285657e-02L,
  1.885544805613116733245378375032056e-02L,
  7.215969754473946942516404696914968e-02L,
  7.215969754473939736240422078066309e-02L,
  7.215969754473947002805481957656930e-02L,
  6.932913870553579423103200089818814e-02L,
  6.932913870553584007834656866484519e-02L,
  6.932913870553579379933829198672931e-02L,
  7.362383783300737021130213746707554e-03L,
  7.362383783300639352618619656117652e-03L,
  7.362383783300721760772621976445149e-03L,
  7.362383783300738927505111793492639e-03L,
  7.362383783300721157944991554547067e-03L,
  7.362383783300641212494778410640677e-03L,
  4.105631542928855018130200663369261e-02L,
  4.105631542928853705855728958269623e-02L,
  4.105631542928847132751023104548306e-02L,
  4.105631542928854963469030740534873e-02L,
  4.105631542928847121120155516746667e-02L,
  4.105631542928853778127224818600921e-02L,
 };
/* End degree 11, 28 point rule */
#else
/* Xn,Yn,Zn are barycentric coord, Wn weight */

#undef W1
#undef W2
#undef W3

/* Degree 6, 12 point. L & J #61 */
#define W1 3.503588271790222e-01/3
#define W2 1.525347191106164e-01/3
#define W3 4.971064537103575e-01/6
#define X1 5.014265096581342e-01
#define Y1 2.492867451709329e-01
#define X2 8.738219710169965e-01
#define Y2 6.308901449150177e-02
#define X3 6.365024991213939e-01
#define Y3 5.314504984483216e-02

REAL gauss2Dpt6[12][3] = {
     {X1,Y1,Z1},{Y1,Z1,X1},{Z1,X1,Y1},
     {X2,Y2,Z2},{Y2,Z2,X2},{Z2,X2,Y2},
     {X3,Y3,Z3},{Y3,Z3,X3},{Z3,X3,Y3},{X3,Z3,Y3},{Y3,X3,Z3},{Z3,Y3,X3}
        };
REAL gauss2Dwt6[12] = { W1, W1, W1, W2, W2, W2, W3, W3, W3, W3, W3, W3 };

#undef X1
#undef X2
#undef X3
#undef Y1
#undef Y2
#undef Y3
#undef W1
#undef W2
#undef W3

/* Degree 8, 16 point. L & J #81 */
#define W0 1.443156076777862e-01/1
#define W1 2.852749028018549e-01/3
#define W2 9.737549286959440e-02/3
#define W3 3.096521116041552e-01/3
#define W4 1.633818850466092e-01/6
#define X0 1.0/3
#define X1 8.141482341455413e-02
#define X2 8.989055433659379e-01
#define X3 6.588613844964797e-01
#define X4 8.394777409957211e-03
#define Y0 1.0/3
#define Y1 4.592925882927229e-01
#define Y2 5.054722831703103e-02
#define Y3 1.705693077517601e-01
#define Y4 7.284923929554041e-01

REAL gauss2Dpt8[16][3] = { {X0,Y0,Z0},
     {X1,Y1,Z1},{Y1,Z1,X1},{Z1,X1,Y1},
     {X2,Y2,Z2},{Y2,Z2,X2},{Z2,X2,Y2},
     {X3,Y3,Z3},{Y3,Z3,X3},{Z3,X3,Y3},
     {X4,Y4,Z4},{Y4,Z4,X4},{Z4,X4,Y4},{X4,Z4,Y4},{Y4,X4,Z4},{Z4,Y4,X4}
        };
REAL gauss2Dwt8[16] = { W0, W1, W1, W1, W2, W2, W2, W3, W3, W3, 
                                W4, W4, W4, W4, W4, W4 };

#undef X0
#undef X1
#undef X2
#undef X3
#undef X4
#undef Y0
#undef Y1
#undef Y2
#undef Y3
#undef Y4
#undef W0
#undef W1
#undef W2
#undef W3
#undef W4

/* degree 11, 28 point. L & J #12 */
#define W0 8.797730116222190e-02
#define W1 2.623293466120857e-02/3
#define W2 1.142447159818060e-01/3
#define W3 5.656634416839376e-02/3
#define W4 2.164790926342230e-01/3
#define W5 2.079874161166116e-01/3
#define W6 4.417430269980344e-02/6
#define W7 2.463378925757316e-01/6
#define X0 1./3
#define X1 9.480217181434233e-01
#define X2 8.114249947041546e-01
#define X3 1.072644996557060e-02
#define X4 5.853132347709715e-01
#define X5 1.221843885990187e-01
#define X6 0.0
#define X7 4.484167758913055e-02
#define Y0 1.0/3
#define Y1 2.598914092828833e-02
#define Y2 9.428750264792270e-02
#define Y3 4.946367750172147e-01
#define Y4 2.073433826145142e-01
#define Y5 4.389078057004907e-01
#define Y6 8.588702812826364e-01
#define Y7 6.779376548825902e-01

REAL gauss2Dpt11[28][3] = { {X0,Y0,Z0},
     {X1,Y1,Z1},{Y1,Z1,X1},{Z1,X1,Y1},
     {X2,Y2,Z2},{Y2,Z2,X2},{Z2,X2,Y2},
     {X3,Y3,Z3},{Y3,Z3,X3},{Z3,X3,Y3},
     {X4,Y4,Z4},{Y4,Z4,X4},{Z4,X4,Y4},
     {X5,Y5,Z5},{Y5,Z5,X5},{Z5,X5,Y5},
     {X6,Y6,Z6},{Y6,Z6,X6},{Z6,X6,Y6},{X6,Z6,Y6},{Y6,X6,Z6},{Z6,Y6,X6},
     {X7,Y7,Z7},{Y7,Z7,X7},{Z7,X7,Y7},{X7,Z7,Y7},{Y7,X7,Z7},{Z7,Y7,X7}
        };
REAL gauss2Dwt11[28] = {W0, W1, W1, W1, W2, W2, W2, W3, W3, W3, W4, W4, W4,
    W5, W5, W5, W6, W6, W6, W6, W6, W6, W7, W7, W7, W7, W7, W7 };
#endif

barytype *gauss2Dpt = gauss2Dpt5;  /* default */
REAL *gauss2Dwt = gauss2Dwt5;  /* default */

#undef X0
#undef X1
#undef X2
#undef X3
#undef X4
#undef X5
#undef X6
#undef X7
#undef Y0
#undef Y1
#undef Y2
#undef Y3
#undef Y4
#undef Y5
#undef Y6
#undef Y7
#undef W0
#undef W1
#undef W2
#undef W3
#undef W4
#undef W5
#undef W6
#undef W7
/* Following degree 13, 37 point formula from TOMS 706 */
/* J. Berntsen and T. Espelid, ACM TOMS 18 (1992) 329-342 */

/*  The abscissas are given in homogeneous coordinates. */
#ifdef LONGDOUBLE
#define X0 0.333333333333333333333333333333L
#define X1 0.950275662924105565450352089520L
#define X2 0.171614914923835347556304795551L
#define X3 0.539412243677190440263092985511L
#define X4 0.772160036676532561750285570113L
#define X5 0.009085399949835353883572964740L
#define X6 0.062277290305886993497083640527L
#define X7 0.022076289653624405142446876931L
#define X8 0.018620522802520968955913511549L
#define X9 0.096506481292159228736516560903L
#define X10 0.851306504174348550389457672223L
#define X11 0.689441970728591295496647976487L
#define X12 0.635867859433872768286976979827L
#define Y0 0.333333333333333333333333333333L
#define Y1 0.024862168537947217274823955239L
#define Y2 0.414192542538082326221847602214L
#define Y3 0.230293878161404779868453507244L
#define Y4 0.113919981661733719124857214943L
#define Y5 0.495457300025082323058213517632L
#define Y6 0.468861354847056503251458179727L
#define Y7 0.851306504174348550389457672223L
#define Y8 0.689441970728591295496647976487L
#define Y9 0.635867859433872768286976979827L
#define Y10 0.022076289653624405142446876931L
#define Y11 0.018620522802520968955913511549L
#define Y12 0.096506481292159228736516560903L
/*  Weights of the degree 13 cubature rule. */
#define W0 0.051739766065744133555179145422L
#define W1 0.008007799555564801597804123460L
#define W2 0.046868898981821644823226732071L
#define W3 0.046590940183976487960361770070L
#define W4 0.031016943313796381407646220131L
#define W5 0.010791612736631273623178240136L
#define W6 0.032195534242431618819414482205L
#define W7 0.015445834210701583817692900053L
#define W8 0.017822989923178661888748319485L
#define W9 0.037038683681384627918546472190L
#define W10 0.015445834210701583817692900053L
#define W11 0.017822989923178661888748319485L
#define W12 0.0370386836813846279185464721L
#else
/*  The abscissas are given in homogeneous coordinates. */
#define X0 0.333333333333333333333333333333
#define X1 0.950275662924105565450352089520
#define X2 0.171614914923835347556304795551
#define X3 0.539412243677190440263092985511
#define X4 0.772160036676532561750285570113
#define X5 0.009085399949835353883572964740
#define X6 0.062277290305886993497083640527
#define X7 0.022076289653624405142446876931
#define X8 0.018620522802520968955913511549
#define X9 0.096506481292159228736516560903
#define X10 0.851306504174348550389457672223
#define X11 0.689441970728591295496647976487
#define X12 0.635867859433872768286976979827
#define Y0 0.333333333333333333333333333333
#define Y1 0.024862168537947217274823955239
#define Y2 0.414192542538082326221847602214
#define Y3 0.230293878161404779868453507244
#define Y4 0.113919981661733719124857214943
#define Y5 0.495457300025082323058213517632
#define Y6 0.468861354847056503251458179727
#define Y7 0.851306504174348550389457672223
#define Y8 0.689441970728591295496647976487
#define Y9 0.635867859433872768286976979827
#define Y10 0.022076289653624405142446876931
#define Y11 0.018620522802520968955913511549
#define Y12 0.096506481292159228736516560903
/*  Weights of the degree 13 cubature rule. */
#define W0 0.051739766065744133555179145422
#define W1 0.008007799555564801597804123460
#define W2 0.046868898981821644823226732071
#define W3 0.046590940183976487960361770070
#define W4 0.031016943313796381407646220131
#define W5 0.010791612736631273623178240136
#define W6 0.032195534242431618819414482205
#define W7 0.015445834210701583817692900053
#define W8 0.017822989923178661888748319485
#define W9 0.037038683681384627918546472190
#define W10 0.015445834210701583817692900053
#define W11 0.017822989923178661888748319485
#define W12 0.037038683681384627918546472190
#endif

REAL gauss2Dpt13[37][3] = { {X0,Y0,Z0},
     {X1,Y1,Z1},{Y1,Z1,X1},{Z1,X1,Y1},
     {X2,Y2,Z2},{Y2,Z2,X2},{Z2,X2,Y2},
     {X3,Y3,Z3},{Y3,Z3,X3},{Z3,X3,Y3},
     {X4,Y4,Z4},{Y4,Z4,X4},{Z4,X4,Y4},
     {X5,Y5,Z5},{Y5,Z5,X5},{Z5,X5,Y5},
     {X6,Y6,Z6},{Y6,Z6,X6},{Z6,X6,Y6},
     {X7,Y7,Z7},{Y7,Z7,X7},{Z7,X7,Y7},
     {X8,Y8,Z8},{Y8,Z8,X8},{Z8,X8,Y8},
     {X9,Y9,Z9},{Y9,Z9,X9},{Z9,X9,Y9},
     {X10,Y10,Z10},{Y10,Z10,X10},{Z10,X10,Y10},
     {X11,Y11,Z11},{Y11,Z11,X11},{Z11,X11,Y11},
     {X12,Y12,Z12},{Y12,Z12,X12},{Z12,X12,Y12}
        };
REAL gauss2Dwt13[37] = {W0, W1, W1, W1, W2, W2, W2, W3, W3, W3, W4, W4, W4,
    W5, W5, W5, W6, W6, W6, W7, W7, W7,
    W8, W8, W8, W9, W9, W9, W10, W10, W10, W11, W11, W11, W12, W12, W12 
};


/* general dimension gauss integration variables and arrays */
int ctrl_num;  /* number of control points */
int gauss2D_num; /* number of integration points */
REAL **gpoly;    /* interpolation polynomial value at integration point k
                         for polynomial of control point j */
REAL ***gpolypartial; /* partials of interpolation polynomials */

REAL **metric;  /* metric values at a point */
REAL ***metric_partial; /* partial derivatives of metric */
REAL **det_array;    /* tangent vector dot products */
int klein_metric_flag;
int metric_convert_flag; /* whether to do form-to-vector conversion */

/* symmetry group stuff */
char *symmetry_name;
SYM_WRAP *sym_wrap;
int       sym_flags;
SYM_FORM *sym_form_pullback;
SYM_INV  *sym_inverse;
SYM_COMP *sym_compose;

#ifdef SHARED_MEMORY
/* process ids */
int proc_ids[MAXPROCS];
int mpflag;  /* whether multiprocessing in action */
int m_breakflag[MAXPROCS]; /* for user interrupts */

/* locks and stuff */
#ifdef SGI_MULTI
usptr_t *lock_arena;  /* arena where locks are */
ulock_t locklist[_MAXLOCKS];  /* only 4096 available */
char lock_arena_name[] = "/tmp/lock_arena"; /* for usinit() */
#endif
#endif

/* for avoiding conflicts in force calculation */
int phead[MAXPROCS][MAXPROCS]; /* [owner][calculator] */
struct procforce *pbase[MAXPROCS];  /* allocated to calculator */
int ptop[MAXPROCS];    /* used per calculator */
int pmax[MAXPROCS];    /* available per calculator */

#ifdef WIN32
void * graphmutex;
void * mem_mutex;
unsigned int locking_thread; /* so we can tell who has it */
#elif defined(PTHREADS)
pthread_mutex_t graphmutex;
pthread_mutex_t mem_mutex;
pthread_t locking_thread;
#else
#endif


/* Multithreading with worker threads */
#ifdef WIN32
volatile LONG busythreads;  /* number of threads that have started on a task */
#else
int busythreads;  /* number of threads that have started on a task */
#endif
int threadflag;   /* 1 if -p option used and worker threads in effect */
int thread_task;  /* which task to do */
element_id global_id;   /* global iteration variable */
struct thread_data **thread_data_ptrs;  /* one per thread to be allocated */
struct thread_data default_thread_data; /* if threads not activated */
struct thread_data *default_thread_data_ptr;


/* global communication to threads */
int m_hess_mode;
int m_rhs_mode;
int m_type;
int m_mode;
REAL *m_rhs;

/* thread stages stuff */
struct thread_stages_data *thread_stages;
int max_thread_stages; /* actual number of stages to do */
int v_partition_coord_attr; /* number of partitioning attribute */
int e_partition_coord_attr; /* number of partitioning attribute */
int f_partition_coord_attr; /* number of partitioning attribute */
int b_partition_coord_attr; /* number of partitioning attribute */
int fe_partition_coord_attr; /* number of partitioning attribute */
int v_partition_stage_attr;  /* which stage element is in */
int v_partition_proc_attr;   /* which processor element is assigned to */
int e_partition_stage_attr;  /* which stage element is in */
int e_partition_proc_attr;   /* which processor element is assigned to */
int f_partition_stage_attr;  /* which stage element is in */
int f_partition_proc_attr;   /* which processor element is assigned to */
int b_partition_stage_attr;  /* which stage element is in */
int b_partition_proc_attr;   /* which processor element is assigned to */
int fe_partition_stage_attr;  /* which stage element is in */
int fe_partition_proc_attr;   /* which processor element is assigned to */
long partition_timestamp;  /* last time partitioning done */

/* temp storage for element id name strings */
char elnames[10][30];

/* profiling */
int q_facet_setup_elapsed_time[2];
int calc_quants_elapsed_time[2];
int calc_quant_grads_elapsed_time[2];
int calc_quant_hess_elapsed_time[2];
int element_setup_elapsed_time[2];
int exparse_elapsed_time[2];
int yyparse_elapsed_time[2];
int yylex_elapsed_time[2];
int kblex_elapsed_time[2];
int hessian_solve_elapsed_time[2];
int hessian_mul_elapsed_time[2];
int hessian_AIJ_setup_elapsed_time[2];
int hessian_constraint_setup_elapsed_time[2];
int hessian_project_setup_elapsed_time[2];
int hessian_factor_elapsed_time[2];
int hessian_CHinvC_elapsed_time[2];
double cpu_speed;

int mpi_debug;  /* to enable verbose MPI messages */
int mpi_show_corona_flag; /* whether to display imported elements */

#ifdef _MSC_VER
__int64 thread_launch_start;
__int64 thread_launch_end;
#endif

#ifdef  __cplusplus
}
#endif
