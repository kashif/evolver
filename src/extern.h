/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/******************************************************************
*
*  File:  extern.h
*
*  Purpose:  declare global variables for Evolver program.
*/  

#ifdef  __cplusplus

/* types for exception handler */
struct loadexcep { int a; };
struct cmd_excep { int a; };
struct graph_excep { int a; };

extern "C" {
#endif

#define PATHSIZE  1000

#define DEFAULT_EDGE_COLOR BLACK
#define DEFAULT_FACET_COLOR WHITE
#define DEFAULT_TARGET_TOLERANCE (1e-4)
struct teix_gvert { REAL angle;
               REAL area;
               REAL star_area;
               REAL normal[MAXCOORD];
               REAL force[MAXCOORD]; /* for mean curvature */
             };
extern struct teix_gvert  *tgverts;
void create_pressure_quant(body_id);
void body_fe_fixup(facet_id);
/* extern char *sys_errlist[];  */  /* Uncomment this if compile error */
extern int broken_pipe_flag; /* so output routines will know to quit */
extern int match_id_flag; /* to make id match datafile number, option -i */
extern char *cmdfilename; /* for saving command line read file */
extern char *current_prompt; /* prompt string being displayed */
extern int echo_flag; /* whether to echo stdin; for piped input */
extern int auto_convert_flag; /* whether to automatically convert_to_quantities */
extern int exit_flag;  /* whether to end this command interpreter */
#define END_COMMANDS 1234551234
extern int hessian_subshell_flag; /* whether in hessian subshell, so can't menu  */
extern int subshell_depth; /* nesting depth of subshells */
extern char *curdir;  /* current directory */


#define DEFAULT_BRIGHTNESS 0.65
extern REAL brightness; /* midlevel gray for screen display */
extern int markedgedrawflag; /* for single-drawing edges */
extern int verbose_flag;  /* for lots of messages */
extern char loadfilename[PATHSIZE]; /* for LOAD command */
extern int addload_flag;  /* if doing ADDLOAD command */
extern jmp_buf loadjumpbuf;  /* for LOAD command */
extern jmp_buf graphjumpbuf;  /* for errors during MS graphing */
#ifdef PTHREADS
extern pthread_t draw_thread_id; /* for graphics thread */
#else
extern unsigned int draw_thread_id; /* for graphics thread */
#endif
extern char *warning_messages;  /* for storing warning messages */
extern int warning_messages_new; /* number of new warning messages since last looked */
extern size_t warning_messages_max; /* allocated bytes for warning_messages */
extern int shadow_warn_flag; /* warn if locals shadow existing names */
#define MAXSUPPRESS 100
extern int warnings_suppressed[MAXSUPPRESS];
extern int warnings_suppressed_count;
extern int file_no; /* number of current source file */
extern int file_no_used; /* number in list */
extern int file_no_max; /* number allocated */
extern char **file_names; /* names of source files */
extern int function_kludge_flag; /* handling errors in function def in datafile */

/* for binary_printf byte order */
extern int big_endian_flag;
extern int little_endian_flag;

/* my own ctypes */
extern char kb_upper_array[256];
extern char kb_lower_array[256];

extern int this_task; /* machine identifier for MPI; really task number */
extern int mpi_subtask_command_flag; /* whether command is running on task
     as subcommand from master instead of running isolated */
extern int mpi_debug;
extern int mpi_show_corona_flag; /* whether to display imported elements */
extern int mpi_local_bodies_flag; 
/* First mpi task is root */
#define MASTER_TASK 0

/* controlling edge and facet deletion options */
extern int star_finagling; /* extra bad configuration detection */
extern int force_deletion; /* even if it seems a bad idea */

/************************************************************/
/* Unified lists of imported elements for MPI version       */
/* On each node, there is list for each other node that     */
/* shares elements.  Should be only a few nontrivial        */
/* neighbors.                                               */
/* These are sparse lists to be used for locating elements  */
/* by id. For compact lists of imported and exported ids,   */
/* see import_elements and export_elements.                 */
struct remote_list_t {
     int *count;  /* used entries for each task */
     int *max;   /* pointers allocated for each task */
     struct element ***ibase;  /* task, ordinal */
};
extern struct remote_list_t remote_elements[NUMELEMENTS];

extern int sparse_ibase_flag; /* for new scheme permitting sparse ibase */

/* types of element storage expansion */
#define EXTEND_BATCH 1
#define EXTEND_FOR_REFINE 2

#define MAXOPTPARAM 100
extern int optparamcount;  /* number thereof */
struct optparam_t {
 int  pnum; /* which parameter this is */
 REAL grad; /* energy gradients */
 REAL velocity;  /* adjusted motion */
 REAL cg;  /* conjugate gradient history */
 REAL old_value;  /* for restoring */
 REAL oldgrad;  /* for conjugate gradient */
 int  rownum;  /* row in Hessian */
 int  vhead_index;
 };
extern struct optparam_t optparam[MAXOPTPARAM];
extern REAL **optparam_congrads;  /* constraint gradients */
#define OPTPARAM_DELTA (1.0e-4)

/* My memory block types, also used for corruption detection */
#define PERM_BLOCK  0xABAB
#define TEMP_BLOCK  0xDCDC
#define ETERNAL_BLOCK 0xEAEA
#define GRAPH_BLOCK 0xCBCB

/* for per-thread temp memory */
extern int this_thread;
#if defined(_WIN32)
#define THREADBLOCK(blocktype) blocktype = \
    (draw_thread_id==GetCurrentThreadId()) ? GRAPH_BLOCK : TEMP_BLOCK
#elif defined(PTHREADS)
#define THREADBLOCK(blocktype) blocktype = \
    (draw_thread_id==pthread_self()) ? GRAPH_BLOCK : TEMP_BLOCK
#else
#define THREADBLOCK(blocktype) blocktype = TEMP_BLOCK
#endif

/* for calc_volgrads() mode */
#define NO_OPTS  0
#define DO_OPTS  1
extern int volgrads_every_flag; /* whether recalc volgrads every projection iteration */

extern int zener_drag_flag; /* whether to do zener drag */
#define ZENER_COEFF_NAME "zener_coeff"
extern int backcull_flag;  /* 3D graphics backculling */
extern int setting_backcull; 

/* slicing view */
extern int slice_view_flag;
extern int slice_coeff_global;
extern REAL slice_coeff[MAXCOORD+2];

/* clipping view */
extern int clip_view_flag;
extern int clip_coeff_global;
#define MAXCLIPS 10
extern REAL clip_coeff[MAXCLIPS][MAXCOORD+2];

extern REAL **vgev;  /* vector vol grads, for approx curvature */
extern REAL **vgef;  /* form vol grads, for approx curvature */

/* auxiliary structures needed to hold accumulated data for some
   squared curvature methods */
/* per vertex structure */
struct v_curve_t { REAL area;     /* area around vertex */
                 REAL a;    /* allocated area */
                 REAL force[MAXCOORD];  /* force on vertex */
                 REAL f;        /* scalar force */
                 REAL h;        /* scalar curvature */
                 REAL star_force[MAXCOORD];  /* star area gradient */
                 REAL deriv2[MAXCOORD][MAXCOORD];  /* self second deriv */
                 REAL normal[MAXCOORD]; /* for effective area */
                 REAL norm;        /* square length of normal */
                 REAL fpgradf[MAXCOORD]; /* for projection gradient part */
                 REAL vol;  /* allocated volume of vertex */
                 int sign;    /* positive if H outward */
                 REAL term;  /* for squared integrands */
               };
extern struct v_curve_t *v_curve;

/* per edge structure */
struct e_curve_t { REAL deriv[2][MAXCOORD]; /* dA_head/dv_tail */
                 REAL aderiv[2][MAXCOORD]; /* extra alloc area derivs */
                 REAL deriv2[MAXCOORD][MAXCOORD];
                 REAL volderiv[2][MAXCOORD];
                    /* [0] is deriv of head vol wrt tail */
               };
extern struct e_curve_t *e_curve; 

/* macro stuff */
extern int keep_macros_flag; /* to preserve macros after datafile. */
#define MACRONAMESIZE 30
struct macro { char name[MACRONAMESIZE];
               int  offset;     /* start of substitute string */
               int  subsize;    /* size of substitute string */
             };
extern struct macro  *macros;  /* dynamically allocated */
extern int macro_count;  /* number of macros defined */
extern char  *macro_subs;  /* string space for substitution strings */

extern int facet_general_flag;
extern REAL factorial[20];
extern int everything_quantities_flag;  /* for pure quantity version, after conversion */
extern int show_all_quantities;  /* to display default quantities also */
extern int random_seed;  /* seed for random number generators */
extern int option_q;  /* record command line option */
extern int dont_resize_flag; /* set if datafile specified view matrix */
extern int do_show_flag; /* for Mac kludge graphics command prompt */

extern char *VERSION;
extern char *evolver_version;  /* for version checking */
extern char needed_version[30];

extern REAL machine_eps;  /* smallest machine resolution at 1.0  */

extern char *typenames[NUMELEMENTS];

/* silent linesplicing character */
#define MOREIN 1

/* free_discards() mode */
#define DISCARDS_ALL 1
#define DISCARDS_SOME 2

/* element attr bits */
#define ALL_ATTR     (ATTR)0xFFFFFFFF
#define ALLOCATED    (ATTR)0x0001
#define NODISPLAY    (ATTR)0x0002
#define NEWELEMENT   (ATTR)0x0004
#define NEWVERTEX    (ATTR)0x0004
#define NEWEDGE      (ATTR)0x0004
#define NEWFACET     (ATTR)0x0004
#define NEWBODY      (ATTR)0x0004
#define WRAPPED      (ATTR)0x0008
#define PINNED_V     (ATTR)0x0008
#define DENSITY      (ATTR)0x0010
#define FIXEDVOL     (ATTR)0x0020
#define FIXED        (ATTR)0x0040
#define BOUNDARY     (ATTR)0x0080
#define NEGBOUNDARY  (ATTR)0x0100
#define BDRY_ENERGY  (ATTR)0x0200
#define CONSTRAINT   (ATTR)0x0400
#define PRESSURE      (ATTR)0x0800
#define BDRY_CONTENT  (ATTR)0x1000
#define HIT_WALL      (ATTR)0x2000
#define NEGCONSTRAINT (ATTR)0x4000
#define HIT_PARTNER   (ATTR)0x8000
#define HAVE_CENTEROFMASS (ATTR)0x8000
#define BARE_NAKED    (ATTR)0x10000
#define Q_MIDPOINT    (ATTR)0x20000
#define TETRA_PT      (ATTR)0x40000
#define TRIPLE_PT     (ATTR)0x80000
#define DID_BODYFRONTFACET (ATTR)0x80000
#define DID_BODYBACKFACET (ATTR)0x100000
#define HIT_ONE_SIDED (ATTR)0x100000
#define Q_MIDFACET    (ATTR)0x200000
#define Q_MIDEDGE     (ATTR)0x400000
#define AXIAL_POINT   (ATTR)0x800000
#define NO_REFINE     (ATTR)0x1000000
#define DISSOLVED     (ATTR)0x2000000
#define EDGE_DRAWN    (ATTR)0x4000000
#define ACTUALVOL     (ATTR)0x8000000
#define REDUNDANT_BIT (ATTR)0x10000000
#define NONCONTENT    (ATTR)0x20000000

#define NO_ORIGINAL   (-1)

/* modes for move_vertices() and hessian_move() */
#define TEST_MOVE    0
#define ACTUAL_MOVE 1
#define SET_VELOCITY 2
/* modes for project_v_constr() */
#define KEEP_ONESIDEDNESS  0
#define RESET_ONESIDEDNESS 1

extern char *dymem; /* dynamic memory region */
extern int  dymemsize; /* size of dynamic memory region */
extern struct constraint null_constraint; /* non-constraint placeholder */
#define GETCONSTR(n) (web.constraints+(n))

/* compare_vertex_edge_attr results */
#define INCOMPARABLE    0
#define A_SUB_B   1
#define A_EQ_B    2
#define A_SUPER_B 3

extern char *areaname;  /* length or area */
extern int read_command_flag;  /* whether commands at end of datafile */
extern int err_tok_gen_flag;  /* whether -E option in effect */
extern int token_count;  /* how many tokens parsed for -E option */
extern int verb_flag;    /* set if lex looking for a verb */
extern int cond_expr_flag; /* set if parser looking for ':' in conditional expr */
extern int read_wrap_flag; /* set when reading wraps in datafile */
extern int exit_after_error; /* auto exit flag */ 
extern int exit_after_warning; /* auto exit flag */ 
extern int break_after_warning; /* auto break flag */ 
extern int last_error;  /* number of last error */
extern int change_flag;  /* set during command that changes surface */
extern int assigntype;  /* type of assignment operator */
extern int uminus_flag; /* whether to interpret " -" as UMINUS */
extern char *cmdptr;    /* current command or input for parsing */
extern int logfile_flag; /* whether logging in progress */
extern char logfilename[PATHSIZE];
extern FILE *logfilefd;
extern int keylogfile_flag; /* whether keylogging in progress */
extern char keylogfilename[PATHSIZE];
extern FILE *keylogfilefd;

/* structure used in check.c */
struct vvvv { element_id id;  /* which element    */
              element_id v[3];    /* id's of vertices */
            };
extern int check_count;      /* number of errors returned by run_checks() */
extern int gocount;          /* number of iterations left */ 
extern int go_display_flag;     /* display each change */
extern int box_flag;  /* whether or not to show outline box */
extern int quiet_flag; /* whether to display normal output */
extern int quiet_go_flag; /* whether to display normal g output */
extern int quiet_load_flag; /* whether to display output while loading file */
extern int shading_flag;  /* for facet shading by orientation */
extern int smooth_graph_flag; /* whether to smoothly plot in lagrange mode */
extern int visibility_test; /* whether to do after depth sort */
extern int visdebuglevel;  /* visibility debug level */
extern int color_flag;    /* facet coloring by user */
extern int rgb_colors_flag;  /* enabling rgb color scheme */
extern int edge_rgb_color_attr; /* number of rgb color attribute */
extern int facet_rgb_color_attr; /* number of rgb color attribute */
extern int facet_rgb_backcolor_attr; /* number of rgb color attribute */
extern int edge_alpha_flag; /* whether edges have alpha channel */
extern int facet_alpha_flag; /* whether facets have alpha channel */
extern int facetback_alpha_flag; /* whether facetbacks have alpha channel */
extern int background_color;  /* graphics background */
extern int geomview_bug_flag; /* for geomview 1.6.1 picking bug */
extern int gv_binary_flag;  /* whether to do geomview in binary */
extern int gv_pipe[2]; /* for pipe for reading geomview pick commands */
extern int pickvnum,pickenum,pickfnum,pickbnum; /* geomview picks */
extern int new_vertex_id,new_edge_id,new_facet_id,new_body_id; /* just created elements */
extern int gv_vect_start; /* vector vertex start in vpicklist */
extern int lazy_transforms_flag;  /* let graphics display handle symmetry */
/* for piping geomview output */
#define GEOM_TO_GEOMVIEW 0
#define GEOM_NAMED_PIPE 1
#define GEOM_PIPE_COMMAND 2
extern int parallel_update_flag[NUMELEMENTS]; /* set when element info changed */
extern element_id *el_list[NUMELEMENTS]; /* for list of active elements */
extern long global_timestamp;  /* universal clock */
extern long web_timestamp; /* something changes */
extern long graph_timestamp;  /* so graph routines know when surface changed */
extern long top_timestamp;    /* timestamp for topology changes */
extern long vedge_timestamp; /* for vertex edgelist currency */
extern long vfacet_timestamp; /* for vertex facetlist currency */
extern long bfacet_timestamp; /* for body facetlist currency */
extern long fixed_volume_timestamp; /* for volume calculation */
extern long info_volume_timestamp; /* for volume calculation */
extern long reset_timestamp;  /* when surface loaded */
#define NOSETSTAMP 0
#define SETSTAMP   1

extern int lmc_mc_attr;  /* vertex mean curvature attribute used by laplacian_mean_curvature */
extern int lmc_mobility_attr;  /* edge/facet mobility attribute used by laplacian_mean_curvature */
extern int quarter_turn_var; /* for quarter_turn symmetry */
extern int need_fe_reorder_flag; /* setting causes recalc to fe_reorder */
extern int ackerman_flag;  /* whether doing phase space motion */
extern char pix_file_name[150]; /* for P 2 */
extern int labelflag; /* whether ps doing labels */
extern int gridflag;  /* whether ps doing gridlines */
extern int ps_colorflag; /* whether ps doing color */
extern int full_bounding_box_flag; /* for window bounding box */
extern int crossingflag; /* whether ps doing crossings */

extern char ps_file_name[1000]; /* ps output file */
extern char *binary_off_filename; 

/* Following PostScript line width variables relative to page size */
extern REAL ps_labelsize;  /* relative size of labels */
extern REAL ps_stringwidth;  /* default edge width */
extern REAL ps_fixededgewidth;
extern REAL ps_tripleedgewidth;
extern REAL ps_conedgewidth;
extern REAL ps_bareedgewidth;
extern REAL ps_gridedgewidth;
extern int ps_widthattr;  /* in case user defines widths */
#define PS_WIDTHNAME  "ps_linewidth"
#define NOLABELS 0
#define LABEL_ID 1
#define LABEL_ORIG 2
extern vertex_id *vpicklist; /* for geomview picking */
extern facet_id *fpicklist;
extern REAL **to_focus;  /* used only by oglgraph.c at present */
extern REAL **from_focus;


#define NESTDEPTH 10
extern element_id junk; /* for MSC bug */
#ifndef PARALLEL_MACHINE
extern element_id xx_id;  /* for macros to prevent multiple evaluation */
extern element_id x1_id,x2_id,x3_id,x4_id,x5_id,x6_id,x7_id,x8_id,x9_id;
extern element_id xa_id,xb_id,xc_id,xd_id,xe_id,xf_id,xg_id,xh_id;
/*so nested macros don't tromp each other*/
#endif

extern struct treenode *permlist;

/* evaluation stack */
#define MAXSTACK 100
struct dstack { REAL value, deriv[2*MAXCOORD];
                REAL second[2*MAXCOORD][2*MAXCOORD]; 
              };

/* for redefining single letter commands */
extern struct expnode single_redefine[128];

/* for dynamic load library functions */
#define MAX_DLL 5
typedef void (*dll_func_type) ARGS((int , REAL*, struct dstack *));
struct dll { char *name;  /* library name */
             void *handle;     /* for dl functions */
           } ;
extern  struct dll dll_list[MAX_DLL];
#define FUNC_VALUE  1
#define FUNC_DERIV  2
#define FUNC_SECOND 3


/* global variable list */
#define GLOBAL_NAME_SIZE 31
struct global
  { char name[GLOBAL_NAME_SIZE + 1];  /* 31 significant characters */
    union { REAL real;
            char *string;
            struct expnode proc;
            struct { REAL *values;
                     char *value_file;
                   } file;
            int quant;  /* quantity */
            int meth_inst; /* method values*/
            dll_func_type funcptr; /* dynamic load function */
            int cnum; /* constraint number */
            int bnum; /* boundary number */
            int offset; /* local variable position on stack */
            void *dataptr;  /* for permanent internal variables */
            element_id id;
          } value;
    union {
      struct {
          int argcount; /* function argument count */
          struct locallist_t *locals;  /* local variable symbol table */
          int  proc_timestamp; /* for ordering procedure defines */
          char *proc_text; /* text of procedure definition */
#define ARGTYPENUM 8
          unsigned char argtypes[ARGTYPENUM]; /* indices into type table */
        } procstuff;
      struct {
          REAL delta; /* for optimizing parameter differencing */
          REAL pscale; /* for optimizing parameter scale matching */
          REAL *gradhess; /* for expressions in constraints etc. */
       } varstuff;
       struct array *arrayptr;
    } attr;
    int  flags;     /* see defines below */
    int  type;      /* datatype for variables */
  };
extern struct locallist_t *query_locals;
#define LOCALSCOPEMAX 100
extern struct locallist_t *local_scope_bases[LOCALSCOPEMAX]; /* for parsing */
extern int local_nest_depth;  /* for local_scope_bases */

/* for global identifier id */
#define GLOBMASK   0x00FFFFFF
#define GTYPEMASK  0xF0000000
#define EPHGLOBAL  0x10000000
#define PERMGLOBAL 0x20000000
#define LOCALVAR   0x30000000

/* stuff for using name_id to pass info about extra attributes */
#define ATTRIBNAME 0x40000000
#define ETSHIFT    24
#define ELTYPEMASK (0xF << ETSHIFT) 
#define set_name_eltype(name_id,eltype) (name_id | ATTRIBNAME | (eltype << ETSHIFT))
#define name_eltype(name_id)   (((name_id) & ELTYPEMASK) >> ETSHIFT)

typedef unsigned int ident_t;
#define ident_type(id)  ((id) & ~GLOBMASK)
#define ident_num(id)  ((id) & GLOBMASK)
#define get_localp(id) (localstack + localbase->list[(id)&GLOBMASK].offset)
#define get_local(id)  (localbase->list[(id)&GLOBMASK])

#define dy_globals web.dy_globals_w
#define globals(gid) ( \
    ((gid)&(~GLOBMASK))==EPHGLOBAL ?  \
    (((struct global *)(dymem + dy_globals))+((gid)&GLOBMASK)) : \
    (((gid)&(~GLOBMASK))==LOCALVAR ?  \
    (&(localbase->list[(gid)&GLOBMASK].g)) : \
    (((gid)&(~GLOBMASK))==PERMGLOBAL ?  \
    (dy_perm_globals+((gid)&GLOBMASK)) : \
    (((struct global *)(dymem + dy_globals))+((gid)&GLOBMASK)) \
   )))
extern struct global *Globals; /* handy for debugging */
#define dy_perm_globals web.dy_perm_globals_w
#define perm_globals(gid) (dy_perm_globals+((gid)&GLOBMASK))
extern struct global *perm_Globals; /* handy for debugging */
#define dy_globalshash web.dy_globalshash_w
#define globalshash ((int *)(dymem + dy_globalshash))
extern int dy_global_hash_max;      /* allocated entries */
extern int dy_global_hash_maxfill;  /* entries before expanding */
extern int dy_global_hash_used;     /* entries actually in use */
/* Bitfield for type of name */
#define NAMETYPEMASK  0xE0000000
#define INDEXMASK     0x1FFFFFFF
#define VARIABLENAME  0x20000000
#define METHODNAME    0x40000000
#define QUANTITYNAME  0x60000000
#define PERM_NAME     0x80000000

/* Hash table actions */
#define HASH_LOOK 0
#define HASH_ADD 1
#define HASH_DELETE 2

                 
/* local variable list for procedure */
struct localvar_t {
   struct global g;
   int prev;  /* up the list; search order */
   int scope_depth;
   int offset;  /* in stack units */
   int size; /* in stack units */
};

struct locallist_t {
    struct localvar_t *list;
    int maxlist;
    int count;
    int totalsize;
    int scope_depth;
    int flags; /* see below */
    ident_t iid; /* of procedure this list belongs to */
};
extern struct locallist_t *localbase;
/* locallist_t flag bits */
#define LL_HAS_ARRAY 1
#define LL_PERMANENT 2
#define LL_IN_USE    4
/* type of listing desired */
#define LIST_PROTO  1
#define LIST_FULL   2

extern int proc_timestamp; /* for ordering procedure definitions */
extern int old_global_count; /* for error recovery */
extern int old_perm_global_count; /* for error recovery */
extern int perm_flag;  /* for whether permanent assignment parsing in place */
extern int reading_comp_quant_flag;  
extern int cur_quant;  /* when reading compound quantity */
extern int compound_hess_flag; /* mode while doing compound hessian */
#define CH_GRADS 1
#define CH_HESS  2
extern int quantities_only_flag; /* for using named quantities only */
extern int gravity_quantity_num;  /* number of quantity for default gravity */
extern int gap_quantity_num;  /* number of quantity for default gap energy */
extern int default_area_quant_num; /* number of quantity for default area */
extern int rotorder_var; /* for flip_rot order */
extern int calc_quant_flag; /* set when quantity calculation underway */
extern char volume_method_name[100];  /* for replacing default */
extern char area_method_name[100];  /* for replacing default */
extern char length_method_name[100];  /* for replacing default */
extern int  length_method_number;  /* for replacing default */
extern int dirichlet_flag;  /* to do area hessian with Dirichlet hessian */
extern int sobolev_flag;  /* to do area hessian with Sobolev hessian */
extern REAL **vproj_base; /* vertex projection matrix row pointers */
extern REAL *vproj_space; /* vertex projection matrix arena */
extern REAL ***conhess_base; /* vertex constraint hessian arena */
extern REAL **ritzvecs;

/* defines for global variable flags */
#define ORDINARY_PARAM 1  /* ordinary real-valued */
#define FILE_VALUES 2    /* values to be read from file */
#define SUBROUTINE  4    /* value is a parse tree */
#define RECALC_PARAMETER 8  /* causes recalc when variable changed */
#define PERMANENT  0x10    /* do not forget for new surface */
#define GLOB_USED  0x20    /* if allocated */
#define QUANTITY_MODULUS  0x40  /* multiplier for quantity */
#define QUANTITY_NAME  0x80  /* actual value for quantity */
#define QUANTITY_TARGET  0x100  /* target value for quantity */
#define QUANTITY_TYPES (QUANTITY_MODULUS|QUANTITY_NAME|QUANTITY_TARGET)
#define METHOD_MODULUS 0x200
#define METHOD_NAME 0x400
#define ARRAY_PARAM 0x800
#define METHOD_TYPES (METHOD_MODULUS|METHOD_NAME)
#define ANY_TYPE  (FILE_VALUES|SUBROUTINE|QUANTITY_TYPES|METHOD_TYPES)
#define STRINGVAL 0x2000  /* string */
#define LEFTOVER  0x4000  /* if permanent left over from prev file */
#define OPTIMIZING_PARAMETER 0x8000  /* variable during optimization */
#define DYNAMIC_LOAD_FUNC 0x10000  /* dynamic load library function */
#define CONSTRAINT_NAME 0x20000  
#define BOUNDARY_NAME 0x40000  
#define GLOB_LOCALVAR 0x80000
#define FUNCTION_NAME 0x100000
#define PROCEDURE_NAME 0x200000
#define INTERNAL_NAME  0x400000
#define READONLY 0x800000
#define INTVAL  0x1000000
#define REALVAL 0x2000000
#define IN_DATAFILE_TOP 0x4000000
#define ALWAYS_RECALC   0x8000000
#define FIXED_SIZE_ARRAY 0x10000000
#define UNFIXED_SIZE_ARRAY 0x20000000

extern struct expnode torus_period_expr[MAXCOORD][MAXCOORD];
extern struct expnode torus_display_period_expr[MAXCOORD][MAXCOORD];

/* for calc_periods() */
#define NO_ADJUST_VOLUMES 0
#define ADJUST_VOLUMES     1

/* Bits for calc_all_grads() and such */
#define CALC_FORCE     1
#define CALC_VOLGRADS 2


extern int torus_display_mode;  /* default, raw, connected, clipped */
/* modes */
#define TORUS_DEFAULT_MODE 0
#define TORUS_RAW_MODE      1
#define TORUS_CONNECTED_MODE 2
#define TORUS_CLIPPED_MODE 3

extern int view_matrix_global; /* global id number */
extern int eigenvalues_list_global; /* for accessing ritz() output list */ 
extern int torus_periods_global;
extern int inverse_periods_global;
extern REAL window_aspect_ratio;

extern int read_depth,include_depth;
extern char datafilename[PATHSIZE];  /* current datafile name */
#define NOTDATAFILENAME  0 /* path_open() mode */
#define SETDATAFILENAME  1 /* path_open() mode */
extern char filename[PATHSIZE];  /* file name in command */
extern FILE *commandfd;  /* command input file */
extern FILE *logfd;  /* command log file */
extern struct cmdfile { 
            FILE *fd;                /* for nested reads    */
            char filename[PATHSIZE]; /* command file name   */
            int line;                /* for error reporting */
            int datafile_flag;       /* whether datafile    */
            int file_no;
         } cmdfile_stack[NESTDEPTH],datafile_stack[NESTDEPTH];

extern int datafile_flag;  /* 1 for datafile, 0 for command so expression
                                        parser knows what's up */
extern int datafile_input_flag; /* whether further input available from datafile */
#define NOT_DATAFILE 0
#define IN_DATAFILE 1
#define PRETEND_DATAFILE 2

extern int topflag;  /* 1 while in datafile top section */
extern int backquote_flag; /* 1 while in backquoted command; kludge */
extern int lists_flag;  /* set when parsing space-separated lists */
#define LISTS_OFF 0
#define LISTS_SOME 1
#define LISTS_FULL 2

extern int use_given_id; /* iterator expr should not use local, for SHOW_ */
extern int const_expr_flag;  /* 1 for const_expr, 0 for command, so expression
                                        parser knows what's up */
extern int boundary_expr_flag; /* so parser knows when parsing boundary */
extern int reading_elements_flag; /* so parser knows attributes should not
     be accepted in expressions */
extern FILE *outfd;     /* for normal output */
extern int check_increase_flag;  /* to detect blowups */
extern int estimate_flag;    /* for toggling estimate of energy decrease */
extern REAL estimated_change; /* stored result */
extern int autorecalc_flag; /* for toggling autorecalc after variable assign  */
extern int autopop_flag;     /* whether to do autopopping */
extern int immediate_autopop_flag; /* set if pop before motion */
extern int autopop_quartic_flag; /* if autopop length = sqrt(sqrt(2*scale)) */
extern int pop_disjoin_flag;  /* whether cones to be disjoined rather than merged */
extern int pop_enjoin_flag;  /* whether cones to be enjoined rather than disjoined */
extern int pop_to_edge_flag;  /* control which way popping goes */
extern int pop_to_face_flag;  /* control which way popping goes */
/* some popping modes for various popping functions */
#define POP_TO_BETTER    0
#define POP_TO_OPEN   1
#define POP_TO_TRIPLE 2
#define POP_TO_TWIST  3
#define POP_TO_TRIANGLE 4
#define POP_TO_EDGE   5
#define POP_TO_FACE   6

extern int autochop_flag;     /* whether to do autochopping */
extern REAL autochop_length;  /* max edge length for autochop */
extern int autopop_count;  /* number of edges found */
extern int autochop_count;  /* number of edges found */
extern int kraynikpopvertex_flag;  /* for special popping of a certain cone */
extern int kraynikpopedge_flag;  /* for special popping of certain edges */
extern int parens;             /* level of parenthesis nesting */
extern int brace_depth;             /* level of brace nesting */
extern int in_quote;         /* so lexer knows when in string */
extern int in_function;         /* so lexer knows when in function def */
extern int in_comment;         /* so lexer knows when in comment */
extern int in_control_structure;  /* so lexer knows when in control structure */
extern int effective_area_flag; /* use quadratic form for area around vertex */
extern int old_area_flag;     /* on for using old effective area */
extern int runge_kutta_flag; /* whether to use runge-kutta method for motion */
extern REAL total_time;      /* total scale factor */
extern REAL star_fraction;  /* weighting factor for star around vertices */
extern int area_fixed_flag;    /* for fixed area constraint */
extern REAL area_fixed_target;  /* target value for fixed area */
extern REAL area_fixed_pressure; /* Lagrange multiplier */
#define AREA_Q_ID 0x1111          /* for constraint identification */
extern int post_project_flag;     /* project to quant constr after each motion */
extern int edge_diffusion_attr;
extern int facet_diffusion_attr;
extern struct expnode mobility_formula;
extern int mobility_flag; /* whether mobility in effect */
extern struct expnode mobility_tensor[MAXCOORD][MAXCOORD];
extern int mobility_tensor_flag; /* whether tensor mobility in effect */
extern int check_pinning_flag;  /* for vertices changing constraints */
extern int nprocs;     /* number of parallel processors */
extern int *v_procnum; /* processors for vertex, index by ord */
extern int procs_requested; /* number of processes desired */
extern REAL proc_total_area[MAXPROCS]; /* for individual processes */
extern int web_checksum; /* to see if web needs sending */
extern int dymem_checksum; /* see if dymem needs sending */
extern int comp_quant_vertex; /* during calc_quant_grad */
extern int comp_quant_vertexi; /* during calc_quant_hess */
extern int comp_quant_vertexj; /* during calc_quant_hess */
extern int comp_quant_type; /* during calc_quant_grad */
extern int comp_quant_stamp; /* coordination with eval_all and eval_sec */
extern vertex_id comp_quant_vi; /* during calc_quant_hess */
extern vertex_id comp_quant_vj; /* during calc_quant_hess */

extern REAL *f_sums;  /* facet_knot_energy, for sums to all other vertices */
extern REAL *bi_sums;  /* b_surface method, for sums to all other vertices */

/*extern char yytext[];*/
extern char *yytext;
extern char *inputbuffer;
extern int inputbufferspot;
extern int inputsave_flag;
extern int yydebug;  /* flag for parser debugging */
extern int help_flag; /* avoid error message while doing help */
extern int memdebug;  /* flag for memory debugging in run_checks() */
extern int itdebug;  /* flag for iteration debugginng */
extern int single_step_debugging; /* flag for debug stop at next line */
#if defined(WIN32) && defined(_DEBUG)
extern int _CrtCheckMemory(void);
#define HEAPCHECK { if ( (_heapchk() != _HEAPOK )    \
        /* || (_CrtCheckMemory() == 0 ) */ ) fprintf(stderr,  \
      "Bad heap at " __FILE__ " line %s. ",__LINE__ ); }
#else
#define HEAPCHECK
#endif
extern int tok;
extern int unput_tok_count;  /* number of pushed-back tokens */
extern int int_val;
extern REAL real_val;
extern int attr_etype; /* for ATTRIBUTE parse */
extern int subtype; /* for INDEXED_SUBTYPE parse */
extern int coord_num;
extern char idname[35]; /* for saving yytext */
extern char set_extra_name[100]; /* for saving name */
extern int line_no;
#define MAXHISTORY 100 
#define HISTORYSPACE 8000
extern char *history_space;
extern int  history_number;    /* number of current command */
extern int history_count;  /* number in list */
extern int  history_offsets[MAXHISTORY];

/* following two are for mode argument of command() */
#define NO_HISTORY 0
#define ADD_TO_HISTORY 1

#define MAXCMDSIZE 2000 
extern char fulltext[MAXCMDSIZE+5]; /* for full text of commands */
extern size_t  fulltextsize;              /* length of command */
extern int aggrtype;  /* aggregate type being parsed */
extern int aggregate_depth; /* nesting depth of aggregate loops */
extern int attr_kind; /* kind of attribute being parsed */
extern char *default_name; /* for unnamed elements */
extern char last_name[50]; /* name of last element generator */
#define HISTBINS 21    /* bins in histogram (actually 1 less )*/

/* symbol table stuff */
#define SYMNAMESIZE 31
struct sym {
    char name[SYMNAMESIZE+1];
    int  type; /* element type, or see defines below */
    int  localnum;  
  };
#define SYMTYPE_INT     6
#define SYMTYPE_REAL    7
#define SYMTYPE_INT_ARRAY     8
#define SYMTYPE_REAL_ARRAY    9
#define MAXARRAYDIMS  8
extern struct sym *elsym;  /* name of element during parsing */
extern struct sym *yysym;  /* name of identifier from lex */
extern struct sym symtable[];    /* symbol table */
struct array { int dim; 
               int datatype;   /* REAL_TYPE_, INTEGER_TYPE_, etc. */
               int itemsize;   /* bytes per item */
               int datacount;   /* total data elements */
               size_t datastart;  /* byte offset of start of data */
               int sizes[MAXARRAYDIMS];    /* arbitrary length, followed by data */
             };
               
/************************************************************************
* structure for defining extra attributes
*/
#define ATTR_NAME_SIZE 31
struct extra { char name[ATTR_NAME_SIZE+1];
               int offset;  /* within allocated space */
               int type;    /* see below */

               struct array array_spec;

//               int adim;     /* number of array dimensions; 0 for scalar */
//               int sizes[MAXEXTRADIM]; /* of dimensions */ 
 //              int itemsize;   /* bytes per item */
  //             int datacount;  /* total elements */

               struct expnode code;  /* for function attribute */
               int flags;   /* see below */
            };

/* flag bits */
#define DUMP_ATTR     1
#define FUNCTION_ATTR 2
#define DIMENSIONED_ATTR 4
#define RECALC_ATTR      8

/* useful for debugging */
extern struct extra *Extras[NUMELEMENTS];

/* datatype types */
#define NULL_TYPE     0
/* these types will be stored on the eval stack as reals */
#define REAL_TYPE     1
#define INTEGER_TYPE  2
#define ULONG_TYPE    3
#define UCHAR_TYPE    4
#define USHORT_TYPE   5
#define UINT_TYPE     6
#define LONG_TYPE    7
#define CHAR_TYPE    8
#define SHORT_TYPE   9 
#define MAX_NUMERIC_TYPE 10
/* end of numeric types */
#define STRING_TYPE  11
#define PTR_TYPE   12
#define VERTEX_TYPE 13
#define EDGE_TYPE   14
#define FACET_TYPE  15
#define BODY_TYPE   16
#define FACETEDGE_TYPE 17
#define ELEMENTID_TYPE 18 
#define BOUNDARY_TYPE  19
#define CONSTRAINT_TYPE 20
#define QUANTITY_TYPE 21
#define INSTANCE_TYPE 22
#define PROCEDURE_TYPE 23

#define NUMDATATYPES 24
/* the following are set in skeleton.c to match above defines */
extern char *datatype_name[NUMDATATYPES];
extern int datatype_size[NUMDATATYPES];


/* squared curvature as part of energy */
extern int sqcurve_ignore_constr; /* set if to count fixed and constrained verts */
extern int square_curvature_flag;  /* set if to be counted */
extern int square_curvature_param;  /* which parameter for modulus */
extern int mean_curvature_param;    /* which parameter for modulus */
extern int mean_curv_int_quantity_num;    /* for everything quantities */
extern int sq_mean_curv_quantity_num; /* for everything quantities */
extern int kusner_flag;    /* set for edge square curvature */
extern int assume_oriented_flag; /* for orientation checking */
extern int boundary_curvature_flag; /* whether to include boundary vertices */
extern int conf_edge_curv_flag; /* set for conformal edge curvature squared */
extern int sqgauss_flag;  /* for squared gaussian curvature */
extern int h0_attr;  /* attribute number for h_zero, if any */
#define H0_IN_GLOBAL 1
#define H0_IN_ATTR  2

#define TEST_SQ    0
#define PLAIN_SQ  1
#define EFF_SQ     2
#define NORMAL_SQ 3
#define PERP_SQ   4
extern int sqgauss_param;    /* which parameter for modulus */
extern int normal_sq_mean_curvature_mi;
extern int eff_area_sq_mean_curvature_mi;
extern int sq_mean_curvature_mi;
extern int mix_sq_mean_curvature_mi;
extern int star_normal_sq_mean_curvature_mi;
extern int star_perp_sq_mean_curvature_mi;
extern int star_eff_area_sq_mean_curvature_mi;
extern int star_sq_mean_curvature_mi;
extern REAL target_length;  /* for string model */
extern int approx_curve_flag;  /* if approximate curvature in effect */
extern int mean_curv_int_flag;  /* for unsquared mean curvature */
extern int normal_curvature_flag; /* choice of curvature formula */
extern int div_normal_curvature_flag; /* choice of curvature formula */
extern int marked_edge_attr;
#define MARKED_EDGE_ATTR_NAME "sqcurve_string_mark"
extern int circular_arc_flag; /* whether to draw edges as arcs */
extern int spherical_arc_flag; /* whether to draw edges as spherical arcs */

/* flag bits to say if should be evaluated since modulus nonzero */
#define EVALUATE 0x0002

#define SELFSIM_NAME "self_sim_coeff"
extern int self_similar_flag;

#define STR_CUR_TOL_NAME "string_curve_tolerance"
REAL string_curve_tolerance; /* quadratic string model smoothness, deg */

/* for restricting motion to be along normals */
extern int normal_motion_flag;
typedef REAL pt_type[MAXCOORD];
extern pt_type *vertex_normals;  /* for storage of normals by ordinal */

/* for Dennis DeTurck unit normal motion */
extern int unit_normal_flag;
extern REAL deturck_factor;  /* weight for unit normal */

extern REAL **identmat;  /* handy identity matrix, set up in init_view */

/* homothety target value, set when homothety toggled on */
extern REAL homothety_target;

extern int scrollbuffersize; /* output console lines */
extern char *msg;      /* for constructing user messages */
extern int msgmax;     /* length allocated */
#define ERRMSGSIZE 2000
extern char errmsg[ERRMSGSIZE];  /* for error() routine */
extern int  parse_error_flag;  /* set when parser hits error */
extern int parse_errors;     /* for counting errors */
extern int  recovery_flag;      /* set while recovering from parsing error */
#define MAXCMDDEPTH 100
extern jmp_buf jumpbuf[MAXCMDDEPTH];    /* for error recovery  */
extern jmp_buf cmdbuf;    /* for command error recovery  */
extern jmp_buf m_jumpbuf[MAXPROCS];    /* for multiproc error recovery  */
#define UNRECOVERABLE 0
#define RECOVERABLE    1
#define WARNING         2
#define PARSE_ERROR    3
#define EXPRESSION_ERROR 4
#define COMMAND_ERROR 5
#define DATAFILE_ERROR 6
#define SYNTAX_ERROR 7
#define Q_ERROR 8
#define RECOVERABLE_QUIET 9
#define RECOVERABLE_ABORT 10

/* for queries */
extern int celement;
extern int commandverb;
extern int condition_flag;
#define ATTRIBUTE 19382
extern struct expnode *show_expr[NUMELEMENTS];  /* for element show exprs */
extern struct expnode show_command[NUMELEMENTS];  /* save for dump */
extern struct expnode show_expr_table[NUMELEMENTS];  /* actual expressions */
extern int query_intval;
extern REAL query_realval;
extern int set_query_type;
extern ATTR set_query_attr;
extern int query_coord;
/* values for set attribute queries */
#define SET_Q_ATTR  1301
#define SET_DENSITY 1302
#define SET_VOLUME  1303
#define SET_CONSTRAINT 1304
#define SET_COORD    1305
#define SET_PARAM    1306
#define SET_TAG      1307
#define UNSET_Q_ATTR 1308
#define UNSET_CONSTRAINT 1309
#define SET_COLOR  1310
#define SET_TRANS  1311
#define SET_FIXED  1312
#define DID_         4000


extern REAL volume_factorial;  /* simplex volume factor */
extern int subsimplex[1<<MAXCOORD][MAXCOORD];  /* for refining simplices */
struct simplex { int pt[MAXCOORD+1]; };
struct divedge { int endpt[2];  /* endpoints */
                 int divpt;      /* dividing point of edge */
               };



/* for expression parsing */
#define LISTMAX 200
extern struct treenode *list;    /* tree */
extern int listtop;  /* first spot reserved for root */
extern int listmax;  /* allocated spots */

/* type of checking to do */
#define PRELIMCHECK  1
#define REGCHECK      2

/* web.representation */
#define STRING    1
#define SOAPFILM 2
#define SIMPLEX  3

/* for vol_project and sp_hessian_solve */
#define NO_SET_PRESSURE 0
#define SET_PRESSURE     1
#define PROJECT_FORCE    2

/* boundary projection types */
#define PARAMPROJ  1
#define TANGPROJ    2
#define GRADPROJ    3
#define PLAINPROJ  4

/* maximum number for constraint projection iterations */
#define MAXCONITER 10

/* for telling constr_proj to detect one-sided constraints */
#define DETECT 1
#define NO_DETECT 0

/* for telling one_sided_adjust() what to adjust */
#define ADJUST_ALL 0
#define ADJUST_VGRADS 1

#define DEFAULT_TOLERANCE  (1e-12)
extern REAL constraint_tolerance;

extern int one_sided_present; /* whether any one-sided constraints exist */
extern int raw_velocity_attr; /* for use by one-sided constraints */
#define RAW_VELOCITY_ATTR_NAME "raw_velocity"

#define ONE_SIDED_LAGRANGE_ATTR_NAME "one_sided_lagrange"
extern int one_sided_lagrange_attr;

/* vertex averaging modes */
#define NOVOLKEEP 0
#define VOLKEEP    1
#define RAWEST     2

extern REAL  overall_size;  /* for anybody who wants to know how big */
extern int breakflag;      /* set by user interrupt */
#define BREAKFULL 1
#define BREAKLOOP 2 /* unused */
#define BREAKREPEAT 3
#define BREAKAFTERWARNING 4
#define BREAKABORT 5
extern int iterate_flag;  /* so handler knows when iteration in progress */
extern int bare_edge_count;  /* edges without facets */

struct oldcoord {  
    REAL  (*coord)[MAXCOORD];  /* allocated space for old coordinates */
    REAL  energy;
    REAL *bod;
    REAL *meth;
    REAL *quant;
    REAL optparam_values[MAXOPTPARAM];
    int  size; /* bytes allocated for coord */
    } ;
extern struct oldcoord saved;
#define SAVE_IN_ATTR 1
#define SAVE_SEPARATE 2

/* ridge or valley indicators  */
#define RIDGE  1
#define VALLEY 2
extern int ridge_color_flag;  /* whether to differently color */
extern int edgeshow_flag;    /* whether to show edges of facets */
extern int triple_edgeshow_flag;    /* whether to show triple edges  */

/* vertex for zooming in on; default is first one read in */
extern int zoom_number;

/* for inner clipping for making zoom pictures */
extern int inner_clip_flag;
extern REAL inner_clip_rad;

/* conjugate gradient stuff */
extern int     conj_grad_flag;  /* whether conjugate gradient in effect */
extern REAL cg_oldsum;  /* total grad*grad from previous step */
extern REAL    (*cg_hvector)[MAXCOORD];  /* saved direction vector */
extern REAL cg_gamma;    /* direction adjustment factor */
extern int  ribiere_flag; /* to do Polak-Ribiere version */
#define RIBIERE_ATTR_NAME "old_force_ribiere"


/* structure for managing the facets of one body */
struct bodyface { facet_id f_id;
                  body_id  b_id;
                  WRAPTYPE wrap;  /* wraps of base vertex */
                  int        wrapflag; /* whether wrap done */
                };
/* wrapflag values */
#define NOT_WRAPPED 0
#define IS_WRAPPED  1
#define BAD_BAD_BAD 2

/* structure for depth sorting triangles */
struct tlink { int node; /* index in tlist */
               struct tlink *next;
             };

/* data structure for painter algorithm */
#define GDIM 3
extern struct tsort { 
          element_id f_id;                /* facet this is for ( or edge ) */
          int color;                      /* color map index */
          int ecolor[FACET_EDGES];        /* edge colors */
          short etype[FACET_EDGES];       /* special edge type */
          int flag;                       /* set if not yet displayed */
          float x[FACET_VERTS][GDIM];     /* display coordinates of vertices */
          float normal[GDIM];             /* rearward normal */
          float mins[GDIM];               /* minimum coordinates */
          float maxs[GDIM];               /* maximum coordinates */
          float width;                    /* for edge */
          vertex_id v_id[FACET_VERTS];    /* vertices, if valid */
          unsigned int quadcode;          /* quadtree encoding */
          int backstamp;                  /* for loop detection */
          struct tsort *prev;             /* for depth order linked list */
          struct tsort *next;             /* for depth order linked list */
          int spot;                       /* spot in draw list */
#ifdef TOPSORT
          int sons;                       /* for topological sort */
          struct tlink *linkhead;
          int from;                       /* for loop detection */
#endif
       } *trilist;
/* flag is EDGE or FACET in lower bits, and LABEL_EDGE, and ... */
#define FLIPPED_FACET 0x1000
#define WAS_BACKMOST  0x2000
#define VISIBLE       0x4000
#define ARC_EDGE      0x8000
#define SPHERE_EDGE  0x10000

extern int graph_capabilities;  /* bits to describe what current graphics 
                                   device can do */
#define GC_ARCS  0x1
#define GS_ARCS  0x2

/* bounding box, for PostScript */
extern REAL bbox_minx,bbox_miny,bbox_maxx,bbox_maxy;
extern int need_bounding_box;  /* flag to tell painter.c to calculate */
/* clip window for PostScript and painter algorithms */
extern REAL minclipx,maxclipx,minclipy,maxclipy;

/* 3D bounding box */
extern REAL bounding_box[MAXCOORD][2];

struct graphdata { REAL  x[MAXCOORD+1];  /* homogeneous coordinates */
                   REAL  norm[MAXCOORD]; /* unit normal */
                   int   color;          /* colormap index */
                   int   backcolor;      /* for back of facet */
                   int   ecolor;         /* edge color */
                   short etype;          /* edge type */
                   element_id id;        /* which element being graphed */
                   element_id v_id;      /* vertex */
                   int flags;
                };
/* flag bits */
/* SIMPLE_FACET and LIST_FACET are used in geomgraph.c */
#define SIMPLE_FACET  1
#define LIST_FACET     2
#define EDGE_ARC 0x8000
#define SPHERE_ARC 0x10000
/* edge types */
#define  INVISIBLE_EDGE 0
#define  REGULAR_EDGE  1
#define  TRIPLE_EDGE    2
#define  SINGLE_EDGE    4
#define  BOUNDARY_EDGE 8
#define  FIXED_EDGE     0x10
#define  CONSTRAINT_EDGE 0x20
#define  BARE_EDGE      0x40
#define  SPLITTING_EDGE 0x80
#define  EBITS          0xFF

#define  LABEL_EDGE     0x100
#define  LABEL_HEAD     0x200
#define  LABEL_TAIL     0x400
#define  LABEL_FACET    0x800
#define  LABEL_REVERSED 0x1000

/* for extracting color bits from int color */
#define GET_RED_BITS(c)  (((unsigned int)(c)>>24) & 0xFF)
#define GET_GREEN_BITS(c)  (((unsigned int)(c)>>16) & 0xFF)
#define GET_BLUE_BITS(c)  (((unsigned int)(c)>>8) & 0xFF)
#define GET_ALPHA_BITS(c)  (((unsigned int)(c)) & 0xFF)

typedef REAL  IColor[4];  /* r,g,b,a;  not same as OOGL ColorA */
#define IRIS_COLOR_MAX 16
extern IColor rgb_colors[IRIS_COLOR_MAX];
extern REAL facet_alpha;  /* global transparency */

/* homogeneous coordinate dimensions */
extern int HOMDIM;

/* function pointers for invoking device-specific graphics */
/* first four are for random-order plotting and are fed raw data */
extern void (*graph_start) ARGS((void));  /* called at start of graphing */
extern void (*graph_edge ) ARGS((struct graphdata *,edge_id));  /* called to graph one triangle */
extern void (*graph_facet) ARGS((struct graphdata *,facet_id));  /* called to graph one triangle */
extern void (*graph_end) ARGS((void));     /* called at end of graphics */
/* second four are for painter algorithm output, are fed digested data */
extern void (*display_edge ) ARGS((struct tsort *));  /* hardware edge  display */
extern void (*display_facet) ARGS((struct tsort *));  /* hardware facet display */
extern void (*init_graphics) ARGS((void));  /* hardware initialization */
extern void (*finish_graphics) ARGS((void));  /* hardware end of picture */
extern void (*close_graphics) ARGS((void));  /* close graphics window */

/* edge thicknesses for painter algorithm */
/* in order BARE_EDGE, FIXED_EDGE, CONSTRAINT_EDGE, BOUNDARY_EDGE,
   SINGLE_EDGE, TRIPLE_EDGE, and  other */
extern REAL *edgewidths;  /* pointer to be set by display() */
extern REAL pswidths[];  /* PostScript; values in variable.c */
extern REAL xwidths[];  /* X windows; values in variable.c */
extern REAL otherwidths[];  /* anything else; values in variable.c */

/* graphing flags */
extern  int init_flag; /* whether graphics initialized */
extern  int bdry_showflag;  /* whether to show facets on boundary */
extern  int no_wall_flag;        /* whether to suppress wall facets */
extern  int normflag;
extern  int thickenflag;
extern  int innerflag;
extern  int outerflag;
extern  int colorflag;
extern  int OOGL_flag;    /* whether MinneView initialized */
extern  int geomview_flag;    /* whether geomview initialized */
extern  int geompipe_flag;    /* for pipe only */
extern  REAL thickness;  /* for thickening */
extern  int user_thickness_flag; /* whether user has specified thickness */
extern  int view_4D_flag;  /* 0 for doing 3D xyz projection graphics, */
                                    /* 1 for outputting 4D graphics */

extern FILE * savefd;    /* file for binary dump of web */
extern FILE * data_fd;    /* file for initial data */

extern char cmapname[100];  /* colormap file name */
typedef REAL maprow[4];
extern  maprow *colormap; /* rgba colormap, values 0 to 1 */
extern int fillcolor;    /* current polygon fill color */
/* gaussian integration coefficients */
extern REAL gcombo[EDGE_CTRL][EDGE_INTERP];
extern REAL sdip[EDGE_CTRL][EDGE_INTERP];
extern REAL ssimp[EDGE_CTRL][EDGE_INTERP];
extern REAL gauss2wt[EDGE_INTERP];
extern REAL poly2partial[FACET_CTRL][2][2];
extern REAL scoeff[EDGE_CTRL][EDGE_CTRL]; /* plane area coeff */
extern REAL vcoeff[FACET_CTRL][FACET_CTRL][FACET_CTRL]; /* volume coeff*/
extern int set_by_user_gauss_1D;  /* minimums set by user */
extern int set_by_user_gauss_2D;
extern REAL **conical_x;
extern REAL *conical_w;


/* general parameters */
extern REAL **view;  /* transformation matrix */ 
extern int datafile_view_flag; /* whether datafile had view matrix */
extern int steps; 
extern int energy_init;      /* to keep track if we have current config energy */

/* for vertex popping */
struct verfacet { vertex_id v_id; facet_id f_id; };

#define MAXWULFF 100
extern REAL wulff_vector[MAXWULFF][MAXCOORD];  /* the vector components */
#ifdef NOPROTO
extern void (*get_wulff)();
#else
extern void (*get_wulff)(REAL *,REAL *);
#endif

extern int interp_bdry_param; /* flag for interp or extrap bdry param */
extern int extra_bdry_attr; /* when extra boundary for vertices */
extern int extra_bdry_param_attr; /* corresponding parameter values */
#define EXTRA_BDRY_NAME "extra_boundary"
#define EXTRA_BDRY_PARAM_NAME "extra_boundary_param"

extern REAL **phase_data;  /* phase boundary energies */
extern char phase_file_name[PATHSIZE];  /* for dump */
extern int  phase_flag;     /* if phase boundary data in effect */
extern int phasemax;  /* number of phases */     

/* for BLAS block factoring */
#define BLAS_BLOCKSIZE 64

extern int fixed_constraint_flag;  /* set if restoring force valid */
extern REAL **leftside;  /* volume projection matrix */
extern REAL **rleftside;  /* volume projection matrix */
extern REAL *rightside;  /* right side of volume gradient constraints */
extern REAL *pressures; /* multiples of gradients to subtract from force */
extern REAL *vpressures;
extern int pressure_set_flag; /* pressures have been given values */
extern REAL *vol_deficit; /* bodyu volume deficits */
extern REAL *vol_restore; /* volume restoring gradient coefficients */
extern int no_refine;    /* keyword NOREFINE in data file header sets this
                                to disable initial triangulation of triangular
                                initial faces. */

/* interpolation structures */
#define MAXLEVEL 30
extern int reflevel;  /* refinement level */
extern REAL extrap_val[MAXLEVEL]; /* final value at level */

/* hessian minimization */
/* info for each vertex */
struct hess_verlist { 
            vertex_id v_id;  /* which vertex this is */
            int flags;       /* for marking if done */
            int freedom;     /* degrees of freedom */
            int rownum;      /* starting row in sparse matrix  */
#ifdef MPI_EVOLVER
            int global_rownum;
#endif
            REAL **proj;     /* projection matrix to constraints */
            REAL ***conhess; /* for constraint hessian */
            REAL slant;      /* cos of angle of normal from constraint */
          };
/* flag bits */
#define HV_DONE 1
#define SUPERNODE_DONE 2
extern int vhead_attr;  /* number of vertex attribute for vhead index */
extern int bhead_attr;  /* number of body attribute for vhead index */
#define VHEAD_ATTR_NAME "__vhead_index"
#define BHEAD_ATTR_NAME "__bhead_index"

/* sparse hessian matrix entry */
struct hess_entry { 
          REAL value;
          int col;    /* which column */
          int row;    /* which row */
       };
#define HASHEMPTY (-1)
extern int hessian_by_diff_flag; /* for crude hessian */
extern int hessian_quiet_flag;  /* 1 to suppress hessian warnings */
extern int hessian_normal_flag; /* 1 for hessian constrained to normal */
extern int hessian_special_normal_flag; /*  for user spec of Hessian direction */
extern struct expnode hessian_special_normal_expr[MAXCOORD];
extern int hessian_normal_perp_flag; /* 1 for hessian metric to normal */
extern int hessian_normal_one_flag; /* 1 for hessian constrained to 1D normal */
extern int hessian_double_normal_flag; /* 1 for hessian constrained to normal */
extern REAL hessian_slant_cutoff; /* for treating constrained vertices as
     fixed with hessian_normal */
extern int hessian_linear_metric_flag; /* linear interp dot product */
extern REAL linear_metric_mix;  /* proportion of linear interp metric */
extern REAL quadratic_metric_mix; /* proportion of quadratic interp metric */
extern int min_square_grad_flag; /* what to minimize in hessian_line_seek */
extern int hess_move_con_flag; /* whether to project to global constraints in move */
                                  /* projecting seems to be good idea */
extern REAL last_hessian_scale; /* from hessian_line_seek() */
extern REAL last_eigenvalue;    /* from eigenprobe and stuff */
extern REAL *conrhs;    /* right side of augmented matrix for constraints */
extern int total_entries; /* of hessian */
extern struct hess_verlist *vhead;  /* main element list */
extern int vhead_count;          /* total number of elements */
#ifdef XXX
extern REAL *X;                 /* solution to augmented matrix  */
extern REAL *rhs;                 /* right side of augmented matrix  */
#endif
extern int hmode; /* mode of motion */
#define SINGLE_DEGREE 2
#define NORMAL_MOTION 1
#define UNRESTRICTED  0
/* type of solution; YSMP path numbers */
#define HESS_CRITICAL 3
#define HESS_DOWNHILL 7
#define FORT
#ifdef FORT
#define A_OFF 1
#else
#define A_OFF 0
#endif
/* for controlling steps */
extern int rhs_flag;     /* filling in right hand side */
extern int hess_flag;    /* filling in hessian matrix */
extern int negdiag;      /* C index of most negative entry on diagonal */

/* Flag for doing sparse rank update for function quantities, to 
   avoid dense hessians */
extern int quantity_function_sparse_flag;

/* Lanczos default parameters */
#define KRYLOVDIM 100
#define NPRINT 15

/**************************************************************************/
/*    Linear system structure                                                             */
/*    for indefinite symmetric square systems, with linear constraints      */
/**************************************************************************/

/*************************************************************************
* The following data structure stores a node of the Metis separator tree
**************************************************************************/
struct SepNodeType {
  int nvtxs;     /* The number of vertices in the graph */
  int lo;        /* Low index of separator (inclusive) */
  int hi;        /* High index of separator (exclusive) */
  int isleaf;    /* Indicates if it is a leaf node */
  union { struct {
      REAL opc;      /* The opcount for this node */
      REAL subopc;   /* The opcount for the subtree of this node */
        } opc;  /* original metis fields */
        struct {
             size_t size;   /* size of matrix */
             int *vlist; /* list of variable numbers */
             REAL *mat;  /* triangular matrix */
        } info;  /* stuff useful in factoring */
  } u;
};
typedef struct SepNodeType SepNodeType;

struct sp_entry { int row,col;
                  REAL value;
                };
extern int blas_flag; /* whether to use BLAS */
extern int augmented_hessian_flag; /* whether to use augmented Hessian */
                                   /* -1 for unset                     */
extern int augmented_hessian_mode; /* set during factoring */
extern int sparse_constraints_flag; /* whether to store constraint gradients
               in sparse format */
extern int BK_flag; /*for enabling Bunch-Kaufman version of sparse factoring*/
extern REAL BKalpha; /* single/REAL pivot ratio */
struct linsys { int flags;  /* status bits, see below */
                int N;      /* number of variables and constraints */
                int maxN;   /* dimension IA can handle */
                int A_rows;  /* number of variables */
                int optparamrowstart;  /* first row of optimizing parameters */
                int bodyrowstart; /* where bodies start */
                int quanrowstart; /* where quantities start */
                int total_rows;   /* whole enchilada */
                /* hashing stuff, for filling of matrix */
                int table_size;  /* allocated hash table size */
                int max_fill;    /* fill limit of hash table */
                int hashcount;   /* hash entries so far */
                struct sp_entry *hashtable; /* the table itself */
                int hash_extraprobes; /* for performance measure */
                /* sparse storage of original matrix in upper triangle */
                int *IA;    /* row starts (unpermuted) */
                int *JA;    /* columns of each entry, starting on diag */
                REAL *A;    /* entry values */
                int maxA;   /* entries allocated for A and JA */
                int *P;     /* permutation in factors; P[0] is first variable */
                int *IP;    /* inverse permutation */
                struct SepNodeType *stree; /* metis separation tree */
                int streemax; /* max index of used stree */
                int maxsepsize; /* maximum separator size */
                int NSP;    /* size of work space, for ysmp */
                int *ISP;   /* work space for ysmp or whoever */
                /* permuted storage */
                int *pIA;    /* row starts */
                int *pJA;    /* columns of each entry, starting on diag */
                REAL *pA;    /* entry values */
                /* factorization */
                REAL lambda; /* shift for A  */
                int *psize;  /* types of pivots on diagonal */
                /* for mindegree factorization */
                int Lsize;   /* number of entries */
                int *LIA;    /* starting indices in LA */
                int *LJA;    /* columns for entries */
                int *LIJA;   /* starts of rows in LJA (overlapped) */
                REAL *LA;    /* values */
                /* constraint stuff */
                int concount; /* number of possible constraints */
                int CN;       /* number of actual constraints */
                int *coninx;  /* map from possible to actual constraints */
                int *coninxinv; /* inverse of coninx */
                REAL **C;     /* constraint gradients, rowwise, orth'd */
                REAL **HinvC; /* H^(-1)*C (transposed, actually) */
                REAL **CHinvCinv; /* (C^T*H^(-1)*C)^(-1) */
                /* sparse constraint stuff */
                int *CIA;  /* indexes of constraint starts in CJA,CA */
                int *CJA;  /* J for constraint gradient entry */
                REAL *CA;   /* values */
                /* inertia */
                int pos,neg,zero;
                int degencon;  /* hopefully, degenerate constraints */
                /* Approximate inverse diagonal, for metric */
                REAL *apinv;
                /* row magnitudes, for nullity detection */
                REAL *rowmag;
                /* low-rank update stuff; update of form V^T F V */
                int low_rank;   /* number of vectors */
                int low_rank_vecsize; /* length of low rank update vectors */
                REAL **low_rank_vectors; /* the vectors V themselves, rowwise */
                REAL **low_rank_form; /* F */
                REAL **low_rank_inverse_form; /* (I + F V A^-1 V)^-1 */
#ifdef MPI_EVOLVER
   /* stuff used in MPI distributed linear systems */
   int *send_counts; /* counts for general MPI scatter/gather */
   int *send_starts; /* offsets for general MPI scatter/gather */
   int *recv_counts; /* counts for general MPI scatter/gather */
   int *recv_starts; /* offsets for general MPI scatter/gather */
   int *rowstarts; /* of globals on all tasks */
   int *rowcounts; /* of globals on all tasks */
   int local_rows;
   int local_start; /* of global row numbers */
   int local_end;
   int *domain_tasks;  /* domain_tasks[i] is task hosting domain i */
   int *task_domains;  /* task_domains[i] is domain hosted on task i */
   int total_factor_fill;  /* just for info */
   int LA_spot;    /* tracker for allocation in LA workspace */
   int *IAtrans;  /* translate local row numbers in IA to global rows */
   int *IAtrans_elim;  /* translate local row numbers to elimination order */
   int *domain_IA; /* starts of rows in domain_entries list */
   int scount;  /* number of snodes actually used (info only) */
   int *listspace;  /* for lists of rows in supernodes */
   int listspacesize;
   int listspacespot;
   int *sepsizes;  /* sizes of final separators between domains */

   int *block_starts; /* interleaved domains and separators in entrylist */
   int *block_counts;
   int *domain_entry_starts; /* in entrylist, sending */
   int *domain_entry_ends; /* in entrylist, sending */
   int *sep_entry_starts; /* in entrylist, sending */
   int *sep_entry_counts;

   int local_elim;          /* rows in local domain */
   int *all_elim_starts;    /* domains and separators postorder */
   int local_elim_start;    /* handy to have */
   int local_elim_end;
   int *homeglobal_elim;    /* elims for home global rows */
   int *domain_elim_starts; /* in terms of elimination order */
   int *domain_elim_ends;   /* needed since domains and separators */
   int *sep_elim_starts;    /* interleaved in elimination order */
   int *sep_elim_ends;
   int *row_tasks;       /* home row task destinations based on elim */
   int *row_tasks_counts; /* counts of those going to each task */
   int *row_tasks_starts; /* offsets for each task */
   int *rows_from;          /* elim numbers of incoming global rows */
   int *rows_from_counts;  /* counts of incoming global rows */
   int *rows_from_starts;  /* starts of incoming global rows */
   int *domain_sepnum;   /* postorder node number of domain */
   int *sep_sepnum;   /* postorder node number of separator */
   int *block_tasks;  /* which tasks host domains/seps in interleaved order */
   struct snode *slist;       /* allocated list of supernodes, enough for
                                 each row in domain and the separator */
   struct snode *last_snode;  /* of domain */
   struct snode *sep_snode;   /* of separator */
   struct snode *csnode[2];   /* of children of separator */
   int *clist[2];   /* row nums of children of separator */
   int ctask[2];   /* tasks of children, for knowing clist and csnode order */
   int *final_list;   /* row nums in final separator */
   int total_ssize;  /* for debug */
   REAL *workspace; /* for temp stack of factoring "extra" data */
   int workspacesize;
   int *domain_counts;
   int domain_entries_size; /* number of entries in matrix for domain */
   int sep_entries_size; /* number of entries in matrix for separator */
   struct hess_entry *domain_entries; /* matrix entries for domain */
   struct hess_entry *sep_entries; /* matrix entries for separator */

#endif
            };
/* flag bits */
#define S_HESSFILLED 1
#define S_RHSFILLED  2
#define S_FACTORED    4
#define S_PROJECTED  8
#define S_ODRV_REORDERED 0x10
#define S_JA_INCLUDED 0x20
#define S_ORDERFOUND  0x40
#define S_USE_ELEMENTS 0x80

/* psize values */
/* size of diagonal elements, 1 for 1x1, 2 for first of 2x2, 
    3 for second of 2x2, 0 for zero valued 1x1 */
#define ZEROPIVOT 0
#define ONEBYONE  1
#define FIRSTOFPAIR 2
#define SECONDOFPAIR 3

/* vector-to-form  metric */
extern struct linsys Met;

extern int ysmp_flag;  /* set if doing Yale Sparse Matrix version */
/* sparse matrix function pointers, for easy switching among algorithms */
/* values for ysmp_flag */
#define MINDEG_FACTORING 0
#define YSMP_FACTORING 1
#define METIS_FACTORING 2

/* for mindeg control */
extern int mindeg_debug_level;
extern int mindeg_margin;
extern int mindeg_min_region_size;

/* multiply vector by original sparse matrix */
extern void (*sp_mul_func)ARGS((struct linsys *, REAL*,REAL*));

/* convert raw Hessian data to standard sparse format */
extern void (*sp_AIJ_setup_func)ARGS((int,struct linsys*));

/* set up  matrices needed for handling constraints */
extern void (*sp_constraint_setup_func)ARGS((int, struct linsys *));

/* set up projection to constraints using hessian metric */
extern void (*sp_hess_project_setup_func)ARGS((struct linsys *)) ;

/* factor matrix */
extern void (*sp_factor_func)ARGS((struct linsys *)) ;

/* Matrix inner product using hessian inverse */
extern void (*sp_CHinvC_func)ARGS((struct linsys *)) ;

/* return a vector in kernel, supposing nullity > 0 */
extern int (*sp_kernel_func)ARGS((struct linsys *,REAL *)) ;

/* solve given rhs */
extern void (*sp_solve_func)ARGS((struct linsys *,REAL *,REAL *)) ;

/* solve multiple given rhs */
extern void (*sp_solve_multi_func)ARGS((struct linsys*,REAL**,REAL**,int));

/* optional ordering of vertices */
extern void (*sp_ordering_func)ARGS((struct linsys *)) ;


/* to suppress a proliferation of warnings */
extern int pos_def_warning_flag;
extern int eigen_pos,eigen_neg,eigen_zero;  /* inertia of shifted hessian */
extern int mat_index; /* number of negatives on diagonal */
extern int mat_null; /* number of zeroes on diagonal */
extern REAL hessian_epsilon; /* cutoff for diagonal elements */
extern REAL hessian_epsilon_default; 
extern int make_pos_def_flag;    /* force hessian to positive definiteness */
extern int hess_debug;  /* debugging flag */


/* for facet area calculation */
#define AREA_ONLY 1
#define ALL_ENERGIES 2

/* model dependent function pointers */
#ifdef NOPROTO
extern REAL (*userfunc[])();
extern REAL (*userfunc_deriv[])();
extern REAL (*userfunc_seconds[])();
extern void (*calc_facet_energy)();
extern void (*calc_facet_forces)();
extern void (*calc_facet_volume)();
extern void (*calc_edge_energy)();
extern void (*calc_edge_forces)();
extern void (*calc_edge_area)();
extern void (*string_grad)();
extern void (*film_grad)();
#else
extern REAL (*userfunc[])(REAL*);
extern REAL (*userfunc_deriv[])(REAL*,REAL*);
extern REAL (*userfunc_seconds[])(REAL*,REAL*,REAL**);
extern void (*calc_facet_energy)(facet_id,int);
extern void (*calc_facet_forces)(facet_id);
extern void (*calc_facet_volume)(facet_id);
extern void (*calc_edge_energy)(edge_id);
extern void (*calc_edge_forces)(edge_id);
extern void (*calc_edge_area)(edge_id);
extern void (*string_grad)(void);
extern void (*film_grad)(void);
#endif

/* gaussian integration on [0,1] */
extern REAL *gauss1Dpt;
extern REAL *gauss1Dwt;
extern int  gauss1D_num;
extern REAL **gauss1poly;  /* interp polys at gauss pts */
extern REAL **gauss1polyd; /* and derivatives              */
                  /* values for linear or quadratic model, whichever in effect */
extern int  edge_ctrl; /* control points on edge, 2 linear, 3 quadratic */

/* 2D gauss */
typedef REAL barytype[3];
extern barytype *gauss2Dpt;
extern REAL *gauss2Dwt; 
extern REAL gauss2Dpt1[1][3]; 
extern REAL gauss2Dwt1[1];
extern REAL gauss2Dpt2[3][3]; 
extern REAL gauss2Dwt2[3];
extern REAL gauss2Dpt5[7][3]; 
extern REAL gauss2Dwt5[7];
extern REAL gauss2Dpt6[12][3]; 
extern REAL gauss2Dwt6[12];
extern REAL gauss2Dpt8[16][3]; 
extern REAL gauss2Dwt8[16];
extern REAL gauss2Dpt11[28][3]; 
extern REAL gauss2Dwt11[28];
extern REAL gauss2Dpt13[37][3]; 
extern REAL gauss2Dwt13[37];

/* general dimension gauss integration variables and arrays */
extern int ctrl_num;  /* number of control points */
extern int gauss2D_num; /* number of integration points */
extern REAL **gpoly;    /* interpolation polynomial value at integration point k
                         for polynomial of control point j */
extern REAL ***gpolypartial; /* partials of interpolation polynomials */
                         /* gausspt,dim,ctrlpt */

#define MAXLAGRANGE 20

/* new gaussian-lagrange structure */
#define MAXGAUSSORDER 40
struct gauss_lag { int lagrange_order;  /* computed for */
                   int  gnumpts;        /* number of gauss points */
                   REAL **gausspt;      /* gaussian integration pts,
                                           in barycentric coords add to 1 */
                   REAL *gausswt;       /* weights at gauss pts */
                   int  lagpts;         /* number of lagrange pts */
                   REAL **gpoly;        /* basis polynomial of lagrange pt
                                           at gauss pt; index [g][l];
                                           mat mult lagrange to get gauss */
                   REAL ***gpolypart;   /* partials of basis polynomials;
                                           index [g][dim][l];  mult lagrange
                                           to get tangents */
                   REAL ***lpolypart;   /* partials at lagrange pts */
               };
extern struct gauss_lag *gauss_lagrange[MAXCOORD];
extern int maxgaussorder[MAXCOORD]; /* allocated for each dimension */

extern int bezier_flag; /* whether to do Bezier basis polynomials in Lagrange */
extern REAL **bezier1invert[MAXLAGRANGE];
extern REAL **bezier1revert[MAXLAGRANGE];
extern REAL **bezier_refine_1d[MAXLAGRANGE];
extern REAL **bezier2invert[MAXLAGRANGE];
extern REAL **bezier2revert[MAXLAGRANGE];
extern REAL **bezier_refine_2d[MAXLAGRANGE];

extern REAL **metric;  /* metric values at a point */
extern REAL ***metric_partial; /* partial derivatives of metric */
extern REAL **det_array;    /* tangent vector dot products */
/*extern REAL **tang; */    /* tangents to surface at point */
extern REAL euclidean_area;    /* euclidean area for conformal metrics */
extern int klein_metric_flag;
extern int metric_convert_flag; /* whether to do form-to-vector conversion */

/* for defining pointer-pointer matrices as local variables */

/* need token-pasting, so gets weird with testing style needed */
#define bbxt 1
#define cxtc
#define bbxtcxtc  2
#if (__STDC__ == 1) || defined(__SUNPRO_C)
#define ddxt 1
#else
#define ddxt  bbxt/**/cxtc
#endif

#if  (ddxt == 1)

#define MAT2D(name,rows,cols)  \
  REAL *name##qXvS[rows];\
  REAL name##xJ[rows][cols];\
  REAL **name = mat2d_setup(name##qXvS,(REAL*)name##xJ,rows,cols)

#define MAT3D(name,rows,cols,levels)  \
  REAL **name##qXvS[(rows)+(rows)*(cols)];\
  REAL name##xJ[rows][cols][levels];\
  REAL *** name=mat3d_setup(name##qXvS,(REAL*)name##xJ,rows,cols,levels)

#define MAT4D(name,rows,cols,levels,tiers)  \
  REAL ***name##qXvS[(rows)+(rows)*(cols)+(rows)*(cols)*(levels)];\
  REAL name##xJ[rows][cols][levels][tiers];\
  REAL ****name=mat4d_setup(name##qXvS,(REAL*)name##xJ,rows,cols,levels,tiers)

#else

#define MAT2D(name,rows,cols)  \
  REAL *name/**/qXvS[rows];\
  REAL name/**/xJ[rows][cols];\
  REAL ** name = mat2d_setup(name/**/qXvS,(REAL*)name/**/xJ,rows,cols)

#define MAT3D(name,rows,cols,levels)  \
  REAL **name/**/qXvS[(rows)+(rows)*(cols)];\
  REAL name/**/xJ[rows][cols][levels];\
  REAL *** name = mat3d_setup(name/**/qXvS,(REAL*)name/**/xJ,rows,cols,levels)

#define MAT4D(name,rows,cols,levels,tiers)  \
  REAL ***name/**/qXvS[(rows)+(rows)*(cols)+(rows)*(cols)*(levels)];\
  REAL name/**/xJ[rows][cols][levels][tiers];\
  REAL **** name=mat4d_setup(name/**/qXvS,(REAL*)name/**/xJ,rows,cols,levels,tiers)
#endif
/* end matrix defines */


struct veredge { vertex_id v_id; edge_id e_id; }; /* used in verpopst.c */

/* Text display */
#define MAXTEXTS 100
struct text_s {
   char *text; /* null-terminated string */
   int  alloc; /* size of text allocation */
   REAL  start_x,start_y; /* starting position in 0-1 window coords */
   REAL vsize;  /* vertical size, pixels?? */
};
extern struct text_s text_chunks[MAXTEXTS];
extern int display_text_count;

/* symmetry group stuff */
extern char *symmetry_name;    /* to be sure datafile matches */
#ifdef NOPROTO
typedef void SYM_WRAP();      /* points to user-defined wrap function */
typedef WRAPTYPE SYM_COMP(); /* points to group composition function */
typedef WRAPTYPE SYM_INV (); /* points to group inverse function      */
typedef void SYM_FORM();      /* points to form pullback function      */
#else
typedef void SYM_WRAP(REAL*,REAL*,WRAPTYPE);
typedef WRAPTYPE SYM_COMP(WRAPTYPE,WRAPTYPE);
typedef WRAPTYPE SYM_INV (WRAPTYPE);
typedef void SYM_FORM(REAL*,REAL*,REAL*,WRAPTYPE);
#endif
extern SYM_WRAP *sym_wrap;
extern int         sym_flags;
extern SYM_FORM *sym_form_pullback;
extern SYM_INV  *sym_inverse;
extern SYM_COMP *sym_compose;
/* structure for registry.c */
struct sym_registry {
    char *name;  /* name to match in datafile SYMMETRY_GROUP phrase */
    int flags;  /* see bits below */
    SYM_WRAP *wrapper; /* point wrap function */
    SYM_COMP *compose; /* group composition function */
    SYM_INV  *inverse; /* group inverse function */
    SYM_FORM *pullback; /* form pullback function */
    };
 extern struct sym_registry sym_register[]; /* actual in registry.c */
/* flag bits */
#define HAS_FIXED_PTS 1
#define NEED_FORM_UNWRAPPING 2
#define DOUBLE_AXIAL 4
    

/* specific case of torus symmetry representation */
#define TWRAPBITS 6
#define POSWRAP    1
#define WRAPMASK  037
#define ALLWRAPMASK 03737373737
#define NEGWRAP    WRAPMASK
#define WRAPNUM(x) ( (x)>(1<<(TWRAPBITS-2)) ? (x)-(1<<(TWRAPBITS-1)) : (x))

/* for symmetry group "rotate" */
extern int rotorder;

/* additional viewing transforms */
extern int transform_count;
extern REAL ***view_transforms;
extern int view_transforms_global; /* global var number */
extern int *view_transform_det; /* to see if normals need flipping */
extern int transforms_flag; /* whether to show transforms */
extern int transform_gen_count;
typedef struct expnode expnodearray[MAXCOORD][MAXCOORD];
extern expnodearray *view_transform_gens_expr;    /* generator expressions */
extern REAL ***view_transform_gens;    /* generator values */
extern char transform_expr[100];  /* save it */
extern int transform_depth;  /* tree depth in transform generation */
extern int *transform_colors;
extern int view_transform_swap_colors_global; /* global var number */
extern int *transform_parity;
extern int *transform_gen_swap;
#define SAME_COLOR  0
#define SWAP_COLORS 1 
extern int transform_colors_flag;

/* color for transparent facets */
#define CLEAR (-1)
/* color for unshown facet */
#define UNSHOWN (-2)
/* color for saying color is not set */
#define NOT_A_COLOR (-3)

/* from borlandc/graphics.h */
#if      !defined(__COLORS)
#define __COLORS

enum COLORS {
     BLACK,    /* dark colors */
     BLUE,
     GREEN,
     CYAN,
     RED,
     MAGENTA,
     BROWN,
     LIGHTGRAY,
     DARKGRAY, /* light colors */
     LIGHTBLUE,
     LIGHTGREEN,
     LIGHTCYAN,
     LIGHTRED,
     LIGHTMAGENTA,
     YELLOW,
     WHITE
};
#endif


#ifdef SHARED_MEMORY
/* process ids */
extern int proc_ids[MAXPROCS];

#define M_ACTIVE 423
#define M_INACTIVE 0
extern int mpflag;  /* whether multiprocessing in action */
extern int m_breakflag[MAXPROCS]; /* for user interrupts */

/* locks and stuff */
#ifdef SGI_MULTI
extern usptr_t *lock_arena;  /* arena where locks are */
extern ulock_t locklist[_MAXLOCKS];  /* only 4096 available */
extern char lock_arena_name[]; /* for usinit() */
#endif
#endif

/* for keeping multiprocessor force updates from conflicting */
struct procforce { vertex_id v_id;
                   int next;  /* link */
                   REAL f[MAXCOORD];
                 };
extern struct procforce *pforce;
extern int phead[MAXPROCS][MAXPROCS]; /* [owner][calculator] */
extern struct procforce *pbase[MAXPROCS];  /* allocated to calculator */
extern int ptop[MAXPROCS];    /* used per calculator */
extern int pmax[MAXPROCS];    /* available per calculator */

/* Graphing mutual exclusion for multi-threaded version */
#define IMMEDIATE_TIMEOUT 0
#define LONG_TIMEOUT 100000
#ifdef WIN32
extern void * graphmutex;
extern void * mem_mutex;
extern unsigned int locking_thread; /* so we can tell who has it */
/* Note ENTER_GRAPH_MUTEX and LEAVE_GRAPH_MUTEX form a {} block! */
/* Also set up to be safe to nest within a thread. */
#define ENTER_GRAPH_MUTEX { int did_graphlock_here;\
     if ( locking_thread != GetCurrentThreadId() ) \
   { MsgWaitForMultipleObjects(1,&graphmutex,0,100000,0); \
     locking_thread = GetCurrentThreadId(); \
     did_graphlock_here = 1; } \
    else did_graphlock_here = 0; 
#define TRY_GRAPH_MUTEX(timeout) ( \
   (MsgWaitForMultipleObjects(1,&graphmutex,0,timeout,0)!=WAIT_TIMEOUT) && \
     ((locking_thread = GetCurrentThreadId())!=0) )
#define END_TRY_GRAPH_MUTEX    \
     {locking_thread = 0; ReleaseMutex(graphmutex);}
#define LEAVE_GRAPH_MUTEX   if ( did_graphlock_here ) \
     {locking_thread = 0; ReleaseMutex(graphmutex); did_graphlock_here = 0; } } 
#define ABORT_GRAPH_MUTEX  \
    if ( GetCurrentThreadId() == locking_thread )\
     {locking_thread = 0; ReleaseMutex(graphmutex); } else {}
#define ENTER_MEM_MUTEX { MsgWaitForMultipleObjects(1,&mem_mutex,0,100000,0); }
#define LEAVE_MEM_MUTEX  { ReleaseMutex(mem_mutex);  }

#elif defined(PTHREADS)
extern pthread_mutex_t graphmutex;
extern pthread_t locking_thread; /* so we can tell who has it */
#define ENTER_GRAPH_MUTEX {int did_graphlock_here;\
    if ( locking_thread != pthread_self() ) \
     { pthread_mutex_lock(&graphmutex);locking_thread=pthread_self();\
       did_graphlock_here = 1;}\
    else did_graphlock_here = 0;
#define TRY_GRAPH_MUTEX(timeout) ((pthread_mutex_trylock(&graphmutex)!=EBUSY) &&\
               (locking_thread=pthread_self()))
#define LEAVE_GRAPH_MUTEX  if ( did_graphlock_here ) \
        {locking_thread=0;pthread_mutex_unlock(&graphmutex);} }
#define END_TRY_GRAPH_MUTEX \
        {locking_thread=0;pthread_mutex_unlock(&graphmutex);} 
#define ABORT_GRAPH_MUTEX  if ( locking_thread == pthread_self() ) \
        {locking_thread=0;pthread_mutex_unlock(&graphmutex);}  else {}
#define ENTER_GRAPH_MUTEX_PR  printf("Lock %s:%d\n",__FILE__,__LINE__);pthread_mutex_lock(&graphmutex);
#define LEAVE_GRAPH_MUTEX_PR printf("Unlock %s:%d\n",__FILE__,__LINE__); pthread_mutex_unlock(&graphmutex); 
extern pthread_mutex_t mem_mutex;
#define ENTER_MEM_MUTEX  pthread_mutex_lock(&mem_mutex);
#define LEAVE_MEM_MUTEX  pthread_mutex_unlock(&mem_mutex); 
#else
#define ENTER_GRAPH_MUTEX {}
#define LEAVE_GRAPH_MUTEX  {}
#define ENTER_MEM_MUTEX {}
#define LEAVE_MEM_MUTEX  {}
#define TRY_GRAPH_MUTEX(timeout)  1
#define ABORT_GRAPH_MUTEX {} 
#endif

/* Multithreading with worker threads */
#ifdef WIN32
extern volatile LONG busythreads;  /* number of threads that have started on a task */
#else
extern int busythreads;  /* number of threads that have started on a task */
#endif
extern int threadflag;   /* 1 if -p option used and worker threads in effect */
extern int thread_task;  /* which task to do */
extern element_id global_id;   /* global iteration variable */
extern int web_locks;  /* informational counters */
extern int thread_locks;
extern int element_locks;
extern int web_unlocks;
extern int thread_unlocks;
extern int element_unlocks;
/* Task definitions */
#define TH_PROJECT_ALL_TEST    1
#define TH_PROJECT_ALL_ACTUAL  2
#define TH_CALC_FACET_ENERGY   3
#define TH_CALC_FACET_FORCES   4
#define TH_MULTI_CALC_QUANT    5
#define TH_MULTI_QUANT_GRADS   6
#define TH_MULTI_QUANT_HESS    7
#define TH_FIX_GRADS           8
#define TH_MOVE_VERTICES       9

#ifdef PTHREAD_LOG
struct thread_event { /* for tracking thread events */
         int type;
         unsigned int time_high;  /* from rdtsc() */
         unsigned int time_low;
   };
#define MAXTHREADEVENTS 10000
extern struct thread_event main_events[MAXTHREADEVENTS];
extern int main_eventcount;
#endif

/* for controlling thread sweep blocks in disjoint stages */
extern int v_partition_coord_attr; /* number of partitioning attribute */
extern int e_partition_coord_attr; /* number of partitioning attribute */
extern int f_partition_coord_attr; /* number of partitioning attribute */
extern int b_partition_coord_attr; /* number of partitioning attribute */
extern int fe_partition_coord_attr; /* number of partitioning attribute */
extern int v_partition_stage_attr;  /* which stage element is in */
extern int v_partition_proc_attr;   /* which processor element is assigned to */
extern int e_partition_stage_attr;  /* which stage element is in */
extern int e_partition_proc_attr;   /* which processor element is assigned to */
extern int f_partition_stage_attr;  /* which stage element is in */
extern int f_partition_proc_attr;   /* which processor element is assigned to */
extern int b_partition_stage_attr;  /* which stage element is in */
extern int b_partition_proc_attr;   /* which processor element is assigned to */
extern int fe_partition_stage_attr;  /* which stage element is in */
extern int fe_partition_proc_attr;   /* which processor element is assigned to */

/* Extra stage in proc 0 will be for elements too big for one stage */
#define MAXTHREADSTAGES 9
struct thread_stages_data {
     element_id *blocks[NUMELEMENTS][MAXTHREADSTAGES];
     int counts[NUMELEMENTS][MAXTHREADSTAGES];
     int allocated[NUMELEMENTS][MAXTHREADSTAGES];
volatile   int stage; /* current stage, can be read by other procs */
     int spot; /* in current block */
};
extern struct thread_stages_data *thread_stages;
extern int max_thread_stages; /* actual number of stages to do */
extern long partition_timestamp;  /* last time partitioning done */
 
struct thread_data {  /* per thread data, pointed to by thread_data_key */
         int worker_id;    /* number of created thread, 0 to nproc-1 */
         int task_serial_number; 
         int task_state;  /* for debugging */
         int in_crit;  /* for debugging */
         element_id iteration_id;
         REAL total_energy;
         REAL total_area;

         /* stuff for single-stack-per-thread implementation of eval() */
         REAL *eval_stack;     /* stack start */
         int eval_stack_size;  /* allocated size */
         REAL *stack_top;      /* occupied stack top */
         int frame_spot; /* start of topmost frame */

#ifdef _MSC_VER
     __int64 stagestart[MAXTHREADSTAGES];
     __int64 stageend[MAXTHREADSTAGES];
#endif
#ifdef PTHREAD_LOG
         int eventcount;  /* for timestamping events */
         struct thread_event events[MAXTHREADEVENTS];
#endif
   };
extern struct thread_data **thread_data_ptrs;  /* one per thread to be allocated */

/* Note: all system-specific thread stuff is in tmain.c */

/* in case some header file uses these, like windows.h */
#undef LOCK_ELEMENT
#undef UNLOCK_ELEMENT

#ifdef PTHREADS
#include <pthread.h>
extern void * element_mutex_ptr;
extern void * web_mutex_ptr;
#define GET_THREAD_DATA (struct thread_data *)pthread_getspecific(thread_data_key)
#define LOCK_ELEMENT(id)  (threadflag ? mylock_element(id) : 0)
#define UNLOCK_ELEMENT(id)  (threadflag ?  (elptr(id)->lock = 0) : 0)
#define LOCK_WEB  (threadflag ? ((pthread_mutex_trylock(web_mutex_ptr)==EBUSY)?\
           (pthread_mutex_lock(web_mutex_ptr),web_locks++): web_unlocks++ ): 0)
#define UNLOCK_WEB  (threadflag ? pthread_mutex_unlock(web_mutex_ptr): 0)
extern pthread_key_t thread_data_key;
#define GET_THREAD_ID  (((struct thread_data*)(GET_THREAD_DATA))->worker_id)
#endif

#ifdef WINTHREADS
#ifndef _WINDOWS_
/* some stuff so windows.h doesn't have to be included everywhere */
extern LPCRITICAL_SECTION element_mutex_ptr;
extern LPCRITICAL_SECTION web_mutex_ptr;
int __stdcall TryEnterCriticalSection(void *);
void __stdcall EnterCriticalSection(void *);
void __stdcall LeaveCriticalSection(void *);
#endif
struct thread_data *win_get_thread_data(unsigned long);
#define GET_THREAD_DATA win_get_thread_data(thread_data_key)
#define GET_THREAD_ID  (((struct thread_data*)(GET_THREAD_DATA))->worker_id)
extern unsigned long thread_data_key;
#define LOCK_ELEMENT(id)  (threadflag?mylock_element(id):0)
#define UNLOCK_ELEMENT(id)  (threadflag?myunlock_element(id):0)
#define LOCK_WEB (threadflag?mylock_web():0)
#define UNLOCK_WEB (threadflag?myunlock_web():0)
#endif

// for systems without threads activated
extern struct thread_data default_thread_data;
extern struct thread_data *default_thread_data_ptr;

#ifndef THREADS
#define LOCK_WEB 
#define UNLOCK_WEB
#define LOCK_ELEMENT(id)
#define UNLOCK_ELEMENT(id)
#define GET_THREAD_DATA (&default_thread_data)
#define GET_THREAD_ID 0
#endif

/* global communication to threads */
extern int m_hess_mode;
extern int m_rhs_mode;
extern int m_type;
extern int m_mode;
extern REAL *m_rhs;

extern  double   cpu_speed;
#if defined(_MSC_VER) && !defined(__BORLANDC__) && !(_IA64_==1) && !defined(_WIN64)
/***************************************************************************
  Some macros for cycle-counting profiling.
  Usage: Put PROF_START(f) at start of function f, PROF_FINISH(f) at end,
   define global
__int32 f_elapsed_time[2];
   and print when done with
  PROF_PRINT(f);
  Make sure all return paths go through end of function. 
***************************************************************************/
/* always do PROF_NOW, so can get CPU counter even if not profiling */
#define PROF_NOW(fullname) {  __asm {rdtsc} __asm {mov fullname,eax}      \
        __asm { mov fullname[4],edx}   }
#define PROF_CYCLES(fullname) ((double)*(__int64*)fullname)
#ifdef PROF_EVALS
#define PROF_EVAL_START(ex) { __asm {rdtsc} __asm {mov eval_elapsed_time,eax}   \
    __asm {mov eval_elapsed_time[4],edx}                                  \
    (ex)->elapsed_time -=  (((REAL)*(__int64*)eval_elapsed_time)); }
#define PROF_EVAL_END(ex) { __asm {rdtsc} __asm {mov eval_elapsed_time,eax}   \
    __asm {mov eval_elapsed_time[4],edx}                                  \
    (ex)->elapsed_time +=  (((REAL)*(__int64*)eval_elapsed_time)); }
#endif

#ifdef PROFILING
#define PROFILING_ENABLED
#define PROF_START(f) { __asm {rdtsc} __asm {sub f##_elapsed_time,eax}      \
        __asm {sbb f##_elapsed_time[4],edx}   }
#define PROF_FINISH(f)  { __asm {rdtsc} __asm {add f##_elapsed_time,eax}   \
    __asm {adc f##_elapsed_time[4],edx} }
#define PROF_ELAPSED(f) (((REAL)*(__int64*)f##_elapsed_time)/cpu_speed)
#define PROF_RESET(f) f##_elapsed_time[0]=f##_elapsed_time[1]=0;
#define PROF_PRINT(f) { fprintf(stderr,"%22s: %13.0f\n",#f,\
    (double)*(__int64*)f##_elapsed_time); \
   f##_elapsed_time[0]=f##_elapsed_time[1]=0;}
#define METHOD_PROFILING_START(mi,mode) {PROF_RESET(mode);PROF_START(mode);}
#define METHOD_PROFILING_END(mi,mode) {PROF_FINISH(mode);mi->mode##_elapsed_time += PROF_ELAPSED(mode);mi->mode##_call_count++;}
#endif

#elif defined(__i386) && defined(__GNUC__) && !defined(__STRICT_ANSI__)
  /* operands are in source-destination order in gcc asm */
#define PROF_NOW(fullname) { \
  asm("rdtsc" : : ); \
  asm("movl %%eax,%0" : "=m"(fullname[0]) : ); \
  asm("movl %%edx,%0" : "=m"(fullname[1]) : ); }
#define PROF_CYCLES(fullname) ((double)*(unsigned long long*)fullname)
#ifdef PROF_EVALS
#define PROF_EVAL_END { \
  asm("push %%eax" : :); \
  asm("push %%edx" : :); \
  asm("rdtsc" : : ); \
  asm("add %%eax,%0" : "=m"(eval_elapsed_time) : ); \
  asm("adc %%edx,%0" : "=m"(eval_elapsed_time[1]) : ); \
  asm("pop %%edx" : :); \
  asm("pop %%eax" : :); \
    (ex)->elapsed_time -=  (((REAL)*(unsigned long long*)eval_elapsed_time)); }
#define PROF_EVAL_END { \
  asm("push %%eax" : :); \
  asm("push %%edx" : :); \
  asm("rdtsc" : : ); \
  asm("add %%eax,%0" : "=m"(eval_elapsed_time) : ); \
  asm("adc %%edx,%0" : "=m"(eval_elapsed_time[1]) : ); \
  asm("pop %%edx" : :); \
  asm("pop %%eax" : :); \
    (ex)->elapsed_time +=  (((REAL)*(unsigned long long*)eval_elapsed_time)); }
#endif
#ifdef PROFILING
#define PROFILING_ENABLED
#define PROF_START(f) { \
  asm("push %%eax" : :); \
  asm("push %%edx" : :); \
  asm("rdtsc" : : ); \
  asm("sub %%eax,%0" : "=m"(f##_elapsed_time) : ); \
  asm("sbb %%edx,%0" : "=m"(f##_elapsed_time[1]) : ); \
  asm("pop %%edx" : :); \
  asm("pop %%eax" : :); } 
#define PROF_FINISH(f) { \
  asm("push %%eax" : :); \
  asm("push %%edx" : :); \
  asm("rdtsc" : : ); \
  asm("add %%eax,%0" : "=m"(f##_elapsed_time) : ); \
  asm("adc %%edx,%0" : "=m"(f##_elapsed_time[1]) : ); \
  asm("pop %%edx" : :); \
  asm("pop %%eax" : :); } 
#define PROF_ELAPSED(f) (((double)*(unsigned long long*)f##_elapsed_time)/cpu_speed)
#define PROF_RESET(f) f##_elapsed_time[0]=f##_elapsed_time[1]=0;
#define PROF_PRINT(f) { fprintf(stderr,"%22s: %13.0f\n",#f,\
    (double)*(unsigned long long*)f##_elapsed_time); \
   f##_elapsed_time[0]=f##_elapsed_time[1]=0;}
#define METHOD_PROFILING_START(mi,mode) {PROF_RESET(mode);PROF_START(mode);}
#define METHOD_PROFILING_END(mi,mode) { PROF_FINISH(mode); \
  mi->mode##_elapsed_time += PROF_ELAPSED(mode);mi->mode##_call_count++;}
#endif
#endif

#ifndef PROF_NOW
#define PROF_NOW(f)  f[0] = f[1] = 0;
#define PROF_CYCLES(f) 0.0
#endif

#ifndef PROFILING_ENABLED
#define PROF_START(f)   
#define PROF_FINISH(f)  
#define PROF_ELAPSED(f)  0.0
#define PROF_RESET(f)   
#define PROF_PRINT(f)   
#define METHOD_PROFILING_START(mi,mode) 
#define METHOD_PROFILING_END(mi,mode) 
#endif

#ifndef PROF_EVAL_START
#define PROF_EVAL_START
#define PROF_EVAL_END
#endif

extern int element_setup_elapsed_time[2];
extern int calc_quants_elapsed_time[2];
extern int calc_quant_grads_elapsed_time[2];
extern int calc_quant_hess_elapsed_time[2];
extern int exparse_elapsed_time[2];
extern int yyparse_elapsed_time[2];
extern int yylex_elapsed_time[2];
extern int kblex_elapsed_time[2];
extern int hessian_solve_elapsed_time[2];
extern int hessian_mul_elapsed_time[2];
extern int hessian_AIJ_setup_elapsed_time[2];
extern int hessian_constraint_setup_elapsed_time[2];
extern int hessian_project_setup_elapsed_time[2];
extern int hessian_factor_elapsed_time[2];
extern int hessian_CHinvC_elapsed_time[2];


#ifdef _MSC_VER
/* Elaborate macro to make element loop fast and local.
   Usage:  THREAD_FOR_ALL_NEW(elementtype,action)
   where action is executable code.  action should refer to
   the element id as *idptr.
   action can be extensive code block, but do not use
   comma in declaration lists; comma ok in argument lists,
   but if not in nested parentheses, then looks like end
   of action argument.
*/

#define THREAD_FOR_ALL_NEW(elementtype,action)                       \
{ element_id *idptr;                                                 \
  int proc,nextproc;                                                 \
  struct thread_stages_data *th;                                     \
  struct thread_data *data;                                          \
  int maxstage;                                                      \
  int elnum,maxelnum;                                                \
  __int32 now[2];                                                    \
                                                                     \
  data = GET_THREAD_DATA;                                            \
  proc = data->worker_id;                                            \
  nextproc = (proc == nprocs - 1) ? 0 : proc+1;                      \
  maxstage = (proc == 0) ? max_thread_stages+1 : max_thread_stages;  \
  th = thread_stages + proc;                                         \
                                                                     \
  for ( th->stage = 0 ; th->stage < maxstage ; th->stage++ )         \
  { /* wait for nextproc to reach same stage */                      \
    while ( thread_stages[nextproc].stage < th->stage ) ;            \
                                                                     \
    PROF_NOW(now);                                                   \
    data->stagestart[th->stage] = *(__int64*)now;                    \
                                                                     \
    /* do all elements in this stage */                              \
    idptr = th->blocks[elementtype][th->stage];                      \
    maxelnum = th->counts[elementtype][th->stage];                   \
    for ( elnum = 0 ; elnum < maxelnum ; elnum++,idptr++ )           \
     action;                                                         \
                                                                     \
    PROF_NOW(now);                                                   \
    data->stageend[th->stage] = *(__int64*)now;                      \
  }                                                                  \
}           

/* end THREAD_FOR_ALL_NEW */

extern __int64 thread_launch_start;
extern __int64 thread_launch_end;

#else
#define THREAD_FOR_ALL_NEW(elementtype,action)                       \
{ element_id *idptr;                                                 \
  int proc,nextproc;                                                 \
  struct thread_stages_data *th;                                     \
  struct thread_data *data;                                          \
  int maxstage;                                                      \
  int elnum,maxelnum;                                                \
  int now[2];                                                    \
                                                                     \
  data = GET_THREAD_DATA;                                            \
  proc = data->worker_id;                                            \
  nextproc = (proc == nprocs - 1) ? 0 : proc+1;                      \
  maxstage = (proc == 0) ? max_thread_stages+1 : max_thread_stages;  \
  th = thread_stages + proc;                                         \
                                                                     \
  for ( th->stage = 0 ; th->stage < maxstage ; th->stage++ )         \
  { /* wait for nextproc to reach same stage */                      \
    while ( thread_stages[nextproc].stage < th->stage ) ;            \
                                                                     \
    /* do all elements in this stage */                              \
    idptr = th->blocks[elementtype][th->stage];                      \
    maxelnum = th->counts[elementtype][th->stage];                   \
    for ( elnum = 0 ; elnum < maxelnum ; elnum++,idptr++ )           \
     action;                                                         \
                                                                     \
  }                                                                  \
}           
/* end THREAD_FOR_ALL_NEW */

#endif

/* profiling storage */
extern int q_facet_setup_elapsed_time[2];

#ifdef  __cplusplus
}
#endif

