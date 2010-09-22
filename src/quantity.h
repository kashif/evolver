/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/******************************************************************
*
*  File:  quantity.h
*
*  Purpose:  Header file for general quantities.
*/

struct qgrad { REAL *g;};
struct qhess { REAL **gg;};
#ifndef MAXMETH
#define MAXMETH 4 
#endif

#ifdef NOPROTO
#define QINT
#define QINFO
#else
#define QINT int
#define QINFO struct qinfo *
typedef struct method_instance *MIPTR;
#endif

extern int gen_quant_list_max;
extern int gen_quant_free_left;
extern int meth_inst_list_max;

#define MAXVCOUNT 100
/* structure for passing info to element methods */
struct qinfo {
    element_id id;
    int flags;  /* see below */
    int method;    /* instance number; unreliable to use pointer here */
    int vcount;  /* number of vertices involved */
    vertex_id v[MAXVCOUNT];  /* vertex list */
    REAL *x[MAXVCOUNT];        /* vertex coordinates */
    WRAPTYPE wraps[MAXVCOUNT];  /* wraps of individual vertices */
    REAL **xx; /* room if facet coords need unwrapping */
    REAL **u;  /* room for affine coords of vertices in torus model */
    REAL **uu[3]; /* ptrs to edge vertex coords for torus volume */
    REAL **ugrad;  /* room for affine gradient in torus model */
    REAL **uugrad[3]; /* ptrs to edge vertex grads for torus volume */
    REAL ****uhess;  /* room for affine hessian in torus model */
    REAL ****uuhess[3]; /* ptrs to edge vertex hessians for torus volume */
    REAL **gauss_pt; /* for gaussian integration */
    int gauss_num;
    REAL ***sides;  /* edge or facet sides, or tangents at gauss pts in quad */
    REAL **ss;      /* matrix of dot products of sides */
    REAL normal[MAXCOORD];  /* facet normal */
    REAL **grad; /* vertex gradients */
    REAL ****hess; /* vertex hessians */
    int axial_order; /* number of times vertex repeats around axial point */
    struct linsys *S;  /* linear system associated with hessian */
    };

/* flags values */
#define INCOMPLETE_STAR  1

/* for type of initialization */
#define METHOD_VALUE      1767
#define METHOD_GRADIENT  4321
#define METHOD_HESSIAN    8763


typedef void (*INIT_METHOD)ARGS((QINT,MIPTR));
typedef REAL (*VALUE_METHOD)(QINFO);
typedef REAL (*GRAD_METHOD)(QINFO);
typedef REAL (*HESS_METHOD)(QINFO);
typedef void (*CLEANUP_METHOD) ARGS((void));

/* structure for one quantity */
#define QNAMESIZE 128
struct gen_quant {
    char name[QNAMESIZE];   /* for identification */
    int num;                /* number in gen_quant list */
    int flags;              /* see defines below */
                            /* used for Q_ENERGY,Q_FIXED, Q_CONSERVED, or Q_INFO */
    REAL target;            /* for fixed quantities */
    REAL value;             /* current value */
    REAL oldvalue;          /* value for restore_coords() */
    REAL abstotal;          /* total abs val of elements, for constraint tol */
    REAL modulus;           /* overall multiplier */
    REAL tolerance;         /* for fixed quantity */
    REAL pressure;          /* Lagrange multiplier */
    REAL oldpressure;       /* for restore_coords() */
    REAL volconst;          /* additive constant, for torus volume */
    body_id b_id;           /* body this is volume of, if any */
    int  vol_number;        /* equivalent body number for fixvol */
    int  fixnum;            /* order in fixvol computations */
    struct qgrad *grad;     /* gradients at vertices */
    struct qhess *hess;     /* hessian on vertices and edges */
    int vhead_index;        /* index into vhead list for hessian */
    int method_count;       /* number of methods */
    int *meth_inst;         /* method instances contributing */
    int meth_inst_space[MAXMETH];  /* in case of only a few instances */
    struct expnode  expr;   /* for compound formula */
    int next_compound;      /* -1 for end of list */
    long timestamp;         /* when quantity last calculated */
    };
#define Q_ENERGY 1    /* part of energy */
#define Q_FIXED  2  /* is a constraint */
#define Q_INFO    4  /* only for information */
#define Q_CONSERVED 8  /* eliminate degree of freedom, but don't evaluate */
#define Q_DOTHIS 0x0010
#define Q_RENORMALIZE 0x0020  /* adjust torus volume */
#define Q_COMPOUND 0x0100    /* quantity defined by compound formula */
#define STANDARD_QUANTITY 0x1000
#define DEFAULT_QUANTITY  0x2000
#define Q_PRESSURE_SET  0x8000
#define Q_REDUNDANT     0x10000  /* skip this quantity due to torus_filled */
#define TORUS_MODULO_MUNGE 0x40000
#define Q_DELETED  0x80000
#define Q_FORWARD_DEF  0x100000

/* structure for one quantity calculating method */
/* split so can use same method for many quantities, */
/* or several methods for one quantity */

struct gen_quant_method {
  char name[QNAMESIZE];  /* for easy user identification */
  int  type;             /* type of element applies to */
  int flags; 
  int spec_flags;        /* what needs to be specified in datafile */
  INIT_METHOD  init;     /* initialization */
  VALUE_METHOD value;    /* function for value on element */
  GRAD_METHOD  gradient; /* function for gradient on element */
  HESS_METHOD  hessian;  /* function for hessian on element */
  CLEANUP_METHOD cleanup; /* clean up any allocated memory, etc. */
  };
/* flags for needed element atributes */
/* (low bits used in global_meth_inst_flags for Q_ENERGY etc.) */
#define NEED_SIDE  0x10
#define NEED_NORMAL 0x20
#define NEED_WINGS  0x40
#define NEED_GAUSS  0x80
#define NEED_STAR  0x100
#define NEED_STRING_STAR  0x200
#define NEED_MARKED_WINGS 0x400
#define NEED_PART_STAR 0x800
#define NEED_FULL_STAR 0x1000
#define ALL_NEEDS 0xFFF0
/* flags bit for orientability */
#define ORIENTABLE_METHOD 0x10000
/*#define TORUS_MODULO_MUNGE 0x40000 also used in flags */

/* bits for spec_flags */
#define NOSPEC          0
#define SPEC_SCALAR    0x0001
#define SPEC_VECTOR    0x0002
#define SPEC_2FORM     0x0004
#define SPEC_EXTRADIM 0x0008
#define SPEC_USE_DENSITY 0x0010
#define SPEC_KVECTOR    0x0020

/* structure for instance of method */
#define MAXMEXPR ((MAXCOORD*MAXCOORD)/2) /* enough for 2-forms */
#define MNAMESIZE 128 
struct method_instance {
  char name[MNAMESIZE];
  int self_id;  /* for use in vgrads */
  int type; /* element type */
  int flags;
  int gen_method;  /* parent method */
  int quant;  /* quantity this is a part of */
  int connum; /* constraint or bdry this may be associated with */
  int quant_index; /* order of method in quantity list */
  int global_low_rank;   /* index in list of all instances used in low rank updates */
  int vec_order;  /* dimension of k-vector */
  int elmodulus;  /* number of extra attribute to use as element modulus */
  struct expnode *expr[MAXMEXPR];  /* whatever interpreted expressions needed */
  REAL modulus;     /* adjustable multiplier */
  REAL value;     /* total value of this instance */
  REAL oldvalue;    /* value for restore_coords() */
  REAL newvalue;     /* accumulating value of this instance */
  REAL abstotal;     /* total of abs values of elements */
  REAL value_addends[MAXADDENDS];  /* for binary tree addition */
#ifdef SHARED_MEMORY
  REAL procvalue[MAXPROCS]; /* separate multiprocessor values */
  REAL procabstotal[MAXPROCS]; /* separate multiprocessor values */
#endif
  REAL stamp;      /* for coordination with eval_all() and eval_sec() */
  REAL **grad;  /* at current vertex, for compound quants */
  REAL ****hess;  /* at current vertex, for compound quants */
  REAL parameter_1;  /* parameter particular to this instance */
  vertex_id *vlist; /* for use in compound Hessian */
  long timestamp;  /* when value last calculated */
#ifdef PROF_START
  /* for profiling execution time */
  int    value_call_count;
  int    grad_call_count;
  int    hess_call_count;
  double value_elapsed_time;  /* in seconds */
  double grad_elapsed_time;
  double hess_elapsed_time;
#endif
  
/*  struct method_instance *next; */  /* linked list */
  };
/* flags, (avoid quantity flag bits) */
#define IMPLICIT_INSTANCE 0x200000
#define DEFAULT_INSTANCE 0x400000
#define METH_PARAMETER_1 0x800000
#define BODY_INSTANCE    0x1000000
#define IGNORE_CONSTR    0x2000000
#define FAKE_IMPLICIT    0x4000000
#define IGNORE_FIXED    0x8000000
#define ELEMENT_MODULUS_FLAG 0x10000000
#define USE_DENSITY     0x20000000
#define GLOBAL_INST     0x40000000
/* start for self_id, to distinguish from bodies and quantities */
#define METHBASE 0x10000

#define METH_INST ((struct method_instance *)(dymem + dy_meth_inst))
#define METH_INSTANCE(n)  (meth_inst_list[abs(n)])
#define dy_meth_inst web.dy_meth_inst_w
extern struct method_instance *Meth_inst;
#define meth_inst_alloc web.meth_inst_alloc_w  /* number allocated */
#define meth_inst_count web.meth_inst_count_w    /* number defined    */
#define LOW_INST 1

#define dy_gen_quants web.dy_gen_quants_w
#define GEN_QUANT(n)  (gen_quant_list[n])
extern struct gen_quant *Gen_quants;
#define gen_quant_count web.gen_quant_count_w /* used */
#define gen_quant_alloc web.gen_quant_alloc_w /* allocated */
extern int compound_quant_list_head; /* -1 for end of list */

/* new system of general quantity allocation */
extern struct gen_quant *gen_quant_free;
extern struct gen_quant **gen_quant_list;
extern int gen_quant_free_left;

/* new system of general instance allocation */
extern struct method_instance *meth_inst_free;
extern struct method_instance **meth_inst_list;
extern int meth_inst_free_left;

/* for list of available methods */
extern struct gen_quant_method basic_gen_methods[];

/* global method instances, applying to every element of type */
#define MAXGLOBINST 100
#define global_meth_inst_flags web.global_meth_inst_flags_w
#define global_meth_inst web.global_meth_inst_w /* lists */
#define global_meth_inst_count web.global_meth_inst_count_w

/* flags telling which quantity calculations necessary */
/* flag set for Q_ENERGY,Q_FIXED, Q_CONSERVED, or Q_INFO if any element
    needs a quantity calculated */
#define quant_flags web.quant_flags_w

void q_edge_setup ARGS((struct linsys *,QINFO,int));
void q_facet_setup ARGS((struct linsys *,QINFO,int));
void q_vertex_setup ARGS((struct linsys *,QINFO,int));
void q_body_setup ARGS((struct linsys *,QINFO,int));
void q_facetedge_setup ARGS((struct linsys *,QINFO,int));

extern void (*q_setup[NUMELEMENTS]) ARGS((struct linsys *,QINFO,int));

void q_edge_setup_q ARGS((QINFO,int));
void q_facet_setup_q ARGS((QINFO,int));
void q_vertex_setup_q ARGS((QINFO,int));
void q_body_setup_q ARGS((QINFO,int));

void q_edge_setup_lagrange ARGS((QINFO,int));
void q_facet_setup_lagrange ARGS((QINFO,int));


/********************************************************************
*   Declarations of available methods.
*/
extern REAL null_q_value(QINFO);
extern REAL null_q_grad(QINFO);
extern REAL null_q_hess(QINFO);

extern void q_edge_tension_init ARGS((QINT,struct method_instance*));
extern REAL q_edge_tension_value(QINFO);
extern REAL q_edge_tension_gradient(QINFO);
extern REAL q_edge_tension_hessian(QINFO);

extern REAL edge_length_q_value(QINFO);
extern REAL edge_length_q_grad(QINFO);
extern REAL edge_length_q_hess(QINFO);

extern REAL lagrange_edge_tension_value(QINFO);
extern REAL lagrange_edge_tension_grad(QINFO);
extern REAL lagrange_edge_tension_hess(QINFO);

extern void circular_arc_length_init ARGS((QINT,struct method_instance*));
extern REAL circular_arc_length_value(QINFO);
extern REAL circular_arc_length_grad(QINFO);
extern REAL circular_arc_length_hess(QINFO);

extern void circular_arc_area_init ARGS((QINT,struct method_instance*));
extern REAL circular_arc_area_value(QINFO);
extern REAL circular_arc_area_grad(QINFO);
extern REAL circular_arc_area_hess(QINFO);

extern void spherical_arc_length_init ARGS((QINT,struct method_instance*));
extern REAL spherical_arc_length_value(QINFO);
extern REAL spherical_arc_length_grad(QINFO);
extern REAL spherical_arc_length_hess(QINFO);

extern void spherical_arc_area_init ARGS((QINT,struct method_instance*));
extern REAL spherical_arc_area_n_value(QINFO);
extern REAL spherical_arc_area_n_grad(QINFO);
extern REAL spherical_arc_area_n_hess(QINFO);
extern REAL spherical_arc_area_s_value(QINFO);
extern REAL spherical_arc_area_s_grad(QINFO);
extern REAL spherical_arc_area_s_hess(QINFO);

extern REAL null_length_value(QINFO);
extern REAL null_length_grad(QINFO);
extern REAL null_length_hess(QINFO);

extern REAL null_area_value(QINFO);
extern REAL null_area_grad(QINFO);
extern REAL null_area_hess(QINFO);


extern REAL q_edge_area(QINFO);
extern REAL q_edge_area_grad(QINFO);
extern REAL q_edge_area_hess(QINFO);

extern REAL q_edge_area_q(QINFO);
extern REAL q_edge_area_q_grad(QINFO);
extern REAL q_edge_area_q_hess(QINFO);

extern REAL q_edge_area_lagrange(QINFO);
extern REAL q_edge_area_lagrange_grad(QINFO);
extern REAL q_edge_area_lagrange_hess(QINFO);

extern REAL q_edge_torus_area(QINFO);
extern REAL q_edge_torus_area_grad(QINFO);
extern REAL q_edge_torus_area_hess(QINFO);

extern REAL q_edge_torus_area_q(QINFO);
extern REAL q_edge_torus_area_q_grad(QINFO);
extern REAL q_edge_torus_area_q_hess(QINFO);

extern REAL q_edge_torus_area_lagrange(QINFO);
extern REAL q_edge_torus_area_lagrange_grad(QINFO);
extern REAL q_edge_torus_area_lagrange_hess(QINFO);

extern REAL gap_energy(QINFO);
extern REAL gap_grads(QINFO);

extern REAL dihedral_hooke_energy(QINFO);
extern REAL dihedral_hooke_grad(QINFO);
extern REAL dihedral_hooke_hess(QINFO);

extern void wulff_method_init ARGS((QINT,struct method_instance*));
extern REAL facet_wulff_value(QINFO);
extern REAL facet_wulff_grad(QINFO);

extern REAL klein_length_method(QINFO);
extern REAL klein_length_method_grad(QINFO);

extern REAL klein_area_method(QINFO);
extern REAL klein_area_method_grad(QINFO);

extern void q_facet_tension_init ARGS((QINT,struct method_instance*));
extern REAL q_facet_tension_value(QINFO);
extern REAL q_facet_tension_gradient(QINFO);
extern REAL q_facet_tension_hessian(QINFO);

extern void q_facet_tension_u_init ARGS((QINT,struct method_instance*));
extern REAL q_facet_tension_u_value(QINFO);
extern REAL q_facet_tension_u_gradient(QINFO);
extern REAL q_facet_tension_u_hessian(QINFO);

extern REAL q_facet_tension_q(QINFO);
extern REAL q_facet_tension_q_grad(QINFO);
extern REAL q_facet_tension_q_hess(QINFO);

extern REAL q_facet_tension_uq(QINFO);
extern REAL q_facet_tension_uq_grad(QINFO);
extern REAL q_facet_tension_uq_hess(QINFO);

extern REAL lagrange_facet_tension_value(QINFO);
extern REAL lagrange_facet_tension_grad(QINFO);
extern REAL lagrange_facet_tension_hess(QINFO);

extern void metric_area_init ARGS((QINT,struct method_instance*));
extern REAL metric_area_value(QINFO);
extern REAL metric_area_grad(QINFO);
extern REAL metric_area_hess(QINFO);

extern REAL area_square_value(QINFO);
extern REAL area_square_gradient(QINFO);

extern void q_facet_volume_init ARGS((QINT,struct method_instance*));
extern REAL q_facet_volume(QINFO);
extern REAL q_facet_volume_grad(QINFO);
extern REAL q_facet_volume_hess(QINFO);

extern REAL q_facet_volume_q(QINFO);
extern REAL q_facet_volume_q_grad(QINFO);
extern REAL q_facet_volume_q_hess(QINFO);

extern REAL q_facet_torus_volume(QINFO);
extern REAL q_facet_torus_volume_grad(QINFO);
extern REAL q_facet_torus_volume_hess(QINFO);

extern REAL q_facet_torus_volume_q(QINFO);
extern REAL q_facet_torus_volume_q_grad(QINFO);
extern REAL q_facet_torus_volume_q_hess(QINFO);

extern REAL lagrange_facet_volume(QINFO);
extern REAL lagrange_facet_volume_grad(QINFO);
extern REAL lagrange_facet_volume_hess(QINFO);

extern REAL q_facet_torus_volume_lagr(QINFO);
extern REAL q_facet_torus_volume_lagr_grad(QINFO);
extern REAL q_facet_torus_volume_lagr_hess(QINFO);

extern void pos_area_hess_init ARGS((QINT,struct method_instance*));
extern REAL pos_area_hess(QINFO);

extern void sobolev_area_init ARGS((QINT,struct method_instance*));
extern REAL sobolev_area_hess(QINFO);

extern void dirichlet_area_init ARGS((QINT,struct method_instance*));
extern REAL dirichlet_area_hess(QINFO);

extern void gauss_integral_init ARGS((QINT,struct method_instance*));
extern REAL gauss_int_gradient(QINFO);
extern REAL gauss_int_energy(QINFO);

extern void sqgauss_method_init ARGS((QINT,struct method_instance*));
extern REAL sqgauss_method_value(QINFO);
extern REAL sqgauss_method_grad(QINFO);

extern void levine_energy_init ARGS((QINT,struct method_instance*));
extern REAL levine_energy_value(QINFO);
extern REAL levine_energy_grad(QINFO);

extern void star_sqgauss_method_init ARGS((QINT,struct method_instance*));
extern REAL star_sqgauss_method_value(QINFO);
extern REAL star_sqgauss_method_grad(QINFO);
extern REAL star_sqgauss_method_hess(QINFO);

extern void star_gauss_method_init ARGS((QINT,struct method_instance*));
extern REAL star_gauss_method_value(QINFO);
extern REAL star_gauss_method_grad(QINFO);
extern REAL star_gauss_method_hess(QINFO);

extern void sqcurve_string_init ARGS((QINT,struct method_instance*));
extern REAL sqcurve_string_value(QINFO);
extern REAL sqcurve_string_grad(QINFO);
extern REAL sqcurve_string_hess(QINFO);
extern void sqcurve_string_marked_init ARGS((QINT,struct method_instance*));
extern REAL curve_power;
extern int curve_power_param;
#define CURVE_POWER_NAME "curvature_power"

extern void sqcurve2_string_init ARGS((QINT,struct method_instance*));
extern REAL sqcurve2_string_value(QINFO);
extern REAL sqcurve2_string_grad(QINFO);
extern REAL sqcurve2_string_hess(QINFO);

extern void sqcurve3_string_init ARGS((QINT,struct method_instance*));
extern REAL sqcurve3_string_value(QINFO);
extern REAL sqcurve3_string_grad(QINFO);
extern REAL sqcurve3_string_hess(QINFO);

extern void sq_mean_curv_cyl_init ARGS((QINT,struct method_instance*));
extern REAL sq_mean_curv_cyl_value(QINFO);
extern REAL sq_mean_curv_cyl_grad(QINFO);
extern REAL sq_mean_curv_cyl_hess(QINFO);

extern void sq_gauss_curv_cyl_init ARGS((QINT,struct method_instance*));
extern REAL sq_gauss_curv_cyl_value(QINFO);
extern REAL sq_gauss_curv_cyl_grad(QINFO);
extern REAL sq_gauss_curv_cyl_hess(QINFO);

extern void circle_willmore_init ARGS((QINT,struct method_instance*));
extern REAL circle_willmore_value(QINFO);
extern REAL circle_willmore_grad(QINFO);
extern REAL circle_willmore_hess(QINFO);

extern void mean_int_init ARGS((QINT,struct method_instance*));
extern REAL mean_int_value(QINFO);
extern REAL mean_int_gradient(QINFO);
extern REAL mean_int_hessian(QINFO);

extern void mean_int_a_init ARGS((QINT,struct method_instance*));
extern REAL mean_int_a_value(QINFO);
extern REAL mean_int_a_gradient(QINFO);
extern REAL mean_int_a_hessian(QINFO);

extern REAL vertex_scalar_integral(QINFO);
extern REAL vertex_scalar_integral_grad(QINFO);
extern REAL vertex_scalar_integral_hess(QINFO);

extern REAL edge_scalar_integral(QINFO);
extern REAL edge_scalar_integral_grad(QINFO);
extern REAL edge_scalar_integral_hess(QINFO);

extern REAL edge_scalar_integral_q(QINFO);
extern REAL edge_scalar_integral_q_grad(QINFO);
extern REAL edge_scalar_integral_q_hess(QINFO);

extern REAL edge_scalar_integral_lagr(QINFO);
extern REAL edge_scalar_integral_lagr_grad(QINFO);
extern REAL edge_scalar_integral_lagr_hess(QINFO);

extern REAL edge_vector_integral(QINFO);
extern REAL edge_vector_integral_grad(QINFO);
extern REAL edge_vector_integral_hess(QINFO);

extern REAL edge_vector_integral_q(QINFO);
extern REAL edge_vector_integral_q_grad(QINFO);
extern REAL edge_vector_integral_q_hess(QINFO);

extern REAL edge_vector_integral_lagrange(QINFO);
extern REAL edge_vector_integral_lagrange_grad(QINFO);
extern REAL edge_vector_integral_lagrange_hess(QINFO);

extern void edge_general_init ARGS((QINT,struct method_instance*));
extern REAL edge_general_value(QINFO);
extern REAL edge_general_grad(QINFO);
extern REAL edge_general_hess(QINFO);
extern REAL edge_general_value_lagrange(QINFO);
extern REAL edge_general_grad_lagrange(QINFO);
extern REAL edge_general_hess_lagrange(QINFO);

extern void facet_scalar_integral_init ARGS((QINT,struct method_instance*));
extern REAL facet_scalar_integral(QINFO);
extern REAL facet_scalar_integral_grad(QINFO);
extern REAL facet_scalar_integral_hess(QINFO);

extern REAL facet_scalar_integral_q(QINFO);
extern REAL facet_scalar_integral_q_grad(QINFO);
extern REAL facet_scalar_integral_q_hess(QINFO);

extern REAL facet_scalar_integral_lagr(QINFO);
extern REAL facet_scalar_integral_lagr_grad(QINFO);
extern REAL facet_scalar_integral_lagr_hess(QINFO);

extern void facet_vector_integral_init ARGS((QINT,struct method_instance*));
extern REAL facet_vector_integral(QINFO);
extern REAL facet_vector_integral_grad(QINFO);
extern REAL facet_vector_integral_hess(QINFO);

extern REAL lagrange_vector_integral(QINFO);
extern REAL lagrange_vector_integral_grad(QINFO);
extern REAL lagrange_vector_integral_hess(QINFO);

extern void simplex_vector_integral_init ARGS((QINT,struct method_instance*));
extern REAL simplex_vector_integral(QINFO);
extern REAL simplex_vector_integral_grad(QINFO);
extern REAL simplex_vector_integral_hess(QINFO);

extern void simplex_k_vector_integral_init ARGS((QINT,struct method_instance*));
extern REAL simplex_k_vector_integral(QINFO);
extern REAL simplex_k_vector_integral_grad(QINFO);
extern REAL simplex_k_vector_integral_hess(QINFO);

extern REAL lagrange_k_vector_integral(QINFO);
extern REAL lagrange_k_vector_integral_grad(QINFO);
extern REAL lagrange_k_vector_integral_hess(QINFO);

extern REAL facet_vector_integral_q(QINFO);
extern REAL facet_vector_integral_q_grad(QINFO);
extern REAL facet_vector_integral_q_hess(QINFO);

extern void facet_2form_integral_init ARGS((QINT,struct method_instance*));
extern REAL facet_2form_integral(QINFO);
extern REAL facet_2form_integral_grad(QINFO);
extern REAL facet_2form_integral_hess(QINFO);

extern REAL facet_2form_integral_lagrange(QINFO);
extern REAL facet_2form_integral_lagrange_grad(QINFO);
extern REAL facet_2form_integral_lagrange_hess(QINFO);

extern void facet_2form_sq_integral_init ARGS((QINT,struct method_instance*));
extern REAL facet_2form_sq_integral(QINFO);
extern REAL facet_2form_sq_integral_grad(QINFO);

extern void facet_general_init ARGS((QINT,struct method_instance*));
extern REAL facet_general_value(QINFO);
extern REAL facet_general_grad(QINFO);
extern REAL facet_general_hess(QINFO);

extern REAL facet_general_value_lagr(QINFO);
extern REAL facet_general_grad_lagr(QINFO);
extern REAL facet_general_hess_lagr(QINFO);

extern void stress_integral_init ARGS((QINT,struct method_instance*));
extern REAL stress_integral(QINFO);
extern REAL stress_integral_grad(QINFO);

extern void sqcurve_method_init ARGS((QINT,struct method_instance*));
extern REAL sqcurve_method_value(QINFO);
extern REAL sqcurve_method_grad(QINFO);
extern void sqcurve_method_cleanup ARGS((void));

extern void star_sqcurve_method_init ARGS((QINT,struct method_instance*));
extern REAL star_sqcurve_method_value(QINFO);
extern REAL star_sqcurve_method_grad(QINFO);
extern REAL star_sqcurve_method_hess(QINFO);

extern void laplacian_mean_curvature_init ARGS((QINT,struct method_instance*));
extern REAL laplacian_mean_curvature_value(QINFO);
extern REAL laplacian_mean_curvature_grad(QINFO);

extern void stokes2d_init ARGS((QINT,struct method_instance*));
extern REAL stokes2d_value(QINFO);
extern REAL stokes2d_grad(QINFO);
extern REAL stokes2d_hess(QINFO);
extern REAL stokes2d_laplacian(QINFO);

extern void hooke_energy_init ARGS((QINT,struct method_instance*));
extern REAL hooke_energy(QINFO);
extern REAL hooke_energy_gradient(QINFO);
extern REAL hooke_energy_hessian(QINFO);

extern void hooke2_energy_init ARGS((QINT,struct method_instance*));
extern REAL hooke2_energy(QINFO);
extern REAL hooke2_energy_gradient(QINFO);
extern REAL hooke2_energy_hessian(QINFO);

extern void hooke3_energy_init ARGS((QINT,struct method_instance*));
extern REAL hooke3_energy(QINFO);
extern REAL hooke3_energy_gradient(QINFO);
extern REAL hooke3_energy_hessian(QINFO);

extern void local_hooke_init ARGS((QINT,struct method_instance*));
extern REAL local_hooke(QINFO);
extern REAL local_hooke_gradient(QINFO);

extern void linear_elastic_init ARGS((QINT,struct method_instance*));
extern REAL linear_elastic_energy(QINFO);
extern REAL linear_elastic_gradient(QINFO);
extern REAL linear_elastic_hessian(QINFO);

extern void general_linear_elastic_init ARGS((QINT,struct method_instance*));
extern REAL general_linear_elastic_energy(QINFO);
extern REAL general_linear_elastic_gradient(QINFO);
extern REAL general_linear_elastic_hessian(QINFO);

extern void linear_elastic_B_init ARGS((QINT,struct method_instance*));
extern REAL linear_elastic_B_energy(QINFO);
extern REAL linear_elastic_B_gradient(QINFO);
extern REAL linear_elastic_B_hessian(QINFO);

extern void relaxed_elastic_init ARGS((QINT,struct method_instance*));
extern REAL relaxed_elastic_energy(QINFO);
extern REAL relaxed_elastic_gradient(QINFO);
extern REAL relaxed_elastic_hessian(QINFO);

extern REAL relaxed_elastic1_energy(QINFO);
extern REAL relaxed_elastic1_gradient(QINFO);
extern REAL relaxed_elastic1_hessian(QINFO);
extern REAL relaxed_elastic2_energy(QINFO);
extern REAL relaxed_elastic2_gradient(QINFO);
extern REAL relaxed_elastic2_hessian(QINFO);

extern void relaxed_elastic_A_init ARGS((QINT,struct method_instance*));
extern REAL relaxed_elastic_A_energy(QINFO);
extern REAL relaxed_elastic_A_gradient(QINFO);
extern REAL relaxed_elastic_A_hessian(QINFO);

extern REAL relaxed_elastic1_A_energy(QINFO);
extern REAL relaxed_elastic1_A_gradient(QINFO);
extern REAL relaxed_elastic1_A_hessian(QINFO);
extern REAL relaxed_elastic2_A_energy(QINFO);
extern REAL relaxed_elastic2_A_gradient(QINFO);
extern REAL relaxed_elastic2_A_hessian(QINFO);

extern void dirichlet_elastic_init ARGS((QINT,struct method_instance*));
extern REAL dirichlet_elastic_energy(QINFO);
extern REAL dirichlet_elastic_gradient(QINFO);
extern REAL dirichlet_elastic_hessian(QINFO);


extern void SVK_init ARGS((QINT,struct method_instance*));
extern REAL SVK_energy(QINFO);
extern REAL SVK_gradient(QINFO);
extern REAL SVK_hessian(QINFO);

extern void Neo_Hookean_init ARGS((QINT,struct method_instance*));
extern REAL Neo_Hookean_energy(QINFO);
extern REAL Neo_Hookean_gradient(QINFO);
extern REAL Neo_Hookean_hessian(QINFO);

extern void knot_energy_init ARGS((QINT,struct method_instance*));

extern void knot_power_init ARGS((QINT,struct method_instance*));

extern REAL knot_energy(QINFO);
extern REAL knot_energy_gradient(QINFO);
extern REAL knot_energy_hessian(QINFO);

extern REAL knot_thickness(QINFO);

extern REAL knot_thickness2(QINFO);

extern REAL knot_thickness_0(QINFO);
extern REAL knot_thickness_0_gradient(QINFO);

extern REAL knot_thickness_p(QINFO);
extern REAL knot_thickness_p_gradient(QINFO);

extern REAL knot_thickness_p2(QINFO);
extern REAL knot_thickness_p2_gradient(QINFO);

extern REAL knot_local_thickness(QINFO);

extern void charge_gradient_init ARGS((QINT,struct method_instance*));
extern REAL charge_gradient(QINFO);
extern REAL charge_gradient_gradient(QINFO);

extern void uniform_knot_energy_init ARGS((QINT,struct method_instance*));
extern REAL uniform_knot_energy(QINFO);
extern REAL uniform_knot_energy_gradient(QINFO);

extern REAL edge_edge_knot_energy(QINFO);
extern REAL edge_edge_knot_energy_gradient(QINFO);

extern REAL edge_min_knot_energy(QINFO);

extern REAL uniform_normalization(QINFO);
extern REAL uniform_binormalization(QINFO);

extern REAL edge_normalization(QINFO);

extern REAL simon_normalization(QINFO);

extern void facet_knot_energy_init ARGS((QINT,struct method_instance*));
extern REAL facet_knot_energy(QINFO);
extern REAL facet_knot_energy_gradient(QINFO);

extern void bi_surface_init ARGS((QINT,struct method_instance*));
extern REAL bi_surface_energy(QINFO);
extern REAL bi_surface_gradient(QINFO);

extern void facet_knot_energy_fix_init ARGS((QINT,struct method_instance*));
extern REAL facet_knot_energy_fix(QINFO);
extern REAL facet_knot_energy_fix_gradient(QINFO);

extern REAL buck_knot_energy(QINFO);
extern REAL buck_knot_energy_gradient(QINFO);

extern REAL proj_knot_energy(QINFO);
extern REAL proj_knot_energy_gradient(QINFO);

extern REAL sin_knot_energy(QINFO);
extern REAL sin_knot_energy_gradient(QINFO);

extern REAL circle_knot_energy(QINFO);
extern REAL circle_knot_energy_gradient(QINFO);

extern REAL average_crossing(QINFO);

extern REAL writhe(QINFO);
extern REAL writhe_gradient(QINFO);

extern REAL twist(QINFO);

extern void sphere_knot_energy_init ARGS((QINT,struct method_instance*));
extern REAL sphere_knot_energy(QINFO);
extern REAL sphere_knot_energy_gradient(QINFO);

extern REAL johndust_energy(QINFO);
extern REAL johndust_gradient(QINFO);

extern void curvature_forces_init ARGS((QINT,struct method_instance*));
extern REAL curvature_forces_energy(QINFO);
extern REAL curvature_forces(QINFO);

/* extern INIT_METHOD ackerman_init; */
extern void ackerman_init ARGS((QINT,MIPTR)); 
extern REAL ackerman_energy(QINFO);
extern REAL ackerman_forces(QINFO);

extern void carter_energy_init ARGS((QINT,struct method_instance*));
extern REAL carter_energy(QINFO);
extern REAL carter_energy_gradient(QINFO);

extern void full_gravity_init ARGS((QINT,struct method_instance*));

extern void gravity_init ARGS((QINT,struct method_instance*));
extern REAL gravity_energy(QINFO);
extern REAL gravity_grads(QINFO);
extern REAL gravity_hessian(QINFO);

extern void string_gravity_init ARGS((QINT,struct method_instance*));
extern REAL string_gravity_energy(QINFO);
extern REAL string_gravity_grads(QINFO);
extern REAL string_gravity_hessian(QINFO);

extern void curvature_binormal_init ARGS((QINT,struct method_instance*));
extern REAL curvature_binormal_energy(QINFO);
extern REAL curvature_binormal_force(QINFO);

extern void ddd_gamma_sq_init ARGS((QINT,struct method_instance*));
extern REAL ddd_gamma_sq_energy(QINFO);
extern REAL ddd_gamma_sq_gradient(QINFO);

extern REAL true_average_crossing(QINFO);
extern REAL true_writhe(QINFO);

extern REAL spherical_area_value(QINFO);
extern REAL spherical_area_grad(QINFO);
