/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/


/**********************************************************************
*
* The ultimate structure for a whole surface , including all global
* variables needed to export for distributed computing.
*/

#ifdef  __cplusplus
extern "C" {
#endif

/* structure type name different from structure variable name since
   Visual C is incompetent at distinguishing in debugger. */

struct webstruct {
     struct skeleton skel[NUMELEMENTS];
     int sizes[NUMELEMENTS];  /* allocated space for element structure */
     int usedsizes[NUMELEMENTS];  /* used space for element structure */
     struct element **elhashtable; /* id hash list of element pointers */
     int elhashcount;  /* actual number of live entries */
     int elhashmask;  /* for picking off index bits of id hash */
     int elhashsize;  /* size of hash table; power of 2 */
     int sdim;  /* dimension of ambient space */
     int dimension;    /* where tension resides */
     int representation; /* STRING, SOAPFILM, or SIMPLEX */
     int modeltype;    /* QUADRATIC, LINEAR, or LAGRANGE; see defines below */
     int lagrange_order; /* polynomial order of elements */
     int headvnum;  /* number of head vertex in edge list */
     int maxparam;    /* maximum number of parameters in any boundary */
     int maxcon;      /* number of constraint structures allocated */
     int highcon;     /* highest constraint number used */
     struct constraint  *constraints; /* constraint definitions */
     conmap_t con_global_map[MAXCONPER]; /* global vertex constraints */
     int con_global_count;  /* number of global vertex constraints */
     REAL tolerance;      /* constraint error tolerance */
     REAL target_tolerance; /* error tolerance for extensive constraints */
     int bdrymax;        /* number of boundary structures allocated */
     int highbdry;       /* highest boundary number used */
     struct boundary *boundaries; /* for free boundaries */
     int diffusion_flag;  /* whether diffusion in effect */
     REAL diffusion_const;  /* coefficient for diffusion */
     REAL simplex_factorial; /* content correction factor for determinant */
     int torus_clip_flag;
     int torus_body_flag;
     int symmetric_content; /* 1 if volumes use symmetric divergence */
     int h_inverse_metric_flag; /* for laplacian of curvature */
     REAL meritfactor;    /* for multiplying figure of merit */
     int gravflag;         /* whether gravity is on */
     REAL grav_const;      /* multiplier for gravitational force */
     int convex_flag;     /* whether any convex boundaries present */
     int pressflag;        /* whether prescribed pressures present */
     int constr_flag;     /* set if there are any one-sided constraints */
     int hide_flag;        /* set for hidden surface removal */
     int motion_flag;     /* set for fixed scale of motion;
                                          otherwise seek minimum. */
     int symmetry_flag;  /* whether symmetry group in effect */
     int torus_flag;     /* whether working in toroidal domain */
     int full_flag;     /* whether torus solidly packed with bodies */
     int pressure_flag;  /* whether pressure used dynamically */
     int projection_flag; /* whether to project */
     int area_norm_flag; /* whether to normalize force by area surrounding vertex */
     int norm_check_flag;  /* whether area normalization checks normal deviation */
     REAL norm_check_max;  /* maximum allowable deviation */
     int vol_flag;         /* whether body volumes up to date */
     int jiggle_flag;     /* whether to jiggle vertices at each move */
     int homothety;        /* flag for homothety adjustment each iteration */
     int wulff_flag;      /* whether we are using wulff shapes for energy */
     int wulff_count;     /* number of Wulff vectors read in */
     char wulff_name[60]; /* Wulff file or keyword */
     vertex_id  zoom_v;    /* vertex to zoom on */
     REAL zoom_radius;     /* current zoom radius */
     REAL total_area;
     REAL total_area_addends[MAXADDENDS]; /* for binary tree addition */
     REAL total_energy;
     REAL total_energy_addends[MAXADDENDS]; /* for binary tree addition */
     REAL spring_energy;
     int total_facets;
     int bodycount;  /* number of bodies */
     body_id outside_body;  /* a body surrounding all others */
     REAL scale;     /* force to motion scale factor */
     REAL scale_scale;     /* over-relaxation factor */
     REAL maxscale;     /* upper limit on scale factor */
     REAL pressure;    /* ambient pressure */
     REAL min_area;        /* criterion on weeding out small triangles */
     REAL min_length;     /* criterion on weeding out small triangles */
     REAL max_len;         /* criterion for dividing long edges */
     REAL max_angle;      /* max allowed deviation from parallelism */
     REAL temperature;  /* "temperature" for jiggling */
     REAL spring_constant;  /* for forcing edges to conform to boundary */
     int  gauss1D_order;        /* order for gaussian 1D integration */
     int  gauss2D_order;        /* order for gaussian 2D integration */
     REAL torusv;                 /* unit cell volume or area */
     REAL **torus_period;
     REAL **inverse_periods;/* inverse matrix of torus periods */
     REAL **torus_display_period;
     REAL display_origin[MAXCOORD];
     REAL **inverse_display_periods;/* inverse of torus display periods */
     int  metric_flag;      /* set if background metric in force */
     int  conformal_flag;  /* set for conformal metrics */
     struct expnode metric[MAXCOORD][MAXCOORD]; /* metric component functions */
      
     /* Some counters.  New scheme: Using word of flag bits for having-been-
        reported status and needing-reported status, so exec() doesn't have
        to zero these for each call to exec. */
     /* Counts that are the result of mass action only are reported 
        immediately */
     int equi_count; 
     int edge_delete_count; 
     int facet_delete_count; 
     int edge_refine_count; 
     int facet_refine_count; 
     int vertex_dissolve_count; 
     int edge_dissolve_count; 
     int facet_dissolve_count; 
     int body_dissolve_count; 
     int edge_reverse_count;
     int facet_reverse_count;
     int vertex_pop_count; 
     int edge_pop_count; 
     int pop_tri_to_edge_count;
     int pop_edge_to_tri_count;
     int pop_quad_to_quad_count;
     int where_count; 
     int edgeswap_count;
     int t1_edgeswap_count;
     int fix_count;
     int unfix_count;
     int notch_count;
 
     /* flag words and bits */
     int counts_reported;
     int counts_changed;
     
     #define equi_count_bit 0x00000001
     #define weed_count_bit 0x00000002
     #define edge_delete_count_bit 0x00000004
     #define facet_delete_count_bit 0x00000008
     #define edge_refine_count_bit 0x00000010
     #define facet_refine_count_bit 0x00000020
     #define notch_count_bit 0x00000040
     #define vertex_dissolve_count_bit 0x00000080
     #define edge_dissolve_count_bit 0x00000100
     #define facet_dissolve_count_bit 0x00000200
     #define body_dissolve_count_bit 0x00000400
     #define vertex_pop_count_bit 0x00000800
     #define edge_pop_count_bit 0x00001000
     #define pop_tri_to_edge_count_bit 0x00004000
     #define pop_edge_to_tri_count_bit 0x00008000
     #define pop_quad_to_quad_count_bit 0x00010000
     #define where_count_bit 0x00020000 
     #define edgeswap_count_bit 0x00040000 
     #define fix_count_bit 0x00080000 
     #define unfix_count_bit 0x00100000 
     #define t1_edgeswap_count_bit 0x00200000 
     #define edge_reverse_count_bit 0x00400000
     #define facet_reverse_count_bit 0x00800000
     

     /* here follows stuff moved from independent globals to inside web
         so as to be easily exported.  Previous global names are defined
         to web fields elsewhere.
         */
     DY_OFFSET dy_gen_quants_w;
     int gen_quant_count_w;
     int gen_quant_alloc_w;
     int global_count;
     int maxglobals;  /* number allocated */
     int perm_global_count;
     int max_perm_globals;  /* number allocated */

     DY_OFFSET dy_meth_inst_w; /* for storing instance structures */
     int meth_inst_alloc_w;  /* number allocated */
     int meth_inst_count_w;  /* number defined    */

     /* global method instances, applying to every element of type */
     int global_meth_inst_w[NUMELEMENTS][MAXGLOBINST]; /* lists */
     int global_meth_inst_count_w[NUMELEMENTS];

     /* flags telling which quantity calculations necessary */
     /* flag set for Q_ENERGY,Q_FIXED, or Q_INFO if any element
         needs a quantity calculated */
     int quant_flags_w[NUMELEMENTS];

     DY_OFFSET dy_freestart_w;  /* initial block of freelist, 0 if none */
#define dy_freestart web.dy_freestart_w
     DY_OFFSET dy_globals_w;  /* global variable table */
     struct global *dy_perm_globals_w;
     DY_OFFSET dy_globalshash_w; /* hash list for global variables */

     /* common */
     int meth_attr[NUMELEMENTS] ; /* method instances list */
     int mpi_export_attr[NUMELEMENTS] ; /* method instances list */
  };

extern struct webstruct web;


#ifdef  __cplusplus
}
#endif
