/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/**********************************************************************
*
*  File: skeleton.h
*
*  Header file for evolver.  Defines skeleton structures.
*
*/

/* depth of binary tree addition */
#define MAXADDENDS 30


/**********************************************************************
*
* structure for expression; goes here instead of express.h since needed
*/
 
#define EXPNAMESIZE 64
struct expnode {
                 struct treenode *start;  /* start of node list */
                 struct treenode *root;   /* root node, end of list */
                 struct locallist_t *locals; /* local variable symbol table */
                 int flag;    /* USERCOPY if user must free; HAS_STRING */
                 int stack_max;     /* maximum stack usage (excluding locals)*/
                 char name[EXPNAMESIZE];  /* for error messages */
                 REAL elapsed_time;
               };

/* flags, also used for treenode? */
#define USERCOPY 1
#define NOUSERCOPY 0
#define USING_PARAM_FLAG 0x200 

/************************************************************************
* base structure for a skeleton of a particular dimension 
*/
struct skeleton {
     int            type;       /* type of element, see defines above */
     int            dimension;  /* dimension of element               */
     int            ctrlpts;    /* number of control points           */
     INDIRECT_TYPE  *ibase;     /* to indirect list                   */
     int            ialloc;     /* number elements allocated to ibase */
     long           maxcount;   /* elements allocated                 */
     int            alloc;      /* number actually in use             */
     element_id     free;       /* start of free list                 */
     element_id     freelast;   /* end of free list                   */
     /* following used if hash storage in effect */
     struct element *freehead;  /* start of freelist                  */
     int            free_spot;  /* for scanning for empties           */

     element_id     used;       /* start of in-use elements           */
     element_id     last;       /* end of in-use chain, for adding on */
     element_id     discard;    /* start of discard list              */
     int            discard_count;  /* how many marked for discard    */
     long           count;      /* number active                      */
     ORDTYPE        max_ord;    /* highest ordinal                    */
     int            extreme[MAXCOORD+1]; /* indices of corners in Lagrange */
     int            maxextra;   /* extra attr struct allocated        */
     DY_OFFSET      dy_extras;    /* extra attributes definitions       */
     int            extra_count;        /* number of extra attributes */
  } ;

/************************************************************************
*
*  Union skeleton element structure for common operations.
*
*/

typedef int MAP;           /* constraint, etc, bitmap type */
typedef long int ATTR;     /* attribute bitmap type */
typedef short int tagtype; /* element tag type */
typedef short ETYPE;       /* element type type */
typedef long int WRAPTYPE; /* symmetry group element */

typedef int NTYPE; /* for node indexes, types, etc. */

     /* common fields; added to each element struct type */
#ifdef MPI_EVOLVER
#define BASIC_STUFF \
     element_id    forechain;       /* for element and free list forechain */\
     element_id    backchain;       /* for element and free list backchain */\
     element_id    local_id;        /* for freelisting; uses ibase index */\
     ATTR          attr;            /* attribute bits */\
     element_id    self_id;         /* for identifying self */ 
#else
#define BASIC_STUFF \
     element_id    forechain;       /* for element and free list forechain */\
     element_id    backchain;       /* for element and free list backchain */\
     ATTR          attr;            /* attribute bits */\
     element_id    self_id;         /* for identifying self */
#endif

#define COMMON_STUFF \
     BASIC_STUFF \
     element_id      original;      /* datafile original number */ \
     unsigned short  method_count;  /* number of method instances */ 

struct element {
  COMMON_STUFF
  };

/*****************************************************************
*
*    Structures peculiar to each dimension of skeleton.
*/


/****************************************************
*
*  facetedge structure 
*/

struct facetedge
  { 
     BASIC_STUFF
     edge_id      fe_edge_id;  /* oriented edge of base pair */
     facet_id     fe_facet_id; /* oriented face of base pair */
     facetedge_id nextedge[2];  /* 0 previous, 1 next */
     facetedge_id nextfacet[2];  /* 0 previous, 1 next */
  };

 
/****************************************************
*
*  vertex structure - will be extended by other attributes.
*/

struct vertex
  { 
     COMMON_STUFF
     REAL star;     /* area of surrounding facets */
     edge_id e_id;  /* link to global structure */
                    /* may really be facet in Lagrange model */
     facet_id f_id; /* link to global structure in simplex model */
     int valence;   /* number of edges incident */
  };

/* attribute numbers for standard attributes */
/* be sure these numbers are in same order as allocation in reset_skeleton() */
#define V_COORD_ATTR       0
#define V_OLDCOORD_ATTR    1
#define V_PARAM_ATTR       2
#define V_FORCE_ATTR       3
#define V_VELOCITY_ATTR    4
#define V_CONSTR_LIST_ATTR 5
#define V_METHOD_LIST_ATTR 6
#define V_NORMAL_ATTR      7

extern int V_BOUNDARY_ATTR;
 
/****************************************************
*
*  edge structure - will be extended by other attributes.
*/
  
struct edge
  { 
     COMMON_STUFF
     facetedge_id fe_id;    /* link to global structure */
     edge_id next_vedge[2]; /* link to next edge around vertex */
     REAL density;          /* energy per unit length */
     REAL length;           /* edge length; be careful of validity */
     REAL star;             /* total area of adjacent facets */
     short color;           /* for display */
  };

/* attribute numbers for standard attributes */
/* be sure these numbers are in same order as allocation in reset_skeleton() */
#define E_DENSITY_ATTR       0
#define E_VERTICES_ATTR      1
#define E_VECTOR_ATTR        2
#define E_WRAP_ATTR          3
#define E_CONSTR_LIST_ATTR   4

extern int E_BOUNDARY_ATTR;

/******************************************************
*
*  facet structure - will be extended by other attributes.
*/

struct facet
  { 
     COMMON_STUFF
     facetedge_id fe_id;  /* link to global structure */
     REAL  density;       /* for doing real currents and varifolds */
     REAL  area;          /* for diffusion or whatever */
     short color;         /* for display */
     short backcolor;     /* for different color backside */
};
/* attribute numbers for standard attributes */
#define F_CONSTR_LIST_ATTR   0
#define F_VERTICES_ATTR      1
#define F_NORMAL_ATTR        2
#define F_BODY_LIST_ATTR     3
#define F_NEXT_VFACET_ATTR   4
#define F_NEXT_BFACET_ATTR   5

extern int F_BOUNDARY_ATTR;
extern int F_TAG_ATTR; 
extern int F_PHASE_ATTR;



/*********************************************************
*
*  body structure
*/

struct body
  { 
     COMMON_STUFF
     REAL fixvol;         /* volume constraint */
     REAL volume;         /* actual volume      */
     REAL volume_addends[MAXADDENDS]; /* for binary tree addition */
     REAL oldvolume;      /* volume for restore_coords() */
     REAL actualvolume;   /* set by actual_volume in datafile */
     REAL abstotal;       /* for judging constraint tolerance */
     REAL pressure;       /* internal pressure */
     REAL oldpressure;    /* for restore_coords() */
     REAL volconst;       /* for body volume constant componenet */
     REAL oldvolconst;    /* for restore_coords() */
     REAL density;        /* for gravitational potential energy */
     REAL cm[MAXCOORD];   /* center of mass, unit cell coords */
     facet_id f_id;       /* link to global structure */
     int  volquant;       /* number of named quantity used for volume */
     int  ambquant;       /* number of named quantity used for ambient pressure */
     int  volmeth;        /* number of pos volume method instance */
     int  voltimestamp;   /* of last volume calculation */
     int  fixnum;         /* order in fixvol computations */
  }; 
extern int B_PHASE_ATTR;

/*********************************************************************
*
*    Boundaries and constraints
*/

/* for specifying free boundaries in parametric form */
#define BDRYNAMESIZE 32
struct boundary
  {  char name[BDRYNAMESIZE]; /* name, if any */
     ATTR attr;               /* attribute flags, see below for #defines */
     int pcount;              /* number of parameters */
     int num;                 /* boundary number */
     struct expnode *coordf[MAXCOORD];  /* coordinate functions for eval() */
     struct expnode *envect[MAXCOORD];  /* functions for boundary energy */
     struct expnode *convect[MAXCOORD]; /* functions for boundary interior content */
     int compcount;           /* number of components for integrands */
     int energy_method;       /* if using named quantity version */
     int content_rank;        /* for multiple content integrals on vertex or edge */
  };


/* for specifying constraints */
#define MAXCONCOMP (MAXCOORD*(MAXCOORD-1)/2)
#define CONNAMESIZE 32
struct constraint
  {
     char name[CONNAMESIZE];  /* constraint name */
     ATTR attr;               /* attribute flags, see below for #defines */
     struct expnode *formula; /* function expression for eval() */
     int compcount;           /* number of components for integrands */
     struct expnode *envect[MAXCONCOMP]; /* functions for energy integral */
     struct expnode *convect[MAXCONCOMP]; /* functions for content integral */
     int energy_method;       /* if using named quantity version */
     int content_rank;        /* for multiple content integrals on vertex or edge */
  };

/* boundary and constraint attributes */
#define NONPOSITIVE  0x0001
#define NONNEGATIVE  0x0002
#define GLOBAL       0x0004
#define B_CONVEX     0x0008
#define QFIXED       0x0010
#define IN_USE       0x0020
#define CON_ENERGY   0x0040
#define CON_CONTENT  0x0080
#define USURPED_BY_QUANTITY 0x0100
#define NAMED_THING  0x0200
#define NONWALL      0x0400
#define PARTNER_HITTING 0x0800
#define CON_FORWARD_DEF 0x1000
#define BDRY_FORWARD_DEF 0x1000

extern element_id
  NULLVERTEX,
  NULLEDGE,
  NULLFACET,
  NULLBODY,
  NULLFACETEDGE;

/* vertex volume gradient storage */

/* structures chained from each vertex */
typedef struct volgrad 
{
  int  fixnum;              /* order in fixvol calculations */
  int  qnum;                /* quantity number, or one-sided con num */
  body_id bb_id;            /* body for real, or which quantity represents
                                  or vertex for one-sided con */
  struct volgrad *chain;    /* next body for this vertex */
  REAL *grad;      /* volume gradient of body for this vertex */
  REAL *velocity;  /* form converted to vector */
  REAL *raw_velocity; /* for one-sided constraints */
//  REAL *perp;      /* velocity component perpendicular to pointwise const  */
} volgrad;

struct vgradblock   /* block header */
{ struct volgrad *base;      /* structure space */
  REAL *values;             /* for variable dimension data */
  int max;                  /* number allocated */
  int top;                  /* number used */
  struct vgradblock *next;  /* chaining */
};

extern struct vgradblock *vgradbase; /* allocated list header block start */
extern int     vgradtop;      /* number of first free  structure */
extern long    vgradmax;      /* number allocated */
extern long    vgradlastused;     /* number actually used last time round */

/* loop macros, for use when lists are not modified in loop */

#ifdef MPI_EVOLVER
/* MFOR includes imported elements; FOR doesn't */
/* except FOR_ALL_BODIES includes all, for now */
#define FOR_ALL_ELEMENTS(type,id) \
for (id = web.skel[type].used ; valid_id(id) ; id = elptr(id)->forechain) \
  if ( !valid_element(id) || (id_task(id)!=this_task) ) continue; else

#define FOR_ALL_VERTICES(v_id) \
for (v_id = web.skel[VERTEX].used ; valid_id(v_id) ; \
v_id = vptr(v_id)->forechain) \
  if ( !valid_element(v_id) || (id_task(v_id)!=this_task) ) continue; else

#define FOR_ALL_EDGES(e_id) \
for (e_id = web.skel[EDGE].used ; valid_id(e_id) ;\
 e_id = eptr(e_id)->forechain ) \
  if ( !valid_element(e_id) || (id_task(e_id)!=this_task) )\
    continue;  else

#define FOR_ALL_FACETS(f_id) \
for (f_id = web.skel[FACET].used ; valid_id(f_id) ; \
f_id = fptr(f_id)->forechain) \
  if ( !valid_element(f_id) || (id_task(f_id)!=this_task) ) continue; else

#define FOR_ALL_BODIES(b_id) \
for (b_id = web.skel[BODY].used ; valid_id(b_id) ; \
b_id = bptr(b_id)->forechain) \
  if ( !valid_element(b_id) /* || (id_task(b_id)!=this_task)*/ ) continue; else

#define FOR_ALL_FACETEDGES(fe_id) \
for (fe_id = web.skel[FACETEDGE].used ; valid_id(fe_id) ; \
fe_id = feptr(fe_id)->forechain) \
  if ( !valid_element(fe_id) || (id_task(fe_id)!=this_task) ) continue; else

#define MFOR_ALL_ELEMENTS(type,id) \
for (id = web.skel[type].used ; valid_id(id) ; id = elptr(id)->forechain) \
  if ( !valid_element(id) ) continue; else

#define MFOR_ALL_VERTICES(v_id) \
for (v_id = web.skel[VERTEX].used ; valid_id(v_id) ; \
v_id = vptr(v_id)->forechain) \
  if ( !valid_element(v_id) ) continue; else

#define MFOR_ALL_EDGES(e_id) \
for (e_id = web.skel[EDGE].used ; valid_id(e_id) ; \
e_id = eptr(e_id)->forechain) \
  if ( !valid_element(e_id) ) continue; else

#define MFOR_ALL_FACETS(f_id) \
for (f_id = web.skel[FACET].used ; valid_id(f_id) ; \
f_id = fptr(f_id)->forechain) \
  if ( !valid_element(f_id) ) continue; else

#define MFOR_ALL_BODIES(b_id) \
for (b_id = web.skel[BODY].used ; valid_id(b_id) ; \
b_id = bptr(b_id)->forechain) \
  if ( !valid_element(b_id) ) continue; else

#define MFOR_ALL_FACETEDGES(fe_id) \
for (fe_id = web.skel[FACETEDGE].used ; valid_id(fe_id) ; \
fe_id = feptr(fe_id)->forechain) \
  if ( !valid_element(fe_id) ) continue; else
#else
/* non-MPI */
#define FOR_ALL_ELEMENTS(type,id) \
for (id = web.skel[type].used ; valid_id(id) ; id = elptr(id)->forechain) \
  if ( !(elptr(id)->attr & ALLOCATED) ) continue; else

#define FOR_ALL_VERTICES(v_id) \
for (v_id = web.skel[VERTEX].used ; valid_id(v_id) ; \
v_id = vptr(v_id)->forechain) \
  if ( !(vptr(v_id)->attr & ALLOCATED) ) continue; else

#define FOR_ALL_EDGES(e_id) \
for (e_id = web.skel[EDGE].used ; valid_id(e_id) ; \
e_id = eptr(e_id)->forechain) \
  if ( !(eptr(e_id)->attr & ALLOCATED) ) continue; else

#define FOR_ALL_FACETS(f_id) \
for (f_id = web.skel[FACET].used ; valid_id(f_id) ; \
f_id = fptr(f_id)->forechain) \
  if ( !(fptr(f_id)->attr & ALLOCATED) ) continue; else

#define FOR_ALL_BODIES(b_id) \
for (b_id = web.skel[BODY].used ; valid_id(b_id) ; \
b_id = bptr(b_id)->forechain) \
  if ( !(bptr(b_id)->attr & ALLOCATED) ) continue; else

#define FOR_ALL_FACETEDGES(fe_id) \
for (fe_id = web.skel[FACETEDGE].used ; valid_id(fe_id) ; \
fe_id = feptr(fe_id)->forechain) \
  if ( !(feptr(fe_id)->attr & ALLOCATED) ) continue; else

#define MFOR_ALL_VERTICES(v_id) FOR_ALL_VERTICES(v_id) 

#define MFOR_ALL_EDGES(e_id) FOR_ALL_EDGES(e_id) 

#define MFOR_ALL_FACETS(f_id) FOR_ALL_FACETS(f_id) 

#define MFOR_ALL_BODIES(b_id)  FOR_ALL_BODIES(b_id) 

#define MFOR_ALL_FACETEDGES(fe_id) FOR_ALL_FACETEDGES(fe_id) 

#endif

#ifdef THREADS
/* Thread version of these loops depend on initialization of
   the iteration variable in thread_launch. 
*/
#define THR_ALL(id) for ( ; valid_id(id = thread_next_element()) ; ) 
#define THREAD_FOR_ALL_ELEMENTS(type,id) \
  for ( threadflag ? 0 : (global_id = web.skel[type].used) ; \
          valid_id(id = thread_next_element()) ; )
#define THREAD_FOR_ALL_VERTICES(v_id)    \
  for ( threadflag ? 0 : (global_id = web.skel[VERTEX].used) ; \
          valid_id(v_id = thread_next_element()) ; )
#define THREAD_FOR_ALL_EDGES(e_id)      \
  for ( threadflag ? 0 : (global_id = web.skel[EDGE].used) ; \
          valid_id(e_id = thread_next_element()) ; )
#define THREAD_FOR_ALL_FACETS(f_id)      \
  for ( threadflag ? 0 : (global_id = web.skel[FACET].used) ; \
          valid_id(f_id = thread_next_element()) ; )
#define THREAD_FOR_ALL_BODIES(b_id)      \
  for ( threadflag ? 0 : (global_id = web.skel[BODY].used) ; \
          valid_id(b_id = thread_next_element()) ; )
#define THREAD_FOR_ALL_FACETEDGES(fe_id) \
  for ( threadflag ? 0 : (global_id = web.skel[FACETEDGE].used) ; \
          valid_id(fe_id = thread_next_element()) ; )
#else
#define THREAD_FOR_ALL_ELEMENTS(type,id) FOR_ALL_ELEMENTS(type,id)
#define THREAD_FOR_ALL_VERTICES(v_id) FOR_ALL_VERTICES(v_id)
#define THREAD_FOR_ALL_EDGES(e_id) FOR_ALL_EDGES(e_id)
#define THREAD_FOR_ALL_FACETS(f_id) FOR_ALL_FACETS(f_id)
#define THREAD_FOR_ALL_BODIES(b_id) FOR_ALL_BODIES(b_id)
#define THREAD_FOR_ALL_FACETEDGES(fe_id) FOR_ALL_FACETEDGES(fe_id)
#endif


/* These access functions are defined as macros, since they involve
    only one evaluation of the arguments */

#define get_edge_midv(e_id)            (get_edge_vertices(e_id)[2])

#define get_fe_side(fe_id,x)  get_edge_side(get_fe_edge(fe_id),x)



/* attribute macros */
#define EXTRAS(type)  ((struct extra *)(dymem + web.skel[type].dy_extras))
#define V_EXTRA(v,attr) ((char*)(vptr(v))+EXTRAS(VERTEX)[attr].offset)
#define E_EXTRA(e,attr) ((char*)(eptr(e))+EXTRAS(EDGE)[attr].offset)
#define F_EXTRA(f,attr) ((char*)(fptr(f))+EXTRAS(FACET)[attr].offset)

#define VREAL(v,attr) ((REAL*)((char*)(vptr(v))+EXTRAS(VERTEX)[attr].offset))
#define VREALP(vp,attr) ((REAL*)((char*)vp+EXTRAS(VERTEX)[attr].offset))
#define VINT(v,attr) ((int*)((char*)(vptr(v))+EXTRAS(VERTEX)[attr].offset))
#define VPTR(v,attr) ((char**)((char*)(vptr(v))+EXTRAS(VERTEX)[attr].offset))
#define V_ELID(v,attr) ((element_id*)((char*)(eptr(e))+EXTRAS(VERTEX)[attr].offset))

#define EREAL(e,attr) ((REAL*)((char*)(eptr(e))+EXTRAS(EDGE)[attr].offset))
#define EINT(e,attr) ((int*)((char*)(eptr(e))+EXTRAS(EDGE)[attr].offset))
#define EULONG(e,attr) ((unsigned long*)((char*)(eptr(e))+EXTRAS(EDGE)[attr].offset))
#define E_ELID(e,attr) ((element_id*)((char*)(eptr(e))+EXTRAS(EDGE)[attr].offset))

#define FREAL(f,attr) ((REAL*)((char*)(fptr(f))+EXTRAS(FACET)[attr].offset))
#define FINT(f,attr) ((int*)((char*)(fptr(f))+EXTRAS(FACET)[attr].offset))
#define FULONG(f,attr) ((unsigned long*)((char*)(fptr(f))+EXTRAS(FACET)[attr].offset))
#define F_ELID(f,attr) ((element_id*)((char*)(fptr(f))+EXTRAS(FACET)[attr].offset))

#define BREAL(b,attr) ((REAL*)((char*)(bptr(b))+EXTRAS(BODY)[attr].offset))
#define BINT(b,attr) ((int*)((char*)(bptr(b))+EXTRAS(BODY)[attr].offset))
#define BULONG(b,attr) ((unsinged long*)((char*)(bptr(b))+EXTRA(BODY)[attr].offset))
#define B_ELID(b,attr) ((element_id*)((char*)(bptr(b))+EXTRA(BODY)[attr].offset))
#define FEREAL(fe,attr) ((REAL*)((char*)(feptr(fe))+EXTRAS(FACETEDGE)[attr].offset))
#define FEINT(fe,attr) ((int*)((char*)(feptr(fe))+EXTRAS(FACETEDGE)[attr].offset))

#define get_coord(v_id)  VREAL((v_id),V_COORD_ATTR)
#define get_oldcoord(v_id)  VREAL((v_id),V_OLDCOORD_ATTR)
#define get_param(v_id)  VREAL((v_id),V_PARAM_ATTR)
#define get_boundary(v_id)        (V_BOUNDARY_ATTR ?\
                     web.boundaries+*VINT(v_id,V_BOUNDARY_ATTR) : NULL)
#define get_vertex_boundary(v_id)        (V_BOUNDARY_ATTR ?\
                     web.boundaries+*VINT(v_id,V_BOUNDARY_ATTR) : NULL)
#define get_vertex_boundary_num(v_id)        (V_BOUNDARY_ATTR ?\
                     *VINT(v_id,V_BOUNDARY_ATTR) : 0)
#define get_edge_boundary_num(e_id)        (E_BOUNDARY_ATTR ?\
                     *EINT(e_id,E_BOUNDARY_ATTR) : 0)
#define get_facet_boundary_num(f_id)        (F_BOUNDARY_ATTR ?\
                     *FINT(f_id,F_BOUNDARY_ATTR) : 0)
#define set_boundary_num(v_id,bnum) (V_BOUNDARY_ATTR ?\
                                    (*VINT(v_id,V_BOUNDARY_ATTR) = (bnum)) : 0)
#define set_vertex_boundary_num(v_id,bnum) (V_BOUNDARY_ATTR ?\
                                    (*VINT(v_id,V_BOUNDARY_ATTR) = (bnum)) : 0)
#define get_edge_boundary(e_id)    (E_BOUNDARY_ATTR ?\
                      web.boundaries+*EINT(e_id,E_BOUNDARY_ATTR) : NULL)
#define set_edge_boundary_num(e_id,bnum)  (E_BOUNDARY_ATTR ?\
                                    (*EINT(e_id,E_BOUNDARY_ATTR) = (bnum)) : 0)
#define get_facet_boundary(f_id)  (F_BOUNDARY_ATTR ? \
                     web.boundaries+*FINT(f_id,F_BOUNDARY_ATTR) : 0)
#define set_facet_boundary_num(f_id,bnum) (F_BOUNDARY_ATTR ?\
                                    (*FINT(f_id,F_BOUNDARY_ATTR) = (bnum)) : 0)
#define get_vertex_vhead(v_id) (vhead + (*VINT(v_id,vhead_attr)))
#define get_vertex_v_normal(v_id) (v_normal[*VINT(v_id,vhead_attr)])
#define set_vertex_vhead(v_id,k) (*VINT(v_id,vhead_attr)=(k))
#define get_body_vhead(b_id) (vhead + (*BINT(b_id,bhead_attr)))
#define set_body_vhead(b_id,k) (*BINT(b_id,bhead_attr)=(k))
#define get_body_cm(b_id)  (bptr(b_id)->cm)


/* Constraints */
typedef unsigned int conmap_t;  /* in element lists */
/* indicates vertex hit constraint */
#define CON_HIT_BIT (((conmap_t)1)<<(8*sizeof(conmap_t)-1))
#define TEMP_CON_HIT_BIT (((conmap_t)1)<<(8*sizeof(conmap_t)-2))
/* maximum possible constraints given size of conmap_t */
#define MAXMAXCON ((((unsigned int)1)<<(8*sizeof(conmap_t)-2))-1)
/* mask to strip hit bit */
#define CONMASK (MAXMAXCON)
/* maximum size of per-element global constraint list */
#define MAXCONPER 23

extern conmap_t nullcon[2];  /* default empty constraint list */
#define get_constraint(n)        (GETCONSTR((n)&CONMASK))
#define get_v_constraint_map(v_id)   \
  ( EXTRAS(VERTEX)[V_CONSTR_LIST_ATTR].array_spec.datacount ? \
  (conmap_t*)(V_EXTRA(v_id,V_CONSTR_LIST_ATTR)) : nullcon)
#define get_e_constraint_map(e_id)  \
  ( EXTRAS(EDGE)[E_CONSTR_LIST_ATTR].array_spec.datacount ? \
  (conmap_t*)(E_EXTRA(e_id,E_CONSTR_LIST_ATTR)) : nullcon)
#define get_f_constraint_map(f_id)  \
  ( EXTRAS(FACET)[F_CONSTR_LIST_ATTR].array_spec.datacount ? \
  (conmap_t*)(F_EXTRA(f_id,F_CONSTR_LIST_ATTR)) : nullcon)

#define get_force(v_id)                   VREAL((v_id),V_FORCE_ATTR)
#define get_force_p(vp)                   VREALP((vp),V_FORCE_ATTR)
#define get_velocity(v_id)                VREAL((v_id),V_VELOCITY_ATTR)
#define get_restore(v_id)                 VREAL((v_id),V_RESTORE_ATTR) 
#define set_vertex_edge(v_id,e)           (vptr(v_id)->e_id = (e))
#define get_vertex_edge(v_id)             (vptr(v_id)->e_id)
#define get_vertex_facet(v_id)      (vptr(v_id)->f_id)
#define set_vertex_facet(v_id,ff_id)       (vptr(v_id)->f_id=ff_id)
#define get_vertex_star(v_id)             (vptr(v_id)->star)
#define set_vertex_star(v_id,a)           (vptr(v_id)->star = (a))
#define add_vertex_star(v_id,a)           (vptr(v_id)->star += (a))
#define set_vertex_valence(v_id,n)        (vptr(v_id)->valence = (n))
#define add_vertex_valence(v_id,n)        (vptr(v_id)->valence += (n))
#define get_vertex_valence(v_id)          (vptr(v_id)->valence)

#define get_fe_wrap(fe_id)          get_edge_wrap(get_fe_edge(fe_id)) 
#define get_fe_tailv(fe_id)         get_edge_tailv(get_fe_edge(fe_id))
#define get_fe_headv(fe_id)         get_edge_headv(get_fe_edge(fe_id))
#define get_fe_midv(fe_id)          get_edge_midv(get_fe_edge(fe_id))
#define set_edge_length(e_id,x)     (eptr(e_id)->length = (x))
#define get_edge_density(e_id)      (eptr(e_id)->density)
#define set_edge_density(e_id,den)  (eptr(e_id)->density = (den))
#define get_edge_star(e_id)         (eptr(e_id)->star)
#define set_edge_star(e_id,a)       (eptr(e_id)->star = (a))
#define add_edge_star(e_id,a)       (eptr(e_id)->star += (a))

#define get_facet_vertices(f_id)   F_ELID(f_id,F_VERTICES_ATTR)
#define get_edge_vertices(e_id)    E_ELID(e_id,E_VERTICES_ATTR) 

#define get_facet_density(f_id)      (fptr(f_id)->density)
#define set_facet_density(f_id,den)  (fptr(f_id)->density = (den))

#define get_facet_area(f_id)    (fptr(f_id)->area)
#define set_facet_area(f_id,a)  (fptr(f_id)->area = (a))
#define add_facet_area(f_id,a)  (fptr(f_id)->area += inverted(f_id)?-(a):(a))

#define get_facet_flux(f_id)    (fptr(f_id)->flux)
#define set_facet_flux(f_id,a)  (fptr(f_id)->flux = (a))

#define get_body_voltimestamp(b_id) (bptr(b_id)->voltimestamp)
#define get_body_volquant(b_id)     (bptr(b_id)->volquant)
#define set_body_volquant(b_id,n)   (bptr(b_id)->volquant = (n))

#define get_body_fixnum(b_id)     (bptr(b_id)->fixnum)
#define set_body_fixnum(b_id,n)   (bptr(b_id)->fixnum = (n))

#define get_body_ambquant(b_id)     (bptr(b_id)->ambquant)
#define set_body_ambquant(b_id,n)   (bptr(b_id)->ambquant = (n))

#define get_body_volmeth(b_id)   (bptr(b_id)->volmeth)
#define set_body_volmeth(b_id,n) (bptr(b_id)->volmeth = (n))

#define save_body_volume(b_id) (bptr(b_id)->oldvolume = bptr(b_id)->volume)
#define restore_body_volume(b_id) (bptr(b_id)->volume = bptr(b_id)->oldvolume)
#define save_body_pressure(b_id) (bptr(b_id)->oldpressure = bptr(b_id)->pressure)
#define restore_body_pressure(b_id) (bptr(b_id)->pressure = bptr(b_id)->oldpressure)
#define save_body_volconst(b_id) (bptr(b_id)->oldvolconst = bptr(b_id)->volconst)
#define restore_body_volconst(b_id) (bptr(b_id)->volconst = bptr(b_id)->oldvolconst)
#define get_body_oldvolume(b_id) (bptr(b_id)->oldvolume)
#define set_body_oldvolume(b_id,vol) (bptr(b_id)->oldvolume = vol)

#define get_body_actualvolume(b_id) (bptr(b_id)->actualvolume)
#define set_body_actualvolume(b_id,vol) (bptr(b_id)->actualvolume = vol)

#define get_v_extra(v_id,n) ((char*)vptr(v_id)  \
         + EXTRAS(VERTEX)[n].offset)
#define get_e_extra(e_id,n) ((char*)eptr(e_id)  \
         + EXTRAS(EDGE)[n].offset)
#define get_f_extra(f_id,n) ((char*)fptr(f_id)  \
         + EXTRAS(FACET)[n].offset)
#define get_b_extra(b_id,n) ((char*)bptr(b_id)  \
         + EXTRAS(BODY)[n].offset)


#define get_attr(id)     (elptr(id)->attr)
#define get_vattr(id)    (vptr(id)->attr)
#define get_eattr(id)    (eptr(id)->attr)
#define get_fattr(id)    (fptr(id)->attr)
#define get_battr(id)    (bptr(id)->attr)
#define get_feattr(id)  (feptr(id)->attr)

#define set_f_phase(f_id,p)  (EXTRAS(FACET)[F_PHASE_ATTR].array_spec.datacount ? (*FINT(f_id,F_PHASE_ATTR) = (p)):0)
#define set_b_phase(b_id,p)  (EXTRAS(BODY)[B_PHASE_ATTR].array_spec.datacount ? (*BINT(b_id,B_PHASE_ATTR) = (p)):0)

#define get_f_phase(f_id)  ((EXTRAS(FACET)[F_PHASE_ATTR].array_spec.datacount && valid_id(f_id)) ? (*FINT(f_id,F_PHASE_ATTR)):0)
#define get_b_phase(b_id)  ((EXTRAS(BODY)[B_PHASE_ATTR].array_spec.datacount && valid_id(b_id)) ? (*BINT(b_id,B_PHASE_ATTR)):0)


#define set_original(id,oid)  (elptr(id)->original = (oid) )
#define get_original(id)  (elptr(id)->original)

#define set_edge_color(e_id,col)    (eptr(e_id)->color = (short)(col))
#define get_edge_color(e_id)            (eptr(e_id)->color)
#define set_facet_color(f_id,col) (fptr(f_id)->color = fptr(f_id)->backcolor = (short)(col))
#define get_facet_color(f_id)            (fptr(f_id)->color)
#define set_facet_frontcolor(f_id,col) \
    (inverted(f_id) ? (fptr(f_id)->backcolor = (short)(col)) : (fptr(f_id)->color = (short)(col)))
#define get_facet_frontcolor(f_id)     \
    (inverted(f_id) ? fptr(f_id)->backcolor : fptr(f_id)->color )
#define set_facet_backcolor(f_id,col) \
    (inverted(f_id) ? (fptr(f_id)->color = (short)(col)) : (fptr(f_id)->backcolor = (short)(col)))
#define get_facet_backcolor(f_id)  \
    (inverted(f_id) ? fptr(f_id)->color : fptr(f_id)->backcolor )


