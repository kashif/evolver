/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/


/********************************************************************
*
*  File: storage.h
*
*  Purpose:  Header file defining details of storage implementation.
*                All machine-dependent gory details should be here
*                (for inclusion in other source files that need to know)
*                or in storage.c (purely private details).
*
*      This version has element ids typed as longs.
*      Also implements id as offset from element base.
*      All elements of same type in one memory block, extended 
*        as needed.  To be used on at least 32 bit machines.
*/

/* using offset as id is 15% faster overall than using ordinal */

#define NUMELEMENTS 5

/* these values used for identification and as index into skeleton info */
#define  VERTEX    0
#define  EDGE      1
#define  FACET     2
#define  BODY      3
#define  FACETEDGE 4 

/* other object type ids, used in mpi transfers for example */
#define  QUANTITY_OBJECT 5
#define  INSTANCE_OBJECT 6

#ifdef xelement
/*****************************************************************
*
*  Universal element identifier.  Don't want to use straight
*  pointer since base changes when realloced.
*  (not actually used; just for documentation)
*/

typedef struct xelement_id {
     unsigned int  type : 3;    /* see enum below */
     unsigned int  valid: 1;    /* valid id bit */
     unsigned int  sign : 1;    /* set for reverse orientation */
     unsigned int  offset: 27;    /* offset from block start */
     } xelement_id;
#endif

#ifndef TASK_ID_BITS
#define TASK_ID_BITS 0
#endif

#define MAX_TASK ((1<<TASK_ID_BITS)-1)

#ifdef LONG_ID
/* shifts for fields */
#define TYPESHIFT  60
#define VALIDSHIFT 59
#define SIGNSHIFT  58
#define TASK_ID_SHIFT (SIGNSHIFT-TASK_ID_BITS)

/* masks for fields */
#define TYPEMASK    (0x7ULL << TYPESHIFT)
#define VALIDMASK   (0x1ULL << VALIDSHIFT)
#define SIGNMASK    (0x1ULL << SIGNSHIFT)
#define OFFSETMASK  ((1ULL<<TASK_ID_SHIFT)-1) 
#define TASKMASK (((1ULL<<SIGNSHIFT)-1)^OFFSETMASK)
 
/* outside storage.*, element_id structure is not visible; */    
typedef unsigned long long int
      element_id, vertex_id, edge_id, facet_id, body_id, facetedge_id; 
#define MPI_IDTYPE MPI_LONG_LONG_INT

/* appropriate ID hash function */
#define id_hash(id) int64hash((id)&~(VALIDMASK|SIGNMASK))

#else
/* masks for fields */
#define TYPEMASK    0xE0000000
#define VALIDMASK   0x10000000
#define SIGNMASK    0x08000000
#define OFFSETMASK  ((1U<<TASK_ID_SHIFT)-1) 
#define TASKMASK (((1U<<SIGNSHIFT)-1)^OFFSETMASK)

/* shifts for fields */
#define TYPESHIFT  29
#define VALIDSHIFT 28
#define SIGNSHIFT  27
#define TASK_ID_SHIFT (SIGNSHIFT-TASK_ID_BITS)

/* outside storage.*, element_id structure is not visible  */    
typedef unsigned int
      element_id, vertex_id, edge_id, facet_id, body_id, facetedge_id; 
#define MPI_IDTYPE MPI_UNSIGNED

/* appropriate ID hash function */
#define id_hash(id) int32hash((id)&~(VALIDMASK|SIGNMASK))

#endif

#define NULLID 0L 

/* to get type of an element */
#define id_type(id)  (int)(((id)&TYPEMASK) >> TYPESHIFT)

/* to give switched orientation of first if that of second is inverted */
#define same_sign(id1,id2)     ((id1) ^ ((id2) & SIGNMASK))

/* to test whether two elements have the same orientation */
#define like_sign(id1,id2)     (((id1) & SIGNMASK) == ((id2) & SIGNMASK))

/* number of elements to allocate memory for at one time */
#define BATCHSIZE 100

/* machine number of id, for MPI */
#define id_task(id) (int)(((id)&TASKMASK) >> TASK_ID_SHIFT)

/* for denoting freed spot in hash table */
#define ELHASH_FREE ((struct element *)1)

/* Macros for getting structure pointer from id */
#ifdef HASH_ID
#define elptr(id)    ((struct element *)(elhash_lookup(id)))
#define vptr(v_id)   ((struct vertex *)(elhash_lookup(v_id)))
#define eptr(e_id)   ((struct edge   *)(elhash_lookup(e_id)))
#define fptr(f_id)   ((struct facet  *)(elhash_lookup(f_id)))
#define bptr(b_id)   ((struct body   *)(elhash_lookup(b_id)))
#define feptr(fe_id) ((struct facetedge *)(elhash_lookup(fe_id)))


#else
/* lookup in ibase pointer list for each element */
#ifdef MPI_EVOLVER
#define elptr(id)   ((struct element *)((id_task(id)==this_task?web.skel[id_type(id)].ibase[(id)&OFFSETMASK]:mpi_remote_elptr(id))))
#define vptr(v_id)   ((struct vertex *)((id_task(v_id)==this_task?web.skel[VERTEX].ibase[(v_id)&OFFSETMASK]:mpi_remote_elptr(v_id))))
#define eptr(e_id)   ((struct edge   *)((id_task(e_id)==this_task?web.skel[EDGE].ibase[(e_id)&OFFSETMASK]:mpi_remote_elptr(e_id))))
#define fptr(f_id)   ((struct facet  *)((id_task(f_id)==this_task?web.skel[FACET].ibase[(f_id)&OFFSETMASK]:mpi_remote_elptr(f_id))))
#define xxbptr(b_id)   ((struct body   *)((id_task(b_id)==this_task?web.skel[BODY].ibase[(b_id)&OFFSETMASK]:mpi_remote_elptr(b_id))))
#define bptr(b_id)   ((struct body   *)(web.skel[BODY].ibase[(b_id)&OFFSETMASK]))
#define feptr(fe_id) ((struct facetedge *)((id_task(fe_id)==this_task?web.skel[FACETEDGE].ibase[(fe_id)&OFFSETMASK]:mpi_remote_elptr(fe_id))))
#else
#define elptr(id)   ((struct element *)(web.skel[id_type(id)].ibase[(id)&OFFSETMASK]))
#define vptr(v_id)   ((struct vertex *)(web.skel[VERTEX].ibase[(v_id)&OFFSETMASK]))
#define eptr(e_id)   ((struct edge   *)(web.skel[EDGE].ibase[(e_id)&OFFSETMASK]))
#define fptr(f_id)   ((struct facet  *)(web.skel[FACET].ibase[(f_id)&OFFSETMASK]))
#define bptr(b_id)   ((struct body   *)(web.skel[BODY].ibase[(b_id)&OFFSETMASK]))
#define feptr(fe_id) ((struct facetedge *)(web.skel[FACETEDGE].ibase[(fe_id)&OFFSETMASK]))
#endif

/* end HASH_ID */
#endif

#define ordinal(id)  (valid_id(id) ? (int)((id) & OFFSETMASK) : -1 )

#ifdef MPI_EVOLVER
#define loc_ordinal(id)  (valid_id(id) ? (int)(elptr(id)->local_id & OFFSETMASK) : -1 )
#else
#define loc_ordinal(id)  (valid_id(id) ? (int)((id) & OFFSETMASK) : -1 )
#endif

/* Macros for producing element name strings for printing. */
/* Note these use global permanent strings to store return string */
/* so printf's with multiple names should use different ELNAMEs. */
/* ELNAME is unsigned, SELNAME is signed */
extern char elnames[10][30];
#ifdef MPI_EVOLVER
#define ELNAME(id) (valid_id(id) ? (sprintf(elnames[0],"%d@%d",(int)((id)&OFFSETMASK)+1,id_task(id)),elnames[0]) : "")
#define ELNAME1(id) (valid_id(id) ? (sprintf(elnames[1],"%d@%d",(int)((id)&OFFSETMASK)+1,id_task(id)),elnames[1]) : "")
#define ELNAME2(id) (valid_id(id) ? (sprintf(elnames[2],"%d@%d",(int)((id)&OFFSETMASK)+1,id_task(id)),elnames[2]) : "")
#define ELNAME3(id) (valid_id(id) ? (sprintf(elnames[3],"%d@%d",(int)((id)&OFFSETMASK)+1,id_task(id)),elnames[3]) : "")
#define ELNAME4(id) (valid_id(id) ? (sprintf(elnames[4],"%d@%d",(int)((id)&OFFSETMASK)+1,id_task(id)),elnames[4]) : "")
#define ELNAME5(id) (valid_id(id) ? (sprintf(elnames[5],"%d@%d",(int)((id)&OFFSETMASK)+1,id_task(id)),elnames[5]) : "")
#define ELNAME6(id) (valid_id(id) ? (sprintf(elnames[6],"%d@%d",(int)((id)&OFFSETMASK)+1,id_task(id)),elnames[6]) : "")
#define ELNAME7(id) (valid_id(id) ? (sprintf(elnames[7],"%d@%d",(int)((id)&OFFSETMASK)+1,id_task(id)),elnames[7]) : "")
#define ELNAME8(id) (valid_id(id) ? (sprintf(elnames[8],"%d@%d",(int)((id)&OFFSETMASK)+1,id_task(id)),elnames[8]) : "")
#define ELNAME9(id) (valid_id(id) ? (sprintf(elnames[9],"%d@%d",(int)((id)&OFFSETMASK)+1,id_task(id)),elnames[9]) : "")
#define SELNAME(id) (valid_id(id) ? (sprintf(elnames[0],"%s%d@%d",(inverted(id)?"-":""),(int)((id)&OFFSETMASK)+1,id_task(id)),elnames[0]) : "")
#define SELNAME1(id) (valid_id(id) ? (sprintf(elnames[1],"%s%d@%d",(inverted(id)?"-":""),(int)((id)&OFFSETMASK)+1,id_task(id)),elnames[1]) : "")
#define SELNAME2(id) (valid_id(id) ? (sprintf(elnames[2],"%s%d@%d",(inverted(id)?"-":""),(int)((id)&OFFSETMASK)+1,id_task(id)),elnames[2]) : "")
#define SELNAME3(id) (valid_id(id) ? (sprintf(elnames[3],"%s%d@%d",(inverted(id)?"-":""),(int)((id)&OFFSETMASK)+1,id_task(id)),elnames[3]) : "")
#define SELNAME4(id) (valid_id(id) ? (sprintf(elnames[4],"%s%d@%d",(inverted(id)?"-":""),(int)((id)&OFFSETMASK)+1,id_task(id)),elnames[4]) : "")
#define SELNAME5(id) (valid_id(id) ? (sprintf(elnames[5],"%s%d@%d",(inverted(id)?"-":""),(int)((id)&OFFSETMASK)+1,id_task(id)),elnames[5]) : "")
#define SELNAME6(id) (valid_id(id) ? (sprintf(elnames[6],"%s%d@%d",(inverted(id)?"-":""),(int)((id)&OFFSETMASK)+1,id_task(id)),elnames[6]) : "")
#define SELNAME7(id) (valid_id(id) ? (sprintf(elnames[7],"%s%d@%d",(inverted(id)?"-":""),(int)((id)&OFFSETMASK)+1,id_task(id)),elnames[7]) : "")
#define SELNAME8(id) (valid_id(id) ? (sprintf(elnames[8],"%s%d@%d",(inverted(id)?"-":""),(int)((id)&OFFSETMASK)+1,id_task(id)),elnames[8]) : "")
#define SELNAME9(id) (valid_id(id) ? (sprintf(elnames[9],"%s%d@%d",(inverted(id)?"-":""),(int)((id)&OFFSETMASK)+1,id_task(id)),elnames[9]) : "")
#else
#define ELNAME(id) (valid_id(id) ? (sprintf(elnames[0],"%d",(int)((id)&OFFSETMASK)+1),elnames[0]) : "")
#define ELNAME1(id) (valid_id(id) ? (sprintf(elnames[1],"%d",(int)((id)&OFFSETMASK)+1),elnames[1]) : "")
#define ELNAME2(id) (valid_id(id) ? (sprintf(elnames[2],"%d",(int)((id)&OFFSETMASK)+1),elnames[2]) : "")
#define ELNAME3(id) (valid_id(id) ? (sprintf(elnames[3],"%d",(int)((id)&OFFSETMASK)+1),elnames[3]) : "")
#define ELNAME4(id) (valid_id(id) ? (sprintf(elnames[4],"%d",(int)((id)&OFFSETMASK)+1),elnames[4]) : "")
#define ELNAME5(id) (valid_id(id) ? (sprintf(elnames[5],"%d",(int)((id)&OFFSETMASK)+1),elnames[5]) : "")
#define ELNAME6(id) (valid_id(id) ? (sprintf(elnames[6],"%d",(int)((id)&OFFSETMASK)+1),elnames[6]) : "")
#define ELNAME7(id) (valid_id(id) ? (sprintf(elnames[7],"%d",(int)((id)&OFFSETMASK)+1),elnames[7]) : "")
#define ELNAME8(id) (valid_id(id) ? (sprintf(elnames[8],"%d",(int)((id)&OFFSETMASK)+1),elnames[8]) : "")
#define ELNAME9(id) (valid_id(id) ? (sprintf(elnames[9],"%d",(int)((id)&OFFSETMASK)+1),elnames[9]) : "")
#define SELNAME(id) (valid_id(id) ? (sprintf(elnames[0],"%s%d",(inverted(id)?"-":""),(int)((id)&OFFSETMASK)+1),elnames[0]) : "")
#define SELNAME1(id) (valid_id(id) ? (sprintf(elnames[1],"%s%d",(inverted(id)?"-":""),(int)((id)&OFFSETMASK)+1),elnames[1]) : "")
#define SELNAME2(id) (valid_id(id) ? (sprintf(elnames[2],"%s%d",(inverted(id)?"-":""),(int)((id)&OFFSETMASK)+1),elnames[2]) : "")
#define SELNAME3(id) (valid_id(id) ? (sprintf(elnames[3],"%s%d",(inverted(id)?"-":""),(int)((id)&OFFSETMASK)+1),elnames[3]) : "")
#define SELNAME4(id) (valid_id(id) ? (sprintf(elnames[4],"%s%d",(inverted(id)?"-":""),(int)((id)&OFFSETMASK)+1),elnames[4]) : "")
#define SELNAME5(id) (valid_id(id) ? (sprintf(elnames[5],"%s%d",(inverted(id)?"-":""),(int)((id)&OFFSETMASK)+1),elnames[5]) : "")
#define SELNAME6(id) (valid_id(id) ? (sprintf(elnames[6],"%s%d",(inverted(id)?"-":""),(int)((id)&OFFSETMASK)+1),elnames[6]) : "")
#define SELNAME7(id) (valid_id(id) ? (sprintf(elnames[7],"%s%d",(inverted(id)?"-":""),(int)((id)&OFFSETMASK)+1),elnames[7]) : "")
#define SELNAME8(id) (valid_id(id) ? (sprintf(elnames[8],"%s%d",(inverted(id)?"-":""),(int)((id)&OFFSETMASK)+1),elnames[8]) : "")
#define SELNAME9(id) (valid_id(id) ? (sprintf(elnames[9],"%s%d",(inverted(id)?"-":""),(int)((id)&OFFSETMASK)+1),elnames[9]) : "")
#endif

/* Macros for manipulating and testing element ids */
#define edge_inverse(id)   inverse_id(id)
#define facet_inverse(id)  inverse_id(id)
#define fe_inverse(id)     inverse_id(id)
#define invert(id)         ((id) ^= SIGNMASK)
#define equal_id(a,b)      ((a)==(b))
#define equal_element(a,b) (((a)|SIGNMASK) == ((b)|SIGNMASK))
#define valid_id(id)       ((int)(((id)&VALIDMASK) >> VALIDSHIFT))
#define inverted(id)       ((int)(((id)&SIGNMASK) >> SIGNSHIFT))
#define inverse_id(id)     ((id) ^ SIGNMASK)
#define positive_id(id)    ((id) & ~SIGNMASK)
typedef int ORDTYPE;       /* element numbering type */

typedef struct element *INDIRECT_TYPE; 
extern struct blocklist_struct
     { struct element *blockptr; /* allocated block */
       int start_ord;            /* ordinal of first element */
       int count;                /* elements in block */
     } *blocklist[NUMELEMENTS];
extern int blockcount[NUMELEMENTS];  /* how many blocks allocated */
extern int blockmax[NUMELEMENTS];  /* length of blocklist */

/* individual indirect block pointer arrays */
extern INDIRECT_TYPE *vibase;
extern INDIRECT_TYPE *eibase;
extern INDIRECT_TYPE *fibase;
extern INDIRECT_TYPE *bibase;
extern INDIRECT_TYPE *feibase;


/* for getting id1 with orientation sign of id2 */
#define copy_sign(id1,id2)     (((id1)&~SIGNMASK) | ((id2) & SIGNMASK))
