/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/********************************************************************
*
*  File: storage.c
*
*  Purpose:  File defining details of storage implementation.
*            All machine-dependent gory details should be here
*            (purely private details) or in storage.h
*            (for inclusion in other source files that need to know).
*                
*        This version has element ids implemented as longs.
*/
#include "include.h"

struct blocklist_struct *blocklist[NUMELEMENTS]; /* list of allocated blocks */
int blockcount[NUMELEMENTS];  /* how many blocks allocated */
int blockmax[NUMELEMENTS];  /* length of blocklist */


int sparse_ibase_flag; /* for permitting sparse ibase */

/* individual indirect block pointer arrays, handy for debugging */
/* set to web.skel[].ibase */
INDIRECT_TYPE *vibase;
INDIRECT_TYPE *eibase;
INDIRECT_TYPE *fibase;
INDIRECT_TYPE *bibase;
INDIRECT_TYPE *feibase;

/* The web structure */
struct webstruct web; 

element_id
  NULLVERTEX  =  (element_id)VERTEX  << TYPESHIFT, 
  NULLEDGE     =  (element_id)EDGE  << TYPESHIFT,
  NULLFACET    =  (element_id)FACET  << TYPESHIFT,
  NULLBODY     =  (element_id)BODY  << TYPESHIFT,  
  NULLFACETEDGE =  (element_id)FACETEDGE << TYPESHIFT;

#ifdef ELPTRFUNC
struct element *elptr(id)
element_id id;
{
  int type = id_type(id);
#ifdef MPI_EVOLVER
  if ( id_task(id) != this_task ) 
    return mpi_remote_elptr(id);
#endif
  return (struct element *)(web.skel[type].ibase[id & OFFSETMASK]);
}
#endif

int oid(id)
element_id id;
{ return inverted(id) ? -(ordinal(id)+1) : (ordinal(id) +1) ;
}
/* handy for debugging */
struct vertex * Vptr ARGS((element_id)); 
struct edge * Eptr ARGS((element_id));
struct facet * Fptr ARGS((element_id));
struct body * Bptr ARGS((element_id)); 
struct facetedge * Feptr ARGS((element_id));

struct vertex * Vptr(id) vertex_id id; { return vptr(id); }
struct edge * Eptr(id) edge_id id; { return eptr(id); }
struct facet * Fptr(id) facet_id id; { return fptr(id); }
struct body * Bptr(id) body_id id; { return bptr(id); }
struct facetedge * Feptr(id) facetedge_id id; { return feptr(id); }

/***********************************************************************
*
*  function: expand()
*
*  purpose:  Increase size of element structures.  Works only for
*                indexed or indirect id. Will also decrease size.
*/
void expand(type,newsize)
int type;  /* VERTEX, etc. */
int newsize; /* new size of structure */
{ char *newblock,*oldblock;
  int oldsize = web.sizes[type];
  int count = web.skel[type].maxcount;
  int i,n;                                   
  char *newptr,*oldptr;
  int copysize;

  /* round up newsize to alignment for doubles */
  newsize = ((newsize+sizeof(REAL)-1)/sizeof(REAL))*sizeof(REAL);
  if ( newsize == web.sizes[type] ) return; /* don't have to expand */
  web.sizes[type] = newsize;
  if ( count == 0 ) return; /* don't have to reallocate space */
  copysize = (newsize < oldsize) ? newsize : oldsize;

  ENTER_GRAPH_MUTEX;
  /* Don't mess with structures while graph thread using them */

#ifdef HASH_ID
  web.skel[type].freehead = NULL;
#endif

  for ( n = 0 ; n < blockcount[type] ; n++ )
  { INDIRECT_TYPE *iptr;
     char *spot;
     struct blocklist_struct *b = blocklist[type] + n;

     newblock = mycalloc(b->count,newsize);
     for ( i = 0, oldptr = (char*)(b->blockptr), newptr = newblock; 
             i < b->count ; i++, oldptr += oldsize, newptr += newsize )
     { memcpy(newptr,oldptr,copysize);
       #ifdef MPI_EVOLVER
       { element_id id =  ((struct element*)oldptr)->self_id;
         if ( id_task(id) != this_task )
         { /* update pointer in remote_elements */
           remote_elements[type].ibase[id_task(id)][ordinal(id)] = 
               (struct element *)newptr;   
          }
       }
       #endif
     }
     oldblock = (char*)b->blockptr;
     b->blockptr = (struct element*)newblock;
     /* update indirect pointers */
     /* keeping in mind structures not necessarily in id order 
        due to reorder command */
     for ( i=0,spot=newblock, iptr=web.skel[type].ibase+b->start_ord ; 
                i<b->count ; i++,spot+=newsize,iptr++ )
     {  
#ifdef MPI_EVOLVER
         element_id id = ((struct element *)spot)->local_id;
#else
         element_id id = ((struct element *)spot)->self_id;
#endif

#ifdef HASH_ID
      { struct element *el_ptr;
        el_ptr = (struct element *)spot;
        if ( el_ptr->self_id ) /* replace in hash table */
          elhash_replace(id,el_ptr);
        else /* add to freelist */
        { *(struct element **)&(el_ptr->forechain) = web.skel[type].freehead;
          web.skel[type].freehead = el_ptr;
        }
      }
#else
        web.skel[type].ibase[ordinal(id)] = (struct element *)spot;
#endif
     }
     myfree(oldblock);
  }

  LEAVE_GRAPH_MUTEX;

  parallel_update_flag[type] = 1;
} 

/***********************************************************************
*
*  function: extend()
*
*  purpose: allocate more empty element structures
*/

void extend(type,mode)
int type;
int mode; /* EXTEND_BATCH or EXTEND_FOR_REFINE */
{
#ifdef HASH_ID
  elhash_extend(type,mode);
#else
  int number=0;  /* of new items to allocate */
  char *newblock;
  INDIRECT_TYPE *newiblock;
  element_id id,backid;
  struct element *newptr = NULL;
  int k;
  int allocsize;
  long  oldnum = web.skel[type].maxcount;
  long  newnum=0;
  int neword;


  ENTER_GRAPH_MUTEX;
  /* Don't mess with structures while graph thread using them */

  if ( blockcount[type] >= blockmax[type] )
  { blocklist[type] = (struct blocklist_struct*)
      kb_realloc((char*)(blocklist[type]),
         (blockcount[type]+BATCHSIZE)*sizeof(struct blocklist_struct));
     blockmax[type] += BATCHSIZE;
  }
  if ( mode == EXTEND_BATCH )
  {
    /* calculate number of structures to fit in block size just under 2^n */
    allocsize = BATCHSIZE*web.sizes[type];
    k = 0x100 ; while ( k < allocsize ) k <<= 1 ;
    number = (k-16)/web.sizes[type]; /* maybe room for block header */
    newnum = web.skel[type].maxcount + number;
  }
  else if ( mode == EXTEND_FOR_REFINE )
  { /* increase by 2^surface_dimension factor */
    if ( type == BODY ) goto extend_exit;   /* don't need more bodies */
    number = web.skel[type].count*(1<<web.dimension)
                - web.skel[type].maxcount + 100;
    newnum = web.skel[type].maxcount + number;
    if ( number <= 0 ) { goto extend_exit; }/* don't need any more */
  }
  else kb_error(2474,"Internal error: bad mode for extend().\n",RECOVERABLE);
  
  if ( (web.skel[type].maxcount + number) > OFFSETMASK )
  { sprintf(errmsg, "Trying to allocate more %s than ID format allows, %Ld\n",
          typenames[type],OFFSETMASK);
    kb_error(3712,errmsg,RECOVERABLE);
  }

  newblock = mycalloc(number,web.sizes[type]);
  blocklist[type][blockcount[type]].start_ord = oldnum ;
  blocklist[type][blockcount[type]].count = number ;
  blocklist[type][blockcount[type]++].blockptr = (struct element *)newblock;

  while ( newnum > web.skel[type].ialloc )
  {
    if ( web.skel[type].ibase == NULL )
    { newiblock = (INDIRECT_TYPE*)mycalloc(number,sizeof(INDIRECT_TYPE));
      web.skel[type].ialloc = number;
    }
    else
    { int allocnum = web.skel[type].ialloc < 100 ? 100 : 2*web.skel[type].ialloc;
      newiblock = (INDIRECT_TYPE*)kb_realloc((char*)(web.skel[type].ibase),
        allocnum*sizeof(INDIRECT_TYPE));
      web.skel[type].ialloc = allocnum;
    }
    web.skel[type].ibase = newiblock;
    switch(type)
    {    case VERTEX: vibase = newiblock; break;
         case EDGE:    eibase = newiblock; break;
         case FACET:  fibase = newiblock; break;
         case BODY:    bibase = newiblock; break;
         case FACETEDGE: feibase = newiblock; break;
    }
  }

  /* add to end of freelist */

  /* find first empty slot */
  if ( sparse_ibase_flag )
  { /* search for empty ibase slots from start */
    for ( neword = 0 ; neword < web.skel[type].ialloc ; neword++ )
      if ( web.skel[type].ibase[neword] == NULL )
        break;
  }
  else /* dense ibase */
    neword = web.skel[type].maxcount;

  id = ((element_id)type << TYPESHIFT) | VALIDMASK | neword; 
#ifdef MPI_EVOLVER
  id |= (element_id)this_task << TASK_ID_SHIFT;  
#endif

  if ( valid_id(web.skel[type].freelast) )
  { backid = web.skel[type].freelast;
    elptr(backid)->forechain = id;
  }
  else
  { backid = NULLID;
    web.skel[type].free = id;
  }

  for ( k = 0 ; k < number ; k++ )
  { newptr = (struct element *)(newblock + k*web.sizes[type]); 
    web.skel[type].ibase[neword] = newptr;
    newptr->self_id = id;
    #ifdef MPI_EVOLVER
      newptr->local_id = id;
    #endif
    if ( k < number-1 )
    {
      if ( sparse_ibase_flag )
      { for ( neword = 0 ; neword < web.skel[type].ialloc ; neword++ )
          if ( web.skel[type].ibase[neword] == NULL )
            break;
      }
      else 
        neword++;
      id = ((element_id)type << TYPESHIFT) | VALIDMASK | neword; 
#ifdef MPI_EVOLVER
      id |= (element_id)this_task << TASK_ID_SHIFT;  
#endif

      newptr->forechain = id;
    }
    newptr->backchain = backid;
    backid = newptr->self_id;

  }

  web.skel[type].freelast = backid;
  web.skel[type].maxcount += number;
  
extend_exit:
  LEAVE_GRAPH_MUTEX;
  
/* end ifdef HASH_ID */
#endif


}

/************************************************************************
*
* function: new_element()
*
* purpose: Allocate new element from freelist
*
*/

element_id new_element(type,parent,desired_id)
int type;
element_id parent; /* for inherited stuff */
element_id desired_id; /* or NULLID */
{
  element_id newid;
  struct element *newptr,*last;
  ORDTYPE ord;
  
#ifdef HASH_ID
  newptr = elhash_new_element(type,desired_id);
  newid = newptr->self_id;
#else
  newid = web.skel[type].free;
  
  if ( !valid_id(newid) ) /* free list empty */
  { if ( aggregate_depth == 0 )
    { free_discards(DISCARDS_SOME);
      newid = web.skel[type].free;
    }
  }

  if ( !valid_id(newid) ) /* free list empty */
  { extend(type,EXTEND_BATCH);
    newid = web.skel[type].free;
  }
  newptr = elptr(newid);

  /* remove from free chain */
  web.skel[type].free = newptr->forechain;
  if ( valid_id(web.skel[type].free) )
    elptr(web.skel[type].free)->backchain = NULLID;
  else
    web.skel[type].freelast = NULLID;
#endif

  /* clean out old info */
  ord = ordinal(newid);
  memset((char *)newptr,0,web.sizes[type]);
  if ( ord > web.skel[type].max_ord ) 
  { web.skel[type].max_ord = ord;
  }
  newptr->original = NULLID;

#ifndef HASH_ID
  if ( match_id_flag && !addload_flag && datafile_flag )
  { /* link in numerical order */
    int i;
    struct element *prev,*next;
    for ( i = ord-1 ; i >= 0 ; i-- )
    { prev = web.skel[type].ibase[i];
      if ( prev && (prev->attr & ALLOCATED) )
      { if ( valid_id(prev->forechain) )
        { next = elptr(prev->forechain);
          next->backchain = newid;
        }
        newptr->forechain = prev->forechain;
        prev->forechain = newid;
        newptr->backchain = prev->self_id;
        break;
      }
    }
    if ( i < 0 )  /* no predecessor */
    { newptr->forechain = web.skel[type].used;
      if ( valid_id(web.skel[type].used) )
      { next = elptr(web.skel[type].used);
        next->backchain = newid;
      }
      web.skel[type].used = newid;
    }
    if ( !valid_id(newptr->forechain) ) web.skel[type].last = newid;
  }
  else
  #endif
  { /* add to end of in-use chain */
    newptr->forechain = NULLID;
    newptr->backchain = web.skel[type].last;
    if ( valid_id(web.skel[type].last) )
    {
       last = elptr(web.skel[type].last);     /* find end of in-use chain */
       last->forechain = newid;
    }
    else
    {        
       web.skel[type].used = newid;
    }
    web.skel[type].last = newid;
  }

  newptr->attr = ALLOCATED | NEWELEMENT;

  newptr->self_id = newid;
  #ifdef MPI_EVOLVER
  newptr->local_id = newid;
  #endif
  web.skel[type].count++;  
  
  #ifdef MPI_EVOLVER
  /* kludge to guarantee max_ord gives enough spaces */
  if ( web.skel[type].count > web.skel[type].max_ord+1 )
    web.skel[type].max_ord = web.skel[type].count-1;
  #endif

  /* inherited attributes and named methods from parent */
  if ( valid_id(parent) )
  { struct element *e_ptr = elptr(parent);
    int *instlist = (int*)((char*)e_ptr + get_meth_offset(id_type(parent)));
    int i;

    newptr->attr |= e_ptr->attr & (NODISPLAY|FIXED|BOUNDARY|NEGBOUNDARY|
        BDRY_ENERGY|CONSTRAINT|BDRY_CONTENT|NEGCONSTRAINT|BARE_NAKED);

    if ( e_ptr->attr & CONSTRAINT )
    { conmap_t *parent_conmap=NULL;
      switch ( id_type(parent) )
      { case VERTEX: parent_conmap = get_v_constraint_map(parent); break;
        case EDGE  : parent_conmap = get_e_constraint_map(parent); break;
        case FACET : parent_conmap = get_f_constraint_map(parent); break;
      }
      switch ( type )
      { case VERTEX: set_v_conmap(newid,parent_conmap); break; 
        case EDGE  : set_e_conmap(newid,parent_conmap); break; 
        case FACET : set_f_conmap(newid,parent_conmap); break; 
      }
    }
    if ( e_ptr->attr & BOUNDARY )
    { int bnum=0;
      switch ( id_type(parent) )
      { case VERTEX: bnum = get_vertex_boundary_num(parent); break;
        case EDGE  : bnum = get_edge_boundary_num(parent); break;
        case FACET : bnum = get_facet_boundary_num(parent); break;
      }
      switch ( type )
      { case VERTEX: set_boundary_num(newid,bnum); break;
        case EDGE  : set_edge_boundary_num(newid,bnum); break;
        case FACET : set_facet_boundary_num(newid,bnum); break;
      }
    }

    for ( i = 0 ; i < (int)e_ptr->method_count ; i++ )
    { if ( METH_INSTANCE(abs(instlist[i]))->type <= type )
         apply_method_num(newid,instlist[i]);
    }
  }

  return newid;
} /* end new_element() */

/**************************************************************************
*
* function: free_element()
*
* purpose: mark element as unallocated, but leave it in the chain of
*          used elements.  Moved to freelist later by free_discards.
*/
void free_element(id)  
element_id id;
{
  struct element *ptr;
  int type = id_type(id);

  if ( !valid_element(id) )
  {
    sprintf(errmsg,
      "Internal error: Trying to free invalid element %s id %s \n",
            typenames[type],ELNAME(id));
    kb_error(1311,errmsg,WARNING);
    return;
  }
     
  if ( type == EDGE ) /* remove from vertex lists */
  { vertex_id tailv = get_edge_tailv(id);
    vertex_id headv = get_edge_headv(id);
    vertex_id midv;
    int i; 
    remove_vertex_edge(tailv,id);
    remove_vertex_edge(headv,inverse_id(id));
    if ( web.modeltype == QUADRATIC )
    { midv = get_edge_midv(id);
      if ( valid_id(midv) )
        free_element(midv);
     }
    if ( web.modeltype == LAGRANGE )
    { vertex_id *v = get_edge_vertices(id);
      for ( i = 1 ; i < web.lagrange_order ; i++ )
      { if ( valid_id(v[i]) )
          free_element(v[i]);
      }
    }
  }
    
  if ( type == FACET ) 
  { 
    /* remove from body facet lists */
    set_facet_body(id,NULLID);
    set_facet_body(inverse_id(id),NULLID);

  }
  ptr = elptr(id);
  if ( !(ptr->attr & ALLOCATED) )
  { sprintf(errmsg,
      "Internal error: Trying to free unallocated element type %d id %s \n",
            type,ELNAME(id));
    kb_error(1313,errmsg,WARNING);
    return;
  }
  ptr->attr &= ~ALLOCATED;

  #ifdef MPI_EVOLVER
  if ( id_task(id) != this_task )
  { struct element *e;
    mpi_remove_remote(id);
    if ( valid_id(ptr->forechain) )
    { e = elptr(ptr->forechain);
      e->backchain = ptr->local_id;
    }
    else
      web.skel[type].last = ptr->local_id;
    if ( valid_id(ptr->backchain) )
    { e = elptr(ptr->backchain);
      e->forechain = ptr->local_id;
    }
    else
      web.skel[type].used = ptr->local_id;
  }
  else
    mpi_remove_export(id); /* remove from export list */
  #endif


#ifdef OLDDISCARD
  /* remove from in-use list */
  if ( valid_id(ptr->forechain) )
     elptr(ptr->forechain)->backchain = ptr->backchain;
  else
     web.skel[type].last = ptr->backchain;

/**** DON'T DO THIS! Don't mess with forechains until free_discards()! */
  if ( valid_id(ptr->backchain) )
    {
      /*     elptr(ptr->backchain)->forechain = ptr->forechain;  */
    }
  else
     web.skel[type].used = ptr->forechain;
/*****/

  /* add to discard list */
  /* save forechain for generators */
  ptr->backchain = web.skel[type].discard;
  web.skel[type].discard = id & (TYPEMASK | VALIDMASK | OFFSETMASK);
                                 /* clear old bits, keep only needed */
#else
  web.skel[type].discard_count++;
#endif

  web.skel[type].count--; 

}

/* reclaim element from discard list */
void unfree_element(id)
element_id id;
{
  struct element *ptr;
  int type = id_type(id);

  if ( !valid_id(id) )
  { sprintf(errmsg,"Internal error: Trying to unfree invalid id %08lX \n",
        (unsigned long)id);
    kb_error(1314,errmsg,RECOVERABLE);
  }
    
  ptr = elptr(id);
  if ( ptr->attr & ALLOCATED )
  { sprintf(errmsg,
     "Internal error: Trying to unfree allocated element id %08lX \n",
          (unsigned long)id);
    kb_error(1315,errmsg,RECOVERABLE);
  }
  ptr->attr |= ALLOCATED;

  #ifdef MPI_EVOLVER
  if ( id_task(id) != this_task )
    mpi_unfree_element(id,ptr);
  #endif

  web.skel[type].discard_count--;

  web.skel[type].count++;  
}

/* index as id */
element_id get_ordinal_id(type,ord)
int type; /* type of element */
int ord;  /* ordinal of element, signed */
{ element_id id;
  struct element *ep;

  if ( (type < 0) || (type > NUMELEMENTS) ) return NULLID;
  if ( abs(ord) > web.skel[type].max_ord ) return NULLID;
  id = ((element_id)type << TYPESHIFT) | VALIDMASK | abs(ord);
  if ( ord < 0 ) invert(id);

#ifdef MPI_EVOLVER
  id |= (element_id)this_task << TASK_ID_SHIFT;
#endif
  ep = elptr(id);
  if ( ep == NULL ) return NULLID;
  if ( ep->attr & ALLOCATED ) return id;
  return NULLID;
}

/* completion of partial id */
element_id get_full_id(type,partid)
int type; /* type of element */
element_id partid;  /* ordinal of element, 0-based, with sign bit and mpi task number */
                    /* partid should also have VALIDMASK if not known bad */
{ element_id id;
  struct element *ep;
  int task = id_task(partid);
  int ord = (int)(partid & OFFSETMASK);

  if ( (type < 0) || (type > NUMELEMENTS) ) return NULLID;
#ifdef MPI_EVOLVER
  if ( task == this_task )
#endif
    if ( ord > web.skel[type].max_ord ) return NULLID;
  #ifdef MPI_EVOLVER
  if ( task >= mpi_nprocs )
	return NULLID;
  #endif
  id = partid | ((element_id)type << TYPESHIFT);

  ep = elptr(id|VALIDMASK);
  if ( ep == NULL ) return NULLID;
  if ( ep->attr & ALLOCATED ) return id|VALIDMASK;
  return NULLID;
}

int generate_all(type,idptr,sentinel)  /* re-entrant */
int type;
element_id *idptr;
element_id *sentinel; /* to record original end of list */
{
  struct element *ptr;

  if ( !valid_id(*idptr) ) /* first time */
  { *idptr = web.skel[type].used;
    *sentinel = web.skel[type].last;   /* may be a discard */
    if ( !valid_id(*idptr) ) return 0;
    ptr = elptr(*idptr);
    if ( (ptr->attr & ALLOCATED)
    #ifdef MPI_EVOLVER
     && (id_task(ptr->self_id) == this_task)
    #endif
     )
      return 1;
  }
   
    ptr = elptr(*idptr);
    do
      { if ( equal_id(*idptr,*sentinel) ) return 0;
      *idptr = ptr->forechain;
      if ( !valid_id(*idptr) ) return 0;
      ptr = elptr(*idptr);
    }
    while ( !(ptr->attr & ALLOCATED)
    #ifdef MPI_EVOLVER
     || (id_task(ptr->self_id) != this_task)
    #endif
     );


  return 1;
}

/***************************************************************************
* 
* function: memory_report()
*
* purpose: implement 'c' command memory usage report
*/
void memory_report()
{
    long mem;
    int k;

    #ifdef MPI_EVOLVER
    if ( this_task == 0 )
    { mpi_count_report();
    }
    else
    #endif
    {
    mem = 0;
    for ( k = 0 ; k < NUMELEMENTS ; k++ )
      mem += web.skel[k].count*web.sizes[k];

    sprintf(errmsg,
    "Vertices: %ld  Edges: %ld  Facets: %ld  Bodies: %ld  Facetedges: %ld\nElement memory: %ld\n",
                web.skel[0].count,web.skel[1].count,web.skel[2].count,
                web.skel[3].count,web.skel[4].count, mem);
    outstring(errmsg);
    }

    if ( verbose_flag )
    { /* report element sizes */
      for ( k = 0 ; k < NUMELEMENTS ; k++ )
      { sprintf(msg,"%10.10s size: %4d bytes;  number allocated: %d\n",
          typenames[k],web.sizes[k],web.skel[k].maxcount);
        outstring(msg);
      }
      sprintf(msg,"quantity size:   %4d bytes;  number allocated: %4d\n",
          sizeof(struct gen_quant),gen_quant_list_max);
      outstring(msg);
      sprintf(msg,"instance size:   %4d bytes;  number allocated: %4d\n",
          sizeof(struct method_instance),meth_inst_list_max);
      outstring(msg);
    }

#if defined(_WIN32) && defined(_HEAPOK)
#ifndef _HEAPINFO
#define _HEAPINFO _heapinfo
#endif

    if ( _heapchk() != _HEAPOK )
      kb_error(1317,"Internal error: Corrupt heap! Memory is trashed.\n",WARNING);
    else if ( verbose_flag )
    { struct _HEAPINFO hinfo;
      int b_use=0,b_free=0;
      size_t mem_use=0,mem_free=0;
      char * heaptop = NULL; 
      char * heapstart = NULL;
      MEMORYSTATUS memstat;

      hinfo._pentry = NULL;
      while ( _heapwalk(&hinfo) == _HEAPOK )
      {   if ( heapstart == NULL )
             heapstart = (char*)hinfo._pentry; 
 
#ifdef HEAPLIST
         sprintf(errmsg,"%p %10ld %s end %p\n",hinfo._pentry,hinfo._size,
             hinfo._useflag ? "used" : "free",
                  (char*)hinfo._pentry+hinfo._size);
          outstring(errmsg);
#endif
       if (hinfo._useflag)
        {   
          b_use++; mem_use+= hinfo._size;
        }
        else { b_free++; mem_free += hinfo._size; }
        if ( (char*)hinfo._pentry + hinfo._size > heaptop )
          heaptop = (char*)hinfo._pentry + hinfo._size;
      }
      outstring("\n");
      sprintf(errmsg,"blocks in use: %d    memory in use: %ld \n",
                    b_use,mem_use);
      outstring(errmsg);
      sprintf(errmsg,"blocks free:    %d    memory free:    %ld \n",
                 b_free,mem_free);
      outstring(errmsg);
      sprintf(errmsg,"Heap top: %p\n",heaptop);
      outstring(errmsg);
      sprintf(errmsg,"Heap size: %4.2f MB\n",
             (heaptop-heapstart)/1024./1024.);
      outstring(errmsg);

      memstat.dwLength = sizeof(memstat);
      GlobalMemoryStatus(&memstat);
      sprintf(errmsg,"Physical memory size: %d bytes   Virtual memory top: %X\n",
           memstat.dwTotalPhys,memstat.dwTotalVirtual);
      outstring(errmsg);
    }
#endif

#if defined(_UNISTD_H)
  if ( verbose_flag )
  { /* do this only on unix systems with unistd.h */
    sprintf(msg,"\nTotal data memory arena %d\n",
      (char*)sbrk(0)-(char*)&evolver_version);
    outstring(msg); 
  }
#endif

#if defined(M_MXFAST) && defined(IRIS)
  if ( verbose_flag ) 
     { char *ptr;
        struct mallinfo m;
        /* using libmalloc.a for debugging */
        ptr = malloc(10);
        if ( ptr == NULL )
          erroutstring("Bad heap.\n");
        else myfree(ptr);
        m = mallinfo();
        sprintf(msg,"Arena %d     Ordblocks: %d    Orduse: %d     Ordfree: %d\n",
            m.arena,m.ordblks,m.uordblks,m.fordblks);
        outstring(msg);
        sprintf(msg,"Small blocks: %d Small use: %d Small free: %d\n",
            m.smblks,m.usmblks,m.fsmblks);
        outstring(msg);
}
#endif

#if defined(SUNXX)
    if ( memdebug )
      if ( malloc_verify() != 1 )
         kb_error(1318,"Internal error: Malloc_verify() failed.\n.",RECOVERABLE);

#endif

  mem_list_summary();
  dy_check();
}

/**************************************************************************
*
* function: reset_skeleton()
*
* purpose: Clean out old surface and initialize empty web.
*          Note all permanently allocated memory cleaned out en masse
*               previously.
*
*/

void reset_skeleton()
{
  int i;
  int three = FACET_VERTS;
  int permcount = web.perm_global_count;
  int maxperm   = web.max_perm_globals;
  struct global *perm = perm_globals(0);

  ENTER_GRAPH_MUTEX

  gauss1Dpt = NULL; 
  gauss1Dwt = NULL;

  memset((char *)&web,0,sizeof(web));  /* total clean out */
  
  web.perm_global_count = permcount;
  web.max_perm_globals = maxperm;
  dy_perm_globals = perm;

  web.sizes[VERTEX] = sizeof(struct vertex);
  web.sizes[EDGE] = sizeof(struct edge);
  web.sizes[FACET] = sizeof(struct facet);
  web.sizes[BODY] = sizeof(struct body);
  web.sizes[FACETEDGE] = sizeof(struct facetedge);
  web.usedsizes[VERTEX] = sizeof(struct vertex);
  web.usedsizes[EDGE] = sizeof(struct edge);
  web.usedsizes[FACET] = sizeof(struct facet);
  web.usedsizes[BODY] = sizeof(struct body);
  web.usedsizes[FACETEDGE] = sizeof(struct facetedge);

  for ( i = 0 ; i < NUMELEMENTS ; i++ ) 
  { web.skel[i].max_ord = -1;
    blocklist[i] = NULL;
    blockmax[i] = blockcount[i] = 0;
  }
  vibase = NULL;
  eibase = NULL;
  fibase = NULL;
  bibase = NULL;
  feibase = NULL;

  web.skel[EDGE].dimension = 1;
  web.skel[FACET].dimension = 2;
  web.skel[BODY].dimension = 3;

  LEAVE_GRAPH_MUTEX 

  /* set up permanent attributes, empty to start with */
  /* have REAL attributes first, so can align on 8-byte */
  /* Be sure the order given here is same as order in skeleton.h */
  /* following each element structure definition. */
  /* vertex */
  add_attribute(VERTEX,"__x",REAL_TYPE,1,NULL,0,NULL);
  EXTRAS(VERTEX)[V_COORD_ATTR].flags |= RECALC_ATTR;
  add_attribute(VERTEX,"__oldx",REAL_TYPE,1,NULL,0,NULL);
  add_attribute(VERTEX,"__p",REAL_TYPE,1,NULL,0,NULL);
  EXTRAS(VERTEX)[V_PARAM_ATTR].flags |= RECALC_ATTR;
  add_attribute(VERTEX,"__force",REAL_TYPE,1,NULL,0,NULL);
  add_attribute(VERTEX,"__velocity",REAL_TYPE,1,NULL,0,NULL);
  add_attribute(VERTEX,"__v_constraint_list",INTEGER_TYPE,1,NULL,0,NULL);
  web.meth_attr[VERTEX] = 
        add_attribute(VERTEX,"__v_method_list",INTEGER_TYPE,1,NULL,0,NULL);
  add_attribute(VERTEX,"__vertex_normal",REAL_TYPE,1,NULL,0,NULL);
  /* edge */
  add_attribute(EDGE,"density",REAL_TYPE,0,NULL,0,NULL);
  add_attribute(EDGE,"__e_vertices",ELEMENTID_TYPE,1,NULL,0,NULL);
  add_attribute(EDGE,"__edge_vector",REAL_TYPE,1,NULL,0,NULL);/*dummy*/
  add_attribute(EDGE,"__wrap_list",INTEGER_TYPE,1,NULL,0,NULL);
  add_attribute(EDGE,"__e_constraint_list",INTEGER_TYPE,1,NULL,0,NULL);
  web.meth_attr[EDGE] = 
        add_attribute(EDGE,"__e_method_list",INTEGER_TYPE,1,NULL,0,NULL);
  /* facet */
  add_attribute(FACET,"__f_constraint_list",INTEGER_TYPE,1,NULL,0,NULL);
  add_attribute(FACET,"__f_vertices",ELEMENTID_TYPE,1,&three,0,NULL);
  add_attribute(FACET,"__facet_normal",REAL_TYPE,1,NULL,0,NULL);
  add_attribute(FACET,"__body_list",ELEMENTID_TYPE,1,NULL,0,NULL);
  add_attribute(FACET,"__next_vfacet_list",ELEMENTID_TYPE,0,NULL,0,NULL);
  add_attribute(FACET,"__next_bfacet_list",ELEMENTID_TYPE,1,NULL,0,NULL);
  web.meth_attr[FACET] = 
        add_attribute(FACET,"__f_method_list",INTEGER_TYPE,1,NULL,0,NULL);
  /* body */
  web.meth_attr[BODY] = 
        add_attribute(BODY,"__b_method_list",INTEGER_TYPE,1,NULL,0,NULL);
 

  F_TAG_ATTR = F_PHASE_ATTR = V_BOUNDARY_ATTR = E_BOUNDARY_ATTR = 
    F_BOUNDARY_ATTR = B_PHASE_ATTR = 0;

#ifdef MPI_EVOLVER
  { int m = MPI_EXPORT_MAX;
    web.mpi_export_attr[VERTEX] = 
        add_attribute(VERTEX,"__mpi_export_v",USHORT_TYPE,1,&m,0,NULL);
    web.mpi_export_attr[EDGE] = 
        add_attribute(EDGE,"__mpi_export_e",USHORT_TYPE,1,&m,0,NULL);
    web.mpi_export_attr[FACET] = 
        add_attribute(FACET,"__mpi_export_f",USHORT_TYPE,1,&m,0,NULL);
    web.mpi_export_attr[BODY] = 
        add_attribute(BODY,"__mpi_export_b",USHORT_TYPE,1,&m,0,NULL);
    web.mpi_export_attr[FACETEDGE] = 
        add_attribute(FACETEDGE,"__mpi_export_fe",USHORT_TYPE,1,&m,0,NULL);
    mpi_export_voffset = EXTRAS(VERTEX)[web.mpi_export_attr[VERTEX]].offset;
    mpi_export_eoffset = EXTRAS(EDGE)[web.mpi_export_attr[EDGE]].offset;
    mpi_export_foffset = EXTRAS(FACET)[web.mpi_export_attr[FACET]].offset;
    mpi_export_boffset = EXTRAS(BODY)[web.mpi_export_attr[BODY]].offset;
    mpi_export_feoffset = EXTRAS(FACETEDGE)[web.mpi_export_attr[FACETEDGE]].offset;
  }
#endif
}

/***********************************************************************
*
* function: free_discards()
*
* purpose: Totally free up discard list.  To be called only when
*          no lists are being used.
*/
void free_discards(mode)
int mode; /* DISCARDS_ALL or DISCARDS_SOME */
{ int type;

  for ( type = 0 ; type < NUMELEMENTS ; type++ )
  { element_id id,next_id;
    int small_potatoes = (mode==DISCARDS_ALL) ? 0 : web.skel[type].count/10; 
    if ( web.skel[type].discard_count <= small_potatoes )
       continue; /* don't bother with small potatoes */
    id = web.skel[type].used;
    while ( valid_id(id) )
    { struct element *ptr = elptr(id);
      next_id = ptr->forechain;
      if ( !(ptr->attr & ALLOCATED) )
      { /* move to free list */
#ifdef MPI_EVOLVER
        element_id loc_id = ptr->local_id;
#else
        element_id loc_id = id;
#endif
        if ( valid_id(ptr->forechain) )
          elptr(ptr->forechain)->backchain = ptr->backchain;
        else web.skel[type].last = ptr->backchain;
        if ( valid_id(ptr->backchain) )
          elptr(ptr->backchain)->forechain = ptr->forechain;
        else web.skel[type].used = ptr->forechain;

#ifdef HASH_ID
        *(struct element **)&(ptr->forechain) = web.skel[type].freehead; 
        web.skel[type].freehead = ptr;
        elhash_delete(id);
        ptr->self_id = NULLID;
        web.skel[type].alloc--;
#else
        if ( valid_id(web.skel[type].free) )
          elptr(web.skel[type].free)->backchain = loc_id;
        else
          web.skel[type].freelast = loc_id;

        ptr->forechain = web.skel[type].free;
        ptr->backchain = NULLID;
        web.skel[type].free = loc_id;
#endif
      }
      id = next_id;
    }
    web.skel[type].discard_count = 0;
  }

}

/**************************************************************************
*
* function: move_to_free_front()
*
* purpose: get particular id element to front of free list so id will 
*             match datafile number.  Called only during datafile.
*/

void move_to_free_front(type,id)
int type; /* element type */
int id;    /* element number    */
{ int ord = id - 1;


#ifdef HASH_ID
 if ( !match_id_flag || addload_flag ) return; /* for old way */
 web.skel[type].free_spot = ord;
#else

  element_id eid;
  struct element *eptr,*fptr,*bptr;
 
  if ( !match_id_flag || addload_flag ) return; /* for old way */

  if ( sparse_ibase_flag )
  { /* extend ibase with empty slots far enough */
    if ( id > web.skel[type].ialloc )
    { int imax = web.skel[type].ialloc > 1024 ? 2*web.skel[type].ialloc : 1024;
      while ( imax < id ) 
        imax *= 2;
      web.skel[type].ibase = (INDIRECT_TYPE*)kb_realloc((char*)web.skel[type].ibase,
                                 imax*sizeof(INDIRECT_TYPE));
      web.skel[type].ialloc = imax;
    }
    if ( !valid_id(web.skel[type].free) )
      extend(type,EXTEND_BATCH); /* refill freelist */ 
    if ( web.skel[type].ibase[ord] == NULL ) 
    { /* move first of freelist to desired spot */
      element_id free_id = web.skel[type].free;
      int free_ord;
      free_ord = ordinal(free_id);
      eptr = web.skel[type].ibase[ord] = web.skel[type].ibase[free_ord];
      web.skel[type].ibase[free_ord] = NULL;
      eptr->self_id = ((element_id)type << TYPESHIFT) | VALIDMASK | ord; 
#ifdef MPI_EVOLVER
      eptr->self_id |= (element_id)this_task << TASK_ID_SHIFT;  
#endif
      web.skel[type].free = eptr->self_id;
      if ( !valid_id(eptr->forechain) )
        web.skel[type].freelast = eptr->self_id;
         
      return; 
    } 
  }
  else
  { /* dense ibase, extend and allocate empty structures */
    while ( id > web.skel[type].maxcount ) 
     extend(type,EXTEND_BATCH);
  }

  eid = ((element_id)type << TYPESHIFT) | VALIDMASK | abs(ord);
  #ifdef MPI_EVOLVER
  eid |= (element_id)this_task << TASK_ID_SHIFT;  
  #endif

  eptr = elptr(eid);
  if ( valid_element(eid) )
  { kb_error(2187,"Internal error: Cannot find element in free list.\n",
       DATAFILE_ERROR);
    return;
  }

  if ( eid == web.skel[type].free ) return; /* already in place */
  if ( eid == web.skel[type].freelast ) 
    web.skel[type].freelast = eptr->backchain;

  /* now cut out of free list */
  if ( valid_id(eptr->backchain) )
  { bptr = elptr(eptr->backchain);
    bptr->forechain = eptr->forechain; /* cut */
  }
  if ( valid_id(eptr->forechain) )
  { fptr = elptr(eptr->forechain);
    fptr->backchain = eptr->backchain;
  }

  /* paste at front of free list */
  if ( valid_id(web.skel[type].free) )
  { fptr = elptr(web.skel[type].free);
    fptr->backchain = eid;
  }
  eptr->forechain = web.skel[type].free; 
  web.skel[type].free = eid;
#endif
}

/*********************************************************************
* 
* Function: reorder_storage()
*
* Purpose: order storage of element structures according to value
* of element extra attribute order_key (real). Meant to order storage
* in cache and swap friendly way.  Invoked by command reorder_storage.
*
* Elements generators work through forechain pointer, so we must
* re-order storage and re-link forechain pointers.
**********************************************************************/

static int key_offset; /* offset of key in element structure */
static char * keynames[NUMELEMENTS] = {"vertex_order_key",
    "edge_order_key","facet_order_key","body_order_key",
    "facetedge_order_key"};
int esort ARGS((char*,char*));

int esort (a,b)  /* sort routine */
char *a, *b;
{ if ( *(REAL*)(a+key_offset) < *(REAL*)(b+key_offset) ) return -1;
  if ( *(REAL*)(a+key_offset) > *(REAL*)(b+key_offset) ) return  1;
  return 0;
}

void reorder_storage()
{ struct element *newblock[NUMELEMENTS];
  int i,j,n;

  free_discards(DISCARDS_ALL);

  /* allocate single-block space for each type  */
  for ( n = 0 ; n < NUMELEMENTS ; n++ )
    newblock[n] = (struct element *)mycalloc(web.skel[n].maxcount+1,
                          web.sizes[n]);
  
  /* copy element structures. */
  for ( n = 0 ; n < NUMELEMENTS ; n++ )
  { element_id id = web.skel[n].used;
    char *spot = (char*)(newblock[n]);
    while ( valid_id(id) )
    { struct element *ep = elptr(id);
      memcpy(spot,(char*)ep,web.sizes[n]);
      id = ep->forechain;
      spot += web.sizes[n];
    }
    #ifndef HASH_ID
    /* free list at end */
    id = web.skel[n].free;
    while ( valid_id(id) )
    { struct element *ep = elptr(id);
      memcpy(spot,(char*)ep,web.sizes[n]);
      id = ep->forechain;
      spot += web.sizes[n];
    }
    #endif
  }

  /* sort elements in use */
  for ( i = 0 ; i < NUMELEMENTS ; i++ )
  { struct extra *ex;
    int k;
     
    if ( web.skel[i].count == 0 ) continue;
    key_offset = -1; /* sentinel value */
    for ( k = 0, ex = EXTRAS(i) ; k < web.skel[i].extra_count ; k++ , ex++ )
      if ( stricmp(keynames[i], ex->name) == 0 )
      { key_offset = ex->offset;
        break;
      }
    if ( key_offset < 0 )
    { for ( n = 0 ; n < NUMELEMENTS ; n++ ) myfree((char*)newblock[n]);
      sprintf(errmsg,"reorder_storage: %s attribute not defined.\n",keynames[i]);
      kb_error(2188,errmsg, RECOVERABLE);
    }
    qsort((char*)newblock[i],web.skel[i].count,web.sizes[i],FCAST esort);
  }

  /* reset ibase array of pointers and list links */
  for ( i = 0 ; i < NUMELEMENTS ; i++ )
  { struct element *ep,*nextep;
    int k;

    if  ( web.skel[i].maxcount == 0 ) continue;
    
    if ( web.skel[i].count )
    {
      web.skel[i].used = newblock[i]->self_id;
      newblock[i]->backchain = NULLID;
      for ( k = 0, ep = newblock[i] ; k < web.skel[i].count-1 ; k++ )
      { nextep = (struct element *)((char*)ep + web.sizes[i]);
        ep->forechain = nextep->self_id;
        nextep->backchain = ep->self_id;
        web.skel[i].ibase[ordinal(ep->self_id)] = ep;
        ep = nextep;
      }
      web.skel[i].ibase[ordinal(ep->self_id)] = ep;
      ep->forechain = NULLID;
      web.skel[i].last = ep->self_id;
    }
    /* and free list */
    for (  k = web.skel[i].count ; k < web.skel[i].maxcount ; k++ )
    { ep = (struct element *)((char*)(newblock[i]) + k*web.sizes[i]);
      web.skel[i].ibase[ordinal(ep->self_id)] = ep;
    }
  }

  /* free old storage */
  for ( i = 0 ; i < NUMELEMENTS ; i++ )
  { for ( j = 0 ; j < blockcount[i] ; j++ )
          myfree((char *)blocklist[i][j].blockptr);
    blockmax[i] = blockcount[i] = 0;
  }

  /* establish new */
  for ( i = 0 ; i < NUMELEMENTS ; i++ )
  { if ( web.skel[i].count == 0 ) continue;
    blocklist[i][0].blockptr = newblock[i];
    blocklist[i][0].count = web.skel[i].maxcount;
    blockmax[i] = blockcount[i] = 1;
  }

  global_timestamp++;
  top_timestamp = global_timestamp;
}

/*************************************************************************
*
* Function: renumber_all()
*
* Purpose: Renumber elements according to linked list order.
*
*/

void renumber_all()
{ int type;
  struct element *ep=NULL;
  element_id id;
  int k;
  int dim,off,dima,offa,dimb,offb,dimc,offc;
  struct element **newibase[NUMELEMENTS];

  free_discards(DISCARDS_ALL);

  for ( type = VERTEX ; type <= FACETEDGE ; type++ )
     newibase[type] = 
         (struct element **)mycalloc(web.skel[type].ialloc,
                  sizeof(element_id *)); 

  for ( type = VERTEX ; type <= FACETEDGE ; type++ )
  { element_id count;

    /* reset self-id of used elements  */
    count = 0;
    id = web.skel[type].used;
    while ( valid_id(id) )
    { ep = elptr(id);
      ep->self_id = (ep->self_id & (~OFFSETMASK)) | count;
      count++;
      id = ep->forechain;
    }

  }
 /* reset mutual links within structures */
  FOR_ALL_FACETEDGES(id)
  { struct facetedge *ep = feptr(id);
    ep->fe_edge_id = copy_sign(elptr(ep->fe_edge_id)->self_id,ep->fe_edge_id);
    ep->fe_facet_id = valid_id(ep->fe_facet_id) ? 
		copy_sign(elptr(ep->fe_facet_id)->self_id,ep->fe_facet_id) : ep->fe_facet_id;
    ep->nextedge[0] = copy_sign(elptr(ep->nextedge[0])->self_id,ep->nextedge[0]);
    ep->nextedge[1] = copy_sign(elptr(ep->nextedge[1])->self_id,ep->nextedge[1]);
    ep->nextfacet[0] = copy_sign(elptr(ep->nextfacet[0])->self_id,ep->nextfacet[0]);
    ep->nextfacet[1] = copy_sign(elptr(ep->nextfacet[1])->self_id,ep->nextfacet[1]);
  }

  FOR_ALL_VERTICES(id)
  { struct vertex *ep = vptr(id);
    ep->e_id = copy_sign(elptr(ep->e_id)->self_id,ep->e_id);
  }

  off = EXTRAS(EDGE)[E_VERTICES_ATTR].offset;  /* endpoints */
  dim = EXTRAS(EDGE)[E_VERTICES_ATTR].array_spec.datacount;  
  FOR_ALL_EDGES(id)    
  { struct edge *ep = eptr(id);
    vertex_id *vp;
    ep->fe_id = copy_sign(elptr(ep->fe_id)->self_id,ep->fe_id);
    ep->next_vedge[0] = copy_sign(elptr(ep->next_vedge[0])->self_id,
      ep->next_vedge[0]);
    ep->next_vedge[1] = copy_sign(elptr(ep->next_vedge[1])->self_id,
      ep->next_vedge[1]);
    vp = (vertex_id*)((char*)ep + off);
    for ( k = 0 ; k < dim ; k++ )
      vp[k] = vptr(vp[k])->self_id;
  }

  off = EXTRAS(FACET)[F_VERTICES_ATTR].offset;  /* endpoints */
  dim = EXTRAS(FACET)[F_VERTICES_ATTR].array_spec.datacount;  
  offa = EXTRAS(FACET)[F_BODY_LIST_ATTR].offset;  /* bodies */
  dima = EXTRAS(FACET)[F_BODY_LIST_ATTR].array_spec.datacount;  
  offb = EXTRAS(FACET)[F_NEXT_VFACET_ATTR].offset;  /* links */
  dimb = EXTRAS(FACET)[F_NEXT_VFACET_ATTR].array_spec.datacount;  
  offc = EXTRAS(FACET)[F_NEXT_BFACET_ATTR].offset;  /* links */
  dimc = EXTRAS(FACET)[F_NEXT_BFACET_ATTR].array_spec.datacount;  
  FOR_ALL_FACETS(id)  
  { struct facet *ep = fptr(id);
    element_id *p;
    ep->fe_id = copy_sign(elptr(ep->fe_id)->self_id,ep->fe_id);
    p = (element_id*)((char*)ep + off);
    for ( k = 0 ; k < dim ; k++ )
      if ( valid_id(p[k]) )
         p[k] = copy_sign(elptr(p[k])->self_id,p[k]);
    p = (element_id*)((char*)ep + offa);
    for ( k = 0 ; k < dima ; k++ )
      if ( valid_id(p[k]) )
         p[k] = copy_sign(elptr(p[k])->self_id,p[k]);
    p = (element_id*)((char*)ep + offb);
    for ( k = 0 ; k < dimb ; k++ )
      if ( valid_id(p[k]) )
         p[k] = copy_sign(elptr(p[k])->self_id,p[k]);
    p = (element_id*)((char*)ep + offc);
    for ( k = 0 ; k < dimc ; k++ )
      if ( valid_id(p[k]) )
         p[k] = copy_sign(elptr(p[k])->self_id,p[k]);
  }

  FOR_ALL_BODIES(id)    
  { struct body * ep = bptr(id);
    ep->f_id = copy_sign(elptr(ep->f_id)->self_id,ep->f_id);
  }

  /* now new pointers in newibase */
  for ( type = VERTEX ; type <= FACETEDGE ; type++ )
  { int count = 0;
    if ( web.skel[type].count == 0 ) continue; 
    FOR_ALL_ELEMENTS(type,id)
      newibase[type][count++] = elptr(id);
  } 


  /* fix up links */
  for ( type = VERTEX ; type <= FACETEDGE ; type++ )
  { if ( web.skel[type].count == 0 ) continue;
    if ( web.skel[type].count == 1 )
    { newibase[type][0]->forechain = NULLID;
      newibase[type][0]->backchain = NULLID;
      continue;
    }
    newibase[type][0]->forechain = newibase[type][0]->self_id+1;
    newibase[type][0]->backchain = NULLID;
    for ( k = 1 ; k < web.skel[type].count-1; k++ )
    {
      newibase[type][k]->forechain = newibase[type][k]->self_id+1;
      newibase[type][k]->backchain = newibase[type][k]->self_id-1;
    }
    newibase[type][k]->forechain = NULLID;
    newibase[type][k]->backchain = newibase[type][k]->self_id - 1;
    web.skel[type].used = newibase[type][0]->self_id;
    web.skel[type].last = newibase[type][k]->self_id;
  }

  #ifndef HASH_ID
  /* fix up freelists */
  for ( type = VERTEX ; type <= FACETEDGE ; type++ )
  { int count;
    element_id backid = NULLID,newid;

    count = web.skel[type].count;
    id = web.skel[type].free;
    if ( !valid_id(id) ) continue;
    newid = (id & (~OFFSETMASK)) | count;
    web.skel[type].free = newid;

    while ( valid_id(id) )
    { ep = elptr(id);
      id = ep->forechain;
      ep->forechain = ++newid;
      ep->backchain = backid;
      backid = id;
      newibase[type][count] = ep;
      ep->self_id = (ep->self_id & (~OFFSETMASK)) | count;
      count++;
    } 
    ep->forechain = NULLID;
    web.skel[type].freelast = backid;
  }
  #endif

  /* miscellaneous elements */
  if ( valid_id(web.zoom_v) ) web.zoom_v = vptr(web.zoom_v)->self_id;
  if ( pickvnum ) pickvnum = loc_ordinal(vibase[pickvnum-1]->self_id)+1;
  if ( pickenum ) pickenum = loc_ordinal(eibase[pickenum-1]->self_id)+1;
  if ( pickfnum ) pickfnum = loc_ordinal(fibase[pickfnum-1]->self_id)+1;

  /* swap ibase to new */
  for ( type = VERTEX ; type <= FACETEDGE ; type++ )
  { myfree((char*)(web.skel[type].ibase));
    web.skel[type].ibase = newibase[type];
  }
  vibase = web.skel[VERTEX].ibase;
  eibase = web.skel[EDGE].ibase;
  fibase = web.skel[FACET].ibase;
  bibase = web.skel[BODY].ibase;
  feibase = web.skel[FACETEDGE].ibase;

  global_timestamp++;
  top_timestamp = global_timestamp;
  
}

/*************************************************************************
*
* function: interp_edge_attribute()
*
* purpose:  Find interpolated value of a vertex attribute at a Gauss point
*           on an edge.
*/

REAL interp_edge_attribute(eid,ext,inx,ptnum)
edge_id eid;
struct extra *ext;  /* extra attribute involved */
int inx;  /* index within attribute */
int ptnum;  /* which gauss point */
{
  int ctrl = web.skel[EDGE].ctrlpts;
  REAL sum = 0.0;
  int i;
  vertex_id *v = get_edge_vertices(eid);
  struct gauss_lag *gl = &gauss_lagrange[EDGE][web.gauss1D_order];

  if ( web.modeltype == QUADRATIC )
  { sum += gl->gpoly[ptnum][0]*get_extra_attrib_value(v[1],ext,inx);
    sum += gl->gpoly[ptnum][1]*get_extra_attrib_value(v[2],ext,inx);
    sum += gl->gpoly[ptnum][2]*get_extra_attrib_value(v[0],ext,inx);
  } else
  for ( i = 0 ; i < ctrl ; i++ )
    sum += gl->gpoly[ptnum][i]*get_extra_attrib_value(v[i],ext,inx);
  return sum;
}


/*************************************************************************
*
* function: interp_facet_attribute()
*
* purpose:  Find interpolated value of a vertex attribute at a Gauss point
*           on a facet.
*/

REAL interp_facet_attribute(fid,ext,inx,ptnum)
facet_id fid;
struct extra *ext;  /* extra attribute involved */
int inx;  /* index within attribute */
int ptnum;  /* which gauss point */
{
  int ctrl = web.skel[FACET].ctrlpts;
  REAL sum = 0.0;
  int i;
  vertex_id *v;
  vertex_id vv[2*FACET_VERTS];
  struct gauss_lag *gl = &gauss_lagrange[FACET][web.gauss2D_order];

  if ( web.modeltype == LINEAR )
  { facetedge_id fe = get_facet_fe(fid);
    for ( i = 0 ; i < FACET_VERTS ; i++ )
    { vv[i] = get_fe_tailv(fe);
      fe = get_next_edge(fe);
    }
    v = vv;
  } else if ( web.modeltype == QUADRATIC ) 
  { facetedge_id fe = get_facet_fe(fid);
    vv[0] = get_fe_tailv(fe);
    vv[1] = get_fe_midv(fe);
    vv[2] = get_fe_headv(fe);
    fe = get_next_edge(fe);
    vv[4] = get_fe_midv(fe);
    vv[5] = get_fe_headv(fe);
    fe = get_next_edge(fe);
    vv[3] = get_fe_midv(fe);
    v = vv;
  } else /* LAGRANGE */
  v = get_facet_vertices(fid);

  for ( i = 0 ; i < ctrl ; i++ )
    sum += gl->gpoly[ptnum][i]*get_extra_attrib_value(v[i],ext,inx);
  return sum;
}

/*************************************************************************
*
* function: get_extra_attrib_value()
*
* purpose: Return an extra attribute value as a real.
*
*/

REAL get_extra_attrib_value(id,ext,inx)
element_id id;
struct extra *ext;
int inx;
{
  if ( inx >= ext->array_spec.datacount )
  { sprintf(errmsg,"Attribute %s total index is %d; maximum is %d.\n",
       ext->name,inx+1,ext->array_spec.datacount);
    kb_error(1151,errmsg,RECOVERABLE);
  }
  if ( inx < 0 )
  { sprintf(errmsg,"Attribute %s index zero or negative: %d.\n",
    ext->name,inx+1);
  kb_error(2523,errmsg,RECOVERABLE);
  }

  switch ( ext->type )
  { case REAL_TYPE: return ((REAL*)get_extra_ptr(id,ext))[inx];
    case INTEGER_TYPE: return (REAL)(((int*)get_extra_ptr(id,ext))[inx]);
    case UINT_TYPE: return (REAL)(((unsigned int*)get_extra_ptr(id,ext))[inx]);
    case UCHAR_TYPE: 
      return (REAL)(((unsigned char*)get_extra_ptr(id,ext))[inx]);
    case CHAR_TYPE: 
      return (REAL)(((char*)get_extra_ptr(id,ext))[inx]);
    case USHORT_TYPE: 
      return (REAL)(((unsigned short int*)get_extra_ptr(id,ext))[inx]);
    case SHORT_TYPE: 
      return (REAL)(((short int*)get_extra_ptr(id,ext))[inx]);
    case ULONG_TYPE: 
      return (REAL)(((unsigned long int*)get_extra_ptr(id,ext))[inx]);
    case LONG_TYPE: 
      return (REAL)(((long int*)get_extra_ptr(id,ext))[inx]);
    case PTR_TYPE: 
      return (REAL)(unsigned long int)(((char**)get_extra_ptr(id,ext))[inx]);
    default: kb_error(3101,
      "get_extra_attribute_value() trying to get nonumeric type.\n",
           RECOVERABLE);
     break;
  }
  return 0.0;  /* shouldn't get here. */
}

/***********************************************************************/
/***********************************************************************

 Routines for locating element structures through hash of element ID.
 Meant for handling sparse distributions of IDs, for example in large
 parallel distributed surfaces.  ID is hashed on task, element type,
 and ordinal; not on valid bit or orientation.  Unified list for all
 elements.
*************************************************************************/

/************************************************************************
*
* Function: int32hash()
*
* Purpose: hash 32 bits to 32 bits
*/

unsigned int int32hash(key)
unsigned int key;
{
  key += ~(key << 15);
  key ^=  (key >> 10);
  key +=  (key << 3);
  key ^=  (key >> 6);
  key += ~(key << 11);
  key ^=  (key >> 16);
  return key;
}

/************************************************************************
*
* Function: int64hash()
*
* Purpose: hash 64 bits to 64 bits
* Integer hashing functions from Thomas Wang,
* http://www.concentric.net/~Ttwang/tech/inthash.htm
*/

#ifdef NOLONGLONG
unsigned long int64hash(key)
unsigned long key;
#else
unsigned long long int64hash(key)
unsigned long long key;
#endif
{
  key += ~(key << 32);
  key ^= (key >> 22);
  key += ~(key << 13);
  key ^= (key >> 8);
  key += (key << 3);
  key ^= (key >> 15);
  key += ~(key << 27);
  key ^= (key >> 31);
  return key;
}


/*************************************************************************
*
* Function: elhash_lookup()
*
* Purpose: Retrieve element structure pointer from hash table
*
*/

struct element * elhash_lookup(id)
element_id id;
{ element_id key;
  int inx;

  key = id_hash(id);
  inx = ((int)key) & web.elhashmask;
  for (;;)
  { if ( web.elhashtable[inx] == NULL )
      return NULL;
    if ( web.elhashtable[inx] != ELHASH_FREE )
      if ( equal_element(web.elhashtable[inx]->self_id,id) )
        return web.elhashtable[inx];
    inx++;
    inx &= web.elhashmask; /* wraparound, maybe */
  }

}

/*************************************************************************
*
* Function: elhash_bigger()
*
* Purpose: Double size of element hash table.
*
*/

void elhash_bigger()
{ int oldsize = web.elhashsize;
  struct element ** oldtable;
  int n;

  if ( web.elhashsize == 0 )
  { web.elhashsize = 4096;
    web.elhashmask = 0xFFF;
  }
  else
  {
    web.elhashmask |= web.elhashsize; /* depending on power of 2 */
    web.elhashsize *= 2;
  }

  oldtable = web.elhashtable;
  web.elhashtable = (struct element **)mycalloc(web.elhashsize,sizeof(struct element*));

  /* rehash */
  web.elhashcount = 0;
  for ( n = 0 ; n < oldsize ; n++ )
    if ( oldtable[n] && (oldtable[n] != ELHASH_FREE) )
      elhash_insert(oldtable[n]->self_id,oldtable[n]);
}


/***********************************************************************
*
*  function: elhash_extend()
*
*  purpose: allocate more empty element structures using pointer freelist
*/

void elhash_extend(type,mode)
int type;
int mode; /* EXTEND_BATCH or EXTEND_FOR_REFINE */
{
  int number=0;  /* of new items to allocate */
  char *newblock;
  INDIRECT_TYPE *newiblock;
  struct element *newptr = NULL, *oldptr;
  int k;
  int allocsize;
  long  oldnum = web.skel[type].maxcount;
  long  newnum=0;

  ENTER_GRAPH_MUTEX;
  /* Don't mess with structures while graph thread using them */

  if ( blockcount[type] >= blockmax[type] )
  { blocklist[type] = (struct blocklist_struct*)
      kb_realloc((char*)(blocklist[type]),
         (blockcount[type]+BATCHSIZE)*sizeof(struct blocklist_struct));
     blockmax[type] += BATCHSIZE;
  }
  if ( mode == EXTEND_BATCH )
  {
    /* calculate number of structures to fit in block size just under 2^n */
    allocsize = BATCHSIZE*web.sizes[type];
    k = 0x100 ; while ( k < allocsize ) k <<= 1 ;
    number = (k-16)/web.sizes[type]; /* maybe room for block header */
    newnum = web.skel[type].maxcount + number;
  }
  else if ( mode == EXTEND_FOR_REFINE )
  { /* increase by 2^surface_dimension factor */
    number = web.skel[type].count*(1<<web.dimension)
                - web.skel[type].maxcount + 100;
    newnum = web.skel[type].maxcount + number;
    if ( number <= 0 ) { goto elhash_extend_exit; }/* don't need any more */
  }
  else kb_error(2476,"Internal error: bad mode for elhash_extend().\n",
    RECOVERABLE);
  
  if ( (web.skel[type].maxcount + number) > OFFSETMASK )
  { sprintf(errmsg, "Trying to allocate more %s than ID format allows, %Ld\n",
          typenames[type],OFFSETMASK);
    kb_error(3214,errmsg,RECOVERABLE);
  }

  newblock = mycalloc(number,web.sizes[type]);
  blocklist[type][blockcount[type]].start_ord = oldnum ;
  blocklist[type][blockcount[type]].count = number ;
  blocklist[type][blockcount[type]++].blockptr = (struct element *)newblock;

  while ( newnum > web.skel[type].ialloc )
  {
    if ( web.skel[type].ibase == NULL )
    { newiblock = (INDIRECT_TYPE*)mycalloc(number,sizeof(INDIRECT_TYPE));
      web.skel[type].ialloc = number;
    }
    else
    { newiblock = (INDIRECT_TYPE*)kb_realloc((char*)(web.skel[type].ibase),
        2*web.skel[type].ialloc*sizeof(INDIRECT_TYPE));
      web.skel[type].ialloc *= 2;
    }
    web.skel[type].ibase = newiblock;
    switch(type)
    {    case VERTEX: vibase = newiblock; break;
         case EDGE:    eibase = newiblock; break;
         case FACET:  fibase = newiblock; break;
         case BODY:    bibase = newiblock; break;
         case FACETEDGE: feibase = newiblock; break;
    }
  }

  /* add to freelist */
  oldptr = (struct element *)newblock;
  for ( k = 0 ; k < number ; k++ )
  { newptr = (struct element *)(newblock + k*web.sizes[type]); 
    *(struct element**)&(oldptr->forechain) = newptr;
    oldptr = newptr;
  }
  *(struct element**)&(oldptr->forechain) = web.skel[type].freehead;
  web.skel[type].freehead = (struct element *)newblock;
  
elhash_extend_exit:
  LEAVE_GRAPH_MUTEX;
  

}

/*************************************************************************
*
* Function: elhash_new_element()
*
* Purpose: Allocate new element from freelist.  If desired_id is non-null,
*          it will allocate that id.
*/

struct element *elhash_new_element(type,desired_id)
int type; /* of element */
element_id desired_id;
{ struct element *newptr,*oldptr;
  element_id newid;
  int spot;

  if ( web.skel[type].freehead == NULL )
    /* allocate another batch */
    elhash_extend(type,EXTEND_BATCH);
  newptr = web.skel[type].freehead;
  web.skel[type].freehead = *(struct element**)&(newptr->forechain);

  if ( web.skel[type].maxcount == 0 )
    web.skel[type].maxcount = 1024;
  if ( web.skel[type].alloc > 0.8*web.skel[type].maxcount )
    web.skel[type].maxcount *= 2;
    
  /* find next available element number, using persistant scan */
  if ( desired_id ) 
  { spot = (int)(desired_id & OFFSETMASK);
    newid = ((element_id)type << TYPESHIFT) | VALIDMASK | spot;
    oldptr = elhash_lookup(newid); 
    if ( oldptr )
      kb_error(5433,"Duplicate element number.\n",RECOVERABLE);
    elhash_insert(newid,newptr);
  }
  else 
  {  spot = web.skel[type].free_spot;
    do 
    {
      newid = ((element_id)type << TYPESHIFT) | VALIDMASK | spot;
      oldptr = elhash_lookup(newid); 
      spot++;
      if ( spot >= web.skel[type].maxcount )
        spot = 0; 
    } while (oldptr != NULL);
    elhash_insert(newid,newptr);
    web.skel[type].free_spot = spot;
  }
  newptr->self_id = newid;
  web.skel[type].alloc++;
 
  return newptr;
} 

/*************************************************************************
*
* Function: elhash_insert()
*
* Purpose: Add one id to element hash table.  Not an error if already
*          there.
*/

void elhash_insert(id,eptr)
element_id id;
struct element * eptr;
{ element_id key;
  int inx;
  
  if ( web.elhashcount >= web.elhashsize/2 )
    elhash_bigger();

  key = id_hash(id);
  inx = ((int)key) & web.elhashmask;
  for (;;)
  { if ( (web.elhashtable[inx] == NULL)
      || (web.elhashtable[inx] == ELHASH_FREE) )
    { web.elhashtable[inx] = eptr;
      web.elhashcount++;
      return;
    }
    if ( equal_element(web.elhashtable[inx]->self_id,id))
    { web.elhashtable[inx] = eptr;  /* maybe change due to expand() */
      return;
    }
    inx++;
    inx &= web.elhashmask; /* wraparound, maybe */
  }
   
}
/*************************************************************************
*
* Function: elhash_insert()
*
* Purpose: Replace pointer for one id to element hash table.  
*          NOTE: Old pointer must still be valid!
*/

void elhash_replace(id,eptr)
element_id id;
struct element * eptr;
{ element_id key;
  int inx;
  
  key = id_hash(id);
  inx = ((int)key) & web.elhashmask;
  for (;;)
  { if ( web.elhashtable[inx] == NULL)
      kb_error(4382,"INTERNAL ERROR - elhash_replace(): entry not found.\n", RECOVERABLE);
    if ( web.elhashtable[inx] != ELHASH_FREE )
      if ( equal_element(web.elhashtable[inx]->self_id,id))
      { web.elhashtable[inx] = eptr;  /* maybe change due to expand() */
        return;
      }
    inx++;
    inx &= web.elhashmask; /* wraparound, maybe */
  }
   
}

/*************************************************************************
*
* Function: elhash_delete()
*
* Purpose: Delete an entry from the element hash table.
*/

void elhash_delete(id)
element_id id;
{ element_id key;
  int inx;

  /* First, find it */
  key = id_hash(id);
  inx = ((int)key) & web.elhashmask;
  for (;;)
  { if ( web.elhashtable[inx] == NULL )
      return; /* wasn't there */
    if ( (web.elhashtable[inx] != ELHASH_FREE) &&
            (equal_element(web.elhashtable[inx]->self_id,id)) )
      break;
    inx++;
    inx &= web.elhashmask; /* wraparound, maybe */
  }

  /* mark as freed, not empty, so searches will continue past */
  web.elhashtable[inx] = ELHASH_FREE;

  web.elhashcount--;
}

/* end element id hashing ********************************************/

#ifdef MPI_EVOLVER
/*************************************************************************
*
* Function: mpi_remote_present()
*
* purpose: test if a particular remote element id exists locally.
*/

int mpi_remote_present(id)
element_id id;
{ struct element *eptr = mpi_remote_elptr(id);
  if ( eptr == NULL ) 
    return 0;
  if ( eptr->attr & ALLOCATED ) 
    return 1;
  return 0; 
}
#endif

/************************************************************************
*
* function: valid_element()
*
* purpose: To see if the element referred to by an element id is
*          actually present and valid.
*/

int valid_element(id)
element_id id;
{ struct element *ep;
  if ( !valid_id(id) ) return 0;
  ep = elptr(id);
  if ( ep == NULL ) return 0;
  if ( ep->attr & ALLOCATED ) return 1;
  return 0;
}


/************************************************************************
*
* function: binary_tree_add()
*
* purpose: implements binary tree addition for accurate sum of large
*          number of addends.
*/

void binary_tree_add(addends,term)
REAL *addends;
REAL term;
{ int i;
  for ( i = 0 ; addends[i] != 0.0 ; i++ )
  { term += addends[i];
    addends[i] = 0.0;
  }
  addends[i] = term;
}

