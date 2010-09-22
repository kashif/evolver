/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/**********************************************************************
*
*  File: symtable.c
*
*  Purpose: Handles symbol table functions for queries.
*/

#include "include.h"
#include "ytab.h"

/* symbol table variables */
#define SYMMAX 200
struct sym symtable[SYMMAX];    /* symbol table */
#define MAXSYMDEPTH 10
int symsave[MAXSYMDEPTH];  /* start of scope pointers */
int symdepth;  /* depth of symbol table */
int symtop;  /* current stack top, index of first empty spot */

/**********************************************************************
*
*  function: begin_scope()
*
*  purpose:  enter scope, save symbol stack position
*/

int begin_scope()
{
  if ( symdepth >= MAXSYMDEPTH - 1 )
     kb_error(1666,"Scopes nested too deep.\n",RECOVERABLE);

  symsave[symdepth++] = symtop;
  return symdepth;
}


/**********************************************************************
*
*  function: end_scope()
*
*  purpose:  leave scope, pop symbol stack 
*/

void end_scope()
{ int oldsymtop = symtop;
  if ( symdepth >= 1 ) /* clear stack */
      symtop = symsave[--symdepth];
  if ( oldsymtop > symtop )
     memset((char*)(symtable+symtop),0,(oldsymtop-symtop)*sizeof(struct sym));
}

/**********************************************************************
*
*  function: set_scope()
*
*  purpose:  chop symbol stack after error
*/

void set_scope(depth)
int depth;
{ int oldsymtop = symtop;
  symdepth = depth-1;
  if ( symdepth > 0 )
    symtop = symsave[symdepth-1];
  else symtop = 0;
  if ( oldsymtop > symtop )
     memset((char*)(symtable+symtop),0,(oldsymtop-symtop)*sizeof(struct sym));
}



/**********************************************************************
*
*  function: clear_symtable()
*
*  purpose:  resets symbol table after command error
*/

void clear_symtable()
{ 
  
  symdepth = 0;
  symtop = 0;
  memset((char*)symtable,0,SYMMAX*sizeof(struct sym));
}

/**********************************************************************
*
*  function: symbol_add()
*
*  purpose: add symbol to symbol table
*
*/
#ifdef __WIN32__
struct sym * symbol_add(char * name,int type)
#else
struct sym * symbol_add(name,type)
char *name;
int type;
#endif

{ int i;

  /* check for duplication in scope */
  for ( i = symsave[symdepth-1] ; i < symtop ; i++ )
     if ( strncmp(symtable[i].name,name,SYMNAMESIZE) == 0 )
     { sprintf(errmsg,"Duplicate name '%s' in scope.\n",name);
        kb_error(1667,errmsg,RECOVERABLE);
     }
     
  if ( symtop >= SYMMAX )
     kb_error(1668,"Internal error: Symbol table overflow.\n",RECOVERABLE);

  strncpy(symtable[symtop].name,name,SYMNAMESIZE);
  symtable[symtop].type = type;
  return symtable + symtop++;
}

/**********************************************************************
*
*  function: symbol_lookup()
*
*  purpose: find symbol in symbol table.  Returns pointer to
*              symbol structure if found, NULL if not.
*/

#ifdef __WIN32__
struct sym *symbol_lookup(char *name)
#else
struct sym *symbol_lookup(name)
char *name;
#endif
{
  int i;

  for ( i = symtop-1 ; i >= 0 ; i-- )
     if ( strncmp(symtable[i].name,name,SYMNAMESIZE) == 0 )
        return symtable + i;  /* success */

  return NULL; /* failure */
}

/**********************************************************************
  Global variable runtime table.  Implemented as a hash table. 
  Hash table itself only has indices into true global table, so 
  parsed expressions can have permanent indices into global table, 
  but hash table can expand. Hash table is twice the size of global
  table to keep sparsity and simplicity.   Hash entry -1 denotes unused.
  For distributed processing convenience, hash table and global table
  kept in dymem, so dy_globals and dy_globalshash are actually offsets
  in dymem, and globals and globalshash are macros for pointers to
  start of the tables.

  Table hashes global variable names, quantity names, and method
  instance names.  Hash table entry is integer, with high two bits
  denoting type of name, and lower bits being index into appropriate
  table.

**********************************************************************/

int global_hash ARGS((char*));
void expand_global_hash ARGS((void));

/**********************************************************************
*
* function: global_hash()
*
* purpose: Hash function for global variable names.  Case insensitive.
*/

#define HASHSTRIDE (2*3*5*7*11*13*17*19 + 1)

int global_hash(name)
char *name;
{ int hashval = 0;  
  size_t len = strlen(name);
  size_t i;
 
  for ( i = 0; i < len ; i++ )
    hashval = (hashval + tolower(name[i])) * HASHSTRIDE;
  return abs(hashval);
}

/**********************************************************************
*
* function: expand_global_hash()
*
* purpose: double size of global name hash table, rehashing 
*          existing names.
*/

void expand_global_hash()
{ int n;

  if ( dy_globalshash ) dy_free(dy_globalshash);

  if ( dy_global_hash_max == 0 ) /* start up */
    dy_global_hash_max = (web.perm_global_count < 50) ? 200 : 4*web.perm_global_count;
  else dy_global_hash_max *= 2;
  dy_globalshash = dy_calloc(dy_global_hash_max,sizeof(int));
  dy_global_hash_maxfill = dy_global_hash_max/2;
  dy_global_hash_used = 0;

  /* rehash */
  for ( n = 0 ; n < web.perm_global_count ; n++ )
  { int spot = global_hash(perm_globals(n)->name) % dy_global_hash_max;
    while ( globalshash[spot] ) 
    { spot++; if ( spot == dy_global_hash_max ) spot = 0; }
    globalshash[spot] = PERM_NAME | n;
    dy_global_hash_used++;
  }
  for ( n = 0 ; n < web.global_count ; n++ )
  { int spot = global_hash(globals(n)->name) % dy_global_hash_max;
    while ( globalshash[spot] ) 
    { spot++; if ( spot == dy_global_hash_max ) spot = 0; }
    globalshash[spot] = VARIABLENAME | n;
    dy_global_hash_used++;
  }
  for ( n = 0 ; n < gen_quant_count ; n++ )
  { int spot = global_hash(GEN_QUANT(n)->name) % dy_global_hash_max;
    while ( globalshash[spot] ) 
    { spot++; if ( spot == dy_global_hash_max ) spot = 0; }
    globalshash[spot] = QUANTITYNAME | n;
    dy_global_hash_used++;
  }
  for ( n = LOW_INST ; n < meth_inst_count ; n++ )
  { int spot = global_hash(METH_INSTANCE(n)->name) % dy_global_hash_max;
    while ( globalshash[spot] ) 
    { spot++; if ( spot == dy_global_hash_max ) spot = 0; }
    globalshash[spot] = METHODNAME | n;
      dy_global_hash_used++;
  }
}

/**********************************************************************
*
*  function: lookup_global_hash()
*
*  purpose:  add new name to global name hash list
*
*  return:  0 if not already there.
*           name type and index if already there (see defines above)
*          
*/

int lookup_global_hash(name,inx,type,addflag)
char *name;
int inx;   /* index in appropriate table */
int type;  /* type of table */
int addflag; /* whether to add if not found, or delete */
{ 
  int hashval;
  int spot;  /* in hash table */
  int entry;   /* index in global table */
  int *htable;  /* globalshash too buried in macros for debug */
  
  if ( dy_global_hash_used >= dy_global_hash_maxfill )
      expand_global_hash();
  htable = globalshash;

  hashval = global_hash(name);
  spot = hashval % dy_global_hash_max;
  for (;;)
  { entry = htable[spot];
    switch   ( entry & NAMETYPEMASK )
    { 
      case VARIABLENAME:
        if ( stricmp(name,globals((entry & INDEXMASK)|EPHGLOBAL)->name) == 0 )
        { if ( addflag == HASH_DELETE )
          { htable[spot] = 0; dy_global_hash_used--;} 
          return entry;
        }
        break;
      
      case PERM_NAME:
        if ( stricmp(name,perm_globals((entry & INDEXMASK)|PERMGLOBAL)->name) == 0 )
        { if ( addflag == HASH_DELETE )
          { htable[spot] = 0; dy_global_hash_used--;} 
          return entry;
        }
        break;

      case QUANTITYNAME:
        if ( stricmp(name,GEN_QUANT(entry & INDEXMASK)->name) == 0 )
        { if ( addflag == HASH_DELETE )
          { htable[spot] = 0; dy_global_hash_used--;} 
          return entry;
        }
        break;

      case METHODNAME:
        if ( stricmp(name,METH_INSTANCE(entry & INDEXMASK)->name) == 0 )
        { if ( addflag == HASH_DELETE ) 
          { htable[spot] = 0; dy_global_hash_used--;} 
          return entry;
        }
        break;

      default:  
        if ( addflag == HASH_ADD )
        { htable[spot] = type | inx;
          dy_global_hash_used++;
        }
        return 0;
          
    }
    /* have to keep searching */
    spot++;
    if ( spot == dy_global_hash_max ) spot = 0;
  } 
  
  /* can't get here */  
}

/**********************************************************************
*
*  function: add_global()
*
*  purpose:  add new global variable
*
*  return:    index number in global list
*/

int add_global(name)
char *name;
{ 
  int slot;
  int inx;   /* index in global table */
  struct global *g;
  ident_t iid;

  if ( web.maxglobals == 0 ) /* very first initialization */
  { web.maxglobals = 100;
    dy_globals = dy_calloc(web.maxglobals,sizeof(struct global));
    Globals = globals(0);  /* handy for debugging */
  }

  slot = web.global_count;
  iid = slot | EPHGLOBAL;
  g = globals(iid);
  memset((char*)g,0,sizeof(struct global));
  strncpy(g->name,name,GLOBAL_NAME_SIZE);
  g->flags |= GLOB_USED;
  
  if ( strlen(name) > GLOBAL_NAME_SIZE )
  { name[GLOBAL_NAME_SIZE] = 0;
    sprintf(errmsg,"Name too long. Truncated to %s.\n",name);
    kb_error(1669,errmsg,WARNING);
  }

  /* see if already existed */
  inx = lookup_global_hash(name,slot,VARIABLENAME,HASH_ADD);
  if ( inx ) /* already there */
  { sprintf(errmsg,"The name \"%s\" is already in use%s.\n",name,
      (inx&NAMETYPEMASK)==QUANTITYNAME ? " as a quantity name" :  
      (inx&NAMETYPEMASK)==PERM_NAME ? " as a permanent name" :  
      (inx&NAMETYPEMASK)==METHODNAME ? " as a method instance name" : "");
    kb_error(2554,errmsg,RECOVERABLE);
  }
  else web.global_count++;

  /* see if we want to expand */
  if ( web.global_count == web.maxglobals )
  { 
     web.maxglobals *= 2;
     dy_globals = dy_realloc(dy_globals,web.maxglobals,sizeof(struct global));
     Globals = globals(0);  /* handy for debugging */
  }

  return iid; 
}


/**********************************************************************
*
*  function: lookup_global()
*
*  purpose:  searches for global variable
*
*  return:    index number in global list, -1 if not found
*/

int lookup_global(name)
char *name;
{ 
  int inx;

  if ( web.global_count == 0 ) return -1;

  /* search for name */
  inx = lookup_global_hash(name,0,VARIABLENAME,HASH_LOOK);
  if ( inx == 0 ) 
    return -1;
  if ( (inx & NAMETYPEMASK) == VARIABLENAME )
    return (inx & INDEXMASK) | EPHGLOBAL;
  return -1;
}

/**************************************************************************
*
* function: rewind_globals()
*
* purpose: Delete globals added during failed parse.
*
*/

void rewind_globals(rewind_spot)
int rewind_spot; /* where to rewind to */
{
  int k;
  /* unhash in reverse order of adding */
  for ( k = web.global_count-1 ; k >= rewind_spot ; k-- )
    lookup_global_hash(globals(k)->name,0,0,HASH_DELETE);
  web.global_count = rewind_spot;
}

/*****************************************************************
*
* function: clear_globals()
*
* purpose: Reset global name hash table to empty.
*/

void clear_globals()
{
  dy_globalshash = 0;
  dy_global_hash_max = 0;
  dy_global_hash_maxfill = 0;
  dy_global_hash_used = 0;
}

/**********************************************************************

  Corresponding functions for permanent globals.

**********************************************************************/

/**********************************************************************
*
*  function: add_perm_global()
*
*  purpose:  add new perm_global variable
*
*  return:    index number in perm_global list
*                -index - 1 if already there
*/

int add_perm_global(name)
char *name;
{ 
  int slot;
  int inx;   /* index in global table */

  if ( web.max_perm_globals == 0 ) /* very first initialization */
  { web.max_perm_globals = 100;
    dy_perm_globals = calloc(web.max_perm_globals,sizeof(struct global));
    perm_Globals = perm_globals(0);  /* handy for debugging */
  }

  slot = web.perm_global_count;
  strncpy(perm_globals(slot)->name,name,GLOBAL_NAME_SIZE);
  perm_globals(slot)->flags |= GLOB_USED;
  
  if ( strlen(name) > GLOBAL_NAME_SIZE )
  { name[GLOBAL_NAME_SIZE] = 0;
    sprintf(errmsg,"Name too long. Truncated to %s.\n",name);
    kb_error(2584,errmsg,WARNING);
  }

  /* see if already existed */
  inx = lookup_global_hash(name,slot,PERM_NAME,HASH_ADD);
  if ( inx ) /* already there */
  { sprintf(errmsg,"The name \"%s\" is already in use%s.\n",name,
      (inx&NAMETYPEMASK)==QUANTITYNAME ? " as a quantity name" :  
      (inx&NAMETYPEMASK)==VARIABLENAME ? " as a variable name" :  
      (inx&NAMETYPEMASK)==METHODNAME ? " as a method instance name" : "");
    kb_error(2576,errmsg,RECOVERABLE);
  }
  else web.perm_global_count++;

  /* see if we want to expand */
  if ( web.perm_global_count == web.max_perm_globals )
  { 
     web.max_perm_globals *= 2;
     dy_perm_globals = 
        realloc(dy_perm_globals,web.max_perm_globals*sizeof(struct global));
     perm_Globals = perm_globals(0);  /* handy for debugging */
  }

  return slot | PERMGLOBAL;
}


/**********************************************************************
*
*  function: lookup_perm_global()
*
*  purpose:  searches for global variable
*
*  return:    index number in global list, -1 if not found
*/

int lookup_perm_global(name)
char *name;
{ 
  int inx;

  if ( web.perm_global_count == 0 ) return -1;

  /* search for name */
  inx = lookup_global_hash(name,0,PERM_NAME,HASH_LOOK);
  if ( inx == 0 ) 
    return -1;
  if ( (inx & NAMETYPEMASK) == PERM_NAME )
    return (inx & INDEXMASK) | PERMGLOBAL;
  return -1;
}

/**************************************************************************
*
* function: rewind_perm_globals()
*
* purpose: Delete permanent globals added during failed parse.
*
*/

void perm_rewind_globals(perm_rewind_spot)
int perm_rewind_spot; /* where to rewind to */
{
  int k;
  /* unhash in reverse order of adding */
  for ( k = web.perm_global_count-1 ; k >= perm_rewind_spot ; k-- )
    lookup_global_hash(perm_globals(k)->name,0,0,HASH_DELETE);
  web.perm_global_count = perm_rewind_spot;
}

/**************************************************************************
   Local variable functions.
   Local variables are stored in procedure-specific name lists, so
     re-defining procedure doesn't lead to memory leak.
   Each list is tree-linked for block scoping.
**************************************************************************/

struct locallist_t *local_scope_bases[LOCALSCOPEMAX]; /* for reference during parsing */
int local_nest_depth;
struct locallist_t *localbase;

/**************************************************************************
*
* function: init_local_scope()
*
* purpose: Set up local scope structure for new command definition.
*/

void init_local_scope(iid)
ident_t iid;  /* identifier of procedure to begin new scope */
{
  struct locallist_t *locals;
    
  locals = (iid & PERMGLOBAL ) ?
       (struct locallist_t *)calloc(1,sizeof(struct locallist_t)) :
       (struct locallist_t *)mycalloc(1,sizeof(struct locallist_t)) ;
  if ( iid & PERMGLOBAL ) 
      locals->flags |= LL_PERMANENT;

  if ( local_nest_depth < 0 ) 
     local_nest_depth = 0;
  local_scope_bases[local_nest_depth] = locals;
  local_nest_depth++;
  
  locals->iid = iid; 
  locals->maxlist = 8;
  locals->list = (locals->flags & LL_PERMANENT ) ?
      (struct localvar_t*)calloc(locals->maxlist,
                                 sizeof(struct localvar_t)) :
      (struct localvar_t*)mycalloc(locals->maxlist,
                                 sizeof(struct localvar_t)) ;
  localbase = locals;
  locals->list[0].prev = -1;

}

/**************************************************************************
*
* function: exit_local_scope()
*
* purpose: finish procedure list local definitions.
* return: sets global variable localbase to the completed 
*                  locals structure, for parser to attach
*                  to code block.
*/

void exit_local_scope()
{
  struct locallist_t *locals;
  
  local_nest_depth--;

  locals = local_scope_bases[local_nest_depth];
  if ( locals && !(locals->flags & LL_IN_USE) )
  { myfree((char*)(locals->list));
    myfree((char*)locals);
  }
  local_scope_bases[local_nest_depth] = NULL;
  if ( local_nest_depth > 0 )
    localbase = local_scope_bases[local_nest_depth-1];
  else localbase = NULL;
}

/**************************************************************************
*
* function: begin_local_scope()
*
* purpose: start block scope
*/

void begin_local_scope()
{ struct locallist_t *locals;
 
  if ( local_nest_depth == 0 )
  { /* coming in from unnamed command */
    /*init_local_scope(0); */
    kb_error(3501,"INTERNAL ERROR: local_scope not initialized.\n",
       RECOVERABLE);
  }
    
  locals = local_scope_bases[local_nest_depth-1]; 
  locals->list[locals->count].scope_depth++;
  locals->scope_depth++;
}

/**************************************************************************
*
* function: end_local_scope()
*
* purpose: end block scope
*/

void end_local_scope()
{ struct locallist_t *locals;
  int n;
  
  locals = local_scope_bases[local_nest_depth-1]; 
  if ( !locals ) return;
  
  if ( locals->list )
  {  
    n = locals->list[locals->count].prev;
  
    while ( (n >= 0) && (locals->list[n].scope_depth == locals->scope_depth) )
      n = locals->list[n].prev;

    locals->list[locals->count].prev = n;
    locals->list[locals->count].scope_depth--;
  }
  locals->scope_depth--;
}

/**************************************************************************
*
* function: add_local_var()
*
* purpose: add a local variable to procedure local variable list
*/

ident_t add_local_var(name,size)
char *name; /* NULL if just reserving space for anonymous value */
int size;   /* in stack entries */
{ int n;
  struct localvar_t *v;
  int depth;
  struct locallist_t *locals;
  int retval;
  
  locals = local_scope_bases[local_nest_depth-1]; 
   
  depth = locals->scope_depth;
 
  v = locals->list + locals->count;

  if ( name )
  { /* search for duplicate in current scope */
    for ( n = v->prev ; (n >= 0) && (locals->list[n].scope_depth == depth) ; 
          n = locals->list[n].prev )
      if ( strcmp(name,locals->list[n].g.name) == 0 )
      { sprintf(errmsg,"Local variable %s already declared in current block.\n",
           name);
        kb_error(2613,errmsg,COMMAND_ERROR);
      }
 
    /* add to list */
    strncpy(v->g.name,name,GLOBAL_NAME_SIZE);
    v->g.flags |= GLOB_USED | GLOB_LOCALVAR;
    v->offset = v->g.value.offset = locals->totalsize;
  }
  
  /* extend list, if necessary */
  if ( locals->count >= locals->maxlist-1 )
  { locals->list = (locals->flags & LL_PERMANENT) ?
     (struct localvar_t*)realloc((char*)locals->list,
      locals->maxlist*2*sizeof(struct localvar_t))  :
    (struct localvar_t*)kb_realloc((char*)locals->list,
      locals->maxlist*2*sizeof(struct localvar_t));
    locals->maxlist*=2;
    v = locals->list + locals->count;   /* reset dangling pointer */
  }
  v->size = size;
  v->offset = locals->totalsize;

  /* move counter up */ 
  v[1].prev = locals->count;
  v[1].scope_depth = v->scope_depth;
  locals->totalsize = v->offset + size;
  retval = locals->count | LOCALVAR;
  locals->count++;
 
  return retval;
} /* end add_local_var */


/**************************************************************************
*
* function: lookup_local_var()
*
* purpose: lookup a local variable to procedure local variable list
*/

ident_t lookup_local_var(name)
char *name;
{ int n;
  struct localvar_t *v;
  struct locallist_t *locals;
   
  if ( local_nest_depth <= 0 ) return 0; 
  locals = local_scope_bases[local_nest_depth-1]; 
  if ( locals->list == NULL ) return 0;
  
  v = locals->list + locals->count;

  /* search for duplicate in current scope */
  for ( n = v->prev ; n >= 0 ; n = ( n ? locals->list[n].prev : -1) )
    if ( strcmp(name,locals->list[n].g.name) == 0 )
    { return LOCALVAR | n;
    }

  return 0; /* not found */
}

/**************************************************************************
*
* function: locals_copy()
*
* purpose: make a copy of local variable structure when assigning
*          a code block in eval().
*/
void locals_copy(dest,src)
struct locallist_t **dest;
struct locallist_t *src;
{
  if ( *dest )
  { /* have old structure */
    myfree((char*)(*dest)->list);
  }
  else
    *dest = (struct locallist_t *)mycalloc(1,sizeof(struct locallist_t));
  if ( src )
  {
    **dest = *src;
    (*dest)->list = (struct localvar_t *)mycalloc(src->count,
                       sizeof(struct localvar_t));
    memcpy((*dest)->list,src->list,src->count*sizeof(struct localvar_t));
  }
  else 
    memset((char*)(*dest),0,sizeof(struct locallist_t));
} 

/**************************************************************************
*
* function: locals_copy_perm()
*
* purpose: make a copy of local variable structure when assigning
*          a code block in eval().  Permanent procedure version.
*/
void locals_copy_perm(dest,src)
struct locallist_t **dest;
struct locallist_t *src;
{
  if ( *dest )
  { /* have old structure */
    free((char*)(*dest)->list);
  }
  else
    *dest = (struct locallist_t *)calloc(1,sizeof(struct locallist_t));
  if ( src )
  {
    **dest = *src;
    (*dest)->list = (struct localvar_t *)calloc(src->count,
                       sizeof(struct localvar_t));
    memcpy((*dest)->list,src->list,src->count*sizeof(struct localvar_t));
  }
  else 
    memset((char*)(*dest),0,sizeof(struct locallist_t));
} 


/**************************************************************************
*
* function: initialize_perm_globals()
*
* purpose:  set up Evolver internal variables in permanent symbol table.
*/

void initialize_perm_globals()
{ int k;
  struct global *g;

  k = add_perm_global("view_transform_swap_colors");
  view_transform_swap_colors_global = k;
  g = globals(k);
  g->attr.arrayptr = 
     (struct array*)calloc(sizeof(struct array)+1*sizeof(int),1); 
  g->attr.arrayptr->dim = 1;
  g->attr.arrayptr->itemsize = sizeof(int);
  g->attr.arrayptr->datatype = INTEGER_TYPE;
  g->attr.arrayptr->datacount = 0;
  g->attr.arrayptr->sizes[0] = 0;
  g->attr.arrayptr->datastart = 0; 
  g->flags = INTERNAL_NAME|PERMANENT|ARRAY_PARAM|READONLY;

  k = add_perm_global("view_transforms");
  view_transforms_global = k;
  g = globals(k);
  g->attr.arrayptr = 
     (struct array*)calloc(sizeof(struct array)+3*sizeof(int),1); 
  g->attr.arrayptr->dim = 3;
  g->attr.arrayptr->itemsize = sizeof(REAL);
  g->attr.arrayptr->datatype = REAL_TYPE;
  g->attr.arrayptr->datacount = 0;
  g->attr.arrayptr->sizes[0] = 0;
  g->attr.arrayptr->sizes[1] = 0;
  g->attr.arrayptr->sizes[2] = 0;
  g->attr.arrayptr->datastart = 0; 
  g->flags = INTERNAL_NAME|PERMANENT|ARRAY_PARAM|READONLY;

  k = add_perm_global("view_matrix");
  view_matrix_global = k;
  g = globals(k);
  g->attr.arrayptr = 
     (struct array*)calloc(sizeof(struct array)+2*sizeof(int),1); 
  g->attr.arrayptr->dim = 2;
  g->attr.arrayptr->itemsize = sizeof(REAL);
  g->attr.arrayptr->datatype = REAL_TYPE;
  g->attr.arrayptr->datacount = 0;
  g->attr.arrayptr->sizes[0] = 0;
  g->attr.arrayptr->sizes[1] = 0;
  g->attr.arrayptr->sizes[2] = 0;
  g->attr.arrayptr->datastart = 0; 
  g->flags = INTERNAL_NAME|PERMANENT|ARRAY_PARAM;

  k = add_perm_global("torus_periods");
  torus_periods_global = k;
  g = globals(torus_periods_global);
  g->attr.arrayptr = 
     (struct array*)calloc(sizeof(struct array)+2*sizeof(int),1); 
  g->attr.arrayptr->dim = 2;
  g->attr.arrayptr->itemsize = sizeof(REAL);
  g->attr.arrayptr->datatype = REAL_TYPE;
  g->attr.arrayptr->sizes[0] = 0;
  g->attr.arrayptr->sizes[1] = 0;
  g->attr.arrayptr->datacount = 0;
  g->flags = INTERNAL_NAME|PERMANENT|ARRAY_PARAM|READONLY;

  k = add_perm_global("inverse_periods");
  inverse_periods_global = k;
  g = globals(k);
  g->attr.arrayptr = 
     (struct array*)calloc(sizeof(struct array)+2*sizeof(int),1); 
  g->attr.arrayptr->dim = 2;
  g->attr.arrayptr->itemsize = sizeof(REAL);
  g->attr.arrayptr->datatype = REAL_TYPE;
  g->attr.arrayptr->datacount = 0;
  g->attr.arrayptr->sizes[0] = 0;
  g->attr.arrayptr->sizes[1] = 0;
  g->attr.arrayptr->sizes[2] = 0;
  g->attr.arrayptr->datastart = 0; 
  g->flags = INTERNAL_NAME|PERMANENT|ARRAY_PARAM|READONLY;

  k = add_perm_global("estimated_change");
  g = globals(k);
  g->value.dataptr = &estimated_change;
  g->flags = INTERNAL_NAME|PERMANENT|REALVAL|READONLY;

  k = add_perm_global("eigenvalues");
  eigenvalues_list_global = k;
  g = globals(eigenvalues_list_global);
  g->attr.arrayptr = 
     (struct array*)calloc(sizeof(struct array)+1*sizeof(int),1); 
  g->attr.arrayptr->dim = 1;
  g->attr.arrayptr->itemsize = sizeof(REAL);
  g->attr.arrayptr->datatype = REAL_TYPE;
  g->attr.arrayptr->sizes[0] = 0;
  g->attr.arrayptr->datacount = 0;
  g->flags = INTERNAL_NAME|PERMANENT|ARRAY_PARAM|READONLY;

  k = add_perm_global("slice_coeff");
  slice_coeff_global = k;
  g = globals(k);
  g->attr.arrayptr = 
     (struct array*)calloc(sizeof(struct array)+1*sizeof(int),1); 
  g->attr.arrayptr->dim = 1;
  g->attr.arrayptr->itemsize = sizeof(REAL);
  g->attr.arrayptr->datatype = REAL_TYPE;
  g->attr.arrayptr->datacount = MAXCOORD+2;
  g->attr.arrayptr->sizes[0] = MAXCOORD+2;
  g->attr.arrayptr->datastart = (char*)slice_coeff-(char*)(g->attr.arrayptr); 
  g->flags = PERMANENT|ARRAY_PARAM|RECALC_PARAMETER|ALWAYS_RECALC;


  k = add_perm_global("clip_coeff");
  clip_coeff_global = k;
  g = globals(k);
  g->attr.arrayptr = 
     (struct array*)calloc(sizeof(struct array)+1*sizeof(int),1); 
  g->attr.arrayptr->dim = 2;
  g->attr.arrayptr->itemsize = sizeof(REAL);
  g->attr.arrayptr->datatype = REAL_TYPE;
  g->attr.arrayptr->datacount = MAXCLIPS*(MAXCOORD+2);
  g->attr.arrayptr->sizes[0] = MAXCLIPS;
  g->attr.arrayptr->sizes[1] = MAXCOORD+2;
  g->attr.arrayptr->datastart = (char*)clip_coeff-(char*)(g->attr.arrayptr); 
  g->flags = PERMANENT|ARRAY_PARAM|RECALC_PARAMETER|ALWAYS_RECALC;

}


/*************************************************************************
*
* function: allocate_transform_colors()
*
* purpose: allocate transform_colors array and update global variable info.
*/
void allocate_transform_colors(count)
int count;
{ struct global *g;

  if ( transform_colors ) myfree((char*)transform_colors);
  if ( count )
    transform_colors = (int*)mycalloc(count,sizeof(int));
  g = globals(view_transform_swap_colors_global);
  g->attr.arrayptr->sizes[0] = count;
  g->attr.arrayptr->datacount = count;
  g->attr.arrayptr->datastart = count ?
        ((char*)transform_colors - (char*)(g->attr.arrayptr)) : 0;
}

/*************************************************************************
*
* function: set_view_transforms_global()
*
* purpose: update view_transforms[][][] global variable info.
*/
void set_view_transforms_global()
{ struct global *g;

  /* need to reorder, since graphgen can leave it in a mess */
  view_transforms = matrix3_reorder(view_transforms,transform_count,SDIM+1,SDIM+1);
  g = globals(view_transforms_global); 
  g->attr.arrayptr->sizes[0] = transform_count;
  g->attr.arrayptr->sizes[1] = SDIM+1;
  g->attr.arrayptr->sizes[2] = SDIM+1;
  g->attr.arrayptr->datacount = transform_count*(SDIM+1)*(SDIM+1);
  g->attr.arrayptr->datastart = transform_count ?
        (char*)view_transforms[0][0] - (char*)(g->attr.arrayptr) : 0;
}


/*************************************************************************
*
* function: set_torus_periods_global()
*
* purpose: update torus_periods[][] global variable info.
*/
void set_torus_periods_global()
{ struct global *g;

  g = globals(torus_periods_global);
  g->attr.arrayptr->sizes[0] = SDIM;
  g->attr.arrayptr->sizes[1] = SDIM;
  g->attr.arrayptr->datacount = SDIM*SDIM;
  g->attr.arrayptr->datastart = 
        (char*)&web.torus_period[0][0] - (char*)(g->attr.arrayptr);
}


/*************************************************************************
*
* function: set_inverse_periods_global()
*
* purpose: update inverse_periods[][] global variable info.
*/
void set_inverse_periods_global()
{ struct global *g;

  g = globals(inverse_periods_global);
  g->attr.arrayptr->sizes[0] = SDIM;
  g->attr.arrayptr->sizes[1] = SDIM;
  g->attr.arrayptr->datacount = SDIM*SDIM;
  g->attr.arrayptr->datastart = 
        (char*)&web.inverse_periods[0][0] - (char*)(g->attr.arrayptr);
}


/*************************************************************************
*
* function: set_view_matrix_global()
*
* purpose: update view_matrix[][] global variable info.
*/
void set_view_matrix_global()
{ struct global *g;

  g = globals(view_matrix_global);
  g->attr.arrayptr->sizes[0] = SDIM+1;
  g->attr.arrayptr->sizes[1] = SDIM+1;
  g->attr.arrayptr->datacount = (SDIM+1)*(SDIM+1);
  g->attr.arrayptr->datastart = 
        (char*)&view[0][0] - (char*)(g->attr.arrayptr);
}


/*************************************************************************
*
* function: set_eigenvalue_list_global()
*
* purpose: update eigenvalue[] global variable info.
*/
void set_eigenvalue_list_global(evalues,count)
REAL *evalues;
int count;
{ struct global *g;

  g = globals(eigenvalues_list_global);
  g->attr.arrayptr->sizes[0] = count;
  g->attr.arrayptr->datacount = count;
  g->attr.arrayptr->datastart = 
        (char*)evalues - (char*)(g->attr.arrayptr);
}

/********************************************************************************
*
* Function: get_name_arrayptr()
*
* Purpose: return a pointer to the array info structure associated with a
*          global variable or extra attribute.  Called from eval*().
*/
struct array *get_name_arrayptr(int name_id,  REAL *newstack, struct locallist_t *localbase)
{
  if ( (name_id & GTYPEMASK) == ATTRIBNAME )
  { struct extra *ex = EXTRAS(name_eltype(name_id)) + (name_id & GLOBMASK);
    return &(ex->array_spec);
  }
  else /* regular global */
  { struct global *g;
    g = globals(name_id);
 
    if ( newstack && ((g->flags & (GLOB_LOCALVAR|FIXED_SIZE_ARRAY))==GLOB_LOCALVAR) )
       return *(struct array **)(newstack+g->value.offset);
    else
       return g->attr.arrayptr;
  }

  return NULL; /* just to keep some compilers happy */
}

/********************************************************************************
*
* Function: get_name_name()
*
* Purpose: return a pointer to the string name associated with a
*          global variable or extra attribute id number.  Called from eval*().
*/
char *get_name_name(int name_id, struct locallist_t *localbase)
{
  if ( (name_id & GTYPEMASK) == ATTRIBNAME )
  { struct extra *ex = EXTRAS(name_eltype(name_id)) + (name_id & GLOBMASK);
    return ex->name;
  }
  else /* regular global */
  { struct global *g;
    g = globals(name_id);
    return g->name;
  }

  return NULL;  /* just to keep some compilers happy */
}

/********************************************************************************
*
* Function: get_name_dim()
*
* Purpose: return dimension of a
*          global variable or extra attribute id number. 
*/
int get_name_dim(int name_id, struct locallist_t *localbase)
{
  if ( (name_id & GTYPEMASK) == ATTRIBNAME )
  { struct extra *ex = EXTRAS(name_eltype(name_id)) + (name_id & GLOBMASK);
    return ex->array_spec.dim;
  }
  else /* regular global */
  { struct global *g;
    g = globals(name_id);
    return g->attr.arrayptr->dim;
  }

  return 0;  /* just to keep some compilers happy */
}
/********************************************************************************
*
* Function: get_name_datatype()
*
* Purpose: return datattype of a
*          global variable or extra attribute id number. 
*/
int get_name_datatype(int name_id, struct locallist_t *localbase)
{
  if ( (name_id & GTYPEMASK) == ATTRIBNAME )
  { struct extra *ex = EXTRAS(name_eltype(name_id)) + (name_id & GLOBMASK);
    return ex->array_spec.datatype;
  }
  else /* regular global */
  { struct global *g;
    g = globals(name_id);
    return g->type;
  }

  return 0; /* just to keep some compilers happy */
}
