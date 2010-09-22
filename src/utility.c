/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*************************************************************
*
*     file:    utility.c
*
*     Purpose:    Various utility routines.
*/

#include "include.h"

/**************************************************************
*  
*  Function: catcher()
*
*  Purpose:  User interrupt catcher.  Sets breakflag so 
*        anybody in a long loop can test to see if
*        user wants to break out.
*/

#ifdef WIN32
#define write(a,b,c) erroutstring(b)
#endif

void catcher(sig)
int sig;
{
#ifdef SIGALRM
  if ( sig == SIGALRM ) 
      signal(SIGALRM,catcher);
#endif

  if ( sig == SIGINT ) 
     { signal(SIGINT,catcher);

#ifdef SGI_MULTI 
    if ( mpflag == M_ACTIVE )
      { if ( m_breakflag[GET_THREAD_ID] )
        longjmp(m_jumpbuf[GET_THREAD_ID],1);
         else m_breakflag[GET_THREAD_ID] = BREAKFULL;
         if ( GET_THREAD_ID == 0 ) 
        write(2,"\nWill break after iteration.\n",29);
         breakflag = BREAKFULL;
         iterate_flag = 0;
      }
    else
      if ( GET_THREAD_ID > 0 ) {}
    else
#endif
    if ( iterate_flag == 1 )
      { write(2,"\nWill break after iteration.\n",29);
         breakflag = BREAKFULL;
         iterate_flag = 0;
      }
    else if ( iterate_flag == 2 ) /* graphing or something */
      { write(2,"\nWill abort operation.\n",24);
         breakflag = BREAKFULL;
         iterate_flag = 0;
      }
    else
      { write(2,"\nAborting operation.\n",22);
        if ( TRY_GRAPH_MUTEX(IMMEDIATE_TIMEOUT) )
           ABORT_GRAPH_MUTEX
        else
           erroutstring("In GRAPH_MUTEX.  May be deadlocked with graphics thread.\n");
#ifdef WIN32
/* separate thread for signal handler, so can't longjmp out of error */
  erroutstring("Can't nicely abort command in WIN32.  Hang in there, or\n");
  erroutstring("use Task Manager to kill Evolver if you think it's hung.\n");
#elif defined(MAC_OS_X)
  if ( iterate_flag > 2 )
    erroutstring("Haven't figured out how to handle this yet.\n");
#else
  kb_error(1357,"",RECOVERABLE_QUIET);
#endif
      }
    while ( commandfd && (commandfd  != stdin) )
         pop_commandfd();
    quiet_flag = 0; 
   }
    
#ifdef SIGTERM
  if ( sig == SIGTERM )  /* for outside kill and dump */
    { sprintf(errmsg,"Caught SIGTERM. proc %d ",getpid());
      erroutstring(errmsg);
      do_dump(NULL); /* dump to default */
      my_exit(1);
    }
#endif

#ifdef SIGHUP
  if ( sig == SIGHUP )  /* for outside kill and dump */
    {
      do_dump(NULL); /* dump to default */
      my_exit(1);
    }
#endif

#ifdef SIGPIPE
  else if ( sig == SIGPIPE )
     { signal(SIGPIPE,catcher);
    write(2,"Broken pipe signal.\n",20);
    broken_pipe_flag = 1;
     }
#endif
}

/***********************************************************************
************************************************************************
                    Memory allocation routines
************************************************************************/

/* Header structure for each block */
struct memhead {  struct memhead *prev, *next; /* doubly linked list */
                  size_t size;
                  int type;   /* see below */
#ifdef MEMSTRINGS
                  char file[28];
                  int line;
#endif
               };
/* Memory block list heads */
struct memhead *perm_block_head;
struct memhead *temp_block_head;
struct memhead *graph_block_head;
struct memhead *eternal_block_head;

/***********************************************************************
*
* function: kb_checksum()
*
* purpose: to calculate a simple checksum on a block of memory.
*/

int kb_checksum(mem,size)
char *mem;
int    size;
{
  int sum = 0;
  int *a,*b;
  b = (int *)mem;
  a = b + size/sizeof(int);
  do { sum += *a; } while ( --a != b);
  return sum;
}

/********************************************************************
*********************************************************************
              Memory allocation routines.  These put my own
              headers on memory blocks, and keep linked lists
              for easy garbage collection.  To start with, there
              are surface-permanent and command-temporary lists,
              but there could be more, for example of info that
              needs to be distributed on a network implementation,
              instead of the present dymem.
********************************************************************/

void mem_sanity_check ARGS((void));

/********************************************************************
*
* function: mem_sanity_check()
*
* purpose: Checks all lists. 
*/

void mem_sanity_check()
{ struct memhead *head;
  int blocktype;

  THREADBLOCK(blocktype);

#if defined(_WIN32) && defined(_HEAPOK)
#ifndef _HEAPINFO
#define _HEAPINFO _heapinfo
#endif

  if ( _heapchk() != _HEAPOK )
    kb_error(2580,"Internal error: Corrupt heap! Memory is trashed.\n",RECOVERABLE);
#endif

  for ( head = eternal_block_head; head ; head = head->next )
  { if ( head->type != ETERNAL_BLOCK )
      kb_error(2581,"Bad eternal memory block chain!\n",RECOVERABLE);
  }

  for ( head = perm_block_head ; head ; head = head->next )
  { if ( head->type != PERM_BLOCK )
      kb_error(2460,"Bad permanent memory block chain!\n",RECOVERABLE);
  }

  if ( blocktype == TEMP_BLOCK )
  for ( head = temp_block_head ; head ; head = head->next )
  { if ( head->type != TEMP_BLOCK )
      kb_error(3208,"Bad temporary memory block chain!\n",RECOVERABLE);
  }

  if ( blocktype == GRAPH_BLOCK )
  for ( head = graph_block_head ; head ; head = head->next )
  { if ( head->type != GRAPH_BLOCK )
      kb_error(2461,"Bad graphics memory block chain!\n",RECOVERABLE);
  }
}

/********************************************************************
*
* function: mem_list_summary()
*
* purpose: Prints summary stats of memory lists.
*/

void mem_list_summary()
{ struct memhead *head;
  size_t total=0,subtotal;
  size_t count=0,subcount;

  for ( head = eternal_block_head, subcount=subtotal=0 ; head ; head = head->next )
  { if ( head->type != ETERNAL_BLOCK )
      kb_error(2462,"Bad eternal memory block chain!\n",RECOVERABLE);
    else { subtotal += head->size; subcount++; }
  }
  if ( verbose_flag )
  { sprintf(errmsg,"Session: %7d blocks, %10d bytes \n",subcount,subtotal);
    outstring(errmsg);
  }
  total += subtotal; count += subcount;

  for ( head = perm_block_head, subcount=subtotal=0 ; head ; head = head->next )
  { if ( head->type != PERM_BLOCK )
      kb_error(2463,"Bad permanent memory block chain!\n",RECOVERABLE);
    else { subtotal += head->size; subcount++; }
  }
  if ( verbose_flag )
  { sprintf(errmsg,"Permanent: %5d blocks, %10d bytes \n",subcount,subtotal);
    outstring(errmsg);
  }
  total += subtotal; count += subcount;

  for ( head = temp_block_head, subcount=subtotal=0 ; head ; head = head->next )
  { if ( head->type != TEMP_BLOCK )
      kb_error(2464,"Bad temporary memory block chain!\n",RECOVERABLE);
    else { subtotal += head->size; subcount++; }
  }

  for ( head = graph_block_head ; head ; head = head->next )
  { if ( head->type != GRAPH_BLOCK )
      kb_error(3210,"Bad graphics memory block chain!\n",RECOVERABLE);
    else { subtotal += head->size; subcount++; }
  }

  if ( verbose_flag )
  { sprintf(errmsg,"Temporary: %5d blocks, %10d bytes\n",subcount,subtotal);
    outstring(errmsg);
  }
  total += subtotal; count += subcount;

  if ( verbose_flag )
    sprintf(errmsg,"Total data memory: %d blocks, %d bytes.\n",count,total);
  else
    sprintf(errmsg,"Total data memory: %d bytes.\n",total);
  outstring(errmsg);

#ifdef LINUX
  if ( verbose_flag )
  { FILE *fd;
    int i,vmem=0,mempages=0;
    char procname[100];
    sprintf(procname,"/proc/%d/stat",getpid()); 
    fd = fopen(procname,"r");
if ( fd == NULL ) perror(procname);
    for ( i = 0 ; i < 22 ; i++ ) fscanf(fd,"%s",msg);
    fscanf(fd,"%d",&vmem);
    sprintf(errmsg,"Virtual memory: %d\n",vmem);
    outstring(errmsg);
    fscanf(fd,"%d",&mempages);
    sprintf(errmsg,"Resident memory: %d\n",mempages*getpagesize());
    outstring(errmsg);
  }
#endif
}


/********************************************************************
*
* function: mem_list_dump()
*
* purpose: Prints details of memory lists.
*/

void mem_list_dump()
{ struct memhead *head;
  size_t subcount;

#ifdef MEMSTRINGS
  outstring("\nEternal memory blocks:\n");
  for ( head = eternal_block_head, subcount=0 ; head ; head = head->next )
  { subcount++;
    sprintf(msg,"%5d. %10d bytes at %p; %28s line %4d\n", 
      subcount,head->size,head,head->file,head->line);
    outstring(msg);
  }

  outstring("\nPermanent memory blocks:\n");
  for ( head = perm_block_head, subcount=0 ; head ; head = head->next )
  { subcount++;
    sprintf(msg,"%5d. %10d bytes at %p; %28s line %4d\n", 
      subcount,head->size,head,head->file,head->line);
    outstring(msg);
  }

  outstring("\nTemporary memory blocks:\n");
  for ( head = temp_block_head, subcount=0 ; head ; head = head->next )
  { subcount++;
    sprintf(msg,"%5d. %10d bytes at %p; %28s line %4d\n", 
      subcount,head->size,head,head->file,head->line);
    outstring(msg);
  }

  outstring("\nGraph memory blocks:\n");
  for ( head = graph_block_head, subcount=0 ; head ; head = head->next )
  { subcount++;
    sprintf(msg,"%5d. %10d bytes at %p; %28s line %4d\n", 
      subcount,head->size,head,head->file,head->line);
    outstring(msg);
  }
#else
  outstring("\nEternal memory blocks:\n");
  for ( head = eternal_block_head, subcount=0 ; head ; head = head->next )
  { subcount++;
    sprintf(msg,"%5d. %10d bytes at %p\n", 
      subcount,head->size,head);
    outstring(msg);
  }

  outstring("\nPermanent memory blocks:\n");
  for ( head = perm_block_head, subcount=0 ; head ; head = head->next )
  { subcount++;
    sprintf(msg,"%5d. %10d bytes at %p\n", 
      subcount,head->size,head);
    outstring(msg);
  }

  outstring("\nTemporary memory blocks:\n");
  for ( head = temp_block_head, subcount=0 ; head ; head = head->next )
  { subcount++;
    sprintf(msg,"%5d. %10d bytes at %p\n", 
      subcount,head->size,head);
    outstring(msg);
  }

  outstring("\nGraph memory blocks:\n");
  for ( head = graph_block_head, subcount=0 ; head ; head = head->next )
  { subcount++;
    sprintf(msg,"%5d. %10d bytes at %p\n", 
      subcount,head->size,head);
    outstring(msg);
  }
#endif
}

/********************************************************************
              Generic list calloc,realloc,free routines.
*********************************************************************/

/********************************************************************
*
* function: list_calloc()
*
* purpose: allocate memory block on desired list.
*/

#ifdef MEMSTRINGS
char *list_calloc(num,size,type,file,line)
size_t num; /* number of chunks */
size_t size; /* size of chunks */
int type; /* memory list type */
char *file;
size_t line;
#else
char *list_calloc(num,size,type)
size_t num; /* number of chunks */
size_t size; /* size of chunks */
int type; /* memory list type */
#endif
{ struct memhead **listhead=NULL;
  struct memhead *ptr;

  switch(type)
  { case PERM_BLOCK: listhead = &perm_block_head; break;
    case TEMP_BLOCK: listhead = &temp_block_head; break;
    case GRAPH_BLOCK: listhead = &graph_block_head; break;
    case ETERNAL_BLOCK: listhead = &eternal_block_head; break;
    default: kb_error(2452,"Internal error: illegal memory block type.\n",
       RECOVERABLE);
  }
 
  ptr = (struct memhead *)calloc(sizeof(struct memhead)+num*size,1);
  if ( ptr == NULL )
  { 
   mem_sanity_check(); 
    sprintf(errmsg,"Internal error: Cannot allocate memory size %d*%d = %d.\n",
         num,size,num*size);
    kb_error(2550,errmsg, RECOVERABLE);
  }

  /* fill in header */
  ENTER_MEM_MUTEX
  ptr->next = *listhead;
  if ( *listhead )
    (*listhead)->prev = ptr;
  *listhead = ptr;
  LEAVE_MEM_MUTEX
  ptr->size = num*size;
  ptr->type = type;
#ifdef MEMSTRINGS
  strncpy(ptr->file,file,sizeof(ptr->file)-1);
  ptr->line = line;
#endif
  
  if ( memdebug ) mem_sanity_check();

  return (char*)(ptr+1);
}  

/************************************************************************
*
* function: list_realloc()
*
* purpose: reallocate memory block on a list.
*
*/

#ifdef MEMSTRINGS
char *list_realloc(ptr,size,type,file,line)
char *ptr;  /* old block */
size_t size;   /* new size */
int type;   /* of list */
char *file;
size_t line;
#else
char *list_realloc(ptr,size,type)
char *ptr;  /* old block */
size_t size;   /* new size */
int type;   /* of list */
#endif
{ struct memhead **listhead=NULL;
  struct memhead *newhead;
  struct memhead *oldhead;
  char *newptr;
  size_t oldsize;

  switch(type)
  { case PERM_BLOCK: listhead = &perm_block_head; break;
    case TEMP_BLOCK: listhead = &temp_block_head; break;
    case GRAPH_BLOCK: listhead = &graph_block_head; break;
    case ETERNAL_BLOCK: listhead = &eternal_block_head; break;
    default: kb_error(2453,"Internal error: illegal memory block type.\n",
       RECOVERABLE);
  }

#ifdef MEMSTRINGS
  if ( ptr == NULL ) return list_calloc(size,1,type,file,line);
#else
  if ( ptr == NULL ) return list_calloc(size,1,type);
#endif

  oldhead = (struct memhead *)ptr - 1;
  if ( oldhead->type != type )
  { sprintf(errmsg,"Internal error: Trying to realloc memory block to different list.\n");
  #ifdef MEMSTRINGS
    sprintf(errmsg+strlen(errmsg),"File %s, line %d; original alloc %s:%d\n",
        file,line,oldhead->file,oldhead->line);
  #endif
    kb_error(2448,errmsg,RECOVERABLE);
  }
  oldsize = oldhead->size;
 
  ENTER_MEM_MUTEX  /* since manipulating shared variables */
  /* delete from my list */
  if ( oldhead->next ) oldhead->next->prev = oldhead->prev;
  if ( oldhead->prev ) oldhead->prev->next = oldhead->next;
  else *listhead = oldhead->next;

  newhead = 
     (struct memhead *)realloc((char*)oldhead,sizeof(struct memhead)+size);
  if ( newhead == NULL )
  { sprintf(errmsg,
     "Internal error: Cannot reallocate memory from old size %d to new %d.\n",
       oldsize,size);
    kb_error(1360,errmsg,RECOVERABLE);
  }

  /* fill in header */
  newhead->next = *listhead;
  newhead->prev = NULL;
  if ( *listhead )
    (*listhead)->prev = newhead;
  *listhead = newhead;
  LEAVE_MEM_MUTEX
  newhead->size = size;
  newhead->type = type;
#ifdef MEMSTRINGS
  strncpy(newhead->file,file,sizeof(newhead->file)-1);
  newhead->line = line;
#endif
  
  newptr = (char*)(newhead+1);
  if (oldsize < size )  memset(newptr+oldsize,0,size-oldsize);


  if ( memdebug ) mem_sanity_check();

  return newptr;

} /* end of list_realloc() */

/**************************************************************************
*
* function: list_free()
*
* purpose: free one block on a memory list.
*/

void list_free(ptr,type)
char *ptr;
int type;  /* of memory list */
{ struct memhead *head;
  struct memhead **listhead=NULL;

  if ( !ptr )
  { if ( memdebug ) erroutstring("\n");
    return;
  }
  
  if ( memdebug ) mem_sanity_check();

  
  switch(type)
  { case PERM_BLOCK: listhead = &perm_block_head; break;
    case TEMP_BLOCK: listhead = &temp_block_head; break;
    case GRAPH_BLOCK: listhead = &graph_block_head; break;
    case ETERNAL_BLOCK: listhead = &eternal_block_head; break;
    default: kb_error(2454,"Internal error: illegal memory block type.\n",
       RECOVERABLE);
  }

  head = (struct memhead *)ptr - 1;

  if ( memdebug )
  { 
#ifdef MEMSTRINGS
    sprintf(msg,"  allocated from %s:%d\n",head->file,head->line);
#else
    sprintf(msg,"\n");
#endif
    erroutstring(msg);
  }

  if ( head->type != type )
  { switch ( head->type )
    { case ETERNAL_BLOCK: case TEMP_BLOCK: case PERM_BLOCK:
      case GRAPH_BLOCK:
#ifdef MEMSTRINGS
		  sprintf(errmsg,"Internal error: Trying to free memory block from wrong type list.\nAllocated from %s:%d\n",
              head->file,head->line);
#else
		  sprintf(errmsg,"Internal error: Trying to free memory block from wrong type list.\n");

#endif
		  kb_error(2465,errmsg,RECOVERABLE);
       default: 
#ifdef MEMSTRINGS
		  sprintf(errmsg,"Internal error: Trying to free corrupt memory block.\nAllocated from %s:%d\n",
              head->file,head->line);
         kb_error(2466,errmsg,RECOVERABLE);
#else
         kb_error(3209,
          "Internal error: trying to free corrupt memory block.\n",
          RECOVERABLE);
#endif
     }
   }

  /* delete from my list */
  ENTER_MEM_MUTEX
  if ( head->next ) head->next->prev = head->prev;
  if ( head->prev ) head->prev->next = head->next;
  else *listhead = head->next;
  LEAVE_MEM_MUTEX
  free((char*)head);

  if ( memdebug ) mem_sanity_check();

}

/**************************************************************************
*
* function: list_free_all()
*
* purpose: free all blocks on a list.
*/

void list_free_all(type)
int type;  /* of list */
{ struct memhead **listhead=NULL;
  struct memhead *head,*nexthead;

  switch(type)
  { case PERM_BLOCK: listhead = &perm_block_head; break;
    case TEMP_BLOCK: listhead = &temp_block_head; break;
    case GRAPH_BLOCK: listhead = &graph_block_head; break;
    case ETERNAL_BLOCK: listhead = &eternal_block_head; break;
    default: kb_error(2455,"Internal error: illegal memory block type.\n",
       RECOVERABLE);
  }

  ENTER_MEM_MUTEX
  for ( head = *listhead; head ; head = nexthead )
  { if ( head->type != type )
      kb_error(3905,"Internal error: corrupt memory list.\n",RECOVERABLE);
    if ( memdebug )
    {
#ifdef MEMSTRINGS
       sprintf(msg,"Freeing %p, %d bytes, allocated at %s:%d\n",head+1,
           head->size,head->file,head->line);
#else
       sprintf(msg,"Freeing %p, %d bytes\n",head+1,head->size);
#endif
       erroutstring(msg);
    }
    nexthead = head->next;
    free((char*)head);
  }
  *listhead = NULL;
  LEAVE_MEM_MUTEX
} /* end list_free_all */

/********************************************************************
              Permanent memory allocation. These blocks
              will be automatically freed at the start of 
              a new surface, eliminating a lot of clean-up
              drudgery.
********************************************************************/

/*********************************************************************
*
*  function: kb_calloc
*
*  purpose: memory allocation with error detection
*          Also memory debugging.
*          Called by macro mycalloc(num,size).
*/

#ifdef MEMSTRINGS
char *kb_calloc(num,size,file,line)
size_t num,size;
char *file;
size_t line;
#else
char *kb_calloc(num,size)
size_t num,size;
#endif
{ char *ptr;

  if ( memdebug )
  {
#ifdef MEMSTRINGS
    sprintf(errmsg,"mycalloc %30.30s %5d: %10d ",file,line,num*size);
    erroutstring(errmsg);
#else
    sprintf(errmsg,"mycalloc %d*%d = %d bytes ",num,size,num*size);
    erroutstring(errmsg); 
#endif
  }

#ifdef MEMSTRINGS
  ptr = list_calloc(num,size,PERM_BLOCK,file,line);
#else
  ptr = list_calloc(num,size,PERM_BLOCK);
#endif

  if ( memdebug )
  { sprintf(errmsg,"at %p\n",ptr); erroutstring(errmsg);
    mem_sanity_check();
  }

  return ptr;
}  /* end kb_calloc */


/*********************************************************************
*
*  function: KB_realloc
*
*  purpose: memory re-allocation with error detection
*          Called through macro kb_realloc for optional memory debug.
*/

#ifdef MEMSTRINGS
char *KB_realloc(ptr,size,file,line)
char *ptr;
size_t size;
char *file; size_t line;
#else
char *KB_realloc(ptr,size)
char *ptr;
size_t size;
#endif
{ char *newptr;

  if ( memdebug )
  { 
#ifdef MEMSTRINGS
     sprintf(errmsg,"%30.30s %4d: realloc old %p size %d to size %d \n",
         file,line,ptr, (ptr ? ((struct memhead *)ptr)[-1].size:0) ,size);
#else
     sprintf(errmsg,"realloc old %p size %d to size %d \n",ptr,
          (ptr ? ((struct memhead *)ptr)[-1].size:0 ) ,size);
#endif
     erroutstring(errmsg);
  }

#ifdef MEMSTRINGS
  if ( ptr == NULL ) newptr = kb_calloc(size,1,file,line);
  else newptr = list_realloc(ptr,size,PERM_BLOCK,file,line);
#else
  if ( ptr == NULL ) newptr = mycalloc(size,1);
  else newptr = list_realloc(ptr,size,PERM_BLOCK);
#endif

  if ( memdebug )
  { sprintf(errmsg,"at %p\n",newptr); erroutstring(errmsg);
    mem_sanity_check();
  }

  return newptr;
} /* end kb_realloc() */

/********************************************************************
*
* function:  myfree()
*
* purpose:  Freeing memory.  Common call for permanent memory freeing.
*
*/

void myfree(ptr)
char *ptr;
{ 
  if ( memdebug )
  { 
     sprintf(errmsg,"free %p ",ptr);
     erroutstring(errmsg);
  }

  list_free(ptr,PERM_BLOCK);
}

/********************************************************************
              Temporary memory allocation. These blocks
              will be automatically freed at the end of 
              each command, although subroutines should
              practice good memory hygeine and free all
              their own memory.  Mostly intended in case
              of error recovery.
********************************************************************/

/********************************************************************
*
* function:  kb_temp_calloc()
*
* purpose:  memory allocation that can be undone by longjmp at error.
*
*/

#ifdef MEMSTRINGS
char *kb_temp_calloc(num,size,file,line)
size_t num,size;
char *file;
size_t line;
#else
char *kb_temp_calloc(num,size)
size_t num,size;
#endif
{ char *ptr;
  int blocktype = TEMP_BLOCK;
 
  if ( memdebug )
  {
#ifdef MEMSTRINGS
    sprintf(errmsg,"%30.30s %4d: ",file,line);
    erroutstring(errmsg);
#endif
    sprintf(errmsg,"temp_calloc %9d bytes ",num*size);
    erroutstring(errmsg);
  }

  THREADBLOCK(blocktype);

#ifdef MEMSTRINGS
  ptr = list_calloc(num,size,blocktype,file,line);
#else
  ptr = list_calloc(num,size,blocktype);
#endif

  if ( memdebug )
  { sprintf(errmsg,"at %p \n",ptr); erroutstring(errmsg);
    mem_sanity_check();
  }

  return ptr;
}

/********************************************************************
*
* function:  temp_realloc()
*
* purpose:  Changing size of temporary memory allocation.
*
*/

#ifdef MEMSTRINGS
char * kb_temp_realloc(ptr,size,file,line)
char *ptr;
size_t size;
char *file;
size_t line;
#else
char * kb_temp_realloc(ptr,size)
char *ptr;
size_t size;
#endif
{
  char *newptr;
  int blocktype = TEMP_BLOCK;

  if ( memdebug )
  { 
#ifdef MEMSTRINGS
    sprintf(errmsg,"%30.30s %4d: temp_realloc\n old size %9d at %p to size %9d",
         file,line, (ptr ? ((struct memhead *)ptr)[-1].size:0),ptr ,size);
#else
    sprintf(errmsg,"temp_realloc old %p size %9d to size %9d",ptr,
          (ptr ? ((struct memhead *)ptr)[-1].size:0 ) ,size);
#endif
    erroutstring(errmsg);
  }

  THREADBLOCK(blocktype);

#ifdef MEMSTRINGS
  newptr = list_realloc(ptr,size,blocktype,file,line);
#else
  newptr = list_realloc(ptr,size,blocktype);
#endif

  if ( memdebug )
  { sprintf(errmsg," at %p \n",newptr); erroutstring(errmsg);
    mem_sanity_check();
  }

  return newptr;
}


/********************************************************************
*
* function:  temp_free()
*
* purpose:  Usual freeing of temporary memory allocation.
*
*/

void temp_free(ptr)
char *ptr;
{
  int blocktype = TEMP_BLOCK;
  if ( memdebug )
  { 
     sprintf(errmsg,"temp_free %p ",ptr);
     erroutstring(errmsg);
  }
  THREADBLOCK(blocktype);

  list_free(ptr,blocktype);

} /* end temp_free */

/********************************************************************
*
* function:  temp_free_all() 
*
* purpose:  Freeing of all temporary memory in case of error.
*
*/

void temp_free_all()
{
  int blocktype = 0;

  #ifdef _DEBUGXX
  int oldmemdebug = memdebug;
  memdebug=1;
  #endif
  
 if ( memdebug )
    erroutstring("Entering temp_free_all\n");
    
  THREADBLOCK(blocktype);
  list_free_all(blocktype);

  #ifdef MPI_EVOLVER
  if ( this_task == 0 && blocktype != GRAPH_BLOCK )
    mpi_temp_free_all();
  #endif
  
  #ifdef _DEBUGXX
  memdebug = oldmemdebug;
  #endif

  /* pointers to be annulled in main thread */
  if ( blocktype == TEMP_BLOCK )
  {
    saved.coord = NULL;
    vhead = NULL;
    pressures = NULL;
    conrhs = NULL;
    vgradbase = NULL;
    rightside = NULL;
    vpressures = NULL;
    e_curve = NULL;
    v_curve = NULL;
  }
}


/********************************************************************

    Memory allocation routines for the block of memory holding
    dynamic structures of surface description.  Enables
    exporting surface info to multiple processors.

    Returns are offsets from start of dynamic region.

*********************************************************************/
 
#define ALIGNSIZE 8
struct dy_head { int size; /* bytes in block, including structures */
                 int state; /* see defines; also used as magic number */
           DY_OFFSET prev; /* linear block list */
           DY_OFFSET next;
           DY_OFFSET prevfree; /* disordered free block list */
           DY_OFFSET nextfree; 
              };
#define DY_FREE  0xCCAA
#define DY_USED  0xDDBB
#define DY_STARTSIZE 0x1000

int dy_minfrag = 2*sizeof(struct dy_head);
int dy_overhead = sizeof(struct dy_head);
DY_OFFSET dy_firstblock;
DY_OFFSET dy_lastblock;

DY_OFFSET dy_calloc(num,size)
int num;
int size;
{ struct dy_head *ptr,*newptr,*lastptr;
  DY_OFFSET off;
  int needed;
  int surplus;
  int asize;
  int i;
  
  if ( dymemsize == 0 )  /* initial allocation */
  { if ( sizeof(struct dy_head) % ALIGNSIZE )
    { sprintf(errmsg,
   "Internal error: sizeof(struct dy_head) %d not multiple of ALIGNSIZE %d\n",
        sizeof(struct dy_head),ALIGNSIZE);
      kb_error(1364,errmsg,UNRECOVERABLE);
    }
    dymem = mycalloc(DY_STARTSIZE,1);
    dymemsize = DY_STARTSIZE;
    strcpy(dymem,"DY_MEM"); /* so looks pretty in debugger */
    dy_firstblock = sizeof(struct dy_head);
    ptr = (struct dy_head *)dymem+1; /* so offset 0 not used */
    ptr->size = DY_STARTSIZE-sizeof(struct dy_head);
    ptr->state = DY_FREE;
    ptr->next = 0;
    ptr->prev = 0;
    dy_lastblock = sizeof(struct dy_head);

    dy_freestart = sizeof(struct dy_head);
    ptr->prevfree = 0; /* first in list */
    ptr->nextfree = 0; /* last in list */
  }

  /* size of needed block */
  asize = ((num*size + ALIGNSIZE - 1)/ALIGNSIZE)*ALIGNSIZE;
  if ( asize <= dy_minfrag ) asize = dy_minfrag;
  needed = sizeof(struct dy_head) + asize;

  if ( memdebug )
  { sprintf(errmsg,"dy_alloc %d*%d = %d bytes.\n",num,size,asize);
    erroutstring(errmsg);
  }

retry_alloc: 

  /* look for first fit in free block list */
  if ( dy_freestart )
    for ( off = dy_freestart ; off > 0 ; off = ptr->nextfree )
    { 
      ptr = (struct dy_head *)(dymem + off);
      /* check size */
      if ( needed > ptr->size )
      { if ( ptr->nextfree == 0 ) break;
        else continue;
      }
      surplus = ptr->size - needed;
      if ( surplus > dy_minfrag )
      { /* divide block */
        newptr = (struct dy_head *)(dymem + off + needed);
        newptr->size = ptr->size - needed;
        newptr->state = DY_FREE;
        newptr->next = ptr->next;
        newptr->prev = off;
        if ( newptr->next )
           ((struct dy_head *)(dymem + newptr->next))->prev = 
                off + needed; 
        else 
           dy_lastblock = off + needed;
        newptr->nextfree = ptr->nextfree;
        newptr->prevfree = ptr->prevfree;

        ptr->size = needed;
        ptr->next = off + needed;
        if (ptr->prevfree )
         ((struct dy_head *)(dymem + ptr->prevfree))->nextfree = off + needed;
        else dy_freestart = off + needed;
        if ( ptr->nextfree )
         ((struct dy_head *)(dymem + ptr->nextfree))->prevfree = off + needed;
      }
      else
      { /* remove from freelist */
        if ( ptr->prevfree )
           ((struct dy_head *)(dymem + ptr->prevfree))->nextfree 
                = ptr->nextfree;
        else dy_freestart = ptr->nextfree;
        if ( ptr->nextfree )
           ((struct dy_head *)(dymem + ptr->nextfree))->prevfree 
                = ptr->prevfree;
        ptr->nextfree = 0;     
        ptr->prevfree = 0;
      }
      ptr->state = DY_USED;
#ifdef _DEBUG
dy_check();
#endif
      /* some handy debugging equivalences */
      Globals = globals(0);
      for ( i = 0 ; i < NUMELEMENTS ; i++ )
       Extras[i] = EXTRAS(i);
      return off + sizeof(struct dy_head);
    }

  /* if here, then no block big enough */
  if ( memdebug ) erroutstring ("expanding dymem.\n");
  ENTER_GRAPH_MUTEX
  dymem = kb_realloc(dymem,2*dymemsize);
  LEAVE_GRAPH_MUTEX
  lastptr = (struct dy_head *)(dymem + dy_lastblock);

  if ( lastptr->state == DY_FREE )
    { /* combine with last block */
      lastptr->size += dymemsize;
    }
  else 
    { /* put in free block list at head */
      ptr = (struct dy_head *)(dymem + dymemsize);
      ptr->size = dymemsize;
      ptr->state = DY_FREE;
      ptr->prev = dy_lastblock;
      ptr->next = 0;
      lastptr = (struct dy_head *)(dymem + dy_lastblock);
      dy_lastblock = dymemsize;
      lastptr->next = dymemsize;
      ptr->prevfree = 0;
      ptr->nextfree = dy_freestart;
      if ( dy_freestart )
         ((struct dy_head *)(dymem+dy_freestart))->prevfree = dymemsize;
      dy_freestart = dymemsize;
    }
      
  dymemsize = 2*dymemsize;
  goto retry_alloc;

}

DY_OFFSET dy_realloc(old,newnum,newsize)
DY_OFFSET old;
int newnum;
int newsize;
{ DY_OFFSET newspot;
  struct dy_head *ptr;
  int i;

  if ( memdebug )
  { erroutstring("dy_realloc "); }

  ENTER_GRAPH_MUTEX
  newspot = dy_calloc(newnum,newsize);
  if ( old )
  { ptr = (struct dy_head *)(dymem + old) - 1;
    if ( ptr->size < newnum*newsize )
      memcpy(dymem+newspot,dymem+old,ptr->size);
    else  memcpy(dymem+newspot,dymem+old,newnum*newsize);
    dy_free(old);
  }
  LEAVE_GRAPH_MUTEX

  /* some handy debugging equivalences */
  Globals = globals(0);
  for ( i = 0 ; i < NUMELEMENTS ; i++ )
       Extras[i] = EXTRAS(i);

#ifdef _DEBUG
dy_check();
#endif
  return newspot;
}

void dy_free(spot)
DY_OFFSET spot;
{ DY_OFFSET off = spot - sizeof(struct dy_head);
  struct dy_head *ptr = (struct dy_head *)(dymem + off);
  struct dy_head *prevhead,*nexthead;

  if ( ptr->state != DY_USED )
     kb_error(1366,"Internal error: Trying to dy_free unallocated block.\n",
       RECOVERABLE);

  memset(dymem+spot,0,ptr->size - dy_overhead);
  ptr->nextfree = dy_freestart;
  ptr->prevfree = 0;
  if ( dy_freestart )
     ((struct dy_head *)(dymem + dy_freestart))->prevfree = off;
  dy_freestart = off;
  ptr->state = DY_FREE;
 
  /* see if can combine with nbrs */
  /* check preceding block */
  if ( ptr->prev )
    { prevhead = (struct dy_head *)(dymem + ptr->prev);
      if ( prevhead->state == DY_FREE )
      { /* combine */
        prevhead->size += ptr->size;
        prevhead->next = ptr->next;
        if ( ptr->next )
        { nexthead = (struct dy_head *)(dymem + ptr->next);
          nexthead->prev = ptr->prev;
        }
        if ( off == dy_lastblock ) 
          dy_lastblock = ptr->prev;
        /* remove from freelist */
        if ( ptr->nextfree )
          ((struct dy_head*)(dymem+ptr->nextfree))->prevfree = ptr->prevfree; 
        if ( ptr->prevfree )
          ((struct dy_head*)(dymem+ptr->prevfree))->nextfree = ptr->nextfree; 
        else dy_freestart = ptr->nextfree;
        memset((char*)ptr,0,sizeof(struct dy_head));
        ptr = prevhead;  /* ready for next stage of combining */
        
      }
    } 
  /* check following block */
  if ( ptr->next )
    { nexthead = (struct dy_head *)(dymem + ptr->next);
      if ( nexthead->state == DY_FREE )
      { /* combine */
        if ( ptr->next == dy_lastblock )
           dy_lastblock = nexthead->prev;
        prevhead = ptr; ptr = nexthead;
        prevhead->size += ptr->size;
        prevhead->next = ptr->next;
        if ( ptr->next )
        { nexthead = (struct dy_head *)(dymem + ptr->next);
          nexthead->prev = ptr->prev;
        }
        /* remove from freelist */
        if ( ptr->nextfree )
          ((struct dy_head*)(dymem+ptr->nextfree))->prevfree = ptr->prevfree; 
        if ( ptr->prevfree )
          ((struct dy_head*)(dymem+ptr->prevfree))->nextfree = ptr->nextfree; 
        else dy_freestart = ptr->nextfree;
        memset((char*)ptr,0,sizeof(struct dy_head));
      }
    } 
#ifdef _DEBUG
dy_check();
#endif
}

void dy_check()
{
  struct dy_head *head,*nexthead;
  int freeblocks = 0;
  int usedblocks = 0;
  int freebytes = 0;
  int usedbytes = 0;
  DY_OFFSET off;

  for ( off = dy_firstblock ; off != 0 ; off = head->next )
  { head = (struct dy_head *)(dymem + off);
    switch ( head->state )
    { case DY_FREE: freeblocks++; freebytes += head->size; break;
      case DY_USED: usedblocks++; usedbytes += head->size; break;
      default: kb_error(2468,"Internal error: Corrupt dy_mem; bad state.\n",
         RECOVERABLE);
         return;
    }
    if ( head->next )
    { nexthead = (struct dy_head *)(dymem + head->next);
      if ( off != nexthead->prev )
        kb_error(2469,"Internal error: Corrupt dy_mem; bad prev.\n",RECOVERABLE);
    }
  }

  if ( memdebug )
  { sprintf(errmsg,"Dymem arena: %d\n",dymemsize);
    erroutstring(errmsg);
    sprintf(errmsg,"Dymem free blocks: %d  free bytes: %d\n",freeblocks,freebytes);
    erroutstring(errmsg);
    sprintf(errmsg,"Dymem used blocks: %d  used bytes: %d\n",usedblocks,usedbytes);
    erroutstring(errmsg);
  }

  /* check freelist integrity */
  for ( off = dy_freestart ; (off != 0) && freeblocks ; off = head->nextfree )
  { head = (struct dy_head *)(dymem + off);
    if ( head->state != DY_FREE )
       kb_error(2555,"Internal error: Corrupt dy_mem nextfree.\n",RECOVERABLE);
    if ( head->nextfree )
    { nexthead = (struct dy_head *)(dymem + head->nextfree);
      if ( off != nexthead->prevfree )
        kb_error(2470,"Internal error: Corrupt dy_mem prevfree.\n",RECOVERABLE);
    }
    freeblocks--;
  }
  if ( freeblocks || (off != 0) )
        kb_error(2471,"Internal error: Corrupt dy_mem free list.\n",RECOVERABLE);

} /* end dy_check() */

/********* end of dynamic memory stuff **************************/


/**************************************************************
*
*  Function: calc_edge()
*
*  Purpose:  Calculate the length of an edge from its endpoint
*        coordinates.  Puts result into edge structure.
*
*/

void calc_edge(e_id)
edge_id e_id;
{
  REAL len=0.0;
  REAL s[MAXCOORD];
  REAL midx[MAXCOORD];
  int i,j,k;
  vertex_id tv = get_edge_tailv(e_id);
  REAL *xt=get_coord(tv);
  REAL euclidean;

  if ( everything_quantities_flag ) 
  { instance_attribute(e_id,length_method_number);
       /* legitimate methods set edge length */
    return;
  }

  get_edge_side(e_id,s);
  euclidean = SDIM_dot(s,s);
  if ( klein_metric_flag )
  { for ( i = 0 ; i < SDIM ; i++ ) midx[i] = xt[i] + s[i];/*unwrap head*/
    len = klein_length(xt,midx);
  }
  else if ( web.metric_flag )
  {
     /* energy due to linear tension, metric evaluated at midpoint */
     for ( k = 0, len = 0.0 ; k < gauss1D_num ; k++ )
     { REAL sum;
       for ( i = 0 ; i < SDIM ; i++ )
          midx[i] = gauss1Dpt[k]*s[i] + xt[i];
       if ( web.conformal_flag )
       { REAL gg = eval(&web.metric[0][0],midx,NULLID,NULL);
         len += gauss1Dwt[k]*sqrt(gg*euclidean);
       }
       else
       { for ( sum = 0.0, i = 0 ; i < SDIM ; i++ )
         for ( j = 0 ; j < SDIM ; j++ )
          sum += s[i]*s[j]*eval(&web.metric[i][j],midx,NULLID,NULL);
         len += gauss1Dwt[k]*sqrt(sum);
       }
     }
  }
  else
  {
    switch (web.modeltype)
    { case LINEAR: len = sqrt(euclidean); break;
      case QUADRATIC:
      { MAT2D(x,3,MAXCOORD);
        REAL tang[MAXCOORD];
        int m;

        len = 0;
        get_edge_verts(e_id,x,NULL);
        for ( m = 0 ; m < gauss1D_num ; m++ )
        { for ( j = 0 ; j < SDIM ; j ++ )
         { tang[j] = 0.0;
            for ( k = 0 ; k < edge_ctrl ; k++ )
            tang[j] += gauss1polyd[k][m]*x[k][j];
         }
          len += gauss1Dwt[m]*sqrt(SDIM_dot(tang,tang));
        }
     } break;
    case LAGRANGE:
    { /* tangent vectors at gauss points */
      int m;
      MAT2D(x,20,MAXCOORD);
      REAL tang[MAXCOORD];
      int ctrl = web.skel[EDGE].ctrlpts;
      struct gauss_lag *gl = &gauss_lagrange[1][web.gauss1D_order];
      len = 0.0;
      get_edge_verts(e_id,x,NULL);
      for ( m = 0 ; m < gl->gnumpts ; m++ )
      {  vec_mat_mul(gl->gpolypart[m][0],x,tang,ctrl,SDIM);
          len += gl->gausswt[m]*sqrt(SDIM_dot(tang,tang));
      }

    }

    break; 

   } /* end switch */
 }
 set_edge_length(e_id,len);
}

/************************************************************
*
*  get_edge_side()
*
*  Purpose:  calculate the vector of an edge
*
*  Input:     edge_id e_id - specifies edge
*        REAL  *x;  - 3 components of vector
*
*  Output:    x[] has side components
*/

void get_edge_side(e_id,x)
edge_id e_id;
REAL *x;
{
  REAL *y,*z;
  int i;
  REAL w[MAXCOORD];
  
  y = get_coord(get_edge_tailv(e_id));
  z = get_coord(get_edge_headv(e_id));
  if ( web.symmetry_flag ) 
  {
    (*sym_wrap)(z,w,get_edge_wrap(e_id));
    z = w;
  }
  for ( i = 0 ; i < SDIM ; i++ ) 
      x[i] = z[i] - y[i];
}

/************************************************************
*
*  get_edge_tail_tangent()
*
*  Purpose:  Calculate the tangent vector of an edge at the tail.
*            Magnitude of tangent equal to length, pretty much.
*
*  Input:     edge_id e_id - specifies edge
*        REAL  *x;  - 3 components of vector
*
*  Output:    x[] has side components
*/

void get_edge_tail_tangent(e_id,x)
edge_id e_id;
REAL *x;
{
  int i,n;
  REAL w[MAXCOORD],ww[MAXCOORD];
  
  if ( web.lagrange_order == 1 )
  { 
    REAL *y,*z;
    y = get_coord(get_edge_tailv(e_id));
    z = get_coord(get_edge_headv(e_id));
    if ( web.symmetry_flag ) 
    {
      (*sym_wrap)(z,w,get_edge_wrap(e_id));
      z = w;
    }
    for ( i = 0 ; i < SDIM ; i++ ) 
      x[i] = z[i] - y[i];
    return;
  }

  if ( web.modeltype == QUADRATIC )
  { vertex_id v[3];
    REAL *xx[3];
    v[0] = get_edge_tailv(e_id);
    v[1] = get_edge_midv(e_id);
    v[2] = get_edge_headv(e_id);
    xx[0] = get_coord(v[0]);
    xx[1] = get_coord(v[1]);
    xx[2] = get_coord(v[2]);
    if ( web.symmetry_flag )
    { WRAPTYPE wrap = get_edge_wrap(e_id);
      (*sym_wrap)(xx[2],w,wrap);
      xx[2] = w;
      if ( inverted(e_id) )
      { (*sym_wrap)(xx[1],ww,wrap);
        xx[1] = ww;
      }
    }
    if ( circular_arc_flag )
    { REAL dx1 = xx[1][0] - xx[0][0];
      REAL dx2 = xx[2][0] - xx[1][0];
      REAL dy1 = xx[1][1] - xx[0][1];
      REAL dy2 = xx[2][1] - xx[1][1];
      REAL denom1 = sqrt(dx1*dx1+dy1*dy1);
      REAL denom2 = sqrt(dx2*dx2+dy2*dy2);
      REAL chord,sinth,costh,angle,length;
      if (denom1*denom2 == 0.0) {x[0] = x[1] = 0.0; return ;}
      chord = sqrt((dx1+dx2)*(dx1+dx2)+(dy1+dy2)*(dy1+dy2));
      sinth = (dx1*dy2 - dy1*dx2)/denom1/denom2;
      costh = (dx1*dx2 + dy1*dy2)/denom1/denom2;
      angle = atan2(sinth,costh);
      if ( sinth == 0.0 )
      { for ( i = 0 ; i < SDIM ; i++ )
          x[i] = xx[2][i] - xx[0][i];
        return;
      }
      length = chord*angle/sinth;
      x[0] = length*(costh*(dx1+dx2) + sinth*(dy1+dy2))/chord;
      x[1] = length*(costh*(dy1+dy2) - sinth*(dx1+dx2))/chord;
      return; 
    }
    else 
    { /* regular quadratic */
      for ( i = 0 ; i < SDIM ; i++ )
          x[i] = -3*xx[0][i] + 4*xx[1][i] - xx[2][i];
    }
    return;
  }

  if ( web.modeltype == LAGRANGE )
  { struct gauss_lag *gl = &gauss_lagrange[1][web.gauss1D_order];
    int ctrl = web.skel[EDGE].ctrlpts;
    int spot = inverted(e_id) ? ctrl-1 : 0;
    vertex_id *v;
    REAL *xx[MAXLAGRANGE+1];

    v = get_edge_vertices(e_id);
    for ( i = 0 ; i < ctrl ; i++ )
      xx[i] = get_coord(v[i]);
    if ( web.symmetry_flag )
    { WRAPTYPE wrap = get_edge_wrap(e_id);
      (*sym_wrap)(xx[ctrl-1],w,wrap);
      xx[ctrl-1] = w;
    }
    for ( i = 0 ; i < SDIM ; i++ ) 
    { x[i] = 0;
      for ( n = 0 ; n < ctrl ; n++ )
        x[i] += gl->lpolypart[spot][0][n]*xx[n][i];
      x[i] *= ctrl;  /* scaling to length of edge */
      if ( inverted(e_id) )
        x[i] = -x[i];
    }
  }
}

/*************************************************************
*
*  function: get_edge_adjust()
*
*  purpose:  adjust a side vector for torus wrap.
*        used only in torvol.c.
*
*/
  
void get_edge_adjust(e_id,w)
edge_id e_id;
REAL *w;
{
  REAL x[MAXCOORD];
  int i ;

  for ( i = 0 ; i < SDIM ; i++ ) x[i] = 0.0;
  (*sym_wrap)(x,w,get_edge_wrap(e_id));
}
             


/***************************************************************
*
*  Function:  get_vertex_evalence
*
*  Purpose:  return number of edges around vertex
*
*/

int get_vertex_evalence(v_id)
vertex_id v_id;
{
  int n = 0;
  edge_id e_id;
  edge_id next_e;
  int attr = get_vattr(v_id);

  if ( attr & (Q_MIDPOINT|Q_MIDEDGE) ) return 1;
  e_id = get_vertex_edge(v_id);
  if ( !valid_element(e_id) ) return 0;
  next_e = e_id;
  do
  { n++;
    next_e = get_next_tail_edge(next_e);
    if ( n > 2*web.skel[EDGE].count )
    { sprintf(errmsg,
         "Internal error: vertex_evalence() in infinite loop on vertex %s.\n",
           ELNAME(v_id));
      kb_error(2482,errmsg,RECOVERABLE);
    }
  }
  while ( !equal_id(next_e,e_id) );
  return n;
}


/***************************************************************
*
*  Function:  get_edge_valence
*
*  Purpose:  return number of facets around edge
*
*/

int get_edge_valence(e_id)
edge_id e_id;
{
  int n = 0,m=0;
  facetedge_id fe = get_edge_fe(e_id);
  facetedge_id next_fe = fe;

  if ( !valid_id(fe) ) return 0;
  do
  { if ( valid_id(get_fe_facet(fe)) ) n++;
    next_fe = get_next_facet(next_fe);
    m++;  /* infinite loop preventer */
    if ( m > web.skel[FACETEDGE].count )
    { sprintf(errmsg,
         "Internal error: edge_valence() in infinite loop on edge %s.\n",
           ELNAME(e_id));
      kb_error(2480,errmsg,RECOVERABLE);
    }
  }
  while ( next_fe != fe );
  return n;
}



/***************************************************************
*
*  Function:  get_facet_valence
*
*  Purpose:  return number of edges around facet
*
*/

int get_facet_valence(f_id)
facet_id f_id;
{
  int n = 0;
  facetedge_id fe = get_facet_fe(f_id);
  facetedge_id next_fe = fe;

  if ( !valid_id(fe) ) return 0;
  do
  { n++;
    next_fe = get_next_edge(next_fe);
    if ( n > 2*web.skel[EDGE].count )
    { sprintf(errmsg,
         "Internal error: facet_valence() in infinite loop on facet %s.\n",
           ELNAME(f_id));
      kb_error(2481,errmsg,RECOVERABLE);
    }
  }
  while ( valid_id(next_fe) && (next_fe != fe) );
  return n;
}

/***************************************************************
*
*  Function:  get_body_valence
*
*  Purpose:  return number of facets around a body
*
*/

int get_body_valence(b_id)
body_id b_id;
{
  int n = 0;
  facet_id f_id = get_body_facet(b_id);
  facet_id next_f = f_id;

  if ( !valid_id(f_id) ) return 0;
  if ( web.representation == STRING ) return get_facet_valence(f_id);
/*
  if ( bfacet_timestamp < top_timestamp )
  { make_bfacet_lists();
    bfacet_timestamp = top_timestamp;
  }
*/
  do
  { n++;
    next_f = get_next_body_facet(next_f);
  }
  while ( (next_f != f_id) && (n < 3*web.skel[FACET].count) );
  return n;
}

/*********************************************************************
*
* function: set_facet_vertices()
*
* purpose: Fill in vertex lists of facets.
*/
void set_facet_vertices ARGS((void));
void set_facet_vertices()
{
  /* have to make expandable v list for quadratic mode */
}
/****************************************************************
*
*  function: begin_normal_motion()
*
*  purpose: calculates vertex normals as volume gradients, normalized to 
*           unit length.
*
*/

void begin_normal_motion()
{
  vertex_id v_id;
  facetedge_id fe;
  MAT2D(normals,MAXCOORD,MAXCOORD);
  int i;
  pt_type *vn;
  int normcount;
 
  if ( vertex_normals != NULL ) myfree((char*)vertex_normals); 
  vertex_normals = (pt_type *)mycalloc(
     web.skel[VERTEX].max_ord+1,sizeof(pt_type));

  FOR_ALL_VERTICES(v_id)
  { REAL mag;
    fe = get_vertex_fe(v_id);
    if ( !valid_id(fe) ) continue;
    normcount = new_calc_vertex_normal(v_id,normals);
    project_vertex_normals(v_id,normals,normcount);
    vn = vertex_normals + loc_ordinal(v_id);
    mag = sqrt(dot(normals[0],normals[0],SDIM));
    if ( mag != 0.0 )
    for ( i = 0 ; i < SDIM ; i++ )
      (*vn)[i] = normals[0][i]/mag;
  }
  normal_motion_flag = 1;
}

/*****************************************************************
*
* function: end_normal_motion()
*
* purpose: free memory used in normal motion mode
*
*/

void end_normal_motion()
{
  if ( vertex_normals != NULL ) myfree((char*)vertex_normals);
  vertex_normals = NULL;
  normal_motion_flag = 0;
}



/***********************************************************
*
*  Function: vertex_total_vol_grad()
*
*  Purpose: Calculate the gradient of combined body volume at a vertex.
*           Meant for triple lines and such in surface diffusion.
*
*  Input:  vertex id
*          norm -  pointer to place for normal vector
*
*
*  Output:  Average normal, normalized to unit length.
*
*  Return value: Length of original normal, i.e. volume gradient.
*/

REAL vertex_total_vol_grad(v_id,norm)
vertex_id v_id;
REAL *norm;
{
  int i,j;
  REAL y[MAXCOORD],z[MAXCOORD];
  REAL fnorm[MAXCOORD];
  facetedge_id fe_id,first_fe;
  REAL size;
  
  /* check for string */
  if ( web.representation == STRING )
  { edge_id e_id,start_e;
    
    norm[0] = norm[1] = 0.0;

    start_e = e_id = get_vertex_edge(v_id);
    do 
    { facetedge_id fe,fe2;
      facet_id f_id;

      get_edge_side(e_id,y);
      fe = get_edge_fe(e_id);
      if ( valid_id(fe) )
      { fe2 = get_next_facet(fe);
        if ( equal_id(fe,fe2) ) 
        { /* valence 1, so count it according to facet orientation */
          f_id = get_fe_facet(fe);
          if ( inverted(f_id) )
          { norm[0] -= y[1];
            norm[1] += y[0];
          }
          else
          { norm[0] += y[1];
            norm[1] -= y[0];
          }
        }
      }
      e_id = get_next_tail_edge(e_id);   
    } while ( !equal_id(e_id,start_e) );
    goto normalize;
  }

  /* Get here only for soapfilm model. */

  first_fe = fe_id = get_vertex_first_facet(v_id);
  for ( i = 0 ; i < SDIM ; i++ ) norm[i] = 0.0;
  do
  { facet_id f_id = get_fe_facet(fe_id);
    body_id b_id = get_facet_body(f_id);
    body_id bb_id = get_facet_body(invert(f_id));
    if ( valid_id(b_id) != valid_id(bb_id) )
    { /* one-sided, so count it */  
      get_fe_side(fe_id,z);
      get_fe_side(get_prev_edge(fe_id),y);
      cross_prod(y,z,fnorm);
      if ( valid_id(b_id) )
        for ( i = 0 ; i < SDIM ; i++ ) norm[i] += fnorm[i];
      else
        for ( i = 0 ; i < SDIM ; i++ ) norm[i] -= fnorm[i];

    }
    fe_id = get_next_vertex_facet(v_id,fe_id);
  } while ( !equal_id(fe_id,first_fe) );

  normalize:
  if ( get_vattr(v_id) & CONSTRAINT )
    {
      conmap_t * conmap = get_v_constraint_map(v_id);
      int oncount = 0,hitcount;
      struct constraint *con[MAXCONPER];
      int conlist[MAXCONPER];
      REAL perp[MAXCOORD];

      for ( j = 1 ; j <= (int)conmap[0] ; j++ )
        { 
          if ( conmap[j] & CON_HIT_BIT )
          { conlist[oncount] = conmap[j];
            con[oncount++] = get_constraint(conmap[j]);
          }
        }

      hitcount = constr_proj(TANGPROJ,oncount,con,get_coord(v_id),
                 norm,perp,conlist,DETECT,v_id);
      if ( hitcount != oncount )
        { clear_v_constraint_status(v_id);
          for ( j = 0 ; j < hitcount ; j++ )
             set_v_constraint_status(v_id,conlist[j]);
        }
      for ( j = 0 ; j < SDIM ; j++ )
        norm[j] -= perp[j];
     }
      
  size = sqrt(SDIM_dot(norm,norm));
  if ( size <= 0.0 ) return 0.0; 
  for ( i = 0 ; i < SDIM ; i++ ) norm[i] /= size;
  return size/web.simplex_factorial;
}


/***********************************************************
*
*  Function: calc_vertex_normal
*
*  Purpose: Calculate average normal of facets around a vertex.
*           Only does the facets
*          in the same face as given facet; will not cross
*          multiple edges.
*
*  Input:    vertex id
*          facet-edge id - with tail at vertex, for defining face
*          norm -  pointer to place for normal vector
*
*
*  Output:  Average normal, normalized to unit length.
*
*  Return value: Length of original normal, i.e. volume gradient.
*/

REAL calc_vertex_normal(v_id,fe,norm)
vertex_id v_id;
facetedge_id fe;
REAL *norm;
{
  int i,j;
  REAL y[MAXCOORD],z[MAXCOORD];
  REAL fnorm[MAXCOORD];
  facetedge_id sidea;
  struct boundary *bdry;  /* to check staying on same boundary */
  REAL size;


  /* check for string */
  if ( web.representation == STRING )
  { edge_id e_id,ee_id;
    e_id = get_vertex_edge(v_id);
    ee_id = get_next_tail_edge(e_id);

    get_edge_side(e_id,z);
    get_edge_side(ee_id,y);
    if ( equal_id(e_id,ee_id) ) { norm[0] = y[1]; norm[1] = -y[0]; }
    else { norm[0] = y[1] - z[1]; norm[1] = z[0] - y[0]; }
    if (inverted(e_id)) { norm[0] = -norm[0]; norm[1] = -norm[1];}
    goto normalize;
  }

  if ( !valid_id(fe) )
  { fe = get_vertex_fe(v_id);
    if ( !valid_id(fe) )
    { for ( i = 0 ; i < SDIM ; i++ ) norm[i] = 0.0;
      return 0.0;
    }
  }

  for ( i = 0 ; i < SDIM ; i++ ) norm[i] = 0.0;
  bdry = get_facet_boundary(get_fe_facet(fe));  /* original boundary */

  /* go around one way to edge of face */
  sidea = fe;
  do
  {
    get_fe_side(sidea,z);
    get_fe_side(get_prev_edge(sidea),y);
    cross_prod(y,z,fnorm);
    for ( i = 0 ; i < SDIM ; i++ ) norm[i] += fnorm[i];

    /* go to next facet */
    sidea = get_prev_edge(sidea);
    if ( equal_id(sidea,get_next_facet(sidea)) ) break;  /* edge */
    sidea = fe_inverse(get_next_facet(sidea));
    if ( bdry != get_facet_boundary(get_fe_facet(sidea)) ) break;
    if ( equal_id(sidea,fe) ) goto onormalize; /* went all the way around */
  }
  while ( equal_id(get_next_facet(sidea),get_prev_facet(sidea)) );

  /* go around the other way */
  sidea = fe;
  while ( equal_id(get_next_facet(sidea),get_prev_facet(sidea)) )
  {
    if ( equal_id(sidea,get_next_facet(sidea)) ) break;  /* edge */
    sidea = fe_inverse(get_next_facet(sidea));
    sidea = get_next_edge(sidea);
    if ( bdry != get_facet_boundary(get_fe_facet(sidea)) ) break;

    get_fe_side(sidea,z);
    get_fe_side(get_prev_edge(sidea),y);
    cross_prod(y,z,fnorm);
    for ( i = 0 ; i < SDIM ; i++ ) norm[i] += fnorm[i];
  }

  /* orient */
  onormalize:
  if ( inverted(get_fe_facet(fe)) )
      for ( i = 0 ; i < SDIM ; i++ ) norm[i] = -norm[i];

  normalize:
  if ( get_vattr(v_id) & CONSTRAINT )
    {
      conmap_t * conmap = get_v_constraint_map(v_id);
      int oncount = 0,hitcount;
      struct constraint *con[MAXCONPER];
      int conlist[MAXCONPER];
      REAL perp[MAXCOORD];

      for ( j = 1 ; j <= (int)conmap[0] ; j++ )
        { 
          if ( conmap[j] & CON_HIT_BIT )
          { conlist[oncount] = conmap[j];
            con[oncount++] = get_constraint(conmap[j]);
          }
        }

      hitcount = constr_proj(TANGPROJ,oncount,con,get_coord(v_id),
                 norm,perp,conlist,DETECT,v_id);
      if ( hitcount != oncount )
        { clear_v_constraint_status(v_id);
          for ( j = 0 ; j < hitcount ; j++ )
             set_v_constraint_status(v_id,conlist[j]);
        }
      for ( j = 0 ; j < SDIM ; j++ )
        norm[j] -= perp[j];
     }
      
  size = sqrt(SDIM_dot(norm,norm));
  if ( size <= 0.0 ) return 0.0; 
  for ( i = 0 ; i < SDIM ; i++ ) norm[i] /= size;
  return size/web.simplex_factorial;
}

/***********************************************************
*
*  Function: calc_vertex_smooth_normal
*
*  Purpose: Calculate average normal of facets around a vertex
*          for grapics normal interpolation.  Only does the facets
*          in the same face as given facet; will not cross
*          multiple edges, or boundary or constraint edges
*          when different from facet.  Only uses first 3 dimensions.
*          Does not project normal to constraints.
*
*  Input:    vertex id
*          facet-edge id - with tail at vertex, for defining face
*          norm -  pointer to place for normal vector
*
*
*  Output:  Average normal, normalized to unit length.
*
*  Return value: none.
*/

void calc_vertex_smooth_normal(v_id,fe,norm)
vertex_id v_id;
facetedge_id fe;
REAL *norm;
{
  int i;
  REAL y[MAXCOORD],z[MAXCOORD];
  REAL fnorm[MAXCOORD];
  facetedge_id sidea;
  facet_id f_id = get_fe_facet(fe);
  struct boundary *bdry;  /* to check staying on same boundary */
  REAL size;


  /* check for string */
  if ( web.representation != SOAPFILM )
  { kb_error(2208,"Internal error: Using calc_vertex_smooth_normal() on non-soapfilm surface.\n",
     RECOVERABLE); 
  }

  if ( !valid_id(fe) )
     kb_error(2209,"Internal error: Invalid fe in calc_vertex_smooth_normal().\n",RECOVERABLE);

  bdry = get_facet_boundary(f_id);  /* original boundary */

  get_facet_normal(f_id,norm);  /* starter normal */

  /* go around one way to edge of face */
  sidea = fe;
  for(;;)
  { edge_id e_id;
    /* go to next facet */
    sidea = get_prev_edge(sidea);
    if ( equal_id(sidea,get_next_facet(sidea)) ) break;  /* valence 1 */
    if ( !equal_id(get_next_facet(sidea),get_prev_facet(sidea)) ) break;
    e_id = get_fe_edge(sidea);
    if ( !equal_constr(f_id,e_id) ) break;
    if ( bdry != get_edge_boundary(e_id) ) break;
    sidea = fe_inverse(get_next_facet(sidea));
    if ( bdry != get_facet_boundary(get_fe_facet(sidea)) ) break;
    if ( equal_id(sidea,fe) ) goto snormalize; /* went all the way around */

    get_fe_side(sidea,z);
    get_fe_side(get_prev_edge(sidea),y);
    cross_prod(y,z,fnorm);
    for ( i = 0 ; i < SDIM ; i++ ) norm[i] += fnorm[i]/2;

  }

  /* go around the other way */
  sidea = fe;
  while ( equal_id(get_next_facet(sidea),get_prev_facet(sidea)) )
  { edge_id e_id;
    if ( equal_id(sidea,get_next_facet(sidea)) ) break;  /* edge */
    e_id = get_fe_edge(sidea);
    if ( !equal_constr(f_id,e_id) ) break;
    if ( bdry != get_edge_boundary(e_id) ) break;
    sidea = fe_inverse(get_next_facet(sidea));
    sidea = get_next_edge(sidea);
    if ( bdry != get_facet_boundary(get_fe_facet(sidea)) ) break;

    get_fe_side(sidea,z);
    get_fe_side(get_prev_edge(sidea),y);
    cross_prod(y,z,fnorm);
    for ( i = 0 ; i < SDIM ; i++ ) norm[i] += fnorm[i]/2;
  }

  snormalize:

  size = sqrt(SDIM_dot(norm,norm));
  if ( size <= 0.0 ) return; 
  for ( i = 0 ; i < SDIM ; i++ ) norm[i] /= size;
}

/***************************************************************************
*
* function: lagrange_edge_normal()
*
* purpose: compute geometric normal space at a lagrange point of an edge.
*/
int lagrange_edge_normal ARGS((vertex_id,edge_id,REAL**));
int lagrange_edge_normal(v_id,e_id,norm)
vertex_id v_id;
edge_id e_id;
REAL **norm;
{
  MAT2D(vx,MAXVCOUNT,MAXCOORD);
  struct gauss_lag *gl = &gauss_lagrange[1][web.gauss1D_order];
  vertex_id *v = get_edge_vertices(e_id);
  int k,m,i;
  int ctrl = web.skel[EDGE].ctrlpts;
  REAL tang[MAXCOORD],*t=tang;

  get_edge_verts(e_id,vx,NULL);
  for ( k = 0 ; k < ctrl ; k++ ) if ( v[k] == v_id ) break;
#ifdef VOLUMEGRAD
  /* volume gradient */
  norm[0] = norm[1] = 0.0;
  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL y,dx;
    for ( y = 0.0, dx = 0.0, j = 0 ; j < ctrl ; j++ )
    { y += gl->gpoly[m][j]*vx[j][1];
      dx += gl->gpolypart[m][0][j]*vx[j][0];
    }
    norm[0] -= gl->gausswt[m]*y*gl->gpolypart[m][0][k];
    norm[1] -= gl->gausswt[m]*dx*gl->gpoly[m][k];
  }
#else
    /* geometric normal */
    for ( i = 0 ; i < SDIM ; i++ ) tang[i] = 0.0;
    for ( m = 0 ; m < ctrl ; m++ )
    { REAL prime;
      prime = gl->lpolypart[k][0][m]; 
      for ( i = 0 ; i < SDIM ; i++ )
      tang[i] += prime*vx[m][i];
    }
    return kernel_basis_rows(&t,norm,1,SDIM);
#endif
}

/***************************************************************************
*
* function: lagrange_facet_normal()
*
* purpose: compute geometric normal at a lagrange point of a facet.
*/
int lagrange_facet_normal ARGS((vertex_id,facet_id,REAL**));
int lagrange_facet_normal(v_id,f_id, norm)
vertex_id v_id;
facet_id f_id;
REAL **norm;
{ 
  vertex_id *v = get_facet_vertices(f_id);
  struct gauss_lag *gl = &gauss_lagrange[web.dimension][web.gauss2D_order];
  MAT2D(sides,MAXCOORD,MAXCOORD);
  MAT2D(vx,MAXVCOUNT,MAXCOORD);
  int ctrl = web.skel[FACET].ctrlpts;
  int i,j,k,kk;

  get_facet_verts(f_id,vx,NULL);
  for ( k = 0 ; k < ctrl ; k++ ) if ( v[k] == v_id ) break;
  if ( k >= ctrl )
     { sprintf(errmsg,
          "Internal error: Can't find vertex %s in facet %s.\n",ELNAME(v_id),
             ELNAME1(f_id));
       kb_error(2210,errmsg,RECOVERABLE); 
     }

  for ( j = 0 ; j < SDIM ; j++ ) sides[0][j] = 0.0;
  for ( i = 1 ; i <= web.dimension ; i++ )
    for ( j = 0 ; j < SDIM ; j++ )
      for ( kk = 0, sides[i][j] = 0.0 ; kk < ctrl ; kk++ )
         sides[i][j] += gl->lpolypart[k][i-1][kk]*vx[kk][j];
  if ( inverted(f_id) ) 
     for ( j = 0 ; j < SDIM ; j++ ) sides[1][j] = -sides[1][j];
  return kernel_basis_rows(sides+1,norm,web.dimension,SDIM);
}

/***************************************************************************
*
* function: quadratic_edge_normal()
*
* purpose: compute geometric normal space at a point of a quadratic edge.
*/

REAL quad_tang_coeffs[3][3] = { { -1.5,2.0,-0.5},
       {-0.5,0.0,0.5},{ 0.5,-2.0,1.5}};
int quadratic_edge_normal ARGS((vertex_id,edge_id,REAL**));
int quadratic_edge_normal(v_id,e_id,norm)
vertex_id v_id;
edge_id e_id;
REAL **norm;
{
  MAT2D(vx,MAXVCOUNT,MAXCOORD);
  vertex_id *v = get_edge_vertices(e_id);
  int k,m,i;
  int ctrl = web.skel[EDGE].ctrlpts;  /* should be 3 */
  REAL tang[MAXCOORD],*t=tang;

  get_edge_verts(e_id,vx,NULL);  /* has midv in vx[1] */
  for ( k = 0 ; k < ctrl ; k++ ) if ( v[k] == v_id ) break;
  if ( k != 0 ) k = 3 - k; /* switch vertices 1 and 2 compared to lagrange */

  /* geometric normal */
  for ( i = 0 ; i < SDIM ; i++ ) tang[i] = 0.0;
  for ( m = 0 ; m < ctrl ; m++ )
  { REAL prime;
    prime = quad_tang_coeffs[k][m]; 
    for ( i = 0 ; i < SDIM ; i++ )
    tang[i] += prime*vx[m][i];
  }
  return kernel_basis_rows(&t,norm,1,SDIM);

}

/***************************************************************************
*
* function: quadratic_facet_normal()
*
* purpose: compute geometric normal at a lagrange point of a facet.
*/
int qlconvert[6] = { 0, 1, 2, 4, 5, 3}; /* index conversion */
int lqconvert[6] = { 0, 1, 2, 5, 3, 4}; /* index conversion */
int quadratic_facet_normal ARGS((vertex_id,facet_id,REAL**));
int quadratic_facet_normal(v_id,f_id, norm)
vertex_id v_id;
facet_id f_id;
REAL **norm;
{
  vertex_id v[6];
  struct gauss_lag *gl = &gauss_lagrange[web.dimension][web.gauss2D_order];
  MAT2D(sides,MAXCOORD,MAXCOORD);
  MAT2D(vx,MAXVCOUNT,MAXCOORD);
  int ctrl = web.skel[FACET].ctrlpts;
  int i,j,k,kk;
  facetedge_id fe_id;

  fe_id = get_facet_fe(f_id);
  for ( i = 0 ; i < FACET_EDGES ; i++ )
     { v[qlconvert[2*i]] = get_fe_tailv(fe_id);
       v[qlconvert[2*i+1]] = get_fe_midv(fe_id);
       fe_id = get_next_edge(fe_id);
     }
  get_facet_verts(f_id,vx,NULL);
  for ( k = 0 ; k < ctrl ; k++ ) if ( v[k] == v_id ) break;
  if ( k >= ctrl )
     { sprintf(errmsg,"Internal error: Can't find vertex %s in facet %s.\n",
           ELNAME(v_id), ELNAME1(f_id));
       kb_error(2211,errmsg,RECOVERABLE); 
     }

  for ( j = 0 ; j < SDIM ; j++ ) sides[0][j] = 0.0;
  for ( i = 1 ; i <= web.dimension ; i++ )
    for ( j = 0 ; j < SDIM ; j++ )
      for ( kk = 0, sides[i][j] = 0.0 ; kk < ctrl ; kk++ )
        sides[i][j] += gl->lpolypart[k][i-1][kk]*vx[lqconvert[kk]][j];
  return kernel_basis_rows(sides+1,norm,web.dimension,SDIM);
}

/***********************************************************
*
*  Function: new_calc_vertex_normal
*
*  Purpose: Calculate subspace normal to surface for purposes
*          of Hessian normal or normal motion.  Normal space
*          is essentially that spanned by the volume normals
*          of all adjacent bodies.  Idea is to rule out 
*          purely tangential motions.
*
*  Input:    vertex id
*          norm -  pointer to place for normal basis
*
*
*  Output:  Basis for normal space if not full dimension
*  Return value: Dimension of normal
*/

int new_calc_vertex_normal(v_id,norm)
vertex_id v_id;
REAL **norm; /* returned basis */
{
  int i,j,kk;
  REAL y[MAXCOORD],z[MAXCOORD];
  MAT2D(fnorm,MAXCOORD,MAXCOORD);
  MAT2D(a,2,MAXCOORD);
  facetedge_id sidea,sideb,ffe;
  struct boundary *bdry;  /* to check staying on same boundary */
  REAL size;
  int retval = 0;
  facetedge_id fe;
  int valence,bares=0,singles=0,doubles=0,triples=0,bareflag=0;
  int vattr;

  if (web.representation == SIMPLEX ) return simplex_vertex_normal(v_id,norm);

  vattr = get_vattr(v_id);

  if ( hessian_special_normal_flag )
  { 
    REAL *x = get_coord(v_id); 
    for ( i = 0 ; i < SDIM ; i++ )
      norm[0][i] = eval(hessian_special_normal_expr+i,x,v_id,NULL);
    size = sqrt(SDIM_dot(norm[0],norm[0]));
    if ( size == 0.0 )
    { sprintf(errmsg, "hessian_special_normal_vector is zero at vertex %s.\n",
             ELNAME(v_id));
      kb_error(2212,errmsg,RECOVERABLE);
    }
    for ( i = 0 ; i < SDIM ; i++ ) norm[0][i] = norm[0][i]/size;
    return 1; /* one-dimensional normal only */
  }                             

  if ( hessian_normal_one_flag )
  { MAT2D(mat,MAXCOORD,MAXCOORD);
    facet_id f_id;
    edge_id e_id, starte;

    for ( i = 0 ; i < SDIM ; i++ )
      for ( j = 0 ; j < SDIM ; j++ )
         mat[i][j] = 0.0;
    if ( web.representation == SOAPFILM )
    { facetedge_id fe_id,startfe; 
      fe_id = startfe = get_vertex_first_facet(v_id);
      do 
      { f_id = get_fe_facet(fe_id);
        get_facet_normal(f_id,fnorm[0]);
        size = SDIM_dot(fnorm[0],fnorm[0]);
        for ( i = 0 ; i < SDIM ; i++ )
           for ( j = 0 ; j < SDIM ; j++ )
              mat[i][j] += fnorm[0][i]*fnorm[0][j]/size;
        fe_id = get_next_vertex_facet(v_id,fe_id);
      } while ( !equal_element(fe_id,startfe) );
    }
    else if ( web.representation == STRING )
    { 
      e_id = starte = get_vertex_edge(v_id);
      do 
      { get_edge_side(e_id,fnorm[0]);
        mat[0][0] += fnorm[0][1]*fnorm[0][1];
        mat[0][1] -= fnorm[0][1]*fnorm[0][0];
        mat[1][0] -= fnorm[0][0]*fnorm[0][1];
        mat[1][1] += fnorm[0][0]*fnorm[0][0];
        e_id = get_next_tail_edge(e_id);
      } while ( !equal_element(e_id,starte) );
    }

    /* find eigenvector */
    for ( kk = 0 ; kk < 10 ; kk++ )
    { matvec_mul(mat,fnorm[0],y,SDIM,SDIM);
      matvec_mul(mat,y,fnorm[0],SDIM,SDIM);
    }

    size = sqrt(SDIM_dot(fnorm[0],fnorm[0]));
    for ( i = 0 ; i < SDIM ; i++ ) norm[0][i] = fnorm[0][i]/size;
    return 1;
  } /* end hessian_normal_one_flag */

  if ( vattr & Q_MIDFACET ) 
  { /* Lagrange model */
     facet_id f_id = get_vertex_facet(v_id);
     retval = lagrange_facet_normal(v_id,f_id,norm);
     goto ncn_exit;
  }

  /* Want to check for bare edges in SOAPFILM model */
  /* so can handle same way as string edges  */
  if ( web.representation == SOAPFILM )
  { edge_id e_id,start_e;

    e_id = get_vertex_edge(v_id);
    if ( !valid_id(e_id) ) goto full_dim; /* bare point */

    start_e = e_id;
    do 
    { valence = get_edge_valence(e_id);
      if ( valence == 0 ) bares++;
      if ( valence == 1 ) singles++;
      if ( valence == 2 ) doubles++;
      if ( valence >= 3 ) triples++;
      if ( vattr & (Q_MIDPOINT|Q_MIDEDGE) ) break;
      e_id = get_next_tail_edge(e_id);
    } while ( !equal_id(e_id,start_e) );

    if ( triples >= SDIM ) goto full_dim;
    if ( singles >= SDIM ) goto full_dim;
    if ( bares >= SDIM ) goto full_dim;
    if ( bares && (singles+doubles+triples) ) goto full_dim;
    if ( triples >= 1 )
    { if ( vattr & (Q_MIDPOINT|Q_MIDEDGE) ) bareflag = 1; /* effectively */
      else goto tripletest;
    }
    if ( (singles==0) && (doubles==0) )
      bareflag = 1;
  }
    
  if ( (vattr & Q_MIDEDGE) && (bareflag || (web.representation == STRING)) )
  { 
     edge_id e_id = get_vertex_edge(v_id);
     retval = lagrange_edge_normal(v_id,e_id,norm);
     goto ncn_exit;
  }

  if ( (vattr & Q_MIDPOINT) && (bareflag || (web.representation == STRING)) )
  { 
     edge_id e_id = get_vertex_edge(v_id);
     retval = quadratic_edge_normal(v_id,e_id,norm);
     goto ncn_exit;
  }

  /* check for string */
  if ( web.representation == STRING )
  { /* Must be endpoint of string edge */
    edge_id e_id,ee_id,eee_id;
    e_id = get_vertex_edge(v_id);
    if ( !valid_id(e_id) ) goto full_dim;
    ee_id = get_next_tail_edge(e_id);
    eee_id = get_next_tail_edge(ee_id);

    if ( !equal_id(e_id,eee_id)  )
       goto full_dim;  /* triple vertex at least */

    if ( (web.modeltype == LAGRANGE) && (web.lagrange_order >= 2) )
       retval = lagrange_edge_normal(v_id,e_id,norm);
    else if ( web.modeltype == QUADRATIC )
    { MAT2D(vx,3,MAXCOORD);
      REAL *t = y;
      get_edge_verts(e_id,vx,NULL);
      for ( i = 0 ; i < SDIM ; i++ ) 
         y[i] = -1.5*vx[0][i] + 2*vx[1][i] - 0.5*vx[2][i];
      retval = kernel_basis_rows(&t,norm,1,SDIM);
    }
    else 
    { REAL *t = y;
      get_edge_side(e_id,y);
      if ( !equal_id(e_id,ee_id ) )
      { get_edge_side(ee_id,z);
        for ( i = 0 ; i < SDIM ; i++ ) y[i] -= z[i];
      }
      retval = kernel_basis_rows(&t,norm,1,SDIM);
    }
    if ( inverted(e_id) )
      for ( i = 0 ; i < SDIM ; i++ ) norm[0][i] = -norm[0][i];

    goto ncn_exit;    
  }

  /* now have 2D facet */
  fe = get_vertex_fe(v_id);
  if ( !valid_id(fe) ) 
     goto full_dim; 
  bdry = get_facet_boundary(get_fe_facet(fe));  /* original boundary */

  sidea = fe;
  if ( vattr & Q_MIDPOINT )
  { /* quadratic model */
    /* already tested for bare or triple edges, so one or two facets */
    /* If two, combine normals */
     MAT2D(norm2,MAXCOORD,MAXCOORD);
     facet_id f_id = get_fe_facet(sidea);
     if ( inverted(f_id) )
        sidea = inverse_id(sidea); 
     if ( !equal_id(get_next_facet(sidea),get_prev_facet(sidea)) )
       goto tripletest;
     retval = quadratic_facet_normal(v_id,get_fe_facet(sidea),norm);
     sideb = get_next_facet(sidea); 
     if ( equal_id(sidea,sideb) ) goto ncn_exit;
     sideb = inverse_id(sideb);
     retval = quadratic_facet_normal(v_id,get_fe_facet(sideb),norm2);
     for ( i = 0 ; i < retval ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
           norm[i][j] += norm2[i][j];
     goto ncn_exit;
  }
  if ( vattr & Q_MIDEDGE )
  { /* Lagrange model */
     MAT2D(norm2,MAXCOORD,MAXCOORD);
     facet_id f_id = get_fe_facet(sidea);
     if ( inverted(f_id) )
        sidea = inverse_id(sidea); 
     if ( !equal_id(get_next_facet(sidea),get_prev_facet(sidea)) )
       goto tripletest;
     retval = lagrange_facet_normal(v_id,get_fe_facet(sidea),norm);
     sideb = get_next_facet(sidea); 
     if ( equal_id(sidea,sideb) ) goto ncn_exit;
     sideb = inverse_id(sideb);
     retval = lagrange_facet_normal(v_id,get_fe_facet(sideb),norm2);
     for ( i = 0 ; i < retval ; i++ )
         for ( j = 0 ; j < SDIM ; j++ )
             norm[i][j] += norm2[i][j];
     goto ncn_exit;
  }
  /* usual case of facet in N dim */
  /* if get here, know no triple lines */
  { for ( j = 0 ; j < SDIM ; j++ )
       for ( i = 0 ; i < SDIM ; i++ ) norm[j][i] = 0.0;

    /* go around one way to edge of face */
    do
     { 
       if ( vattr & AXIAL_POINT )
       { get_fe_side(sidea,z);
         get_fe_side(get_next_edge(sidea),y);
         cross_prod(y,z,fnorm[1]);
         retval = 1;
       }
       else
       { 
         switch ( web.modeltype )
         { case LINEAR: 
              get_fe_side(sidea,a[0]);
              get_fe_side(inverse_id(get_prev_edge(sidea)),a[1]);
              if ( SDIM == 3 )
              { cross_prod(a[0],a[1],fnorm[0]);
                retval = 1;
              } 
              else
                retval = kernel_basis_rows(a,fnorm,2,SDIM);
              break;
           case QUADRATIC: 
             retval = quadratic_facet_normal(v_id,get_fe_facet(sidea),fnorm);
             break;
           case LAGRANGE: 
             retval = lagrange_facet_normal(v_id,get_fe_facet(sidea),fnorm);  
             break;
         }
       }
       for ( j = 0 ; j < retval ; j++ )
        for ( i = 0 ; i < SDIM ; i++ ) 
         norm[j][i] += fnorm[j][i];

       /* go to next facet */
       sidea = get_prev_edge(sidea);
       if ( equal_id(sidea,get_next_facet(sidea)) ) break;  /* edge */
       sidea = fe_inverse(get_next_facet(sidea));
       if ( bdry != get_facet_boundary(get_fe_facet(sidea)) ) break;
       if ( equal_id(sidea,fe) )
       { if ( inverted(get_fe_facet(sidea)) )
            for ( i = 0 ; i < SDIM ; i++ ) norm[0][i] = -norm[0][i];
         goto ncn_exit; /* went all the way around */
       }
     }
     while ( equal_id(get_next_facet(sidea),get_prev_facet(sidea)) );
  }
  /* go around the other way */
  sidea = fe;
  while ( equal_id(get_next_facet(sidea),get_prev_facet(sidea)) )
  {
    if ( equal_id(sidea,get_next_facet(sidea)) ) break;  /* edge */
    sidea = fe_inverse(get_next_facet(sidea));
    sidea = get_next_edge(sidea);
    if ( bdry != get_facet_boundary(get_fe_facet(sidea)) ) break;

    get_fe_side(sidea,z);
    if ( get_vattr(v_id) & AXIAL_POINT )
    { REAL *u = fnorm[0];       
      switch ( web.modeltype )
      { case LINEAR: get_fe_side(get_next_edge(sidea),y);
                       cross_prod(y,z,fnorm[0]); break;
        case QUADRATIC: 
          quadratic_facet_normal(v_id,get_fe_facet(sidea),&u);
          break;
        case LAGRANGE: 
          lagrange_facet_normal(v_id,get_fe_facet(sidea),&u);  
          break;
      }
    }
    else
    { 
         switch ( web.modeltype )
         { case LINEAR: 
              get_fe_side(sidea,a[0]);
              get_fe_side(inverse_id(get_prev_edge(sidea)),a[1]);
              if ( SDIM == 3 )
              { cross_prod(a[0],a[1],fnorm[0]);
                retval = 1;
              } 
              else
                retval = kernel_basis_rows(a,fnorm,2,SDIM);
              break;
           case QUADRATIC: 
             retval = quadratic_facet_normal(v_id,get_fe_facet(sidea),fnorm);
             break;
           case LAGRANGE: 
             retval = lagrange_facet_normal(v_id,get_fe_facet(sidea),fnorm);  
             break;
         }
    }
    for ( j = 0 ; j < retval ; j++ )
      for ( i = 0 ; i < SDIM ; i++ ) 
        norm[j][i] += fnorm[j][i];
  }

  if ( !equal_id(get_next_facet(sidea),get_prev_facet(sidea)) )
     goto tripletest;
      if( inverted(get_fe_facet(sidea)) )
         for ( i = 0 ; i < SDIM ; i++ ) norm[0][i] = -norm[0][i];

  goto ncn_exit;

tripletest:
  /* if get here, know there are one or two triple edges into vertex */
  { /* triple line */
     REAL *t = fnorm[0];
     int triple_count = 0;
     facetedge_id start_e;
     edge_id e_id;

     for ( i = 0 ; i < SDIM ; i++ ) fnorm[0][i] = 0.;
     start_e = e_id = get_vertex_edge(v_id);
     do 
     { int valence = get_edge_valence(e_id);
       if ( valence >= 3 )  
       { if ( web.modeltype==QUADRATIC )
         { retval = quadratic_edge_normal(v_id,e_id,norm);
           goto ncn_exit; /* lazy; just use first triple */
         }
         if ( web.modeltype==LAGRANGE )
         { retval = lagrange_edge_normal(v_id,e_id,norm);
           goto ncn_exit; /* lazy; just use first triple */
         }
         /* accumulate tangents */
         ffe = get_edge_fe(e_id);
         if ( !equal_id(get_next_facet(ffe),get_prev_facet(ffe)) )
         { /* triple again */
            triple_count++;
            get_fe_side(ffe,z);
            switch ( triple_count )
            { case 1:
                for ( i = 0 ; i < SDIM ; i++ ) fnorm[0][i] += z[i]; break;
              case 2:
                for ( i = 0 ; i < SDIM ; i++ ) fnorm[0][i] -= z[i]; break;
              default: goto full_dim;  /* more than 2 */
            }
         }
       }
       e_id = get_next_tail_edge(e_id);
     } while ( !equal_element(start_e,e_id) );
       retval = kernel_basis_rows(&t,norm,1,SDIM);
   }
   goto ncn_exit;
  
ncn_exit:

  return retval;
  
full_dim:
  for ( i = 0 ; i < SDIM ; i++ )
  { for ( j = 0 ; j < SDIM ; j++ )
       norm[i][j] = 0.0;
    norm[i][i] = 1.0;
  }
  return SDIM;
}

/*************************************************************************
*
* Function: project_vertex_normals()
*
* Purpose: Take incoming normal basis, project to constraints,
*          and orthonormalize.  Meant to be called after 
*          new_calc_vertex_normal().
*
* Return: Number of independent basis vectors.
*         Basis vectors altered in place.
*/

int project_vertex_normals(v_id,normals,normcount)
vertex_id v_id;
REAL **normals;
int normcount;
{ conmap_t *conmap;
  int oncount;
  MAT2D(grads,MAXCOORD,MAXCOORD);
  MAT2D(pp,MAXCOORD,MAXCOORD);
  MAT2D(qq,MAXCOORD,MAXCOORD);
  REAL dummy;
  int i,j,n;

  conmap = get_v_constraint_map(v_id);
  oncount = 0;
  if ( conmap[0] )
  { /* calculate projection matrix to fewer degrees of freedom */
    for ( j = 1; j <= (int)conmap[0] ; j++ )
    { struct constraint *constr;
      if ( !(conmap[j] & CON_HIT_BIT) ) continue;
      constr = get_constraint(conmap[j]);
      eval_all(constr->formula,get_coord(v_id),SDIM,
            &dummy, grads[oncount],v_id);
      oncount++;
    }
    if ( oncount > 0 )
    {
      /* project basis to constraints */
      oncount = gram_schmidt(grads,oncount,SDIM);

      mat_mul_tr(grads,normals,qq,oncount,SDIM,normcount);
      tr_mat_mul(grads,qq,pp,oncount,SDIM,normcount);
      for ( j = 0 ; j < normcount ; j++ )
        for ( i = 0 ; i < SDIM ; i++ )
           normals[j][i] -= pp[i][j];

    }
    /* orthonormalize  */
    for ( i = 0 ; i < normcount ; i++ )
    { /* subtract previous components */
      REAL sum;
      for ( j = 0 ; j < i ; j++ )
      { sum = dot(normals[i],normals[j],SDIM);
        for ( n = 0 ; n < SDIM ; n++ )
          normals[i][n] -= sum*normals[j][n];
      }
      /* normalize */
      sum = dot(normals[i],normals[i],SDIM);
      sum = sqrt(sum);
      if ( sum > hessian_epsilon )
         for ( n = 0 ; n < SDIM ; n++ ) normals[i][n] /= sum;
      else /* have to skip this direction */
      { for ( n = 0 ; n < SDIM ; n++ )
          normals[i][n] = normals[normcount-1][n];
        normcount--;
        i--;
      }
    }
  }
  return normcount; 
}



/***************************************************************
*
*  Function: get_facet_normal()
*
*  Purpose: find facet normal (length = area).
*          3D only
*
*/

void get_facet_normal(f_id,normal)
facet_id f_id;
REAL *normal;
{ facetedge_id fe;
  REAL side1[MAXCOORD],side2[MAXCOORD];
  int i;

  if ( web.representation == SIMPLEX )
    kb_error(1368,"Internal error: Trying to take normal in simplex model.\n",
       RECOVERABLE);

  if ( web.modeltype == LINEAR )  /* better for strange symmetries */
  { MAT2D(x,FACET_VERTS,MAXCOORD);
    get_facet_verts(f_id,x,NULL);  /* in tail order */
    for ( i = 0 ; i < SDIM ; i++ )
    { side1[i] = x[1][i] - x[0][i];
      side2[i] = x[2][i] - x[1][i];
    }
  }
  else
  { fe = get_facet_fe(f_id);
    get_edge_side(get_fe_edge(fe),side1);
    fe = get_next_edge(fe);
    get_edge_side(get_fe_edge(fe),side2);
  }
  cross_prod(side1,side2,normal);
  for ( i = 0 ; i < SDIM ; i++ )
     normal[i] /= 2;  /* triangle factor */
}
  
#ifdef NEWFACETNORMAL
attempt to do normal at arbitrary point in facet.  Incomplete
/***************************************************************
*
*  Function: new_get_facet_normal()
*
*  Purpose: find facet normal (length = area).
*          Any dimension, linear, quadratic, or Lagrange model.
*          Needs barycentric coords for quadratic or Lagrange.
*
*/

void new_get_facet_normal(f_id,normal,b)
facet_id f_id;
REAL *normal;
REAL *b; /* barycentric coords */
{ facetedge_id fe;
  MAT2D(x,MAXCOORD+1,MAXCOORD);
  int i;
  int m;
  MAT2D(sides,MAXCOORD,MAXCOORD);

  normal[i] /= 2;  /* triangle factor */

  if ( web.modeltype == LINEAR )
  {
    get_facet_verts(f_id,x,NULL);  /* in tail order */

    /* fan of sides from v0 */
    for ( i = 0 ; i < web.dimension ; i++ )
    for ( j = 0 ; j < SDIM ; j++ )
          sides[i][j] = x[i+1][j] - x[0][j];

    kernel_basis_rows(sides,&normal,web.dimension,SDIM);
  }
  else if ( web.modeltype == QUADRATIC )
  {
     get_facet_verts(f_id,x,NULL);  /* in tail order */
     /* tangents at point */
     mat_mult(gpolypartial[m],x,sides,web.dimension,FACET_CTRL,SDIM);
     kernel_basis_rows(sides,&normal,web.dimension,SDIM);
  }
  else if ( web.modeltype == LAGRANGE )
  {
     int ctrl = web.skel[FACET].ctrlpts;
     int dim = web.dimension;
     struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];
     get_facet_verts(f_id,x,NULL); 
     /* tangent vectors at gauss points */
     mat_mult(gl->gpolypart[m],x,sides,dim,ctrl,SDIM);
     kernel_basis_rows(sides,&normal,web.dimension,SDIM);
  }

}
#endif  
  
/***************************************************************
*
*  Function: get_edge_verts()
*
*  Purpose: to get all vertices of an edge, nicely 
*          displaced relative to first vertex of edge in case of symmetry gp
*          NOTE: Quadratic model: midv in verts[1]
*
*/

void get_edge_verts(e_id,verts,wraps)
edge_id e_id;
REAL **verts;    /* third spot NULL if don't want midpts in quadratic */
WRAPTYPE *wraps;  /* wraps of vertices; NULL if not wanted */
{
  int j,k;
  WRAPTYPE wrap = 0;
  REAL *x;

  if ( web.representation == SIMPLEX )
     kb_error(1369,"Internal error: Can't do get_edge_verts for simplex model.\n",RECOVERABLE);

  if ( web.modeltype == LAGRANGE )
  { vertex_id *v = get_edge_vertices(e_id);
     int n = web.skel[EDGE].ctrlpts;
     if ( web.symmetry_flag )
     {
      wrap = get_edge_wrap(e_id);
     }
     for ( j = 0 ; j < n ; j++ )
     { x = get_coord(v[j]);
       if ( wrap && (j > 0) && (inverted(e_id) || (j==n-1)))
         (*sym_wrap)(x,verts[j],wrap);
       else
       for ( k = 0 ; k < SDIM ; k++ )
         verts[j][k] = x[k];
     }
     return;
  }

  if ( web.symmetry_flag )
    {
      x = get_coord(get_edge_tailv(e_id));
      for ( j = 0 ; j < SDIM ; j++ ) verts[0][j] = x[j];
      wrap = get_edge_wrap(e_id);
      if ( (web.modeltype == QUADRATIC) && verts[2] ) 
      { if ( wraps ){wraps[0] = 0;  wraps[2] = wrap; }
         x = get_coord(get_edge_headv(e_id));
         (*sym_wrap)(x,verts[2],wrap);
         x = get_coord(get_edge_midv(e_id));
         if ( inverted(e_id) )
         { if ( wraps ) wraps[1] = wrap;
           (*sym_wrap)(x,verts[1],wrap);
         }
         else
         { for ( j = 0 ; j < SDIM ; j++ ) verts[1][j] = x[j];
           if ( wraps ) wraps[1] = 0;
         }
      } 
      else
      { if ( wraps ){wraps[0] = 0;  wraps[1] = wrap; }
         x = get_coord(get_edge_headv(e_id));
         (*sym_wrap)(x,verts[1],wrap);
      }
    }
  else
    { x = get_coord(get_edge_tailv(e_id));
      for ( j = 0 ; j < SDIM ; j++ ) verts[0][j] = x[j];
      x = get_coord(get_edge_headv(e_id));
      if ( (web.modeltype == QUADRATIC) && verts[2] ) 
      { for ( j = 0 ; j < SDIM ; j++ ) verts[2][j] = x[j];
        x = get_coord(get_edge_midv(e_id));
        for ( j = 0 ; j < SDIM ; j++ ) verts[1][j] = x[j];
      }
      else
       for ( j = 0 ; j < SDIM ; j++ ) verts[1][j] = x[j];
    }
}
  
/***************************************************************
*
*  Function: get_facet_verts()
*
*  Purpose: to get all three vertices of a facet, nicely 
*          displaced relative to first vertex of facet in case of symmetry gp
*
*/

void get_facet_verts(f_id,verts,wraps)
facet_id f_id;
REAL **verts;    /* fourth spot NULL if don't want midpts in quadratic */
WRAPTYPE *wraps;  /* wraps of vertices; NULL if not wanted */
{
  facetedge_id fe;
  int i,j;
  WRAPTYPE wrap,fewrap;
  REAL *x;
  
  if ( web.modeltype == LAGRANGE )
    { vertex_id *v = get_facet_vertices(f_id);
      int ctrlpts = web.skel[FACET].ctrlpts;
      int n = web.lagrange_order;

      f_id = positive_id(f_id); /* since vertices in particular order */

      for ( i = 0 ; i < ctrlpts ; i++ )
      { 
        x = get_coord(v[i]);
       
        for ( j = 0 ; j < SDIM ; j++ )
          verts[i][j] = x[j];
      }
      if ( web.symmetry_flag )
      {
        if ( wraps ) 
          for ( j = 0 ; j < ctrlpts ; j++ ) wraps[j] = 0;
    
        /* gotta go through the 3 edges */
        /* first edge */
        fe = get_facet_fe(f_id);
        wrap = get_fe_wrap(fe);
        if ( wrap && inverted(get_fe_edge(fe)) )
          for ( j = 1 ; j < n ; j++ )
          { (*sym_wrap)(get_coord(v[j]),verts[j],wrap);
             if ( wraps ) wraps[j] = wrap;
          }
        if ( wrap )
        { (*sym_wrap)(get_coord(v[n]),verts[n],wrap);
          if ( wraps ) wraps[n] = wrap ;
        }

        /* second edge */
        fe = get_next_edge(fe);
        if ( wrap && !inverted(get_fe_edge(fe)) )
          for ( j = 1 ; j < n ; j++ )
          { int m = n*(j+1) - j*(j-1)/2;
             (*sym_wrap)(get_coord(v[m]),verts[m],wrap);
             if ( wraps ) wraps[m] = wrap;
          }
        fewrap = get_fe_wrap(fe);
        wrap = (*sym_compose)(wrap,fewrap);
        if ( wrap && inverted(get_fe_edge(fe)) )
          for ( j = 1 ; j < n ; j++ )
          { int m = n*(j+1) - j*(j-1)/2;
             (*sym_wrap)(get_coord(v[m]),verts[m],wrap);
             if ( wraps ) wraps[m] = wrap;
          }
        if ( wrap )
        { int m = n*(n+3)/2;
          (*sym_wrap)(get_coord(v[m]),verts[m],wrap);
          if ( wraps ) wraps[m] = wrap;
        }
     
        /* third edge */ 
        fe = get_next_edge(fe);
        if ( wrap && !inverted(get_fe_edge(fe)) )
          for ( j = 1 ; j < n ; j++ )
          { int m = n*j + j*(3-j)/2;
             (*sym_wrap)(get_coord(v[m]),verts[m],wrap);
             if ( wraps ) wraps[m] = wrap;
          }
        fewrap = get_fe_wrap(fe);
        wrap = (*sym_compose)(wrap,fewrap);
        if ( wrap && inverted(get_fe_edge(fe)) )
          for ( j = 1 ; j < n ; j++ )
          { int m = n*j + j*(3-j)/2;
             (*sym_wrap)(get_coord(v[m]),verts[m],wrap);
             if ( wraps ) wraps[m] = wrap;
          }
      }
      return;
    }

  if ( (web.modeltype == QUADRATIC) && verts[3] ) 
  { get_facet_verts_q(f_id,verts,wraps); return; }

  if ( web.representation == SIMPLEX )
  { vertex_id *v = get_facet_vertices(f_id);
    for ( i = 0 ; i <= web.dimension ; i++ )
     { x = get_coord(v[i]);
        for ( j = 0 ; j < SDIM ; j++ )
          verts[i][j] = x[j];
     }
    return;
  }

  fe = get_facet_fe(f_id);
  if ( web.symmetry_flag )
  { wrap = 0;
    for ( i = 0 ; i < FACET_VERTS ; i++ )
    { 
      x = get_coord(get_fe_tailv(fe));
      if ( wraps ) wraps[i] = wrap;
      (*sym_wrap)(x,verts[i],wrap);
      fewrap = get_fe_wrap(fe);
      wrap = (*sym_compose)(wrap,fewrap);
      fe = get_next_edge(fe);
    }
  }
  else for ( i = 0 ; i < FACET_VERTS ; i++ )
  { vertex_id v_id = get_fe_tailv(fe);
    x = get_coord(v_id);
    for ( j = 0 ; j < SDIM ; j++ )
      verts[i][j] = x[j];
    fe = get_next_edge(fe);

  }
}
  
/***************************************************************
*
*  Function: get_facet_verts_special()
*
*  Purpose: to get all three vertices of a facet, nicely 
*          displaced to fundamental cell in case of symmetry group.
*          Extra work to get them to come out close to origin.
*
*/

void get_facet_verts_special(f_id,verts,wraps)
facet_id f_id;
REAL **verts;    /* fourth spot NULL if don't want midpts in quadratic */
WRAPTYPE *wraps;  /* wraps of vertices; NULL if not wanted */
{
  facetedge_id fe;
  int i,j,ii;
  WRAPTYPE wrap,fewrap;
  REAL *x;

  if ( web.modeltype == LAGRANGE )
  { vertex_id *v = get_facet_vertices(f_id);
    int k = web.skel[FACET].ctrlpts;
    for ( i = 0 ; i < k ; i++ )
     { x = get_coord(v[i]);
       for ( j = 0 ; j < SDIM ; j++ )
          verts[i][j] = x[j];
     }
     return;
  }

  if ( (web.modeltype == QUADRATIC) && verts[3] ) 
  { get_facet_verts_q(f_id,verts,wraps); return; }

  if ( web.representation == SIMPLEX )
  { vertex_id *v = get_facet_vertices(f_id);
    for ( i = 0 ; i <= web.dimension ; i++ )
     { x = get_coord(v[i]);
       for ( j = 0 ; j < SDIM ; j++ )
          verts[i][j] = x[j];
     }
     return;
  }

  fe = get_facet_fe(f_id);
  if ( web.symmetry_flag )
  {
    /* start with point closest to origin */
    if ( !(sym_flags & HAS_FIXED_PTS) )
    { REAL dd,mindd;
      int mini=0;
      facetedge_id minfe=fe;
      mindd = 1e40;
      for ( i = 0 ; i < FACET_VERTS ; i++, fe = get_next_edge(fe) )
      { x = get_coord(get_fe_tailv(fe));
         dd = SDIM_dot(x,x);
         if ( dd < 0.9*mindd ) { mindd = dd; mini = i; minfe = fe;}
      }
      i = mini;
      fe = minfe;
    }
    else { i = 0; }
    wrap = 0;
    for ( ii = 0 ; ii < FACET_VERTS ; ii++ )
    { int jj = (i+ii)%FACET_VERTS;
      x = get_coord(get_fe_tailv(fe));
      if ( wraps ) wraps[jj] = wrap;
      (*sym_wrap)(x,verts[jj],wrap);
      fewrap = get_fe_wrap(fe);
      wrap = (*sym_compose)(wrap,fewrap);
      fe = get_next_edge(fe);
    }
  }
  else for ( i = 0 ; i < FACET_VERTS ; i++ )
    { x = get_coord(get_fe_tailv(fe));
      for ( j = 0 ; j < SDIM ; j++ )
        verts[i][j] = x[j];
      fe = get_next_edge(fe);
    }
}
  
/***************************************************************
*
*  Function: get_facet_verts_q()
*
*  Purpose: to get all vertices of a facet, nicely 
*          displaced to fundamental cell in case of symmetry group.
*          Quadratic model.  Midpoints interleaved with corners.
*
*/

void get_facet_verts_q(f_id,verts,wraps)
facet_id f_id;
REAL **verts;
WRAPTYPE *wraps;  /* NULL if not wanted */
{
  facetedge_id fe;
  int i,j,ii;
  WRAPTYPE wrap,fewrap;
  REAL *x;

  if ( web.representation == SIMPLEX )
    { kb_error(1370,"No quadratic model with simplices.\n",RECOVERABLE);

    }

  fe = get_facet_fe(f_id);
  if ( web.symmetry_flag )
  {
    i = 0;
    wrap = 0;
    for ( ii = 0 ; ii < FACET_VERTS ; ii++ )
    { int jj = (i+ii)%FACET_VERTS;
      x = get_coord(get_fe_tailv(fe));
      if ( wraps ) wraps[2*jj] = wrap;
      (*sym_wrap)(x,verts[2*jj],wrap);

      x = get_coord(get_fe_midv(fe));

      if ( !inverted(get_fe_edge(fe)) )
      { if ( wraps ) wraps[2*jj+1] = wrap;
         (*sym_wrap)(x,verts[2*jj+1],wrap);
      }

      fewrap = get_fe_wrap(fe);
      wrap = (*sym_compose)(wrap,fewrap);

      if ( inverted(get_fe_edge(fe)) )
      { if ( wraps ) wraps[2*jj+1] = wrap;
         (*sym_wrap)(x,verts[2*jj+1],wrap);
      }

      fe = get_next_edge(fe);
    }
  }
  else for ( i = 0 ; i < FACET_VERTS ; i++ )
    { x = get_coord(get_fe_tailv(fe));
      for ( j = 0 ; j < SDIM ; j++ )
        verts[2*i][j] = x[j];
      x = get_coord(get_fe_midv(fe));
      for ( j = 0 ; j < SDIM ; j++ )
        verts[2*i+1][j] = x[j];
      fe = get_next_edge(fe);
    }
}

/*****************************************************************
*
* Function: path_open()
*
*  Purpose: Open file for reading on EVOLVERPATH
*/

#ifdef __WIN32__
FILE *path_open(char *name,int mode)    /* else TC++ has conniptions */
#else
FILE *path_open(name,mode)
char *name;
int mode; /* NOTDATAFILENAME or SETDATAFILENAME */
#endif
{
  char path[PATHSIZE];
  size_t len;
  FILE *fd = NULL;
  char *env;

#ifdef MPI_EVOLVER
  /* use filename as format string to generate various file names */
  char taskpath[PATHSIZE];
  sprintf(taskpath,name,this_task);
  name = taskpath;
#endif

  env = getenv("EVOLVERPATH");

#if defined(WIN32) && !defined(__BORLANDC__)
  /* Using wildcards! */

  /* try given name */
  strncpy(path,name,sizeof(path));
  for(;;)
  { /* try paths in EVOLVERPATH */
    intptr_t ret;
    struct _finddata_t finddata;

    ret = _findfirst(path,&finddata);
    if ( ret != -1 )
    { /* finddata.name only has filename, not path stuff */
      char *slash = strrchr(path,'/');
      if ( !slash ) slash = strrchr(path,'\\');
      if ( slash ) slash++;
      else slash = path;
      strncpy(slash,finddata.name,sizeof(path)-(slash-path));
      fd = fopen(path,"r");
      _findclose(ret);
      break;
    }

    if ( env == NULL ) break;
    len = strcspn(env,ENVPATHCHAR);
    if ( len == 0 ) break;
    strncpy(path,env,len);
    path[len] = PATHCHAR;
    strncpy(path+len+1,name,sizeof(path)-len-2);
    if ( env[len] == 0 ) env = NULL; /* end of EVOLVERPATH */
    else env += len+1;
  } 
  
  /* try .fe extension */
  if ( fd == NULL)
  {
    env = getenv("EVOLVERPATH");
    strncpy(path,name,sizeof(path));
    strcat(path,".fe");
    for ( ;; )
    {
      intptr_t ret;
      struct _finddata_t finddata;

      ret = _findfirst(path,&finddata);
      if ( ret != -1 )
      { char *slash = strrchr(path,'/');
        if ( !slash ) slash = strrchr(path,'\\');
        if ( slash ) slash++;
        else slash = path;
        strncpy(slash,finddata.name,sizeof(path)-(slash-path));
        fd = fopen(path,"r");
        _findclose(ret);
        /* scrape the ".fe" off the end of the path since user didn't give it */
        path[strlen(path)-3] = 0;
        break;
      }

      /* try paths in EVOLVERPATH */
      if ( env == NULL ) break;
      len = strcspn(env,ENVPATHCHAR);
      if ( len == 0 ) break;
      strncpy(path,env,len);
      path[len] = PATHCHAR;
      strncpy(path+len+1,name,sizeof(path)-len-2);
      strcat(path,".fe");
      if ( env[len] == 0 ) env = NULL; /* end of EVOLVERPATH */
      else env += len+1;
    } 
  }
#elif defined(_GLOB_H)
  /* Using wildcards! */

  /* try given name */
  strncpy(path,name,sizeof(path));
  for(;;)
  { /* try paths in EVOLVERPATH */
    int ret;
    glob_t globdata;

    memset(&globdata,0,sizeof(globdata));
    ret = glob(path,0,NULL,&globdata);
    if ( ret == 0 ) /* success */
    { 
      strncpy(path,globdata.gl_pathv[0],sizeof(path));
      fd = fopen(path,"r");
      globfree(&globdata);
      break;
    }

    if ( env == NULL ) break;
    len = strcspn(env,ENVPATHCHAR);
    if ( len == 0 ) break;
    strncpy(path,env,len);
    path[len] = PATHCHAR;
    strncpy(path+len+1,name,sizeof(path)-len-2);
    if ( env[len] == 0 ) env = NULL; /* end of EVOLVERPATH */
    else env += len+1;
  } 
  
  /* try .fe extension */
  if ( fd == NULL)
  {
    env = getenv("EVOLVERPATH");
    strncpy(path,name,sizeof(path));
    strcat(path,".fe");
    for ( ;; )
    { glob_t globdata;
      int ret;

      ret = glob(path,0,NULL,&globdata);
      if ( ret == 0 ) /* success */
      { 
        strncpy(path,globdata.gl_pathv[0],sizeof(path));
        fd = fopen(path,"r");
        globfree(&globdata);
        /* scrape the ".fe" off the end of the path since user didn't give it */
        path[strlen(path)-3] = 0;
        break;
      }

      /* try paths in EVOLVERPATH */
      if ( env == NULL ) break;
      len = strcspn(env,ENVPATHCHAR);
      if ( len == 0 ) break;
      strncpy(path,env,len);
      path[len] = PATHCHAR;
      strncpy(path+len+1,name,sizeof(path)-len-2);
      strcat(path,".fe");
      if ( env[len] == 0 ) env = NULL; /* end of EVOLVERPATH */
      else env += len+1;
    } 
  }
#else
  /* try given name */
  strncpy(path,name,sizeof(path));
  while ( (fd = fopen(path,"r")) == NULL)
  { /* try paths in EVOLVERPATH */
    if ( env == NULL ) break;
    len = strcspn(env,ENVPATHCHAR);
    if ( len == 0 ) break;
    strncpy(path,env,len);
    path[len] = PATHCHAR;
    strncpy(path+len+1,name,sizeof(path)-len-2);
    if ( env[len] == 0 ) env = NULL; /* end of EVOLVERPATH */
    else env += len+1;
  } 
  
  /* try .fe extension */
  if ( fd == NULL)
  {
    env = getenv("EVOLVERPATH");
    strncpy(path,name,sizeof(path));
    strcat(path,".fe");
    while ( (fd = fopen(path,"r")) == NULL)
     { /* try paths in EVOLVERPATH */
        if ( env == NULL ) break;
        len = strcspn(env,ENVPATHCHAR);
        if ( len == 0 ) break;
        strncpy(path,env,len);
        path[len] = PATHCHAR;
        strncpy(path+len+1,name,sizeof(path)-len-2);
        strcat(path,".fe");
        if ( env[len] == 0 ) env = NULL; /* end of EVOLVERPATH */
        else env += len+1;
     } 
  }
#endif
 
  if ( fd && (mode==SETDATAFILENAME) )
  { /* has to be set so repeat open works */
    /* copy from end, getting same number of directories */
    char *found; /* from findfile() */
    char *user; /* user input search string */

    found = path + strlen(path) - 1;
    user  = name + strlen(name) - 1;
    while ( user >= name )
    { while ( (*user != PATHCHAR) && (*user != '\\') && (user >= name) )
         user--;
      while ( (*found != PATHCHAR) && (*found != '\\') && (found >= path) )
         found--;
      if ( user >= name ) user--; else break;
      if ( found >= path ) found--; else break;
    } 
    strncpy(datafilename,found+1,PATHSIZE);
  }

  return fd;
}

/**************************************************************************
*
* function: calc_view_transform_gens()
*
* purpose: evaluate expressions in view_transform_gens
*
* return: 1 if anything changed; 0 else.
*/

int calc_view_transform_gens()
{ int n,i,j,nmax;
  REAL value;
  int change = 0;

  nmax = web.torus_flag ? transform_gen_count - SDIM : transform_gen_count;
  for ( n = 0 ; n < nmax ; n++ )
    for ( i = 0 ; i <= SDIM ; i++ )
      for ( j = 0 ; j <= SDIM ; j++ )
      { value = eval(&view_transform_gens_expr[n][i][j],NULL,NULLID,NULL);
        if ( value != view_transform_gens[n][i][j] ) 
        { view_transform_gens[n][i][j] = value;
          change = 1;
        }
      } 
 return change;
}

/*******************************************************************
*
*  function: calc_periods()
*
*  purpose: calculate torus periods from expressions
*/

void calc_periods(mode)
int mode;  /* NO_ADJUST_VOLUMES or ADJUST_VOLUMES */
{ int i,j;
  REAL value;
  REAL old_torusv = web.torusv;
  MAT2D(invper,MAXCOORD,MAXCOORD); /* so don't have temporary bad values
      in web.inverse_periods since graphgen() may be using it */
  int k;

  if ( torus_period_expr[0][0].start )
  { for ( i = 0 ; i < SDIM ; i++ )
    { for ( j = 0 ; j < SDIM ; j++ )
      { value = eval(&torus_period_expr[i][j],NULL,NULLID,NULL);
        invper[j][i] = web.torus_period[i][j] = value;
      }
    }
    web.torusv = det_adjoint(invper,SDIM);
    if ( web.torusv == 0.0 )
      kb_error(1377,"Degenerate torus unit cell.\n",RECOVERABLE);
    for ( i = 0 ; i < SDIM ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
       web.inverse_periods[i][j] = invper[i][j]/web.torusv;
  }

  if ( web.torus_display_period )
  { REAL det;
    for ( i = 0 ; i < SDIM ; i++ )
    { for ( j = 0 ; j < SDIM ; j++ )
      { value = eval(&torus_display_period_expr[i][j],NULL,NULLID,NULL);
        invper[j][i] = web.torus_display_period[i][j] = value;
      }
    }
    det = det_adjoint(invper,SDIM);
    if ( det == 0.0 )
      kb_error(3377,"Degenerate display periods.\n",RECOVERABLE);
    for ( i = 0 ; i < SDIM ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
       web.inverse_display_periods[i][j] = invper[i][j]/det;
  }


  /* if change in periods, adjust volumes and volconsts */
  if ( (mode == ADJUST_VOLUMES) && (old_torusv != 0.0) && !web.pressure_flag)
  { body_id b_id;
    value = web.torusv/old_torusv;
    FOR_ALL_BODIES(b_id)
    { set_body_volume(b_id,value*(get_body_volume(b_id)),SETSTAMP);
      save_body_volume(b_id);
      if ( get_battr(b_id) & FIXEDVOL )
          set_body_fixvol(b_id,value*(get_body_fixvol(b_id)));
      set_body_volconst(b_id,value*(get_body_volconst(b_id)));
      if ( everything_quantities_flag )
      { struct gen_quant *q = GEN_QUANT(get_body_volquant(b_id));
        q->value *= value;
        q->target *= value;
        q->volconst *= value;
      }
    }
  }

  /* adjust view transform generators for automatic torus symmetries */
  if ( web.torus_flag )
  {
    if ( view_transform_gens == NULL )
    { view_transform_gens = dmatrix3(SDIM,SDIM+1,SDIM+1);
      transform_gen_count = SDIM;
    }
    if ( transform_gen_swap == NULL )
       transform_gen_swap = (int*)mycalloc(SDIM,sizeof(int));
    for ( j=transform_gen_count-SDIM, k=0 ; j<transform_gen_count ; k++,j++ )
    { matcopy(view_transform_gens[j],identmat,SDIM+1, SDIM+1);
      for ( i = 0 ; i < SDIM ; i++ )
      view_transform_gens[j][i][SDIM] = web.torus_period[k][i];
    }
    if (transform_expr && *transform_expr)
      /* re-evaluate expressions */
      if ( calc_view_transform_gens() )  /*  see if changed */
        transform_gen_expr(transform_expr);
  }
}
  
/***************************************************
* 
*  Handy for graphics function pointers for 
*  functions that have nothing to do.
*/

void null_function() {}

/************************************************************
*
*  For uninitialized function pointers.
*
*/

void bad_function()
{ kb_error(1371,"Internal error:  Using uninitialized function pointer.\n",
     WARNING);
}

/*****************************************************************
*
* Function: distance()
*
*  Purpose: Finds distance between two vertices.
*/

REAL distance(v1,v2)
vertex_id v1,v2;
{
  REAL *c1,*c2;
  REAL sum;
  int i;

  c1 = get_coord(v1);
  c2 = get_coord(v2);
  sum = 0.0;
  for ( i = 0 ; i < SDIM ; i++ )
     sum += (c1[i] - c2[i])*(c1[i] - c2[i]);
  return sqrt(sum);
}

/***************************************************************************
*
* function: grule()
*
* purpose: Calculate abscissas and weights for gaussian quadrature
*         on [0,1].
*
* Adapted from a FORTRAN routine GRULE in a book on numerical integration 
*/

void grule(n,x,w)
int n;  /* number of points */
REAL *x;    /* for abscissas  x[0] ... x[n-1] */
REAL *w;    /* for weights     w[0] ... w[n-1] */
{
  REAL pkm1,pk,t,t1,pkp1,den,d1,dpn,d2pn,d3pn,d4pn,u,v,h,p,dp,fx,x0;
  int m,e1,i,k;
  
  if ( n < -1 )
  { /* rectangle rule */
    for ( k = 0 ; k < abs(n) ; k++ )
      { x[k] = k/(REAL)(abs(n)-1);
         w[k] = 1/(REAL)abs(n);
      }
  }
  m = (n+1)/2;
  e1 = n*(n+1);
  for ( i = 1 ; i <= m ; i++ )
  {
    /* original calculation on [-1,1] */
    t = (4*i - 1)*M_PI/(4*n+2);
    x0 = (1 - (1 - 1.0/n)/(8*n*n))*cos(t);
    pkm1 = 1.0;
    pk = x0;
    for ( k = 2 ; k <= n ; k++ )
     {
        t1 = x0*pk;
        pkp1 = t1 - pkm1 - (t1 - pkm1)/k + t1;
        pkm1 = pk;
        pk = pkp1;
     }
    den = 1 - x0*x0;
    d1 = n*(pkm1 - x0*pk);
    dpn = d1/den;
    d2pn = (2*x0*dpn - e1*pk)/den;
    d3pn = (4*x0*d2pn + (2 - e1)*dpn)/den;
    d4pn = (6*x0*d3pn + (6 - e1)*d2pn)/den;
    u = pk/dpn;
    v = d2pn/dpn;
    h = -u*(1 + 0.5*u*(v + u*(v*v - d3pn/3/dpn)));
    p = pk + h*(dpn + 0.5*h*(d2pn + h/3*(d3pn + 0.25*h*d4pn)));
    dp = dpn + h*(d2pn + .5*h*(d3pn + h*d4pn/3));
    h = h - p/dp;
    x[i-1] = x0 + h;
    fx = d1 - h*e1*((((0.2*h*d4pn + d3pn)*0.25*h + d2pn)*h/3 + dpn)*0.5*h + pk);
    w[n-i] = w[i-1] = (1 - x[i-1]*x[i-1])/fx/fx;

    /* normalization to [0,1] */
    x[i-1] = (1 + x[i-1])/2;
    x[n-i] =  1 - x[i-1];
  }
  if ( 2*m > n ) x[m-1] = 0.5;
}


/****************************************************************
*
*  function: binom_coeff()
*
*  purpose: calculate binomial coefficient (small numbers only)
*
*/

int binom_coeff(n,k)
int n,k;
{ int i, c;

  if ( n < k ) return 0;
  if ( (n-k) < k ) k = n - k; 
  for ( i = 0, c = 1 ; i < k ; i++ )
  { c *= n - i; c /= i+1; }
  return c;
}

/**********************************************************************
*
*  function: set_e_phase_density()
*
*  purpose: calculate edge density from phases of neighboring facets.
*/
void set_e_phase_density(e_id)
edge_id e_id;
{ facetedge_id fe = get_edge_fe(e_id);
  int i = get_f_phase(get_fe_facet(fe));      
  int j = get_f_phase(get_fe_facet(get_next_facet(fe)));      
      set_edge_density(e_id,phase_data[i][j]);
}

/**********************************************************************
*
*  function: set_f_phase_density()
*
*  purpose: calculate facet density from phases of neighboring bodies.
*/
void set_f_phase_density(f_id)
facet_id f_id;
{ 
  int i = get_b_phase(get_facet_body(f_id));     
  int j = get_b_phase(get_facet_body(inverse_id(f_id)));      
  set_attr(f_id,DENSITY);
  set_facet_density(f_id,phase_data[i][j]);
}

/**********************************************************************
*
*  function: make_vedge_lists()
*
*  purpose: construct from scratch edge lists aound vertices.
*    These are singly linked circular lists with links residing
*    in edge structures.
*/

void make_vedge_lists()
{ vertex_id v_id;
  edge_id e_id;

  if ( web.representation == SIMPLEX ) return;

  /* clear out all old data */
  MFOR_ALL_EDGES(e_id)
  { set_next_tail_edge(e_id,NULLID);
    set_next_tail_edge(inverse_id(e_id),NULLID);
  }
  MFOR_ALL_VERTICES(v_id)
  { if ( !(get_vattr(v_id) & (Q_MIDEDGE|Q_MIDFACET|Q_MIDPOINT)) )
      set_vertex_edge(v_id,NULLID);
  }

  /* now splice in all edges */
  MFOR_ALL_EDGES(e_id)
  { insert_vertex_edge(get_edge_tailv(e_id),e_id);
    insert_vertex_edge(get_edge_headv(e_id),inverse_id(e_id));
  }
  vedge_timestamp = top_timestamp; 
}


/**********************************************************************
*
*  function: make_vfacet_lists()
*
*  purpose: construct from scratch facet lists aound vertices.
*    These are singly linked circular lists with links residing
*    in facet structures.
*
*  Should only be used currently with SIMPLEX model, since others
*  use edge links around vertex.
*/

void make_vfacet_lists()
{ vertex_id v_id;
  facet_id f_id, ff_id,fff_id;
  int i;
  int facet_verts;

  /* set all facet vertices */
  facet_verts = web.skel[FACET].ctrlpts;
  expand_attribute(FACET,F_NEXT_VFACET_ATTR,&facet_verts);
  if ( web.representation == SOAPFILM ) /* set up facet vertex lists */
  { if ( web.modeltype != LAGRANGE )
    FOR_ALL_FACETS(f_id)
    { facetedge_id fe = get_facet_fe(f_id);
      vertex_id *fv = get_facet_vertices(f_id);

      for ( i = 0 ; i < FACET_VERTS ; i++ )
      { 
        v_id = get_fe_tailv(fe);
        fv[i] = v_id;
        fe = get_next_edge(fe);
        if ( web.modeltype == QUADRATIC )
           fv[i+FACET_VERTS] = get_edge_midv(get_fe_edge(fe));
      }
    }
  }
  else if ( web.representation == STRING )
  { edge_id e_id;
    FOR_ALL_EDGES(e_id)
    { facetedge_id fe = get_edge_fe(e_id);
      if ( !valid_id(fe) )
        continue;
      f_id = get_fe_facet(fe);
      if ( !valid_id(f_id) )
        continue;
      set_vertex_facet(get_edge_tailv(e_id),f_id);
      set_vertex_facet(get_edge_headv(e_id),f_id);
    }
  }
  else if ( web.representation == SIMPLEX )
    FOR_ALL_FACETS(f_id) /* simplex vertex to facet links */
    { vertex_id *fv = get_facet_vertices(f_id);
      for ( i = 0 ; i < facet_verts ; i++ )
      { 
        set_vertex_facet(fv[i],f_id);
      }
    }
  
  /* start by making one-element loops on facets */
  FOR_ALL_VERTICES(v_id)
  {
     f_id = get_vertex_facet(v_id);
     if ( valid_id(f_id) )
     { set_next_vertex_facet(v_id,f_id,f_id);
       set_vertex_facet(v_id,f_id);
     }
  }
  if ( web.representation == STRING )
  { 
     /* not using explicit linked list; 
        since get_next_vertex_facet() uses
        edge list around vertex and facet 
        list around edge.
        */
  }
  else
  FOR_ALL_FACETS(f_id)
  { 
    vertex_id *fv = get_facet_vertices(f_id);
    for ( i = 0 ; i < facet_verts ; i++ )
    { v_id = fv[i]; 
      ff_id = get_vertex_facet(v_id);
      if ( !equal_id(f_id,ff_id) )
      { /* splice in after first one */
        fff_id = get_next_vertex_facet(v_id,ff_id);
        if ( !equal_element(f_id,fff_id) )
        { set_next_vertex_facet(v_id,ff_id,f_id);
          set_next_vertex_facet(v_id,f_id,fff_id);
        }
      }
    }
  }
  vfacet_timestamp = top_timestamp; 
}

/**********************************************************************
*
*  function: make_bfacet_lists()
*
*  purpose: construct from scratch facet lists aound bodies.
*    These are doubly linked circular lists with links residing
*    in facet structures.
*/

void make_bfacet_lists()
{ body_id b_id;
  facet_id f_id, ff_id,fff_id,fi_id;
  int four = 4;

  if ( bfacet_timestamp >= top_timestamp ) return; 
  if ( web.skel[BODY].count == 0 ) return;
  expand_attribute(FACET,F_NEXT_BFACET_ATTR,&four);
  /* start by making one-element loops on facets pointed to
      by body facet_edge link */
  FOR_ALL_BODIES(b_id)
  { /* make sure legal pointer */
    f_id = get_body_facet(b_id);
    if ( !valid_id(f_id) || 
    #ifdef MPI_EVOLVER
         !mpi_remote_present(f_id) ||
    #endif  
        !equal_id(b_id,get_facet_body(f_id))  )
    { set_body_facet(b_id,NULLID);
      FOR_ALL_FACETS(f_id)
      { if ( equal_id(b_id,get_facet_body(f_id)) ) 
        {set_body_facet(b_id,f_id); break; }
        if ( equal_id(b_id,get_facet_body(inverse_id(f_id))) ) 
        {set_body_facet(b_id,inverse_id(f_id)); break; }
      }   
    }   
    f_id = get_body_facet(b_id);
    if ( valid_id(f_id) )
    {
      set_next_body_facet(f_id,f_id);
      set_prev_body_facet(f_id,f_id);
    }
  }

  /* now splice in all other facets */
  FOR_ALL_FACETS(f_id)
  { b_id = get_facet_body(f_id);
    if ( valid_id(b_id) )
    { ff_id = get_body_facet(b_id);
      if ( !equal_id(f_id,ff_id) )
      { /* splice in after first one */
        fff_id = get_next_body_facet(ff_id);
        set_next_body_facet(ff_id,f_id);
        set_prev_body_facet(f_id,ff_id);
        set_next_body_facet(f_id,fff_id);
        set_prev_body_facet(fff_id,f_id);
      }
    }
    fi_id = inverse_id(f_id);
    b_id = get_facet_body(fi_id);
    if ( valid_id(b_id) )
      { ff_id = get_body_facet(b_id);
        if ( !equal_id(fi_id,ff_id) )
         { /* splice in after first one */
            fff_id = get_next_body_facet(ff_id);
            set_next_body_facet(ff_id,fi_id);
            set_prev_body_facet(fi_id,ff_id);
            set_next_body_facet(fi_id,fff_id);
            set_prev_body_facet(fff_id,fi_id);
         }
      }
  }
  bfacet_timestamp = top_timestamp; 
}

/*******************************************************************
*
* function: insert_vertex_edge(v_id,e_id)
*
* purpose: fix up edge links around vertex when edge added
*
*/

void insert_vertex_edge(v_id,e_id)
vertex_id v_id;
edge_id    e_id;
{ edge_id ee_id,eee_id; 
  int n;
  
  #ifdef MPI_EVOLVER
  if ( !elptr(v_id) || !elptr(e_id) ) 
    return;
  #endif

  if ( !equal_id(v_id,get_edge_tailv(e_id)) )
  { kb_error(1372,"Internal error: Trying to insert edge to wrong vertex.\n",
       WARNING);
    return;
  }
  ee_id = get_vertex_edge(v_id);
  if ( !valid_id(ee_id) )
  { set_next_tail_edge(e_id,e_id);
    set_vertex_edge(v_id,e_id);
    return;
  }
  /* go around to be sure not already there */
  eee_id = ee_id; n = 0;
  do 
  { eee_id = get_next_tail_edge(eee_id);
    if ( equal_id(eee_id,e_id) ) return;
    if ( ++n > 2*web.skel[EDGE].count+5 ) /* sanity check */
    { sprintf(errmsg,"Internal error: Vertex %s edge loop not closed.\n",
          ELNAME(v_id));
      kb_error(1373,errmsg,WARNING);
      return;
    }
  } while ( !equal_id(eee_id,ee_id) );

  /* insert into existing list */
  ee_id = get_next_tail_edge(eee_id);
  set_next_tail_edge(eee_id,e_id);
  set_next_tail_edge(e_id,ee_id);
} /* end insert_vertex_edge */

/*******************************************************************
*
* function: remove_vertex_edge(v_id,e_id)
*
* purpose: fix up edge links around vertex when edge removed
*
*/

void remove_vertex_edge(v_id,e_id)
vertex_id v_id;
edge_id    e_id;  /* tail is v_id */
{ edge_id ee_id,eee_id,eeee_id; 
  vertex_id vv_id = get_edge_tailv(e_id);
  int n = 0;

#ifdef MPI_EVOLVER
  if ( !mpi_remote_present(v_id) )
     return;
#endif

  if ( !equal_id(v_id,vv_id) )
  { sprintf(errmsg,
    "Internal error: Trying to detach edge %s from vertex %s instead of %s.\n",
       ELNAME(e_id),ELNAME1(v_id),ELNAME2(vv_id));
     kb_error(1374,errmsg,  WARNING);
    return;
  }
  /* make sure edge actually in vertex edge loop */
  ee_id = get_vertex_edge(v_id);
  if ( !valid_id(ee_id)) return;
  eee_id = ee_id;
  do
  { 
  #ifdef MPI_EVOLVER
    if ( !mpi_remote_present(eee_id) )
       return;  /* fail, but don't crash */
  #endif
    if ( equal_id(eee_id,e_id) ) break;
    eee_id = get_next_tail_edge(eee_id);
    if ( ++n > 2*web.skel[EDGE].count ) /* sanity check */ 
    { kb_error(1375,"Internal error: Vertex edge loop not closed.\n",WARNING);
      break;
    }
  } while ( !equal_id(eee_id,ee_id) );
  if ( !equal_id(eee_id,e_id) ) return;

  ee_id = get_next_tail_edge(e_id);
  if ( equal_id(e_id,ee_id) )
    { set_vertex_edge(v_id,NULLID);
      return;
    }
  #ifdef MPI_EVOLVER
      if ( !mpi_remote_present(ee_id) )
         return;
  #endif
  eee_id = ee_id;
  eeee_id = get_next_tail_edge(ee_id);
  #ifdef MPI_EVOLVER
      if ( !mpi_remote_present(eeee_id) )
         return;
   #endif

  n = 0;
  while ( !equal_id(e_id,eeee_id) )
    { eee_id = eeee_id;
      eeee_id = get_next_tail_edge(eee_id);
      #ifdef MPI_EVOLVER
      if ( !mpi_remote_present(eeee_id) )
         return;
      #endif
      if ( ++n > 2*web.skel[EDGE].count ) /* sanity check */
      { sprintf(errmsg, "Internal error: Vertex %s edge loop not closed.\n",
           ELNAME(v_id));
        kb_error(1376,errmsg, WARNING);
        break;
      }
    }
  set_next_tail_edge(eee_id,ee_id);
  set_next_tail_edge(e_id,e_id);
  set_vertex_edge(v_id,ee_id);
  
  /* clean up in case of constraint content quantities */
  if ( everything_quantities_flag && (web.representation == STRING) )
  { int k,n;
    conmap_t *map = get_v_constraint_map(v_id);
    #define MAXTODO 10
    conmap_t to_do[MAXTODO]; 
    /* make list since unset and set can change order in map */
    for ( k = 1, n = 0; k <= (int)*map ; k++ )
    { int connum = map[k]&CONMASK;
      struct constraint *con = get_constraint(connum);
      if ( (con->attr & CON_CONTENT) && (n < MAXTODO) )
        to_do[n++] = connum;
    }
    for ( k = 0 ; k < n ; k++ )
    { unset_v_constraint_map(v_id,to_do[k]);
      set_v_constraint_map(v_id,to_do[k]);
    }  
   }

} /* end remove_vertex_edge() */

/*******************************************************************
*
*  function: get_vertex_first_facet()
*
*  purpose: find some facet adjoining vertex, first facet in 
*           implicit list found by following vertex edge list
*           around and then facets around edge.
*
*  Returns facetedge_id if possible, otherwise facet_id
*/

element_id get_vertex_first_facet(v_id)
vertex_id v_id;
{ 

  /* special cases */
  if ( web.representation == SIMPLEX )
  { if ( vfacet_timestamp < top_timestamp )
      make_vfacet_lists(); 
    return get_vertex_facet(v_id);
  }

  if ( get_vattr(v_id) & Q_MIDFACET ) 
    return get_vertex_facet(v_id);

  return get_next_vertex_facet(v_id,NULLID);

} /* end get_vertex_first_facet() */

/************************************************************************
*
* Function:  get_next_vertex_facet()
*
* Purpose: get next facet in implicit chain around vertex.
*
* Returns facetedge_id if possible, otherwise facet_id
*/
element_id get_next_vertex_facet(v_id,prev_id)
vertex_id v_id;
element_id prev_id; /* either facet_id or facetedge_id;
                       if null, gets first one */
{ 
  edge_id start_e,ee_id;
  facetedge_id fe_id,start_fe, first_fe = NULLID;
  facet_id next_f;
  int found; /* whether old f_id found yet */

  if ( web.representation == SIMPLEX ) 
  {
    return get_simplex_next_vertex_facet(v_id,prev_id);
  }

  if ( id_type(prev_id) == FACET )
    return NULLID;  /* interior Lagrange point */

  /* now trace loop of edges and facetedges */
  start_e = get_vertex_edge(v_id);
  if ( !valid_id(start_e) ) return NULLID;
  ee_id = start_e;
  found = !valid_id(prev_id);
  do
  { fe_id = start_fe = get_edge_fe(ee_id);
    if ( valid_id(fe_id) ) 
    do
    { 
      facetedge_id ret_fe = fe_id;  /* return value */
      next_f = get_fe_facet(fe_id);

      if ( web.representation == STRING )
      { if ( !valid_id(next_f) ) 
           return NULLID;
        if ( !valid_id(get_prev_edge(fe_id)) && inverted(next_f) )
        { /* in case facet doesn't have positive facet */
          next_f = positive_id(next_f);
          ret_fe = inverse_id(fe_id);
        }
      }
      
      if ( !inverted(next_f) ) 
      { if ( !valid_id(first_fe) )
          first_fe = ret_fe;
        if ( found )
          return ret_fe;
        if ( equal_element(fe_id,prev_id) )
          found = 1;
      }
      fe_id = get_next_facet(fe_id);
    } while ( !equal_id(start_fe,fe_id) );

    ee_id = get_next_tail_edge(ee_id);
  } while ( !equal_id(ee_id,start_e) );

  if ( found )
    return first_fe; /* looped all the way around */

  return NULLID;
}

/***********************************************************************

   vertex facet list stuff for simplex model

*/

/*******************************************************************
*
* function: insert_vertex_facet(v_id,f_id)
*
* purpose: fix up facet links around vertex when facet added
*
*/

void insert_vertex_facet(v_id,f_id)
vertex_id v_id;
facet_id    f_id;
{ facet_id ff_id,fff_id; 
  int n;

  ff_id = get_vertex_facet(v_id);
  if ( !valid_id(ff_id) )
  { set_next_vertex_facet(v_id,f_id,f_id);
    set_vertex_facet(v_id,f_id);
    set_vertex_facet(v_id,f_id);
    return;
  }
  /* go around to be sure not already there */
  fff_id = ff_id; n = 0;
  do 
  { fff_id = get_next_vertex_facet(v_id,fff_id);
    if ( equal_element(fff_id,f_id) ) return;
    if ( ++n > 2*web.skel[FACET].count+5 ) /* sanity check */
    { kb_error(2441,"Internal error: Vertex facet loop not closed.\n",WARNING);
      return;
    }
  } while ( !equal_id(fff_id,ff_id) );

  /* insert into existing list */
  ff_id = get_next_vertex_facet(v_id,fff_id);
  set_next_vertex_facet(v_id,fff_id,f_id);
  set_next_vertex_facet(v_id,f_id,ff_id);
}

/*******************************************************************
*
* function: remove_vertex_facet(v_id,f_id)
*
* purpose: fix up facet links around vertex when facet removed
*
*/

void remove_vertex_facet(v_id,f_id)
vertex_id v_id;
facet_id    f_id;
{ facet_id ff_id,fff_id,ffff_id; 
  int n = 0;

  /* find it */
  ff_id = get_vertex_facet(v_id);
  if ( !valid_id(ff_id)) return;
  fff_id = ff_id;
  do
  { if ( equal_element(fff_id,f_id) ) break;
    fff_id = get_next_vertex_facet(v_id,fff_id);
    if ( ++n > 2*web.skel[FACET].count ) /* sanity check */ 
    { kb_error(2442,"Internal error: Vertex facet loop not closed.\n",WARNING);
      break;
    }
  } while ( !equal_element(fff_id,ff_id) );
  if ( !equal_id(fff_id,f_id) ) return;

  ff_id = get_next_vertex_facet(v_id,f_id);
  if ( equal_element(f_id,ff_id) )
    { set_vertex_facet(v_id,NULLID);
      return;
    }
  fff_id = ff_id;
  ffff_id = get_next_vertex_facet(v_id,ff_id);
  n = 0;
  while ( !equal_element(f_id,ffff_id) )
    { fff_id = ffff_id;
      ffff_id = get_next_vertex_facet(v_id,fff_id);
      if ( ++n > 2*web.skel[FACET].count ) /* sanity check */
      { kb_error(2443,"Internal error: Vertex facet loop not closed.\n",WARNING);
        break;
      }
    }
  set_next_vertex_facet(v_id,fff_id,ff_id);
  set_next_vertex_facet(v_id,f_id,f_id);
  set_vertex_facet(v_id,ff_id);
  if ( web.representation==SIMPLEX )
     set_vertex_facet(v_id,ff_id);
}

facet_id get_simplex_next_vertex_facet(v_id,f_id) 
vertex_id v_id; 
facet_id f_id;
{ int i;
  vertex_id *v = get_facet_vertices(f_id);
  facet_id *f = F_ELID(f_id,F_NEXT_VFACET_ATTR);
  int vmax = web.skel[FACET].ctrlpts;

  for ( i = 0 ; i < vmax ; i++ )
     if (equal_id(v_id,v[i]))  return f[i] ;
  sprintf(errmsg,"Internal error: get_next_vertex_facet failure v %s f %s\n",
      ELNAME(v_id), ELNAME1(f_id));
  kb_error(1306,errmsg,RECOVERABLE);

  return NULLID;
}

void set_next_vertex_facet(v_id,f_id,ff_id)
      vertex_id v_id; facet_id f_id,ff_id;
{
   vertex_id *v = get_facet_vertices(f_id);
   facet_id *f = F_ELID(f_id,F_NEXT_VFACET_ATTR);
   int vmax = web.skel[FACET].ctrlpts;
   int i;
   for ( i = 0 ; i < vmax ; i++ )
   { if (equal_id(v_id,v[i]))
     { f[i] = ff_id ;
       break;
     }
   }
}
/* end vertex facet list stuff for simplex model
***********************************************************************/


/************************************************************************
*
* function: count_fixed_vol()
*
* purpose: return number of fixed volumes and quantities
*
*/

int count_fixed_vol()
{ int retval = 0;
  body_id bi_id;  /* identifier for body i */
  int n;
  struct gen_quant *gq;

  if ( !everything_quantities_flag && !web.pressure_flag )
    FOR_ALL_BODIES(bi_id)
    if ( get_battr(bi_id) & (FIXEDVOL) ) retval++;
  for (  n = 0 ; n < gen_quant_count ; n++ )
  { gq = GEN_QUANT(n);
    if ( gq->flags & (Q_FIXED|Q_CONSERVED) )
        retval++;
  }
  return retval;
}

/*****************************************************************
*
*  Random number generator using the Mersenne Twister.
*

* A C-program for MT19937B: Integer Version 
* genrand() generate one pseudorandom integer which is  
* uniformly distributed among the 32bit unsigned integers
* sgenrand(seed) set initial values to the working area of 624 words.
* sgenrand(seed) must be called once before calling genrand()    
* (seed is any integer except 0).                    

* Modified to produce batch of drands by K. Brakke 

* Reference: 
   Makoto Matsumoto and Takuji Nishimura, 
      Mersenne Twister: A 623-Dimensionally Equidistributed Uniform
      Pseudo-Random Number Generator, ACM Transactions on Modeling and
      Computer Simulation, Vol. 8, No. 1, January 1998, pp. 3-30.
      In ACM on-line Library, 
   http://www.acm.org/pubs/citations/journals/tomacs/1998-8-1/p3-matsumoto/
*/
/* 
    LICENCE CONDITIONS: 

        Matsumoto and Nishimura consent to GNU General 
        Public Licence for this code.
*/


/* Period parameters */  
#define N  624
#define NR 312
#define M 397
#define MATRIX_A 0x9908b0df   /* constant vector a */
#define UPPER_MASK 0x80000000 /* most significant w-r bits */
#define LOWER_MASK 0x7fffffff /* least significant r bits */

/* for tempering */   
#define TEMPERING_MASK_B 0x9d2c5680
#define TEMPERING_MASK_C 0xefc60000
#define TEMPERING_SHIFT_U(y)  (y >> 11)
#define TEMPERING_SHIFT_S(y)  (y << 7)
#define TEMPERING_SHIFT_T(y)  (y << 15)
#define TEMPERING_SHIFT_L(y)  (y >> 18)

static unsigned long ptgfsr[N]; /* set initial seeds: N = 624 words */
static REAL drands[NR];  /* batch of random doubles */
static int initflag;
static REAL rfactor = 0.0;
static int k = N;
static int kr = NR;
void sgenrand(unsigned long);

void
sgenrand(seed)
unsigned long seed; /* seed should not be 0 */
{
  int k;
  
  /* setting initial seeds to ptgfsr[N] using     */
  /* the generator Line 25 of Table 1 in      */
  /* [KNUTH 1981, The Art of Computer Programming */
  /*    Vol. 2 (2nd Ed.), pp102]          */

  ptgfsr[0]= seed & 0xffffffff;
  for (k=1; k<N; k++)
    ptgfsr[k] = (69069 * ptgfsr[k-1]) & 0xffffffff;
}

void kb_initr(ind)
int ind;
{ sgenrand((unsigned long)ind); 
  rfactor = 1.0/( (REAL)0xffffffff + 1.0) ;
  initflag = 1;
  k = N; kr = NR;
}

static unsigned long mag01[2]={0x0, MATRIX_A};
REAL kb_drand()
{
  unsigned long y;
  /* mag01[x] = x * MATRIX_A  for x=0,1 */
  
  if(kr == NR){ /* generate N words at one time, and NR drands */
    int kk;
    if ( initflag == 0 ) kb_initr(1);
    for (kk=0;kk<N-M;kk++) {
      y = (ptgfsr[kk]&UPPER_MASK)|(ptgfsr[kk+1]&LOWER_MASK);
      ptgfsr[kk] = ptgfsr[kk+M] ^ (y >> 1) ^ mag01[y & 0x1];
    }
    for (;kk<N-1;kk++) {
      y = (ptgfsr[kk]&UPPER_MASK)|(ptgfsr[kk+1]&LOWER_MASK);
      ptgfsr[kk] = ptgfsr[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1];
    }
    y = (ptgfsr[N-1]&UPPER_MASK)|(ptgfsr[0]&LOWER_MASK);
    ptgfsr[N-1] = ptgfsr[M-1] ^ (y >> 1) ^ mag01[y & 0x1];

    for ( k = 0 ; k < N ; k++ )
    { y = ptgfsr[k];
      y ^= TEMPERING_SHIFT_U(y);
      y ^= TEMPERING_SHIFT_S(y) & TEMPERING_MASK_B;
      y ^= TEMPERING_SHIFT_T(y) & TEMPERING_MASK_C;
      y &= 0xffffffff; /* you may delete this line if word size = 32 */
      y ^= TEMPERING_SHIFT_L(y);
      ptgfsr[k] = y;
    }

    for ( kr = 0 ; kr < NR ; kr++ )
      drands[kr] = (ptgfsr[2*kr]*rfactor + ptgfsr[2*kr+1])*rfactor;
    
    k = 0;
    kr = 0;
  }
  
  return drands[kr++];
}

#ifdef MSC
/* for METIS and other third parties */
double drand48() 
{ return (double)kb_drand(); }
void srand48(int seed) 
{ kb_initr(seed); }
#endif

/***********************************************************************
*
* function: wrap_vertex()
*
* purpose: Adjust vertex position by given wrap.
*          Volconst adjustments taken care of in volume calculation.
*
*/

void wrap_vertex(v_id,wrap)
vertex_id v_id;
int wrap;
{ int i;
  REAL newx[MAXCOORD];
  REAL *x = get_coord(v_id);
  edge_id e_id,start_e;

  if ( (web.modeltype != LINEAR) && (web.modeltype != QUADRATIC) )
    kb_error(4323,
      "Can only do vertex wrap in LINEAR and QUADRATIC models so far.\n",
         RECOVERABLE);

  if ( !web.symmetry_flag ) return;
  if ( get_vattr(v_id) & (Q_MIDPOINT|Q_MIDEDGE|Q_MIDFACET) ) return;

  ENTER_GRAPH_MUTEX;
  sym_wrap(x,newx,wrap);
  for ( i = 0 ; i < SDIM ; i++ ) x[i] = newx[i];
  start_e = e_id = get_vertex_edge(v_id);
  do
  { set_edge_wrap(e_id,sym_compose(wrap,get_edge_wrap(e_id)));
    if ( (web.modeltype == QUADRATIC) && !inverted(e_id) )
    { x = get_coord(get_edge_midv(e_id));
      sym_wrap(x,newx,wrap);
      for ( i = 0 ; i < SDIM ; i++ ) x[i] = newx[i];
    }
    e_id = get_next_tail_edge(e_id);
  } while ( !equal_id(start_e,e_id) );
  LEAVE_GRAPH_MUTEX;
}



