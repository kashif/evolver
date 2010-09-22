
/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

int crit_count; /* debugging */

/**********************************************************
*
*  File: tmain.c
*
*  Contents:  main() for Surface Evolver
*/

#if defined(WIN32) && !defined(WINVER)
#define _WIN32_WINNT 0x0500
#define _WIN32_WINDOWS 0x0410
#define WINVER 0x0400
#endif
#include "include.h"

int just_lex_flag = 0;  /* for -l option */

/***********************************************************************/
/* System-specific thread stuff is all in this file.                   */

#ifdef WINTHREADS
/* MS Windows multithreading */
DWORD WINAPI winthread_worker( void * arg );
CRITICAL_SECTION thread_cs;  /* main thread critical section */
CRITICAL_SECTION element_cs;  /* per element critical section */
CRITICAL_SECTION web_cs;  /* web structure critical section */
LPCRITICAL_SECTION element_mutex_ptr = &element_cs; /* for use in other files */
LPCRITICAL_SECTION web_mutex_ptr = &web_cs; /* for use in other files */
HANDLE workthread_wakeup; /* worker thread wake-up event */
HANDLE mainthread_wakeup; /* resume main thread event */
HANDLE barrier_event;  /* for synchronizing worker threads */
volatile LONG barrier_count;
HANDLE *threadlist;   /* thread handles */
DWORD thread_data_key;
#endif
#ifdef PTHREADS
/* portable Unix multithreading */
#include <semaphore.h>
/*#include <asm/msr.h> */
pthread_mutex_t thread_mutex;  /* main thread critical section */
pthread_mutex_t element_mutex;  /* element modify critical section */
pthread_mutex_t web_mutex;  /* web modify critical section */
void *element_mutex_ptr = (void*)&element_mutex; /* for use in other files */
void *web_mutex_ptr = (void*)&web_mutex; /* for use in other files */
pthread_cond_t workthread_wakeup; /* worker thread wake-up event */
pthread_cond_t mainthread_wakeup; /* resume main thread event */
pthread_t *threadlist;   /* thread ids */
void * pthread_worker ( void *);
pthread_key_t thread_data_key;
int task_serial_number; /* global */
#endif

int spincount = 0;  /* times to try spinlock before sleeping */

/* End of thread stuff.                                                */
/***********************************************************************/



/* Separate thread for periodically printing state; useful when deadlocked */
int web_locks;
int thread_locks;
int element_locks;
int web_unlocks;
int thread_unlocks;
int element_unlocks;

void * reporter_thread ARGS((void * ));

void * reporter_thread(void * arg)
{ puts("Reporter thread running.");
  
 for (;;)
 {
#ifdef WIN32
     Sleep(10000);
#else
  sleep(10);
#endif
  printf("Reporter: global_id %08LX\n",global_id);
  printf("web_locks %d  element_locks %d  thread_locks %d\n",
          web_locks,element_locks,thread_locks);
  printf("web_unlocks %d  element_unlocks %d  thread_unlocks %d\n",
          web_unlocks,element_unlocks,thread_unlocks);
#ifdef XXXX
  if ( pthread_mutex_trylock(&thread_mutex) == EBUSY )
    puts("thread_mutex locked");
  else
  { pthread_mutex_unlock(&thread_mutex);
    puts("thread_mutex unlocked");
  }
  if ( pthread_mutex_trylock(&web_mutex) == EBUSY )
    puts("web_mutex locked");
  else
  { pthread_mutex_unlock(&web_mutex);
    puts("web_mutex unlocked");
  }
  if ( pthread_mutex_trylock(&element_mutex) == EBUSY )
    puts("element_mutex locked");
  else
  { pthread_mutex_unlock(&element_mutex);
    puts("element_mutex unlocked");
  }
#endif
 }
}
int tty_flag;  /* to do tty interface instead of menus */

#if  defined(MOTIF) || defined(MAC_APP) || defined(WIN32S) || defined(MAC_CW)
#define main old_main
#endif

#ifdef SHARED_MEMORY
void m_set_idlist()
{ proc_ids[GET_THREAD_ID] = getpid(); }
#endif

#if defined(MPI_EVOLVER)
#define main old_main 
#endif
 
int main ARGS2((argc,argv),
int argc,
char *argv[]) 
{ int pause_flag=0;  /* whether to pause after banner message */

  msgmax = 2000; 
  if ( !msg ) msg = my_list_calloc(1,msgmax,ETERNAL_BLOCK); 
  set_ctypes();

  outfd = stdout;  
  sprintf(msg,"Surface Evolver %s\n\n",VERSION);
  outstring(msg);
#ifdef LONGDOUBLE
  sprintf(msg,"Compiled for %d byte long double.\n\n",sizeof(REAL));
  outstring(msg);
#endif

#ifdef MPI_EVOLVER
  MPI_Barrier(MPI_COMM_WORLD); /* wait for everybody to print */
#endif


 if ( sizeof(element_id) > sizeof(REAL) )
    kb_error(3102,"Bad datatype sizes: element_id is %d bytes, but REAL is %d.\n",
      UNRECOVERABLE);
      
#ifdef _HISTORY_H_
 /* readline history initialization */
 using_history(); 
#endif

#ifdef SGI_MULTI
  procs_requested = 1;  /* default at John's request.  m_get_numprocs(); */
#endif

#ifdef WIN32
/* Mutex for use between graph thread and main thread. */
/* Nameless to prevent interference between separate processes. */
  graphmutex = CreateMutex(NULL,0,NULL);
  mem_mutex = CreateMutex(NULL,0,NULL);
#if defined(PROF_EVALS) || defined(PROFILING)
  SetThreadAffinityMask(GetCurrentThread(),1);
#endif
#ifdef MPI_EVOLVER
  mpi_mutex = CreateMutex(NULL,0,NULL);
#endif
#elif defined(PTHREADS)
  pthread_mutex_init(&graphmutex,NULL);
  pthread_mutex_init(&mem_mutex,NULL);
#ifdef MPI_EVOLVER
  pthread_mutex_init(&mpi_mutex,NULL);
#endif
#endif

  print_express(NULL,0); /* just to initialize string allocation */

  /* find machine resolution */
  { REAL eps,one = 1.0;
    for ( eps = 1.0 ; one + eps != one ; eps /= 2.0 ) ;
    machine_eps = 2.0*eps;
  }

  find_cpu_speed();

  initialize_perm_globals(); /* put some internal variable names in
     permanent symbol table */
  
  /* parse command line options */
  if ( argc > 0 )
  {
     argv++; argc--;
     while (  argc && (argv[0] != NULL) && (argv[0][0] == '-') )
     { switch ( argv[0][1] )
       {  
          case 'E': err_tok_gen_flag = 1;
                    break;
          case 'a': auto_convert_flag = (argv[0][2]=='-') ? 0 : 1; break;
          case 'q': option_q = (argv[0][2]=='-') ? 0 : 1; break; 
          case 'Q': quiet_load_flag = 1; break;
          case 'e': echo_flag = 1; break;
		  case 'Z': pause_flag = 1; break;  /* chance to attach debugger */
#ifdef MPI_EVOLVER
          case 'z': mpi_debug = 1; break;
#endif
          case 't': tty_flag = 1;
                break;
          case 'u': tty_flag = 1; 
                break;
          case 'f' : /* commands from file */
                 cmdfilename = argv[0]+2;
                 if ( cmdfilename[0] == 0 ) /* probably a space inserted */
                    { cmdfilename = *++argv; argc--; }
                 break;
          case 'd' :  /* parser debug */
                 yydebug = 1;
                 break;
          case 'i' : match_id_flag = 1; break;
          case 'I' : sparse_ibase_flag = 1; break;
          case 'p' : procs_requested = atoi(argv[0]+2); 
#if defined(SGI_MULTI) || defined (THREADS)
                if ( procs_requested < 1 )
                  { kb_error(1321, 
                      "-p with nonpositive number. Threads set to 1.\n",
                                    WARNING);
                     procs_requested = 1;
                  }
                if ( procs_requested > MAXPROCS )
                { sprintf(errmsg,
    "This Evolver only compiled for a maximum of %d threads.\n",MAXPROCS);
                   kb_error(2551,errmsg,WARNING);
                  sprintf(errmsg,
     "Threads set to %d. Recompile with -DMAXPROCS=%d if you want more.\n",
                    MAXPROCS,procs_requested);
                  erroutstring(errmsg);
                  procs_requested = MAXPROCS;
               }
                  
#ifdef THREADS
                threadflag = 1;
#endif
#else
                kb_error(1322,"-p option not effective.  This Evolver not compiled for multithreading.\n", WARNING);

#endif
                break; 
          case 'x' : exit_after_error = 1; break;
          case 'w' : exit_after_warning = exit_after_error = 1; break;
          case 'y' : break_after_warning = 1; break;
          case 'm' : memdebug = 1; break;
          case 'l' : just_lex_flag = 1; break;
          default:
                 sprintf(msg,"Illegal option: %s\n",argv[0]); outstring(msg);
          case 'h' :
                 outstring("Legal options: \n"); 
                 outstring("  -ffilename          take commands from file\n"); 
                 outstring("  -i                     use id numbers as in datafile\n"); 
                 outstring("  -q                     convert to named quantities\n"); 
                 outstring("  -a                     auto convert to named quantities when needed\n"); 
                 outstring("  -x                     exit after error\n"); 
                 outstring("  -w                     exit after warning\n"); 
                 outstring("  -y                     break after warning\n"); 
                 outstring("  -d                     parser debugging on\n"); 
                 outstring("  -m                     memory debugging on\n"); 
#ifdef SGI_MULTI
                 outstring("  -pn                    use n processes \n"); 
#endif
#if defined(THREADS)
                 outstring("  -pn                    use n worker threads \n"); 
#endif
                 outstring("  -h                     print this help\n"); 
                 break;
            }
        argv++; argc--;
     }
  }

  if ( pause_flag )
  { prompt("Hit ENTER to continue.\n",msg,sizeof(msg));
  }

#ifdef SGI_MULTI
  sprintf(msg,"Using %d processes on %d processors.\n\n",
      procs_requested,m_get_numprocs()); 
  outstring(msg);
  m_set_procs(procs_requested);
  if ( m_get_numprocs() > 1 )
  { int n;
     /* set up list of locks available for critical sections */
     usconfig(CONF_INITSIZE,200*_MAXLOCKS);
     usconfig(CONF_ARENATYPE,US_SHAREDONLY);
     usconfig(CONF_INITUSERS,4+m_get_numprocs());
     lock_arena = usinit(lock_arena_name);
     if ( lock_arena == NULL ) { perror(lock_arena_name); exit(2); }
     for ( n = 0 ; n < _MAXLOCKS ; n++ )
     { locklist[n] = usnewlock(lock_arena);
        if ( locklist[n] == NULL )
        { fprintf(stderr,"lock allocation failure on lock %d.\n",n);
          perror("usnewlock");
          exit(2);
        }
     }
     m_fork(m_set_idlist);
     m_park_procs();
     mpflag = M_INACTIVE;
  }
#endif
  nprocs = procs_requested;

#ifdef WINTHREADS
  /* Set up worker threads for MS-Windows multiprocessor machines */
  thread_data_key = TlsAlloc();
  TlsSetValue(thread_data_key,(void*)&default_thread_data);  // for main thread
  if ( threadflag )
  { int i;
    int retval;
    SYSTEM_INFO si;
    GetSystemInfo(&si);
    workthread_wakeup = CreateEvent(NULL,TRUE,FALSE,NULL);
    mainthread_wakeup = CreateEvent(NULL,TRUE,TRUE,NULL);
    barrier_event = CreateEvent(NULL,TRUE,TRUE,NULL);

#if _WIN32_WINNT >= 0x0500  
    { retval = InitializeCriticalSectionAndSpinCount(&thread_cs,spincount); 
      retval = InitializeCriticalSectionAndSpinCount(&element_cs,spincount); 
      retval = InitializeCriticalSectionAndSpinCount(&web_cs,spincount); 
    }
#else
    { InitializeCriticalSection(&thread_cs); 
      InitializeCriticalSection(&element_cs); 
      InitializeCriticalSection(&web_cs); 
    }
#endif
    threadlist = (HANDLE*)my_list_calloc(procs_requested,sizeof(HANDLE),
                       ETERNAL_BLOCK);
    thread_data_ptrs = (struct thread_data**)my_list_calloc(procs_requested,
                      sizeof(struct thread_data*),ETERNAL_BLOCK);
    busythreads = procs_requested;
    barrier_count = procs_requested;
    ResetEvent(barrier_event);
    for ( i = 0 ; i < procs_requested ; i++ )
    { DWORD tid;  /* dummy for thread id */
      thread_data_ptrs[i] = 
           (struct thread_data *)my_list_calloc(1,sizeof(struct thread_data),
                 ETERNAL_BLOCK);
      thread_data_ptrs[i]->worker_id = i;
      threadlist[i] = CreateThread(NULL,0,winthread_worker,
                         thread_data_ptrs[i],0,&tid);  
      if ( threadlist[i] == NULL )
      { FormatMessage( 
           FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
           NULL, GetLastError(),
           MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), /* Default language */
           (LPTSTR) &errmsg, ERRMSGSIZE, NULL );
        erroutstring("Cannot create worker threads.\n");
        kb_error(2190,errmsg,UNRECOVERABLE);
      }
    }
    WaitForSingleObject(mainthread_wakeup,INFINITE);
    sprintf(msg,"Created %d worker threads on %d processor machine.\n",
        procs_requested,si.dwNumberOfProcessors);
    if ( si.dwNumberOfProcessors == 1 ) 
       spincount = 1;  /* spin worthless on 1 cpu */
    else spincount = 4000;
    outstring(msg);
  }
#endif

#ifdef PTHREADS
  /* Set up worker threads for Unix multiprocessor machines with pthreads */
  pthread_key_create(&thread_data_key,NULL);
  pthread_setspecific(thread_data_key,(void*)&default_thread_data); // for main thread
  if ( threadflag )
  { int i;
    pthread_cond_init(&mainthread_wakeup,NULL);
    pthread_cond_init(&workthread_wakeup,NULL);

    pthread_mutex_init(&thread_mutex, NULL);
    pthread_mutex_init(&element_mutex, NULL);
    threadlist = (pthread_t *)my_list_calloc(procs_requested+10,sizeof(int),
              ETERNAL_BLOCK);
    thread_data_ptrs = (struct thread_data**)my_list_calloc(procs_requested,
                      sizeof(struct thread_data *),ETERNAL_BLOCK);
    busythreads = procs_requested;  /* countdown barrier */
    pthread_mutex_lock(&thread_mutex); 
    for ( i = 0 ; i < procs_requested ; i++ )
    { int ret;  /* dummy for thread id */
      thread_data_ptrs[i] = (struct thread_data *)my_list_calloc(1,
                sizeof(struct thread_data),ETERNAL_BLOCK);
      thread_data_ptrs[i]->worker_id = i;
      ret = pthread_create(threadlist+i,NULL,pthread_worker,
               (void*)(thread_data_ptrs[i]));
      if ( ret != 0 )
      {
        kb_error(2191,"Cannot create worker threads.\n",UNRECOVERABLE);
      }
    }
    /* wait for workers to begin waiting; */
    pthread_cond_wait(&mainthread_wakeup,&thread_mutex);
    pthread_mutex_unlock(&thread_mutex);

    sprintf(msg,"Created %d worker threads.\n", procs_requested);
    outstring(msg);
  }
#endif

  /* If not doing multiple processes then have a thread data
     structure so we don't have to write different code
     for threading and nonthreading various places */
  if ( thread_data_ptrs == NULL )
  { thread_data_ptrs = &default_thread_data_ptr;
    thread_data_ptrs[0] = &default_thread_data;
    #ifdef WINTHREADS
    /* set per-thread data */
    TlsSetValue(thread_data_key,(void*)&default_thread_data);
    #endif
    #ifdef PTHREADS
    /* set per-thread data */
    pthread_setspecific(thread_data_key,(void*)&default_thread_data);
    #endif
  }


  signal(SIGINT,catcher);    /* to catch user interrupt */     
#ifdef SIGUSR1
  signal(SIGUSR1,catcher);    /* to catch user interrupt */     
#endif
#ifdef SIGALRM
  signal(SIGALRM,catcher);    /* to catch alarm clock */     
#endif
#ifdef SIGTERM
  signal(SIGTERM,catcher);    /* to catch user interrupt, dump and kill  */     
#endif
#ifdef SIGHUP
  signal(SIGHUP,catcher);    /* to catch user interrupt, dump and kill  */     
#endif
#ifdef SIGPIPE
  signal(SIGPIPE,catcher);    /* to catch broken pipe */     
#endif

  ENTER_GRAPH_MUTEX;
  scoeff_init();
  vcoeff_init();  
  reset_web();  /* in case no datafile on command line */
  init_view();
  LEAVE_GRAPH_MUTEX;
 
  if ( argc && argv &&  argv[0] && argv[1] )
     kb_error(1323,"Extra command line arguments ignored.\n",WARNING);


  /* command sources stack */
  push_commandfd(stdin,"stdin");

  subshell_depth = 0;
  if ( cmdfilename )    /* trap back to here if error and skip cmdfilename */
  { if ( !setjmp(jumpbuf[subshell_depth]) )    
      push_commandfd(NULL,cmdfilename);
  }

#ifdef __cplusplus
  char *to_load = (argc > 0) ? argv[0] : NULL; 
  for ( ;; )
  {
    try
    {
      startup(to_load);
      datafile_flag = 0;
      /* use C++ exception mechanism instead of setjmp/longjmp */
      for (;;)
      {
        try
        { 
          exec_commands(NULL,"Enter command: "); /* command read/execute loop */
        }
        catch ( int k )
        { 
        }
      }
    }
    catch ( loadexcep k )  /* corresponds to loadjumpbuf */
    { /* LOAD command returns here */
      if ( list && (list != permlist)) 
      { myfree((char*)list); list = NULL; }/* plug memory leak */
      to_load = loadfilename[0] ? loadfilename : NULL;
    }
    catch ( int j )   /* corresponds to jumpbuf */
    { to_load = NULL;
    }
  }

#else
  if ( setjmp(loadjumpbuf) )
  { /* LOAD command returns here */
    if ( list && (list != permlist)) 
    { myfree((char*)list); list = NULL; }/* plug memory leak */
#ifdef MPI_EVOLVER
    if ( this_task == MASTER_TASK )
       mpi_loadfile();
#endif
    startup(loadfilename);

  }
  else
  { if ( setjmp(jumpbuf[subshell_depth]) )    /* return here after datafile errors */
    { 
        startup(NULL);
    }
    else
    { if ( argc > 0 ) 
	    startup(argv[0]);
      else
	  {
#ifdef MPI_EVOLVER
		  if ( this_task != MASTER_TASK )
		    return 0; /* let master task ask for datafile */  
#endif
		  startup(NULL);
       }

    }
  }
  datafile_flag = 0;
#endif

#ifdef MPI_EVOLVER
//  MPI_Barrier(MPI_COMM_WORLD);
  if ( this_task > 0 )
    return 0;   /* return to mpi main() */
  calc_energy();  /* initial energy */
#endif

  subshell_depth = 0;
#ifdef __cplusplus
#else
  /* return here after commandfile  errors */
  while ( setjmp(jumpbuf[subshell_depth]) != 0 );   
  exec_commands(NULL,"Enter command: ");  /* command read and execute loop */
#endif

  my_exit(0);

  return 0; /* success return code */
}

#ifdef __cplusplus
void loadstub()
{ struct loadexcep something;
  throw something;
}
#endif

/********************************************************************
*
*  function: my_exit()
* 
*  purpose: graceful exit from program
*/

void my_exit ARGS1((code),
int code)
{
  if ( OOGL_flag ) End_OOGL();

#ifdef SIGTERM
  signal(SIGTERM,SIG_DFL);
#endif

#ifdef SGI_MULTI
  if ( nprocs > 1)
     { m_rele_procs();
       m_kill_procs();  /* kill any parallel threads */
     }
#endif

#if defined(MPI_EVOLVER)
  mpi_my_exit();
#endif

#ifdef USE_READLINE /* CSL */
  save_readline_history();
#endif
  

  exit(code);
}

/********************************************************************
*
*  function: exec_commands()
*
*  purpose: reads and executes commands from input.
*              pops command file stack whenever end of file.
*/

int exec_commands ARGS2((basefd,promptstring),
FILE *basefd,  /* stop when get back to this input source */
char *promptstring)  /* prompt to use */
{ /* main event loop of program */
  while ( commandfd && (commandfd != basefd))
  {
    char response[200];

    datafile_flag = 0;  /* in case of error bailout sometimes */
    if ( !subshell_depth )
    {
      temp_free_all(); /* stray memory blocks */
      free_discards(DISCARDS_ALL); /* from previous cycle */
      memset(response,0,sizeof(response));
    }
    if ( prompt(promptstring,response,sizeof(response)) == EOF ) 
      pop_commandfd();
    else 
      if ( old_menu(response) == END_COMMANDS )
         return END_COMMANDS;
  }
  return 0;
}

#ifdef MAC_OS_X
/********************************************************************
*
* function: mac_exec_commands()
*
* purpose: wrapper for exec_commands so Mac main thread (which is
*          the draw thread) can call with one argument.
*/
struct thread_data mac_exec_thread_data;
void *mac_exec_commands ARGS1((arg),
FILE *arg)
{

#ifdef __cplusplus
  char *to_load = NULL;
  int flag; /* so doesn't prompt user for input file first time, if have */
  pthread_setspecific(thread_data_key,(void*)&mac_exec_thread_data);
  flag = !datafilename[0];
  for ( ;; )
  {
    if ( flag )
       startup(to_load);
    flag = 1;
    try
    {
      /* use C++ exception mechanism instead of setjmp/longjmp */
      for (;;)
      {
        try
        { 
          chdir(curdir);
          exec_commands(NULL,"Enter command: "); /* command read/execute loop */
        }
        catch ( int k )
        { 
        }
      }
    }
    catch ( loadexcep k )  /* corresponds to loadjmpbuf */
    { /* LOAD command returns here */
      if ( list && (list != permlist)) 
      { myfree((char*)list); list = NULL; }/* plug memory leak */
      to_load = loadfilename[0] ? loadfilename : NULL;
    }
    catch ( int j )   /* corresponds to jumpbuf */
    { to_load = NULL;
    }
  }

#else
  pthread_setspecific(thread_data_key,(void*)&mac_exec_thread_data);
  if ( setjmp(loadjumpbuf) )
  { /* LOAD command returns here */
    if ( list && (list != permlist)) 
    { myfree((char*)list); list = NULL; }/* plug memory leak */
    startup(loadfilename);

  }
  else
  { if ( setjmp(jumpbuf[subshell_depth]) )    /* return here after datafile errors */
    { 
       exec_commands(NULL,"Enter command: "); /* command read/execute loop */
       // startup(NULL);
    }
    else
    { if ( do_show_flag == 1 )
         do_show(); 
      chdir(curdir);   /* so command thread gets proper directory */
      exec_commands(NULL,"Enter command: "); /* command read/execute loop */
     //  startup(NULL);
    }
  }
  datafile_flag = 0;
#endif

#ifdef MPI_EVOLVER
  MPI_Barrier(MPI_COMM_WORLD);
  if ( this_task > 0 )
    return 0;   /* return to mpi main() */
  calc_energy();  /* initial energy */
#endif

#ifdef __cplusplus
#else
  while ( setjmp(jumpbuf[subshell_depth]) != 0 );    /* return here after commandfile  errors */
  exec_commands(NULL,"Enter command: ");  /* command read and execute loop */
#endif

  return NULL;
}
#endif

/********************************************************************
*
*  function: exec_file()
*
*  purpose: reads and executes commands from a file.
*              to be used by read "filename" command.
*              can be used in the middle of executing a command.
*/

void exec_file ARGS2((fd,name),
FILE *fd,  /* file, if already opened, like stdin */
char *name) /* file name, if not already opened */
{ int old_read_depth = read_depth;
  push_commandfd(fd,name);
  do  /* main event loop of program */
  {
    char response[200];

    temp_free_all(); /* stray memory blocks */
    free_discards(DISCARDS_SOME); /* from previous cycle */
    memset(response,0,sizeof(response));
    if ( prompt("Enter command: ",response,sizeof(response)) == EOF ) 
      pop_commandfd();
    else 
      old_menu(response);
  }
  while ( read_depth > old_read_depth ); 
}

/****************************************************************
*
* Function: startup()
*
* Purpose:  Start new datafile.
*
*****************************************************************/

void startup ARGS1((file_name),
char *file_name)  /* NULL if need to ask for name */
{
  char *name = file_name;
  char response[100];
  FILE *newfd;

  /* be sure graphics thread is ok before loading new file */
  ENTER_GRAPH_MUTEX;
  LEAVE_GRAPH_MUTEX;

  datafile_flag = 0;
  /* close leftover input files */
  for ( ; read_depth > (cmdfilename ? 2 : 1) ; read_depth-- ) 
      fclose(cmdfile_stack[read_depth-1].fd);

file_retry:
  if ( name == NULL )
  { char *c;
#ifdef MPI_EVOLVER
    if ( this_task != MASTER_TASK )
       return; /* wait for master to get new name */
#endif
    prompt("Enter new datafile name (none to continue, q to quit): ",
       response,sizeof(response));
    c = strchr(response,'\n');
    if ( c ) *c = 0;
    c = strchr(response,' ');
    if ( c ) *c = 0;
    if ( (strcmp(response,"q") == 0) || (strcmp(response,"quit")==0)
           || (strcmp(response,"bye")==0) || (strcmp(response,"exit")==0))
             my_exit(0);
    else if ( !response[0] ) return; /* continue same */
    name = response;
#ifdef MPI_EVOLVER
	strcpy(loadfilename,name);
	mpi_loadfile();
#endif
  }
  newfd = path_open(name,SETDATAFILENAME);
  if (newfd == NULL)
  { if ( name[0] )
    { 
	  sprintf(msg,"Cannot open datafile %s.\n",name);
      erroutstring(msg);
      if ( exit_after_error ) my_exit(1);
      name = NULL;
      while ( read_depth > (cmdfilename ? 2 : 1)  ) pop_commandfd(); 
      goto file_retry;
    }
    return; /* continue with old */
  }

#ifdef WIN32
#ifdef MPI_EVOLVER
  sprintf(msg,"Surface Evolver MPI - %s",name);
#else
  sprintf(msg,"Surface Evolver - %s",name);
#endif
  SetConsoleTitle(msg);
#endif

  ENTER_GRAPH_MUTEX;
  reset_web(); 
  init_view();

if (memdebug) memory_report();

  push_commandfd(newfd,name); /* start #include stack */

#ifdef __WIN32__
if ( heapcheck() < 0 )
  kb_error(1324,"Internal error: Corrupt heap.\n",UNRECOVERABLE);

#endif

  datafile_flag = 1;  /* so parser knows */
  datafile_input_flag = 1;  /* so lex input knows */
  cmdptr = 0;

  initialize();
  LEAVE_GRAPH_MUTEX;

  datafile_flag = 0;
  if ( read_depth > 0 )
        cmdfile_stack[read_depth-1].line = line_no;

  if ( datafile_view_flag )
  { int i,j;
    REAL sum; 

    for ( sum = 0.0, i = 0 ; i < 3 ; i++ )
      for ( j = 0 ; j < 3 ; j++ ) sum += fabs(view[i][j]);
    overall_size = 3/sum;
    dont_resize_flag = 1;
  }
  else
  { 
    resize(); 
  }
  graph_new_surface(); /* tell graphics we have new one */

  if ( parse_errors ) 
  { while ( (read_depth > 1) ) pop_commandfd();
     kb_error(1325,"Invalid datafile.  Surface may not be in a consistent state.\n",RECOVERABLE);
  }

  run_checks();
  update_display();
  
  #ifndef MPI_EVOLVER
  calc_content(Q_ENERGY|Q_FIXED|Q_INFO|Q_RENORMALIZE);
  if ( web.torus_flag ) fix_volconst();
  calc_pressure();
  calc_energy();  /* just to get initial total area */
  target_length = web.total_area; /* for square curvature string model */
  if ( OOGL_flag ) ask_wrap_display();
  #endif 
}

#ifdef THREADS
/****************************************************************************
*****************************************************************************
    Multi-threading for multi-processing with shared memory.
****************************************************************************/

void thread_stage_setup ARGS((void));

/*****************************************************************************
*
* function: task_caller()
*
* purpose: provide common place for various type worker threads to
*          call the desired task.
*/
void task_caller ARGS1((thread_tasknum),
int thread_tasknum)
{ 
  struct thread_data *data = GET_THREAD_DATA;
__int32 task_caller_elapsed_time[2];
__int32 now[2];
task_caller_elapsed_time[0] = 0;
task_caller_elapsed_time[1] = 0;
data->task_state = 1;

PROF_NOW(now);
#ifdef _MSC_VER
data->stagestart[0] = *(__int64*)now;
#endif

PROF_START(task_caller)
  switch ( thread_tasknum )
    {
      case TH_PROJECT_ALL_TEST: thread_project_all(TEST_MOVE); break;
      case TH_PROJECT_ALL_ACTUAL: thread_project_all(ACTUAL_MOVE); break;
      case TH_CALC_FACET_ENERGY:
             data->total_energy = data->total_area = 0.0;
             thread_calc_facet_energy(); break;
      case TH_CALC_FACET_FORCES: thread_calc_facet_forces(); break;
      case TH_MULTI_CALC_QUANT: multi_calc_quants(m_type); break;
      case TH_MULTI_QUANT_GRADS: m_calc_quant_grads(m_type); break;
      case TH_FIX_GRADS: m_fix_grads(); break;
      case TH_MULTI_QUANT_HESS: m_calc_quant_hess(m_type,m_mode,m_rhs); break;
      case TH_MOVE_VERTICES: thread_move_vertices(); break;
      default: sprintf(errmsg,"Invalid thread task: %d\n",thread_tasknum);
               kb_error(2192,errmsg,UNRECOVERABLE);
    }
PROF_FINISH(task_caller)

#ifdef  ZZZZZ
if(verbose_flag)
{ char *taskname;
  switch ( thread_tasknum )
    {
      case TH_PROJECT_ALL_TEST: taskname="thread_project_all"; break;
      case TH_PROJECT_ALL_ACTUAL: taskname="thread_project_all"; break;
      case TH_CALC_FACET_ENERGY:
             taskname="thread_calc_facet_energy"; break;
      case TH_CALC_FACET_FORCES: taskname="thread_calc_facet_forces"; break;
      case TH_MULTI_CALC_QUANT: taskname="multi_calc_quants"; break;
      case TH_MULTI_QUANT_GRADS: taskname="m_calc_quant_grads"; break;
      case TH_FIX_GRADS: taskname="m_fix_grads"; break;
      case TH_MULTI_QUANT_HESS: taskname="m_calc_quant_hess"; break;
      default: taskname="default";
    }
printf("%30s",taskname);
PROF_PRINT(task_caller)
}
#endif

  data->task_state = 2;

}
#endif

#ifdef WINTHREADS
/*****************************************************************************
*
* function: winthread_worker()
*
* purpose: Main winthread worker function.  Waits for wakeup call and
*          then executes task.
*
*/

DWORD WINAPI winthread_worker ( void * arg )
{ struct thread_data *data = (struct thread_data *)arg;

  /* set per-thread data */
  TlsSetValue(thread_data_key,(void*)data);
 
  /* Set affinity mask.  Actually seems to hurt. */ 
  /*
  SetThreadAffinityMask(threadlist[data->worker_id],1<<data->worker_id);
  */
  
  /* Loop waiting for wakeup calls.  Use single event rather than event */
  /* for each type of task since MAXIMUM_WAIT_OBJECTS is 64. */
  for (;;)
  { 
    InterlockedDecrement(&busythreads);
    if ( busythreads == 0 )
    { 
      ResetEvent(workthread_wakeup); 
      SetEvent(barrier_event);
    }
    WaitForSingleObject(barrier_event,INFINITE);
    InterlockedDecrement(&barrier_count);
    if ( barrier_count == 0 )
       SetEvent(mainthread_wakeup);
    WaitForSingleObject(workthread_wakeup,INFINITE);
    task_caller(thread_task);
  }
}
#endif

#ifdef PTHREADS
/*****************************************************************************
*
* function: pthread_worker()
*
* purpose: Main pthreads worker function.  Waits for wakeup call and
*          then executes task.
*          arg is thread data area.
*/

void *pthread_worker( void * arg )
{ struct thread_data *data = (struct thread_data *)arg;

  /* set per-thread data */
  pthread_setspecific(thread_data_key,(void*)data);
  
  for (;;)
  { 
    /* barrier song and dance to let main thread know workers are waiting */
    pthread_mutex_lock(&thread_mutex);
    busythreads--;
    if ( busythreads == 0 )
      pthread_cond_signal(&mainthread_wakeup);
    pthread_cond_wait(&workthread_wakeup,&thread_mutex);
    pthread_mutex_unlock(&thread_mutex);

#ifdef PTHREAD_LOG
{ /* log thread event */
   rdtsc(data->events[data->eventcount].time_low,
         data->events[data->eventcount].time_high);
  data->events[data->eventcount].type = thread_task;
  data->eventcount++;
}
#endif

    task_caller(thread_task);

#ifdef PTHREAD_LOG
{ /* log thread event */
   rdtsc(data->events[data->eventcount].time_low,
         data->events[data->eventcount].time_high);
  data->events[data->eventcount].type = -thread_task;
  data->eventcount++;
}
#endif
  }
  return NULL;
} /* end pthread_worker() */
#endif

#ifdef THREADS
/**************************************************************************
*
* function: pcomp()
*
* purpose: comparison function for sorting partition coordinates.
*/
struct pcoord { REAL p_coord; vertex_id v_id; };
int pcomp ARGS((struct pcoord*,struct pcoord*));

int pcomp ARGS2((a,b),
struct pcoord *a, struct pcoord *b)
{
  if ( a->p_coord < b->p_coord ) return -1;
  if ( a->p_coord > b->p_coord ) return  1;
  return 0;
}

/**************************************************************************
*
* function: thread_stage_setup()
*
* purpose: Initialize element lists for thread stages.
*/

void thread_stage_setup()
{ int i,pcount,pspot,perstage;
  vertex_id v_id;
  edge_id e_id;
  facet_id f_id;
  int proc,stage;
  struct pcoord *pcoords;
  struct thread_stages_data *th;
  int one = 1;

  max_thread_stages = 2;  /* simple 1-D partitioning */
  
  /* Make sure vertex partitioning coord exists */
  v_partition_coord_attr = find_attribute(VERTEX,"v_partition_coord");
  if ( v_partition_coord_attr < 0 )
  { v_partition_coord_attr = add_attribute(VERTEX,"v_partition_coord",
        REAL_TYPE,0,&one,0,NULL);
  }

  /* default for now: use x coordinate as partition coordinate */
  FOR_ALL_VERTICES(v_id)
  { *((REAL*)(get_extra(v_id,v_partition_coord_attr))) = get_coord(v_id)[0];
  }
  
  v_partition_stage_attr = find_attribute(VERTEX,"v_partition_stage");
  if ( v_partition_stage_attr < 0 )
  { v_partition_stage_attr = add_attribute(VERTEX,"v_partition_stage",
        INTEGER_TYPE,0,&one,0,NULL);
  }
  v_partition_proc_attr = find_attribute(VERTEX,"v_partition_proc");
  if ( v_partition_proc_attr < 0 )
  { v_partition_proc_attr = add_attribute(VERTEX,"v_partition_proc",
        INTEGER_TYPE,0,&one,0,NULL);
  }
  
  e_partition_stage_attr = find_attribute(EDGE,"e_partition_stage");
  if ( e_partition_stage_attr < 0 )
  { e_partition_stage_attr = add_attribute(EDGE,"e_partition_stage",
        INTEGER_TYPE,0,&one,0,NULL);
  }
  e_partition_proc_attr = find_attribute(EDGE,"e_partition_proc");
  if ( e_partition_proc_attr < 0 )
  { e_partition_proc_attr = add_attribute(EDGE,"e_partition_proc",
        INTEGER_TYPE,0,&one,0,NULL);
  }
  
  f_partition_stage_attr = find_attribute(FACET,"f_partition_stage");
  if ( f_partition_stage_attr < 0 )
  { f_partition_stage_attr = add_attribute(FACET,"f_partition_stage",
        INTEGER_TYPE,0,&one,0,NULL);
  }
  f_partition_proc_attr = find_attribute(FACET,"f_partition_proc");
  if ( f_partition_proc_attr < 0 )
  { f_partition_proc_attr = add_attribute(FACET,"f_partition_proc",
        INTEGER_TYPE,0,&one,0,NULL);
  }
  
  /* Find percentiles, for load balancing */
  pcoords = (struct pcoord*)temp_calloc(web.skel[VERTEX].count,
                 sizeof(struct pcoord));
  pcount = 0;
  FOR_ALL_VERTICES(v_id)
  { pcoords[pcount].p_coord = *((REAL*)(get_extra(v_id,v_partition_coord_attr)));
    pcoords[pcount].v_id = v_id;
    pcount++;
  } 
  qsort((char*)pcoords,pcount,sizeof(struct pcoord),FCAST pcomp);
  pspot = 0;
  perstage = pcount/(nprocs*max_thread_stages) + 1;
  for ( proc = 0 ; proc < nprocs ; proc++ )
  {
    for ( stage = 0 ; stage < max_thread_stages ; stage++ )
    { 
       for ( i = 0 ;  (i < perstage) && (pspot < pcount) ; i++,pspot++ )
       { *((int *)(get_extra(pcoords[pspot].v_id,v_partition_proc_attr))) = proc;
         *((int *)(get_extra(pcoords[pspot].v_id,v_partition_stage_attr))) = stage;
       }
    }
  }
  temp_free((char*)pcoords);

  if ( thread_stages == NULL )
    thread_stages = (struct thread_stages_data*)mycalloc(nprocs,
                         sizeof(struct thread_stages_data));

  /* set up all element's stage lists */

       for ( proc = 0 ; proc < nprocs ; proc++ )
        for ( stage = 0 ; stage < max_thread_stages ; stage++ )
          thread_stages[proc].counts[VERTEX][stage] = 0;
       thread_stages[0].counts[VERTEX][max_thread_stages] = 0;
    
       FOR_ALL_VERTICES(v_id) 
       { proc = *(int*)get_extra(v_id,v_partition_proc_attr);
         stage = *(int*)get_extra(v_id,v_partition_stage_attr);
         th = thread_stages+proc;
         if ( th->counts[VERTEX][stage] >= th->allocated[VERTEX][stage] )
         { int newalloc = th->allocated[VERTEX][stage] + 10
                     + 2*web.skel[VERTEX].count/(nprocs*max_thread_stages);
           th->blocks[VERTEX][stage] = (element_id *)kb_realloc(
                (char*)(th->blocks[VERTEX][stage]),newalloc*sizeof(element_id));
           th->allocated[VERTEX][stage] = newalloc;
         }
         th->blocks[VERTEX][stage][th->counts[VERTEX][stage]++] = v_id;
       }

       for ( proc = 0 ; proc < nprocs ; proc++ )
        for ( stage = 0 ; stage < max_thread_stages ; stage++ )
          thread_stages[proc].counts[EDGE][stage] = 0;
       thread_stages[0].counts[EDGE][max_thread_stages] = 0;

       FOR_ALL_EDGES(e_id) 
       { vertex_id head = get_edge_headv(e_id); 
         vertex_id tail = get_edge_tailv(e_id); 
         int hproc = *(int*)get_extra(head,v_partition_proc_attr);
         int tproc = *(int*)get_extra(tail,v_partition_proc_attr);
         int hstage = *(int*)get_extra(head,v_partition_stage_attr);
         int tstage = *(int*)get_extra(tail,v_partition_stage_attr);
         int hband = hstage + max_thread_stages*hproc;
         int tband = tstage + max_thread_stages*tproc;
         if ( abs(hband-tband) <= 1 )
         { if ( hband < tband ) { proc = hproc; stage = hstage; } 
           else { proc = tproc; stage = tstage;}
         }
         else if ( web.symmetry_flag && (hband == 0) && 
                    (tband == nprocs*max_thread_stages-1 ) )
           { proc = tproc; stage = tstage;}
         else if ( web.symmetry_flag && (tband == 0) && 
                    (hband == nprocs*max_thread_stages-1 ) )
           { proc = hproc; stage = hstage;}
         else /* big element */
           { proc = 0; stage = max_thread_stages; }

         th = thread_stages+proc;
         if ( th->counts[EDGE][stage] >= th->allocated[EDGE][stage] )
         { int newalloc = th->allocated[EDGE][stage] + 10
                     + 2*web.skel[EDGE].count/(nprocs*max_thread_stages);
           th->blocks[EDGE][stage] = (element_id *)kb_realloc(
                (char*)(th->blocks[EDGE][stage]),newalloc*sizeof(element_id));
           th->allocated[EDGE][stage] = newalloc;
         }
         th->blocks[EDGE][stage][th->counts[EDGE][stage]++] = e_id;
         *((int *)(get_extra(e_id,e_partition_proc_attr))) = proc;
         *((int *)(get_extra(e_id,e_partition_stage_attr))) = stage;
       }

       for ( proc = 0 ; proc < nprocs ; proc++ )
        for ( stage = 0 ; stage < max_thread_stages ; stage++ )
          thread_stages[proc].counts[FACET][stage] = 0;
       thread_stages[0].counts[FACET][max_thread_stages] = 0;

       FOR_ALL_FACETS(f_id) 
       { facetedge_id fe = get_facet_fe(f_id);
         int topband = -1;
         int lowband = 1000000;
         int vproc,vstage,vband;

         for ( i = 0 ; i < FACET_VERTS ; i++ )
         { v_id = get_fe_headv(fe); 
           vproc = *(int*)get_extra(v_id,v_partition_proc_attr);
           vstage = *(int*)get_extra(v_id,v_partition_stage_attr);
           vband = vstage + max_thread_stages*vproc;
           if ( vband < lowband ) lowband = vband;
           if ( vband > topband ) topband = vband;
           fe = get_next_edge(fe);
         }

         if ( topband-lowband <= 1 )
         { proc = lowband/max_thread_stages; 
           stage = lowband % max_thread_stages;
         }
         else if ( web.symmetry_flag && (lowband == 0) && 
                    (topband == nprocs*max_thread_stages-1 ) )
         { proc = topband/max_thread_stages; 
           stage = topband % max_thread_stages;
         }
         else /* big element */
         { proc = 0; stage = max_thread_stages; }

         th = thread_stages+proc;
         if ( th->counts[FACET][stage] >= th->allocated[FACET][stage] )
         { int newalloc = th->allocated[FACET][stage] + 10
                     + 2*web.skel[FACET].count/(nprocs*max_thread_stages);
           th->blocks[FACET][stage] = (element_id *)kb_realloc(
                (char*)(th->blocks[FACET][stage]),newalloc*sizeof(element_id));
           th->allocated[FACET][stage] = newalloc;
         }
         th->blocks[FACET][stage][th->counts[FACET][stage]++] = f_id;
         *((int *)(get_extra(f_id,f_partition_proc_attr))) = proc;
         *((int *)(get_extra(f_id,f_partition_stage_attr))) = stage;
       }

  partition_timestamp = global_timestamp;

} /* end thread_stage_setup */
#endif

#ifdef WINTHREADS
/**************************************************************************
*
* function: thread_launch()
*
* purpose: Send signal to launch worker threads, after initializing
*          global iteration variable.
*/

void thread_launch(int task, int element_type)
{ int i,j,proc;
  __int32 now[2];
  
  PROF_NOW(now);
  thread_launch_start = *(__int64*)now;

  global_id = web.skel[element_type].used;  /* in case no threads */

  if ( partition_timestamp < top_timestamp )
    thread_stage_setup();
  for ( proc = 0 ; proc < nprocs ; proc++ )
    thread_stages[proc].stage = thread_stages[proc].spot = 0;

  thread_task = task; 
  ResetEvent(mainthread_wakeup);  /* just to be sure */
  busythreads = procs_requested;
  ResetEvent(barrier_event);
  barrier_count = procs_requested;
  SetEvent(workthread_wakeup);
  WaitForSingleObject(mainthread_wakeup,INFINITE);

  /* Cleanup and amalgamation of data */
  switch ( task )
  { case TH_CALC_FACET_ENERGY:
      for ( i = 0 ; i < nprocs ; i++ )
      { web.total_energy += thread_data_ptrs[i]->total_energy;
        web.total_area  += thread_data_ptrs[i]->total_area;
      }
      break;
  }

  PROF_NOW(now);
  thread_launch_end = *(__int64*)now;

if ( verbose_flag )
{
/* print detailed clock timing */
printf("thread_launch_start   %12I64X\n",thread_launch_start-thread_launch_start);
for (i = 0 ; i <= max_thread_stages ; i++ )
  printf("task 0 stage %d start: %12I64X end: %12I64X\n",i,
    thread_data_ptrs[0]->stagestart[i]-thread_launch_start,
    thread_data_ptrs[0]->stageend[i]-thread_launch_start);
for ( j = 1; j < nprocs ; j++ )
{ for (i = 0 ; i < max_thread_stages ; i++ )
    printf("task 1 stage %d start: %12I64X end: %12I64X\n",i,
      thread_data_ptrs[j]->stagestart[i]-thread_launch_start,
      thread_data_ptrs[j]->stageend[i]-thread_launch_start);
}
printf("thread_launch_end     %12I64X\n",thread_launch_end-thread_launch_start);
}

}
#endif

#ifdef PTHREADS
/**************************************************************************
*
* function: thread_launch()
*
* purpose: Send signal to launch worker threads, after initializing
*          global iteration variable.
* Note: Have to be very careful with timing, since wake up happens only
* if thread is actually waiting when signal made; signal ignored if
* made before wait starts.
*/

void thread_launch(int task, int element_type)
{ int i,proc;

  global_id = web.skel[element_type].used;
  for ( i = 0 ; i < nprocs ; i++ )
  { /* make sure we are starting with an actual element */
    while ( valid_id(global_id) && !valid_element(global_id) )
      global_id = elptr(global_id)->forechain;
 
    thread_data_ptrs[i]->iteration_id = global_id;
    if ( valid_id(global_id) )
      global_id = elptr(global_id)->forechain;
     
  }
  if ( partition_timestamp < top_timestamp )
    thread_stage_setup();
  for ( proc = 0 ; proc < nprocs ; proc++ )
    thread_stages[proc].stage = thread_stages[proc].spot = 0;

  thread_task = task; 
  pthread_mutex_lock(&thread_mutex);  /* wait for last thread to wait */
  busythreads = procs_requested;
  pthread_cond_broadcast(&workthread_wakeup);

  /* wait for workers to begin waiting; */
  pthread_cond_wait(&mainthread_wakeup,&thread_mutex);
  pthread_mutex_unlock(&thread_mutex);

#ifdef PTHREAD_LOG
{ /* log thread event */
   rdtsc(main_events[main_eventcount].time_low,
         main_events[main_eventcount].time_high);
  main_events[main_eventcount].type = 0;
  main_eventcount++;
}
#endif

  /* Cleanup and amalgamation of data */
  switch ( task )
  { case TH_CALC_FACET_ENERGY:
      for ( i = 0 ; i < nprocs ; i++ )
      { web.total_energy += thread_data_ptrs[i]->total_energy;
        web.total_area   += thread_data_ptrs[i]->total_area;
      }
      break;
  }
}
#endif

#ifndef THREADS
void thread_launch(int task, int element_type)
{ sprintf(errmsg,"Internal error. Function thread_launch called in non-threaded Evolver. Task %d, type %d.\n",task,element_type);
  kb_error(2193,errmsg,RECOVERABLE);
}
#endif



#ifdef THREADS
/**************************************************************************
*
* function: thread_next_element()
*
* purpose: Get next element on task list for a calling thread.
*          Assumes global_id initialized to first element of 
*          appropriate element list.  Goes until end of list, not
*          sentinel, so to be used for functions that don't modify
*          the element list.
*
*  Performance note: with two processors, get twice as many thread_locks
*  as thread_unlocks!!  Get about half as many web_locks as web_unlocks.
*/


element_id thread_next_element()
{ element_id id;
  int proc,nextproc;
  int eltype;
  struct thread_stages_data *th;
  struct thread_data *data;

  __int32 now[2];  /* for timing */


  /* First, in case multithreading not being used */
  if ( threadflag == 0 )
  {
    while ( valid_id(global_id) & !valid_element(global_id) )
      global_id = elptr(global_id)->forechain;
    if ( !valid_id(global_id) ) return NULLID;

    id = global_id;
    global_id = elptr(id)->forechain;
    return id;
  }

  data = GET_THREAD_DATA;
  proc = data->worker_id;
  th = thread_stages + proc;
  eltype = id_type(global_id);
  for (;;)  /* loop just for case when starting new stage */
  {
    if ( th->spot < th->counts[eltype][th->stage] )
    { id = th->blocks[eltype][th->stage][th->spot++];
      break;
    }
    else
    { th->stage++;  /* signify done */
#ifdef _MSC_VER
PROF_NOW(now);
data->stageend[th->stage-1] = *(__int64*)now;
#endif
      th->spot = 0;
      if ( th->stage == ((proc==0) ? max_thread_stages+1 : max_thread_stages) )
      { id = NULLID;
        break;
      }
      else /* look for next stage */
      { if ( th->stage < max_thread_stages )
        {  nextproc = (proc == nprocs - 1) ? 0 : proc+1;
           while ( thread_stages[nextproc].stage < th->stage ) ;  /* wait */
        }
        else /* proc 0 to do big elements */
        { for ( nextproc = 1 ; nextproc < nprocs ; nextproc++ )
           while ( thread_stages[nextproc].stage < th->stage ) ;  /* wait */
        }  
#ifdef _MSC_VER
PROF_NOW(now);
data->stagestart[th->stage] = *(__int64*)now;
#endif
      }
    }
  } 
  return id;
}
#endif

/***************************************************************************
*
*/

#ifdef WINTHREADS
/* Indirection to lock functions to avoid including windows.h everywhere */
int mylock_element(element_id id)
{  if ( TryEnterCriticalSection(element_mutex_ptr)==0 )
    {EnterCriticalSection(element_mutex_ptr);
     element_locks++;
    }
   else element_unlocks++; 
   return 0;  /* so can be used in conditionals */
}
int myunlock_element(element_id id)
{  LeaveCriticalSection(element_mutex_ptr); 
   return 0;
}
int mylock_web(void)
{  if ( TryEnterCriticalSection(web_mutex_ptr)==0 )
    {EnterCriticalSection(web_mutex_ptr);
     web_locks++;
    }
   else web_unlocks++; 
   return 0;
}
int myunlock_web(void)
{  LeaveCriticalSection(web_mutex_ptr); 
   return 0;
}
struct thread_data *win_get_thread_data(DWORD key)
{ return (struct thread_data *)TlsGetValue(key);
}
#endif

#ifdef PTHREADS
int mylock_element(element_id id)
{
  /* Using lock field in element structure.             */
  /* using lock count semantics, i.e. lock=1 is locked. */
  /* Note we have to save ebx, since caller assumes saved. */
#ifdef TIMING
  __asm__ (
  "       pushl  %ebx\n"
  "       pushl  8(%ebp)\n"
  "       call   elptr\n"
  "       addl   $4,%esp\n"
  "       movl   %eax,%ebx\n"
  "       addl   $16,%ebx\n"
  ".L8889j:   \n"
  "       movl   $1,%eax\n"
  "LOCK   cmpxchgl    %eax,0(%ebx)\n"
  "       jz     .L8889j  \n"
  "       popl   %ebx\n"
  );
#endif
  return 0;
}
#endif

/**************************************************************************
*
* function: find_cpu_speed()
* 
* purpose: tries to find out current CPU speed for profiling reports.
*/
void find_cpu_speed()
{
#if defined(_MSC_VER) && defined(PROFILING)
  { /* use high-performance counter to estimate frequency */
    LARGE_INTEGER freq,start,finish;
    __int32 profstart[2],profnow[2];
    QueryPerformanceFrequency(&freq);
    PROF_NOW(profstart);
    PROF_NOW(profnow);
    QueryPerformanceCounter(&start);
    do {
      if ( *(__int64*)profnow - *(__int64*)profstart <= 0 )
      { /* something wrong; maybe hibernated in middle of loop! */
        return;
      }
      PROF_NOW(profnow);
    }
    while ( *(__int64*)profnow - *(__int64*)profstart < 100000000 );
    QueryPerformanceCounter(&finish);
    cpu_speed = (*(__int64*)profnow-*(__int64*)profstart)/
             ((finish.QuadPart-start.QuadPart)/(double)freq.QuadPart);
  }
#else
   cpu_speed = 3e9;  /* rough guess */
#endif
}



