/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/**********************************************************************
*
*  File: userio.c
*
*  Purpose: Handles interaction with user.  All interaction goes
*              through this file for easy porting.
*/

#ifdef WIN32
#undef FIXED
#undef DOUBLE
#endif
#include "include.h"

/***********************************************************************
*
*  Function: outstring()
*
*  Purpose: Prints string for user.  Does not wait for response.
*              Does not add newline.
*/

void outstring ARGS1((outmsg),
CONST char *outmsg)
{
  if ( quiet_flag || !outmsg ) return;
  if ( broken_pipe_flag )
  { broken_pipe_flag = 0; kb_error(1349,"Broken pipe.\n",RECOVERABLE); }

  if ( outfd == NULL ) outfd = stdout;    /* maybe after error */

  if ( logfile_flag && (outfd==stdout) )
      fprintf(logfilefd,outmsg);

#if defined(MAC_APP) || defined(WIN32S) || defined(MAC_CW)
  if ( outfd == stdout )
     write_to_console(outmsg);
  else
     fputs(outmsg,outfd);
#else
  fputs(outmsg,outfd);
  fflush(outfd);
#endif
} 

/***********************************************************************
*
*  Function: erroutstring()
*
*  Purpose: Prints error string for user.  Does not wait for response.
*              Does not add newline.
*/

void erroutstring ARGS1((outmsg),
char *outmsg)
{
  if ( !outmsg ) 
	  return;
#if defined(MAC_APP) || defined(WIN32S) || defined(MAC_CW)
  write_to_console(outmsg);
#else
#ifdef MPI_EVOLVER
  fprintf(stderr,"Task %d: ",this_task);
#endif
  fputs(outmsg,stderr);
  fflush(stderr);
#endif
  if ( logfile_flag )
      fprintf(logfilefd,outmsg);

}

/***********************************************************************
*
* function: my_fgets()
*
* purpose: replacement for fgets() that recognizes various line end
* formats: CR, NL, CR NL, and converts all to NL.  Returned string
* ends with null, included in max size.
*/

char *my_fgets ARGS3((s,n,stream),
char *s,
int n,  /* max size of string */
FILE *stream) 
{ char *p;
  int k;
  for ( k = 0, p = s ; k < n-1 ; k++,p++ )
  { int c;
    c = fgetc(stream);
    if ( (c == EOF) && ( k == 0 ) ) return NULL;
    if ( c == EOF ) { *p = 0; return s; }
    *p = (char)c;
    if ( c == '\n' ) { *(++p) = 0; return s; }
    if ( c == '\r' )
     { *p = '\n'; *(++p) = 0;
        c = fgetc(stream);
        if ( (c != '\n') && (c != EOF) ) ungetc(c, stream); 
        return s; 
     }
  }
  *p = 0;  /* null terminator */
  return s;
}

/***********************************************************************
*
*  Function: getstring()
*
*  Purpose: Gets string from user. 
*/

void getstring ARGS2((inmsg,max),
char *inmsg,
int max)
{ char *c;
  max -= 2; /* ensure room */
  while ( commandfd && (commandfd != stdin) )
  { /* from command input file */
    if (my_fgets(inmsg,max,commandfd) == NULL) 
    { pop_commandfd();
      continue;  /* will fall thru to stdin eventually */
    }
    else 
    {
      if ( !topflag && !quiet_load_flag ) outstring(inmsg); /* echo */
      if ( (int)strlen(inmsg) == max-1 ) 
      {inmsg[max-1] = MOREIN; inmsg[max] = 0; }
      return;
    }
  }

  /* from stdin */
  broken_pipe_flag = 0; /* in case left over */

#if defined(MAC_APP) || defined(WIN32S) || defined(MAC_CW)
  read_line_from_console(inmsg);
#else
  if ( my_fgets(inmsg,max,stdin) == NULL )
     my_exit(0);
  c = inmsg + strlen(inmsg) - 1;
  if ( *c == '\n' ) *c = 0;
  if ( echo_flag ) { outstring(inmsg);  outstring("\n"); }
#endif

  if ( keylogfile_flag ) fprintf(keylogfilefd,"%s\n",inmsg);
}

#ifdef OOGL
/* Geomview picking stuff */
/***************************************************************************
*
* function: read_pick()
*
* purpose: Read pick message from geomview. 
*
* return: 0 for no pick, 1 for pick
*/
int read_pick()
{
  char pickbuf[500],*ptr;
  int len;
  int vnum=-1,ednum=-1,fnum=-1;
  int prim;

  len = read(gv_pipe[0],pickbuf,sizeof(pickbuf));
  if ( len > 0 )
   { 
     pickbuf[len] = 0;
     ptr = strchr(pickbuf+1,'('); /* coord of picked pt */
     if ( !ptr ) goto pick_fail_exit;
     ptr = strchr(ptr+1,'('); /* coord of picked vertex */
     if ( !ptr ) goto pick_fail_exit;
     ptr = strchr(ptr+1,'('); /* coord of picked edge */
     if ( !ptr ) goto pick_fail_exit;
     ptr = strchr(ptr+1,'('); /* coord of picked face */
     if ( !ptr ) goto pick_fail_exit;
     ptr = strchr(ptr+1,'('); /* path to picked primitive */
     if ( !ptr ) goto pick_fail_exit;
     prim = atoi(ptr+1);
     if ( setting_backcull ) prim--;
     ptr = strchr(ptr+1,')')+1; /* index of picked primitive */
     if ( !ptr ) goto pick_fail_exit;
     vnum = atoi(ptr);
     ptr = strchr(ptr+1,'('); /* endpoints of picked edge */
     if ( !ptr ) goto pick_fail_exit;
     if ( ptr[1] != ')' && (web.representation != SIMPLEX) )
     { /* figure edge from endpoints */
        int v1,v2;
        vertex_id v_id1,v_id2;
        edge_id e_id,e1;

        sscanf(ptr+1,"%d %d",&v1,&v2);
        if ( prim > 0 ) 
        { v_id1 = vpicklist[gv_vect_start+v1]; 
           v_id2 = vpicklist[gv_vect_start+v2]; 
        }
        else { v_id1 = vpicklist[v1]; v_id2 = vpicklist[v2]; }
        if ( valid_id(v_id1) && valid_id(v_id2) &&
               !(get_vattr(v_id1) & Q_MIDFACET)
                    && !(get_vattr(v_id2) & Q_MIDFACET) )
        {
           if ( web.modeltype != LINEAR )
           { edge_id e2,e_id2;
             e1 = e_id = get_vertex_edge(v_id1);
             do 
             {
                e2 = e_id2 = get_vertex_edge(v_id2);
                do
                { 
                   if ( equal_element(e_id,e_id2) )
                   { ednum = loc_ordinal(e_id) + 1; break; }
                   if ( get_vattr(v_id2) & (Q_MIDPOINT | Q_MIDEDGE) )
                    break;
                   e_id2 = get_next_tail_edge(e_id2);
                } while ( !equal_id(e2,e_id2) );

                if ( get_vattr(v_id1) & (Q_MIDPOINT | Q_MIDEDGE) )
                   break;
                e_id = get_next_tail_edge(e_id);
             } while ( (ednum < 0) &&  !equal_id(e1,e_id) );
           }
           else  /* LINEAR */
           { e1 = e_id = get_vertex_edge(v_id1);
             do 
             { if ( equal_id(v_id2,get_edge_headv(e_id)) )
                { ednum = loc_ordinal(e_id) + 1; break; }
                e_id = get_next_tail_edge(e_id);
             } while ( !equal_id(e1,e_id) );
           }
        }
     }
     ptr = strchr(ptr+1,')')+1; /* index of picked face */
     if ( !ptr ) goto pick_fail_exit;
     fnum = atoi(ptr);
     /* puts(pickbuf); */
     strcpy(msg,"Picked ");
     if ( vnum >= 0 )
     { if ( prim > 0 ) vnum += gv_vect_start;
       pickvnum = loc_ordinal(vpicklist[vnum]) + 1;
       sprintf(msg+strlen(msg),"vertex %d  ",pickvnum);
       if ( geomview_bug_flag == 1 )
       { strcat(msg,
        "\nGeomview version 1.6.1 before 1.6.1p7 does not do picking correctly.\n");
          strcat(msg,
               "Get fixed version of gvx from ftp://www.susqu.edu   /priv/slevy.\n");
         geomview_bug_flag = 2;
       }
       if ( geomview_bug_flag == 2 )
          strcat(msg," (probably wrong)");
     } 
     if ( ednum > 0 )
     { pickenum = ednum;
       sprintf(msg+strlen(msg),"edge %d  ",pickenum);
       if ( geomview_bug_flag == 1 )
       { strcat(msg,
        "\nGeomview version 1.6.1 before 1.6.1p7 does not do picking correctly.\n");
         strcat(msg,
         "Get fixed version of gvx from ftp://www.susqu.edu   /priv/slevy.\n");
         geomview_bug_flag = 2;
       }
       if ( geomview_bug_flag == 2 )
         strcat(msg," (probably wrong)");
     } 
     if ( fnum >= 0 )
     { pickfnum = loc_ordinal(fpicklist[fnum]) + 1;
        sprintf(msg+strlen(msg),"facet %d  ",pickfnum);
     } 
     strcat(msg,"\n");
     outstring(msg);
   }
  return (len > 0) ? 1 : 0;

pick_fail_exit:
  outstring("\nMessage from geomview:\n");
  outstring(pickbuf);
  if ( current_prompt ) outstring(current_prompt);
  return 0;
}

/***************************************************************************
*
* function: check_pick()
*
* purpose: Check for pick message from geomview.  Waits for input on
*             either stdin or gv_pipe.
*
* return: 0 for stdin, 1 for pick
*/
#ifdef POLLIN
extern int poll ARGS((struct pollfd *, unsigned long, int));
#endif

int check_pick()
{ 
#ifdef POLLIN
  struct pollfd pp[2];

  /* see if anything ready to read */
  pp[0].fd = 0; /* stdin */
  pp[0].events = POLLIN;
  pp[1].fd = gv_pipe[0]; /* geomview pick */
  pp[1].events = POLLIN;
  poll(pp,2,-1);
  if ( pp[1].revents ) 
     return read_pick();
#elif !defined(WIN32)
/* try using select() from time.h */
  fd_set fdset;
  struct timeval timeout;

  do
  { timeout.tv_sec = 5; timeout.tv_usec = 0; /* linux select() modifies */
    FD_ZERO(&fdset);
    FD_SET(0,&fdset);
    FD_SET(gv_pipe[0],&fdset);
  } while (  select(FD_SETSIZE,&fdset,NULL,NULL,&timeout) == 0 );
  if ( FD_ISSET(gv_pipe[0],&fdset) ) 
     return read_pick();
  else return 0;
#endif
  return 0;
}
#endif

/***********************************************************************
*
*  Function: prompt()
*
*  Purpose: Displays prompt and gets response from user.
*/
#ifdef USE_READLINE //CSL
#include "readline.c"
#endif

int prompt ARGS3((promptmsg,inmsg,max),
char *promptmsg, /* to user */
char *inmsg,    /* from user, space already allocated */
int max) /* max char, including terminal null */
{ char *c,*ptr;
  int oldquiet;
  
  inmsg[0] = 0;  /* in case no message */

#ifdef PTHREAD_LOG
/* print thread event log */
if ( threadflag )
{ int i;
  int k[MAXPROCS], kmain = 0;
  for ( i = 0 ; i < nprocs ; i++ ) k[i] = 0;
  /* print in time order */
  for(;;)
  { unsigned int lowtime = 0,hightime = 0xFFFFFFFF;
    int type, lowwhich = -1;
    struct thread_event *em;
    for ( i = 0 ; i < nprocs ; i++ ) 
    { struct thread_event *e = thread_data_ptrs[i]->events + k[i];
      if ( k[i] >=  thread_data_ptrs[i]->eventcount ) continue;
      if ( (e->time_high < hightime)
        || ((e->time_high == hightime) && ( e->time_low < lowtime ))  )
      {  lowwhich = i; hightime = e->time_high; lowtime = e->time_low;
         type = e->type;
      }
    }
    em = main_events + kmain;
    if ( (kmain < main_eventcount) && ((em->time_high < hightime)
        || ((em->time_high == hightime) && ( em->time_low < lowtime )))  )
    { printf("m %08X%08X %d\n",em->time_high,em->time_low,em->type);
      kmain++;
    }
    else if ( lowwhich >= 0 )
    { printf("%d %08X%08X %d\n",lowwhich,hightime,lowtime,type);
      k[lowwhich]++;
    }
    else break;
  }
  for ( i = 0 ; i < nprocs ; i++ ) thread_data_ptrs[i]->eventcount = 0;
  main_eventcount = 0;

}
#endif

  if ( commandfd && (commandfd != stdin) )
  { /* from command input file */
    if (my_fgets(inmsg,max,commandfd) == NULL) 
    { return EOF; }
    else 
    { oldquiet = quiet_flag; /* quiet_flag = 0; */
#ifdef USE_READLINE //CSL
     if(promptmsg == MOREPROMPT || promptmsg == CONTPROMPT)
     { promptmsg="more> "; } 
#endif  
      if ( !quiet_load_flag )
      { outstring(promptmsg);  /* so appears on screen also */
        outstring(inmsg); /* echo */
      }
      if ( (c = strchr(inmsg,'\n')) != NULL ) 
      { if ( strcmp(promptmsg,"Enter command: ") != 0 )
          *c = 0;
      }
      else if ( (int)strlen(inmsg) == max-1 ) 
        inmsg[max-1] = MOREIN; 
      quiet_flag = oldquiet;
      return 1; /* not EOF */         
    }
  }

  breakflag = 0;  /* back to user input */
#ifdef USE_READLINE //CSL
   current_prompt = promptmsg==MOREPROMPT || promptmsg==CONTPROMPT ? "more> " : promptmsg;
#else
  current_prompt = promptmsg; /* for those who want to redisplay prompt */
#endif

#if defined(MAC_APP) || defined(WIN32S) || defined(MAC_CW)
  write_to_console(promptmsg);
  read_line_from_console(inmsg);
#else
#if defined(_READLINE_H_)
  if(use_readline(promptmsg,inmsg,max) == EOF )
      return EOF;
  else
#else
  {
  /* from stdin */
  oldquiet = quiet_flag; quiet_flag = 0;
  outstring(promptmsg);
  quiet_flag = oldquiet;

#ifdef WIN32
  signal(SIGINT,SIG_IGN); /* no interrupt during input (esp. WIN32) */
#endif

#ifdef OOGL
  /* check for geomview pick */
  if ( geomview_flag && !geompipe_flag )
    while ( check_pick() == 1 ) 
        outstring(promptmsg);
#endif

  if ( my_fgets(inmsg,max,stdin) == NULL )
     return EOF;
  if ( echo_flag ) { outstring(inmsg); outstring("\n"); }
  signal(SIGINT,catcher);
  }
#endif
#endif

  current_prompt = NULL;
  /* strip whitespace from start of inmsg */
  ptr = inmsg;
  while ( (*ptr==' ') || (*ptr=='\t') ) ptr++;
  if ( ptr != inmsg )
  { for ( c = inmsg ; *ptr ; ) *(c++) = *(ptr++);
     *c = 0;
  }
  /* strip nl from end */
  c = inmsg + strlen(inmsg) - 1;
  if ( *c == '\n' ) *c = 0;
  if ( logfile_flag ) fprintf(logfilefd,"%s\n",inmsg);
  if ( keylogfile_flag ) fprintf(keylogfilefd,"%s\n",inmsg);

  return 1; /* not EOF */
}

/***********************************************************************
*
*  function: add_path_to_name()
*
*  purpose:  form a new file name
*/
 
void add_path_to_name ARGS3((path,name,result),
     const char *path,const char *name, char *result) /* the result */
/* the result has length PATHSIZE */
{ char *slash;
  strncpy(result,path,PATHSIZE-1); result[PATHSIZE-1]=0;
  slash=strrchr(result,'/');
  if(!slash) slash = strrchr(result,'\\');
  if(slash) slash++;
  else slash=result;
  strncpy(slash,name,PATHSIZE-1-(slash-result));
}
 

/***********************************************************************
*
*  function: push_datafd()
*
*  purpose:  push new datafile include file on stack.
*/

void push_datafd ARGS2((cfd,name),
FILE *cfd, /* NULL if not opened yet */
char *name)  /* name of file */
{
  if ( include_depth >= NESTDEPTH-1 )
     kb_error(1352,"INCLUDEs nested too deeply.\n",DATAFILE_ERROR);

  if ( cfd == NULL )
     cfd = path_open(name,NOTDATAFILENAME);

  /* CSL: try the directory of the previous entry */
  if( cfd==NULL && include_depth>0 && datafile_stack[include_depth-1].fd != stdin )
  {
    add_path_to_name(datafile_stack[include_depth-1].filename,name,filename);
    cfd=path_open(filename,NOTDATAFILENAME);
  }
 
  if ( cfd == NULL )
  { sprintf(errmsg,"Cannot open %s.",name);
     kb_error(1353,errmsg,DATAFILE_ERROR);

     return;
  }
  if ( include_depth > 0 )
      datafile_stack[include_depth-1].line = line_no;
  line_no = 1;
  strncpy(datafile_stack[include_depth].filename,name,PATHSIZE-1);
  datafile_stack[include_depth].line = 0;
  datafile_stack[include_depth++].fd = data_fd = cfd;
  yylex_init();  /* reset lex */
}

/***********************************************************************
*
*  function: pop_datafd()
*
*  purpose:  Exit out of one level of datafile include nest.
*/

void pop_datafd()
{
  if ( include_depth <= 1 )
     { include_depth = 0; data_fd = NULL;}
  else fclose(data_fd);
  if ( include_depth > 0 ) 
    { data_fd = datafile_stack[--include_depth-1].fd;
      line_no = datafile_stack[include_depth-1].line;
    } 
  else data_fd = NULL;
}

/***********************************************************************
*
*  function: push_commandfd()
*
*  purpose:  push new command file on stack.
*/
  
void push_commandfd ARGS2((cfd,name),
FILE *cfd, /* NULL if not opened yet */
char *name)  /* name of file */
{
  if ( (cfd==NULL) && (name==NULL) ) return;
  if ( read_depth >= NESTDEPTH-1 )
     kb_error(1354,"READs nested too deeply.\n",RECOVERABLE);

  if ( cfd == NULL )
     cfd = path_open(name,NOTDATAFILENAME);

  /* CSL: try the directory of the previous entry */
  if( cfd==NULL && read_depth>0 && cmdfile_stack[read_depth-1].fd != stdin )
  {
    add_path_to_name(cmdfile_stack[read_depth-1].filename,name,filename);
    cfd=path_open(filename,NOTDATAFILENAME);
  }


  if ( cfd == NULL )
  { sprintf(errmsg,"Cannot open %s.",name);
    kb_error(1355,errmsg,RECOVERABLE);
  }
  if ( (cfd == stdin) && (read_depth == 1) ) read_depth = 0;

  strncpy(cmdfile_stack[read_depth].filename,name,PATHSIZE-1);
  cmdfile_stack[read_depth].datafile_flag = datafile_flag;
  if ( read_depth > 0 )
     cmdfile_stack[read_depth-1].line = line_no; /* save previous line number */
  line_no = 1;
  cmdfile_stack[read_depth].fd = commandfd = cfd;
  if ( file_no_used >= file_no_max ) 
  { file_names = (char**)kb_realloc((char*)file_names,
        (2*file_no_max+10)*sizeof(char*));
    file_no_max = 2*file_no_max+10;
  } 
  file_names[file_no_used] = mycalloc(strlen(name)+4,1);
  strcpy(file_names[file_no_used],name);
  if ( read_depth > 0 )
  { file_no = file_no_used;
    cmdfile_stack[read_depth].file_no = file_no;
  }
  file_no_used++;
  read_depth++;
  yylex_init();  /* reset lex */ 
}

/***********************************************************************
*
*  function: pop_commandfd()
*
*  purpose:  Exit out of one level of command file nest.
*/

void pop_commandfd()
{
  if ( in_comment )
          { kb_error(2205,"End of file in comment\n",WARNING );  }
#ifdef WIN32
  if ( commandfd == stdin )
  { int tty = _isatty(_fileno(stdin));
    if ( !tty ) 
       my_exit(0); /* exit in case of redirected input */
    else return; /* Ctrl-C gives spurious EOF */
    /* Note:   SIGINT is not supported for any Win32 application, 
       including Windows 98/Me and Windows NT/2000/XP. When a CTRL+C 
       interrupt occurs, Win32 operating systems generate a new thread 
       to specifically handle that interrupt. This can cause a 
       single-thread application such as UNIX, to become multithreaded, 
       resulting in unexpected behavior. (MSDN Library on SIGINT )
       */
 }
#endif
  if ( datafile_input_flag && !cmdfile_stack[read_depth-1].datafile_flag 
          /*&& !addload_flag */)
  { datafile_input_flag = 0;
    datafile_flag = 0; /* safe since doing one-char read-ahead */
    return;  /* lex needs to keep reading EOF for datafile */
  }
  if ( read_depth <= 1 )
  { read_depth = 0; commandfd = NULL; file_no = 0;}
  else fclose(commandfd);
  if ( read_depth > 0 ) 
  { commandfd = cmdfile_stack[--read_depth-1].fd;
    line_no = cmdfile_stack[read_depth-1].line;
    file_no = cmdfile_stack[read_depth-1].file_no;
  }
  if ( read_depth <= 1 )
  { cmdfilename = NULL;
    if ( warning_messages_new )
      outstring("\nNOTE: There were warning messages. To review, do \"print warning_messages\"\n");
    warning_messages_new = 0;
  }
}

/**************************************************************************
*
* function: add_warning_messages
*
* purpose: appends message to warning message list.
*/

#define WARNING_MESSAGES_MAX 100000

void add_warning_message ARGS1((message),
char *message)
{ size_t needed = (warning_messages ? strlen(warning_messages):0) + strlen(message) + 10;
  if ( warning_messages_max < needed )
  { if ( warning_messages_max < WARNING_MESSAGES_MAX )
    { warning_messages = (char*)kb_realloc(warning_messages,needed+1000);
      warning_messages_max = needed+1000;
    }
    else
    { /* delete early messages */
      char *c;
      for ( c = warning_messages + 1000 ; *c != '\n' ; c++ ) ;
      memcpy(warning_messages,c,warning_messages_max-(c-warning_messages));
    }
  }
  strcat(warning_messages,message);
  warning_messages_new++;
}

#ifdef __cplusplus
void do_throw ( int errnum )
{
  throw errnum;
}
void do_cmd_throw ( )
{ struct cmd_excep c;
  throw c;
}
void do_graph_throw ( )
{ struct graph_excep c;
  throw c;
}

#endif

/***********************************************************************
*
*  Purpose: Error handling.  Prints error message.  If error is
*     recoverable, longjmps to pre-set spot.  If not, exits.
*
*  May be system dependent for message display.
*/

void kb_error ARGS3((errnum,emsg,mode),
int errnum, /* error identifier */
char *emsg, /* might be msg or errmsg */
int mode)
{
  extern int line_no;
  char *fullmsg;
  int size;
  int prev_read_depth;
  int i;

  if ( emsg == NULL ) emsg = "";  /* just in case */
  if ( emsg == errmsg ) { fullmsg = msg; size = msgmax; }
  else { fullmsg = errmsg; size = sizeof(errmsg); }

  last_error = errnum;

  if ( read_depth > 1 ) 
  { sprintf(fullmsg,"\n%s Line %d:\n",
      cmdfile_stack[read_depth-1].filename,line_no); 
  }
  else fullmsg[0] = 0;

  reading_comp_quant_flag = 0;  /* just in case */

  switch ( mode )
  {
     case UNRECOVERABLE:
        sprintf(fullmsg+strlen(fullmsg),"FATAL ERROR %d: ",errnum);
        strncat(fullmsg,emsg,size);
        if ( datafile_flag )
          dump_buff(fullmsg+strlen(fullmsg),size-strlen(fullmsg));
        erroutstring(fullmsg);
        my_exit(errnum);  

     case RECOVERABLE_ABORT:
     case RECOVERABLE:
        sprintf(fullmsg+strlen(fullmsg),"ERROR %d: ",errnum);
        strncat(fullmsg,emsg,size);
        /* pop stack of command files */
        if ( read_depth > 0 ) cmdfile_stack[read_depth-1].line = line_no;
        while ( commandfd && (commandfd  != stdin) )
          {
/*
            sprintf(fullmsg+strlen(fullmsg),"file %s at line %d\n",
                  cmdfile_stack[read_depth-1].filename,
                  cmdfile_stack[read_depth-1].line);
*/
             pop_commandfd();
          }
        strcat(fullmsg,"\n");
        erroutstring(fullmsg);
        goto bailout;

     case RECOVERABLE_QUIET:
        /* pop stack of command files */
        while ( commandfd && (commandfd  != stdin) )
             pop_commandfd();
        goto bailout;

     case Q_ERROR:
        sprintf(fullmsg+strlen(fullmsg),"ERROR %d: ",errnum);
        strcat(fullmsg,emsg); 
        dump_buff(fullmsg+strlen(fullmsg),size-strlen(fullmsg));
        /* pop stack of command files */
        if ( read_depth > 0 ) cmdfile_stack[read_depth-1].line = line_no;
        prev_read_depth = read_depth+1;
        while ( commandfd && (commandfd  != stdin) )
          { if ( read_depth < prev_read_depth )
               sprintf(fullmsg+strlen(fullmsg),"file %s at line %d\n",
                  cmdfile_stack[read_depth-1].filename,
                  cmdfile_stack[read_depth-1].line); 
            prev_read_depth = read_depth; /* pop_commandfd() doesn't always pop */            
             if ( datafile_flag )
             { parse_error_flag = 1;
               recovery_flag = 1; 
               erroutstring(fullmsg);
               if ( exit_after_error ) my_exit(errnum);
               return;
             }
             else 
             pop_commandfd();
          }
        erroutstring(fullmsg);
        erroutstring("\n");
        goto bailout;

     case WARNING:
        for ( i = 0 ; i < warnings_suppressed_count ; i++ )
          if ( errnum == warnings_suppressed[i] )
            return;
        sprintf(fullmsg+strlen(fullmsg),"WARNING %d: ",errnum);
        strncat(fullmsg,emsg,size);
        strcat(fullmsg,"\n");
        erroutstring(fullmsg);
        add_warning_message(fullmsg);
        if ( exit_after_warning ) my_exit(errnum);
        if ( break_after_warning ) breakflag = BREAKAFTERWARNING;
        return;

    case EXPRESSION_ERROR:
        sprintf(fullmsg+strlen(fullmsg),"SYNTAX ERROR %d: ",errnum);
        strncat(fullmsg,emsg,size);
        dump_buff(fullmsg+strlen(fullmsg),size-strlen(fullmsg));
        erroutstring(fullmsg);
        add_warning_message(fullmsg);
        if ( ++parse_errors >= 5 ) 
          {
             erroutstring("Too many errors. Aborting.\n");
             /* pop stack of command files */
             while ( commandfd && (commandfd  != stdin) ) pop_commandfd();
             goto bailout;
          }
        parse_error_flag = 1;
        recovery_flag = 1;
        if ( exit_after_error ) my_exit(errnum);
        break;

     case SYNTAX_ERROR:
        strncat(fullmsg,emsg,size); 
        /*
        dump_buff(fullmsg+strlen(fullmsg),size-strlen(fullmsg));
        */
        rewind_globals(old_global_count);
        perm_rewind_globals(old_perm_global_count);
        local_nest_depth = 0;
        /* pop stack of command files */
        if ( !function_kludge_flag )
        { FILE *oldcommandfd = commandfd;
          if ( commandfd && (commandfd  != stdin) ) pop_commandfd();
          while ( commandfd && (commandfd  != stdin) )
          { char c[100];
            if ( commandfd != oldcommandfd )
            {
              sprintf(c,"called from file %s at line %d\n",
                  cmdfile_stack[read_depth-1].filename,
                  cmdfile_stack[read_depth-1].line);
              strncat(fullmsg,c,size-strlen(fullmsg));
            }
            pop_commandfd();
          }
        }
        strcat(fullmsg,"\n");
        erroutstring(fullmsg); 
        const_expr_flag = 0;
        if ( exit_after_error ) my_exit(errnum);
        break; /* return to parser for its message */

     case COMMAND_ERROR:
  if ( datafile_flag )
   { /* kludge since cmdjmpbuf empty in datafile */
    kb_error(errnum,emsg,DATAFILE_ERROR);
    return;
    }
        sprintf(fullmsg+strlen(fullmsg),"ERROR %d: ",errnum);
        strncat(fullmsg,emsg,size);
        dump_buff(fullmsg+strlen(fullmsg),size-strlen(fullmsg));
        parse_error_flag = 1;
        rewind_globals(old_global_count);
        perm_rewind_globals(old_perm_global_count);
        /* pop stack of command files */
        if ( read_depth > 0 ) cmdfile_stack[read_depth-1].line = line_no;
        while ( commandfd && (commandfd  != stdin) )
          {
/*
             char stuff[200];
             sprintf(stuff,"file %s at line %d\n",
                  cmdfile_stack[read_depth-1].filename,
                  cmdfile_stack[read_depth-1].line);
                 strncat(fullmsg,stuff,size); 
*/
             pop_commandfd();
          }
        strcat(fullmsg,"\n");
        erroutstring(fullmsg);
        #ifdef PTHREADS
        if ( pthread_equal(locking_thread,pthread_self()) )
        {  ABORT_GRAPH_MUTEX
        }
        #endif
        #ifdef WINTHREADS
        if ( GetCurrentThreadId() == locking_thread )
        {  ABORT_GRAPH_MUTEX
        }
        #endif
        if ( subshell_depth == 0 )
          temp_free_all();
        if ( exit_after_error ) my_exit(errnum);
        FPRESET;
        quiet_flag = 0;
#ifdef __cplusplus
        do_cmd_throw();
#else
        longjmp(cmdbuf,1);
#endif
        break;

     case PARSE_ERROR:
        sprintf(fullmsg+strlen(fullmsg),"SYNTAX ERROR %d: ",errnum);
        strncat(fullmsg,emsg,size);
        dump_buff(fullmsg+strlen(fullmsg),size-strlen(fullmsg));
        strcat(fullmsg,"\n");
        erroutstring(fullmsg);
        add_warning_message(fullmsg);
        if ( ++parse_errors >= 5 ) 
          {
             erroutstring("Too many errors in datafile. Aborting.\n");
             /* pop stack of command files */
             while ( commandfd && (commandfd  != stdin) ) pop_commandfd();
             goto bailout;
          }
        parse_error_flag = 1;
        const_expr_flag = 0;
        recovery_flag = 1;
        if ( exit_after_error ) my_exit(errnum);
        break;
        
     case DATAFILE_ERROR:
        sprintf(fullmsg+strlen(fullmsg),"DATAFILE ERROR %d: ",errnum);
        strncat(fullmsg,emsg,size); 
        dump_buff(fullmsg+strlen(fullmsg),size-strlen(fullmsg));
        strcat(fullmsg,"\n");
        erroutstring(fullmsg);
        add_warning_message(fullmsg);
        if ( ++parse_errors >= 5 ) 
          {
             erroutstring("Too many errors in datafile. Aborting.\n");
             /* pop stack of command files */
             while ( commandfd && (commandfd  != stdin) ) pop_commandfd();
             update_display(); /* so can see what we have so far */
             goto bailout;
          }
        parse_error_flag = 1;
        recovery_flag = 1;
        if ( exit_after_error ) my_exit(errnum);
        local_nest_depth = 0;
        return;
     }

  quiet_flag = 0;
  return;

bailout:

  #ifdef MPI_EVOLVER
  if ( (this_task != 0) && !mpi_local_error_bailout )
  { erroutstring("ALL SLAVE TASK ERRORS FATAL! KILLING ALL PROCESSES!\n");
    MPI_Abort(MPI_COMM_WORLD,1);
  }
  #endif

  if ( outfd != stdout )
  { if ( outfd != NULL )
    {
       fclose(outfd);
    }
    outfd = stdout;    /* in case of previous piping */
  }
  breakflag = 0; iterate_flag = 0;
  
  if ( mode == RECOVERABLE_ABORT )
  {
    subshell_depth = 0;
    hessian_subshell_flag = 0;
  }
  
  if ( saved.coord ) 
  { restore_coords(&saved,SAVE_IN_ATTR); 
    unsave_coords(&saved,SAVE_IN_ATTR); 
    ABORT_GRAPH_MUTEX;
    recalc();
  }
  clear_symtable();     /* clear symbol table */
  vgrad_end();          /* clear up volume gradients */
  memset((char*)(&Met),0,sizeof(Met));
  #ifdef PTHREADS
  if ( pthread_equal(locking_thread,pthread_self()) )
  {  ABORT_GRAPH_MUTEX
  }
  #endif
  #ifdef WINTHREADS
  if ( GetCurrentThreadId() == locking_thread )
  {  ABORT_GRAPH_MUTEX
  }
  #endif
  if ( subshell_depth == 0 )
     temp_free_all();      
  if ( list && (list != permlist) )
    { myfree((char*)list); list = NULL; } /* plug memory leak */
  quiet_flag = 0;
  gocount = 1;
  local_nest_depth = 0;
  if ( exit_after_error ) my_exit(errnum);
  FPRESET;
  ABORT_GRAPH_MUTEX;
#ifdef WIN32
  if ( draw_thread_id == GetCurrentThreadId() )
#elif defined(PTHREADS)
  if ( draw_thread_id == pthread_self() ) 
#endif

#if defined(WIN32) || defined(PTHREADS)
#ifdef __cplusplus
      do_graph_throw();
#else
      if ( *(int*)&graphjumpbuf )
        longjmp(graphjumpbuf,1);
#endif
#endif
 { struct thread_data *td = GET_THREAD_DATA;
   td->stack_top = td->eval_stack;
   td->frame_spot = 0;
 }
 
#ifdef __cplusplus
  do_throw(errnum);
#else
  longjmp(jumpbuf[subshell_depth],1);
#endif
  
}
 
/**************************************************************************
*
* function: start_logfile()
*
* purpose: open logfile and start logging user input and screen output
*
* arguments: char * filename : Name of file to open, append mode.
*                                        If null, use logfilename[].
*
*/

void start_logfile ARGS1((filename),
char *filename)
{
  char *name = filename ? filename : logfilename;

  if ( name[0] == 0 )
     kb_error(2206,"Missing log file name.\n",RECOVERABLE);

  stop_logfile();

  logfilefd = fopen(name,"a");
  if ( !logfilefd ) { perror(name); return; }

  logfile_flag = 1;
  if ( filename ) strncpy(logfilename,filename,sizeof(logfilename)-1);

}

/***************************************************************************
*
* function: stop_logfile()
*
* purpose: end logging input and output.
*/

void stop_logfile()
{
  if ( logfilefd ) fclose(logfilefd);
  logfilefd = NULL;
  logfile_flag = 0;
}

/**************************************************************************
*
* function: start_keylogfile()
*
* purpose: open keylogfile and start logging user input
*
* arguments: char * filename : Name of file to open, append mode.
*                                        If null, use keylogfilename[].
*
*/

void start_keylogfile ARGS1((filename),
char *filename)
{
  char *name = filename ? filename : keylogfilename;

  if ( name[0] == 0 )
     kb_error(2420,"Missing key log file name.\n",RECOVERABLE);

  stop_keylogfile();

  keylogfilefd = fopen(name,"a");
  if ( !keylogfilefd ) { perror(name); return; }

  keylogfile_flag = 1;
  if ( filename ) strncpy(keylogfilename,filename,sizeof(keylogfilename)-1);

}

/***************************************************************************
*
* function: stop_keylogfile()
*
* purpose: end key logging
*/

void stop_keylogfile()
{
  if ( keylogfilefd ) fclose(keylogfilefd);
  keylogfilefd = NULL;
  keylogfile_flag = 0;
}


/****************************************************************************
*
* function: set_scroll_size()
*
* purpose: set output console scroll buffer size in Windows
*/

void set_scroll_size ARGS1((rows),
int rows)
{
#ifdef MSC
  COORD size;
  if ( rows < 1)
    kb_error(2428,"Minimum scroll buffer size is 1.  Command ignored.\n",
         WARNING);
  size.X = 80;
  size.Y = (short)rows;
  SetConsoleScreenBufferSize(GetStdHandle(STD_OUTPUT_HANDLE),size);
#else
  kb_error(2429,"ScrollBufferSize not implemented on this system.\n",
     WARNING);
#endif
}

