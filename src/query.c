/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/
 
 
/**********************************************************
*
*  File: query.c
*
*  Contents:  query interpreter for Surface Evolver
*/

#include "include.h"
#include "ytab.h"

int commandverb;

/***********************************************************************
  History mechanism.
  Command texts are stored in linear array history_space[], separated by
  nulls, most recent first.  history_offsets[] contains indices of starts
  of commands, -1 for no command.
  history_number is permanent counter of commands.
  history_count is number of currently saved commands.
**************************************************************************/


/**************************************************************************
*  
* function: new_history()
*
* purpose:  Appends command to history list, queue fashion.
*/

void new_history ARGS1((text),
char *text)
{ size_t length;
  int k;

  if ( history_space == NULL )
  { history_space = my_list_calloc(HISTORYSPACE,1,ETERNAL_BLOCK);
    for ( k = 0 ; k < MAXHISTORY ; k++ ) history_offsets[k] = -1; /* empty */
    history_count = 0;
  }
  length = strlen(text)+1;
  if ( length >= HISTORYSPACE-5 ) 
  { kb_error(1585,"Command too long to save in history list.\n",WARNING);
    return;
  }
  kb_memmove(history_space+length,history_space,HISTORYSPACE-length);
  for ( k = history_count-1 ; k >= 0 ; k-- )
  { /* adjust pointers in history list */
    history_offsets[k+1] = history_offsets[k] + (int)length;
    if ( history_offsets[k+1] >= HISTORYSPACE )
       { history_offsets[k+1] = -1; history_count--; }
  }
  strcpy(history_space,text);
  history_offsets[0] = 0;
  if ( history_count < MAXHISTORY-1 ) history_count++;
  history_number++;
}

/**************************************************************************
*  
* function: old_history()
*
* purpose:  Checks commands beginning with ! for history list match.
* return:   retval from command()
*/

int old_history ARGS1((text),
char *text)
{ char *h;
  int k;
  int hnum;
  int  retval = 0;

  if ( history_space == NULL )
    { history_space = my_list_calloc(HISTORYSPACE,1,ETERNAL_BLOCK);
      for ( k = 0 ; k < MAXHISTORY ; k++ ) history_offsets[k] = -1; /* empty */
      history_count = 0;
    }

  if (text[1] == '!' ) 
    {
      if ( history_count > 0 ) 
      { k = 0; goto do_old;
      }
      else { kb_error(1586,"History list is empty.\n",WARNING); return retval; }

    }
  else if ( isdigit(text[1]) )
     { hnum = atoi(text+1);
       k = history_number - hnum;
       if ( (k >= 0) && (k < history_count) )
          goto do_old;
       else kb_error(1587,"Command not in history list.\n",RECOVERABLE);
     }
  else for ( k = 0 ; k < history_count ; k++ )
     { h = history_space+history_offsets[k]; 
       if ( strlen(h) < strlen(text+1) ) continue; /* prevent compare overrun */
       if ( strncmp(h,text+1,strlen(text+1)) == 0 )
          goto do_old;
     }
  kb_error(1588,"Command not found in history.\n",RECOVERABLE);


do_old:
  h = history_space + history_offsets[k];
  outstring(h);outstring("\n");
  retval = command(h,ADD_TO_HISTORY);  /* reinserts fulltext copy in history list */
  return retval;
}

/**************************************************************************
*  
* function: catfulltext()
*
* purpose:  catenates line to full text of command
*/

void catfulltext ARGS1((stuff),
char *stuff)
{

  if ( fulltextsize > MAXCMDSIZE-3 ) return; /* too long */
  if ( fulltextsize > 0 ) 
     { strcat(fulltext,"  "); /* indentation */
        fulltextsize += 2;
     }
  strncpy(fulltext+fulltextsize,stuff,MAXCMDSIZE-fulltextsize);
  fulltextsize = strlen(fulltext);
  if ( fulltext[fulltextsize-1] != '\n' ) fulltext[fulltextsize++] = '\n';
  fulltext[fulltextsize] = '\0';

}

/**************************************************************************
*  
* function: command()
*
* purpose:  Executes a text command string.
*
* return:  END_COMMANDS if current command interpreter loop is to end.
*          0 else
*/

int command ARGS2((text,mode),
char *text,
int mode) /* NO_HISTORY or ADD_TO_HISTORY */
{
  struct expnode qnode;  /* for query expression */
  int retval = 0;
  int old_scope;
  struct treenode *old_list;
  int old_listtop, old_listmax;
  int old_brace_depth = brace_depth;

  memset(&qnode,0,sizeof(struct expnode));
  sprintf(qnode.name,"command");
 
  old_listmax = listmax;
  old_list = list; old_listtop = listtop; /* for nested parsing */
  listmax = 30;
  list = (struct treenode *)mycalloc(listmax,sizeof(struct treenode));
  list[1].type = SETUP_FRAME_;
  listtop = 2;
  loopdepth = 0;
  parse_error_flag = 0;
  lists_flag = 0;
  cmdptr = text;
  old_scope = begin_scope();


#ifdef __cplusplus
  try
  {
#else
  if ( setjmp(cmdbuf) )
  { set_scope(old_scope);  
    cmdptr = NULL;  
    myfree((char *)list); 
    list = NULL;
    memset(local_scope_bases,0,sizeof(local_scope_bases));
	local_nest_depth = 0;
    return 0; 
  }
#endif

  verb_flag = 1; /* tell lex we need a verb first */
  old_global_count = web.global_count; /* for error recovery */
  old_perm_global_count = web.perm_global_count; /* for error recovery */
  /* local_nest_depth = 0; */
  init_local_scope(0);
  fulltextsize = 0; 
  catfulltext(text);

  retval = yybegin();  /* 0 for accept, 1 for error */
  qnode.locals = localbase;
  if ( localbase )
    qnode.locals->flags |= LL_IN_USE;
  exit_local_scope();

  cmdptr = NULL;

  if ( (retval == 1) || (parse_error_flag) )
  { myfree((char *)list); 
    if ( list == permlist ) permlist = NULL;
    list = NULL; set_scope(old_scope); 
    return 0;  
  }

  qnode.start=list;
  qnode.root = list + listtop - 1;
  if ( qnode.root->type == SETUP_FRAME_ ) /* empty command */
  { retval = 0;
    goto command_exit;
  }
  if ( qnode.root->type != CMDLIST_ )  /* expression or something */
     kb_error(1589,"Illegal command.\n",COMMAND_ERROR);
  

  list[0] = list[listtop-1];  /* root also in first spot */
  if ( list[0].left ) list[0].left += listtop - 1;
  if ( list[0].right ) list[0].right += listtop - 1;

  /* put DONE marker after root */
  list[listtop++].type = FINISHED;

  /* figure stack usage */
  stack_usage(&qnode);

  /* set element loop flag bit in nodes */
  if ( !function_kludge_flag )
    mark_element_loops(&list[0],0);

  if ( logfd ) 
  fprintf(logfd,"%s\n",text);

  /* add to history list */
  if ( mode == ADD_TO_HISTORY ) 
    new_history(fulltext);

  /* initialize current element to NULLID */
  aggregate_depth = 0; calc_quant_flag = 0;
  exit_flag = 0;
  eval(&qnode,NULL,NULLID,NULL);
  retval = exit_flag ? END_COMMANDS : 1;
  exit_flag = 0;

command_exit:
  qnode.flag = USERCOPY; /* so free_expr will work */
  free_expr(&qnode); 
  list = NULL;
  set_scope(old_scope);
  list = old_list; listtop = old_listtop; /* for nested parsing */
  listmax = old_listmax;
  brace_depth = old_brace_depth;
  return retval;

#ifdef __cplusplus
  }
  catch(cmd_excep c)
  { set_scope(old_scope);  
    cmdptr = NULL;  
    myfree((char *)list); 
    list = NULL;
    memset(local_scope_bases,0,sizeof(local_scope_bases));
	local_nest_depth = 0;
    return 0; 
  }
#endif
}

/*************************************************************************
*
* function: stack_usage()
*
* purpose: figure maximum stack usage of expression or command.
*
*/

void stack_usage ARGS1((ex ),
struct expnode *ex)
{ int usage = 0;
  struct treenode *node ;
#ifdef _DEBUG
  int didwarn = 0;
#endif
  
  ex->stack_max = 0;
  for ( node = ex->start+1 ; node->type != FINISHED ; node++ )
  { usage += node->stack_delta;
    if ( usage > ex->stack_max ) ex->stack_max = usage;
#ifdef _DEBUG
    if ( (usage < 0) && (!didwarn) ) 
    { sprintf(errmsg,"Internal error: stack usage negative, node type %d.\n",
        node->type);
      sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
         file_names[node->file_no],node->line_no);
      kb_error(2670,errmsg,WARNING);
      didwarn = 1;
    }     
    node->stack_spot = usage;
#endif
  }
}

/***********************************************************************
*
* Function: mark_element_loops()
*
* Purpose: Mark flags of expression nodes that are within 
*          element loops, so can know when freeing discards is
*          ill-advised.   Recursively transverses tree pre-order.
*/

void mark_element_loops(root,mark)
struct treenode *root;
int mark;  /* whether root in loop */
{ if ( mark ) 
    root->flags |= IN_ELEMENT_LOOP;
  else mark = root->flags & IN_ELEMENT_LOOP;

  if ( root->left )
    mark_element_loops(root+root->left,mark);
  if ( root->right )
    mark_element_loops(root+root->right,mark);
}

/************************************************************************
*
* function: print_profiling()
*
* purpose: Print profiling elapsed times.  Prints comma form of clock
8          counts.
*/

void commaize(s)   /* put commas in integers at end of string */
char *s;
{ int n,k,groups,j;
  
  k = strlen(s);
  
  while ( k > 0 )
  {
    /* find last digit */
    for ( n = k; n > 0 ; n-- )
      if ( isdigit(s[n]) ) break;
    if ( !isdigit(s[n]) ) return;
    /* find first digit */
    for ( k = n-1 ; k > 0 ; k-- )
      if ( !isdigit(s[k]) ) break;
    groups = (n-k-1)/3;
    for ( j = k+1 ; j < n ; j++ )
    { s[j-groups] = s[j];
      if ( ((n-j+groups) % 4) == 0  )
      { groups--;
        s[j-groups] = ',';
      }
    }
  }
}
  
void print_profiling()
{ 
  #ifdef PROF_EVALS
  int i,k;
  int do_flag = 0;

  outstring("\nInclusive profiling counts: \n");
  outstring("                      Name            CPU Cycles\n");
  for ( i = 0 ; i < web.global_count ; i++ )
  {
    if ( globals(i)->flags & (FUNCTION_NAME|PROCEDURE_NAME|SUBROUTINE) )
    { 
      sprintf(msg,"  %30s     %15.f\n",globals(i)->name,
          globals(i)->value.proc.elapsed_time);
      commaize(msg); 
      outstring(msg);
    }
  }

  for ( i = 0, do_flag = 0; i < NUMELEMENTS ; i++ )
	if ( show_expr_table[i].root )
		do_flag = 1;
  if ( do_flag )
  {
    outstring("\nElement show expressions:\n");
    outstring("                   Element            CPU Cycles\n");
    for ( i = 0 ; i < NUMELEMENTS ; i++ )
    { sprintf(msg,"  %30s %15.f\n",typenames[i],show_expr_table[i].elapsed_time);
      commaize(msg);
      outstring(msg);
    }
  }

  for ( i = LOW_INST, do_flag = 0 ; i < meth_inst_count ; i++ )
	  if ( METH_INSTANCE(i)->expr[0] )
		  do_flag = 1;
  if ( do_flag )
  {
    outstring("\nMethod instance expressions:\n");
    outstring("                     Method instance            CPU Cycles\n");
    for ( i = LOW_INST; i < meth_inst_count ; i++ )
    { REAL total_time = 0.0;
      struct method_instance *mi = METH_INSTANCE(i);
	  for ( k = 0 ; k < MAXMEXPR ; k++ )
		if ( mi->expr[k] )
			total_time += mi->expr[k]->elapsed_time;
		else break;
      sprintf(msg,"  %40s %15.f\n",mi->name,total_time);
	  commaize(msg);
	  outstring(msg);
    }
  }

  for ( i = 0, do_flag = 0 ; i < web.bdrymax ; i++ )
	  if ( web.boundaries[i].coordf[0] )
		  do_flag = 1;
  if ( do_flag )
  { 
    outstring("\nBoundary expressions:\n");
    outstring("                    Boundary  Formula Cycles   Energy Cycles  Content Cycles\n");
    for ( i = 0 ; i < web.bdrymax ; i++ )
    { REAL coord_time = 0.0;
      REAL energy_time = 0.0;
      REAL content_time = 0.0;
      struct boundary *b = web.boundaries + i;

	  if ( !b->coordf[0] )
		continue;
	  for ( k = 0 ; k < SDIM ; k++ )
	  { if ( b->coordf[k] )
			coord_time += b->coordf[k]->elapsed_time;
	    if ( b->envect[k] )
			energy_time += b->envect[k]->elapsed_time;
	    if ( b->convect[k] )
			content_time += b->convect[k]->elapsed_time;
	  }	
      sprintf(msg,"%30s %15.f %15.f %15.f\n",b->name,coord_time,energy_time,content_time);
	  commaize(msg);
	  outstring(msg);
    }
  }

  for ( i = 0, do_flag = 0 ; i < web.maxcon ; i++ )
	  if ( get_constraint(i)->formula )
		  do_flag = 1;
  if ( do_flag )
  {
    outstring("\nConstraint expressions\n");
    outstring("                    Constraint  Formula Cycles   Energy Cycles  Content Cycles\n");
    for ( i = 0 ; i < web.maxcon ; i++ )
    { struct constraint *con = get_constraint(i);
      REAL coord_time = 0.0;
      REAL energy_time = 0.0;
	  REAL content_time = 0.0;
    
	  if ( !con->formula ) 
		continue;
	  for ( k = 0 ; k < SDIM ; k++ )
	  { if ( con->formula )
			coord_time += con->formula->elapsed_time;
	    if ( con->envect[k] )
			energy_time += con->envect[k]->elapsed_time;
	    if ( con->convect[k] )
			content_time += con->convect[k]->elapsed_time;
	  }	
      sprintf(msg,"%30s %15.f %15.f %15.f\n",con->name,coord_time,energy_time,content_time);
	  commaize(msg);
	  outstring(msg);
    }
  }
  #else
  outstring("Evaluation profiling not enabled; compile with PROF_EVALS.\n");
  #endif
}


/************************************************************************
*
* function: reset_profiling()
*
* purpose: Reset profiling elapsed times.
*/

void reset_profiling()
{ int i,k;
  for ( i = 0 ; i < web.global_count ; i++ )
  {
    if ( globals(i)->flags & (FUNCTION_NAME|PROCEDURE_NAME|SUBROUTINE) )
      globals(i)->value.proc.elapsed_time = 0.0;

  }
 
    for ( i = 0 ; i < NUMELEMENTS ; i++ )
    { show_expr_table[i].elapsed_time = 0.0;
    }
  

 

    for ( i = LOW_INST; i < meth_inst_count ; i++ )
    { 
      struct method_instance *mi = METH_INSTANCE(i);
	  for ( k = 0 ; k < MAXMEXPR ; k++ )
		if ( mi->expr[k] )
			mi->expr[k]->elapsed_time = 0.0;
		else break;
    }
  
    for ( i = 0 ; i < web.bdrymax ; i++ )
    { 
      struct boundary *b = web.boundaries + i;

	  if ( !b->coordf[0] )
		continue;
	  for ( k = 0 ; k < SDIM ; k++ )
	  { b->coordf[k]->elapsed_time = 0.0;
	    b->envect[k]->elapsed_time = 0.0;
	    b->convect[k]->elapsed_time = 0.0;
	  }	
      
    }
  

  
    for ( i = 0 ; i < web.maxcon ; i++ )
    { struct constraint *con = get_constraint(i);
	  if ( !con->formula ) 
		continue;
	  for ( k = 0 ; k < SDIM ; k++ )
	  { con->formula->elapsed_time = 0.0;
	    con->envect[k]->elapsed_time = 0.0;
	    con->convect[k]->elapsed_time = 0.0;
	  }	
    }
  
}

/***************************************************************************
*
* function: mark_recalc_params()
*
* purpose: Traverse quantities and expressions and things and find
*          variables whose change should cause a recalc().
*/

void mark_recalc_params()
{
  int n,i,k,j;

  /* Clear old recalc flags */
  for ( n = 0 ; n < web.global_count ; n++ )
  { struct global *g = globals(n);
    if ( g->flags & ALWAYS_RECALC )
      g->flags |= RECALC_PARAMETER;
    else
      g->flags &= ~RECALC_PARAMETER;
  }

  /* Go through all expressions attached to various things. */
  if ( torus_period_expr[0][0].root )
   for ( i = 0 ; i < SDIM ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
     { mark_recalc_expr(&torus_period_expr[i][j]);
       mark_recalc_expr(&torus_display_period_expr[i][j]);
     }
  if ( mobility_flag ) mark_recalc_expr(&mobility_formula);
  if ( mobility_tensor_flag )
   for ( i = 0 ; i < SDIM ; i++ )
    for ( j = 0 ; j < SDIM ; j++ )
      mark_recalc_expr(&torus_period_expr[i][j]);
  if ( web.metric_flag )
   for ( i = 0 ; i < SDIM ; i++ )
    for ( j = 0 ; j < SDIM ; j++ )
      mark_recalc_expr(&web.metric[i][j]);
  for ( i = 0 ; i < NUMELEMENTS ; i++ )
    mark_recalc_expr(show_expr[i]);
  for ( i = 0 ; i < web.bdrymax ; i++ )
  { struct boundary *b = web.boundaries + i;
    if ( !(b->attr & IN_USE) ) continue;
    if ( b->attr & CON_ENERGY )
      for ( j = 0 ; j < SDIM ; j++ )
      { mark_recalc_expr(b->coordf[j]);
        mark_recalc_expr(b->envect[j]);
        mark_recalc_expr(b->convect[j]);
      }
  }
  for ( i = 0 ; i < web.maxcon ; i++ )
  { struct constraint *con = get_constraint(i);
    if ( !(con->attr & IN_USE) ) continue;
    if ( con->attr & CON_ENERGY )
    { mark_recalc_expr(con->formula);
      for ( j = 0 ; j < SDIM ; j++ )
      { mark_recalc_expr(con->envect[j]);
        mark_recalc_expr(con->convect[j]);
      }
    }
 }

  for ( k = LOW_INST  ; k < meth_inst_count  ; k++ )
  { struct method_instance * mi = METH_INSTANCE(k); /* since some init may move things */
    struct gen_quant *q;
    if ( mi->quant >= 0 ) q = GEN_QUANT(mi->quant);
    else continue;
    if ( !(q->flags & Q_ENERGY) ) continue;
    for ( i = 0 ; i < MAXMEXPR ; i++ )
      mark_recalc_expr(mi->expr[i]);
  }
 
} /* end mark_recalc_params() */
 
/*************************************************************************
*
* function: mark_recalc_expr()
*
* purpose: Mark the functions and variables in an expression known
*          to cause recalc.
*
* return: how many new ones marked
*/
int mark_recalc_expr(ex)
struct expnode *ex;
{ int count = 0 ;
  struct treenode *node;

  if ( ex == NULL ) return 0;
  if ( ex->start == NULL ) return 0;

  for ( node = ex->start ; node != ex->root ; node ++  )
    if ( node->type == PUSHGLOBAL_ )
    { struct global *g;
      if ( (node->op1.name_id &(~GLOBMASK))==LOCALVAR )
         continue; 
      g = globals(node->op1.name_id);
      if ( g->flags & RECALC_PARAMETER )
         continue;
      g->flags |= RECALC_PARAMETER;
      count ++;
    }

  return count;
} /* end mark_recalc_expr() */
