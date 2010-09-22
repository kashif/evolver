/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*****************************************************************
*
*  File: yexparse.c
*
*  Purpose: To read and parse user commands and functions in algebraic form
*          for evolver constraints.  Expressions are given
*          in algebraic form and parsed with yacc (see command.yac
*          and y.tab.c).
*          
*/

#include "include.h" 
#include "lex.h"
#include "ytab.h"
  

/*****************************************************************
*
*  Function exparse()
*
*  Purpose: reading, parsing, checking of algebraic function definition.
*          Takes tokens from yylex() until non-expression token.
*          Function value is size in bytes of tree.
*
*/

#define LISTMAX 200

struct treenode *list;    /* tree */
struct treenode *permlist;    /* to use if user doesn't want own copy */
NTYPE listtop;  /* first spot reserved for root */
NTYPE maxp;
int listmax;  /* allocated */

/* for BREAK and CONTINUE */
#define LOOPMAX 30
int loopdepth;
int loopbase[LOOPMAX];

int using_param_flag;  /* so can detect if using boundary parameters */

int exparse ARGS3((maxparam,enode,flag),
int maxparam,  /* maximum number of parameters allowed */
struct expnode *enode,    /* pointer to storage pointer */
int flag)    /* whether to make copy for user */
{
  int retval;
  struct treenode *old_list = list;  /* for nested parsing */
  int old_listtop = listtop;
  int old_listmax = listmax;
  int old_brace_depth = brace_depth;


  PROF_START(exparse);

  listmax = LISTMAX;
  if ( permlist == NULL )
     permlist = 
      (struct treenode *)mycalloc(listmax,sizeof(struct treenode));
  maxp = (NTYPE)maxparam;

  if ( flag == USERCOPY )
  { enode->start = list = 
      (struct treenode *)mycalloc(listmax,sizeof(struct treenode));
  }
  else enode->start = list = permlist;

  list[1].type = SETUP_FRAME_;
  listtop = 2;
  parse_error_flag = 0;
  brace_depth = parens = in_quote = 0;
  /* unput expression start token for yacc */
  tok = EXPRESSION_START_; unput_tok();

  using_param_flag = 0;
  if ( !const_expr_flag || backquote_flag )
  { 
    /*local_nest_depth = 0;*/
    init_local_scope(0);
    begin_local_scope();
  }
  PROF_FINISH(exparse);
  PROF_START(yyparse); 
  retval = yyparse();  /* 0 for accept, 1 for error */
  PROF_FINISH(yyparse);
  PROF_START(exparse);
  if ( !const_expr_flag || backquote_flag )
  { end_local_scope();
    enode->locals = localbase;
    if ( localbase )
      localbase->flags |= LL_IN_USE;
    exit_local_scope();
  } 

  if ( (tok != 0) && (tok != ',') && (tok != LEXERROR) )
  {
    /* push back last token */
    unput_tok();
  }

  if ( (retval == 1) || parse_error_flag  || (listtop == 1) )
  { if ( flag == USERCOPY ) 
    { myfree((char *)enode->start); 
      if ( enode->start == list ) list = NULL;
      if ( enode->start == permlist ) permlist = NULL;
    }
    enode->start = NULL;
    retval = -1; 
    goto exparse_exit;
  }

  /* free excess list */
  if ( flag == USERCOPY )
    enode->start = list = (struct treenode *)kb_realloc((char *)list,
     (listtop+2)*sizeof(struct treenode));
  
  enode->root = list + listtop - 1;
  list[0] = list[listtop-1];  /* root also in first spot */
  list[0].left += listtop - 1;
  list[0].right += listtop - 1;

  /* put DONE marker after root */
  list[listtop++].type = FINISHED;

  /* figure stack usage */
  stack_usage(enode);

  enode->flag = flag;
  if ( using_param_flag ) 
    enode->flag |= USING_PARAM_FLAG;

  retval = listtop*sizeof(struct treenode);

  list = old_list;                /* for nested parsing */
  listtop = old_listtop;
  listmax = old_listmax;
  brace_depth = old_brace_depth;

exparse_exit:
  PROF_FINISH(exparse);
  return retval;

}

/*********************************************************************
*
*  Function: free_expr()
*
*  Purpose:  Deallocate lists of expression.
*
*/

void free_expr ARGS1((ex),
struct expnode *ex)
{ 

  if ( ex == NULL ) return;
  if ( ex->flag == NOUSERCOPY ) return;
  if ( ex->start )
  { 
   struct treenode *node;
    for ( node = ex->start ; node != ex->root ; node ++  )
    {
    #ifdef XXXXXXX
   /* free local symbol tables ?? */
      if ( node->flags & HAS_LOCALLIST )
      { struct locallist_t *locals = node->op5.locals;
        if ( locals->list ) 
        { if ( locals->flags & LL_PERMANENT ) free((char*)locals->list);
          else myfree((char*)locals->list);
        }
        if ( locals->flags & LL_PERMANENT ) 
          free((char*)locals);
        else 
          myfree((char*)locals);
      }
    #endif
      if ( node->flags & HAS_STRING )
        myfree(node->op1.string);
      if ( node->flags & HAS_STRING_5 )
        myfree(node->op5.string);
    }
    myfree((char *)ex->start);
  }
  ex->root = NULL;
  ex->start = NULL;

  if ( ex->locals )
  { myfree((char*)(ex->locals->list));
    myfree((char*)(ex->locals));
  }
  ex->locals = NULL;

}
/*********************************************************************
*
*  Function: perm_free_expr()
*
*  Purpose:  Deallocate lists of permanent expression.
*
*/

void perm_free_expr ARGS1((ex),
struct expnode *ex)
{ struct treenode *node;

  if ( ex == NULL ) return;
  if ( ex->flag == NOUSERCOPY ) return;
  if ( ex->start )
  { for ( node = ex->start ; node != ex->root ; node ++  )
    if ( node->flags & HAS_STRING )
      free(node->op1.string);
    if ( node->flags & HAS_STRING_5 )
      free(node->op5.string);
     free((char *)ex->start);
  }
  ex->root = NULL;
  ex->start = NULL;
}

/*********************************************************************
*
*  Function:  makenode()
*
*  Purpose: Add nodes to parse tree when called by yyparse.
*          Linear order of node list is such that sequential
*          execution is postorder.  If left > right, then
*          subtrees physically swapped, but they must be adjacent.
*
*  Return:  Index of created node.
*/

void more_makenode ARGS((NTYPE,NTYPE,NTYPE));

int makenode ARGS3((ntype,left,right),
NTYPE ntype,  /* type of node */
NTYPE left,  /* left son, if any */
NTYPE right) /* right son, if any */
{
  short type = (short)ntype;
  int i,n;
  struct treenode *nnode;
  struct global *g;
  int etype = -1; /* element type */

  if ( listtop > listmax - 10 )
  { list = (struct treenode *)kb_realloc((char*)list,
              (listmax+LISTMAX)*sizeof(struct treenode));
    listmax += LISTMAX;
  }
  else
     memset((char*)(list+listtop),0,2*sizeof(struct treenode)); /* clear nodes */
  switch ( type )
  {
      case NULLBLOCK_:
         list[listtop].type = NULLBLOCK_;
         break;

      case NULLCMD_:
         list[listtop].type = NULLCMD_;
         break;

      case NOP_:
         list[listtop].type = NOP_;
         break;

      case SUPPRESS_WARNING_:
      case UNSUPPRESS_WARNING_:
         list[listtop].type = type;
         list[listtop].op1.name_id = left;
         list[listtop].stack_delta = -1;
         break;

      case WHEREAMI_COMMAND_:
         list[listtop].type = WHEREAMI_COMMAND_;
         break;

      case SET_BREAKPOINT_:
         list[listtop].type = type;
         list[listtop].op1.name_id = left;
         list[listtop].left = right - listtop;
         list[listtop].flags |= EPHEMERAL;
         break;

      case UNSET_BREAKPOINT_:
         list[listtop].type = type;
         list[listtop].op1.name_id = left;
         if ( right )
           list[listtop].left = right - listtop;
         list[listtop].flags |= EPHEMERAL;
         break;

      case BACKQUOTE_START_:
         list[listtop].type = type;
         break;

      case BACKQUOTE_END_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].right = right - listtop;
         list[left].op1.skipsize = right-left+1; /* so START can jump over */
         break;

      /* command ',' expr */
      case ACOMMANDEXPR_:
         list[listtop].type = ACOMMANDEXPR_;
         list[listtop].left = left - listtop;
         list[listtop].right = right - listtop;
         list[listtop].datatype = REAL_TYPE;
         break;


      /* IF THEN ELSE parse tree

             ELSE_
             /     \
              IF_    command2
             /    \
          IFTEST_  command1
          /
     testexpr

     sequence: testexpr IFTEST_ command1 IF_ command2 ELSE_

     */
      case IFTEST_:
         list[listtop].type = IFTEST_;
         list[listtop].left = left - listtop; /* test expression */
         /* op1 will be offset for jump if condition false */
         list[listtop].stack_delta = -1;
         break;

      case IF_:
         list[listtop].type = IF_;
         list[listtop].left = left - listtop;  /* IFTEST_ */
         list[listtop].right = right - listtop; /* THEN command */
         list[left].op1.skipsize = listtop - left; /* in IFTEST_ node */
         /* op1 will be offset to skip ELSE part */
         break;

      case ELSE_: /* really the continue node at root of IF tree */
         list[listtop].type = ELSE_;
         list[listtop].left = left - listtop; /* IF_ node */
         if ( right ) list[listtop].right = right - listtop; /* command2 */
         list[left].op1.skipsize = listtop - left; /* in IF_ node */
         break;
      
      case DECLARE_LOCAL_:
         list[listtop].type = DECLARE_LOCAL_;
         list[listtop].op1.name_id = left; /* identifier */
         globals(left)->flags |= GLOB_LOCALVAR;
   /*     globals(left)->flags |= ORDINARY_PARAM; */
         break;
 
      case LOCAL_LIST_START_:
         list[listtop].type = LOCAL_LIST_START_;
         list[listtop].left = left-listtop;
         break;


      case INDEXSET_:
         list[listtop].type = INDEXSET_;
         /* "left" is previous indexset  */
         /* "right" is next index expression */
         /* note we have to use left pointer if any used at all for
            later nodes trying to find start of subtree */
         if ( left )
         { /* then left is previous indexlist */
           list[listtop].left = left-listtop;
           list[listtop].right = right-listtop;
           list[listtop].op1.indexcount = list[left].op1.indexcount+1; 
         } 
         else 
         { list[listtop].op1.indexcount = 1;
           list[listtop].left = right-listtop;
         }
         /* Sanity check on index */
         if ( list[right].type == PUSHCONST )
         { if ( (int)(list[right].op1.real) < 1 ) 
           { sprintf(errmsg,"Index %d must be positive.\n",
                      list[listtop].op1.indexcount);
             kb_error(2238,errmsg, COMMAND_ERROR);          
           }
         }
         break;

      case DIMENSIONSET_:
         /* "left" is previous indexset, except in leaf it is index expr  */
         /* "right" is next index expression, except 0 in leaf */
         list[listtop].type = DIMENSIONSET_;
         if ( left )  /* have previous dimensionset */
         { list[listtop].left = left-listtop;
           list[listtop].op1.indexcount = list[left].op1.indexcount+1;
           list[listtop].right = right-listtop;
         } else 
         { list[listtop].op1.indexcount = 1;
           list[listtop].left = right-listtop;
         }
         /* Sanity check on index */
         if ( list[right].type == PUSHCONST )
         { if ( (int)(list[right].op1.real) < 0 ) /* can be 0 in define */
           { sprintf(errmsg,"Index %d must be nonnegative.\n",
                      list[listtop].op1.indexcount);
             kb_error(2522,errmsg, COMMAND_ERROR);          
           }
         }
         break;

      case SINGLE_ASSIGN_:
   /* tree:      SINGLE_ASSIGN
                /          \
        SINGLE_ELEMENT_    SET_ATTRIBUTE_A
             /              /         \
         single            expr          index expr
      */
         list[listtop].type = type;
         list[listtop].op1.assigntype = assigntype;
         list[listtop].left = left-listtop;  /* single element */
         list[listtop].right = right-listtop; /* attribute and expression */
         list[listtop].stack_delta = -3;
         break;

      case DEFINE_EXTRA_: /* new element attribute */
         list[listtop].type = type;
         if ( left ) list[listtop].left = left-listtop; /* dimension expression */
         list[listtop].op2.eltype = right; /* element type */
         break;

      case DEFINE_EXTRA_INDEX_:  /* index added to extra attribute */
       { struct extra *ex;
         list[listtop].type = type;
         list[listtop].left = left-listtop;  /* definition header */
         list[listtop].right = right-listtop;  /* indexset expression */
         list[listtop].op1.extranum = list[left].op1.extranum; 
         list[listtop].op2.eltype = list[left].op2.eltype; 
         ex = EXTRAS(list[left].op2.eltype) + list[left].op1.extranum;
         if ( (ex->array_spec.datacount > 0) && (ex->array_spec.dim != list[right].op1.indexcount) )
         { sprintf(errmsg,"Cannot change number of dimensions of %s\n",
                ex->name);
           kb_error(4562,errmsg,COMMAND_ERROR);
         }
         ex->array_spec.dim = list[right].op1.indexcount;
         ex->flags |= DIMENSIONED_ATTR;
         list[listtop].stack_delta = -ex->array_spec.dim;
       }
       break;
 
      case ATTR_FUNCTION_: /* attribute function */
         list[listtop].type = type;
         list[listtop].left = left-listtop; /* main define  */
         break;

      case ATTR_FUNCTION_END_: /* attribute function */
         list[listtop].type = type;
         list[listtop].left = left-listtop; /* ATTR_FUNCTION node  */
         list[listtop].right = right-listtop; /* function */
         break;


      case RETURN_: /* end command */
         list[listtop].type = type;
         if ( left ) list[listtop].left = left - listtop; /* return expr */
         break;

      case RESET_COUNTS_:  /* set counts back to 0 */
         list[listtop].type = type;
         break;

      case FLUSH_COUNTS_:  /* print pending counts */
         list[listtop].type = type;
         break;

      case PAUSE_:  /* wait for user */
         list[listtop].type = type;
         break;

      case PRINT_PROFILING_:
      case RESET_PROFILING_:
         list[listtop].type = type;
         break;

      case BREAK_:
      case CONTINUE_:
         list[listtop].type = type;
         if ( loopdepth < 1 )
           kb_error(1388,"Cannot BREAK or CONTINUE unless inside a loop.\n",
            COMMAND_ERROR);
         if ( loopdepth < left )
           kb_error(1389,"BREAK or CONTINUE out of too many levels.\n",
            COMMAND_ERROR);
         list[listtop].op1.skipsize = loopbase[loopdepth-left]-listtop;
         list[listtop].op2.breakdepth = left; 
         break;

/* WHILE DO tree:     WHILE_END_
                      /        \
                WHILE_TOP_    command 
                 /
            testexpr

execution sequence: testexpr WHILE_TOP_ command WHILE_END_
*/
      case WHILE_TOP_:
         list[listtop].type = WHILE_TOP_;
         list[listtop].left = left - listtop;
         /* op1 will be offset to after WHILE_END_ */
         loopbase[loopdepth++] = listtop;
             /* find start of WHILE expression */
         for ( n = left ; list[n].left ; n += list[n].left ) ; 
         list[listtop].op4.contjump = n - listtop - 1; /* for continue */
         i = add_local_var(NULL,1);
         list[listtop].stackpos = get_local(i).offset;
         list[listtop].stack_delta = -1;
         break;

      case WHILE_END_:
         list[listtop].type = WHILE_END_;
         list[listtop].left = left - listtop; /* test */
         list[listtop].right = right - listtop; /* command */
         list[left].op1.skipsize = listtop - left; /* jump if test fails */
         list[left].op3.breakjump = listtop - left; /* for break */
         /* find start of WHILE test expression and set jump back */
         for ( n = left ; list[n].left ; n += list[n].left ) ; 
         list[listtop].op1.skipsize = n - listtop - 1; /* allow increment */
         loopdepth--;
         if ( loopdepth < 0 )
           kb_error(1390,"Internal error: loopdepth negative.\n",COMMAND_ERROR);

         break;


/*  DO WHILE tree:        DO_END_
                        /        \
                     DO_TOP_    test expr
                    /             / 
                   DO_ENTRY_     command

execution sequence:  DO_ENTRY command DO_TOP_ test expr DO_END_
*/
      case DO_ENTRY_:
         list[listtop].type = type;
         loopbase[loopdepth++] = listtop;
         /* op3 will hold offsets for break and continue */
         i = add_local_var(NULL,1);
         list[listtop].stackpos = get_local(i).offset;
         break;

      case DO_TOP_:
         list[listtop].type = type;
         list[listtop].left = left - listtop; /* DO_ENTRY */
         list[listtop].right = right - listtop; /* command */
         i = add_local_var(NULL,1);
         list[listtop].stackpos = get_local(i).offset;
         break;

      case DO_END_:
      { int entry;
        list[listtop].type = type;
        list[listtop].left = left - listtop; /* DO_TOP_ */
        list[listtop].right = right - listtop; /* command */
        /* find start of DO loop */
        for ( n = left ; list[n].left ; n += list[n].left ) ; 
        list[listtop].op1.skipsize = n - listtop - 1; /* allow increment */
        /* find start of test loop */
        for ( n = right ; list[n].left ; n += list[n].left ) ; 
        entry = left + list[left].left;  /* DO_ENTRY_ */
        list[entry].op3.breakjump = listtop-entry; /* break */
        list[entry].op4.contjump = n - entry - 1;  /* continue */
        loopdepth--;
        if ( loopdepth < 0 )
          kb_error(1391,"Internal error: loopdepth negative.\n",COMMAND_ERROR);
        list[listtop].stack_delta = -1;

        break;
      }

/* FOR syntax:   FOR ( command1 ; expr ; command2 ) command3 

 FOR tree:
                       FOR_END_
                      /       \
               FOR_TOP_       command3
              /       \
       FOR_HEAD_    command2
       /       \
  FOR_ENTRY    expr
      |
  command1

 FOR execution sequence:

 command1 expr FOR_HEAD_ command2 FOR_TOP_ command3 FOR_END_
 FOR_ENTRY.op3.breakjump holds break jump
 FOR_ENTRY.op4.contjump holds continue jump
 FOR_HEAD_.op1.skipsize is jump to command3
 FOR_HEAD_.op2.jumpsize is jump out of loop
 FOR_TOP_.op1.skipsize is jump back to expr
 FOR_END_.op1.skipsize is jump back to command2
*/
      case FOR_ENTRY_:
         list[listtop].type = type;
         list[listtop].left = left - listtop; /* command1 */ 
         loopbase[loopdepth++] = listtop;
         /* op3 will hold offsets for break and continue */
         i = add_local_var(NULL,1);
         list[listtop].stackpos = get_local(i).offset;
         break;

      case FOR_HEAD_:
         list[listtop].type = type;
         list[listtop].left = left - listtop; /* FOR_ENTRY */ 
         list[listtop].right = right - listtop; /* expr */
         list[left].op4.contjump = listtop-left; /* expr follows immediately */
         list[listtop].stack_delta = -1;
         break;

      case FOR_TOP_:
         list[listtop].type = type;
         list[listtop].left = left - listtop; /* FOR_HEAD_ */
         list[listtop].right = right - listtop; /* command2 */
         list[listtop].op1.skipsize = left + list[left].left - listtop;
         list[left].op1.skipsize = listtop - left; 
         break;

      case FOR_END_:
         list[listtop].type = type;
         list[listtop].left = left - listtop; /* FOR_TOP_ */
         if ( right )
           list[listtop].right = right - listtop; /* command3 */
         list[left+list[left].left].op2.jumpsize = 
                             listtop - left - list[left].left;
         list[listtop].op1.skipsize = (left + list[left].left) - listtop;
         n = left+list[left].left;  /* n = FOR_HEAD */
         n += list[n].left;   /* n = FOR_ENTRY */
         list[n].op3.breakjump = listtop-n; /* break */
         loopdepth--;
         if ( loopdepth < 0 )
          kb_error(2515,"Internal error: loopdepth negative.\n",COMMAND_ERROR);
         break;


/* >> redirection tree:     REDIRECT_END_
                             /        \
                        REDIRECT_     command
                        /
                     stringexpr

execution sequence: stringexpr REDIRECT_ command REDIRECT_END_
*/
      case REDIRECT_: /* >> */
      case REDIRECTOVER_: /* >>> */
         list[listtop].type = type;
         list[listtop].left = left - listtop; /* stringexpr */
         break;

      case REDIRECT_END_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         if ( right ) 
           list[listtop].right = right - listtop;
         list[listtop].stack_delta = -1;      
         break;

/* piping tree:      PIPE_END_
                    /        \
                PIPE_     command
                /
             stringexpr

execution sequence: stringexpr PIPE_ command PIPE_END_
*/
      case PIPE_:
         list[listtop].type = PIPE_;
         list[listtop].left = left - listtop; /* command */
         break;

      case PIPE_END_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         if ( right ) list[listtop].right = right - listtop;
         list[listtop].stack_delta = -1;
         break;

      case TRANSFORM_DEPTH_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;    /* number of transforms */
         list[listtop].stack_delta = -1;
         break;

      case VIEW_TRANSFORM_PARITY_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;    /* index of transform */
         list[listtop].datatype = REAL_TYPE;
         break;

      case VIEW_TRANSFORM_SWAP_COLORS_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;    /* index of transform */
         list[listtop].datatype = REAL_TYPE;
         break;

      case CREATE_VERTEX_:
         list[listtop].type = type;
         list[listtop].left = left - listtop; /* expression list for coords */
         if ( list[left].op1.indexcount != SDIM )
         { sprintf(msg,
          "Need exactly %d coordinates in NEW_VERTEX (...).\n",SDIM);
             kb_error(2217,msg,COMMAND_ERROR);
         }
         list[listtop].stack_delta = 1 - SDIM;
         list[listtop].datatype = REAL_TYPE;
         break;

      case CREATE_EDGE_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;    /* id of tail vertex */
         list[listtop].right = right - listtop;    /* id of head vertex */
         list[listtop].stack_delta = -1;
         list[listtop].datatype = REAL_TYPE;
         break;

      case CREATE_FACET_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;    /* expression list for edges */
         if ( (web.representation == SIMPLEX) 
             && (list[left].op1.argcount != web.dimension+1) )
         { sprintf(msg,"Need exactly %d vertices in NEW_FACET (...).\n",
              web.dimension+1);
           kb_error(2218,msg,COMMAND_ERROR);
         }
         list[listtop].stack_delta = 1 - list[left].op1.argcount;
         list[listtop].datatype = REAL_TYPE;
         break;

      case CREATE_BODY_:
         list[listtop].type = type;
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

      case ELINDEX_: 
         list[listtop].type = type;
         list[listtop].left = left-listtop;
         if ( right )
         { list[listtop].right = right - listtop;
           list[listtop].stack_delta = -1;
         }
         break;

      case PUSH_ELEMENT_ID_:
         list[listtop].type = PUSH_ELEMENT_ID_;
         if ( left == 0 )
           kb_error(6343,"Element id 0 is illegal.\n",RECOVERABLE); 
         list[listtop].op1.id = (element_id)abs(left) - 1;
         if ( left < 0 ) 
           invert(list[listtop].op1.id);
#ifdef MPI_EVOLVER
         list[listtop].op1.id |= ((element_id)right << TASK_ID_SHIFT);
#endif
         list[listtop].stack_delta = 1;
         break;
         

      case VALID_ELEMENT_:
         list[listtop].type = type;
         list[listtop].op1.eltype = left;
         list[listtop].left = right - listtop;    /* index expression */
         list[listtop].datatype = REAL_TYPE;
         break;

      case VALID_CONSTRAINT_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;    /* index expression */
         list[listtop].datatype = REAL_TYPE;
         break;

      case VALID_BOUNDARY_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;    /* index expression */
         list[listtop].datatype = REAL_TYPE;
         break;

      
      case MATRIX_MULTIPLY_:
        { struct array *a = get_name_arrayptr(list[left].op2.name_id,NULL,localbase);
          struct array *b = get_name_arrayptr(list[right].op2.name_id,NULL,localbase);
          struct array *c = get_name_arrayptr(list[int_val].op2.name_id,NULL,localbase);

          list[listtop].type = type;
          list[listtop].op1.name_id = list[left].op2.name_id;  /* first multiplicand */
          list[listtop].op2.name_id = list[right].op2.name_id;  /* second multiplicand */
          list[listtop].op3.name_id = list[int_val].op2.name_id;  /* result */
          list[listtop].stack_delta = -3;

          if ( a->datatype != REAL_TYPE)
           kb_error(3863,"matrix_multiply: first matrix is not of type REAL.\n",
            RECOVERABLE);
          if ( b->datatype != REAL_TYPE )
            kb_error(3864,"matrix_multiply: second matrix is not of type REAL.\n",
              RECOVERABLE);
          if ( c->datatype != REAL_TYPE )
            kb_error(3865,"matrix_multiply: third matrix is not of type REAL.\n",
              RECOVERABLE);
  
          if ( a == c )
           kb_error(3866,
            "matrix_multiply: first and third matrices must not be the same matrix.\n",
               RECOVERABLE);
          if ( b == c )
            kb_error(3867,
              "matrix_multiply: second and third matrices must not be the same matrix.\n",
                RECOVERABLE);
        }
         break;

      case MATRIX_INVERSE_:
        { struct array *a = get_name_arrayptr(list[left].op2.name_id,NULL,localbase);
          struct array *b = get_name_arrayptr(list[right].op2.name_id,NULL,localbase);

          list[listtop].type = type;
          list[listtop].op1.name_id = list[left].op2.name_id;  /* original */
          list[listtop].op2.name_id = list[right].op2.name_id;  /* inverse */
          list[listtop].stack_delta = -1;
          list[listtop].datatype = REAL_TYPE;

          if ( a->datatype != REAL_TYPE )
            kb_error(3874,"matrix_inverse: first matrix is not of type REAL.\n",
              RECOVERABLE);
          if ( b->datatype != REAL_TYPE )
            kb_error(3875,"matrix_inverse: second matrix is not of type REAL.\n",
              RECOVERABLE); 
 
          if ( a->dim != 2 )
            kb_error(3870,"matrix_inverse first array is not two-dimensional.\n",
              RECOVERABLE); 
          if ( b->dim != 2 )
            kb_error(3871,"matrix_inverse second array is not two-dimensional.\n",
              RECOVERABLE); 
         }
         break;

      case MATRIX_DETERMINANT_:
       { struct array *a = get_name_arrayptr(list[left].op2.name_id,NULL,localbase);

         list[listtop].type = type;
         list[listtop].op1.name_id = list[left].op2.name_id;  /* original */
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         
         if ( a->dim != 2 )
           kb_error(3219,"matrix_determinant array is not two-dimensional.\n",
             RECOVERABLE); 
        }
        break;

      case MERGE_VERTEX_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;    /* id of first vertex */
         list[listtop].right = right - listtop;    /* id of second vertex */
         list[listtop].stack_delta = -2;
         break;

      case MERGE_EDGE_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;    /* oid of first edge */
         list[listtop].right = right - listtop;    /* oid of second edge */
         list[listtop].stack_delta = -2;
         break;

      case MERGE_FACET_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;    /* oid of first facet */
         list[listtop].right = right - listtop;    /* oid of second facet */
         list[listtop].stack_delta = -2;
         break;


/* Aggregate loops.
(not all nodes need be present)
Tree:            AGGREGATE_END_
                  /          \
          AGGREGATE_INIT_    AGGREGATE_
             or SET_INIT_   /         \ 
                          WHERE_        SET_ATTRIBUTE_L
                          /      \        /            \
                    element_gen  expr    value expr    index expr

sequence: INIT_ element_gen expr WHERE_ value index SET_AT_L AGGR_  AGGR_END_
         prep                                    cleanup

If present, SET_ATTRIBUTE_L will do setting and AGGREGATE_ does looping.
Else AGGREGATE_ does both.
Actually, AGGREGATE_ type is collection of node types.
*/
      case SET_INIT_: 
         list[listtop].type = type;
         loopbase[loopdepth++] = listtop;
         i = add_local_var(NULL,1);
         list[listtop].stackpos = get_local(i).offset;
         break;

      case AGGREGATE_INIT_:
         list[listtop].type = type;
         list[listtop].op1.aggrtype = aggrtype;
         loopbase[loopdepth-1] = listtop;  
         i = add_local_var(NULL,1);
         list[listtop].stackpos = get_local(i).offset;
         switch ( aggrtype )
         { case MAX_: case MIN_: case SUM_: case COUNT_:
             list[listtop].stack_delta = 1;
             break;
           case AVG_:
             list[listtop].stack_delta = 2;
             break;
           case HISTOGRAM_: case LOGHISTOGRAM_:
             list[listtop].stack_delta = 3 + HISTBINS + 1;
             break;
         }
         /* op1 has aggregate type; op2 will have element type; */
         /* op3 will have break and continue jump offsets */
         break;

      case WHERE_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;  /* generator */
         list[listtop].right = right - listtop;  /* condition expr */
         list[listtop].stack_delta = -1;
         list[listtop].op2.localnum = list[left].op2.localnum;
         /* op1 will hold runtime where count */
         break;
         
      case AGGREGATE_:
         list[listtop].type = aggrtype;
         /* op1 holds offset back to start of loop */
         if ( list[left].type == SINGLE_ELEMENT_ )
         { list[listtop].op1.skipsize = 1; /* don't go back */
           nnode = list+left;
           nnode->op1.skipsize = listtop - left; /* in case of WHERE_ */
         }
         else if ( list[left].type == WHERE_ )
         { list[listtop].op1.skipsize = list[left].left + left - listtop;
           nnode = list+left+list[left].left;
           if ( nnode->type == SINGLE_ELEMENT_ )
              nnode->op1.skipsize = listtop - (left+list[left].left);
         }
         else 
         { list[listtop].op1.skipsize = left - listtop; /* no WHERE_ */
           nnode = list+left;
         }
         list[listtop].left = left - listtop;  /* generator */
         if ( right ) list[listtop].right = right - listtop;  /* value */
         /* also handy to have element type */
         etype = (nnode+nnode->left)->op1.eltype; /* from INIT */
         /* some type error checking and stuff */
         switch ( aggrtype )
         {
           case SUM_: case AVG_: case COUNT_: case HISTOGRAM_:
           case LOGHISTOGRAM_: case MAX_: case MIN_:
             list[listtop].stack_delta = -1; break;

           case FIX_: 
            if ( etype == BODY )
             kb_error(2230,
              "Cannot FIX bodies. To fix volume, do \"set body target expr\".\n",
                    COMMAND_ERROR);
            break;

           case UNFIX_: 
            if ( etype == BODY )
             kb_error(2881,
               "Cannot UNFIX bodies. To unfix volume, \"unset body target\".\n",
                    COMMAND_ERROR);
            break;

           case VERTEX_AVERAGE_:
           case RAW_VERTEX_AVERAGE_:
           case RAWEST_VERTEX_AVERAGE_:
             if ( web.representation == SIMPLEX )
               kb_error(2219,"No vertex_average in simplex model yet.\n",
                 COMMAND_ERROR);
             if ( (etype != VERTEX) )
               kb_error(1238,"Can vertex_average only vertices.\n",COMMAND_ERROR);
             break;
           case DELETE_: 
             if ( (etype != EDGE) && (etype != FACET) )
               kb_error(1392,"Can delete only edges or facets.\n",COMMAND_ERROR);
             break;

           case REVERSE_ORIENTATION_: 
             if ( (etype != EDGE) && (etype != FACET) )
               kb_error(4392,"Can reverse_orientation only edges or facets.\n",COMMAND_ERROR);
             break;

           case POP_:
             if ( web.representation == SIMPLEX )
                kb_error(2432,"Can pop only in SOAPFILM or STRING model.\n",
                  COMMAND_ERROR);
             if ( (etype != EDGE) && (etype != VERTEX) )
                kb_error(2433,"Can pop only edges or vertices.\n",
                  COMMAND_ERROR);
             if ( (etype == EDGE) && (web.representation==STRING) )
                kb_error(2434,"Can pop edges only in soapfilm model.\n",
                   COMMAND_ERROR);
             break;

           case POP_TRI_TO_EDGE_:
             if ( web.representation != SOAPFILM )
                kb_error(2803,"Can pop_tri_to_edge only in SOAPFILM model.\n",
                  COMMAND_ERROR);
             if ( etype != FACET )
                kb_error(2804,"Can pop_tri_to_edge only facets.\n",
                  COMMAND_ERROR);
             break;

           case POP_EDGE_TO_TRI_:
             if ( web.representation != SOAPFILM )
                kb_error(2806,"Can pop_edge_to_tro only in SOAPFILM model.\n",
                  COMMAND_ERROR);
             if ( etype != EDGE )
                kb_error(2807,"Can pop_edge_to_tri only edges.\n",
                  COMMAND_ERROR);
             break;

           case POP_QUAD_TO_QUAD_:
             if ( web.representation != SOAPFILM )
                kb_error(2808,"Can pop_quad_to_quad only in SOAPFILM model.\n",
                  COMMAND_ERROR);
             if ( etype != FACET )
                kb_error(2809,"Can pop_quad_to_quad only facets.\n",
                  COMMAND_ERROR);
             break;

           case EDGESWAP_: 
             if ( web.representation != SOAPFILM )
                kb_error(1393,"Can edgeswap only in the SOAPFILM model. Did you mean t1_edgeswap?\n",
                   COMMAND_ERROR);
             if ( (etype != EDGE) )
                kb_error(1394,"Can edgeswap only edges.\n",COMMAND_ERROR);
             break;

           case T1_EDGESWAP_: 
             if ( web.representation != STRING )
                kb_error(3910,"Can t1_edgeswap only in the STRING model.\n",
                   COMMAND_ERROR);
             if ( (etype != EDGE) )
                kb_error(3657,"Can t1_edgeswap only edges.\n",COMMAND_ERROR);
             break;

           case EQUIANGULATE_: 
             if ( web.representation != SOAPFILM )
                kb_error(2500,"Can equiangulate only in the SOAPFILM model.\n",
                   COMMAND_ERROR);
             if ( (etype != EDGE) )
                kb_error(2501,"Can edgeswap only edges.\n",COMMAND_ERROR);
             break;

           case REFINE_: 
             if ( (etype != EDGE) && (etype != FACET) )
                kb_error(1241,"Can refine only edges or facets.\n",COMMAND_ERROR);

             break;
           case SET_NO_DISPLAY_: 
             if ( etype != FACET )
                kb_error(1265,"No_display only applies to facets.\n",
                   COMMAND_ERROR);
             break;
           case SET_NONCONTENT_: 
             if ( (etype != EDGE) && (etype != FACET) )
                kb_error(2902,"Noncontent only applies to edges or facets.\n",
                   COMMAND_ERROR);
             break;
           case SET_HIT_PARTNER_: 
             if ( etype != VERTEX )
                kb_error(3001,"Hit_partner only applies to vertices.\n",
                   COMMAND_ERROR);
             break;
           case SET_NO_REFINE_: 
             if ( (etype != EDGE) && (etype != FACET) )
                kb_error(1395,"No_refine only applies to edges or facets.\n",
                   COMMAND_ERROR);
             break;
           case SET_CONSTRAINT_:
           case UNSET_CONSTRAINT_:
           case SET_BOUNDARY_:
           case UNSET_BOUNDARY_:
              list[listtop].stack_delta = -1;
              break;
           case SET_EXTRA_ATTR_:
            { struct extra *ex;
              ex = EXTRAS(etype);
              for ( n = 0 ; n < web.skel[etype].extra_count ; n++,ex++ )
              if ( stricmp(ex->name,set_extra_name) == 0 ) break;
              if ( n == web.skel[etype].extra_count )
                kb_error(1396,"Internal error: Invalid extra attribute number.\n",COMMAND_ERROR);
              n += etype << ESHIFT; /* handy to have type here */
              list[listtop].op3.extra_info = n;
              list[listtop].flags |= EPHEMERAL;
              list[listtop].stack_delta = -ex->array_spec.dim-1;
            }
            break;
          }
        if ( (aggrtype >= SET_COORD_) && (aggrtype <= SET_COORD_ + MAXCOORD))
        { list[listtop].type = SET_COORD_;
          n = aggrtype - SET_COORD_ - 1; 
          if ( n >= SDIM )
             kb_error(2220,"Coordinate dimension exceeds space dimension.\n",
               COMMAND_ERROR);
          if ( etype != VERTEX )
             kb_error(1398,"Can set coordinates only for vertices.\n",
                  COMMAND_ERROR);
          list[listtop].op2.coordnum = n;
        }
        else if ((aggrtype>=SET_PARAM_) && (aggrtype <= SET_PARAM_+MAXPARAM))
        { list[listtop].type = SET_PARAM_;
          list[listtop].op2.coordnum = aggrtype - SET_PARAM_ - 1; 
          if ( etype != VERTEX )
             kb_error(1399,"Can set parameters only for vertices.\n",
                  COMMAND_ERROR);
        }
        else list[listtop].op2.eltype = etype;
        break;

      case AGGREGATE_END_:
        { struct treenode *wnode;
          list[listtop].type = type;
          list[listtop].op1.aggrtype = aggrtype;
          list[listtop].left = left - listtop;  /* aggr init */
          list[listtop].right = right - listtop;  /* aggr op */
          list[listtop].stack_delta =  -list[left].stack_delta - 3;
          list[listtop].datatype = REAL_TYPE;
          if ( (aggrtype == SUM_) || (aggrtype == COUNT_) ||
               (aggrtype == AVG_) || (aggrtype == MAX_ ) ||
               (aggrtype == MIN_ ) ) list[listtop].stack_delta += 1;
          wnode = list+right + list[right].left;
          list[listtop].flags |= IN_ELEMENT_LOOP;
          if ( wnode[wnode->left].type != SINGLE_ELEMENT_ )
          { if ( wnode->type == WHERE_ )
            { nnode = wnode + wnode->left;
              nnode->op1.skipsize = (int)((list + listtop) - nnode);
            }  
            else
              wnode->op1.skipsize = listtop - (right + list[right].left);
          }
        }
        /* element type in aggr_init node */
        list[left].op2.eltype = list[right].op2.eltype;
        list[left].op3.breakjump = listtop-left-1; /* break to here */
        list[left].op4.contjump = right-left+list[right].op1.skipsize-1;
        list[loopbase[loopdepth-1]].op3.breakjump = 
            listtop-loopbase[loopdepth-1]-1; /* break to here */
        loopdepth--;
        if ( loopdepth < 0 )
          kb_error(1400,"Internal error: loopdepth negative.\n",COMMAND_ERROR);
        break;
         

/* Element generator.
Tree:              WHERE_
                  /      \
           element_gen    expr


                SINGLE_ELEMENT_
                /         
              INDEXED_SUBTYPE_
             /             \
          single        expr

             INDEXED_ELEMENT_
             /
          expr

         SYMBOL_ELEMENT_

              NEXT_ELEMENT_
              /
         INIT_ELEMENT_


*/
      case INIT_ELEMENT_:
        if ( (list[listtop-1].type != AGGREGATE_INIT_)
           && (list[listtop-1].type != SET_INIT_) )
        { int k;  /* for break and continue */
          int n;
          i = add_local_var(NULL,1);
          n = get_local(i).offset;
          list[listtop].stackpos = n; 
          for ( k = listtop-1; k >= 0 ; k-- )
           if ( list[k].type==AGGREGATE_INIT_ ||
             list[k].type==SET_INIT_ )
           { list[k].stackpos = n; break; }
        }
        list[listtop].op1.eltype = left; /* save type */
        list[listtop].op2.localnum = use_given_id ? 0 : add_local_var(NULL,1);
        /* op5.string will have name, if any */
        switch ( left )
        { case VERTEX: etype = INIT_VERTEX_; break;
          case EDGE  : etype = INIT_EDGE_  ; break;
          case FACET : etype = INIT_FACET_ ; break;
          case BODY  : etype = INIT_BODY_  ; break;
          case FACETEDGE  : etype = INIT_FACETEDGE_  ; break;
          default : kb_error(1401,"Internal error: Bad INIT_ELEMENT_ type.\n",
             COMMAND_ERROR);

        }
        list[listtop].type = etype;
        list[listtop].stack_delta = 3;
        break;
 
      case INIT_SUBELEMENT_: /* subelement of named element */
         etype = right;
         list[listtop].op1.eltype = etype; /* save type */
         list[listtop].left = left - listtop; /* single element */
         list[listtop].op2.localnum = list[left].op2.localnum;
         { int k;  /* for break and continue */
           int n;
           i = add_local_var(NULL,1);
           n = get_local(i).offset;
           list[listtop].stackpos = 0;
           for ( k = listtop-1; k >= 0 ; k-- )
           if ( list[k].type==AGGREGATE_INIT_ ||
            list[k].type==SET_INIT_ )
             { list[k].stackpos = n; break; }
         }
        
         switch ( list[left].op1.eltype ) /* parent type */
         { case VERTEX:
             switch ( etype )
             { 
               case EDGE  : etype = INIT_VERTEX_EDGE_  ;
                 if ( web.representation == SIMPLEX )
                   kb_error(1402,"Vertex edge iterator not valid in simplex model.\n",
                  COMMAND_ERROR); 
                 break;
               case FACET : etype = INIT_VERTEX_FACET_ ;
                 break;
               case BODY  : etype = INIT_VERTEX_BODY_  ; break;
               default : kb_error(1403,"Cannot do vertices of vertex.\n",COMMAND_ERROR);
             } 
             break;
          case EDGE:
             switch ( etype )
             { case VERTEX: etype = INIT_EDGE_VERTEX_; break;
               case EDGE : kb_error(1406,"Cannot do edges of edge.\n",
                    COMMAND_ERROR);
               case FACET : etype = INIT_EDGE_FACET_ ; 
                  if ( web.representation == SIMPLEX )
                    kb_error(1404,"Edge facet iterator not valid in simplex model.\n",
                      COMMAND_ERROR);
                  break;
               case BODY  : etype = INIT_EDGE_BODY_  ;
                  if ( web.representation == SIMPLEX )
                      kb_error(1405,"Edge body iterator not valid in simplex model.\n",
                        COMMAND_ERROR);
                  break;
               case FACETEDGE: etype = INIT_EDGE_FACETEDGE_ ;
                  break;
               default : kb_error(2830,"Cannot do facetedges of edge.\n",
                 COMMAND_ERROR);
             } 
             break;
          case FACET:
            switch ( etype )
             { case VERTEX: etype = INIT_FACET_VERTEX_; break;
               case EDGE  : etype = INIT_FACET_EDGE_  ; 
                 if ( web.representation == SIMPLEX )
                   kb_error(1407,"Facet edge iterator not valid in simplex model.\n",
                  COMMAND_ERROR);
                 break;
               case BODY  : etype = INIT_FACET_BODY_  ;
                 if ( web.representation == SIMPLEX )
                    kb_error(1408,"Facet body iterator not valid in simplex model.\n",
                      COMMAND_ERROR);
                 break;
               case FACETEDGE : kb_error(3755,"Cannot do facetedges of facet.\n",COMMAND_ERROR);
               default : kb_error(1409,"Cannot do facets of facet.\n",COMMAND_ERROR);
             } 
             break;
          case BODY:
            switch ( etype )
             { case VERTEX: etype = INIT_BODY_VERTEX_; break;
               case EDGE  : etype = INIT_BODY_EDGE_  ; break;
               case FACET : etype = INIT_BODY_FACET_ ; break;
               default : kb_error(1410,"Internal error: Bad INIT_ELEMENT_ type.\n",COMMAND_ERROR);
             } 
             break;
          case FACETEDGE:
            switch ( etype )
             { 
               case EDGE  : etype = INIT_FACETEDGE_EDGE_  ; break;
               case FACET : etype = INIT_FACETEDGE_FACET_ ; break;
               default : kb_error(3914,"Facetedge can only have edge or facet subelement.\n",COMMAND_ERROR);
             } 
             break;

          default: kb_error(1411,"Cannot do bodies of body.\n",COMMAND_ERROR);

         }  
         list[listtop].type = etype;
         list[listtop].stack_delta = 3;
         break;
 
      case NEXT_ELEMENT_:
         list[listtop].left = left - listtop;  /* element initializer */
         /* op5.string will have name, if any */
         /* op2.localnum will point to iteration local variable */
         /* op1 reserved for jump to end of loop */
         /* left->op2.localnum will point to parent */
         list[listtop].op2.localnum = use_given_id ? 0 : add_local_var(NULL,1);
         if ( loopdepth > 0 ) 
           list[loopbase[loopdepth-1]].op4.contjump = 
             listtop-loopbase[loopdepth-1]-1;  /* CONTINUE to here */
         switch ( list[left].type ) /* parent type */
        { 
          case INIT_VERTEX_EDGE_  : type = NEXT_VERTEX_EDGE_  ; break;
          case INIT_VERTEX_FACET_ : type = NEXT_VERTEX_FACET_ ; break;
          case INIT_VERTEX_BODY_  : type = NEXT_VERTEX_BODY_  ; break;
          case INIT_EDGE_VERTEX_: type = NEXT_EDGE_VERTEX_; break;
          case INIT_EDGE_FACET_ : type = NEXT_EDGE_FACET_ ; break;
          case INIT_EDGE_FACETEDGE_ : type = NEXT_EDGE_FACETEDGE_ ; break;
          case INIT_EDGE_BODY_  : type = NEXT_EDGE_BODY_  ; break;
          case INIT_FACET_VERTEX_: type = NEXT_FACET_VERTEX_; break;
          case INIT_FACET_EDGE_  : type = NEXT_FACET_EDGE_  ; break;
          case INIT_FACET_BODY_  : type = NEXT_FACET_BODY_  ; break;
          case INIT_BODY_VERTEX_: type = NEXT_BODY_VERTEX_; break;
          case INIT_BODY_EDGE_  : type = NEXT_BODY_EDGE_  ; break;
          case INIT_BODY_FACET_ : type = NEXT_BODY_FACET_ ; break;
          case INIT_VERTEX_: type = NEXT_VERTEX_; break;
          case INIT_EDGE_  : type = NEXT_EDGE_  ; break;
          case INIT_FACET_: type = NEXT_FACET_ ; break;
          case INIT_BODY_  : type = NEXT_BODY_  ; break;
          case INIT_FACETEDGE_ : type = NEXT_FACETEDGE_; break;
          case INIT_FACETEDGE_EDGE_ : type = NEXT_FACETEDGE_EDGE_; break;
          case INIT_FACETEDGE_FACET_ : type = NEXT_FACETEDGE_FACET_; break;
          default : kb_error(1412,"Internal error: Bad NEXT_ELEMENT_ type.\n",COMMAND_ERROR);

        }
         list[listtop].type = type;
         break;
 
      case RITZ_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].right = right - listtop;
         list[listtop].stack_delta = -2;
         break;

      case WRAP_VERTEX_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].right = right - listtop;
         list[listtop].stack_delta = -2;
         break;

      case LANCZOS_:
      case EIGENPROBE_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].stack_delta = -1;
         if ( right ) 
         { list[listtop].right = right - listtop;
           list[listtop].stack_delta = -2;
         }
         break;

      case HESSIAN_SADDLE_:
      case HESSIAN_SEEK_:
         list[listtop].type = type;
         if ( left ) 
         { list[listtop].left = left - listtop;
           list[listtop].stack_delta = -1;
         }     
         break;

      case MOVE_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].stack_delta = -1;
         break;

      case AREAWEED_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].stack_delta = -1;
         break;

      case METIS_:
      case METIS_READJUST_:
      case KMETIS_:
      case BODY_METIS_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].stack_delta = -1;
         break;

      case OMETIS_:
         list[listtop].type = type;
         if ( left )
         {  list[listtop].left = left - listtop;
            list[listtop].stack_delta = -1;
         }
         break;

      case EDGEWEED_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].stack_delta = -1;
         break;

      case EDGEDIVIDE_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].stack_delta = -1;
         break;

    case HISTORY_:
         list[listtop].type = type;
         break;
     
    case LAGRANGE_:
         list[listtop].type = type;
         list[listtop].left = left-listtop; /* degree approximation */
         list[listtop].stack_delta = -1;
         break;
     
    case BURCHARD_:
         list[listtop].type = type;
         list[listtop].op1.maxsteps = left; /* number of steps */
         break;
     
    case ZOOM_:
         list[listtop].type = type;
         if ( left )
         {  list[listtop].left = left - listtop; /* vertex number */
            list[listtop].stack_delta = -1;
         }
         if ( right ) 
         {  list[listtop].left = right - listtop;  /* radius expression */
            list[listtop].stack_delta += -1;
         }
         break;
     
    case SET_GRAVITY_: 
         list[listtop].type = type;
         list[listtop].left = left - listtop;  /* value expression */
         list[listtop].op1.assigntype = assigntype;
         list[listtop].stack_delta = -1;
         break;

    case SET_AMBIENT_PRESSURE_: case SET_GAP_CONSTANT_:
    case NOTCH_: case SET_AUTOCHOP_: case SET_FIXED_AREA_:
    case SET_OPTIMIZE_: case SET_SCALE_:
    case JIGGLE_: case SET_BACKGROUND_:
    case PRINT_: case STRPRINT_:
    case INVOKE_P_MENU_: case SET_MODEL_: case SKINNY_: case TORDUP_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;  /* value expression */
         list[listtop].stack_delta = -1;
         break;

    case SET_INTERNAL_:
         list[listtop].type = type;
         list[listtop].left = right - listtop;  /* value expression */
         list[listtop].op1.name_id = left;  /* variable */
         list[listtop].op2.assigntype = assigntype;  
         list[listtop].stack_delta = (assigntype == ASSIGN_) ? -1 : 0;

         switch(left)
        { /* break on settable variables */
          case V_AMBIENT_PRESSURE: case V_HESSIAN_SLANT_CUTOFF:
          case GRAV_CONST_: case V_BREAKFLAG_: case V_VISIBILITY_DEBUG_:
          case V_TOLERANCE: case V_HESS_EPSILON: 
          case V_SCALE_SCALE: case V_TIME: case V_SCALE: 
          case V_JIG_TEMP: case V_SCALE_LIMIT: case V_GAP_CONSTANT:
          case V_THICKNESS: case V_TARGET_TOLERANCE: case V_SCROLLBUFFERSIZE_:
          case V_PICKVNUM: case V_PICKENUM: case V_PICKFNUM:
          case V_LINEAR_METRIC_MIX: case V_QUADRATIC_METRIC_MIX:
          case V_RANDOM_SEED: break; case V_BRIGHTNESS: 
          case V_BACKGROUND: break; case V_LAST_ERROR: case V_DIFFUSION_:
          case V_INTEGRAL_ORDER_1D: case V_INTEGRAL_ORDER_2D: case V_DIFFUSION:
          case V_PS_STRINGWIDTH_: case V_PS_FIXEDEDGEWIDTH_:
          case V_PS_TRIPLEEDGEWIDTH_: case V_PS_GRIDEDGEWIDTH_:
          case V_PS_CONEDGEWIDTH_: case V_PS_BAREEDGEWIDTH_:
          case V_PS_LABELSIZE_: case V_MINDEG_MARGIN: case V_MINDEG_DEBUG_LEVEL:
          case V_MINDEG_MIN_REGION_SIZE: case V_WINDOW_ASPECT_RATIO:
          case V_CORONA_STATE: case V_STRING_CURVE_TOLERANCE:
            break;


          case V_INTEGRAL_ORDER:
            kb_error(1413,
               "Please use integral_order_1d or integral_order_2d.\n",WARNING);
            break;
          default: 
            sprintf(msg,"Cannot set internal variable '%s'.\n",keywordname(left));
            kb_error(1414,msg,EXPRESSION_ERROR);

        }
         list[listtop].stack_delta = -1;
         break;

    case FIX_QUANTITY_: case UNFIX_QUANTITY_:
    case SET_Q_FIXED_: case SET_Q_ENERGY_: case SET_Q_INFO_: 
    case SET_Q_CONSERVED_:
         list[listtop].type = type;
         list[listtop].op1.quant_id = left;  /* named quantity var number */
         list[listtop].flags |= EPHEMERAL;
         break;

    case FIX_PARAMETER_:
    case UNFIX_PARAMETER_:
         list[listtop].type = type;
         list[listtop].op1.name_id = left; /* variable number */
         if ( !(globals(left)->flags & ORDINARY_PARAM ) )
         { sprintf(errmsg,"'%s' cannot be made an optimizing parameter.\n",
              globals(left)->name);
           kb_error(2223,errmsg,COMMAND_ERROR);
         }
         list[listtop].flags |= EPHEMERAL;
         break;

    case PRINT_ARRAY_LVALUE_:
         list[listtop].type = type;
         list[listtop].left = left-listtop;  /* arraylvalue */
         list[listtop].op2.name_id = list[left].op2.name_id;
         list[listtop].stack_delta = -1;
         break;

    case PRINT_ARRAYPART_:
         list[listtop].type = type;
         list[listtop].left = left-listtop;  /* arrayhead */
         list[listtop].op1.indexcount = list[left].op2.indexcount;
         list[listtop].flags |= EPHEMERAL;
         list[listtop].stack_delta = -list[listtop].op1.indexcount;
         break;

    case PRINT_ARRAY_:
    case PRINT_PROCEDURE_:
    case EXPRINT_PROCEDURE_:
    case PRINT_LETTER_:
         list[listtop].type = type;
         list[listtop].op1.name_id = left;
         list[listtop].flags |= EPHEMERAL;
         break;

    case PRINT_PERM_PROCEDURE_:
         list[listtop].type = type;
         list[listtop].op1.name_id = left;
         break;

    case UNREDEFINE_SINGLE_:
         list[listtop].type = type;
         list[listtop].op1.letter = right; 
         if ( right > 127 )
           kb_error(1415,"Illegal unredefine.\n",COMMAND_ERROR);
         break;

    case REDEFINE_SINGLE_:
         list[listtop].type = type;
         list[listtop].op1.letter = right; /* letter */
         list[listtop].op5.locals = localbase;
         if ( localbase )
            localbase->flags |= LL_IN_USE;
         if ( right > 127 ) 
           kb_error(1416,"Illegal redefine.\n",COMMAND_ERROR);
         right = listtop;
         subtree_swap(&left,&right);
         list[right].op2.jumpsize = left - right; /* proc root */
         list[listtop].line_no = line_no;
         list[listtop].file_no = file_no;
         listtop++;
         list[listtop].type = SET_PROC_END_;
         list[listtop].left = right - listtop; /* action node */
         list[listtop].right = left - listtop; /* procedure */
         break;

    case SET_PROCEDURE_:
       { struct global *g = globals(right);
         list[listtop].type = type;
         list[listtop].op1.name_id = right; /* variable number */
         if ( g->flags & ORDINARY_PARAM )
         { sprintf(errmsg,"Cannot redefine variable '%s' as a command.\n",
             g->name);
           kb_error(3357,errmsg,COMMAND_ERROR);
         }
         g->flags |= SUBROUTINE;
         list[listtop].op5.locals = localbase; 
         if ( localbase )
         { list[listtop].flags |= HAS_LOCALLIST;
           localbase->flags |= LL_IN_USE;
         }
         right = listtop;
         subtree_swap(&left,&right);
         list[right].op2.jumpsize = left - right; /* proc root */
         list[listtop].line_no = line_no;
         list[listtop].file_no = file_no;
         listtop++;
         list[listtop].type = SET_PROC_END_;
         list[listtop].left = right - listtop; /* action node */
         list[listtop].right = left - listtop; /* procedure */
         list[listtop].flags |= EPHEMERAL;
         break;
       }

    case SET_PERM_PROCEDURE_:
       { struct global *g = globals(right);
         list[listtop].type = type;
         list[listtop].op1.name_id = right; /* variable number */
         if ( g->flags & ORDINARY_PARAM )
         { sprintf(errmsg,"Cannot redefine variable '%s' as a command.\n",
             g->name);
           kb_error(2516,errmsg,COMMAND_ERROR);
         }
         g->flags |= SUBROUTINE;
         right = listtop;
         subtree_swap(&left,&right);
         /* see if any ephemeral stuff, and mark as part of permanent cmd */
         for ( n = right ; n < listtop ; n++ )
         { if ( list[n].flags & EPHEMERAL )
           { sprintf(errmsg,"Non-permanent items in definition of %s.\n",
               g->name);
             kb_error(2517,errmsg,COMMAND_ERROR);
           }
           list[n].flags |= PERMNODE; 
         }
         list[right].op2.jumpsize = left - right; /* proc root */
         list[listtop].line_no = line_no;
         list[listtop].file_no = file_no;
         list[listtop].op5.locals = localbase; 
         if ( localbase )
         { list[listtop].flags |= HAS_LOCALLIST;
           localbase->flags |= LL_IN_USE;
         }
         listtop++;
         list[listtop].type = SET_PERM_PROC_END_;
         list[listtop].left = right - listtop; /* action node */
         list[listtop].right = left - listtop; /* procedure */
         break;
       }

    case ARGLIST_:
         list[listtop].type = type;
         list[listtop].left = left ? left - listtop : 0; /* prev args */
         if ( right )
         { list[listtop].op1.name_id = right; /* name id */
           globals(right)->flags |= ORDINARY_PARAM;
         }
         list[listtop].op2.argcount =  /* count of arguments */
              (left ? list[left].op2.argcount : 0) + (right?1:0);
         list[listtop].op3.argtype = int_val; /* argument type */
         break;

    case FUNCTION_DEF_START_:
    case FUNCTION_PROTO_START_:
       { struct global *g = globals(left);
         list[listtop].type = type;
         list[listtop].op1.name_id = left; /* name id */
         if ( g->flags & ORDINARY_PARAM )
         { sprintf(errmsg,"Cannot redefine '%s' as a function.\n",g->name);
           kb_error(3358,errmsg,COMMAND_ERROR);
         }
         g->flags |= FUNCTION_NAME;
         /* op2.jumpsize for skip ahead, set later */
         /* op3.argcount for argument count */
         list[listtop].op4.ret_type = right;  /* type of return value */
         break;
       }
       
    case FUNCTION_HEAD_:
         list[listtop].type = type;
         list[listtop].left = left - listtop; /* function def start */
         list[listtop].right = right - listtop; /* function arglist */
         list[listtop].op1.name_id = list[left].op1.name_id; /* name id */
         list[left].op3.argcount = list[right].op2.argcount; /* arg count */
         globals(list[left].op1.name_id)->attr.procstuff.argcount =
              list[right].op2.argcount;
         list[listtop].op4.ret_type = list[left].op4.ret_type; 
         break;

    case SET_FUNCTION_:
         list[listtop].type = type;
         list[listtop].left = left - listtop; /* function head */
         list[listtop].right = right - listtop; /* function body */
         list[listtop].op1.name_id = list[left].op1.name_id; /* name id */
         n = left + list[left].left;
         list[n].op2.jumpsize = listtop - n; /* for FUNCTION_START_ jump */
         list[listtop].flags |= EPHEMERAL;
         list[listtop].op4.ret_type = list[left].op4.ret_type;
         if ( topflag ) 
           globals(list[left].op1.name_id)->flags |= IN_DATAFILE_TOP;
         break;

    case FUNCTION_PROTO_:
         list[listtop].type = type;
         list[listtop].left = left - listtop; /* function head */
         list[listtop].op1.name_id = list[left].op1.name_id; /* name id */
         n = left + list[left].left;
         list[n].op2.jumpsize = listtop - n; /* for FUNCTION_PROTO_START_ jump */
         list[n].type = FUNCTION_PROTO_START_;
         list[listtop].flags |= EPHEMERAL;
         list[listtop].op4.ret_type = list[left].op4.ret_type; /* type */
         break;

    case FUNCTION_CALL_:
       { struct global *g = globals(left);
         list[listtop].type = type;
         if ( right )
           list[listtop].left = right - listtop; /* argument expressions */
         list[listtop].op1.name_id = left;  /* function name id */
         if ( list[right].op1.argcount != g->attr.procstuff.argcount )
         { sprintf(errmsg,"Function \"%s\" needs %d arguments; call has %d,\n",
               g->name,g->attr.procstuff.argcount,list[right].op1.argcount);
           kb_error(2620,errmsg,COMMAND_ERROR);
         }
         list[listtop].op2.argcount = g->attr.procstuff.argcount;
         list[listtop].stack_delta = 1 - g->attr.procstuff.argcount;
         list[listtop].datatype = g->type <= MAX_NUMERIC_TYPE ? REAL_TYPE :
                 g->type;
         break;
      }
    case FUNCTION_CALL_RETURN_:
         list[listtop].type = type;
         list[listtop].left = left - listtop; /* to FUNCTION_CALL */
         list[listtop].op2.argcount = list[left].op2.argcount; 
         break;


    case PROCEDURE_DEF_START_:
    case PROCEDURE_PROTO_START_:
       { struct global *g = globals(left);
         list[listtop].type = type;
         list[listtop].op1.name_id = left; /* name id */
         if ( g->flags & ORDINARY_PARAM )
         { sprintf(errmsg,"Cannot redefine '%s' as a function.\n",g->name);
           kb_error(1417,errmsg,COMMAND_ERROR);
         }
         g->flags |= PROCEDURE_NAME;
         /* op2.jumpsize for skip ahead, set later */
         /* op3.argcount for argument count */
         break;
       }
       
    case PROCEDURE_HEAD_:
       { struct global *g;
         list[listtop].type = type;
         list[listtop].left = left - listtop; /* procedure def start */
         list[listtop].right = right - listtop; /* procedure arglist */
         list[listtop].op1.name_id = list[left].op1.name_id; /* name id */
         list[left].op3.argcount = list[right].op2.argcount; /* arg count */
         g = globals(list[left].op1.name_id);
         g->attr.procstuff.argcount = list[right].op2.argcount;
         break;
       }
    case SET_ARGSPROC_:
         list[listtop].type = type;
         list[listtop].left = left - listtop; /* function head */
         list[listtop].right = right - listtop; /* function body */
         list[listtop].op1.name_id = list[left].op1.name_id; /* name id */
         n = left + list[left].left;
         list[n].op2.jumpsize = listtop - n; /* for PROCEDURE_START_ jump */
         list[listtop].flags |= EPHEMERAL;
         if ( topflag ) 
           globals(list[left].op1.name_id)->flags |= IN_DATAFILE_TOP;
         break;

    case PROCEDURE_PROTO_:
         list[listtop].type = type;
         list[listtop].left = left - listtop; /* function head */
         list[listtop].op1.name_id = list[left].op1.name_id; /* name id */
         n = left + list[left].left;
         list[n].op2.jumpsize = listtop - n; /* for PROCEDURE_PROTO_START_ jump */
         list[n].type = PROCEDURE_PROTO_START_;
         list[listtop].flags |= EPHEMERAL;
         break;

    case PROCEDURE_CALL_:
       { struct global *g = globals(left);
         list[listtop].type = type;
         if ( right )
           list[listtop].left = right - listtop; /* argument expressions */
         list[listtop].op1.name_id = left;  /* procedure name id */
         if ( list[right].op1.argcount != g->attr.procstuff.argcount )
         { sprintf(errmsg,"Procedure \"%s\" needs %d arguments; call has %d.\n",
               g->name,g->attr.procstuff.argcount,list[right].op1.argcount);
           kb_error(2623,errmsg,COMMAND_ERROR);
         }
         list[listtop].op2.argcount = g->attr.procstuff.argcount; 
         list[listtop].stack_delta = -g->attr.procstuff.argcount;
         break;
      }

    case PROCEDURE_CALL_RETURN_:
         list[listtop].type = type;
         list[listtop].left = left - listtop; /* to PROCEDURE_CALL */
         list[listtop].op2.argcount = list[left].op2.argcount; 
         break;

    case DEFINE_IDENT_:
       { struct global *g = globals(left);
         list[listtop].type = type;
         list[listtop].op1.name_id = left; /* variable number */
         list[listtop].op2.valtype = right; /* type */
         g->type = right;
         if ( right == STRING_TYPE ) g->flags |= STRINGVAL;
         else if ( !(g->flags & ANY_TYPE)  )
         { g->flags |= ORDINARY_PARAM;
           list[listtop].flags |= EPHEMERAL;
           g->attr.varstuff.delta = OPTPARAM_DELTA;
           g->attr.varstuff.pscale = 1.0;
         }
         break;
       }
       
    case DEFINE_ARRAY_:
       { struct global *g = globals(left);
         int datatype = int_val; /* kludge due to shortage of arguments */
         int dim = list[right].op1.indexcount;
         int datastart;

         if ( g->flags & ORDINARY_PARAM )
         { sprintf(errmsg,
             "Illegal to convert ordinary variable \"%s\" into array.\n",
               g->name); 
           kb_error(3289,errmsg,Q_ERROR);
         }

         if ( g->flags & FIXED_SIZE_ARRAY )
         { sprintf(errmsg,"Cannot re-declare a local fixed-size array %s.\n",
               g->name);
           kb_error(3176,errmsg,COMMAND_ERROR);
         }

         if ( dim > MAXARRAYDIMS )
         { sprintf(errmsg,
                "Maximum number of array dimensions is %d; %s has %d.\n",
                   MAXARRAYDIMS,g->name,dim);
           kb_error(3177,errmsg,COMMAND_ERROR);
         }
         
         if ( !g->attr.arrayptr )
           g->attr.arrayptr = (struct array*)mycalloc(1,sizeof(struct array));
         g->attr.arrayptr->datatype = g->type = datatype;
         g->attr.arrayptr->dim = dim;
         
         g->flags |= ARRAY_PARAM;
         g->flags &= ~ORDINARY_PARAM;
  
         /* see if fixed-zized local array, so we can allocate stack space */
         if ( g->flags & GLOB_LOCALVAR && !(g->flags & UNFIXED_SIZE_ARRAY) )
         { /* test previous nodes for fixed sizes */
           int cantflag = 0; /* set if not fixed sizes */
           int indexset_node = right;
           int rexpr_node=0;
           int i;
           for ( i = dim-1 ; i >= 0 ; i-- )
           { rexpr_node = indexset_node + 
               (i ? list[indexset_node].right : list[indexset_node].left);
             if ( list[rexpr_node].type != PUSHCONST )
             { cantflag = 1;
               break;
             }
             indexset_node += list[indexset_node].left;
           }

           if ( cantflag == 0 )
           { int datacount = 1; 
             int pointercount = 1; 
             g->flags |= FIXED_SIZE_ARRAY;
             g->attr.arrayptr = (struct array*)mycalloc(1,sizeof(struct array));
             g->attr.arrayptr->datatype = g->type = datatype;
             g->attr.arrayptr->dim = dim;
             /* gather sizes */
             indexset_node = right;
             for ( i = dim-1 ; i >= 0 ; i-- )
             { int size;
               rexpr_node = indexset_node + 
                (i ? list[indexset_node].right : list[indexset_node].left);
               size = (int)list[rexpr_node].op1.real;
               g->attr.arrayptr->sizes[i] = size;
               datacount *= size;
               if ( i < dim-1 ) pointercount *= size;
               indexset_node += list[indexset_node].left;
             }
             /* pop index nodes off execution list */
             listtop = rexpr_node;
             memset(list+listtop,0,sizeof(struct treenode));
 
             g->attr.arrayptr->itemsize = datatype_size[datatype];
             g->attr.arrayptr->datacount = datacount;
              
             i = add_local_var(NULL,datacount+pointercount);
             datastart = get_local(i).offset;
             g = globals(left); /* kludge since add_local_var can move things */
             g->attr.arrayptr->datastart = datastart;

             list[listtop].type = DEFINE_FIXED_LOCAL_ARRAY_;
             list[listtop].op1.name_id = left; /* global variable number */
             list[listtop].op2.valtype = datatype; /* type */

             break;
           }
           else g->flags |= UNFIXED_SIZE_ARRAY;
         }
         
         list[listtop].type = type;
         list[listtop].left = right-listtop; /* index expression list */
         list[listtop].op1.name_id = left; /* global variable number */
         list[listtop].op2.valtype = datatype;
         g->flags |= ARRAY_PARAM;
         g->flags &= ~ORDINARY_PARAM;
         if ( !(g->flags & GLOB_LOCALVAR) && g->attr.arrayptr &&
                   g->type != datatype )
         { sprintf(errmsg,
             "Array %s type declaration conflicts with previous type.\n",
                 g->name);
           kb_error(2224,errmsg,COMMAND_ERROR);
         }
         g->type = datatype;
                
         if ( (g->attr.arrayptr->dim > 0) &&
                        (g->attr.arrayptr->dim != list[right].op1.indexcount) )
         { sprintf(errmsg,"Cannot change the number of dimensions of array %s.\n",
                 g->name);
           kb_error(2225,errmsg,COMMAND_ERROR);
         }
         else g->attr.arrayptr->dim = list[right].op1.indexcount;
         list[listtop].flags |= EPHEMERAL;
         list[listtop].stack_delta = -list[right].op1.indexcount;
         break;
       }

    case ARRAY_HEAD_:
         list[listtop].type = type;
         list[listtop].op1.name_id = right; /* global variable number */
         list[listtop].left = left-listtop;  /* indexset */
         list[listtop].op2.indexcount = list[left].op1.indexcount; /* # indices */
/*
         if ( list[left].op1.indexcount != globals(right)->dim ) 
         { sprintf(errmsg,
              "Array %s has wrong number of dimensions; should be %d\n",
                 globals(int_val)->name,globals(int_val)->dim);
           kb_error(3360,errmsg,COMMAND_ERROR);
         }
*/
         list[listtop].flags |= EPHEMERAL;
         break;

    /* whole-array syntax */

    case ARRAYIDENT_:  /* push datastart for an array */
         list[listtop].type = type;
         list[listtop].op2.name_id = left;
         list[listtop].stack_delta = 1;
         break;

    case ATTRIB_LVALUE_ :
         list[listtop].type = type;
         if ( left )
           list[listtop].left = left - listtop;
         list[listtop].stack_delta = 1;
         break;

    case ARRAY_LVALUE_INDEXED_: /* nop at execution */
       { struct array *a = get_name_arrayptr(list[left].op2.name_id,NULL,localbase);
         if ( a->dim != list[right].op1.indexcount ) 
         { sprintf(errmsg,"Array %s needs %d indexes, has %d.\n",
             get_name_name(list[left].op2.name_id,localbase),a->dim,list[right].op1.indexcount);
           kb_error(3267,errmsg,Q_ERROR);
         }
         check_readonly_attr(list[left].op2.name_id);
         list[listtop].type = type;
         list[listtop].left = left-listtop;
         list[listtop].right = right-listtop;
         list[listtop].op1.indexcount = list[right].op1.indexcount;
         list[listtop].op2.name_id = list[left].op2.name_id;
         break;
      }

    case ARRAY_EVAL_: /* rexpr: arraylvalue indexset */
       { struct array *a = get_name_arrayptr(list[left].op2.name_id,NULL,localbase);
         if ( a->dim != list[right].op1.indexcount ) 
         { sprintf(errmsg,"Array %s needs %d indexes, has only %d.\n",
             get_name_name(list[left].op2.name_id,localbase),a->dim,list[right].op1.indexcount);
           kb_error(3268,errmsg,Q_ERROR);
         }
         list[listtop].type = type;
         list[listtop].left = left-listtop;
         list[listtop].right = right-listtop;
         list[listtop].op2.name_id = list[left].op2.name_id;
         list[listtop].datatype = REAL_TYPE;
         if ( (list[left].type == ARRAY_VERTEX_NORMAL_) ||
                 (list[left].type == ARRAY_EDGE_VECTOR_) ||
                 (list[left].type == ARRAY_FACET_NORMAL_) )
            list[listtop].flags |= IS_VIRTUAL_ATTR;
         list[listtop].stack_delta = list[right].op1.indexcount;
         break;
       }

    case ARRAY_ASSIGNOP_SINGLE_:
         check_readonly_attr(list[left].op2.name_id);
         list[listtop].type = type;
         list[listtop].left = left-listtop;
         list[listtop].right = right-listtop;
         if ( check_recalc_attr(list[left].op2.name_id) )
           list[listtop].flags |= RECALC_NODE;
         /* list[listtop].stack_delta done in command.yac */
         break;


    case DOT_:  /* dot product */
         { 
           int name1 = list[left].op2.name_id;
           int name2 = list[right].op2.name_id;
           int adim = get_name_dim(name1,localbase);
           int bdim = get_name_dim(name2,localbase);
           if ( (adim != 1) || (bdim != 1) )
              kb_error(3329,"Dot product operands must be one dimensional.\n",
                 COMMAND_ERROR);
           if ( (get_name_datatype(name1,localbase) != REAL_TYPE) || 
                    (get_name_datatype(name2,localbase) != REAL_TYPE) )  
              kb_error(3330,"Dot product operands must be of type real.\n",
                 COMMAND_ERROR);
           list[listtop].type = type;
           list[listtop].left = left-listtop;
           list[listtop].right = right-listtop;
           list[listtop].datatype = REAL_TYPE;
           list[listtop].op2.name_id = name1;
           list[listtop].op3.name_id = name2;
           list[listtop].stack_delta = 1;
         }
         break;



    case ARRAY_ASSIGNOP_ARRAY_: /* full array syntax */
         /* see if we have special attributes on right side */
         { int name_id = list[right].op2.name_id;
           check_readonly_attr(list[left].op2.name_id);
 
           if ( name_id == set_name_eltype(V_NORMAL_ATTR,VERTEX) )
             list[listtop].type = ARRAY_ASSIGNOP_VERTEX_NORMAL_;
           else if ( name_id == set_name_eltype(E_VECTOR_ATTR,EDGE) )
             list[listtop].type = ARRAY_ASSIGNOP_EDGE_VECTOR_;
           else if ( name_id == set_name_eltype(F_NORMAL_ATTR,FACET) )
             list[listtop].type = ARRAY_ASSIGNOP_FACET_NORMAL_;
           else
             list[listtop].type = type;

           list[listtop].left = left-listtop;  /* lvalue */
           list[listtop].right = right-listtop;  /* rvalue */
           list[listtop].op2.name_id = list[left].op2.name_id;
           list[listtop].op3.name_id = list[right].op2.name_id;
           if ( check_recalc_attr(list[left].op2.name_id) )
             list[listtop].flags |= RECALC_NODE;
           /* op1.assigntype will be set back in command.yac */
           /* Can do dimension check now */
           if ( check_array_dims_same(list[left].op2.name_id,
                           list[right].op2.name_id) == 0 )
            kb_error(4378,"Arrays don't have same number of dimensions or types different.\n",
               COMMAND_ERROR);
           list[listtop].stack_delta = -2;
         }
         break;

    case ARRAY_ASSIGNOP_SCALAR_: /* full array syntax */
         check_readonly_attr(list[left].op2.name_id);
         list[listtop].type = type;
         list[listtop].op2.name_id = list[left].op2.name_id;
         list[listtop].left = left-listtop;  /* lvalue array */
         list[listtop].right = right-listtop;  /* rexpr */
         if ( check_recalc_attr(list[left].op2.name_id) )
           list[listtop].flags |= RECALC_NODE;
         list[listtop].stack_delta = -2;
         /* op1.assigntype will be set back in command.yac */
         break;
 
    case ARRAY_RVALUE_: /* nop needed for tree construction */
         list[listtop].type = type;
         list[listtop].left = left-listtop;  /* lvalue array */
         list[listtop].right = right-listtop;  /* rexpr */
         break;

    case ARRAY_ASSIGNOP_S_X_A_: /* full array syntax, scalar times array */
         check_readonly_attr(list[left].op2.name_id);
         list[listtop].type = type;
         list[listtop].op2.name_id = list[left].op2.name_id;
         list[listtop].left = left-listtop;  /* lvalue array */
         list[listtop].right = right-listtop;  /* rvalue */
         if ( check_recalc_attr(list[left].op2.name_id) )
           list[listtop].flags |= RECALC_NODE;
         /* op1.assigntype will be set back in command.yac */
         list[listtop].stack_delta = -3;
         break;

    case ARRAY_ASSIGNOP_A_P_A_: /* full array syntax, array plus array */
         check_readonly_attr(list[left].op2.name_id);
         list[listtop].type = type;
         list[listtop].left = left-listtop;  /* lvalue array */
         list[listtop].right = right-listtop;  /* rvalue */
         list[listtop].op2.name_id = list[left].op2.name_id;
         if ( check_recalc_attr(list[left].op2.name_id) )
           list[listtop].flags |= RECALC_NODE;
         /* op1.assigntype will be set back in command.yac */
         list[listtop].stack_delta = -3;
         break;

    case ARRAY_ASSIGNOP_A_S_A_: /* full array syntax, array subtract array */
         check_readonly_attr(list[left].op2.name_id);
         list[listtop].type = type;
         list[listtop].left = left-listtop;  /* lvalue array */
         list[listtop].right = right-listtop;  /* rvalue */
         list[listtop].op2.name_id = list[left].op2.name_id;
         if ( check_recalc_attr(list[left].op2.name_id) )
           list[listtop].flags |= RECALC_NODE;
         /* op1.assigntype will be set back in command.yac */
         list[listtop].stack_delta = -3;
         break;

   /* end whole-array syntax */

    case ARRAYASSIGN:
         list[listtop].type = type;
         list[listtop].left = left-listtop;  /* arrayhead */
         list[listtop].right = right-listtop;  /* rhs */
         list[listtop].op1.assigntype = assigntype;
         list[listtop].op2.name_id = list[left].op1.name_id; /* glob var number */

         g =  globals(list[left].op1.name_id);
         if ( list[left].op2.indexcount != g->attr.arrayptr->dim ) 
         { sprintf(errmsg,
              "Array %s has wrong number of dimensions; should be %d\n",
                 g->name,g->attr.arrayptr->dim);
           kb_error(2226,errmsg,COMMAND_ERROR);
         } 
         if ( g->flags & READONLY )
         { sprintf(errmsg, "Array %s is read-only.\n",g->name);
           kb_error(2846,errmsg,COMMAND_ERROR);
         }
         list[listtop].stack_delta = -list[left].op2.indexcount-1;
         if ( !(g->flags & PERMANENT) )
           list[listtop].flags |= EPHEMERAL;

         break;


    case ARRAYEVAL:
       {
         g = globals(list[left].op1.name_id);

         list[listtop].type = type;
         list[listtop].left = left-listtop;
         list[listtop].op2.name_id = list[left].op1.name_id;
         list[listtop].flags |= EPHEMERAL;
         if ( list[left].op2.indexcount != g->attr.arrayptr->dim ) 
         { sprintf(errmsg,
              "Array %s has wrong number of dimensions; should be %d\n",
                 g->name, g->attr.arrayptr->dim );
           kb_error(2608,errmsg,COMMAND_ERROR);
         }
         list[listtop].stack_delta = -list[left].op2.indexcount+1;
         list[listtop].datatype = (g->type <= MAX_NUMERIC_TYPE) ? REAL_TYPE:
           g->type;
         break;
       }

    case PDELTA_LVALUE_: case PSCALE_LVALUE_:
         list[listtop].type = type;
         list[listtop].op1.name_id = left; /* variable number */
         break;

    case SET_DELTA_: case SET_PARAM_SCALE:
         list[listtop].type = type;
         list[listtop].op1.name_id = left; /* variable number */
         list[listtop].left = right - listtop;  /* value expression */
         list[listtop].op2.assigntype = assigntype;
         if ( !(globals(left)->flags & ANY_TYPE)  )
            globals(left)->flags |= ORDINARY_PARAM;
         if ( perm_flag ) globals(left)->flags |= PERMANENT;
         list[listtop].flags |= EPHEMERAL;
         list[listtop].stack_delta = -1;
         break;

    case SET_ELEMENT_GLOBAL_:
       { struct global *g = globals(left);
         int eltype = -1;  /* of right side */

         list[listtop].type = type;
         list[listtop].op1.name_id = left; /* variable number */
         list[listtop].left = right - listtop;  /* value expression */
         if ( !(g->flags & ANY_TYPE)  )
            g->flags |= ORDINARY_PARAM;
         if ( (list[listtop].datatype < VERTEX_TYPE)
                || (list[listtop].datatype > FACETEDGE_TYPE) )
            kb_error(2884,"Right side expression must be an element.\n",
                  COMMAND_ERROR);
         switch ( list[right].op1.eltype )
         { case VERTEX:
            eltype = VERTEX_TYPE; break;
           case EDGE:
            eltype = EDGE_TYPE; break;
           case FACET:
            eltype = FACET_TYPE; break;
           case BODY:
            eltype = BODY_TYPE; break;
           case FACETEDGE:
            eltype = FACETEDGE_TYPE; break;
           default:
             kb_error(3684,"Illegal element type.\n",Q_ERROR);
         }
         if ( g->type && (g->type != eltype) )
         { sprintf(errmsg,
            "Cannot assign expression of type %s to variable %s of type %s.\n",
             datatype_name[eltype],globals(left)->name,datatype_name[g->type]);
           kb_error(3686,errmsg,Q_ERROR);
         }
         else g->type = eltype;
         if ( perm_flag ) g->flags |= PERMANENT;
         list[listtop].flags |= EPHEMERAL;
         list[listtop].stack_delta = -1;
         break;
       }

    case SET_GLOBAL_: 
    case PLUSASSIGN_: case SUBASSIGN_: case MULTASSIGN_: case DIVASSIGN_:
      {  
         struct global *g = globals(left);

         list[listtop].type = type;
         list[listtop].op1.name_id = left; /* variable number */
         list[listtop].left = right - listtop;  /* value expression */
         if ( !(g->flags & ANY_TYPE)  )
            g->flags |= ORDINARY_PARAM;
         if ( !(g->flags & (ORDINARY_PARAM|GLOB_LOCALVAR)) )
         { sprintf(errmsg, "Variable %s is not proper type for left side of assighmant.\n",g->name);
           kb_error(3021,errmsg,Q_ERROR);
         }
         if ( perm_flag ) g->flags |= PERMANENT;
         list[listtop].flags |= EPHEMERAL;
         list[listtop].stack_delta = -1;
         g->type = list[right].datatype;
         break;
      }

    case SET_PERM_GLOBAL_: 
         list[listtop].type = type;
         list[listtop].op1.name_id = left; /* variable number */
         list[listtop].left = right - listtop;  /* value expression */
         perm_globals(left)->flags |= ORDINARY_PARAM;
         perm_globals(left)->attr.varstuff.delta = OPTPARAM_DELTA;
         perm_globals(left)->attr.varstuff.pscale = 1.0;
         list[listtop].stack_delta = -1;
         break;

    case SET_SGLOBAL_:
    case SET_PERM_SGLOBAL_:
         list[listtop].type = type;
         list[listtop].op1.name_id = left; /* variable number */
         list[listtop].left = right - listtop;  /* value expression */
         globals(left)->flags |= STRINGVAL;
         globals(left)->flags &= ~ORDINARY_PARAM;
         if ( type == SET_SGLOBAL_ ) list[listtop].flags |= EPHEMERAL;
         if ( perm_flag ) globals(left)->flags |= PERMANENT;
         list[listtop].stack_delta = -1;
         break;

    case SHOW_:
    case SHOW_EXPR_:
         list[listtop].type = type;
         /* put node in front of expression */
         right = listtop;
         subtree_swap(&left,&right);
         list[right].op1.skipsize = left - right; 
         list[listtop].line_no = line_no;
         list[listtop].file_no = file_no;
         listtop++;
         list[listtop].type = SHOW_END_;
         list[listtop].left = right - listtop; /* action node */
         list[listtop].right = left - listtop; /* procedure */
         break;

    case SELF_ELEMENT_:
         list[listtop].type = type;
         list[listtop].op1.eltype = left; 
         list[listtop].op2.localnum = elsym->localnum; 
         break;

    case SINGLE_ELEMENT_EXPR_:
         list[listtop].type = type;
         list[listtop].left = left - listtop; /* action node */
         list[listtop].op1.eltype = right; 
         list[listtop].stack_delta = 1;
         list[listtop].datatype = VERTEX_TYPE + right;
         break;

    case ELEMENT_IDENT_:
         list[listtop].type = type;
         list[listtop].op3.name_id = left;
         list[listtop].op2.localnum = add_local_var(NULL,1);
         break;

    case SYMBOL_ELEMENT_:
         list[listtop].type = type;
         list[listtop].op1.eltype = symtable[left].type; 
         list[listtop].op2.localnum = symtable[left].localnum; 
         list[listtop].op5.string =
           (char*)mycalloc(strlen(symtable[left].name)+1,1);
         list[listtop].flags |= HAS_STRING_5;
         strcpy(list[listtop].op5.string,symtable[left].name);
         break;

    case SINGLE_ELEMENT_: /* generator */
         list[listtop].type = SINGLE_ELEMENT_INIT_; /* for dummy loop */
         list[listtop].line_no = line_no;
         list[listtop].file_no = file_no;
         list[listtop].stack_delta = 3;
         listtop++;
         list[listtop].type = type;
         list[listtop].left = left-listtop;  /* index value */
         list[listtop].right = -1; /* to INIT */
         list[listtop].op2.localnum = add_local_var(NULL,1);
                    /* local will hold id at runtime */
         /* op1 will hold jump to end of loop */
         break;

    case INDEXED_ELEMENT_: 
         list[listtop].type = type;
         list[listtop].left = right-listtop;  /* index value */
         list[listtop].op1.eltype = left;    /* element type */
         list[listtop].stack_delta = -1;
         list[listtop].op2.localnum = add_local_var(NULL,1);
                    /* local will hold id at runtime */
         /* Sanity check on index */
         if ( list[right].type == PUSHCONST )
         { if ( list[right].op1.real == 0.0 ) 
           kb_error(2228,"Element index must be nonzero.\n",COMMAND_ERROR);
         }
         break;

    case INDEXED_SUBTYPE_: 
         list[listtop].type = type;
         list[listtop].left = left-listtop;  /* single element */
         list[listtop].right = right-listtop;  /* index value */
         list[listtop].op1.eltype = subtype;    /* element type */
         list[listtop].stack_delta = -1;
         list[listtop].op2.localnum = add_local_var(NULL,1);
                    /* local will hold id at runtime */
         /* Sanity check on index */
         if ( list[right].type == PUSHCONST )
         { if ( list[right].op1.real <= 0.0 ) 
           kb_error(2229,"Element index must be positive.\n",COMMAND_ERROR);
         }
         /* some type checking */
         etype = list[left].op1.eltype;
         if ( etype == subtype )
           kb_error(1418,"Cannot do same subtype as type of element.\n",
            COMMAND_ERROR);
         switch ( etype )
         { case VERTEX:
             switch ( subtype /* subtype */ )
             { case BODY: case FACETEDGE: 
                  kb_error(1419,"Unimplemented subtype of vertex.\n",COMMAND_ERROR);
             }
             break;
           case EDGE:
             switch ( subtype /* subtype */ )
             { case BODY:
                  kb_error(1420,"Unimplemented subtype of edge.\n",COMMAND_ERROR);
             }
             break;
           case FACET:
             switch ( subtype /* subtype */ )
             { case FACETEDGE: 
                  kb_error(1421,"Unimplemented subtype of facet.\n",COMMAND_ERROR);
             }
             break;
           case BODY:
             switch ( subtype /* subtype */ )
             { case FACETEDGE: 
               case VERTEX:
               case EDGE:
                  kb_error(1422,"Unimplemented subtype of body.\n",COMMAND_ERROR);
             }
             break;
           case FACETEDGE:
             switch ( subtype /* subtype */ )
             { case FACETEDGE: 
               case VERTEX:
               case BODY:
                  kb_error(1423,"Unimplemented subtype of facet.\n",COMMAND_ERROR);
             }
             break;
         }
         break;

    case SHOW_VOL_:  case CHECK_: case LONG_JIGGLE_: case RAW_VERAVG_:
    case STABILITY_TEST_: case UTEST_: case GO_: case SHELL_: 
    case ALICE_:      case TOPINFO_: case RECALC_: case COUNTS_:
    case EXTRAPOLATE_: case HESSIAN_: case SOBOLEV_: 
    case SHOWQ_:case RAWEST_VERAVG_: case DIRICHLET_: case BOTTOMINFO_:
    case CLOSE_SHOW_: case REBODY_: case LIST_PROCS_: case HESSIAN_MENU_:
    case DIRICHLET_SEEK_: case SOBOLEV_SEEK_: case CONVERT_TO_QUANTS_:
    case LIST_ATTRIBUTES_: case REORDER_STORAGE_: case RENUMBER_ALL_:
    case DUMP_MEMLIST_: case FREE_DISCARDS_: case REPARTITION_:
    case SUBCOMMAND_: case ABORT_: case SIMPLEX_TO_FE_:
         list[listtop].type = type;
         break;

    case LIST_BOUNDARY_: case LIST_CONSTRAINT_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].stack_delta = -1;
         break;

    case LIST_QUANTITY_: 
         list[listtop].type = type;
         list[listtop].op1.quant_id = left;
         break;
 
    case LIST_METHOD_INSTANCE_: 
         list[listtop].type = type;
         list[listtop].op1.meth_id = left;
         break;
 

    /* toggles */ case LINEAR_: case QUADRATIC_: case QUIETGO_:
    case KUSNER_: case ESTIMATE_: case DETURCK_: case HOMOTHETY_:
    case SQGAUSS_: case AUTOPOP_: case AUTOCHOP_: case QUIET_:
    case OLD_AREA_: case APPROX_CURV_: case RUNGE_KUTTA_: 
    case CHECK_INCREASE_:  case DEBUG_: case MEAN_CURV_: case RIBIERE_CG_:
    case DIFFUSION_: case GRAVITY_: case CONJ_GRAD_: case TRANSFORMS_:
    case CONF_EDGE_SQCURV_: case EFFECTIVE_AREA_: case AREA_FIXED_:
    case RAW_CELLS_: case CONNECTED_CELLS_: case CLIPPED_CELLS_:
    case THICKEN_: case SHOW_INNER_: case SHOW_OUTER_: case COLORMAP_: 
    case HESSIAN_DIFF_: case POST_PROJECT_: case MEAN_CURV_INT_:
    case OPTIMIZE_: case NORMAL_CURVATURE_: case DIV_NORMAL_CURVATURE_:
    case SHADING_:  case FACET_COLORS_: case BOUNDARY_CURVATURE_:
    case NORMAL_MOTION_: case PINNING_: case VIEW_4D_: case MEMDEBUG_:
    case METRIC_CONVERSION_: case AUTORECALC_: case GV_BINARY_:
    case SELF_SIMILAR_: case AUTODISPLAY_: case FORCE_POS_DEF_:
    case ASSUME_ORIENTED_: case HESSIAN_QUIET_: case JIGGLE_TOGGLE_:
    case HESSIAN_NORMAL_: case YSMP_: case BUNCH_KAUFMAN_:
    case QUANTITIES_ONLY_: case LINEAR_METRIC_: case GEOMVIEW_TOGGLE_:
    case GEOMPIPE_TOGGLE_: case SQUARED_GRADIENT_: case H_INVERSE_METRIC_:
    case HESSIAN_DOUBLE_NORMAL_: case INTERP_BDRY_PARAM_: case LOGFILE_TOGGLE_:
    case HESSIAN_NORMAL_ONE_: case PSCOLORFLAG_: case GRIDFLAG_:
    case CROSSINGFLAG_: case LABELFLAG_: case SHOW_ALL_QUANTITIES_:
    case HESSIAN_NORMAL_PERP_: case HESSIAN_SPECIAL_NORMAL_: case ITDEBUG_:
    case METIS_FACTOR_: case VOLGRADS_EVERY_: case ZENER_DRAG_: 
    case BACKCULL_: case INTERP_NORMALS_: case TORUS_FILLED_: case VERBOSE_:
    case AMBIENT_PRESSURE_: case DIRICHLET_MODE_: case SOBOLEV_MODE_:
    case KRAYNIKPOPVERTEX_FLAG_: case KEYLOGFILE_TOGGLE_:
    case KRAYNIKPOPEDGE_FLAG_: case VISIBILITY_TEST_: case SPARSE_CONSTRAINTS_:
    case BLAS_FLAG_: case AUGMENTED_HESSIAN_: case BREAK_AFTER_WARNING_:
    case RGB_COLORS_FLAG_:  case CIRCULAR_ARC_DRAW_: case BEZIER_BASIS_:
    case SMOOTH_GRAPH_: case MPI_DEBUG_: case POP_DISJOIN_:
    case POP_TO_EDGE_: case POP_TO_FACE_: case POP_ENJOIN_:
    case FULL_BOUNDING_BOX_: case BIG_ENDIAN_: case LITTLE_ENDIAN_:
    case QUIETLOAD_: case SLICE_VIEW_: case FUNCTION_QUANTITY_SPARSE_:
    case CLIP_VIEW_: case STAR_FINAGLING_: case FORCE_DELETION_:
    case AUTOPOP_QUARTIC_: case IMMEDIATE_AUTOPOP_:
         list[listtop].type = type;
         list[listtop].op1.toggle_state = left; /* toggle state */
         break;
     
    case SYSTEM_: 
    case READ_:
    case EXEC_: case PARALLEL_EXEC_:
    case CHDIR_:
    case SHOW_TRANS_:
    case SET_COLORMAP_:
    case TRANSFORM_EXPR_:  case KEYLOGFILE_: case OOGLFILE_:
    case BINARY_OFF_FILE_:
    case GEOMVIEW_: case GEOMPIPE_: case POSTSCRIPT_: case LOGFILE_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].stack_delta = -1;
         break;

    case TASK_EXEC_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].right = right - listtop;
         list[listtop].stack_delta = -2;
         break;

    case TEXT_SPOT_: /* just accumulate two arguments */
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].right = right - listtop;
         break;

    case DISPLAY_TEXT_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].right = right - listtop;
         list[listtop].stack_delta = -2;  /* pops 3, leaves string id */
         break;

    case DELETE_TEXT_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].stack_delta = -1; 
         break;


    case PRINTFHEAD_:
    case BINARY_PRINTFHEAD_:
    case ERRPRINTFHEAD_:
    case SPRINTFHEAD_:
         list[listtop].type = type;
         /* copy and convert escape sequences */
         if ( left ) 
         { list[listtop].left = left - listtop;
           list[listtop].stack_delta = 0; /* since pushes result string */
         }
         else 
         {
           char *c, *s;
           list[listtop].op1.string = 
              (char*)mycalloc(strlen(yytext)+1,sizeof(char));
           list[listtop].flags |= HAS_STRING;
           s = yytext; /* source */
           c = list[listtop].op1.string; /* destination */
           while ( *s )
             if ( *s == '\\' )
             { switch ( s[1] )
               { case 'n' : *(c++) = '\n'; s += 2; break;
                 case 't' : *(c++) = '\t'; s += 2; break;
                 case 'b' : *(c++) = '\b'; s += 2; break;
                 case 'r' : *(c++) = '\r'; s += 2; break;
                 default:  *(c++) =  s[1]; s += 2; break;
               }
             }
             else *(c++) = *(s++);
           *c = '\0';
         }
         break;

    case SPRINTF_: /* with expressions */
         list[listtop].type = type;
         list[listtop].left = left - listtop; /* has string */
         list[listtop].right = right - listtop; /* expressions */
         list[left].type = PRESPRINTF_;
         list[left].stack_delta = 0;
         list[listtop].stack_delta = -list[right].op1.argcount;
         list[listtop].datatype = STRING_TYPE;
         list[listtop].flags |= DEALLOCATE_POINTER; 
         break;

    case PRINTF_: /* with expressions */
    case BINARY_PRINTF_: /* with expressions */
    case ERRPRINTF_:
         list[listtop].type = type;
         list[listtop].left = left - listtop; /* has string */
         list[listtop].right = right - listtop; /* expressions */
         list[left].type = PREPRINTF_;
         list[left].stack_delta = 0;
         list[listtop].stack_delta = -list[right].op1.argcount-1;
         break;

    case EXPRLIST_:
         list[listtop].type = type;
         list[listtop].left = left - listtop; /*  expr */ 
         if ( right )
         { list[listtop].right = right - listtop; /* exprlist */
           list[listtop].op1.argcount = list[right].op1.argcount+1;
         }
         else list[listtop].op1.argcount = 1; /* arg count */
         break;

    case DATAFILENAME_:
    case WARNING_MESSAGES_:
    case GET_TRANSFORM_EXPR_:
         list[listtop].type = type;
         list[listtop].stack_delta = 1;
         list[listtop].datatype = STRING_TYPE;
         break;

    case HELP_KEYWORD:
         list[listtop].type = type;
         list[listtop].op1.string = 
            (char*)mycalloc(strlen(yytext)+1,sizeof(char));
         list[listtop].flags |= HAS_STRING;
         strcpy(list[listtop].op1.string,yytext);
         list[listtop].datatype = STRING_TYPE;
         break;

    default:
      more_makenode(type,left,right);
      break;
    }

  if ( listtop > listmax-2 )
        kb_error(1336,"Expression too long",COMMAND_ERROR);


  list[listtop].line_no = line_no;
  list[listtop].file_no = file_no;
  return listtop++;

}


/*********************************************************************
*
*  Function:  subtree_swap()
*
*  Purpose:    Swap two adjacent subtrees at the end of the list
*              so they are in proper order for sequential execution.
*
*  Return:     Adjust index values in arguments.
*/

void subtree_swap ARGS2((left,right),
int *left,  /* index of left subtree */
int *right) /* index of right subtree */
{ int n;
  struct treenode *temp;

  if ( *left && *right && (*left < *right ) )
  { int leftstart = *left, rightstart = *right;
    int leftsize,rightsize;
    for (;;)
    { if ( list[leftstart].left && list[leftstart].right )
      { if ( list[leftstart].left < list[leftstart].right )
          leftstart += list[leftstart].left;
        else leftstart += list[leftstart].right;
      }
      else if ( list[leftstart].left )
        leftstart += list[leftstart].left;
      else if ( list[leftstart].right )
        leftstart += list[leftstart].right;
      else break;
    }
    for (;;)
    { if ( list[rightstart].left && list[rightstart].right )
      { if ( list[rightstart].left < list[rightstart].right )
          rightstart += list[rightstart].left;
        else rightstart += list[rightstart].right;
      }
      else if ( list[rightstart].left )
        rightstart += list[rightstart].left;
      else if ( list[rightstart].right )
        rightstart += list[rightstart].right;
      else break;
    }
    if ( rightstart != *left + 1 )
     { sprintf(errmsg,
          "Internal error: Left: %d-%d  right: %d-%d subtrees not adjacent.\n",
          leftstart,*left,rightstart,*right);
        kb_error(1387,errmsg,COMMAND_ERROR);
     }      

    /* swap subtrees */
    leftsize = *left - leftstart + 1;
    rightsize = *right - rightstart +1;
    temp = (struct treenode*)temp_calloc(leftsize,sizeof(struct treenode));
    memcpy((char*)(temp),(char*)(list+leftstart),
        leftsize*sizeof(struct treenode));
    kb_memmove((char*)(list+leftstart),(char*)(list+rightstart),
        rightsize*sizeof(struct treenode));
    memcpy((char*)(list+leftstart+rightsize),(char*)(temp),
        leftsize*sizeof(struct treenode));
    temp_free((char*)temp);

    /* adjust subtree pointers */
    *left += rightsize;
    *right -= leftsize;
    /* kludge to adjust pointers to local variables */
    for ( n = leftstart ; n < leftstart+rightsize ; n++ )
    { 
      if ( ((list[n].type == BREAK_) || (list[n].type == CONTINUE_)) &&
         (n+leftsize+list[n].op1.skipsize < leftstart)  ) 
            list[n].op1.skipsize += leftsize;
    }
    for ( n = leftstart+rightsize ; n < leftstart+rightsize+leftsize ; n++ )
    { 
      if ( ((list[n].type == BREAK_) || (list[n].type == CONTINUE_)) &&
         (n-rightsize+list[n].op1.skipsize < leftstart)  ) 
            list[n].op1.skipsize -= rightsize;
    } 
  }
}    

/**********************************************************************
*
* Function: more_makenode()
*
* Purpose: Overflow from makenode(), to keep code under 32K for Mac
*/

void more_makenode ARGS3((type,left,right),
NTYPE type, NTYPE left, NTYPE right)
{ int n,etype;

  switch (type)
  {
    case QUOTATION_:
         list[listtop].type = type;
         list[listtop].op1.string = 
            (char*)mycalloc(strlen(yytext)+1,sizeof(char));
         list[listtop].flags |= HAS_STRING;
         strcpy(list[listtop].op1.string,yytext);
         list[listtop].stack_delta = 1;
         list[listtop].datatype = STRING_TYPE;
         break;

    case DUMP_:
    case LOAD_:
    case PERMLOAD_:
    case ADDLOAD_:
         list[listtop].type = type;
         if ( left ) 
         { list[listtop].left = left - listtop;
           list[listtop].stack_delta = -1;
         }
         break;

    case SINGLE_LETTER_:
         list[listtop].type = SINGLE_LETTER_;
         list[listtop].op1.letter = left;
         break;

    case SINGLE_REDEFD_:
         list[listtop].type = SINGLE_REDEFD_;
         list[listtop].op1.letter = left;
         break;

    case CMDLIST_:  
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         if ( right ) list[listtop].right = right - listtop;
         break;

    case COMMAND_BLOCK_:  
         list[listtop].type = COMMAND_BLOCK_;
         list[listtop].left = left - listtop;
         break;

    case REPEAT_INIT_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].stack_delta = +1;
         break;

    case REPEAT_:
         list[listtop].type = REPEAT_;
         list[listtop].left = left - listtop;
         list[listtop].right = right - listtop;
         list[left].op1.skipsize = right+1-left; /* for skipping body */
         list[listtop].stack_delta = -2;
         break;

    case EPRINT_:
         list[listtop].type = type;
         break;

      case COND_TEST_:
         list[listtop].type = COND_TEST_;
         list[listtop].left = left - listtop;
         /* inx will be target for false jmp */
         list[listtop].stack_delta = -1;
         break;

      case COND_EXPR_:
         list[listtop].type = COND_EXPR_;
         list[listtop].left = left - listtop;
         list[listtop].right = right - listtop;
         list[left].op1.skipsize = listtop - left; /* to skip TRUE part */
         list[listtop].stack_delta = -1;  /* since one expression skipped */
         list[listtop].datatype = list[right].datatype;
         break;

      case COND_ELSE_: /* really the continue node at root of IF */
         list[listtop].type = COND_ELSE_;
         list[listtop].left = left - listtop;
         list[listtop].right = right - listtop;
         list[left].op1.skipsize = listtop - left; /* to skip ELSE part */
         if ( list[left].datatype != list[right].datatype )
            kb_error(3710,"Datatypes disagree in conditional expression.\n",
               COMMAND_ERROR);
         list[listtop].datatype = list[right].datatype;
         break;
      
    case SIZEOF_ATTR_: /* current attribute dimension */
         list[listtop].type = SIZEOF_ATTR_;
         list[listtop].op1.extranum = left;  /* attr num */
         list[listtop].op2.eltype = right; /* etype */
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case SIZEOF_ARRAY_: /* current array total size */
         list[listtop].type = SIZEOF_ARRAY_;
         list[listtop].op1.name_id = left;  /* array id */
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case SIZEOF_STRING_: /* string length */
         list[listtop].type = SIZEOF_STRING_;
         list[listtop].left = left-listtop;  /* string expr */
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;


    case DEFINE_CONSTRAINT_:
       { int old_verb_flag;
         list[listtop].type = type;
         /* Parse-time evaluation and construction of quantity */
	 datafile_flag = PRETEND_DATAFILE; 
         old_verb_flag = verb_flag; verb_flag = 0; 
         list[listtop].op1.con_id = read_constraint(); 
	 datafile_flag = NOT_DATAFILE;
         verb_flag = old_verb_flag;
         if ( tok == ';' ) /* in case reentrant parser ate ';' */
            kb_unput(';');
       }
         break;

    case DEFINE_BOUNDARY_:
       { int old_verb_flag;
         list[listtop].type = type;
         /* Parse-time evaluation and construction of quantity */
		 datafile_flag = PRETEND_DATAFILE; old_verb_flag = verb_flag; verb_flag = 0; 
         list[listtop].op1.bdry_id = read_boundary(); 
		 datafile_flag = NOT_DATAFILE;
         verb_flag = old_verb_flag;
         if ( tok == ';' ) /* in case reentrant parser ate ';' */
            kb_unput(';');
       }
         break;

    case DEFINE_QUANTITY_:
       { int old_verb_flag;
         list[listtop].type = type;
         /* Parse-time evaluation and construction of quantity */
		 datafile_flag = 1; old_verb_flag = verb_flag; verb_flag = 0; 
         list[listtop].op1.quant_id = read_quantity(); 
		 datafile_flag = 0;
         verb_flag = old_verb_flag;
         if ( tok == ';' ) /* in case reentrant parser ate ';' */
            kb_unput(';');
       }
         break;

    case DEFINE_METHOD_INSTANCE_:
       { int old_verb_flag;
         list[listtop].type = type;
         /* Parse-time evaluation and construction of quantity */
		 datafile_flag = 1; old_verb_flag = verb_flag; verb_flag = 0; 
         list[listtop].op1.quant_id = read_method_instance(); 
		 datafile_flag = 0;
         verb_flag = old_verb_flag;
         if ( tok == ';' ) /* in case reentrant parser ate ';' */
            kb_unput(';');
       }
         break;

    case IS_DEFINED_:
         /* Parse-time evaluation. */
         list[listtop].type = type;
         list[listtop].left = left-listtop; 
         list[listtop].stack_delta = 0;
         list[listtop].datatype = REAL_TYPE;
         break;

    case PUSHCONST:
         list[listtop].type = PUSHCONST;
         list[listtop].op1.real = real_val;
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case PUSH_NAMED_QUANTITY:
         list[listtop].type = type;
         list[listtop].op1.quant_id = left;
         list[listtop].flags |= EPHEMERAL;
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case PUSH_METHOD_INSTANCE_:
         list[listtop].type = type;
         list[listtop].op1.meth_id = left;
         list[listtop].flags |= EPHEMERAL;
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case PUSHPI:
         list[listtop].type = type;
         list[listtop].op1.real = M_PI;
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case PUSHE:
         list[listtop].type = type;
         list[listtop].op1.real = M_E;
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case PUSHG:
         list[listtop].type = type;
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case DATE_AND_TIME_:
         list[listtop].type = type;
         list[listtop].stack_delta = 1;
         list[listtop].datatype = STRING_TYPE;
         break;

    case TOGGLEVALUE:
         list[listtop].type = type;
         list[listtop].op1.toggle_id = left; /* which toggle */
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case GET_TORUS_PERIODS_:
    case GET_INVERSE_PERIODS_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].right = right - listtop;
         list[listtop].stack_delta = -1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case EXP:
         if ( is_constant(left) )
         { /* fold constants */
           listtop--;
           list[listtop].type = PUSHCONST;
           list[listtop].op1.real = exp(list[listtop].op1.real);
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case LOG:
         if ( is_constant(left) )
         { /* fold constants */
           listtop--;
           list[listtop].type = PUSHCONST;
           if ( list[listtop].op1.real < 0.0 )
             kb_error(1425,"log of negative number.\n", COMMAND_ERROR );
           list[listtop].op1.real = log(list[listtop].op1.real);
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case ABS:
         if ( is_constant(left) )
         { /* fold constants */
           listtop--;
           list[listtop].type = PUSHCONST;
           list[listtop].op1.real = fabs(list[listtop].op1.real);
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case SIN:
         if ( is_constant(left) )
         { /* fold constants */
           listtop--;
           list[listtop].type = PUSHCONST;
           list[listtop].op1.real = sin(list[listtop].op1.real);
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case SINH:
         if ( is_constant(left) )
         { /* fold constants */
           REAL x;
           listtop--;
           x = exp(list[listtop].op1.real);
           list[listtop].type = PUSHCONST;
           list[listtop].op1.real = (x-1/x)/2;
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case COSH:
         if ( is_constant(left) )
         { /* fold constants */
           REAL x;
           listtop--;
           x = exp(list[listtop].op1.real);
           list[listtop].type = PUSHCONST;
           list[listtop].op1.real = (x+1/x)/2;
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case TANH:
         if ( is_constant(left) )
         { /* fold constants */
           REAL x;
           listtop--;
           x = exp(list[listtop].op1.real);
           list[listtop].type = PUSHCONST;
           list[listtop].op1.real = (x-1/x)/(x+1/x);
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case ATANH:
         if ( is_constant(left) )
         { /* fold constants */
           REAL x;
           listtop--;
           x = list[listtop].op1.real;
           list[listtop].type = PUSHCONST;
           list[listtop].op1.real = (log(x+1)-log(1-x))/2;
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case ASINH:
         if ( is_constant(left) )
         { /* fold constants */
           REAL x;
           listtop--;
           x = list[listtop].op1.real;
           list[listtop].type = PUSHCONST;
           list[listtop].op1.real = log(x + sqrt(x*x+1));
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case ACOSH:
         if ( is_constant(left) )
         { /* fold constants */
           REAL x;
           listtop--;
           x = list[listtop].op1.real;
           list[listtop].type = PUSHCONST;
           list[listtop].op1.real = 2*log(sqrt(x+1) + sqrt(x-1))-log(2.0);
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case ASIN:
         if ( is_constant(left) )
         { /* fold constants */
           listtop--;
           list[listtop].type = PUSHCONST;
           if ( fabs(list[listtop].op1.real) > 1.0 )
             kb_error(1426,"asin argument out of bounds.\n", COMMAND_ERROR );
           list[listtop].op1.real = asin(list[listtop].op1.real);
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case COS:
         if ( is_constant(left) )
         { /* fold constants */
           listtop--;
           list[listtop].type = PUSHCONST;
           list[listtop].op1.real = cos(list[listtop].op1.real);
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case ACOS:
         if ( is_constant(left) )
         { /* fold constants */
           listtop--;
           list[listtop].type = PUSHCONST;
           if ( fabs(list[listtop].op1.real) > 1.0 )
             kb_error(1427,"acos argument out of bounds.\n", COMMAND_ERROR );
           list[listtop].op1.real = acos(list[listtop].op1.real);
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case TAN:
         if ( is_constant(left) )
         { /* fold constants */
           listtop--;
           list[listtop].type = PUSHCONST;
           list[listtop].op1.real = tan(list[listtop].op1.real);
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case ATAN:
         if ( is_constant(left) )
         { /* fold constants */
           listtop--;
           list[listtop].type = PUSHCONST;
           list[listtop].op1.real = atan(list[listtop].op1.real);
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case SQR:
         if ( is_constant(left) )
         { /* fold constants */
           listtop--;
           list[listtop].type = PUSHCONST;
           list[listtop].op1.real = list[listtop].op1.real*list[listtop].op1.real;
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case SQRT:
         if ( is_constant(left) )
         { /* fold constants */
           listtop--;
           list[listtop].type = PUSHCONST;
           if ( list[listtop].op1.real < 0.0 )
             kb_error(1428,"sqrt of negative number.\n", COMMAND_ERROR );
           list[listtop].op1.real = sqrt(list[listtop].op1.real);
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case ELLIPTICK:
         if ( is_constant(left) )
         { /* fold constants */
           listtop--;
           list[listtop].type = PUSHCONST;
           list[listtop].op1.real = ellipticK(list[listtop].op1.real);
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case ELLIPTICE:
         if ( is_constant(left) )
         { /* fold constants */
           listtop--;
           list[listtop].type = PUSHCONST;
           list[listtop].op1.real = ellipticE(list[listtop].op1.real);
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;


    case CEIL_:
         if ( is_constant(left) )
         { /* fold constants */
           listtop--;
           list[listtop].type = PUSHCONST;
           list[listtop].op1.real = ceil(list[listtop].op1.real);
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case FLOOR_:
         if ( is_constant(left) )
         { /* fold constants */
           listtop--;
           list[listtop].type = PUSHCONST;
           list[listtop].op1.real = floor(list[listtop].op1.real);
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case CHS:
         if ( is_constant(left) )
         { /* fold constants */
           listtop--;
           list[listtop].type = PUSHCONST;
           list[listtop].op1.real = -list[listtop].op1.real;
           break;
         }
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;


    case MAXIMUM_:
         if ( is_constant(right) && is_constant(left) )
         { /* fold constants */
           listtop -= 2;
           if ( (left != listtop) || (right != listtop+1) )
             kb_error(1429,"Constant folding not working.",COMMAND_ERROR);
           list[listtop].op1.real = 
             (list[left].op1.real > list[right].op1.real) ?
                list[left].op1.real : list[right].op1.real;
           list[listtop].type = PUSHCONST;
           list[listtop].stack_delta = 1;
           break;
         }
         list[listtop].right = right - listtop;
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].stack_delta = -1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case MINIMUM_:
         if ( is_constant(right) && is_constant(left) )
         { /* fold constants */
           listtop -= 2;
           if ( (left != listtop) || (right != listtop+1) )
              kb_error(1430,"Internal error: Constant folding not working.",
                 COMMAND_ERROR);
           list[listtop].op1.real = 
             (list[left].op1.real < list[right].op1.real) ?
                list[left].op1.real : list[right].op1.real;
           list[listtop].type = PUSHCONST;
           list[listtop].stack_delta = 1;
           break;
         }
         list[listtop].right = right - listtop;
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].stack_delta = -1;
         list[listtop].datatype = list[right].datatype;
         break;

    case ATAN2_:
    case INCOMPLETE_ELLIPTICF:
    case INCOMPLETE_ELLIPTICE:
         if ( is_constant(right) && is_constant(left) )
         { /* fold constants */
           listtop -= 2;
           if ( (left != listtop) || (right != listtop+1) )
              kb_error(1431,"Internal error: Constant folding not working.",
                 COMMAND_ERROR);
           switch(type)
           { case ATAN2_: list[listtop].op1.real = 
                atan2(list[left].op1.real,list[right].op1.real);
              break;
             case INCOMPLETE_ELLIPTICF: list[listtop].op1.real = 
                incompleteEllipticF(list[left].op1.real,list[right].op1.real);
                 break;
             case INCOMPLETE_ELLIPTICE: list[listtop].op1.real = 
                incompleteEllipticE(list[left].op1.real,list[right].op1.real);
              break;
           }
           list[listtop].type = PUSHCONST;
           list[listtop].stack_delta = 1;
           break;
         }
         list[listtop].right = right - listtop;
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].stack_delta = -1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case WRAP_COMPOSE_:
         list[listtop].right = right - listtop;
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].stack_delta = -1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case WRAP_INVERSE_:
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

    case POW:
    case '^':
      if ( is_constant(right) )
      { REAL p = list[right].op1.real;
        if ( floor(p) == p )
        { /* integer power */
          n = (int)p;
          if ( n == 0 )
          { kb_error(1432,"Exponent 0.  Did you mean this?\n",WARNING);
             listtop--;
             list[listtop].type = REPLACECONST;
             list[listtop].op1.real = 1.0;
             break;
          }
          else if ( n == 1 )
          { /* leave alone */
             kb_error(1433,"Exponent 1.  Did you mean this?\n",WARNING);
             listtop-=2;
             break;
          }
          else if ( n == 2 )
          { listtop--;
            if ( is_constant(left) )
            { listtop--;
              list[listtop].type = PUSHCONST;
              list[listtop].op1.real *= list[listtop].op1.real;
              list[listtop].stack_delta = 1;
            }
            else
            { list[listtop].type = SQR;
              list[listtop].left = left - listtop;
              list[listtop].stack_delta = 0;
            }
            break;
          }
          else if ( n == -1 )
          { listtop--;
            if ( is_constant(left) )
            { listtop--;
              list[listtop].op1.real = 1./list[listtop].op1.real;
            }
            else
            { list[listtop].type = INV;
              list[listtop].left = left - listtop;
              list[listtop].stack_delta = 0;
            }
            break;
          }

          /* general */
          if ( is_constant(left) )
          { /* fold constants */
             int k;
             REAL x;

             listtop -= 2;
             list[listtop].type = PUSHCONST;
             list[listtop].stack_delta = 1;
             if ( left != listtop )
               kb_error(1434,"Internal error: Constant folding not working.",
                 COMMAND_ERROR);
             x = list[left].op1.real;
             for ( k = 1 ; k < abs(n) ; k++ )
               list[left].op1.real *= x;
             if ( n < 0 )
               list[left].op1.real = 1/list[left].op1.real;
             break;
          }
          listtop--; /* pop constant power */
          list[listtop].left = left - listtop;
          list[listtop].op1.intpow = n;
          list[listtop].type = INTPOW;
          break;
        }
      }
      if ( is_constant(right) && is_constant(left) )
        { /* fold constants */
          listtop -= 2;
          if ( (left != listtop) || (right != listtop+1) )
             kb_error(1435,"Internal error: Constant folding not working.",
                COMMAND_ERROR);
          list[listtop].op1.real = 
              pow(list[left].op1.real,list[right].op1.real);
          list[listtop].type = PUSHCONST;
          list[listtop].stack_delta = 1;
          break;
        }
      list[listtop].right = right - listtop;
      list[listtop].left = left - listtop;
      list[listtop].type = type;
      list[listtop].stack_delta = -1;
      list[listtop].datatype = REAL_TYPE;
      break;

    case '+':
        if ( is_constant(right) && is_constant(left) )
        { /* fold constants */
          
          listtop -= 2;
          if ( (left != listtop) || (right != listtop+1) )
             kb_error(1436,"Internal error: Constant folding not working.",
                 COMMAND_ERROR);
          list[listtop].op1.real = list[left].op1.real + list[right].op1.real;
          list[listtop].type = PUSHCONST;
          list[listtop].stack_delta = 1;
        }
        else if ( is_constant(right) && (list[right].op1.real==0.0) )
          listtop -= 2; /* just leave left summand */
        else if ( is_constant(left) && (list[left].op1.real==0.0) )
        { subtree_swap(&left,&right);
          listtop -= 2;
        }
        else
        {
          list[listtop].right = right - listtop;
          list[listtop].left = left - listtop;
          list[listtop].type = PLUS;
          list[listtop].stack_delta = -1;
        }
        list[listtop].datatype = REAL_TYPE;
        break;

    case '-':
        if ( is_constant(right) && is_constant(left) )
        { /* fold constants */
          
          listtop -= 2;
          if ( (left != listtop) || (right != listtop+1) )
             kb_error(1437,"Internal error: Constant folding not working.",
                COMMAND_ERROR);
          list[listtop].op1.real = list[left].op1.real - list[right].op1.real;
          list[listtop].type = PUSHCONST;
          list[listtop].stack_delta = 1;
        }
        else if ( is_constant(right) && (list[right].op1.real==0.0) )
          listtop -= 2; /* just leave left summand */
        else if ( is_constant(left) && (list[left].op1.real==0.0) )
        { subtree_swap(&left,&right);
          listtop -= 1;
          list[listtop].left = right - listtop;
          list[listtop].type = CHS;
          list[listtop].stack_delta = 0;
        }
        else
        {
          list[listtop].right = right - listtop;
          list[listtop].left = left - listtop;
          list[listtop].type = MINUS;
          list[listtop].stack_delta = -1;
        }
        list[listtop].datatype = REAL_TYPE;
        break;

    case '=':  /* equality as low priority minus */
        if ( is_constant(right) && is_constant(left) )
        { /* fold constants */
          
          listtop -= 2;
          if ( (left != listtop) || (right != listtop+1) )
             kb_error(1438,"Internal error: Constant folding not working.",
                COMMAND_ERROR);
          list[listtop].op1.real = list[left].op1.real - list[right].op1.real;
          list[listtop].type = PUSHCONST;
          list[listtop].stack_delta = 1;
        }
        else if ( is_constant(right) && (list[right].op1.real==0.0) )
          listtop -= 2; /* just leave left summand */
        else
        {
          list[listtop].right = right - listtop;
          list[listtop].left = left - listtop;
          list[listtop].type = EQUATE;
          list[listtop].stack_delta = -1;
        }
        list[listtop].datatype = REAL_TYPE;
        break;

    case '*':
        if ( is_constant(right) && is_constant(left) )
        { /* fold constants */
          
          listtop -= 2;
          if ( (left != listtop) || (right != listtop+1) )
             kb_error(1439,"Internal error: Constant folding not working.",
               COMMAND_ERROR);
          list[listtop].op1.real = list[left].op1.real * list[right].op1.real;
          list[listtop].type = PUSHCONST;
          list[listtop].stack_delta = 1;
        }
        else if ( is_constant(left) && (list[left].op1.real==0.0) )
         { 
           listtop = left; /* just leave 0 */
         }
        else if ( ( is_constant(right) && (list[right].op1.real==0.0) ) )
         { subtree_swap(&left,&right);
           listtop = right; /* just leave 0 */
         }
        else if ( is_constant(right) && (list[right].op1.real==1.0) )
           listtop -= 2; /* just leave left multiplicand */
        else if ( is_constant(left) && (list[left].op1.real==1.0) )
        { subtree_swap(&left,&right);
          listtop -= 2;
        }
        else
        {
          list[listtop].right = right - listtop;
          list[listtop].left = left - listtop;
          list[listtop].type = TIMES;
          list[listtop].stack_delta = -1;
        }
        list[listtop].datatype = REAL_TYPE;
        break;

    case '/':
        if ( is_constant(right) && is_constant(left) )
        { /* fold constants */
          
          listtop -= 2;
          if ( (left != listtop) || (right != listtop+1) )
             kb_error(1440,"Internal error: Constant folding not working.",
               COMMAND_ERROR);
          if ( list[right].op1.real == 0.0 ) 
          {
             kb_error(1441,"Divide by zero.",COMMAND_ERROR);
          }
          list[listtop].op1.real = list[left].op1.real / list[right].op1.real;
          list[listtop].type = PUSHCONST;
        }
        else if  ( is_constant(left) && (list[left].op1.real==0.0) ) 
        { listtop = left; /* just leave 0 */
          list[listtop].op1.real = 0.0;
          list[listtop].type = PUSHCONST;
          list[listtop].stack_delta = 1;
        }
        else if ( is_constant(right) && (list[right].op1.real==0.0) )
           kb_error(1442,"Divide by zero.",COMMAND_ERROR);
        else if ( is_constant(right) && (list[right].op1.real==1.0) )
           listtop -= 2; /* just leave numerator */
        else
        {
          list[listtop].right = right - listtop;
          list[listtop].left = left - listtop;
          list[listtop].type = DIVIDE;
          list[listtop].stack_delta = -1;
        }
        list[listtop].datatype = REAL_TYPE;
        break;

    case '%':
        if ( is_constant(right) && is_constant(left) )
        { /* fold constants */
          listtop -= 2;
          if ( (left != listtop) || (right != listtop+1) )
             kb_error(1443,"Internal error: Constant folding not working.",
               COMMAND_ERROR);
          list[listtop].op1.real = list[left].op1.real - 
              floor(list[left].op1.real / list[right].op1.real)
              *list[right].op1.real;
          list[listtop].type = PUSHCONST;
          list[listtop].stack_delta = 1;
        }
        else
        { list[listtop].right = right - listtop;
          list[listtop].left = left - listtop;
          list[listtop].type = REALMOD;
          list[listtop].stack_delta = -1;
        }
        list[listtop].datatype = REAL_TYPE;
        break;

    case IMOD_:
        if ( is_constant(right) && is_constant(left) )
        { /* fold constants */
          listtop -= 2;
          if ( (left != listtop) || (right != listtop+1) )
             kb_error(1444,"Internal error: Constant folding not working.",
               COMMAND_ERROR);
          list[listtop].op1.real = floor(list[left].op1.real) - 
              floor(floor(list[left].op1.real)/floor(list[right].op1.real))
              *floor(list[right].op1.real);
          list[listtop].type = PUSHCONST;
          list[listtop].stack_delta = 1;
        }
        else
        { list[listtop].right = right - listtop;
          list[listtop].left = left - listtop;
          list[listtop].type = IMOD_;
          list[listtop].stack_delta = -1;
        }
        list[listtop].datatype = REAL_TYPE;
        break;

    case IDIV_:
        if ( is_constant(right) && is_constant(left) )
        { /* fold constants */
          listtop -= 2;
          if ( (left != listtop) || (right != listtop+1) )
             kb_error(1445,"Internal error: Constant folding not working.",
               COMMAND_ERROR);
          list[listtop].op1.real = 
              (int)(list[left].op1.real)/(int)(list[right].op1.real);
          list[listtop].type = PUSHCONST;
          list[listtop].stack_delta = 1;
        }
        else
        { list[listtop].right = right - listtop;
          list[listtop].left = left - listtop;
          list[listtop].type = IDIV_;
          list[listtop].stack_delta = -1;
        }
        list[listtop].datatype = REAL_TYPE;
        break;

    case PUSHPARAM:
        if ( maxp == 0 )
          kb_error(1446,"Constant expression required.\n",EXPRESSION_ERROR);
        list[listtop].type = PUSHPARAM;
        list[listtop].op1.coordnum = n = coord_num-1;
        if ( (n < 0) || (n >= maxp) )
        {
          sprintf(errmsg,"Coordinate number too high: %d\n",n+1);
          kb_error(1447,errmsg,EXPRESSION_ERROR);
        }
        list[listtop].stack_delta = 1;
        list[listtop].datatype = REAL_TYPE;
        break;

    case USERFUNC:
         list[listtop].type = USERFUNC;
         list[listtop].op1.userfunc = (NTYPE)int_val-1;
         if ( int_val < 1 || (userfunc[int_val-1] == NULL) )
         { sprintf(errmsg,"Invalid user function number: %d\n",int_val);
           kb_error(1448,errmsg,EXPRESSION_ERROR);
         }     
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case VIEW_MATRIX_:
         list[listtop].right = right - listtop;
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].stack_delta = -1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case GET_INTERNAL_:
         list[listtop].type = GET_INTERNAL_;
         list[listtop].op1.name_id = left;
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case PUSHQPRESSURE_:
    case PUSHQTARGET_:
    case PUSHQVALUE_:
    case PUSHQMODULUS_:
    case PUSHQTOLERANCE_:
    case PUSHMMODULUS_:
    case PUSHQVOLCONST_:
    case PUSHQPARAMETER_1_:
    case PUSHQFIXED_:
    case PUSHQENERGY_:
    case PUSHQCONSERVED_:
    case PUSHQINFO_ONLY_:
         list[listtop].type = type;
         list[listtop].op1.quant_id = left;
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case PUSHMVALUE_:
         list[listtop].type = type;
         list[listtop].op1.meth_id = left;
         if ( reading_comp_quant_flag ) 
         { int i;
           struct gen_quant *q = GEN_QUANT(cur_quant);
           METH_INSTANCE(left)->flags |= Q_COMPOUND;

           /* see if in quantity's list */
           for ( i = 0 ; i < q->method_count ; i++ )
             if ( q->meth_inst[i] == left ) 
                break;
           if ( i == q->method_count )
           {  /* add to list */
              METH_INSTANCE(left)->quant = cur_quant;
              METH_INSTANCE(left)->quant_index = q->method_count;
			  attach_method_num(cur_quant,left);
           }
         }
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case VIEW_MATRIX_LVALUE_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].right = right - listtop;
         break;

    case SET_VIEW_MATRIX_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].right = right - listtop;
         list[listtop].op2.assigntype = assigntype;
         list[listtop].stack_delta = -3;
         break;
         
    case SET_QMODULUS_:
    case SET_QTOLERANCE_:
    case SET_MMODULUS_:
    case SET_QVOLCONST_:
    case SET_QTARGET_:
    case SET_QPARAMETER_1_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].op1.quant_id = right;
         list[listtop].op2.assigntype = assigntype;
         list[listtop].stack_delta = -1;
         break;

    case DYNAMIC_LOAD_FUNC_:
         list[listtop].type = DYNAMIC_LOAD_FUNC_;
         list[listtop].op1.funcptr = globals(left)->value.funcptr;
         list[listtop].op2.name_id = left;
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case PUSHDELTA_:
    case PUSH_PARAM_SCALE:
         list[listtop].type = type;
         list[listtop].op1.name_id = left;
         list[listtop].flags |= EPHEMERAL;
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case PUSH_PARAM_FIXED:
         list[listtop].type = type;
         list[listtop].op1.name_id = left;
         list[listtop].flags |= EPHEMERAL;
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case PUSH_PARAM_EXTRA_:  
         if ( (right != V_VELOCITY_ATTR) && (right != V_FORCE_ATTR) )
           kb_error(2473,"Illegal attribute of variable.\n",COMMAND_ERROR);
         list[listtop].type = type;
         list[listtop].op1.name_id = left; /* which parameter */
         list[listtop].op2.extranum = right; /* which attribute */
         list[listtop].flags |= EPHEMERAL;
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case PUSHGLOBAL_:
       { 
         struct global *g = globals(left);
         list[listtop].type = PUSHGLOBAL_;
         list[listtop].op1.name_id = left;
         list[listtop].flags |= EPHEMERAL;
         if ( (g->flags & METHOD_NAME) && reading_comp_quant_flag )
         { struct method_instance *mi=METH_INSTANCE(left);
           mi->flags |= Q_COMPOUND;
           quant_flags[basic_gen_methods[mi->gen_method].type] 
            |= GEN_QUANT(cur_quant)->flags &
                     (Q_ENERGY|Q_FIXED|Q_INFO|Q_CONSERVED);
           attach_method_num(cur_quant,left);
         }
         list[listtop].datatype = (g->type <= MAX_NUMERIC_TYPE) ? REAL_TYPE :
           g->type;
         list[listtop].stack_delta = 1;
         break;
       }
     
    case PUSH_PERM_GLOBAL_:
    case PERM_STRINGGLOBAL_:
       { struct global *g;
         list[listtop].type = type;
         list[listtop].op1.name_id = left;
         list[listtop].stack_delta = 1;
         g = globals(left);
         list[listtop].datatype = g->type <= MAX_NUMERIC_TYPE ? REAL_TYPE :
            g->type;
         break;
       }
       
    case PERM_PROCEDURE_:
         list[listtop].type = type;
         list[listtop].op1.name_id = left;
         break;

    case STRINGGLOBAL_:
         list[listtop].type = type;
         list[listtop].op1.name_id = left;
         list[listtop].flags |= EPHEMERAL;
         list[listtop].stack_delta = 1;
         list[listtop].datatype = STRING_TYPE;
         break;

    case PROCEDURE_:
         list[listtop].type = type;
         list[listtop].op1.name_id = left;
         list[listtop].flags |= EPHEMERAL;
         break;

    case SET_CONSTRAINT_GLOBAL:
    case UNSET_CONSTRAINT_GLOBAL:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].stack_delta = -1;
         break;

    case SET_CONSTRAINT_NAME_GLOBAL:
    case UNSET_CONSTRAINT_NAME_GLOBAL:
         list[listtop].type = type;
         list[listtop].stack_delta = 0;
         list[listtop].op3.connum = globals(left)->value.cnum; 
         break;

        
        
    case ON_CONSTRAINT_:
    case HIT_CONSTRAINT_:
    case ON_BOUNDARY_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;
         list[listtop].flags |= EPHEMERAL;
         list[listtop].stack_delta = 0;
         list[listtop].datatype = REAL_TYPE;
         break;

    case ON_CONSTRAINT_NAME:
    case HIT_CONSTRAINT_NAME:
         list[listtop].type = type;
         list[listtop].op3.connum = globals(left)->value.cnum; 
                                     /* actual constraint number */
         list[listtop].flags |= EPHEMERAL;
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case ON_BOUNDARY_NAME:
         list[listtop].type = type;
         list[listtop].op3.bdrynum = globals(left)->value.bnum;
         list[listtop].flags |= EPHEMERAL;
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case ON_QUANTITY_:
         list[listtop].type = type;
         list[listtop].op2.quant_id = left;
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case ON_METHOD_INSTANCE_:
         list[listtop].type = type;
         list[listtop].op2.meth_id = left;
         list[listtop].stack_delta = 1;
         list[listtop].datatype = REAL_TYPE;
         break;

    case INDEXED_COORD_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;   
         if ( list[left].op1.indexcount != 1 )
           kb_error(2568,"Coordinate can have only one index.\n",COMMAND_ERROR);
         list[listtop].datatype = REAL_TYPE;
         break;

    case PRINT_VERTEXNORMAL_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;    /* element */
         break;

    case GET_VERTEXNORMAL_:
         list[listtop].type = type;
         list[listtop].left = left - listtop;    /* index set */
         if ( list[left].op1.indexcount != 1 )
           kb_error(2569,"Vertexnormal can have only one index.\n",
              COMMAND_ERROR);
         list[listtop].datatype = REAL_TYPE;
         break;

     case INDEXED_ATTRIBUTE:  /* get extra attribute value */
            { struct extra *ex;
              etype = (unsigned)right >> YYTYPESHIFT;
              right = right & YYSHIFTMASK;
              ex = EXTRAS(etype) + right;
              list[listtop].type = GET_EXTRA_ATTR_;
              list[listtop].op2.eltype = etype;
              list[listtop].op3.extranum = right;  /* which extra */
              list[listtop].left = left - listtop;  /* index */
              list[listtop].stack_delta = 1 - ex->array_spec.dim;
              if ( ex->array_spec.dim != list[left].op1.indexcount )
              { sprintf(errmsg,"Attribute %s must have %d indices.\n",
                 ex->name,ex->array_spec.dim);
                kb_error(2513,errmsg,COMMAND_ERROR);
              }
              list[listtop].flags |= EPHEMERAL;
            }
            break;

     case PRINT_ATTR_ARRAY_:  /* print element attribute array or slice */
            { struct extra *ex;
              int exnum;
              etype = int_val >> YYTYPESHIFT;
              exnum = int_val & YYSHIFTMASK;
              ex = EXTRAS(etype) + exnum;
              list[listtop].type = type;
              list[listtop].op1.indexcount = right ? list[right].op1.indexcount : 0; /* indices */
              list[listtop].op2.eltype = etype;
              list[listtop].op3.extranum = exnum;  /* which extra */
              list[listtop].left = left - listtop;  /* element */
              if ( right ) list[listtop].right = right - listtop;  /* index */
              if ( ex->array_spec.dim < list[right].op1.indexcount )
              { sprintf(errmsg,"Attribute %s must have at most %d indices.\n",
                 ex->name,ex->array_spec.dim);
                kb_error(2644,errmsg,COMMAND_ERROR);
              }
              list[listtop].flags |= EPHEMERAL;
              list[listtop].stack_delta = -list[listtop].op1.indexcount;
            }
            break;

     case QUALIFIED_ATTRIBUTE:
          list[listtop].type = QUALIFIED_ATTRIBUTE;
          list[listtop].left = left-listtop;
          list[listtop].right = right-listtop;
          list[listtop].datatype = REAL_TYPE;
          etype = list[left].op1.eltype; /* element type */
          /* element type error checking */
          check_element_type(list[right].type,etype);
          break;
  
     case ATTRIBUTE:
        list[listtop].type = (NTYPE)left;
        /* some special treatments */
        switch ( left )
        { 
          case COORD_: list[listtop].op2.coordnum = right-1;
             if ( right > SDIM )
             kb_error(2475,"Coordinate dimension exceeds space dimension.\n",
                 COMMAND_ERROR);
             break;
          case PARAM_: list[listtop].op2.coordnum = right-1;
             using_param_flag = 1;
             break;
          case GET_INSTANCE_:
          case GET_QUANTITY_: list[listtop].op2.quant_id = right;
             break;
          case GET_EXTRA_ATTR_:
            { struct extra *ex;
              etype = (unsigned)right >> YYTYPESHIFT;
              ex = EXTRAS(etype) + (right & YYSHIFTMASK);

              if ( ex->array_spec.dim > 0 ) 
              { sprintf(errmsg,
                  "\"%s\" is an indexed attribute; index is missing.  Or use element name to print as full array.\n",
                      ex->name);
                kb_error(2634,errmsg,Q_ERROR);
              }

              list[listtop].op2.eltype = etype;
              list[listtop].op3.extranum = (right & YYSHIFTMASK);
              list[listtop].flags |= EPHEMERAL;
              list[listtop].stack_delta = 1-ex->array_spec.dim;
            }
            break;
         }
         if ( left != GET_EXTRA_ATTR_ )
            list[listtop].stack_delta = 1;
         break;

     case SET_ATTRIBUTE_:
     case SET_ATTRIBUTE_L:
     case SET_ATTRIBUTE_A:
          if ( elsym == NULL )
              kb_error(1477,"Don't have element for attribute to apply to.\n",
                    COMMAND_ERROR);
          list[listtop].type = type;
          if ( left ) list[listtop].left = left-listtop;  /* value */
          if ( right )
          { /* index; check which attributes are legal */
            list[listtop].right = right-listtop;
            switch ( attr_kind )
             { case SET_EXTRA_ATTR_: 
                 list[listtop].flags |= EPHEMERAL;
                 break;
               case SET_COORD_1: case SET_PARAM_1: break;
               default: kb_error(1478,"Attribute is not indexed.\n",COMMAND_ERROR);
             }
          }
          list[listtop].op1.localnum = elsym->localnum; 
          list[listtop].op2.attr_kind = attr_kind;
         /* to node */
          /* error checking on attributes and element types */
          etype = elsym->type;
          switch ( attr_kind )
          {
            case SET_EXTRA_ATTR_:
            { struct extra *ex;
              ex = EXTRAS(etype);
              for ( n = 0 ; n < web.skel[etype].extra_count ; n++,ex++ )
                if ( stricmp(ex->name,set_extra_name) == 0 ) break;
              if ( n == web.skel[etype].extra_count )
              { sprintf(errmsg,"Invalid extra attribute name '%s'.\n",
                set_extra_name);
                kb_error(1479,errmsg,COMMAND_ERROR);
              }
              list[listtop].op3.extra_info = (etype << ESHIFT) + n;
              if ( !(ex->flags & DIMENSIONED_ATTR) && right )
              { sprintf(errmsg,"Cannot use index with attribute '%s'.\n",ex->name);
                kb_error(2499,errmsg,COMMAND_ERROR);
              }
              if ( (ex->flags & DIMENSIONED_ATTR) && !right )
              { sprintf(errmsg,"Must use index with attribute '%s'.\n",ex->name);
                kb_error(1481,errmsg,COMMAND_ERROR);
              }
              if ( right && (list[right].op1.indexcount != ex->array_spec.dim) )
              { sprintf(errmsg,"Attribute '%s' has %d indexes.\n",
                     ex->name,ex->array_spec.dim);
                kb_error(2498,errmsg,COMMAND_ERROR);
              }
            list[listtop].stack_delta = -1-ex->array_spec.dim;
            }
            break;
          case SET_WRAP_:
            if ( etype != EDGE )
            kb_error(2239,"Wrap only for edges.\n",COMMAND_ERROR);
            list[listtop].stack_delta = -1;
            break;
          case SET_COORD_: case SET_COORD_1: case SET_COORD_2:
          case SET_COORD_3: case SET_COORD_4: case SET_COORD_5:
          case SET_COORD_6: case SET_COORD_7: case SET_COORD_8:
            if ( attr_kind-SET_COORD_1 >= SDIM )
             kb_error(2543,"Coordinate dimension exceeds space dimension.\n",
               COMMAND_ERROR);
            if ( etype != VERTEX )
              kb_error(1482,"Coordinates only for vertices.\n",COMMAND_ERROR);
            list[listtop].stack_delta = -1;
            break;
          case SET_PARAM_: case SET_PARAM_1: case SET_PARAM_2:
          case SET_PARAM_3: case SET_PARAM_4:
            if ( etype != VERTEX )
              kb_error(1483,"Boundary parameters only for vertices.\n",COMMAND_ERROR);
            list[listtop].stack_delta = -1;
            break;
          case SET_TAG_:
            if ( etype != FACET )
              kb_error(1484,"Tag only for facets now.\n",COMMAND_ERROR);
            list[listtop].stack_delta = -1;
            break;
          case SET_OPACITY_:
            if ( etype != FACET )
              kb_error(1485,"Opacity only for facets.\n",COMMAND_ERROR);
            list[listtop].stack_delta = -1;
            break;
          case SET_FRONTCOLOR_:
          case SET_BACKCOLOR_:
            if ( (etype != FACET) )
              kb_error(1304,"Front or back color only for facets.\n",COMMAND_ERROR);
            list[listtop].stack_delta = -1;
            break;
          case SET_FRONTBODY_:
          case SET_BACKBODY_:
            if ( web.representation == STRING )
            { if ( (etype != FACET) && (etype != EDGE) )
                kb_error(1486,
     "Frontbody or backbody only for facets or edges in string model.\n",
                COMMAND_ERROR);
            }
            else if (etype != FACET) 
               kb_error(1308,"Frontbody or backbody only for facets.\n",COMMAND_ERROR);
            list[listtop].stack_delta = -1;
            break;
          case SET_COLOR_:
            if ( !((etype == FACET) || (etype == EDGE)) )
               kb_error(1487,"Color only for edges or facets.\n",COMMAND_ERROR);
            list[listtop].stack_delta = -1;
            break;
          case SET_VOLCONST_:
            if ( etype != BODY )
             kb_error(1488,"Volconst only for bodies.\n",COMMAND_ERROR);
            list[listtop].stack_delta = -1;
            break;
          case SET_TARGET_:
            if ( etype != BODY )
             kb_error(1489,"Target volume only for bodies.\n",COMMAND_ERROR);
            list[listtop].stack_delta = -1;
            break;
          case SET_VOLUME_:
            if ( etype != BODY )
             kb_error(1490,"Target volume only for bodies.\n",COMMAND_ERROR);
            kb_error(1491,
            "Volume is read-only. Setting TARGET instead.\n",
              WARNING);
            list[listtop].op2.attr_kind = SET_TARGET_;
            list[listtop].stack_delta = -1;
            break;
          case SET_PRESSURE_:
            if ( etype != BODY )
            kb_error(1492,"Pressure only for bodies.\n",COMMAND_ERROR);
            list[listtop].stack_delta = -1;
            break;
          case SET_PHASE_:
          case SET_CONSTRAINT_: 
          case SET_BOUNDARY_: 
          case SET_ORIGINAL_: 
            list[listtop].stack_delta = -1;
            break;
          case SET_DENSITY_:
            if ( etype == VERTEX )
            kb_error(1493,"No density for vertices.\n",COMMAND_ERROR);
            list[listtop].stack_delta = -1;
            break;
          case SET_FIXED_:
            if ( etype == FACETEDGE )
            kb_error(2527,"No fixedness for facetedges.\n",COMMAND_ERROR);
            if ( etype == BODY )
            kb_error(2528,"Use 'set body target ...' to fix volume.\n",
               COMMAND_ERROR);
            break;
          case SET_HIT_PARTNER_:
            if ( etype != VERTEX )
            kb_error(3002,"Hit_partner is only for vertices.\n",COMMAND_ERROR);
            break;
          case SET_NO_DISPLAY_:
            if ( etype != FACET )
            kb_error(1495,"No_display is only for facets.\n",COMMAND_ERROR);
            break;
          case SET_NO_REFINE_:
            if ( etype == BODY || etype == VERTEX )
            kb_error(1496,"No no_refine for vertices or bodies.\n",
                COMMAND_ERROR);
            break;
          case SET_NONCONTENT_:
             if ( ((web.representation == STRING) && (etype != EDGE)) || 
                  ((web.representation != STRING) && (etype != FACET)) )
                kb_error(2903,"Noncontent only applies to edges or facets.\n",
                   COMMAND_ERROR);
            break;
         }
         /* error checking for arithmetic assigns */
         if ( type == SET_ATTRIBUTE_A  )
         { switch ( attr_kind )
           { case SET_FIXED_: case SET_TRIPLE_PT_: case SET_TETRA_PT_: 
             case SET_AXIAL_POINT_:
                kb_error(2241,"Cannot assign value to this attribute with :=.\n",
                 COMMAND_ERROR);
           }
           if ( assigntype != ASSIGN_ ) 
           switch ( attr_kind )
           { case SET_ORIENTATION_: case SET_PHASE_: case SET_OPACITY_:
             case SET_CONSTRAINT_: case SET_ORIGINAL_: case SET_COLOR_:
             case SET_FRONTCOLOR_: case SET_BACKCOLOR_: case SET_WRAP_:
             case SET_TAG_: case SET_FRONTBODY_: case SET_BACKBODY_:
             case SET_BOUNDARY_:
             kb_error(2242,
               "Cannot use arithmetic assign with this attribute.\n",
             COMMAND_ERROR);
           } 
         }
         break;

     case EQ_:
     case NE_:
     case GE_:
     case LE_:
     case GT_:
     case LT_:
         list[listtop].right = right - listtop;
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].stack_delta = -1;
         list[listtop].datatype = REAL_TYPE;
         break;

     case AND_:  /* for short-circuit evaluation, have to move test */
     case OR_:    /* node between operands */
      { int top;
         top = listtop;
         list[listtop].type = type;
         subtree_swap(&right,&top);
         list[top].right = right - top;
         list[top].left = left - top;
         list[top].op1.skipsize = listtop - top;
         listtop++;
         list[listtop].type = CONJUNCTION_END;
         list[listtop].left = top - listtop;
         list[listtop].stack_delta = -1;
         list[listtop].datatype = REAL_TYPE;
         break;
      }
     case NOT_:
         list[listtop].left = left - listtop;
         list[listtop].type = type;
         list[listtop].datatype = REAL_TYPE;
         break;

     default:
        sprintf(errmsg,"Internal error: Unknown MAKENODE %d type %s\n",type,tokname(type));
        kb_error(1329,errmsg,COMMAND_ERROR);

      }
}

/******************************************************************
*
*  Function: is_constant()
*
*  Purpose:  See if tree node type is a constant value.
*            Also checks that datatype is REAL_TYPE.
*/

int is_constant ARGS1((node),
int node)
{ 
  if ( list[node].datatype != REAL_TYPE )
    kb_error(3711,"Operand datatype is not numeric.\n",
      datafile_flag ? DATAFILE_ERROR : COMMAND_ERROR);

  switch(list[node].type)
  {  case PUSHPI:
     case PUSHE:
     case PUSHCONST:
        return 1;
     default:
        return 0;
  }
}

/**********************************************************************
*
* Function: check_element_type()
*
* purpose: see if attribute is legal for element 
*/

void check_element_type ARGS2((attrib,etype),
int attrib,
int etype)
{
  switch ( attrib )
  {  
    case GET_TAG_:
     if ( etype != FACET )
         kb_error(1455,"Tag only for facets now.\n",COMMAND_ERROR);
     break;

    case GET_MIDV_:
     if ( etype != EDGE )
         kb_error(1456,"Midv only for quadratic edges.\n",COMMAND_ERROR);
            break;

    case GET_SQ_MEAN_CURV_:
     if ( etype != VERTEX )
         kb_error(1457,"Square mean curvature only for vertices.\n",
         COMMAND_ERROR);
     break;

    case GET_FRONTBODY_:
    case GET_BACKBODY_:
     if ( (etype != FACET) && !((etype==EDGE)&&(web.representation==STRING)))
         kb_error(1297,"Frontbody or backbody only for facets, or string model edges.\n",
           COMMAND_ERROR);
      break;

    case GET_FRONTCOLOR_:
    case GET_BACKCOLOR_:
     if ( (etype != FACET) )
         kb_error(1458,"Frontcolor or backcolor only for facets.\n",COMMAND_ERROR);
      break;

    case GET_COLOR_:
     if ( !((etype == FACET) || (etype == EDGE)) )
         kb_error(1459,"Color only for edges or facets.\n",COMMAND_ERROR);
      break;

    case BARE_:
      if ( (etype == BODY) || (etype == FACET) )
          kb_error(1460,"No bareness for facets or bodies.\n",COMMAND_ERROR);
      break;

    case GET_SHOW_:
     if ( (etype != EDGE) && (etype != FACET) )
        kb_error(2243,"\"Show\" only for edges or facets.\n",COMMAND_ERROR);
     break;


    case GET_ORIENTATION_:
     if ( (etype != EDGE) && (etype != FACET) )
        kb_error(1461,"Orientation only for edges or facets.\n",COMMAND_ERROR);
     break;

    case GET_LENGTH_:
     if ( etype != EDGE )
         kb_error(1462,"Length only for edges.\n",COMMAND_ERROR);
     break;

    case GET_MEANCURV_:
     if ( etype != VERTEX )
         kb_error(2872,"Mean_curvature only for vertices.\n",COMMAND_ERROR);
     if ( web.representation == SIMPLEX )
        kb_error(4567,
 "Mean_curvature attribute implemented only for string and soapfilm models.\n",
         RECOVERABLE);
     break;

    case GET_VERTEXNORMAL_:
     if ( etype != VERTEX )
         kb_error(2244,"Facetnormal only for edges.\n",COMMAND_ERROR);
      break;

    case GET_FIXEDVOL_:
     if ( etype != BODY )
         kb_error(1463,"Fixedvol only for bodies.\n",COMMAND_ERROR);
     break;

    case GET_WRAP_:
     if ( etype != EDGE )
         kb_error(1464,"Wrap only for edges.\n",COMMAND_ERROR);
     break;

    case GET_MID_EDGE_:
     if ( etype != VERTEX )
         kb_error(3112,"Mid_edge only for vertices.\n",COMMAND_ERROR);
     break;

    case GET_MID_FACET_:
     if ( etype != VERTEX )
         kb_error(3113,"Mid_facet only for vertices.\n",COMMAND_ERROR);
     break;

    case GET_DIHEDRAL_:
     if ( web.representation == SOAPFILM )
     { if (etype != EDGE) 
          kb_error(1465,"Dihedral only for edges.\n",COMMAND_ERROR);
     }
     else if ( web.representation == STRING )
     { if (etype != VERTEX) 
         kb_error(1466,"Dihedral only for vertices.\n",COMMAND_ERROR);
     }
     else 
      kb_error(1467,
      "Dihedral defined only for STRING and SOAPFILM models.\n",COMMAND_ERROR);
     break;

    case GET_AREA_:
     if ( etype != FACET )
         kb_error(1468,"Area only for facets.\n",COMMAND_ERROR);
     break;

    case GET_EDGE_:
     if ( etype != FACETEDGE )
         kb_error(1469,"Edge only for facetedges.\n",COMMAND_ERROR);
     break;

    case GET_FACET_:
     if ( etype != FACETEDGE )
         kb_error(1470,"Facet only for facetedges.\n",COMMAND_ERROR);
     break;

    case GET_TARGET_:
     if ( etype != BODY )
         kb_error(1471,"Target only for bodies.\n",COMMAND_ERROR);
     break;

    case GET_VOLCONST_:
     if ( etype != BODY )
         kb_error(1472,"Volconst only for bodies.\n",COMMAND_ERROR);
     break;

    case GET_VOLUME_:
     if ( etype != BODY )
         kb_error(1473,"Volume only for bodies.\n",COMMAND_ERROR);
     break;

    case GET_DENSITY_:
     if ( etype == VERTEX )
         kb_error(1474,"No density for vertices.\n",COMMAND_ERROR);
     break;

    case SET_HIT_PARTNER_:
     if ( etype != VERTEX )
         kb_error(3003,"Hit_partner only for vertices.\n",
           COMMAND_ERROR);
     break;

    case SET_NO_REFINE_:
     if ( etype == BODY || etype == VERTEX )
         kb_error(1494,"No no_refine for vertices or bodies.\n",
           COMMAND_ERROR);
     break;

    case SET_NO_DISPLAY_:
     if ( etype != FACET )
         kb_error(1320,"No_display is only for facets.\n",
         COMMAND_ERROR);
         break;
      }
}

/**********************************************************************
*
* function:  check_array_dims_same()
*
* purpose: see if two arrays are of the same dimensionality (i.e
*          same number of indices, not number of elements).
*          Also of same type.
*
* return: 1 if same, 0 if not.
*/
int  check_array_dims_same(
   int left, /* id number of array */
   int right)   /* id number of second array */
{
  struct array *alvalue;
  struct array *arvalue;
  int adim,atype,bdim,btype;

  if ( (left & GTYPEMASK) == ATTRIBNAME )
  { struct extra *ex = EXTRAS(name_eltype(left)) + (left & GLOBMASK);
    alvalue = &(ex->array_spec);
    adim = alvalue->dim;
    atype = alvalue->datatype;
  }
  else
  { struct global *glvalue = globals(left);
    adim = glvalue->attr.arrayptr->dim;
    atype = glvalue->type;
  }
  if ( (right & GTYPEMASK) == ATTRIBNAME )
  { struct extra *ex = EXTRAS(name_eltype(right)) + (right & GLOBMASK);
    arvalue = &(ex->array_spec);
    bdim = arvalue->dim;
    btype = arvalue->datatype;
 }
  else
  { struct global *grvalue = globals(right);
    arvalue = grvalue->attr.arrayptr;
    bdim = grvalue->attr.arrayptr->dim;
    btype = grvalue->type;
 }

  if ( atype != btype )
    return 0;
  return adim == bdim;
}

/*********************************************************************
*
* function: check_recalc_attr()
*
* Purpose: see if attribute has RECALC_ATTR flag bit set.
*/

int check_recalc_attr(int name_id)
{
  if ( (name_id & GTYPEMASK) == ATTRIBNAME )
  { struct extra *ex = EXTRAS(name_eltype(name_id)) + (name_id & GLOBMASK);
    return ex->flags & RECALC_ATTR;
  }
 
  return 0;
}
/*********************************************************************
*
* function: check_readonly_attr()
*
* purpose:  See if left side of assignment is to read-only attribute.
*/

void check_readonly_attr(int name_id)
{
  if ( (name_id & GTYPEMASK) == ATTRIBNAME )
  { if ( name_id == set_name_eltype(V_NORMAL_ATTR,VERTEX) )
      kb_error(3017,"__vertex_normal is a read-only attribute.\n",
        COMMAND_ERROR);
    if ( name_id == set_name_eltype(E_VECTOR_ATTR,EDGE) )
      kb_error(3016,"__e_vector is a read-only attribute.\n",
        COMMAND_ERROR);
    if ( name_id == set_name_eltype(F_NORMAL_ATTR,FACET) )
      kb_error(3018,"__e_vector is a read-only attribute.\n",
        COMMAND_ERROR);
  }
} /* end check_readonly_attr() */

/*********************************************************************
*
* function: check_special_attr()
*
* purpose:  Tell user that calculated attributes on right side are
*           only implemented as lone terms.
*/

void check_special_attr(int name_id)
{
  if ( (name_id & GTYPEMASK) == ATTRIBNAME )
  { if ( name_id == set_name_eltype(V_NORMAL_ATTR,VERTEX) )
      kb_error(3123,"__vertex_normal term must be lone term on right side.\n",
        COMMAND_ERROR);
    if ( name_id == set_name_eltype(E_VECTOR_ATTR,EDGE) )
      kb_error(3024,"__e_vector term must be lone term on right side.\n",
        COMMAND_ERROR);
    if ( name_id == set_name_eltype(F_NORMAL_ATTR,FACET) )
      kb_error(3124,"__e_vector term must be lone term on right side.\n",
        COMMAND_ERROR);
  }
}
