/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/***************************************************************** 
*
*  File: exprint.c
*
*  Purpose: To print user commands and functions in algebraic form
*          
*/

#include "include.h" 
#include "lex.h"
#include "ytab.h"
  
void check_room_left ARGS((size_t));
void print_quote ARGS((char*));
void linebreak ARGS((void));
void newline ARGS((void));

struct locallist_t *current_proc_locals[100];
int current_proc_depth;

char * assign_symbol ARGS((int));
char * assign_symbol (sym)
int sym;
{ switch ( sym )
  { case ASSIGN_: return ":="; 
     case PLUSASSIGN_: return "+=";
     case SUBASSIGN_: return "-="; 
     case MULTASSIGN_: return "*=";
     case DIVASSIGN_: return "/=";
  }
  return ""; 
}

/*****************************************************************
*
*  Function print_express()
*
*  Purpose: print expression in algebraic format
*          Uses private working space, expanding as needed.
*          Returns pointer to working space.
*
*/

static char *pos;  /* current position in string */
static char vch;
static size_t  strsize;
static char *strstart;
static char *linestart;
static int  bracket_depth; 

/* precedence levels for knowing how to parenthesize */
#define PREC_POW      50
#define PREC_UMINUS   45
#define PREC_MUL      40
#define PREC_DIV      40
#define PREC_ADD      35
#define PREC_SUB      35
#define PREC_COMP     30
#define PREC_NOT      25
#define PREC_AND      20
#define PREC_OR       15
#define PREC_COND     13
#define PREC_ASSIGN   10
#define PREC_ARG       0

/**************************************************************************
*
* Function: print_express()
*
* Purpose: Convert parse tree for expression to ASCII string in 
*          character array pos.
*/

char *print_express(node,c)
struct expnode *node;    /* expression tree */
int c;     /* character for parameters */
{
  if ( !strstart )  /* for first time thru */
    { strsize = 200;
      strstart = my_list_calloc(1,strsize,ETERNAL_BLOCK);
    }
  else strstart[0] = '\0';
  linestart = pos = strstart;
  vch = (char)c;

  if ( !node || !node->root ) { strcpy(strstart,"{}"); return strstart;}

  bracket_depth = 0;
  current_proc_locals[0] = node->locals;
  current_proc_depth = 0;
  exprint_recur(node->root,0);

  return strstart;
}

/**********************************************************************
*
* Function: linebreak()
*
* Purpose: Insert a linebreak when the line gets too long.
*/

#define INDENT 2
#define GOODLEN 75
#define MINLEN  30
void linebreak()
{ int i;
  char *c,*cc;
  char *minline = linestart + MINLEN;
  int extra_indent = 0;

  if ( pos - linestart < GOODLEN ) return;

  cc = NULL;
 
  /* search for end of quote, starting at end so don't break quote */
  for ( c = pos-1 ; c != linestart ; c-- )
      if ( *c == '"' ) { cc = (c[1] == ';') ? c+1 : c; break; } 

  if ( cc == NULL ) /* search for end bracket */
     for ( c = linestart + GOODLEN ; c != minline ; c-- )
    if ( *c == '}' ) { cc = (c[1] == ';') ? c+1 : c; break; }
  if ( cc == NULL ) /* scan back for handy ';' */ 
    for ( c = linestart + GOODLEN ; c != minline ; c-- )
     if ( *c == ';' ) { cc = c;  break; }
  if ( cc == NULL ) /* just look for { */
     for ( c = linestart + GOODLEN ; c != minline ; c-- )
       if ( *c == '{' ) { cc = c; break; }
  if ( cc == NULL ) extra_indent = 2;
  if ( cc == NULL ) /* just look for space */
     for ( c = linestart + GOODLEN ; c != minline ; c-- )
       if ( *c == ' ' ) { cc = c; break; }
  if ( cc == NULL ) /* just look for comma */
     for ( c = linestart + GOODLEN ; c != minline ; c-- )
       if ( *c == ',' ) { cc = c; break; }
  if ( cc == NULL ) /* just look for = */
     for ( c = linestart + GOODLEN ; c != minline ; c-- )
       if ( *c == '=' ) { cc = c; break; }
  if ( cc == NULL ) /* just look for logic signs */
     for ( c = linestart + GOODLEN ; c != minline ; c-- )
       if ( (*c == '&') || (*c == '%') ) 
          { cc = c; break; }
  if ( cc == NULL ) /* just look for arithmetic signs */
   for ( c = linestart + GOODLEN ; c != minline ; c-- )
    if ( ((*c == '+') || (*c == '-')) &&  !(isalpha(c[-1]) && !isalpha(c[-2]))) 
          { cc = c; break; }              /* don't split scientific notation */
  if ( (cc == NULL) || (cc - linestart < GOODLEN/2) )
     for ( c = linestart + GOODLEN ; c != minline ; c-- )
       if ( ((*c == '*') || (*c == '/')) && (c[1] != '=') && (c[1] != '*') ) 
         { cc = c; break; }
  if ( cc == NULL ) /* just look for closing parenthesis */
     for ( c = linestart + GOODLEN ; c != minline ; c-- )
       if ( *c == ')' ) { cc = c; break; }
  if ( cc == NULL ) /* just look for opening parenthesis */
     for ( c = linestart + GOODLEN ; c != minline ; c-- )
       if ( *c == '(' ) { cc = c; break; }
 
  /* Have break, so do split, with *cc as last character on old line */
  if ( cc ) 
  { char *ch;
    int bd = bracket_depth;
    linestart = cc+3; 
     c = cc+1 ; while (*c == ' ' ) c++ ;  /* skip spaces */
     for ( ch = c ; ch < pos ; ch++ ) /* unwind bracket depth */
       if ( *ch == '{' ) bd--;
       else if ( *ch == '}' ) bd++;
     pos = cc + 3 + INDENT*bd + extra_indent + strlen(c);
     kb_memmove(cc+3+INDENT*bd+extra_indent,c,strlen(c));
     cc[1] = (char)(bd ? ' ' : '\\');
     cc[2] = '\n';
     for ( i = 0 ; i < INDENT*bd + extra_indent ; i++ ) cc[i+3] = ' ';
     *pos = 0;
  }
  else
  { if ( bracket_depth <= 0 ) *(pos++) = '\\';
    *(pos++) = '\n';
    linestart = pos;
    for ( i = 0 ; i < INDENT*bracket_depth ; i++ ) *(pos++) = ' ';
    *pos = 0;
  }
}

/***************************************************************************
*
* function:  newline()
*
* purpose:  Begin a new line, suitably indented.
*/
void newline()
{
  int i;
  *(pos++) = '\n';
  for ( i = 0 ; i < INDENT*bracket_depth ; i++ )
    *(pos++) = ' ';
  *pos = 0;
  linestart = pos;
}

/**********************************************************************
*
* Function: check_room_left()
* 
* Purpose: Keep print string from overflowing.
*/

void check_room_left(n)
size_t n;  /* room needed */
{
   /* check room remaining */
   if ( (pos + n - strstart) > (int)strsize )
    { size_t len = pos - strstart;
      size_t linespot = linestart - strstart;
      strstart = my_list_realloc(strstart,strsize + 1000 + n,ETERNAL_BLOCK); 
      strsize += 1000 + n;
      linestart = strstart + linespot;
      pos = strstart + len;
    }
}

void print_quote(c)
char *c;
{ check_room_left(strlen(c)+30);
  convert_string(c,pos);
  pos += strlen(pos);
}

/********************************************************************
*
* function: convert_string()
*
* purpose: convert string from internal to printable representation.
*         Converts to C escapes, encloses in quotes.
*/

void convert_string(c,p)
char *c;    /* source */
char *p; /* destination */
{
  /* convert to C escape sequences */
  *(p++) = '"';
  if ( c )
   for ( ; *c ; c++ )
    switch ( *c )
      { case '\n': *(p++) = '\\'; *(p++) = 'n'; break;
        case '\r': *(p++) = '\\'; *(p++) = 'r'; break;
        case '\t': *(p++) = '\\'; *(p++) = 't'; break;
        case '\b': *(p++) = '\\'; *(p++) = 'b'; break;
        case '"': *(p++) = '\\'; *(p++) = 'q'; break;
        case '\\': *(p++) = '\\'; *(p++) = '\\'; break;
        default: *(p++) = *c;
      }
  *(p++) = '"';
  *p = 0;
  return;
}   

/*************************************************************************
*
* function: exprint_recur()
*
* purpose: Convert node of parse tree to ASCII, recursively doing sons.
*/

void exprint_recur(node,prec_parent) 
struct treenode *node;
int prec_parent;  /* precedence level of parent, for parenthesizing */
{
  struct treenode *nn;
  struct extra *ex;
  struct locallist_t *localbase = current_proc_locals[current_proc_depth];

   check_room_left(1000); 
  /* insert some handy line breaks */
  if ( (pos - linestart > GOODLEN) /* && (pos[-2] != ';') */ ) 
    linebreak(); 
  switch ( node->type )
    { 
      case NOP_:  return;
      case NULLBLOCK_: sprintf(pos,"{}");pos+=2; return;
      case NULLCMD_:  return;

      case BREAKPOINT_:
         sprintf(pos,"breakpoint %s ",globals(node->op1.name_id)->name);
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         return;
         
      case UNSET_BREAKPOINT_:
         if ( node->left )
         { sprintf(pos,"unset breakpoint %s ",globals(node->op1.name_id)->name);
           pos += strlen(pos);
           exprint_recur(node+node->left,prec_parent);
         }
         else
         { sprintf(pos,"unset breakpoints");
           pos += strlen(pos);
         }
         return;
         
      case SUPPRESS_WARNING_:
         sprintf(pos,"suppress_warning "); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         return;

      case UNSUPPRESS_WARNING_:
         sprintf(pos,"unsuppress_warning "); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         return;

      case KEYLOGFILE_:
         sprintf(pos,"keylogfile "); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         return;

      case LOGFILE_:
         sprintf(pos,"logfile "); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         return;

      case CMDLIST_:
         exprint_recur(node+node->left,prec_parent);
         if ( node->right )
         {  sprintf(pos,"; "); pos += 2;
            newline();
            exprint_recur(node+node->right,prec_parent);
          }
         return;
     
      case BACKQUOTE_START_: return;
      case BACKQUOTE_END_:
         exprint_recur(node+node->right,prec_parent); /* left was dummy */
         return;

      case TEXT_SPOT_:
         exprint_recur(node+node->left,prec_parent); 
         *(pos++) = ','; 
         exprint_recur(node+node->right,prec_parent);
         return;

      case DISPLAY_TEXT_:
         sprintf(pos,"display_text("); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         *(pos++) = ','; 
         exprint_recur(node+node->right,prec_parent);
         *(pos++) = ')'; 
         return;

      case DELETE_TEXT_:
         sprintf(pos,"delete_text("); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         *(pos++) = ')'; 
         return;


      case ACOMMANDEXPR_:
         *(pos++) = '`';  /* surround with backquotes */
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos,";`");
         pos+=2; /* surround with backquotes; make sure of ; */
         if ( node->right )
          { sprintf(pos,", "); pos += 2;
            exprint_recur(node+node->right,prec_parent);
          }
         return;

      case ATTR_FUNCTION_END_:
         exprint_recur(node+node->left,prec_parent);  /* define part */
         strcat(pos," function {"); pos += strlen(pos);
         exprint_recur(node+node->right,prec_parent);  /* code part */
         strcat(pos," } "); pos += strlen(pos);
         return;
     
      case ATTR_FUNCTION_:
         exprint_recur(node+node->left,prec_parent);  /* define part */
         return;

      case WRAP_VERTEX_:
         strcat(pos,"wrap_vertex("); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos,","); pos++;
         exprint_recur(node+node->right,prec_parent);
         sprintf(pos,")"); pos++;
         return;

      case CREATE_VERTEX_:
         sprintf(pos,"new_vertex("); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos,")"); pos++;
         return;

      case CREATE_EDGE_:
         sprintf(pos,"new_edge("); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos,","); pos++;
         exprint_recur(node+node->right,prec_parent);
         sprintf(pos,")"); pos++;
         return;

      case CREATE_FACET_:
         sprintf(pos,"new_facet("); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos,")"); pos++;
         return;

      case CREATE_BODY_:
         sprintf(pos,"new_body"); pos += strlen(pos);
         return;

      case ELINDEX_:
         exprint_recur(node+node->left,prec_parent);
         if ( node->right )
         { *pos = '@'; pos++; *pos = 0;
            exprint_recur(node+node->right,prec_parent);
         }
         return;

      case PUSH_ELEMENT_ID_:
		  sprintf(pos,"%%sd@%d\n",(inverted(node->op1.id)?"-":""),
             node->op1.id & OFFSETMASK, id_task(node->op1.id));
         pos += strlen(pos);
         return;
         
      case VALID_ELEMENT_:
         sprintf(pos,"valid_element(%s[",typenames[node->op1.eltype]); 
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos,"])"); pos+=2;
         return;
   
      case VALID_CONSTRAINT_:
         sprintf(pos,"valid_constraint("); 
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos,")"); pos+=1;
         return;
   
      case VALID_BOUNDARY_:
         sprintf(pos,"valid_boundary("); 
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos,")"); pos+=1;
         return;
   
      case MATRIX_INVERSE_:
         sprintf(pos,"matrix_inverse(%s,%s)",
            globals(node->op1.name_id)->name,globals(node->op2.name_id)->name);
         pos += strlen(pos);
         return;

      case MATRIX_MULTIPLY_:
         sprintf(pos,"matrix_multiply(%s,%s,%s)",
            globals(node->op1.name_id)->name,globals(node->op2.name_id)->name,
            globals(node->op3.name_id)->name);
         pos += strlen(pos);
         return;

      case MATRIX_DETERMINANT_:
         sprintf(pos,"matrix_determinant(%s)",
            globals(node->op1.name_id)->name);
         pos += strlen(pos);
         return;

      case MERGE_VERTEX_:
         sprintf(pos,"vertex_merge("); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos,","); pos++;
         exprint_recur(node+node->right,prec_parent);
         sprintf(pos,")"); pos++;
         return;

      case MERGE_EDGE_:
         sprintf(pos,"edge_merge("); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos,","); pos++;
         exprint_recur(node+node->right,prec_parent);
         sprintf(pos,")"); pos++;
         return;

      case MERGE_FACET_:
         sprintf(pos,"facet_merge("); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos,","); pos++;
         exprint_recur(node+node->right,prec_parent);
         sprintf(pos,")"); pos++;
         return;

      case COMMAND_BLOCK_:
         sprintf(pos,"{ "); pos+=2; bracket_depth++;
         exprint_recur(node+node->left,prec_parent);
         bracket_depth--;
         newline();
         sprintf(pos,"}"); pos++;
         return;

      case LOCAL_LIST_START_:
         sprintf (pos,"local ");
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         break;

      case DECLARE_LOCAL_:
       { 
         if ( node->left )
         { 
           exprint_recur(node+node->left,0);
           *pos = ','; pos++;
         }
         sprintf (pos,"%s",globals(node->op1.name_id)->name);
         pos += strlen(pos);
         return;
       }
      
      case DEFINE_QUANTITY_:
       { struct gen_quant *g = GEN_QUANT(node->op1.quant_id);
         sprintf(pos,"/* Definition of quantity %s was originally here.*/",
            g->name);
         pos += strlen(pos);
         return;
       }

      case DEFINE_METHOD_INSTANCE_:
       { struct method_instance *mi = METH_INSTANCE(node->op1.meth_id);
         sprintf(pos,
           "/* Definition of method instance %s was originally here.*/",
            mi->name);
         pos += strlen(pos);
         return;
       }

      case DEFINE_CONSTRAINT_:
       { struct constraint *con = get_constraint(node->op1.con_id);
         if ( con->attr & NAMED_THING )
           sprintf(pos,
             "/* Definition of constraint %s was originally here.*/",
              con->name);
         else
           sprintf(pos,
             "/* Definition of constraint %d was originally here.*/",
              node->op1.con_id);
         pos += strlen(pos);
         return;
       }

      case DEFINE_BOUNDARY_:
       { struct boundary *bdry = web.boundaries+node->op1.bdry_id;
         if ( bdry->attr & NAMED_THING )
           sprintf(pos,
             "/* Definition of boundary %s was originally here.*/",
              bdry->name);
         else
           sprintf(pos,
             "/* Definition of boundary %d was originally here.*/",
              node->op1.con_id);
         pos += strlen(pos);
         return;
       }

      case DEFINE_EXTRA_:
         ex = EXTRAS(node->op2.eltype)+node->op1.extranum;
         sprintf(pos,"define %s attribute %s %s",
           typenames[node->op2.eltype],ex->name, datatype_name[ex->type]);
         pos += strlen(pos);
         if ( ex->code.start )
         { strcat(pos," function "); pos += strlen(pos);
           /*current_proc_locals[++current_proc_depth] = ex->locals;*/
           exprint_recur(ex->code.root,prec_parent);
           /*current_proc_depth--;*/
         }
         break;

      case DEFINE_EXTRA_INDEX_:
         exprint_recur(node+node->left,prec_parent);
         exprint_recur(node+node->right,prec_parent);
         break; 

      case DEFINE_ARRAY_: 
         sprintf(pos,"define %s %s",globals(node->op1.name_id)->name,
            datatype_name[node->op2.valtype]);
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         break;

      case DEFINE_FIXED_LOCAL_ARRAY_: 
        { struct global *g = globals(node->op1.name_id);
          struct array *a = g->attr.arrayptr;
          int i;

          sprintf(pos,"define %s %s",g->name,
            datatype_name[node->op2.valtype]);
          pos += strlen(pos);
          for ( i = 0 ; i < a->dim ; i++ )
          { sprintf(pos,"[%d]",a->sizes[i]);
            pos += strlen(pos);
          }
        }
         break;

      case ARRAY_EVAL_:
         exprint_recur(node+node->left,prec_parent);
         exprint_recur(node+node->right,prec_parent);
         break;

      case ARRAY_HEAD_:
         exprint_recur(node+node->left,prec_parent);
         break;

      case ARRAYASSIGN:
         sprintf(pos,"%s",globals(node->op2.name_id)->name);
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         switch ( node->op1.assigntype )
         { case ASSIGN_: sprintf(pos," := "); break;
           case PLUSASSIGN_: sprintf(pos," += "); break;
           case SUBASSIGN_: sprintf(pos," -= "); break;
           case MULTASSIGN_: sprintf(pos," *= "); break;
           case DIVASSIGN_: sprintf(pos," /= "); break;
         } 
         pos += strlen(pos);
         exprint_recur(node+node->right,prec_parent);
         break;

      case ARRAYEVAL:
         sprintf(pos,"%s",globals(node->op2.name_id)->name);
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         break;  

      /* whole-array syntax */
      case ARRAYIDENT_:
         sprintf(pos,"%s",get_name_name(node->op2.name_id,localbase));
         pos += strlen(pos);
         break;
      case ARRAY_ASSIGNOP_FACET_NORMAL_:
      case ARRAY_ASSIGNOP_EDGE_VECTOR_ :
      case ARRAY_ASSIGNOP_VERTEX_NORMAL_:
      case ARRAY_ASSIGNOP_ARRAY_ :
      case ARRAY_ASSIGNOP_SCALAR_ :
      case ARRAY_ASSIGNOP_S_X_A_:
      case ARRAY_ASSIGNOP_A_P_A_:
      case ARRAY_ASSIGNOP_A_S_A_:
         exprint_recur(node+node->left,prec_parent);
         if ( !(node->flags & SET_ASSIGNOP) )
         {
           switch ( node->op1.assigntype )
           { case ASSIGN_: sprintf(pos," := "); break;
             case PLUSASSIGN_: sprintf(pos," += "); break;
             case SUBASSIGN_: sprintf(pos," -= "); break;
             case MULTASSIGN_: sprintf(pos," *= "); break;
             case DIVASSIGN_: sprintf(pos," /= "); break;
           } 
           pos += strlen(pos);
         }
         exprint_recur(node+node->right,prec_parent);
         break;
      case ARRAY_ASSIGNOP_SINGLE_:
         exprint_recur(node+node->left,prec_parent);
         if ( !(node->flags & SET_ASSIGNOP) )
         {
           switch ( node->op1.assigntype )
           { case ASSIGN_: sprintf(pos," := "); break;
             case PLUSASSIGN_: sprintf(pos," += "); break;
             case SUBASSIGN_: sprintf(pos," -= "); break;
             case MULTASSIGN_: sprintf(pos," *= "); break;
             case DIVASSIGN_: sprintf(pos," /= "); break;
           } 
           pos += strlen(pos);
         }
         else 
         { *pos = '('; *(++pos) = 0; /* avoid - after ] */}
         exprint_recur(node+node->right,prec_parent);
         if ( node->flags & SET_ASSIGNOP )
         { *pos = ')'; *(++pos) = 0; }
         break;
      case DOT_:
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos," dot_product ");
         pos += strlen(pos);
         exprint_recur(node+node->right,prec_parent);
         break;
      case ARRAY_LVALUE_INDEXED_:
         exprint_recur(node+node->left,prec_parent);
         exprint_recur(node+node->right,prec_parent);
         *pos = ' '; *(++pos) = 0;
         break;
      case PRINT_ARRAY_LVALUE_:
         sprintf(pos,"print ");
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         break;
           
      case ATTRIB_LVALUE_:
      case ARRAY_VERTEX_NORMAL_:
      case ARRAY_EDGE_VECTOR_:
      case ARRAY_FACET_NORMAL_:
         if ( node->left )
         { exprint_recur(node+node->left,prec_parent);
         /*  sprintf(pos," %s",get_name_name(node->op2.name_id,localbase)); */
         }
         if ( node->op1.localnum == 0)
           sprintf(pos," %s",get_name_name(node->op2.name_id,localbase));
         else
           sprintf(pos,".%s",get_name_name(node->op2.name_id,localbase));
         pos += strlen(pos);
         break;
      case ARRAY_RVALUE_:
         exprint_recur(node+node->left,PREC_MUL);
         sprintf(pos," %c ",node->op1.intval);
         pos += strlen(pos);
         exprint_recur(node+node->right,prec_parent);
         break;

      /* end whole-array syntax */

      case SET_CONSTRAINT_GLOBAL:
         strcat(pos,"set constraint "); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         strcat(pos," global "); pos += strlen(pos);
         break;

      case UNSET_CONSTRAINT_GLOBAL:
         strcat(pos,"unset constraint "); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         strcat(pos," global "); pos += strlen(pos);
         break;

      case SET_CONSTRAINT_NAME_GLOBAL:
         sprintf(pos,"set constraint %s global",
              get_constraint(node->op3.connum)->name); 
         pos += strlen(pos);   
         break;

      case UNSET_CONSTRAINT_NAME_GLOBAL:
         sprintf(pos,"unset constraint %s global",
              get_constraint(node->op3.connum)->name); 
         pos += strlen(pos);
         break;

      case RESET_COUNTS_:
         strcat(pos,"reset_counts"); pos += strlen(pos); 
         return;

      case FLUSH_COUNTS_:
         strcat(pos,"flush_counts"); pos += strlen(pos); 
         return;

      case PRINT_PROFILING_:
         strcat(pos,"print profiling"); pos += strlen(pos); 
         return;

      case RESET_PROFILING_:
         strcat(pos,"reset_profiling"); pos += strlen(pos); 
         return;

      case PAUSE_:
         strcat(pos,"pause"); pos += strlen(pos); 
         return;

      case RETURN_:
         strcat(pos,"return "); pos += strlen(pos); 
         if ( node->left )
           exprint_recur(node+node->left,prec_parent);
         return;

      case DATE_AND_TIME_:
         strcat(pos,"date_and_time"); pos += strlen(pos); 
         return;

      case BREAK_:
         if ( node->op2.breakdepth > 1 )
         sprintf(pos,"break %d",node->op2.breakdepth);
         else strcat(pos,"break "); 
         pos += strlen(pos);
         return;

      case CONTINUE_:
         if ( node->op2.breakdepth > 1 )
         sprintf(pos,"continue %d",node->op2.breakdepth);
         else strcat(pos,"continue "); 
         pos += strlen(pos);
         return;

      case HISTORY_:
         strcat(pos,"history "); pos += strlen(pos);
         return;

      case GET_TRANSFORM_EXPR_:
         strcat(pos,"transform_expr "); pos += strlen(pos);
         return;

      case WARNING_MESSAGES_:
         strcat(pos,"warning_messages "); pos += strlen(pos);
         return;

      case DATAFILENAME_:
         strcat(pos,"datafilename "); pos += strlen(pos);
         return;

      case REPEAT_:
         exprint_recur(node+node->right,prec_parent);
         nn = node + node->left;
         exprint_recur(nn+nn->left,prec_parent);
         return;

      case EXPRLIST_:
         exprint_recur(node+node->left,prec_parent);
         if ( node->right )
          { sprintf(pos,", "); pos += 2;
            exprint_recur(node+node->right,prec_parent);
          }
         return;
     
      case QUOTATION_:
         print_quote(node->op1.string);
         return;

      case SPRINTFHEAD_:
      case PRESPRINTF_:
         sprintf(pos,"sprintf ");
         pos += strlen(pos);
         if ( node->op1.string ) print_quote(node->op1.string);
         else exprint_recur(node+node->left,prec_parent);
         return;

      case PREPRINTF_:
      case PRINTFHEAD_:
         sprintf(pos,"printf ");
         pos += strlen(pos);
         if ( node->op1.string ) print_quote(node->op1.string);
         else exprint_recur(node+node->left,prec_parent);
         return;

      case ERRPRINTFHEAD_:
         sprintf(pos,"errprintf ");
         pos += strlen(pos);
         if ( node->op1.string ) print_quote(node->op1.string);
         else exprint_recur(node+node->left,prec_parent);
         return;

      case BINARY_PRINTFHEAD_:
         sprintf(pos,"binary_printf ");
         pos += strlen(pos);
         if ( node->op1.string ) print_quote(node->op1.string);
         else exprint_recur(node+node->left,prec_parent);
         return;

      case PRINTF_:
      case BINARY_PRINTF_:
      case ERRPRINTF_:
      case SPRINTF_:
         exprint_recur(node+node->left,prec_parent);
         strcat(pos++,",");
         exprint_recur(node+node->right,prec_parent);
         return;

      case STRPRINT_: 
      case PRINT_: 
      case EPRINT_:
         sprintf(pos,"print "); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         return;
    
      case PRINT_LETTER_: 
         sprintf(pos,"print %c ",node->op1.name_id); 
         pos += strlen(pos);
         return;
    
      case PRINT_PROCEDURE_: 
      case PRINT_ARRAY_: 
         sprintf(pos,"print %s ",globals(node->op1.name_id)->name); 
         pos += strlen(pos);
         return;
    
      case PRINT_ARRAYPART_: 
         sprintf(pos,"print %s",globals(node[node->left].op1.name_id)->name);
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         return;
    
      case PRINT_VERTEXNORMAL_:
         sprintf(pos,"print ");
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         strcat(pos,".vertexnormal");
         pos += strlen(pos);
         return;

      case PRINT_ATTR_ARRAY_: 
         sprintf(pos,"print ");
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos,".%s",EXTRAS(node->op2.eltype)[node->op3.extranum].name);
         pos += strlen(pos);
         if ( node->right )
           exprint_recur(node+node->right,prec_parent);
         return;
    
      case PRINT_PERM_PROCEDURE_: 
         sprintf(pos,"print %s ",perm_globals(node->op1.name_id)->name); 
         pos += strlen(pos);
         return;
    
      case EXPRINT_PROCEDURE_: 
         sprintf(pos,"exprint %s ",globals(node->op1.name_id)->name); 
         pos += strlen(pos);
         return;
    
      case ELSE_: /* root of IF */
         exprint_recur(node+node->left,prec_parent); /* IF part */
         if ( node->right )
           { sprintf(pos," else "); pos += strlen(pos);
             newline();
             exprint_recur(node+node->right,prec_parent); /* command */
           }
         bracket_depth--;
         return;

      case IF_:
         exprint_recur(node+node->left,prec_parent); /* IF part */
         exprint_recur(node+node->right,prec_parent); /* command */
         return;
         
      case IFTEST_:
         sprintf(pos,"if ( "); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent); /* expr */
         sprintf(pos," ) then "); pos += strlen(pos);
         bracket_depth++;
         newline();
         return;

      case WHILE_END_:
         exprint_recur(node+node->left,prec_parent); /* test part */
         sprintf(pos," do "); pos += strlen(pos);
         bracket_depth++;
         if ( node->right )
         { newline();
           exprint_recur(node+node->right,prec_parent); /* command */
         } else
         { strcat(pos," ;"); pos += strlen(pos); }
         bracket_depth--;
         return;

      case WHILE_TOP_:
         sprintf(pos,"while ("); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent); /* expr */
         strcat(pos,") "); pos += strlen(pos);
         return;

      case DO_TOP_:
         exprint_recur(node+node->right,prec_parent); /* command */
         return;

      case DO_END_:
         sprintf(pos,"do "); pos += strlen(pos);
         bracket_depth++;
         newline();
         exprint_recur(node+node->left,prec_parent); /* command */
         bracket_depth--;
         newline();
         sprintf(pos," while ("); pos += strlen(pos);
         exprint_recur(node+node->right,prec_parent); /* expr */
         strcat(pos,") "); pos += strlen(pos);
         return;

      case FOR_END_:
         sprintf(pos,"for ( "); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent); /* FOR_TOP_ */
         sprintf(pos," ) "); pos += strlen(pos);
         bracket_depth++;
         newline();
         if ( node->right )
           exprint_recur(node+node->right,prec_parent); /* command3 */
         else { strcat(pos," ;") ; pos += strlen(pos); } /* empty command3 */
         bracket_depth--;
         newline();
         return;

      case FOR_TOP_:
         exprint_recur(node+node->left,prec_parent); /* FOR_HEAD_ */
         sprintf(pos," ; "); pos += strlen(pos);
         exprint_recur(node+node->right,prec_parent); /* command2 */
         return;

      case FOR_HEAD_:
         exprint_recur(node+node->left,prec_parent); /* FOR_ENTRY */
         sprintf(pos," ; "); pos += strlen(pos);
         exprint_recur(node+node->right,prec_parent); /* expr */
         return;

      case FOR_ENTRY_: 
         exprint_recur(node+node->left,prec_parent); /* command1 */
         return;

      case REDIRECT_:
         if ( node->left ) 
           { strcat(pos,">> "); pos += strlen(pos);
             exprint_recur(node+node->left,prec_parent); /* command */
           }
         else
           sprintf(pos,">> \"%s\" ",node->op1.string); pos += strlen(pos);
         return;

      case REDIRECTOVER_:
         if ( node->left ) 
           { strcat(pos,">>> "); pos += strlen(pos);
             exprint_recur(node+node->left,prec_parent); /* command */
           }
         else
           sprintf(pos,">>> \"%s\" ",node->op1.string); pos += strlen(pos);
         return;

      case PIPE_:
         if ( node->left ) 
           { strcat(pos,"| "); pos += strlen(pos);
             exprint_recur(node+node->left,prec_parent); /* command */
           }
         else
           sprintf(pos,"| \"%s\" ",node->op1.string); pos += strlen(pos);
         return;

      case PIPE_END_:
      case REDIRECT_END_:
         exprint_recur(node+node->right,prec_parent); /* command */
         exprint_recur(node+node->left,prec_parent); /* pipe */
         return;

      case SINGLE_REDEFD_:
         sprintf(pos,"%c ",node->op1.letter); 
         pos += strlen(pos);
         return;

      case SINGLE_LETTER_:
         if ( single_redefine[node->op1.letter].start )
            sprintf(pos,"'%c' ",node->op1.letter);
         else sprintf(pos,"%c ",node->op1.letter); 
         pos += strlen(pos);
         return;

    /* toggles */ case QUIETGO_: case QUIETLOAD_: case SLICE_VIEW_:
    case CLIP_VIEW_: case STAR_FINAGLING_: case FORCE_DELETION_:
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
    case QUANTITIES_ONLY_: case LINEAR_METRIC_:
    case SQUARED_GRADIENT_: case H_INVERSE_METRIC_:
    case HESSIAN_DOUBLE_NORMAL_: case INTERP_BDRY_PARAM_: 
    case HESSIAN_NORMAL_ONE_: case PSCOLORFLAG_: case GRIDFLAG_:
    case CROSSINGFLAG_: case LABELFLAG_: case SHOW_ALL_QUANTITIES_:
    case HESSIAN_NORMAL_PERP_: case HESSIAN_SPECIAL_NORMAL_: case ITDEBUG_:
    case METIS_FACTOR_: case VOLGRADS_EVERY_: case ZENER_DRAG_: 
    case BACKCULL_: case INTERP_NORMALS_: case TORUS_FILLED_: case VERBOSE_:
    case AMBIENT_PRESSURE_: case DIRICHLET_MODE_: case SOBOLEV_MODE_:
    case KRAYNIKPOPVERTEX_FLAG_: case FUNCTION_QUANTITY_SPARSE_:
    case KRAYNIKPOPEDGE_FLAG_: case VISIBILITY_TEST_: case SPARSE_CONSTRAINTS_:
    case BLAS_FLAG_: case AUGMENTED_HESSIAN_: case BREAK_AFTER_WARNING_:
    case RGB_COLORS_FLAG_:  case CIRCULAR_ARC_DRAW_: case BEZIER_BASIS_:
    case SMOOTH_GRAPH_: case MPI_DEBUG_: case POP_DISJOIN_:
    case POP_TO_EDGE_: case POP_TO_FACE_: case POP_ENJOIN_:
    case FULL_BOUNDING_BOX_: case BIG_ENDIAN_: case LITTLE_ENDIAN_:
    case AUTOPOP_QUARTIC_: case IMMEDIATE_AUTOPOP_:
         sprintf(pos,"%s %s ",keywordname(node->type),
          node->op1.toggle_state==ON_?"ON":"OFF"); pos += strlen(pos);
         break;

    case LOGFILE_TOGGLE_:
         sprintf(pos,"logfile %s ",
          node->op1.toggle_state==ON_?"ON":"OFF"); pos += strlen(pos);
         break;

    case KEYLOGFILE_TOGGLE_:
         sprintf(pos,"keylogfile %s ",
          node->op1.toggle_state==ON_?"ON":"OFF"); pos += strlen(pos);
         break;

    case GEOMVIEW_TOGGLE_:
         sprintf(pos,"geomview %s ",
          node->op1.toggle_state==ON_?"ON":"OFF"); pos += strlen(pos);
         break;

    case GEOMPIPE_TOGGLE_:
         sprintf(pos,"geompipe %s ",
          node->op1.toggle_state==ON_?"ON":"OFF"); pos += strlen(pos);
         break;

    case GEOMPIPE_:
         sprintf(pos,"geompipe "); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         break;

    case POSTSCRIPT_:
         sprintf(pos,"postscript "); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         break;

    case BINARY_OFF_FILE_:
         sprintf(pos,"binary_off_file "); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         break;

    case OOGLFILE_:
         sprintf(pos,"ooglfile "); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         break;

    case TOGGLEVALUE:
         sprintf(pos,"(%s) ",keywordname(node->op1.toggle_id)); 
         pos += strlen(pos);
         break;
    
    case GET_INTERNAL_:
         sprintf(pos,"%s",keywordname(node->op1.name_id));
         pos += strlen(pos);
         break;

    case PROCEDURE_:
         sprintf(pos,"%s ",globals(node->op1.name_id)->name);
         pos += strlen(pos);
         break;

    case PERM_PROCEDURE_:
         sprintf(pos,"%s ",perm_globals(node->op1.name_id)->name);
         pos += strlen(pos);
         break;

    case FIX_PARAMETER_:
          sprintf(pos,"fix %s",globals(node->op1.name_id)->name);
          pos += strlen(pos);
          break;

    case UNFIX_PARAMETER_:
          sprintf(pos,"unfix %s",globals(node->op1.name_id)->name);
          pos += strlen(pos);
          break;

    case FIX_QUANTITY_:
          sprintf(pos,"fix %s",GEN_QUANT(node->op1.quant_id)->name);
          pos += strlen(pos);
          break;

    case UNFIX_QUANTITY_:
          sprintf(pos,"unfix %s",GEN_QUANT(node->op1.quant_id)->name);
          pos += strlen(pos);
          break;

    case SET_Q_FIXED_:
          sprintf(pos,"set %s fixed",GEN_QUANT(node->op1.quant_id)->name);
          pos += strlen(pos);
          break;

    case SET_Q_ENERGY_:
          sprintf(pos,"set %s energy",GEN_QUANT(node->op1.quant_id)->name);
          pos += strlen(pos);
          break;

    case SET_Q_INFO_:
          sprintf(pos,"set %s info_only",GEN_QUANT(node->op1.quant_id)->name);
          pos += strlen(pos);
          break;

    case SET_Q_CONSERVED_:
          sprintf(pos,"set %s conserved",GEN_QUANT(node->op1.quant_id)->name);
          pos += strlen(pos);
          break;

    case SET_INTERNAL_:
         sprintf(pos,"%s %s ",keywordname(node->op1.name_id),
           assign_symbol(node->op2.assigntype));
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         break;

    case VIEW_MATRIX_LVALUE_:
         sprintf(pos,"view_matrix["); pos += strlen("view_matrix[");
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos,"]["); pos += 2;
         exprint_recur(node+node->right,prec_parent);
         sprintf(pos,"]"); pos += 1;
         break;

    case SET_VIEW_MATRIX_:
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos," := "); pos += 4;
         exprint_recur(node+node->right,prec_parent);
         break;

    case SET_SCALE_:
          sprintf(pos,"m "); pos += strlen(pos);
          exprint_recur(node+node->left,prec_parent);
          break;

    case SET_OPTIMIZE_:
          sprintf(pos,"optimize "); pos += strlen(pos);
          exprint_recur(node+node->left,prec_parent);
          break;

    case SET_FIXED_AREA_:
          sprintf(pos,"area_fixed := "); pos += strlen(pos);
          exprint_recur(node+node->left,prec_parent);
          break;

    case SET_GAP_CONSTANT_:
          sprintf(pos,"gap_constant := "); pos += strlen(pos);
          exprint_recur(node+node->left,prec_parent);
          break;

    case SKINNY_:
          sprintf(pos,"K "); pos += strlen(pos);
          exprint_recur(node+node->left,prec_parent);
          break;

    case TORDUP_:
          sprintf(pos,"y "); pos += strlen(pos);
          exprint_recur(node+node->left,prec_parent);
          break;

    case SET_MODEL_:
          sprintf(pos,"M "); pos += strlen(pos);
          exprint_recur(node+node->left,prec_parent);
          break;

    case INVOKE_P_MENU_:
          sprintf(pos,"P "); pos += strlen(pos);
          exprint_recur(node+node->left,prec_parent);
          break;

    case SET_GRAVITY_:
          switch ( node->op1.assigntype )
          { case ASSIGN_: sprintf(pos,"G "); break;
            case PLUSASSIGN_: sprintf(pos,"gravity += "); break;
            case SUBASSIGN_: sprintf(pos,"gravity -= "); break;
            case MULTASSIGN_: sprintf(pos,"gravity *= "); break;
            case DIVASSIGN_: sprintf(pos,"gravity /= "); break;
          } 
          pos += strlen(pos);
          exprint_recur(node+node->left,prec_parent);
          break;

    case SET_DIFFUSION_:
          sprintf(pos,"diffusion := "); pos += strlen(pos);
          exprint_recur(node+node->left,prec_parent);
          break;

    case SET_THICKEN_:
          sprintf(pos,"thicken := "); pos += strlen(pos);
          exprint_recur(node+node->left,prec_parent);
          break;

    case SET_AUTOCHOP_:
          sprintf(pos,"autochop := "); pos += strlen(pos);
          exprint_recur(node+node->left,prec_parent);
          break;

    case SET_AMBIENT_PRESSURE_:
          sprintf(pos,"p "); pos += strlen(pos);
          exprint_recur(node+node->left,prec_parent);
          break;

    case ZOOM_:
          sprintf(pos,"zoom "); pos+=strlen(pos);
          if ( node->left )
            { exprint_recur(node+node->left,prec_parent);
              exprint_recur(node+node->right,prec_parent);
            }
          break;

    case CHDIR_:
    case SYSTEM_:
    case EXEC_: case PARALLEL_EXEC_:
    case READ_:
    case LOAD_:
    case PERMLOAD_:
    case ADDLOAD_:
    case SHOW_TRANS_:
    case TRANSFORM_EXPR_:
    case GEOMVIEW_:
         sprintf(pos,"%s ",keywordname(node->type));
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         break;
 
    case TASK_EXEC_:
         sprintf(pos,"%s ",keywordname(node->type));
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         *pos = ','; pos++;
         exprint_recur(node+node->right,prec_parent);
         break;


      case VIEW_TRANSFORM_PARITY_:
         strcat(pos,"view_transform_parity["); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         strcat(pos,"]"); pos++;
         break;

      case VIEW_TRANSFORM_SWAP_COLORS_:
         strcat(pos,"view_transform_swap_colors["); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         strcat(pos,"]"); pos++;
         break;

      case VIEW_TRANSFORMS_NOP_:
         strcat(pos,"view_transforms["); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         strcat(pos,"]["); pos += 2;
         exprint_recur(node+node->right,prec_parent);
         break;

      case VIEW_TRANSFORMS_ELEMENT_:
         exprint_recur(node+node->left,prec_parent);
         strcat(pos,"]["); pos += 2;
         exprint_recur(node+node->right,prec_parent);
         *(pos++) = ']';
         break;

    case IS_DEFINED_:
         sprintf(pos,"is_defined(");
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         *(pos++) = ')';
         break;

    case DUMP_:
         sprintf(pos,"dump ");
         pos += strlen(pos);
         if ( node->left )
         exprint_recur(node+node->left,prec_parent);
         break;

    case SET_COLORMAP_:
         sprintf(pos,"colormap := "); 
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         break;

      case SHOW_VOL_:  case CHECK_: case LONG_JIGGLE_: case RAW_VERAVG_:
      case STABILITY_TEST_: case UTEST_: case GO_: case SHELL_: 
      case ALICE_:    case RECALC_: case COUNTS_: case RAWEST_VERAVG_:
      case EXTRAPOLATE_: case LINEAR_: case QUADRATIC_: case REBODY_:
      case HESSIAN_: case SHOWQ_: case CLOSE_SHOW_: case HESSIAN_MENU_:
      case DIRICHLET_: case SOBOLEV_: case REORDER_STORAGE_:
      case DIRICHLET_SEEK_: case SOBOLEV_SEEK_: case CONVERT_TO_QUANTS_:
      case RENUMBER_ALL_: case DUMP_MEMLIST_: case FREE_DISCARDS_:
      case REPARTITION_: case SUBCOMMAND_: case ABORT_:
      case SIMPLEX_TO_FE_:
         sprintf(pos,"%s ",keywordname(node->type)); pos+=strlen(pos);
         break;
             
      case BURCHARD_:
         sprintf(pos,"%s %d ",keywordname(node->type),node->op1.maxsteps); 
         pos+=strlen(pos);
         break;
             
      case SHOW_END_:
         if ( node[node->left].type == SHOW_ )
           sprintf(pos,"show_expr ");  /* prevent extraneous shows from dump */
         else sprintf(pos,"show_expr ");
         pos += strlen(pos);
         exprint_recur(node+node->right,prec_parent);
         break;

      case SET_PROC_END_:
       { struct global *g = globals(node[node->left].op1.name_id);
         if ( node[node->left].type == REDEFINE_SINGLE_ )
          sprintf(pos,"%c :::= ",node[node->left].op1.letter);
         else if ( g->flags & PERMANENT )
          sprintf(pos,"%s ::= ",g->name);
         else  sprintf(pos,"%s := ",g->name);
         pos += strlen(pos);
         if ( node[node->right].type != COMMAND_BLOCK_ )
         { strcat(pos,"{"); pos++; } 
         current_proc_locals[++current_proc_depth] = g->value.proc.locals;
         exprint_recur(node+node->right,prec_parent);
         current_proc_depth--;
         if ( node[node->right].type != COMMAND_BLOCK_ )
         { strcat(pos,"}"); pos++; } 
         break;
       }
      case SET_PERM_PROC_END_:
         sprintf(pos,"%s ::= ",perm_globals(node[node->left].op1.name_id)->name);
         pos += strlen(pos);
         if ( node[node->right].type != COMMAND_BLOCK_ )
         { strcat(pos,"{"); pos++; } 
         exprint_recur(node+node->right,prec_parent);
         if ( node[node->right].type != COMMAND_BLOCK_ )
         { strcat(pos,"}"); pos++; } 
         break;

      case ARGLIST_:
       { struct global *g;
         if ( node->op1.name_id == 0 ) break; /* empty list */
         g = globals(node->op1.name_id);
         if ( node->left )
         { exprint_recur(node+node->left,prec_parent); /* arglist */
           strcat(pos,","); pos++;
         }
         sprintf(pos,"%s %s",datatype_name[node->op3.argtype],g->name);
         pos += strlen(pos);
         break;
       }

      case FUNCTION_CALL_:
       { struct global *g = globals(node->op1.name_id);
         sprintf(pos,"%s(",g->name);
         pos += strlen(pos);  
         if ( node->left )
           exprint_recur(node+node->left,prec_parent); /* arglist */
         strcat(pos,")"); pos++;
         break;
       }
      case FUNCTION_CALL_RETURN_:
         exprint_recur(node+node->left,prec_parent); /* FUNCTION_CALL_ */
         break;
      case FUNCTION_START_: break;
      case FUNCTION_DEF_START_: break;
      case FUNCTION_PROTO_START_: break;

      case FUNCTION_HEAD_:
         exprint_recur(node+node->right,prec_parent); /* arglist */
         break;

      case SET_FUNCTION_:
      case FUNCTION_PROTO_:
       { struct global *g = globals(node[node->left].op1.name_id);
         sprintf(pos,"function %s %s (",datatype_name[node->op4.ret_type],
                g->name);
         pos += strlen(pos);
         current_proc_locals[++current_proc_depth] = g->value.proc.locals;
         exprint_recur(node+node->left,prec_parent); /* arglist */
         if ( node->type == SET_FUNCTION_ )
         { strcat(pos,")\n"); pos += strlen(pos);
           exprint_recur(node+node->right,prec_parent); /* body */
         }
         else { strcat(pos,");\n"); pos += strlen(pos); }
         current_proc_depth--;
         break;
       }
      case PROCEDURE_CALL_:
         sprintf(pos,"%s(",globals(node->op1.name_id)->name);
         pos += strlen(pos); 
         if ( node->left ) 
            exprint_recur(node+node->left,prec_parent); /* arglist */
         strcat(pos,")"); pos++;
         break;
      case PROCEDURE_CALL_RETURN_:
         exprint_recur(node+node->left,prec_parent); /* PROCEDURE_CALL_ */
         break;

      case PROCEDURE_START_: break;
      case PROCEDURE_DEF_START_: break;
      case PROCEDURE_PROTO_START_: break;

      case PROCEDURE_HEAD_:
         exprint_recur(node+node->right,prec_parent); /* arglist */
         break;

      case SET_ARGSPROC_:
      case PROCEDURE_PROTO_:
       { struct global *g = globals(node[node->left].op1.name_id);
         sprintf(pos,"procedure %s (",g->name);
         pos += strlen(pos);
         current_proc_locals[++current_proc_depth] = g->value.proc.locals;
         exprint_recur(node+node->left,prec_parent); /* arglist */
         if ( node->type == SET_ARGSPROC_ )
         { strcat(pos,")\n"); pos += strlen(pos);
           exprint_recur(node+node->right,prec_parent); /* body */
         }
         else { strcat(pos,");\n"); pos += strlen(pos); }
         current_proc_depth--;
         break;
       }

      case DEFINE_IDENT_:
         sprintf(pos,"define %s %s",globals(node->op1.name_id)->name,
              datatype_name[node->op2.valtype]);
         pos += strlen(pos);
         break;

    case SET_DELTA_:
          sprintf(pos,"%s.pdelta",globals(node->op1.name_id)->name);
          pos += strlen(pos);
          switch ( node->op2.assigntype )
          { case ASSIGN_: sprintf(pos," := "); break;
            case PLUSASSIGN_: sprintf(pos," += "); break;
            case SUBASSIGN_: sprintf(pos," -= "); break;
            case MULTASSIGN_: sprintf(pos," *= "); break;
            case DIVASSIGN_: sprintf(pos," /= "); break;
          } 
          pos += strlen(pos);
          exprint_recur(node+node->left,prec_parent);
          break;

    case SET_PARAM_SCALE:
          sprintf(pos,"%s.pscale",globals(node->op1.name_id)->name);
          pos += strlen(pos);
          switch ( node->op2.assigntype )
          { case ASSIGN_: sprintf(pos," := "); break;
            case PLUSASSIGN_: sprintf(pos," += "); break;
            case SUBASSIGN_: sprintf(pos," -= "); break;
            case MULTASSIGN_: sprintf(pos," *= "); break;
            case DIVASSIGN_: sprintf(pos," /= "); break;
          } 
          pos += strlen(pos);
          exprint_recur(node+node->left,prec_parent);
          break;

      case SET_GLOBAL_: case SET_SGLOBAL_:
         { struct global *g = globals(node->op1.name_id);
           if ( g->flags & PERMANENT )
           sprintf(pos,"%s ::= ",g->name);
           else sprintf(pos,"%s := ",g->name);
           pos += strlen(pos);
           exprint_recur(node+node->left,prec_parent);
           break;
         }
      case SET_PERM_GLOBAL_: case SET_PERM_SGLOBAL_:
         sprintf(pos,"%s ::= ",globals(node->op1.name_id)->name);
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         break;

      case PLUSASSIGN_:
         sprintf(pos,"%s += ",globals(node->op1.name_id)->name);
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         break;

      case SUBASSIGN_:
         sprintf(pos,"%s -= ",globals(node->op1.name_id)->name);
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         break;

      case MULTASSIGN_:
         sprintf(pos,"%s *= ",globals(node->op1.name_id)->name);
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         break;

      case DIVASSIGN_:
         sprintf(pos,"%s /= ",globals(node->op1.name_id)->name);
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         break;

      case SIZEOF_ATTR_:
         sprintf(pos,"sizeof(%s)",
           EXTRAS(node->op2.eltype)[node->op1.extranum].name); 
         pos += strlen(pos);
         break;

      case SIZEOF_ARRAY_:
         sprintf(pos,"sizeof(%s)",globals(node->op1.name_id)->name);
         pos += strlen(pos);
         break;

      case SIZEOF_STRING_:
         sprintf(pos,"sizeof(");
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         strcat(pos,")");
         pos++;
         break;

      case LAGRANGE_:
         sprintf(pos,"%s ",keywordname(node->type)); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         return;

      case LANCZOS_:
      case EIGENPROBE_: 
         sprintf(pos,"%s ",keywordname(node->type)); pos += strlen(pos);
         if ( node->right )
         { strcat(pos,"("); pos++;
           exprint_recur(node+node->left,prec_parent);
           strcat(pos,","); pos++;
           exprint_recur(node+node->right,prec_parent);
           strcat(pos,")"); pos++;
         }
         else exprint_recur(node+node->left,prec_parent);
         return;

      case RITZ_:
         sprintf(pos,"%s(",keywordname(node->type)); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         strcat(pos,","); pos++;
         exprint_recur(node+node->right,prec_parent);
         strcat(pos,")"); pos++;
         return;

      case GET_TORUS_PERIODS_:
         sprintf(pos,"torus_periods"); pos += strlen(pos);
         strcat(pos++,"[");
         exprint_recur(node+node->left,prec_parent);
         strcat(pos,"]["); pos+=2;
         exprint_recur(node+node->right,prec_parent);
         strcat(pos++,"]");
         return;

      case GET_INVERSE_PERIODS_:
         sprintf(pos,"inverse_periods"); pos += strlen(pos);
         strcat(pos++,"[");
         exprint_recur(node+node->left,prec_parent);
         strcat(pos,"]["); pos+=2;
         exprint_recur(node+node->right,prec_parent);
         strcat(pos++,"]");
         return;

      case HESSIAN_SADDLE_:
      case HESSIAN_SEEK_:
         sprintf(pos,"%s ",keywordname(node->type)); pos += strlen(pos);
         if ( node->left )
           exprint_recur(node+node->left,prec_parent);
         return;

      case MOVE_:
      case AREAWEED_:
      case EDGEWEED_:
      case METIS_:
      case METIS_READJUST_:
      case KMETIS_:
      case BODY_METIS_:
      case NOTCH_:
      case EDGEDIVIDE_:
         sprintf(pos,"%s ",keywordname(node->type)); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         return;

      case OMETIS_:
         sprintf(pos,"%s ",keywordname(node->type)); pos += strlen(pos);
         if ( node->left )
           exprint_recur(node+node->left,prec_parent);
         return;

      case JIGGLE_:
         strcat(pos,"j "); pos += 2;
         exprint_recur(node+node->left,prec_parent);
         return;

      case LIST_PROCS_:
         sprintf(pos,"list procedures "); pos+=strlen(pos);
         break;
             
      case LIST_ATTRIBUTES_:
         sprintf(pos,"list attributes "); pos+=strlen(pos);
         break;

      case LIST_QUANTITY_:
         sprintf(pos,"list quantity %s",GEN_QUANT(node->op1.quant_id)->name);
         pos += strlen(pos);
         break;

      case LIST_METHOD_INSTANCE_:
         sprintf(pos,"list method_instance %s",
              METH_INSTANCE(node->op1.meth_id)->name);
         pos += strlen(pos);
         break;
             
      case LIST_CONSTRAINT_:
         {
           if ( node->op1.con_id > 0 )
           { sprintf(pos,"list constraint %s",
                get_constraint(node->op1.con_id)->name);
             pos += strlen(pos);
           }
           else
           { sprintf(pos,"list constraint ");
             pos += strlen(pos);
             exprint_recur(node+node->left,prec_parent);
           }
         }    
         break;

      case LIST_BOUNDARY_:
         { 
           if ( node->op1.bdry_id > 0 )
           { sprintf(pos,"list constraint %s",
                web.boundaries[node->op1.bdry_id].name);
             pos += strlen(pos);
           }
           else
           { sprintf(pos,"list boundary ");
             pos += strlen(pos);
             exprint_recur(node+node->left,prec_parent);
           }
         }    
         break;

      case TOPINFO_:
         sprintf(pos,"list topinfo "); pos+=strlen(pos);
         break;
             
      case BOTTOMINFO_:
         sprintf(pos,"list bottominfo "); pos+=strlen(pos);
         break;
             
      case AGGREGATE_END_:
         exprint_recur(node+node->right,prec_parent);
         return;

      case LIST_: case DELETE_: case REFINE_: case DISSOLVE_: case POP_:
      case FIX_: case UNFIX_: case EDGESWAP_: case VERTEX_AVERAGE_:
      case RAW_VERTEX_AVERAGE_: case RAWEST_VERTEX_AVERAGE_:
      case EQUIANGULATE_: case POP_EDGE_TO_TRI_: case POP_TRI_TO_EDGE_:
      case POP_QUAD_TO_QUAD_: case T1_EDGESWAP_:
      case REVERSE_ORIENTATION_:
      case WHEREAMI_COMMAND_:
         sprintf(pos,"%s ",keywordname(node->type));
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         return;

      case SINGLE_ELEMENT_INIT_:
         return;

      case SINGLE_ELEMENT_:
         exprint_recur(node+node->left,prec_parent);
         if ( node[node->left].type == INDEXED_SUBTYPE_ ||
           node[node->left].type == INDEXED_ELEMENT_ )
           if ( node[node->left].op5.string )
             { sprintf(pos," %s ",node[node->left].op5.string);
               pos += strlen(pos);
             }      
         return;

      case GET_VERTEXNORMAL_:
         strcat(pos,"vertexnormal"); pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         return;


      case INDEXED_COORD_:
         strcat(pos,"x"); pos += 1;
         exprint_recur(node+node->left,prec_parent);
         return;

      case INDEXED_ELEMENT_:
         switch ( node->op1.eltype )
         { case VERTEX: strcat(pos,"vertex["); break;
           case EDGE: strcat(pos,"edge["); break;
           case FACET: strcat(pos,"facet["); break;
           case BODY: strcat(pos,"body["); break;
           case FACETEDGE: strcat(pos,"facetedge["); break;
         }
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         strcat(pos++,"]");
         return;

      case INDEXED_SUBTYPE_:
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos,".%s[",typenames[node->op1.eltype]);
         pos += strlen(pos);
         exprint_recur(node+node->right,prec_parent);
         strcat(pos,"]");
         pos += strlen(pos);
         return;

      case SELF_ELEMENT_:
         strcat(pos,"self"); pos+=4; break;

      case SINGLE_ELEMENT_EXPR_:
         exprint_recur(node+node->left,prec_parent);
         break;

      case SYMBOL_:
      case SYMBOL_ELEMENT_:
         sprintf(pos,"%s", node->op5.string);
         pos += strlen(pos); break;

      case SET_PHASE_:
         set_print(node,"set","phase",prec_parent); break;

      case SET_DENSITY_:
         set_print(node,"set","density",prec_parent); break;

      case SET_VOLCONST_:
         set_print(node,"set","volconst",prec_parent); break;

      case SET_TARGET_:
         set_print(node,"set","target",prec_parent); break;

      case SET_PRESSURE_:
         set_print(node,"set","pressure",prec_parent); break;

      case SET_OPACITY_:
         set_print(node,"set","opacity",prec_parent); break;

      case SET_CONSTRAINT_:
         set_print(node,"set","constraint",prec_parent); break;

      case SET_CONSTRAINT_NAME:
         set_print(node,"set","constraint ",prec_parent); 
         strcat(pos,get_constraint((int)(node[node->left].op1.real))->name);
         pos += strlen(pos); break;

      case SET_NAMED_QUANTITY_:
         set_print(node,"set","quantity",prec_parent); break;

      case UNSET_NAMED_QUANTITY_:
         set_print(node,"unset","quantity",prec_parent); break;

      case SET_METHOD_INSTANCE_:
         set_print(node,"set","method_instance",prec_parent); break;

      case UNSET_METHOD_INSTANCE_:
         set_print(node,"unset","method_instance",prec_parent); break;

      case SET_EXTRA_ATTR_:
            { ex = EXTRAS(node->op3.extra_info >> ESHIFT)
                        +(node->op3.extra_info & 0xFF);
              set_print(node,"set",ex->name,prec_parent);
              if ( node->right ) 
                exprint_recur(node+node->right,prec_parent);
            }
         break;

      case SET_COLOR_:
         set_print(node,"set","color",prec_parent); break;

      case SET_FRONTCOLOR_:
         set_print(node,"set","frontcolor",prec_parent); break;

      case SET_BACKCOLOR_:
         set_print(node,"set","backcolor",prec_parent); break;

      case SET_TAG_:
         set_print(node,"set","tag",prec_parent); break;

      case SET_BACKGROUND_:
         set_print(node,"set","background",prec_parent); break;

      case SET_COORD_:
         { char cname[10];
           if ( (vch == 'X') && (node->op2.coordnum+1 <= 3) )
             sprintf(cname,"%c",'x'+node->op2.coordnum);
           else
             sprintf(cname,"%c%d",vch,node->op2.coordnum+1);
           set_print(node,"set",cname,prec_parent);
         }
         break;

      case SET_PARAM_:
         { char cname[10];
           sprintf(cname,"P%d",node->op2.coordnum+1);
           set_print(node,"set",cname,prec_parent);
         }
         break;

      case UNSET_FIXED_:
         set_print(node,"unset","fixed",prec_parent); break;

      case SET_FIXED_:
         set_print(node,"set","fixed",prec_parent); break;

      case UNSET_BARE_:
         set_print(node,"unset","bare",prec_parent); break;

      case SET_BARE_:
         set_print(node,"set","bare",prec_parent); break;

      case UNSET_NO_DISPLAY_:
         set_print(node,"unset","no_display",prec_parent); break;

      case SET_NO_DISPLAY_:
         set_print(node,"set","no_display",prec_parent); break;

      case UNSET_NONCONTENT_:
         set_print(node,"unset","noncontent",prec_parent); break;

      case SET_NONCONTENT_:
         set_print(node,"set","noncontent",prec_parent); break;

      case UNSET_HIT_PARTNER_:
         set_print(node,"unset","hit_partner",prec_parent); break;

      case SET_HIT_PARTNER_:
         set_print(node,"set","hit_partner",prec_parent); break;

      case UNSET_NO_REFINE_:
         set_print(node,"unset","no_refine",prec_parent); break;

      case SET_NO_REFINE_:
         set_print(node,"set","no_refine",prec_parent); break;

      case UNSET_TRIPLE_PT_:
         set_print(node,"unset","triple_point",prec_parent); break;

      case SET_ORIENTATION_:
         set_print(node,"set","orientation",prec_parent); break;

      case UNSET_TETRA_PT_:
         set_print(node,"unset","tetra_point",prec_parent); break;

      case UNSET_AXIAL_POINT_:
         set_print(node,"unset","axial_point",prec_parent); break;

      case UNSET_FACET_BODY_:
         set_print(node,"unset","body",prec_parent); break;

      case SET_FRONTBODY_:
         set_print(node,"set","frontbody",prec_parent); break;

      case UNSET_FRONTBODY_:
         set_print(node,"unset","frontbody",prec_parent); break;

      case SET_BACKBODY_:
         set_print(node,"set","backbody",prec_parent); break;

      case UNSET_BACKBODY_:
         set_print(node,"unset","backbody",prec_parent); break;

      case UNSET_DENSITY_:
         set_print(node,"unset","density",prec_parent); break;

      case UNSET_PRESSURE_:
         set_print(node,"unset","pressure",prec_parent); break;

      case UNSET_VOLUME_:
      case UNSET_TARGET_:
         set_print(node,"unset","target",prec_parent); break;

      case UNSET_CONSTRAINT_:
         set_print(node,"unset","constraint",prec_parent); 
         break;

      case UNSET_CONSTRAINT_NAME:
       { char temp[100];
         sprintf(temp,"constraint %s",get_constraint(node->op3.connum)->name);
         set_print(node,"unset",temp,prec_parent); 
         pos += strlen(pos);
         break;
       }

      case UNSET_BOUNDARY_:
         set_print(node,"unset","boundary",prec_parent); break;

      case UNSET_BOUNDARY_NAME:
       { char temp[100];
         sprintf(temp,"boundary %s",web.boundaries[node->op3.bdrynum].name);
         set_print(node,"unset",temp,prec_parent); 
         pos += strlen(pos);
         break;
       }
      case SET_INIT_ : break;

      case SET_ATTRIBUTE_LOOP_:
      { struct treenode *nnode;

         sprintf(pos," set ");
         pos += strlen(pos);
         nnode = node + node->left;
         if ( nnode->type == WHERE_ ) nnode += nnode->left; /* get NEXT_ */
         exprint_recur(nnode,prec_parent);

         if ( node->right )  exprint_recur(node+node->right,prec_parent);
         if ( node[node->left].type == WHERE_ )
           { node+= node->left;
             sprintf(pos," where "); pos += strlen(pos);
             exprint_recur(node+node->right,prec_parent);
           }
       }
       return;

      case FOREACH_:
         sprintf(pos,"foreach ");
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos," do "); 
         pos += strlen(pos);
         bracket_depth++;
         newline();
         exprint_recur(node+node->right,prec_parent);
         bracket_depth--;
         return;

      case MAX_: case MIN_: case SUM_: case AVG_: case COUNT_:
      case HISTOGRAM_: case LOGHISTOGRAM_:
         sprintf(pos,"%s(",keywordname(node->type));
         pos += strlen(pos);
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos,","); pos++;
         exprint_recur(node+node->right,prec_parent);
         sprintf(pos,")"); pos++;
         return;

      case WHERE_:
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos," where "); pos += strlen(pos);
         exprint_recur(node+node->right,prec_parent);
         return;

      case NEXT_VERTEX_: case NEXT_EDGE_VERTEX_: case NEXT_FACET_VERTEX_:
      case NEXT_BODY_VERTEX_: case NEXT_FACETEDGE_:
      case NEXT_EDGE_: case NEXT_VERTEX_EDGE_: case NEXT_FACET_EDGE_:
      case NEXT_FACET_: case NEXT_VERTEX_FACET_: case NEXT_EDGE_FACET_:
      case NEXT_BODY_: case NEXT_VERTEX_BODY_: case NEXT_EDGE_BODY_:
      case NEXT_FACET_BODY_:
      case NEXT_BODY_FACET_:
      case NEXT_BODY_EDGE_:
         exprint_recur(node+node->left,prec_parent);
         if ( strcmp(node->op5.string,default_name) != 0 )
           { sprintf(pos,"%s ",node->op5.string);
             pos += strlen(pos);
           } 
         return; 
      
      case INIT_FACETEDGE_: 
         sprintf(pos,"facetedges "); pos += strlen(pos);
         return;
      
      case INIT_VERTEX_: 
         sprintf(pos,"vertices "); pos += strlen(pos);
         return;
      
      case INIT_EDGE_VERTEX_: case INIT_FACET_VERTEX_: case INIT_BODY_VERTEX_:
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos,".vertices "); 
         pos += strlen(pos);
         return;
      
      case INIT_EDGE_: 
         sprintf(pos,"edges "); pos += strlen(pos);
         return;
      
      case INIT_VERTEX_EDGE_: case INIT_FACET_EDGE_: case INIT_BODY_EDGE_:
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos,".edges "); 
         pos += strlen(pos);
         return;
      
      case INIT_FACET_: 
         sprintf(pos,"facets "); pos += strlen(pos);
         return;
      
      case INIT_VERTEX_FACET_: case INIT_EDGE_FACET_: case INIT_BODY_FACET_:
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos,".facets "); 
         pos += strlen(pos);
         return;
      
      case INIT_BODY_: 
         sprintf(pos,"bodies "); pos += strlen(pos);
         break;

      case INIT_VERTEX_BODY_: case INIT_EDGE_BODY_: case INIT_FACET_BODY_:
         exprint_recur(node+node->left,prec_parent);
         sprintf(pos,".bodies "); 
          pos += strlen(pos);
         return;
      
      case PUSH_NAMED_QUANTITY:
         sprintf(pos,"%s ",GEN_QUANT(node->op1.quant_id)->name);
         pos += strlen(pos);
         return;

      case PUSH_METHOD_INSTANCE_:
         sprintf(pos,"%s ",METH_INSTANCE(node->op1.meth_id)->name);
         pos += strlen(pos);
         return;

      case PUSHCONST:
#ifdef LONGDOUBLE
         sprintf(pos,"%1.*Lg",DPREC,node->op1.real);
#else
         sprintf(pos,"%1.15g",node->op1.real);
#endif 
         pos += strlen(pos);
         return;

      case PUSHPI:
         sprintf(pos,"pi");
         pos += strlen(pos);
         return;

      case PUSHE:
         sprintf(pos,"e");
         pos += strlen(pos);
         return;

      case PUSHG:
         sprintf(pos,"G");
         pos += strlen(pos);
         return;

      case COORD_:
         if ( node->left )
         { exprint_recur(node+node->left,PREC_COND);
           sprintf(pos,"."); pos += strlen(pos);
         }
         if ( (vch == 'X') && (node->op2.coordnum+1 <= 3) )
           sprintf(msg,"%c",'x'+node->op2.coordnum);
         else
           sprintf(msg,"%c%d",vch,node->op2.coordnum+1);
         print_attr(node,msg);
         return;

      case PARAM_:
         sprintf(msg,"P%d",node->op2.coordnum+1);
         print_attr(node,msg);
         return;

      case PUSHPARAM:
	     if ( (vch == 'X') && (node->op1.coordnum+1 <= 3) )
           sprintf(msg,"%c",'x'+node->op1.coordnum);
         else
		   sprintf(msg,"%c%d",vch,node->op1.coordnum+1);
         print_attr(node,msg);
         return;

    case SET_MMODULUS_:
          sprintf(pos,"%s.modulus %s ",METH_INSTANCE(node->op1.meth_id)->name,
              assign_symbol(node->op2.assigntype)); 
          pos += strlen(pos);
          exprint_recur(node+node->left,prec_parent);
          break;

    case SET_QMODULUS_:
          sprintf(pos,"%s.modulus %s ",GEN_QUANT(node->op1.quant_id)->name,
          assign_symbol(node->op2.assigntype)); 
          pos += strlen(pos);
          exprint_recur(node+node->left,prec_parent);
          break;

    case SET_QTARGET_:
          sprintf(pos,"%s.target %s ",GEN_QUANT(node->op1.quant_id)->name,
          assign_symbol(node->op2.assigntype)); 
          pos += strlen(pos);
          exprint_recur(node+node->left,prec_parent);
          break;

    case SET_QVOLCONST_:
          sprintf(pos,"%s.volconst %s ",GEN_QUANT(node->op1.quant_id)->name,
          assign_symbol(node->op2.assigntype)); 
          pos += strlen(pos);
          exprint_recur(node+node->left,prec_parent);
          break;

    case SET_QTOLERANCE_:
          sprintf(pos,"%s.tolerance %s ",GEN_QUANT(node->op1.quant_id)->name,
          assign_symbol(node->op2.assigntype)); 
          pos += strlen(pos);
          exprint_recur(node+node->left,prec_parent);
          break;

      case PUSHQFIXED_:
         sprintf(pos,"%s.fixed",GEN_QUANT(node->op1.quant_id)->name);
         pos += strlen(pos);
         return;

      case PUSHQENERGY_:
         sprintf(pos,"%s.energy",GEN_QUANT(node->op1.quant_id)->name);
         pos += strlen(pos);
         return;

      case PUSHQINFO_ONLY_:
         sprintf(pos,"%s.info_only",GEN_QUANT(node->op1.quant_id)->name);
         pos += strlen(pos);
         return;

      case PUSHQCONSERVED_:
         sprintf(pos,"%s.conserved",GEN_QUANT(node->op1.quant_id)->name);
         pos += strlen(pos);
         return;

      case PUSHQPRESSURE_:
         sprintf(pos,"%s.pressure",GEN_QUANT(node->op1.quant_id)->name);
         pos += strlen(pos);
         return;

      case PUSHQTARGET_:
         sprintf(pos,"%s.target",GEN_QUANT(node->op1.quant_id)->name);
         pos += strlen(pos);
         return;

      case PUSHMVALUE_:
         sprintf(pos,"%s.value",METH_INSTANCE(node->op1.meth_id)->name);
         pos += strlen(pos);
         return;

      case PUSHQVALUE_:
         sprintf(pos,"%s.value",GEN_QUANT(node->op1.quant_id)->name);
         pos += strlen(pos);
         return;

      case PUSHMMODULUS_:
         sprintf(pos,"%s.modulus",GEN_QUANT(node->op1.meth_id)->name);
         pos += strlen(pos);
         return;

      case PUSHQMODULUS_:
         sprintf(pos,"%s.modulus",GEN_QUANT(node->op1.quant_id)->name);
         pos += strlen(pos);
         return;

      case PUSHQVOLCONST_:
         sprintf(pos,"%s.volconst",globals(node->op1.quant_id)->name);
         pos += strlen(pos);
         return;

      case PUSHDELTA_:
         sprintf(pos,"%s.pdelta",globals(node->op1.name_id)->name);
         pos += strlen(pos);
         return;

      case PUSH_PARAM_SCALE:
         sprintf(pos,"%s.pscale",globals(node->op1.name_id)->name);
         pos += strlen(pos);
         return;

      case PUSH_PARAM_FIXED:
         sprintf(pos,"%s.fixed",globals(node->op1.name_id)->name);
         pos += strlen(pos);
         return;

      case PUSH_PARAM_EXTRA_:
         sprintf(pos,"%s.%s",globals(node->op1.name_id)->name,
              EXTRAS(0)[node->op2.extranum].name);
         pos += strlen(pos);
         return;

      case ELEMENT_IDENT_:
         { struct global *g = globals(node->op3.name_id); 
           sprintf(pos,"%s",g->name);
           pos += strlen(pos);
           return;
         }
      case PUSHGLOBAL_:
      case STRINGGLOBAL_:
       { struct global *g = globals(node->op1.name_id);
         if ( g->flags & QUANTITY_NAME )
           sprintf(pos,"total %s",g->name);
         else sprintf(pos,"%s",g->name);
         pos += strlen(pos);
         return;
       }
      case PUSH_PERM_GLOBAL_:
      case PERM_STRINGGLOBAL_:
         sprintf(pos,"%s",perm_globals(node->op1.name_id)->name);
         pos += strlen(pos);
         return;

      case USERFUNC:
         sprintf(pos,"usr%d",node->op1.userfunc+1);
         pos += strlen(pos);
         return;

      case DYNAMIC_LOAD_FUNC_:
         sprintf(pos,globals(node->op2.name_id)->name);
         pos += strlen(pos);
         return;

      case PLUS:
         binary_print(node,prec_parent,PREC_ADD," + ",PREC_ADD);
         return;

      case MINUS:
         binary_print(node,prec_parent,PREC_SUB," - ",PREC_SUB+1);
         return;

      case EQUATE:
         binary_print(node,prec_parent,PREC_ASSIGN," = ",PREC_ASSIGN+1);
         return;

      case TIMES:
         binary_print(node,prec_parent,PREC_MUL,"*",PREC_MUL);
         return;

      case DIVIDE:
         binary_print(node,prec_parent,PREC_DIV,"/",PREC_DIV+1);
         return;

      case REALMOD:
         binary_print(node,prec_parent,PREC_DIV,"%%",PREC_DIV+1);
         return;

      case IMOD_:
         binary_print(node,prec_parent,PREC_DIV," imod ",PREC_DIV+1);
         return;

      case IDIV_:
         binary_print(node,prec_parent,PREC_DIV," idiv ",PREC_DIV+1);
         return;

      case COND_ELSE_:
         binary_print(node,prec_parent,PREC_COND,"):(",PREC_COND);
         strcat(pos++,")");
         return;

      case COND_EXPR_:
         strcat(pos++,"(");
         binary_print(node,prec_parent,PREC_COND,")?(",PREC_COND);
         return;

      case COND_TEST_:
         exprint_recur(node+node->left,PREC_COND);
         return;

      case INV:
         exprint_recur(node+node->left,PREC_POW);
         sprintf(pos,"^(-1)");
         pos += strlen(pos);
         return;

      case INTPOW:
         exprint_recur(node+node->left,PREC_POW);
         sprintf(pos,"^%1d",node->op1.intpow);
         pos += strlen(pos);
         return;

      case POW:
         binary_print(node,prec_parent,PREC_POW,"**",PREC_POW);
         return;

      case MAXIMUM_:
         strcat(pos,"maximum(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         strcat(pos++,",");
         exprint_recur(node+node->right,PREC_ARG);
         strcat(pos++,")");
         return;

      case MINIMUM_:
         strcat(pos,"minimum(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         strcat(pos++,",");
         exprint_recur(node+node->right,PREC_ARG);
         strcat(pos++,")");
         return;

      case INCOMPLETE_ELLIPTICF:
         sprintf(pos,"incompleteEllipticF(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         strcat(pos++,",");
         exprint_recur(node+node->right,PREC_ARG);
         strcat(pos++,")");
         return;

      case INCOMPLETE_ELLIPTICE:
         sprintf(pos,"incompleteEllipticE(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         strcat(pos++,",");
         exprint_recur(node+node->right,PREC_ARG);
         strcat(pos++,")");
         return;

      case ATAN2_:
         sprintf(pos,"atan2(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         strcat(pos++,",");
         exprint_recur(node+node->right,PREC_ARG);
         strcat(pos++,")");
         return;

      case WRAP_COMPOSE_:
         sprintf(pos,"wrap_compose(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         strcat(pos++,",");
         exprint_recur(node+node->right,PREC_ARG);
         strcat(pos++,")");
         return;

      case WRAP_INVERSE_:
         sprintf(pos,"wrap_inverse(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;

      case SQR:
         sprintf(pos,"(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")^2");
         pos += strlen(pos);
         return;

      case SQRT:
         sprintf(pos,"sqrt(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;

      case FLOOR_:
         sprintf(pos,"floor(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;

      case CEIL_:
         sprintf(pos,"ceil(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;

      case ABS:
         sprintf(pos,"abs(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;

      case SINH:
         sprintf(pos,"sinh(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;

      case COSH:
         sprintf(pos,"cosh(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;

      case TANH:
         sprintf(pos,"tanh(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;

      case ACOSH:
         sprintf(pos,"acosh(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;

      case ASINH:
         sprintf(pos,"asinh(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;

      case ATANH:
         sprintf(pos,"atanh(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;

      case SIN:
         sprintf(pos,"sin(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;

      case COS:
         sprintf(pos,"cos(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;

      case TAN:
         sprintf(pos,"tan(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;

      case EXP:
         sprintf(pos,"exp(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;

      case LOG:
         sprintf(pos,"log(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;

      case ASIN:
         sprintf(pos,"asin(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;

      case ACOS:
         sprintf(pos,"acos(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;

      case ATAN:
         sprintf(pos,"atan(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;
      
      case ELLIPTICK:
         sprintf(pos,"ellipticK(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;
      
      case ELLIPTICE:
         sprintf(pos,"ellipticE(");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         sprintf(pos,")");
         pos += strlen(pos);
         return;
      
      case CHS:
         sprintf(pos,"-");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_UMINUS);
         return;
      
      case NOT_:
         sprintf(pos," not ");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_NOT);
         return;
      
      case VIEW_MATRIX_:
         sprintf(pos,"view_matrix[");
         pos += strlen(pos);
         exprint_recur(node+node->left,PREC_ARG);
         strcat(pos,"]["); pos += 2;
         exprint_recur(node+node->right,PREC_ARG);
         strcat(pos++,"]");
         return;

      case LENGTH_:
      case VALENCE_:
      case AREA_:
      case VOLUME_:
      case DENSITY_:
      case PHASE_:
      case ID_:
      case STAR_:
      case OID_:
      case TAG_:
      case ORIGINAL_:
      case FIXED_:
      case NO_REFINE_:
      case HIT_PARTNER_:
      case NONCONTENT_:
      case NODISPLAY_:
      case BARE_:
      case SQ_MEAN_CURV_:
         sprintf(pos," %s ",keywordname(node->type));
         pos += strlen(pos);
         return;

      case GET_EXTRA_ATTR_:
         ex = EXTRAS(node->op2.eltype) + node->op3.extranum;
         print_attr(node,ex->name);
         if ( node->left ) 
           exprint_recur(node+node->left,prec_parent);       
         break;

      case ON_QUANTITY_:
         print_attr(node,"on_quantity ");
         strcat(pos,GEN_QUANT(node->op2.quant_id)->name);
         pos += strlen(pos);
         return;

      case ON_METHOD_INSTANCE_:
         print_attr(node,"on_method_instance ");
         strcat(pos,METH_INSTANCE(node->op2.meth_id)->name);
         pos += strlen(pos);
         return;

      case ON_CONSTRAINT_:
         print_attr(node,"on_constraint ");
         exprint_recur(node+node->left,prec_parent);
         return;

      case ON_CONSTRAINT_NAME:
         print_attr(node,"on_constraint ");
         strcat(pos,get_constraint(node->op3.connum)->name);
         pos += strlen(pos);
         return;

      case HIT_CONSTRAINT_:
         print_attr(node,"hit_constraint ");
         exprint_recur(node+node->left,prec_parent);
         return;

      case HIT_CONSTRAINT_NAME:
         print_attr(node,"hit_constraint ");
         strcat(pos,get_constraint(node->op3.connum)->name);
         pos += strlen(pos);
         return;

      case ON_BOUNDARY_:
         print_attr(node,"on_boundary ");
         exprint_recur(node+node->left,prec_parent);
         return;

      case ON_BOUNDARY_NAME:
         print_attr(node,"on_boundary ");
         strcat(pos,web.boundaries[node->op3.bdrynum].name);
         pos += strlen(pos);
         return;

      case QUALIFIED_ATTRIBUTE:
         exprint_recur(node+node->left,prec_parent);
         strcat(pos,"."); pos++;
         exprint_recur(node+node->right,prec_parent);
         return;

      case GET_MIDV_:
         print_attr(node,"midv");
         return;

      case GET_TRIPLE_PT_:
         print_attr(node,"triple_point");
         return;

      case GET_TETRA_PT_:
         print_attr(node,"tetra_point");
         return;

      case GET_AXIAL_POINT_:
         print_attr(node,"axial_point");
         return;

      case GET_FIXED_:
         print_attr(node,"fixed");
         return;

      case GET_BARE_:
         print_attr(node,"bare");
         return;

      case GET_NO_DISPLAY_:
         print_attr(node,"no_display");
         return;

      case GET_NONCONTENT_:
         print_attr(node,"noncontent");
         return;

      case GET_HIT_PARTNER_:
         print_attr(node,"hit_partner");
         return;

      case GET_NO_REFINE_:
         print_attr(node,"no_refine");
         return;

      case GET_ORIGINAL_:
         print_attr(node,"original");
         return;

      case GET_ID_:
         print_attr(node,"id");
         return;

      case GET_STAR_:
         print_attr(node,"star");
         return;

      case GET_OID_:
         print_attr(node,"oid");
         return;

      case GET_VALENCE_:
         print_attr(node,"valence");
         return;

      case GET_COLOR_:
         print_attr(node,"color");
         return;

      case GET_FRONTCOLOR_:
         print_attr(node,"frontcolor");
         return;

      case GET_BACKCOLOR_:
         print_attr(node,"backcolor");
         return;

      case GET_FRONTBODY_:
         print_attr(node,"frontbody");
         return;

      case GET_BACKBODY_:
         print_attr(node,"backbody");
         return;

      case GET_TAG_:
         print_attr(node,"tag");
         return;

      case GET_ORIENTATION_:
         print_attr(node,"orientation");
         return;

      case GET_SHOW_:
         print_attr(node,"show");
         return;

      case GET_LENGTH_:
         print_attr(node,"length");
         return;

      case GET_MEANCURV_:
         print_attr(node,"mean_curvature");
         return;

      case GET_FIXEDVOL_:
         print_attr(node,"volfixed");
         return;

      case GET_MID_EDGE_:
         print_attr(node,"mid_edge");
         return;

      case GET_MID_FACET_:
         print_attr(node,"mid_facet");
         return;

      case GET_WRAP_:
         print_attr(node,"wrap");
         return;

      case GET_SQ_MEAN_CURV_:
         print_attr(node,"sqcurve");
         return;

      case GET_DIHEDRAL_:
         print_attr(node,"dihedral");
         return;

      case GET_AREA_:
         print_attr(node,"area");
         return;

      case GET_VOLUME_:
         print_attr(node,"volume");
         return;

      case GET_VOLCONST_:
         print_attr(node,"volconst");
         return;

      case GET_TARGET_:
         print_attr(node,"target");
         return;

      case GET_MPI_TASK_:
         print_attr(node,"mpi_task");
         return;

      case GET_PRESSURE_:
         print_attr(node,"pressure");
         return;

      case GET_USERATTR_:
         print_attr(node,"user_attr");
         return;

      case GET_DENSITY_:
         print_attr(node,"density");
         return;

      case GET_PHASE_:
         print_attr(node,"phase");
         return;

      case GET_QUANTITY_:
         print_attr(node,GEN_QUANT(node->op2.quant_id)->name);
         return;

      case GET_INSTANCE_:
         print_attr(node,METH_INSTANCE(node->op2.meth_id)->name);
         return;

      case INDEXSET_:
      case DIMENSIONSET_:
         if ( node->right )
         { exprint_recur(node+node->left,prec_parent);
           strcat(pos++,"[");
           exprint_recur(node+node->right,prec_parent);
           strcat(pos++,"]");
         } else
         {
           strcat(pos++,"[");
           exprint_recur(node+node->left,prec_parent);
           strcat(pos++,"]");
         }
         break;
  
      case SET_ATTRIBUTE_:
         strcat(pos,"set "); pos += 4;
      case SET_ATTRIBUTE_A: /* single element assign */
      case SET_ATTRIBUTE_L:  /* skip printing set when in set loop */
        if ( pos[-1] != '.' ) strcat(pos++," "); /* just to be sure */
        switch ( node->op2.attr_kind )
        { case SET_DENSITY_: print_set_attr(node,"density"); break;
          case SET_EXTRA_ATTR_: 
            { ex = EXTRAS(node->op3.extra_info>>ESHIFT)
                        +(node->op3.extra_info&0xFF);
              print_attr(node,ex->name);
              if ( node->right )        
                exprint_recur(node+node->right,prec_parent);
            }
            break;
          case SET_PHASE_ : print_set_attr(node,"phase"); break;
          case SET_WRAP_ : print_set_attr(node,"wrap"); break;
          case SET_ORIENTATION_ : print_set_attr(node,"orientation"); break;
          case SET_TARGET_: print_set_attr(node,"target"); break;
          case SET_VOLCONST_: print_set_attr(node,"volconst"); break;
          case SET_PRESSURE_: print_set_attr(node,"pressure"); break;
          case SET_OPACITY_: print_set_attr(node,"opacity"); break;
          case SET_COLOR_: print_set_attr(node,"color"); break;
          case SET_ORIGINAL_: print_set_attr(node,"original"); break;
          case SET_FRONTBODY_: print_set_attr(node,"frontbody"); break;
          case SET_BACKBODY_: print_set_attr(node,"backbody"); break;
          case SET_FRONTCOLOR_: print_attr(node,"frontcolor"); break;
          case SET_BACKCOLOR_: print_set_attr(node,"backcolor"); break; 
          case SET_CONSTRAINT_: 
             { struct constraint *con;
               print_set_attr(node,"constraint "); 
               if ( node[node->left].type == PUSHCONST )
               { int cnum = (int)(node[node->left].op1.real);
                 con = get_constraint(cnum);
                 if ( (cnum <= web.maxcon) && (con->attr & NAMED_THING) )
                 { strcat(pos,con->name);
                   pos += strlen(pos); 
                   return;
                 } /* else recursion takes care of expression number constant */
               }
               pos += strlen(pos); 
               break;
             }
          case SET_BOUNDARY_: 
             { struct boundary *bdry;
               print_set_attr(node,"boundary "); 
               if ( node[node->left].type == PUSHCONST )
               { int bnum = (int)(node[node->left].op1.real);
                 bdry = web.boundaries+bnum;
                 if ( (bnum <= web.bdrymax) && (bdry->attr & NAMED_THING) )
                 { strcat(pos,bdry->name);
                   pos += strlen(pos); 
                   return;
                 } /* else recursion takes care of expression number constant */
               }
               pos += strlen(pos); 
               break;
             }
          case SET_TAG_: print_set_attr(node,"tag"); break;
          case SET_FIXED_: print_set_attr(node,"fixed"); break;
          case SET_BARE_: print_set_attr(node,"bare"); break;
          case SET_NO_REFINE_: print_set_attr(node,"no_refine"); break;
          case SET_HIT_PARTNER_: print_set_attr(node,"hit_partner"); break;
          case SET_NONCONTENT_: print_set_attr(node,"noncontent"); break;
          case SET_NO_DISPLAY_: print_set_attr(node,"no_display"); break;
          case SET_TETRA_PT_: print_set_attr(node,"tetra_pt"); break;
          case SET_AXIAL_POINT_: print_set_attr(node,"axial_point"); break;
          case SET_TRIPLE_PT_: print_set_attr(node,"triple_point"); break;
          case SET_COORD_1: 
             if ( node->right )
             { print_set_attr(node,"x"); 
               exprint_recur(node+node->right,prec_parent);
               break;
             }
             else print_set_attr(node,"X1"); 
             break;
          case SET_COORD_2: print_set_attr(node,"X2"); break;
          case SET_COORD_3: print_set_attr(node,"X3"); break;
          case SET_COORD_4: print_set_attr(node,"X4"); break;
          case SET_COORD_5: print_set_attr(node,"X5"); break;
          case SET_COORD_6: print_set_attr(node,"X6"); break;
          case SET_COORD_7: print_set_attr(node,"X7"); break;
          case SET_COORD_8: print_set_attr(node,"X8"); break;
          case SET_PARAM_1: 
             if ( node->right )
             { print_set_attr(node,"p1["); pos += 3;
               exprint_recur(node+node->right,prec_parent);
               strcat(pos,"]"); pos += strlen(pos);
               break;
             }
             else print_set_attr(node,"P1"); 
             break;
          case SET_PARAM_2: print_set_attr(node,"P2"); break;
          case SET_PARAM_3: print_set_attr(node,"P3"); break;
          case SET_PARAM_4: print_set_attr(node,"P4"); break;
          default:  
              sprintf(errmsg,"Internal error: bad SET_ATTRIBUTE type %d.\n",
                node->op2.attr_kind);
              kb_error(1655,errmsg,RECOVERABLE);

        }
        if ( node->type == SET_ATTRIBUTE_A )
          { switch ( node[1].op1.assigntype )
             { case ASSIGN_: strcat(pos," := "); break;
               case PLUSASSIGN_: strcat(pos," += "); break;
               case SUBASSIGN_: strcat(pos," -= "); break;
               case MULTASSIGN_: strcat(pos," *= "); break;
               case DIVASSIGN_: strcat(pos," /= "); break;
             }
             pos += 4; 
          }
        strcat(pos," ("); pos += 2;
        if ( node->left )  exprint_recur(node+node->left,prec_parent);
        strcat(pos++,")");
        return;

     case SINGLE_ASSIGN_ : 
         exprint_recur(node+node->left,prec_parent);
         *(pos++) = '.'; *pos = 0;
         exprint_recur(node+node->right,prec_parent);
         return;

     case EQ_:
         binary_print(node,prec_parent,PREC_COMP," == ",PREC_COMP);
         return;
     case NE_:
         binary_print(node,prec_parent,PREC_COMP," != ",PREC_COMP);
         return;
     case GE_:
         binary_print(node,prec_parent,PREC_COMP," >= ",PREC_COMP);
         return;
     case LE_:
         binary_print(node,prec_parent,PREC_COMP," <= ",PREC_COMP);
         return;
     case GT_:
         binary_print(node,prec_parent,PREC_COMP," > ",PREC_COMP);
         return;
     case LT_:
         binary_print(node,prec_parent,PREC_COMP," < ",PREC_COMP);
         return;
     case AND_:
         binary_print(node,prec_parent,PREC_AND," && ",PREC_AND);
         return;
     case OR_:
         binary_print(node,prec_parent,PREC_OR," || ",PREC_OR);
         return;
     case CONJUNCTION_END:     
         exprint_recur(node+node->left,prec_parent);
         return;

     default: 
         sprintf(pos,"(unknown)");
         pos += strlen(pos);
         sprintf(errmsg,"Printing of expression node type %s (%d) unimplemented.\n",
         tokname(node->type),node->type);
         kb_error(1656,errmsg,WARNING);

         return;
     }
  return ; /* shouldn't happen */
} /* end exprint_recur */

/**************************************************************************
*
* function: binary_print()
*
* purpose: print binary operation with parentheses if needed.
*
*/

void binary_print(node,prec_parent,prec1,op,prec2)
struct treenode *node;
int prec_parent;
int prec1;
char *op;
int prec2;
{
  if ( prec_parent > prec1 ) 
   { sprintf(pos,"(");
     pos += strlen(pos);
   }
  exprint_recur(node+node->left,prec1);
  sprintf(pos,op);
  pos += strlen(pos);
  exprint_recur(node+node->right,prec2);
  if ( prec_parent > prec1 )
    { sprintf(pos,")");
      pos += strlen(pos);
    }
  return;
}

/**************************************************************************
*
* function: set_print()
*
* purpose: print SET type command, with possible WHERE clause.
*
*/

void set_print(node,keyw,attrw,prec_parent)
struct treenode *node;
char *keyw,*attrw;
int prec_parent;
{ struct treenode *nnode;
  sprintf(pos,"%s ",keyw);
  pos += strlen(pos);
  nnode = node + node->left;
  if ( nnode->type == WHERE_ ) nnode += nnode->left; /* get NEXT_ */
  exprint_recur(nnode,prec_parent);
  sprintf(pos," %s ",attrw); 
  pos += strlen(pos);
  if ( node->right )  exprint_recur(node+node->right,prec_parent);
  if ( node[node->left].type == WHERE_ )
  { node+= node->left;
    sprintf(pos," where "); pos += strlen(pos);
    exprint_recur(node+node->right,prec_parent);
  }
  return;
}

void print_attr(node,word)
struct treenode *node;
char *word;
{
  sprintf(pos,"%s",word); 
  pos += strlen(pos); return;
}

void print_set_attr(node,word)
struct treenode *node;
char *word;
{
  sprintf(pos,"%s",word); 
  pos += strlen(pos); return;
}

