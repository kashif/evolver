/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/


/*****************************************************************
*
*  File: eval_all.c
*
*  Purpose: To evaluate first derivatives of expressions.
*        
*/

#include "include.h" 
#include "ytab.h"

/* for easy assignments */
#define FIRST for ( i = 0 ; i < pcount ; i++ ) stacktop->deriv[i] 
#define SECOND  for ( i = 0 ; i < pcount ; i++ )\
                    for ( j = 0 ; j < pcount ; j++ ) stacktop->second[i][j] 


/*****************************************************************
*
*  Function eval_all()
*
*  Purpose: runtime tree_evaluation of expression and all of its
*             first partial derivatives.
*
*/

void eval_all(ex,params,pcount,fval,partials,q_id)
struct expnode *ex;        /* expression tree */
REAL *params;     /* vector of parameters */
int  pcount;      /* number of variables */
REAL *fval;        /* function value */
REAL *partials;  /* values of partials */
element_id q_id; /* reference element, if any */
{
  int i,n;
  REAL x,y;
  struct estack { REAL value, deriv[2*MAXCOORD]; } localstack[100];
  struct estack *stacktop = localstack;
  struct treenode *node;
  element_id id;
  struct locallist_t *localbase = ex->locals;

  for ( i = 0 ; i < pcount ; i++ )
      stacktop->deriv[i] = 0.0;
  stacktop->value = 0.0;  /* for empty expression */
  if ( ex == NULL ) return;
  if ( ex->start == NULL )
     kb_error(1017,"Internal error: eval_all expression node null.\n",
       RECOVERABLE);


  for ( node = ex->start+1 ; ; node++ )
  {
    switch ( node->type )
    {
      case SETUP_FRAME_: 
             break;
             
      case EXPRLIST_:
             break;  /* leave expression on stack */
 
      case FUNCTION_CALL_:
      { struct thread_data *td = GET_THREAD_DATA;
        REAL value;
        
		/* push arguments on eval() stack */
        for ( i = 0 ; i < node->op2.argcount ; i++ )
          *(++(td->stack_top)) = stacktop[i-node->op2.argcount+1].value;
        value = eval(&globals(node->op1.name_id)->value.proc,
           NULL,NULLID,NULL);
        td->stack_top -= node->op2.argcount; /* pop arguments */
        (++stacktop)->value = value;
        FIRST = 0.0;
        break;
      }
      
	  case FUNCTION_CALL_RETURN_:
        {
          /* nothing to do here since FUNCTION_CALL used eval() */
          break;
        }

      case INDEXSET_: break; /* just accumulate index values */

      case ARRAY_HEAD_: break;  /* let indices accumulate */

      case ARRAYEVAL:
      { struct array *a = globals(node->op2.name_id)->attr.arrayptr;
        REAL value;
        int i,offset;
        void *lvalue;
        for ( i = 0 ; i < a->dim ; i++ )
        { int k = (int)stacktop[i+1-a->dim].value;
          if ( k < 1 )
          { sprintf(errmsg,"Array index %d of array %s is %d. Indexes start at 1.",
               i+1,globals(node->op2.name_id)->name,k);
            sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                 file_names[node->file_no],node->line_no);
            kb_error(2533,errmsg,RECOVERABLE);
          }
          if ( k > a->sizes[i] )
          { sprintf(errmsg,"Array index %d of array %s is %d; exceeds bound of %d.",
               i+1,globals(node->op2.name_id)->name,k,a->sizes[i]);
            sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                 file_names[node->file_no],node->line_no);
            kb_error(2536,errmsg,RECOVERABLE);
          }
        }
        for ( i = 1, offset = (int)stacktop[1-a->dim].value-1 ; i < a->dim ; i++ )
        { offset *= a->sizes[i];
          offset += (int)stacktop[i+1-a->dim].value-1;  /* 1-based indexing */
        }
        stacktop -= a->dim;
        lvalue = ((char *)a) + a->datastart + offset*a->itemsize;
        switch ( a->datatype )
        { case REAL_TYPE: value = *(REAL*)(lvalue);    break;
          case INTEGER_TYPE: value = *(int*)(lvalue);    break;
          case UINT_TYPE: value = *(unsigned int*)(lvalue);    break;
          case CHAR_TYPE: value = *(char*)(lvalue);    break;
          case UCHAR_TYPE: value = *(unsigned char*)(lvalue);    break;
          case SHORT_TYPE: value = *(short int*)(lvalue);    break;
          case USHORT_TYPE: value = *(unsigned short int*)(lvalue);    break;
          case LONG_TYPE: value = *(long*)(lvalue);    break;
          case ULONG_TYPE: value = *(unsigned long*)(lvalue);    break;
          case PTR_TYPE: value = (unsigned long)*(char**)(lvalue);    break;
          case VERTEX_TYPE:
          case EDGE_TYPE:
          case FACET_TYPE:
          case BODY_TYPE:
          case FACETEDGE_TYPE:
          case ELEMENTID_TYPE: value = (int)*(element_id*)(lvalue); break;
          default: value = *(int*)(lvalue); break;

        }
        (++stacktop)->value = value;
        FIRST=0.0;
      }
      break;

      case BACKQUOTE_START_:
        { struct expnode bqnode = *ex;
          bqnode.start = node;
          bqnode.root = node+node->op1.skipsize;
          bqnode.locals = localbase;
          eval(&bqnode,params,q_id,NULL);
          node += node->op1.skipsize-1;
        }
        break;
      case BACKQUOTE_END_ : break;  /* just a placeholder */
      case ACOMMANDEXPR_: /* backquoted command at start of expression */
           break;

       case SET_GLOBAL_:
         { struct global *g = globals(node->op1.name_id);
           g->value.real = stacktop->value;
           if ( g->attr.varstuff.gradhess == NULL )
             g->attr.varstuff.gradhess = (REAL*)mycalloc(MAXCOORD*(MAXCOORD+1), sizeof(REAL));
           for ( i = 0 ; i < pcount ; i++ )
                g->attr.varstuff.gradhess[i] = stacktop->deriv[i];
           stacktop--;
           break;
         }

       case GET_VOLCONST_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           (++stacktop)->value = get_body_volconst(id);
           FIRST = 0.0;
           break;

 
       case GET_TARGET_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           (++stacktop)->value = get_body_fixvol(id);
           FIRST = 0.0;
           break;

       case GET_MPI_TASK_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           (++stacktop)->value = id_task(id);
           FIRST = 0.0;
           break;

       case REPLACECONST:
           for ( i = 0 ; i < pcount ; i++ )
              stacktop->deriv[i] = 0.0;
           stacktop->value = node->op1.real;
           break;

     case COND_TEST_:
           if ( (stacktop--)->value == 0. )
           { /* jump */
             node += node->op1.skipsize;
           }
           break;

     case COND_EXPR_:
           /* did first command, so skip second */
           node += node->op1.skipsize;
           break;

     case COND_ELSE_:
           break;


     case MAXIMUM_:
           stacktop--;
           if  (stacktop[0].value < stacktop[1].value) 
             stacktop[0] = stacktop[1];
           break;

     case MINIMUM_:
           stacktop--;
           if (stacktop[0].value > stacktop[1].value) 
             stacktop[0] = stacktop[1];
           break;

     case TOGGLEVALUE:
        (++stacktop)->value = (REAL)get_toggle_value(node->op1.toggle_state);
        FIRST = 0.0;
        break;

     case SIZEOF_:
        (++stacktop)->value = (REAL) 
            EXTRAS(node->op2.eltype)[node->op1.extranum].array_spec.datacount;
        FIRST = 0.0;
        break;

     case PUSHCONST:
     case PUSHPI:
     case PUSHE:
        (++stacktop)->value = node->op1.real;
        for ( i = 0 ; i < pcount ; i++ )
           stacktop->deriv[i] = 0.0;
        break;

     case PUSHG:
        (++stacktop)->value = web.gravflag ? web.grav_const : 0.;
        for ( i = 0 ; i < pcount ; i++ )
           stacktop->deriv[i] = 0.0;
        break;

     case PUSHGLOBAL_:
     case PUSHADJUSTABLE:
           { struct global *g = globals(node->op1.name_id);
             if ( g->flags & FILE_VALUES )
             (++stacktop)->value = g->value.file.values[int_val];
             else if ( g->flags & STRINGVAL )
             (++stacktop)->value = 0.0;
             else
             (++stacktop)->value = g->value.real;
             if ( g->attr.varstuff.gradhess ) 
               for ( i = 0 ; i < pcount ; i++ )
                stacktop->deriv[i] = g->attr.varstuff.gradhess[i];
             else FIRST = 0.0;
           }
           break;

     case PUSHPARAM:
        (++stacktop)->value = params[node->op1.coordnum];
        for ( i = 0 ; i < pcount ; i++ )
             if ( i == node->op1.coordnum )
                stacktop->deriv[i] = 1.0;
             else stacktop->deriv[i] = 0.0;
        break;

     case PARAM_:
        if ( node->op1.localnum ) 
           id = *(element_id*)get_localp(node->op1.localnum);
        else id = q_id;
        (++stacktop)->value = get_param(id)[node->op2.coordnum];
        for ( i = 0 ; i < SDIM ; i++ )
             if ( i == node->op2.coordnum )
                stacktop->deriv[i] = 1.0;
             else stacktop->deriv[i] = 0.0;
        break;


     case PUSHQPRESSURE_:
         (++stacktop)->value = GEN_QUANT(node->op1.quant_id)->pressure;
         FIRST = 0.0;
         break;

     case PUSHQTARGET_:
          (++stacktop)->value = GEN_QUANT(node->op1.quant_id)->target;
          FIRST = 0.0;
          break;

     case PUSHQMODULUS_:
          (++stacktop)->value = GEN_QUANT(node->op1.quant_id)->modulus;
          FIRST = 0.0;
          break;

     case PUSHQTOLERANCE_:
          (++stacktop)->value = GEN_QUANT(node->op1.quant_id)->tolerance;
          FIRST = 0.0;
          break;

     case PUSHMMODULUS_:
          (++stacktop)->value
             = METH_INSTANCE(node->op1.quant_id)->modulus;
          FIRST = 0.0;
          break;

     case PUSHQVALUE_:
          { struct gen_quant *q = GEN_QUANT(node->op1.quant_id);
            (++stacktop)->value = q->value;
            FIRST = 0.0;
          }
          break;

     case PUSHMVALUE_:
          { struct method_instance *mi = METH_INSTANCE(node->op1.meth_id);
            (++stacktop)->value = mi->value;
            if ( (mi->flags & Q_COMPOUND) && (mi->stamp == comp_quant_stamp) )
            { for ( i = 0 ; i < pcount ; i++ )
                     stacktop->deriv[i] = mi->grad[comp_quant_vertex][i];
            }
            else FIRST = 0.0;
          }
          break;

     case PUSHQVOLCONST_:
          (++stacktop)->value
             = GEN_QUANT(node->op1.quant_id)->volconst;
          FIRST = 0.0;
          break;

 
     case PUSH_NAMED_QUANTITY:
     case PUSH_METHOD_INSTANCE_:
          (++stacktop)->value = (REAL)node->op1.quant_id;
          FIRST = 0.0;
          break;


     case GET_INTERNAL_:
           (++stacktop)->value = get_internal_variable(node->op1.name_id);
           FIRST = 0.0;
           break;
  
     case DYNAMIC_LOAD_FUNC_:
           (*node->op1.funcptr)(FUNC_DERIV,params,(struct dstack*)(++stacktop));
           break;

     case USERFUNC:
           stacktop++;
           stacktop->value =
             (*userfunc_deriv[node->op1.userfunc])(params,stacktop->deriv);
           break;

     case INDEXED_ELEMENT_:
         { int ord = (int)((stacktop--)->value);
           element_id id = get_ordinal_id(node->op1.eltype,abs(ord)-1);
           if ( !valid_id(id) ) 
           { sprintf(errmsg,"%s index %d is not valid.\n",
                 typenames[node->op1.eltype],ord);
             sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                 file_names[node->file_no],node->line_no);
             kb_error(1289,errmsg,RECOVERABLE);
           }
           if ( ord < 0 ) invert(id);
           *(element_id*)get_localp(node->op2.localnum) = id;
           break;
         }

     case QUALIFIED_ATTRIBUTE:
         break; /* just a no-op in execution */


/* need logical expressions for conditional expressions */

     case GT_:
           stacktop--;
           stacktop[0].value = (REAL)(stacktop[0].value > stacktop[1].value);
           FIRST = 0.0;
           break;

     case LT_:
           stacktop--;
           stacktop[0].value = (REAL)(stacktop[0].value < stacktop[1].value);
           FIRST = 0.0;
           break;

     case LE_:
           stacktop--;
           stacktop[0].value = (REAL)(stacktop[0].value <= stacktop[1].value);
           FIRST = 0.0;
           break;

     case GE_:
           stacktop--;
           stacktop[0].value = (REAL)(stacktop[0].value >= stacktop[1].value);
           FIRST = 0.0;
           break;

     case NE_:
           stacktop--;
           stacktop[0].value = (REAL)(stacktop[0].value != stacktop[1].value);
           FIRST = 0.0;
           break;

     case EQ_:
           stacktop--;
           stacktop[0].value = (REAL)(stacktop[0].value == stacktop[1].value);
           FIRST = 0.0;
           break;

     case AND_: /* short-circuit */
	   if ( stacktop->value == 0.0 )
	   { (++stacktop)->value = 0.0;
             FIRST = 0.0;
             node += node->op1.skipsize;
	   } 
           break;

     case CONJUNCTION_END: 
	   /* short-circuiting results in second arg being answer */
	   stacktop--;
	   *stacktop = stacktop[1];
           if ( stacktop->value != 0.0 ) 
             stacktop->value = 1.0;
	   break;

     case OR_:
	   if ( stacktop->value != 0.0 )
	   { (++stacktop)->value = 1.0;
             FIRST = 0.0;
             node += node->op1.skipsize;
	   } 
           break;

     case NOT_:
           stacktop[0].value = (REAL)(!stacktop[0].value);
           FIRST = 0.0;
           break;

     case PLUS:
           stacktop--;
           stacktop[0].value += stacktop[1].value;
           for ( i = 0 ; i < pcount ; i++ )
              stacktop[0].deriv[i] += stacktop[1].deriv[i];
           break;

     case MINUS:
     case EQUATE:
           stacktop--;
           stacktop[0].value -= stacktop[1].value;
           for ( i = 0 ; i < pcount ; i++ )
              stacktop[0].deriv[i] -= stacktop[1].deriv[i];
           break;

     case TIMES:
           stacktop--;
           for ( i = 0 ; i < pcount ; i++ )
              stacktop[0].deriv[i] = stacktop[1].value*stacktop[0].deriv[i]
                        + stacktop[0].value*stacktop[1].deriv[i];
           stacktop[0].value *= stacktop[1].value;
           break;

     case DIVIDE:
           stacktop--;
           if ( stacktop[1].value == 0.0 )
           { sprintf(errmsg,"Divide by zero.\n");
             sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                 file_names[node->file_no],node->line_no);
             kb_error(1019,errmsg,RECOVERABLE);
           }

           for ( i = 0 ; i < pcount ; i++ )
              stacktop[0].deriv[i] = (stacktop[1].value*stacktop[0].deriv[i]
                        - stacktop[0].value*stacktop[1].deriv[i])
                        /stacktop[1].value/stacktop[1].value;
           stacktop[0].value /= stacktop[1].value;
           break;

     case REALMOD:
           stacktop--;
           if ( stacktop[1].value == 0.0 )
           { sprintf(errmsg,"Modulus base zero.\n");
             sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                 file_names[node->file_no],node->line_no);
             kb_error(1020,errmsg,RECOVERABLE);
           }

           for ( i = 0 ; i < pcount ; i++ )
              stacktop[0].deriv[i] = stacktop[0].deriv[i]
                        -  floor(stacktop[0].value/stacktop[1].value)
                                    *stacktop[1].deriv[i];
           stacktop[0].value -= floor(stacktop[0].value/stacktop[1].value)
                                           *stacktop[1].value;
           break;

     case IMOD_:
           stacktop--;
           if ( stacktop[1].value == 0.0 ) 
             kb_error(1021,"Modulus base zero.\n",RECOVERABLE);

           stacktop[0].value = floor(stacktop[0].value) - 
               floor(floor(stacktop[0].value)/floor(stacktop[1].value))
                                                 *floor(stacktop[1].value);
           for ( i = 0 ; i < pcount ; i++ )
              stacktop[0].deriv[i] = 0.0; 
           break;

     case IDIV_:
           stacktop--;
           if ( (int)stacktop[1].value == 0 ) 
             kb_error(1022,"Division by zero.\n",RECOVERABLE);
           stacktop[0].value = 
               (REAL)((int)(stacktop[0].value)/(int)(stacktop[1].value));
           for ( i = 0 ; i < pcount ; i++ )
             stacktop[0].deriv[i] = 0.0; 
           break;

     case INTPOW:
         if ( node->op1.intpow == 0 )
           { stacktop->value = 1.0;
             for ( i = 0 ; i < pcount ; i++ )
                stacktop->deriv[i] = 0.0;
           }
         else if ( node->op1.intpow == 1 )
           { /* no action necessary */
           }
         else
           { x = stacktop->value;
             if ( node->op1.intpow > 1 ) /* get n-1 power first */ 
                for ( n = 1 ; n < node->op1.intpow-1 ; n++ )
                   stacktop->value *= x;
             else if ( node->op1.intpow < 0 ) 
             { if ( x == 0.0 ) 
                { sprintf(errmsg,"Negative power (%d) of zero.\n",node->op1.intpow);
                   kb_error(1023,errmsg,RECOVERABLE);
                }
                stacktop->value = 1/x;
                for ( n = 0 ; n < -node->op1.intpow ; n++ )
                   stacktop->value *= 1/x;
             }
             for ( i = 0 ; i < pcount ; i++ )
                stacktop->deriv[i] = 
                    node->op1.intpow*stacktop->value*stacktop->deriv[i];
             stacktop->value *= x;
           }
           break;

     case POW:
        stacktop--;
        x = stacktop[0].value;
        y = stacktop[1].value;
        if ( x == 0.0 )
        { stacktop->value = 0.0;
          if ( y == 1.0 ) 
            for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] = stacktop[0].deriv[i];
           else if ( y > 1.0 ) 
            for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] = 0.;
           else
           { sprintf(errmsg,"Negative power (%f) of zero in derivative.\n",
                    (DOUBLE)(y-1));
             kb_error(1024,errmsg,RECOVERABLE);
           }
        }
        else if ( (x < 0) && ( (REAL)(int)y != y ) )
        { sprintf(errmsg,"Nonintegral power (%f) of negative number.\n",(double)y);
          kb_error(2003,errmsg,RECOVERABLE);
        }
        else 
        {
          stacktop->value = pow(x,y);             
          for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] = (log(fabs(x))*stacktop[1].deriv[i]
                 + y/x*stacktop[0].deriv[i]) *stacktop->value;
        }
        break;

     case ATAN2_:
        stacktop--;
        y = stacktop[0].value;
        x = stacktop[1].value;
        stacktop->value = atan2(y,x);
        for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] = (x*stacktop[0].deriv[i]
                 - y*stacktop[1].deriv[i])/(x*x + y*y);
        break;

     case INCOMPLETE_ELLIPTICF:
        stacktop--;
        y = stacktop[0].value;
        x = stacktop[1].value;
        stacktop->value = incompleteEllipticF(x,y);
        for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] = 
                incompleteEllipticFdphi(x,y)*stacktop[0].deriv[i]
              + incompleteEllipticFdm(x,y)*stacktop[1].deriv[i];
        break;

     case INCOMPLETE_ELLIPTICE:
        stacktop--;
        y = stacktop[0].value;
        x = stacktop[1].value;
        stacktop->value = incompleteEllipticE(x,y);
        for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] = 
                incompleteEllipticEdphi(x,y)*stacktop[0].deriv[i]
              + incompleteEllipticEdm(x,y)*stacktop[1].deriv[i];
        break;

     case ELLIPTICK:
        x = stacktop[0].value;
        stacktop->value = ellipticK(x);
        for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] = ellipticKdm(x)*stacktop[0].deriv[i];
        break;

     case ELLIPTICE:
        x = stacktop[0].value;
        stacktop->value = ellipticE(x);
        for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] = ellipticEdm(x)*stacktop[0].deriv[i];
        break;

     case SQR:
        for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] *= 2*stacktop->value;
        stacktop->value *= stacktop->value;
        break;

     case SQRT:
        if ( stacktop->value < 0.0 )
          kb_error(2496,"Square root of negative number.\n",RECOVERABLE);
        stacktop->value = sqrt(stacktop->value);
        if ( stacktop->value == 0.0 )
           for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] = 0.0;
        else
           for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] /= 2*stacktop->value;
        break;

     case CEIL_:
          stacktop->value = ceil(stacktop->value);
          FIRST = 0;
          break;

     case FLOOR_:
          stacktop->value = floor(stacktop->value);
          FIRST = 0;
          break;

     case ABS:
           if ( stacktop->value < 0.0 )
           {
             for ( i = 0 ; i < pcount ; i++ )
                   stacktop->deriv[i] = -stacktop->deriv[i];
             stacktop->value = -stacktop->value;
           }
           break;

     case SIN:
        x = cos(stacktop->value);
        for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] *= x;
        stacktop->value = sin(stacktop->value);
        break;

     case COS:
        x = -sin(stacktop->value);
        for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] *= x;
        stacktop->value = cos(stacktop->value);
        break;

     case ATAN:
        x = (1 + stacktop->value*stacktop->value);
        for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] /= x;
        stacktop->value = atan(stacktop->value);
        break;

     case EXP:
        stacktop->value = exp(stacktop->value);
        for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] *= stacktop->value;
        break;

     case SINH:
        y = exp(stacktop->value);
        x = (y+1/y)/2;
        for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] *= x;
        stacktop->value = (y-1/y)/2;
        break;

     case COSH:
        y = exp(stacktop->value);
        x = (y-1/y)/2;
        for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] *= x;
        stacktop->value = (y+1/y)/2;
        break;

     case TANH:
           y = exp(stacktop->value);
           x = (y-1/y)/2;
           y = (y+1/y)/2;
           FIRST  /= y*y ;
           stacktop->value = x/y;
           break;

     case ASINH:
           y = 1/sqrt(1+stacktop->value*stacktop->value);
           FIRST *= y;
           stacktop->value = log(stacktop->value + 
                    sqrt(stacktop->value*stacktop->value + 1));
           break;

     case ACOSH:
           if ( stacktop->value <= 1.0 )
             kb_error(2490,"Acosh argument less than 1.\n",RECOVERABLE);
           y = 1/sqrt(stacktop->value*stacktop->value - 1);
           FIRST *= y;
           stacktop->value = 2*log(sqrt(stacktop->value + 1) +
                 sqrt(stacktop->value - 1)) - log(2.0);
           break;

     case ATANH:
           if ( stacktop->value >= 1.0 )
             kb_error(2491,"Acosh argument less than 1.\n",RECOVERABLE);
           y = 1/(1 - stacktop->value*stacktop->value);
           FIRST *= y;
           stacktop->value = log(stacktop->value + 1)/2 -
                 log(1 - stacktop->value)/2;
           break;

     case LOG:
        if ( stacktop->value <= 0.0 )
        { sprintf(errmsg,"Log argument is %18.15f; must be positive.\n",
              stacktop->value);
          sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                file_names[node->file_no],node->line_no);
          kb_error(2577,errmsg, RECOVERABLE );
        }
        for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] /= stacktop->value;
        stacktop->value = log(stacktop->value);
        break;

     case ASIN:
        if ( fabs(stacktop->value) > 1.0 )
        { sprintf(errmsg,"Asin argument is %18.15f, magnitude greater than 1.\n",
              stacktop->value);
          sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                file_names[node->file_no],node->line_no);
          kb_error(2579,errmsg, RECOVERABLE );
        }
        x = sqrt(1 - stacktop->value*stacktop->value);
        for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] /= x;
        stacktop->value = asin(stacktop->value);
        break;

     case ACOS:
        if ( fabs(stacktop->value) >= 1.0 )
        { sprintf(errmsg,"Acos argument is %18.15f, magnitude greater than 1.\n",
              stacktop->value);
          sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                file_names[node->file_no],node->line_no);
          kb_error(2583,errmsg, RECOVERABLE );
        }
        x  = -sqrt(1 - stacktop->value*stacktop->value);
        for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] /= x;
        stacktop->value = acos(stacktop->value);
        break;

     case TAN:
        stacktop->value = tan(stacktop->value);
        x = 1+stacktop->value*stacktop->value;
        for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] *= x;
        break;
     
     case CHS:
        for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] = -stacktop->deriv[i];
        stacktop->value = -stacktop->value;
        break;
     
     case INV:
        if ( stacktop->value == 0.0 )
             kb_error(2495,"Division by zero.\n",RECOVERABLE);
        for ( i = 0 ; i < pcount ; i++ )
             stacktop->deriv[i] /= stacktop->value*stacktop->value;
        stacktop->value = 1/stacktop->value;
        break;

     /* attribute values */
     case GET_SQ_MEAN_CURV_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           stacktop->value = vertex_sq_mean_curvature(id);
           FIRST = 0.0;
           break;

     case GET_MEANCURV_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           stacktop->value = vertex_mean_curvature(id);
           FIRST = 0.0;
           break;

     case LENGTH_:
     case GET_LENGTH_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           calc_edge(id);
           ++stacktop;
           stacktop->value = get_edge_length(id);
           FIRST = 0.0;
           break;

     case GET_DIHEDRAL_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           if ( id_type(id) == EDGE )    stacktop->value = dihedral(id);
           else if ( id_type(id) == VERTEX )    stacktop->value = vertex_angle(id);
           else    stacktop->value = 0.0;
           FIRST = 0.0;
           break;

     case VALENCE_:
     case GET_VALENCE_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           switch ( id_type(id) )
            { case VERTEX:
                    stacktop->value = (REAL)get_vertex_evalence(id);
                 break;
               case EDGE:
                    stacktop->value = (REAL)get_edge_valence(id);
                 break;
               case FACET:
                    stacktop->value = (REAL)get_facet_valence(id);
                 break;
               case BODY:
                    stacktop->value = (REAL)get_body_valence(id);
                 break;
            }
           FIRST = 0.0;
           break;

     case GET_EDGE_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           stacktop->value = (REAL)(ordinal(get_fe_edge(id))+1);
           FIRST = 0.0;
           break;

     case GET_FACET_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           stacktop->value = (REAL)(ordinal(get_fe_facet(id))+1);
           FIRST = 0.0;
           break;

        case AREA_:
        case GET_AREA_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           stacktop->value = get_facet_area(id);
           FIRST = 0.0;
           break;

        case GET_WRAP_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           stacktop->value = (REAL)get_edge_wrap(id); 
           FIRST = 0.0;
           break;

        case GET_PRESSURE_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           switch ( id_type(id) )
             { case BODY:    stacktop->value = get_body_pressure(id); break;
               default: kb_error(1025,"Pressure only for bodies.\n",RECOVERABLE);
             }
           FIRST = 0.0;
           break;

        case GET_USERATTR_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           stacktop->value = user_attribute(id);
           FIRST = 0.0;
           break;

        case GET_QUANTITY_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           stacktop->value = quantity_attribute(id,node->op2.quant_id);
           FIRST = 0.0;
           break;

        case GET_INSTANCE_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           stacktop->value = instance_attribute(id,node->op2.meth_id);
           FIRST = 0.0;
           break;

        case GET_PHASE_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           switch ( id_type(id) )
             { 
                case FACET:  stacktop->value = (REAL)get_f_phase(id); break;
                case BODY:    stacktop->value = (REAL)get_b_phase(id); break;
                default: 
                 kb_error(1026,"Phase of wrong type element.\n",RECOVERABLE);
             }
           FIRST = 0.0;
           break;

        case DENSITY_:
        case GET_DENSITY_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           switch ( id_type(id) )
             { case EDGE:    stacktop->value = get_edge_density(id); break;
               case FACET:    stacktop->value = get_facet_density(id); break;
               case BODY:    stacktop->value = get_body_density(id); break;
               default: kb_error(1027,"Density of wrong type element.\n",RECOVERABLE);
             }
           FIRST = 0.0;
           break;

        case GET_STAR_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           switch ( id_type(id) ) 
             { case VERTEX:    stacktop->value = get_vertex_star(id); break;
               case EDGE:    stacktop->value = get_edge_star(id); break;
               default:    stacktop->value = 0.0; break;
             }
           FIRST = 0.0;
           break;

        case VOLUME_:
        case GET_VOLUME_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           stacktop->value = get_body_volume(id);
           FIRST = 0.0;
           break;

        case ID_:
        case GET_ID_:
        case GET_OID_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           if ( (node->type == GET_OID_) && inverted(id) )
                stacktop->value = -(REAL)(ordinal(id)+1);
           else    stacktop->value = (REAL)(ordinal(id)+1);
           FIRST = 0.0;
           break;

        case ORIGINAL_:
        case GET_ORIGINAL_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           stacktop->value = valid_id(id) ? (REAL)ordinal(get_original(id))+1:0;
           FIRST = 0.0;
           break;

        case GET_FRONTCOLOR_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           stacktop->value = (REAL)get_facet_frontcolor(id); 
           FIRST = 0.0;
           break;

        case GET_COLOR_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           switch ( id_type(id) )
            { case EDGE:    stacktop->value = (REAL)get_edge_color(id); break;
              case FACET:   stacktop->value = (REAL)get_facet_color(id); break;
              default:    stacktop->value = 0.0;
            }
           FIRST = 0.0;
           break;

        case GET_BACKCOLOR_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           switch ( id_type(id) )
            { case FACET: stacktop->value = (REAL)get_facet_backcolor(id);
                   break;
              default:    stacktop->value = 0.0;
            }
           FIRST = 0.0;
           break;


        case GET_FRONTBODY_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           switch ( id_type(id) )
            { case BODY:  stacktop->value = 
                 (REAL)ordinal(get_facet_body(id))+1; break;
              default: stacktop->value = 0.0;
            }
           FIRST = 0.0;
           break;

        case GET_BACKBODY_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           switch ( id_type(id) )
            { case BODY:  stacktop->value = 
                   (REAL)ordinal(get_facet_body(inverse_id(id)))+ 1;
                   break;
              default: stacktop->value = 0.0;
            }
           FIRST = 0.0;
           break;

        case TAG_:
        case GET_TAG_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           stacktop->value = (REAL)get_tag(id);
           FIRST = 0.0;
           break;

        case BARE_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           stacktop->value = (get_attr(id) & BARE_NAKED) ? 1.0 : 0.0;
           FIRST = 0.0;
           break;

        case GET_MIDV_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           stacktop->value = ordinal(get_edge_midv(id)) + 1.;
           FIRST = 0.0;
           break;

        case FIXED_:
        case GET_FIXED_:
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           stacktop->value = (get_attr(id) & FIXED) ? 1.0 : 0.0;
           FIRST = 0.0;
           break;

        case GET_EXTRA_ATTR_:
         { struct extra *ext;
           int spot,k;
          
         
         n = node->op3.extranum;
         if ( node->op1.localnum ) 
            id = *(element_id*)get_localp(node->op1.localnum);
         else id = q_id;
         ext = EXTRAS(node->op2.eltype) + n;
         /* get index */
         spot = 0;
         for ( k = 0 ; k < ext->array_spec.dim ; k++ )
         { int j = (int)(stacktop[-ext->array_spec.dim+k+1].value);
           spot *= ext->array_spec.sizes[k];
           if ( (j < 1) || (j > ext->array_spec.sizes[k]) )
           { sprintf(errmsg,
                "Attribute %s index %d is %d; maximum is %d (in %s).\n",
                ext->name,k+1,j,ext->array_spec.sizes[k],ex->name);
             kb_error(2525,errmsg,RECOVERABLE);
           }
           spot += (int)(stacktop[-ext->array_spec.dim+k+1].value) - 1;
         }
         stacktop -= ext->array_spec.dim;
         if ( id_type(id) != node->op2.eltype )
         { if ( (id_type(id)==EDGE) && (node->op2.eltype==VERTEX) && params )
           { ext = EXTRAS(VERTEX) + n;
             (++stacktop)->value = interp_edge_attribute(id,ext,spot,(int)params[2*SDIM]);
             FIRST = 0.0;
             break;
           }
           else 
           if ( (id_type(id)==FACET) && (node->op2.eltype==VERTEX) && params )
           { ext = EXTRAS(VERTEX) + n;
             (++stacktop)->value = interp_facet_attribute(id,ext,spot,(int)params[2*SDIM]);
             FIRST = 0.0;
             break;
           }
           else 
           { sprintf(errmsg,
               "Attribute %s is %s attribute, not %s attribute (in %s).\n",
               EXTRAS(node->op2.eltype)[n].name,
                 typenames[node->op2.eltype], typenames[id_type(id)],ex->name);
             kb_error(2537,errmsg,RECOVERABLE);
           }
         }

         if ( ext->code.start ) 
         { int oldflag = autorecalc_flag;
           autorecalc_flag = 0;
           eval(&ext->code,NULL,id,NULL);  /* side-effect fills in values */
           autorecalc_flag = oldflag;
          }
         switch ( ext->type )
         { case REAL_TYPE: 
            (++stacktop)->value = ((REAL*)get_extra(id,n))[spot]; 
            break;
           case INTEGER_TYPE: 
           case CONSTRAINT_TYPE: 
           case BOUNDARY_TYPE: 
           case QUANTITY_TYPE: 
           case INSTANCE_TYPE: 
           case PROCEDURE_TYPE: 
            (++stacktop)->value = (REAL)((int*)get_extra(id,n))[spot];
            break;
           case UINT_TYPE: 
            (++stacktop)->value = (REAL)((unsigned int*)get_extra(id,n))[spot];
            break;
           case USHORT_TYPE: 
            (++stacktop)->value = (REAL)((unsigned short*)get_extra(id,n))[spot];
            break;
           case SHORT_TYPE: 
            (++stacktop)->value = (REAL)((short*)get_extra(id,n))[spot];
            break;
           case ULONG_TYPE: 
            (++stacktop)->value = (REAL)((unsigned long*)get_extra(id,n))[spot];
            break;
           case LONG_TYPE: 
            (++stacktop)->value = (REAL)((long*)get_extra(id,n))[spot];
            break;
           case UCHAR_TYPE: 
            (++stacktop)->value = (REAL)((unsigned char*)get_extra(id,n))[spot];
              break;
           case CHAR_TYPE: 
            (++stacktop)->value = (REAL)((char*)get_extra(id,n))[spot];
              break;
           case PTR_TYPE: 
            (++stacktop)->value = (REAL)(unsigned long int)((char**)get_extra(id,n))[spot];
              break;
           case ELEMENTID_TYPE: 
           case VERTEX_TYPE: 
           case EDGE_TYPE: 
           case FACET_TYPE: 
           case BODY_TYPE: 
           case FACETEDGE_TYPE: 
            (++stacktop)->value = (REAL)((element_id*)get_extra(id,n))[spot];
            break;
         }
       }
           FIRST = 0.0;
         break;

        case ON_CONSTRAINT_:
         { int testcon = (int)(stacktop--)->value;
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           switch(id_type(id))
            { case VERTEX: stacktop->value = (REAL)v_on_constraint(id,testcon);
                   break;
              case EDGE  : stacktop->value = (REAL)e_on_constraint(id,testcon);
                   break;
              case FACET : stacktop->value = (REAL)f_on_constraint(id,testcon);
                   break;
              default: kb_error(1030,"Can't do constraints on this type element.\n",
                    RECOVERABLE);
            }
           }
           FIRST = 0.0;
           break;

        case HIT_CONSTRAINT_:
         { 
           int testcon = (int)(stacktop--)->value;
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           switch(id_type(id))
            { case VERTEX: stacktop->value = (REAL)get_v_constraint_status(id,testcon);
                    break;
              default: kb_error(1031,"Can do hit_constraints only on vertices.\n",

                    RECOVERABLE);
            }
         }
         FIRST = 0.0;
         break;

        case ON_BOUNDARY_:
         { struct boundary *b=NULL;
           int testb = (int)(stacktop--)->value;
           if ( node->op1.localnum ) 
              id = *(element_id*)get_localp(node->op1.localnum);
           else id = q_id;
           ++stacktop;
           switch(id_type(id))
            { case VERTEX: b = get_boundary(id); break;
              case EDGE  : b = get_edge_boundary(id); break;
              case FACET : b = get_facet_boundary(id); break;
              default: kb_error(1032,"Can't do boundary on this type element.\n",
                    RECOVERABLE);
            }
           stacktop->value = (b == web.boundaries+testb) ? 1.0 : 0.0;
         }
         FIRST = 0.0;
         break;

     /* whole-array syntax */

     case ARRAYIDENT_: /* push datastart for array */
        { struct global *glvalue = globals(node->op2.name_id);
          struct array *alvalue = glvalue->attr.arrayptr;
          if ( glvalue->flags & FIXED_SIZE_ARRAY )
            *(struct estack**)(++stacktop) = 
                localstack + glvalue->attr.arrayptr->datastart;
          else 
            *(char**)(++stacktop) = (char*)alvalue + alvalue->datastart;
          break;
        }

    case ATTRIB_LVALUE_:  /* push datastart for attribute array */
        { element_id id;
          n = node->op2.name_id & GLOBMASK; /* attribute number */
          if ( node->op1.localnum )
            id = *(element_id*)get_localp(node->op1.localnum);
          else id = q_id;
          *(char**)(++stacktop) = (char*)get_extra(id,n);
        }
        break;

    case ARRAY_VERTEX_NORMAL_:
    case ARRAY_EDGE_VECTOR_:
    case ARRAY_FACET_NORMAL_:
        { element_id id;
          REAL *datastart =  (REAL*)get_localp(node->op3.localnum);
          *(REAL**)(++stacktop) = datastart;
          if ( node->flags & IS_RVALUE )
          { if ( node->op1.localnum )
              id = *(element_id*)get_localp(node->op1.localnum);
            else id = q_id;
            switch ( node->type )
            { case ARRAY_VERTEX_NORMAL_:
               { MAT2D(normal,MAXCOORD,MAXCOORD);
                 REAL mag;
                 int normcount;

                 normcount = new_calc_vertex_normal(id,normal);
                 project_vertex_normals(id,normal,normcount);
                 mag = sqrt(SDIM_dot(normal[0],normal[0]));
                 if ( mag == 0.0 ) mag = 1;
                 for ( i = 0 ; i < SDIM ; i++ )
                    datastart[i] = normal[0][i]/mag;
              
                 break;
                }
              case ARRAY_EDGE_VECTOR_:
                 get_edge_side(id,datastart);
                 break;
              case ARRAY_FACET_NORMAL_:
                 get_facet_normal(id,datastart);
                 break;
            }
          }
        }
        break;



    case ARRAY_LVALUE_INDEXED_:
        break;
    case ARRAY_RVALUE_ :
        break;
        
    case DOT_:  /* dot product */
        { struct array *a,*b;
          int name1 = node->op2.name_id;
          int name2 = node->op3.name_id;
          REAL *datastart1,*datastart2;
          REAL sum;
          int count;
          a = get_name_arrayptr(name1,NULL,localbase);
          b = get_name_arrayptr(name2,NULL,localbase);
          count = (a->datacount < b->datacount) ? a->datacount : b->datacount;
          datastart1 = *(REAL**)(stacktop--);
          datastart2 = *(REAL**)(stacktop--);
          for ( sum = 0.0, i = 0 ; i < count ; i++ )
            sum += datastart1[i]*datastart2[i];
          (++stacktop)->value = sum;
          FIRST = 0.0;

          break;
        }
 
 
    case ARRAY_EVAL_:  /* rexpr: arraylvalue indexset */
      { /* use info on stack to push value of array element.
             stack: datastart index-values -> rexpr */
        struct array *a;
        REAL value=0.0;
        int i,offset;
        void *lvalue;
        char *datastart;

        a = get_name_arrayptr(node->op2.name_id,NULL,NULL);

        for ( i = 0 ; i < a->dim ; i++ )
        { int k = (int)stacktop[i+1-a->dim].value;
          if ( k < 1 )
          { sprintf(errmsg,
             "Array index %d of array %s is %d. Indexes start at 1.\n",
               i+1,get_name_name(node->op2.name_id,localbase),k);
            sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                 file_names[node->file_no],node->line_no);
            kb_error(3007,errmsg,RECOVERABLE);
          }
          if ( k > a->sizes[i] )
          { sprintf(errmsg,"Array index %d of array %s is %d; exceeds bound of %d.\n",
               i+1,get_name_name(node->op2.name_id,localbase),k,a->sizes[i]);
            sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                 file_names[node->file_no],node->line_no);
            kb_error(3008,errmsg,RECOVERABLE);
          }
        }
        for ( i = 1, offset = (int)stacktop[1-a->dim].value-1 ; i < a->dim ; i++ )
        { offset *= a->sizes[i];
          offset += (int)stacktop[i+1-a->dim].value-1;  /* 1-based indexing */
        }
        stacktop -= a->dim;
        datastart = *(char**)(stacktop--);
        lvalue = datastart + offset*a->itemsize;
        switch ( a->datatype )
        { case REAL_TYPE: value = *(REAL*)(lvalue);  break;
          case INTEGER_TYPE: value = *(int*)(lvalue); break;
          case UINT_TYPE: value = *(unsigned int*)(lvalue); break;
          case SHORT_TYPE: value = *(short int*)(lvalue); break;
          case USHORT_TYPE: value = *(unsigned short int*)(lvalue); break;
          case LONG_TYPE: value = *(long int*)(lvalue); break;
          case ULONG_TYPE: value = *(unsigned long int*)(lvalue); break;
          case CHAR_TYPE: value = *(char*)(lvalue); break;
          case UCHAR_TYPE: value = *(unsigned char*)(lvalue); break;
          case PTR_TYPE: value = (unsigned long int)*(char**)(lvalue); break;
          case VERTEX_TYPE:
          case EDGE_TYPE:
          case FACET_TYPE:
          case BODY_TYPE:
          case FACETEDGE_TYPE:
          case ELEMENTID_TYPE:  break;
          default: value = *(int*)(lvalue); break;
        
        }

         (++stacktop)->value = value;
         FIRST = 0.0;
        break;
      }
        case FINISHED:
          *fval = stacktop->value;
          for ( i = 0 ; i < pcount ; i++ )
             partials[i] = stacktop->deriv[i]; 
          return;

        default:
           sprintf(errmsg,"Bad expression eval_all() node type %d: %s.",
                 node->type,tokname(node->type));
          kb_error(1033,errmsg,RECOVERABLE);
          break;
      }      
    if ( node == ex->root ) break;
  }      

  *fval = stacktop->value;
  for ( i = 0 ; i < pcount ; i++ )
  partials[i] = stacktop->deriv[i]; 
  return;
} /* end eval_all() */





