/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/


/*****************************************************************
*
*  File: eval_sec.c
*
*  Purpose: To evaluate first and second derivatives of expressions
*        
*/

/* NOTE: Compiling with full optimization in Visual Studio .NET beta 1
   takes a very long time, like an hour.  Seems to be due to "global
   optimization" option.  Set optimization to custom, just for speed,
   in file properties in project.
*/

#include "include.h" 
#include "ytab.h"

/* for easy assignments */
#define FIRST for ( i = 0 ; i < pcount ; i++ ) stacktop->deriv[i] 
#define SECOND  for ( i = 0 ; i < pcount ; i++ )\
                    for ( j = 0 ; j < pcount ; j++ ) stacktop->second[i][j] 

/* to save a little code size */
void zero_seconds ARGS((int, struct dstack *));
void zero_seconds (pcount, stacktop)
int pcount;
struct dstack *stacktop;
{ int i,j;
  FIRST = 0.0;
  SECOND = 0.0;
}

#ifdef MAC_CW
#define ESTACKSIZE 20
#else
#define ESTACKSIZE 100
#endif

/*****************************************************************
*
*  Function eval_second()
*
*  Purpose: runtime tree_evaluation of expression and all of its
*              partial derivatives and second derivatives.
*
*/

void eval_second(ex,params,pcount,fval,partials,seconds,q_id)
struct expnode *ex; /* expression tree */
REAL *params;       /* vector of paramters */
int  pcount;        /* number of variables */
REAL *fval;         /* function value */
REAL *partials;     /* values of partials */
REAL **seconds;     /* second derivatives */
element_id q_id;    /* reference element, if any */
{
  int n,m;   /* i, j missing since want them local for optimization */
  REAL x,y;
  struct dstack localstack[ESTACKSIZE];
  struct dstack *stacktop = localstack;
  struct treenode *node;
  element_id id;
  struct locallist_t *localbase = ex->locals;

  if ( pcount > 2*MAXCOORD )
      kb_error(1009,"More variables than 2*MAXCOORD in eval_second().\n",RECOVERABLE);

  stacktop->value = 0.0;  /* in case of empty expression */
  zero_seconds(pcount,stacktop);

  if ( ex ) 
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
        int i,j;
        
		/* push arguments on eval() stack */
        for ( i = 0 ; i < node->op2.argcount ; i++ )
          *(++(td->stack_top)) = stacktop[i-node->op2.argcount+1].value;

        value = eval(&globals(node->op1.name_id)->value.proc,
           NULL,NULLID,NULL);
        td->stack_top -= node->op2.argcount; /* pop arguments */
        (++stacktop)->value = value;
        FIRST = 0.0;
        SECOND = 0.0;
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
        REAL value=0.0;
        int i,j,offset;
        void *lvalue;
        for ( i = 0 ; i < a->dim ; i++ )
        { int k = (int)stacktop[i+1-a->dim].value;
          if ( k < 1 )
          { sprintf(errmsg,"Array index %d of array %s is %d. Indexes start at 1.",
               i+1,globals(node->op2.name_id)->name,k);
            sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                 file_names[node->file_no],node->line_no);
            kb_error(2534,errmsg,RECOVERABLE);
          }
          if ( k > a->sizes[i] )
          { sprintf(errmsg,"Array index %d of array %s is %d; exceeds bound of %d.",
               i+1,globals(node->op2.name_id)->name,k,a->sizes[i]);
            sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                 file_names[node->file_no],node->line_no);
            kb_error(2535,errmsg,RECOVERABLE);
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
        SECOND = 0.0;
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
           int i,j;
           g->value.real = stacktop->value;
           if ( g->attr.varstuff.gradhess == NULL )
              g->attr.varstuff.gradhess = (REAL*)mycalloc(MAXCOORD*(MAXCOORD+1), sizeof(REAL));
           for ( i = 0 ; i < pcount ; i++ )
              g->attr.varstuff.gradhess[i] = stacktop->deriv[i];
           for ( i = 0 ; i < pcount ; i++ )
             for ( j = 0 ; j < pcount ; j++ )
                (g->attr.varstuff.gradhess+MAXCOORD)[i*MAXCOORD+j] = stacktop->second[i][j];
           stacktop--;
           break;
         }

       case REPLACECONST:
          zero_seconds(pcount,stacktop);
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
          zero_seconds(pcount,stacktop);
          break;

       case SIZEOF_:
          (++stacktop)->value = 
              (REAL)EXTRAS(node->op2.eltype)[node->op1.extranum].array_spec.datacount;
          zero_seconds(pcount,stacktop);
          break;

       case PUSHCONST:
       case PUSHPI:
       case PUSHE:
          (++stacktop)->value = node->op1.real;
          zero_seconds(pcount,stacktop);
             break;

       case PUSHG:
          (++stacktop)->value = web.gravflag ? web.grav_const : 0.0;
          zero_seconds(pcount,stacktop);
             break;

       case PUSHGLOBAL_:
       case PUSHADJUSTABLE:
             { struct global *g = globals(node->op1.name_id);
               int i,j;
               if ( g->flags & FILE_VALUES )
               (++stacktop)->value = g->value.file.values[int_val];
               else if ( g->flags & STRINGVAL )
               (++stacktop)->value = 0.0;
               else
               (++stacktop)->value = g->value.real;
               if ( g->attr.varstuff.gradhess )
               { for ( i = 0 ; i < pcount ; i++ )
                   stacktop->deriv[i] = g->attr.varstuff.gradhess[i];
                 for ( i = 0 ; i < pcount ; i++ )
                   for ( j = 0 ; j < pcount ; j++ )
                     stacktop->second[i][j] = 
                       (g->attr.varstuff.gradhess+MAXCOORD)[i*MAXCOORD+j];
               }
               else zero_seconds(pcount,stacktop);
             }
             break;

       case PUSHPARAM:
        { int i,j;
          (++stacktop)->value = params[node->op1.coordnum];
          for ( i = 0 ; i < pcount ; i++ )
             if ( i == node->op1.coordnum )
                stacktop->deriv[i] = 1.0;
             else stacktop->deriv[i] = 0.0;
          SECOND = 0.0;
          break;
        }

     case PARAM_:
      { int i,j;
        if ( node->op1.localnum ) 
           id = *(element_id*)get_localp(node->op1.localnum);
        else id = q_id;
        (++stacktop)->value = get_param(id)[node->op2.coordnum];
        for ( i = 0 ; i < SDIM ; i++ )
             if ( i == node->op2.coordnum )
                stacktop->deriv[i] = 1.0;
             else stacktop->deriv[i] = 0.0;
        SECOND = 0;
        break;
      }

      case PUSHQPRESSURE_:
           (++stacktop)->value = GEN_QUANT(node->op1.quant_id)->pressure;
           zero_seconds(pcount,stacktop);
           break;

       case PUSHQTARGET_:
           (++stacktop)->value = GEN_QUANT(node->op1.quant_id)->target;
           zero_seconds(pcount,stacktop);
           break;

       case PUSHQMODULUS_:
           (++stacktop)->value = GEN_QUANT(node->op1.quant_id)->modulus;
           zero_seconds(pcount,stacktop);
           break;

       case PUSHQTOLERANCE_:
           (++stacktop)->value = GEN_QUANT(node->op1.quant_id)->tolerance;
           zero_seconds(pcount,stacktop);
           break;

       case PUSHMMODULUS_:
           (++stacktop)->value = METH_INSTANCE(node->op1.meth_id)->modulus;
           zero_seconds(pcount,stacktop);
           break;

       case PUSHQVALUE_:
         { struct gen_quant *q = GEN_QUANT(node->op1.quant_id);
           if ((q->timestamp<graph_timestamp) || (q->timestamp<web_timestamp))
                      calc_quants(q->flags&(Q_INFO|Q_ENERGY|Q_FIXED));
           (++stacktop)->value = q->value;
           zero_seconds(pcount,stacktop);
         }
         break;

       case PUSHMVALUE_:
         { struct method_instance *mi = METH_INSTANCE(node->op1.meth_id);
           int i,j;
           if ( mi->flags & Q_COMPOUND )
           { if ( compound_hess_flag == CH_GRADS )
             { if ( quantity_function_sparse_flag )
               { /* saving up second partials of quantity wrt methods */
                 (++stacktop)->value = mi->value;
                 FIRST = 0.0;
                 stacktop->deriv[mi->quant_index] = 1.0;
                 SECOND = 0.0;
               }
               else /* old dense way */
               { volgrad *vgptri = get_vertex_vgrad(comp_quant_vi);
                 volgrad *vgptrj = get_vertex_vgrad(comp_quant_vj);
                 for ( ; vgptri ; vgptri = vgptri->chain ) 
                   if ( vgptri->qnum == mi->self_id ) break;
                 for ( ; vgptrj ; vgptrj = vgptrj->chain ) 
                   if ( vgptrj->qnum == mi->self_id ) break;
                 (++stacktop)->value = mi->value;
                 for ( i = 0 ; i < SDIM ; i++ )
                   { stacktop->deriv[i] = (vgptri ? vgptri->grad[i] : 0.0);
                     stacktop->deriv[i+SDIM] = (vgptrj ? vgptrj->grad[i] : 0.0);
                   }
                 SECOND = 0.0;
               }
             }
             if ( compound_hess_flag == CH_HESS )
             { (++stacktop)->value = mi->value;
               FIRST = 0.0;
               if ( mi->stamp == comp_quant_stamp )
                 for ( i = 0 ; i < pcount ; i++ )
                   for ( j = 0 ; j < pcount ; j++ )
                     stacktop->second[i][j] = 
                       mi->hess[comp_quant_vertexi][comp_quant_vertexj][i][j];
               else SECOND = 0.0;
             }
           }
           else 
           { if ((mi->timestamp<graph_timestamp) || (mi->timestamp<web_timestamp))
                      calc_quants((Q_INFO|Q_ENERGY|Q_FIXED));
             (++stacktop)->value = mi->value;
             zero_seconds(pcount,stacktop);
           }
         }
         break;

       case PUSHQVOLCONST_:
           (++stacktop)->value = GEN_QUANT(node->op1.quant_id)->volconst;
           zero_seconds(pcount,stacktop);
           break;


       case PUSH_NAMED_QUANTITY:
       case PUSH_METHOD_INSTANCE_:
             (++stacktop)->value = (REAL)node->op1.quant_id;
             zero_seconds(pcount,stacktop);
             break;


       case GET_INTERNAL_:
             (++stacktop)->value = get_internal_variable(node->op1.name_id);
             zero_seconds(pcount,stacktop);
             break;

       case DYNAMIC_LOAD_FUNC_:
             (*node->op1.funcptr)(FUNC_SECOND,params,(struct dstack*)(++stacktop));
             break;

       case USERFUNC:
          { MAT2D(tmp,MAXCOORD,MAXCOORD);
            int i,j;
            stacktop++;
            stacktop->value =
              (*userfunc_seconds[node->op1.userfunc])(params,stacktop->deriv,tmp);
            SECOND = tmp[i][j];
          }
          break;

         case INDEXED_ELEMENT_:
           { int ord = (int)((stacktop--)->value);
             id = get_ordinal_id(node->op1.eltype,abs(ord)-1);
             if ( !valid_id(id) ) 
             { sprintf(errmsg,"%s index %d is not valid.\n",
                   typenames[node->op1.eltype],ord);
               sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                 file_names[node->file_no],node->line_no);
               kb_error(1201,errmsg,RECOVERABLE);
             }
             if ( ord < 0 ) invert(id);
             *(element_id*)get_localp(node->op2.localnum) = id;
             break;
           }

       case QUALIFIED_ATTRIBUTE:
           break; /* just a no-op in execution */

/*need logical expressions for conditional expressions */

       case GT_:
             stacktop--;
             stacktop[0].value = (REAL)(stacktop[0].value > stacktop[1].value);
             zero_seconds(pcount,stacktop);
             break;

       case LT_:
             stacktop--;
             stacktop[0].value = (REAL)(stacktop[0].value < stacktop[1].value);
             zero_seconds(pcount,stacktop);
             break;

       case LE_:
             stacktop--;
             stacktop[0].value = (REAL)(stacktop[0].value <= stacktop[1].value);
             zero_seconds(pcount,stacktop);
             break;

       case GE_:
             stacktop--;
             stacktop[0].value = (REAL)(stacktop[0].value >= stacktop[1].value);
             zero_seconds(pcount,stacktop);
             break;

       case NE_:
             stacktop--;
             stacktop[0].value = (REAL)(stacktop[0].value != stacktop[1].value);
             zero_seconds(pcount,stacktop);
             break;

       case EQ_:
             stacktop--;
             stacktop[0].value = (REAL)(stacktop[0].value == stacktop[1].value);
             zero_seconds(pcount,stacktop);
             break;

     case AND_: /* short-circuit */
	   if ( stacktop->value == 0.0 )
	   { (++stacktop)->value = 0.0;
             zero_seconds(pcount,stacktop);
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

     case OR_:  /* short-circuit */
	   if ( stacktop->value != 0.0 )
	   { (++stacktop)->value = 1.0;
             zero_seconds(pcount,stacktop);
             node += node->op1.skipsize;
	   } 
           break;

       case NOT_:
             stacktop[0].value = (REAL)(!stacktop[0].value);
             zero_seconds(pcount,stacktop);
             break;

       case PLUS:
           { int i,j;
             stacktop--;
             stacktop[0].value += stacktop[1].value;
             FIRST += stacktop[1].deriv[i];
             SECOND += stacktop[1].second[i][j];
             break;
           }
          
       case MINUS:
       case EQUATE:
           { int i,j;
             stacktop--;
             stacktop[0].value -= stacktop[1].value;
             FIRST -= stacktop[1].deriv[i];
             SECOND -= stacktop[1].second[i][j];
             break;
           }

       case TIMES:
           { int i,j;
             stacktop--;
             SECOND = stacktop[0].second[i][j]*stacktop[1].value
                      + stacktop[0].deriv[i]*stacktop[1].deriv[j]
                      + stacktop[0].deriv[j]*stacktop[1].deriv[i]
                      + stacktop[0].value*stacktop[1].second[i][j];
             FIRST = stacktop[1].value*stacktop[0].deriv[i]
                          + stacktop[0].value*stacktop[1].deriv[i];
             stacktop[0].value *= stacktop[1].value;
             break;
           }

       case DIVIDE:
           { int i,j;
             stacktop--;
             if ( stacktop[1].value == 0.0 )
               { sprintf(errmsg,"Division by zero.\n");
                 sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                    file_names[node->file_no],node->line_no);
                 kb_error(1011,errmsg,RECOVERABLE);
               }

             SECOND = (stacktop[0].second[i][j]*stacktop[1].value
                      - stacktop[0].deriv[i]*stacktop[1].deriv[j]
                      - stacktop[0].deriv[j]*stacktop[1].deriv[i]
                      - stacktop[0].value*stacktop[1].second[i][j]
                      + 2*stacktop[0].value*stacktop[1].deriv[i]
                         *stacktop[1].deriv[j]/stacktop[1].value)
                          /stacktop[1].value/stacktop[1].value;
             FIRST = (stacktop[1].value*stacktop[0].deriv[i]
                          - stacktop[0].value*stacktop[1].deriv[i])
                          /stacktop[1].value/stacktop[1].value;
             stacktop[0].value /= stacktop[1].value;
             break;
           }

       case REALMOD:
           { int i,j;
             stacktop--;
             if ( stacktop[1].value == 0.0 )
               { sprintf(errmsg,"Modulus base 0.\n");
                 sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                    file_names[node->file_no],node->line_no);
                 kb_error(1012,errmsg,RECOVERABLE);
               }
             y = floor(stacktop[0].value/stacktop[1].value); /* integer part */
             FIRST = stacktop[0].deriv[i] - y*stacktop[1].deriv[i];
             SECOND = stacktop[0].second[i][j] - y*stacktop[1].second[i][j];
             stacktop[0].value -= y*stacktop[1].value;
             break;
           }

       case IMOD_:
             stacktop--;
             if ( stacktop[1].value == 0.0 ) 
               { sprintf(errmsg,"Modulus base 0.\n");
                 sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                    file_names[node->file_no],node->line_no);
                 kb_error(1013,errmsg,RECOVERABLE);
               }
             stacktop[0].value = floor(stacktop[0].value) - 
                 floor(floor(stacktop[0].value)/floor(stacktop[1].value))
                                                   *floor(stacktop[1].value);
             zero_seconds(pcount,stacktop);
             break;

       case IDIV_:
             stacktop--;
             if ( (int)stacktop[1].value == 0 ) 
               { sprintf(errmsg,"Division by zero.\n");
                 sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                    file_names[node->file_no],node->line_no);
                 kb_error(1014,errmsg,RECOVERABLE);
               }
             stacktop[0].value = 
                 (REAL)((int)(stacktop[0].value)/(int)(stacktop[1].value));
             zero_seconds(pcount,stacktop);
             break;

       case INTPOW:
           if ( node->op1.intpow == 0 )
              { stacktop->value = 1.;
                 zero_seconds(pcount,stacktop);
              }
           else if ( node->op1.intpow == 1 )
              { /* no action necessary */
              }
           else
             { REAL f1,f2=1.0;
               int i,j;
               x = stacktop->value;
               if ( node->op1.intpow > 1 ) /* get n-2 power first */ 
                  for ( n = 1 ; n < node->op1.intpow-1 ; n++ )
                     f2 *= x;
               else if ( node->op1.intpow < 0 ) 
               { if ( x == 0.0 ) 
                 { sprintf(errmsg,"Negative power zero.\n");
                   sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                      file_names[node->file_no],node->line_no);
                   kb_error(1015,errmsg,RECOVERABLE);
                 }
                 for ( f2 = 1/x, n = 0 ; n <= -node->op1.intpow ; n++ )
                     f2 /= x;
               }
               f1 = f2*x;
               stacktop->value = f1*x;
               SECOND = node->op1.intpow*(node->op1.intpow-1)*f2
                         *stacktop->deriv[i]*stacktop->deriv[j] +
                      node->op1.intpow*f1*stacktop->second[i][j];
               FIRST = node->op1.intpow*f1*stacktop->deriv[i];
             }
           break;

       case POW:
          stacktop--;
          x = stacktop[0].value;
          y = stacktop[1].value;
          if ( x == 0.0 )
          { stacktop->value = 0.0;
            if ( y > 2.0 )
            { int i,j;  FIRST = 0.0; SECOND = 0.0; }
            else if ( y < 2.0 )
            { sprintf(errmsg,"Negative power (%f) of zero in second derivative.\n",
                      (DOUBLE)(y-2));
              sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                 file_names[node->file_no],node->line_no);
              kb_error(2005,errmsg,RECOVERABLE);
            }
          }
          else if ( (x < 0) && ( (REAL)(int)y != y ) )
          { sprintf(errmsg,"Nonintegral power (%f) of negative number.\n",(double)y);
            sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                 file_names[node->file_no],node->line_no);
            kb_error(2006,errmsg,RECOVERABLE);
          }
          else
          { int i,j;
            stacktop->value = pow(x,y);
            SECOND = ((log(fabs(x))*stacktop[1].deriv[i]+y/x*stacktop[0].deriv[i])
                 *(log(fabs(x))*stacktop[1].deriv[j]+y/x*stacktop[0].deriv[j])
                 +log(fabs(x))*stacktop->second[i][j] 
                 + stacktop[0].deriv[i]* stacktop[1].deriv[j]/x
                 + stacktop[0].deriv[j]* stacktop[1].deriv[i]/x
                 + stacktop[0].deriv[i]* stacktop[0].deriv[j]*y/x/x
                 + stacktop[0].second[i][j]*y/x)*stacktop->value;
            FIRST = (log(fabs(x))*stacktop[1].deriv[i]
                   + y/x*stacktop[0].deriv[i]) *stacktop->value;
          }
          break;

       /* This case causes a 2-min slowdown in VisualStudio.NET opt compile time*/
       case ATAN2_:
        { REAL denom;
          int i,j;
          stacktop--;
          y = stacktop[0].value;
          x = stacktop[1].value;
          stacktop->value = atan2(y,x);
          denom = x*x + y*y;
          SECOND = (x*stacktop[0].second[i][j]
              +stacktop[1].deriv[j]*stacktop[0].deriv[i]
              -stacktop[0].deriv[j]*stacktop[1].deriv[i]
              -y*stacktop[1].second[i][j])/denom
              - 2*(x*stacktop[0].deriv[i]-y*stacktop[1].deriv[i])
                  *(x*stacktop[1].deriv[j]+y*stacktop[0].deriv[j])/denom/denom;
          FIRST = (x*stacktop[0].deriv[i]-y*stacktop[1].deriv[i])/denom;
          break;
        }

       case SQR:
        { int i,j;
          SECOND = 2*(stacktop->deriv[i]*stacktop->deriv[j]
                              + stacktop->value*stacktop->second[i][j]);
          FIRST *= 2*stacktop->value;
          stacktop->value *= stacktop->value;
          break;
        }

       case SQRT:
           { int i,j;
             if ( stacktop->value < 0.0 )
             { sprintf(errmsg,"Square root of negative number.\n");
               sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                 file_names[node->file_no],node->line_no);
               kb_error(2567,errmsg,RECOVERABLE);
             }
             x = sqrt(stacktop->value);
             if ( x == 0.0 )
              { SECOND = 0.0; FIRST = 0.0;}
             else
              { SECOND = (stacktop->second[i][j]/2 - stacktop->deriv[i]*
                         stacktop->deriv[j]/stacktop->value/4)/x;
                FIRST /= 2*x;
              }
             stacktop->value = x;
             break;
           }

     case CEIL_:
        { int i,j;
          stacktop->value = ceil(stacktop->value);
          FIRST = 0;
          SECOND = 0;
          break;
        }

     case FLOOR_:
        { int i,j;
          stacktop->value = floor(stacktop->value);
          FIRST = 0;
          SECOND = 0;
          break;
        }

       case ABS:
             if ( stacktop->value < 0.0 )
             { int i,j;
               FIRST *= -1 ;
               SECOND *= -1;
               stacktop->value = -stacktop->value;
             }
             break;

       case SIN:
           { int i,j;
             x = cos(stacktop->value); y = sin(stacktop->value);
             SECOND = -y*stacktop->deriv[i]*stacktop->deriv[j]
                         + x*stacktop->second[i][j];
             FIRST *= x;
             stacktop->value = y;
             break;
           }

       case COS:
           { int i,j;
             x = cos(stacktop->value); y = sin(stacktop->value);
             SECOND = -x*stacktop->deriv[i]*stacktop->deriv[j]
                         - y*stacktop->second[i][j];
             FIRST *= -y;
             stacktop->value = x;
             break;
           }

       case ATAN:
           { int i,j;
             x = (1 + stacktop->value*stacktop->value);
             SECOND = -2*stacktop->value/x/x*stacktop->deriv[i]
             *stacktop->deriv[j] +stacktop->second[i][j]/x;
             FIRST /= x;
             stacktop->value = atan(stacktop->value);
             break ;
           }

       case EXP:
           { int i,j;
             stacktop->value = exp(stacktop->value);
             SECOND = (stacktop->deriv[i]*stacktop->deriv[j] +
                          stacktop->second[i][j])*stacktop->value;
             FIRST *= stacktop->value;
             break;
           }

       case LOG:
           { int i,j;
             if ( stacktop->value <= 0.0 )
             { sprintf(errmsg,"Log of nonpositive number.\n");
               sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                 file_names[node->file_no],node->line_no);
               kb_error(2497,errmsg,RECOVERABLE);
             }
             SECOND = (stacktop->second[i][j] 
                  - stacktop->deriv[i]*stacktop->deriv[j]/stacktop->value)
                  /stacktop->value;
             FIRST /= stacktop->value;
             stacktop->value = log(stacktop->value) ;
              break;
           }

       case ASIN:
           { int i,j;
             if ( fabs(stacktop->value) >= 1.0 )
             { sprintf(errmsg,"Asin argument magnitude greater than 1.\n");
               sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                  file_names[node->file_no],node->line_no);
               kb_error(2570,errmsg,RECOVERABLE);
             }
               x = 1 - stacktop->value*stacktop->value;
               y = sqrt(x);
               SECOND = stacktop->second[i][j]/y +
                  stacktop->deriv[i]*stacktop->deriv[j]*stacktop->value/x/y;
               FIRST /= y;
               stacktop->value = asin(stacktop->value);
               break ;
           }

       case ACOS:
           { int i,j;
             if ( fabs(stacktop->value) >= 1.0 )
             { sprintf(errmsg,"Acos argument magnitude greater than 1.\n");
               sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                    file_names[node->file_no],node->line_no);
               kb_error(2571,errmsg,RECOVERABLE);
             }
               x = 1 - stacktop->value*stacktop->value;
               y = sqrt(x);
               SECOND = -stacktop->second[i][j]/y -
                  stacktop->deriv[i]*stacktop->deriv[j]*stacktop->value/x/y;
               FIRST /= -y;
               stacktop->value = acos(stacktop->value);
               break;
           }

       case TAN:
           { int i,j;
               stacktop->value = tan(stacktop->value);
               x = 1+stacktop->value*stacktop->value; /* sec^2 */
               SECOND = x*stacktop->second[i][j] +
                 2*x*stacktop->value*stacktop->deriv[i]*stacktop->deriv[j];
               FIRST *= x;
                break;
           }

       case SINH:
           { int i,j;
             y = exp(stacktop->value);
             x = (y+1/y)/2;
             y = (y-1/y)/2;
             SECOND = y*stacktop->deriv[i]*stacktop->deriv[j]
                         + x*stacktop->second[i][j];
             FIRST *= x;
             stacktop->value = y;
             break ;
           }
       
       case COSH:
           { int i,j;
             y = exp(stacktop->value);
             x = (y-1/y)/2;
             y = (y+1/y)/2;
             SECOND = y*stacktop->deriv[i]*stacktop->deriv[j]
                         + x*stacktop->second[i][j];
             FIRST *= x;
             stacktop->value = y;
             break;
           }

       case TANH:
           { int i,j;
             y = exp(stacktop->value);
             x = (y-1/y)/2;
             y = (y+1/y)/2;
             SECOND = -2*x/y/y/y*stacktop->deriv[i]*stacktop->deriv[j]
                         + 1/y/y*stacktop->second[i][j];
             FIRST  /= y*y ;
             stacktop->value = x/y;
             break;
           }

       case ASINH:
           { int i,j;
             y = 1/sqrt(1+stacktop->value*stacktop->value);
             x = -stacktop->value*y*y*y;
             SECOND = x*stacktop->deriv[i]*stacktop->deriv[j]
                         + y*stacktop->second[i][j];
             FIRST *= y;
             stacktop->value = log(stacktop->value + 
                      sqrt(stacktop->value*stacktop->value + 1));
             break;
           }

       case ACOSH:
           { int i,j;
             if ( stacktop->value <= 1.0 )
             { sprintf(errmsg,"Acosh argument less than 1.\n");
               sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                 file_names[node->file_no],node->line_no);
               kb_error(2572,errmsg,RECOVERABLE);
             }
             y = 1/sqrt(stacktop->value*stacktop->value - 1);
             x = -stacktop->value*y*y*y;
             SECOND = x*stacktop->deriv[i]*stacktop->deriv[j]
                         + y*stacktop->second[i][j];
             FIRST *= y;
             stacktop->value = 2*log(sqrt(stacktop->value + 1) +
                   sqrt(stacktop->value - 1)) - log(2.0);
             break;
           }

       case ATANH:
           { int i,j;
             if ( fabs(stacktop->value)  >= 1 )
             { sprintf(errmsg,"Atanh argument magnitude greater than 1.\n");
               sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                 file_names[node->file_no],node->line_no);
               kb_error(2573,errmsg,RECOVERABLE);
             }
             y = 1/(1 - stacktop->value*stacktop->value);
             x = 2*stacktop->value*y*y;
             SECOND = x*stacktop->deriv[i]*stacktop->deriv[j]
                         + y*stacktop->second[i][j];
             FIRST *= y;
             stacktop->value = log(stacktop->value + 1)/2 -
                   log(stacktop->value - 1)/2;
             break;
           }

       case CHS:
           { int i,j;
             SECOND *= -1;
             FIRST = -stacktop->deriv[i];
             stacktop->value = -stacktop->value;
             break;
           }
       
       case INV:
           { int i,j;
             if ( stacktop->value == 0.0 )
               { sprintf(errmsg,"Division by zero.\n");
                 sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                    file_names[node->file_no],node->line_no);
                 kb_error(2574,errmsg,RECOVERABLE);
               }
             SECOND = (2*stacktop->deriv[i]*stacktop->deriv[i]/stacktop->value
                 - stacktop->second[i][j])/stacktop->value/stacktop->value;
             FIRST /= stacktop->value*stacktop->value;
             stacktop->value = 1/stacktop->value;
             break;
           }

       case ELLIPTICK:
           { int i,j;
             x = stacktop->value;
             stacktop->value = ellipticK(x);
             SECOND = ellipticKdmdm(x)*stacktop->deriv[i]*stacktop->deriv[j]
                          + ellipticKdm(x)*stacktop->second[i][j];
             FIRST = ellipticKdm(x)*stacktop->deriv[i];
             break;
           }

       case ELLIPTICE:
           { int i,j;
             x = stacktop->value;
             stacktop->value = ellipticE(x);
             SECOND = ellipticEdmdm(x)*stacktop->deriv[i]*stacktop->deriv[j]
                          + ellipticEdm(x)*stacktop->second[i][j];
             FIRST = ellipticEdm(x)*stacktop->deriv[i];
             break;
           }
 
       case INCOMPLETE_ELLIPTICF:
            { REAL phi,m,val,dm,dphi,ddm,ddphi,dmdphi;
              int i,j;
              stacktop--;
              phi = stacktop[0].value;
              m = stacktop[1].value;
              val = incompleteEllipticFseconds(phi,m,
                                     &dphi,&dm,&ddphi,&ddm,&dmdphi);
              stacktop->value = val;
              SECOND = ddphi*stacktop[0].deriv[i]*stacktop[0].deriv[j]
                + ddm*stacktop[1].deriv[i]*stacktop[1].deriv[j]
                + dmdphi*(stacktop[0].deriv[i]*stacktop[1].deriv[j]
                        + stacktop[1].deriv[i]*stacktop[0].deriv[j])
                + dphi*stacktop[0].second[i][j]
                + dm*stacktop[1].second[i][j];
              FIRST = dphi*stacktop[0].deriv[i]+dm*stacktop[1].deriv[i];
              break;
            }
 
       case INCOMPLETE_ELLIPTICE:
            { REAL phi,m,val,dm,dphi,ddm,ddphi,dmdphi;
              int i,j;
              stacktop--;
              phi = stacktop[0].value;
              m = stacktop[1].value;
              val = incompleteEllipticEseconds(phi,m,
                                   &dphi,&dm,&ddphi,&ddm,&dmdphi);
              stacktop->value = val;
              SECOND = ddphi*stacktop[0].deriv[i]*stacktop[0].deriv[j]
                + ddm*stacktop[1].deriv[i]*stacktop[1].deriv[j]
                + dmdphi*(stacktop[0].deriv[i]*stacktop[1].deriv[j]
                        + stacktop[1].deriv[i]*stacktop[0].deriv[j])
                + dphi*stacktop[0].second[i][j]
                + dm*stacktop[1].second[i][j];
              FIRST = dphi*stacktop[0].deriv[i]+dm*stacktop[1].deriv[i];
              break;
            }
       /* attribute values */
       case GET_SQ_MEAN_CURV_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             stacktop->value = vertex_sq_mean_curvature(id);
             zero_seconds(pcount,stacktop);
             break;

       case GET_MEANCURV_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             stacktop->value = vertex_mean_curvature(id);
             zero_seconds(pcount,stacktop);
             break;

       case LENGTH_:
       case GET_LENGTH_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             calc_edge(id);
             ++stacktop;
             stacktop->value = get_edge_length(id);
             zero_seconds(pcount,stacktop);
             break;

       case GET_DIHEDRAL_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             ++stacktop;
             if ( id_type(id) == EDGE )    stacktop->value = dihedral(id);
             else if ( id_type(id) == VERTEX )    stacktop->value = vertex_angle(id);
             else    stacktop->value = 0.0;
             zero_seconds(pcount,stacktop);
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
             zero_seconds(pcount,stacktop);
             break;

       case GET_EDGE_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             ++stacktop;
             stacktop->value = (REAL)(ordinal(get_fe_edge(id))+1);
             zero_seconds(pcount,stacktop);
             break;

       case GET_FACET_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             ++stacktop;
             stacktop->value = (REAL)(ordinal(get_fe_facet(id))+1);
             zero_seconds(pcount,stacktop);
             break;

          case AREA_:
          case GET_AREA_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             ++stacktop;
             stacktop->value = get_facet_area(id);
             zero_seconds(pcount,stacktop);
             break;

          case GET_WRAP_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             ++stacktop;
             stacktop->value = (REAL)get_edge_wrap(id); 
             zero_seconds(pcount,stacktop);
             break;

          case GET_PRESSURE_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             ++stacktop;
             switch ( id_type(id) )
               { 
                  case BODY:    stacktop->value = get_body_pressure(id); break;
                  default: sprintf(errmsg,"Pressure only for bodies.\n");
                   sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                    file_names[node->file_no],node->line_no);
                    kb_error(1003,errmsg,RECOVERABLE);
               }
             zero_seconds(pcount,stacktop);
             break;

          case GET_USERATTR_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             ++stacktop;
             stacktop->value = user_attribute(id);
             zero_seconds(pcount,stacktop);
             break;

          case GET_QUANTITY_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             ++stacktop;
             stacktop->value = quantity_attribute(id,node->op2.quant_id);
             zero_seconds(pcount,stacktop);
             break;

          case GET_INSTANCE_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             ++stacktop;
             stacktop->value = instance_attribute(id,node->op2.meth_id);
             zero_seconds(pcount,stacktop);
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
                  default: sprintf(errmsg,"Phase of wrong type element.\n");
                   sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                     file_names[node->file_no],node->line_no);
                   kb_error(1010,errmsg,RECOVERABLE);
               }
             zero_seconds(pcount,stacktop);
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
                 default: sprintf(errmsg,"Density of wrong type element.\n");
                  sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                    file_names[node->file_no],node->line_no);
                  kb_error(1018,errmsg,RECOVERABLE);
               }
             zero_seconds(pcount,stacktop);
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
             zero_seconds(pcount,stacktop);
             break;

          case VOLUME_:
          case GET_VOLUME_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             ++stacktop;
             stacktop->value = get_body_volume(id);
             zero_seconds(pcount,stacktop);
             break;

          case GET_VOLCONST_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             (++stacktop)->value = get_body_volconst(id);
             zero_seconds(pcount,stacktop);
             break;

          case GET_TARGET_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             (++stacktop)->value = get_body_fixvol(id);
             zero_seconds(pcount,stacktop);
             break;

          case GET_MPI_TASK_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             (++stacktop)->value = id_task(id);
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
             zero_seconds(pcount,stacktop);
             break;

          case ORIGINAL_:
          case GET_ORIGINAL_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             ++stacktop;
             stacktop->value = valid_id(id)?(REAL)ordinal(get_original(id))+1:0;
             zero_seconds(pcount,stacktop);
             break;

          case GET_COLOR_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             ++stacktop;
             switch ( id_type(id) )
              { case EDGE:     stacktop->value = (REAL)get_edge_color(id); break;
                 case FACET:     stacktop->value = (REAL)get_facet_color(id); break;
                 default:    stacktop->value = 0.0;
              }
             zero_seconds(pcount,stacktop);
             break;


          case GET_FRONTCOLOR_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             ++stacktop;
             switch ( id_type(id) )
              { case FACET: stacktop->value = (REAL)get_facet_frontcolor(id);
                     break;
                default:    stacktop->value = 0.0;
              }
             zero_seconds(pcount,stacktop);
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
             zero_seconds(pcount,stacktop);
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
             zero_seconds(pcount,stacktop);
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
             zero_seconds(pcount,stacktop);
             break;

          case TAG_:
          case GET_TAG_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             ++stacktop;
             stacktop->value = (REAL)get_tag(id);
             zero_seconds(pcount,stacktop);
             break;

          case BARE_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             ++stacktop;
             stacktop->value = (get_attr(id) & BARE_NAKED) ? 1.0 : 0.0;
             zero_seconds(pcount,stacktop);
             break;

          case GET_MIDV_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             ++stacktop;
             stacktop->value = ordinal(get_edge_midv(id)) + 1.;
             zero_seconds(pcount,stacktop);
             break;

          case FIXED_:
          case GET_FIXED_:
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             ++stacktop;
             stacktop->value = (get_attr(id) & FIXED) ? 1.0 : 0.0;
             zero_seconds(pcount,stacktop);
             break;

          case GET_EXTRA_ATTR_:
           { struct extra *ext;
             int spot,k;
             int i; 
             
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
                 sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                     file_names[node->file_no],node->line_no);
                 kb_error(2578,errmsg,RECOVERABLE);
               }
               spot += (int)(stacktop[-ext->array_spec.dim+k+1].value) - 1;
             }
             stacktop -= ext->array_spec.dim;
             if ( id_type(id) != node->op2.eltype )
             { if ( (id_type(id)==EDGE) && (node->op2.eltype==VERTEX) && params )
               { ext = EXTRAS(VERTEX) + n;
                 (++stacktop)->value = interp_edge_attribute(id,ext,spot,(int)params[2*SDIM]);
                 FIRST = 0.0; zero_seconds(pcount,stacktop);
                 break;
               }
               else 
               if ( (id_type(id)==FACET) && (node->op2.eltype==VERTEX) && params )
               { ext = EXTRAS(VERTEX) + n;
                 (++stacktop)->value = interp_facet_attribute(id,ext,spot,(int)params[2*SDIM]);
                 FIRST = 0.0; zero_seconds(pcount,stacktop);
                 break;
               }
               else 
               { sprintf(errmsg,
                   "Attribute %s is %s attribute, not %s attribute (in %s).\n",
                   EXTRAS(node->op2.eltype)[n].name,
                     typenames[node->op2.eltype], typenames[id_type(id)],ex->name);
                 sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                     file_names[node->file_no],node->line_no);
                 kb_error(2538,errmsg,RECOVERABLE);
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
               case ELEMENTID_TYPE: 
               case VERTEX_TYPE: 
               case EDGE_TYPE: 
               case FACET_TYPE: 
               case BODY_TYPE: 
               case FACETEDGE_TYPE: 
                (++stacktop)->value = (REAL)((element_id*)get_extra(id,n))[spot];
                break;
             }
             FIRST = 0.0; zero_seconds(pcount,stacktop);
             break;
           }

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
                 default: sprintf(errmsg,
                     "Can't do constraints on this type element.\n");
                   sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                      file_names[node->file_no],node->line_no);
                   kb_error(1052,errmsg,RECOVERABLE);
              }
             }
             zero_seconds(pcount,stacktop);
             break;

          case HIT_CONSTRAINT_:
           { 
             int testcon = (int)(stacktop--)->value;
             if ( node->op1.localnum ) 
                id = *(element_id*)get_localp(node->op1.localnum);
             else id = q_id;
             ++stacktop;
             switch(id_type(id))
              { case VERTEX: stacktop->value =
                      (REAL)get_v_constraint_status(id,testcon);
                      break;
                 default: sprintf(errmsg,
                    "Can do hit_constraints only on vertices.\n");
                   sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                      file_names[node->file_no],node->line_no);
                   kb_error(1053,errmsg,RECOVERABLE);
              }
           }
           zero_seconds(pcount,stacktop);
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
                default: sprintf(errmsg,
                   "Can't do boundary on this type element.\n");
                  sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                   file_names[node->file_no],node->line_no);
                  kb_error(1054,errmsg,RECOVERABLE);
              }
             stacktop->value = (b == web.boundaries+testb) ? 1.0 : 0.0;
           }
           zero_seconds(pcount,stacktop);
           break;

       /* whole-array syntax */

   case ARRAYIDENT_: /* push datastart for array */
        { struct global *glvalue = globals(node->op2.name_id);
          struct array *alvalue = glvalue->attr.arrayptr;
          if ( glvalue->flags & FIXED_SIZE_ARRAY )
            *(REAL**)(++stacktop) = 
                (REAL*)get_localp(glvalue->attr.arrayptr->datastart);
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
          int i;
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
          int i,count;
          a = get_name_arrayptr(name1,NULL,localbase);
          b = get_name_arrayptr(name2,NULL,localbase);
          count = (a->datacount < b->datacount) ? a->datacount : b->datacount;
          datastart1 = *(REAL**)(stacktop--);
          datastart2 = *(REAL**)(stacktop--);
          for ( sum = 0.0, i = 0 ; i < count ; i++ )
            sum += datastart1[i]*datastart2[i];
          (++stacktop)->value = sum;
          zero_seconds(pcount,stacktop);
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
            kb_error(3010,errmsg,RECOVERABLE);
          }
          if ( k > a->sizes[i] )
          { sprintf(errmsg,"Array index %d of array %s is %d; exceeds bound of %d.\n",
               i+1,get_name_name(node->op2.name_id,localbase),k,a->sizes[i]);
            sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                 file_names[node->file_no],node->line_no);
            kb_error(3009,errmsg,RECOVERABLE);
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
        zero_seconds(pcount,stacktop);

        break;
      }

      /* end whole-array syntax */

          default:
             sprintf(errmsg,"Bad expression eval_second() node type: %s.",
                   tokname(node->type));
             sprintf(errmsg+strlen(errmsg),"(source file %s, line %d)\n",
                 file_names[node->file_no],node->line_no);
             kb_error(1016,errmsg,RECOVERABLE);
             break;
      }      
    if ( node == ex->root ) break;
  }      

  *fval = stacktop->value;
  for ( n = 0 ; n < pcount ; n++ )
  { partials[n] = stacktop->deriv[n]; 
    for ( m = 0 ; m < pcount ; m++ )
      seconds[n][m] = stacktop->second[n][m];
  }

  return;
} /* end eval_second() */




