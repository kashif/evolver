/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/***********************************************************************
*
*  File: check.c
*
*  Contents:  Various integrity and consistency checks.
*
*/

#include "include.h"

/* maximum errors of any type before aborting */
#define MAXERR 20
int wrap_check ARGS((void));
int check_body_facets ARGS((void));

/*********************************************************************
*
*  Function: run_checks()
*
*  Purpose:  Overall control of checking.
*/

int run_checks()
{
  int numerr = 0;
  if ( memdebug ) memory_report();
  numerr += list_check();
  if ( web.representation != SIMPLEX )
    { numerr += facetedge_check(REGCHECK);
      if ( web.skel[BODY].count > 0 )
        numerr += facet_body_check();
      numerr += collapse_check();
      numerr += vertex_facet_check();
    }
  numerr += method_check();
  if ( web.symmetry_flag ) numerr += wrap_check(); 

  #ifdef MPI_EVOLVER
  numerr += mpi_task_checks();
  #endif

  check_count = numerr;
  return numerr;
} /* end run_checks() */

/**********************************************************************
*
* Function: method_check()
*
* purpose: see if any method applied twice to an element.
*
* return: number of duplications.
*/

int method_check()
{ int type;
  element_id id;
  int count = 0;
  int k,n;

  for ( type = VERTEX; type < FACETEDGE ; type++ )
  { int meth_offset =  get_meth_offset(type);
    FOR_ALL_ELEMENTS(type,id)
     { struct element *e_ptr = elptr(id);
       int *instlist = (int*)((char*)e_ptr + meth_offset);
       for ( k = 0; k < (int)e_ptr->method_count ; k++ )
         for ( n = k+1; n < (int)e_ptr->method_count ; n++ )
           if ( abs(instlist[k]) == abs(instlist[n]) )
           { sprintf(msg,"%s %s has method %s twice.\n",
               typenames[type],ELNAME(id),METH_INSTANCE(abs(instlist[k]))->name);
             outstring(msg);
             count++;
           }
      }
  }
  return count;
} /* end method_check() */

/**********************************************************************
*
*  Function: list_check()
*
*  Purpose:  Check element linked lists.
*
*/

int list_check()
{
  int type;
  element_id id;
  int numerr = 0;


  /* free_discards(DISCARDS_ALL); */
  for ( type = 0 ; type < NUMELEMENTS ; type++ )
  { 
    element_id backid = NULLID;

    if ( (web.representation == SIMPLEX)  && (type == EDGE) ) continue;
    #ifndef HASH_ID
    /* check free list */
    {  int freecount,maxfree;
     maxfree = web.skel[type].maxcount - web.skel[type].count
                 - web.skel[type].discard_count;
    id = web.skel[type].free;
    freecount = 0;
    while ( id != NULLID )
    { freecount++;
      if ( freecount > maxfree )
      { sprintf(msg,"Type %d freelist has too many elements: %d.\n",
            type,freecount);
        erroutstring(msg);
        if ( ++numerr > MAXERR )
        {erroutstring("Too many errors.\n"); return numerr;}
        break;
      }
      if ( id_type(id) != type )
      { sprintf(msg,"Type %d freelist has bad id %lX\n",type,(unsigned long)id);
        erroutstring(msg);
        if ( ++numerr > MAXERR )
        {erroutstring("Too many errors.\n"); return numerr;}
        break;
      }
      if ( elptr(id)->backchain != backid )
      { sprintf(msg,
         "Type %d freelist has bad backchain %X instead of %X for id %lX\n",
            type, (unsigned long)elptr(id)->backchain,(unsigned long)backid,
           (unsigned long)id);
        erroutstring(msg);
        if ( ++numerr > MAXERR )
        {erroutstring("Too many errors.\n"); return numerr;}
      }
      backid = id;
       
      id = elptr(id)->forechain;
    } /* end while */
    if ( backid != web.skel[type].freelast )
    { sprintf(msg,"Type %d freelist has bad freelast %lX instead of %lX.\n",
          type, (unsigned long)web.skel[type].freelast,(unsigned long)backid);
      erroutstring(msg);
    }

    if ( freecount != maxfree )
    { sprintf(msg,"Type %d freelist has %d elements instead of %d.\n",
          type,freecount,maxfree);
      erroutstring(msg);
    }
    if ( !equal_id(id,NULLID) )
    { sprintf(msg,"Type %d freelist last id is non-null: %lX\n",type,
          (unsigned long)id);
      erroutstring(msg);
    }
    }
    #endif
    
#ifndef MPI_EVOLVER
  { int usedcount,maxused,discards;
    element_id prev_id;

    /* check used list */
    maxused = web.skel[type].count;
    id = web.skel[type].used;
    prev_id = NULLID;
    usedcount = 0;
    discards = 0;
    while ( valid_id(id) )
    { 
      if ( valid_element(id) )
        usedcount++;
      else discards++;

      if ( usedcount > maxused )
      { sprintf(msg,"Type %d usedlist has too many elements: %d.\n",
            type,usedcount);
        erroutstring(msg);
        if ( ++numerr > MAXERR )
        {erroutstring("Too many errors.\n"); return numerr;}
        break;
      }
      if ( id_type(id) != type )
      { sprintf(msg,"Type %d usedlist has bad id %lX of type %d\n",
            type,(unsigned long)id,id_type(id));
        erroutstring(msg);
        if ( ++numerr > MAXERR )
        { erroutstring("Too many errors.\n"); return numerr;}
        break;
      }
      if ( !equal_id(prev_id,elptr(id)->backchain) )
      { sprintf(msg,"Type %d used list id %lX has backchain %lX instead of %lX\n",
              type,(unsigned long)id,(unsigned long)elptr(id)->backchain,
             (unsigned long)prev_id);
        erroutstring(msg);
        if ( ++numerr > MAXERR )
        {erroutstring("Too many errors.\n"); return numerr;}
      }
      prev_id = id;
      id = elptr(id)->forechain;
    } /* end while */
    if ( usedcount != maxused )
    { sprintf(msg,"Type %d usedlist has %d elements.\n",type,usedcount);
      erroutstring(msg);
    }
    if ( discards != web.skel[type].discard_count )
    { sprintf(msg,"Type %d usedlist has %d elements.\n",type,usedcount);
      erroutstring(msg);
    }
    if ( !equal_id(id,NULLID) )
    { sprintf(msg,"Type %d usedlist last id is non-null: %lX\n",type,
          (unsigned long)id);
      erroutstring(msg);
    }
  }
#endif
  } /* end for loop */

  #ifdef MPI_EVOLVER
  for ( type = VERTEX ; type < NUMELEMENTS ; type++ )
  { int k;
    for ( k = 0 ; k < web.skel[type].maxcount ; k++ )
      if ( (web.skel[type].ibase[k]->local_id & OFFSETMASK) != k )
      { sprintf(msg,"Task %d: local_id is %08X on %s ibase[0x%X], self id %08X\n",
          this_task,(int)(web.skel[type].ibase[k]->local_id),
           typenames[type],k, (int)(web.skel[type].ibase[k]->self_id));
        erroutstring(msg);
      }
  }
  #endif
  
  return numerr;
} /* end list_check() */



/**********************************************************************
*
*  Function: facetedge_check()
*
*  Purpose:  Check integrity of facetedge chains both ways.
*/

int facetedge_check(flag)
int flag;  /* PRELIMCHECK for check before subdivision, else REGCHECK */
{
  vertex_id v_id;
  edge_id e_id;
  facet_id f_id;
  facetedge_id fe_id,last_fe;
  int count;
  int numerr = 0;
  int n;

  /* check facetedge chain consistencies */
  if ( numerr >= MAXERR ) numerr = 0;
  count = 0;
  MFOR_ALL_FACETEDGES(fe_id)
  { facetedge_id fe;

    f_id = get_fe_facet(fe_id);
 #ifdef MPI_EVOLVER
	if ( (web.representation==SOAPFILM) || (id_task(f_id)==this_task) )
#endif
   if ( valid_id(f_id) && !valid_element(f_id) )
     { sprintf(msg,"Facetedge  %s links to invalid facet %08lX.\n",
          ELNAME(fe_id), (unsigned long)f_id);
        erroutstring(msg);
     }
#ifdef MPI_EVOLVER
	if ( (web.representation==SOAPFILM) || (id_task(f_id)==this_task) )
#endif
    if ( valid_id(f_id) && !valid_element(get_facet_fe(f_id)) )
    { sprintf(msg,"Facetedge %s links to facet %s with invalid facetedge %s.\n",
        ELNAME(fe_id),ELNAME1(f_id),ELNAME2(get_facet_fe(f_id)));
      erroutstring(msg);
    }
    e_id = get_fe_edge(fe_id);
 #ifdef MPI_EVOLVER
    if ( mpi_corona_state > NO_CORONA || id_task(e_id) == this_task )
 #endif
    {
    if ( valid_id(e_id) && !valid_element(e_id) )
    { sprintf(msg,"Facetedge %s links to invalid edge %08lX.\n",
            ELNAME(fe_id),(unsigned long)e_id);
       erroutstring(msg);
    }
    if ( valid_id(e_id) && !valid_element(get_edge_fe(e_id)) )
     { sprintf(msg,"Facetedge %s links to edge %s with invalid facetedge %s.\n",
         ELNAME(fe_id),ELNAME1(e_id),ELNAME2(get_edge_fe(e_id)));
        erroutstring(msg);
     }
    }
    fe = get_prev_edge(fe_id);
    if ( valid_id(fe) && !equal_id(get_next_edge(fe),fe_id) )
      { sprintf(msg,"Facetedge %s has bad prev edge link\n",ELNAME(fe_id));
        erroutstring(msg);
        if ( ++numerr > MAXERR )
              {erroutstring("Too many errors.\n"); return numerr;}
      }
    fe = get_next_edge(fe_id);
    if ( valid_id(fe) && !equal_id(get_prev_edge(fe),fe_id) )
      { sprintf(msg,"Facetedge %s has bad next edge link\n",ELNAME(fe_id));
        erroutstring(msg);
        if ( ++numerr > MAXERR )
              {erroutstring("Too many errors.\n"); return numerr;}
      }
    fe = get_next_facet(fe_id);
    if ( valid_id(fe) && !equal_id(get_prev_facet(fe),fe_id) )
      { sprintf(msg,"Facetedge %s has bad next facet link\n",ELNAME(fe_id));
        erroutstring(msg);
        if ( ++numerr > MAXERR )
              {erroutstring("Too many errors.\n"); return numerr;}
      }
    fe = get_prev_facet(fe_id);
    if ( valid_id(fe) && !equal_id(get_next_facet(fe),fe_id) )
      { sprintf(msg,"Facetedge %s has bad prev facet link\n",ELNAME(fe_id));
        erroutstring(msg);
        if ( ++numerr > MAXERR )
              {erroutstring("Too many errors.\n"); return numerr;}
      }
    fe = get_next_edge(fe_id);
#ifndef MPI_EVOLVER
    if ( valid_id(fe) && !equal_id(get_fe_headv(fe_id),get_fe_tailv(fe)) )
      { sprintf(msg,"Facetedge %s head vertex disagrees with next tail.\n",
        ELNAME(fe_id));
        erroutstring(msg);
        if ( ++numerr > MAXERR )
              {erroutstring("Too many errors.\n"); return numerr;}
      }
#endif
    fe = get_prev_edge(fe_id);
#ifndef MPI_EVOLVER
    if ( valid_id(fe) && !equal_id(get_fe_tailv(fe_id),get_fe_headv(fe)) )
      { sprintf(msg,"Facetedge %s tail vertex disagrees with prev head.\n",
        ELNAME(fe_id));
        erroutstring(msg);
        if ( ++numerr > MAXERR )
              {erroutstring("Too many errors.\n"); return numerr;}
      }
#endif
    count++;
  }
  if ( count != web.skel[FACETEDGE].count )
    { sprintf(msg,"Only %d facetedges out of %ld generated.\n",
         count,web.skel[FACETEDGE].count);
      erroutstring(msg);
    }

    /* check that vertices have legit edge link */
  FOR_ALL_VERTICES(v_id)
  {
    e_id = get_vertex_edge(v_id);
    if ( !valid_id(e_id) ) continue; 
    if ( !valid_element(e_id) ) 
     { sprintf(errmsg,"Vertex %s has invalid edge link.\n",
          ELNAME(v_id)+1);
       kb_error(1829,errmsg,WARNING);
       continue;
     }
    else if (get_vattr(v_id) & Q_MIDPOINT)
     { if ( !equal_id(v_id,get_edge_midv(e_id)) )
        { sprintf(errmsg,"Vertex %s has bad midpoint edge link.\n",ELNAME(v_id));
          kb_error(1830,errmsg,WARNING);
          continue;
        }
     }
    else if (get_vattr(v_id) & Q_MIDEDGE)
    { vertex_id *v = get_edge_vertices(e_id);
      int i;
      for ( i = 0 ; i <= web.lagrange_order ; i++ )
         if ( equal_id(v_id,v[i]) ) break;
      if ( i > web.lagrange_order )
      { sprintf(errmsg,"Vertex %s has bad edge link.\n",ELNAME(v_id));
        kb_error(1831,errmsg,WARNING);
      }
    }
    else if (get_vattr(v_id) & Q_MIDFACET) { }
    else if ( !equal_id(v_id,get_edge_tailv(e_id)) )
     { sprintf(errmsg,"Vertex %s has bad edge link.\n",ELNAME(v_id));
       kb_error(1832,errmsg,WARNING);
       continue;
     }
  } /* end vertices */

  FOR_ALL_EDGES(e_id)
  { edge_id ee_id;

    v_id = get_edge_tailv(e_id);
    n = 0; ee_id = e_id;
    do 
    { ee_id = get_next_tail_edge(ee_id); 
      if ( ++n > 2*web.skel[EDGE].count )
      { sprintf(errmsg,"Vertex %s has bad edge loop from edge %s.\n",
            ELNAME(v_id),ELNAME1(e_id));
        kb_error(1833,errmsg,WARNING);
        break;
      }
    }
    while ( !equal_id(ee_id,e_id) );
    v_id = get_edge_headv(e_id);
    n = 0; ee_id = e_id;
    do 
    { ee_id = get_next_head_edge(ee_id); 
      if ( ++n > 2*web.skel[EDGE].count )
      { sprintf(errmsg,"Vertex %s has bad edge loop from edge %s.\n",
            ELNAME(v_id),ELNAME1(e_id));
        kb_error(1834,errmsg,WARNING);
        break;
      }
    }
    while ( !equal_id(ee_id,e_id) );
  } /* end edges */

  /* some more checks on edges */
  count = 0;
  bare_edge_count = 0;
  FOR_ALL_EDGES(e_id)
  { vertex_id v_id = get_edge_headv(e_id);
    facetedge_id first_fe;

#ifdef MPI_EVOLVER
    if ( mpi_corona_state > NO_CORONA || id_task(v_id) == this_task )
#endif
    if ( get_vattr(v_id) & AXIAL_POINT )
      { sprintf(msg,"Vertex %s is axial and not first vertex of edge %s.\n",
            ELNAME(v_id),ELNAME1(e_id));
        erroutstring(msg);
      }
    last_fe = NULLID;
    fe_id = first_fe = get_edge_fe(e_id);
    if ( valid_id(fe_id) ) do
    { edge_id ee_id = get_fe_edge(fe_id);
      if ( !equal_id(e_id,ee_id) )
         { sprintf(msg,"Facetedge %s on edge %s instead of %s.\n",
             ELNAME(fe_id),SELNAME1(ee_id), SELNAME2(e_id));
           erroutstring(msg);
           if ( ++numerr > MAXERR )
              {erroutstring("Too many errors.\n"); goto facet_check;}
         }
      count++;
      if ( count > web.skel[FACETEDGE].count )
      { sprintf(msg,"Bad chain of facetedges around edge %s.\n",
          ELNAME(e_id));
        erroutstring(msg);
        if ( ++numerr > MAXERR )
              {erroutstring("Too many errors.\n"); goto facet_check;}
      }
      last_fe = fe_id;
      fe_id = get_next_facet(fe_id);
    } while ( valid_id(fe_id) && !equal_id(fe_id,first_fe) );
    if ( last_fe == NULLID )
     { 
       if ( (web.representation == SOAPFILM) && (flag == REGCHECK) ) 
       { edge_id orig;
         bare_edge_count++;
         if ( get_eattr(e_id) & BARE_NAKED ) continue;
         sprintf(msg,"Edge %s has no facets.\n",ELNAME(e_id));
         erroutstring(msg);
         orig = get_original(e_id);
         if ( valid_id(orig) && !equal_element(orig,e_id) )
            { sprintf(msg,"      (originally edge %s)\n",ELNAME(e_id));
              erroutstring(msg);
            }
       }
     }
    else if ( !equal_id(get_edge_fe(e_id),get_next_facet(last_fe)) )
     { edge_id orig;
       sprintf(msg,"Facets around edge %s do not link up.\n",
          ELNAME(e_id));
       erroutstring(msg);
       orig = get_original(e_id);
       if ( valid_id(orig) && !equal_element(orig,e_id) )
          { sprintf(msg,"      (originally edge %s)\n",ELNAME(e_id));
            erroutstring(msg);
          }
     }
  }
#ifndef MPI_EVOLVER
  if ( count != web.skel[FACETEDGE].count )
  { sprintf(msg,"Edges have %d facetedges out of %ld used.\n",
        count,web.skel[FACETEDGE].count);
    erroutstring(msg);
    ++numerr;
  }
#endif

facet_check: 
  if ( numerr >= MAXERR ) numerr = 0;
  if ( web.representation == SOAPFILM ) 
  { int i;
    count = 0;
    MFOR_ALL_FACETS(f_id)
    { facetedge_id first_fe;
      int thiscount = 0;
      
      last_fe = NULLID;
      fe_id = first_fe = get_facet_fe(f_id);
      for ( i = 0 ; i < FACET_EDGES ; i++, fe_id = get_next_edge(fe_id) )
      { vertex_id v_id;
        edge_id e_id = get_fe_edge(fe_id);
        facet_id ff_id;
  #ifdef MPI_EVOLVER
       if ( mpi_corona_state == NO_CORONA )
       { if ( id_task(e_id) == this_task )
         { v_id = get_edge_tailv(e_id);
           if ( id_task(v_id) == this_task )
           { if ( (thiscount != 0) && (get_vattr(v_id) & AXIAL_POINT))
             { set_facet_fe(f_id,fe_id);       
             }
           }
         }
       }
       else
   #endif
       { v_id = get_edge_tailv(e_id);
         if ( (thiscount != 0) && (get_vattr(v_id) & AXIAL_POINT))
         { set_facet_fe(f_id,fe_id);       
         }
       }

        ff_id = get_fe_facet(fe_id);
        if ( !equal_id(f_id,ff_id) )
        { sprintf(msg,"Facetedge %s on facet %s instead of %s.\n",
             ELNAME(fe_id),SELNAME1(ff_id), SELNAME2(f_id));
           erroutstring(msg);
           if ( ++numerr > MAXERR )
              {erroutstring("Too many errors.\n"); break;}
         }
         count++;
         if ( ++thiscount > web.skel[FACETEDGE].count )
         { facet_id orig;
           sprintf(msg,"Facetedge loop not closed on facet %s.\n",
             ELNAME(f_id));
           erroutstring(msg);
           orig = get_original(f_id);
           if ( valid_id(orig) && !equal_element(orig,f_id)  )
             { sprintf(msg,"      (originally facet %s)\n", ELNAME(orig));
                erroutstring(msg);
             }
           break;
         }
       last_fe = fe_id;
     } /* end while */
     fe_id = get_next_edge(last_fe);
     if ( !equal_id(first_fe,fe_id) )
     { facet_id orig;
       sprintf(msg,"Edges around facet %s do not link up.\n",
          ELNAME(f_id));
       erroutstring(msg);
       orig = get_original(f_id);
       if ( valid_id(orig) &&  !equal_element(orig,f_id) )
          { sprintf(msg,"      (originally facet %s)\n", ELNAME(orig));
            erroutstring(msg);
          }
       if ( ++numerr > MAXERR )
              {erroutstring("Too many errors.\n"); break;}
     }
     if ( (thiscount != 3) && (flag == REGCHECK)  )
     { facet_id orig;
       sprintf(msg,"Facet %s has %d edges.\n",ELNAME(f_id),thiscount);
       erroutstring(msg);
       orig = get_original(f_id);
       if ( valid_id(orig) && !equal_element(orig,f_id) )
          { sprintf(msg,"      (originally facet %s)\n", ELNAME(orig));
            erroutstring(msg);
          }
       if ( ++numerr > MAXERR )
          {erroutstring("Too many errors.\n"); break;}
     }
    }
    if ( count != web.skel[FACETEDGE].count )
     { sprintf(msg,"Facets have %d facet-edges out of %ld used.\n",
          count,web.skel[FACETEDGE].count);
       erroutstring(msg);
       ++numerr;
     }
  } /* end SOAPFILM */
  return numerr;
} /* end facetedge_check() */


/************************************************************************
*
* function: vertex_facet_check()
*
* purpose:  Check the vertex facet circularly linked lists.
*/
int vertex_facet_check()
{ vertex_id v_id;
  int numerr = 0;

  MFOR_ALL_VERTICES(v_id)
  { element_id f_id, start_f_id;
    int count = 0;
    start_f_id = get_vertex_first_facet(v_id);
    if ( !valid_id(start_f_id) )
      continue;
    f_id = start_f_id;
    do
    { f_id = get_next_vertex_facet(v_id,f_id);
      count++;
      if ( count > web.skel[FACET].count )
      { sprintf(msg,"Vertex %s has bad facet list.\n",ELNAME(v_id));
        erroutstring(msg);
        ++numerr;
        break;
      }
    } while ( !equal_id(f_id,start_f_id) );

  }

  if ( (numerr > 0) && (web.representation == SIMPLEX) )
  { erroutstring("Remaking vertex facet lists.\n");
    make_vfacet_lists();
  }

  return numerr;
}

/************************************************************************
*
*  Function: facet_body_check()
*
*  Purpose:  Check whether adjacent facets have same bodies.
*        Should be used only for complete bodies.
*     Also checks body facet lists.
*/

int facet_body_check()
{
  facetedge_id fe;
  facet_id f_id;
  body_id b_id;
  int numerr = 0;
  element_id orig;
      

  FOR_ALL_BODIES(b_id)
  { f_id = get_body_facet(b_id);
    if ( !valid_id(f_id) ) continue;
    if ( !valid_element(f_id) )
    { sprintf(msg,"Body %s has invalid facet link to facet %s.\n",
       ELNAME(b_id),ELNAME1(f_id));
      erroutstring(msg);
      numerr++;
      continue;
     }
     if ( !equal_id(b_id,get_facet_body(f_id) ) )
     { sprintf(msg,"Body %s has link to facet %s, which is on body %s.\n",
         ELNAME(b_id),SELNAME1(f_id),ELNAME2(get_facet_body(f_id)));
       erroutstring(msg);
       numerr++;
     }
  }

  if ( web.representation != SOAPFILM ) return numerr;

  FOR_ALL_FACETEDGES(fe)
  { body_id bb_id;
    facet_id ff_id;
    edge_id e_id;
    int k;

    for ( k = 0 ; k < 2 ; k++ )
    { fe = inverse_id(fe);
     
      f_id = get_fe_facet(fe);
      b_id = get_facet_body(f_id);
      if ( equal_id(fe,get_prev_facet(fe)) ) continue;
      ff_id = get_fe_facet(fe_inverse(get_prev_facet(fe)));
      bb_id = get_facet_body(ff_id);
      e_id = get_fe_edge(fe);
      if ( /* !equal_id(b_id,bb_id)  */
           (ordinal(b_id) != ordinal(bb_id))  /* for MPI, temp kludge */
             && !(get_attr(e_id)&(CONSTRAINT|FIXED|BOUNDARY)) )
      { sprintf(msg,"Inconsistent bodies for facets on edge %s.",
              ELNAME(e_id) );
        erroutstring(msg);
        numerr++;
        orig = get_original(e_id);
        if ( valid_id(orig) && !equal_element(orig,e_id) )
        { sprintf(msg,"      (originally edge %s)",ELNAME(orig));
          erroutstring(msg);
        }
        sprintf(msg,"\n     facet %d",oid(f_id));
        erroutstring(msg);
        orig = get_original(f_id);
        if ( valid_id(orig) && !equal_element(orig,f_id) )
        { sprintf(msg," (orig. %s)",ELNAME(orig)); erroutstring(msg); }
        if ( valid_id(b_id) )
        {
          sprintf(msg," has body %s",ELNAME(b_id));
            erroutstring(msg);
          orig = get_original(b_id);
          if ( valid_id(orig) && !equal_element(orig,b_id) )
             { sprintf(msg," (orig. %s)",ELNAME(orig));
               erroutstring(msg);
             }
        }
        else erroutstring(" has no body");
        sprintf(msg,"; facet %d",oid(ff_id));
        erroutstring(msg);
        orig = get_original(ff_id);
        if ( valid_id(orig) && !equal_element(orig,ff_id) )
        { sprintf(msg," (orig. %s)",ELNAME(orig));
          erroutstring(msg);
        }
        if ( valid_id(bb_id) )
        {
          sprintf(msg," has body %s",ELNAME(bb_id));
          erroutstring(msg);
          orig = get_original(bb_id);
          if ( valid_id(orig) && !equal_element(orig,bb_id) )
             { sprintf(msg," (orig. %s)",ELNAME(orig));
               erroutstring(msg);
             }
        }
        else erroutstring(" has no body");
        erroutstring(".\n");
        /* just a warning, not an error */
      }
    }
   } /* end FACETEDGES */
  numerr += check_body_facets();
  return numerr;
} /* end facet_body_check() */

/************************************************************************
*
* function: check_body_facets()
*
* purpose:  check body facet lists 
*/

int check_body_facets()
{ int numerr = 0;
  body_id b_id;

  FOR_ALL_BODIES(b_id)
  { facet_id  start_f,next_f;
    facet_id f_id = get_body_facet(b_id);
    if ( !valid_id(f_id) ) continue;
    start_f = f_id; 
    do 
    { if ( !valid_element(f_id) )
      { sprintf(msg,"Invalid facet %s on body facet list of body %s.\n",
              ELNAME(f_id),ELNAME1(b_id));
        erroutstring(msg);
        numerr++;
      }        
      if ( !equal_id(b_id,get_facet_body(f_id)) )
      { sprintf(msg,"Facet %s on body %s facet list, but is on body %s.\n",
          SELNAME(f_id),ELNAME1(b_id),ELNAME2(get_facet_body(f_id))); 
        erroutstring(msg);
        numerr++;
      }
      next_f = get_next_body_facet(f_id);  
      if ( !equal_id(f_id,get_prev_body_facet(next_f)) )
      { sprintf(msg,"Facet %s next body facet has bad prev_body_facet.\n",
         SELNAME(f_id));
        erroutstring(msg);
        numerr++;
        break;
      }
      f_id = next_f;
    } while ( !equal_id(f_id,start_f) );
  }
  return numerr;
}

/**********************************************************************
*
*  Function: collapse_check()
*
*  Purpose:  Checks whether adjacent facets have collapsed.
*        Tests whether they have the same third vertex.
*        Also checks for edges that have the same endpoints.
*/

int vvvvcomp(a,b)
struct vvvv *a,*b;
{ int i;
  for ( i = 0 ; i < 3 ; i++ )
  { if ( a->v[i] < b->v[i] ) return -1;
    if ( a->v[i] > b->v[i] ) return 1;
  }
  return 0;
}

int collapse_check()
{
  struct vvvv *vvlist,*current;
  facet_id f_id;
  edge_id e_id;
  vertex_id tmp;
  vertex_id v1,v2,v3;
  int i;
  int count;
  int numerr = 0;
  element_id orig;

  /* check edges */
  if ( web.skel[EDGE].count == 0 ) return numerr;
  vvlist = (struct vvvv *)temp_calloc(web.skel[EDGE].count,sizeof(struct vvvv));

  current = vvlist;
  count = 0;
  FOR_ALL_EDGES(e_id)
  { 
    v1 = get_edge_tailv(e_id);
    v2 = get_edge_headv(e_id);
    if ( equal_element(v1,v2) && !(get_vattr(v1) & AXIAL_POINT) )
    { sprintf(msg,"Edge %s",ELNAME(e_id));
      erroutstring(msg);
      orig = get_original(e_id);
      if ( valid_id(orig) && !equal_element(orig,e_id) )
      { sprintf(msg," (orig. %s)",ELNAME(orig));
        erroutstring(msg);
      }
      sprintf(msg," is loop on vertex %s",ELNAME(v1));
      erroutstring(msg);
      orig = get_original(get_edge_tailv(e_id));
      if ( valid_id(orig) && !equal_element(orig,v1) )
      { sprintf(msg," (orig. %s)",ELNAME(orig));
        erroutstring(msg);
      }
      erroutstring(".\n");
      numerr++;
    }
    if ( v1 <= v2 )
      { current->id = e_id;
        current->v[0] = v1;
        current->v[1] = v2;
      }
    else
      { current->id = inverse_id(e_id);
        current->v[0] = v2;
        current->v[1] = v1;
      }
    if ( ++count > web.skel[EDGE].count )
      { erroutstring("Edge count disagrees with supposed value.");
        count--;
        numerr++;
        break;
      }
    current++;
  }
  qsort((char *)vvlist,count,sizeof(struct vvvv),FCAST vvvvcomp);

  /* scan list for duplicates */
  for ( i = 0 ; i < count-1 ; i++ )
   { if ( vvvvcomp(vvlist+i,vvlist+i+1) == 0 )
     if ( !web.symmetry_flag ||
        (get_edge_wrap(vvlist[i].id) == get_edge_wrap(vvlist[i+1].id)) )
      { sprintf(msg,"Edges %s and %s have same endpoints: %s %s.\n",
         ELNAME(vvlist[i].id),ELNAME1(vvlist[i+1].id),
           ELNAME2(vvlist[i].v[0]), ELNAME3(vvlist[i].v[1]));
         erroutstring(msg);
         numerr++;
      }
   }
  temp_free((char*)vvlist);

  if ( web.representation == STRING ) return numerr;

#ifdef MPI_EVOLVER
  if ( mpi_corona_state == NO_CORONA )
     return numerr;
#endif

  /* check facets */
  if ( web.skel[FACET].count == 0 ) return numerr;
  vvlist = (struct vvvv *)temp_calloc(web.skel[FACET].count,sizeof(struct vvvv));

  current = vvlist;
  count = 0;
  FOR_ALL_FACETS(f_id)
  { 
    facetedge_id fe;
    fe = get_facet_fe(f_id);
    v1 = positive_id(get_fe_tailv(fe));
    v2 = positive_id(get_fe_headv(fe));
    v3 = positive_id(get_fe_headv(get_next_edge(fe)));
    /* bubble sort */
    if ( v1 > v2 )
      { tmp = v1; v1 = v2; v2 = tmp; invert(f_id); }
    if ( v2 > v3 )
      { tmp = v2; v2 = v3; v3 = tmp; invert(f_id); }
    if ( v1 > v2 )
      { tmp = v1; v1 = v2; v2 = tmp; invert(f_id); }
    current->id = f_id;
    current->v[0] = v1;
    current->v[1] = v2;
    current->v[2] = v3;
    if ( ++count > web.skel[FACET].count )
      { erroutstring("Facet count disagrees with supposed value.");
        count--;
        if ( ++numerr > MAXERR )
              {erroutstring("Too many errors.\n"); return numerr;}
        break;
      }
    current++;
  }
  qsort((char *)vvlist,count,sizeof(struct vvvv),FCAST vvvvcomp);

  /* scan list for duplicates */
  for ( i = 0 ; i < count-1 ; i++ )
  { if ( (vvvvcomp(vvlist+i,vvlist+i+1) == 0) 
          && !(get_vattr(vvlist[i].v[0]) & AXIAL_POINT)
          && !(get_vattr(vvlist[i].v[1]) & AXIAL_POINT)
          && !(get_vattr(vvlist[i].v[2]) & AXIAL_POINT)
       )
    { sprintf(msg,"Facets %s and %s have same vertices: %s %s %s.\n",
         ELNAME(vvlist[i].id),ELNAME1(vvlist[i+1].id), ELNAME2(vvlist[i].v[0]),
         ELNAME3(vvlist[i].v[1]),ELNAME4(vvlist[i].v[2]));
      erroutstring(msg);
      numerr++;
    }
  }
  temp_free((char*)vvlist);
  return numerr;
} /* end collapse_check() */



/**********************************************************************
*
*  Function:  normal_change_check()
*
*  Purpose:    Checks how much the normal of facets have changed
*         during motion. Returns the largest (delta normal)/(normal)
*/

REAL normal_change_check()
{
  facet_id f_id;
  facetedge_id fe;
  vertex_id v_id;
  REAL side[2][MAXCOORD];
  REAL max_delta;
  REAL old_normal[MAXCOORD];
  REAL new_normal[MAXCOORD];
  REAL x[3][MAXCOORD];
  int ord_v;
  REAL diff;
  struct boundary *bdry;
  int i,n;

  max_delta = 0.0;

  FOR_ALL_FACETS(f_id)
  { int ii;
    if ( get_fattr(f_id) & FIXED ) continue;

    /* get old normal */
    fe = get_facet_fe(f_id);
    for ( ii = 0 ; ii < FACET_EDGES ; ii++, fe = get_next_edge(fe) )
    {
      v_id = get_fe_tailv(fe);
      ord_v = loc_ordinal(v_id);
      if ( get_vattr(v_id) & BOUNDARY )     
      { bdry = get_boundary(v_id);
        for ( i = 0 ; i < SDIM ; i++ )
          x[ii][i] = eval(bdry->coordf[i],saved.coord[ord_v],v_id,NULL);
      }
      else
      {    
        for ( i = 0 ; i < SDIM ; i++ )
           x[ii][i] = saved.coord[ord_v][i];
      }
    }
    for ( i = 0 ; i < SDIM ; i++ )
    { side[0][i] = x[1][i] - x[0][i];
      side[1][i] = x[2][i] - x[0][i];
    }
    cross_prod(side[0],side[1],old_normal);

    /* get new normal */
    fe = get_facet_fe(f_id);
    for ( n = 0 ; n < FACET_EDGES ; n++, fe = get_next_edge(fe) )
    {
      v_id = get_fe_tailv(fe);
      for ( i = 0 ; i < SDIM ; i++ )
          x[n][i] = get_coord(v_id)[i];
    }
    for ( i = 0 ; i < SDIM ; i++ )
    { side[0][i] = x[1][i] - x[0][i];
      side[1][i] = x[2][i] - x[0][i];
    }
    cross_prod(side[0],side[1],new_normal);

    /* test difference */
    for ( i = 0 ; i < SDIM ; i++ )
     side[0][i] = new_normal[i] - old_normal[i];
    diff = sqrt(SDIM_dot(side[0],side[0])/
                         SDIM_dot(old_normal,old_normal));
    if ( diff > max_delta ) 
    max_delta = diff;
  }

  return max_delta;
} /* end normal_change_check() */

/********************************************************************
*
* function: wrap_check()
*
* purpose: check wraps around facet make identity, except for axial
*         point.  And checks torus wraps for validity.
*
* return: number of bad facets.
*/

int wrap_check()
{ edge_id e_id;
  facet_id f_id;
  int numerr = 0;
  int count = 0;
  int i;

  if ( web.torus_flag )
  FOR_ALL_EDGES(e_id)
  { WRAPTYPE wrap = get_edge_wrap(e_id);
    for ( i = 0 ; i < SDIM ; i++, wrap >>= TWRAPBITS )
      switch ( wrap & WRAPMASK  )
      {
          case  NEGWRAP :  break;
          case  0       :  break;
          case  POSWRAP :  break;
          default : 
              sprintf(errmsg,"Big wrap %d on edge %s period %d\n",
                  WRAPNUM(wrap&WRAPMASK),ELNAME(e_id),i+1);
              erroutstring(errmsg);
              numerr++;
            break;
      }
  }

  FOR_ALL_FACETS(f_id)
  {
     facetedge_id fe,start_fe;
     int wrap = 0;
     fe = start_fe = get_facet_fe(f_id);
     count = 0;
     if ( valid_id(fe) )
     do
     { if ( get_vattr(get_fe_tailv(fe)) & AXIAL_POINT )
       { wrap = 0; break; }
       wrap = (*sym_compose)(wrap,get_fe_wrap(fe));
       fe = get_next_edge(fe);
       count++;
       if ( count > 2*web.skel[EDGE].count ) 
       { sprintf(errmsg,"Facet %s has unclosed edge loop.\n",ELNAME(f_id));
         erroutstring(errmsg);
         numerr++;
         break;
       }
     } while ( valid_id(fe) && !equal_id(fe,start_fe) );
     if ( valid_id(fe) && (wrap != 0) )
       { sprintf(errmsg,"Wraps around facet %s not consistent.\n",ELNAME(f_id));
         kb_error(2000,errmsg,WARNING);
         numerr++;
       }
  }
  return numerr;
} /* end wrap_check() */



