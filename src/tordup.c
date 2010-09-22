/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/***********************************************************************
*
*  File:        tordup.c
*
*  Function:  tordup()
*
*  Purpose:    Duplicate a torus configuration  in the direction of
*              one period.  Meant for studying long-term behavior
*              of Ostwald ripening on ever larger scales.  Also good
*              for duplicating a single cell and resolving vertices
*              to try to beat Kelvin.
*/

#include "include.h"

static  int v_count;
static  int ecount;
static  int fcount;
static  int bcount;
static  int fecount;

static  element_id  *vlist,*elist,*flist,*felist,*blist;

void tordup(m)
int m;  /* period to duplicate, 0,1, or 2 */
{
  vertex_id v_id;
  edge_id    e_id;
  facet_id  f_id;
  body_id    b_id;
  facetedge_id fe_id;
  facetedge_id new_fe;
  facet_id new_f;
  body_id new_b;
  edge_id new_e;
  int i,j;
  REAL *x;
  REAL adjust;
  facetedge_id *body_fe_list;

  v_count = web.skel[VERTEX].max_ord+1;
  ecount = web.skel[EDGE].max_ord+1;
  fcount = web.skel[FACET].max_ord+1;
  bcount = web.skel[BODY].max_ord+1;
  fecount = web.skel[FACETEDGE].max_ord+1;

  /* allocate room for lists of old and new elements */
  vlist = (element_id *)temp_calloc(sizeof(element_id),2*v_count);
  elist = (element_id *)temp_calloc(sizeof(element_id),2*ecount);
  flist = (element_id *)temp_calloc(sizeof(element_id),2*fcount);
  blist = (element_id *)temp_calloc(sizeof(element_id),2*bcount);
  felist = (element_id *)temp_calloc(sizeof(element_id),2*fecount);
  body_fe_list = (element_id *)temp_calloc(sizeof(element_id),2*bcount);
  
  /* record old body data */
  FOR_ALL_BODIES(b_id)
  { blist[loc_ordinal(b_id)] = b_id;
    body_fe_list[loc_ordinal(b_id)] = get_body_fe(b_id);
  }

  /* relocate new vertices */
  FOR_ALL_VERTICES(v_id)
  { /* list old vertices */
    i = loc_ordinal(v_id);
    vlist[i] = v_id;
  }
  for ( i = 0 ; i < v_count ; i++ )
  { /* create corresponding new vertices */
    v_id = vlist[i];
    if ( !valid_id(v_id) ) continue;
    vlist[i+v_count] = dup_vertex(v_id);
    x = get_coord(vlist[i+v_count]);
    for ( j = 0 ; j < SDIM ; j++ )
      x[j] += web.torus_period[m][j];
  }  

  /* fix edge endpoints */
  FOR_ALL_EDGES(e_id)
  { /* list old edges */
    i = loc_ordinal(e_id);
    elist[i] = e_id;
  }
  for ( i = 0 ; i < ecount ; i++ )
  { /* create corresponding new edges */
    vertex_id h,t;
    WRAPTYPE wrap;

    e_id = elist[i];
    if ( !valid_id(e_id) ) continue;
    new_e = dup_edge(e_id);
    elist[i+ecount] = new_e;
    h = get_edge_headv(e_id);
    t = get_edge_tailv(e_id);
    if ( web.modeltype == QUADRATIC )
    { free_element(get_edge_midv(new_e));
      set_edge_midv(new_e,vlist[v_count+loc_ordinal(get_edge_midv(e_id))]);
    }
    wrap = get_edge_wrap(e_id);
    if ( (wrap >> (TWRAPBITS*m)) & WRAPMASK )
    { remove_vertex_edge(h,inverse_id(e_id));
      set_edge_headv(e_id,upgrade(h));
      set_edge_headv(new_e,h);
      set_edge_tailv(new_e,upgrade(t));
    }
    else
    { set_edge_headv(new_e,upgrade(h));
      set_edge_tailv(new_e,upgrade(t));
    }
  }
        
  /* new facets */
  FOR_ALL_FACETS(f_id)
  {
    i = loc_ordinal(f_id);
    flist[i]= f_id;
  }
  for ( i = 0 ; i < fcount ; i++ )
  { /* create corresponding new facets */

    f_id = flist[i];
    if ( !valid_id(f_id) ) continue;
    new_f = dup_facet(f_id);
    flist[i+fcount] = new_f;
     
  }

  /* new facet-edges */
  FOR_ALL_FACETEDGES(fe_id)
  {
    i = loc_ordinal(fe_id);
    felist[i] = fe_id;
  }
  for ( i = 0 ; i < fecount ; i++ )
  { /* create corresponding new facet-edges */

    fe_id = felist[i];
    if ( !valid_id(fe_id) ) continue;
    f_id = get_fe_facet(fe_id);
    e_id = get_fe_edge(fe_id);
    new_fe = new_facetedge(upgrade(f_id),upgrade(e_id));
    felist[i+fecount] = new_fe;
  }

  for ( i = 0 ; i < fecount ; i++ )
  { /* upgrade links */

    fe_id = felist[i];
    if ( !valid_id(fe_id) ) continue;  /* only want old ones */
    new_fe = upgrade(fe_id);
    set_next_edge(new_fe,upgrade(get_next_edge(fe_id)));
    set_prev_edge(new_fe,upgrade(get_prev_edge(fe_id)));
    set_next_facet(new_fe,upgrade(get_next_facet(fe_id)));
    set_prev_facet(new_fe,upgrade(get_prev_facet(fe_id)));
  }
  
  /* upgrade edge fe links */
  for ( i = 0 ; i < ecount ; i++ )
  {
    e_id = elist[i];
    if ( !valid_id(e_id) ) continue;
    fe_id = get_edge_fe(e_id);
    set_edge_fe(upgrade(e_id),upgrade(fe_id));
  }        

  /* go around old facets, linking in new facetedges if necessary */
  for ( i = 0 ; i < fcount ; i++ )
  {
    int newflag;
    facetedge_id fe,first_fe,new_next_fe,next_fe;

    f_id = flist[i];
    if ( !valid_id(f_id) ) continue;
    new_f = upgrade(f_id);

    newflag = 0;  /* whether we should be doing new fe's */
    fe = first_fe = get_facet_fe(f_id);
    if ( !valid_id(fe) ) continue;

    set_facet_fe(new_f,upgrade(fe));

    do
    {
      int sign;

      new_fe = upgrade(fe);
      if ( newflag )
      {
        set_fe_facet(fe,new_f);
        set_fe_facet(new_fe,f_id);
      }
      next_fe = get_next_edge(fe);
      new_next_fe = upgrade(next_fe);
      sign = 0;
      if ( !inverted(get_fe_edge(fe)) )
        if ( get_fe_wrap(fe) & (WRAPMASK<<(TWRAPBITS*m)) )
          sign++;
      if ( inverted(get_fe_edge(next_fe)) )
        if ( get_fe_wrap(next_fe) & (WRAPMASK<<(TWRAPBITS*m)) )
          sign++;
      if ( sign == 1 )
      {
        /* cross-link */
        set_next_edge(fe,new_next_fe);
        set_prev_edge(new_next_fe,fe);
        set_next_edge(new_fe,next_fe);
        set_prev_edge(next_fe,new_fe);
        newflag = !newflag;
      }
      fe = next_fe;
    }
    while ( !equal_id(fe,first_fe) );

    if ( newflag )
    { /* really only one facet, so get rid of new one */
      fe = first_fe;
      do 
      {
        set_fe_facet(fe,f_id);
        fe = get_next_edge(fe);
      }
      while ( !equal_id(fe,first_fe) );
      free_element(new_f);
    }
  }

  /* unwrapping appropriate edges */
  for ( i = 0 ; i < ecount ; i++ )
  { WRAPTYPE mask = WRAPMASK<<(TWRAPBITS*m);
    WRAPTYPE wrap,oldwrap,newwrap;
    int wrapnum;
     
    if ( !valid_id(elist[i]) ) continue;
    wrap = get_edge_wrap(elist[i]); 
    wrapnum = WRAPNUM((wrap>>(TWRAPBITS*m)) & WRAPMASK);
    oldwrap = (wrap & (~mask))|(((wrapnum>>1)&WRAPMASK)<<(TWRAPBITS*m));
    newwrap = (wrap & (~mask))|((((wrapnum+1)>>1)&WRAPMASK)<<(TWRAPBITS*m));
    set_edge_wrap(elist[i],oldwrap);
    set_edge_wrap(elist[i+ecount],newwrap);
  }

  /* create corresponding new facets */
  for ( i = 0 ; i < fcount ; i++ )
  {
    f_id = flist[i];
    if ( !valid_id(f_id) ) continue;
    new_f =  flist[i+fcount];
   
    /* erase body-facet links; will reset later */
    set_facet_body(f_id,NULLBODY);
    set_facet_body(inverse_id(f_id), NULLBODY);
    set_facet_body(new_f,NULLBODY);
    set_facet_body(inverse_id(new_f), NULLBODY);
   
  }
  
  /* new bodies */
  for ( i = 0 ; i < bcount ; i++ )
  { /* create corresponding new facet-edges */

    b_id = blist[i];
    if ( !valid_id(b_id) ) continue;
    new_b = dup_body(b_id);
    blist[i+bcount] = new_b;
    fe_id = body_fe_list[i];
    f_id = get_fe_facet(fe_id);
    set_facet_body(f_id,b_id);
    /* reset, since was wiped earlier */
    body_fe_list[bcount+i] = upgrade(fe_id);
    if ( get_battr(b_id) & HAVE_CENTEROFMASS )
    { REAL *oldcm = get_body_cm(b_id);
      REAL *newcm = get_body_cm(new_b);
      oldcm[m] = 0.5;
      newcm[m] = 0.5;
    }
  }

  /* adjust facet bodies */
  if ( web.representation == STRING )
  {
    for ( i = 0 ; i < fcount ; i++ )
      if ( valid_id(flist[i]) )
      { b_id = get_facet_body(flist[i]);
        if ( !valid_id(b_id) ) continue;
        b_id = upgrade(b_id);
        if ( valid_id(flist[i+fcount]) )
           set_facet_body(flist[i+fcount],b_id);
        else free_element(b_id);
      }
  }
  else  /* SOAPFILM */
  { /* have to go around finding contiguous facets of bodies */
    int changeflag;
    facet_id ff_id;

    /* start with canonical facets of old bodies */
    for ( i = 0 ; i < bcount ; i++ )
    {
      b_id = blist[i];
      if ( !valid_id(b_id) ) continue;
      f_id = get_body_facet(b_id);
      set_facet_body(f_id,b_id);
    }
 
    /* go around finding adjacent facets */
    do
    { facetedge_id sentinel;
      changeflag = 0;
      fe_id = NULLFACETEDGE;
      while ( generate_all(FACETEDGE,&fe_id,&sentinel) )
      { if ( equal_id(get_next_facet(fe_id),fe_id) )
          continue; /* valence 1 edge */
        f_id = get_fe_facet(fe_id);
        b_id = get_facet_body(f_id);
        if ( valid_id(b_id) ) 
        { ff_id = inverse_id(get_fe_facet(get_prev_facet(fe_id)));
          if ( !valid_id(get_facet_body(ff_id)) )
          { set_facet_body(ff_id,b_id);
            changeflag++;
          }
        }
        invert(fe_id);
        f_id = get_fe_facet(fe_id);
        b_id = get_facet_body(f_id);
        if ( valid_id(b_id) ) 
        { ff_id = inverse_id(get_fe_facet(get_prev_facet(fe_id)));
          if ( !valid_id(get_facet_body(ff_id)) )
          { set_facet_body(ff_id,b_id);
            changeflag++;
          }
        }
      }
    } while ( changeflag );

    /* now find new bodies whose canonical faces 
       have not been usurped */
    for ( i = 0 ; i < bcount ; i++ )
    {
      b_id = blist[i+bcount];
      if ( !valid_id(b_id) ) continue;

      f_id = get_fe_facet(body_fe_list[i+bcount]);
      if ( valid_id(get_facet_body(f_id)) )
      { /* two bodies are really one */
        free_element(b_id);
        set_body_volume(blist[i],2*get_body_volume(blist[i]),SETSTAMP);
        set_body_oldvolume(blist[i],2*get_body_oldvolume(blist[i]));
        if ( get_battr(b_id) & FIXEDVOL )
          set_body_fixvol(blist[i],2*get_body_fixvol(blist[i]));
        continue;
      } 
      set_facet_body(f_id,b_id);
    }
 
    /* go around finding adjacent facets */
    do
    { facetedge_id sentinel;
      changeflag = 0;
      fe_id = NULLFACETEDGE;
      while ( generate_all(FACETEDGE,&fe_id,&sentinel) )
      { if ( equal_id(get_next_facet(fe_id),fe_id) )
          continue; /* valence 1 edge */
        f_id = get_fe_facet(fe_id);
        b_id = get_facet_body(f_id);
        if ( valid_id(b_id) ) 
        { ff_id = inverse_id(get_fe_facet(get_prev_facet(fe_id)));
          if ( !valid_id(get_facet_body(ff_id)) )
          { set_facet_body(ff_id,b_id);
            changeflag++;
          }
        }
        invert(fe_id);
        f_id = get_fe_facet(fe_id);
        b_id = get_facet_body(f_id);
        if ( valid_id(b_id) ) 
        { ff_id = inverse_id(get_fe_facet(get_prev_facet(fe_id)));
          if ( !valid_id(get_facet_body(ff_id)) )
          { set_facet_body(ff_id,b_id);
            changeflag++;
          }
        }
      }
    } while ( changeflag );
  }
  /* adjust torus period and inverse period matrix */
  for ( i = 0 ; i < SDIM ; i++ )
  { sprintf(msg,"2*(%s)",print_express(&torus_period_expr[m][i],' '));
    cmdptr = msg;
    exparse(0,&torus_period_expr[m][i],USERCOPY);
    cmdptr = NULL;
  }
  if ( web.torus_display_period )
  for ( i = 0 ; i < SDIM ; i++ )
  { sprintf(msg,"2*(%s)",print_express(&torus_display_period_expr[m][i],' '));
    cmdptr = msg;
    exparse(0,&torus_display_period_expr[m][i],USERCOPY);
    cmdptr = NULL;
  }
  calc_periods(NO_ADJUST_VOLUMES);

  /* phase boundary energies */
  if ( phase_flag && (web.representation == STRING) )
    FOR_ALL_EDGES(e_id)
      set_e_phase_density(e_id);

  if ( phase_flag && (web.representation != STRING) )
    FOR_ALL_FACETS(f_id)
      set_f_phase_density(f_id);

  if ( everything_quantities_flag )
     reconvert_bodies_to_quantities();

  /* free lists */
  temp_free((char *)vlist);
  temp_free((char *)elist);
  temp_free((char *)flist);
  temp_free((char *)blist);
  temp_free((char *)felist);
  temp_free((char *)body_fe_list);

  /* fix up volconsts */

  calc_content(Q_INFO|Q_FIXED);  /* all volumes */

  /* get volume of piece of unit cell */
  if ( SDIM == 2 )
  {
     adjust = web.torusv  /* /2 */;
  }
  else /* web.representation == SOAPFILM */
  {
     adjust = web.torusv  /* /6 */;
  }

  /* adjust volconsts */
  FOR_ALL_BODIES(b_id)
    { REAL vol = get_body_volume(b_id);
      REAL old = get_body_oldvolume(b_id);
      REAL vc = get_body_volconst(b_id);
      REAL calcvol = vol-vc;
      REAL newvc = old - calcvol;
      newvc = adjust*floor(0.5+newvc/adjust);
      set_body_volconst(b_id,newvc);
      set_body_volume(b_id,calcvol+newvc,SETSTAMP);
   }

  top_timestamp = ++global_timestamp;

}

 
/**************************************************
*
*  Function: upgrade()
*
*  Purpose:  Find the new element corresponding 
*                to an old element.
*/

element_id upgrade(id)
element_id id;
{
    int j = loc_ordinal(id);
    element_id new_id;

    if ( !valid_id(id) ) return NULLID;

    switch( id_type(id) )
      {
         case VERTEX: new_id = vlist[j+v_count];
                      if ( !equal_id(id,vlist[j]) ) invert(new_id);
                      break;
         case EDGE  : new_id = elist[j+ecount];
                      if ( !equal_id(id,elist[j]) ) invert(new_id);
                      break;
         case FACET : new_id = flist[j+fcount];
                      if ( !equal_id(id,flist[j]) ) invert(new_id);
                      break;
         case BODY  : new_id = blist[j+bcount];
                      if ( !equal_id(id,blist[j]) ) invert(new_id);
                      break;
         case FACETEDGE: new_id = felist[j+fecount];
                      if ( !equal_id(id,felist[j]) ) invert(new_id);
                      break;
         default:     new_id = NULLID;
      }
    return new_id;
}

/********************************************************************
*
* function: reconvert_bodies_to_quantities()
*
* purpose: straightens out body quantities after messing around
*             with rebody() or tordup().  Erases all old instances
*             and recreates whole structure.
*/

void reconvert_bodies_to_quantities()
{ int type;
  int k;
  struct method_instance *mi;
  struct element *e_ptr;
  body_id b_id;
  element_id id;

  for ( type = VERTEX ; type <= FACET ; type++ )
  { int meth_offset = get_meth_offset(type);
    int *methlist;
    FOR_ALL_ELEMENTS(type,id)
    { e_ptr = elptr(id);
      methlist = (int*)((char*)e_ptr+meth_offset);
      for ( k = 0 ; k < (int)e_ptr->method_count ; k++ )
      { 
        mi = METH_INSTANCE(abs(methlist[k]));
        if ( mi->flags & BODY_INSTANCE )
        { methlist[k] = methlist[--(e_ptr->method_count)];
          k--;
        }
      }
    }
  }
  FOR_ALL_BODIES(b_id)
     convert_new_body_to_quantity(b_id);
  convert_bodies_to_quantities();
}

             

