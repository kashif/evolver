/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/***************************************************************
*  File: inline.h                                              *
*  Purpose: Functions eligible for inlining.  Various systems  *
*  have various things that need to be done to get inilning to *
*  work,                                                       * 
***************************************************************/

 
#if defined(INLINE)



INLINE void set_attr(id,attrib)
element_id id;
ATTR attrib;
{
  elptr(id)->attr |= attrib;
}

INLINE void unset_attr(id,attrib)
element_id id;
ATTR attrib;
{
  elptr(id)->attr &= ~attrib;
}

INLINE void set_fe_edge(fe_id,e_id)
facetedge_id fe_id;
edge_id e_id;
{
  if ( inverted(fe_id) ) invert(e_id);
  feptr(fe_id)->fe_edge_id = e_id;
  top_timestamp = ++global_timestamp;
}

INLINE edge_id get_fe_edge(fe_id)
facetedge_id fe_id;
{
  edge_id e_id;
    
  e_id = feptr(fe_id)->fe_edge_id;

  /*
  if ( inverted(fe_id) ) invert(e_id);
  return e_id;
  */
  
  return same_sign(e_id,fe_id);
}

INLINE facet_id get_fe_facet(fe_id)
facetedge_id fe_id;
{
  facet_id f_id;
  
  if ( !valid_id(fe_id) ) return NULLFACET;
  f_id = feptr(fe_id)->fe_facet_id;
  if ( inverted(fe_id) ) invert(f_id);
  return f_id;
}

INLINE facetedge_id get_prev_edge(fe_id)
facetedge_id fe_id;
{
  if ( inverted(fe_id) ) return inverse_id(feptr(fe_id)->nextedge[1]);
  else return feptr(fe_id)->nextedge[0];
}

INLINE facetedge_id get_next_edge(fe_id)
facetedge_id fe_id;
{
  if ( inverted(fe_id) ) return inverse_id(feptr(fe_id)->nextedge[0]);
  else return feptr(fe_id)->nextedge[1];
}

INLINE facetedge_id get_prev_facet(fe_id)
facetedge_id fe_id;
{
  if ( inverted(fe_id) ) 
    return inverse_id(feptr(fe_id)->nextfacet[1]);
  else 
    return feptr(fe_id)->nextfacet[0];
}

INLINE facetedge_id get_next_facet(fe_id)
facetedge_id fe_id;
{
  if ( inverted(fe_id) ) 
    return inverse_id(feptr(fe_id)->nextfacet[0]);
  else 
    return feptr(fe_id)->nextfacet[1];
}

INLINE void set_prev_edge(fe_id,fe)
facetedge_id fe_id;
facet_id fe;
{
  if ( !valid_id(fe_id) ) return;
  if ( inverted(fe_id) )
    { invert(fe);
      feptr(fe_id)->nextedge[1] = fe;
    }
  else
      feptr(fe_id)->nextedge[0] = fe;
  top_timestamp = ++global_timestamp;
}


INLINE void set_next_edge(fe_id,fe)
facetedge_id fe_id,fe;
{
  if ( !valid_id(fe_id) ) return;
  if ( inverted(fe_id) )
    { invert(fe);
      feptr(fe_id)->nextedge[0] = fe;
    }
  else
      feptr(fe_id)->nextedge[1] = fe;
}


INLINE void set_prev_facet(fe_id,fe)
facetedge_id fe_id,fe;
{
  if ( !valid_id(fe_id) ) return;
  if ( inverted(fe_id) )
  { invert(fe);
    feptr(fe_id)->nextfacet[1] = fe;
  }
  else
    feptr(fe_id)->nextfacet[0] = fe;
  top_timestamp = ++global_timestamp;
}


INLINE void set_next_facet(fe_id,fe)
facetedge_id fe_id,fe;
{
  if ( !valid_id(fe_id) ) return;
  if ( inverted(fe_id) )
  { invert(fe);
    feptr(fe_id)->nextfacet[0] = fe;
  }
  else
    feptr(fe_id)->nextfacet[1] = fe;
}


INLINE void set_edge_wrap(e_id,wrap)
edge_id e_id;
WRAPTYPE  wrap;
{
 *EINT(e_id,E_WRAP_ATTR) =  inverted(e_id)  ? (*sym_inverse)(wrap) : wrap ;
}

INLINE WRAPTYPE get_edge_wrap(e_id)
edge_id e_id;
{
  WRAPTYPE wrap = *EINT(e_id,E_WRAP_ATTR) ;
  return    ( inverted(e_id) ? (*sym_inverse)(wrap) : wrap );
}

INLINE void set_edge_fe(e_id,fe)
edge_id e_id;
facetedge_id fe;
{
  if ( inverted(e_id) ) invert(fe);
  eptr(e_id)->fe_id = fe;
  top_timestamp = ++global_timestamp;
}

INLINE facetedge_id get_edge_fe(e_id)
edge_id e_id;
{   struct edge *ep;
    facetedge_id fe;

 	ep = eptr(e_id);
    if ( !ep ) return NULLID;
    fe = ep->fe_id; 
    if ( inverted(e_id) ) invert(fe);
    return fe;
}

INLINE vertex_id get_edge_tailv(e_id)
edge_id e_id;
{
  if ( inverted(e_id) )
     return get_edge_vertices(e_id)[web.headvnum];
  else
     return get_edge_vertices(e_id)[0];
}

INLINE vertex_id get_edge_headv(e_id)
edge_id e_id;
{
  if ( inverted(e_id) )
     return get_edge_vertices(e_id)[0];
  else
     return get_edge_vertices(e_id)[web.headvnum];
}

INLINE void set_edge_tailv(e_id,v_id)
edge_id e_id;
vertex_id v_id;
{ vertex_id oldv;

  /* make sure edge not in loop of old vertex */
  oldv = get_edge_tailv(e_id);
  if ( valid_id(oldv) && !equal_id(oldv,v_id) )
    remove_vertex_edge(oldv,e_id);

  if ( inverted(e_id) )
     get_edge_vertices(e_id)[web.headvnum] = v_id;
  else
     get_edge_vertices(e_id)[0] = v_id;
  insert_vertex_edge(v_id,e_id);
  top_timestamp = ++global_timestamp;
}


INLINE void set_edge_headv(e_id,v_id)
edge_id e_id;
vertex_id v_id;
{
  if ( inverted(e_id) )
     get_edge_vertices(e_id)[0] = v_id;
  else
     get_edge_vertices(e_id)[web.headvnum] = v_id;
  insert_vertex_edge(v_id,inverse_id(e_id));
  top_timestamp = ++global_timestamp;
}

INLINE void set_edge_midv(e_id,v_id)
edge_id e_id;
vertex_id v_id;
{ get_edge_vertices(e_id)[2] = v_id;
  set_vertex_edge(v_id,e_id);
  set_attr(v_id,Q_MIDPOINT);
  top_timestamp = ++global_timestamp;
}


INLINE body_id get_facet_body(f_id)
facet_id f_id;
{
  if ( web.skel[BODY].count == 0 ) return NULLID;
  if ( !valid_id(f_id) ) return NULLID;
  if ( inverted(f_id) ) return F_ELID(f_id,F_BODY_LIST_ATTR)[1];
  else  return F_ELID(f_id,F_BODY_LIST_ATTR)[0];
}


INLINE facetedge_id get_facet_fe(f_id)
facet_id f_id;
{
  facetedge_id fe;
  
  if ( !valid_id(f_id) ) return NULLID;
  fe = fptr(f_id)->fe_id;
  if ( inverted(f_id) ) invert(fe);
  return fe;
}

INLINE edge_id get_next_tail_edge(e_id)  edge_id e_id;
{ return eptr(e_id)->next_vedge[inverted(e_id) ?1: 0] ; }

INLINE edge_id get_next_head_edge(e_id)  edge_id e_id;
{ return inverse_id(eptr(e_id)->next_vedge[inverted(e_id) ?0: 1]); }

INLINE void set_next_tail_edge(e_id,ee_id) edge_id e_id,ee_id;
{ eptr(e_id)->next_vedge[inverted(e_id) ?1: 0] = (ee_id); }

INLINE facet_id get_body_facet(b_id)  body_id b_id;
{ return ( valid_id(b_id) ? bptr(b_id)->f_id : NULLID ); }

INLINE facetedge_id get_body_fe(b_id)  body_id b_id;
{ facet_id f_id =  valid_id(b_id) ? bptr(b_id)->f_id : NULLID ;
  return valid_id(f_id) ? get_facet_fe(f_id) : NULLID;
}

INLINE facetedge_id get_vertex_fe(v_id) vertex_id v_id;
{ edge_id xx_id=vptr(v_id)->e_id;
  return  valid_id(xx_id)?same_sign(eptr(xx_id)->fe_id,xx_id):NULLID;
}

INLINE void set_body_facet(b_id,f_id)  body_id b_id; facet_id f_id;
{  if ( valid_id(b_id) )  bptr(b_id)->f_id = (f_id); }

INLINE REAL get_body_density(b_id) body_id b_id;
    { return  ( valid_id(b_id) ?  bptr(b_id)->density : 0.0 ) ; }

INLINE REAL get_body_volume(b_id) body_id b_id;
    { return    ( valid_id(b_id) ?  bptr(b_id)->volume : 0.0 ) ; }

INLINE REAL get_body_fixvol(b_id) body_id b_id;
     { return ( valid_id(b_id) ?  bptr(b_id)->fixvol : 0.0 ) ; }

INLINE REAL get_body_abstotal(b_id) body_id b_id;
     { return ( valid_id(b_id) ?  bptr(b_id)->abstotal : 0.0 ) ; }

INLINE REAL  get_body_pressure(b_id) body_id b_id;
    { return  ( valid_id(b_id) ?    bptr(b_id)->pressure : 0.0 ) ; }

INLINE REAL get_body_volconst(b_id) body_id b_id;
  { return    ( valid_id(b_id) ?  bptr(b_id)->volconst : 0.0 ) ; }

INLINE void set_body_density(b_id,v) body_id b_id; REAL v;
      {         ( valid_id(b_id) ?  bptr(b_id)->density = (v) : 0.0 ) ; }

INLINE void set_body_pressure(b_id,v) body_id b_id; REAL v;
      {         ( valid_id(b_id) ?  bptr(b_id)->pressure = (v) : 0.0 ) ; }

INLINE void set_body_volconst(b_id,v) body_id b_id; REAL v;
{ if ( !valid_id(b_id) ) return;
  bptr(b_id)->volconst = v;
  if ( everything_quantities_flag )
  { struct gen_quant *q = GEN_QUANT(get_body_volquant(b_id));
    q->volconst = v;
  }
}


/*************************************************************************
*
*  function: get_extra()
*
*  purpose: return pointer to value of extra attribute of an element.
*/

INLINE char *get_extra(id,n)
element_id id;
int n; /* number of extra attribute */
{ int type = id_type(id);

  return ( (char*)elptr(id) + EXTRAS(type)[n].offset);
}

/*************************************************************************
*
*  function: get_extra_ptr()
*
*  purpose: return pointer to value of extra attribute of an element.
*/

INLINE char *get_extra_ptr(id,ext)
element_id id;
struct extra *ext; /* extra attribute defining structure */
{ 
  return ( (char*)elptr(id) + ext->offset);
}


/***********************************************************************
*
* function: get_meth_offset()
*
* purpose: return offset of method instance list in element structure.
*/
INLINE int get_meth_offset(type)
int type; /* of element */
{ int meth_offset;
  meth_offset = EXTRAS(type)[web.meth_attr[type]].offset; 
  return meth_offset;
}

#endif
