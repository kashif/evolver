/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*****************************************************************
*
*  File: skeleton.c
*
*  Purpose: Variables and functions for skeleton element handling
*/

#include "include.h"

/* non-inlined versions of inlinable functions */
#ifndef INLINE
#define INLINE
#include "inline.h"
#endif

/* extra attribute type sizes, matching order in skeleton.h */

int datatype_size[NUMDATATYPES] = 
  {0,sizeof(REAL),sizeof(int),sizeof(unsigned long), sizeof(unsigned char),
   sizeof(unsigned short), sizeof(unsigned int),
   sizeof(long int),sizeof(char),sizeof(short),0,sizeof(char*),sizeof(char*),
   sizeof(vertex_id),sizeof(edge_id),sizeof(facet_id),sizeof(body_id),
   sizeof(facetedge_id),sizeof(element_id),sizeof(int),sizeof(int),sizeof(int),sizeof(int),
   sizeof(int)
  };

char *datatype_name[NUMDATATYPES] =
  { " ","real","integer","ulong_int","uchar","ushort_int","uint",
    "long_int","char","short_int"," ","string","pointer", "vertex_type",
    "edge_type","facet_type","body_type","facetedge_type", "element_id",
    "boundary_type","constraint_type","quantity_type","method_instance_type",
    "procedure"
  };

/* quadratic interpolation coefficients */
/* partials of interpolation polynomials at midpoints of patch edges */
/* control points are numbered 0 to 5 counterclockwise */
/* midpoints are numbered 0 to 2 counterclockwise after control pt 0 */
/* dip[midpoint][control point][partial]  */

REAL dip[FACET_EDGES][FACET_CTRL][2] = {
    { { -0.5, -0.5 }, { 0.0, -1.0 }, { 0.5, 0.0 }, { 0.0, 1.0 },
      { 0.0, -0.5 }, { 0.0, 1.0 } },
    { { 0.5, 0.5 }, { -1.0, -1.0 }, { 0.5, 0.0  }, { 1.0, 1.0 },
      { 0.0, 0.5 }, { -1.0, -1.0 } },
    { { -0.5, -0.5 }, { 1.0, 0.0 }, { -0.5, 0.0 }, { 1.0, 0.0 },
      { 0.0, 0.5 }, { -1.0, 0.0 } }
  };

int Ord(id)    /* useful in debugger */
element_id id;
{ return loc_ordinal(id); }

void vloop ARGS((vertex_id));

void vloop(v_id) /* prints vertex edge loop; useful in debugger */
element_id v_id;
{ edge_id e_id,ee_id;
  int n = 0;
  e_id = get_vertex_edge(v_id);
  if ( !valid_id(e_id) ) 
     { puts("No valid edge on vertex."); return; }
  ee_id = e_id;
  do { printf("%d ",inverted(ee_id)?-(Ord(ee_id)+1): (Ord(ee_id)+1)); 
       ee_id = get_next_tail_edge(ee_id);
       if ( ++n > web.skel[EDGE].count )
       { puts("Unclosed loop."); break;}
     } while ( ee_id != e_id );
  printf("\n");
}


void set_facet_body(f_id,b_id)
facet_id f_id;
body_id  b_id;  /* may be invalid for unsetting */
{ body_id bb_id;
  
  if ( web.skel[BODY].count == 0 ) return;
  if ( !valid_id(f_id) ) return;

#ifdef MPI_EVOLVER
  if ( id_task(f_id) != this_task )
	  return; /* imported facets not part of local body chains */
#endif

  bb_id = get_facet_body(f_id);
  if ( equal_id(bb_id,b_id) ) return;

  /* check if old body of this facet links to this facet */
  if ( valid_id(bb_id) )
  { 
    facet_id ff_id = get_body_facet(bb_id);
    facet_id next_f,prev_f;
    
    if ( valid_id(ff_id) ) 
    {
      if ( equal_id(f_id,ff_id) )  /* need to give body new link */
      { 
        prev_f = get_prev_body_facet(f_id);
        if ( equal_id(prev_f,f_id) || !valid_id(prev_f) ||
         !equal_id(bb_id,get_facet_body(prev_f))  )
            set_body_facet(bb_id,NULLID);
        else
          set_body_facet(bb_id,prev_f);
      }
     
      /* remove from old body facet list */
      next_f = get_next_body_facet(f_id);
      prev_f = get_prev_body_facet(f_id);
      set_next_body_facet(prev_f,next_f);
      set_prev_body_facet(next_f,prev_f);
    }
  }
 

  /* insert in new body list */
  if ( valid_id(b_id) )
  { 
    facet_id nextf = get_body_facet(b_id);
    if ( valid_id(nextf) )
    { facet_id prevf = get_prev_body_facet(nextf);
      set_next_body_facet(f_id,nextf);
      set_prev_body_facet(nextf,f_id);
      set_next_body_facet(prevf,f_id);
      set_prev_body_facet(f_id,prevf);
    }
    else /* first facet for body */
    { 
      set_next_body_facet(f_id,f_id);
      set_prev_body_facet(f_id,f_id);
      set_body_facet(b_id,f_id);
    }
  }

  if ( everything_quantities_flag )
  {
     if ( inverted(f_id) )  /* back facet */
     { 
        body_id bb_id = F_ELID(f_id,F_BODY_LIST_ATTR)[1];
        if ( equal_id(bb_id,b_id) ) return;
        if ( valid_id(bb_id) ) /* cancel out if already there */
          apply_method_num(inverse_id(f_id),get_body_volmeth(bb_id)); 
        F_ELID(f_id,F_BODY_LIST_ATTR)[1] = b_id;
        if ( valid_id(b_id) )
          apply_method_num(f_id,get_body_volmeth(b_id)); 
     }
     else /* front facet */
     { 
        body_id bb_id = F_ELID(f_id,F_BODY_LIST_ATTR)[0];
        if ( equal_id(bb_id,b_id) ) return;
        if ( valid_id(bb_id) )
          apply_method_num(inverse_id(f_id),get_body_volmeth(bb_id)); 
        F_ELID(f_id,F_BODY_LIST_ATTR)[0] = b_id;
        if ( valid_id(b_id) )
          apply_method_num(f_id,get_body_volmeth(b_id)); 
     }
     /* and content integrands on edges */
     if ( web.representation == SOAPFILM )
     { facetedge_id fe = get_facet_fe(f_id);
       edge_id e_id;
       int i;
       unsigned int k;
       conmap_t *map;
       char name[100];

       for ( i = 0 ; i < FACET_EDGES ; i++, fe = get_next_edge(fe))
       { e_id = get_fe_edge(fe);
         if ( !get_eattr(e_id) & BDRY_CONTENT ) continue;
         map = get_e_constraint_map(e_id);
         for ( k = 1 ; k <= map[0] ; k++ )
         { struct constraint *con = get_constraint(map[k]);
           int inst_num;
           if ( con->attr & CON_CONTENT )
           { if ( valid_id(bb_id) )
             { if ( con->attr & NAMED_THING )
                 sprintf(name,"body_%d_%s_meth",ordinal(bb_id)+1,con->name);
               else
                 sprintf(name,"body_%d_con_%d_meth",ordinal(bb_id)+1,map[k]);
               inst_num = find_method_instance(name);
               if ( inst_num < 0 )
                  inst_num = create_body_constraint_content_method(bb_id,map[k]);
               apply_method_num(inverse_id(e_id),inst_num); /* cancel */
             }
             if ( valid_id(b_id) )
             { if ( con->attr & NAMED_THING ) 
                 sprintf(name,"body_%d_%s_meth",ordinal(b_id)+1,con->name);
               else 
                 sprintf(name,"body_%d_con_%d_meth",ordinal(b_id)+1,map[k]);
               inst_num = find_method_instance(name);
               if ( inst_num < 0 )
                  inst_num = create_body_constraint_content_method(b_id,map[k]);
               apply_method_num(e_id,inst_num);
             }
           }
         }
       }
     }
  }
  else
  {
     if ( inverted(f_id) )  
       F_ELID(f_id,F_BODY_LIST_ATTR)[1] = b_id;
     else  
       F_ELID(f_id,F_BODY_LIST_ATTR)[0] = b_id;
  }
  top_timestamp = ++global_timestamp;

}
/**************************************************************************
*
* Function: set_facet_fe()
*
* Purpose: Link facet to facetedge.
*
*/
void set_facet_fe(f_id,fe)
facet_id f_id;
facetedge_id fe;
{
  if ( inverted(f_id) ) { invert(fe); invert(f_id); }
  fptr(f_id)->fe_id = fe;
  if ( web.representation == STRING )
  { body_id b_id = get_facet_body(f_id);
    if ( valid_id(b_id) )
       set_body_facet(b_id,f_id);
    b_id = get_facet_body(inverse_id(f_id));
    if ( valid_id(b_id) )
       set_body_facet(b_id,inverse_id(f_id));
  }
  top_timestamp = ++global_timestamp;
}


REAL get_vertex_length_star(v_id) vertex_id v_id;
{ edge_id e_id = get_vertex_edge(v_id);
  edge_id firste = e_id;
  REAL star = 0.0;
  if ( get_vattr(v_id) & (Q_MIDPOINT|Q_MIDEDGE) ) return get_edge_length(e_id);
  if ( !valid_id(e_id) ) return 0.0;
  do { star += get_edge_length(e_id); e_id = get_next_tail_edge(e_id);}
  while ( !equal_element(e_id,firste) );
  return star;
}

REAL get_vertex_area_star(v_id) vertex_id v_id;
{ 
  REAL star = 0.0;

  if ( web.representation == STRING ) 
      return get_vertex_length_star(v_id);
  else if ( web.representation == SOAPFILM )
  {
     facetedge_id fe_id = get_vertex_first_facet(v_id);
     facetedge_id firstfe = fe_id;
     facet_id f_id;
     if ( !valid_id(fe_id) ) return 0.0;
     do 
     { f_id = get_fe_facet(fe_id);
       star += get_facet_area(f_id); 
       fe_id = get_next_vertex_facet(v_id,fe_id);
     }
     while ( !equal_element(fe_id,firstfe) );
  }
  else /* SIMPLEX */
  {
     facet_id f_id = get_vertex_first_facet(v_id);
     facet_id firstf = f_id;
     if ( !valid_id(f_id) ) return 0.0;
     do 
     { star += get_facet_area(f_id); 
       f_id = get_next_vertex_facet(v_id,f_id);
     }
     while ( !equal_element(f_id,firstf) );
  } 
  return star;
}

int get_vertex_fvalence(v_id) vertex_id v_id;
{ 
 int valence = 0;
 facet_id f_id = get_vertex_first_facet(v_id);
 facet_id firstf = f_id;

 if ( !valid_id(f_id) ) return 0;
 do 
 { valence++; f_id = get_next_vertex_facet(v_id,f_id);}
 while ( !equal_element(f_id,firstf) );
 return valence; 
}

void set_next_body_facet(f_id,ff_id)  facet_id f_id,ff_id;
  { F_ELID(f_id,F_NEXT_BFACET_ATTR)[inverted(f_id)?1:0] = (ff_id) ; }

void set_prev_body_facet(f_id,ff_id)  facet_id f_id,ff_id;
  { F_ELID(f_id,F_NEXT_BFACET_ATTR)[inverted(f_id)?3:2] = (ff_id) ; }

facet_id get_next_body_facet(f_id) facet_id f_id;
{ return    F_ELID(f_id,F_NEXT_BFACET_ATTR)[inverted(f_id)?1:0]; }

facet_id get_prev_body_facet(f_id) facet_id f_id;
{ return    F_ELID(f_id,F_NEXT_BFACET_ATTR)[inverted(f_id)?3:2]; }

void set_body_volume(b_id,v,mode)  
body_id b_id; 
REAL v;
int mode; /* NOSETSTAMP or SETSTAMP */
{ struct body *b = bptr(b_id);
  if ( !valid_id(b_id) ) return;
  b->volume = v; 
  b->abstotal = fabs(v); 
  if ( mode == SETSTAMP )
    b->voltimestamp = global_timestamp;
/*
  if ( web.representation == STRING)
  { facet_id f_id = get_body_facet(b_id));
    if ( valid_id(f_id) ) set_facet_area(f_id,v);
  }
*/
  if ( everything_quantities_flag )
  { struct gen_quant *q = GEN_QUANT(b->volquant);
    q->value = v;
  }
} 

void add_body_volume(b_id,v)  /* uses binary tree add */ 
body_id b_id; REAL v;
{ struct body *b;
  if ( !valid_id(b_id) ) return;
  b = bptr(b_id);
  binary_tree_add(b->volume_addends,v);
  b->abstotal += fabs(v); 
}

void add_body_volume_plain(b_id,v)  /* with updates of related quantities */
body_id b_id; REAL v;
{ struct body *b;
  if ( !valid_id(b_id) ) return;
  b = bptr(b_id);
  b->volume += v;
  b->abstotal += fabs(v); 
  b->voltimestamp = global_timestamp;
/*
  if ( web.representation == STRING)
  { facet_id f_id = get_fe_facet(b->fe_id);
    if ( valid_id(f_id) ) set_facet_area(f_id,b->volume);
  }
*/
  if ( everything_quantities_flag )
  { struct gen_quant *q = GEN_QUANT(b->volquant);
    q->value = b->volume;
  }
} 

void add_body_abstotal(b_id,v)
body_id b_id; REAL v;
{ struct body *b;
  if ( !valid_id(b_id) ) return;
  b = bptr(b_id);
  b->abstotal += fabs(v); 
}

void set_body_fixvol(b_id,v) 
body_id b_id; 
REAL v;
{ if ( valid_id(b_id) )
  { bptr(b_id)->fixvol = (v); 
    if ( everything_quantities_flag )
    { struct gen_quant *q = GEN_QUANT(get_body_volquant(b_id));
      q->target = v;
      if ( !(q->flags & Q_FIXED) )
      { q->flags &= ~(Q_INFO|Q_ENERGY|Q_CONSERVED);
        q->flags |= Q_FIXED;
      }
      if ( web.pressure_flag )
      { q = GEN_QUANT(get_body_ambquant(b_id));
        q->flags &= ~(Q_INFO|Q_FIXED|Q_CONSERVED);
        q->flags |= Q_ENERGY;
      }
    }
  }
  else
  { sprintf(errmsg,"fix body volume: illegal body %s.\n",ELNAME(b_id));
    kb_error(1307,errmsg,RECOVERABLE);
  }
}

vertex_id new_vertex(x,parent)
REAL *x;
element_id parent; /* for inherited stuff */
{ 
  int i;
  vertex_id v_id;
  REAL *y;

  v_id = new_element(VERTEX,parent,NULLID);
  if ( x )
  { y = get_coord(v_id);
    for ( i = 0 ; i < SDIM ; i++ ) y[i] = x[i];
  }
  set_vertex_edge(v_id,NULLID);
  set_vertex_facet(v_id,NULLID);

  set_v_global(v_id);

  /* interrupt conjugate gradient */
  if ( cg_hvector ) { myfree((char *)cg_hvector); cg_hvector = NULL; }

  return v_id;
}

vertex_id dup_vertex(old_v)
vertex_id old_v;
{ 
  vertex_id v_id;
  struct vertex *v_id_p;

  v_id = new_element(VERTEX,NULLID,NULLID);
  v_id_p = vptr(v_id);
  memcpy((char *)&(v_id_p->attr),(char *)&(elptr(old_v)->attr),
              web.sizes[VERTEX] - ((char*)&(v_id_p->attr)-(char*)v_id_p));
  v_id_p->self_id = v_id;  /* restore new id */
  v_id_p->e_id = NULLID;
  return v_id;
}

edge_id    new_edge(tail_id,head_id,parent)
vertex_id tail_id,head_id;
element_id parent; /* for inherited stuff */
{ 
  edge_id e_id;
  vertex_id v_id;
  REAL *x,*h,*t;
  int i,k;

  e_id = new_element(EDGE,parent,NULLID);
  set_edge_fe(e_id,NULLFACETEDGE);
  set_edge_color(e_id,DEFAULT_EDGE_COLOR);
  if ( valid_id(tail_id) && valid_id(head_id) )
  { set_edge_tailv(e_id,tail_id);
     set_edge_headv(e_id,head_id);
     if ( (web.modeltype == QUADRATIC) && valid_id(head_id) )
     { /* quadratic version; linear interpolation of midpoint */
        v_id = new_element(VERTEX,parent,NULLID);
        set_edge_midv(e_id,v_id);
        h = get_coord(head_id);
        t = get_coord(tail_id);
        x = get_coord(v_id);
        for ( i = 0 ; i < SDIM ; i++ )
          x[i] = (h[i] + t[i])/2.0;
     }
     else if ( (web.modeltype == LAGRANGE) && valid_id(head_id) )
     { /* Lagrange version; linear interpolation of points */
        vertex_id *v = get_edge_vertices(e_id);
        h = get_coord(head_id);
        t = get_coord(tail_id);
        for ( k = 1 ; k < web.lagrange_order ; k++ )
        { v[k] = new_element(VERTEX,parent,NULLID);
          set_attr(v[k],Q_MIDEDGE);
          set_vertex_edge(v[k],e_id);
          x = get_coord(v[k]);
          for ( i = 0 ; i < SDIM ; i++ )
             x[i] = (k*h[i] + (web.lagrange_order-k)*t[i])/web.lagrange_order;
        }
     }
  }
  return e_id;
}

edge_id dup_edge(old_e)
edge_id old_e;
{ 
  edge_id e_id;
  struct edge *e_id_p,*old_e_p;
  vertex_id newmid; 

  e_id = new_element(EDGE,NULLID,NULLID);
  e_id_p = eptr(e_id);
  old_e_p = eptr(old_e);
  memcpy((char *)&(e_id_p->attr),(char *)&(old_e_p->attr),
              web.sizes[EDGE] - ((char*)&(e_id_p->attr)-(char*)e_id_p));
  e_id_p->self_id = e_id; /* restore new id */
  e_id_p->next_vedge[0] = NULLID;
  e_id_p->next_vedge[1] = NULLID;
  if ( web.modeltype == QUADRATIC )
  { newmid = new_vertex(NULL,old_e);
    set_edge_midv(e_id,newmid);
	set_attr(newmid,Q_MIDPOINT);
	set_vertex_edge(newmid,e_id);
  } 
  else if ( web.modeltype == LAGRANGE )
  { int i;
    vertex_id *v = get_edge_vertices(e_id);
    for ( i = 1 ; i < web.lagrange_order ; i++ )
      v[i] = new_vertex(NULL,e_id);
  } 
  if ( inverted(old_e) ) return inverse_id(e_id);
  return e_id;
}

void recalc_facet_area(f_id)
facet_id f_id;
{
  if ( everything_quantities_flag )
    quantity_attribute(f_id,default_area_quant_num);
  else
    (*calc_facet_energy)(f_id,AREA_ONLY);
}

facet_id  new_facet ()
{ 
  facet_id f_id;

  f_id = new_element(FACET,NULLID,NULLID);
  set_facet_color(f_id,DEFAULT_FACET_COLOR);
  set_facet_density(f_id,1.0);
  return f_id;
}

facet_id dup_facet(old_f)
facet_id old_f;
{ 
  facet_id f_id;
  struct facet *f_id_p;
  body_id b_id,*fb;
  int sign = inverted(old_f);

  old_f = positive_id(old_f);  /* since new facet id will be positive */
  
  f_id = new_element(FACET,NULLID,NULLID);
  f_id_p = fptr(f_id);
  memcpy((char *)&(f_id_p->attr),(char *)&(elptr(old_f)->attr),
              web.sizes[FACET] - ((char*)&(f_id_p->attr)-(char*)f_id_p));
  f_id_p->self_id = f_id; /* restore new id */
  set_attr(f_id,NEWELEMENT);

  /* update body facet lists */
  if ( web.skel[BODY].count )
  { fb = F_ELID(f_id,F_BODY_LIST_ATTR);
    fb[0] = fb[1] = NULLID; /* so set_facet_body works */
    fb = F_ELID(f_id,F_NEXT_BFACET_ATTR);
    fb[0] = fb[1] = fb[2] = fb[3] = NULLID;
    b_id = get_facet_body(old_f);
    if ( valid_id(b_id) )
      set_facet_body(f_id,b_id);
    b_id = get_facet_body(inverse_id(old_f));
    if ( valid_id(b_id) )
      set_facet_body(inverse_id(f_id),b_id);
  }

  return sign ? inverse_id(f_id) : f_id;
}

body_id new_body()
{ int two = 2;
  int four = 4;
  body_id b_id;

  expand_attribute(FACET,F_BODY_LIST_ATTR,&two);
  expand_attribute(FACET,F_NEXT_BFACET_ATTR,&four);
  b_id = new_element(BODY,NULLID,NULLID);
  set_body_facet(b_id,NULLID);
  if ( everything_quantities_flag ) 
      convert_new_body_to_quantity(b_id);
  web.bodycount++;
  return b_id;
}

body_id dup_body(old_b)
body_id old_b;
{ 
  body_id b_id;
  struct body *b_id_p;

  b_id = new_element(BODY,NULLID,NULLID);
  b_id_p = bptr(b_id);
  memcpy((char *)&(b_id_p->attr),(char *)&(elptr(old_b)->attr),
              web.sizes[BODY] - ((char*)&(b_id_p->attr)-(char*)b_id_p));
  b_id_p->self_id = b_id; /* restore new id */
  set_body_facet(b_id,NULLID);
  if ( everything_quantities_flag ) 
      convert_new_body_to_quantity(b_id);
  web.bodycount++;
  return b_id;
}

facetedge_id new_facetedge(f_id,e_id)
facet_id f_id;
edge_id e_id;
{ 
  facetedge_id fe_id;
 
  fe_id = new_element(FACETEDGE,NULLID,NULLID);
  set_fe_edge(fe_id,e_id);
  set_fe_facet(fe_id,f_id);
  set_prev_edge(fe_id,NULLFACETEDGE);
  set_next_edge(fe_id,NULLFACETEDGE);
  set_prev_facet(fe_id,NULLFACETEDGE);      
  set_prev_facet(fe_id,NULLFACETEDGE);
  #ifndef MPI_EVOLVER
  { vertex_id headv,tailv;
    tailv = get_edge_tailv(e_id);  
    headv = get_edge_headv(e_id);
    if ( !valid_id(get_vertex_edge(tailv)) )
      set_vertex_edge(tailv,e_id);
    if ( !valid_id(get_vertex_edge(headv)) )
      set_vertex_edge(headv,e_id);
  }
  #endif

  if ( web.representation==STRING && everything_quantities_flag )
  { /* attach volume quantities */
     body_id b_id;
     b_id = get_facet_body(f_id);
     if ( valid_id(b_id) )
     { if ( same_sign(f_id,e_id) )
          apply_method_num(e_id,get_body_volmeth(b_id));
        else
          apply_method_num(inverse_id(e_id),get_body_volmeth(b_id));
     }
     b_id = get_facet_body(inverse_id(f_id));
     if ( valid_id(b_id) )
     { if ( same_sign(f_id,e_id) )
          apply_method_num(inverse_id(e_id),get_body_volmeth(b_id));
        else
          apply_method_num(e_id,get_body_volmeth(b_id));
     }
  }
  return fe_id;
}


void set_fe_facet(fe_id,f_id)
facetedge_id fe_id;
facet_id f_id;
{ facet_id oldf = get_fe_facet(fe_id);

  feptr(fe_id)->fe_facet_id = inverted(fe_id) ? inverse_id(f_id) : f_id;

  if ( web.representation==STRING && everything_quantities_flag &&
        !equal_id(oldf,f_id) )
  { /* detach old volume quantity and attach new volume quantity */
     body_id b_id, old_b_id;
     edge_id e_id = get_fe_edge(fe_id);
     b_id = get_facet_body(f_id);

     old_b_id = valid_id(oldf) ? get_facet_body(oldf) : NULLID;
     if ( valid_id(old_b_id) )
        unapply_method(e_id,get_body_volmeth(old_b_id));
     old_b_id = valid_id(oldf) ? get_facet_body(inverse_id(oldf)) : NULLID;
     if ( valid_id(old_b_id) )
        unapply_method(e_id,get_body_volmeth(old_b_id));
              
              
     if ( valid_id(b_id) )
     { if ( same_sign(f_id,e_id) )
          apply_method_num(e_id,get_body_volmeth(b_id));
        else
          apply_method_num(inverse_id(e_id),get_body_volmeth(b_id));
     }
     b_id = get_facet_body(inverse_id(f_id));
     if ( valid_id(b_id) )
     { if ( same_sign(f_id,e_id) )
          apply_method_num(inverse_id(e_id),get_body_volmeth(b_id));
        else
          apply_method_num(e_id,get_body_volmeth(b_id));
     }
  }
}

REAL get_edge_length(e_id)
edge_id e_id;
{
  return    (eptr(e_id)->length);
}


REAL get_facet_pressure(f_id)
facet_id f_id;
{ 
  return  (get_body_pressure(get_facet_body(f_id)) - 
        get_body_pressure(get_facet_body(facet_inverse(f_id))));
}


/********************************************************************
*
* Function: compare_vertex_attr(va,vb)
* 
* Purpose: Compare constraints of vertex and vertex.
*
* Return value: 0 INCOMPARABLE  Incomparable
*               1 A_SUB_B Vertex proper subset of vertex
*               2 A_EQ_B Vertex equal vertex
*               3 A_SUPER_B Vertex superset of vertex
*/

int compare_vertex_attr(va,vb)
vertex_id va,vb;
{ int i,j;
  conmap_t * con1,*con2;
  int same = 0;

  /* check constraints */
  con1 = get_v_constraint_map(va);
  con2 = get_v_constraint_map(vb);

  for ( i = 1 ; i <= (int)con2[0] ; i++ )
  { for ( j = 1 ; j <= (int)con1[0] ; j++ )
      if ( (con1[j]&CONMASK) == (con2[i]&CONMASK) )
      { same++;
        break;
      }
  }

  if ( (same==(int)con1[0]) && (same==(int)con2[0]) )
    return A_EQ_B;
  if ( (same==(int)con1[0]) && (same < (int)con2[0]) )
    return A_SUB_B;
  if ( (same < (int)con1[0]) && (same==(int)con2[0]) )
    return A_SUPER_B;
  return INCOMPARABLE;
}

/********************************************************************
*
* Function: compare_vertex_edge_attr(va,eb)
* 
* Purpose: Compare constraints of vertex and edge.
*
* Return value: 0 INCOMPARABLE  Incomparable
*               1 A_SUB_B Vertex proper subset of edge
*               2 A_EQ_B Vertex equal edge
*               3 A_SUPER_B Vertex superset of edge
*/

int compare_vertex_edge_attr(va,eb)
vertex_id va;
edge_id eb;
{ int i,j;
  conmap_t * con1,*con2;
  int same = 0;

  /* check constraints */
  con1 = get_v_constraint_map(va);
  con2 = get_e_constraint_map(eb);

  for ( i = 1 ; i <= (int)con2[0] ; i++ )
  { for ( j = 1 ; j <= (int)con1[0] ; j++ )
      if ( (con1[j]&CONMASK) == (con2[i]&CONMASK) )
      { same++;
        break;
      }
  }

  if ( (same==(int)con1[0]) && (same==(int)con2[0]) )
    return A_EQ_B;
  if ( (same==(int)con1[0]) && (same < (int)con2[0]) )
    return A_SUB_B;
  if ( (same < (int)con1[0]) && (same==(int)con2[0]) )
    return A_SUPER_B;
  return INCOMPARABLE;

}

/********************************************************************
*
* Function: compare_edge_attr(ea,eb)
* 
* Purpose: Compare constraints of edge and edge.
*
* Return value: 0 INCOMPARABLE  Incomparable
*               1 A_SUB_B edge proper subset of edge
*               2 A_EQ_B edge equal edge
*               3 A_SUPER_B edge superset of edge
*/

int compare_edge_attr(ea,eb)
edge_id ea,eb;
{ int i,j;
  int fixa = get_eattr(ea) & FIXED;
  int fixb = get_eattr(eb) & FIXED;
  conmap_t * con1,*con2;
  int same = 0;
  
  /* check constraints */
  con1 = get_e_constraint_map(ea);
  con2 = get_e_constraint_map(eb);

  for ( i = 1 ; i <= (int)con2[0] ; i++ )
  { for ( j = 1 ; j <= (int)con1[0] ; j++ ) 
      if ( (con1[j]&CONMASK) == (con2[i]&CONMASK) )
      { same++;
        break;
      }
  }

  if ( (same==(int)con1[0]) && (same==(int)con2[0]) && !fixa && !fixb )
    return A_EQ_B;
  if ( (same==(int)con1[0]) && (same < (int)con2[0]) && !fixa )
    return A_SUB_B;
  if ( (same < (int)con1[0]) && (same==(int)con2[0]) && !fixb )
    return A_SUPER_B;
  return INCOMPARABLE;
}


/********************************************************************
*
* Function: compare_edge_facet_attr(ea,fb)
* 
* Purpose: Compare constraints of edge and facet.
*
* Return value: 0 INCOMPARABLE  Incomparable
*               1 A_SUB_B Edge proper subset of facet
*               2 A_EQ_B Edge equal facet
*               3 A_SUPER_B Edge superset of facet
*/

int compare_edge_facet_attr(ea,fb)
edge_id ea;
facet_id fb;
{ int i,j;
  conmap_t * con1,*con2;
  int fixa = get_eattr(ea) & FIXED;
  int fixb = get_fattr(fb) & FIXED;
  int same = 0;

  /* check constraints */
  con1 = get_e_constraint_map(ea);
  con2 = get_f_constraint_map(fb);

  for ( i = 1 ; i <= (int)con2[0] ; i++ )
  { for ( j = 1 ; j <= (int)con1[0] ; j++ )
      if ( (con1[j]&CONMASK) == (con2[i]&CONMASK) )
      { same++;
        break;
      }
  }

  if ( (same==(int)con1[0]) && (same==(int)con2[0])&& !fixa && !fixb )
    return A_EQ_B;
  if ( (same==(int)con1[0]) && (same < (int)con2[0]) && !fixa )
    return A_SUB_B;
  if ( (same < (int)con1[0]) && (same==(int)con2[0]) && !fixb )
    return A_SUPER_B;
  return INCOMPARABLE;

}

/**************************************************************
*
*  Function:  equal_constr()
*
*  Purpose:    See if two elements have the same set of constraints.
*
*/

int equal_constr(id1,id2)
element_id id1,id2;
{ int i,j;
  conmap_t * con1=NULL,*con2=NULL;

  switch ( id_type(id1) )
     {
        case VERTEX: 
                         con1        = get_v_constraint_map(id1);
                         break;

        case EDGE  : 
                         con1        = get_e_constraint_map(id1);
                         break;

        case FACET : 
                         con1        = get_f_constraint_map(id1);
                         break;
     }


  switch ( id_type(id2) )
     {
        case VERTEX: 
                         con2        = get_v_constraint_map(id2);
                         break;

        case EDGE  :
                         con2        = get_e_constraint_map(id2);
                         break;

        case FACET :
                         con2        = get_f_constraint_map(id2);
                         break;
     }
  if ( con2[0] != con1[0] ) return 0;
  for ( i = 1 ; i <= (int)con1[0] ; i++ )
  { for ( j = 1 ; j <= (int)con2[0] ; j++ )
        if ( (con1[i]&CONMASK) == (con2[j]&CONMASK) ) break;
     if ( j > (int)con2[0] ) return 0;
  }
  return 1;
}

/*************************************************************************
*
*  function: add_attribute()
*
*  purpose: add extra attribute to an element type
*
*  return:  index of attribute
*/

int add_attribute(e_type,name,attr_type,dim,dims,dumpflag,code)
int e_type; /* VERTEX ,... */
char *name;
int attr_type; /* REAL_TYPE or INTEGER_TYPE or ULONG_TYPE etc */
int dim; /* number of dimensions, 0 for scalar */
int *dims; /* sizes of dimensions, NULL for all sizes 0 */
            /* Note: scalar still needs size of 0 or 1 */ 
int dumpflag; /* whether appears in dump file */
struct expnode *code; /* nonnull for function attribute */
{ int newsize,newcount=0;
  struct extra *ex;
  int oldsize;
  int att_inx;

  att_inx = find_attribute(e_type,name);
  if ( att_inx >= 0 )
    return att_inx;

#ifdef MPI_EVOLVER
  if ( this_task == MASTER_TASK )
  { struct mpi_command message;
    int i;

    if ( code )
    { kb_error(4525,"MPI Evolver doesn't do code attributes yet.\n",
          RECOVERABLE);
    }
    message.cmd = mpi_ADD_ATTRIBUTE;
    strncpy(message.name,name,MPI_NAME_SIZE);
    message.i = attr_type;
    message.type = e_type;
    message.mode = dumpflag;
	message.size = dims ? 1 : 0; /* just using as indicator */
    message.count = dim;
	if ( dim )
      for (i = 0 ; i < dim ; i++ )
		  message.data[i] = dims ? dims[i] : 0;
	else 
		message.data[0] = dims ? dims[0] : 0;
    MPI_Bcast(&message,sizeof(struct mpi_command),MPI_BYTE,MASTER_TASK,
        MPI_COMM_WORLD);
  }
#endif

  if ( web.skel[e_type].extra_count >= web.skel[e_type].maxextra-1 )
  { web.skel[e_type].dy_extras = 
       dy_realloc(web.skel[e_type].dy_extras,
       (web.skel[e_type].maxextra+10),sizeof(struct extra));
    web.skel[e_type].maxextra += 10;
  }

  /* expand space */ 
  /* get old used space, minus any padding, and pad to proper size */
  if ( web.skel[e_type].extra_count > 0 )
  { ex = EXTRAS(e_type) + web.skel[e_type].extra_count-1;
    oldsize = ex->offset + ex->array_spec.datacount*datatype_size[ex->type];
  }
  else oldsize = web.sizes[e_type];
  if  (oldsize % datatype_size[attr_type])
    oldsize += datatype_size[attr_type] - (oldsize % datatype_size[attr_type]);

  ex = EXTRAS(e_type) + web.skel[e_type].extra_count;

  if ( dim == 0 ) newcount = dims ? dims[0] : 0;
  else if ( dim > MAXARRAYDIMS )
  { sprintf(errmsg,"Extra attribute \"%s\" has %d dimensions, exceeding limit of %d.\n", name,dim,MAXARRAYDIMS);
    kb_error(2510,errmsg,RECOVERABLE);
  }
  else if ( dims == NULL ) newcount = 0;
  else
  { int i;
    for ( i = 0, newcount = 1 ; i < dim ; i++ )
    { newcount *= dims[i];
      ex->array_spec.sizes[i] = dims[i];
    }
  }
  if ( (newcount > 0) || (oldsize > web.sizes[e_type]) )
  { newsize =  newcount*datatype_size[attr_type];
    expand(e_type,oldsize + newsize); 
  }
  strncpy(ex->name,name,ATTR_NAME_SIZE);
  ex->type = attr_type;
  ex->array_spec.datatype = attr_type;
  ex->array_spec.itemsize = datatype_size[attr_type];
 
  ex->offset = oldsize;
  ex->array_spec.datacount = newcount;
  ex->array_spec.dim = dim;
  if ( dim > 0 ) ex->flags |= DIMENSIONED_ATTR;
  if ( code ) ex->code = *code;
  if ( dumpflag ) ex->flags |= DUMP_ATTR;

  if ( stricmp(name,EXTRA_BDRY_NAME) == 0 )
  { extra_bdry_attr = web.skel[e_type].extra_count;
    if ( ex->type != INTEGER_TYPE )
      kb_error(2842,"Attribute extra_boundary must be of type integer.\n",
         RECOVERABLE);
    if ( e_type != VERTEX )
      kb_error(2843,"Attribute extra_boundary should be vertex attribute.\n",
         RECOVERABLE);
  }
  else if ( stricmp(name,EXTRA_BDRY_PARAM_NAME) == 0 )
  { extra_bdry_param_attr = web.skel[e_type].extra_count;
    if ( ex->type != REAL_TYPE )
      kb_error(3369,"Attribute extra_boundary_param must be of type real.\n",
         RECOVERABLE);
    if ( e_type != VERTEX )
      kb_error(2845,
        "Attribute extra_boundary_param should be vertex attribute.\n",
         RECOVERABLE);
  }
  web.skel[e_type].extra_count++;
  return web.skel[e_type].extra_count - 1; /* index */
}


/*************************************************************************
*
*  function: expand_attribute()
*
*  purpose: enlarge space for attribute of an element type
*              or shrink it.
*/

void expand_attribute(e_type,attr_num,newsizes)
int e_type; /* VERTEX ,... */
int attr_num; /* number of attribute */
int *newsizes; /* new numbers of components, even for scalar */
{ int newsize;  
  int diff;  /* difference between old and new total sizes */
  struct extra *ex;
  int chunksize,offset,available,needed;
  int pointercount=0;
  char *spot;
  element_id id;
  int k,d,n,dsize,dest,inx,blocksize;
  char *temp=NULL; /* for shuffling higher dim entries */

  ex = EXTRAS(e_type) + attr_num;

#ifdef MPI_EVOLVER
  if ( this_task == MASTER_TASK )
  { struct mpi_command message;
    int i;
    message.cmd = mpi_EXPAND_ATTRIBUTE;
    message.i = attr_num;
    message.type = e_type;
    message.count = ex->adim;
	if ( ex->adim )
      for (i = 0 ; i < ex->adim ; i++ )
        message.data[i] = newsizes[i];
	else message.data[0] = newsizes[0];
    MPI_Bcast(&message,sizeof(struct mpi_command),MPI_BYTE,MASTER_TASK,
        MPI_COMM_WORLD);
  }
#endif

  dsize = ex->array_spec.itemsize;

  if ( ex->array_spec.dim == 0 ) newsize = newsizes[0];
  else
  { pointercount = 1;
    for ( newsize = 1, k = 0 ; k < ex->array_spec.dim ; k++ )
    { newsize *= newsizes[k];
      if ( k < ex->array_spec.dim - 1 )
        pointercount *= newsizes[k];
    }
  }
  

  if ( (ex->array_spec.dim <= 1) && (newsize == ex->array_spec.datacount) ) 
     return;

  /* expand or contract space */
  /* see how much extra space is needed */
  if ( attr_num < web.skel[e_type].extra_count-1 )
         available = ex[1].offset - ex[0].offset;
  else available = web.sizes[e_type] - ex->offset;
  needed = newsize*datatype_size[ex->type]+pointercount*sizeof(REAL*);
  if ( ex->array_spec.dim >= 2 ) 
     temp = (char*)temp_calloc(needed,1);
  if ( needed > available )
  { /* expand */
    /* check alignment of following fields */
    diff = needed - available;
    for ( k = attr_num+1 ; k < web.skel[e_type].extra_count ; k++ )
      while ( diff % datatype_size[EXTRAS(e_type)[k].type] )
        diff++;
    expand(e_type,web.sizes[e_type] + diff); 
    /* move stuff above */
    if ( attr_num < web.skel[e_type].extra_count-1 )
      offset = EXTRAS(e_type)[attr_num+1].offset;
    else offset = web.sizes[e_type] - diff;
    chunksize = web.sizes[e_type] - offset - diff;
    if ( chunksize || ( ex->array_spec.dim >= 2) )
    { element_id sentinel;
      id = NULLID; 
      if ( web.skel[e_type].count > 0 ) 
        while ( generate_all(e_type,&id,&sentinel) )
        { 
          spot = (char*)elptr(id) + offset;
          kb_memmove(spot + diff,spot,chunksize);
          if ( ex->array_spec.dim >= 2 )
          { /* entry shuffle via temp */
            char *old = (char*)elptr(id) + EXTRAS(e_type)[attr_num].offset;
            for ( n = 0 ; n < ex->array_spec.datacount ; n++ )
            { /* figure out indices and new spot */
              int oldinx = n;
              for ( d = ex->array_spec.dim-1, dest = 0, blocksize = 1 ; d >= 0 ; d-- )
              { inx = oldinx % ex->array_spec.sizes[d];
                if ( inx >= newsizes[d] ) goto skipentry; 
                dest += blocksize*inx;
                blocksize *= newsizes[d];
                oldinx = oldinx/ex->array_spec.sizes[d];
              }
              kb_memmove(temp+dest*dsize,old+n*dsize,dsize);
skipentry:    ;
            }
            kb_memmove(old,temp,needed);
          }
          else
            memset(spot,0,diff);
        }
    }
    for ( k = attr_num+1 ; k < web.skel[e_type].extra_count ; k++ )
         EXTRAS(e_type)[k].offset += diff;
  }
  else if ( needed < available )
  { /* maybe shrink */
     /* check alignment of following fields */
     diff = available - needed;
     for ( k = attr_num+1 ; k < web.skel[e_type].extra_count ; k++ )
        while ( diff % datatype_size[EXTRAS(e_type)[k].type] )
          diff--;
     /* move stuff above */
     if ( attr_num < web.skel[e_type].extra_count-1 )
       offset = EXTRAS(e_type)[attr_num+1].offset;
     else offset = web.sizes[e_type];
     chunksize = web.sizes[e_type] - offset;
     if ( chunksize || (ex->array_spec.dim >= 2) )
     { element_id sentinel;
       id = NULLID;
       if ( web.skel[e_type].count > 0 ) 
        while ( generate_all(e_type,&id,&sentinel) )
        {
          if ( ex->array_spec.dim >= 2 )
          { /* entry shuffle via temp */
            char *old = (char*)elptr(id)+EXTRAS(e_type)[attr_num].offset;
            for ( n = 0 ; n < ex->array_spec.datacount ; n++ )
            { /* figure out indices and new spot */
              int oldinx = n;
              for ( d = ex->array_spec.dim-1, dest = 0, blocksize = 1 ; d >= 0 ; d-- )
              { inx = oldinx % ex->array_spec.sizes[d];
                if ( inx >= newsizes[d] ) goto skipentry2; 
                dest += blocksize*inx;
                blocksize *= newsizes[d];
                oldinx = oldinx/ex->array_spec.sizes[d];
              }
              kb_memmove(temp+dest*dsize,old+n*dsize,dsize);
skipentry2:    ;
            }
            kb_memmove(old,temp,needed);
          }
          spot = (char*)elptr(id) + offset;
          kb_memmove(spot - diff,spot,chunksize);
        }
     }
     for ( k = attr_num+1 ; k < web.skel[e_type].extra_count ; k++ )
         EXTRAS(e_type)[k].offset -= diff;
     expand(e_type,web.sizes[e_type] - diff); 
  }
  if ( ex->array_spec.dim >= 2 ) temp_free(temp);
  ex->array_spec.datacount = newsize;
  for ( n = 0 ; n < ex->array_spec.dim ; n++ )
    ex->array_spec.sizes[n] = newsizes[n];
  parallel_update_flag[e_type] = 1;

#ifdef MPI_EVOLVER
  mpi_export_voffset = EXTRAS(VERTEX)[web.mpi_export_attr[VERTEX]].offset;
  mpi_export_eoffset = EXTRAS(EDGE)[web.mpi_export_attr[EDGE]].offset;
  mpi_export_foffset = EXTRAS(FACET)[web.mpi_export_attr[FACET]].offset;
  mpi_export_boffset = EXTRAS(BODY)[web.mpi_export_attr[BODY]].offset;
  mpi_export_feoffset =
          EXTRAS(FACETEDGE)[web.mpi_export_attr[FACETEDGE]].offset; 
#endif
}

/*************************************************************************
*
*  function: find_attribute()
*
*  purpose: find extra attribute by name, if it exists.
*  return: index number if found, -1 if not.
*/

int find_attribute(etype,name)
int etype;
char *name;
{ struct extra *ex;
  int n;
  ex = EXTRAS(etype);
  for ( n = 0 ; n < web.skel[etype].extra_count ; n++,ex++ )
    if ( stricmp(ex->name,name) == 0 ) break;
  if ( n == web.skel[etype].extra_count )
     return -1;
  return n;
}
/**************************************************************************
*
* function: find_extra()
*
* purpose: return index of named attribute, searching all element types.
*             return -1 if not found.
*/
int find_extra(name,etype)
char *name;
int *etype; /* for returning element type */
{ int el_type,qnum,n;
  struct extra *ex;

  for ( el_type = VERTEX, qnum = -1 ; el_type <= FACETEDGE ; el_type++ )
  { ex = EXTRAS(el_type);
    for ( n = 0 ; n < web.skel[el_type].extra_count ; n++,ex++ )
      if ( stricmp(ex->name,name) == 0 )
      {*etype = el_type;qnum = n;break;}
  }
  return qnum;
}

/***************************************************************************
  Constraint handling routines
****************************************************************************/

/* Methodology:
    Constraint map is array of conmap_t.
    First entry is number of constraints.
    Then follow constraint numbers, with high bit CON_HIT_BIT set
      if constraint is hit.
    Allocated as an extra attribute if needed.
*/

conmap_t nullcon[2]; /* default empty list */

void set_v_global(v_id)
vertex_id v_id;
{ int k;
  for ( k = 0 ; k < web.con_global_count ; k++ )
     set_v_constraint_map(v_id,web.con_global_map[k]);
}

void set_v_conmap(v_id,map) 
vertex_id v_id;
conmap_t *map;
{ int k, m=(int)map[0];
  for ( k = 1 ; k <= m ; k++ )
    set_v_constraint_map(v_id,map[k]);
  map = get_v_constraint_map(v_id);
  if ( map[0] == 0 ) unset_attr(v_id,CONSTRAINT);
}

void set_e_conmap(e_id,map)  
edge_id e_id;
conmap_t *map;
{ int k, m=(int)map[0];
  for ( k = 1 ; k <= m ; k++ )
    set_e_constraint_map(e_id,map[k]);
  map = get_e_constraint_map(e_id);
  if ( map[0] == 0 ) unset_attr(e_id,CONSTRAINT);
}

void set_f_conmap(f_id,map)  
facet_id f_id;
conmap_t *map;
{ int k, m=(int)map[0];
  for ( k = 1 ; k <= m ; k++ )
    set_f_constraint_map(f_id,map[k]);
  map = get_f_constraint_map(f_id);
  if ( map[0] == 0 ) unset_attr(f_id,CONSTRAINT);
}

void set_v_constraint_map(v_id,n)  
vertex_id v_id;
int n;  /* constraint number */
{ conmap_t *map;
  int maxcon = EXTRAS(VERTEX)[V_CONSTR_LIST_ATTR].array_spec.datacount;
  struct constraint *constr;
  int k,j;
  int four = 4;

  n &= CONMASK;
  if ( maxcon == 0 )
     expand_attribute(VERTEX,V_CONSTR_LIST_ATTR,&four);
  map = get_v_constraint_map(v_id);
  for ( k = 1; k <= (int)*map ; k++ )
     if ( (map[k] & CONMASK) == ((conmap_t)n & CONMASK) ) return;
  if ( k >= maxcon )
  { int newmax = maxcon+4;
    expand_attribute(VERTEX,V_CONSTR_LIST_ATTR,&newmax);
    map = get_v_constraint_map(v_id);
  }  
  constr = get_constraint(n);
  map[k] = (conmap_t)n; 
  if ( !(constr->attr & (NONPOSITIVE|NONNEGATIVE) )) 
    map[k] |= CON_HIT_BIT;
  map[0]++; /* counter */

  set_attr(v_id,CONSTRAINT);
  if ( (constr->attr & CON_ENERGY) && (web.representation == STRING) )
  { set_attr(v_id, BDRY_ENERGY);
     if ( everything_quantities_flag )
        apply_method_num(v_id,constr->energy_method);
  }
  if ( (constr->attr & CON_CONTENT) && (web.representation == STRING) )
  { 
    set_attr(v_id, BDRY_CONTENT);

    if ( everything_quantities_flag )
    { edge_id e_id,first_e;
      int max_rank,min_rank;
        
      min_rank = MAXINT; max_rank = 0;
      for ( j = 1 ; j <= (int)map[0] ; j++ )
      { struct constraint *c;
        if ( !(map[j] & CON_HIT_BIT) ) continue;
        c = get_constraint(map[j]);
        if ( c->content_rank < min_rank ) min_rank = c->content_rank;
        if ( c->content_rank > max_rank ) max_rank = c->content_rank;
      }

      first_e = e_id = get_vertex_edge(v_id);
      if ( valid_id(e_id) && !(get_eattr(e_id) & NONCONTENT) )
     do
     { char name[100];
       body_id b_id;
       facetedge_id first_fe,fe;
       first_fe = fe = get_edge_fe(e_id);
       if ( valid_id(fe) )  do
       { facet_id f_id = get_fe_facet(fe);
         fe = get_next_facet(fe);
         if ( !valid_id(f_id) ) continue;
         b_id = get_facet_body(f_id);
         
         if ( valid_id(b_id)  && 
           ( (!inverted(f_id) && constr->content_rank >= max_rank) 
               || (inverted(f_id) && constr->content_rank <= min_rank)))
         { if ( constr->attr & NAMED_THING )
             sprintf(name,"body_%d_%s_meth",ordinal(b_id)+1,constr->name);
           else
             sprintf(name,"body_%d_con_%d_meth",ordinal(b_id)+1,n);
           attach_method(get_body_volquant(b_id),name);
           apply_method(inverse_id(v_id),name);
         }

         b_id = get_facet_body(inverse_id(f_id));         
         if ( valid_id(b_id) && 
              ( (!inverted(f_id) && constr->content_rank >= max_rank) 
               || (inverted(f_id) && constr->content_rank <= min_rank)) )
         { if ( constr->attr & NAMED_THING )
             sprintf(name,"body_%d_%s_meth",ordinal(b_id)+1,constr->name);
           else
             sprintf(name,"body_%d_con_%d_meth",ordinal(b_id)+1,n);
           attach_method(get_body_volquant(b_id),name);
           apply_method(v_id,name);
         }
       } while ( !equal_id(fe,first_fe) );
       e_id = get_next_tail_edge(e_id);
     } while ( !equal_id(first_e,e_id));
   }
  }
}

void unset_v_constraint_map(v_id,n)    
vertex_id v_id;
int n;
{ conmap_t *map;
  int maxcon = EXTRAS(VERTEX)[V_CONSTR_LIST_ATTR].array_spec.datacount;
  int k,j;

  n &= CONMASK;
  if ( maxcon == 0 ) return;
  map = get_v_constraint_map(v_id);
  for ( k = 1; k <= (int)*map ; k++ )
     if ( (map[k] & CONMASK) == (conmap_t)n ) break;
  if ( k > (int)*map ) return;
  map[0]--;
  for ( j = k ; j <= (int)*map ; j++ ) map[j] = map[j+1];
  map[j] = 0;
  if ( map[0] == 0 ) unset_attr(v_id,CONSTRAINT);
  if ( everything_quantities_flag )
  { struct constraint *con = get_constraint(n);
    int i;
    if ( con->attr & CON_ENERGY )
            unapply_method(v_id,con->energy_method);
    if ( con->attr & CON_CONTENT )
    { int meth_offset = get_meth_offset(VERTEX);
      int *instlist = (int*)((char*)elptr(v_id) + meth_offset);
      int mcount = elptr(v_id)->method_count;
      for ( i = 0 ; i < mcount ; i++ )
        if ( METH_INSTANCE(abs(instlist[i]))->connum == n )
           unapply_method(v_id,instlist[i]);
    }
  }
}

int v_on_constraint(v_id,n)  
vertex_id v_id;
int n;
{ conmap_t *map;
  int maxcon = EXTRAS(VERTEX)[V_CONSTR_LIST_ATTR].array_spec.datacount;
  int k;

  n &= CONMASK;
  if ( maxcon == 0 ) return 0;
  map = get_v_constraint_map(v_id);
  for ( k = 1; k <= (int)*map ; k++ )
  { if ( (map[k] & CONMASK) == (conmap_t)n ) 
        return 1;
  }
  return 0;
}

int v_hit_constraint_count(v_id)  
vertex_id v_id;
{ conmap_t *map;
  int maxcon = EXTRAS(VERTEX)[V_CONSTR_LIST_ATTR].array_spec.datacount;
  int count = 0;
  int k;

  if ( maxcon == 0 ) return 0;
  map = get_v_constraint_map(v_id);
  for ( k = 1; k <= (int)*map ; k++ )
  { if ( (map[k] & CON_HIT_BIT) ) 
      count++;
  }
  return count;
}

void get_v_common_conmap(v1,v2,conmap)
vertex_id v1,v2;
conmap_t *conmap;
{ conmap_t *map1 = get_v_constraint_map(v1);
  unsigned int k;

  conmap[0] = 0;
  for ( k = 1; k <= map1[0] ; k++ )
     if ( v_on_constraint(v2,map1[k]) )
        conmap[++conmap[0]] = map1[k] & (conmap_t)CONMASK;
}

int get_v_constraint_status(v_id,n)  
vertex_id v_id;
int n;
{ conmap_t *map;
  int maxcon = EXTRAS(VERTEX)[V_CONSTR_LIST_ATTR].array_spec.datacount;
  int k;

  n &= CONMASK;
  if ( maxcon == 0 ) return 0;
  map = get_v_constraint_map(v_id);
  for ( k = 1; k <= (int)*map ; k++ )
  { if ( (map[k] & CONMASK) == (conmap_t)n ) 
        return (map[k] & CON_HIT_BIT) ? 1 : 0;
  }
  return 0;
}

void clear_v_conmap(v_id)
vertex_id v_id;
{ conmap_t *map = get_v_constraint_map(v_id);
  unsigned int k;

  for ( k = 1 ; k <= map[0] ; k++ )
    map[k] = 0;
  map[0] = 0;
}

void set_v_constraint_status(v_id,n)  
vertex_id v_id;
int n;
{ conmap_t *map;
  int maxcon = EXTRAS(VERTEX)[V_CONSTR_LIST_ATTR].array_spec.datacount;

  int k;
  n &= CONMASK;
  if ( maxcon == 0 ) return;
  map = get_v_constraint_map(v_id);
  for ( k = 1; k <= (int)*map ; k++ )
  { if ( (map[k] & CONMASK) == (conmap_t)n ) 
    { map[k] |= CON_HIT_BIT;
      set_attr(v_id,HIT_WALL);
      return;
    }
  }
}

void unset_v_constraint_status(v_id,n) 
vertex_id v_id;
int n;
{ conmap_t *map;
  int maxcon = EXTRAS(VERTEX)[V_CONSTR_LIST_ATTR].array_spec.datacount;

  int k;
  n &= CONMASK;
  if ( maxcon == 0 ) return;
  map = get_v_constraint_map(v_id);
  for ( k = 1; k <= (int)*map ; k++ )
  { if ( (map[k] & CONMASK) == (conmap_t)n ) 
     { map[k] &= ~CON_HIT_BIT;
       return;
     }
  }
}

void clear_v_constraint_status(v_id) 
vertex_id v_id;
{ conmap_t *map;
  int maxcon = EXTRAS(VERTEX)[V_CONSTR_LIST_ATTR].array_spec.datacount;

  int k;
  if ( maxcon == 0 ) return;
  map = get_v_constraint_map(v_id);
  for ( k = 1; k <= (int)*map ; k++ )
  { map[k] &= ~CON_HIT_BIT;
  }

}

void set_e_constraint_map(e_id,n)  
edge_id e_id;
int n;
{ conmap_t *map;
  int maxcon = EXTRAS(EDGE)[E_CONSTR_LIST_ATTR].array_spec.datacount;
  int k;
  struct constraint *constr;
  int four = 4;

  n &= CONMASK;
  if ( maxcon == 0 )
     expand_attribute(EDGE,E_CONSTR_LIST_ATTR,&four);
  map = get_e_constraint_map(e_id);
  for ( k = 1; k <= (int)*map ; k++ )
     if ( (map[k] & CONMASK) == (conmap_t)n ) return;
  if ( k >= maxcon )
  { int newmax = maxcon+4;
    expand_attribute(EDGE,E_CONSTR_LIST_ATTR,&newmax);
    map = get_e_constraint_map(e_id);
  } 
  map[k] = (conmap_t)n; 
  map[0]++; /* counter */

  set_attr(e_id,CONSTRAINT);
  constr = get_constraint(n);
  if ( constr->attr & CON_ENERGY )
  { set_attr(e_id, BDRY_ENERGY);
    if ( everything_quantities_flag )
       apply_method_num(positive_id(e_id),constr->energy_method);
/* positive_id() to agree with old way */
  }
  if ( constr->attr & CON_CONTENT )
  { set_attr(e_id, BDRY_CONTENT);  /* BIG PROBLEM HERE GETTING RIGHT BODY!!! */
    if ( everything_quantities_flag )
    { facetedge_id fe,first_fe = get_edge_fe(e_id);
      int min_rank, max_rank,j;

      min_rank = MAXINT; max_rank = 0;
      for ( j = 1 ; j <= (int)map[0] ; j++ )
      { 
        struct constraint *c = get_constraint(map[j]);
        if ( c->content_rank < min_rank ) min_rank = c->content_rank;
        if ( c->content_rank > max_rank ) max_rank = c->content_rank;
      }
      fe = first_fe;
      if ( valid_id(first_fe) )
      do
      { char name[100];
        body_id b_id;
        facet_id f_id;
        f_id = get_fe_facet(fe);
        fe = get_next_facet(fe);
        if ( !valid_id(f_id) ) continue;
        if ( get_fattr(f_id) & NONCONTENT ) continue;
        b_id = get_facet_body(f_id);
        if ( valid_id(b_id) && (constr->content_rank >= max_rank))
        { if ( constr->attr & NAMED_THING )
            sprintf(name,"body_%d_%s_meth",ordinal(b_id)+1,constr->name);
          else
            sprintf(name,"body_%d_con_%d_meth",ordinal(b_id)+1,n);
          attach_method(get_body_volquant(b_id),name);
          apply_method(e_id,name);
        }
        b_id = get_facet_body(inverse_id(f_id));
        if ( valid_id(b_id) && (constr->content_rank <= min_rank))
        { if ( constr->attr & NAMED_THING )
            sprintf(name,"body_%d_%s_meth",ordinal(b_id)+1,constr->name);
          else
           sprintf(name,"body_%d_con_%d_meth",ordinal(b_id)+1,n);
          attach_method(get_body_volquant(b_id),name);
          apply_method(inverse_id(e_id),name);
        }
      } while ( !equal_id(fe,first_fe) );

    }
  }
}

int e_on_constraint(e_id,n)  
edge_id e_id;
int n;
{ conmap_t *map;
  int maxcon = EXTRAS(EDGE)[E_CONSTR_LIST_ATTR].array_spec.datacount;
  int k;

  n &= CONMASK;
  if ( maxcon == 0 ) return 0;
  map = get_e_constraint_map(e_id);
  for ( k = 1; k <= (int)*map ; k++ )
  { if ( (map[k] & CONMASK) == (conmap_t)n ) 
        return 1;
  }
  return 0;
}

void unset_e_constraint_map(e_id,n) 
edge_id e_id;
int n;
{ conmap_t *map;
  int maxcon = EXTRAS(EDGE)[E_CONSTR_LIST_ATTR].array_spec.datacount;
  int j,k;

  n &= CONMASK;
  if ( maxcon == 0 ) return;
  map = get_e_constraint_map(e_id);
  for ( k = 1; k <= (int)*map ; k++ )
    if ( (map[k] & CONMASK) == (conmap_t)n ) break;
  if ( k > (int)*map ) return;

  map[0]--;
  for ( j = k ; j <= (int)*map ; j++ ) map[j] = map[j+1];
  map[j] = 0;
  if ( map[0] == 0 ) unset_attr(e_id,CONSTRAINT);

  if ( everything_quantities_flag )
  { struct constraint *con = get_constraint(n);
    if ( con->attr & CON_ENERGY )
            unapply_method(e_id,con->energy_method);
    if ( con->attr & CON_CONTENT )
    { int meth_offset = get_meth_offset(EDGE);
      int i;
      int *instlist = (int*)((char*)elptr(e_id) + meth_offset);
      int mcount = elptr(e_id)->method_count;
      for ( i = 0 ; i < mcount ; i++ )
        if ( METH_INSTANCE(abs(instlist[i]))->flags & BODY_INSTANCE )
           unapply_method(e_id,instlist[i]);
    }
  }

}

void set_f_constraint_map(f_id,n)  
facet_id f_id;
int n;
{ conmap_t *map;
  int maxcon = EXTRAS(FACET)[F_CONSTR_LIST_ATTR].array_spec.datacount;
  int k;
  int four = 4;

  n &= CONMASK;  /* get rid of hit bit */
  if ( maxcon == 0 )
     expand_attribute(FACET,F_CONSTR_LIST_ATTR,&four);
  map = get_f_constraint_map(f_id);
  for ( k = 1; k <= (int)*map ; k++ )
     if ( (map[k] & CONMASK) == (conmap_t)n ) return;
  if ( k >= maxcon )
  { int newmax = maxcon+4;
    expand_attribute(FACET,F_CONSTR_LIST_ATTR,&newmax);
    map = get_f_constraint_map(f_id);
  } 
  map[k] = (conmap_t)n; 
  map[0]++; /* counter */
}

int f_on_constraint(f_id,n)  
facet_id f_id;
int n;
{ conmap_t *map;
  int maxcon = EXTRAS(FACET)[F_CONSTR_LIST_ATTR].array_spec.datacount;
  int k;

  n &= CONMASK;
  if ( maxcon == 0 ) return 0;
  map = get_f_constraint_map(f_id);
  for ( k = 1; k <= (int)*map ; k++ )
  { if ( (map[k] & CONMASK) == (conmap_t)n ) 
        return 1;
  }
  return 0;
}

void unset_f_constraint_map(f_id,n)    
facet_id f_id;
int n;
{ conmap_t *map;
  int maxcon = EXTRAS(FACET)[F_CONSTR_LIST_ATTR].array_spec.datacount;
  int k,j;

  n &= CONMASK;
  if ( maxcon == 0 ) return;
  map = get_f_constraint_map(f_id);
  for ( k = 1; k <= (int)*map ; k++ )
     if ( (map[k] & CONMASK) == (conmap_t)n ) break;
  if ( k > (int)*map ) return;
  map[0]--;
  for ( j = k ; j <= (int)*map ; j++ ) map[j] = map[j+1];
  map[j] = 0;
  if ( map[0] == 0 ) unset_attr(f_id,CONSTRAINT);
  if ( everything_quantities_flag )
  { struct constraint *con = get_constraint(n);
    if ( con->attr & CON_ENERGY )
       unapply_method(f_id,con->energy_method);
  }
}

int get_tag(f_id)
facet_id f_id;
{ if ( F_TAG_ATTR == 0 ) return 0;
  if ( EXTRAS(FACET)[F_TAG_ATTR].array_spec.datacount > 0 )
     return (*FINT(f_id,F_TAG_ATTR));
  else return 0;
}

void set_tag(f_id,t)
facet_id f_id;
int t;
{ int one=1;
  if ( EXTRAS(FACET)[F_TAG_ATTR].array_spec.datacount == 0 )
     expand_attribute(FACET,F_TAG_ATTR,&one);
  *FINT(f_id,F_TAG_ATTR) = t;
}
