/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*************************************************************
*
*      file:      model.c
*
*      Purpose:  Switch between LINEAR and QUADRATIC models,
*                and other such transformations.
*
*/

#include "include.h"

/*************************************************************************
*
* function: adjust_integration_orders ()
*
* purpose: set minimal orders of numerical integration to fit element order.
*
*/

void adjust_integration_orders ARGS((int));
void adjust_integration_orders(l_order)
int l_order;  /* new element order */
{ int ord1,ord2;
  
  /* 2D orders actually implemented are 1,2,5,6,8,11,13,14,... */

  if ( l_order == 1 ) { ord1 = 3; ord2 = 3; }
  else if ( l_order == 2 ) { ord1 = 5; ord2 = 3; }
  else { ord1 = 2*l_order; ord2 = 2*l_order; }

  if ( (web.gauss1D_order < ord1) || (set_by_user_gauss_1D < ord1) ) 
        web.gauss1D_order = ord1;
  if ( (web.gauss2D_order < ord2) || (set_by_user_gauss_2D < ord2) ) 
        web.gauss2D_order = ord2;
}
/*************************************************************************
*
* Function: lagrange_to_linear()
*
* purpose: Convert from Lagrange model to Linear model.
*/
void lagrange_to_linear() 
{ 
#ifdef MPI_EVOLVER
  if ( this_task == MASTER_TASK )
    mpi_model(1,LINEAR);
#endif

  lagrange_to_lagrange(1);
  web.headvnum = 1;
  web.modeltype = LINEAR;
  gauss_setup();    /* set up gaussian integration arrays */
  recalc();
}
/*************************************************************************
*
* Function: lagrange_to_quad()
*
* purpose: Convert from Lagrange model to quadratic model.
*/
void lagrange_to_quad() 
{ edge_id e_id;
  vertex_id *v,vv;


#ifdef MPI_EVOLVER
  if ( this_task == MASTER_TASK )
    mpi_model(2,QUADRATIC);
#endif

  lagrange_to_lagrange(2);

  ENTER_GRAPH_MUTEX;

  MFOR_ALL_EDGES(e_id)
  { v = get_edge_vertices(e_id);
    vv = v[1]; v[1] = v[2]; v[2] = vv;
    unset_attr(vv,Q_MIDEDGE);
    set_attr(vv,Q_MIDPOINT);
  }
  web.headvnum = 1;
  web.modeltype = QUADRATIC;
  gauss_setup();    /* set up gaussian integration arrays */
  
  LEAVE_GRAPH_MUTEX;

  recalc();
}
/*************************************************************************
*
* Function: linear_to_lagrange()
*
* purpose: Convert from linear model to lagrange model.
*/
void linear_to_lagrange(l_order) 
int l_order;
{ int dim;
  facet_id f_id;
  facetedge_id fe;
  vertex_id *v;

  if ( web.skel[VERTEX].count > 0 )
    if ( !everything_quantities_flag )
    {  if ( auto_convert_flag ) convert_to_quantities(); else
        kb_error(1797,"Must do convert_to_quantities for Lagrange model.\n",
          RECOVERABLE);
    }

#ifdef MPI_EVOLVER
  if ( this_task == MASTER_TASK )
    mpi_model(l_order,LAGRANGE);
#endif

  if ( web.representation == SOAPFILM )
     MFOR_ALL_FACETS(f_id)
     { fe = get_facet_fe(f_id);
       v = get_facet_vertices(f_id);
       v[0] = get_fe_tailv(fe);
       fe = get_next_edge(fe);
       v[1] = get_fe_tailv(fe);
       fe = get_next_edge(fe);
       v[2] = get_fe_tailv(fe);
     }
  web.modeltype = LAGRANGE;
  web.lagrange_order = 1;
  dim = (web.representation==STRING)? 1 : web.dimension-1;
  adjust_integration_orders(l_order);
  gauss_lagrange_setup(web.dimension,web.lagrange_order,web.gauss2D_order);
  gauss_lagrange_setup(dim,web.lagrange_order,web.gauss1D_order);
  lagrange_to_lagrange(l_order);
  if ( everything_quantities_flag ) recalc();
}
/*************************************************************************
*
* Function: quad_to_lagrange()
*
* purpose: Convert from quadratic model to Lagrange model.
*/
void quad_to_lagrange(l_order) 
int l_order;
{ int dim;
  facet_id f_id;
  facetedge_id fe;
  edge_id e_id;
  vertex_id *v,vv;

  if ( !everything_quantities_flag )
  { if ( auto_convert_flag ) convert_to_quantities(); else
     kb_error(1798,"Must do convert_to_quantities for Lagrange model.\n",
          RECOVERABLE);
  }

#ifdef MPI_EVOLVER
  if ( this_task == MASTER_TASK )
    mpi_model(l_order,LAGRANGE);
#endif

  ENTER_GRAPH_MUTEX;

  MFOR_ALL_FACETS(f_id)
     { fe = get_facet_fe(f_id);
       v = get_facet_vertices(f_id);
       v[0] = get_fe_tailv(fe);
       v[1] = get_fe_midv(fe);
       fe = get_next_edge(fe);
       v[2] = get_fe_tailv(fe);
       v[4] = get_fe_midv(fe);
       fe = get_next_edge(fe);
       v[5] = get_fe_tailv(fe);
       v[3] = get_fe_midv(fe);
     }
  MFOR_ALL_EDGES(e_id)
  { v = get_edge_vertices(e_id);
     vv = v[1]; v[1] = v[2]; v[2] = vv; 
     unset_attr(v[1],Q_MIDPOINT);
     set_attr(v[1],Q_MIDEDGE);
  }
  web.headvnum = 2;
  web.modeltype = LAGRANGE;
  web.lagrange_order = 2;
  dim = (web.representation==STRING)? 1 : web.dimension-1;
  adjust_integration_orders(l_order);
  gauss_lagrange_setup(web.dimension,web.lagrange_order,web.gauss2D_order);
  gauss_lagrange_setup(dim,web.lagrange_order,web.gauss1D_order);

  LEAVE_GRAPH_MUTEX;

  lagrange_to_lagrange(l_order);
  if ( everything_quantities_flag ) recalc();
}
/*************************************************************************
*
* Function: lagrange_to_lagrange()
*
* purpose: Convert order of Lagrange model.
*/
void lagrange_to_lagrange(l_order) 
int l_order;
{ int new_ectrl,new_fctrl; /* new number of control points */ 
  int old_ectrl = web.skel[EDGE].ctrlpts;
  int old_fctrl = web.skel[FACET].ctrlpts;
  int i,k,m,spot; 
  int old,new;
  int inx[MAXCOORD+1];
  int oldinx[MAXCOORD+1];
  REAL **trans; /* transformation matrix old to new points */
  REAL **newx;  /* new coords */
  REAL **oldx;  /* old coords */
  int zeros;
  facet_id f_id;
  edge_id e_id;
  int dim;


  if ( web.lagrange_order == l_order ) return; /* already OK */
  if ( l_order < 1 ) 
     kb_error(1799,"Lagrange order must be at least 1.\n",RECOVERABLE);
  if ( l_order > MAXLAGRANGE ) 
  { sprintf(errmsg,"Lagrange order must be at most %d.\n",MAXLAGRANGE);
    kb_error(3505,errmsg, RECOVERABLE);
  }

#ifdef MPI_EVOLVER
  if ( this_task == MASTER_TASK )
    mpi_model(l_order,LAGRANGE);
#endif

  ENTER_GRAPH_MUTEX;

  bezier_refine_1d_init(l_order);
  bezier_refine_2d_init(l_order);

  if ( web.representation == SIMPLEX )
  { simplex_lagrange_to_lagrange(l_order); return; }

  if ( web.representation == STRING )
  { new_ectrl = binom_coeff(l_order+web.dimension,web.dimension);
    new_fctrl = binom_coeff(l_order+web.dimension+1,web.dimension+1);
  }
  else 
  { new_ectrl = binom_coeff(l_order+web.dimension-1,web.dimension-1);
    new_fctrl = binom_coeff(l_order+web.dimension,web.dimension);
  }
  if ( l_order > web.lagrange_order )
  { expand_attribute(EDGE,E_VERTICES_ATTR,&new_ectrl);
    expand_attribute(FACET,F_VERTICES_ATTR,&new_fctrl);
  } /* have to wait for later for shrink case */

  /* recalc all facet interior points */
  trans = dmatrix(0,new_fctrl,0,old_fctrl);
  newx = dmatrix(0,new_fctrl,0,SDIM);
  oldx = dmatrix(0,old_fctrl,0,SDIM);

  /* calculate transition matrix */
  dim = web.dimension;
  adjust_integration_orders(l_order);
  gauss_lagrange_setup(dim,l_order,web.gauss2D_order);
  if ( bezier_flag )
    bezier_trans_2d(web.lagrange_order,l_order,trans);
  else
  {
    for ( m = 0 ; m <= dim ; m++ ) inx[m] = 0;
    inx[0] = l_order;
    new = 0;
    do
    { old = 0;
      for ( m = 0 ; m <= dim ; m++ ) oldinx[m] = 0;
      oldinx[0] = web.lagrange_order;
      do
       { REAL prod;
         for ( k = 0, prod = 1.0 ; k <= dim ; k++ )
          for ( m = 0 ; m < oldinx[k] ; m++ )
           prod *= ((REAL)(inx[k])/l_order*web.lagrange_order-m)/(oldinx[k]-m);
         trans[new][old] = prod;
         old++;
       } while ( increment_lagrange_index(dim,oldinx) );
      new++;
    } while ( increment_lagrange_index(dim,inx) );
  }

  /* calc indices of extreme vertices; handy elsewhere */
  for ( m = 0 ; m <= dim ; m++ ) inx[m] = 0;
  for ( k = 0 ; k <= dim ; k++ )
  { inx[k] = l_order;
    new = lagrange_index(dim,l_order,inx);
    inx[k] = 0;
    web.skel[FACET].extreme[k] = new;
  }


  if ( web.representation != STRING )
  MFOR_ALL_FACETS(f_id)
   { vertex_id *v;
     dim = web.dimension;
     get_facet_verts(f_id,oldx,NULL);
     mat_mult(trans,oldx,newx,new_fctrl,web.skel[FACET].ctrlpts,SDIM);
     v = get_facet_vertices(f_id);

     /* free old interior */
     for ( m = 0 ; m <= dim ; m++ ) inx[m] = 0;
     inx[0] = web.lagrange_order;
     for ( k = 0 ; k < old_fctrl ; k++ )
     { for ( m = 0 ; m <= dim ; m++ )
          if ( inx[m] == 0 ) break;
        if ( m > dim ) free_element(v[k]);  /* free only interior */
        increment_lagrange_index(dim,inx);
     }
     /* move extreme vertices */
     for ( m = 0 ; m <= dim ; m++ ) inx[m] = 0;
     if ( l_order < web.lagrange_order )
        for ( k = 0 ; k <= dim ; k++ )
        { inx[k] = web.lagrange_order; 
          old = lagrange_index(dim,web.lagrange_order,inx); 
          inx[k] = l_order;
          new = lagrange_index(dim,l_order,inx);
          inx[k] = 0;
          v[new] = v[old];
        }
     else
        for ( k = dim ; k >= 0 ; k-- )
        { inx[k] = web.lagrange_order; 
          old = lagrange_index(dim,web.lagrange_order,inx); 
          inx[k] = l_order;
          new = lagrange_index(dim,l_order,inx);
          inx[k] = 0;
          v[new] = v[old];
        }

     /* create new interior */
     for ( m = 0 ; m <= dim ; m++ ) inx[m] = 0;
     inx[0] = l_order;
     for ( k = 0 ; k < new_fctrl ; k++ )
     { for ( m = 0 ; m <= dim ; m++ )
          if ( inx[m] == 0 ) break;
        if ( m > dim ) /* create interior vertex */
        { v[k] = new_vertex(newx[k],f_id);  
          if ( get_fattr(f_id) & FIXED ) set_attr(v[k],FIXED);
          set_attr(v[k],Q_MIDFACET);
          set_vertex_facet(v[k],f_id);  /* since doesn't have edge */
          /* find centerpoint parameters for facet on boundary */
          if ( get_fattr(f_id) & BOUNDARY )    /* not working for torus */
          {
             REAL defaultp[MAXCOORD];
             REAL *paramb,*parammid,*xb;
             vertex_id base_v;
             REAL s[MAXCOORD];
             struct boundary *bdry;
             facetedge_id fe;

             set_attr(v[k],BOUNDARY);
             bdry = get_facet_boundary(f_id);
             set_boundary_num(v[k],bdry->num);

             /* v[k] parameters extrapolate from a vertex */
             /* try to find a vertex on same boundary */
             base_v = NULLVERTEX;
             fe = get_facet_fe(f_id);
             for ( i = 0 ; i < FACET_EDGES ; i++, fe = get_next_edge(fe) )
                { 
                  if ( bdry == get_boundary(get_fe_tailv(fe)) )
                     base_v = get_fe_tailv(fe);
                }
             if ( valid_id(base_v) )
                { paramb = get_param(base_v);
                  xb = get_coord(base_v);
                  for ( i = 0 ; i < SDIM ; i++ )
                     s[i] = xb[i];  /* displacement vector */
                }
             else
                { paramb = defaultp;
                  defaultp[0] = defaultp[1] = defaultp[2] = 0.1;
                  for ( i = 0 ; i < SDIM ; i++ )
                     s[i] = eval(bdry->coordf[i],defaultp,NULLID,NULL);
                  sprintf(msg,
                    "Could not find vertex on same boundary as facet %s.\n",
                     ELNAME(f_id));
                }

             parammid = get_param(v[k]);
             b_extrapolate(bdry,s,newx[k],newx[k],paramb,parammid,v[k]);
          }
          else if ( get_fattr(f_id) & CONSTRAINT )    
          {
             ATTR attr = get_fattr(f_id) & (BDRY_ENERGY|BDRY_CONTENT|CONSTRAINT);
             conmap_t * conmap = get_f_constraint_map(f_id);

             set_attr(v[k],attr);
             set_v_conmap(v[k],conmap);
             project_v_constr(v[k],ACTUAL_MOVE,RESET_ONESIDEDNESS);
          }
        }
        increment_lagrange_index(dim,inx);
     }
  }
  /* do edges */
  /* calculate transition matrix */
  dim = (web.representation==STRING)? 1 : web.dimension-1;
  gauss_lagrange_setup(dim,l_order,web.gauss1D_order);

  if ( bezier_flag )
  { bezier_trans_1d(web.lagrange_order,l_order,trans);
  }
  else
  { /* regular lagrange stuff */
  
    for ( m = 0 ; m <= dim ; m++ ) inx[m] = 0;
    inx[0] = l_order;
    new = 0;
    do
    { old = 0;
      for ( m = 0 ; m <= dim ; m++ ) oldinx[m] = 0;
      oldinx[0] = web.lagrange_order;
      do
      { REAL prod;
        for ( k = 0, prod = 1.0 ; k <= dim ; k++ )
          for ( m = 0 ; m < oldinx[k] ; m++ )
           prod *= ((REAL)(inx[k])/l_order*web.lagrange_order - m)/(oldinx[k]-m); 
        trans[new][old] = prod;
        old++;
      } while ( increment_lagrange_index(dim,oldinx) );
      new++;
    } while ( increment_lagrange_index(dim,inx) );
  }

  /* calc indices of extreme vertices; handy elsewhere */
  for ( m = 0 ; m <= dim ; m++ ) inx[m] = 0;
  for ( k = 0 ; k <= dim ; k++ )
  { inx[k] = l_order;
    new = lagrange_index(dim,l_order,inx);
    inx[k] = 0;
    web.skel[EDGE].extreme[k] = new;
  }

  MFOR_ALL_EDGES(e_id)
   { vertex_id *v;
     get_edge_verts(e_id,oldx,NULL);
     mat_mult(trans,oldx,newx,new_ectrl,old_ectrl,SDIM);
     v = get_edge_vertices(e_id);

     /* free old interior */
     for ( m = 0 ; m <= dim ; m++ ) inx[m] = 0;
     inx[0] = web.lagrange_order;
     for ( k = 0 ; k < old_ectrl ; k++ )
     { for ( m = 0 ; m <= dim ; m++ )
          if ( inx[m] == 0 ) break;
        if ( m > dim ) free_element(v[k]);  /* free only interior */
        increment_lagrange_index(dim,inx);
     }
     /* move extreme vertices */
     for ( m = 0 ; m <= dim ; m++ ) inx[m] = 0;
     if ( l_order < web.lagrange_order )
        for ( k = 0 ; k <= dim ; k++ )
        { inx[k] = web.lagrange_order; 
          old = lagrange_index(dim,web.lagrange_order,inx); 
          inx[k] = l_order;
          new = lagrange_index(dim,l_order,inx);
          inx[k] = 0;
          v[new] = v[old];
        }
     else
        for ( k = dim ; k >= 0 ; k-- )
        { inx[k] = web.lagrange_order; 
          old = lagrange_index(dim,web.lagrange_order,inx); 
          inx[k] = l_order;
          new = lagrange_index(dim,l_order,inx);
          inx[k] = 0;
          v[new] = v[old];
        }

     /* create new interior */
     for ( m = 0 ; m <= dim ; m++ ) inx[m] = 0;
     inx[0] = l_order;
     for ( k = 0 ; k < new_ectrl ; k++ )
     { for ( m = 0,zeros=0 ; m <= dim ; m++ )
          if ( inx[m] == 0 ) zeros++;
        if ( zeros != dim )
        {
          v[k] = new_vertex(newx[k],e_id); 
          if ( get_eattr(e_id) & FIXED ) 
             set_attr(v[k],FIXED);
          set_attr(v[k],Q_MIDEDGE);
          set_vertex_edge(v[k],e_id);

          /* for boundary edges, cannot just interpolate parameters
              due to wrap-around of angular parameters. So tangent extrapolate
              from one endpoint.
         */
          if ( get_eattr(e_id) & BOUNDARY )
          { 
             struct boundary *bdry;
             REAL *paramb,*parammid,*mu,*mv;
             vertex_id base_v = NULLID;
             vertex_id headv = v[l_order];
             vertex_id tailv = v[0];
     
             if ( web.representation == SIMPLEX )
                kb_error(1802,"Can't do Lagrange boundaries in SIMPLEX model.\n",RECOVERABLE);

             bdry = get_edge_boundary(e_id);
             if ( get_boundary(headv) == bdry )
                 base_v = headv;
             else if ( get_boundary(tailv) == bdry )
                 base_v = tailv;
             else
                { sprintf(errmsg,
         "Vertices %s and %s of edge %s are on different boundaries, %s, %s, %s.\n",
                    ELNAME(headv),ELNAME1(tailv),ELNAME2(e_id),
                     get_boundary(headv) ? get_boundary(headv)->name : "none",
                     get_boundary(tailv) ? get_boundary(tailv)->name : "none",
                     bdry ? bdry->name : "none");
                  kb_error(1803,errmsg,RECOVERABLE);

                }

             set_attr(v[k],BOUNDARY);
             set_boundary_num(v[k],bdry->num);
     
             /* projecting on tangent */
             mv = get_coord(v[k]);
             mu = get_coord(base_v);
             paramb = get_param(base_v);
             parammid = get_param(v[k]);
             b_extrapolate(bdry,mu,mv,mv,paramb,parammid,v[k]);

          }
          else if ( get_eattr(e_id) & CONSTRAINT )
          { 
             ATTR attr = get_eattr(e_id) & (BDRY_ENERGY|BDRY_CONTENT|CONSTRAINT );
             conmap_t * conmap = get_e_constraint_map(e_id);
     
             set_attr(v[k],attr);
             set_v_conmap(v[k],conmap);
             project_v_constr(v[k],ACTUAL_MOVE,RESET_ONESIDEDNESS);
          }
        }
        increment_lagrange_index(dim,inx);
     }
   }
  /* now install edge vertices in facets */
  /* being careful with edge orientation */
  if ( web.representation != STRING )
    MFOR_ALL_FACETS(f_id)
     { vertex_id *v,*ev;
        facetedge_id fe;

        dim = web.dimension;
        v = get_facet_vertices(f_id);
/* just 2D facets for now */
        fe=get_facet_fe(f_id);
        e_id = get_fe_edge(fe);
        ev = get_edge_vertices(e_id);
        if ( inverted(f_id) == inverted(e_id) )
          for ( k = 1 ; k < l_order ; k++ ) v[k] = ev[k];
        else
          for ( k = 1 ; k < l_order ; k++ ) v[k] = ev[l_order-k];

        fe=get_next_edge(fe);
        e_id = get_fe_edge(fe);
        ev = get_edge_vertices(e_id);
        for ( k = 1 ; k < l_order ; k++ )
        { inx[1] = l_order-k;
          inx[2] = k;
          inx[0] = 0;
          spot = lagrange_index(dim,l_order,inx);
          if ( inverted(f_id) == inverted(e_id) )
             v[spot] = ev[k];
          else
             v[spot] = ev[l_order-k];
        }
        fe=get_next_edge(fe);
        e_id = get_fe_edge(fe);
        ev = get_edge_vertices(e_id);
        for ( k = 1 ; k < l_order ; k++ )
        { inx[1] = 0;
          inx[0] = k;
          inx[2] = l_order - k;
          spot = lagrange_index(dim,l_order,inx);
          if ( inverted(f_id) == inverted(e_id) )
             v[spot] = ev[k];
          else
             v[spot] = ev[l_order-k];
        }
     }
    
  free_matrix(trans);
  free_matrix(newx);
  free_matrix(oldx);

  if ( l_order < web.lagrange_order )
  { expand_attribute(EDGE,E_VERTICES_ATTR,&new_ectrl);
    expand_attribute(FACET,F_VERTICES_ATTR,&new_fctrl);
  } /* can now shrink  */

  web.lagrange_order = l_order;
  web.skel[FACET].ctrlpts = new_fctrl;
  web.skel[EDGE].ctrlpts = edge_ctrl = new_ectrl;
  change_flag = 1;
  web_timestamp = top_timestamp = ++global_timestamp;
  web.headvnum = l_order;

  LEAVE_GRAPH_MUTEX;

}

/*******************************************************************
*
*    Function:  change_model()
*
*    Purpose:  Ask user what model he wants.
*/

void change_model()
{
  char ans[100];

  if ( web.modeltype == LINEAR )
  { outstring("Current model type is LINEAR.\n");
    if ( web.symmetric_content )
    { outstring("SYMMETRIC CONTENT supported only in linear model.\n");
      return;
    }
    if ( SDIM > 3 )
    { outstring("Space dimension > 3 supported only in linear model.\n");
      return;
    }
    prompt("Pick new model type, 1 LINEAR,  2 QUADRATIC, >2 LAGRANGE: ",
              ans,sizeof(ans));
    if ( logfd ) fprintf(logfd,"%s\n",ans);
    switch ( ans[0] )
    { case '1': break;
      case '2': linear_to_quad(); break;
      default : linear_to_lagrange(ans[0]-'0'); break;
    }
  }
  else if ( web.modeltype == QUADRATIC )
  { outstring("Current model type is QUADRATIC.\n");
    prompt("Pick new model type, 1 LINEAR,  2 QUADRATIC, >2 LAGRANGE: ",
      ans,sizeof(ans));
    if ( logfd ) fprintf(logfd,"%s\n",ans);
    switch ( ans[0] )
    { case '1': quad_to_linear(); break;
      case '2': break;
      default : quad_to_lagrange(ans[0]-'0'); break;
    }
  }
  else if ( web.modeltype == LAGRANGE )
  { outstring("Current model type is LAGRANGE.\n");
    prompt("Pick new model type, 1 LINEAR,  2 QUADRATIC, >2 LAGRANGE: ",ans,sizeof(ans));
    if ( logfd ) fprintf(logfd,"%s\n",ans);
    switch ( ans[0] )
    { case '1': lagrange_to_linear(); break;
      case '2': lagrange_to_quad(); break;
      default : lagrange_to_lagrange(ans[0]-'0'); break;
    }
  }
}

/***************************************************************
*
*  Function: linear_to_quad()
*
*  Purpose:  Changes linear patch model to quadratic patch
*                model by inserting midpoints in all edges
*                and resetting function pointers.
*/

void linear_to_quad()
{ int three = 3,facet_ctrl = FACET_CTRL;
  edge_id e_id;
        
  if ( web.modeltype == QUADRATIC ) return;

  if ( square_curvature_flag )
      kb_error(1827,"Cannot do square curvature in quadratic model.\n",
            RECOVERABLE);

  if ( web.symmetry_flag && (web.skel[BODY].count > web.full_flag) 
      && !everything_quantities_flag )
  { body_id b_id;
    FOR_ALL_BODIES(b_id)
      if ( get_battr(b_id) & FIXEDVOL )
      { if ( auto_convert_flag ) convert_to_quantities(); else
        { kb_error(1804,"Quotient spaces volume needs convert_to_quantities.\n",
            RECOVERABLE);
          break;
        }
      }
  } 

  if ( web.symmetric_content  && !everything_quantities_flag )
  { if ( auto_convert_flag ) convert_to_quantities(); else
    kb_error(1805,"Do convert_to_quantities for quadratic SYMMETRIC CONTENT.\n",
         RECOVERABLE);
  }

  if ( web.metric_flag  && !everything_quantities_flag )
  { if ( auto_convert_flag ) convert_to_quantities(); else
    kb_error(2849,"Do convert_to_quantities for quadratic metric model.\n",
         RECOVERABLE);
  }

  if ( (SDIM > 3) && !(everything_quantities_flag) )
  { if ( auto_convert_flag ) convert_to_quantities(); else
     kb_error(1807,"Do convert_to_quantities to do high dimension space quadratic.\n",
        RECOVERABLE);
  }

#ifdef MPI_EVOLVER
  if ( this_task == MASTER_TASK )
    mpi_model(2,QUADRATIC);
#endif

  ENTER_GRAPH_MUTEX;
  
  expand_attribute(EDGE,E_VERTICES_ATTR,&three);
  expand_attribute(FACET,F_VERTICES_ATTR,&facet_ctrl);
  MFOR_ALL_EDGES(e_id)
  { REAL *t,x[MAXCOORD];
    int i;
    vertex_id headv,tailv;
    vertex_id new_v;
    REAL side[MAXCOORD];
        
    headv = get_edge_headv(e_id);
    tailv = get_edge_tailv(e_id);
    t = get_coord(tailv);
    get_edge_side(e_id,side);
    for ( i = 0 ; i < SDIM ; i++ )
      x[i] = t[i] + side[i]/2;
    new_v = new_vertex(x,e_id);
    set_edge_midv(e_id,new_v);

    if ( get_eattr(e_id) & FIXED ) 
         set_attr(new_v,FIXED);

    /* for boundary edges, cannot just interpolate parameters
       due to wrap-around of angular parameters. So tangent extrapolate
       from one endpoint.
    */
    if ( get_eattr(e_id) & BOUNDARY )
    { struct boundary *bdry;
      REAL *paramb,*parammid,*mu,*mv;
      vertex_id base_v = NULLID;
     
      bdry = get_edge_boundary(e_id);
      if ( get_boundary(headv) == bdry )
          base_v = headv;
      else if ( get_boundary(tailv) == bdry )
          base_v = tailv;
      else
         { sprintf(errmsg,
  "Vertices %s and %s of edge %s are on different boundaries than the edge.\n",
              ELNAME(headv),ELNAME1(tailv),ELNAME2(e_id)+1);
           kb_error(1808,errmsg,RECOVERABLE);
         }

      set_attr(new_v,BOUNDARY);
      set_boundary_num(new_v,bdry->num);
     
      /* projecting on tangent */
      mv = get_coord(new_v);
      mu = get_coord(base_v);
      paramb = get_param(base_v);
      parammid = get_param(new_v);
      b_extrapolate(bdry,mu,mv,mv,paramb,parammid,new_v);

    }
    else if ( get_eattr(e_id) & CONSTRAINT )
    { ATTR attr = get_eattr(e_id) & (BDRY_ENERGY|BDRY_CONTENT|CONSTRAINT );
      conmap_t * conmap = get_e_constraint_map(e_id);
    
      set_attr(new_v,attr);
      set_v_conmap(new_v,conmap);
      project_v_constr(new_v,ACTUAL_MOVE,RESET_ONESIDEDNESS);
    }
  }

  web.modeltype = QUADRATIC;
  web.lagrange_order = 2;
  web.skel[EDGE].ctrlpts = edge_ctrl = 3;
  web.skel[FACET].ctrlpts = 6;

  /* redirect functions */
  calc_facet_volume = facet_volume_q;
  film_grad = film_grad_q;
  calc_edge_area = edge_area_q;
  string_grad = string_grad_q;
  calc_facet_energy = facet_energy_q;
  calc_facet_forces = facet_force_q;
  if ( web.metric_flag )
    {
      calc_edge_energy = edge_energy_q_metric;
      calc_edge_forces  = edge_force_q_metric;
    }
  else
    {
      calc_edge_energy = edge_energy_q;
      calc_edge_forces  = edge_force_q;
    }

  adjust_integration_orders(2);
  gauss_setup();    /* set up gaussian integration arrays */
  change_flag = 1;
  web_timestamp = top_timestamp = ++global_timestamp;

  LEAVE_GRAPH_MUTEX;

}
  
/***************************************************************
*
*  Function: quad_to_linear()
*
*  Purpose:  Changes quadratic patch model to linear patch
*                model by deleting midpoints from all edges
*                and resetting function pointers.
*/

void quad_to_linear()
{
  edge_id e_id;

  if ( web.modeltype == LINEAR ) return;

#ifdef MPI_EVOLVER
  if ( this_task == MASTER_TASK )
    mpi_model(1,LINEAR);
#endif

  ENTER_GRAPH_MUTEX;
  
  MFOR_ALL_EDGES(e_id)
  {
     free_element(get_edge_midv(e_id));
  }

  web.modeltype = LINEAR;
  web.lagrange_order = 1;
  calc_facet_energy = facet_energy_l;
  calc_facet_forces = facet_force_l;
  calc_facet_volume = facet_volume_l;
  film_grad = film_grad_l;
  if ( web.metric_flag )
    {
      calc_edge_energy = edge_energy_l_metric;
      calc_edge_forces  = edge_force_l_metric;
    }
  else
    {
      calc_edge_energy = edge_energy_l;
      calc_edge_forces  = edge_force_l;
    }
  calc_edge_area = edge_area_l;
  string_grad = string_grad_l;
  web.skel[EDGE].ctrlpts = edge_ctrl = web.skel[EDGE].dimension+1;
  web.skel[FACET].ctrlpts = web.skel[FACET].dimension+1;

  adjust_integration_orders(1);
  gauss_setup();    /* set up gaussian integration arrays */
  change_flag = 1;
  web_timestamp = top_timestamp = ++global_timestamp;
  
  LEAVE_GRAPH_MUTEX;
}
 
/**********************************************************************
*
*  function: gauss_setup()
*
*  purpose:  Initialize arrays used in gaussian integration.
*
*  Does arbitrary order in 1D.
*  Currently does only 7-pt integration for 2D
*  and n+1 pt for over 2D.
*
***********************************************************************/

#include "f2c.h"
int gaussq_ ARGS((integer*, integer*, doublereal*, doublereal*, integer*, 
    doublereal*, doublereal*, doublereal*, doublereal*));
int class_ ARGS((integer*, integer*, doublereal*, doublereal*, doublereal*, 
    doublereal*, doublereal*));
int gausq2_ ARGS((integer*, doublereal*, doublereal*, doublereal*, integer*));
REAL pow_dd ARGS((REAL*,REAL*));
REAL dgamma_ ARGS((REAL*));
REAL d1mach_ ARGS((void));
REAL d_sign ARGS((REAL*,REAL*)); /* from f2c's libf77 */

void gauss_setup()
{  int i,j,k,m;

    if ( web.representation == STRING )
         gauss_lagrange_setup(1,web.lagrange_order,web.gauss1D_order);
    else
    { gauss_lagrange_setup(web.dimension-1,web.lagrange_order,web.gauss1D_order);
      gauss_lagrange_setup(web.dimension,web.lagrange_order,web.gauss2D_order);
    }

    /* set number of control points */
    ctrl_num = web.dimension + 1;
    if ( web.modeltype == QUADRATIC )
      ctrl_num += web.dimension*(web.dimension+ 1)/2;

    /* set number of integration points and weights */
    gauss1D_num = (abs(web.gauss1D_order)+2)/2;
    if ( web.dimension == 2 )
      { 
         if ( web.gauss2D_order >=  14 ) 
         { /* use conical product formula */
            int npts = 1+web.gauss2D_order/2; /* per dimension */
            if ( conical_x ) free_matrix(conical_x);
            if ( conical_w ) myfree((char*)conical_w);
            conical_x = dmatrix(0,npts*npts-1,0,2);
            conical_w = (REAL*)mycalloc(npts*npts,sizeof(REAL));
            simplex_quadrature(2,web.gauss2D_order,npts*npts,conical_x,conical_w);
            gauss2D_num = npts*npts;
            gauss2Dpt = (barytype*)(conical_x[0]);
            gauss2Dwt = conical_w;
         }
         else if ( web.gauss2D_order >=  12 ) 
            { gauss2D_num = 37;
              gauss2Dpt = gauss2Dpt13;
              gauss2Dwt = gauss2Dwt13;
            }
         else if ( web.gauss2D_order >=  9 ) 
            { gauss2D_num = 28;
              gauss2Dpt = gauss2Dpt11;
              gauss2Dwt = gauss2Dwt11;
            }
         else if ( web.gauss2D_order >=  7 ) 
            { gauss2D_num = 16;
              gauss2Dpt = gauss2Dpt8;
              gauss2Dwt = gauss2Dwt8;
            }
         else if ( web.gauss2D_order >= 6 ) 
            { gauss2D_num = 12;
              gauss2Dpt = gauss2Dpt6;
              gauss2Dwt = gauss2Dwt6;
            }
         else if ( web.gauss2D_order >= 3 ) 
            { gauss2D_num = 7;
              gauss2Dpt = gauss2Dpt5;
              gauss2Dwt = gauss2Dwt5;
            }
         else if ( web.gauss2D_order >= 2 )
            { gauss2D_num = 3;
              gauss2Dpt = gauss2Dpt2;
              gauss2Dwt = gauss2Dwt2;
            }
         else
            { gauss2D_num = 1;
              gauss2Dpt = gauss2Dpt1;
              gauss2Dwt = gauss2Dwt1;
            }
      }
    else gauss2D_num = web.dimension + 1;

    /* always have 1D integration for edges on constraints */
    web.gauss1D_order = abs(web.gauss1D_order);
    gauss1Dpt = (REAL *)mycalloc(gauss1D_num,sizeof(REAL));
    gauss1Dwt = (REAL *)mycalloc(gauss1D_num,sizeof(REAL));
    grule(gauss1D_num,gauss1Dpt,gauss1Dwt);
    if ( gauss1poly ) 
    { free_matrix(gauss1poly); gauss1poly = NULL;
      free_matrix(gauss1polyd); gauss1polyd = NULL;
    }
    if ( web.modeltype == LINEAR )
    { edge_ctrl = 2;
      gauss1poly = dmatrix(0,edge_ctrl-1,0,gauss1D_num-1);
      gauss1polyd = dmatrix(0,edge_ctrl-1,0,gauss1D_num-1);
      for ( m = 0 ; m < gauss1D_num ; m++ )
      { gauss1poly[0][m] = (1-gauss1Dpt[m]);
         gauss1poly[1][m] = gauss1Dpt[m];
         gauss1polyd[0][m] = -1.0;
         gauss1polyd[1][m] =  1.0;
      }
    }
    else if ( web.modeltype == QUADRATIC )
    { edge_ctrl = 3;
      gauss1poly = dmatrix(0,edge_ctrl-1,0,gauss1D_num-1);
      gauss1polyd = dmatrix(0,edge_ctrl-1,0,gauss1D_num-1);
      for ( m = 0 ; m < gauss1D_num ; m++ )
      { gauss1poly[0][m] = (1-2*gauss1Dpt[m])*(1-gauss1Dpt[m]);
         gauss1poly[1][m] = 4*gauss1Dpt[m]*(1-gauss1Dpt[m]);
         gauss1poly[2][m] = gauss1Dpt[m]*(2*gauss1Dpt[m]-1);
         gauss1polyd[0][m] = 4*gauss1Dpt[m]-3;
         gauss1polyd[1][m] = 4 - 8*gauss1Dpt[m];
         gauss1polyd[2][m] = 4*gauss1Dpt[m]-1;
      }
    }
    /* could have more modeltypes here */
    /* would have to up EDGE_CTRL to max of possible edge_ctrl */

    if ( web.dimension == 1 )
    {  gauss2Dwt = gauss1Dwt; gauss2Dpt = gauss2Dpt; gauss2D_num = gauss1D_num; 
    }
    else if ( web.dimension > 2 )
      { gauss2Dwt = (REAL *)mycalloc(gauss2D_num,sizeof(REAL));
         for ( i = 0 ; i < gauss2D_num ; i++ )
            gauss2Dwt[i] = 1.0/gauss2D_num;  /* trivial points */
        }

    /* set up interpolation polynomial values */
    if ( gpoly ) free_matrix(gpoly);
    gpoly = dmatrix(0,gauss2D_num-1,0,ctrl_num-1);
    if ( gpolypartial ) free_matrix3(gpolypartial);
    gpolypartial = dmatrix3(gauss2D_num,web.dimension,ctrl_num);
    if ( web.dimension == 1 )
      for ( j = 0 ; j < gauss2D_num ; j++ )
        for ( i = 0 ; i < ctrl_num ;  i++ )
        {
          REAL p=1.0,sum=0.0;
          int scale = ctrl_num - 1;
          for ( m = 0 ; m < ctrl_num ; m++ )
          { if ( m == i ) continue;
            p *= (gauss1Dpt[j]*scale - m)/(i - m);
            if ( p == 0.0 ) break;
            sum += scale/(gauss1Dpt[j]*scale - m);
          }
          gpoly[j][i] = p;
          gpolypartial[j][0][i] = sum*p;
        }
    else if ( web.dimension == 2 )
    { if ( web.modeltype == LINEAR )
      { for ( k = 0 ; k < gauss2D_num ; k++ )
        {
          gpoly[k][0] = gauss2Dpt[k][0];
          gpoly[k][1] = gauss2Dpt[k][1];
          gpoly[k][2] = gauss2Dpt[k][2];
          gpolypartial[k][0][0] = -1.0;
          gpolypartial[k][1][0] = -1.0;
          gpolypartial[k][0][1] =  1.0;
          gpolypartial[k][1][2] =  1.0;    /* others 0 */
        }
      }
      else /* QUADRATIC */
      { for ( k = 0 ; k < gauss2D_num ; k++ )
          for ( j = 0 ; j < ctrl_num ; j++ )
          { gpoly[k][j] = intpoly6(j,2*gauss2Dpt[k][1],2*gauss2Dpt[k][2]);
            for ( i = 0 ; i < 2 ; i++ )
              gpolypartial[k][i][j] = 2*intpoly6part(j,i,
                               2*gauss2Dpt[k][1], 2*gauss2Dpt[k][2]);
                                  /* since intpoly was on side 2 triangle */
                                  /* and gauss2Dpt barycentric  */
          }
       }
     }
     else /* higher dimension */
     { /* crude: gauss pts same as control points */
       for ( k = 0 ; k < gauss2D_num ; k++ )
       { gpoly[k][k] = 1.0;  /* rest 0 */
         for ( j = 0 ; j < web.dimension ; j++ )
         {
           gpolypartial[k][j][0] = -1.0;
           gpolypartial[k][j][j+1] = 1.0;
         }          
       }
     }
}

    

/*************************************************************************
*
* function: simplex_quadrature()
*
* purpose: Set up quadrature coefficients on simplex.  Uses conical product
* formula from Stroud that represents simplex as crushed cube, with
* product of 1D Gauss-Jacobi formulas.
*
* return: Number of quadrature points, ((order+1)/2)^dim.
*
*/

int simplex_quadrature(dim,order,maxpt,x,w)
int dim;  /* dimension of simplex */
int order; /* order of polynomial to do correctly */
int maxpt; /* points available in x,w */
REAL **x; /* for barycentric coordinates of quadrature points */
REAL *w;  /* weights at quadrature points */
{ int N = 1+order/2;  /* points needed in 1D integration */
  int bsize;  /* number of total points */
  int iter[100]; /* iteration variables */
  int n;
  int count;
  int depth;
  REAL **gj_x;  /* gauss-jacobi nodes    [alpha][j] */
  REAL **gj_w;  /* gauss-jacobi weights [alpha][j] */
  REAL coeff; /* simplex factor */

  if ( N < 2 ) N = 2;  /* the lowest we have */

  for ( n = 0, bsize = 1 ; n < dim ; n++ ) bsize *= N;
  if ( bsize > maxpt ) { puts("too few points.\n"); exit(1); }

  gj_x = dmatrix(0,dim-1,0,N);
  gj_w = dmatrix(0,dim-1,0,N);

  /* get gauss-jacobi info */
  for ( n = 0 ; n < dim ; n++ )
     gauss_jacobi(N,(REAL)n,gj_x[n],gj_w[n]);

  /* overall simplex factor */
  coeff = (REAL)(1 << ((dim*(dim+1))/2)); 
  for ( n = 1 ; n <= dim ; n++ ) coeff /= n;

  /* iteration over all N^dim points */
  /* a little tricky since nesting depth is not fixed */
  for ( n = 0 ; n < dim ; n++ ) iter[n] = 0;

  count = 0; /* points so far */
  do
  { REAL prod;
     /* at bottom, do some work */
     /* weight */
     for ( w[count] = 1., n = 0 ; n < dim ; n++ )
        w[count] *= gj_w[dim-1-n][iter[n]];
     w[count] /= coeff;
     /* coordinates */
     for ( prod = 1., n = 0, x[count][dim] = 1.0 ; n < dim ; n++ )
     { x[count][n] = (1-gj_x[dim-1-n][iter[n]])/2*prod;
        x[count][dim] -= x[count][n];  /* barycentric */
        prod *= 1 - (1-gj_x[dim-1-n][iter[n]])/2;
     }
     count++;

     depth = dim-1;
     while ( ++iter[depth] >= N ) 
     { iter[depth] = 0;  /* prepare for next descent */
        depth--;  /* back up a level */
        if ( depth < 0 ) break; 
     }
  } while ( depth >= 0 );

  free_matrix(gj_x);
  free_matrix(gj_w);
  return count;
}


/****************************************************************************
*
* function: gauss_jacobi()
*
* purpose: calculate nodes and weights for gauss-jacobi integration
*             on (-1,1) with weight (1+x)^beta
*/

void gauss_jacobi(n,beta,t,w)
int n; /* the number of points used for the quadrature rule */
REAL beta; /* parameter */
REAL *t; /* will contain the desired nodes. */
REAL *w; /* will contain the desired weights w(j). */
{ int kind = 5; /* jacobi, w(x) = (1-x)**alpha * (1+x)**beta on (-1,1) */
  REAL alpha  = 0.0;
  int kpts = 0; /*  endpoint nodes */
  REAL endpts[2]; /* fixed endpint nodes */
  REAL *b; /* real scratch array of length n */

  b = (REAL*)temp_calloc(n,sizeof(REAL));
  gaussq_(&kind, &n, &alpha, &beta, &kpts, endpts, b, t, w);
  temp_free((char*)b);
}

/* some stuff for gaussq() */
REAL dgamma_(x)
REAL *x;
{ REAL b;
  int n,k;

  n = (int)*x;
  if ( *x != (REAL)n )
    kb_error(1810,"Noninteger dgamma_ argument.\n",RECOVERABLE);

  if ( n < 1 )
    kb_error(1811,"Nonpositive dgamma_ argument.\n",RECOVERABLE);

  for ( b = 1.0, k = 2 ; k < n ; k++ ) b *= k;
  return b;
}

REAL d1mach_() { /* return DBL_EPSILON; */
  REAL eps;
  REAL one = 1.0;
  for ( eps = 1.0 ; one + eps != one ; eps /= 2.0 ) ;
  return 2.0*eps;
}

REAL pow_dd(x,y)
REAL *x,*y;
{ REAL a =  pow(*x,*y); 
  return  a;
}

REAL d_sign(a,b) /* from f2c's libf77 */
REAL *a,*b;
{
  REAL x;
  x = (*a >= 0. ? *a : - *a);
  return( *b >= 0. ? x : -x);
}

/***********************************************************************/
/* gaussq.f -- translated by f2c (version 19950314).
    You must link the resulting object file with the libraries:
          -lf2c -lm    (in that order)
*/


/* Table of constant values */

static doublereal c_b19 = 2.;
doublereal solve_ ARGS((REAL *, int *, REAL *, REAL *));

/* ====================================================================== */
/* NIST Guide to Available Math Software.                                 */
/* Fullsource for module GAUSSQ from package GO.                          */
/* Retrieved from NETLIB on Sat Mar 18 10:07:24 1995.                     */
/* ====================================================================== */

/* Subroutine */ int gaussq_(kind, n, alpha, beta, kpts, endpts, b, t, w)
integer *kind, *n;
doublereal *alpha, *beta;
integer *kpts;
doublereal *endpts, *b, *t, *w;
{
     /* System generated locals */
     integer i__1;
     doublereal d__1;


     /* Local variables */
     integer ierr, i;
     doublereal t1;
     doublereal muzero, gam;


/*              this set of routines computes the nodes t(j) and weights */
/*          w(j) for gaussian-type quadrature rules with pre-assigned */
/*          nodes.  these are used when one wishes to approximate */

/*                      integral (from a to b)  f(x) w(x) dx */

/*                                        n */
/*          by                         sum w  f(t ) */
/*                                      j=1  j     j */

/*          (note w(x) and w(j) have no connection with each other.) */
/*          here w(x) is one of six possible non-negative weight */
/*          functions (listed below), and f(x) is the */
/*          function to be integrated.  gaussian quadrature is particularly 
*/
/*          useful on infinite intervals (with appropriate weight */
/*          functions), since then other techniques often fail. */

/*              associated with each weight function w(x) is a set of */
/*          orthogonal polynomials.  the nodes t(j) are just the zeroes */
/*          of the proper n-th degree polynomial. */

/*      input parameters (all real numbers are in REAL precision) */

/*          kind      an integer between 1 and 6 giving the type of */
/*                      quadrature rule: */

/*          kind = 1:  legendre quadrature, w(x) = 1 on (-1, 1) */
/*          kind = 2:  chebyshev quadrature of the first kind */
/*                         w(x) = 1/sqrt(1 - x*x) on (-1, +1) */
/*          kind = 3:  chebyshev quadrature of the second kind */
/*                         w(x) = sqrt(1 - x*x) on (-1, 1) */
/*          kind = 4:  hermite quadrature, w(x) = exp(-x*x) on */
/*                         (-infinity, +infinity) */
/*          kind = 5:  jacobi quadrature, w(x) = (1-x)**alpha * (1+x)** */
/*                         beta on (-1, 1), alpha, beta .gt. -1. */
/*                         note: kind=2 and 3 are a special case of this. */
/*          kind = 6:  generalized laguerre quadrature, w(x) = exp(-x)* */
/*                         x**alpha on (0, +infinity), alpha .gt. -1 */

/*          n          the number of points used for the quadrature rule */
/*          alpha     real parameter used only for gauss-jacobi and gauss- */
/*                      laguerre quadrature (otherwise use 0.d0). */
/*          beta      real parameter used only for gauss-jacobi quadrature-- 
*/
/*                      (otherwise use 0.d0) */
/*          kpts      (integer) normally 0, unless the left or right end- */
/*                      point (or both) of the interval is required to be a */
/*                      node (this is called gauss-radau or gauss-lobatto */
/*                      quadrature).  then kpts is the number of fixed */
/*                      endpoints (1 or 2). */
/*          endpts    real array of length 2.  contains the values of */
/*                      any fixed endpoints, if kpts = 1 or 2. */
/*          b          real scratch array of length n */

/*      output parameters (both REAL precision arrays of length n) */

/*          t          will contain the desired nodes. */
/*          w          will contain the desired weights w(j). */

/*      underflow may sometimes occur, but is harmless. */

/*      references */
/*          1.  golub, g. h., and welsch, j. h., "calculation of gaussian */
/*                quadrature rules," mathematics of computation 23 (april, */
/*                1969), pp. 221-230. */
/*          2.  golub, g. h., "some modified matrix eigenvalue problems," */
/*                siam review 15 (april, 1973), pp. 318-334 (section 7). */
/*          3.  stroud and secrest, gaussian quadrature formulas, prentice- 
*/
/*                hall, englewood cliffs, n.j., 1966. */

/*          original version 20 jan 1975 from stanford */
/*          modified 21 dec 1983 by eric grosse */
/*             imtql2 => gausq2 */
/*             hex constant => d1mach (from core library) */
/*             compute pi using datan */
/*             removed accuracy claims, description of method */
/*             added single precision version */


     /* Parameter adjustments */
     --w;
     --t;
     --b;
     --endpts;

     /* Function Body */
     class_(kind, n, alpha, beta, &b[1], &t[1], &muzero);

/*              the matrix of coefficients is assumed to be symmetric. */
/*              the array t contains the diagonal elements, the array */
/*              b the off-diagonal elements. */
/*              make appropriate changes in the lower right 2 by 2 */
/*              submatrix. */

     if (*kpts == 0) {
          goto L100;
     }
     if (*kpts == 2) {
          goto L50;
     }

/*              if kpts=1, only t(n) must be changed */

/* Computing 2nd power */
     d__1 = b[*n - 1];
     t[*n] = solve_(&endpts[1], n, &t[1], &b[1]) * (d__1 * d__1) + endpts[1];
     goto L100;

/*              if kpts=2, t(n) and b(n-1) must be recomputed */

L50:
     gam = solve_(&endpts[1], n, &t[1], &b[1]);
     t1 = (endpts[1] - endpts[2]) / (solve_(&endpts[2], n, &t[1], &b[1]) - gam)
                ;
     b[*n - 1] = sqrt(t1);
     t[*n] = endpts[1] + gam * t1;

/*              note that the indices of the elements of b run from 1 to n-1 
*/
/*              and thus the value of b(n) is arbitrary. */
/*              now compute the eigenvalues of the symmetric tridiagonal */
/*              matrix, which has been modified as necessary. */
/*              the method used is a ql-type method with origin shifting */

L100:
     w[1] = 1.;
     i__1 = *n;
     for (i = 2; i <= i__1; ++i) {
/* L105: */
          w[i] = 0.;
     }

     gausq2_(n, &t[1], &b[1], &w[1], &ierr);

     i__1 = *n;
     for (i = 1; i <= i__1; ++i) {
/* L110: */
          w[i] = muzero * w[i] * w[i];
     }

     return 0;
} /* gaussq_ */




doublereal solve_(shift, n, a, b)
doublereal *shift;
integer *n;
doublereal *a, *b;
{
     /* System generated locals */
     integer i__1;
     doublereal ret_val, d__1;

     /* Local variables */
     integer i;
     doublereal alpha;
     integer nm1;


/*         this procedure performs elimination to solve for the */
/*         n-th component of the solution delta to the equation */

/*                 (jn - shift*identity) * delta  = en, */

/*         where en is the vector of all zeroes except for 1 in */
/*         the n-th position. */

/*         the matrix jn is symmetric tridiagonal, with diagonal */
/*         elements a(i), off-diagonal elements b(i).  this equation */
/*         must be solved to obtain the appropriate changes in the lower */
/*         2 by 2 submatrix of coefficients for orthogonal polynomials. */



     /* Parameter adjustments */
     --b;
     --a;

     /* Function Body */
     alpha = a[1] - *shift;
     nm1 = *n - 1;
     i__1 = nm1;
     for (i = 2; i <= i__1; ++i) {
/* L10: */
/* Computing 2nd power */
          d__1 = b[i - 1];
          alpha = a[i] - *shift - d__1 * d__1 / alpha;
     }
     ret_val = 1. / alpha;
     return ret_val;
} /* solve_ */




/* Subroutine */ int class_(kind, n, alpha, beta, b, a, muzero)
integer *kind, *n;
doublereal *alpha, *beta, *b, *a, *muzero;
{
     /* System generated locals */
     integer i__1;
     doublereal d__1, d__2, d__3;


     /* Local variables */
     integer i;
     doublereal ab;
     doublereal pi;
     integer nm1;
     doublereal a2b2, abi;


/*              this procedure supplies the coefficients a(j), b(j) of the */
/*          recurrence relation */

/*                 b p (x) = (x - a ) p    (x) - b    p    (x) */
/*                  j j                j    j-1         j-1 j-2 */

/*          for the various classical (normalized) orthogonal polynomials, 
*/
/*          and the zero-th moment */

/*                 muzero = integral w(x) dx */

/*          of the given polynomial's weight function w(x).  since the */
/*          polynomials are orthonormalized, the tridiagonal matrix is */
/*          guaranteed to be symmetric. */

/*              the input parameter alpha is used only for laguerre and */
/*          jacobi polynomials, and the parameter beta is used only for */
/*          jacobi polynomials.  the laguerre and jacobi polynomials */
/*          require the gamma function. */


     /* Parameter adjustments */
     --a;
     --b;

     /* Function Body */
     pi = atan(1.) * 4.;
     nm1 = *n - 1;
     switch ((int)*kind) {
          case 1:  goto L10;
          case 2:  goto L20;
          case 3:  goto L30;
          case 4:  goto L40;
          case 5:  goto L50;
          case 6:  goto L60;
     }

/*                  kind = 1:  legendre polynomials p(x) */
/*                  on (-1, +1), w(x) = 1. */

L10:
     *muzero = 2.;
     i__1 = nm1;
     for (i = 1; i <= i__1; ++i) {
          a[i] = 0.;
          abi = (doublereal) i;
/* L11: */
          b[i] = abi / sqrt(abi * 4 * abi - 1.);
     }
     a[*n] = 0.;
     return 0;

/*                  kind = 2:  chebyshev polynomials of the first kind t(x) */
/*                  on (-1, +1), w(x) = 1 / sqrt(1 - x*x) */

L20:
     *muzero = pi;
     i__1 = nm1;
     for (i = 1; i <= i__1; ++i) {
          a[i] = 0.;
/* L21: */
          b[i] = .5;
     }
     b[1] = sqrt(.5);
     a[*n] = 0.;
     return 0;

/*                  kind = 3:  chebyshev polynomials of the second kind u(x) 
*/
/*                  on (-1, +1), w(x) = sqrt(1 - x*x) */

L30:
     *muzero = pi / 2.;
     i__1 = nm1;
     for (i = 1; i <= i__1; ++i) {
          a[i] = 0.;
/* L31: */
          b[i] = .5;
     }
     a[*n] = 0.;
     return 0;

/*                  kind = 4:  hermite polynomials h(x) on (-infinity, */
/*                  +infinity), w(x) = exp(-x**2) */

L40:
     *muzero = sqrt(pi);
     i__1 = nm1;
     for (i = 1; i <= i__1; ++i) {
          a[i] = 0.;
/* L41: */
          b[i] = sqrt(i / 2.);
     }
     a[*n] = 0.;
     return 0;

/*                  kind = 5:  jacobi polynomials p(alpha, beta)(x) on */
/*                  (-1, +1), w(x) = (1-x)**alpha + (1+x)**beta, alpha and */
/*                  beta greater than -1 */

L50:
     ab = *alpha + *beta;
     abi = ab + 2.;
     d__1 = ab + 1.;
     d__2 = *alpha + 1.;
     d__3 = *beta + 1.;
     *muzero = pow_dd(&c_b19, &d__1) * dgamma_(&d__2) * dgamma_(&d__3) / 
                dgamma_(&abi);
     a[1] = (*beta - *alpha) / abi;
     b[1] = sqrt((*alpha + 1.) * 4. * (*beta + 1.) / ((abi + 1.) * abi * abi));
     a2b2 = *beta * *beta - *alpha * *alpha;
     i__1 = nm1;
     for (i = 2; i <= i__1; ++i) {
          abi = i * 2. + ab;
          a[i] = a2b2 / ((abi - 2.) * abi);
/* L51: */
          b[i] = sqrt(i * 4. * (i + *alpha) * (i + *beta) * (i + ab) / ((abi * 
                     abi - 1) * abi * abi));
/* ok so far */
     }
     abi = *n * 2. + ab;
     a[*n] = a2b2 / ((abi - 2.) * abi);
     return 0;

/*                  kind = 6:  laguerre polynomials l(alpha)(x) on */
/*                  (0, +infinity), w(x) = exp(-x) * x**alpha, alpha greater 
*/
/*                  than -1. */

L60:
     d__1 = *alpha + 1.;
     *muzero = dgamma_(&d__1);
     i__1 = nm1;
     for (i = 1; i <= i__1; ++i) {
          a[i] = i * 2. - 1. + *alpha;
/* L61: */
          b[i] = sqrt(i * (i + *alpha));
     }
     a[*n] = *n * 2. - 1 + *alpha;
     return 0;
} /* class_ */



/* Subroutine */ int gausq2_(n, d, e, z, ierr)
integer *n;
doublereal *d, *e, *z;
integer *ierr;
{
     /* System generated locals */
     integer i__1, i__2;
     doublereal d__1, d__2, d__3;

     /* Local variables */
     doublereal b, c, f, g;
     integer i, j, k, l, m;
     doublereal p, r, s;
     integer ii;
     doublereal machep;
     integer mml;


/*      this subroutine is a translation of an algol procedure, */
/*      num. math. 12, 377-383(1968) by martin and wilkinson, */
/*      as modified in num. math. 15, 450(1970) by dubrulle. */
/*      handbook for auto. comp., vol.ii-linear algebra, 241-248(1971). */
/*      this is a modified version of the 'eispack' routine imtql2. */

/*      this subroutine finds the eigenvalues and first components of the 
*/
/*      eigenvectors of a symmetric tridiagonal matrix by the implicit ql 
*/
/*      method. */

/*      on input: */

/*          n is the order of the matrix; */

/*          d contains the diagonal elements of the input matrix; */

/*          e contains the subdiagonal elements of the input matrix */
/*             in its first n-1 positions.  e(n) is arbitrary; */

/*          z contains the first row of the identity matrix. */

/*        on output: */

/*          d contains the eigenvalues in ascending order.  if an */
/*             error exit is made, the eigenvalues are correct but */
/*             unordered for indices 1, 2, ..., ierr-1; */

/*          e has been destroyed; */

/*          z contains the first components of the orthonormal eigenvectors 
*/
/*             of the symmetric tridiagonal matrix.  if an error exit is */
/*             made, z contains the eigenvectors associated with the stored 
*/
/*             eigenvalues; */

/*          ierr is set to */
/*             zero         for normal return, */
/*             j             if the j-th eigenvalue has not been */
/*                            determined after 30 iterations. */

/*      ------------------------------------------------------------------ 
*/


     /* Parameter adjustments */
     --z;
     --e;
     --d;

     /* Function Body */
     machep = d1mach_();
     *ierr = 0;
     if (*n == 1) {
          goto L1001;
     }

     e[*n] = 0.;
     i__1 = *n;
     for (l = 1; l <= i__1; ++l) {
          j = 0;
/*      :::::::::: look for small sub-diagonal element :::::::::: */
L105:
          i__2 = *n;
          for (m = l; m <= i__2; ++m) 
          { if (m == *n) { goto L120; }
            if ((d__1 = e[m], fabs(d__1)) <= machep * ((d__2 = d[m], fabs(d__2))
                            + (d__3 = d[m + 1], fabs(d__3))))
            { goto L120; }
/* L110: */
          }

L120:
          p = d[l];
          if (m == l) {
                goto L240;
          }
          if (j == 30) {
                goto L1000;
          }
          ++j;
/*      :::::::::: form shift :::::::::: */
          g = (d[l + 1] - p) / (e[l] * 2.);
          r = sqrt(g * g + 1.);
          g = d[m] - p + e[l] / (g + d_sign(&r, &g));
          s = 1.;
          c = 1.;
          p = 0.;
          mml = m - l;

/*      :::::::::: for i=m-1 step -1 until l do -- :::::::::: */
          i__2 = mml;
          for (ii = 1; ii <= i__2; ++ii) {
                i = m - ii;
                f = s * e[i];
                b = c * e[i];
                if (fabs(f) < fabs(g)) {
                     goto L150;
                }
                c = g / f;
                r = sqrt(c * c + 1.);
                e[i + 1] = f * r;
                s = 1. / r;
                c *= s;
                goto L160;
L150:
                s = f / g;
                r = sqrt(s * s + 1.);
                e[i + 1] = g * r;
                c = 1. / r;
                s *= c;
L160:
                g = d[i + 1] - p;
                r = (d[i] - g) * s + c * 2. * b;
                p = s * r;
                d[i + 1] = g + p;
                g = c * r - b;
/*      :::::::::: form first component of vector :::::::::: */
                f = z[i + 1];
                z[i + 1] = s * z[i] + c * f;
/* L200: */
                z[i] = c * z[i] - s * f;
          }

          d[l] -= p;
          e[l] = g;
          e[m] = 0.;
          goto L105;
L240:
          ;
     }

/*      :::::::::: order eigenvalues and eigenvectors :::::::::: */
     i__1 = *n;
     for (ii = 2; ii <= i__1; ++ii) {
          i = ii - 1;
          k = i;
          p = d[i];

          i__2 = *n;
          for (j = ii; j <= i__2; ++j) {
                if (d[j] >= p) {
                     goto L260;
                }
                k = j;
                p = d[j];
L260:
                ;
          }

          if (k == i) {
                goto L300;
          }
          d[k] = d[i];
          d[i] = p;
          p = z[i];
          z[i] = z[k];
          z[k] = p;
L300:
          ;
     }

     goto L1001;
/*      :::::::::: set error -- no convergence to an */
/*                     eigenvalue after 30 iterations :::::::::: */
L1000:
     *ierr = l;
L1001:
     return 0;
/*      :::::::::: last card of gausq2 :::::::::: */
} /* gausq2_ */

/*****************************************************************************
******************************************************************************
      Gauss-Lagrange elements of arbitrary order and dimension.
      Lagrange element of order n is simplex with control points
      in barycentric lattice with n points along each 1-edge.

******************************************************************************/

/******************************************************************************
*
* function: lagrange_index()
*
* purpose: convert lagrange barycentric index vector to linear index.
*    inx[0] least significant, inx[dim] most significant
*
*/
int lagrange_index(dim,order,inx)
int dim;
int order;
int *inx;
{ int spot;
  int k;
  int left;

  spot = binom_coeff(order+dim,dim)-1;
  left = order;
  for ( k = dim ; k > 0 ; k--  )
  { 
     left -= inx[k];
     spot -= binom_coeff(left-1+k,k);
  }
  return spot;
}
 
/******************************************************************************
*
* function: increment_lagrange_index()
*
* purpose: increment lagrange barycentric index vector.
*    inx[0] least significant, inx[dim] most significant.
*  So corner vertices appear in proper order.
*  Before first call, initialize inx[0] = lagrange_order
*  Return: 1  valid next index
*             0  done
*/

int increment_lagrange_index(dim,inx)
int dim;
int *inx;
{ int j;

  for ( j = 0 ; j < dim ; j++ )
     if ( inx[j] > 0 ) 
     { inx[j+1]++; inx[0] = inx[j]-1; if ( j > 0 ) inx[j] = 0; return 1; }
  return 0;  /* done */
}


/******************************************************************************
* 
* function: gauss_lagrange_setup()
*
* purpose: initialize lagrange element interpolation matrices
*             for finding gauss points from lagrange points.
*
*    Uses new global gauss arrays.
*/

void gauss_lagrange_setup(dim,lagrange_order,gauss_order)
int dim;  /* dimension of lagrange element */
int lagrange_order; /* order of lagrange element */
int gauss_order;    /* order of polynomial to integrate exactly */
{ struct gauss_lag *gl;
  int i,j,k,n;
  REAL *temp;
  int linx[MAXCOORD+1];

  if ( dim > MAXCOORD )
     kb_error(1812,"Surface dimension too high in gauss_lagrange_setup().\n",
          RECOVERABLE);

  if ( dim < 1 )
     kb_error(1813,"Dimension too low in gauss_lagrange_setup().\n",
          RECOVERABLE);

  if ( gauss_order > maxgaussorder[dim] )
  { gauss_lagrange[dim] = (struct gauss_lag *)kb_realloc(
       (char*)(gauss_lagrange[dim]),
       (gauss_order+1)*sizeof(struct gauss_lag));
    maxgaussorder[dim] = gauss_order;
  }
         
  gl = &gauss_lagrange[dim][gauss_order];
  if ( gl->gausspt ) goto do_lagrange; 

  /* first, do gauss points and weights */
  switch ( dim )
  { case 1: gl->gnumpts = (abs(gauss_order)+2)/2;
            gl->gausspt = dmatrix(0,gl->gnumpts-1,0,1);
            gl->gausswt = (REAL*)mycalloc(gl->gnumpts,sizeof(REAL));
            temp = (REAL*)temp_calloc(gl->gnumpts,sizeof(REAL));
            grule(gl->gnumpts,temp,gl->gausswt);
            for ( i = 0 ; i < gl->gnumpts ; i++ )
            { gl->gausspt[i][0] = temp[i];
              gl->gausspt[i][1] = 1.0 - temp[i];
            }
            temp_free((char*)temp);
            break;
     case 2:
         if ( gauss_order >=  14 ) 
         { /* use conical product formula */
            int npts = 1+gauss_order/2; /* per dimension */
            gl->gnumpts = npts*npts;
            gl->gausspt = dmatrix(0,gl->gnumpts-1,0,2);
            gl->gausswt = (REAL*)mycalloc(npts*npts,sizeof(REAL));
            simplex_quadrature(2,gauss_order,npts*npts,gl->gausspt,gl->gausswt);
         }
         else if ( gauss_order >=  12 ) 
            { gl->gnumpts = 37;
              gl->gausspt = dmatrix(0,gl->gnumpts-1,0,2);
              for ( i = 0 ; i < gl->gnumpts ; i++ )
                 for ( j = 0 ; j <= dim ; j++ )
                    gl->gausspt[i][j] = gauss2Dpt13[i][j];
              gl->gausswt = gauss2Dwt13;
            }
         else if ( gauss_order >=  9 ) 
            { gl->gnumpts = 28;
              gl->gausspt = dmatrix(0,gl->gnumpts-1,0,2);
              for ( i = 0 ; i < gl->gnumpts ; i++ )
                 for ( j = 0 ; j <= dim ; j++ )
                    gl->gausspt[i][j] = gauss2Dpt11[i][j];
              gl->gausswt = gauss2Dwt11;
            }
         else if ( gauss_order >=  7 ) 
            { gl->gnumpts = 16;
              gl->gausspt = dmatrix(0,gl->gnumpts-1,0,2);
              for ( i = 0 ; i < gl->gnumpts ; i++ )
                 for ( j = 0 ; j <= dim ; j++ )
                    gl->gausspt[i][j] = gauss2Dpt8[i][j];
              gl->gausswt = gauss2Dwt8;
            }
         else if ( gauss_order >= 6 ) 
            { gl->gnumpts = 12;
              gl->gausspt = dmatrix(0,gl->gnumpts-1,0,2);
              for ( i = 0 ; i < gl->gnumpts ; i++ )
                 for ( j = 0 ; j <= dim ; j++ )
                    gl->gausspt[i][j] = gauss2Dpt6[i][j];
              gl->gausswt = gauss2Dwt6;
            }
         else if ( gauss_order >= 3 ) 
            { gl->gnumpts = 7;
              gl->gausspt = dmatrix(0,gl->gnumpts-1,0,2);
              for ( i = 0 ; i < gl->gnumpts ; i++ )
                 for ( j = 0 ; j <= dim ; j++ )
                    gl->gausspt[i][j] = gauss2Dpt5[i][j];
              gl->gausswt = gauss2Dwt5;
            }
         else if ( gauss_order >= 2 )
            { gl->gnumpts = 3;
              gl->gausspt = dmatrix(0,gl->gnumpts-1,0,2);
              for ( i = 0 ; i < gl->gnumpts ; i++ )
                 for ( j = 0 ; j <= dim ; j++ )
                    gl->gausspt[i][j] = gauss2Dpt2[i][j];
              gl->gausswt = gauss2Dwt2;
            }
         else
            { gl->gnumpts = 1;
              gl->gausspt = dmatrix(0,gl->gnumpts-1,0,2);
              for ( i = 0 ; i < gl->gnumpts ; i++ )
                 for ( j = 0 ; j <= dim ; j++ )
                    gl->gausspt[i][j] = gauss2Dpt1[i][j];
              gl->gausswt = gauss2Dwt1;
            }
         break;

         default: /* over 2 */
         { 
            int npts = 1+gauss_order/2; /* per dimension */
            for ( i = 0, gl->gnumpts = 1 ; i < dim ; i++ ) gl->gnumpts *= npts;
            gl->gausspt = dmatrix(0,gl->gnumpts-1,0,dim);
            gl->gausswt = (REAL*)mycalloc(gl->gnumpts,sizeof(REAL));
            simplex_quadrature(dim,gauss_order,gl->gnumpts,gl->gausspt,gl->gausswt);
         }
         break;
  }
do_lagrange:

    /* now set up lagrange to gauss interpolation matrices */
    /* Lagrange points are in linear array in barycentric order */

    if ( gl->gpoly ) free_matrix(gl->gpoly);
    if ( gl->gpolypart ) free_matrix3(gl->gpolypart);
    if ( gl->lpolypart ) free_matrix3(gl->lpolypart);

    gl->lagrange_order = lagrange_order;
    gl->lagpts = binom_coeff(dim+lagrange_order,lagrange_order);
    gl->gpoly = dmatrix(0,gl->gnumpts-1,0,gl->lagpts-1);
    gl->gpolypart = dmatrix3(gl->gnumpts,dim,gl->lagpts);
    gl->lpolypart = dmatrix3(gl->lagpts,dim,gl->lagpts);

    for ( n = 0 ; n < gl->gnumpts ; n++ )
    { REAL *x;
      int inx[MAXCOORD+1];

      x = gl->gausspt[n];  /* full barycentric coords, adding to 1 */
      for ( i = 0 ; i <= dim ; i++ ) inx[i] = 0;
      inx[0] = lagrange_order;
      do 
      { REAL prod, prodpart[MAXCOORD];
        int ix,mm;
        for ( k = 0 ; k <= dim ; k++ ) prodpart[k] = 0.0;
        for ( i = 0, prod = 1.0, mm = 1 ; i <= dim ; i++ )
          for ( j = 0 ; j < inx[i] ; j++, mm++ )
          { if ( bezier_flag )
            { for ( k = 0 ; k <= dim ; k++ )
              { prodpart[k] *= mm*x[i]/(j+1);
                if ( i == k ) prodpart[k] += prod*mm/(j+1.);
              }
              prod *= mm*x[i]/(j+1.);
            }
            else /* regular Lagrange polynomials */ 
            { for ( k = 0 ; k <= dim ; k++ )
              { prodpart[k] *= (lagrange_order*x[i]-j)/(inx[i]-j);
                if ( i == k ) prodpart[k] += prod*lagrange_order/(inx[i]-j);
              }
              prod *= (lagrange_order*x[i]-j)/(inx[i]-j);
            }
          }
        ix = lagrange_index(dim,lagrange_order,inx);
        gl->gpoly[n][ix] = prod;
        for ( k = 0 ; k < dim ; k++ )
          gl->gpolypart[n][k][ix] = -(prodpart[k] - prodpart[dim]);
          /* neg since natural coord runs opposite barycentric coord */
      } while ( increment_lagrange_index(dim,inx) );
    }

    /* partials to calculate tangents at lagrange points */
    for ( i = 0 ; i <= dim ; i++ ) linx[i] = 0;
    linx[0] = lagrange_order;
    for ( n = 0 ; n < gl->lagpts ; n++ )
    { 
      int inx[MAXCOORD+1];

      for ( i = 0 ; i <= dim ; i++ ) inx[i] = 0;
      inx[0] = lagrange_order;
      do 
      {  REAL prod, prodpart[MAXCOORD];
         int ix;
         for ( k = 0 ; k <= dim ; k++ ) prodpart[k] = 0.0;
         for ( i = 0, prod = 1.0 ; i <= dim ; i++ )
         { 
           for ( j = 0 ; j < inx[i] ; j++ )
           { for ( k = 0 ; k <= dim ; k++ )
             { prodpart[k] *= ((REAL)linx[i]-j)/(inx[i]-j);
               if ( i == k ) 
                  prodpart[k] += prod/(inx[i]-j);
             }
             prod *= ((REAL)linx[i]-j)/(inx[i]-j);
           }
         }
         ix = lagrange_index(dim,lagrange_order,inx);
         for ( k = 0 ; k < dim ; k++ )
            gl->lpolypart[n][k][ix] = -(prodpart[k] - prodpart[dim]);
            /* neg since natural coord runs opposite barycentric coord */
      } while ( increment_lagrange_index(dim,inx) );
      increment_lagrange_index(dim,linx);
    }

}


/*************************************************************************
     Simplex model lagrange model changing.  Can't assume edges 
     join facets.  Have to construct catalog of points, indexed
     by barycentric coords.
*************************************************************************/


struct baryhash { int nn; /* number of vertices */
                  vertex_id vv[MAXCOORD+1]; /* vertices */
                  int xx[MAXCOORD+1];  /* barycentric coordinates */
                  vertex_id v_id; /* the vertex */
                } *baryhash_table;
int baryhash_size; /* allocated size */
int baryhash_count; /* active entries */
void baryhash_init ARGS((int));
int baryhash_func ARGS((int,vertex_id*,int*));
vertex_id baryhash_find ARGS((int,vertex_id*,int*,vertex_id));
void baryhash_end ARGS((void));

void baryhash_init(fctrl)
int fctrl; /* points per facet */
{ 
    baryhash_size = (3*fctrl/2)*web.skel[FACET].count;
    baryhash_table = (struct baryhash*)temp_calloc(baryhash_size,
        sizeof(struct baryhash));
    baryhash_count = 0;
}
void baryhash_end()
{ temp_free((char*)baryhash_table);
  baryhash_size = baryhash_count = 0;
}
int baryhash_func(n,v,x)
int n;
vertex_id *v;
int  *x;
{ unsigned int hash;
  int k;
  for ( k = 0, hash = 0 ; k < n ; k ++ ) 
  { hash += (unsigned int)(v[k]*1843723 + x[k]*17977); }
  return  hash % baryhash_size;
}

/* find and/or insert */
vertex_id baryhash_find(n,v,x,newv)
int n;        /* number of vertices */
vertex_id *v; /* vertices */
int *x;       /* barycentric coords, relative to l_order */
vertex_id newv; /* insert if nonnul and not found */
{ int nn;  /* number of nonzero  coords */
  vertex_id vv[MAXCOORD+1];  /* needed vertices, in order */
  int xx[MAXCOORD+1];  /* needed coordinates */
  int spot;
  int i,j,k;
  struct baryhash *b;

  /* clean up key with insertion sort */
  for ( i = 0, j = 0 ; i < n ; i++ )
  { 
     if ( x[i] == 0 ) continue;
     for ( k = j; k > 0 ; k-- )
        if ( vv[k-1] > v[i] )
        { vv[k] = vv[k-1]; xx[k] = xx[k-1]; }
        else break;
     vv[k] = v[i];
     xx[k] = x[i];
     j++;
  }
  nn = j;
  spot = baryhash_func(nn,vv,xx);
  for (;; spot++)
  { 
     if ( spot >= baryhash_size ) spot = 0;
     b = baryhash_table + spot;
     if ( b->nn == 0 ) /* vacant */
        break;
     if ( b->nn != nn ) continue;
     for ( i = 0 ; i < nn ; i++ )
        if ( (b->vv[i] != vv[i]) || (b->xx[i] != xx[i]) )
          break;
     if ( i == nn )  /* match */
     { if ( (newv != NULLID) && (newv != b->v_id) ) 
        { printf("nn = %d    newv = %08lX  b->v_id = %08lX\n",nn,
             (unsigned long)newv,(unsigned long)b->v_id);
          for ( j = 0 ; j < nn ; j++ ) printf("%08lX %d ",vv[j],xx[j]);
          printf("\n");
          kb_error(1815,"Internal error: Duplicate vertex in baryhash!\n",RECOVERABLE);     

        }
        return b->v_id;
     }
  }
  /* empty_spot: */
  if ( newv != NULLID )
  { /* insert */
     b->nn = nn;
     for ( k = 0 ; k < nn ; k++ )
     { b->vv[k] = vv[k];
        b->xx[k] = xx[k];
     }
     b->v_id = newv;
     baryhash_count++;
     if ( (REAL)baryhash_count > .9*baryhash_size ) 
        kb_error(1816,"Internal error: Baryhash near full!\n",WARNING);
  }
  return NULLID;
}
/************************************************************************
*
* Function: simplex_lagrange_to_lagrange()
*
* purpose: Convert order of Lagrange model.
*/
void simplex_lagrange_to_lagrange(l_order) 
int l_order;
{ int new_ectrl,new_fctrl; /* new number of control points */ 
  int old_ectrl = web.skel[EDGE].ctrlpts;
  int old_fctrl = web.skel[FACET].ctrlpts;
  int i,k,m; 
  int old,new;
  int inx[MAXCOORD+1];
  int oldinx[MAXCOORD+1];
  REAL **trans; /* transformation matrix old to new points */
  REAL **newx;  /* new coords */
  REAL **oldx;  /* old coords */
  facet_id f_id;
  edge_id e_id;
  vertex_id v_id;
  int dim;
  vertex_id cornerv[MAXCOORD+1]; /* extreme vertices of facet */

  if ( web.lagrange_order == l_order ) return; /* already OK */
  if ( l_order < 1 ) 
     kb_error(1817,"Lagrange order must be at least 1.\n",RECOVERABLE);

  ENTER_GRAPH_MUTEX;
  
  MFOR_ALL_VERTICES(v_id)  
    unset_attr(v_id,NEWVERTEX);  /* so can tell which new */

  new_ectrl = binom_coeff(l_order+web.dimension-1,web.dimension-1);
  new_fctrl = binom_coeff(l_order+web.dimension,web.dimension);
  baryhash_init(new_fctrl);
  
  if ( l_order > web.lagrange_order )
  { expand_attribute(EDGE,E_VERTICES_ATTR,&new_ectrl);
    expand_attribute(FACET,F_VERTICES_ATTR,&new_fctrl);
  } /* have to wait for later for shrink case */

  /* recalc all facet points */
  trans = dmatrix(0,new_fctrl,0,old_fctrl);
  newx = dmatrix(0,new_fctrl,0,SDIM);
  oldx = dmatrix(0,old_fctrl,0,SDIM);
  /* calculate transition matrix */
  dim = web.dimension;
  adjust_integration_orders(l_order);
  gauss_lagrange_setup(dim,l_order,web.gauss2D_order);
  for ( m = 0 ; m <= dim ; m++ ) inx[m] = 0;
  inx[0] = l_order;
  new = 0;
  do
   { old = 0;
     for ( m = 0 ; m <= dim ; m++ ) oldinx[m] = 0;
     oldinx[0] = web.lagrange_order;
     do
     { REAL prod;
        for ( k = 0, prod = 1.0 ; k <= dim ; k++ )
         for ( m = 0 ; m < oldinx[k] ; m++ )
          prod *= ((REAL)(inx[k])/l_order*web.lagrange_order - m)/(oldinx[k]-m); 
        trans[new][old] = prod;
        old++;
     } while ( increment_lagrange_index(dim,oldinx) );
     new++;
   } while ( increment_lagrange_index(dim,inx) );
  /* calc indices of extreme vertices; handy elsewhere */
  for ( m = 0 ; m <= dim ; m++ ) inx[m] = 0;
  for ( k = 0 ; k <= dim ; k++ )
     { inx[k] = l_order;
       new = lagrange_index(dim,l_order,inx);
       inx[k] = 0;
       web.skel[FACET].extreme[k] = new;
     }

  MFOR_ALL_FACETS(f_id)
   { vertex_id *v;
     dim = web.dimension;
     get_facet_verts(f_id,oldx,NULL);
     mat_mult(trans,oldx,newx,new_fctrl,web.skel[FACET].ctrlpts,SDIM);
     v = get_facet_vertices(f_id);

     /* take care of extreme vertices */
     for ( m = 0 ; m <= dim ; m++ ) inx[m] = 0;
     for ( k = 0 ; k <= dim ; k++ )
        { inx[k] = web.lagrange_order; 
          old = lagrange_index(dim,web.lagrange_order,inx); 
          inx[k] = 0;
          cornerv[k] = v[old];
          baryhash_find(1,&cornerv[k],&l_order,cornerv[k]);
        }

     /* create new interior */
     for ( m = 0 ; m <= dim ; m++ ) inx[m] = 0;
     inx[0] = l_order;
     for ( k = 0 ; k < new_fctrl ; k++ )
     { int fixed;
        vertex_id newv;
        conmap_t conmap[30];
        newv = baryhash_find(dim+1,cornerv,inx,NULLID);
        if ( newv == NULLID )
        { newv = new_vertex(newx[k],f_id);  
          set_attr(newv,Q_MIDFACET);
          set_vertex_facet(newv,f_id);  /* since doesn't have edge */
          /* set common attributes */
          for ( i = 0, fixed = 0; i <= dim ; i++ )
          { if ( inx[i] == 0 ) continue; /* doesn't contribute */
             if ( get_vattr(cornerv[i]) & FIXED ) fixed = 0;
             /* find centerpoint parameters for facet on boundary */
             if ( get_vattr(cornerv[i]) & BOUNDARY )    /* not working for torus */
                kb_error(1819,"Can't do simplex Lagrange with boundaries yet.\n",
                  RECOVERABLE);
             if ( get_vattr(cornerv[i]) & CONSTRAINT )    
             { get_v_common_conmap(newv,cornerv[i],conmap);
                set_v_conmap(newv,conmap);
             }
          }
          if ( fixed ) set_attr(newv,FIXED);
          if ( get_vattr(newv) & CONSTRAINT )
             project_v_constr(newv,ACTUAL_MOVE,RESET_ONESIDEDNESS);
          baryhash_find(dim+1,cornerv,inx,newv);
        }
        v[k] = newv;
        increment_lagrange_index(dim,inx);
     }
   }

  /* do edges */
  /* calculate transition matrix */
  dim = web.dimension-1;
  gauss_lagrange_setup(dim,l_order,web.gauss1D_order);

  for ( m = 0 ; m <= dim ; m++ ) inx[m] = 0;
  inx[0] = l_order;
  new = 0;
  do
  { old = 0;
     for ( m = 0 ; m <= dim ; m++ ) oldinx[m] = 0;
     oldinx[0] = web.lagrange_order;
     do
     { REAL prod;
        for ( k = 0, prod = 1.0 ; k <= dim ; k++ )
         for ( m = 0 ; m < oldinx[k] ; m++ )
          prod *= ((REAL)(inx[k])/l_order*web.lagrange_order - m)/(oldinx[k]-m); 
        trans[new][old] = prod;
        old++;
     } while ( increment_lagrange_index(dim,oldinx) );
     new++;
  } while ( increment_lagrange_index(dim,inx) );

  /* calc indices of extreme vertices; handy elsewhere */
  for ( m = 0 ; m <= dim ; m++ ) inx[m] = 0;
  for ( k = 0 ; k <= dim ; k++ )
     { inx[k] = l_order;
        new = lagrange_index(dim,l_order,inx);
        inx[k] = 0;
        web.skel[EDGE].extreme[k] = new;
     }

  MFOR_ALL_EDGES(e_id)
  { vertex_id *v;
     get_edge_verts(e_id,oldx,NULL);
     mat_mult(trans,oldx,newx,new_ectrl,old_ectrl,SDIM);
     v = get_edge_vertices(e_id);

     /* take care of extreme vertices */
     for ( m = 0 ; m <= dim ; m++ ) inx[m] = 0;
     for ( k = 0 ; k <= dim ; k++ )
        { inx[k] = web.lagrange_order; 
          old = lagrange_index(dim,web.lagrange_order,inx); 
          cornerv[k] = v[old];
          baryhash_find(1,&cornerv[k],&l_order,cornerv[k]);
        }

     /* create new interior */
     for ( m = 0 ; m <= dim ; m++ ) inx[m] = 0;
     inx[0] = l_order;
     for ( k = 0 ; k < new_ectrl ; k++ )
     { int fixed;
        vertex_id newv;
        newv = baryhash_find(dim+1,cornerv,inx,NULLID);
        if ( newv == NULLID )
        { conmap_t conmap[30];
          newv = new_vertex(newx[k],e_id);  
          set_attr(newv,Q_MIDEDGE);
          set_vertex_edge(newv,e_id); 
          /* set common attributes */
          for ( i = 0, fixed = 0; i <= dim ; i++ )
          { if ( inx[i] == 0 ) continue; /* doesn't contribute */
             if ( get_vattr(cornerv[i]) & FIXED ) fixed = 0;
             /* find centerpoint parameters for facet on boundary */
             if ( get_vattr(cornerv[i]) & BOUNDARY )    /* not working for torus */
                kb_error(1820,"Can't do simplex Lagrange with boundaries yet.\n",
                  RECOVERABLE);
             if ( get_vattr(cornerv[i]) & CONSTRAINT )    
             { get_v_common_conmap(newv,cornerv[i],conmap);
                set_v_conmap(newv,conmap);
             }
          }
          if ( fixed ) set_attr(newv,FIXED);
          if ( get_vattr(newv) & CONSTRAINT )
             project_v_constr(newv,ACTUAL_MOVE,RESET_ONESIDEDNESS);
          baryhash_find(dim+1,cornerv,inx,newv);
        }
        v[k] = newv;
        increment_lagrange_index(dim,inx);
     }
  }
    
  free_matrix(trans);
  free_matrix(newx);
  free_matrix(oldx);
  baryhash_end();

  if ( l_order < web.lagrange_order )
  { expand_attribute(EDGE,E_VERTICES_ATTR,&new_ectrl);
    expand_attribute(FACET,F_VERTICES_ATTR,&new_fctrl);
  } /* can now shrink  */

  /* delete all old non-extreme vertices */
  MFOR_ALL_VERTICES(v_id)  
  { if ( !(get_vattr(v_id) & NEWVERTEX)
                && (get_vattr(v_id) & (Q_MIDFACET|Q_MIDEDGE)) )
         free_element(v_id);
  }

  web.lagrange_order = l_order;
  web.skel[FACET].ctrlpts = new_fctrl;
  web.skel[EDGE].ctrlpts = edge_ctrl = new_ectrl;
  change_flag = 1;
  web_timestamp = top_timestamp = ++global_timestamp;
  web.headvnum = l_order;

  LEAVE_GRAPH_MUTEX;
}

/***********************************************************************
*
* function: lagrange_to_bezier()
*
* purpose: convert lagrange control points to bezier control points.
*/

void lagrange_to_bezier()
{
  int i,j;
  REAL *oldx[(MAXLAGRANGE+1)*(MAXLAGRANGE+2)/2];
  REAL **newx;
  edge_id e_id;
  facet_id f_id;
  vertex_id *v;

  if ( web.modeltype != LAGRANGE ) return;

  newx = dmatrix(0,web.skel[FACET].ctrlpts,0,SDIM-1);

  /* do facet interiors first, so can use unchanged edge vertices */ 
  if ( web.representation == SOAPFILM )
  MFOR_ALL_FACETS(f_id)
  { 
    v = get_facet_vertices(f_id);
    for ( i = 0 ; i <= web.skel[FACET].ctrlpts ; i++ )
      oldx[i] = get_coord(v[i]);
    mat_mult(bezier2invert[web.lagrange_order],oldx,newx,
		web.skel[FACET].ctrlpts,web.skel[FACET].ctrlpts,SDIM);
    for ( i = 1 ; i < web.skel[FACET].ctrlpts ; i++ ) 
    { if ( get_vattr(v[i]) & Q_MIDFACET )
      for ( j = 0 ; j < SDIM ; j++ )
        oldx[i][j] = newx[i][j];
    }
  }
 
  MFOR_ALL_EDGES(e_id)
  { 
    v = get_edge_vertices(e_id);
    for ( i = 0 ; i <= web.lagrange_order ; i++ )
      oldx[i] = get_coord(v[i]);
    mat_mult(bezier1invert[web.lagrange_order],oldx,newx,
		web.lagrange_order+1,web.lagrange_order+1,SDIM);
    for ( i = 1 ; i <  web.lagrange_order ; i++ ) /* no changing ends */
      for ( j = 0 ; j < SDIM ; j++ )
        oldx[i][j] = newx[i][j];
  }

  free_matrix(newx);
}

/***********************************************************************
*
* function: bezier_to_lagrange()
*
* purpose: convert bezier control points to lagrange control points.
*/

void bezier_to_lagrange()
{
  int i,j;
  REAL *oldx[(MAXLAGRANGE+1)*(MAXLAGRANGE+2)/2];
  REAL **newx;
  edge_id e_id;
  facet_id f_id;
  vertex_id *v;

  if ( web.modeltype != LAGRANGE ) return;

  newx = dmatrix(0,web.skel[FACET].ctrlpts,0,SDIM-1);
 
  /* do facet interior vertices first, so can use unchanged edge verts */
  if ( web.representation == SOAPFILM )
  MFOR_ALL_FACETS(f_id)
  { 
    v = get_facet_vertices(f_id);
    for ( i = 0 ; i <= web.skel[FACET].ctrlpts ; i++ )
      oldx[i] = get_coord(v[i]);
    mat_mult(bezier2revert[web.lagrange_order],oldx,newx,
		web.skel[FACET].ctrlpts,web.skel[FACET].ctrlpts,SDIM);
    for ( i = 1 ; i < web.skel[FACET].ctrlpts ; i++ ) 
    { if ( get_vattr(v[i]) & Q_MIDFACET )
      for ( j = 0 ; j < SDIM ; j++ )
        oldx[i][j] = newx[i][j];
    }
  }

  MFOR_ALL_EDGES(e_id)
  { 
    v = get_edge_vertices(e_id);
    for ( i = 0 ; i <= web.lagrange_order ; i++ )
      oldx[i] = get_coord(v[i]);
    mat_mult(bezier1revert[web.lagrange_order],oldx,newx,
		web.lagrange_order+1,web.lagrange_order+1,SDIM);
    for ( i = 1 ; i <  web.lagrange_order ; i++ ) /* no changing ends */
      for ( j = 0 ; j < SDIM ; j++ )
        oldx[i][j] = newx[i][j];
  }
 
  free_matrix(newx);
}

/************************************************************************
*
* Function: simplex_to_fe()
*
* Purpose: Convert from simplex model to facetedge model.
*          Only does 1D and 2D surfaces.
*/
struct vpair { vertex_id v[2];
               edge_id e;
} *vpair_list;
int vpair_count;
int vpaircomp(a,b)
struct vpair *a,*b;
{ if ( a->v[0] < b->v[0] ) return -1;
  if ( a->v[0] > b->v[0] ) return  1;
  if ( a->v[1] < b->v[1] ) return -1;
  if ( a->v[1] > b->v[1] ) return  1;
  return 0;
} 
void add_to_vpair_list(v1,v2)
vertex_id v1,v2;
{ if ( v1 < v2 ) 
  { vpair_list[vpair_count].v[0] = v1;
    vpair_list[vpair_count].v[1] = v2;
  }
  else
  { vpair_list[vpair_count].v[0] = v2;
    vpair_list[vpair_count].v[1] = v1;
  }
  vpair_count++;
}

facetedge_id vpair_fe(v1,v2,f_id)
vertex_id v1,v2;
facet_id f_id;
{ struct vpair key, *spot;
  facetedge_id fe,oldfe;
  if ( v1 < v2 ) { key.v[0] = v1; key.v[1] = v2; }
  else { key.v[0] = v2; key.v[1] = v1; }
  spot = bsearch(&key,vpair_list,vpair_count,sizeof(struct vpair),
      FCAST vpaircomp);
  if ( spot == NULL )
  { kb_error(6432,"simplex_to_fe: could not find edge in vpair_list.\n",
       RECOVERABLE);
  }
  fe = new_facetedge((v1 < v2) ? f_id : inverse_id(f_id),spot->e);
  oldfe = get_edge_fe(spot->e);
  if ( valid_id(oldfe) )
  { facetedge_id tempfe = get_next_facet(oldfe);
    set_next_facet(oldfe,fe);
    set_prev_facet(fe,oldfe);
    set_next_facet(fe,tempfe);
    set_prev_facet(tempfe,fe);
  }
  else
  { set_next_facet(fe,fe);
    set_prev_facet(fe,fe);
    set_edge_fe(spot->e,fe);
  }
  
  return (v1 < v2) ? fe : inverse_id(fe);
}

void simplex_to_fe()
{ int i,j;
  facet_id f_id;
  edge_id e_id;

  /* Check surface properties */
  if ( web.representation != SIMPLEX ) 
  { outstring("Surface is not in simplex representation.\n");
    return;
  }
  if ( web.dimension > 2 )
  { sprintf(errmsg,
     "simplex_to_fe: Surface is %d dimensional; must be 1 or 2.\n",
        web.dimension);
    kb_error(5366,errmsg,RECOVERABLE);
  }


  /* Create new edges that may not be represented */
  /* First, make list of all vertex pairs needed */
  vpair_list = (struct vpair *)mycalloc(3*web.skel[FACET].count,
        sizeof(struct vpair));
  vpair_count = 0;

  FOR_ALL_FACETS(f_id)
  { vertex_id *v = get_facet_vertices(f_id);
    add_to_vpair_list(v[0],v[1]);
    add_to_vpair_list(v[1],v[2]);
    add_to_vpair_list(v[2],v[0]);
  }
  /* Sort list and get unique edges */
  qsort(vpair_list,vpair_count,sizeof(struct vpair),FCAST vpaircomp);
  for ( i = 1, j = 0 ; i < vpair_count ; i++ )
    if ( vpaircomp(vpair_list+j,vpair_list+i) < 0 )
      vpair_list[++j] = vpair_list[i];
  vpair_count = j+1;

  /* Record existing edges */
  FOR_ALL_EDGES(e_id)
  { vertex_id headv = get_edge_headv(e_id);
    vertex_id tailv = get_edge_tailv(e_id);
    struct vpair keypair, *spot;
    if ( headv < tailv ) { keypair.v[0] = headv; keypair.v[1] = tailv; }
    else { keypair.v[0] = tailv; keypair.v[1] = headv; }
    spot = bsearch(&keypair,vpair_list,vpair_count,sizeof(struct vpair),
               FCAST vpaircomp );
    if ( spot )
       spot->e = (headv < tailv) ? inverse_id(e_id) : e_id;
  }
  /* Create needed new edges */
  for ( i = 0 ; i < vpair_count ; i++ )
    if ( !valid_id(vpair_list[i].e) )
      vpair_list[i].e = new_edge(vpair_list[i].v[0],vpair_list[i].v[1],NULLID);

  /* Create facet-edges */
  FOR_ALL_FACETS(f_id)
  { vertex_id *v = get_facet_vertices(f_id);
    facetedge_id fe1 = vpair_fe(v[0],v[1],f_id);
    facetedge_id fe2 = vpair_fe(v[1],v[2],f_id);
    facetedge_id fe3 = vpair_fe(v[2],v[0],f_id);
    set_next_edge(fe1,fe2);
    set_next_edge(fe2,fe3);
    set_next_edge(fe3,fe1);
    set_prev_edge(fe1,fe3);
    set_prev_edge(fe2,fe1);
    set_prev_edge(fe3,fe2);
    set_facet_fe(f_id,fe1);
  }

  /* flip representation */
  web.representation = (web.dimension==1) ? STRING : SOAPFILM;

  /* set links around edges */
  if ( web.representation == SOAPFILM )
    FOR_ALL_EDGES(e_id)
      fe_reorder(e_id);

} /* end simplex_to_fe */
  
