/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/******************************************************************
*
*  File: zoom.c
*
*  Contents:  Routines to zoom in on vertex.
*
*/

#include "include.h"

/******************************************************************
*
*  Function: zoom_vertex()
*
*  Purpose:  Zoom in on a vertex by chopping off everything beyond
*            a given distance and fixing in place the jagged edges.
*/

void zoom_vertex(v_id,radius)
vertex_id v_id;  /* vertex to zoom on */
REAL radius;      /* cutoff distance from v_id */
{
  vertex_id vv_id;
  edge_id e_id;
  facet_id f_id;
  facetedge_id fe_id,next,prev;
  int i; 

  if ( (web.bodycount != 0) && (web.representation == SOAPFILM) )
     kb_error(1540,"Zoom is not implemented for bodies.\n",RECOVERABLE);

  if ( radius <= 0.0 )
     kb_error(1541,"Must have positive cut-off radius.\n",RECOVERABLE);

  if ( !valid_id(v_id) || !valid_element(v_id) )
     { sprintf(errmsg,"Vertex %s is not valid.\n",ELNAME(v_id));
       kb_error(1542,errmsg,RECOVERABLE);
     }

  /* eliminate all vertices beyond cutoff */
  FOR_ALL_VERTICES(vv_id)
     { 
        if ( distance(v_id,vv_id) > radius )
          set_attr(vv_id,DISSOLVED);
     }

  /* eliminate all edges connected to gone vertices */
  FOR_ALL_EDGES(e_id)
     { int val_head,val_tail;
        
        val_tail = !(get_vattr(get_edge_tailv(e_id)) & DISSOLVED);
        val_head = !(get_vattr(get_edge_headv(e_id)) & DISSOLVED);
        if ( !val_head || !val_tail )
          { if (val_head) 
                { set_attr(get_edge_headv(e_id),FIXED);
                }
            if (val_tail) 
                { set_attr(get_edge_tailv(e_id),FIXED);
                }
             set_attr(e_id,DISSOLVED);
          }
     }

  /* eliminate all facets verging on gone edges */
  FOR_ALL_FACETS(f_id)
     { 
        fe_id = get_facet_fe(f_id);
        for ( i = 0 ; i < FACET_EDGES ; i++ )
          { if ( get_eattr(get_fe_edge(fe_id)) & DISSOLVED )
                { set_attr(f_id,DISSOLVED);
                  break;
                }
             fe_id = get_next_edge(fe_id);
          }
     }
     
  /* eliminate all facet-edges on gone facets */
  FOR_ALL_FACETEDGES(fe_id)
     { 
        e_id = get_fe_edge(fe_id);
        if ( get_eattr(e_id) & DISSOLVED ) 
         { set_attr(fe_id,DISSOLVED); continue; }
        f_id = get_fe_facet(fe_id);
        if ( valid_id(f_id) && (get_fattr(f_id) & DISSOLVED) )
          {    /* have kept edge on removed facet */
                 /* patch up ragged edges */
                 next = get_next_facet(fe_id);
                 prev = get_prev_facet(fe_id);
                 if ( equal_id(next,fe_id) )
                  { /* was only facet on edge */
                     set_edge_fe(e_id,NULLFACETEDGE);
                  }
                 else
                  { /* close ranks */
                     set_next_facet(prev,next);
                     set_prev_facet(next,prev);
                     set_edge_fe(e_id,next);
                  }
            
                 if ( web.representation == SOAPFILM)
                  {
                     /* fix edge in place */
                     set_attr(e_id,FIXED);
                     set_attr(get_edge_tailv(e_id),FIXED);
                     set_attr(get_edge_headv(e_id),FIXED);
                  }
                 set_attr(fe_id,DISSOLVED);
          }
     }
     FOR_ALL_VERTICES(v_id)
        if ( get_vattr(v_id) & DISSOLVED ) free_element(v_id);
     FOR_ALL_EDGES(e_id)
        if ( get_eattr(e_id) & DISSOLVED ) free_element(e_id);
     FOR_ALL_FACETS(f_id)
        if ( get_fattr(f_id) & DISSOLVED ) free_element(f_id);
     FOR_ALL_FACETEDGES(fe_id)
        if ( get_attr(fe_id) & DISSOLVED ) free_element(fe_id);

}


