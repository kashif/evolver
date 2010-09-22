/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

#include "include.h"
  
/********************************************************************
*
*  Function: diffuse()
*
*  Purpose:  Diffuse mass through permeable surfaces.
*            Assumes string lengths or film areas
*            already calculated; also assumes pressures
*            known.
*/

/* for per-element diffusion constant */
#define DIFFUSE_EATTR_NAME "edge_diffusion"
#define DIFFUSE_FATTR_NAME "facet_diffusion"
int edge_diffusion_attr;
int facet_diffusion_attr;

void diffuse()
{
  edge_id e_id;
  facet_id f_id;
  facetedge_id fe_id;
  body_id b_id,bb_id;
  REAL pressure;
  REAL mass;
  REAL coeff;

  if ( web.representation == STRING )
  { edge_diffusion_attr = find_attribute(EDGE,DIFFUSE_EATTR_NAME);
    FOR_ALL_EDGES(e_id)
    { fe_id = get_edge_fe(e_id);
      if ( !valid_id(fe_id) ) return;
      b_id = get_facet_body(get_fe_facet(fe_id));
      if ( !valid_id(b_id) ) invert(fe_id);
      b_id = get_facet_body(get_fe_facet(fe_id));
      if ( !valid_id(b_id) ) continue;
      pressure = get_body_pressure(b_id);
       
      if ( !equal_id(fe_id,get_next_facet(fe_id)) )
      { fe_id = get_next_facet(fe_id);
        bb_id = get_facet_body(get_fe_facet(fe_id));
        if ( !valid_id(bb_id) ) invert(fe_id);
        bb_id = get_facet_body(get_fe_facet(fe_id));
        pressure -= get_body_pressure(bb_id);
      }
      else bb_id = NULLBODY;
      if ( edge_diffusion_attr >= 0 )
         coeff = *(REAL*)get_extra(e_id,edge_diffusion_attr);
      else coeff = web.diffusion_const; 
      mass = web.scale*coeff*pressure*get_edge_length(e_id);
      set_body_fixvol(b_id,get_body_fixvol(b_id)-mass);
      if ( valid_id(bb_id) )
        set_body_fixvol(bb_id,get_body_fixvol(bb_id)+mass);
    }
  } /* end STRING */
  else
  { facet_diffusion_attr = find_attribute(FACET,DIFFUSE_FATTR_NAME);
    FOR_ALL_FACETS(f_id)
    { pressure = 0.0;
      b_id = get_facet_body(f_id);
      if ( valid_id(b_id) ) 
       pressure = get_body_pressure(b_id);
      else pressure = web.pressure;
          
      bb_id = get_facet_body(inverse_id(f_id));
      if ( valid_id(bb_id) )
          pressure -= get_body_pressure(bb_id);
      else pressure -= web.pressure;
      if ( facet_diffusion_attr >= 0 )
         coeff = *(REAL*)get_extra(f_id,facet_diffusion_attr);
      else coeff = web.diffusion_const; 
          
      mass = web.scale*coeff*pressure*get_facet_area(f_id);
      if ( valid_id(b_id) )
         set_body_fixvol(b_id,get_body_fixvol(b_id)-mass);
      if ( valid_id(bb_id) )
         set_body_fixvol(bb_id,get_body_fixvol(bb_id)+mass);
    }
  }
} /* end diffuse() */
