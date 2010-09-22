/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*********************************************************************
*
*     file:        hidim.c
*
*     Contents:  Functions calculating energy and its
*                    gradients for the LINEAR SOAPFILM model
*                    in arbitrary ambient dimension.
*                    Plain area only; no wulff vectors or surface energy
*                    or gravity.
*/

#include "include.h"

/************************************************************************
*
*  Calculates all forces on control points due to facet and
*  accumulates them at each control point.
*/

void facet_force_l_hi_d(f_id)
facet_id f_id;
{
  REAL side[FACET_EDGES][MAXCOORD];
  int i,j,k;
  REAL area;
  facetedge_id fe_id;
  REAL *x[FACET_VERTS];
  REAL *force[FACET_VERTS];
  REAL wee_area = 0.0000000001*web.min_area;
  REAL factor,s1s1,s1s2,s2s2;
  vertex_id v_id[MAXCOORD+1];
  edge_id    e_id[FACET_EDGES];

  /* get side vectors */
  fe_id = get_facet_fe(f_id);
  for ( i = 0 ; i < FACET_EDGES ; i++, fe_id = get_next_edge(fe_id) )
     { v_id[i] = get_fe_tailv(fe_id);
        e_id[i] = get_fe_edge(fe_id);
        x[i] = get_coord(v_id[i]);
        force[i] = get_force(v_id[i]);
     }

  /* calculate sides and product matrix */ 
  for ( i = 0 ; i < FACET_EDGES ; i++ )
     { int ii = (i+1)%FACET_EDGES;
        for ( j = 0 ; j < SDIM ; j++ )
          side[i][j] = x[ii][j] - x[i][j];
     } 
  s1s1 = SDIM_dot(side[0],side[0]);
  s1s2 = SDIM_dot(side[0],side[1]);
  s2s2 = SDIM_dot(side[1],side[1]);
  area = sqrt(s1s1*s2s2 - s1s2*s1s2)/2;

  /* an error check, and accommodation for possibly deliberately
      degenerate triangles on boundary */
  if ( area < wee_area )
     { facetedge_id ffe;
          
        ffe = fe_id;
        outstring("WARNING! Zero area triangle!\n");
        outstring("Facet-edges and sides: \n");
        for ( i = 0 ; i < FACET_EDGES ; i++, ffe = get_next_edge(ffe) )
        { 
          sprintf(msg," %8lX    %18.15f %18.15f %18.15f\n",(unsigned long)ffe,
                  (DOUBLE)side[i][0],(DOUBLE)side[i][1],(DOUBLE)side[i][2]);
          outstring(msg);
        }

        prompt("Hit RETURN to continue.",msg,msgmax);
     }

  set_facet_area(f_id,area);  /* half of parallelogram magnitude */
  if ( get_fattr(f_id) & DENSITY )
     factor = get_facet_density(f_id)/area/4;
  else factor = 1/area/4;

  if ( ((square_curvature_flag | mean_curv_int_flag) & EVALUATE)
            && !kusner_flag && !conf_edge_curv_flag ) 
     sqcurve_force(v_id,e_id,side);

  /* accumulate star area around each vertex and edge */
     {
        fe_id = get_facet_fe(f_id);
        for ( i = 0 ; i < FACET_EDGES ; i++, fe_id = get_next_edge(fe_id) )
         {
            edge_id ee_id = get_fe_edge(fe_id);
            vertex_id vv_id = get_edge_headv(ee_id);
            add_vertex_star(vv_id,area);
            add_edge_star(ee_id,area);
          }
     }

  /* force on each vertex */

  for ( k = 0 ; k < SDIM ; k++ )
     { REAL temp1,temp2;
        temp1 = (side[0][k]*s2s2 - side[1][k]*s1s2)*factor;
        temp2 = (side[1][k]*s1s1 - side[0][k]*s1s2)*factor;
        force[0][k] += temp1;
        force[1][k] -= temp1 - temp2;
        force[2][k] -= temp2;
     }
}


/*********************************************************************
*
*  Function: facet_energy_l_hi_d()
*
*  Purpose:  Calculates energy due to facet for LINEAR SOAPFILM.
*                For arbitrary ambient dimension.
*/

void facet_energy_l_hi_d(f_id)
facet_id f_id;
{
  REAL side[FACET_EDGES][MAXCOORD];
  int i,j;
  REAL energy;
  facetedge_id fe_id;
  REAL *x[FACET_VERTS];
  REAL s1s1,s1s2,s2s2;
  vertex_id v_id[MAXCOORD+1];

  /* get side vectors */
  fe_id = get_facet_fe(f_id);
  for ( i = 0 ; i < FACET_EDGES ; i++, fe_id = get_next_edge(fe_id) )
     { v_id[i] = get_fe_tailv(fe_id);
       x[i] = get_coord(v_id[i]);
     }

  /* calculate sides and product matrix */ 
  for ( i = 0 ; i < FACET_EDGES ; i++ )
     { int ii = (i+1)%FACET_EDGES;
       for ( j = 0 ; j < SDIM ; j++ )
          side[i][j] = x[ii][j] - x[i][j];
     } 
  s1s1 = SDIM_dot(side[0],side[0]);
  s1s2 = SDIM_dot(side[0],side[1]);
  s2s2 = SDIM_dot(side[1],side[1]);
  energy = sqrt(s1s1*s2s2 - s1s2*s1s2)/2;  /* half of parallelogram */
  web.total_area    += energy;
  set_facet_area(f_id,energy);  

  if ( get_fattr(f_id) & DENSITY )
         energy *= get_facet_density(f_id);

  web.total_energy    += energy;

  /* do square curvature if wanted */
  if ( ((square_curvature_flag | mean_curv_int_flag) & EVALUATE )
            && !kusner_flag && !conf_edge_curv_flag )
     sqcurve_energy(v_id,side);

      
  /* accumulate 1/3 area around each vertex to scale motion */
  if ( web.area_norm_flag )
     {
        fe_id = get_facet_fe(f_id);
        for ( i = 0 ; i < FACET_EDGES ; i++, fe_id = get_next_edge(fe_id) )
         {
            vertex_id vv_id = get_fe_headv(fe_id);
            add_vertex_star(vv_id,energy);
          }
     }
    
}
 
