/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/******************************************************************
*
*  File:  quantity.c
*
*  Purpose:  General handling of general quantities.
*/

#include "include.h"

struct gen_quant *Gen_quants;
struct method_instance *Meth_inst;
int compound_quant_list_head = -1;

/* new system of general quantity allocation */
struct gen_quant **gen_quant_list;
struct gen_quant *gen_quant_free;
int gen_quant_list_max;
int gen_quant_free_left;

/* new system of general instance allocation */
struct method_instance **meth_inst_list;
int meth_inst_list_max;
struct method_instance *meth_inst_free;
int meth_inst_free_left;

/* element setup routines */
void (*q_setup[NUMELEMENTS]) ARGS((struct linsys *,QINFO,int)) =
 { q_vertex_setup,q_edge_setup,q_facet_setup,q_body_setup,q_facetedge_setup};

struct gen_quant_method basic_gen_methods[] =
{
  {"null_length",EDGE,NEED_SIDE,NOSPEC,NULL, null_length_value, null_length_grad, 
    null_length_hess,NULL},
  {"null_area",FACET,NEED_SIDE,NOSPEC,NULL, null_area_value, null_area_grad, 
    null_area_hess,NULL},
   
  {"edge_tension",EDGE,NEED_SIDE,NOSPEC,q_edge_tension_init,
    q_edge_tension_value, q_edge_tension_gradient, q_edge_tension_hessian,NULL},
  {"edge_length",EDGE,NEED_SIDE,NOSPEC,q_edge_tension_init,
    q_edge_tension_value, q_edge_tension_gradient, q_edge_tension_hessian,NULL},
  {"density_edge_length",EDGE,NEED_SIDE,SPEC_USE_DENSITY,q_edge_tension_init,
    q_edge_tension_value, q_edge_tension_gradient, q_edge_tension_hessian,NULL},
  {"facet_tension",FACET,NEED_SIDE,NOSPEC, q_facet_tension_init,
    q_facet_tension_value, q_facet_tension_gradient, q_facet_tension_hessian,NULL},
  {"facet_area",FACET,NEED_SIDE,NOSPEC, q_facet_tension_init,
      q_facet_tension_value, q_facet_tension_gradient, q_facet_tension_hessian,NULL},
  {"density_facet_area",FACET,NEED_SIDE,SPEC_USE_DENSITY, q_facet_tension_init,
      q_facet_tension_value, q_facet_tension_gradient, q_facet_tension_hessian,NULL},
  {"facet_area_u",FACET,NEED_SIDE,NOSPEC, q_facet_tension_u_init,
      q_facet_tension_u_value, q_facet_tension_u_gradient, 
      q_facet_tension_u_hessian,NULL},
  {"density_facet_area_u",FACET,NEED_SIDE,SPEC_USE_DENSITY,
      q_facet_tension_u_init, q_facet_tension_u_value, 
      q_facet_tension_u_gradient, q_facet_tension_u_hessian,NULL},
  {"pos_area_hess",FACET,NEED_SIDE,NOSPEC, pos_area_hess_init,
      q_facet_tension_value, q_facet_tension_gradient, pos_area_hess,NULL},
  {"sobolev_area",FACET,NEED_SIDE,NOSPEC, sobolev_area_init,
      q_facet_tension_value, q_facet_tension_gradient, sobolev_area_hess,NULL},
  {"dirichlet_area",FACET,NEED_SIDE,NOSPEC, dirichlet_area_init,
      q_facet_tension_value, q_facet_tension_gradient, dirichlet_area_hess,NULL},
  {"wulff_energy",FACET,0,NOSPEC, wulff_method_init,
      facet_wulff_value, facet_wulff_grad, null_q_hess,NULL},
  {"area_square",FACET,NEED_SIDE,NOSPEC, NULL,
      area_square_value, area_square_gradient, null_q_hess,NULL},
  {"vertex_scalar_integral", VERTEX,ORIENTABLE_METHOD,SPEC_SCALAR, NULL,
      vertex_scalar_integral,vertex_scalar_integral_grad,
      vertex_scalar_integral_hess,NULL},
  {"edge_scalar_integral",EDGE,NEED_SIDE|NEED_GAUSS,SPEC_SCALAR, NULL,
      edge_scalar_integral,edge_scalar_integral_grad,edge_scalar_integral_hess,NULL},
  {"edge_vector_integral",EDGE,NEED_SIDE|NEED_GAUSS|ORIENTABLE_METHOD,
      SPEC_VECTOR, NULL,
      edge_vector_integral,edge_vector_integral_grad,edge_vector_integral_hess,NULL},
  {"edge_general_integral",EDGE,NEED_SIDE|NEED_GAUSS,
        SPEC_SCALAR|SPEC_EXTRADIM, edge_general_init, edge_general_value,
         edge_general_grad,edge_general_hess,NULL},
  {"edge_area",EDGE,ORIENTABLE_METHOD,NOSPEC, NULL, 
      q_edge_area,q_edge_area_grad,q_edge_area_hess,NULL},
  {"edge_torus_area",EDGE,ORIENTABLE_METHOD,NOSPEC, NULL, 
      q_edge_torus_area,q_edge_torus_area_grad,q_edge_torus_area_hess,NULL},
  {"circular_arc_length",EDGE,NEED_SIDE,NOSPEC, circular_arc_length_init, 
      circular_arc_length_value,circular_arc_length_grad,circular_arc_length_hess,NULL},
  {"circular_arc_area",EDGE,ORIENTABLE_METHOD|NEED_SIDE,NOSPEC, circular_arc_area_init, 
      circular_arc_area_value,circular_arc_area_grad,circular_arc_area_hess,NULL},
  {"string_gravity",EDGE,NEED_SIDE|NEED_GAUSS|ORIENTABLE_METHOD,
      NOSPEC,string_gravity_init,
      string_gravity_energy,string_gravity_grads,string_gravity_hessian,NULL},
  {"gap_energy",EDGE,NEED_SIDE,NOSPEC,NULL,gap_energy,gap_grads,null_q_hess,NULL},
  {"klein_length",EDGE,0,NOSPEC,NULL,klein_length_method,
            klein_length_method_grad,null_q_hess,NULL},
  {"klein_area",FACET,0,NOSPEC,NULL,klein_area_method,
            klein_area_method_grad,null_q_hess,NULL},
  {"facet_scalar_integral",FACET,NEED_SIDE|NEED_GAUSS,SPEC_SCALAR, 
      facet_scalar_integral_init, facet_scalar_integral,
      facet_scalar_integral_grad,facet_scalar_integral_hess,NULL},
  {"facet_vector_integral",FACET,NEED_SIDE|NEED_NORMAL|NEED_GAUSS
      |ORIENTABLE_METHOD,
      SPEC_VECTOR, facet_vector_integral_init, facet_vector_integral, 
      facet_vector_integral_grad, facet_vector_integral_hess,NULL},
  {"facet_2form_integral",FACET,NEED_SIDE|NEED_GAUSS|ORIENTABLE_METHOD,
      SPEC_2FORM, facet_2form_integral_init, 
      facet_2form_integral,facet_2form_integral_grad,facet_2form_integral_hess,NULL},
  {"facet_2form_sq_integral",FACET,NEED_SIDE|NEED_GAUSS,
      SPEC_2FORM, facet_2form_sq_integral_init, 
      facet_2form_sq_integral,facet_2form_sq_integral_grad,null_q_hess,NULL},
  {"facet_general_integral",FACET,NEED_SIDE|NEED_NORMAL|NEED_GAUSS,
      SPEC_SCALAR|SPEC_EXTRADIM, facet_general_init, facet_general_value, 
      facet_general_grad, facet_general_hess,NULL},
  {"metric_facet_area",FACET,NEED_GAUSS|NEED_SIDE,NOSPEC, metric_area_init, 
      metric_area_value, metric_area_grad, metric_area_hess,NULL},
  {"metric_edge_length",EDGE,NEED_GAUSS,NOSPEC, metric_area_init, metric_area_value, 
      metric_area_grad, metric_area_hess,NULL},
  {"spherical_area",FACET,NEED_SIDE,NOSPEC, 
      NULL, spherical_area_value,spherical_area_grad,NULL,NULL},
  {"spherical_arc_length",EDGE,NEED_SIDE,NOSPEC, 
      NULL, spherical_arc_length_value,spherical_arc_length_grad,spherical_arc_length_hess,NULL},
  {"spherical_arc_area_n",EDGE,NEED_SIDE|ORIENTABLE_METHOD,NOSPEC, 
     spherical_arc_area_init, spherical_arc_area_n_value,
     spherical_arc_area_n_grad,spherical_arc_area_n_hess,NULL},
  {"spherical_arc_area_s",EDGE,NEED_SIDE|ORIENTABLE_METHOD,NOSPEC, 
     spherical_arc_area_init, spherical_arc_area_s_value,
     spherical_arc_area_s_grad,spherical_arc_area_s_hess,NULL},
  {"facet_volume",FACET,NEED_SIDE|TORUS_MODULO_MUNGE|ORIENTABLE_METHOD,NOSPEC, 
      q_facet_volume_init, q_facet_volume,q_facet_volume_grad,q_facet_volume_hess,NULL},
  {"facet_torus_volume",FACET,NEED_SIDE|TORUS_MODULO_MUNGE|ORIENTABLE_METHOD,
      NOSPEC, NULL, 
      q_facet_torus_volume,q_facet_torus_volume_grad,q_facet_torus_volume_hess,NULL},
  {"simplex_vector_integral",FACET,NEED_SIDE|NEED_GAUSS|ORIENTABLE_METHOD,
      SPEC_VECTOR, simplex_vector_integral_init, simplex_vector_integral, 
      simplex_vector_integral_grad, simplex_vector_integral_hess,NULL},
  {"simplex_k_vector_integral",FACET,NEED_SIDE|NEED_GAUSS|ORIENTABLE_METHOD,
      SPEC_KVECTOR, simplex_k_vector_integral_init, simplex_k_vector_integral, 
      simplex_k_vector_integral_grad, simplex_k_vector_integral_hess,NULL},
  {"edge_k_vector_integral",EDGE,NEED_SIDE|NEED_GAUSS|ORIENTABLE_METHOD,
      SPEC_KVECTOR, simplex_k_vector_integral_init, simplex_k_vector_integral, 
      simplex_k_vector_integral_grad, simplex_k_vector_integral_hess,NULL},
  {"stress_integral",FACET,NEED_SIDE|NEED_NORMAL,SPEC_SCALAR,
      stress_integral_init,stress_integral, stress_integral_grad, null_q_hess,NULL},
  {"gravity_method",FACET,NEED_SIDE|ORIENTABLE_METHOD,NOSPEC,
      gravity_init,gravity_energy, gravity_grads,gravity_hessian,NULL},
  {"full_gravity_method",FACET,NEED_SIDE|ORIENTABLE_METHOD,NOSPEC,
      full_gravity_init,gravity_energy, gravity_grads,gravity_hessian,NULL},
  {"sqcurve_string",VERTEX,NEED_WINGS,NOSPEC,sqcurve_string_init,
      sqcurve_string_value, sqcurve_string_grad,sqcurve_string_hess,NULL},
  {"sqcurve_string_marked",VERTEX,NEED_MARKED_WINGS,NOSPEC,sqcurve_string_marked_init,
      sqcurve_string_value, sqcurve_string_grad,sqcurve_string_hess,NULL},
  {"sqcurve2_string",VERTEX,NEED_WINGS,NOSPEC,sqcurve2_string_init,
      sqcurve2_string_value, sqcurve2_string_grad,NULL,NULL},
  {"sqcurve3_string",VERTEX,NEED_WINGS,NOSPEC,sqcurve3_string_init,
      sqcurve3_string_value, sqcurve3_string_grad,sqcurve3_string_hess,NULL},
  {"sq_mean_curv_cyl",VERTEX,NEED_WINGS,NOSPEC,sq_mean_curv_cyl_init,
      sq_mean_curv_cyl_value, sq_mean_curv_cyl_grad,sq_mean_curv_cyl_hess,NULL},
  {"sq_gaussian_curv_cyl",VERTEX,NEED_WINGS,NOSPEC,sq_gauss_curv_cyl_init,
      sq_gauss_curv_cyl_value, sq_gauss_curv_cyl_grad,sq_gauss_curv_cyl_hess,NULL},
  {"mean_curvature_integral",EDGE,NEED_SIDE|NEED_WINGS,NOSPEC,mean_int_init,
      mean_int_value, mean_int_gradient,mean_int_hessian,NULL},
  {"mean_curvature_integral_A",EDGE,NEED_SIDE|NEED_WINGS,NOSPEC,mean_int_a_init,
      mean_int_a_value, mean_int_a_gradient,mean_int_a_hessian,NULL},
  {"gauss_curvature_integral",FACET,0,NOSPEC,gauss_integral_init,
      gauss_int_energy, gauss_int_gradient, null_q_hess ,NULL},
  {"levine_energy",VERTEX,NEED_FULL_STAR,NOSPEC,levine_energy_init,
      levine_energy_value, levine_energy_grad, null_q_hess ,NULL},
/*
  {"sq_gauss_curvature",VERTEX,0,NOSPEC,sqgauss_method_init,
      sqgauss_method_value, sqgauss_method_grad, null_q_hess ,NULL},
*/
  {"star_gauss_curvature",VERTEX,NEED_PART_STAR,NOSPEC,star_gauss_method_init,
      star_gauss_method_value, star_gauss_method_grad, star_gauss_method_hess ,NULL},
  {"sq_gauss_curvature",VERTEX,NEED_FULL_STAR,NOSPEC,star_sqgauss_method_init,
      star_sqgauss_method_value, star_sqgauss_method_grad, 
      star_sqgauss_method_hess ,NULL},
  {"sq_mean_curvature",VERTEX,0,NOSPEC,sqcurve_method_init,
      sqcurve_method_value, sqcurve_method_grad, null_q_hess,
      sqcurve_method_cleanup},
  {"eff_area_sq_mean_curvature",VERTEX,0,NOSPEC,sqcurve_method_init,
      sqcurve_method_value, sqcurve_method_grad, null_q_hess,
      sqcurve_method_cleanup },
  {"normal_sq_mean_curvature",VERTEX,0,NOSPEC,sqcurve_method_init,
      sqcurve_method_value, sqcurve_method_grad, null_q_hess,
      sqcurve_method_cleanup },
  {"star_sq_mean_curvature",VERTEX,NEED_PART_STAR,NOSPEC,star_sqcurve_method_init,
      star_sqcurve_method_value, star_sqcurve_method_grad, star_sqcurve_method_hess ,NULL},
  {"star_eff_area_sq_mean_curvature",VERTEX,NEED_PART_STAR,NOSPEC,
      star_sqcurve_method_init, star_sqcurve_method_value, 
              star_sqcurve_method_grad, star_sqcurve_method_hess ,NULL},
  {"star_normal_sq_mean_curvature",VERTEX,NEED_PART_STAR,NOSPEC,
      star_sqcurve_method_init, star_sqcurve_method_value, 
      star_sqcurve_method_grad, star_sqcurve_method_hess ,NULL},
  {"star_perp_sq_mean_curvature",VERTEX,NEED_PART_STAR,NOSPEC,
      star_sqcurve_method_init, star_sqcurve_method_value, 
      star_sqcurve_method_grad, star_sqcurve_method_hess ,NULL},
  {"circle_willmore",EDGE,NEED_WINGS|NEED_SIDE,NOSPEC,
      circle_willmore_init, circle_willmore_value, 
      circle_willmore_grad, circle_willmore_hess ,NULL},
  {"laplacian_mean_curvature",VERTEX,NEED_PART_STAR,NOSPEC,
      laplacian_mean_curvature_init, laplacian_mean_curvature_value,
      laplacian_mean_curvature_grad, NULL, NULL },
  {"stokes2d",VERTEX,NEED_PART_STAR,NOSPEC, stokes2d_init, 
    stokes2d_value,   stokes2d_grad, stokes2d_hess ,NULL},
  {"stokes2d_laplacian",VERTEX,NEED_PART_STAR,NOSPEC, stokes2d_init, 
    stokes2d_laplacian,   NULL, NULL ,NULL},
  {"hooke_energy",EDGE,NEED_SIDE,NOSPEC,hooke_energy_init,hooke_energy,
      hooke_energy_gradient,hooke_energy_hessian,NULL},
  {"hooke2_energy",EDGE,NEED_SIDE,NOSPEC,hooke2_energy_init,hooke2_energy,
      hooke2_energy_gradient,hooke2_energy_hessian,NULL},
  {"hooke3_energy",EDGE,NEED_SIDE,NOSPEC,hooke3_energy_init,hooke3_energy,
      hooke3_energy_gradient,hooke3_energy_hessian,NULL},
  {"local_hooke_energy",VERTEX,0,NOSPEC,local_hooke_init,
      local_hooke, local_hooke_gradient,null_q_hess,NULL},
  {"dihedral_hooke",EDGE,NEED_WINGS,NOSPEC,NULL,
      dihedral_hooke_energy, dihedral_hooke_grad, dihedral_hooke_hess ,NULL},
  {"linear_elastic",FACET,NEED_SIDE,NOSPEC,linear_elastic_init,
      linear_elastic_energy, linear_elastic_gradient,linear_elastic_hessian,NULL},
  {"general_linear_elastic",FACET,NEED_SIDE,NOSPEC,general_linear_elastic_init,
      general_linear_elastic_energy, general_linear_elastic_gradient,
      general_linear_elastic_hessian,NULL},
  {"linear_elastic_B",FACET,NEED_SIDE,NOSPEC,linear_elastic_B_init,
      linear_elastic_B_energy, linear_elastic_B_gradient,linear_elastic_B_hessian,NULL},
  {"relaxed_elastic",FACET,NEED_SIDE,NOSPEC,relaxed_elastic_init,
      relaxed_elastic_energy, relaxed_elastic_gradient,relaxed_elastic_hessian,NULL},
  {"relaxed_elastic1",FACET,NEED_SIDE,NOSPEC,relaxed_elastic_init,
      relaxed_elastic1_energy, relaxed_elastic1_gradient,relaxed_elastic1_hessian,NULL},
  {"relaxed_elastic2",FACET,NEED_SIDE,NOSPEC,relaxed_elastic_init,
      relaxed_elastic2_energy, relaxed_elastic2_gradient,relaxed_elastic2_hessian,NULL},
  {"relaxed_elastic_A",FACET,NEED_SIDE,NOSPEC,relaxed_elastic_A_init,
      relaxed_elastic_A_energy, relaxed_elastic_A_gradient,relaxed_elastic_A_hessian,NULL},
  {"relaxed_elastic1_A",FACET,NEED_SIDE,NOSPEC,relaxed_elastic_A_init,
      relaxed_elastic1_A_energy, relaxed_elastic1_A_gradient,relaxed_elastic1_A_hessian,NULL},
  {"relaxed_elastic2_A",FACET,NEED_SIDE,NOSPEC,relaxed_elastic_A_init,
      relaxed_elastic2_A_energy, relaxed_elastic2_A_gradient,relaxed_elastic2_A_hessian,NULL},
  {"dirichlet_elastic",FACET,NEED_SIDE,NOSPEC,dirichlet_elastic_init,
      dirichlet_elastic_energy, dirichlet_elastic_gradient,
      dirichlet_elastic_hessian,NULL},
  {"SVK_elastic",FACET,NEED_SIDE,NOSPEC,SVK_init,
      SVK_energy, SVK_gradient,SVK_hessian,NULL},
  {"neo_hookean",FACET,NEED_SIDE,NOSPEC,Neo_Hookean_init,
      Neo_Hookean_energy, Neo_Hookean_gradient,Neo_Hookean_hessian,NULL},
  {"knot_energy",VERTEX,0,NOSPEC,knot_power_init,knot_energy,
      knot_energy_gradient,knot_energy_hessian,NULL},
  {"charge_gradient",VERTEX,0,NOSPEC,charge_gradient_init,charge_gradient,
      charge_gradient_gradient,null_q_hess,NULL},
  {"uniform_knot_energy",VERTEX,0,NOSPEC,uniform_knot_energy_init,
      uniform_knot_energy, uniform_knot_energy_gradient,null_q_hess,NULL},
  {"uniform_knot_energy_normalizer",VERTEX,0,NOSPEC,knot_power_init,
      uniform_normalization, null_q_grad, null_q_hess,NULL},
  {"uniform_knot_normalizer1",VERTEX,0,NOSPEC,knot_power_init,
      uniform_normalization, null_q_grad, null_q_hess,NULL},
  {"uniform_knot_normalizer2",VERTEX,0,NOSPEC,knot_power_init,
      uniform_binormalization, null_q_grad, null_q_hess,NULL},
  {"edge_knot_energy_normalizer",EDGE,0,NOSPEC,knot_power_init,
      edge_normalization, null_q_grad, null_q_hess,NULL},
  {"simon_knot_energy_normalizer",EDGE,0,NOSPEC,knot_power_init,
      simon_normalization, null_q_grad, null_q_hess,NULL},
  {"edge_min_knot_energy",EDGE,0,NOSPEC,knot_power_init,
      edge_min_knot_energy, null_q_grad, null_q_hess,NULL},
  {"edge_knot_energy",EDGE,0,NOSPEC,knot_power_init,
      edge_edge_knot_energy, edge_edge_knot_energy_gradient,null_q_hess,NULL},
  {"edge_edge_knot_energy",EDGE,0,NOSPEC,knot_power_init,
      edge_edge_knot_energy, edge_edge_knot_energy_gradient,null_q_hess,NULL},
  {"facet_knot_energy",VERTEX,0,NOSPEC,facet_knot_energy_init,
      facet_knot_energy, facet_knot_energy_gradient,null_q_hess,NULL},
  {"facet_knot_energy_fix",VERTEX,0,NOSPEC,facet_knot_energy_fix_init,
      facet_knot_energy_fix, facet_knot_energy_fix_gradient,null_q_hess,NULL},
  {"bi_surface",VERTEX,0,SPEC_SCALAR,bi_surface_init,
      bi_surface_energy, bi_surface_gradient,null_q_hess,NULL},
  {"buck_knot_energy",EDGE,0,NOSPEC,knot_power_init,
      buck_knot_energy, buck_knot_energy_gradient,null_q_hess,NULL},
  {"proj_knot_energy",EDGE,0,NOSPEC,knot_power_init,
      proj_knot_energy, proj_knot_energy_gradient,null_q_hess,NULL},
  {"sin_knot_energy",EDGE,0,NOSPEC,NULL,
      sin_knot_energy, sin_knot_energy_gradient,null_q_hess,NULL},
  {"circle_knot_energy",EDGE,0,NOSPEC,NULL,
      circle_knot_energy, circle_knot_energy_gradient,null_q_hess,NULL},
  {"twist",EDGE,0,NOSPEC,NULL, twist, null_q_grad, null_q_hess,NULL},
  {"writhe",EDGE,0,NOSPEC,NULL, writhe, writhe_gradient,null_q_hess,NULL},
  {"average_crossings",EDGE,0,NOSPEC, NULL,
      average_crossing,null_q_grad,null_q_hess,NULL},
  {"sphere_knot_energy",FACET,0,NOSPEC,sphere_knot_energy_init,
      sphere_knot_energy, sphere_knot_energy_gradient,null_q_hess,NULL},
  {"knot_thickness_0",VERTEX,0,NOSPEC, knot_power_init,knot_thickness_0,
         knot_thickness_0_gradient, null_q_hess,NULL},
  {"knot_thickness",VERTEX,0,NOSPEC, NULL,knot_thickness,
         null_q_grad, null_q_hess,NULL},
  {"knot_thickness_p",VERTEX,0,NOSPEC, uniform_knot_energy_init,
        knot_thickness_p, knot_thickness_p_gradient, null_q_hess,NULL},
  {"knot_thickness_p2",VERTEX,0,NOSPEC, uniform_knot_energy_init,
     knot_thickness_p2, knot_thickness_p2_gradient, null_q_hess,NULL},
  {"knot_thickness2",VERTEX,0,NOSPEC, NULL, knot_thickness2,
     null_q_grad, null_q_hess,NULL},
  {"knot_local_thickness",VERTEX,0,NOSPEC, NULL, knot_local_thickness,
     null_q_grad, null_q_hess,NULL},
  {"curvature_function",VERTEX,0,NOSPEC,curvature_forces_init,
      curvature_forces_energy, curvature_forces,null_q_hess,NULL},
  {"johndust",VERTEX,0,NOSPEC,NULL,johndust_energy,
      johndust_gradient,null_q_hess,NULL},
  {"true_writhe",EDGE,0,NOSPEC,NULL, true_writhe, null_q_grad,null_q_hess,NULL},
  {"true_average_crossings",EDGE,0,NOSPEC, NULL,
      true_average_crossing,null_q_grad,null_q_hess,NULL},
  {"ackerman",VERTEX,0,NOSPEC,ackerman_init,ackerman_energy,
      ackerman_forces,null_q_hess,NULL},
  {"carter_energy",FACET,NEED_SIDE,NOSPEC,carter_energy_init,
      carter_energy, carter_energy_gradient,null_q_hess,NULL},
  {"curvature_binormal",VERTEX,0,NOSPEC,curvature_binormal_init,
      curvature_binormal_energy, curvature_binormal_force ,null_q_hess,NULL},
  {"ddd_gamma_sq",EDGE,NEED_STRING_STAR,NOSPEC,ddd_gamma_sq_init,
      ddd_gamma_sq_energy, ddd_gamma_sq_gradient ,null_q_hess,NULL},
  { " ",0,0,0,NULL,NULL,NULL,NULL,NULL}    /* to signal end of array */
};

/*********************************************************************/


/*************************************************************************
*
*  function: quantity_init()
*
*  purpose: initialize quantity structures for a surface
*/

void quantity_init()
{ int k;

  /* set lists to empty; memory deallocation taken care of by global
     permanent memory freeing at the start of each new surface */
  gen_quant_count = 0;
  gen_quant_list_max = 0;
  gen_quant_free_left = 0;
  gen_quant_free = NULL;

  meth_inst_count = 0;
  meth_inst_list_max = 0;
  meth_inst_free_left = 0;
  meth_inst_free = NULL;

  compound_quant_list_head = -1;

  meth_inst_count = LOW_INST;  /* skip 0, so all indexes signable */
  meth_inst_alloc = 20;
  dy_meth_inst = dy_calloc(meth_inst_alloc,sizeof(struct method_instance));

  for ( k = 0 ; k < NUMELEMENTS ; k++ ) 
  { quant_flags[k] = 0;
    global_meth_inst_count[k] = 0;
  }
  memset((char*)global_meth_inst,0,sizeof(global_meth_inst));
}

/*************************************************************************
*
* function: find_quantity()
*
* purpose: find existing quantity by name in globals table.
*
* return: quantity number, or -1 if not found.
*/

int find_quantity(name)
char *name;
{ int q;

  q = lookup_global_hash(name,0,QUANTITYNAME,HASH_LOOK);
  if ( (q & NAMETYPEMASK) == QUANTITYNAME )
    return q & INDEXMASK;

  return -1;
}

/*************************************************************************
*
* function: find_method_instance()
*
* purpose: find existing instance by name
*
* return: instance number, or -1 if not found.
*/

int find_method_instance(name)
char *name;
{ int n;
  
  n = lookup_global_hash(name,0,METHODNAME,HASH_LOOK);
  if ( (n & NAMETYPEMASK) == METHODNAME )
    return n & INDEXMASK;

  return -1;
}

/*************************************************************************
*
*  function: new_quantity()
*
*  purpose:  set up new named quantity.
*            returns number of new quantity.
*            If already exists, returns number, does not re-initialize.
*/

int new_quantity(name,mode)
char *name;  /* identifying name for this quantity */
int mode; /* Q_ENERGY, Q_FIXED, Q_CONSERVED, or Q_INFO */
{ int q=0;  /* new structure */
  int g;  /* hash return */
 
  /* check to see if name used, and add if not */
  g = lookup_global_hash(name,0,QUANTITYNAME,0);
  if ( g != 0 )
  { int qq = g & INDEXMASK;
    if ( datafile_flag == IN_DATAFILE )
    { if ( (GEN_QUANT(qq)->flags & Q_FORWARD_DEF) || addload_flag )
      { 
        q = qq;
      }
      else
      { 
        sprintf(errmsg,"'%s' already declared.\n",name);
        kb_error(1555,errmsg,RECOVERABLE);
      }
    }
  }
  else /* need to allocate new structure */
  { /* check to see if any free ones left */
    if ( gen_quant_free_left <= 0 )
    { gen_quant_free_left = 100;
      gen_quant_free = (struct gen_quant*)mycalloc(gen_quant_free_left,
                        sizeof(struct gen_quant));
    }
    if ( gen_quant_count >= gen_quant_list_max )
    { if ( gen_quant_list_max == 0 )
      { gen_quant_list_max = 1000;
        gen_quant_list = (struct gen_quant**)mycalloc(gen_quant_list_max,
             sizeof(struct gen_quant*));
      }
      else
      { gen_quant_list = (struct gen_quant **)kb_realloc((char*)gen_quant_list,
           2*gen_quant_list_max*sizeof(struct gen_quant*));
        gen_quant_list_max *= 2;
     }
    }
    
    /* initialize */
    q = gen_quant_count; 
    gen_quant_list[q] = gen_quant_free;
    g = lookup_global_hash(name,q,QUANTITYNAME,HASH_ADD);
    GEN_QUANT(q)->num = q;
    GEN_QUANT(q)->modulus = 1.0;
    GEN_QUANT(q)->timestamp = -1;
    GEN_QUANT(q)->tolerance = -1.0;
    strncpy(GEN_QUANT(q)->name,name,sizeof(GEN_QUANT(q)->name));
  
    gen_quant_count++;  /* so seen by global_hash_expand */
    gen_quant_free++; 
    gen_quant_free_left--;  
  }
    
  GEN_QUANT(q)->flags = mode;
  return q;
}

/*************************************************************************
*
*  function: new_method_instance()
*
*  purpose:  set up new method instance.
*            If already exists, then returns instance number; 
*               does not re-initialize.
*  return:    index of new instance in METH_INST list
*                
*/

int new_method_instance(meth_name,inst_name)
char *meth_name,*inst_name;
{ struct method_instance * m; /*  new instance */
  struct gen_quant_method *gm=NULL;
  int n=0,nb;
  int inst_num; /* number of new instance */
  int g; /* hash return */

  if ( meth_name )
  {
    nb =  sizeof(basic_gen_methods)/sizeof(struct gen_quant_method);

    for ( gm = basic_gen_methods,n = 0 ; n < nb ; n++,gm++ )
      if ( stricmp(gm->name,meth_name) == 0 ) break;
    if ( n >= nb )
    { sprintf(errmsg,"No method '%s' exists.\n",meth_name);
      kb_error(1559,errmsg,DATAFILE_ERROR); return -1; 
    }
  }
  
  /* make sure there is room for new instance */
  if ( meth_inst_free_left <= 0 )
  { meth_inst_free_left = 100;
    meth_inst_free = (struct method_instance*)mycalloc(meth_inst_free_left,
                        sizeof(struct method_instance));
  }
  if ( meth_inst_count >= meth_inst_list_max )
  { if ( meth_inst_list_max == 0 )
    { meth_inst_list_max = 1000;
      meth_inst_list = (struct method_instance**)mycalloc(meth_inst_list_max,
             sizeof(struct method_instance*));
    }
    else
    { meth_inst_list = 
        (struct method_instance **)kb_realloc((char*)meth_inst_list,
           2*meth_inst_list_max*sizeof(struct method_instance*));
      meth_inst_list_max *= 2;
    }
  }

  inst_num = meth_inst_count;
  meth_inst_list[inst_num] = meth_inst_free;
  m = METH_INSTANCE(inst_num);
  memset(m,0,sizeof(struct method_instance));
  m->self_id = METHBASE + inst_num;
  m->modulus = 1.0;
  m->gen_method = n;
  m->quant = -1; /* not attached to quantity yet */
  m->timestamp = -1;
  if ( gm ) 
  { m->type = gm->type;
    if ( gm->spec_flags & SPEC_USE_DENSITY )
      m->flags |= USE_DENSITY;
   }
  if ( strlen(inst_name) >= MNAMESIZE )
  { sprintf(errmsg,"Method name too long: %s\n",inst_name);
    kb_error(2856,errmsg,RECOVERABLE);
  }
  strncpy(m->name,inst_name,sizeof(m->name));

  g = lookup_global_hash(inst_name,inst_num,METHODNAME,HASH_ADD);
  if ( g != 0 )
  { int inum = g & INDEXMASK;
    
    if ( datafile_flag == IN_DATAFILE )
    { if ( (METH_INSTANCE(inum)->flags & Q_FORWARD_DEF) || addload_flag )
      { *METH_INSTANCE(inum) = *m; /* copy over initialization */
        inst_num = inum;
        METH_INSTANCE(inum)->self_id = METHBASE + inst_num;
      }
      else
      { sprintf(errmsg,"'%s' already declared.\n",inst_name);
        memset((char*)m,0,sizeof(struct method_instance));    
        kb_error(1558,errmsg,RECOVERABLE);
      }
    }
  }
  else
  { meth_inst_count++; 
    meth_inst_free++; 
    meth_inst_free_left--;  
  }

  return inst_num;
}

/*********************************************************************
*
*  function: add_standard_quantity()
*
*  purpose: for common quantities, creates method instance
*              and quantity.  Global energy only.
*/

int add_standard_quantity(meth_name,modulus)
char *meth_name;  /* name of method (not instance) */
REAL modulus;
{
  int mi;
  int q;
  char inst_name[40];

  strncpy(inst_name,meth_name,25);
  strcat(inst_name,"_inst");
  mi = new_method_instance(meth_name,inst_name);
  q = new_quantity(meth_name,Q_ENERGY);
  attach_method_num(q,mi);
  apply_method(NULLID,inst_name);
  METH_INSTANCE(mi)->modulus = 1.0;
  GEN_QUANT(q)->modulus = modulus;
  GEN_QUANT(q)->flags |= STANDARD_QUANTITY;
 
  return q;
}

/*********************************************************************
*
* function: attach_method()
*
* purpose: attach method instance to quantity
*
* return: instance number
*/

int attach_method(quantnum,meth_name)
int quantnum;
char *meth_name;
{ 
  int inst_num;

  inst_num = find_method_instance(meth_name);
  if ( inst_num < 0 )
  { sprintf(errmsg,"Undefined method instance '%s'. \n",meth_name);
    kb_error(1562,errmsg,DATAFILE_ERROR); return -1; 
  }
  return attach_method_num(quantnum,inst_num);
}

int attach_method_num(quantnum,inst_num)
int quantnum;
int inst_num;
{ struct method_instance *m = METH_INSTANCE(inst_num);
  struct gen_quant_method *gm;
  struct gen_quant *quant = GEN_QUANT(quantnum);
  int i;

  if ( (inst_num < 0) || (inst_num >= meth_inst_count) )
   kb_error(2446,
    "Internal error: Unknown method number in attach_method_num().\n",
     RECOVERABLE);

  if ( (m->quant >= 0) && (m->quant != quantnum) )
  { sprintf(errmsg,"%s: Sorry, but for now, a method instance can only belong to one quantity.\n",m->name);
    kb_error(1564,errmsg,DATAFILE_ERROR);
  }

  m->quant = quant->num;
  gm = basic_gen_methods + m->gen_method;
  if ( (gm->gradient==NULL) && (quant->flags & (Q_ENERGY|Q_FIXED|Q_CONSERVED)) )
  { sprintf(errmsg,"Sorry; method %s does not have a gradient available.\n",gm->name);
    kb_error(1565,errmsg,DATAFILE_ERROR);
  }
  for ( i = 0 ; i < quant->method_count ; i++ )
    if ( quant->meth_inst[i] == inst_num ) 
      goto attach_method_num_exit;

  if ( quant->meth_inst == NULL )
    quant->meth_inst = quant->meth_inst_space;
  else
  if ( quant->method_count >= MAXMETH )
  { if ( quant->meth_inst == quant->meth_inst_space )
    { quant->meth_inst = (int*)mycalloc(quant->method_count+1,sizeof(int));
      memcpy(quant->meth_inst,quant->meth_inst_space,MAXMETH*sizeof(int));
    }
    else
      quant->meth_inst = (int*)kb_realloc((char*)(quant->meth_inst),
         (quant->method_count+1)*sizeof(int));
  }

  quant->meth_inst[quant->method_count++] = inst_num; 
  quant_flags[gm->type] |= quant->flags & (Q_ENERGY|Q_FIXED|Q_INFO|Q_CONSERVED);
  quant->flags |= (basic_gen_methods[m->gen_method].flags & TORUS_MODULO_MUNGE);

attach_method_num_exit:
  return inst_num;
}

/*********************************************************************
*
* function: apply_method()
*
* purpose:  make a method apply to an element
*              if element id inverted and method orientable, then
*                    store method number as negative.
*              for null element, assumed to be global method of type
*/

void apply_method(id,method_name)
element_id id;
char *method_name;
{ 
  int inst_num;

  /* search for method */
  inst_num = find_method_instance(method_name);
  if ( inst_num < 0 )
  { sprintf(errmsg, "Undefined method instance '%s' on %s %s. \n",
         method_name,typenames[id_type(id)],ELNAME(id));
    kb_error(1566, errmsg,DATAFILE_ERROR); 
    return; 
  }

/* really annoying in case of compound quantity
  if ( valid_id(id) && (m->quant == -1) )
  { sprintf(errmsg,"Method instance '%s' not attached to a quantity.\n",
          method_name);
    kb_error(1567,errmsg,WARNING);
  }
*/

  apply_method_num(id,inst_num);
}


/*********************************************************************
*
* function: apply_method_num()
*
* purpose:  make a method apply to an element (by method number)
*              if element id inverted and method orientable, then
*                    store method number as negative.
*              for null element, assumed to be global method of type
*            If method of opposite sign already existing, then
*              it is deleted, due to cancellation.
*/

void apply_method_num(id,inst_num)
element_id id;
int inst_num;  /* method instance number */
{ 
  struct method_instance *m;
  struct element *e_ptr;
  int type; /* of element */
  struct gen_quant_method *gm;
  int i;

  /* search for method */
  if ( inst_num < -meth_inst_count )
     { kb_error(2178,"Internal error: Undefined method instance. \n",DATAFILE_ERROR); return; }
  if ( inst_num > meth_inst_count )
     { kb_error(2179,"Internal error: Undefined method instance. \n",DATAFILE_ERROR); return; }
  m = METH_INSTANCE(abs(inst_num));

  if ( m->flags & GLOBAL_INST )
  { /* trying to apply global method to single element */
    if ( inverted(id) )
    
    { kb_error(4569,"Cannot apply signed method/quantity to individual element.\n",DATAFILE_ERROR);
    }
    return;
  }
  gm = basic_gen_methods + m->gen_method;
  if ( valid_id(id) ) /* individual element */
  { int *instlist,maxinst;
    int meth_offset; 

    if ( inverted(id) && (gm->flags & ORIENTABLE_METHOD) )
       inst_num = -inst_num;
    type = id_type(id);
    meth_offset = get_meth_offset(type); 
    e_ptr = elptr(id);
    /* see if already there */
    instlist = (int*)((char*)e_ptr + meth_offset); 
    for ( i = 0 ; i < (int)e_ptr->method_count ; i++ )
    {  if ( instlist[i] == inst_num ) return;
       if ( instlist[i] == -inst_num )
       { /* cancels out */
         instlist[i] = instlist[--e_ptr->method_count];	
         return;
       }
    } 
    /* add */ 
    maxinst = EXTRAS(type)[web.meth_attr[type]].array_spec.datacount;
    if ( (int)e_ptr->method_count >= maxinst )
    { int newc = maxinst+4;
      expand_attribute(type,web.meth_attr[type],&newc);
      e_ptr = elptr(id);  /* since things moved */
      instlist = (int*)((char*)e_ptr + meth_offset); 
    }
    instlist[e_ptr->method_count] = inst_num;
    e_ptr->method_count++;
  }
  else /* global */
  { m->flags |= GLOBAL_INST;
    type = gm->type;
    /* see if already there */
    for ( i = 0 ; i < global_meth_inst_count[type] ; i++ )
      if ( global_meth_inst[type][i] == inst_num )
         break;
    if ( i >= global_meth_inst_count[type] )
    { /* add it */
      if ( global_meth_inst_count[type] >= MAXGLOBINST )
        kb_error(1568,"Too many global method instances.\n",DATAFILE_ERROR);
      else global_meth_inst[type][global_meth_inst_count[type]++] = inst_num;
    }
  } 
}


/*********************************************************************
*
* function: unapply_method()
*
* purpose:  disable a method for an element.
*              
*/

void unapply_method(id,inst_num)
element_id id;
int inst_num;  /* index in method instance list */
{ struct element *e_ptr;
  int i;
  int type = id_type(id);
  int meth_offset = EXTRAS(type)[web.meth_attr[type]].offset;
  int *methlist = (int*)((char*)elptr(id) + meth_offset);

  if ( valid_id(id) ) /* individual element */
  { e_ptr = elptr(id);
    /* see if already there */
    for ( i = 0 ; i < (int)e_ptr->method_count ; i++ )
      if ( abs(methlist[i]) == abs(inst_num) ) 
      { /* delete */
        methlist[i] = methlist[--e_ptr->method_count];
        break;
      }
  }
}

/********************************************************************
*
* function: apply_quantity()
*
* purpose: apply quantity methods to element, if appropriate
*/

void apply_quantity(id,quantnum)
element_id id;
int quantnum;
{ struct gen_quant *g = GEN_QUANT(quantnum);
  int i;
  int count = 0;
  if ( g->flags & Q_COMPOUND )
     kb_error(2180,"Compound quantity should have individual method instances applied to elements.\n",RECOVERABLE);
  for ( i = 0 ; i < g->method_count ; i++ )
  { struct method_instance *mi = METH_INSTANCE(g->meth_inst[i]);
    struct gen_quant_method *gm = basic_gen_methods + mi->gen_method;
    if ( gm->type == id_type(id) )
    { apply_method_num(id,g->meth_inst[i]);
      count++;
    }
  }
  if ( count == 0 )
  { sprintf(errmsg,"Quantity '%s' has no methods applying to this type element.\n",
         g->name);
     kb_error(1569,errmsg, WARNING);
  }
}


/********************************************************************
*
* function: unapply_quantity()
*
* purpose: disable quantity methods to element, if appropriate
*/

void unapply_quantity(id,quantnum)
element_id id;
int quantnum;
{ struct gen_quant *g = GEN_QUANT(quantnum);
  int i;
  for ( i = 0 ; i < g->method_count ; i++ )
    { struct method_instance *mi = METH_INSTANCE(g->meth_inst[i]);
      struct gen_quant_method *gm = basic_gen_methods + mi->gen_method;
      if ( gm->type == id_type(id) )
         { unapply_method(id,g->meth_inst[i]);
         }
    }
}

/*************************************************************************
*
*  function: q_info_init()
*
*  purpose: allocates q_info arrays
*
*/


void q_info_init(q_info,mode)
struct qinfo *q_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */
{ int m,i;
  int maxgauss = (gauss1D_num > gauss2D_num) ? gauss1D_num : gauss2D_num;

  if ( web.dimension >= SOAPFILM )
  {
  if ( gauss_lagrange[web.dimension][web.gauss2D_order].gnumpts > maxgauss )
    maxgauss = gauss_lagrange[web.dimension][web.gauss2D_order].gnumpts; 
  if ( gauss_lagrange[web.dimension-1][web.gauss1D_order].gnumpts > maxgauss )
    maxgauss = gauss_lagrange[web.dimension-1][web.gauss1D_order].gnumpts; 
  }
  else 
  if ( gauss_lagrange[web.dimension][web.gauss1D_order].gnumpts > maxgauss )
    maxgauss = gauss_lagrange[web.dimension][web.gauss1D_order].gnumpts; 

  if ( web.skel[FACET].ctrlpts > MAXVCOUNT )
     kb_error(1570,"Vertices in facet exceed MAXVCOUNT.\n",RECOVERABLE);


  memset((char*)q_info,0,sizeof(struct qinfo));
  /* set up matrices in qinfo with lots of room */
  q_info->xx = dmatrix(0,MAXVCOUNT,0,SDIM);
  if ( web.torus_flag )
  { q_info->u = dmatrix(0,MAXVCOUNT,0,SDIM);
     if ( (mode == METHOD_GRADIENT) || (mode == METHOD_HESSIAN) )
        q_info->ugrad = dmatrix(0,MAXVCOUNT,0,SDIM);
     if ( mode == METHOD_HESSIAN )
        q_info->uhess = dmatrix4(MAXVCOUNT,MAXVCOUNT,SDIM,SDIM);
  }
  q_info->sides = dmatrix3(maxgauss,MAXVCOUNT,SDIM);
  if ( web.modeltype == LINEAR )
     for ( m = 1 ; m < maxgauss ; m++ )
        q_info->sides[m] = q_info->sides[0];  /* since all the same */
  q_info->ss = dmatrix(0,2*SDIM,0,2*SDIM);
  if ( (mode == METHOD_GRADIENT) || (mode == METHOD_HESSIAN) )
     q_info->grad = dmatrix(0,MAXVCOUNT,0,SDIM);
  q_info->gauss_pt = dmatrix(0,maxgauss,0,2*SDIM); /* extra for point number */
  if ( web.torus_flag && (web.representation == SOAPFILM) )
  { /* allocate some working space for facet_volume method */
    int ectrl = web.lagrange_order + 1;
    q_info->uu[0] = (REAL **)temp_calloc(ectrl*3,sizeof(REAL*));
    q_info->uu[1] = q_info->uu[0]+ectrl; q_info->uu[2] = q_info->uu[1]+ectrl;
    if ( (mode == METHOD_GRADIENT) || (mode == METHOD_HESSIAN) )
    { q_info->uugrad[0] = (REAL **)temp_calloc(ectrl*3,sizeof(REAL*));
      q_info->uugrad[1] = q_info->uugrad[0]+ectrl; 
      q_info->uugrad[2] = q_info->uugrad[1]+ectrl;
    }
    if ( mode == METHOD_HESSIAN )
    { q_info->uuhess[0] = (REAL ****)temp_calloc(ectrl*3,sizeof(REAL***));
      q_info->uuhess[1] = q_info->uuhess[0]+ectrl; 
      q_info->uuhess[2] = q_info->uuhess[1]+ectrl;
      q_info->uuhess[0][0] = (REAL***)temp_calloc(3*ectrl*ectrl,sizeof(REAL**));
      for ( i = 0 ; i < 3 ; i++ )
        for ( m = 0 ; m < ectrl ; m++ )
           q_info->uuhess[i][m] = q_info->uuhess[0][0] + i*ectrl*ectrl + m*ectrl;
    }
  }     
}

/***********************************************************************
*
* function: q_info_free()
*
* purpose: deallocate all memory allocated to q_info structure.
*
*/

void q_info_free(q_info)
struct qinfo *q_info;
{
  free_matrix(q_info->xx);
  free_matrix3(q_info->sides);
  free_matrix(q_info->ss);
  free_matrix(q_info->grad);
  free_matrix(q_info->gauss_pt);
  if ( q_info->uu[0] ) 
  { temp_free((char*)q_info->uu[0]);
    temp_free((char*)q_info->uugrad[0]);
  }
  if ( q_info->uuhess[0] )
  { temp_free((char*)q_info->uuhess[0][0]);
    temp_free((char*)q_info->uuhess[0]);
  }
  if ( q_info->u )
  { free_matrix(q_info->u);
    free_matrix(q_info->ugrad);
    free_matrix4(q_info->uhess);
  }
}


/***************************************************************************
*
* function: global_meth_needs()
*
* purpose: find set of q_info needs for active methods. To be called
*    by quantity calc functions after inspecting quantities.
*/
int global_meth_needs(type)
int type;
{ int k;
  int needs = 0;
 
  for ( k = 0 ; k < global_meth_inst_count[type]; k++ )
  { int mi = global_meth_inst[type][k];
    if ( (METH_INSTANCE(mi)->flags & Q_DOTHIS) 
                  && (METH_INSTANCE(mi)->type == type) )
       needs |= basic_gen_methods[METH_INSTANCE(mi)->gen_method].flags;
  }
  return needs;
}

#ifdef KSR

__shared pthread_mutex_t ksrlock;
int team_id = -1;

int ksr_counter;
int m_next()
{ int val;
  M_LOCK(&ksr_counter); 
  val = ksr_counter++;
  M_UNLOCK(&ksr_counter);  
  return val;
}

void m_fork(func,type,mode)  
void (*func)();
int type;
int mode;
{ if ( team_id == -1 ) 
     { team_id = pr_create_team(NUMPROCS);
       pthread_mutex_init(&ksrlock,pthread_mutexattr_default);
     }
  ksr_counter = 0;
  pr_pcall(team_id,func,copyargs(type,mode));
}
#endif

int *v_procnum; /* processors for vertex, index by ord */
#if defined(SHARED_MEMORY)

/************************************************************************
*
* function: make_el_list()
*
* purpose: make element list for processes to step through.
*
* algorithm: sort in top dimension coordinate
*/

long el_list_timestamp[NUMELEMENTS] = { -1,-1,-1,-1,-1 };
  /* to see if need remake list */

int vertex_comp(a,b)
vertex_id *a,*b;
{ REAL xa = get_coord(*a)[SDIM-1];
  REAL xb = get_coord(*b)[SDIM-1];
  if ( xa < xb ) return -1;
  if ( xa > xb ) return 1;
  return 0;
}
int edge_comp(a,b)
edge_id *a,*b;
{ REAL xa = get_coord(get_edge_tailv(*a))[SDIM-1];
  REAL xb = get_coord(get_edge_tailv(*b))[SDIM-1];
  if ( xa < xb ) return -1;
  if ( xa > xb ) return 1;
  return 0;
}
int facet_comp(a,b)
facet_id *a,*b;
{ REAL xa,xb;
  if ( web.representation == SIMPLEX )
  { xa = get_coord(get_facet_vertices(*a)[0])[SDIM-1];
    xb = get_coord(get_facet_vertices(*b)[0])[SDIM-1];
  }
  else
  { xa = get_coord(get_fe_tailv(get_facet_fe(*a)))[SDIM-1];
    xb = get_coord(get_fe_tailv(get_facet_fe(*b)))[SDIM-1];
  }
  if ( xa < xb ) return -1;
  if ( xa > xb ) return 1;
  return 0;
}

void make_el_list(type)
int type;  /* element type */
{ element_id id,*tlist; 
  int pnum,i,bin,end,start;
  if ( el_list[type] ) myfree((char*)el_list[type]);
  tlist = el_list[type] = (element_id*)mycalloc(web.skel[type].count,
                                                     sizeof(element_id));
  FOR_ALL_ELEMENTS(type,id)
     *(tlist++) = id;

  switch(type)
  { case VERTEX: 
       qsort((char*)(el_list[type]),web.skel[type].count,
       sizeof(element_id), FCAST vertex_comp); 

       /* assign processor numbers */
       if ( v_procnum ) myfree((char*)v_procnum);
       v_procnum = (int*)mycalloc(web.skel[VERTEX].max_ord+1,sizeof(int));
       bin = (web.skel[type].count + nprocs - 1)/nprocs;
       for ( pnum = 0 ; pnum < nprocs ; pnum++ )
       { start = pnum*bin;
         end = start + bin;
         if ( end > web.skel[type].count ) end = web.skel[type].count;
         for ( i = start ; i < end ; i++ )
            v_procnum[loc_ordinal(el_list[VERTEX][i])] = pnum;
       }
       break;

    case EDGE: 
       qsort((char*)(el_list[type]),web.skel[type].count,
       sizeof(element_id), FCAST edge_comp); 
       break;

    case FACET: 
       qsort((char*)(el_list[type]),web.skel[type].count,
       sizeof(element_id), FCAST facet_comp); 
       break;
  }

  el_list_timestamp[type] = top_timestamp;
}
/***********************************************************************
*
*  function: multi_calc_quants()
*
*  purpose:  calculate all values of quantities in parallel.
*                This function called in parallel from calc_quants().
*                m_next used to parcel out elements to processors.
*
*/

void multi_calc_quants(type)
int type;    /* element type */
{
  int k;
  struct element *e_ptr;
  struct gen_quant_method *gm;
  int mi;
  REAL val;
  struct qinfo q_info;  /* data passing structure */
  int meth_offset = EXTRAS(type)[web.meth_attr[type]].offset; 
  int me = GET_THREAD_ID;  /* which process I am */
  int global_needs;

#ifdef THREADS
__int32 multi_calc_quants_elapsed_time[2];
multi_calc_quants_elapsed_time[0] = 0;
multi_calc_quants_elapsed_time[1] = 0;
#endif
  
#ifdef SIGUSR1
  signal(SIGUSR1,catcher);    /* to catch user interrupt */ 
#endif
  signal(SIGINT,catcher);    /* to catch user interrupt */     
  m_breakflag[me] = 0;
  if ( setjmp(m_jumpbuf[me]) ) { q_info_free(&q_info); return; }

PROF_START(multi_calc_quants)
  q_info_init(&q_info,METHOD_VALUE); 

  global_needs = global_meth_needs(type);

  THREAD_FOR_ALL_NEW(type,   /* following block is macro argument! */
  { int setup_flag = 0;
    int needs;  /* particular setup needs for current mode */
   
    q_info.id = *idptr;
    e_ptr = elptr(q_info.id);
    /* get setup flags */
    needs = global_needs;
    for ( k = 0 ; k < e_ptr->method_count ; k++ )
    { int mm;
      mm = ((int*)((char*)e_ptr+meth_offset))[k];
      mi = abs(mm);
      if ( (METH_INSTANCE(mi)->flags & Q_DOTHIS) 
             && (METH_INSTANCE(mi)->type == type) )
        needs |= basic_gen_methods[METH_INSTANCE(mi)->gen_method].flags;
    }

    for ( k = 0 ; k < global_meth_inst_count[type]; k++ )
    { mi = global_meth_inst[type][k];
      if ( (METH_INSTANCE(mi)->flags & Q_DOTHIS) 
                  && (METH_INSTANCE(mi)->type == type) )
      { 
        if ( !setup_flag ) 
        { (*q_setup[type])(NULL,&q_info,needs); setup_flag = 1; }
        q_info.method = mi;
        gm = basic_gen_methods + METH_INSTANCE(mi)->gen_method;
        val = (*gm->value)(&q_info);
        if ( METH_INSTANCE(mi)->flags & ELEMENT_MODULUS_FLAG )
            val *= *(REAL*)get_extra(q_info.id,METH_INSTANCE(mi)->elmodulus);
        METH_INSTANCE(mi)->procvalue[me] += val;
        METH_INSTANCE(mi)->procabstotal[me] += fabs(val);
      }
    }
    for ( k = 0 ; k < e_ptr->method_count ; k++ )
    { int mm;
      mm = ((int*)((char*)e_ptr+meth_offset))[k];
      q_info.method = mi = abs(mm);
      if ( (METH_INSTANCE(mi)->flags & Q_DOTHIS) 
             && (METH_INSTANCE(mi)->type == type) )
      {
        if ( !setup_flag )  
        { (*q_setup[type])(NULL,&q_info,needs); setup_flag = 1; }
        gm = basic_gen_methods + METH_INSTANCE(mi)->gen_method;
        val = (*gm->value)(&q_info);
        if ( METH_INSTANCE(mi)->flags & ELEMENT_MODULUS_FLAG )
            val *= *(REAL*)get_extra(q_info.id,METH_INSTANCE(mi)->elmodulus);
        METH_INSTANCE(mi)->procvalue[me] += (mm < 0 ) ? -val : val;
        METH_INSTANCE(mi)->procabstotal[me] += fabs(val);
      }
    }
  }
) /* end THREAD_FOR_ALL_NEW macro */

  q_info_free(&q_info);
PROF_FINISH(multi_calc_quants)
/*
  if ( verbose_flag )
  { PROF_PRINT(multi_calc_quants)
  }
*/
}
#endif

/***********************************************************************
*
*  function: calc_quants()
*
*  purpose:  calculate all values of quantities.
*                Info passed to methods in global structure
*
*/

REAL calc_quants(mode)
int mode;  /* energy, constraint, and/or info flag bits */
{ int k;
  struct element *e_ptr;
  struct gen_quant *q = NULL;
  int type; /* element type */
  struct method_instance * mi;
  struct gen_quant_method *gm;
  REAL energy = 0.0;
  struct qinfo q_info;  /* data passing structure */
  int todo = 0; /* whether any to do */
  int global_needs;
#ifdef PROFILING_ENABLED
  __int32 value_elapsed_time[2];
#endif

  PROF_START(calc_quants);

  if ( calc_quant_flag ) return 0.0;
  calc_quant_flag = 1; /* so no recursive evaluation */

  /* method initialization */
  for ( type = 0 ; type < NUMELEMENTS ; type++ ) quant_flags[type] = 0;
  for ( k = LOW_INST  ; k < meth_inst_count  ; k++ )
  { mi = METH_INSTANCE(k); /* since some init may move things */
    gm = basic_gen_methods + mi->gen_method;
    if ( mi->quant >= 0 ) q = GEN_QUANT(mi->quant);
    if ( (!(mi->flags&Q_COMPOUND) && (mi->quant >= 0) &&  
                   ((q->modulus == 0.0) || !(q->flags & mode)))
          || (mi->modulus == 0.0) /* || (mi->timestamp == global_timestamp) */ )
    { mi->flags &= ~Q_DOTHIS;
      if ( q->flags & mode ) mi->timestamp = global_timestamp;
    }
    else
    { int kk;
	  mi->flags |= Q_DOTHIS;
      mi->newvalue = 0.0;
      for ( kk = 0 ; kk < MAXADDENDS ; kk++ ) mi->value_addends[kk] = 0.0;
      mi->abstotal = 0.0;
      mi->timestamp = global_timestamp;
      quant_flags[basic_gen_methods[mi->gen_method].type] |= mode;
      if ( gm->init ) 
           (*gm->init)(METHOD_VALUE,mi);
      todo = 1;
    }
  }

  if ( !todo ) goto add_to_quantities;

  q_info_init(&q_info,METHOD_VALUE); 

  for ( type = VERTEX ; type <= BODY ; type++ )
  if ( quant_flags[type] & mode )
  { int meth_offset = get_meth_offset(type); 
#if defined(SHARED_MEMORY)
    if ( (nprocs > 1) || threadflag ) 
    { int i;
      if ( el_list_timestamp[VERTEX] < top_timestamp )
         make_el_list(VERTEX);  /* need for force */
      if ( el_list_timestamp[type] < top_timestamp )
         make_el_list(type);
      for ( k = LOW_INST  ; k < meth_inst_count  ; k++ )
      { mi = METH_INSTANCE(k); 
         if ( mi->flags & Q_DOTHIS )
          for ( i = 0 ; i < nprocs ; i++ )
          {  mi->procvalue[i] = 0.0;
             mi->procabstotal[i] = 0.0;
          }
      }
      for ( i = 0 ; i < nprocs ; i++ ) proc_total_area[i] = 0.0;
#ifdef SGI_MULTI
      if ( mpflag == M_INACTIVE ) m_rele_procs();  /* resume parked procs */
      mpflag = M_ACTIVE;
      m_fork(multi_calc_quants,type,mode);
      m_park_procs();
      mpflag = M_INACTIVE; 
#endif
#ifdef THREADS
      m_type = type;
      thread_launch(TH_MULTI_CALC_QUANT,type);
#endif

      /* sum separate process values */
      for ( k = LOW_INST  ; k < meth_inst_count  ; k++ )
      { mi = METH_INSTANCE(k); 
         if ( mi->flags & Q_DOTHIS )
          for ( i = 0 ; i < nprocs ; i++ )
          { mi->newvalue += mi->procvalue[i];
            mi->abstotal += mi->procabstotal[i];
          }
      }
      for ( i = 0 ; i < nprocs ; i++ )
         web.total_area += proc_total_area[i];
    }
    else
#endif
  {
    global_needs = global_meth_needs(type);
    FOR_ALL_ELEMENTS(type,q_info.id)
    { int setup_flag = 0;
      REAL value;
      int needs;
      e_ptr = elptr(q_info.id);
      /* get setup flags */
      needs = global_needs;
      for ( k = 0 ; k < e_ptr->method_count ; k++ )
      { int mm;
        mm = ((int*)((char*)e_ptr+meth_offset))[k];
        mi = METH_INSTANCE(abs(mm));
        if ( (mi->flags & Q_DOTHIS) && (mi->type == type) )
          needs |= basic_gen_methods[mi->gen_method].flags;
      }
      for ( k = 0 ; k < global_meth_inst_count[type] ; k++ )
      { int m =  global_meth_inst[type][k];
        mi = METH_INSTANCE(m);
        if ( (mi->flags & Q_DOTHIS) && (mi->type == type) )
        { q_info.method = m;
          PROF_START(element_setup);
          if ( !setup_flag ) 
          { (*q_setup[type])(NULL,&q_info,needs); setup_flag = 1; }
          PROF_FINISH(element_setup);
          mi = METH_INSTANCE(m);  /* may have changed */
          METHOD_PROFILING_START(mi,value);
          gm = basic_gen_methods + mi->gen_method;
          value = (*gm->value)(&q_info);
           if ( mi->flags & ELEMENT_MODULUS_FLAG )
            value *= *(REAL*)get_extra(q_info.id,mi->elmodulus);
          binary_tree_add(mi->value_addends,value);
          mi->abstotal += fabs(value);
          METHOD_PROFILING_END(mi,value);
        }
     }
      for ( k = 0 ; k < (int)e_ptr->method_count ; k++ )
      { int m,mm;
        mm = ((int*)((char*)e_ptr+meth_offset))[k];
        m = abs(mm);
        q_info.method = m;
        mi = METH_INSTANCE(m);
        if ( (mi->flags & Q_DOTHIS) && (mi->type == type) )
        { gm = basic_gen_methods + mi->gen_method;
          PROF_START(element_setup);
          if ( !setup_flag ) 
          { (*q_setup[type])(NULL,&q_info,needs); setup_flag = 1; }
          PROF_FINISH(element_setup);
          mi = METH_INSTANCE(m);
          METHOD_PROFILING_START(mi,value);
          value =  (*gm->value)(&q_info);
           if ( mi->flags & ELEMENT_MODULUS_FLAG )
            value *= *(REAL*)get_extra(q_info.id,mi->elmodulus);
          if (mm < 0) value = -value;
          binary_tree_add(mi->value_addends,value);
          mi->abstotal += fabs(value);
          METHOD_PROFILING_END(mi,value);
       }
      }
    }
  }
 }
  q_info_free(&q_info);

  /* finish off binary tree addition */
  for ( k = LOW_INST  ; k < meth_inst_count  ; k++ )
  { int kk;
    mi = METH_INSTANCE(k);
    if ( mi->flags & Q_DOTHIS )
      for ( kk = 0 ; kk < MAXADDENDS ; kk++ )
        mi->newvalue += mi->value_addends[kk];
  }

  add_to_quantities:
  for ( k = LOW_INST  ; k < meth_inst_count  ; k++ )
  { mi = METH_INSTANCE(k);
    if ( mi->flags & Q_DOTHIS )
      mi->value = mi->modulus*mi->newvalue;
  }

  /* combine methods to quantities */
  for ( k = 0 ; k < gen_quant_count ; k++ )
  { q = GEN_QUANT(k);
    if ( q->flags & mode ) 
    { 
      #ifdef MPI_EVOLVER
      q->value = 0.0;
      #else
      q->value = q->volconst;
      #endif
      q->abstotal = 0.0;
      q->timestamp = global_timestamp;
    }
  }
  for ( k = LOW_INST  ; k < meth_inst_count  ; k++ )
  { mi = METH_INSTANCE(k);
    if ( mi->quant < 0 ) continue;
    q = GEN_QUANT(mi->quant);
    if ( q->flags & mode )
    {  q->value += mi->value;
       q->abstotal += mi->abstotal;
    }
  }
  
  for ( k = compound_quant_list_head ; k >= 0 ; k = q->next_compound )
  { q = GEN_QUANT(k);
    if ( (q->flags & mode) && ( q->flags & Q_COMPOUND ) )
    {  q->value = eval(&q->expr,NULL,NULLID,NULL);
       q->abstotal = 1.0;  /* best I can think of for the moment */
    }
  }
  
  /* set up body volumes */
  if ( (mode & Q_FIXED) && web.torus_flag )
  { /* munge fixed volumes modulo torus volume */
    for ( k = 0 ; k < gen_quant_count ; k++ )
    { q = GEN_QUANT(k);
      if ( (q->flags & Q_FIXED) && (q->flags & TORUS_MODULO_MUNGE) ) 
          q->value -= web.torusv*
                  (int)(0.5+(q->value - q->target)/(q->modulus*web.torusv));
    }
  }
  if ( everything_quantities_flag )
  { body_id b_id;
    FOR_ALL_BODIES(b_id)
    { q = GEN_QUANT(get_body_volquant(b_id));
      if ( q->flags & mode )
         set_body_volume(b_id,q->value,SETSTAMP);
    }
  }
  
  /* multiply by quantity modulus */
  for ( k = 0 ; k < gen_quant_count ; k++ )
  { q = GEN_QUANT(k);
    if ( q->flags & mode ) 
    {     
      q->value *= q->modulus;    
      q->abstotal *= fabs(q->modulus);
      q->timestamp = global_timestamp;
    }
  }

  /* add to total energy, if needed */
  if ( mode & Q_ENERGY )
  {
     for ( k = 0 ; k < gen_quant_count ; k++ )
     { q = GEN_QUANT(k);
        if ( q->flags & Q_ENERGY ) 
          energy += q->value;
     }
  }

  

  /* take care of any cleanup */
  for ( k = LOW_INST  ; k < meth_inst_count  ; k++ )
  { mi = METH_INSTANCE(k); /* since some init may move things */
    gm = basic_gen_methods + mi->gen_method;
    if ( gm->cleanup && mi->flags & Q_DOTHIS ) gm->cleanup();
  }

  calc_quant_flag = 0;

  PROF_FINISH(calc_quants)

  return energy;
} /* end calc_quants() */ 

/********************************************************************
*
* function: quantity_attribute()
*
* purpose: calculate quantity for one element
*          Warning: does not do own initialization!
*          Uses any initialization left over from calc_quants().
*/

REAL quantity_attribute(id,qnum)
element_id id;
int qnum;  /* number of quantity */
{
  int k;
  struct element *e_ptr;
  struct gen_quant *q;
  int type = id_type(id); /* element type */
  struct method_instance *mi;
  struct gen_quant_method *gm;
  REAL retval = 0.0,value;
  struct qinfo q_info;  /* data passing structure */
  int meth_offset = EXTRAS(type)[web.meth_attr[type]].offset;
  int *methlist = (int*)((char*)elptr(id) + meth_offset);
  int needs = 0;

  q_info_init(&q_info,METHOD_VALUE);  /* terribly inefficient */

  q_info.id = id;
  e_ptr = elptr(q_info.id);
  (*q_setup[type])(NULL,&q_info,needs);
  for ( k = 0 ; k < global_meth_inst_count[type] ; k++ )
  { mi = METH_INSTANCE(global_meth_inst[type][k]);
    if ( mi->quant != qnum ) continue;
    if ( mi->type != type ) continue;
    q =  GEN_QUANT(mi->quant);
    q_info.method = global_meth_inst[type][k];
    gm = basic_gen_methods + mi->gen_method;
    if ( (gm->flags & ALL_NEEDS) & ~needs )
    { needs |= gm->flags; (*q_setup[type])(NULL,&q_info,needs); }
    value = q->modulus*mi->modulus*(*gm->value)(&q_info);
    if ( mi->flags & ELEMENT_MODULUS_FLAG )
       value *= *(REAL*)get_extra(q_info.id,mi->elmodulus);
    retval += value;
  }
  for ( k = 0 ; k < (int)e_ptr->method_count ; k++ )
  { int mm = methlist[k];
    int m = abs(mm);
    q_info.method =  m;
    mi = METH_INSTANCE(m);
    if ( mi->quant != qnum ) continue;
    if ( mi->type != type ) continue;
    q =  GEN_QUANT(mi->quant);
    gm = basic_gen_methods + mi->gen_method;
    if ( (gm->flags & ALL_NEEDS) & ~needs )
    { needs |= gm->flags; (*q_setup[type])(NULL,&q_info,needs); }
    value = q->modulus*mi->modulus*(*gm->value)(&q_info);
    if ( mi->flags & ELEMENT_MODULUS_FLAG )
       value *= *(REAL*)get_extra(q_info.id,mi->elmodulus);
    if ( mm < 0 ) retval -= value;
    else  retval += value;
  }
  q_info_free(&q_info);
  return retval;
}


/********************************************************************
*
* function: instance_attribute()
*
* purpose: calculate method instance for one element
*             Warning: does not do own initialization!
*             Uses any initialization left over from calc_quants().
*/

REAL instance_attribute(id,qnum)
element_id id;
int qnum;  /* number of instance */
{
  int k;
  struct element *e_ptr;
  int type = id_type(id); /* element type */
  struct method_instance *mi=NULL;
  struct gen_quant_method *gm;
  REAL retval = 0.0;
  struct qinfo q_info;  /* data passing structure */
  int meth_offset = EXTRAS(type)[web.meth_attr[type]].offset;
  int *methlist = (int*)((char*)elptr(id) + meth_offset);
  int needs = 0;

  q_info_init(&q_info,METHOD_VALUE);  /* terribly inefficient */

  q_info.id = id;
  e_ptr = elptr(q_info.id);
  (*q_setup[type])(NULL,&q_info,needs);
  for ( k = 0 ; k < global_meth_inst_count[type] ; k++ )
  { if ( qnum != global_meth_inst[type][k] ) continue;
    mi = METH_INSTANCE(global_meth_inst[type][k]);
    if ( mi->type != type ) continue;
    q_info.method = global_meth_inst[type][k];
    gm = basic_gen_methods + mi->gen_method;
    if ( (gm->flags & ALL_NEEDS) & ~needs )
    { needs |= gm->flags; (*q_setup[type])(NULL,&q_info,needs); }
    retval += mi->modulus*(*gm->value)(&q_info);
    break;
  }
  for ( k = 0 ; k < (int)e_ptr->method_count ; k++ )
  { int mm = methlist[k];
    int m = abs(mm);
    if ( qnum != m ) continue;
    mi = METH_INSTANCE(m);
    q_info.method = m;
    if ( mi->type != type ) continue;
    gm = basic_gen_methods + mi->gen_method;
    if ( (gm->flags & ALL_NEEDS) & ~needs )
    { needs |= gm->flags; (*q_setup[type])(NULL,&q_info,needs); }
    if ( mm < 0 ) retval -=  mi->modulus*(*gm->value)(&q_info);
    else retval +=  mi->modulus*(*gm->value)(&q_info);
    break;
  } 
  if ( mi && (mi->flags & ELEMENT_MODULUS_FLAG) )
       retval *= *(REAL*)get_extra(id,METH_INSTANCE(qnum)->elmodulus);
  q_info_free(&q_info);
  return retval;
}

#if defined(SHARED_MEMORY)
/* SGI multiple processor stuff */
void m_fill_grad(struct hess_verlist *, REAL *,int,REAL *);
void m_fill_mixed_entry(vertex_id,vertex_id,REAL**,int);
/***********************************************************************
*
*  function: m_calc_quant_grads()
*
*  purpose:  calculate gradients of quantities
*            Called by calc_quant_grads()
*
*/

void m_calc_quant_grads(type)
int type;    /* element type */
{ int i,j;
  struct element *e_ptr;
  struct gen_quant *q;
  volgrad *vgptr;  /* constraint gradients */
  int flag;  /* 0 if doing global quantities, 1 for local */
  vertex_id v;
  struct gen_quant_method *gm;
  struct method_instance  *mi;
  REAL val;
  REAL *f;
  int inum;
  struct qinfo q_info;  /* data passing structure */
  int meth_offset = get_meth_offset(type); 
  int me = GET_THREAD_ID;
  int global_needs;

#ifdef SIGUSR1
  signal(SIGUSR1,catcher);    /* to catch user interrupt */   
#endif
  signal(SIGINT,catcher);    /* to catch user interrupt */     
  m_breakflag[me] = 0;
  if ( setjmp(m_jumpbuf[me]) ) { q_info_free(&q_info); return; }

  q_info_init(&q_info,METHOD_GRADIENT);

  global_needs = global_meth_needs(type);

  THREAD_FOR_ALL_NEW(type,  /* following block is macro argument! */
  { int k;
    int setup_flag = 0;
    int needs;

    q_info.id = *idptr;
    e_ptr = elptr(q_info.id);

    /* get setup flags */
    needs = global_needs;
    for ( k = 0 ; k < e_ptr->method_count ; k++ )
    { int mm;
      mm = ((int*)((char*)e_ptr+meth_offset))[k];
      mi = METH_INSTANCE(abs(mm));
      if ( (mi->flags & Q_DOTHIS) && (mi->type == type) )
        needs |= basic_gen_methods[mi->gen_method].flags;
    }

    inum = global_meth_inst_count[type];
    for ( flag = 0 ; flag < 2 ; flag++,inum = e_ptr->method_count,k=0 )
     for ( k = 0 ; k < inum ; k++ )
     { int mm;
       int sign = 1;
       if ( flag ) 
       { mm = ((int*)((char*)e_ptr+meth_offset))[k];
         q_info.method = abs(mm);
         if ( mm < 0 ) sign = -1;
       }
       else  q_info.method = global_meth_inst[type][k];
       mi = METH_INSTANCE(q_info.method);
       if ( mi->quant >= 0 ) q = GEN_QUANT(mi->quant);
       if ( (mi->flags & Q_DOTHIS) && (mi->type == type) )
       { REAL c = sign*mi->modulus*q->modulus;
         REAL *p;
         REAL **pp;
         gm = basic_gen_methods + mi->gen_method;
         if ( !setup_flag ) 
         { (*q_setup[type])(NULL,&q_info,needs); setup_flag = 1; }
         for ( i = 0, pp = q_info.grad ; i < q_info.vcount ; i++,pp++ )
           for ( j = 0, p = *pp ; j < SDIM ; j++,p++ ) *p = 0.0;
         val = (*gm->gradient)(&q_info);
         mi = METH_INSTANCE(q_info.method);
         if ( mi->flags & ELEMENT_MODULUS_FLAG )
         { REAL emdls = *(REAL*)get_extra(q_info.id,mi->elmodulus);
           val *= emdls;
           for ( i = 0 ; i < q_info.vcount ; i++ )
               for ( j = 0 ; j < SDIM ; j++ )
                    q_info.grad[i][j] *= emdls;
         }
         mi->procvalue[me] += sign*val;
         mi->procabstotal[me] += fabs(val);
         if ( mi->flags & Q_COMPOUND )
         { if ( q_info.vcount > MAXCOORD+2 )
             kb_error(2181,
"Too many vertices in method for compound quantity, due to lazy programmer.\n",
RECOVERABLE);
           for ( i = 0 ; i < q_info.vcount ; i++ )
             for ( j = 0 ; j < SDIM ; j++ )
               mi->grad[i][j] = q_info.grad[i][j];
         }
         if ( mi->quant >= 0 && !(mi->flags & Q_COMPOUND) )
           for ( i = 0 ; i < q_info.vcount ; i++ )
           { 
             REAL wforce[MAXCOORD];  /* unwrapped forces */
             REAL *ff;
             v = q_info.v[i];
             if ( q_info.wraps[i] )
             {  (*sym_form_pullback)(q_info.x[i],wforce,q_info.grad[i],
                              q_info.wraps[i]);
                ff = wforce;
             }
             else ff = q_info.grad[i];
             if ( q->flags & Q_ENERGY )
             { int procnum = v_procnum[loc_ordinal(v)]; 
               if ( procnum == me )
               { /* no conflicts */
                 f = get_force(v);
                 vector_add_smul(f,ff,-c, SDIM);
               }
               else /* have to save */
               { int newp = ptop[me]++;
                 struct procforce *pf;
                 if ( newp >= pmax[me] )
                 { pbase[me] = (struct procforce *)kb_realloc(
                       (char*)(pbase[me]),2*pmax[me]*sizeof(struct procforce));
                   pmax[me] = 2*pmax[me];
                 }
                 pf = pbase[me] + newp;
                 pf->v_id = v;
                 pf->next = phead[procnum][me];
                 phead[procnum][me] = newp;
                 for ( j = 0 ; j < SDIM ; j++ )
                    pf->f[j] = -c*ff[j];
               }
             }
             else 
             { M_LOCK(vgradbase);
               vgptr = get_bv_new_vgrad(q->fixnum,v);
               vgptr->bb_id = q->b_id;
               vgptr->qnum = mi->quant;
               vector_add_smul(vgptr->grad,ff, c, SDIM);
               M_UNLOCK(vgradbase);
             }
           }
        }
     }
  }
 ) /* end of macro */

  q_info_free(&q_info);

} /* m_calc_quant_grads */

/***********************************************************************
*
*  function: m_fix_grads()
*
*  purpose:  Processes add forces from other processes to own vertices.
*/
void m_fix_grads()
{ int me = GET_THREAD_ID;
  int pnum;
  int i,k;
  struct procforce *p;
  REAL *f;

  for ( pnum = 0 ; pnum < nprocs ; pnum++ )
  { for ( k = phead[me][pnum] ; k >= 0 ; k = p->next )
     { p = pbase[pnum] + k;
        f = get_force(p->v_id);
        for ( i = 0 ; i < SDIM ; i++ ) f[i] += p->f[i];
     }
  }
}
#endif

/***********************************************************************
*
*  function: calc_quant_grads()
*
*  purpose:  calculate gradients of quantities
*                Called by vol_project(), which does all other 
*                structure initializing and  messing around.
*                Also calculates quantity values themselves.
*
*/

void calc_quant_grads(mode)
int mode; /* energy or constraint, or Q_COMPOUND for aiding hessian */
{ int i,k;
  struct element *e_ptr;
  struct gen_quant *q = NULL;
  volgrad *vgptr;  /* constraint gradients */
  int flag;  /* 0 if doing global quantities, 1 for local */
  vertex_id v;
  int type;  /* element type */
  struct method_instance  *mi;
  struct gen_quant_method *gm;
  int inum;
  struct qinfo q_info;  /* data passing structure */
  int todo = 0; /* whether any to do */
  int global_needs;
#ifdef PROFILING_ENABLED
  __int32 grad_elapsed_time[2];
#endif

  PROF_START(calc_quant_grads);

  /* method initialization */
  comp_quant_stamp = 0;
  for ( type = 0 ; type < NUMELEMENTS ; type++ ) quant_flags[type] = 0;
  for ( k = LOW_INST  ; k < meth_inst_count  ; k++ )
  { mi = METH_INSTANCE(k); /* since some init may have moved things */
    mi->stamp = 0;  /* so only current methods used in eval_all() */
    gm = basic_gen_methods + mi->gen_method;
    if ( mi->quant >= 0 ) q = GEN_QUANT(mi->quant); else q = NULL;
    if ( !q || (q->modulus==0.0) || !(q->flags & mode & ~Q_COMPOUND) ||
            (mi->modulus==0.0) )
      mi->flags &= ~Q_DOTHIS;
    else
    { mi->flags |= Q_DOTHIS;
      mi->newvalue = 0.0;
      mi->abstotal = 0.0;
      mi->timestamp = global_timestamp;
      quant_flags[basic_gen_methods[mi->gen_method].type] |= mode;
      if ( gm->init ) 
              (*gm->init)(METHOD_GRADIENT,mi);
      todo = 1;
    }
  }
  if ( !todo ) 
  { PROF_FINISH(calc_quant_grads);
    return;
  }

  q_info_init(&q_info,METHOD_GRADIENT); 

   
  if ( compound_quant_list_head >= 0 )
    { for (  k=LOW_INST ; k < meth_inst_count ; k++ )
      { mi = METH_INSTANCE(k);
        if ( mi->flags & Q_COMPOUND )
        { mi->grad = dmatrix(0,MAXVCOUNT,0,SDIM);
        }
      }
    }

  for ( type = VERTEX ; type <= BODY ; type++ )
  if ( quant_flags[type] & mode )
  { int meth_offset = get_meth_offset(type); 
  
    global_needs = global_meth_needs(type);
    
#if defined(SHARED_MEMORY)
    if ((compound_quant_list_head == -1) && ((nprocs > 1) || threadflag) ) 
    { 
      if ( el_list_timestamp[VERTEX] < top_timestamp )
         make_el_list(VERTEX);  /* need for force */
      if ( el_list_timestamp[type] < top_timestamp )
         make_el_list(type);
      for ( k = LOW_INST  ; k < meth_inst_count  ; k++ )
      { mi = METH_INSTANCE(k); 
         if ( mi->flags & Q_DOTHIS )
          for ( i = 0 ; i < nprocs ; i++ )
          {  mi->procvalue[i] = 0.0;
             mi->procabstotal[i] = 0.0;
          }
      }
      for ( i = 0 ; i < nprocs ; i++ ) proc_total_area[i] = 0.0;
      for ( i = 0 ; i < nprocs ; i++ )
      { int j;
         ptop[i] = 0 ;
         if ( pbase[i] == NULL )
         { pmax[i] = 1000;
           pbase[i] = (struct procforce *)mycalloc(pmax[i],
              sizeof(struct procforce));
         }
         for ( j = 0 ; j < nprocs ; j++ ) phead[i][j] = -1;
      }
#ifdef SGI_MULTI
      if ( mpflag == M_INACTIVE ) m_rele_procs();  /* resume parked procs */
      mpflag = M_ACTIVE;
      m_fork(m_calc_quant_grads,type,mode);
      m_fork(m_fix_grads,0,0);
      m_park_procs();
      mpflag = M_INACTIVE; 
#endif
#ifdef THREADS
      m_type = type;
      thread_launch(TH_MULTI_QUANT_GRADS,type);
      thread_launch(TH_FIX_GRADS,0);
#endif

      /* sum separate process values */
      for ( k = LOW_INST  ; k < meth_inst_count  ; k++ )
      { mi = METH_INSTANCE(k); 
         if ( mi->flags & Q_DOTHIS )
          for ( i = 0 ; i < nprocs ; i++ )
          { mi->newvalue += mi->procvalue[i];
            mi->abstotal += mi->procabstotal[i];
          }
      }
      for ( i = 0 ; i < nprocs ; i++ )
         web.total_area += proc_total_area[i];
    }
    else
#endif
    /* Don't put anything here; non-shared falls through!! */
    
    FOR_ALL_ELEMENTS(type,q_info.id)
    { int j;
      int setup_flag = 0;
      int needs;

      e_ptr = elptr(q_info.id);
      /* get setup flags */
      needs = global_needs;
      for ( k = 0 ; k < e_ptr->method_count ; k++ )
      { int mm;
        mm = ((int*)((char*)e_ptr+meth_offset))[k];
        mi = METH_INSTANCE(abs(mm));
        if ( (mi->flags & Q_DOTHIS) && (mi->type == type) )
          needs |= basic_gen_methods[mi->gen_method].flags;
      }
      inum = global_meth_inst_count[type];
      ++comp_quant_stamp;
      for ( flag = 0 ; flag < 2 ; flag++,inum = e_ptr->method_count,k=0 )
      { for ( k = 0 ; k < inum ; k++ )
        { int mm;
          int sign = 1;
          if ( flag ) 
          { mm = ((int*)((char*)e_ptr+meth_offset))[k];
            q_info.method = abs(mm);
            if ( mm < 0 ) sign = -1;
          }
          else  q_info.method = global_meth_inst[type][k];
          mi = METH_INSTANCE(q_info.method);
         if ( mi->quant >= 0 ) q = GEN_QUANT(mi->quant);
          if ( (mi->flags & Q_DOTHIS) && (mi->type == type) )
          { REAL c = 0;  /* net coefficient */
            REAL value;
            gm = basic_gen_methods + mi->gen_method;
            PROF_START(element_setup);
            if ( !setup_flag ) 
            { (*q_setup[type])(NULL,&q_info,needs); setup_flag = 1; }
            PROF_FINISH(element_setup);
            mi = METH_INSTANCE(q_info.method);
            METHOD_PROFILING_START(mi,grad);
            for ( i = 0 ; i < q_info.vcount ; i++ ) /* methods don't know */
              for ( j = 0 ; j < SDIM ; j++ )          /* how many */
                  q_info.grad[i][j] = 0.0;
            value = (*gm->gradient)(&q_info);
            if ( mi->flags & ELEMENT_MODULUS_FLAG )
            { REAL emdls = *(REAL*)get_extra(q_info.id,mi->elmodulus);
              value *= emdls;
              for ( i = 0 ; i < q_info.vcount ; i++ )
                for ( j = 0 ; j < SDIM ; j++ )
                  q_info.grad[i][j] *= emdls;
            }
            mi->newvalue += sign*value;
            mi->abstotal += fabs(value);
            if ( mi->flags & Q_COMPOUND ) 
            {
              for ( i = 0 ; i < q_info.vcount ; i++ )
              { for ( j = 0 ; j < SDIM ; j++ )
                  mi->grad[i][j] = mi->modulus*q_info.grad[i][j];
              }
              mi->stamp = comp_quant_stamp;
            }
            if ( mi->quant >= 0 )
               c = sign*mi->modulus*q->modulus;
            for ( i = 0 ; i < q_info.vcount ; i++ )
            { 
              REAL wforce[MAXCOORD];  /* unwrapped forces */
              REAL *ff;
              v = q_info.v[i];
              if ( q_info.wraps[i] )
              { (*sym_form_pullback)(q_info.x[i],wforce,q_info.grad[i],
                                 q_info.wraps[i]);
                ff = wforce;
              }
              else ff = q_info.grad[i];
              if ( mi->quant >= 0 && !(mi->flags & Q_COMPOUND) )
              { if ( q->flags & Q_ENERGY & mode )
                { REAL *f = get_force(v);
                  vector_add_smul(f,ff, -c, SDIM);
                }
                else if ( q->flags & (Q_FIXED|Q_CONSERVED) & mode )
                { vgptr = get_bv_new_vgrad(q->fixnum,v);
                  vgptr->bb_id = q->b_id;
                  vgptr->qnum = mi->quant;
                  vector_add_smul(vgptr->grad,ff, c, SDIM);
                }
              }
              if ( mode & mi->flags & Q_COMPOUND )
              { 
                  vgptr = get_bv_new_vgrad(mi->self_id,v);
                  vgptr->bb_id = mi->self_id;
                  vgptr->qnum = mi->self_id;
                  vector_add_smul(vgptr->grad,ff, mi->modulus, SDIM);
                 
              }
            }
            METHOD_PROFILING_END(mi,grad);
          }
        }
      }  /* end calculation of all method instances */

      /* check out compound quantities */
      for ( k = compound_quant_list_head ; k >= 0 ; k = q->next_compound )
      { q = GEN_QUANT(k);
        if ( (q->flags & mode) && (q->flags & Q_COMPOUND) ) 
        { for ( i = 0 ; i < q_info.vcount ; i++ )
          { REAL dummy,partials[MAXCOORD];
            comp_quant_vertex = i; /* so eval_all knows */ 
            eval_all(&q->expr,NULL,SDIM,&dummy,partials,NULLID);
            v = q_info.v[i];
            if ( q->flags & Q_ENERGY )
            { REAL *f = get_force(v);
              vector_add_smul(f,partials, -q->modulus, SDIM);
            }
            else 
            { 
              vgptr = get_bv_new_vgrad(q->fixnum,v);
              vgptr->bb_id = q->b_id;
              vgptr->qnum = k;
              vector_add_smul(vgptr->grad,partials, q->modulus, SDIM);
            }
          }
        }
      }
    }
  }

  for ( k = LOW_INST  ; k < meth_inst_count  ; k++ )
  { mi = METH_INSTANCE(k);
    if ( mi->flags & Q_DOTHIS )
    {  mi->value = mi->modulus*mi->newvalue;
       mi->abstotal *= fabs(mi->modulus);
    }
  }

  /* combine methods to quantities */
  for ( k = 0 ; k < gen_quant_count ; k++ )
  { q = GEN_QUANT(k);
    if ( q->flags & mode ) 
    { q->value = q->volconst;
      q->abstotal = 0.0;
      q->timestamp = global_timestamp;
    }
  }
  for ( k = LOW_INST  ; k < meth_inst_count  ; k++ )
  { mi = METH_INSTANCE(k);
    if ( mi->quant < 0 ) continue;
    q = GEN_QUANT(mi->quant);
    if ( q->flags & mode ) 
    { q->value += q->modulus*mi->value;
      q->abstotal += fabs(q->modulus)*mi->abstotal;
    }
  }

  for ( k = 0 ; k < gen_quant_count ; k++ )
  { q = GEN_QUANT(k);
    if ( q->flags & mode ) 
      if ( q->flags & Q_COMPOUND )
      {  q->value = q->modulus*eval(&q->expr,NULL,NULLID,NULL);
         q->abstotal = 1.0;
      }
  }

  if ( (mode & Q_FIXED) && web.torus_flag )
  { /* munge fixed volumes modulo torus volume */
    for (  k = 0 ; k < gen_quant_count ; k++ )
    { q = GEN_QUANT(k);
      if ( (q->flags & Q_FIXED) 
              && (q->flags & TORUS_MODULO_MUNGE) ) 
          q->value -= q->modulus*web.torusv*
                  (int)(0.5+(q->value - q->target)/(q->modulus*web.torusv));
    }
  }

  /* take care of any cleanup */
  for ( k = LOW_INST  ; k < meth_inst_count  ; k++ )
  { mi = METH_INSTANCE(k); /* since some init may move things */
    gm = basic_gen_methods + mi->gen_method;
    if ( gm->cleanup && mi->flags & Q_DOTHIS ) gm->cleanup();
  }

  if ( compound_quant_list_head >= 0 )
    for ( k = LOW_INST ; k < meth_inst_count ; k++ )
    { mi = METH_INSTANCE(k);
      if ( mi->flags & Q_COMPOUND )
      { free_matrix(mi->grad); mi->grad = NULL;
      }
    }

  q_info_free(&q_info);

  PROF_FINISH(calc_quant_grads);
} /* end calc_quant_grads() */


/***********************************************************************
*
*  function: calc_quant_hess()
*
*  purpose:  calculate hessians of quantities
*
*/

void calc_quant_hess(S,rhs_mode,hess_mode,rhs)
struct linsys *S; /* to gather hessian */
int rhs_mode;    /* whether to do rhs */
int hess_mode;    /* 1 for full hessian */
REAL *rhs;
{ int i,ii,k,j,jj,m,n;
  struct element *e_ptr;
  struct gen_quant *q = NULL;
  int flag;  /* 0 if doing global quantities, 1 for local */
  int type;  /* element type */
  struct method_instance  *mi;
  struct gen_quant_method *gm;
  int inum; 
  REAL g[MAXCOORD],*ggg;
  REAL **p1,**p2;
  struct hess_verlist *va,*vb;
  int mode = Q_ENERGY|Q_FIXED|Q_CONSERVED;
  REAL coeff; /* net modulus, including pressure if fixed quant */
  REAL ccoeff=0.0; /* net modulus */
  struct qinfo q_info;  /* data passing structure */
  int todo = 0;
  MAT2D(seconds,2*MAXCOORD,2*MAXCOORD);
  int global_needs;
#ifdef PROFILING_ENABLED
  __int32 hess_elapsed_time[2];
#endif

  PROF_START(calc_quant_hess);

  if ( compound_quant_list_head >= 0 )
  {
    /* get total gradients for compound quantities */
    vgrad_end();
    vgrad_init(1);
    calc_quant_grads(Q_COMPOUND|Q_FIXED|Q_ENERGY|Q_CONSERVED);
	if ( quantity_function_sparse_flag )
		add_vgrads_to_update(S);
    compound_hess_flag = CH_GRADS;
    for ( k = 0 ; k < gen_quant_count ; k++ )
    { q = GEN_QUANT(k);
      if ( !(q->flags & Q_COMPOUND ) ) continue;
      if ( q->flags & (Q_ENERGY|Q_FIXED|Q_CONSERVED) )
      { REAL coeff = q->modulus;
        if ( q->flags & (Q_FIXED|Q_CONSERVED) ) coeff *= -q->pressure; 

        if ( quantity_function_sparse_flag )
        { /* second partials of quantity wrt methods */
          REAL dummy,partials[2*MAXCOORD];
		  eval_second(&q->expr,NULL,q->method_count,&dummy,partials,seconds,NULLID);
          for ( i = 0 ; i < q->method_count ; i++ )
          { int ii = METH_INSTANCE(q->meth_inst[i])->global_low_rank;
            for ( j = 0 ; j < q->method_count ; j++ )   
            { int jj = METH_INSTANCE(q->meth_inst[j])->global_low_rank;
              S->low_rank_form[ii][jj] += coeff*seconds[i][j];
            }
          }
        }
        else /* old dense way */
        FOR_ALL_VERTICES(comp_quant_vi)
        { REAL dummy,partials[2*MAXCOORD];
          if ( get_vattr(comp_quant_vi) & FIXED ) continue;
          FOR_ALL_VERTICES(comp_quant_vj)
          { 
            if ( comp_quant_vi > comp_quant_vj ) continue;
            if ( get_vattr(comp_quant_vj) & FIXED ) continue;
            eval_second(&q->expr,NULL,2*SDIM,&dummy,partials,seconds,NULLID);
            for ( m = 0 ; m < SDIM ; m++ )
             for ( n = 0 ; n < SDIM ; n++ )
              seconds[m+SDIM][n] *= coeff;
            fill_mixed_entry(S,comp_quant_vj,comp_quant_vi,seconds+SDIM);
          }
        }

      }
    }
  }
  compound_hess_flag = 0;
  comp_quant_stamp = 0;

  /* method initialization */
  for ( type = 0 ; type < NUMELEMENTS ; type++ ) quant_flags[type] = 0;
  for ( k = LOW_INST  ; k < meth_inst_count  ;k++ )
    { mi = METH_INSTANCE(k);
      mi->stamp = 0;  /* so only current methods used in eval_all() */
      gm = basic_gen_methods + mi->gen_method;
      if ( mi->quant >= 0 ) q = GEN_QUANT(mi->quant);
      if ( (!(mi->flags&Q_COMPOUND) && (mi->quant >= 0) &&  
                        ((q->modulus == 0.0) || !(q->flags & mode)))
                  || (mi->modulus == 0.0) || 
             ((mi->quant >= 0) && (q->flags & Q_REDUNDANT)) )
         mi->flags &= ~Q_DOTHIS;
      else
      { mi->flags |= Q_DOTHIS;
        if ( (gm->hessian == NULL) || (gm->hessian == null_q_hess) )
        { sprintf(errmsg,"Method %s has no Hessian available.\n",gm->name);
          kb_error(1571,errmsg,RECOVERABLE);
        }
        mi->newvalue = 0.0;
        mi->abstotal = 0.0;
        mi->timestamp = global_timestamp;
        quant_flags[basic_gen_methods[mi->gen_method].type] |= mode;
        if ( gm->init ) 
              (*gm->init)(METHOD_HESSIAN,mi);
        todo = 1;
      }
    }
  if ( !todo )
  { PROF_FINISH(calc_quant_hess);
    return;
  }

  /* set up matrices in qinfo with lots of room */
  q_info_init(&q_info,METHOD_HESSIAN); 
  q_info.hess = dmatrix4(MAXVCOUNT,MAXVCOUNT,SDIM,SDIM);
  p1 = dmatrix(0,MAXCOORD-1,0,MAXCOORD-1);
  p2 = dmatrix(0,MAXCOORD-1,0,MAXCOORD-1);

  if ( compound_quant_list_head >= 0 )
    {  
      for (  k=LOW_INST ; k < meth_inst_count ; k++ )
      { mi = METH_INSTANCE(k);
        if ( mi->flags & Q_COMPOUND )
        { mi->grad = dmatrix(0,MAXVCOUNT,0,SDIM);
          mi->hess = dmatrix4(MAXVCOUNT,MAXVCOUNT,SDIM,SDIM);
        }
      }
    }

  for ( type = VERTEX ; type <= BODY ; type++ )
  if ( quant_flags[type] & mode )
  { int meth_offset = get_meth_offset(type); 
#if defined(SHARED_MEMORY)
    if ( (compound_quant_list_head == -1) && ((nprocs > 1) || threadflag) ) 
    { 
      if ( el_list_timestamp[VERTEX] < top_timestamp )
         make_el_list(VERTEX);  /* need for force */
      if ( el_list_timestamp[type] < top_timestamp )
         make_el_list(type);
      for ( k = LOW_INST  ; k < meth_inst_count  ; k++ )
      { mi = METH_INSTANCE(k); 
         if ( mi->flags & Q_DOTHIS )
          for ( i = 0 ; i < nprocs ; i++ )
          {  mi->procvalue[i] = 0.0;
             mi->procabstotal[i] = 0.0;
          }
      }
      m_hess_mode = hess_mode;
      m_rhs_mode  = rhs_mode;
#ifdef SGI_MULTI
      if ( mpflag == M_INACTIVE ) m_rele_procs();  /* resume parked procs */
      mpflag = M_ACTIVE;
      m_quanrowstart = S->quanrowstart;
      m_bodyrowstart = S->bodyrowstart;
      m_fork(m_calc_quant_hess,type,mode,rhs);
      m_park_procs();
      mpflag = M_INACTIVE; 
#endif
#ifdef THREADS
      m_type = type;
      m_mode = mode;
      m_rhs  = rhs;
      thread_launch(TH_MULTI_QUANT_HESS,type);
#endif
      m_fix_hess(S);
      /* sum separate process values */
      for ( k = LOW_INST  ; k < meth_inst_count  ; k++ )
      { mi = METH_INSTANCE(k); 
         if ( mi->flags & Q_DOTHIS )
          for ( i = 0 ; i < nprocs ; i++ )
          { mi->newvalue += mi->procvalue[i];
            mi->abstotal += mi->procabstotal[i];
          }
      }
    }
    else
#endif
   {
   
    global_needs = global_meth_needs(type);
    FOR_ALL_ELEMENTS(type,q_info.id)
    { int setup_flag = 0;
      int needs;
      ++comp_quant_stamp;
      e_ptr = elptr(q_info.id);
      /* get setup flags */
      needs = global_needs;
      for ( k = 0 ; k < e_ptr->method_count ; k++ )
      { int mm;
        mm = ((int*)((char*)e_ptr+meth_offset))[k];
        mi = METH_INSTANCE(abs(mm));
        if ( (mi->flags & Q_DOTHIS) && (mi->type == type) )
        needs |= basic_gen_methods[mi->gen_method].flags;
      }
      inum = global_meth_inst_count[type];
      for ( flag = 0 ; flag < 2 ; flag++,inum = e_ptr->method_count,k=0 )
        for ( k = 0 ; k < inum ; k++ )
        { int mm;
          int sign = 1;
          if ( flag ) 
          { mm = ((int*)((char*)e_ptr+meth_offset))[k];
            q_info.method = abs(mm);
            if ( mm < 0 ) sign = -1;
          }
          else  q_info.method = global_meth_inst[type][k];
          mi = METH_INSTANCE(q_info.method);
          if ( (mi->flags & Q_DOTHIS) && (mi->type == type) )
          { REAL value;
            if ( mi->quant >= 0 ) q = GEN_QUANT(mi->quant);
            PROF_START(element_setup);
            if ( !setup_flag ) 
            { (*q_setup[type])(S,&q_info,needs); setup_flag = 1; }
            PROF_FINISH(element_setup);
            coeff = sign*q->modulus*mi->modulus;
            if ( q->flags & (Q_FIXED|Q_CONSERVED) )
             { ccoeff = coeff; coeff *= -q->pressure; }
            mi = METH_INSTANCE(q_info.method);
            METHOD_PROFILING_START(mi,hess);
            gm = basic_gen_methods + mi->gen_method;
            zerohess(&q_info);
            if ( hess_mode ) value = (*gm->hessian)(&q_info);
            else value = (*gm->gradient)(&q_info);
            if ( mi->flags & ELEMENT_MODULUS_FLAG )
            { REAL emdls = *(REAL*)get_extra(q_info.id,mi->elmodulus);
              value *= emdls;
              for ( i = 0 ; i < q_info.vcount ; i++ )
                for ( j = 0 ; j < SDIM ; j++ )
                  q_info.grad[i][j] *= emdls;
              for ( i = 0 ; i < q_info.vcount ; i++ )
               for ( ii = 0 ; ii < q_info.vcount ; ii++ )
                for ( j = 0 ; j < SDIM ; j++ )
                 for ( jj = 0 ; jj < SDIM ; jj++ )
                   q_info.hess[i][ii][j][jj] *= emdls;
            }
            mi->newvalue += sign*value;
            mi->abstotal += fabs(value);

            /* unwrap */
            if ( sym_flags & NEED_FORM_UNWRAPPING )
            { /* gradient */
              REAL grad[MAXCOORD];
              for ( i = 0 ; i < q_info.vcount ; i++ )
              { if ( q_info.wraps[i] )
                { (*sym_form_pullback)(q_info.x[i],grad,q_info.grad[i],
                                  q_info.wraps[i]);
                  for ( j = 0 ; j < SDIM ; j++ ) q_info.grad[i][j] = grad[j];
                }
              }
              if ( hess_mode )
              { for ( i = 0 ; i < q_info.vcount ; i++ )
                for ( ii = 0 ; ii < q_info.vcount ; ii++ )
                { if ( q_info.wraps[i] )
                    for ( jj = 0 ; jj < SDIM ; jj++ )
                    { REAL tmp[MAXCOORD];
                      for ( j = 0 ; j < SDIM ; j++ ) 
                            tmp[j]=q_info.hess[i][ii][j][jj];
                      (*sym_form_pullback)(q_info.x[i],grad,tmp,q_info.wraps[i]);
                      for ( j = 0 ; j < SDIM ; j++ )
                        q_info.hess[i][ii][j][jj] = grad[j];
                    }
                  if ( q_info.wraps[ii] )
                    for ( j = 0 ; j < SDIM ; j++ )
                    { (*sym_form_pullback)(q_info.x[ii],grad,
                           q_info.hess[i][ii][j],q_info.wraps[ii]);
                      for ( jj = 0 ; jj < SDIM ; jj++ )
                         q_info.hess[i][ii][j][jj] = grad[jj];
                    }
                 }
               }
             }

                
            if ( mi->flags & Q_COMPOUND ) 
            { mi->vlist = q_info.v; /* for eval_sec */
              for ( i = 0 ; i < q_info.vcount ; i++ )
                 for ( m = 0 ; m < SDIM ; m++ )
                    mi->grad[i][m] = mi->modulus*q_info.grad[i][m];
              for ( i = 0 ; i < q_info.vcount ; i++ )
                for ( j = 0 ; j < q_info.vcount ; j++ )
                 for ( m = 0 ; m < SDIM ; m++ )
                  for ( n = 0 ; n < SDIM ; n++ )
                    mi->hess[i][j][m][n] = mi->modulus*q_info.hess[i][j][m][n];
              mi->stamp = comp_quant_stamp;
            }
           else
           {
             for ( i = 0 ; i < q_info.vcount ; i++ )
             { REAL grad[MAXCOORD];
               va = get_vertex_vhead(q_info.v[i]);
               for ( j = 0 ; j < SDIM ; j++ )
                    grad[j] = coeff*q_info.grad[i][j];
               fill_grad(S,va,grad,rhs);
             }

             /* second derivatives */
             if ( hess_mode && (mode & (Q_FIXED|Q_ENERGY|Q_CONSERVED))  )
               for ( i = 0 ; i < q_info.vcount ; i++ )
                 { va = get_vertex_vhead(q_info.v[i]);
                   if ( va->freedom == 0 ) continue;

                   for ( j = i ; j < q_info.vcount ; j++ )
                   { vb = get_vertex_vhead(q_info.v[j]);
                     if ( vb->freedom == 0 ) continue;
                     for ( n = 0 ; n < SDIM ; n++ )
                       for ( m = 0 ; m < SDIM ; m++ )
                         q_info.hess[i][j][m][n] *= coeff;
                     fill_mixed_entry(S,q_info.v[i],q_info.v[j],q_info.hess[i][j]);

                     if ( (i != j) && (q_info.v[i] == q_info.v[j]) ) /* also transpose */
                     { MAT2D(transpose,MAXCOORD,MAXCOORD);
                       for ( n = 0 ; n < SDIM ; n++ )
                         for ( m = 0 ; m < SDIM ; m++ )
                           transpose[m][n] = q_info.hess[i][j][n][m];
                       fill_mixed_entry(S,q_info.v[i],q_info.v[j],transpose);
                     }
                   } /* end inner vertex loop */

                   /* fixed quantity gradients for left side */
                   if ( q->flags & (Q_FIXED|Q_CONSERVED) )
                   { /* find entry */
                     int currentrow;
                     if ( va->proj )
                     { vec_mat_mul(q_info.grad[i],va->proj,g,SDIM,
                                    va->freedom);
                       ggg = g;
                     }
                     else ggg = q_info.grad[i];
                     currentrow = S->quanrowstart + mi->quant;
                     for ( m = 0 ; m < va->freedom ; m++ )
                     { sp_hash_search(S,va->rownum+m,currentrow,ccoeff*ggg[m]);
                     }
                   }
                 } /* end second derivatives */
               } /* end non-compound */
               METHOD_PROFILING_END(mi,hess);
            } /* end if */ 
        } /* end method instance loop */

        /* Here we take care of compound quantities */
        for ( k = compound_quant_list_head ; k >= 0 ; k = q->next_compound )
        { q = GEN_QUANT(k);
          if ( (q->flags & mode) && (q->flags & Q_COMPOUND) ) 
          {
            for ( i = 0 ; i < q_info.vcount ; i++ )
            { REAL dummy,partials[MAXCOORD];
              REAL grad[MAXCOORD];
              comp_quant_vertex = i; /* so eval_all knows */ 
              comp_quant_type = type;
              eval_all(&q->expr,NULL,SDIM,&dummy,partials,NULLID);
              va = get_vertex_vhead(q_info.v[i]);
              for ( j = 0 ; j < SDIM ; j++ )
                   grad[j] = q->modulus*partials[j];
              fill_grad(S,va,grad,rhs);

              /* fixed quantity gradients for left side */
              if ( (q->flags & (Q_FIXED|Q_CONSERVED)) && hess_mode )
              { /* find entry */
                int currentrow;
                if ( va->proj )
                { vec_mat_mul(grad,va->proj,g,SDIM, va->freedom);
                  ggg = g;
                }
                else ggg = grad;
                currentrow = S->quanrowstart + k;
                for ( m = 0 ; m < va->freedom ; m++ )
                { sp_hash_search(S,va->rownum+m,currentrow,q->modulus*ggg[m]);
                }
              }
            }

            /* second derivatives */
            compound_hess_flag = CH_HESS;
            if ( hess_mode )
              for ( i = 0 ; i < q_info.vcount ; i++ )
              { REAL dummy[MAXCOORD];
                REAL partials[MAXCOORD];
                va = get_vertex_vhead(q_info.v[i]);
                if ( va->freedom == 0 ) continue;

                for ( j = i ; j < q_info.vcount ; j++ )
                { vb = get_vertex_vhead(q_info.v[j]);
                  if ( vb->freedom == 0 ) continue;
                  comp_quant_vertexi = i; /* so eval_all knows */ 
                  comp_quant_vertexj = j; /* so eval_all knows */ 
                  eval_second(&q->expr,NULL,SDIM,dummy,partials,seconds,NULLID);
                  for ( n = 0 ; n < SDIM ; n++ )
                    for ( m = 0 ; m < SDIM ; m++ )
                      q_info.hess[i][j][m][n] = q->modulus*seconds[m][n];
                  fill_mixed_entry(S,q_info.v[i],q_info.v[j],q_info.hess[i][j]);

                  if ( (i != j) && (q_info.v[i] == q_info.v[j]) ) /* also transpose */
                  { MAT2D(transpose,MAXCOORD,MAXCOORD);
                    for ( n = 0 ; n < SDIM ; n++ )
                      for ( m = 0 ; m < SDIM ; m++ )
                        transpose[m][n] = q_info.hess[i][j][n][m];
                    fill_mixed_entry(S,q_info.v[i],q_info.v[j],transpose);
                  }
                } /* end inner vertex loop */

              } /* end second derivatives */
              compound_hess_flag = 0;
           }
         } /* end compound quantities */
      } /* end all element loop */
   }
  }  /* end single thread */
  
  /* free stuff */
  free_matrix4(q_info.hess);
  free_matrix(p1);
  free_matrix(p2);
  q_info_free(&q_info);
  if ( compound_quant_list_head >= 0 )
  for ( k = LOW_INST ; k < meth_inst_count ; k++ )
  { mi = METH_INSTANCE(k);
    if ( mi->flags & Q_COMPOUND )
    { free_matrix(mi->grad); mi->grad = NULL;
      free_matrix4(mi->hess); mi->hess = NULL;
    }
  }

  for ( k = LOW_INST ; k < meth_inst_count ; k++ )
  { mi = METH_INSTANCE(k);
    if ( mi->flags & Q_DOTHIS )
    {  mi->value = mi->modulus*mi->newvalue;
       mi->abstotal *= fabs(mi->modulus);
    }
  }

  /* combine methods to quantities */
  for (  k = 0 ; k < gen_quant_count ; k++ )
  { q = GEN_QUANT(k);
    if ( q->flags & mode ) 
    { q->value = q->volconst;
      q->abstotal = 0.0;
      q->timestamp = global_timestamp;
    }
  }
  for ( k = LOW_INST  ; k < meth_inst_count  ; k++ )
    { mi = METH_INSTANCE(k);
      if ( mi->flags & Q_COMPOUND ) continue;
      gm = basic_gen_methods + mi->gen_method;
      if ( mi->quant >= 0 ) q = GEN_QUANT(mi->quant);
      if ( (mi->quant >= 0) && (q->flags & mode) ) 
      { q->value += q->modulus*mi->value;
        q->abstotal += fabs(q->modulus)*mi->abstotal;
      }
    }
  for ( k = compound_quant_list_head ; k >= 0 ; k = q->next_compound )
  { q = GEN_QUANT(k);
    if ( (q->flags & mode) && ( q->flags & Q_COMPOUND ) )
    {  q->value = q->modulus*eval(&q->expr,NULL,NULLID,NULL);
       q->abstotal = 1.0;
    }
  }

  if ( (mode & Q_FIXED) && web.torus_flag )
  { /* munge fixed volumes modulo torus volume */
    for ( k = 0 ; k < gen_quant_count ; k++ )
    { q = GEN_QUANT(k);
      if ( (q->flags & Q_FIXED) 
             && (q->flags & TORUS_MODULO_MUNGE) ) 
          q->value -= q->modulus*web.torusv*
                    (int)(0.5+(q->value - q->target)/(q->modulus*web.torusv));
    }
  }

  /* take care of any cleanup */
  for ( k = LOW_INST  ; k < meth_inst_count  ; k++ )
  { mi = METH_INSTANCE(k); /* since some init may move things */
    gm = basic_gen_methods + mi->gen_method;
    if ( gm->cleanup && mi->flags & Q_DOTHIS ) gm->cleanup();
  }

  vgrad_end();
 
  PROF_FINISH(calc_quant_hess);
}

#if defined(SHARED_MEMORY)
/* SGI multiple processor stuff */

/**********************************************************************
    Hash list routines for Hessian matrix entries.
    Key is (row,col).
**********************************************************************/

#define PRIME 99991
#define hash(row,col)  (abs((row)*97+(col)*PRIME))
static int m_max_fill[MAXPROCS];     /* max number of entries until enlarge */
static int m_hashcount[MAXPROCS];    /* current number of entries */
static int m_hash_per_row[MAXPROCS];    /* estimate size of table */
static int m_hash_extraprobes; /* for measuring efficiency */
static int m_bodyrowstart,m_quanrowstart,m_total_rows;
struct hess_entry *m_hashtable[MAXPROCS];  /* the table */
int m_table_size[MAXPROCS];  /* hashtable size */
void m_hess_hash_search(int,int,REAL,int);
void m_hess_hash_init(void);
void m_hess_hash_expand(void);

/********************************************************************
* 
* function: m_hess_hash_init()
*
* purpose: Initialize hash table.
*/
void m_hess_hash_init()
{ int i;
  int me = GET_THREAD_ID;

  if ( m_hash_per_row[me] < 1 ) m_hash_per_row[me] = 1;
  m_table_size[me] = m_hash_per_row[me]*SDIM*web.skel[VERTEX].max_ord/nprocs;
  m_max_fill[me] = 4*m_table_size[me]/5;
  if ( !hessian_quiet_flag )
  { sprintf(msg,"m_Hess init alloc: %d\n",m_table_size[me]);
     outstring(msg);
  }
  m_hashcount[me] = 0;
  if ( m_hashtable[me] ) temp_free((char*)m_hashtable[me]);
  m_hashtable[me] = 
     (struct hess_entry *)mycalloc(m_table_size[me],sizeof(struct hess_entry));
  for ( i = 0 ; i < m_table_size[me] ; i++ ) m_hashtable[me][i].row = HASHEMPTY;
  m_hash_extraprobes = 0;
}

/********************************************************************
* 
* function: m_hess_hash_expand()
*
* purpose: Expands hash table
*/

void m_hess_hash_expand()
{ struct hess_entry *newtable,*oldtable;
  int i;
  int me = GET_THREAD_ID;
  struct hess_entry *e;
  int newsize; 
  int oldsize = m_table_size[me];

  if ( !m_hashtable[me] ) m_hess_hash_init();
  newsize = m_table_size[me]*2; 
  oldtable = m_hashtable[me];
  newtable = 
     (struct hess_entry *)mycalloc(newsize,sizeof(struct hess_entry));
  for ( i = 0 ; i < newsize ; i++ ) newtable[i].row = HASHEMPTY;
  m_table_size[me] =  newsize;
  m_max_fill[me] = 4*m_table_size[me]/5;
  m_hashtable[me] = newtable;

  /* reinsert */
  m_hashcount[me] = 0;
  for ( i = 0, e = oldtable ; i < oldsize ; i++,e++ )
     if ( e->row != HASHEMPTY )
        m_hess_hash_search(e->col,e->row,e->value,me);
  myfree((char*)oldtable);
}

/********************************************************************
* 
* function: m_hess_hash_search()
*
* purpose: Finds existing entry or allocates entry.
*             Installs key values, and adds hessian value.
*/
void m_hess_hash_search(col,row,value,tid)
int row,col;  /* meant to do upper triangle */
REAL value;  /* value to add */
int tid;        /* thread id */
{
  struct hess_entry *e;
  int spot;
  struct hess_entry *hashtab;
  int tab_size = m_table_size[tid];

  if ( row > col ) return;

  if ( m_hashcount[tid] >= m_max_fill[tid] ) m_hess_hash_expand();
  hashtab = m_hashtable[tid];

  /* search hash table */
  spot = hash(row,col) % tab_size;
  e = hashtab + spot;
  while ( e->row != HASHEMPTY )
  { if ( (e->row == row) && (e->col == col) )
     { e->value += value; return; 
     }
     spot++;
     if ( spot >= tab_size ) spot -= tab_size;
     e = hashtab + spot;
     m_hash_extraprobes++;
  }
  /* if here, then have empty slot and need to insert */
  e->col = col; e->row = row;  m_hashcount[tid]++; 
  e->value = value;
}

/***********************************************************************
*
*  function: m_calc_quant_hess()
*
*  purpose:  calculate hessians of quantities
*                Called by calc_quant_hess() per processor
*
*/

void m_calc_quant_hess(type,mode,rhs)
int type;    /* element type */
int mode;    /* ENERGY, FIXED, or INFO_ONLY */
REAL *rhs;
{ int i,n,j,m,nn;
  struct element *e_ptr;
  struct gen_quant *q;
  int flag;  /* 0 if doing global quantities, 1 for local */
  struct gen_quant_method *gm;
  struct method_instance  *mi;
  int inum;
  struct qinfo q_info;  /* data passing structure */
  int meth_offset = get_meth_offset(type); 
  int me = GET_THREAD_ID;
  int start,end,bin;
  REAL **p1,**p2;
  REAL coeff,ccoeff=0.0;
  struct hess_verlist *va,*vb;
  REAL g[MAXCOORD],*ggg;
  int global_needs;

#ifdef SIGUSR1
  signal(SIGUSR1,catcher);    /* to catch user interrupt */ 
#endif
  signal(SIGINT,catcher);    /* to catch user interrupt */     
  m_breakflag[me] = 0;
  if ( setjmp(m_jumpbuf[me]) ) { q_info_free(&q_info); return; }

  /* set up matrices in qinfo with lots of room */
  q_info_init(&q_info,METHOD_HESSIAN); 
  q_info.hess = dmatrix4(MAXVCOUNT,MAXVCOUNT,SDIM,SDIM);
  p1 = dmatrix(0,MAXCOORD-1,0,MAXCOORD-1);
  p2 = dmatrix(0,MAXCOORD-1,0,MAXCOORD-1);

  /* initialize local list */
  m_hess_hash_init();

  global_needs = global_meth_needs(type);
  bin = (web.skel[type].count + nprocs - 1)/nprocs;
  start = me*bin;
  end = start + bin;
  if ( end > web.skel[type].count ) end = web.skel[type].count;
  for ( nn = start ; nn < end ; nn++ )
  { int k;
    int setup_flag = 0;
    int needs;

    q_info.id = el_list[type][nn]; 
    e_ptr = elptr(q_info.id);

    /* get setup flags */
    needs = global_needs;
    for ( k = 0 ; k < e_ptr->method_count ; k++ )
    { int mm;
      mm = ((int*)((char*)e_ptr+meth_offset))[k];
      mi = METH_INSTANCE(abs(mm));
      if ( (mi->flags & Q_DOTHIS) && (mi->type == type) )
        needs |= basic_gen_methods[mi->gen_method].flags;
    }

    inum = global_meth_inst_count[type];
    for ( flag = 0 ; flag < 2 ; flag++,inum = e_ptr->method_count,k=0 )
     for ( k = 0 ; k < inum ; k++ )
     { int mm,ii,jj;
       int sign = 1;
       REAL value;
       if ( flag ) 
       { mm = ((int*)((char*)e_ptr+meth_offset))[k];
         q_info.method = abs(mm);
         if ( mm < 0 ) sign = -1;
       }
       else  q_info.method = global_meth_inst[type][k];
       mi = METH_INSTANCE(q_info.method);
       if ( (mi->flags & Q_DOTHIS) && (mi->type == type) )
        { q = GEN_QUANT(mi->quant);
          coeff = sign*q->modulus*mi->modulus;
          if ( !setup_flag ) 
          { (*q_setup[type])(NULL,&q_info,needs); setup_flag = 1; }
          if ( q->flags & (Q_FIXED|Q_CONSERVED) )
           { ccoeff = coeff; coeff *= -q->pressure; }
          mi = METH_INSTANCE(q_info.method);
          gm = basic_gen_methods + mi->gen_method;
          zerohess(&q_info);

          if ( m_hess_mode ) value = (*gm->hessian)(&q_info);
          else value = (*gm->gradient)(&q_info); 
          if ( mi->flags & ELEMENT_MODULUS_FLAG )
          { REAL emdls = *(REAL*)get_extra(q_info.id,mi->elmodulus);
            value *= emdls;
            for ( i = 0 ; i < q_info.vcount ; i++ )
              for ( j = 0 ; j < SDIM ; j++ )
                q_info.grad[i][j] *= emdls;
            for ( i = 0 ; i < q_info.vcount ; i++ )
                 for ( ii = 0 ; ii < q_info.vcount ; ii++ )
              for ( j = 0 ; j < SDIM ; j++ )
               for ( jj = 0 ; jj < SDIM ; jj++ )
                 q_info.hess[i][ii][j][jj] *= emdls;
          }
          mi->newvalue += sign*value;
          mi->abstotal += fabs(value);

          /* unwrap */
          if ( sym_flags & NEED_FORM_UNWRAPPING )
          { /* gradient */
            int ii,jj;
            REAL grad[MAXCOORD];
            for ( i = 0 ; i < q_info.vcount ; i++ )
            {
               if ( q_info.wraps[i] )
               { (*sym_form_pullback)(q_info.x[i],grad,q_info.grad[i],
                                q_info.wraps[i]);
                  for ( j = 0 ; j < SDIM ; j++ ) q_info.grad[i][j] = grad[j];
               }
            }
            if ( m_hess_mode )
              for ( i = 0 ; i < q_info.vcount ; i++ )
               for ( ii = 0 ; ii < q_info.vcount ; ii++ )
               { if ( q_info.wraps[i] )
                   for ( jj = 0 ; jj < SDIM ; jj++ )
                   { REAL tmp[MAXCOORD];
                      for ( j = 0 ; j < SDIM ; j++ ) 
                        tmp[j]=q_info.hess[i][ii][j][jj];
                      (*sym_form_pullback)(q_info.x[i],grad,tmp,q_info.wraps[i]);
                      for ( j = 0 ; j < SDIM ; j++ )
                        q_info.hess[i][ii][j][jj] = grad[j];
                    }
                  if ( q_info.wraps[ii] )
                   for ( j = 0 ; j < SDIM ; j++ )
                   { 
                      (*sym_form_pullback)(q_info.x[ii],grad,q_info.hess[i][ii][j],
                                                   q_info.wraps[ii]);
                       for ( jj = 0 ; jj < SDIM ; jj++ )
                           q_info.hess[i][ii][j][jj] = grad[jj];
                   }
               }
          }

          for ( i = 0 ; i < q_info.vcount ; i++ )
            { REAL grad[MAXCOORD];
               va = get_vertex_vhead(q_info.v[i]);
               for ( j = 0 ; j < SDIM ; j++ )
                  grad[j] = coeff*q_info.grad[i][j];
               m_fill_grad(va,grad,me,rhs);
            }
          /* second derivatives */
          if ( m_hess_mode )
           for ( i = 0 ; i < q_info.vcount ; i++ )
            { va = get_vertex_vhead(q_info.v[i]);
               if ( va->freedom == 0 ) continue;

               for ( j = i ; j < q_info.vcount ; j++ )
               { vb = get_vertex_vhead(q_info.v[j]);
                 if ( vb->freedom == 0 ) continue;
                 for ( n = 0 ; n < SDIM ; n++ )
                    for ( m = 0 ; m < SDIM ; m++ )
                       q_info.hess[i][j][m][n] *= coeff;
                 m_fill_mixed_entry(q_info.v[i],q_info.v[j],
                      q_info.hess[i][j],me);
                 if ( (i != j) && (q_info.v[i] == q_info.v[j]) )
                    m_fill_mixed_entry(q_info.v[i],q_info.v[j],
                       q_info.hess[i][j],me);

               } /* end inner vertex loop */

               /* fixed quantity gradients for left side */
               if ( q->flags & (Q_FIXED|Q_CONSERVED) )
               { /* find entry */
                 int currentrow;
                 if ( va->proj )
                 { vec_mat_mul(q_info.grad[i],va->proj,g,SDIM,
                                va->freedom);
                   ggg = g;
                 }
                 else ggg = q_info.grad[i];
                 currentrow = m_quanrowstart + mi->quant;
                 for ( m = 0 ; m < va->freedom ; m++ )
                 { 
                    m_hess_hash_search(currentrow,va->rownum+m,
                           ccoeff*ggg[m],me);
                 }
               }
            } /* end second derivatives */
         } /* end if */ 
      } /* end method instance loop */
   } /* end all element loop */

  /* free stuff */
  free_matrix4(q_info.hess);
  free_matrix(p1);
  free_matrix(p2);
  q_info_free(&q_info);
}


/*************************************************************************
* 
* function: m_fill_grad()
*
* purpose: Process gradient of function at constraint 
*          Just stores in local list for future processing.
*/

void m_fill_grad(v,grad,tid,rhs)
struct hess_verlist *v;
REAL *grad;
REAL *rhs;
int tid; /* thread id */
{ REAL g[MAXCOORD];
  int k,a,b;

  if ( rhs_flag )
  { M_LOCK(rhs);
     if ( v->proj )
     { vec_mat_mul(grad,v->proj,g,SDIM,v->freedom);
        for ( k = 0 ; k < v->freedom ; k++ )
            rhs[v->rownum+k] -= g[k];
     }
     else
        for ( k = 0 ; k < v->freedom ; k++ )
            rhs[v->rownum+k] -= grad[k];
     M_UNLOCK(rhs);
  }

  if ( hess_flag && v->conhess )
  { 
     for ( a = 0 ; a < v->freedom ; a++ )
      for ( b = 0 ; b <= a ; b++ )
      { 
         m_hess_hash_search(v->rownum+a,v->rownum+b,
                  SDIM_dot(grad,v->conhess[a][b]),tid);
      }
  }
}


/*************************************************************************
*
* function: m_fill_mixed_entry()
*
* purpose: fill proper hessian matrix spot for mixed vertex second derivs
*             For multi-proc, just stores in local list.
*/

void m_fill_mixed_entry(v_id1,v_id2,mixed,tid)
vertex_id v_id1,v_id2;
REAL **mixed; /* full dim values */
int tid; /* thread id */
{ int k,j;
  REAL **oo;
  MAT2D(temp_mat,MAXCOORD,MAXCOORD);
  MAT2D(temp_mat2,MAXCOORD,MAXCOORD);
  struct hess_verlist *v1,*v2;
  
  v1 = get_vertex_vhead(v_id1); 
  v2 = get_vertex_vhead(v_id2);
  if ( v1->proj )
     { tr_mat_mul(v1->proj,mixed,temp_mat,SDIM,v1->freedom,SDIM);
        oo = temp_mat;
     }
  else oo = mixed;
  if ( v2->proj )
     { mat_mult(oo,v2->proj,temp_mat2,v1->freedom,SDIM,v2->freedom);
        oo = temp_mat2;
     }
  if ( v1->rownum < v2->rownum )
  for ( j = 0 ; j < v1->freedom ; j++ )
    for ( k = 0 ; k < v2->freedom ; k++ )
      m_hess_hash_search(v2->rownum+k,v1->rownum+j,oo[j][k],tid);
  else
  for ( j = 0 ; j < v1->freedom ; j++ )
    for ( k = 0 ; k < v2->freedom ; k++ )
      m_hess_hash_search(v1->rownum+j,v2->rownum+k,oo[j][k],tid);
}


/*******************************************************************
*
* function: m_fix_hess()
*
* purpose: stores local lists into regular hessian list.
*             THis version still serial!!
*/
void m_fix_hess(S)
struct linsys *S;
{ int p,i;

  for ( p = 0 ; p < nprocs ; p++ )
  { struct hess_entry *e;
     int end = m_table_size[p];
     for ( i = 0, e = m_hashtable[p] ; i < end ; i++,e++ )
        if ( e->row != HASHEMPTY )
          sp_hash_search(S,e->row,e->col,e->value);
     myfree((char*)(m_hashtable[p]));  m_hashtable[p] = NULL;
     m_hash_per_row[p] = 1 + (5*m_hashcount[p])/(4*SDIM*web.skel[VERTEX].max_ord);
       /* for next time */

     if ( !hessian_quiet_flag )
     { sprintf(msg,"m_hashcount[%d]: %d  m_table_size[%d]: %d\n",
          p,m_hashcount[p],p,m_table_size[p]);
        outstring(msg);
     }
  }
  if ( !hessian_quiet_flag )
  { sprintf(msg,"m_hash_extraprobes: %d\n",m_hash_extraprobes);
     outstring(msg);
  }
}
#endif

/*******************************************************************
*
*  function: q_vertex_setup()
* 
*  purpose:  calculate vertex attributes needed for quantities.
*/

void q_vertex_setup(S,v_info,needs)
struct linsys *S;
struct qinfo *v_info;
int needs;  /* particular setup needs for current mode */
{ /* struct vertex *v_ptr = (struct vertex *)elptr(v_info->id); */
  int i,j;
  vertex_id q_id;

  /* W A R N I N G */
  /* Be sure to place all data consistently for all methods !!! */
  v_info->S = S;
  q_id = v_info->id;
  v_info->vcount = 1;
  v_info->v[0] = v_info->id;
  v_info->x[0] = get_coord(v_info->v[0]);
  if ( needs & NEED_WINGS ) /* pair of adjacent vertices */
  { facetedge_id right_fe;
    edge_id e_id[3];
    right_fe = get_vertex_fe(v_info->id);
    if ( !valid_id(right_fe) ) goto vset_exit;
    v_info->vcount++; 
    v_info->v[1] = get_fe_headv(right_fe);
    e_id[1] = get_fe_edge(right_fe);
    e_id[2] = get_next_tail_edge(e_id[1]);
    /* get incoming first, outgoing second, if possible */
    if ( positive_id(e_id[1]) && !positive_id(e_id[2]) )
    { edge_id etmp = e_id[1];
      e_id[1] = e_id[2];
      e_id[2] = etmp;
    }
    if ( !equal_id(e_id[1],e_id[2]) ) 
    { v_info->vcount++; 
      v_info->v[2] = get_edge_headv(e_id[2]);
    }
    for ( i = 1 ; i < v_info->vcount ; i++ )
    {
      v_info->x[i] = get_coord(v_info->v[i]);
      if ( web.symmetry_flag )
      { (*sym_wrap)(v_info->x[i],v_info->xx[i],get_edge_wrap(e_id[i]));
         v_info->x[i] = v_info->xx[i];
      }
      for ( j = 0 ; j < SDIM ; j++ )
         v_info->sides[0][i-1][j] = v_info->x[i][j] - v_info->x[0][j];
    }
  }
  if ( needs & NEED_MARKED_WINGS ) 
     { /* pair of marked edge vertices 
          for sqcurve_string_marked */
        edge_id thise,starte;
        edge_id e_id[3];

        starte = get_vertex_edge(v_info->id);
        i = 1;
        thise = starte;
        do
        {
          if ( !valid_id(thise) ) goto vset_exit;
          if ( (marked_edge_attr>0) && *EINT(thise,marked_edge_attr) )
          {
            e_id[i] = thise;
            v_info->vcount++; 
            v_info->v[i] = get_edge_headv(e_id[i]);
            i++;
            if ( i >= 3 ) break;
          }
          thise = get_next_tail_edge(thise);
        } while ( !equal_id(thise,starte) );

        for ( i = 1 ; i < v_info->vcount ; i++ )
        {
          v_info->x[i] = get_coord(v_info->v[i]);
          if ( web.symmetry_flag )
          { (*sym_wrap)(v_info->x[i],v_info->xx[i],get_edge_wrap(e_id[i]));
             v_info->x[i] = v_info->xx[i];
          }
          for ( j = 0 ; j < SDIM ; j++ )
             v_info->sides[0][i-1][j] = v_info->x[i][j] - v_info->x[0][j];
        }
     }
  if ( needs & (NEED_FULL_STAR|NEED_PART_STAR) )  /* ring of facets */
  { facetedge_id fe,startfe,next_fe;
     edge_id e_id;
     int k=0;
     if ( get_vattr(q_id) & BARE_NAKED ) goto nostar;
     fe = get_vertex_fe(q_id);
     if ( !valid_id(fe) ) goto nostar;
     if ( inverted(get_fe_facet(fe)) ) 
          fe = inverse_id(get_prev_edge(fe));
     startfe = fe;
     if ( get_vattr(q_id) & AXIAL_POINT )
     { /* want to walk around outside ring */
        facetedge_id fa,next_fe;
        WRAPTYPE wrap = get_fe_wrap(fe);
        WRAPTYPE wrap0 = wrap;
        REAL *y;
        v_info->axial_order = 0;
        next_fe = fa = get_next_edge(fe);
        do
        { v_info->v[k+1] = get_fe_tailv(next_fe);
          v_info->wraps[k+1] = wrap;
          y = get_coord(v_info->v[k+1]);
          (*sym_wrap)(y,v_info->sides[0][k],wrap);
          for ( j = 0 ; j < SDIM ; j++ )
              v_info->sides[0][k][j] -= v_info->x[0][j];
          v_info->vcount++;
          k++;
          if ( k >= MAXVCOUNT-1 )
          { sprintf(errmsg,
              "quantity_setup: More than %d vertices around vertex %s.\n",
                  MAXVCOUNT-1, ELNAME(v_info->id));
            kb_error(1337,errmsg,RECOVERABLE);
          }
          wrap = (*sym_compose)(wrap,get_fe_wrap(next_fe));
          next_fe = get_next_edge(next_fe);
          next_fe = inverse_id(get_next_facet(next_fe));
          next_fe = get_next_edge(next_fe);
          if ( next_fe == fa ) v_info->axial_order++;
        } while ( (next_fe != fa) || (*sym_compose)((*sym_inverse)(wrap),wrap0));
        v_info->flags &= ~INCOMPLETE_STAR;

     }
     else
     { /* normal vertex star */
       /* first, see if we have partial star, and if so, start at one end */
       fe = startfe;
       v_info->flags &= ~INCOMPLETE_STAR;
       do
       { next_fe = get_next_facet(fe);
         if ( equal_id(next_fe,fe) )
         { /* have end of star */
           startfe = fe;
           v_info->flags |= INCOMPLETE_STAR;
           if ( needs & NEED_FULL_STAR )
           { sprintf(errmsg,"Vertex %s does not have a complete star (as needed by some method instance).\n",ELNAME(v_info->id));
            kb_error(1573,errmsg,RECOVERABLE);
           }
           break;
         }
         fe = inverse_id(get_prev_edge(next_fe)); 
       } 
       while ( !equal_id(fe,startfe) );

       /* now do the actual work */
       do 
       { 
          e_id = get_fe_edge(fe);
          v_info->v[k+1] = get_edge_headv(e_id);
          if(web.symmetry_flag) v_info->wraps[k+1] = get_edge_wrap(e_id);
          get_edge_side(e_id,v_info->sides[0][k]);
          v_info->vcount++;
          k++;
          if ( k >= MAXVCOUNT-1 )
          { sprintf(errmsg,
              "quantity_setup: More than %d vertices around vertex %s.\n",
                 MAXVCOUNT-1, ELNAME(v_info->id));
               kb_error(1572,errmsg,RECOVERABLE);
          }
          if ( !equal_id(fe,startfe) && equal_id(fe,get_next_facet(fe)) )
            break; /* found end of partial star */
          fe = get_prev_edge(fe);
          next_fe = get_next_facet(fe);
          fe = inverse_id(next_fe);
       } while ( !equal_id(fe,startfe) );
     }
nostar: ;
  }
vset_exit: ; 
}

/*******************************************************************
*
*  function: q_edge_setup()
* 
*  purpose:  calculate edge attributes needed for quantities.
*/

void q_edge_setup(S,e_info,needs)
struct linsys *S;
struct qinfo *e_info;
int needs;  /* particular setup needs for current mode */
{ int i,j;

  if ( web.modeltype == QUADRATIC ) 
  { q_edge_setup_q(e_info,needs); return; }
  if ( web.modeltype == LAGRANGE ) 
  { q_edge_setup_lagrange(e_info,needs); return; }

  /* W A R N I N G */
  /* Be sure to place all data consistently for all methods !!! */
  e_info->S = S;
  e_info->vcount = (web.representation == SIMPLEX) ? web.dimension : 2;
  e_info->v[0] = get_edge_tailv(e_info->id);
  e_info->v[1] = get_edge_headv(e_info->id);
  e_info->x[0] = get_coord(e_info->v[0]);
  e_info->x[1] = get_coord(e_info->v[1]);
  if ( web.symmetry_flag )
  { WRAPTYPE wrap = get_edge_wrap(e_info->id);
    (*sym_wrap)(e_info->x[1],e_info->xx[1],wrap);
    e_info->x[1] = e_info->xx[1];
    e_info->wraps[1] = wrap;
  }
  if ( needs & NEED_SIDE )
     for ( i = 0 ; i < SDIM ; i++ ) 
        e_info->sides[0][0][i] = e_info->x[1][i] - e_info->x[0][i];
  if ( needs & NEED_WINGS )
  { facetedge_id fe,left_fe,right_fe;
    e_info->vcount = 4; /* need wing vertices, also */
    fe = get_edge_fe(e_info->id);
    left_fe = inverse_id(get_prev_edge(fe));
    right_fe = inverse_id(get_prev_edge(get_next_facet(fe)));
    e_info->v[2] = get_fe_headv(left_fe);
    e_info->v[3] = get_fe_headv(right_fe);
    if ( web.symmetry_flag )
    { e_info->wraps[2] = get_edge_wrap(get_fe_edge(left_fe));
      e_info->wraps[3] = get_edge_wrap(get_fe_edge(right_fe));
    }
    for ( i = 2 ; i <= 3 ; i++ )
    {
      e_info->x[i] = get_coord(e_info->v[i]);
      if ( web.symmetry_flag )
      { (*sym_wrap)(e_info->x[i],e_info->xx[i],e_info->wraps[i]);
        e_info->x[i] = e_info->xx[i];
      }
      for ( j = 0 ; j < SDIM ; j++ )
         e_info->sides[0][i-1][j] = e_info->x[i][j] - e_info->x[0][j];
    }
  }
  if ( needs & NEED_STRING_STAR )
  { edge_id e_id1,e_id2 = e_info->id,e_id3;
     /* careful of weird packaging */
     e_info->vcount = 4;  /* need vertices of adjacent edges also */
     e_id1 = inverse_id(get_next_tail_edge(e_id2));
     e_id3 = inverse_id(get_next_head_edge(e_id2));
     /* have to leave first two vertices in place for other methods */
     e_info->v[2] = get_edge_tailv(e_id1);                                     
     e_info->v[3] = get_edge_headv(e_id3);
     for ( j = 0 ; j < 4  ; j++ ) e_info->x[j] = get_coord(e_info->v[j]);
     for ( j = 0 ; j < SDIM ; j++ ) 
     { 
        e_info->sides[0][0][j] = e_info->x[1][j] - e_info->x[0][j];
        e_info->sides[0][1][j] = e_info->x[0][j] - e_info->x[2][j];
        e_info->sides[0][2][j] = e_info->x[3][j] - e_info->x[1][j];
     }
  }
  if ( needs & NEED_GAUSS )
  { int m;
    for ( m = 0 ; m < gauss1D_num ; m++ )
      for ( i = 0 ; i < SDIM ; i++ )
         e_info->gauss_pt[m][i] = gauss1poly[0][m]*e_info->x[0][i]
                                + gauss1poly[1][m]*e_info->x[1][i];
  }
}

/*******************************************************************
*
*  function: q_edge_setup_q()
* 
*  purpose:  calculate edge attributes needed for quantities.
*                For quadratic model.
*/

void q_edge_setup_q(e_info,needs)
struct qinfo *e_info;
int needs;  /* particular setup needs for current mode */
{ int i,j;

  /* W A R N I N G */
  /* Be sure to place all data consistently for all methods !!! */

  e_info->vcount = (web.representation == SIMPLEX) ? web.dimension : 3;
  e_info->v[0] = get_edge_tailv(e_info->id);
  e_info->v[1] = get_edge_midv(e_info->id);
  e_info->v[2] = get_edge_headv(e_info->id);
  e_info->x[0] = get_coord(e_info->v[0]);
  e_info->x[1] = get_coord(e_info->v[1]);
  e_info->x[2] = get_coord(e_info->v[2]);
  if ( web.symmetry_flag )
     { WRAPTYPE wrap = get_edge_wrap(e_info->id);
        (*sym_wrap)(e_info->x[2],e_info->xx[2],wrap);
        e_info->x[2] = e_info->xx[2];
        e_info->wraps[2] = wrap;
     }
  if ( needs & NEED_SIDE ) 
  { /* tangent vectors at gauss points */
    int m;
    REAL t; 
    for ( m = 0 ; m < gauss1D_num ; m++ )
      for ( i = 0 ; i < SDIM ; i++ )
      { for ( j = 0, t = 0.0 ; j < edge_ctrl ; j++ )
            t += gauss1polyd[j][m]*e_info->x[j][i];
         e_info->sides[m][0][i] = t;
      }
  }
  if ( needs & NEED_WINGS )
     { kb_error(1575,"Can't do quadratic model with edge WINGS (needed for some method instance).\n",RECOVERABLE);
     }
  if ( needs & NEED_GAUSS )
     { int m;
        REAL t; 
        for ( m = 0 ; m < gauss1D_num ; m++ )
          for ( i = 0 ; i < SDIM ; i++ )
          { for ( j = 0, t = 0.0 ; j < edge_ctrl ; j++ )
                t += gauss1poly[j][m]*e_info->x[j][i];
             e_info->gauss_pt[m][i] = t;
          }
     }
}


/*******************************************************************
*
*  function: q_edge_setup_lagrange()
* 
*  purpose:  calculate edge attributes needed for quantities.
*                For Lagrange model.
*/

void q_edge_setup_lagrange(e_info,needs)
struct qinfo *e_info;
int needs;  /* particular setup needs for current mode */
{ int i;
  int ctrl = web.skel[EDGE].ctrlpts;
  int dim = (web.representation==STRING) ? 1 : web.dimension - 1 ;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss1D_order];
  vertex_id *v;

  /* W A R N I N G */
  /* Be sure to place all data consistently for all methods !!! */

  e_info->vcount = ctrl;
  v = get_edge_vertices(e_info->id);
  if ( inverted(e_info->id) )
  for ( i = 0 ; i < ctrl ; i++ )
  { e_info->v[i] = v[ctrl-i-1];
    e_info->x[i] = get_coord(v[ctrl-i-1]);
  }
  else
  for ( i = 0 ; i < ctrl ; i++ )
  { e_info->v[i] = v[i];
    e_info->x[i] = get_coord(v[i]);
  }
  if ( web.symmetry_flag )
  { WRAPTYPE wrap = get_edge_wrap(e_info->id);
    (*sym_wrap)(e_info->x[ctrl-1],e_info->xx[ctrl-1],wrap);
    e_info->x[ctrl-1] = e_info->xx[ctrl-1];
    e_info->wraps[ctrl-1] = wrap;
  }
  if ( needs & NEED_SIDE ) 
  { /* tangent vectors at gauss points */
    int m;
    for ( m = 0 ; m < gl->gnumpts ; m++ )
      mat_mult(gl->gpolypart[m],e_info->x,e_info->sides[m],dim,ctrl,SDIM);
  }
  if ( needs & NEED_WINGS )
  { kb_error(1576,
     "Can't do Lagrange model with WINGS (needed for some method instance).\n",
        RECOVERABLE);
  }
  if ( needs & NEED_GAUSS )
     mat_mult(gl->gpoly,e_info->x,e_info->gauss_pt,gl->gnumpts,ctrl,SDIM);
}

/*******************************************************************
*
*  function: q_facet_setup()
* 
*  purpose:  calculate facet attributes needed for quantities.
*/

void q_facet_setup(S,f_info,needs)
struct linsys *S;
struct qinfo *f_info;
int needs;  /* particular setup needs for current mode */
{ facetedge_id fe_id;
  int i,j;

  f_info->S = S;
  if ( web.modeltype == QUADRATIC ) 
  { q_facet_setup_q (f_info,needs); return; }
  if ( web.modeltype == LAGRANGE )  
  { q_facet_setup_lagrange(f_info,needs); return;}

  /* W A R N I N G */
  /* Be sure to place all data consistently for all methods !!! */

  f_info->vcount = web.dimension+1;
  if ( web.representation == SIMPLEX )
  { vertex_id *vv = get_facet_vertices(f_info->id);
    for ( i = 0 ; i <= web.dimension ; i++ )
    { f_info->v[i] = vv[i];
      f_info->x[i] = f_info->xx[i];
    }
  }
  else
  { 
    fe_id = get_facet_fe(f_info->id);
    for ( i = 0 ; i < FACET_EDGES ; i++ )
    { 
      f_info->v[i] = get_fe_tailv(fe_id);
      f_info->x[i] = f_info->xx[i];
      fe_id = get_next_edge(fe_id);
    } 
  }
  get_facet_verts(f_info->id,f_info->x,f_info->wraps);  /* in tail order */

  /* fan of sides from v0 */
  if ( needs & NEED_SIDE )
  { for ( i = 0 ; i < web.dimension ; i++ )
      for ( j = 0 ; j < SDIM ; j++ )
        f_info->sides[0][i][j] = f_info->x[i+1][j] - f_info->x[0][j];
  }

  if ( needs & NEED_NORMAL )
  { cross_prod(f_info->sides[0][0],f_info->sides[0][1],f_info->normal);
  }

  if ( needs & NEED_GAUSS )
    mat_mult(gpoly,f_info->x,f_info->gauss_pt,gauss2D_num,ctrl_num,SDIM);
}

/*******************************************************************
*
*  function: q_facet_setup_q()
* 
*  purpose:  calculate facet attributes needed for quantities.
*                Quadratic model.
*/

void q_facet_setup_q(f_info,needs)
struct qinfo *f_info;
int needs;  /* particular setup needs for current mode */
{ facetedge_id fe_id;
  int i,m;

  /* W A R N I N G */
  /* Be sure to place all data consistently for all methods !!! */

  f_info->vcount = 6; 
  fe_id = get_facet_fe(f_info->id);
  for ( i = 0 ; i < FACET_EDGES ; i++ )
  { 
    f_info->v[2*i] = get_fe_tailv(fe_id);
    f_info->x[2*i] = f_info->xx[2*i];
    f_info->v[2*i+1] = get_fe_midv(fe_id);
    f_info->x[2*i+1] = f_info->xx[2*i+1];
    fe_id = get_next_edge(fe_id);
  } 
  get_facet_verts(f_info->id,f_info->x,f_info->wraps);  /* in tail order */

  /* tangents at gauss points */
  for ( m = 0 ; m < gauss2D_num ; m++ )
      mat_mult(gpolypartial[m],f_info->x,f_info->sides[m],
                   web.dimension,FACET_CTRL,SDIM);

  /* if ( needs & NEED_NORMAL ) */

  if ( needs & NEED_GAUSS )
     mat_mult(gpoly,f_info->x,f_info->gauss_pt,gauss2D_num,FACET_CTRL,SDIM);
}


/*******************************************************************
*
*  function: q_facet_setup_lagrange()
* 
*  purpose:  calculate facetedge attributes needed for quantities.
*                For Lagrange model.
*/

void q_facet_setup_lagrange(f_info,needs)  
struct qinfo *f_info;
int needs;  /* particular setup needs for current mode */
{ int i,j;
  int ctrl = web.skel[FACET].ctrlpts;
  int dim = web.dimension;
  struct gauss_lag *gl = &gauss_lagrange[dim][web.gauss2D_order];
  vertex_id *v;

  /* W A R N I N G */
  /* Be sure to place all data consistently for all methods !!! */

  f_info->vcount = ctrl;
  v = get_facet_vertices(f_info->id);
  if ( inverted(f_info->id) )  /* invert orientation of vertices */
    for ( j = 0 ; j <= web.lagrange_order ; j++ )
      for ( i = 0 ; i+j <= web.lagrange_order ; i++ )
      { f_info->v[j*(web.lagrange_order+1)-j*(j-1)/2+i] =
          v[i*(web.lagrange_order+1)-i*(i-1)/2+j];
        f_info->x[j*(web.lagrange_order+1)-j*(j-1)/2+i] = 
          f_info->xx[i*(web.lagrange_order+1)-i*(i-1)/2+j];
      }
  else 
    for ( i = 0 ; i < ctrl ; i++ )
    { f_info->v[i] = v[i];
      f_info->x[i] = f_info->xx[i];
    }
  get_facet_verts(f_info->id,f_info->x,f_info->wraps);  
  if ( needs & NEED_SIDE ) 
  { /* tangent vectors at gauss points */
    int m;
    for ( m = 0 ; m < gl->gnumpts ; m++ )
      mat_mult(gl->gpolypart[m],f_info->x,f_info->sides[m],dim,ctrl,SDIM);
  }
  /* always need gauss */
  mat_mult(gl->gpoly,f_info->x,f_info->gauss_pt,gl->gnumpts,ctrl,SDIM);

}

/*******************************************************************
*
*  function: q_body_setup()
* 
*  purpose:  calculate body attributes needed for quantities.
*/

void q_body_setup(S,b_info,needs)
struct linsys *S;
struct qinfo *b_info;
int needs;  /* particular setup needs for current mode */
{
}


/*******************************************************************
*
*  function: q_facetedge_setup()
* 
*  purpose:  calculate facetedge attributes needed for quantities.
*/

void q_facetedge_setup(S,fe_info,needs)
struct linsys *S;
struct qinfo *fe_info;
int needs;  /* particular setup needs for current mode */
{
}

/*****************************************************************8
*
* functions: null_q_value(), null_q_grad(), null_q_hess()
*
* purpose: traps for undefined quantity methods.
*/

REAL null_q_value(q_info)
struct qinfo *q_info;
{ sprintf(errmsg,"Method value function not implemented for %s.\n",
     basic_gen_methods[METH_INSTANCE(q_info->method)->gen_method].name);
  kb_error(1577,errmsg,RECOVERABLE);

  return 0.0;
}

REAL null_q_grad(q_info)
struct qinfo *q_info;
{ sprintf(errmsg,"Method gradient function not implemented for %s.\n",
     basic_gen_methods[METH_INSTANCE(q_info->method)->gen_method].name);
  kb_error(1578,errmsg,RECOVERABLE);

  return 0.0;
}

REAL null_q_hess(q_info)
struct qinfo *q_info;
{ sprintf(errmsg,"Quantity hessian function not implemented for %s.\n",
     basic_gen_methods[METH_INSTANCE(q_info->method)->gen_method].name);
  kb_error(1579,errmsg,RECOVERABLE);

  return 0.0;
}

/* handy for zeroing out gradient and hessian */
void zerohess(q_info)
struct qinfo *q_info;
{ int m,i,j,k;
  for ( m = 0 ; m < q_info->vcount ; m++ )
     for ( j = 0 ; j < SDIM ; j++ ) 
        q_info->grad[m][j] = 0.0;
  for ( m = 0 ; m < q_info->vcount ; m++ )
    for ( i = 0 ; i < q_info->vcount ; i++ )
     for ( j = 0 ; j < SDIM ; j++ ) 
      for ( k = 0 ; k < SDIM ; k++ ) 
          q_info->hess[m][i][j][k] = 0.0;
}

/*********************************************************************

     Phase space evolution - film with inertia.
     Coordinates represent position and velocity.

     Requested by Jeremy Ackerman

**********************************************************************/

/***********************************************************************
*
* function: ackerman_init()
*
* purpose: initial checks for inertial motion
*/

void ackerman_init(mode,mi)
int mode;
struct method_instance *mi;
{ int sdim2=2*SDIM;
  if ( 2*SDIM > MAXCOORD )
     kb_error(1580,"ackerman method: Dimension too high for phase space motion.\n",RECOVERABLE);

  expand_attribute(VERTEX,V_COORD_ATTR,&sdim2);
  expand_attribute(VERTEX,V_OLDCOORD_ATTR,&sdim2);
  expand_attribute(VERTEX,V_FORCE_ATTR,&sdim2);
  expand_attribute(VERTEX,V_VELOCITY_ATTR,&sdim2);
  ackerman_flag = 1;
}

/***********************************************************************
*
* function: ackerman_energy()
*
* purpose: dummy energy
*/

REAL ackerman_energy(v_info)
struct qinfo *v_info;
{ return 0.0;
}

/***********************************************************************
*
* function: ackerman_forces()
*  
* purpose: forces in  phase space
*/

REAL ackerman_forces(v_info)
struct qinfo *v_info;
{ REAL *f = get_force(v_info->id);
  REAL *x = get_coord(v_info->id);
/*  REAL star = get_vertex_star(v_info->id); */
  int i;

  /* modify first order forces to second order */
  for ( i = 0 ; i < SDIM ; i++ )
    {
      f[i+SDIM] = f[i];
      f[i] = 0.0;
      v_info->grad[0][i] = -x[i+SDIM];
    }
  return 0.0;
}

/************************************************************************
*
* function: add_vgrads_to_update()
*
* purpose: add method gradients to low rank update part of hessian.
*/

void add_vgrads_to_update(S)
struct linsys *S;
{ vertex_id v_id;

  FOR_ALL_VERTICES(v_id)
  { struct hess_verlist *v1 = get_vertex_vhead(v_id);
    struct volgrad *vgptr = get_vertex_vgrad(v_id);
	for ( ; vgptr ; vgptr = vgptr->chain )
	{ struct method_instance *mi;
	  int j;
	  if ( !(vgptr->qnum & METHBASE ) ) continue;
	  mi = METH_INSTANCE(vgptr->qnum - METHBASE);
      if ( v1->proj )
      { REAL tempvec[MAXCOORD];
        vec_mat_mul(vgptr->grad,v1->proj,tempvec,SDIM,v1->freedom);
        for ( j = 0 ; j < v1->freedom ; j++ )
            S->low_rank_vectors[mi->global_low_rank][v1->rownum+j]
                        += tempvec[j];
       }
       else
       { for ( j = 0 ; j < SDIM ; j++ )
           S->low_rank_vectors[mi->global_low_rank][v1->rownum+j]
              += vgptr->grad[j];
       }
     }
   }
}
