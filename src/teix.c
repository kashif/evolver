/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/**********************************************************************
*
*  File: tiex.c
*
* Contents: Miscellaneous stuff:
*      curvature forces
*      Laplacian of mean curvature
*      multigrid
*/

#include "include.h"

/********************************************************************
*
*  Function: curvature_forces()
*
*  Purpose:  Forces as function of curvatures.
*            Finds mean and gaussian curvature at each vertex,
*            and normal.
*
*/

struct teix_gvert  *tgverts;

void curvature_forces_init(mode,mi)
int mode;
struct method_instance *mi;
{ 
  vertex_id v[3];
  edge_id e_id;
  facet_id f_id;
  facetedge_id fe;
  int fixcount;
  int i,j;
  REAL area;
  REAL side[3][MAXCOORD];
  REAL ss[3];
  REAL st[3];
  REAL angle;
  REAL c;
  struct teix_gvert  *gv,*vc[3];
  REAL normal[MAXCOORD];

  if ( tgverts ) myfree((char*)tgverts);
  tgverts = (struct teix_gvert*)mycalloc(web.skel[VERTEX].max_ord+1,
                                            sizeof(struct teix_gvert));

  /* accumulate angles and stuff around vertices */
  FOR_ALL_FACETS(f_id)
  { fe = get_facet_fe(f_id);
    fixcount = 0;
    for ( i = 0; i < FACET_VERTS ; i++,fe=get_next_edge(fe) )
    { e_id = get_fe_edge(fe);
      get_edge_side(e_id,side[i]);
      v[i] = get_edge_tailv(e_id);
      if ( get_vattr(v[i]) & (FIXED|BOUNDARY) ) fixcount++;
    }
    for ( i = 0 ; i < FACET_VERTS ; i++ )
    { ss[i] = SDIM_dot(side[i],side[i]);
      st[i] = SDIM_dot(side[i],side[(i+2)%3]);
    }
    area = 0.5*sqrt(ss[0]*ss[1]-st[1]*st[1]);
    cross_prod(side[0],side[1],normal);
    for ( i = 0 ; i < FACET_VERTS ; i++ )
    { REAL dd = ss[i]*ss[(i+2)%3];
      gv = tgverts + loc_ordinal(v[i]);
      if ( dd > 0.0 )
      { c = -st[i]/sqrt(ss[i]*ss[(i+2)%3]);
        if ( fabs(c) <= 1.0 )
        { angle = acos(c);
          gv->angle += angle;
        }
      } 
      gv->area  += area/3;
      gv->star_area  += area/(3-fixcount);
      for ( j = 0 ; j < SDIM ; j++ )
      gv->normal[j] += normal[j];
    }

    /* for mean curvature */

    for ( i = 0 ; i < FACET_VERTS ; i++ )
      vc[i] = tgverts + loc_ordinal(v[i]);

    for ( i = 0 ; i < SDIM ; i++ )
    { 
       vc[0]->force[i] += (ss[1]*side[2][i]-st[2]*side[1][i])/4/area;
       vc[1]->force[i] += (ss[2]*side[0][i]-st[0]*side[2][i])/4/area;
       vc[2]->force[i] += (ss[0]*side[1][i]-st[1]*side[0][i])/4/area;
    }
  }
}
 
REAL curvature_forces_energy(v_info)
struct qinfo *v_info;
{ return 0.0; /* fake it */
}

REAL curvature_forces(v_info)
struct qinfo *v_info;
{ REAL K; /* gauss curvature of vertex */
  REAL H; /* mean curvature of vertex, average of sectional curvatures */
  struct teix_gvert *vg = tgverts + loc_ordinal(v_info->id);
  REAL norm;
  REAL f; /* function of H and K */
  int i;

  if ( get_vattr(v_info->id) & (FIXED|BOUNDARY) ) return 0.0;

  /* normalize normal */
  norm = sqrt(SDIM_dot(vg->normal,vg->normal));
  for ( i = 0 ; i < SDIM ; i++ )
     vg->normal[i] /= norm;

  K = (2*M_PI - vg->angle)/vg->area;

  H = SDIM_dot(vg->force,vg->normal)/2/vg->area;

  /* fix here for function of H and K */
  f =  (H>0.0) ? exp(K) : -exp(K);    /* to detect misorientation */
  /* end of fixing */

  /* force as multiple of normal */
  for ( i = 0 ; i < SDIM ; i++ )
     v_info->grad[0][i] = -f*vg->normal[i];
 
  return 0.0;  /* fake energy */
}

/****************************************************************************
*
*  Function: vertex_mean_curvature()
*
*  Purpose: calculate mean curvature at a vertex, as force divided by area,
*  area being volume gradient.  Actually, does dot product, etc.
*/

REAL vertex_mean_curvature(v_id)
vertex_id v_id;
{ 
  REAL meanc = 0.0;
  REAL force[MAXCOORD],projf[MAXCOORD];
  REAL area = 0.0,farea;
  REAL s1[MAXCOORD],s2[MAXCOORD];
  REAL vnorm[MAXCOORD];
  int i;

  for ( i = 0 ; i < MAXCOORD ; i++ ) 
    force[i] = 0.0;

  if ( web.representation == STRING )
  { edge_id start_e, e_id;
    REAL len;

    e_id = start_e = get_vertex_edge(v_id);
    if ( !valid_id(e_id) ) return 0.0;
    do 
    {
      REAL s11;
      get_edge_side(e_id,s1); 
      s11 = dot(s1,s1,SDIM);
      len = sqrt(s11);
      for ( i = 0 ; i < SDIM ; i++ ) 
        force[i] += 1/len*s1[i];
      e_id = get_next_tail_edge(e_id);
    } while ( !equal_element(start_e,e_id) );

    force_project(force,v_id,projf);
  }
  else if ( web.representation == SOAPFILM )
  { facetedge_id fe_id,start_fe;
    fe_id = start_fe = get_vertex_first_facet(v_id); /* really returns fe */
    if ( !valid_id(fe_id) ) return 0.0;
    do 
    { facetedge_id fe;
      REAL s11,s12,s22;
      fe = fe_id;
      while ( !equal_id(v_id,get_fe_headv(fe)) )
        fe = get_next_edge(fe);
      get_edge_side(get_fe_edge(fe),s1); 
      get_edge_side(get_fe_edge(get_prev_edge(fe)),s2); 
      s11 = dot(s1,s1,SDIM);
      s12 = dot(s1,s2,SDIM);
      s22 = dot(s2,s2,SDIM);
      farea = sqrt(s11*s22-s12*s12);
      for ( i = 0 ; i < SDIM ; i++ ) 
        force[i] += 1/farea*(s1[i]*s22 - s12*s2[i])/2;
      fe_id = get_next_vertex_facet(v_id,fe_id);
    } while ( !equal_element(start_fe,fe_id) );

    force_project(force,v_id,projf);
  }
  
  area = calc_vertex_normal(v_id,NULLID,vnorm); /* norm comes back unit */
  if ( area == 0.0 ) meanc = 0.0;
  else if ( web.representation == SOAPFILM )
    meanc = dot(force,vnorm,SDIM)/area*3/2.0;
  else
    meanc = dot(force,vnorm,SDIM)/area*2;

  return meanc;
} /* end vertex_mean_curvature */

/****************************************************************************
*
*  Function: sd_vertex_mean_curvature()
*
*  Purpose: calculate mean curvature at a vertex, as force divided by area,
*  area being volume gradient.  Actually, does dot product, etc.
*
*  Special version for surface diffusion: takes edge/facet tensions into
*  account, and uses total-volume grad as vertex normal.
*/

REAL sd_vertex_mean_curvature(v_id)
vertex_id v_id;
{
  REAL meanc = 0.0;
  REAL force[MAXCOORD],projf[MAXCOORD];
  REAL area = 0.0,farea;
  REAL s1[MAXCOORD],s2[MAXCOORD];
  REAL vnorm[MAXCOORD];
  int i;

  for ( i = 0 ; i < MAXCOORD ; i++ ) 
    force[i] = 0.0;

  if ( web.representation == STRING )
  { edge_id start_e, e_id;
    REAL len;

    e_id = start_e = get_vertex_edge(v_id);
    if ( !valid_id(e_id) ) return 0.0;
    do 
    { REAL tension = get_edge_density(e_id);
      REAL s11;
      get_edge_side(e_id,s1); 
      s11 = dot(s1,s1,SDIM);
      len = sqrt(s11);
      for ( i = 0 ; i < SDIM ; i++ ) 
        force[i] -= tension/len*s1[i];
      e_id = get_next_tail_edge(e_id);
    } while ( !equal_element(start_e,e_id) );

    force_project(force,v_id,projf);
  }
  else if ( web.representation == SOAPFILM )
  { facetedge_id fe_id,start_fe;

    fe_id = start_fe = get_vertex_first_facet(v_id);
    if ( !valid_id(fe_id) ) return 0.0;
    do 
    { facetedge_id fe;
      REAL s11,s12,s22;
      REAL tension = get_facet_density(get_fe_facet(fe_id));
      fe = fe_id;
      for ( i = 0 ; i < FACET_VERTS ; i++ )
      { if ( equal_id(v_id,get_fe_headv(fe)) )
          break;
        fe = get_next_edge(fe);
      }
      if ( i == FACET_VERTS )
      { sprintf(errmsg,"Internal error: sd_vertex_mean_curvature at vertex %s.\n",
          ELNAME(v_id));
        kb_error(3745,errmsg,RECOVERABLE);
      }
      get_edge_side(get_fe_edge(fe),s1); 
      get_edge_side(get_fe_edge(get_prev_edge(fe)),s2); 
      s11 = dot(s1,s1,SDIM);
      s12 = dot(s1,s2,SDIM);
      s22 = dot(s2,s2,SDIM);
      farea = sqrt(s11*s22-s12*s12);
      for ( i = 0 ; i < SDIM ; i++ ) 
        force[i] += tension/farea*(s1[i]*s22 - s12*s2[i])/2;
      fe_id = get_next_vertex_facet(v_id,fe_id);
    } while ( !equal_element(start_fe,fe_id) );

    force_project(force,v_id,projf);
  }
  
  area = vertex_total_vol_grad(v_id,vnorm); /* norm comes back unit */
  if ( area == 0.0 ) meanc = 0.0;
  else if ( web.representation == SOAPFILM )
    meanc = dot(force,vnorm,SDIM)/area*3/2.0;
  else
    meanc = dot(force,vnorm,SDIM)/area*2;

  return meanc;
} /* end sd_vertex_mean_curvature() */

/******************************************************************************
       Laplacian of mean curvature method, for surface diffusion.

The curvature at each vertex is calculated as a scalar, in the same way as
for area_normalized area gradient, i.e. area gradient dotted with volume
gradient, divided by the star area.

The Laplacian is calculated as described by physical diffusionn.

String model:  The gradient of curvature along each edge.  The
flow of "stuff" along each edge is proportional to the gradient.
There is a resultant net flux into the "star" of a vertex. The flux
divided by the volume gradient of the vertex gives the velocity,
i.e. the Laplacian of curvature.

Soapfilm model: The three curvatures at the vertices define a curvature
gradient vector.  There results a flux of stuff into the star of the
vertex.  This is divided by the volume gradient.

******************************************************************************/

/************************************************************************
*
* function: laplacian_mean_curvature_init();
*
* purpose: Calculate area gradient at each vertex, for later use by
*          individual vertex method.
*/
#define LMC_MC_ATTR_NAME "lmc_mean_curvature"
int lmc_mc_attr;
#define LMC_MOBILITY_ATTR_NAME "lmc_mobility"
int lmc_mobility_attr;

void laplacian_mean_curvature_init(mode,mi)
int mode;
struct method_instance *mi;
{ int one = 1;
  vertex_id v_id;

  if ( (web.representation != STRING)  && (web.representation != SOAPFILM) )
    kb_error(4085,"Can do laplacian_mean_curvature only in STRING and SOAPFILM models.\n",
         RECOVERABLE);

  if ( lmc_mc_attr < 0 )
    lmc_mc_attr = add_attribute(VERTEX,LMC_MC_ATTR_NAME,REAL_TYPE,0,&one,0,NULL);


  /* create mobility attribute, if user didn't */
  if ( web.representation == STRING )
  { edge_id e_id;
    lmc_mobility_attr = find_attribute(EDGE,LMC_MOBILITY_ATTR_NAME);
    if ( lmc_mobility_attr < 0 )
    { lmc_mobility_attr = add_attribute(EDGE,LMC_MOBILITY_ATTR_NAME,REAL_TYPE,0,&one,0,NULL);
      FOR_ALL_EDGES(e_id)
         *(REAL*)get_extra(e_id,lmc_mobility_attr) = 1.0;
    }
   }
   else
   { facet_id f_id;
     lmc_mobility_attr = find_attribute(FACET,LMC_MOBILITY_ATTR_NAME);
    if ( lmc_mobility_attr < 0 )
    { lmc_mobility_attr = add_attribute(FACET,LMC_MOBILITY_ATTR_NAME,REAL_TYPE,0,&one,0,NULL);
      FOR_ALL_FACETS(f_id)
         *(REAL*)get_extra(f_id,lmc_mobility_attr) = 1.0;
    }

  }

  /* mean curvatures at vertices */
  FOR_ALL_VERTICES(v_id)
    *(REAL*)get_extra(v_id,lmc_mc_attr) = sd_vertex_mean_curvature(v_id);
}

REAL laplacian_mean_curvature_value(v_info)
struct qinfo *v_info;
{ REAL energy = 0.0;
 
  /* Try integral of square gradient of mean curvature as energy,
     even though it is not exact. */

  if ( web.representation == STRING )
  {
    REAL h1,h;
    edge_id e_id,start_e;

    h = *(REAL*)get_extra(v_info->v[0],lmc_mc_attr);
    e_id = start_e = get_vertex_edge(v_info->v[0]);
    do
    { REAL len1;
      vertex_id v1;

      len1 = get_edge_length(e_id);
      v1 = get_edge_headv(e_id);
      h1 = *(REAL*)get_extra(v1,lmc_mc_attr);
      energy += (h-h1)*(h-h1)/len1/2;  /* /2 since double counting */
      e_id = get_next_tail_edge(e_id);
    } while ( !equal_id(e_id,start_e) );
  }

  if ( web.representation == SOAPFILM )
  { 
    REAL h1,h,h2;
    REAL side1[MAXCOORD],side2[MAXCOORD];
    edge_id edge1,edge2;
    vertex_id v1,v2;
    facet_id f_id;
    facetedge_id fe,start_fe;
    REAL mobility;
   
    h = *(REAL*)get_extra(v_info->v[0],lmc_mc_attr);

    fe = start_fe = get_vertex_first_facet(v_info->v[0]);
    do
    { REAL s11,s12,s22,det;

      f_id = get_fe_facet(fe);
      mobility = *(REAL*)get_f_extra(f_id,lmc_mobility_attr);
      if ( mobility != 0.0 )
      {
        edge1 = get_fe_edge(fe);
        get_edge_side(edge1,side1);
        v1 = get_edge_headv(edge1);
        h1 = *(REAL*)get_extra(v1,lmc_mc_attr);
        edge2 = get_fe_edge(inverse_id(get_prev_edge(fe)));
        get_edge_side(edge2,side2);
        v2 = get_edge_headv(edge2);
        h2 = *(REAL*)get_extra(v2,lmc_mc_attr);


        s11 = SDIM_dot(side1,side1);
        s12 = SDIM_dot(side1,side2);
        s22 = SDIM_dot(side2,side2);
        det = sqrt(s11*s22 - s12*s12);
        energy += ((h1-h)*(h1-h)*s22 - 2*(h1-h)*(h2-h)*s12 + (h2-h)*(h2-h)*s11)/det/6;
      }
      fe = get_next_vertex_facet(v_info->v[0],fe);
    } while ( !equal_id(fe,start_fe) );

  } /* end soapfilm */

  return energy;
}

REAL laplacian_mean_curvature_grad(v_info)
struct qinfo *v_info;
{
  int i;
  REAL normal[MAXCOORD];

  if ( web.representation == STRING )
  {
    /* compute Laplacian and multiply by normal */
    REAL len1;
    vertex_id v1;
    REAL h1,h;
    REAL lap = 0;
    REAL flux; /* into vertex star */
    REAL vgradmag;
    REAL mobility;
    edge_id e_id,start_e;

    vgradmag = vertex_total_vol_grad(v_info->v[0],normal);
    if ( vgradmag == 0 )
       return 0.0; /* internal vertex probably */

    h = *(REAL*)get_extra(v_info->v[0],lmc_mc_attr);
    e_id = start_e = get_vertex_edge(v_info->v[0]);
    flux = 0.0;
    do
    {
      len1 = get_edge_length(e_id);
      v1 = get_edge_headv(e_id);
      h1 = *(REAL*)get_extra(v1,lmc_mc_attr);
      mobility = *(REAL*)get_e_extra(e_id,lmc_mobility_attr);
      flux += mobility*(h-h1)/len1;
      e_id = get_next_tail_edge(e_id);
    } while ( !equal_id(e_id,start_e) );

    /* now multiply by normal */
    lap = flux/vgradmag;
    for ( i = 0 ; i < SDIM ; i++ )
       v_info->grad[0][i] = lap*normal[i];
   
    return 0;
  }

  /* Just soapfilm gets here */

  if ( web.representation == SOAPFILM )
  { REAL vgradmag;
    REAL h1,h,h2;
    REAL lap;
    REAL side1[MAXCOORD],side2[MAXCOORD];
    REAL flux;
    edge_id edge1,edge2;
    vertex_id v1,v2;
    facet_id f_id;
    facetedge_id fe,start_fe;
    REAL mobility;
   
    vgradmag = vertex_total_vol_grad(v_info->v[0],normal)/3;
    if ( vgradmag == 0.0 )
        return 0.0; /* internal vertex, probably */

    /* compute Laplacian and multiply by normal */
     
    h = *(REAL*)get_extra(v_info->v[0],lmc_mc_attr);

    flux = 0.0;
    fe = start_fe = get_vertex_first_facet(v_info->v[0]);
    do
    { REAL s11,s12,s22,det;

      f_id = get_fe_facet(fe);
      mobility = *(REAL*)get_f_extra(f_id,lmc_mobility_attr);
      if ( mobility != 0.0 )
      {
        edge1 = get_fe_edge(fe);
        get_edge_side(edge1,side1);
        v1 = get_edge_headv(edge1);
        h1 = *(REAL*)get_extra(v1,lmc_mc_attr);
        edge2 = get_fe_edge(inverse_id(get_prev_edge(fe)));
        get_edge_side(edge2,side2);
        v2 = get_edge_headv(edge2);
        h2 = *(REAL*)get_extra(v2,lmc_mc_attr);


        s11 = SDIM_dot(side1,side1);
        s12 = SDIM_dot(side1,side2);
        s22 = SDIM_dot(side2,side2);
        det = sqrt(s11*s22 - s12*s12);
        flux += mobility*((s11-2*s12+s22)*h + (s12-s22)*h1 + (s12-s11)*h2)/det;
      }
      fe = get_next_vertex_facet(v_info->v[0],fe);
    } while ( !equal_id(fe,start_fe) );
    lap = flux/vgradmag/2;

    /* now multiply by unit normal */
    for ( i = 0 ; i < SDIM ; i++ )
      v_info->grad[0][i] = lap*normal[i];
    
  } /* end soapfilm */

  return 0.0;
} /* end laplacian_mean_curvature_grad() */


/******************************************************************************

      MULTIGRID stuff.


*****************************************************************************/


/* Structure to hold multigrid info */
struct multigrid_s {
       struct linsys *fine;   /* high resolution system */
       struct linsys *coarse; /* coarse system */
       int *marks;    /* for recording which are coarse */
       int coarse_count;   /* number of coarse variables */
       int *fine_to_coarse; /* index translation */
       int *coarse_to_fine; /* and the other way */
       /* interpolation, first index is fine variable */
       int *interp_IA;
       int *interp_JA;
       REAL *interp_A;
       /* transpose interpolation, first indes is coarse variable */
       int *interp_tr_IA;
       int *interp_tr_JA;
       REAL *interp_tr_A;
       /* strongly connected neighbors */
       int *snbr_starts;
       int *snbr_counts;
       int *snbr_lists;
       REAL *wnbr_asums; /* sums of weak neighbor coefficients */
};

struct multigrid_s * init_multigrid ARGS(( struct linsys *));
void multigrid_cleanup ARGS((struct multigrid_s **));
void choose_coarse_points ARGS((struct multigrid_s*));
void interpolation_weights ARGS((struct multigrid_s*));
void interpolation_transpose ARGS((struct multigrid_s*));
void coarse_matrix ARGS((struct multigrid_s*));
 
void do_multigrid(S)
struct linsys *S;
{ struct multigrid_s *mg;
  mg = init_multigrid(S);
  multigrid_cleanup(&mg);
}
 
 
/* lambda count handling, for tracking largest lambda */
#define LAMBDAMAX 100
struct lambda_s { struct lambda_s *prev,*next;
                  int value;
} *lambda_list, *lambda_heads[LAMBDAMAX];
int lambdamax;
int lambda_count;

static REAL  omega = 0.25; /* "strong" connection cutoff ratio */

void print_lambda_lists()
{ int n;
  struct lambda_s *p;
  
  for ( n = 0 ; n < LAMBDAMAX ; n++ )
    if ( lambda_heads[n] )
     { printf(" Lambda %d: ",n);
       for ( p = lambda_heads[n] ; p ; p = p->next )
         printf(" %d",p-lambda_list);
       printf("\n");
    }
}

void lambda_init()
{ lambda_count = 0;
  lambdamax = -1;
  memset(lambda_heads,0,sizeof(lambda_heads));
  
}

void lambda_insert(n,value)
int n;  /* spot in list */
int value;
{ if ( value >= LAMBDAMAX )
   value = LAMBDAMAX-1;
  lambda_list[n].value = value;
  lambda_list[n].prev = NULL;
  lambda_list[n].next = lambda_heads[value];
  if ( lambda_heads[value] )
     lambda_heads[value]->prev = lambda_list + n;
  lambda_heads[value] = lambda_list + n;
  if ( value > lambdamax )
     lambdamax = value;  
  lambda_count++;
  
}

void lambda_delete(spot)
int spot; /* in list */
{ int value = lambda_list[spot].value;
  if ( lambda_list[spot].prev )
    lambda_list[spot].prev->next = lambda_list[spot].next;
  else
  { lambda_heads[value] = lambda_list[spot].next;
    if ( (lambda_heads[value] == NULL) && (value == lambdamax) )
    { /* reset lambdamax */
      for ( ; (lambdamax >= 0) && (lambda_heads[lambdamax] == NULL) ; lambdamax-- ) ;
    }
  }
  if ( lambda_list[spot].next )
    lambda_list[spot].next->prev = lambda_list[spot].prev;
  lambda_count--;
  
}

void increment_lambda(spot)
int spot; /* in list */
{ lambda_delete(spot);
  lambda_list[spot].value++;
  lambda_insert(spot,lambda_list[spot].value);
   
}

void decrement_lambda(spot)
int spot; /* in list */
{ lambda_delete(spot);
  lambda_list[spot].value++;
  lambda_insert(spot);
 
}

int get_highest_lambda()
{ 

  int ret = lambda_heads[lambdamax] - lambda_list;

  lambda_delete(ret);
  return ret;
}

static  int unmark = 0;
static  int coarse_mark = 1;
static  int fine_mark = 2;

/**************************************************************************
*
* Function: init_multigrid()
*
* Purpose: Set up multigrid structures, restriction and prolongation
*          matrices.
*
* Assumes input system described in N, IA, JA, A.
*/

struct multigrid_s * init_multigrid( fine )
struct linsys *fine;
{
  /* Select coarse points by going through fine points in order,
     and taking point as coarse point unless it has been marked
     as a neighbor of a previously selected coarse point.
  */

  struct multigrid_s *mg = (struct multigrid_s *)temp_calloc(1, 
                                        sizeof(struct multigrid_s));

  mg->fine = fine;

  /* Following algebraic multigrid method of Ruge & Stuben in
     Multigrid Methods (1987) ed. McCormick, chapter 4 
   */

  lambda_init();

  choose_coarse_points(mg);

  interpolation_weights(mg);

  interpolation_transpose(mg);

  coarse_matrix(mg);
 
  return mg;

} /* end init_multigrid */

/**************************************************************************
*
* function: choose_coarse_points()
*
* purpose: Choose subset of points to be the smaller coarse set.
*          Follows Ruge/Stuben algorithm.
*
* input: fine system, as pointed to by mg multigrid structure.
*
* output: 
*/

void  choose_coarse_points(mg)
struct multigrid_s *mg;
{ REAL *amaxes; /* maximum matrix values on row */ 
  int *nbr_count;  /* number of neighbors of each fine variable */
  int n,i,j;
  struct linsys *fine = mg->fine;
  int cspot;
  int *stnbr_starts,*stnbr_counts,*stnbr_lists;
  int *wnbr_starts,*wnbr_counts,*wnbr_lists;

  /* Coarse point choice, stage 1 (algorithm A1 on p. 102) */

  mg->marks = (int*)temp_calloc(fine->N,sizeof(int));

  /* Get maximum magnitudes in rows, so can find "strong" neighbors */
  /* And count neighbors, while we're at it */
  amaxes = (REAL*)temp_calloc(fine->N,sizeof(REAL));
  nbr_count = (int*)temp_calloc(fine->N,sizeof(int));
  for ( n = 0 ; n < fine->N ; n++ )
    for ( j = fine->IA[n]-A_OFF+1 ; j < fine->IA[n+1]-A_OFF ; j++ )
    { double mag = fabs(fine->A[j]);
      int jvar;
      if ( mag > amaxes[n] )
        amaxes[n] = mag;
      jvar = fine->JA[j] - A_OFF; 
      if ( mag > amaxes[jvar] )
        amaxes[jvar] = mag;
      nbr_count[n]++;
      nbr_count[jvar]++;
    }

  /* "Strong" cutoff is taken to be 0.25 of max */
  for ( n = 0 ; n < fine->N ; n++ )
    amaxes[n] *= omega;

  /* Record strong neighbors, and reciprocal strong neighbors */
  /* and weakly connected neighbors, for later */
  mg->snbr_starts = (int*)temp_calloc(fine->N+1,sizeof(int));
  mg->snbr_counts = (int*)temp_calloc(fine->N,sizeof(int));
  mg->snbr_lists = (int*)temp_calloc(2*fine->IA[fine->N],sizeof(int));

  stnbr_starts = (int*)temp_calloc(fine->N+1,sizeof(int));
  stnbr_counts = (int*)temp_calloc(fine->N,sizeof(int));
  stnbr_lists = (int*)temp_calloc(2*fine->IA[fine->N],sizeof(int));

  wnbr_starts = (int*)temp_calloc(fine->N+1,sizeof(int));
  wnbr_counts = (int*)temp_calloc(fine->N,sizeof(int));
  wnbr_lists = (int*)temp_calloc(2*fine->IA[fine->N],sizeof(int));
  mg->wnbr_asums = (REAL*)temp_calloc(fine->N,sizeof(REAL));

  for ( n = 0 ; n < fine->N ; n++ )
  { mg->snbr_starts[n+1] = mg->snbr_starts[n] + nbr_count[n];
    stnbr_starts[n+1] = stnbr_starts[n] + nbr_count[n];
    wnbr_starts[n+1] = wnbr_starts[n] + nbr_count[n];
  }
  for ( n = 0 ; n < fine->N ; n++ )
   for ( j = fine->IA[n]-A_OFF+1 ; j < fine->IA[n+1]-A_OFF ; j++ )
   { double mag = fabs(fine->A[j]);
     int jvar = fine->JA[j] - A_OFF; 
     if ( mag > amaxes[n] )
     { /* strong nbr */
       mg->snbr_lists[mg->snbr_starts[n] + mg->snbr_counts[n]++] = jvar;
       stnbr_lists[stnbr_starts[jvar] + stnbr_counts[jvar]++] = n;
     }
     else /* weak neighbor */
     { wnbr_lists[wnbr_starts[n] + wnbr_counts[n]++] = jvar;
       mg->wnbr_asums[n] += fine->A[j];
     }

     if ( mag > amaxes[jvar] )
     { /* strong nbr */
       mg->snbr_lists[mg->snbr_starts[jvar] + mg->snbr_counts[jvar]++] = n;
       stnbr_lists[stnbr_starts[n] + stnbr_counts[n]++] = jvar;
     }
     else /* weak neighbor */
     { wnbr_lists[wnbr_starts[jvar] + wnbr_counts[jvar]++] = n;
       mg->wnbr_asums[jvar] += fine->A[j];
     }
   }

  /* set up lambdas in heap so can track max lambda */
  lambda_list = (struct lambda_s *)temp_calloc(fine->N,sizeof(struct lambda_s));
  for ( n = 0 ; n < fine->N ; n++ )
    lambda_insert(n,stnbr_counts[n]);
  
   
  /* iterate marking coarse points */
  while ( lambda_count > 0 )
  { int k;
     /* remove from unmarked list and mark as coarse */ 
     i = get_highest_lambda();

     mg->marks[i] = coarse_mark;
     mg->coarse_count++;
     
printf("Coarse mark %d\n",i);

     /* mark fine neighbors */
     for ( j = 0 ; j < stnbr_counts[i] ; j++ )
     { int nbr = stnbr_lists[stnbr_starts[i] + j];
       if ( mg->marks[nbr] != unmark )
           continue;
       mg->marks[nbr] = fine_mark;
printf("   Fine mark %d\n",nbr);
       lambda_delete(nbr);
       for ( k = 0 ; k < mg->snbr_counts[nbr] ; k++ )
       { int nbrnbr = mg->snbr_lists[mg->snbr_starts[nbr] + k];
         if ( mg->marks[nbrnbr] != unmark )
           continue;
         increment_lambda(nbrnbr);
       }
     } 
     for ( j = 0 ; j < mg->snbr_counts[i] ; j++ )
     { int nbr = mg->snbr_lists[mg->snbr_starts[i] + j];
       if ( mg->marks[nbr] != unmark )
          continue;
       decrement_lambda(nbr);
     }
  }

  /* Finalize choice of coarse points */
  /* (algorithm A3, p. 102; weight calculation separate) */
  for ( n = 0 ; n < fine->N ; n++ )
  { int sfcount; /* number of strong fine neigbors, not coarse */
    int ctwiddle_count;
    int ctwiddle=0; /* candidate coarse variable */
    int c_nbrs[1000];
    int strong_fnbrs[1000];
    int c_nbr_count;
    int k;


    if ( mg->marks[n] != fine_mark ) 
      continue;

    /* A3.3: set C_i = S_i intersect C, Ds_i = S_i - C_i */
    ctwiddle_count = 0;
    sfcount = 0;
    c_nbr_count = 0;
    for ( k = 0 ; k < mg->snbr_counts[n] ; k++ )
    { int snbr = mg->snbr_lists[mg->snbr_starts[n] + k];
      if ( mg->marks[snbr] == coarse_mark )
      { c_nbrs[c_nbr_count++] = snbr;
      }
      else
      { strong_fnbrs[sfcount++] = snbr;  /* Ds_i */
      }
    }

step4:
    /* A3.5  */
    for ( j = 0 ; j < sfcount ; j++ )
    { /* A3.6 test S_j intersect C_i */
      int jnbr = strong_fnbrs[j];
      int meet_flag = 0;
      int nn,jj;
      
      for ( nn = 0 ; (meet_flag == 0) && (nn < c_nbr_count) ; nn++ )
        for ( jj = 0 ; jj < mg->snbr_counts[jnbr] ; jj++ )
          if ( c_nbrs[nn] == mg->snbr_lists[mg->snbr_starts[jnbr]+jj] )
          { meet_flag = 1; 
            break;
          }

      if ( meet_flag == 0 )
      { /* step 7 */
        if ( ctwiddle_count )  /* punt, and make it coarse */
        { mg->marks[n] = coarse_mark;
          mg->coarse_count++;
          break;
        }
        ctwiddle = jnbr;
        ctwiddle_count = 1;
        c_nbrs[c_nbr_count++] = jnbr;
        strong_fnbrs[j] = strong_fnbrs[--sfcount];
        goto step4;  /* try again */
      }

    } /* end step A3.5 */
     
    /* step A3.9 */
    if ( ctwiddle_count )
    {  mg->marks[ctwiddle] = coarse_mark;
       mg->coarse_count++;
    }
  }   /* end step A3.2 */

  /* coarse system aliases of coarse variables */
  mg->coarse = (struct linsys *)temp_calloc(1,sizeof(struct linsys));
  mg->fine_to_coarse = (int*)temp_calloc(fine->N,sizeof(int));
  mg->coarse_to_fine = (int*)temp_calloc(mg->coarse_count,sizeof(int));
  for ( n = 0, cspot = 0 ; n < fine->N ; n++ )
    if ( mg->marks[n] == coarse_mark )
    { mg->fine_to_coarse[n] = cspot;
      mg->coarse_to_fine[cspot] = n;
      cspot++;
    }


  temp_free((char*)nbr_count);
  temp_free((char*)amaxes);

  temp_free((char*)stnbr_starts);
  temp_free((char*)stnbr_counts);
  temp_free((char*)stnbr_lists);
  temp_free((char*)wnbr_starts);
  temp_free((char*)wnbr_counts);
  temp_free((char*)wnbr_lists);

} /* end choose_coarse_points() */

/***************************************************************************
*
* function: get_a()
*
* purpose: pluck entry from fine matrix A.
*/
REAL get_a(S,row,col)
struct linsys *S;
int row,col;
{ int kk;

  /* get into upper diagonal */
  if ( row > col )
  { int temp = row;
    row = col;
    col = temp;
  }

  for ( kk = S->IA[row]-A_OFF+1 ; kk < S->IA[row+1]-A_OFF ; kk++ )
   if ( S->JA[kk] == col )
     return S->A[kk];

  return 0.0;
} /* end get_a() */

/*************************************************************************
*
* function: interpolation_weights()
*
* purpose: Calculate interpolation weights for multigrid coarsening
*
*/

void  interpolation_weights(mg)
struct multigrid_s *mg;
{ struct linsys *fine = mg->fine;
  int c_spot; /* current place in interpolation long lists */
  int n; /* index of fine variables */
  int interp_alloc; /* how many entries allocated */

  /* Interpolation weights (algorithm A3, p. 102) */

  interp_alloc = 2*fine->IA[fine->N];
  mg->interp_JA = (int*)temp_calloc(interp_alloc,sizeof(int));
  mg->interp_IA = (int*)temp_calloc(fine->N+1,sizeof(int));
  mg->interp_A  = (REAL*)temp_calloc(interp_alloc,sizeof(int));

  c_spot = 0;

  for ( n = 0 ; n < fine->N ; n++ )
  { int strong_fnbrs[1000];
    int sfcount; /* of strong_fnbrs */
    int k,kk,j;
    REAL d_i; /* diagonal entry for this variable */
    REAL d[1000]; /* for nbrs */
    
    if ( c_spot + fine->N >= interp_alloc )
    { /* need more room */
      interp_alloc *= 2;
      mg->interp_JA = (int*)temp_realloc((char*)(mg->interp_JA),
                                 interp_alloc*sizeof(int));
      mg->interp_A  = (REAL*)temp_realloc((char*)(mg->interp_A),
                                 interp_alloc*sizeof(REAL));
    }

    mg->interp_IA[n] = c_spot; 
 
    if ( mg->marks[n] == coarse_mark ) 
    { /* interpolate just on self */
      mg->interp_A[c_spot] = 1.0; 
      mg->interp_JA[c_spot] = n;
      continue;
    }

    /* A3.3: set C_i = S_i intersect C */
    sfcount = 0;
    for ( k = 0 ; k < mg->snbr_counts[n] ; k++ )
    { int snbr = mg->snbr_lists[mg->snbr_starts[n] + k];
      if ( mg->marks[snbr] == coarse_mark )
        mg->interp_JA[c_spot++] = snbr; /* add to coarse nbr list */
      else
        strong_fnbrs[sfcount++] = snbr;  /* Ds_i */
    }

    /* A3.4, d_i = a_ii + sum weak */
    d_i = fine->A[fine->IA[n]-A_OFF] + mg->wnbr_asums[n];
    for ( k = mg->interp_IA[n], kk=0 ; k < c_spot ; k++,kk++ )
      d[kk] = get_a(fine,n,mg->interp_JA[k]);



    /* A3.5  */
    for ( j = 0 ; j < sfcount ; j++ )
    { REAL denom;
      int jnbr = strong_fnbrs[j];

      /* step 8 */
      /* denominator */
      for ( k = mg->interp_IA[n], denom = 0 ; k < c_spot ; k++ )
        denom += get_a(fine,jnbr,mg->interp_JA[k]);
      /* dk's */ 
      for ( k = mg->interp_IA[n] ; k < c_spot ; k++ )
        d[k] += get_a(fine,n,jnbr)*get_a(fine,jnbr,mg->interp_JA[k])/denom;
    } /* end step A3.5 */
     
    /* step A3.9 */
    for ( k = mg->interp_IA[n], kk=0 ; k < c_spot ; k++,kk++ )
      mg->interp_A[k] = -d[kk]/d_i;

  }   /* end step A3.2 */

  mg->interp_IA[n] = c_spot;  

  /* free unneeded space */
  interp_alloc = c_spot;
  mg->interp_JA = (int*)temp_realloc((char*)(mg->interp_JA),
                                 interp_alloc*sizeof(int));
  mg->interp_A  = (REAL*)temp_realloc((char*)(mg->interp_A),
                                 interp_alloc*sizeof(REAL));
} /* end interpolation_weights() */

/************************************************************************
*
* function: interpolation_transpose()
*
*/

void interpolation_transpose(mg)
struct multigrid_s *mg;
{ int k,j;

  /* transpose of interpolation matrix, i.e. store fine row adjacent,
     and index starts by coarse number */

  int *interp_tr_counts = (int*)temp_calloc(mg->coarse_count,sizeof(int));
  for ( k = 0 ; k < mg->fine->N ; k++ )
    for ( j = mg->interp_IA[k] ; j < mg->interp_IA[k+1] ; j++ )
    { int cnum = mg->fine_to_coarse[mg->interp_JA[j]];
      interp_tr_counts[cnum]++;
    }
    
  mg->interp_tr_IA = (int*)temp_calloc(mg->coarse_count+1,sizeof(int));
  for ( k = 0 ; k < mg->coarse_count ; k++ )
  { mg->interp_tr_IA[k+1] = mg->interp_tr_IA[k] +
            interp_tr_counts[k];
    interp_tr_counts[k] = 0;
  }
  
  mg->interp_tr_A = (REAL*)temp_calloc(mg->interp_tr_IA[mg->coarse_count],
      sizeof(REAL));
  mg->interp_tr_JA = (int*)temp_calloc(mg->interp_tr_IA[mg->coarse_count],
      sizeof(int));
     
  for ( k = 0 ; k < mg->fine->N ; k++ )
    for ( j = mg->interp_IA[k] ; j < mg->interp_IA[k+1] ; j++ )
    { int cnum = mg->fine_to_coarse[mg->interp_JA[j]];
      int spot =  mg->interp_tr_IA[cnum] + interp_tr_counts[cnum] ;
      mg->interp_tr_A[spot] = mg->interp_A[j];
      mg->interp_tr_JA[spot] = k;
      interp_tr_counts[cnum]++;
    }

} /* end interpolation_transpose() */

/**************************************************************************
*
* function: coarse_matrix()
*
* purpose: calculate matrix for coarse system.
*
*/

void coarse_matrix(mg)
struct multigrid_s *mg;
{ int n,nk,kk,k,j;
  int temp_top;
  int *temp_IA,*temp_JA;
  REAL *temp_A;
  int *temp_counts;
  int temp_A_alloc;
  int c_base,c_top;
  int *new_upper;
  int spot;
  int coarse_alloc;
  
  /* Now assemble the coarse system */

  /* project the A matrix, A_c = Interp^tr A_f Interp */
  /* Doing half of diagonal, to take advantage of symmetry */
  /* First, A_f Interp */
  /* using row-wise interp to avoid quadratic time */

  temp_IA = (int*)temp_calloc(mg->fine->N+1,sizeof(int));
  temp_counts = (int*)temp_calloc(mg->fine->N,sizeof(int));
  temp_A_alloc = mg->fine->IA[mg->fine->N];
  temp_A = (REAL*)temp_calloc(temp_A_alloc,sizeof(REAL));
  temp_JA = (int*)temp_calloc(temp_A_alloc,sizeof(int));
 
  mg->coarse->N = mg->coarse_count;  
  
  for ( n = 0 , temp_top = 0; n < mg->fine->N ; n++ )
  { int temp_base = temp_top;
  
     
    temp_IA[n] = temp_base;
    for ( nk = mg->fine->IA[n]-A_OFF ; nk < mg->fine->IA[n+1]-A_OFF ; nk++ )
    { int m,mm;
      int m_spot;
      int jk = mg->fine->JA[nk]-A_OFF;
      REAL aval = mg->fine->A[nk];
      if ( nk == mg->fine->IA[n]-A_OFF ) /* diagonal */
        aval /= 2;
      m_spot = temp_base;
      
      if ( temp_top + mg->coarse->N > temp_A_alloc )
      { temp_A_alloc *= 2;
        temp_A = (REAL*)temp_realloc((char*)temp_A,temp_A_alloc*sizeof(REAL));
        temp_JA = (int*)temp_realloc((char*)temp_JA,temp_A_alloc*sizeof(int));
      }
      
      for ( kk = mg->interp_IA[jk] ; kk < mg->interp_IA[jk+1] ; kk++ )
      { int ck = mg->interp_JA[kk];
        REAL prod = aval*mg->interp_A[kk];
        /* linear search up, since we expect relatively few entries */
        for ( m = m_spot ; m < temp_top ; m++ )
        { if ( ck == temp_JA[m] )
          { temp_A[m] += prod;
            break;
          }
          if ( ck > temp_JA[m] )
            continue;
          /* have to make room */
          for ( mm = temp_top-1 ; mm >= m ; mm-- )
          { temp_JA[mm+1] = temp_JA[mm];
            temp_A[mm+1] = temp_A[mm];
          }
          temp_A[m] = prod;
          temp_JA[m] = ck;
          temp_top++;
          break;
        }
        if ( m == temp_top )
        { /* new one on top */
          temp_JA[temp_top] = ck;
          temp_A[temp_top] = prod;
          temp_top++;
        }
      }
          
    }
  }
  temp_IA[mg->fine->N] = temp_top;
 

  /* next, Interp^Tr * temp */
  /* using transposed version of interp */
  c_base = 0;
  c_top = 0;

  mg->coarse->IA = (int*)temp_calloc(mg->coarse_count+1,sizeof(int));
  coarse_alloc = temp_IA[mg->fine->N];
  mg->coarse->JA = (int*)temp_calloc(coarse_alloc,sizeof(int));
  mg->coarse->A = (REAL*)temp_calloc(coarse_alloc,sizeof(REAL));
  for ( k = 0 ; k < mg->coarse_count ; k++ )
  { mg->coarse->IA[k] = c_base = c_top;
    for ( n = mg->interp_tr_IA[k] ; n < mg->interp_tr_IA[k+1] ; n++ )
    { int fn = mg->interp_tr_JA[n];
      int j_spot = c_base;
      int m,mm;
      if ( c_top + mg->coarse->N >= coarse_alloc )
      { /* need more room */
        coarse_alloc *= 2;
        mg->coarse->JA = (int*)temp_realloc((char*)(mg->coarse->JA),
                                        coarse_alloc*sizeof(int));
        mg->coarse->A = (REAL*)temp_realloc((char*)(mg->coarse->A),
                                        coarse_alloc*sizeof(REAL));
      }
      for ( m = temp_IA[fn] ; m < temp_IA[fn+1] ; m++ )
      { REAL prod = mg->interp_tr_A[n]*temp_A[m];
        int ck = temp_JA[m];
        /* linear search up */
        for ( ; j_spot < c_top ; j_spot++ )
        { if ( ck == mg->coarse->JA[j_spot] )
          { mg->coarse->A[j_spot] += prod;
            break;
          }
          if ( ck > mg->coarse->JA[j_spot] )
             continue;
          /* have to make room */
          for ( mm = c_top-1 ; mm >= j_spot ; mm-- )
          { mg->coarse->JA[mm+1] = mg->coarse->JA[mm];
            mg->coarse->A[mm+1] = mg->coarse->A[mm];
          }
          mg->coarse->JA[j_spot] = ck;
          mg->coarse->A[j_spot] = prod;
          c_top++;
          break;
        }
        if ( j_spot == c_top )
        { /* new one on top */
          mg->coarse->JA[c_top] = ck;
          mg->coarse->A[c_top] = prod;
          c_top++;
        }
      }
    }
  }
  mg->coarse->IA[mg->coarse_count] = c_top;
  mg->coarse->N = mg->coarse_count;
  /* now sum with transpose and keep just upper triangle */
  /* count how many new upper spots might be needed */
  new_upper = (int*)temp_calloc(mg->coarse->N,sizeof(int));
  for ( n = 0 ; n < mg->coarse->N ; n++ )
  { for ( j = mg->coarse->IA[n] ; j < mg->coarse->IA[n+1] ; j++ )
      if ( mg->coarse->JA[j] < n )
         new_upper[mg->coarse->JA[j]]++;
      else
         new_upper[n]++;
  }
   
  for ( n = 0 ; n < mg->coarse->N ; n++ )
    temp_IA[n+1] = temp_IA[n] + new_upper[n];
  for ( n = 0 ; n < mg->coarse->N ; n++ )
    for ( j = mg->coarse->IA[n] ; j < mg->coarse->IA[n+1] ; j++ )
    { REAL val = mg->coarse->A[j];
      int row,col;
      int i,m;
      if ( mg->coarse->JA[j] < n )
      { /* add to transpose spot */
        row = mg->coarse->JA[j];
        col = n;
      }
      else 
      { row = n;
        col = mg->coarse->JA[j];
        if ( row == col )
          val *= 2;
      }
      /* now bubble up */
      for ( i = 0 ; i < temp_counts[row] ; i++ )
      { int ispot = temp_IA[row] + i;
        if ( col == temp_JA[ispot] )
        { temp_A[ispot] += val;
          break;
        }
        if ( col > temp_JA[ispot] )
          continue;
        /* have to insert new one */
        for ( m = temp_IA[row] + temp_counts[row]-1 ; m > ispot ; m-- )
        { temp_JA[m+1] = temp_JA[m];
          temp_A[m+1] = temp_A[m];
        }
        temp_counts[row]++;
        temp_JA[ispot] = col;
        temp_A[ispot] = val;
        break;
      }
    }
 
  /* now compact back for final system */
  spot = 0;
  for ( n = 0 ; n < mg->coarse->N ; n++ )
  { mg->coarse->IA[n] = spot;
    for ( k = 0 ; k < temp_counts[n] ; k++ )
    { mg->coarse->JA[spot] = temp_JA[temp_IA[n]+k];
      mg->coarse->A[spot] = temp_A[temp_IA[n]+k];
      spot++;
    }
  }
  mg->coarse->IA[mg->coarse->N] = spot;
   
  /* Fortran offset */
  if ( A_OFF )
  { int jtop = mg->coarse->IA[mg->coarse->N];
    for ( n = 0 ; n < mg->coarse->N ; n++ )
      mg->coarse->IA[n] += A_OFF;
    for ( n = 0 ; n < jtop ; n++ )
      mg->coarse->JA[n] += A_OFF;
  }

 
  /* free unneeded memory */
  mg->coarse->JA = (int*)temp_realloc((char*)(mg->coarse->JA),
            mg->coarse->IA[mg->coarse->N]*sizeof(int));
  mg->coarse->A = (REAL*)temp_realloc((char*)(mg->coarse->A),
            mg->coarse->IA[mg->coarse->N]*sizeof(REAL));
       

  temp_free((char*)temp_IA);
  temp_free((char*)temp_JA);
  temp_free((char*)temp_A);
  temp_free((char*)temp_counts);
  temp_free((char*)new_upper);
  
} /* end coarse_matrix() */

/*************************************************************************
*
* function: multigrid_cleanup()
*
* purpose: deallocate memory in multigrid structure
*
*/

void multigrid_cleanup(mgp)
struct multigrid_s **mgp;
{ struct multigrid_s *mg = *mgp;

   free_system(mg->coarse);
   temp_free((char*)mg->marks); 
   temp_free((char*)mg->fine_to_coarse);
   temp_free((char*)mg->coarse_to_fine); 
   temp_free((char*)mg->interp_IA);
   temp_free((char*)mg->interp_JA);
   temp_free((char*)mg->interp_A);
   temp_free((char*)mg->interp_tr_IA);
   temp_free((char*)mg->interp_tr_JA);
   temp_free((char*)mg->interp_tr_A);
   temp_free((char*)mg->snbr_starts);
   temp_free((char*)mg->snbr_counts);
   temp_free((char*)mg->snbr_lists);
   temp_free((char*)mg->wnbr_asums); /* sums of weak neighbor coefficients */

  temp_free((char*)mg);
  *mgp = NULL;
}
/******************************************************************************

   End    MULTIGRID stuff.


*****************************************************************************/
