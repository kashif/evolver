/************************************************************* 
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/ 
  
/********************************************************************* 
*
*  file: alice.c                         
*
*  purpose: Do tests of smoothed curvature for Alice Underwood
*           Also Craig Carter's energy method.
*
*/
 
#include "include.h"
  
REAL xalice[MAXCOORD+4];  /* center of test bump */
REAL valice[MAXCOORD+4] = { 0.0, 0.0, 1.0 };  /* vector of test bump */
REAL radius;               /* characteristic scale of bump */

REAL calc_bump ARGS((REAL*));

REAL calc_bump(pt)
REAL *pt;  /* coordinates */
{
  REAL xx[MAXCOORD];
  int i;
  REAL value;
  REAL r;

  for ( i = 0 ; i < SDIM ; i++ )
     xx[i] = pt[i] - xalice[i];

  /* function is fourth-order bump */
  r = SDIM_dot(xx,xx)/radius/radius;
  value = 1/(1+r)/(1+r);

  return value;
}

void alice()
{
  edge_id f_id;
  REAL sum=0.0;
  char p[90];

  sprintf(p,"Enter center of test function (%f %f %f): ",
      (DOUBLE)xalice[0],(DOUBLE)xalice[1],(DOUBLE)xalice[2]);
  prompt(p,msg,msgmax);
#ifdef LONGDOUBLE
  sscanf(msg,"%Lf %Lf %Lf %Lf %Lf", xalice,xalice+1,xalice+2,xalice+3,xalice+4);
#else
  sscanf(msg,"%lf %lf %lf %lf %lf", xalice,xalice+1,xalice+2,xalice+3,xalice+4);
#endif 

  sprintf(p,"Enter vector of test function (%f %f %f): ",
      (DOUBLE)valice[0],(DOUBLE)valice[1],(DOUBLE)valice[2]);
  prompt(p,msg,msgmax);
#ifdef LONGDOUBLE
  sscanf(msg,"%Lf %Lf %Lf %Lf %Lf", valice,valice+1,valice+2,valice+3,valice+4);
#else
  sscanf(msg,"%lf %lf %lf %lf %lf", valice,valice+1,valice+2,valice+3,valice+4);
#endif 

  sprintf(p,"Enter characteristic size of test function (%f): ",(DOUBLE)radius);
  prompt(p,msg,msgmax);
#ifdef LONGDOUBLE
  sscanf(msg,"%Lf", &radius);
#else
  sscanf(msg,"%lf", &radius);
#endif 

  /* integrate over every edge */
  FOR_ALL_FACETS(f_id)
     {
        REAL midpt[MAXCOORD];
        REAL norm[FACET_EDGES][MAXCOORD];
        REAL *x[FACET_EDGES+1];
        edge_id e_id[FACET_EDGES];
        REAL a,b,s0s0,s0s1,s1s1,surd;
        int  i,j,m;
        facetedge_id fe_id;
        REAL side[FACET_EDGES][MAXCOORD];
        REAL len[FACET_EDGES];

        /* get sides */
        fe_id = get_facet_fe(f_id);
        for ( i = 0 ; i < FACET_EDGES ; i++ )
          { 
             e_id[i] = get_fe_edge(fe_id);
             get_edge_side(e_id[i],side[i]);
             x[i] = get_coord(get_edge_headv(e_id[i]));
             fe_id = get_next_edge(fe_id);
          }
        x[FACET_EDGES] = x[0]; /* easy wrap */

        s0s0 = SDIM_dot(side[0],side[0]);
        s0s1 = SDIM_dot(side[1],side[0]);
        s1s1 = SDIM_dot(side[1],side[1]);
        len[0] = sqrt(s0s0);
        len[1] = sqrt(s1s1);
        len[2] = sqrt(s0s0 + s1s1 + 2*s0s1);
        surd = sqrt(s0s0*s1s1 - s0s1*s0s1);
        a = s0s0/surd; b = s0s1/surd;
        for ( i = 0 ; i < SDIM ; i++ )
          { norm[0][i] = -a*side[1][i] + b*side[0][i];
             norm[2][i] =  a*side[1][i] - b*side[0][i];
          }
        a = s1s1/surd; b = s0s1/surd;
        for ( i = 0 ; i < SDIM ; i++ )
          { norm[1][i] =  a*side[0][i] - b*side[1][i];
             norm[2][i] += -a*side[0][i] + b*side[1][i];
          }
        /* along each edge */
        for ( j = 0 ; j < FACET_EDGES ; j++ )
          { 
             if ( get_eattr(e_id[j]) & (FIXED|CONSTRAINT|BOUNDARY) ) 
                continue;
             for ( m = 0 ; m < gauss1D_num ; m++ )
              {
                 for ( i = 0 ; i < SDIM ; i++ )
                    midpt[i] = gauss1Dpt[m]*x[j][i] + (1 - gauss1Dpt[m])*x[j+1][i];
                 sum += gauss1Dwt[m]*SDIM_dot(valice,norm[j])
                            *calc_bump(midpt)*len[j];
              }
          }
     }

  sprintf(msg,"Variation: %f\n",(DOUBLE)(sum/radius/radius));
  outstring(msg);
}



/******************************************************************

Craig Carter's energy 

Given bodies $B_1$ and $B_2$ in $R^3$, define the energy
     E = \int_{B_1}\int_{B_2} {1 \over |z_1 - z_2|^{p} } d^3 z_2 d^3 z_1
This reduces to 
E = {1\over (3-p)(2-p)}\sum_{F_2\in\partial B_2}\sum_{F_1\in\partial B_1}
     N_1 \cdot N_2 \int_{F_2}\int_{F_1}{1\over |z_1 - z_2|^{p-2}}
     d^2 z_1 d^2 z_2.
And if we crudely approximate with centroids $\bar z_1$ and $\bar z_2$,
E = {1\over (3-p)(2-p)}\sum_{F_2\in\partial B_2}\sum_{F_1\in\partial B_1}
          {A_1 \cdot A_2 \over |\bar z_1 - \bar z_2|^{p-2}},
where $A_1$ and $A_2$ are unnormalized area vectors for the facets.

******************************************************************/

/***************************************************************
*
*  function: carter_energy_init()
*
*  purpose: initialization for carter_energy() and 
*              carter_energy_gradient().
*
*    No special prep.
*/

#define CARTER_POWER_NAME "carter_power"
REAL carter_power;
void carter_energy_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{ int param;
  param = lookup_global(CARTER_POWER_NAME);
  if ( param < 0 ) /* missing, so add */
        { param = add_global(CARTER_POWER_NAME);
          globals(param)->value.real = 6.0;  /* default */
          globals(param)->flags |=  ORDINARY_PARAM | RECALC_PARAMETER | ALWAYS_RECALC;
        }
  carter_power = globals(param)->value.real;

}

/**************************************************************
*
*  function: carter_energy()
*  
*  purpose: calculates energy of one pair of facets, if on different bodies.
*
*  input: info about facet is in qinfo structure.
*
*/

REAL carter_energy(f_info)
struct qinfo *f_info;
{ facet_id f1 = f_info->id,f2;
  REAL **x=f_info->x;
  MAT2D(y,FACET_VERTS,MAXCOORD); /* vertex coordinates */
  REAL energy = 0.0;
  REAL t1[MAXCOORD],t2[MAXCOORD];
  REAL det,rj,r[MAXCOORD];
  int j;
  body_id b_id = get_facet_body(f_info->id);
  body_id bb_id;

  if ( !valid_id(b_id) ) return 0.0;

  FOR_ALL_FACETS(f2)
    { if ( f2 <= f1 ) continue; /* each pair once */
      bb_id = get_facet_body(f2);
      if ( !valid_id(bb_id) ) continue;
      if ( equal_id(b_id,bb_id) ) continue;
      get_facet_verts(f2,y,NULL);
      for (j=0; j<SDIM; j++)
      {  t1[j] = y[1][j] - y[0][j];
          t2[j] = y[2][j] - y[0][j];
      }
      for (j=0; j<SDIM; j++)
          r[j] = (y[0][j]+y[1][j]+y[2][j] - (x[0][j]+x[1][j]+x[2][j]))/3;
      rj = SDIM_dot(r,r);
      det = SDIM_dot(f_info->sides[0][0],t1)*SDIM_dot(f_info->sides[0][1],t2)
              - SDIM_dot(f_info->sides[0][0],t2)*SDIM_dot(f_info->sides[0][1],t1);
      energy += det/pow(rj,carter_power/2-1);
    }
  return energy/(3-carter_power)/(2-carter_power)/4;
}

/**************************************************************
*
*  function: carter_energy_gradient()
*  
*  purpose: calculates gradient and energy of one pair of facets, 
*    if on different bodies.
*
*  input: info about facet is in qinfo structure.
*
*/

REAL carter_energy_gradient(f_info)
struct qinfo *f_info;
{ facet_id f1 = f_info->id,f2;
  REAL **x=f_info->x;
  MAT2D(y,FACET_VERTS,MAXCOORD); /* vertex coordinates */
  REAL energy = 0.0;
  REAL t1[MAXCOORD],t2[MAXCOORD];
  REAL rjgrad[FACET_VERTS][MAXCOORD];
  REAL detgrad[FACET_VERTS][MAXCOORD];
  REAL det,rj,r[MAXCOORD];
  int j,k;
  body_id b_id = get_facet_body(f_info->id);
  body_id bb_id;
  REAL s1t1,s1t2,s2t1,s2t2;
  REAL p,pp;

  if ( !valid_id(b_id) ) return 0.0;

  for ( k = 0 ; k < FACET_VERTS ; k++ )
    for ( j = 0 ; j < SDIM ; j++ ) 
     f_info->grad[k][j] = 0.0;

  FOR_ALL_FACETS(f2)
    { if ( f2 == f1 ) continue; /* no self energy */
      bb_id = get_facet_body(f2);
      if ( !valid_id(bb_id) ) continue;
      if ( equal_id(b_id,bb_id) ) continue;
      get_facet_verts(f2,y,NULL);
      for (j=0; j<SDIM; j++)
      {  t1[j] = y[1][j] - y[0][j];
          t2[j] = y[2][j] - y[0][j];
      }
      for (j=0; j<SDIM; j++)
          r[j] = (y[0][j]+y[1][j]+y[2][j] - (x[0][j]+x[1][j]+x[2][j]))/3;
      rj = SDIM_dot(r,r);
      for ( k = 0 ; k < FACET_VERTS ; k++ )
        for ( j = 0 ; j < SDIM ; j++ )
          rjgrad[k][j] = -2*r[j]/3;
      s1t1 = SDIM_dot(f_info->sides[0][0],t1);
      s1t2 = SDIM_dot(f_info->sides[0][0],t2);
      s2t1 = SDIM_dot(f_info->sides[0][1],t1);
      s2t2 = SDIM_dot(f_info->sides[0][1],t2);
      det = s1t1*s2t2 - s1t2*s2t1;
      for ( j = 0 ; j < SDIM ; j++ )
      { detgrad[0][j] = -t1[j]*s2t2 + t2[j]*s2t1 - t2[j]*s1t1 + t1[j]*s1t2;
         detgrad[1][j] = t1[j]*s2t2 - t2[j]*s2t1;
         detgrad[2][j] = t2[j]*s1t1 - t1[j]*s1t2;
      }
      p = 1/pow(rj,carter_power/2-1);
      pp = (carter_power/2-1)*det*p/rj;
      energy += det*p;
      for ( k = 0 ; k < FACET_VERTS ; k++ )
        for ( j = 0 ; j < SDIM ; j++ )
          f_info->grad[k][j] += (detgrad[k][j]*p - pp*rjgrad[k][j]);
    }
  for ( k = 0 ; k < FACET_VERTS ; k++ )
      for ( j = 0 ; j < SDIM ; j++ )
            f_info->grad[k][j] /= (3-carter_power)*(2-carter_power)*4 ;

  return energy/(3-carter_power)/(2-carter_power)/4/2;
}

