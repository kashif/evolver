/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/
 
/*********************************************************************
* 
*  File: curtest.c
*
*  Purpose: Test curvature of quadratic soapfilms to see if
*              the curvature is monotone.
*
*/

#include "include.h"

/*********************************************************************
*
*  Function:  curtest()
*
*  Purpose:    Overall control of curvature testing.
*
*/

void curtest()
{
  facet_id f_id;
  edge_id e_id;
  int retval;
  int edgetally[3],facettally[3];
  int i;

  if ( web.modeltype != QUADRATIC )
     { outstring("Cannot only do curvature sign test on QUADRATIC model.\n");
        return;
     }

  for ( i = 0 ; i < 3; i++ )
     edgetally[i] = facettally[i] = 0;
     

  FOR_ALL_FACETS(f_id)
     {
        retval = curtest_facet(f_id);
        if ( retval < 0 ) facettally[0]++;
        else if ( retval == 0 ) facettally[1]++;
        else facettally[2]++;
     }

  FOR_ALL_EDGES(e_id)
     {
        facetedge_id fe1,fe2;

        fe1 = get_edge_fe(e_id);
        if ( !valid_id(fe1) )  continue;

        /* ensure orientation lines up with original definition */
        if ( inverted(get_fe_facet(fe1)) )
          { invert(e_id);
             invert(fe1);
          }

        fe2 = get_next_facet(fe1);
        if ( equal_id(fe1,fe2) ) continue; /* only 1 facet on edge */
        if ( !equal_id(fe1,get_next_facet(fe2)) ) continue; /* more than 2 */
        retval = curtest_edge(e_id,fe1,fe2);
        if ( retval < 0 ) edgetally[0]++;
        else if ( retval == 0 ) edgetally[1]++;
        else edgetally[2]++;
     }

  /* report results */
  sprintf(msg,"\nPositive curvature edges:  %6d\n",edgetally[2]);
     outstring(msg);
  sprintf(msg,"Negative curvature edges:  %6d\n",edgetally[0]);
     outstring(msg);
  sprintf(msg,"Mixed curvature edges:      %6d\n",edgetally[1]);
     outstring(msg);
  sprintf(msg,"Positive curvature facets: %6d\n",facettally[2]);
     outstring(msg);
  sprintf(msg,"Negative curvature facets: %6d\n",facettally[0]);
     outstring(msg);
  sprintf(msg,"Mixed curvature facets:     %6d\n\n",facettally[1]);
     outstring(msg);

}

/*******************************************************************
*
*  Function:  curtest_edge()
*
*  Purpose:    See if two facets meet along an edge with monotone
*                 meeting angle.
*
*  Return:     Sign of (normal 1) x (normal 2) . (edge)
*                 +1 for all positive
*                 -1 for all negative
*                  0 otherwise
*/

/* evaluation points of cubic angle polynomial */
REAL uu[4] = {0.0, 0.7, 1.3, 2.0};  /* parameter along edge, 0 < u < 2 */

/* coefficients for finding polynomial coefficients */
REAL b1 = 0.3, b2 = 0.09, b3 = 0.027, denom = 1.0/(1 - 0.09);

int curtest_edge(e_id,fe_1,fe_2)
edge_id e_id;    /* the edge */
facetedge_id fe_1,fe_2;    /* link edge with facets */ 
{
  REAL *x[2][FACET_CTRL];  /* pointers to sets of vertices, starting along edge */
  REAL h[4];        /* values of curvature polynomial */
  REAL a[4];        /* polynomial coefficients */
  int i,j,k;
  int sign;         /* return value */
  REAL tu[MAXCOORD];  /* common tangent along edge */
  REAL tv[2][MAXCOORD]; /* the other tangent for each face */
  REAL discr;     /* quadratic formula discriminant */
  REAL r[2];      /* quadratic roots */
  REAL val;        /* curvature values */

  /* gather vertex coordinates */
  x[0][0] = x[1][0] = get_coord(get_edge_tailv(e_id));
  x[0][1] = x[1][1] = get_coord(get_edge_midv(e_id));
  x[0][2] = x[1][2] = get_coord(get_edge_headv(e_id));
  x[0][3] = get_coord(get_fe_midv(get_next_edge(fe_1)));
  x[1][3] = get_coord(get_fe_midv(get_next_edge(fe_2)));
  x[0][4] = get_coord(get_fe_headv(get_next_edge(fe_1)));
  x[1][4] = get_coord(get_fe_headv(get_next_edge(fe_2)));
  x[0][5] = get_coord(get_fe_midv(get_prev_edge(fe_1)));
  x[1][5] = get_coord(get_fe_midv(get_prev_edge(fe_2)));

  /* calculate test triple product at 4 points (it being cubic) */
  for ( i = 0 ; i < 4 ; i++ )
    {
      /* edge tangent */
      for ( j = 0 ; j < SDIM ; j++ )
         {
            tu[j] = x[0][0][j]*(uu[i] - 1.5) + 2*x[0][1][j]*(1 - uu[i])
                        + x[0][2][j]*(uu[i] - 0.5);
            for ( k = 0 ; k < 2 ; k++ )
              tv[k][j] = x[k][0][j]*(uu[i] - 1.5) - x[k][1][j]*uu[i]
                              + x[k][3][j]*uu[i] - 0.5*x[k][4][j]
                                + x[k][5][j]*(2.0 - uu[i]);
         }
      h[i] = triple_prod(tv[0],tv[1],tu);
    }
 
  /* see if signs at endpoints same or opposite */
  if ( (h[0] > 0.0) != (h[3] > 0.0) )  return 0;
  if ( h[0] > 0.0 ) sign = 1;
     else sign = -1;

  /* get polynomial coefficients */
  /* (remapping to points -1, -0.3, 0.3, 1) */
  a[0] = (h[1] + h[2] - b2*(h[0] + h[3]))/denom;
  a[1] = (h[2] - h[1] - b3*(h[3] - h[0]))/denom;
  a[2] = (h[0] - h[2] + h[3] - h[1])/denom;
  a[3] = (b1*(h[3] - h[0]) - (h[2] - h[1]))/denom;


  /* now decide on sign of cubic polynomial fitting h[] */
  /* find max and min points */
  discr = a[2]*a[2] - 3*a[1]*a[3];
  if ( discr <= 0.0 ) return sign;  /* no critical points */

  discr = sqrt(discr);
  r[0] = (-a[2] + discr)/3/a[3];
  r[1] = (-a[2] - discr)/3/a[3];
  for ( i = 0 ; i < 2 ; i++ )
     { if ( (r[i] < -1.0) || (r[i] > 1.0) ) continue;
       val = a[0] + r[i]*(a[1] + r[i]*(a[2] + r[i]*a[3]));
       if ( (val > 0.0) != (h[0] > 0.0) ) return 0;
     }

  return sign;
}

/*******************************************************************
*
*  Function:  curtest_facet()
*
*  Purpose:    See if a facet has constant sign mean curvature.
*
*  Return:     Sign of curvature with respect to oriented facet:
*                 +1 for all positive
*                 -1 for all negative
*                  0 otherwise
*/

int curtest_facet(f_id)
facet_id f_id;
{
  REAL *x[FACET_CTRL];  /* pointers to coordinates */
  facetedge_id fe;
  REAL tt[2][2][MAXCOORD];  /* position second partials */
  REAL tutu,tvtv,tutv,t[2][MAXCOORD];
  int i,j,k,m;
  int sign = 0;    /* return value 1 or -1 at end if all same curvature */
  REAL u,v;
  REAL vv[MAXCOORD];  /* first variation vector, sort of */
  REAL h;                 /* curvature at test point */

  /* gather vertices */
  fe = get_facet_fe(f_id);
  x[0] = get_coord(get_fe_tailv(fe));
  x[1] =  get_coord(get_fe_midv(fe));
  fe = get_next_edge(fe);
  x[2] =  get_coord(get_fe_tailv(fe));
  x[3] =  get_coord(get_fe_midv(fe));
  fe = get_next_edge(fe);
  x[4] =  get_coord(get_fe_tailv(fe));
  x[5] =  get_coord(get_fe_midv(fe));

  /* calculate position second partials (independent of u,v) */
  for ( i = 0 ; i < 2 ; i++ )
     for ( j = 0 ; j < 2 ; j++ )
        for ( k = 0 ; k < SDIM ; k++ )
          { tt[i][j][k] = 0.0;
             for ( m = 0 ; m < FACET_CTRL ; m++ )
                tt[i][j][k] += poly2partial[m][i][j]*x[m][k];
          }

    /* loop for sampling curvature */
    for ( u = 0.0 ; u < 2.00001 ; u += 0.4 )
      for ( v = 0.0 ; u+v < 2.00001 ; v += 0.4 )
         {
            /* inner loop does test at one point */

            /* calculate tangents */
            for ( i = 0 ; i < 2 ; i++ )
              for ( k = 0 ; k < SDIM ; k++ )
                 { t[i][k] = 0.0;
                    for ( m = 0 ; m < FACET_CTRL ; m++ )
                      t[i][k] += intpoly6part(m,i,u,v)*x[m][k];
                 }

            /* tangent dot products */
            tutu = SDIM_dot(t[0],t[0]);
            tutv = SDIM_dot(t[0],t[1]);
            tvtv = SDIM_dot(t[1],t[1]);

            /* assemble variation vector */
            for ( k = 0 ; k < SDIM ; k++ )
              vv[k] = 2*tutv*tt[0][1][k] - tutu*tt[1][1][k] - tvtv*tt[0][0][k];

            /* test normal component */
            h = triple_prod(t[0],t[1],vv);
            if ( h > 0.0 )
              { if ( sign == 0 ) sign = 1;
                 else if ( sign < 0 ) return 0;
              }
            else
              if ( sign == 0 ) sign = -1;
              else if ( sign > 0 ) return 0;

            /* end inner loop */

         }

  return sign;
}
