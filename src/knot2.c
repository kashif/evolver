/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

#include "include.h"

/* for accessing knot energy exponent as adjustable parameter */
extern int exponent_param;  /* parameter number */
#define KNOTPOWER_NAME "knot_power"  /* name in datafile */


/******************************************************************

proj knot energy 

Suggested by Gregory Buck

Between pairs of edges, energy is inverse power of distance
between midpoints of edges, but projected on normal to edge.

******************************************************************/

/**************************************************************
*
*  function: proj_knot_energy()
*  
*  purpose: calculates energy of one pair of edges.
*
*  input: info about edge is in qinfo structure.
*
*/

REAL proj_knot_energy(e_info)
struct qinfo *e_info;
{ edge_id e1 = e_info->id,e2;
  REAL *x1,*x2,*yy1,*y2; /* end coordinates */
  REAL power;
  REAL energy = 0.0;
  REAL dx1[MAXCOORD];
  REAL dx2[MAXCOORD];
  REAL r[MAXCOORD];
  REAL LL1,L1,LL2,L2,rr,re1,re2;
  REAL p;
  REAL b1,b2,c1,c2;
  int j;

  power = globals(exponent_param)->value.real;
  x1 = get_coord(get_edge_tailv(e1));
  x2 = get_coord(get_edge_headv(e1));
  for ( j = 0 ; j < SDIM ; j++ ) dx1[j] = x2[j] - x1[j];
  LL1 = SDIM_dot(dx1,dx1);
  L1 = sqrt(LL1);
  FOR_ALL_EDGES(e2)
    { if ( e2 <= e1 ) continue; /* each pair once */
      yy1 = get_coord(get_edge_tailv(e2));
      y2 = get_coord(get_edge_headv(e2));
      for ( j = 0 ; j < SDIM ; j++ ) dx2[j] = y2[j] - yy1[j];
      LL2 = SDIM_dot(dx2,dx2);
      L2 = sqrt(LL2);
      for ( j = 0 ; j < SDIM ; j++ ) 
         r[j] = (yy1[j] + y2[j] - x1[j] - x2[j])/2;
      rr = SDIM_dot(r,r);
      re1 = SDIM_dot(r,dx1);
      re2 = SDIM_dot(r,dx2);
      p = pow(rr ,-power/2);
      b1 = 1 - re1*re1/rr/LL1;
      b2 = 1 - re2*re2/rr/LL2;
      c1 = pow(b1,power/2);
      c2 = pow(b2,power/2);
      energy += L1*L2*p*(c1 + c2);
    }
  return 2*energy; /* since doing each pair once */
}

/**************************************************************
*
*  function: proj_knot_energy_gradient()
*  
*  purpose: calculates energy of one edge due to potential
*              with all others.
*
*  input: info about edge is in qinfo structure.
*
*/

REAL proj_knot_energy_gradient(e_info)
struct qinfo *e_info;
{ edge_id e1 = e_info->id,e2;
  REAL *x1,*x2,*yy1,*y2; /* end coordinates */
  REAL power,poww;
  REAL energy = 0.0;
  REAL dx1[MAXCOORD];
  REAL dx2[MAXCOORD];
  REAL r[MAXCOORD];
  REAL LL1,L1,LL2,L2,rr,re1,re2;
  REAL p,en;
  REAL c1,c2;
  REAL b1,b2;
  int i,j;

  power = globals(exponent_param)->value.real;
  poww = power/2;
  for ( i = 0 ; i < 2 ; i++ )
     for ( j = 0 ; j < SDIM ; j++ ) e_info->grad[i][j] = 0.0;
  x1 = get_coord(get_edge_tailv(e1));
  x2 = get_coord(get_edge_headv(e1));
  for ( j = 0 ; j < SDIM ; j++ ) dx1[j] = x2[j] - x1[j];
  LL1 = SDIM_dot(dx1,dx1);
  L1 = sqrt(LL1);
  FOR_ALL_EDGES(e2)
    { if ( e2 == e1 ) continue;
      yy1 = get_coord(get_edge_tailv(e2));
      y2 = get_coord(get_edge_headv(e2));
      for ( j = 0 ; j < SDIM ; j++ ) dx2[j] = y2[j] - yy1[j];
      LL2 = SDIM_dot(dx2,dx2);
      L2 = sqrt(LL2);
      for ( j = 0 ; j < SDIM ; j++ ) 
         r[j] = (yy1[j] + y2[j] - x1[j] - x2[j])/2;
      rr = SDIM_dot(r,r);
      re1 = SDIM_dot(r,dx1);
      re2 = SDIM_dot(r,dx2);
      p = pow(rr ,-power/2);
      b1 = 1 - re1*re1/rr/LL1;
      if ( b1 <= 0.0 ) continue;
      b2 = 1 - re2*re2/rr/LL2;
      if ( b2 <= 0.0 ) continue;
      if ( poww == 0.0 ) 
          c1 = c2 = 1.0;
      else 
         { c1 = pow(b1,poww);
            c2 = pow(b2,poww);
         }
      en = L1*L2*p*(c1*b1 + c2*b2);
      energy += en;
      for ( j = 0 ; j < SDIM ; j++ ) 
        { e_info->grad[0][j] += 2*en/LL1*(-dx1[j])
             + (-power)*en/rr*(-r[j])
             + L1*L2*p*(power)*c1*(-1)*
                 (-re1*re1/LL1/rr/rr*(-r[j])
                  -2*re1*re1/rr/LL1/LL1*(-dx1[j])
                  + re1/rr/LL1*(-dx1[j] - 2*r[j]))
             + L1*L2*p*(power)*c2*(-1)*
                 (-re2*re2/LL2/rr/rr*(-r[j])
                  + re2/rr/LL2*(-dx2[j]));

          e_info->grad[1][j] += 2*en/LL1*(dx1[j])
             + (-power)*en/rr*(-r[j])
             + L1*L2*p*(power)*c1*(-1)*
                 (-re1*re1/LL1/rr/rr*(-r[j])
                  -2*re1*re1/rr/LL1/LL1*(dx1[j])
                  + re1/rr/LL1*(-dx1[j] + 2*r[j]))
             + L1*L2*p*(power)*c2*(-1)*
                 (-re2*re2/LL2/rr/rr*(-r[j])
                  + re2/rr/LL2*(-dx2[j]));
        }
    }

  return energy;  /* since doing all pairs */
}

/******************************************************************

Local hooke energy

Suggested and programmed by John Sullivan

Energy tries to equalize lengths of edges coming in to any one vertex
*/

/***************************************************************
*
*  function: local_hooke_init()
*
*  purpose: initialization for local_hooke()
*
*    Get global variable local_hooke_flag
*/

static int local_hooke_flag;
#define HOOKE_FLAG_NAME "local_hooke_flag"

void local_hooke_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{
  int flag_var;
  flag_var = lookup_global(HOOKE_FLAG_NAME);
  if ( flag_var < 0 ) /* missing, so add */
  { flag_var = add_global(HOOKE_FLAG_NAME);
    globals(flag_var)->value.real = 0.0;  /* default */
    globals(flag_var)->flags |=  ORDINARY_PARAM | RECALC_PARAMETER | ALWAYS_RECALC;
  }
  local_hooke_flag = (int)globals(flag_var)->value.real;
}

/**************************************************************
*
*  function: local_hooke()
*  
*  purpose: calculates local_hooke number of one pair of edges.
*
*  input: info about edge is in qinfo structure.
*
*/

REAL local_hooke(v_info)
struct qinfo *v_info;
{ vertex_id v0 = v_info->id;
  edge_id e1,e2;
  REAL l1,l2,energy;

  if ((!local_hooke_flag) && (get_vertex_evalence(v0) != 2))
     return 0.;    /* for now calculation is wrong if flag is set */

  /* e1 = get_vertex_edge(v0); */
  e1 = get_fe_edge(get_vertex_fe(v0));
  e2 = get_next_tail_edge(e1);

  l1 = get_edge_length(e1); l2 = get_edge_length(e2);
  energy = (l1-l2)/(l1+l2);
  energy = energy*energy;

  return energy;
}

/**************************************************************
*
*  function: local_hooke_gradient()
*  
*  purpose: calculates energy gradient of one edge due to potential
*              with all others.  Not yet implemented.
*
*  input: info about edge is in qinfo structure.
*
*/

REAL local_hooke_gradient(v_info)
struct qinfo *v_info;
{ vertex_id v0 = v_info->id;
  edge_id e1,e2, e0,e3;
  REAL l1,l2,grad,sum,energy, l0,l3;
  REAL ev1[MAXCOORD], ev2[MAXCOORD];
  int i;

  for (i=0; i<SDIM; i++)
     v_info->grad[0][i] = 0.;
  if (!local_hooke_flag && (get_vertex_evalence(v0) != 2))
     return 0.;    /* for now calculation is wrong if flag is set */

  e1 = get_fe_edge(get_vertex_fe(v0));
  e2 = get_next_tail_edge(e1);

  l1 = get_edge_length(e1); l2 = get_edge_length(e2);
  sum = l1+l2;
  energy = (l1-l2)/sum;
  grad = 4*energy/sum/sum; /* dbydl1 = grad*l2; dbydl2 = -grad*l1 */

  get_edge_side(e1,ev1); get_edge_side(e2,ev2); 
  for (i=0; i<SDIM; i++)
     v_info->grad[0][i] = -ev1[i]*grad*l2/l1 + ev2[i]*grad*l1/l2;

  e0 = get_next_head_edge(e1); e3 = get_next_head_edge(e2);
  /*    ---><---.---><---    **
  **     e3  e2 v e1  e0     */
  l0 = get_edge_length(e0); l3 = get_edge_length(e3);

  sum = l0+l1; grad = 4*(l0-l1)/sum/sum/sum;
  for (i=0; i<SDIM; i++)
     v_info->grad[0][i] += ev1[i]*grad*l0/l1;
  sum = l2+l3; grad = 4*(l2-l3)/sum/sum/sum;
  for (i=0; i<SDIM; i++)
     v_info->grad[0][i] -= ev2[i]*grad*l3/l2;

  return energy*energy;
}

/******************************************************************

average crossing number for knots

Suggested by Doug Zare
Programmed by John Sullivan

Between pairs of edges, energy is inverse cube power of distance
between midpoints of edges, times triple product of edge vectors
and distance vector.

E = 1/d^3 * (e1,e2,d)
*/

/**************************************************************
*
*  function: average_crossing()
*  
*  purpose: calculates average crossing number of one pair of edges.
*
*  input: info about edge is in qinfo structure.
*
*/

REAL average_crossing(e_info)
struct qinfo *e_info;
{ edge_id e1 = e_info->id,e2;
  REAL *x1,*x2,*yy1,*y2; /* end coordinates */
  REAL ee,energy = 0.0;
  REAL dx1[MAXCOORD];
  REAL dx2[MAXCOORD];
  REAL LL1,LL2,dd,de1,de2;
  REAL e1e2;
  int j;

  x1 = get_coord(get_edge_tailv(e1));
  x2 = get_coord(get_edge_headv(e1));
  for ( j = 0 ; j < SDIM ; j++ ) dx1[j] = x2[j] - x1[j];
  LL1 = SDIM_dot(dx1,dx1);
  FOR_ALL_EDGES(e2)
    { if ( e2 <= e1 ) continue; /* each pair once */
      yy1 = get_coord(get_edge_tailv(e2));
      y2 = get_coord(get_edge_headv(e2));
      LL2 = dd = de1 = de2 = e1e2 = 0.0;
      for ( j = 0 ; j < SDIM ; j++ )
      {
            REAL rj;
            dx2[j] = y2[j] - yy1[j];
            LL2 += dx2[j]*dx2[j];
            rj = (yy1[j] + y2[j] - x1[j] - x2[j])/2;
            dd += rj*rj;
            de1 += rj*dx1[j];
            de2 += rj*dx2[j];
            e1e2 += dx1[j]*dx2[j];
      }
      ee = (LL1*LL2*dd + 2*e1e2*de1*de2
                            - LL1*de2*de2 - LL2*de1*de1 - dd*e1e2*e1e2)/dd;
      if (ee>0.)
            energy += sqrt(ee)/dd;
    }
  return energy/2/M_PI;
}

/*****************************************************

twisting number for curves

Suggested and programmed by John Sullivan

Integral of torsion is approximated by looking at triples of adjacent
edges; if A,B,C are the edge vectors, then the sin of the angle the
osculating plane twists by (from AxB to BxC) is
    [A,B,C] |B|
    -----------
    |AxB| |BxC|
(This is analogous to t = [T,T',T'']/k^2.)

This function assumes the edges in each component are consistently oriented.

Gradient not implemented.
*/

/**************************************************************
*
*  function: twist()
*  
*  purpose: calculates average crossing number of one pair of edges.
*
*  input: info about edge is in qinfo structure.
*
*/

REAL twist(e_info)
struct qinfo *e_info;
{ edge_id e1 = e_info->id, e0,e2;
  REAL a0[MAXCOORD],a1[MAXCOORD],a2[MAXCOORD]; /* edge vectors */
  REAL a01[MAXCOORD], a12[MAXCOORD];
  REAL l0 = 0.0, l1 = 0.0, l2 = 0.0;
  int j;

  e0 = get_next_tail_edge(e1);
  e2 = get_next_head_edge(e1);
  get_edge_side(e0,a0); get_edge_side(e1,a1); get_edge_side(e2,a2);

  cross_prod(a0,a1,a01); cross_prod(a1,a2,a12);

  for ( j = 0 ; j < SDIM ; j++ )
  {
     l0 += a01[j]*a01[j];
     l1 += a1[j]*a1[j];
     l2 += a12[j]*a12[j];
  }

  if ( l0*l2 == 0.0 ) return 0.0;
  return asin(triple_prod(a0,a1,a2)*sqrt(l1/l0/l2))/2/M_PI;
}

/*****************************************************

writhing number for curves

Suggested by Hermann Gluck
Programmed by John Sullivan

Between pairs of edges, energy is inverse cube power of distance
between midpoints of edges, times triple product of edge vectors
and distance vector.

E = 1/d^3 * (e1,e2,d)
*/

/**************************************************************
*
*  function: writhe()
*  
*  purpose: calculates average crossing number of one pair of edges.
*
*  input: info about edge is in qinfo structure.
*
*/

REAL writhe(e_info)
struct qinfo *e_info;
{ edge_id e1 = e_info->id,e2;
  vertex_id e1h,e2h,e1t,e2t;
  REAL *x1,*x2,*yy1,*y2; /* end coordinates */
  REAL energy = 0.0;
  REAL dx1[MAXCOORD];
  REAL dx2[MAXCOORD];
  REAL d[MAXCOORD];
  REAL dd;
  int j;

  e1t = get_edge_tailv(e1);
  e1h = get_edge_headv(e1);
  x1 = get_coord(e1t);
  x2 = get_coord(e1h);
  for ( j = 0 ; j < SDIM ; j++ ) dx1[j] = x2[j] - x1[j];
  FOR_ALL_EDGES(e2)
    { if ( e2 == e1 ) continue;  /* equal edges */
      e2t = get_edge_tailv(e2);
      e2h = get_edge_headv(e2);
      if (e2t==e1h || e2h==e1t) continue; /* adjacent edges */
      yy1 = get_coord(e2t);
      y2 = get_coord(e2h);
      dd = 0.0;
      for ( j = 0 ; j < SDIM ; j++ )
      {
            dx2[j] = y2[j] - yy1[j];
            d[j] = (yy1[j] + y2[j] - x1[j] - x2[j])/2;
            dd += d[j]*d[j];
      }
      energy += triple_prod(dx1,d,dx2)/dd/sqrt(dd);
    }
  return energy/4/M_PI;
}

/**************************************************************
*
*  function: writhe_gradient()
*  
*  purpose: calculates gradient of writhe for one edge
*
*  input: info about edge is in qinfo structure.
*
*/

REAL writhe_gradient(e_info)
struct qinfo *e_info;
{ 
  edge_id e1 = e_info->id,e2;
  vertex_id e1h,e2h,e1t,e2t;
  REAL *x1,*x2,*yy1,*y2; /* end coordinates */
  REAL triple,ee,energy = 0.0;
  REAL dx1[MAXCOORD];
  REAL dx2[MAXCOORD];
  REAL d[MAXCOORD];
  REAL e1xe2[MAXCOORD];
  REAL dxe2[MAXCOORD];
  REAL dd;
  int i,j;

  e1t = get_edge_tailv(e1);
  e1h = get_edge_headv(e1);
  x1 = get_coord(e1t);
  x2 = get_coord(e1h);
  for ( i = 0 ; i < 2 ; i++ )
     for ( j = 0 ; j < SDIM ; j++ ) e_info->grad[i][j] = 0.0;
  for ( j = 0 ; j < SDIM ; j++ ) dx1[j] = x2[j] - x1[j];
  FOR_ALL_EDGES(e2)
    { if ( e2 == e1 ) continue;  /* equal edges */
      e2t = get_edge_tailv(e2);
      e2h = get_edge_headv(e2);
      if (e2t==e1h || e2h==e1t) continue; /* adjacent edges */
      yy1 = get_coord(e2t);
      y2 = get_coord(e2h);
      dd = 0.0;
      for ( j = 0 ; j < SDIM ; j++ )
      {
            dx2[j] = y2[j] - yy1[j];
            d[j] = (yy1[j] + y2[j] - x1[j] - x2[j])/2;
            dd += d[j]*d[j];
      }
      triple = triple_prod(dx1,d,dx2);
      ee = triple/dd/sqrt(dd);
      energy += ee;
      cross_prod(dx1,dx2,e1xe2); cross_prod(d,dx2,dxe2);
      for ( j = 0 ; j < SDIM ; j++ ) 
      {
         register REAL common1 = 3*ee/dd * d[j]/2;
         register REAL common2 = e1xe2[j]/dd/sqrt(dd)/2;
         register REAL oppose = dxe2[j]/dd/sqrt(dd);
         e_info->grad[0][j] += (common1 + common2 - oppose)/2/M_PI;
         e_info->grad[1][j] += (common1 + common2 + oppose)/2/M_PI;
      }
    }
  return energy/4/M_PI;
}

/******************************************************************

true average crossing number and writhe for knots

Suggested and Programmed by John Sullivan

Between pairs of edges, energy is area of spherical quadrilateral
of visibility from one to the other.

Thus if endpoints are a,b and c,d, let q1=d-a, q2=c-a, q3=c-b, q4=d-b.
Then let ci = (q(i-1) x qi) . (qi x q(i+1)) / |q(i-1) x qi| |qi x q(i+1)|
This is the cosine of one external angle of the spherical quadrilateral.
Thus the area is  2pi - sum(arccos(ci))
Actually, since arccos always gives a positive angle, this assumes
the quadrilateral is convex; luckily it will be.
Actually, for writhe we want to recover the sign of these angles,
to see if we're tracing the quadrilateral clockwise or counterclockwise.
This sign just comes from the sign of the volume of the tetrahedron abcd.

*/


/**************************************************************
*
*  function: true_average_crossing()
*  
*  purpose: calculates average crossing number of one pair of edges.
*
*  input: info about edge is in qinfo structure.
*
*/

REAL true_ax_wr ARGS((struct qinfo *, int));

REAL true_ax_wr(e_info, absval)
struct qinfo *e_info;
int absval;
{ edge_id e1 = e_info->id,e2;
  vertex_id e1t,e1h,e2t,e2h;
  REAL *a,*b,*c,*d; /* end coordinates */
  REAL energy = 0.0;
  REAL q[4][MAXCOORD];
  REAL qq[4];
  REAL qr[4];
  REAL pr[4];
  REAL sum;
  int i,j;

  for ( i = 0 ; i < 4 ; i++ ) qq[i] = qr[i] = pr[i] = 0.0;

  e1t = get_edge_tailv(e1); e1h = get_edge_headv(e1);
  a = get_coord(e1t);
  b = get_coord(e1h);

  FOR_ALL_EDGES(e2)
    { if ( e2 <= e1 ) continue; /* each pair once */
      e2t = get_edge_tailv(e2);
      e2h = get_edge_headv(e2);
      if (e2t==e1h || e2h==e1t) continue; /* adjacent edges */
      c = get_coord(e2t);
      d = get_coord(e2h);
      for ( j = 0 ; j < SDIM ; j++ )
      {
            q[0][j] = d[j]-a[j]; q[1][j] = c[j]-a[j];
            q[2][j] = c[j]-b[j]; q[3][j] = d[j]-b[j];
      }
      for (i=0; i<4; i++)
      {
          int im1 = (i-1)&3, ip1 = (i+1)&3;
          qq[i] = SDIM_dot(q[i],q[i]);
          qr[i] = SDIM_dot(q[i],q[ip1]);
          pr[i] = SDIM_dot(q[im1],q[ip1]);
      }
      sum=0.;
      for (i=0; i<4; i++)
      {
          int im1 = (i-1)&3, ip1 = (i+1)&3;
          REAL denom=(qq[im1]*qq[i]-qr[im1]*qr[im1])*(qq[i]*qq[ip1]-qr[i]*qr[i]);
          REAL cosine;
          if (denom>0.) cosine = (qr[im1]*qr[i] - pr[i]*qq[i]) / sqrt(denom);
          else            cosine = 0.;
          if  (cosine>1.)        cosine=1.;
          else if (cosine<-1.) cosine=-1.;
          sum += acos(cosine);
      }
      sum = 1-sum/2/M_PI;
      if (!absval && triple_prod(q[0],q[1],q[2]) < 0.) sum = -sum;
      energy += sum;
    }

  return energy;
}

REAL true_average_crossing(e_info)
struct qinfo *e_info;
{ return true_ax_wr(e_info, 1); }

REAL true_writhe(e_info)
struct qinfo *e_info;
{ return true_ax_wr(e_info, 0); }


#ifdef XXXX
/**************************************************************
*
*  function: true_writhe_gradient()
*  
*  purpose: calculates gradient of writhe for one edge
*
*  input: info about edge is in qinfo structure.
*/
NOT IMPLEMENTED YET

REAL true_writhe_gradient(e_info)
struct qinfo *e_info;
{ 
  edge_id e1 = e_info->id,e2;
  vertex_id e1h,e2h,e1t,e2t;
  REAL *x1,*x2,*yy1,*y2; /* end coordinates */
  REAL triple,ee,energy = 0.0;
  REAL dx1[MAXCOORD];
  REAL dx2[MAXCOORD];
  REAL d[MAXCOORD];
  REAL e1xe2[MAXCOORD];
  REAL dxe2[MAXCOORD];
  REAL dd;
  int i,j;

  e1t = get_edge_tailv(e1);
  e1h = get_edge_headv(e1);
  x1 = get_coord(e1t);
  x2 = get_coord(e1h);
  for ( i = 0 ; i < 2 ; i++ )
     for ( j = 0 ; j < SDIM ; j++ ) e_info->grad[i][j] = 0.0;
  for ( j = 0 ; j < SDIM ; j++ ) dx1[j] = x2[j] - x1[j];
  FOR_ALL_EDGES(e2)
    { if ( e2 == e1 ) continue;  /* equal edges */
      e2t = get_edge_tailv(e2);
      e2h = get_edge_headv(e2);
      if (e2t==e1h || e2h==e1t) continue; /* adjacent edges */
      yy1 = get_coord(e2t);
      y2 = get_coord(e2h);
      dd = 0.0;
      for ( j = 0 ; j < SDIM ; j++ )
      {
            dx2[j] = y2[j] - yy1[j];
            d[j] = (yy1[j] + y2[j] - x1[j] - x2[j])/2;
            dd += d[j]*d[j];
      }
      triple = triple_prod(dx1,d,dx2);
      ee = triple/dd/sqrt(dd);
      energy += ee;
      cross_prod(dx1,dx2,e1xe2); cross_prod(d,dx2,dxe2);
      for ( j = 0 ; j < SDIM ; j++ ) 
      {
         register REAL common1 = 3*ee/dd * d[j]/2;
         register REAL common2 = e1xe2[j]/dd/sqrt(dd)/2;
         register REAL oppose = dxe2[j]/dd/sqrt(dd);
         e_info->grad[0][j] += (common1 + common2 - oppose)/2/M_PI;
         e_info->grad[1][j] += (common1 + common2 + oppose)/2/M_PI;
      }
    }
  return energy/4/M_PI;
}
#endif

