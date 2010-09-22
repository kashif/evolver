/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

#include "include.h"

/* for accessing knot energy exponent as adjustable parameter */
extern int exponent_param;  /* parameter number */
int cos_exponent_param;  /* parameter number */
#define KNOTPOWER_NAME "knot_power"  /* name in datafile */

/******************************************************************

edge edge knot energy 

Suggested and programmed by John Sullivan

Between pairs of edges, energy is inverse square power of distance
between midpoints of edges.

E = 1/d^2 * |e1||e2|


This should be roughly the same as uniform_knot_energy, but distances
are calculated from edge midpoints.  Also, this automatically handles
vertices of degree != 2.  It is the same as circle_knot_energy except for
the factor of (1-cos).
******************************************************************/

/**************************************************************
*
*  function: edge_edge_knot_energy()
*  
*  purpose: calculates energy of one pair of edges.
*
*  input: info about edge is in qinfo structure.
*
*/

REAL edge_edge_knot_energy(e_info)
struct qinfo *e_info;
{ edge_id e1 = e_info->id,e2;
  REAL *x1,*x2,*yy1,*y2; /* end coordinates */
  REAL energy = 0.0;
  REAL dx1[MAXCOORD];
  REAL dx2[MAXCOORD];
  REAL LL1,L1,LL2,L2,dd;
  int j;

  x1 = get_coord(get_edge_tailv(e1));
  x2 = get_coord(get_edge_headv(e1));
  for ( j = 0 ; j < SDIM ; j++ ) dx1[j] = x2[j] - x1[j];
  LL1 = SDIM_dot(dx1,dx1);
  L1 = sqrt(LL1);
  FOR_ALL_EDGES(e2)
    { if ( e2 == e1 ) continue; /* skip self */
      yy1 = get_coord(get_edge_tailv(e2));
      y2 = get_coord(get_edge_headv(e2));
      LL2 = dd = 0.0;
      for ( j = 0 ; j < SDIM ; j++ )
      {
            REAL rj;
            dx2[j] = y2[j] - yy1[j];
            LL2 += dx2[j]*dx2[j];
            rj = (yy1[j] + y2[j] - x1[j] - x2[j])/2;
            dd += rj*rj;
      }
      L2 = sqrt(LL2);
      energy += L1*L2/dd;
    }
  return energy;
}

/**************************************************************
*
*  function: edge_edge_knot_energy_gradient()
*  
*  purpose: calculates energy gradient of one edge due to potential
*              with all others.
*
*  input: info about edge is in qinfo structure.
*
*/

REAL edge_edge_knot_energy_gradient(e_info)
struct qinfo *e_info;
{ edge_id e1 = e_info->id,e2;
  REAL *x1,*x2,*yy1,*y2; /* end coordinates */
  REAL energy = 0.0;
  REAL dx1[MAXCOORD];
  REAL dx2[MAXCOORD];
  REAL r[MAXCOORD];
  REAL LL1,L1,LL2,L2,dd;
  REAL en1;
  int i,j;

  for ( i = 0 ; i < 2 ; i++ )
     for ( j = 0 ; j < SDIM ; j++ ) e_info->grad[i][j] = 0.0;
  x1 = get_coord(get_edge_tailv(e1));
  x2 = get_coord(get_edge_headv(e1));
  for ( j = 0 ; j < SDIM ; j++ ) dx1[j] = x2[j] - x1[j];
  LL1 = SDIM_dot(dx1,dx1);
  L1 = sqrt(LL1);
  FOR_ALL_EDGES(e2)
    { if ( e2 == e1 ) continue; /* skip self */
      yy1 = get_coord(get_edge_tailv(e2));
      y2 = get_coord(get_edge_headv(e2));
      LL2 = dd = 0.0;
      for ( j = 0 ; j < SDIM ; j++ )
      {
            dx2[j] = y2[j] - yy1[j];
            LL2 += dx2[j]*dx2[j];
            r[j] = (yy1[j] + y2[j] - x1[j] - x2[j])/2;
            dd += r[j]*r[j];
      }
      L2 = sqrt(LL2);
      en1 = L1*L2/dd;
      energy += en1;
      for ( j = 0 ; j < SDIM ; j++ ) 
      {
         register REAL common = 2*en1/dd*r[j];
         register REAL oppose = 2*L2/L1/dd*dx1[j];
         e_info->grad[0][j] += common - oppose;
         e_info->grad[1][j] += common + oppose;
      }
    }

  return energy;  /* since doing all pairs */
}

/******************************************************************
*
*  function: edge_normalization()
*
*  purpose: calculates internal knot energy to normalize
*              singular divergence of integral.
*/
#define do_nextedge(ee) (ee = inverse_id(get_next_head_edge(ee)))

REAL edge_normalization(e_info)
struct qinfo *e_info;
{
  edge_id e_id;
  REAL ti,tj;
  REAL dist=0., energy=0., comp_len;
  REAL power;

  e_id = e_info->id;
  comp_len = tj = ti = get_edge_length(e_id);

  power = globals(exponent_param)->value.real;
  for ( do_nextedge(e_id); e_id != e_info->id ; do_nextedge(e_id) )
        comp_len += get_edge_length(e_id);
  for ( do_nextedge(e_id); e_id != e_info->id ; do_nextedge(e_id) )
     {
        REAL shortdist;
        dist += tj/2;
        tj = get_edge_length(e_id);
        dist += tj/2;
        shortdist = (2*dist<comp_len? dist : comp_len-dist);
        energy += ti*tj/pow(shortdist,power);
     }
  return energy;
}

/******************************************************************

edge min knot energy 

Suggested and programmed by John Sullivan

Between pairs of edges, energy is inverse square power of distance
between closest points of edges.  This is not a smooth function,
so we don't try to compute a gradient.

E = 1/d^2 * |e1||e2|


This should be roughly the same as edge_edge_knot_energy, but distances
are calculated from edge midpoints there.
******************************************************************/

/**************************************************************
*
*  function: edge_min_knot_energy()
*  
*  purpose: calculates energy of one pair of edges.
*
*  input: info about edge is in qinfo structure.
*
*/
#define clip(x) ( x = (x<0.? 0. : (x>1.? 1. : x)) )

REAL edge_min_knot_energy(e_info)
struct qinfo *e_info;
{ edge_id e1 = e_info->id,e2;
  REAL *x1,*x2,*yy1,*y2; /* end coordinates */
  REAL energy = 0.0;
  REAL v[MAXCOORD], w[MAXCOORD], d[MAXCOORD];
  REAL vv,ww,vw,dv,dw,dd, mind;
  REAL vwvw, s,t;
  int j;

  x1 = get_coord(get_edge_tailv(e1));
  x2 = get_coord(get_edge_headv(e1));
  for ( j = 0 ; j < SDIM ; j++ ) v[j] = x2[j] - x1[j];
  vv = SDIM_dot(v,v);
  FOR_ALL_EDGES(e2)
    { if ( e2 == e1 ) continue; /* skip self */
      if (get_edge_tailv(e2) == get_edge_headv(e1)
        || get_edge_tailv(e1) == get_edge_headv(e2)) continue; /* skip nhbrs */
      yy1 = get_coord(get_edge_tailv(e2));
      y2 = get_coord(get_edge_headv(e2));
      ww = vw = dv = dw = dd = 0.0;
      for ( j = 0 ; j < SDIM ; j++ )
      {
            w[j] = y2[j] - yy1[j];
            d[j] = x1[j] - yy1[j];
            ww += w[j]*w[j]; vw += v[j]*w[j];
            dv += d[j]*v[j]; dw += d[j]*w[j]; dd += d[j]*d[j];
      }
      vwvw = vv*ww-vw*vw;
      if (vwvw > 0.)
      {
            s = (vw*dw-ww*dv)/vwvw; t = (-vw*dv+vv*dw)/vwvw;
            if (s>=0. && s<=1.)
            {
                if (t<0.)    { t = 0.; s = -dv/vv; clip(s); }
                else if (t>1.)        { t = 1.; s = (vw-dv)/vv; clip(s); }
            }
            else if (t>=0. && t<=1.)
            {
                if (s<0.)    { s = 0.; t = dw/ww; clip(t); }
                else if (s>1.)        { s = 1.; t = (vw+dw)/ww; clip(t); }
            }
            else if (s<0. && t<0.)
            {
                /* s=0 and t=dw/ww clipped; or t=0 and s=-dv/vv clipped */
                if (dw/ww > 0.)      { s = 0.; t = dw/ww; clip(t); }
                else                     { t = 0.; s =-dv/vv; clip(s); }
            }
            else if (s<0. && t>1.)
            {
                /* s=0 and t=dw/ww clipped; or t=1 and s=(vw-dv)/vv clipped */
                if (dw/ww < 1.)      { s = 0.; t = dw/ww; clip(t); }
                else                     { t = 1.; s =(vw-dv)/vv; clip(s); }
            }
            else if (s>1. && t<0.)
            {
                /* s=1 and t=(vw+dw)/ww clipped; or t=0 and s=-dv/vv clipped */
                if ((vw+dw)/ww > 0.)          { s = 1.; t = (vw+dw)/ww; clip(t); }
                else                     { t = 0.; s =-dv/vv; clip(s); }
            }
            else if (s>1. && t>1.)
            {
                /* s=0 and t=(vw+dw)/ww clipped; or t=1 and s=(vw-dv)/vv clipped */
                if ((vw+dw)/ww < 1.)          { s = 1.; t = (vw+dw)/ww; clip(t); }
                else                     { t = 1.; s =(vw-dv)/vv; clip(s); }
            }

            mind = dd+2*s*dv-2*t*dw+s*s*vv+t*t*ww-2*s*t*vw;
      }
      else /* v,w are parallel */
      {
          mind = dd - dv*dv/vv; /* the dist between || lines */
          if (vv>ww) /* w is shorter edge, look at its endpoints */
          {
                s = (dv-vw)/vv; if (s>=0. && s<=1.) goto ok;
                s = dv/vv; if (s>=0. && s<=1.) goto ok;
                if (vw<0.)
                     mind = (s<0.? dd : dd+2*dv-2*dw+vv+ww-2*vw);
                else
                     mind = (s<0.? dd-2*dw+ww : dd+2*dv+vv);

          }
          else /* v is shorter edge, look at its endpoints */
          {
                t = (-dw-vw)/ww; if (t>=0. && t<=1.) goto ok;
                t = -dw/ww; if (t>=0. && t<=1.) goto ok;
                if (vw<0.)
                     mind = (t<0.? dd : dd+2*dv-2*dw+vv+ww-2*vw);
                else
                     mind = (t<0.? dd+2*dv+vv : dd-2*dw+ww);
          }
      }
 ok: energy += sqrt(vv*ww)/mind;
    }
  return energy;
}

/******************************************************************
*
*  function: simon_normalization()
*
*  purpose: calculates internal knot energy to normalize
*              singular divergence of integral.
*/

REAL simon_normalization(e_info)
struct qinfo *e_info;
{
  edge_id e_id;
  REAL ti;
  REAL dist, energy=0.;
  int comp_nedge=1;
  int j;
  REAL power;

  e_id = e_info->id;

  power = globals(exponent_param)->value.real;
  for ( e_id = inverse_id(get_next_head_edge(e_id))
        ; e_id != e_info->id
        ; e_id = inverse_id(get_next_head_edge(e_id)) )
          comp_nedge++;
  ti = 2*sin(M_PI/comp_nedge);
  for (j=2; 2*j<=comp_nedge; j++)
     {
        dist = 2*sin((j-1)*M_PI/comp_nedge);
        energy += ti*ti/pow(dist,power);
        if (2*j==comp_nedge) energy -= ti*ti/pow(dist,power)/2;
     }
  return 2*energy;
}

/******************************************************************

circle knot energy 

Suggested by Peter Doyle

Between pairs of edges, energy is inverse square power of distance
between midpoints of edges, times (1 - cos theta) where theta is
between one edge and circle thru midpoint tangent to midpoint of
other edge.

E = 1/d^2 * (|e1||e2| - e1.e2 +2*e1.d*e2.d/d^2)
*/

/**************************************************************
*
*  function: circle_knot_energy()
*  
*  purpose: calculates energy of one pair of edges.
*
*  input: info about edge is in qinfo structure.
*
*/

REAL circle_knot_energy(e_info)
struct qinfo *e_info;
{ edge_id e1 = e_info->id,e2;
  REAL *x1,*x2,*yy1,*y2; /* end coordinates */
  REAL energy = 0.0;
  REAL dx1[MAXCOORD];
  REAL dx2[MAXCOORD];
  REAL LL1,L1,LL2,L2,dd,de1,de2;
  REAL e1e2;
  int j;

  x1 = get_coord(get_edge_tailv(e1));
  x2 = get_coord(get_edge_headv(e1));
  for ( j = 0 ; j < SDIM ; j++ ) dx1[j] = x2[j] - x1[j];
  LL1 = SDIM_dot(dx1,dx1);
  L1 = sqrt(LL1);
  FOR_ALL_EDGES(e2)
    { if ( e2 == e1 ) continue; 
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
      L2 = sqrt(LL2);
      energy += (L1*L2  + e1e2 - 2*de1*de2/dd)/dd;
         /* 0 for cocircular edges */
    }
  return energy;
}

/**************************************************************
*
*  function: circle_knot_energy_gradient()
*  
*  purpose: calculates energy gradient of one edge due to potential
*              with all others.
*
*  input: info about edge is in qinfo structure.
*
*/

REAL circle_knot_energy_gradient(e_info)
struct qinfo *e_info;
{ edge_id e1 = e_info->id,e2;
  REAL *x1,*x2,*yy1,*y2; /* end coordinates */
  REAL energy = 0.0;
  REAL dx1[MAXCOORD];
  REAL dx2[MAXCOORD];
  REAL r[MAXCOORD];
  REAL LL1,L1,LL2,L2,dd,de1,de2;
  REAL e1e2;
  REAL en1,en2;
  int i,j;

  for ( i = 0 ; i < 2 ; i++ )
     for ( j = 0 ; j < SDIM ; j++ ) e_info->grad[i][j] = 0.0;
  x1 = get_coord(get_edge_tailv(e1));
  x2 = get_coord(get_edge_headv(e1));
  for ( j = 0 ; j < SDIM ; j++ ) dx1[j] = x2[j] - x1[j];
  LL1 = SDIM_dot(dx1,dx1);
  L1 = sqrt(LL1);
  FOR_ALL_EDGES(e2)
    { if ( e2 == e1 ) continue; /* each pair once */
      yy1 = get_coord(get_edge_tailv(e2));
      y2 = get_coord(get_edge_headv(e2));
      LL2 = dd = de1 = de2 = e1e2 = 0.0;
      for ( j = 0 ; j < SDIM ; j++ )
      {
            dx2[j] = y2[j] - yy1[j];
            LL2 += dx2[j]*dx2[j];
            r[j] = (yy1[j] + y2[j] - x1[j] - x2[j])/2;
            dd += r[j]*r[j];
            de1 += r[j]*dx1[j];
            de2 += r[j]*dx2[j];
            e1e2 += dx1[j]*dx2[j];
      }
      L2 = sqrt(LL2);
      de1 /= dd; de2 /= dd;
      en1 = (L1*L2 + e1e2)/dd;
      en2 = -2*de1*de2;
      energy += en1+en2;
      for ( j = 0 ; j < SDIM ; j++ ) 
      {
         register REAL common = 
              2*((en1+2*en2)*r[j] + de1*dx2[j]+de2*dx1[j])/dd;
         register REAL oppose =
              2*(L2/L1*dx1[j] + dx2[j] - 2*r[j]*de2)/dd;
         e_info->grad[0][j] += common - oppose;
         e_info->grad[1][j] += common + oppose;
      }
    }

  return energy;  /* since doing all pairs */
}

/******************************************************************

sin knot energy 

Suggested by Oded Schramm
Programmed by John Sullivan

Between pairs of edges, energy is inverse square power of distance
between midpoints of edges, times (sin theta) where theta is
between one edge and circle thru midpoint tangent to midpoint of
other edge.

E = |e1||e2|/d^2 * sqrt(1 - e1.e2/|e1||e2| +2*e1.d*e2.d/d^2/|e1||e2|)
  = L1 L2/d^2 * sqrt(s)

*/

/**************************************************************
*
*  function: sin_knot_energy()
*  
*  purpose: calculates energy of one pair of edges.
*
*  input: info about edge is in qinfo structure.
*
*/

REAL sin_knot_energy(e_info)
struct qinfo *e_info;
{ edge_id e1 = e_info->id,e2;
  REAL *x1,*x2,*yy1,*y2; /* end coordinates */
  REAL energy = 0.0;
  REAL dx1[MAXCOORD];
  REAL dx2[MAXCOORD];
  REAL LL1,L1,LL2,L2,dd,de1,de2;
  REAL e1e2, L1L2, s, t;
  int j;

  x1 = get_coord(get_edge_tailv(e1));
  x2 = get_coord(get_edge_headv(e1));
  for ( j = 0 ; j < SDIM ; j++ ) dx1[j] = x2[j] - x1[j];
  LL1 = SDIM_dot(dx1,dx1);
  L1 = sqrt(LL1);
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
      L2 = sqrt(LL2); L1L2 = L1*L2;
      t = (2*de1*de2/dd - e1e2)/L1L2;
      s = 1-t*t; if (s<=0.) continue;
      energy += L1L2/dd * sqrt(s); /* 0 for cocircular edges */
    }
  return 2*energy;
}

/**************************************************************
*
*  function: sin_knot_energy_gradient()
*  
*  purpose: calculates energy gradient of one edge due to potential
*              with all others.
*
*  input: info about edge is in qinfo structure.
*
*/

REAL sin_knot_energy_gradient(e_info)
struct qinfo *e_info;
{ edge_id e1 = e_info->id,e2;
  REAL *x1,*x2,*yy1,*y2; /* end coordinates */
  REAL energy = 0.0;
  REAL dx1[MAXCOORD];
  REAL dx2[MAXCOORD];
  REAL r[MAXCOORD];
  REAL LL1,L1,LL2,L2,dd,de1,de2;
  REAL e1e2, L1L2, s, t;
  REAL en, fac;
  int i,j;

  for ( i = 0 ; i < 2 ; i++ )
     for ( j = 0 ; j < SDIM ; j++ ) e_info->grad[i][j] = 0.0;
  x1 = get_coord(get_edge_tailv(e1));
  x2 = get_coord(get_edge_headv(e1));
  for ( j = 0 ; j < SDIM ; j++ ) dx1[j] = x2[j] - x1[j];
  LL1 = SDIM_dot(dx1,dx1);
  L1 = sqrt(LL1);
  FOR_ALL_EDGES(e2)
    { if ( e2 == e1 ) continue; /* each ordered pair once */
      yy1 = get_coord(get_edge_tailv(e2));
      y2 = get_coord(get_edge_headv(e2));
      LL2 = dd = de1 = de2 = e1e2 = 0.0;
      for ( j = 0 ; j < SDIM ; j++ )
      {
            dx2[j] = y2[j] - yy1[j];
            LL2 += dx2[j]*dx2[j];
            r[j] = (yy1[j] + y2[j] - x1[j] - x2[j])/2;
            dd += r[j]*r[j];
            de1 += r[j]*dx1[j];
            de2 += r[j]*dx2[j];
            e1e2 += dx1[j]*dx2[j];
      }
      L2 = sqrt(LL2); L1L2 = L1*L2;
      t = (2*de1*de2/dd - e1e2)/L1L2;
      s = 1-t*t; if (s<=0.) continue;
      en = sqrt(s)/dd; fac = t*en/s;
      energy += (en = en*L1L2); /* 0 for cocircular edges */

      de1 /= dd; de2 /= dd;
      for ( j = 0 ; j < SDIM ; j++ ) 
      {
         register REAL common = 
              en/dd*r[j] + fac*(de2*dx1[j] + de1*dx2[j] - 2*de1*de2*r[j]);
         register REAL oppose =
              en/LL1*dx1[j] + fac*(dx2[j] - 2*de2*r[j] + t*L2/L1*dx1[j]);
         e_info->grad[0][j] += 2*(common - oppose);
         e_info->grad[1][j] += 2*(common + oppose);
      }
    }

  return energy;  /* since doing all pairs */
}



/******************************************************************

sphere knot energy 

Suggested by John Sullivan and Rob Kusner

Between pairs of facets, energy is inverse square power of distance
between midpoints of facets, times (1 - cos theta) where theta is
between one facet and sphere thru midpoint tangent to midpoint of
other facet.
                                            | r.r 2r.s1 2r.s2|
E = (1/4)1/r^4 * (|a1||a2| - (det|t1.r t1.s1 t1.s2|  )/r^2)
                                            |t2.r t2.s1 t2.s2|

                                                |r.r/2 r.s1  r.s2|
E = 1/r^4 * (|a1||a2|/4) ( 1 - 2*det|t1.r t1.s1 t1.s2| / (r^2 |a1||a2|) )^2
                                                |t2.r t2.s1 t2.s2|
                                      
where r is vector between midpoints, s1,s2 are sides of one facet,
and t1,t2 are sides of the other.
******************************************************************/
#define SURF_KNOTPOW_NAME "surface_knot_power"
#define SURF_COSPOW_NAME "surface_cos_power"
static REAL spower,cpower;

void sphere_knot_energy_init(mode,mi)
int mode;
struct method_instance *mi;
{
  exponent_param = lookup_global(SURF_KNOTPOW_NAME);
  if ( exponent_param < 0 ) /* missing, so add */
        { exponent_param = add_global(SURF_KNOTPOW_NAME);
          globals(exponent_param)->value.real = 4.0;  /* default */
          globals(exponent_param)->flags |=  ORDINARY_PARAM | RECALC_PARAMETER | ALWAYS_RECALC;
        }
  spower = globals(exponent_param)->value.real/2;
  cos_exponent_param = lookup_global(SURF_COSPOW_NAME);
  if ( cos_exponent_param < 0 ) /* missing, so add */
        { cos_exponent_param = add_global(SURF_COSPOW_NAME);
          globals(cos_exponent_param)->value.real = 1.0;  /* default */
          globals(cos_exponent_param)->flags |= ORDINARY_PARAM|RECALC_PARAMETER | ALWAYS_RECALC;
        }
  cpower = globals(cos_exponent_param)->value.real;
}

/**************************************************************
*
*  function: sphere_knot_energy()
*  
*  purpose: calculates energy of one pair of facets.
*
*  input: info about facet is in qinfo structure.
*
*/

REAL sphere_knot_energy(f_info)
struct qinfo *f_info;
{ facet_id f1 = f_info->id,f2;
  REAL *x[FACET_VERTS],*y[FACET_VERTS]; /* vertex coordinates */
  REAL energy = 0.0;
  REAL s1[MAXCOORD],s2[MAXCOORD];
  REAL t1[MAXCOORD],t2[MAXCOORD];
  REAL rr2,rs1,rs2,t1r,t1s1,t1s2,t2r,t2s1,t2s2;
  REAL s1s1,s1s2,s2s2,t1t1,t1t2,t2t2;
  REAL As,AsAt,det,angfac;
  int j;
  facetedge_id fe;
  vertex_id v[FACET_VERTS],w[FACET_VERTS];
  REAL pp;
  REAL xmid[MAXCOORD];
  REAL rj[MAXCOORD];

  fe = get_facet_fe(f1);
  for ( j = 0 ; j < FACET_VERTS ; j++ )
    { v[j] = get_fe_tailv(fe);
      x[j] = get_coord(v[j]);
      fe = get_next_edge(fe);
    }

  for ( j = 0 ; j < SDIM ; j++ )
     { s1[j] = x[1][j] - x[0][j];
        s2[j] = x[2][j] - x[0][j];
        xmid[j] = (x[0][j]+x[1][j]+x[2][j])/3;
     }
  s1s1 = SDIM_dot(s1,s1);
  s1s2 = SDIM_dot(s1,s2);
  s2s2 = SDIM_dot(s2,s2);
  As = sqrt(s1s1*s2s2 - s1s2*s1s2);
  FOR_ALL_FACETS(f2)
    { if ( f2 <= f1 ) continue; /* each pair once */
      fe = get_facet_fe(f2);
      for ( j = 0 ; j < FACET_VERTS ; j++ )
        { w[j] = get_fe_tailv(fe);
          y[j] = get_coord(w[j]);
          fe = get_next_edge(fe);
        }
      t1t1=t1t2=t2t2=rr2=rs1=rs2=t1r=t1s1=t1s2=t2r=t2s1=t2s2=0.0;
      for (j=0; j<SDIM; j++)
      {
          t1[j] = y[1][j] - y[0][j];
          t2[j] = y[2][j] - y[0][j];
          t1t1 += t1[j]*t1[j]; t1t2 += t1[j]*t2[j]; t2t2 += t2[j]*t2[j];
          rj[j] = (y[0][j]+y[1][j]+y[2][j])/3 - xmid[j];
          rr2 += rj[j]*rj[j]; 
      }
      rr2 /= 2;
      AsAt = As*sqrt(t1t1*t2t2-t1t2*t1t2);
      if ( cpower == 0.0 ) pp = 1.0;
      else 
      { 
         for (j=0; j<SDIM; j++)
         {
             t1r += t1[j]*rj[j]; t2r += t2[j]*rj[j];
             rs1 += rj[j]*s1[j]; t1s1 += t1[j]*s1[j]; 
             t2s1 += t2[j]*s1[j]; rs2 += rj[j]*s2[j]; 
             t1s2 += t1[j]*s2[j]; t2s2 += t2[j]*s2[j];
         }
         det =    rr2*t1s1*t2s2 + rs1*t1s2*t2r + rs2*t1r*t2s1
                 - rr2*t1s2*t2s1 - rs1*t1r*t2s2 - rs2*t1s1*t2r;
         angfac = 1 + det/rr2/AsAt; /* 0 for cospherical faces */
         pp = pow(angfac,cpower);
      }
      energy += AsAt * pp / pow(2*rr2,spower);
    }
  return 2*energy/4;  /* As,At are 2*areas; also we want full REAL sum */
}

/**************************************************************
*
*  function: sphere_knot_energy_gradient()
*  
*  purpose: calculates energy of one facet's vertices due to potential
*              with all others.
*
*  input: info about facet is in qinfo structure.
*
*/

REAL sphere_knot_energy_gradient(f_info)
struct qinfo *f_info;
{ facet_id f1 = f_info->id,f2;
  REAL *x[FACET_VERTS],*y[FACET_VERTS]; /* vertex coordinates */
  REAL energy = 0.0;
  REAL s1[MAXCOORD],s2[MAXCOORD];
  REAL t1[MAXCOORD],t2[MAXCOORD];
  REAL r[MAXCOORD];
  REAL dAs1[MAXCOORD],dAs2[MAXCOORD];
  REAL rr2,rs1,rs2,t1r,t1s1,t1s2,t2r,t2s1,t2s2;
  REAL s1s1,s1s2,s2s2,t1t1,t1t2,t2t2;
  REAL dEdrr2,dEdrs1,dEdrs2,dEdt1r,dEdt1s1,dEdt1s2,dEdt2r,dEdt2s1,dEdt2s2;
  REAL angfac,mult,qq;
  REAL As,AsAt,detr;
  facetedge_id fe;
  vertex_id v[FACET_VERTS],w[FACET_VERTS];
  int i,j;

  for ( i = 0 ; i < FACET_VERTS ; i++ )
     for ( j = 0 ; j < SDIM ; j++ ) f_info->grad[i][j] = 0.0;
  fe = get_facet_fe(f1);
  for ( j = 0 ; j < FACET_VERTS ; j++ )
    { v[j] = get_fe_tailv(fe);
      x[j] = get_coord(v[j]);
      fe = get_next_edge(fe);
    }

  for ( j = 0 ; j < SDIM ; j++ )
     { s1[j] = x[1][j] - x[0][j];
        s2[j] = x[2][j] - x[0][j];
     }
  s1s1 = SDIM_dot(s1,s1);
  s1s2 = SDIM_dot(s1,s2);
  s2s2 = SDIM_dot(s2,s2);
  As = sqrt(s1s1*s2s2 - s1s2*s1s2);
  for ( j = 0 ; j < SDIM ; j++ ) 
  {
          dAs1[j] = (s2s2*s1[j] - s1s2*s2[j])/As;
          dAs2[j] = (s1s1*s2[j] - s1s2*s1[j])/As;
  }
  FOR_ALL_FACETS(f2)
    { if ( f1 == f2 ) continue; /* don't do self */
      fe = get_facet_fe(f2);
      for ( j = 0 ; j < FACET_VERTS ; j++ )
        { w[j] = get_fe_tailv(fe);
          y[j] = get_coord(w[j]);
          fe = get_next_edge(fe);
        }
      t1t1=t1t2=t2t2=rr2=rs1=rs2=t1r=t1s1=t1s2=t2r=t2s1=t2s2=0.0;
      for (j=0; j<SDIM; j++)
      {
          t1[j] = y[1][j] - y[0][j];
          t2[j] = y[2][j] - y[0][j];
          t1t1 += t1[j]*t1[j]; t1t2 += t1[j]*t2[j]; t2t2 += t2[j]*t2[j];
      }
      for (j=0; j<SDIM; j++)
      {
          r[j] = (y[0][j]+y[1][j]+y[2][j] - (x[0][j]+x[1][j]+x[2][j]))/3;
          rr2 += r[j]*r[j]; t1r += t1[j]*r[j]; t2r += t2[j]*r[j];
          rs1 += r[j]*s1[j]; t1s1 += t1[j]*s1[j]; t2s1 += t2[j]*s1[j];
          rs2 += r[j]*s2[j]; t1s2 += t1[j]*s2[j]; t2s2 += t2[j]*s2[j];
      }
      rr2 /= 2;
      AsAt = As*sqrt(t1t1*t2t2-t1t2*t1t2);
      if (fabs(cpower)<.0001)
      {
            REAL q;
            q = AsAt/2/pow(2*rr2,spower);
            energy += q/2;
            mult = q/As;
            for ( j = 0 ; j < SDIM ; j++ ) 
             {
                  register REAL ff1, ff2;
                  ff1 = mult*dAs1[j]; ff2 = mult*dAs2[j];
                  f_info->grad[0][j] -= ff1+ff2;
                  f_info->grad[1][j] += ff1;
                  f_info->grad[2][j] += ff2;
             }
            dEdrr2 =  - spower*q/rr2/3;
            for ( j = 0 ; j < SDIM ; j++ ) 
             {
                register REAL common;
                common = dEdrr2*r[j];
                f_info->grad[0][j] -= common;
                f_info->grad[1][j] -= common;
                f_info->grad[2][j] -= common;
             }
             continue;
      }
      detr = (rr2*t1s1*t2s2 + rs1*t1s2*t2r + rs2*t1r*t2s1 - rr2*t1s2*t2s1
                 - rs1*t1r*t2s2 - rs2*t1s1*t2r)/rr2;
      angfac = 1 + detr/AsAt;
      if ( cpower == 1.0 )
          qq = 1.0 / pow(2*rr2,spower+1);
      else 
          qq = pow(angfac,cpower-1) / pow(2*rr2,spower+1);
      energy += qq*angfac*rr2*AsAt/2;

      /* mult = enf*(AsAt-detr)/As; */
      mult = qq*rr2 * (angfac - cpower*detr/AsAt) * AsAt/As;
      for ( j = 0 ; j < SDIM ; j++ ) 
        {
             register REAL ff1, ff2;
             ff1 = mult*dAs1[j]; ff2 = mult*dAs2[j];
             f_info->grad[0][j] -= ff1+ff2;
             f_info->grad[1][j] += ff1;
             f_info->grad[2][j] += ff2;
        }

      mult = cpower*qq/3;
      dEdrr2 = mult*(t1s1*t2s2 - t1s2*t2s1 - detr) - spower*qq*angfac*AsAt/3;
      dEdrs1 = mult*(t1s2*t2r - t1r*t2s2); dEdrs2 = mult*(t1r*t2s1 - t1s1*t2r);
      for ( j = 0 ; j < SDIM ; j++ ) 
        {
          register REAL rs1r, rs2r, common;
          rs1r = dEdrs1*r[j]*3; rs2r = dEdrs2*r[j]*3;
          common = dEdrr2*r[j] + dEdrs1*s1[j] + dEdrs2*s2[j];
          f_info->grad[0][j] -= (common + rs1r + rs2r);
          f_info->grad[1][j] -= (common - rs1r);
          f_info->grad[2][j] -= (common - rs2r);
        }
      dEdt1r = mult*(rs2*t2s1 - rs1*t2s2); dEdt2r = mult*(rs1*t1s2 - rs2*t1s1);
      mult *= 3;
      dEdt1s1 = mult*(rr2*t2s2 - rs2*t2r); dEdt1s2 = mult*(rs1*t2r - rr2*t2s1);
      dEdt2s1 = mult*(rs2*t1r - rr2*t1s2); dEdt2s2 = mult*(rr2*t1s1 - rs1*t1r);
      for ( j = 0 ; j < SDIM ; j++ ) 
        {
          register REAL tr, ts1, ts2;
          tr = dEdt1r*t1[j] + dEdt2r*t2[j];
          ts1 = dEdt1s1*t1[j] + dEdt2s1*t2[j];
          ts2 = dEdt1s2*t1[j] + dEdt2s2*t2[j];
          f_info->grad[0][j] -= (tr+ts1+ts2);
          f_info->grad[1][j] -= (tr-ts1);
          f_info->grad[2][j] -= (tr-ts2);
        }
    }

  return energy;  /* since doing all pairs */
}
