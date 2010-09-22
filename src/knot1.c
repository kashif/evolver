/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/


/******************************************************************
*
*    file:        knotenergy.c
*
*    Contents:  Functions calculating knot energy and its gradient
*
*                  Has both equilibrium charge distribution
*                  (for use with length constraint) and
*                  uniform charge disitribution.
*      Also has thickness (ropelength) added 01Jun by JMS
*/

#include "include.h"

/* for accessing knot energy exponent as adjustable parameter */
int exponent_param;  /* parameter number */
#define KNOTPOWER_NAME "knot_power"  /* name in datafile */

static int ke_power_flag; /* 0: real, 1: integer, -1: log */
static int ke_power_i;        /* half of even integer power */
static int ke_power_l;        /* log_2 of power */
static REAL ke_power;         /* half of power */

#define CHARGE_ATTR_NAME "node_charge"
int charge_attr = -1;

/* prototypes to keep some compilers happy */
REAL radp ARGS((REAL *, REAL *, REAL *));
REAL dradp ARGS((REAL*, REAL*, REAL*, REAL*, REAL));
void xfacet_knot_energy_init ARGS((int,struct method_instance *));
REAL xfacet_knot_energy ARGS(( struct qinfo *));
REAL xfacet_knot_energy_gradient ARGS(( struct qinfo *));
REAL rad ARGS((REAL*,REAL*,REAL*));

/***************************************************************
*
*  function: knot_power_init()
*
*  purpose: initialization for all knot_energies which have variable power
*/

void knot_power_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{
  charge_attr = find_attribute(VERTEX,CHARGE_ATTR_NAME);
          /* see if charge attribute */

  exponent_param = lookup_global(KNOTPOWER_NAME);
  if ( exponent_param < 0 ) /* missing, so add */
    { exponent_param = add_global(KNOTPOWER_NAME);
      globals(exponent_param)->value.real = 2.0;  /* default */
      globals(exponent_param)->flags |=  ORDINARY_PARAM | RECALC_PARAMETER | ALWAYS_RECALC;
    }
  ke_power = globals(exponent_param)->value.real/2; /* since rr is squared */
  ke_power_i = (int) ke_power;
  ke_power_flag = 0; /* use REAL by default */
  if (ke_power<0.) return; /* don't do anything special for negative powers */
  ke_power_l = (int) (log(ke_power)/log(2.)); /* see if power of 2 */
  if ( fabs(ke_power_l - log(ke_power)/log(2.)) < 1e-8  && ke_power_l <= 20)
    ke_power_flag = -1;
  else if ( fabs(ke_power_i - ke_power) < 1e-12 && ke_power <= 150)
    ke_power_flag = 1;
}

#define ke_pow(r,p) \
 {if (ke_power_flag>0) {int i; for (p = r,i=1; i<ke_power_i; i++) p *= r;}\
  else if (ke_power_flag<0) {int i; for (p=r,i=0; i<ke_power_l; i++) p *= p;}\
  else p = pow(r,ke_power);}

/**************************************************************
*
*  function: radp(a, b, c)
*
*  purpose: calculates radius of circle through three points,
*            raised to -2p power (p=ke_power).
*  written to only work in 3d
*/

REAL radp(a, b, c)
REAL *a, *b, *c;
{
  REAL vab[3], vbc[3], vca[3];
  REAL lab=0, lbc=0, lca=0, Area=0, rr, temp;
  int i;
  for ( i = 0 ; i < 3 ; i++ ) {
    vab[i] = b[i] - a[i]; lab += vab[i]*vab[i];
    vbc[i] = c[i] - b[i]; lbc += vbc[i]*vbc[i];
    vca[i] = a[i] - c[i]; lca += vca[i]*vca[i];
  }
  /*lab=SDIM_dot(vab,vab); lbc=SDIM_dot(vbc,vbc); lca=SDIM_dot(vca,vca);*/
  /*cross_prod(vca,vab,Ar); Area = SDIM_dot(Ar,Ar);*/
  temp = vca[0]*vab[1]-vca[1]*vab[0]; Area += temp*temp;
  temp = vca[1]*vab[2]-vca[2]*vab[1]; Area += temp*temp;
  temp = vca[2]*vab[0]-vca[0]*vab[2]; Area += temp*temp;
  rr = 4*Area/(lab*lbc*lca); if (rr==0.) return 0;
  ke_pow(rr,temp);
  return temp;
}


/**************************************************************
*
*  function: rad(a, b, c)
*
*  purpose: calculates radius of circle through three points,
*            same as radp for p=1;
*  written to only work in 3d
*/

REAL rad(a, b, c)
REAL *a, *b, *c;
{
/******
  REAL vab[MAXCOORD], vbc[MAXCOORD], vca[MAXCOORD], Ar[MAXCOORD];
  REAL lab, lbc, lca, Area, rr; int i;
  for ( i = 0 ; i < SDIM ; i++ )
    {vab[i] = b[i] - a[i]; vbc[i] = c[i] - b[i]; vca[i] = a[i] - c[i];}
  lab = SDIM_dot(vab,vab); lbc = SDIM_dot(vbc,vbc); lca = SDIM_dot(vca,vca);
  cross_prod(vca,vab,Ar); Area = SDIM_dot(Ar,Ar);
******/
  REAL vab[3], vbc[3], vca[3];
  REAL lab=0, lbc=0, lca=0, Area=0, temp;
  int i;
  for ( i = 0 ; i < 3 ; i++ ) {
    vab[i] = b[i] - a[i]; lab += vab[i]*vab[i];
    vbc[i] = c[i] - b[i]; lbc += vbc[i]*vbc[i];
    vca[i] = a[i] - c[i]; lca += vca[i]*vca[i];
  }
  temp = vca[0]*vab[1]-vca[1]*vab[0]; Area += temp*temp;
  temp = vca[1]*vab[2]-vca[2]*vab[1]; Area += temp*temp;
  temp = vca[2]*vab[0]-vca[0]*vab[2]; Area += temp*temp;
/*****/
  return sqrt(lab*lbc*lca/(4*Area));
}

/**************************************************************
*
*  function: dradp(a, b, c, result)
*
*  purpose: calculates gradient, w.r.t. a, of radp(a,b,c)
*       and adds it (times mult) to result; returns radp (times mult).
*  written to only work in 3d
*/

REAL dradp(a, b, c, result, mult)
REAL *a, *b, *c, *result, mult;
{
  REAL vab[3], vbc[3], vca[3], Ar[3], tmp[3];
  REAL lab=0, lbc=0, lca=0, Area, rp, rr;
  int i;
  for ( i = 0 ; i < 3 ; i++ ) {
    vab[i] = b[i] - a[i]; lab += vab[i]*vab[i];
    vbc[i] = c[i] - b[i]; lbc += vbc[i]*vbc[i];
    vca[i] = a[i] - c[i]; lca += vca[i]*vca[i];
  }
  /*lab=SDIM_dot(vab,vab); lbc=SDIM_dot(vbc,vbc); lca=SDIM_dot(vca,vca);*/
  cross_prod(vca,vab,Ar); Area = SDIM_dot(Ar,Ar);

  rr = 4*Area/(lab*lbc*lca); if (rr==0.) return 0;
  ke_pow(rr,rp); rp *= mult;
  mult = -2*ke_power*rp; cross_prod(vbc,Ar,tmp);
  for ( i = 0 ; i < SDIM ; i++ )
    result[i] += mult*(vca[i]/lca - vab[i]/lab + tmp[i]/Area);
  return rp;
}

/**************************************************************
*
*  function: knot_thickness_0()
*  
*  purpose: calculates global radius of curvature at one vertex,
*              raised to -p power.  No factor of length in integral.
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL knot_thickness_0(v_info)
struct qinfo *v_info;
{ REAL *a = get_coord(v_info->id), *b, *c;
  REAL thick = 0.0;  /* for this vertex */
  edge_id e_id;  /* edge to create triple */

  FOR_ALL_EDGES(e_id)
  { vertex_id eh_id = get_edge_headv(e_id);
    vertex_id et_id = get_edge_tailv(e_id);
    if ( v_info->id == eh_id || v_info->id == et_id ) continue; /* incident */
    b = get_coord(eh_id); c = get_coord(et_id);
    thick += radp(a,b,c);
  }
  return thick;
}


/**************************************************************
*
*  function: knot_thickness_0_gradient()
*  
*  purpose: gradient of knot_thickness_0
*
*/

REAL knot_thickness_0_gradient(v_info)
struct qinfo *v_info;
{ REAL *a , *b, *c, *d;
  REAL thick = 0.0;  /* thickness at this vertex */
  edge_id e_id;  /* edge to create triple */
  vertex_id v_id, v1_id, v2_id;  /* vertex to create triple */

  a = get_coord(v_info->id);
  FOR_ALL_EDGES(e_id)
  { vertex_id eh_id = get_edge_headv(e_id);
    vertex_id et_id = get_edge_tailv(e_id);
    if ( v_info->id == eh_id || v_info->id == et_id ) continue; /* incident */
    b = get_coord(eh_id); c = get_coord(et_id);
    thick += dradp(a,b,c, v_info->grad[0], 1.);
  }
  e_id = get_vertex_edge(v_info->id);
  b = get_coord(v1_id=get_edge_headv(e_id));
  e_id = get_next_tail_edge(e_id);  /* assuming <= two edges per vertex */
  c = get_coord(v2_id=get_edge_headv(e_id)); /* b and c are now two nbrs of v */
  if (v1_id != v2_id) /* skip this if v had valence 1 */
    dradp(a,b,c, v_info->grad[0], 2.); /* case of triples (v,v1,v2),(v,v2,v1) */
  FOR_ALL_VERTICES(v_id)
  { if (v_id == v_info->id || v_id == v1_id || v_id == v2_id) continue;
    d = get_coord(v_id);
    dradp(a,b,d, v_info->grad[0], 1.);
    if (v1_id != v2_id)
      dradp(a,c,d, v_info->grad[0], 1.);
  }
  return thick;
}


/**************************************************************
*
*  function: knot_thickness()
*  
*  purpose: calculates global radius of curvature at one vertex v,
*      as min of  r(v,eh,et).
*  Because of "min", this has no gradient.
*  It should be used with  "min(vertex,thick)",  not  "total thick"
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL knot_thickness(v_info)
struct qinfo *v_info;
{ REAL *a = get_coord(v_info->id), *b, *c;
  REAL r, thick = -1.;
  edge_id e_id;  /* edge to create triple */

  FOR_ALL_EDGES(e_id)
  { vertex_id eh_id = get_edge_headv(e_id);
    vertex_id et_id = get_edge_tailv(e_id);
    if ( v_info->id == eh_id || v_info->id == et_id ) continue; /* incident */
    b = get_coord(eh_id); c = get_coord(et_id);
    r = rad(a,b,c);
    if (thick < 0 || r < thick) thick = r;
  }
  return thick; /* returns -1 if component only has 2 edges */
}


/**************************************************************
*
*  function: knot_thickness2()
*  
*  purpose: calculates global radius of curvature at one vertex v,
*      as min of  r(v,w1,w2) where w1 and w2 are nbrs of some w.
*  Because of "min", this has no gradient.
*  It should be used with  "min(vertex,thick)",  not  "total thick"
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL knot_thickness2(v_info)
struct qinfo *v_info;
{ REAL *a = get_coord(v_info->id), *b, *c;
  REAL r, thick = -1.;
  vertex_id v_id,w_id,w1_id,w2_id;  /* vertex and neighbors to create triple */
  edge_id e_id;

  v_id = v_info->id;
  FOR_ALL_VERTICES(w_id)
  {
    e_id = get_vertex_edge(w_id);
    if ((w1_id = get_edge_headv(e_id))==v_id) continue;
    b = get_coord(w1_id);
    e_id = get_next_tail_edge(e_id);  /* assuming two edges per vertex */
    if ((w2_id = get_edge_headv(e_id))==v_id) continue;
    c = get_coord(w2_id);
    r = rad(a,b,c);
    if (thick < 0 || r < thick) thick = r;
  }
  return thick;
}

/**************************************************************
*
*  function: knot_local_thickness()
*  
*  purpose: calculates local radius of curvature at one vertex v,
*      as r(v1,v,v2), where v1 and v2 are nbrs of v.
*  No gradient or L^p version for now; intended for use at
*  individual vertices.
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL knot_local_thickness(v_info)
struct qinfo *v_info;
{ REAL *a = get_coord(v_info->id), *b, *c;
  edge_id e_id;  /* edge to create triple */

  e_id = get_vertex_edge(v_info->id);
  b = get_coord(get_edge_headv(e_id));
  e_id = get_next_tail_edge(e_id);  /* assuming exactly two edges per vertex */
  c = get_coord(get_edge_headv(e_id)); /* b and c are two nbrs of v */

  return rad(b,a,c);
}

/**************************************************************
*
*  function: knot_thickness_p2()
*  
*  purpose: calculates global radius of curvature at one vertex v,
*      as Lp integral of  r(v,w1,w2).  Includes factors of length at v and w.
*      Here w1 and w2 are the two neighbors of vertex w.
*      This has not been extended to allow open arcs (valence 1 verts).
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL knot_thickness_id ARGS((vertex_id));
REAL knot_thickness_id2 ARGS((vertex_id));

REAL knot_thickness_p2(v_info)
struct qinfo *v_info;
{ return knot_thickness_id2(v_info->id); }

REAL knot_thickness_id2(v_id)
vertex_id v_id;
{ REAL *a = get_coord(v_id), *b, *c;
  REAL thick = 0.0;  /* for this vertex */
  vertex_id w_id,w1_id,w2_id;  /* vertex and neighbors to create triple */
  edge_id e_id;  /* temp edge */

  FOR_ALL_VERTICES(w_id)
  {
    if (w_id==v_id) continue;  /* don't include local thickness at v */
    e_id = get_vertex_edge(w_id);
    if ((w1_id = get_edge_headv(e_id))==v_id) continue;
    b = get_coord(w1_id);
    e_id = get_next_tail_edge(e_id);  /* assuming two edges per vertex */
    if ((w2_id = get_edge_headv(e_id))==v_id) continue;
    c = get_coord(w2_id);
    thick += radp(a,b,c) * get_vertex_star(w_id);
  }
  return thick * get_vertex_star(v_id);
}


/**************************************************************
*
*  function: knot_thickness_p2_gradient()
*  
*  purpose: calculates gradient of global radius of curvature at one vertex v,
*      as Lp integral of  r(v,w1,w2).  Includes factors of length at v and w.
*      Here w1 and w2 are the two neighbors of vertex w.
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL knot_thickness_p2_gradient(v_info)
struct qinfo *v_info;
{ vertex_id v_id = v_info->id;
  REAL *a = get_coord(v_id), *b, *c, *d, *bb, *cc;
  REAL temp[MAXCOORD], e1[MAXCOORD], e2[MAXCOORD];
  REAL l1, l2, lv1, lv2, lv;
  REAL thick = 0.0;  /* for this vertex */
  vertex_id v1_id,v2_id, w_id,w1_id,w2_id, v11_id,v22_id;  /* neighbors */
  edge_id e_id, e1_id, e2_id;  /* temp edges */
  int i;

  e1_id = get_vertex_edge(v_id);
  v1_id=get_edge_headv(e1_id); lv1 = get_vertex_star(v1_id);
  get_edge_side(e1_id,e1); l1 = get_edge_length(e1_id);
  e2_id = get_next_tail_edge(e1_id); /* assuming two edges per vertex */
  v2_id=get_edge_headv(e2_id); lv2 = get_vertex_star(v2_id);
   /* v1 and v2 are two nbrs of v  and lv1,lv2 are their star lengths */
  get_edge_side(e2_id,e2); l2 = get_edge_length(e2_id);
    /* e1, e2 are edge vectors out of v; l1, l2 are their lengths */
  for ( i = 0 ; i < SDIM ; i++ )
  {
    e1[i] /= 2*l1; e2[i] /= 2*l2; /* now they're half unit vectors */
    temp[i] = 0;
  }
  lv = get_vertex_star(v_id); /* should be half of l1+l2 */

  FOR_ALL_VERTICES(w_id)
  {
    if (w_id == v_id) continue;
    e_id = get_vertex_edge(w_id);
    if ((w1_id = get_edge_headv(e_id))==v_id) continue;
    e_id = get_next_tail_edge(e_id);  /* assuming two edges per vertex */
    if ((w2_id = get_edge_headv(e_id))==v_id) continue;
    thick += dradp(a,get_coord(w1_id),get_coord(w2_id),
                 temp, get_vertex_star(w_id));
  }     /* now temp is sum_w l(w)*d_v(rp(v,w1,w2)) */
  for ( i = 0 ; i < SDIM ; i++ )
    v_info->grad[0][i] += (temp[i]*lv - thick*(e1[i]+e2[i]));
    /* so far we should have the gradient of knot_thickness_p2(v_info) */
    /* which is: d_v(l(v))*thick + l(v)*temp */
  thick *= lv;  /* this is the energy at v, to be returned */

    /* Now we must compute derivative due to v of all other terms,
    first just its effect on length for energy of neighbors, then
    when v is playing the role of w (affects only length) or w1 or w2 */
  for ( i = 0 ; i < SDIM ; i++ )
    v_info->grad[0][i] -= (  knot_thickness_id2(v1_id)*e1[i]/lv1
               + knot_thickness_id2(v2_id)*e2[i]/lv2 );
    /* this is effect of v on l(v1) and l(v2) in energy at v1 and v2 */

  b = get_coord(v1_id); c = get_coord(v2_id);  /* will need these nbrs */
  e_id = get_next_head_edge(e1_id); /* move out beyond v1 */
  bb = get_coord(v11_id=get_edge_tailv(e_id)); /* coords of next vertex */
  e_id = get_next_head_edge(e2_id); /* move out beyond v2 */
  cc = get_coord(v22_id=get_edge_tailv(e_id)); /* coords of next vertex */

  FOR_ALL_VERTICES(w_id)
  { /* here w is playing the role of v, and v that of w (or w1 or w2) */
    REAL t,lw;
    if (w_id == v_id) continue; /* not allowed in any of the three */
    d = get_coord(w_id); lw = get_vertex_star(w_id);

    if (w_id != v1_id && w_id != v2_id)
    {
      t = radp(d,b,c) * lw;
      for ( i = 0 ; i < SDIM ; i++ )
        v_info->grad[0][i] -= t * (e1[i]+e2[i]); /* v as w affects len only */
    }

    /* now, when v1=b is w, v=a is w1, and v11=bb is w2 */
    if (w_id != v1_id && w_id != v11_id)
    {
      t = dradp(a,bb,d, v_info->grad[0], lv1*lw) / lv1; /* effect on radp() */
      for ( i = 0 ; i < SDIM ; i++ )
        v_info->grad[0][i] -= t*e1[i]; /* finally, effects on l(v1) */
    }

    /* now, when v2=c is w, v=a is w2, and cc is w1 */
    if (w_id != v2_id && w_id != v22_id)
    {
      t = dradp( a,cc,d, v_info->grad[0], lv2*lw ) / get_vertex_star(v2_id);
      for ( i = 0 ; i < SDIM ; i++ )
        v_info->grad[0][i] -= t*e2[i];
    }
  }
 return thick * get_vertex_star(v_id);
}


/**************************************************************
*
*  function: knot_thickness_p()
*  
*  purpose: calculates global radius of curvature at one vertex v,
*      as Lp integral of  r(v,w,w).  Includes factors of length at v and w.
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL knot_thickness_id();

REAL knot_thickness_p(v_info)
struct qinfo *v_info;
{ return knot_thickness_id(v_info->id); }

REAL knot_thickness_id(v_id)
vertex_id v_id;
{ REAL *a = get_coord(v_id), *b, *c;
  REAL thick = 0.0;  /* for this vertex */
  edge_id e_id;  /* edge to create triple */

  FOR_ALL_EDGES(e_id)
  { vertex_id eh_id = get_edge_headv(e_id);
    vertex_id et_id = get_edge_tailv(e_id);
    if ( v_id == eh_id || v_id == et_id ) continue; /* incident */
    b = get_coord(eh_id); c = get_coord(et_id);
    thick += radp(a,b,c) * get_edge_length(e_id);
  }
  return thick * get_vertex_star(v_id); /* this is rp(v,eh,et)*l(e)*l(v) */
}


/**************************************************************
*
*  function: knot_thickness_p_gradient()
*  
*  purpose: gradient of knot_thickness_p
*
*/

REAL knot_thickness_p_gradient(v_info)
struct qinfo *v_info;
{ REAL *a , *b, *c, *d;
  REAL temp[MAXCOORD], e1[MAXCOORD], e2[MAXCOORD], l1, l2;
  REAL thick = 0.0;  /* thickness at this vertex */
  edge_id e_id;  /* edge to create triple */
  vertex_id w_id, v1_id, v2_id;  /* vertex to create triple */
  int i;

  a = get_coord(v_info->id);
  e_id = get_vertex_edge(v_info->id);
  v1_id=get_edge_headv(e_id);
  get_edge_side(e_id,e1); l1 = get_edge_length(e_id);
  e_id = get_next_tail_edge(e_id);  /* assuming <= two edges per vertex */
  v2_id=get_edge_headv(e_id); /* v1 and v2 are two nbrs of v */
  get_edge_side(e_id,e2); l2 = get_edge_length(e_id);
  if (v1_id == v2_id) for (i=0; i<SDIM; i++) e2[i] = 0.; /* v val. 1, no e2 */

  for ( i = 0 ; i < SDIM ; i++ ) temp[i] = 0.;
  FOR_ALL_EDGES(e_id)
  { vertex_id eh_id = get_edge_headv(e_id);
    vertex_id et_id = get_edge_tailv(e_id);
    if ( v_info->id == eh_id || v_info->id == et_id ) continue; /* incident */
    b = get_coord(eh_id); c = get_coord(et_id);
    thick += dradp(a,b,c,temp, get_edge_length(e_id));
  }     /* now temp is sum_e l(e)*d_v(rp(v,eh,et)) */
  for ( i = 0 ; i < SDIM ; i++ )
    v_info->grad[0][i] += (temp[i] * get_vertex_star(v_info->id) -
                             thick * (e1[i]/l1 + e2[i]/l2)/2);
    /* so far we should have the gradient of knot_thickness_p(v_info) */
    /* which is: d_v(l(v))*thick + l(v)*temp */
  thick *= get_vertex_star(v_info->id);

    /* Next we consider the triple v1,v,v2,
       which enters into thickness at v1 and v2 */
  b = get_coord(v1_id); c = get_coord(v2_id);
  if (v1_id != v2_id) /* skip if v valence 1 */
  {
    dradp(a,b,c, v_info->grad[0],       /* triples (v,v1,v2), (v,v2,v1) */
        l1*get_vertex_star(v2_id) + l2*get_vertex_star(v1_id));
	/* this term gets d_v rp(a,b,c) times appropriate lengths */
    for ( i = 0 ; i < SDIM ; i++ )
    { v_info->grad[0][i] -= radp(a,b,c) *  /* - is from orientation of ei */
			  ( get_vertex_star(v2_id)*e1[i]/l1 +
			    get_vertex_star(v1_id)*e2[i]/l2  );
	/* term above accounts for changing lengths at v as l(e) factor */
      v_info->grad[0][i] -=
       (  knot_thickness_id(v1_id)*e1[i]/l1/get_vertex_star(v1_id)/2
        + knot_thickness_id(v2_id)*e2[i]/l2/get_vertex_star(v2_id)/2 );
	/* this is effect of v on l(v1) and l(v2) in energy at v1 and v2 */
    }
  }

  FOR_ALL_VERTICES(w_id)
  { if (w_id == v_info->id || w_id == v1_id || w_id == v2_id) continue;
    d = get_coord(w_id);
    dradp(a,b,d, v_info->grad[0], l1*get_vertex_star(w_id));
    if (v1_id != v2_id) dradp(a,c,d, v_info->grad[0], l2*get_vertex_star(w_id));
	/* these are changes d_v (rp(w,ei)) times lengths, i=1,2 */
    for ( i = 0 ; i < SDIM ; i++ )
      v_info->grad[0][i] -= (  radp(a,b,d) * get_vertex_star(w_id) * e1[i]/l1
                             + radp(a,c,d) * get_vertex_star(w_id) * e2[i]/l2 );
        /* v's effect on l(ei), i=1,2 */
  }
  return thick;
}


/**************************************************************
*
*  function: knot_energy()
*  
*  purpose: calculates energy of one vertex due to potential
*              with all others.
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL knot_energy(v_info)
struct qinfo *v_info;
{ REAL *x = get_coord(v_info->id);
  REAL energy = 0.0;  /* for this vertex */
  vertex_id v_id;  /* other vertex */
  int i;
  REAL d[MAXCOORD]; /* difference vector between vertices */
  REAL rr,p;
  REAL vcharge = charge_attr >= 0 ? 
        *((REAL*)get_extra(v_info->id,charge_attr)) : 1.;

  FOR_ALL_VERTICES(v_id)
  { REAL *y = get_coord(v_id);
    REAL charges;
    if ( v_id <= v_info->id ) continue; /* each pair once */
    charges = charge_attr >= 0 ? 
                  *((REAL*)get_extra(v_id,charge_attr))*vcharge : 1.;
    for ( i = 0 ; i < SDIM ; i++ ) d[i] = x[i] - y[i];
    rr = SDIM_dot(d,d);
    ke_pow(rr,p);
    energy += charges/p;  /* inverse power potential */
  }
  return 2*energy; /* since each pair once */
}



/**************************************************************
*
*  function: knot_energy_gradient()
*  
*  purpose: calculates energy of one vertex due to potential
*              with all others.
*
*  input: info about vertex is in global qinfo structure.
*
*/

REAL knot_energy_gradient(v_info)
struct qinfo *v_info;
{ REAL *x = get_coord(v_info->id);
  REAL energy = 0.0;  /* for this vertex */
  vertex_id v_id;
  int i;
  REAL d[MAXCOORD]; /* difference vector between vertices */
  REAL rr,p,fact;
  REAL vcharge = charge_attr >= 0 ? 
        *((REAL*)get_extra(v_info->id,charge_attr)) : 1.;

  FOR_ALL_VERTICES(v_id)
  { REAL *y = get_coord(v_id);
    REAL charges;
    if ( equal_id(v_info->id,v_id) ) continue;
    charges = charge_attr >= 0 ? 
              *((REAL*)get_extra(v_id,charge_attr))*vcharge : 1.;
    for ( i = 0 ; i < SDIM ; i++ ) d[i] = x[i] - y[i];
    rr = SDIM_dot(d,d);
    ke_pow(rr,p);
    energy += charges/p;
    fact = 2*ke_power/(p*rr);
    for ( i = 0 ; i < SDIM ; i++ ) 
      v_info->grad[0][i] -= 2*charges*fact*d[i];
  }
  return energy;
}

/*******************************************************
*
*     function: knot_energy_hessian()
*
*     purpose: calculate knot energy hessian
*/


REAL knot_energy_hessian(v_info)
struct qinfo *v_info;
{
  REAL *x = get_coord(v_info->id);
  REAL energy = 0.0;  /* for this vertex */
  vertex_id w_id;
  int i,j;
  REAL d[MAXCOORD]; /* difference vector between vertices */
  REAL rr,p,fact;
  MAT2D(hessvw,MAXCOORD,MAXCOORD);
  MAT2D(hessvv,MAXCOORD,MAXCOORD);
  REAL modulus;
  REAL vcharge = charge_attr >= 0 ? 
        *((REAL*)get_extra(v_info->id,charge_attr)) : 1.;

  modulus = METH_INSTANCE(v_info->method)->modulus
                  *GEN_QUANT(METH_INSTANCE(v_info->method)->quant)->modulus;
  /* need modulus due to direct insertion into hessian */

  for(i=0;i<SDIM;i++) for(j=0;j<SDIM;j++) hessvv[i][j]=0.;

  FOR_ALL_VERTICES(w_id)
  { REAL *y = get_coord(w_id);
    REAL charges;
    if ( equal_id(v_info->id,w_id) ) continue;
    charges = charge_attr >= 0 ? 
              *((REAL*)get_extra(w_id,charge_attr))*vcharge : 1.;
    for ( i = 0 ; i < SDIM ; i++ ) d[i] = x[i] - y[i];
    rr = SDIM_dot(d,d);
    ke_pow(rr,p);
    energy += charges/p;
    fact = 2*ke_power/(p*rr);
    for ( i = 0 ; i < SDIM ; i++ ) 
      v_info->grad[0][i] -= 2*charges*fact*d[i];
    for ( i=0; i<SDIM; i++ )
      for ( j=0; j<SDIM; j++ )
      {
        register REAL hessij = fact*((2*ke_power+2)/rr * d[i]*d[j] - (i==j));
        hessij *= modulus*charges;
        hessvw[i][j] = - hessij;
        hessvv[i][j] += 2*hessij;
      }
    fill_mixed_entry(v_info->S, v_info->id, w_id, hessvw);
  }
  fill_self_entry(v_info->S, v_info->id, hessvv);
  return modulus*energy;
}

/********************************************************************/
/*  This set of routines is for the gradient^2 of the last energy */
/*  assuming the points are constrained to the unit sphere */

static REAL *charge_grads = NULL;
static int cg_nverts = 0;

/***************************************************************
*
*  function: charge_gradient_init()
*
*  purpose: initialization for charge_gradient() and 
*              charge_gradient_gradient().
*/
void charge_gradient_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{
    vertex_id u_id,v_id;  /* two vertices */
    int nv;

    knot_power_init(mode,mi);

    if ((nv = web.skel[VERTEX].max_ord+1) != cg_nverts)
    {
         if (charge_grads) list_free((char*)charge_grads,ETERNAL_BLOCK);
         charge_grads = (REAL*)my_list_calloc(nv, MAXCOORD*sizeof(REAL),ETERNAL_BLOCK);
         cg_nverts = nv;
     }

    FOR_ALL_VERTICES(u_id)
    {
      int i;
      REAL *x = get_coord(u_id);
      REAL *w = charge_grads+MAXCOORD*ordinal(u_id);

      for ( i = 0 ; i < SDIM ; i++ ) w[i] = 0.;
      FOR_ALL_VERTICES(v_id)
      {
          REAL *y = get_coord(v_id);
          REAL d[MAXCOORD];         /* difference vector */
          REAL rr;

          if (u_id==v_id) continue;
          for ( i = 0 ; i < SDIM ; i++ ) d[i] = x[i] - y[i];
          rr = SDIM_dot(d,d);
          for ( i = 0 ; i < SDIM ; i++ ) w[i] -= d[i]/pow(rr,(2*ke_power+2)/2);
      }
   }
}

/**************************************************************
*
*  function: charge_gradient()
*  
*  purpose: calculates force^2 on one vertex from charges at all others
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL charge_gradient(v_info)
struct qinfo *v_info;
{
  REAL *x = get_coord(v_info->id);
  REAL *w = charge_grads+MAXCOORD*ordinal(v_info->id);
  REAL wx = SDIM_dot(w,x);
  return SDIM_dot(w,w) - wx*wx;
}


/**************************************************************
*
*  function: charge_gradient_gradient()
*  
*  purpose: calculates energy of one vertex due to potential
*              with all others.
*
*  input: info about vertex is in global qinfo structure.
*
*/

REAL charge_gradient_gradient(v_info)
struct qinfo *v_info;
{ REAL *xi = get_coord(v_info->id);
  vertex_id v_id;
  int i;
  REAL d[MAXCOORD]; /* difference vector between vertices */
  REAL *wi,*wj; /* force vectors at vertices */
  REAL yi[MAXCOORD], yd[MAXCOORD]; /* more forces */
  REAL rr,p,ddd;

  wi = charge_grads+MAXCOORD*ordinal(v_info->id);
  ddd = SDIM_dot(wi,xi);
  for ( i = 0 ; i < SDIM ; i++ ) 
      v_info->grad[0][i] = -2*ddd*wi[i]; /* diagonal term in gradient */
  for ( i = 0 ; i < SDIM ; i++ )  yi[i] = wi[i] - ddd*xi[i];
  FOR_ALL_VERTICES(v_id)
  { REAL *xj = get_coord(v_id);
    if ( equal_id(v_info->id,v_id) ) continue;
    for ( i = 0 ; i < SDIM ; i++ ) d[i] = xi[i] - xj[i];
    rr = SDIM_dot(d,d);
    wj = charge_grads+MAXCOORD*ordinal(v_id);
    ddd = SDIM_dot(wj,xj);
    for ( i = 0 ; i < SDIM ; i++ ) yd[i] = yi[i] - wj[i] + ddd*xj[i];
    ddd = SDIM_dot(yd,d);
    p = pow(rr,(2*ke_power+2)/2);  /* power of distance */
    for ( i = 0 ; i < SDIM ; i++ )
      v_info->grad[0][i] += (-2*yd[i] + 2*(2*ke_power+2)*ddd*d[i]/rr)/p;
  }
  return SDIM_dot(yi,yi);
}

/* Next set of routines is for uniform charge distribution */

/***************************************************************
*
*  function: uniform_knot_energy_init()
*
*  purpose: initialization for knot_energy() and 
*              knot_energy_gradient() and knot_thickness_p().
*/

void uniform_knot_energy_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{
  edge_id e_id;
  vertex_id v_id;

  knot_power_init(mode,mi);

  FOR_ALL_VERTICES(v_id)
  {
     set_vertex_star(v_id,0.);
     get_vertex_evalence(v_id);
  }
  FOR_ALL_EDGES(e_id)
  {
     calc_edge(e_id);
     add_vertex_star(get_edge_tailv(e_id),get_edge_length(e_id)/2);
     add_vertex_star(get_edge_headv(e_id),get_edge_length(e_id)/2);
  } /* each vertex_star is now half the total length of incident edges */
}

/**************************************************************
*
*  function: uniform_knot_energy()
*  
*  purpose: calculates energy of one vertex due to potential
*              with all others. Charge at vertex is length of star.
*
*  input: info about vertex is in global qinfo structure.
*
*/

REAL uniform_knot_energy(v_info)
struct qinfo *v_info;
{ REAL *x = get_coord(v_info->id);
  REAL energy = 0.0;  /* for this vertex */
  vertex_id v_id;  /* other vertex */
  int i;
  REAL d[MAXCOORD]; /* difference vector between vertices */
  REAL rr;
  REAL ti;  /* length weights of vertices, ti for home vertex */
  REAL p;

  ti = get_vertex_star(v_info->id);
  FOR_ALL_VERTICES(v_id)
  { REAL *y = get_coord(v_id);
    if ( v_id <= v_info->id ) continue; /* each pair once */
      for ( i = 0 ; i < SDIM ; i++ ) d[i] = x[i] - y[i];
    rr = SDIM_dot(d,d);
    ke_pow(rr,p);
    energy += ti*get_vertex_star(v_id)/p;
  }
  return 2*energy; /* since each pair once */
}


/**************************************************************
*
*  function: uniform_knot_energy_gradient()
*  
*  purpose: calculates energy of one vertex due to potential
*              with all others.
*
*  input: info about vertex is in global qinfo structure.
*
*/

REAL uniform_knot_energy_gradient(v_info)
struct qinfo *v_info;
{ REAL *x = get_coord(v_info->id);
  REAL energy = 0.0;  /* for this vertex */
  vertex_id v_id;
  int i;
  REAL d[MAXCOORD]; /* difference vector between vertices */
  REAL rr,p;
  REAL ti,tj;  /* length weights of vertices, ti for home vertex */
  edge_id e_id;
  REAL left[MAXCOORD],right[MAXCOORD]; /* edge vectors */
  REAL sum,sumleft,sumright;
  REAL *xleft,*xright;
  REAL leftmag,rightmag;

  for ( i = 0 ; i < SDIM ; i++ ) 
      v_info->grad[0][i] = 0.0; /* initialize gradient */
  ti = get_vertex_star(v_info->id);
  e_id = get_vertex_edge(v_info->id);
  xright = get_coord(get_edge_headv(e_id));
  e_id = get_next_tail_edge(e_id);  /* assuming exactly two edges per vertex */
  xleft = get_coord(get_edge_headv(e_id));
  sum = sumleft = sumright = 0.0;
  FOR_ALL_VERTICES(v_id)
  { REAL *y = get_coord(v_id);
    REAL fact;

    tj = get_vertex_star(v_id);

    for ( i = 0 ; i < SDIM ; i++ ) d[i] = x[i] - y[i];
    rr = SDIM_dot(d,d);
    if (v_id != v_info->id)
    {
      ke_pow(rr,p);
      fact = ti*tj/p;
      energy += fact;
      fact *= 4*ke_power/rr;
      for ( i = 0 ; i < SDIM ; i++ ) 
        v_info->grad[0][i] -= fact*d[i];
      sum += tj/p;
    }

    for ( i = 0 ; i < SDIM ; i++ ) d[i] = xleft[i] - y[i];
    rr = SDIM_dot(d,d);
    if ( rr > 1e-18) /* don't do self */
     { 
       ke_pow(rr,p);
       sumleft += tj/p;
     }

    for ( i = 0 ; i < SDIM ; i++ ) d[i] = xright[i] - y[i];
    rr = SDIM_dot(d,d);
    if ( rr > 1e-18) /* don't do self */
     {
       ke_pow(rr,p);
       sumright += tj/p;
     }
  }
  for ( i = 0 ; i < SDIM ; i++ )
  { left[i] = x[i] - xleft[i];
    right[i] = x[i] - xright[i];
  }
  leftmag = sqrt(SDIM_dot(left,left));
  rightmag = sqrt(SDIM_dot(right,right));
  for ( i = 0 ; i < SDIM ; i++ )
      v_info->grad[0][i] += ((left[i]/leftmag + right[i]/rightmag)*sum
                     + left[i]/leftmag*sumleft + right[i]/rightmag*sumright);
  return energy;
}

/******************************************************************
*
*  function: uniform_normalization()
*
*  purpose: calculates internal knot energy to normalize
*              singular divergence of integral.
*/

REAL uniform_normalization(v_info)
struct qinfo *v_info;
{
  vertex_id v_id;
  edge_id e_id,ee_id;
  REAL ti,tj;
  REAL dist=0.,energy=0.;
  REAL power;

  e_id = get_vertex_edge(v_info->id);
  if ( !valid_id(e_id) ) return 0.0;
  ti = get_edge_length(e_id);
  ee_id = get_next_tail_edge(e_id); /* assuming exactly two edges per vertex */
  ti = (ti + get_edge_length(ee_id))/2;

  power = globals(exponent_param)->value.real;
  for ( v_id = get_edge_headv(e_id) ; v_id != v_info->id ;
             e_id = ee_id, v_id = get_edge_headv(e_id) )
  {
    dist += (tj = get_edge_length(e_id));
    ee_id = inverse_id(get_next_head_edge(e_id));
    tj = (tj + get_edge_length(ee_id))/2;
    energy += ti*tj/pow(dist,power);
  }
  return 2*energy;
}

/******************************************************************
*
*  function: uniform_binormalization()
*
*  purpose: calculates internal knot energy to normalize
*              singular divergence of integral.  Uses shorter distance.
*/

REAL uniform_binormalization(v_info)
struct qinfo *v_info;
{
  vertex_id v_id;
  edge_id e_id,ee_id;
  REAL ti,tj;
  REAL dist=0.,energy=0.,comp_len;
  REAL power;

  e_id = get_vertex_edge(v_info->id);
  if ( !valid_id(e_id) ) return 0.0;
  comp_len = ti = get_edge_length(e_id);
  ee_id = get_next_tail_edge(e_id); /* assuming exactly two edges per vertex */
  ti = (ti + get_edge_length(ee_id))/2;

  power = globals(exponent_param)->value.real;
  for ( ee_id = inverse_id(get_next_head_edge(e_id)); ee_id != e_id;
          ee_id = inverse_id(get_next_head_edge(ee_id)) )
     comp_len += get_edge_length(ee_id);
  for ( v_id = get_edge_headv(e_id) ; v_id != v_info->id ;
             e_id = ee_id, v_id = get_edge_headv(e_id) )
  {
    dist += (tj = get_edge_length(e_id));
    ee_id = inverse_id(get_next_head_edge(e_id));
    tj = (tj + get_edge_length(ee_id))/2;
    energy += ti*tj/pow((2*dist<comp_len? dist : comp_len-dist), power);
  }
  return energy;
}

/***************************************************************
*
*  function: facet_knot_energy_init()
*
*  purpose: initialization for knot_energy() and 
*              knot_energy_gradient().
*/

#define SURF_KNOTPOW_NAME "surface_knot_power"

void facet_knot_energy_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{
  exponent_param = lookup_global(SURF_KNOTPOW_NAME);
  if ( exponent_param < 0 ) /* missing, so add */
  { exponent_param = add_global(SURF_KNOTPOW_NAME);
    globals(exponent_param)->value.real = 4.0;  /* default */
    globals(exponent_param)->flags |=  ORDINARY_PARAM | RECALC_PARAMETER | ALWAYS_RECALC;
  }
    

  {
  REAL *x;
  vertex_id v_id,vv_id; 
  int i;
  REAL d[MAXCOORD]; /* difference vector between vertices */
  REAL rr;
  REAL power;
  REAL tj;  
  facetedge_id fe;
  REAL area,s11,s12,s22,side1[MAXCOORD],side2[MAXCOORD];
  facet_id f_id;

  power = globals(exponent_param)->value.real;

  /* will need areas of all vertex stars */
  FOR_ALL_VERTICES(v_id) set_vertex_star(v_id,0.0);
  FOR_ALL_FACETS(f_id)
    { fe = get_facet_fe(f_id);
      get_edge_side(get_fe_edge(fe),side1);
      get_edge_side(get_fe_edge(get_next_edge(fe)),side2);
      s11 = SDIM_dot(side1,side1);
      s22 = SDIM_dot(side2,side2);
      s12 = SDIM_dot(side1,side2);
      area = sqrt(s11*s22 - s12*s12)/2;
      for ( i = 0 ; i < FACET_VERTS ; i++ )
        { vv_id = get_fe_tailv(fe);
          add_vertex_star(vv_id,area/3);
          fe = get_next_edge(fe);
        }
    }

  /* basic sums */
  if ( f_sums ) myfree((char*)f_sums);
  f_sums = (REAL *)mycalloc(web.skel[VERTEX].max_ord+1,sizeof(REAL));

  FOR_ALL_VERTICES(v_id)
  { int ordv = ordinal(v_id);
     REAL val;
     REAL ti = get_vertex_star(v_id);
     x = get_coord(v_id);
     FOR_ALL_VERTICES(vv_id)
     { REAL *y = get_coord(vv_id);
        int ordvv = ordinal(vv_id);
        if ( ordvv <= ordv ) continue;
        tj = get_vertex_star(vv_id);
        for ( i = 0 ; i < SDIM ; i++ ) d[i] = x[i] - y[i];
        rr = SDIM_dot(d,d);
        val = pow(rr,power/2);  /* inverse power potential */
        f_sums[ordv] += tj/val;
        f_sums[ordvv] += ti/val;
     }
  }
  }

}

/**************************************************************
*
*  function: facet_knot_energy()
*  
*  purpose: calculates energy of one vertex due to potential
*              with all others. Charge at vertex is area of star.
*
*  input: info about vertex is in global qinfo structure.
*
*/

REAL facet_knot_energy(v_info)
struct qinfo *v_info;
{ REAL ti,energy;

  ti = get_vertex_star(v_info->id);
  energy = ti*f_sums[ordinal(v_info->id)];
  return energy; /* because pair counts in both orders */
}


/**************************************************************
*
*  function: facet_knot_energy_gradient()
*  
*  purpose: calculates energy of one vertex due to potential
*              with all others.
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL facet_knot_energy_gradient(v_info)
struct qinfo *v_info;
{ REAL *x = get_coord(v_info->id);
  vertex_id v_id;
  int i;
  REAL d[MAXCOORD]; /* difference vector between vertices */
  REAL rr,p;
  REAL power,halfpower;
  REAL ti,tj;  /* length weights of vertices, ti for home vertex */
  facetedge_id fe,start_fe,next_fe;
  REAL sumi,sumj,sumjj,area;
  REAL s11,s12,s22,side1[MAXCOORD],side2[MAXCOORD];
  REAL da[MAXCOORD],sum[MAXCOORD];

  ti = get_vertex_star(v_info->id);
  power = globals(exponent_param)->value.real;
  halfpower = power/2;
  for ( i = 0 ; i < SDIM ; i++ ) 
    v_info->grad[0][i] = 0.0; /* intialize gradient */

  /* distant change part */
  for ( i = 0 ; i < SDIM ; i++ ) sum[i] = 0.0;
  FOR_ALL_VERTICES(v_id)
  { REAL *y = get_coord(v_id);

    tj = get_vertex_star(v_id);
    for ( i = 0 ; i < SDIM ; i++ ) d[i] = x[i] - y[i];
    rr = SDIM_dot(d,d);
    if ( rr > 1e-12 ) /* don't do self */
     { p = power*tj/rr/pow(rr,halfpower);  /* inverse power potential */
       for ( i = 0 ; i < SDIM ; i++ ) 
          sum[i] -= p*d[i];
     }
  }
  for ( i = 0 ; i < SDIM ; i++ )
      v_info->grad[0][i] += 2*ti*sum[i];
 
  /* area change part */
  /* go around all neighbor vertices */
  start_fe = get_vertex_fe(v_info->id);
  sumi = f_sums[ordinal(v_info->id)];
  if ( valid_id(start_fe) )
    for ( fe = start_fe ; ; )
    { 
      next_fe = inverse_id(get_next_facet(get_prev_edge(fe)));
      get_edge_side(get_fe_edge(fe),side1);
      get_edge_side(get_fe_edge(get_next_edge(fe)),side2);
      s11 = SDIM_dot(side1,side1);
      s12 = SDIM_dot(side1,side2);
      s22 = SDIM_dot(side2,side2);
      area = sqrt(s11*s22 - s12*s12);
      for ( i = 0 ; i < SDIM ; i++ )
         da[i] = (s12*side2[i] - s22*side1[i])/area/6;
      sumj = f_sums[ordinal(get_fe_headv(fe))];
      sumjj = f_sums[ordinal(get_fe_headv(next_fe))];
      for ( i = 0 ; i < SDIM ; i++ )
         v_info->grad[0][i] += 2*(sumj*da[i] + sumjj*da[i] + sumi*da[i]);
      if ( next_fe == start_fe ) break;
      fe = next_fe;
    }
  return 2*ti*sumi;
}

/**************************************************************************/
/**************************************************************************/

/****************************************************************************
*
* function: facet_knot_energy_fix()
*
* purpose: provide adjacent vertex correction to facet_knot_energy
*
*/

static REAL *fix_sums;  /* for sums to other vertices connected by edges */

/***************************************************************
*
*  function: facet_knot_energy_fix_init()
*
*/

void facet_knot_energy_fix_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{
  edge_id e_id;
  REAL *x;
  vertex_id v_id,vv_id; 
  int i;
  REAL d[MAXCOORD]; /* difference vector between vertices */
  REAL rr;
  REAL power;
  facetedge_id fe;
  REAL area,s11,s12,s22,side1[MAXCOORD],side2[MAXCOORD];
  facet_id f_id;

  exponent_param = lookup_global(SURF_KNOTPOW_NAME);
  if ( exponent_param < 0 ) /* missing, so add */
   { exponent_param = add_global(SURF_KNOTPOW_NAME);
     globals(exponent_param)->value.real = 4.0;  /* default */
     globals(exponent_param)->flags |=  ORDINARY_PARAM | RECALC_PARAMETER | ALWAYS_RECALC;
   }
    
  /* will need areas of all vertex stars */
  FOR_ALL_VERTICES(v_id) set_vertex_star(v_id,0.0);
  FOR_ALL_FACETS(f_id)
    { fe = get_facet_fe(f_id);
      get_edge_side(get_fe_edge(fe),side1);
      get_edge_side(get_fe_edge(get_next_edge(fe)),side2);
      s11 = SDIM_dot(side1,side1);
      s22 = SDIM_dot(side2,side2);
      s12 = SDIM_dot(side1,side2);
      area = sqrt(s11*s22 - s12*s12)/2;
      for ( i = 0 ; i < 3 ; i++ )
        { vv_id = get_fe_tailv(fe);
          add_vertex_star(vv_id,area/3);
          fe = get_next_edge(fe);
        }
    }

  /* get basic sums */
  power = globals(exponent_param)->value.real/2; /* since rr already square */
  if ( fix_sums ) myfree((char*)fix_sums);
  fix_sums = (REAL *)mycalloc(web.skel[VERTEX].max_ord+1,sizeof(REAL));
  FOR_ALL_EDGES(e_id)
  { 
     REAL val;
     int ordv;
     REAL *y;
     REAL ti,tj;
     int ordvv;
     v_id = get_edge_tailv(e_id);
     vv_id = get_edge_headv(e_id);
     ordv = ordinal(v_id);
     ordvv = ordinal(vv_id);
     x = get_coord(v_id);
     y = get_coord(vv_id);
     ti = get_vertex_star(v_id);
     tj = get_vertex_star(vv_id);
     for ( i = 0 ; i < SDIM ; i++ ) d[i] = x[i] - y[i];
     rr = SDIM_dot(d,d);
     val = pow(rr,power);  /* inverse power potential */
     fix_sums[ordv] += tj/val;
     fix_sums[ordvv] += ti/val;
  }
}

/**************************************************************
*
*  function: facet_knot_energy_fix()
*  
*  purpose: calculates energy of one vertex due to potential
*              with its neighbors. Charge at vertex is area of star.
*
*  input: info about vertex is in global qinfo structure.
*
*/

REAL facet_knot_energy_fix(v_info)
struct qinfo *v_info;
{ REAL ti,energy;

  ti = get_vertex_star(v_info->id);
  energy = ti*fix_sums[ordinal(v_info->id)];
  return energy;
}


/**************************************************************
*
*  function: facet_knot_energy_fix_gradient()
*  
*  purpose: calculates energy of one vertex due to potential
*              with all others.
*
*  input: info about vertex is in global qinfo structure.
*
*/

REAL facet_knot_energy_fix_gradient(v_info)
struct qinfo *v_info;
{ REAL *x = get_coord(v_info->id);
  vertex_id v_id;
  int i;
  REAL d[MAXCOORD]; /* difference vector between vertices */
  REAL rr,p;
  REAL power,halfpower;
  REAL ti,tj;  /* length weights of vertices, ti for home vertex */
  facetedge_id fe,start_fe,next_fe;
  REAL sumi,sumj,sumjj,area;
  REAL s11,s12,s22,side1[MAXCOORD],side2[MAXCOORD];
  REAL da[MAXCOORD],sum[MAXCOORD];

  ti = get_vertex_star(v_info->id);
  power = globals(exponent_param)->value.real;
  halfpower = power/2;
  for ( i = 0 ; i < SDIM ; i++ ) 
    v_info->grad[0][i] = 0.0; /* intialize gradient */

  /* distant change part */
  for ( i = 0 ; i < SDIM ; i++ ) sum[i] = 0.0;
  /* go around all neighbor vertices */
  start_fe = get_vertex_fe(v_info->id);
  if (valid_id(start_fe))
    for ( fe = start_fe ; ; )
    { 
      REAL *y;
      v_id = get_fe_headv(fe);
      y = get_coord(v_id);

      tj = get_vertex_star(v_id);
      for ( i = 0 ; i < SDIM ; i++ ) d[i] = x[i] - y[i];
      rr = SDIM_dot(d,d);
      if ( rr > 1e-12 ) /* don't do self */
       { p = power*tj/rr/pow(rr,halfpower);  /* inverse power potential */
         for ( i = 0 ; i < SDIM ; i++ ) 
           sum[i] -= p*d[i];
       }
      next_fe = inverse_id(get_next_facet(get_prev_edge(fe)));
      if ( next_fe == start_fe ) break;
      fe = next_fe;
    }
  for ( i = 0 ; i < SDIM ; i++ )
      v_info->grad[0][i] += ti*sum[i];
 
  /* area change part */
  /* go around all neighbor vertices */
  start_fe = get_vertex_fe(v_info->id);
  sumi = fix_sums[ordinal(v_info->id)];
  if (valid_id(start_fe))
    for ( fe = start_fe ; ; )
    { 
      next_fe = inverse_id(get_next_facet(get_prev_edge(fe)));
      get_edge_side(get_fe_edge(fe),side1);
      get_edge_side(get_fe_edge(get_next_edge(fe)),side2);
      s11 = SDIM_dot(side1,side1);
      s12 = SDIM_dot(side1,side2);
      s22 = SDIM_dot(side2,side2);
      area = sqrt(s11*s22 - s12*s12);
      for ( i = 0 ; i < SDIM ; i++ )
         da[i] = (s12*side2[i] - s22*side1[i])/area/6;
      sumj = fix_sums[ordinal(get_fe_headv(fe))];
      sumjj = fix_sums[ordinal(get_fe_headv(next_fe))];
      for ( i = 0 ; i < SDIM ; i++ )
         v_info->grad[0][i] += sumj*da[i] + sumjj*da[i] + sumi*da[i];
      if ( next_fe == start_fe ) break;
      fe = next_fe;
    }
  return ti*sumi;
}


/**************************************************************************/
/**************************************************************************/

/***************************************************************
*
*  function: xfacet_knot_energy_init()
*
*  purpose: initialization for knot_energy() and 
*              knot_energy_gradient().
*  Does not do fsums in attempt to be more efficient with MPI
*/


void xfacet_knot_energy_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{ vertex_id v_id,vv_id; 
  int i;
  facetedge_id fe;
  REAL area,s11,s12,s22,side1[MAXCOORD],side2[MAXCOORD];
  facet_id f_id;

  exponent_param = lookup_global(SURF_KNOTPOW_NAME);
  if ( exponent_param < 0 ) /* missing, so add */
        { exponent_param = add_global(SURF_KNOTPOW_NAME);
          globals(exponent_param)->value.real = 4.0;  /* default */
          globals(exponent_param)->flags |=  ORDINARY_PARAM | RECALC_PARAMETER | ALWAYS_RECALC;
        }


  /* will need areas of all vertex stars */
  FOR_ALL_VERTICES(v_id) set_vertex_star(v_id,0.0);
  FOR_ALL_FACETS(f_id)
    { fe = get_facet_fe(f_id);
      get_edge_side(get_fe_edge(fe),side1);
      get_edge_side(get_fe_edge(get_next_edge(fe)),side2);
      s11 = SDIM_dot(side1,side1);
      s22 = SDIM_dot(side2,side2);
      s12 = SDIM_dot(side1,side2);
      area = sqrt(s11*s22 - s12*s12)/2;
      for ( i = 0 ; i < 3 ; i++ )
        { vv_id = get_fe_tailv(fe);
          add_vertex_star(vv_id,area/3);
          fe = get_next_edge(fe);
        }
    }

}

/**************************************************************
*
*  function: facet_knot_energy()
*  
*  purpose: calculates energy of one vertex due to potential
*              with all others. Charge at vertex is area of star.
*
*  input: info about vertex is in global qinfo structure.
*
*/

REAL xfacet_knot_energy(v_info)
struct qinfo *v_info;
{ REAL ti,energy;

  ti = get_vertex_star(v_info->id);
  energy = ti*f_sums[ordinal(v_info->id)];
  return energy;
}


/**************************************************************
*
*  function: facet_knot_energy_gradient()
*  
*  purpose: calculates energy of one vertex due to potential
*              with all others.
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL xfacet_knot_energy_gradient(v_info)
struct qinfo *v_info;
{ REAL *x = get_coord(v_info->id);
  vertex_id v_id;
  int i;
  REAL d[MAXCOORD]; /* difference vector between vertices */
  REAL rr,p;
  REAL power,halfpower;
  REAL ti,tj;  /* length weights of vertices, ti for home vertex */
  facetedge_id fe,start_fe,next_fe;
  REAL sumi,sumj,sumjj,area;
  REAL s11,s12,s22,side1[MAXCOORD],side2[MAXCOORD];
  REAL da[MAXCOORD],sum[MAXCOORD];

  ti = get_vertex_star(v_info->id);
  power = globals(exponent_param)->value.real;
  halfpower = power/2;
  for ( i = 0 ; i < SDIM ; i++ ) 
    v_info->grad[0][i] = 0.0; /* intialize gradient */

  /* distant change part */
  for ( i = 0 ; i < SDIM ; i++ ) sum[i] = 0.0;
  FOR_ALL_VERTICES(v_id)
  { REAL *y = get_coord(v_id);

    tj = get_vertex_star(v_id);
    for ( i = 0 ; i < SDIM ; i++ ) d[i] = x[i] - y[i];
    rr = SDIM_dot(d,d);
    if ( rr > 1e-12 ) /* don't do self */
     { p = power*tj/rr/pow(rr,halfpower);  /* inverse power potential */
       for ( i = 0 ; i < SDIM ; i++ ) 
          sum[i] -= p*d[i];
     }
  }
  for ( i = 0 ; i < SDIM ; i++ )
      v_info->grad[0][i] += ti*sum[i];
 
  /* area change part */
  /* go around all neighbor vertices */
  start_fe = get_vertex_fe(v_info->id);
  sumi = f_sums[ordinal(v_info->id)];
  if ( valid_id(start_fe) )
    for ( fe = start_fe ; ; )
    { 
      next_fe = inverse_id(get_next_facet(get_prev_edge(fe)));
      get_edge_side(get_fe_edge(fe),side1);
      get_edge_side(get_fe_edge(get_next_edge(fe)),side2);
      s11 = SDIM_dot(side1,side1);
      s12 = SDIM_dot(side1,side2);
      s22 = SDIM_dot(side2,side2);
      area = sqrt(s11*s22 - s12*s12);
      for ( i = 0 ; i < SDIM ; i++ )
         da[i] = (s12*side2[i] - s22*side1[i])/area/6;
      sumj = f_sums[ordinal(get_fe_headv(fe))];
      sumjj = f_sums[ordinal(get_fe_headv(next_fe))];
      for ( i = 0 ; i < SDIM ; i++ )
         v_info->grad[0][i] += sumj*da[i] + sumjj*da[i] + sumi*da[i];
      if ( next_fe == start_fe ) break;
      fe = next_fe;
    }
  return ti*sumi;
}

/**************************************************************************/
/**************************************************************************


buck knot energy 

Suggested by Gregory Buck

Between pairs of edges, energy is 1/(d1+d2+d3+d4-2(L1+l2))
where d's are cross distances between vertices and L's are
edge lengths.  Idea is to provide infinite barrier to edge
crossing.

******************************************************************/

/**************************************************************
*
*  function: buck_knot_energy()
*  
*  purpose: calculates energy of one vertex due to potential
*              with all others. Charge at vertex is area of star.
*
*  input: info about vertex is in global qinfo structure.
*
*/

REAL buck_knot_energy(v_info)
struct qinfo *v_info;
{ edge_id e1 = v_info->id,e2;
  REAL *x1,*x2,*yy1,*y2; /* end coordinates */
  REAL d1,d2,d3,d4,L1,L2;
  REAL power,denom;
  REAL energy = 0.0;
  REAL dx[MAXCOORD];
  int j;

  power = globals(exponent_param)->value.real;
  x1 = get_coord(get_edge_tailv(e1));
  x2 = get_coord(get_edge_headv(e1));
  for ( j = 0 ; j < SDIM ; j++ ) dx[j] = x2[j] - x1[j];
  L1 = sqrt(SDIM_dot(dx,dx));
  FOR_ALL_EDGES(e2)
    { if ( e2 <= e1 ) continue; /* each pair once */
      yy1 = get_coord(get_edge_tailv(e2));
      y2 = get_coord(get_edge_headv(e2));
      for ( j = 0 ; j < SDIM ; j++ ) dx[j] = y2[j] - yy1[j];
      L2 = sqrt(SDIM_dot(dx,dx));
      for ( j = 0 ; j < SDIM ; j++ ) dx[j] = yy1[j] - x1[j];
      d1 = sqrt(SDIM_dot(dx,dx));
      if ( d1 <= 0.0 ) continue; /* prevent division by 0 */
      for ( j = 0 ; j < SDIM ; j++ ) dx[j] = yy1[j] - x2[j];
      d2 = sqrt(SDIM_dot(dx,dx));
      if ( d2 <= 0.0 ) continue; /* for adjacent edges     */
      for ( j = 0 ; j < SDIM ; j++ ) dx[j] = x2[j] - y2[j];
      d3 = sqrt(SDIM_dot(dx,dx));
      if ( d3 <= 0.0 ) continue; /* prevent division by 0 */
      for ( j = 0 ; j < SDIM ; j++ ) dx[j] = y2[j] - x1[j];
      d4 = sqrt(SDIM_dot(dx,dx));
      if ( d4 <= 0.0 ) continue; /* prevent division by 0 */
      denom = d1+d2+d3+d4-2*(L1+L2);
      if ( denom <= 0.0 )
         kb_error(1543,"Buck denominator nonpositive.\n",RECOVERABLE);

      energy += pow(denom ,-power);
    }
  return 2*energy; /* since each pair once */
}

/**************************************************************
*
*  function: buck_knot_energy_gradient()
*  
*  purpose: calculates energy of one vertex due to potential
*              with all others.
*
*  input: info about vertex is in global qinfo structure.
*
*/

REAL buck_knot_energy_gradient(v_info)
struct qinfo *v_info;
{ edge_id e1 = v_info->id,e2;
  REAL *x1,*x2,*yy1,*y2; /* end coordinates */
  REAL d1,d2,d3,d4,L1,L2;
  REAL power;
  REAL energy = 0.0;
  REAL denom,coeff;
  int i,j;
  REAL dL1[MAXCOORD], dL2[MAXCOORD], dd1[MAXCOORD], dd2[MAXCOORD], 
         dd3[MAXCOORD], dd4[MAXCOORD];

  for ( i = 0 ; i < 2 ; i++ )
     for ( j = 0 ; j < SDIM ; j++ ) v_info->grad[i][j] = 0.0;
  power = globals(exponent_param)->value.real;
  x1 = get_coord(get_edge_tailv(e1));
  x2 = get_coord(get_edge_headv(e1));
  for ( j = 0 ; j < SDIM ; j++ ) dL1[j] = x2[j] - x1[j];
  L1 = sqrt(SDIM_dot(dL1,dL1));
  FOR_ALL_EDGES(e2)
    { 
      yy1 = get_coord(get_edge_tailv(e2));
      y2 = get_coord(get_edge_headv(e2));
      for ( j = 0 ; j < SDIM ; j++ ) dL2[j] = y2[j] - yy1[j];
      L2 = sqrt(SDIM_dot(dL2,dL2));
      for ( j = 0 ; j < SDIM ; j++ ) dd1[j] = x1[j] - yy1[j];
      d1 = sqrt(SDIM_dot(dd1,dd1));
      if ( d1 <= 0.0 ) continue; /* prevent division by 0 */
      for ( j = 0 ; j < SDIM ; j++ ) dd2[j] = yy1[j] - x2[j];
      d2 = sqrt(SDIM_dot(dd2,dd2));
      if ( d2 <= 0.0 ) continue; /* for identical vertices */
      for ( j = 0 ; j < SDIM ; j++ ) dd3[j] = x2[j] - y2[j];
      d3 = sqrt(SDIM_dot(dd3,dd3));
      if ( d3 <= 0.0 ) continue; /* since contribution  */
      for ( j = 0 ; j < SDIM ; j++ ) dd4[j] = y2[j] - x1[j];
      d4 = sqrt(SDIM_dot(dd4,dd4));
      if ( d4 <= 0.0 ) continue; /* fixed at 0.  Burma Shave */
      denom = d1+d2+d3+d4-2*(L1+L2);
      if ( denom <= 0.0 )
         kb_error(1544,"Buck denominator nonpositive.\n",RECOVERABLE);

      energy += pow(denom ,-power);
      coeff = -2*power*pow(denom,-power-1);
      for ( j = 0 ; j < SDIM ; j++ )
         { v_info->grad[0][j] += coeff*(dd1[j]/d1 - dd4[j]/d4 + 2*dL1[j]/L1);
            v_info->grad[1][j] += coeff*(dd3[j]/d3 - dd2[j]/d2 - 2*dL1[j]/L1);
         }
    }
  return energy;
}

/******************* bi_surface named method **********************

  Double integral over surface, i.e. all pairs of vertices  
  weighted with adjacent facet areas.  Adapted from facet_knot_energy.

  Uses arbitrary formula for energy, function of vector between
  vertices, instead of just power rule.  Formula is "scalar_integrand"
  in datafile definition.

  Also, vertex pairs it is evaluated over can be controlled.  Only
  those with different values of the vertex attribute "bi_surface_attr"
  will be included, if bi_surface_attr is defined; otherwise all pairs.

***********************************************************************/

/***************************************************************
*
*  function: bi_surface_init()
*
*  purpose: initialization for bi_surface()
*/

int bi_surface_attr; /* number of attribute */
char *bi_surface_attr_name = "bi_surface_attr";
REAL *bi_sums;  /* values for vertices */

void bi_surface_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{
  REAL *x;
  vertex_id v_id,vv_id; 
  int i;
  REAL d[MAXCOORD]; /* difference vector between vertices */
  REAL rr;
  REAL tj;  
  facetedge_id fe;
  REAL area,s11,s12,s22,side1[MAXCOORD],side2[MAXCOORD];
  facet_id f_id;

  bi_surface_attr = find_attribute(VERTEX,bi_surface_attr_name);
  if ( bi_surface_attr >= 0 )
  { /* check proper type */
	  if ( EXTRAS(VERTEX)[bi_surface_attr].type != INTEGER_TYPE )
      kb_error(4927,
       "The type of vertex attribute bi_surface_attr must be integer.\n",
         RECOVERABLE);
  }

  /* will need areas of all vertex stars */
  FOR_ALL_VERTICES(v_id) set_vertex_star(v_id,0.0);
  FOR_ALL_FACETS(f_id)
    { fe = get_facet_fe(f_id);
      get_edge_side(get_fe_edge(fe),side1);
      get_edge_side(get_fe_edge(get_next_edge(fe)),side2);
      s11 = SDIM_dot(side1,side1);
      s22 = SDIM_dot(side2,side2);
      s12 = SDIM_dot(side1,side2);
      area = sqrt(s11*s22 - s12*s12)/2;
      for ( i = 0 ; i < FACET_VERTS ; i++ )
        { vv_id = get_fe_tailv(fe);
          add_vertex_star(vv_id,area/3);
          fe = get_next_edge(fe);
        }
    }

  /* basic sums */
  if ( bi_sums ) myfree((char*)bi_sums);
  bi_sums = (REAL *)mycalloc(web.skel[VERTEX].max_ord+1,sizeof(REAL));

  FOR_ALL_VERTICES(v_id)
  { int ordv = ordinal(v_id);
    int vattr;
    REAL val;
    REAL ti = get_vertex_star(v_id);
    x = get_coord(v_id);
    if ( bi_surface_attr >= 0) 
      vattr = *(int*)get_extra(v_id,bi_surface_attr);
    FOR_ALL_VERTICES(vv_id)
    { 
      int vvattr;
      REAL *y = get_coord(vv_id);
      int ordvv = ordinal(vv_id);
      if ( ordvv <= ordv ) continue;
      if ( bi_surface_attr >= 0) 
      { vvattr = *(int*)get_extra(vv_id,bi_surface_attr);
        if ( vvattr == vattr ) continue; /* only do for different values */
      }
      tj = get_vertex_star(vv_id);
      for ( i = 0 ; i < SDIM ; i++ ) d[i] = x[i] - y[i];
      rr = SDIM_dot(d,d);
      val = eval(mi->expr[0],d,v_id,NULL); 
      bi_sums[ordv] += tj*val;
      bi_sums[ordvv] += ti*val;
    }
  }

}

/**************************************************************
*
*  function: bi_surface_energy()
*  
*  purpose: calculates energy of one vertex due to potential
*              with all others. Charge at vertex is area of star.
*
*  input: info about vertex is in global qinfo structure.
*
*/

REAL bi_surface_energy(v_info)
struct qinfo *v_info;
{ REAL ti,energy;

  ti = get_vertex_star(v_info->id);
  energy = ti*bi_sums[ordinal(v_info->id)];
  return energy; /* because pair counts in both orders */
}


/**************************************************************
*
*  function: bi_surface_gradient()
*  
*  purpose: calculates energy of one vertex due to potential
*              with all others.
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL bi_surface_gradient(v_info)
struct qinfo *v_info;
{ REAL *x = get_coord(v_info->id);
  vertex_id v_id;
  int i;
  REAL d[MAXCOORD]; /* difference vector between vertices */
  REAL power,halfpower;
  REAL ti,tj;  /* length weights of vertices, ti for home vertex */
  facetedge_id fe,start_fe,next_fe;
  REAL sumi,sumj,sumjj,area;
  REAL s11,s12,s22,side1[MAXCOORD],side2[MAXCOORD];
  REAL da[MAXCOORD],sum[MAXCOORD];
  int vattr;
  struct method_instance *mi = METH_INSTANCE(v_info->method);

  if ( bi_surface_attr >= 0)
    vattr = *(int*)get_extra(v_info->id,bi_surface_attr);

  ti = get_vertex_star(v_info->id);
  power = globals(exponent_param)->value.real;
  halfpower = power/2;
  for ( i = 0 ; i < SDIM ; i++ ) 
    v_info->grad[0][i] = 0.0; /* intialize gradient */

  /* distant change part */
  for ( i = 0 ; i < SDIM ; i++ ) sum[i] = 0.0;
  FOR_ALL_VERTICES(v_id)
  { REAL *y = get_coord(v_id);
    REAL val;
	REAL partials[MAXCOORD];

    if ( bi_surface_attr >= 0)
    { int vvattr = *(int*)get_extra(v_id,bi_surface_attr);
      if ( vvattr == vattr )
        continue;
    }
    else
      if ( equal_id(v_id,v_info->id) )
        continue; /* don't do self */

    tj = get_vertex_star(v_id);
    for ( i = 0 ; i < SDIM ; i++ ) d[i] = x[i] - y[i];
    eval_all(mi->expr[0],d,3,&val,partials,v_info->id);
    for ( i = 0 ; i < SDIM ; i++ ) 
      sum[i] += tj*partials[i];
  }
  for ( i = 0 ; i < SDIM ; i++ )
      v_info->grad[0][i] += 2*ti*sum[i];
 
  /* area change part */
  /* go around all neighbor vertices */
  start_fe = get_vertex_fe(v_info->id);
  sumi = bi_sums[ordinal(v_info->id)];
  if ( valid_id(start_fe) )
    for ( fe = start_fe ; ; )
    { 
      next_fe = inverse_id(get_next_facet(get_prev_edge(fe)));
      get_edge_side(get_fe_edge(fe),side1);
      get_edge_side(get_fe_edge(get_next_edge(fe)),side2);
      s11 = SDIM_dot(side1,side1);
      s12 = SDIM_dot(side1,side2);
      s22 = SDIM_dot(side2,side2);
      area = sqrt(s11*s22 - s12*s12);
      for ( i = 0 ; i < SDIM ; i++ )
         da[i] = (s12*side2[i] - s22*side1[i])/area/6;
      sumj = bi_sums[ordinal(get_fe_headv(fe))];
      sumjj = bi_sums[ordinal(get_fe_headv(next_fe))];
      for ( i = 0 ; i < SDIM ; i++ )
         v_info->grad[0][i] += 2*(sumj*da[i] + sumjj*da[i] + sumi*da[i]);
      if ( next_fe == start_fe ) break;
      fe = next_fe;
    }
  return 2*ti*sumi;
}

/**************************************************************************/
/**************************************************************************/

