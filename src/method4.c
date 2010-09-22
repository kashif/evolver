/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*********************************************************************
*
*     file:        method4.c
*
*     contents:  miscellaneous named methods
*/

#include "include.h"



/*****************************************************************
*
* function: johndust_energy()
*
* purpose: Energy request of John Sullivan, for point pairs on 
*     sphere.  E = (pi - asin(d/2))/d, where d is chord distance.
*
*/

REAL johndust_energy(v_info)
struct qinfo *v_info;
{ vertex_id v_id;
  int i;
  REAL energy = 0.0;
  REAL d,r[MAXCOORD];
  REAL *x = get_coord(v_info->id);

  FOR_ALL_VERTICES(v_id)
  { REAL *y = get_coord(v_id);
    if ( v_id <= v_info->id ) continue; /* each pair once */
    for ( i = 0 ; i < SDIM ; i++ ) r[i] = x[i] - y[i];
    d = sqrt(SDIM_dot(r,r));
    if ( d >= 2.0 ) energy += M_PI/4;  /* antipodes */
    else  energy += (M_PI - asin(d/2))/d;    
  }
  return energy;
}

/**************************************************************
*
*  function: johndust_gradient()
*  
*  purpose: calculates energy of one vertex due to others
*
*  input: info about vertex is in global qinfo structure.
*
*/

REAL johndust_gradient(v_info)
struct qinfo *v_info;
{ REAL *x = get_coord(v_info->id);
  REAL energy = 0.0;  /* for this vertex */
  vertex_id v_id;
  int i;
  REAL r[MAXCOORD]; /* difference vector between vertices */
  REAL d,p,dp;

  for ( i = 0 ; i < SDIM ; i++ ) 
      v_info->grad[0][i] = 0.0; /* intialize gradient */
  FOR_ALL_VERTICES(v_id)
  { REAL *y = get_coord(v_id);
    if ( equal_id(v_info->id,v_id) ) continue;
    for ( i = 0 ; i < SDIM ; i++ ) r[i] = x[i] - y[i];
    d = sqrt(SDIM_dot(r,r));
    if ( d >= 2.0 ) continue; /* antipodes have no force */
    p = (M_PI - asin(d/2))/d;
    energy += p;
    dp = -p/d - 0.5/d/sqrt(1 - d*d/4);
    for ( i = 0 ; i < SDIM ; i++ ) 
      v_info->grad[0][i] += dp/d*r[i];
  }
  return energy;
}


/******************************************************************************
  
         String Gravity quantity stuff

******************************************************************************/


/***************************************************************
*
*  function: string_gravity_init()
*
*  purpose: initialization for gravity method
*/

void string_gravity_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{
  /* method modulus is gravitation constant */
  if ( gravity_quantity_num >= 0 )
     GEN_QUANT(gravity_quantity_num)->modulus =
         web.gravflag ? web.grav_const : 0.0;
}

/**************************************************************
*
*  function: string_gravity_all()
*
*  purpose: calculates value, gradient, and hessian of one 
*        facet due to gravitational potential.
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL string_gravity_all_q ARGS((struct qinfo *,int));
REAL string_gravity_all_lagrange ARGS((struct qinfo *,int));
REAL get_edge_gdensity ARGS((edge_id));
REAL string_gravity_all ARGS((struct qinfo *,int));

REAL get_edge_gdensity(e_id)
edge_id e_id;
{ REAL gdensity;
  facetedge_id fe,ffe;
  facet_id f_id;
  body_id b_id;

  gdensity = 0.0;
  fe = get_edge_fe(e_id);
  f_id = get_fe_facet(fe);
  b_id = get_facet_body(f_id);
  if ( valid_id(b_id) )
          gdensity += get_body_density(b_id);
  b_id = get_facet_body(facet_inverse(f_id));
  if ( valid_id(b_id) )
          gdensity -= get_body_density(b_id);
  ffe = get_next_facet(fe);
  if ( !equal_id(fe,ffe) )
  { f_id = get_fe_facet(ffe);
    b_id = get_facet_body(f_id);
    if ( valid_id(b_id) )
          gdensity += get_body_density(b_id);
    b_id = get_facet_body(facet_inverse(f_id));
    if ( valid_id(b_id) )
          gdensity -= get_body_density(b_id);
  }
  return gdensity;
}

REAL string_gravity_all(e_info,mode)
struct qinfo *e_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */
{
  int i,j;
  REAL jac; /* jacobian */
  REAL sum = 0.0;  /* quadratic z sum of facet */
  REAL djdx[MAXCOORD],dsdz[MAXCOORD];
  REAL c = 1/6.;  /* coefficient */
  REAL gdensity;

  if ( web.modeltype == QUADRATIC ) return string_gravity_all_q(e_info,mode);
  if ( web.modeltype == LAGRANGE ) 
     return string_gravity_all_lagrange(e_info,mode);

  gdensity = get_edge_gdensity(e_info->id);
  if ( gdensity == 0.0 )
  { 
     return 0.0;
  }
  c *= gdensity;

  jac = -e_info->sides[0][0][0];
  for ( i = 0 ; i < e_info->vcount ; i++ )
    for ( j = 0 ; j <= i ; j++ )
      sum += e_info->x[i][1]*e_info->x[j][1];
  if ( mode == METHOD_VALUE ) return jac*sum*c;

  djdx[0] = 1.0;
  djdx[1] = -1.0;
  dsdz[0] = 2*e_info->x[0][1]+e_info->x[1][1];
  dsdz[1] = e_info->x[0][1]+2*e_info->x[1][1];

  for ( i = 0 ; i < ctrl_num;  i++ )
  { e_info->grad[i][0] = djdx[i]*sum*c;
    e_info->grad[i][1] = dsdz[i]*jac*c;
  }

  if ( mode == METHOD_GRADIENT ) return jac*sum*c;

  /* second partials, self */
  for ( i = 0 ; i < ctrl_num;  i++ )
  { e_info->hess[i][i][0][1] = e_info->hess[i][i][1][0] = djdx[i]*dsdz[i]*c;
    e_info->hess[i][i][1][1] = 2*jac*c;
    e_info->hess[i][i][0][0] = 0.0;
  }

  /* second partials, mixed */
  for ( i = 0 ; i < ctrl_num ; i++ )
    for ( j = 0 ; j < ctrl_num ; j++ )
     { if ( j == i ) continue;
       e_info->hess[i][j][0][0] = 0.0;
       e_info->hess[i][j][0][1] = djdx[i]*dsdz[j]*c;
       e_info->hess[i][j][1][0] = djdx[j]*dsdz[i]*c;
       e_info->hess[i][j][1][1] = jac*c;
     }
  return jac*sum*c;
}

/**************************************************************
*
*  function: string_gravity_all_q()
*
*  purpose: calculates value, gradient, and hessian of one 
*        facet due to gravitational potential.  Quadratic model.
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL string_gravity_all_q(e_info,mode)
struct qinfo *e_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */
{
  int i,k,kk,m;
  REAL **g=NULL,****h=NULL;
  REAL value = 0.0;
  REAL gdensity,w,z;
  REAL tang,sum;

  if ( mode == METHOD_GRADIENT ) 
  { g = e_info->grad;
  }
  else if ( mode == METHOD_HESSIAN ) 
  { g = e_info->grad;
    h = e_info->hess;
  }

  gdensity = get_edge_gdensity(e_info->id);
  if ( gdensity == 0.0 ) return 0.0;

  for ( m = 0 ; m < gauss1D_num ; m++ )
   { z = e_info->gauss_pt[m][1];
     w = gdensity*gauss1Dwt[m];
     tang = 0.0;
     for ( k = 0 ; k < edge_ctrl ; k++ )
          tang += gauss1polyd[k][m]*e_info->x[k][0];
     value += w*tang*(-z*z/2);
     if ( mode == METHOD_VALUE ) continue;

      sum = (-z)*tang;
      for ( i = 0 ; i < edge_ctrl ; i++ )
      { g[i][0] += w*gauss1polyd[i][m]*(-z*z/2);
         g[i][1] += w*gauss1poly[i][m]*sum;
      }
      if ( mode == METHOD_GRADIENT ) continue;

      sum = -tang;
      for ( k = 0 ; k < edge_ctrl ; k++ )
      for ( kk = 0 ; kk < edge_ctrl ; kk++ )
      {
         h[k][kk][1][1] += w*sum*gauss1poly[k][m]*gauss1poly[kk][m];
         h[k][kk][0][1] += w*gauss1polyd[k][m]*(-z)*gauss1poly[kk][m];
         h[k][kk][1][0] += w*gauss1polyd[kk][m]*(-z)*gauss1poly[k][m];
      }
  }
  return value;
}



/**************************************************************
*
*  function: string_gravity_all_lagrange()
*
*  purpose: calculates value, gradient, and hessian of one 
*        facet due to gravitational potential.  Lagrange model.
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL string_gravity_all_lagrange(e_info,mode)
struct qinfo *e_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */
{
  REAL gdensity;
  REAL sum,y;
  int m,i,k,kk;
  REAL value = 0.0;
  REAL val;
  REAL tang;
  REAL sign = (get_eattr(e_info->id) & NEGBOUNDARY) ? -1.0 : 1.0;
  struct gauss_lag *gl = &gauss_lagrange[1][web.gauss1D_order];

  gdensity = get_edge_gdensity(e_info->id);
  if ( gdensity == 0.0 ) return 0.0;

  for ( m = 0 ; m < gl->gnumpts ; m++ )
  { REAL weight = sign*gdensity*gl->gausswt[m];
    tang = 0.0;
    for ( k = 0 ; k < gl->lagpts ; k++ )
       tang += gl->gpolypart[m][0][k]*e_info->x[k][0];
    y = e_info->gauss_pt[m][1];
    val = -y*y/2; 
    value += weight*val*tang;
    if ( mode == METHOD_VALUE ) continue;
            
    sum = -y*tang;
    for ( i = 0 ; i < gl->lagpts ; i++ )
    { e_info->grad[i][0] += weight*gl->gpolypart[m][0][i]*val;
      e_info->grad[i][1] += weight*gl->gpoly[m][i]*sum;
    }
    if ( mode == METHOD_GRADIENT ) continue;

    sum = -tang;
    for ( k = 0 ; k < gl->lagpts ; k++ )
      for ( kk = 0 ; kk < gl->lagpts ; kk++ )
        { e_info->hess[k][kk][1][1] += weight*
                    sum*gl->gpoly[m][k]*gl->gpoly[m][kk];
          e_info->hess[k][kk][0][1] += weight*
                      gl->gpolypart[m][0][k]*(-y)*gl->gpoly[m][kk];
          e_info->hess[k][kk][1][0] += weight*
                      gl->gpolypart[m][0][kk]*(-y)*gl->gpoly[m][k];
        }
   }

  return value;
}



/**************************************************************
*
*  function: string_gravity_energy()
*
*  purpose: calculates energy of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL string_gravity_energy(e_info)
struct qinfo *e_info;
{
 return string_gravity_all(e_info,METHOD_VALUE);
}



/**************************************************************
*
*  function: string_gravity_grads()
*
*  purpose: calculates gradient of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL string_gravity_grads(e_info)
struct qinfo *e_info;
{
 return string_gravity_all(e_info,METHOD_GRADIENT);
}


/**************************************************************
*
*  function: string_gravity_hessian()
*
*  purpose: calculates hessian of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL string_gravity_hessian(e_info)
struct qinfo *e_info;
{
 return string_gravity_all(e_info,METHOD_HESSIAN);
}


/*************************************************************************
    
    Curvature rotated to binormal direction.
    Localized Induction Equation.
    For Ron Perline.

***************************************************************************/

void curvature_binormal_init(mode,mi)
int mode;
struct method_instance *mi;
{
}

REAL curvature_binormal_energy(v_info)
struct qinfo *v_info;
{ return 0.0;
}

REAL curvature_binormal_force(v_info)
struct qinfo *v_info;
{ 
  vertex_id v_id = v_info->id;
  edge_id e1,e2;
  REAL s1[MAXCOORD],s2[MAXCOORD];
  REAL l1,l2;
  REAL denom;
  int i;
  int sign = 1;  /* orientation correction */

  e1 = get_vertex_edge(v_id);
  e2 = get_next_tail_edge(e1);
  if ( inverted(e1) ) { invert(e1); sign = -1; }
  if ( inverted(e2) ) invert(e2);
  get_edge_side(e1,s1);
  get_edge_side(e2,s2);
  cross_prod(s1,s2,v_info->grad[0]);
  l1 = sqrt(SDIM_dot(s1,s1));
  l2 = sqrt(SDIM_dot(s2,s2));
  denom = sign*l1*l2*(l1+l2)/2;
  for ( i = 0 ; i < SDIM ; i++ )
     v_info->grad[0][i] /= denom;
  return 0.0; /* energy */
}




/*************************************************************************
    
    Various curve curvature quantities.
    For Ron Perline.

***************************************************************************/
 
/**************************************************************************
    quantity ddd_gamma_sq
    Third deriv of curve position as function of arclength, squared.
****************************************************************************/

void ddd_gamma_sq_init(mode,mi)
int mode;
struct method_instance *mi;
{
}

REAL ddd_gamma_sq_energy(e_info)
struct qinfo *e_info;
{ REAL *side1,*side2,*side3;
  REAL s1,s2,s3;
  REAL dddgamma[MAXCOORD];
  int i;

  side1 = e_info->sides[0][1];
  side2 = e_info->sides[0][0];
  side3 = e_info->sides[0][2];
  s1 = sqrt(SDIM_dot(side1,side1));
  s2 = sqrt(SDIM_dot(side2,side2));
  s3 = sqrt(SDIM_dot(side3,side3));
  for ( i = 0 ; i < SDIM ; i++ )
     dddgamma[i] = side1[i]/s1/s2/(s2+s3)
                      - (side1[i]+side2[i])/s2/s3/(s1+s2)
                      + (side1[i]+side2[i]+side3[i])/s3/(s2+s3)/(s1+s2+s3);
  return 36*SDIM_dot(dddgamma,dddgamma)*s2;
}

REAL ddd_gamma_sq_gradient(e_info)
struct qinfo *e_info;
{ REAL *side1,*side2,*side3;
  REAL s1,s2,s3;
  REAL dddgamma[MAXCOORD];
  REAL Ddddgamma[4][MAXCOORD][MAXCOORD];
  REAL energy;
  int i,j,k;

  side1 = e_info->sides[0][1];
  side2 = e_info->sides[0][0];
  side3 = e_info->sides[0][2];
  s1 = sqrt(SDIM_dot(side1,side1));
  s2 = sqrt(SDIM_dot(side2,side2));
  s3 = sqrt(SDIM_dot(side3,side3));
  for ( i = 0 ; i < SDIM ; i++ )
     dddgamma[i]  = side1[i]/s1/s2/(s2+s3)
                      - (side1[i]+side2[i])/s2/s3/(s1+s2)
                      + (side1[i]+side2[i]+side3[i])/s3/(s2+s3)/(s1+s2+s3);

  energy = 36*SDIM_dot(dddgamma,dddgamma)*s2;

  for ( k = 0 ; k < 4 ; k++ )
    for ( i = 0 ; i < SDIM ; i++ )
     for ( j = 0 ; j < SDIM ; j++ )
      Ddddgamma[k][i][j] = 0.0;

  /* now the gradients on the four vertices */
  /* recalling numbering order 2 0 1 3 */
  for ( i = 0 ; i < SDIM ; i++ )
  { 
     Ddddgamma[2][i][i] -=  1.0/s1/s2/(s2+s3) - 1.0/s2/s3/(s1+s2)
                                    + 1.0/s3/(s2+s3)/(s1+s2+s3);
     Ddddgamma[0][i][i] +=  1.0/s1/s2/(s2+s3) - 1.0/s2/s3/(s1+s2)
                                    + 1.0/s3/(s2+s3)/(s1+s2+s3);
     Ddddgamma[0][i][i] -= -1/s2/s3/(s1+s2) + 1/s3/(s2+s3)/(s1+s2+s3);
     Ddddgamma[1][i][i] += -1/s2/s3/(s1+s2) + 1/s3/(s2+s3)/(s1+s2+s3);
     Ddddgamma[1][i][i] -= 1.0/s3/(s2+s3)/(s1+s2+s3);
     Ddddgamma[3][i][i] += 1.0/s3/(s2+s3)/(s1+s2+s3);
     for ( j = 0 ; j < SDIM ; j++ )
     { REAL d;
        d = side1[i]/s1/s2/(s2+s3)*(-side1[j]/s1/s1)
         - (side1[i]+side2[i])/s2/s3/(s1+s2)*(-side1[j]/s1/(s1+s2))
         + (side1[i]+side2[i]+side3[i])/s3/(s2+s3)/(s1+s2+s3)
                  *(-side1[j]/s1/(s1+s2+s3));
        Ddddgamma[2][i][j] -= d; 
        Ddddgamma[0][i][j] += d;
        d = side1[i]/s1/s2/(s2+s3)*(-side2[j]/s2/s2-side2[j]/s2/(s2+s3))
        - (side1[i]+side2[i])/s2/s3/(s1+s2)*(-side2[j]/s2/s2-side2[j]/s2/(s1+s2))
+ (side1[i]+side2[i]+side3[i])/s3/(s2+s3)/(s1+s2+s3)*(-side2[j]/s2/(s2+s3)
     - side2[j]/s2/(s1+s2+s3));
        Ddddgamma[0][i][j] -= d; 
        Ddddgamma[1][i][j] += d;
        d = side1[i]/s1/s2/(s2+s3)*(-side3[j]/s3/(s2+s3))
            - (side1[i]+side2[i])/s2/s3/(s1+s2)*(-side3[j]/s3/s3)
            + (side1[i]+side2[i]+side3[i])/s3/(s2+s3)/(s1+s2+s3)
            *(-side3[j]/s3/s3 - side3[j]/s3/(s2+s3) -side3[j]/s3/(s1+s2+s3));
        Ddddgamma[1][i][j] -= d; 
        Ddddgamma[3][i][j] += d;
     }
  }
  for ( k = 0 ; k < 4; k++ )
  { for ( j = 0 ; j < SDIM ; j++ )
     { REAL g;
        for ( i = 0, g = 0.0 ; i < SDIM ; i++ )
          g += 2*dddgamma[i]*Ddddgamma[k][i][j];
        e_info->grad[k][j] = 36*g*s2;
     }
  }
  for ( j = 0 ; j < SDIM ; j++ )
  { e_info->grad[0][j] -= energy*side2[j]/s2/s2;
     e_info->grad[1][j] += energy*side2[j]/s2/s2;
  }
  return energy;
}


/*********************************************************************

                     Gap energy methods

**********************************************************************/

/*****************************************************************
*
*  Function: gap_energy()
*
*  Purpose:  Calculate energy of kludge constraint force.
*                Constant factor included to make it best approx
*                of true area.
*/

REAL gap_energy(e_info)
struct qinfo *e_info;
{
  edge_id e_id = e_info->id;
  REAL sprenergy = 0.0;
  REAL *s;
  REAL ss;
  struct constraint *constr[MAXCONPER];
  int concount;
  conmap_t * conmap;
  int i,j; 

  if ( get_eattr(e_id) & FIXED ) return 0.0;
  if ( !(get_eattr(e_id) & CONSTRAINT) ) return 0.0;

  /* find which constraints have CONVEX attribute */
  conmap = get_e_constraint_map(e_id);
  for ( j = 1,i = 0 ; j <= (int)conmap[0] ; j++ )
  { constr[i] = get_constraint(conmap[j]);
    if ( constr[i]->attr & B_CONVEX ) 
            i++;    /* keep this one */
  }
  if ( i == 0 ) return 0.0;
  concount = i;  

  /* now the calculation */
  s = e_info->sides[0][0];
  ss = SDIM_dot(s,s);

  for ( i = 0 ; i < concount ; i++ )
  { REAL *coord,ff,fs;
    REAL fval,grad[MAXCOORD];
    coord = e_info->x[0];
    eval_all(constr[i]->formula,coord,SDIM,&fval,grad,e_id);
    ff = SDIM_dot(grad,grad);
    if ( ff <= 0.0 )
    { sprintf(errmsg,"Edge %s is on a CONVEX constraint at zero gradient.\n",
          ELNAME(e_info->id));
      kb_error(2148,errmsg,WARNING);
      ff = 1.0;
    }
    fs = SDIM_dot(s,grad);
    sprenergy += fabs(fs)*sqrt(ss/ff)/12;

    coord = e_info->x[1];
    eval_all(constr[i]->formula,coord,SDIM,&fval,grad,e_id);
    ff = SDIM_dot(grad,grad);
    if ( ff <= 0.0 )
    { sprintf(errmsg,"Edge %s is on a CONVEX constraint at zero gradient.\n",
          ELNAME(e_info->id)+1);
      kb_error(2149,errmsg,WARNING);
      ff = 1.0;
    }
    fs = SDIM_dot(s,grad);
    sprenergy += fabs(fs)*sqrt(ss/ff)/12;
  }

  sprenergy *= web.spring_constant;
  return sprenergy;
}

/****************************************************************
*
*  Function: gap_grads()
*
*  Purpose:  Since only vertices are actually confined to constraints,
*                edges and faces supposedly on constraints can pull
*                away from convex constraints, and in fact do, since
*                a long edge short-cuts the constraints.  To prevent
*                this and encourage equal-length constraint edges, an
*                energy penalty is inflicted for an edge angling away
*                from its constraint.  
*                The energy penalty is 2/3 of the area of the right
*                triangle whose base is half the side and whose hypoteneuse
*                lies on the constraint tangent.  This is done for both
*                ends of the side.
*/


REAL  gap_grads(e_info)
struct qinfo *e_info;
{
  edge_id e_id = e_info->id;
  REAL *s;
  REAL ss; /* square lengths */
  struct constraint *constr[MAXCONPER];
  int concount;
  conmap_t * conmap;
  int i,j; 
  MAT2D(second,MAXCOORD,MAXCOORD); /* for second partials */
  REAL sprenergy = 0.0;

  if ( get_eattr(e_id) & FIXED ) return 0.0;
  if ( !(get_eattr(e_id) & CONSTRAINT) ) return 0.0;

  /* find which constraints have CONVEX attribute */
  conmap = get_e_constraint_map(e_id);
  for ( j = 1,i=0 ; j <= (int)conmap[0] ; j++ )
  {
    constr[i] = get_constraint(conmap[j]);
    if ( constr[i]->attr & B_CONVEX ) i++;    /* keep this one */
  }
  if ( i == 0 ) return 0.0;
  concount = i;  

  /* now the calculation */
  s = e_info->sides[0][0];
  ss = SDIM_dot(s,s);

  for ( i = 0 ; i < concount ; i++ )
  { REAL *coord;
    REAL fval,grad[MAXCOORD];
    REAL ff,fs,t;

    coord = e_info->x[0];
    eval_second(constr[i]->formula,coord,SDIM,&fval,grad,second,e_info->id);
    ff = SDIM_dot(grad,grad);
    if ( ff <= 0.0 ) ff = 1.0;
    fs = SDIM_dot(s,grad);
    t = sqrt(ss/ff);
    sprenergy += fabs(fs)*sqrt(ss/ff)/12;
    if ( fs < 0.0 ) t = -t;  /* to take care of fabs() */
    for ( j = 0 ; j < SDIM ; j++ )
    { REAL g;
      g = -t*grad[j] + t*SDIM_dot(s,second[j]) 
                     + fs/t*(-s[j]/ff - ss/ff/ff*SDIM_dot(grad,second[j]));
      e_info->grad[0][j] += web.spring_constant*g/12;
      g = t*grad[j] + fs/t/ff*s[j];
      e_info->grad[1][j] += web.spring_constant*g/12;
    }
    coord = e_info->x[1];
    eval_second(constr[i]->formula,coord,SDIM,&fval,grad,second,e_info->id);
    ff = SDIM_dot(grad,grad);
    if ( ff <= 0.0 ) ff = 1.0;
    fs = SDIM_dot(s,grad);
    sprenergy += fabs(fs)*sqrt(ss/ff)/12;
    t = sqrt(ss/ff);
    if ( fs < 0.0 ) t = -t;  /* to take care of fabs() */
    for ( j = 0 ; j < SDIM ; j++ )
    { REAL g;
      g = t*grad[j] + t*SDIM_dot(s,second[j]) 
                    + fs/t*(s[j]/ff - ss/ff/ff*SDIM_dot(grad,second[j]));
      e_info->grad[1][j] += web.spring_constant*g/12;
      g = -t*grad[j] - fs/t/ff*s[j];
      e_info->grad[0][j] += web.spring_constant*g/12;
    }
  }

  sprenergy *= web.spring_constant;
  return sprenergy;
} /* end gap_grads() */

/***********************************************************************/
/************************************************************************
     Named method: linear_elastic

     Linear elastic strain energy on facets.
     (for Frank Baginski's NASA balloon models)

     Let S be Gram matrix of unstrained facet (dots of sides).
     Let Q be the inverse of S.
     Let F be Gram matrix of strained facet.
     Let C = (FQ-I)/2, the Cauchy-Green strain tensor.
     Let v be Poisson ratio.
     Then energy density is 
        (1/2/(1+v))(Tr(C^2) + v*(Tr C)^2/(1-(dim-1)*v))

     Each facet has extra attribute poisson_ratio and 
     extra attribute array form_factors[3] = {s11,s12,s22}
     where sij = dot(si,sj) and s1 = (v1-v0), s2 = (v2-v0).

************************************************************************/

#define POISSON_NAME "poisson_ratio"
#define FORM_FACTORS_NAME "form_factors"
int poisson_attr; /* number of poisson_ratio extra attribute */
int form_factors_attr; /* number of form_factors extra attribute */

/***************************************************************
*
*  function: linear_elastic_init()
*
*  purpose: Make sure needed extra attributes are present.
*              
*
*/

void linear_elastic_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{ 

  if ( web.modeltype != LINEAR )
     kb_error(2529,"linear_elastic method only for LINEAR model.\n",RECOVERABLE);

  if ( web.dimension != 2 )
     kb_error(2150,"linear_elastic method only for SOAPFILM model.\n",
        RECOVERABLE);

  /* extra edge atribute */
  poisson_attr = find_attribute(FACET,POISSON_NAME);
  if ( poisson_attr < 0 ) /* not found */
     kb_error(3200,"Facet extra attribute poisson_ratio missing. Needed by linear_elastic.\n",RECOVERABLE);

  form_factors_attr = find_attribute(FACET,FORM_FACTORS_NAME);
  if ( form_factors_attr < 0 ) /* not found */
     kb_error(2152,"Facet extra attribute form_factors real[3] missing. Needed by linear_elastic.\n",RECOVERABLE);

  if ( EXTRAS(FACET)[form_factors_attr].array_spec.datacount != 3 )
     kb_error(2153,"Facet extra attribute form_factors must have size 3.\n",
        RECOVERABLE);
}

/************************************************************************
*
* function: linear_elastic_all()
*
* purpose: energy, gradient, and hessian for linear_elastic method.
*/
REAL linear_elastic_all ARGS((struct qinfo *,int));

REAL linear_elastic_all(f_info,mode)
struct qinfo *f_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */
{
  REAL *s;  /* pointer to extra attributes */
  REAL **side; 
  REAL q11,q12,q22;  /* Q entries */
  REAL det;    /* det S */
  REAL area;  /* reference area of facet */
  REAL poisson; /* poisson ratio */
  REAL f11,f12,f22;
  REAL c11,c12,c21,c22;
  REAL energy;
  REAL dc11dv[FACET_VERTS][MAXCOORD];
  REAL dc12dv[FACET_VERTS][MAXCOORD];
  REAL dc21dv[FACET_VERTS][MAXCOORD];
  REAL dc22dv[FACET_VERTS][MAXCOORD];
  REAL ddc11dv[FACET_VERTS][MAXCOORD][FACET_VERTS];
  REAL ddc12dv[FACET_VERTS][MAXCOORD][FACET_VERTS];
  REAL ddc21dv[FACET_VERTS][MAXCOORD][FACET_VERTS];
  REAL ddc22dv[FACET_VERTS][MAXCOORD][FACET_VERTS];
  int i,j,ii,jj;
  REAL coeff1,coeff2;

  poisson = *(REAL*)get_extra(f_info->id,poisson_attr);
  s = (REAL*)get_extra(f_info->id,form_factors_attr);
  det = s[0]*s[2] - s[1]*s[1];
  if ( det <= 0.0 )
  { if ( mode == METHOD_VALUE ) return 0.0;
    sprintf(errmsg,"linear_elastic: Facet %s has unstrained area <= 0.\n",
      ELNAME(f_info->id));
    kb_error(2154,errmsg,RECOVERABLE);
  }
  area = sqrt(det)/2;
  coeff1 = area/8/(1 + poisson);
  coeff2 = coeff1*poisson/(1 - (web.dimension-1)*poisson);
  q11 = s[2]/det; q12 = -s[1]/det; q22 = s[0]/det;

  side = f_info->sides[0];
  f11 = SDIM_dot(side[0],side[0]);
  f12 = SDIM_dot(side[0],side[1]);
  f22 = SDIM_dot(side[1],side[1]);

  c11 = f11*q11 + f12*q12 - 1;
  c12 = f11*q12 + f12*q22;
  c21 = f12*q11 + f22*q12;
  c22 = f12*q12 + f22*q22 - 1;

  energy = coeff1*(c11*c11+c12*c21+c12*c21+c22*c22)
                 + coeff2*(c11+c22)*(c11+c22);

  if ( mode == METHOD_VALUE ) return energy;

  /* gradient */

  for ( i = 0 ; i < SDIM  ; i++ )
  {
     dc11dv[1][i] = 2*side[0][i]*q11 + side[1][i]*q12;
     dc11dv[2][i] =                          side[0][i]*q12;
     dc12dv[1][i] = 2*side[0][i]*q12 + side[1][i]*q22;
     dc12dv[2][i] =                          side[0][i]*q22;
     dc21dv[1][i] = side[1][i]*q11;
     dc21dv[2][i] = side[0][i]*q11 + 2*side[1][i]*q12;
     dc22dv[1][i] = side[1][i]*q12;
     dc22dv[2][i] = side[0][i]*q12 + 2*side[1][i]*q22;
     dc11dv[0][i] = -(dc11dv[1][i] + dc11dv[2][i]);
     dc12dv[0][i] = -(dc12dv[1][i] + dc12dv[2][i]);
     dc21dv[0][i] = -(dc21dv[1][i] + dc21dv[2][i]);
     dc22dv[0][i] = -(dc22dv[1][i] + dc22dv[2][i]);
  }


  for ( j = 0 ; j < FACET_VERTS  ; j++ )
     for ( i = 0 ; i < SDIM  ; i++ )
     { f_info->grad[j][i] = (coeff1*(2*c11*dc11dv[j][i]  
                                 + 2*c12*dc21dv[j][i]
                                 + 2*c21*dc12dv[j][i]
                                 + 2*c22*dc22dv[j][i]) 
                  + 2*coeff2*(c11+c22)*(dc11dv[j][i] +dc22dv[j][i]));
     }

  if ( mode == METHOD_GRADIENT ) return energy;

  /* hessian */

  for ( i = 0 ; i < SDIM  ; i++ )
  {
     ddc11dv[1][i][1] = 2*q11;
     ddc11dv[1][i][2] = q12;

     ddc11dv[2][i][1] = q12;
     ddc11dv[2][i][2] = 0.0;

     ddc12dv[1][i][1] = 2*q12;
     ddc12dv[1][i][2] = q22;

     ddc12dv[2][i][1] = q22;
     ddc12dv[2][i][2] = 0.0;

     ddc21dv[1][i][1] = 0.0;
     ddc21dv[1][i][2] = q11;

     ddc21dv[2][i][1] = q11;
     ddc21dv[2][i][2] = 2*q12;

     ddc22dv[1][i][1] = 0.0;
     ddc22dv[1][i][2] = q12;

     ddc22dv[2][i][1] = q12;
     ddc22dv[2][i][2] = 2*q22;

     for ( j = 1 ; j < FACET_VERTS;  j++ )
     { 
        ddc11dv[0][i][j] = -(ddc11dv[1][i][j] + ddc11dv[2][i][j]);
        ddc12dv[0][i][j] = -(ddc12dv[1][i][j] + ddc12dv[2][i][j]);
        ddc21dv[0][i][j] = -(ddc21dv[1][i][j] + ddc21dv[2][i][j]);
        ddc22dv[0][i][j] = -(ddc22dv[1][i][j] + ddc22dv[2][i][j]);
        ddc11dv[j][i][0] = -(ddc11dv[j][i][1] + ddc11dv[j][i][2]);
        ddc12dv[j][i][0] = -(ddc12dv[j][i][1] + ddc12dv[j][i][2]);
        ddc21dv[j][i][0] = -(ddc21dv[j][i][1] + ddc21dv[j][i][2]);
        ddc22dv[j][i][0] = -(ddc22dv[j][i][1] + ddc22dv[j][i][2]);
     }
     ddc11dv[0][i][0] = -(ddc11dv[1][i][0] + ddc11dv[2][i][0]);
     ddc12dv[0][i][0] = -(ddc12dv[1][i][0] + ddc12dv[2][i][0]);
     ddc21dv[0][i][0] = -(ddc21dv[1][i][0] + ddc21dv[2][i][0]);
     ddc22dv[0][i][0] = -(ddc22dv[1][i][0] + ddc22dv[2][i][0]);
  }

  for ( j = 0 ; j < FACET_VERTS  ; j++ )
     for ( i = 0 ; i < SDIM  ; i++ )
        for ( jj = 0 ; jj < FACET_VERTS  ; jj++ )
        {
          f_info->hess[j][jj][i][i] += 
                (coeff1*( 2*c11*ddc11dv[j][i][jj]
                         + 2*c12*ddc21dv[j][i][jj]
                         + 2*c21*ddc12dv[j][i][jj]
                         + 2*c22*ddc22dv[j][i][jj]) 
                 + 2*coeff2*(c11+c22)*(ddc11dv[j][i][jj]+ddc22dv[j][i][jj]));

          for ( ii = 0 ; ii < SDIM  ; ii++ )
             f_info->hess[j][jj][i][ii] += 
                  (coeff1*(2*dc11dv[jj][ii]*dc11dv[j][i] 
                          + 2*dc12dv[jj][ii]*dc21dv[j][i]
                          + 2*dc21dv[jj][ii]*dc12dv[j][i]
                          + 2*dc22dv[jj][ii]*dc22dv[j][i]
                         ) 
                  + 2*coeff2*(dc11dv[jj][ii]+dc22dv[jj][ii])
                                     *(dc11dv[j][i] +dc22dv[j][i]));
         }

  return energy;
}

/**************************************************************
*
*  function: linear_elastic_energy()
*
*  purpose: calculates energy of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL linear_elastic_energy(f_info)
struct qinfo *f_info;
{
 return linear_elastic_all(f_info,METHOD_VALUE);
}



/**************************************************************
*
*  function: linear_elastic_gradient()
*
*  purpose: calculates gradient of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL linear_elastic_gradient(f_info)
struct qinfo *f_info;
{
 return linear_elastic_all(f_info,METHOD_GRADIENT);
}


/**************************************************************
*
*  function: linear_elastic_hessian()
*
*  purpose: calculates hessian of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL linear_elastic_hessian(f_info)
struct qinfo *f_info;
{
 return linear_elastic_all(f_info,METHOD_HESSIAN);
}

/************************************************************************
     Named method: linear_elastic_B

     Linear elastic strain energy on facets, with mobile reference
     coordinates as two extra dimensions at each vertex.  Hence 3D
     balloon has to be set up as 5D surface.  Does not use the
     form_factors attribute of linear_elastic.  Does use same poisson_ratio.
     (for Frank Baginski's NASA balloon models)

     Let S be Gram matrix of unstrained facet (dots of sides).
     Let Q be the inverse of S.
     Let F be Gram matrix of strained facet.
     Let C = (FQ-I)/2, the Cauchy-Green strain tensor.
     Let v be Poisson ratio.
     Then energy density is 
        (1/2/(1+v))(Tr(C^2) + v*(Tr C)^2/(1-(dim-1)*v))

     Each facet has extra attribute poisson_ratio.

************************************************************************/

#define POISSON_NAME "poisson_ratio"
#define FORM_FACTORS_NAME "form_factors"
#define LEBWEIGHT_NAME "LEBweight"
int LEBweight_attr; /* optional per-facet weight factor */

/************************************************************************
*
*  function: linear_elastic_B_init()
*
*  purpose: Make sure needed extra attributes are present.
*              
*
*/

void linear_elastic_B_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{ 

  if ( web.modeltype != LINEAR )
     kb_error(2155,"linear_elastic_B method only for LINEAR model.\n",RECOVERABLE);

  /* extra facet attribute for poisson_ratio */
  poisson_attr = find_attribute(FACET,POISSON_NAME);
  if ( poisson_attr < 0 ) /* not found */
     kb_error(2156,"Facet extra attribute poisson_ratio missing. Needed by linear_elastic_B.\n",RECOVERABLE);

  LEBweight_attr = find_attribute(FACET,LEBWEIGHT_NAME); /* optional */

  if ( web.dimension != 2 )
     kb_error(2157,"linear_elastic_B method only for SOAPFILM model.\n",RECOVERABLE);
}

/************************************************************************
*
* function: linear_elastic_B_all()
*
* purpose: energy, gradient, and hessian for linear_elastic_B method.
*/
REAL linear_elastic_B_all ARGS((struct qinfo *,int));

REAL linear_elastic_B_all(f_info,mode)
struct qinfo *f_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */
{
  REAL q11,q12,q22;  /* Q entries */
  REAL Q[2][2];
  REAL det;    /* det S */
  REAL area;  /* reference area of facet */
  REAL areasign; /* for reference area orientation */
  REAL poisson; /* poisson ratio */
  REAL f11,f12,f22;
  REAL F[2][2];
  REAL c11,c12,c21,c22;
  REAL energy;
  REAL stuff;
  REAL dadv[FACET_VERTS][MAXCOORD];
  REAL dstuff[FACET_VERTS][MAXCOORD];
  REAL dCdv[2][2][FACET_VERTS][MAXCOORD];
  REAL dFdv[2][2][FACET_VERTS][MAXCOORD];
  REAL dQdv[2][2][FACET_VERTS][MAXCOORD];
  REAL dSdv[2][2][FACET_VERTS][MAXCOORD];
  REAL ddFdv[2][2][2][2];
  REAL ddSdv[2][2][2][2];
  REAL ddCdv[2][2][FACET_VERTS][FACET_VERTS][MAXCOORD][MAXCOORD];
  REAL ddQdv[2][2][FACET_VERTS][FACET_VERTS][MAXCOORD][MAXCOORD];
  REAL ddstuff[FACET_VERTS][FACET_VERTS][MAXCOORD][MAXCOORD];
  REAL ddadv[FACET_VERTS][FACET_VERTS][MAXCOORD][MAXCOORD];
  int i,j,m,n,a,b,c,d,e;
  REAL coeff1,coeff2;
  REAL s[2][2];  /* form factors matrix */
  int sdim = SDIM - web.dimension; /* true space dimension */
  REAL combo;  /* combination coeff for second edge orthogonality */
  REAL weight;  /* facet weighting factor */
  REAL side[2][MAXCOORD]; 

  poisson = *(REAL*)get_extra(f_info->id,poisson_attr);
  if ( LEBweight_attr >= 0 )
    weight = *(REAL*)get_extra(f_info->id,LEBweight_attr);
  else weight = 1.0;

  /* compute orthogonal second side */
  for ( j = 0 ; j < 2 ; j++ ) 
   for ( i = 0 ; i < SDIM ; i++ ) 
    side[j][i] = f_info->sides[0][j][i];
  s[0][0] = dot(side[0]+sdim,side[0]+sdim,web.dimension);  /* in ref */
  if ( s[0][0] == 0.0 ) return 0.0;
  s[0][1] = dot(side[0]+sdim,side[1]+sdim,web.dimension);  /* in ref */
  combo = s[0][1]/s[0][0];
  for ( i = 0 ; i < SDIM ; i++ ) 
    side[1][i] -= combo*side[0][i];

  /* compute form factors */
  for ( i = 0 ; i < web.dimension ; i++ )
    for ( j = 0 ; j < web.dimension ; j++ )
      s[i][j] = dot(side[i]+sdim,side[j]+sdim,web.dimension); 
  det = s[0][0]*s[1][1] - s[0][1]*s[1][0];
  if ( det <= 0.0 )
     { if ( mode == METHOD_VALUE ) return 0.0;
       sprintf(errmsg,"linear_elastic_B: Facet %s has unstrained area 0.\n",
          ELNAME(f_info->id));
       kb_error(2158,errmsg,RECOVERABLE);
     }
  area = side[0][sdim]*side[1][sdim+1] - side[0][sdim+1]*side[1][sdim];
  areasign = (area < 0.0) ? -0.5 : 0.5; /* triangle factor and orientation */
  area *= areasign;
  coeff1 = 1.0/8/(1 + poisson);
  coeff2 = coeff1*poisson/(1 - (web.dimension-1)*poisson);
  Q[0][0] = q11 = s[1][1]/det; 
  Q[0][1] = Q[1][0] = q12 = -s[0][1]/det; 
  Q[1][1] = q22 = s[0][0]/det;

  F[0][0] = f11 = dot(side[0],side[0],sdim);
  F[0][1] = F[1][0] = f12 = dot(side[0],side[1],sdim);
  F[1][1] = f22 = dot(side[1],side[1],sdim);

  c11 = f11*q11 + f12*q12 - 1;
  c12 = f11*q12 + f12*q22;
  c21 = f12*q11 + f22*q12;
  c22 = f12*q12 + f22*q22 - 1;

  stuff = coeff1*(c11*c11+c12*c21+c12*c21+c22*c22)
                 + coeff2*(c11+c22)*(c11+c22);
  energy = weight*area*stuff;

  if ( mode == METHOD_VALUE ) return energy;

  /* gradient */

  for ( i = 0 ; i < sdim  ; i++ )  /* with respect to space coord */
  {  
    for ( a = 0 ; a < web.dimension ; a++ )
      for ( b = 0 ; b < web.dimension ; b++ )
        for ( m = 0 ; m < web.dimension ; m++ )
          dFdv[a][b][m][i] = (m==a ? side[b][i] : 0.0)
                           + (m==b ? side[a][i] : 0.0);
  }
  for ( i = sdim ; i < SDIM  ; i++ )  /* with respect to ref coord */
  {  
    for ( a = 0 ; a < web.dimension ; a++ )
      for ( b = 0 ; b < web.dimension ; b++ )
        for ( m = 0 ; m < web.dimension ; m++ )
          dSdv[a][b][m][i] = (m==a ? side[b][i] : 0.0)
                           + (m==b ? side[a][i] : 0.0);
  }
  for ( i = sdim ; i < SDIM  ; i++ )  /* with respect to ref coord */
  {  
    for ( a = 0 ; a < web.dimension ; a++ )
      for ( b = 0 ; b < web.dimension ; b++ )
        for ( m = 0 ; m < web.dimension ; m++ )
        { REAL sum = 0.0;
          for ( c = 0 ; c < web.dimension ; c++ )
            for ( d = 0 ; d < web.dimension ; d++ )
                sum -= Q[a][c]*dSdv[c][d][m][i]*Q[d][b];
            dQdv[a][b][m][i] = sum;
        }
  }

  for ( i = 0 ; i < sdim  ; i++ )  /* with respect to space coord */
  {
     for ( a = 0 ; a < web.dimension ; a++ )
        for ( b = 0 ; b < web.dimension ; b++ )
          for ( m = 0 ; m < web.dimension ; m++ )
          { REAL sum = 0.0;
             for ( c = 0 ; c < web.dimension ; c++ )
                sum += dFdv[a][c][m][i]*Q[c][b];
             dCdv[a][b][m][i] = sum;
          }
  }
  for ( i = sdim ; i < SDIM  ; i++ )  /* with respect to ref coord */
  { 
     for ( a = 0 ; a < web.dimension ; a++ )
        for ( e = 0 ; e < web.dimension ; e++ )
          for ( m = 0 ; m < web.dimension ; m++ )
          { REAL sum = 0.0;
             for ( b = 0 ; b < web.dimension ; b++ )
                sum += F[a][b]*dQdv[b][e][m][i];
             dCdv[a][e][m][i] = sum;
          }
  }


  for ( j = 0 ; j < web.dimension  ; j++ )
     for ( i = 0 ; i < SDIM  ; i++ )
     { dstuff[j][i] = (coeff1*(2*c11*dCdv[0][0][j][i]  
                                 + 2*c12*dCdv[1][0][j][i]
                                 + 2*c21*dCdv[0][1][j][i]
                                 + 2*c22*dCdv[1][1][j][i]) 
                  + 2*coeff2*(c11+c22)*(dCdv[0][0][j][i] +dCdv[1][1][j][i])); 
     }

  /* reference area change */
  for ( i = 0 ; i < SDIM  ; i++ ) 
    for ( a = 0 ; a < FACET_VERTS ; a++ ) dadv[a][i] = 0.0;
  dadv[0][sdim] = side[1][sdim+1]*areasign;
  dadv[1][sdim+1] = side[0][sdim]*areasign;
  dadv[0][sdim+1] = -side[1][sdim]*areasign;
  dadv[1][sdim] = -side[0][sdim+1]*areasign;

  /* grand finale for gradient */
  for ( a = 0 ; a < web.dimension ; a++ )
     for ( i = 0 ; i < SDIM  ; i++ ) 
     { f_info->grad[a+1][i] = weight*(dadv[a][i]*stuff + area*dstuff[a][i]);
       f_info->grad[0][i] -= weight*(dadv[a][i]*stuff + area*dstuff[a][i]);
     }
  /* combo correction */
  for ( i = 0 ; i < SDIM  ; i++ ) 
  { f_info->grad[1][i] -= combo*f_info->grad[2][i];
    f_info->grad[0][i] += combo*f_info->grad[2][i];
  }
 
  if ( mode == METHOD_GRADIENT ) return energy;

  /* hessian */

  /* calculate ddFdv and ddSdv, which are same and nonzero only for 
      like coordinates, so coordinate index not used */
  for ( a = 0 ; a < web.dimension ; a++ )
     for ( b = 0 ; b < web.dimension ; b++ )
        for ( m = 0 ; m < web.dimension ; m++ )
          for ( n = 0 ; n < web.dimension ; n++ )
          { ddFdv[a][b][m][n] = (a==m && b==n ? 1.0 : 0.0) + (a==n && b==m ? 1.0 : 0.0);
             ddSdv[a][b][m][n] = (a==m && b==n ? 1.0 : 0.0) + (a==n && b==m ? 1.0 : 0.0);
          }

  /* calculate ddQdv */
  for ( i = sdim ; i < SDIM  ; i++ )
    for ( j = sdim ; j < SDIM  ; j++ )
     for ( a = 0 ; a < web.dimension ; a++ )
        for ( b = 0 ; b < web.dimension ; b++ )
          for ( m = 0 ; m < web.dimension ; m++ )
             for ( n = 0 ; n < web.dimension ; n++ )
             { REAL sum = 0.0;
                for ( c = 0 ; c < web.dimension ; c++ )
                  for ( d = 0 ; d < web.dimension ; d++ )
                     sum += - dQdv[a][c][m][i]*dSdv[c][d][n][j]*Q[d][b]
                              - ( i==j ? Q[a][c]*ddSdv[c][d][m][n]*Q[d][b] : 0.0 )
                              - Q[a][c]*dSdv[c][d][m][i]*dQdv[d][b][n][j];
                ddQdv[a][b][m][n][i][j] = sum; 
             }

  /* calculate ddCdv */
  for ( i = 0 ; i < SDIM  ; i++ )
    for ( j = 0 ; j < SDIM  ; j++ )
     for ( a = 0 ; a < web.dimension ; a++ )
        for ( b = 0 ; b < web.dimension ; b++ )
        { 
          for ( m = 0 ; m < web.dimension ; m++ )
             for ( n = 0 ; n < web.dimension ; n++ )
             { REAL sum = 0.0;
                for ( c = 0 ; c < web.dimension ; c++ )
                  sum += ( (i==j && i < sdim) ? ddFdv[a][c][m][n]*Q[c][b] : 0.0 )
                         + ( (i < sdim && j >= sdim) ? dFdv[a][c][m][i]*dQdv[c][b][n][j] : 0.0)
                         + ( (j < sdim && i >= sdim) ? dFdv[a][c][n][j]*dQdv[c][b][m][i] : 0.0)
                         + ( (i >= sdim && j >= sdim) ? F[a][c]*ddQdv[c][b][m][n][i][j] : 0.0);
                ddCdv[a][b][m][n][i][j] = sum;
             }
  }
  /* calculate ddstuff */
  for ( i = 0 ; i < SDIM  ; i++ )
    for ( j = 0 ; j < SDIM  ; j++ )
     for ( a = 0 ; a < web.dimension ; a++ )
        for ( b = 0 ; b < web.dimension ; b++ )
        { REAL term; 
          term = (coeff1*( 2*c11*ddCdv[0][0][a][b][i][j]
                         + 2*c12*ddCdv[1][0][a][b][i][j]
                         + 2*c21*ddCdv[0][1][a][b][i][j]
                         + 2*c22*ddCdv[1][1][a][b][i][j]) 
                 + 2*coeff2*(c11+c22)*(ddCdv[0][0][a][b][i][j]+ddCdv[1][1][a][b][i][j]))

                 + (coeff1*(2*dCdv[0][0][a][i]*dCdv[0][0][b][j] 
                          + 2*dCdv[0][1][a][i]*dCdv[1][0][b][j]
                          + 2*dCdv[1][0][a][i]*dCdv[0][1][b][j]
                          + 2*dCdv[1][1][a][i]*dCdv[1][1][b][j]
                         ) 
                  + 2*coeff2*(dCdv[0][0][a][i]+dCdv[1][1][a][i])
                                     *(dCdv[0][0][b][j] +dCdv[1][1][b][j]));
            ddstuff[a][b][i][j] = term;
         }

  /* area hessian */
  for ( i = sdim ; i < SDIM  ; i++ )
    for ( j = sdim ; j < SDIM  ; j++ )
     for ( a = 0 ; a < web.dimension ; a++ )
        for ( b = 0 ; b < web.dimension ; b++ )
          ddadv[a][b][i][j] = 0.0;
  ddadv[0][1][sdim][sdim+1] = areasign;
  ddadv[1][0][sdim+1][sdim] = areasign;
  ddadv[0][1][sdim+1][sdim] = -areasign;
  ddadv[1][0][sdim][sdim+1] = -areasign;


  /* big Hessian finale */
  for ( i = 0 ; i < SDIM  ; i++ )
    for ( j = 0 ; j < SDIM  ; j++ )
     for ( a = 0 ; a < web.dimension ; a++ )
        for ( b = 0 ; b < web.dimension ; b++ )
        { REAL term;
            term =
                 ( (i >= sdim && j >= sdim) ? ddadv[a][b][i][j]*stuff : 0.0)
              + dadv[a][i]*dstuff[b][j]
              + dadv[b][j]*dstuff[a][i]
              + area*ddstuff[a][b][i][j];
          term *= weight;
          f_info->hess[a+1][b+1][i][j] = term;
          f_info->hess[a+1][0][i][j] -= term;
          f_info->hess[0][b+1][i][j] -= term;
          f_info->hess[0][0][i][j] += term;
        }
  /* combo correction */
  for ( i = 0 ; i < SDIM  ; i++ ) 
   for ( j = 0 ; j < SDIM  ; j++ ) 
   { REAL term;
     term = combo*(combo*f_info->hess[2][2][i][j]-f_info->hess[1][2][i][j]
                          -f_info->hess[2][1][i][j]);
     f_info->hess[1][1][i][j] += term;
     f_info->hess[0][1][i][j] -= term;
     f_info->hess[1][0][i][j] -= term;
     f_info->hess[0][0][i][j] += term;
     term = combo*f_info->hess[2][2][i][j];
     f_info->hess[1][2][i][j] -= term;
     f_info->hess[0][2][i][j] += term;
     f_info->hess[1][0][i][j] += term;
     f_info->hess[0][0][i][j] -= term;
     term = combo*f_info->hess[2][2][i][j];
     f_info->hess[2][1][i][j] -= term;
     f_info->hess[0][1][i][j] += term;
     f_info->hess[2][0][i][j] += term;
     f_info->hess[0][0][i][j] -= term;
   }
  return energy;
}

/**************************************************************
*
*  function: linear_elastic_B_energy()
*
*  purpose: calculates energy of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL linear_elastic_B_energy(f_info)
struct qinfo *f_info;
{
 return linear_elastic_B_all(f_info,METHOD_VALUE);
}



/**************************************************************
*
*  function: linear_elastic_B_gradient()
*
*  purpose: calculates gradient of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL linear_elastic_B_gradient(f_info)
struct qinfo *f_info;
{
 return linear_elastic_B_all(f_info,METHOD_GRADIENT);
}


/**************************************************************
*
*  function: linear_elastic_B_hessian()
*
*  purpose: calculates hessian of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL linear_elastic_B_hessian(f_info)
struct qinfo *f_info;
{
 return linear_elastic_B_all(f_info,METHOD_HESSIAN);
}

/************************************************************************
*************************************************************************

     Named method: relaxed_elastic

     Linear elastic strain energy on facets, with mobile reference
     coordinates as two extra dimensions at each vertex.  Hence 3D
     balloon has to be set up as 5D surface.  Does not use the
     form_factors attribute of linear_elastic.  Does use same poisson_ratio.
     Revised from linear_elastic_B to implement Bill Collier's
     relaxed energy model.  Basically, stress is set to 0 when stress
     would be negative, to model wrinkling.
     (for Frank Baginski's NASA balloon models)

     Let S be Gram matrix of unstrained facet (dots of sides).
     Let Q be the inverse of S.
     Let F be Gram matrix of strained facet.
     Let C = (FQ-I)/2, the Cauchy-Green strain tensor.
     Let v be Poisson ratio.
     Let eps1 and eps2 be the eigenvalues of C.
     Let sigma1 = eps1+v*eps2 and sigma2 = eps2+v*eps1 be the stress
       eigenvalues.
     Then energy density is 
        (1/2/(1+v))(Tr(C^2) + v*(Tr C)^2/(1-(dim-1)*v))    sigma1,sigma2 >= 0
        eps1^2/2    sigma2 < 0, eps1 > 0
        eps2^2/2    sigma1 < 0, eps2 > 0
        0           eps2 < 0, eps1 < 0.

     Each facet has extra attribute poisson_ratio for v.

************************************************************************/

/************************************************************************
*
*  function: relaxed_elastic_init()
*
*  purpose: Make sure needed extra attributes are present.
*              
*
*/

void relaxed_elastic_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{ 

  if ( web.modeltype != LINEAR )
     kb_error(2159,"relaxed_elastic method only for LINEAR model.\n",RECOVERABLE);

  if ( web.dimension != 2 )
     kb_error(2160,"relaxed_elastic method only for SOAPFILM model.\n",RECOVERABLE);

  /* extra facet attribute for poisson_ratio */
  poisson_attr = find_attribute(FACET,POISSON_NAME);
  if ( poisson_attr < 0 ) /* not found */
     kb_error(2161,"Facet extra attribute poisson_ratio missing. Needed by relaxed_elastic.\n",RECOVERABLE);

  LEBweight_attr = find_attribute(FACET,LEBWEIGHT_NAME); /* optional */

}

/************************************************************************
*
* function: relaxed_elastic_all()
*
* purpose: energy, gradient, and hessian for relaxed_elastic_method.
*/

REAL relaxed_elastic_all ARGS((struct qinfo *,int,int));
#define ONE_STRESS 1
#define TWO_STRESS 2

REAL relaxed_elastic_all(f_info,mode,part)
struct qinfo *f_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */
int part; /* flag for one stress, two stress, or both parts */
{
  REAL q11,q12,q22;  /* Q entries */
  REAL Q[2][2];
  REAL det;    /* det S */
  REAL area;  /* reference area of facet */
  REAL areasign; /* for reference area orientation */
  REAL poisson; /* poisson ratio */
  REAL f11,f12,f22;
  REAL F[2][2];
  REAL c11,c12,c21,c22;
  REAL energy;
  REAL stuff;
  REAL dadv[FACET_VERTS][MAXCOORD];
  REAL dstuff[FACET_VERTS][MAXCOORD];
  REAL dCdv[2][2][FACET_VERTS][MAXCOORD];
  REAL dFdv[2][2][FACET_VERTS][MAXCOORD];
  REAL dQdv[2][2][FACET_VERTS][MAXCOORD];
  REAL dSdv[2][2][FACET_VERTS][MAXCOORD];
  REAL ddFdv[2][2][2][2];
  REAL ddSdv[2][2][2][2];
  REAL ddCdv[2][2][FACET_VERTS][FACET_VERTS][MAXCOORD][MAXCOORD];
  REAL ddQdv[2][2][FACET_VERTS][FACET_VERTS][MAXCOORD][MAXCOORD];
  REAL ddstuff[FACET_VERTS][FACET_VERTS][MAXCOORD][MAXCOORD];
  REAL ddadv[FACET_VERTS][FACET_VERTS][MAXCOORD][MAXCOORD];
  int i,j,m,n,a,b,c,d,e;
  REAL coeff1,coeff2;
  REAL s[2][2];  /* form factors matrix */
  int sdim = SDIM - web.dimension; /* true space dimension */
  REAL combo;  /* combination coeff for second edge orthogonality */
  REAL weight;  /* facet weighting factor */
  REAL side[2][MAXCOORD]; 
  REAL eps1,eps2; /* strain eigenvalues */
  REAL TrC,DetC;  /* trace and determinant of C */

  poisson = *(REAL*)get_extra(f_info->id,poisson_attr);
  if ( LEBweight_attr >= 0 )
    weight = *(REAL*)get_extra(f_info->id,LEBweight_attr);
  else weight = 1.0;

  /* compute orthogonal second side */
  for ( j = 0 ; j < 2 ; j++ ) 
   for ( i = 0 ; i < SDIM ; i++ ) 
    side[j][i] = f_info->sides[0][j][i];
  s[0][0] = dot(side[0]+sdim,side[0]+sdim,web.dimension);  /* in ref */
  if ( s[0][0] == 0.0 ) return 0.0;
  s[0][1] = dot(side[0]+sdim,side[1]+sdim,web.dimension);  /* in ref */
  combo = s[0][1]/s[0][0];
  for ( i = 0 ; i < SDIM ; i++ ) 
    side[1][i] -= combo*side[0][i];

  /* compute form factors */
  for ( i = 0 ; i < web.dimension ; i++ )
    for ( j = 0 ; j < web.dimension ; j++ )
      s[i][j] = dot(side[i]+sdim,side[j]+sdim,web.dimension); 
  det = s[0][0]*s[1][1] - s[0][1]*s[1][0];
  if ( det <= 0.0 )
     { if ( mode == METHOD_VALUE ) return 0.0;
       sprintf(errmsg,"relaxed_elastic_ Facet %s has unstrained area 0.\n",
          ELNAME(f_info->id));
       kb_error(2162,errmsg,RECOVERABLE);
     }
  area = side[0][sdim]*side[1][sdim+1] - side[0][sdim+1]*side[1][sdim];
  areasign = (area < 0.0) ? -0.5 : 0.5; /* triangle factor and orientation */
  area *= areasign;
  coeff1 = 1.0/8/(1 + poisson);
  coeff2 = coeff1*poisson/(1 - (web.dimension-1)*poisson);
  Q[0][0] = q11 = s[1][1]/det; 
  Q[0][1] = Q[1][0] = q12 = -s[0][1]/det; 
  Q[1][1] = q22 = s[0][0]/det;

  F[0][0] = f11 = dot(side[0],side[0],sdim);
  F[0][1] = F[1][0] = f12 = dot(side[0],side[1],sdim);
  F[1][1] = f22 = dot(side[1],side[1],sdim);

  c11 = f11*q11 + f12*q12 - 1;
  c12 = f11*q12 + f12*q22;
  c21 = f12*q11 + f22*q12;
  c22 = f12*q12 + f22*q22 - 1;

  TrC = c11 + c22;
  DetC = c11*c22 - c12*c21;
  eps1 = (TrC + sqrt(TrC*TrC - 4*DetC))/2;
  eps2 = (TrC - sqrt(TrC*TrC - 4*DetC))/2;
 
  if ( eps1 <= 0.0 ) return 0.0;  /* relaxed in both directions */
  stuff = 0.0;
  if ( eps2 + poisson*eps1 <= 0.0 )
  { /* relaxed in one dimension */
    if ( part & ONE_STRESS )
      stuff = eps1*eps1/2/4;  /* 1/4 fudge factor to agree with other */
  }
  else
  { /* stressed in both directions */
    if ( part & TWO_STRESS )
      stuff = coeff1*(c11*c11+c12*c21+c12*c21+c22*c22)
                 + coeff2*(c11+c22)*(c11+c22);
  }
  energy = weight*area*stuff;

  if ( mode == METHOD_VALUE ) return energy;

  /* gradient */

  for ( i = 0 ; i < sdim  ; i++ )  /* with respect to space coord */
  {  
    for ( a = 0 ; a < web.dimension ; a++ )
      for ( b = 0 ; b < web.dimension ; b++ )
        for ( m = 0 ; m < web.dimension ; m++ )
          dFdv[a][b][m][i] = (m==a ? side[b][i] : 0.0)
                           + (m==b ? side[a][i] : 0.0);
  }
  for ( i = sdim ; i < SDIM  ; i++ )  /* with respect to ref coord */
  {  
    for ( a = 0 ; a < web.dimension ; a++ )
      for ( b = 0 ; b < web.dimension ; b++ )
        for ( m = 0 ; m < web.dimension ; m++ )
          dSdv[a][b][m][i] = (m==a ? side[b][i] : 0.0)
                           + (m==b ? side[a][i] : 0.0);
  }
  for ( i = sdim ; i < SDIM  ; i++ )  /* with respect to ref coord */
  {  
    for ( a = 0 ; a < web.dimension ; a++ )
      for ( b = 0 ; b < web.dimension ; b++ )
        for ( m = 0 ; m < web.dimension ; m++ )
        { REAL sum = 0.0;
          for ( c = 0 ; c < web.dimension ; c++ )
            for ( d = 0 ; d < web.dimension ; d++ )
                sum -= Q[a][c]*dSdv[c][d][m][i]*Q[d][b];
            dQdv[a][b][m][i] = sum;
        }
  }

  for ( i = 0 ; i < sdim  ; i++ )  /* with respect to space coord */
  {
     for ( a = 0 ; a < web.dimension ; a++ )
        for ( b = 0 ; b < web.dimension ; b++ )
          for ( m = 0 ; m < web.dimension ; m++ )
          { REAL sum = 0.0;
             for ( c = 0 ; c < web.dimension ; c++ )
                sum += dFdv[a][c][m][i]*Q[c][b];
             dCdv[a][b][m][i] = sum;
          }
  }
  for ( i = sdim ; i < SDIM  ; i++ )  /* with respect to ref coord */
  { 
     for ( a = 0 ; a < web.dimension ; a++ )
        for ( e = 0 ; e < web.dimension ; e++ )
          for ( m = 0 ; m < web.dimension ; m++ )
          { REAL sum = 0.0;
             for ( b = 0 ; b < web.dimension ; b++ )
                sum += F[a][b]*dQdv[b][e][m][i];
             dCdv[a][e][m][i] = sum;
          }
  }


  for ( j = 0 ; j < web.dimension  ; j++ )
    for ( i = 0 ; i < SDIM  ; i++ ) dstuff[j][i] = 0.0;
  if ( eps2 + poisson*eps1 <= 0.0 )
  { /* relaxed in one dimension */
    if ( part & ONE_STRESS )
    for ( j = 0 ; j < web.dimension  ; j++ )
     for ( i = 0 ; i < SDIM  ; i++ )
     { dstuff[j][i] = eps1*0.5/4*(dCdv[0][0][j][i]+dCdv[1][1][j][i]
        + 0.5/sqrt(TrC*TrC - 4*DetC)*(2*TrC*(dCdv[0][0][j][i]+dCdv[1][1][j][i])
           - 4*(c11*dCdv[1][1][j][i] + dCdv[0][0][j][i]*c22
                 - c12*dCdv[1][0][j][i] - c21*dCdv[0][1][j][i])));
     }
  }
  else /* stressed in both directions */
  { 
    if ( part & TWO_STRESS )
    for ( j = 0 ; j < web.dimension  ; j++ )
     for ( i = 0 ; i < SDIM  ; i++ )
     { dstuff[j][i] = (coeff1*(2*c11*dCdv[0][0][j][i]  
                                 + 2*c12*dCdv[1][0][j][i]
                                 + 2*c21*dCdv[0][1][j][i]
                                 + 2*c22*dCdv[1][1][j][i]) 
                  + 2*coeff2*(c11+c22)*(dCdv[0][0][j][i] +dCdv[1][1][j][i])); 
     }
  }

  /* reference area change */
  for ( i = 0 ; i < SDIM  ; i++ ) 
    for ( a = 0 ; a < FACET_VERTS ; a++ ) dadv[a][i] = 0.0;
  dadv[0][sdim] = side[1][sdim+1]*areasign;
  dadv[1][sdim+1] = side[0][sdim]*areasign;
  dadv[0][sdim+1] = -side[1][sdim]*areasign;
  dadv[1][sdim] = -side[0][sdim+1]*areasign;

  /* grand finale for gradient */
  for ( a = 0 ; a < web.dimension ; a++ )
     for ( i = 0 ; i < SDIM  ; i++ ) 
     { f_info->grad[a+1][i] = weight*(dadv[a][i]*stuff + area*dstuff[a][i]);
       f_info->grad[0][i] -= weight*(dadv[a][i]*stuff + area*dstuff[a][i]);
     }
  /* combo correction */
  for ( i = 0 ; i < SDIM  ; i++ ) 
  { f_info->grad[1][i] -= combo*f_info->grad[2][i];
    f_info->grad[0][i] += combo*f_info->grad[2][i];
  }
 
  if ( mode == METHOD_GRADIENT ) return energy;

  /* hessian */

  /* calculate ddFdv and ddSdv, which are same and nonzero only for 
      like coordinates, so coordinate index not used */
  for ( a = 0 ; a < web.dimension ; a++ )
     for ( b = 0 ; b < web.dimension ; b++ )
        for ( m = 0 ; m < web.dimension ; m++ )
          for ( n = 0 ; n < web.dimension ; n++ )
          { ddFdv[a][b][m][n] = (a==m && b==n ? 1.0 : 0.0) + (a==n && b==m ? 1.0 : 0.0);
             ddSdv[a][b][m][n] = (a==m && b==n ? 1.0 : 0.0) + (a==n && b==m ? 1.0 : 0.0);
          }

  /* calculate ddQdv */
  for ( i = sdim ; i < SDIM  ; i++ )
    for ( j = sdim ; j < SDIM  ; j++ )
     for ( a = 0 ; a < web.dimension ; a++ )
        for ( b = 0 ; b < web.dimension ; b++ )
          for ( m = 0 ; m < web.dimension ; m++ )
             for ( n = 0 ; n < web.dimension ; n++ )
             { REAL sum = 0.0;
                for ( c = 0 ; c < web.dimension ; c++ )
                  for ( d = 0 ; d < web.dimension ; d++ )
                     sum += - dQdv[a][c][m][i]*dSdv[c][d][n][j]*Q[d][b]
                              - ( i==j ? Q[a][c]*ddSdv[c][d][m][n]*Q[d][b] : 0.0 )
                              - Q[a][c]*dSdv[c][d][m][i]*dQdv[d][b][n][j];
                ddQdv[a][b][m][n][i][j] = sum; 
             }

  /* calculate ddCdv */
  for ( i = 0 ; i < SDIM  ; i++ )
    for ( j = 0 ; j < SDIM  ; j++ )
     for ( a = 0 ; a < web.dimension ; a++ )
        for ( b = 0 ; b < web.dimension ; b++ )
        { 
          for ( m = 0 ; m < web.dimension ; m++ )
             for ( n = 0 ; n < web.dimension ; n++ )
             { REAL sum = 0.0;
                for ( c = 0 ; c < web.dimension ; c++ )
                  sum += ( (i==j && i < sdim) ? ddFdv[a][c][m][n]*Q[c][b] : 0.0 )
                         + ( (i < sdim && j >= sdim) ? dFdv[a][c][m][i]*dQdv[c][b][n][j] : 0.0)
                         + ( (j < sdim && i >= sdim) ? dFdv[a][c][n][j]*dQdv[c][b][m][i] : 0.0)
                         + ( (i >= sdim && j >= sdim) ? F[a][c]*ddQdv[c][b][m][n][i][j] : 0.0);
                ddCdv[a][b][m][n][i][j] = sum;
             }
  }
  /* calculate ddstuff */
    for ( i = 0 ; i < SDIM  ; i++ )
     for ( j = 0 ; j < SDIM  ; j++ )
      for ( a = 0 ; a < web.dimension ; a++ )
        for ( b = 0 ; b < web.dimension ; b++ )
         ddstuff[a][b][i][j] = 0.0;
  if ( eps2 + poisson*eps1 <= 0.0 )
  { /* relaxed in one dimension */
    if ( part & ONE_STRESS )
    for ( i = 0 ; i < SDIM  ; i++ )
     for ( j = 0 ; j < SDIM  ; j++ )
      for ( a = 0 ; a < web.dimension ; a++ )
        for ( b = 0 ; b < web.dimension ; b++ )
     { REAL deps1a,deps1b,disc,ddisca,ddiscb,dddisc,ddeps;
       REAL dTrCa,dTrCb,dDetCa,dDetCb;


    dTrCa = dCdv[0][0][a][i]+dCdv[1][1][a][i];
    dTrCb = dCdv[0][0][b][j]+dCdv[1][1][b][j];
    dDetCa = c11*dCdv[1][1][a][i] + dCdv[0][0][a][i]*c22
                 - c12*dCdv[1][0][a][i] - c21*dCdv[0][1][a][i];
    dDetCb = c11*dCdv[1][1][b][j] + dCdv[0][0][b][j]*c22
                 - c12*dCdv[1][0][b][j] - c21*dCdv[0][1][b][j];

    disc = TrC*TrC - 4*DetC;

    ddisca = 2*TrC*dTrCa - 4*dDetCa;
    ddiscb = 2*TrC*dTrCb - 4*dDetCb;

    deps1a = (dTrCa + 0.5/sqrt(disc)*ddisca)/2;
    deps1b = (dTrCb + 0.5/sqrt(disc)*ddiscb)/2;

    dddisc = 2*dTrCa*dTrCb +
           + 2*TrC*(ddCdv[0][0][a][b][i][j]+ddCdv[1][1][a][b][i][j])

           - 4*(dCdv[0][0][b][j]*dCdv[1][1][a][i] + dCdv[0][0][a][i]*dCdv[1][1][b][j]
                 - dCdv[0][1][b][j]*dCdv[1][0][a][i] - dCdv[1][0][b][j]*dCdv[0][1][a][i])

           - 4*(c11*ddCdv[1][1][a][b][i][j] + ddCdv[0][0][a][b][i][j]*c22
                 - c12*ddCdv[1][0][a][b][i][j] - c21*ddCdv[0][1][a][b][i][j]);

    ddeps = 0.5*(ddCdv[0][0][a][b][i][j]+ddCdv[1][1][a][b][i][j]
              -.25/sqrt(disc)/disc*ddisca*ddiscb + 0.5/sqrt(disc)*dddisc);

            ddstuff[a][b][i][j] = (deps1a*deps1b + eps1*ddeps)/4; 
   

     }
  }
  else
  { 
    if ( part & TWO_STRESS )
    for ( i = 0 ; i < SDIM  ; i++ )
     for ( j = 0 ; j < SDIM  ; j++ )
      for ( a = 0 ; a < web.dimension ; a++ )
        for ( b = 0 ; b < web.dimension ; b++ )
        { REAL term; 
          term = (coeff1*( 2*c11*ddCdv[0][0][a][b][i][j]
                         + 2*c12*ddCdv[1][0][a][b][i][j]
                         + 2*c21*ddCdv[0][1][a][b][i][j]
                         + 2*c22*ddCdv[1][1][a][b][i][j]) 
                 + 2*coeff2*(c11+c22)*(ddCdv[0][0][a][b][i][j]+ddCdv[1][1][a][b][i][j]))

                 + (coeff1*(2*dCdv[0][0][a][i]*dCdv[0][0][b][j] 
                          + 2*dCdv[0][1][a][i]*dCdv[1][0][b][j]
                          + 2*dCdv[1][0][a][i]*dCdv[0][1][b][j]
                          + 2*dCdv[1][1][a][i]*dCdv[1][1][b][j]
                         ) 
                  + 2*coeff2*(dCdv[0][0][a][i]+dCdv[1][1][a][i])
                                     *(dCdv[0][0][b][j] +dCdv[1][1][b][j]));
            ddstuff[a][b][i][j] = term;
         }
  }

  /* area hessian */
  for ( i = sdim ; i < SDIM  ; i++ )
    for ( j = sdim ; j < SDIM  ; j++ )
     for ( a = 0 ; a < web.dimension ; a++ )
        for ( b = 0 ; b < web.dimension ; b++ )
          ddadv[a][b][i][j] = 0.0;
  ddadv[0][1][sdim][sdim+1] = areasign;
  ddadv[1][0][sdim+1][sdim] = areasign;
  ddadv[0][1][sdim+1][sdim] = -areasign;
  ddadv[1][0][sdim][sdim+1] = -areasign;


  /* big Hessian finale */
  for ( i = 0 ; i < SDIM  ; i++ )
    for ( j = 0 ; j < SDIM  ; j++ )
     for ( a = 0 ; a < web.dimension ; a++ )
        for ( b = 0 ; b < web.dimension ; b++ )
        { REAL term;
            term =
                 ( (i >= sdim && j >= sdim) ? ddadv[a][b][i][j]*stuff : 0.0)
              + dadv[a][i]*dstuff[b][j]
              + dadv[b][j]*dstuff[a][i]
              + area*ddstuff[a][b][i][j];
          term *= weight;
          f_info->hess[a+1][b+1][i][j] = term;
          f_info->hess[a+1][0][i][j] -= term;
          f_info->hess[0][b+1][i][j] -= term;
          f_info->hess[0][0][i][j] += term;
        }
  /* combo correction */
  for ( i = 0 ; i < SDIM  ; i++ ) 
   for ( j = 0 ; j < SDIM  ; j++ ) 
   { REAL term;
     term = combo*(combo*f_info->hess[2][2][i][j]-f_info->hess[1][2][i][j]
                          -f_info->hess[2][1][i][j]);
     f_info->hess[1][1][i][j] += term;
     f_info->hess[0][1][i][j] -= term;
     f_info->hess[1][0][i][j] -= term;
     f_info->hess[0][0][i][j] += term;
     term = combo*f_info->hess[2][2][i][j];
     f_info->hess[1][2][i][j] -= term;
     f_info->hess[0][2][i][j] += term;
     f_info->hess[1][0][i][j] += term;
     f_info->hess[0][0][i][j] -= term;
     term = combo*f_info->hess[2][2][i][j];
     f_info->hess[2][1][i][j] -= term;
     f_info->hess[0][1][i][j] += term;
     f_info->hess[2][0][i][j] += term;
     f_info->hess[0][0][i][j] -= term;
   }
  return energy;
}

/**************************************************************
*
*  function: relaxed_elastic_energy()
*
*  purpose: calculates energy of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL relaxed_elastic_energy(f_info)
struct qinfo *f_info;
{
 return relaxed_elastic_all(f_info,METHOD_VALUE,ONE_STRESS|TWO_STRESS);
}



/**************************************************************
*
*  function: relaxed_elastic_gradient()
*
*  purpose: calculates gradient of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL relaxed_elastic_gradient(f_info)
struct qinfo *f_info;
{
 return relaxed_elastic_all(f_info,METHOD_GRADIENT,ONE_STRESS|TWO_STRESS);
}


/**************************************************************
*
*  function: relaxed_elastic_hessian()
*
*  purpose: calculates hessian of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL relaxed_elastic_hessian(f_info)
struct qinfo *f_info;
{
 return relaxed_elastic_all(f_info,METHOD_HESSIAN,ONE_STRESS|TWO_STRESS);
}

/* Partial relaxed energy methods */

REAL relaxed_elastic1_energy(f_info)
struct qinfo *f_info;
{ return relaxed_elastic_all(f_info,METHOD_VALUE,ONE_STRESS); }

REAL relaxed_elastic1_gradient(f_info)
struct qinfo *f_info;
{ return relaxed_elastic_all(f_info,METHOD_GRADIENT,ONE_STRESS); }

REAL relaxed_elastic1_hessian(f_info)
struct qinfo *f_info;
{ return relaxed_elastic_all(f_info,METHOD_HESSIAN,ONE_STRESS); }

REAL relaxed_elastic2_energy(f_info)
struct qinfo *f_info;
{ return relaxed_elastic_all(f_info,METHOD_VALUE,TWO_STRESS); }

REAL relaxed_elastic2_gradient(f_info)
struct qinfo *f_info;
{ return relaxed_elastic_all(f_info,METHOD_GRADIENT,TWO_STRESS); }

REAL relaxed_elastic2_hessian(f_info)
struct qinfo *f_info;
{ return relaxed_elastic_all(f_info,METHOD_HESSIAN,TWO_STRESS); }

/************************************************************************
*************************************************************************

     Named method: relaxed_elastic_A

     Linear elastic strain energy on facets, with fixed reference
     coordinates manifested as form_factors for each facet.
     This allows more flexibility in reference coordinates assigned
     to vertices that are taped together.
     Revised from linear_elastic to implement Bill Collier's
     relaxed energy model.  Basically, stress is set to 0 when stress
     would be negative, to model wrinkling.
     (for Frank Baginski's NASA balloon models)

     Let S be Gram matrix of unstrained facet (form_factors, dots of sides).
     Let Q be the inverse of S.
     Let F be Gram matrix of strained facet.
     Let C = (FQ-I)/2, the Cauchy-Green strain tensor.
     Let v be Poisson ratio.
     Let eps1 and eps2 be the eigenvalues of C.
     Let sigma1 = eps1+v*eps2 and sigma2 = eps2+v*eps1 be the stress
       eigenvalues.
     Then energy density is 
        (1/2/(1+v))(Tr(C^2) + v*(Tr C)^2/(1-(dim-1)*v))    sigma1,sigma2 >= 0
        eps1^2/2    sigma2 < 0, eps1 > 0
        eps2^2/2    sigma1 < 0, eps2 > 0
        0           eps2 < 0, eps1 < 0.

     Each facet has extra attribute poisson_ratio for v.

************************************************************************/

/************************************************************************
*
*  function: relaxed_elastic_A_init()
*
*  purpose: Make sure needed extra attributes are present.
*              
*
*/

void relaxed_elastic_A_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{ 

  if ( web.modeltype != LINEAR )
     kb_error(2163,"relaxed_elastic_A method only for LINEAR model.\n",RECOVERABLE);

  if ( web.dimension != 2 )
     kb_error(2164,"relaxed_elastic_A method only for SOAPFILM model.\n",RECOVERABLE);

  /* extra facet attribute for poisson_ratio */
  poisson_attr = find_attribute(FACET,POISSON_NAME);
  if ( poisson_attr < 0 ) /* not found */
     kb_error(2165,"Facet extra attribute poisson_ratio missing. Needed by relaxed_elastic_A.\n",RECOVERABLE);

  form_factors_attr = find_attribute(FACET,FORM_FACTORS_NAME);
  if ( form_factors_attr < 0 ) /* not found */
     kb_error(2166,"Facet extra attribute form_factors real[3] missing. Needed by relaxed_elastic_A.\n",RECOVERABLE);

  if ( EXTRAS(FACET)[form_factors_attr].array_spec.datacount != 3 )
     kb_error(2167,"Facet extra attribute form_factors must have size 3.\n",
        RECOVERABLE);

  LEBweight_attr = find_attribute(FACET,LEBWEIGHT_NAME); /* optional */

}

/************************************************************************
*
* function: relaxed_elastic_A_all()
*
* purpose: energy, gradient, and hessian for relaxed_elastic_A_method.
*/

REAL relaxed_elastic_A_all ARGS((struct qinfo *,int,int));
#define ONE_STRESS 1
#define TWO_STRESS 2

REAL relaxed_elastic_A_all(f_info,mode,part)
struct qinfo *f_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */
int part; /* flag for one stress, two stress, or both parts */
{
  REAL q11,q12,q22;  /* Q entries */
  REAL Q[2][2];
  REAL det;    /* det S */
  REAL area;  /* reference area of facet */
  REAL poisson; /* poisson ratio */
  REAL f11,f12,f22;
  REAL c11,c12,c21,c22;
  REAL energy;
  REAL stuff;
  REAL dstuff[FACET_VERTS][MAXCOORD];
  REAL dCdv[2][2][FACET_VERTS][MAXCOORD];
  REAL dFdv[2][2][FACET_VERTS][MAXCOORD];
  REAL ddFdv[2][2][2][2];
  REAL ddCdv[2][2][FACET_VERTS][FACET_VERTS][MAXCOORD][MAXCOORD];
  REAL ddstuff[FACET_VERTS][FACET_VERTS][MAXCOORD][MAXCOORD];
  int i,j,m,n,a,b,c;
  REAL coeff1,coeff2;
  REAL *s;  /* form factors */
  int sdim = SDIM; /* true space dimension */
  REAL weight;  /* facet weighting factor */
  REAL **side;  /* sides of facet */
  REAL eps1,eps2; /* strain eigenvalues */
  REAL TrC,DetC;  /* trace and determinant of C */

  poisson = *(REAL*)get_extra(f_info->id,poisson_attr);
  if ( LEBweight_attr >= 0 )
    weight = *(REAL*)get_extra(f_info->id,LEBweight_attr);
  else weight = 1.0;

  s = (REAL*)get_extra(f_info->id,form_factors_attr);
  det = s[0]*s[2] - s[1]*s[1];

  if ( det <= 0.0 )
  { if ( mode == METHOD_VALUE ) return 0.0;
    sprintf(errmsg,"relaxed_elastic_A: Facet %s has unstrained area 0.\n",
        ELNAME(f_info->id));
    kb_error(2168,errmsg,RECOVERABLE);
  }
  area = sqrt(det)/2;
  coeff1 = 1.0/8/(1 + poisson);
  coeff2 = coeff1*poisson/(1 - (web.dimension-1)*poisson);
  Q[0][0] = q11 = s[2]/det; 
  Q[0][1] = Q[1][0] = q12 = -s[1]/det; 
  Q[1][1] = q22 = s[0]/det;

  side = f_info->sides[0];
  f11 = dot(side[0],side[0],sdim);
  f12 = dot(side[0],side[1],sdim);
  f22 = dot(side[1],side[1],sdim);

  c11 = f11*q11 + f12*q12 - 1;
  c12 = f11*q12 + f12*q22;
  c21 = f12*q11 + f22*q12;
  c22 = f12*q12 + f22*q22 - 1;

  TrC = c11 + c22; 
  DetC = c11*c22 - c12*c21;
  eps1 = (TrC + sqrt(TrC*TrC - 4*DetC))/2;
  eps2 = (TrC - sqrt(TrC*TrC - 4*DetC))/2;
 
  if ( eps1 <= 0.0 ) return 0.0;  /* relaxed in both directions */
  stuff = 0.0;
  if ( eps2 + poisson*eps1 <= 0.0 )
  { /* relaxed in one dimension */
    if ( part & ONE_STRESS )
      stuff = eps1*eps1/2/4;  /* 1/4 fudge factor to agree with other */
  }
  else
  { /* stressed in both directions */
    if ( part & TWO_STRESS )
      stuff = coeff1*(c11*c11+c12*c21+c12*c21+c22*c22)
                 + coeff2*(c11+c22)*(c11+c22);
  }
  energy = weight*area*stuff;

  if ( mode == METHOD_VALUE ) return energy;

  /* gradient */

  for ( i = 0 ; i < sdim  ; i++ )  /* with respect to space coord */
  {  
    for ( a = 0 ; a < web.dimension ; a++ )
      for ( b = 0 ; b < web.dimension ; b++ )
        for ( m = 0 ; m < web.dimension ; m++ )
          dFdv[a][b][m][i] = (m==a ? side[b][i] : 0.0)
                           + (m==b ? side[a][i] : 0.0);
  }

  for ( i = 0 ; i < sdim  ; i++ )  /* with respect to space coord */
  {
     for ( a = 0 ; a < web.dimension ; a++ )
        for ( b = 0 ; b < web.dimension ; b++ )
          for ( m = 0 ; m < web.dimension ; m++ )
          { REAL sum = 0.0;
             for ( c = 0 ; c < web.dimension ; c++ )
                sum += dFdv[a][c][m][i]*Q[c][b];
             dCdv[a][b][m][i] = sum;
          }
  }

  for ( j = 0 ; j < web.dimension  ; j++ )
    for ( i = 0 ; i < SDIM  ; i++ ) dstuff[j][i] = 0.0;
  if ( eps2 + poisson*eps1 <= 0.0 )
  { /* relaxed in one dimension */
    if ( part & ONE_STRESS )
    for ( j = 0 ; j < web.dimension  ; j++ )
     for ( i = 0 ; i < SDIM  ; i++ )
     { dstuff[j][i] = eps1*0.5/4*(dCdv[0][0][j][i]+dCdv[1][1][j][i]
        + 0.5/sqrt(TrC*TrC - 4*DetC)*(2*TrC*(dCdv[0][0][j][i]+dCdv[1][1][j][i])
           - 4*(c11*dCdv[1][1][j][i] + dCdv[0][0][j][i]*c22
                 - c12*dCdv[1][0][j][i] - c21*dCdv[0][1][j][i])));
     }
  }
  else /* stressed in both directions */
  { 
    if ( part & TWO_STRESS )
    for ( j = 0 ; j < web.dimension  ; j++ )
     for ( i = 0 ; i < SDIM  ; i++ )
     { dstuff[j][i] = (coeff1*(2*c11*dCdv[0][0][j][i]  
                                 + 2*c12*dCdv[1][0][j][i]
                                 + 2*c21*dCdv[0][1][j][i]
                                 + 2*c22*dCdv[1][1][j][i]) 
                  + 2*coeff2*(c11+c22)*(dCdv[0][0][j][i] +dCdv[1][1][j][i])); 
     }
  }

  /* grand finale for gradient */
  for ( a = 0 ; a < web.dimension ; a++ )
     for ( i = 0 ; i < SDIM  ; i++ ) 
     { f_info->grad[a+1][i] = weight*area*dstuff[a][i];
       f_info->grad[0][i] -= weight*area*dstuff[a][i];
     }
 
  if ( mode == METHOD_GRADIENT ) return energy;

  /* hessian */

  /* calculate ddFdv */ 
  for ( a = 0 ; a < web.dimension ; a++ )
    for ( b = 0 ; b < web.dimension ; b++ )
      for ( m = 0 ; m < web.dimension ; m++ )
        for ( n = 0 ; n < web.dimension ; n++ )
        { ddFdv[a][b][m][n] =
                 (a==m && b==n ? 1.0 : 0.0) + (a==n && b==m ? 1.0 : 0.0);
        }

  /* calculate ddCdv */
  for ( i = 0 ; i < SDIM  ; i++ )
    for ( j = 0 ; j < SDIM  ; j++ )
     for ( a = 0 ; a < web.dimension ; a++ )
        for ( b = 0 ; b < web.dimension ; b++ )
        { 
          for ( m = 0 ; m < web.dimension ; m++ )
             for ( n = 0 ; n < web.dimension ; n++ )
             { REAL sum = 0.0;
                for ( c = 0 ; c < web.dimension ; c++ )
                  sum += ( i==j ? ddFdv[a][c][m][n]*Q[c][b] : 0.0 );
                ddCdv[a][b][m][n][i][j] = sum;
             }
  }
  /* calculate ddstuff */

  for ( i = 0 ; i < SDIM  ; i++ )
   for ( j = 0 ; j < SDIM  ; j++ )
    for ( a = 0 ; a < web.dimension ; a++ )
      for ( b = 0 ; b < web.dimension ; b++ )
       ddstuff[a][b][i][j] = 0.0;

  if ( eps2 + poisson*eps1 <= 0.0 )
  { /* relaxed in one dimension */
    if ( part & ONE_STRESS )
    for ( i = 0 ; i < SDIM  ; i++ )
     for ( j = 0 ; j < SDIM  ; j++ )
      for ( a = 0 ; a < web.dimension ; a++ )
        for ( b = 0 ; b < web.dimension ; b++ )
        { REAL deps1a,deps1b,disc,ddisca,ddiscb,dddisc,ddeps;
          REAL dTrCa,dTrCb,dDetCa,dDetCb;


          dTrCa = dCdv[0][0][a][i]+dCdv[1][1][a][i];
          dTrCb = dCdv[0][0][b][j]+dCdv[1][1][b][j];
          dDetCa = c11*dCdv[1][1][a][i] + dCdv[0][0][a][i]*c22
                       - c12*dCdv[1][0][a][i] - c21*dCdv[0][1][a][i];
          dDetCb = c11*dCdv[1][1][b][j] + dCdv[0][0][b][j]*c22
                       - c12*dCdv[1][0][b][j] - c21*dCdv[0][1][b][j];

          disc = TrC*TrC - 4*DetC;
      
          ddisca = 2*TrC*dTrCa - 4*dDetCa;
          ddiscb = 2*TrC*dTrCb - 4*dDetCb;

          deps1a = (dTrCa + 0.5/sqrt(disc)*ddisca)/2;
          deps1b = (dTrCb + 0.5/sqrt(disc)*ddiscb)/2;

          dddisc = 2*dTrCa*dTrCb +
                 + 2*TrC*(ddCdv[0][0][a][b][i][j]+ddCdv[1][1][a][b][i][j])

                 - 4*(dCdv[0][0][b][j]*dCdv[1][1][a][i] + dCdv[0][0][a][i]*dCdv[1][1][b][j]
                 - dCdv[0][1][b][j]*dCdv[1][0][a][i] - dCdv[1][0][b][j]*dCdv[0][1][a][i])

                 - 4*(c11*ddCdv[1][1][a][b][i][j] + ddCdv[0][0][a][b][i][j]*c22
                 - c12*ddCdv[1][0][a][b][i][j] - c21*ddCdv[0][1][a][b][i][j]);

          ddeps = 0.5*(ddCdv[0][0][a][b][i][j]+ddCdv[1][1][a][b][i][j]
                    -.25/sqrt(disc)/disc*ddisca*ddiscb + 0.5/sqrt(disc)*dddisc);

          ddstuff[a][b][i][j] = (deps1a*deps1b + eps1*ddeps)/4; 
   

        }
  }
  else
  { 
    if ( part & TWO_STRESS )
    for ( i = 0 ; i < SDIM  ; i++ )
     for ( j = 0 ; j < SDIM  ; j++ )
      for ( a = 0 ; a < web.dimension ; a++ )
        for ( b = 0 ; b < web.dimension ; b++ )
        { REAL term; 
          term = (coeff1*( 2*c11*ddCdv[0][0][a][b][i][j]
                         + 2*c12*ddCdv[1][0][a][b][i][j]
                         + 2*c21*ddCdv[0][1][a][b][i][j]
                         + 2*c22*ddCdv[1][1][a][b][i][j]) 
                 + 2*coeff2*(c11+c22)*(ddCdv[0][0][a][b][i][j]+ddCdv[1][1][a][b][i][j]))

                 + (coeff1*(2*dCdv[0][0][a][i]*dCdv[0][0][b][j] 
                          + 2*dCdv[0][1][a][i]*dCdv[1][0][b][j]
                          + 2*dCdv[1][0][a][i]*dCdv[0][1][b][j]
                          + 2*dCdv[1][1][a][i]*dCdv[1][1][b][j]
                         ) 
                  + 2*coeff2*(dCdv[0][0][a][i]+dCdv[1][1][a][i])
                                     *(dCdv[0][0][b][j] +dCdv[1][1][b][j]));
            ddstuff[a][b][i][j] = term;
         }
  }

  /* big Hessian finale */
  for ( i = 0 ; i < SDIM  ; i++ )
    for ( j = 0 ; j < SDIM  ; j++ )
     for ( a = 0 ; a < web.dimension ; a++ )
        for ( b = 0 ; b < web.dimension ; b++ )
        { REAL term;
          term = area*ddstuff[a][b][i][j];
          term *= weight;
          f_info->hess[a+1][b+1][i][j] = term;
          f_info->hess[a+1][0][i][j] -= term;
          f_info->hess[0][b+1][i][j] -= term;
          f_info->hess[0][0][i][j] += term;
        }
  return energy;
}

/**************************************************************
*
*  function: relaxed_elastic_A_energy()
*
*  purpose: calculates energy of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL relaxed_elastic_A_energy(f_info)
struct qinfo *f_info;
{
 return relaxed_elastic_A_all(f_info,METHOD_VALUE,ONE_STRESS|TWO_STRESS);
}



/**************************************************************
*
*  function: relaxed_elastic_A_gradient()
*
*  purpose: calculates gradient of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL relaxed_elastic_A_gradient(f_info)
struct qinfo *f_info;
{
 return relaxed_elastic_A_all(f_info,METHOD_GRADIENT,ONE_STRESS|TWO_STRESS);
}


/**************************************************************
*
*  function: relaxed_elastic_A_hessian()
*
*  purpose: calculates hessian of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL relaxed_elastic_A_hessian(f_info)
struct qinfo *f_info;
{
 return relaxed_elastic_A_all(f_info,METHOD_HESSIAN,ONE_STRESS|TWO_STRESS);
}

/* Partial relaxed energy methods */

REAL relaxed_elastic1_A_energy(f_info)
struct qinfo *f_info;
{ return relaxed_elastic_A_all(f_info,METHOD_VALUE,ONE_STRESS); }

REAL relaxed_elastic1_A_gradient(f_info)
struct qinfo *f_info;
{ return relaxed_elastic_A_all(f_info,METHOD_GRADIENT,ONE_STRESS); }

REAL relaxed_elastic1_A_hessian(f_info)
struct qinfo *f_info;
{ return relaxed_elastic_A_all(f_info,METHOD_HESSIAN,ONE_STRESS); }

REAL relaxed_elastic2_A_energy(f_info)
struct qinfo *f_info;
{ return relaxed_elastic_A_all(f_info,METHOD_VALUE,TWO_STRESS); }

REAL relaxed_elastic2_A_gradient(f_info)
struct qinfo *f_info;
{ return relaxed_elastic_A_all(f_info,METHOD_GRADIENT,TWO_STRESS); }

REAL relaxed_elastic2_A_hessian(f_info)
struct qinfo *f_info;
{ return relaxed_elastic_A_all(f_info,METHOD_HESSIAN,TWO_STRESS); }

/***************************************************************************
****************************************************************************

                         Named Method dihedral_hooke

Energy is edge length times square of angle between normals of adjacent
facets.  Actually, e = (1 - cos(angle))*length.

***************************************************************************/

/********************************************************************
*
*  Function: dihedral_hooke_energy()
*
*  Purpose:  Does energy calculation for an edge.
*
*/

REAL dihedral_hooke_energy(e_info)
struct qinfo *e_info;
{ 
  REAL s1[MAXCOORD],s2[MAXCOORD],t2[MAXCOORD];
  REAL s1s1,s1s2,s1t2,s2s2,t2t2,s2t2;
  REAL a1,a2;
  REAL det;
  facetedge_id fe_s1,fe_s2,fe_t2;
  edge_id e_id = e_info->id;
  REAL cos_th;    /* cosine of angle between facets */

  /* get edge vectors away from tail vertex */
  fe_s1 = get_edge_fe(e_id);
  fe_s2 = get_prev_edge(get_next_facet(fe_s1));
  fe_s2 = inverse_id(fe_s2);
  fe_t2 = get_prev_edge(fe_s1);
  fe_t2 = inverse_id(fe_t2);
  get_fe_side(fe_s1,s1);
  get_fe_side(fe_s2,s2);
  get_fe_side(fe_t2,t2);

  s1s1 = SDIM_dot(s1,s1);
  s1s2 = SDIM_dot(s1,s2);
  s1t2 = SDIM_dot(s1,t2);
  t2t2 = SDIM_dot(t2,t2);
  s2s2 = SDIM_dot(s2,s2);
  s2t2 = SDIM_dot(s2,t2);

  det = s1s1*t2t2 - s1t2*s1t2;
  a1 = sqrt(det);
  det = s1s1*s2s2 - s1s2*s1s2;
  a2 = sqrt(det);

  cos_th = (s1s2*s1t2 - s2t2*s1s1)/a1/a2;

  return sqrt(s1s1)*(1 - cos_th);
}


/************************************************************************
*
*  Function: dihedral_hooke_grad()
*
*  Purpose:  Does gradient calculation.
*
*/

REAL dihedral_hooke_grad(e_info)
struct qinfo *e_info;
{ 
  REAL s1[MAXCOORD],s2[MAXCOORD],t2[MAXCOORD];
  REAL s1s1,s1s2,s1t2,s2s2,t2t2,s2t2;
  REAL a1,a2;
  REAL det;
  facetedge_id fe_s1,fe_s2,fe_t2;
  edge_id e_id = e_info->id;
  REAL cos_th;    /* cosine of angle between facets */
  REAL dcosds1[MAXCOORD],dcosds2[MAXCOORD],dcosdt2[MAXCOORD];
  REAL da1ds1[MAXCOORD], da1dt2[MAXCOORD];
  REAL da2ds1[MAXCOORD], da2ds2[MAXCOORD];
  int i;

  /* get edge vectors away from tail vertex */
  fe_s1 = get_edge_fe(e_id);
  fe_s2 = get_prev_edge(get_next_facet(fe_s1));
  fe_s2 = inverse_id(fe_s2);
  fe_t2 = get_prev_edge(fe_s1);
  fe_t2 = inverse_id(fe_t2);
  get_fe_side(fe_s1,s1);
  get_fe_side(fe_s2,s2);
  get_fe_side(fe_t2,t2);

  s1s1 = SDIM_dot(s1,s1);
  s1s2 = SDIM_dot(s1,s2);
  s1t2 = SDIM_dot(s1,t2);
  t2t2 = SDIM_dot(t2,t2);
  s2s2 = SDIM_dot(s2,s2);
  s2t2 = SDIM_dot(s2,t2);

  det = s1s1*t2t2 - s1t2*s1t2;
  a1 = sqrt(det);
  det = s1s1*s2s2 - s1s2*s1s2;
  a2 = sqrt(det);

  cos_th = (s1s2*s1t2 - s2t2*s1s1)/a1/a2;

  /* gradients of various terms */
  for ( i = 0 ; i < SDIM ; i++ )
  { da1ds1[i] = 0.5/a1*(2*s1[i]*t2t2 - 2*s1t2*t2[i]);
    da1dt2[i] = 0.5/a1*(2*s1s1*t2[i] - 2*s1t2*s1[i]);
    da2ds1[i] = 0.5/a2*(2*s1[i]*s2s2 - 2*s1s2*s2[i]);
    da2ds2[i] = 0.5/a2*(2*s1s1*s2[i] - 2*s1s2*s1[i]);
    dcosds1[i] = (s2[i]*s1t2 + s1s2*t2[i] - 2*s2t2*s1[i])/a1/a2
                    - cos_th/a1*da1ds1[i] - cos_th/a2*da2ds1[i];
    dcosdt2[i] = (s1s2*s1[i] - s2[i]*s1s1)/a1/a2
                    - cos_th/a1*da1dt2[i];
    dcosds2[i] = (s1[i]*s1t2 - t2[i]*s1s1)/a1/a2
                    - cos_th/a2*da2ds2[i];
  }

  for ( i = 0 ; i < SDIM ; i++ )
  { REAL f;  /* part of force */

    f = s1[i]*(1 - cos_th)/sqrt(s1s1);
    e_info->grad[1][i] += f;
    e_info->grad[0][i] -= f;

    f = -sqrt(s1s1)*dcosds1[i];
    e_info->grad[1][i] += f;
    e_info->grad[0][i] -= f;

    f = -sqrt(s1s1)*dcosds2[i];
    e_info->grad[3][i] += f;
    e_info->grad[0][i] -= f;

    f = -sqrt(s1s1)*dcosdt2[i];
    e_info->grad[2][i] += f;
    e_info->grad[0][i] -= f;
  }
  return sqrt(s1s1)*(1 - cos_th);
}


/************************************************************************
*
*  Function: dihedral_hooke_hess()
*
*  Purpose:  Does gradient and hessian calculation.
*
*/

static REAL Gtemp[6];
static REAL Ltemp[6][6];
static REAL Htemp[6][6];
static int  Lcount;
void gradList ARGS((REAL,REAL,REAL,REAL,REAL,REAL));
void LList ARGS((REAL*,REAL*,REAL*,REAL*,REAL*,REAL*));
REAL *List ARGS((REAL,REAL,REAL,REAL,REAL,REAL));

/* utility functions for deciphering Mathematica output */
void gradList(a,b,c,d,e,f)
REAL a,b,c,d,e,f;
{ Gtemp[0] = a;
  Gtemp[1] = b;
  Gtemp[2] = c;
  Gtemp[3] = d;
  Gtemp[4] = e;
  Gtemp[5] = f;
}
void LList(a,b,c,d,e,f)
REAL *a,*b,*c,*d,*e,*f;
{ int i;
  for ( i = 0 ; i < 6 ; i++ ) 
  { Htemp[0][i] = a[i];
     Htemp[1][i] = b[i];
     Htemp[2][i] = c[i];
     Htemp[3][i] = d[i];
     Htemp[4][i] = e[i];
     Htemp[5][i] = f[i];
  }
}
REAL * List(a,b,c,d,e,f)
REAL a,b,c,d,e,f;
{ Ltemp[Lcount][0] = a;
  Ltemp[Lcount][1] = b;
  Ltemp[Lcount][2] = c;
  Ltemp[Lcount][3] = d;
  Ltemp[Lcount][4] = e;
  Ltemp[Lcount][5] = f;
  return Ltemp[Lcount++];
}

/* This has a really long expression in it, which some
    compilers may not be able to handle.  If so, just
    use gcc instead of cc, or #ifdef out the bulk of the function, 
    since you're probably not going to use it anyway.
    */
REAL dihedral_hooke_hess(e_info)
struct qinfo *e_info;
{  
#ifndef MAC_CW
  int i,j,k,n,jj,nn;
  REAL s1[MAXCOORD],s2[MAXCOORD],t2[MAXCOORD];
  REAL s1s1,s1s2,s1t2,s2s2,t2t2,s2t2;
  REAL a1,a2;
  REAL det;
  facetedge_id fe_s1,fe_s2,fe_t2;
  edge_id e_id = e_info->id;
  REAL cos_th;    /* cosine of angle between facets */
  REAL dcosds1[MAXCOORD],dcosds2[MAXCOORD],dcosdt2[MAXCOORD];
  REAL da1ds1[MAXCOORD], da1dt2[MAXCOORD];
  REAL da2ds1[MAXCOORD], da2ds2[MAXCOORD];
  REAL dqdx[6][3][MAXCOORD];
  REAL ddqdxx[6][3][3];

  /* get edge vectors away from tail vertex */
  fe_s1 = get_edge_fe(e_id);
  fe_s2 = get_prev_edge(get_next_facet(fe_s1));
  fe_s2 = inverse_id(fe_s2);
  fe_t2 = get_prev_edge(fe_s1);
  fe_t2 = inverse_id(fe_t2);
  get_fe_side(fe_s1,s1);
  get_fe_side(fe_s2,s2);
  get_fe_side(fe_t2,t2);

  s1s1 = SDIM_dot(s1,s1);
  s1s2 = SDIM_dot(s1,s2);
  s1t2 = SDIM_dot(s1,t2);
  t2t2 = SDIM_dot(t2,t2);
  s2s2 = SDIM_dot(s2,s2);
  s2t2 = SDIM_dot(s2,t2);

  det = s1s1*t2t2 - s1t2*s1t2;
  a1 = sqrt(det);
  det = s1s1*s2s2 - s1s2*s1s2;
  a2 = sqrt(det);

  cos_th = (s1s2*s1t2 - s2t2*s1s1)/a1/a2;

  /* gradients of various terms */
  for ( i = 0 ; i < SDIM ; i++ )
  { da1ds1[i] = 0.5/a1*(2*s1[i]*t2t2 - 2*s1t2*t2[i]);
    da1dt2[i] = 0.5/a1*(2*s1s1*t2[i] - 2*s1t2*s1[i]);
    da2ds1[i] = 0.5/a2*(2*s1[i]*s2s2 - 2*s1s2*s2[i]);
    da2ds2[i] = 0.5/a2*(2*s1s1*s2[i] - 2*s1s2*s1[i]);
    dcosds1[i] = (s2[i]*s1t2 + s1s2*t2[i] - 2*s2t2*s1[i])/a1/a2
                    - cos_th/a1*da1ds1[i] - cos_th/a2*da2ds1[i];
    dcosdt2[i] = (s1s2*s1[i] - s2[i]*s1s1)/a1/a2
                    - cos_th/a1*da1dt2[i];
    dcosds2[i] = (s1[i]*s1t2 - t2[i]*s1s1)/a1/a2
                    - cos_th/a2*da2ds2[i];
  }

  /* gradients */
  for ( i = 0 ; i < SDIM ; i++ )
  { REAL f;  /* part of force */

    f = s1[i]*(1 - cos_th)/sqrt(s1s1);
    e_info->grad[1][i] += f;
    e_info->grad[0][i] -= f;

    f = -sqrt(s1s1)*dcosds1[i];
    e_info->grad[1][i] += f;
    e_info->grad[0][i] -= f;

    f = -sqrt(s1s1)*dcosds2[i];
    e_info->grad[3][i] += f;
    e_info->grad[0][i] -= f;

    f = -sqrt(s1s1)*dcosdt2[i];
    e_info->grad[2][i] += f;
    e_info->grad[0][i] -= f;
  }

  /* hessian */

  /* from Mathematica */
#define Sqrt(a) sqrt(a)
#define Power(a,b)  pow(a,(REAL)(b))

  gradList(Sqrt(s1s1)*(((s1s2*s1t2 - s1s1*s2t2)*t2t2)/
          (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
             Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) + 
         s2t2/(Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
             Sqrt(-Power(s1t2,2) + s1s1*t2t2)) + 
         (s2s2*(s1s2*s1t2 - s1s1*s2t2))/
          (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
             Sqrt(-Power(s1t2,2) + s1s1*t2t2))) + 
     (1 - (s1s2*s1t2 - s1s1*s2t2)/
          (Sqrt(-Power(s1s2,2) + s1s1*s2s2)*Sqrt(-Power(s1t2,2) + s1s1*t2t2)))/
      (2.*Sqrt(s1s1)),Sqrt(s1s1)*
     (-(s1t2/(Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2))) - 
        (s1s2*(s1s2*s1t2 - s1s1*s2t2))/
         (Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
            Sqrt(-Power(s1t2,2) + s1s1*t2t2))),
    Sqrt(s1s1)*(-((s1t2*(s1s2*s1t2 - s1s1*s2t2))/
            (Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
              Power(-Power(s1t2,2) + s1s1*t2t2,1.5))) - 
        s1s2/(Sqrt(-Power(s1s2,2) + s1s1*s2s2)*Sqrt(-Power(s1t2,2) + s1s1*t2t2))
        ),(Power(s1s1,1.5)*(s1s2*s1t2 - s1s1*s2t2))/
     (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
        Sqrt(-Power(s1t2,2) + s1s1*t2t2)),
    Power(s1s1,1.5)/
     (Sqrt(-Power(s1s2,2) + s1s1*s2s2)*Sqrt(-Power(s1t2,2) + s1s1*t2t2)),
    (Power(s1s1,1.5)*(s1s2*s1t2 - s1s1*s2t2))/
     (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
        Power(-Power(s1t2,2) + s1s1*t2t2,1.5)));

     Lcount = 0;
     LList(List(Sqrt(s1s1)*((-3*(s1s2*s1t2 - s1s1*s2t2)*Power(t2t2,2))/
            (4.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
              Power(-Power(s1t2,2) + s1s1*t2t2,2.5)) - 
          (s2t2*t2t2)/
            (Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
              Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) - 
          (s2s2*(s1s2*s1t2 - s1s1*s2t2)*t2t2)/
            (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
              Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) - 
          (s2s2*s2t2)/
            (Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2)) - 
          (3*Power(s2s2,2)*(s1s2*s1t2 - s1s1*s2t2))/
            (4.*Power(-Power(s1s2,2) + s1s1*s2s2,2.5)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2))) + 
      (((s1s2*s1t2 - s1s1*s2t2)*t2t2)/
            (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
              Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) + 
          s2t2/(Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2)) + 
          (s2s2*(s1s2*s1t2 - s1s1*s2t2))/
            (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2)))/Sqrt(s1s1) - 
      (1 - (s1s2*s1t2 - s1s1*s2t2)/
            (Sqrt(-Power(s1s2,2) + s1s1*s2s2)*Sqrt(-Power(s1t2,2) + s1s1*t2t2)))/
        (4.*Power(s1s1,1.5)),Sqrt(s1s1)*
        ((s1t2*t2t2)/
            (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
              Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) + 
          (s1s2*(s1s2*s1t2 - s1s1*s2t2)*t2t2)/
            (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
              Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) + 
          (s1t2*s2s2)/
            (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2)) + 
          (s1s2*s2t2)/
            (Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2)) + 
          (3*s1s2*s2s2*(s1s2*s1t2 - s1s1*s2t2))/
            (2.*Power(-Power(s1s2,2) + s1s1*s2s2,2.5)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2))) + 
      (-(s1t2/(Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
                 Sqrt(-Power(s1t2,2) + s1s1*t2t2))) - 
          (s1s2*(s1s2*s1t2 - s1s1*s2t2))/
            (Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2)))/(2.*Sqrt(s1s1)),
     Sqrt(s1s1)*((3*s1t2*(s1s2*s1t2 - s1s1*s2t2)*t2t2)/
            (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
              Power(-Power(s1t2,2) + s1s1*t2t2,2.5)) + 
          (s1t2*s2t2)/
            (Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
              Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) + 
          (s1t2*s2s2*(s1s2*s1t2 - s1s1*s2t2))/
            (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
              Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) + 
          (s1s2*t2t2)/
            (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
              Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) + 
          (s1s2*s2s2)/
            (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2))) + 
      (-((s1t2*(s1s2*s1t2 - s1s1*s2t2))/
              (Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
                 Power(-Power(s1t2,2) + s1s1*t2t2,1.5))) - 
          s1s2/(Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2)))/(2.*Sqrt(s1s1)),
     -(Power(s1s1,1.5)*(s1s2*s1t2 - s1s1*s2t2)*t2t2)/
        (4.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
          Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) - 
      (Power(s1s1,1.5)*s2t2)/
        (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
          Sqrt(-Power(s1t2,2) + s1s1*t2t2)) - 
      (3*Power(s1s1,1.5)*s2s2*(s1s2*s1t2 - s1s1*s2t2))/
        (4.*Power(-Power(s1s2,2) + s1s1*s2s2,2.5)*
          Sqrt(-Power(s1t2,2) + s1s1*t2t2)) + 
      (3*Sqrt(s1s1)*(s1s2*s1t2 - s1s1*s2t2))/
        (4.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
          Sqrt(-Power(s1t2,2) + s1s1*t2t2)),
     -(Power(s1s1,1.5)*t2t2)/
        (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
          Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) - 
      (Power(s1s1,1.5)*s2s2)/
        (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
          Sqrt(-Power(s1t2,2) + s1s1*t2t2)) + 
      (3*Sqrt(s1s1))/
        (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*Sqrt(-Power(s1t2,2) + s1s1*t2t2)),
     (-3*Power(s1s1,1.5)*(s1s2*s1t2 - s1s1*s2t2)*t2t2)/
        (4.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
          Power(-Power(s1t2,2) + s1s1*t2t2,2.5)) - 
      (Power(s1s1,1.5)*s2t2)/
        (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
          Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) - 
      (Power(s1s1,1.5)*s2s2*(s1s2*s1t2 - s1s1*s2t2))/
        (4.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
          Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) + 
      (3*Sqrt(s1s1)*(s1s2*s1t2 - s1s1*s2t2))/
        (4.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
          Power(-Power(s1t2,2) + s1s1*t2t2,1.5))),
    List(Sqrt(s1s1)*((s1t2*t2t2)/
            (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
              Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) + 
          (s1s2*(s1s2*s1t2 - s1s1*s2t2)*t2t2)/
            (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
              Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) + 
          (s1t2*s2s2)/
            (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2)) + 
          (s1s2*s2t2)/
            (Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2)) + 
          (3*s1s2*s2s2*(s1s2*s1t2 - s1s1*s2t2))/
            (2.*Power(-Power(s1s2,2) + s1s1*s2s2,2.5)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2))) + 
      (-(s1t2/(Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
                 Sqrt(-Power(s1t2,2) + s1s1*t2t2))) - 
          (s1s2*(s1s2*s1t2 - s1s1*s2t2))/
            (Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2)))/(2.*Sqrt(s1s1)),
     Sqrt(s1s1)*((-2*s1s2*s1t2)/
          (Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
             Sqrt(-Power(s1t2,2) + s1s1*t2t2)) - 
         (3*Power(s1s2,2)*(s1s2*s1t2 - s1s1*s2t2))/
          (Power(-Power(s1s2,2) + s1s1*s2s2,2.5)*
             Sqrt(-Power(s1t2,2) + s1s1*t2t2)) - 
         (s1s2*s1t2 - s1s1*s2t2)/
          (Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
             Sqrt(-Power(s1t2,2) + s1s1*t2t2))),
     Sqrt(s1s1)*(-(Power(s1t2,2)/
             (Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
                Power(-Power(s1t2,2) + s1s1*t2t2,1.5))) - 
         (s1s2*s1t2*(s1s2*s1t2 - s1s1*s2t2))/
          (Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
             Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) - 
         Power(s1s2,2)/
          (Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
             Sqrt(-Power(s1t2,2) + s1s1*t2t2)) - 
         1/(Sqrt(-Power(s1s2,2) + s1s1*s2s2)*Sqrt(-Power(s1t2,2) + s1s1*t2t2))),
     (Power(s1s1,1.5)*s1t2)/
        (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
          Sqrt(-Power(s1t2,2) + s1s1*t2t2)) + 
      (3*Power(s1s1,1.5)*s1s2*(s1s2*s1t2 - s1s1*s2t2))/
        (2.*Power(-Power(s1s2,2) + s1s1*s2s2,2.5)*
          Sqrt(-Power(s1t2,2) + s1s1*t2t2)),
     (Power(s1s1,1.5)*s1s2)/
      (Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*Sqrt(-Power(s1t2,2) + s1s1*t2t2)),
     (Power(s1s1,1.5)*s1t2)/
        (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
          Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) + 
      (Power(s1s1,1.5)*s1s2*(s1s2*s1t2 - s1s1*s2t2))/
        (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
          Power(-Power(s1t2,2) + s1s1*t2t2,1.5))),
    List(Sqrt(s1s1)*((3*s1t2*(s1s2*s1t2 - s1s1*s2t2)*t2t2)/
            (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
              Power(-Power(s1t2,2) + s1s1*t2t2,2.5)) + 
          (s1t2*s2t2)/
            (Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
              Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) + 
          (s1t2*s2s2*(s1s2*s1t2 - s1s1*s2t2))/
            (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
              Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) + 
          (s1s2*t2t2)/
            (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
              Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) + 
          (s1s2*s2s2)/
            (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2))) + 
      (-((s1t2*(s1s2*s1t2 - s1s1*s2t2))/
              (Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
                 Power(-Power(s1t2,2) + s1s1*t2t2,1.5))) - 
          s1s2/(Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2)))/(2.*Sqrt(s1s1)),
     Sqrt(s1s1)*(-(Power(s1t2,2)/
             (Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
                Power(-Power(s1t2,2) + s1s1*t2t2,1.5))) - 
         (s1s2*s1t2*(s1s2*s1t2 - s1s1*s2t2))/
          (Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
             Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) - 
         Power(s1s2,2)/
          (Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
             Sqrt(-Power(s1t2,2) + s1s1*t2t2)) - 
         1/(Sqrt(-Power(s1s2,2) + s1s1*s2s2)*Sqrt(-Power(s1t2,2) + s1s1*t2t2))),
     Sqrt(s1s1)*((-3*Power(s1t2,2)*(s1s2*s1t2 - s1s1*s2t2))/
          (Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
             Power(-Power(s1t2,2) + s1s1*t2t2,2.5)) - 
         (2*s1s2*s1t2)/
          (Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
             Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) - 
         (s1s2*s1t2 - s1s1*s2t2)/
          (Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
             Power(-Power(s1t2,2) + s1s1*t2t2,1.5))),
     (Power(s1s1,1.5)*s1t2*(s1s2*s1t2 - s1s1*s2t2))/
        (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
          Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) + 
      (Power(s1s1,1.5)*s1s2)/
        (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
          Sqrt(-Power(s1t2,2) + s1s1*t2t2)),
     (Power(s1s1,1.5)*s1t2)/
      (Sqrt(-Power(s1s2,2) + s1s1*s2s2)*Power(-Power(s1t2,2) + s1s1*t2t2,1.5)),
     (3*Power(s1s1,1.5)*s1t2*(s1s2*s1t2 - s1s1*s2t2))/
        (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
          Power(-Power(s1t2,2) + s1s1*t2t2,2.5)) + 
      (Power(s1s1,1.5)*s1s2)/
        (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
          Power(-Power(s1t2,2) + s1s1*t2t2,1.5))),
    List((Sqrt(s1s1)*(s1s2*s1t2 - s1s1*s2t2))/
        (4.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
          Sqrt(-Power(s1t2,2) + s1s1*t2t2)) + 
      Sqrt(s1s1)*(-(s1s1*(s1s2*s1t2 - s1s1*s2t2)*t2t2)/
            (4.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
              Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) - 
          (s1s1*s2t2)/
            (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2)) - 
          (3*s1s1*s2s2*(s1s2*s1t2 - s1s1*s2t2))/
            (4.*Power(-Power(s1s2,2) + s1s1*s2s2,2.5)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2)) + 
          (s1s2*s1t2 - s1s1*s2t2)/
            (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2))),
     Sqrt(s1s1)*((s1s1*s1t2)/
          (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
             Sqrt(-Power(s1t2,2) + s1s1*t2t2)) + 
         (3*s1s1*s1s2*(s1s2*s1t2 - s1s1*s2t2))/
          (2.*Power(-Power(s1s2,2) + s1s1*s2s2,2.5)*
             Sqrt(-Power(s1t2,2) + s1s1*t2t2))),
     Sqrt(s1s1)*((s1s1*s1t2*(s1s2*s1t2 - s1s1*s2t2))/
          (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
             Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) + 
         (s1s1*s1s2)/
          (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
             Sqrt(-Power(s1t2,2) + s1s1*t2t2))),
     (-3*Power(s1s1,2.5)*(s1s2*s1t2 - s1s1*s2t2))/
      (4.*Power(-Power(s1s2,2) + s1s1*s2s2,2.5)*
         Sqrt(-Power(s1t2,2) + s1s1*t2t2)),
     -Power(s1s1,2.5)/
      (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
         Sqrt(-Power(s1t2,2) + s1s1*t2t2)),
     -(Power(s1s1,2.5)*(s1s2*s1t2 - s1s1*s2t2))/
      (4.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
         Power(-Power(s1t2,2) + s1s1*t2t2,1.5))),
    List(Sqrt(s1s1)/
        (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*Sqrt(-Power(s1t2,2) + s1s1*t2t2))
        + Sqrt(s1s1)*(-(s1s1*t2t2)/
            (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
              Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) - 
          (s1s1*s2s2)/
            (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
              Sqrt(-Power(s1t2,2) + s1s1*t2t2)) + 
          1/(Sqrt(-Power(s1s2,2) + s1s1*s2s2)*Sqrt(-Power(s1t2,2) + s1s1*t2t2)))
      ,(Power(s1s1,1.5)*s1s2)/
      (Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*Sqrt(-Power(s1t2,2) + s1s1*t2t2)),
     (Power(s1s1,1.5)*s1t2)/
      (Sqrt(-Power(s1s2,2) + s1s1*s2s2)*Power(-Power(s1t2,2) + s1s1*t2t2,1.5)),
     -Power(s1s1,2.5)/
      (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
         Sqrt(-Power(s1t2,2) + s1s1*t2t2)),0,
     -Power(s1s1,2.5)/
      (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
         Power(-Power(s1t2,2) + s1s1*t2t2,1.5))),
    List((Sqrt(s1s1)*(s1s2*s1t2 - s1s1*s2t2))/
        (4.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
          Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) + 
      Sqrt(s1s1)*((-3*s1s1*(s1s2*s1t2 - s1s1*s2t2)*t2t2)/
            (4.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
              Power(-Power(s1t2,2) + s1s1*t2t2,2.5)) - 
          (s1s1*s2t2)/
            (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
              Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) - 
          (s1s1*s2s2*(s1s2*s1t2 - s1s1*s2t2))/
            (4.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
              Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) + 
          (s1s2*s1t2 - s1s1*s2t2)/
            (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
              Power(-Power(s1t2,2) + s1s1*t2t2,1.5))),
     Sqrt(s1s1)*((s1s1*s1t2)/
          (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
             Power(-Power(s1t2,2) + s1s1*t2t2,1.5)) + 
         (s1s1*s1s2*(s1s2*s1t2 - s1s1*s2t2))/
          (2.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
             Power(-Power(s1t2,2) + s1s1*t2t2,1.5))),
     Sqrt(s1s1)*((3*s1s1*s1t2*(s1s2*s1t2 - s1s1*s2t2))/
          (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
             Power(-Power(s1t2,2) + s1s1*t2t2,2.5)) + 
         (s1s1*s1s2)/
          (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
             Power(-Power(s1t2,2) + s1s1*t2t2,1.5))),
     -(Power(s1s1,2.5)*(s1s2*s1t2 - s1s1*s2t2))/
      (4.*Power(-Power(s1s2,2) + s1s1*s2s2,1.5)*
         Power(-Power(s1t2,2) + s1s1*t2t2,1.5)),
     -Power(s1s1,2.5)/
      (2.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
         Power(-Power(s1t2,2) + s1s1*t2t2,1.5)),
     (-3*Power(s1s1,2.5)*(s1s2*s1t2 - s1s1*s2t2))/
      (4.*Sqrt(-Power(s1s2,2) + s1s1*s2s2)*
         Power(-Power(s1t2,2) + s1s1*t2t2,2.5))));

#define S1S1 0
#define S1S2 1
#define S1T2 2
#define S2S2 3
#define S2T2 4
#define T2T2 5
#define S1 0
#define T2 1
#define S2 2
 
    /* chain rule intermediate partials */

    for ( i = 0 ; i < 6 ; i++ )
      for ( j = 0 ; j < 3 ; j++ )
         for ( k = 0 ; k < SDIM ; k++ )
             dqdx[i][j][k] = 0.0;

    for ( i = 0 ; i < SDIM ; i++ )
    {
      dqdx[S1S1][S1][i] = 2*s1[i];
      dqdx[S1S2][S1][i] = s2[i];
      dqdx[S1S2][S2][i] = s1[i];
      dqdx[S1T2][S1][i] = t2[i];
      dqdx[S1T2][T2][i] = s1[i];
      dqdx[S2S2][S2][i] = 2*s2[i];
      dqdx[S2T2][S2][i] = t2[i];
      dqdx[S2T2][T2][i] = s2[i];
      dqdx[T2T2][T2][i] = 2*t2[i];
    }

    for ( i = 0 ; i < 6 ; i++ )
      for ( j = 0 ; j < 3 ; j++ )
         for ( k = 0 ; k < 3 ; k++ )
             ddqdxx[i][j][k] = 0.0;

    ddqdxx[S1S1][S1][S1] = 2.0;
    ddqdxx[S1S2][S1][S2] = 1.0;
    ddqdxx[S1S2][S2][S1] = 1.0;
    ddqdxx[S1T2][S1][T2] = 1.0;
    ddqdxx[S1T2][T2][S1] = 1.0;
    ddqdxx[S2S2][S2][S2] = 2.0;
    ddqdxx[S2T2][S2][T2] = 1.0;
    ddqdxx[S2T2][T2][S2] = 1.0;
    ddqdxx[T2T2][T2][T2] = 2.0;

    /* now assemble */

    for ( n = 0 ; n < 3 ; n++ ) /* which edge fan vertex */
     for ( i = 0 ; i < SDIM ; i++ ) /* dimension on first */
      for ( nn = 0 ; nn < 3 ; nn++ ) /* which edge fan vertex */
        for ( k = 0 ; k < SDIM ; k++ ) /* dimension on second */
         { REAL sum = 0.0;
            for ( j = 0 ; j < 6 ; j++ )
            { for ( jj = 0 ; jj < 6; jj++ )
                sum += Htemp[j][jj]*dqdx[j][n][i]*dqdx[jj][nn][k];
              if ( i == k ) sum += Gtemp[j]*ddqdxx[j][n][nn];
            }
            e_info->hess[n+1][nn+1][i][k] += sum;
            e_info->hess[n+1][0][i][k] -= sum;
            e_info->hess[0][nn+1][i][k] -= sum;
            e_info->hess[0][0][i][k] += sum;
         }

    return sqrt(s1s1)*(1 - cos_th);
#else
    kb_error(2169,"dihedral_hooke hessian not implemented on Mac 68K.\n",WARNING);
    return 0.0;
#endif
}

