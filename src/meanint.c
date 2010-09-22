/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*************************************************************
*
*     file:        meanint.c
*
*     Purpose:    Implements routines for integral of mean
*                 curvature over a surface. Computation is 
*                 exact, as the integral can be defined for a
*                 polyhedral surface as edge dihedral angle
*                 times edge length.  Mean curvature is average
*                 of sectional curvatures, not sum.
*
*                 Implemented as general quantity.  Uses info
*                 passed in qinfo structure.
*                 qinfo has vertices: tail,head,left wing, right wing
*                 qinfo has sides: edge,left from tail, right from tail
*     Basic formulas:
* 
*     cos(theta) =              <A,B><A,C> - <A,A><B,C>
*                      _________________________________________________
*                      sqrt(<A,A><B,B>-<A,B>^2)sqrt((<A,A><C,C>-<A,C>^2)
*
*     sin(theta) =             <A x B, C> ||A||
*                      _________________________________________________
*                      sqrt(<A,A><B,B>-<A,B>^2)sqrt((<A,A><C,C>-<A,C>^2)
*
*     tan(theta) =      <A x B, C> ||A||
*                      _______________________
*                      <A,B><A,C> - <A,A><B,C>
*
*     Will use qinfo.ss to hold side products.
*/

#include "include.h"

/*********************************************************************
*
* function: mean_int_init()
*
* purpose:  Check illegalities
*
*/

void mean_int_init(mode,mi)
int mode;
struct method_instance *mi;
{
  if ( web.dimension != 2 )
     kb_error(1594,"mean_curvature_integral method only for 2D facets.\n",RECOVERABLE);

  if ( web.modeltype != LINEAR )
     kb_error(1595,"mean_curvature_integral method only for LINEAR model.\n",RECOVERABLE);


  if ( everything_quantities_flag && mean_curv_int_flag )
     GEN_QUANT(mean_curv_int_quantity_num)->modulus = 
        globals(mean_curvature_param)->value.real;
}

/**************************************************************
*  
*  Function: mean_int_value()
*
*  Purpose:  Computes contribution of one edge. 
*/

REAL mean_int_value(e_info)
struct qinfo *e_info;
{ REAL sinq,cosq; /* proprotional to sin and cos of angle */
  REAL theta; /* the angle */
  REAL len; /* length of side */
  REAL vol; /* volume of parallelpiped of sides */
  int sign = inverted(get_fe_facet(get_edge_fe(e_info->id))) ? 1 : -1;
  
  mat_tsquare(e_info->sides[0],e_info->ss,3,SDIM); /* side products */
  len = sqrt(e_info->ss[0][0]);
  vol = triple_prod(e_info->sides[0][0],e_info->sides[0][1],e_info->sides[0][2]);
  if ( vol == 0.0 ) return vol;
  sinq = len*vol;
  cosq = e_info->ss[0][1]*e_info->ss[0][2] - e_info->ss[0][0]*e_info->ss[1][2];
  theta = atan2(sinq,cosq);
  return sign*theta*len/2;
}

/**************************************************************
*  
*  Function: mean_int_gradient()
*
*  Purpose:  Computes contribution of one edge to gradient. 
*/

REAL mean_int_gradient(e_info)
struct qinfo *e_info;
{ REAL sinq,cosq; /* proprotional to sin and cos of angle */
  REAL dsinq,dcosq; /* derivatives */
  REAL theta;
  REAL dtheta;
  REAL len,dlen;
  REAL vol,dvol;
  int i,j;
  REAL g;
  int sign = inverted(get_fe_facet(get_edge_fe(e_info->id))) ? 1 : -1;
  
  for ( i = 0 ; i < 4 ; i++ )
    for ( j = 0 ; j < SDIM ; j++ )
      e_info->grad[i][j] = 0.0;

  mat_tsquare(e_info->sides[0],e_info->ss,3,SDIM); /* side products */
  len = sqrt(e_info->ss[0][0]);
  vol = triple_prod(e_info->sides[0][0],e_info->sides[0][1],e_info->sides[0][2]);
  if ( vol == 0.0 )  return 0.0;
  sinq = len*vol;
  cosq = e_info->ss[0][1]*e_info->ss[0][2] - e_info->ss[0][0]*e_info->ss[1][2];
  theta = atan2(sinq,cosq);

  /* derivatives of stuff, side by side */
  /* doing partial derivatives in each coordinate */
  for ( i = 0 ; i < SDIM ; i++ )
    { /* main edge */
      dlen = e_info->sides[0][0][i]/len;
      dvol = e_info->sides[0][1][(i+1)%3]*e_info->sides[0][2][(i+2)%3]
                - e_info->sides[0][1][(i+2)%3]*e_info->sides[0][2][(i+1)%3];
      dsinq = dlen*vol + len*dvol;
      dcosq = e_info->sides[0][1][i]*e_info->ss[0][2]
              + e_info->ss[0][1]*e_info->sides[0][2][i]
            - 2*e_info->sides[0][0][i]*e_info->ss[1][2];
      dtheta = (cosq*dsinq-dcosq*sinq)/(sinq*sinq+cosq*cosq);
      g = sign*(dtheta*len + theta*dlen)/2;
      e_info->grad[1][i] += g;
      e_info->grad[0][i] -= g;

      /* left edge */
      dvol = e_info->sides[0][2][(i+1)%3]*e_info->sides[0][0][(i+2)%3]
                - e_info->sides[0][2][(i+2)%3]*e_info->sides[0][0][(i+1)%3];
      dsinq = len*dvol;
      dcosq = e_info->sides[0][0][i]*e_info->ss[0][2]
                    - e_info->ss[0][0]*e_info->sides[0][2][i];
      dtheta = (cosq*dsinq-dcosq*sinq)/(sinq*sinq+cosq*cosq);
      g = sign*dtheta*len/2;
      e_info->grad[2][i] += g;
      e_info->grad[0][i] -= g;

      /* right edge */
      dvol = e_info->sides[0][0][(i+1)%3]*e_info->sides[0][1][(i+2)%3]
                - e_info->sides[0][0][(i+2)%3]*e_info->sides[0][1][(i+1)%3];
      dsinq = len*dvol;
      dcosq = e_info->sides[0][0][i]*e_info->ss[0][1]
                 - e_info->ss[0][0]*e_info->sides[0][1][i];
      dtheta = (cosq*dsinq-dcosq*sinq)/(sinq*sinq+cosq*cosq);
      g = sign*dtheta*len/2;
      e_info->grad[3][i] += g;
      e_info->grad[0][i] -= g;
    }
  return sign*theta*len/2;
}

/**************************************************************
*  
*  Function: mean_int_hessian()
*
*  Purpose:  Computes contribution of one edge to hessian. 
*/

REAL mean_int_hessian(e_info)
struct qinfo *e_info;
{ REAL sinq,cosq; /* proprotional to sin and cos of angle */
  REAL dsinq,dcosq; /* derivatives */
  REAL theta;
  REAL dtheta;
  REAL len,dlen;
  REAL vol,dvol;
  int i,j,ii,jj;
  REAL g;
  int sign = inverted(get_fe_facet(get_edge_fe(e_info->id))) ? 1 : -1;
  REAL ax,ay,az,bx,by,bz,cx,cy,cz;
  REAL aa,ab,ac,bb,bc,cc,a,aaa,det;
  REAL value;
 
  /* same value and gradient calculation as mean_int_gradient() */
  mat_tsquare(e_info->sides[0],e_info->ss,3,SDIM); /* side products */
  len = sqrt(e_info->ss[0][0]);
  vol = triple_prod(e_info->sides[0][0],e_info->sides[0][1],e_info->sides[0][2]);
  if ( vol == 0.0 )  return 0.0;
  sinq = len*vol;
  cosq = e_info->ss[0][1]*e_info->ss[0][2] - e_info->ss[0][0]*e_info->ss[1][2];
  theta = atan2(sinq,cosq);

  /* derivatives of stuff, side by side */
  /* doing partial derivatives in each coordinate */
  for ( i = 0 ; i < SDIM ; i++ )
    { /* main edge */
      dlen = e_info->sides[0][0][i]/len;
      dvol = e_info->sides[0][1][(i+1)%3]*e_info->sides[0][2][(i+2)%3]
                - e_info->sides[0][1][(i+2)%3]*e_info->sides[0][2][(i+1)%3];
      dsinq = dlen*vol + len*dvol;
      dcosq = e_info->sides[0][1][i]*e_info->ss[0][2]
              + e_info->ss[0][1]*e_info->sides[0][2][i]
            - 2*e_info->sides[0][0][i]*e_info->ss[1][2];
      dtheta = (cosq*dsinq-dcosq*sinq)/(sinq*sinq+cosq*cosq);
      g = sign*(dtheta*len + theta*dlen)/2;
      e_info->grad[1][i] += g;
      e_info->grad[0][i] -= g;

      /* left edge */
      dvol = e_info->sides[0][2][(i+1)%3]*e_info->sides[0][0][(i+2)%3]
                - e_info->sides[0][2][(i+2)%3]*e_info->sides[0][0][(i+1)%3];
      dsinq = len*dvol;
      dcosq = e_info->sides[0][0][i]*e_info->ss[0][2]
                    - e_info->ss[0][0]*e_info->sides[0][2][i];
      dtheta = (cosq*dsinq-dcosq*sinq)/(sinq*sinq+cosq*cosq);
      g = sign*dtheta*len/2;
      e_info->grad[2][i] += g;
      e_info->grad[0][i] -= g;

      /* right edge */
      dvol = e_info->sides[0][0][(i+1)%3]*e_info->sides[0][1][(i+2)%3]
                - e_info->sides[0][0][(i+2)%3]*e_info->sides[0][1][(i+1)%3];
      dsinq = len*dvol;
      dcosq = e_info->sides[0][0][i]*e_info->ss[0][1]
                 - e_info->ss[0][0]*e_info->sides[0][1][i];
      dtheta = (cosq*dsinq-dcosq*sinq)/(sinq*sinq+cosq*cosq);
      g = sign*dtheta*len/2;
      e_info->grad[3][i] += g;
      e_info->grad[0][i] -= g;
    }


  /* Now Hessian, as ground out by Mathematica, in meanint.m */
  ax = e_info->sides[0][0][0];
  ay = e_info->sides[0][0][1];
  az = e_info->sides[0][0][2];
  bx = e_info->sides[0][1][0];
  by = e_info->sides[0][1][1];
  bz = e_info->sides[0][1][2];
  cx = e_info->sides[0][2][0];
  cy = e_info->sides[0][2][1];
  cz = e_info->sides[0][2][2];
  aa = e_info->ss[0][0];
  bb = e_info->ss[1][1];
  cc = e_info->ss[2][2];
  ab = e_info->ss[0][1];
  ac = e_info->ss[0][2];
  bc = e_info->ss[1][2];
  a = sqrt(aa);
  aaa = a*aa;
  det = vol;
  value = a*atan2(det*a,ab*ac-aa*bc);

  /* Copied from Mathematica */
#define Power(q,p)  ((q)*(q))
#define ArcTan(x,y) atan2((y),(x))

    e_info->hess[1][1][0][0] = ((2*a*ax*
            ((ab*ac - aa*bc)*(ax*(-(az*by*cx) + ay*bz*cx + az*bx*cy - 
                    ay*bx*cz) + aa*(-(bz*cy) + by*cz) + 
                 Power(ax,2)*(-(bz*cy) + by*cz)) + 
              aa*(2*ax*bc - ac*bx - ab*cx)*det))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) - 
         (2*aaa*((ab*ac - aa*bc)*(ax*
                  (-(az*by*cx) + ay*bz*cx + az*bx*cy - ay*bx*cz) + 
                 aa*(-(bz*cy) + by*cz) + Power(ax,2)*(-(bz*cy) + by*cz)) + 
              aa*(2*ax*bc - ac*bx - ab*cx)*det)*
            ((ab*ac - aa*bc)*(-2*ax*bc + ac*bx + ab*cx) - 
              aa*(bz*cy - by*cz)*det + ax*Power(det,2)))/
          Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2) + 
         (a*(Power(aa,2)*(az*(bc - 2*bx*cx)*(-(by*cx) + bx*cy) + 
                 ay*(bc - 2*bx*cx)*(bz*cx - bx*cz) + 
                 ax*(bc + 2*bx*cx)*(bz*cy - by*cz)) - ab*ac*Power(ax,2)*det + 
              aa*(ab*ac*(-(az*by*cx) + ay*bz*cx + az*bx*cy - 3*ax*bz*cy - 
                    ay*bx*cz + 3*ax*by*cz) + Power(ax,2)*bc*det)))/
          (Power(ab,2)*Power(ac,2) - 2*aa*ab*ac*bc + 
            aa*(aa*Power(bc,2) + Power(det,2))) + 
         aa*ArcTan(ab*ac - aa*bc,a*det) - Power(ax,2)*ArcTan(ab*ac - aa*bc,a*det)\
    )/aaa;
    e_info->hess[1][1][0][1] = ((a*ay*
            ((ab*ac - aa*bc)*(ax*(-(az*by*cx) + ay*bz*cx + az*bx*cy - 
                    ay*bx*cz) + aa*(-(bz*cy) + by*cz) + 
                 Power(ax,2)*(-(bz*cy) + by*cz)) + 
              aa*(2*ax*bc - ac*bx - ab*cx)*det))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) + 
         (a*ax*((ab*ac - aa*bc)*(aa*(bz*cx - bx*cz) + 
                 Power(ay,2)*(bz*cx - bx*cz) + 
                 ay*(-(az*by*cx) + az*bx*cy - ax*bz*cy + ax*by*cz)) + 
              aa*(2*ay*bc - ac*by - ab*cy)*det))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) + 
         (a*(-(Power(aa,2)*(-2*ax*bc + ac*bx + ab*cx)*(bz*cx - bx*cz)) + 
              aa*(-2*ay*bc + ac*by + ab*cy)*
               (ax*(-(az*by*cx) + ay*bz*cx + az*bx*cy - ay*bx*cz) + 
                 aa*(-(bz*cy) + by*cz) + Power(ax,2)*(-(bz*cy) + by*cz)) + 
              (ab*ac - aa*bc)*(Power(ax,2)*ay*(bz*cy - by*cz) + 
                 aa*ay*(-(bz*cy) + by*cz) + 
                 ax*(ay*az*(by*cx - bx*cy) + aa*(bz*cx - bx*cz) + 
                    Power(ay,2)*(-(bz*cx) + bx*cz))) + 
              aa*ay*(2*ax*bc - ac*bx - ab*cx)*det - 
              Power(aa,2)*(by*cx + bx*cy)*det))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) - 
         (2*aaa*((ab*ac - aa*bc)*(ax*
                  (-(az*by*cx) + ay*bz*cx + az*bx*cy - ay*bx*cz) + 
                 aa*(-(bz*cy) + by*cz) + Power(ax,2)*(-(bz*cy) + by*cz)) + 
              aa*(2*ax*bc - ac*bx - ab*cx)*det)*
            ((ab*ac - aa*bc)*(-2*ay*bc + ac*by + ab*cy) + 
              aa*(bz*cx - bx*cz)*det + ay*Power(det,2)))/
          Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2) - 
         ax*ay*ArcTan(ab*ac - aa*bc,a*det))/aaa;
    e_info->hess[1][1][0][2] = ((a*az*
            ((ab*ac - aa*bc)*(ax*(-(az*by*cx) + ay*bz*cx + az*bx*cy - 
                    ay*bx*cz) + aa*(-(bz*cy) + by*cz) + 
                 Power(ax,2)*(-(bz*cy) + by*cz)) + 
              aa*(2*ax*bc - ac*bx - ab*cx)*det))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) - 
         (2*aaa*((ab*ac - aa*bc)*(ax*
                  (-(az*by*cx) + ay*bz*cx + az*bx*cy - ay*bx*cz) + 
                 aa*(-(bz*cy) + by*cz) + Power(ax,2)*(-(bz*cy) + by*cz)) + 
              aa*(2*ax*bc - ac*bx - ab*cx)*det)*
            ((ab*ac - aa*bc)*(-2*az*bc + ac*bz + ab*cz) - 
              aa*(by*cx - bx*cy)*det + az*Power(det,2)))/
          Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2) + 
         (a*ax*(aa*(2*az*bc - ac*bz - ab*cz)*det + 
              (-(ab*ac) + aa*bc)*(aa*(by*cx - bx*cy) - az*det)))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) + 
         (a*(-(Power(aa,2)*(-2*ax*bc + ac*bx + ab*cx)*(-(by*cx) + bx*cy)) + 
              aa*(2*az*bc - ac*bz - ab*cz)*
               (ax*(az*by*cx - ay*bz*cx - az*bx*cy + ay*bx*cz) + 
                 aa*(bz*cy - by*cz) + Power(ax,2)*(bz*cy - by*cz)) + 
              aa*az*(2*ax*bc - ac*bx - ab*cx)*det - 
              Power(aa,2)*(bz*cx + bx*cz)*det + 
              (-(ab*ac) + aa*bc)*(aa*
                  (ax*by*cx - ax*bx*cy + az*bz*cy - az*by*cz) + ax*az*det)))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) - 
         ax*az*ArcTan(ab*ac - aa*bc,a*det))/aaa;
    e_info->hess[1][1][1][0] = ((a*ay*
            ((ab*ac - aa*bc)*(ax*(-(az*by*cx) + ay*bz*cx + az*bx*cy - 
                    ay*bx*cz) + aa*(-(bz*cy) + by*cz) + 
                 Power(ax,2)*(-(bz*cy) + by*cz)) + 
              aa*(2*ax*bc - ac*bx - ab*cx)*det))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) + 
         (a*ax*((ab*ac - aa*bc)*(aa*(bz*cx - bx*cz) + 
                 Power(ay,2)*(bz*cx - bx*cz) + 
                 ay*(-(az*by*cx) + az*bx*cy - ax*bz*cy + ax*by*cz)) + 
              aa*(2*ay*bc - ac*by - ab*cy)*det))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) + 
         (a*(-(Power(aa,2)*(-2*ay*bc + ac*by + ab*cy)*(-(bz*cy) + by*cz)) + 
              aa*(2*ax*bc - ac*bx - ab*cx)*
               (aa*(-(bz*cx) + bx*cz) + Power(ay,2)*(-(bz*cx) + bx*cz) + 
                 ay*(az*by*cx - az*bx*cy + ax*bz*cy - ax*by*cz)) + 
              (ab*ac - aa*bc)*(Power(ax,2)*ay*(bz*cy - by*cz) + 
                 aa*ay*(-(bz*cy) + by*cz) + 
                 ax*(ay*az*(by*cx - bx*cy) + aa*(bz*cx - bx*cz) + 
                    Power(ay,2)*(-(bz*cx) + bx*cz))) + 
              aa*ax*(2*ay*bc - ac*by - ab*cy)*det - 
              Power(aa,2)*(by*cx + bx*cy)*det))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) - 
         (2*aaa*((ab*ac - aa*bc)*(aa*(bz*cx - bx*cz) + 
                 Power(ay,2)*(bz*cx - bx*cz) + 
                 ay*(-(az*by*cx) + az*bx*cy - ax*bz*cy + ax*by*cz)) + 
              aa*(2*ay*bc - ac*by - ab*cy)*det)*
            ((ab*ac - aa*bc)*(-2*ax*bc + ac*bx + ab*cx) - 
              aa*(bz*cy - by*cz)*det + ax*Power(det,2)))/
          Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2) - 
         ax*ay*ArcTan(ab*ac - aa*bc,a*det))/aaa;
    e_info->hess[1][1][1][1] = ((2*a*ay*
            ((ab*ac - aa*bc)*(aa*(bz*cx - bx*cz) + 
                 Power(ay,2)*(bz*cx - bx*cz) + 
                 ay*(-(az*by*cx) + az*bx*cy - ax*bz*cy + ax*by*cz)) + 
              aa*(2*ay*bc - ac*by - ab*cy)*det))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) - 
         (2*aaa*((ab*ac - aa*bc)*(aa*(bz*cx - bx*cz) + 
                 Power(ay,2)*(bz*cx - bx*cz) + 
                 ay*(-(az*by*cx) + az*bx*cy - ax*bz*cy + ax*by*cz)) + 
              aa*(2*ay*bc - ac*by - ab*cy)*det)*
            ((ab*ac - aa*bc)*(-2*ay*bc + ac*by + ab*cy) + 
              aa*(bz*cx - bx*cz)*det + ay*Power(det,2)))/
          Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2) + 
         (a*(Power(aa,2)*(az*(by*cx - bx*cy)*(-bc + 2*by*cy) - 
                 ay*(bc + 2*by*cy)*(bz*cx - bx*cz) + 
                 ax*(bc - 2*by*cy)*(-(bz*cy) + by*cz)) - 
              ab*ac*Power(ay,2)*det + 
              aa*(ab*ac*(-(az*by*cx) + 3*ay*bz*cx + az*bx*cy - ax*bz*cy - 
                    3*ay*bx*cz + ax*by*cz) + Power(ay,2)*bc*det)))/
          (Power(ab,2)*Power(ac,2) - 2*aa*ab*ac*bc + 
            aa*(aa*Power(bc,2) + Power(det,2))) + 
         aa*ArcTan(ab*ac - aa*bc,a*det) - Power(ay,2)*ArcTan(ab*ac - aa*bc,a*det)\
    )/aaa;
    e_info->hess[1][1][1][2] = ((a*az*
            ((ab*ac - aa*bc)*(aa*(bz*cx - bx*cz) + 
                 Power(ay,2)*(bz*cx - bx*cz) + 
                 ay*(-(az*by*cx) + az*bx*cy - ax*bz*cy + ax*by*cz)) + 
              aa*(2*ay*bc - ac*by - ab*cy)*det))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) - 
         (2*aaa*((ab*ac - aa*bc)*(aa*(bz*cx - bx*cz) + 
                 Power(ay,2)*(bz*cx - bx*cz) + 
                 ay*(-(az*by*cx) + az*bx*cy - ax*bz*cy + ax*by*cz)) + 
              aa*(2*ay*bc - ac*by - ab*cy)*det)*
            ((ab*ac - aa*bc)*(-2*az*bc + ac*bz + ab*cz) - 
              aa*(by*cx - bx*cy)*det + az*Power(det,2)))/
          Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2) + 
         (a*ay*(aa*(2*az*bc - ac*bz - ab*cz)*det + 
              (-(ab*ac) + aa*bc)*(aa*(by*cx - bx*cy) - az*det)))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) + 
         (a*(-(Power(aa,2)*(-2*ay*bc + ac*by + ab*cy)*(-(by*cx) + bx*cy)) + 
              aa*(2*az*bc - ac*bz - ab*cz)*
               (aa*(-(bz*cx) + bx*cz) + Power(ay,2)*(-(bz*cx) + bx*cz) + 
                 ay*(az*by*cx - az*bx*cy + ax*bz*cy - ax*by*cz)) + 
              aa*az*(2*ay*bc - ac*by - ab*cy)*det - 
              Power(aa,2)*(bz*cy + by*cz)*det + 
              (-(ab*ac) + aa*bc)*(aa*
                  (ay*by*cx - az*bz*cx - ay*bx*cy + az*bx*cz) + ay*az*det)))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) - 
         ay*az*ArcTan(ab*ac - aa*bc,a*det))/aaa;
    e_info->hess[1][1][2][0] = ((a*az*
            ((ab*ac - aa*bc)*(ax*(-(az*by*cx) + ay*bz*cx + az*bx*cy - 
                    ay*bx*cz) + aa*(-(bz*cy) + by*cz) + 
                 Power(ax,2)*(-(bz*cy) + by*cz)) + 
              aa*(2*ax*bc - ac*bx - ab*cx)*det))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) + 
         (a*ax*(aa*(2*az*bc - ac*bz - ab*cz)*det + 
              (-(ab*ac) + aa*bc)*(aa*(by*cx - bx*cy) - az*det)))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) - 
         (2*aaa*((ab*ac - aa*bc)*(-2*ax*bc + ac*bx + ab*cx) - 
              aa*(bz*cy - by*cz)*det + ax*Power(det,2))*
            (aa*(2*az*bc - ac*bz - ab*cz)*det + 
              (-(ab*ac) + aa*bc)*(aa*(by*cx - bx*cy) - az*det)))/
          Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2) + 
         (a*(-(Power(aa,2)*(-2*az*bc + ac*bz + ab*cz)*(-(bz*cy) + by*cz)) + 
              aa*ax*(2*az*bc - ac*bz - ab*cz)*det - 
              Power(aa,2)*(bz*cx + bx*cz)*det - 
              aa*(2*ax*bc - ac*bx - ab*cx)*(aa*(-(by*cx) + bx*cy) + az*det) + 
              (-(ab*ac) + aa*bc)*(aa*
                  (ax*by*cx - ax*bx*cy + az*bz*cy - az*by*cz) + ax*az*det)))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) - 
         ax*az*ArcTan(ab*ac - aa*bc,a*det))/aaa;
    e_info->hess[1][1][2][1] = ((a*az*
            ((ab*ac - aa*bc)*(aa*(bz*cx - bx*cz) + 
                 Power(ay,2)*(bz*cx - bx*cz) + 
                 ay*(-(az*by*cx) + az*bx*cy - ax*bz*cy + ax*by*cz)) + 
              aa*(2*ay*bc - ac*by - ab*cy)*det))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) + 
         (a*ay*(aa*(2*az*bc - ac*bz - ab*cz)*det + 
              (-(ab*ac) + aa*bc)*(aa*(by*cx - bx*cy) - az*det)))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) - 
         (2*aaa*((ab*ac - aa*bc)*(-2*ay*bc + ac*by + ab*cy) + 
              aa*(bz*cx - bx*cz)*det + ay*Power(det,2))*
            (aa*(2*az*bc - ac*bz - ab*cz)*det + 
              (-(ab*ac) + aa*bc)*(aa*(by*cx - bx*cy) - az*det)))/
          Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2) + 
         (a*(-(Power(aa,2)*(-2*az*bc + ac*bz + ab*cz)*(bz*cx - bx*cz)) + 
              aa*ay*(2*az*bc - ac*bz - ab*cz)*det - 
              Power(aa,2)*(bz*cy + by*cz)*det + 
              aa*(2*ay*bc - ac*by - ab*cy)*(aa*(by*cx - bx*cy) - az*det) + 
              (-(ab*ac) + aa*bc)*(aa*
                  (ay*by*cx - az*bz*cx - ay*bx*cy + az*bx*cz) + ay*az*det)))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) - 
         ay*az*ArcTan(ab*ac - aa*bc,a*det))/aaa;
    e_info->hess[1][1][2][2] = ((2*a*az*
            (aa*(2*az*bc - ac*bz - ab*cz)*det + 
              (-(ab*ac) + aa*bc)*(aa*(by*cx - bx*cy) - az*det)))/
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) - 
         (2*aaa*((ab*ac - aa*bc)*(-2*az*bc + ac*bz + ab*cz) - 
              aa*(by*cx - bx*cy)*det + az*Power(det,2))*
            (aa*(2*az*bc - ac*bz - ab*cz)*det + 
              (-(ab*ac) + aa*bc)*(aa*(by*cx - bx*cy) - az*det)))/
          Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2) + 
         (a*(Power(aa,2)*((ay*bz*cx - ax*bz*cy - ay*bx*cz + ax*by*cz)*
                  (bc - 2*bz*cz) + az*(by*cx - bx*cy)*(bc + 2*bz*cz)) - 
              ab*ac*Power(az,2)*det + 
              aa*(ab*ac*(-3*az*by*cx + ay*bz*cx + 3*az*bx*cy - ax*bz*cy - 
                    ay*bx*cz + ax*by*cz) + Power(az,2)*bc*det)))/
          (Power(ab,2)*Power(ac,2) - 2*aa*ab*ac*bc + 
            aa*(aa*Power(bc,2) + Power(det,2))) + 
         aa*ArcTan(ab*ac - aa*bc,a*det) - Power(az,2)*ArcTan(ab*ac - aa*bc,a*det)\
    )/aaa;
    e_info->hess[1][2][0][0] = (-(((ab*ac - aa*bc)*
               (ax*(-(az*by*cx) + ay*bz*cx + az*bx*cy - ay*bx*cz) + 
                 aa*(-(bz*cy) + by*cz) + Power(ax,2)*(-(bz*cy) + by*cz)) + 
              aa*(2*ax*bc - ac*bx - ab*cx)*det)*
            (2*(ab*ac - aa*bc)*(ac*ax - aa*cx) + 2*aa*(az*cy - ay*cz)*det)) + 
         ax*((ab*ac - aa*bc)*(az*cy - ay*cz) - (ac*ax - aa*cx)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) + 
         (ax*(ab*ac - aa*bc)*(az*cy - ay*cz) - 
            aa*(-2*ax*bc + ac*bx + ab*cx)*(az*cy - ay*cz) + 
            (ac*ax - aa*cx)*(ax*(-(az*by*cx) + ay*bz*cx + az*bx*cy - 
                  ay*bx*cz) + aa*(-(bz*cy) + by*cz) + 
               Power(ax,2)*(-(bz*cy) + by*cz)) - aa*(ay*cy + az*cz)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[1][2][0][1] = (-(((ab*ac - aa*bc)*
               (ax*(-(az*by*cx) + ay*bz*cx + az*bx*cy - ay*bx*cz) + 
                 aa*(-(bz*cy) + by*cz) + Power(ax,2)*(-(bz*cy) + by*cz)) + 
              aa*(2*ax*bc - ac*bx - ab*cx)*det)*
            (2*(ab*ac - aa*bc)*(ac*ay - aa*cy) - 2*aa*(az*cx - ax*cz)*det)) + 
         ax*((ab*ac - aa*bc)*(-(az*cx) + ax*cz) - (ac*ay - aa*cy)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) + 
         (-(aa*(-2*ax*bc + ac*bx + ab*cx)*(-(az*cx) + ax*cz)) + 
            (ab*ac - aa*bc)*(-(ax*az*cx) + aa*cz + Power(ax,2)*cz) + 
            (ac*ay - aa*cy)*(ax*(-(az*by*cx) + ay*bz*cx + az*bx*cy - 
                  ay*bx*cz) + aa*(-(bz*cy) + by*cz) + 
               Power(ax,2)*(-(bz*cy) + by*cz)) - aa*(ay*cx - 2*ax*cy)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[1][2][0][2] = (-(((ab*ac - aa*bc)*
               (ax*(-(az*by*cx) + ay*bz*cx + az*bx*cy - ay*bx*cz) + 
                 aa*(-(bz*cy) + by*cz) + Power(ax,2)*(-(bz*cy) + by*cz)) + 
              aa*(2*ax*bc - ac*bx - ab*cx)*det)*
            (2*(ab*ac - aa*bc)*(ac*az - aa*cz) + 2*aa*(ay*cx - ax*cy)*det)) + 
         ax*((ab*ac - aa*bc)*(ay*cx - ax*cy) - (ac*az - aa*cz)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) + 
         (-(aa*(-2*ax*bc + ac*bx + ab*cx)*(ay*cx - ax*cy)) + 
            (-(ab*ac) + aa*bc)*(-(ax*ay*cx) + aa*cy + Power(ax,2)*cy) + 
            (ac*az - aa*cz)*(ax*(-(az*by*cx) + ay*bz*cx + az*bx*cy - 
                  ay*bx*cz) + aa*(-(bz*cy) + by*cz) + 
               Power(ax,2)*(-(bz*cy) + by*cz)) - aa*(az*cx - 2*ax*cz)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[1][2][1][0] = (-(((ab*ac - aa*bc)*
               (aa*(bz*cx - bx*cz) + Power(ay,2)*(bz*cx - bx*cz) + 
                 ay*(-(az*by*cx) + az*bx*cy - ax*bz*cy + ax*by*cz)) + 
              aa*(2*ay*bc - ac*by - ab*cy)*det)*
            (2*(ab*ac - aa*bc)*(ac*ax - aa*cx) + 2*aa*(az*cy - ay*cz)*det)) + 
         ay*((ab*ac - aa*bc)*(az*cy - ay*cz) - (ac*ax - aa*cx)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) + 
         (-(aa*(-2*ay*bc + ac*by + ab*cy)*(az*cy - ay*cz)) + 
            (-(ab*ac) + aa*bc)*(-(ay*az*cy) + aa*cz + Power(ay,2)*cz) + 
            (ac*ax - aa*cx)*(aa*(bz*cx - bx*cz) + 
               Power(ay,2)*(bz*cx - bx*cz) + 
               ay*(-(az*by*cx) + az*bx*cy - ax*bz*cy + ax*by*cz)) + 
            aa*(2*ay*cx - ax*cy)*det)*(Power(ab*ac - aa*bc,2) + aa*Power(det,2)))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[1][2][1][1] = (-(((ab*ac - aa*bc)*
               (aa*(bz*cx - bx*cz) + Power(ay,2)*(bz*cx - bx*cz) + 
                 ay*(-(az*by*cx) + az*bx*cy - ax*bz*cy + ax*by*cz)) + 
              aa*(2*ay*bc - ac*by - ab*cy)*det)*
            (2*(ab*ac - aa*bc)*(ac*ay - aa*cy) - 2*aa*(az*cx - ax*cz)*det)) + 
         ay*((ab*ac - aa*bc)*(-(az*cx) + ax*cz) - (ac*ay - aa*cy)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) + 
         (ay*(-(ab*ac) + aa*bc)*(az*cx - ax*cz) - 
            aa*(-2*ay*bc + ac*by + ab*cy)*(-(az*cx) + ax*cz) + 
            (ac*ay - aa*cy)*(aa*(bz*cx - bx*cz) + 
               Power(ay,2)*(bz*cx - bx*cz) + 
               ay*(-(az*by*cx) + az*bx*cy - ax*bz*cy + ax*by*cz)) - 
            aa*(ax*cx + az*cz)*det)*(Power(ab*ac - aa*bc,2) + aa*Power(det,2)))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[1][2][1][2] = (-(((ab*ac - aa*bc)*
               (aa*(bz*cx - bx*cz) + Power(ay,2)*(bz*cx - bx*cz) + 
                 ay*(-(az*by*cx) + az*bx*cy - ax*bz*cy + ax*by*cz)) + 
              aa*(2*ay*bc - ac*by - ab*cy)*det)*
            (2*(ab*ac - aa*bc)*(ac*az - aa*cz) + 2*aa*(ay*cx - ax*cy)*det)) + 
         ay*((ab*ac - aa*bc)*(ay*cx - ax*cy) - (ac*az - aa*cz)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) + 
         (-(aa*(-2*ay*bc + ac*by + ab*cy)*(ay*cx - ax*cy)) + 
            (ab*ac - aa*bc)*(aa*cx + ay*(ay*cx - ax*cy)) + 
            (-(ac*az) + aa*cz)*(aa*(-(bz*cx) + bx*cz) + 
               Power(ay,2)*(-(bz*cx) + bx*cz) + 
               ay*(az*by*cx - az*bx*cy + ax*bz*cy - ax*by*cz)) - 
            aa*(az*cy - 2*ay*cz)*det)*(Power(ab*ac - aa*bc,2) + aa*Power(det,2)))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[1][2][2][0] = (az*
          ((ab*ac - aa*bc)*(az*cy - ay*cz) - (ac*ax - aa*cx)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) - 
         (2*(ab*ac - aa*bc)*(ac*ax - aa*cx) + 2*aa*(az*cy - ay*cz)*det)*
          (aa*(2*az*bc - ac*bz - ab*cz)*det + 
            (-(ab*ac) + aa*bc)*(aa*(by*cx - bx*cy) - az*det)) + 
         (Power(ab*ac - aa*bc,2) + aa*Power(det,2))*
          (-(aa*(-2*az*bc + ac*bz + ab*cz)*(az*cy - ay*cz)) + 
            (ab*ac - aa*bc)*(aa*cy + az*(az*cy - ay*cz)) + 
            aa*(2*az*cx - ax*cz)*det + 
            (ac*ax - aa*cx)*(aa*(-(by*cx) + bx*cy) + az*det)))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[1][2][2][1] = (az*
          ((ab*ac - aa*bc)*(-(az*cx) + ax*cz) - (ac*ay - aa*cy)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) - 
         (2*(ab*ac - aa*bc)*(ac*ay - aa*cy) - 2*aa*(az*cx - ax*cz)*det)*
          (aa*(2*az*bc - ac*bz - ab*cz)*det + 
            (-(ab*ac) + aa*bc)*(aa*(by*cx - bx*cy) - az*det)) + 
         (Power(ab*ac - aa*bc,2) + aa*Power(det,2))*
          (-(aa*(-2*az*bc + ac*bz + ab*cz)*(-(az*cx) + ax*cz)) + 
            (-(ab*ac) + aa*bc)*(aa*cx + az*(az*cx - ax*cz)) + 
            aa*(2*az*cy - ay*cz)*det + 
            (ac*ay - aa*cy)*(aa*(-(by*cx) + bx*cy) + az*det)))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[1][2][2][2] = (az*
          ((ab*ac - aa*bc)*(ay*cx - ax*cy) - (ac*az - aa*cz)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) - 
         (2*(ab*ac - aa*bc)*(ac*az - aa*cz) + 2*aa*(ay*cx - ax*cy)*det)*
          (aa*(2*az*bc - ac*bz - ab*cz)*det + 
            (-(ab*ac) + aa*bc)*(aa*(by*cx - bx*cy) - az*det)) + 
         (Power(ab*ac - aa*bc,2) + aa*Power(det,2))*
          (az*(ab*ac - aa*bc)*(ay*cx - ax*cy) - 
            aa*(ay*cx - ax*cy)*(-2*az*bc + ac*bz + ab*cz) - 
            aa*(ax*cx + ay*cy)*det + 
            (-(ac*az) + aa*cz)*(aa*(by*cx - bx*cy) - az*det)))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[1][3][0][0] = (-((2*(ab*ac - aa*bc)*(ab*ax - aa*bx) - 
              2*aa*(az*by - ay*bz)*det)*
            ((ab*ac - aa*bc)*(ax*(-(az*by*cx) + ay*bz*cx + az*bx*cy - 
                    ay*bx*cz) + aa*(-(bz*cy) + by*cz) + 
                 Power(ax,2)*(-(bz*cy) + by*cz)) + 
              aa*(2*ax*bc - ac*bx - ab*cx)*det)) + 
         ax*((ab*ac - aa*bc)*(-(az*by) + ay*bz) - (ab*ax - aa*bx)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) + 
         (ax*(-(ab*ac) + aa*bc)*(az*by - ay*bz) - 
            aa*(-(az*by) + ay*bz)*(-2*ax*bc + ac*bx + ab*cx) + 
            (ab*ax - aa*bx)*(ax*(-(az*by*cx) + ay*bz*cx + az*bx*cy - 
                  ay*bx*cz) + aa*(-(bz*cy) + by*cz) + 
               Power(ax,2)*(-(bz*cy) + by*cz)) - aa*(ay*by + az*bz)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[1][3][0][1] = (-((2*(ab*ac - aa*bc)*(ab*ay - aa*by) + 
              2*aa*(az*bx - ax*bz)*det)*
            ((ab*ac - aa*bc)*(ax*(-(az*by*cx) + ay*bz*cx + az*bx*cy - 
                    ay*bx*cz) + aa*(-(bz*cy) + by*cz) + 
                 Power(ax,2)*(-(bz*cy) + by*cz)) + 
              aa*(2*ax*bc - ac*bx - ab*cx)*det)) + 
         ax*((ab*ac - aa*bc)*(az*bx - ax*bz) - (ab*ay - aa*by)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) + 
         ((-(ab*ac) + aa*bc)*(-(ax*az*bx) + aa*bz + Power(ax,2)*bz) - 
            aa*(az*bx - ax*bz)*(-2*ax*bc + ac*bx + ab*cx) + 
            (ab*ay - aa*by)*(ax*(-(az*by*cx) + ay*bz*cx + az*bx*cy - 
                  ay*bx*cz) + aa*(-(bz*cy) + by*cz) + 
               Power(ax,2)*(-(bz*cy) + by*cz)) - aa*(ay*bx - 2*ax*by)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[1][3][0][2] = (-((2*(ab*ac - aa*bc)*(ab*az - aa*bz) - 
              2*aa*(ay*bx - ax*by)*det)*
            ((ab*ac - aa*bc)*(ax*(-(az*by*cx) + ay*bz*cx + az*bx*cy - 
                    ay*bx*cz) + aa*(-(bz*cy) + by*cz) + 
                 Power(ax,2)*(-(bz*cy) + by*cz)) + 
              aa*(2*ax*bc - ac*bx - ab*cx)*det)) + 
         ax*((ab*ac - aa*bc)*(-(ay*bx) + ax*by) - (ab*az - aa*bz)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) + 
         ((ab*ac - aa*bc)*(-(ax*ay*bx) + aa*by + Power(ax,2)*by) + 
            aa*(ay*bx - ax*by)*(-2*ax*bc + ac*bx + ab*cx) + 
            (-(ab*az) + aa*bz)*(ax*
                (az*by*cx - ay*bz*cx - az*bx*cy + ay*bx*cz) + 
               aa*(bz*cy - by*cz) + Power(ax,2)*(bz*cy - by*cz)) - 
            aa*(az*bx - 2*ax*bz)*det)*(Power(ab*ac - aa*bc,2) + aa*Power(det,2)))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[1][3][1][0] = (-((2*(ab*ac - aa*bc)*(ab*ax - aa*bx) - 
              2*aa*(az*by - ay*bz)*det)*
            ((ab*ac - aa*bc)*(aa*(bz*cx - bx*cz) + 
                 Power(ay,2)*(bz*cx - bx*cz) + 
                 ay*(-(az*by*cx) + az*bx*cy - ax*bz*cy + ax*by*cz)) + 
              aa*(2*ay*bc - ac*by - ab*cy)*det)) + 
         ay*((ab*ac - aa*bc)*(-(az*by) + ay*bz) - (ab*ax - aa*bx)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) + 
         ((ab*ac - aa*bc)*(-(ay*az*by) + aa*bz + Power(ay,2)*bz) + 
            aa*(az*by - ay*bz)*(-2*ay*bc + ac*by + ab*cy) + 
            (ab*ax - aa*bx)*(aa*(bz*cx - bx*cz) + 
               Power(ay,2)*(bz*cx - bx*cz) + 
               ay*(-(az*by*cx) + az*bx*cy - ax*bz*cy + ax*by*cz)) + 
            aa*(2*ay*bx - ax*by)*det)*(Power(ab*ac - aa*bc,2) + aa*Power(det,2)))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[1][3][1][1] = (-((2*(ab*ac - aa*bc)*(ab*ay - aa*by) + 
              2*aa*(az*bx - ax*bz)*det)*
            ((ab*ac - aa*bc)*(aa*(bz*cx - bx*cz) + 
                 Power(ay,2)*(bz*cx - bx*cz) + 
                 ay*(-(az*by*cx) + az*bx*cy - ax*bz*cy + ax*by*cz)) + 
              aa*(2*ay*bc - ac*by - ab*cy)*det)) + 
         ay*((ab*ac - aa*bc)*(az*bx - ax*bz) - (ab*ay - aa*by)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) + 
         (ay*(ab*ac - aa*bc)*(az*bx - ax*bz) - 
            aa*(az*bx - ax*bz)*(-2*ay*bc + ac*by + ab*cy) + 
            (ab*ay - aa*by)*(aa*(bz*cx - bx*cz) + 
               Power(ay,2)*(bz*cx - bx*cz) + 
               ay*(-(az*by*cx) + az*bx*cy - ax*bz*cy + ax*by*cz)) - 
            aa*(ax*bx + az*bz)*det)*(Power(ab*ac - aa*bc,2) + aa*Power(det,2)))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[1][3][1][2] = (-((2*(ab*ac - aa*bc)*(ab*az - aa*bz) - 
              2*aa*(ay*bx - ax*by)*det)*
            ((ab*ac - aa*bc)*(aa*(bz*cx - bx*cz) + 
                 Power(ay,2)*(bz*cx - bx*cz) + 
                 ay*(-(az*by*cx) + az*bx*cy - ax*bz*cy + ax*by*cz)) + 
              aa*(2*ay*bc - ac*by - ab*cy)*det)) + 
         ay*((ab*ac - aa*bc)*(-(ay*bx) + ax*by) - (ab*az - aa*bz)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) + 
         ((-(ab*ac) + aa*bc)*(aa*bx + ay*(ay*bx - ax*by)) - 
            aa*(-(ay*bx) + ax*by)*(-2*ay*bc + ac*by + ab*cy) - 
            (ab*az - aa*bz)*(aa*(-(bz*cx) + bx*cz) + 
               Power(ay,2)*(-(bz*cx) + bx*cz) + 
               ay*(az*by*cx - az*bx*cy + ax*bz*cy - ax*by*cz)) - 
            aa*(az*by - 2*ay*bz)*det)*(Power(ab*ac - aa*bc,2) + aa*Power(det,2)))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[1][3][2][0] = (az*
          ((ab*ac - aa*bc)*(-(az*by) + ay*bz) - (ab*ax - aa*bx)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) - 
         (2*(ab*ac - aa*bc)*(ab*ax - aa*bx) - 2*aa*(az*by - ay*bz)*det)*
          (aa*(2*az*bc - ac*bz - ab*cz)*det + 
            (-(ab*ac) + aa*bc)*(aa*(by*cx - bx*cy) - az*det)) + 
         (Power(ab*ac - aa*bc,2) + aa*Power(det,2))*
          ((-(ab*ac) + aa*bc)*(aa*by + az*(az*by - ay*bz)) - 
            aa*(-(az*by) + ay*bz)*(-2*az*bc + ac*bz + ab*cz) + 
            aa*(2*az*bx - ax*bz)*det + 
            (ab*ax - aa*bx)*(aa*(-(by*cx) + bx*cy) + az*det)))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[1][3][2][1] = (az*
          ((ab*ac - aa*bc)*(az*bx - ax*bz) - (ab*ay - aa*by)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) - 
         (2*(ab*ac - aa*bc)*(ab*ay - aa*by) + 2*aa*(az*bx - ax*bz)*det)*
          (aa*(2*az*bc - ac*bz - ab*cz)*det + 
            (-(ab*ac) + aa*bc)*(aa*(by*cx - bx*cy) - az*det)) + 
         (Power(ab*ac - aa*bc,2) + aa*Power(det,2))*
          ((ab*ac - aa*bc)*(aa*bx + az*(az*bx - ax*bz)) - 
            aa*(az*bx - ax*bz)*(-2*az*bc + ac*bz + ab*cz) + 
            aa*(2*az*by - ay*bz)*det + 
            (ab*ay - aa*by)*(aa*(-(by*cx) + bx*cy) + az*det)))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[1][3][2][2] = (az*
          ((ab*ac - aa*bc)*(-(ay*bx) + ax*by) - (ab*az - aa*bz)*det)*
          (Power(ab*ac - aa*bc,2) + aa*Power(det,2)) - 
         (2*(ab*ac - aa*bc)*(ab*az - aa*bz) - 2*aa*(ay*bx - ax*by)*det)*
          (aa*(2*az*bc - ac*bz - ab*cz)*det + 
            (-(ab*ac) + aa*bc)*(aa*(by*cx - bx*cy) - az*det)) + 
         (Power(ab*ac - aa*bc,2) + aa*Power(det,2))*
          (az*(-(ab*ac) + aa*bc)*(ay*bx - ax*by) - 
            aa*(-(ay*bx) + ax*by)*(-2*az*bc + ac*bz + ab*cz) - 
            aa*(ax*bx + ay*by)*det + 
            (-(ab*az) + aa*bz)*(aa*(by*cx - bx*cy) - az*det)))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[2][2][0][0] = -((aa*
           ((ab*ac - aa*bc)*(az*cy - ay*cz) - (ac*ax - aa*cx)*det)*
           (2*(ab*ac - aa*bc)*(ac*ax - aa*cx) + 2*aa*(az*cy - ay*cz)*det))/
         Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2));
    e_info->hess[2][2][0][1] = (aa*
         (-(((ab*ac - aa*bc)*(az*cy - ay*cz) - (ac*ax - aa*cx)*det)*
              (2*(ab*ac - aa*bc)*(ac*ay - aa*cy) - 2*aa*(az*cx - ax*cz)*det)) + 
           (-((ac*ax - aa*cx)*(-(az*cx) + ax*cz)) + 
              (ac*ay - aa*cy)*(az*cy - ay*cz))*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[2][2][0][2] = (aa*
         (-(((ab*ac - aa*bc)*(az*cy - ay*cz) - (ac*ax - aa*cx)*det)*
              (2*(ab*ac - aa*bc)*(ac*az - aa*cz) + 2*aa*(ay*cx - ax*cy)*det)) + 
           ((ac*ax - aa*cx)*(-(ay*cx) + ax*cy) + 
              (ac*az - aa*cz)*(az*cy - ay*cz))*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[2][2][1][0] = (aa*
         (-(((ab*ac - aa*bc)*(-(az*cx) + ax*cz) - (ac*ay - aa*cy)*det)*
              (2*(ab*ac - aa*bc)*(ac*ax - aa*cx) + 2*aa*(az*cy - ay*cz)*det)) + 
           ((ac*ax - aa*cx)*(-(az*cx) + ax*cz) + 
              (ac*ay - aa*cy)*(-(az*cy) + ay*cz))*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[2][2][1][1] = -((aa*
           ((ab*ac - aa*bc)*(-(az*cx) + ax*cz) - (ac*ay - aa*cy)*det)*
           (2*(ab*ac - aa*bc)*(ac*ay - aa*cy) - 2*aa*(az*cx - ax*cz)*det))/
         Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2));
    e_info->hess[2][2][1][2] = (aa*
         (-(((ab*ac - aa*bc)*(-(az*cx) + ax*cz) - (ac*ay - aa*cy)*det)*
              (2*(ab*ac - aa*bc)*(ac*az - aa*cz) + 2*aa*(ay*cx - ax*cy)*det)) + 
           (-((ac*ay - aa*cy)*(ay*cx - ax*cy)) + 
              (ac*az - aa*cz)*(-(az*cx) + ax*cz))*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[2][2][2][0] = (aa*
         (-(((ab*ac - aa*bc)*(ay*cx - ax*cy) - (ac*az - aa*cz)*det)*
              (2*(ab*ac - aa*bc)*(ac*ax - aa*cx) + 2*aa*(az*cy - ay*cz)*det)) + 
           ((ac*ax - aa*cx)*(ay*cx - ax*cy) - (ac*az - aa*cz)*(az*cy - ay*cz))*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[2][2][2][1] = (aa*
         (-(((ab*ac - aa*bc)*(ay*cx - ax*cy) - (ac*az - aa*cz)*det)*
              (2*(ab*ac - aa*bc)*(ac*ay - aa*cy) - 2*aa*(az*cx - ax*cz)*det)) + 
           ((ac*ay - aa*cy)*(ay*cx - ax*cy) + (ac*az - aa*cz)*(az*cx - ax*cz))*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[2][2][2][2] = -((aa*
           (2*(ab*ac - aa*bc)*(ac*az - aa*cz) + 2*aa*(ay*cx - ax*cy)*det)*
           ((ab*ac - aa*bc)*(ay*cx - ax*cy) - (ac*az - aa*cz)*det))/
         Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2));
    e_info->hess[2][3][0][0] = (aa*
         (-((2*(ab*ac - aa*bc)*(ab*ax - aa*bx) - 2*aa*(az*by - ay*bz)*det)*
              ((ab*ac - aa*bc)*(az*cy - ay*cz) - (ac*ax - aa*cx)*det)) + 
           ((az*by - ay*bz)*(ac*ax - aa*cx) + (ab*ax - aa*bx)*(az*cy - ay*cz) + 
              (Power(ay,2) + Power(az,2))*det)*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[2][3][0][1] = (aa*
         (-((2*(ab*ac - aa*bc)*(ab*ay - aa*by) + 2*aa*(az*bx - ax*bz)*det)*
              ((ab*ac - aa*bc)*(az*cy - ay*cz) - (ac*ax - aa*cx)*det)) + 
           (az*(ab*ac - aa*bc) - (az*bx - ax*bz)*(ac*ax - aa*cx) + 
              (ab*ay - aa*by)*(az*cy - ay*cz) - ax*ay*det)*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[2][3][0][2] = (aa*
         (-((2*(ab*ac - aa*bc)*(ab*az - aa*bz) - 2*aa*(ay*bx - ax*by)*det)*
              ((ab*ac - aa*bc)*(az*cy - ay*cz) - (ac*ax - aa*cx)*det)) + 
           (-(ab*ac*ay) + aa*ay*bc + (ay*bx - ax*by)*(ac*ax - aa*cx) + 
              (ab*az - aa*bz)*(az*cy - ay*cz) - ax*az*det)*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[2][3][1][0] = (aa*
         (-((2*(ab*ac - aa*bc)*(ab*ax - aa*bx) - 2*aa*(az*by - ay*bz)*det)*
              ((ab*ac - aa*bc)*(-(az*cx) + ax*cz) - (ac*ay - aa*cy)*det)) + 
           (-(ab*ac*az) + aa*az*bc + (az*by - ay*bz)*(ac*ay - aa*cy) + 
              (ab*ax - aa*bx)*(-(az*cx) + ax*cz) - ax*ay*det)*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[2][3][1][1] = (aa*
         (-((2*(ab*ac - aa*bc)*(ab*ay - aa*by) + 2*aa*(az*bx - ax*bz)*det)*
              ((ab*ac - aa*bc)*(-(az*cx) + ax*cz) - (ac*ay - aa*cy)*det)) + 
           (-((az*bx - ax*bz)*(ac*ay - aa*cy)) + 
              (ab*ay - aa*by)*(-(az*cx) + ax*cz) + 
              (Power(ax,2) + Power(az,2))*det)*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[2][3][1][2] = (aa*
         (-((2*(ab*ac - aa*bc)*(ab*az - aa*bz) - 2*aa*(ay*bx - ax*by)*det)*
              ((ab*ac - aa*bc)*(-(az*cx) + ax*cz) - (ac*ay - aa*cy)*det)) + 
           (ax*(ab*ac - aa*bc) + (ay*bx - ax*by)*(ac*ay - aa*cy) + 
              (ab*az - aa*bz)*(-(az*cx) + ax*cz) - ay*az*det)*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[2][3][2][0] = (aa*
         (-((2*(ab*ac - aa*bc)*(ab*ax - aa*bx) - 2*aa*(az*by - ay*bz)*det)*
              ((ab*ac - aa*bc)*(ay*cx - ax*cy) - (ac*az - aa*cz)*det)) + 
           (ay*(ab*ac - aa*bc) + (ab*ax - aa*bx)*(ay*cx - ax*cy) + 
              (az*by - ay*bz)*(ac*az - aa*cz) - ax*az*det)*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[2][3][2][1] = (aa*
         (-((2*(ab*ac - aa*bc)*(ab*ay - aa*by) + 2*aa*(az*bx - ax*bz)*det)*
              ((ab*ac - aa*bc)*(ay*cx - ax*cy) - (ac*az - aa*cz)*det)) + 
           (-(ab*ac*ax) + aa*ax*bc + (ab*ay - aa*by)*(ay*cx - ax*cy) - 
              (az*bx - ax*bz)*(ac*az - aa*cz) - ay*az*det)*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[2][3][2][2] = (aa*
         (-((2*(ab*ac - aa*bc)*(ab*az - aa*bz) - 2*aa*(ay*bx - ax*by)*det)*
              ((ab*ac - aa*bc)*(ay*cx - ax*cy) - (ac*az - aa*cz)*det)) + 
           ((ab*az - aa*bz)*(ay*cx - ax*cy) + (ay*bx - ax*by)*(ac*az - aa*cz) + 
              (Power(ax,2) + Power(ay,2))*det)*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[3][3][0][0] = -((aa*
           ((ab*ac - aa*bc)*(-(az*by) + ay*bz) - (ab*ax - aa*bx)*det)*
           (2*(ab*ac - aa*bc)*(ab*ax - aa*bx) - 2*aa*(az*by - ay*bz)*det))/
         Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2));
    e_info->hess[3][3][0][1] = (aa*
         (-(((ab*ac - aa*bc)*(-(az*by) + ay*bz) - (ab*ax - aa*bx)*det)*
              (2*(ab*ac - aa*bc)*(ab*ay - aa*by) + 2*aa*(az*bx - ax*bz)*det)) + 
           ((ab*ax - aa*bx)*(-(az*bx) + ax*bz) + 
              (ab*ay - aa*by)*(-(az*by) + ay*bz))*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[3][3][0][2] = (aa*
         (-(((ab*ac - aa*bc)*(-(az*by) + ay*bz) - (ab*ax - aa*bx)*det)*
              (2*(ab*ac - aa*bc)*(ab*az - aa*bz) - 2*aa*(ay*bx - ax*by)*det)) + 
           (-((ab*ax - aa*bx)*(-(ay*bx) + ax*by)) + 
              (ab*az - aa*bz)*(-(az*by) + ay*bz))*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[3][3][1][0] = (aa*
         (-(((ab*ac - aa*bc)*(az*bx - ax*bz) - (ab*ay - aa*by)*det)*
              (2*(ab*ac - aa*bc)*(ab*ax - aa*bx) - 2*aa*(az*by - ay*bz)*det)) + 
           ((ab*ax - aa*bx)*(az*bx - ax*bz) - 
              (ab*ay - aa*by)*(-(az*by) + ay*bz))*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[3][3][1][1] = -((aa*
           ((ab*ac - aa*bc)*(az*bx - ax*bz) - (ab*ay - aa*by)*det)*
           (2*(ab*ac - aa*bc)*(ab*ay - aa*by) + 2*aa*(az*bx - ax*bz)*det))/
         Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2));
    e_info->hess[3][3][1][2] = (aa*
         (-(((ab*ac - aa*bc)*(az*bx - ax*bz) - (ab*ay - aa*by)*det)*
              (2*(ab*ac - aa*bc)*(ab*az - aa*bz) - 2*aa*(ay*bx - ax*by)*det)) + 
           ((ab*ay - aa*by)*(ay*bx - ax*by) + (ab*az - aa*bz)*(az*bx - ax*bz))*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[3][3][2][0] = (aa*
         (-(((ab*ac - aa*bc)*(-(ay*bx) + ax*by) - (ab*az - aa*bz)*det)*
              (2*(ab*ac - aa*bc)*(ab*ax - aa*bx) - 2*aa*(az*by - ay*bz)*det)) + 
           ((ab*ax - aa*bx)*(-(ay*bx) + ax*by) + 
              (ab*az - aa*bz)*(az*by - ay*bz))*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[3][3][2][1] = (aa*
         (-(((ab*ac - aa*bc)*(-(ay*bx) + ax*by) - (ab*az - aa*bz)*det)*
              (2*(ab*ac - aa*bc)*(ab*ay - aa*by) + 2*aa*(az*bx - ax*bz)*det)) + 
           ((ab*ay - aa*by)*(-(ay*bx) + ax*by) - 
              (ab*az - aa*bz)*(az*bx - ax*bz))*
            (Power(ab*ac - aa*bc,2) + aa*Power(det,2))))/
       Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2);
    e_info->hess[3][3][2][2] = -((aa*
           (2*(ab*ac - aa*bc)*(ab*az - aa*bz) - 2*aa*(ay*bx - ax*by)*det)*
           ((ab*ac - aa*bc)*(-(ay*bx) + ax*by) - (ab*az - aa*bz)*det))/
         Power(Power(ab*ac - aa*bc,2) + aa*Power(det,2),2));

  /* take care of some fudge factors, including orientation */
  for ( i = 1 ; i < 4 ; i++ )
    for ( j = 1 ; j <= i ; j++ )
      for ( ii = 0 ; ii < SDIM ; ii++ )
        for ( jj = 0 ; jj < SDIM ; jj++ )
          e_info->hess[j][i][jj][ii] *= 0.5*sign;

  /* Fill in rest of Hessian by symmetry */
  for ( i = 1 ; i < 4 ; i++ )
    for ( j = i+1 ; j < 4 ; j++ )
      for ( ii = 0 ; ii < SDIM ; ii++ )
        for ( jj = 0 ; jj < SDIM ; jj++ )
          e_info->hess[j][i][jj][ii] = e_info->hess[i][j][ii][jj];

  for ( i = 1 ; i < 4 ; i++ )
    for ( j = 1 ; j < 4 ; j++ )
      for ( ii = 0 ; ii < SDIM ; ii++ )
        for ( jj = 0 ; jj < SDIM ; jj++ )
        { e_info->hess[0][0][ii][jj] += e_info->hess[i][j][ii][jj];
          e_info->hess[0][j][ii][jj] -= e_info->hess[i][j][ii][jj];
          e_info->hess[i][0][ii][jj] -= e_info->hess[i][j][ii][jj];
        }
  
  return sign*theta*len/2;

}

/**************************************************************************

   mean_curvature_integral_a   Same, but using tangent of dihedral angle
   for more stability.  Using sine actually better approximates
   smoothed mean curvature on a sphere, but seems wildly unstable.
   Tangent is much more stable, but has its own cautions:
   1. Use only for small angles.  Tan(pi/2) = infinity, which doesn't work well.
   2. Possible favors uneven triangulation.  At large refinements on sphere, 
      translation eigenvalues can become negative.  Use translation-suppressing
      center-of-mass constraints?

***************************************************************************/

/*********************************************************************
*
* function: mean_int_a_init()
*
* purpose:  Check illegalities
*
*/

void mean_int_a_init(mode,mi)
int mode;
struct method_instance *mi;
{
  if ( web.dimension != 2 )
     kb_error(4594,"mean_curvature_integral_a method only for 2D facets.\n",RECOVERABLE);

  if ( web.modeltype != LINEAR )
     kb_error(4595,"mean_curvature_integral_a method only for LINEAR model.\n",RECOVERABLE);

}

/***************************************************************************
*  
*  Function: mean_int_a_all()
*
*  Purpose:  Computes contribution of one edge for value, grad, and hessian. 
*/
REAL mean_int_a_all(e_info,mode)
struct qinfo *e_info;
int mode; /* METHOD_VALUE, METHOD_GRADIEN, METHOD_HESSIAN */
{ REAL vol;
  int i,j,ii,jj;
  int sign = inverted(get_fe_facet(get_edge_fe(e_info->id))) ? 1 : -1;
  REAL ax,ay,az,bx,by,bz,cx,cy,cz;
  REAL aa,ab,ac,bb,bc,cc,a,aaa,det;
  REAL value;

  /* same value and gradient calculation as mean_int_gradient() */
  mat_tsquare(e_info->sides[0],e_info->ss,3,SDIM); /* side products */
  vol = triple_prod(e_info->sides[0][0],e_info->sides[0][1],e_info->sides[0][2]);
 /* if ( vol == 0.0 )  return 0.0; */
  

  /* Now Hessian, as ground out by Mathematica, in meanint.m */
  ax = e_info->sides[0][0][0];
  ay = e_info->sides[0][0][1];
  az = e_info->sides[0][0][2];
  bx = e_info->sides[0][1][0];
  by = e_info->sides[0][1][1];
  bz = e_info->sides[0][1][2];
  cx = e_info->sides[0][2][0];
  cy = e_info->sides[0][2][1];
  cz = e_info->sides[0][2][2];
  aa = e_info->ss[0][0];
  bb = e_info->ss[1][1];
  cc = e_info->ss[2][2];
  ab = e_info->ss[0][1];
  ac = e_info->ss[0][2];
  bc = e_info->ss[1][2];
  a = sqrt(aa);
  aaa = a*aa;
  det = vol;

  /* Copied from Mathematica */
#undef  Power
#define Power(a,b) ((b==2)?(a)*(a):(a)*(a)*(a))

    value = -((aa*det)/(-(ab*ac) + aa*bc))*sign/2.0;

  if ( mode == METHOD_VALUE ) 
    return value;

    
    e_info->grad[1][0] = (aa*(-(ab*ac) + aa*bc)*(bz*cy - by*cz) + 
         2*ax*(ab*ac - aa*bc)*det + aa*(2*ax*bc - ac*bx - ab*cx)*det)/
       Power(ab*ac - aa*bc,2);
    e_info->grad[1][1] = (aa*(ab*ac - aa*bc)*(bz*cx - bx*cz) + 
         2*ay*(ab*ac - aa*bc)*det + aa*(2*ay*bc - ac*by - ab*cy)*det)/
       Power(ab*ac - aa*bc,2);
    e_info->grad[1][2] = (aa*(-(ab*ac) + aa*bc)*(by*cx - bx*cy) + 
         2*az*(ab*ac - aa*bc)*det + aa*(2*az*bc - ac*bz - ab*cz)*det)/
       Power(ab*ac - aa*bc,2);
    e_info->grad[2][0] = (aa*((ab*ac - aa*bc)*(az*cy - ay*cz) - 
           (ac*ax - aa*cx)*det))/Power(ab*ac - aa*bc,2);
    e_info->grad[2][1] = (aa*((ab*ac - aa*bc)*(-(az*cx) + ax*cz) - 
           (ac*ay - aa*cy)*det))/Power(ab*ac - aa*bc,2);
    e_info->grad[2][2] = (aa*((ab*ac - aa*bc)*(ay*cx - ax*cy) - 
           (ac*az - aa*cz)*det))/Power(ab*ac - aa*bc,2);
    e_info->grad[3][0] = (aa*((ab*ac - aa*bc)*(-(az*by) + ay*bz) - 
           (ab*ax - aa*bx)*det))/Power(ab*ac - aa*bc,2);
    e_info->grad[3][1] = (aa*((ab*ac - aa*bc)*(az*bx - ax*bz) - 
           (ab*ay - aa*by)*det))/Power(ab*ac - aa*bc,2);
    e_info->grad[3][2] = (aa*((ab*ac - aa*bc)*(-(ay*bx) + ax*by) - 
           (ab*az - aa*bz)*det))/Power(ab*ac - aa*bc,2);



  /* take care of some fudge factors, including orientation */
  for ( i = 1 ; i < 4 ; i++ )
      for ( ii = 0 ; ii < SDIM ; ii++ )
          e_info->grad[i][ii] *= 0.5*sign;

  /* set gradient of base vertex */
  for ( i = 1 ; i < 4 ; i++ )
    for ( ii = 0 ; ii < SDIM ; ii++ )
     e_info->grad[0][ii] -= e_info->grad[i][ii];

  if ( mode == METHOD_GRADIENT ) 
    return value;



    e_info->hess[1][1][0][0] = (-2*aa*(-(ab*ac) + aa*bc)*
          (-2*ax*bc + ac*bx + ab*cx)*(bz*cy - by*cz) + 
         4*ax*Power(ab*ac - aa*bc,2)*(-(bz*cy) + by*cz) + 
         2*Power(ab*ac - aa*bc,2)*det + 
         4*ax*(ab*ac - aa*bc)*(2*ax*bc - ac*bx - ab*cx)*det + 
         2*aa*Power(-2*ax*bc + ac*bx + ab*cx,2)*det - 
         2*aa*(-(ab*ac) + aa*bc)*(bc - bx*cx)*det)/Power(ab*ac - aa*bc,3);
    e_info->hess[1][1][0][1] = (2*ax*Power(ab*ac - aa*bc,2)*(bz*cx - bx*cz) - 
         aa*(-(ab*ac) + aa*bc)*(-2*ax*bc + ac*bx + ab*cx)*(-(bz*cx) + bx*cz) + 
         2*ay*Power(ab*ac - aa*bc,2)*(-(bz*cy) + by*cz) + 
         aa*(-(ab*ac) + aa*bc)*(-2*ay*bc + ac*by + ab*cy)*(-(bz*cy) + by*cz) - 
         2*ay*(ab*ac - aa*bc)*(-2*ax*bc + ac*bx + ab*cx)*det - 
         2*ax*(ab*ac - aa*bc)*(-2*ay*bc + ac*by + ab*cy)*det + 
         2*aa*(-2*ax*bc + ac*bx + ab*cx)*(-2*ay*bc + ac*by + ab*cy)*det + 
         aa*(-(ab*ac) + aa*bc)*(by*cx + bx*cy)*det)/Power(ab*ac - aa*bc,3);
    e_info->hess[1][1][0][2] = (2*ax*Power(ab*ac - aa*bc,2)*
          (-(by*cx) + bx*cy) + aa*(-(ab*ac) + aa*bc)*(-2*ax*bc + ac*bx + ab*cx)*
          (-(by*cx) + bx*cy) - aa*(-(ab*ac) + aa*bc)*(-2*az*bc + ac*bz + ab*cz)*
          (bz*cy - by*cz) + 2*az*Power(ab*ac - aa*bc,2)*(-(bz*cy) + by*cz) - 
         2*az*(ab*ac - aa*bc)*(-2*ax*bc + ac*bx + ab*cx)*det - 
         2*ax*(ab*ac - aa*bc)*(-2*az*bc + ac*bz + ab*cz)*det + 
         2*aa*(-2*ax*bc + ac*bx + ab*cx)*(-2*az*bc + ac*bz + ab*cz)*det + 
         aa*(-(ab*ac) + aa*bc)*(bz*cx + bx*cz)*det)/Power(ab*ac - aa*bc,3);
    e_info->hess[1][1][1][0] = (2*ax*Power(ab*ac - aa*bc,2)*(bz*cx - bx*cz) - 
         aa*(-(ab*ac) + aa*bc)*(-2*ax*bc + ac*bx + ab*cx)*(-(bz*cx) + bx*cz) + 
         2*ay*Power(ab*ac - aa*bc,2)*(-(bz*cy) + by*cz) + 
         aa*(-(ab*ac) + aa*bc)*(-2*ay*bc + ac*by + ab*cy)*(-(bz*cy) + by*cz) - 
         2*ay*(ab*ac - aa*bc)*(-2*ax*bc + ac*bx + ab*cx)*det - 
         2*ax*(ab*ac - aa*bc)*(-2*ay*bc + ac*by + ab*cy)*det + 
         2*aa*(-2*ax*bc + ac*bx + ab*cx)*(-2*ay*bc + ac*by + ab*cy)*det + 
         aa*(-(ab*ac) + aa*bc)*(by*cx + bx*cy)*det)/Power(ab*ac - aa*bc,3);
    e_info->hess[1][1][1][1] = (2*
         (2*ay*Power(ab*ac - aa*bc,2)*(bz*cx - bx*cz) + 
           aa*(-(ab*ac) + aa*bc)*(-2*ay*bc + ac*by + ab*cy)*(bz*cx - bx*cz) + 
           Power(ab*ac - aa*bc,2)*det + 
           2*ay*(ab*ac - aa*bc)*(2*ay*bc - ac*by - ab*cy)*det + 
           aa*Power(-2*ay*bc + ac*by + ab*cy,2)*det - 
           aa*(-(ab*ac) + aa*bc)*(bc - by*cy)*det))/Power(ab*ac - aa*bc,3);
    e_info->hess[1][1][1][2] = (-(aa*(-(ab*ac) + aa*bc)*
            (-2*ay*bc + ac*by + ab*cy)*(by*cx - bx*cy)) + 
         2*ay*Power(ab*ac - aa*bc,2)*(-(by*cx) + bx*cy) + 
         2*az*Power(ab*ac - aa*bc,2)*(bz*cx - bx*cz) + 
         aa*(-(ab*ac) + aa*bc)*(-2*az*bc + ac*bz + ab*cz)*(bz*cx - bx*cz) - 
         2*az*(ab*ac - aa*bc)*(-2*ay*bc + ac*by + ab*cy)*det - 
         2*ay*(ab*ac - aa*bc)*(-2*az*bc + ac*bz + ab*cz)*det + 
         2*aa*(-2*ay*bc + ac*by + ab*cy)*(-2*az*bc + ac*bz + ab*cz)*det + 
         aa*(-(ab*ac) + aa*bc)*(bz*cy + by*cz)*det)/Power(ab*ac - aa*bc,3);
    e_info->hess[1][1][2][0] = (2*ax*Power(ab*ac - aa*bc,2)*
          (-(by*cx) + bx*cy) + aa*(-(ab*ac) + aa*bc)*(-2*ax*bc + ac*bx + ab*cx)*
          (-(by*cx) + bx*cy) - aa*(-(ab*ac) + aa*bc)*(-2*az*bc + ac*bz + ab*cz)*
          (bz*cy - by*cz) + 2*az*Power(ab*ac - aa*bc,2)*(-(bz*cy) + by*cz) - 
         2*az*(ab*ac - aa*bc)*(-2*ax*bc + ac*bx + ab*cx)*det - 
         2*ax*(ab*ac - aa*bc)*(-2*az*bc + ac*bz + ab*cz)*det + 
         2*aa*(-2*ax*bc + ac*bx + ab*cx)*(-2*az*bc + ac*bz + ab*cz)*det + 
         aa*(-(ab*ac) + aa*bc)*(bz*cx + bx*cz)*det)/Power(ab*ac - aa*bc,3);
    e_info->hess[1][1][2][1] = (-(aa*(-(ab*ac) + aa*bc)*
            (-2*ay*bc + ac*by + ab*cy)*(by*cx - bx*cy)) + 
         2*ay*Power(ab*ac - aa*bc,2)*(-(by*cx) + bx*cy) + 
         2*az*Power(ab*ac - aa*bc,2)*(bz*cx - bx*cz) + 
         aa*(-(ab*ac) + aa*bc)*(-2*az*bc + ac*bz + ab*cz)*(bz*cx - bx*cz) - 
         2*az*(ab*ac - aa*bc)*(-2*ay*bc + ac*by + ab*cy)*det - 
         2*ay*(ab*ac - aa*bc)*(-2*az*bc + ac*bz + ab*cz)*det + 
         2*aa*(-2*ay*bc + ac*by + ab*cy)*(-2*az*bc + ac*bz + ab*cz)*det + 
         aa*(-(ab*ac) + aa*bc)*(bz*cy + by*cz)*det)/Power(ab*ac - aa*bc,3);
    e_info->hess[1][1][2][2] = (4*az*Power(ab*ac - aa*bc,2)*
          (-(by*cx) + bx*cy) - 2*aa*(-(ab*ac) + aa*bc)*(by*cx - bx*cy)*
          (-2*az*bc + ac*bz + ab*cz) + 2*Power(ab*ac - aa*bc,2)*det + 
         4*az*(ab*ac - aa*bc)*(2*az*bc - ac*bz - ab*cz)*det + 
         2*aa*Power(-2*az*bc + ac*bz + ab*cz,2)*det - 
         2*aa*(-(ab*ac) + aa*bc)*(bc - bz*cz)*det)/Power(ab*ac - aa*bc,3);
    e_info->hess[1][2][0][0] = (2*ax*Power(ab*ac - aa*bc,2)*(az*cy - ay*cz) + 
         aa*(-(ab*ac) + aa*bc)*(-2*ax*bc + ac*bx + ab*cx)*(az*cy - ay*cz) + 
         aa*(-(ab*ac) + aa*bc)*(-(ac*ax) + aa*cx)*(bz*cy - by*cz) - 
         2*ax*(ab*ac - aa*bc)*(ac*ax - aa*cx)*det - 
         2*aa*(-(ac*ax) + aa*cx)*(-2*ax*bc + ac*bx + ab*cx)*det + 
         aa*(-(ab*ac) + aa*bc)*(ay*cy + az*cz)*det)/Power(ab*ac - aa*bc,3);
    e_info->hess[1][2][0][1] = (aa*Power(ab*ac - aa*bc,2)*cz + 
         2*ax*Power(ab*ac - aa*bc,2)*(-(az*cx) + ax*cz) + 
         aa*(-(ab*ac) + aa*bc)*(-2*ax*bc + ac*bx + ab*cx)*(-(az*cx) + ax*cz) + 
         aa*(-(ab*ac) + aa*bc)*(-(ac*ay) + aa*cy)*(bz*cy - by*cz) - 
         2*ax*(ab*ac - aa*bc)*(ac*ay - aa*cy)*det + 
         2*aa*(-2*ax*bc + ac*bx + ab*cx)*(ac*ay - aa*cy)*det + 
         aa*(-(ab*ac) + aa*bc)*(ay*cx - 2*ax*cy)*det)/Power(ab*ac - aa*bc,3);
    e_info->hess[1][2][0][2] = -((aa*Power(ab*ac - aa*bc,2)*cy + 
           2*ax*Power(ab*ac - aa*bc,2)*(-(ay*cx) + ax*cy) + 
           aa*(-(ab*ac) + aa*bc)*(-2*ax*bc + ac*bx + ab*cx)*(-(ay*cx) + ax*cy) + 
           aa*(-(ab*ac) + aa*bc)*(-(ac*az) + aa*cz)*(-(bz*cy) + by*cz) + 
           2*ax*(ab*ac - aa*bc)*(ac*az - aa*cz)*det - 
           2*aa*(2*ax*bc - ac*bx - ab*cx)*(-(ac*az) + aa*cz)*det - 
           aa*(-(ab*ac) + aa*bc)*(az*cx - 2*ax*cz)*det)/Power(ab*ac - aa*bc,3));
    e_info->hess[1][2][1][0] = -((aa*Power(ab*ac - aa*bc,2)*cz + 
           2*ay*Power(ab*ac - aa*bc,2)*(-(az*cy) + ay*cz) + 
           aa*(-(ab*ac) + aa*bc)*(-2*ay*bc + ac*by + ab*cy)*(-(az*cy) + ay*cz) + 
           aa*(-(ab*ac) + aa*bc)*(-(ac*ax) + aa*cx)*(bz*cx - bx*cz) + 
           2*ay*(ab*ac - aa*bc)*(ac*ax - aa*cx)*det + 
           2*aa*(-(ac*ax) + aa*cx)*(-2*ay*bc + ac*by + ab*cy)*det + 
           aa*(-(ab*ac) + aa*bc)*(2*ay*cx - ax*cy)*det)/Power(ab*ac - aa*bc,3));
    e_info->hess[1][2][1][1] = -((2*ay*Power(ab*ac - aa*bc,2)*
            (az*cx - ax*cz) + aa*(-(ab*ac) + aa*bc)*(-2*ay*bc + ac*by + ab*cy)*
            (az*cx - ax*cz) + aa*(-(ab*ac) + aa*bc)*(-(ac*ay) + aa*cy)*
            (bz*cx - bx*cz) + 2*ay*(ab*ac - aa*bc)*(ac*ay - aa*cy)*det + 
           2*aa*(-(ac*ay) + aa*cy)*(-2*ay*bc + ac*by + ab*cy)*det - 
           aa*(-(ab*ac) + aa*bc)*(ax*cx + az*cz)*det)/Power(ab*ac - aa*bc,3));
    e_info->hess[1][2][1][2] = (aa*Power(ab*ac - aa*bc,2)*cx + 
         2*ay*Power(ab*ac - aa*bc,2)*(ay*cx - ax*cy) + 
         aa*(-(ab*ac) + aa*bc)*(-2*ay*bc + ac*by + ab*cy)*(ay*cx - ax*cy) + 
         aa*(-(ab*ac) + aa*bc)*(-(ac*az) + aa*cz)*(-(bz*cx) + bx*cz) - 
         2*ay*(ab*ac - aa*bc)*(ac*az - aa*cz)*det + 
         2*aa*(-2*ay*bc + ac*by + ab*cy)*(ac*az - aa*cz)*det + 
         aa*(-(ab*ac) + aa*bc)*(az*cy - 2*ay*cz)*det)/Power(ab*ac - aa*bc,3);
    e_info->hess[1][2][2][0] = (aa*Power(ab*ac - aa*bc,2)*cy + 
         aa*(-(ab*ac) + aa*bc)*(-(ac*ax) + aa*cx)*(by*cx - bx*cy) + 
         2*az*Power(ab*ac - aa*bc,2)*(az*cy - ay*cz) + 
         aa*(-(ab*ac) + aa*bc)*(-2*az*bc + ac*bz + ab*cz)*(az*cy - ay*cz) - 
         2*az*(ab*ac - aa*bc)*(ac*ax - aa*cx)*det - 
         2*aa*(-(ac*ax) + aa*cx)*(-2*az*bc + ac*bz + ab*cz)*det - 
         aa*(-(ab*ac) + aa*bc)*(2*az*cx - ax*cz)*det)/Power(ab*ac - aa*bc,3);
    e_info->hess[1][2][2][1] = -((aa*Power(ab*ac - aa*bc,2)*cx + 
           aa*(-(ab*ac) + aa*bc)*(-(ac*ay) + aa*cy)*(-(by*cx) + bx*cy) + 
           2*az*Power(ab*ac - aa*bc,2)*(az*cx - ax*cz) + 
           aa*(-(ab*ac) + aa*bc)*(-2*az*bc + ac*bz + ab*cz)*(az*cx - ax*cz) + 
           2*az*(ab*ac - aa*bc)*(ac*ay - aa*cy)*det + 
           2*aa*(-(ac*ay) + aa*cy)*(-2*az*bc + ac*bz + ab*cz)*det + 
           aa*(-(ab*ac) + aa*bc)*(2*az*cy - ay*cz)*det)/Power(ab*ac - aa*bc,3));
    e_info->hess[1][2][2][2] = (2*az*Power(ab*ac - aa*bc,2)*(ay*cx - ax*cy) + 
         aa*(-(ab*ac) + aa*bc)*(by*cx - bx*cy)*(-(ac*az) + aa*cz) + 
         aa*(-(ab*ac) + aa*bc)*(ay*cx - ax*cy)*(-2*az*bc + ac*bz + ab*cz) + 
         aa*(-(ab*ac) + aa*bc)*(ax*cx + ay*cy)*det - 
         2*az*(ab*ac - aa*bc)*(ac*az - aa*cz)*det - 
         2*aa*(-(ac*az) + aa*cz)*(-2*az*bc + ac*bz + ab*cz)*det)/
       Power(ab*ac - aa*bc,3);
    e_info->hess[1][3][0][0] = (2*ax*Power(ab*ac - aa*bc,2)*
          (-(az*by) + ay*bz) - aa*(-(ab*ac) + aa*bc)*(az*by - ay*bz)*
          (-2*ax*bc + ac*bx + ab*cx) + 
         aa*(-(ab*ac) + aa*bc)*(-(ab*ax) + aa*bx)*(bz*cy - by*cz) - 
         2*ax*(ab*ac - aa*bc)*(ab*ax - aa*bx)*det + 
         aa*(-(ab*ac) + aa*bc)*(ay*by + az*bz)*det - 
         2*aa*(-(ab*ax) + aa*bx)*(-2*ax*bc + ac*bx + ab*cx)*det)/
       Power(ab*ac - aa*bc,3);
    e_info->hess[1][3][0][1] = -((aa*Power(ab*ac - aa*bc,2)*bz + 
           2*ax*Power(ab*ac - aa*bc,2)*(-(az*bx) + ax*bz) - 
           aa*(-(ab*ac) + aa*bc)*(az*bx - ax*bz)*(-2*ax*bc + ac*bx + ab*cx) + 
           aa*(-(ab*ac) + aa*bc)*(-(ab*ay) + aa*by)*(-(bz*cy) + by*cz) + 
           2*ax*(ab*ac - aa*bc)*(ab*ay - aa*by)*det - 
           aa*(-(ab*ac) + aa*bc)*(ay*bx - 2*ax*by)*det + 
           2*aa*(-(ab*ay) + aa*by)*(-2*ax*bc + ac*bx + ab*cx)*det)/
         Power(ab*ac - aa*bc,3));
    e_info->hess[1][3][0][2] = (aa*Power(ab*ac - aa*bc,2)*by + 
         2*ax*Power(ab*ac - aa*bc,2)*(-(ay*bx) + ax*by) - 
         aa*(-(ab*ac) + aa*bc)*(ay*bx - ax*by)*(-2*ax*bc + ac*bx + ab*cx) + 
         aa*(-(ab*ac) + aa*bc)*(-(ab*az) + aa*bz)*(bz*cy - by*cz) - 
         2*ax*(ab*ac - aa*bc)*(ab*az - aa*bz)*det + 
         aa*(-(ab*ac) + aa*bc)*(az*bx - 2*ax*bz)*det - 
         2*aa*(-(ab*az) + aa*bz)*(-2*ax*bc + ac*bx + ab*cx)*det)/
       Power(ab*ac - aa*bc,3);
    e_info->hess[1][3][1][0] = (aa*Power(ab*ac - aa*bc,2)*bz + 
         2*ay*Power(ab*ac - aa*bc,2)*(-(az*by) + ay*bz) - 
         aa*(-(ab*ac) + aa*bc)*(az*by - ay*bz)*(-2*ay*bc + ac*by + ab*cy) + 
         aa*(-(ab*ac) + aa*bc)*(-(ab*ax) + aa*bx)*(-(bz*cx) + bx*cz) - 
         2*ay*(ab*ac - aa*bc)*(ab*ax - aa*bx)*det - 
         aa*(-(ab*ac) + aa*bc)*(2*ay*bx - ax*by)*det - 
         2*aa*(-(ab*ax) + aa*bx)*(-2*ay*bc + ac*by + ab*cy)*det)/
       Power(ab*ac - aa*bc,3);
    e_info->hess[1][3][1][1] = -((-2*ay*Power(ab*ac - aa*bc,2)*
            (az*bx - ax*bz) - aa*(-(ab*ac) + aa*bc)*(az*bx - ax*bz)*
            (-2*ay*bc + ac*by + ab*cy) + 
           aa*(-(ab*ac) + aa*bc)*(-(ab*ay) + aa*by)*(bz*cx - bx*cz) + 
           2*ay*(ab*ac - aa*bc)*(ab*ay - aa*by)*det - 
           aa*(-(ab*ac) + aa*bc)*(ax*bx + az*bz)*det + 
           2*aa*(-(ab*ay) + aa*by)*(-2*ay*bc + ac*by + ab*cy)*det)/
         Power(ab*ac - aa*bc,3));
    e_info->hess[1][3][1][2] = -((aa*Power(ab*ac - aa*bc,2)*bx + 
           2*ay*Power(ab*ac - aa*bc,2)*(ay*bx - ax*by) + 
           aa*(-(ab*ac) + aa*bc)*(ay*bx - ax*by)*(-2*ay*bc + ac*by + ab*cy) + 
           aa*(-(ab*ac) + aa*bc)*(-(ab*az) + aa*bz)*(bz*cx - bx*cz) + 
           2*ay*(ab*ac - aa*bc)*(ab*az - aa*bz)*det - 
           aa*(-(ab*ac) + aa*bc)*(az*by - 2*ay*bz)*det + 
           2*aa*(-(ab*az) + aa*bz)*(-2*ay*bc + ac*by + ab*cy)*det)/
         Power(ab*ac - aa*bc,3));
    e_info->hess[1][3][2][0] = -((aa*Power(ab*ac - aa*bc,2)*by + 
           2*az*Power(ab*ac - aa*bc,2)*(az*by - ay*bz) + 
           aa*(-(ab*ac) + aa*bc)*(-(ab*ax) + aa*bx)*(-(by*cx) + bx*cy) + 
           aa*(-(ab*ac) + aa*bc)*(az*by - ay*bz)*(-2*az*bc + ac*bz + ab*cz) + 
           2*az*(ab*ac - aa*bc)*(ab*ax - aa*bx)*det + 
           aa*(-(ab*ac) + aa*bc)*(2*az*bx - ax*bz)*det + 
           2*aa*(-(ab*ax) + aa*bx)*(-2*az*bc + ac*bz + ab*cz)*det)/
         Power(ab*ac - aa*bc,3));
    e_info->hess[1][3][2][1] = (aa*Power(ab*ac - aa*bc,2)*bx + 
         2*az*Power(ab*ac - aa*bc,2)*(az*bx - ax*bz) + 
         aa*(-(ab*ac) + aa*bc)*(-(ab*ay) + aa*by)*(by*cx - bx*cy) + 
         aa*(-(ab*ac) + aa*bc)*(az*bx - ax*bz)*(-2*az*bc + ac*bz + ab*cz) - 
         2*az*(ab*ac - aa*bc)*(ab*ay - aa*by)*det - 
         aa*(-(ab*ac) + aa*bc)*(2*az*by - ay*bz)*det - 
         2*aa*(-(ab*ay) + aa*by)*(-2*az*bc + ac*bz + ab*cz)*det)/
       Power(ab*ac - aa*bc,3);
    e_info->hess[1][3][2][2] = (2*az*Power(ab*ac - aa*bc,2)*
          (-(ay*bx) + ax*by) + aa*(-(ab*ac) + aa*bc)*(-(ab*az) + aa*bz)*
          (by*cx - bx*cy) - aa*(-(ab*ac) + aa*bc)*(ay*bx - ax*by)*
          (-2*az*bc + ac*bz + ab*cz) + 
         aa*(-(ab*ac) + aa*bc)*(ax*bx + ay*by)*det - 
         2*az*(ab*ac - aa*bc)*(ab*az - aa*bz)*det - 
         2*aa*(-(ab*az) + aa*bz)*(-2*az*bc + ac*bz + ab*cz)*det)/
       Power(ab*ac - aa*bc,3);
    e_info->hess[2][2][0][0] = (2*aa*(ac*ax - aa*cx)*
         (-((ab*ac - aa*bc)*(az*cy - ay*cz)) + (ac*ax - aa*cx)*det))/
       Power(ab*ac - aa*bc,3);
    e_info->hess[2][2][0][1] = (aa*
         (-((ab*ac - aa*bc)*(ac*ax - aa*cx)*(-(az*cx) + ax*cz)) + 
           (ab*ac - aa*bc)*(ac*ay - aa*cy)*(-(az*cy) + ay*cz) + 
           2*(ac*ax - aa*cx)*(ac*ay - aa*cy)*det))/Power(ab*ac - aa*bc,3);
    e_info->hess[2][2][0][2] = (aa*
         ((ab*ac - aa*bc)*(ac*ax - aa*cx)*(-(ay*cx) + ax*cy) - 
           (ab*ac - aa*bc)*(ac*az - aa*cz)*(az*cy - ay*cz) + 
           2*(ac*ax - aa*cx)*(ac*az - aa*cz)*det))/Power(ab*ac - aa*bc,3);
    e_info->hess[2][2][1][0] = (aa*
         (-((ab*ac - aa*bc)*(ac*ax - aa*cx)*(-(az*cx) + ax*cz)) + 
           (ab*ac - aa*bc)*(ac*ay - aa*cy)*(-(az*cy) + ay*cz) + 
           2*(ac*ax - aa*cx)*(ac*ay - aa*cy)*det))/Power(ab*ac - aa*bc,3);
    e_info->hess[2][2][1][1] = (2*aa*(ac*ay - aa*cy)*
         ((ab*ac - aa*bc)*(az*cx - ax*cz) + (ac*ay - aa*cy)*det))/
       Power(ab*ac - aa*bc,3);
    e_info->hess[2][2][1][2] = (aa*
         (-((ab*ac - aa*bc)*(ac*ay - aa*cy)*(ay*cx - ax*cy)) + 
           (ab*ac - aa*bc)*(ac*az - aa*cz)*(az*cx - ax*cz) + 
           2*(ac*ay - aa*cy)*(ac*az - aa*cz)*det))/Power(ab*ac - aa*bc,3);
    e_info->hess[2][2][2][0] = (aa*
         ((ab*ac - aa*bc)*(ac*ax - aa*cx)*(-(ay*cx) + ax*cy) - 
           (ab*ac - aa*bc)*(ac*az - aa*cz)*(az*cy - ay*cz) + 
           2*(ac*ax - aa*cx)*(ac*az - aa*cz)*det))/Power(ab*ac - aa*bc,3);
    e_info->hess[2][2][2][1] = (aa*
         (-((ab*ac - aa*bc)*(ac*ay - aa*cy)*(ay*cx - ax*cy)) + 
           (ab*ac - aa*bc)*(ac*az - aa*cz)*(az*cx - ax*cz) + 
           2*(ac*ay - aa*cy)*(ac*az - aa*cz)*det))/Power(ab*ac - aa*bc,3);
    e_info->hess[2][2][2][2] = (2*aa*(ac*az - aa*cz)*
         (-((ab*ac - aa*bc)*(ay*cx - ax*cy)) + (ac*az - aa*cz)*det))/
       Power(ab*ac - aa*bc,3);
    e_info->hess[2][3][0][0] = (aa*
         ((ab*ac - aa*bc)*(az*by - ay*bz)*(ac*ax - aa*cx) - 
           (ab*ac - aa*bc)*(ab*ax - aa*bx)*(az*cy - ay*cz) + 
           (Power(ay,2) + Power(az,2))*(ab*ac - aa*bc)*det + 
           2*(ab*ax - aa*bx)*(ac*ax - aa*cx)*det))/Power(ab*ac - aa*bc,3);
    e_info->hess[2][3][0][1] = (aa*
         (az*Power(ab*ac - aa*bc,2) - 
           (ab*ac - aa*bc)*(az*bx - ax*bz)*(ac*ax - aa*cx) + 
           (ab*ac - aa*bc)*(ab*ay - aa*by)*(-(az*cy) + ay*cz) - 
           ax*ay*(ab*ac - aa*bc)*det - 2*(ab*ay - aa*by)*(-(ac*ax) + aa*cx)*det))/
       Power(ab*ac - aa*bc,3);
    e_info->hess[2][3][0][2] = (aa*
         (-(ay*Power(ab*ac - aa*bc,2)) + 
           (ab*ac - aa*bc)*(ay*bx - ax*by)*(ac*ax - aa*cx) - 
           (ab*ac - aa*bc)*(ab*az - aa*bz)*(az*cy - ay*cz) - 
           ax*az*(ab*ac - aa*bc)*det - 2*(ab*az - aa*bz)*(-(ac*ax) + aa*cx)*det))/
       Power(ab*ac - aa*bc,3);
    e_info->hess[2][3][1][0] = (aa*
         (-(az*Power(ab*ac - aa*bc,2)) + 
           (ab*ac - aa*bc)*(az*by - ay*bz)*(ac*ay - aa*cy) - 
           (ab*ac - aa*bc)*(ab*ax - aa*bx)*(-(az*cx) + ax*cz) - 
           ax*ay*(ab*ac - aa*bc)*det - 2*(ab*ax - aa*bx)*(-(ac*ay) + aa*cy)*det))/
       Power(ab*ac - aa*bc,3);
    e_info->hess[2][3][1][1] = (aa*
         (-((ab*ac - aa*bc)*(az*bx - ax*bz)*(ac*ay - aa*cy)) + 
           (ab*ac - aa*bc)*(ab*ay - aa*by)*(az*cx - ax*cz) + 
           (Power(ax,2) + Power(az,2))*(ab*ac - aa*bc)*det + 
           2*(ab*ay - aa*by)*(ac*ay - aa*cy)*det))/Power(ab*ac - aa*bc,3);
    e_info->hess[2][3][1][2] = (aa*
         (ax*Power(ab*ac - aa*bc,2) + 
           (ab*ac - aa*bc)*(ay*bx - ax*by)*(ac*ay - aa*cy) + 
           (ab*ac - aa*bc)*(ab*az - aa*bz)*(az*cx - ax*cz) - 
           ay*az*(ab*ac - aa*bc)*det - 2*(ab*az - aa*bz)*(-(ac*ay) + aa*cy)*det))/
       Power(ab*ac - aa*bc,3);
    e_info->hess[2][3][2][0] = (aa*
         (ay*Power(ab*ac - aa*bc,2) + 
           (ab*ac - aa*bc)*(ab*ax - aa*bx)*(-(ay*cx) + ax*cy) + 
           (ab*ac - aa*bc)*(az*by - ay*bz)*(ac*az - aa*cz) - 
           ax*az*(ab*ac - aa*bc)*det - 2*(ab*ax - aa*bx)*(-(ac*az) + aa*cz)*det))/
       Power(ab*ac - aa*bc,3);
    e_info->hess[2][3][2][1] = (aa*
         (-(ax*Power(ab*ac - aa*bc,2)) - 
           (ab*ac - aa*bc)*(ab*ay - aa*by)*(ay*cx - ax*cy) - 
           (ab*ac - aa*bc)*(az*bx - ax*bz)*(ac*az - aa*cz) - 
           ay*az*(ab*ac - aa*bc)*det - 2*(ab*ay - aa*by)*(-(ac*az) + aa*cz)*det))/
       Power(ab*ac - aa*bc,3);
    e_info->hess[2][3][2][2] = (aa*
         (-((ab*ac - aa*bc)*(ab*az - aa*bz)*(ay*cx - ax*cy)) + 
           (ab*ac - aa*bc)*(ay*bx - ax*by)*(ac*az - aa*cz) + 
           (Power(ax,2) + Power(ay,2))*(ab*ac - aa*bc)*det + 
           2*(ab*az - aa*bz)*(ac*az - aa*cz)*det))/Power(ab*ac - aa*bc,3);
    e_info->hess[3][3][0][0] = (2*aa*(ab*ax - aa*bx)*
         ((ab*ac - aa*bc)*(az*by - ay*bz) + (ab*ax - aa*bx)*det))/
       Power(ab*ac - aa*bc,3);
    e_info->hess[3][3][0][1] = (aa*
         ((ab*ac - aa*bc)*(ab*ax - aa*bx)*(-(az*bx) + ax*bz) - 
           (ab*ac - aa*bc)*(ab*ay - aa*by)*(-(az*by) + ay*bz) + 
           2*(ab*ax - aa*bx)*(ab*ay - aa*by)*det))/Power(ab*ac - aa*bc,3);
    e_info->hess[3][3][0][2] = (aa*
         (-((ab*ac - aa*bc)*(ab*ax - aa*bx)*(-(ay*bx) + ax*by)) + 
           (ab*ac - aa*bc)*(ab*az - aa*bz)*(az*by - ay*bz) + 
           2*(ab*ax - aa*bx)*(ab*az - aa*bz)*det))/Power(ab*ac - aa*bc,3);
    e_info->hess[3][3][1][0] = (aa*
         ((ab*ac - aa*bc)*(ab*ax - aa*bx)*(-(az*bx) + ax*bz) - 
           (ab*ac - aa*bc)*(ab*ay - aa*by)*(-(az*by) + ay*bz) + 
           2*(ab*ax - aa*bx)*(ab*ay - aa*by)*det))/Power(ab*ac - aa*bc,3);
    e_info->hess[3][3][1][1] = (2*aa*(ab*ay - aa*by)*
         (-((ab*ac - aa*bc)*(az*bx - ax*bz)) + (ab*ay - aa*by)*det))/
       Power(ab*ac - aa*bc,3);
    e_info->hess[3][3][1][2] = (aa*
         ((ab*ac - aa*bc)*(ab*ay - aa*by)*(ay*bx - ax*by) - 
           (ab*ac - aa*bc)*(ab*az - aa*bz)*(az*bx - ax*bz) + 
           2*(ab*ay - aa*by)*(ab*az - aa*bz)*det))/Power(ab*ac - aa*bc,3);
    e_info->hess[3][3][2][0] = (aa*
         (-((ab*ac - aa*bc)*(ab*ax - aa*bx)*(-(ay*bx) + ax*by)) + 
           (ab*ac - aa*bc)*(ab*az - aa*bz)*(az*by - ay*bz) + 
           2*(ab*ax - aa*bx)*(ab*az - aa*bz)*det))/Power(ab*ac - aa*bc,3);
    e_info->hess[3][3][2][1] = (aa*
         ((ab*ac - aa*bc)*(ab*ay - aa*by)*(ay*bx - ax*by) - 
           (ab*ac - aa*bc)*(ab*az - aa*bz)*(az*bx - ax*bz) + 
           2*(ab*ay - aa*by)*(ab*az - aa*bz)*det))/Power(ab*ac - aa*bc,3);
    e_info->hess[3][3][2][2] = (2*aa*(ab*az - aa*bz)*
         ((ab*ac - aa*bc)*(ay*bx - ax*by) + (ab*az - aa*bz)*det))/
       Power(ab*ac - aa*bc,3);




  /* take care of some fudge factors, including orientation */
  for ( i = 1 ; i < 4 ; i++ )
    for ( j = 1 ; j <= i ; j++ )
      for ( ii = 0 ; ii < SDIM ; ii++ )
        for ( jj = 0 ; jj < SDIM ; jj++ )
          e_info->hess[j][i][jj][ii] *= 0.5*sign;

  /* Fill in rest of Hessian by symmetry */
  for ( i = 1 ; i < 4 ; i++ )
    for ( j = i+1 ; j < 4 ; j++ )
      for ( ii = 0 ; ii < SDIM ; ii++ )
        for ( jj = 0 ; jj < SDIM ; jj++ )
          e_info->hess[j][i][jj][ii] = e_info->hess[i][j][ii][jj];

  for ( i = 1 ; i < 4 ; i++ )
    for ( j = 1 ; j < 4 ; j++ )
      for ( ii = 0 ; ii < SDIM ; ii++ )
        for ( jj = 0 ; jj < SDIM ; jj++ )
        { e_info->hess[0][0][ii][jj] += e_info->hess[i][j][ii][jj];
          e_info->hess[0][j][ii][jj] -= e_info->hess[i][j][ii][jj];
          e_info->hess[i][0][ii][jj] -= e_info->hess[i][j][ii][jj];
        }
  
  return value;

}

REAL mean_int_a_value(e_info)
struct qinfo *e_info;
{
  return mean_int_a_all(e_info,METHOD_VALUE);
}
REAL mean_int_a_gradient(e_info)
struct qinfo *e_info;
{
  return mean_int_a_all(e_info,METHOD_GRADIENT);
}
REAL mean_int_a_hessian(e_info)
struct qinfo *e_info;
{
  return mean_int_a_all(e_info,METHOD_HESSIAN);
}




