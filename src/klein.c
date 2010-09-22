/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/******************************************************************
* These functions calculate the length of an edge in the Klein
* model of the hyperbolic plane.
*/

#include "include.h"

#undef SQR
#define SQR(a) ((a)*(a))

REAL coshKleinLength ARGS((REAL*,REAL*));

/*****************************************************************
* This function takes the inverse hyperbolic cosine of a number
* who's value is greater than or equal to one and returns the
* positive answer.  Included here since some libraries don't
* have it or long double version.
*/
REAL kb_acosh ARGS((REAL)); /* prototype */
REAL kb_acosh(value)
REAL value;     /* the value who's acosh is being taken */
{
  if(value>=1)
     return(log(value + sqrt(SQR(value) - 1)));
  else
     return(0);
}

/****************************************************************
* This function finds the hyperbolic cosine of an edge in the Klein
* model of the hyperbolic plane. It is used to calculate edge
* lengths and angles in the model.
*/
REAL coshKleinLength(head, tail)
REAL *head;     /* coordinates of edge */
REAL *tail;
{
  REAL num;     /* numerator and den0minator of returned value */
  REAL den;
  REAL temp;
  REAL retval;

  num = 1 - SDIM_dot(head,tail);
  den = 1 - SDIM_dot(head,head);
  temp = 1 - SDIM_dot(tail,tail);
  if((den*=temp)>0)
  { den = sqrt(den);
     retval = num/den;
  }
  else
     retval = 0.0;

  if ( retval < 1.0 )
     kb_error(2087,"Points outside unit disk in Klein model.\n",RECOVERABLE);
  return(retval);
}

/***********************************************************************
* This function finds the length of an edge in the klein model of
* hyperbolic space.
*/
REAL klein_length(head, tail)
REAL *head;      /* coordinates of edge */
REAL *tail;
{
  return(kb_acosh(coshKleinLength(head, tail)));
}

/**********************************************************************
* This function finds the contribution to the gradient of the edge with
* endpoints tail and head at those endpoints.
* Actually, this adds neg grad to output.
*/
void klein_length_grad(head, tail, head_grad, tail_grad)
REAL *head, *tail;        /* coordinates of edge */
REAL *head_grad, *tail_grad;  /* gradients of head and tail */
{
  int i;
  REAL aa,bb,ab,den,disc;
  aa = 1 - SDIM_dot(head,head);
  bb = 1 - SDIM_dot(tail,tail);
  ab = 1 - SDIM_dot(head,tail);
  disc = ab*ab - aa*bb;
  if ( disc == 0.0 ) 
  { kb_error(3107,"Vertices outside Klein disk, or zero length edge.\n",
         WARNING); return;
  }
  if ( disc < 0.0 ) 
  { kb_error(1664,"Vertices outside Klein disk.\n",WARNING); return; }

  den = sqrt(disc);
  for ( i = 0 ; i < SDIM ; i++ )
  { head_grad[i] -= (ab*head[i]/aa- tail[i])/den;
    tail_grad[i] -= (ab*tail[i]/bb - head[i])/den;
  }
}

/***********************************************************************
* This function finds the area of a triangle in the klein model of the
* hyperbolic plane by using the formula:
*     AREA = PI - angle1 - angle2 - angle3
* 
*  Angles found via hyperbolic law of cosines
*    
*      cosh(C) = cosh(A)cosh(B) + sinh(A)sinh(B)cos(c)
*/
REAL klein_area(triangle)
REAL **triangle;
{
  REAL area;         /* returned area */
  int i;             /* loop iterator */
  int     v;          /* loop iterator */
  int     s;         /* side opposite vertex */

  REAL coshs[MAXCOORD],          /* the coshs of the sides */
        sinhs[MAXCOORD],          /* the sinhs of the sides */
        coss[MAXCOORD];        /* cosines of vertices */

  for ( s = 0 ; s < 3 ; s++ )
  { coshs[s] = coshKleinLength(triangle[(s+1)%3], triangle[(s+2)%3]);
    sinhs[s] = sqrt(coshs[s]*coshs[s] - 1);
    if ( sinhs[s] == 0.0  ) return 0.0;
  }
  for(v=0;v<3;v++) 
      coss[v] = (coshs[(v+1)%3]*coshs[(v+2)%3] - coshs[v]) /
             sinhs[(v+1)%3]/sinhs[(v+2)%3];

  area = M_PI;
  for(i=0;i<3;i++)
     area -= acos(coss[i]);

  return(area);
}

/**************************************************************************
* This function calculates the area gradient at the vertices of a triangle.
*/
void klein_area_grad(triangle, force)
REAL **triangle;        /* the face */
REAL **force;     /* the gradients */
{
  int     v;          /* loop iterator */
  int     s;         /* side opposite vertex */
  int      k,j;      /* indices */

  REAL coshs[MAXCOORD],          /* the coshs of the sides */
       sinhs[MAXCOORD],          /* the sinhs of the sides */
       coss[MAXCOORD],        /* cosines of vertices */
       sins[MAXCOORD],        /* sines of vertices */
       ngrad[3][3][MAXCOORD];      /* neg grad of side wrt vertex */

  for ( s = 0 ; s < 3 ; s++ )
  { coshs[s] = coshKleinLength(triangle[(s+1)%3], triangle[(s+2)%3]);
    sinhs[s] = sqrt(coshs[s]*coshs[s] - 1);
    if ( sinhs[s] == 0.0  ) return ;
  }
  for(v=0;v<3;v++) 
  {
    coss[v] = (coshs[(v+1)%3]*coshs[(v+2)%3] - coshs[v]);
    coss[v] /=     sinhs[(v+1)%3]*sinhs[(v+2)%3];
    sins[v] = sqrt(1 - coss[v]*coss[v]);
  }
  memset((char*)ngrad,0,sizeof(ngrad));
  for ( s = 0 ; s < 3 ; s++ )
     klein_length_grad(triangle[(s+1)%3],triangle[(s+2)%3],
      ngrad[s][(s+1)%3],ngrad[s][(s+2)%3]);

  for ( v = 0 ; v < 3 ; v++ ) /* vertex angle */
  { int vb = (v+1)%3;
    int vc = (v+2)%3;
    REAL denom = sinhs[vb]*sinhs[vc]*sins[v];
    REAL coeffa=sinhs[v]/denom; 
    REAL coeffb=(coshs[vc]-coshs[v]*coshs[vb])/sinhs[vb]/denom;
    REAL coeffc=(coshs[vb]-coshs[v]*coshs[vc])/sinhs[vc]/denom;
    for ( k = 0 ; k < 3 ; k++ ) /* variable vertex */
    for ( j = 0 ; j < SDIM ; j++ ) /* coordinate */
    { 
      force[k][j] -= coeffb*ngrad[vb][k][j] + coeffc*ngrad[vc][k][j]
         + coeffa*ngrad[v][k][j];
    }
  }
}

/**************************************************************************

              Named quantity methods for 2D and 3D

**************************************************************************/

/***********************************************************************
* This function finds the length of an edge in the klein model of
* hyperbolic space.
*/
REAL klein_length_method(e_info)
struct qinfo *e_info;
{ REAL area;
  area = kb_acosh(coshKleinLength(e_info->x[1], e_info->x[0]));
  if ( everything_quantities_flag  && 
      (METH_INSTANCE(e_info->method)->quant == default_area_quant_num) )
     area *= get_edge_density(e_info->id);

  return(area);
}

/**********************************************************************
* This function finds the contribution to the gradient of the edge with
* endpoints tail and head at those endpoints.
*/
REAL klein_length_method_grad(e_info)
struct qinfo *e_info;
{
  int i;
  REAL aa,bb,ab,den,disc;
  REAL *head = e_info->x[1];
  REAL *tail = e_info->x[0];
  REAL fudge;
  if ( everything_quantities_flag  && 
      (METH_INSTANCE(e_info->method)->quant == default_area_quant_num) )
     fudge = get_edge_density(e_info->id);
  else fudge = 1.0;


  aa = 1 - SDIM_dot(head,head);
  bb = 1 - SDIM_dot(tail,tail);
  ab = 1 - SDIM_dot(head,tail);
  disc = ab*ab - aa*bb;
  if ( disc == 0.0 ) 
  { kb_error(3359,"Vertices outside Klein disk, or zero length edge.\n",
         WARNING); return 0.0;
  }
  if ( disc < 0.0 ) 
  { kb_error(1665,"Vertices outside Klein disk.\n",WARNING); return 0.0; }

  den = sqrt(disc);
  for ( i = 0 ; i < SDIM ; i++ )
  { e_info->grad[1][i] += fudge*(ab*head[i]/aa - tail[i])/den;
    e_info->grad[0][i] += fudge*(ab*tail[i]/bb - head[i])/den;
  }
  return(fudge*kb_acosh(coshKleinLength(head, tail)));
}

/***********************************************************************
* This function finds the area of a triangle in the klein model of the
* hyperbolic plane by using the formula:
*     AREA = PI - angle1 - angle2 - angle3
*/
REAL klein_area_method(f_info)
struct qinfo *f_info;
{
  REAL area;         /* returned area */
  int i;             /* loop iterator */
  int     v;          /* loop iterator */
  int     s;         /* side opposite vertex */
  REAL **triangle = f_info->x;

  REAL coshs[MAXCOORD],          /* the coshs of the sides */
       sinhs[MAXCOORD],          /* the sinhs of the sides */
       coss[MAXCOORD];        /* cosines of vertices */

  for ( s = 0 ; s < 3 ; s++ )
  { coshs[s] = coshKleinLength(triangle[(s+1)%3], triangle[(s+2)%3]);
    sinhs[s] = sqrt(coshs[s]*coshs[s] - 1);
    if ( sinhs[s] == 0.0  ) return 0.0;
  }
  for(v=0;v<3;v++) 
      coss[v] = (coshs[(v+1)%3]*coshs[(v+2)%3] - coshs[v]) /
             sinhs[(v+1)%3]/sinhs[(v+2)%3];

  area = M_PI;
  for(i=0;i<3;i++)
     area -= acos(coss[i]);

  if ( everything_quantities_flag  && 
      (METH_INSTANCE(f_info->method)->quant == default_area_quant_num) )
     area *= get_facet_density(f_info->id);

  return(area);
}

/**************************************************************************
* This function calculates the area gradient at the vertices of a triangle.
*/
REAL klein_area_method_grad(f_info)
struct qinfo *f_info;
{ REAL area;
  int     v;          /* loop iterator */
  int     s;         /* side opposite vertex */
  int     i, k,j;      /* indices */
  REAL **triangle = f_info->x;

  REAL coshs[MAXCOORD],          /* the coshs of the sides */
       sinhs[MAXCOORD],          /* the sinhs of the sides */
       coss[MAXCOORD],        /* cosines of vertices */
       sins[MAXCOORD],        /* sines of vertices */
       ngrad[3][3][MAXCOORD];      /* neg grad of side wrt vertex */
  REAL fudge;
  if ( everything_quantities_flag  && 
      (METH_INSTANCE(f_info->method)->quant == default_area_quant_num) )
     fudge = get_facet_density(f_info->id);
  else fudge = 1.0;

  for ( s = 0 ; s < 3 ; s++ )
  { coshs[s] = coshKleinLength(triangle[(s+1)%3], triangle[(s+2)%3]);
    sinhs[s] = sqrt(coshs[s]*coshs[s] - 1);
    if ( sinhs[s] == 0.0  ) return 0.0 ;
  }
  for(v=0;v<3;v++) 
  {
    coss[v] = (coshs[(v+1)%3]*coshs[(v+2)%3] - coshs[v]);
    coss[v] /=     sinhs[(v+1)%3]*sinhs[(v+2)%3];
    sins[v] = sqrt(1 - coss[v]*coss[v]);
  }
  memset((char*)ngrad,0,sizeof(ngrad));
  for ( s = 0 ; s < 3 ; s++ )
     klein_length_grad(triangle[(s+1)%3],triangle[(s+2)%3],
      ngrad[s][(s+1)%3],ngrad[s][(s+2)%3]);

  for ( v = 0 ; v < 3 ; v++ ) /* vertex angle */
  { int vb = (v+1)%3;
    int vc = (v+2)%3;
    REAL denom = sinhs[vb]*sinhs[vc]*sins[v];
    REAL coeffa=sinhs[v]/denom; 
    REAL coeffb=(coshs[vc]-coshs[v]*coshs[vb])/sinhs[vb]/denom;
    REAL coeffc=(coshs[vb]-coshs[v]*coshs[vc])/sinhs[vc]/denom;
    for ( k = 0 ; k < 3 ; k++ ) /* variable vertex */
    for ( j = 0 ; j < SDIM ; j++ ) /* coordinate */
    { 
      f_info->grad[k][j] += fudge*(coeffb*ngrad[vb][k][j]
         + coeffc*ngrad[vc][k][j]
         + coeffa*ngrad[v][k][j]);
    }
  }

  area = M_PI;
  for(i=0;i<3;i++)
     area -= acos(coss[i]);

  return(fudge*area);
}

