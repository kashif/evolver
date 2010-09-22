/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*                                                            *
*  Code for SVK and Neo_hookean submitted by                 *
*  Dr. Rabah Bouzidi                                         *
*  Institut de Recherche en Gnie Civil et Mcanique           *
*  (Civil Engineering and Mechanical Research Institut)      *
*  Nantes University                                         *
*  France                                                    *
*  Tl : +33 2 51 12 55 23                                    *
*  rabah.bouzidi@physique.univ-nantes.fr                     *
*************************************************************/

#include "include.h"

REAL LambertW ARGS((REAL));

/************************************************************************
     Named method: SVK (Saint-Venant - Kirchhoff) potential
	 Psi = lambda/2*(tr(E))^2+mu*(E:E) - (3 lambda + 2 mu) * alpha*(theta)*tr(E)
	 with E=(C-I)/2 the Green-Lagrange Strain tensor
	 theta = T-T0 : temperture variation
	 alpha : thermal dilation coefficient 
     Written by  Dr. Rabah Bouzidi  
************************************************************************/
#define LAMBDA_NAME "SVK_lambda"
#define MU_NAME "SVK_mu"
#define ALPHA_NAME "SVK_alpha"
#define THETA_NAME "SVK_theta"
#define FORM_FACTORS_NAME "form_factors"
static int lambda_attr; /* number of lambda extra attribute */
static int mu_attr; /* number of mu extra attribute */
static int alpha_attr ; /* number of alpha extra attribute */
static int theta_attr ; /* number of theta extra attribute */
extern int form_factors_attr; /* number of form_factors extra attribute */
/***************************************************************
*
*  function: SVK_init()
*
*  purpose: Make sure needed extra attributes are present.
*              
*/
void SVK_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{ 
  if ( web.modeltype != LINEAR )
     kb_error(2811,"Saint-Veant - Kirchhoff method only for LINEAR model.\n",RECOVERABLE);

  if ( web.dimension != 2 )
     kb_error(2812,"Saint-Veant - Kirchhoff method only for SOAPFILM model.\n",
        RECOVERABLE);

  /* extra edge atribute */
  lambda_attr = find_attribute(FACET,LAMBDA_NAME);
  if ( lambda_attr < 0 ) /* not found */
     kb_error(2813,"Facet extra attribute SVK_lambda missing. Needed by SVK_elastic method.\n",RECOVERABLE);
  mu_attr = find_attribute(FACET,MU_NAME);
  if ( mu_attr < 0 ) /* not found */
     kb_error(2814,"Facet extra attribute SVK_mu missing. Needed by SVK_elastic method.\n",RECOVERABLE);

  alpha_attr = find_attribute(FACET,ALPHA_NAME);
  if ( alpha_attr < 0 ) /* not found */
     kb_error(2815,"Facet extra attribute alpha missing. Needed by Saint-Veant - Kirchhoff.\n",RECOVERABLE);

  theta_attr = find_attribute(FACET,THETA_NAME);
  if ( theta_attr < 0 ) /* not found */
     kb_error(2816,"Facet extra attribute theta missing. Needed by Saint-Veant - Kirchhoff.\n",RECOVERABLE);

  form_factors_attr = find_attribute(FACET,FORM_FACTORS_NAME);
  if ( form_factors_attr < 0 ) /* not found */
     kb_error(2817,"Facet extra attribute form_factors real[3] missing. Needed by Saint-Veant - Kirchhoff.\n",RECOVERABLE);

  if ( EXTRAS(FACET)[form_factors_attr].array_spec.datacount != 3 )
     kb_error(2818,"Facet extra attribute form_factors must have size 3.\n",
        RECOVERABLE);
}
/************************************************************************
*
* function: SVK_all()
*
* purpose: energy, gradient, and hessian for linear_elastic method.
*/
REAL SVK_all ARGS((struct qinfo *,int));

REAL SVK_all(f_info,mode)
struct qinfo *f_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */
{
  REAL *s;  /* pointer to extra attributes */
  REAL **side; 
  REAL q11,q12,q22;  /* Q entries */
  REAL det;    /* det S */
  REAL lambda;  /* lambda coefficient  */
  REAL area;  /* reference area of facet (area of facet in initial configuration - undeformed facet)*/
  REAL mu; /* mu coefficient  */
  REAL alpha; /* Thermic dilatation coefficient */
  REAL theta ; /* Relative variation of temperture to the the reference state's temperature*/
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

  /* Potential derivatives */
  REAL dpsirdc11,dpsirdc12,dpsirdc21,dpsirdc22;
  REAL ddpsirdc11dc11, ddpsirdc11dc12, ddpsirdc11dc21, ddpsirdc11dc22;
  REAL ddpsirdc12dc11, ddpsirdc12dc12, ddpsirdc12dc21, ddpsirdc12dc22;
  REAL ddpsirdc21dc11, ddpsirdc21dc12, ddpsirdc21dc21, ddpsirdc21dc22;
  REAL ddpsirdc22dc11, ddpsirdc22dc12, ddpsirdc22dc21, ddpsirdc22dc22;

  int i,j,ii,jj;
  
  lambda = *(REAL*)get_extra(f_info->id,lambda_attr);
  mu = *(REAL*)get_extra(f_info->id,mu_attr);
  alpha = *(REAL*)get_extra(f_info->id,alpha_attr);
  theta = *(REAL*)get_extra(f_info->id,theta_attr);

  s = (REAL*)get_extra(f_info->id,form_factors_attr);
  det = s[0]*s[2] - s[1]*s[1];
  if ( det <= 0.0 )
     { 
        if ( mode == METHOD_VALUE ) return 0.0;
        sprintf(errmsg," SVK_elastic: Facet %d has unstrained area 0.\n",
          ordinal(f_info->id)+1);
        kb_error(2895,errmsg,RECOVERABLE);
     }

  area = sqrt(det)/2.0;
  
  q11 = s[2]/det; q12 = -s[1]/det; q22 = s[0]/det;

  side = f_info->sides[0];
  f11 = SDIM_dot(side[0],side[0]);
  f12 = SDIM_dot(side[0],side[1]);
  f22 = SDIM_dot(side[1],side[1]);

  c11 = f11*q11 + f12*q12;
  c12 = f11*q12 + f12*q22;
  c21 = f12*q11 + f22*q12;
  c22 = f12*q12 + f22*q22;

 /*
 This exression of the potential takes into account the plane stress condition : Sig33 = 0, 
 This condition leads to the expression of c33, which can be written as a
 function of c11 and c22 : c33 = -(lambda*(c11+c22-3)-2*mu)/(lambda+2*mu)
 */

  energy =area*((2.0*mu+3.0*lambda)*alpha*theta*(-(2.0*mu+3.0*lambda)*alpha*theta-2.0*mu*(c11+c22-2.0))+((c22*c22+c11*c11+2.0*c12*c21+2.0-2.0*c11-2.0*c22)*mu*mu+(c11*c22+c12*c21+c22*c22+c11*c11+3.0-3.0*c11-3.0*c22)*lambda*mu))/(2.0*lambda+4.0*mu);
	      
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
	    
  dpsirdc11 = mu*((2.0*c11-2.0)*mu+2.0*lambda*c11-3.0*lambda+lambda*c22-4.0*alpha*theta*mu-6.0*alpha*theta*lambda)/(lambda+2.0*mu)/2.0;
  dpsirdc12 = mu*c21/2.0;
  dpsirdc21 = mu*c12/2.0;
  dpsirdc22 = mu*((2.0*c22-2.0)*mu+2.0*lambda*c22-3.0*lambda+lambda*c11-4.0*alpha*theta*mu-6.0*alpha*theta*lambda)/(lambda+2.0*mu)/2.0;

  for ( j = 0 ; j < FACET_VERTS  ; j++ )
     for ( i = 0 ; i < SDIM  ; i++ )
     { f_info->grad[j][i] = area*(dpsirdc11*dc11dv[j][i]  
                                 +dpsirdc12*dc12dv[j][i]
                                 +dpsirdc21*dc21dv[j][i]
                                 +dpsirdc22*dc22dv[j][i]);
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


  ddpsirdc11dc11 = mu*(mu+lambda)/(lambda+2.0*mu);
  ddpsirdc11dc12 = 0.0;
  ddpsirdc11dc21 = 0.0 ;
  ddpsirdc11dc22 = lambda*mu/(2.0*lambda+4.0*mu);

  ddpsirdc12dc11 = 0.0;
  ddpsirdc12dc12 = 0.0;
  ddpsirdc12dc21 = mu/2.0;
  ddpsirdc12dc22 = 0.0;

  ddpsirdc21dc11 = 0.0;
  ddpsirdc21dc12 = mu/2.0;
  ddpsirdc21dc21 = 0.0;
  ddpsirdc21dc22 = 0.0;

  ddpsirdc22dc11 = mu*lambda/(2.0*lambda+4.0*mu);
  ddpsirdc22dc12 = 0.0;
  ddpsirdc22dc21 = 0.0;
  ddpsirdc22dc22 = mu*(mu+lambda)/(lambda+2.0*mu) ; 

  for ( j = 0 ; j < FACET_VERTS  ; j++ )
     for ( i = 0 ; i < SDIM  ; i++ )
        for ( jj = 0 ; jj < FACET_VERTS  ; jj++ )
        {
          f_info->hess[j][jj][i][i] += 
                  area*(dpsirdc11*ddc11dv[j][i][jj]
                       +dpsirdc12*ddc21dv[j][i][jj]
                       +dpsirdc21*ddc12dv[j][i][jj]
                       +dpsirdc22*ddc22dv[j][i][jj]);

          for ( ii = 0 ; ii < SDIM  ; ii++ )
             f_info->hess[j][jj][i][ii] += 
            area*((ddpsirdc11dc11+ddpsirdc12dc11+ddpsirdc21dc11+ddpsirdc22dc11)*dc11dv[j][i] 
                 +(ddpsirdc11dc12+ddpsirdc12dc12+ddpsirdc21dc12+ddpsirdc22dc12)*dc12dv[j][i]
                 +(ddpsirdc11dc21+ddpsirdc12dc21+ddpsirdc21dc21+ddpsirdc22dc21)*dc21dv[j][i]
                 +(ddpsirdc11dc22+ddpsirdc12dc22+ddpsirdc21dc22+ddpsirdc22dc22)*dc22dv[j][i]);
        }

  return energy;
  }

/**************************************************************
*
*  function: SVK_energy()  (Saint-Veant - Kirchhoff potential)
*
*  purpose: calculates energy of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/
REAL SVK_energy(f_info)
struct qinfo *f_info;
{
 return SVK_all(f_info,METHOD_VALUE);
}
/**************************************************************
*
*  function: SVK_gradient()
*
*  purpose: calculates gradient of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/
REAL SVK_gradient(f_info)
struct qinfo *f_info;
{
 return SVK_all(f_info,METHOD_GRADIENT);
}
/**************************************************************
*
*  function: SVK_hessian()
*
*  purpose: calculates hessian of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/
REAL SVK_hessian(f_info)
struct qinfo *f_info;
{
 return SVK_all(f_info,METHOD_HESSIAN);
}

/***********************************************************************/
/************************************************************************
     Named method: Neo_Hookean
     Written by  Dr. Rabah Bouzidi  
************************************************************************/
#define NEO_LAMBDA_NAME "neo_lambda"
#define NEO_MU_NAME "neo_mu"
#define FORM_FACTORS_NAME "form_factors"
/***************************************************************
*
*  function: Neo_Hookean_init()
*
*  purpose: Make sure needed extra attributes are present.
*              
*/
void Neo_Hookean_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{ 

  if ( web.modeltype != LINEAR )
     kb_error(2819,"Neo_Hookean method only for LINEAR model.\n",RECOVERABLE);

  if ( web.dimension != 2 )
     kb_error(2829,"Neo_Hookean method only for SOAPFILM model.\n",
        RECOVERABLE);

  /* extra edge atribute */
  lambda_attr = find_attribute(FACET,NEO_LAMBDA_NAME);
  if ( lambda_attr < 0 ) /* not found */
     kb_error(2837,"Facet extra attribute neo_lambda missing. Needed by Neo_Hookean.\n",RECOVERABLE);
  mu_attr = find_attribute(FACET,NEO_MU_NAME);
  if ( mu_attr < 0 ) /* not found */
     kb_error(2838,"Facet extra attribute neo_mu missing. Needed by Neo_Hookean.\n",RECOVERABLE);

  form_factors_attr = find_attribute(FACET,FORM_FACTORS_NAME);
  if ( form_factors_attr < 0 ) /* not found */
     kb_error(2839,"Facet extra attribute form_factors real[3] missing. Needed by Neo_Hookean.\n",RECOVERABLE);

  if ( EXTRAS(FACET)[form_factors_attr].array_spec.datacount != 3 )
     kb_error(2850,"Facet extra attribute form_factors must have size 3.\n",
        RECOVERABLE);
}
/************************************************************************
*
* function: neo_Hookean_all()
*
* purpose: energy, gradient, and hessian for neo_Hookean method.
*/
REAL Neo_Hookean_all ARGS((struct qinfo *,int));

REAL Neo_Hookean_all(f_info,mode)
struct qinfo *f_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */
{
  REAL *s;  /* pointer to extra attributes */
  REAL **side; 
  REAL q11,q12,q22;  /* Q entries */
  REAL det;    /* det S */
  REAL lambda;  /* coefficient lambda */
  REAL area;  /* reference area of facet */
  REAL mu; /* coefficient mu */
  REAL f11,f12,f22;
  REAL c11,c12,c21,c22, c33;
  REAL energy;
  REAL dc11dv[FACET_VERTS][MAXCOORD];
  REAL dc12dv[FACET_VERTS][MAXCOORD];
  REAL dc21dv[FACET_VERTS][MAXCOORD];
  REAL dc22dv[FACET_VERTS][MAXCOORD];
  REAL ddc11dv[FACET_VERTS][MAXCOORD][FACET_VERTS];
  REAL ddc12dv[FACET_VERTS][MAXCOORD][FACET_VERTS];
  REAL ddc21dv[FACET_VERTS][MAXCOORD][FACET_VERTS];
  REAL ddc22dv[FACET_VERTS][MAXCOORD][FACET_VERTS];
  /* potential derivatives declaration */
  REAL dpsirdc11,dpsirdc12,dpsirdc21,dpsirdc22;
  REAL ddpsirdc11dc11, ddpsirdc11dc12, ddpsirdc11dc21, ddpsirdc11dc22;
  REAL ddpsirdc12dc11, ddpsirdc12dc12, ddpsirdc12dc21, ddpsirdc12dc22;
  REAL ddpsirdc21dc11, ddpsirdc21dc12, ddpsirdc21dc21, ddpsirdc21dc22;
  REAL ddpsirdc22dc11, ddpsirdc22dc12, ddpsirdc22dc21, ddpsirdc22dc22;

  REAL s1, s2, s3, s4, s5, s6, s7;
  REAL LW, tmp1, tmp2, logtmp2;

  int i,j,ii,jj;
  
  REAL detr;


  lambda = *(REAL*)get_extra(f_info->id,lambda_attr);
  mu = *(REAL*)get_extra(f_info->id,mu_attr);

  s = (REAL*)get_extra(f_info->id,form_factors_attr);
  det = s[0]*s[2] - s[1]*s[1];
  if ( det <= 0.0 )
     { 
        if ( mode == METHOD_VALUE ) return 0.0;
        sprintf(errmsg," Neo_Hookean: Facet %d has unstrained area 0.\n",
          ordinal(f_info->id)+1);
        kb_error(2852,errmsg,RECOVERABLE);
     }

  area = sqrt(det)/2.0;
  
  q11 = s[2]/det; q12 = -s[1]/det; q22 = s[0]/det;

  side = f_info->sides[0];
  f11 = SDIM_dot(side[0],side[0]);
  f12 = SDIM_dot(side[0],side[1]);
  f22 = SDIM_dot(side[1],side[1]);

  c11 = f11*q11 + f12*q12 ;
  c12 = f11*q12 + f12*q22;
  c21 = f12*q11 + f22*q12;
  c22 = f12*q12 + f22*q22;

  detr = c11*c22-c12*c21;
  /* plane stress condition */
  LW = LambertW(2.0*mu/detr/lambda*exp(2.0*mu/lambda));
 
  c33 = exp((-LW*lambda+2.0*mu)/lambda)/detr;
  
  tmp1 = log(exp(-LW+2.0*mu/lambda));
  tmp2 = exp(-(LW*lambda-2.0*mu)/lambda);
  logtmp2 = log(tmp2);


  energy =area*(mu*c11/2.0+mu*c22/2.0+mu/exp(LW)*pow(exp(mu/lambda),2.0)/(detr)/2.0-3.0/2.0*mu-mu*tmp1/2.0+lambda*pow(tmp1,2.0)/8.0);

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
	  

  s1 = (1/(pow(detr,2.0))*LW/(1.0+LW)*c22*exp(-LW+2.0*mu/lambda)/2.0-1/(pow(detr,2.0))*c22*exp(-LW+2.0*mu/lambda)/2.0-LW/(1.0+LW)/(detr)*c22/2.0+1.0/2.0)*mu;      
  s2 = lambda*tmp1*LW/(1.0+LW)/(detr)*c22/4.0;      
  dpsirdc11 = s1+s2;

  s1 = c21/4.0;      
  s4 = 2.0*mu*tmp2+2.0*mu*LW*c11*c22;      
  s3 = s4-2.0*mu*LW*c12*c21-lambda*logtmp2*LW*c11*c22+lambda*logtmp2*LW*c12*c21;      
  s4 = 1/(pow(detr,2.0))/(1.0+LW);      
  s2 = s3*s4;       
  dpsirdc12 = s1*s2;

  s1 = c12/4.0;      
  s4 = 2.0*mu*tmp2+2.0*mu*LW*c11*c22;      
  s3 = s4-2.0*mu*LW*c12*c21-lambda*logtmp2*LW*c11*c22+lambda*logtmp2*LW*c12*c21;      
  s4 = 1/(pow(detr,2.0))/(1.0+LW);      
  s2 = s3*s4;      
  dpsirdc21 = s1*s2;

  s1 = (1/(pow(detr,2.0))*LW/(1.0+LW)*c11*exp(-LW+2.0*mu/lambda)/2.0-1/(pow(detr,2.0))*c11*exp(-LW+2.0*mu/lambda)/2.0-LW/(1.0+LW)/(detr)*c11/2.0+1.0/2.0)*mu;      
  s2 = lambda*tmp1*LW/(1.0+LW)/(detr)*c11/4.0;      
  dpsirdc22 = s1+s2;

  
  for ( j = 0 ; j < FACET_VERTS  ; j++ )
     for ( i = 0 ; i < SDIM  ; i++ )
     { f_info->grad[j][i] = area*(dpsirdc11*dc11dv[j][i]  
                                 +dpsirdc12*dc12dv[j][i]
                                 +dpsirdc21*dc21dv[j][i]
                                 +dpsirdc22*dc22dv[j][i]);
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

  s1 = c22*c22/4.0;      
  s5 = 2.0*mu*pow(LW,2.0)*tmp2+4.0*mu*pow(LW,2.0)*c11*c22+4.0*mu*LW*c11*c22+4.0*mu*tmp2;      
  s4 = s5+4.0*mu*LW*tmp2-4.0*mu*pow(LW,2.0)*c12*c21-4.0*mu*LW*c12*c21+2.0*mu*pow(LW,3.0)*c11*c22-2.0*mu*pow(LW,3.0)*c12*c21;      
  s5 = s4-lambda*pow(LW,2.0)*c12*c21-2.0*lambda*logtmp2*LW*c11*c22+lambda*pow(LW,3.0)*c11*c22+lambda*pow(LW,2.0)*c11*c22;      
  s6 = s5+lambda*logtmp2*pow(LW,3.0)*c12*c21-lambda*logtmp2*pow(LW,3.0)*c11*c22;      
  s7 = s6-lambda*pow(LW,3.0)*c12*c21;      
  s3 = s7+2.0*lambda*logtmp2*pow(LW,2.0)*c12*c21+2.0*lambda*logtmp2*LW*c12*c21-2.0*lambda*logtmp2*pow(LW,2.0)*c11*c22;      
  s4 = 1/(pow(detr,3.0))/pow(1.0+LW,3.0);      
  s2 = s3*s4;      
  ddpsirdc11dc11 = s1*s2;

  s1 = -c22*c21/4.0;      
  s5 = 2.0*mu*pow(LW,2.0)*tmp2+4.0*mu*pow(LW,2.0)*c11*c22+4.0*mu*LW*c11*c22+4.0*mu*tmp2;      
  s4 = s5+4.0*mu*LW*tmp2-4.0*mu*pow(LW,2.0)*c12*c21-4.0*mu*LW*c12*c21+2.0*mu*pow(LW,3.0)*c11*c22-2.0*mu*pow(LW,3.0)*c12*c21;      
  s5 = s4-lambda*pow(LW,2.0)*c12*c21-2.0*lambda*logtmp2*LW*c11*c22+lambda*pow(LW,3.0)*c11*c22+lambda*pow(LW,2.0)*c11*c22;      
  s6 = s5+lambda*logtmp2*pow(LW,3.0)*c12*c21-lambda*logtmp2*pow(LW,3.0)*c11*c22;      
  s7 = s6-lambda*pow(LW,3.0)*c12*c21;      
  s3 = s7+2.0*lambda*logtmp2*pow(LW,2.0)*c12*c21+2.0*lambda*logtmp2*LW*c12*c21-2.0*lambda*logtmp2*pow(LW,2.0)*c11*c22;      
  s4 = 1/(pow(detr,3.0))/pow(1.0+LW,3.0);      
  s2 = s3*s4;      
  ddpsirdc11dc12 = s1*s2;
  
  
  s1 = -c22*c12/4.0;      
  s5 = 2.0*mu*pow(LW,2.0)*tmp2+4.0*mu*pow(LW,2.0)*c11*c22+4.0*mu*LW*c11*c22+4.0*mu*tmp2;      
  s4 = s5+4.0*mu*LW*tmp2-4.0*mu*pow(LW,2.0)*c12*c21-4.0*mu*LW*c12*c21+2.0*mu*pow(LW,3.0)*c11*c22-2.0*mu*pow(LW,3.0)*c12*c21;      
  s5 = s4-lambda*pow(LW,2.0)*c12*c21-2.0*lambda*logtmp2*LW*c11*c22+lambda*pow(LW,3.0)*c11*c22+lambda*pow(LW,2.0)*c11*c22;      
  s6 = s5+lambda*logtmp2*pow(LW,3.0)*c12*c21-lambda*logtmp2*pow(LW,3.0)*c11*c22;      
  s7 = s6-lambda*pow(LW,3.0)*c12*c21;      
  s3 = s7+2.0*lambda*logtmp2*pow(LW,2.0)*c12*c21+2.0*lambda*logtmp2*LW*c12*c21-2.0*lambda*logtmp2*pow(LW,2.0)*c11*c22;      
  s4 = 1/(pow(detr,3.0))/pow(1.0+LW,3.0);      
  s2 = s3*s4;     
  ddpsirdc11dc21 = s1*s2;


  s3 = mu*tmp2*c12*c21/2.0+mu*LW*c11*c11*c22*c22/2.0+lambda*pow(LW,3.0)*c11*c11*c22*c22/4.0+lambda*pow(LW,2.0)*c11*c11*c22*c22/4.0-mu*pow(LW,3.0)*c12*c12*c21*c21/2.0;      
  s4 = s3-mu*pow(LW,2.0)*c12*c12*c21*c21-mu*LW*c12*c12*c21*c21/2.0;      
  s2 = s4+mu*c22*c11*tmp2/2.0+mu*LW*tmp2*c12*c21+mu*pow(LW,2.0)*tmp2*c12*c21/2.0;      
  s4 = s2-lambda*logtmp2*pow(LW,3.0)*c11*c22*c12*c21/4.0;      
  s3 = s4-lambda*logtmp2*pow(LW,2.0)*c11*c22*c12*c21/2.0-lambda*logtmp2*LW*c11*c11*c22*c22/4.0-lambda*pow(LW,3.0)*c11*c22*c12*c21/4.0;      
  s4 = s3-lambda*pow(LW,2.0)*c11*c22*c12*c21/4.0+mu*pow(LW,3.0)*c11*c22*c12*c21/2.0;      
  s5 = s4+mu*pow(LW,2.0)*c11*c22*c12*c21;      
  s1 = s5+lambda*logtmp2*pow(LW,3.0)*c12*c12*c21*c21/4.0+lambda*logtmp2*LW*c12*c12*c21*c21/4.0+lambda*logtmp2*pow(LW,2.0)*c12*c12*c21*c21/2.0;      
  s2 = 1/(pow(detr,3.0))/pow(1.0+LW,3.0);      
  ddpsirdc11dc22 = s1*s2;

  s1 = c21*c22/4.0;      
  s6 = lambda*logtmp2*pow(LW,3.0)*c11*c22-lambda*logtmp2*pow(LW,3.0)*c12*c21;      
  s5 = s6+2.0*lambda*logtmp2*LW*c11*c22+2.0*lambda*logtmp2*pow(LW,2.0)*c11*c22;      
  s6 = s5-2.0*lambda*logtmp2*LW*c12*c21-2.0*lambda*logtmp2*pow(LW,2.0)*c12*c21;      
  s4 = s6-4.0*mu*tmp2-4.0*mu*LW*tmp2-2.0*mu*pow(LW,2.0)*tmp2;      
  s5 = s4+4.0*mu*pow(LW,2.0)*c12*c21+4.0*mu*LW*c12*c21-4.0*mu*pow(LW,2.0)*c11*c22-4.0*mu*LW*c11*c22;      
  s3 = s5-2.0*mu*pow(LW,3.0)*c11*c22+lambda*pow(LW,3.0)*c12*c21+lambda*pow(LW,2.0)*c12*c21-lambda*pow(LW,3.0)*c11*c22-lambda*pow(LW,2.0)*c11*c22+2.0*mu*pow(LW,3.0)*c12*c21;      
  s4 = 1/(pow(detr,3.0))/pow(1.0+LW,3.0);      
  s2 = s3*s4;      
  ddpsirdc12dc11 = s1*s2;

  s1 = -c21*c21/4.0;      
  s6 = lambda*logtmp2*pow(LW,3.0)*c11*c22-lambda*logtmp2*pow(LW,3.0)*c12*c21;      
  s5 = s6+2.0*lambda*logtmp2*LW*c11*c22+2.0*lambda*logtmp2*pow(LW,2.0)*c11*c22;      
  s6 = s5-2.0*lambda*logtmp2*LW*c12*c21-2.0*lambda*logtmp2*pow(LW,2.0)*c12*c21;      
  s4 = s6-4.0*mu*tmp2-4.0*mu*LW*tmp2-2.0*mu*pow(LW,2.0)*tmp2;      
  s5 = s4+4.0*mu*pow(LW,2.0)*c12*c21+4.0*mu*LW*c12*c21-4.0*mu*pow(LW,2.0)*c11*c22-4.0*mu*LW*c11*c22;      
  s3 = s5-2.0*mu*pow(LW,3.0)*c11*c22+lambda*pow(LW,3.0)*c12*c21+lambda*pow(LW,2.0)*c12*c21-lambda*pow(LW,3.0)*c11*c22-lambda*pow(LW,2.0)*c11*c22+2.0*mu*pow(LW,3.0)*c12*c21;      
  s4 = 1/(pow(detr,3.0))/pow(1.0+LW,3.0);      
  s2 = s3*s4;      
  ddpsirdc12dc12 = s1*s2;

  s3 = -mu*LW*c12*c12*c21*c21/2.0-lambda*pow(LW,3.0)*c12*c12*c21*c21/4.0-lambda*pow(LW,2.0)*c12*c12*c21*c21/4.0+mu*pow(LW,3.0)*c11*c11*c22*c22/2.0+mu*pow(LW,2.0)*c11*c11*c22*c22;      
  s4 = s3+mu*c21*c12*tmp2/2.0+mu*pow(LW,2.0)*tmp2*c11*c22/2.0;      
  s2 = s4+mu*LW*tmp2*c11*c22-mu*pow(LW,2.0)*c12*c21*c11*c22+mu*tmp2*c11*c22/2.0;      
  s3 = s2+lambda*logtmp2*LW*c12*c12*c21*c21/4.0+lambda*pow(LW,3.0)*c12*c21*c11*c22/4.0+lambda*pow(LW,2.0)*c12*c21*c11*c22/4.0+mu*LW*c11*c11*c22*c22/2.0;      
  s4 = s3-mu*pow(LW,3.0)*c12*c21*c11*c22/2.0+lambda*logtmp2*pow(LW,2.0)*c12*c21*c11*c22/2.0;      
  s5 = s4+lambda*logtmp2*pow(LW,3.0)*c12*c21*c11*c22/4.0;      
  s1 = s5-lambda*logtmp2*pow(LW,3.0)*c11*c11*c22*c22/4.0-lambda*logtmp2*LW*c11*c11*c22*c22/4.0-lambda*logtmp2*pow(LW,2.0)*c11*c11*c22*c22/2.0;      
  s2 = 1/(pow(detr,3.0))/pow(1.0+LW,3.0);    
  ddpsirdc12dc21 = s1*s2; 

  s1 = c21*c11/4.0;      
  s6 = lambda*logtmp2*pow(LW,3.0)*c11*c22-lambda*logtmp2*pow(LW,3.0)*c12*c21;      
  s5 = s6+2.0*lambda*logtmp2*LW*c11*c22+2.0*lambda*logtmp2*pow(LW,2.0)*c11*c22;      
  s6 = s5-2.0*lambda*logtmp2*LW*c12*c21-2.0*lambda*logtmp2*pow(LW,2.0)*c12*c21;      
  s4 = s6-4.0*mu*tmp2-4.0*mu*LW*tmp2-2.0*mu*pow(LW,2.0)*tmp2;      
  s5 = s4+4.0*mu*pow(LW,2.0)*c12*c21+4.0*mu*LW*c12*c21-4.0*mu*pow(LW,2.0)*c11*c22-4.0*mu*LW*c11*c22;      
  s3 = s5-2.0*mu*pow(LW,3.0)*c11*c22+lambda*pow(LW,3.0)*c12*c21+lambda*pow(LW,2.0)*c12*c21-lambda*pow(LW,3.0)*c11*c22-lambda*pow(LW,2.0)*c11*c22+2.0*mu*pow(LW,3.0)*c12*c21;      
  s4 = 1/(pow(detr,3.0))/pow(1.0+LW,3.0);      
  s2 = s3*s4;      
  ddpsirdc12dc22 =  s1*s2;
  

  s1 = c12*c22/4.0;      
  s5 = 4.0*mu*pow(LW,2.0)*c12*c21+4.0*mu*LW*c12*c21-4.0*mu*pow(LW,2.0)*c11*c22-4.0*mu*LW*c11*c22;      
  s4 = s5-2.0*mu*pow(LW,3.0)*c11*c22+2.0*mu*pow(LW,3.0)*c12*c21+lambda*pow(LW,2.0)*c12*c21-lambda*pow(LW,3.0)*c11*c22-lambda*pow(LW,2.0)*c11*c22;      
  s5 = s4+lambda*pow(LW,3.0)*c12*c21-4.0*mu*tmp2-4.0*mu*LW*tmp2-2.0*mu*pow(LW,2.0)*tmp2;      
  s6 = s5-2.0*lambda*logtmp2*pow(LW,2.0)*c12*c21-2.0*lambda*logtmp2*LW*c12*c21;      
  s7 = s6+2.0*lambda*logtmp2*pow(LW,2.0)*c11*c22;      
  s3 = s7+2.0*lambda*logtmp2*LW*c11*c22+lambda*logtmp2*pow(LW,3.0)*c11*c22-lambda*logtmp2*pow(LW,3.0)*c12*c21;      
  s4 = 1/(pow(detr,3.0))/pow(1.0+LW,3.0);      
  s2 = s3*s4;      
  ddpsirdc21dc11 = s1*s2;


  s4 = -lambda*logtmp2*pow(LW,3.0)*c11*c11*c22*c22/4.0-lambda*logtmp2*LW*c11*c11*c22*c22/4.0;      
  s3 = s4-lambda*logtmp2*pow(LW,2.0)*c11*c11*c22*c22/2.0+lambda*logtmp2*pow(LW,3.0)*c21*c12*c11*c22/4.0-mu*LW*c21*c21*c12*c12/2.0;      
  s2 = s3+mu*tmp2*c11*c22/2.0+mu*LW*c11*c11*c22*c22/2.0-lambda*pow(LW,3.0)*c21*c21*c12*c12/4.0-lambda*pow(LW,2.0)*c21*c21*c12*c12/4.0+mu*pow(LW,3.0)*c11*c11*c22*c22/2.0;      
  s3 = s2+mu*pow(LW,2.0)*c11*c11*c22*c22+mu*c12*c21*tmp2/2.0+mu*pow(LW,2.0)*tmp2*c11*c22/2.0+mu*LW*tmp2*c11*c22;      
  s4 = s3-mu*pow(LW,2.0)*c21*c12*c11*c22-mu*pow(LW,3.0)*c21*c12*c11*c22/2.0;      
  s1 = s4+lambda*logtmp2*LW*c21*c21*c12*c12/4.0+lambda*logtmp2*pow(LW,2.0)*c21*c12*c11*c22/2.0+lambda*pow(LW,3.0)*c21*c12*c11*c22/4.0+lambda*pow(LW,2.0)*c21*c12*c11*c22/4.0;      
  s2 = 1/(pow(detr,3.0))/pow(1.0+LW,3.0);      
  ddpsirdc21dc12 = s1*s2;

  s1 = -c12*c12/4.0;      
  s5 = 4.0*mu*pow(LW,2.0)*c12*c21+4.0*mu*LW*c12*c21-4.0*mu*pow(LW,2.0)*c11*c22-4.0*mu*LW*c11*c22;      
  s4 = s5-2.0*mu*pow(LW,3.0)*c11*c22+2.0*mu*pow(LW,3.0)*c12*c21+lambda*pow(LW,2.0)*c12*c21-lambda*pow(LW,3.0)*c11*c22-lambda*pow(LW,2.0)*c11*c22;      
  s5 = s4+lambda*pow(LW,3.0)*c12*c21-4.0*mu*tmp2-4.0*mu*LW*tmp2-2.0*mu*pow(LW,2.0)*tmp2;      
  s6 = s5-2.0*lambda*logtmp2*pow(LW,2.0)*c12*c21-2.0*lambda*logtmp2*LW*c12*c21;      
  s7 = s6+2.0*lambda*logtmp2*pow(LW,2.0)*c11*c22;      
  s3 = s7+2.0*lambda*logtmp2*LW*c11*c22+lambda*logtmp2*pow(LW,3.0)*c11*c22-lambda*logtmp2*pow(LW,3.0)*c12*c21;      
  s4 = 1/(pow(detr,3.0))/pow(1.0+LW,3.0);      
  s2 = s3*s4;      
  ddpsirdc21dc21 = s1*s2;

  s1 = c12*c11/4.0;      
  s5 = 4.0*mu*pow(LW,2.0)*c12*c21+4.0*mu*LW*c12*c21-4.0*mu*pow(LW,2.0)*c11*c22-4.0*mu*LW*c11*c22;      
  s4 = s5-2.0*mu*pow(LW,3.0)*c11*c22+2.0*mu*pow(LW,3.0)*c12*c21+lambda*pow(LW,2.0)*c12*c21-lambda*pow(LW,3.0)*c11*c22-lambda*pow(LW,2.0)*c11*c22;      
  s5 = s4+lambda*pow(LW,3.0)*c12*c21-4.0*mu*tmp2-4.0*mu*LW*tmp2-2.0*mu*pow(LW,2.0)*tmp2;      
  s6 = s5-2.0*lambda*logtmp2*pow(LW,2.0)*c12*c21-2.0*lambda*logtmp2*LW*c12*c21;      
  s7 = s6+2.0*lambda*logtmp2*pow(LW,2.0)*c11*c22;      
  s3 = s7+2.0*lambda*logtmp2*LW*c11*c22+lambda*logtmp2*pow(LW,3.0)*c11*c22-lambda*logtmp2*pow(LW,3.0)*c12*c21;      
  s4 = 1/(pow(detr,3.0))/pow(1.0+LW,3.0);      
  s2 = s3*s4;      
  ddpsirdc21dc22 = s1*s2;

  s3 = mu*LW*c22*c22*c11*c11/2.0-mu*pow(LW,3.0)*c12*c12*c21*c21/2.0-mu*pow(LW,2.0)*c12*c12*c21*c21-mu*LW*c12*c12*c21*c21/2.0+lambda*pow(LW,2.0)*c22*c22*c11*c11/4.0;      
  s4 = s3+lambda*pow(LW,3.0)*c22*c22*c11*c11/4.0+mu*LW*tmp2*c12*c21;      
  s2 = s4+mu*c11*c22*tmp2/2.0-lambda*logtmp2*pow(LW,3.0)*c22*c11*c12*c21/4.0-lambda*logtmp2*pow(LW,2.0)*c22*c11*c12*c21/2.0;      
  s3 = s2-lambda*pow(LW,3.0)*c22*c11*c12*c21/4.0-lambda*pow(LW,2.0)*c22*c11*c12*c21/4.0-lambda*logtmp2*LW*c22*c22*c11*c11/4.0+mu*pow(LW,3.0)*c22*c11*c12*c21/2.0;      
  s4 = s3+mu*pow(LW,2.0)*c22*c11*c12*c21+mu*tmp2*c12*c21/2.0;      
  s5 = s4+mu*pow(LW,2.0)*tmp2*c12*c21/2.0;      
  s1 = s5+lambda*logtmp2*pow(LW,2.0)*c12*c12*c21*c21/2.0+lambda*logtmp2*pow(LW,3.0)*c12*c12*c21*c21/4.0+lambda*logtmp2*LW*c12*c12*c21*c21/4.0;      
  s2 = 1/(pow(detr,3.0))/pow(1.0+LW,3.0);      
  ddpsirdc22dc11 = s1*s2;

  s1 = c11*c21/4.0;      
  s6 = -4.0*mu*LW*tmp2-2.0*lambda*logtmp2*LW*c12*c21;      
  s5 = s6-2.0*lambda*logtmp2*pow(LW,2.0)*c12*c21+2.0*lambda*logtmp2*LW*c11*c22;      
  s6 = s5+2.0*lambda*logtmp2*pow(LW,2.0)*c11*c22-2.0*mu*pow(LW,2.0)*tmp2;      
  s4 = s6-lambda*logtmp2*pow(LW,3.0)*c12*c21+lambda*logtmp2*pow(LW,3.0)*c11*c22-2.0*mu*pow(LW,3.0)*c11*c22;      
  s5 = s4+4.0*mu*pow(LW,2.0)*c12*c21+4.0*mu*LW*c12*c21-4.0*mu*pow(LW,2.0)*c11*c22-4.0*mu*LW*c11*c22;      
  s3 = s5+lambda*pow(LW,3.0)*c12*c21+lambda*pow(LW,2.0)*c12*c21-lambda*pow(LW,3.0)*c11*c22-lambda*pow(LW,2.0)*c11*c22+2.0*mu*pow(LW,3.0)*c12*c21-4.0*mu*tmp2;      
  s4 = 1/(pow(detr,3.0))/pow(1.0+LW,3.0);      
  ddpsirdc22dc12 = s1*s2;

  s1 = c11*c12/4.0;      
  s6 = -4.0*mu*LW*tmp2-2.0*lambda*logtmp2*LW*c12*c21;      
  s5 = s6-2.0*lambda*logtmp2*pow(LW,2.0)*c12*c21+2.0*lambda*logtmp2*LW*c11*c22;      
  s6 = s5+2.0*lambda*logtmp2*pow(LW,2.0)*c11*c22-2.0*mu*pow(LW,2.0)*tmp2;      
  s4 = s6-lambda*logtmp2*pow(LW,3.0)*c12*c21+lambda*logtmp2*pow(LW,3.0)*c11*c22-2.0*mu*pow(LW,3.0)*c11*c22;      
  s5 = s4+4.0*mu*pow(LW,2.0)*c12*c21+4.0*mu*LW*c12*c21-4.0*mu*pow(LW,2.0)*c11*c22-4.0*mu*LW*c11*c22;      
  s3 = s5+lambda*pow(LW,3.0)*c12*c21+lambda*pow(LW,2.0)*c12*c21-lambda*pow(LW,3.0)*c11*c22-lambda*pow(LW,2.0)*c11*c22+2.0*mu*pow(LW,3.0)*c12*c21-4.0*mu*tmp2;      
  s4 = 1/(pow(detr,3.0))/pow(1.0+LW,3.0);      
  s2 = s3*s4;      
  ddpsirdc22dc21 = s1*s2;

  s1 = -c11*c11/4.0;      
  s6 = -4.0*mu*LW*tmp2-2.0*lambda*logtmp2*LW*c12*c21;      
  s5 = s6-2.0*lambda*logtmp2*pow(LW,2.0)*c12*c21+2.0*lambda*logtmp2*LW*c11*c22;      
  s6 = s5+2.0*lambda*logtmp2*pow(LW,2.0)*c11*c22-2.0*mu*pow(LW,2.0)*tmp2;      
  s4 = s6-lambda*logtmp2*pow(LW,3.0)*c12*c21+lambda*logtmp2*pow(LW,3.0)*c11*c22-2.0*mu*pow(LW,3.0)*c11*c22;      
  s5 = s4+4.0*mu*pow(LW,2.0)*c12*c21+4.0*mu*LW*c12*c21-4.0*mu*pow(LW,2.0)*c11*c22-4.0*mu*LW*c11*c22;      
  s3 = s5+lambda*pow(LW,3.0)*c12*c21+lambda*pow(LW,2.0)*c12*c21-lambda*pow(LW,3.0)*c11*c22-lambda*pow(LW,2.0)*c11*c22+2.0*mu*pow(LW,3.0)*c12*c21-4.0*mu*tmp2;      
  s4 = 1/(pow(detr,3.0))/pow(1.0+LW,3.0);      
  s2 = s3*s4;      
  ddpsirdc22dc22 = s1*s2;

  for ( j = 0 ; j < FACET_VERTS  ; j++ )
     for ( i = 0 ; i < SDIM  ; i++ )
        for ( jj = 0 ; jj < FACET_VERTS  ; jj++ )
        {
          f_info->hess[j][jj][i][i] += 
                  area*(dpsirdc11*ddc11dv[j][i][jj]
                       +dpsirdc12*ddc21dv[j][i][jj]
                       +dpsirdc21*ddc12dv[j][i][jj]
                       +dpsirdc22*ddc22dv[j][i][jj]);

          for ( ii = 0 ; ii < SDIM  ; ii++ )
             f_info->hess[j][jj][i][ii] += 
            area*((ddpsirdc11dc11+ddpsirdc12dc11+ddpsirdc21dc11+ddpsirdc22dc11)*dc11dv[j][i] 
                 +(ddpsirdc11dc12+ddpsirdc12dc12+ddpsirdc21dc12+ddpsirdc22dc12)*dc12dv[j][i]
                 +(ddpsirdc11dc21+ddpsirdc12dc21+ddpsirdc21dc21+ddpsirdc22dc21)*dc21dv[j][i]
                 +(ddpsirdc11dc22+ddpsirdc12dc22+ddpsirdc21dc22+ddpsirdc22dc22)*dc22dv[j][i]);
        }

  return energy;
  }

/* Lambert W function. 
   Was ~/C/LambertW.c written K M Briggs Keith dot Briggs at bt dot com 97 May 21.  
   Revised KMB 97 Nov 20; 98 Feb 11, Nov 24, Dec 28; 99 Jan 13; 00 Feb 23; 01 Apr 09

   Computes Lambert W function, principal branch.
   See LambertW1.c for -1 branch.

   Returned value W(z) satisfies W(z)*exp(W(z))=z
   test data...
      W(1)= 0.5671432904097838730
      W(2)= 0.8526055020137254914
      W(20)=2.2050032780240599705
   To solve (a+b*R)*exp(-c*R)-d=0 for R, use
   R=-(b*W(-exp(-a*c/b)/b*d*c)+a*c)/b/c

   Test: 
     gcc -DTESTW LambertW.c -o LambertW -lm && LambertW
   Library:
     gcc -O3 -c LambertW.c 
*/

REAL LambertW(z)
REAL z;
 {
  int i; 
  const REAL eps=4.0e-16, em1=0.3678794411714423215955237701614608; 
  REAL p,e,t,w;
  if (z<-em1 || !is_finite(z)) { 
    fprintf(stderr,"LambertW: bad argument %g, exiting.\n",z); exit(1); 
  }

  if (0.0==z) return 0.0;
  if (z<-em1+1e-4) { /* series near -em1 in sqrt(q) */
    double q=z+em1,r=sqrt(q),q2=q*q,q3=q2*q;
    w = -1.0
     +2.331643981597124203363536062168*r
     -1.812187885639363490240191647568*q
     +1.936631114492359755363277457668*r*q
     -2.353551201881614516821543561516*q2
     +3.066858901050631912893148922704*r*q2
     -4.175335600258177138854984177460*q3
     +5.858023729874774148815053846119*r*q3
     -8.401032217523977370984161688514*q3*q;  /* error approx 1e-16 */
     fprintf(stderr,"LambertW expression 1: W(%15.12g) = %15.12g \n",z,w); 
	 return w;

  }
  /* initial approx for iteration... */
  if (z<1.0) { /* series near 0 */
    p=sqrt(2.0*(2.7182818284590452353602874713526625*z+1.0));
    w=-1.0+p*(1.0+p*(-0.333333333333333333333+p*0.152777777777777777777777)); 
  } else 
    w=log(z); /* asymptotic */
  if (z>3.0) w-=log(w); /* useful? */
  for (i=0; i<10; i++) { /* Halley iteration */
    e=exp(w); 
    t=w*e-z;
    p=w+1.0;
    t/=e*p-0.5*(p+1.0)*t/p; 
    w-=t;
    if (fabs(t)<eps*(1.0+fabs(w))) return w; /* rel-abs error */
  }
  /* should never get here */
  sprintf(errmsg,"LambertW: No convergence at z=%g, exiting.\n",z); 
  kb_error(3747,errmsg,RECOVERABLE);

  return 0; /* keep compilers happy */
}

/**************************************************************
*
*  function: Neo_Hookean_energy()
*
*  purpose: calculates energy of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/
REAL Neo_Hookean_energy(f_info)
struct qinfo *f_info;
{
 return Neo_Hookean_all(f_info,METHOD_VALUE);
}
/**************************************************************
*
*  function: Neo_Hookean_gradient()
*
*  purpose: calculates gradient of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/
REAL Neo_Hookean_gradient(f_info)
struct qinfo *f_info;
{
 return Neo_Hookean_all(f_info,METHOD_GRADIENT);
}
/**************************************************************
*
*  function: Neo_Hookean_hessian()
*
*  purpose: calculates hessian of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/
REAL Neo_Hookean_hessian(f_info)
struct qinfo *f_info;
{
 return Neo_Hookean_all(f_info,METHOD_HESSIAN);
}


/***********************************************************************/
/************************************************************************
     Named method: general_linear_elastic

     Linear elastic strain energy on facets, fully general model.

     Let S be Gram matrix of unstrained facet (dots of sides).
     Let Q be the inverse of S.
     Let F be Gram matrix of strained facet.
     Let C = (FQ-I)/2, the Cauchy-Green strain tensor.
     Let v be Poisson ratio.
     Then energy density is 
        C_ij K_ijkl C_kl / 2
     Each facet has extra attribute array elastic_coeff[6] in order
     { xx*xx, yy*yy, xx*yy, xx*xy, yy*xy, xy*xy }
     for its K_ijkl (should not include factors of 2 and 4 due to symmetry)
     and extra attribute array form_factors[3] = {s11,s12,s22}
     where sij = dot(si,sj) and s1 = (v1-v0), s2 = (v2-v0).

************************************************************************/

#define ELASTIC_COEFF_NAME "elastic_coeff"
#define ELASTIC_BASE_NAME "elastic_basis"
static int coeff_attr; /* number of elastic_coeff extra attribute */
static int elastic_base_attr; /* number of form_factors extra attribute */

/***************************************************************
*
*  function: general_linear_elastic_init()
*
*  purpose: Make sure needed extra attributes are present.
*              
*
*/

void general_linear_elastic_init(mode,mi)
int mode; /* energy or gradient */
struct method_instance *mi;
{ struct extra *ex;

  if ( web.modeltype != LINEAR )
     kb_error(4015,"general_linear_elastic method only for LINEAR model.\n",
       RECOVERABLE);

  if ( web.dimension != 2 ) 
     kb_error(4022,"general_linear_elastic method only for SOAPFILM model.\n",
        RECOVERABLE);

  /* extra edge atribute */
  coeff_attr = find_attribute(FACET,ELASTIC_COEFF_NAME);
  if ( coeff_attr < 0 ) /* not found */
     kb_error(3201,"Facet extra attribute elastic_coeff missing. Needed by general_linear_elastic.\n",RECOVERABLE);

  elastic_base_attr = find_attribute(FACET,ELASTIC_BASE_NAME);
  if ( elastic_base_attr < 0 ) /* not found */
     kb_error(3203,"Facet extra attribute elastic_basis real[2][2] missing. Needed by general_linear_elastic.\n",RECOVERABLE);

  ex = &EXTRAS(FACET)[elastic_base_attr];
  if ( ex->array_spec.dim != 2 || ex->array_spec.sizes[0] != 2  || ex->array_spec.sizes[1] != 2 )
     kb_error(3204,
      "Facet extra attribute elastic_basis must have dimension [2][2].\n",
        RECOVERABLE);

  ex = &EXTRAS(FACET)[coeff_attr];
  if ( ex->array_spec.dim != 1 || ex->array_spec.sizes[0] != 6 )
    kb_error(3205,
      "Facet extra attribute elastic_coeff must have 1 dimension and size 6.\n",
        RECOVERABLE);
}

/************************************************************************
*
* function: general_linear_elastic_all()
*
* purpose: energy, gradient, and hessian for linear_elastic method.
*/
REAL general_linear_elastic_all ARGS((struct qinfo *,int));

REAL general_linear_elastic_all(f_info,mode)
struct qinfo *f_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */
{
  REAL s[2][2],*sptr;  /* pointer to extra attributes */
  REAL **side; 
  REAL q11,q12,q21,q22;  /* Q entries */
  REAL det;    /* det S */
  REAL area;  /* reference area of facet */
  REAL * coeff;  /* elastic coeff extra attribute */
  REAL elastic_coeff[2][2][2][2];
  REAL f11,f12,f21,f22;
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

  coeff = (REAL*)get_extra(f_info->id,coeff_attr);
  elastic_coeff[0][0][0][0] = coeff[0];
  elastic_coeff[1][1][1][1] = coeff[1];
  elastic_coeff[0][0][1][1] = coeff[2];
  elastic_coeff[1][1][0][0] = coeff[2];
  elastic_coeff[0][0][0][1] = coeff[3]/2;
  elastic_coeff[0][0][1][0] = coeff[3]/2;
  elastic_coeff[0][1][0][0] = coeff[3]/2;
  elastic_coeff[1][0][0][0] = coeff[3]/2;
  elastic_coeff[1][1][1][0] = coeff[4]/2;
  elastic_coeff[0][1][1][1] = coeff[4]/2;
  elastic_coeff[1][0][1][1] = coeff[4]/2;
  elastic_coeff[1][1][0][1] = coeff[4]/2;
  elastic_coeff[1][0][1][0] = coeff[5]/4;
  elastic_coeff[1][0][0][1] = coeff[5]/4;
  elastic_coeff[0][1][0][1] = coeff[5]/4;
  elastic_coeff[0][1][1][0] = coeff[5]/4;

  /* get unstrained sides in columns of S and get inverse */
  sptr = (REAL*)get_extra(f_info->id,elastic_base_attr);
  s[0][0] = sptr[0]; s[1][0] = sptr[1]; s[0][1] = sptr[2]; s[1][1] = sptr[3];
  det = s[0][0]*s[1][1] - s[0][1]*s[1][0];
  if ( det == 0.0 )
  { if ( mode == METHOD_VALUE ) return 0.0;
    sprintf(errmsg,
      "general_linear_elastic: Facet %s has unstrained area 0.\n",
        ELNAME(f_info->id));
    kb_error(3206,errmsg,RECOVERABLE);
  }
  area = fabs(det)/2;
  q11 = s[1][1]/det; q12 = -s[0][1]/det; q21 = -s[1][0]/det; q22 = s[0][0]/det;

  /* Gram matrix of strained sides */
  side = f_info->sides[0];
  f11 = SDIM_dot(side[0],side[0]);
  f21 = f12 = SDIM_dot(side[0],side[1]);
  f22 = SDIM_dot(side[1],side[1]);

  /* Strain matrix */
  c11 = (q11*f11*q11 + q11*f12*q21 + q21*f21*q11 + q21*f22*q21 - 1)/2;
  c12 = (q11*f11*q12 + q11*f12*q22 + q21*f21*q12 + q21*f22*q22)/2;
  c21 = c12;
  c22 = (q12*f11*q12 + q12*f12*q22 + q22*f21*q12 + q22*f22*q22 - 1)/2;

  energy = c11*c11*elastic_coeff[0][0][0][0] 
         + c11*c12*elastic_coeff[0][0][0][1] 
         + c11*c22*elastic_coeff[0][0][1][1] 
         + c11*c21*elastic_coeff[0][0][1][0] 
         + c12*c11*elastic_coeff[0][1][0][0] 
         + c12*c12*elastic_coeff[0][1][0][1] 
         + c12*c22*elastic_coeff[0][1][1][1] 
         + c12*c21*elastic_coeff[0][1][1][0] 
         + c21*c11*elastic_coeff[1][0][0][0] 
         + c21*c12*elastic_coeff[1][0][0][1] 
         + c21*c22*elastic_coeff[1][0][1][1] 
         + c21*c21*elastic_coeff[1][0][1][0] 
         + c22*c11*elastic_coeff[1][1][0][0] 
         + c22*c12*elastic_coeff[1][1][0][1] 
         + c22*c22*elastic_coeff[1][1][1][1] 
         + c22*c21*elastic_coeff[1][1][1][0] ;
  energy *= area/2;
   

  if ( mode == METHOD_VALUE ) return energy;

  /* gradient */

  for ( i = 0 ; i < SDIM  ; i++ )
  {

  dc11dv[1][i] = (q11*2*side[0][i]*q11 + q11*side[1][i]*q21 + q21*side[1][i]*q11                  )/2;
  dc11dv[2][i] = (q11*side[0][i]*q21 + q21*side[0][i]*q11 + q21*2*side[1][i]*q21                 )/2;
  dc12dv[1][i] = (q11*2*side[0][i]*q12 + q11*side[1][i]*q22 + q21*side[1][i]*q12                 )/2;
  dc12dv[2][i] = (q11*side[0][i]*q22 + q21*side[0][i]*q12 + q21*2*side[1][i]*q22                 )/2;
  dc21dv[1][i] = dc12dv[1][i];
  dc21dv[2][i] = dc12dv[2][i];
  dc22dv[1][i] = (q12*2*side[0][i]*q12 + q12*side[1][i]*q22 + q22*side[1][i]*q12                 )/2;
  dc22dv[2][i] = (q12*side[0][i]*q22 + q22*side[0][i]*q12 + q22*2*side[1][i]*q22                 )/2;
     dc11dv[0][i] = -(dc11dv[1][i] + dc11dv[2][i]);
     dc12dv[0][i] = -(dc12dv[1][i] + dc12dv[2][i]);
     dc21dv[0][i] = -(dc21dv[1][i] + dc21dv[2][i]);
     dc22dv[0][i] = -(dc22dv[1][i] + dc22dv[2][i]);
  }


  for ( j = 0 ; j < FACET_VERTS  ; j++ )
     for ( i = 0 ; i < SDIM  ; i++ )
     { f_info->grad[j][i] = area*(
           c11*dc11dv[j][i]*elastic_coeff[0][0][0][0] 
         + c11*dc12dv[j][i]*elastic_coeff[0][0][0][1] 
         + c11*dc22dv[j][i]*elastic_coeff[0][0][1][1] 
         + c11*dc21dv[j][i]*elastic_coeff[0][0][1][0] 
         + c12*dc11dv[j][i]*elastic_coeff[0][1][0][0] 
         + c12*dc12dv[j][i]*elastic_coeff[0][1][0][1] 
         + c12*dc22dv[j][i]*elastic_coeff[0][1][1][1] 
         + c12*dc21dv[j][i]*elastic_coeff[0][1][1][0] 
         + c21*dc11dv[j][i]*elastic_coeff[1][0][0][0] 
         + c21*dc12dv[j][i]*elastic_coeff[1][0][0][1] 
         + c21*dc22dv[j][i]*elastic_coeff[1][0][1][1] 
         + c21*dc21dv[j][i]*elastic_coeff[1][0][1][0] 
         + c22*dc11dv[j][i]*elastic_coeff[1][1][0][0] 
         + c22*dc12dv[j][i]*elastic_coeff[1][1][0][1] 
         + c22*dc22dv[j][i]*elastic_coeff[1][1][1][1] 
         + c22*dc21dv[j][i]*elastic_coeff[1][1][1][0]);
      /* don't need to divide by 2 here, since only doing 
         derivative of second term and using symmetry */
     }

  if ( mode == METHOD_GRADIENT ) return energy;

  /* hessian */

  for ( i = 0 ; i < SDIM  ; i++ )
  {
     ddc11dv[1][i][1] = (q11*2*q11)/2;
     ddc11dv[1][i][2] = ( q11*q21 + q21*q11 )/2;

     ddc11dv[2][i][1] = (q11*q21 + q21*q11 )/2;
     ddc11dv[2][i][2] = (q21*2*q21)/2;
   
     ddc12dv[1][i][1] = (q11*2*q12 )/2;
     ddc12dv[1][i][2] = ( q11*q22 + q21*q12 )/2;

  ddc12dv[2][i][1] = (q11*q22 + q21*q12)/2;
  ddc12dv[2][i][2] = (q21*2*q22 )/2;

  ddc21dv[1][i][1] = ddc12dv[1][i][1];
  ddc21dv[1][i][2] = ddc12dv[1][i][2];

     ddc21dv[2][i][1] = ddc12dv[2][i][1];
     ddc21dv[2][i][2] = ddc12dv[2][i][2];

     ddc22dv[1][i][1] = (q12*2*q12)/2;
     ddc22dv[1][i][2] = (q12*q22 + q22*q12)/2;

     ddc22dv[2][i][1] = (q12*q22 + q22*q12)/2;
     ddc22dv[2][i][2] = (q22*2*q22)/2;

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
          f_info->hess[j][jj][i][i] += area*(
           c11*ddc11dv[j][i][jj]*elastic_coeff[0][0][0][0] 
         + c11*ddc12dv[j][i][jj]*elastic_coeff[0][0][0][1] 
         + c11*ddc22dv[j][i][jj]*elastic_coeff[0][0][1][1] 
         + c11*ddc21dv[j][i][jj]*elastic_coeff[0][0][1][0] 
         + c12*ddc11dv[j][i][jj]*elastic_coeff[0][1][0][0] 
         + c12*ddc12dv[j][i][jj]*elastic_coeff[0][1][0][1] 
         + c12*ddc22dv[j][i][jj]*elastic_coeff[0][1][1][1] 
         + c12*ddc21dv[j][i][jj]*elastic_coeff[0][1][1][0] 
         + c21*ddc11dv[j][i][jj]*elastic_coeff[1][0][0][0] 
         + c21*ddc12dv[j][i][jj]*elastic_coeff[1][0][0][1] 
         + c21*ddc22dv[j][i][jj]*elastic_coeff[1][0][1][1] 
         + c21*ddc21dv[j][i][jj]*elastic_coeff[1][0][1][0] 
         + c22*ddc11dv[j][i][jj]*elastic_coeff[1][1][0][0] 
         + c22*ddc12dv[j][i][jj]*elastic_coeff[1][1][0][1] 
         + c22*ddc22dv[j][i][jj]*elastic_coeff[1][1][1][1] 
         + c22*ddc21dv[j][i][jj]*elastic_coeff[1][1][1][0]);


          for ( ii = 0 ; ii < SDIM  ; ii++ )
             f_info->hess[j][jj][i][ii] += area*(
                 dc11dv[jj][ii]*dc11dv[j][i]*elastic_coeff[0][0][0][0] 
               + dc11dv[jj][ii]*dc12dv[j][i]*elastic_coeff[0][0][0][1] 
               + dc11dv[jj][ii]*dc22dv[j][i]*elastic_coeff[0][0][1][1] 
               + dc11dv[jj][ii]*dc21dv[j][i]*elastic_coeff[0][0][1][0] 
               + dc12dv[jj][ii]*dc11dv[j][i]*elastic_coeff[0][1][0][0] 
               + dc12dv[jj][ii]*dc12dv[j][i]*elastic_coeff[0][1][0][1] 
               + dc12dv[jj][ii]*dc22dv[j][i]*elastic_coeff[0][1][1][1] 
               + dc12dv[jj][ii]*dc21dv[j][i]*elastic_coeff[0][1][1][0] 
               + dc21dv[jj][ii]*dc11dv[j][i]*elastic_coeff[1][0][0][0] 
               + dc21dv[jj][ii]*dc12dv[j][i]*elastic_coeff[1][0][0][1] 
               + dc21dv[jj][ii]*dc22dv[j][i]*elastic_coeff[1][0][1][1] 
               + dc21dv[jj][ii]*dc21dv[j][i]*elastic_coeff[1][0][1][0] 
               + dc22dv[jj][ii]*dc11dv[j][i]*elastic_coeff[1][1][0][0] 
               + dc22dv[jj][ii]*dc12dv[j][i]*elastic_coeff[1][1][0][1] 
               + dc22dv[jj][ii]*dc22dv[j][i]*elastic_coeff[1][1][1][1] 
               + dc22dv[jj][ii]*dc21dv[j][i]*elastic_coeff[1][1][1][0]);

         }

  return energy;
}  /* end general_linear_elastic_all() */

/**************************************************************
*
*  function: general_linear_elastic_energy()
*
*  purpose: calculates energy of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL general_linear_elastic_energy(f_info)
struct qinfo *f_info;
{
 return general_linear_elastic_all(f_info,METHOD_VALUE);
}



/**************************************************************
*
*  function: general_linear_elastic_gradient()
*
*  purpose: calculates gradient of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL general_linear_elastic_gradient(f_info)
struct qinfo *f_info;
{
 return general_linear_elastic_all(f_info,METHOD_GRADIENT);
}


/**************************************************************
*
*  function: general_linear_elastic_hessian()
*
*  purpose: calculates hessian of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL general_linear_elastic_hessian(f_info)
struct qinfo *f_info;
{
 return general_linear_elastic_all(f_info,METHOD_HESSIAN);
}


/***********************************************************************/
/************************************************************************
     Named method: dirichlet_elastic

     Dirichlet elastic strain energy on facets.
     For conformal mappings.

     Let S be Gram matrix of unstrained facet (dots of sides).
     Let Q be the inverse of S.
     Let F be Gram matrix of strained facet.
     Let C = FQ, the linear deformation matrix
     Then energy density is 
       Tr(CC^T)

     Each facet extra attribute array form_factors[3] = {s11,s12,s22}
     where sij = dot(si,sj) and s1 = (v1-v0), s2 = (v2-v0).

************************************************************************/

#define FORM_FACTORS_NAME "form_factors"
extern int form_factors_attr; /* number of form_factors extra attribute */

/***************************************************************
*
*  function: dirichlet_elastic_init()
*
*  purpose: Make sure needed extra attributes are present.
*              
*
*/

void dirichlet_elastic_init(mode,mi)
int mode; /* energy or gradient or hessian */
struct method_instance *mi;
{ 

  if ( web.modeltype != LINEAR )
     kb_error(4529,"dirichlet_elastic method only for LINEAR model.\n",RECOVERABLE);

  if ( web.dimension != 2 )
     kb_error(4150,"dirichlet_elastic method only for SOAPFILM model.\n",
        RECOVERABLE);

  form_factors_attr = find_attribute(FACET,FORM_FACTORS_NAME);
  if ( form_factors_attr < 0 ) /* not found */
     kb_error(4152,"Facet extra attribute form_factors real[3] missing. Needed by dirichlet_elastic.\n",RECOVERABLE);

  if ( EXTRAS(FACET)[form_factors_attr].array_spec.datacount < 3 )
     kb_error(4153,"Facet extra attribute form_factors must have size 3.\n",
        RECOVERABLE);
}

/************************************************************************
*
* function: dirichlet_elastic_all()
*
* purpose: energy, gradient, and hessian for dirichlet_elastic method.
*/
REAL dirichlet_elastic_all ARGS((struct qinfo *,int));

REAL dirichlet_elastic_all(f_info,mode)
struct qinfo *f_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */
{
  REAL *s;  /* pointer to extra attributes */
  REAL **side; 
  REAL q11,q12,q22;  /* Q entries */
  REAL det;    /* det S */
  REAL area;  /* reference area of facet */
  REAL f11,f12,f22;
  REAL c11,c22;
  REAL energy;
  REAL dc11dv[FACET_VERTS][MAXCOORD];
  REAL dc22dv[FACET_VERTS][MAXCOORD];
  REAL ddc11dv[FACET_VERTS][MAXCOORD][FACET_VERTS];
  REAL ddc22dv[FACET_VERTS][MAXCOORD][FACET_VERTS];
  int i,j,jj;

  s = (REAL*)get_extra(f_info->id,form_factors_attr);
  det = s[0]*s[2] - s[1]*s[1];
  if ( det <= 0.0 )
  { if ( mode == METHOD_VALUE ) return 0.0;
    sprintf(errmsg,"dirichlet_elastic: Facet %s has unstrained area <= 0.\n",
      ELNAME(f_info->id));
    kb_error(4154,errmsg,RECOVERABLE);
  }
  area = sqrt(det)/2;
  q11 = s[2]/det; q12 = -s[1]/det; q22 = s[0]/det;

  side = f_info->sides[0];
  f11 = SDIM_dot(side[0],side[0]);
  f12 = SDIM_dot(side[0],side[1]);
  f22 = SDIM_dot(side[1],side[1]);

  c11 = f11*q11 + f12*q12;
  c22 = f12*q12 + f22*q22;

  energy = (c11 + c22)*area; 

  if ( mode == METHOD_VALUE ) return energy;

  /* gradient */

  for ( i = 0 ; i < SDIM  ; i++ )
  {
     dc11dv[1][i] = 2*side[0][i]*q11 + side[1][i]*q12;
     dc11dv[2][i] =                          side[0][i]*q12;
     dc22dv[1][i] = side[1][i]*q12;
     dc22dv[2][i] = side[0][i]*q12 + 2*side[1][i]*q22;
     dc11dv[0][i] = -(dc11dv[1][i] + dc11dv[2][i]);
     dc22dv[0][i] = -(dc22dv[1][i] + dc22dv[2][i]);
  }


  for ( j = 0 ; j < FACET_VERTS  ; j++ )
     for ( i = 0 ; i < SDIM  ; i++ )
     { f_info->grad[j][i] = (dc11dv[j][i] + dc22dv[j][i]) * area;
     }

  if ( mode == METHOD_GRADIENT ) return energy;

  /* hessian */

  for ( i = 0 ; i < SDIM  ; i++ )
  {
     ddc11dv[1][i][1] = 2*q11;
     ddc11dv[1][i][2] = q12;

     ddc11dv[2][i][1] = q12;
     ddc11dv[2][i][2] = 0.0;

     ddc22dv[1][i][1] = 0.0;
     ddc22dv[1][i][2] = q12;

     ddc22dv[2][i][1] = q12;
     ddc22dv[2][i][2] = 2*q22;

     for ( j = 1 ; j < FACET_VERTS;  j++ )
     { 
        ddc11dv[0][i][j] = -(ddc11dv[1][i][j] + ddc11dv[2][i][j]);
        ddc22dv[0][i][j] = -(ddc22dv[1][i][j] + ddc22dv[2][i][j]);
        ddc11dv[j][i][0] = -(ddc11dv[j][i][1] + ddc11dv[j][i][2]);
        ddc22dv[j][i][0] = -(ddc22dv[j][i][1] + ddc22dv[j][i][2]);
     }
     ddc11dv[0][i][0] = -(ddc11dv[1][i][0] + ddc11dv[2][i][0]);
     ddc22dv[0][i][0] = -(ddc22dv[1][i][0] + ddc22dv[2][i][0]);
  }

  for ( j = 0 ; j < FACET_VERTS  ; j++ )
     for ( i = 0 ; i < SDIM  ; i++ )
        for ( jj = 0 ; jj < FACET_VERTS  ; jj++ )
        {
          f_info->hess[j][jj][i][i] += 
                (ddc11dv[j][i][jj] + ddc22dv[j][i][jj]) * area;

         }

  return energy;
}

/**************************************************************
*
*  function: dirichlet_elastic_energy()
*
*  purpose: calculates energy of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL dirichlet_elastic_energy(f_info)
struct qinfo *f_info;
{
 return dirichlet_elastic_all(f_info,METHOD_VALUE);
}



/**************************************************************
*
*  function: dirichlet_elastic_gradient()
*
*  purpose: calculates gradient of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL dirichlet_elastic_gradient(f_info)
struct qinfo *f_info;
{
 return dirichlet_elastic_all(f_info,METHOD_GRADIENT);
}


/**************************************************************
*
*  function: dirichlet_elastic_hessian()
*
*  purpose: calculates hessian of one facet due to potential
*
*  input: info about vertex is in qinfo structure.
*
*/

REAL dirichlet_elastic_hessian(f_info)
struct qinfo *f_info;
{
 return dirichlet_elastic_all(f_info,METHOD_HESSIAN);
}

