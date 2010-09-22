/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/**********************************************************************
*
*  File: sqcurve3.c
*
*  Purpose: Does calculations needed for including square curvature
*              in energy. Linear model only.
*              Named quantity methods.
*
*        Vertex star approach.
*
*        Assumes complete star around vertex. v_info comes in with
*        central vertex first, followed by ring in order.  Sides
*        are in same order.
*/

#include "include.h"

/* some timing results:

    4-processor 4400 SGI (polyphemus)
    Old methods used processors in 2:1:1:1 ratio.  New are 1:1:1:1
    So would scale better to more procs.
    PLAIN:
    Star methods take about same time as old methods, but better load balance
    EFFECTIVE AREA, NORMAL_SQ
     New methods take same clock time, more total time.
    */  

/* local storage max */
#define MAXV 20

REAL star_sqcurve_method_all ARGS((struct qinfo*,int));
static int h0_flag; /* set to use (H - H_0)^2 */
static REAL h0_value;  /* value of H_0 */


/***********************************************************************
*
*  Function: star_sqcurve_method_init()
*
*  Purpose: Initializes data structures for square curvature.
*/

void star_sqcurve_method_init(mode,mi)
int mode; /* METHOD_VALUE or METHOD_GRADIENT */
struct method_instance *mi;
{ int k,n;
  struct gen_quant_method *gm;
  int eltype;

  if ( web.dimension != 2 )
    kb_error(1610,"sq_mean_curvature method only for 2D facets.\n",RECOVERABLE);

  if ( web.modeltype != LINEAR )
     kb_error(1611,"sq_mean_curvature method only for LINEAR model.\n",
        RECOVERABLE);

  if ( everything_quantities_flag & square_curvature_flag )
     GEN_QUANT(sq_mean_curv_quantity_num)->modulus = 
        globals(square_curvature_param)->value.real;

  /* see if using (H - H_0)^2 adjustment */
  h0_flag = 0;
  h0_attr = find_extra("h_zero",&eltype);
  if ( h0_attr >= 0 )
    h0_flag = H0_IN_ATTR; 
  else
  { k = lookup_global("h_zero");
    if ( k >= 0 ) 
    { h0_flag = H0_IN_GLOBAL;
      h0_value = globals(k)->value.real;
    }
  }

  if ( star_sq_mean_curvature_mi < 0 )
  { /* see what method indices correspond to what methods */
     for ( n=0,gm = basic_gen_methods ; gm->name[0] != ' ' ; gm++,n++ )
     { if ( stricmp(gm->name,"star_sq_mean_curvature") == 0 ) 
            star_sq_mean_curvature_mi = n;
       if ( stricmp(gm->name,"star_eff_area_sq_mean_curvature") == 0 ) 
            star_eff_area_sq_mean_curvature_mi = n;
       if ( stricmp(gm->name,"star_normal_sq_mean_curvature") == 0 ) 
            star_normal_sq_mean_curvature_mi = n;
       if ( stricmp(gm->name,"star_perp_sq_mean_curvature") == 0 ) 
            star_perp_sq_mean_curvature_mi = n;
     }
  }
  if ( mi->gen_method == star_eff_area_sq_mean_curvature_mi )
      if ( SDIM != 3 )
             kb_error(1612,"star_eff_area_sq_mean_curvature method only for 3D space.\n",
                RECOVERABLE);
  if ( mi->gen_method == star_normal_sq_mean_curvature_mi )
   if ( SDIM != 3 )
     kb_error(1613,"star_normal_sq_mean_curvature method only for 3D space.\n",
                RECOVERABLE);

  if ( mi->gen_method == star_perp_sq_mean_curvature_mi )
   if ( SDIM != 3 )
     kb_error(2810,"star_perp_sq_mean_curvature method only for 3D space.\n",
                RECOVERABLE);

  if ( self_similar_flag )
  { int param = lookup_global(SELFSIM_NAME);
    kb_error(1614,"Cannot use self-similarity with star_sq_... methods.\n",
      RECOVERABLE);

    if ( param < 0 ) /* missing, so add */
    { param = add_global(SELFSIM_NAME);
      globals(param)->value.real = 1.0;  /* default */
      globals(param)->flags |=  ORDINARY_PARAM | RECALC_PARAMETER | ALWAYS_RECALC;
    }
    if ( h0_flag == 0 ) { h0_flag = H0_IN_GLOBAL; h0_value = 0.0; }
  }
}

/***********************************************************************
*
*  function: star_sqcurve_method_all()
*
*  purpose:  value and gradient calculations using common code.
*
*/

REAL star_sqcurve_method_all(v_info,mode)
struct qinfo *v_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */
{
  int variety; /* PLAIN_SQ, EFF_SQ, NORMAL_SQ, PERP_SQ */
  int pairs;
  int i,j,k,ii,kk;
  REAL energy=0.0,ff,fn=0.0,nn=0.0,area,g,h=0.0;
  REAL dAdv[MAXCOORD], *a, *d, *s1s1, *s1s2, *s2s2;
  REAL vnorm[MAXCOORD];
  REAL **dAdv1,**dAdv2,***dvnorm1,***dvnorm2,**ds2;
  REAL ***ddAdv1dv1,***ddAdv2dv1,***ddAdv1dv2,***ddAdv2dv2;
  REAL ***ddss12,***ddss21,***ddss22;
  REAL **dfndv1,**dfndv2,**dffdv1,**dffdv2,**dnndv1,**dnndv2;
  REAL ***dfdv1,***dfdv2;
  REAL *s1,*s2;
  REAL **s = v_info->sides[0];
  REAL temp[MAXCOORD];
  REAL antisymnorm[MAXCOORD][MAXCOORD];
  REAL antisymf[MAXCOORD][MAXCOORD];
  REAL aa75=0.0,a75=0.0,ffaaa=0.0,ffaa=0.0,an75=0.0,ann75=0.0,fnnna=0.0;
  REAL fnna=0.0,fn75=0.0,fnn75=0.0,ffn75=0.0,afnfn=0.0,afffnfn=0.0,fffnfn=0.0;
  REAL ffafnfnfn=0.0,afffffnfnfnfn=0.0,fffffnfnfna=0.0,fffffnfnfn=0.0,
        fffffnfn=0.0; /* common subexpressions */
  REAL aa[MAXV*5];
  MAT2D(ds1,12*MAXV,MAXCOORD);
  MAT3D(ddss11,16*MAXV,MAXCOORD,MAXCOORD);
  REAL vertex_h0;
  REAL hh0=0.0;  /* curvature adjusted for h-h0 */
  int concount; /* number of constraints vertex is on */
  int final_edge; /* of pairs; wrap to first for complete star */
  MAT2D(proj,MAXCOORD,MAXCOORD);
  struct method_instance *mi = METH_INSTANCE(v_info->method);

  if ( mi->gen_method 
                 == star_normal_sq_mean_curvature_mi )
     variety = NORMAL_SQ;
  else if ( mi->gen_method 
                 == star_perp_sq_mean_curvature_mi )
     variety = PERP_SQ;
  else if ( mi->gen_method 
                 == star_eff_area_sq_mean_curvature_mi )
     variety = EFF_SQ;
  else variety = PLAIN_SQ;

  pairs = (v_info->vcount - 1);
  if ( pairs <= 0 ) return 0.0;
  if ( v_info->flags & INCOMPLETE_STAR )
  { pairs--;
    final_edge = pairs;
  }
  else 
    final_edge = 0;


  if ( v_info->vcount > MAXV )
  { a = (REAL*)mycalloc(5*pairs,sizeof(REAL));
    ds1 = dmatrix(0,12*v_info->vcount,0,SDIM-1);
  } else
  { memset((char*)aa,0,sizeof(REAL)*5*pairs);
    a = aa;
    memset((char*)ds1[0],0,sizeof(REAL)*12*v_info->vcount*MAXCOORD);
  }
  d = a+pairs; s1s1 = d + pairs; s1s2 = s1s1 + pairs; s2s2 = s1s2 + pairs;
  ds2 = ds1 + pairs; dAdv1 = ds2 + pairs; dAdv2 = dAdv1 + pairs;
  dfndv1 = dAdv2 + pairs; dfndv2 = dfndv1 + pairs; dffdv1 = dfndv2 + pairs;
  dffdv2 = dffdv1 + pairs; dnndv1 = dffdv2 + pairs; dnndv2 = dnndv1 + pairs;

  /* basic dot products */
  area = 0.0;
  for ( j = 0 ; j < SDIM ; j++ ) dAdv[j] = vnorm[j] = 0.0;
  for ( k = 0 ; k < pairs ; k++ )
  { s1 = s[k]; s2 = s[(k+1==pairs)?final_edge:k+1];
    s1s1[k] = SDIM_dot(s1,s1);
    s1s2[k] = SDIM_dot(s1,s2);
    s2s2[k] = SDIM_dot(s2,s2);
    d[k] = s1s1[k]*s2s2[k] - s1s2[k]*s1s2[k];
    a[k] = 0.5*sqrt(d[k]);
    area += a[k];
    for ( j = 0 ; j < SDIM ; j++ )
    { ds1[k][j] = 2*(s2s2[k]*s1[j] - s1s2[k]*s2[j]);
      ds2[k][j] = 2*(s1s1[k]*s2[j] - s1s2[k]*s1[j]);
      dAdv1[k][j] = 0.125/a[k]*ds1[k][j];
      dAdv2[k][j] = 0.125/a[k]*ds2[k][j];
      dAdv[j] -= dAdv1[k][j] + dAdv2[k][j];
    }
    if ( variety != PLAIN_SQ )
    { cross_prod(s1,s2,temp);
      for ( j = 0 ; j < SDIM ; j++ )
        vnorm[j] += 0.5*temp[j];
    }
  }

  /* constraint projection */
  concount = constr_proj_matrix_wall(v_info->id,proj);

  if ( concount && (v_info->flags & INCOMPLETE_STAR) )
  { /* project force and normal to constraints. This is enough to take
       care of energy and gradient, since force and energy occur only in
       dot products.
       */
    int i;
    for ( i = 0 ; i < SDIM ; i++ )
       temp[i] = dAdv[i];
    matvec_mul(proj,temp,dAdv,SDIM,SDIM);
    for ( i = 0 ; i < SDIM ; i++ )
        temp[i] = vnorm[i];
    matvec_mul(proj,temp,vnorm,SDIM,SDIM);
  }

  /* energy */
  ff = SDIM_dot(dAdv,dAdv);
  switch ( variety )
  { case TEST_SQ:
        nn = SDIM_dot(vnorm,vnorm);
        energy = nn; break; 
     case PLAIN_SQ: 
        energy = 0.75/area*ff;
        break;
     case EFF_SQ:
        nn = SDIM_dot(vnorm,vnorm);
        if  ( nn <= 0.0 ) return 0.0;
        energy = 0.75*area*ff/nn;
        break;
     case NORMAL_SQ:
        fn = SDIM_dot(dAdv,vnorm);
        if ( fn == 0.0 ) 
        { ff = 0.0; fn = 1e-6; }
        switch ( h0_flag )
          { case H0_IN_GLOBAL: hh0 = ff/fn - 2*h0_value/3; break;
            case H0_IN_ATTR:   hh0 = ff/fn-2*(*VREAL(v_info->id,h0_attr))/3;
                  break;
            default: hh0 = ff/fn; break;
          }
        energy = 0.75*area*hh0*hh0;
        break;
     case PERP_SQ:
        fn = SDIM_dot(dAdv,vnorm);
        nn = SDIM_dot(vnorm,vnorm);
        switch ( h0_flag )
          { case H0_IN_GLOBAL: vertex_h0 = h0_value; break;
            case H0_IN_ATTR:   vertex_h0 = (*VREAL(v_info->id,h0_attr));
                  break;
            default: vertex_h0 = 0.0; break;
          }
        if ( nn == 0.0 || (concount == 2 && nn < 0.01*area*area) )
        { /* C2 line, probably, so h = 0 */
          fn = 0.0; nn = 1.0;
        }
        hh0 = (fn/nn - 2*vertex_h0/3);
        energy = 0.75*area*hh0*hh0;
        break;
    }
    if ( get_vattr(v_info->id) & AXIAL_POINT )
    { if ( sym_flags & DOUBLE_AXIAL )
          energy /= rotorder/2;
      else energy /= rotorder;
      energy *= rotorder/v_info->axial_order;
    }
    if ( mode == METHOD_VALUE ) goto all_exit;

    /* Gradient */

    if ( v_info->vcount > MAXV )
      ddss11 = dmatrix3(16*pairs,SDIM,SDIM);
    else memset((char*)ddss11[0][0],0,sizeof(REAL)*16*pairs*MAXCOORD*MAXCOORD);
    ddss12 = ddss11 + pairs; ddss21 = ddss12 + pairs; ddss22 = ddss21 + pairs;
    ddAdv1dv1 = ddss22 + pairs; ddAdv1dv2 = ddAdv1dv1 + pairs;
    ddAdv2dv1 = ddAdv1dv2 + pairs; ddAdv2dv2 = ddAdv2dv1 + pairs;
    dvnorm1 = ddAdv2dv2 + pairs; dvnorm2 = dvnorm1 + pairs;
    dfdv1 = dvnorm2 + pairs; dfdv2 = dfdv1 + pairs;

    /* first, some more common terms */
    for ( k = 0 ; k < pairs ; k++ )
    { s1 = s[k]; s2 = s[(k+1==pairs)?final_edge:k+1];
      for ( i = 0 ; i < SDIM ; i++ )
      { ddss11[k][i][i] = 2*s2s2[k];
        ddss12[k][i][i] = -2*s1s2[k];
        ddss21[k][i][i] = -2*s1s2[k];
        ddss22[k][i][i] = 2*s1s1[k];
        for ( j = 0 ; j < SDIM ; j++ )
        { ddss11[k][i][j] -= 2*s2[i]*s2[j];
          ddss12[k][i][j] += 4*s1[i]*s2[j] - 2*s2[i]*s1[j];
          ddss21[k][i][j] += 4*s2[i]*s1[j] - 2*s1[i]*s2[j];
          ddss22[k][i][j] -= 2*s1[i]*s1[j];

          ddAdv1dv1[k][i][j] = -0.125/a[k]/a[k]*dAdv1[k][i]*ds1[k][j]
                      + 0.125/a[k]*ddss11[k][i][j];
          ddAdv1dv2[k][i][j] = -0.125/a[k]/a[k]*dAdv1[k][i]*ds2[k][j]
                      + 0.125/a[k]*ddss12[k][i][j];
          dfdv1[k][i][j] = -ddAdv1dv1[k][i][j] - ddAdv1dv2[k][i][j];
          ddAdv2dv1[k][i][j] = -0.125/a[k]/a[k]*dAdv2[k][i]*ds1[k][j]
                      + 0.125/a[k]*ddss21[k][i][j];
          ddAdv2dv2[k][i][j] = -0.125/a[k]/a[k]*dAdv2[k][i]*ds2[k][j]
                      + 0.125/a[k]*ddss22[k][i][j];
          dfdv2[k][i][j] = -ddAdv2dv1[k][i][j] - ddAdv2dv2[k][i][j];
        }
        if ( concount )
        { /* project force and normal to constraints.  */
          for ( j = 0 ; j < SDIM ; j++ )
            temp[j] = dfdv1[k][i][j];
          matvec_mul(proj,temp,dfdv1[k][i],SDIM,SDIM);
          for ( j = 0 ; j < SDIM ; j++ )
            temp[j] = dfdv2[k][i][j];
          matvec_mul(proj,temp,dfdv2[k][i],SDIM,SDIM);
        }
        dffdv1[k][i] = 2*SDIM_dot(dfdv1[k][i],dAdv);
        dffdv2[k][i] = 2*SDIM_dot(dfdv2[k][i],dAdv);
      }

      if ( variety == PLAIN_SQ ) continue;
      dvnorm1[k][0][1] = -0.5*s2[2];
      dvnorm1[k][0][2] =  0.5*s2[1];
      dvnorm1[k][1][0] =  0.5*s2[2];
      dvnorm1[k][1][2] = -0.5*s2[0];
      dvnorm1[k][2][0] = -0.5*s2[1];
      dvnorm1[k][2][1] =  0.5*s2[0];
      dvnorm2[k][0][1] =  0.5*s1[2];
      dvnorm2[k][0][2] = -0.5*s1[1];
      dvnorm2[k][1][0] = -0.5*s1[2];
      dvnorm2[k][1][2] =  0.5*s1[0];
      dvnorm2[k][2][0] =  0.5*s1[1];
      dvnorm2[k][2][1] = -0.5*s1[0];
      if ( concount )
        for ( i = 0 ; i < SDIM ; i++ )
        { /* project force and normal to constraints.  */
          for ( j = 0 ; j < SDIM ; j++ )
            temp[j] = dvnorm1[k][i][j];
          matvec_mul(proj,temp,dvnorm1[k][i],SDIM,SDIM);
          for ( j = 0 ; j < SDIM ; j++ )
            temp[j] = dvnorm2[k][i][j];
          matvec_mul(proj,temp,dvnorm2[k][i],SDIM,SDIM);
        }
      for ( i = 0 ; i < SDIM ; i++ )
      { dfndv1[k][i] = SDIM_dot(dfdv1[k][i],vnorm);
        dfndv1[k][i] += SDIM_dot(dAdv,dvnorm1[k][i]);
        dfndv2[k][i] = SDIM_dot(dfdv2[k][i],vnorm);
        dfndv2[k][i] += SDIM_dot(dAdv,dvnorm2[k][i]);
        dnndv1[k][i] = 2*SDIM_dot(vnorm,dvnorm1[k][i]);
        dnndv2[k][i] = 2*SDIM_dot(vnorm,dvnorm2[k][i]);
      }
    }

  /* now, the actual gradients */
  for ( k = 0 ; k < v_info->vcount ; k++ )
     for ( i = 0 ; i < SDIM ; i++ )
        v_info->grad[k][i] = 0.0;

  switch ( variety )
  { case TEST_SQ:
        for ( k = 0 ; k < pairs ; k++ )
        { REAL *grad2 = (k+1==pairs)?v_info->grad[final_edge+1]:v_info->grad[k+2];
          for ( i = 0 ; i < SDIM ; i++ )
          { g = dnndv1[k][i];
            v_info->grad[k+1][i] += g;
            v_info->grad[0][i] -= g;
            g = dnndv2[k][i];
            grad2[i] += g;
            v_info->grad[0][i] -= g;
          }
         }
        break;

     case PLAIN_SQ: 
        for ( k = 0 ; k < pairs ; k++ )
        { REAL *grad2 = (k+1==pairs)?v_info->grad[final_edge+1]:v_info->grad[k+2];
          for ( i = 0 ; i < SDIM ; i++ )
          { g = -0.75*ff/area/area*dAdv1[k][i] + 0.75/area*dffdv1[k][i];
            v_info->grad[k+1][i] += g;
            v_info->grad[0][i] -= g;
            g = -0.75*ff/area/area*dAdv2[k][i] + 0.75/area*dffdv2[k][i];
            grad2[i] += g;
            v_info->grad[0][i] -= g;
          }
         }
        break;

     case EFF_SQ:
        for ( k = 0 ; k < pairs ; k++ )
        { REAL *grad2 = (k+1==pairs)?v_info->grad[final_edge+1]:v_info->grad[k+2];
          for ( i = 0 ; i < SDIM ; i++ )
          { g = 0.75*ff/nn*dAdv1[k][i] 
                    + 0.75*area/nn*dffdv1[k][i]
                      - 0.75*area/nn/nn*ff*dnndv1[k][i];
             v_info->grad[k+1][i] += g;
             v_info->grad[0][i] -= g;
             g = 0.75*ff/nn*dAdv2[k][i] 
                    + 0.75*area/nn*dffdv2[k][i]
                      - 0.75*area/nn/nn*ff*dnndv2[k][i];
             grad2[i] += g;
             v_info->grad[0][i] -= g;
          }
        } 
        break;

     case NORMAL_SQ:
      if ( fn != 0.0 )
        for ( k = 0 ; k < pairs ; k++ )
        { REAL *grad2 = (k+1==pairs)?v_info->grad[final_edge+1]:v_info->grad[k+2];
          REAL g1,g2,g3;
          for ( i = 0 ; i < SDIM ; i++ )
          { g1 = 0.75*hh0*hh0*dAdv1[k][i]; 
                g2 = 1.5*area/fn*hh0*dffdv1[k][i];
                 g3 =   - 1.5*area*ff*hh0/fn/fn*dfndv1[k][i]; 
             g = g1 + g2 + g3;
             v_info->grad[k+1][i] += g;
             v_info->grad[0][i] -= g;
             g1 = 0.75*hh0*hh0*dAdv2[k][i] ;
                  g2 =  1.5*area/fn*hh0*dffdv2[k][i];
                 g3 =    - 1.5*area*ff*hh0/fn/fn*dfndv2[k][i]; 
             g = g1 + g2 + g3;
             grad2[i] += g;
             v_info->grad[0][i] -= g;
            }
         }
        break;

     case PERP_SQ:
        for ( k = 0 ; k < pairs ; k++ )
        { REAL *grad2 = (k+1==pairs)?v_info->grad[final_edge+1]:v_info->grad[k+2];
          for ( i = 0 ; i < SDIM ; i++ )
          { g = 0.75*hh0*hh0*dAdv1[k][i] 
                    + 1.5*area/nn*hh0*dfndv1[k][i]
                    - 1.5*area*fn*hh0/nn/nn*dnndv1[k][i]; 
             v_info->grad[k+1][i] += g;
             v_info->grad[0][i] -= g;
             g = 0.75*hh0*hh0*dAdv2[k][i] 
                    + 1.5*area/nn*hh0*dfndv2[k][i]
                    - 1.5*area*fn*hh0/nn/nn*dnndv2[k][i]; 
             grad2[i] += g;
             v_info->grad[0][i] -= g;
            }
         }
        break;
    }

    if ( get_vattr(v_info->id) & AXIAL_POINT )
    { REAL fudge = 1./rotorder;
      if ( sym_flags & DOUBLE_AXIAL ) fudge *= 2;
      fudge *= rotorder/v_info->axial_order;
      for ( i = 0 ; i < v_info->vcount ; i++ )
         for ( j = 0 ; j < SDIM ; j++ )
          v_info->grad[i][j] *= fudge;
    }
    if ( mode == METHOD_GRADIENT ) goto all_exit;

    /* Hessian */
    if ( (variety == EFF_SQ) || (variety==PERP_SQ) || (variety == TEST_SQ) )
    { for ( i = 0 ; i < SDIM ; i++ )
      { antisymnorm[i][(i+1)%SDIM] = vnorm[(i+2)%SDIM]/2;
        antisymnorm[(i+1)%SDIM][i] = -vnorm[(i+2)%SDIM]/2;
        antisymnorm[i][i] = 0.0;
      }
    }
    if ( (variety == NORMAL_SQ) || (variety==PERP_SQ) || (variety == TEST_SQ) )
    { for ( i = 0 ; i < SDIM ; i++ )
      { antisymf[i][(i+1)%SDIM] = dAdv[(i+2)%SDIM]/2;
        antisymf[(i+1)%SDIM][i] = -dAdv[(i+2)%SDIM]/2;
        antisymf[i][i] = 0.0;
      }
    }

    
    if ( h0_flag && (variety != NORMAL_SQ) && (variety != PERP_SQ) ) 
      kb_error(1615,
        "Can only use h_zero with star_normal_sq_curve.\n",RECOVERABLE);


    switch ( variety )
    { case PLAIN_SQ:
          aa75 = 0.75/area/area;
          a75 = 0.75/area;
          ffaaa = 1.5*ff/area/area/area;
          ffaa = 0.75*ff/area/area;
          break;
      case EFF_SQ:
          an75 = 0.75*area/nn;
          ann75 = 0.75*area/nn/nn;
          fnnna = 1.5*ff/nn/nn/nn*area;
          fnna = 0.75*ff/nn/nn*area;
          fn75 = 0.75/nn;
          fnn75 = 0.75*ff/nn/nn;
          ffn75 = 0.75*ff/nn;
          break;
      case NORMAL_SQ:
          afnfn = 1.5*area/fn/fn;
          fffnfn = 1.5*hh0/fn;
          afffnfn = 1.5*area*hh0/fn;
          ffafnfnfn = 1.5*area*(ff/fn + hh0)/fn/fn;
          afffffnfnfnfn = 1.5*area*ff*(ff/fn+2*hh0)/fn/fn/fn;
          fffffnfnfna = 1.5*ff*hh0/fn/fn*area;
          fffffnfnfn  = 1.5*ff*hh0/fn/fn;
          fffffnfn = 0.75*hh0*hh0;

          break;
    }


    for ( k = 0 ; k < pairs ; k++ ) 
    { REAL co1 = 0.25/a[k]/a[k]/a[k];
      REAL co2 = 0.125/a[k]/a[k];
      REAL co3 = 0.125/a[k];
      REAL dssf = (SDIM_dot(ds1[k],dAdv)+SDIM_dot(ds2[k],dAdv));
      REAL s1f,s2f;
      REAL ddssf1[MAXCOORD],ddssf2[MAXCOORD];


      for ( i = 0 ; i < SDIM ; i++ )
      { ddssf1[i] = 
            (SDIM_dot(ddss11[k][i],dAdv)+SDIM_dot(ddss12[k][i],dAdv));
        ddssf2[i] =
            (SDIM_dot(ddss21[k][i],dAdv)+SDIM_dot(ddss22[k][i],dAdv));
      }
      s1 = s[k]; s2 = s[(k+1==pairs)?final_edge:k+1];
      s1f = SDIM_dot(s1,dAdv);
      s2f = SDIM_dot(s2,dAdv);

      for ( kk = 0 ; kk < pairs ; kk++ )
      { int nextk = (k+1==pairs) ? final_edge : k+1 ; 
        int nextkk = (kk+1==pairs) ? final_edge : kk+1 ; 
        REAL **h11 = v_info->hess[k+1][kk+1];
        REAL **h12 = v_info->hess[k+1][nextkk+1];
        REAL **h21 = v_info->hess[nextk+1][kk+1];
        REAL **h22 = v_info->hess[nextk+1][nextkk+1];
        REAL **h00 = v_info->hess[0][0];
        REAL **h10 = v_info->hess[k+1][0];
        REAL **h02 = v_info->hess[0][nextkk+1];
        REAL **h20 = v_info->hess[nextk+1][0];
        REAL **h01 = v_info->hess[0][kk+1];
        REAL ddff,dddsf,ddnn,ddfn;

        for ( i = 0 ; i < SDIM ; i++ )
          for ( ii = 0 ; ii < SDIM ; ii++ )
          { /* 11 term */
            ddff = SDIM_dot(dfdv1[k][i],dfdv1[kk][ii]);
            if ( k == kk )
            {
              ddff += (-co1*dAdv1[k][i]*dAdv1[k][ii] + co2*ddAdv1dv1[k][i][ii])
                              *dssf;
              ddff += co2*dAdv1[k][i]*ddssf1[ii];
              ddff += co2*dAdv1[kk][ii]*ddssf1[i];
              dddsf = ((i==ii) ? 4*s2f : 0. )
                            - 2*s2[i]*dAdv[ii] - 2*s2[ii]*dAdv[i];
              ddff -= co3*dddsf;
            }
            ddff *= 2;
            switch ( variety )
              { case TEST_SQ:
                      h = ddnn = 2*SDIM_dot(dvnorm1[k][i],dvnorm1[kk][ii]);
                     break;
                 case PLAIN_SQ:
                     h = -aa75*(dffdv1[k][i]*dAdv1[kk][ii]
                                 + dffdv1[kk][ii]*dAdv1[k][i])
                         + ffaaa*dAdv1[k][i]*dAdv1[kk][ii]
                         + a75*ddff;
                     if ( k == kk )
                        h -= ffaa*ddAdv1dv1[k][i][ii];
                     break;
                 case EFF_SQ:
                      ddnn = 2*SDIM_dot(dvnorm1[k][i],dvnorm1[kk][ii]);
                      h = an75*ddff
                         - ann75*dffdv1[k][i]*dnndv1[kk][ii]
                         - ann75*dffdv1[kk][ii]*dnndv1[k][i]
                         + fnnna*dnndv1[k][i]*dnndv1[kk][ii]
                         - fnna*ddnn
                         + fn75*dffdv1[k][i]*dAdv1[kk][ii]
                         + fn75*dffdv1[kk][ii]*dAdv1[k][i]
                         - fnn75*dAdv1[kk][ii]*dnndv1[k][i]
                         - fnn75*dnndv1[kk][ii]*dAdv1[k][i];
                      if ( k == kk )
                         h += ffn75*ddAdv1dv1[k][i][ii];
                      break;
                 case NORMAL_SQ:
                      ddfn = SDIM_dot(dfdv1[k][i],dvnorm1[kk][ii])
                              + SDIM_dot(dvnorm1[k][i],dfdv1[kk][ii]);
                      if ( k == kk )
                      {
                         ddfn += (-co1*dAdv1[k][i]*dAdv1[k][ii]
                                     + co2*ddAdv1dv1[k][i][ii])
                            *(SDIM_dot(ds1[k],vnorm)+SDIM_dot(ds2[k],vnorm));
                         ddfn += co2*dAdv1[k][i]*
                          (SDIM_dot(ddss11[kk][ii],vnorm)
                              +SDIM_dot(ddss12[kk][ii],vnorm));
                         ddfn += co2*dAdv1[kk][ii]*
                            (SDIM_dot(ddss11[k][i],vnorm)
                              +SDIM_dot(ddss12[k][i],vnorm));
                         dddsf = ((i==ii) ? 4*SDIM_dot(s2,vnorm) : 0. )
                                    - 2*s2[i]*vnorm[ii] - 2*s2[ii]*vnorm[i];
                         ddfn -= co3*dddsf;
                      }
                      h  = afnfn*dffdv1[k][i]*dffdv1[kk][ii]
                          + afffnfn*ddff
                          + fffnfn*dffdv1[k][i]*dAdv1[kk][ii]
                          + fffnfn*dAdv1[k][i]*dffdv1[kk][ii]
                          - ffafnfnfn*dffdv1[k][i]*dfndv1[kk][ii]
                          - ffafnfnfn*dfndv1[k][i]*dffdv1[kk][ii]
                          + afffffnfnfnfn*dfndv1[k][i]*dfndv1[kk][ii]
                          - fffffnfnfna*ddfn
                          - fffffnfnfn*dfndv1[k][i]*dAdv1[kk][ii]
                          - fffffnfnfn*dAdv1[k][i]*dfndv1[kk][ii];
                      if ( k == kk ) 
                         h += fffffnfn*ddAdv1dv1[k][i][ii];
                      break;
                  case PERP_SQ:
                      ddnn = 2*SDIM_dot(dvnorm1[k][i],dvnorm1[kk][ii]);
                      ddfn = SDIM_dot(dfdv1[k][i],dvnorm1[kk][ii])
                              + SDIM_dot(dvnorm1[k][i],dfdv1[kk][ii]);
                      if ( k == kk )
                      {
                         ddfn += (-co1*dAdv1[k][i]*dAdv1[k][ii]
                                     + co2*ddAdv1dv1[k][i][ii])
                            *(SDIM_dot(ds1[k],vnorm)+SDIM_dot(ds2[k],vnorm));
                         ddfn += co2*dAdv1[k][i]*
                          (SDIM_dot(ddss11[kk][ii],vnorm)
                              +SDIM_dot(ddss12[kk][ii],vnorm));
                         ddfn += co2*dAdv1[kk][ii]*
                            (SDIM_dot(ddss11[k][i],vnorm)
                              +SDIM_dot(ddss12[k][i],vnorm));
                         dddsf = ((i==ii) ? 4*SDIM_dot(s2,vnorm) : 0. )
                                    - 2*s2[i]*vnorm[ii] - 2*s2[ii]*vnorm[i];
                         ddfn -= co3*dddsf;
                      }
           h = 1.5*hh0*dAdv1[k][i]*(dfndv1[kk][ii]/nn-fn/nn/nn*dnndv1[kk][ii])
             + 1.5*hh0*dAdv1[kk][ii]*(dfndv1[k][i]/nn-fn/nn/nn*dnndv1[k][i])
             + 1.5*area*(dfndv1[k][i]/nn-fn/nn/nn*dnndv1[k][i])*
                           (dfndv1[kk][ii]/nn-fn/nn/nn*dnndv1[kk][ii])
             + 1.5*area*hh0*(ddfn/nn - dfndv1[k][i]*dnndv1[kk][ii]/nn/nn
                    - dfndv1[kk][ii]*dnndv1[k][i]/nn/nn
                    +2*fn*dnndv1[k][i]*dnndv1[kk][ii]/nn/nn/nn
                     - fn*ddnn/nn/nn);
           if ( k == kk )
              h += 0.75*ddAdv1dv1[k][i][ii]*hh0*hh0;
                      break;
                }
                if ( !is_finite(h) )
                { sprintf(errmsg,
                    "Star sq curvature hessian infinite at vertex %s\n",
                       ELNAME(v_info->id));
                  kb_error(3617,errmsg,RECOVERABLE);
                }
                h11[i][ii] += h;
                h01[i][ii] -= h;
                h10[i][ii] -= h;
                h00[i][ii] += h;

                /* 12 term */
                ddff = SDIM_dot(dfdv1[k][i],dfdv2[kk][ii]);
                if ( k == kk )
                { ddff += (-co1*dAdv1[k][i]*dAdv2[k][ii] + co2*ddAdv1dv2[k][i][ii])
                              *dssf;
                  ddff += co2*dAdv1[k][i]*ddssf2[ii];
                  ddff += co2*dAdv2[kk][ii]*ddssf1[i];
                  dddsf = 4*dAdv[i]*s2[ii] -2*dAdv[ii]*s2[i] 
                              - 2*((i==ii) ? s2f : 0.0)
                          + 4*dAdv[ii]*s1[i] - 2*dAdv[i]*s1[ii]
                              - 2*((i==ii) ? s1f : 0.0);
                  ddff -= co3*dddsf;
                }
              ddff *= 2;
              switch ( variety )
              { case TEST_SQ:
                      ddnn = 2*SDIM_dot(dvnorm1[k][i],dvnorm2[kk][ii]);
                      if ( k == kk ) ddnn += 2*antisymnorm[i][ii];
                      h = ddnn;
                     break;
                 case PLAIN_SQ:
                     h = -aa75*dffdv1[k][i]*dAdv2[kk][ii]
                         - aa75*dffdv2[kk][ii]*dAdv1[k][i]
                         + ffaaa*dAdv1[k][i]*dAdv2[kk][ii]
                         + a75*ddff;
                     if ( k == kk )
                        h -= ffaa*ddAdv1dv2[k][i][ii];
                      break;
                 case EFF_SQ:
                      ddnn = 2*SDIM_dot(dvnorm1[k][i],dvnorm2[kk][ii]);
                      if ( k == kk ) ddnn += 2*antisymnorm[i][ii];
                      h = an75*ddff
                         - ann75*dffdv1[k][i]*dnndv2[kk][ii]
                         - ann75*dffdv2[kk][ii]*dnndv1[k][i]
                         + fnnna*dnndv1[k][i]*dnndv2[kk][ii]
                         - fnna*ddnn
                         + fn75*dffdv1[k][i]*dAdv2[kk][ii]
                         + fn75*dffdv2[kk][ii]*dAdv1[k][i]
                         - fnn75*dAdv2[kk][ii]*dnndv1[k][i]
                         - fnn75*dnndv2[kk][ii]*dAdv1[k][i];
                      if ( k == kk )
                         h += ffn75*ddAdv1dv2[k][i][ii];
                      break;
                 case NORMAL_SQ:
                      ddfn = SDIM_dot(dfdv1[k][i],dvnorm2[kk][ii])
                              + SDIM_dot(dvnorm1[k][i],dfdv2[kk][ii]);
                      if ( k == kk ) ddfn += antisymf[i][ii];
                      if ( k == kk )
                      {
                         ddfn += (-co1*dAdv1[k][i]*dAdv2[k][ii]
                                     + co2*ddAdv1dv2[k][i][ii])
                                     *(SDIM_dot(ds1[k],vnorm)+SDIM_dot(ds2[k],vnorm));
                         ddfn += co2*dAdv1[k][i]*
                          (SDIM_dot(ddss21[kk][ii],vnorm)
                              +SDIM_dot(ddss22[kk][ii],vnorm));
                         ddfn += co2*dAdv2[kk][ii]*
                            (SDIM_dot(ddss11[k][i],vnorm)
                              +SDIM_dot(ddss12[k][i],vnorm));
                         dddsf = 4*vnorm[i]*s2[ii] -2*vnorm[ii]*s2[i] 
                                     - 2*((i==ii) ? SDIM_dot(s2,vnorm) : 0.0)
                                 + 4*vnorm[ii]*s1[i] - 2*vnorm[i]*s1[ii]
                                     - 2*((i==ii) ? SDIM_dot(s1,vnorm) : 0.0);
                         ddfn -= co3*dddsf;
                      }
                      h  = afnfn*dffdv1[k][i]*dffdv2[kk][ii]
                          + afffnfn*ddff
                          - ffafnfnfn*dffdv1[k][i]*dfndv2[kk][ii]
                          + fffnfn*dffdv1[k][i]*dAdv2[kk][ii]
                          + fffnfn*dAdv1[k][i]*dffdv2[kk][ii]
                          - ffafnfnfn*dfndv1[k][i]*dffdv2[kk][ii]
                          + afffffnfnfnfn*dfndv1[k][i]*dfndv2[kk][ii]
                          - fffffnfnfna*ddfn
                          - fffffnfnfn*dfndv1[k][i]*dAdv2[kk][ii]
                          - fffffnfnfn*dAdv1[k][i]*dfndv2[kk][ii];
                      if ( k == kk ) 
                         h += fffffnfn*ddAdv1dv2[k][i][ii];
                      break;
               case PERP_SQ:
                      ddnn = 2*SDIM_dot(dvnorm1[k][i],dvnorm2[kk][ii]);
                      if ( k == kk ) ddnn += 2*antisymnorm[i][ii];
                      ddfn = SDIM_dot(dfdv1[k][i],dvnorm2[kk][ii])
                              + SDIM_dot(dvnorm1[k][i],dfdv2[kk][ii]);
                      if ( k == kk ) ddfn += antisymf[i][ii];
                      if ( k == kk )
                      {
                         ddfn += (-co1*dAdv1[k][i]*dAdv2[k][ii]
                                     + co2*ddAdv1dv2[k][i][ii])
                                     *(SDIM_dot(ds1[k],vnorm)+SDIM_dot(ds2[k],vnorm));
                         ddfn += co2*dAdv1[k][i]*
                          (SDIM_dot(ddss21[kk][ii],vnorm)
                              +SDIM_dot(ddss22[kk][ii],vnorm));
                         ddfn += co2*dAdv2[kk][ii]*
                            (SDIM_dot(ddss11[k][i],vnorm)
                              +SDIM_dot(ddss12[k][i],vnorm));
                         dddsf = 4*vnorm[i]*s2[ii] -2*vnorm[ii]*s2[i] 
                                     - 2*((i==ii) ? SDIM_dot(s2,vnorm) : 0.0)
                                 + 4*vnorm[ii]*s1[i] - 2*vnorm[i]*s1[ii]
                                     - 2*((i==ii) ? SDIM_dot(s1,vnorm) : 0.0);
                         ddfn -= co3*dddsf;
                      }
           h = 1.5*hh0*dAdv1[k][i]*(dfndv2[kk][ii]/nn-fn/nn/nn*dnndv2[kk][ii])
             + 1.5*hh0*dAdv2[kk][ii]*(dfndv1[k][i]/nn-fn/nn/nn*dnndv1[k][i])
             + 1.5*area*(dfndv1[k][i]/nn-fn/nn/nn*dnndv1[k][i])*
                           (dfndv2[kk][ii]/nn-fn/nn/nn*dnndv2[kk][ii])
             + 1.5*area*hh0*(ddfn/nn - dfndv1[k][i]*dnndv2[kk][ii]/nn/nn
                    - dfndv2[kk][ii]*dnndv1[k][i]/nn/nn
                    +2*fn*dnndv1[k][i]*dnndv2[kk][ii]/nn/nn/nn
                     - fn*ddnn/nn/nn);
           if ( k == kk )
              h += 0.75*ddAdv1dv2[k][i][ii]*hh0*hh0;
                      break;
                }
                if ( !is_finite(h) )
                { sprintf(errmsg,
                    "Star sq curvature hessian infinite at vertex %s\n",
                       ELNAME(v_info->id));
                  kb_error(3618,errmsg,RECOVERABLE);
                }
                h12[i][ii] += h;
                h02[i][ii] -= h;
                h10[i][ii] -= h;
                h00[i][ii] += h;

                /* 21 term */
                ddff = SDIM_dot(dfdv2[k][i],dfdv1[kk][ii]);
                if ( k == kk )
                { ddff += (-co1*dAdv2[k][i]*dAdv1[k][ii] + co2*ddAdv2dv1[k][i][ii])
                              *dssf;
                  ddff += co2*dAdv2[k][i]*ddssf1[ii];
                  ddff += co2*dAdv1[kk][ii]*ddssf2[i];
                  dddsf = 4*dAdv[i]*s1[ii] -2*dAdv[ii]*s1[i] 
                              - 2*((i==ii) ? s1f: 0.0)
                          + 4*dAdv[ii]*s2[i] - 2*dAdv[i]*s2[ii]
                              - 2*((i==ii) ? s2f : 0.0);
                  ddff -= co3*dddsf;
                }
              ddff *= 2;
              switch ( variety )
              { case TEST_SQ:
                      ddnn = 2*SDIM_dot(dvnorm2[k][i],dvnorm1[kk][ii]);
                      if ( k == kk ) ddnn += 2*antisymnorm[ii][i];
                      h = ddnn;
                     break;
                 case PLAIN_SQ:
                     h = -aa75*dffdv2[k][i]*dAdv1[kk][ii]
                         - aa75*dffdv1[kk][ii]*dAdv2[k][i]
                         + ffaaa*dAdv2[k][i]*dAdv1[kk][ii]
                         + a75*ddff;
                     if ( k == kk )
                        h -= ffaa*ddAdv2dv1[k][i][ii];
                      break;
                 case EFF_SQ:
                      ddnn = 2*SDIM_dot(dvnorm2[k][i],dvnorm1[kk][ii]);
                      if ( k == kk ) ddnn += 2*antisymnorm[ii][i];
                      h = an75*ddff
                         - ann75*dffdv2[k][i]*dnndv1[kk][ii]
                         - ann75*dffdv1[kk][ii]*dnndv2[k][i]
                         + fnnna*dnndv2[k][i]*dnndv1[kk][ii]
                         - fnna*ddnn
                         + fn75*dffdv2[k][i]*dAdv1[kk][ii]
                         + fn75*dffdv1[kk][ii]*dAdv2[k][i]
                         - fnn75*dAdv1[kk][ii]*dnndv2[k][i]
                         - fnn75*dnndv1[kk][ii]*dAdv2[k][i];
                      if ( k == kk )
                         h += ffn75*ddAdv2dv1[k][i][ii];
                      break;
                 case NORMAL_SQ:
                      ddfn = SDIM_dot(dfdv2[k][i],dvnorm1[kk][ii])
                              + SDIM_dot(dvnorm2[k][i],dfdv1[kk][ii]);
                      if ( k == kk ) ddfn += antisymf[ii][i];
                      if ( k == kk )
                      {
                         ddfn += (-co1*dAdv2[k][i]*dAdv1[k][ii]
                                     + co2*ddAdv2dv1[k][i][ii])
                                     *(SDIM_dot(ds1[k],vnorm)+SDIM_dot(ds2[k],vnorm));
                         ddfn += co2*dAdv2[k][i]*
                          (SDIM_dot(ddss11[kk][ii],vnorm)
                              +SDIM_dot(ddss12[kk][ii],vnorm));
                         ddfn += co2*dAdv1[kk][ii]*
                            (SDIM_dot(ddss21[k][i],vnorm)
                              +SDIM_dot(ddss22[k][i],vnorm));
                         dddsf = 4*vnorm[i]*s1[ii] -2*vnorm[ii]*s1[i] 
                                     - 2*((i==ii) ? SDIM_dot(s1,vnorm) : 0.0)
                                 + 4*vnorm[ii]*s2[i] - 2*vnorm[i]*s2[ii]
                                     - 2*((i==ii) ? SDIM_dot(s2,vnorm) : 0.0);
                         ddfn -= co3*dddsf;
                      }
                      h  = afnfn*dffdv2[k][i]*dffdv1[kk][ii]
                          + afffnfn*ddff
                          - ffafnfnfn*dffdv2[k][i]*dfndv1[kk][ii]
                          + fffnfn*dffdv2[k][i]*dAdv1[kk][ii]
                          - ffafnfnfn*dfndv2[k][i]*dffdv1[kk][ii]
                          + afffffnfnfnfn*dfndv2[k][i]*dfndv1[kk][ii]
                          - fffffnfnfna*ddfn
                          - fffffnfnfn*dfndv2[k][i]*dAdv1[kk][ii]
                          + fffnfn*dAdv2[k][i]*dffdv1[kk][ii]
                          - fffffnfnfn*dAdv2[k][i]*dfndv1[kk][ii];
                      if ( k == kk ) 
                         h += fffffnfn*ddAdv2dv1[k][i][ii];
                      break;
                 case PERP_SQ:
                      ddnn = 2*SDIM_dot(dvnorm2[k][i],dvnorm1[kk][ii]);
                      if ( k == kk ) ddnn += 2*antisymnorm[ii][i];
                      ddfn = SDIM_dot(dfdv2[k][i],dvnorm1[kk][ii])
                              + SDIM_dot(dvnorm2[k][i],dfdv1[kk][ii]);
                      if ( k == kk ) ddfn += antisymf[ii][i];
                      if ( k == kk )
                      {
                         ddfn += (-co1*dAdv2[k][i]*dAdv1[k][ii]
                                     + co2*ddAdv2dv1[k][i][ii])
                                     *(SDIM_dot(ds1[k],vnorm)+SDIM_dot(ds2[k],vnorm));
                         ddfn += co2*dAdv2[k][i]*
                          (SDIM_dot(ddss11[kk][ii],vnorm)
                              +SDIM_dot(ddss12[kk][ii],vnorm));
                         ddfn += co2*dAdv1[kk][ii]*
                            (SDIM_dot(ddss21[k][i],vnorm)
                              +SDIM_dot(ddss22[k][i],vnorm));
                         dddsf = 4*vnorm[i]*s1[ii] -2*vnorm[ii]*s1[i] 
                                     - 2*((i==ii) ? SDIM_dot(s1,vnorm) : 0.0)
                                 + 4*vnorm[ii]*s2[i] - 2*vnorm[i]*s2[ii]
                                     - 2*((i==ii) ? SDIM_dot(s2,vnorm) : 0.0);
                         ddfn -= co3*dddsf;
                      }
           h = 1.5*hh0*dAdv2[k][i]*(dfndv1[kk][ii]/nn-fn/nn/nn*dnndv1[kk][ii])
             + 1.5*hh0*dAdv1[kk][ii]*(dfndv2[k][i]/nn-fn/nn/nn*dnndv2[k][i])
             + 1.5*area*(dfndv2[k][i]/nn-fn/nn/nn*dnndv2[k][i])*
                           (dfndv1[kk][ii]/nn-fn/nn/nn*dnndv1[kk][ii])
             + 1.5*area*hh0*(ddfn/nn - dfndv2[k][i]*dnndv1[kk][ii]/nn/nn
                    - dfndv1[kk][ii]*dnndv2[k][i]/nn/nn
                    +2*fn*dnndv2[k][i]*dnndv1[kk][ii]/nn/nn/nn
                     - fn*ddnn/nn/nn);
           if ( k == kk )
              h += 0.75*ddAdv2dv1[k][i][ii]*hh0*hh0;
                      break;
                }
                if ( !is_finite(h) )
                { sprintf(errmsg,
                    "Star sq curvature hessian infinite at vertex %s\n",
                       ELNAME(v_info->id));
                  kb_error(3619,errmsg,RECOVERABLE);
                }
                h21[i][ii] += h;
                h01[i][ii] -= h;
                h20[i][ii] -= h;
                h00[i][ii] += h;

                /* 22 term */
                ddff = SDIM_dot(dfdv2[k][i],dfdv2[kk][ii]);
                if ( k == kk )
                { ddff += (-co1*dAdv2[k][i]*dAdv2[k][ii] + co2*ddAdv2dv2[k][i][ii])
                              *dssf;
                  ddff += co2*dAdv2[k][i]*ddssf2[ii];
                  ddff += co2*dAdv2[kk][ii]*ddssf2[i];
                  dddsf = ((i==ii) ? 4*s1f : 0. )
                             - 2*s1[i]*dAdv[ii] - 2*s1[ii]*dAdv[i];
                  ddff -= co3*dddsf;
                }
              ddff *= 2;
              switch ( variety )
              { case TEST_SQ:
                      ddnn = 2*SDIM_dot(dvnorm2[k][i],dvnorm2[kk][ii]);
                      h = ddnn;
                     break;
                 case PLAIN_SQ:
                     h = -aa75*dffdv2[k][i]*dAdv2[kk][ii]
                         - aa75*dffdv2[kk][ii]*dAdv2[k][i]
                         + ffaaa*dAdv2[k][i]*dAdv2[kk][ii]
                         + a75*ddff;
                     if ( k == kk )
                        h -= ffaa*ddAdv2dv2[k][i][ii];
                      break;
                 case EFF_SQ:
                      ddnn = 2*SDIM_dot(dvnorm2[k][i],dvnorm2[kk][ii]);
                      h = an75*ddff
                         - ann75*dffdv2[k][i]*dnndv2[kk][ii]
                         - ann75*dffdv2[kk][ii]*dnndv2[k][i]
                         + fnnna*dnndv2[k][i]*dnndv2[kk][ii]
                         - fnna*ddnn
                         + fn75*dffdv2[k][i]*dAdv2[kk][ii]
                         + fn75*dffdv2[kk][ii]*dAdv2[k][i]
                         - fnn75*dAdv2[kk][ii]*dnndv2[k][i]
                         - fnn75*dnndv2[kk][ii]*dAdv2[k][i];
                      if ( k == kk )
                         h += ffn75*ddAdv2dv2[k][i][ii];
                      break;
                 case NORMAL_SQ:
                      ddfn = SDIM_dot(dfdv2[k][i],dvnorm2[kk][ii])
                              + SDIM_dot(dvnorm2[k][i],dfdv2[kk][ii]);
                      if ( k == kk )
                      {
                         ddfn += (-co1*dAdv2[k][i]*dAdv2[k][ii]
                                     + co2*ddAdv2dv2[k][i][ii])
                                     *(SDIM_dot(ds1[k],vnorm)+SDIM_dot(ds2[k],vnorm));
                         ddfn += co2*dAdv2[k][i]*
                          (SDIM_dot(ddss21[kk][ii],vnorm)
                              +SDIM_dot(ddss22[kk][ii],vnorm));
                         ddfn += co2*dAdv2[kk][ii]*
                            (SDIM_dot(ddss21[k][i],vnorm)
                              +SDIM_dot(ddss22[k][i],vnorm));
                         dddsf = ((i==ii) ? 4*SDIM_dot(s1,vnorm) : 0. )
                                    - 2*s1[i]*vnorm[ii] - 2*s1[ii]*vnorm[i];
                         ddfn -= co3*dddsf;
                      }
                      h  = afnfn*dffdv2[k][i]*dffdv2[kk][ii]
                          + afffnfn*ddff
                          - ffafnfnfn*dffdv2[k][i]*dfndv2[kk][ii]
                          + fffnfn*dffdv2[k][i]*dAdv2[kk][ii]
                          - ffafnfnfn*dfndv2[k][i]*dffdv2[kk][ii]
                          + afffffnfnfnfn*dfndv2[k][i]*dfndv2[kk][ii]
                          - fffffnfnfna*ddfn
                          - fffffnfnfn*dfndv2[k][i]*dAdv2[kk][ii]
                          + fffnfn*dAdv2[k][i]*dffdv2[kk][ii]
                          - fffffnfnfn*dAdv2[k][i]*dfndv2[kk][ii];
                      if ( k == kk ) 
                         h += fffffnfn*ddAdv2dv2[k][i][ii];
                      break;
                   case PERP_SQ:
                      ddnn = 2*SDIM_dot(dvnorm2[k][i],dvnorm2[kk][ii]);
                      ddfn = SDIM_dot(dfdv2[k][i],dvnorm2[kk][ii])
                              + SDIM_dot(dvnorm2[k][i],dfdv2[kk][ii]);
                      if ( k == kk )
                      {
                         ddfn += (-co1*dAdv2[k][i]*dAdv2[k][ii]
                                     + co2*ddAdv2dv2[k][i][ii])
                                     *(SDIM_dot(ds1[k],vnorm)+SDIM_dot(ds2[k],vnorm));
                         ddfn += co2*dAdv2[k][i]*
                          (SDIM_dot(ddss21[kk][ii],vnorm)
                              +SDIM_dot(ddss22[kk][ii],vnorm));
                         ddfn += co2*dAdv2[kk][ii]*
                            (SDIM_dot(ddss21[k][i],vnorm)
                              +SDIM_dot(ddss22[k][i],vnorm));
                         dddsf = ((i==ii) ? 4*SDIM_dot(s1,vnorm) : 0. )
                                    - 2*s1[i]*vnorm[ii] - 2*s1[ii]*vnorm[i];
                         ddfn -= co3*dddsf;
                      }
           h = 1.5*hh0*dAdv2[k][i]*(dfndv2[kk][ii]/nn-fn/nn/nn*dnndv2[kk][ii])
             + 1.5*hh0*dAdv2[kk][ii]*(dfndv2[k][i]/nn-fn/nn/nn*dnndv2[k][i])
             + 1.5*area*(dfndv2[k][i]/nn-fn/nn/nn*dnndv2[k][i])*
                           (dfndv2[kk][ii]/nn-fn/nn/nn*dnndv2[kk][ii])
             + 1.5*area*hh0*(ddfn/nn - dfndv2[k][i]*dnndv2[kk][ii]/nn/nn
                    - dfndv2[kk][ii]*dnndv2[k][i]/nn/nn
                    +2*fn*dnndv2[k][i]*dnndv2[kk][ii]/nn/nn/nn
                     - fn*ddnn/nn/nn);
           if ( k == kk )
              h += 0.75*ddAdv2dv2[k][i][ii]*hh0*hh0;
                      break;
                }
                if ( !is_finite(h) )
                { sprintf(errmsg,
                    "Star sq curvature hessian infinite at vertex %s\n",
                       ELNAME(v_info->id));
                  kb_error(3620,errmsg,RECOVERABLE);
                } 
                h22[i][ii] += h;
                h02[i][ii] -= h;
                h20[i][ii] -= h;
                h00[i][ii] += h;

             } /* end for i, ii */
        } /* end for kk */
     } /* end for k */

    if ( get_vattr(v_info->id) & AXIAL_POINT )
    { REAL fudge = 1./rotorder;
      if ( sym_flags & DOUBLE_AXIAL ) fudge *= 2;
      fudge *= rotorder/v_info->axial_order;
      for ( i = 0 ; i < v_info->vcount ; i++ )
        for ( ii = 0 ; ii < v_info->vcount ; ii++ )
         for ( k = 0 ; k < SDIM ; k++ )
          for ( kk = 0 ; kk < SDIM ; kk++ )
             v_info->hess[i][ii][k][kk] *= fudge;
    }

all_exit:

  if ( v_info->vcount > MAXV )
  { myfree((char*)a);
    free_matrix(ds1);
    if ( mode != METHOD_VALUE ) free_matrix3(ddss11);
  }
  return energy;
} /* end star_sq_curve_method_all() */

/*************************************************************************
*
*  function: star_sqcurve_method_value()
*
*  purpose: calculate squared mean curvature of given vertex
*              from precalculated data.
*
*/

REAL star_sqcurve_method_value(v_info)
struct qinfo *v_info;
{
#define USE_ALL
#ifdef USE_ALL
  return star_sqcurve_method_all(v_info,METHOD_VALUE);  
#else

  /* following is streamlined for just energy */

  int variety; /* PLAIN_SQ, EFF_SQ, NORMAL_SQ */
  int pairs;
  int j,k;
  REAL energy,ff,fn,nn,area;
  REAL dAdv[MAXCOORD], a, d, s1s1, s1s2, s2s2;
  REAL vnorm[MAXCOORD];
  REAL ds1,ds2,coeff;
  REAL *s1,*s2;
  REAL **s = v_info->sides[0];
  REAL temp[MAXCOORD];
  MAT2D(proj,MAXCOORD,MAXCOORD); /* for constraint projection */
  int concount; /* number of constraints vertex is on */
  int final_edge; /* of pairs; wrap to first for complete star */
  
  if ( METH_INSTANCE(v_info->method)->gen_method 
           == star_normal_sq_mean_curvature_mi )
     variety = NORMAL_SQ;
  else 
  { if ( h0_flag ) 
     kb_error(1617,"Can only use star_normal_sq_mean_curvature with h_zero.\n",
                RECOVERABLE);
     if ( METH_INSTANCE(v_info->method)->gen_method 
             == star_eff_area_sq_mean_curvature_mi )
        variety = EFF_SQ;
     else variety = PLAIN_SQ;
  }
  pairs = (v_info->vcount - 1);
  if ( pairs <= 0 ) return 0.0;
  if ( v_info->flags & INCOMPLETE_STAR )
  { pairs--;
    final_edge = pairs;
  }
  else 
    final_edge = 0;

  /* constraint projection */
  concount = constr_proj_matrix(v_info->id,proj);

  /* basic dot products */
  area = 0.0;
  for ( j = 0 ; j < SDIM ; j++ ) dAdv[j] = vnorm[j] = 0.0;
  for ( k = 0 ; k < pairs ; k++ )
  { s1 = s[k]; s2 = s[(k+1==pairs)?final_edge:k+1];
    s1s1 = SDIM_dot(s1,s1);
    s1s2 = SDIM_dot(s1,s2);
    s2s2 = SDIM_dot(s2,s2);
    d = s1s1*s2s2 - s1s2*s1s2;
    a = sqrt(d);
    area += a;
    coeff = 0.5/a;
    for ( j = 0 ; j < SDIM ; j++ )
    { ds1 = (s2s2*s1[j] - s1s2*s2[j]);
      ds2 = (s1s1*s2[j] - s1s2*s1[j]);
      dAdv[j] -= coeff*(ds1 + ds2);
    }
    if ( variety != PLAIN_SQ )
    { cross_prod(s1,s2,temp);
      for ( j = 0 ; j < SDIM ; j++ )
        vnorm[j] += 0.5*temp[j];
    }
  }
  area *= 0.5;

  if ( concount )
  { /* project force and normal to constraints. This is enough to take
       care of energy and gradient, since force and energy occur only in
       dot products.
       */
    int i;
    for ( i = 0 ; i < SDIM ; i++ )
       temp[i] = dAdv[i];
    matvec_mul(proj,temp,dAdv,SDIM,SDIM);
    for ( i = 0 ; i < SDIM ; i++ )
       temp[i] = vnorm[i];
    matvec_mul(proj,temp,vnorm,SDIM,SDIM);
  }

  /* energy */
  ff = SDIM_dot(dAdv,dAdv);
  switch ( variety )
  { case PLAIN_SQ: 
        energy = 0.75/area*ff;
        break;
     case EFF_SQ:
        nn = SDIM_dot(vnorm,vnorm);
        energy = 0.75*area*ff/nn;
        break;
     case NORMAL_SQ:
     { REAL h0_val;
       switch ( h0_flag )
       { case H0_IN_GLOBAL: h0_val = h0_value; break;
         case H0_IN_ATTR:   h0_val = *VREAL(v_info->id,h0_attr); break;
         default: h0_val = 0;
       }
       fn = SDIM_dot(dAdv,vnorm);
       if ( fn == 0.0 ) energy = 0.0;
       else energy = 0.75*area*(ff/fn-2*h0_val/3)*(ff/fn-2*h0_val/3);
       break;
     }
   }

  if ( get_vattr(v_info->id) & AXIAL_POINT )
  { energy /= rotorder;
     if ( sym_flags & DOUBLE_AXIAL ) energy *= 2;
     energy *= rotorder/v_info->axial_order;
  }

  return energy;
#endif
}

/*************************************************************************
*
*  function: star_sqcurve_method_grad()
*
*  purpose:  Convert square curvature data into forces for a vertex.
*
*/

REAL star_sqcurve_method_grad(v_info)
struct qinfo *v_info;
{
#ifdef USE_ALL
  return star_sqcurve_method_all(v_info,METHOD_GRADIENT); 
#else 


 /* version optimized for gradient */
 
  int variety; /* PLAIN_SQ, EFF_SQ, NORMAL_SQ */
  int pairs;
  int i,j,k;
  REAL energy,ff,fn,nn,area,g;
  REAL dAdv[MAXCOORD], *a=NULL, *d, *s1s1, *s1s2, *s2s2;
  REAL vnorm[MAXCOORD];
  REAL **dAdv1,**dAdv2,dvnorm1[MAXCOORD][MAXCOORD]
      ,dvnorm2[MAXCOORD][MAXCOORD],**ds2;
  REAL *s1,*s2;
  REAL **s = v_info->sides[0];
  REAL temp[MAXCOORD],ea,a15,ef1,ef2,ns1,ns2;
  REAL aa[MAXV*5];
  MAT2D(ds1,12*MAXV,MAXCOORD);
  REAL h0adj;
  
  switch ( h0_flag )
  { case H0_IN_GLOBAL: h0adj = 2./3*h0_value; break;
    case H0_IN_ATTR:   h0adj = 2./3*(*VREAL(v_info->id,h0_attr)); break;
    default: h0adj = 0.0; break;
  }

  for ( i = 0 ; i < SDIM ; i++ )
    dvnorm1[i][i] = dvnorm2[i][i] = 0.0;

  if ( METH_INSTANCE(v_info->method)->gen_method 
              == star_normal_sq_mean_curvature_mi )
     variety = NORMAL_SQ;
  else if ( METH_INSTANCE(v_info->method)->gen_method 
              == star_eff_area_sq_mean_curvature_mi )
     variety = EFF_SQ;
  else variety = PLAIN_SQ;

  pairs = (v_info->vcount - 1);
  if ( pairs <= 0 ) return 0.0;

  if ( v_info->vcount > MAXV )
  { a = (REAL*)mycalloc(5*pairs,sizeof(REAL));
    ds1 = dmatrix(0,12*v_info->vcount,0,SDIM-1);
  } else
  { memset((char*)aa,0,sizeof(REAL)*5*pairs);
    a = aa;
    memset((char*)ds1[0],0,sizeof(REAL)*12*v_info->vcount*MAXCOORD);
  }
  d = a+pairs; s1s1 = d + pairs; s1s2 = s1s1 + pairs; s2s2 = s1s2 + pairs;
  ds2 = ds1 + pairs; dAdv1 = ds2 + pairs; dAdv2 = dAdv1 + pairs;

  /* basic dot products */
  area = 0.0;
  for ( j = 0 ; j < SDIM ; j++ ) dAdv[j] = vnorm[j] = 0.0;
  for ( k = 0 ; k < pairs ; k++ )
  { s1 = s[k]; s2 = s[(k+1==pairs)?0:k+1];
    s1s1[k] = SDIM_dot(s1,s1);
    s1s2[k] = SDIM_dot(s1,s2);
    s2s2[k] = SDIM_dot(s2,s2);
    d[k] = s1s1[k]*s2s2[k] - s1s2[k]*s1s2[k];
    a[k] = 0.5*sqrt(d[k]);
    area += a[k];
    for ( j = 0 ; j < SDIM ; j++ )
    { ds1[k][j] = 2*(s2s2[k]*s1[j] - s1s2[k]*s2[j]);
      ds2[k][j] = 2*(s1s1[k]*s2[j] - s1s2[k]*s1[j]);
      dAdv1[k][j] = 0.125/a[k]*ds1[k][j];
      dAdv2[k][j] = 0.125/a[k]*ds2[k][j];
      dAdv[j] -= dAdv1[k][j] + dAdv2[k][j];
    }
    if ( variety != PLAIN_SQ )
    { cross_prod(s1,s2,temp);
      for ( j = 0 ; j < SDIM ; j++ )
        vnorm[j] += 0.5*temp[j];
    }
  }

  /* energy */
  ff = SDIM_dot(dAdv,dAdv);
  switch ( variety )
  { case PLAIN_SQ: 
        energy = 0.75/area*ff;
        a15 = 1.5/area;  /* coefficient for later */
        break;
     case EFF_SQ:
        nn = SDIM_dot(vnorm,vnorm);
        energy = 0.75*area*ff/nn;
        ef1 = 1.5*area/nn;    /* coefficients for later */
        ef2 = 1.5*area/nn/nn*ff;
        break;
     case NORMAL_SQ:
        fn = SDIM_dot(dAdv,vnorm);
        if ( fn == 0.0 ) energy = 0.0;
        else
        { energy = 0.75*area*(ff/fn-h0adj)*(ff/fn-h0adj);
          ns1 = 3*area/fn*(ff/fn-h0adj); /* coefficients for later */
          ns2 = 1.5*area*(ff/fn-h0adj)*ff/fn/fn;
        }
        break;
    }

    /* gradient */
    ea = energy/area;
    for ( k = 0 ; k < pairs ; k++ )
    { REAL co = 0.25/a[k];
      REAL coco = 0.5*co/a[k];
      REAL *grad2 = (k+1==pairs)?v_info->grad[1]:v_info->grad[k+2];
      s1 = s[k]; s2 = s[(k+1==pairs)?0:k+1];
      if ( variety != PLAIN_SQ ) 
      { dvnorm1[0][1] = -0.5*s2[2];
        dvnorm1[0][2] =  0.5*s2[1];
        dvnorm1[1][0] =  0.5*s2[2];
        dvnorm1[1][2] = -0.5*s2[0];
        dvnorm1[2][0] = -0.5*s2[1];
        dvnorm1[2][1] =  0.5*s2[0];
        dvnorm2[0][1] =  0.5*s1[2];
        dvnorm2[0][2] = -0.5*s1[1];
        dvnorm2[1][0] = -0.5*s1[2];
        dvnorm2[1][2] =  0.5*s1[0];
        dvnorm2[2][0] =  0.5*s1[1];
        dvnorm2[2][1] = -0.5*s1[0];
      }
      for ( i = 0 ; i < SDIM ; i++ )
      { REAL coeff1 = coco*dAdv1[k][i];
        REAL coeff2 = coco*dAdv2[k][i];
        REAL dAdn1,dAdn2;
        REAL ddAdv1dv,ddAdv2dv;
        dAdn1 = dAdn2 = ddAdv1dv = ddAdv2dv = 0.0;
        for ( j = 0 ; j < SDIM ; j++ )
        { 
          REAL ddss11,ddss12,ddss21,ddss22;
          REAL t1,t2;
          ddss11 = -s2[i]*s2[j];
          ddss12 = 2*s1[i]*s2[j] - s2[i]*s1[j];
          ddss21 = 2*s2[i]*s1[j] - s1[i]*s2[j];
          ddss22 = -s1[i]*s1[j];
          if ( i == j )
          { ddss11 += s2s2[k];
            ddss12 -= s1s2[k];
            ddss21 -= s1s2[k];
            ddss22 += s1s1[k];
          }
          t1 = (coeff1*(ds1[k][j]+ds2[k][j]) - co*(ddss11 + ddss12));
          ddAdv1dv += dAdv[j]*t1;
          t2 = (coeff2*(ds1[k][j]+ds2[k][j]) - co*(ddss21 + ddss22));
          ddAdv2dv += dAdv[j]*t2;
          if ( variety == NORMAL_SQ )
          { dAdn1 += vnorm[j]*t1; dAdn2 += vnorm[j]*t2; }
        }

         switch ( variety )
         { case PLAIN_SQ: 
             g = a15*ddAdv1dv - ea*dAdv1[k][i] ;
             v_info->grad[k+1][i] += g;
             v_info->grad[0][i] -= g;
             g = a15*ddAdv2dv - ea*dAdv2[k][i];
             grad2[i] += g;
             v_info->grad[0][i] -= g;
             break;
           case EFF_SQ:
             g = ea*dAdv1[k][i] + ef1*ddAdv1dv
                      - ef2*SDIM_dot(dvnorm1[i],vnorm);
             v_info->grad[k+1][i] += g;
             v_info->grad[0][i] -= g;
             g = ea*dAdv2[k][i] + ef1*ddAdv2dv
                      - ef2*SDIM_dot(dvnorm2[i],vnorm);
             grad2[i] += g;
             v_info->grad[0][i] -= g;
             break;
            case NORMAL_SQ:
             if ( fn != 0.0 )
              { g = ea*dAdv1[k][i] + ns1*ddAdv1dv
                    - ns2*(dAdn1 + SDIM_dot(dvnorm1[i],dAdv)); 
                 v_info->grad[k+1][i] += g;
                 v_info->grad[0][i] -= g;
                 g = ea*dAdv2[k][i] + ns1*ddAdv2dv
                    - ns2*(dAdn2 + SDIM_dot(dvnorm2[i],dAdv)); 
                 grad2[i] += g;
                 v_info->grad[0][i] -= g;
                }
             break;
         }
      }
    }

  if ( get_vattr(v_info->id) & AXIAL_POINT )
  { REAL fudge = 1./rotorder;
    if ( sym_flags & DOUBLE_AXIAL ) fudge *= 2;
    fudge *= rotorder/v_info->axial_order;
    energy *= fudge;
    for ( i = 0 ; i < v_info->vcount ; i++ )
       for ( j = 0 ; j < SDIM ; j++ )
          v_info->grad[i][j] *= fudge;
  }

  if ( v_info->vcount > MAXV )
  { myfree((char*)a);
    free_matrix(ds1);
  }
  return energy;
#endif
}

/*************************************************************************
*
*  function: star_sqcurve_method_hess()
*
*
*/

REAL star_sqcurve_method_hess(v_info)
struct qinfo *v_info;
{
   return star_sqcurve_method_all(v_info,METHOD_HESSIAN);  
}

/*************************************************************************
**************************************************************************
 
  Method circle_willmore

  Alexander Bobenko's circle-based discrete Willmore energy.
  Is conformally invariant.
  At each vertex, energy is the sum of the angles between facet
  circumcircles - 2*pi.  More simply done as edge quantity, since
  angles at each end are the same.

  For edge e, if adjacent facet edge loops are a e d and b c -e, then
  circle angle beta for edge has
  
  cos(beta) = (<a,c><b,c>-<a,b><c,d>-<b,c><d,a>)/|a|/|b|/|c|/|d|

  For now, assumes all vertices are faceted, and fully starred.
  
   Severe numerical difficulties: Not smooth when angle beta is zero,
  which is all too common.  Set of zero angles should be codimension 2,
  which means generally avoided, but still crops up.

**************************************************************************/

void circle_willmore_init(mode,mi)
int mode; /* METHOD_VALUE or METHOD_GRADIENT */
struct method_instance *mi;
{ REAL vertex_adjust;

  if ( web.modeltype != LINEAR )
    kb_error(4007,"circle_willmore method requires the LINEAR model.\n",
      RECOVERABLE);
  if ( web.representation != SOAPFILM )
    kb_error(3222,"circle_willmore method requires the SOAPFILM model.\n",
      RECOVERABLE);

  vertex_adjust = -M_PI*web.skel[VERTEX].count;

  binary_tree_add(mi->value_addends,vertex_adjust);
}

REAL circle_willmore_all(e_info,mode)
struct qinfo *e_info;
int mode; /* METHOD_VALUE, METHOD_GRADIENT, or METHOD_HESSIAN */
{ REAL *q = e_info->sides[0][0];
  REAL *p = e_info->sides[0][1];
  REAL *r = e_info->sides[0][2];
  REAL pp = dot(p,p,SDIM);
  REAL qq = dot(q,q,SDIM);
  REAL rr = dot(r,r,SDIM);
  REAL pq = dot(p,q,SDIM);
  REAL pr = dot(p,r,SDIM);
  REAL qr = dot(q,r,SDIM);
  REAL value;
  REAL cosbeta;
  REAL dcdp[MAXCOORD],dcdq[MAXCOORD],dcdr[MAXCOORD];
  REAL coeff;
  int i;

  cosbeta = (rr*pq - qq*pr - pp*rr + pp*qr)/sqrt(pp)/sqrt(rr)/
              sqrt(qq-2*pq+pp)/sqrt(rr-2*qr+qq);
  if ( fabs(cosbeta) > 1.0 )
  { sprintf(errmsg,"edge %s cosine of angle out of range in circular_willmore",
      ELNAME(e_info->id));
     kb_error(3850,errmsg,WARNING);
  }
  value = acos(cosbeta);

  if ( mode == METHOD_VALUE ) 
    return value;
 
  /* Gradient */
  if ( cosbeta*cosbeta >= 1.0 ) coeff = 0.0;
  coeff = -1/sqrt(1 - cosbeta*cosbeta);
  for ( i = 0 ; i < SDIM ; i++ )
  { dcdp[i] = (rr*q[i] - qq*r[i] - 2*rr*p[i] + 2*qr*p[i])/sqrt(pp)/sqrt(rr)/
              sqrt(qq-2*pq+pp)/sqrt(rr-2*qr+qq)
            - cosbeta/pp*p[i]
            - cosbeta/(qq-2*pq+pp)*(-q[i] + p[i]);
    dcdq[i] = (rr*p[i] - 2*pr*q[i] + pp*r[i])/sqrt(pp)/sqrt(rr)/
              sqrt(qq-2*pq+pp)/sqrt(rr-2*qr+qq)
            - cosbeta/(qq-2*pq+pp)*(q[i] - p[i])
            - cosbeta/(rr-2*qr+qq)*(-r[i] + q[i]);
    dcdr[i] = (2*r[i]*pq - qq*p[i] - 2*pp*r[i]+ pp*q[i])/sqrt(pp)/sqrt(rr)/
              sqrt(qq-2*pq+pp)/sqrt(rr-2*qr+qq)
            - cosbeta/rr*r[i]
            - cosbeta/(rr-2*qr+qq)*(r[i] - q[i]);
    e_info->grad[1][i] = coeff*dcdq[i]; 
    e_info->grad[0][i] -= coeff*dcdq[i]; 
    e_info->grad[2][i] = coeff*dcdp[i]; 
    e_info->grad[0][i] -= coeff*dcdp[i]; 
    e_info->grad[3][i] = coeff*dcdr[i]; 
    e_info->grad[0][i] -= coeff*dcdr[i]; 
  }

  if ( mode == METHOD_GRADIENT ) 
    return value;


  /* Hessian */
 
  

    return value;
  
}

REAL circle_willmore_value(e_info)
struct qinfo *e_info;
{ return circle_willmore_all(e_info,METHOD_VALUE);
}

REAL circle_willmore_grad(e_info)
struct qinfo *e_info;
{ return circle_willmore_all(e_info,METHOD_GRADIENT);
}

REAL circle_willmore_hess(e_info)
struct qinfo *e_info;
{ return circle_willmore_all(e_info,METHOD_HESSIAN);
}



