/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/**********************************************************************
*
*  File: userfunc.c
*
*  Purpose: Lets user compile own function definitions for use in
*           expressions.  Function passed pointer to coordinate array.
*           User should also provide derivative function.
*           After defining functions, add names to arrays userfunc,
*           userfunc_deriv, and userfunc_second that follow all the
*           function definitions. 
*           Names in this file don't mattter; the first function
*           in the arrays is usr1 in datafile or queries, etc.  
*           When invoked in Evolver formulas, these functions do
*           not take arguments; arguments are implicitly point
*           coordinates.
*           Example: (usr1 + usr3)/usr10.
*
*           Also has functions for handling dynamic load libraries.
*           And elliptic functions.
*/

#include "include.h"

/**************************************************************************/

/**************************************************************************
*
*  function: userfunc_init()
*
*  purpose: called once at the start of a surface to give user functions
*           a chance to initialize.
*/

void userfunc_init()
{
  /* do whatever it takes to initialize user functions */
  
  /*  Example of error reporting. 
  kb_error(2201,"Error in userfunc_init\n",RECOVERABLE);
  */
}

/* usr1 as defined here gives conformal metric for 3-sphere in 
    stereographic projection */

REAL usr1 ARGS((REAL *));
REAL usr1_deriv ARGS((REAL *,REAL *));
REAL usr1_seconds ARGS((REAL *,REAL *,REAL **));

REAL usr1(x)
REAL *x; /* incoming parameters */
{ REAL denom;
  denom = 4+x[0]*x[0]+x[1]*x[1]+x[2]*x[2]; 
  return 16/denom/denom;
}

REAL usr1_deriv(x,partials)
REAL *x; /* incoming parameters */
REAL *partials; /* outgoing partial derivatives */
{ REAL denom,cube;
  int i;

  denom = 4+x[0]*x[0]+x[1]*x[1]+x[2]*x[2]; 
  cube = denom*denom*denom;
  for ( i = 0 ; i < SDIM ; i++ )
     partials[i] = -64/cube*x[i]; 

  return 16/denom/denom;
}

REAL usr1_seconds(x,partials,seconds)
REAL *x; /* incoming parameters */
REAL *partials; /* outgoing partial derivatives */
REAL **seconds; /* outgoing second derivatives */
{ REAL denom,cube,quart;
  int i,j;

  denom = 4+x[0]*x[0]+x[1]*x[1]+x[2]*x[2]; 
  cube = denom*denom*denom;
  quart = cube*denom;
  for ( i = 0 ; i < SDIM ; i++ )
     partials[i] = -64/cube*x[i]; 
  for ( i = 0 ; i < SDIM ; i++ )
  { for ( j = 0 ; j < SDIM ; j++ )
        seconds[i][j] = 384*x[i]*x[j]/quart;
     seconds[i][i] -= 64/cube;
  }

  return 16/denom/denom;
}

/***************************************************************************/

/* Another example of a user function, which is a polynomial in x,y,z.     */
/* This function is referred to as usr2 in expressions.                    */

static REAL usr_poly ARGS((REAL *));
static REAL usr_poly_grad ARGS((REAL *,REAL *));
static REAL usr_poly_hess ARGS((REAL *,REAL *,REAL **));

static REAL usr_poly(x)
REAL *x; /* incoming parameters */
{ 
    return x[0]*x[0] + x[1]*x[2] + x[2]*x[2]*x[2];
}

static REAL usr_poly_grad(x,partials)
REAL *x; /* incoming parameters */
REAL *partials; /* outgoing partial derivatives */
{
  partials[0] = 2*x[0];
  partials[1] = x[2];
  partials[2] = x[1] + 3*x[2]*x[2];

  return x[0]*x[0] + x[1]*x[2] + x[2]*x[2]*x[2];
}

static REAL usr_poly_hess(x,partials,seconds)
REAL *x; /* incoming parameters */
REAL *partials; /* outgoing partial derivatives */
REAL **seconds; /* outgoing second derivatives */
{ 
  partials[0] = 2*x[0];
  partials[1] = x[2];
  partials[2] = x[1] + 3*x[2]*x[2];

  seconds[0][0] = 2.0;
  seconds[0][1] = seconds[1][0] = 0.0;
  seconds[0][2] = seconds[2][0] = 0.0;
  seconds[1][1] = 0.0;
  seconds[1][2] = seconds[2][1] = 1.0;
  seconds[2][2] = 6*x[2];

  return x[0]*x[0] + x[1]*x[2] + x[2]*x[2]*x[2];
}

/**************************************************************************/

/* Add your functions to these arrays; this is how they will be invoked! */
#ifdef NOPROTO
REAL (*userfunc[])() = {usr1,usr_poly};
REAL (*userfunc_deriv[])() = {usr1_deriv,usr_poly_grad};
REAL (*userfunc_seconds[])() = {usr1_seconds,usr_poly_hess};
#else
REAL (*userfunc[])(REAL*) = {usr1,usr_poly};
REAL (*userfunc_deriv[])(REAL*,REAL*) = {usr1_deriv,usr_poly_grad};
REAL (*userfunc_seconds[])(REAL*,REAL*,REAL**) = {usr1_seconds,usr_poly_hess};
#endif

/**************************************************************************/
/**************************************************************************/


/* A user defined attribute function.  Undocumented. */
/* Use "user_attr" in queries like length or area or id */

REAL user_attribute(id)
element_id id;
{
  /* a sample smorgasbord */
  switch ( id_type(id) )
  { case VERTEX: return get_coord(id)[0];
     case EDGE:    return get_edge_length(id);
     case FACET:  return get_facet_area(id);
     case BODY:    return get_body_volume(id);
     case FACETEDGE: return (REAL)(loc_ordinal(id)+1);
  }
  return 0.0;
}

/*********************************************************************
**********************************************************************

             D Y N A M I C     L O A D     L I B R A R I E S

**********************************************************************
*********************************************************************/

/*********************************************************************
*
* function: load_library()
*
* purpose: Find and load dynamic library.  Searches current directory,
*              EVOLVERPATH, default library path.
*/

#ifdef WIN32
#define dlopen(name,mode)  LoadLibrary(name)
#define dlclose(handle)    FreeLibrary(handle)
#define dlsym(handle,name) GetProcAddress(handle,name)
#endif

void load_library(libname)
char *libname;
{  
#ifdef ENABLE_DLL
  int k;
  char *env;
  char path[200];
  int len;
  void *fd;

  for ( k = 0 ; k < MAX_DLL ; k++ )
     if ( dll_list[k].name == NULL ) break;

  if ( k >= MAX_DLL )
     kb_error(2202,"Too many dynamic load libraries.\n",DATAFILE_ERROR);


  env = getenv("EVOLVERPATH");

  /* try current directory first */
  strcpy(path,"./");
  strncpy(path+2,libname,sizeof(path)-2);
  while ( (fd = dlopen(path,RTLD_NOW)) == NULL)
     { /* try paths in EVOLVERPATH */
        if ( env == NULL ) break;
        len = strcspn(env,ENVPATHCHAR);
        if ( len == 0 ) break;
        strncpy(path,env,len);
        path[len] = PATHCHAR;
        strncpy(path+len+1,libname,sizeof(path)-len-2);
        if ( env[len] == 0 ) env = NULL; /* end of EVOLVERPATH */
        else env += len+1;
     } 
  /* try given name */
  if ( ! fd )
  { strncpy(path,libname,sizeof(path));
    fd = dlopen(path,RTLD_NOW);
  }
  
  if ( ! fd )
  { sprintf(errmsg,"Cannot open dynamic library %s. Reason:\n",libname);
#ifndef WIN32
    strncpy(errmsg+strlen(errmsg),dlerror(),sizeof(errmsg)-strlen(errmsg)-2);
#endif   
    kb_error(2203,errmsg,DATAFILE_ERROR);
  }

  dll_list[k].name = mycalloc(1,strlen(libname)+4);
  strcpy(dll_list[k].name,libname);

  dll_list[k].handle = fd;

#else

  kb_error(2204,"This Evolver not compiled for dynamic load libraries.\n",
     DATAFILE_ERROR);
#endif

}

/*************************************************************************
*
* function: unload_libraries
*
* purpose: unload dynamic link libraries
*/

void unload_libraries()
{
  int k;

  for ( k = 0 ; k < MAX_DLL ; k++ )
     if ( dll_list[k].name )
     { myfree(dll_list[k].name);
       dll_list[k].name = NULL;
#ifdef ENABLE_DLL
       dlclose(dll_list[k].handle); 
       dll_list[k].handle = NULL;
#endif
     }
}

/*********************************************************************
*
* function: search_libraries()
*
* purpose: find function name in dynamic load libraries.
*
* return: pointer to function. NULL if not found.
*/

dll_func_type search_libraries(funcname)
char *funcname;
{ 

#ifdef ENABLE_DLL
  int i;
  dll_func_type f;

  for ( i = 0 ; i < MAX_DLL ; i++ )
    if ( dll_list[i].handle ) 
     { f = (dll_func_type)dlsym(dll_list[i].handle,funcname);
        if ( f ) return f;
     }
#endif
  return NULL;
}

/****************************************************************************
                     E L L I P T I C   F U N C T I O N S
****************************************************************************/

/*****************************
 Complete elliptic integrals
*****************************/

REAL ellipticK(m)
REAL m;
{ REAL a,b,anext;

  if ( m >= 1.0 )
     kb_error(2422,"ellipticK domain violation, parameter >= 1.\n",RECOVERABLE);

  a = 1.0;
  b = sqrt(sqrt(1 - m));

  while ( fabs(a-b) > machine_eps )
  { anext = (a + b)/2;
    b = sqrt(sqrt(a*b*(a*a+b*b)/2));
    a = anext;
  }

  return M_PI/2/a/a;
}


REAL ellipticE(m)
REAL m;
{ REAL a,b,anext;
  REAL K,sum = 0;
  REAL ff = 1.0;

  if ( m > 1.0 )
     kb_error(2423,"ellipticE domain violation, parameter > 1.\n",RECOVERABLE);
  if ( m == 1.0 ) return 1.0;

  a = 1.0; 
  b = sqrt(sqrt(1 - m));

  while ( fabs(a-b) > machine_eps )
  { REAL aa = a*a, bb = b*b;
    sum += ff*(aa*aa - (aa+bb)*(aa+bb)/4);
    ff *= 4;
    anext = (a + b)/2;
    b = sqrt(sqrt(a*b*(a*a+b*b)/2));
    a = anext;
  }

  K = M_PI/2/a/a;
  return K*(1.0 - sum);
}

REAL ellipticEdm(m)
REAL m;
{ if ( m == 1.0 ) return 1.0e31;
  return m==0 ? -M_PI/8 : (ellipticE(m) - ellipticK(m))/2/m;
}

REAL ellipticKdm(m)
REAL m;
{ return m==0 ? M_PI/8 : (ellipticE(m) - (1-m)*ellipticK(m))/2/m/(1-m);
}

REAL ellipticEdmdm(m)
REAL m;
{ return  (m==0) ? -3./64*M_PI : ((m-2)*ellipticE(m) - 2*(m-1)*ellipticK(m))
            /4/m/m/(1-m);
}

REAL ellipticKdmdm(m)
REAL m;
{ return (m == 0) ? 9./64*M_PI : 
     ((4*m-2)*ellipticE(m) + (2-5*m+3*m*m)*ellipticK(m))/4/m/m/(1-m)/(1-m);
}


REAL incompleteEllipticFdphi(phi,m)
REAL phi,m;
{ return 1/sqrt(1 - m*sin(phi)*sin(phi));
}


/* following Abramowitz and Stegun 17.6 */
REAL incompleteEllipticE(phi,m)
REAL phi,m;
{ REAL p,tanp,a,b,c,poweroftwo,csum,E,csinphisum,F,K,retval;
  REAL anext,bnext,cnext;

  if ( m > 1.0 )
     kb_error(2424,"incompleteEllipticE domain violation, parameter > 1.\n",
        RECOVERABLE);
  if ( m == 0 ) return phi;

  p = phi; 
  tanp = tan(p);
  a = 1.0; 
  b = sqrt(1-m);
  c = sqrt(m);
  poweroftwo = 1.0;
  csum = c*c;
  csinphisum = 0;

  while ( c > machine_eps )
  { 
    p = 2*p + atan((b/a - 1)*tanp/(1+b/a*tanp*tanp));
    tanp = (1+b/a)*tanp/(1-b/a*tanp*tanp);

    anext = (a+b)/2;
    bnext = sqrt(a*b);
    cnext = (a-b)/2;
    a = anext; b = bnext; c = cnext;

    poweroftwo *= 2;
    csum += poweroftwo*c*c;
    csinphisum += c*sin(p);
  }
  K = M_PI/2/a;
  E = K - csum*K/2;
  F = p/poweroftwo/a;
  retval = E/K*F + csinphisum;

  return retval;
}

REAL incompleteEllipticF(phi,m)
REAL phi,m;
{ REAL p,tanp,a,b,c,poweroftwo,csum,csinphisum,F;
  REAL anext,bnext,cnext;
  p = phi; 
  tanp = tan(p);
  a = 1.0; 
  b = sqrt(1-m);
  c = sqrt(m);
  poweroftwo = 1.0;
  csum = c*c;
  csinphisum = 0;

  if ( m > 1.0 )
     kb_error(2425,"incompleteEllipticE domain violation, parameter > 1.\n",
        RECOVERABLE);
  if ( m == 0 ) return phi;

  while ( c > machine_eps )
  { 
    p = 2*p + atan((b/a - 1)*tanp/(1+b/a*tanp*tanp));
    tanp = (1+b/a)*tanp/(1-b/a*tanp*tanp);

    anext = (a+b)/2;
    bnext = sqrt(a*b);
    cnext = (a-b)/2;
    a = anext; b = bnext; c = cnext;

    poweroftwo *= 2;
    csum += poweroftwo*c*c;
    csinphisum += c*sin(p);
  }
  F = p/poweroftwo/a;

  return F;
}

REAL incompleteEllipticEdphi(phi,m)
REAL phi,m;
{ return sqrt(1 - m*sin(phi)*sin(phi));
}

REAL incompleteEllipticEdm(phi,m)
REAL phi,m;
{ if ( m == 0 )
     return -(2*phi-sin(2*phi))/8;
  return (incompleteEllipticE(phi,m)-incompleteEllipticF(phi,m))/2/m;
}

REAL incompleteEllipticFdm(phi,m)
REAL phi,m;
{ if ( m == 0 )
     return (2*phi-sin(2*phi))/8;
  return incompleteEllipticE(phi,m)/2/(m-1)/m
          - incompleteEllipticF(phi,m)/2/m
              + sin(2*phi)/4/(m-1)/sqrt(1-m*sin(phi)*sin(phi));
}

REAL incompleteEllipticEseconds(phi,m,dphi,dm,ddphi,ddm,dphidm)
REAL phi,m; /* input */
REAL *dphi,*dm,*ddphi,*ddm,*dphidm;  /* output */
{ REAL E,F;
  REAL s = sin(phi);
  REAL s2 = sin(2*phi);
  REAL d = sqrt(1-m*sin(phi)*sin(phi));
  E = incompleteEllipticE(phi,m);
  F = incompleteEllipticF(phi,m);
  *dphi = d;
  if ( m == 0 )
  { *dm = -(2*phi-s2)/8;
    *ddm = -1./128*(12*phi-8*s2+sin(4*phi));
  }
  else
  { *dm   = (E - F)/2/m;
    *ddm =  -1./8/(m-1)/m/m*(2*(m-2)*E - 4*(m-1)*F + m*s2/d);
          
  }
  *ddphi = -m*cos(phi)*s/d;
  *dphidm = -s*s/2/d;
  return E;
}


REAL incompleteEllipticFseconds(phi,m,dphi,dm,ddphi,ddm,dphidm)
REAL phi,m; /* input */
REAL *dphi,*dm,*ddphi,*ddm,*dphidm;  /* output */
{ REAL E,F;
  REAL s = sin(phi);
  REAL s2 = sin(2*phi);
  REAL d = sqrt(1-m*sin(phi)*sin(phi));
  E = incompleteEllipticE(phi,m);
  F = incompleteEllipticF(phi,m);
  *dphi = 1/d;
  if ( m == 0 )
  { *dm = (2*phi-s2)/8;
    *ddm = 3./128*(12*phi-8*s2+sin(2*phi));
  }
  else
  { *dm   = -E/2/(m-1)/m - F/2/m + s2/4/(m-1)/d;
    *ddm = E/2/(m-1)/m/m + E/2/(m-1)/(m-1)/m
          - (E-F)/4/(m-1)/m/m + F/2/m/m + s*s*s2/8/(m-1)/d/d/d
          - s2/4/(m-1)/(m-1)/d + E/4/(m-1)/m/m
          + F/4/m/m - s2/8/m/(m-1)/d;
  }
  *ddphi = m*cos(phi)*s/d/d/d;
  *dphidm = s*s/2/d/d/d;
  return F;
}


