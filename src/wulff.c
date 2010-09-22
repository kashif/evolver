/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/****************************************************************
*
*  File: wulff.c
*
*  Purpose:  Routines dealing with surface energy as function
*             of normal direction.
*/

/*****************************************************************
*
*  Discussion:  For a chunk of surface represented by its
*  normal vector N (length is area of chunk), its energy is
*  the inner product of N with the Wulff vector W, which is a
*  function of N.  For a given set of Wulff vectors, W is the
*  one whose inner product with N is greatest.  The Wulff vectors
*  are the vertices of the Wulff crystal.  For simple area,
*  W is the normalization of N.
*
*  Force is energy gradient, so for N depending on a parameter u,
*
*        dE/du = <dN/du,W> + <N,dW/du>
*
*  However, the fact that W has maximal product with N implies
*  <N,dW/du> = 0, so  dE/du = <dN/du,W>.  So we need only
*  the Wulff vector to calculate both energy and force.
*/

#include "include.h"

/*******************************************************
*
*  wulff_initialize()
*
*  Purpose: Read in Wulff vectors from file and initialze
*           Wulff variables.  If special name, redirects
*           Wulff functions to  special functions in this 
*           file.
*
*  Input:    Name of file with Wulff vectors.
*
*  Output:  Array of Wulff vectors, wulff_flag set
*/

void wulff_initialize(wulffname)
char *wulffname;
{
  FILE *wfd;
  REAL *row; /* current Wulff vector */
  int k;         /* vector number */
  int j;         /* dimension number */

  if ( (web.representation != SOAPFILM ) || (SDIM != 3) )
     kb_error(1385,"Can only do Wulff energy for 2D surface in 3D.\n",RECOVERABLE);

  if ( !wulffname || (wulffname[0] == 0) )
     kb_error(2857,"Missing Wulff file name.\n",RECOVERABLE);
  web.wulff_flag = 1;

  /* save  name */
  strncpy(web.wulff_name,wulffname,sizeof(web.wulff_name));

  /* test special names */
  if ( (strncmp(wulffname,"hemisphere",10) == 0  )
      || (strcmp(wulffname,"hemi") == 0)  )
  { get_wulff = hemi_wulff;
    return;
  }
  else if ( strncmp(wulffname,"lens",4) == 0 )
  { get_wulff = lens_wulff;
    return;
  }

  /* default is from file */
  get_wulff = file_wulff;

  wfd = path_open(wulffname,NOTDATAFILENAME);
  if ( wfd == NULL )
     { sprintf(errmsg,"Cannot open Wulff vector file %s.\n",wulffname);
       kb_error(1386,errmsg,DATAFILE_ERROR);
       return;
     }

  for ( k = 0 ; k < MAXWULFF ; k++ )
     { row = wulff_vector[k];
       for ( j = 0 ; j < SDIM ; j++ )
#ifdef LONGDOUBLE
       if ( fscanf(wfd,"%Lf",row+j) != 1 ) break;
#else
       if ( fscanf(wfd,"%lf",row+j) != 1 ) break;
#endif
     }
  fclose(wfd);
  web.wulff_count = k;
}

/******************************************************************8
*
*  Function: file_wulff()
*
*  Purpose:  Finds Wulff vector that has maximum dot product
*                with given vector.
*
*  Input:     Pointer to given vector, 
*                pointer to destination of Wulff vector.
*
*  Output:    Components of Wulff vector put in place.
*/

void file_wulff(norm,wulff)
REAL *norm;
REAL *wulff;
{
  REAL maxw = -1e20;
  REAL *w; /* Wulff vector being tested */
  int k;
  int best = 0;

  for ( k = 0 ; k < web.wulff_count ; k++ )
     { w = wulff_vector[k];
        if ( SDIM_dot(w,norm) > maxw )
          { best = k;
             maxw = SDIM_dot(w,norm);
          }
     }
  memcpy((char *)wulff,(char *)wulff_vector[best],SDIM*sizeof(REAL));
}

/******************************************************************
*
*  Function: hemi_wulff()
*
*  Purpose:  Provide Wulff vector for upper hemisphere Wulff shape.
*/

void hemi_wulff(normal,wulff)
REAL *normal;
REAL *wulff;
{
  int i;
  REAL norm;

  wulff[0] = normal[0];
  wulff[1] = normal[1];
  if ( normal[2] < 0.0 )
     wulff[2] = 0.0;
  else wulff[2] = normal[2];
  norm = sqrt(SDIM_dot(wulff,wulff));
  if ( norm > 0.0 )
     for ( i = 0 ; i < SDIM ; i++ )
        wulff[i] /= norm;
}

/********************************************************************
*
*  Function: lens_wulff
*
*  Purpose:  Provide Wulff vector for lens-shaped Wulff shape.
*/

void lens_wulff(normal,wulff)
REAL *normal;
REAL *wulff;
{
  REAL norm;

  /* test whether interior or edge Wulff vector */  
  norm = dot(normal,normal,2);     /* x and y only */
  if ( norm < 3*normal[2]*normal[2] )
     { /* interior */
        norm = sqrt(norm + normal[2]*normal[2]);
        if ( norm > 0.0 )
          { wulff[0] = normal[0]/norm;
             wulff[1] = normal[1]/norm;
             wulff[2] = normal[2]/norm - 0.5;
          }
     }
  else
     { /* edge */
        norm = sqrt(norm/0.75);
        wulff[0] = normal[0]/norm;
        wulff[1] = normal[1]/norm;
        wulff[2] = 0.0;
     }
}

/************************************************************************

                 Wulff energy as method

**************************************************************************/

void wulff_method_init(mode,mi)
int mode;
struct method_instance *mi;
{ char response[200];

  if ( web.modeltype != LINEAR )
    kb_error(2859,"Wulff energy can only be done in LINEAR model.\n",
     RECOVERABLE);

  if ( web.wulff_flag == 0 ) 
  { prompt("Enter Wulff name (hemi,lens, or filename): ",response,sizeof(response));
     wulff_initialize(response);
  }
}

REAL facet_wulff_value(f_info)
struct qinfo *f_info;
{ REAL normal[MAXCOORD];
  REAL wulff [MAXCOORD];
  REAL density = get_facet_density(f_info->id);
  REAL energy;
  REAL side  [MAXCOORD][MAXCOORD];
  int i,j;

  for ( i = 0 ; i < FACET_EDGES ; i++ )
     { int ii = (i+1)%FACET_EDGES;
        for ( j = 0 ; j < SDIM ; j++ )
            side[i][j] = f_info->x[ii][j] - f_info->x[i][j];
     }
  /* calculate normal */ 
  cross_prod(side[0],side[1],normal);
  (*get_wulff)(normal,wulff);
  energy = SDIM_dot(wulff,normal)/2;
  return density*energy;
}

REAL facet_wulff_grad(f_info)
struct qinfo *f_info;
{ REAL normal[MAXCOORD];
  REAL wulff [MAXCOORD];
  REAL temp  [MAXCOORD];
  REAL side  [MAXCOORD][MAXCOORD];
  REAL density = get_facet_density(f_info->id);
  REAL energy;
  int i,j;

  for ( i = 0 ; i < FACET_EDGES ; i++ )
     { int ii = (i+1)%FACET_EDGES;
        for ( j = 0 ; j < SDIM ; j++ )
            side[i][j] = f_info->x[ii][j] - f_info->x[i][j];
     }
  /* calculate normal */ 
  cross_prod(side[0],side[1],normal);
  (*get_wulff)(normal,wulff);
  /* force on each vertex */
  for ( i = 0 ; i < FACET_VERTS ; i++ )  /* vertex loop */
      { int k;
         j = (i+1)%FACET_EDGES;  /* opposite side */
         cross_prod(side[j],wulff,temp);
         for ( k = 0 ; k < SDIM ; k++ ) 
             f_info->grad[i][k] -= density*temp[k]/2;
      }
  energy = SDIM_dot(wulff,normal)/2;
  return density*energy;
}
