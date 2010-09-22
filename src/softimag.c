/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*****************************************************************
*
*  File: softimag.c
*
*  Purpose:  Triangle list file output for Softimage input.
*/

#include "include.h"

static FILE *fd;

/*****************************************************************
*
*  Function: softimage()
*
*  Purpose:  Write Softimage format files.
*/

void softimage()
{
  char file_name[100];
  char name[100];
  vertex_id v_id;
  facet_id f_id;
  int *vnumber;
  int n;

  prompt("Enter file name (no suffix): ",name,sizeof(name));

  /* model file */
  strcpy(file_name,name);
  strcat(file_name,".mdl");
  fd = fopen(file_name,"w");
  if ( fd == NULL )
  { perror(file_name);
    return;
  }
  fprintf(fd,"SOFTIMAGE 4D Creative Environment    v 1.6  \"ASCII\"\n\n\n");
  fprintf(fd,"  MODL    \"%s\"\n  {\n     type     PMSH\n",name);
  fprintf(fd,"     nbdef    1\n");
  fprintf(fd,"     scal        1.000000  1.000000  1.000000\n");
  fprintf(fd,"     rot         0.000000  0.000000  0.000000\n");
  fprintf(fd,"     trans      0.000000  0.000000  0.000000\n");
  fprintf(fd,"  }\n");
  fclose(fd);


  strcpy(file_name,name);
  strcat(file_name,".def");
  fd = fopen(file_name,"w");
  if ( fd == NULL )
  { perror(file_name);
    return;
  }
  fprintf(fd,"SOFTIMAGE 4D Creative Environment    v 1.6  \"ASCII\"\n\n");
  fprintf(fd,"PMSH    \"%s\"\n  {\n  ",name);

  /* vertex list */
  vnumber = (int*)temp_calloc(web.skel[VERTEX].max_ord+1,sizeof(int*));

  fprintf(fd,"    vertex %ld\n",web.skel[VERTEX].count);
  n = 1;
  FOR_ALL_VERTICES(v_id)
  { REAL *x = get_coord(v_id);
    fprintf(fd,"%12d %14.12f %14.12f %14.12f\n",n,
      (DOUBLE)x[0],(DOUBLE)x[1],(DOUBLE)x[2]);
    vnumber[loc_ordinal(v_id)] = n;
    n++;
  } 
 

  /* triangle list */
  fprintf(fd,"\n    polygon %ld\n",web.skel[FACET].count);
  FOR_ALL_FACETS(f_id)
  { facetedge_id fe = get_facet_fe(f_id);
    facetedge_id next_fe = get_next_edge(fe);
    int oh = vnumber[loc_ordinal(get_fe_headv(fe))];
    fprintf(fd,"     %10d %10d %10d %10d\n",oh,
            vnumber[loc_ordinal(get_fe_headv(next_fe))],
            vnumber[loc_ordinal(get_fe_tailv(fe))],
            vnumber[loc_ordinal(get_fe_tailv(fe))]);
  } 
  fprintf(fd,"    }\n\n");
 
  fclose(fd);

  temp_free((char*)vnumber);
}


