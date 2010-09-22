/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/**********************************************************************
*
*  File: pixgraph.c      
*
*  Purpose: Creates data file for Pixar from current triangulation.
*              Hooks directly to graphgen().
*
*/

#include "include.h"

/* Pixar defines */
#define QUAD 1
#define NQUAD 2
#define CQUAD 3
#define CNQUAD 4


static FILE *pfd = NULL;
static  REAL **verts; /* for adjusted triangle vertices */

/****************************************************************************
*
*  Function: pix_start()
*
*  Purpose:  Initialize Pixar output file.
*/


char pix_file_name[150];         /* base picture name  */

void pix_start()
{
  char quadname[160];    /* quadrilateral file */
  char response[120];
  char *tptr;

  if ( pix_file_name[0] == 0 )
  {  prompt("Enter name of picture file: ",pix_file_name,
       sizeof(pix_file_name)-10);

  if ( commandfd == stdin )
  {
    for ( normflag = -1 ; normflag == -1 ; )
    {
      prompt("Do normal interpolation? ",response,sizeof(response));
      switch(toupper(response[0]))
         { case 'Y': normflag = 1; break;
           case 'N': normflag = 0; break;
           case '0': 
              kb_error(3223,"Pixar file aborted.\n",RECOVERABLE);
           default: outstring("Unintelligible response. Try again.\n");
         }
    }

    prompt("Do inner, outer, or all surfaces? (i,o,a)",response,sizeof(response));
    switch ( response[0] )
    { case 'i' : innerflag = 1; outerflag = 0; break;
      case 'o' : innerflag = 0; outerflag = 1; break;
      default  : innerflag = 1; outerflag = 1; break;
    }
    prompt("Do body colors? ",response,sizeof(response));
    if ( toupper(response[0]) == 'Y' )
      colorflag = 1;
    else colorflag = 0;
    if ( colorflag && ( strlen(cmapname) == 0 ) )
    { prompt("Enter name of colormap file: ",cmapname,sizeof(cmapname));
      if ( cmapname[0] == 0 )
      { outstring("No colormap used.\n"); colorflag = 0; }
    }

    thickness = overall_size/1000;
    sprintf(msg,"Thicken(n | y [thickness(%g)])? ",(DOUBLE)thickness);
    prompt(msg,response,sizeof(response));
    if ( toupper(response[0]) == 'Y' )
    { thickenflag = 1;
      strtok(response," \t,;:\n");
      tptr = strtok(NULL," \t;:,\n");
      if ( tptr )
        thickness = atof(tptr);
    }
    else thickenflag = 0;
  }
  }
  
  strcpy(quadname,pix_file_name);
  strcat(quadname,".quad");
  pix_file_name[0] = 0;
  pfd = fopen(quadname,"w"); 
  if (pfd == NULL)
  { perror(quadname); 
    kb_error(3373,"Bad filename.\n",RECOVERABLE);
  } 

  if ( normflag ) 
  { fprintf(pfd,"CNPOLY");
  }
  else 
  { fprintf(pfd,"CPOLY");
  }
  fprintf(pfd,"\n");
  verts = dmatrix(0,2,0,2);
}

/************************************************************************
*
*  Function: pix_facet()
*
*  Purpose:  Accepts facets from graphgen() and plots them.
*/

void pix_facet(gdata,f_id)
struct graphdata *gdata;
facet_id f_id;
{
  int i,j;
  REAL map[4];  /* color of this vertex */
  int color = get_facet_color(f_id);

  if ( color == CLEAR ) return;
  if ( colormap )
      for ( i = 0 ; i < 4 ; i++ ) map[i] = colormap[gdata[0].color][i];
  else  {
             for ( j = 0 ; j < 3 ; j++ )
                map[j] = rgb_colors[color][j];
             map[3] = facet_alpha;
          }
             
  if ( thickenflag )
     for ( i = 0 ; i < FACET_VERTS ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
          gdata[i].x[j] += gdata[i].norm[j]*thickness;

  for ( i = 0 ; i < FACET_VERTS ; i++ )
     { 
        fprintf(pfd,"%17.15f %17.15f %17.15f  ",(DOUBLE)gdata[i].x[0],
            (DOUBLE)gdata[i].x[1],(DOUBLE)gdata[i].x[2]); 
        if ( normflag )
          { fprintf(pfd,"%5.3f %5.3f %5.3f     ",(DOUBLE)gdata[i].norm[0],
                      (DOUBLE)gdata[i].norm[1],(DOUBLE)gdata[i].norm[2]);
          }
         fprintf(pfd,"%5.3f %5.3f %5.3f %5.3f    ",
                    (DOUBLE)map[0],(DOUBLE)map[1],(DOUBLE)map[2],(DOUBLE)map[3]);
     }

  i = FACET_VERTS-1;  /* repeat last point */ 
  fprintf(pfd,"%17.15f %17.15f %17.15f  ",(DOUBLE)gdata[i].x[0],
      (DOUBLE)gdata[i].x[1],(DOUBLE)gdata[i].x[2]); 
  if ( normflag )
     { fprintf(pfd,"%5.3f %5.3f %5.3f     ",(DOUBLE)gdata[i].norm[0],
                 (DOUBLE)gdata[i].norm[1],(DOUBLE)gdata[i].norm[2]);
     }
  fprintf(pfd,"%5.3f %5.3f %5.3f %5.3f    ",
              (DOUBLE)map[0],(DOUBLE)map[1],(DOUBLE)map[2],(DOUBLE)map[3]);

  fprintf(pfd,"\n");

  if ( thickenflag )
    { /* do other orientation */
     for ( i = 0 ; i < FACET_VERTS ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
          gdata[i].x[j] -= gdata[i].norm[j]*2*thickness;

     for ( i = FACET_VERTS-1 ; i >=  0 ; i-- )
      { 
        fprintf(pfd,"%17.15f %17.15f %17.15f  ",(DOUBLE)gdata[i].x[0],
            (DOUBLE)gdata[i].x[1],(DOUBLE)gdata[i].x[2]); 
        if ( normflag )
          { fprintf(pfd,"%5.3f %5.3f %5.3f     ",-(DOUBLE)gdata[i].norm[0],
                      -(DOUBLE)gdata[i].norm[1],-(DOUBLE)gdata[i].norm[2]);
          }
        fprintf(pfd,"%5.3f %5.3f %5.3f %5.3f    ",(DOUBLE)map[0],(DOUBLE)map[1],
            (DOUBLE)map[2],(DOUBLE)map[3]);
      }

     i = 0;  /* repeat last point */ 
     fprintf(pfd,"%17.15f %17.15f %17.15f  ",(DOUBLE)gdata[i].x[0],
         (DOUBLE)gdata[i].x[1],(DOUBLE)gdata[i].x[2]); 
     if ( normflag )
        { fprintf(pfd,"%5.3f %5.3f %5.3f     ",-(DOUBLE)gdata[i].norm[0],
                 -(DOUBLE)gdata[i].norm[1],-(DOUBLE)gdata[i].norm[2]);
        }
     fprintf(pfd,"%5.3f %5.3f %5.3f %5.3f    ",(DOUBLE)map[0],(DOUBLE)map[1],(DOUBLE)map[2],(DOUBLE)map[3]);
    } /* end thickenflag */

  fprintf(pfd,"\n");
}

/*******************************************************************
*
*  Function: pix_end()
*
*  Purpose:  Finish off Pixar file.
*
*/

void pix_end()
{
  fclose(pfd);
  free_matrix(verts);
}





