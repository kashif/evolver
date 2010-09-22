/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/


/**********************************************************
*
*  File: MVgraph.c
*
*  Contents:  Routines for interface with shared memory
*                 MinneView.
*/

#include "include.h"


#ifndef OOGL
void Begin_OOGL()
{
  kb_error(1251,"This Evolver not compiled with the OOGL option.\n",WARNING);
}

void UpdateOOGL()
{
}

void End_OOGL()
{
}

#else 

void Begin_OOGL() {}

void End_OOGL()
{
  OOGL_flag = 0;
  if ( geomview_flag ) End_geomview();
}

void UpdateOOGL()
{
  void (*old_start)ARGS((void));
  void (*old_end)ARGS((void));  
  void (*old_gedge)ARGS((struct graphdata *,edge_id));
  void (*old_gfacet)ARGS((struct graphdata*,facet_id)); 
 
  /* if user has asked us to quit, don't bother redisplaying */
  /* (especially since graphgen resets breakflag!) */
  if (breakflag) return;

  /* save current screen graphics pointers */
  old_start = graph_start;
  old_end    = graph_end;
  old_gfacet = graph_facet;
  old_gedge = graph_edge;

  /* OOGL pointers */
  if ( geomview_flag )
    {
      graph_start = geomview_start;
      graph_facet = geomview_facet;
      graph_edge  = geomview_edge;
      graph_end    = geomview_end;
    }

  /* do output */
  graphgen();

  /* restore old graphics */
  graph_start = old_start;
  graph_end    = old_end;
  graph_edge  = old_gedge;
  graph_facet = old_gfacet;

}
#endif

