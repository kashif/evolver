/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/* Null graphics module for evolver */

#include "include.h"

/* callable null functions of the right types */
void null_start ARGS((void)) {}
void null_edge(struct graphdata * g,edge_id id) {}
void null_facet(struct graphdata * g,facet_id id) {}
void display_null(struct tsort * t) {}

void graph_new_surface() {}

void display()
{
  if ( init_graphics == NULL )
     kb_error(1252,"No screen display available.  This Evolver compiled with nulgraph.c.\n",WARNING);

  ENTER_GRAPH_MUTEX
  init_graphics = null_function;
  finish_graphics = null_function;
  graph_edge = null_edge;
  graph_start = null_start;
  graph_facet = null_facet;
  graph_end = null_function;
  display_facet = display_null;
  
  graphgen();
  LEAVE_GRAPH_MUTEX
}
