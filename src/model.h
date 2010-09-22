/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/


/**********************************************************************
*
*  File: model.h
*
*  Header file for evolver.  Defines model parameters, i.e. 
*       for torus model, lagrange model, space dimension.
*
*/

#ifdef SDIM
#define DEFAULT_SDIM SDIM
#ifndef MAXCOORD
#define MAXCOORD SDIM
#endif
#if SDIM==2
#define SDIM_dot(x,y)  ((x)[0]*(y)[0] + (x)[1]*(y)[1])
#else
#if SDIM==3
#define SDIM_dot(x,y)  ((x)[0]*(y)[0] + (x)[1]*(y)[1] + (x)[2]*(y)[2])
#else
#define SDIM_dot(x,y)  dot(x,y,SDIM)
#endif
#endif
#else
#define SDIM web.sdim
#define SDIM_dot(x,y)  dot(x,y,SDIM)
#define DEFAULT_SDIM 3
#endif

/* maximum number of shared memory processors */
#ifdef SHARED_MEMORY
#ifndef MAXPROCS
#define MAXPROCS 4
#endif
#else
#define MAXPROCS 1
#endif

/* maximum dimensionality */
#ifndef MAXCOORD
#define MAXCOORD 4
#endif

/* MAXPARAM is maximum number of boundary parameters. Must be
    at most MAXCOORD since is saved in same space by save_coord(). */
#define MAXPARAM MAXCOORD
#define EDGE_VERTS 2
#define FACET_VERTS 3
#define FACET_EDGES 3

/* model types for web.modeltype */
#define LINEAR     1
#define QUADRATIC 2
#define LAGRANGE  3

/* Quadratic model point counts */
/* Control points per edge */
#define EDGE_CTRL  3
/* Control points per facet */
#define FACET_CTRL 6
/* Integration points per edge */
#define EDGE_INTERP  3
/* Integration points per facet */
#define FACET_INTERP 7

/* quadratic interpolation coefficients */
/* partials of interpolation polynomials at midpoints of patch edges */
/* control points are numbered 0 to 5 counterclockwise */
/* midpoints are numbered 0 to 2 counterclockwise after control pt 0 */
/* dip[midpoint][control point][partial]  */

extern REAL dip[3][FACET_CTRL][2];

/* Histogram size */
#define HISTO_BINS 40 
#define HISTO_BINSIZE (1/M_LN2)
