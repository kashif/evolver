/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/


#include "include.h"

int find_vertex_average ARGS(( vertex_id , REAL *, int));
void old_vertex_average ARGS((int));

/**************************************************************
*
*  Function: vertex_average()
*
*  Purpose:  For soapfilm model, move unconstrained vertices
*            to average position of neighbors.
*
*/

static  struct averages { REAL x[MAXCOORD];
                          REAL normal[MAXCOORD]; 
                          REAL area;  /* weighting */
                          int status;    /* <0 for don't move */
                          int triples;  /* number of triple lines into vertex */
                          edge_id trip_e[2]; /* id's of triple edges */
                        } *average;

void vertex_average(mode)
int mode;    /* VOLKEEP to keep volumes on both sides same */
             /* NOVOLKEEP to obey just constraints */
             /* RAWEST to ignore all restrictions */
{ 
  REAL *x;
  vertex_id v_id;
  int i;
 
  #ifdef MPI_EVOLVER
  if ( this_task == 0 )
    mpi_vertex_average(mode);
  #endif

  if ( web.skel[VERTEX].count == 0 ) return;

  average = (struct averages *)temp_calloc(web.skel[VERTEX].max_ord+1,
                                                  sizeof(struct averages));
  FOR_ALL_VERTICES(v_id)
   if ( get_vattr(v_id) & (FIXED|BOUNDARY) ) 
     average[loc_ordinal(v_id)].status = -10;

  if ( web.representation == SIMPLEX ) 
    { old_vertex_average(mode); return; }

  FOR_ALL_VERTICES(v_id)
     average[loc_ordinal(v_id)].status = 
              find_vertex_average(v_id,average[loc_ordinal(v_id)].x,mode);

  FOR_ALL_VERTICES(v_id)
  {
     int attr = get_vattr(v_id);
     REAL *newx = average[loc_ordinal(v_id)].x;
     x = get_coord(v_id);
     if (attr & BOUNDARY )
     { /* update boundary parameter to agree with coordinates */
        b_extrapolate(get_boundary(v_id),x,newx,newx,get_param(v_id),get_param(v_id),v_id);
     }
     else
     {
        for ( i = 0 ; i < SDIM ; i++ ) x[i] = newx[i];
          if (attr & CONSTRAINT )
             project_v_constr(v_id,ACTUAL_MOVE,RESET_ONESIDEDNESS);
     }
  }

  temp_free((char*)average);
}

/***********************************************************************
*
* Function: old_vertex_average()
*
* Purpose: Mass vertex averaging, invoked by V, rawv, rawestv.
*/

void old_vertex_average(mode)
int mode;    /* VOLKEEP to keep volumes on both sides same */
             /* NOVOLKEEP to obey just constraints */
             /* RAWEST to ignore all restrictions */
{
  facet_id f_id;
  edge_id e_id;
  vertex_id v_id;
  REAL *x;
  struct averages *ave;
  int i;
  REAL xbar[MAXCOORD];
  REAL lambda;

  if ( web.modeltype == LAGRANGE )
     kb_error(1379,"No vertex averaging in Lagrange model yet.\n",RECOVERABLE );


  if ( web.skel[VERTEX].count == 0 ) return;

  average = (struct averages *)temp_calloc(web.skel[VERTEX].max_ord+1,
                                                  sizeof(struct averages));
  FOR_ALL_VERTICES(v_id)
      if ( get_vattr(v_id) & (FIXED|BOUNDARY) ) average[loc_ordinal(v_id)].status = -10;

  /* accumulate data on neighbors and weights */
  if ( web.representation == SIMPLEX )
  { if ( mode == VOLKEEP )
      kb_error(1380,
      "Vertex averaging mode V not implemented for simplex model. Use rawv.\n",
                 RECOVERABLE);
    FOR_ALL_FACETS(f_id)
      simplex_facet_average(f_id,mode);
  } 
  else if ( web.representation == SOAPFILM )
  { FOR_ALL_FACETS(f_id)
      facet_average(f_id,mode);
  } 
  else if ( web.representation == STRING )
  { FOR_ALL_EDGES(e_id)
     edge_average(e_id,mode);
  } 

  /* move vertices */
  if ( web.representation != STRING )
  { FOR_ALL_VERTICES(v_id)
    {
      if ( get_vattr(v_id) & (FIXED|BOUNDARY) ) continue;
      ave = average + loc_ordinal(v_id);
      if (ave->status <= 0 ) continue;
      if (ave->area == 0.0 ) continue;
      if (ave->triples == 1 ) continue;
      x = get_coord(v_id);
      for ( i = 0 ; i < SDIM ; i++ )
          xbar[i] = ave->x[i]/ave->area;

      if ( mode == VOLKEEP )
      { REAL numer;
        /* volume preserving projection */
        numer = SDIM_dot(ave->normal,xbar) 
                    - SDIM_dot(ave->normal,x);
        if ( numer != 0.0 ) /* in case ave->normal == 0 */
           lambda = numer/SDIM_dot(ave->normal,ave->normal);
        else lambda = 0.0;
        if  ( ave->triples > 0 ) /* along edge */
           for ( i = 0 ; i < SDIM ; i++ )
             xbar[i] = x[i] + lambda*ave->normal[i];
        else /* subtract normal component */
           for ( i = 0 ; i < SDIM ; i++ )
             xbar[i] -= lambda*ave->normal[i];
      }
      /* tends to round off sharp vertices, I hope */
      if ( web.modeltype == QUADRATIC )
      { REAL *xm;
        edge_id next_e;
        { next_e = e_id = get_vertex_edge(v_id);
          do
          { xm = get_coord(get_edge_midv(next_e));
            for ( i = 0 ; i < SDIM ; i++ )
               xm[i] += (xbar[i]-x[i])/2;
            next_e = get_next_tail_edge(next_e);
          }
          while ( !equal_element(next_e,e_id) );
        }
      }
      for ( i = 0 ; i < SDIM ; i++ )
         x[i] = xbar[i];

      if ( get_vattr(v_id) & CONSTRAINT )
        project_v_constr(v_id,ACTUAL_MOVE,RESET_ONESIDEDNESS);
    }

  }
  else /* string */
  { FOR_ALL_VERTICES(v_id)
    {
      ave = average + loc_ordinal(v_id);
      if (ave->status != 2 ) continue; /* too many edges or something */
      x = get_coord(v_id);

      if ( mode == VOLKEEP )
      { /* ave->normal really tangent */
         /* area preserving projection */
         lambda = - SDIM_dot(ave->normal,ave->x)/3 /* prevent overshoot */
                        /SDIM_dot(ave->normal,ave->normal);
         for ( i = 0 ; i < SDIM ; i++ )
            x[i] -=  lambda*ave->normal[i];
      }
      else
      {
         for ( i = 0 ; i < SDIM ; i++ )
            x[i] +=  ave->x[i]/ave->status/2;  /* avoid overshoot */
      }
    }
  }

  if ( web.modeltype == QUADRATIC )
  { FOR_ALL_EDGES(e_id)
       new_vertex_average(get_edge_midv(e_id),VOLKEEP);
  }

  temp_free((char *)average);

  /* recalculation responsibility of caller */
} /* end old_vertex_average() */

/***********************************************************************
*
* function: facet_average()
*
* purpose: find one facet's contributions to averaging of its vertices.
*
*/      

void facet_average(f_id,mode)
facet_id f_id;
int mode; /* averaging mode */
{
  int i,j,k,ii;
  REAL side[FACET_EDGES][2][MAXCOORD];
  REAL normal[MAXCOORD];
  REAL area;
  facetedge_id fe_id;
  int sign;
  REAL *x[3];

  /* get side vectors */
  fe_id = get_facet_fe(f_id);
  for ( i = 0 ; i < FACET_EDGES ; i++ )
  { 
    get_fe_side(fe_id,side[i][0]);
    get_fe_side(inverse_id(fe_id),side[i][1]); /* in case of wraps */
    x[i] = get_coord(get_fe_tailv(fe_id));
    fe_id = get_next_edge(fe_id);
  }
     
  /* calculate normal */ 
  cross_prod(side[0][0],side[1][0],normal);
  for ( i = 3 ; i < SDIM ; i++ ) normal[i] = 0.0 ; /* kludge */
     
  area = get_facet_area(f_id);  /* in case of metric or wraps */

  k = -1;
  fe_id = get_facet_fe(f_id);
  for ( ii = 0 ; ii < FACET_EDGES ; ii++, fe_id = get_next_edge(fe_id) )
    { edge_id e_id = get_fe_edge(fe_id);
      vertex_id headv = get_fe_headv(fe_id);
      vertex_id tailv = get_fe_tailv(fe_id);
      struct averages *head,*tail;

      k++; /* instead of maybe being skipped by continue at end of loop */
      head = average + loc_ordinal(headv);
      tail = average + loc_ordinal(tailv);

      if ( mode != RAWEST )
      { /* check constraint compatibility */
         int attr = get_vattr(tailv);
         int found,kk,bad;

         if ( (attr & CONSTRAINT) )
          { conmap_t * vconmap = get_v_constraint_map(tailv);
             conmap_t * econmap = get_e_constraint_map(e_id);

             bad = 0;
             for ( j = 1 ; j <= (int)vconmap[0] ; j++ )
             { for ( kk = 1, found = 0 ; kk <= (int)econmap[0] ; kk++ )
                  if ( econmap[kk] == (vconmap[j] & ~CON_HIT_BIT) )
                  { found = 1; break; }
                if ( !found ) {bad = 1; break;}
             }
             if ( bad ) continue; /* with next edge */
          }
         else if ( attr & BOUNDARY )
         { 
            if ( get_edge_boundary(e_id) != get_boundary(tailv) ) continue;
         } 
      }
      if ( mode != RAWEST )
         if ( !equal_id(get_prev_facet(fe_id),get_next_facet(fe_id)) )
          { /* triple edge, just move by edge */
             if ( head->status < 0 ) {}
             else if ( head->triples == 0 )
             { /* first detection */
                for ( i = 0 ; i < SDIM ; i++ ) 
                  { head->x[i] = x[(k+1)%3][i] - side[k][0][i]; 
                     head->normal[i] = -side[k][0][i]; 
                  }
                head->status = 1;
                head->area = 1.0;
                head->triples++;
                head->trip_e[0] = e_id;
             }
             else if ( head->triples == 1 )
             { /* second detection */
                if ( !equal_element(e_id,head->trip_e[0]) )
                { for ( i = 0 ; i < SDIM ; i++ ) 
                  { head->x[i] += x[(k+1)%3][i] - side[k][0][i]; 
                     head->normal[i] += side[k][0][i]; 
                  }
                  head->status = 2;
                  head->area = 2.0;
                  head->triples++;
                  head->trip_e[1] = e_id;
                }
             }
             else
                if ( !equal_element(e_id,head->trip_e[0]) &&
                      !equal_element(e_id,head->trip_e[1]) )
                { head->status = -10; /* don't move */
                  head->triples = 3;
                }

             if ( tail->status < 0 ) {}
             else if ( tail->triples == 0 )
             { /* first detection */
                for ( i = 0 ; i < SDIM ; i++ ) 
                { tail->x[i] = x[k][i] + side[k][0][i]; 
                  tail->normal[i] = side[k][0][i]; 
                }
                tail->status = 1;
                tail->area = 1.0;
                tail->triples++;
                tail->trip_e[0] = e_id;
             }
             else if ( tail->triples == 1 )
             { /* second detection */
                if ( !equal_element(e_id,tail->trip_e[0]) )
                { for ( i = 0 ; i < SDIM ; i++ ) 
                  { tail->x[i] += x[k][i] + side[k][0][i]; 
                    tail->normal[i] -= side[k][0][i]; 
                  }
                  tail->status = 2;
                  tail->area = 2.0;
                  tail->triples++;
                  tail->trip_e[1] = e_id;
                }
             }
             else
                if ( !equal_element(e_id,tail->trip_e[0]) &&
                      !equal_element(e_id,tail->trip_e[1]) )
                { tail->status = -10; /* don't move */
                  tail->triples = 3;
                }
          }
      if ( tail->triples > 0 ) continue;

      if ( (tail->status >= 0) || (mode == RAWEST) )
         {
            /* make sure signs consistent around vertex */
            facetedge_id vfe = get_vertex_fe(tailv);  /* reference orientation */
            facetedge_id thisfe = fe_id;

            for(;;) 
             { if ( equal_id(vfe,thisfe) ) { sign = 1; break; }
                thisfe = get_next_facet(thisfe);
                if ( equal_id(vfe,thisfe) ) { sign = -1; break; }
                thisfe = inverse_id(get_prev_edge(thisfe));
                if ( equal_id(fe_id,thisfe) )
                  { sign = 0; 
                     goto afterlost; /* lost */ 
                  } 
             } 
                
            tail->area += area;
            for ( i = 0 ; i < SDIM ; i++ )
             { 
                tail->normal[i] += sign*normal[i];
                tail->x[i] += (x[k][i]+(side[k][0][i]+side[(k+2)%3][1][i])/3)*area;
             }
            tail->status++;
         }

afterlost: ;
    }
}
 

/***********************************************************************
*
* function: edge_average()
*
* purpose: find one edge's contributions to averaging of its vertices.
*
*/      

void edge_average(e_id,mode)
edge_id e_id;
int mode; /* averaging mode */
{
  int i,j,k;
  struct averages *head,*tail;
  vertex_id headv = get_edge_headv(e_id);
  vertex_id tailv = get_edge_tailv(e_id);
  REAL side[MAXCOORD];
  int attr;

  get_edge_side(e_id,side);  /* in case of wraps */

  /* head */
  head = average + loc_ordinal(headv);
  attr = get_vattr(headv);
  if ( attr & FIXED ) goto dotail;
  if (  mode != RAWEST )
  { if ( attr & BOUNDARY )
     { if ( !get_eattr(e_id) & BOUNDARY ) goto dotail;
        if ( get_boundary(headv) != get_edge_boundary(e_id) )
          goto dotail;
     }
     /* check constraint compatibility */
     if ( (attr & CONSTRAINT) )
          { conmap_t * vconmap = get_v_constraint_map(headv);
             conmap_t * econmap = get_e_constraint_map(e_id);
             int found;

             for ( j = 1 ; j <= (int)vconmap[0] ; j++ )
             { for ( k = 1, found = 0 ; k <= (int)econmap[0] ; k++ )
                  if ( econmap[k] == (vconmap[j] & ~CON_HIT_BIT) )
                  { found = 1; break; }
                if ( !found ) goto dotail;
             }
          }
  }
  for ( i = 0 ; i < SDIM ; i++ )
     head->x[i] -= side[i];
    
  if ( head->status )
     for ( i = 0 ; i < SDIM ; i++ )
        head->normal[i] += side[i];
  else
     for ( i = 0 ; i < SDIM ; i++ )
        head->normal[i] -= side[i];
     
  head->status++;  /* mark one more edge done */

dotail:
  tail = average + loc_ordinal(tailv);

  attr = get_vattr(tailv);
  if ( attr & FIXED ) return;
  if (  mode != RAWEST )
  { if ( attr & BOUNDARY )
     { if ( !get_eattr(e_id) & BOUNDARY ) return;
        if ( get_boundary(tailv) != get_edge_boundary(e_id) )
          return;
     }
     /* check constraint compatibility */
     if ( (attr & CONSTRAINT) )
          { conmap_t * vconmap = get_v_constraint_map(tailv);
             conmap_t * econmap = get_e_constraint_map(e_id);
             int found;

             for ( j = 1 ; j <= (int)vconmap[0] ; j++ )
             { for ( k = 1, found = 0 ; k <= (int)econmap[0] ; k++ )
                  if ( econmap[k] == (vconmap[j] & ~CON_HIT_BIT) )
                  { found = 1; break; }
                if ( !found ) return;
             }
          }
  }


  for ( i = 0 ; i < SDIM ; i++ )
     tail->x[i] += side[i];
    
  if ( tail->status )
     for ( i = 0 ; i < SDIM ; i++ )
        tail->normal[i] -= side[i];
  else
     for ( i = 0 ; i < SDIM ; i++ )
        tail->normal[i] += side[i];
     
  tail->status++;  /* mark one more edge done */
     
  
}

/***********************************************************************
*
*  function: simplex_facet_average()
*
*  purpose:  add neighboring vertex coordinates for one facet
*
*/

void simplex_facet_average(f_id,mode)
facet_id f_id;
int mode;
{
  struct averages *head;
  vertex_id *v = get_facet_vertices(f_id);
  int i,j,n;
  unsigned int k;

  for ( i = 0 ; i <= web.dimension ; i++ )
  { REAL *x = get_coord(v[i]);

    for ( j = 0 ; j <= web.dimension ; j++ )
    { head = average + loc_ordinal(v[j]);
      if ( mode != RAWEST )
      { conmap_t *conmapj = get_v_constraint_map(v[j]);
        for ( k = 1 ; k <= conmapj[0] ; k++ )
          if ( !v_on_constraint(v[i],conmapj[k]) ) break;
        if ( k <= conmapj[0] ) continue;
      }
      for ( n = 0 ; n < SDIM ; n++ )
      head->x[n] += x[n];
      head->area += 1.0;
      head->status++;
    }
  }
}

/***********************************************************************
*
* function: new_vertex_average()
*
* purpose: move vertex to average position of its neighbors
*          Called via the vertex_average command.
*
* return: 0 if vertex not moved,
*            1 if vertex moved.
*/

int new_vertex_average(v_id,mode)
vertex_id v_id;
int mode;    /* VOLKEEP to keep volumes on both sides same */
             /* NOVOLKEEP to obey just constraints */
             /* RAWEST to ignore all restrictions */
{ REAL newx[MAXCOORD];
  REAL *x;
  int i;
  int attr = get_vattr(v_id);
  int retval;

  retval = find_vertex_average(v_id,newx,mode);
  if ( retval == 0 ) return 0;
  x = get_coord(v_id);
  if (attr & BOUNDARY )
  { /* update boundary parameter to agree with coordinates */
    b_extrapolate(get_boundary(v_id),x,newx,newx,get_param(v_id),
     get_param(v_id),v_id);
  }
  else
  { for ( i = 0 ; i < SDIM ; i++ ) x[i] = newx[i];
      if (attr & CONSTRAINT )
        project_v_constr(v_id,ACTUAL_MOVE,RESET_ONESIDEDNESS);
  }
  return 1;
}

/***********************************************************************
*
* function: find_vertex_average()
*
* purpose: find average position for vertex, but do not move it.
*
* return: 0 if vertex not moved, position is old position.
*            1 if vertex moved, position is new position.
*/

int find_vertex_average(v_id,vx,mode)
vertex_id v_id;
REAL *vx;
int mode;    /* VOLKEEP to keep volumes on both sides same */
             /* NOVOLKEEP to obey just constraints */
             /* RAWEST to ignore all restrictions */
{ int attr = get_vattr(v_id);
  REAL xsum[MAXCOORD];  
  REAL *oldx;
  REAL x[MAXCOORD];
  edge_id e_id,start_e;
  int i,j,k,n;
  REAL weight,total_weight;
  MAT2D(norm,MAXCOORD,MAXCOORD);
  int dm;
  int triple_flag = 0; /* whether any triple (or more) edges found */
  int single_flag = 0; /* whether any single edges found */


  oldx = get_coord(v_id);
  for ( i = 0 ; i < SDIM ; i++ ) vx[i] = oldx[i];

  if ( web.modeltype == LAGRANGE )
     return lagrange_vertex_average(v_id,vx);

  if ( attr & (FIXED|AXIAL_POINT) ) return 0;

  for ( i = 0 ; i < SDIM ; i++ ) xsum[i] = 0.0;

  if ( attr & Q_MIDPOINT )
  { /* quadratic model midpoint of edge */
    facetedge_id fe;
    facet_id f_id;
    REAL *xt,side[MAXCOORD],coeff;
    e_id = get_vertex_edge(v_id);
    xt = get_coord(get_edge_tailv(e_id));
    get_edge_side(e_id,side);
    fe = get_edge_fe(e_id);
    /* for soapfilm, keep height above midpoint */
    if ( (web.representation == SOAPFILM) && valid_id(fe) )
    { f_id = get_fe_facet(fe);
      if ( valid_id(f_id) )
      { get_facet_normal(f_id,norm[0]); 
        coeff = (SDIM_dot(vx,norm[0])-SDIM_dot(xt,norm[0]))/
                      SDIM_dot(norm[0],norm[0]);
        for ( i = 0 ; i < SDIM ; i++ ) 
           vx[i] = xt[i] + 0.5*side[i] + coeff*norm[0][i];
        goto new_avg_exit;
      }
    }
    /* if didn't work, just center midpoints a little */
    coeff = 0.5 - (SDIM_dot(vx,side)-SDIM_dot(xt,side))/
                     SDIM_dot(side,side);
    for ( i = 0 ; i < SDIM ; i++ ) vx[i] += coeff*side[i];
  }
  else if ( attr & Q_MIDFACET )
  { /* Lagrange model middle of facet */
     return lagrange_vertex_average(v_id,vx);
  }
  else if ( attr & Q_MIDEDGE )
  { /* Lagrange model middle of edge */
     return lagrange_vertex_average(v_id,vx);
  }
  else
  { /* corner vertex, average with neighbors */
    e_id = start_e = get_vertex_edge(v_id);
    if ( !valid_id(e_id) ) return 0;
    k = 0; total_weight = 0.0;
    /* get weighted sum of neighbors */
    do
    { facetedge_id fe,start_fe;

      if ( mode != RAWEST )
      { /* check constraint compatibility */
         int m;

         if ( (attr & CONSTRAINT) )
          { conmap_t * vconmap = get_v_constraint_map(v_id);
             conmap_t * econmap = get_e_constraint_map(e_id);

             for ( j = 1 ; j <= (int)vconmap[0] ; j++ )
             { int found;
                for ( m = 1, found = 0 ; m <= (int)econmap[0] ; m++ )
                  if ( econmap[m] == (vconmap[j] & ~CON_HIT_BIT) )
                  { found = 1; break; }
                if ( !found ) goto loopend;
             }
          }
         else if ( attr & BOUNDARY )
         { 
            if ( get_edge_boundary(e_id) != get_boundary(v_id) ) goto loopend;
         } 
      }

      /* check valence */
      if ( (mode != RAWEST) && (web.representation == SOAPFILM) )
      { fe = get_edge_fe(e_id);
         if ( equal_id(fe,get_next_facet(fe)) )
         { /* edge edge */
           if ( !single_flag )
           { /* first, so reset */
              total_weight = 0.0;
              k = 0;
              for ( i = 0 ; i < SDIM ; i++ ) xsum[i] = 0.0;
            }
            single_flag++;
          }
         else if ( !equal_id(get_next_facet(fe),get_prev_facet(fe)) )
         { /* triple, at least */
            if ( !triple_flag )
            { /* first, so reset */
              total_weight = 0.0;
              k = 0;
              for ( i = 0 ; i < SDIM ; i++ ) xsum[i] = 0.0;
            }
            triple_flag++;
            if ( triple_flag >= 3 ) return 0; /* too many triple lines */
         }
         else
         { if ( triple_flag ) goto loopend; 
         }
      }

      if ( (web.representation == STRING) || (get_eattr(e_id) & BARE_NAKED)
               || single_flag || triple_flag )
          weight = 1.0;
      else
      { fe = start_fe = get_edge_fe(e_id);
        weight = 0.0;
        if ( valid_id(fe) )
          do { facet_id f_id = get_fe_facet(fe);
		       REAL a = get_facet_area(f_id);
		       if ( a == 0.0 ) 
			   { (*calc_facet_energy)(f_id,AREA_ONLY);
			      a = get_facet_area(f_id);
			   }
		       weight += a;
               fe = get_next_facet(fe);
             } while ( !equal_id(fe,start_fe) );
      }
      if ( weight == 0.0 ) goto loopend;
      get_edge_side(e_id,x);
      for ( i = 0 ; i < SDIM ; i++ ) xsum[i] += weight*weight*x[i];
      total_weight += weight*weight;
      k++;
loopend:
      e_id = get_next_tail_edge(e_id);
    } while ( !equal_id(e_id,start_e) );

    if ( k <= 1 ) return 0;  /* only found at most one edge */

    if ( mode == VOLKEEP )
     { /* project motion tangentially */
        dm = new_calc_vertex_normal(v_id,norm);
        if ( dm < SDIM )
        { dm = gram_schmidt(norm,dm,SDIM);
          for ( n = 0 ; n < dm ; n++ )
          { REAL c = SDIM_dot(xsum,norm[n]);
             for ( i = 0 ; i < SDIM ; i++ ) 
                 xsum[i] -= c*norm[n][i];
          }
        }
        else for ( i = 0 ; i < SDIM ; i++ ) xsum[i] = 0.0; /* triple pt fixed */
     }

     if ( (attr & CONSTRAINT) )
     { unsigned int jj;
       conmap_t * conmap = get_v_constraint_map(v_id);
       int oncount = 0;
       struct constraint *con[MAXCONPER];
       int conlist[MAXCONPER];
       REAL perp[MAXCOORD];

       for ( jj = 1 ; jj <= conmap[0] ; jj++ )
       { if ( conmap[jj] & CON_HIT_BIT )
         { conlist[oncount] = conmap[jj] & CONMASK;
           con[oncount] = get_constraint(conmap[jj]);
           if ( !(con[oncount]->attr & (NONNEGATIVE|NONPOSITIVE) ) )
              oncount++;  /* will do one-sided later */
         }
       }
       if ( oncount )
       { constr_proj(TANGPROJ,oncount,con,vx,
                        xsum,perp,conlist,NO_DETECT,v_id);
         for ( j = 0 ; j < SDIM ; j++ )
           xsum[j] -= perp[j];
       }
     }

     for ( i = 0 ; i < SDIM ; i++ )
         vx[i] += 0.25*xsum[i]/total_weight;
  }

new_avg_exit:

  return 1;
}

/******************************************************************************
 * 
 * function: lagrange_vertex_average()
 *
 * purpose: Do vertex averaging for a vertex in the Lagrange model.
 */

int lagrange_vertex_average(v_id,newx)
vertex_id v_id;
REAL *newx;
{ int attr = get_vattr(v_id);
  int i,j,k;
  
  if ( attr & Q_MIDEDGE )
  { /* average with neighbors on edge */
    edge_id e_id = get_vertex_edge(v_id);
    vertex_id *v = get_edge_vertices(e_id);
    int knx; /* index of vertex */
    vertex_id vv[2];  /* the two neighbors to average with */
    REAL perturb[MAXCOORD];
    REAL dxdu[MAXCOORD],u,du,rhs,*x;
    struct gauss_lag *g = &gauss_lagrange[1][web.gauss1D_order];
    int ctrl = web.lagrange_order+1;
    REAL aa;
    REAL *xx[MAXLAGRANGE+1];

    /* find which vertex it is in the edge */
    for ( knx = 0 ; knx <= web.lagrange_order ; knx++ )
      if ( equal_id(v_id,v[knx]) )
        goto edge_found_knx;
    kb_error(3789,"Internal error:lagrange_vertex_average() couldn't find vertex index in edge.\n",
      RECOVERABLE);
         
 edge_found_knx:
    /* the two neighbors */
    vv[0] = v[knx-1] ;
    vv[1] = v[knx+1];

    /* find perturbation in u,v so can calculate new position exactly on 
       the surface */
    /* first, the xyz perturbation */
    memset((char*)newx,0,SDIM*sizeof(REAL));
    for ( i = 0 ; i < 2 ; i++ )
    { REAL *xx = get_coord(vv[i]);
      for ( j = 0 ; j < SDIM ; j++ )
        newx[j] += xx[j];
    }
    x = get_coord(v_id);
    for ( j = 0 ; j < SDIM ; j++ )
      perturb[j] = newx[j]/2 - x[j];

    /* get partials matrix */
    for(j=0;j<SDIM;j++) dxdu[j] = 0.0;
    for ( k = 0 ; k < ctrl ; k++ )
    { xx[k] = get_coord(v[k]);
      for ( j = 0 ; j < SDIM ; j++ )
      { dxdu[j] += g->lpolypart[knx][0][k]*xx[k][j];
      }
    }
    rhs = dot(perturb,dxdu,SDIM);
    aa =  dot(dxdu,dxdu,SDIM);
    du = rhs/aa;
    u = (knx + du)/web.lagrange_order;
    lagrange_eval_1d(web.lagrange_order,SDIM,u,xx,newx);
    /* newx holds return value */
     
    return 1;
  }
  else if ( attr & Q_MIDFACET )
  { facet_id f_id = get_vertex_facet(v_id);
    vertex_id *v = get_facet_vertices(f_id);
    int inx=0,jnx,knx; /* col, row, index of vertex */
    vertex_id vv[6];  /* the six neighbors to average with */
    MAT2D(dxdu,MAXCOORD,2);
    REAL *xx[(MAXLAGRANGE+1)*(MAXLAGRANGE+2)/2];
    MAT2D(aa,2,2);
    REAL perturb[MAXCOORD], *p=perturb;
    REAL u[2],du[2],rhs[2],*r=rhs,*x;
    struct gauss_lag *g = &gauss_lagrange[2][web.gauss2D_order];
    int ctrl = (web.lagrange_order+1)*(web.lagrange_order+2)/2;

    /* find which vertex it is in the facet */
    for ( jnx = 0, knx = 0 ; jnx <= web.lagrange_order ; jnx++ )
      for ( inx = 0 ; inx+jnx <= web.lagrange_order ; inx++,knx++ )
        if ( equal_id(v_id,v[knx]) )
          goto found_knx;
    kb_error(3788,"Internal error:lagrange_vertex_average() couldn't find vertex index in facet.\n",
      RECOVERABLE);
         
 found_knx:
    /* the six neighbors */
    vv[0] = v[knx - web.lagrange_order + jnx - 2];
    vv[1] = v[knx - web.lagrange_order + jnx - 1];
    vv[2] = v[knx - 1];
    vv[3] = v[knx + 1];
    vv[4] = v[knx + web.lagrange_order - jnx];
    vv[5] = v[knx + web.lagrange_order - jnx + 1];

    /* find perturbation in u,v so can calculate new position exactly on 
       the surface */
    /* first, the xyz perturbation */
    memset((char*)newx,0,SDIM*sizeof(REAL));
    for ( i = 0 ; i < 6 ; i++ )
    { REAL *xx = get_coord(vv[i]);
      for ( j = 0 ; j < SDIM ; j++ )
        newx[j] += xx[j];
    }
    x = get_coord(v_id);
    for ( j = 0 ; j < SDIM ; j++ )
      perturb[j] = newx[j]/6 - x[j];

    /* get partials matrix */
    for(j=0;j<SDIM;j++) dxdu[j][0] = dxdu[j][1] = 0.0;
    for ( k = 0 ; k < ctrl ; k++ )
    { xx[k] = get_coord(v[k]);
      for ( j = 0 ; j < SDIM ; j++ )
      { dxdu[j][0] += g->lpolypart[knx][0][k]*xx[k][j];
        dxdu[j][1] += g->lpolypart[knx][1][k]*xx[k][j];
      }
    }
    /* kludging due to way lpolypart defined in terms of barycentric */
    for ( j = 0  ; j < SDIM  ; j++ )
    { dxdu[j][0] -= dxdu[j][1];
      dxdu[j][1] += dxdu[j][0];
    }
    /* Least squares solve for du */
    mat_mult(&p,dxdu,&r,1,SDIM,2);
    tr_mat_mul(dxdu,dxdu,aa,SDIM,2,2);
    mat_inv(aa,2);
    matvec_mul(aa,rhs,du,2,2);
    u[0] = (inx + du[0])/web.lagrange_order;
    u[1] = (jnx + du[1])/web.lagrange_order;
    lagrange_eval_2d(web.lagrange_order,SDIM,u,xx,newx);
    /* newx holds return value */
     
    return 1;
  }
  else /* corner */
  { /* not doing anything yet */
    return 0;
  }

}
