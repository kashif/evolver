/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/*********************************************************************
*
*     file:        simequi2.c
*
*     Contents:  Equiangulation for simplex model.
*                    Local triangulation flipping.
*/

#include "include.h"

/*********************************************************************
*
*  function: simplex_equiangulate()
*
*  purpose: equiangulation for simplex representation.
*
*  return: number of equiangulations done.
*/

struct fstruct { vertex_id v[MAXCOORD];  /* face verts */
                 vertex_id opp[2];         /* opposite verts */
                 facet_id  f_id[2];        /* facets involved */
               } *facelist;

int void_test2 ARGS((struct fstruct *));
int scomp ARGS((vertex_id *,vertex_id *));

int scomp(a,b)
vertex_id *a,*b;
{ int i;
  for ( i = 0 ; i < web.dimension ; i++,a++,b++ )
     if ( *a < *b ) return -1;
     else if ( *a > *b ) return 1;
  return 0; /* equal */
}

int simplex_equiangulate()
{
  int i,j,k;
  facet_id f_id,ff_id;
  struct fstruct *fl,*f1;  /* pointer into face list */
  struct fstruct tetra_tmp;
  int fcount;
  int same;
  int flipcount = 0;
  vertex_id *v;

    /* create face list with pairs of opposite vertices */
    fcount = web.skel[FACET].count*(web.dimension+1);
    facelist = (struct fstruct *)temp_calloc(fcount,sizeof(struct fstruct));
    fl = facelist;
    FOR_ALL_FACETS(f_id)
      {
         int opp; /* which vertex opposite */
         vertex_id *fv = get_facet_vertices(f_id);

         unset_attr(f_id,NEWFACET); /* clear marks */
         for ( opp = 0 ; opp <= web.dimension ; opp++ )
            { int flips = opp;  /* track orientation */
              /* copy face in sorted order vertices */
              for ( i = 0,j = 0 ; i <= web.dimension ; i++ )
                 { if ( i == opp ) continue;
                    k = j;
                    while ( (k > 0) && (fv[i] < fl->v[k-1]) )
                      { fl->v[k] = fl->v[k-1];
                         k--; flips++;
                      }
                    fl->v[k] = fv[i];
                    j++;
                 }
              /* add opposite vertex in appropriate slot */
              fl->opp[flips & 1] = fv[opp];
              fl->f_id[flips & 1] = f_id;
              fl++;
            }
      }

    /* sort face list */
    qsort((char*)facelist,fcount,sizeof(struct fstruct),FCAST scomp);

    /* join pairs of faces */
    /* saving those faces with exactly two neighbors */
    same = 1; /* number of facets face j has so far */
    for ( i = 1, j = 0 ; i < fcount ; i++ )
      { 
         if ( scomp((vertex_id *)(facelist+j),(vertex_id *)(facelist+i)) == 0 )
            { /* same face */
              for ( k = 0 ; k < 2 ; k++ )
                 if ( valid_id(facelist[i].opp[k]) )
                 {  facelist[j].opp[k] = facelist[i].opp[k];
                     facelist[j].f_id[k] = facelist[i].f_id[k];
                 }
              same++;
            }
         else /* new face */
         { if ( same==2 ) j++;
            facelist[j] = facelist[i];
            same = 1;
         }
      }
    fcount = j;  /* number of unique faces */

    /* look for all void-violating opposite vertices */
    for ( fl = facelist, i = 0 ; i < fcount ; i++,fl++ )
      { int retval;
         facet_id newf[MAXCOORD];

         /* see if facets involved are legit */
         if ( !valid_element(fl->f_id[0]) ) continue;
         if ( !valid_element(fl->f_id[1]) ) continue;
         if ( get_fattr(fl->f_id[0]) & (NEWFACET|FIXED) ) continue;
         if ( get_fattr(fl->f_id[1]) & (NEWFACET|FIXED) ) continue;

         if ( !valid_id(fl->f_id[0]) || !valid_id(fl->f_id[1]) )
            { sprintf(msg,"Face doesn't have valid opposite facets.\n");
              outstring(msg);
              continue;
            }

         retval = void_test2(fl);
         switch ( retval )
         { case 0: break;
            case 1: /* inside, want split along central axis */
                  /* check equality of facet constraints and stuff  */
                  /* test for equal density */
                  if ( (get_fattr(fl->f_id[0])&DENSITY) || (get_fattr(fl->f_id[1])&DENSITY) )
                     if ( fabs(get_facet_density(fl->f_id[0]) - get_facet_density(fl->f_id[1])) > 1e-10 )
                         break;
                  /* test for equal constraints */
                  if ( !equal_constr(fl->f_id[0],fl->f_id[1]) ) break;
                  /* test for equal boundary */
                  if (  get_facet_boundary(fl->f_id[0]) != get_facet_boundary(fl->f_id[1]) )
                    break;

                  newf[0] = fl->f_id[0];
                  newf[1] = fl->f_id[1];
                  for ( j = 2; j < web.dimension ; j++ )
                      newf[j] = dup_facet(newf[0]);
                  for ( j = 0 ; j < web.dimension ; j++ )
                  { v = get_facet_vertices(newf[j]);
                     for ( k = 0 ; k < web.dimension-1 ; k++ )
                        v[k] = fl->v[(k+j) % web.dimension];
                     v[k++] = fl->opp[0];
                     v[k] = fl->opp[1];
                     set_attr(newf[j],NEWFACET); /* so others won't try to flip */
                  }
                  flipcount++;
                  break;
            default: /* outside side j, retval = 2 + j */
                  if ( web.dimension != 3 )
                  { kb_error(1875,"Simplex consolidation equiangulation only for surface dimension 3.\n",
                        WARNING);
                     break;
                  }
                  j = retval-2;
                  /* look for third tetra on edge */
                  for ( i = k = 0 ; i < web.dimension ; i++ ) 
                     if ( i != j ) tetra_tmp.v[k++] = fl->v[i];
                  for ( i = web.dimension-1 ; i > 0 ; i-- )
                     if ( fl->opp[0] > tetra_tmp.v[i-1] ) break; 
                     else tetra_tmp.v[i] = tetra_tmp.v[i-1];
                  tetra_tmp.v[i] = fl->opp[0]; 
                  f1 = (struct fstruct *)bsearch((char*)&tetra_tmp,(char*)facelist,fcount,
                                sizeof(struct fstruct), FCAST scomp);
                  if ( f1 == NULL ) break;

                  ff_id = f1->f_id[0];
                  if ( equal_id(ff_id,fl->f_id[0]) ) ff_id = f1->f_id[1];
                  if ( equal_id(ff_id,fl->f_id[1]) ) ff_id = f1->f_id[0];
                  v = get_facet_vertices(ff_id);
                  for ( i = 0 ; i <= web.dimension ; i++ )
                     if ( equal_id(v[i],fl->opp[1]) ) break;
                  if ( i > web.dimension ) break; /* third tetra doesn't fill wedge */

                  /* now have third tetra */
                  if ( !valid_element(ff_id) ) continue;  /* test for contamination */
                  if ( get_fattr(ff_id) & (NEWFACET|FIXED) ) continue;
                  /* check equality of facet constraints and stuff  */
                  /* test for equal density */
                  if ( (get_fattr(fl->f_id[0])&DENSITY) || (get_fattr(fl->f_id[1])&DENSITY)
                     ||  (get_fattr(ff_id)&DENSITY) )
                     if ( (fabs(get_facet_density(fl->f_id[0]) - get_facet_density(fl->f_id[1])) > 1e-10) 
                        || (fabs(get_facet_density(fl->f_id[0]) - get_facet_density(ff_id)) > 1e-10)  )
                         break;
                  /* test for equal constraints */
                  if ( !equal_constr(fl->f_id[0],fl->f_id[1]) ) break;
                  if ( !equal_constr(fl->f_id[0],ff_id) ) break;
                  /* test for equal boundary */
                  if (  get_facet_boundary(fl->f_id[0]) != get_facet_boundary(fl->f_id[1]) )
                    break;
                  if (  get_facet_boundary(fl->f_id[0]) != get_facet_boundary(ff_id) )
                    break;

                  free_element(ff_id);
                  v = get_facet_vertices(fl->f_id[0]);
                  v[0] = fl->v[(j+web.dimension-1) % web.dimension];
                  v[1] = fl->v[j];
                  v[2] = fl->opp[0];
                  v[3] = fl->opp[1];
                  v = get_facet_vertices(fl->f_id[1]);
                  v[0] = fl->v[(j+1) % web.dimension];
                  v[1] = fl->v[j];
                  v[2] = fl->opp[1];
                  v[3] = fl->opp[0];

                  set_attr(fl->f_id[0],NEWFACET); /* mark as contaminated */
                  set_attr(fl->f_id[1],NEWFACET);
                  flipcount++;
                  break;
         }
      }
  if ( flipcount > 0 ) top_timestamp = ++global_timestamp;
  return flipcount;
}


/*******************************************************************
*
*  function: void_test2()
*
*  purpose:  see whether opposite vertices violate void
*
*  return: 0 if not, 
*             1 if yes and connector inside common facet, 
*             2+j if yes and connector outside on side j
*/

int void_test2(fp)
struct fstruct *fp;
{
    REAL *x[MAXCOORD+1];
    REAL ss[MAXCOORD];  /* squares of sides */
    int k,j;
    REAL rr;  /* square radius of void */
    REAL center[MAXCOORD];
    REAL lam[MAXCOORD];
    MAT2D(mat,MAXCOORD,MAXCOORD);
    MAT2D(side,MAXCOORD,MAXCOORD);
    int retval;  /* result to return */
    REAL *y;
    REAL z[MAXCOORD];

    /* first, calculate center of void */
    x[0] = get_coord(fp->opp[0]);
    for ( k = 0 ; k < web.dimension ; k++ )
      {  x[k+1] = get_coord(fp->v[k]);
          for ( j = 0 ; j < SDIM ; j++ )
             side[k][j] = x[k+1][j] - x[0][j];
      }
    for ( k = 0 ; k < web.dimension ; k++ )
      { 
         for ( j = 0 ; j <= k ; j++ )
            mat[j][k] = mat[k][j] = SDIM_dot(side[j],side[k]);
         ss[k] = mat[k][k]/2;
      }
    mat_inv(mat,web.dimension);
    matvec_mul(mat,ss,lam,web.dimension,web.dimension);
    vec_mat_mul(lam,side,center,web.dimension,SDIM);
    rr = dot(center,center,web.dimension);
    for ( k = 0 ; k < SDIM ; k++ )
      center[k] += x[0][k];

    /* now see if other vertex is in the void */
    y = get_coord(fp->opp[1]);
    for ( j = 0 ; j < SDIM ; j++ )
            z[j] = y[j] - center[j];
    if ( SDIM_dot(z,z) < rr - 1e-10 ) retval = 1;
    else retval = 0;

    if ( retval )
    { /* see if connecting segment passes through facet */
      REAL q[MAXCOORD]; /* relative coord of opposite point */
      REAL s[MAXCOORD];
      for ( j = 0 ; j < SDIM ; j++ ) z[j] = y[j] - x[0][j];
      matvec_mul(side,z,s,web.dimension,SDIM);
      matvec_mul(mat,s,q,web.dimension,web.dimension);
      for ( j = 0 ; j < web.dimension ; j++ )
         if ( q[j] < 0.0 ) retval = 2+j;
    }
 return retval;
}

/*****************************************************************************
*
* function: simplex_delaunay_test()
*/

void simplex_delaunay_test()
{
  facet_id f_id;
  
  FOR_ALL_FACETS(f_id)
  {
    vertex_id v_id, *v = get_facet_vertices(f_id);
    v_id = void_test(v,web.dimension);
    if ( valid_id(v_id) )
    { sprintf(msg,"Vertex %s inside void of facet %s\n",ELNAME(v_id),
              ELNAME2(f_id));
      erroutstring(msg);
    }
  }

}


vertex_id void_test(v,dim)
vertex_id *v;
int dim;
{
    REAL *x[MAXCOORD+1];
    REAL ss[MAXCOORD];  /* squares of sides */
    int k,j;
    REAL rr;  /* square radius of void */
    REAL center[MAXCOORD];
    REAL lam[MAXCOORD];
    vertex_id v_id,bad_v = NULLID;
  REAL **mat = dmatrix(0,web.dimension-1,0,web.dimension-1);
  REAL **side = dmatrix(0,SDIM-1,0,SDIM-1);

    /* first, calculate center of void */
    x[0] = get_coord(v[0]);
    for ( k = 1 ; k <= dim ; k++ )
      {  x[k] = get_coord(v[k]);
          for ( j = 0 ; j < SDIM ; j++ )
             side[k-1][j] = x[k][j] - x[0][j];
      }
    for ( k = 0 ; k < dim ; k++ )
      { 
         for ( j = 0 ; j <= k ; j++ )
            mat[j][k] = mat[k][j] = SDIM_dot(side[j],side[k]);
         ss[k] = mat[k][k];
      }
    mat_inv(mat,dim);
    matvec_mul(mat,ss,lam,dim,dim);
    rr = dot(lam,ss,dim)/4;
    vec_mat_mul(lam,side,center,dim,SDIM);
    for ( k = 0 ; k < SDIM ; k++ )
      center[k] = x[0][k] + center[k]/2;

    /* now see if any other vertices are in the void */
    FOR_ALL_VERTICES(v_id)
      { REAL *y;
         REAL z[MAXCOORD];

         for ( k = 0 ; k <= dim ; k++ )
            if ( equal_id(v_id,v[k]) ) break;
         if ( k <= dim ) continue;  /* skip vertices in facet */

         y = get_coord(v_id);
         for ( j = 0 ; j < SDIM ; j++ )
            z[j] = y[j] - center[j];
         if ( SDIM_dot(z,z) >= rr - 1e-10 ) continue;
#if 1        
sprintf(msg,"Void violation by %g\n",(DOUBLE)(-SDIM_dot(z,z) + rr));
erroutstring(msg);
#endif
         bad_v = v_id;
         break;
      }
 free_matrix(side);
 free_matrix(mat);
 return bad_v;
}

