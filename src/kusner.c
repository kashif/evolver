/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/**********************************************************************
*
*  File: kusner.c
*
*  Purpose: Does calculations needed for including square curvature
*              in energy. Linear model only.
*              Rob Kusner version - curvature energy on edges
*/

#include "include.h"

/********************************************************************
*
*  Function: kusner_energy()
*
*  Purpose:  Does square curvature energy calculation for an edge.
*                Edge curvature version.
*
*/

void kusner_energy()
{ 
  REAL s1[MAXCOORD],s2[MAXCOORD],t2[MAXCOORD];
  REAL s1s1,s1s2,s1t2,s2s2,t2t2,s2t2;
  REAL a1,a2;
  REAL det;
  facetedge_id fe_s1,fe_s2,fe_t2;
  edge_id e_id;
  REAL cos_th;    /* cosine of angle between facets */
  REAL modulus = 3*globals(square_curvature_param)->value.real;

  FOR_ALL_EDGES(e_id)
    {
      if ( get_attr(e_id) & FIXED ) continue;

      /* get edge vectors away from tail vertex */
      fe_s1 = get_edge_fe(e_id);
      fe_s2 = get_prev_edge(get_next_facet(fe_s1));
      fe_s2 = inverse_id(fe_s2);
      fe_t2 = get_prev_edge(fe_s1);
      fe_t2 = inverse_id(fe_t2);
      get_fe_side(fe_s1,s1);
      get_fe_side(fe_s2,s2);
      get_fe_side(fe_t2,t2);

      s1s1 = SDIM_dot(s1,s1);
      s1s2 = SDIM_dot(s1,s2);
      s1t2 = SDIM_dot(s1,t2);
      t2t2 = SDIM_dot(t2,t2);
      s2s2 = SDIM_dot(s2,s2);
      s2t2 = SDIM_dot(s2,t2);

      det = s1s1*t2t2 - s1t2*s1t2;
      a1 = sqrt(det);
      det = s1s1*s2s2 - s1s2*s1s2;
      a2 = sqrt(det);

      cos_th = (s1s2*s1t2 - s2t2*s1s1)/a1/a2;

      binary_tree_add(web.total_energy_addends,
                     modulus*s1s1*(1 - cos_th)/(a1 + a2));
    }
}


/************************************************************************
*
*  Function: kusner_force()
*
*  Purpose:  Does square curvature force calculation.
*
*/

void kusner_force()
{ 
  REAL s1[MAXCOORD],s2[MAXCOORD],t2[MAXCOORD];
  REAL s1s1,s1s2,s1t2,s2s2,t2t2,s2t2;
  REAL a1,a2;
  REAL det;
  facetedge_id fe_s1,fe_s2,fe_t2;
  edge_id e_id;
  REAL cos_th;    /* cosine of angle between facets */
  REAL *of,*s1f,*s2f,*t2f;  /* vertex force pointers */
  REAL dcosds1[MAXCOORD],dcosds2[MAXCOORD],dcosdt2[MAXCOORD];
  REAL da1ds1[MAXCOORD], da1dt2[MAXCOORD];
  REAL da2ds1[MAXCOORD], da2ds2[MAXCOORD];
  int i;
  REAL modulus = 3*globals(square_curvature_param)->value.real;

  FOR_ALL_EDGES(e_id)
    {
      if ( get_attr(e_id) & FIXED ) continue;

      /* get edge vectors away from tail vertex */
      fe_s1 = get_edge_fe(e_id);
      fe_s2 = get_prev_edge(get_next_facet(fe_s1));
      fe_s2 = inverse_id(fe_s2);
      fe_t2 = get_prev_edge(fe_s1);
      fe_t2 = inverse_id(fe_t2);
      get_fe_side(fe_s1,s1);
      get_fe_side(fe_s2,s2);
      get_fe_side(fe_t2,t2);
      of = get_force(get_edge_tailv(e_id));
      s1f = get_force(get_edge_headv(e_id));
      s2f = get_force(get_fe_headv(fe_s2));
      t2f = get_force(get_fe_headv(fe_t2));


      s1s1 = SDIM_dot(s1,s1);
      s1s2 = SDIM_dot(s1,s2);
      s1t2 = SDIM_dot(s1,t2);
      t2t2 = SDIM_dot(t2,t2);
      s2s2 = SDIM_dot(s2,s2);
      s2t2 = SDIM_dot(s2,t2);

      det = s1s1*t2t2 - s1t2*s1t2;
      a1 = sqrt(det);
      det = s1s1*s2s2 - s1s2*s1s2;
      a2 = sqrt(det);

      cos_th = (s1s2*s1t2 - s2t2*s1s1)/a1/a2;

      /* gradients of various terms */
      for ( i = 0 ; i < SDIM ; i++ )
         { da1ds1[i] = 0.5/a1*(2*s1[i]*t2t2 - 2*s1t2*t2[i]);
            da1dt2[i] = 0.5/a1*(2*s1s1*t2[i] - 2*s1t2*s1[i]);
            da2ds1[i] = 0.5/a2*(2*s1[i]*s2s2 - 2*s1s2*s2[i]);
            da2ds2[i] = 0.5/a2*(2*s1s1*s2[i] - 2*s1s2*s1[i]);
            dcosds1[i] = (s2[i]*s1t2 + s1s2*t2[i] - 2*s2t2*s1[i])/a1/a2
                            - cos_th/a1*da1ds1[i] - cos_th/a2*da2ds1[i];
            dcosdt2[i] = (s1s2*s1[i] - s2[i]*s1s1)/a1/a2
                            - cos_th/a1*da1dt2[i];
            dcosds2[i] = (s1[i]*s1t2 - t2[i]*s1s1)/a1/a2
                            - cos_th/a2*da2ds2[i];
         }

      for ( i = 0 ; i < SDIM ; i++ )
         { REAL f;  /* part of force */

            f = modulus*2*s1[i]*(1 - cos_th)/(a1 + a2);
            s1f[i] -= f;
            of[i]  += f;

            f = -modulus*s1s1*dcosds1[i]/(a1 + a2);
            s1f[i] -= f;
            of[i]  += f;

            f = -modulus*s1s1*(1 - cos_th)/(a1 + a2)/(a1 + a2)*da1ds1[i];
            s1f[i] -= f;
            of[i]  += f;

            f = -modulus*s1s1*(1 - cos_th)/(a1 + a2)/(a1 + a2)*da2ds1[i];
            s1f[i] -= f;
            of[i]  += f;

            f = -modulus*s1s1*dcosds2[i]/(a1 + a2);
            s2f[i] -= f;
            of[i]  += f;

            f = -modulus*s1s1*(1 - cos_th)/(a1 + a2)/(a1 + a2)*da2ds2[i];
            s2f[i] -= f;
            of[i]  += f;

            f = -modulus*s1s1*dcosdt2[i]/(a1 + a2);
            t2f[i] -= f;
            of[i]  += f;

            f = -modulus*s1s1*(1 - cos_th)/(a1 + a2)/(a1 + a2)*da1dt2[i];
            t2f[i] -= f;
            of[i]  += f;
         }
    }
}

/**************************************************************************
*
* function:  approx_curvature()
*
* purpose: calculate approximate curvature according to Dziuk and Schmidt.
*             Their definition is that linear interpolation of approx curvature
*             integrated with linear interpolation of velocity gives rate of 
*             area change.  So first need regular vertex force, and then
*             can solve symmetric system for approximate curvatures.
*             Coefficient matrix has star areas times 1/6 along diagonal and difacet
*             areas off diagonal times 1/12
*/


/*  C declarations of the YSMP routines  */
#include "f2c.h"

int odrv_ ARGS(( integer *, integer *,integer *,REAL *, integer *,integer *,
integer *,integer *, integer *, integer *));

int sdrvmd_ ARGS(( integer *, integer *,integer *, integer *,integer *,REAL *,
          REAL *,REAL *, integer *,integer *,REAL *,integer *,
          integer *, integer *, REAL *));
                                                     
void sdrv_flag_check ARGS((integer , integer , integer ));
void odrv_flag_check ARGS((integer , integer ));

void approx_curvature()
{
  integer *IA, *JA, *P, *IP, NSP, *ISP;
  doublereal *A;
  integer i,j, PATH, FLAG;
  integer count;
  doublereal *B;
  REAL *RSP;
  integer ESP;
  REAL EMAX;
  integer N = web.skel[VERTEX].max_ord+1;
  int Total_entries = web.skel[VERTEX].count + web.skel[EDGE].count;
  edge_id e_id;

  /* allocate storage for arrays to be passed to ysmp */
  IA = (integer *)temp_calloc(N+1,sizeof(integer));
  JA = (integer *)temp_calloc(Total_entries,sizeof(integer));
  A = (REAL *)temp_calloc(Total_entries,sizeof(REAL));
  P = (integer *)temp_calloc(N,sizeof(integer));
  IP = (integer *)temp_calloc(N,sizeof(integer));
  NSP = 8*N + 16*Total_entries;
  ISP = (integer *)temp_calloc(NSP,sizeof(integer));
  B  = (REAL *)temp_calloc(N,sizeof(REAL));  

  /* count entries needed for each row */
  FOR_ALL_EDGES(e_id)
    {
      int tail = loc_ordinal(get_edge_tailv(e_id));
      int head = loc_ordinal(get_edge_headv(e_id));
      if ( tail > head ) IA[head]++;
      else IA[tail]++;
    }
  /* set up IA pointers */
  count = 0;
  for ( i = 0 ; i < N ; i++ )
    { int temp = IA[i] + 1;  /* include diagonal element */
      IA[i] = count + 1;  /* FORTRAN indexing */
      count += temp;
      JA[IA[i]-1] = i+1;  /* diagonal */
    }
  IA[N] = count + 1;

  /* set up JA column index list for off diagonal */
  /* and fill in facet star areas */
  FOR_ALL_EDGES(e_id)
    {
      int tail = loc_ordinal(get_edge_tailv(e_id));
      int head = loc_ordinal(get_edge_headv(e_id));
      facetedge_id fe = get_edge_fe(e_id);
      REAL area1 = get_facet_area(get_fe_facet(fe));
      REAL area2;
      int base,addend;
      fe = get_next_facet(fe);
      area2 = get_facet_area(get_fe_facet(fe));
      
      /* add to vertex stars */
      A[IA[tail]-1] += (area1 + area2)/12;  /* each area will be added twice */
      A[IA[head]-1] += (area1 + area2)/12;  /* each area will be added twice */

      if ( tail > head ) { base = head; addend = tail; }
      else { base = tail; addend = head; }
      /* seek column, add if not already there */
      for ( j = IA[base]-1 ; j < IA[base+1]-1 ; j++ )
         if ( (JA[j] == addend+1) || (JA[j] == 0) )
             { JA[j] = addend + 1; /* in case first time */
                A[j] += (area1 + area2)/12; 
                break;
             }
      if ( j == IA[base+1]-1 )
         kb_error(1650,"Internal error: approx_curvature: cannot find edge in JA list.\n",RECOVERABLE);

    }
      

#ifdef GDEBUG
/* some debug printing */
{ REAL x;
printf("IA: ");
for ( i = 0 ; i <= N ; i++ ) printf(" %d",IA[i]);
printf("\nJA: ");
for ( i = 0 ; i < count ; i++ ) printf(" %d",JA[i]);
printf("\n");
for ( i = 0 ; i < N ; i++ ) 
  { int j,k,m;
     for ( m = 0 ; m < i ; m++ ) printf("            ");
     for ( m = i,  j = 0, k = IA[i]-1 ; m < N /* j < IA[i+1]-IA[i] */; m++ )
        if ( (m == JA[k]-1) && (k < IA[i+1]-1) )
          { printf(" %f",(DOUBLE)A[k]); k++; j++; }
        else printf(" %f",0.0);
     printf("\n");
  }
}
#endif

  /*  Call ODRV to perform the reordering on A */
        PATH = 2;          
        odrv_( &N, IA,JA,A, P,IP, &NSP,ISP, &PATH, &FLAG );
        odrv_flag_check(FLAG,N);

  /*  Call SDRVMD to compute the solution */
       RSP = (REAL *) ISP;
       PATH = 5; /* SSF and SNF only */
       sdrvmd_(&N,P,IP,IA,JA,A,B,B,&NSP,ISP,RSP,&ESP, &PATH,&FLAG,&EMAX);
       sdrv_flag_check(ESP,FLAG,N);

  /* each coordinate gives a right side */
  for ( j = 0 ; j < SDIM ; j++ )
     { vertex_id v_id;
        FOR_ALL_VERTICES(v_id)
          { 
             int vnum = loc_ordinal(v_id);
             B[vnum] = get_force(v_id)[j];
          }
        RSP = (REAL *) ISP;
        PATH = 3; /* SNS */
        sdrvmd_(&N,P,IP,IA,JA,A,B,B,&NSP,ISP,RSP,&ESP, &PATH,&FLAG,&EMAX);
        sdrv_flag_check(ESP,FLAG,N);
        FOR_ALL_VERTICES(v_id)
          { 
             int vnum = loc_ordinal(v_id);
             get_force(v_id)[j] = B[vnum];
          }
     }


  /* free stuff */
  temp_free((char *)IA);
  temp_free((char *)A);
  temp_free((char *)JA);
  temp_free((char *)B);
  temp_free((char *)ISP);
  temp_free((char *)IP);
  temp_free((char *)P);
}

/**************************************************************/

/* general mobility routines */

#define FORT
#ifdef FORT
#define A_OFF 1
#else
#define A_OFF 0
#endif

/* structure for storing constraint and boundary vertex basis */
struct basis { vertex_id v_id;    /* which vertex */
                  REAL vvec[MAXCOORD][MAXCOORD];  /* basis vectors */
                  REAL *vec[MAXCOORD];  /* for matrix routines */
                } *conbasis;
int cb_count; /* how many needed */
struct basis **cb_list;  /* indexed by vertex ordinal */
int *dimf;    /* degrees of freedom of vertices */

static REAL *A;
static int *IA,*JA,*P,*IP,NSP,*ISP;

/*****************************************************************
*
* function: mobility_setup()
*
* purpose:  sets up sparse arrays for mobility matrix for 
*              approximate curvature.  Subsequent calls to 
*              mobility_mult() transform a formfield to a vectorfield.
*/

static  REAL *RSP;
static  integer mobN; 
static  int *IA_INV;

void mobility_setup()
{
  integer i,j,k, PATH, FLAG=0;
  integer count;
  integer ESP;
  REAL EMAX;
  int Total_entries;
  vertex_id v_id;
  edge_id e_id;
  facet_id f_id;
  REAL **proj;  /* normal projection matrix */
  REAL **q,**qq,**qqq; /* basis product matrices */
  struct basis *cb,*cba,*cbb,*cbh,*cbt;
  int dim_a;

  if ( web.modeltype != LINEAR )
     kb_error(2088,"approx_curv only for LINEAR model.\n",RECOVERABLE);
  proj = dmatrix(0,SDIM-1,0,SDIM-1);
  qq = dmatrix(0,SDIM-1,0,SDIM-1);
  qqq = dmatrix(0,SDIM-1,0,SDIM-1);

  /*************************/
  /* set up conbasis first */
  /*************************/

  /* count vertices on constraints and boundaries */
  cb_count = 0;
  FOR_ALL_VERTICES(v_id)
     { 
        if ( get_vattr(v_id) & (BOUNDARY|HIT_WALL)  )
          cb_count++;
     }

  /* allocate space */
  cb_list = (struct basis **)temp_calloc(web.skel[VERTEX].max_ord+1,
                                                  sizeof(struct basis *));
  dimf = (int *)temp_calloc(web.skel[VERTEX].max_ord+1,sizeof(int));
  if ( cb_count )
  conbasis = (struct basis *)temp_calloc(cb_count,sizeof(struct basis));
  for ( j = 0 ; j < cb_count ; j++ )
      for( i = 0 ; i < SDIM ; i++ )
          conbasis[j].vec[i] = conbasis[j].vvec[i];

  /* fill in */
  cb_count = 0;
  mobN = 0;
  FOR_ALL_VERTICES(v_id)
     { int ord = loc_ordinal(v_id);
        if ( get_vattr(v_id) & FIXED ) continue;  /* leave dimf as 0 */
        if ( get_vattr(v_id) & BOUNDARY ) 
          dimf[ord] = bdry_basis(v_id,conbasis[cb_count].vec);
        else if ( get_vattr(v_id) & HIT_WALL )
         dimf[ord] = constr_basis(v_id,conbasis[cb_count].vec);
        else { mobN += SDIM; dimf[ord] = SDIM;  continue; }
        conbasis[cb_count].v_id = v_id;
        mobN += dimf[ord];
        cb_list[loc_ordinal(v_id)] = conbasis + cb_count;
        cb_count++;
     }

  Total_entries = (mobN*(SDIM+1))/2+SDIM*SDIM*web.skel[EDGE].count;
  /* allocate storage for arrays to be passed to ysmp */
  IA = (integer *)temp_calloc(mobN+1,sizeof(integer));
  JA = (integer *)temp_calloc(Total_entries,sizeof(integer));
  A = (REAL *)temp_calloc(Total_entries,sizeof(REAL));
  P = (integer *)temp_calloc(mobN,sizeof(integer));
  IP = (integer *)temp_calloc(mobN,sizeof(integer));
  IA_INV = (int *)temp_calloc(web.skel[VERTEX].max_ord+1,sizeof(integer));

#define EMPTY (-1)
  for ( i = 0 ; i < Total_entries ; i++ ) JA[i] = EMPTY;

  /* count entries needed for each row, row size accum in IA */
  k = 0;
  FOR_ALL_VERTICES(v_id) /* diagonal elements */
     { int ord = loc_ordinal(v_id);
       cb = cb_list[ord];
       IA_INV[ord] = k;
       dim_a = dimf[ord];
       for ( i = 0 ; i < dim_a ; i++ )
         IA[k++] = dim_a - i;
     }
  FOR_ALL_EDGES(e_id)
    { int tord = loc_ordinal(get_edge_tailv(e_id));
      int hord = loc_ordinal(get_edge_headv(e_id));
      int tail = IA_INV[tord];
      int head = IA_INV[hord];
      int dim_h = dimf[hord];
      int dim_t = dimf[tord];
      if ( tail > head ) for ( i = 0 ; i < dim_h ; i++ ) IA[head+i] += dim_t;
      else for ( i = 0 ; i < dim_t ; i++ ) IA[tail+i] += dim_h;
    }
  /* set up IA pointers */
  count = 0;
  FOR_ALL_VERTICES(v_id)
    { int ord = loc_ordinal(v_id);
      int dim = dimf[ord];
      i = IA_INV[ord];
      for ( k = 0 ; k < dim ; k++ )
         {
            int temp = IA[i+k];
            IA[i+k] = count + A_OFF;  /* FORTRAN indexing */
            for ( j = 0 ; j < dim - k ; j++ )
              JA[count + j] = i+k+A_OFF+j;  /* diagonal */
            count += temp;
         }
    }
  IA[mobN] = count + A_OFF;

  /* set up JA column index list for off diagonal */
  /* and fill in star areas */

  if ( web.representation == STRING )
  FOR_ALL_EDGES(e_id)
    { int tord = loc_ordinal(get_edge_tailv(e_id));
      int hord = loc_ordinal(get_edge_headv(e_id));
      int tail = IA_INV[tord];
      int head = IA_INV[hord];
      int base,addend;
      REAL length;
      REAL side[MAXCOORD];
      int orda,ordb;
      
      length = get_edge_length(e_id);
      get_edge_side(e_id,side);

      /* figure normal projection matrix */
      for ( i = 0 ; i < SDIM ; i++ )
         {
          if ( effective_area_flag )
             for ( j = i ; j < SDIM ; j++ )
             { /* not pure projection, to keep pos def */
                 proj[i][j] = - /* 0.99* */ side[i]*side[j]/length; 
              }
            else for ( j = i ; j < SDIM ; j++ ) proj[i][j] = 0.0;
            proj[i][i] += length;  
         }

      /* add to vertex stars */
      cbh = cb_list[hord];
      if ( cbh )
        { for ( i = 0 ; i < dimf[hord] ; i++ )
            for ( j = i ; j < dimf[hord] ; j++ )
                 A[IA[head+i]-A_OFF+(j-i)] += 
                  quadratic_form(cbh->vec[i],proj,cbh->vec[j],SDIM)/3; 
        }
      else
        { for ( i = 0 ; i < dimf[hord] ; i++ )
            for ( j = i ; j < dimf[hord] ; j++ )
                 A[IA[head+i]-A_OFF+(j-i)] += proj[i][j]/3;
        }
      cbt = cb_list[tord];
      if ( cbt )
        { for ( i = 0 ; i < dimf[tord] ; i++ )
            for ( j = i ; j < dimf[tord] ; j++ )
                 A[IA[tail+i]-A_OFF+(j-i)] += 
                  quadratic_form(cbh->vec[i],proj,cbh->vec[j],SDIM)/3; 
        }
      else
        { for ( i = 0 ; i < dimf[tord] ; i++ )
            for ( j = i ; j < dimf[tord] ; j++ )
                 A[IA[tail+i]-A_OFF+(j-i)] += proj[i][j]/3;
        }

      if ( tail > head )
         { base = head; addend = tail; orda = tord; ordb = hord; }
      else
         { base = tail; addend = head; orda = hord; ordb = tord; }

      if ( (cbb = cb_list[ordb]) == NULL )
          q = proj; 
      else 
         { mat_mult(cbb->vec,proj,qq,dimf[ordb],SDIM,SDIM);
          q = qq;
         }
      if ( (cba = cb_list[orda]) != NULL )
         { mat_mul_tr(q,cba->vec,qqq,dimf[ordb],SDIM,dimf[orda]);
          q = qqq;
         }
         
      /* seek column, add if not already there */
      for ( i = 0 ; i < dimf[ordb] ; i++ )
        {
          for ( j = IA[base+i]-A_OFF ; j < IA[base+i+1]-A_OFF ; j++ )
            if ( (JA[j] == addend+A_OFF) || (JA[j] == EMPTY) ) break;
          if ( j == IA[base+i+1]-A_OFF )
            kb_error(1651,"Internal error: approx_curvature: cannot find edge in JA list.\n",RECOVERABLE);


          for ( k = 0 ; k < dimf[orda] ; k++ )
                { JA[j+k] = addend + A_OFF + k; /* in case first time */
                  A[j+k] += q[i][k]/6;
                }
        }
    }

  if ( web.representation == SOAPFILM )
  FOR_ALL_FACETS(f_id)
    {
      REAL area = get_facet_area(f_id);
      REAL s1s1,s2s2,s1s2;  /* dot products */
      facetedge_id fe = get_facet_fe(f_id);
      edge_id e_id1 = get_fe_edge(fe);
      edge_id e_id2 = get_fe_edge(get_next_edge(fe));
      REAL side1[MAXCOORD],side2[MAXCOORD];
      int m;

      /* calc normal projection matrix */
      get_edge_side(e_id1,side1);
      get_edge_side(e_id2,side2);
      s1s1 = SDIM_dot(side1,side1);
      s2s2 = SDIM_dot(side2,side2);
      s1s2 = SDIM_dot(side1,side2);
      for ( i = 0 ; i < SDIM ; i++ )
      { 
         if ( effective_area_flag )
          for ( j = 0 ; j < SDIM ; j++ )
             proj[i][j] = -(s2s2*side1[i]*side1[j] - s1s2*side1[i]*side2[j] 
                - s1s2*side2[i]*side1[j] + s1s1*side2[i]*side2[j])/4/area;
          else
             for ( j = 0 ; j < SDIM ; j++ ) proj[i][j] = 0.0;
         proj[i][i] += area;
      }
      for ( m = 0 ; m < 3 ; m++, fe = get_next_edge(fe) )
        { int tord = loc_ordinal(get_fe_tailv(fe));
          int hord = loc_ordinal(get_fe_headv(fe));
          int tail = IA_INV[tord];
          int head = IA_INV[hord];
          int base,addend;
          int orda,ordb;
      
          /* add to vertex stars */
          cb = cb_list[tord];
          if ( cb )
            { for ( i = 0 ; i < dimf[tord] ; i++ )
                for ( j = i ; j < dimf[tord] ; j++ )
                 A[IA[tail+i]-A_OFF+(j-i)] += 
                  quadratic_form(cb->vec[i],proj,cb->vec[j],SDIM)/6; 
            }
          else
            { for ( i = 0 ; i < dimf[tord] ; i++ )
                for ( j = i ; j < dimf[tord] ; j++ )
                 A[IA[tail+i]-A_OFF+(j-i)] += proj[i][j]/6;
            }

          if ( tail > head )
             { base = head; addend = tail; orda = tord; ordb = hord; }
          else
             { base = tail; addend = head; orda = hord; ordb = tord; }

          if ( (cbb = cb_list[ordb]) == NULL )
              q = proj; 
          else 
             { mat_mult(cbb->vec,proj,qq,dimf[ordb],SDIM,SDIM);
              q = qq;
             }
          if ( (cba = cb_list[orda]) != NULL )
             { mat_mul_tr(q,cba->vec,qqq,dimf[ordb],SDIM,dimf[orda]);
              q = qqq;
             }

          /* seek column, add if not already there */
          if ( dimf[orda] > 0 )
            for ( i = 0 ; i < dimf[ordb] ; i++ )
            {
              for ( j = IA[base+i]-A_OFF ; j < IA[base+i+1]-A_OFF ; j++ )
                if ( (JA[j] == addend+A_OFF) || (JA[j] == EMPTY) ) break;
              if ( j == IA[base+i+1]-A_OFF )
                kb_error(1652,"Internal error: approx_curvature: cannot find edge in JA list.\n",RECOVERABLE);


              for ( k = 0 ; k < dimf[orda] ; k++ )
                    { JA[j+k] = addend + A_OFF + k; /* in case first time */
                      A[j+k] += q[i][k]/12;
                    }
            }
        }
    }

#ifdef GDEBUG
/* some debug printing */
{ REAL x;
printf("IA: ");
for ( i = 0 ; i <= N ; i++ ) printf(" %d",IA[i]);
printf("\nJA: ");
for ( i = 0 ; i < count ; i++ ) printf(" %d",JA[i]);
printf("\n");
for ( i = 0 ; i < N ; i++ ) 
  { int j,k,m;
     for ( m = 0 ; m < i ; m++ ) printf("            ");
     for ( m = i,  j = 0, k = IA[i]-A_OFF ; m < N /* j < IA[i+1]-IA[i] */; m++ )
        if ( (m == JA[k]-A_OFF) && (k < IA[i+1]-A_OFF) )
          { printf(" %8.6g",(DOUBLE)A[k]); k++; j++; }
        else printf(" %8.6g",0.0);
     printf("\n");
  }
}
#endif

#ifdef FORT
  NSP = 8*mobN + 16*Total_entries;
  ISP = (integer *)temp_calloc(NSP,sizeof(integer));
  /*  Call ODRV to perform the reordering on A */
        PATH = 2;          
        odrv_( &mobN, IA,JA,A, P,IP, &NSP,ISP, &PATH, &FLAG );
        odrv_flag_check(FLAG,mobN);

  /*  Call SDRVMD to compute the factorization */
      {  RSP = (REAL *) ISP;
          PATH = 5; /* SSF and SNF only */
          sdrvmd_(&mobN,P,IP,IA,JA,A,NULL,NULL,&NSP,ISP,RSP,&ESP, &PATH,&FLAG,&EMAX);
          sdrv_flag_check(ESP,FLAG,mobN);
      }
#else
  kb_error(5423,"Internal error: Trying abandoned path in mobility_setup().\n",
    RECOVERABLE);
#endif

  free_matrix(proj);
  free_matrix(qq);
  free_matrix(qqq);
} /* end mobility_setup */

/*********************************************************************
*
* function: conbasis_mult()
*
* purpose:  multiply vector (form) by constraint bases of vertices
*
*/

void conbasis_mult ARGS((REAL*,REAL*));
void conbasis_tmult ARGS((REAL*,REAL*));

void conbasis_mult(X,Y)
REAL *X;  /* incoming, SDIM coords per vertex */
REAL *Y;  /* outgoing, degrees of freedom per vertex */
{
  vertex_id v_id;
  int i=0;
  int j;
  int k = 0; /* place in X */
  struct basis *cb;

  FOR_ALL_VERTICES(v_id)
     { int ord = loc_ordinal(v_id);
        int dim = dimf[ord];
        k = ord*SDIM;
        i = IA_INV[ord];
        if ( (cb = cb_list[ord]) != NULL )
         { for ( j = 0 ; j < dim ; j++,i++ )
              Y[i] = SDIM_dot(X+k,cb->vec[j]);
          }
        else /* just copy */
         for ( j = 0 ; j < dim ; j++,i++ )
          Y[i] = X[k+j];
     }
}


/*********************************************************************
*
* function: conbasis_tmult()
*
* purpose:  form linear combos of constraint bases of vertices
*
*/

void conbasis_tmult(X,Y)
REAL *X;  /* incoming, degrees of freedom per vertex */
REAL *Y;  /* outgoing, SDIM coords per vertex */
{
  vertex_id v_id;
  int i;
  int j,m;
  int k = 0; /* place in Y */
  struct basis *cb;

  FOR_ALL_VERTICES(v_id)
     { int ord = loc_ordinal(v_id);
        int dim = dimf[ord];
        k = ord*SDIM;
        i = IA_INV[ord];
        if ( (cb = cb_list[ord]) != NULL )
         { for ( m = 0 ; m < SDIM ; m++ )
              { Y[k+m] = 0.0; 
                for ( j = 0 ; j < dim ; j++ )
                  Y[k+m] += X[i+j]*cb->vec[j][m];
              }
           k += SDIM;
         }
        else /* just copy */
         for ( j = 0 ; j < dim ; j++,i++ )
          Y[k+j] = X[i];
     }
}

/*********************************************************************
*
*  function: mobility_mult()
*
*  purpose:  multiply a formfield by the mobility matrix.
*                actually does just one component at a time.
*                conversion done in place.
*
*/

void mobility_mult(B)
doublereal *B;
{
  integer PATH, FLAG=0;
  integer ESP;
  REAL EMAX;
  REAL *temp = (REAL *)temp_calloc(mobN,sizeof(REAL));

  conbasis_mult(B,temp);

#ifdef FORT
  RSP = (REAL *) ISP;
  PATH = 3; /* SNS */
  sdrvmd_(&mobN,P,IP,IA,JA,A,temp,temp,&NSP,ISP,RSP,&ESP, &PATH,&FLAG,&EMAX);
  sdrv_flag_check(ESP,FLAG,mobN);
#else
  kb_error(5424,"Internal error: Trying abandoned path in mobility_mult().\n",
    RECOVERABLE);
#endif
 
  conbasis_tmult(temp,B);
}

/***********************************************************************
*
* function: mobility_cleanup()
*
*/

void mobility_cleanup()
{
  /* free stuff */
  free_matrix(vgef); vgef = NULL;
  free_matrix(vgev); vgev = NULL;
  temp_free((char *)IA); IA = NULL;
  temp_free((char *)IA_INV); IA_INV = NULL;
  temp_free((char *)A);  A = NULL;
  temp_free((char *)JA); JA = NULL;
  temp_free((char *)IP); IP = NULL;
  temp_free((char *)P);  P = NULL;
#ifdef FORT
  temp_free((char *)ISP);    ISP = NULL;
#else
  temp_free((char *)F);  F = NULL;
  temp_free((char *)IW);  IW = NULL;
#endif
  temp_free((char *)cb_list);  cb_list = NULL;
  if ( conbasis ) { temp_free((char *)conbasis); conbasis = NULL; }
  temp_free((char*)dimf); dimf = NULL;
}

/********************************************************************
*
* function: stability_test()
*
* purpose: find largest eigenvalue of mobility matrix, by starting
*             with random vector and repeatedly applying mobility
*             matrix.
*/

void stability_test()
{
  doublereal *B;
  int i;
  REAL oldmag,newmag;

  mobility_setup();
  B  = (REAL *)temp_calloc(mobN,sizeof(REAL));  
  for ( i = 0 ; i < mobN ; i++ ) B[i] = drand48(); /* random vector */
  oldmag = dot(B,B,mobN);
  for ( i = 0 ; i < 20 ; i++ )
    {
        mobility_mult(B);
        newmag = dot(B,B,mobN);
        printf("%3d. ratio %f \n",i,(DOUBLE)sqrt(newmag/oldmag));
        oldmag = newmag;
    }
  mobility_cleanup();
  temp_free((char *)B);
}


/********************************************************************
*
*  Function: conf_edge_curv_energy()
*
*  Purpose:  Does square curvature energy calculation for an edge.
*                Conformal edge curvature version.  Curvature on edge
*                is curvature of sphere through edge endpoints and
*                the two other vertices on facets on the edge.
*                H^2 only; not (H-H_0)^2.
*
*/

void conf_edge_curv_energy()
{ 
  REAL s1s1,s1s2,s1t2,s2s2,t2t2;
  REAL det;
  REAL a1,a2;
  facetedge_id fe_s1,fe_s2,fe_t2;
  edge_id e_id;
  REAL modulus = globals(square_curvature_param)->value.real;
  REAL **mat = dmatrix(0,SDIM,0,SDIM);
  REAL squares[MAXCOORD]; /* squares of edges */
  REAL r_square;
  REAL center[MAXCOORD];
  int i;

  FOR_ALL_EDGES(e_id)
    {
      if ( get_attr(e_id) & FIXED ) continue;

      /* get edge vectors away from tail vertex */
      fe_s1 = get_edge_fe(e_id);
      fe_s2 = get_prev_edge(get_next_facet(fe_s1));
      fe_s2 = inverse_id(fe_s2);
      fe_t2 = get_prev_edge(fe_s1);
      fe_t2 = inverse_id(fe_t2);
      get_fe_side(fe_s1,mat[0]);
      get_fe_side(fe_s2,mat[1]);
      get_fe_side(fe_t2,mat[2]);

      squares[0] = s1s1 = SDIM_dot(mat[0],mat[0]);
      s1s2 = SDIM_dot(mat[0],mat[1]);
      s1t2 = SDIM_dot(mat[0],mat[2]);
      squares[2] = t2t2 = SDIM_dot(mat[2],mat[2]);
      squares[1] = s2s2 = SDIM_dot(mat[1],mat[1]);

      det = s1s1*t2t2 - s1t2*s1t2;
      a1 = sqrt(det)/2;
      det = s1s1*s2s2 - s1s2*s1s2;
      a2 = sqrt(det)/2;

      det = det_adjoint(mat,SDIM);
      if ( det == 0.0 ) continue;
      matvec_mul(mat,squares,center,SDIM,SDIM);
      for ( i = 0 ; i < SDIM ; i++ )
         center[i] /= 2*det;

      r_square = SDIM_dot(center,center);
      binary_tree_add(web.total_energy_addends,modulus/r_square*(a1+a2)/3);
    }

 free_matrix(mat);
}


/************************************************************************
*
*  Function: conf_edge_curv_force()
*
*  Purpose:  Does square curvature force calculation.
*
*/

void conf_edge_curv_force()
{ 
  REAL s1[MAXCOORD],s2[MAXCOORD],t2[MAXCOORD];
  REAL s1s1,s1s2,s1t2,s2s2,t2t2;
  REAL a1,a2;
  REAL det;
  facetedge_id fe_s1,fe_s2,fe_t2;
  edge_id e_id;
  REAL *of,*s1f,*s2f,*t2f;  /* vertex force pointers */
  int i;
  REAL modulus = globals(square_curvature_param)->value.real;
  REAL squares[MAXCOORD]; /* squares of edges */
  REAL r_square;
  REAL center[MAXCOORD];
  REAL **mat = dmatrix(0,SDIM,0,SDIM);
  REAL coeffs[MAXCOORD];

  FOR_ALL_EDGES(e_id)
    {
      if ( get_attr(e_id) & FIXED ) continue;

      /* get edge vectors away from tail vertex */
      fe_s1 = get_edge_fe(e_id);
      fe_s2 = get_prev_edge(get_next_facet(fe_s1));
      fe_s2 = inverse_id(fe_s2);
      fe_t2 = get_prev_edge(fe_s1);
      fe_t2 = inverse_id(fe_t2);
      get_fe_side(fe_s1,mat[0]);
      get_fe_side(fe_s2,mat[1]);
      get_fe_side(fe_t2,mat[2]);
      get_fe_side(fe_s1,s1);
      get_fe_side(fe_s2,s2);
      get_fe_side(fe_t2,t2);

      squares[0] = s1s1 = SDIM_dot(mat[0],mat[0]);
      s1s2 = SDIM_dot(mat[0],mat[1]);
      s1t2 = SDIM_dot(mat[0],mat[2]);
      squares[2] = t2t2 = SDIM_dot(mat[2],mat[2]);
      squares[1] = s2s2 = SDIM_dot(mat[1],mat[1]);

      of = get_force(get_edge_tailv(e_id));
      s1f = get_force(get_edge_headv(e_id));
      s2f = get_force(get_fe_headv(fe_s2));
      t2f = get_force(get_fe_headv(fe_t2));

      det = s1s1*t2t2 - s1t2*s1t2;
      a1 = sqrt(det)/2;
      det = s1s1*s2s2 - s1s2*s1s2;
      a2 = sqrt(det)/2;

      det = det_adjoint(mat,SDIM);
      if ( det == 0.0 ) continue;  /* flat */
      matvec_mul(mat,squares,center,SDIM,SDIM);
      for ( i = 0 ; i < SDIM ; i++ )
         center[i] /= 2*det; /* to complete inverse of mat and calc of center */

      r_square = SDIM_dot(center,center);

      /* gradients of various terms */
      vec_mat_mul(center,mat,coeffs,SDIM,SDIM);
      for ( i = 0 ; i < SDIM ; i++ )
         coeffs[i] *= 2/det; /* to complete inverse of mat */
      for ( i = 0 ; i < SDIM ; i++ )
         { REAL f;  /* part of force */

            f = -modulus*coeffs[0]*(s1[i]-center[i])*(a1+a2)/3/r_square/r_square;
            s1f[i] -= f;
            of[i]  += f;

            f = -modulus*coeffs[1]*(s2[i]-center[i])*(a1+a2)/3/r_square/r_square;
            s2f[i] -= f;
            of[i]  += f;

            f = -modulus*coeffs[2]*(t2[i]-center[i])*(a1+a2)/3/r_square/r_square;
            t2f[i] -= f;
            of[i]  += f;

            f =  modulus*(s2s2*s1[i] - s1s2*s2[i])/4/a1/r_square/3;
            f += modulus*(t2t2*s1[i] - s1t2*t2[i])/4/a2/r_square/3;
            s1f[i] -= f;
            of[i]  += f;

            f = modulus*(s1s1*s2[i] - s1s2*s1[i])/4/a1/r_square/3;
            s2f[i] -= f;
            of[i]  += f;

            f = modulus*(s1s1*t2[i] - s1t2*s1[i])/4/a2/r_square/3;
            t2f[i] -= f;
            of[i]  += f;
         }
    }
}

