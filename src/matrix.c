/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/* matrix.c */

/* matrix routines, mostly from Numerical Recipes */

/* Note: matrix allocation routine assumes whole matrix can be 
    allocated in one call to calloc.  If not true (say for large
    matrices on an IBM PC), dmatrix() and free_matrix() will
    have to be modified */

/* Note: Matrices allocated as row-pointer structures.
    The -1 index is used to store a private copy of 
    pointer to memory, so users may swap row pointers freely.
*/

#include "include.h"

#ifdef BLAS
/* prototypes */
void DPOTRF ARGS((char*,int*,double*,int*,int*));
void DPOTRS ARGS((char*,int*,int*,double*,int*,double*,int*,int*));
void DGEMM ARGS((char*,char*,int*,int*,int*,double*,
           double*,int*,double*,int*,double*,double*,int*));
void DSYMM ARGS((char*,char*,int*,int*,double*,double*,int*,
          double*,int*,double*,double*,int*));
void DGEMV ARGS((char*,int*,int*,double*,double*,int*,double*,int*,double*,  
              double*,int*));
void DSYMV ARGS((char*,int*,double*,double*,int*,double*,int*,double*,  
              double*,int*));
#endif


/******************************************************************
*
* Function: matcopy()
*
* Purpose:  copies matrix b to matrix a 
*           for zero-base indexing only 
*/

void matcopy ARGS4((a,b,rows,cols),
REAL **a, REAL **b,
int rows, int cols)
{
  int i;

  for ( i = 0 ; i < rows ; i++ )
     memcpy((char *)a[i],(char *)b[i],cols*sizeof(REAL));
}

/******************************************************************
*
* Function: kb_dmatrix()
*
* Purpose: Allocates zeroed 2D matrix in pointer-pointer form,
*          a REAL matrix with range [rlo..rhi][clo..chi] 
*/

#ifdef MEMSTRINGS
REAL **kb_dmatrix ARGS6((rlo,rhi,clo,chi,file,line),
int rlo, int rhi, int clo, int chi,
char *file, int line)
#else
REAL **kb_dmatrix ARGS4((rlo,rhi,clo,chi),
int rlo, int rhi, int clo, int chi)
#endif
{
  int i;
  REAL **m;

#ifdef MEMSTRINGS
  if ( memdebug)
  { sprintf(msg,"dmatrix from %s  %d.\n",file,line);
     outstring(msg);
  }
#endif
  if ( rhi-rlo+1 == 0 ) return NULL;
  m = (REAL **)mycalloc((unsigned)(rhi-rlo+1+1),sizeof(REAL *));
  m -= rlo;
  m++; /* room for private pointer */

  m[rlo-1] = (REAL *) mycalloc((unsigned)(chi-clo+1),(rhi-rlo+1)*sizeof(REAL));
  for ( i = rlo ; i <= rhi ; i++ )
        m[i] = m[rlo-1] + (i - rlo)*(chi - clo + 1) - clo;

  return m;
}

/******************************************************************
*
* Function: kb_dmatrix3()
*
* Purpose: Allocates a zeroed 3D matrix in pointer-pointer-pointer form,
*  a REAL matrix with range [0..n1-1][0..n2-1][0..n3-1] 
*
*/

#ifdef MEMSTRINGS
REAL ***kb_dmatrix3 ARGS5((n1,n2,n3,file,line),
int n1, int n2,int n3,
char * file, int line)
#else
REAL ***kb_dmatrix3 ARGS3((n1,n2,n3),
int n1,int n2,int n3)
#endif
{
  int i,j;
  REAL ***m;

#ifdef MEMSTRINGS
  if ( memdebug )
  { sprintf(msg,"dmatrix3 from %s  %d.\n",file,line);
     outstring(msg);
  }
#endif

  if ( n1 <= 0 ) n1 = 1;
  if ( n2 <= 0 ) n2 = 1;
  /* assumes all pointers same machine size and alignment */
  m = (REAL ***)mycalloc((n2+1)*n1+1,sizeof(REAL **));

  m++; /* room for private pointer to doubles */
  for ( i = 0 ; i < n1 ; i++ )
     m[i] = (REAL **)(m + n1 + i*n2);
  m[0][0] = (REAL *) mycalloc(n1*n2*n3,sizeof(REAL));
  m[-1] = (REAL **)(m[0][0]);
  for ( i = 0 ; i < n1 ; i++ )
     for ( j = 0 ; j < n2 ; j++ )
        m[i][j] = m[0][0] + i*n2*n3 + j*n3;

  return m;
}

/*********************************************************************
*
* function: matrix3_reorder()
*
* purpose: Reorder data of a 3D matrix into canonical order.
*          Creates new matrix, and frees old one.
*
* return: 
*/
REAL *** matrix3_reorder ARGS4((a,maxi,maxj,maxk),
REAL ***a,
int maxi, int maxj, int maxk) /* dimensions */
{ int i,j,k;
  REAL ***newa;
  newa = dmatrix3(maxi,maxj,maxk);
  for ( i = 0 ; i < maxi; i++ )
    for ( j = 0 ; j < maxj ; j++ )
      for ( k = 0 ; k < maxk ; k++ )
        newa[i][j][k] = a[i][j][k];
  free_matrix3(a);
  return newa;
}

/******************************************************************
*
* Function: kb_dmatrix4()
*
* Purpose: Allocates a zeroed 4D matrix in pointer form,
*  a REAL matrix with range [0..n1-1][0..n2-1][0..n3-1][0..n4-1] 
*/

#ifdef MEMSTRINGS
REAL ****kb_dmatrix4 ARGS6((n1,n2,n3,n4,file,line),
int n1, int n2, int n3, int n4,
char * file, int line)
#else
REAL ****kb_dmatrix4 ARGS4((n1,n2,n3,n4),
int n1, int n2, int n3, int n4)
#endif
{
  int i,j,k;
  REAL ****m;

#ifdef MEMSTRINGS
  if ( memdebug )
  { sprintf(msg,"dmatrix4 from %s  %d.\n",file,line);
     outstring(msg);
  }
#endif

  if ( n1 <= 0 ) n1 = 1;
  if ( n2 <= 0 ) n2 = 1;
  if ( n3 <= 0 ) n3 = 1;

  /* assumes all pointers same machine size and alignment */
  m = (REAL ****)mycalloc(1+n1+n1*n2+n1*n2*n3,sizeof(REAL ***));

  m++; /* room for private pointer */
  for ( i = 0 ; i < n1 ; i++ )
     { m[i] = (REAL ***)(m + n1 + i*n2);
        for ( j = 0 ; j < n2 ; j++ )
          m[i][j] = (REAL **)(m + n1 + n1*n2 + i*n2*n3 + j*n3);
     }
  m[0][0][0] = (REAL *) mycalloc(n1*n2*n3*n4,sizeof(REAL));
  m[-1] = (REAL***)(m[0][0][0]);
  for ( i = 0 ; i < n1 ; i++ )
     for ( j = 0 ; j < n2 ; j++ )
        for ( k = 0 ; k < n3 ; k++ )
          m[i][j][k] = m[0][0][0] + i*n2*n3*n4 + j*n3*n4 + k*n4;

  return m;
}


/******************************************************************
*
* Function: kb_temp_dmatrix()
*
* Purpose: Allocates zeroed 2D matrix in pointer-pointer form,
*          a REAL matrix with range [rlo..rhi][clo..chi] 
*          Temporary memory.
*/

#ifdef MEMSTRINGS
REAL **kb_temp_dmatrix ARGS6((rlo,rhi,clo,chi,file,line),
int rlo, int rhi, int clo, int chi,
char *file, int line)
#else
REAL **kb_temp_dmatrix ARGS4((rlo,rhi,clo,chi),
int rlo, int rhi, int clo, int chi)
#endif
{
  int i;
  REAL **m;

#ifdef MEMSTRINGS
  if ( memdebug)
  { sprintf(msg,"temp_dmatrix from %s  %d.\n",file,line);
     outstring(msg);
  }
#endif
  if ( rhi-rlo+1 == 0 ) return NULL;
  m = (REAL **)temp_calloc((unsigned)(rhi-rlo+1+1),sizeof(REAL *));
  m -= rlo;
  m++; /* room for private pointer */

  m[rlo-1] = (REAL *) temp_calloc((unsigned)(chi-clo+1),(rhi-rlo+1)*sizeof(REAL));
  for ( i = rlo ; i <= rhi ; i++ )
        m[i] = m[rlo-1] + (i - rlo)*(chi - clo + 1) - clo;

  return m;
}

/******************************************************************
*
* Function: kb_temp_dmatrix3()
*
* Purpose: Allocates a zeroed 3D matrix in pointer-pointer-pointer form,
*  a REAL matrix with range [0..n1-1][0..n2-1][0..n3-1] 
*  Temporary memory.
*
*/

#ifdef MEMSTRINGS
REAL ***kb_temp_dmatrix3 ARGS5((n1,n2,n3,file,line),
int n1, int n2,int n3,
char * file, int line)
#else
REAL ***kb_temp_dmatrix3 ARGS3((n1,n2,n3),
int n1,int n2,int n3)
#endif
{
  int i,j;
  REAL ***m;

#ifdef MEMSTRINGS
  if ( memdebug )
  { sprintf(msg,"temp_dmatrix3 from %s  %d.\n",file,line);
     outstring(msg);
  }
#endif

  if ( n1 <= 0 ) n1 = 1;
  if ( n2 <= 0 ) n2 = 1;
  /* assumes all pointers same machine size and alignment */
  m = (REAL ***)temp_calloc((n2+1)*n1+1,sizeof(REAL **));

  m++; /* room for private pointer to doubles */
  for ( i = 0 ; i < n1 ; i++ )
     m[i] = (REAL **)(m + n1 + i*n2);
  m[0][0] = (REAL *) temp_calloc(n1*n2*n3,sizeof(REAL));
  m[-1] = (REAL **)(m[0][0]);
  for ( i = 0 ; i < n1 ; i++ )
     for ( j = 0 ; j < n2 ; j++ )
        m[i][j] = m[0][0] + i*n2*n3 + j*n3;

  return m;
}

/******************************************************************
*
* Function: kb_temp_dmatrix4()
*
* Purpose: Allocates a zeroed 4D matrix in pointer form,
*  a REAL matrix with range [0..n1-1][0..n2-1][0..n3-1][0..n4-1] 
*  Temporary memory.
*/

#ifdef MEMSTRINGS
REAL ****kb_temp_dmatrix4 ARGS6((n1,n2,n3,n4,file,line),
int n1, int n2, int n3, int n4,
char * file, int line)
#else
REAL ****kb_temp_dmatrix4 ARGS4((n1,n2,n3,n4),
int n1, int n2, int n3, int n4)
#endif
{
  int i,j,k;
  REAL ****m;

#ifdef MEMSTRINGS
  if ( memdebug )
  { sprintf(msg,"temp_dmatrix4 from %s  %d.\n",file,line);
     outstring(msg);
  }
#endif

  if ( n1 <= 0 ) n1 = 1;
  if ( n2 <= 0 ) n2 = 1;
  if ( n3 <= 0 ) n3 = 1;

  /* assumes all pointers same machine size and alignment */
  m = (REAL ****)temp_calloc(1+n1+n1*n2+n1*n2*n3,sizeof(REAL ***));

  m++; /* room for private pointer */
  for ( i = 0 ; i < n1 ; i++ )
     { m[i] = (REAL ***)(m + n1 + i*n2);
        for ( j = 0 ; j < n2 ; j++ )
          m[i][j] = (REAL **)(m + n1 + n1*n2 + i*n2*n3 + j*n3);
     }
  m[0][0][0] = (REAL *) temp_calloc(n1*n2*n3*n4,sizeof(REAL));
  m[-1] = (REAL***)(m[0][0][0]);
  for ( i = 0 ; i < n1 ; i++ )
     for ( j = 0 ; j < n2 ; j++ )
        for ( k = 0 ; k < n3 ; k++ )
          m[i][j][k] = m[0][0][0] + i*n2*n3*n4 + j*n3*n4 + k*n4;

  return m;
}

/**********************************************************************
*
* Function: perm_matrix2()
*
* purpose:  Allocate pointer-style 2D matrix in permanent memory.
*/
REAL **perm_matrix2(rows,cols)
int rows,cols;
{ REAL **m;
  int i;
  m = (double**)calloc(rows,sizeof(double*));
  m[0] = (double*)calloc(rows*cols,sizeof(double));
  for ( i = 1 ; i < rows ; i++ )
    m[i] = m[i-1] + cols;
  return m;
}
  
/******************************************************************
*
* Function: matNd_setup()
*
* Purpose: routines for initializing matrices declared as local variables 
*          with MAT2D etc macros.  Note it does not zero the entries!
*/

REAL ** mat2d_setup ARGS4((name,spacename,rows,cols),
REAL **name,
REAL *spacename,
int rows, int cols)
{ REAL **spot = name;
  for ( ; rows > 0 ; rows--,spacename += cols,spot++ )
     *spot = spacename;
  return name;
}

REAL *** mat3d_setup ARGS5((name,spacename,rows,cols,levels),
REAL ***name,
REAL *spacename,
int rows, int cols, int levels)
{ int i;
  REAL ***spot;
  REAL **row = (REAL **)(name + rows);
  for ( spot = name ; rows > 0 ; rows--,spot++ )
  { *spot = row;
     for ( i = 0 ; i < cols ; i++,spacename += levels, row++ )
        *row = spacename;
  }
  return name;
}

REAL **** mat4d_setup ARGS6((name,spacename,rows,cols,levels,tiers),
REAL ****name,
REAL *spacename,
int rows, int cols, int levels, int tiers)
{ int i,j;
  REAL ***row = (REAL ***)(name + rows);
  REAL **col = (REAL **)(name + rows + rows*cols);
  REAL ****spot;
  for (spot=name ; rows > 0 ; rows--,spot++ )
  { *spot = row;
     for ( i = 0 ; i < cols ; i++, row++ )
     { *row = col;
        for ( j = 0 ; j < levels ; j++,spacename += tiers, col++ )
          *col = spacename;
     }
  }
  return name;
}
/* end local declaration routines */

/******************************************************************
*
* Function: ivector()
*
* Purpose: allocate integer or real vector with given index range.
*/

int *ivector ARGS2((lo,hi),
int lo, int hi)
/* allocates a int vector with range [lo..hi] */
{
  int *v;

  v = (int *)mycalloc((unsigned)(hi-lo+1),sizeof(int));
  return v-lo;
}

void free_ivector ARGS3((v,lo,hi),
int *v, int lo,int hi)
{
  myfree((char *)(v+lo));
}

/******************************************************************
*
* Function: free_matrixN()
*
* Purpose: Deallocate storage allocated by kb_dmatrixN().
*/

void free_matrix ARGS1((m),
REAL **m)
{
  if ( !m ) return;
  myfree((char *)m[-1]);  /* using private pointer */
  myfree((char *)(m-1));
}

void free_matrix3 ARGS1((m),
REAL ***m)
{
  if ( !m ) return;
  myfree((char *)m[-1]);
  myfree((char *)(m-1));
}

void free_matrix4 ARGS1((m),
REAL ****m)
{
  if ( !m ) return;
  myfree((char *)m[-1]);
  myfree((char *)(m-1));
}


void free_temp_matrix ARGS1((m),
REAL **m)
{
  if ( !m ) return;
  temp_free((char *)m[-1]);  /* using private pointer */
  temp_free((char *)(m-1));
}

void free_temp_matrix3 ARGS1((m),
REAL ***m)
{
  if ( !m ) return;
  temp_free((char *)m[-1]);
  temp_free((char *)(m-1));
}

void free_temp_matrix4 ARGS1((m),
REAL ****m)
{
  if ( !m ) return;
  temp_free((char *)m[-1]);
  temp_free((char *)(m-1));
}

/******************************************************************
*
* Function: vector_add()
*
* Purpose: add vector b to vector a 
*/
void vector_add ARGS3((a,b,n),
REAL *a, REAL *b,
int n)
{ for(;n!=0;n--) *(a++) += *(b++);
}

/******************************************************************
*
* Function: vector_add_smul()
*
* Purpose: add scalar multiple of vector b to vector a 
*/
void vector_add_smul ARGS4((a,b,c,n),
REAL *a, REAL *b,
REAL c,
int n)
{ for(;n!=0;n--) *(a++) += c*(*(b++));
}

/******************************************************************
*
* Function: vector_sub()
*
* Purpose: subtract vector b from vector a  
*/

void vector_sub ARGS3((a,b,n),
REAL *a, REAL *b,
int n)
{ for(;n!=0;n--) *(a++) -= *(b++);
}

/******************************************************************
*
* Function: vnormal()
*
* Purpose: given 3 points, find cross product of sides 
*/
void vnormal ARGS4((a,b,c,n),
REAL *a, REAL *b, REAL *c, REAL *n)
{
  REAL aa[MAXCOORD],bb[MAXCOORD];
  int i;

  for ( i = 0 ; i < SDIM ; i++ )
  { aa[i] = a[i] - c[i];
    bb[i] = b[i] - c[i];
  }
  cross_prod(aa,bb,n);
}
  
/******************************************************************
*
* Function: cross_product()
*
* Purpose; Find 3D cross product of a and b, return in c
*/
void cross_prod ARGS3((a,b,c),
REAL *a, REAL *b, REAL *c)
{
  c[0] = a[1]*b[2] - a[2]*b[1];
  c[1] = a[2]*b[0] - a[0]*b[2];
  c[2] = a[0]*b[1] - a[1]*b[0];
} 

/******************************************************************
*
* Function: triple_prod()
*
* Purpose: Find scalar triple product in 3D.
*/
REAL triple_prod ARGS3((a,b,c),
REAL *a, REAL *b, REAL *c)
{
  return  a[0]*(b[1]*c[2] - b[2]*c[1]) - a[1]*(b[0]*c[2] - b[2]*c[0])
             + a[2]*(b[0]*c[1] - b[1]*c[0]);
}

/******************************************************************
*
* Function: dot(), dotdf(), dotf()
*
* Purpose: dot products of various REALs and floats.
*
*/
/* dot product of REALS */
REAL dot ARGS3((a,b,n),
REAL *a, REAL *b,
int n)  /* number of items */
{
  REAL x = 0.0;
  for (  ; --n >= 0 ;  ) x += (*(a++))*(*(b++));
  return x;
}

/* dot product for doubles and floats */
REAL dotdf ARGS3((a,b,n),
REAL *a,
float *b,
int n)  /* number of items */
{
  REAL x = 0.0;
  for (  ; --n >= 0 ;  ) x += (*(a++))*(*(b++));
  return x;
}

/* dot product for floats */
REAL dotf ARGS3((a,b,n),
float *a, float *b,
int n)  /* number of items */
{
  REAL x = 0.0;
  for (  ; --n >= 0 ;  ) x += (*(a++))*(*(b++));
  return x;
}


/******************************************************************
*
* Function: matvec_mul()
*
* Purpose:  matrix times vector multiplication, c = a * b 
*/
void matvec_mul ARGS5((a,b,c,rows,cols),
REAL **a, REAL *b, REAL *c,
int rows, int cols)
{
  int i,j;

  for ( i = 0 ; i < rows ; i++ )
  { c[i] = 0.0;
    for ( j = 0 ; j < cols ; j++ )
      c[i] += a[i][j]*b[j];
  }
}


/******************************************************************
*
* Function: vec_mat_mul()
*
* Purpose: vector times matrix multiplication, c = a * b 
*/
void vec_mat_mul ARGS5((a,b,c,rows,cols),
REAL *a, REAL **b, REAL *c,
int rows, int cols)
{
  int i,j;

  for ( i = 0 ; i < cols ; i++ )
  { c[i] = 0.0;
    for ( j = 0 ; j < rows ; j++ )
      c[i] += a[j]*b[j][i];
  }
}

/******************************************************************
*
* Function: mat_mult()
*
* Purpose: matrix by matrix multiplication,  c = a * b 
*          a is imax x jmax, b is jmax x kmax, c is imax x kmax
*       a, b, and c need not be distinct.
*       Tests for zero entries in first matrix for larger matrices,
*         so if one matrix is sparse, put it first.
*/
void mat_mult ARGS6((a,b,c,imax,jmax,kmax),
REAL **a, REAL **b, REAL **c,  /* not assumed distinct */
int imax, int jmax,int kmax)
{ int i,j,k;

  if ( (a == c) || (b == c) )
  { if ( (imax<=MAXCOORD)&&(kmax<=MAXCOORD))
    { MAT2D(temp,MAXCOORD,MAXCOORD);  /* local temp space */
      for ( i = 0 ; i < imax ; i++ )
        for ( k = 0 ; k < kmax ; k++ )
          for ( j = 0, temp[i][k] = 0.0 ; j < jmax ; j++ )
             temp[i][k] += a[i][j]*b[j][k];
      matcopy(c,temp,imax,kmax);
    }
    else /* have to go to the effort to get temp work space */
    { REAL **temp = dmatrix(0,imax-1,0,kmax-1);  /* temporary storage */
      for ( i = 0 ; i < imax ; i++ )
        for ( j = 0 ; j < jmax ; j++ )
        { REAL aa = a[i][j];
          if ( aa==0.0 ) continue;
          for ( k = 0 ; k < kmax ; k++ )
            temp[i][k] += aa*b[j][k];
        }
      matcopy(c,temp,imax,kmax);
      free_matrix(temp);
    }
  }
  else
  { for ( i = 0 ; i < imax ; i++ )
    { for ( k = 0 ; k < kmax ; k++ ) c[i][k] = 0.0;
      for ( j = 0 ; j < jmax ; j++ )
      { REAL aa = a[i][j];
        if ( aa == 0.0 ) continue;
        for ( k = 0 ; k < kmax ; k++ )
          c[i][k] += aa*b[j][k];
      }
    }
  }
}

/******************************************************************
*
* Function: tr_mat_mul()
*
* Purpose: matrix transpose by matrix multiplication 
*          output: c = aT*b 
*          a is imax x jmax, b is imax x kmax, c is jmax x kmax 
*       a, b, and c need not be distinct.
*/
void tr_mat_mul ARGS6((a,b,c,imax,jmax,kmax),
REAL **a, REAL **b, REAL **c,  /* not assumed distinct */
int imax,int jmax,int kmax) 
{
  REAL **temp;  /* temporary storage, if needed */
  int i,j,k;

  if ( (a == c) || (b == c) )
  { temp = dmatrix(0,jmax-1,0,kmax-1);  /* temporary storage */
    for ( j = 0 ; j < jmax ; j++ )
      for ( k = 0 ; k < kmax ; k++ )
        for ( i = 0 ; i < imax ; i++ )
          temp[j][k] += a[i][j]*b[i][k];
    matcopy(c,temp,jmax,kmax);
    free_matrix(temp);
  }
  else
  { REAL *s;
    for ( j = 0 ; j < jmax ; j++ )
      for ( k = 0, s = c[j] ; k < kmax ; k++,s++ )
      { *s = 0.0;
         for ( i = 0 ; i < imax ; i++ )
           *s += a[i][j]*b[i][k];
      }
  }
}

/******************************************************************
*
* Function: mat_mul_tr()
*
* Purpose: matrix by matrix transpose multiplication,  c = a * bT 
*       a is imax x jmax, b is kmax x jmax, c is imax x kmax
*       a, b, and c need not be distinct.
*/  
void mat_mul_tr ARGS6((a,b,c,imax,jmax,kmax),
REAL **a, REAL **b, REAL **c,  /* not assumed distinct */
int imax, int jmax, int kmax)
{
  REAL **temp;  /* temporary storage, if needed */
  int i,j,k;

  if ( (a == c) || (b == c) )
  { temp = dmatrix(0,imax-1,0,kmax-1);  /* temporary storage */
    for ( i = 0 ; i < imax ; i++ )
      for ( j = 0 ; j < jmax ; j++ )
        for ( k = 0 ; k < kmax ; k++ )
          temp[i][k] += a[i][j]*b[k][j];
    matcopy(c,temp,imax,kmax);
    free_matrix(temp);
  }
  else
  { 
    for ( k = 0 ; k < kmax ; k++ )
    { for ( i = 0 ; i < imax ; i++ ) c[i][k] = 0.0;
      for ( j = 0 ; j < jmax ; j++ ) 
      { REAL bb = b[k][j];
        if ( bb == 0.0 ) continue;
        for ( i = 0 ; i < imax ; i++ )
          c[i][k] += a[i][j]*bb;
      }
    }
  }
}

/******************************************************************
*
* Function: mat_tsquare()
*
* Purpose: matrix times own transpose, b = a*aT
*          a and b must be different. 
*/
void  mat_tsquare ARGS4((a,b,n,m),
REAL **a, /* original */
REAL **b, /* square  b = a*aT */
int n, int m) /* a is nxm, b is nxn */
{
  int i,j;
  if ( a == b )
    kb_error(2141,"mat_tsquare: a and b same (internal error).\n",RECOVERABLE);
  for ( i = 0 ; i < n ; i++ )
    for ( j = 0 ; j <= i ; j++ )
      b[i][j] = b[j][i] = dot(a[i],a[j],m);
}

/******************************************************************
*
* Function: quadratic_form()
*
* Purpose: quadratic form evaluation, a*b*c; only uses lower triangle 
*/
REAL quadratic_form ARGS4((a,b,c,n),
REAL *a, REAL **b, REAL *c,
int n) /* size */ 
{ int i,j;
  REAL sum = 0.0;
  REAL temp;

  for ( i = 0 ; i < n ; i++ )
  { temp = b[i][0]*c[0];
    for ( j = 1 ; j <= i ; j++ )
      temp += b[i][j]*c[j];
    for (  ; j < n ; j++ )
      temp += b[j][i]*c[j];
    sum += a[i]*temp;
  }

  return sum;
}


/******************************************************************
*
* Function: mat_inv()
*
* Purpose: in-place matrix inverse by gauss-jordan 
* returns -1 for singular matrix, 
*          >= 0 for nonsingular, but value is not index, since
*               pivoting is not symmetric. 
*           
*/

#define SWAP(a,b) {REAL temp = (a); (a) = (b); (b) = temp; }
#define SMALL 10

int mat_inv ARGS2((a,n),
REAL **a,     /* matrix to invert in place */
int n)        /* size of matrix */
{
  int *indxc,*indxr,*ipiv;
  int i,icol=0,irow=0,j,k,l,ll;
  REAL big,dum,pivinv;
  int retval = 1;  /* default return value is success */
  int temp1[SMALL],temp2[SMALL],temp3[SMALL]; /* avoid alloc for small sizes */

  if ( n <= SMALL )
  { indxc = temp1; indxr = temp2; ipiv = temp3; }
  else
  { /* large size */
    indxc = ivector(0,n-1);
    indxr = ivector(0,n-1);
    ipiv  = ivector(0,n-1);
  }
  for ( j = 0 ; j < n ; j++ ) 
    ipiv[j] = -1;
  for ( i = 0 ; i < n ; i++ )
  { /* find pivot */
    big = 0.0;
    for ( j = 0 ; j < n ; j++ )
    { if ( ipiv[j] != 0 )
         for ( k = 0 ; k < n ; k++ )
         { if ( ipiv[k] == -1 )
           { if ( fabs(a[j][k]) >= big )
             { big = fabs(a[j][k]);
               irow = j;
               icol = k;
             }
           }
           else 
             if ( ipiv[k] > 0 ) 
             { retval = -1; goto mat_inv_exit; }
         }
    }
    ++(ipiv[icol]);

    if ( irow != icol )
       for ( l = 0 ; l < n ; l++ ) 
         SWAP(a[irow][l],a[icol][l])
    indxr[i] = irow;
    indxc[i] = icol;
    if ( a[icol][icol] == 0.0 ) 
    { retval = -1; goto mat_inv_exit; }
    pivinv = 1/a[icol][icol];
    a[icol][icol] = 1.0;
    for ( l = 0 ; l < n ; l++ ) 
      a[icol][l] *= pivinv;
    for ( ll = 0  ; ll < n ; ll++ )
      if ( ll != icol )
      { dum = a[ll][icol];
        a[ll][icol] = 0.0;
        for ( l = 0 ; l < n ; l++ ) 
          a[ll][l] -= a[icol][l]*dum;
      }
  }
  for ( l = n-1 ; l >= 0 ; l-- )
  { if ( indxr[l] != indxc[l] )
       for ( k = 0 ; k < n ; k++ )
          SWAP(a[k][indxr[l]],a[k][indxc[l]])
  }

mat_inv_exit:
  if ( n > SMALL )
  { free_ivector(ipiv,0,n-1);
    free_ivector(indxr,0,n-1);
    free_ivector(indxc,0,n-1);
  }
  return retval;
} /* end mat_inv() */


/***********************************************************************
*
* Function: mat_approx_solve()
*
* Purpose: Solve dense linear system, possibly singular, with
*          criterion for omitting singular rows.  Meant for 
*          solving for redundant constraints.
*
* Return value: effective rank of matrix; solution in b
*/
REAL mat_approx_solve_epsilon = 1e-6;

int mat_approx_solve ARGS3((a,n,b),
REAL **a,     /* matrix to invert in place */
int n,        /* size of matrix */
REAL *b)      /* right hand side */
{
  int i,j,l,ll,irow=0;
  REAL big,dum,pivinv;
  int rank = n;  /* return value is effective rank of matrix */

  /* normalize row magnitudes */
  for ( i = 0 ; i < n ; i++ )
  { REAL mag = 0.0;
    for ( j = 0 ; j < n ; j++ )
      mag += a[i][j]*a[i][j];
    if ( mag > 0.0 )
      mag = 1/sqrt(mag);
    for ( j = 0 ; j < n ; j++ )
      a[i][j] *= mag;
    b[i] *= mag;
  }

  /* now gaussian elimination */
  for ( i = 0 ; i < n ; i++ )
  { /* find pivot */
    big = 0.0;
    for ( j = i ; j < n ; j++ )
    { 
      if ( fabs(a[j][i]) >= big )
      { big = fabs(a[j][i]);
        irow = j;
      }
    }

    if ( irow != i )
    { for ( l = 0 ; l < n ; l++ ) 
        SWAP(a[irow][l],a[i][l])
      SWAP(b[irow],b[i]);
    }
    if ( a[i][i] < mat_approx_solve_epsilon ) 
    { rank--; continue; }

    pivinv = 1/a[i][i];
    a[i][i] = 1.0;
    for ( l = i+1 ; l < n ; l++ ) 
      a[i][l] *= pivinv;
    b[i] *= pivinv;
    for ( ll = 0  ; ll < n ; ll++ )
    { if ( ll == i ) continue;
      { dum = a[ll][i];
        a[ll][i] = 0.0;
        for ( l = 0 ; l < n ; l++ ) 
          a[ll][l] -= a[i][l]*dum;
        b[ll] -= b[i]*dum;
      }
    }
  }

  return rank;

} /* end mat_approx_solve() */


/******************************************************************
*
* Function: mat_inv_sym()
*
* Purpose: in-place symmetric matrix inverse with Bunch-Kauffman pivoting
* returns -1 for singular matrix, 
*          >= 0 for nonsingular, value is index
*           
*/

#define PIV1 1
#define PIV2 2
#define PIV3 3  /* second of double pivot */

int mat_inv_sym ARGS2((a,n),
REAL **a,     /* matrix to invert in place */
int n)        /* size of matrix */
{
  int *spiv; /* 1 or 2, size of pivot */
  int *ipiv; /* permutation */
  int i,irow=0,j,k;
  REAL piv;
  int temp1[SMALL],temp3[SMALL]; /* avoid alloc for small sizes */
  int negs = 0;

  if ( n <= SMALL )
  { spiv = temp1; ipiv = temp3; }
  else
  { /* large size */
    spiv = ivector(0,n-1);
    ipiv = ivector(0,n-1);
  }
  for ( j = 0 ; j < n ; j++ ) 
    ipiv[j] = j;

  /* First, do LDL with Bunch-Kaufman pivoting */
  for ( i = 0 ; i < n ; i++ )
  { /* find pivot */
    /* max in pivot column */
    int s = 1; /* pivot size */
    double lambda = 0.0;
    for ( j = i+1 ; j < n ; j++ )
    { if ( fabs(a[i][j]) > lambda )
      { lambda = fabs(a[i][j]);
        irow = j;
      }
    }
    if ( lambda > 0.0 )
    { if ( fabs(a[i][i]) > BKalpha*lambda )
        s = 1; 
      else
      { double sigma = 0.0;
        for ( j = i ; j < n ; j++ )
          if ( fabs(a[j][irow]) > sigma )
            sigma = fabs(a[j][irow]);
        if ( sigma*fabs(a[i][i]) >= BKalpha*lambda*lambda )
          s = PIV1;
        else if ( fabs(a[irow][irow]) >= BKalpha*sigma )
        { 
          /* swap irow and i */
          int itmp = ipiv[i];
          ipiv[i] = ipiv[irow];
          ipiv[irow] = itmp;
          s = PIV1;
		  for ( j = 0 ; j < i ; j++ )
            SWAP(a[i][j],a[irow][j]);
          SWAP(a[i][i],a[irow][irow]);
          for ( j = i+1 ; j < irow ; j++ )
            SWAP(a[j][i],a[irow][j]);
          for ( j = irow+1 ; j < n ; j++ )
            SWAP(a[j][i],a[j][irow]);
        }
        else
        { /* swap irow and i+1 */
          int itmp = ipiv[i+1];
          ipiv[i+1] = ipiv[irow];
          ipiv[irow] = itmp;
          s = PIV2;
          if ( irow != i+1 )
          { for ( j = 0 ; j <= i ; j++ )
              SWAP(a[i+1][j],a[irow][j]);
            SWAP(a[i+1][i+1],a[irow][irow]);
            for ( j = i+2 ; j < irow ; j++ )
              SWAP(a[j][i+1],a[irow][j]);
            for ( j = irow+1 ; j < n ; j++ )
              SWAP(a[j][i+1],a[j][irow]);
          }
        }
      }
    }
   
    /* now pivot step */
    if ( s == PIV1 )
    { if ( a[i][i] == 0.0 ) 
      { negs = -1;
        goto mat_inv_sym_exit; 
      }
      piv = 1/a[i][i]; 
      a[i][i] = piv;
      for ( j = i+1 ; j < n ; j++ )
      { for ( k = i+1 ; k < j ; k++ )
           a[j][k] -= a[j][i]*a[k][i];
	    a[j][j] -= a[j][i]*a[j][i]*piv;
        a[j][i] *= piv;
      }
      spiv[i] = PIV1;
      if ( piv < 0.0 ) negs++;
    }
    else /* s == 2, so 2 x 2 pivot */
    { double t;
	  double det = a[i][i]*a[i+1][i+1] - a[i+1][i]*a[i+1][i];
      if ( det == 0.0 ) 
      { negs = -1;
        goto mat_inv_sym_exit; 
      }
      t = a[i][i];
      a[i][i] = a[i+1][i+1]/det;
      a[i+1][i+1] = t/det; 
      a[i][i+1] = a[i+1][i] /= -det;
      for ( j = i+1 ; j < n ; j++ )
      { for ( k = i+1 ; k < j ; k++ )
           a[j][k] -= a[j][i]*a[k][i] - a[j][i+1]*a[k][i+1];
        t = a[i][i]*a[j][i] + a[i+1][i]*a[j][i+1];
        a[j][i+1] = a[i+1][i]*a[j][i] + a[i+1][i+1]*a[j][i+1];
        a[j][i] = t;
      }
      spiv[i] = PIV2;
      spiv[i+1] = PIV3;
      if ( det < 0.0 ) negs++;
      else if ( a[i][i] < 0.0 ) negs += 2;
      i++; /* extra increment */
    }

  }

  /* convert from LDL to inverse */
  /* invert L to upper triangle U */
  for ( i = 1 ; i < n ; i++ )
  { for ( j = 0 ; j < i-1 ; j++ )
    { a[j][i] = -a[i][j];
      for ( k = i+1 ; k < n ; k++ )
       a[j][k] += a[k][i]*a[j][i];
    }
    if ( spiv[i] == PIV3 )
      for ( k = i+1 ; k < n ; k++ )
         a[i-1][k] = 0.0;
    else
    { a[i-1][i] = -a[i][i-1];
      for ( k = i+1 ; k < n ; k++ )
        a[i-1][k] = a[k][i]*a[i-1][i]; 
    }
  }
  /* multiply by already inverted diagonal into lower */
  for ( j = 0 ; j < n ; j++ )
  { if ( spiv[j] == PIV1 )
    { for ( i = 0 ; i < j ; i++ )
        a[j][i] = a[i][j]*a[j][j];
    }
    else  /* PIV2 */
    { for ( i = 0 ; i < j-1 ; i++ )
      { a[j][i] = a[i][j]*a[j][j] + a[i][j+1]*a[j+1][j];
        a[j+1][i] = a[i][j+1]*a[j][j+1] + a[i][j+1]*a[j+1][j+1];
      }
      j++; /* extra increment */
    }
  }
  /* multiply lower triangle and upper to get full inverse in lower */
  for ( i = 0 ; i < n ; i++ )
  { 
    for ( j = i ; j < n ; j++ )
	{ double sum;
      int first;
	  if ( j == i )
	  { sum = a[i][i];
	    if ( spiv[j] == PIV2 )
	      first = i+2;
		else 
		  first = i+1;
	   }
	   else if ( (j == i-1) && (spiv[j]==PIV2) ) 
	   { sum = 0;
	     first = i+1;
	   }
	   else 
	   { sum = a[i][j]*a[i][i];
	     first = i+1;
	   }
       for ( k = first ; k < n ; k++ )
        sum += a[i][k]*a[k][j];
       a[i][j] = sum;
    }
  }
  /* unpermute lower to upper */
  /* first, off diagonal */
  for ( i = 0 ; i < n ; i++ )
  { for ( j = 0 ; j < i ; j++ )
      a[ipiv[j]][ipiv[i]] = a[i][j];
  }
  /* and then the diagonal */
  for ( i = 0 ; i < n ; i++ )
  { j = i;
    while ( ipiv[j] != j )
    { int ti = ipiv[j];
      double t = a[ti][ti];
      a[ti][ti] = a[j][j];
      a[j][j] = t;
      ipiv[j] = ipiv[ti]; 
      ipiv[ti] = ti;
    }
  }  

  /* symmetrize from upper to lower */
  for ( i = 0 ; i < n ; i++ )
    for ( j = 0 ; j < i ; j++ )
      a[i][j] = a[j][i];

  
mat_inv_sym_exit:
  if ( n > SMALL )
  { free_ivector(ipiv,0,n-1);
    free_ivector(spiv,0,n-1);
  }
  return negs;
}

/*************************************************************************
*
* function: LD_factor()
*
* purpose: factor dense symmetric matrix into LDL^T form.  Touches only
*          lower triangle.  Does not pivot, so should be used only
*          on positive definite matrices.
*/

int LD_factor ARGS2(( H, N ),
REAL **H,  /* lower triangular, row by row */
int N)     /* size */
{ int i,j,k;
  int negs = 0;

#ifdef BLAS
  if ( blas_flag )
  { char uplo = 'U';
    int stride = H[1] - H[0];
    int info=0;
    DPOTRF(&uplo,&N,&H[0][0],&stride,&info);
  }
  else
#endif
  for ( i = 0 ; i < N ; i++ )   /* pivot (i,i) */
  { REAL pivot = H[i][i];
    if ( pivot == 0.0 )
    { kb_error(2553,"Trying to factor singular matrix; using 1 as pivot.\n",
       WARNING);
      pivot = 1.0;
    }
    if ( pivot < 0 ) negs++;
    pivot = 1/pivot;
    for ( j = i+1 ; j < N ; j++ )  /* row j */
    { REAL x = H[j][i];
      for ( k = i+1 ; k < j ; k++ ) /* col k */
        H[j][k] -= x*H[k][i];
      H[j][i] = x*pivot;
      H[j][j] -= x*x*pivot;
    }
  }
 return negs;
}

/***********************************************************************
*
* function: LD_solve
*
* purpose: solve for a single rhs using matrix factored by LD_factor.
*/

void LD_solve ARGS4((LD,B,X,N),
REAL **LD,  /* from LD_factor() */
REAL *B,  /* rhs */
REAL *X,  /* solution */
int N)
{ int i,j;
#ifdef BLAS
  if ( blas_flag )
  { char uplo = 'U';
    int stride = LD[1] - LD[0];
    int info=0;
    int nrhs = 1; /* number of rhs */
    for ( i = 0 ; i < N ; i++ ) X[i] = B[i]; /* since DPOTRS overwrites */
    DPOTRS(&uplo,&N,&nrhs,&LD[0][0],&stride,&X[0],&N,&info);
  }
  else
#endif
  {  for ( i = 0 ; i < N ; i++ )
     { X[i] = B[i];
       for ( j = 0 ; j < i ; j++ )
         X[i] -= LD[i][j]*X[j];
     }
     for ( i = 0 ; i < N ; i++ )
       X[i] /= LD[i][i];
     for ( i = N-1 ; i >= 0 ; i-- )
     { for ( j = i+1 ; j < N ; j++ )
        X[i] -= LD[j][i]*X[j];
     }
  }
}


/******************************************************************
*
* Function: det_adjoint()
*
* Purpose: calculates determinant in place and leaves adjoint transpose 
*/
REAL  det_adjoint ARGS2((a,n),
REAL **a,     /* matrix to change in place */
int n)        /* size of matrix */
{
  int *indxc,*indxr,*ipiv;
  int i,icol=0,irow=0,j,k,l,ll;
  REAL big,dum,pivinv,piv;
  int temp1[SMALL],temp2[SMALL],temp3[SMALL]; /* avoid alloc for small sizes */
  REAL det = 1.0;  /* will multiply by pivots */

  if ( n <= 0 )
    kb_error(1205,"Internal error: Matrix size not positive.",RECOVERABLE);

  if ( n == 1 ) { det = a[0][0]; a[0][0] = 1.0; return det; }
  if ( n == 2 )
  { REAL temp;
    det = a[0][0]*a[1][1] - a[0][1]*a[1][0];
    temp = a[0][0]; a[0][0] = a[1][1]; a[1][1] = temp;
    a[0][1] = -a[0][1]; a[1][0] = -a[1][0];
    return det;
  }

  if ( n <= SMALL )
  { indxc = temp1; indxr = temp2; ipiv = temp3; }
  else
  { /* large size */
    indxc = ivector(0,n-1);
    indxr = ivector(0,n-1);
    ipiv  = ivector(0,n-1);
  }
  for ( j = 0 ; j < n ; j++ ) ipiv[j] = -1;
  for ( i = 0 ; i < n-1 ; i++ )
  { big = 0.0;
    for ( j = 0 ; j < n ; j++ )
      if ( ipiv[j] != 0 )
         for ( k = 0 ; k < n ; k++ )
         { if ( ipiv[k] == -1 )
           { if ( fabs(a[j][k]) >= big )
             { big = fabs(a[j][k]);
               irow = j;
               icol = k;
             }
           }
           else if ( ipiv[k] > 0 )
           { kb_error(1206,"Internal: ipiv > 0.\n",WARNING); det = 0.0; goto det_exit; }
         }
      ++(ipiv[icol]);

      if ( irow != icol )
      { for ( l = 0 ; l < n ; l++ ) SWAP(a[irow][l],a[icol][l])
        det = -det;
      }
      indxr[i] = irow;
      indxc[i] = icol;
      det *= a[icol][icol];  /* build determinant */
      if ( a[icol][icol] == 0.0 ) { goto det_lowrank; }
      pivinv = 1/a[icol][icol];
      a[icol][icol] = 1.0;
      for ( l = 0 ; l < n ; l++ ) a[icol][l] *= pivinv;
      for ( ll = 0  ; ll < n ; ll++ )
        if ( ll != icol )
        { dum = a[ll][icol];
          a[ll][icol] = 0.0;
          for ( l = 0 ; l < n ; l++ ) a[ll][l] -= a[icol][l]*dum;
        }
  }
  /* special treatment for last pivot; works even if zero */
  for ( j = 0 ; j < n ; j++ )
     if ( ipiv[j] != 0 ) { irow = icol = j; break; }
  indxr[n-1] = irow;
  indxc[n-1] = icol;
  piv = a[icol][icol];
  a[icol][icol] = 1.0;
  for ( l = 0 ; l < n ; l++ ) a[icol][l] *= det;
  for ( ll = 0  ; ll < n ; ll++ )
    if ( ll != icol )
    { dum = a[ll][icol];
      a[ll][icol] = 0.0;
      for ( l = 0 ; l < n ; l++ ) 
         a[ll][l] = a[ll][l]*piv*det - a[icol][l]*dum;
    }
  det *= piv;

  for ( l = n-1 ; l >= 0 ; l-- )
  { if ( indxr[l] != indxc[l] )
      for ( k = 0 ; k < n ; k++ )
        SWAP(a[k][indxr[l]],a[k][indxc[l]])
  }

det_exit:
  if ( n > SMALL )
  { free_ivector(ipiv,0,n-1);
    free_ivector(indxr,0,n-1);
    free_ivector(indxc,0,n-1);
  }
  return det;

det_lowrank: /* rank less than n-1, so adjoint = 0 */
  for ( i = 0 ; i < n ; i++ )
     for ( j = 0 ; j < n ; j++ )
        a[i][j] = 0.0;
  det = 0.0;
  goto det_exit;
  
}


/******************************************************************
*
* Function: determinant()
*
* Purpose: calculates determinant; no change in matrix for 3x3 or smaller 
*           otherwise calls det_adjoint()
*/
REAL  determinant ARGS2((a,n),
REAL **a,     /* matrix to change in place */
int n)        /* size of matrix */
{
  if ( n == 1 ) { return a[0][0];  }
  if ( n == 2 ) { return  a[0][0]*a[1][1] - a[0][1]*a[1][0]; }
  if ( n == 3 )
     { return a[0][0]*(a[1][1]*a[2][2] - a[1][2]*a[2][1])
              - a[0][1]*(a[1][0]*a[2][2] - a[1][2]*a[2][0])
              + a[0][2]*(a[1][0]*a[2][1] - a[1][1]*a[2][0]);
     }
  return det_adjoint(a,n);  /* other cases */
}

/******************************************************************
*
* Function: print_matrix()
*
*/
void print_matrix ARGS3((a,rows,cols),
REAL **a,
int rows, int cols)
{
  int i,j;

  for ( i = 0 ; i < rows ; i++ )
    { msg[0] = 0;
      for ( j = 0 ; j < cols ; j++ )
        sprintf(msg+strlen(msg),"%10.6f ",(DOUBLE)a[i][j]);
      strcat(msg,"\n");
      outstring(msg);
    }
}

/******************************************************************
*
* Function: exterior_product()
*
* Purpose: conversion of k vectors to a k-vector 
*          components in index lexicographic order 
*/
void exterior_product ARGS4((v,w,k,n),
REAL **v,  /* list of k vectors */
REAL *w,    /* returned k-vector */
int k,      /* number of vectors */
int n)      /* space dimension */
{
  /* anticipate only small k, so just brute force */
  int i1,i2,i3;

  switch ( k )
    {
      case 1:  for ( i1 = 0 ; i1 < n ; i1++ ) *(w++) = v[0][i1];
               break;

      case 2:  for ( i1 = 0 ; i1 < n ; i1++ )
                 for ( i2 = i1+1 ; i2 < n ; i2++ )
                    *(w++) = v[0][i1]*v[1][i2] - v[0][i2]*v[1][i1];
               break;

      case 3:  for ( i1 = 0 ; i1 < n ; i1++ )
                 for ( i2 = i1+1 ; i2 < n ; i2++ )
                   for ( i3 = i2+1 ; i3 < n ; i3++ )
                      *(w++) = v[0][i1]*v[1][i2]*v[2][i3]
                             + v[0][i2]*v[1][i3]*v[2][i1] 
                             + v[0][i3]*v[1][i1]*v[2][i2] 
                             - v[0][i1]*v[1][i3]*v[2][i2] 
                             - v[0][i3]*v[1][i2]*v[2][i1] 
                             - v[0][i2]*v[1][i1]*v[2][i3] ;
               break;

      default: sprintf(errmsg,"Exterior product of %d vectors.\n",k);
               kb_error(1207,errmsg,RECOVERABLE);

               break;
    }
}

/**********************************************************************
*
*  function: kernel_basis()
*
*  purpose:  Find basis for kernel of matrix (nullspace of rows)
*/

int kernel_basis ARGS4((a,ker,imax,jmax),
REAL **a,  /* the matrix, will be altered */
REAL **ker, /* for basis vectors in columns */
int imax, int jmax)  /* rows and columns of a */
{
  int i,j,k;
  int pivrow[20];    /* pivot row in column */
  int n; /* nullity */

  for ( j = 0 ; j < jmax ; j++ ) pivrow[j] = -1;  /* mark as no pivot in col */

  /* get row echelon form, pivot largest in each row */
  for ( i = 0 ; i < imax ; i++ )
  { int piv = -1;
    REAL b,big,p;

    /* find largest element in row */
    big = 0.0;
    for ( j = 0 ; j < jmax ; j++ )
      if ( fabs(a[i][j]) > big )
      { big = fabs(a[i][j]);
        piv = j;
      }
    if ( piv == -1 ) continue; /* row of zeros */
    pivrow[piv] = i;

    /* pivot step */
    p = a[i][piv];
    for ( j = 0 ; j < jmax ; j++ )
      a[i][j] /= p;
    for ( k = 0 ; k < imax ; k++ )
    { if ( k == i ) continue;
      b = a[k][piv];
      for ( j = 0 ; j < jmax ; j++ )
           a[k][j] -= b*a[i][j];
    }
  }         

  /* now find kernel basis */
  for ( j = 0, n = 0 ; j < jmax ; j++ )
  { if ( pivrow[j] >= 0 ) continue;  /* column has leading 1 */
    /* column j is parameter column */
    for ( k = 0 ; k < jmax ; k++ )
    { if ( pivrow[k] >= 0 )
         ker[k][n] = -a[pivrow[k]][j];
      else if ( k == j )
         ker[k][n] = 1.0;
      else ker[k][n] = 0.0;
    }
    n++;
  }
  return n; /* nullity */
}

/**********************************************************************
*
*  function: kernel_basis_rows()
*
*  purpose:  Find basis for kernel of matrix (nullspace of rows)
*                Returns basis rowwise.
*      basis vectors normalized, but not orthohormal.
*/

int kernel_basis_rows ARGS4((a,ker,imax,jmax),
REAL **a,  /* the matrix, will be altered */
REAL **ker, /* for basis vectors in rows */
int imax, int jmax)  /* rows and columns of a */
{
  int i,j,k;
  int pivrow[20];    /* pivot row in column */
  int pivcol[20];    /* pivot column  in row */
  int n; /* nullity */
  int  detsign=1;  /* to try to keep orientation of normal positive */

  for ( j = 0 ; j < jmax ; j++ ) pivrow[j] = -1;  /* mark as no pivot in col */

  /* get row echelon form, pivot largest in each row */
  for ( i = 0 ; i < imax ; i++ )
  { int piv = -1;
    REAL b,big,p;

    /* find largest element in row */
    big = 0.0;
    for ( j = 0 ; j < jmax ; j++ )
      if ( fabs(a[i][j]) > big )
      { big = fabs(a[i][j]);
        piv = j;
      }
    if ( piv == -1 ) continue; /* row of zeros */
    pivrow[piv] = i; pivcol[i] = piv;

    /* pivot step */
    p = a[i][piv];   if ( p < 0 ) detsign = -detsign;
    for ( j = 0 ; j < jmax ; j++ )
      a[i][j] /= p;
    for ( k = 0 ; k < imax ; k++ )
    { if ( k == i ) continue;
      b = a[k][piv];
      for ( j = 0 ; j < jmax ; j++ )
         a[k][j] -= b*a[i][j];
    }
  }         

  /* now find kernel basis */
  for ( j = 0, n = 0 ; j < jmax ; j++ )
  { int sign;
    if ( pivrow[j] >= 0 ) continue;  /* column has leading 1 */
    /* column j is parameter column */
    pivcol[imax+n] = j;
    /* get sign for pos det */
    for (sign = detsign, k=0 ; k <= imax+n ; k++ )
      for ( i = k+1 ; i <= imax+n ; i++ )
        if ( pivcol[k] > pivcol[i] ) sign = -sign;
    for ( k = 0 ; k < jmax ; k++ )
    { if ( pivrow[k] >= 0 )
        ker[n][k] = -sign*a[pivrow[k]][j];
      else if ( k == j )
        ker[n][k] = sign;
      else ker[n][k] = 0.0;
    }
    n++;
  }
  /* normalize */
  for ( i = 0 ; i < n ; i ++ )
  { REAL mag;
    mag = sqrt(SDIM_dot(ker[i],ker[i]));
    for ( j = 0 ; j < SDIM ; j++ )
      ker[i][j] /= mag;
  } 

  return n; /* nullity */
}


/*********************************************************************
*
* function: matrix_index()
*
* purpose:  return number of negative eigenvalues of matrix
*           Does not destroy original.  For symmetric matrices.
*
*/

int matrix_index ARGS2((M,n),
REAL **M,  /* square matrix */
int n)  /* size */
{ REAL **a = dmatrix(0,n-1,0,n-1);
  REAL *tempptr;
  int row,col,prow=0;
  REAL maxp;
  int i,j;
  int indx = 0;
  REAL temp;
  REAL *firstrow;  /* for proper freeing after swapping rows */

  firstrow = a[0];
  matcopy(a,M,n,n);


  /* basically, gauss elimination to lower triangular form
     with partial pivoting.  
  */
  for ( col = 0 ; col < n ; col++ )
  { /* find max pivot in diagonal */
    maxp = 0.0;
    for ( row = col ; row < n ; row++ )
       if ( fabs(a[row][row]) > maxp ) { maxp = fabs(a[row][row]); prow = row; }
    if ( maxp == 0.0 ) continue;
    if ( prow != col )
    { /* swap rows and columns to keep symmetric */
      tempptr = a[prow]; a[prow] = a[col]; a[col] = tempptr;
      for ( j = col; j < n ; j++ )
      { temp = a[j][col]; a[j][col] = a[j][prow]; a[j][prow] = temp; }
    }
    if ( a[col][col] < 0.0 ) indx++;
    for ( row = col+1 ; row < n ; row++ )
      for ( i = col+1 ; i < n ; i++ )
      { a[row][i] -= a[row][col]/a[col][col]*a[col][i];
      }
  }
  a[0] = firstrow; free_matrix(a);
  return indx;
}


/****************************************************************************
*
* function: jacobi_eigenpairs()
*
* purpose: find eigenpairs of small dense symmetric matrix by jacobi rotations
*          From Numerical Recipes 11.1.
*
* output: eigenvalues in d[], sorted in descending order, and
*         corresponding eigenvectors in columns of v[][].
*/
void jacobi_eigenpairs ARGS5((a,n,d,v,work),
REAL **a,  /* input matrix, destroyed */
int n, /* size */
REAL *d, /* for return of eigenvalues */
REAL **v,  /* for return of eigenvectors */
REAL *work)  /* space for 2*n values */
{ REAL sm,h,tresh,dum,g,t,theta,tau,s,c;
  int i,iq,ip,nrot,j;
  REAL *z,*b;

  z = work;
  b = work+n;

  /* initialize v to identity */
  for ( ip = 0 ; ip < n ; ip++ )
  { for ( iq = 0 ; iq < n ; iq++ ) v[ip][iq] = 0.0;
    v[ip][ip] = 1.0;
  }

  for ( ip = 0 ; ip < n ; ip++ )
  { b[ip] = a[ip][ip];
    d[ip] = b[ip];
    z[ip] = 0.0;
  }

  nrot = 0;
  for ( i = 1 ; i < 50 ; i++ )
  { sm = 0.0;
    for ( ip = 0 ; ip < n-1; ip++ )
      for ( iq = ip + 1 ; iq < n ; iq++ )
        sm += fabs(a[ip][iq]);
    if ( sm == 0.0 ) goto jacobi_exit; /* normal exit */

    if ( i < 4 ) tresh = .2*sm/n/n;
    else tresh = 0.0;

    for ( ip = 0 ; ip < n-1 ; ip++ )
     for ( iq = ip+1 ; iq < n ; iq++ )
     { g = 100*fabs(a[ip][iq]);
       dum = fabs(d[ip]);
       if ( (i > 4) && (dum+g == dum) && (fabs(d[iq])+g == fabs(d[iq])) )
         a[ip][iq] = 0.0;
       else if ( fabs(a[ip][iq]) > tresh ) 
       { h = d[iq] - d[ip];
         if ( fabs(h) + g == fabs(h) ) t = a[ip][iq]/h;
         else 
         { theta = .5*h/a[ip][iq];
           t = 1.0/(fabs(theta) + sqrt(1 + theta*theta));
           if ( theta < 0.0 ) t = -t;
         }
         c = 1.0/sqrt(1 + t*t);
         s = t*c;
         tau = s/(1+c);
         h = t*a[ip][iq];
         z[ip] -= h;
         z[iq] += h;
         d[ip] -= h;
         d[iq] += h;
         a[ip][iq] = 0.0;
         for ( j = 0 ; j <= ip-1 ; j++ )
         { g = a[j][ip];
           h = a[j][iq];
           a[j][ip] = g - s*(h+g*tau);
           a[j][iq] = h + s*(g-h*tau);
         }
         for ( j = ip+1 ; j <= iq-1 ; j++ )
         { g = a[ip][j];
           h = a[j][iq];
           a[ip][j] = g - s*(h+g*tau);
           a[j][iq] = h + s*(g-h*tau);
         }
         for ( j = iq+1 ; j < n ; j++ )
         { g = a[ip][j];
           h = a[iq][j];
           a[ip][j] = g - s*(h+g*tau);
           a[iq][j] = h + s*(g-h*tau);
         }
         for ( j = 0 ; j < n ; j++ )
         { g = v[j][ip];
           h = v[j][iq];
           v[j][ip] = g - s*(h+g*tau);
           v[j][iq] = h + s*(g-h*tau);
         }
         nrot++;
      } /* end if */

     } /* end iq */
      /* end ip */

     for ( ip = 0 ; ip < n ; ip++ )
     { b[ip] += z[ip];
       d[ip] = b[ip];
       z[ip] = 0.0;
     }
  } /* end i */

  kb_error(2548,"50 Jacobi iterations should never happen.\n",WARNING);
  return;

jacobi_exit:
  /* sort eigenpairs in descending order, insertion sort */
  for ( i = 0 ; i < n-1 ; i++ )
  { REAL p;
    int k;
    k = i;
    p = d[i];
    for ( j = i + 1 ; j < n ; j++ )
    { if ( d[j] >= p ) 
      { k = j; p = d[j]; }
    }
    if ( k != i )
    { d[k] = d[i];
      d[i] = p;
      for ( j = 0 ; j < n ; j++ ) 
      { p = v[j][i]; v[j][i] = v[j][k]; v[j][k] = p; }
    }
  }
  return; 
}

/**********************************************************************
*
* function: det_hess(a,h,n)
*
* Purpose: find hessian of determinant as function of entries
*
* Returns  h[i1][j1][i2][j2] as d^2 det(a)/da[i1][j1]/da[i2][j2]
*/

void det_hess ARGS3((a,h,n),
REAL **a,
REAL ****h,
int n)  /* size */
{ int i1,i2,jj1,j2,k;

  /* copy original matrix into h lots of times */
  for ( i1 = 0 ; i1 < n ; i1++ )
    for ( jj1 = 0 ; jj1 < n ; jj1++ )
     for ( i2 = 0 ; i2 < n ; i2++ )
      for ( j2 = 0 ; j2 < n ; j2++ )
         h[i1][jj1][i2][j2] = a[i2][j2];

  /* replace element row and column with identity stuff */
  for ( i1 = 0 ; i1 < n ; i1++ )
    for ( jj1 = 0 ; jj1 < n ; jj1++ )
    { for ( k = 0 ; k < n ; k++ )
      { h[i1][jj1][i1][k] = 0.0;
         h[i1][jj1][k][jj1] = 0.0;
      }
      h[i1][jj1][i1][jj1] = 1.0;
    }

  /* find adjoints */
  for ( i1 = 0 ; i1 < n ; i1++ )
    for ( jj1 = 0 ; jj1 < n ; jj1++ )
    { det_adjoint(h[i1][jj1],n);
      h[i1][jj1][jj1][i1] = 0.0; /* need fixup */
    }

  /* transpose to get back to hessian */
  for ( i1 = 0 ; i1 < n ; i1++ )
    for ( jj1 = 0 ; jj1 < n ; jj1++ )
     for ( i2 = 1 ; i2 < n ; i2++ )
      for ( j2 = 0 ; j2 < i2 ; j2++ )
      { REAL tmp =  h[i1][jj1][i2][j2];
         h[i1][jj1][i2][j2] = h[i1][jj1][j2][i2];
         h[i1][jj1][j2][i2] = tmp;
      }
}

/**********************************************************************
*
* function: gram_schmidt()
*
* purpose: orthonormalize rows of a matrix
*
* return: number of independent rows
*/

int gram_schmidt ARGS3((mat,rows,cols),
REAL **mat,
int rows, int cols)
{ int i,j,k;
  REAL d;
  for ( i = 0 ; i < rows ; i++ )
  { for ( j = 0 ; j < i ; j++ )
    { REAL c = dot(mat[i],mat[j],cols);
      for ( k = 0 ; k < cols ; k++ ) mat[i][k] -= c*mat[j][k];
    }
    d = dot(mat[i],mat[i],cols);
    if ( d == 0.0 ) 
    { rows--; 
      for ( k = 0 ; k < cols ; k++ ) mat[i][k] = mat[rows][k];
      i--;
    }
    else
    { d = 1/sqrt(d);
      for ( k = 0 ; k < cols ; k++ ) mat[i][k] *= d;
    }
  }
  return rows;
} 

/**************************************************************************
*
* function: mat_inv_sparse()
*
* purpose: invert large sparse symmetric matrix in place, with input in 
*          dense format.
*          Should be just temporary until callers get fully integrated
*          with sparse matrix stuff.
*/

int mat_inv_sparse ARGS2((a,n),
REAL **a,     /* matrix to invert in place */
int n)        /* size of matrix */
{ struct linsys S;
  int i,j,k;
  REAL *BB,*Y;

  memset(&S,0,sizeof(S));
  S.N = n;
  /* count entries */
  S.IA = (int *)temp_calloc(n+1,sizeof(int));
  for ( i = 0, k = 0 ; i < n ; i++ )
  { S.IA[i] = k + A_OFF;
    for ( j = i ; j < n ; j++ )
      if ( a[i][j] != 0.0 ) k++;
  }
  S.IA[i] = k + A_OFF;

  /* allocate main space and fill */
  S.JA = (int *)temp_calloc(k,sizeof(int));
  S.A  = (REAL *)temp_calloc(k,sizeof(REAL));
  S.P = (int *)temp_calloc(n,sizeof(int));
  S.IP = (int *)temp_calloc(n,sizeof(int));
  for ( i = 0, k = 0 ; i < n ; i++ )
  { 
    for ( j = i ; j < n ; j++ )
      if ( a[i][j] != 0.0 )
      { S.A[k] = a[i][j];
        S.JA[k] = j + A_OFF;
        k++;
      }
  }

#ifdef USEMINDEG
  xmd_factor(&S);  /* use mindeg, since internals known */
#else
  ysmp_factor(&S);
#endif

  /* solve back for inverse matrix */
  BB = (REAL*)temp_calloc(S.N+10,sizeof(REAL));  /* intermediate solutions */
  Y = (REAL*)temp_calloc(S.N+10,sizeof(REAL));  /* intermediate solutions */
  for ( i = 0 ; i < n ; i++ )
  { 
    memset(BB,0,n*sizeof(REAL));
    BB[i] = 1.0;
#ifdef USEMINDEG
/* mindeg is too specific, using vertex structure to optimize for hessian */
    /* solve U^T Y = B */
   int *jp;REAL *e;
    for ( j = 0 ; j < S.N ; j++ )
    { int start,end;
      Y[j] = BB[S.LJA[S.LIJA[j]]];  /* for BK inner permutation */
      if ( S.psize[j] == FIRSTOFPAIR ) start = 2;
      else start = 1;
      end = S.LIA[j+1];
      for ( i=S.LIA[j]+start, e=S.LA+i , jp=S.LJA+S.LIJA[j]+start ;
                   i < end ; i++,e++,jp++ )
        BB[*jp] -= (*e)*Y[j];
    }

    /* solve D V = Y (will use Y to store V) */
    for ( j = 0 ; j < S.N ; j++ )
    { if ( S.psize[j] == ONEBYONE )
          Y[j] /= S.LA[S.LIA[j]];
      else if ( S.psize[j] == ZEROPIVOT ) Y[j] = 0.0;  
      else
      { REAL piv[2][2];
        REAL pinv[2][2];
        REAL det,yy;
        piv[0][0] = S.LA[S.LIA[j]];
        piv[0][1] = piv[1][0] = S.LA[S.LIA[j]+1];
        piv[1][1] = S.LA[S.LIA[j+1]];
        det = piv[0][0]*piv[1][1] - piv[0][1]*piv[1][0];
        pinv[0][0] = piv[1][1]/det;
        pinv[1][0] = pinv[0][1] = -piv[0][1]/det;
        pinv[1][1] = piv[0][0]/det;
        yy = Y[j]*pinv[0][0] + Y[j+1]*pinv[1][0];
        Y[j+1] = Y[j]*pinv[0][1] + Y[j+1]*pinv[1][1];
        Y[j] = yy;
        j++;
      }
    }

    /* solve U X = V */
    for ( j = S.N-1 ; j >= 0 ; j-- )
    { int start,end;
      if ( S.psize[j] == FIRSTOFPAIR ) start = 2;
      else start = 1;
      end = S.LIA[j+1];
      for ( k=S.LIA[j]+start, e=S.LA+k, jp=S.LJA+S.LIJA[j]+start  ;
           k < end ; k++,e++,jp++ )
         Y[j] -= (*e)*BB[*jp];
      BB[S.LJA[S.LIJA[j]]] = Y[j];
    }

    /* unpermute */
    for ( j = 0 ; j < S.N ; j++ )
      a[i][S.P[j]] = BB[j];
/* end of mindeg version */
#else
    ysmp_solve(&S,BB,BB);
    for ( j = 0 ; j < S.N ; j++ )
      a[i][j] = BB[j];
#endif
  }

  temp_free((char*)Y);
  temp_free((char*)BB);
  free_system(&S);
  return 1;
}

/*************************************************************************
  General sparse matrix construction scheme.  Does not assume symmetry.
  Usage: call sp_hash_init() to initialize,
              sp_hash() to add value,
              sp_hash_end() at end to sort and gather into linear system.

  Some benchmarks for using sparse_constraints vs dense constraints on
  tgrid4:               Seconds for 'g'
            Bodies     Sparse     Dense
              16
              64                  0.0141
             256                  0.9281
            1024        0.0953   58.36      memory 13MB (dense)
            4096        0.687
           16384        3.737
           65536       27.3
          262144      182.5      10^9       memory 555MB (sparse) 64GB(dense)
  So there is a five million fold speed-up for the largest sparse
  system my machine can handle.
  No jiggling, so only one pass through volume projection.  With jiggling
  and zero scale, single iteration shows volumes projected back to
  targets with maximum error 5e-7!

  Benchmarking on twointor, likewise no jiggle, m 0, g:
           Bodies        Sparse    Dense
              2          0.0048    0.0025
              4          0.0066    0.0040
             16          0.0181    0.0147
             64          0.0722    0.0713
            256          0.285     1.118
           1024          1.73     59.1        memory 45MB(dense)
           4096         16.3                  memory 116MB(sparse)
           8192        145.5                  memory 250MB(sparse)
  Note twointor.fe has more elements per body (13 v, 36 f) than
  tgrid4.fe (1 v, 1 f) and has a denser constraint matrix, being
  three dimensional instead of two dimensional. 

**************************************************************************/
#define SP_PRIME 99991
#define sp_hash(row,col)  (abs((row)*97+(col)*SP_PRIME))

/********************************************************************
* 
* function: sp_hash_init()
*
* purpose: Initialize hash table.
*/
void sp_hash_init ARGS2((S,size_estimate),
struct linsys *S,
int size_estimate) /* from sp_hash_end of previous invocation */
{ int i;
  int estimate = SDIM*(web.skel[VERTEX].max_ord+10);
   
  S->table_size = size_estimate > 0 ? 
       ( size_estimate > estimate ? estimate : size_estimate) : 
         estimate;
  S->max_fill = 4*S->table_size/5;  /* leave enough empty for decent hash */
  if ( !hessian_quiet_flag )
  { sprintf(msg,"Sparse init alloc: %d\n",S->table_size);
    outstring(msg);
  }
  S->hashcount = 0;
  if ( S->hashtable ) temp_free((char*)S->hashtable);
  S->hashtable = 
     (struct sp_entry *)temp_calloc(S->table_size,sizeof(struct sp_entry));
  for ( i = 0 ; i < S->table_size ; i++ ) S->hashtable[i].row = HASHEMPTY;
  S->hash_extraprobes = 0;
}

/********************************************************************
* 
* function: sp_hash_expand()
*
* purpose: Expands hash table
*/

void sp_hash_expand ARGS1((S),
struct linsys *S)
{ struct sp_entry *newtable,*oldtable;
  int i;
  struct sp_entry *e;
  int newsize; 
  int oldsize = S->table_size;

  if ( !S->hashtable ) sp_hash_init(S,0);
  newsize = S->table_size*2; 
  oldtable = S->hashtable;
  newtable = 
     (struct sp_entry *)temp_calloc(newsize,sizeof(struct sp_entry));
  for ( i = 0 ; i < newsize ; i++ ) newtable[i].row = HASHEMPTY;
  S->table_size =  newsize;
  S->max_fill = 4*S->table_size/5;
  S->hashtable = newtable;

  /* reinsert */
  for ( i = 0, e = oldtable ; i < oldsize ; i++,e++ )
     if ( e->row != HASHEMPTY )
     { int spot = sp_hash(e->row,e->col) % S->table_size;
       struct sp_entry *ee;
       for ( ee = S->hashtable + spot; ee->row != HASHEMPTY ; spot++ )
       { if ( spot == S->table_size ) spot = 0;
         ee = S->hashtable + spot;
       }
       *ee = *e;
     }
  temp_free((char*)oldtable);

  if ( !hessian_quiet_flag )
  { sprintf(msg,"Expanded hashtable size: %d.\n",S->table_size);
    outstring(msg);
  }
}

/********************************************************************
* 
* function: sp_hash_search()
*
* purpose: Finds existing entry or allocates entry.
*          Installs key values, and adds hessian value.
*/
void sp_hash_search ARGS4((S,row,col,value),
struct linsys *S,
int row, int col,  /* Note these are in reverse order as from hessian version */
REAL value)  /* value to add */
{
  struct sp_entry *e;
  int spot;


  if ( value == 0.0 ) return;   

  if ( S->hashcount >= S->max_fill ) 
     sp_hash_expand(S);

  /* search hash table */
  spot = sp_hash(row,col) % S->table_size;
  e = S->hashtable + spot;
  while ( e->row != HASHEMPTY )
  { if ( (e->row == row) && (e->col == col) )
    { e->value += value;
      return;
    }
    spot++;
    if ( spot >= S->table_size ) spot -= S->table_size;
    e = S->hashtable + spot;
    S->hash_extraprobes++;
  }
  /* if here, then have empty slot and need to insert */
  e->col = col; e->row = row;  S->hashcount++; 
  e->value = value;

}


/********************************************************************
* 
* function: sp_hash_end()
*
* purpose: Puts entries in sparse packed format; allocates own lists.
*          Create sorted list of sparse entries, row major order
*          Does 2-digit radix sort on row,col with count sort on each.
*          Deallocate hessian hash table.
*
* return:  Estimated size of hash table to use next time.
*/
int sp_hash_end ARGS4((S,rows,cols,index_start),
struct linsys *S,
int rows, /* number of rows */
int cols, /* number of columns */
int index_start) /* for 0 or 1 based indexing */
{ int i;
  struct sp_entry *e;
  int *counts;
  int *starts;
  int *spots; 
  int sum,oldsum;

  S->N = rows;
  S->maxN = rows > cols ? rows : cols;
  S->IA = (int *)temp_calloc(S->maxN+1,sizeof(int));
  counts = (int *)temp_calloc(3*cols+1,sizeof(int));
  starts = counts + cols;
  spots = starts + cols + 1;

  /* first stage: column binning */
  /* count entries in columns */
  for ( i = 0, e = S->hashtable ; i < S->table_size ; i++, e++ )
    if ( e->row != HASHEMPTY ) 
       counts[e->col]++;

  /* get starting points of each column */
  for ( i = 0, sum = 0 ; i < cols  ; i++ )
  { 
    spots[i] = starts[i] = sum;
    sum += counts[i];
  } 
  starts[cols] = sum;
  S->maxA = sum + S->maxN;  /* extra room for later expansion */
  S->JA = (int *)temp_calloc(S->maxA,sizeof(int));
  S->A = (REAL *)temp_calloc(S->maxA,sizeof(REAL));

  /* sort into column bins */
  for ( i = 0, e = S->hashtable ; i < S->table_size ; i++,e++ )
  { 
    struct sp_entry eorig;
    struct sp_entry etmp;
    if ( e->row == HASHEMPTY ) continue;
    if ( i < spots[e->col] ) continue; /* already in place */
    eorig = *e;
    e->row = HASHEMPTY;
    do
    { /* follow chain of replacements */
      int spot;
      spot = spots[eorig.col]++;
      etmp = S->hashtable[spot];
      S->hashtable[spot] = eorig;
      eorig = etmp;
    }
    while ( eorig.row != HASHEMPTY ); /* stop when get back to empty */ 
  }


  /* Second stage: row binning */
  /* count entries in row */

  for ( i = 0, e = S->hashtable ; i < sum ; i++, e++ )
     S->IA[e->row]++;

  /* get starting points of each row */
  for ( i = 0, sum = 0 ; i < rows  ; i++ )
  { oldsum = sum;
    sum += S->IA[i];
    S->IA[i] = oldsum;
  } 
  S->IA[rows] = sum;

  /* sort into row bins */
  for ( i = 0, e = S->hashtable ; i < sum ; i++,e++ )
  { 
    S->JA[S->IA[e->row]] = e->col;
    S->A[S->IA[e->row]] = e->value;
    S->IA[e->row]++;
  }
  /* put back IA */
  for ( i = rows ; i > 0 ; i-- )
    S->IA[i] = S->IA[i-1];
  S->IA[0] = 0;

  if ( index_start )
  { 
    for ( i = 0 ; i < S->IA[rows] ; i++ )
       S->JA[i] += index_start;
    for ( i = 0 ; i <= rows ; i++ )
       S->IA[i] += index_start;
  }

  temp_free((char*)counts);

  if ( !hessian_quiet_flag )
  { sprintf(msg,"Sparse entries: %d  Final hashtable size: %d\n",
        S->hashcount, S->table_size);
    outstring(msg);
    sprintf(msg,"Hash extra probes: %d\n",S->hash_extraprobes);
    outstring(msg);
  }
 
  temp_free((char*)S->hashtable);
  S->hashtable = NULL;

  return S->IA[S->N] + S->IA[S->N]/3;  /* estimate for next time */
}
/********************************************************************
     End sparse hash routines.
********************************************************************/

/*******************************************************************
     Using BLAS and LAPACK to factor large dense symmetric matrix
     for Hessian with lots of constraints.  To enable, -DBLAS in
     Makefile, and link with libblas and liblapack (ATLAS or whatever;
     Intel Math Kernel Library mkl_c.lib for Intel; needs mkl_sys.dll
     also).  Command line toggle blas_enable.  

     Usage: Incoming matrix should be block lower triangular, with
     blocksize BLAS_BLOCKSIZE.  Full blocks on diagonal, although
     only lower triangle has to filled on entry to LD_block_factor().
     Last odd rows also have to be full multiple of BLAS_BLOCKSIZE.
     Call LD_block_factor(H,N) first, then LD_block_solve(H,B,X,N)
     to solve for one right hand side.
********************************************************************/

/* Timings on block factoring, 1200 MHz dual Athlon MP.  Entries are Mflops
                              Matrix size
       block     512  1024  2048  4096  8192
        size   
         16      233   225   204   192   184
         32      340   360   366   371   373
         64      315   362   410   443   459
        128      211   283   340   377   399
        256      131   184   240   288   321
        512       99   122   164   213   254
  For comparison, my LDL in plain C runs at 32 Mflops, 
  and LAPACK DPPTRF at 66 Mflops.  Seems to use both CPUs
  for the DGEMM matrix multiplies, but only one for factoring.
*/

#ifdef BLAS

/* CPU clock timing */
__int32 LD_block_factor_elapsed_time[2];


int blocksize = BLAS_BLOCKSIZE;
static REAL workspace[BLAS_BLOCKSIZE][BLAS_BLOCKSIZE];
static REAL *work[BLAS_BLOCKSIZE];
static REAL workv[BLAS_BLOCKSIZE];
static int ipiv[BLAS_BLOCKSIZE]; /* for Bunch-Kaufman pivot info */

/*************************************************************************
*
* function: LD_block_factor()
*
* Purpose: BLAS and LAPACK used to block factor symmetric dense matrix.
*    Usage: Incoming matrix should be block lower triangular, with
*    blocksize BLAS_BLOCKSIZE.  Full blocks on diagonal, although
*    only lower triangle has to filled on entry to LD_block_factor().
*    Last odd rows also have to be full multiple of BLAS_BLOCKSIZE.
*    H will be overwritten.
* 
* Return value: >= 0  index of matrix
*               < 0   error
*/

/**********************************************************************
*
*  function: kb_DSYTRF()
*
*  purpose:  My replacement for buggy MKL code for DSYTRF
*            Does in-place Bunch-Kaufman factoring of lowet
*            triangular dense matrix (although full storage room).
*
*  returns: index of matrix.
*/

REAL BK_alpha = 0.6404;

void L_swap_columns(H,N,stride,i,r)
REAL *H;
int N;
int stride;
int i,r; /* the columns to swap */
{ int j;
  REAL tmp;
  REAL *ispot;
  REAL *rspot;

  if ( i == r ) return;
  if ( i > r ) { j = i; i = r; r = j; }

  for ( j=0, ispot=H+i*stride, rspot=H+r*stride ; j < i ; j++,ispot++,rspot++ )
  { tmp = *ispot; *ispot = *rspot; *rspot = tmp; }
  tmp = *ispot;
  *ispot = H[r*stride+r];
  H[r*stride+r] = tmp;
  for ( j = i+1, ispot += stride, rspot++ ; j < r ; j++,ispot+=stride,rspot++ )
  { tmp = *ispot; *ispot = *rspot; *rspot = tmp; } 
  for ( j = r+1, ispot+=stride,rspot+=stride; j < N ;
                                   j++,ispot+=stride,rspot+= stride)
  { tmp = *ispot; *ispot = *rspot; *rspot = tmp; } 

}

/************************************************************************
*
* function: kb_DSYTRF()
*
* purpose: Bunch-Kaufman factor dense matrix.
*          Uses lower triangle of full matrix.
*
* return: Index of matrix (number of negative entries on diagonal)
*/

int kb_DSYTRF(N,H,stride,ipiv,info)
int N;    /* matrix size */
REAL *H;  /* start of storage */
int stride;  /* entries between row starts */
int *ipiv;   /* returned pivot info; my own scheme: 
                  1-based indexing
                  abs value is what original column wound up here;
                  pos for 1x1, neg for first(!) of 2x2 pivots.
             */
int *info; /* 0 for success, -1 error, 1 singular */
{ int negs = 0;
  int i,j,k;

  *info = 0;  /* default is success */
  for ( i = 0 ; i < N ; i++ ) ipiv[i] = i+1; /* identity perm */

  for ( i = 0 ; i < N ; i++ )
  { REAL colmax,rcolmax;
    int r; /* row of max value */
    int pivotsize = 1;   
    REAL *ispot,*jspot,*kspot;

    /* first step is pivot selection */
    colmax = 0.0;
    r = -1;
    for ( j = i+1 ; j < N ; j++ )
    { REAL v = fabs(H[j*stride + i]);
      if ( v > colmax ) { colmax = v; r = j; }
    }
    if ( r == -1 ) /* zero column */
    { if ( H[i*stride+i] < 0.0 ) negs++;
      else if ( H[i*stride+i] == 0.0 ) *info = 1;
      goto skippivot;
    }
    if ( fabs(H[i*stride + i]) >= BK_alpha*colmax )
      goto dopivot;
    /* now need max in column r */
    rcolmax = colmax;
    for ( j = i+i ; j <= r ; j++ )
    { REAL v = fabs(H[r*stride+j]);
      if ( v > rcolmax ) rcolmax = v;
    }
    for ( j = r+1 ; j < N ; j++ )
    { REAL v = fabs(H[j*stride+r]);
      if ( v > rcolmax ) rcolmax = v;
    }
    if ( fabs(H[i*stride+i])*rcolmax >= BK_alpha*colmax*colmax )
      goto dopivot;
    if ( fabs(H[r*stride+r]) >= BK_alpha*rcolmax )
    { 
      L_swap_columns(H,N,stride,i,r);
      j = ipiv[i]; ipiv[i] = ipiv[r]; ipiv[r] = j;
    }
    else
    { pivotsize = 2; 
      L_swap_columns(H,N,stride,i+1,r);
      j = ipiv[i+1]; ipiv[i+1] = ipiv[r]; ipiv[r] = j;
    }
  
dopivot:
    if ( pivotsize == 1 )
    { REAL pivot;
      ispot = H+i*stride+i;
      pivot = *ispot;
      if ( pivot == 0.0 ) {*info = 1; goto skippivot;}
      if ( pivot < 0.0 ) negs++;
      pivot = 1/pivot; 
      for ( j = i+1  ; j < N ; j++ )
      { REAL x;
        jspot = H+j*stride+i;
        x = *jspot;
        *jspot *= pivot;
        kspot = ispot+stride;
        for ( k = i+1, jspot++ ; k <= j ; k++, jspot++, kspot+=stride )
          *jspot -= *kspot*x;
      }
    }
    else /* pivotsize 2 */
    { REAL det,p11,p21,p22,tmp;
      ispot = H+i*stride+i;
      p11 = *ispot; ispot += stride;
      p21 = *ispot; 
      p22 = ispot[1];
      det = p11*p22 - p21*p21;
      if ( (det > 0.0) && (p11+p22 < 0.0) ) negs += 2;
      else if ( det < 0.0 ) negs++;
      tmp = p11;
      p11 = p22/det;
      p22 = tmp/det;
      p21 = -p21/det;
      for ( j = i+2; j < N ; j++ )
      { REAL x1,x2;
        jspot = H+j*stride+i;
        x1 = jspot[0]; x2 = jspot[1];
        jspot[0] = p11*x1 + p21*x2;
        jspot[1] = p21*x1 + p22*x2;
        kspot = ispot+stride;
        for ( k = i+2, jspot+=2 ; k <= j ; k++, jspot++, kspot+=stride )
          *jspot -= kspot[0]*x1 + kspot[1]*x2;
      }
      ipiv[i] = -ipiv[i];
      i++;
    }
skippivot: ;
  }
  return negs;
}
/**********************************************************************
*
* function: kb_DSYTRS()
*
* purpose: solve for single rhs using factored matrix.
*/

void kb_DSYTRS(N,LD,stride,ipiv,B,work,info)
int N;
REAL *LD;  /* from kb_DSYTRF() */
int stride;
int *ipiv; /* ipiv[i] is what column wound up in column i, 1-based indexing */
REAL *B;  /* rhs,solution */
REAL *work; /* must be size N */
int *info;
{ int i,j;
  REAL *X = work;
  REAL *ispot,*jspot;

  /* permute */
  for ( i = 0 ; i < N ; i++ ) work[i] = B[abs(ipiv[i])-1];

  /* solve left factor */
  for ( i = 0, ispot = LD ; i < N ; i++, ispot += stride )
  { if ( ipiv[i] > 0 )   /* 1x1 pivot */
    { for ( j = 0, jspot = ispot ; j < i ; j++,jspot++ )
        X[i] -= (*jspot)*X[j];
    }
    else /* 2x2 pivot */
    { for ( j = 0, jspot = ispot ; j < i ; j++,jspot++ )
      { X[i] -= (*jspot)*X[j];
        X[i+1] -= jspot[stride]*X[j];
      }
      i++; ispot += stride;
    }
  }
  for ( i = 0, ispot = LD ; i < N ; i++, ispot+=stride+1 )
  { if ( ipiv[i] > 0 )   /* 1x1 pivot */
     X[i] /= *ispot;
    else /* 2x2 pivot */
    { REAL p11 = *ispot; 
      REAL p21 = ispot[stride];
      REAL p22 = ispot[stride+1];
      REAL det = p11*p22 - p21*p21;
      REAL x1 = X[i], x2 = X[i+1];
      X[i] = (p22*x1 - p21*x2)/det;
      X[i+1] = (-p21*x1 + p11*x2)/det;
      i++; ispot += stride+1;
    }
  }
  for ( i = N-1 ; i >= 0 ; i-- )
  { if ( ipiv[i] > 0 )   /* 1x1 pivot */
      for ( j = i+1, jspot = ispot ; j < N ; j++ )
        X[i] -= LD[j*stride+i]*X[j];
    else /* 2x2 pivot */
    { for ( j = i+2 ; j < N ; j++ )
      { X[i] -= LD[j*stride+i]*X[j];
      }
    }
  }
  /* permute */
  for ( i = 0 ; i < N ; i++ ) 
    B[abs(ipiv[i])-1] = work[i];

}

/************************************************************************
*
* function: kb_DSYTRI()
*
* purpose: convert factored matrix to inverse.
*
*/

void kb_DSYTRI(N,H,stride,ipiv,info)
int N;
REAL *H;  /* from kb_DSYTRF() */
int stride;
int *ipiv; /* ipiv[i] is what column wound up in column i, 1-based indexing */
int *info;
{ int i,j,k;
  REAL *ispot,sum,tmp;

  /* convert L to inverse */
  for ( i = 0 ; i < N ; i++ )
  { int jstart = (ipiv[i] < 0) ? i+2 : i+1;
    for ( j = jstart ; j < N ; j++ )
    { int endk = ((i>0) && (ipiv[i-1] < 0)) ? i-1 : i;
      for ( k = 0 ; k < endk ; k++ )
        H[j*stride+k] -= H[i*stride+k]*H[j*stride+i];
      H[j*stride+i] = -H[j*stride+i];
    }
  }

  /* convert D to inverse */
  for ( i = 0 ; i < N ; i++ )
  { ispot = H + i*stride + i;
    if ( ipiv[i] > 0 )
      *ispot = 1/(*ispot);
    else /* 2x2 pivot */
    { REAL p11 = *ispot; 
      REAL p21 = ispot[stride];
      REAL p22 = ispot[stride+1];
      REAL det = p11*p22 - p21*p21;
      H[i*stride+i] = p22/det;
      H[(i+1)*stride+i] = -p21/det;
      H[(i+1)*stride+i+1] = p11/det;
      i++;
    }
  }

  /* multiply out L^TD into upper triangle */
  for ( i = 0 ; i < N ; i++ )
  { if ( ipiv[i] > 0 )
    { REAL p = H[i*stride+i];
      for ( k = 0 ; k < i ; k++ )
        H[k*stride+i] = p*H[i*stride+k];
    }
    else
    { REAL p11 = H[i*stride+i];
      REAL p21 = H[(i+1)*stride+i];
      REAL p22 = H[(i+1)*stride+i+1];
      for ( k = 0 ; k < i ; k++ )
      { H[k*stride+i] = p11*H[i*stride+k] + p21*H[(i+1)*stride+k];
        H[k*stride+i+1] = p21*H[i*stride+k] + p22*H[(i+1)*stride+k];
      }
      H[i*stride+i+1] = H[(i+1)*stride+i];
      i++;
    }
  }

  /* multiply the two triangles L^TD*L into the lower */
  for ( i = 0 ; i < N ; i++ )  
  {
    if ( ipiv[i] > 0 )  /* 1x1 pivot */
    {
      for ( j = 0 ; j < i ; j++ )
      { 
        for ( k = i, sum = 0.0 ; k < N ; k++ )
          sum += H[i*stride+k]*H[k*stride+j];
        H[i*stride+j] = sum;
      }
      for ( j = i+1, sum = 0.0 ; j < N ; j++ )
       sum += H[i*stride+j]*H[j*stride+i];
      H[i*stride+i] += sum;
    }
    else /* 2x2 pivot */
    {
      for ( j = 0 ; j < i ; j++ )
      { REAL tmp; 
        for ( k = i, sum = 0.0 ; k < N ; k++ )
          sum += H[i*stride+k]*H[k*stride+j];
        tmp = sum;
        for ( k = i, sum = 0.0 ; k < N ; k++ )
          sum += H[(i+1)*stride+k]*H[k*stride+j];
        H[(i+1)*stride+j] = sum;
        H[i*stride+j] = tmp;
      }

      /* diagonal */
      for ( j = i+2, sum = 0.0 ; j < N ; j++ )
       sum += H[(i+1)*stride+j]*H[j*stride+i];
      tmp = sum; 
      for ( j = i+2, sum = 0.0 ; j < N ; j++ )
       sum += H[i*stride+j]*H[j*stride+i];
      H[i*stride+i] += sum;
      for ( j = i+2, sum = 0.0 ; j < N ; j++ )
       sum += H[(i+1)*stride+j]*H[j*stride+(i+1)];
      H[(i+1)*stride+(i+1)] += sum;
      H[(i+1)*stride+i] += tmp;
     
      i++;
    }
  }

  /* permute, using upper triangle as work space */
  /* diagonal, top row as work space */

  for ( i = 0 ; i < N ; i++ )
    H[0*stride+abs(ipiv[i])-1] = H[i*stride+i];
  for ( i = 1 ; i < N ; i++ )
    H[i*stride+i] = H[0*stride+i];
  /* now the main part */
  for ( i = 1 ; i < N ; i++ )
  { int fromi = abs(ipiv[i])-1;
    for ( j = 0 ; j < i ; j++ )
    { int fromj = abs(ipiv[j])-1;
      if ( fromi > fromj )
        H[fromj*stride+fromi] = H[i*stride+j];
      else
        H[fromi*stride+fromj] = H[i*stride+j];
    }
  }

  /* copy upper to lower */
  for ( i = 1 ; i < N ; i++ )
    for ( j = 0 ; j < i ; j++ )
      H[i*stride+j] = H[j*stride+i];
}

/*************************************************************************
*
* function: LD_block_factor()
*
* purpose: Do LDL factoring in block form to take advantage of DGEMM.
*
* return: Index.
*/

/* returns index */
int LD_block_factor( H, N )  /* actually, D^-1 on diagonal */
REAL **H;  /* lower block triangular, row by row */
           /* i.e. each consecutive BLOCKSIZE rows same length, */
           /* in multiples of BLOCKSIZE */
int N;     /* size */
{ int i,j,k;
  int info;
  int rowcount; /* rows in block; less than blocksize for last block */
  int negs = 0;
 
  for ( i = 0 ; i < N ; i += blocksize )   /* pivot (i,i) */
  { /* get pivot inverse in place */
    int stride = i+blocksize;
    rowcount = (N - i > blocksize) ? blocksize : N - i;

    /* Inverse of diagonal block */
    /* Bunch-Kaufman factor */
    negs += kb_DSYTRF(rowcount,&H[i][i],stride,ipiv,&info);
    /* now the inverse */
    kb_DSYTRI(rowcount,&H[i][i],stride,ipiv,&info);

    /* update row */
    for ( j = i+blocksize ; j < N ; j += blocksize )  /* row j */
    { REAL x = H[j][i];
      char transa = 'T';
      char transb = 'N';
      REAL alpha = -1.0;
      REAL beta  =  1.0;
      int stridea;
      int strideb;
      int stridec;
      char side = 'L';
      int ii,jj;
      char uplo;

      rowcount = (N - j > blocksize) ? blocksize : N - j;

      for ( k = i+blocksize ; k < j ; k += blocksize ) /* col k */
      { stridea = k+blocksize;
        strideb = j+blocksize;
        stridec = j+blocksize;
        DGEMM(&transa,&transb,&blocksize,&rowcount,&blocksize,&alpha,
           &H[k][i],&stridea,&H[j][i],&strideb,&beta,&H[j][k],&stridec);
      }
      /* H[j][i] = x*pivot;  temp store result in work */
      alpha =  1.0;
      beta  =  0.0;
      stridea = i+blocksize;
      strideb = j+blocksize;
      stridec = blocksize;
      alpha = 1.0; beta = 0.0;
      side = 'L';  /* symmetric matrix on the left */
      uplo = 'U';  /* transposed since Fortran */
      DSYMM(&side,&uplo,&blocksize,&rowcount,&alpha,&H[i][i],&stridea,
          &H[j][i],&strideb,&beta,&workspace[0][0],&stridec);

      /* H[j][j] -= x*pivot*x; */
      transa = 'T';
      transb = 'N';
      alpha = -1.0;
      beta  =  1.0;
      stridea = j+blocksize;
      strideb = blocksize;
      stridec = j+blocksize;
      DGEMM(&transa,&transb,&rowcount,&rowcount,&blocksize,&alpha,
         &H[j][i],&stridea,&workspace[0][0],&strideb,&beta,&H[j][j],&stridec);
      /* copy workspace back to H[j][i] */
      for ( jj = 0 ; jj < rowcount ; jj++ )
        for ( ii = 0 ; ii < blocksize ; ii++ )
          H[j+jj][i+ii] = workspace[jj][ii];
    }
  }
  return negs;
}

/**************************************************************************
*
* function: LD_block_solve()
*
* purpose: Solve for single right hand side using factoring produced
*          by LD_block_factor().
*/

void LD_block_solve(LD,B,X,N)
REAL **LD;  /* from LD_block_factor(), inverse blocks on diagonal */
REAL *B;  /* rhs */
REAL *X;  /* solution */
int N;  /* size of system */
{ int i,j;
  REAL alpha = 0.0;
  REAL beta = 0.0;
  int stridea = 1;
  int incx=1,incy=1;
  char trans = 'N';
  int rowcount;  /* rows in block; less than blocksize at end */
 
  for ( i = 0 ; i < N ; i++ ) X[i] = B[i];

  for ( i = 0 ; i < N ; i += blocksize )
  { 
    rowcount = (N - i > blocksize) ? blocksize : N - i;
    for ( j = 0 ; j < i ; j += blocksize )
    { /*  X[i] -= LD[i][j]*X[j]; */
      alpha = -1.0;
      beta = 1.0;
      stridea = i+blocksize;
      incx = 1;
      incy = 1;
      trans = 'T';
      DGEMV(&trans,&blocksize,&rowcount,&alpha,&LD[i][j],&stridea,
         &X[j],&incx,&beta,&X[i],&incy);
    }
  }
  for ( i = 0 ; i < N ; i += blocksize )
  { /* X[i] /= LD[i][i]; */
    char uplo = 'U';
    rowcount = (N - i > blocksize) ? blocksize : N - i;
    alpha = 1.0;
    beta = 0.0;
    stridea = i+blocksize;
    incx = 1;
    incy = 1;
    DSYMV(&uplo,&rowcount,&alpha,&LD[i][i],&stridea,&X[i],&incx,
       &beta,&workv[0],&incy);  /* need separate destination */
    for ( j = 0 ; j < rowcount ; j++ )
       X[i+j] = workv[j];
  }

  for ( i = N-rowcount ; i >= 0 ; i -= blocksize )
  { 
    for ( j = i+blocksize ; j < N ; j += blocksize )
    { /*  X[i] -= LD[j][i]*X[j]; */
      rowcount = (N - j > blocksize) ? blocksize : N - j;
      alpha = -1.0;
      beta = 1.0;
      stridea = j+blocksize;
      incx = 1;
      incy = 1;
      trans = 'N';
      DGEMV(&trans,&blocksize,&rowcount,&alpha,&LD[j][i],&stridea,
         &X[j],&incx,&beta,&X[i],&incy);
    }
  }
}


#endif
/**********************************************************************/

/* Now some Bezier and Lagrange stuff for refining and graphics
   subdivision
*/
 
/**********************************************************************
*
* function: bezier_eval()
*
* purpose: Evaluation of multi-dimensional simplicial Bezier basis
*          function at given point.
*/
REAL bezier_eval ARGS4((order,dim,inx,x),
int order, /* Lagrange order */
int dim,  /* simplicial dimension */
int *inx, /* which basis function, barycentric, sums to order  */
REAL *x)   /* barycentric coords of target, sums to 1 */
{ REAL prod, prodpart[MAXCOORD];
  int i,j,k,mm;
  for ( k = 0 ; k <= dim ; k++ ) prodpart[k] = 0.0;
    for ( i = 0, prod = 1.0, mm = 1 ; i <= dim ; i++ )
      for ( j = 0 ; j < inx[i] ; j++, mm++ )
      { for ( k = 0 ; k <= dim ; k++ )
        { prodpart[k] *= mm*x[i]/(j+1);
          if ( i == k ) prodpart[k] += prod*mm/(j+1.);
        }
        prod *= mm*x[i]/(j+1.);
      }
  return prod;
}

/***************************************************************************
*
* function: bezier_eval_1d()
*
* purpose: Get point on curve at given parameter.
*/
void bezier_eval_1d ARGS5((order,sdim,p,ctrl,dest),
int order,
int sdim, /* space dimension*/
REAL p,   /* parameter, 0 to 1 */
REAL **ctrl,  /* control point coordinates */
REAL *dest)  /* output point */
{ int inx[2];
  REAL t[2];
  int i,k;

  t[0] = p; t[1] = 1 - p;
  for ( k = 0 ; k < sdim ; k++ ) dest[k] = 0.0;
  for ( i = 0 ; i <= order ; i++ )
  { REAL bezval;
    inx[0] = order-i;
    inx[1] = i;
    bezval = bezier_eval(order,1,inx,t);
    for ( k = 0 ; k < sdim ; k++ )
      dest[k] += bezval*ctrl[i][k];
  }

}

/***************************************************************************
*
* function: bezier_convert_1D_init()
*
* purpose: initialize matrix for converting from points on curve to
*          control points.
*
*/

void bezier_convert_1D_init ARGS1((order),
int order)
{ int i,j;
  REAL x[2];  /* barycentric coords of target point */
  int inx[2]; /* barycentric index of basis function */
  REAL **a = dmatrix(0,order,0,order); 
  REAL **b = dmatrix(0,order,0,order); 

  if ( bezier1invert[order] ) return;

  /* fill in forward matrix */
  for ( i = 0 ; i <= order ; i++ )  /* point on curve */
  { x[0] = (order-i)/(REAL)order;
    x[1] = i/(REAL)order;
    for ( j = 0 ; j <= order ; j++ ) /* control point */
    { inx[0] = order-j;
      inx[1] = j;
      a[i][j] = b[i][j] = bezier_eval(order,1,inx,x);
    }
  } 
  mat_inv(a,order+1);
  bezier1invert[order] = a;
  bezier1revert[order] = b;
}

/***************************************************************************
*
* function: bezier_convert_2D_init()
*
* purpose: initialize matrix for converting from points on surface to
*          control points.
*
*/

void bezier_convert_2D_init ARGS1((order),
int order)
{ int i,j,k,m,kk,mm;
  REAL x[3];  /* barycentric coords of target point */
  int inx[3]; /* barycentric index of basis function */
  int numpts = ((order+1)*(order+2))/2;
  REAL **a = dmatrix(0,numpts-1,0,numpts-1); 
  REAL **b = dmatrix(0,numpts-1,0,numpts-1); 

  if ( bezier2invert[order] ) return;

  /* fill in forward matrix */
  for ( i = 0, kk=0 ; i <= order ; i++ )  /* point on curve */
    for ( k = 0 ; i+k <= order ; k++,kk++ )  /* point on curve */
    { x[0] = (order-i-k)/(REAL)order;
      x[1] = i/(REAL)order;
      x[2] = k/(REAL)order;
      for ( j = 0, mm=0 ; j <= order ; j++ ) /* control point */
        for ( m = 0 ; m+j <= order ; m++,mm++ ) /* control point */
        { inx[0] = order-j-m;
          inx[1] = j;
          inx[2] = m;
          a[kk][mm] = b[kk][mm] = bezier_eval(order,2,inx,x);
        }
  } 
  mat_inv(a,numpts);
  bezier2invert[order] = a;
  bezier2revert[order] = b;

}

/*************************************************************************
*
* function: bezier_trans_1d()
*
* purpose: Create transition matrix from old order bezier control points
*          to new.  Goes through intermediary of curve points.
*/

void bezier_trans_1d ARGS3((old_order,new_order,trans),
int old_order,
int new_order,
REAL **trans) /* trans[new][old] */
{ int i,k;
  int inx[2];
  REAL t[2];
  MAT2D(x,MAXLAGRANGE+1,MAXLAGRANGE+1);  /* old control to curve points */

  if ( !bezier1invert[new_order] )
    bezier_convert_1D_init(new_order);

  /* old control to curve point matrix */
  for ( i = 0 ; i <= new_order ; i++ )
  { 
    t[0] = (new_order - i)/(REAL)new_order;
    t[1] = i/(REAL)new_order;
    for ( k = 0 ; k <= old_order ; k++ )
    { inx[0] = old_order-k; inx[1] = k;
      x[i][k] = bezier_eval(old_order,1,inx,t);
    }
  }
  
  /* now get final transition matrix */
  mat_mult(bezier1invert[new_order],x,trans,new_order+1,new_order+1,old_order+1);
}

/*************************************************************************
*
* function: bezier_trans_2d()
*
* purpose: Create transition matrix from old order bezier control points
*          to new.  Goes through intermediary of surface points.
*/

void bezier_trans_2d ARGS3((old_order,new_order,trans),
int old_order,
int new_order,
REAL **trans) /* trans[new][old] */
{ int i,j,k,m,jj,kk;
  int inx[3];
  REAL t[3];
  int old_numpts = ((old_order+1)*(old_order+2))/2;
  int new_numpts = ((new_order+1)*(new_order+2))/2;
  REAL **x;

  /* old control to surface points */
  x = dmatrix(0,new_numpts-1,0,old_numpts-1);

  if ( !bezier2invert[new_order] )
    bezier_convert_2D_init(new_order);

  /* old control to surface point matrix */
  for ( j = 0, jj=0 ; j <= new_order ; j++ )
    for ( i = 0 ; i+j <= new_order ; i++,jj++ )
    { 
      t[0] = (new_order - i - j)/(REAL)new_order;
      t[1] = j/(REAL)new_order;
      t[2] = i/(REAL)new_order;
      for ( k = 0, kk = 0 ; k <= old_order ; k++ )
        for ( m = 0 ; m+k <= old_order ; m++, kk++ )
        { inx[0] = old_order-k-m; inx[1] = k; inx[2] = m;
          x[jj][kk] = bezier_eval(old_order,2,inx,t);
        }
    }
  
  /* now get final transition matrix */
  mat_mult(bezier2invert[new_order],x,trans,new_numpts,new_numpts,old_numpts);
}

/***************************************************************************
*
* function:  bezier_refine_1d_init()
*
* purpose: set up matrices for calculating new control points for 
*          refined edge.
*/

void bezier_refine_1d_init ARGS1((order),
int order)
{ int i,k;
  int inx[2];
  REAL t[2];
  MAT2D(x,2*MAXLAGRANGE+1,MAXLAGRANGE+1);  /* old control to curve points */

  if ( bezier_refine_1d[order] ) return;
  bezier_refine_1d[order] = dmatrix(0,2*order,0,order);

  if ( !bezier1invert[order] )
    bezier_convert_1D_init(order);

  /* old control to curve point matrix */
  for ( i = 0 ; i <= 2*order ; i++ )
  { 
    t[0] = (2*order - i)/(REAL)(2*order);
    t[1] = i/(REAL)(2*order);
    for ( k = 0 ; k <= order ; k++ )
    { inx[0] = order-k; inx[1] = k;
      x[i][k] = bezier_eval(order,1,inx,t);
    }
  }
  
  /* now get final transition matrix */
  mat_mult(bezier1invert[order],x,bezier_refine_1d[order],
    order+1,order+1,order+1);
  mat_mult(bezier1invert[order],x+order,bezier_refine_1d[order]+order,
    order+1,order+1,order+1);
}

/***************************************************************************
*
* function:  bezier_refine_2d_init()
*
* purpose: set up matrices for calculating new control points for 
*          refined facet.
*/

void bezier_refine_2d_init ARGS1((order),
int order)
{ int i,j,k,m,kk,mm,jj;
  int inx[3];
  REAL t[3];
  int old_numpts = ((order+1)*(order+2))/2;
  int new_numpts = (2*order+1)*(order+1);
  REAL ***x = dmatrix3(2*order+1,2*order+1,old_numpts);
              /* old control to curve points */
  REAL **z,**w; /* for pieces of whole matrix */

  if ( bezier_refine_2d[order] ) return;
  bezier_refine_2d[order] = dmatrix(0,new_numpts,0,old_numpts);

  if ( !bezier2invert[order] )
    bezier_convert_2D_init(order);

  /* old control to curve point matrix */
  for ( j = 0 ; j <= 2*order ; j++ )
    for ( i = 0 ; i+j <= 2*order ; i++ )
    { 
      t[0] = (2*order - i - j)/(REAL)(2*order);
      t[1] = j/(REAL)(2*order);
      t[2] = i/(REAL)(2*order);
      for ( k = 0, kk = 0 ; k <= order ; k++ )
        for ( m = 0 ; m+k <= order ; m++,kk++ )
        { inx[0] = order-k-m; inx[1] = k; inx[2] = m;
          x[i][j][kk] = bezier_eval(order,2,inx,t);
        }
    }
  
  /* now get final transition matrix in four sections */
  z =  dmatrix(0,old_numpts-1,0,old_numpts-1);
  w =  dmatrix(0,old_numpts-1,0,old_numpts-1);
  /* lower left */
  for ( j = 0, jj = 0 ; j <= order ; j++ )
    for ( i = 0 ; i+j <= order ; i++, jj++ )
      for ( kk = 0 ; kk < old_numpts ; kk++ )
        z[jj][kk] = x[i][j][kk];
  mat_mult(bezier2invert[order],z,w,old_numpts,old_numpts,old_numpts);
  for ( j = 0, jj = 0, mm = 0 ; j <= order ; j++, mm += order )
    for ( i = 0 ; i+j <= order ; i++, jj++, mm++ )
      for ( kk = 0 ; kk < old_numpts ; kk++ )
        bezier_refine_2d[order][mm][kk] = w[jj][kk];

  /* lower right */
  for ( j = 0, jj = 0 ; j <= order ; j++ )
    for ( i = 0 ; i+j <= order ; i++, jj++ )
      for ( kk = 0 ; kk < old_numpts ; kk++ )
        z[jj][kk] = x[i+order][j][kk];
  mat_mult(bezier2invert[order],z,w,old_numpts,old_numpts,old_numpts);
  for ( j = 0, jj = 0, mm = order ; j <= order ; j++, mm += order )
    for ( i = 0 ; i+j <= order ; i++, jj++, mm++ )
      for ( kk = 0 ; kk < old_numpts ; kk++ )
        bezier_refine_2d[order][mm][kk] = w[jj][kk];

  /* upper left */
  for ( j = 0, jj = 0 ; j <= order ; j++ )
    for ( i = 0 ; i+j <= order ; i++, jj++ )
      for ( kk = 0 ; kk < old_numpts ; kk++ )
        z[jj][kk] = x[i][j+order][kk];
  mat_mult(bezier2invert[order],z,w,old_numpts,old_numpts,old_numpts);
  for ( j = 0, jj = 0, mm = 3*((order+1)*order)/2 ; j <= order ; j++ )
    for ( i = 0 ; i+j <= order ; i++, jj++, mm++ )
      for ( kk = 0 ; kk < old_numpts ; kk++ )
        bezier_refine_2d[order][mm][kk] = w[jj][kk];

  /* middle */
  for ( j = order, jj = 0 ; j >= 0 ; j-- )
    for ( i = order ; i+j >= order ; i--, jj++ )
      for ( kk = 0 ; kk < old_numpts ; kk++ )
        z[jj][kk] = x[i][j][kk];
  mat_mult(bezier2invert[order],z,w,old_numpts,old_numpts,old_numpts);
  for ( j=order, jj=0, mm = ((3*order+2)*(order+1))/2-1 ; j >= 0 ;
                   j--,mm -= 2*(order-j)-1 )
    for ( i = order ; i+j >= order ; i--, jj++, mm-- )
      for ( kk = 0 ; kk < old_numpts ; kk++ )
         bezier_refine_2d[order][mm][kk] = w[jj][kk];


  free_matrix3(x);
  free_matrix(z);
  free_matrix(w);
}

/***********************************************************************
*
* function: lagrange_eval_1d()
*
* purpose: Evaluate one curve point using Lagrange interpolation.
*/

void lagrange_eval_1d ARGS5((order,sdim,t,ctrl,dest),
int order,
int sdim, /* space dimension*/
REAL t,   /* parameter, 0 to 1 */
REAL **ctrl,  /* control point coordinates */
REAL *dest)  /* output point */
{
  int i,k,m;

  for ( k = 0 ; k < sdim ; k++ )
    dest[k] = 0.0;
  for ( i = 0 ; i <= order ;  i++ )
  {
    REAL p=1.0;
    for ( m = 0 ; m <= order ; m++ )
    { if ( m == i ) continue;
      p *= (t*order - m)/(i - m);
    }
    for ( k = 0 ; k < sdim ; k++ )
      dest[k] += p*ctrl[i][k];
  }
}


/***********************************************************************
*
* function: lagrange_eval_2d()
*
* purpose: Evaluate one surface point using Lagrange 2D interpolation.
*/

void lagrange_eval_2d ARGS5((order,sdim,t,ctrl,dest),
int order,
int sdim, /* space dimension*/
REAL *t,   /* parameters, 0 to 1 */
REAL **ctrl,  /* control point coordinates, in linear order */
REAL *dest)  /* output point */
{
  int i,j,k,m,jj;
  
  for ( m = 0 ; m < sdim ; m++ )
    dest[m] = 0.0;
  for ( j = 0, jj = 0 ; j <= order ;  j++ )
  {
    for ( i = 0 ; i+j <= order ; i++,jj++ )
    { REAL p=1.0;

      k = order-i-j;
      for ( m = 0 ; m < i ; m++ )
        p *= (t[0]*order - m)/(i - m);
      for ( m = 0 ; m < j ; m++ )
        p *= (t[1]*order - m)/(j - m);
      for ( m = 0 ; m < k ; m++ )
        p *= ((1-t[0]-t[1])*order - m)/(k - m);

      for ( m = 0 ; m < sdim ; m++ )
        dest[m] += p*ctrl[jj][m];
    }
  }
}


/***************************************************************************
*
* function: bezier_eval_2d()
*
* purpose: Get point on surface at given parameters.
*/
void bezier_eval_2d ARGS5((order,sdim,p,ctrl,dest),
int order,
int sdim, /* space dimension*/
REAL *p,   /* two parameters, 0 to 1 barycentric */
REAL **ctrl,  /* control point coordinates, in linear order */
REAL *dest)  /* output point */
{ int inx[3];
  REAL t[3];
  int i,j,k,jj;

  t[0] = p[0]; t[1] = p[1]; t[2] = 1.0 - t[0] - t[1];
  for ( k = 0 ; k < sdim ; k++ ) dest[k] = 0.0;
  for ( j = 0, jj = 0  ; j <= order ; j++ )
    for ( i = 0 ; i+j <= order ; i++,jj++ )
    { REAL bezval;
      inx[0] = order-i-j;
      inx[1] = j;
      inx[2] = i;
      bezval = bezier_eval(order,2,inx,t);
      for ( k = 0 ; k < sdim ; k++ )
        dest[k] += bezval*ctrl[jj][k];
    }

}

/**************************************************************************
*
* function: tetra_vol()
*
* purpose: calculate volume of a tetrahedron in 3D from its four vertices.
*/

REAL tetra_vol ARGS4((a,b,c,d),
REAL *a, REAL *b, REAL *c, REAL *d)
{ REAL bb[3],cc[3],dd[3];
  REAL vol;
  int i;
  for ( i = 0 ; i < 3 ; i++ )
  { bb[i] = b[i] - a[i];
    cc[i] = c[i] - a[i];
    dd[i] = d[i] - a[i];
  }
  vol = (bb[0]*(cc[1]*dd[2] - cc[2]*dd[1]) + bb[1]*(cc[2]*dd[0] - cc[0]*dd[2])
      + bb[2]*(cc[0]*dd[1] - cc[1]*dd[0]))/6;
  return vol;
}


/****************************************************************************
*
* function: matrix_multiply_command()
*
* purpose: implements the matrix_multiply user command.
*/
void matrix_multiply_command ARGS6((a,b,c,adata,bdata,cdata),
struct array *a, struct array *b,struct array *c, /* array infofor the matrices */
REAL *adata, REAL *bdata, REAL *cdata ) /* data addresses */
{
  int i,j,k;

  if ( (a->dim == 2) && (b->dim == 2) )
  {
    if ( c->dim != 2 )
      kb_error(3798,"matrix_multiply third array is not two-dimensional.\n",
        RECOVERABLE); 
  
    if ( a->sizes[1] != b->sizes[0] )
      kb_error(4012,
       "matrix_multiply: sizes of first and second matrices disagree.\n",
         RECOVERABLE);
    if ( a->sizes[0] != c->sizes[0] )
      kb_error(3217,
       "matrix_multiply: sizes of first and third matrices disagree.\n",
         RECOVERABLE);
    if ( b->sizes[1] != c->sizes[1] )
      kb_error(3862,
       "matrix_multiply: sizes of second and third matrices disagree.\n",
         RECOVERABLE);
  
    /* finally ready to roll */
    for ( i = 0 ; i < a->sizes[0] ; i++ )
      for ( j = 0 ; j < b->sizes[1] ; j++ )
      { REAL sum = 0.0;
        for ( k = 0 ; k < a->sizes[1] ; k++ )
          sum += adata[i*a->sizes[1]+k]*bdata[k*b->sizes[1]+j];
        cdata[i*c->sizes[1]+j] = sum;
      }
    return;
  } /* end 2D by 2D */


  if ( (a->dim == 1) && (b->dim == 2) )
  {
    if ( c->dim != 1 )
      kb_error(4013,"matrix_multiply third array is not one-dimensional.\n",
        RECOVERABLE); 
  
    if ( a->sizes[0] != b->sizes[0] )
      kb_error(3860,
       "matrix_multiply: sizes of first and second matrices disagree.\n",
         RECOVERABLE);
    if ( b->sizes[1] != c->sizes[0] )
      kb_error(4010,
       "matrix_multiply: sizes of second and third matrices disagree.\n",
         RECOVERABLE);
  
    /* finally ready to roll */
      for ( j = 0 ; j < b->sizes[1] ; j++ )
      { REAL sum = 0.0;
        for ( k = 0 ; k < a->sizes[0] ; k++ )
          sum += adata[k]*bdata[k*b->sizes[1]+j];
        cdata[j] = sum;
      }

      return;
    } /* end 1D by 2D */

  if ( (a->dim == 2) && (b->dim == 1) )
  {
    if ( c->dim != 1 )
      kb_error(4014,"matrix_multiply third array is not one-dimensional.\n",
        RECOVERABLE); 
  
    if ( a->sizes[1] != b->sizes[0] )
      kb_error(4011,
       "matrix_multiply: sizes of first and second matrices disagree.\n",
         RECOVERABLE);
  
    /* finally ready to roll */
    for ( i = 0 ; i < a->sizes[0] ; i++ )
      { REAL sum = 0.0;
        for ( k = 0 ; k < a->sizes[1] ; k++ )
          sum += adata[i*a->sizes[1]+k]*bdata[k];
        cdata[i] = sum;
      }
     return;
    } /* end 2D by 1D */


   kb_error(3869,"matrix_multiply: illegal matrix dimensions\n",RECOVERABLE);
}


/****************************************************************************
*
* function: matrix_inverse_command()
*
* purpose: implements the matrix_inverse user command.
*/
int matrix_inverse_command ARGS4((a,b,adata,bdata),
struct array *a,struct array *b,/* variable id numbers for the matrices */
REAL *adata,REAL *bdata  /* source and destination matrices */
) 
{ 
  int i;
  int retval = 1;  /* 1 for success, 0 for singular */
  REAL **bptrs;
 

  if ( a->sizes[0] != a->sizes[1] )
    kb_error(3218, "matrix_inverse: first matrix is not square.\n",
       RECOVERABLE);
  if ( b->sizes[0] != b->sizes[1] )
    kb_error(3872, "matrix_inverse: second matrix is not square.\n",
       RECOVERABLE);
  if ( a->sizes[0] != b->sizes[0] )
    kb_error(3873,
     "matrix_inverse: sizes of first and second matrices disagree.\n",
       RECOVERABLE);

  if ( bdata != adata )
    for ( i = 0 ; i < a->datacount ; i++ )
      bdata[i] = adata[i];

  /* set up row pointers in allocated space after data */
  bptrs = (REAL**)(bdata + b->datacount);
  for ( i = 0 ; i < b->sizes[0] ; i++ )
    bptrs[i] = bdata + i*b->sizes[1];
 
  /* finally ready to roll */
  retval = mat_inv(bptrs,a->sizes[0]);
  if ( retval < 0 )
    retval = 0; 

  return retval;
}

/****************************************************************************
*
* function: matrix_determinant_command()
*
* purpose: implements the matrix_determinant user command.
*/
REAL matrix_determinant_command ARGS2((a,adata),
struct array *a, /* variable id numbers for the matrix */
REAL *adata ) /* data address */
{ 
  REAL **atmp;
  int i,j;
  REAL retval;

  if ( a->sizes[0] != a->sizes[1] )
    kb_error(3220, "matrix_determinant: matrix is not square.\n",
       RECOVERABLE);

  if ( a->datatype != REAL_TYPE )
     kb_error(3221,"matrix_determinant: matrix is not of type REAL.\n",
         RECOVERABLE);

  atmp = dmatrix(0,a->sizes[0],0,a->sizes[1]);  
 
  for ( i = 0 ; i < a->sizes[0] ; i++ )
    for ( j = 0 ; j < a->sizes[1] ; j++ )
      atmp[i][j] = adata[i*a->sizes[0]+j];
 
  /* finally ready to roll */
  retval = determinant(atmp,a->sizes[0]);

  free_matrix(atmp);
 
  return retval;
}
