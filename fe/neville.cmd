// neville.cmd
// Neville's algorithm for computing Bspline values

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// Contents:
//   neville1  - interpolation and derivatives in one dimension
//   neville2  - interpolation and derivatives in two dimensions.

// Neville algorithm for interpolating along one dimension. 

// Usage of neville1:
// neville1_data should be set up by caller, redimensioning if necessary
// Indexed by point number along curve and data dimension.

define neville1_data real[0][0]; // not modified by neville1

// Returned from neville1.  Index is data dimension.
define neville1_value real[0];   // interpolated values
define neville1_deriv real[0];   // interpolated deriv wrt u 

procedure neville1 ( 
      real neville1_order,  // order of interpolation polynomial
      real neville1_dim,    // range dimension
      real neville1_u       // interpolation spot, between 0 and 1
  )  
{
  local neville1_array; local neville1_darray;
  define neville1_array real[neville1_order+1][neville1_dim];
  define neville1_darray real[neville1_order+1][neville1_dim];
  define neville1_value real[neville1_dim];   // interpolated values
  define neville1_deriv real[neville1_dim];   // interpolated deriv wrt u 
  local inx;local dinx;
  inx := 1;
  while ( inx <= neville1_order+1 ) do
  { 
    dinx := 1;
    while ( dinx <= neville1_dim ) do
    { neville1_array[inx][dinx] := neville1_data[inx][dinx];
      neville1_darray[inx][dinx] := 0;
      dinx += 1;
    };
    inx += 1;
  };
  // scale npoint to integer based value
  local nnpoint;
  nnpoint := neville1_u*neville1_order;
  local linx;
  linx := 1;
  while ( linx <= neville1_order ) do
  { inx := 1;
    while ( inx + linx <= neville1_order + 1 ) do
    { dinx := 1;
      while ( dinx <= neville1_dim ) do
      { // do derivatives first, since dependent on current value
        neville1_darray[inx][dinx] := 
         ((inx-1+linx - nnpoint)*neville1_darray[inx][dinx]
              + (nnpoint - (inx-1))*neville1_darray[inx+1][dinx])/linx +
         (-neville1_array[inx][dinx] + neville1_array[inx+1][dinx])/linx;
        neville1_array[inx][dinx] := 
         ((inx-1+linx - nnpoint)*neville1_array[inx][dinx]
              + (nnpoint - (inx-1))*neville1_array[inx+1][dinx])/linx;
        dinx += 1;
      };
      inx += 1;
    };
    linx += 1;
  };
  dinx := 1;
  while ( dinx <= neville1_dim ) do
  { neville1_value[dinx] := neville1_array[1][dinx];
    neville1_deriv[dinx] := neville1_darray[1][dinx]*neville1_order;
    dinx += 1;
  };
}
 

// Neville algorithm for interpolating in 2D

// Usage of neville2:
// Initialize neville2_data, neville2_u arrays.
// Call neville2.

// Input data, indexed by (i,j) node coordinate and data dimension.
define neville2_data real[0][0][0];
define neville2_u real[2]; // incoming; should sum to at most 1
neville2_u[1] :=  1/3;
neville2_u[2] :=  1/3; 

// Return values, indexed by data dimension.
define neville2_value real[0] // return values of position
define neville2_deriv real[0][2] // return values of partials

procedure neville2 (
   real neville2_order,  // of polynomial
   real neville2_dim     // dimension of values
  )
{
  local neville2_array; local neville2_darray; local unpoint;

  define neville2_array 
     real[neville2_order+1][neville2_order+1][neville2_dim];
  define neville2_darray 
     real[neville2_order+1][neville2_order+1][neville2_dim][2];
  define uupoint real[2];  // scaled target coordinates
  define neville2_value real[neville2_dim]; // return values of position
  define neville2_deriv real[neville2_dim][2]; // return values of partials

  // initialize data; kludge due to fact that indexing on vertices
  // only does the three corners.
  local inx; local jnx; local dinx;
  inx := 1;
  while ( inx <= neville2_order+1 ) do
  { jnx := 1;
    while ( inx + jnx <= neville2_order+2 ) do
    { dinx := 1;
      while ( dinx <= neville2_dim ) do
      { neville2_array[inx][jnx][dinx] :=  neville2_data[inx][jnx][dinx];
        neville2_darray[inx][jnx][dinx][1] := 0;
        neville2_darray[inx][jnx][dinx][2] := 0;
        dinx += 1;
      };
      jnx += 1;
    };
    inx += 1;
  };

  // scale npoint to integer based value
  uupoint[1] := neville2_u[1]*neville2_order;
  uupoint[2] := neville2_u[2]*neville2_order;
  local linx;
  linx := 1;
  while ( linx <= neville2_order ) do
  { inx := 1;
    while ( inx + linx <= neville2_order + 1 ) do
    { jnx := 1;
      while ( inx + jnx + linx <= neville2_order + 2 ) do
      { dinx := 1;
        while ( dinx <= neville2_dim ) do
        { // do derivatives first, since dependent on current value
          neville2_darray[inx][jnx][dinx][1] :=  
                ((inx+jnx-2+linx-uupoint[1]-uupoint[2])
                       *neville2_darray[inx][jnx][dinx][1]
              + (uupoint[1]-(inx-1))*neville2_darray[inx+1][jnx][dinx][1]
              + (uupoint[2]-(jnx-1))*neville2_darray[inx][jnx+1][dinx][1])/linx
              + (-neville2_array[inx][jnx][dinx]
                  + neville2_array[inx+1][jnx][dinx] )/linx;
          neville2_darray[inx][jnx][dinx][2] :=  
                ((inx+jnx-2+linx-uupoint[1]-uupoint[2])
                       *neville2_darray[inx][jnx][dinx][2]
              + (uupoint[1]-(inx-1))*neville2_darray[inx+1][jnx][dinx][2]
              + (uupoint[2]-(jnx-1))*neville2_darray[inx][jnx+1][dinx][2])/linx
              + (-neville2_array[inx][jnx][dinx]
                  + neville2_array[inx][jnx+1][dinx] )/linx;

          neville2_array[inx][jnx][dinx] :=  
         ((inx+jnx-2+linx-uupoint[1]-uupoint[2])*neville2_array[inx][jnx][dinx]
              + (uupoint[1] - (inx-1))*neville2_array[inx+1][jnx][dinx]
              + (uupoint[2] - (jnx-1))*neville2_array[inx][jnx+1][dinx])/linx;
          dinx += 1;
        };
        jnx += 1;
      };
      inx += 1;
    };
    linx += 1;
  };
  dinx := 1;
  while ( dinx <= neville2_dim ) do
  { neville2_value[dinx] := neville2_array[1][1][dinx];
    neville2_deriv[dinx][1] := neville2_darray[1][1][dinx][1]*neville2_order;
    neville2_deriv[dinx][2] := neville2_darray[1][1][dinx][2]*neville2_order;
    dinx += 1;
  };
}
 
