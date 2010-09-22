// gaussmap.cmd
// Converts each vertex coordinate to unit normal.
// No vertices should be on constraints or fixed.
// Saves original coordinates so "revert" restores original surface.
// Before running "gaussmap", vertices should be freed from all
// constraints and boundaries.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// Usage: gaussmap

define vertex attribute gaussx real[3]
define vertex attribute oldx real[3]
gaussmap := { 
   foreach vertex vv do  
   { local connum;
     connum := 1;
     do { unset vv constraint connum; connum += 1; }
     while ( connum < 30 );
     vv.oldx[1] := vv.x[1];
     vv.oldx[2] := vv.x[2];
     vv.oldx[3] := vv.x[3];
     vv.gaussx[1] := vv.vertexnormal[1];
     vv.gaussx[2] := vv.vertexnormal[2];
     vv.gaussx[3] := vv.vertexnormal[3];
   };
   set vertex x gaussx[1];
   set vertex y gaussx[2];
   set vertex z gaussx[3];
}

revert := {
   set vertex x oldx[1];
   set vertex y oldx[2];
   set vertex z oldx[3];
}

// Command to find spherical area of gauss map, Lagrange model
calc_gaussarea := {
   local jnx, jnxx;
   if quadratic then lagrange 2; 
   gaussarea := 0;
   jnx := 2;
   jnxx := 3;
   foreach facet ff do
   { local angle_a, angle_b, angle_c;
     angle_a := asin(sqrt(
        (ff.vertex[1].vertexnormal[1]-ff.vertex[jnx].vertexnormal[1])^2
       +(ff.vertex[1].vertexnormal[2]-ff.vertex[jnx].vertexnormal[2])^2
       +(ff.vertex[1].vertexnormal[3]-ff.vertex[jnx].vertexnormal[3])^2)/2);
     angle_b := asin(sqrt(
        (ff.vertex[jnxx].vertexnormal[1]-ff.vertex[jnx].vertexnormal[1])^2
       +(ff.vertex[jnxx].vertexnormal[2]-ff.vertex[jnx].vertexnormal[2])^2
       +(ff.vertex[jnxx].vertexnormal[3]-ff.vertex[jnx].vertexnormal[3])^2)/2);
     angle_c := asin(sqrt(
        (ff.vertex[jnxx].vertexnormal[1]-ff.vertex[1].vertexnormal[1])^2
       +(ff.vertex[jnxx].vertexnormal[2]-ff.vertex[1].vertexnormal[2])^2
       +(ff.vertex[jnxx].vertexnormal[3]-ff.vertex[1].vertexnormal[3])^2)/2);
     gaussarea += 4*atan(sqrt(tan((angle_a+angle_b+angle_c)/2)
                         *tan((angle_a+angle_b-angle_c)/2)
                         *tan((angle_a-angle_b+angle_c)/2)
                         *tan((-angle_a+angle_b+angle_c)/2)));
   };
   printf "Gauss map area: %18.15f or %18.15f*pi\n",gaussarea,gaussarea/pi;
}
     
