// band.cmd

// Surface Evolver command to create triangulated band bordering
// deisgnated edges and vertices.  Purpose is to make extremely
// accurate PostScript files without notching on thick edges.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// WARNING: This command modifies the surface by creating a lot of
// tiny facets.  You should use only on a disposable copy of your surface.

// Usage: set bandcolor and bandwidth to desired values; bandwidth is
//           the width of the band in surface coordinates on one
//           side of an edge.
//        set the edge attribute inband to nonzero for those edges
//           to have band drawn along them.
//        run makeband

// When creating PostScript file, use the NN response to P 3 "Show grid lines?"

// IMPORTANT NOTE:  The algorithm has trouble where inband edges meet
// at an angle, since each vertex has a single band distance attribute
// and the vertex may be a neighbor to two different bands.  This results
// in ragged bands near corners.  A cure is to do banding in stages,
// doing one "original" edge at a time.

// Not wise to run more than once along a given edge.  Numerical
// variations due to new vertices lead to a lot more unnecessary
// refinements.

define edge attribute inband integer // 1 if band center, 0 if not
define vertex attribute banddist real // distance from band center.
define facet attribute bandused integer // so use each facet at most once

bandcolor := black  // Set this to desired color before makeband.
bandwidth := 0.010  // Set this to desired half-width before makeband.

makeband := {
   local maxbanddist,changed,eps;

   maxbanddist := 100000*bandwidth;
   set vertex banddist maxbanddist;
   foreach edge ee where inband do set ee.vertex banddist 0;
   foreach edge ee where (ee.inband==0) and (ee.vertex[1].banddist==0)
       and (ee.vertex[2].banddist==0)  
    do { refine ee; ee.vertex[2].banddist := maxbanddist; } ;
   set facet bandused 0;
   // Calculate vertex distances from band center 
   do
   { changed := 0;
     foreach facet ff do
     { local s1,s2,s3,d1,d2,d3;
       local newd1,newd2,newd3;

       if ff.bandused then continue;
       s1 := ff.edge[1].length;
       s2 := ff.edge[2].length;
       s3 := ff.edge[3].length;
       d1 := ff.vertex[1].banddist;
       d2 := ff.vertex[2].banddist;
       d3 := ff.vertex[3].banddist;
       if ( (d2 < d1 or d3 < d1) and 
          (  ( (d2 < bandwidth) && (d3 < maxbanddist) ) 
         or ( (d2 < maxbanddist) && (d3 < bandwidth) ) ) ) then
       { newd1 := d3 + s3*sin(asin((d2-d3)/s2)+acos((-s1^2+s2^2+s3^2)/2/s2/s3));
         if ( newd1 > 0 and newd1 < d1 ) then
            { ff.vertex[1].banddist := newd1; changed += 1; 
              ff.bandused := 1; continue;
            };
       };
       if ( (d1 < d2 or d3 < d2 ) and 
          ( ( (d1 < bandwidth) && (d3 < maxbanddist) ) 
         or ( (d1 < maxbanddist) && (d3 < bandwidth) ) ) ) then
       { newd2 := d1 + s1*sin(asin((d3-d1)/s3)+acos((-s2^2+s3^2+s1^2)/2/s3/s1));
         if ( newd2 > 0 and newd2 < d2 ) then
            { ff.vertex[2].banddist := newd2; changed += 1;
              ff.bandused := 1; continue; 
            };
       };
       if ( (d1 < d3 or d2 < d3 ) and 
          ( ( (d1 < bandwidth) && (d2 < maxbanddist) ) 
         or ( (d1 < maxbanddist) && (d2 < bandwidth) ) ) ) then
       { newd3 := d2 + s2*sin(asin((d1-d2)/s1)+acos((-s3^2+s1^2+s2^2)/2/s1/s2));
         if ( newd3 > 0 and newd3 < d3 ) then
            { ff.vertex[3].banddist := newd3; changed += 1; 
              ff.bandused := 1; continue; 
            };
       };
     };
   } while ( changed > 0 );
   printf "\n";

   // Subdivide edges spanning band boundary
   eps := 1e-4*bandwidth;  // numerical margin for error
   foreach edge ee do
   { local d1,d2;
     local new_x,new_y,new_z;
     local lambda;

     d1 := ee.vertex[1].banddist;
     d2 := ee.vertex[2].banddist;
     if ( ((d1<=bandwidth+eps)&&(d2<=bandwidth+eps)) || 
           ((d1>=bandwidth-eps)&&(d2>=bandwidth-eps)))
       then continue;
     lambda := (bandwidth - d2)/(d1 - d2);
     if ( lambda < .00001 or lambda > .99999 ) then continue;
     new_x := lambda*ee.vertex[1].x + (1-lambda)*ee.vertex[2].x;
     new_y := lambda*ee.vertex[1].y + (1-lambda)*ee.vertex[2].y;
     new_z := lambda*ee.vertex[1].z + (1-lambda)*ee.vertex[2].z;
     refine ee;
     ee.vertex[2].x := new_x;
     ee.vertex[2].y := new_y;
     ee.vertex[2].z := new_z;
     ee.vertex[2].banddist := bandwidth;
   };

   // Color band triangles
   foreach facet ff do
   { if avg(ff.vertex,banddist) < bandwidth then set ff color bandcolor; };

}

// to do edges on different constraints one constraint at a time.
cband := {
   local connum;
   for ( connum := 1 ; connum <= high_constraint ; connum += 1 ) 
   { if valid_constraint(connum) then
     { set edge inband on_constraint connum;
       makeband;
     }
   };
}

// makeband usage:
//   set bandwidth to desired half-width (in surface coordinates)
//   set edge inband to 1 along band center, 0 elsewhere
//   set bandcolor to desired color
//   run makeband
// In making postscript, answer N to gridlines, and y to colors.
