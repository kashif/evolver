// slicer.cmd --- create intersection of surface with plane
// plane eq: a*x + b*y + c*z = d

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// Commands included: 
//   drawslice - make new edges across facets along plane.
//   slicer - make slice and dissolve all facets, edges, and vertices
//            on negative side of the plane. Calls drawslice.
//   slice_list - print coordinates of vertices along a slice.

// "slicer" usage: 
// Set slice_a,slice_b,slice_c,slice_d coefficients so
// slice_a*x + slice_b*y + slice_c*z >= slice_d
// is the side you want, and do "slicer".
// output: truncated surface on positive side of plane
// Try not to slice exactly through vertices!!
// Default plane is slice_a := 0, slice_b := 0, slice_c := 1, slice_d := .1

// To mark the vertices and edges created by the current slicing,
// there are a vertex attribute v_timestamp and an edge attribute 
// e_timestamp that are set to slice_timestamp, which is incremented
// each time slicer is called.

// Works in torus by rewrapping wrapped edges that would be cut
// so unwrapped part is on positive side of cut plane.

// Slice_list:
// Listing of vertices along the slice, beginning at user-designated vertex.
// If the slice has multiple components, you will have to run separately
// for each component.
// Usage: set startv to the first vertex on the slice and call slice_list
// Output is to stdout, so will usually be redirected, e.g.
//    slice_list >>> "slice.dat"

//Set these coefficients for the slicing plane
slice_a := 0; 
slice_b := 0; 
slice_c := 1; 
slice_d := .1; 

// Attributes for marking vertices and edges on slice with timestamp
slice_timestamp := 0
define vertex attribute v_timestamp integer
define edge attribute e_timestamp integer

// First put in new edges along slicing plane
drawslice := { 
        local lambda;
        local denom;
        local xx1;
        local yy1;
        local zz1;
        local xx2;
        local yy2;
        local zz2;

        slice_timestamp += 1;  // marker for this slice
        foreach edge ee do 
        {
          xx1 := ee.vertex[1].x; 
          yy1 := ee.vertex[1].y; 
          zz1 := ee.vertex[1].z; 
          xx2 := xx1 + ee.x;  // using edge vector in case of torus wrap
          yy2 := yy1 + ee.y; 
          zz2 := zz1 + ee.z;
          denom := slice_a*(xx1-xx2)+slice_b*(yy1-yy2)+slice_c*(zz1-zz2);
          if ( denom != 0.0 ) then 
          { 
            lambda := (slice_d-slice_a*xx2-slice_b*yy2-slice_c*zz2)/denom; 
            if ( (lambda >= 0) and (lambda <= 1) ) then 
            { 
              if torus then 
                if ee.wrap then
                { if denom > 0 then // tail on positive side
                    wrap_vertex(ee.vertex[2].id,ee.wrap)
                  else  // head on positive side
                    wrap_vertex(ee.vertex[1].id,wrap_inverse(ee.wrap));
                }; 
         
              xb := lambda*xx1+(1-lambda)*xx2; 
              yb := lambda*yy1+(1-lambda)*yy2;
              zb := lambda*zz1+(1-lambda)*zz2; 
              refine ee;
              ee.vertex[2].v_timestamp := slice_timestamp;
              ee.vertex[2].x := xb;
              ee.vertex[2].y := yb;
              ee.vertex[2].z := zb;
            } 
            else if torus and ee.wrap then
            { // try wrapping from head
              xx2 := ee.vertex[2].x; 
              yy2 := ee.vertex[2].y; 
              zz2 := ee.vertex[2].z; 
              xx1 := xx2 - ee.x;  // using edge vector in case of torus wrap
              yy1 := yy2 - ee.y; 
              zz1 := zz2 - ee.z;
              denom := slice_a*(xx1-xx2)+slice_b*(yy1-yy2)+slice_c*(zz1-zz2);
              if ( denom != 0.0 ) then 
              { 
                lambda := (slice_d-slice_a*xx2-slice_b*yy2-slice_c*zz2)/denom; 
                if ( (lambda >= 0) and (lambda <= 1) ) then 
                { 
                  if torus then 
                    if ee.wrap then
                    { if denom > 0 then // tail on positive side
                        wrap_vertex(ee.vertex[2].id,ee.wrap)
                      else  // head on positive side
                        wrap_vertex(ee.vertex[1].id,wrap_inverse(ee.wrap));
                    };
             
                  xb := lambda*xx1+(1-lambda)*xx2; 
                  yb := lambda*yy1+(1-lambda)*yy2;
                  zb := lambda*zz1+(1-lambda)*zz2; 
                  refine ee;
                  ee.vertex[2].v_timestamp := slice_timestamp;
                  ee.vertex[2].x := xb;
                  ee.vertex[2].y := yb;
                  ee.vertex[2].z := zb;
                }
              } 
          }
        } ; 
      } ; 

     // mark edges created by slicing
     set edge ee e_timestamp min(ee.vertex,v_timestamp); 
   }

slicer := {
    drawslice;
    former_autodisplay := (autodisplay);
    autodisplay off; // prevent display while dissolving
    foreach facet ff where   // again, careful of torus wraps
      slice_a*(ff.vertex[1].x+ff.edge[1].x/3-ff.edge[3].x/3) +
      slice_b*(ff.vertex[1].y+ff.edge[1].y/3-ff.edge[3].y/3) +
      slice_c*(ff.vertex[1].z+ff.edge[1].z/3-ff.edge[3].z/3)  < slice_d do
    { unset ff frontbody;
      unset ff backbody;
      dissolve ff;
    };
    dissolve bodies bbb where sum(bbb.facets,1) == 0;
    dissolve edges;  // just does bare edges
    dissolve vertices; // just does bare vertices
    if former_autodisplay then autodisplay on;
}

color_slice := {
    foreach facet ff where   // again, careful of torus wraps
      slice_a*(ff.vertex[1].x+ff.edge[1].x/3-ff.edge[3].x/3) +
      slice_b*(ff.vertex[1].y+ff.edge[1].y/3-ff.edge[3].y/3) +
      slice_c*(ff.vertex[1].z+ff.edge[1].z/3-ff.edge[3].z/3)  < slice_d do
       set ff color magenta;      

}

// Listing of vertices along the slice, beginning at user-designated vertex.
// If the slice has multiple components, you will have to run separately
// for each component.
// Usage: set startv to the first vertex on the slice and call slice_list
// Output is to stdout, so will usually be redirected, e.g.
//    slice_list >>> "slice.dat"

startv := 1
slice_list := {
   if vertex[startv].v_timestamp != slice_timestamp then
   { errprintf "slice_list: starting vertex startv is %d, not on latest slice.\n",
        startv;
     return;
   };
   thisv := startv;
   thisedge := 0;
   do
   { // print current vertex coordinates
     printf "%20.15f %20.15f %20.15f\n",vertex[thisv].x,vertex[thisv].y,
       vertex[thisv].z;
     nextedge := 0;
     foreach vertex[thisv].edge ee where ee.e_timestamp == slice_timestamp
     do { if ee.oid != -thisedge then { nextedge := ee.oid; break; }};
     if nextedge == 0 then break; /* done */ 
     thisv := edge[nextedge].vertex[2].id;
     thisedge := nextedge;
   } while thisv != startv; // done
}

// "slicer" usage: 
// Set slice_a,slice_b,slice_c,slice_d coefficients so
// slice_a*x + slice_b*y + slice_c*z >= slice_d
// is the side you want, and do "slicer".
// Default plane is slice_a := 0, slice_b := 0, slice_c := 1, slice_d := .1
