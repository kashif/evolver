// CMCcousin.cmd

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// A constant mean curvature surface in R^3 has a "cousin" minimal
// surface in S^3, which is isometric to it and has tangent planes
// rotated by 90 degrees.  In S^3, the translation is done through
// the quaternion group.

// This representation uses the 4th coordinate as the quaternion
// scalar component, for better mapping between R^3 and S^3 at quaternion unit.

// Datafiles should be set up in 4 dimensions, with S^3 implemented as
// level set constraint x^2 + y^2 + z^2 + w^2 = 1

// Works best if starting edge starte is toward the center of the surface
// rather than on the outside.

// Procedures contained in this file:
// s3_to_r3: Converts minimal surface in S^3 to CMC 1 surface in R^3.
//           Remove all constraints and boundaries before invoking.
// r3_to_s3: Converts CMC 1 surface in R^3 to minimal surface in S^3.
//           Remove all constraints and boundaries before invoking.
// procedure centralize(integer v_id): translates S^3 so given vertex
//           is at (0,0,0,1).


// For converting to adjoint
define vertex attribute newx real [4];
define edge attribute eflag integer;
define edge attribute enewx real [4];

// Angle of Bonnet rotation, degrees
bangle := 90


// unit normal of S^3 facet
define s3points real[3][4]  // input
define s3normal real[4]  // for return
calc_s3_normal := {
  // triple product in R^4
  s3normal[1] := s3points[1][2]*(s3points[2][3]*s3points[3][4]
                         - s3points[2][4]*s3points[3][3])
               + s3points[1][3]*(s3points[2][4]*s3points[3][2]
                         - s3points[2][2]*s3points[3][4])
               + s3points[1][4]*(s3points[2][2]*s3points[3][3]
                         - s3points[2][3]*s3points[3][2]);
  s3normal[2] := -(s3points[1][1]*(s3points[2][3]*s3points[3][4]
                         - s3points[2][4]*s3points[3][3])
               + s3points[1][3]*(s3points[2][4]*s3points[3][1]
                         - s3points[2][1]*s3points[3][4])
               + s3points[1][4]*(s3points[2][1]*s3points[3][3]
                         - s3points[2][3]*s3points[3][1]));
  s3normal[3] := s3points[1][1]*(s3points[2][2]*s3points[3][4]
                         - s3points[2][4]*s3points[3][2])
               + s3points[1][2]*(s3points[2][4]*s3points[3][1]
                         - s3points[2][1]*s3points[3][4])
               + s3points[1][4]*(s3points[2][1]*s3points[3][2]
                         - s3points[2][2]*s3points[3][1]);
  s3normal[4] := -(s3points[1][1]*(s3points[2][2]*s3points[3][3]
                         - s3points[2][3]*s3points[3][2])
               + s3points[1][2]*(s3points[2][3]*s3points[3][1]
                         - s3points[2][1]*s3points[3][3])
               + s3points[1][3]*(s3points[2][1]*s3points[3][2]
                         - s3points[2][2]*s3points[3][1]));
  mag := sqrt(s3normal[1]^2 + s3normal[2]^2 + s3normal[3]^2 + s3normal[4]^2);
  for ( inx := 1 ; inx < 4 ; inx += 1 )
    s3normal[inx] /= mag;
}

// Swaps conjugate sets of coordinates.
flip := {
   foreach vertex vv do 
   { tmp := vv.x; vv.x := vv.newx[1]; vv.newx[1] := tmp; 
     tmp := vv.y; vv.y := vv.newx[2]; vv.newx[2] := tmp; 
     tmp := vv.z; vv.z := vv.newx[3]; vv.newx[3] := tmp; 
     tmp := vv.w; vv.w := vv.newx[4]; vv.newx[4] := tmp; 
   }
}

starte := 0    // user should set starte to set origin of adjoint.
define rn real[3][4];
define emid real[3][4];

calc_s3_to_r3 := {
   set edge eflag 0;
   if starte == 0 then
     foreach edge ee do { starte := ee.id; break; };  // just to get starter
   edge[starte].enewx[1] := 0;
   edge[starte].enewx[2] := 0;
   edge[starte].enewx[3] := 0;
   edge[starte].eflag := 1;

   bs := sin(bangle*pi/180);
   bc := cos(bangle*pi/180);  
   ecount := 1;
   endflag := 0;
   loopcount := 1;
   while ( !endflag ) do
   { endflag := 1; 
     foreach facet ff do
     { enum := 1; 
       while ( enum <= 5 ) do
       { thise := (enum imod 3) + 1;
         nexte := ((enum+1) imod 3) + 1;
         othere := ((enum+2) imod 3) + 1;
         if ( ff.edge[thise].eflag>=loopcount and !ff.edge[nexte].eflag ) then
         { // transform this facet
           // normal in S3
           for ( inx := 1 ; inx <= 4 ; inx += 1 )
             for ( vnx := 1 ; vnx <= 3 ; vnx += 1 )
               s3points[vnx][inx] := ff.vertex[vnx].x[inx];
           calc_s3_normal;
           // edge midpoints in S3 
           for ( enx := 1 ; enx <= 3 ; enx += 1 )
           { for ( inx := 1 ; inx <= 4 ; inx += 1 )
               emid[enx][inx] := avg(ff.edge[enx].vertex,x[inx]);
             mag := sqrt(emid[enx][1]^2 + emid[enx][2]^2 + emid[enx][3]^2
                             + emid[enx][4]^2);
             for ( inx := 1 ; inx <= 4 ; inx += 1 )
               emid[enx][inx] /= mag;
             // Use inverse quaternion of each edge midpoint to convert
             // common normal to R3 normal
             rn[enx][1] := emid[enx][4]*s3normal[1] - s3normal[4]*emid[enx][1]
                      - (emid[enx][2]*s3normal[3] - emid[enx][3]*s3normal[2]);
             rn[enx][2] := emid[enx][4]*s3normal[2] - s3normal[4]*emid[enx][2]
                      - (emid[enx][3]*s3normal[1] - emid[enx][1]*s3normal[3]);
             rn[enx][3] := emid[enx][4]*s3normal[3] - s3normal[4]*emid[enx][3]
                      - (emid[enx][1]*s3normal[2] - emid[enx][2]*s3normal[1]);
           };

           for ( inx := 1 ; inx <= 3 ; inx += 1 )
             ff.edge[nexte].enewx[inx] := 
                ff.edge[thise].enewx[inx] + rn[nexte][inx] - rn[thise][inx]; 

           ff.edge[nexte].eflag := loopcount+1;
           endflag := 0; 
           ecount += 1;
         };
         enum += 1;
       };  // end while
     };
     if !quiet then printf "%g edges done.\n",ecount;
     loopcount += 1;
   };
   set vertex newx[1] 0;
   set vertex newx[2] 0;
   set vertex newx[3] 0;
   foreach facet ff do 
   { // extend center facet to original vertex; can't simply average
     //   midedge vertices around an original vertex since that doesn't
     //   work for vertices on the boundary.
     vva := 1; while ( vva <= 3 ) do
     { vvb := vva==3 ? 1 : vva+1;
       vvc := vva==1 ? 3 : vva-1;
       kk := 1; while ( kk <= 3 ) do
       {
         ff.vertex[vva].newx[kk] += ff.edge[vva].enewx[kk] 
               - ff.edge[vvb].enewx[kk] + ff.edge[vvc].enewx[kk]; 
         kk += 1;
       };
       vva += 1;
     }
   };
   foreach vertex vv do
   { 
     nbrs := sum(vv.facet, 1);
     if ( nbrs != 0 ) then
     { vv.newx[1] /= nbrs;
       vv.newx[2] /= nbrs;
       vv.newx[3] /= nbrs;
       vv.newx[4] := 0;
     };
   };
  
} // end calc_s3_to_r3

// try using average normal at edge midpoints
calc_s3_to_r3_a := {
   define facet attribute snormal real[4];
   define nmid real[4][4];

   set edge eflag 0;
   if starte == 0 then
     foreach edge ee do { starte := ee.id; break; };  // just to get starter
   edge[starte].enewx[1] := 0;
   edge[starte].enewx[2] := 0;
   edge[starte].enewx[3] := 0;
   edge[starte].eflag := 1;

   foreach facet ff do
   { for ( inx := 1 ; inx <= 4 ; inx += 1 )
       for ( vnx := 1 ; vnx <= 3 ; vnx += 1 )
         s3points[vnx][inx] := ff.vertex[vnx].x[inx];
     calc_s3_normal;
     for ( inx := 1 ; inx <= 4 ; inx += 1 )
       ff.snormal[inx] := s3normal[inx];
   };
   bs := sin(bangle*pi/180);
   bc := cos(bangle*pi/180);  
   ecount := 1;
   endflag := 0;
   loopcount := 1;
   while ( !endflag ) do
   { endflag := 1; 
     foreach facet ff do
     { enum := 1; 
       while ( enum <= 5 ) do
       { thise := (enum imod 3) + 1;
         nexte := ((enum+1) imod 3) + 1;
         othere := ((enum+2) imod 3) + 1;
         if ( ff.edge[thise].eflag>=loopcount and !ff.edge[nexte].eflag ) then
         { // transform this facet
           // edge midpoints in S3 
           for ( enx := 1 ; enx <= 3 ; enx += 1 )
           { for ( inx := 1 ; inx <= 4 ; inx += 1 )
             { emid[enx][inx] := avg(ff.edge[enx].vertex,x[inx]);
               nmid[enx][inx] := avg(ff.edge[enx].facet,snormal[inx]);
             };
             mag := sqrt(emid[enx][1]^2 + emid[enx][2]^2 + emid[enx][3]^2
                             + emid[enx][4]^2);
             nmag := sqrt(nmid[enx][1]^2 + nmid[enx][2]^2 + nmid[enx][3]^2
                             + nmid[enx][4]^2);
             for ( inx := 1 ; inx <= 4 ; inx += 1 )
             { emid[enx][inx] /= mag;
               nmid[enx][inx] /= nmag;
             };
             // Use inverse quaternion of each edge midpoint to convert
             // common normal to R3 normal
             rn[enx][1] := emid[enx][4]*nmid[enx][1] - nmid[enx][4]*emid[enx][1]
                      - (emid[enx][2]*nmid[enx][3] - emid[enx][3]*nmid[enx][2]);
             rn[enx][2] := emid[enx][4]*nmid[enx][2] - nmid[enx][4]*emid[enx][2]
                      - (emid[enx][3]*nmid[enx][1] - emid[enx][1]*nmid[enx][3]);
             rn[enx][3] := emid[enx][4]*nmid[enx][3] - nmid[enx][4]*emid[enx][3]
                      - (emid[enx][1]*nmid[enx][2] - emid[enx][2]*nmid[enx][1]);
           };

           for ( inx := 1 ; inx <= 3 ; inx += 1 )
             ff.edge[nexte].enewx[inx] := 
                ff.edge[thise].enewx[inx] + rn[nexte][inx] - rn[thise][inx]; 

           ff.edge[nexte].eflag := loopcount+1;
           endflag := 0; 
           ecount += 1;
         };
         enum += 1;
       };  // end while
     };
     if !quiet then printf "%g edges done.\n",ecount;
     loopcount += 1;
   };
   set vertex newx[1] 0;
   set vertex newx[2] 0;
   set vertex newx[3] 0;
   foreach facet ff do 
   { // extend center facet to original vertex; can't simply average
     //   midedge vertices around an original vertex since that doesn't
     //   work for vertices on the boundary.
     vva := 1; while ( vva <= 3 ) do
     { vvb := vva==3 ? 1 : vva+1;
       vvc := vva==1 ? 3 : vva-1;
       kk := 1; while ( kk <= 3 ) do
       {
         ff.vertex[vva].newx[kk] += ff.edge[vva].enewx[kk] 
               - ff.edge[vvb].enewx[kk] + ff.edge[vvc].enewx[kk]; 
         kk += 1;
       };
       vva += 1;
     }
   };
   foreach vertex vv do
   { 
     nbrs := sum(vv.facet, 1);
     if ( nbrs != 0 ) then
     { vv.newx[1] /= nbrs;
       vv.newx[2] /= nbrs;
       vv.newx[3] /= nbrs;
       vv.newx[4] := 0;
     };
   };
  
} // end calc_s3_to_r3

calc_r3_to_s3 := {
   set edge eflag 0;
   if starte == 0 then
     foreach edge ee do { starte := ee.id; break; };  // just to get starter
   edge[starte].enewx[1] := 0;
   edge[starte].enewx[2] := 0;
   edge[starte].enewx[3] := 0;
   edge[starte].enewx[4] := 1;
   edge[starte].eflag := 1;

   bs := sin(bangle*pi/180);
   bc := cos(bangle*pi/180);  
   ecount := 1;
   endflag := 0;
   loopcount := 1;
   while ( !endflag ) do
   { endflag := 1; 
     foreach facet ff do
     { enum := 1; 
       while ( enum <= 5 ) do
       { thise := (enum imod 3) + 1;
         nexte := ((enum+1) imod 3) + 1;
         othere := ((enum+2) imod 3) + 1;
         if ( ff.edge[thise].eflag>=loopcount and !ff.edge[nexte].eflag ) then
         { // facet unit normal
           nx := ff.x;
           ny := ff.y;
           nz := ff.z;
           norm := sqrt(nx^2+ny^2+nz^2);
           nx := nx/norm; ny := ny/norm; nz := nz/norm;

           // vector from thise to nexte
           vx := -ff.edge[othere].x/2;
           vy := -ff.edge[othere].y/2;
           vz := -ff.edge[othere].z/2;

           // rotate 90 degrees about normal (using cross product)
           qx := -(ny*vz - nz*vy);
           qy := -(nz*vx - nx*vz);
           qz := -(nx*vy - ny*vx);
           qw := sqrt(1 - (qx^2 + qy^2 + qz^2));

           // quaternion multiplication by position of thise midpoint
           tx := ff.edge[thise].enewx[1];
           ty := ff.edge[thise].enewx[2];
           tz := ff.edge[thise].enewx[3];
           tw := ff.edge[thise].enewx[4];
           ff.edge[nexte].enewx[1] := tw*qx + qw*tx + (ty*qz - tz*qy);
           ff.edge[nexte].enewx[2] := tw*qy + qw*ty + (tz*qx - tx*qz);
           ff.edge[nexte].enewx[3] := tw*qz + qw*tz + (tx*qy - ty*qx); 
           ff.edge[nexte].enewx[4] := tw*qw - (tx*qx + ty*qy + tz*qz);

           ff.edge[nexte].eflag := loopcount+1;
           endflag := 0; 
           ecount += 1;
         };
         enum += 1;
       };  // end while
     };
     if !quiet then printf "%g edges done.\n",ecount;
     loopcount += 1;
   };
   set vertex newx[1] 0;
   set vertex newx[2] 0;
   set vertex newx[3] 0;
   set vertex newx[4] 0;
   foreach facet ff do 
   { // extend center facet to original vertex; can't simply average
     //   midedge vertices around an original vertex since that doesn't
     //   work for vertices on the boundary.
     for ( vva := 1; vva <= 3 ; vva += 1 ) 
     { vvb := vva==3 ? 1 : vva+1;
       vvc := vva==1 ? 3 : vva-1;
       for ( kk := 1; kk <= 4 ; kk += 1 ) 
       {
         ff.vertex[vva].newx[kk] += ff.edge[vva].enewx[kk] 
               - ff.edge[vvb].enewx[kk] + ff.edge[vvc].enewx[kk]; 
       };
     };
   };
   foreach vertex vv do
   { 
     nbrs := sum(vv.facet, 1);
     if ( nbrs != 0 ) then
     { vv.newx[1] /= nbrs;
       vv.newx[2] /= nbrs;
       vv.newx[3] /= nbrs;
       vv.newx[4] /= nbrs;
     };
   };
  
} // end calc_r3_to_s3

s3_to_r3 := { 
   foreach vertex vv do
     if vv.__v_constraint_list[1] != 0 then
     { errprintf "s3_to_r3 error: vertex %d is still on a constraint.\n",
         vv.id;
       return;
     };
   autodisplay_state := (autodisplay);
   autodisplay off;
   calc_s3_to_r3;
   flip;
   if ( autodisplay_state ) then autodisplay;
}

r3_to_s3 := { 
   foreach vertex vv do
     if vv.__v_constraint_list[1] != 0 then
     { errprintf "s3_to_r3 error: vertex %d is still on a constraint.\n",
         vv.id;
       return;
     };
   autodisplay_state := (autodisplay);
   autodisplay off;
   calc_r3_to_s3;
   flip;
   if ( autodisplay_state ) then autodisplay;
}

   
// Utility function for translating surface in sphere to get
// desired vertex at identity element (0,0,0,1).
procedure centralize ( integer v_id ) {
  ax := vertex[v_id].x;
  ay := vertex[v_id].y;
  az := vertex[v_id].z;
  aw := vertex[v_id].w;
  foreach vertex vv do {
    vx := vv.x;
    vy := vv.y;
    vz := vv.z;
    vw := vv.w;
    vv.x := aw*vx - vw*ax - (ay*vz - az*vy);
    vv.y := aw*vy - vw*ay - (az*vx - ax*vz);
    vv.z := aw*vz - vw*az - (ax*vy - ay*vx);
    vv.w := aw*vw + ax*vx + ay*vy + az*vz;
  };
}

