// xray.cmd

// Produces xray image of a 3D wet foam.  Calculates liquid content on
// grid of probe lines.  Plateau borders are detected as bounded by
// facets with less than 2/3 of the maximum facet tension.
// Outputs a PostScript file to stdout with a grid of grayscale pixels.
// Usage: set xgridsize and ygridsize to the desired resolution, 
// i.e. pixels across and down and give the command
//        xray >>> "filename.ps"
// The xray command will calculate the bounding box of the surface itself.
// Works in torus or non-torus mode.  In torus mode, everything will
// automatically be wrapped back to the unit cell, so you don't have
// to worry about that.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

xgridsize := 100  // xgridsize x ygridsize grid
ygridsize := 100  


xray := { 
  local xx,yy,jitterx,jittery,minx,miny,maxx,maxy,tension_cutoff;
  local dx,dy,ax,ay,az,bx,by,bz,cx,cy,cz,hix,hiy,lox,loy,maxi,maxj;
  local mini,minj,fsign,xyarea,ii,jj,area1,area2,area3,maxr,xpts,ypts;

  define results real[xgridsize][ygridsize];
  for ( xx := 1 ; xx <= xgridsize ; xx += 1 )
    for ( yy := 1 ; yy <= ygridsize ; yy += 1 )
      results[xx][yy] := 0.0; // clean out old results
  
  // random offsets to prevent edge effects */
  jitterx := 0.5213414312341;
  jittery := 0.4934129768877;

  /* box to map results to */
  if torus then
  { // assumes orthogonal fundamental region
    minx := 0;
    miny := 0;
    maxx := torus_periods[1][1];
    maxy := torus_periods[2][2];
    tension_cutoff := max(facet,tension)*2/3;  // do only Plateau borders
  }
  else
  { minx := min(vertex,x);
    maxx := max(vertex,x);
    miny := min(vertex,y);
    maxy := max(vertex,y);
    tension_cutoff := 100000; // do all facets
  };

  dx := (maxx-minx)/xgridsize;
  dy := (maxy-miny)/ygridsize;

  foreach facet ff where tension < tension_cutoff do
  { ax := ff.vertex[1].x;
    ay := ff.vertex[1].y;
    az := ff.vertex[1].z;
    bx := ax + ff.edge[1].x;  // let Evolver take care of unwrapping
    by := ay + ff.edge[1].y;
    bz := az + ff.edge[1].z;
    cx := bx + ff.edge[2].x;  // let Evolver take care of unwrapping
    cy := by + ff.edge[2].y;
    cz := bz + ff.edge[2].z;
    // get bounding box to find possible grid points
    hix := (ax > bx) ? (ax > cx ? ax : cx) : (bx > cx ? bx : cx);
    hiy := (ay > by) ? (ay > cy ? ay : cy) : (by > cy ? by : cy);
    lox := (ax < bx) ? (ax < cx ? ax : cx) : (bx < cx ? bx : cx);
    loy := (ay < by) ? (ay < cy ? ay : cy) : (by < cy ? by : cy);
    // compact to integers
    maxi := floor(hix/dx-jitterx);
    maxj := floor(hiy/dy-jittery);
    mini := ceil(lox/dx-jitterx);
    minj := ceil(loy/dy-jittery);
    // loop among possible values
    fsign := ff.z > 0 ? 1 : -1;
    xyarea := 2*abs(ff.z);
    for ( ii := mini ; ii <= maxi ; ii += 1 )
     for ( jj := minj ; jj <= maxj ; jj += 1 )
     { // test inclusion
       xx := ii*dx + dx*jitterx; yy := jj*dy + dy*jittery;
       area1 := fsign*((ax-xx)*(by-yy) - (bx-xx)*(ay-yy));
       area2 := fsign*((bx-xx)*(cy-yy) - (cx-xx)*(by-yy));
       area3 := fsign*((cx-xx)*(ay-yy) - (ax-xx)*(cy-yy));
       if ( (area1 > 0) and (area2 > 0) and (area3 > 0) ) then
       { results[(ii imod xgridsize)+1][(jj imod ygridsize)+1] += 
            fsign*(area1*cz + area2*az + area3*bz)/xyarea;
       }
     } 
  };
  if ( torus ) then
  { // correct results for wrap in z 
    for ( ii := 1 ; ii <= xgridsize ; ii += 1 )
      for ( jj := 1 ; jj <= ygridsize ; jj += 1 )
        results[ii][jj] := results[ii][jj] mod torus_periods[3][3];
  };

  // map to range 0,1
  maxr := 0;
  for ( ii := 1 ; ii <= xgridsize ; ii += 1 )
    for ( jj := 1 ; jj <= ygridsize ; jj += 1 )
      if results[ii][jj] > maxr then maxr := results[ii][jj];
  if maxr > 0 then
    for ( ii := 1 ; ii <= xgridsize ; ii += 1 )
      for ( jj := 1 ; jj <= ygridsize ; jj += 1 )
        results[ii][jj] /= maxr;

  /* output results in postscript format, making low density white */
  xpts := 500; ypts := 500; // point size of image
  // using kludges to get %% stuff to print right on Windows and Unix
  printf "%!"; printf"PS-Adobe-3.0 EPSF-3.0\n";
  printf "%%"; printf"BoundingBox: 0 0 %d %d\n",xpts,ypts;
  printf "%%"; printf"Creator: Surface Evolver xray.cmd\n";
  printf "%%"; printf"EndComments\n\n";
  printf "%f %f scale\n",xpts/xgridsize,ypts/ygridsize;
  printf "/px { newpath setgray moveto 1 0 rlineto 0 1 rlineto -1 0 rlineto closepath fill} def\n";
  for ( ii := 0 ; ii < xgridsize ; ii += 1 )
    for ( jj := 0 ; jj < ygridsize ; jj += 1 )
    { printf "%f %f %f px\n",ii,jj,(1-results[ii+1][jj+1]);
    };
  printf "\nshowpage\n";
  printf "\n%%"; printf"EOF\n";
}

    
