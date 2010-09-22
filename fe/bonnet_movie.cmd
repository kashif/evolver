// bonnet_movie.cmd

// Makes in-memory movie of Bonnet rotation of minimal surface,
// one frame per degree for 360 degrees.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// Usage: Evolve initial minimal surface, remove all level-set constraints
//        and boundaries, get nice view in graphics window.
//        Run "show_movie" to see screen display of Bonnet rotation.
//        Run "postscript_movie" to create sequence of PostScript files.

read "adjoint.cmd"

midvertex := 5  // vertex to keep at origin

frame_delay := 0.03  // seconds between frames

// Store coordinates for complete Bonnet rotation
define vertex attribute bonnet real[360][3]
make_movie := {
  local midx,midy,midz;
  quiet on;
  for ( bangle := 0 ; bangle < 359.5 ; bangle += 1 )
  { adjoint;
    midx := vertex[midvertex].x;
    midy := vertex[midvertex].y;
    midz := vertex[midvertex].z;
   
    foreach vertex vv do
    { vv.bonnet[bangle+1][1] := vv.x-midx;
      vv.bonnet[bangle+1][2] := vv.y-midy;
      vv.bonnet[bangle+1][3] := vv.z-midz;
    };
    flip;
  };
  quiet off;
}

procedure set_bonnet(integer angle)
{ 
    foreach vertex vv do
    { vv.x := vv.bonnet[angle+1][1];
      vv.y := vv.bonnet[angle+1][2];
      vv.z := vv.bonnet[angle+1][3];
    };
    recalc;
}

show_movie := {
  local next_time;
  next_time := clock;
  for ( angle := 0; angle < 359.5 ; angle += 1 )
  { while clock < next_time do {};
    next_time += frame_delay;
    set_bonnet(angle);
  } 
}

// write postscript file for each frame
postscript_movie := {
  full_bounding_box on;
  for ( angle := 0; angle < 359.5 ; angle += 1 )
  {
    set_bonnet(angle);
    postscript sprintf"%s.%03d",datafilename,angle;
  } 
}
  
// continuous loop
movie := { for (;;) show_movie; }


// Write split-vertex adjoint datafile.  To be done after conconj;
// uses enewx values to compute vertices.
define facet attribute split_bonnet real[3][360][3]
write_split_movie := {
  printf "// split-vertex discrete adjoint of %s, rotation angle %f.\n",
    datafilename,bangle;
  printf "\ndefine vertex attribute bonnet real[360][3]\n\n";
  printf "\nvertices\n";
  foreach facet ff do
  { printf "%d   %15.10f %15.10f %15.10f bonnet ",3*ff.id-2,
      ff.edge[1].enewx[1]+ff.edge[2].enewx[1]-ff.edge[3].enewx[1],
      ff.edge[1].enewx[2]+ff.edge[2].enewx[2]-ff.edge[3].enewx[2],
      ff.edge[1].enewx[3]+ff.edge[2].enewx[3]-ff.edge[3].enewx[3];
    print ff.split_bonnet[1];
    printf "\n";
    printf "%d   %15.10f %15.10f %15.10f bonnet ",3*ff.id-1,
     -ff.edge[1].enewx[1]+ff.edge[2].enewx[1]+ff.edge[3].enewx[1],
     -ff.edge[1].enewx[2]+ff.edge[2].enewx[2]+ff.edge[3].enewx[2],
     -ff.edge[1].enewx[3]+ff.edge[2].enewx[3]+ff.edge[3].enewx[3];
    print ff.split_bonnet[2];
    printf "\n";
    printf "%d   %15.10f %15.10f %15.10f bonnet ",3*ff.id,
      ff.edge[1].enewx[1]-ff.edge[2].enewx[1]+ff.edge[3].enewx[1],
      ff.edge[1].enewx[2]-ff.edge[2].enewx[2]+ff.edge[3].enewx[2],
      ff.edge[1].enewx[3]-ff.edge[2].enewx[3]+ff.edge[3].enewx[3];
    print ff.split_bonnet[3];
    printf "\n";
  };
  printf "\nedges\n";
  foreach facet ff do
  { printf "%d   %d %d\n",3*ff.id-2,3*ff.id-2,3*ff.id-1;
    printf "%d   %d %d\n",3*ff.id-1,3*ff.id-1,3*ff.id;
    printf "%d   %d %d\n",3*ff.id,3*ff.id,3*ff.id-2;
  };
  printf "\nfaces\n";
  foreach facet ff do 
    printf "%d    %d %d %d\n",ff.id,3*ff.id-2,3*ff.id-1,3*ff.id;
  printf "\nread\n";
  printf "read \"bonnet_movie.cmd\"\n";
}

make_split_movie := {
  local midx,midy,midz;
  quiet on;
  for ( bangle := 0 ; bangle < 359.5 ; bangle += 1 )
  { adjoint;
    midx := vertex[midvertex].x;
    midy := vertex[midvertex].y;
    midz := vertex[midvertex].z;
   
    foreach edge ee  do
    { ee.enewx[1] -= midx;
      ee.enewx[2] -= midy;
      ee.enewx[3] -= midz;
    };
    foreach facet ff do
    { 
      ff.split_bonnet[1][bangle+1][1] :=
        ff.edge[1].enewx[1]+ff.edge[2].enewx[1]-ff.edge[3].enewx[1];
      ff.split_bonnet[1][bangle+1][2] :=
        ff.edge[1].enewx[2]+ff.edge[2].enewx[2]-ff.edge[3].enewx[2];
      ff.split_bonnet[1][bangle+1][3] :=
        ff.edge[1].enewx[3]+ff.edge[2].enewx[3]-ff.edge[3].enewx[3];
      ff.split_bonnet[2][bangle+1][1] :=
       -ff.edge[1].enewx[1]+ff.edge[2].enewx[1]+ff.edge[3].enewx[1];
      ff.split_bonnet[2][bangle+1][2] :=
       -ff.edge[1].enewx[2]+ff.edge[2].enewx[2]+ff.edge[3].enewx[2];
      ff.split_bonnet[2][bangle+1][3] :=
       -ff.edge[1].enewx[3]+ff.edge[2].enewx[3]+ff.edge[3].enewx[3];
      ff.split_bonnet[3][bangle+1][1] :=
        ff.edge[1].enewx[1]-ff.edge[2].enewx[1]+ff.edge[3].enewx[1];
      ff.split_bonnet[3][bangle+1][2] :=
        ff.edge[1].enewx[2]-ff.edge[2].enewx[2]+ff.edge[3].enewx[2];
      ff.split_bonnet[3][bangle+1][3] :=
        ff.edge[1].enewx[3]-ff.edge[2].enewx[3]+ff.edge[3].enewx[3];
    };
    flip;
  };
  quiet off;
}

