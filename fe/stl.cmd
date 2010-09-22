// stl.cmd

// Surface Evolver command to produce STL format text file from surface.
// Evolver command line usage:
//    read "stl.cmd"
//    stl >>> "filename.stl"

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

stl := { 
   local mag,inx;
   printf "solid\n";
   foreach facet ff do
   { mag := sqrt(ff.x^2+ff.y^2+ff.z^2);
     printf "facet normal %f %f %f\n",ff.x/mag,ff.y/mag,ff.z/mag;
     printf "   outer loop\n";
     for ( inx := 1 ; inx <= 3 ; inx += 1 )
       printf "     vertex %f %f %f\n",ff.vertex[inx].x,ff.vertex[inx].y,
             ff.vertex[inx].z;
     printf "   endloop\n";
     printf "  endfacet\n";
   };
   printf "endsolid\n";
}

