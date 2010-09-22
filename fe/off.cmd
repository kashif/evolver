// off.cmd

// Surface Evolver command to print OFF format file.
// usage:
//   do_off >>> "filename.off"

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

do_off := {
   // file header
   printf "OFF\n%g %g %g\n",vertex_count,facet_count,edge_count;

   // vertex list
   foreach vertex do { printf "%f %f %f \n",x,y,z };

   // triangle list
   foreach facet ff do 
   { printf "3 ";
     foreach ff.vertex do printf "%g ",id-1; printf "\n";
   }
}

