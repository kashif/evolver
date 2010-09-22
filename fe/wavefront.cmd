// wavefront.cmd

// Surface Evolver command file for producing Wavefront format file
// for surface, suitable for feeding to JavaView.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// Usage:
//    wavefront >>> "filename.obj"
// or, for version with normals at vertices,
//    wavefrontn >>> "filename.obj"

define vertex attribute v_num integer
wavefront := { 
   // Make sure vertices consecutively numbered
   new_v_num := 1;
   foreach vertex vv do { vv.v_num := new_v_num; new_v_num += 1 };
   
   printf "# Wavefront .obj file for %s\n",datafilename;
   printf "# Produced by the Surface Evolver.\n\n";

   foreach vertex vv do printf "v %f %f %f\n",vv.x,vv.y,vv.z;
   printf "\n";
   foreach facet ff do printf "f %g %g %g\n",ff.vertex[1].v_num,
      ff.vertex[2].v_num, ff.vertex[3].v_num;

}

// version with normals at vertices
wavefrontn := { 
   // Make sure vertices consecutively numbered
   new_v_num := 1;
   foreach vertex vv do { vv.v_num := new_v_num; new_v_num += 1 };
   
   printf "# Wavefront .obj file for %s\n",datafilename;
   printf "# Produced by the Surface Evolver.\n\n";

   foreach vertex vv do printf "v %f %f %f\n",vv.x,vv.y,vv.z;
   printf "\n";
   foreach vertex vv do printf "vn %f %f %f\n",vv.vertexnormal[1],
      vv.vertexnormal[2],vv.vertexnormal[3];
   printf "\n";
   foreach facet ff do printf "f %g/%g %g/%g %g/%g\n",ff.vertex[1].v_num,
      ff.vertex[1].v_num, ff.vertex[2].v_num,
      ff.vertex[2].v_num, ff.vertex[3].v_num,
      ff.vertex[3].v_num;

}

