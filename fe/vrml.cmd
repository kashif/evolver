// vrml.cmd
// Makes VRML file for surface.

// Usage: Set edge_flag to 1 if you want do see all edges.
//        Run "vrml" and re-direct output to file, e.g.
//        Enter command: vrml >>> "myfile.wrl";
 
// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

define vertex attribute order_num integer
edge_flag := 0 // 1 for all edges, 0 for special edges only
vrml := {
   local counter;
   counter := 0;
   printf "#VRML V1.0 ascii\n\n";
   printf "Separator {\n";
   printf "  DEF Title Info { string \"%s\" }\n",datafilename;
   printf "  DEF SceneInfo Info { string \"Created by Surface Evolver\" }\n";
   printf "  DEF BackgroundColor Info { string \".5 .6 1\" } \n";
   printf "  DirectionalLight { intensity .5 direction 0 0 -1 } \n";
   printf "  MaterialBinding { value PER_FACE_INDEXED }\n";
   printf "  Material { \n";
   printf "   diffuseColor [ 0.0 0.0 0.0    , 0.0 0.0 0.5 ,\n";
   printf "   0.0 0.5 0.0 , 0.0 0.5 0.5  ,  0.5 0.0 0.0    , 0.5 0.0 0.5    ,\n";
   printf "   0.5 0.25 0.  , .3 .3 .3 , .15 .15 .15  , .25 .25 .5 , .25 .5 .25 ,\n";
   printf "   .25 .5 .5 , .5 .25 .25 , .5 .25 .5 , .5 .5 .0 , .5 .5 .5   ] \n";
   printf "   emissiveColor [ 0.0 0.0 0.0    , 0.0 0.0 0.5 ,\n";
   printf "   0.0 0.5 0.0 , 0.0 0.5 0.5  ,  0.5 0.0 0.0    , 0.5 0.0 0.5    ,\n";
   printf "   0.5 0.25 0.  , .3 .3 .3 , .15 .15 .15  , .25 .25 .5 , .25 .5 .25 ,\n";
   printf "   .25 .5 .5 , .5 .25 .25 , .5 .25 .5 , .5 .5 .0 , .5 .5 .5   ] \n";
   printf "  }\n";
   printf "  Separator {\n";
   printf "    Coordinate3 { point [\n";
   foreach vertex jvv do { printf "        %f %f %f,\n",jvv.x,jvv.y,jvv.z;
            set jvv order_num counter; counter := counter + 1; };
   printf "        ]\n         }\n";
   printf "    IndexedFaceSet { coordIndex [\n";
   foreach facet jff do printf "        %g,%g,%g,-1,\n",
      jff.vertex[1].order_num,jff.vertex[2].order_num,jff.vertex[3].order_num;
   printf "           ] \n";
   printf "    materialIndex [\n";
   foreach facet jff do printf "   %g,\n",jff.color;
   printf "    ]\n";
   printf "     }\n";
   printf "  Material { ambientColor 0 0 0 diffuseColor 0 0 0 }\n";
   printf "    IndexedLineSet { coordIndex [\n";
   if edge_flag then
     foreach edge jee do printf "       %g,%g,-1,\n",
      jee.vertex[1].order_num,jee.vertex[2].order_num
   else  foreach edge jee where valence != 2 do printf "       %g,%g,-1,\n",
      jee.vertex[1].order_num,jee.vertex[2].order_num;
   printf "          ] } \n";
   printf "   }\n";
   printf "}\n";

} // end vrml

