// povrays.cmd

// Surface Evolver command for producing POV-Ray input file.
// Output is to stdout, so redirect to desired file.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// Like povray.cmd, but with vertex normals for a smooth surface.
// A normal at a vertex of a facet is the average of all adjacent
//   facet normals within a certain angle of the original facet normal

// Usage: 
//        Use the "show edge where ..." command to declare which
//        edges are to be depicted as thin cylinders.
//        Set "edge_radius" to desired radius of edge cylinders.
//        Set "critcos" to critical cosine of angle for facets
//           regarded as smoothly joined.
//        Run "povray" and redirect to desired file, e.g.
//            Enter command: povray >>> "something.pov"
// Run "povrays" with output redirected to a file, e.g.
//    Enter command: povrays >>> "something.pov"

edge_radius := 0.003; // adjust this for desired radius of edge cylinders
critcos := 0.8;  // criterion for vertex normal averaging.
povrays := {
   local ffx,ffy,ffz,ffnorm,vernum,nx,ny,nz,fffx,fffy,fffz,fffnorm;
   local costheta,nnorm;

   printf "// %s in POV-Ray format.\n\n",datafilename;

   printf "light_source { <0,0,300> color rgb <1,1,1> }\n";
   printf "light_source { <100,0,0> color rgb <1,1,1> }\n";
   printf "camera { location <12,0,0> sky <0,0,1>  // right handed \n";
   printf "         up <0,0,1> right <1.3,0,0> look_at <0,0,0> angle 15 }\n";
   printf "background { color <0.3,0.8,1.0> } // light blue\n\n";
   printf "// Textures corresponding to Evolver colors\n\n";
  
   printf "#declare t_black = texture { pigment { rgb <0.0,0.0,0.0> }}\n";
   printf "#declare t_blue = texture { pigment { rgb <0.0,0.0,1.,> }}\n";
   printf "#declare t_green = texture { pigment { rgb <0.0,1.,0.0,> }}\n";
   printf "#declare t_cyan = texture { pigment { rgb <0.0,1.,1.,> }}\n"; 
   printf "#declare t_red = texture { pigment { rgb <1.,0.0,0.0,> }}\n";
   printf "#declare t_magenta = texture { pigment { rgb <1.,0.0,1.,> }}\n";
   printf "#declare t_brown = texture { pigment { rgb <1.,0.5,0.,> }}\n";
   printf "#declare t_lightgray = texture { pigment { rgb <.6,.6,.6,> }}\n";
   printf "#declare t_darkgray = texture { pigment { rgb <.3,.3,.3,> }}\n";
   printf "#declare t_lightblue = texture { pigment { rgb <.3,.8,1.,> }}\n"; 
   printf "#declare t_lightgreen = texture { pigment { rgb <.5,1.,.5,> }}\n";
   printf "#declare t_lightcyan = texture { pigment { rgb <.5,1.,1.,> }}\n";
   printf "#declare t_lightred = texture { pigment { rgb <1.,.5,.5,> }}\n";
   printf "#declare t_lightmagenta = texture { pigment { rgb <1.,.5,1.,> }}\n";
   printf "#declare t_yellow = texture { pigment { rgb <1.,1.,.0,> }}\n";
   printf "#declare t_white = texture { pigment { rgb <1.,1.,1.,> }}\n"; 

   printf "\n//One overall object.\n";
   printf "union {\n";
   printf "// All facets in one big mesh object for efficiency.\n";
   printf "   mesh { \n";
   foreach facet ff do {

     printf "   smooth_triangle { ";
     ffx := ff.x; ffy := ff.y; ffz := ff.z;
     ffnorm := sqrt(ffx^2 + ffy^2 + ffz^2);
     vernum := 1; 
     while ( vernum <= 3 ) do
     { nx := 0; ny := 0; nz := 0;
       foreach ff.vertex[vernum].facet fff do
       { fffx := fff.x; fffy := fff.y; fffz := fff.z;
         fffnorm := sqrt(fffx^2 + fffy^2 + fffz^2);
         costheta := (fffx*ffx+fffy*ffy+fffz*ffz)/ffnorm/fffnorm;
         if ( costheta > critcos ) then
         { nx := nx + fffx/fffnorm;
           ny := ny + fffy/fffnorm;
           nz := nz + fffz/fffnorm;
         } 
         else if ( costheta < -critcos ) then  // in case nonorientable
         { nx := nx - fffx/fffnorm;
           ny := ny - fffy/fffnorm;
           nz := nz - fffz/fffnorm;
         } 
       };
       nnorm := sqrt(nx^2+ny^2+nz^2);
       if vernum > 1 then printf ",";
       printf "<%f,%f,%f>,<%f,%f,%f>",
        ff.vertex[vernum].x,ff.vertex[vernum].y,ff.vertex[vernum].z, 
        nx/nnorm,ny/nnorm,nz/nnorm;
       vernum := vernum + 1;
     };
      printf " texture {";
      if ( ff.color == white ) then printf " t_white "
      else if ( ff.color == black ) then printf " t_black "
      else if ( ff.color == blue) then printf " t_blue "
      else if ( ff.color == green ) then printf " t_green "
      else if ( ff.color == cyan ) then printf " t_cyan "
      else if ( ff.color == red ) then printf " t_red "
      else if ( ff.color == magenta ) then printf " t_magenta "
      else if ( ff.color == brown ) then printf " t_brown "
      else if ( ff.color == lightgray ) then printf " t_lightgray "
      else if ( ff.color == darkgray ) then printf " t_darkgray "
      else if ( ff.color == lightblue ) then printf " t_lightblue "
      else if ( ff.color == lightgreen ) then printf " t_lightgreen "
      else if ( ff.color == lightcyan ) then printf " t_lightcyan "
      else if ( ff.color == lightred ) then printf " t_lightred "
      else if ( ff.color == lightmagenta ) then printf " t_lightmagenta "
      else if ( ff.color == yellow ) then printf " t_yellow ";
      printf " } }\n";
   };
   printf "  }  // end of mesh object\n";

   // Do desired edges
   printf "#declare edge_radius = %f;\n",edge_radius; 
   foreach edge ee where ee.show do
   { printf "cylinder { <%f,%f,%f>,<%f,%f,%f> edge_radius texture { t_black } }\n",
       ee.vertex[1].x,ee.vertex[1].y,ee.vertex[1].z,
       ee.vertex[2].x,ee.vertex[2].y,ee.vertex[2].z;
   };

   // Windup
   printf "// overall viewing transformation\n";
   printf "  matrix < %f,%f,%f,\n",
        view_matrix[1][1],view_matrix[2][1],view_matrix[3][1];
   printf "           %f,%f,%f,\n",
        view_matrix[1][2],view_matrix[2][2],view_matrix[3][2];
   printf "           %f,%f,%f,\n",
        view_matrix[1][3],view_matrix[2][3],view_matrix[3][3];
   printf "           %f,%f,%f>\n",
        view_matrix[1][4],view_matrix[2][4],view_matrix[3][4];
   printf " }  // end of all objects\n";
}


