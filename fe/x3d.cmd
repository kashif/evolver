// x3d.cmd
// Surface Evolver script to export surface as an X3D file for 3D viewing
// in a browser.
// Usage:  x3d >>> "filename.x3d"

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

define rgb real[16][4];
 rgb[1][1] := 0.0; rgb[1][2] := 0.0; rgb[1][3] := 0.0;
 rgb[2][1] := 0.0; rgb[2][2] := 0.0; rgb[2][3] := 1.;
 rgb[3][1] := 0.0; rgb[3][2] := 1.; rgb[3][3] := 0.0;
 rgb[4][1] := 0.0; rgb[4][2] := 1.; rgb[4][3] := 1.;
 rgb[5][1] := 1.; rgb[5][2] := 0.0; rgb[5][3] := 0.0;
 rgb[6][1] := 1.; rgb[6][2] := 0.0; rgb[6][3] := 1.;
 rgb[7][1] := 1.; rgb[7][2] := 0.5; rgb[7][3] := 0.;
 rgb[8][1] := .6; rgb[8][2] := .6; rgb[8][3] := .6;
 rgb[9][1] := .3; rgb[9][2] := .3; rgb[9][3] := .3;
 rgb[10][1] := .3; rgb[10][2] := .8; rgb[10][3] := 1.;
 rgb[11][1] := .5; rgb[11][2] := 1.; rgb[11][3] := .5;
 rgb[12][1] := .5; rgb[12][2] := 1.; rgb[12][3] := 1.;
 rgb[13][1] := 1.; rgb[13][2] := .5; rgb[13][3] := .5;
 rgb[14][1] := 1.; rgb[14][2] := .5; rgb[14][3] := 1;
 rgb[15][1] := 1.; rgb[15][2] := 1.; rgb[15][3] := .0;
 rgb[16][1] := 1; rgb[16][2] := 1; rgb[16][3] := 1;

x3d := {
 printf"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
 printf"<!DOCTYPE X3D PUBLIC \"http://www.web3D.org/TaskGroups/x3d/translation/x3d-draft.dtd\" \"file://localhost/C:/www.web3D.org/TaskGroups/x3d/translation/x3d-draft.dtd\" >\n";
 printf "<X3D>\n";
 printf "  <Header>\n";
 printf "    <meta name=\"file\" content=\"%s.x3d\"/>\n",datafilename;
 printf "    <meta name=\"description\" content=\"triangulated surface\"/>\n";
 printf "    <meta name=\"generator\" content=\"Surface Evolver, x3d.cmd\"/>\n";
 printf "  </Header>\n";
 printf "  <Scene>\n";
 printf "    <Transform>\n";
 foreach facet ff where color >= 0 do
 { printf "      <Shape>\n";
   printf "        <Appearance>\n";
   printf "          <Material diffuseColor=\"%f %f %f\"/>\n", rgb[ff.color+1][1],
                       rgb[ff.color+1][2],rgb[ff.color+1][3];
   printf "        </Appearance>\n";
   printf "        <IndexedFaceSet coordIndex=\"0 1 2 0 -1\" solid=\"FALSE\">\n";
   printf "          <Coordinate point=\"%f %f %f, %f %f %f, %f %f %f\"/>\n",
                      ff.vertex[1].x,ff.vertex[1].y,ff.vertex[1].z, 
                      ff.vertex[2].x,ff.vertex[2].y,ff.vertex[2].z, 
                      ff.vertex[3].x,ff.vertex[3].y,ff.vertex[3].z;
   printf "        </IndexedFaceSet>\n";
   printf "      </Shape>\n";
 };
 printf "    </Transform>\n";
 printf "  </Scene>\n";
 printf "</X3D>\n";
}
