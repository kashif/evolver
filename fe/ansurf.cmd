// ansurf.cmd
// Surface Evolver command to produce file of ANSYS input for 
//   vertices, edges,  and faces to produce a surface
//   for ANSYS meshing.  Beware this is a very simple-minded
//   translation to ANSYS format.
// Modified Feb. 2006 to not assume consecutive numbering of elements.
// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// usage:  ansurf >>> "filename"

// Attributes to hold ANSYS numbering of elements
define vertex attribute ansurf_knumber real
define edge attribute ansurf_lnumber real
define facet attribute ansurf_alnumber real

// vertices as ANSYS keypoints
ansurf_nodes := { 
       local knumber;
       knumber := 0;
       foreach vertex vv do
       { printf "k,,%20.15g,%20.15g,%20.15g\n",x,y,z;
         knumber += 1;
         vv.ansurf_knumber := knumber;
       }
     }

ansurf_edges := {
       local lnumber;
       lnumber := 0;
       if (quadratic) then 
       foreach edge ee do
       { printf "larc,%g,%g,%g\n",ee.vertex[1].ansurf_knumber,
           ee.vertex[2].ansurf_knumber,ee.vertex[3].ansurf_knumber;
         lnumber += 1;
         ee.ansurf_lnumber := lnumber;
       }
       else
       foreach edge ee do
       { printf "l,%g,%g\n",ee.vertex[1].ansurf_knumber,
             ee.vertex[2].ansurf_knumber;
         lnumber += 1;
         ee.ansurf_lnumber := lnumber;
       }
     }

ansurf_faces := {
       local alnumber;
       alnumber := 0;
       foreach facet ff do
       { printf "al,%g,%g,%g\n",
           ff.edge[1].ansurf_lnumber,ff.edge[2].ansurf_lnumber,
             ff.edge[3].ansurf_lnumber;
         alnumber += 1;
         ff.ansurf_alnumber := alnumber; 
       }
 }

// define volumes, one per body
ansurf_bodies := { foreach body bb do
     { // select areas
       local flag;
       flag := 0;
       foreach bb.facet ff do
       { if flag then printf "ASEL,A,AREA,,%g,%g\n",ff.ansurf_alnumber,
           ff.ansurf_alnumber
         else printf "ASEL,S,AREA,,%g,%g\n",ff.ansurf_alnumber,
           ff.ansurf_alnumber;
         flag := 1;
       };
       printf "VA,ALL\n";
     }
  }

// define areas
ansurf := { printf "/PREP7\n";
            printf "/NOPR\n";
            ansurf_nodes;
            ansurf_edges;
            ansurf_faces;
            ansurf_bodies;
        }

// Usage: ansurf >>> "ansys_file"
