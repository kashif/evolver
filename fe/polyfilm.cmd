// polyfilm.cmd
// command to produce Polycut format file
// usage: polyfilm >>> "filename"

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

define vertex attribute offnumber integer;

// command to print OFF format part (the vertices and facets)
verlist := { local offnum;
             offnum := 0;
             foreach vertex vv do 
             { printf "%f %f %f \n",x,y,z;
               vv.offnumber := offnum;
               offnum += 1;
             }
}  // end verlist

facelist := { foreach facet ff do 
              { printf "3 ";
                foreach ff.vertex vv do printf "%g ",vv.offnumber; 
                printf "\n";
              }
}  // end facelist

do_off := { printf "OFF\n%g %g %g\n",vertex_count,facet_count,edge_count;
	        verlist;
            facelist
}  // end do_off

// vect-like file command for triple lines
traa := { trcount ::= sum(edges where valence==3,1)}
trbb := { foreach edge ee where ee.valence==3 do 
          { foreach ee.vertex vv do printf "%f %f %f  ",x,y,z ;
		    printf "\n";
          } 
}
trips := { printf "TRIPLE\n"; traa; printf "%g\n",trcount; trbb }

polyfilm := {do_off; trips}

