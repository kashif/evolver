// dirichlet_to_disk.cmd

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// For mapping simply connected regions to unit disk minimizing Dirichlet
// energy to get conformal mappings.

// Prerequisites: Dirichlet energy defined, but no others.
// Usage: Remove all constraints and boundaries.  Run to_disk.  Then evolve.

define constraint circle_con formula: x^2 + y^2 = 1
define facet attribute form_factors real[3]

set_ff := {
  foreach facet ff do
  {  ff.form_factors[1] := ff.edge[1].length^2;
     ff.form_factors[2] := -(ff.edge[1].x*ff.edge[3].x
                        + ff.edge[1].y*ff.edge[3].y);
     ff.form_factors[3] := ff.edge[3].length^2;
  }
}

// Mapping to triangle.  Especially nice since can use the three conformal
// degrees of freedom to map particular vertices to corners, and straight
// sides should let Hessian minimize Dirichlet energy in one step.
define cornerx real[4];
cornerx[1] :=  1.0;
cornerx[2] :=  0.0;
cornerx[3] := -1.0;
cornerx[4] :=  1.0;
define cornery real[4];
cornery[1] := 0.0;
cornery[2] := sqrt(3);
cornery[3] := 0.0;
cornery[4] := 0.0;
define constraint side_1_con formula: sqrt(3)*x + y = sqrt(3);
define constraint side_2_con formula: -sqrt(3)*x + y = sqrt(3);
define constraint side_3_con formula: y = 0;
define cornerv integer[3]
cornerv[1] := 0
cornerv[2] := 0
cornerv[3] := 0
define sidecounts integer[3]  // how many sides mapped to each triangle side
to_triangle := {
   // Check simple connectivity
   if vertex_count - edge_count + facet_count != 1 then
   { errprintf "to_disk error: Surface not simply connected. Aborting.\n";
     return;
   };
   set_ff;  // set form factors
   set facet tension 0;

   // Go around outside edges, mapping to unit circle
   unfix vertices;
   if ( valid_element(vertex[cornerv[1]]) and
           (sum(vertex[cornerv[1]].edge,valence==1) == 2) and
        valid_element(vertex[cornerv[2]]) and
           (sum(vertex[cornerv[2]].edge,valence==1) == 2) and
        valid_element(vertex[cornerv[3]]) and
           (sum(vertex[cornerv[3]].edge,valence==1) == 2) ) then
   { foreach vertex[cornerv[1]].edge where valence == 1 do
     { thise := oid;
       break;
     };
     starte := thise;
     counter := 0;
     sidenumber := 1;
     do 
     { foreach edge[thise].vertex[2].edge eee where valence == 1 and
       eee.id != thise do
       { nexte := eee.oid; break; };
       thise := nexte;
       counter += 1;
       if ( (edge[thise].vertex[1].id == cornerv[1]) or
          (edge[thise].vertex[1].id == cornerv[2]) or
          (edge[thise].vertex[1].id == cornerv[3]) ) then
       { sidecounts[sidenumber] := counter;
         counter := 0;
         sidenumber += 1;
       };
     } while thise != starte;
   }
   else
   { printf "Legal corner vertices not given, so doing my own.\n";
     rimcount := sum(edge,valence==1);
     sidecounts[1] := rimcount idiv 3;
     sidecounts[2] := rimcount idiv 3;
     sidecounts[3] := rimcount - sidecounts[1] - sidecounts[2];
     foreach edge ee where valence ==  1 do { starte := ee.id; break; };
   };
   // Now go around moving vertices to triangle sides
   thise := starte;
   for ( sidenum := 1 ; sidenum <= 3;  sidenum += 1 )
   {
     for ( counter := 1; counter <= sidecounts[sidenum] ; counter += 1 )
     { lambda := counter/sidecounts[sidenum];
       edge[thise].vertex[1].x := (1-lambda)*cornerx[sidenum]
                                   + lambda*cornerx[sidenum+1];
       edge[thise].vertex[1].y := (1-lambda)*cornery[sidenum]
                                   + lambda*cornery[sidenum+1];
       if ( space_dimension > 2) then
         edge[thise].vertex[1].x[3] := 0; 
       if  sidenum == 1 then
         set edge[thise].vertex[1] constraint side_1_con;
       if  sidenum == 2 then
         set edge[thise].vertex[1] constraint side_2_con;
       if  sidenum == 3 then
         set edge[thise].vertex[1] constraint side_3_con;

       foreach edge[thise].vertex[2].edge eee where valence == 1 and
         eee.id != thise do
       { nexte := eee.oid; break; };
       thise := nexte;
     };
  }; 
  // Big move with rim fixed, since movable rim can have negative
  // eigenvalues.
  fix vertex where on_constraint circle_con;
  hessian;
  unfix vertex;
}
 

to_disk := {
   // Check simple connectivity
   if vertex_count - edge_count + facet_count != 1 then
   { errprintf "to_disk error: Surface not simply connected. Aborting.\n";
     return;
   };
   set_ff;  // set form factors
   set facet tension 0;

   // Go around outside edges, mapping to unit circle
   unfix vertices;
   rimcount := sum(edge,valence==1);
   foreach edge ee where valence ==  1 do { starte := ee.id; break; };
   thise := starte;
   counter := 0;
   do 
   { 
     edge[thise].vertex[1].x := cos(counter/rimcount*2*pi); 
     edge[thise].vertex[1].y := sin(counter/rimcount*2*pi); 
     if ( space_dimension > 2) then
       edge[thise].vertex[1].x[3] := 0; 
     set edge[thise].vertex[1] constraint circle_con;
     foreach edge[thise].vertex[2].edge eee where valence == 1 and
       eee.id != thise do
     { nexte := eee.oid; break; };
     thise := nexte;
     counter += 1;
   } while thise != starte;
   
  // Big move with rim fixed, since movable rim can have negative
  // eigenvalues.
  fix vertex where on_constraint circle_con;
  hessian;
  unfix vertex;
}
 
   
   
// Prerequisites: Dirichlet energy defined, but no others.
// Usage: Remove all constraints and boundaries.  Run to_disk.  Then evolve.
