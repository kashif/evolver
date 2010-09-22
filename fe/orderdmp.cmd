// orderdmp.cmd
// Evolver command to number string vertices consecutively  
// by means of extra attribute 'number',
//  and create an ordered dump file.
//  Pipe output to file to save it.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// Usage: oderdmp

define vertex attribute number integer

orderdmp := {
   local e_id,newv_id,ecount,first_e,v_id,newe_id;

   list topinfo;
   ecount := 0; // for safety
   // get starting edge, since can't assume edge[1] exists
   foreach edge do e_id := id;
   first_e := e_id;
   v_id := edge[e_id].vertex[1].id;
   // follow connected edges
   printf "\nVertices\n";
   do 
   { set vertex[v_id] number ecount+1;
     printf "%g  %g  %g  %g\n",ecount+1,vertex[v_id].x,vertex[v_id].y,
        vertex[v_id].z;
     foreach edge[e_id].vertex vv do 
     { if ( vv.id != v_id ) then
       { newv_id := vv.id;
         foreach vv.edge ee do 
           if ee.id != e_id then newe_id := ee.id
       }
     };
     e_id := newe_id; v_id := newv_id;
     ecount := ecount + 1;
   } while ( (e_id != first_e) and (ecount <= edge_count) );
   printf "\nEdges\n";
   foreach edge ee do 
   {
     printf"%g   %g %g \n",ee.id,ee.vertex[1].number,ee.vertex[2].number;
   };
   list bottominfo;
 }
