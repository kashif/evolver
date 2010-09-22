// order.cmd
// Evolver command to number string-model vertices consecutively.
// Result is order number in vertex extra attribute 'number'.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// Usage: order

define vertex attribute number integer

order := {
   local ecount,e_id,first_e,v_id,newv_id,newe_id;

   ecount := 0; // for safety
   // get starting edge, since can't assume edge[1] exists
   foreach edge do { e_id := id; break; };
   first_e := e_id;
   v_id := edge[e_id].vertex[1].id;

   // follow connected edges
   do 
   { set vertex[v_id] number ecount+1;
     foreach edge[e_id].vertex vv do 
     { if ( vv.id != v_id ) then
       { newv_id := vv.id;
         foreach vv.edge ee do 
         if ee.id != e_id then newe_id := ee.id
       }
     };
     e_id := newe_id; v_id := newv_id;
     ecount := ecount + 1;
   } while ( (e_id != first_e) and (ecount <= edge_count) )
 }
