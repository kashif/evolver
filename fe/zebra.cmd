// zebra.cmd
// Evolver command to alternately color string edges black and white

// Usage: Set color1 and color2 to the two colors you want, if the
//        default black and white are not suitable.  Then run "zebra".

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

color1 := white
color2 := black
zebra := {
   local ecount,first_e,v_id,zcolor,zinx,e_id;

   ecount := 0; // for safety

   // Look for valence 2 vertices with same color edges and
   // propagate zebra coloring in both directions.
   foreach vertex vv where valence==2 and vv.edge[1].color == vv.edge[2].color do
   {
     first_e := vv.edge[1].oid; 
     v_id := vv.id;
     zcolor := color1;

     for ( zinx := 1 ; zinx <= 2 ; zinx += 1 )
     {
       // follow connected edges
       e_id := first_e;
       do 
       { set edge[e_id] color zcolor;
         v_id := edge[e_id].vertex[2].id;
         if vertex[v_id].valence != 2 then break;
         if vertex[v_id].edge[1].oid == -e_id then
           e_id := vertex[v_id].edge[2].oid
         else
           e_id := vertex[v_id].edge[1].oid;
         zcolor := (zcolor == color1) ? color2 : color1;
         ecount := ecount + 1;
       } while ( (v_id != vv.id) and (ecount <= edge_count) );

       if ( v_id == vv.id ) then break; // don't need to go around the other way
       // set things up to go around other way next time through the loop.
       v_id := vv.id;
       zcolor := color2;
       first_e := vv.edge[2].oid;
     }

   } // end valence 2 vertex loop
} // end zebra
