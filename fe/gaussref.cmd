// gaussref.cmd

// Refining using Gauss map as criterion.
// Refines edges where difference in normal
// exceeds user-set amount.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// Usage: set the variable gaussref_tolerance, and do gaussref.

// Set by user; difference in normals in radians
gaussref_tolerance := 0.3


gaussref := {
   local ax,ay,az,bx,by,bz,diff,maga,magb,alen,maxdiff,triples,blen;
   local gaussref_count;

   gaussref_count := 0;
   foreach edge eee do
   { if  max ( eee.vertex[1].edge,valence) <= 2 and
         max ( eee.vertex[2].edge,valence) <= 2  then
     { ax := eee.vertex[1].vertexnormal[1];
       ay := eee.vertex[1].vertexnormal[2];
       az := eee.vertex[1].vertexnormal[3];
       bx := eee.vertex[2].vertexnormal[1];
       by := eee.vertex[2].vertexnormal[2];
       bz := eee.vertex[2].vertexnormal[3];
       diff := pi/2 - abs(pi/2 - acos(ax*bx+ay*by+az*bz)) ;
       if ( diff > gaussref_tolerance )
         then { 
           refine eee; gaussref_count += 1; 
         }
     }
     else if eee.valence == 2 then
     { ax := eee.facet[1].x[1];
       ay := eee.facet[1].x[2];
       az := eee.facet[1].x[3];
       bx := eee.facet[2].x[1];
       by := eee.facet[2].x[2];
       bz := eee.facet[2].x[3];
       maga := sqrt(ax^2+ay^2+az^2);
       magb := sqrt(bx^2+by^2+bz^2);
       diff := pi/2 - abs(pi/2 - acos((ax*bx+ay*by+az*bz)/maga/magb)) ;
       if ( diff > gaussref_tolerance )
         then { 
           refine eee; gaussref_count += 1; 
         }
     }
     else if eee.valence >= 3 then
     { // check bend in continuation triple lines
       ax := eee.x;
       ay := eee.y;
       az := eee.z;
       alen := eee.length;
       maxdiff := 0;
       triples := 0;
       foreach eee.vertex vvv do
       { foreach vvv.edge eeee where (eeee.id != eee.id) and (valence >= 3) do
         { triples += 1;
           bx := eeee.x;
           by := eeee.y;
           bz := eeee.z;
           blen := eeee.length;
           diff := pi/2 - abs(pi/2 - acos((ax*bx+ay*by+az*bz)/alen/blen)) ;
           if ( diff > gaussref_tolerance )
           then { 
             if ( diff > maxdiff ) then maxdiff := diff;
           }
         }
       };
       if ( (triples <= 1) and (maxdiff > gaussref_tolerance) )
           then { 
             refine eee; gaussref_count += 1; 
           }
     }
   };
  printf "Edges refined by gaussref: %d\n",gaussref_count;
}

