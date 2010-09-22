// gaussequi.cmd

// Equiangulation using Gauss map for swap criterion.
// But has problems; can produce zero area facets.  Also, swapping an
// edge changes the Gauss map, so maybe things don't improve.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// Usage: gaussequi

gaussequi := {
   local ax,ay,az,bx,by,bz,ffnum,cx,cy,cz,dx,dy,dz,aa,bb,cc,dd,ee;
   swapcount := 0;
   foreach edge eee where not fixed and valence == 2 do
   { ax := eee.vertex[1].vertexnormal[1];
     ay := eee.vertex[1].vertexnormal[2];
     az := eee.vertex[1].vertexnormal[3];
     bx := eee.vertex[2].vertexnormal[1];
     by := eee.vertex[2].vertexnormal[2];
     bz := eee.vertex[2].vertexnormal[3];
     ffnum := eee.facet[1].id;
     foreach facet[ffnum].vertex vv do
     { if vv.id != eee.vertex[1].id and vv.id != eee.vertex[2].id then
       { vvnum := vv.id; break; }
     };
     cx := vertex[vvnum].vertexnormal[1];
     cy := vertex[vvnum].vertexnormal[2];
     cz := vertex[vvnum].vertexnormal[3];
     ffnum := eee.facet[2].id;
     foreach facet[ffnum].vertex vv do
     { if vv.id != eee.vertex[1].id and vv.id != eee.vertex[2].id then
       { vvnum := vv.id; break; }
     };
     dx := vertex[vvnum].vertexnormal[1];
     dy := vertex[vvnum].vertexnormal[2];
     dz := vertex[vvnum].vertexnormal[3];
     
     aa := (ax-bx)^2+(ay-by)^2+(az-bz)^2;
     bb := (ax-cx)^2+(ay-cy)^2+(az-cz)^2;
     cc := (bx-cx)^2+(by-cy)^2+(bz-cz)^2;
     dd := (ax-dx)^2+(ay-dy)^2+(az-dz)^2;
     ee := (bx-dx)^2+(by-dy)^2+(bz-dz)^2;

     if ( (bb+cc-aa)*sqrt(dd*ee) + (dd+ee-aa)*sqrt(bb*cc) < 0 ) then
     { edgeswap eee;
       swapcount += 1;
     };
   };
   printf "Edges gaussequi swapped: %d\n",swapcount;
}

