// multiplicate.cmd

// Surface Evolver script to create datafile with surface duplicated
// according to view transforms in effect.  Writes datafile to stdout.
// Does not create new elements in current surface, since together 
// with view transforms, that would result in quadratic explosion.

// WARNING: This loses all element attributes in the output datafile.
// But does preserve "edgetype" attribute

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// usage: multiplicate >>> "newdatafile.fe"

define edge attribute edgetype integer // in case doesn't exist

eps := 1e-4  // tolerance for identifying vertices

// Hash table for finding identified vertices.  Generates hash value
// from skew plane.
mskew1 := 0.361234123413728768
mskew2 := 0.725423451231725277
mskew3 := 0.5227723243451234514
vertex_hash_init := {
  vertex_hash_table_size := vertex_count*transform_count;
  define vertex_hash_table integer[vertex_hash_table_size];
  define vertex_hash_chain integer[vertex_hash_table_size];
  define newcoord real[vertex_hash_table_size][3];
  vertex_entry_count := 0;
}

// deallocate memory
vertex_hash_end := {
  define vertex_hash_table integer[0];
  define vertex_hash_chain integer[0];
  define newcoord real[0][0];
}

// Returns sequential number of found or new vertex.
function integer vertex_hash_add(real xx, real yy, real zz)
{ local hashval,hashspot;
  local vnum,dist;

  hashval := floor((mskew1*xx + mskew2*yy + mskew3*zz)/eps);
  hashspot := hashval imod vertex_hash_table_size;

  // See if there
  for ( vnum := vertex_hash_table[hashspot] ; vnum != 0 ;
             vnum := vertex_hash_chain[vnum] )
  {
    dist := sqrt((xx-newcoord[vnum][1])^2 + (yy-newcoord[vnum][2])^2 +
                 (zz-newcoord[vnum][3])^2);
    if dist < eps then return vnum;
  };

  // Not there, so add
  vertex_entry_count += 1;
  vnum := vertex_entry_count;
  vertex_hash_chain[vnum] := vertex_hash_table[hashspot];
  vertex_hash_table[hashspot] := vnum;
  newcoord[vnum][1] := xx;
  newcoord[vnum][2] := yy;
  newcoord[vnum][3] := zz;
  return vnum;
 
}

// Hash table for finding identified edges.
edge_hash_init := {
  edge_hash_table_size := edge_count*transform_count;
  define edge_hash_table integer[edge_hash_table_size];
  define edge_hash_chain integer[edge_hash_table_size];
  define new_edge_verts integer[edge_hash_table_size][2];
  edge_entry_count := 0;
}

// deallocate memory
edge_hash_end := {
  define edge_hash_table integer[0];
  define edge_hash_chain integer[0];
  define new_edge_verts integer[0][0];
}

// Edge hasher. Returns sequential number (signed) of found or new edge.
function integer edge_hash_add(integer tailv, integer headv)
{ local temp,signflag;
  local hashval,hashspot;
  local edgenum;

  // get vertices in canonical order
  if tailv > headv then
  { temp := tailv;
    tailv := headv;
    headv := temp;
    signflag := -1;
  }
  else signflag := 1;

  hashval := tailv*737 + headv;
  hashspot := hashval imod edge_hash_table_size;

  // See if there
  for ( edgenum := edge_hash_table[hashspot] ; edgenum != 0 ;
             edgenum := edge_hash_chain[edgenum] )
  {
    if (tailv == new_edge_verts[edgenum][1]) and 
               (headv == new_edge_verts[edgenum][2])
      then return signflag*edgenum;
  };

  // Not there, so add
  edge_entry_count += 1;
  edgenum := edge_entry_count;
  edge_hash_chain[edgenum] := edge_hash_table[hashspot];
  edge_hash_table[hashspot] := edgenum;
  new_edge_verts[edgenum][1] := tailv;
  new_edge_verts[edgenum][2] := headv;
  return signflag*edgenum;
 
}
multiplicate :=  {

   local tcount,high_vertex,vx,vy,vz;
   local high_edge,thistail,thishead,fstride,tdet,edge1,edge2,edge3;

   list topinfo;

   define vertex attribute tx real[transform_count];
   define vertex attribute ty real[transform_count];
   define vertex attribute tz real[transform_count];
   define vertex attribute valias integer[transform_count];
   define edge attribute ehead integer[transform_count];
   define edge attribute etail integer[transform_count];
   define edge attribute ealias integer[transform_count];

   printf "\nVertices\n";
   vertex_hash_init;
   tcount := 1;
   high_vertex := 0;
   while ( tcount <= transform_count ) do
   { foreach vertex vv do
     { 
       vx := view_transforms[tcount][1][1]*vv.x
                       + view_transforms[tcount][1][2]*vv.y
                       + view_transforms[tcount][1][3]*vv.z
                       + view_transforms[tcount][1][4]*1;
       vy := view_transforms[tcount][2][1]*vv.x
                       + view_transforms[tcount][2][2]*vv.y
                       + view_transforms[tcount][2][3]*vv.z
                       + view_transforms[tcount][2][4]*1;
       vz := view_transforms[tcount][3][1]*vv.x
                       + view_transforms[tcount][3][2]*vv.y
                       + view_transforms[tcount][3][3]*vv.z
                       + view_transforms[tcount][3][4]*1;
       vv.tx[tcount] := vx; vv.ty[tcount] := vy; vv.tz[tcount] := vz;
       // search for alias
       vv.valias[tcount] := vertex_hash_add(vx,vy,vz);
       if vv.valias[tcount] > high_vertex then
       { printf "%d  %18.15f %18.15f %18.15f ",vv.valias[tcount],
             vx,vy,vz;
         printf "\n";
         high_vertex := vv.valias[tcount];
       };
     };
     tcount += 1;
   };
   vertex_hash_end;

   printf "\nEdges\n";
   tcount := 1;
   edge_hash_init;
   high_edge := 0;
   while ( tcount <= transform_count ) do
   { foreach edge ee do
     { 
       thistail :=  vertex[ee.vertex[1].id].valias[tcount];
       thishead :=  vertex[ee.vertex[2].id].valias[tcount];
       ee.ehead[tcount] := thishead;
       ee.etail[tcount] := thistail;
       // search for aliases
       ee.ealias[tcount] := edge_hash_add(thistail,thishead);
       if ( abs(ee.ealias[tcount]) > high_edge ) then
       { printf "%d   %d %d edgetype %d",abs(ee.ealias[tcount]),
              minimum(thistail,thishead),maximum(thistail,thishead),edgetype;
         if ee.bare then printf " bare ";
         printf "\n";
         high_edge := abs(ee.ealias[tcount]);
       };
     };
     tcount += 1;

   };
   edge_hash_end;

   printf "\nFaces\n";
   fstride := max(facet,id);
   tcount := 1;
   while ( tcount <= transform_count ) do
   { tdet := view_transforms[tcount][1][1]*
         (view_transforms[tcount][2][2]*view_transforms[tcount][3][3] 
          - view_transforms[tcount][3][2]*view_transforms[tcount][2][3]) 
        - view_transforms[tcount][1][2]*
         (view_transforms[tcount][2][1]*view_transforms[tcount][3][3] 
          - view_transforms[tcount][3][1]*view_transforms[tcount][2][3]) 
        + view_transforms[tcount][1][3]*
         (view_transforms[tcount][2][1]*view_transforms[tcount][3][2] 
          - view_transforms[tcount][3][1]*view_transforms[tcount][2][2]);
     foreach facet ff do 
     { edge1 := edge[ff.edge[1].id].ealias[tcount];
       edge2 := edge[ff.edge[2].id].ealias[tcount];
       edge3 := edge[ff.edge[3].id].ealias[tcount];
       if ( view_transform_swap_colors[tcount] != (tdet < 0.0) ) then
       // inverted
       printf "%d   %d %d %d\n",ff.id + (tcount-1)*fstride,
         ((ff.edge[3].oid > 0) ? -edge3 : edge3),
         ((ff.edge[2].oid > 0) ? -edge2 : edge2),
         ((ff.edge[1].oid > 0) ? -edge1 : edge1)
       else
       printf "%d   %d %d %d\n",ff.id + (tcount-1)*fstride,
         ((ff.edge[1].oid > 0) ? edge1 : -edge1),
         ((ff.edge[2].oid > 0) ? edge2 : -edge2),
         ((ff.edge[3].oid > 0) ? edge3 : -edge3);
     };
     tcount += 1;
   };

   // not listing bottominfo on purpose; too much extraneous stuff

   // free attribute storage
   define vertex attribute tx real[0];
   define vertex attribute ty real[0];
   define vertex attribute tz real[0];
   define vertex attribute valias integer[0];
   define edge attribute ehead integer[0];
   define edge attribute etail integer[0];
   define edge attribute ealias integer[0];

}

aa := 1
pview := { 
   printf "%f %f %f %f\n",view_transforms[aa][1][1],view_transforms[aa][1][2],
         view_transforms[aa][1][3],view_transforms[aa][1][4];
   printf "%f %f %f %f\n",view_transforms[aa][2][1],view_transforms[aa][2][2],
         view_transforms[aa][2][3],view_transforms[aa][2][4];
   printf "%f %f %f %f\n",view_transforms[aa][3][1],view_transforms[aa][3][2],
         view_transforms[aa][3][3],view_transforms[aa][3][4];
   printf "%f %f %f %f\n",view_transforms[aa][4][1],view_transforms[aa][4][2],
         view_transforms[aa][4][3],view_transforms[aa][4][4];
}

// Paste things together that didn't get pasted, by finding edges that
// come out of vertices in the same direction.
paste := { 
  local paste_count;

  paste_count := 0;  // track how many found, so know when no more left
  foreach vertex vv do
  { foreach vv.edge ee do
    { 
       foreach vv.edge eee do
       { if ee.id == eee.id then continue;
         if (ee.x-eee.x)^2 + (ee.y-eee.y)^2 + (ee.z-eee.z)^2 < eps^2 then
         { edge_merge(ee.oid,eee.oid);
           paste_count += 1;
           break;
         }
       }
     }
   };
   printf "Edges pasted: %d\n",paste_count;
}
 

// usage: Set view transforms as desired, then do
//        multiplicate >>> "newdatafile.fe"
