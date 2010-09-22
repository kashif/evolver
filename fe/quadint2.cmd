// quadint2.cmd
// For detecting intersection of quadratic facets in 3D
// Not suitable for torus model.
// Needs quadbbox.cmd to be loaded first.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// Usage: quadmeet
// Prints id numbers of pairs of intersecting facets and colors them green.

a_refine_order := 4
b_refine_order := 5

eps := 1e-10  // tolerance for being equal to zero
ax := 0; bx := 0; cx := 0; ay := 0; by := 0; cy := 0; az := 0; bz := 0; cz := 0;
px := 0; py := 0; pz := 0; qx := 0; qy := 0; qz:= 0;
uuu := 0; vvv := 0;
xa1 := 0; xa2 := 0; xa3 := 0; xa4 := 0; xa5 := 0; xa6 := 0
ya1 := 0; ya2 := 0; ya3 := 0; ya4 := 0; ya5 := 0; ya6 := 0
za1 := 0; za2 := 0; za3 := 0; za4 := 0; za5 := 0; za6 := 0
intersection_found := 0; 

// detection of triangle and edge intersection 
detect :=   { 
        local acx,acy,acz,bcx,bcy,bcz,qpx,qpy,qpz,qcx,qcy,qcz;
        local denom,lambda,mu,sigma;
        do {
        acx := ax-cx; acy := ay-cy; acz := az-cz;
        bcx := bx-cx; bcy := by-cy; bcz := bz-cz;
        qpx := qx-px; qpy := qy-py; qpz := qz-pz;
        qcx := qx-cx; qcy := qy-cy; qcz := qz-cz;
        denom := qpx*(acy*bcz-acz*bcy) - qpy*(acx*bcz-acz*bcx)
                + qpz*(acx*bcy-acy*bcx);
                if ( abs(denom) < eps ) then break;
        lambda := (qpx*(qcy*bcz-qcz*bcy) - qpy*(qcx*bcz-qcz*bcx)
                + qpz*(qcx*bcy-qcy*bcx))/denom;
                if ( lambda <= eps ) then break;
        mu := (qpx*(acy*qcz-acz*qcy) - qpy*(acx*qcz-acz*qcx)
                + qpz*(acx*qcy-acy*qcx))/denom;
                if ( mu <= eps ) then break;
        if ( lambda + mu > 1-eps ) then break;
        sigma := (qcx*(acy*bcz-acz*bcy) - qcy*(acx*bcz-acz*bcx)
                + qcz*(acx*bcy-acy*bcx))/denom;
        if ( (sigma <= eps) or (sigma > 1-eps) ) then break;

        // now have intersection 
        intersection_found := 1;
      } while 0;  // so we could use break

} // end detect

calccoords := {
           xx := 0.5*(uuu+vvv-1)*(uuu+vvv-2)*xa1 + uuu*(2-uuu-vvv)*xa2
              + 0.5*uuu*(uuu-1)*xa3 + vvv*(2-uuu-vvv)*xa4
              + uuu*vvv*xa5 + 0.5*vvv*(vvv-1)*xa6; 
           yy := 0.5*(uuu+vvv-1)*(uuu+vvv-2)*ya1 + uuu*(2-uuu-vvv)*ya2
              + 0.5*uuu*(uuu-1)*ya3 + vvv*(2-uuu-vvv)*ya4
              + uuu*vvv*ya5 + 0.5*vvv*(vvv-1)*ya6; 
           zz := 0.5*(uuu+vvv-1)*(uuu+vvv-2)*za1 + uuu*(2-uuu-vvv)*za2
              + 0.5*uuu*(uuu-1)*za3 + vvv*(2-uuu-vvv)*za4
              + uuu*vvv*za5 + 0.5*vvv*(vvv-1)*za6; 
} // end calccoords

refine_and_test := { 
         // Test intersections of planar subtriangles and sub edges
         adelta := 2/a_refine_order;
         bdelta := 2/b_refine_order;
         ua := 0;
         while ( ua < 1.9999 ) do
         { va := 0;
           while ( va < 1.9999 - ua ) do
           {  uuu := ua; vvv := va; calccoords; ax := xx; ay := yy; az := zz;
              uuu := ua+adelta; vvv := va; calccoords; bx := xx; by := yy; bz := zz;
              uuu := ua; vvv := va+adelta; calccoords; cx := xx; cy := yy; cz := zz;
              ub := 0;
              while ( ub < 1.9999 ) do
              { vb := 0;
                while ( vb < 1.9999 - ub ) do
                {
                  uuu := ub; vvv := vb; calccoords; px := xx; py := yy; pz := zz;
                  uuu := ub+bdelta; vvv := vb; calccoords; qx := xx; qy := yy; qz := zz;
                  detect; if intersection_found then break;
                  uuu := ub; vvv := vb+bdelta; calccoords; px := xx; py := yy; pz := zz;
                  detect; if intersection_found then break;
                  uuu := ub; vvv := vb; calccoords; qx := xx; qy := yy; qz := zz;
                  detect;  if intersection_found then break;
                  vb := vb + bdelta;
                };
                if intersection_found then break;
                ub := ub + bdelta;
              };
              if intersection_found then break;
              va := va + adelta;
           };
           if intersection_found then break;
           ua := ua + adelta;
         }    
    } // end  refine_and_test
  
quadmeet := { 
    intersect_total := 0;
    fboxes;   // find facet bounding boxes
    // test all facet pairs
    foreach facet fa do
    { foreach facet fb where fb.id > fa.id do
      {  // check adjacency
         if ( fa.vertex[1].id == fb.vertex[1].id ) then continue;
         if ( fa.vertex[2].id == fb.vertex[1].id ) then continue;
         if ( fa.vertex[3].id == fb.vertex[1].id ) then continue;
         if ( fa.vertex[1].id == fb.vertex[2].id ) then continue;
         if ( fa.vertex[2].id == fb.vertex[2].id ) then continue;
         if ( fa.vertex[3].id == fb.vertex[2].id ) then continue;
         if ( fa.vertex[1].id == fb.vertex[3].id ) then continue;
         if ( fa.vertex[2].id == fb.vertex[3].id ) then continue;
         if ( fa.vertex[3].id == fb.vertex[3].id ) then continue;
         // first, check bounding boxes
         if ( fb.fbox[1] >= fa.fbox[2] or
              fb.fbox[2] <= fa.fbox[1] or
              fb.fbox[3] >= fa.fbox[4] or
              fb.fbox[4] <= fa.fbox[3] or
              fb.fbox[5] >= fa.fbox[6] or
              fb.fbox[6] <= fa.fbox[5] ) then continue;

         // extract coordinates
         xa1 := fa.edge[1].vertex[1].x;
         xa2 := fa.edge[1].vertex[3].x;
         xa3 := fa.edge[1].vertex[2].x;
         xa4 := fa.edge[3].vertex[3].x;
         xa5 := fa.edge[2].vertex[3].x;
         xa6 := fa.edge[2].vertex[2].x;
         ya1 := fa.edge[1].vertex[1].y;
         ya2 := fa.edge[1].vertex[3].y;
         ya3 := fa.edge[1].vertex[2].y;
         ya4 := fa.edge[3].vertex[3].y;
         ya5 := fa.edge[2].vertex[3].y;
         ya6 := fa.edge[2].vertex[2].y;
         za1 := fa.edge[1].vertex[1].z;
         za2 := fa.edge[1].vertex[3].z;
         za3 := fa.edge[1].vertex[2].z;
         za4 := fa.edge[3].vertex[3].z;
         za5 := fa.edge[2].vertex[3].z;
         za6 := fa.edge[2].vertex[2].z;
         xb1 := fb.edge[1].vertex[1].x;
         xb2 := fb.edge[1].vertex[3].x;
         xb3 := fb.edge[1].vertex[2].x;
         xb4 := fb.edge[3].vertex[3].x;
         xb5 := fb.edge[2].vertex[3].x;
         xb6 := fb.edge[2].vertex[2].x;
         yb1 := fb.edge[1].vertex[1].y;
         yb2 := fb.edge[1].vertex[3].y;
         yb3 := fb.edge[1].vertex[2].y;
         yb4 := fb.edge[3].vertex[3].y;
         yb5 := fb.edge[2].vertex[3].y;
         yb6 := fb.edge[2].vertex[2].y;
         zb1 := fb.edge[1].vertex[1].z;
         zb2 := fb.edge[1].vertex[3].z;
         zb3 := fb.edge[1].vertex[2].z;
         zb4 := fb.edge[3].vertex[3].z;
         zb5 := fb.edge[2].vertex[3].z;
         zb6 := fb.edge[2].vertex[2].z;

         refine_and_test;
         if intersection_found then
          { intersect_total := intersect_total + 1;
            set fa color green; set fb color green;
            printf "Facets %g and %g intersect.\n",fa.id,fb.id;
          }
      } 
  };
  printf "Intersections found: %g\n",intersect_total;
} // end quadmeet
