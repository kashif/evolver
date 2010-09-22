// quadmeet.cmd
// For detecting intersection of quadratic facets in 3D
// Not suitable for torus model.
// Needs quadbbox.cmd to be loaded first.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// Usage: quadmeet
// Results: Prints id numbers of intersecting facets.

tiny := 1e-20  // criterion for dist^2 = 0
quadmeet := { 
    local xa1,xa2,xa3,xa4,xa5,xa6;
    local ya1,ya2,ya3,ya4,ya5,ya6;
    local za1,za2,za3,za4,za5,za6;
    local xb1,xb2,xb3,xb4,xb5,xb6;
    local yb1,yb2,yb3,yb4,yb5,yb6;
    local zb1,zb2,zb3,zb4,zb5,zb6;
    local eps,ua,va,ub,vb,xa,ya,za,xb,yb,zb,dx,dy,dz,dd;
    local xau,xav,yau,yav,zau,zav;
    local xbu,xbv,ybu,ybv,zbu,zbv;
    local uagrad,vagrad,ubgrad,vbgrad,tt;

    fboxes;   // find facet bounding boxes
    // test all facet pairs
    foreach facet fa do
    { foreach facet fb where fb.id > fa.id do
      {  // first, check bounding boxes
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
         // Find minimum distance by Newton's Method 
         eps := .2; // edge guard
         ua := .7; va := .6; ub := .71; vb := .67;
         while ( eps > 1e-6 ) do
         {
           xa := 0.5*(ua+va-1)*(ua+va-2)*xa1 + ua*(2-ua-va)*xa2
              + 0.5*ua*(ua-1)*xa3 + va*(2-ua-va)*xa4
              + ua*va*xa5 + 0.5*va*(va-1)*xa6; 
           ya := 0.5*(ua+va-1)*(ua+va-2)*ya1 + ua*(2-ua-va)*ya2
              + 0.5*ua*(ua-1)*ya3 + va*(2-ua-va)*ya4
              + ua*va*ya5 + 0.5*va*(va-1)*ya6; 
           za := 0.5*(ua+va-1)*(ua+va-2)*za1 + ua*(2-ua-va)*za2
              + 0.5*ua*(ua-1)*za3 + va*(2-ua-va)*za4
              + ua*va*za5 + 0.5*va*(va-1)*za6; 
           xb := 0.5*(ub+vb-1)*(ub+vb-2)*xb1 + ub*(2-ub-vb)*xb2
              + 0.5*ub*(ub-1)*xb3 + vb*(2-ub-vb)*xb4
              + ub*vb*xb5 + 0.5*vb*(vb-1)*xb6; 
           yb := 0.5*(ub+vb-1)*(ub+vb-2)*yb1 + ub*(2-ub-vb)*yb2
              + 0.5*ub*(ub-1)*yb3 + vb*(2-ub-vb)*yb4
              + ub*vb*yb5 + 0.5*vb*(vb-1)*yb6; 
           zb := 0.5*(ub+vb-1)*(ub+vb-2)*zb1 + ub*(2-ub-vb)*zb2
              + 0.5*ub*(ub-1)*zb3 + vb*(2-ub-vb)*zb4
              + ub*vb*zb5 + 0.5*vb*(vb-1)*zb6; 
           dx := xa-xb; dy := ya-yb; dz := za - zb;
           dd := dx*dx + dy*dy + dz*dz;
           if ( dd < tiny ) then break;
           xau := (ua + va - 1.5)*xa1 + (2-2*ua-va)*xa2 + (ua-0.5)*xa3
                   - va*xa4 + va*xa5;
           xav := (ua + va - 1.5)*xa1 - ua*xa2 +(2-ua-2*va)*xa4 + ua*xa5
                    + (va - 0.5)*xa6;
           yau := (ua + va - 1.5)*ya1 + (2-2*ua-va)*ya2 + (ua-0.5)*ya3
                   - va*ya4 + va*ya5;
           yav := (ua + va - 1.5)*ya1 - ua*ya2 +(2-ua-2*va)*ya4 + ua*ya5
                    + (va - 0.5)*ya6;
           zau := (ua + va - 1.5)*za1 + (2-2*ua-va)*za2 + (ua-0.5)*za3
                   - va*za4 + va*za5;
           zav := (ua + va - 1.5)*za1 - ua*za2 +(2-ua-2*va)*za4 + ua*za5
                    + (va - 0.5)*za6;
           xbu := (ub + vb - 1.5)*xb1 + (2-2*ub-vb)*xb2 + (ub-0.5)*xb3
                   - vb*xb4 + vb*xb5;
           xbv := (ub + vb - 1.5)*xb1 - ub*xb2 +(2-ub-2*vb)*xb4 + ub*xb5
                    + (vb - 0.5)*xb6;
           ybu := (ub + vb - 1.5)*yb1 + (2-2*ub-vb)*yb2 + (ub-0.5)*yb3
                   - vb*yb4 + vb*yb5;
           ybv := (ub + vb - 1.5)*yb1 - ub*yb2 +(2-ub-2*vb)*yb4 + ub*yb5
                    + (vb - 0.5)*yb6;
           zbu := (ub + vb - 1.5)*zb1 + (2-2*ub-vb)*zb2 + (ub-0.5)*zb3
                   - vb*zb4 + vb*zb5;
           zbv := (ub + vb - 1.5)*zb1 - ub*zb2 +(2-ub-2*vb)*zb4 + ub*zb5
                    + (vb - 0.5)*zb6;
           uagrad := 2*dx*xau + 2*dy*yau + 2*dz*zau;
           vagrad := 2*dx*xav + 2*dy*yav + 2*dz*zav;
           ubgrad := -(2*dx*xbu + 2*dy*ybu + 2*dz*zbu);
           vbgrad := -(2*dx*xbv + 2*dy*ybv + 2*dz*zbv);
         
           // find multiple of gradient; factor of 2 due to dist^2
           tt := 2*dd/(uagrad*uagrad+vagrad*vagrad+ubgrad*ubgrad+vbgrad*vbgrad);
          
           // clamp to triangle boundaries if necessary
           // actually, clamp short of bdry so don't have to worry
           // about being on boundary.
           if ( ua - tt*uagrad < eps ) then tt := (-eps+ua)/uagrad;
           if ( va - tt*vagrad < eps ) then tt := (-eps+va)/vagrad;
           if ( ua - tt*uagrad + va - tt*vagrad > 2 - eps )
             then tt := (-2 + eps + ua + va)/(uagrad + vagrad);
           if ( ub - tt*ubgrad < eps ) then tt := (-eps+ub)/ubgrad;
           if ( vb - tt*vbgrad < eps ) then tt := (-eps+vb)/vbgrad;
           if ( ub - tt*ubgrad + vb - tt*vbgrad > 2 - eps )
             then tt := -(2 - eps - ub - vb)/(ubgrad + vbgrad);
           ua := ua - tt*uagrad;
           va := va - tt*vagrad;
           ub := ub - tt*ubgrad;
           vb := vb - tt*vbgrad;
           eps := eps/5;
         };
         if ( dd < tiny )
          then printf "Facets %d and %d intersect at (%g,%g,%g).\n",xa,ya,za;
      } 
  }
}
