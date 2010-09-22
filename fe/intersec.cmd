// intersec.cmd
// Evolver command to detect intersection of linear edges and facets.
// For each facet, finds if any edge intersects in interior.
// Usage: read "intersec.cmd"
//    detect
// Output: prints ids of facets and edges that intersect.
// Notes: In quadratic model, treats edges and facets as simply linear.
//        Will not work in Lagrange model.

eps := 1e-10  // tolerance for being equal to zero
detect := { 
    local ax,ay,az,bx,by,bz,cx,cy,cz,acx,bcx,px,py,pz,qx,qy,qz,qpx,qcx;
    local denom,lambda,mu,sigma;
    local acy,acz,bcy,bcz,qpy,qpz,qcy,qcz;

    if lagrange then
    { printf "intersect.cmd will not work in Lagrange model.\n";return;};
    foreach facet ff do
    { ax := ff.vertex[1].x;
      ay := ff.vertex[1].y;
      az := ff.vertex[1].z;
      bx := ff.vertex[2].x;
      by := ff.vertex[2].y;
      bz := ff.vertex[2].z;
      cx := ff.vertex[3].x;
      cy := ff.vertex[3].y;
      cz := ff.vertex[3].z;
      acx := ax-cx; acy := ay-cy; acz := az- cz;
      bcx := bx-cx; bcy := by-cy; bcz := bz- cz;
      foreach edge ee do
      { px := ee.vertex[1].x;
        py := ee.vertex[1].y;
        pz := ee.vertex[1].z;
        qx := ee.vertex[2].x;
        qy := ee.vertex[2].y;
        qz := ee.vertex[2].z;
        qpx := qx-px; qpy := qy-py; qpz := qz-pz;
        qcx := qx-cx; qcy := qy-cy; qcz := qz-cz;
        denom := qpx*(acy*bcz-acz*bcy) - qpy*(acx*bcz-acz*bcx)
          + qpz*(acx*bcy-acy*bcx);
        if ( abs(denom) < eps ) then continue;
        lambda := (qpx*(qcy*bcz-qcz*bcy) - qpy*(qcx*bcz-qcz*bcx)
          + qpz*(qcx*bcy-qcy*bcx))/denom;
        if ( lambda <= eps ) then continue;
        mu := (qpx*(acy*qcz-acz*qcy) - qpy*(acx*qcz-acz*qcx)
          + qpz*(acx*qcy-acy*qcx))/denom;
                if ( mu <= eps ) then continue;
        if ( lambda + mu > 1-eps ) then continue;
        sigma := (qcx*(acy*bcz-acz*bcy) - qcy*(acx*bcz-acz*bcx)
          + qcz*(acx*bcy-acy*bcx))/denom;
        if ( (sigma <= eps) or (sigma > 1-eps) ) then continue;

        // now have intersection 
        printf "Facet %g and edge %g intersect.\n",ff.id,ee.id;
      }

    }
  }
