// slice2.cmd --- produce intersection of surface with plane
// plane eq: aa*x + bb*y + cc*z = dd
// output: length of slice, and area inside slice.
// Does not modify surface.
// Note all area inside slice is counted as positive!
// Do not to slice exactly through vertices!!  Avoid special
//  values for dd like 0 or .5!

// Fixed to do oriented facets of a particular body. 

// Usage: set plane coefficients, then do slice2(body number).
// Results: Intersection length printed out, left in variable lensum.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

procedure slice2(integer body_num) := { 
       local any,xx1,yy1,zz1,xx2,yy2,zz2;
       local denom,lambda,zaa,zbb,darea;
       lensum := 0; areasum := 0;

       xb := 0; yb := 0; zb := 0;  // just for declaration before use.
       foreach body[body_num].facet ff do 
       { any := 0; 
         foreach ff.edge ee do 
         {
           xx1 := ee.vertex[1].x; 
           yy1 := ee.vertex[1].y; 
           zz1 := ee.vertex[1].z; 
           xx2 := ee.vertex[2].x; 
           yy2 := ee.vertex[2].y; 
           zz2 := ee.vertex[2].z;
           denom := aa*(xx1-xx2)+bb*(yy1-yy2)+cc*(zz1-zz2);
           if ( denom != 0.0 ) then 
           { 
             lambda := (dd-aa*xx2-bb*yy2-cc*zz2)/denom; 
             if ( (lambda >= 0) and (lambda <= 1) ) then 
             { 
               xa := xb; ya := yb; za := zb;
               xb := lambda*xx1+(1-lambda)*xx2; 
               yb := lambda*yy1+(1-lambda)*yy2;
               zb := lambda*zz1+(1-lambda)*zz2; 
               any := any+1; 
             } 
           }
         } ; 
         if any == 2 then
         { 
           local triple;
           dx := xa-xb; dy := ya-yb; dz := za - zb;
           lensum := lensum + sqrt(dx^2+dy^2+dz^2);
           zaa := za - dd/cc; zbb := zb - dd/cc;
           fx := ff.x; fy := ff.y; fz := ff.z;
           triple := fx*(dy*cc-dz*bb)+fy*(dz*aa-dx*cc)+fz*(dx*bb-dy*aa);

           darea := (xa*yb-xb*ya)/2;
           if ( triple > 0 ) then areasum := areasum + darea
           else areasum := areasum - darea;
         }
       };
       areasum := areasum*sqrt(aa*aa+bb*bb+cc*cc)/cc;
       printf "Circumference: %18.15g  Area: %18.15g\n",lensum,areasum;
     } // end slice

