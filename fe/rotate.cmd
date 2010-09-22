// rotate.cmd
// Surface Evolver procedure to rotate unfixed vertices by angle about z axis.
// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// Usage: rotate(angle)
//  where angle is in radians.

procedure rotate(real roth) := { 
  local sss,ccc,rrad,olds;
  sss := sin(roth); ccc := cos(roth);
  foreach vertex vv where not fixed do
  { rrad := vv.x^2+vv.y^2;
    oldx := vv.x;
    set vv x vv.x*ccc-vv.y*sss;
    set vv y oldx*sss+vv.y*ccc;
  }
}  
