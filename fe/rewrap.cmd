// rewrap.cmd

// Commands to rewrap torus vertices and edges to get them nicely
// within unit cell.  This version does 3D; for 2D see rewrap2.cmd.
// Moves vertices at most one period at a time, so you may have to
// repeat if things are very bad to start with.

// Uses torus wrap representation and wrap_vertex builtin command.

// Usage: rewrap

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

rewrap := { 
  if space_dimension != 3 then
  { errprintf"rewrap is for space dimension 3; for 2D use rewrap2.cmd.\n";
    return;
  };
  define body attribute old_volume real; // so can adjust volconst
  set body old_volume volume;
  foreach vertex vv where vv.x*inverse_periods[1][1]+vv.y*inverse_periods[1][2]
      + vv.z*inverse_periods[1][3] < 0 do wrap_vertex(vv.id,1);
  foreach vertex vv where vv.x*inverse_periods[1][1]+vv.y*inverse_periods[1][2]
      + vv.z*inverse_periods[1][3] >= 1 do wrap_vertex(vv.id,31);
  foreach vertex vv where vv.x*inverse_periods[2][1]+vv.y*inverse_periods[2][2]
      + vv.z*inverse_periods[2][3] < 0 do wrap_vertex(vv.id,64);
  foreach vertex vv where vv.x*inverse_periods[2][1]+vv.y*inverse_periods[2][2]
      + vv.z*inverse_periods[2][3] >= 1 do wrap_vertex(vv.id,1984);
  foreach vertex vv where vv.x*inverse_periods[3][1]+vv.y*inverse_periods[3][2]
      + vv.z*inverse_periods[3][3] < 0 do wrap_vertex(vv.id,4096);
  foreach vertex vv where vv.x*inverse_periods[3][1]+vv.y*inverse_periods[3][2]
      + vv.z*inverse_periods[3][3] >= 1 do wrap_vertex(vv.id,126976);
  recalc;
  // Adjust volconst
  local torvol;
  torvol :=  abs((torus_periods[1][1]*torus_periods[2][2]
               - torus_periods[1][2]*torus_periods[2][1])*torus_periods[3][3]
          + (torus_periods[1][2]*torus_periods[2][3]
               - torus_periods[1][3]*torus_periods[2][2])*torus_periods[3][1] 
          + (torus_periods[1][3]*torus_periods[2][1]
               - torus_periods[1][1]*torus_periods[2][3])*torus_periods[3][2]);
  set body volconst floor((old_volume - volume - volconst)/torvol+.5)*torvol;

}

