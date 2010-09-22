// unshear.cmd

// Evolver command to get torus fundamental region closer to rectangular.
// Works in either 2 or 3 dimensions. (eventually, just 2D now)

// NOTE: "unshear" needs to change the torus periods, but can't directly
// since torus_periods is a read-only variable (because it is remembered
// as a formula).  So the assumption is that the shear entries of the
// torus_periods matrix are variables of particular names, shearij for
// torus_periods[i][j].  (remember indexing starts with 1)

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

if not is_defined("shear21") then
  errprintf "unshear.cmd assumes torus_periods[2][1] defined as shear21.\n" ;

// 2D version
unshear2 := {
  local shifts,vshifts,wrap1,wrap2,wraprest;

  // test for horizontal shear
  shifts := floor(torus_periods[2][1]/torus_periods[1][1] + .5);

  shear21 -= shifts*torus_periods[1][1];
  // move vertices over
  foreach vertex vv do
  { vshifts := -floor(vv.x*inverse_periods[1][1]+vv.y*inverse_periods[1][2]);   
    if vshifts != 0 then
        wrap_vertex(vv.id,(vshifts imod 32));
  };
  // change edge wraps
  foreach edge ee where ((ee.wrap idiv 64) imod 32) == 1 do
  { wrap1 := ee.wrap imod 64;
    wraprest := ee.wrap - wrap1;
    if wrap1 > 15 then wrap1 -= 32;
    wrap1 += shifts;
    ee.wrap := wraprest + (wrap1 imod 32);
  }; 
  foreach edge ee where ((ee.wrap idiv 64) imod 32) == 31 do
  { wrap1 := ee.wrap imod 64;
    wraprest := ee.wrap - wrap1;
    if wrap1 > 15 then wrap1 -= 32;
    wrap1 += -shifts;
    ee.wrap := wraprest + (wrap1 imod 32);
  }; 

  recalc;  // get inverse periods set up right for next stage 

  // test for vertical shear
  shifts := floor(torus_periods[1][2]/torus_periods[2][2] + .5);

  shear12 -= shifts*torus_periods[2][2]; 
  // move vertices over
  foreach vertex vv do
  { vshifts := -floor(vv.x*inverse_periods[2][1]+vv.y*inverse_periods[2][2]);   
    if vshifts != 0 then
        wrap_vertex(vv.id,(vshifts imod 32)*64);
  };

  // change edge wraps
  foreach edge ee where (ee.wrap imod 32) == 1 do
  { wrap2 := (ee.wrap idiv 64) imod 64;
    wraprest := ee.wrap - wrap2*64;
    if wrap2 > 15 then wrap2 -= 32;
    wrap2 += shifts;
    ee.wrap := wraprest + (wrap2 imod 32)*64;
  }; 
  foreach edge ee where (ee.wrap imod 32) == 31 do
  { wrap2 := (ee.wrap idiv 64) imod 64;
    wraprest := ee.wrap - wrap2*64;
    if wrap2 > 15 then wrap2 -= 32;
    wrap2 -= shifts;
    ee.wrap := wraprest + (wrap2 imod 32)*64;
  }; 
  
}

// 3D version
unshear3 := {
  errprintf "Unshear not yet implemented for 3D.\n";
}

// Overall command, just a wrapper
unshear := {
   if not torus then
   { errprintf "Unshear only relevant to torus model.\n";
     return;
   };
   if space_dimension == 2 then unshear2 
   else if space_dimension == 3 then unshear3 
   else errprintf "Unshear not implemented for space dimension %d\n",
           space_dimension;
}

