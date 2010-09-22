// wetfoam2.cmd
// Evolver command to convert dry foam to wet foam.
// Dry foam assumptions:
//    In torus domain  (so no awkward boundary conditions)
//    Only one high valence edge per facet (will refine if finds more )
//    Facet tension assumed to be uniformly 1.

// Usage: Set "spread" to relative size of border (default 0.2), then
//        run wetfoam with output redirected to desired file, e.g.
//        Enter command: wetfoam >>> "wetfile.fe"

// The resulting file has Plateau border body, whose number is recorded
// in the variable border_body.

// This file replaces wetfoam.cmd, which is now obsolete.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

/* Parameter for initial size of Plateau borders */
/* Note this is relative to the size of facets, not absolute. */
spread := .2

// Other global variables used internally here
ccount := 0
voffset := 0
wrapnum := 0; 
border_body := 0;

init_attributes := {
 // some useful attributes, all as dimensioned so can be shrunk to zero size
 // when done with them.
 define vertex attribute hivalence integer[1]; // number of high-valence edges
 define facet attribute corners integer[3];  // which "corner" each vertex
                                           //  belongs to
 define facet attribute newedge integer[1];   // pulled-out edge
 define facet attribute corneredge1 integer[3];// new edge forward from corner
 define facet attribute corneredge2 integer[3];// new edge backward from corner
 define facet attribute didflag1 integer[3];// whether put vertex face here
 define facet attribute didflag2 integer[3];// whether put vertex face here
 define facet attribute diagedge integer[1];   // diagonal across rectangle
}

shrink_attributes := {
  // make storage small so to not pollute memory of original or new file.
  define vertex attribute hivalence integer[0];
  define facet attribute corners integer[0];  
  define facet attribute newedge integer[0]; 
  define facet attribute corneredge1 integer[0];
  define facet attribute corneredge2 integer[0];
  define facet attribute didflag1 integer[0];
  define facet attribute didflag2 integer[0];
  define facet attribute diagedge integer[0];

  // and a few arrays while we are at it
  define cornerx real[0];
  define cornery real[0];
  define cornerz real[0];
  define cornern integer[0]; 
  define borderbody integer[0];
}

/* tests and preliminaries */
vertest := { foreach vertex vv do 
                 vv.hivalence[1] := sum(vv.edge where valence >= 3,1);
           }

facettest := { local triple;
               foreach facet ff do
               { triple := sum(ff.edge where valence >= 3, 1);
                 if ( triple >= 2 ) then
                 {  refine ff;  
                 }
               }
             }

wettests := {  vertest; facettest; }

/* enumerate and consolidate facet corners */
/* corner number of 0 means not adjacent to hi-valence vertex */
do_corners := {
    local inx;

    /* enumerate */
    ccount := 1;
    foreach facet ff do
    { 
      inx := 1;
      foreach ff.vertex vv do
      { if vv.hivalence[1] > 0 then
        { ff.corners[inx] := ccount;
          ccount += 1;
        };
        inx += 1;
      };
    };

    /* consolidate */
    local changes;
    do
    { changes := 0;
      foreach edge ee do
      { if ee.valence != 2 then continue;
        local ffid; local fffid; local knx;
        ffid := ee.facet[1].id;
        fffid := ee.facet[2].id;
        inx := 1;
        foreach facet[ffid].vertex vv do
        { if vv.id != ee.vertex[1].id && vv.id != ee.vertex[2].id then 
          { inx += 1; continue; };
          knx := 1;
          foreach facet[fffid].vertex vvv do
          { if vv.id == vvv.id then
            { if facet[ffid].corners[inx] == facet[fffid].corners[knx]  then
                 break;
              if facet[ffid].corners[inx] < facet[fffid].corners[knx]  then
                 facet[fffid].corners[knx] := facet[ffid].corners[inx]
              else facet[ffid].corners[inx] := facet[fffid].corners[knx];
              changes += 1;
              break;
            };
            knx += 1;
          };
          inx += 1;
        }
      }
    } while changes;

    /* array to hold info on corner vertices */
    define cornerx real[ccount];
    define cornery real[ccount];
    define cornerz real[ccount];
    define cornern integer[ccount];  // number of facets in corner
    inx := 1; while inx <= ccount do 
    { cornern[inx] := 0; cornerx[inx] := 0; cornery[inx] := 0;
      cornerz[inx] := 0; inx += 1; 
    };
    foreach facet ff do
    { inx := 1;
      local ffid;
      ffid := ff.id;  /* want positive orientation */
      while inx <= 3 do
      { if ff.corners[inx] != 0 then
        { cornern[ff.corners[inx]] += 1;
          cornerx[ff.corners[inx]] += 
               spread*facet[ffid].edge[inx].x + 2*facet[ffid].vertex[inx].x
               - spread*facet[ffid].edge[inx==1 ? 3 : inx-1].x; 
          cornery[ff.corners[inx]] += 
               spread*facet[ffid].edge[inx].y + 2*facet[ffid].vertex[inx].y
               - spread*facet[ffid].edge[inx==1 ? 3 : inx-1].y; 
          cornerz[ff.corners[inx]] +=  
               spread*facet[ffid].edge[inx].z + 2*facet[ffid].vertex[inx].z
               - spread*facet[ffid].edge[inx==1 ? 3 : inx-1].z; 
        };
        inx += 1;
      };
    };
    local kk;
    kk := 1;
    while kk <= ccount do
    { if cornern[kk] > 0 then
      { cornerx[kk] /= 2*cornern[kk];
        cornery[kk] /= 2*cornern[kk];
        cornerz[kk] /= 2*cornern[kk];
      };
      kk += 1;
    };
} /* end do_corners */

/* print vertex list */
wetverts := { 
              voffset := max(vertex,id)+1; /* start of new vertices */
              printf "\nvertices\n";
              foreach vertex vv do
                if ( vv.hivalence[1] < 1 ) then 
                     printf "%d  %g %g %g\n",vv.id, vv.x,vv.y,vv.z;
              local kk;
              kk := 1;
              while kk <= ccount do
              { if cornern[kk] > 0 then
                { printf "%d   %g %g %g\n",kk+voffset,
                    cornerx[kk],cornery[kk],cornerz[kk];
                };
                kk += 1;
              }
            }  /* end wetverts */

/* convert numerical torus wrap to string and print; called by wetedges */
/* incoming argument: wrapnum */
wrapconvert := { local ww;
                 ww := floor(1e-6 + (wrapnum % 64)); 
                 if ( ww == 0 ) then printf " *"
                 else if ( ww == 1 ) then printf " +"
                 else printf " -";
                 ww := floor(1e-6 + (floor(1e-6+wrapnum/64) % 64) );
                 if ( ww == 0 ) then printf " *"
                 else if ( ww == 1 ) then printf " +"
                 else printf " -";
                 ww := floor(1e-6 + (floor(1e-6+wrapnum/4096) % 64) );
                 if ( ww == 0 ) then printf " *"
                 else if ( ww == 1 ) then printf " +"
                 else printf " -";
               }

/* print edge list */
wetedges := { local ecounter;
              local eeid; local first_corner; local corner;
              local prevff; local previnx; local firstff;
              local firstinx; local prevcorner; 
              local ffid; local inx;
              local ffid2; local eeid2;

              ecounter := max(edge,id)+1; /* start of new edges */
              printf "\nedges\n";
              /* first, old edges that we keep */
              foreach edge ee where valence == 2 do
              { printf "%d  ", ee.id;
                wrapnum := ee.wrap;
                foreach ee.vertex vv do
                { if ( vv.hivalence[1] > 0 ) then 
                  { /* find proper pull-out vertex */
                    local ffid;
                    ffid := ee.facet[1].id;
                    inx := 1;
                    while inx <= 3 do
                    { if facet[ffid].vertex[inx].id == vv.id then
                      { printf "%d ",facet[ffid].corners[inx]+voffset;
                        break;
                      };
                      inx += 1;
                    }
                  }
                  else printf "%d ",vv.id;
                }; 
                if ( torus ) then wrapconvert;
                printf "\n";
              };
              foreach edge ee where valence >= 3 do
              { local tail1; local tail2; local tail3;
                local head1; local head2; local head3;
                local vv1; local inx;
                local headv; local tailv; local ffid;
                local vv2;

                tail1 := 0; tail2 := 0; tail3 := 0;
                head1 := 0; head2 := 0; head3 := 0;
                wrapnum := ee.wrap;
                /* assemble data about new vertices */
                vv1 := ee.vertex[1].id; vv2 := ee.vertex[2].id;
                foreach ee.facet ff do
                { /* new edge between facet corners */
                  ffid := ff.id;  /* want positive orientation of facet */
                  inx := 1;
                  foreach facet[ffid].vertex vv do
                  { if vv.id == vv1 then tailv := ff.corners[inx]+voffset;
                    if vv.id == vv2 then headv := ff.corners[inx]+voffset;
                    inx += 1;
                  };
                  ff.newedge[1] := ecounter;
                  ff.diagedge[1] := ecounter+3;
                  printf "%d  %d %d ",ecounter,tailv,headv; 
                  if torus then wrapconvert;
                  printf "/* from edge %d */ ",ee.id;
                  printf "\n";
                  tail1 := tail2; tail2 := tail3; tail3 := tailv;
                  head1 := head2; head2 := head3; head3 := headv;
                  ecounter += 1;
                };
                printf "%d  %d %d ",ecounter,tail1,head2; 
                if torus then wrapconvert;
                printf "\n";
                ecounter += 1;
                printf "%d  %d %d ",ecounter,tail2,head3; 
                if torus then wrapconvert;
                printf "\n";
                ecounter += 1;
                printf "%d  %d %d ",ecounter,tail3,head1; 
                if torus then wrapconvert;
                printf "\n";
                ecounter += 1;
              };
 
              /* interior points of hi-valence edges */
              foreach vertex vv where vv.hivalence[1] == 2 do
              { /* find the high-valence edges first */
                local ffid;

                eeid := 0;
                foreach vv.edge ee do
                { if ee.valence > 2 then 
                  { eeid2 := eeid; eeid := ee.id; }
                };
                first_corner := 0; corner := 0; prevff:=0; previnx:=0;
                firstff := 0; firstinx := 0;
                foreach edge[eeid].facet ff do
                { /* find proper corner */
                  prevcorner := corner; 
                  ffid := ff.id;
                  inx := 1; 
                  while inx <= 3 do
                  { if facet[ffid].vertex[inx].id == vv.id then
                    { corner := facet[ffid].corners[inx];
                      break;
                    };
                    inx += 1;
                  };
                  if ( first_corner == 0 ) then
                  { first_corner := corner; prevff := ffid;
                    firstff := ffid; firstinx := inx; 
                    previnx := inx;
                    continue; 
                  };
                  /* edge from previous corner */
                  if torus then
                    printf "%d  %d %d * * *\n",ecounter,prevcorner+voffset,
                                          corner+voffset
                  else
                    printf "%d  %d %d\n",ecounter,prevcorner+voffset,
                                          corner+voffset;
                  facet[prevff].corneredge1[previnx] := ecounter;
                  facet[ffid].corneredge2[inx] := ecounter;
                  ecounter += 1;
                  prevff := ffid; previnx := inx;
                };
                if torus then
                  printf "%d  %d %d * * *\n",ecounter,corner+voffset,
                                          first_corner+voffset
                else
                  printf "%d  %d %d \n",ecounter,corner+voffset,
                                          first_corner+voffset;
                facet[prevff].corneredge1[previnx] := ecounter;
                facet[firstff].corneredge2[firstinx] := ecounter;
                ecounter += 1;
   
                /* set corneredges on facets around the other edge */
                local signum; local finx; local finx2; local inx2;
                if  (edge[eeid].vertex[1].id == edge[eeid2].vertex[2].id) or
                    (edge[eeid].vertex[2].id == edge[eeid2].vertex[1].id) then
                  signum := 1  // both in same direction
                else signum := -1; 
                finx2 := 1;
                foreach edge[eeid2].facet fff do
                { ffid2 := fff.id;
                  inx2 := 1;
                  while inx2 <= 3 do
                  { if facet[ffid2].vertex[inx2].id == vv.id then
                    { finx := 1;
                      foreach edge[eeid].facet ff do
                      { ffid := ff.id;
                        inx := 1;
                        while inx <= 3 do
                        { if facet[ffid].corners[inx] != 
                                    facet[ffid2].corners[inx2] then 
                           { inx += 1; continue; };
                           if signum == 1 then 
                           { facet[ffid2].corneredge1[inx2] :=
                               facet[ffid].corneredge1[inx];
                             facet[ffid2].corneredge2[inx2] :=
                               facet[ffid].corneredge2[inx];
                           }
                           else // opposite directions
                           { facet[ffid2].corneredge1[inx2] :=
                               -facet[ffid].corneredge2[inx];
                             facet[ffid2].corneredge2[inx2] :=
                               -facet[ffid].corneredge1[inx];
                           };
                           break 3;
                        };
                        inx += 1;
                      };
                      finx += 1;
                    };
                    inx2 += 1;
                  };
                  finx2 += 1;
                         
                }; 
              };  /* end hivalence edge interior vertices */

              /* ends of hi-valence edges */
              foreach edge ee where valence >= 3 do
              { foreach ee.vertex vv where vv.hivalence[1] >= 3 do
                {
                  first_corner := 0; corner := 0; prevff:=0; previnx:=0;
                  firstff := 0; firstinx := 0;
                  foreach ee.facet ff do
                  { /* find proper corner */
                    prevcorner := corner;
                    ffid := ff.id;
                    inx := 1; 
                    while inx <= 3 do
                    { if facet[ffid].vertex[inx].id == vv.id then
                      { corner := facet[ffid].corners[inx];
                        break;
                      };
                      inx += 1;
                    };
                    if ( first_corner == 0 ) then
                    { first_corner := corner; prevff := ffid;
                      firstff := ffid; firstinx := inx; 
                      previnx := inx;
                      continue;
                    };
                    /* edge from previous corner */
                    if torus then
                      printf "%d  %d %d * * * oldedge %d\n",ecounter,
                         prevcorner+voffset, corner+voffset, ee.id
                    else
                      printf "%d  %d %d oldedge %d\n",ecounter,
                         prevcorner+voffset, corner+voffset, ee.id;
                    facet[prevff].corneredge1[previnx] := ecounter;
                    facet[ffid].corneredge2[inx] := ecounter;
                    ecounter += 1;
                    prevff := ffid; previnx := inx;
                  };
                  if torus then
                    printf "%d  %d %d * * * oldedge %d\n",ecounter,
                      corner+voffset, first_corner+voffset, ee.id
                  else
                    printf "%d  %d %d oldedge %d\n",ecounter,
                      corner+voffset, first_corner+voffset, ee.id;
                  facet[prevff].corneredge1[previnx] := ecounter;
                  facet[firstff].corneredge2[firstinx] := ecounter;
                  ecounter += 1;
                }
              };

          } /* end wetedges */

/* auxiliary routine to do one pulled-out face around a hi-valence vertex.
   Input is ffid, signed id of starting facet,
            vvid, id of vertex in question.
*/
ffid := 0; // just to declare
vvid := 0; // just to declare
fcounter := 0; // just to declare
define borderbody integer[1] // just to declare

do_one_vertex_face :=
           {  local startffid; local did_fnum; local inx;
              local eeid; local cinx; local cedge; local finx;
              local evalence;

                  startffid := ffid;
                  did_fnum := 0;
                  do
                  {
                    /* find edge with head at vertex */
                    inx := 1;
                    foreach facet[ffid].edge ee do
                    { if ee.vertex[2].id == vvid then
                      { eeid := ee.oid; break; };
                      inx += 1;
                    };
                    if edge[eeid].valence >= 3 then
                    {
                      /* calculate proper corneredge */
                      if ffid > 0 then
                         cinx := inx == 3 ? 1 : inx+1
                      else if inx == 1 then cinx := 1
                      else cinx := 5-inx;
                      if eeid > 0 then
                      { if facet[ffid].didflag1[cinx] then return ;
                        cedge := facet[ffid].corneredge1[cinx];
                        facet[ffid].didflag1[cinx] := 1;
                      }
                      else 
                      { if facet[ffid].didflag2[cinx] then return;
                        cedge := -facet[ffid].corneredge2[cinx];
                        facet[ffid].didflag2[cinx] := 1;
                      };
                      if did_fnum ==  0 then
                      {  printf "%d   ",fcounter;
                         borderbody[fcounter] := facet[ffid].backbody;
                         fcounter += 1; 
                      };
                      did_fnum := 1;
                      printf "%d ",cedge;
                    };

                    /* find next facet around edge */
                     finx := 1; evalence := edge[eeid].valence;
                     foreach edge[eeid].facet fff do
                     { if fff.oid == ffid then break;
                       finx += 1;
                     };
                     ffid := -edge[eeid].facet[finx==evalence ? 1 : finx+1].oid;

                     /* add edge between corners, if necessary */

                   } while ffid != startffid;
                   printf " tension 0.5 ftype vertexfacet // by old vertex %d\n",vvid;

             }  /* end do_one_vertex_face */

/* print face list, less triangles at nodes */
wetfaces := { 

              fstart := max(facet,id)+1;
              fcounter := fstart;
              define borderbody integer[fcounter + 40*sum(vertex,hivalence[1]>2)
                 + 2*sum(edge where valence >= 3, valence)];
              printf "\nfaces\n";
              /* old facets, replacing triple edges with new */
              foreach facet ff do
              { printf "%d   ",ff.id;
                foreach ff.edge ee do
                { if ( ee.valence == 2 ) then printf "%d ",ee.oid
                  else 
                    printf "%d ",(ee.oid<0?-(ff.newedge[1]):ff.newedge[1])
                };
                printf " oldbodies %d %d ",ff.frontbody,ff.backbody;
                printf " ftype oldfacet\n";
              };

              /* tube faces around triple lines */
              foreach edge ee where valence >= 3 do
              { local evalence; local finx;
                evalence := ee.valence; 
                finx := 1;
                while  finx <= evalence do
                { local ea; local diag; local inx; local eb; local ed;
                  local ec;
                  ea := ee.facet[finx].newedge[1];
                  ec := ee.facet[finx==evalence?1:finx+1].newedge[1];
                  diag := ee.facet[finx].diagedge[1];
                  inx := 1;
                  ffid := ee.facet[finx].id;
                  while inx <= 3 do
                  { if facet[ffid].vertex[inx].id == ee.vertex[1].id then
                      eb := facet[ffid].corneredge1[inx];
                    if facet[ffid].vertex[inx].id == ee.vertex[2].id then
                      ed := facet[ffid].corneredge1[inx];
                    inx += 1;
                  };
                  printf "%d   %d %d %d tension 0.5 ftype tubefacet\n",
                                             fcounter,-ea,diag,-ed;
                  borderbody[fcounter] := ee.facet[finx].backbody;
                  fcounter += 1;
                  printf "%d   %d %d %d tension 0.5 ftype tubefacet\n",
                                             fcounter,eb,ec,-diag;
                  borderbody[fcounter] := ee.facet[finx].backbody;
                  fcounter += 1;
                  finx += 1;
                }
              };
    
              /* vertex faces */
              printf "//vertex faces:\n";

              foreach facet ff do
              { ff.didflag1[1] := 0;
                ff.didflag1[2] := 0;
                ff.didflag1[3] := 0;
                ff.didflag2[1] := 0;
                ff.didflag2[2] := 0;
                ff.didflag2[3] := 0;
              };
              foreach vertex vv where hivalence[1] >= 3 do
              { /* try starting at each facet */
                vvid := vv.id; // for parameter passing
                foreach vv.facet ff do
                { 
                  ffid := ff.oid;
                  do_one_vertex_face;
                  ffid := -ff.oid;
                  do_one_vertex_face;
                 }  /* end foreach facet facet */
              } /* end foreach vertex */

            } /* end wetfaces */


/* print body list */
/* new facets will be added to bodies at load time */
wetbodies := { printf "\nbodies \n";
               local ss,nn,mm,border_volume;
               ss := spread*avg(edge where valence==2,length);
               border_volume := sum(edge where valence==3,length)*
                  ss*ss*0.8;
               foreach body bod do 
               { printf "%d  ",bod.id,bod.target;
                 nn := 0;
                 foreach bod.facet ff do
                 { printf "%d ",ff.oid;
                   nn += 1;
                   if ( nn == 10 ) then 
                   { printf "\\\n    "; nn := 0;}
                 };
                 for ( mm := 1 ; mm < fcounter ; mm += 1 )
                 { if borderbody[mm] == bod.id then
                   { printf "%d ",-mm;
                     nn += 1;
                     if ( nn == 10 ) then 
                     { printf "\\\n    "; nn := 0;}
                   };
                 };
                 printf "  volume %g \n",bod.target;
               }; 
                
               /* border body can be given its facets */ 
               border_body := max(body,id)+1;
               printf "%d   ",border_body;
               local fnum;
               for ( fnum := fstart ; fnum < fcounter ; fnum := fnum )
               { for ( nn := 1 ; (nn <= 10) and (fnum < fcounter) ; nn += 1 )
                 { printf "%d ",fnum; fnum += 1; };
                 printf "\\\n";
               };
               printf " volume %g",border_volume;
               printf "\n";
     } /* end wetbodies */

/* commands to finish creating facets and bodies at load time */
read_section := {
  printf "\nread\n\n";

  printf "recalc\n";
  printf "set body target volume // so volumes don't exceed torus volume\n";  
  printf "border_body := %d\n",border_body;
}

wetfoam := { 
             // applicability tests
             if  space_dimension != 3  then
             { errprintf "wetfoam only works in space dimension 3.\n"; return; };
             if  surface_dimension != 2  then
             { errprintf "wetfoam only works for surface dimension 2.\n"; 
               return; 
             };
//             if  !torus  then
//             { errprintf "wetfoam only works for torus domain.\n"; return; };
             if lagrange_order != 1  then
             { errprintf "wetfoam only works for linear model.\n"; return; };


             // minimize arrays to avoid datafile pollution
             define cornerx real[1];
             define cornery real[1];
             define cornerz real[1];
             define cornern integer[1];  // number of facets in corner

             shrink_attributes;
             list topinfo;
             init_attributes; 
             wettests; 
             printf "\ndefine edge attribute oldedge integer\n\n";
             printf "\ndefine facet attribute oldbodies integer[2]\n\n";
             printf "\ndefine facet attribute ftype integer\n";
             printf "parameter oldfacet = 1 // original facet\n";
             printf "parameter tubefacet = 2 // new facet along tube\n";
             printf "parameter vertexfacet = 3 // new facet around vertex\n\n";
             do_corners; 
             wetverts; 
             wetedges; 
             wetfaces; 
             wetbodies; 
             read_section;
             shrink_attributes;
           }

// To user: run wetfoam with output redirected to desired file, i.e.
// Enter command: wetfoam >>> "wetfile.fe"



