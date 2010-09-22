// quadbbox.cmd
// Finds bounding box for each facet in quadratic model.
// Not suitable for torus model.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// Usage: Run eboxes or fboxes.  Results left in the arrays ebox or fbox
// attributes.

define edge attribute ebox real [6]   // xmin,xmax,ymin,ymax,zmin,zmax     
define facet attribute fbox real [6]

eboxes := { 
            local ii,v0,v1,v2,v4,uopt,denom;
            if ( !quadratic ) then print "eboxes: Must be in quadratic mode.\n"
            else foreach edge ee do 
             { ii := 1;
               while ( ii <= 3 ) do
               {
                if ( ii == 1 ) then 
                { v0 := ee.vertex[1].x;
                  v2 := ee.vertex[2].x;
                  v1 := ee.vertex[3].x;
                } else if ( ii == 2 ) then
                { v0 := ee.vertex[1].y;
                  v2 := ee.vertex[2].y;
                  v1 := ee.vertex[3].y;
                } else
                { v0 := ee.vertex[1].z;
                  v2 := ee.vertex[2].z;
                  v1 := ee.vertex[3].z;
                } ;
                 if ( v0 > v1 ) 
                   then { set ee ebox[2*ii-1]  v1; set ee ebox[2*ii] v0; }
                   else { set ee ebox[2*ii-1] v0; set ee ebox[2*ii] v1; };
                 if ( v2 < ee.ebox[2*ii-1] ) then set ee ebox[2*ii-1] v2; 
                 if ( v2 > ee.ebox[2*ii] ) then set ee ebox[2*ii] v2; 
                 denom := v0 - 2*v1 + v2;
                 if ( denom == 0.0 ) then { ii := ii+1; continue; };
                 uopt := (-1.5*v0 + 2*v1 - .5*v2)/denom;
                 if ( uopt <= 0.0 or uopt >= 1.0 )
                    then { ii := ii+1;continue; };
                 v4 := 0.5*(1-uopt)*(2-uopt)*v0 + uopt*(2-uopt)*v1
                          + 0.5*uopt*(uopt-1)*v2;
                 if ( v4 < ee.ebox[2*ii-1] ) then set ee ebox[2*ii-1] v4; 
                 if ( v4 > ee.ebox[2*ii] ) then set ee ebox[2*ii] v4; 
                 ii := ii + 1;
               }
          }
      }

fboxes := {
            local ii,v0,v1,v2,v3,v4,v5,uopt,denom;
            local a11,a12,b1,a21,a22,b2,vopt,vcrit;
            eboxes;
            if ( ! quadratic ) then print "fboxes: Must be in quadratic mode.\n"
            else foreach facet ff do
            { 
              ii := 0;  /* which coordinate */ 
              while ( ii < 3 ) do
              { ii := ii + 1;   // so can use continue
                /* first, edge boxes */
                set ff fbox[2*ii-1]  ff.edge[1].ebox[2*ii-1];
                set ff fbox[2*ii]  ff.edge[1].ebox[2*ii];
                if ( ff.edge[2].ebox[2*ii-1] < ff.fbox[2*ii-1] )
                     then set ff fbox[2*ii-1] ff.edge[2].ebox[2*ii-1];
                if ( ff.edge[2].ebox[2*ii] > ff.fbox[2*ii] )
                     then set ff fbox[2*ii] ff.edge[2].ebox[2*ii];
                if ( ff.edge[3].ebox[2*ii-1] < ff.fbox[2*ii-1] )
                     then set ff fbox[2*ii-1] ff.edge[3].ebox[2*ii-1];
                if ( ff.edge[3].ebox[2*ii] > ff.fbox[2*ii] )
                     then set ff fbox[2*ii] ff.edge[3].ebox[2*ii];
                if ( ii == 1 ) then
                { v0 := ff.edge[1].vertex[1].x;
                  v1 := ff.edge[1].vertex[3].x;
                  v2 := ff.edge[1].vertex[2].x;
                  v3 := ff.edge[3].vertex[3].x;
                  v4 := ff.edge[2].vertex[3].x;
                  v5 := ff.edge[2].vertex[2].x;
                } else if ( ii == 2 ) then
                { v0 := ff.edge[1].vertex[1].y;
                  v1 := ff.edge[1].vertex[3].y;
                  v2 := ff.edge[1].vertex[2].y;
                  v3 := ff.edge[3].vertex[3].y;
                  v4 := ff.edge[2].vertex[3].y;
                  v5 := ff.edge[2].vertex[2].y;
                } else 
                { v0 := ff.edge[1].vertex[1].z;
                  v1 := ff.edge[1].vertex[3].z;
                  v2 := ff.edge[1].vertex[2].z;
                  v3 := ff.edge[3].vertex[3].z;
                  v4 := ff.edge[2].vertex[3].z;
                  v5 := ff.edge[2].vertex[2].z;
                };
                // x_u coeff of u
                a11 := v0 - 2*v1 + v2;
                // x_u coeff of v
                a12 := v0 - v1 - v3 + v4;
                // x_u rhs
                b1 := -(-1.5*v0 + 2*v1 - .5*v2);
                // x_v coeff of u
                a21 := v0 - v1 - v3 + v4;
                // x_v coeff of v
                a22 := v0 - 2*v3 + v5;
                // x_v rhs
                b2 := -(-1.5*v0 + 2*v3 - 0.5*v5);
                // solve for critical point
                denom := a11*a22 - a12*a21;
                
                if ( denom == 0.0 ) then continue;
                uopt := (b1*a22 - b2*a12)/denom;
                vopt := (a11*b2 - a21*b1)/denom;
                if ( uopt <= 0.0 ) then continue;
                if ( vopt <= 0.0 ) then continue;
                if ( uopt+vopt >= 2.0 ) then continue;
                vcrit := 0.5*(uopt+vopt-1)*(uopt+vopt-2)*v0
                      + uopt*(2-uopt-vopt)*v1
                      + 0.5*uopt*(uopt-1)*v2
                      + vopt*(2-uopt-vopt)*v3
                      + uopt*vopt*v4
                      + 0.5*vopt*(vopt-1)*v5;
                if ( vcrit < ff.fbox[2*ii-1] ) then set ff fbox[2*ii-1] vcrit; 
                if ( vcrit > ff.fbox[2*ii] ) then set ff fbox[2*ii] vcrit; 
               } 
            } 
        } 
