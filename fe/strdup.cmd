// strdup.cmd

// Evolver command to write a datafile that is an n-fold
// covering of a string.  Meant to generate multiple coverings
// of elastic figure 8's.

// Usage: set dupnum to desired multiple and run "strdup".

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

dupnum := 2   // covering order
strdup := { 
        local sheet,vbase,connectflag,swapedge,ebase;
        list topinfo;
        printf "\nvertices\n";
        sheet := 0;
        while ( sheet < dupnum ) do
        { vbase := sheet*vertex_count;
          foreach vertex do 
            printf  "%g  %g %g\n",id+vbase,x,y;
          sheet := sheet + 1;
        };
        printf "\nedges\n";
        connectflag := 0;
        sheet := 0;
        swapedge := -1;
        while ( sheet < dupnum ) do
        { ebase := sheet*edge_count;
          vbase := sheet*vertex_count;
          foreach edge ee do 
          { if connectflag and (swapedge != ee.id) then
               printf  "%g  %g %g\n",
                   ee.id+ebase,ee.vertex[1].id+vbase,ee.vertex[2].id+vbase
            else
            { printf  "%g  %g %g\n",
                  ee.id+ebase,ee.vertex[1].id+vbase,
                     ee.vertex[2].id+((sheet+1)imod dupnum)*vertex_count;
              connectflag := 1;
              swapedge := ee.id;
            }
          };
          sheet := sheet + 1;
        };
       printf "\nread\n";
       list procedures;
    }  // end strdup
