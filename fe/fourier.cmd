// fourier.cmd
// Evolver command to print Fourier components for closed curve

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// Usage: f_component(order)

procedure f_component (integer f_order)
// f_order is the order of Fourier components.
{
   local e_id,first_e,v_id,newe_id;

   x_sin_coeff := 0.0;
   y_sin_coeff := 0.0;
   z_sin_coeff := 0.0;
   x_cos_coeff := 0.0;
   y_cos_coeff := 0.0;
   z_cos_coeff := 0.0;
   ecount := 0; 
   // get starting edge, since can't assume edge[1] exists
   foreach edge do e_id := id;
   first_e := e_id;
   v_id := edge[e_id].vertex[1].id;
   // follow connected edges
   do { 
     local ss,cc,newv_id;
     ss := sin(f_order*ecount*2*pi/edge_count)*2/edge_count;
     x_sin_coeff := x_sin_coeff + vertex[v_id].x*ss; 
     y_sin_coeff := y_sin_coeff + vertex[v_id].y*ss; 
     z_sin_coeff := z_sin_coeff + vertex[v_id].z*ss; 
     cc := cos(f_order*ecount*2*pi/edge_count)*2/edge_count;
     x_cos_coeff := x_cos_coeff + vertex[v_id].x*cc; 
     y_cos_coeff := y_cos_coeff + vertex[v_id].y*cc; 
     z_cos_coeff := z_cos_coeff + vertex[v_id].z*cc; 
     foreach edge[e_id].vertex vv do 
     { if ( vv.id != v_id ) then
       { newv_id := vv.id;
         foreach vv.edge ee do 
            if ee.id != e_id then newe_id := ee.id
        }
     };
     e_id := newe_id; v_id := newv_id;
     ecount := ecount + 1;
   } while ( (e_id != first_e) and (ecount <= edge_count) );
   printf "Order %g           x                 y                 z \n",f_order;
   printf "sin: %20.15f %20.15f %20.15f\n",x_sin_coeff,y_sin_coeff,z_sin_coeff;
   printf "cos: %20.15f %20.15f %20.15f\n",x_cos_coeff,y_cos_coeff,z_cos_coeff;
}
