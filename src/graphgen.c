/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/


/**********************************************************************
*
*  File: graphgen.c
*
*  Purpose: Generates triangles with normals and colors for    
*              feeding to device-specific graphing routines.
*
*/

#include "include.h"
#include "ytab.h"

/* prototypes to keep some compilers happy */
int INDEX_TO_RGBA ARGS((int));
int get_edge_color_2 ARGS((edge_id));
int get_facet_color_2 ARGS((facet_id));
int get_facet_backcolor_2 ARGS((facet_id));
int get_facet_frontcolor_2 ARGS((facet_id));

/*************************************************************************
*  Some preliminary stuff for handling RGBA colors.
*/
int INDEX_TO_RGBA(c)
int c;
{ return 
    ((int)(rgb_colors[c][0]*255)<<24) +
    ((int)(rgb_colors[c][1]*255)<<16) +
    ((int)(rgb_colors[c][2]*255)<<8)  +
    ((int)(rgb_colors[c][3]*255));
}

int get_edge_color_2(e_id)
edge_id e_id;
{ if ( edge_rgb_color_attr > 0 )
  { REAL *c = (REAL*)get_extra(e_id,edge_rgb_color_attr);
    return 
    ((int)(c[0]*255)<<24) +
    ((int)(c[1]*255)<<16) +
    ((int)(c[2]*255)<<8)  +
    (edge_alpha_flag ? ((int)(c[3]*255)) : 255);
  } 
  else return get_edge_color(e_id);
}

int get_facet_frontcolor_2(f_id)
facet_id f_id;
{ if ( facet_rgb_color_attr > 0 )
  { REAL *c = (REAL*)get_extra(f_id,facet_rgb_color_attr);
    return 
    ((int)(c[0]*255)<<24) +
    ((int)(c[1]*255)<<16) +
    ((int)(c[2]*255)<<8)  +
    (facet_alpha_flag ? ((int)(c[3]*255)) : 255);
  } 
  else return get_facet_frontcolor(f_id);
}

int get_facet_color_2(f_id)
facet_id f_id;
{ return get_facet_frontcolor_2(f_id);
}

int get_facet_backcolor_2(f_id)
facet_id f_id;
{
  if ( facet_rgb_color_attr > 0 )
  {
    if ( facet_rgb_backcolor_attr > 0 )
    { REAL *c = (REAL*)get_extra(f_id,facet_rgb_backcolor_attr);
      return 
      ((int)(c[0]*255)<<24) +
      ((int)(c[1]*255)<<16) +
      ((int)(c[2]*255)<<8)  +
      (facetback_alpha_flag ? ((int)(c[3]*255)) : 255);
    } 
    else return INDEX_TO_RGBA(get_facet_backcolor(f_id));
  }
  else return get_facet_backcolor(f_id);
}

/* Data used in plotting circular arcs */
/* using fact that circle is inversion of line */
 static REAL wlambda[17] = 
           { 1e30,10,5,4,3,2,1.5,1.2,1,.85,.7,.6,.5,.35,.25,.12,0.};

/*****************************************************************************
*
*  Function: graphgen()
*
*  purpose:  Generates data for each triangle and calls the display
*                function graph_facet().
*
*
*  Return value: number of triangles plotted
*/

int graphgen()
{
  int  graphcount = 0;  /* number of facets done */
  int b,i;
  REAL *c;
  FILE *mapfd = NULL;
  int dummy;
  struct extra *ex;

  if ( torus_display_mode == TORUS_CLIPPED_MODE )
    lazy_transforms_flag = 0 ;

  if ( slice_view_flag )
  { /* check coefficients set */
    for ( i = 0 ; i < SDIM ; i++ )
      if ( slice_coeff[i] )
        break;
    if ( i == SDIM )
      kb_error(1900,"slice_view on, but slice_coeff[] not set.\n",
        RECOVERABLE);
  }

  if ( rgb_colors_flag )
  { int three = 3;
    /* see if using RGBA colors for elements */
    edge_rgb_color_attr = find_extra("ergb",&dummy);
    if ( edge_rgb_color_attr > 0 )
    { ex = EXTRAS(EDGE)+edge_rgb_color_attr;
      if ( ex->array_spec.datacount < 3 ) 
        expand_attribute(EDGE,edge_rgb_color_attr,&three);
      edge_alpha_flag = (ex->array_spec.datacount >= 4);
    }
    facet_rgb_color_attr = find_extra("frgb",&dummy);
    if ( facet_rgb_color_attr > 0 )
    { ex = EXTRAS(FACET)+facet_rgb_color_attr;
      if ( ex->array_spec.datacount < 3 )
        expand_attribute(FACET,facet_rgb_color_attr,&three);
      facet_alpha_flag = (ex->array_spec.datacount >= 4);
    }
    facet_rgb_backcolor_attr = find_extra("fbrgb",&dummy);
    if ( facet_rgb_backcolor_attr > 0 )
    { ex = EXTRAS(FACET)+facet_rgb_backcolor_attr;
      if ( ex->array_spec.datacount < 3 ) 
        expand_attribute(FACET,facet_rgb_backcolor_attr,&three);
      facetback_alpha_flag = (ex->array_spec.datacount >= 4);
    }
  } else /* turn off rgb */
  { edge_rgb_color_attr = -1;
    facet_rgb_color_attr = -1;
    facet_rgb_backcolor_attr = -1;
  }
  (*graph_start)();  /* device-specific initialization */

  if ( markedgedrawflag )
  { edge_id e_id;
    FOR_ALL_EDGES(e_id) unset_attr(e_id,EDGE_DRAWN);
  }

  iterate_flag = 2;
  if ( box_flag )
    for ( i = 0 ; i < SDIM ; i++ ) 
      bounding_box[i][0] = bounding_box[i][1] = 0.0;

  if ( colorflag )
  do 
  {
    if ( strlen(cmapname) == 0 )
      prompt("Enter name of colormap file: ",cmapname,sizeof(cmapname));
    if ( cmapname[0] == 0 )
    { outstring("No colormap used.\n"); colorflag = 0; }
    mapfd = path_open(cmapname,NOTDATAFILENAME);
    if ( mapfd )
    {
      colormap = (maprow *)temp_calloc(4*web.bodycount,sizeof(REAL));
      for ( b = 0 ; b < web.bodycount ; b++ )
      { c = colormap[b];
#ifdef LONGDOUBLE
        if ( fscanf(mapfd,"%Lf %Lf %Lf %Lf",
#else
        if ( fscanf(mapfd,"%lf %lf %lf %lf",
#endif
            c,c+1,c+2,c+3) != 4 ) break;
      }
      if ( b < web.bodycount )
      { sprintf(errmsg,
             "Colormap file has only %d entries for %d bodies.\n",
              b,web.bodycount);
        kb_error(1046,errmsg,WARNING);
        for ( ; b < web.bodycount ; b++ )
        { c = colormap[b];
          c[0] = c[1] = c[2] = c[3] = 0.5;
        } 
      }
    }
    else perror(cmapname);
  }  while ( mapfd == NULL );
  /* call appropriate facet generator */
  if ( web.symmetry_flag && (torus_display_mode==TORUS_CONNECTED_MODE) )
  { if ( web.skel[BODY].count <= 0 )
    { kb_error(1049,
      "There are no bodies to display connectedly.  Reverting to raw mode.\n",
      WARNING);
      torus_display_mode = TORUS_RAW_MODE;
    }
  }
  if ( web.symmetry_flag && (torus_display_mode==TORUS_CONNECTED_MODE) )
  { if ( web.representation == STRING ) 
      torus_cells();
    else torus_bodies();
    bare_edges();
  }
  else 
  { if ( web.representation == SIMPLEX ) hi_dim_graph();
    else if ( web.representation == STRING ) 
    { if ( show_expr[FACET] && show_expr[FACET]->start )
        plain_string_facets(); 
      plain_edges();  /* so edges get drawn on top */
    }
    else if ( web.representation == SOAPFILM ) 
    { plain_facets();  bare_edges();
    }
  }


  /* bounding box (torus only) */
  if ( box_flag && (web.torus_flag || (torus_display_mode == TORUS_CLIPPED_MODE)))
  { int k,m,n,a[MAXCOORD];
    struct graphdata gdata[2];
    REAL **per = web.torus_display_period ? web.torus_display_period : web.torus_period;
   
    memset(gdata,0,2*sizeof(struct graphdata));
    gdata[0].color = gdata[0].ecolor = 
      (edge_rgb_color_attr > 0 ) ? INDEX_TO_RGBA(BLUE) : BLUE;
    gdata[0].etype = REGULAR_EDGE;
    
    if ( SDIM == 2 )
     for ( a[0] = 0 ; a[0] <= 1 ; a[0]++ )
      for ( a[1] = 0 ; a[1] <= 1 ; a[1]++ )
       for ( k = 0 ; k < 2 ; k++ )
       { if ( a[k] == 0 )
         { for ( m = 0 ; m < SDIM ; m++ )
           { for ( n = 0, gdata[0].x[m] = web.display_origin[m] ; n < SDIM ; n++ )
               gdata[0].x[m] += a[n]*per[n][m];
             gdata[1].x[m] = gdata[0].x[m] + per[k][m];
           }
           (*graph_edge_transforms)(gdata,NULLID); 
           /* (*graph_edge)(gdata,NULLID); */
         }
       }
    else
    for ( a[0] = 0 ; a[0] <= 1 ; a[0]++ )
     for ( a[1] = 0 ; a[1] <= 1 ; a[1]++ )
      for ( a[2] = 0 ; a[2] <= 1 ; a[2]++ )
       for ( k = 0 ; k < 3 ; k++ )
        if ( a[k] == 0 )
        { for ( m = 0 ; m < SDIM ; m++ )
          { for ( n = 0, gdata[0].x[m] = web.display_origin[m] ; n < SDIM ; n++ )
              gdata[0].x[m] += a[n]*per[n][m];
            gdata[1].x[m] = gdata[0].x[m] + per[k][m];
          }
          (*graph_edge_transforms)(gdata,NULLID); 
          /*(*graph_edge)(gdata,NULLID);*/
        }
  }
  else if ( box_flag )
  { int k,m,a[MAXCOORD];
    struct graphdata gdata[2];

    memset(gdata,0,2*sizeof(struct graphdata));
    gdata[0].color = gdata[0].ecolor = 
       (edge_rgb_color_attr > 0 ) ? INDEX_TO_RGBA(BLACK) : BLACK;
    gdata[0].etype = REGULAR_EDGE;

    if ( SDIM == 2 )
    { for ( a[0] = 0 ; a[0] <= 1 ; a[0]++ )
      for ( a[1] = 0 ; a[1] <= 1 ; a[1]++ )
       for ( k = 0 ; k < 2 ; k++ )
         if ( a[k] == 0 )
         { for ( m = 0 ; m < SDIM ; m++ )
           { gdata[0].x[m] = bounding_box[m][a[m]];
             gdata[1].x[m] = bounding_box[m][m==k?1:a[m]];
           }
           graph_edge_clip(gdata,NULLID);
         }
    }
    else
    for ( a[0] = 0 ; a[0] <= 1 ; a[0]++ )
     for ( a[1] = 0 ; a[1] <= 1 ; a[1]++ )
      for ( a[2] = 0 ; a[2] <= 1 ; a[2]++ )
       for ( k = 0 ; k < 3 ; k++ )
        if ( a[k] == 0 )
        { for ( m = 0 ; m < SDIM ; m++ )
          { gdata[0].x[m] = bounding_box[m][a[m]];
            gdata[1].x[m] = bounding_box[m][m==k?1:a[m]];
          }
          graph_edge_clip(gdata,NULLID);       
        }
  }
 


 
  (*graph_end)();  /* device-specific termination */

  if ( colorflag ) temp_free((char *)colormap);

  return  graphcount;
} /* end graphgen() */

/*****************************************************************
*
*  function: plain_facets()
*
*  purpose:  plots all facets one by one.
*
*/

void plain_facets()
{
  int i,j,ii,jj,k;
  facetedge_id fe,fe_id;
  edge_id e_id;
  facet_id f_id;
  body_id b0_id,b1_id;
  REAL **verts; /* for adjusted triangle vertices */
  struct graphdata *gdata;
  int ctrlpts = web.skel[FACET].ctrlpts;
  int segs = 8;  /* for smooth_graph */
  int to_alloc = ctrlpts+1 > ((segs+1)*(segs+2))/2+1  ?
                 ctrlpts+1 : ((segs+1)*(segs+2))/2+1; 
  REAL ***points = NULL;
  int gorder = 1; /* order actually used in graphing subfacets */

  gdata = (struct graphdata *)temp_calloc(to_alloc,sizeof(struct graphdata));
  verts = (REAL **)temp_calloc(ctrlpts,sizeof(REAL *));
  if ( web.modeltype == LAGRANGE  )
  { gorder = smooth_graph_flag ? segs : web.lagrange_order;
    points = temp_dmatrix3(gorder+1,gorder+1,SDIM);
    for ( i = 0 ; i < ctrlpts ; i++ ) 
      verts[i] = gdata[i].x;
  }
  else
  for ( i = 0 ; i < FACET_VERTS ; i++ ) 
  { if ( web.modeltype == QUADRATIC ) /* mdpts after verts in gdata */
    { verts[2*i] = gdata[i].x;
      verts[2*i+1] = gdata[FACET_VERTS+i].x;  
    }
    else 
      verts[i] = gdata[i].x;
  }

  for ( i = 0 ; i <= ctrlpts ; i++ ) 
      gdata[i].x[3] = 1.0;  /* homogeneous coord */
     
  MFOR_ALL_FACETS(f_id)
  { 
    int nbrs;  /* number of neighboring bodies */
    int fattr = get_fattr(f_id);
    if ( breakflag ) break;

    #ifdef MPI_EVOLVER
    if ( !mpi_show_corona_flag && (id_task(f_id) != this_task) )
       continue;
    #endif

    if ( (fattr & (BOUNDARY|CONSTRAINT)) && !bdry_showflag )
        continue;
    if ( fattr & NODISPLAY )
        continue;
    gdata[0].color = get_facet_color_2(f_id);
    gdata[0].backcolor = get_facet_backcolor_2(f_id);

    if ( no_wall_flag )
    { /* skip facets with all three vertices on walls */
      fe = get_facet_fe(f_id);

      if ( get_vattr(get_fe_headv(fe)) & (HIT_WALL|CONSTRAINT) )
         if ( get_vattr(get_fe_tailv(fe)) & (HIT_WALL|CONSTRAINT) ) 
          { fe = get_next_edge(fe);
            if ( get_vattr(get_fe_headv(fe)) & (HIT_WALL|CONSTRAINT) )
               continue;
          }
    }      

    if ( show_expr[FACET] && show_expr[FACET]->start )
    { 
      if ( !eval(show_expr[FACET],NULL,f_id,NULL) ) 
         gdata[0].color = gdata[0].backcolor = UNSHOWN; /* maybe do edges */
    }

    nbrs =  (valid_id(get_facet_body(f_id)) ? 1 : 0) 
            + (valid_id(get_facet_body(facet_inverse(f_id))) ? 1 : 0);
    if ( (nbrs >= 2) && !innerflag ) continue;
    if ( (nbrs < 2) && !outerflag ) continue;
    if ( colorflag ) /* get vertex color */
    { 
      b0_id = get_facet_body(f_id);
      b1_id = get_facet_body(facet_inverse(f_id));
      for ( i = 0 ; i < ctrlpts ; i++ )    /* vertex loop */
      { if ( valid_id(b0_id) ) 
         gdata[i].backcolor = 
           (facet_rgb_color_attr > 0 )? INDEX_TO_RGBA(loc_ordinal(b0_id)) : 
                 loc_ordinal(b0_id);
        else  gdata[i].backcolor = 0;
        if ( valid_id(b1_id) ) 
          gdata[i].color =
           ( facet_rgb_color_attr > 0 ) ? INDEX_TO_RGBA(loc_ordinal(b1_id)): 
                loc_ordinal(b1_id);
        else  gdata[i].color = 0;
      }
    }
    /* get vertices; verts is list of gdata.x pointers */
    if ( labelflag != 0 ) get_facet_verts(f_id,verts,NULL);
    else get_facet_verts_special(f_id,verts,NULL);
    if ( web.modeltype == LAGRANGE )
    { vertex_id *v = get_facet_vertices(f_id);
      for ( i = 0 ; i < ctrlpts ; i++ ) gdata[i].v_id = v[i];
    }
    else 
    { fe = get_facet_fe(f_id);
      for ( i = 0 ; i < FACET_VERTS ; i++, fe = get_next_edge(fe) )
      { e_id = get_fe_edge(fe);
        if ( web.modeltype == LINEAR ) gdata[i].v_id = get_edge_tailv(e_id);
        else if ( web.modeltype == QUADRATIC )
        { gdata[i].v_id = get_edge_tailv(e_id);
          gdata[FACET_VERTS+i].v_id = get_edge_midv(e_id); 
        }
      }
    }
    /* do inner clipping, if called for */
    if ( inner_clip_flag )
    { 
      for ( i = 0 ; i < ctrlpts ; i++ )
      { REAL dist = 0.0;
        REAL *x = get_coord(web.zoom_v);

        for ( j = 0 ; j < SDIM ; j++ )
           dist += (x[j]-verts[i][j])*(x[j]-verts[i][j]);

        if ( sqrt(dist) > inner_clip_rad ) break; /* it's a keeper */
      }
      if ( i == ctrlpts ) continue; /* entirely inside */
    }
  
    if ( gdata[0].color != gdata[0].backcolor )
    { /* need normal for separation */ 
      REAL dd;
      if ( web.modeltype == LAGRANGE )
         vnormal(gdata[0].x,gdata[web.lagrange_order].x,gdata[ctrlpts-1].x,
                   gdata[0].norm);
      else vnormal(gdata[0].x,gdata[1].x,gdata[2].x,gdata[0].norm);
      dd = sqrt(SDIM_dot(gdata[0].norm,gdata[0].norm));
      if ( dd > 0.0 )
         for ( i = 0 ; i < SDIM ; i++ )
           gdata[0].norm[i] /= dd;
    }
    if ( normflag || thickenflag ) 
    {
      fe = get_facet_fe(f_id);
      for ( i = 0 ; i < FACET_VERTS ; i++ )
      { calc_vertex_smooth_normal(get_fe_tailv(fe),fe,gdata[i].norm);
        fe = get_next_edge(fe);
      }
    }
    /* check for special edges */
    fe_id = get_facet_fe(f_id);
    if ( web.representation != SIMPLEX )
     for ( i = 0 ; i < 3 ; i++, fe_id = get_next_edge(fe_id) )
     { int eattr;
       gdata[i].etype = INVISIBLE_EDGE; /* default */
       e_id = get_fe_edge(fe_id);
       if ( inverted(e_id) ) gdata[i].etype |= LABEL_REVERSED;
       gdata[i].ecolor = get_edge_color_2(e_id);
       gdata[i].id = e_id;
       eattr = get_eattr(e_id);
       if ( markedgedrawflag )
       { if ( eattr & EDGE_DRAWN ) continue; }             
             
       if ( get_edge_color(e_id) == CLEAR ) continue;
       if ( eattr & BOUNDARY ) gdata[i].etype |= BOUNDARY_EDGE;
       if ( equal_id(get_next_facet(fe_id),fe_id) ) /* valence 1 */
          gdata[i].etype |= SINGLE_EDGE;
       else if  ( !equal_id(get_next_facet(fe_id),get_prev_facet(fe_id)) )
          gdata[i].etype |= TRIPLE_EDGE; /* triple line at least */
       if ( (eattr & HIT_WALL) && !(fattr & CONSTRAINT) )
          gdata[i].etype |= CONSTRAINT_EDGE;
       if ( (eattr & FIXED) && !(fattr & FIXED) )
          gdata[i].etype |= FIXED_EDGE;
       if ( show_expr[EDGE] && show_expr[EDGE]->start )
       { if ( eval(show_expr[EDGE],NULL,e_id,NULL) ) 
            gdata[i].etype |= REGULAR_EDGE;
         else gdata[i].etype &= (~EBITS) | INVISIBLE_EDGE;
       }
       if ( markedgedrawflag && ((gdata[i].etype & EBITS) != INVISIBLE_EDGE) )
         set_attr(e_id,EDGE_DRAWN);
     }

    if ( ridge_color_flag )
    {
      REAL side[FACET_EDGES][MAXCOORD];
      REAL otherside[FACET_EDGES][MAXCOORD];
            
      fe = get_facet_fe(f_id);
      for ( i = 0 ; i < FACET_EDGES ; i++ )
      { get_fe_side(fe,side[i]);
        get_fe_side(get_next_edge(get_next_facet(fe)),otherside[i]);
        fe = get_next_edge(fe);
      }
      for ( i = 0 ; i < FACET_EDGES ; i++ )
       if ( triple_prod(side[i],side[(i+1)%FACET_EDGES],otherside[i]) < 0.0 )
         gdata[i].ecolor = 
          ( edge_rgb_color_attr > 0 ) ? INDEX_TO_RGBA(RIDGE) : RIDGE;
       else gdata[i].ecolor = 
          ( edge_rgb_color_attr > 0 ) ? INDEX_TO_RGBA(VALLEY) : VALLEY; 
    }

    if ( web.modeltype == LINEAR )
    { /* call option-specific routine */
      gdata->flags |= LABEL_FACET|LABEL_EDGE|LABEL_HEAD|LABEL_TAIL;
      option_facet(gdata,f_id);         
    } 
    else if ( (web.modeltype == QUADRATIC) )
    { 
      /* quadratic, plot as four subtriangles */
      struct graphdata qdata[FACET_VERTS+1];
      int flags = (gdata[0].flags | LIST_FACET) & 
           ~(LABEL_FACET|LABEL_EDGE|LABEL_HEAD|LABEL_TAIL);
      memcpy((char*)qdata,(char*)gdata,3*sizeof(struct graphdata));

      for( j=0 ; j<SDIM ; j++ )
         { qdata[1].x[j] = gdata[3].x[j]; qdata[2].x[j]=gdata[5].x[j]; }
      qdata[1].etype=INVISIBLE_EDGE;
      qdata[1].v_id = gdata[3].v_id;
      qdata[2].v_id = gdata[5].v_id;
      qdata[0].id = gdata[0].id; 
      qdata[1].id = NULLEDGE; 
      qdata[2].id = gdata[2].id;
      qdata[0].flags = flags;
      qdata[1].ecolor = BLACK;
      option_facet(qdata,f_id);

      for(j=0;j<SDIM;j++)
      { qdata[0].x[j]=gdata[3].x[j];
        qdata[1].x[j]=gdata[1].x[j];
        qdata[2].x[j]=gdata[4].x[j];
      }
      qdata[1].etype=gdata[1].etype;
      qdata[2].etype=INVISIBLE_EDGE;
      qdata[0].v_id = gdata[3].v_id;
      qdata[1].v_id = gdata[1].v_id;
      qdata[2].v_id = gdata[4].v_id;
      qdata[0].id = gdata[0].id; 
      qdata[1].id = gdata[1].id; 
      qdata[2].id = NULLEDGE;
      qdata[2].ecolor = BLACK;
      qdata[0].flags = flags;
      qdata[1].ecolor = gdata[1].ecolor;
      option_facet(qdata,f_id);

      for(j=0;j<SDIM;j++)
      { qdata[0].x[j]=gdata[5].x[j];
        qdata[1].x[j]=gdata[4].x[j];
        qdata[2].x[j]=gdata[2].x[j];
      }
      qdata[2].etype=gdata[2].etype;
      qdata[0].etype=INVISIBLE_EDGE;
      qdata[0].v_id = gdata[5].v_id;
      qdata[1].v_id = gdata[4].v_id;
      qdata[2].v_id = gdata[2].v_id;
      qdata[0].id = NULLEDGE; 
      qdata[1].id = gdata[1].id; 
      qdata[2].id = gdata[2].id;
      qdata[0].flags = flags;
      qdata[0].ecolor = BLACK;
      qdata[2].ecolor = gdata[2].ecolor;
      option_facet(qdata,f_id);

      for(j=0;j<SDIM;j++)
      { qdata[0].x[j]=gdata[3].x[j];
        qdata[1].x[j]=gdata[4].x[j];
        qdata[2].x[j]=gdata[5].x[j];
      }
      qdata[0].etype=INVISIBLE_EDGE;
      qdata[1].etype=INVISIBLE_EDGE;
      qdata[2].etype=INVISIBLE_EDGE;
      qdata[0].v_id = gdata[3].v_id;
      qdata[1].v_id = gdata[4].v_id;
      qdata[2].v_id = gdata[5].v_id;
      qdata[0].id = NULLEDGE; qdata[1].id = NULLEDGE;
      qdata[2].id = NULLEDGE;
      qdata[0].flags = flags | LABEL_FACET;
      qdata[1].ecolor = BLACK;
      qdata[2].ecolor = BLACK;
      option_facet(qdata,f_id);
    } /* end quadratic */
    else /* Lagrange */
    {
      /* plot as subtriangles */
      int flags = (gdata[0].flags|LIST_FACET) & 
           ~(LABEL_FACET|LABEL_EDGE|LABEL_HEAD|LABEL_TAIL);
      struct graphdata qdata[FACET_VERTS+1];
      memcpy((char*)qdata,(char*)gdata,3*sizeof(struct graphdata));
      if ( bezier_flag )
      { REAL t[2];
        for ( i = 0 ; i <= gorder ; i++ )
        { t[0] = i/(REAL)gorder;
          for ( j = 0 ; i+j <= gorder ; j++ )
          { t[1] = j/(REAL)gorder;
            bezier_eval_2d(web.lagrange_order,SDIM,t,verts,
               points[(gorder-i-j)][j]);
          }
        }
      }
      else /* regular lagrange */
      { REAL t[2];
        if ( smooth_graph_flag )
        { for ( i = 0 ; i <= gorder ; i++ )
          { t[0] = i/(REAL)gorder;
            for ( j = 0 ; i+j <= gorder ; j++ )
            { t[1] = j/(REAL)gorder;
              lagrange_eval_2d(web.lagrange_order,SDIM,t,verts,points[i][j]);
            }
          }
        }
        else /* just copy over */
          for ( j = 0,jj=0 ;  j <= web.lagrange_order ; j++ )
            for ( i = 0 ; i+j <= web.lagrange_order ; i++,jj++ )
              for ( k = 0 ; k < SDIM ; k++ )
                 points[i][j][k]=gdata[jj].x[k];
      }

      for ( j = 0,jj=0,ii=gorder+1 ; j < gorder ; j++,jj++ )
        for ( i = 0 ; i < gorder-j ; i++,jj++,ii++ )
        { /* first, lower left triangles */ 
          for ( k = 0 ; k < SDIM ; k++ )
          { qdata[0].x[k] = points[i][j][k];
            qdata[1].x[k] = points[i+1][j][k];
            qdata[2].x[k] = points[i][j+1][k];
          }
          qdata[0].v_id = gdata[jj].v_id; 
          qdata[1].v_id = gdata[jj+1].v_id;
          qdata[2].v_id = gdata[ii].v_id;
          qdata[0].id = (j == 0) ? gdata[0].id : NULLEDGE;
          qdata[1].id = (i == gorder-j-1) ? gdata[1].id : NULLEDGE;
          qdata[2].id = (i == 0) ? gdata[2].id : NULLEDGE;
          qdata[0].etype = (j==0) ? gdata[0].etype : INVISIBLE_EDGE;
          qdata[1].etype = (i+j+1==gorder) ? gdata[1].etype : INVISIBLE_EDGE;
          qdata[2].etype = (i==0) ? gdata[2].etype : INVISIBLE_EDGE;
          qdata[0].ecolor = (j==0) ? gdata[0].ecolor : BLACK;
          qdata[1].ecolor = (i+j+1==gorder) ? gdata[1].ecolor : BLACK;
          qdata[2].ecolor = (i==0) ? gdata[2].ecolor : BLACK;
          qdata[0].flags = flags;
          option_facet(qdata,f_id);

          if ( i < gorder-j-1 )
          { /* now upper right triangles */
            int n;
            for ( k = 0 ; k < SDIM ; k++ )
            { qdata[0].x[k] = points[i+1][j][k];
              qdata[1].x[k] = points[i+1][j+1][k];
              qdata[2].x[k] = points[i][j+1][k];
            }
            qdata[0].v_id = gdata[jj+1].v_id;
            qdata[1].v_id = gdata[ii+1].v_id;
            qdata[2].v_id = gdata[ii].v_id;
            for ( k = 0 ; k < FACET_EDGES ; k++ ) qdata[k].id = NULLEDGE;
            qdata[0].flags = flags |
              (i==gorder/3 && j==gorder/3 ? LABEL_FACET:0);
            for ( n = 0 ; n < FACET_VERTS ; n++ ) 
            { qdata[n].etype = INVISIBLE_EDGE;
              qdata[n].ecolor = BLACK;
            }
            option_facet(qdata,f_id);
          }
       }
    } /* end lagrange */
  } /* end FOR_ALL_FACETS */

  temp_free((char*)gdata);
  temp_free((char*)verts);
  if ( points ) free_temp_matrix3(points);

} /* end plain_facets() */

/*****************************************************************
*
*  function: plain_string_facets()
*
*  purpose:  Plots string facets if enabled by show_expr.
*            Since facet outline may be open polygon, it
*            has to tessellate it first.
*
*/

void plain_string_facets()
{
  int i,n;
  facet_id f_id;
  struct graphdata *gdata;
  int to_alloc = 100;

  gdata = (struct graphdata *)temp_calloc(to_alloc,sizeof(struct graphdata));

  for ( i = 0 ; i < to_alloc ; i++ ) 
  { gdata[i].x[3] = 1.0;  /* homogeneous coord */
    gdata[i].etype = INVISIBLE_EDGE;
  }
     
  MFOR_ALL_FACETS(f_id)
  { 
    int fattr = get_fattr(f_id);
    int vcount;  /* number of vertices */
    facetedge_id start_fe,fe;
    REAL *x;
    vertex_id midv;

    if ( breakflag ) break;

    #ifdef MPI_EVOLVER
    if ( !mpi_show_corona_flag && (id_task(f_id) != this_task) )
       continue;
    #endif

    if ( !show_expr[FACET] || !show_expr[FACET]->start || 
                           !eval(show_expr[FACET],NULL,f_id,NULL) ) 
        continue;

    if ( (fattr & (BOUNDARY|CONSTRAINT)) && !bdry_showflag )
        continue;
    if ( fattr & NODISPLAY )
        continue;

    gdata[0].color = get_facet_color_2(f_id);
    gdata[0].backcolor = get_facet_backcolor_2(f_id);
    vcount = 0;

    /* get vertices; verts is list of gdata.x pointers */
    fe = start_fe = get_facet_fe(f_id);
    if ( !valid_id(fe) )
      continue;
    gdata[vcount].v_id = get_fe_tailv(fe);
    x = get_coord(gdata[vcount].v_id);
    for ( i = 0 ; i < SDIM ; i++ )
    { gdata[vcount].x[i] = x[i];
    }
    vcount++;

    do
    { REAL s[MAXCOORD];
    
      if ( web.modeltype == LINEAR )
      {
        gdata[vcount].v_id = get_fe_headv(fe);
        get_edge_side(get_fe_edge(fe),s);
  
        for ( i = 0 ; i < SDIM ; i++ )
          gdata[vcount].x[i] = gdata[vcount-1].x[i] + s[i]; 

        vcount++;
      }
      else if ( web.modeltype == QUADRATIC )
      { REAL *tailx = gdata[vcount-1].x,*midxp;
        REAL midx[MAXCOORD],headx[MAXCOORD];
        REAL w1[MAXCOORD],w2[MAXCOORD],w[MAXCOORD];
        REAL mag,mag1,mag2;
        int k;
        int segments;
        REAL ang;
        
        gdata[vcount+1].v_id = get_fe_headv(fe);
        get_edge_side(get_fe_edge(fe),s);
  
        for ( i = 0 ; i < SDIM ; i++ )
          headx[i] = tailx[i] + s[i]; 
        midv = get_fe_midv(fe);
        midxp = get_coord(midv);
        
        if ( inverted(get_fe_edge(fe)) )
        { REAL *headxp = get_coord(get_fe_headv(fe));
           for ( i = 0 ; i < SDIM ; i++ )
              midx[i] = headx[i] - (headxp[i]-midxp[i]);
        }
        else 
        { REAL *tailxp = get_coord(get_fe_tailv(fe));
          for ( i = 0 ; i < SDIM ; i++ )
              midx[i] = tailx[i] + (midxp[i]-tailxp[i]);
        }

        for (i = 0 ; i < SDIM ; i++ ) 
        { w1[i] = midx[i] - tailx[i];
          w2[i] = headx[i] - tailx[i];
        }
        mag1 = SDIM_dot(w1,w1); mag2 = SDIM_dot(w2,w2);
        for ( i = 0 ; i < SDIM  ; i++ )
        { w1[i] /= mag1; w2[i] /= mag2; }
          
        /* figure out how many segments to do */
        ang = 4*acos(SDIM_dot(w1,w2)*sqrt(mag1*mag2));
        segments = (int)(ang/(M_PI/180*string_curve_tolerance));
        if ( segments < 2 ) 
          segments = 2;
          
        if ( vcount+segments >= to_alloc-20 )
        { to_alloc *= 2;
          gdata = (struct graphdata *)temp_realloc((char*)gdata,
            to_alloc*sizeof(struct graphdata));
          tailx = gdata[vcount-1].x; /* since it moved */
        }
        
        for ( k = 1 ; k <= segments ; k++ )
        { 
          if ( circular_arc_flag )
          { for ( i = 0 ; i < SDIM ; i++ ) /* circle as inversion of line */
              w[i] = w2[i] + (segments-k)/(REAL)k*(w1[i]-w2[i]);
            mag = SDIM_dot(w,w);
            for ( i = 0 ; i < SDIM ; i++ )
              gdata[vcount].x[i] = tailx[i] + w[i]/mag;
          }
          else /* quadratic spline */
          { REAL t = 2*k/(REAL)segments;
            REAL c1 = (t-1)*(t-2)/2;
            REAL c2 = t*(2-t);
            REAL c3 = t*(t-1)/2; 
            for ( i = 0 ; i < SDIM ; i++ )
              gdata[vcount].x[i] = c1*tailx[i]+c2*midx[i]+c3*headx[i];
          }
          vcount++;
        }
        
      }
      else /* LAGRANGE */
      { edge_id e_id = get_fe_edge(fe);
        vertex_id *v = get_edge_vertices(e_id);
        REAL *midx;
        
        gdata[vcount+web.lagrange_order-1].v_id = get_fe_headv(fe);
        get_edge_side(get_fe_edge(fe),s);
  
        for ( i = 0 ; i < SDIM ; i++ )
          gdata[vcount+web.lagrange_order-1].x[i] = gdata[vcount-1].x[i] + s[i]; 
      
        if ( inverted(get_fe_edge(fe)) )
        { REAL *headx = get_coord(get_fe_headv(fe));
          for ( n = 1 ; n < web.lagrange_order ; n++ )
          { midx = get_coord(v[web.lagrange_order - n]);
            for ( i = 0 ; i < SDIM ; i++ )
              gdata[vcount+n-1].x[i] = gdata[vcount+web.lagrange_order-1].x[i] - (headx[i]-midx[i]);
          }
        }
        else 
        { REAL *tailx = get_coord(get_fe_tailv(fe));
          for ( n = 1 ; n < web.lagrange_order ; n++ )
          { midx = get_coord(v[n]);
            for ( i = 0 ; i < SDIM ; i++ )
              gdata[vcount+n-1].x[i]  = gdata[vcount-1].x[i] + (midx[i]-tailx[i]);
          }
        }

        vcount += web.lagrange_order;
      }
      
      if ( vcount >= to_alloc-20 )
      { to_alloc *= 2;
        gdata = (struct graphdata *)temp_realloc((char*)gdata,
           to_alloc*sizeof(struct graphdata));
      }
      
      fe = get_next_edge(fe);
    } while ( valid_id(fe) && !equal_id(fe,start_fe) );
    graph_string_facet(f_id,gdata,vcount);  

  } /* end FOR_ALL_FACETS */

  temp_free((char*)gdata);

} /* end plain_facets() */

/*************************************************************************
*
* function: option_facet()
*
* purpose: call facet plotter for specific options in effect
*/
void option_facet(gdata,f_id)
struct graphdata *gdata;
facet_id f_id;
{
  if ( web.torus_flag )
  { gdata[0].flags &= ~(LIST_FACET | SIMPLE_FACET);
  }
  if ( web.torus_flag && (torus_display_mode == TORUS_CLIPPED_MODE) )
  {  torus_clip(gdata,SDIM-1,f_id); }
  else
  { if ( !web.symmetry_flag && (web.representation != STRING) ) 
      gdata[0].flags |= SIMPLE_FACET;
    graph_facet_transforms(gdata,f_id);
  }
}

/*****************************************************************
*
* function: graph_edge_transforms()
*
* purpose: graph edge and all transforms specified in datafile
*
*/

void graph_edge_transforms(gdata,e_id)
struct graphdata *gdata;
edge_id e_id;
{ int i,j,k;
  struct graphdata gt[3];
  int ctrl_pts = gdata[0].flags & EDGE_ARC ? 3 : 2;

  memset(gt,0,3*sizeof(struct graphdata));
  for ( i = 0 ; i < ctrl_pts ; i++ ) 
     gdata[i].x[SDIM] = 1.0;  /* homogeneous coord */
  if ( box_flag )
     for ( j = 0 ; j < ctrl_pts ; j++ )
       for  ( k = 0 ; k < SDIM ; k++ )
       { if ( gdata[j].x[k] < bounding_box[k][0] )
           bounding_box[k][0] = gdata[j].x[k];
         if ( gdata[j].x[k] > bounding_box[k][1] )
           bounding_box[k][1] = gdata[j].x[k];
       }

  if ( transforms_flag && !lazy_transforms_flag && view_transforms &&
             (transform_colors_flag || (graph_facet != geomview_facet) )  )
    for ( i = 0 ;  i < transform_count ; i++ )
    { gt[0] = gdata[0];
      gt[1].v_id = gdata[1].v_id;
      for ( j = 0 ; j < ctrl_pts ; j++ )
        { matvec_mul(view_transforms[i],gdata[j].x,gt[j].x,
                  SDIM+1,SDIM+1); 
          for ( k = 0 ; k <= SDIM ; k++ )
          { gt[j].x[k] /= gt[j].x[SDIM]; /* normalize */
            if ( box_flag )
            { if ( gt[j].x[k] < bounding_box[k][0] )
                 bounding_box[k][0] = gt[j].x[k];
              if ( gt[j].x[k] > bounding_box[k][1] )
                 bounding_box[k][1] = gt[j].x[k];
            }
          }
        }
        if ( (web.representation == STRING) && transform_colors_flag )
          gt[0].ecolor = transform_colors[i];
        if ( (torus_display_mode == TORUS_CLIPPED_MODE) &&
            web.torus_display_period && !web.torus_flag )
          torus_edge_clip(gt,0);
        else 
        graph_edge_clip(gt,e_id);
     }
  else
  { if ( (torus_display_mode == TORUS_CLIPPED_MODE) &&
       web.torus_display_period && !web.torus_flag )
      torus_edge_clip(gdata,0);
    else 
     graph_edge_clip(gdata,e_id);
  }
} /* graph_edge_transforms */

/*****************************************************************
*
* function: graph_facet_transforms()
*
* purpose: graph facet and all transforms specified in datafile
*
*/

void graph_facet_transforms(gdata,f_id)
struct graphdata *gdata;
facet_id f_id;
{ struct graphdata gt[FACET_VERTS+1];
  int i,j,k;

  memset(gt,0,(FACET_VERTS+1)*sizeof(struct graphdata));

  for ( i = 0 ; i < FACET_VERTS ; i++ ) 
     gdata[i].x[SDIM] = 1.0;  /* homogeneous coord */
  if ( box_flag )
    for ( j = 0 ; j < FACET_VERTS ; j++ )
      for ( k = 0 ; k < SDIM ; k++ )
      { if ( gdata[j].x[k] < bounding_box[k][0] )
          bounding_box[k][0] = gdata[j].x[k];
        if ( gdata[j].x[k] > bounding_box[k][1] )
          bounding_box[k][1] = gdata[j].x[k];
      }
  if ( transforms_flag && !lazy_transforms_flag && view_transform_det &&
             (transform_colors_flag || (graph_facet != geomview_facet) )  )
  {
    for ( i = 0 ;  i < transform_count ; i++ )
    { if ( view_transform_det[i] > 0 )
      { for ( j = 0 ; j < FACET_VERTS ; j++ )
        { gt[j] = gdata[j];
          matvec_mul(view_transforms[i],gdata[j].x,gt[j].x,SDIM+1,SDIM+1); 
          for ( k = 0 ; k <= SDIM ; k++ )
             gt[j].x[k] /= gt[j].x[SDIM]; /* normalize */
          matvec_mul(view_transforms[i],gdata[j].norm,gt[j].norm,
             SDIM,SDIM); 
        }
      }
      else
      { /* reverse orientation */
        for ( j = 0 ; j < FACET_VERTS ; j++ )
        { gt[j] = gdata[2-j];
          matvec_mul(view_transforms[i],gdata[2-j].x,gt[j].x,SDIM+1,SDIM+1); 
          for ( k = 0 ; k <= SDIM ; k++ )
            gt[j].x[k] /= gt[j].x[SDIM]; /* normalize */
          matvec_mul(view_transforms[i],gdata[2-j].norm,gt[j].norm,SDIM,SDIM); 
        }
        gt[0].color = gdata[0].color;
        gt[0].backcolor = gdata[0].backcolor;
        gt[0].ecolor = gdata[1].ecolor; gt[1].ecolor = gdata[0].ecolor;
        gt[2].ecolor = gdata[2].ecolor;
        gt[0].etype = gdata[1].etype; gt[1].etype = gdata[0].etype;
        gt[2].etype = gdata[2].etype;
        gt[0].flags = gdata[0].flags;
      }

      if ( transform_colors[i] == SWAP_COLORS )
      { int temp = gt[0].color;
        gt[0].color = gt[0].backcolor;
        gt[0].backcolor = temp;
      }
      else if ( transform_colors_flag && (transform_colors[i] != SAME_COLOR) ) 
        gt[0].color = gt[0].backcolor = transform_colors[i];
      if ( box_flag )
      for ( j = 0 ; j < FACET_VERTS ; j++ )
        for ( k = 0 ; k < SDIM ; k++ )
        { if ( gt[j].x[k] < bounding_box[k][0] )
            bounding_box[k][0] = gt[j].x[k];
          if ( gt[j].x[k] > bounding_box[k][1] )
            bounding_box[k][1] = gt[j].x[k];
        }
      if ( (torus_display_mode == TORUS_CLIPPED_MODE) &&
            web.torus_display_period && !web.torus_flag )
        torus_clip(gt,SDIM-1,f_id);
      else if ( slice_view_flag )
        slice_facet(gt);
      else if ( clip_view_flag )
        clip_facet(gt,f_id,0);
      else
        (*graph_facet)(gt,f_id);
    }
  }
  else 
  { if ( (torus_display_mode == TORUS_CLIPPED_MODE) &&
            web.torus_display_period && !web.torus_flag )
      torus_clip(gdata,SDIM-1,f_id);
    else if ( slice_view_flag )
        slice_facet(gdata);
    else if ( clip_view_flag )
        clip_facet(gdata,f_id,0);
    else
      (*graph_facet)(gdata,f_id);
  }
}

/*****************************************************************
*
*  function: plain_edges()
*
*  purpose:  plots all edges one by one.
*
*/

void plain_edges()
{ 
  int i,k;
  edge_id e_id;
  REAL **verts; /* for adjusted triangle vertices */
  struct graphdata *gdata;
  int ctrlpts = web.skel[EDGE].ctrlpts;
  int segs = smooth_graph_flag ? 8 : ctrlpts-1;  /* for smooth plotting */
  int to_alloc =  ctrlpts > (segs+1) ? ctrlpts : (segs+1);
  gdata = (struct graphdata *)mycalloc(to_alloc,sizeof(struct graphdata));

  verts = temp_dmatrix(0,to_alloc,0,SDIM);
  gdata[0].etype = REGULAR_EDGE;
  MFOR_ALL_EDGES(e_id)
  { if ( breakflag ) break;

    #ifdef MPI_EVOLVER
    if ( !mpi_show_corona_flag && (id_task(e_id) != this_task) )
       continue;
    #endif

    if ( show_expr[EDGE] && show_expr[EDGE]->start )
      if ( !eval(show_expr[EDGE],NULL,e_id,NULL) )
        continue;

    gdata[0].flags = 0;

    /* get vertices */
    get_edge_verts(e_id,verts,NULL);
    for ( i = 0 ; i < SDIM ; i++ )
    { gdata[1].x[i] = verts[1][i];
      gdata[0].x[i] = verts[0][i];
      gdata[0].v_id = get_edge_tailv(e_id);
      gdata[1].v_id = get_edge_headv(e_id);
      if ( web.modeltype == QUADRATIC )
      { gdata[2].x[i] = verts[2][i];
        gdata[2].v_id = get_edge_midv(e_id);
      }
      else if ( web.modeltype == LAGRANGE )
      { vertex_id *v = get_edge_vertices(e_id);
        for ( k = 0 ; k < ctrlpts ; k++ )
        { gdata[k].x[i] = verts[k][i];
          gdata[k].v_id = v[k];
        }
      }
    }
    gdata[0].color = gdata[0].ecolor = get_edge_color_2(e_id);
    gdata[0].id = e_id;
    if ( inverted(e_id) ) gdata[i].etype |= LABEL_REVERSED;

    /* call device-specific routine */
    if ( web.modeltype == LINEAR )
    { gdata[0].flags |= LABEL_EDGE|LABEL_HEAD|LABEL_TAIL;
      if ( web.torus_flag && (torus_display_mode == TORUS_CLIPPED_MODE) ) 
        torus_edge_clip(gdata,0);
      else if ( spherical_arc_flag )
      { if ( graph_capabilities & GS_ARCS )
        { gdata[0].flags |= SPHERE_ARC;
          (*graph_edge_transforms)(gdata,e_id);
        }
        else  /* do by hand */
        { REAL rr = SDIM_dot(gdata[0].x,gdata[0].x);
          REAL base[MAXCOORD];
          REAL perp[MAXCOORD];
          REAL ab = SDIM_dot(gdata[0].x,gdata[1].x);
          REAL angle,dangle;
          int n;
          if ( ab >= rr )
          { sprintf(errmsg,"Edge %s endpoints are antipodal; cannot be graphed as spherical arc.\n",
              ELNAME(e_id));
            kb_error(3733,errmsg,WARNING);
            return;
          }
          for ( i = 0 ; i < SDIM ; i++ ) 
          { base[i] = gdata[0].x[i];
            perp[i] = 1/sqrt(1-ab*ab/rr/rr)*(gdata[1].x[i] - ab/rr*gdata[0].x[i]);
          }
          angle = acos(ab/rr);
          dangle = M_PI/40;
          segs = (int)ceil(angle/dangle);
          dangle = angle/segs;
          for ( n = 0 ; n < segs ; n++ )
          { if ( n > 0 ) 
              for ( i = 0 ; i < SDIM ; i++ )
                gdata[0].x[i] = gdata[1].x[i];
            for ( i = 0 ; i < SDIM ; i++ )
                gdata[1].x[i] = cos((n+1)*dangle)*base[i] + sin((n+1)*dangle)*perp[i];
            (*graph_edge_transforms)(gdata,e_id);
          }
        }  
      }
      else
        (*graph_edge_transforms)(gdata,e_id);
    }
    else if ( (web.modeltype == QUADRATIC) && circular_arc_flag ) 
    { /* arc, so plot in segments, unless have circular arc mode */
      REAL headx[MAXCOORD];
      REAL tailx[MAXCOORD];
      REAL midx[MAXCOORD];
      REAL w1[MAXCOORD],mag1;
      REAL w2[MAXCOORD],mag2;
      REAL w[MAXCOORD],mag;
      REAL xx[MAXCOORD];
      for (i = 0 ; i < SDIM ; i++ ) 
      { headx[i] = gdata[2].x[i]; 
        tailx[i] = gdata[0].x[i];
        midx[i] = gdata[1].x[i];
        w1[i] = midx[i] - tailx[i];
        w2[i] = headx[i] - tailx[i];
      }
      mag1 = SDIM_dot(w1,w1); mag2 = SDIM_dot(w2,w2);
      if ( graph_capabilities & GC_ARCS )
      { gdata[0].flags |= EDGE_ARC;
        if ( web.torus_flag && (torus_display_mode == TORUS_CLIPPED_MODE)) 
           torus_edge_clip(gdata,0);
        else (*graph_edge_transforms)(gdata,e_id);
      }
      else
      { int segments;
        REAL ang;
        
        for ( i = 0 ; i < SDIM  ; i++ )
        { w1[i] /= mag1; w2[i] /= mag2; }
        for ( i = 0 ; i < SDIM ; i++ ) gdata[0].x[i] = tailx[i];

        /* figure out how many segments to do */
        ang = 4*acos(SDIM_dot(w1,w2)*sqrt(mag1*mag2));
        segments = (int)(ang/(M_PI/180*string_curve_tolerance));
        if ( segments < 2 ) 
            segments = 2;

        gdata[1].v_id = NULLID;
        for ( k = 1 ; k <= segments ; k++ )
        { if ( k == segments/2 )
            gdata[0].v_id = get_edge_midv(e_id);
          else if ( k > 1 )
            gdata[0].v_id = NULLID;
          if ( k == segments )
            gdata[1].v_id = get_edge_headv(e_id);

          for ( i = 0 ; i < SDIM ; i++ )
             w[i] = w2[i] + (segments-k)/(REAL)k*(w1[i]-w2[i]);
          mag = SDIM_dot(w,w);
          for ( i = 0 ; i < SDIM ; i++ )
            gdata[1].x[i] = xx[i] = tailx[i] + w[i]/mag;
          
          if ( labelflag )
          { gdata[0].flags &= ~(LABEL_EDGE|LABEL_HEAD|LABEL_TAIL);
            if ( k == 1 ) gdata[0].flags |= LABEL_TAIL;
            if ( k == segments/2 ) gdata[0].flags |= LABEL_EDGE;
            if ( k == segments ) gdata[0].flags |= LABEL_HEAD;
          }
          if ( web.torus_flag && (torus_display_mode == TORUS_CLIPPED_MODE)) 
             torus_edge_clip(gdata,0);
          else (*graph_edge_transforms)(gdata,e_id);
          for ( i = 0 ; i < SDIM ; i++ )  /* gdata[1].x was messed up by clip */
             gdata[0].x[i] = xx[i];
        }
      }
    }
    else if ( web.modeltype == QUADRATIC ) /* quadratic, so plot in segments */
      { REAL headx[MAXCOORD];
        REAL tailx[MAXCOORD];
        REAL midx[MAXCOORD];
        REAL w1[MAXCOORD],w2[MAXCOORD],mag1,mag2,ang;
        
        for ( i = 0 ; i < SDIM ; i++ ) 
        { headx[i] = gdata[2].x[i]; tailx[i] = gdata[0].x[i];
          midx[i] = gdata[1].x[i];
          w1[i] = midx[i] - tailx[i];
          w2[i] = headx[i] - tailx[i];
        }
        /* figure out how many segments to do */
        mag1 = SDIM_dot(w1,w1); mag2 = SDIM_dot(w2,w2);
        ang = 4*acos(SDIM_dot(w1,w2)/sqrt(mag1*mag2));
        segs = (int)(ang/(M_PI/180*string_curve_tolerance));
        if ( segs < 2 ) 
            segs = 2;
        for ( k = 1 ; k <= segs ; k++ )
        { for ( i = 0 ; i < SDIM ; i++ )
            gdata[1].x[i] = (1-k/(REAL)segs)*(1-k*2./segs)*tailx[i]
                                    + k*4./segs*(1-k/(REAL)segs)*midx[i]
                                    - k/(REAL)segs*(1-k*2./segs)*headx[i];
          if ( labelflag )
          { gdata[0].flags &= ~(LABEL_EDGE|LABEL_HEAD|LABEL_TAIL);
            if ( k == 1 ) gdata[0].flags |= LABEL_TAIL;
            if ( k == segs/2+1 ) gdata[0].flags |= LABEL_EDGE;
            if ( k == segs ) gdata[0].flags |= LABEL_HEAD;
          }
          if ( web.torus_flag && (torus_display_mode == TORUS_CLIPPED_MODE)) 
            torus_edge_clip(gdata,0);
          else (*graph_edge_transforms)(gdata,e_id);
          for ( i = 0 ; i < SDIM ; i++ )  /* gdata[1].x was messed up by clip */
             gdata[0].x[i] = (1-k/(REAL)segs)*(1-k*2./segs)*tailx[i]
                                   + k*4./segs*(1-k/(REAL)segs)*midx[i]
                                   - k/(REAL)segs*(1-k*2./segs)*headx[i];
        }
     }
     else if ( web.modeltype == LAGRANGE )
     { /* plot subsegments */
       if ( smooth_graph_flag || bezier_flag )
       { /* plot lots of segments */
         vertex_id headv = gdata[web.lagrange_order].v_id;
         for ( k = 0 ; k < segs ; k++ )
         { gdata[k].ecolor = gdata[0].ecolor;
           gdata[k].etype = gdata[0].etype;
           /* were messed up by clip of previous segment */
           if ( bezier_flag )
           { bezier_eval_1d(web.lagrange_order,SDIM,k/(REAL)segs,verts,
               gdata[k].x);
             bezier_eval_1d(web.lagrange_order,SDIM,(k+1.)/segs,verts,
               gdata[k+1].x);
           }
           else
           { /* regular lagrange */
             lagrange_eval_1d(web.lagrange_order,SDIM,k/(REAL)segs,verts,
               gdata[k].x);
             lagrange_eval_1d(web.lagrange_order,SDIM,(k+1.)/segs,verts,
               gdata[k+1].x);
           }
           gdata[k].flags &= ~(LABEL_EDGE|LABEL_HEAD|LABEL_TAIL);
           if ( k == 0 ) gdata[0].flags |= LABEL_TAIL;
           if ( k == segs/2 ) gdata[k].flags |= LABEL_EDGE;
           if ( k == segs-1 )
           { gdata[k].flags |= LABEL_HEAD;
             gdata[k+1].v_id = headv;
           }
           else gdata[k+1].v_id = NULLID;
           if ( web.torus_flag && (torus_display_mode == TORUS_CLIPPED_MODE) ) 
                torus_edge_clip(gdata+k,0);
           else (*graph_edge_transforms)(gdata+k,e_id);
         }
       }
       else  /* just plot between control points */
       { for ( k = 0 ; k < ctrlpts-1 ; k++ )
         { gdata[k].ecolor = gdata[0].ecolor;
           gdata[k].etype = gdata[0].etype;
           for ( i = 0 ; i < SDIM ; i++ )  /* were messed up by clip */
           { gdata[k].x[i] = verts[k][i];
             gdata[k+1].x[i] = verts[k+1][i];
           }
           gdata[k].flags &= ~(LABEL_EDGE|LABEL_HEAD|LABEL_TAIL);
           if ( k == 0 ) gdata[0].flags |= LABEL_TAIL;
           if ( k == (ctrlpts-1)/2 ) gdata[k].flags |= LABEL_EDGE;
           if ( k == ctrlpts-2 ) gdata[k].flags |= LABEL_HEAD;
           if ( web.torus_flag && (torus_display_mode == TORUS_CLIPPED_MODE) ) 
                torus_edge_clip(gdata+k,0);
           else (*graph_edge_transforms)(gdata+k,e_id);
         }
       }
     }
   }
  free_temp_matrix(verts);
  myfree((char*)gdata);

}  /* end plain_edges() */


/*****************************************************************
*
*  function: bare_edges()
*
*  purpose:  plots all facetless edges.
*
*/

void bare_edges()
{
  int i;
  edge_id e_id;
  REAL *verts[3]; 
  struct graphdata gdata[3];     
  facetedge_id fe;

  memset((char*)gdata,0,3*sizeof(struct graphdata));
  gdata[0].etype = BARE_EDGE;
  for ( i = 0 ; i < 2 ; i++ ) verts[i] = gdata[i].x;
  MFOR_ALL_EDGES(e_id)
     { 
        if ( breakflag ) break;

        #ifdef MPI_EVOLVER
        if ( !mpi_show_corona_flag && (id_task(e_id) != this_task) )
           continue;
        #endif

        gdata[0].id = e_id;
        fe = get_edge_fe(e_id);
        if ( valid_id(fe) && valid_id(get_fe_facet(fe)) ) continue;
        if ( show_expr[EDGE] && show_expr[EDGE]->start )
            if ( !eval(show_expr[EDGE],NULL,e_id,NULL) ) continue;

        /* get vertices */
        gdata[0].v_id = get_edge_tailv(e_id);
        gdata[1].v_id = get_edge_headv(e_id);
        verts[0] = get_coord(get_edge_tailv(e_id));
        verts[1] = get_coord(get_edge_headv(e_id));
        for ( i = 0 ; i < SDIM ; i++ )
        { gdata[0].x[i] = verts[0][i];
        }
        gdata[0].color = gdata[0].ecolor = get_edge_color_2(e_id);
        if ( web.symmetry_flag )
          (*sym_wrap)(verts[1],gdata[1].x,get_edge_wrap(e_id));
        else for ( i = 0 ; i < SDIM ; i++ )
          gdata[1].x[i] = verts[1][i];

        /* call device-specific routine */
        gdata[0].flags = (LABEL_EDGE|LABEL_HEAD|LABEL_TAIL);
        if ( web.torus_flag && (torus_display_mode == TORUS_CLIPPED_MODE) ) 
            torus_edge_clip(gdata,0);
        else (*graph_edge_transforms)(gdata,e_id);
     }
}


/*****************************************************************
*
*  function: triple_edges()
*
*  purpose:  plots all multiple-facet edges if triple_show_flag set.
*
*/

void triple_edges()
{
  int i;
  edge_id e_id;
  facetedge_id fe_id;
  REAL *verts[3]; /* for adjusted triangle vertices */
  struct graphdata gdata[3];

  memset(gdata,0,3*sizeof(struct graphdata));

  gdata[0].etype = TRIPLE_EDGE;
  for ( i = 0 ; i < 2 ; i++ ) verts[i] = gdata[i].x;
  MFOR_ALL_EDGES(e_id)
     { 
        if ( breakflag ) break;

        #ifdef MPI_EVOLVER
        if ( !mpi_show_corona_flag && (id_task(e_id) != this_task) )
           continue;
        #endif

        fe_id = get_edge_fe(e_id);
        if ( equal_id(get_next_facet(fe_id),get_prev_facet(fe_id)) ) continue;
        if ( show_expr[EDGE] && show_expr[EDGE]->start )
            if ( !eval(show_expr[EDGE],NULL,e_id,NULL) ) continue;

        /* get vertices */
        verts[0] = get_coord(get_edge_tailv(e_id));
        verts[1] = get_coord(get_edge_headv(e_id));
        verts[2] = get_coord(get_edge_midv(e_id));
        for ( i = 0 ; i < SDIM ; i++ )
        { gdata[0].x[i] = verts[0][i];
          gdata[2].x[i] = verts[2][i];
        }
        if ( web.symmetry_flag )
          (*sym_wrap)(verts[1],gdata[1].x,get_edge_wrap(e_id));
        else
          for ( i = 0 ; i < SDIM ; i++ )
             gdata[1].x[i] = verts[1][i];
        gdata[0].id = e_id;
        gdata[0].color = gdata[0].ecolor = get_edge_color_2(e_id);

        /* call device-specific routine */
        gdata[0].flags |= LABEL_EDGE|LABEL_HEAD|LABEL_TAIL;
        if ( web.torus_flag && (torus_display_mode == TORUS_CLIPPED_MODE) ) 
            torus_edge_clip(gdata,0);
        else (*graph_edge_transforms)(gdata,e_id);
     }
}

/**********************************************************************
* 
*  Function: torus_edge_clip()
*
*  Purpose:  Recursive routine to subdivide edges crossing 
*                torus cell faces.  Edges have been already 
*                unwrapped by torus periods.
*/

void torus_edge_clip(gdata,m)
struct graphdata *gdata;  /* edge to check */
int m;                    /* coordinate number */
{
  struct graphdata gdata1[2];  /* for possible subdivided edge */
  int i,k;
  REAL t,a,b;
  int wrap[2];
  REAL **per=NULL;
  REAL **invper=NULL;
  REAL orig[MAXCOORD];  /* cell coords of display origin */

  if ( gdata[0].flags & EDGE_ARC ) 
  { torus_arc_clip(gdata,m);
    return;
  }

  memset(gdata1,0,2*sizeof(struct graphdata));

  if ( web.torus_display_period && web.torus_period )
  { /* per = web.torus_display_period; */
    per = web.torus_period;
    invper = web.inverse_display_periods;
  }
  else if ( web.torus_period )
  { per = web.torus_period;
    invper = web.inverse_periods;
  }
  else if ( web.torus_display_period )
  { per = web.torus_display_period; 
    invper = web.inverse_display_periods;
  }
  else 
    graph_edge_clip(gdata,gdata->id);

  for ( i = 0 ; i < SDIM ; i++ )
    orig[i] = SDIM_dot(invper[i],web.display_origin);
   
  /* see if any vertices outside cell in this coordinate */
  for ( i = 0 ; i < 2 ; i++ )
     wrap[i] = (int)floor(SDIM_dot(invper[m],gdata[i].x)-orig[m]);

  /* split, if necessary */
  if ( wrap[0] != wrap[1] )
  { int cut = (wrap[0] > wrap[1]) ? wrap[0] : wrap[1];

    /* set up head of new edge */
    gdata1[1] = gdata[1];
    gdata1[0].id = gdata[0].id;
    gdata1[0].etype = gdata[0].etype;
    gdata1[0].flags = gdata[0].flags & ~LABEL_TAIL;
    gdata[0].flags &= ~LABEL_HEAD;
    gdata[1].v_id = NULLID;
    gdata1[0].v_id = NULLID;

    /* calculate new vertex */
    a = SDIM_dot(invper[m],gdata[0].x)-orig[m];
    b = SDIM_dot(invper[m],gdata1[1].x)-orig[m];
    t = (cut - a)/(b - a);
    if ( t < 0.0 ) t = 0.0;   /* clamp */
    else if ( t > 1.0 ) t = 1.0;
    for ( k = 0 ; k < SDIM ; k++ )
      gdata1[0].x[k] = gdata[1].x[k] =
          (1 - t)*gdata[0].x[k] + t*gdata1[1].x[k];

    /* wrap new edge vertices properly */
    for ( i = 0 ; i < 2 ; i++ )
     for ( k = 0 ; k < SDIM ; k++ )
         gdata1[i].x[k] -= wrap[1]*per[m][k];

    /* kludge for display_periods to get back towards box */
    /*
    if ( web.torus_display_period )
    { int ww = (int)floor((SDIM_dot(invper[1-m],gdata1[0].x) + 
                         SDIM_dot(invper[1-m],gdata1[1].x))/2 - orig[1-m]);
      for ( i = 0 ; i < 2 ; i++ )
       for ( k = 0 ; k < SDIM ; k++ )
         gdata1[i].x[k] -= ww*per[1-m][k];
    }
    */

    /* send on for further check, or plot */
    gdata1[0].color = gdata1[0].ecolor = gdata[0].color;
    if ( m == SDIM-1 ) 
    { if ( web.torus_flag && (torus_display_mode == TORUS_CLIPPED_MODE) )
        (*graph_edge_transforms)(gdata1,gdata[0].id);
      else
        graph_edge_clip(gdata1,gdata[0].id);
    }
    else
      torus_edge_clip(gdata1,m+1);
  }

  /* wrap vertices properly */
  for ( i = 0 ; i < 2 ; i++ )
    for ( k = 0 ; k < SDIM ; k++ )
       gdata[i].x[k] -=  wrap[0]*per[m][k];

  /* kludge for display_periods to get back towards box */
  /*
    if ( web.torus_display_period )
    { int ww = (int)floor((SDIM_dot(invper[1-m],gdata[0].x) +
                 SDIM_dot(invper[1-m],gdata[1].x))/2-orig[1-m]);
      for ( i = 0 ; i < 2 ; i++ )
       for ( k = 0 ; k < SDIM ; k++ )
         gdata[i].x[k] -= ww*per[1-m][k];
    }
    */

  /* send on original edge structure */
  if ( m == SDIM-1 ) 
  { if ( web.torus_flag && (torus_display_mode == TORUS_CLIPPED_MODE) )
      (*graph_edge_transforms)(gdata,gdata[0].id);
    else
      graph_edge_clip(gdata,gdata->id);
  }
  else
    torus_edge_clip(gdata,m+1);

} /* end torus_edge_clip() */


/**********************************************************************
* 
*  Function: torus_arc_clip()
*
*  Purpose:  Recursive routine to subdivide circular arc edges crossing 
*                torus cell faces.  Edges have been already 
*                unwrapped by torus periods.
*/

void torus_arc_clip(gdata,m)
struct graphdata *gdata;  /* edge to check */
int m;                    /* coordinate number */
{
  struct graphdata gdata1[3];  /* for possible subdivided edge */
  struct graphdata gdata2[3];  /* for possible subdivided edge */
  int i,j;
  REAL a,b,c,h,k,u,v,x0,x1,x2,y0,y1,y2,t1,t2,discr;
  int wrap[3];
  REAL **per;
  REAL **invper;
  REAL w1[MAXCOORD],w2[MAXCOORD],mag1,mag2,w1w2,center[2],radius;
  REAL angle1,angle2,det,theta1,theta2;

  if ( web.torus_display_period )
  { per = web.torus_display_period;
    invper = web.inverse_display_periods;
  }
  else
  { per = web.torus_period;
    invper = web.inverse_periods;
  }

  for ( i = 0 ; i < 3 ; i++ )
    gdata1[i] = gdata2[i] = gdata[i]; /* flags and stuff */

  /* see where vertices are relative to cell in this coordinate */
  for ( i = 0 ; i < 3 ; i++ )
     wrap[i] = (int)floor(SDIM_dot(invper[m],gdata[i].x));

  /* solve for geometric parameters */
  for (i = 0 ; i < SDIM ; i++ )
  { w1[i] = gdata[1].x[i] - gdata[0].x[i];
    w2[i] = gdata[2].x[i] - gdata[0].x[i];
  }
  det = w1[0]*w2[1] - w1[1]*w2[0];
  mag1 = SDIM_dot(w1,w1); mag2 = SDIM_dot(w2,w2);
  w1w2 = w1[0]*w2[0] + w1[1]*w2[1];
  if ( 4000*det*det < mag1*mag1*mag2 + mag1*mag2*mag2 - 2*mag1*w1w2*mag2 )
  { /* practically straight line */
    for ( i = 0 ; i < SDIM ; i++ )
      gdata[1].x[i] = gdata[2].x[i];
    gdata[0].flags &= ~EDGE_ARC;
    torus_edge_clip(gdata,m);
    return;
  }
  /* circle */
  center[0] = gdata[0].x[0] + 0.5*(w2[1]*mag1 - w1[1]*mag2)/det;
  center[1] = gdata[0].x[1] + 0.5*(-w2[0]*mag1 + w1[0]*mag2)/det;
  radius =  sqrt((mag1*mag1*mag2+mag1*mag2*mag2-2*mag1*w1w2*mag2)/4/det/det);
  angle1 = atan2(gdata[0].x[1]-center[1],gdata[0].x[0]-center[0]);
  angle2 = atan2(gdata[2].x[1]-center[1],gdata[2].x[0]-center[0]);
  if ( det < 0.0 ) /* swap to get counterclockwise order around circle */
  { REAL temp = angle1; angle1 = angle2; angle2 = temp; 
    wrap[0] = wrap[2]; /* only one we use */
    temp = gdata[0].x[0]; gdata[0].x[0] = gdata[2].x[0]; gdata[2].x[0] = temp;
    temp = gdata[0].x[1]; gdata[0].x[1] = gdata[2].x[1]; gdata[2].x[1] = temp;
  }
  if ( angle2 < angle1 )
    angle2 += 2*M_PI;

  /* shrink arc a bit to avoid numerical problems */
  angle1 += 1e-10;
  angle2 -= 1e-10;
  wrap[0] = (int)floor(invper[m][0]*(center[0]+radius*cos(angle1))
              + invper[m][1]*(center[1]+radius*sin(angle1)));
  
  /* intersect with line for low side */
  h = center[0]; k = center[1]; 
  u = per[1-m][0]; v = per[1-m][1];
  x0 = 0.0; y0 = 0.0;
  a = u*u + v*v;
  b = 2*(u*(x0-h) + v*(y0-k));
  c = (x0-h)*(x0-h) + (y0-k)*(y0-k) - radius*radius;
  discr = b*b - 4*a*c;
  if ( discr > 0.0 )
  { /* potential intersection */
    t1 = b > 0.0 ? -2*c/(b + sqrt(discr)) : (-b + sqrt(discr))/2/a;
    t2 = b > 0.0 ? (-b - sqrt(discr))/2/a : -2*c/(b - sqrt(discr));
    x1 = x0 + t1*u; y1 = y0 + t1*v;
    x2 = x0 + t2*u; y2 = y0 + t2*v;
    theta1 = atan2(y1-center[1],x1-center[0]);
    theta2 = atan2(y2-center[1],x2-center[0]);
    if ( theta1 < angle1 )  theta1 += 2*M_PI;
    if ( theta2 < angle1 )  theta2 += 2*M_PI;
    if ( theta1 > theta2 )
    { REAL temp = theta1; theta1 = theta2; theta2 = temp;
      temp = x1; x1 = x2; x2 = temp;
      temp = y1; y1 = y2; y2 = temp;
    }
    if ( theta1 >= angle2 )
    { /* no intersection, but may have to wrap */
      if ( wrap[0] < 0 )
       for ( i = 0 ; i < 3 ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
         gdata[i].x[j] += per[m][j];
    }
    else /* split in at least two */
    { if ( theta2 > angle2 )
      { /* split in half, top half in gdata in case of further split on top */
        if ( wrap[0] >= 0 )
        { gdata1[0].x[0] = x1;
          gdata1[0].x[1] = y1;
          gdata1[0].flags |= EDGE_ARC;
          gdata1[1].x[0] = center[0] + radius*cos((angle2+theta1)/2);
          gdata1[1].x[1] = center[1] + radius*sin((angle2+theta1)/2);
          gdata1[2] = gdata[2];
  
          gdata[1].x[0] = center[0] + radius*cos((angle1+theta1)/2);
          gdata[1].x[1] = center[1] + radius*sin((angle1+theta1)/2);
          gdata[2].x[0] = x1;
          gdata[2].x[1] = y1;
          for ( i = 0 ; i < 3 ; i++ )
           for ( j = 0 ; j < SDIM ; j++ )
            gdata1[i].x[j] += per[m][j];
        }
        else
        { gdata1[0] = gdata[0];
          gdata1[1].x[0] = center[0] + radius*cos((angle1+theta1)/2);
          gdata1[1].x[1] = center[1] + radius*sin((angle1+theta1)/2);
          gdata1[2].x[0] = x1;
          gdata1[2].x[1] = y1;
  
          gdata[0].x[0] = x1;
          gdata[0].x[1] = y1;
          gdata[1].x[0] = center[0] + radius*cos((angle2+theta1)/2);
          gdata[1].x[1] = center[1] + radius*sin((angle2+theta1)/2);
          for ( i = 0 ; i < 3 ; i++ )
           for ( j = 0 ; j < SDIM ; j++ )
            gdata1[i].x[j] += per[m][j];
        }
      }
      else /* split in three */
      { if ( wrap[0] >= 0 )
        {
          gdata2[0].x[0] = x2;
          gdata2[0].x[1] = y2;
          gdata2[1].x[0] = center[0] + radius*cos((angle2+theta2)/2);
          gdata2[1].x[1] = center[1] + radius*sin((angle2+theta2)/2);
          gdata2[2] = gdata[2];
          gdata2[0].flags |= EDGE_ARC;
  
          gdata1[0].flags |= EDGE_ARC;
          gdata1[0].x[0] = x2;
          gdata1[0].x[1] = y2;
          gdata1[1].x[0] = center[0] + radius*cos((theta2+theta1)/2);
          gdata1[1].x[1] = center[1] + radius*sin((theta2+theta1)/2);
          gdata1[2].x[0] = x1;
          gdata1[2].x[1] = y1;
  
          gdata[1].x[0] = center[0] + radius*cos((angle1+theta1)/2);
          gdata[1].x[1] = center[1] + radius*sin((angle1+theta1)/2);
          gdata[2].x[0] = x1;
          gdata[2].x[1] = y1;
          for ( i = 0 ; i < 3 ; i++ )
           for ( j = 0 ; j < SDIM ; j++ )
            gdata1[i].x[j] += per[m][j];
        }
        else
        {
          gdata2[0].x[0] = x2;
          gdata2[0].x[1] = y2;
          gdata2[1].x[0] = center[0] + radius*cos((angle2+theta2)/2);
          gdata2[1].x[1] = center[1] + radius*sin((angle2+theta2)/2);
          gdata2[2] = gdata[2];
          gdata2[0].flags |= EDGE_ARC;
  
          gdata1[0] = gdata[0];
          gdata1[1].x[0] = center[0] + radius*cos((angle1+theta1)/2);
          gdata1[1].x[1] = center[1] + radius*sin((angle1+theta1)/2);
          gdata1[2].x[0] = x1;
          gdata1[2].x[1] = y1;
  
          gdata[0].x[0] = x2;
          gdata[0].x[1] = y2;
          gdata[1].x[0] = center[0] + radius*cos((theta2+theta1)/2);
          gdata[1].x[1] = center[1] + radius*sin((theta2+theta1)/2);
          gdata[2].x[0] = x1;
          gdata[2].x[1] = y1;
  
          for ( i = 0 ; i < 3 ; i++ )
           for ( j = 0 ; j < SDIM ; j++ )
           { gdata1[i].x[j] += per[m][j];
             gdata2[i].x[j] += per[m][j];
           }
        }
        /* send on for further check, or plot */
        if ( m == SDIM-1 ) 
          (*graph_edge_transforms)(gdata2,gdata[0].id);
        else
          torus_edge_clip(gdata2,m+1);
      }
      /* send on for further check, or plot */
      if ( m == SDIM-1 ) 
        (*graph_edge_transforms)(gdata1,gdata[0].id);
      else
        torus_edge_clip(gdata1,m+1);
    }
  }

  /* intersect with line for high side */
  h = center[0]; k = center[1]; 
  u = per[1-m][0]; v = per[1-m][1];
  x0 = per[m][0]; y0 = per[m][1];
  a = u*u + v*v;
  b = 2*(u*(x0-h) + v*(y0-k));
  c = (x0-h)*(x0-h) + (y0-k)*(y0-k) - radius*radius;
  discr = b*b - 4*a*c;
  if ( discr > 0.0 )
  { /* potential intersection */
    t1 = b > 0.0 ? -2*c/(b + sqrt(discr)) : (-b + sqrt(discr))/2/a;
    t2 = b > 0.0 ? (-b - sqrt(discr))/2/a : -2*c/(b - sqrt(discr));
    x1 = x0 + t1*u; y1 = y0 + t1*v;
    x2 = x0 + t2*u; y2 = y0 + t2*v;
    theta1 = atan2(y1-center[1],x1-center[0]) - 0.000000001;
    theta2 = atan2(y2-center[1],x2-center[0]);
    if ( theta1 < angle1 )  theta1 += 2*M_PI;
    if ( theta2 < angle1 )  theta2 += 2*M_PI;
    if ( theta1 > theta2 )
    { REAL temp = theta1; theta1 = theta2; theta2 = temp;
      temp = x1; x1 = x2; x2 = temp;
      temp = y1; y1 = y2; y2 = temp;
    }
    if ( theta1 > angle2 )
    { /* no intersection, but may have to wrap */
      if ( wrap[0] > 0 )
       for ( i = 0 ; i < 3 ; i++ )
        for ( j = 0 ; j < SDIM ; j++ )
         gdata[i].x[j] -= per[m][j];
    }
    else
    { /* split in at least two */
      if ( theta2 > angle2 )
      { /* split in half, bottom half in gdata */
        if ( wrap[0] <= 0 )
        { gdata1[0].x[0] = x1;
          gdata1[0].x[1] = y1;
          gdata1[0].flags |= EDGE_ARC;
          gdata1[1].x[0] = center[0] + radius*cos((angle2+theta1)/2);
          gdata1[1].x[1] = center[1] + radius*sin((angle2+theta1)/2);
          gdata1[2] = gdata[2];
  
          gdata[1].x[0] = center[0] + radius*cos((angle1+theta1)/2);
          gdata[1].x[1] = center[1] + radius*sin((angle1+theta1)/2);
          gdata[2].x[0] = x1;
          gdata[2].x[1] = y1;
          for ( i = 0 ; i < 3 ; i++ )
           for ( j = 0 ; j < SDIM ; j++ )
            gdata1[i].x[j] -= per[m][j];
        }
        else
        { gdata1[0] = gdata[0];
          gdata1[1].x[0] = center[0] + radius*cos((angle1+theta1)/2);
          gdata1[1].x[1] = center[1] + radius*sin((angle1+theta1)/2);
          gdata1[2].x[0] = x1;
          gdata1[2].x[1] = y1;
  
          gdata[0].x[0] = x1;
          gdata[0].x[1] = y1;
          gdata[1].x[0] = center[0] + radius*cos((angle2+theta1)/2);
          gdata[1].x[1] = center[1] + radius*sin((angle2+theta1)/2);
          for ( i = 0 ; i < 3 ; i++ )
           for ( j = 0 ; j < SDIM ; j++ )
            gdata1[i].x[j] -= per[m][j];
        }
      }
      else /* split in three */
      { if ( wrap[0] <= 0 )
        {
          gdata2[0].x[0] = x2;
          gdata2[0].x[1] = y2;
          gdata2[1].x[0] = center[0] + radius*cos((angle2+theta2)/2);
          gdata2[1].x[1] = center[1] + radius*sin((angle2+theta2)/2);
          gdata2[2] = gdata[2];
          gdata2[0].flags |= EDGE_ARC;
  
          gdata1[0].flags |= EDGE_ARC;
          gdata1[0].x[0] = x2;
          gdata1[0].x[1] = y2;
          gdata1[1].x[0] = center[0] + radius*cos((theta2+theta1)/2);
          gdata1[1].x[1] = center[1] + radius*sin((theta2+theta1)/2);
          gdata1[2].x[0] = x1;
          gdata1[2].x[1] = y1;
  
          gdata[1].x[0] = center[0] + radius*cos((angle1+theta1)/2);
          gdata[1].x[1] = center[1] + radius*sin((angle1+theta1)/2);
          gdata[2].x[0] = x1;
          gdata[2].x[1] = y1;
          for ( i = 0 ; i < 3 ; i++ )
           for ( j = 0 ; j < SDIM ; j++ )
            gdata1[i].x[j] -= per[m][j];
        }
        else
        {
          gdata2[0].x[0] = x2;
          gdata2[0].x[1] = y2;
          gdata2[1].x[0] = center[0] + radius*cos((angle2+theta2)/2);
          gdata2[1].x[1] = center[1] + radius*sin((angle2+theta2)/2);
          gdata2[2] = gdata[2];
          gdata2[0].flags |= EDGE_ARC;
  
          gdata1[0] = gdata[0];
          gdata1[1].x[0] = center[0] + radius*cos((angle1+theta1)/2);
          gdata1[1].x[1] = center[1] + radius*sin((angle1+theta1)/2);
          gdata1[2].x[0] = x1;
          gdata1[2].x[1] = y1;
  
          gdata[0].x[0] = x2;
          gdata[0].x[1] = y2;
          gdata[1].x[0] = center[0] + radius*cos((theta2+theta1)/2);
          gdata[1].x[1] = center[1] + radius*sin((theta2+theta1)/2);
          gdata[2].x[0] = x1;
          gdata[2].x[1] = y1;
  
          for ( i = 0 ; i < 3 ; i++ )
           for ( j = 0 ; j < SDIM ; j++ )
           { gdata1[i].x[j] -= per[m][j];
             gdata2[i].x[j] -= per[m][j];
           }
        }
        /* send on for further check, or plot */
        if ( m == SDIM-1 ) 
          (*graph_edge_transforms)(gdata2,gdata[0].id);
        else
          torus_edge_clip(gdata2,m+1);
      }
      /* send on for further check, or plot */
      if ( m == SDIM-1 ) 
        (*graph_edge_transforms)(gdata1,gdata[0].id);
      else
        torus_edge_clip(gdata1,m+1);
    }
  }

  /* send on original edge structure */
  if ( m == SDIM-1 ) 
    (*graph_edge_transforms)(gdata,gdata[0].id);
  else
    torus_edge_clip(gdata,m+1);

} /* end torus_arc_clip() */

/**********************************************************************
* 
*  Function: torus_clip()
*
*  Purpose:  Recursive routine to subdivide triangles crossing 
*            torus cell faces.  Also handles unit cell wrapping 
*            in general. 
*/

void torus_clip(gdata,m,f_id)
struct graphdata *gdata;  /* triangle to check */
int m;                    /* coordinate number */
facet_id f_id;            /* parent facet */
{
  struct graphdata gdata0[FACET_VERTS+1],gdata1[FACET_VERTS+1],
      gdata2[FACET_VERTS+1];
  int i,k,mm;
  REAL t,d,a,b;
  int wrap[FACET_VERTS];
  int cut,oddman,oddflag;
  int oddwrap;
  REAL w;
  REAL **per=NULL;
  REAL **invper=NULL;
  REAL orig[MAXCOORD]; 

  if ( web.torus_display_period && web.torus_period )
  { /* per = web.torus_display_period; */
    per = web.torus_period;
    invper = web.inverse_display_periods;
  }
  else if ( web.torus_period )
  { per = web.torus_period;
    invper = web.inverse_periods;
  }
  else if ( web.torus_display_period )
  { per = web.torus_display_period; 
    invper = web.inverse_display_periods;
  }
  else if ( slice_view_flag )
    slice_facet(gdata);
  else if ( clip_view_flag )
    clip_facet(gdata,f_id,0);
  else 
    (*graph_facet)(gdata,f_id);
   
  for ( i = 0 ; i < SDIM ; i++ )
    orig[i] = SDIM_dot(invper[i],web.display_origin);

  memset(gdata0,0,(FACET_VERTS+1)*sizeof(struct graphdata));
  memset(gdata1,0,(FACET_VERTS+1)*sizeof(struct graphdata));
  memset(gdata2,0,(FACET_VERTS+1)*sizeof(struct graphdata));

  for ( i = 0 ; i < FACET_VERTS ; i++ ) gdata0[i] = gdata[i]; /* local copy */

  /* see if any vertices outside cell in this coordinate */

 for ( i = 0 ; i < FACET_VERTS ; i++ )
  { 
    w = SDIM_dot(invper[m],gdata0[i].x) - orig[m];
    wrap[i] = (w < .5) ? ((int)floor(w+.00000001)):((int)floor(w-.00000001));
    /* slight shift to prevent spurious wraps due to rounding */
  }

  /* find odd vertex */
  oddflag = 1;  
  if ( (wrap[0] == wrap[1]) && (wrap[1] == wrap[2]) )
     { oddflag = 0;  oddman = 0; }
  else if ( wrap[0] == wrap[1] ) oddman = 2;
  else if ( wrap[1] == wrap[2] ) oddman = 0;
  else if ( wrap[2] == wrap[0] ) oddman = 1;
  else
    { sprintf(msg,"Triangle spans too many periods. Wraps %d %d %d\n",
            wrap[0],wrap[1],wrap[2]);
      erroutstring(msg);
      oddman = 0;
    }

  /* wrap new triangle vertices properly */
  oddwrap = wrap[oddman];
  for ( i = 0 ; i < FACET_VERTS ; i++ )
  { 
    wrap[i] -= oddwrap;
    for ( k = 0 ; k < SDIM ; k++ )
    { d = oddwrap*per[m][k];
      gdata0[i].x[k] -= d;
    }
  }

  /* kludge for display_periods to get back towards box */
  if ( web.torus_flag && web.torus_display_period )
    for ( mm = 0 ; mm < SDIM ; mm++ )
      if ( mm != m )
      { int ww = (int)floor((SDIM_dot(invper[mm],gdata0[0].x) + 
                         SDIM_dot(invper[mm],gdata0[1].x) +
                         SDIM_dot(invper[mm],gdata0[2].x))/FACET_VERTS
                      - orig[mm]);
        for ( i = 0 ; i < FACET_VERTS ; i++ )
         for ( k = 0 ; k < SDIM ; k++ )
           gdata0[i].x[k] -= ww*per[mm][k];
      }


  /* split, if necessary */
  if ( oddflag )
     {
        int pair1 = (oddman+1)%FACET_VERTS;
        int pair2 = (oddman+2)%FACET_EDGES;

        gdata0[0].flags = 0;  /* vertices the hard way */
        /* find wrap multiple that cuts triangle */
        if ( wrap[oddman] < wrap[pair1] )
           cut = wrap[pair1];
        else
           cut = wrap[oddman];

        /* set up new triangles */
        for ( i = 0 ; i < FACET_VERTS ; i++ )
        { gdata2[i] = gdata0[i];
          gdata1[i] = gdata0[i];
        } 

        /* calculate new vertices */
        a = SDIM_dot(invper[m],gdata0[oddman].x)-orig[m];
        b = SDIM_dot(invper[m],gdata1[pair1].x)-orig[m];
        if ( fabs(a-b) < 0.00000001 ) t = 0.5;
        else t = (cut - a)/(b - a);
        if ( t < 0.0 ) t = 0.0;   /* clamp; necessary due to wiggle room above */
        else if ( t > 1.0 ) t = 1.0;
        for ( k = 0 ; k < SDIM ; k++ )
          gdata1[oddman].x[k] = gdata0[pair1].x[k] =
              (1 - t)*gdata0[oddman].x[k] + t*gdata1[pair1].x[k];
        b = SDIM_dot(invper[m],gdata2[pair2].x)-orig[m];
        if ( fabs(a-b) < 0.00000001 ) t = 0.5;
        else t = (cut - a)/(b - a);
        if ( t < 0.0 ) t = 0.0;   /* clamp; necessary due to wiggle room above */
        else if ( t > 1.0 ) t = 1.0;
        for ( k = 0 ; k < SDIM ; k++ )
          gdata2[oddman].x[k] = gdata0[pair2].x[k] = gdata1[pair2].x[k] =
              (1 - t)*gdata0[oddman].x[k] + t*gdata2[pair2].x[k];
        gdata0[pair1].v_id = NULLID;
        gdata0[pair2].v_id = NULLID;
        gdata1[oddman].v_id = NULLID;
        gdata1[pair2].v_id = NULLID;
        gdata2[oddman].v_id = NULLID;
        /* new edge colors are clear */
        gdata0[pair1].ecolor = CLEAR;
        gdata1[pair1].ecolor = gdata1[pair2].ecolor = CLEAR;
        gdata2[oddman].ecolor = CLEAR;
        gdata0[pair1].etype = INVISIBLE_EDGE;
        gdata1[pair1].etype = gdata1[pair2].etype = INVISIBLE_EDGE;
        gdata2[oddman].etype = INVISIBLE_EDGE;

        /* wrap new triangle vertices properly */
        for ( i = 0 ; i < FACET_VERTS ; i++ )
         for ( k = 0 ; k < SDIM ; k++ )
          { d = wrap[pair1]*per[m][k];
            gdata1[i].x[k] -= d;
            gdata2[i].x[k] -= d;
          }

        /* kludge for display_periods to get back towards box */
        if ( web.torus_display_period )
          for ( mm = 0 ; mm < SDIM ; mm++ )
            if ( mm != m )
            { int ww = (int)floor((SDIM_dot(invper[mm],gdata1[0].x) + 
                         SDIM_dot(invper[mm],gdata1[1].x) +
                         SDIM_dot(invper[mm],gdata1[2].x))/FACET_VERTS
                     - orig[mm]);
              for ( i = 0 ; i < FACET_VERTS ; i++ )
               for ( k = 0 ; k < SDIM ; k++ )
                 gdata1[i].x[k] -= ww*per[mm][k];
              ww = (int)floor((SDIM_dot(invper[mm],gdata2[0].x) + 
                         SDIM_dot(invper[mm],gdata2[1].x) +
                         SDIM_dot(invper[mm],gdata2[2].x))/FACET_VERTS
                      - orig[mm]);
              for ( i = 0 ; i < FACET_VERTS ; i++ )
               for ( k = 0 ; k < SDIM ; k++ )
                 gdata2[i].x[k] -= ww*per[mm][k];
            }

 
        /* send on for further check, or plot */
        if ( m == 0 ) 
        { 
          if ( web.torus_flag && (torus_display_mode == TORUS_CLIPPED_MODE) )
          { (*graph_facet_transforms)(gdata1,f_id);
            (*graph_facet_transforms)(gdata2,f_id);
          }
          else if ( slice_view_flag )
          { slice_facet(gdata1);
            slice_facet(gdata2);
          }
          else if ( clip_view_flag )
          { clip_facet(gdata1,f_id,0);
            clip_facet(gdata2,f_id,0);
          }
          else
          { (*graph_facet)(gdata1,f_id);
            (*graph_facet)(gdata2,f_id);
          }
          
        }
        else
        { torus_clip(gdata1,m-1,f_id);
          torus_clip(gdata2,m-1,f_id);
        }
      }

    /* send on original triangle structure */
    if ( m == 0 ) 
    { if ( web.torus_flag && (torus_display_mode == TORUS_CLIPPED_MODE) )
        (*graph_facet_transforms)(gdata0,f_id);
      else if ( slice_view_flag )
        slice_facet(gdata0);
      else if ( clip_view_flag )
        clip_facet(gdata0,f_id,0);
      else
        (*graph_facet)(gdata0,f_id);
    }
    else
      torus_clip(gdata0,m-1,f_id);

} /* end torus_clip */

/********************************************************************
*
* function: bfcomp()
*
* purpose:  comparison for ordering structures in torus_bodies()
*
*/
 
int bfcomp(a,b)
struct bodyface *a,*b;
{
  if ( loc_ordinal(a->b_id) < loc_ordinal(b->b_id) ) return -1;
  if ( loc_ordinal(a->b_id) > loc_ordinal(b->b_id) ) return 1;
  if ( loc_ordinal(a->f_id) < loc_ordinal(b->f_id) ) return -1;
  if ( loc_ordinal(a->f_id) > loc_ordinal(b->f_id) ) return 1;
  if ( inverted(a->f_id) < inverted(b->f_id) ) return -1;
  if ( inverted(a->f_id) > inverted(b->f_id) ) return 1;

  return 0;
}

/**********************************************************************
*
*  Function: torus_bodies()
*
*  Purpose:  To display SOAPFILM torus model in terms of connected
*            bodies.  Finds all facets of each body and plots them
*            in a connected manner.
*/

void torus_bodies()
{
  struct bodyface *allfaces;
  int allfacetop;
  int facemax;
  facetedge_id fe,fen,ff,fa,fan;
  body_id b_id;
  int i,j,k,kk,bj;
  int numleft;
  int oldnumleft;
  int startflag;
  struct bodyface *bf;
  struct graphdata *gdata;
  facet_id f_id;
  WRAPTYPE xwrap=0; /* for centering body */
  int ctrlpts = web.skel[FACET].ctrlpts;
  REAL **xxx; /* for LAGRANGE unwrapped vertices */
  int *bodystarts; /* spots in bodyface list */

  gdata = (struct graphdata *)temp_calloc(ctrlpts+1,sizeof(struct graphdata));
  xxx = temp_dmatrix(0,ctrlpts,0,SDIM);

  facemax = 2*web.skel[FACET].count;
  allfaces = (struct bodyface *) temp_calloc(facemax,sizeof(struct bodyface));
  bodystarts = (int *)temp_calloc(web.skel[BODY].max_ord+3,sizeof(int));


  allfacetop = 0;

  /* first, get a list of all facets bounding the body,
     with proper orientation. Some may be included twice
     with opposite orientations in case the body wraps
     around the torus.
   */
  MFOR_ALL_FACETS(f_id)
  { body_id front,back;

    #ifdef MPI_EVOLVER
    if ( !mpi_show_corona_flag && (id_task(f_id) != this_task) )
       continue;
    #endif

    front = get_facet_body(f_id);
    back = get_facet_body(facet_inverse(f_id));

    if ( valid_id(front) ) 
    { allfaces[allfacetop].b_id = front;
      allfaces[allfacetop++].f_id = f_id;        
    }
    if ( valid_id(back) )
    { allfaces[allfacetop].b_id = get_facet_body(facet_inverse(f_id));
      allfaces[allfacetop++].f_id = facet_inverse(f_id);         
    }
    if ( !valid_id(front) && !valid_id(back) )
    { allfaces[allfacetop].b_id = web.skel[BODY].max_ord+1; 
                                 /* big, so bare facets sort at end */
      allfaces[allfacetop++].f_id = f_id;         
    }
  }

  /* sort in facet order, so can find facets quickly */
  qsort((char *)allfaces,allfacetop,sizeof(struct bodyface),FCAST bfcomp);

  /* find ranges for particular bodies */
  bodystarts[0] = 0;
  i = loc_ordinal(allfaces[0].b_id);  /* first body may not have ordinal 0 */
  for ( j = 0 ; j <= i ; j++ ) bodystarts[j] = 0;
  for ( k = 1 ; k < allfacetop ; k++ )
  { if ( allfaces[k].b_id != allfaces[k-1].b_id )
    { i = loc_ordinal(allfaces[k].b_id); 
      for ( ; j <= i ; j++ ) bodystarts[j] = k;
    }
  }
  for ( ; j <= web.skel[BODY].max_ord+2 ; j++ )
    bodystarts[j] = k;

  /* Now go through list repeatedly, marking proper wraps
     for facet base vertices as can.
   */
  for ( bj = 0 ; bj <= web.skel[BODY].max_ord+1 ; bj++ )
  { int facetop;
    struct bodyface *faces;

    faces = allfaces + bodystarts[bj];
    facetop = bodystarts[bj+1] - bodystarts[bj];
    if ( facetop == 0 ) continue;

    if ( bj <= web.skel[BODY].max_ord ) 
      b_id = get_ordinal_id(BODY,bj);
    else b_id = 0;

    if ( breakflag ) break;
    if ( valid_id(b_id) && show_expr[BODY] && show_expr[BODY]->start )
      if ( !eval(show_expr[BODY],NULL,b_id,NULL) ) continue;

    numleft = oldnumleft = facetop;
    do
    { startflag = (numleft == oldnumleft); /* see if need to start body piece */
      oldnumleft = numleft;
      for ( k = 0 ; k < facetop ; k++ )
      {
        if ( faces[k].wrapflag ) continue;
        if ( startflag ) /* for starting disconnected body pieces */
        { faces[k].wrap = 0; /* wrap to base cell */
          faces[k].wrapflag = IS_WRAPPED;
          numleft--;
          startflag = 0;
          continue;
        }
        fe = get_facet_fe( faces[k].f_id );
        for ( i = 0 ; i < FACET_VERTS ; i++ )  /* check for wrapped neighbor */
        { struct bodyface key;
          int counter;
          
          if ( equal_id(fe,get_prev_facet(fe)) )
          { /* valence 1 edge */
            fe = get_next_edge(fe);
            continue;
          }
          fen = fe_inverse(get_prev_facet(fe));
          ff = get_fe_facet(fen);
          if ( !equal_id(get_facet_body(ff),b_id) )
            continue;
          key.f_id = ff;
          key.b_id = b_id;
          bf = (struct bodyface *)bsearch((char *)&key,(char *)faces,
                   facetop, sizeof(struct bodyface),FCAST bfcomp);
          if ( bf == NULL )
          { sprintf(errmsg,
                "INTERNAL ERROR, torus_bodies(): face %s missing neighbor face %s on body %s.\n",
                 ELNAME(faces[k].f_id),ELNAME1(ff), ELNAME2(b_id));
            kb_error(1050,errmsg,WARNING);
            fe = get_next_edge(fe);
            faces[k].wrapflag = BAD_BAD_BAD;
            numleft--;
            continue;
          }
          if ( bf-> wrapflag != IS_WRAPPED) 
          { fe = get_next_edge(fe);
            continue;
          }
                
          /* now have wrapped neighbor */ 
  
          /* start at base point of neighbor and follow edges
             accumulating wraps until new base point is reached */
  
          faces[k].wrap = bf->wrap; 
          fan = get_facet_fe(bf->f_id);
          fen = get_next_edge(fen);  /* so tail of fan winds up at tail of fe */
          counter = 0;
          while ( !equal_id(fan,fen) )
          { /* walk around neighbor */
            faces[k].wrap = (*sym_compose)(faces[k].wrap,get_fe_wrap(fan)); 
            fan = get_next_edge(fan);
            if ( ++counter >= 3 ) break; /* just in case */
          }
          fa = get_facet_fe(faces[k].f_id);
          counter = 0;
          while ( !equal_id(fa,fe) )
          { /* walk backward around new facet */
            fe = get_prev_edge(fe);
            faces[k].wrap = (*sym_compose)(faces[k].wrap,
                                              (*sym_inverse)(get_fe_wrap(fe)));
            if ( ++counter >= 3 ) break;  /* just in case */ 
          }
              
          faces[k].wrapflag = 1;
          numleft--;
          break;  
       } 
      }
    } while ( numleft > 0 );

    /* Adjusting body wrap */
    if ( !web.torus_flag || !(get_battr(b_id) & HAVE_CENTEROFMASS) )
    { /* try to center body in cell by finding most common wrap */
      struct { WRAPTYPE wrap; int count; } xxwrap[50];
      int wrapcount = 1;  /* number of different wraps */
      memset((char*)xxwrap,0,sizeof(xxwrap));
      for ( k = 0 ; k < facetop ; k++ )
      { for ( i = 0 ; i < wrapcount ; i++ )
          if ( xxwrap[i].wrap == faces[k].wrap )
          { xxwrap[i].count++;
            break;
          }
        if ( (i == wrapcount) && (i < 50-1) )
        { xxwrap[wrapcount].wrap = faces[k].wrap;
          xxwrap[wrapcount++].count = 1;
        }
      }
      for ( k = 0, i = 0 ; i < wrapcount ; i++ )
        if ( xxwrap[i].count > xxwrap[k].count ) k = i;
      xwrap = (*sym_inverse)(xxwrap[k].wrap);
    }
    if ( web.torus_flag )
    { int vcount;
      REAL *cm;
      REAL new_cm[MAXCOORD];
      REAL u[MAXCOORD];
      facet_id f_id;
      facetedge_id fe;
      vertex_id v_id;
      REAL adjust;
      REAL x[MAXCOORD];

      /* get vertex-wise center of mass */
      vcount = 0;
      cm = get_body_cm(b_id);  /* in unit cell coordinates */
      if ( !(get_battr(b_id) & HAVE_CENTEROFMASS) )
      { for ( i = 0 ; i < SDIM ; i++ )
          cm[i] = 0.5;
        set_attr(b_id,HAVE_CENTEROFMASS);
      }
      for ( i = 0 ; i < SDIM ; i++ )
        new_cm[i] = 0.0;
      for ( kk = 0 ; kk < facetop ; kk++ )
      { f_id = faces[kk].f_id;
        fe = get_facet_fe(f_id);
        v_id = get_fe_tailv(fe);
        (*sym_wrap)(get_coord(v_id),x,faces[kk].wrap);
        for ( i = 0 ; i < SDIM ; i++ )
          new_cm[i] += x[i];
        vcount++;
      }
      for ( i = 0 ; i < SDIM ; i++ )
        new_cm[i] /= vcount;
      /* wrap to best agreement with old (which should be middle first time) */
      if ( get_battr(b_id) & HAVE_CENTEROFMASS )
      { matvec_mul(web.inverse_periods,new_cm,u,SDIM,SDIM);
        xwrap = 0;
        for ( i = SDIM-1 ; i >= 0 ; i-- )
        { xwrap <<= TWRAPBITS;
          adjust = floor(cm[i]-u[i]+0.5);
          xwrap |= (int)(adjust) & WRAPMASK;
          cm[i] = u[i] + adjust; /* for next time around */
        }
      }
      else /* save most common wrap (for backwards compatibility) */
      { (*sym_wrap)(new_cm,x,xwrap);
        matvec_mul(web.inverse_periods,x,cm,SDIM,SDIM);
      }
    } 
     
    /* now plot all the facets */
    for ( kk = 0 ; kk < facetop ; kk++ )
    { WRAPTYPE wrap;
      int fattr;
      facetedge_id fe_id;
      edge_id e_id;

      if ( breakflag ) break;

      f_id = faces[kk].f_id;
      fattr = get_fattr(f_id);
      if ( fattr & NODISPLAY ) continue;
      fe = get_facet_fe(f_id);
      wrap = (*sym_compose)(faces[kk].wrap,xwrap);
      if ( web.modeltype == LAGRANGE )
      { vertex_id *v = get_facet_vertices(f_id);
        WRAPTYPE lwrap;
        get_facet_verts(f_id,xxx,NULL);  /* for positive facet */
        if ( inverted(f_id) )
           lwrap = (*sym_compose)(get_fe_wrap(fe),wrap);
        else lwrap = wrap;
        for ( i = 0 ; i < ctrlpts ; i++ )
        { int ii;
          if ( inverted(f_id) )
          { int row=0,col;
            int n;
            n = 0;
            while ( n + (web.lagrange_order-row)  < i )
            { n += web.lagrange_order-row+1;
              row++;
            }
            col = i - n;
            ii = n + (web.lagrange_order-row-col);
          }
          else ii = i;
          gdata[i].v_id = v[ii];
          (*sym_wrap)(xxx[ii],gdata[i].x,lwrap);
          if ( colorflag )  
            gdata[i].color = gdata[i].backcolor = 
                       (facet_rgb_color_attr > 0 ) ?
                         INDEX_TO_RGBA(loc_ordinal(b_id)) : loc_ordinal(b_id);
           else
           { if ( inverted(f_id) )
             { gdata[i].color = get_facet_backcolor_2(f_id);
               gdata[i].backcolor = get_facet_frontcolor_2(f_id);
             }
             else
             { gdata[i].color = get_facet_frontcolor_2(f_id);
               gdata[i].backcolor = get_facet_backcolor_2(f_id);
             }
           }
         }
       }
       else
       for ( i = 0 ; i < FACET_VERTS ; i++ )    /* vertex loop */
       { REAL *verts;
         vertex_id v_id;

          e_id = get_fe_edge(fe);
          gdata[i].v_id = v_id = get_edge_tailv(e_id);
          verts = get_coord(v_id);
          (*sym_wrap)(verts,gdata[i].x,wrap);
          if ( colorflag )  gdata[i].color = gdata[i].backcolor = 
             (facet_rgb_color_attr > 0 ) ? INDEX_TO_RGBA(loc_ordinal(b_id)) :
                      loc_ordinal(b_id);
          else
          { gdata[i].color = get_facet_frontcolor_2(f_id);
            gdata[i].backcolor = get_facet_backcolor_2(f_id);
          }

          /* wrap vertices that belong with base of edge */
          if ( web.modeltype == QUADRATIC )
          { if ( !inverted(e_id) )
             { verts = get_coord(get_edge_midv(e_id));
               (*sym_wrap)(verts,gdata[FACET_VERTS+i].x,wrap);
             }
             gdata[FACET_VERTS+i].color = gdata[i].color;
             gdata[FACET_VERTS+i].backcolor = gdata[i].backcolor;
             gdata[FACET_VERTS+i].v_id = get_edge_midv(e_id);
          }

          wrap = (*sym_compose)(wrap,get_fe_wrap(fe));
          if ( web.modeltype == QUADRATIC && inverted(e_id) )
          { verts = get_coord(get_edge_midv(e_id));
            (*sym_wrap)(verts,gdata[FACET_VERTS+i].x,wrap);
          }
          fe = get_next_edge(fe);
        }

        if ( show_expr[FACET] && show_expr[FACET]->start )
          if ( !eval(show_expr[FACET],NULL,f_id,NULL) ) 
            gdata[0].color = gdata[0].backcolor = UNSHOWN; /* maybe do edges */


        /* do inner clipping, if called for */
        if ( inner_clip_flag )
        {
          for ( i = 0 ; i < FACET_VERTS ; i++ )
          { REAL dist = 0.0;
            REAL *x = get_coord(web.zoom_v);

            for ( j = 0 ; j < SDIM ; j++ )
              dist += (x[j]-gdata[i].x[j])*(x[j]-gdata[i].x[j]);
    
            if ( sqrt(dist) > inner_clip_rad ) break; /* it's a keeper */
          }
             if ( i == FACET_VERTS ) continue; /* entirely inside */
        }

        if ( gdata[0].color != gdata[0].backcolor )
        { /* need normal for separation */ 
          REAL dd;
          if ( web.modeltype == LAGRANGE )
            vnormal(gdata[0].x,gdata[web.lagrange_order].x,gdata[ctrlpts-1].x,
                          gdata[0].norm);
          else vnormal(gdata[0].x,gdata[1].x,gdata[2].x,gdata[0].norm);
          dd = sqrt(SDIM_dot(gdata[0].norm,gdata[0].norm));
          for ( i = 0 ; i < SDIM ; i++ )
          gdata[0].norm[i] /= dd;
        }

        fe = get_facet_fe(f_id);
        if ( normflag || thickenflag  )
         { fe = get_facet_fe(f_id);
           for ( i = 0 ; i < FACET_VERTS ; i++ )
           { calc_vertex_smooth_normal(get_fe_tailv(fe),fe,gdata[i].norm);
             fe = get_next_edge(fe);
           }
         }
  
        /* check for special edges */
        fe_id = get_facet_fe(f_id);
        for ( i = 0 ; i < 3 ; i++, fe_id = get_next_edge(fe_id) )
          { int eattr;
             gdata[i].etype = INVISIBLE_EDGE; /* default */
             gdata[i].id = e_id = get_fe_edge(fe_id);
             gdata[i].ecolor = get_edge_color_2(e_id);
             eattr = get_eattr(e_id);
             if ( get_edge_color(e_id) == CLEAR ) continue;
             if ( eattr & BOUNDARY ) gdata[i].etype |= BOUNDARY_EDGE;
             if ( equal_id(get_next_facet(fe_id),fe_id) ) /* valence 1 */
                gdata[i].etype |= SINGLE_EDGE;
             else if  ( !equal_id(get_next_facet(fe_id),get_prev_facet(fe_id)) )
                gdata[i].etype |= TRIPLE_EDGE; /* triple line at least */
             if ( (eattr & HIT_WALL) && !(fattr & CONSTRAINT) )
                gdata[i].etype |= CONSTRAINT_EDGE;
             if ( (eattr & FIXED) && !(fattr & FIXED) )
                gdata[i].etype |= FIXED_EDGE;
             if ( show_expr[EDGE] && show_expr[EDGE]->start )
             { if ( eval(show_expr[EDGE],NULL,e_id,NULL) ) 
                  gdata[i].etype |= REGULAR_EDGE;
               else gdata[i].etype = INVISIBLE_EDGE;
             }

           }
        /* call device-specific routine */
        if ( web.modeltype == LINEAR )
        { /* call option-specific routine */
          gdata->flags |= LABEL_FACET;
          option_facet(gdata,f_id);         
        }
        else if ( web.modeltype == QUADRATIC )
        /* plot as four subtriangles */
        { 
          struct graphdata qdata[FACET_VERTS+1];
          memcpy((char*)qdata,(char*)gdata,3*sizeof(struct graphdata));
          qdata[0].flags = 0;

          for( j = 0 ; j < SDIM ; j++ )
          { qdata[1].x[j] = gdata[3].x[j];
            qdata[2].x[j] = gdata[5].x[j];
          }
          qdata[1].etype=INVISIBLE_EDGE;
          qdata[1].v_id = gdata[3].v_id;
          qdata[2].v_id = gdata[5].v_id;
          qdata[0].flags = 0;
          option_facet(qdata,f_id);

          for(j=0;j<SDIM;j++)
          { qdata[0].x[j]=gdata[3].x[j];qdata[1].x[j]=gdata[1].x[j];
            qdata[2].x[j]=gdata[4].x[j];
          }
          qdata[1].etype=gdata[1].etype;
          qdata[2].etype=INVISIBLE_EDGE;
          qdata[0].v_id = gdata[3].v_id;
          qdata[1].v_id = gdata[1].v_id;
          qdata[2].v_id = gdata[4].v_id;
          qdata[0].flags = 0;
          option_facet(qdata,f_id);

          for(j=0;j<SDIM;j++)
          { qdata[0].x[j]=gdata[5].x[j];qdata[1].x[j]=gdata[4].x[j];
            qdata[2].x[j]=gdata[2].x[j];
          }
          qdata[2].etype=gdata[2].etype;
          qdata[0].etype=INVISIBLE_EDGE;
          qdata[0].v_id = gdata[5].v_id;
          qdata[1].v_id = gdata[4].v_id;
          qdata[2].v_id = gdata[2].v_id;
          qdata[0].flags = 0;
          option_facet(qdata,f_id);

          for(j=0;j<SDIM;j++)
          { qdata[0].x[j]=gdata[3].x[j];qdata[1].x[j]=gdata[4].x[j];
            qdata[2].x[j]=gdata[5].x[j];
          }
          qdata[0].etype=INVISIBLE_EDGE;
          qdata[1].etype=INVISIBLE_EDGE;
          qdata[2].etype=INVISIBLE_EDGE;
          qdata[0].v_id = gdata[3].v_id;
          qdata[1].v_id = gdata[4].v_id;
          qdata[2].v_id = gdata[5].v_id;
          qdata[0].flags = LABEL_FACET;
          option_facet(qdata,f_id);
        }
        else if ( web.modeltype == LAGRANGE )
        { int ii,jj;
          /* plot as subtriangles */
          int flags = (gdata[0].flags|LIST_FACET) & ~LABEL_FACET;
          struct graphdata qdata[FACET_VERTS+1];
          memcpy((char*)qdata,(char*)gdata,3*sizeof(struct graphdata));
          for ( i = 0 ; i < FACET_VERTS ; i++ ) qdata[i].etype = INVISIBLE_EDGE;
          for ( j = 0,jj=0,ii=web.lagrange_order+1 ; 
                            j < web.lagrange_order ; j++,jj++ )
            for ( i = 0 ; i < web.lagrange_order-j ; i++,jj++,ii++ )
             { 
                for ( k = 0 ; k < 3 ; k++ )
                { qdata[0].x[k]=gdata[jj].x[k];
                  qdata[1].x[k]=gdata[jj+1].x[k];
                  qdata[2].x[k]=gdata[ii].x[k];
                }
                qdata[0].v_id = gdata[jj].v_id;
                qdata[1].v_id = gdata[jj+1].v_id;
                qdata[2].v_id = gdata[ii].v_id;
                qdata[0].etype = (j==0) ? gdata[0].etype : INVISIBLE_EDGE;
                qdata[1].etype = (i+j+1==web.lagrange_order) ? gdata[1].etype : INVISIBLE_EDGE;
                qdata[2].etype = (i==0) ? gdata[2].etype : INVISIBLE_EDGE;
                qdata[0].flags = flags;
                option_facet(qdata,f_id);

                if ( i < web.lagrange_order-j-1 )
                { int n;
                  for ( k = 0 ; k < 3 ; k++ )
                  { qdata[0].x[k]=gdata[jj+1].x[k];
                     qdata[1].x[k]=gdata[ii+1].x[k];
                     qdata[2].x[k]=gdata[ii].x[k];
                  }
                  qdata[0].v_id = gdata[jj+1].v_id;
                  qdata[1].v_id = gdata[ii+1].v_id;
                  qdata[2].v_id = gdata[ii].v_id;
                  qdata[0].flags = flags |
              (i==web.lagrange_order/3 && j==web.lagrange_order/3 ? LABEL_FACET:0);
                  for ( n = 0 ; n < FACET_VERTS ; n++ ) 
                     qdata[n].etype = INVISIBLE_EDGE;
                  option_facet(qdata,f_id);
                }
             }
        }

     }  /* end facets */
      
    }  /* end bodies */

    temp_free((char *)allfaces);
    temp_free((char *)gdata);
    temp_free((char *)bodystarts);
    free_temp_matrix(xxx);
} /* end torus_bodies() */


/**********************************************************************
*
*  Function: torus_cells()
*
*  Purpose:  To display STRING torus model in terms of connected
*                cells. Assumes everything in x-y plane.  Graphs edges
*                around each facet in a connected manner.
*/

void torus_cells()
{
  facet_id f_id;
  struct graphdata gdata[3];
  REAL *x;
  facetedge_id fe_id,fe;
  int i; 
  WRAPTYPE wrap = 0;
  edge_id e_id;
  REAL **invper;
  /* for showing full facets, accumulate list of vertices */
  struct graphdata *facet_gdata = NULL;
  int facet_gcount = 0;
  int facet_galloc = 0;

  memset(gdata,0,3*sizeof(struct graphdata));

  if ( show_expr[FACET] && show_expr[FACET]->start ) 
  { facet_galloc = 100;
    facet_gdata = (struct graphdata *)temp_calloc(facet_galloc,
       sizeof(struct graphdata));
  }
 
/*
  if ( web.torus_display_period )
  { 
    invper = web.inverse_display_periods;
  }
  else
*/
  { 
    invper = web.inverse_periods;
  }
  MFOR_ALL_FACETS(f_id)
  { int show_this_facet = 0;


    #ifdef MPI_EVOLVER
    if ( !mpi_show_corona_flag && (id_task(f_id) != this_task) )
       continue;
    #endif

    if ( show_expr[FACET] && show_expr[FACET]->start )
    { if ( !eval(show_expr[FACET],NULL,f_id,NULL) ) continue;
      else show_this_facet = 1;
    }
    
    if ( breakflag ) break;
    facet_gcount = 0;

    fe_id = get_facet_fe(f_id);
    x = get_coord(get_fe_tailv(fe_id));
    for ( i = 0 ; i < SDIM ; i++ )
      gdata[0].x[i] = x[i];

    /* wrap back to center */
    if ( web.torus_flag )
    { int ww[MAXCOORD];
      for ( i = 0 ; i < SDIM ; i++ )
        ww[i] = (int)floor(SDIM_dot(invper[i],gdata[0].x));
      for ( i = 0, wrap = 0 ; i < SDIM ; i++ )
      { wrap <<= TWRAPBITS;
        wrap |= ww[i] & WRAPMASK;
      }
      (*sym_wrap)(x,gdata[0].x,wrap);
    }
    if ( show_this_facet )
    { facet_gdata[0] = gdata[0];
      facet_gcount = 1;
    }

    fe = fe_id;
    do
    { /* follow edges, keeping track of wrap */
      if ( !valid_id(fe) ) break;
      gdata[0].id = e_id = get_fe_edge(fe);
      gdata[0].color = gdata[0].ecolor = get_edge_color_2(gdata[0].id);
      gdata[0].etype = REGULAR_EDGE;
      gdata[0].flags = 0;
      gdata[0].v_id = get_fe_tailv(fe);
      gdata[1].v_id = get_fe_headv(fe);
      
      if ( show_this_facet && (facet_gcount > facet_galloc-20) )
      { facet_galloc *= 2;
        facet_gdata = (struct graphdata *)temp_realloc((char*)facet_gdata,
           facet_galloc*sizeof(struct graphdata));
      }
 
      if ( web.modeltype == LINEAR )
      {
        wrap = (*sym_compose)(wrap,get_fe_wrap(fe)); 
      }
      else if ( web.modeltype == QUADRATIC )
      { gdata[1] = gdata[0];
        if ( inverted(e_id) )
           wrap = (*sym_compose)(wrap,get_fe_wrap(fe)); 
        x = get_coord(get_fe_midv(fe));
        (*sym_wrap)(x,gdata[1].x,wrap); 
        gdata[0].flags &= LABEL_TAIL;
        if ( circular_arc_flag && (graph_capabilities & GC_ARCS) )
        { x = get_coord(get_fe_headv(fe));
          if ( !inverted(e_id) )
            wrap = (*sym_compose)(wrap,get_fe_wrap(fe)); 
          (*sym_wrap)(x,gdata[2].x,wrap); 
          gdata[0].flags |= EDGE_ARC;
          (*graph_edge_transforms)(gdata,gdata[0].id);
          gdata[0] = gdata[2];
          goto next_edge;
        }
        else
        { REAL headx[MAXCOORD];
          REAL tailx[MAXCOORD];
          REAL midx[MAXCOORD];
          REAL w1[MAXCOORD],mag1;
          REAL w2[MAXCOORD],mag2;
          REAL w[MAXCOORD],mag;
          REAL xx[MAXCOORD];
          REAL ang;
          int segments;
          int k;

          x = get_coord(get_fe_headv(fe));
          if ( !inverted(e_id) )
            wrap = (*sym_compose)(wrap,get_fe_wrap(fe)); 
          (*sym_wrap)(x,gdata[2].x,wrap); 


          for (i = 0 ; i < SDIM ; i++ ) 
          { headx[i] = gdata[2].x[i]; 
            tailx[i] = gdata[0].x[i];
            midx[i] = gdata[1].x[i];
            w1[i] = midx[i] - tailx[i];
            w2[i] = headx[i] - tailx[i];
          }
          mag1 = SDIM_dot(w1,w1); mag2 = SDIM_dot(w2,w2);
          for ( i = 0 ; i < SDIM  ; i++ )
          { w1[i] /= mag1; w2[i] /= mag2; }

          /* figure out how many segments to do */
          ang = 4*acos(SDIM_dot(w1,w2)*sqrt(mag1*mag2));
          segments = (int)(ang/(M_PI/180*string_curve_tolerance));
          if ( segments < 2 ) 
            segments = 2;
                
          if ( show_this_facet )
            while (facet_gcount+segments > facet_galloc-20 )
            { facet_galloc *= 2;
              facet_gdata = (struct graphdata *)temp_realloc((char*)facet_gdata,
                facet_galloc*sizeof(struct graphdata));
            }
 
          for ( i = 0 ; i < SDIM ; i++ ) gdata[0].x[i] = tailx[i];
          for ( k = 1 ; k <= segments ; k++ )
          { 
            if ( circular_arc_flag )
            { /* use circle as inversion of line */
              for ( i = 0 ; i < SDIM ; i++ )
                w[i] = w2[i] + (segments-k)/(REAL)k*(w1[i]-w2[i]);
              mag = SDIM_dot(w,w);
              for ( i = 0 ; i < SDIM ; i++ )
                gdata[1].x[i] = xx[i] = tailx[i] + w[i]/mag;
            }
            else /* quadratic spline */
            { REAL t = 2*k/(REAL)segments;
              REAL c1 = (t-1)*(t-2)/2;
              REAL c2 = t*(2-t);
              REAL c3 = t*(t-1)/2; 
              for ( i = 0 ; i < SDIM ; i++ )
                gdata[1].x[i] = xx[i] = c1*tailx[i]+c2*midx[i]+c3*headx[i];
            }
            if ( show_this_facet )
               facet_gdata[facet_gcount++] = gdata[1];
            if ( labelflag )
            {
              gdata[0].flags &= ~(LABEL_EDGE|LABEL_HEAD|LABEL_TAIL);
              if ( k == 1 ) gdata[0].flags |= LABEL_TAIL;
              if ( k == segments/2 ) gdata[0].flags |= LABEL_EDGE;
              if ( k == segments ) gdata[0].flags |= LABEL_HEAD;
            }
            (*graph_edge_transforms)(gdata,e_id);
            for ( i = 0 ; i < SDIM ; i++ )  /* gdata[1].x was messed by clip */
               gdata[0].x[i] = xx[i];
          }
          goto next_edge;
        }
      }
      else if ( web.modeltype == LAGRANGE )
      { int k;
        vertex_id *v = get_edge_vertices(e_id);
        gdata[1] = gdata[0];
        if ( inverted(e_id) )
        { wrap = (*sym_compose)(wrap,get_fe_wrap(fe)); 
          for ( k = web.lagrange_order - 1; k > 0 ; k-- )
          { x = get_coord(v[k]);
            (*sym_wrap)(x,gdata[1].x,wrap);
             if ( k == web.lagrange_order/2 ) 
                 gdata[0].flags |= LABEL_EDGE;
             if ( k == web.lagrange_order - 2 )
                 gdata[0].flags |= LABEL_HEAD;
             if ( show_this_facet )
               facet_gdata[facet_gcount++] = gdata[1];
             (*graph_edge_transforms)(gdata,gdata[0].id);
             gdata[0] = gdata[1];
          }
        }
        else
        {
          for ( k = 1;  k < web.lagrange_order ; k++ )
          { x = get_coord(v[k]);
            (*sym_wrap)(x,gdata[1].x,wrap);
            if ( k == web.lagrange_order/2 ) 
                gdata[0].flags |= LABEL_EDGE;
            if ( k == web.lagrange_order - 2 )
                gdata[0].flags |= LABEL_HEAD;
            if ( show_this_facet )
               facet_gdata[facet_gcount++] = gdata[1];
            (*graph_edge_transforms)(gdata,gdata[0].id);
            gdata[0] = gdata[1];
          }
          wrap = (*sym_compose)(wrap,get_fe_wrap(fe)); 
        }
      }
      /* now the head vertex for everybody */
      x = get_coord(get_fe_headv(fe));
      (*sym_wrap)(x,gdata[1].x,wrap); 
      if ( show_this_facet )
         facet_gdata[facet_gcount++] = gdata[1];
      gdata[0].flags |= LABEL_EDGE|LABEL_TAIL;
      (*graph_edge_transforms)(gdata,gdata[0].id);
      gdata[0] = gdata[1];
next_edge:      
      fe = get_next_edge(fe);
    }
    while ( !equal_id(fe,fe_id) );

    if ( show_this_facet )
      graph_string_facet(f_id,facet_gdata,facet_gcount);
  }
  
  if ( facet_gdata )
    temp_free((char*)facet_gdata);

} /* end  torus_cells() */

/**************************************************************************
*
* Function: graph_string_facet()
*
* Purpose: In string model, graph one polygonal facet from list
*          of vertices in graphdata structures.
*/
void graph_string_facet(f_id,facet_gdata,facet_gcount)
facet_id f_id;
struct graphdata *facet_gdata; /* the list */
int facet_gcount; /* how many in the list */
{ struct graphdata gdata[FACET_VERTS+1]; /* for an individual triangle, plus wrap */
  int i,n;

  memset((char*)gdata,0,sizeof(gdata));

  gdata[0].color = get_facet_frontcolor(f_id);
  gdata[0].backcolor = get_facet_backcolor(f_id);
  /* first vertex to be average of others */
  for ( n = 0 ; n < facet_gcount ; n++ )
  { for ( i = 0 ; i < SDIM ; i++ )
     gdata[0].x[i] += facet_gdata[n].x[i];
    facet_gdata[n].etype = INVISIBLE_EDGE;
  }
  for ( i = 0 ; i < SDIM ; i++ ) 
    gdata[0].x[i] /= facet_gcount;

  for ( n = 0 ; n <  facet_gcount-1 ; n++ )
  { gdata[1] = facet_gdata[n];
    gdata[2] = facet_gdata[n+1];
    option_facet(gdata,f_id);         
  }
  gdata[1] = facet_gdata[facet_gcount-1];
  gdata[2] = facet_gdata[0];
  option_facet(gdata,f_id);         
}

/************************************************************************
*
* function: slice_edge()
*
* purpose: Graph point, since slice of edge is point.
*          To be called after transforms done.
*/

void slice_edge(gdata)
struct graphdata *gdata;
{ 
}


/************************************************************************
*
* function: slice_facet()
*
* purpose: graph 1D edge resulting from slicing plane through 2D facet.
*          To be called after transforms done.
*/

void slice_facet(gdata)
struct graphdata *gdata;
{ struct graphdata sliceg[FACET_VERTS];
  int i,j;
  int m = 0; /* how many plane crossings found */

  memset(sliceg,0,sizeof(sliceg));
  sliceg[0].etype = REGULAR_EDGE;
  sliceg[0].ecolor = gdata[0].color;
  for ( i = 0 ; i < FACET_VERTS ; i++ )
  { int ii = (i < FACET_VERTS-1) ? i+1 : 0 ;
    REAL denom=0;
    REAL numer=slice_coeff[SDIM];
    REAL lambda;
    for ( j = 0 ; j < SDIM ; j++ )
    { numer -= slice_coeff[j]*gdata[ii].x[j];
      denom += slice_coeff[j]*(gdata[i].x[j] - gdata[ii].x[j]);
    }
    if ( denom == 0.0 )
      continue;
    lambda = numer/denom;
    if ( (lambda < 0.0) || (lambda > 1.0) )
      continue;
    for ( j = 0 ; j < SDIM ; j++ )
      sliceg[m].x[j] = lambda*gdata[i].x[j] + (1-lambda)*gdata[ii].x[j];
    m++;
  }
  if ( m == 2 )
    (*graph_edge)(sliceg,NULLID);
  else if ( m == 3 )
    (*graph_facet)(sliceg,NULLID);
}


/************************************************************************
*
* function: graph_edge_clip()
*
* purpose: wrapper for clip or slice stage
*/
void graph_edge_clip(g,id)
struct graphdata *g;
edge_id id;
{
  if ( slice_view_flag )
    slice_edge(g);
  else if ( clip_view_flag )
    clip_edge(g,id,0);
  else
    (*graph_edge)(g,id);
}

/************************************************************************
*
* function: clip_edge()
*
* purpose: Enforce clipping planes as defined in clip_coeff[][],
*          for clip_view toggle.
*          To be called after transforms done.
*/

void clip_edge(gdata,id,which)
struct graphdata *gdata;
edge_id id; /* original edge */
int which;  /* which clipping plane at this stage */
{ struct graphdata clipg[EDGE_VERTS];
  int i,j;
  int innies[EDGE_VERTS],outies[EDGE_VERTS];
  int in_count = 0,out_count = 0;
  
  /* Find which vertices are on which side */
  for ( i = 0 ; i < EDGE_VERTS ; i++ )
  { REAL value = clip_coeff[which][SDIM];
    for ( j = 0 ; j < SDIM ; j++ )
      value -= clip_coeff[which][j]*gdata[i].x[j];
    if ( value >= 0.0 )
      innies[in_count++] = i;
    else
      outies[out_count++] = i;
  }
  
  if ( in_count == 0 )
    return; /* nothing to show */

  if ( in_count == EDGE_VERTS ) /* don't have to clip */
  { if ( which < MAXCLIPS-1 )
      clip_edge(gdata,id,which+1);
    else
      (*graph_edge)(gdata,id);
    return;
  } 
    
  /* have to clip */
  
  clipg[0] = clipg[1] = gdata[0]; /* id, color, type, etc. */
  if ( innies[0] == 1 )
    for ( j = 0 ; j < SDIM ; j++ )
      clipg[0].x[j] = gdata[1].x[j];
  
  /* find intersections of clip plane with edge */
  { REAL lambda;
    REAL denom=0;
    REAL numer=clip_coeff[which][SDIM];
    for ( j = 0 ; j < SDIM ; j++ )
    { numer -= clip_coeff[which][j]*gdata[0].x[j];
      denom += clip_coeff[which][j]*(gdata[1].x[j] - gdata[0].x[j]);
    }
    if ( denom == 0.0 )
      lambda = 0;
    else 
      lambda = numer/denom;
    if ( lambda < 0.0 ) lambda = 0.0;
    else if ( lambda > 1.0 ) lambda = 1.0;

    for ( j = 0 ; j < SDIM ; j++ )
      clipg[1].x[j] = lambda*gdata[1].x[j] + (1-lambda)*gdata[0].x[j];
  }

 
  if ( which < MAXCLIPS-1 )
    clip_edge(clipg,id,which+1);
  else
    (*graph_edge)(clipg,id);
   
} /* end clip_edge() */


/************************************************************************
*
* function: clip_facet()
*
* purpose: Enforce clipping planes as defined in clip_coeff[][],
*          for clip_view toggle.
*          To be called after transforms done.
*/

void clip_facet(gdata,f_id,which)
struct graphdata *gdata;
facet_id f_id;  /* original facet */
int which;  /* which clipping plane at this stage */
{ struct graphdata clipg[2][FACET_VERTS];
  int i,j;
  int innies[FACET_VERTS]; /* which vertices in clip space */
  int in_count=0;
  int outies[FACET_VERTS];  /* which vertices out of clip space */
  int out_count=0;
  int oddie;  /* odd vertex, in or out */
  int pair[2]; /* the other pair */

  /* Find which vertices are on which side and */
  for ( i = 0 ; i < FACET_VERTS ; i++ )
  { REAL value = clip_coeff[which][SDIM];
    for ( j = 0 ; j < SDIM ; j++ )
      value -= clip_coeff[which][j]*gdata[i].x[j];
    if ( value >= 0.0 )
      innies[in_count++] = i;
    else
      outies[out_count++] = i;
  }
  
  if ( in_count == 0 )
    return; /* nothing to show */

  if ( in_count == 3 ) /* don't have to clip */
  { if ( which < MAXCLIPS-1 )
      clip_facet(gdata,f_id,which+1);
    else
      (*graph_facet)(gdata,f_id);
    return;
  } 
    
  /* have to clip, maybe 2 facets result */
  
  clipg[0][0] = clipg[0][1] = gdata[0]; /* facet id, color, normsl, etc. */
  clipg[0][0].etype = INVISIBLE_EDGE;
  clipg[0][1].etype = INVISIBLE_EDGE;
  clipg[0][0].ecolor = CLEAR;
  clipg[0][1].ecolor = CLEAR;
  
  if ( in_count == 1 )
  { oddie = innies[0];
    pair[0] = outies[0];
    pair[1] = outies[1];
  }
  else /* in_count == 2 */
  { oddie = outies[0];
    pair[0] = innies[0];
    pair[1] = innies[1];
  }

  /* find intersections of clip plane with facet edges */
  for ( i = 0 ; i < 2 ; i++ )
  { REAL lambda;
    REAL denom=0;
    REAL numer=clip_coeff[which][SDIM];
    for ( j = 0 ; j < SDIM ; j++ )
    { numer -= clip_coeff[which][j]*gdata[pair[i]].x[j];
      denom += clip_coeff[which][j]*(gdata[oddie].x[j] - gdata[pair[i]].x[j]);
    }
    if ( denom == 0.0 )
      lambda = 0;
    else 
      lambda = numer/denom;
    if ( lambda < 0.0 ) lambda = 0.0;
    else if ( lambda > 1.0 ) lambda = 1.0;

    for ( j = 0 ; j < SDIM ; j++ )
      clipg[0][i].x[j] = lambda*gdata[oddie].x[j] + (1-lambda)*gdata[pair[i]].x[j];
  }

  if ( in_count == 1 )
  { /* third vertex is innie */
    if ( innies[0] == 0 )
    { clipg[0][2] = clipg[0][1];
      clipg[0][1] = clipg[0][0];
      clipg[0][0] = gdata[0];
      clipg[0][2].ecolor = gdata[2].ecolor;
      clipg[0][2].etype = gdata[2].etype;
    }
    else if ( innies[0] == 1 )
    { clipg[0][2] = clipg[0][1];
      clipg[0][1] = gdata[1];
      clipg[0][0].etype = gdata[0].etype;
      clipg[0][0].ecolor = gdata[0].ecolor;
    }
    else
      clipg[0][2] = gdata[innies[0]];

  }
  else /* two innies, so have to split */
  { clipg[1][0] = clipg[1][1] = gdata[0]; /* facet id, color, etc */
    clipg[1][0].etype = INVISIBLE_EDGE;
    clipg[1][1].etype = INVISIBLE_EDGE;
    clipg[1][0].ecolor = CLEAR;
    clipg[1][1].ecolor = CLEAR;
 
    if ( outies[0] == 0 )
    { clipg[0][2] = clipg[0][1];
      clipg[0][1] = gdata[1];
      clipg[0][0].etype = gdata[0].etype;
      clipg[0][0].ecolor = gdata[0].ecolor;
      clipg[0][1].etype = INVISIBLE_EDGE;   
      clipg[0][1].ecolor = CLEAR;   
      clipg[1][0] = clipg[0][2];
      clipg[1][1] = gdata[1];
      clipg[1][2] = gdata[2];
    }
    else if ( outies[0] == 1 )
    { clipg[0][2] = gdata[0];
      clipg[1][0] = gdata[0];
      clipg[1][0].etype = INVISIBLE_EDGE;
      clipg[1][0].ecolor = CLEAR;
      clipg[1][1] = clipg[0][1];
      clipg[1][1].etype = gdata[1].etype;
      clipg[1][1].ecolor = gdata[1].ecolor;
      clipg[1][2] = gdata[2];
    }
    else
    { 
      clipg[0][2] = clipg[0][1];
      clipg[0][1] = gdata[1];
      clipg[1][0] = gdata[0];
      clipg[1][1] = gdata[1];
      clipg[1][1].etype = INVISIBLE_EDGE;
      clipg[1][1].ecolor = CLEAR;
      clipg[1][2] = clipg[0][0];
      clipg[1][2].etype = gdata[2].etype;
      clipg[1][2].ecolor = gdata[2].ecolor;
    }  
  
    if ( which < MAXCLIPS-1 )
      clip_facet(clipg[1],f_id,which+1);
    else
      (*graph_facet)(clipg[1],f_id);

  }
 
  if ( which < MAXCLIPS-1 )
    clip_facet(clipg[0],f_id,which+1);
  else
    (*graph_facet)(clipg[0],f_id);
  
  
} /* end clip_facet() */

