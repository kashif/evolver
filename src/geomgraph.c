/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/


/**********************************************************************
*
*  File: geomgraph.c
*
*  Contents:  Routines for interface with geomview via pipe
*                 Meant to coexist with mvgraph.c for minneview,
*                    which has several things this file uses.
*
*                 Version using OFF format 
*/

#include "include.h"

static FILE *pfd = NULL;

#ifdef OOGL

#ifdef WIN32
#define popen _popen
#define pipe(fds) _pipe(fds,256,O_BINARY)
#endif

static char gv_version[100];  /* geomview version string */
static  char pipename[100]; /* for named pipe to geomview */
static  char geom_name[100]; /* for geomview name of object */
char *base_name; /* for forming pipe name */
static int gdim; /* 3 or 4 dimensions     */

/* for facets */
static float *vvlist;  /* vertex list */
static struct flist_t { int  num;    /* number of vertices         */
                        int  v[3];   /* vertex list                */
                        int  colors; /* number of colors following */
                        float c[4];  /* color rgba                 */
                      } *fflist;
static int *vindex; /* for translating vertex id to index */
static int vercount,vmaxcount;
static int ecount;
static int fcount,fmaxcount;

/* for edges */
struct elist_t { float x[2][4];      /* 2 endpoints            */
                 float c[4];         /* colors                 */
                 vertex_id v_id[2];  /* endpoints, for picking */
               } *eelist;
int edgecount; /* actual edges */
int edgemax;   /* allocated    */

/***************************************************************
*
* function: Begin_geomview()
*
* purpose: Spawns geomview.
*
*/

void Begin_geomview(outname)
char *outname; /* if want particular name for pipe, NULL else */
{ 
  if ( geomview_flag ) 
     kb_error(1657,"Cannot have multiple geomview displays.\n",RECOVERABLE);

  gv_version[0] = '0'; gv_version[1] = 0;

  base_name = strrchr(datafilename,PATHCHAR);
  if ( base_name == NULL ) base_name = datafilename;
  else base_name++;  /* skip '/' */

  /* using named pipe */
  if ( outname ) strcpy(pipename,outname);
  else 
  { sprintf(pipename,"/tmp/%s.XXXXXX",base_name); 
#ifdef LINUX
    /* gcc complains about mktemp, so we use mkstemp */
    mkstemp(pipename); /* actually creates file */
    unlink(pipename);  /* so can recreate it later as pipe */
#else
    mktemp(pipename);
#endif
  }


  /* spawn geomview */
  if ( geompipe_flag == GEOM_TO_GEOMVIEW)
  { 
#ifdef WIN32
     kb_error(2414,"There is no Windows version of geomview.\n",RECOVERABLE);
#else
     int gv_pid;
     int to_gv_pipe[2];
     pipe(gv_pipe); /* from geomview stdout */
     pipe(to_gv_pipe); /* to geomview stdin */
     signal(SIGINT,SIG_IGN);  /* to avoid geomview 1.6 CTRL-C bug */
     gv_pid = fork();
     if ( gv_pid == 0 ) 
        { /* child */
          close(0);
          dup(to_gv_pipe[0]);
          close(to_gv_pipe[0]);
          close(to_gv_pipe[1]);
          close(1);
          dup(gv_pipe[1]);
          close(gv_pipe[0]);
          close(gv_pipe[1]);
          signal(SIGINT,SIG_IGN);
          execlp("geomview","geomview","-c","(interest (pick world))","-",NULL);
          perror("geomview"); /* only error gets here */
          kb_error(1660,"geomview invocation failed.\n",RECOVERABLE);

        }
     /* evolver */
     signal(SIGINT,catcher);
     close(gv_pipe[1]);
     close(to_gv_pipe[0]);
     pfd = fdopen(to_gv_pipe[1],"w"); /* hooked to stdin of geomview*/

     /* get geomview version */
     fprintf(pfd,"(echo (geomview-version) \"\n\")\n");
     fflush(pfd);
     read(gv_pipe[0],gv_version,sizeof(gv_version));
     if ((strcmp(gv_version,"\"1.6") > 0)&&(strcmp(gv_version,"\"1.6.1p7") < 0))
     { geomview_bug_flag = 1;
     }
#endif
    }
  else if ( geompipe_flag == GEOM_PIPE_COMMAND )
    {
      pfd = popen(outname,"w");
      if ( pfd == NULL )
      { sprintf(errmsg,"%s: spawn failed.\n",outname);
         kb_error(2075,errmsg,RECOVERABLE);
      }
    }
  else /* just a pipe, without geomview */
    {
#ifdef WIN32
      outstring("Warning: Windows named pipes do not appear as regular files,\n");
      outstring("so an ordinary file will be created instead and not deleted when pipe closed.\n");
      prompt("Enter file name: ",pipename,sizeof(pipename));

#else
      sprintf(msg,"test -p %s || (test -x /etc/mknod && /etc/mknod %s p) || (test -s /usr/sbin/mknod && /usr/sbin/mknod %s p) || (test -s /usr/bin/mkfifo && /usr/bin/mkfifo %s) || /bin/mknod %s p",
          pipename,pipename,pipename,pipename,pipename);
      if ( system(msg) != 0 )
      { sprintf(errmsg,"Pipe creation failed for %s\n",pipename);
        kb_error(1661,errmsg,RECOVERABLE) ;
      }
      sleep(1);  
      sprintf(msg,"Pipe name: %s\n",pipename);
      outstring(msg);
#endif
      pfd = fopen(pipename,"w");
      if ( pfd == NULL )
      { perror(pipename);
        kb_error(1662,"Check that needed directories exists and try again.\n",
          RECOVERABLE);
      }
    }

  geomview_flag = 1;     /* tell everybody we're initialized */
  OOGL_flag = 1;     /* tell everybody we're initialized */
  UpdateOOGL();      /* make initial display                 */
}
 
/*********************************************************************
*
* function: end_geomview_object()
*
* purpose; delete current surface from geomview display
*
*/

void end_geomview_object()
{
  if ( geomview_flag  && geom_name[0] )
     fprintf(pfd,"(delete %s)\n", geom_name);
}

/*******************************************************************
*
* function: End_geomview()
*
* purpose: Close down geomview process.
*
*/

void End_geomview()
{
  fprintf(pfd,"(exit)\n"); 
  fclose(pfd);
#ifndef WIN32
  if ( geompipe_flag == GEOM_NAMED_PIPE)
  { sprintf(msg,"rm %s",pipename);
    system(msg);
  }
#endif
  geomview_flag = 0;
  geompipe_flag = GEOM_TO_GEOMVIEW;
  geom_name[0] = 0;
  OOGL_flag = 0;
  pfd = NULL;

}

/************************************************************************
*
* function: geomview_start()
*
* purpose: initialize for new surface in geomview.
*
*/
void geomview_start()
{
  vertex_id v_id;
  int i,k;
  float *vv;

  gdim = 3;
  if ( view_4D_flag )  gdim = 4; 

  /* initialize lists for facets */
  vmaxcount = vercount = web.skel[VERTEX].count;
  ecount = web.skel[EDGE].count; if ( ecount <= 0 ) ecount = 1; /* GV bug */
  fmaxcount = web.skel[FACET].count+1;
  fcount = 0;

  vvlist = (float *)temp_calloc(vmaxcount*gdim,sizeof(float));
  fflist = (struct flist_t *)temp_calloc(fmaxcount, sizeof(struct flist_t));
  vindex = (int *)temp_calloc(web.skel[VERTEX].max_ord+2,sizeof(int));

  /* pick lists, elements in order fed to geomview */
  if ( vpicklist ) myfree((char*)vpicklist);
  vpicklist = (vertex_id*)mycalloc(vmaxcount,sizeof(vertex_id));
  if ( fpicklist ) myfree((char*)fpicklist);
  fpicklist = (facet_id*)mycalloc(fmaxcount,sizeof(facet_id));

  vv = vvlist;
  k = 0;
  FOR_ALL_VERTICES(v_id)
  { 
     REAL *x = get_coord(v_id);
     for ( i = 0 ; i < gdim ; i++,vv++ )
       if ( i < web.sdim )
         *vv = (float)x[i]; 
     vindex[loc_ordinal(v_id)] = k; /* so can reference by list order */
     vpicklist[k] = v_id;
     k++;
  }

  /* initialize list for edges */
  if ( web.representation == STRING )
     edgemax = web.skel[EDGE].count;
  else edgemax = 100;
  edgecount = 0;
  eelist = (struct elist_t *)temp_calloc(edgemax,sizeof(struct elist_t));
}

/***********************************************************************
*
* function: geomview_edge()
*
* purpose: add an edge to geomview display list.
*
*/

void geomview_edge(gdata,e_id)
struct graphdata *gdata;
edge_id e_id;
{ struct elist_t *ee;
  int color;
  int j,k;

  color = gdata[0].ecolor;
  if ( color == CLEAR ) return;
  if ( (color < 0) || (color >= IRIS_COLOR_MAX) )
     color = DEFAULT_EDGE_COLOR;

  if ( edgecount >= edgemax-1 )
  { eelist = (struct elist_t*)temp_realloc((char*)eelist,
          2*edgemax*sizeof(struct elist_t));
    edgemax *= 2;
  }
  ee = eelist + edgecount;

  if ( edge_rgb_color_attr > 0 )
  { ee->c[0] = (float)(((color>>24)&0xFF)/255.);
    ee->c[1] = (float)(((color>>16)&0xFF)/255.);
    ee->c[2] = (float)(((color>>8)&0xFF)/255.);
    ee->c[3] = (float)(facet_alpha_flag ? (color&0xFF)/255. : facet_alpha);
  }
  else
  { for ( j = 0 ; j < 3 ; j++ )
       ee->c[j] = (float)(rgb_colors[color][j]);
    ee->c[3] = (float)(facet_alpha);
  }

  /* record for later VECT format */
  for ( k = 0 ; k < 2 ; k++ )
    for ( j = 0 ; j < SDIM ; j++ )
      ee->x[k][j] = (float)gdata[k].x[j];
  /* pick data */
  if ( valid_id(e_id) )
  { ee->v_id[0] = get_edge_tailv(e_id);
    ee->v_id[1] = get_edge_headv(e_id);
  }
  else
  { ee->v_id[0] = gdata[0].v_id;
    ee->v_id[1] = gdata[1].v_id;
  }

  edgecount++;
}

/************************************************************************
*
*  Function: geomview_facet()
*
*  Purpose:  Accepts facets from graphgen() and plots them.
*/

void geomview_facet(gdata,f_id)
struct graphdata *gdata;
facet_id f_id;
{ struct flist_t *ff;
  int color;
  int i,j,k;
  int dim = (web.sdim < gdim) ? web.sdim : gdim;

  color = gdata[0].color;
  if ( color == UNSHOWN ) goto do_edges;
  if ( (color >= IRIS_COLOR_MAX) )
     color = DEFAULT_EDGE_COLOR;

  if ( fcount >= fmaxcount-2 )
  { fflist = (struct flist_t*)temp_realloc((char*)fflist,
         2*fmaxcount*sizeof(struct flist_t));
    fpicklist = (facet_id*)kb_realloc((char*)fpicklist,
         2*fmaxcount*sizeof(facet_id));
    fmaxcount *= 2;
  }
  if ( color != CLEAR )
  {
    ff = fflist + fcount;
    ff->num = FACET_VERTS; /* 3 vertices on polygon */
    ff->colors = 4; 
    if ( facet_rgb_color_attr > 0 )
    { ff->c[0] = (float)(((color>>24)&0xFF)/255.);
      ff->c[1] = (float)(((color>>16)&0xFF)/255.);
      ff->c[2] = (float)(((color>>8)&0xFF)/255.);
      ff->c[3] = (float)(facet_alpha_flag ? (color&0xFF)/255. : facet_alpha);
    }
    else
    { for ( j = 0 ; j < 3 ; j++ )
        ff->c[j] = (float)rgb_colors[color][j];
      ff->c[3] = (float)facet_alpha;
    }
    fpicklist[fcount] = f_id;

    if ( gdata[0].flags & LIST_FACET )
     for ( i = 0 ; i < FACET_VERTS ; i++ )
     {
       ff->v[i] = vindex[loc_ordinal(gdata[i].v_id)];
     }
    else if ( gdata[0].flags & SIMPLE_FACET ) /* can use regular vertices */
    { facetedge_id fe = get_facet_fe(f_id);
       if ( valid_id(fe) )
        for ( i = 0 ; i < FACET_VERTS ; i++ )
        {
           ff->v[i] = vindex[loc_ordinal(get_fe_tailv(fe))];
           fe = get_next_edge(fe);
        }
    }
    else /* have to add vertices to list also */
    {
       while ( vercount + FACET_VERTS > vmaxcount )
       { vvlist=(float*)temp_realloc((char*)vvlist,
               gdim*2*vmaxcount*sizeof(float));
         vpicklist=(vertex_id*)kb_realloc((char*)vpicklist,
               2*vmaxcount*sizeof(vertex_id));
         vmaxcount *= 2;
       }
       for ( k = 0 ; k < FACET_VERTS ; k++,vercount++ )
       { for ( j = 0 ; j < dim ; j++ )
            vvlist[vercount*gdim+j] = (float)gdata[k].x[j];
         vpicklist[vercount] = gdata[k].v_id;
         ff->v[k] = vercount;
       }
    }
  fcount++;
  }

  if ( gdata[0].color != gdata[0].backcolor )
    if ( gdata[0].backcolor != CLEAR )
    { /* add perturbed back face */
      ff = fflist + fcount;
      ff->num = 3; /* 3 vertices on polygon */
      ff->colors = 4; 
      color = gdata[0].backcolor;
      if ( facet_rgb_color_attr > 0 )
      { ff->c[0] = (float)(((color>>24)&0xFF)/255.);
        ff->c[1] = (float)(((color>>16)&0xFF)/255.);
        ff->c[2] = (float)(((color>>8)&0xFF)/255.);
        ff->c[3] = (float)(facet_alpha_flag ? (color&0xFF)/255. : facet_alpha);
      }
      else
      for ( j = 0 ; j < 3 ; j++ )
        ff->c[j] = (float)rgb_colors[color][j];
      ff->c[3] = (float)facet_alpha;

      fpicklist[fcount] = f_id;
      while ( vercount + 3 > vmaxcount )
      { vvlist=(float*)temp_realloc((char*)vvlist,
           gdim*2*vmaxcount*sizeof(float));
        vpicklist=(vertex_id*)kb_realloc((char*)vpicklist,
           2*vmaxcount*sizeof(vertex_id));
        vmaxcount *= 2;
      }
     for ( k = 0 ; k < 3 ; k++,vercount++ )
     { for ( j = 0 ; j < dim ; j++ )
         vvlist[vercount*gdim+j] = 
              (float)(gdata[2-k].x[j] - thickness*gdata[0].norm[j]);
       vpicklist[vercount] = vpicklist[ff->v[k]];
       ff->v[k] = vercount;
     }
     fcount++; 
  }

  /* do designated edges */
do_edges:
  gdata[FACET_VERTS] = gdata[0];  /* last is first */
  for ( k = 0 ; k < FACET_VERTS ; k++ )
     if ( (gdata[k].etype & EBITS) != INVISIBLE_EDGE )
        geomview_edge(gdata+k,gdata[k].id);
}

/*******************************************************************
*
*  Function: geomview_end()
*
*  Purpose:  Finish off geomview stuffing.
*
*/

void geomview_end()
{ int i,j,k;
  float *vv = vvlist;
  struct flist_t *ff = fflist;
  int ret;  /* return code from fprintf or fwrite */

  base_name = strrchr(datafilename,PATHCHAR);
  if ( base_name == NULL ) base_name = datafilename;
  else base_name++;  /* skip '/' */
  if ( !base_name || !base_name[0] ) goto g_exit;  /* no surface */

  setting_backcull = 0; /* so read_pick() knows */
  /* if ( gdim < 4 ) */
  { /* command format wrapper */
    strncpy(geom_name,base_name,sizeof(geom_name));
    fprintf(pfd,"(geometry %s ",geom_name); 
    if ( transforms_flag && (transform_count>1) && !transform_colors_flag )
      fprintf(pfd," { INST \nunit { = \n");
    fprintf(pfd,"LIST {\n");
    if ( strcmp(gv_version,"\"1.6") > 0 )
    { if ( backcull_flag ) fprintf(pfd,"{appearance { backcull}}\n");
      else fprintf(pfd,"{appearance {-backcull}}\n");
      setting_backcull = 1;
    }
  }

  /* facets */
  if ( fcount > 0 )
  {
    if ( gv_binary_flag )
    { if ( gdim == 3 )
          ret =fprintf(pfd,"{OFF BINARY\n");
      else 
          ret =fprintf(pfd,"{4OFF BINARY\n");
      if ( ret < 0 ) return; /* broken pipe */
      fwrite(&vercount,1,sizeof(int),pfd);
      fwrite(&fcount,1,sizeof(int),pfd);
      fwrite(&ecount,1,sizeof(int),pfd);
      fwrite(vvlist,vercount*gdim,sizeof(float),pfd);
      fwrite(fflist,fcount,sizeof(struct flist_t),pfd);
    }
    else
    { if ( gdim == 3 )
         ret =fprintf(pfd,"{OFF\n");  /* COFF used for vertex colors only */
      else 
         ret =fprintf(pfd,"{4OFF\n"); /* COFF used for vertex colors only */
      if ( ret < 0 ) return; /* broken pipe */
      fprintf(pfd,"%d %d %d\n",vercount,fcount,ecount); 
      for ( k = 0, vv = vvlist ; (k < vercount)  ; k++,vv+=gdim )
      { if ( gdim == 3 )
          fprintf(pfd,"%f %f %f\n",(DOUBLE)vv[0],(DOUBLE)vv[1],(DOUBLE)vv[2]);
        else
          fprintf(pfd,"%f %f %f %f\n",(DOUBLE)vv[0],(DOUBLE)vv[1],(DOUBLE)vv[2],
              (DOUBLE)vv[3]);
      }
      for ( k = 0, ff = fflist ; (k < fcount)  ; k++,ff++ )
        fprintf(pfd,"3 %d %d %d  %f %f %f\n",ff->v[0],ff->v[1],
            ff->v[2], (DOUBLE)ff->c[0],(DOUBLE)ff->c[1],(DOUBLE)ff->c[2]);
    }
    fprintf(pfd,"}\n");
  }      /* end facets */

  /* edges */
  if ( edgecount > 0 )
  {  struct elist_t *ee;
     int new_vmax;

     /* append edge vertices to pick list */
     if ( fcount > 0 ) 
     { new_vmax = vercount + 2*edgecount;
     }
     else
     { new_vmax = 2*edgecount;
       vercount = 0;
     }
     gv_vect_start = vercount;
     vpicklist=(vertex_id*)kb_realloc((char*)vpicklist,
                      new_vmax*sizeof(vertex_id));

     for ( i = 0, ee = eelist ; i < edgecount ; i++,ee++,vercount += 2 )
     { vpicklist[vercount] = ee->v_id[0];
       vpicklist[vercount+1] = ee->v_id[1];
     }

     if ( gv_binary_flag & 0  ) /* Geomview vect binary fouled up somehow */
     { int tmp;
       if ( gdim == 4 )
          fprintf(pfd,"{4VECT BINARY\n");
       else
          fprintf(pfd,"{VECT BINARY\n");
       fwrite(&edgecount,1,sizeof(int),pfd);  /* NPOLYLINES */
       tmp = 2*edgecount;
       fwrite(&tmp,1,sizeof(int),pfd);    /* NVERTICES */
       fwrite(&edgecount,1,sizeof(int),pfd);  /* NCOLORS */
       tmp = 2;
       for ( i = 0 ; (i < edgecount)  ; i++ ) 
          fwrite(&tmp,1,sizeof(int),pfd);    /* vertices per line */
       tmp = 1;
       for ( i = 0 ; (i < edgecount)  ; i++ ) 
          fwrite(&tmp,1,sizeof(int),pfd);     /* colors per line */
       for ( i = 0,ee=eelist ; (i < edgecount) ; i++,ee++ ) 
            /* vertex coords */
       { fwrite(ee->x[0],gdim,sizeof(float),pfd); /* vertices */
         fwrite(ee->x[1],gdim,sizeof(float),pfd);
       }
       for ( i = 0,ee=eelist ; (i < edgecount)  ; i++,ee++ ) 
           /* line colors */
           fwrite(ee->c,4,sizeof(float),pfd); /* colors */
     }
     else
     {
       /* first line: nlines,nvertices,ncolors */
       if ( gdim == 4 )
          fprintf(pfd,"{4VECT \n%d %d %d\n",edgecount,2*edgecount,edgecount);
       else
          fprintf(pfd,"{VECT \n%d %d %d\n",edgecount,2*edgecount,edgecount);
       for ( i = 0 ; (i < edgecount)  ; i++ ) 
           fprintf(pfd,"2\n"); /* vert per edge */
       for ( i = 0 ; (i < edgecount)  ; i++ ) 
           fprintf(pfd,"1\n"); /* colors per edge */
       for ( i = 0,ee=eelist ; (i < edgecount) ; i++,ee++ ) 
          /* vertex coords */
       {  if ( gdim == 4 )
           fprintf(pfd,"%f %f %f %f  %f %f %f %f\n",(DOUBLE)ee->x[0][0],
              (DOUBLE)ee->x[0][1], (DOUBLE)ee->x[0][2],(DOUBLE)ee->x[0][3],
              (DOUBLE)ee->x[1][0],(DOUBLE)ee->x[1][1],(DOUBLE)ee->x[1][2],
              (DOUBLE)ee->x[1][3]);
          else 
           fprintf(pfd,"%f %f %f    %f %f %f\n",
              (DOUBLE)ee->x[0][0],(DOUBLE)ee->x[0][1],(DOUBLE)ee->x[0][2],
              (DOUBLE)ee->x[1][0],(DOUBLE)ee->x[1][1],(DOUBLE)ee->x[1][2]);
       }
       for ( i = 0,ee=eelist ; (i < edgecount)  ; i++,ee++ ) 
        /* line colors */
        fprintf(pfd,"%f %f %f %f\n",(DOUBLE)ee->c[0],(DOUBLE)ee->c[1],
          (DOUBLE)ee->c[2],(DOUBLE)ee->c[3]);
     }
     fprintf(pfd,"}\n");
  } /* end edges */

  /* end wrapping */
  fprintf(pfd,"}\n"); /* end LIST */

  /* transforms, if any */
  if ( transforms_flag && (transform_count>1) && !transform_colors_flag  )
  { fprintf(pfd,"\n}\ntlist { =\n");
    fprintf(pfd,"\n");
    for ( k = 0 ; k < transform_count ; k++ )
    { for ( i = 0 ; i < 4 ; i++ )
      { for ( j = 0 ; j < 4 ; j++ )
        { if ( view_4D_flag ) /* pass on 4x4 corner */
            fprintf(pfd,"%f ",(DOUBLE)view_transforms[k][j][i]);
          else if ( (i < SDIM) && (j < SDIM) )
            fprintf(pfd,"%f ",(DOUBLE)view_transforms[k][j][i]);
          else if ( (i < SDIM) && (j == 3) )
            fprintf(pfd,"%f ",(DOUBLE)view_transforms[k][SDIM][i]);
          else if ( (j < SDIM) && (i == 3) )
            fprintf(pfd,"%f ",(DOUBLE)view_transforms[k][j][SDIM]);
          else if ( (j == 3) && (i == 3) )
            fprintf(pfd,"%f ",(DOUBLE)view_transforms[k][SDIM][SDIM]);
          else 
            fprintf(pfd,"%f ",(DOUBLE)identmat[j][i]);
        }
        fprintf(pfd,"\n");
      }
      fprintf(pfd,"\n");
    }
    fprintf(pfd,"}\n}\n");

  }
  fprintf(pfd,"\n)"); /* matches (geometry */

 fprintf(pfd,"\n(event-pick on)\n");
 fprintf(pfd,"(pickable %s)\n",geom_name);
 fprintf(pfd,"(interest (pick World))\n");  /* enable picking */ 
 fflush(pfd);

g_exit:
  temp_free((char*)vvlist);
  temp_free((char*)eelist);
  temp_free((char*)fflist);
  temp_free((char*)vindex);
}

#else


/*********************************
* stubs for non-geomview systems *
*********************************/

void Begin_geomview(name)
char *name;
{
    kb_error(2076,"This Evolver not compiled for Geomview. Compile with -DOOGL.\n",RECOVERABLE);
}
void End_geomview()
{
}
void end_geomview_object()
{
}
void geomview_facet(gdata,f_id)
struct graphdata *gdata;
facet_id f_id;
{
}
#endif

/**************************************************************************
*
* function: geomview_command()
*
* purpose: forward a command to geoview, if active.
*/

void geomview_command(cmd)
char *cmd;
{ if ( geomview_flag )
  { fprintf(pfd,cmd); fflush(pfd); }
  else kb_error(1663,"Geomview not active.\n",WARNING);

}

