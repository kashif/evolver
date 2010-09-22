// iges.cmd
// Surface Evolver script to write IGES file for surface, using IGES
// finite element entity (type 136), which handles linear, quadratic,
// and cubic triangles (among many other types not of interest).

// Finite element type not official "geometry"????  Rhino says can't
// find any independent geometry, and http://www.iges5x.org/taxonomy/ has
// FEM types included under "non-geometry taxonomy'.
// Also not listed in Table 3 on p. 38 of IGES-6 documentation.

// So we try using parametric patches, type 114.

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

// usage: iges >>> "filename.igs"

// Set up color translation array
define iges_colors integer [16];
//iges_colors[black] := 1
iges_colors[red]   := 2
iges_colors[green] := 3
iges_colors[blue]  := 4
iges_colors[yellow] := 5
iges_colors[magenta] := 6
iges_colors[cyan]  := 7
iges_colors[white] := 8
iges_colors[brown]  := 2 // red
iges_colors[lightgray]  := 8 // white
iges_colors[darkgray]  := 1 // black
iges_colors[lightblue]  := 4 // blue
iges_colors[lightcyan]  := 7 // cyan
iges_colors[lightred]  := 2 // red
iges_colors[lightmagenta]  := 7

iges := { 

  local start_counter,global_counter,message,xmax,ymax,zmax;
  local maxsize,entype,paramdata,structure,linefont,level,view;
  local transmat,label,status,directory_counter,lineweight;
  local colornum,paramcount,form,reserved,entlabel,entsubscr;
  local parameter_counter,ctype,ptype,mvalue,nvalue,minu,maxu;
  local minv,maxv,co00,co10,co01,co11,line,subcount;

  // Flag section
  // Don't need this since not doing binary or compressed format.

  // Start section
  start_counter := 0;

  start_counter += 1;
  printf "%-72sS%07d\n","IGES version of Surface Evolver surface",start_counter;
  start_counter += 1;
  printf "   %-69sS%07d\n",datafilename,start_counter;
  start_counter += 1;
  printf "%-72sS%07d\n","Created using iges.cmd Evolver script.",
     start_counter;

  // Global section
  global_counter := 0;

  global_counter += 1;
  printf "%-72sG%07d\n","1H,,1H;,",global_counter;
  global_counter += 1;
  printf "%02dH%-69sG%07d\n",sizeof("Surface Evolver"),"Surface Evolver,",
     global_counter;
  global_counter += 1;
  message := sprintf "%s,",datafilename;
  printf "%02dH%-69sG%07d\n",sizeof(datafilename),message,global_counter;
  global_counter += 1;
  printf "%02dH%-69sG%07d\n",sizeof("Surface Evolver"),"Surface Evolver,",
     global_counter;
  global_counter += 1;
  printf "%02dH%-69sG%07d\n",sizeof("Surface Evolver"),"Surface Evolver,",
     global_counter;
  global_counter += 1;
  printf "%-72sG%07d\n","32,75,6,75,15,,1.0,1,2HIN,32768,0.0394,",
    global_counter;
  global_counter += 1;
  printf "%-72sG%07d\n","15H00000000.000000,", global_counter; // date

  xmax := max(vertex,abs(x));
  ymax := max(vertex,abs(y));
  zmax := max(vertex,abs(z));
  maxsize := (xmax > ymax) ? xmax : ymax;
  maxsize := (zmax > maxsize) ? zmax : maxsize;
  message := sprintf "%g,%g,",maxsize/10000000,maxsize;
  global_counter += 1;
  printf "%-72sG%07d\n",message, global_counter; 
  
  global_counter += 1;
  printf "%02dH%-69sG%07d\n",sizeof("Name of author"),"Name of author,", 
       global_counter; 
  global_counter += 1;
  printf "%02dH%-69sG%07d\n",sizeof("Author's organization"),
      "Author's organization,", global_counter;
  global_counter += 1;
  printf "%-72sG%07d\n","15H00000000.000000;", global_counter; // date

  // Directory entry section. Each entry 20 8-char fields on two lines.
  // Fields with default values
  entype := 0;      // 1 and 11
  paramdata := 1;   // 2
  structure := 0;   // 3
  linefont  := 0;   // 4
  level     := 0;   // 5
  view      := 0;   // 6
  transmat  := 0;   // 7
  label     := 0;   // 8
  status    := "00000000";   // 9; actually 4 two-digit numbers
  directory_counter := 0;  // 10 and 20
  lineweight := 0;   // 12
  colornum   := 0;   // 13
  paramcount := 0;   // 14
  form       := 0;   // 15
  reserved   := "        ";   // 16 and 17
  entlabel   := "entity";  // 18
  entsubscr  := 0;   // 19
 

  // Facets as "parametric surface" types
  entype := 114;
  status := "00010001";
  paramcount := 1;
  entlabel := "   FACET";
  define facet attribute fpdata integer;
  define facet attribute fdir integer;
  foreach facet ff do 
  { ff.fpdata := paramdata;
    entsubscr  := ff.id; 
    directory_counter += 1;  ff.fdir := directory_counter;
    printf "%8d%8d%8d%8d%8d%8d%8d%8d%8sD%7d\n",entype,paramdata,structure,
      linefont,level,view,transmat,label,status,directory_counter;
    directory_counter += 1;
    printf "%8d%8d%8d%8d%8d%8s%8s%8s%8dD%7d\n",entype,lineweight,
      iges_colors[ff.color],
      paramcount,form,reserved,reserved,entlabel,entsubscr,directory_counter;
    paramdata += 7;
  }; 
  // Geometry element
  entype := 402;
  status := "00000301";
  paramcount := ceil(facet_count/10);
  form := 7;
  entlabel := " SURFACE";
  entsubscr  := 1; 
  directory_counter += 1;  
  printf "%8d%8d%8d%8d%8d%8d%8d%8d%8sD%7d\n",entype,paramdata,structure,
     linefont,level,view,transmat,label,status,directory_counter;
  directory_counter += 1;
  printf "%8d%8d%8d%8d%8d%8s%8s%8s%8dD%7d\n",entype,lineweight,colornum,
     paramcount,form,reserved,reserved,entlabel,entsubscr,directory_counter;


  // Parameter data section
  parameter_counter := 0;

  // Facets
  entype := 114;
  ctype := 1;  // linear
  ptype := 0;  // unspecified
  mvalue := 1;  // number of u segments
  nvalue := 1;  // number of v segments
  minu := 0;
  maxu := 1;
  minv := 0;
  maxv := 1;
  foreach facet ff do
  { parameter_counter += 1;
    if ff.fpdata != parameter_counter then
       errprintf
     "ERROR: bad facet parameter line number, facet %d. Is %d, should be %d.\n",
         ff.id,parameter_counter,ff.fpdata;
    message := sprintf "%d,%d,%d,%d,%d,%g,%g,%g,%g,",entype,ctype,ptype,
         mvalue,nvalue,minu,maxu,minv,maxv;
    printf "%-64s%8dP%7d\n",message,ff.fdir,parameter_counter;
    co00 := ff.vertex[1].x;
    co10 := ff.vertex[2].x-ff.vertex[1].x;
    co01 := ff.vertex[3].x-ff.vertex[1].x;
    co11 := ff.vertex[1].x-ff.vertex[2].x;
    message := sprintf "%10.7f,%10.7f,0,0,%10.7f,%10.7f,0,0,",
       co00,co10,co01,co11;
    parameter_counter += 1;
    printf "%-64s%8dP%7d\n",message,ff.fdir,parameter_counter;
    message :=  "0,0,0,0,0,0,0,0,";
    parameter_counter += 1;
    printf "%-64s%8dP%7d\n",message,ff.fdir,parameter_counter;
    co00 := ff.vertex[1].y;
    co10 := ff.vertex[2].y-ff.vertex[1].y;
    co01 := ff.vertex[3].y-ff.vertex[1].y;
    co11 := ff.vertex[1].y-ff.vertex[2].y;
    message := sprintf "%10.7f,%10.7f,0,0,%10.7f,%10.7f,0,0,",
       co00,co10,co01,co11;
    parameter_counter += 1;
    printf "%-64s%8dP%7d\n",message,ff.fdir,parameter_counter;
    message :=  "0,0,0,0,0,0,0,0,";
    parameter_counter += 1;
    printf "%-64s%8dP%7d\n",message,ff.fdir,parameter_counter;
    co00 := ff.vertex[1].z;
    co10 := ff.vertex[2].z-ff.vertex[1].z;
    co01 := ff.vertex[3].z-ff.vertex[1].z;
    co11 := ff.vertex[1].z-ff.vertex[2].z;
    message := sprintf "%10.7f,%10.7f,0,0,%10.7f,%10.7f,0,0,",
       co00,co10,co01,co11;
    parameter_counter += 1;
    printf "%-64s%8dP%7d\n",message,ff.fdir,parameter_counter;
    message :=  "0,0,0,0,0,0,0,0,";
    parameter_counter += 1;
    printf "%-64s%8dP%7d\n",message,ff.fdir,parameter_counter;
  };
  // Geometry element
  line := sprintf "402,%d,",facet_count;
  subcount := 0;
  foreach facet ff do
  { if subcount == 10 then
    { 
      parameter_counter += 1;
      printf "%-64s%8dP%7d\n",line,directory_counter-1,parameter_counter;
      line := "" ;
      subcount := 0;
    };
    line := sprintf"%s%d,",line,ff.fdir;
    subcount += 1;
  };
  line := sprintf "%s0,0;",line;  // sample files ended with 2 extra 0's
  parameter_counter += 1;
  printf "%-64s%8dP%7d\n",line,directory_counter-1,parameter_counter;

 
  // Terminate section
  printf "S%07dG%07dD%07dP%07d%40sT0000001\n",start_counter,global_counter,
     directory_counter,parameter_counter," ";
}


