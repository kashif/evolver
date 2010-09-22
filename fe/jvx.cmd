// jvx.cmd
// Create jvx file for JavaView

// Usage:  jvx >>> "filename.jvx"

// Programmer: Ken Brakke, brakke@susqu.edu, http://www.susqu.edu/brakke

define vertex attribute jvx_number integer;    // for implicit vertex order
define facet attribute fjvxnum integer;    // for implicit facet order

define facetcolorcount integer[16];  // to see which color facets are present
define edgecolorcount integer[16];  // to see which color edges are present
define rgb integer[16][3];     // color definitions
rgb[1][1] := 0; rgb[1][2] := 0; rgb[1][3] := 0;
rgb[2][1] := 0; rgb[2][2] := 0; rgb[2][3] := 255;
rgb[3][1] := 0; rgb[3][2] := 255; rgb[3][3] := 0;
rgb[4][1] := 0; rgb[4][2] := 255; rgb[4][3] := 255;
rgb[5][1] := 255; rgb[5][2] := 0; rgb[5][3] := 0;
rgb[6][1] := 255; rgb[6][2] := 0; rgb[6][3] := 255;
rgb[7][1] := 255; rgb[7][2] := 127; rgb[7][3] := 0;
rgb[8][1] := 160; rgb[8][2] := 160; rgb[8][3] := 160;
rgb[9][1] := 80; rgb[9][2] := 80; rgb[9][3] := 80;
rgb[10][1] := 80; rgb[10][2] := 200; rgb[10][3] := 255;
rgb[11][1] := 127; rgb[11][2] := 255; rgb[11][3] := 127;
rgb[12][1] := 127; rgb[12][2] := 255; rgb[12][3] := 255;
rgb[13][1] := 255; rgb[13][2] := 127; rgb[13][3] := 127;
rgb[14][1] := 255; rgb[13][2] := 127; rgb[13][3] := 255;
rgb[15][1] := 255; rgb[15][2] := 255; rgb[15][3] := 0;
rgb[16][1] := 255; rgb[16][2] := 255; rgb[16][3] := 255;

jvx := { 
  // First, bounding box.
  maxx := max(vertex,x); minx := min(vertex,x);
  maxy := max(vertex,y); miny := min(vertex,y);
  maxz := max(vertex,z); minz := min(vertex,z);
  printf "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" standalone=\"yes\"?>\n";
  printf "<jvx-model>\n";
  printf "<meta generator=\"Surface Evolver 2.16; jvx.cmd\"/>\n";
//  printf "<meta date=\"%s\"/>\n",date_and_time;
  printf "<title> %s </title>\n",datafilename;
  printf "<version>1.0</version>\n";
/*
  printf "<authors>\n";
  printf " <author>\n";
  printf "   <firstname>Kenneth</firstname>\n";
  printf "   <lastname>Brakke</lastname>\n";
  printf "   <affiliation>\n";
  printf "      <organization>Susquehanna University</organization>\n";
  printf "      <address>\n";
  printf "        <line>Mathematics Department</line>\n";
  printf "        <line>Susquehanna University</line>\n";
  printf "        <line>Selinsgrove, PA 17870</line>\n";
  printf "        <line>USA</line>\n";
  printf "      </address>\n";
  printf "   </affiliation>\n";
  printf "   <email>brakke@susqu.edu</email>\n";
  printf "   <url>http://www.susqu.edu/brakke</url>\n";
  printf " </author>\n";
  printf "</authors>\n";
  printf "<description>\n";
  printf "  <abstract>One-line description of surface.</abstract>\n";
  printf "  <detail>\n";
  printf "    Paragraph of description here.\n";
  printf "  </detail>\n";
  printf "  <keywords>\n";
  printf "    <keyword>minimal surface</keyword>\n";
  printf "  </keywords>\n";
  printf "  <msc2000>\n";
  printf "    <primary>49Q20</primary>\n";
  printf "    <secondary> optional </secondary>\n";
  printf "    <secondary> optional </secondary>\n";
  printf "  </msc2000>\n";
  printf "  <software>Surface Evolver 2.16</software>\n";
  printf "</description>\n";
*/
  printf "<geometries>\n";
  printf "  <geometry name=\"surface1\">\n";
  printf "    <pointSet dim=\"3\" point=\"hide\">\n";
  printf "      <points num=\"%d\">\n",vertex_count;
  jvxnum := 0;
  foreach vertex vv do
  { printf "          <p> %f %f %f </p>\n",vv.x,vv.y,vv.z; 
    vv.jvx_number := jvxnum;
    jvxnum += 1;
  };
  printf "      </points>\n";
  printf "    </pointSet>\n";


  facetnum := 0;
  foreach facet ff where ff.show and color != clear do
  { ff.fjvxnum := facetnum; facetnum += 1; };
  colornum := 1;
  while colornum <= 16 do { facetcolorcount[colornum] := 0; colornum += 1; };
  foreach facet ff where ff.show and color != clear do
   facetcolorcount[ff.color+1] += 1; 
  colornum := 1; numcolors := 0;
  while colornum <= 16 do 
  { if facetcolorcount[colornum] > 0 then numcolors += 1; colornum += 1; };
  if numcolors > 1 then
   printf "    <faceSet face=\"show\" edge=\"hide\" color=\"show\">\n"
  else printf "    <faceSet face=\"show\" edge=\"hide\">\n";
  printf "      <faces num=\"%d\">\n",sum(facets,show);
  foreach facet ff where ff.show  and color != clear do
  { printf "        <f> %d %d %d </f>\n",ff.vertex[1].jvx_number,
          ff.vertex[2].jvx_number, ff.vertex[3].jvx_number; 
  };
  if numcolors <= 1 then 
  { colornum := 1; 
    while colornum <= 16 do
    { if facetcolorcount[colornum] > 0 then
        printf "        <color> %d %d %d </color>\n",rgb[colornum][1],
          rgb[colornum][2],rgb[colornum][3];
      colornum += 1;
    };
  };
  printf "      </faces>\n";
  if numcolors > 1 then 
  {
    printf "      <colors>\n";
    foreach facet ff where ff.show and color != clear do
    { printf "        <c> %d %d %d </c>\n",rgb[ff.color+1][1],
            rgb[ff.color+1][2],rgb[ff.color+1][3];
    };
    printf "      </colors>\n";
  };
  printf "      <neighbours num=\"%d\">\n",sum(facets,show);
  foreach facet ff where ff.show and color != clear do
    printf "        <nb> %d %d %d </nb>\n",
      ff.edge[2].valence == 2 ?
      max(ff.edge[2].facet where id != ff.id,fjvxnum) : -1,
      ff.edge[3].valence == 2 ?
      max(ff.edge[3].facet where id != ff.id,fjvxnum) : -1,
      ff.edge[1].valence == 2 ?
      max(ff.edge[1].facet where id != ff.id,fjvxnum) : -1;
  printf "      </neighbours>\n";
//  printf "      <edges num=\"%d\">\n",sum(edges,show);
//  foreach edge ee where ee.show do
//  { printf "        <e> %d %d </e>\n",ee.vertex[1].jvx_number,
//          ee.vertex[2].jvx_number; 
//  };
//  printf "        <color> 0 0 0 </color>\n";
//  printf "      </edges>\n";
  printf "      <edges>\n";
  printf "         <thickness> 1.0 </thickness>\n";
  printf "         <color type=\"rgb\">0 0 0</color>\n";
  printf "      </edges>\n";
  printf "    </faceSet>\n";
  printf "    <bndbox visible=\"hide\">\n";
  printf "      <p> %f %f %f </p>\n",minx,miny,minz;
  printf "      <p> %f %f %f </p>\n",maxx,maxy,maxz;
  printf "    </bndbox>\n";
  printf "    <center visible=\"hide\">\n";
  printf "      <p> %f %f %f </p>\n",(maxx+minx)/2,(maxy+miny)/2,
             (maxz+minz)/2;
  printf "    </center>\n";
  printf "  </geometry>\n";


  colornum := 1;
  while colornum <= 16 do { edgecolorcount[colornum] := 0; colornum += 1; };
  foreach edge ee where ee.show do
   edgecolorcount[ee.color+1] += 1; 
  colornum := 1; numcolors := 0;
  while colornum <= 16 do 
  { if edgecolorcount[colornum] > 0 then numcolors += 1; colornum += 1; };
  printf "  <geometry name=\"surface1 edges\">\n";
  printf "    <pointSet dim=\"3\" point=\"hide\">\n";
  printf "      <points num=\"%d\">\n",2*sum(edges,show);
  foreach edge ee where ee.show do
  { printf "          <p> %f %f %f </p>\n",ee.vertex[1].x,ee.vertex[1].y,ee.vertex[1].z; 
    printf "          <p> %f %f %f </p>\n",ee.vertex[2].x,ee.vertex[2].y,ee.vertex[2].z; 
  };
  printf "      </points>\n";
  printf "    </pointSet>\n";
  if numcolors > 1 then 
    printf "    <lineSet color=\"show\">\n"
  else printf "    <lineSet>\n";
  printf "      <lines num=\"%d\">\n",sum(edges,show);
  jvxnum := 0;
  foreach edge ee where ee.show do
  { printf "          <l> %d %d </l>\n",jvxnum,jvxnum+1; 
    jvxnum += 2;
  };
  printf "          <thickness> 2 </thickness>\n";
  if numcolors <= 1 then 
  { colornum := 1; 
    while colornum <= 16 do
    { if edgecolorcount[colornum] > 0 then
        printf "          <color> %d %d %d </color>\n",rgb[colornum][1],
          rgb[colornum][2],rgb[colornum][3];
      colornum += 1;
    };
  };
  printf "      </lines>\n";
  if ( numcolors > 1 ) then
  {
    printf "      <colors>\n";
    foreach edge ee where ee.show do
    { printf "        <c> %d %d %d </c>\n", rgb[ee.color+1][1],
         rgb[ee.color+1][2],rgb[ee.color+1][3];
    };
    printf "      </colors>\n";
  };
  printf "    </lineSet>\n";
  printf "    <bndbox visible=\"hide\">\n";
  printf "      <p> %f %f %f </p>\n",minx,miny,minz;
  printf "      <p> %f %f %f </p>\n",maxx,maxy,maxz;
  printf "    </bndbox>\n";
  printf "    <center visible=\"hide\">\n";
  printf "      <p> %f %f %f </p>\n",(maxx+minx)/2,(maxy+miny)/2,
             (maxz+minz)/2;
  printf "    </center>\n";
  printf "  </geometry>\n";
  printf "</geometries>\n";
  printf "</jvx-model>\n";
}

  

