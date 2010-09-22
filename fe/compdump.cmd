// compdump.cmd
// command for dumping datafile directly to compressed form
// 

dumpcmd := sprintf "compress >%s.dmp.Z",datafilename
dodump := { list topinfo;
	    print "\nvertices\n"; list vertices; 
	    print "\nedges\n"; list edges; 
	    print "\nfaces\n"; list facets; 
	    print "\nbodies\n"; list bodies;
	    list bottominfo }
compdump := { print dumpcmd;  dodump | dumpcmd }

