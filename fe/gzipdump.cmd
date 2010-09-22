// gzipdump.cmd
// Evolver command for dumping datafile directly to gzip form
// Needs Evolver version 1.99e or later.

// Usage: gzipdump
// As is, this will dump to a gzipped file with a default name.
// You may change the filename in the command below to your own liking.

// gzipped file may be reloaded directly with shell command like
//   /etc/mknod epipe p; ((gunzip <cube.dmp.gz >epipe;rm epipe)& evolver epipe)

// I suggest defining a csh alias like 
// alias gzev '/etc/mknod epipe p;((gzip -d < \!$ >epipe;rm epipe)& evolver \!:1- epipe)'
// so you can just do
//   gzev cube.dmp.gz
// The alias even works with options, as in 
//   gzev -q -p2 cube.dmp.gz

dodump := { 
        list topinfo; 
	    print "\nvertices\n"; list vertices; 
	    print "\nedges\n"; list edges; 
	    print "\nfaces\n"; list facets; 
	    print "\nbodies\n"; list bodies;
	    list bottominfo 
}

gzipdump := { 
   filename := sprintf "%s.dmp.gz",datafilename;
   dumpcmd := sprintf "gzip >%s",filename;
   printf "Dumping to %s\n",filename;
   dodump | dumpcmd 
}

