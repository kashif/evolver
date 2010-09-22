# Makefile for Surface Evolver
SHELL= /bin/sh

# INSTRUCTIONS: Customize this makefile for your system by choosing
# your compiler (cc or gcc or whatever), and then uncomment the
# appropriate CFLAGS, GRAPH, and GRAPHLIB definitions for your system.
# If lines need to be modified for your system, please let me know
# at brakke@susqu.edu.

# The following systems have their own sections:
# Generic Unix 
# Linux OpenGL
# Linux Xwindows 
# cygwin 
# Mac OSX
# Sun Xwindows 
# 32-bint Solaris OpenGL 
# 64-bint Solaris OpenGL

# NOTE: -DOOGL should be specified in CFLAGS if you want Evolver to 
# be able to use geomview.  And -DOOGL is harmless in any case. 

# NOTE: You can use the readline input editing system (if your system has it)
# by adding -DUSE_READLINE to CFLAGS and  -lreadline -lcurses to GRAPHLIB.
# But beware; readline may be incompatible with using geomview.
# Also, the readline code was contributed by a user for version 2.30,
# and may be incompatible with readline libraries on some systems (Mac,
# for instance).

#-----------------------------------------------------------------------------

# Pick your compiler, and add any options you want, such as optimization
CC= cc 
#CC= gcc

#---------------------Start of system choices---------------------------------

# Remove #'s from following 3 lines for generic Unix without builtin graphics.
# Add -DOOGL to CFLAGS if you are using geomview.
#CFLAGS= -DGENERIC
#GRAPH= nulgraph.o 
#GRAPHLIB= 

#---- Generic Unix -----------------------------------------------------------

# Remove #'s from following 3 lines for generic Unix with X-windows. 
# Add -DOOGL to CFLAGS if you are using geomview.
# You might have to add something like -I/usr/X11R6/include to CFLAGS if 
# there is a problem finding Xlib.h while compiling xgraph.c, and add
# -L/usr/X11R6/lib to GRAPHLIB.
#CFLAGS= -DGENERIC
#GRAPH= xgraph.o 
#GRAPHLIB= -lX11 
#some places might have -lX11-mit

#----- Linux OpenGL ----------------------------------------------------------

# Remove #'s from following 3 lines for LINUX with OpenGL GLUT graphics.
# The graphics are on a second thread, so pthreads are needed.
# NOTE: -DPTRHEADS is necessary with glutgraph.o.
#CFLAGS= -DLINUX -DOOGL -DPTHREADS
#GRAPH= glutgraph.o 
#GRAPHLIB= -lGL -lGLU -lglut -lpthread
# NOTE: It has been reported to me that RedHat 9 needs the following line
# for GRAPHLIB, but earlier and later versions do okay with the GRAPHLIB
# line just above.
#GRAPHLIB= -lGL -lGLU -lglut -L/usr/X11R6/lib -lXi -lXmu -lpthread

#------ Linux Xwindows ------------------------------------------------------

# Remove #'s from following 3 lines for LINUX with crummy Xwindows graphics.
# You may have to modify the X11 lib path given here.
#CFLAGS= -DLINUX -DOOGL 
#GRAPH= xgraph.o 
#GRAPHLIB= -L/usr/X11R6/lib -lX11 

#-------- cygwin --------------------------------------------------------------

# Remove #'s from the following three lines for cygwin.
#CFLAGS= -DLINUX -DOOGL -DPTHREADS
#GRAPH= glutgraph.o 
#GRAPHLIB=   -lglut -lglu32 -lopengl32 -lpthread

#------- Mac OSX -------------------------------------------------------------

# Remove #'s from following 5 lines for MAC OSX with OpenGL GLUT graphics.
#INC=/System/Library/Frameworks/GLUT.framework/Versions/A/Headers
#CFLAGS= -DLINUX -DPTHREADS -DOOGL -DMAC_OS_X -I$(INC)
#GRAPH= glutgraph.o
#GLDIR=/System/Library/Frameworks/OpenGL.framework/Libraries
#GRAPHLIB= -L$(GLDIR)  -lGL -lGLU -lobjc  -framework GLUT

#------- Sun Xwindows --------------------------------------------------------

# Remove #'s from following 3 lines for SUNs or SPARCSTATIONs with X-windows
# WARNING: If you use -O2 optimization, you may have to compile popfilm.c
# without optimization, because sun optimization may hang.
#CFLAGS= -DSUN -DOOGL -I/usr/openwin/include
#GRAPH= xgraph.o 
#GRAPHLIB= -L/usr/openwin/lib  -lX11 

#------- 32-bint Solaris OpenGL ------------------------------------------------------

# 32-bit SOLARIS with GLUT.  The GLUT graphics window runs in a separate
# thread;  -DPTHREADS enables threads in Evolver source code, and -mt
# enables it in the Solaris cc compiler.  For the gcc compiler, use -pthreads 
# instead of -mt.
# You should check to see that the GLUTHOME directory listed below exists;
# it may be some other place on your machine.  If you can't find it,
# download glut-3.7b.sparc_solaris.tar.gz from www.sun.com.
#GLUTHOME= /usr/local/sparc_solaris/glut-3.7
#INCLUDE= -I/usr/openwin/include -I$(GLUTHOME)/include
#CFLAGS= -DSUN -DOOGL -DGLUT -DPTHREADS -mt $(INCLUDE)
#GRAPH= glutgraph.o 
#GRAPHLIB= -L/usr/openwin/lib  -lX11 -L$(GLUTHOME)/lib/glut -lglut -lGL -lGLU -lXmu

#------- 64-bint Solaris OpenGL ------------------------------------------------------

# 64-bit SOLARIS with GLUT.  Also see 32 bit comments above.
# You should check to see that the GLUTHOME directory listed below exists;
# it may be some other place on your machine.  If you can't find it,
# download glut-3.7b.sparc_solaris_64.tar.gz from www.sun.com.
#GLUTHOME= /usr/local/sparc_solaris_64bit/glut-3.7
#INCLUDE= -I/usr/openwin/include -I$(GLUTHOME)/include
#CFLAGS= -DSUN -DOOGL -DGLUT -DPTHREADS -mt $(INCLUDE) -m64 
#GRAPH= glutgraph.o 
#GRAPHLIB= -L/usr/openwin/lib  -lX11 -L$(GLUTHOME)/lib/glut -lglut -lGL -lGLU -lXmu

#Note on Solaris:
#SOLARISFLAGS= -L/usr/ucblib -lucb -R/usr/ucblib -lnsl -lsocket
#are needed for some programs on Solaris; but not this one -- they're poison
#for Evolver!

#-----------------------------------------------------------------------------

# Remove #'s from following 3 lines for DEC Alpha with X-windows 
# Add -DOOGL to CFLAGS if you have the X version of geomview.
#CFLAGS= -DDECALPHA -O -Olimit 2000
#GRAPH= xgraph.o
#GRAPHLIB= -lX11

#-----------------------------------------------------------------------------

# IRIX 4, with old gl graphics
# Remove #'s from the following three lines for Iris workstations with 
# geomview.  Those with R4000 CPUs or above may add -mips2 to CFLAGS.
#CFLAGS = -DIRIS -DOOGL -O2 -Olimit 3000 
#GRAPH= iriszgraph.o
#GRAPHLIB=  -lgl_s -lmalloc -lc_s

#-----------------------------------------------------------------------------

# IRIX 5
# Remove #'s from the following three lines for Iris workstations with 
# geomview.  Those with R4000 CPUs or above may add -mips2 to CFLAGS.
#CFLAGS = -DIRIS -DOOGL -O2 -Olimit 3000 
#GRAPH= xgraph.o
#GRAPHLIB=   -lmalloc -lX11

#-----------------------------------------------------------------------------

# IRIX 5, parallel
# Remove #'s from the following three lines for Iris workstations with 
# multiple processors and geomview.  OK for single processors, too.
# Those with R4000 CPUs or above may add -mips2 to CFLAGS.
#CFLAGS = -DIRIS -DOOGL -DSGI_MULTI -O2 -Olimit 3000 -mips2
#GRAPH= xgraph.o
#GRAPHLIB=   -lmalloc -lX11

#-----------------------------------------------------------------------------

# IRIX 6
# Remove #'s from the following three lines for Iris workstations with 
# IRIX 6.x operating system.  Add -DSGI_MULTI to CLFAGS if you
# have multiple processors.
#CFLAGS = -DIRIS -DOOGL -O2 -OPT:Olimit=10000
#GRAPH= xgraph.o
#GRAPHLIB=   -lmalloc -lX11

#-----------------------------------------------------------------------------


# Remove #'s from following 3 lines for  HP workstation
# Omit -Aa if you're using gcc
#CFLAGS= -Aa  -D_HPUX_SOURCE  -I/usr/include/X11R5
#GRAPH= xgraph.o
#GRAPHLIB= -L/usr/lib/X11R5 -lX11

#-----------------------------------------------------------------------------

# Remove #'s from following 3 lines for NeXTStep without screen graphics
# (see separate ftp archive evolver.next.tar.Z for graphics version)
#CFLAGS= -DNeXT
#GRAPH= nulgraph.o 
#GRAPHLIB= 

#-------------------End of system-specific options----------------------------

OBJ= calcforc.o  variable.o trirevis.o  stringl.o stringq.o model.o\
	 fixvol.o query.o matrix.o grapher.o painter.o filml.o filmq.o\
	 torvol.o lexinit.o graphgen.o modify.o userio.o boundary.o\
         curtest.o display.o yexparse.o lexyy.o ytab.o hessian.o\
	 evaltree.o cnstrnt.o verpopst.o popfilm.o machine.o veravg.o \
	 pixgraph.o tmain.o  tordup.o wulff.o help.o psgraph.o check.o  \
	 utility.o skeleton.o  storage.o dump.o iterate.o filgraph.o zoom.o\
         softimag.o mvgraph.o  diffuse.o sqcurve.o klein.o\
	 command.o hidim.o simplex.o metric.o torus.o quotient.o alice.o\
	 sdrv.o odrv.o userfunc.o kusner.o  simequi2.o\
	 geomgraph.o symtable.o exprint.o quantity.o meanint.o mindeg.o\
	 dodecGroup.o registry.o khyp.o gauss.o knot1.o eval_all.o\
	 lexinit2.o evalmore.o knot2.o knot3.o teix.o sqcurve2.o\
	 hessian2.o hessian3.o method1.o method2.o method3.o  bk.o\
	 method4.o method5.o eval_sec.o sqcurve3.o metis.o lagrange.o


evolver: makemark  $(OBJ)  $(GRAPH)
	$(CC) $(CFLAGS)  $(OBJ) $(GRAPH) $(GRAPHLIB) -o evolver -lm   

# This is to get global dependencies on the main header files.
makemark: skeleton.h storage.h model.h web.h
	if [[ -z "$(GRAPH)" ]] ; then (echo "ERROR: You need to uncomment your system's lines in Makefile.") ; fi
	rm *.o || true
	touch makemark

.c.o:
	$(CC) $(CFLAGS) -c  $<
 

# lexyy.c and ytab.c should only be remade when interface
# language is changed, which users shouldn't be touching.
#lexyy.c: datafile.lex express.h ytab.c lex.h
#	lex datafile.lex
#	sed '/#ident/d' <lex.yy.c >t.c
#	sed '/int \* p, int m/s/(int \* p, int m)/(p,m) int *p,m;/' <t.c >lexyy.c
#	rm lex.yy.c

#ytab.c:  command.yac  express.h
#	yacc -d command.yac
#	sed '/#ident/d' <y.tab.c >tmp.c
#	sed '/malloc()/d' <tmp.c >ytab.c
#	rm tmp.c
#	cp y.tab.h ytab.h
#	rm y.tab.c y.tab.h

ytab.h: ytab.c 

lexinit.o: lex.h lexinit.c express.h ytab.h

query.o: lex.h ytab.h query.c

evaltree.o: evaltree.c lex.h ytab.h express.h

eval_all.o: eval_all.c lex.h ytab.h express.h

eval_sec.o: eval_all.c lex.h ytab.h express.h

yexparse.o: yexparse.c lex.h ytab.h express.h

exprint.o: exprint.c lex.h ytab.h express.h

tmain.o: tmain.c include.h


