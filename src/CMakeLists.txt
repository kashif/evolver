find_package (OpenGL)
find_package (GLUT)
find_package (Threads)

include_directories (
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${GLUT_INCLUDE_DIR}
  ${OPENGL_INCLUDE_DIR}
)

set (targetName evolver)

if (APPLE)
  add_definitions(-DLINUX -DPTHREADS -DOOGL -DMAC_OS_X)
endif (APPLE)

if (CYGWIN)
  add_definitions(-DLINUX -DOOGL -DPTHREADS)
endif(CYGWIN)

if (UNIX)
  add_definitions(-DLINUX -DOOGL -DPTHREADS)
endif (UNIX)

add_executable (evolver
  alice.c
  alloca.c
  bk.c
  boundary.c
  calcforc.c
  check.c
  cnstrnt.c
  command.c
  curtest.c
  diffuse.c
  display.c
  dodecGroup.c
  dump.c
  eval_all.c
  eval_sec.c
  evalmore.c
  evaltree.c
  exprint.c
  filgraph.c
  filml.c
  filmq.c
  fixvol.c
  gauss.c
  geomgraph.c
  glutgraph.c
  grapher.c
  graphgen.c
  help.c
  hessian.c
  hessian2.c
  hessian3.c
  hidim.c
  iterate.c
  khyp.c
  klein.c
  knot1.c
  knot2.c
  knot3.c
  kusner.c
  lagrange.c
  lexinit.c
  lexinit2.c
  lexyy.c
  machine.c
  matrix.c
  meanint.c
  method1.c
  method2.c
  method3.c
  method4.c
  method5.c
  metis.c
  metric.c
  mindeg.c
  model.c
  modify.c
  mvgraph.c
  odrv.c
  painter.c
  pixgraph.c
  popfilm.c
  psgraph.c
  quantity.c
  query.c
  quotient.c
  readline.c
  registry.c
  sdrv.c
  simequi2.c
  simplex.c
  skeleton.c
  softimag.c
  sqcurve.c
  sqcurve2.c
  sqcurve3.c
  storage.c
  stringl.c
  stringq.c
  symtable.c
  teix.c
  tmain.c
  tordup.c
  torus.c
  torvol.c
  trirevis.c
  userfunc.c
  userio.c
  utility.c
  variable.c
  veravg.c
  verpopst.c
  wulff.c
  yexparse.c
  ytab.c
  zoom.c
)

target_link_libraries (evolver
  ${CMAKE_THREAD_LIBS_INIT}
  ${GLUT_LIBRARIES}
  ${OPENGL_LIBRARIES}
)
