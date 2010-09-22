/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/


/**********************************************************
*
*  File: registry.c
*
*  Purpose: Declare lists of functions availiable for
*          symmetry groups.
*/

/****************************************************************
*                                          
*              SYMMETRY GROUP FUNCTIONS   
*                                        
* Instructions to those who wish to add a new set of symmetry  
* functions:    Write a source file (like quotient.c) with    
* the necessary symmetry functions.  Be sure your functions  
* have unique names.  Then create an additional structure in
* the array sym_functions below.  This array will be searched 
* for a name matching the symmetry group name in the datafile.
* Add the name of your source file to the files in Makefile. 
*                                         
* Registry structure: (for documentation only; actual globally 
* visible declaration is in extern.h)    
*/

#ifdef DONT_COMPILE_THIS
struct sym_registry {
     char *name; /* to be matched in datafile */
     int flags;  /* see bits below */
     void (*wrapper)(REAL *in,REAL *out,int wrap); /* point wrapper */
     int (*compose)(int wrap1,int wrap2); /* group composition */
     int (*inverse)(int wrap); /* group inverse */
     void (*pullback)(REAL *x,REAL *xform,REAL *yform,int wrap);
        /* pullback of yform to xform at x */
     } sym_register[];
/* flag bits */
/* for group with fixed points, which should be given attribute AXIAL_POINT */ 
#define HAS_FIXED_PTS 1
/* for group which has nontrivial form pullback */
#define NEED_FORM_UNWRAPPING 2
/* for group with period two points on rotation axis, which should be given attribute AXIAL_POINT */ 
#define DOUBLE_AXIAL 4
#endif
/****************************************************************/

/****************************************************************/
/* Prototypes:  For this registry to work, your functions must  */
/* be declared before the registry. So add your declarations     */
/* to the list below.                             */
/****************************************************************/

#include "include.h"

/* Declarations, using appropriate typedefs */

/* Torus, from torus.c */
extern SYM_WRAP torus_wrap;
extern WRAPTYPE torus_compose ARGS((WRAPTYPE,WRAPTYPE));
extern WRAPTYPE  torus_inverse ARGS((WRAPTYPE));
extern SYM_FORM torus_form_pullback;

/* XYZ Rotation, from quotient.c */
extern SYM_WRAP xyz_wrap;
extern WRAPTYPE xyz_compose ARGS((WRAPTYPE,WRAPTYPE));
extern WRAPTYPE xyz_inverse ARGS((WRAPTYPE));
extern SYM_FORM xyz_form_pullback;

/* Rotation, from quotient.c */
extern SYM_WRAP rot_wrap;
extern WRAPTYPE rot_compose ARGS((WRAPTYPE,WRAPTYPE));
extern WRAPTYPE rot_inverse ARGS((WRAPTYPE));
extern SYM_FORM rot_form_pullback;

/* Cube point group, from quotient.c */
extern SYM_WRAP pgcube_wrap;
extern WRAPTYPE pgcube_compose ARGS((WRAPTYPE,WRAPTYPE));
extern WRAPTYPE pgcube_inverse ARGS((WRAPTYPE));
extern SYM_FORM pgcube_form_pullback;

/* Full cube lattice group, from quotient.c */
extern SYM_WRAP cubel_wrap;
extern WRAPTYPE cubel_compose ARGS((WRAPTYPE,WRAPTYPE));
extern WRAPTYPE cubel_inverse ARGS((WRAPTYPE));
extern SYM_FORM cubel_form_pullback;

/* Flip-Rotation, from quotient.c */
extern SYM_WRAP frot_wrap;
extern SYM_FORM frot_form_pullback;

/* Central symmetry group, from quotient.c */
extern SYM_WRAP central_wrap;
extern WRAPTYPE central_compose ARGS((WRAPTYPE,WRAPTYPE));
extern WRAPTYPE central_inverse ARGS((WRAPTYPE));
extern SYM_FORM central_form_pullback;

/* Genus 2 hyperbolic space, Klein model, from khyp.c */
extern SYM_WRAP khyp_wrap;
extern WRAPTYPE khyp_compose ARGS((WRAPTYPE,WRAPTYPE));
extern WRAPTYPE  khyp_inverse ARGS((WRAPTYPE));
extern SYM_FORM khyp_form_pullback;

/* hyperbolic space, Klein model, from dodecGroup.c (Ken Bromberg) */
extern SYM_WRAP dodec_wrap;
extern WRAPTYPE dodec_compose ARGS((WRAPTYPE,WRAPTYPE));
extern WRAPTYPE  dodec_inverse ARGS((WRAPTYPE));
extern SYM_FORM dodec_form_pullback;

/* screw rotation, from quotient.c  */
extern SYM_WRAP screw_wrap;
extern WRAPTYPE screw_compose ARGS((WRAPTYPE,WRAPTYPE));
extern WRAPTYPE  screw_inverse ARGS((WRAPTYPE));
extern SYM_FORM screw_form_pullback;

/* quarter_turn torus, from quotient.c  */
extern SYM_WRAP quarter_turn_wrap;
extern WRAPTYPE quarter_turn_compose ARGS((WRAPTYPE,WRAPTYPE));
extern WRAPTYPE quarter_turn_inverse ARGS((WRAPTYPE));
extern SYM_FORM quarter_turn_form_pullback;

/* symmetry function registry array */

 struct sym_registry sym_register[] = {
    {"torus",0,torus_wrap,torus_compose,torus_inverse,torus_form_pullback},
    {"genus2",NEED_FORM_UNWRAPPING,
     khyp_wrap,khyp_compose,khyp_inverse,khyp_form_pullback},
    {"dodecahedron",NEED_FORM_UNWRAPPING,
     dodec_wrap,dodec_compose,dodec_inverse,dodec_form_pullback},
    {"xyz",HAS_FIXED_PTS|NEED_FORM_UNWRAPPING,
     xyz_wrap,xyz_compose,xyz_inverse,xyz_form_pullback},
    {"rotate",HAS_FIXED_PTS|NEED_FORM_UNWRAPPING,
     rot_wrap,rot_compose,rot_inverse,rot_form_pullback},
    {"cubocta",HAS_FIXED_PTS|NEED_FORM_UNWRAPPING,
     pgcube_wrap,pgcube_compose,pgcube_inverse,pgcube_form_pullback},
    {"cubelat",HAS_FIXED_PTS|NEED_FORM_UNWRAPPING,
     cubel_wrap,cubel_compose,cubel_inverse,cubel_form_pullback},
    {"flip_rotate",HAS_FIXED_PTS|DOUBLE_AXIAL|NEED_FORM_UNWRAPPING,
     frot_wrap,rot_compose,rot_inverse,frot_form_pullback},
    {"central_symmetry",HAS_FIXED_PTS|NEED_FORM_UNWRAPPING,
     central_wrap,central_compose,central_inverse,central_form_pullback},
    {"screw_symmetry",HAS_FIXED_PTS|NEED_FORM_UNWRAPPING,
     screw_wrap,screw_compose,screw_inverse,screw_form_pullback},
    {"quarter_turn",NEED_FORM_UNWRAPPING,
     quarter_turn_wrap,quarter_turn_compose,quarter_turn_inverse,
     quarter_turn_form_pullback},
    {NULL,0,NULL,NULL,NULL,NULL}  /* NULL name ends search */
    };



