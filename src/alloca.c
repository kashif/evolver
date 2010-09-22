/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/


/**********************************************************
*
*  File: alloca.c
*
*  Contents: Kludge substitute for alloca() for those 
*            systems that do not have it. alloca() is
*            an old unix function that allocates memory
*            from the stack. The version of YACC I use
*            uses alloca in case of parsing stack overflow,
*            so alloca should almost never be called.
*            Link with this file only if the linker
*            complains about not finding alloca.
*            This file simply substitutes a call to
*            malloc(), so represents a potential
*            memory leak.
*/

#include "include.h"

void * alloca(size_t size) 
{ return (void*)malloc(size); 
}
