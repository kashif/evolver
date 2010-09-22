/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/************************************************************
*
*  File:  lex.h
*
*  Contents: Definitions for lexical analyzer for reading
*        data files.
*/

#ifndef _LEX_H_
#define _LEX_H_

/* Data type for yylval and parse semantics stack. */
typedef struct { int i;   /* standard integer yylval */
                 int qnum;
                 REAL r;
                 int etype;  /* element type */
                 int datatype;  /* expression datatype */ 
                 struct sym *symptr; /* iteration variable names */
                 char lexeme[32]; /* for identifiers */
              } yystype;
#define YYSTYPE yystype
extern YYSTYPE yylval;  /* parser terminal value */

#endif
