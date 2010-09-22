/* A Bison parser, made by GNU Bison 2.1.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* Written by Richard Stallman by simplifying the original so called
   ``semantic'' parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     EXPRESSION_START_ = 258,
     COMMAND_START_ = 259,
     HISTORY_ = 260,
     GEOMVIEW_ = 261,
     VIEW_MATRIX_ = 262,
     LEAD_INTEGER_ = 263,
     INTEGER_ = 264,
     REAL_ = 265,
     SIGNED_NUMBER_ = 266,
     NEWIDENT_ = 267,
     REDEFINE_ = 268,
     MATHFUNC_ = 269,
     MATHFUNC2_ = 270,
     POW_ = 271,
     USERFUNC_ = 272,
     MIDV_ = 273,
     DATAFILENAME_ = 274,
     LOGFILE_ = 275,
     PI_ = 276,
     E_ = 277,
     G_ = 278,
     PARAM_ = 279,
     SYMBOL_ = 280,
     TOTAL_ = 281,
     EXTRA_ATTRIBUTE_ = 282,
     FIXEDVOL_ = 283,
     IDENT_ = 284,
     UMINUS_ = 285,
     SHELL_ = 286,
     COLOR_ = 287,
     HESSIAN_ = 288,
     VOLCONST_ = 289,
     TORUS_PERIODS_ = 290,
     VERTICES_ = 291,
     EDGES_ = 292,
     FACETS_ = 293,
     BODIES_ = 294,
     HESSIAN_MENU_ = 295,
     POSTSCRIPT_ = 296,
     LENGTH_ = 297,
     AREA_ = 298,
     VOLUME_ = 299,
     ID_ = 300,
     OID_ = 301,
     TAG_ = 302,
     ORIGINAL_ = 303,
     FACETEDGES_ = 304,
     WRAP_ = 305,
     QUOTATION_ = 306,
     UNSET_ = 307,
     TOPINFO_ = 308,
     OPACITY_ = 309,
     VALENCE_ = 310,
     HESSIAN_SADDLE_ = 311,
     SET_ = 312,
     FIXED_ = 313,
     DENSITY_ = 314,
     PRESSURE_ = 315,
     CONSTRAINT_ = 316,
     COORD_ = 317,
     DISSOLVE_ = 318,
     WHERE_ = 319,
     LIST_ = 320,
     SHOW_ = 321,
     DELETE_ = 322,
     REFINE_ = 323,
     RECALC_ = 324,
     SHOWQ_ = 325,
     EDGESWAP_ = 326,
     FIX_ = 327,
     UNFIX_ = 328,
     TOGGLENAME_ = 329,
     TOGGLEVALUE_ = 330,
     STAR_ = 331,
     QUANTITY_NAME_ = 332,
     PAUSE_ = 333,
     GO_ = 334,
     SHOW_VOL_ = 335,
     CHECK_ = 336,
     READ_ = 337,
     ZOOM_ = 338,
     ON_ = 339,
     OFF_ = 340,
     GEOMPIPE_ = 341,
     SELF_ = 342,
     SINGLE_LETTER_ = 343,
     LONG_JIGGLE_ = 344,
     RAW_VERAVG_ = 345,
     COUNTS_ = 346,
     CHDIR_ = 347,
     ALICE_ = 348,
     STABILITY_TEST_ = 349,
     DEFINE_ = 350,
     UPLUS_ = 351,
     DATATYPE_ = 352,
     FLUSH_COUNTS_ = 353,
     AUTOCHOP_ = 354,
     UTEST_ = 355,
     ATTRIBUTE_ = 356,
     RITZ_ = 357,
     MOVE_ = 358,
     VERTEXNORMAL_ = 359,
     POP_ = 360,
     SYSTEM_ = 361,
     TETRA_POINT_ = 362,
     TRIPLE_POINT_ = 363,
     LANCZOS_ = 364,
     EIGENPROBE_ = 365,
     EXEC_ = 366,
     AREAWEED_ = 367,
     EDGEWEED_ = 368,
     GRAVITY_ = 369,
     EDGEDIVIDE_ = 370,
     LINEAR_ = 371,
     QUADRATIC_ = 372,
     DIFFUSION_ = 373,
     EXTRAPOLATE_ = 374,
     TRANSFORM_DEPTH_ = 375,
     PRINTF_ = 376,
     ERRPRINTF_ = 377,
     PRINT_ = 378,
     MAX_ = 379,
     MIN_ = 380,
     COUNT_ = 381,
     SUM_ = 382,
     AVG_ = 383,
     BREAK_ = 384,
     CONTINUE_ = 385,
     SIZEOF_ = 386,
     TRANSFORM_EXPR_ = 387,
     BARE_ = 388,
     BOTTOMINFO_ = 389,
     METIS_ = 390,
     KMETIS_ = 391,
     KEYLOGFILE_ = 392,
     SCALE_ = 393,
     BURCHARD_ = 394,
     REBODY_ = 395,
     BOUNDARY_ = 396,
     ORIENTATION_ = 397,
     OMETIS_ = 398,
     SQ_MEAN_CURV_ = 399,
     FRONTCOLOR_ = 400,
     SINGLE_REDEFD_ = 401,
     METHOD_NAME_ = 402,
     TASK_EXEC_ = 403,
     RAWEST_VERAVG_ = 404,
     SINGLE_LETTER_ARG_ = 405,
     BACKCOLOR_ = 406,
     LAGRANGE_ = 407,
     RETURN_ = 408,
     TRANSFORM_EXPR_VERB_ = 409,
     OOGLFILE_ = 410,
     PARALLEL_EXEC_ = 411,
     BINARY_OFF_FILE_ = 412,
     SPRINTF_ = 413,
     CONVERT_TO_QUANTS_ = 414,
     METIS_FACTOR_ = 415,
     FUNCTION_ = 416,
     EXPRINT_ = 417,
     DIHEDRAL_ = 418,
     WRAP_VERTEX_ = 419,
     ARRAYIDENT_ = 420,
     DATE_AND_TIME_ = 421,
     LOCAL_ = 422,
     SHOW_EXPR_ = 423,
     SHOW_TRANS_ = 424,
     AXIAL_POINT_ = 425,
     ENERGY_ = 426,
     CONSERVED_ = 427,
     INFO_ONLY_ = 428,
     ASSIGN_ = 429,
     PROCEDURE_ = 430,
     FOREACH_ = 431,
     STRINGGLOBAL_ = 432,
     EQUIANGULATE_ = 433,
     HISTOGRAM_ = 434,
     LOGHISTOGRAM_ = 435,
     AREA_FIXED_ = 436,
     QUIT_ = 437,
     WARNING_MESSAGES_ = 438,
     IF_ = 439,
     WHILE_ = 440,
     DO_ = 441,
     NO_REFINE_ = 442,
     STRING_ = 443,
     NONCONTENT_ = 444,
     FOR_ = 445,
     HIT_PARTNER_ = 446,
     FRONTBODY_ = 447,
     BACKBODY_ = 448,
     COLORFILE_ = 449,
     PERM_STRINGGLOBAL_ = 450,
     FUNCTION_IDENT_ = 451,
     THICKEN_ = 452,
     COLORMAP_ = 453,
     REDIRECT_ = 454,
     NEWVERTEX_ = 455,
     NEWEDGE_ = 456,
     NEWFACET_ = 457,
     MODULUS_ = 458,
     TARGET_ = 459,
     VALUE_ = 460,
     INVERSE_PERIODS_ = 461,
     NEWBODY_ = 462,
     DELTA_ = 463,
     GAP_CONSTANT_ = 464,
     DUMP_ = 465,
     NOTCH_ = 466,
     QUANTITY_ = 467,
     LOAD_ = 468,
     PERM_PROCEDURE_ = 469,
     PROCEDURE_WORD_ = 470,
     DYNAMIC_LOAD_FUNC_ = 471,
     PERM_IDENT_ = 472,
     PERMLOAD_ = 473,
     HELP_ = 474,
     VERTEX_AVERAGE_ = 475,
     METHOD_INSTANCE_ = 476,
     RAW_VERTEX_AVERAGE_ = 477,
     OPTIMIZE_ = 478,
     REDIRECTOVER_ = 479,
     TOLERANCE_ = 480,
     RAWEST_VERTEX_AVERAGE_ = 481,
     JIGGLE_ = 482,
     VIEW_TRANSFORMS_ = 483,
     CLOSE_SHOW_ = 484,
     IS_DEFINED_ = 485,
     NODISPLAY_ = 486,
     PERM_ASSIGN_ = 487,
     PHASE_ = 488,
     VIEW_TRANSFORM_SWAP_COLORS_ = 489,
     BACKQUOTE_COMMA_ = 490,
     INTERNAL_VARIABLE_ = 491,
     DIRICHLET_ = 492,
     SOBOLEV_ = 493,
     VIEW_TRANSFORM_PARITY_ = 494,
     SOBOLEV_SEEK_ = 495,
     DIRICHLET_SEEK_ = 496,
     HESSIAN_SEEK_ = 497,
     REORDER_STORAGE_ = 498,
     RENUMBER_ALL_ = 499,
     CONSTRAINT_NAME_ = 500,
     BOUNDARY_NAME_ = 501,
     PROCEDURE_IDENT_ = 502,
     POP_TRI_TO_EDGE_ = 503,
     POP_EDGE_TO_TRI_ = 504,
     POP_QUAD_TO_QUAD_ = 505,
     SHOWVERB_ = 506,
     PROCEDURES_ = 507,
     MPI_TASK_ATTR_ = 508,
     T1_EDGESWAP_ = 509,
     MERGE_EDGE_ = 510,
     MERGE_FACET_ = 511,
     MERGE_VERTEX_ = 512,
     RESET_COUNTS_ = 513,
     VALID_ELEMENT_ = 514,
     MID_EDGE_ = 515,
     MID_FACET_ = 516,
     GO_COUNT_ = 517,
     ELEMENT_IDENT_ = 518,
     BODY_METIS_ = 519,
     REVERSE_ORIENTATION_ = 520,
     MATRIX_MULTIPLY_ = 521,
     MATRIX_INVERSE_ = 522,
     BINARY_PRINTF_ = 523,
     DUMP_MEMLIST_ = 524,
     FREE_DISCARDS_ = 525,
     REPARTITION_ = 526,
     METIS_READJUST_ = 527,
     MEAN_CURVATURE_ = 528,
     GLOBAL_ = 529,
     LEAD_INTEGER_AT_ = 530,
     INTEGER_AT_ = 531,
     MATRIX_DETERMINANT_ = 532,
     SUBCOMMAND_ = 533,
     ABORT_ = 534,
     BREAKPOINT_ = 535,
     WHEREAMI_ = 536,
     ADDLOAD_ = 537,
     SIMPLEX_TO_FE_ = 538,
     DISPLAY_TEXT_ = 539,
     DELETE_TEXT_ = 540,
     SUPPRESS_WARNING_ = 541,
     UNSUPPRESS_WARNING_ = 542,
     RESET_PROFILING_ = 543,
     VALID_CONSTRAINT_ = 544,
     VALID_BOUNDARY_ = 545,
     ARRAY_ATTRIBUTE_ = 546,
     PROFILING_ = 547,
     ASSIGNOP_ = 548,
     PIPE_ = 549,
     THEN_ = 550,
     ELSE_ = 551,
     OR_ = 552,
     AND_ = 553,
     NOT_ = 554,
     NE_ = 555,
     GE_ = 556,
     LE_ = 557,
     EQ_ = 558,
     ON_CONSTRAINT_ = 559,
     HIT_CONSTRAINT_ = 560,
     ON_BOUNDARY_ = 561,
     ON_QUANTITY_ = 562,
     ON_METHOD_INSTANCE_ = 563,
     DOT_ = 564,
     IDIV_ = 565,
     IMOD_ = 566,
     EPRINT_ = 567
   };
#endif
/* Tokens.  */
#define EXPRESSION_START_ 258
#define COMMAND_START_ 259
#define HISTORY_ 260
#define GEOMVIEW_ 261
#define VIEW_MATRIX_ 262
#define LEAD_INTEGER_ 263
#define INTEGER_ 264
#define REAL_ 265
#define SIGNED_NUMBER_ 266
#define NEWIDENT_ 267
#define REDEFINE_ 268
#define MATHFUNC_ 269
#define MATHFUNC2_ 270
#define POW_ 271
#define USERFUNC_ 272
#define MIDV_ 273
#define DATAFILENAME_ 274
#define LOGFILE_ 275
#define PI_ 276
#define E_ 277
#define G_ 278
#define PARAM_ 279
#define SYMBOL_ 280
#define TOTAL_ 281
#define EXTRA_ATTRIBUTE_ 282
#define FIXEDVOL_ 283
#define IDENT_ 284
#define UMINUS_ 285
#define SHELL_ 286
#define COLOR_ 287
#define HESSIAN_ 288
#define VOLCONST_ 289
#define TORUS_PERIODS_ 290
#define VERTICES_ 291
#define EDGES_ 292
#define FACETS_ 293
#define BODIES_ 294
#define HESSIAN_MENU_ 295
#define POSTSCRIPT_ 296
#define LENGTH_ 297
#define AREA_ 298
#define VOLUME_ 299
#define ID_ 300
#define OID_ 301
#define TAG_ 302
#define ORIGINAL_ 303
#define FACETEDGES_ 304
#define WRAP_ 305
#define QUOTATION_ 306
#define UNSET_ 307
#define TOPINFO_ 308
#define OPACITY_ 309
#define VALENCE_ 310
#define HESSIAN_SADDLE_ 311
#define SET_ 312
#define FIXED_ 313
#define DENSITY_ 314
#define PRESSURE_ 315
#define CONSTRAINT_ 316
#define COORD_ 317
#define DISSOLVE_ 318
#define WHERE_ 319
#define LIST_ 320
#define SHOW_ 321
#define DELETE_ 322
#define REFINE_ 323
#define RECALC_ 324
#define SHOWQ_ 325
#define EDGESWAP_ 326
#define FIX_ 327
#define UNFIX_ 328
#define TOGGLENAME_ 329
#define TOGGLEVALUE_ 330
#define STAR_ 331
#define QUANTITY_NAME_ 332
#define PAUSE_ 333
#define GO_ 334
#define SHOW_VOL_ 335
#define CHECK_ 336
#define READ_ 337
#define ZOOM_ 338
#define ON_ 339
#define OFF_ 340
#define GEOMPIPE_ 341
#define SELF_ 342
#define SINGLE_LETTER_ 343
#define LONG_JIGGLE_ 344
#define RAW_VERAVG_ 345
#define COUNTS_ 346
#define CHDIR_ 347
#define ALICE_ 348
#define STABILITY_TEST_ 349
#define DEFINE_ 350
#define UPLUS_ 351
#define DATATYPE_ 352
#define FLUSH_COUNTS_ 353
#define AUTOCHOP_ 354
#define UTEST_ 355
#define ATTRIBUTE_ 356
#define RITZ_ 357
#define MOVE_ 358
#define VERTEXNORMAL_ 359
#define POP_ 360
#define SYSTEM_ 361
#define TETRA_POINT_ 362
#define TRIPLE_POINT_ 363
#define LANCZOS_ 364
#define EIGENPROBE_ 365
#define EXEC_ 366
#define AREAWEED_ 367
#define EDGEWEED_ 368
#define GRAVITY_ 369
#define EDGEDIVIDE_ 370
#define LINEAR_ 371
#define QUADRATIC_ 372
#define DIFFUSION_ 373
#define EXTRAPOLATE_ 374
#define TRANSFORM_DEPTH_ 375
#define PRINTF_ 376
#define ERRPRINTF_ 377
#define PRINT_ 378
#define MAX_ 379
#define MIN_ 380
#define COUNT_ 381
#define SUM_ 382
#define AVG_ 383
#define BREAK_ 384
#define CONTINUE_ 385
#define SIZEOF_ 386
#define TRANSFORM_EXPR_ 387
#define BARE_ 388
#define BOTTOMINFO_ 389
#define METIS_ 390
#define KMETIS_ 391
#define KEYLOGFILE_ 392
#define SCALE_ 393
#define BURCHARD_ 394
#define REBODY_ 395
#define BOUNDARY_ 396
#define ORIENTATION_ 397
#define OMETIS_ 398
#define SQ_MEAN_CURV_ 399
#define FRONTCOLOR_ 400
#define SINGLE_REDEFD_ 401
#define METHOD_NAME_ 402
#define TASK_EXEC_ 403
#define RAWEST_VERAVG_ 404
#define SINGLE_LETTER_ARG_ 405
#define BACKCOLOR_ 406
#define LAGRANGE_ 407
#define RETURN_ 408
#define TRANSFORM_EXPR_VERB_ 409
#define OOGLFILE_ 410
#define PARALLEL_EXEC_ 411
#define BINARY_OFF_FILE_ 412
#define SPRINTF_ 413
#define CONVERT_TO_QUANTS_ 414
#define METIS_FACTOR_ 415
#define FUNCTION_ 416
#define EXPRINT_ 417
#define DIHEDRAL_ 418
#define WRAP_VERTEX_ 419
#define ARRAYIDENT_ 420
#define DATE_AND_TIME_ 421
#define LOCAL_ 422
#define SHOW_EXPR_ 423
#define SHOW_TRANS_ 424
#define AXIAL_POINT_ 425
#define ENERGY_ 426
#define CONSERVED_ 427
#define INFO_ONLY_ 428
#define ASSIGN_ 429
#define PROCEDURE_ 430
#define FOREACH_ 431
#define STRINGGLOBAL_ 432
#define EQUIANGULATE_ 433
#define HISTOGRAM_ 434
#define LOGHISTOGRAM_ 435
#define AREA_FIXED_ 436
#define QUIT_ 437
#define WARNING_MESSAGES_ 438
#define IF_ 439
#define WHILE_ 440
#define DO_ 441
#define NO_REFINE_ 442
#define STRING_ 443
#define NONCONTENT_ 444
#define FOR_ 445
#define HIT_PARTNER_ 446
#define FRONTBODY_ 447
#define BACKBODY_ 448
#define COLORFILE_ 449
#define PERM_STRINGGLOBAL_ 450
#define FUNCTION_IDENT_ 451
#define THICKEN_ 452
#define COLORMAP_ 453
#define REDIRECT_ 454
#define NEWVERTEX_ 455
#define NEWEDGE_ 456
#define NEWFACET_ 457
#define MODULUS_ 458
#define TARGET_ 459
#define VALUE_ 460
#define INVERSE_PERIODS_ 461
#define NEWBODY_ 462
#define DELTA_ 463
#define GAP_CONSTANT_ 464
#define DUMP_ 465
#define NOTCH_ 466
#define QUANTITY_ 467
#define LOAD_ 468
#define PERM_PROCEDURE_ 469
#define PROCEDURE_WORD_ 470
#define DYNAMIC_LOAD_FUNC_ 471
#define PERM_IDENT_ 472
#define PERMLOAD_ 473
#define HELP_ 474
#define VERTEX_AVERAGE_ 475
#define METHOD_INSTANCE_ 476
#define RAW_VERTEX_AVERAGE_ 477
#define OPTIMIZE_ 478
#define REDIRECTOVER_ 479
#define TOLERANCE_ 480
#define RAWEST_VERTEX_AVERAGE_ 481
#define JIGGLE_ 482
#define VIEW_TRANSFORMS_ 483
#define CLOSE_SHOW_ 484
#define IS_DEFINED_ 485
#define NODISPLAY_ 486
#define PERM_ASSIGN_ 487
#define PHASE_ 488
#define VIEW_TRANSFORM_SWAP_COLORS_ 489
#define BACKQUOTE_COMMA_ 490
#define INTERNAL_VARIABLE_ 491
#define DIRICHLET_ 492
#define SOBOLEV_ 493
#define VIEW_TRANSFORM_PARITY_ 494
#define SOBOLEV_SEEK_ 495
#define DIRICHLET_SEEK_ 496
#define HESSIAN_SEEK_ 497
#define REORDER_STORAGE_ 498
#define RENUMBER_ALL_ 499
#define CONSTRAINT_NAME_ 500
#define BOUNDARY_NAME_ 501
#define PROCEDURE_IDENT_ 502
#define POP_TRI_TO_EDGE_ 503
#define POP_EDGE_TO_TRI_ 504
#define POP_QUAD_TO_QUAD_ 505
#define SHOWVERB_ 506
#define PROCEDURES_ 507
#define MPI_TASK_ATTR_ 508
#define T1_EDGESWAP_ 509
#define MERGE_EDGE_ 510
#define MERGE_FACET_ 511
#define MERGE_VERTEX_ 512
#define RESET_COUNTS_ 513
#define VALID_ELEMENT_ 514
#define MID_EDGE_ 515
#define MID_FACET_ 516
#define GO_COUNT_ 517
#define ELEMENT_IDENT_ 518
#define BODY_METIS_ 519
#define REVERSE_ORIENTATION_ 520
#define MATRIX_MULTIPLY_ 521
#define MATRIX_INVERSE_ 522
#define BINARY_PRINTF_ 523
#define DUMP_MEMLIST_ 524
#define FREE_DISCARDS_ 525
#define REPARTITION_ 526
#define METIS_READJUST_ 527
#define MEAN_CURVATURE_ 528
#define GLOBAL_ 529
#define LEAD_INTEGER_AT_ 530
#define INTEGER_AT_ 531
#define MATRIX_DETERMINANT_ 532
#define SUBCOMMAND_ 533
#define ABORT_ 534
#define BREAKPOINT_ 535
#define WHEREAMI_ 536
#define ADDLOAD_ 537
#define SIMPLEX_TO_FE_ 538
#define DISPLAY_TEXT_ 539
#define DELETE_TEXT_ 540
#define SUPPRESS_WARNING_ 541
#define UNSUPPRESS_WARNING_ 542
#define RESET_PROFILING_ 543
#define VALID_CONSTRAINT_ 544
#define VALID_BOUNDARY_ 545
#define ARRAY_ATTRIBUTE_ 546
#define PROFILING_ 547
#define ASSIGNOP_ 548
#define PIPE_ 549
#define THEN_ 550
#define ELSE_ 551
#define OR_ 552
#define AND_ 553
#define NOT_ 554
#define NE_ 555
#define GE_ 556
#define LE_ 557
#define EQ_ 558
#define ON_CONSTRAINT_ 559
#define HIT_CONSTRAINT_ 560
#define ON_BOUNDARY_ 561
#define ON_QUANTITY_ 562
#define ON_METHOD_INSTANCE_ 563
#define DOT_ 564
#define IDIV_ 565
#define IMOD_ 566
#define EPRINT_ 567




/* Copy the first part of user declarations.  */

  
#include "include.h"
#include "lex.h"

#define YYSTYPE yystype
#define gettxt(a,b) (b)
#define yylex kb_yylex

int assignbacktrack ARGS((void));

#ifndef __GNUC__
#ifdef YYBISON
/* for Bison */
#ifndef __yy_memcpy
static void __yy_memcpy ARGS((char *, char *, int ));
#endif
#endif
#endif

/* for bison version 2.1 output */
#define __STDC__ 1

/* for non-ANSI compilers */
#define const

#ifndef NO_YACC_DEBUG
#define YYDEBUG 1
#endif
int help_flag; /* avoid error message while doing help */

/* Backtrack to previous := in inputbuffer */
int assignbacktrack ()
{ int spot;
  for ( spot = inputbufferspot - 1; spot > 0 ; spot-- )
    if ( inputbuffer[spot-1] == ':' && inputbuffer[spot] == '=' )
       return spot+1;
  return 0;
}


/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 219 of yacc.c.  */


#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T) && (defined (__STDC__) || defined (__cplusplus))
# include <stddef.h> /* INFRINGES ON USER NAME SPACE */
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

#if ! defined (yyoverflow) || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if defined (__STDC__) || defined (__cplusplus)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     define YYINCLUDED_STDLIB_H
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2005 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM ((YYSIZE_T) -1)
#  endif
#  ifdef __cplusplus
extern "C" {
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if (! defined (malloc) && ! defined (YYINCLUDED_STDLIB_H) \
	&& (defined (__STDC__) || defined (__cplusplus)))
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if (! defined (free) && ! defined (YYINCLUDED_STDLIB_H) \
	&& (defined (__STDC__) || defined (__cplusplus)))
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifdef __cplusplus
}
#  endif
# endif
#endif /* ! defined (yyoverflow) || YYERROR_VERBOSE */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (defined (YYSTYPE_IS_TRIVIAL) && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short int yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short int) + sizeof (YYSTYPE))			\
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined (__GNUC__) && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (0)
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif

#if defined (__STDC__) || defined (__cplusplus)
   typedef signed char yysigned_char;
#else
   typedef short int yysigned_char;
#endif

/* YYFINAL -- State number of the termination state. */
#define YYFINAL  6
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   37216

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  335
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  108
/* YYNRULES -- Number of rules. */
#define YYNRULES  917
/* YYNRULES -- Number of states. */
#define YYNSTATES  1329

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   567

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned short int yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,   328,     2,     2,
     300,   301,   326,   324,   293,   325,   294,   327,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   309,   295,
     314,   307,   313,   308,   334,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   296,     2,   297,   332,     2,   302,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   298,     2,   299,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,   156,   157,   158,   159,   160,   161,   162,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
     175,   176,   177,   178,   179,   180,   181,   182,   183,   184,
     185,   186,   187,   188,   189,   190,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,   201,   202,   203,   204,
     205,   206,   207,   208,   209,   210,   211,   212,   213,   214,
     215,   216,   217,   218,   219,   220,   221,   222,   223,   224,
     225,   226,   227,   228,   229,   230,   231,   232,   233,   234,
     235,   236,   237,   238,   239,   240,   241,   242,   243,   244,
     245,   246,   247,   248,   249,   250,   251,   252,   253,   254,
     255,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   303,   304,
     305,   306,   310,   311,   312,   315,   316,   317,   318,   319,
     320,   321,   322,   323,   329,   330,   331,   333
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned short int yyprhs[] =
{
       0,     0,     3,     5,     6,    10,    12,    14,    18,    22,
      26,    30,    34,    38,    42,    46,    50,    53,    56,    58,
      60,    62,    65,    68,    70,    73,    76,    79,    81,    83,
      84,    89,    92,    96,    98,   100,   102,   105,   108,   110,
     113,   115,   117,   119,   122,   125,   126,   132,   135,   137,
     141,   144,   146,   149,   152,   154,   157,   160,   163,   165,
     168,   171,   174,   176,   179,   182,   185,   187,   190,   193,
     196,   199,   202,   205,   208,   210,   213,   215,   218,   221,
     223,   226,   229,   231,   234,   237,   239,   241,   244,   247,
     248,   253,   256,   259,   260,   264,   268,   271,   274,   278,
     282,   285,   289,   292,   295,   298,   302,   305,   308,   311,
     313,   315,   318,   321,   323,   326,   329,   332,   335,   337,
     340,   343,   346,   350,   353,   356,   360,   363,   366,   369,
     372,   375,   378,   381,   386,   389,   392,   395,   398,   401,
     404,   407,   410,   413,   416,   419,   422,   424,   427,   430,
     433,   436,   439,   442,   445,   452,   455,   458,   465,   468,
     475,   478,   481,   484,   487,   489,   492,   495,   497,   500,
     503,   505,   507,   509,   511,   513,   515,   517,   519,   521,
     523,   525,   527,   529,   533,   536,   539,   542,   545,   547,
     549,   552,   554,   556,   558,   560,   562,   564,   566,   568,
     570,   572,   574,   576,   578,   580,   582,   584,   586,   588,
     590,   592,   594,   596,   598,   600,   603,   606,   609,   612,
     616,   619,   623,   626,   630,   633,   637,   640,   642,   645,
     647,   650,   652,   655,   657,   659,   661,   664,   668,   669,
     674,   678,   679,   684,   688,   692,   696,   700,   703,   708,
     713,   718,   723,   727,   731,   735,   739,   743,   747,   751,
     755,   759,   762,   766,   769,   772,   775,   779,   782,   788,
     794,   800,   806,   812,   816,   820,   825,   830,   835,   840,
     845,   849,   853,   857,   861,   865,   868,   871,   874,   877,
     880,   883,   886,   889,   892,   895,   898,   900,   903,   907,
     910,   913,   916,   919,   923,   926,   930,   934,   937,   940,
     943,   946,   949,   952,   956,   960,   964,   968,   970,   974,
     978,   982,   985,   988,   991,   993,   995,   997,   999,  1001,
    1003,  1005,  1007,  1009,  1011,  1013,  1015,  1018,  1022,  1027,
    1030,  1033,  1037,  1042,  1045,  1048,  1050,  1052,  1053,  1055,
    1057,  1058,  1059,  1067,  1069,  1071,  1072,  1073,  1080,  1084,
    1087,  1090,  1092,  1096,  1100,  1103,  1106,  1109,  1113,  1118,
    1122,  1127,  1132,  1137,  1142,  1144,  1146,  1148,  1150,  1153,
    1156,  1159,  1162,  1166,  1170,  1174,  1180,  1186,  1192,  1196,
    1199,  1201,  1203,  1209,  1215,  1218,  1219,  1220,  1228,  1230,
    1233,  1236,  1239,  1242,  1245,  1248,  1251,  1252,  1257,  1260,
    1263,  1266,  1270,  1273,  1276,  1279,  1282,  1285,  1286,  1290,
    1291,  1297,  1301,  1302,  1308,  1312,  1313,  1318,  1319,  1324,
    1328,  1332,  1335,  1338,  1340,  1344,  1346,  1348,  1350,  1352,
    1354,  1356,  1358,  1360,  1362,  1364,  1366,  1367,  1372,  1373,
    1378,  1379,  1384,  1387,  1390,  1394,  1398,  1400,  1404,  1408,
    1410,  1413,  1416,  1419,  1422,  1425,  1428,  1430,  1434,  1438,
    1440,  1444,  1448,  1450,  1454,  1455,  1460,  1462,  1465,  1468,
    1471,  1474,  1477,  1480,  1483,  1486,  1489,  1493,  1496,  1499,
    1502,  1505,  1508,  1511,  1513,  1514,  1520,  1523,  1527,  1532,
    1535,  1539,  1544,  1547,  1554,  1557,  1562,  1565,  1574,  1577,
    1579,  1584,  1587,  1592,  1595,  1602,  1605,  1610,  1613,  1615,
    1618,  1620,  1624,  1626,  1629,  1631,  1634,  1642,  1645,  1650,
    1653,  1658,  1661,  1668,  1671,  1678,  1681,  1688,  1691,  1700,
    1703,  1711,  1717,  1721,  1726,  1733,  1736,  1742,  1746,  1748,
    1751,  1753,  1756,  1758,  1760,  1762,  1765,  1768,  1771,  1773,
    1776,  1778,  1781,  1783,  1786,  1788,  1791,  1793,  1796,  1798,
    1801,  1803,  1806,  1808,  1811,  1813,  1816,  1818,  1821,  1823,
    1825,  1828,  1831,  1834,  1837,  1840,  1843,  1845,  1847,  1849,
    1851,  1853,  1855,  1858,  1863,  1867,  1872,  1874,  1876,  1878,
    1881,  1883,  1889,  1894,  1900,  1902,  1905,  1907,  1910,  1913,
    1916,  1919,  1922,  1926,  1930,  1932,  1934,  1936,  1939,  1941,
    1943,  1945,  1947,  1949,  1951,  1953,  1956,  1959,  1961,  1965,
    1969,  1971,  1973,  1975,  1977,  1979,  1981,  1983,  1985,  1987,
    1989,  1991,  1993,  1995,  1997,  1999,  2001,  2003,  2005,  2007,
    2009,  2011,  2013,  2015,  2017,  2019,  2021,  2023,  2025,  2027,
    2029,  2031,  2033,  2035,  2037,  2039,  2041,  2043,  2045,  2047,
    2049,  2051,  2053,  2055,  2057,  2059,  2061,  2064,  2067,  2070,
    2073,  2076,  2079,  2082,  2085,  2088,  2091,  2094,  2097,  2100,
    2103,  2105,  2108,  2111,  2116,  2119,  2123,  2128,  2133,  2138,
    2141,  2143,  2145,  2147,  2150,  2153,  2157,  2160,  2162,  2164,
    2166,  2168,  2172,  2176,  2178,  2181,  2185,  2189,  2193,  2197,
    2201,  2205,  2209,  2213,  2217,  2221,  2225,  2229,  2232,  2236,
    2240,  2243,  2245,  2247,  2249,  2251,  2253,  2255,  2257,  2259,
    2261,  2266,  2269,  2276,  2278,  2282,  2286,  2290,  2294,  2298,
    2302,  2306,  2310,  2314,  2318,  2322,  2326,  2330,  2334,  2338,
    2342,  2346,  2351,  2356,  2360,  2364,  2368,  2372,  2376,  2380,
    2384,  2388,  2392,  2396,  2400,  2404,  2408,  2412,  2416,  2420,
    2424,  2427,  2430,  2433,  2436,  2439,  2440,  2441,  2449,  2453,
    2455,  2457,  2459,  2461,  2463,  2465,  2467,  2470,  2473,  2475,
    2480,  2483,  2484,  2488,  2490,  2493,  2494,  2498,  2501,  2502,
    2510,  2511,  2519,  2520,  2524,  2526,  2528,  2530,  2532,  2534,
    2536,  2538,  2540,  2542,  2544,  2548,  2554,  2556,  2558,  2560,
    2562,  2564,  2566,  2568,  2570,  2572,  2574,  2576,  2578,  2580,
    2582,  2584,  2586,  2588,  2590,  2592,  2594,  2596,  2599,  2601,
    2603,  2605,  2607,  2608,  2615,  2616,  2624,  2629,  2635,  2641,
    2646,  2651,  2658,  2666,  2673,  2680,  2688,  2693,  2698,  2705,
    2712,  2717,  2722,  2729,  2736,  2740,  2744,  2750,  2756,  2760,
    2764,  2770,  2776,  2779,  2781,  2783,  2785,  2787,  2789,  2791,
    2793,  2795,  2797,  2799,  2801,  2803,  2805,  2807,  2809,  2811,
    2813,  2817,  2823,  2825,  2829,  2831,  2836,  2843,  2849,  2851,
    2855,  2857,  2862,  2868,  2875,  2878,  2880,  2883
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const short int yyrhs[] =
{
     336,     0,    -1,     4,    -1,    -1,     4,   337,   338,    -1,
     341,    -1,   340,    -1,   339,   304,   416,    -1,   339,   304,
       1,    -1,   339,   199,   416,    -1,   339,   199,     1,    -1,
     339,   224,   416,    -1,   339,   224,     1,    -1,   280,   175,
     365,    -1,   280,   196,   365,    -1,   280,   247,   365,    -1,
     280,     1,    -1,    52,   280,    -1,   281,    -1,     1,    -1,
     175,    -1,   175,   365,    -1,   175,     1,    -1,   214,    -1,
     214,   365,    -1,   214,     1,    -1,   339,     1,    -1,   347,
      -1,   348,    -1,    -1,   298,   343,   341,   299,    -1,   298,
     299,    -1,   298,     1,   299,    -1,   345,    -1,   339,    -1,
     342,    -1,   342,   365,    -1,   342,     1,    -1,   295,    -1,
     339,   295,    -1,   345,    -1,   339,    -1,   346,    -1,   348,
     339,    -1,   348,   346,    -1,    -1,   184,   365,   350,   305,
     339,    -1,   184,     1,    -1,   349,    -1,   349,   306,   339,
      -1,   306,     1,    -1,   308,    -1,     6,   416,    -1,     6,
     414,    -1,     6,    -1,     6,     1,    -1,    86,   416,    -1,
      86,   414,    -1,    86,    -1,    86,     1,    -1,    20,   416,
      -1,    20,   414,    -1,    20,    -1,    20,     1,    -1,   137,
     416,    -1,   137,   414,    -1,   137,    -1,   137,     1,    -1,
      41,   416,    -1,    41,     1,    -1,   157,   416,    -1,   157,
       1,    -1,   155,   416,    -1,   155,     1,    -1,     5,    -1,
       5,     1,    -1,   153,    -1,   153,   365,    -1,   153,     1,
      -1,   129,    -1,   129,     9,    -1,   129,     1,    -1,   130,
      -1,   130,     9,    -1,   130,     1,    -1,    79,    -1,   262,
      -1,    79,   365,    -1,    79,     1,    -1,    -1,   185,   365,
     352,   186,    -1,   351,   339,    -1,   185,     1,    -1,    -1,
     186,   354,   344,    -1,   353,   185,   365,    -1,   353,     1,
      -1,   186,     1,    -1,   190,   300,   345,    -1,   355,   365,
     295,    -1,   355,   295,    -1,   356,   339,   301,    -1,   356,
     301,    -1,   357,   339,    -1,   190,     1,    -1,   190,   300,
       1,    -1,   355,     1,    -1,   356,     1,    -1,   357,     1,
      -1,    88,    -1,   146,    -1,   146,   365,    -1,   146,     1,
      -1,   150,    -1,    88,   365,    -1,    88,     1,    -1,   150,
     365,    -1,   150,     1,    -1,    82,    -1,    82,   416,    -1,
      82,     1,    -1,   120,   365,    -1,   120,   174,   365,    -1,
     120,     1,    -1,   154,   416,    -1,   154,   174,   416,    -1,
     154,     1,    -1,   106,   416,    -1,   106,     1,    -1,   111,
     416,    -1,   111,     1,    -1,   156,   416,    -1,   156,     1,
      -1,   148,   365,   293,   416,    -1,   148,     1,    -1,    92,
     416,    -1,    92,     1,    -1,   135,   365,    -1,   135,     1,
      -1,   136,   365,    -1,   136,     1,    -1,   272,   365,    -1,
     272,     1,    -1,   264,   365,    -1,   264,     1,    -1,   143,
     365,    -1,   143,    -1,   143,     1,    -1,   113,   365,    -1,
     113,     1,    -1,   112,   365,    -1,   112,     1,    -1,   115,
     365,    -1,   115,     1,    -1,   109,   300,   365,   293,   365,
     301,    -1,   109,   365,    -1,   109,     1,    -1,   102,   300,
     365,   293,   365,   301,    -1,   102,     1,    -1,   110,   300,
     365,   293,   365,   301,    -1,   110,   365,    -1,   110,     1,
      -1,   103,   365,    -1,   103,     1,    -1,    56,    -1,    56,
     365,    -1,    56,     1,    -1,   242,    -1,   242,   365,    -1,
     242,     1,    -1,    91,    -1,   182,    -1,   278,    -1,   279,
      -1,   283,    -1,   243,    -1,   244,    -1,   269,    -1,   270,
      -1,   271,    -1,   119,    -1,   140,    -1,    83,    -1,    83,
     365,   365,    -1,    83,     1,    -1,   139,     9,    -1,   152,
     365,    -1,   152,     1,    -1,    80,    -1,    78,    -1,   123,
     292,    -1,   288,    -1,    98,    -1,   258,    -1,    81,    -1,
      70,    -1,    89,    -1,    90,    -1,   149,    -1,    93,    -1,
     116,    -1,   117,    -1,    94,    -1,   100,    -1,    31,    -1,
     159,    -1,   160,    -1,   237,    -1,   241,    -1,   238,    -1,
     240,    -1,    33,    -1,    40,    -1,   219,    -1,    69,    -1,
      65,    53,    -1,    65,   134,    -1,    65,   101,    -1,    65,
     252,    -1,    65,   141,   365,    -1,    65,   246,    -1,    65,
      61,   365,    -1,    65,   245,    -1,    65,   212,    77,    -1,
      65,    77,    -1,    65,   221,   147,    -1,    65,   147,    -1,
     229,    -1,    74,   414,    -1,    74,    -1,   223,   414,    -1,
     223,    -1,    99,   414,    -1,    99,    -1,   227,    -1,     3,
      -1,   358,   365,    -1,   358,   365,   293,    -1,    -1,   236,
     436,   359,   365,    -1,   236,   436,     1,    -1,    -1,    57,
     236,   360,   365,    -1,    57,   236,     1,    -1,    57,   114,
     365,    -1,    57,   114,     1,    -1,   114,   436,   365,    -1,
     114,     1,    -1,    57,    61,   365,   274,    -1,    52,    61,
     365,   274,    -1,    57,    61,   245,   274,    -1,    52,    61,
     245,   274,    -1,    57,   245,   274,    -1,    52,   245,   274,
      -1,    57,   138,   365,    -1,    57,   138,     1,    -1,   138,
     436,   365,    -1,   138,   436,     1,    -1,    57,   118,   365,
      -1,    57,   118,     1,    -1,   209,   436,   365,    -1,   209,
       1,    -1,   181,   174,   365,    -1,   211,   365,    -1,   211,
       1,    -1,    99,   365,    -1,    99,   174,   365,    -1,    99,
       1,    -1,    77,   294,   204,   436,   365,    -1,    77,   294,
     203,   436,   365,    -1,    77,   294,   225,   436,   365,    -1,
     147,   294,   203,   436,   365,    -1,    77,   294,    34,   436,
     365,    -1,    77,   294,     1,    -1,   147,   294,     1,    -1,
      57,    77,   204,   365,    -1,    57,    77,   203,   365,    -1,
      57,    77,   225,   365,    -1,    57,   147,   203,   365,    -1,
      57,    77,    34,   365,    -1,    57,    77,    58,    -1,    57,
      77,   173,    -1,    57,    77,   171,    -1,    57,    77,   172,
      -1,    57,    77,     1,    -1,   286,   365,    -1,   286,     1,
      -1,   287,   365,    -1,   287,     1,    -1,   213,   416,    -1,
     213,     1,    -1,   282,   416,    -1,   282,     1,    -1,   218,
     416,    -1,   218,     1,    -1,   210,   416,    -1,   210,    -1,
     210,     1,    -1,   194,   174,   416,    -1,   223,   365,    -1,
     223,     1,    -1,    29,   174,    -1,   217,   232,    -1,   177,
     174,   416,    -1,   361,   416,    -1,   177,   174,   365,    -1,
     195,   232,   416,    -1,   177,   232,    -1,   195,   174,    -1,
     361,   365,    -1,   362,   365,    -1,   263,   174,    -1,   363,
     365,    -1,    29,   294,   208,    -1,    29,   294,   138,    -1,
     364,   436,   365,    -1,    29,   294,    58,    -1,   364,    -1,
      29,   294,     1,    -1,    29,   303,   365,    -1,    29,   174,
       1,    -1,    29,   318,    -1,    12,   318,    -1,    29,     1,
      -1,    97,    -1,   188,    -1,    12,    -1,    29,    -1,   165,
      -1,   175,    -1,   196,    -1,   177,    -1,    77,    -1,   147,
      -1,   245,    -1,   246,    -1,   300,   301,    -1,   300,   366,
     367,    -1,   369,   293,   366,   367,    -1,   300,     1,    -1,
     300,   366,    -1,   369,   293,     1,    -1,   369,   293,   366,
       1,    -1,   369,     1,    -1,   369,   301,    -1,   342,    -1,
     295,    -1,    -1,    12,    -1,   196,    -1,    -1,    -1,   161,
     366,   371,   372,   368,   373,   370,    -1,    12,    -1,   247,
      -1,    -1,    -1,   215,   374,   375,   368,   376,   370,    -1,
     161,   366,     1,    -1,   161,     1,    -1,   215,     1,    -1,
     161,    -1,    95,    29,   366,    -1,    95,    12,   366,    -1,
      95,    29,    -1,    95,    12,    -1,    95,   177,    -1,   296,
     365,   297,    -1,   377,   296,   365,   297,    -1,   296,   365,
     297,    -1,   378,   296,   365,   297,    -1,    95,   165,   366,
     378,    -1,    95,    12,   366,   378,    -1,    95,    29,   366,
     378,    -1,   379,    -1,   165,    -1,   104,    -1,   291,    -1,
     412,   291,    -1,   412,    12,    -1,   412,     1,    -1,   380,
     377,    -1,   381,   436,   365,    -1,   380,   436,   380,    -1,
     380,   436,   365,    -1,   380,   436,   365,   326,   380,    -1,
     380,   436,   380,   324,   380,    -1,   380,   436,   380,   325,
     380,    -1,   380,   329,   380,    -1,   380,   377,    -1,    27,
      -1,   291,    -1,    95,   409,   101,   382,   366,    -1,    95,
     409,   101,    12,   366,    -1,   383,   378,    -1,    -1,    -1,
     383,   161,   384,   298,   385,   341,   299,    -1,   383,    -1,
      95,   212,    -1,    95,   221,    -1,    95,    61,    -1,    95,
     141,    -1,    95,     1,    -1,    12,   174,    -1,    12,   232,
      -1,    -1,    57,    12,   388,   174,    -1,    57,    12,    -1,
      12,     1,    -1,    12,     1,    -1,    12,   174,     1,    -1,
     386,   416,    -1,   386,   365,    -1,   386,     1,    -1,   387,
     416,    -1,   387,   365,    -1,    -1,   386,   389,   340,    -1,
      -1,   386,   298,   390,   341,   299,    -1,   386,   298,   299,
      -1,    -1,   387,   298,   391,   341,   299,    -1,   387,   298,
     299,    -1,    -1,   175,   174,   392,   339,    -1,    -1,   214,
     232,   393,   339,    -1,   175,   174,     1,    -1,   175,   232,
       1,    -1,   361,   298,    -1,   167,   394,    -1,   395,    -1,
     394,   293,   395,    -1,    12,    -1,    29,    -1,   165,    -1,
     175,    -1,   196,    -1,   177,    -1,    77,    -1,   147,    -1,
     245,    -1,   246,    -1,     1,    -1,    -1,    88,    13,   396,
     339,    -1,    -1,   150,    13,   397,   339,    -1,    -1,   146,
      13,   398,   339,    -1,    88,    13,    -1,   146,    13,    -1,
     365,   293,   399,    -1,   365,   293,     1,    -1,   365,    -1,
     416,   293,   399,    -1,   416,   293,     1,    -1,   416,    -1,
     121,   416,    -1,   121,     1,    -1,   268,   416,    -1,   268,
       1,    -1,   122,   416,    -1,   122,     1,    -1,   400,    -1,
     400,   293,   399,    -1,   400,   293,     1,    -1,   401,    -1,
     401,   293,   399,    -1,   401,   293,     1,    -1,   402,    -1,
     402,   293,   399,    -1,    -1,   402,   293,     1,   403,    -1,
     123,    -1,   404,   215,    -1,   404,   416,    -1,   404,   175,
      -1,   404,   196,    -1,   404,   247,    -1,   404,   214,    -1,
     162,   175,    -1,   404,   365,    -1,   404,   380,    -1,   404,
     412,   419,    -1,   404,    88,    -1,   404,   150,    -1,   404,
     146,    -1,   404,     1,    -1,   169,   416,    -1,   169,     1,
      -1,   302,    -1,    -1,   405,   341,   235,   406,   365,    -1,
     405,     1,    -1,   196,   300,   301,    -1,   196,   300,   399,
     301,    -1,   196,     1,    -1,   247,   300,   301,    -1,   247,
     300,   399,   301,    -1,   247,     1,    -1,   164,   300,   365,
     293,   365,   301,    -1,   164,     1,    -1,   239,   296,   365,
     297,    -1,   239,     1,    -1,   284,   300,   365,   293,   365,
     293,   416,   301,    -1,   284,     1,    -1,   284,    -1,   285,
     300,   365,   301,    -1,   285,     1,    -1,   200,   300,   399,
     301,    -1,   200,     1,    -1,   201,   300,   365,   293,   365,
     301,    -1,   201,     1,    -1,   202,   300,   399,   301,    -1,
     202,     1,    -1,   207,    -1,   207,     1,    -1,   365,    -1,
     365,   334,   365,    -1,   276,    -1,    30,   276,    -1,   275,
      -1,    30,   275,    -1,   259,   300,   409,   296,   407,   297,
     301,    -1,   259,     1,    -1,   289,   300,   365,   301,    -1,
     289,     1,    -1,   290,   300,   365,   301,    -1,   290,     1,
      -1,   257,   300,   365,   293,   365,   301,    -1,   257,     1,
      -1,   255,   300,   365,   293,   365,   301,    -1,   255,     1,
      -1,   256,   300,   365,   293,   365,   301,    -1,   256,     1,
      -1,   266,   300,   380,   293,   380,   293,   380,   301,    -1,
     266,     1,    -1,   266,   300,   380,   293,   380,   293,     1,
      -1,   266,   300,   380,   293,     1,    -1,   266,   300,     1,
      -1,   277,   300,   380,   301,    -1,   267,   300,   380,   293,
     380,   301,    -1,   267,     1,    -1,   267,   300,   380,   293,
       1,    -1,   267,   300,     1,    -1,    65,    -1,    65,     1,
      -1,    67,    -1,    67,     1,    -1,   220,    -1,   222,    -1,
     226,    -1,   220,     1,    -1,   222,     1,    -1,   226,     1,
      -1,    63,    -1,    63,     1,    -1,   265,    -1,   265,     1,
      -1,    68,    -1,    68,     1,    -1,    71,    -1,    71,     1,
      -1,   254,    -1,   254,     1,    -1,   178,    -1,   178,     1,
      -1,   105,    -1,   105,     1,    -1,   248,    -1,   248,     1,
      -1,   249,    -1,   249,     1,    -1,   250,    -1,   250,     1,
      -1,    72,    -1,    73,    -1,    72,    29,    -1,    73,    29,
      -1,    72,    77,    -1,    73,    77,    -1,    72,     1,    -1,
      73,     1,    -1,    36,    -1,    37,    -1,    38,    -1,    39,
      -1,    49,    -1,   409,    -1,   412,   409,    -1,   409,   296,
     407,   297,    -1,   409,   296,     1,    -1,   409,   296,   365,
       1,    -1,    25,    -1,    87,    -1,   263,    -1,   411,   294,
      -1,   411,    -1,   412,   409,   296,   365,   297,    -1,   412,
     409,   296,     1,    -1,   412,   409,   296,   365,     1,    -1,
     412,    -1,   411,    12,    -1,   410,    -1,   410,    12,    -1,
     411,   175,    -1,   410,   175,    -1,   411,    29,    -1,   410,
      29,    -1,   413,    64,   365,    -1,   413,    64,     1,    -1,
      84,    -1,    85,    -1,    51,    -1,   415,    51,    -1,   415,
      -1,   177,    -1,   195,    -1,    19,    -1,   183,    -1,   166,
      -1,   132,    -1,   158,   416,    -1,   158,     1,    -1,   417,
      -1,   417,   293,   399,    -1,   417,   293,     1,    -1,    62,
      -1,    24,    -1,    42,    -1,   273,    -1,    66,    -1,   142,
      -1,    76,    -1,   144,    -1,   163,    -1,    55,    -1,    43,
      -1,    44,    -1,    34,    -1,   204,    -1,   253,    -1,    59,
      -1,    60,    -1,    45,    -1,    46,    -1,    47,    -1,    32,
      -1,   145,    -1,   151,    -1,   193,    -1,   192,    -1,    48,
      -1,    58,    -1,   187,    -1,   191,    -1,   189,    -1,   231,
      -1,    28,    -1,   170,    -1,   108,    -1,   107,    -1,    18,
      -1,    50,    -1,   260,    -1,   261,    -1,   133,    -1,   233,
      -1,    77,    -1,   147,    -1,    27,    -1,   418,    -1,   419,
      -1,   419,   377,    -1,   319,   245,    -1,   319,   365,    -1,
     319,     1,    -1,   321,   246,    -1,   321,   365,    -1,   321,
       1,    -1,   320,   245,    -1,   320,   365,    -1,   320,     1,
      -1,   322,    77,    -1,   322,     1,    -1,   323,   147,    -1,
     323,     1,    -1,   420,    -1,   412,   420,    -1,   412,    12,
      -1,   230,   300,   416,   301,    -1,   230,     1,    -1,   230,
     300,   416,    -1,   131,   300,   291,   301,    -1,   131,   300,
     165,   301,    -1,   131,   300,   416,   301,    -1,   131,     1,
      -1,    75,    -1,    99,    -1,   152,    -1,   333,   365,    -1,
     333,     1,    -1,   300,   365,   301,    -1,   300,     1,    -1,
     236,    -1,   138,    -1,    29,    -1,   217,    -1,    29,   294,
      27,    -1,    29,   294,     1,    -1,   216,    -1,    26,    77,
      -1,    77,   294,    60,    -1,    77,   294,   203,    -1,    77,
     294,   225,    -1,   147,   294,   203,    -1,    77,   294,   204,
      -1,    77,   294,   205,    -1,   147,   294,   205,    -1,    77,
     294,    34,    -1,    77,   294,    58,    -1,    77,   294,   171,
      -1,    77,   294,   173,    -1,    77,   294,   172,    -1,    77,
       1,    -1,    77,   294,     1,    -1,   147,   294,     1,    -1,
     147,     1,    -1,     9,    -1,     8,    -1,    10,    -1,    11,
      -1,   421,    -1,    21,    -1,    22,    -1,    23,    -1,    17,
      -1,    14,   300,   365,   301,    -1,    14,     1,    -1,    15,
     300,   365,   293,   365,   301,    -1,    15,    -1,   365,   324,
     365,    -1,   365,   325,   365,    -1,   365,   307,   365,    -1,
     365,   327,   365,    -1,   365,   326,   365,    -1,   365,   328,
     365,    -1,   365,   331,   365,    -1,   365,   330,   365,    -1,
     365,   332,   365,    -1,   365,   314,   365,    -1,   365,   313,
     365,    -1,   365,   315,   365,    -1,   365,   318,   365,    -1,
     365,   317,   365,    -1,   365,   316,   365,    -1,   365,   311,
     365,    -1,   365,   310,   365,    -1,   365,   324,   380,     1,
      -1,   365,   325,   380,     1,    -1,   365,   324,     1,    -1,
     365,   325,     1,    -1,   365,   307,     1,    -1,   365,   327,
       1,    -1,   365,   326,     1,    -1,   365,   328,     1,    -1,
     365,   331,     1,    -1,   365,   330,     1,    -1,   365,   332,
       1,    -1,   365,   314,     1,    -1,   365,   313,     1,    -1,
     365,   315,     1,    -1,   365,   318,     1,    -1,   365,   317,
       1,    -1,   365,   316,     1,    -1,   365,   311,     1,    -1,
     365,   310,     1,    -1,    30,   365,    -1,    30,     1,    -1,
      96,   365,    -1,   312,   365,    -1,   312,     1,    -1,    -1,
      -1,   365,   308,   422,   365,   309,   423,   365,    -1,   365,
     308,     1,    -1,   124,    -1,   125,    -1,   127,    -1,   128,
      -1,   126,    -1,   179,    -1,   180,    -1,   179,     1,    -1,
     180,     1,    -1,   176,    -1,   426,   413,   186,   339,    -1,
     176,     1,    -1,    -1,   251,   427,   413,    -1,   251,    -1,
     251,     1,    -1,    -1,   168,   428,   413,    -1,   168,     1,
      -1,    -1,   425,   429,   300,   413,   293,   365,   301,    -1,
      -1,   424,   430,   300,   413,   293,   365,   301,    -1,    -1,
     408,   431,   413,    -1,    57,    -1,   187,    -1,   191,    -1,
      58,    -1,   133,    -1,   189,    -1,   231,    -1,   170,    -1,
     107,    -1,   108,    -1,   432,   413,   433,    -1,   432,   413,
     433,    64,   365,    -1,   192,    -1,   193,    -1,    32,    -1,
     145,    -1,   151,    -1,    59,    -1,    48,    -1,    34,    -1,
      44,    -1,   204,    -1,    60,    -1,    54,    -1,    61,    -1,
     141,    -1,    47,    -1,    62,    -1,    24,    -1,   233,    -1,
      27,    -1,   142,    -1,    50,    -1,   294,   434,    -1,   434,
      -1,   174,    -1,   303,    -1,   318,    -1,    -1,   411,   294,
     434,   437,   436,   365,    -1,    -1,   411,   294,   434,   438,
     377,   436,   365,    -1,   432,   413,   435,   365,    -1,   432,
     413,   435,   377,   365,    -1,   432,   413,   291,   377,   365,
      -1,   432,   413,   291,   380,    -1,   432,   413,   291,   365,
      -1,   432,   413,   291,   365,    64,   365,    -1,   432,   413,
     291,   377,   365,    64,   365,    -1,   432,   413,   291,   380,
      64,   365,    -1,   432,   413,   435,   365,    64,   365,    -1,
     432,   413,   435,   377,   365,    64,   365,    -1,   432,   413,
     212,    77,    -1,   439,   413,   212,    77,    -1,   432,   413,
     212,    77,    64,   365,    -1,   439,   413,   212,    77,    64,
     365,    -1,   432,   413,   221,   147,    -1,   439,   413,   221,
     147,    -1,   432,   413,   221,   147,    64,   365,    -1,   439,
     413,   221,   147,    64,   365,    -1,   432,   413,    77,    -1,
     439,   413,    77,    -1,   432,   413,    77,    64,   365,    -1,
     439,   413,    77,    64,   365,    -1,   432,   413,   147,    -1,
     439,   413,   147,    -1,   432,   413,   147,    64,   365,    -1,
     439,   413,   147,    64,   365,    -1,   432,     1,    -1,    52,
      -1,    58,    -1,   191,    -1,   133,    -1,   187,    -1,   189,
      -1,   231,    -1,    59,    -1,    44,    -1,   204,    -1,    60,
      -1,   108,    -1,   107,    -1,   170,    -1,   192,    -1,   193,
      -1,    39,    -1,   439,   413,   440,    -1,   439,   413,   440,
      64,   365,    -1,   245,    -1,   439,   413,   441,    -1,   245,
      -1,   439,   413,    61,   365,    -1,   439,   413,    61,   365,
      64,   365,    -1,   439,   413,   441,    64,   365,    -1,   246,
      -1,   439,   413,   442,    -1,   246,    -1,   439,   413,   141,
     365,    -1,   439,   413,   442,    64,   365,    -1,   439,   413,
     141,   365,    64,   365,    -1,    52,     1,    -1,     3,    -1,
     219,   185,    -1,   219,     1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short int yyrline[] =
{
       0,   158,   158,   160,   159,   162,   164,   167,   172,   175,
     180,   185,   191,   196,   199,   202,   205,   209,   211,   214,
     216,   218,   222,   227,   229,   232,   237,   239,   241,   243,
     243,   246,   248,   250,   252,   254,   256,   261,   268,   269,
     271,   273,   275,   277,   280,   283,   283,   286,   289,   291,
     293,   295,   300,   301,   302,   303,   306,   307,   308,   309,
     312,   313,   314,   315,   318,   319,   320,   321,   324,   325,
     328,   329,   332,   333,   336,   337,   340,   341,   342,   345,
     346,   347,   350,   351,   352,   355,   359,   367,   373,   375,
     375,   378,   380,   384,   384,   388,   390,   393,   396,   399,
     401,   408,   409,   413,   415,   418,   421,   424,   427,   432,
     434,   436,   440,   443,   445,   449,   452,   471,   476,   478,
     480,   484,   487,   490,   493,   495,   497,   500,   502,   505,
     507,   510,   512,   515,   518,   521,   523,   526,   528,   530,
     532,   534,   536,   538,   540,   542,   544,   546,   549,   551,
     554,   556,   559,   561,   564,   566,   568,   572,   574,   577,
     580,   582,   586,   587,   590,   591,   592,   596,   597,   598,
     602,   604,   606,   608,   610,   612,   614,   616,   618,   620,
     622,   624,   627,   628,   629,   632,   635,   636,   639,   640,
     641,   642,   643,   644,   645,   646,   647,   648,   649,   650,
     651,   652,   653,   654,   655,   656,   657,   658,   659,   660,
     661,   662,   663,   664,   665,   666,   667,   668,   669,   670,
     672,   677,   679,   684,   686,   688,   690,   692,   693,   694,
     695,   696,   697,   698,   699,   702,   704,   706,   709,   709,
     712,   715,   715,   718,   721,   722,   725,   728,   732,   734,
     736,   738,   740,   742,   745,   747,   750,   752,   755,   757,
     761,   764,   767,   770,   771,   774,   775,   776,   780,   782,
     784,   786,   788,   790,   793,   797,   799,   801,   804,   806,
     809,   811,   813,   815,   818,   831,   833,   835,   837,   841,
     842,   845,   846,   849,   850,   853,   854,   855,   858,   861,
     862,   865,   867,   869,   871,   873,   875,   877,   880,   883,
     886,   889,   891,   894,   895,   896,   917,   918,   936,   939,
     960,   963,   968,   973,   977,   979,   981,   982,   990,   998,
    1006,  1014,  1022,  1030,  1038,  1046,  1056,  1059,  1062,  1065,
    1072,  1075,  1079,  1082,  1085,  1089,  1090,  1091,  1096,  1097,
    1100,  1104,  1099,  1125,  1126,  1129,  1133,  1128,  1152,  1154,
    1156,  1158,  1162,  1163,  1168,  1169,  1173,  1176,  1177,  1179,
    1180,  1183,  1190,  1197,  1204,  1227,  1231,  1240,  1262,  1290,
    1294,  1300,  1304,  1312,  1319,  1324,  1339,  1361,  1383,  1389,
    1396,  1397,  1399,  1416,  1430,  1435,  1440,  1434,  1456,  1459,
    1460,  1462,  1463,  1466,  1469,  1477,  1484,  1484,  1490,  1498,
    1501,  1505,  1508,  1509,  1510,  1514,  1517,  1519,  1519,  1530,
    1530,  1541,  1547,  1547,  1558,  1564,  1564,  1577,  1576,  1591,
    1594,  1598,  1607,  1608,  1609,  1612,  1615,  1624,  1633,  1642,
    1651,  1660,  1669,  1678,  1687,  1697,  1703,  1702,  1710,  1709,
    1718,  1717,  1724,  1726,  1730,  1731,  1733,  1734,  1735,  1737,
    1741,  1742,  1744,  1745,  1747,  1749,  1751,  1752,  1753,  1756,
    1757,  1758,  1761,  1762,  1763,  1763,  1767,  1768,  1769,  1770,
    1771,  1772,  1773,  1775,  1781,  1786,  1791,  1854,  1855,  1856,
    1857,  1862,  1863,  1866,  1873,  1872,  1877,  1885,  1890,  1895,
    1899,  1904,  1909,  1913,  1916,  1922,  1925,  1930,  1935,  1939,
    1943,  1947,  1953,  1956,  1960,  1963,  1966,  1969,  1972,  1973,
    1976,  1977,  1978,  1979,  1980,  1981,  1983,  1985,  1989,  1991,
    1996,  1998,  2003,  2006,  2010,  2013,  2017,  2020,  2024,  2027,
    2032,  2037,  2042,  2047,  2051,  2054,  2058,  2062,  2068,  2070,
    2084,  2086,  2090,  2091,  2092,  2094,  2097,  2100,  2104,  2105,
    2109,  2110,  2114,  2115,  2120,  2121,  2126,  2127,  2131,  2133,
    2137,  2138,  2142,  2143,  2148,  2149,  2153,  2154,  2158,  2159,
    2161,  2162,  2163,  2166,  2169,  2173,  2178,  2179,  2180,  2181,
    2182,  2183,  2189,  2204,  2208,  2210,  2214,  2216,  2225,  2229,
    2230,  2231,  2234,  2236,  2243,  2255,  2272,  2287,  2304,  2306,
    2308,  2310,  2312,  2316,  2320,  2321,  2323,  2324,  2332,  2333,
    2334,  2335,  2336,  2337,  2338,  2339,  2341,  2343,  2344,  2345,
    2349,  2350,  2351,  2352,  2353,  2354,  2355,  2356,  2357,  2358,
    2359,  2360,  2361,  2362,  2363,  2364,  2365,  2366,  2367,  2368,
    2369,  2370,  2371,  2372,  2373,  2374,  2375,  2376,  2377,  2378,
    2379,  2380,  2381,  2382,  2383,  2384,  2385,  2386,  2387,  2388,
    2389,  2390,  2391,  2392,  2400,  2404,  2416,  2444,  2448,  2452,
    2456,  2460,  2464,  2468,  2472,  2476,  2480,  2484,  2488,  2492,
    2496,  2511,  2517,  2523,  2524,  2526,  2531,  2537,  2539,  2541,
    2549,  2550,  2551,  2552,  2553,  2556,  2557,  2559,  2560,  2561,
    2562,  2564,  2565,  2570,  2571,  2572,  2573,  2574,  2575,  2576,
    2577,  2578,  2579,  2580,  2581,  2582,  2583,  2585,  2594,  2600,
    2604,  2612,  2613,  2617,  2618,  2619,  2620,  2621,  2622,  2623,
    2624,  2625,  2629,  2630,  2636,  2637,  2638,  2639,  2640,  2641,
    2642,  2643,  2644,  2645,  2646,  2647,  2648,  2649,  2650,  2651,
    2652,  2653,  2655,  2657,  2659,  2661,  2663,  2665,  2667,  2669,
    2671,  2673,  2675,  2677,  2679,  2681,  2683,  2685,  2687,  2689,
    2693,  2694,  2696,  2697,  2698,  2702,  2703,  2702,  2705,  2711,
    2712,  2713,  2714,  2715,  2716,  2717,  2718,  2720,  2723,  2725,
    2732,  2736,  2736,  2741,  2742,  2745,  2745,  2750,  2753,  2753,
    2762,  2762,  2772,  2771,  2782,  2785,  2786,  2787,  2788,  2789,
    2790,  2791,  2792,  2793,  2795,  2802,  2812,  2813,  2814,  2815,
    2816,  2817,  2818,  2819,  2820,  2821,  2822,  2823,  2824,  2825,
    2826,  2827,  2828,  2829,  2830,  2834,  2835,  2836,  2837,  2838,
    2839,  2840,  2843,  2843,  2860,  2860,  2878,  2890,  2904,  2926,
    2945,  2965,  2997,  3030,  3060,  3080,  3101,  3111,  3121,  3137,
    3154,  3164,  3174,  3190,  3207,  3217,  3227,  3243,  3260,  3270,
    3280,  3296,  3313,  3324,  3325,  3326,  3327,  3328,  3329,  3330,
    3331,  3332,  3333,  3334,  3335,  3336,  3337,  3338,  3339,  3340,
    3341,  3348,  3357,  3359,  3369,  3372,  3380,  3395,  3410,  3411,
    3421,  3423,  3431,  3446,  3462,  3467,  3469,  3472
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "EXPRESSION_START_", "COMMAND_START_",
  "HISTORY_", "GEOMVIEW_", "VIEW_MATRIX_", "LEAD_INTEGER_", "INTEGER_",
  "REAL_", "SIGNED_NUMBER_", "NEWIDENT_", "REDEFINE_", "MATHFUNC_",
  "MATHFUNC2_", "POW_", "USERFUNC_", "MIDV_", "DATAFILENAME_", "LOGFILE_",
  "PI_", "E_", "G_", "PARAM_", "SYMBOL_", "TOTAL_", "EXTRA_ATTRIBUTE_",
  "FIXEDVOL_", "IDENT_", "UMINUS_", "SHELL_", "COLOR_", "HESSIAN_",
  "VOLCONST_", "TORUS_PERIODS_", "VERTICES_", "EDGES_", "FACETS_",
  "BODIES_", "HESSIAN_MENU_", "POSTSCRIPT_", "LENGTH_", "AREA_", "VOLUME_",
  "ID_", "OID_", "TAG_", "ORIGINAL_", "FACETEDGES_", "WRAP_", "QUOTATION_",
  "UNSET_", "TOPINFO_", "OPACITY_", "VALENCE_", "HESSIAN_SADDLE_", "SET_",
  "FIXED_", "DENSITY_", "PRESSURE_", "CONSTRAINT_", "COORD_", "DISSOLVE_",
  "WHERE_", "LIST_", "SHOW_", "DELETE_", "REFINE_", "RECALC_", "SHOWQ_",
  "EDGESWAP_", "FIX_", "UNFIX_", "TOGGLENAME_", "TOGGLEVALUE_", "STAR_",
  "QUANTITY_NAME_", "PAUSE_", "GO_", "SHOW_VOL_", "CHECK_", "READ_",
  "ZOOM_", "ON_", "OFF_", "GEOMPIPE_", "SELF_", "SINGLE_LETTER_",
  "LONG_JIGGLE_", "RAW_VERAVG_", "COUNTS_", "CHDIR_", "ALICE_",
  "STABILITY_TEST_", "DEFINE_", "UPLUS_", "DATATYPE_", "FLUSH_COUNTS_",
  "AUTOCHOP_", "UTEST_", "ATTRIBUTE_", "RITZ_", "MOVE_", "VERTEXNORMAL_",
  "POP_", "SYSTEM_", "TETRA_POINT_", "TRIPLE_POINT_", "LANCZOS_",
  "EIGENPROBE_", "EXEC_", "AREAWEED_", "EDGEWEED_", "GRAVITY_",
  "EDGEDIVIDE_", "LINEAR_", "QUADRATIC_", "DIFFUSION_", "EXTRAPOLATE_",
  "TRANSFORM_DEPTH_", "PRINTF_", "ERRPRINTF_", "PRINT_", "MAX_", "MIN_",
  "COUNT_", "SUM_", "AVG_", "BREAK_", "CONTINUE_", "SIZEOF_",
  "TRANSFORM_EXPR_", "BARE_", "BOTTOMINFO_", "METIS_", "KMETIS_",
  "KEYLOGFILE_", "SCALE_", "BURCHARD_", "REBODY_", "BOUNDARY_",
  "ORIENTATION_", "OMETIS_", "SQ_MEAN_CURV_", "FRONTCOLOR_",
  "SINGLE_REDEFD_", "METHOD_NAME_", "TASK_EXEC_", "RAWEST_VERAVG_",
  "SINGLE_LETTER_ARG_", "BACKCOLOR_", "LAGRANGE_", "RETURN_",
  "TRANSFORM_EXPR_VERB_", "OOGLFILE_", "PARALLEL_EXEC_",
  "BINARY_OFF_FILE_", "SPRINTF_", "CONVERT_TO_QUANTS_", "METIS_FACTOR_",
  "FUNCTION_", "EXPRINT_", "DIHEDRAL_", "WRAP_VERTEX_", "ARRAYIDENT_",
  "DATE_AND_TIME_", "LOCAL_", "SHOW_EXPR_", "SHOW_TRANS_", "AXIAL_POINT_",
  "ENERGY_", "CONSERVED_", "INFO_ONLY_", "ASSIGN_", "PROCEDURE_",
  "FOREACH_", "STRINGGLOBAL_", "EQUIANGULATE_", "HISTOGRAM_",
  "LOGHISTOGRAM_", "AREA_FIXED_", "QUIT_", "WARNING_MESSAGES_", "IF_",
  "WHILE_", "DO_", "NO_REFINE_", "STRING_", "NONCONTENT_", "FOR_",
  "HIT_PARTNER_", "FRONTBODY_", "BACKBODY_", "COLORFILE_",
  "PERM_STRINGGLOBAL_", "FUNCTION_IDENT_", "THICKEN_", "COLORMAP_",
  "REDIRECT_", "NEWVERTEX_", "NEWEDGE_", "NEWFACET_", "MODULUS_",
  "TARGET_", "VALUE_", "INVERSE_PERIODS_", "NEWBODY_", "DELTA_",
  "GAP_CONSTANT_", "DUMP_", "NOTCH_", "QUANTITY_", "LOAD_",
  "PERM_PROCEDURE_", "PROCEDURE_WORD_", "DYNAMIC_LOAD_FUNC_",
  "PERM_IDENT_", "PERMLOAD_", "HELP_", "VERTEX_AVERAGE_",
  "METHOD_INSTANCE_", "RAW_VERTEX_AVERAGE_", "OPTIMIZE_", "REDIRECTOVER_",
  "TOLERANCE_", "RAWEST_VERTEX_AVERAGE_", "JIGGLE_", "VIEW_TRANSFORMS_",
  "CLOSE_SHOW_", "IS_DEFINED_", "NODISPLAY_", "PERM_ASSIGN_", "PHASE_",
  "VIEW_TRANSFORM_SWAP_COLORS_", "BACKQUOTE_COMMA_", "INTERNAL_VARIABLE_",
  "DIRICHLET_", "SOBOLEV_", "VIEW_TRANSFORM_PARITY_", "SOBOLEV_SEEK_",
  "DIRICHLET_SEEK_", "HESSIAN_SEEK_", "REORDER_STORAGE_", "RENUMBER_ALL_",
  "CONSTRAINT_NAME_", "BOUNDARY_NAME_", "PROCEDURE_IDENT_",
  "POP_TRI_TO_EDGE_", "POP_EDGE_TO_TRI_", "POP_QUAD_TO_QUAD_", "SHOWVERB_",
  "PROCEDURES_", "MPI_TASK_ATTR_", "T1_EDGESWAP_", "MERGE_EDGE_",
  "MERGE_FACET_", "MERGE_VERTEX_", "RESET_COUNTS_", "VALID_ELEMENT_",
  "MID_EDGE_", "MID_FACET_", "GO_COUNT_", "ELEMENT_IDENT_", "BODY_METIS_",
  "REVERSE_ORIENTATION_", "MATRIX_MULTIPLY_", "MATRIX_INVERSE_",
  "BINARY_PRINTF_", "DUMP_MEMLIST_", "FREE_DISCARDS_", "REPARTITION_",
  "METIS_READJUST_", "MEAN_CURVATURE_", "GLOBAL_", "LEAD_INTEGER_AT_",
  "INTEGER_AT_", "MATRIX_DETERMINANT_", "SUBCOMMAND_", "ABORT_",
  "BREAKPOINT_", "WHEREAMI_", "ADDLOAD_", "SIMPLEX_TO_FE_",
  "DISPLAY_TEXT_", "DELETE_TEXT_", "SUPPRESS_WARNING_",
  "UNSUPPRESS_WARNING_", "RESET_PROFILING_", "VALID_CONSTRAINT_",
  "VALID_BOUNDARY_", "ARRAY_ATTRIBUTE_", "PROFILING_", "','", "'.'", "';'",
  "'['", "']'", "'{'", "'}'", "'('", "')'", "'`'", "ASSIGNOP_", "PIPE_",
  "THEN_", "ELSE_", "'='", "'?'", "':'", "OR_", "AND_", "NOT_", "'>'",
  "'<'", "NE_", "GE_", "LE_", "EQ_", "ON_CONSTRAINT_", "HIT_CONSTRAINT_",
  "ON_BOUNDARY_", "ON_QUANTITY_", "ON_METHOD_INSTANCE_", "'+'", "'-'",
  "'*'", "'/'", "'%'", "DOT_", "IDIV_", "IMOD_", "'^'", "EPRINT_", "'@'",
  "$accept", "whole", "@1", "commandline", "command", "vcommand",
  "commands", "commandblock", "@2", "onecommand", "commandsemic",
  "commandterm", "commandlist", "commandlistterm", "ifhead", "@3",
  "whilehead", "@4", "dohead", "@5", "forentry", "forhead", "fortop",
  "estart", "@6", "@7", "identassign", "permidentassign", "elidassign",
  "lvalue", "rexpr", "datatype", "argident", "arglist", "argliststart",
  "protobody", "functionname", "@8", "@9", "procedurename", "@10", "@11",
  "indexset", "dimensionset", "arraydecl", "arraylvalue",
  "arraylvalueindexset", "extraat", "defextra", "@12", "@13", "newlvalue",
  "new_permlvalue", "@14", "@15", "@16", "@17", "@18", "@19",
  "localidlist", "localid", "@20", "@21", "@22", "exprlist", "printfhead",
  "binaryprintfhead", "errprintfhead", "@23", "print", "backquote", "@24",
  "elindex", "verb", "eltype", "multiple", "single", "singlep",
  "element_gen", "toggle", "quotation_concat", "stringexpr", "sprintfhead",
  "getattrib", "ggetattrib", "fullattrib", "signed_expr", "@25", "@26",
  "aggregate", "histotype", "foreachhead", "@27", "@28", "@29", "@30",
  "@31", "set", "set_attrib", "setattrib", "setattribb", "assignop", "@32",
  "@33", "unset", "unsetattrib", "conname", "bdryname", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short int yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,   416,   417,   418,   419,   420,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,   431,   432,   433,   434,
     435,   436,   437,   438,   439,   440,   441,   442,   443,   444,
     445,   446,   447,   448,   449,   450,   451,   452,   453,   454,
     455,   456,   457,   458,   459,   460,   461,   462,   463,   464,
     465,   466,   467,   468,   469,   470,   471,   472,   473,   474,
     475,   476,   477,   478,   479,   480,   481,   482,   483,   484,
     485,   486,   487,   488,   489,   490,   491,   492,   493,   494,
     495,   496,   497,   498,   499,   500,   501,   502,   503,   504,
     505,   506,   507,   508,   509,   510,   511,   512,   513,   514,
     515,   516,   517,   518,   519,   520,   521,   522,   523,   524,
     525,   526,   527,   528,   529,   530,   531,   532,   533,   534,
     535,   536,   537,   538,   539,   540,   541,   542,   543,   544,
     545,   546,   547,    44,    46,    59,    91,    93,   123,   125,
      40,    41,    96,   548,   549,   550,   551,    61,    63,    58,
     552,   553,   554,    62,    60,   555,   556,   557,   558,   559,
     560,   561,   562,   563,    43,    45,    42,    47,    37,   564,
     565,   566,    94,   567,    64
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned short int yyr1[] =
{
       0,   335,   336,   337,   336,   338,   339,   339,   339,   339,
     339,   339,   339,   340,   340,   340,   340,   340,   340,   340,
     340,   340,   340,   340,   340,   340,   339,   341,   341,   343,
     342,   342,   342,   344,   344,   339,   339,   339,   345,   345,
     346,   347,   348,   347,   348,   350,   349,   340,   340,   340,
     340,   340,   339,   339,   339,   339,   339,   339,   339,   339,
     339,   339,   339,   339,   339,   339,   339,   339,   340,   340,
     340,   340,   340,   340,   340,   340,   340,   340,   340,   340,
     340,   340,   340,   340,   340,   340,   340,   340,   340,   352,
     351,   340,   340,   354,   353,   340,   340,   339,   355,   356,
     356,   357,   357,   340,   340,   340,   356,   357,   340,   340,
     340,   340,   340,   340,   340,   340,   340,   340,   340,   340,
     340,   339,   339,   339,   340,   340,   340,   340,   340,   340,
     340,   340,   340,   340,   340,   340,   340,   340,   340,   340,
     340,   340,   340,   340,   340,   339,   339,   339,   340,   340,
     340,   340,   340,   340,   340,   340,   340,   340,   340,   340,
     340,   340,   340,   340,   340,   340,   340,   340,   340,   340,
     340,   340,   340,   340,   340,   340,   340,   340,   340,   340,
     340,   340,   340,   340,   340,   340,   340,   340,   340,   340,
     340,   340,   340,   340,   340,   340,   340,   340,   340,   340,
     340,   340,   340,   340,   340,   340,   340,   340,   340,   340,
     340,   340,   340,   340,   340,   340,   340,   340,   340,   340,
     340,   340,   340,   340,   340,   340,   340,   340,   340,   340,
     340,   340,   340,   340,   340,   358,   336,   336,   359,   339,
     339,   360,   340,   340,   340,   340,   339,   339,   340,   340,
     340,   340,   340,   340,   340,   340,   339,   339,   340,   340,
     339,   339,   339,   340,   340,   339,   339,   339,   339,   339,
     339,   339,   339,   339,   339,   339,   339,   339,   339,   339,
     339,   339,   339,   339,   339,   339,   339,   339,   339,   340,
     340,   340,   340,   340,   340,   340,   340,   340,   339,   340,
     339,   361,   362,   339,   339,   339,   339,   339,   339,   339,
     339,   363,   339,   364,   364,   339,   365,   365,   339,   339,
     339,   339,   339,   339,   366,   366,   367,   367,   367,   367,
     367,   367,   367,   367,   367,   367,   368,   369,   369,   368,
     369,   369,   369,   369,   368,   370,   370,   370,   371,   371,
     372,   373,   340,   374,   374,   375,   376,   340,   340,   340,
     340,   340,   340,   340,   340,   340,   340,   377,   377,   378,
     378,   379,   379,   379,   340,   380,   380,   380,   380,   380,
     380,   381,   340,   340,   340,   340,   340,   340,   365,   365,
     382,   382,   383,   383,   340,   384,   385,   340,   340,   340,
     340,   340,   340,   340,   386,   387,   388,   386,   386,   339,
     365,   339,   339,   339,   339,   339,   339,   389,   339,   390,
     339,   339,   391,   339,   339,   392,   339,   393,   339,   339,
     339,   339,   340,   394,   394,   395,   395,   395,   395,   395,
     395,   395,   395,   395,   395,   395,   396,   340,   397,   340,
     398,   340,   340,   340,   399,   399,   399,   399,   399,   399,
     400,   400,   401,   401,   402,   402,   340,   340,   340,   340,
     340,   340,   340,   340,   403,   340,   404,   340,   340,   340,
     340,   340,   340,   340,   340,   340,   340,   340,   340,   340,
     340,   340,   340,   405,   406,   365,   365,   365,   365,   365,
     340,   340,   340,   340,   340,   365,   365,   365,   365,   339,
     339,   339,   365,   365,   365,   365,   365,   365,   365,   365,
     407,   407,   407,   407,   407,   407,   365,   365,   365,   365,
     365,   365,   340,   340,   340,   340,   340,   340,   340,   340,
     340,   340,   340,   365,   365,   365,   365,   365,   408,   340,
     408,   340,   408,   408,   408,   340,   340,   340,   408,   340,
     408,   340,   408,   340,   408,   340,   408,   340,   408,   340,
     408,   340,   408,   340,   408,   340,   408,   340,   408,   408,
     340,   340,   340,   340,   340,   340,   409,   409,   409,   409,
     409,   410,   410,   411,   411,   411,   411,   411,   411,   412,
     412,   411,   411,   411,   413,   413,   413,   413,   413,   413,
     413,   413,   413,   413,   414,   414,   415,   415,   416,   416,
     416,   416,   416,   416,   416,   417,   417,   416,   416,   416,
     418,   418,   418,   418,   418,   418,   418,   418,   418,   418,
     418,   418,   418,   418,   418,   418,   418,   418,   418,   418,
     418,   418,   418,   418,   418,   418,   418,   418,   418,   418,
     418,   418,   418,   418,   418,   418,   418,   418,   418,   418,
     418,   418,   418,   418,   419,   420,   420,   420,   420,   420,
     420,   420,   420,   420,   420,   420,   420,   420,   420,   420,
     365,   365,   365,   365,   365,   365,   365,   365,   365,   365,
     365,   365,   365,   365,   365,   365,   365,   365,   365,   365,
     365,   365,   365,   365,   365,   365,   365,   365,   365,   365,
     365,   365,   365,   365,   365,   365,   365,   365,   365,   365,
     365,   365,   365,   365,   421,   365,   365,   365,   365,   365,
     365,   365,   365,   365,   365,   365,   365,   365,   365,   365,
     365,   365,   365,   365,   365,   365,   365,   365,   365,   365,
     365,   365,   365,   365,   365,   365,   365,   365,   365,   365,
     365,   365,   365,   365,   365,   365,   365,   365,   365,   365,
     365,   365,   365,   365,   365,   422,   423,   365,   365,   424,
     424,   424,   424,   424,   425,   425,   340,   340,   426,   340,
     340,   427,   340,   340,   340,   428,   340,   340,   429,   340,
     430,   365,   431,   340,   432,   433,   433,   433,   433,   433,
     433,   433,   433,   433,   340,   340,   434,   434,   434,   434,
     434,   434,   434,   434,   434,   434,   434,   434,   434,   434,
     434,   434,   434,   434,   434,   434,   434,   435,   435,   436,
     436,   436,   437,   339,   438,   339,   340,   340,   340,   340,
     340,   340,   340,   340,   340,   340,   340,   340,   340,   340,
     340,   340,   340,   340,   340,   340,   340,   340,   340,   340,
     340,   340,   340,   439,   440,   440,   440,   440,   440,   440,
     440,   440,   440,   440,   440,   440,   440,   440,   440,   440,
     340,   340,   441,   340,   365,   340,   340,   340,   442,   340,
     365,   340,   340,   340,   340,   336,   340,   340
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     1,     0,     3,     1,     1,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     2,     2,     1,     1,
       1,     2,     2,     1,     2,     2,     2,     1,     1,     0,
       4,     2,     3,     1,     1,     1,     2,     2,     1,     2,
       1,     1,     1,     2,     2,     0,     5,     2,     1,     3,
       2,     1,     2,     2,     1,     2,     2,     2,     1,     2,
       2,     2,     1,     2,     2,     2,     1,     2,     2,     2,
       2,     2,     2,     2,     1,     2,     1,     2,     2,     1,
       2,     2,     1,     2,     2,     1,     1,     2,     2,     0,
       4,     2,     2,     0,     3,     3,     2,     2,     3,     3,
       2,     3,     2,     2,     2,     3,     2,     2,     2,     1,
       1,     2,     2,     1,     2,     2,     2,     2,     1,     2,
       2,     2,     3,     2,     2,     3,     2,     2,     2,     2,
       2,     2,     2,     4,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     2,     2,
       2,     2,     2,     2,     6,     2,     2,     6,     2,     6,
       2,     2,     2,     2,     1,     2,     2,     1,     2,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     3,     2,     2,     2,     2,     1,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     2,     2,     3,
       2,     3,     2,     3,     2,     3,     2,     1,     2,     1,
       2,     1,     2,     1,     1,     1,     2,     3,     0,     4,
       3,     0,     4,     3,     3,     3,     3,     2,     4,     4,
       4,     4,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     2,     3,     2,     2,     2,     3,     2,     5,     5,
       5,     5,     5,     3,     3,     4,     4,     4,     4,     4,
       3,     3,     3,     3,     3,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     2,
       2,     2,     2,     3,     2,     3,     3,     2,     2,     2,
       2,     2,     2,     3,     3,     3,     3,     1,     3,     3,
       3,     2,     2,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     2,     3,     4,     2,
       2,     3,     4,     2,     2,     1,     1,     0,     1,     1,
       0,     0,     7,     1,     1,     0,     0,     6,     3,     2,
       2,     1,     3,     3,     2,     2,     2,     3,     4,     3,
       4,     4,     4,     4,     1,     1,     1,     1,     2,     2,
       2,     2,     3,     3,     3,     5,     5,     5,     3,     2,
       1,     1,     5,     5,     2,     0,     0,     7,     1,     2,
       2,     2,     2,     2,     2,     2,     0,     4,     2,     2,
       2,     3,     2,     2,     2,     2,     2,     0,     3,     0,
       5,     3,     0,     5,     3,     0,     4,     0,     4,     3,
       3,     2,     2,     1,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     4,     0,     4,
       0,     4,     2,     2,     3,     3,     1,     3,     3,     1,
       2,     2,     2,     2,     2,     2,     1,     3,     3,     1,
       3,     3,     1,     3,     0,     4,     1,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     3,     2,     2,     2,
       2,     2,     2,     1,     0,     5,     2,     3,     4,     2,
       3,     4,     2,     6,     2,     4,     2,     8,     2,     1,
       4,     2,     4,     2,     6,     2,     4,     2,     1,     2,
       1,     3,     1,     2,     1,     2,     7,     2,     4,     2,
       4,     2,     6,     2,     6,     2,     6,     2,     8,     2,
       7,     5,     3,     4,     6,     2,     5,     3,     1,     2,
       1,     2,     1,     1,     1,     2,     2,     2,     1,     2,
       1,     2,     1,     2,     1,     2,     1,     2,     1,     2,
       1,     2,     1,     2,     1,     2,     1,     2,     1,     1,
       2,     2,     2,     2,     2,     2,     1,     1,     1,     1,
       1,     1,     2,     4,     3,     4,     1,     1,     1,     2,
       1,     5,     4,     5,     1,     2,     1,     2,     2,     2,
       2,     2,     3,     3,     1,     1,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     1,     3,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       1,     2,     2,     4,     2,     3,     4,     4,     4,     2,
       1,     1,     1,     2,     2,     3,     2,     1,     1,     1,
       1,     3,     3,     1,     2,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     2,     3,     3,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       4,     2,     6,     1,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     4,     4,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       2,     2,     2,     2,     2,     0,     0,     7,     3,     1,
       1,     1,     1,     1,     1,     1,     2,     2,     1,     4,
       2,     0,     3,     1,     2,     0,     3,     2,     0,     7,
       0,     7,     0,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     3,     5,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     1,     1,
       1,     1,     0,     6,     0,     7,     4,     5,     5,     4,
       4,     6,     7,     6,     6,     7,     4,     4,     6,     6,
       4,     4,     6,     6,     3,     3,     5,     5,     3,     3,
       5,     5,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       3,     5,     1,     3,     1,     4,     6,     5,     1,     3,
       1,     4,     5,     6,     2,     1,     2,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned short int yydefact[] =
{
       0,   235,     3,     0,     0,     0,     1,   732,   731,   733,
     734,     0,     0,   743,   739,   665,   736,   737,   738,   631,
     596,     0,   673,   661,   709,     0,   650,   642,   586,   587,
     588,   589,   632,   640,   641,   647,   648,   649,   655,   590,
     666,   639,   656,   645,   646,   630,   634,   700,   636,     0,
     597,     0,   701,   376,   664,   663,   789,   790,   793,   791,
     792,     0,   669,   708,   635,   637,   651,     0,   652,   702,
     638,   375,   662,   657,   659,   658,   654,   653,     0,     0,
       0,     0,   643,     0,   713,   710,     0,   660,   670,   707,
       0,   904,   910,   644,     0,   667,   668,   598,     0,   633,
       0,     0,     0,     0,   377,     0,   493,     0,     0,     0,
       0,     0,     0,     0,   317,   236,     0,     0,     0,   600,
       0,   674,   675,   690,   735,   810,    19,     0,     0,     0,
       0,     0,   204,   211,   212,     0,     0,     0,   814,     0,
       0,     0,     0,   214,   195,     0,     0,     0,   229,     0,
     189,     0,   188,   194,     0,     0,     0,     0,   196,   197,
     170,     0,   199,   202,     0,   192,     0,   203,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   200,
     201,   180,     0,     0,     0,   476,     0,     0,     0,     0,
       0,     0,     0,   181,     0,     0,     0,     0,   198,     0,
       0,     0,     0,     0,     0,     0,   205,   206,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   171,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   234,   227,     0,   207,   209,   210,   208,     0,   175,
     176,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     193,    86,   598,     0,     0,     0,     0,   177,   178,   179,
       0,   172,   173,     0,    18,     0,   174,   509,     0,     0,
       0,   191,    38,     0,     0,    51,     4,     0,     6,     5,
       0,    40,    42,    27,     0,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   374,     0,     0,   398,     0,
       0,   466,   469,   472,     0,   812,   600,     0,   808,     0,
       0,     0,   410,   741,     0,     0,   714,     0,   781,   780,
     727,     0,   782,   699,     0,   730,     0,   499,     0,   513,
       0,   515,     0,   517,     0,   519,   694,     0,   506,     0,
     527,     0,   545,     0,     0,   508,     0,   529,     0,   531,
       0,   706,     0,   784,   783,   679,   677,   678,   685,   683,
     684,   682,   680,   681,   687,   686,   689,   688,   704,   703,
     237,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   389,   496,     0,     0,   599,   380,   692,   671,   672,
     378,     0,   691,   676,     0,    75,    55,   621,   616,   614,
     615,   624,     0,   623,   619,   622,   620,    53,   618,    52,
     627,   409,     0,   405,   322,    63,    61,    60,   323,     0,
       0,     0,   321,    69,    68,   914,     0,     0,    17,   166,
     165,   408,     0,     0,     0,     0,     0,     0,     0,     0,
     559,   549,   215,     0,   224,   217,   216,     0,   226,     0,
       0,   222,   220,   218,   551,   563,   565,   584,   580,   582,
     585,   581,   583,   228,     0,    88,    87,   120,   119,   184,
       0,    59,    57,    56,   115,   446,   114,   136,   135,   403,
     365,   364,   401,   402,     0,   366,   399,   400,     0,   267,
       0,   265,   232,   158,     0,   163,   162,   571,   128,   127,
     156,     0,   155,   161,     0,   160,   130,   129,   151,   150,
     149,   148,   247,   849,   850,   851,     0,   153,   152,   123,
       0,   121,   461,   460,   465,   464,   190,    81,    80,    84,
      83,   138,   137,   140,   139,    67,    65,    64,     0,   185,
     147,   145,   112,   450,   111,     0,   134,     0,   117,   448,
     116,   187,   186,    78,    77,   126,     0,   124,    73,    72,
     132,   131,    71,    70,   359,   324,   325,     0,   483,   504,
       0,   445,   435,   436,   441,   442,   437,   438,   440,   439,
     443,   444,   432,   433,   807,     0,   492,   491,    22,     0,
       0,    21,   800,     0,   307,   569,   796,   797,     0,    47,
      45,    92,    89,    97,     0,   104,     0,     0,   308,     0,
     261,     0,   297,   295,   264,   263,   290,   289,    25,   427,
      24,   360,   353,   354,   355,   302,   294,   293,   917,   916,
     555,   556,   300,   299,   230,   557,     0,   169,   168,   502,
       0,   573,   575,   577,   804,     0,   567,   535,     0,   537,
       0,   533,     0,   311,   144,   143,   561,   539,     0,   463,
     462,   142,   141,    16,     0,     0,     0,   292,   291,   511,
       0,   286,   285,   288,   287,     0,    31,     0,    50,    26,
       0,     0,    39,     0,    37,    36,     0,    44,     0,     0,
      96,     0,   106,   100,     0,   107,   102,     0,    19,     0,
     431,   309,   304,   310,   312,     0,   381,     0,     0,   395,
       0,   394,   414,   419,   413,     0,   412,   422,   416,   415,
       0,     0,     0,   490,   487,   489,   488,   479,     0,   482,
     477,   481,   484,   485,     0,   478,     0,   599,   379,     0,
     591,   606,   600,   604,     0,   882,     0,     0,     0,     0,
     712,   711,   316,   314,   313,   728,   722,   723,   715,   724,
     726,   725,   716,   719,   720,   717,     0,     0,     0,   729,
     718,   721,   497,   456,     0,   459,     0,     0,     0,   695,
       0,     0,   547,     0,     0,     0,     0,     0,   705,   765,
     746,   788,     0,   779,   760,   778,   759,   773,   754,   772,
     753,   774,   755,   777,   758,   776,   757,   775,   756,   763,
     744,     0,   764,   745,     0,   767,   748,   766,   747,   768,
     749,   770,   751,   769,   750,   771,   752,     0,   388,     0,
     494,   594,     0,   524,   522,     0,     0,     0,     0,   626,
     625,   617,     0,   411,   320,   318,   319,   904,     0,   253,
       0,   904,     0,   284,     0,   280,   282,   283,   281,     0,
       0,     0,   245,   244,   259,   258,   255,   254,     0,   243,
       0,   252,   221,   219,   223,   225,   273,     0,     0,     0,
       0,   183,     0,   363,   362,     0,     0,   266,     0,     0,
       0,   246,   122,   257,   256,     0,   274,     0,     0,     0,
     125,   358,   348,   349,   350,     0,     0,   806,   429,     0,
     430,   305,   303,   262,     0,     0,     0,    94,    33,   105,
       0,    98,   298,   306,   260,     0,     0,   240,     0,   500,
       0,   802,     0,     0,     0,   542,     0,    13,    14,    15,
       0,    32,     0,    10,     9,    12,    11,     8,     7,     0,
      95,    99,   101,   315,   384,   383,   382,     0,     0,     0,
     421,     0,   814,   233,     0,    93,     0,   231,   418,   424,
       0,   468,   467,   471,   470,   474,   473,   675,   813,   842,
     844,   828,   833,   834,   840,   832,   846,   837,   831,   836,
     838,   841,   839,   845,   829,   830,   826,   827,   835,   843,
     852,     0,   607,   611,   609,   605,   610,   608,   592,     0,
       0,   817,   874,   822,   823,   818,   878,   821,   815,   819,
     816,     0,     0,   820,     0,     0,   824,   848,     0,   899,
     891,   884,   890,   893,     0,   875,   895,   894,   886,     0,
     879,   896,   887,   888,   885,   897,   898,   892,     0,     0,
     889,   902,   908,   900,   903,   909,   740,     0,   697,   696,
     698,     0,   498,     0,   512,     0,   516,   693,   505,     0,
       0,   543,     0,   528,   530,     0,   761,   762,   367,     0,
       0,   525,   523,   595,     0,   593,   602,     0,     0,   629,
     628,   251,   249,   407,   250,   248,   279,   276,   275,   277,
     278,   242,     0,     0,     0,     0,     0,   372,   373,   371,
       0,   390,   391,     0,     0,     0,     0,     0,     0,   133,
       0,     0,     0,   434,     0,     0,    90,     0,     0,   356,
       0,   239,   501,     0,     0,     0,     0,   510,    30,     0,
       0,     0,   396,   369,     0,     0,     0,   475,     0,     0,
       0,   613,   612,     0,     0,     0,   866,   870,   860,     0,
     859,   847,     0,   856,     0,   905,     0,   911,     0,   867,
     871,     0,     0,     0,     0,   455,   454,   458,   457,     0,
     520,     0,   546,     0,     0,   786,   368,   495,   521,   603,
     601,     0,   272,   269,   268,   270,   393,   392,     0,     0,
       0,   271,   351,     0,     0,   339,   336,   340,   347,   343,
       0,   344,     0,     0,     0,   541,     0,   385,   386,   387,
       0,   370,   420,   423,     0,     0,     0,   876,   880,     0,
       0,     0,   858,     0,   825,     0,   857,     0,   877,     0,
     881,     0,     0,   901,   907,   912,   742,   514,     0,   544,
       0,     0,     0,   157,   154,   159,   347,   503,   326,   327,
     332,   333,   328,   329,   331,   330,   334,   335,   337,   346,
     345,   357,   341,     0,   534,   536,   532,     0,     0,   853,
       0,     0,   868,   872,   861,     0,   863,   864,     0,   906,
     913,   869,   873,   526,     0,   787,   811,   352,   342,   338,
     540,     0,   397,   855,   809,   862,   865,   507,   538
};

/* YYDEFGOTO[NTERM-NUM]. */
static const short int yydefgoto[] =
{
      -1,     3,     5,   286,   287,   288,   289,   290,   697,   937,
     291,   292,   293,   294,   295,   934,   296,   935,   297,   624,
     298,   299,   300,     4,   948,   890,   301,   302,   303,   114,
     793,   587,  1288,  1149,  1150,  1291,   924,  1141,  1276,   644,
     946,  1228,   401,   731,   305,   116,   307,  1133,   308,   977,
    1240,   309,   310,   870,   735,   981,   990,   929,   945,   602,
     603,   902,   919,   915,   794,   311,   312,   313,  1167,   314,
     117,  1100,   856,   315,   118,   761,   119,   120,   764,   512,
     428,   795,   430,   121,   122,   123,   124,   812,  1271,   125,
     318,   319,   665,   605,   759,   414,   756,   320,  1046,  1020,
    1048,   536,  1168,  1169,   321,  1073,  1074,  1075
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -1183
static const int yypact[] =
{
      92,    71,   107,   111, 31079, 32882, -1183, -1183, -1183, -1183,
   -1183,    97,     2,  -188, -1183, -1183, -1183, -1183, -1183, -1183,
   -1183,    41, -1183, -1183,  -173, 12050, -1183, -1183, -1183, -1183,
   -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183,
   -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183,  1497,
   -1183, 31079, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183,
   -1183,    30, -1183, -1183, -1183, -1183, -1183,  1851, -1183, -1183,
   -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183,    39,    62,
      72,    76, -1183,  2187, -1183, -1183,    78, -1183, -1183, -1183,
      26, -1183, -1183, -1183,    96, -1183, -1183, -1183,    98, -1183,
    -176,   101,   112,   114, -1183, 12367, -1183, 12684, 13001, 13318,
   13635,    73,    54, 13952, -1183, 35914,  -215, 33173,  -171,  -158,
   32025, -1183,  -153, -1183, -1183, -1183, -1183,  1293,  1147,     1,
   31823,     9, -1183, -1183, -1183,  1922,  1686,  4741,   827,   606,
   35925,   972,  1340, -1183, -1183,  2326,   750,  1577,    -9,  -156,
   -1183,  5058, -1183, -1183,  1167,  5375, 35405,  3473, -1183, -1183,
   -1183,  2258, -1183, -1183,  4497, -1183,  2522, -1183,   115, 14269,
    2575,  2642, 14586, 14903,  2871, 15220, 15537,    37, 15854, -1183,
   -1183, -1183, 11416,  2959,  3276,  -148,   597,   616, 16171, 16488,
   35436,   -91,   137, -1183,  5692,  3790,  -149, 16805, -1183,  4107,
   17122,  6009,  1883,  3593,  3910,  4046, -1183, -1183,   405,   -26,
     118,   514,  2595,  4139,  2839,  2711,  -102,  2912,   127,   128,
     -20, -1183, 17439, 17756, 33464,   130,   -17,   -90,    55, 35478,
   18073,  4227,  4424,    38,   -73,  4364,  1668,  3229,  3295,  3156,
    3339, -1183, -1183,   -91, -1183, -1183, -1183, -1183,  6326, -1183,
   -1183,   133,  3546,  3612,  3656, 35463,  3863,   134,   136,   138,
   -1183, -1183,   -13, 18390,  3929,   139,  4456, -1183, -1183, -1183,
   18707, -1183, -1183,   283, -1183,  4544, -1183, -1183,   140, 19024,
   19341, -1183, -1183, 32591,   162, -1183, -1183,    66, -1183, -1183,
    6643, -1183, -1183, -1183, 32300,  -140, 34337,    64, 11733, 33755,
   34626, 28226, 31079, 31079,   -91, -1183,   -81,   -91,  -110,  7603,
   28543,  -126,  -120,  -117,  8246, -1183,  -113,   202, -1183,  1356,
    3973,  1356, -1183, -1183, 31079, 31079, -1183,    42, -1183,  -155,
   -1183,   954,  -155, -1183,   921, -1183,    69, -1183, 28860, -1183,
   29494, -1183, 31079, -1183, 29494, -1183, -1183,   646, -1183, 31079,
   -1183,   242, -1183,   563,  2310, -1183, 31079, -1183, 31079, -1183,
   31079, -1183, 36358, -1183,  5129, -1183, -1183,  1255, -1183, -1183,
    1255, -1183, -1183,  1255, -1183, -1183, -1183, -1183, -1183, -1183,
   -1183, 19658, 19975, 20292, 20609, 20926, 21243, 21560, 21877, 22194,
   22511, 22828, 23145, 23462, 23779, 24096, 24413, 24730, 25047, 31079,
    2310,  -109,    53,   -56, 10782, -1183, -1183,   518, -1183, -1183,
   -1183,  -107, -1183,  -109,  -106, -1183, -1183, -1183, -1183, -1183,
   -1183, -1183,  4680, -1183, -1183, -1183, -1183, -1183,   149, -1183,
     -95, -1183,  7929, -1183, -1183, -1183, -1183, -1183, -1183,  8563,
      61, 31079, -1183, -1183, -1183, -1183, 31396,   -67, -1183, -1183,
   36858,    34, 31713,   898, 25364, 25681, 25998,    40, 26315,   -61,
   -1183, -1183, -1183, 31079, -1183, -1183, -1183, 31079, -1183,   144,
      70, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183,
   -1183, -1183, -1183, -1183,    57, -1183, 36858, -1183, -1183, -1183,
   27900, -1183, -1183, -1183, -1183,    33, 36858, -1183, -1183, -1183,
     -40,   -40, -1183, -1183,   -40, -1183, -1183, -1183,   124, -1183,
   31079, 36858, -1183, -1183, 31079, -1183, 36858, -1183, -1183, -1183,
   -1183, 12367, 36858, -1183, 12367, 36858, -1183, -1183, -1183, 36858,
   -1183, 36858, -1183, -1183, -1183, -1183, 31079, -1183, 36858, -1183,
   31079, 36858, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183,
   -1183, -1183, 36858, -1183, 36858, -1183, -1183, -1183, 26632, -1183,
   -1183, 36858, -1183,   435, 36858,    28, -1183, 35940, -1183, -1183,
   36858, -1183, 36858, -1183, 36858, -1183,   646, -1183, -1183, -1183,
   -1183, -1183, -1183, -1183, -1183, -1183, -1183,    52, -1183, -1183,
   31079, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183,
   -1183, -1183,   -51, -1183, -1183,  1356, -1183, -1183, -1183, 34915,
     246, 36858, -1183, 29494, -1183, -1183, -1183, -1183, 31079, -1183,
   36858, -1183, 36858, -1183, 32882, -1183, 34046,   646, -1183,   646,
   -1183, 31079, -1183, -1183, -1183, 36858, -1183, -1183, -1183, -1183,
   36858, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183,
   -1183, -1183, -1183, 36858, -1183, -1183, 26949, -1183, 36858, -1183,
   29177, -1183, -1183, -1183, -1183,  1356, -1183, -1183, 31079, -1183,
   31079, -1183, 31079, -1183, -1183, 36858, -1183, -1183,   899, -1183,
   -1183, -1183, 36858, -1183, 31079, 31079, 31079, -1183, -1183, -1183,
   31079, -1183, 36858, -1183, 36858,   -46, -1183, 32882, -1183, -1183,
    4681,  4773, -1183,  4851, -1183, 36858,   567, -1183, 34337,  2004,
   -1183, 31079, -1183, -1183, 35693,    10, -1183,    59, -1183,  2694,
   -1183, 36858, -1183, 36858, 36858, 31079,  -109, 31079, 31079, -1183,
   31079,   -42, -1183,   -43, 36858, 35203, -1183,   -36, 36858, -1183,
    8880,  9197,  9514, -1183, -1183, -1183, -1183, -1183,  1113, -1183,
   -1183, -1183, 36858,  -215, 32025, -1183,  1356, 35492, -1183,   -11,
    -171,    49,   233,   242,   -16, -1183, 35744, 35908, 36386, 35966,
   -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183,
   -1183, -1183, -1183, -1183, -1183, -1183,   -14,    -8,    -6, -1183,
   -1183, -1183, -1183, 35992,    -5,    -1,    12, 36018,    24,    58,
   36226,    27, -1183,    60,   106, 36044, 36414, 36442, -1183, -1183,
    4953, -1183, 31079, -1183,  4812, -1183,  5129, -1183,  5129, -1183,
    5129, -1183,  5129, -1183,  5129, -1183,  5129, -1183,  5129, -1183,
     181,    21, -1183,   181,    22, -1183,  -155, -1183,  -155, -1183,
    -155, -1183,  -155, -1183,  -155, -1183, -1183, 36252, -1183, 31079,
   -1183, -1183, 11099, -1183, -1183,   157,   119, 27266,  1356, -1183,
   -1183, -1183,  9831, -1183, -1183, -1183, 36858,   145, 31782, -1183,
     247,   151, 35800, -1183, 31079, -1183, -1183, -1183, -1183, 31079,
   31079, 31079, -1183, 36858, -1183, 36858, -1183, 36858, 31079, -1183,
   31079, -1183, 36858, 36858, -1183, -1183, -1183,   -91,   -91,   -91,
     -91, 36858, 34337,   135,   135,   135,    18, 36858, 36070, 35848,
   35888, 36858, 36858, -1183, 36858, 34337, -1183,   -91,   646, 34337,
   -1183, -1183, -1183, -1183, -1183, 36096,   514,   267, -1183, 34337,
   -1183, 36858, -1183, 36858,   148,   258,    31, -1183, -1183,   109,
     125, -1183, -1183, -1183, 36858, 34337,   132, -1183, 31079, -1183,
     159,   267, 36122, 36148, 36174, -1183,   152, 36858, 36858, 36858,
   36470, -1183,   150, -1183, -1183, -1183, -1183, -1183, -1183,  2809,
   36858, -1183, -1183, 36858, 36884,  -169, 36858,   164, 36278, 31079,
   -1183, 32882,   680,    -9,  6960, -1183,  7277, 29811, -1183, -1183,
   32882, -1183, -1183, -1183, -1183, -1183, -1183,   918,   267, -1183,
   -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183,
   -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183,
     170,  1356, -1183, -1183, -1183, -1183, -1183, -1183,  -107, 27583,
   34337, -1183,   412, -1183, -1183, -1183,   413, -1183, -1183, -1183,
   -1183,   392,   331, -1183, 30445, 35492,   416, -1183, 30445, -1183,
   -1183, -1183, -1183, -1183, 31079,   428, -1183, -1183, -1183, 31079,
     430, -1183, -1183, -1183, -1183, -1183, -1183, -1183,   418,   349,
   -1183, -1183, -1183,   433,   434,   440, -1183, 31079, -1183, -1183,
   -1183, 10148, -1183, 10465, -1183, 31079, -1183, -1183, -1183, 30128,
     932, -1183, 31079, -1183, -1183, 36832, -1183, -1183, -1183, 36304,
   31079, -1183, -1183, -1183, 31079, -1183, -1183,   245,   -23, -1183,
   -1183, -1183, -1183, -1183, -1183, -1183, 36858, 36858, 36858, 36858,
   36858, 36858, 31079, 31079, 31079, 31079,  3011,   -42,   -42,   -42,
     -40, -1183, -1183,   -40, 31079, 31079, 31079,  3126, 31079, -1183,
    3328,   132, 31079, -1183,  3443, 34337, -1183,  3645,    35, -1183,
      14, 36858, -1183, 31079, 31079, 31079,  1400, -1183, -1183, 23462,
    2310,  2310, -1183, -1183, 36330,   200,   206, -1183,   -91,  -153,
     -22, -1183, 36858,  3760, 31079, 31079,   450,   452, 31829, 30762,
     -29, -1183, 31079, 31876, 30762, 31920, 31079, 35502, 31079,   457,
     458, 31079, 31079, 31079, 36498, -1183, -1183, -1183, -1183, 36526,
   36804,   226, -1183,   223, 36200, -1183, -1183, 36858, 36858, -1183,
   -1183, 31079, 36858, 36858, 36858, 36858, -1183, -1183, 36554, 36582,
   36610, 36858, -1183, 36638,  3962, -1183, -1183,  4198,  -213, -1183,
      85, -1183, 36666, 36694, 36722, -1183,   232,  -215, -1183, -1183,
   32882, -1183, -1183, -1183, 31079,   366, 31079, 36858, 36858, 31079,
   31079, 31079, 35529, 31079, 36858, 31079, 35592, 31079, 36858, 31079,
   36858, 31079, 31079, 36858, 36858, 36858, -1183, -1183,   227, -1183,
     646, 31079, 36750, -1183, -1183, -1183,  -213, -1183, -1183, -1183,
   -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183,
   -1183, -1183, -1183,  1029, -1183, -1183, -1183,  2640,   230, 36858,
   31079, 36778, 36858, 36858, 36858, 31079, 36858, 36858, 31079, 36858,
   36858, 36858, 36858, -1183,   234,  4953, -1183, -1183, -1183, -1183,
   -1183,   236, -1183, 36858, -1183, 36858, 36858, -1183, -1183
};

/* YYPGOTO[NTERM-NUM].  */
static const short int yypgoto[] =
{
   -1183, -1183, -1183, -1183,  -282,  -202,  -101, -1182, -1183, -1183,
    -534,   240, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183,
   -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183,    75,
      -4,  -476,  -752,  -597, -1183,  -731, -1183, -1183, -1183, -1183,
   -1183, -1183,  -118,  -816, -1183,   147, -1183, -1183, -1183, -1183,
   -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183,
    -380, -1183, -1183, -1183,  -331, -1183, -1183, -1183, -1183, -1183,
   -1183, -1183,  -541, -1183,  -115, -1183,    47,   103,  -301,  -122,
   -1183,  1155, -1183, -1183,  -205,  -119, -1183, -1183, -1183, -1183,
   -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183, -1183,  -729,
   -1183,  -184, -1183, -1183, -1183, -1183, -1183, -1183
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -916
static const short int yytable[] =
{
     115,   412,   431,   323,   413,   411,   427,   558,   436,   796,
     438,   -19,   706,   798,   709,  1229,   403,   717,   719,   766,
     767,   329,  1096,  1097,   903,   904,   483,   348,   905,   916,
    1130,   333,   699,  -452,   492,  1253,  1225,  1047,   532,   641,
     337,  1029,  1029,   770,   631,  1131,  1290,   332,  1029,   508,
     642,   729,   316,   921,   -19,   376,   630,   585,   896,   656,
     699,  1022,   865,   339,   922,   710,   -41,   699,   556,   771,
     789,  -915,   613,   341,   374,   419,   420,   343,  1023,   346,
     304,   399,  1289,   533,   628,   283,  1292,  1127,  1128,  1129,
     938,   897,   941,   533,  1290,     1,     2,   350,   322,   352,
     772,   362,   355,   364,   367,   370,   373,    -2,   317,   379,
     -19,     6,   325,   357,   400,   359,   513,   654,   326,   589,
     725,   327,   727,   728,   354,   404,   699,   399,   616,   617,
     614,   625,   585,   450,   659,   667,   405,   669,   484,   671,
     677,   689,   629,   399,   546,   565,   559,   486,   586,   588,
     375,   490,   306,   496,   618,  1160,  1161,   627,  1103,   645,
     400,   673,   511,   698,   316,   516,   708,   740,   522,   525,
    1030,   529,   531,   741,   538,   432,   742,   398,   541,   850,
     773,   757,   585,   439,   552,   554,   730,   849,   726,   857,
     561,   564,   304,   567,   858,   570,   572,   574,   862,   773,
     861,   377,   411,   406,   760,   760,   760,   869,  -406,   -19,
     611,   533,   534,   891,   758,   399,   -34,   895,   620,   622,
     317,   894,   534,   586,  1024,   906,   635,   535,   640,   533,
     700,   917,  -452,   433,   -19,   653,   801,   535,    28,    29,
      30,    31,   926,   888,   658,  1025,  1209,   930,   923,   711,
     774,    39,   -19,   961,   979,   701,   980,  -452,   700,   675,
     898,   899,  1026,   989,   306,   700,   682,   399,  -452,   774,
    1211,  1246,   790,   586,   791,   692,   694,   -19,    28,    29,
      30,    31,   900,   701,   683,   643,   705,  1078,   -19,  1021,
     701,    39,  1083,  1079,   714,  1080,  1082,   721,   723,   724,
     400,   -41,   324,   440,   927,   734,   738,  1230,   -19,  1132,
     752,   -19,   441,  1084,   -19,  1231,  1181,   399,   399,   434,
     768,   769,   349,  1089,   700,  1086,   702,   442,  -452,   950,
     334,  1029,  -452,   -19,  -452,   703,  1226,  -452,   797,   338,
     534,   316,   936,   316,   940,   800,   316,   316,   -19,   701,
     400,   400,   805,  1090,   806,   535,   807,   -19,   534,  1087,
     972,   702,   340,   703,   951,   -41,   762,   762,   762,   304,
     703,   304,   342,   535,   304,   304,   344,   810,   347,   814,
     816,   818,   820,   822,   824,   826,   828,   830,   833,   836,
     838,   840,   842,   844,   846,   847,   351,   317,   353,   317,
     855,   356,   317,   317,   -19,  -361,   584,  1091,  1027,   992,
     994,   996,   358,   -19,   360,   514,  1105,   754,   590,  1111,
     702,  1113,   763,   763,   763,  1114,   969,  -794,  -795,   703,
     626,   730,  1148,   660,   668,  -453,   670,   866,   672,   678,
     690,   306,   868,   306,  1146,  1156,   306,   306,   872,  1158,
     883,   885,   887,  1145,  -520,   998,   317,   317,   684,   892,
    1152,   753,  1162,   893,   381,   382,  -854,   383,   384,  1176,
     385,   386,   387,   388,   389,   390,  1174,  1175,  1177,   685,
    1182,   391,   392,   393,   394,   395,   901,   396,   397,   398,
     760,  1104,  1186,   410,  1188,  1189,  1190,  1191,  1192,  1242,
     803,   804,   585,   317,  1193,  1243,   907,   393,   394,   395,
     908,   396,   397,   398,  1249,   591,  1250,   909,  -379,  -379,
     910,  1261,  1262,  1268,  1269,  1297,   592,   405,  1313,  1322,
     686,  1110,   911,   988,   707,  1327,   912,  1328,   831,   834,
     533,  1319,  1210,   593,  1222,  1317,  1143,   848,  1201,   997,
     760,     0,   381,   382,   914,   383,   384,  1108,   385,   386,
     387,   388,   389,   390,   802,     0,     0,   -43,   699,   391,
     392,   393,   394,   395,     0,   396,   397,   398,     0,     0,
       0,     0,  -379,     0,     0,     0,   925,     0,    20,     0,
    -361,   594,     0,   586,     0,     0,   962,   -79,   547,    28,
      29,    30,    31,     0,  -361,     0,   548,   460,     0,   931,
       0,     0,    39,     0,   933,     0,   -82,   549,     0,     0,
    1126,     0,     0,     0,     0,   550,     0,   944,     0,  -361,
       0,  -558,     0,  1137,  -453,   412,     0,  1140,     0,   411,
    -361,   760,  -558,  -558,  -558,  -558,     0,  1144,  1028,     0,
      50,     0,   762,     0,  1216,  -558,     0,  1217,     0,  -453,
       0,   595,   849,  1147,   952,   417,   953,    53,   954,   534,
    -453,   316,  1227,   316,     0,     0,     0,     0,     0,   596,
     957,   958,   959,     0,   535,     0,   960,     0,     0,   597,
       0,   598,     0,  -558,     0,     0,     0,   418,     0,   304,
    -361,   304,     0,  -379,  -361,     0,  -361,   970,   763,  -361,
     599,  -361,   762,  1122,  1123,  1124,  1125,  -379,     0,     0,
    1170,   973,     0,   974,   976,     0,   978,   317,    71,   317,
    -453,     0,     0,  1138,  -453,     0,  -453,     0,     0,  -453,
       0,   452,  -379,   760,   316,     0,     0,     0,  1173,     0,
    1196,   477,  1198,  -379,  1293,   316,     0,     0,     0,   600,
     601,     0,     0,     0,     0,     0,   700,     0,   763,     0,
       0,   306,   304,   306,     0,  -578,     0,     0,   421,   478,
       0,   317,   -79,   304,     0,     0,  -578,  -578,  -578,  -578,
       0,   701,     0,     0,   454,     0,   -79,     0,   455,  -578,
     317,   -82,   -43,   762,   422,     0,     0,     0,  1095,     0,
       0,   317,   423,  -379,  -379,   -82,     0,  -379,   456,  -379,
       0,   -79,  -379,   424,  -379,   956,    97,   479,     0,   425,
       0,     0,   -79,     0,     0,     0,     0,  -578,   317,   451,
     -82,   426,  -379,  -379,   306,  1099,     0,  -379,   329,     0,
       0,   -82,     0,  1107,   104,   306,     0,     0,     0,   763,
       0,     0,   702,  1224,     0,   654,   -43,     0,     0,  -558,
    1116,   703,     0,     0,   975,  1117,  1118,  1119,     0,   413,
    1165,     0,   306,     0,  1120,     0,  1121,     0,   452,  1166,
       0,     0,   -79,     0,     0,     0,   -79,     0,   -79,   873,
     955,   -79,     0,   -79,   453,   762,   760,     0,     0,     0,
       0,   -82,     0,     0,     0,   -82,   458,   -82,  -486,  -486,
     -82,     0,   -82,     0,    20,   459,  1179,     0,     0,     0,
    1184,     0,   874,  1202,     0,    28,    29,    30,    31,     0,
     417,   454,     0,     0,  1151,   455,     0,     0,    39,   316,
       0,     0,     0,     0,     0,   775,   875,    20,     0,     0,
       0,   763,   316,     0,     0,   456,   316,     0,    28,    29,
      30,    31,   418,   474,   457,  1164,   316,   304,     0,     0,
     611,    39,   640,   653,  1244,     0,    50,     0,   776,     0,
     304,     0,   316,     0,   304,     0,     0,  -550,     0,     0,
       0,     0,     0,    53,   304,   317,     0,     0,  -550,  -550,
    -550,  -550,   777,  -578,   778,     0,     0,     0,   317,    50,
     304,  -550,   317,     0,     0,  1172,     0,     0,   316,     0,
    1318,     0,   317,     0,     0,     0,    53,   316,     0,     0,
    1178,  1278,     0,     0,  1183,     0,     0,     0,   317,   306,
    1185,  1245,     0,   421,     0,  1187,   304,     0,  1279,  -550,
       0,  1300,   306,   458,    71,   304,   306,     0,   762,   876,
     877,   878,   459,  1194,     0,     0,   306,   316,     0,   422,
       0,  1199,     0,     0,   317,  1200,   786,   423,  1204,     0,
       0,     0,   306,   317,     0,     0,  1207,    71,   424,     0,
    1208,   879,   880,  -486,   425,   304,  1280,     0,     0,     0,
       0,     0,     0,  -480,   337,     0,   426,  -486,  1212,  1213,
    1214,  1215,     0,   881,   763,   779,   780,   781,   306,     0,
    1218,  1219,  1220,   317,  1221,     0,     0,   306,  1223,  1298,
       0,     0,  -486,     0,     0,     0,     0,   -54,   416,  1232,
    1233,  1234,     0,  -486,     0,   836,     0,   782,   783,   784,
       0,     0,    97,     0,     0,     0,   417,  -118,   487,     0,
    1247,  1248,     0,     0,     0,  1252,  1281,   306,  1254,   785,
    1256,     0,  1258,     0,  1260,     0,   417,  1263,  1264,  1265,
     104,  1180,   316,   317,  1282,    97,     0,     0,   418,     0,
       0,     0,     0,     0,  1283,     0,  1284,  1272,     0,     0,
       0,     0,   787,  -486,   399,     0,     0,  -486,   418,  -486,
     304,     0,  -486,   104,  -486,  1285,     0,     0,     0,     0,
       0,   419,   420,     0,     0,  -550,     0,  1203,     0,     0,
    1299,     0,  1301,     0,     0,  1302,  1303,  1304,   317,  1306,
       0,  1307,     0,  1309,     0,  1310,     0,  1311,  1312,   317,
       0,     0,     0,   317,   317,     0,     0,  1315,     0,     0,
       0,     0,     0,     0,  1286,  1287,     0,     0,     0,   421,
       0,     0,     0,   429,     0,   437,     0,   316,     0,     0,
     444,     0,   306,   -74,   415,     0,  1323,     0,  -480,   421,
       0,  1325,     0,  1236,  1326,   422,  1237,  1238,  1239,   488,
       0,   493,  -480,   423,     0,   304,   498,     0,     0,     0,
       0,     0,     0,     0,   424,   422,   519,     0,     0,   527,
     425,     0,   -54,   423,     0,     0,     0,  -480,   543,   545,
       0,   475,   426,   317,   424,   557,   -54,     0,  -480,     0,
     425,     0,  -118,     0,     0,     0,     0,   577,   579,   581,
     583,     0,   426,     0,     0,  -562,  -118,     0,   607,     0,
       0,   -54,     0,     0,     0,     0,  -562,  -562,  -562,  -562,
       0,    20,   -54,     0,   633,     0,   637,   306,     0,  -562,
     647,  -118,    28,    29,    30,    31,     0,     0,     0,     0,
     317,  1235,  -118,     0,     0,    39,     0,     0,  -480,     0,
       0,     0,  -480,   338,  -480,     0,     0,  -480,     0,  -480,
       0,   680,     0,     0,     0,    20,     0,  -562,     0,     0,
     688,     0,     0,     0,     0,     0,    28,    29,    30,    31,
       0,     0,   -54,    50,  1321,     0,   -54,     0,   -54,    39,
       0,   -54,     0,   -54,     0,     0,   722,     0,     0,     0,
       0,     0,  -118,     0,   736,   739,  -118,     0,  -118,   755,
       0,  -118,     0,  -118,     0,     0,     0,     0,   -74,     0,
       0,     0,     0,     0,     0,     0,     0,    50,     0,   788,
       0,     0,   -74,     0,     0,     0,     0,  -671,   330,     0,
       0,     0,   799,     0,    53,  -671,  -671,  -671,  -671,  -671,
       0,  -671,  -671,     0,  -671,  -671,     0,   -74,  -671,  -671,
    -671,  -671,  -671,  -671,  -671,  -671,  -671,  -671,   -74,  -671,
       0,  -671,     0,  -671,  -671,  -671,  -671,     0,     0,  -671,
    -671,  -671,  -671,  -671,  -671,  -671,  -671,  -671,     0,     0,
       0,  -671,  -671,     0,     0,  -671,  -671,  -671,  -671,  -671,
       0,  -671,     0,  -671,     0,    71,     0,     0,     0,     0,
       0,     0,  -671,  -671,  -671,     0,     0,   860,   480,   391,
     392,   393,   394,   395,  -671,   396,   397,   398,   -74,     0,
       0,     0,   -74,  -671,   -74,     0,  -671,   -74,     0,   -74,
       0,  -671,  -579,  -562,  -671,  -671,   481,     0,     0,     0,
       0,     0,     0,  -579,  -579,  -579,  -579,     0,     0,    97,
       0,  -671,  -671,  -671,  -671,  -671,  -579,     0,  -671,     0,
    -671,     0,     0,     0,     0,  -671,     0,     0,  -671,  -671,
       0,  -671,  -671,     0,  -671,     0,     0,     0,  -671,  -671,
       0,     0,     0,     0,   482,     0,     0,     0,     0,     0,
    -671,     0,  -671,    97,  -579,     0,     0,  -671,  -213,   648,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -671,  -671,  -671,     0,  -671,   445,  -671,  -671,
    -671,   104,     0,  -671,     0,     0,  -671,  -671,  -671,  -671,
       0,  -671,     0,     0,  -671,     0,     0,     0,     0,  -671,
       0,  -883,     0,  -671,  -671,     0,     0,     0,  -671,     0,
       0,  -671,  -883,  -883,  -883,  -883,     0,  -671,  -671,     0,
    -671,   920,  -671,  -671,     0,  -883,  -671,     0,     0,     0,
       0,     0,  -671,  -671,     0,     0,     0,   446,     0,     0,
    -671,     0,     0,     0,     0,     0,  -671,  -671,  -671,     0,
    -671,     0,     0,     0,  -671,     0,     0,     0,   932,     0,
    -671,  -671,     0,  -883,  -671,     0,     0,     0,     0,     0,
       0,  -671,   942,     0,   943,     0,  -671,  -671,  -671,     0,
    -671,   331,  -671,  -671,  -671,     0,  -671,  -671,  -671,  -671,
       0,  -671,  -671,  -671,  -671,  -671,  -671,  -671,  -671,  -671,
    -671,  -671,  -671,  -671,  -671,  -671,  -671,  -671,  -671,  -671,
    -671,  -671,  -671,  -671,  -671,  -671,     0,  -671,  -671,  -671,
    -671,  -671,     0,     0,     0,     0,     0,     0,     0,     0,
    -579,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -672,   335,   649,     0,   964,   966,     0,   968,  -672,
    -672,  -672,  -672,  -672,     0,  -672,  -672,  -213,  -672,  -672,
       0,     0,  -672,  -672,  -672,  -672,  -672,  -672,  -672,  -672,
    -672,  -672,     0,  -672,   575,  -672,     0,  -672,  -672,  -672,
    -672,     0,  -213,  -672,  -672,  -672,  -672,  -672,  -672,  -672,
    -672,  -672,   417,  -213,     0,  -672,  -672,     0,     0,  -672,
    -672,  -672,  -672,  -672,     0,  -672,     0,  -672,     0,     0,
       0,     0,     0,   443,     0,     0,  -672,  -672,  -672,     0,
       0,   447,     0,     0,   418,     0,     0,     0,  -672,     0,
       0,   417,     0,     0,     0,     0,     0,  -672,     0,  -883,
    -672,     0,     0,     0,     0,  -672,     0,     0,  -672,  -672,
       0,     0,     0,  -213,     0,     0,   448,  -213,     0,  -213,
       0,     0,  -213,   418,  -213,  -672,  -672,  -672,  -672,  -672,
       0,     0,  -672,     0,  -672,     0,     0,     0,     0,  -672,
       0,     0,  -672,  -672,     0,  -672,  -672,     0,  -672,     0,
       0,     0,  -672,  -672,   -91,   699,     0,     0,     0,     0,
       0,     0,     0,     0,  -672,   421,  -672,     0,     0,     0,
       0,  -672,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -672,  -672,  -672,     0,
    -672,   422,  -672,  -672,  -672,     0,     0,  -672,     0,   423,
    -672,  -672,  -672,  -672,   421,  -672,     0,   576,  -672,     0,
     424,     0,     0,  -672,     0,     0,   425,  -672,  -672,     0,
       0,     0,  -672,  1139,     0,  -672,     0,     0,   426,     0,
     422,  -672,  -672,     0,  -672,     0,  -672,  -672,   423,     0,
    -672,     0,     0,     0,     0,     0,  -672,  -672,     0,   424,
       0,     0,     0,     0,  -672,   425,     0,     0,     0,     0,
    -672,  -672,  -672,     0,  -672,     0,     0,   426,  -672,     0,
       0,     0,     0,     0,  -672,  -672,     0,     0,  -672,     0,
       0,     0,     0,     0,     0,  -672,     0,     0,     0,     0,
    -672,  -672,  -672,     0,  -672,   336,  -672,  -672,  -672,     0,
    -672,  -672,  -672,  -672,     0,  -672,  -672,  -672,  -672,  -672,
    -672,  -672,  -672,  -672,  -672,  -672,  -672,  -672,  -672,  -672,
    -672,  -672,  -672,  -672,  -672,  -672,  -672,  -672,  -672,  -672,
       0,  -672,  -672,  -672,  -672,  -672,     0,  -518,   345,   -91,
       0,     0,     0,     0,     0,  -518,  -518,  -518,  -518,  -518,
       0,  -518,  -518,   700,  -518,  -518,     0,     0,  -518,  -518,
    -518,  -518,  -518,  -518,  -518,  -518,  -518,  -518,     0,  -518,
       0,  -518,     0,  -518,  -518,  -518,  -518,     0,   701,  -518,
    -518,  -518,  -518,  -518,  -518,  -518,  -518,  -518,     0,   -91,
       0,  -518,  -518,     0,     0,  -518,  -518,  -518,  -518,  -518,
       0,  -518,     0,  -518,     0,     0,     0,     0,     0,   497,
       0,     0,  -518,  -518,  -518,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -518,     0,     0,   417,     0,     0,
       0,     0,     0,  -518,     0,     0,  -518,     0,     0,     0,
       0,  -518,     0,     0,  -518,  -518,     0,     0,     0,   -91,
       0,     0,     0,   -91,     0,   -91,     0,     0,   703,   418,
     -91,  -518,  -518,  -518,  -518,  -518,     0,     0,  -518,     0,
    -518,     0,     0,     0,     0,  -518,     0,   476,  -518,  -518,
       0,  -518,  -518,     0,  -518,    20,     0,     0,  -518,  -518,
       0,     0,     0,     0,     0,     0,    28,    29,    30,    31,
    -518,  -564,  -518,     0,     0,     0,     0,  -518,     0,    39,
       0,     0,  -564,  -564,  -564,  -564,     0,     0,     0,     0,
       0,     0,  -518,  -518,  -518,  -564,  -518,     0,  -518,  -518,
    -518,     0,     0,  -518,     0,     0,  -518,  -518,  -518,  -518,
     421,  -518,     0,     0,  -518,     0,     0,    50,     0,  -518,
       0,     0,     0,  -518,  -518,     0,     0,     0,  -518,     0,
       0,  -518,     0,  -564,    53,     0,   422,  -518,  -518,     0,
    -518,     0,  -518,  -518,   423,  1314,  -518,     0,     0,     0,
       0,     0,  -518,  -518,     0,   424,     0,     0,     0,     0,
    -518,   425,     0,     0,     0,     0,  -518,  -518,  -518,     0,
    -518,     0,     0,   426,  -518,     0,     0,     0,     0,     0,
    -518,  -518,     0,     0,  -518,     0,     0,     0,     0,     0,
       0,  -518,     0,     0,     0,    71,  -518,  -518,  -518,     0,
    -518,  -518,  -518,     0,  -518,     0,  -518,  -518,  -518,  -518,
       0,  -518,  -518,  -518,  -518,  -518,  -518,  -518,  -518,  -518,
    -518,  -518,  -518,  -518,  -518,  -518,  -518,  -518,  -518,  -518,
    -518,  -518,  -518,  -518,  -518,  -518,     0,  -518,  -518,  -518,
    -518,  -518,  -233,   509,     0,     0,     0,     0,     0,     0,
       7,     8,     9,    10,    11,     0,    12,    13,     0,    14,
      15,     0,     0,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,     0,    26,     0,    27,     0,    28,    29,
      30,    31,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    97,     0,     0,   517,    41,     0,     0,
      42,    43,    44,     0,    45,     0,     0,     0,    46,  -564,
       0,     0,     0,     0,     0,     0,   604,    47,    48,    49,
    -570,   104,     0,     0,     0,     0,   419,   420,     0,    50,
       0,  -570,  -570,  -570,  -570,     0,     0,     0,    51,     0,
    -805,    52,     0,     0,  -570,     0,    53,     0,     0,    54,
      55,  -805,  -805,  -805,  -805,     0,     0,     0,     0,     0,
       0,  1320,     0,   518,  -805,     0,    56,    57,    58,    59,
      60,     0,     0,    61,     0,    62,     0,     0,     0,     0,
      63,   417,  -570,     0,    64,    20,    65,    66,     0,    67,
       0,     0,     0,    68,    69,     0,    28,    29,    30,    31,
       0,     0,  -805,     0,     0,    70,     0,    71,     0,    39,
       0,     0,    72,   418,  -103,   699,   510,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -233,     0,    73,
       0,    74,   612,    75,    76,    77,     0,     0,    78,     0,
       0,  -233,    79,    80,    81,     0,    82,    50,     0,    83,
       0,     0,     0,     0,     0,     0,  -798,     0,    84,    85,
       0,     0,     0,     0,    53,     0,  -233,  -798,  -798,  -798,
    -798,     0,    86,    87,     0,    88,     0,  -233,    89,     0,
    -798,    90,     0,     0,     0,     0,     0,    91,    92,     0,
       0,     0,     0,     0,   421,    93,     0,     0,     0,     0,
       0,    94,    95,    96,     0,    97,     0,     0,     0,    98,
       0,     0,     0,     0,     0,    99,     0,     0,  -798,   100,
     422,     0,     0,     0,     0,    71,   101,     0,   423,   -49,
     699,   102,   103,   104,     0,     0,     0,  -233,     0,   424,
       0,  -233,   105,  -233,   106,   425,  -233,     0,  -233,     0,
       0,     0,     0,     0,   107,     0,     0,   426,  -570,   -20,
     608,   108,   109,   110,   111,   112,     0,     7,     8,     9,
      10,    11,     0,    12,    13,   113,    14,    15,  -805,     0,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
       0,    26,   526,    27,     0,    28,    29,    30,    31,  -103,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
     417,     0,     0,   700,    41,     0,     0,    42,    43,    44,
       0,    45,     0,    97,     0,    46,     0,     0,     0,     0,
       0,     0,     0,   615,    47,    48,    49,     0,   701,     0,
       0,     0,   418,     0,     0,     0,    50,     0,     0,  -103,
       0,   104,     0,     0,     0,    51,     0,  -568,    52,     0,
       0,     0,     0,    53,     0,     0,    54,    55,  -568,  -568,
    -568,  -568,     0,     0,     0,     0,     0,     0,     0,     0,
     542,  -568,     0,    56,    57,    58,    59,    60,     0,     0,
      61,     0,    62,     0,  -798,     0,     0,    63,   417,     0,
       0,    64,     0,    65,    66,     0,    67,     0,     0,  -103,
      68,    69,     0,  -103,   -49,  -103,     0,     0,   703,  -568,
    -103,     0,    70,   421,    71,     0,     0,     0,   700,    72,
     418,  -447,   699,   609,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   -20,     0,    73,     0,    74,   422,
      75,    76,    77,   701,     0,    78,     0,   423,   -20,    79,
      80,    81,     0,    82,   -49,     0,    83,     0,   424,     0,
       0,     0,     0,     0,   425,    84,    85,     0,     0,     0,
       0,     0,     0,   -20,     0,     0,   426,     0,     0,    86,
      87,   610,    88,     0,   -20,    89,     0,     0,    90,     0,
       0,     0,     0,     0,    91,    92,     0,     0,     0,     0,
       0,   421,    93,     0,     0,     0,     0,     0,    94,    95,
      96,     0,    97,     0,   -49,     0,    98,     0,   -49,     0,
     -49,     0,    99,   -49,     0,   -49,   100,   422,     0,     0,
       0,     0,     0,   101,     0,   423,  -451,   699,   102,   103,
     104,     0,     0,     0,   -20,     0,   424,     0,   -20,   105,
     -20,   106,   425,   -20,     0,   -20,     0,     0,     0,     0,
       0,   107,     0,     0,   426,     0,  -231,   652,   108,   109,
     110,   111,   112,     0,     7,     8,     9,    10,    11,     0,
      12,    13,   113,    14,    15,  -568,     0,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,     0,    26,     0,
      27,     0,    28,    29,    30,    31,  -447,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,     0,     0,
     700,    41,     0,     0,    42,    43,    44,     0,    45,     0,
       0,     0,    46,     0,     0,     0,     0,     0,     0,     0,
     650,    47,    48,    49,     0,   701,     0,     0,     0,     0,
     419,   420,     0,    50,     0,     0,  -447,     0,     0,     0,
       0,     0,    51,     0,  -552,    52,     0,     0,     0,     0,
      53,     0,     0,    54,    55,  -552,  -552,  -552,  -552,     0,
       0,     0,     0,     0,     0,     0,     0,   544,  -552,     0,
      56,    57,    58,    59,    60,     0,     0,    61,     0,    62,
       0,     0,     0,     0,    63,   417,   651,     0,    64,     0,
      65,    66,     0,    67,     0,     0,  -447,    68,    69,     0,
    -447,  -451,  -447,     0,     0,   703,  -552,  -447,     0,    70,
    -553,    71,     0,     0,     0,   700,    72,   418,  -449,   699,
       0,  -553,  -553,  -553,  -553,     0,     0,     0,     0,     0,
     655,  -231,     0,    73,  -553,    74,     0,    75,    76,    77,
     701,     0,    78,     0,     0,  -231,    79,    80,    81,     0,
      82,  -451,     0,    83,  -554,     0,     0,     0,     0,     0,
       0,     0,    84,    85,     0,  -554,  -554,  -554,  -554,     0,
    -231,     0,  -553,     0,     0,     0,    86,    87,  -554,    88,
       0,  -231,    89,     0,     0,    90,     0,     0,     0,     0,
       0,    91,    92,     0,     0,     0,     0,     0,   421,    93,
       0,     0,     0,     0,     0,    94,    95,    96,     0,    97,
       0,  -451,     0,    98,     0,  -451,  -554,  -451,     0,    99,
     703,     0,  -451,   100,   422,     0,     0,     0,     0,     0,
     101,     0,   423,  -426,   699,   102,   103,   104,     0,     0,
       0,  -231,     0,   424,     0,  -231,   105,  -231,   106,   425,
    -231,     0,  -231,     0,     0,     0,     0,     0,   107,     0,
       0,   426,     0,  -109,   494,   108,   109,   110,   111,   112,
       0,     7,     8,     9,    10,    11,   495,    12,    13,   113,
      14,    15,  -552,     0,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,     0,    26,     0,    27,     0,    28,
      29,    30,    31,  -449,     0,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,     0,     0,   700,    41,     0,
       0,    42,    43,    44,     0,    45,     0,     0,     0,    46,
       0,     0,     0,     0,     0,     0,     0,   661,    47,    48,
      49,     0,   701,     0,     0,     0,     0,     0,  -553,     0,
      50,     0,     0,  -449,     0,     0,     0,     0,     0,    51,
       0,  -572,    52,     0,     0,     0,     0,    53,     0,     0,
      54,    55,  -572,  -572,  -572,  -572,     0,     0,     0,     0,
       0,     0,     0,     0,   578,  -572,     0,    56,    57,    58,
      59,    60,  -554,     0,    61,     0,    62,     0,     0,     0,
       0,    63,   417,   662,     0,    64,     0,    65,    66,     0,
      67,     0,     0,  -449,    68,    69,     0,  -449,  -426,  -449,
       0,     0,   703,  -572,  -449,     0,    70,  -574,    71,     0,
       0,     0,   700,    72,   418,  -428,   699,     0,  -574,  -574,
    -574,  -574,     0,     0,     0,     0,     0,   663,  -109,     0,
      73,  -574,    74,     0,    75,    76,    77,   701,     0,    78,
       0,     0,  -109,    79,    80,    81,     0,    82,  -426,     0,
      83,  -576,     0,     0,     0,     0,     0,     0,     0,    84,
      85,     0,  -576,  -576,  -576,  -576,     0,  -109,     0,  -574,
       0,     0,     0,    86,    87,  -576,    88,     0,  -109,    89,
       0,     0,    90,     0,     0,     0,     0,     0,    91,    92,
       0,     0,     0,     0,     0,   421,    93,     0,     0,     0,
       0,     0,    94,    95,    96,     0,    97,     0,  -426,     0,
      98,     0,  -426,  -576,  -426,     0,    99,   703,     0,  -426,
     100,   422,     0,     0,     0,     0,     0,   101,     0,   423,
    -799,   699,   102,   103,   104,     0,     0,     0,  -109,     0,
     424,     0,  -109,   105,  -109,   106,   425,  -109,     0,  -109,
       0,     0,     0,     0,     0,   107,     0,     0,   426,     0,
    -110,   562,   108,   109,   110,   111,   112,     0,     7,     8,
       9,    10,    11,   563,    12,    13,   113,    14,    15,  -572,
       0,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,     0,    26,     0,    27,     0,    28,    29,    30,    31,
    -428,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,     0,     0,   700,    41,     0,     0,    42,    43,
      44,     0,    45,     0,     0,     0,    46,     0,     0,     0,
       0,     0,     0,     0,   666,    47,    48,    49,     0,   701,
       0,     0,     0,     0,     0,  -574,     0,    50,     0,     0,
    -428,     0,     0,     0,     0,     0,    51,     0,  -566,    52,
       0,     0,     0,     0,    53,     0,     0,    54,    55,  -566,
    -566,  -566,  -566,     0,     0,     0,     0,     0,     0,     0,
       0,   580,  -566,     0,    56,    57,    58,    59,    60,  -576,
       0,    61,     0,    62,     0,     0,     0,     0,    63,   417,
     676,     0,    64,     0,    65,    66,     0,    67,     0,     0,
    -428,    68,    69,     0,  -428,  -799,  -428,     0,     0,   703,
    -566,  -428,     0,    70,  -560,    71,     0,     0,     0,   700,
      72,   418,   -46,   699,     0,  -560,  -560,  -560,  -560,     0,
       0,     0,     0,     0,   765,  -110,     0,    73,  -560,    74,
       0,    75,    76,    77,   701,     0,    78,     0,     0,  -110,
      79,    80,    81,     0,    82,  -799,     0,    83,    20,     0,
       0,     0,     0,     0,     0,     0,    84,    85,     0,    28,
      29,    30,    31,     0,  -110,     0,  -560,     0,     0,     0,
      86,    87,    39,    88,     0,  -110,    89,     0,     0,    90,
       0,     0,     0,     0,     0,    91,    92,     0,     0,     0,
       0,     0,   421,    93,     0,     0,     0,   582,     0,    94,
      95,    96,     0,    97,     0,  -799,     0,    98,     0,  -799,
      50,  -799,     0,    99,   703,   417,  -799,   100,   422,     0,
       0,     0,     0,     0,   101,     0,   423,     0,     0,   102,
     103,   104,     0,     0,     0,  -110,     0,   424,     0,  -110,
     105,  -110,   106,   425,  -110,     0,  -110,   418,     0,     0,
       0,     0,   107,     0,     0,   426,     0,  -113,   568,   108,
     109,   110,   111,   112,     0,     7,     8,     9,    10,    11,
     569,    12,    13,   113,    14,    15,  -566,     0,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,     0,    26,
     606,    27,     0,    28,    29,    30,    31,   -46,     0,    32,
      33,    34,    35,    36,    37,    38,    39,    40,   417,     0,
       0,   700,    41,     0,     0,    42,    43,    44,     0,    45,
       0,     0,     0,    46,     0,     0,     0,     0,   421,     0,
       0,     0,    47,    48,    49,     0,   701,     0,     0,     0,
     418,     0,  -560,     0,    50,     0,     0,   -46,     0,     0,
       0,     0,     0,    51,   422,     0,    52,     0,     0,     0,
    1278,    53,   423,     0,    54,    55,     0,     0,     0,     0,
       0,     0,     0,   424,     0,     0,     0,  1279,   636,   425,
       0,    56,    57,    58,    59,    60,    97,     0,    61,     0,
      62,   426,     0,     0,     0,    63,   417,     0,     0,    64,
       0,    65,    66,     0,    67,     0,     0,   -46,    68,    69,
       0,   -46,     0,   -46,     0,     0,   -46,     0,   -46,     0,
      70,   421,    71,     0,     0,  1280,     0,    72,   418,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -113,     0,    73,     0,    74,   422,    75,    76,
      77,     0,     0,    78,     0,   423,  -113,    79,    80,    81,
       0,    82,     0,     0,    83,     0,   424,     0,     0,     0,
       0,     0,   425,    84,    85,     0,     0,     0,     0,     0,
       0,  -113,     0,     0,   426,     0,     0,    86,    87,     0,
      88,     0,  -113,    89,     0,  1281,    90,     0,     0,     0,
       0,     0,    91,    92,     0,     0,     0,     0,     0,   421,
      93,     0,     0,  1282,     0,   646,    94,    95,    96,     0,
      97,     0,     0,  1283,    98,  1284,     0,     0,     0,     0,
      99,     0,     0,   417,   100,   422,     0,     0,     0,     0,
       0,   101,     0,   423,  1285,     0,   102,   103,   104,     0,
       0,     0,  -113,     0,   424,     0,  -113,   105,  -113,   106,
     425,  -113,     0,  -113,     0,   418,     0,     0,     0,   107,
       0,     0,   426,     0,   -23,   638,   108,   109,   110,   111,
     112,     0,     7,     8,     9,    10,    11,     0,    12,    13,
     113,    14,    15,  1286,  1287,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,     0,    26,   679,    27,     0,
      28,    29,    30,    31,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,   417,     0,     0,     0,    41,
       0,     0,    42,    43,    44,     0,    45,     0,     0,     0,
      46,     0,     0,     0,     0,     0,   421,     0,   499,    47,
      48,    49,     0,     0,     0,     0,     0,   418,     0,   500,
       0,    50,     0,     0,     0,     0,     0,     0,     0,     0,
      51,     0,   422,    52,     0,     0,   501,     0,    53,     0,
     423,    54,    55,    28,    29,    30,    31,     0,     0,     0,
       0,   424,     0,     0,     0,   687,    39,   425,    56,    57,
      58,    59,    60,     0,     0,    61,     0,    62,   502,   426,
       0,     0,    63,   417,     0,     0,    64,     0,    65,    66,
       0,    67,     0,     0,     0,    68,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    70,   421,    71,
       0,     0,     0,     0,    72,   418,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   -23,
       0,    73,     0,    74,   422,    75,    76,    77,     0,     0,
      78,     0,   423,   -23,    79,    80,    81,     0,    82,     0,
       0,    83,     0,   424,     0,     0,     0,     0,   503,   425,
      84,    85,     0,     0,     0,     0,     0,     0,   -23,     0,
       0,   426,     0,     0,    86,    87,   639,    88,     0,   -23,
      89,     0,   504,    90,     0,     0,     0,     0,     0,    91,
      92,     0,     0,     0,   505,     0,   421,    93,     0,     0,
       0,   859,   963,    94,    95,    96,     0,    97,     0,     0,
       0,    98,     0,     0,     0,     0,     0,    99,     0,   417,
     417,   100,   422,     0,     0,     0,     0,     0,   101,   506,
     423,     0,     0,   102,   103,   104,     0,     0,   507,   -23,
       0,   424,     0,   -23,   105,   -23,   106,   425,   -23,     0,
     -23,   418,   418,     0,     0,     0,   107,     0,     0,   426,
       0,  -164,   449,   108,   109,   110,   111,   112,     0,     7,
       8,     9,    10,    11,     0,    12,    13,   113,    14,    15,
       0,     0,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,     0,    26,   965,    27,     0,    28,    29,    30,
      31,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,   417,     0,     0,     0,    41,     0,     0,    42,
      43,    44,     0,    45,     0,     0,     0,    46,     0,     0,
       0,     0,   421,   421,     0,     0,    47,    48,    49,     0,
       0,     0,     0,     0,   418,     0,     0,     0,    50,     0,
       0,     0,     0,     0,     0,     0,     0,    51,   422,   422,
      52,     0,     0,     0,     0,    53,   423,   423,    54,    55,
       0,     0,   967,     0,     0,     0,     0,   424,   424,     0,
       0,     0,     0,   425,   425,    56,    57,    58,    59,    60,
     417,     0,    61,     0,    62,   426,   426,     0,     0,    63,
       0,     0,     0,    64,     0,    65,    66,     0,    67,     0,
       0,     0,    68,    69,     0,     0,     0,     0,     0,     0,
       0,     0,   418,     0,    70,   421,    71,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -164,     0,    73,     0,
      74,   422,    75,    76,    77,     0,     0,    78,     0,   423,
    -164,    79,    80,    81,     0,    82,     0,     0,    83,     0,
     424,     0,     0,     0,     0,     0,   425,    84,    85,     0,
       0,     0,     0,     0,     0,  -164,     0,     0,   426,     0,
       0,    86,    87,     0,    88,     0,  -164,    89,     0,     0,
      90,     0,     0,   421,     0,     0,    91,    92,     0,     0,
       0,     0,     0,     0,    93,     0,     0,     0,     0,     0,
      94,    95,    96,     0,    97,     0,     0,     0,    98,   422,
       0,     0,     0,     0,    99,     0,     0,   423,   100,     0,
       0,     0,     0,     0,     0,   101,     0,     0,   424,     0,
     102,   103,   104,     0,   425,     0,  -164,     0,     0,     0,
    -164,   105,  -164,   106,     0,  -164,   426,  -164,     0,     0,
       0,     0,     0,   107,     0,     0,     0,     0,   -85,   485,
     108,   109,   110,   111,   112,     0,     7,     8,     9,    10,
      11,     0,    12,    13,   113,    14,    15,     0,     0,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,     0,
      26,     0,    27,     0,    28,    29,    30,    31,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
       0,     0,     0,    41,     0,     0,    42,    43,    44,     0,
      45,     0,     0,   384,    46,   385,   386,   387,   388,   389,
     390,     0,     0,    47,    48,    49,   391,   392,   393,   394,
     395,     0,   396,   397,   398,    50,     0,     0,     0,     0,
       0,     0,     0,     0,    51,     0,     0,    52,     0,     0,
       0,     0,    53,     0,     0,    54,    55,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    56,    57,    58,    59,    60,     0,     0,    61,
       0,    62,     0,     0,     0,     0,    63,     0,     0,     0,
      64,     0,    65,    66,     0,    67,     0,     0,     0,    68,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    70,     0,    71,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   -85,     0,    73,     0,    74,     0,    75,
      76,    77,     0,     0,    78,     0,     0,   -85,    79,    80,
      81,   382,    82,   383,   384,    83,   385,   386,   387,   388,
     389,   390,     0,     0,    84,    85,     0,   391,   392,   393,
     394,   395,   -85,   396,   397,   398,     0,     0,    86,    87,
       0,    88,     0,   -85,    89,     0,     0,    90,     0,     0,
       0,     0,     0,    91,    92,     0,     0,     0,     0,     0,
       0,    93,     0,     0,     0,     0,     0,    94,    95,    96,
       0,    97,     0,     0,     0,    98,     0,     0,     0,     0,
       0,    99,     0,     0,     0,   100,     0,     0,     0,     0,
       0,     0,   101,     0,     0,     0,     0,   102,   103,   104,
       0,     0,     0,   -85,     0,     0,     0,   -85,   105,   -85,
     106,     0,   -85,     0,   -85,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,  -182,   489,   108,   109,   110,
     111,   112,     0,     7,     8,     9,    10,    11,     0,    12,
      13,   113,    14,    15,     0,     0,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,     0,    26,     0,    27,
       0,    28,    29,    30,    31,     0,     0,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,     0,     0,     0,
      41,     0,     0,    42,    43,    44,     0,    45,     0,     0,
       0,    46,   385,   386,   387,   388,   389,   390,     0,     0,
      47,    48,    49,   391,   392,   393,   394,   395,     0,   396,
     397,   398,    50,     0,     0,     0,     0,     0,     0,     0,
       0,    51,     0,     0,    52,     0,     0,     0,     0,    53,
       0,     0,    54,    55,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    56,
      57,    58,    59,    60,     0,     0,    61,     0,    62,     0,
       0,     0,     0,    63,     0,     0,     0,    64,     0,    65,
      66,     0,    67,     0,     0,     0,    68,    69,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    70,     0,
      71,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -182,     0,    73,     0,    74,     0,    75,    76,    77,     0,
       0,    78,     0,     0,  -182,    79,    80,    81,     0,    82,
       0,     0,    83,     0,     0,     0,     0,     0,     0,     0,
       0,    84,    85,     0,     0,     0,     0,     0,     0,  -182,
       0,     0,     0,     0,     0,    86,    87,     0,    88,     0,
    -182,    89,     0,     0,    90,     0,     0,     0,     0,     0,
      91,    92,     0,     0,     0,     0,     0,     0,    93,     0,
       0,     0,     0,     0,    94,    95,    96,     0,    97,     0,
       0,     0,    98,     0,     0,     0,     0,     0,    99,     0,
       0,     0,   100,     0,     0,     0,     0,     0,     0,   101,
       0,     0,     0,     0,   102,   103,   104,     0,     0,     0,
    -182,     0,     0,     0,  -182,   105,  -182,   106,     0,  -182,
       0,  -182,     0,     0,     0,     0,     0,   107,     0,     0,
       0,     0,  -146,   560,   108,   109,   110,   111,   112,     0,
       7,     8,     9,    10,    11,     0,    12,    13,   113,    14,
      15,     0,     0,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,     0,    26,     0,    27,     0,    28,    29,
      30,    31,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,     0,     0,     0,    41,     0,     0,
      42,    43,    44,     0,    45,     0,     0,     0,    46,     0,
       0,     0,     0,     0,     0,     0,     0,    47,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
       0,     0,     0,     0,     0,     0,     0,     0,    51,     0,
       0,    52,     0,     0,     0,     0,    53,     0,     0,    54,
      55,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    56,    57,    58,    59,
      60,     0,     0,    61,     0,    62,     0,     0,     0,     0,
      63,     0,     0,     0,    64,     0,    65,    66,     0,    67,
       0,     0,     0,    68,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    70,     0,    71,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -146,     0,    73,
       0,    74,     0,    75,    76,    77,     0,     0,    78,     0,
       0,  -146,    79,    80,    81,     0,    82,     0,     0,    83,
       0,     0,     0,     0,     0,     0,     0,     0,    84,    85,
       0,     0,     0,     0,     0,     0,  -146,     0,     0,     0,
       0,     0,    86,    87,     0,    88,     0,  -146,    89,     0,
       0,    90,     0,     0,     0,     0,     0,    91,    92,     0,
       0,     0,     0,     0,     0,    93,     0,     0,     0,     0,
       0,    94,    95,    96,     0,    97,     0,     0,     0,    98,
       0,     0,     0,     0,     0,    99,     0,     0,     0,   100,
       0,     0,     0,     0,     0,     0,   101,     0,     0,     0,
       0,   102,   103,   104,     0,     0,     0,  -146,     0,     0,
       0,  -146,   105,  -146,   106,     0,  -146,     0,  -146,     0,
       0,     0,     0,     0,   107,     0,     0,     0,     0,   -76,
     573,   108,   109,   110,   111,   112,     0,     7,     8,     9,
      10,    11,     0,    12,    13,   113,    14,    15,     0,     0,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
       0,    26,     0,    27,     0,    28,    29,    30,    31,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,     0,     0,     0,    41,     0,     0,    42,    43,    44,
       0,    45,     0,     0,     0,    46,     0,     0,     0,     0,
       0,     0,     0,     0,    47,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,     0,     0,     0,
       0,     0,     0,     0,     0,    51,     0,     0,    52,     0,
       0,     0,     0,    53,     0,     0,    54,    55,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    56,    57,    58,    59,    60,     0,     0,
      61,     0,    62,     0,     0,     0,     0,    63,     0,     0,
       0,    64,     0,    65,    66,     0,    67,     0,     0,     0,
      68,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    70,     0,    71,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   -76,     0,    73,     0,    74,     0,
      75,    76,    77,     0,     0,    78,     0,     0,   -76,    79,
      80,    81,     0,    82,     0,     0,    83,     0,     0,     0,
       0,     0,     0,     0,     0,    84,    85,     0,     0,     0,
       0,     0,     0,   -76,     0,     0,     0,     0,     0,    86,
      87,     0,    88,     0,   -76,    89,     0,     0,    90,     0,
       0,     0,     0,     0,    91,    92,     0,     0,     0,     0,
       0,     0,    93,     0,     0,     0,     0,     0,    94,    95,
      96,     0,    97,     0,     0,     0,    98,     0,     0,     0,
       0,     0,    99,     0,     0,     0,   100,     0,     0,     0,
       0,     0,     0,   101,     0,     0,     0,     0,   102,   103,
     104,     0,     0,     0,   -76,     0,     0,     0,   -76,   105,
     -76,   106,     0,   -76,     0,   -76,     0,     0,     0,     0,
       0,   107,     0,     0,     0,     0,  -167,   657,   108,   109,
     110,   111,   112,     0,     7,     8,     9,    10,    11,     0,
      12,    13,   113,    14,    15,     0,     0,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,     0,    26,     0,
      27,     0,    28,    29,    30,    31,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,     0,     0,
       0,    41,     0,     0,    42,    43,    44,     0,    45,     0,
       0,     0,    46,     0,     0,     0,     0,     0,     0,     0,
       0,    47,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,     0,     0,     0,     0,     0,     0,
       0,     0,    51,     0,     0,    52,     0,     0,     0,     0,
      53,     0,     0,    54,    55,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      56,    57,    58,    59,    60,     0,     0,    61,     0,    62,
       0,     0,     0,     0,    63,     0,     0,     0,    64,     0,
      65,    66,     0,    67,     0,     0,     0,    68,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    70,
       0,    71,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -167,     0,    73,     0,    74,     0,    75,    76,    77,
       0,     0,    78,     0,     0,  -167,    79,    80,    81,     0,
      82,     0,     0,    83,     0,     0,     0,     0,     0,     0,
       0,     0,    84,    85,     0,     0,     0,     0,     0,     0,
    -167,     0,     0,     0,     0,     0,    86,    87,     0,    88,
       0,  -167,    89,     0,     0,    90,     0,     0,     0,     0,
       0,    91,    92,     0,     0,     0,     0,     0,     0,    93,
       0,     0,     0,     0,     0,    94,    95,    96,     0,    97,
       0,     0,     0,    98,     0,     0,     0,     0,     0,    99,
       0,     0,     0,   100,     0,     0,     0,     0,     0,     0,
     101,     0,     0,     0,     0,   102,   103,   104,     0,     0,
       0,  -167,     0,     0,     0,  -167,   105,  -167,   106,     0,
    -167,     0,  -167,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,   -35,   704,   108,   109,   110,   111,   112,
       0,     7,     8,     9,    10,    11,     0,    12,    13,   113,
      14,    15,     0,     0,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,     0,    26,     0,    27,     0,    28,
      29,    30,    31,     0,     0,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,     0,     0,     0,    41,     0,
       0,    42,    43,    44,     0,    45,     0,     0,     0,    46,
       0,     0,     0,     0,     0,     0,     0,     0,    47,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      50,     0,     0,     0,     0,     0,     0,     0,     0,    51,
       0,     0,    52,     0,     0,     0,     0,    53,     0,     0,
      54,    55,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    56,    57,    58,
      59,    60,     0,     0,    61,     0,    62,     0,     0,     0,
       0,    63,     0,     0,     0,    64,     0,    65,    66,     0,
      67,     0,     0,     0,    68,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    70,     0,    71,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   -35,     0,
      73,     0,    74,     0,    75,    76,    77,     0,     0,    78,
       0,     0,   -35,    79,    80,    81,     0,    82,     0,     0,
      83,     0,     0,     0,     0,     0,     0,     0,     0,    84,
      85,     0,     0,     0,     0,     0,     0,   -35,     0,     0,
       0,     0,     0,    86,    87,     0,    88,     0,   -35,    89,
       0,     0,    90,     0,     0,     0,     0,     0,    91,    92,
       0,     0,     0,     0,     0,     0,    93,     0,     0,     0,
       0,     0,    94,    95,    96,     0,    97,     0,     0,     0,
      98,     0,     0,     0,     0,     0,    99,     0,     0,     0,
     100,     0,     0,     0,     0,     0,     0,   101,     0,     0,
       0,     0,   102,   103,   104,     0,     0,     0,   -35,     0,
       0,     0,   -35,   105,   -35,   106,     0,   -35,     0,   -35,
       0,     0,     0,     0,     0,   107,     0,     0,     0,     0,
     -20,   608,   108,   109,   110,   111,   112,     0,     7,     8,
       9,    10,    11,     0,    12,    13,   113,    14,    15,     0,
       0,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,     0,    26,     0,    27,     0,    28,    29,    30,    31,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,     0,     0,     0,    41,     0,     0,    42,    43,
      44,     0,    45,     0,     0,     0,    46,     0,     0,     0,
       0,     0,     0,     0,     0,    47,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    50,     0,     0,
       0,     0,     0,     0,     0,     0,    51,     0,     0,    52,
       0,     0,     0,     0,    53,     0,     0,    54,    55,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    56,    57,    58,    59,    60,     0,
       0,    61,     0,    62,     0,     0,     0,     0,    63,     0,
       0,     0,    64,     0,    65,    66,     0,    67,     0,     0,
       0,    68,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    70,     0,    71,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   -20,     0,    73,     0,    74,
       0,    75,    76,    77,     0,     0,    78,     0,     0,   -20,
      79,    80,    81,     0,    82,     0,     0,    83,     0,     0,
       0,     0,     0,     0,     0,     0,    84,    85,     0,     0,
       0,     0,     0,     0,   -20,     0,     0,     0,     0,     0,
      86,    87,     0,    88,     0,   -20,    89,     0,     0,    90,
       0,     0,     0,     0,     0,    91,    92,     0,     0,     0,
       0,     0,     0,    93,     0,     0,     0,     0,     0,    94,
      95,    96,     0,    97,     0,     0,     0,    98,     0,     0,
       0,     0,     0,    99,     0,     0,     0,   100,     0,     0,
       0,     0,     0,     0,   101,     0,     0,     0,     0,   102,
     103,   104,     0,     0,     0,   -20,     0,     0,     0,   -20,
     105,   -20,   106,     0,   -20,     0,   -20,     0,     0,     0,
       0,     0,   107,     0,     0,     0,     0,   -23,   638,   108,
     109,   110,   111,   112,     0,     7,     8,     9,    10,    11,
       0,    12,    13,   113,    14,    15,     0,     0,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,     0,    26,
       0,    27,     0,    28,    29,    30,    31,     0,     0,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,     0,
       0,     0,    41,     0,     0,    42,    43,    44,     0,    45,
       0,     0,     0,    46,     0,     0,     0,     0,     0,     0,
       0,     0,    47,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,     0,     0,     0,     0,     0,
       0,     0,     0,    51,     0,     0,    52,     0,     0,     0,
       0,    53,     0,     0,    54,    55,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    56,    57,    58,    59,    60,     0,     0,    61,     0,
      62,     0,     0,     0,     0,    63,     0,     0,     0,    64,
       0,    65,    66,     0,    67,     0,     0,     0,    68,    69,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      70,     0,    71,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   -23,     0,    73,     0,    74,     0,    75,    76,
      77,     0,     0,    78,     0,     0,   -23,    79,    80,    81,
       0,    82,     0,     0,    83,     0,     0,     0,     0,     0,
       0,     0,     0,    84,    85,     0,     0,     0,     0,     0,
       0,   -23,     0,     0,     0,     0,     0,    86,    87,     0,
      88,     0,   -23,    89,     0,     0,    90,     0,     0,     0,
       0,     0,    91,    92,     0,     0,     0,     0,     0,     0,
      93,     0,     0,     0,     0,     0,    94,    95,    96,     0,
      97,     0,     0,     0,    98,     0,     0,     0,     0,     0,
      99,     0,     0,     0,   100,     0,     0,     0,     0,     0,
       0,   101,     0,     0,     0,     0,   102,   103,   104,     0,
       0,     0,   -23,     0,     0,     0,   -23,   105,   -23,   106,
       0,   -23,     0,   -23,     0,     0,     0,     0,     0,   107,
       0,     0,     0,     0,     0,     0,   108,   109,   110,   111,
     112,     0,     0,     0,   732,     0,     0,     0,  -417,     0,
     113,     7,     8,     9,    10,    11,     0,    12,    13,     0,
      14,    15,   417,     0,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,  -417,    26,  -417,    27,     0,    28,
      29,    30,    31,  -417,  -417,    32,    33,    34,    35,    36,
      37,    38,    39,    40,   418,  -417,     0,     0,    41,  -417,
    -417,    42,    43,    44,     0,    45,  -417,     0,  -417,    46,
    -417,  -417,  -417,  -417,  -417,  -417,  -417,  -417,    47,    48,
      49,  -417,  -417,  -417,  -417,  -417,  -417,     0,     0,     0,
      50,  -417,  -417,  -417,  -417,  -417,  -417,  -417,  -417,    51,
       0,  -417,    52,  -417,     0,  -417,  -417,    53,  -417,  -417,
      54,    55,  -417,  -417,  -417,  -417,  -417,     0,  -417,  -417,
    -417,     0,  -417,     0,  -417,  -417,  -417,    56,    57,    58,
      59,    60,  -417,  -417,    61,   421,    62,     0,  -417,  -417,
       0,    63,  -417,  -417,     0,    64,     0,    65,    66,  -417,
      67,  -417,  -417,  -417,    68,    69,  -417,  -417,  -417,  -417,
    -417,   422,  -417,  -417,  -417,  -417,    70,  -417,    71,   423,
    -417,  -417,  -417,    72,     0,     0,     0,     0,  -417,  -417,
     424,  -417,  -417,  -417,     0,  -417,   425,  -417,  -417,  -417,
      73,     0,    74,  -417,    75,    76,    77,     0,   426,    78,
       0,     0,     0,    79,    80,    81,     0,    82,     0,     0,
      83,     0,     0,  -417,  -417,     0,  -417,  -417,  -417,    84,
      85,  -417,  -417,  -417,     0,  -417,  -417,     0,     0,  -417,
    -417,     0,  -417,    86,    87,     0,    88,     0,     0,    89,
    -417,  -417,    90,  -417,  -417,  -417,  -417,  -417,    91,    92,
    -417,  -417,  -417,  -417,  -417,     0,    93,  -417,  -417,  -417,
    -417,  -417,    94,    95,    96,  -417,    97,  -417,  -417,  -417,
      98,  -417,  -417,  -417,  -417,  -417,    99,     0,     0,     0,
     100,  -417,  -417,  -417,  -417,  -417,  -417,   101,     0,     0,
       0,  -417,   102,   103,   104,     0,     0,     0,     0,     0,
       0,   733,     0,   105,     0,   106,     0,     0,     0,  -417,
       0,  -417,     0,     0,     0,   107,     0,     0,     0,     0,
       0,     0,   108,   109,   110,   111,   112,     0,     0,     0,
     863,     0,     0,     0,  -404,     0,   113,  -404,  -404,  -404,
    -404,  -404,     0,  -404,  -404,     0,  -404,  -404,  -404,     0,
    -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,
    -404,  -404,  -404,  -404,     0,  -404,  -404,  -404,  -404,  -404,
    -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,
    -404,  -404,     0,     0,  -404,  -404,  -404,  -404,  -404,  -404,
       0,  -404,  -404,     0,  -404,  -404,  -404,  -404,  -404,  -404,
    -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,
    -404,  -404,  -404,     0,     0,     0,  -404,  -404,  -404,  -404,
    -404,  -404,  -404,  -404,  -404,  -404,     0,  -404,  -404,  -404,
       0,  -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,
    -404,  -404,  -404,     0,  -404,  -404,  -404,     0,  -404,     0,
    -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,
    -404,  -404,  -404,     0,  -404,  -404,     0,  -404,  -404,  -404,
       0,  -404,     0,  -404,  -404,  -404,  -404,  -404,  -404,  -404,
    -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,
    -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,
       0,     0,     0,     0,  -404,  -404,  -404,  -404,  -404,  -404,
       0,  -404,  -404,  -404,  -404,  -404,  -404,     0,  -404,  -404,
    -404,  -404,  -404,     0,  -404,  -404,     0,     0,     0,  -404,
    -404,  -404,     0,  -404,     0,     0,  -404,     0,     0,  -404,
    -404,     0,  -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,
       0,  -404,  -404,     0,     0,  -404,  -404,     0,  -404,  -404,
    -404,     0,  -404,     0,     0,  -404,  -404,  -404,  -404,  -404,
    -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,
    -404,     0,  -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,
    -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,  -404,
    -404,  -404,  -404,     0,     0,     0,  -404,  -404,  -404,  -404,
    -404,  -404,  -404,  -404,     0,     0,     0,  -404,  -404,  -404,
    -404,     0,     0,     0,     0,     0,     0,  -404,     0,  -404,
       0,  -404,     0,     0,     0,  -404,     0,  -404,     0,     0,
       0,  -404,     0,     0,     0,     0,     0,   743,  -404,  -404,
    -404,  -404,  -404,     0,     7,     8,     9,    10,    11,     0,
      12,    13,  -404,    14,    15,   417,     0,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,     0,    26,     0,
      27,     0,    28,    29,    30,    31,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,   418,     0,     0,
       0,    41,     0,     0,    42,    43,    44,     0,    45,     0,
       0,     0,    46,     0,     0,     0,     0,     0,     0,     0,
       0,    47,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,   744,     0,     0,     0,     0,     0,
       0,     0,    51,     0,     0,    52,     0,     0,     0,     0,
      53,     0,     0,    54,    55,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      56,    57,    58,    59,    60,     0,     0,    61,   421,    62,
       0,     0,     0,     0,    63,     0,     0,     0,    64,     0,
      65,    66,   745,    67,     0,     0,   746,    68,    69,     0,
       0,     0,     0,     0,   422,     0,     0,     0,     0,    70,
       0,    71,   423,     0,     0,     0,    72,     0,     0,     0,
       0,   747,     0,   424,     0,     0,     0,     0,     0,   425,
       0,     0,     0,    73,     0,    74,     0,    75,    76,    77,
       0,   426,   748,     0,     0,     0,    79,    80,    81,     0,
      82,     0,     0,    83,     0,     0,     0,     0,     0,     0,
     749,   750,    84,    85,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    86,    87,     0,    88,
       0,     0,    89,     0,     0,    90,     0,     0,     0,     0,
       0,    91,    92,   751,     0,     0,     0,     0,     0,    93,
       0,     0,     0,     0,     0,    94,    95,    96,     0,    97,
       0,     0,     0,    98,     0,     0,     0,     0,     0,    99,
       0,     0,     0,   100,     0,     0,     0,     0,     0,     0,
     101,     0,     0,     0,     0,   102,   103,   104,     0,     0,
       0,     0,     0,     0,     0,     0,   105,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,     0,   864,   108,   109,   110,   111,   112,
       0,  -301,  -301,  -301,  -301,  -301,     0,  -301,  -301,   113,
    -301,  -301,  -301,     0,  -301,  -301,  -301,  -301,  -301,  -301,
    -301,  -301,  -301,  -301,     0,  -301,     0,  -301,     0,  -301,
    -301,  -301,  -301,     0,     0,  -301,  -301,  -301,  -301,  -301,
    -301,  -301,  -301,  -301,  -301,     0,     0,     0,  -301,     0,
       0,  -301,  -301,  -301,     0,  -301,     0,     0,     0,  -301,
       0,     0,     0,     0,     0,     0,     0,     0,  -301,  -301,
    -301,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -301,     0,     0,     0,     0,     0,     0,     0,     0,  -301,
       0,     0,  -301,     0,     0,     0,     0,  -301,     0,     0,
    -301,  -301,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -301,  -301,  -301,
    -301,  -301,     0,     0,  -301,  -301,  -301,     0,     0,     0,
       0,  -301,     0,     0,     0,  -301,     0,  -301,  -301,     0,
    -301,     0,     0,     0,  -301,  -301,     0,     0,     0,     0,
       0,  -301,     0,     0,     0,     0,  -301,     0,  -301,  -301,
       0,     0,     0,  -301,     0,     0,     0,     0,     0,     0,
    -301,     0,     0,     0,     0,     0,  -301,     0,     0,     0,
    -301,     0,  -301,     0,  -301,  -301,  -301,     0,  -301,  -301,
       0,     0,     0,  -301,  -301,  -301,     0,  -301,     0,     0,
    -301,     0,     0,     0,     0,     0,     0,     0,     0,  -301,
    -301,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -301,  -301,     0,  -301,     0,     0,  -301,
       0,     0,  -301,     0,     0,     0,     0,     0,  -301,  -301,
       0,     0,     0,     0,     0,     0,  -301,     0,     0,     0,
       0,     0,  -301,  -301,  -301,     0,  -301,     0,     0,     0,
    -301,     0,     0,     0,     0,     0,  -301,     0,     0,     0,
    -301,     0,     0,     0,     0,     0,     0,  -301,     0,     0,
       0,     0,  -301,  -301,  -301,     0,     0,     0,     0,     0,
       0,  -301,     0,  -301,     0,  -301,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -301,     0,     0,     0,     0,
       0,   991,  -301,  -301,  -301,  -301,  -301,     0,     7,     8,
       9,    10,    11,     0,    12,    13,  -301,    14,    15,   417,
       0,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,     0,    26,     0,    27,     0,    28,    29,    30,    31,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,   418,     0,     0,     0,    41,     0,     0,    42,    43,
      44,     0,    45,     0,     0,     0,    46,     0,     0,     0,
       0,     0,     0,     0,     0,    47,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    50,     0,     0,
       0,     0,     0,     0,     0,     0,    51,     0,     0,    52,
       0,     0,     0,     0,    53,     0,     0,    54,    55,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    56,    57,    58,    59,    60,     0,
       0,    61,   421,    62,     0,     0,     0,     0,    63,     0,
       0,     0,    64,     0,    65,    66,     0,    67,     0,     0,
       0,    68,    69,     0,     0,     0,     0,     0,   422,     0,
       0,     0,     0,    70,     0,    71,   423,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,   424,     0,     0,
       0,     0,     0,   425,     0,     0,     0,    73,     0,    74,
       0,    75,    76,    77,     0,   426,    78,     0,     0,     0,
      79,    80,    81,     0,    82,     0,     0,    83,     0,     0,
       0,     0,     0,     0,     0,     0,    84,    85,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      86,    87,     0,    88,     0,     0,    89,     0,     0,    90,
       0,     0,     0,     0,     0,    91,    92,     0,     0,     0,
       0,     0,     0,    93,     0,     0,     0,     0,     0,    94,
      95,    96,     0,    97,     0,     0,     0,    98,     0,     0,
       0,     0,     0,    99,     0,     0,     0,   100,     0,     0,
       0,     0,     0,     0,   101,     0,     0,     0,     0,   102,
     103,   104,     0,     0,     0,     0,     0,     0,     0,     0,
     105,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   107,     0,     0,     0,     0,     0,   993,   108,
     109,   110,   111,   112,     0,     7,     8,     9,    10,    11,
       0,    12,    13,   113,    14,    15,   417,     0,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,     0,    26,
       0,    27,     0,    28,    29,    30,    31,     0,     0,    32,
      33,    34,    35,    36,    37,    38,    39,    40,   418,     0,
       0,     0,    41,     0,     0,    42,    43,    44,     0,    45,
       0,     0,     0,    46,     0,     0,     0,     0,     0,     0,
       0,     0,    47,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,     0,     0,     0,     0,     0,
       0,     0,     0,    51,     0,     0,    52,     0,     0,     0,
       0,    53,     0,     0,    54,    55,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    56,    57,    58,    59,    60,     0,     0,    61,   421,
      62,     0,     0,     0,     0,    63,     0,     0,     0,    64,
       0,    65,    66,     0,    67,     0,     0,     0,    68,    69,
       0,     0,     0,     0,     0,   422,     0,     0,     0,     0,
      70,     0,    71,   423,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,   424,     0,     0,     0,     0,     0,
     425,     0,     0,     0,    73,     0,    74,     0,    75,    76,
      77,     0,   426,    78,     0,     0,     0,    79,    80,    81,
       0,    82,     0,     0,    83,     0,     0,     0,     0,     0,
       0,     0,     0,    84,    85,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    86,    87,     0,
      88,     0,     0,    89,     0,     0,    90,     0,     0,     0,
       0,     0,    91,    92,     0,     0,     0,     0,     0,     0,
      93,     0,     0,     0,     0,     0,    94,    95,    96,     0,
      97,     0,     0,     0,    98,     0,     0,     0,     0,     0,
      99,     0,     0,     0,   100,     0,     0,     0,     0,     0,
       0,   101,     0,     0,     0,     0,   102,   103,   104,     0,
       0,     0,     0,     0,     0,     0,     0,   105,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   107,
       0,     0,     0,     0,     0,   995,   108,   109,   110,   111,
     112,     0,     7,     8,     9,    10,    11,     0,    12,    13,
     113,    14,    15,   417,     0,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,     0,    26,     0,    27,     0,
      28,    29,    30,    31,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,   418,     0,     0,     0,    41,
       0,     0,    42,    43,    44,     0,    45,     0,     0,     0,
      46,     0,     0,     0,     0,     0,     0,     0,     0,    47,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,     0,     0,     0,     0,     0,     0,     0,     0,
      51,     0,     0,    52,     0,     0,     0,     0,    53,     0,
       0,    54,    55,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    56,    57,
      58,    59,    60,     0,     0,    61,   421,    62,     0,     0,
       0,     0,    63,     0,     0,     0,    64,     0,    65,    66,
       0,    67,     0,     0,     0,    68,    69,     0,     0,     0,
       0,     0,   422,     0,     0,     0,     0,    70,     0,    71,
     423,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,   424,     0,     0,     0,     0,     0,   425,     0,     0,
       0,    73,     0,    74,     0,    75,    76,    77,     0,   426,
      78,     0,     0,     0,    79,    80,    81,     0,    82,     0,
       0,    83,     0,     0,     0,     0,     0,     0,     0,     0,
      84,    85,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    86,    87,     0,    88,     0,     0,
      89,     0,     0,    90,     0,     0,     0,     0,     0,    91,
      92,     0,     0,     0,     0,     0,     0,    93,     0,     0,
       0,     0,     0,    94,    95,    96,     0,    97,     0,     0,
       0,    98,     0,     0,     0,     0,     0,    99,     0,     0,
       0,   100,     0,     0,     0,     0,     0,     0,   101,     0,
       0,     0,     0,   102,   103,   104,     0,     0,     0,     0,
       0,     0,     0,     0,   105,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   107,     0,     0,     0,
       0,     0,  1109,   108,   109,   110,   111,   112,     0,     7,
       8,     9,    10,    11,     0,    12,    13,   113,    14,    15,
     417,     0,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,     0,    26,     0,    27,     0,    28,    29,    30,
      31,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,   418,     0,     0,     0,    41,     0,     0,    42,
      43,    44,     0,    45,     0,     0,     0,    46,     0,     0,
       0,     0,     0,     0,     0,     0,    47,    48,    49,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    50,     0,
       0,     0,     0,     0,     0,     0,     0,    51,     0,     0,
      52,     0,     0,     0,     0,    53,     0,     0,    54,    55,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    56,    57,    58,    59,    60,
       0,     0,    61,   421,    62,     0,     0,     0,     0,    63,
       0,     0,     0,    64,     0,    65,    66,     0,    67,     0,
       0,     0,    68,    69,     0,     0,     0,     0,     0,   422,
       0,     0,     0,     0,    70,     0,    71,   423,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,   424,     0,
       0,     0,     0,     0,   425,     0,     0,     0,    73,     0,
      74,     0,    75,    76,    77,     0,   426,    78,     0,     0,
       0,    79,    80,    81,     0,    82,     0,     0,    83,     0,
       0,     0,     0,     0,     0,     0,     0,    84,    85,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    86,    87,     0,    88,     0,     0,    89,     0,     0,
      90,     0,     0,     0,     0,     0,    91,    92,     0,     0,
       0,     0,     0,     0,    93,     0,     0,     0,     0,     0,
      94,    95,    96,     0,    97,     0,     0,     0,    98,     0,
       0,     0,     0,     0,    99,     0,     0,     0,   100,     0,
       0,     0,     0,     0,     0,   101,     0,     0,     0,     0,
     102,   103,   104,     0,     0,     0,     0,     0,     0,     0,
       0,   105,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   107,     0,     0,     0,     0,     0,  1195,
     108,   109,   110,   111,   112,     0,     7,     8,     9,    10,
      11,     0,    12,    13,   113,    14,    15,   417,     0,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,     0,
      26,     0,    27,     0,    28,    29,    30,    31,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,   418,
       0,     0,     0,    41,     0,     0,    42,    43,    44,     0,
      45,     0,     0,     0,    46,     0,     0,     0,     0,     0,
       0,     0,     0,    47,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,     0,     0,     0,     0,
       0,     0,     0,     0,    51,     0,     0,    52,     0,     0,
       0,     0,    53,     0,     0,    54,    55,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    56,    57,    58,    59,    60,     0,     0,    61,
     421,    62,     0,     0,     0,     0,    63,     0,     0,     0,
      64,     0,    65,    66,     0,    67,     0,     0,     0,    68,
      69,     0,     0,     0,     0,     0,   422,     0,     0,     0,
       0,    70,     0,    71,   423,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,   424,     0,     0,     0,     0,
       0,   425,     0,     0,     0,    73,     0,    74,     0,    75,
      76,    77,     0,   426,    78,     0,     0,     0,    79,    80,
      81,     0,    82,     0,     0,    83,     0,     0,     0,     0,
       0,     0,     0,     0,    84,    85,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    86,    87,
       0,    88,     0,     0,    89,     0,     0,    90,     0,     0,
       0,     0,     0,    91,    92,     0,     0,     0,     0,     0,
       0,    93,     0,     0,     0,     0,     0,    94,    95,    96,
       0,    97,     0,     0,     0,    98,     0,     0,     0,     0,
       0,    99,     0,     0,     0,   100,     0,     0,     0,     0,
       0,     0,   101,     0,     0,     0,     0,   102,   103,   104,
       0,     0,     0,     0,     0,     0,     0,     0,   105,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,     0,  1197,   108,   109,   110,
     111,   112,     0,     7,     8,     9,    10,    11,     0,    12,
      13,   113,    14,    15,   417,     0,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,     0,    26,     0,    27,
       0,    28,    29,    30,    31,     0,     0,    32,    33,    34,
      35,    36,    37,    38,    39,    40,   418,     0,     0,     0,
      41,     0,     0,    42,    43,    44,     0,    45,     0,     0,
       0,    46,     0,     0,     0,     0,     0,     0,     0,     0,
      47,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    50,     0,     0,     0,     0,     0,     0,     0,
       0,    51,     0,     0,    52,     0,     0,     0,     0,    53,
       0,     0,    54,    55,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    56,
      57,    58,    59,    60,     0,     0,    61,   421,    62,     0,
       0,     0,     0,    63,     0,     0,     0,    64,     0,    65,
      66,     0,    67,     0,     0,     0,    68,    69,     0,     0,
       0,     0,     0,   422,     0,     0,     0,     0,    70,     0,
      71,   423,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,   424,     0,     0,     0,     0,     0,   425,     0,
       0,     0,    73,     0,    74,     0,    75,    76,    77,     0,
     426,    78,     0,     0,     0,    79,    80,    81,     0,    82,
       0,     0,    83,     0,     0,     0,     0,     0,     0,     0,
       0,    84,    85,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    86,    87,     0,    88,     0,
       0,    89,     0,     0,    90,     0,     0,     0,     0,     0,
      91,    92,     0,     0,     0,     0,     0,     0,    93,     0,
       0,     0,     0,     0,    94,    95,    96,     0,    97,     0,
       0,     0,    98,     0,     0,     0,     0,     0,    99,     0,
       0,     0,   100,     0,     0,     0,     0,     0,     0,   101,
       0,     0,     0,     0,   102,   103,   104,     0,     0,     0,
       0,     0,     0,     0,     0,   105,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   107,     0,     0,
       0,     0,     0,   851,   108,   109,   110,   111,   112,     0,
       7,     8,     9,    10,    11,     0,    12,    13,   113,    14,
      15,     0,     0,    16,    17,    18,    19,    20,    21,    22,
      23,    24,   852,     0,    26,     0,    27,     0,    28,    29,
      30,    31,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,     0,     0,     0,    41,     0,     0,
      42,    43,    44,     0,    45,     0,     0,     0,    46,     0,
       0,     0,     0,     0,     0,     0,     0,    47,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
       0,     0,     0,     0,     0,     0,     0,     0,    51,     0,
       0,    52,     0,     0,     0,     0,    53,     0,     0,    54,
      55,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    56,    57,    58,    59,
      60,     0,     0,    61,     0,    62,     0,     0,     0,     0,
      63,     0,     0,     0,    64,     0,    65,    66,     0,    67,
       0,     0,     0,    68,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    70,     0,    71,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,    74,     0,    75,    76,    77,     0,     0,    78,     0,
       0,     0,    79,    80,    81,     0,    82,     0,     0,    83,
       0,     0,     0,     0,     0,     0,     0,     0,    84,    85,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    86,    87,     0,    88,     0,     0,    89,     0,
       0,    90,     0,     0,     0,     0,     0,    91,    92,     0,
       0,     0,     0,     0,     0,    93,     0,     0,     0,     0,
       0,    94,    95,    96,     0,    97,     0,     0,     0,    98,
       0,     0,     0,     0,     0,    99,     0,   853,   854,   100,
       0,     0,     0,     0,     0,     0,   101,     0,     0,     0,
       0,   102,   103,   104,     0,     0,     0,     0,     0,     0,
       0,     0,   105,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   107,     0,     0,     0,     0,     0,
     328,   108,   109,   110,   111,   112,     0,     7,     8,     9,
      10,    11,     0,    12,    13,   113,    14,    15,     0,     0,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
       0,    26,     0,    27,     0,    28,    29,    30,    31,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,     0,     0,     0,    41,     0,     0,    42,    43,    44,
       0,    45,     0,     0,     0,    46,     0,     0,     0,     0,
       0,     0,     0,     0,    47,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,     0,     0,     0,
       0,     0,     0,     0,     0,    51,     0,     0,    52,     0,
       0,     0,     0,    53,     0,     0,    54,    55,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    56,    57,    58,    59,    60,     0,     0,
      61,     0,    62,     0,     0,     0,     0,    63,     0,     0,
       0,    64,     0,    65,    66,     0,    67,     0,     0,     0,
      68,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    70,     0,    71,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,    74,     0,
      75,    76,    77,     0,     0,    78,     0,     0,     0,    79,
      80,    81,     0,    82,     0,     0,    83,     0,     0,     0,
       0,     0,     0,     0,     0,    84,    85,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    86,
      87,     0,    88,     0,     0,    89,     0,     0,    90,     0,
       0,     0,     0,     0,    91,    92,     0,     0,     0,     0,
       0,     0,    93,     0,     0,     0,     0,     0,    94,    95,
      96,     0,    97,     0,     0,     0,    98,     0,     0,     0,
       0,     0,    99,     0,  1101,  1102,   100,     0,     0,     0,
       0,     0,     0,   101,     0,     0,     0,     0,   102,   103,
     104,     0,     0,     0,     0,     0,     0,     0,     0,   105,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   107,     0,     0,     0,     0,     0,   539,   108,   109,
     110,   111,   112,     0,     7,     8,     9,    10,    11,     0,
      12,    13,   113,    14,    15,     0,     0,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,     0,    26,     0,
      27,     0,    28,    29,    30,    31,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,     0,     0,
       0,    41,     0,     0,    42,    43,    44,     0,    45,     0,
       0,     0,    46,     0,     0,     0,     0,     0,     0,     0,
       0,    47,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,     0,     0,     0,     0,     0,     0,
       0,     0,    51,     0,     0,    52,     0,     0,     0,     0,
      53,     0,     0,    54,    55,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      56,    57,    58,    59,    60,     0,     0,    61,     0,    62,
       0,     0,     0,     0,    63,     0,     0,     0,    64,     0,
      65,    66,     0,    67,     0,     0,     0,    68,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    70,
       0,    71,     0,     0,     0,     0,    72,     0,     0,     0,
     540,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,    74,     0,    75,    76,    77,
       0,     0,    78,     0,     0,     0,    79,    80,    81,     0,
      82,     0,     0,    83,     0,     0,     0,     0,     0,     0,
       0,     0,    84,    85,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    86,    87,     0,    88,
       0,     0,    89,     0,     0,    90,     0,     0,     0,     0,
       0,    91,    92,     0,     0,     0,     0,     0,     0,    93,
       0,     0,     0,     0,     0,    94,    95,    96,     0,    97,
       0,     0,     0,    98,     0,     0,     0,     0,     0,    99,
       0,     0,     0,   100,     0,     0,     0,     0,     0,     0,
     101,     0,     0,     0,     0,   102,   103,   104,     0,     0,
       0,     0,     0,     0,     0,     0,   105,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,     0,   712,   108,   109,   110,   111,   112,
       0,     7,     8,     9,    10,    11,     0,    12,    13,   113,
      14,    15,     0,     0,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,     0,    26,     0,    27,     0,    28,
      29,    30,    31,     0,     0,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,     0,     0,     0,    41,     0,
       0,    42,    43,    44,     0,    45,     0,     0,     0,    46,
       0,     0,     0,     0,     0,     0,     0,     0,    47,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      50,     0,     0,     0,     0,     0,     0,     0,     0,    51,
       0,     0,    52,     0,     0,     0,     0,    53,     0,     0,
      54,    55,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    56,    57,    58,
      59,    60,     0,     0,    61,     0,    62,     0,     0,     0,
       0,    63,     0,     0,     0,    64,     0,    65,    66,     0,
      67,     0,     0,     0,    68,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    70,     0,    71,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,    74,     0,    75,    76,    77,     0,     0,    78,
       0,     0,     0,    79,    80,    81,     0,    82,     0,     0,
      83,     0,     0,     0,     0,     0,     0,     0,     0,    84,
      85,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    86,    87,     0,    88,     0,     0,    89,
       0,     0,    90,     0,     0,     0,     0,     0,    91,    92,
       0,     0,     0,     0,     0,     0,    93,     0,     0,     0,
       0,     0,    94,    95,    96,     0,    97,     0,     0,     0,
      98,     0,     0,     0,     0,     0,    99,     0,     0,     0,
     100,     0,     0,     0,     0,     0,     0,   101,     0,     0,
       0,     0,   102,   103,   104,     0,     0,     0,   713,     0,
       0,     0,     0,   105,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   107,     0,     0,     0,     0,
       0,   328,   108,   109,   110,   111,   112,     0,     7,     8,
       9,    10,    11,     0,    12,    13,   113,    14,    15,     0,
       0,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,     0,    26,     0,    27,     0,    28,    29,    30,    31,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,     0,     0,     0,    41,     0,     0,    42,    43,
      44,     0,    45,     0,     0,     0,    46,     0,     0,     0,
       0,     0,     0,     0,     0,    47,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    50,     0,     0,
       0,     0,     0,     0,     0,     0,    51,     0,     0,    52,
       0,     0,     0,     0,    53,     0,     0,    54,    55,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    56,    57,    58,    59,    60,     0,
       0,    61,     0,    62,     0,     0,     0,     0,    63,     0,
       0,     0,    64,     0,    65,    66,     0,    67,     0,     0,
       0,    68,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    70,     0,    71,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,    74,
       0,    75,    76,    77,     0,     0,    78,     0,     0,     0,
      79,    80,    81,     0,    82,     0,     0,    83,     0,     0,
       0,     0,     0,     0,     0,     0,    84,    85,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      86,    87,     0,    88,     0,     0,    89,     0,     0,    90,
       0,     0,     0,     0,     0,    91,    92,     0,     0,     0,
       0,     0,     0,    93,     0,     0,     0,     0,     0,    94,
      95,    96,     0,    97,     0,     0,     0,    98,     0,     0,
       0,     0,     0,    99,     0,     0,     0,   100,     0,     0,
       0,     0,     0,     0,   101,     0,     0,     0,     0,   102,
     103,   104,     0,     0,     0,     0,     0,     0,     0,     0,
     105,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   107,     0,     0,     0,     0,     0,   361,   108,
     109,   110,   111,   112,     0,     7,     8,     9,    10,    11,
       0,    12,    13,   113,    14,    15,     0,     0,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,     0,    26,
       0,    27,     0,    28,    29,    30,    31,     0,     0,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,     0,
       0,     0,    41,     0,     0,    42,    43,    44,     0,    45,
       0,     0,     0,    46,     0,     0,     0,     0,     0,     0,
       0,     0,    47,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,     0,     0,     0,     0,     0,
       0,     0,     0,    51,     0,     0,    52,     0,     0,     0,
       0,    53,     0,     0,    54,    55,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    56,    57,    58,    59,    60,     0,     0,    61,     0,
      62,     0,     0,     0,     0,    63,     0,     0,     0,    64,
       0,    65,    66,     0,    67,     0,     0,     0,    68,    69,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      70,     0,    71,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,    74,     0,    75,    76,
      77,     0,     0,    78,     0,     0,     0,    79,    80,    81,
       0,    82,     0,     0,    83,     0,     0,     0,     0,     0,
       0,     0,     0,    84,    85,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    86,    87,     0,
      88,     0,     0,    89,     0,     0,    90,     0,     0,     0,
       0,     0,    91,    92,     0,     0,     0,     0,     0,     0,
      93,     0,     0,     0,     0,     0,    94,    95,    96,     0,
      97,     0,     0,     0,    98,     0,     0,     0,     0,     0,
      99,     0,     0,     0,   100,     0,     0,     0,     0,     0,
       0,   101,     0,     0,     0,     0,   102,   103,   104,     0,
       0,     0,     0,     0,     0,     0,     0,   105,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   107,
       0,     0,     0,     0,     0,   363,   108,   109,   110,   111,
     112,     0,     7,     8,     9,    10,    11,     0,    12,    13,
     113,    14,    15,     0,     0,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,     0,    26,     0,    27,     0,
      28,    29,    30,    31,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,     0,     0,     0,    41,
       0,     0,    42,    43,    44,     0,    45,     0,     0,     0,
      46,     0,     0,     0,     0,     0,     0,     0,     0,    47,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,     0,     0,     0,     0,     0,     0,     0,     0,
      51,     0,     0,    52,     0,     0,     0,     0,    53,     0,
       0,    54,    55,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    56,    57,
      58,    59,    60,     0,     0,    61,     0,    62,     0,     0,
       0,     0,    63,     0,     0,     0,    64,     0,    65,    66,
       0,    67,     0,     0,     0,    68,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    70,     0,    71,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,    74,     0,    75,    76,    77,     0,     0,
      78,     0,     0,     0,    79,    80,    81,     0,    82,     0,
       0,    83,     0,     0,     0,     0,     0,     0,     0,     0,
      84,    85,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    86,    87,     0,    88,     0,     0,
      89,     0,     0,    90,     0,     0,     0,     0,     0,    91,
      92,     0,     0,     0,     0,     0,     0,    93,     0,     0,
       0,     0,     0,    94,    95,    96,     0,    97,     0,     0,
       0,    98,     0,     0,     0,     0,     0,    99,     0,     0,
       0,   100,     0,     0,     0,     0,     0,     0,   101,     0,
       0,     0,     0,   102,   103,   104,     0,     0,     0,     0,
       0,     0,     0,     0,   105,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   107,     0,     0,     0,
       0,     0,   365,   108,   109,   110,   111,   112,     0,     7,
       8,     9,    10,    11,     0,    12,    13,   113,    14,    15,
       0,     0,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,     0,    26,     0,    27,     0,    28,    29,    30,
      31,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,     0,     0,     0,    41,     0,     0,    42,
      43,    44,     0,    45,     0,     0,     0,    46,     0,     0,
       0,     0,     0,     0,     0,     0,    47,    48,    49,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    50,     0,
       0,     0,     0,     0,     0,     0,     0,    51,     0,     0,
      52,     0,     0,     0,     0,    53,     0,     0,    54,    55,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    56,    57,    58,    59,    60,
       0,     0,    61,     0,    62,     0,     0,     0,     0,    63,
       0,     0,     0,    64,     0,    65,    66,     0,    67,     0,
       0,     0,    68,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    70,     0,    71,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
      74,     0,    75,    76,    77,     0,     0,    78,     0,     0,
       0,    79,    80,    81,     0,    82,     0,     0,    83,     0,
       0,     0,     0,     0,     0,     0,     0,    84,    85,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    86,    87,     0,    88,     0,     0,    89,     0,     0,
      90,     0,     0,     0,     0,     0,   366,    92,     0,     0,
       0,     0,     0,     0,    93,     0,     0,     0,     0,     0,
      94,    95,    96,     0,    97,     0,     0,     0,    98,     0,
       0,     0,     0,     0,    99,     0,     0,     0,   100,     0,
       0,     0,     0,     0,     0,   101,     0,     0,     0,     0,
     102,   103,   104,     0,     0,     0,     0,     0,     0,     0,
       0,   105,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   107,     0,     0,     0,     0,     0,   368,
     108,   109,   110,   111,   112,     0,     7,     8,     9,    10,
      11,     0,    12,    13,   113,    14,    15,     0,     0,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,     0,
      26,     0,    27,     0,    28,    29,    30,    31,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
       0,     0,     0,    41,     0,     0,    42,    43,    44,     0,
      45,     0,     0,     0,    46,     0,     0,     0,     0,     0,
       0,     0,     0,    47,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,     0,     0,     0,     0,
       0,     0,     0,     0,    51,     0,     0,    52,     0,     0,
       0,     0,    53,     0,     0,    54,    55,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    56,    57,    58,    59,    60,     0,     0,    61,
       0,    62,     0,     0,     0,     0,    63,     0,     0,     0,
      64,     0,    65,    66,     0,    67,     0,     0,     0,    68,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    70,     0,    71,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,    74,     0,    75,
      76,    77,     0,     0,    78,     0,     0,     0,    79,    80,
      81,     0,    82,     0,     0,    83,     0,     0,     0,     0,
       0,     0,     0,     0,    84,    85,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    86,    87,
       0,    88,     0,     0,    89,     0,     0,    90,     0,     0,
       0,     0,     0,   369,    92,     0,     0,     0,     0,     0,
       0,    93,     0,     0,     0,     0,     0,    94,    95,    96,
       0,    97,     0,     0,     0,    98,     0,     0,     0,     0,
       0,    99,     0,     0,     0,   100,     0,     0,     0,     0,
       0,     0,   101,     0,     0,     0,     0,   102,   103,   104,
       0,     0,     0,     0,     0,     0,     0,     0,   105,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,     0,   371,   108,   109,   110,
     111,   112,     0,     7,     8,     9,    10,    11,     0,    12,
      13,   113,    14,    15,     0,     0,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,     0,    26,     0,    27,
       0,    28,    29,    30,    31,     0,     0,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,     0,     0,     0,
      41,     0,     0,    42,    43,    44,     0,    45,     0,     0,
       0,    46,     0,     0,     0,     0,     0,     0,     0,     0,
      47,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    50,     0,     0,     0,     0,     0,     0,     0,
       0,    51,     0,     0,    52,     0,     0,     0,     0,    53,
       0,     0,    54,    55,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    56,
      57,    58,    59,    60,     0,     0,    61,     0,    62,     0,
       0,     0,     0,    63,     0,     0,     0,    64,     0,    65,
      66,     0,    67,     0,     0,     0,    68,    69,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    70,     0,
      71,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,    74,     0,    75,    76,    77,     0,
       0,    78,     0,     0,     0,    79,    80,    81,     0,    82,
       0,     0,    83,     0,     0,     0,     0,     0,     0,     0,
       0,    84,    85,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    86,    87,     0,    88,     0,
       0,    89,     0,     0,    90,     0,     0,     0,     0,     0,
      91,   372,     0,     0,     0,     0,     0,     0,    93,     0,
       0,     0,     0,     0,    94,    95,    96,     0,    97,     0,
       0,     0,    98,     0,     0,     0,     0,     0,    99,     0,
       0,     0,   100,     0,     0,     0,     0,     0,     0,   101,
       0,     0,     0,     0,   102,   103,   104,     0,     0,     0,
       0,     0,     0,     0,     0,   105,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   107,     0,     0,
       0,     0,     0,   378,   108,   109,   110,   111,   112,     0,
       7,     8,     9,    10,    11,     0,    12,    13,   113,    14,
      15,     0,     0,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,     0,    26,     0,    27,     0,    28,    29,
      30,    31,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,     0,     0,     0,    41,     0,     0,
      42,    43,    44,     0,    45,     0,     0,     0,    46,     0,
       0,     0,     0,     0,     0,     0,     0,    47,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
       0,     0,     0,     0,     0,     0,     0,     0,    51,     0,
       0,    52,     0,     0,     0,     0,    53,     0,     0,    54,
      55,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    56,    57,    58,    59,
      60,     0,     0,    61,     0,    62,     0,     0,     0,     0,
      63,     0,     0,     0,    64,     0,    65,    66,     0,    67,
       0,     0,     0,    68,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    70,     0,    71,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,    74,     0,    75,    76,    77,     0,     0,    78,     0,
       0,     0,    79,    80,    81,     0,    82,     0,     0,    83,
       0,     0,     0,     0,     0,     0,     0,     0,    84,    85,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    86,    87,     0,    88,     0,     0,    89,     0,
       0,    90,     0,     0,     0,     0,     0,    91,    92,     0,
       0,     0,     0,     0,     0,    93,     0,     0,     0,     0,
       0,    94,    95,    96,     0,    97,     0,     0,     0,    98,
       0,     0,     0,     0,     0,    99,     0,     0,     0,   100,
       0,     0,     0,     0,     0,     0,   101,     0,     0,     0,
       0,   102,   103,   104,     0,     0,     0,     0,     0,     0,
       0,     0,   105,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   107,     0,     0,     0,     0,     0,
     515,   108,   109,   110,   111,   112,     0,     7,     8,     9,
      10,    11,     0,    12,    13,   113,    14,    15,     0,     0,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
       0,    26,     0,    27,     0,    28,    29,    30,    31,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,     0,     0,     0,    41,     0,     0,    42,    43,    44,
       0,    45,     0,     0,     0,    46,     0,     0,     0,     0,
       0,     0,     0,     0,    47,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,     0,     0,     0,
       0,     0,     0,     0,     0,    51,     0,     0,    52,     0,
       0,     0,     0,    53,     0,     0,    54,    55,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    56,    57,    58,    59,    60,     0,     0,
      61,     0,    62,     0,     0,     0,     0,    63,     0,     0,
       0,    64,     0,    65,    66,     0,    67,     0,     0,     0,
      68,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    70,     0,    71,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,    74,     0,
      75,    76,    77,     0,     0,    78,     0,     0,     0,    79,
      80,    81,     0,    82,     0,     0,    83,     0,     0,     0,
       0,     0,     0,     0,     0,    84,    85,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    86,
      87,     0,    88,     0,     0,    89,     0,     0,    90,     0,
       0,     0,     0,     0,    91,    92,     0,     0,     0,     0,
       0,     0,    93,     0,     0,     0,     0,     0,    94,    95,
      96,     0,    97,     0,     0,     0,    98,     0,     0,     0,
       0,     0,    99,     0,     0,     0,   100,     0,     0,     0,
       0,     0,     0,   101,     0,     0,     0,     0,   102,   103,
     104,     0,     0,     0,     0,     0,     0,     0,     0,   105,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   107,     0,     0,     0,     0,     0,   520,   108,   109,
     110,   111,   112,     0,     7,     8,     9,    10,    11,     0,
      12,    13,   113,    14,    15,     0,     0,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,     0,    26,     0,
      27,     0,    28,    29,    30,    31,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,     0,     0,
       0,    41,     0,     0,    42,    43,    44,     0,    45,     0,
       0,     0,    46,     0,     0,     0,     0,     0,     0,     0,
       0,    47,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,     0,     0,     0,     0,     0,     0,
       0,     0,    51,     0,     0,    52,     0,     0,     0,     0,
      53,     0,     0,    54,    55,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      56,    57,    58,    59,    60,     0,     0,    61,     0,    62,
       0,     0,     0,     0,    63,     0,     0,     0,    64,     0,
      65,    66,     0,    67,     0,     0,     0,    68,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    70,
       0,    71,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,    74,     0,    75,    76,    77,
       0,     0,    78,     0,     0,     0,    79,    80,    81,     0,
      82,     0,     0,    83,     0,     0,     0,     0,     0,     0,
       0,     0,    84,    85,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    86,    87,     0,    88,
       0,     0,    89,     0,     0,    90,     0,     0,     0,     0,
       0,    91,    92,     0,     0,     0,     0,     0,     0,    93,
       0,     0,     0,     0,     0,    94,    95,    96,     0,    97,
       0,     0,     0,    98,     0,     0,     0,     0,     0,    99,
       0,     0,     0,   100,     0,     0,     0,     0,     0,     0,
     101,     0,     0,     0,     0,   102,   103,   104,     0,     0,
       0,     0,     0,     0,     0,     0,   521,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,     0,   523,   108,   109,   110,   111,   112,
       0,     7,     8,     9,    10,    11,     0,    12,    13,   113,
      14,    15,     0,     0,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,     0,    26,     0,    27,     0,    28,
      29,    30,    31,     0,     0,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,     0,     0,     0,    41,     0,
       0,    42,    43,    44,     0,    45,     0,     0,     0,    46,
       0,     0,     0,     0,     0,     0,     0,     0,    47,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      50,     0,     0,     0,     0,     0,     0,     0,     0,    51,
       0,     0,    52,     0,     0,     0,     0,    53,     0,     0,
      54,    55,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    56,    57,    58,
      59,    60,     0,     0,    61,     0,    62,     0,     0,     0,
       0,    63,     0,     0,     0,    64,     0,    65,    66,     0,
      67,     0,     0,     0,    68,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    70,     0,    71,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,    74,     0,    75,    76,    77,     0,     0,    78,
       0,     0,     0,    79,    80,    81,     0,    82,     0,     0,
      83,     0,     0,     0,     0,     0,     0,     0,     0,    84,
      85,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    86,    87,     0,    88,     0,     0,    89,
       0,     0,    90,     0,     0,     0,     0,     0,    91,    92,
       0,     0,     0,     0,     0,     0,    93,     0,     0,     0,
       0,     0,    94,    95,    96,     0,    97,     0,     0,     0,
      98,     0,     0,     0,     0,     0,    99,     0,     0,     0,
     100,     0,     0,     0,     0,     0,     0,   101,     0,     0,
       0,     0,   102,   103,   104,     0,     0,     0,     0,     0,
       0,     0,     0,   524,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   107,     0,     0,     0,     0,
       0,   528,   108,   109,   110,   111,   112,     0,     7,     8,
       9,    10,    11,     0,    12,    13,   113,    14,    15,     0,
       0,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,     0,    26,     0,    27,     0,    28,    29,    30,    31,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,     0,     0,     0,    41,     0,     0,    42,    43,
      44,     0,    45,     0,     0,     0,    46,     0,     0,     0,
       0,     0,     0,     0,     0,    47,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    50,     0,     0,
       0,     0,     0,     0,     0,     0,    51,     0,     0,    52,
       0,     0,     0,     0,    53,     0,     0,    54,    55,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    56,    57,    58,    59,    60,     0,
       0,    61,     0,    62,     0,     0,     0,     0,    63,     0,
       0,     0,    64,     0,    65,    66,     0,    67,     0,     0,
       0,    68,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    70,     0,    71,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,    74,
       0,    75,    76,    77,     0,     0,    78,     0,     0,     0,
      79,    80,    81,     0,    82,     0,     0,    83,     0,     0,
       0,     0,     0,     0,     0,     0,    84,    85,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      86,    87,     0,    88,     0,     0,    89,     0,     0,    90,
       0,     0,     0,     0,     0,    91,    92,     0,     0,     0,
       0,     0,     0,    93,     0,     0,     0,     0,     0,    94,
      95,    96,     0,    97,     0,     0,     0,    98,     0,     0,
       0,     0,     0,    99,     0,     0,     0,   100,     0,     0,
       0,     0,     0,     0,   101,     0,     0,     0,     0,   102,
     103,   104,     0,     0,     0,     0,     0,     0,     0,     0,
     105,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   107,     0,     0,     0,     0,     0,   530,   108,
     109,   110,   111,   112,     0,     7,     8,     9,    10,    11,
       0,    12,    13,   113,    14,    15,     0,     0,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,     0,    26,
       0,    27,     0,    28,    29,    30,    31,     0,     0,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,     0,
       0,     0,    41,     0,     0,    42,    43,    44,     0,    45,
       0,     0,     0,    46,     0,     0,     0,     0,     0,     0,
       0,     0,    47,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,     0,     0,     0,     0,     0,
       0,     0,     0,    51,     0,     0,    52,     0,     0,     0,
       0,    53,     0,     0,    54,    55,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    56,    57,    58,    59,    60,     0,     0,    61,     0,
      62,     0,     0,     0,     0,    63,     0,     0,     0,    64,
       0,    65,    66,     0,    67,     0,     0,     0,    68,    69,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      70,     0,    71,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,    74,     0,    75,    76,
      77,     0,     0,    78,     0,     0,     0,    79,    80,    81,
       0,    82,     0,     0,    83,     0,     0,     0,     0,     0,
       0,     0,     0,    84,    85,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    86,    87,     0,
      88,     0,     0,    89,     0,     0,    90,     0,     0,     0,
       0,     0,    91,    92,     0,     0,     0,     0,     0,     0,
      93,     0,     0,     0,     0,     0,    94,    95,    96,     0,
      97,     0,     0,     0,    98,     0,     0,     0,     0,     0,
      99,     0,     0,     0,   100,     0,     0,     0,     0,     0,
       0,   101,     0,     0,     0,     0,   102,   103,   104,     0,
       0,     0,     0,     0,     0,     0,     0,   105,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   107,
       0,     0,     0,     0,     0,   537,   108,   109,   110,   111,
     112,     0,     7,     8,     9,    10,    11,     0,    12,    13,
     113,    14,    15,     0,     0,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,     0,    26,     0,    27,     0,
      28,    29,    30,    31,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,     0,     0,     0,    41,
       0,     0,    42,    43,    44,     0,    45,     0,     0,     0,
      46,     0,     0,     0,     0,     0,     0,     0,     0,    47,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,     0,     0,     0,     0,     0,     0,     0,     0,
      51,     0,     0,    52,     0,     0,     0,     0,    53,     0,
       0,    54,    55,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    56,    57,
      58,    59,    60,     0,     0,    61,     0,    62,     0,     0,
       0,     0,    63,     0,     0,     0,    64,     0,    65,    66,
       0,    67,     0,     0,     0,    68,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    70,     0,    71,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,    74,     0,    75,    76,    77,     0,     0,
      78,     0,     0,     0,    79,    80,    81,     0,    82,     0,
       0,    83,     0,     0,     0,     0,     0,     0,     0,     0,
      84,    85,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    86,    87,     0,    88,     0,     0,
      89,     0,     0,    90,     0,     0,     0,     0,     0,    91,
      92,     0,     0,     0,     0,     0,     0,    93,     0,     0,
       0,     0,     0,    94,    95,    96,     0,    97,     0,     0,
       0,    98,     0,     0,     0,     0,     0,    99,     0,     0,
       0,   100,     0,     0,     0,     0,     0,     0,   101,     0,
       0,     0,     0,   102,   103,   104,     0,     0,     0,     0,
       0,     0,     0,     0,   105,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   107,     0,     0,     0,
       0,     0,   551,   108,   109,   110,   111,   112,     0,     7,
       8,     9,    10,    11,     0,    12,    13,   113,    14,    15,
       0,     0,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,     0,    26,     0,    27,     0,    28,    29,    30,
      31,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,     0,     0,     0,    41,     0,     0,    42,
      43,    44,     0,    45,     0,     0,     0,    46,     0,     0,
       0,     0,     0,     0,     0,     0,    47,    48,    49,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    50,     0,
       0,     0,     0,     0,     0,     0,     0,    51,     0,     0,
      52,     0,     0,     0,     0,    53,     0,     0,    54,    55,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    56,    57,    58,    59,    60,
       0,     0,    61,     0,    62,     0,     0,     0,     0,    63,
       0,     0,     0,    64,     0,    65,    66,     0,    67,     0,
       0,     0,    68,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    70,     0,    71,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
      74,     0,    75,    76,    77,     0,     0,    78,     0,     0,
       0,    79,    80,    81,     0,    82,     0,     0,    83,     0,
       0,     0,     0,     0,     0,     0,     0,    84,    85,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    86,    87,     0,    88,     0,     0,    89,     0,     0,
      90,     0,     0,     0,     0,     0,    91,    92,     0,     0,
       0,     0,     0,     0,    93,     0,     0,     0,     0,     0,
      94,    95,    96,     0,    97,     0,     0,     0,    98,     0,
       0,     0,     0,     0,    99,     0,     0,     0,   100,     0,
       0,     0,     0,     0,     0,   101,     0,     0,     0,     0,
     102,   103,   104,     0,     0,     0,     0,     0,     0,     0,
       0,   105,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   107,     0,     0,     0,     0,     0,   553,
     108,   109,   110,   111,   112,     0,     7,     8,     9,    10,
      11,     0,    12,    13,   113,    14,    15,     0,     0,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,     0,
      26,     0,    27,     0,    28,    29,    30,    31,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
       0,     0,     0,    41,     0,     0,    42,    43,    44,     0,
      45,     0,     0,     0,    46,     0,     0,     0,     0,     0,
       0,     0,     0,    47,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,     0,     0,     0,     0,
       0,     0,     0,     0,    51,     0,     0,    52,     0,     0,
       0,     0,    53,     0,     0,    54,    55,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    56,    57,    58,    59,    60,     0,     0,    61,
       0,    62,     0,     0,     0,     0,    63,     0,     0,     0,
      64,     0,    65,    66,     0,    67,     0,     0,     0,    68,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    70,     0,    71,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,    74,     0,    75,
      76,    77,     0,     0,    78,     0,     0,     0,    79,    80,
      81,     0,    82,     0,     0,    83,     0,     0,     0,     0,
       0,     0,     0,     0,    84,    85,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    86,    87,
       0,    88,     0,     0,    89,     0,     0,    90,     0,     0,
       0,     0,     0,    91,    92,     0,     0,     0,     0,     0,
       0,    93,     0,     0,     0,     0,     0,    94,    95,    96,
       0,    97,     0,     0,     0,    98,     0,     0,     0,     0,
       0,    99,     0,     0,     0,   100,     0,     0,     0,     0,
       0,     0,   101,     0,     0,     0,     0,   102,   103,   104,
       0,     0,     0,     0,     0,     0,     0,     0,   105,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,     0,   566,   108,   109,   110,
     111,   112,     0,     7,     8,     9,    10,    11,     0,    12,
      13,   113,    14,    15,     0,     0,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,     0,    26,     0,    27,
       0,    28,    29,    30,    31,     0,     0,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,     0,     0,     0,
      41,     0,     0,    42,    43,    44,     0,    45,     0,     0,
       0,    46,     0,     0,     0,     0,     0,     0,     0,     0,
      47,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    50,     0,     0,     0,     0,     0,     0,     0,
       0,    51,     0,     0,    52,     0,     0,     0,     0,    53,
       0,     0,    54,    55,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    56,
      57,    58,    59,    60,     0,     0,    61,     0,    62,     0,
       0,     0,     0,    63,     0,     0,     0,    64,     0,    65,
      66,     0,    67,     0,     0,     0,    68,    69,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    70,     0,
      71,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,    74,     0,    75,    76,    77,     0,
       0,    78,     0,     0,     0,    79,    80,    81,     0,    82,
       0,     0,    83,     0,     0,     0,     0,     0,     0,     0,
       0,    84,    85,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    86,    87,     0,    88,     0,
       0,    89,     0,     0,    90,     0,     0,     0,     0,     0,
      91,    92,     0,     0,     0,     0,     0,     0,    93,     0,
       0,     0,     0,     0,    94,    95,    96,     0,    97,     0,
       0,     0,    98,     0,     0,     0,     0,     0,    99,     0,
       0,     0,   100,     0,     0,     0,     0,     0,     0,   101,
       0,     0,     0,     0,   102,   103,   104,     0,     0,     0,
       0,     0,     0,     0,     0,   105,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   107,     0,     0,
       0,     0,     0,   571,   108,   109,   110,   111,   112,     0,
       7,     8,     9,    10,    11,     0,    12,    13,   113,    14,
      15,     0,     0,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,     0,    26,     0,    27,     0,    28,    29,
      30,    31,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,     0,     0,     0,    41,     0,     0,
      42,    43,    44,     0,    45,     0,     0,     0,    46,     0,
       0,     0,     0,     0,     0,     0,     0,    47,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
       0,     0,     0,     0,     0,     0,     0,     0,    51,     0,
       0,    52,     0,     0,     0,     0,    53,     0,     0,    54,
      55,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    56,    57,    58,    59,
      60,     0,     0,    61,     0,    62,     0,     0,     0,     0,
      63,     0,     0,     0,    64,     0,    65,    66,     0,    67,
       0,     0,     0,    68,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    70,     0,    71,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,    74,     0,    75,    76,    77,     0,     0,    78,     0,
       0,     0,    79,    80,    81,     0,    82,     0,     0,    83,
       0,     0,     0,     0,     0,     0,     0,     0,    84,    85,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    86,    87,     0,    88,     0,     0,    89,     0,
       0,    90,     0,     0,     0,     0,     0,    91,    92,     0,
       0,     0,     0,     0,     0,    93,     0,     0,     0,     0,
       0,    94,    95,    96,     0,    97,     0,     0,     0,    98,
       0,     0,     0,     0,     0,    99,     0,     0,     0,   100,
       0,     0,     0,     0,     0,     0,   101,     0,     0,     0,
       0,   102,   103,   104,     0,     0,     0,     0,     0,     0,
       0,     0,   105,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   107,     0,     0,     0,     0,     0,
     619,   108,   109,   110,   111,   112,     0,     7,     8,     9,
      10,    11,     0,    12,    13,   113,    14,    15,     0,     0,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
       0,    26,     0,    27,     0,    28,    29,    30,    31,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,     0,     0,     0,    41,     0,     0,    42,    43,    44,
       0,    45,     0,     0,     0,    46,     0,     0,     0,     0,
       0,     0,     0,     0,    47,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,     0,     0,     0,
       0,     0,     0,     0,     0,    51,     0,     0,    52,     0,
       0,     0,     0,    53,     0,     0,    54,    55,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    56,    57,    58,    59,    60,     0,     0,
      61,     0,    62,     0,     0,     0,     0,    63,     0,     0,
       0,    64,     0,    65,    66,     0,    67,     0,     0,     0,
      68,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    70,     0,    71,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,    74,     0,
      75,    76,    77,     0,     0,    78,     0,     0,     0,    79,
      80,    81,     0,    82,     0,     0,    83,     0,     0,     0,
       0,     0,     0,     0,     0,    84,    85,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    86,
      87,     0,    88,     0,     0,    89,     0,     0,    90,     0,
       0,     0,     0,     0,    91,    92,     0,     0,     0,     0,
       0,     0,    93,     0,     0,     0,     0,     0,    94,    95,
      96,     0,    97,     0,     0,     0,    98,     0,     0,     0,
       0,     0,    99,     0,     0,     0,   100,     0,     0,     0,
       0,     0,     0,   101,     0,     0,     0,     0,   102,   103,
     104,     0,     0,     0,     0,     0,     0,     0,     0,   105,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   107,     0,     0,     0,     0,     0,   621,   108,   109,
     110,   111,   112,     0,     7,     8,     9,    10,    11,     0,
      12,    13,   113,    14,    15,     0,     0,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,     0,    26,     0,
      27,     0,    28,    29,    30,    31,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,     0,     0,
       0,    41,     0,     0,    42,    43,    44,     0,    45,     0,
       0,     0,    46,     0,     0,     0,     0,     0,     0,     0,
       0,    47,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,     0,     0,     0,     0,     0,     0,
       0,     0,    51,     0,     0,    52,     0,     0,     0,     0,
      53,     0,     0,    54,    55,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      56,    57,    58,    59,    60,     0,     0,    61,     0,    62,
       0,     0,     0,     0,    63,     0,     0,     0,    64,     0,
      65,    66,     0,    67,     0,     0,     0,    68,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    70,
       0,    71,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,    74,     0,    75,    76,    77,
       0,     0,    78,     0,     0,     0,    79,    80,    81,     0,
      82,     0,     0,    83,     0,     0,     0,     0,     0,     0,
       0,     0,    84,    85,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    86,    87,     0,    88,
       0,     0,    89,     0,     0,    90,     0,     0,     0,     0,
       0,    91,    92,     0,     0,     0,     0,     0,     0,    93,
       0,     0,     0,     0,     0,    94,    95,    96,     0,    97,
       0,     0,     0,    98,     0,     0,     0,     0,     0,    99,
       0,     0,     0,   100,     0,     0,     0,     0,     0,     0,
     101,     0,     0,     0,     0,   102,   103,   104,     0,     0,
       0,     0,     0,     0,     0,     0,   105,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,     0,   634,   108,   109,   110,   111,   112,
       0,     7,     8,     9,    10,    11,     0,    12,    13,   113,
      14,    15,     0,     0,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,     0,    26,     0,    27,     0,    28,
      29,    30,    31,     0,     0,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,     0,     0,     0,    41,     0,
       0,    42,    43,    44,     0,    45,     0,     0,     0,    46,
       0,     0,     0,     0,     0,     0,     0,     0,    47,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      50,     0,     0,     0,     0,     0,     0,     0,     0,    51,
       0,     0,    52,     0,     0,     0,     0,    53,     0,     0,
      54,    55,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    56,    57,    58,
      59,    60,     0,     0,    61,     0,    62,     0,     0,     0,
       0,    63,     0,     0,     0,    64,     0,    65,    66,     0,
      67,     0,     0,     0,    68,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    70,     0,    71,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,    74,     0,    75,    76,    77,     0,     0,    78,
       0,     0,     0,    79,    80,    81,     0,    82,     0,     0,
      83,     0,     0,     0,     0,     0,     0,     0,     0,    84,
      85,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    86,    87,     0,    88,     0,     0,    89,
       0,     0,    90,     0,     0,     0,     0,     0,    91,    92,
       0,     0,     0,     0,     0,     0,    93,     0,     0,     0,
       0,     0,    94,    95,    96,     0,    97,     0,     0,     0,
      98,     0,     0,     0,     0,     0,    99,     0,     0,     0,
     100,     0,     0,     0,     0,     0,     0,   101,     0,     0,
       0,     0,   102,   103,   104,     0,     0,     0,     0,     0,
       0,     0,     0,   105,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   107,     0,     0,     0,     0,
       0,   674,   108,   109,   110,   111,   112,     0,     7,     8,
       9,    10,    11,     0,    12,    13,   113,    14,    15,     0,
       0,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,     0,    26,     0,    27,     0,    28,    29,    30,    31,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,     0,     0,     0,    41,     0,     0,    42,    43,
      44,     0,    45,     0,     0,     0,    46,     0,     0,     0,
       0,     0,     0,     0,     0,    47,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    50,     0,     0,
       0,     0,     0,     0,     0,     0,    51,     0,     0,    52,
       0,     0,     0,     0,    53,     0,     0,    54,    55,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    56,    57,    58,    59,    60,     0,
       0,    61,     0,    62,     0,     0,     0,     0,    63,     0,
       0,     0,    64,     0,    65,    66,     0,    67,     0,     0,
       0,    68,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    70,     0,    71,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,    74,
       0,    75,    76,    77,     0,     0,    78,     0,     0,     0,
      79,    80,    81,     0,    82,     0,     0,    83,     0,     0,
       0,     0,     0,     0,     0,     0,    84,    85,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      86,    87,     0,    88,     0,     0,    89,     0,     0,    90,
       0,     0,     0,     0,     0,    91,    92,     0,     0,     0,
       0,     0,     0,    93,     0,     0,     0,     0,     0,    94,
      95,    96,     0,    97,     0,     0,     0,    98,     0,     0,
       0,     0,     0,    99,     0,     0,     0,   100,     0,     0,
       0,     0,     0,     0,   101,     0,     0,     0,     0,   102,
     103,   104,     0,     0,     0,     0,     0,     0,     0,     0,
     105,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   107,     0,     0,     0,     0,     0,   681,   108,
     109,   110,   111,   112,     0,     7,     8,     9,    10,    11,
       0,    12,    13,   113,    14,    15,     0,     0,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,     0,    26,
       0,    27,     0,    28,    29,    30,    31,     0,     0,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,     0,
       0,     0,    41,     0,     0,    42,    43,    44,     0,    45,
       0,     0,     0,    46,     0,     0,     0,     0,     0,     0,
       0,     0,    47,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,     0,     0,     0,     0,     0,
       0,     0,     0,    51,     0,     0,    52,     0,     0,     0,
       0,    53,     0,     0,    54,    55,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    56,    57,    58,    59,    60,     0,     0,    61,     0,
      62,     0,     0,     0,     0,    63,     0,     0,     0,    64,
       0,    65,    66,     0,    67,     0,     0,     0,    68,    69,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      70,     0,    71,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,    74,     0,    75,    76,
      77,     0,     0,    78,     0,     0,     0,    79,    80,    81,
       0,    82,     0,     0,    83,     0,     0,     0,     0,     0,
       0,     0,     0,    84,    85,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    86,    87,     0,
      88,     0,     0,    89,     0,     0,    90,     0,     0,     0,
       0,     0,    91,    92,     0,     0,     0,     0,     0,     0,
      93,     0,     0,     0,     0,     0,    94,    95,    96,     0,
      97,     0,     0,     0,    98,     0,     0,     0,     0,     0,
      99,     0,     0,     0,   100,     0,     0,     0,     0,     0,
       0,   101,     0,     0,     0,     0,   102,   103,   104,     0,
       0,     0,     0,     0,     0,     0,     0,   105,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   107,
       0,     0,     0,     0,     0,   691,   108,   109,   110,   111,
     112,     0,     7,     8,     9,    10,    11,     0,    12,    13,
     113,    14,    15,     0,     0,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,     0,    26,     0,    27,     0,
      28,    29,    30,    31,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,     0,     0,     0,    41,
       0,     0,    42,    43,    44,     0,    45,     0,     0,     0,
      46,     0,     0,     0,     0,     0,     0,     0,     0,    47,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,     0,     0,     0,     0,     0,     0,     0,     0,
      51,     0,     0,    52,     0,     0,     0,     0,    53,     0,
       0,    54,    55,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    56,    57,
      58,    59,    60,     0,     0,    61,     0,    62,     0,     0,
       0,     0,    63,     0,     0,     0,    64,     0,    65,    66,
       0,    67,     0,     0,     0,    68,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    70,     0,    71,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,    74,     0,    75,    76,    77,     0,     0,
      78,     0,     0,     0,    79,    80,    81,     0,    82,     0,
       0,    83,     0,     0,     0,     0,     0,     0,     0,     0,
      84,    85,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    86,    87,     0,    88,     0,     0,
      89,     0,     0,    90,     0,     0,     0,     0,     0,    91,
      92,     0,     0,     0,     0,     0,     0,    93,     0,     0,
       0,     0,     0,    94,    95,    96,     0,    97,     0,     0,
       0,    98,     0,     0,     0,     0,     0,    99,     0,     0,
       0,   100,     0,     0,     0,     0,     0,     0,   101,     0,
       0,     0,     0,   102,   103,   104,     0,     0,     0,     0,
       0,     0,     0,     0,   105,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   107,     0,     0,     0,
       0,     0,   693,   108,   109,   110,   111,   112,     0,     7,
       8,     9,    10,    11,     0,    12,    13,   113,    14,    15,
       0,     0,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,     0,    26,     0,    27,     0,    28,    29,    30,
      31,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,     0,     0,     0,    41,     0,     0,    42,
      43,    44,     0,    45,     0,     0,     0,    46,     0,     0,
       0,     0,     0,     0,     0,     0,    47,    48,    49,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    50,     0,
       0,     0,     0,     0,     0,     0,     0,    51,     0,     0,
      52,     0,     0,     0,     0,    53,     0,     0,    54,    55,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    56,    57,    58,    59,    60,
       0,     0,    61,     0,    62,     0,     0,     0,     0,    63,
       0,     0,     0,    64,     0,    65,    66,     0,    67,     0,
       0,     0,    68,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    70,     0,    71,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
      74,     0,    75,    76,    77,     0,     0,    78,     0,     0,
       0,    79,    80,    81,     0,    82,     0,     0,    83,     0,
       0,     0,     0,     0,     0,     0,     0,    84,    85,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    86,    87,     0,    88,     0,     0,    89,     0,     0,
      90,     0,     0,     0,     0,     0,    91,    92,     0,     0,
       0,     0,     0,     0,    93,     0,     0,     0,     0,     0,
      94,    95,    96,     0,    97,     0,     0,     0,    98,     0,
       0,     0,     0,     0,    99,     0,     0,     0,   100,     0,
       0,     0,     0,     0,     0,   101,     0,     0,     0,     0,
     102,   103,   104,     0,     0,     0,     0,     0,     0,     0,
       0,   105,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   107,     0,     0,     0,     0,     0,   809,
     108,   109,   110,   111,   112,     0,     7,     8,     9,    10,
      11,     0,    12,    13,   113,    14,    15,     0,     0,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,     0,
      26,     0,    27,     0,    28,    29,    30,    31,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
       0,     0,     0,    41,     0,     0,    42,    43,    44,     0,
      45,     0,     0,     0,    46,     0,     0,     0,     0,     0,
       0,     0,     0,    47,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,     0,     0,     0,     0,
       0,     0,     0,     0,    51,     0,     0,    52,     0,     0,
       0,     0,    53,     0,     0,    54,    55,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    56,    57,    58,    59,    60,     0,     0,    61,
       0,    62,     0,     0,     0,     0,    63,     0,     0,     0,
      64,     0,    65,    66,     0,    67,     0,     0,     0,    68,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    70,     0,    71,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,    74,     0,    75,
      76,    77,     0,     0,    78,     0,     0,     0,    79,    80,
      81,     0,    82,     0,     0,    83,     0,     0,     0,     0,
       0,     0,     0,     0,    84,    85,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    86,    87,
       0,    88,     0,     0,    89,     0,     0,    90,     0,     0,
       0,     0,     0,    91,    92,     0,     0,     0,     0,     0,
       0,    93,     0,     0,     0,     0,     0,    94,    95,    96,
       0,    97,     0,     0,     0,    98,     0,     0,     0,     0,
       0,    99,     0,     0,     0,   100,     0,     0,     0,     0,
       0,     0,   101,     0,     0,     0,     0,   102,   103,   104,
       0,     0,     0,     0,     0,     0,     0,     0,   105,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,     0,   811,   108,   109,   110,
     111,   112,     0,  -785,  -785,  -785,  -785,  -785,     0,  -785,
    -785,   113,  -785,  -785,     0,     0,  -785,  -785,  -785,  -785,
    -785,  -785,  -785,  -785,  -785,  -785,     0,  -785,     0,  -785,
       0,  -785,  -785,  -785,  -785,     0,     0,  -785,  -785,  -785,
    -785,  -785,  -785,  -785,  -785,  -785,     0,     0,     0,     0,
    -785,     0,     0,  -785,  -785,  -785,     0,  -785,     0,     0,
       0,  -785,     0,     0,     0,     0,     0,     0,     0,     0,
    -785,  -785,  -785,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -785,     0,     0,     0,     0,     0,     0,     0,
       0,  -785,     0,     0,  -785,     0,     0,     0,     0,  -785,
       0,     0,  -785,  -785,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -785,
    -785,  -785,  -785,  -785,     0,     0,  -785,     0,  -785,     0,
       0,     0,     0,  -785,     0,     0,     0,  -785,     0,  -785,
    -785,     0,  -785,     0,     0,     0,  -785,  -785,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -785,     0,
    -785,     0,     0,     0,     0,  -785,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -785,     0,  -785,     0,  -785,  -785,  -785,     0,
       0,  -785,     0,     0,     0,  -785,  -785,  -785,     0,  -785,
       0,     0,  -785,     0,     0,     0,     0,     0,     0,     0,
       0,  -785,  -785,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -785,  -785,     0,  -785,     0,
       0,  -785,     0,     0,  -785,     0,     0,     0,     0,     0,
    -785,  -785,     0,     0,     0,     0,     0,     0,  -785,     0,
       0,     0,     0,     0,  -785,  -785,  -785,     0,  -785,     0,
       0,     0,  -785,     0,     0,     0,     0,     0,  -785,     0,
       0,     0,  -785,     0,     0,     0,     0,     0,     0,  -785,
       0,     0,     0,     0,  -785,  -785,  -785,     0,     0,     0,
       0,     0,     0,     0,     0,  -785,     0,  -785,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -785,     0,     0,
       0,     0,     0,   813,  -785,  -785,  -785,  -785,  -785,     0,
       7,     8,     9,    10,    11,     0,    12,    13,  -785,    14,
      15,     0,     0,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,     0,    26,     0,    27,     0,    28,    29,
      30,    31,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,     0,     0,     0,    41,     0,     0,
      42,    43,    44,     0,    45,     0,     0,     0,    46,     0,
       0,     0,     0,     0,     0,     0,     0,    47,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
       0,     0,     0,     0,     0,     0,     0,     0,    51,     0,
       0,    52,     0,     0,     0,     0,    53,     0,     0,    54,
      55,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    56,    57,    58,    59,
      60,     0,     0,    61,     0,    62,     0,     0,     0,     0,
      63,     0,     0,     0,    64,     0,    65,    66,     0,    67,
       0,     0,     0,    68,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    70,     0,    71,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,    74,     0,    75,    76,    77,     0,     0,    78,     0,
       0,     0,    79,    80,    81,     0,    82,     0,     0,    83,
       0,     0,     0,     0,     0,     0,     0,     0,    84,    85,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    86,    87,     0,    88,     0,     0,    89,     0,
       0,    90,     0,     0,     0,     0,     0,    91,    92,     0,
       0,     0,     0,     0,     0,    93,     0,     0,     0,     0,
       0,    94,    95,    96,     0,    97,     0,     0,     0,    98,
       0,     0,     0,     0,     0,    99,     0,     0,     0,   100,
       0,     0,     0,     0,     0,     0,   101,     0,     0,     0,
       0,   102,   103,   104,     0,     0,     0,     0,     0,     0,
       0,     0,   105,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   107,     0,     0,     0,     0,     0,
     815,   108,   109,   110,   111,   112,     0,     7,     8,     9,
      10,    11,     0,    12,    13,   113,    14,    15,     0,     0,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
       0,    26,     0,    27,     0,    28,    29,    30,    31,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,     0,     0,     0,    41,     0,     0,    42,    43,    44,
       0,    45,     0,     0,     0,    46,     0,     0,     0,     0,
       0,     0,     0,     0,    47,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,     0,     0,     0,
       0,     0,     0,     0,     0,    51,     0,     0,    52,     0,
       0,     0,     0,    53,     0,     0,    54,    55,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    56,    57,    58,    59,    60,     0,     0,
      61,     0,    62,     0,     0,     0,     0,    63,     0,     0,
       0,    64,     0,    65,    66,     0,    67,     0,     0,     0,
      68,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    70,     0,    71,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,    74,     0,
      75,    76,    77,     0,     0,    78,     0,     0,     0,    79,
      80,    81,     0,    82,     0,     0,    83,     0,     0,     0,
       0,     0,     0,     0,     0,    84,    85,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    86,
      87,     0,    88,     0,     0,    89,     0,     0,    90,     0,
       0,     0,     0,     0,    91,    92,     0,     0,     0,     0,
       0,     0,    93,     0,     0,     0,     0,     0,    94,    95,
      96,     0,    97,     0,     0,     0,    98,     0,     0,     0,
       0,     0,    99,     0,     0,     0,   100,     0,     0,     0,
       0,     0,     0,   101,     0,     0,     0,     0,   102,   103,
     104,     0,     0,     0,     0,     0,     0,     0,     0,   105,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   107,     0,     0,     0,     0,     0,   817,   108,   109,
     110,   111,   112,     0,     7,     8,     9,    10,    11,     0,
      12,    13,   113,    14,    15,     0,     0,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,     0,    26,     0,
      27,     0,    28,    29,    30,    31,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,     0,     0,
       0,    41,     0,     0,    42,    43,    44,     0,    45,     0,
       0,     0,    46,     0,     0,     0,     0,     0,     0,     0,
       0,    47,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,     0,     0,     0,     0,     0,     0,
       0,     0,    51,     0,     0,    52,     0,     0,     0,     0,
      53,     0,     0,    54,    55,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      56,    57,    58,    59,    60,     0,     0,    61,     0,    62,
       0,     0,     0,     0,    63,     0,     0,     0,    64,     0,
      65,    66,     0,    67,     0,     0,     0,    68,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    70,
       0,    71,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,    74,     0,    75,    76,    77,
       0,     0,    78,     0,     0,     0,    79,    80,    81,     0,
      82,     0,     0,    83,     0,     0,     0,     0,     0,     0,
       0,     0,    84,    85,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    86,    87,     0,    88,
       0,     0,    89,     0,     0,    90,     0,     0,     0,     0,
       0,    91,    92,     0,     0,     0,     0,     0,     0,    93,
       0,     0,     0,     0,     0,    94,    95,    96,     0,    97,
       0,     0,     0,    98,     0,     0,     0,     0,     0,    99,
       0,     0,     0,   100,     0,     0,     0,     0,     0,     0,
     101,     0,     0,     0,     0,   102,   103,   104,     0,     0,
       0,     0,     0,     0,     0,     0,   105,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,     0,   819,   108,   109,   110,   111,   112,
       0,     7,     8,     9,    10,    11,     0,    12,    13,   113,
      14,    15,     0,     0,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,     0,    26,     0,    27,     0,    28,
      29,    30,    31,     0,     0,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,     0,     0,     0,    41,     0,
       0,    42,    43,    44,     0,    45,     0,     0,     0,    46,
       0,     0,     0,     0,     0,     0,     0,     0,    47,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      50,     0,     0,     0,     0,     0,     0,     0,     0,    51,
       0,     0,    52,     0,     0,     0,     0,    53,     0,     0,
      54,    55,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    56,    57,    58,
      59,    60,     0,     0,    61,     0,    62,     0,     0,     0,
       0,    63,     0,     0,     0,    64,     0,    65,    66,     0,
      67,     0,     0,     0,    68,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    70,     0,    71,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,    74,     0,    75,    76,    77,     0,     0,    78,
       0,     0,     0,    79,    80,    81,     0,    82,     0,     0,
      83,     0,     0,     0,     0,     0,     0,     0,     0,    84,
      85,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    86,    87,     0,    88,     0,     0,    89,
       0,     0,    90,     0,     0,     0,     0,     0,    91,    92,
       0,     0,     0,     0,     0,     0,    93,     0,     0,     0,
       0,     0,    94,    95,    96,     0,    97,     0,     0,     0,
      98,     0,     0,     0,     0,     0,    99,     0,     0,     0,
     100,     0,     0,     0,     0,     0,     0,   101,     0,     0,
       0,     0,   102,   103,   104,     0,     0,     0,     0,     0,
       0,     0,     0,   105,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   107,     0,     0,     0,     0,
       0,   821,   108,   109,   110,   111,   112,     0,     7,     8,
       9,    10,    11,     0,    12,    13,   113,    14,    15,     0,
       0,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,     0,    26,     0,    27,     0,    28,    29,    30,    31,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,     0,     0,     0,    41,     0,     0,    42,    43,
      44,     0,    45,     0,     0,     0,    46,     0,     0,     0,
       0,     0,     0,     0,     0,    47,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    50,     0,     0,
       0,     0,     0,     0,     0,     0,    51,     0,     0,    52,
       0,     0,     0,     0,    53,     0,     0,    54,    55,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    56,    57,    58,    59,    60,     0,
       0,    61,     0,    62,     0,     0,     0,     0,    63,     0,
       0,     0,    64,     0,    65,    66,     0,    67,     0,     0,
       0,    68,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    70,     0,    71,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,    74,
       0,    75,    76,    77,     0,     0,    78,     0,     0,     0,
      79,    80,    81,     0,    82,     0,     0,    83,     0,     0,
       0,     0,     0,     0,     0,     0,    84,    85,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      86,    87,     0,    88,     0,     0,    89,     0,     0,    90,
       0,     0,     0,     0,     0,    91,    92,     0,     0,     0,
       0,     0,     0,    93,     0,     0,     0,     0,     0,    94,
      95,    96,     0,    97,     0,     0,     0,    98,     0,     0,
       0,     0,     0,    99,     0,     0,     0,   100,     0,     0,
       0,     0,     0,     0,   101,     0,     0,     0,     0,   102,
     103,   104,     0,     0,     0,     0,     0,     0,     0,     0,
     105,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   107,     0,     0,     0,     0,     0,   823,   108,
     109,   110,   111,   112,     0,     7,     8,     9,    10,    11,
       0,    12,    13,   113,    14,    15,     0,     0,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,     0,    26,
       0,    27,     0,    28,    29,    30,    31,     0,     0,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,     0,
       0,     0,    41,     0,     0,    42,    43,    44,     0,    45,
       0,     0,     0,    46,     0,     0,     0,     0,     0,     0,
       0,     0,    47,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,     0,     0,     0,     0,     0,
       0,     0,     0,    51,     0,     0,    52,     0,     0,     0,
       0,    53,     0,     0,    54,    55,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    56,    57,    58,    59,    60,     0,     0,    61,     0,
      62,     0,     0,     0,     0,    63,     0,     0,     0,    64,
       0,    65,    66,     0,    67,     0,     0,     0,    68,    69,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      70,     0,    71,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,    74,     0,    75,    76,
      77,     0,     0,    78,     0,     0,     0,    79,    80,    81,
       0,    82,     0,     0,    83,     0,     0,     0,     0,     0,
       0,     0,     0,    84,    85,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    86,    87,     0,
      88,     0,     0,    89,     0,     0,    90,     0,     0,     0,
       0,     0,    91,    92,     0,     0,     0,     0,     0,     0,
      93,     0,     0,     0,     0,     0,    94,    95,    96,     0,
      97,     0,     0,     0,    98,     0,     0,     0,     0,     0,
      99,     0,     0,     0,   100,     0,     0,     0,     0,     0,
       0,   101,     0,     0,     0,     0,   102,   103,   104,     0,
       0,     0,     0,     0,     0,     0,     0,   105,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   107,
       0,     0,     0,     0,     0,   825,   108,   109,   110,   111,
     112,     0,     7,     8,     9,    10,    11,     0,    12,    13,
     113,    14,    15,     0,     0,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,     0,    26,     0,    27,     0,
      28,    29,    30,    31,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,     0,     0,     0,    41,
       0,     0,    42,    43,    44,     0,    45,     0,     0,     0,
      46,     0,     0,     0,     0,     0,     0,     0,     0,    47,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,     0,     0,     0,     0,     0,     0,     0,     0,
      51,     0,     0,    52,     0,     0,     0,     0,    53,     0,
       0,    54,    55,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    56,    57,
      58,    59,    60,     0,     0,    61,     0,    62,     0,     0,
       0,     0,    63,     0,     0,     0,    64,     0,    65,    66,
       0,    67,     0,     0,     0,    68,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    70,     0,    71,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,    74,     0,    75,    76,    77,     0,     0,
      78,     0,     0,     0,    79,    80,    81,     0,    82,     0,
       0,    83,     0,     0,     0,     0,     0,     0,     0,     0,
      84,    85,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    86,    87,     0,    88,     0,     0,
      89,     0,     0,    90,     0,     0,     0,     0,     0,    91,
      92,     0,     0,     0,     0,     0,     0,    93,     0,     0,
       0,     0,     0,    94,    95,    96,     0,    97,     0,     0,
       0,    98,     0,     0,     0,     0,     0,    99,     0,     0,
       0,   100,     0,     0,     0,     0,     0,     0,   101,     0,
       0,     0,     0,   102,   103,   104,     0,     0,     0,     0,
       0,     0,     0,     0,   105,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   107,     0,     0,     0,
       0,     0,   827,   108,   109,   110,   111,   112,     0,     7,
       8,     9,    10,    11,     0,    12,    13,   113,    14,    15,
       0,     0,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,     0,    26,     0,    27,     0,    28,    29,    30,
      31,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,     0,     0,     0,    41,     0,     0,    42,
      43,    44,     0,    45,     0,     0,     0,    46,     0,     0,
       0,     0,     0,     0,     0,     0,    47,    48,    49,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    50,     0,
       0,     0,     0,     0,     0,     0,     0,    51,     0,     0,
      52,     0,     0,     0,     0,    53,     0,     0,    54,    55,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    56,    57,    58,    59,    60,
       0,     0,    61,     0,    62,     0,     0,     0,     0,    63,
       0,     0,     0,    64,     0,    65,    66,     0,    67,     0,
       0,     0,    68,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    70,     0,    71,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
      74,     0,    75,    76,    77,     0,     0,    78,     0,     0,
       0,    79,    80,    81,     0,    82,     0,     0,    83,     0,
       0,     0,     0,     0,     0,     0,     0,    84,    85,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    86,    87,     0,    88,     0,     0,    89,     0,     0,
      90,     0,     0,     0,     0,     0,    91,    92,     0,     0,
       0,     0,     0,     0,    93,     0,     0,     0,     0,     0,
      94,    95,    96,     0,    97,     0,     0,     0,    98,     0,
       0,     0,     0,     0,    99,     0,     0,     0,   100,     0,
       0,     0,     0,     0,     0,   101,     0,     0,     0,     0,
     102,   103,   104,     0,     0,     0,     0,     0,     0,     0,
       0,   105,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   107,     0,     0,     0,     0,     0,   829,
     108,   109,   110,   111,   112,     0,     7,     8,     9,    10,
      11,     0,    12,    13,   113,    14,    15,     0,     0,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,     0,
      26,     0,    27,     0,    28,    29,    30,    31,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
       0,     0,     0,    41,     0,     0,    42,    43,    44,     0,
      45,     0,     0,     0,    46,     0,     0,     0,     0,     0,
       0,     0,     0,    47,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,     0,     0,     0,     0,
       0,     0,     0,     0,    51,     0,     0,    52,     0,     0,
       0,     0,    53,     0,     0,    54,    55,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    56,    57,    58,    59,    60,     0,     0,    61,
       0,    62,     0,     0,     0,     0,    63,     0,     0,     0,
      64,     0,    65,    66,     0,    67,     0,     0,     0,    68,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    70,     0,    71,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,    74,     0,    75,
      76,    77,     0,     0,    78,     0,     0,     0,    79,    80,
      81,     0,    82,     0,     0,    83,     0,     0,     0,     0,
       0,     0,     0,     0,    84,    85,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    86,    87,
       0,    88,     0,     0,    89,     0,     0,    90,     0,     0,
       0,     0,     0,    91,    92,     0,     0,     0,     0,     0,
       0,    93,     0,     0,     0,     0,     0,    94,    95,    96,
       0,    97,     0,     0,     0,    98,     0,     0,     0,     0,
       0,    99,     0,     0,     0,   100,     0,     0,     0,     0,
       0,     0,   101,     0,     0,     0,     0,   102,   103,   104,
       0,     0,     0,     0,     0,     0,     0,     0,   105,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,     0,   832,   108,   109,   110,
     111,   112,     0,     7,     8,     9,    10,    11,     0,    12,
      13,   113,    14,    15,     0,     0,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,     0,    26,     0,    27,
       0,    28,    29,    30,    31,     0,     0,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,     0,     0,     0,
      41,     0,     0,    42,    43,    44,     0,    45,     0,     0,
       0,    46,     0,     0,     0,     0,     0,     0,     0,     0,
      47,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    50,     0,     0,     0,     0,     0,     0,     0,
       0,    51,     0,     0,    52,     0,     0,     0,     0,    53,
       0,     0,    54,    55,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    56,
      57,    58,    59,    60,     0,     0,    61,     0,    62,     0,
       0,     0,     0,    63,     0,     0,     0,    64,     0,    65,
      66,     0,    67,     0,     0,     0,    68,    69,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    70,     0,
      71,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,    74,     0,    75,    76,    77,     0,
       0,    78,     0,     0,     0,    79,    80,    81,     0,    82,
       0,     0,    83,     0,     0,     0,     0,     0,     0,     0,
       0,    84,    85,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    86,    87,     0,    88,     0,
       0,    89,     0,     0,    90,     0,     0,     0,     0,     0,
      91,    92,     0,     0,     0,     0,     0,     0,    93,     0,
       0,     0,     0,     0,    94,    95,    96,     0,    97,     0,
       0,     0,    98,     0,     0,     0,     0,     0,    99,     0,
       0,     0,   100,     0,     0,     0,     0,     0,     0,   101,
       0,     0,     0,     0,   102,   103,   104,     0,     0,     0,
       0,     0,     0,     0,     0,   105,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   107,     0,     0,
       0,     0,     0,   835,   108,   109,   110,   111,   112,     0,
       7,     8,     9,    10,    11,     0,    12,    13,   113,    14,
      15,     0,     0,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,     0,    26,     0,    27,     0,    28,    29,
      30,    31,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,     0,     0,     0,    41,     0,     0,
      42,    43,    44,     0,    45,     0,     0,     0,    46,     0,
       0,     0,     0,     0,     0,     0,     0,    47,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
       0,     0,     0,     0,     0,     0,     0,     0,    51,     0,
       0,    52,     0,     0,     0,     0,    53,     0,     0,    54,
      55,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    56,    57,    58,    59,
      60,     0,     0,    61,     0,    62,     0,     0,     0,     0,
      63,     0,     0,     0,    64,     0,    65,    66,     0,    67,
       0,     0,     0,    68,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    70,     0,    71,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,    74,     0,    75,    76,    77,     0,     0,    78,     0,
       0,     0,    79,    80,    81,     0,    82,     0,     0,    83,
       0,     0,     0,     0,     0,     0,     0,     0,    84,    85,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    86,    87,     0,    88,     0,     0,    89,     0,
       0,    90,     0,     0,     0,     0,     0,    91,    92,     0,
       0,     0,     0,     0,     0,    93,     0,     0,     0,     0,
       0,    94,    95,    96,     0,    97,     0,     0,     0,    98,
       0,     0,     0,     0,     0,    99,     0,     0,     0,   100,
       0,     0,     0,     0,     0,     0,   101,     0,     0,     0,
       0,   102,   103,   104,     0,     0,     0,     0,     0,     0,
       0,     0,   105,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   107,     0,     0,     0,     0,     0,
     837,   108,   109,   110,   111,   112,     0,     7,     8,     9,
      10,    11,     0,    12,    13,   113,    14,    15,     0,     0,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
       0,    26,     0,    27,     0,    28,    29,    30,    31,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,     0,     0,     0,    41,     0,     0,    42,    43,    44,
       0,    45,     0,     0,     0,    46,     0,     0,     0,     0,
       0,     0,     0,     0,    47,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,     0,     0,     0,
       0,     0,     0,     0,     0,    51,     0,     0,    52,     0,
       0,     0,     0,    53,     0,     0,    54,    55,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    56,    57,    58,    59,    60,     0,     0,
      61,     0,    62,     0,     0,     0,     0,    63,     0,     0,
       0,    64,     0,    65,    66,     0,    67,     0,     0,     0,
      68,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    70,     0,    71,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,    74,     0,
      75,    76,    77,     0,     0,    78,     0,     0,     0,    79,
      80,    81,     0,    82,     0,     0,    83,     0,     0,     0,
       0,     0,     0,     0,     0,    84,    85,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    86,
      87,     0,    88,     0,     0,    89,     0,     0,    90,     0,
       0,     0,     0,     0,    91,    92,     0,     0,     0,     0,
       0,     0,    93,     0,     0,     0,     0,     0,    94,    95,
      96,     0,    97,     0,     0,     0,    98,     0,     0,     0,
       0,     0,    99,     0,     0,     0,   100,     0,     0,     0,
       0,     0,     0,   101,     0,     0,     0,     0,   102,   103,
     104,     0,     0,     0,     0,     0,     0,     0,     0,   105,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   107,     0,     0,     0,     0,     0,   839,   108,   109,
     110,   111,   112,     0,     7,     8,     9,    10,    11,     0,
      12,    13,   113,    14,    15,     0,     0,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,     0,    26,     0,
      27,     0,    28,    29,    30,    31,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,     0,     0,
       0,    41,     0,     0,    42,    43,    44,     0,    45,     0,
       0,     0,    46,     0,     0,     0,     0,     0,     0,     0,
       0,    47,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,     0,     0,     0,     0,     0,     0,
       0,     0,    51,     0,     0,    52,     0,     0,     0,     0,
      53,     0,     0,    54,    55,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      56,    57,    58,    59,    60,     0,     0,    61,     0,    62,
       0,     0,     0,     0,    63,     0,     0,     0,    64,     0,
      65,    66,     0,    67,     0,     0,     0,    68,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    70,
       0,    71,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,    74,     0,    75,    76,    77,
       0,     0,    78,     0,     0,     0,    79,    80,    81,     0,
      82,     0,     0,    83,     0,     0,     0,     0,     0,     0,
       0,     0,    84,    85,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    86,    87,     0,    88,
       0,     0,    89,     0,     0,    90,     0,     0,     0,     0,
       0,    91,    92,     0,     0,     0,     0,     0,     0,    93,
       0,     0,     0,     0,     0,    94,    95,    96,     0,    97,
       0,     0,     0,    98,     0,     0,     0,     0,     0,    99,
       0,     0,     0,   100,     0,     0,     0,     0,     0,     0,
     101,     0,     0,     0,     0,   102,   103,   104,     0,     0,
       0,     0,     0,     0,     0,     0,   105,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,     0,   841,   108,   109,   110,   111,   112,
       0,     7,     8,     9,    10,    11,     0,    12,    13,   113,
      14,    15,     0,     0,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,     0,    26,     0,    27,     0,    28,
      29,    30,    31,     0,     0,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,     0,     0,     0,    41,     0,
       0,    42,    43,    44,     0,    45,     0,     0,     0,    46,
       0,     0,     0,     0,     0,     0,     0,     0,    47,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      50,     0,     0,     0,     0,     0,     0,     0,     0,    51,
       0,     0,    52,     0,     0,     0,     0,    53,     0,     0,
      54,    55,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    56,    57,    58,
      59,    60,     0,     0,    61,     0,    62,     0,     0,     0,
       0,    63,     0,     0,     0,    64,     0,    65,    66,     0,
      67,     0,     0,     0,    68,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    70,     0,    71,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,    74,     0,    75,    76,    77,     0,     0,    78,
       0,     0,     0,    79,    80,    81,     0,    82,     0,     0,
      83,     0,     0,     0,     0,     0,     0,     0,     0,    84,
      85,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    86,    87,     0,    88,     0,     0,    89,
       0,     0,    90,     0,     0,     0,     0,     0,    91,    92,
       0,     0,     0,     0,     0,     0,    93,     0,     0,     0,
       0,     0,    94,    95,    96,     0,    97,     0,     0,     0,
      98,     0,     0,     0,     0,     0,    99,     0,     0,     0,
     100,     0,     0,     0,     0,     0,     0,   101,     0,     0,
       0,     0,   102,   103,   104,     0,     0,     0,     0,     0,
       0,     0,     0,   105,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   107,     0,     0,     0,     0,
       0,   843,   108,   109,   110,   111,   112,     0,     7,     8,
       9,    10,    11,     0,    12,    13,   113,    14,    15,     0,
       0,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,     0,    26,     0,    27,     0,    28,    29,    30,    31,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,     0,     0,     0,    41,     0,     0,    42,    43,
      44,     0,    45,     0,     0,     0,    46,     0,     0,     0,
       0,     0,     0,     0,     0,    47,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    50,     0,     0,
       0,     0,     0,     0,     0,     0,    51,     0,     0,    52,
       0,     0,     0,     0,    53,     0,     0,    54,    55,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    56,    57,    58,    59,    60,     0,
       0,    61,     0,    62,     0,     0,     0,     0,    63,     0,
       0,     0,    64,     0,    65,    66,     0,    67,     0,     0,
       0,    68,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    70,     0,    71,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,    74,
       0,    75,    76,    77,     0,     0,    78,     0,     0,     0,
      79,    80,    81,     0,    82,     0,     0,    83,     0,     0,
       0,     0,     0,     0,     0,     0,    84,    85,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      86,    87,     0,    88,     0,     0,    89,     0,     0,    90,
       0,     0,     0,     0,     0,    91,    92,     0,     0,     0,
       0,     0,     0,    93,     0,     0,     0,     0,     0,    94,
      95,    96,     0,    97,     0,     0,     0,    98,     0,     0,
       0,     0,     0,    99,     0,     0,     0,   100,     0,     0,
       0,     0,     0,     0,   101,     0,     0,     0,     0,   102,
     103,   104,     0,     0,     0,     0,     0,     0,     0,     0,
     105,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   107,     0,     0,     0,     0,     0,   845,   108,
     109,   110,   111,   112,     0,     7,     8,     9,    10,    11,
       0,    12,    13,   113,    14,    15,     0,     0,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,     0,    26,
       0,    27,     0,    28,    29,    30,    31,     0,     0,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,     0,
       0,     0,    41,     0,     0,    42,    43,    44,     0,    45,
       0,     0,     0,    46,     0,     0,     0,     0,     0,     0,
       0,     0,    47,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,     0,     0,     0,     0,     0,
       0,     0,     0,    51,     0,     0,    52,     0,     0,     0,
       0,    53,     0,     0,    54,    55,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    56,    57,    58,    59,    60,     0,     0,    61,     0,
      62,     0,     0,     0,     0,    63,     0,     0,     0,    64,
       0,    65,    66,     0,    67,     0,     0,     0,    68,    69,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      70,     0,    71,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,    74,     0,    75,    76,
      77,     0,     0,    78,     0,     0,     0,    79,    80,    81,
       0,    82,     0,     0,    83,     0,     0,     0,     0,     0,
       0,     0,     0,    84,    85,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    86,    87,     0,
      88,     0,     0,    89,     0,     0,    90,     0,     0,     0,
       0,     0,    91,    92,     0,     0,     0,     0,     0,     0,
      93,     0,     0,     0,     0,     0,    94,    95,    96,     0,
      97,     0,     0,     0,    98,     0,     0,     0,     0,     0,
      99,     0,     0,     0,   100,     0,     0,     0,     0,     0,
       0,   101,     0,     0,     0,     0,   102,   103,   104,     0,
       0,     0,     0,     0,     0,     0,     0,   105,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   107,
       0,     0,     0,     0,     0,   882,   108,   109,   110,   111,
     112,     0,     7,     8,     9,    10,    11,     0,    12,    13,
     113,    14,    15,     0,     0,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,     0,    26,     0,    27,     0,
      28,    29,    30,    31,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,     0,     0,     0,     0,    41,
       0,     0,    42,    43,    44,     0,    45,     0,     0,     0,
      46,     0,     0,     0,     0,     0,     0,     0,     0,    47,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,     0,     0,     0,     0,     0,     0,     0,     0,
      51,     0,     0,    52,     0,     0,     0,     0,    53,     0,
       0,    54,    55,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    56,    57,
      58,    59,    60,     0,     0,    61,     0,    62,     0,     0,
       0,     0,    63,     0,     0,     0,    64,     0,    65,    66,
       0,    67,     0,     0,     0,    68,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    70,     0,    71,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,    74,     0,    75,    76,    77,     0,     0,
      78,     0,     0,     0,    79,    80,    81,     0,    82,     0,
       0,    83,     0,     0,     0,     0,     0,     0,     0,     0,
      84,    85,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    86,    87,     0,    88,     0,     0,
      89,     0,     0,    90,     0,     0,     0,     0,     0,    91,
      92,     0,     0,     0,     0,     0,     0,    93,     0,     0,
       0,     0,     0,    94,    95,    96,     0,    97,     0,     0,
       0,    98,     0,     0,     0,     0,     0,    99,     0,     0,
       0,   100,     0,     0,     0,     0,     0,     0,   101,     0,
       0,     0,     0,   102,   103,   104,     0,     0,     0,     0,
       0,     0,     0,     0,   105,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   107,     0,     0,     0,
       0,     0,   884,   108,   109,   110,   111,   112,     0,     7,
       8,     9,    10,    11,     0,    12,    13,   113,    14,    15,
       0,     0,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,     0,    26,     0,    27,     0,    28,    29,    30,
      31,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,     0,     0,     0,    41,     0,     0,    42,
      43,    44,     0,    45,     0,     0,     0,    46,     0,     0,
       0,     0,     0,     0,     0,     0,    47,    48,    49,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    50,     0,
       0,     0,     0,     0,     0,     0,     0,    51,     0,     0,
      52,     0,     0,     0,     0,    53,     0,     0,    54,    55,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    56,    57,    58,    59,    60,
       0,     0,    61,     0,    62,     0,     0,     0,     0,    63,
       0,     0,     0,    64,     0,    65,    66,     0,    67,     0,
       0,     0,    68,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    70,     0,    71,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
      74,     0,    75,    76,    77,     0,     0,    78,     0,     0,
       0,    79,    80,    81,     0,    82,     0,     0,    83,     0,
       0,     0,     0,     0,     0,     0,     0,    84,    85,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    86,    87,     0,    88,     0,     0,    89,     0,     0,
      90,     0,     0,     0,     0,     0,    91,    92,     0,     0,
       0,     0,     0,     0,    93,     0,     0,     0,     0,     0,
      94,    95,    96,     0,    97,     0,     0,     0,    98,     0,
       0,     0,     0,     0,    99,     0,     0,     0,   100,     0,
       0,     0,     0,     0,     0,   101,     0,     0,     0,     0,
     102,   103,   104,     0,     0,     0,     0,     0,     0,     0,
       0,   105,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   107,     0,     0,     0,     0,     0,   886,
     108,   109,   110,   111,   112,     0,     7,     8,     9,    10,
      11,     0,    12,    13,   113,    14,    15,     0,     0,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,     0,
      26,     0,    27,     0,    28,    29,    30,    31,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
       0,     0,     0,    41,     0,     0,    42,    43,    44,     0,
      45,     0,     0,     0,    46,     0,     0,     0,     0,     0,
       0,     0,     0,    47,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,     0,     0,     0,     0,
       0,     0,     0,     0,    51,     0,     0,    52,     0,     0,
       0,     0,    53,     0,     0,    54,    55,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    56,    57,    58,    59,    60,     0,     0,    61,
       0,    62,     0,     0,     0,     0,    63,     0,     0,     0,
      64,     0,    65,    66,     0,    67,     0,     0,     0,    68,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    70,     0,    71,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,    74,     0,    75,
      76,    77,     0,     0,    78,     0,     0,     0,    79,    80,
      81,     0,    82,     0,     0,    83,     0,     0,     0,     0,
       0,     0,     0,     0,    84,    85,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    86,    87,
       0,    88,     0,     0,    89,     0,     0,    90,     0,     0,
       0,     0,     0,    91,    92,     0,     0,     0,     0,     0,
       0,    93,     0,     0,     0,     0,     0,    94,    95,    96,
       0,    97,     0,     0,     0,    98,     0,     0,     0,     0,
       0,    99,     0,     0,     0,   100,     0,     0,     0,     0,
       0,     0,   101,     0,     0,     0,     0,   102,   103,   104,
       0,     0,     0,     0,     0,     0,     0,     0,   105,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,     0,   889,   108,   109,   110,
     111,   112,     0,  -241,  -241,  -241,  -241,  -241,     0,  -241,
    -241,   113,  -241,  -241,     0,     0,  -241,  -241,  -241,  -241,
    -241,  -241,  -241,  -241,  -241,  -241,     0,  -241,     0,  -241,
       0,  -241,  -241,  -241,  -241,     0,     0,  -241,  -241,  -241,
    -241,  -241,  -241,  -241,  -241,  -241,     0,     0,     0,     0,
    -241,     0,     0,  -241,  -241,  -241,     0,  -241,     0,     0,
       0,  -241,     0,     0,     0,     0,     0,     0,     0,     0,
    -241,  -241,  -241,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -241,     0,     0,     0,     0,     0,     0,     0,
       0,  -241,     0,     0,  -241,     0,     0,     0,     0,  -241,
       0,     0,  -241,  -241,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -241,
    -241,  -241,  -241,  -241,     0,     0,  -241,     0,  -241,     0,
       0,     0,     0,  -241,     0,     0,     0,  -241,     0,  -241,
    -241,     0,  -241,     0,     0,     0,  -241,  -241,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -241,     0,
    -241,     0,     0,     0,     0,  -241,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -241,     0,  -241,     0,  -241,  -241,  -241,     0,
       0,  -241,     0,     0,     0,  -241,  -241,  -241,     0,  -241,
       0,     0,  -241,     0,     0,     0,     0,     0,     0,     0,
       0,  -241,  -241,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -241,  -241,     0,  -241,     0,
       0,  -241,     0,     0,  -241,     0,     0,     0,     0,     0,
    -241,  -241,     0,     0,     0,     0,     0,     0,  -241,     0,
       0,     0,     0,     0,  -241,  -241,  -241,     0,  -241,     0,
       0,     0,  -241,     0,     0,     0,     0,     0,  -241,     0,
       0,     0,  -241,     0,     0,     0,     0,     0,     0,  -241,
       0,     0,     0,     0,  -241,  -241,  -241,     0,     0,     0,
       0,     0,     0,     0,     0,  -241,     0,  -241,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -241,     0,     0,
       0,     0,     0,   913,  -241,  -241,  -241,  -241,  -241,     0,
       7,     8,     9,    10,    11,     0,    12,    13,  -241,    14,
      15,     0,     0,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,     0,    26,     0,    27,     0,    28,    29,
      30,    31,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,     0,     0,     0,    41,     0,     0,
      42,    43,    44,     0,    45,     0,     0,     0,    46,     0,
       0,     0,     0,     0,     0,     0,     0,    47,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
       0,     0,     0,     0,     0,     0,     0,     0,    51,     0,
       0,    52,     0,     0,     0,     0,    53,     0,     0,    54,
      55,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    56,    57,    58,    59,
      60,     0,     0,    61,     0,    62,     0,     0,     0,     0,
      63,     0,     0,     0,    64,     0,    65,    66,     0,    67,
       0,     0,     0,    68,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    70,     0,    71,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,    74,     0,    75,    76,    77,     0,     0,    78,     0,
       0,     0,    79,    80,    81,     0,    82,     0,     0,    83,
       0,     0,     0,     0,     0,     0,     0,     0,    84,    85,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    86,    87,     0,    88,     0,     0,    89,     0,
       0,    90,     0,     0,     0,     0,     0,    91,    92,     0,
       0,     0,     0,     0,     0,    93,     0,     0,     0,     0,
       0,    94,    95,    96,     0,    97,     0,     0,     0,    98,
       0,     0,     0,     0,     0,    99,     0,     0,     0,   100,
       0,     0,     0,     0,     0,     0,   101,     0,     0,     0,
       0,   102,   103,   104,     0,     0,     0,     0,     0,     0,
       0,     0,   105,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   107,     0,     0,     0,     0,     0,
     947,   108,   109,   110,   111,   112,     0,  -238,  -238,  -238,
    -238,  -238,     0,  -238,  -238,   113,  -238,  -238,     0,     0,
    -238,  -238,  -238,  -238,  -238,  -238,  -238,  -238,  -238,  -238,
       0,  -238,     0,  -238,     0,  -238,  -238,  -238,  -238,     0,
       0,  -238,  -238,  -238,  -238,  -238,  -238,  -238,  -238,  -238,
       0,     0,     0,     0,  -238,     0,     0,  -238,  -238,  -238,
       0,  -238,     0,     0,     0,  -238,     0,     0,     0,     0,
       0,     0,     0,     0,  -238,  -238,  -238,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -238,     0,     0,     0,
       0,     0,     0,     0,     0,  -238,     0,     0,  -238,     0,
       0,     0,     0,  -238,     0,     0,  -238,  -238,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -238,  -238,  -238,  -238,  -238,     0,     0,
    -238,     0,  -238,     0,     0,     0,     0,  -238,     0,     0,
       0,  -238,     0,  -238,  -238,     0,  -238,     0,     0,     0,
    -238,  -238,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -238,     0,  -238,     0,     0,     0,     0,  -238,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -238,     0,  -238,     0,
    -238,  -238,  -238,     0,     0,  -238,     0,     0,     0,  -238,
    -238,  -238,     0,  -238,     0,     0,  -238,     0,     0,     0,
       0,     0,     0,     0,     0,  -238,  -238,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -238,
    -238,     0,  -238,     0,     0,  -238,     0,     0,  -238,     0,
       0,     0,     0,     0,  -238,  -238,     0,     0,     0,     0,
       0,     0,  -238,     0,     0,     0,     0,     0,  -238,  -238,
    -238,     0,  -238,     0,     0,     0,  -238,     0,     0,     0,
       0,     0,  -238,     0,     0,     0,  -238,     0,     0,     0,
       0,     0,     0,  -238,     0,     0,     0,     0,  -238,  -238,
    -238,     0,     0,     0,     0,     0,     0,     0,     0,  -238,
       0,  -238,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -238,     0,     0,     0,     0,     0,  1106,  -238,  -238,
    -238,  -238,  -238,     0,     7,     8,     9,    10,    11,     0,
      12,    13,  -238,    14,    15,     0,     0,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,     0,    26,     0,
      27,     0,    28,    29,    30,    31,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,     0,     0,
       0,    41,     0,     0,    42,    43,    44,     0,    45,     0,
       0,     0,    46,     0,     0,     0,     0,     0,     0,     0,
       0,    47,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,     0,     0,     0,     0,     0,     0,
       0,     0,    51,     0,     0,    52,     0,     0,     0,     0,
      53,     0,     0,    54,    55,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      56,    57,    58,    59,    60,     0,     0,    61,     0,    62,
       0,     0,     0,     0,    63,     0,     0,     0,    64,     0,
      65,    66,     0,    67,     0,     0,     0,    68,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    70,
       0,    71,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,    74,     0,    75,    76,    77,
       0,     0,    78,     0,     0,     0,    79,    80,    81,     0,
      82,     0,     0,    83,     0,     0,     0,     0,     0,     0,
       0,     0,    84,    85,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    86,    87,     0,    88,
       0,     0,    89,     0,     0,    90,     0,     0,     0,     0,
       0,    91,    92,     0,     0,     0,     0,     0,     0,    93,
       0,     0,     0,     0,     0,    94,    95,    96,     0,    97,
       0,     0,     0,    98,     0,     0,     0,     0,     0,    99,
       0,     0,     0,   100,     0,     0,     0,     0,     0,     0,
     101,     0,     0,     0,     0,   102,   103,   104,     0,     0,
       0,     0,     0,     0,     0,     0,   105,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,     0,  1171,   108,   109,   110,   111,   112,
       0,     7,     8,     9,    10,    11,     0,    12,    13,   113,
      14,    15,     0,     0,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,     0,    26,     0,    27,     0,    28,
      29,    30,    31,     0,     0,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,     0,     0,     0,    41,     0,
       0,    42,    43,    44,     0,    45,     0,     0,     0,    46,
       0,     0,     0,     0,     0,     0,     0,     0,    47,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      50,     0,     0,     0,     0,     0,     0,     0,     0,    51,
       0,     0,    52,     0,     0,     0,     0,    53,     0,     0,
      54,    55,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    56,    57,    58,
      59,    60,     0,     0,    61,     0,    62,     0,     0,     0,
       0,    63,     0,     0,     0,    64,     0,    65,    66,     0,
      67,     0,     0,     0,    68,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    70,     0,    71,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,    74,     0,    75,    76,    77,     0,     0,    78,
       0,     0,     0,    79,    80,    81,     0,    82,     0,     0,
      83,     0,     0,     0,     0,     0,     0,     0,     0,    84,
      85,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    86,    87,     0,    88,     0,     0,    89,
       0,     0,    90,     0,     0,     0,     0,     0,    91,    92,
       0,     0,     0,     0,     0,     0,    93,     0,     0,     0,
       0,     0,    94,    95,    96,     0,    97,     0,     0,     0,
      98,     0,     0,     0,     0,     0,    99,     0,     0,     0,
     100,     0,     0,     0,     0,     0,     0,   101,     0,     0,
       0,     0,   102,   103,   104,     0,     0,     0,     0,     0,
       0,     0,     0,   105,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   107,     0,     0,     0,     0,
       0,     0,   108,   109,   110,   111,   112,     0,     7,     8,
       9,    10,    11,     0,    12,    13,   113,    14,    15,     0,
       0,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,     0,    26,     0,    27,     0,    28,    29,    30,    31,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,     0,     0,     0,     0,    41,     0,     0,    42,    43,
      44,     0,    45,     0,     0,     0,    46,     0,     0,     0,
       0,     0,     0,     0,     0,    47,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    50,     0,     0,
       0,     0,     0,     0,     0,     0,    51,     0,     0,    52,
       0,     0,     0,     0,    53,     0,     0,    54,    55,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    56,    57,    58,    59,    60,     0,
       0,    61,     0,    62,     0,     0,     0,     0,    63,     0,
       0,     0,    64,     0,    65,    66,     0,    67,     0,     0,
       0,    68,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    70,     0,    71,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,    74,
       0,    75,    76,    77,     0,     0,    78,     0,     0,     0,
      79,    80,    81,     0,    82,     0,     0,    83,     0,     0,
       0,     0,     0,     0,     0,     0,    84,    85,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      86,    87,     0,    88,     0,     0,    89,     0,     0,    90,
       0,     0,     0,     0,     0,    91,    92,     0,     0,     0,
       0,     0,     0,    93,     0,     0,     0,     0,     0,    94,
      95,    96,     0,    97,     0,     0,     0,    98,     0,     0,
       0,     0,     0,    99,     0,     0,     0,   100,     0,     0,
       0,     0,     0,     0,   101,     0,     0,     0,     0,   102,
     103,   104,     0,     0,     0,     0,     0,     0,     0,     0,
     105,     0,   106,     0,     0,     0,     0,   381,   382,     0,
     383,   384,   107,   385,   386,   387,   388,   389,   390,   108,
     109,   110,   111,   112,   391,   392,   393,   394,   395,     0,
     396,   397,   398,   113,     7,     8,     9,    10,    11,     0,
      12,    13,     0,    14,    15,   417,     0,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,     0,    26,     0,
      27,     0,    28,    29,    30,    31,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,   418,     0,     0,
       0,    41,     0,     0,    42,    43,    44,     0,    45,     0,
       0,     0,    46,     0,     0,     0,     0,     0,     0,     0,
       0,    47,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,     0,     0,     0,     0,     0,     0,
       0,     0,    51,     0,     0,    52,     0,     0,     0,     0,
      53,     0,     0,    54,    55,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      56,    57,    58,    59,    60,     0,     0,    61,   421,    62,
       0,     0,     0,     0,    63,     0,     0,     0,    64,     0,
      65,    66,     0,    67,     0,     0,     0,    68,    69,     0,
       0,     0,     0,     0,   422,     0,     0,     0,     0,    70,
       0,    71,   423,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,   424,     0,     0,     0,     0,     0,   425,
       0,     0,     0,    73,     0,    74,     0,    75,    76,    77,
       0,   426,    78,     0,     0,     0,    79,    80,    81,     0,
      82,     0,     0,    83,     0,     0,     0,     0,     0,     0,
       0,     0,    84,    85,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    86,    87,     0,    88,
       0,     0,    89,     0,     0,    90,     0,     0,     0,     0,
       0,    91,    92,     0,     0,     0,     0,     0,     0,    93,
       0,     0,     0,     0,     0,    94,    95,    96,     0,    97,
       0,     0,     0,    98,     0,     0,     0,     0,     0,    99,
       0,     0,     0,   100,     0,     0,     0,     0,     0,     0,
     101,     0,     0,     0,     0,   102,   103,   104,     0,     0,
       0,     0,     0,     0,   720,     0,   105,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,     0,     0,   108,   109,   110,   111,   112,
       0,     7,     8,     9,    10,    11,     0,    12,    13,   113,
      14,    15,   417,     0,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,     0,    26,     0,    27,     0,    28,
      29,    30,    31,     0,     0,    32,    33,    34,    35,    36,
      37,    38,    39,    40,   418,     0,     0,     0,    41,     0,
       0,    42,    43,    44,     0,    45,     0,     0,     0,    46,
       0,     0,     0,     0,     0,     0,     0,     0,    47,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      50,     0,     0,     0,     0,     0,     0,     0,     0,    51,
       0,     0,    52,     0,     0,     0,     0,    53,     0,     0,
      54,    55,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    56,    57,    58,
      59,    60,     0,     0,    61,   421,    62,     0,     0,     0,
       0,    63,     0,     0,     0,    64,     0,    65,    66,     0,
      67,     0,     0,     0,    68,    69,     0,     0,     0,     0,
       0,   422,     0,     0,     0,     0,    70,     0,    71,   423,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
     424,     0,     0,     0,     0,     0,   425,     0,     0,     0,
      73,     0,    74,     0,    75,    76,    77,     0,   426,    78,
       0,     0,     0,    79,    80,    81,     0,    82,     0,     0,
      83,     0,     0,     0,     0,     0,     0,     0,     0,    84,
      85,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    86,    87,     0,    88,     0,     0,    89,
       0,     0,    90,     0,     0,     0,     0,     0,    91,    92,
       0,     0,     0,     0,     0,     0,    93,     0,     0,     0,
       0,     0,    94,    95,    96,     0,    97,     0,     0,     0,
      98,     0,     0,     0,     0,     0,    99,     0,     0,     0,
     100,     0,     0,     0,     0,     0,     0,   101,     0,     0,
       0,     0,   102,   103,   104,     0,     0,     0,     0,     0,
       0,   737,     0,   105,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   107,     0,     0,     0,     0,
       0,     0,   108,   109,   110,   111,   112,     0,     7,     8,
       9,    10,    11,     0,    12,    13,   113,    14,    15,   417,
       0,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,     0,    26,     0,    27,     0,    28,    29,    30,    31,
       0,     0,    32,    33,    34,    35,    36,    37,    38,    39,
      40,   418,     0,     0,     0,    41,     0,     0,    42,    43,
      44,     0,    45,     0,     0,     0,    46,     0,     0,     0,
       0,     0,     0,     0,     0,    47,    48,    49,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    50,     0,     0,
       0,     0,     0,     0,     0,     0,    51,     0,     0,    52,
       0,     0,     0,     0,    53,     0,     0,    54,    55,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    56,    57,    58,    59,    60,     0,
       0,    61,   421,    62,     0,     0,     0,     0,    63,     0,
       0,     0,    64,     0,    65,    66,     0,    67,     0,     0,
       0,    68,    69,     0,     0,     0,     0,     0,   422,     0,
       0,     0,     0,    70,     0,    71,   423,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,   424,     0,     0,
       0,     0,     0,   425,     0,     0,     0,    73,     0,    74,
       0,    75,    76,    77,     0,   426,    78,     0,     0,     0,
      79,    80,    81,     0,    82,     0,     0,    83,     0,     0,
       0,     0,     0,     0,     0,     0,    84,    85,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      86,    87,     0,    88,     0,     0,    89,     0,     0,    90,
       0,     0,     0,     0,     0,    91,    92,     0,     0,     0,
       0,     0,     0,    93,     0,     0,     0,     0,     0,    94,
      95,    96,     0,    97,     0,     0,     0,    98,     0,     0,
       0,     0,     0,    99,     0,     0,     0,   100,     0,     0,
       0,     0,     0,     0,   101,     0,     0,     0,     0,   102,
     103,   104,     0,     0,     0,     0,     0,     0,     0,     0,
     105,   792,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   107,     0,     0,     0,     0,     0,     0,   108,
     109,   110,   111,   112,     0,     7,     8,     9,    10,    11,
       0,    12,    13,   113,    14,    15,   417,     0,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,     0,    26,
       0,    27,     0,    28,    29,    30,    31,     0,     0,    32,
      33,    34,    35,    36,    37,    38,    39,    40,   418,     0,
       0,     0,    41,     0,     0,    42,    43,    44,     0,    45,
       0,     0,     0,    46,     0,     0,     0,     0,     0,     0,
       0,     0,    47,    48,    49,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    50,     0,     0,     0,     0,     0,
       0,     0,     0,    51,     0,     0,    52,     0,     0,     0,
       0,    53,     0,     0,    54,    55,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    56,    57,    58,    59,    60,     0,     0,    61,   421,
      62,     0,     0,     0,     0,    63,     0,     0,     0,    64,
       0,    65,    66,     0,    67,     0,     0,     0,    68,    69,
       0,     0,     0,     0,     0,   422,     0,     0,     0,     0,
      70,     0,    71,   423,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,   424,     0,     0,     0,     0,     0,
     425,     0,     0,     0,    73,     0,    74,     0,    75,    76,
      77,     0,   426,    78,     0,     0,     0,    79,    80,    81,
       0,    82,     0,     0,    83,     0,     0,     0,     0,     0,
       0,     0,     0,    84,    85,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    86,    87,     0,
      88,     0,     0,    89,     0,     0,    90,     0,     0,     0,
       0,     0,    91,    92,     0,     0,     0,     0,     0,     0,
      93,     0,     0,     0,     0,     0,    94,    95,    96,     0,
      97,     0,     0,     0,    98,     0,     0,     0,     0,     0,
      99,     0,     0,     0,   100,     0,     0,     0,     0,     0,
       0,   101,     0,     0,     0,     0,   102,   103,   104,     0,
       0,     0,     0,     0,     0,     0,     0,   105,   949,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   107,
       0,     0,     0,     0,     0,     0,   108,   109,   110,   111,
     112,     0,     7,     8,     9,    10,    11,     0,    12,    13,
     113,    14,    15,   417,     0,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,     0,    26,     0,    27,     0,
      28,    29,    30,    31,     0,     0,    32,    33,    34,    35,
      36,    37,    38,    39,    40,   418,     0,     0,     0,    41,
       0,     0,    42,    43,    44,     0,    45,     0,     0,     0,
      46,     0,     0,     0,     0,     0,     0,     0,     0,    47,
      48,    49,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,     0,     0,     0,     0,     0,     0,     0,     0,
      51,     0,     0,    52,     0,     0,     0,     0,    53,     0,
       0,    54,    55,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    56,    57,
      58,    59,    60,     0,     0,    61,   421,    62,     0,     0,
       0,     0,    63,     0,     0,     0,    64,     0,    65,    66,
       0,    67,     0,     0,     0,    68,    69,     0,     0,     0,
       0,     0,   422,     0,     0,     0,     0,    70,     0,    71,
     423,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,   424,     0,     0,     0,     0,     0,   425,     0,     0,
       0,    73,     0,    74,     0,    75,    76,    77,     0,   426,
      78,     0,     0,     0,    79,    80,    81,     0,    82,     0,
       0,    83,     0,     0,     0,     0,     0,     0,     0,     0,
      84,    85,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    86,    87,     0,    88,     0,     0,
      89,     0,     0,    90,     0,     0,     0,     0,     0,    91,
      92,     0,     0,     0,     0,     0,     0,    93,     0,     0,
       0,     0,     0,    94,    95,    96,     0,    97,     0,     0,
       0,    98,     0,     0,     0,     0,     0,    99,     0,     0,
       0,   100,     0,     0,     0,     0,     0,     0,   101,     0,
       0,     0,     0,   102,   103,   104,     0,     0,     0,     0,
       0,     0,     0,     0,   105,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   107,     0,     0,     0,
       0,     0,     0,   108,   109,   110,   111,   112,     0,     7,
       8,     9,    10,    11,     0,    12,    13,   113,    14,    15,
       0,     0,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,     0,    26,     0,    27,     0,    28,    29,    30,
      31,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,    40,     0,     0,     0,     0,    41,     0,     0,    42,
      43,    44,     0,    45,     0,     0,     0,    46,     0,     0,
       0,     0,     0,     0,     0,     0,    47,    48,    49,     0,
       0,     0,     0,     0,     0,   419,   420,     0,    50,     0,
       0,     0,     0,     0,     0,     0,     0,    51,     0,     0,
      52,     0,     0,     0,     0,    53,     0,     0,    54,    55,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    56,    57,    58,    59,    60,
       0,     0,    61,     0,    62,     0,     0,     0,     0,    63,
       0,     0,     0,    64,     0,    65,    66,     0,    67,     0,
       0,     0,    68,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    70,     0,    71,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
      74,     0,    75,    76,    77,     0,     0,    78,     0,     0,
       0,    79,    80,    81,     0,    82,     0,     0,    83,     0,
       0,     0,     0,     0,     0,     0,     0,    84,    85,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    86,    87,     0,    88,     0,     0,    89,     0,     0,
      90,     0,     0,     0,     0,     0,    91,    92,     0,     0,
       0,     0,     0,     0,    93,     0,     0,     0,     0,     0,
      94,    95,    96,     0,    97,     0,     0,     0,    98,     0,
       0,     0,     0,     0,    99,     0,     0,     0,   100,     0,
       0,     0,     0,     0,     0,   101,     0,     0,     0,     0,
     102,   103,   104,     0,     0,     0,     0,     0,     0,     0,
       0,   105,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   107,     0,     0,     0,     0,     0,     0,
     108,   109,   110,   111,   112,     0,     7,     8,     9,    10,
      11,     0,    12,    13,   113,    14,    15,     0,     0,    16,
      17,    18,    19,    20,    21,    22,    23,    24,   852,     0,
      26,     0,    27,     0,    28,    29,    30,    31,     0,     0,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
       0,     0,     0,    41,     0,     0,    42,    43,    44,     0,
      45,     0,     0,     0,    46,     0,     0,     0,     0,     0,
       0,     0,     0,    47,    48,    49,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    50,     0,     0,     0,     0,
       0,     0,     0,     0,    51,     0,     0,    52,     0,     0,
       0,     0,    53,     0,     0,    54,    55,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    56,    57,    58,    59,    60,     0,     0,    61,
       0,    62,     0,     0,     0,     0,    63,     0,     0,     0,
      64,     0,    65,    66,     0,    67,     0,     0,     0,    68,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    70,     0,    71,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,    74,     0,    75,
      76,    77,     0,     0,    78,     0,     0,     0,    79,    80,
      81,     0,    82,     0,     0,    83,     0,     0,     0,     0,
       0,     0,     0,     0,    84,    85,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    86,    87,
       0,    88,     0,     0,    89,     0,     0,    90,     0,     0,
       0,     0,     0,    91,    92,     0,     0,     0,     0,     0,
       0,    93,     0,     0,     0,     0,     0,    94,    95,    96,
       0,    97,     0,     0,     0,    98,     0,     0,     0,     0,
       0,    99,     0,   853,   854,   100,     0,     0,     0,     0,
       0,     0,   101,     0,     0,     0,     0,   102,   103,   104,
       0,     0,     0,     0,     0,     0,     0,     0,   105,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,     0,     0,   108,   109,   110,
     111,   112,     0,     7,     8,     9,    10,    11,     0,    12,
      13,   113,    14,    15,     0,     0,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,     0,    26,     0,    27,
       0,    28,    29,    30,    31,     0,     0,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,     0,     0,     0,
      41,     0,     0,    42,    43,    44,     0,    45,     0,     0,
       0,    46,     0,     0,     0,     0,     0,     0,     0,     0,
      47,    48,    49,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    50,     0,     0,     0,     0,     0,     0,     0,
       0,    51,     0,     0,    52,     0,     0,     0,     0,    53,
       0,     0,    54,    55,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    56,
      57,    58,    59,    60,     0,     0,    61,     0,    62,     0,
       0,     0,     0,    63,     0,     0,     0,    64,     0,    65,
      66,     0,    67,     0,     0,     0,    68,    69,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    70,     0,
      71,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,    74,     0,    75,    76,    77,     0,
       0,    78,     0,     0,     0,    79,    80,    81,     0,    82,
       0,     0,    83,     0,     0,     0,     0,     0,     0,     0,
       0,    84,    85,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    86,    87,     0,    88,     0,
       0,    89,     0,     0,    90,     0,     0,     0,     0,     0,
      91,    92,     0,     0,     0,     0,     0,     0,    93,     0,
       0,     0,     0,     0,    94,    95,    96,     0,    97,     0,
       0,     0,    98,     0,     0,     0,     0,     0,    99,     0,
       0,     0,   100,     0,     0,     0,     0,     0,     0,   101,
       0,     0,     0,     0,   102,   103,   104,     0,     0,     0,
       0,   399,     0,     0,     0,   105,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   107,     0,     0,
       0,     0,     0,     0,   108,   109,   110,   111,   112,     0,
       7,     8,     9,    10,    11,     0,    12,    13,   113,    14,
      15,     0,     0,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,     0,    26,     0,    27,     0,    28,    29,
      30,    31,     0,     0,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,     0,     0,     0,    41,     0,     0,
      42,    43,    44,     0,    45,     0,     0,     0,    46,     0,
       0,     0,     0,     0,     0,     0,     0,    47,    48,    49,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    50,
       0,     0,     0,     0,     0,     0,     0,     0,    51,     0,
       0,    52,     0,     0,     0,     0,    53,     0,     0,    54,
      55,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    56,    57,    58,    59,
      60,     0,     0,    61,     0,    62,     0,     0,     0,     0,
      63,     0,     0,     0,    64,     0,    65,    66,     0,    67,
       0,     0,     0,    68,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    70,     0,    71,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,    74,     0,    75,    76,    77,     0,     0,    78,     0,
       0,     0,    79,    80,    81,     0,    82,     0,     0,    83,
       0,     0,     0,     0,     0,     0,     0,     0,    84,    85,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    86,    87,     0,    88,     0,     0,    89,     0,
       0,    90,     0,     0,     0,     0,     0,    91,    92,     0,
       0,     0,     0,     0,     0,    93,     0,     0,     0,     0,
       0,    94,    95,    96,     0,    97,     0,     0,     0,    98,
       0,     0,     0,     0,     0,    99,     0,     0,     0,   100,
       0,     0,     0,     0,     0,     0,   101,     0,     0,     0,
       0,   102,   103,   104,     0,     0,     0,     0,   849,     0,
       0,     0,   105,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   107,     0,     0,     0,     0,     0,
       0,   108,   109,   110,   111,   112,     0,     7,     8,     9,
      10,    11,     0,    12,    13,   113,    14,    15,     0,     0,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
       0,    26,     0,    27,     0,    28,    29,    30,    31,     0,
       0,    32,    33,    34,    35,    36,    37,    38,    39,    40,
       0,     0,     0,     0,    41,     0,     0,    42,    43,    44,
       0,    45,     0,     0,     0,    46,     0,     0,     0,     0,
       0,     0,     0,     0,    47,    48,    49,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    50,     0,     0,     0,
       0,     0,     0,     0,     0,    51,     0,     0,    52,     0,
       0,     0,     0,    53,     0,     0,    54,    55,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    56,    57,    58,    59,    60,     0,     0,
      61,     0,    62,     0,     0,     0,     0,    63,     0,     0,
       0,    64,     0,    65,    66,     0,    67,     0,     0,     0,
      68,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    70,     0,    71,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,    74,     0,
      75,    76,    77,     0,     0,    78,     0,     0,     0,    79,
      80,    81,     0,    82,     0,     0,    83,     0,     0,     0,
       0,     0,     0,     0,     0,    84,    85,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    86,
      87,     0,    88,     0,     0,    89,     0,     0,    90,     0,
       0,     0,     0,     0,    91,    92,     0,     0,     0,     0,
       0,     0,    93,     0,     0,     0,     0,     0,    94,    95,
      96,     0,    97,     0,     0,     0,    98,     0,     0,     0,
       0,     0,    99,     0,     0,     0,   100,     0,     0,     0,
       0,     0,     0,   101,     0,     0,     0,     0,   102,   103,
     104,     0,     0,     0,     0,     0,     0,     0,     0,   105,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   107,     0,     0,     0,     0,     0,     0,   108,   109,
     110,   111,   112,     0,     7,     8,     9,    10,    11,     0,
      12,    13,   113,    14,    15,     0,     0,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,     0,    26,     0,
      27,     0,    28,    29,    30,    31,     0,     0,    32,    33,
      34,    35,    36,    37,    38,    39,    40,     0,     0,     0,
       0,    41,     0,     0,    42,    43,    44,     0,    45,     0,
       0,     0,    46,     0,     0,     0,     0,     0,     0,     0,
       0,    47,    48,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    50,     0,     0,     0,     0,     0,     0,
       0,     0,    51,     0,     0,    52,     0,     0,     0,     0,
      53,     0,     0,    54,    55,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      56,    57,    58,    59,    60,     0,     0,    61,     0,    62,
       0,     0,     0,     0,    63,     0,     0,     0,    64,     0,
      65,    66,     0,    67,     0,     0,     0,    68,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    70,
       0,    71,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,    74,     0,    75,    76,    77,
       0,     0,    78,     0,     0,     0,    79,    80,    81,     0,
      82,     0,     0,    83,     0,     0,     0,     0,     0,     0,
       0,     0,    84,    85,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    86,    87,     0,    88,
       0,     0,    89,     0,     0,    90,     0,     0,     0,     0,
       0,   867,    92,     0,     0,     0,     0,     0,     0,    93,
       0,     0,     0,     0,     0,    94,    95,    96,     0,    97,
       0,     0,     0,    98,     0,     0,     0,     0,     0,    99,
       0,     0,     0,   100,     0,     0,     0,     0,     0,     0,
     101,     0,     0,     0,     0,   102,   103,   104,     0,     0,
       0,     0,     0,     0,     0,     0,   105,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,     0,     0,   108,   109,   110,   111,   112,
       0,     7,     8,     9,    10,    11,     0,    12,    13,   113,
      14,    15,     0,     0,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,     0,    26,     0,    27,     0,    28,
      29,    30,    31,     0,     0,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,     0,     0,     0,    41,     0,
       0,    42,    43,    44,     0,    45,     0,     0,     0,    46,
       0,     0,     0,     0,     0,     0,     0,     0,    47,    48,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      50,     0,     0,     0,     0,     0,     0,     0,     0,    51,
       0,     0,    52,     0,     0,     0,     0,    53,     0,     0,
      54,    55,     0,   -62,   435,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    56,    57,    58,
      59,    60,   417,     0,    61,     0,    62,     0,     0,     0,
       0,    63,     0,     0,     0,    64,     0,    65,    66,     0,
      67,     0,     0,     0,    68,    69,     0,     0,     0,     0,
       0,     0,     0,     0,   418,     0,    70,     0,    71,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1251,     0,     0,     0,     0,     0,     0,
      73,     0,    74,     0,    75,    76,    77,   419,   420,    78,
       0,     0,     0,    79,    80,    81,     0,    82,     0,     0,
      83,     0,     0,     0,     0,     0,     0,     0,     0,    84,
      85,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1255,     0,     0,    86,    87,     0,    88,     0,     0,    89,
       0,     0,    90,     0,     0,   421,     0,     0,   871,    92,
       0,     0,     0,     0,     0,     0,    93,     0,     0,     0,
       0,     0,    94,    95,    96,     0,    97,     0,     0,     0,
      98,   422,     0,     0,  1257,     0,    99,     0,     0,   423,
     100,     0,     0,     0,     0,     0,     0,   101,     0,     0,
     424,     0,   102,   103,   104,     0,   425,     0,   -62,     0,
       0,     0,     0,   105,     0,   106,     0,     0,   426,     0,
       0,     0,   -62,     0,     0,   107,   406,     0,     0,     0,
       0,     0,   108,   109,   110,   111,   112,   407,     0,     0,
       0,     0,     0,    15,     0,     0,   113,   -62,     0,    19,
       0,     0,    22,    23,     0,     0,  1112,    26,   -62,    27,
       0,    28,    29,    30,    31,     0,     0,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,     0,     0,     0,
      41,     0,     0,    42,    43,    44,     0,    45,     0,   381,
     382,    46,   383,   384,     0,   385,   386,   387,   388,   389,
     390,    48,   408,     0,     0,     0,   391,   392,   393,   394,
     395,     0,   396,   397,   398,     0,     0,     0,   -62,     0,
       0,     0,   -62,     0,   -62,     0,     0,   -62,     0,   -62,
       0,     0,    54,    55,     0,     0,   381,   382,     0,   383,
     384,     0,   385,   386,   387,   388,   389,   390,     0,     0,
       0,     0,     0,   391,   392,   393,   394,   395,    62,   396,
     397,   398,     0,     0,     0,     0,     0,    64,     0,    65,
      66,     0,   409,     0,     0,     0,    68,     0,     0,     0,
       0,     0,     0,   381,   382,     0,   383,   384,    70,   385,
     386,   387,   388,   389,   390,    72,     0,     0,     0,     0,
     391,   392,   393,   394,   395,     0,   396,   397,   398,     0,
       0,     0,    73,     0,    74,     0,    75,    76,    77,     0,
       0,     0,     0,     0,     0,     0,     0,   381,   382,    82,
     383,   384,     0,   385,   386,   387,   388,   389,   390,     0,
       0,     0,     0,     0,   391,   392,   393,   394,   395,     0,
     396,   397,   398,     0,     0,     0,    87,     0,    88,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    93,     0,
       0,     0,     0,     0,     0,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    99,     0,
     -28,   126,     0,     0,     0,   127,   128,     0,     0,     0,
       0,     0,   129,     0,     0,     0,   410,     0,     0,     0,
     130,     0,     0,     0,     0,    20,     0,     0,     0,   131,
       0,   132,     0,   133,     0,     0,    28,    29,    30,    31,
     134,   135,     0,     0,   108,   109,   110,   111,   112,    39,
       0,     0,   136,     0,     0,     0,   137,   138,     0,     0,
       0,     0,     0,   139,     0,   140,     0,   141,   142,   143,
     144,   145,   146,   147,   148,     0,     0,   149,   150,   151,
     152,   153,   154,   155,     0,     0,   156,    50,   157,   158,
     159,   160,   161,   162,   163,   164,     0,     0,   165,   166,
     167,     0,   168,   169,    53,   170,   171,     0,     0,   172,
     173,   174,   175,   176,   177,   178,   179,   180,     0,   181,
     182,   183,   184,   185,     0,     0,     0,     0,     0,   186,
     187,     0,     0,     0,     0,   188,   189,   190,   191,   192,
     193,     0,     0,   194,     0,     0,   195,   196,   197,   198,
     199,     0,   200,   201,   202,   203,   204,   205,     0,   206,
     207,   208,   209,     0,   210,    71,     0,   211,   212,   213,
       0,     0,     0,     0,     0,   214,   215,   216,   217,   218,
     219,   220,   221,     0,   222,   223,   224,     0,     0,     0,
     225,     0,     0,     0,   226,   227,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   228,
     229,   230,     0,   231,   232,   233,     0,   234,   235,   236,
     237,     0,   238,   239,     0,     0,   240,   241,     0,   242,
       0,     0,     0,     0,     0,   -28,   243,   244,   245,     0,
     246,   247,   248,   249,   250,     0,     0,   251,   252,   253,
     254,   255,     0,     0,   256,   257,   258,   259,   260,     0,
       0,     0,   261,   262,   263,   264,   265,     0,   266,   267,
     268,   269,   270,     0,     0,     0,     0,     0,   271,   272,
     273,   274,   275,   276,   277,   278,   279,   280,   281,     0,
       0,   104,   695,     0,     0,   282,   -29,   -29,   283,   -28,
       0,     0,     0,   -29,     0,     0,   284,     0,   285,     0,
       0,   -29,     0,     0,     0,     0,   -29,     0,     0,     0,
     -29,     0,   -29,     0,   -29,     0,     0,   -29,   -29,   -29,
     -29,   -29,   -29,     0,     0,     0,     0,     0,     0,     0,
     -29,     0,     0,   -29,     0,     0,     0,   -29,   -29,     0,
       0,     0,     0,     0,   -29,     0,   -29,     0,   -29,   -29,
     -29,   -29,   -29,   -29,   -29,   -29,     0,     0,   -29,   -29,
     -29,   -29,   -29,   -29,   -29,     0,     0,   -29,   -29,   -29,
     -29,   -29,   -29,   -29,   -29,   -29,   -29,     0,     0,   -29,
     -29,   -29,     0,   -29,   -29,   -29,   -29,   -29,     0,     0,
     -29,   -29,   -29,   -29,   -29,   -29,   -29,   -29,   -29,     0,
     -29,   -29,   -29,   -29,   -29,     0,     0,     0,     0,     0,
     -29,   -29,     0,     0,     0,     0,   -29,   -29,   -29,   -29,
     -29,   -29,     0,     0,   -29,     0,     0,   -29,   -29,   -29,
     -29,   -29,     0,   -29,   -29,   -29,   -29,   -29,   -29,     0,
     -29,   -29,   -29,   -29,     0,   -29,   -29,     0,   -29,   -29,
     -29,     0,     0,     0,     0,     0,   -29,   -29,   -29,   -29,
     -29,   -29,   -29,   -29,     0,   -29,   -29,   -29,     0,     0,
       0,   -29,     0,     0,     0,   -29,   -29,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     -29,   -29,   -29,     0,   -29,   -29,   -29,     0,   -29,   -29,
     -29,   -29,     0,   -29,   -29,     0,     0,   -29,   -29,     0,
     -29,     0,     0,     0,     0,     0,     0,   -29,   -29,   -29,
       0,   -29,   -29,   -29,   -29,   -29,     0,     0,   -29,   -29,
     -29,   -29,   -29,     0,     0,   -29,   -29,   -29,   -29,   -29,
       0,     0,     0,   -29,   -29,   -29,   -29,   -29,     0,   -29,
     -29,   -29,   -29,   -29,     0,     0,     0,     0,     0,   -29,
     -29,   -29,   -29,   -29,   -29,   -29,   -29,   -29,   -29,   -29,
       0,     0,   -29,   126,     0,     0,   -29,   127,   128,   -29,
     696,     0,     0,     0,   129,     0,     0,   -29,     0,   -29,
       0,     0,   130,     0,     0,     0,     0,    20,     0,     0,
       0,   131,     0,   132,     0,   133,     0,     0,    28,    29,
      30,    31,   134,   135,     0,     0,     0,     0,     0,     0,
       0,    39,     0,     0,   136,     0,     0,     0,   137,   138,
       0,     0,     0,     0,     0,   139,     0,   140,     0,   141,
     142,   143,   144,   145,   146,   147,   148,     0,     0,   149,
     150,   151,   152,   153,   154,   155,     0,     0,   156,    50,
     157,   158,   159,   160,   161,   162,   163,   164,     0,     0,
     165,   166,   167,     0,   168,   169,    53,   170,   171,     0,
       0,   172,   173,   174,   175,   176,   177,   178,   179,   180,
       0,   181,   182,   183,   184,   185,     0,     0,     0,     0,
       0,   186,   187,     0,     0,     0,     0,   188,   189,   190,
     191,   192,   193,     0,     0,   194,     0,     0,   195,   196,
     197,   198,   199,     0,   200,   201,   202,   203,   204,   205,
       0,   206,   207,   208,   209,     0,   210,    71,     0,   211,
     212,   213,     0,     0,     0,     0,     0,   214,   215,   216,
     217,   218,   219,   220,   221,     0,   222,   223,   224,     0,
       0,     0,   225,     0,     0,     0,   226,   227,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   228,   229,   230,     0,   231,   232,   233,     0,   234,
     235,   236,   237,     0,   238,   239,     0,     0,   240,   241,
       0,   242,     0,     0,     0,     0,     0,     0,   243,   244,
     245,     0,   246,   247,   248,   249,   250,     0,     0,   251,
     252,   253,   254,   255,     0,     0,   256,   257,   258,   259,
     260,     0,     0,     0,   261,   262,   263,   264,   265,     0,
     266,   267,   268,   269,   270,     0,     0,     0,     0,     0,
     271,   272,   273,   274,   275,   276,   277,   278,   279,   280,
     281,     0,     0,   104,   402,     0,     0,   282,   127,   128,
     283,     0,     0,     0,     0,   129,     0,     0,   284,     0,
     285,     0,     0,   130,     0,     0,     0,     0,    20,     0,
       0,     0,   131,     0,   132,     0,   133,     0,     0,    28,
      29,    30,    31,   134,   135,     0,     0,     0,     0,     0,
       0,     0,    39,     0,     0,   136,     0,     0,     0,   137,
     138,     0,     0,     0,     0,     0,   139,     0,   140,     0,
     141,   142,   143,   144,   145,   146,   147,   148,     0,     0,
     149,   150,   151,   152,   153,   154,   155,     0,     0,   156,
      50,   157,   158,   159,   160,   161,   162,   163,   164,     0,
       0,   165,   166,   167,     0,   168,   169,    53,   170,   171,
       0,     0,   172,   173,   174,   175,   176,   177,   178,   179,
     180,     0,   181,   182,   183,   184,   185,     0,     0,     0,
       0,     0,   186,   187,     0,     0,     0,     0,   188,   189,
     190,   191,   192,   193,     0,     0,   194,     0,     0,   195,
     196,   197,   198,   199,     0,   200,   201,   202,   203,   204,
     205,     0,   206,   207,   208,   209,     0,   210,    71,     0,
     211,   212,   213,     0,     0,     0,     0,     0,   214,   215,
     216,   217,   218,   219,   220,   221,     0,   222,   223,   224,
       0,     0,     0,   225,     0,     0,     0,   226,   227,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   228,   229,   230,     0,   231,   232,   233,     0,
     234,   235,   236,   237,     0,   238,   239,     0,     0,   240,
     241,     0,   242,     0,     0,     0,     0,     0,     0,   243,
     244,   245,     0,   246,   247,   248,   249,   250,     0,     0,
     251,   252,   253,   254,   255,     0,     0,   256,   257,   258,
     259,   260,     0,     0,     0,   261,   262,   263,   264,   265,
       0,   266,   267,   268,   269,   270,     0,     0,     0,     0,
       0,   271,   272,   273,   274,   275,   276,   277,   278,   279,
     280,   281,     0,     0,   104,   623,     0,     0,   282,   -93,
     -93,   283,     0,     0,     0,     0,   -93,     0,     0,   284,
       0,   285,     0,     0,   -93,     0,     0,     0,     0,   -93,
       0,     0,     0,   -93,     0,   -93,     0,   -93,     0,     0,
     -93,   -93,   -93,   -93,   -93,   -93,     0,     0,     0,     0,
       0,     0,     0,   -93,     0,     0,   -93,     0,     0,     0,
     -93,   -93,     0,     0,     0,     0,     0,   -93,     0,   -93,
       0,   -93,   -93,   -93,   -93,   -93,   -93,   -93,   -93,     0,
       0,   -93,   -93,   -93,   -93,   -93,   -93,   -93,     0,     0,
     -93,   -93,   -93,   -93,   -93,   -93,   -93,   -93,   -93,   -93,
       0,     0,   -93,   -93,   -93,     0,   -93,   -93,   -93,   -93,
     -93,     0,     0,   -93,   -93,   -93,   -93,   -93,   -93,   -93,
     -93,   -93,     0,   -93,   -93,   -93,   -93,   -93,     0,     0,
       0,     0,     0,   -93,   -93,     0,     0,     0,     0,   -93,
     -93,   -93,   -93,   -93,   -93,     0,     0,   -93,     0,     0,
     -93,   -93,   -93,   -93,   -93,     0,   -93,   -93,   -93,   -93,
     -93,   -93,     0,   -93,   -93,   -93,   -93,     0,   -93,   -93,
       0,   -93,   -93,   -93,     0,     0,     0,     0,     0,   -93,
     -93,   -93,   -93,   -93,   -93,   -93,   -93,     0,   -93,   -93,
     -93,     0,     0,     0,   -93,     0,     0,     0,   -93,   -93,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   -93,   -93,   -93,     0,   -93,   -93,   -93,
       0,   -93,   -93,   -93,   -93,     0,   -93,   -93,     0,     0,
     -93,   -93,     0,   -93,     0,     0,     0,     0,     0,     0,
     -93,   -93,   -93,     0,   -93,   -93,   -93,   -93,   -93,     0,
       0,   -93,   -93,   -93,   -93,   -93,     0,     0,   -93,   -93,
     -93,   -93,   -93,     0,     0,     0,   -93,   -93,   -93,   -93,
     -93,     0,   -93,   -93,   -93,   -93,   -93,     0,     0,     0,
       0,     0,   -93,   -93,   -93,   -93,   -93,   -93,   -93,   -93,
     -93,   -93,   -93,     0,     0,   -93,   715,     0,     0,   -93,
     127,   128,   -93,     0,     0,     0,     0,   129,     0,     0,
     -93,     0,   -93,     0,     0,   130,     0,     0,     0,     0,
      20,     0,     0,     0,   131,     0,   132,     0,   133,     0,
       0,    28,    29,    30,    31,   134,   135,     0,     0,     0,
       0,     0,     0,     0,    39,     0,     0,   136,     0,     0,
       0,   137,   138,     0,     0,     0,     0,     0,   139,     0,
     140,     0,   141,   142,   143,   144,   145,   146,   147,   148,
       0,     0,   149,   150,   151,   152,   153,   154,   155,     0,
       0,   156,    50,   157,   158,   159,   160,   161,   162,   163,
     164,     0,     0,   165,   166,   167,     0,   168,   169,    53,
     170,   171,     0,     0,   172,   173,   174,   175,   176,   177,
     178,   179,   180,     0,   181,   182,   183,   184,   185,     0,
       0,     0,     0,     0,   186,   187,     0,     0,     0,     0,
     188,   189,   190,   191,   192,   193,     0,     0,   194,     0,
       0,   195,   196,   197,   198,   199,     0,   200,   201,   202,
     203,   204,   205,     0,   206,   207,   208,   209,     0,   210,
      71,     0,   211,   212,   213,     0,     0,     0,     0,     0,
     214,   215,   216,   217,   218,   219,   220,   221,     0,   222,
     223,   224,     0,     0,     0,   225,     0,     0,     0,   226,
     227,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   228,   229,   230,     0,   231,   232,
     233,     0,   234,   235,   236,   237,     0,   238,   239,     0,
       0,   240,   241,     0,   242,     0,     0,     0,     0,     0,
       0,   243,   244,   245,     0,   246,   247,   248,   249,   250,
       0,     0,   251,   252,   253,   254,   255,     0,     0,   256,
     257,   258,   259,   260,     0,     0,     0,   261,   262,   263,
     264,   265,     0,   266,   267,   268,   269,   270,     0,     0,
       0,     0,     0,   271,   272,   273,   274,   275,   276,   277,
     278,   279,   280,   281,     0,     0,   104,   939,     0,     0,
       0,   127,   128,   283,     0,     0,   716,     0,   129,     0,
       0,   284,     0,   285,     0,     0,   130,     0,     0,     0,
       0,    20,     0,     0,     0,   131,     0,   132,     0,   133,
       0,     0,    28,    29,    30,    31,   134,   135,     0,     0,
       0,     0,     0,     0,     0,    39,     0,     0,   136,     0,
       0,     0,   137,   138,     0,     0,     0,     0,     0,   139,
       0,   140,     0,   141,   142,   143,   144,   145,   146,   147,
     148,     0,     0,   149,   150,   151,   152,   153,   154,   155,
       0,     0,   156,    50,   157,   158,   159,   160,   161,   162,
     163,   164,     0,     0,   165,   166,   167,     0,   168,   169,
      53,   170,   171,     0,     0,   172,   173,   174,   175,   176,
     177,   178,   179,   180,     0,   181,   182,   183,   184,   185,
       0,     0,     0,     0,     0,   186,   187,     0,     0,     0,
       0,   188,   189,   190,   191,   192,   193,     0,     0,   194,
       0,     0,   195,   196,   197,   198,   199,     0,   200,   201,
     202,   203,   204,   205,     0,   206,   207,   208,   209,     0,
     210,    71,     0,   211,   212,   213,     0,     0,     0,     0,
       0,   214,   215,   216,   217,   218,   219,   220,   221,     0,
     222,   223,   224,     0,     0,     0,   225,     0,     0,     0,
     226,   227,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   228,   229,   230,     0,   231,
     232,   233,     0,   234,   235,   236,   237,     0,   238,   239,
       0,     0,   240,   241,     0,   242,     0,     0,     0,     0,
       0,     0,   243,   244,   245,     0,   246,   247,   248,   249,
     250,     0,     0,   251,   252,   253,   254,   255,     0,     0,
     256,   257,   258,   259,   260,     0,     0,     0,   261,   262,
     263,   264,   265,     0,   266,   267,   268,   269,   270,     0,
       0,     0,     0,     0,   271,   272,   273,   274,   275,   276,
     277,   278,   279,   280,   281,     0,     0,   104,   126,     0,
       0,   282,   127,   128,   283,     0,     0,     0,     0,   129,
       0,     0,   284,     0,   285,     0,     0,   130,     0,     0,
       0,     0,    20,     0,     0,     0,   131,     0,   132,     0,
     133,     0,     0,    28,    29,    30,    31,   134,   135,     0,
       0,     0,     0,     0,     0,     0,    39,     0,     0,   136,
       0,     0,     0,   137,   138,     0,     0,     0,     0,     0,
     139,     0,   140,     0,   141,   142,   143,   144,   145,   146,
     147,   148,     0,     0,   149,   150,   151,   152,   153,   154,
     155,     0,     0,   156,    50,   157,   158,   159,   160,   161,
     162,   163,   164,     0,     0,   165,   166,   167,     0,   168,
     169,    53,   170,   171,     0,     0,   172,   173,   174,   175,
     176,   177,   178,   179,   180,     0,   181,   182,   183,   184,
     185,     0,     0,     0,     0,     0,   186,   187,     0,     0,
       0,     0,   188,   189,   190,   191,   192,   193,     0,     0,
     194,     0,     0,   195,   196,   197,   198,   199,     0,   200,
     201,   202,   203,   204,   205,     0,   206,   207,   208,   209,
       0,   210,    71,     0,   211,   212,   213,     0,     0,     0,
       0,     0,   214,   215,   216,   217,   218,   219,   220,   221,
       0,   222,   223,   224,     0,     0,     0,   225,     0,     0,
       0,   226,   227,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   228,   229,   230,     0,
     231,   232,   233,     0,   234,   235,   236,   237,     0,   238,
     239,     0,     0,   240,   241,     0,   242,     0,     0,     0,
       0,     0,     0,   243,   244,   245,     0,   246,   247,   248,
     249,   250,     0,     0,   251,   252,   253,   254,   255,     0,
       0,   256,   257,   258,   259,   260,     0,     0,     0,   261,
     262,   263,   264,   265,     0,   266,   267,   268,   269,   270,
       0,     0,     0,     0,     0,   271,   272,   273,   274,   275,
     276,   277,   278,   279,   280,   281,     0,   718,   104,     0,
       0,   127,   128,     0,     0,   283,     0,     0,   129,     0,
       0,     0,     0,   284,     0,   285,   130,     0,     0,     0,
       0,    20,     0,     0,     0,   131,     0,   132,     0,   133,
       0,     0,    28,    29,    30,    31,   134,   135,     0,     0,
       0,     0,     0,     0,     0,    39,     0,     0,   136,     0,
       0,     0,   137,   138,     0,     0,     0,     0,     0,   139,
       0,   140,     0,   141,   142,   143,   144,   145,   146,   147,
     148,     0,     0,   149,   150,   151,   152,   153,   154,   155,
       0,     0,   156,    50,   157,   158,   159,   160,   161,   162,
     163,   164,     0,     0,   165,   166,   167,     0,   168,   169,
      53,   170,   171,     0,     0,   172,   173,   174,   175,   176,
     177,   178,   179,   180,     0,   181,   182,   183,   184,   185,
       0,     0,     0,     0,     0,   186,   187,     0,     0,     0,
       0,   188,   189,   190,   191,   192,   193,     0,     0,   194,
       0,     0,   195,   196,   197,   198,   199,     0,   200,   201,
     202,   203,   204,   205,     0,   206,   207,   208,   209,     0,
     210,    71,     0,   211,   212,   213,     0,     0,     0,     0,
       0,   214,   215,   216,   217,   218,   219,   220,   221,     0,
     222,   223,   224,     0,     0,     0,   225,     0,     0,     0,
     226,   227,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   228,   229,   230,     0,   231,
     232,   233,     0,   234,   235,   236,   237,     0,   238,   239,
       0,     0,   240,   241,     0,   242,     0,     0,     0,     0,
       0,     0,   243,   244,   245,     0,   246,   247,   248,   249,
     250,     0,     0,   251,   252,   253,   254,   255,     0,     0,
     256,   257,   258,   259,   260,     0,     0,     0,   261,   262,
     263,   264,   265,     0,   266,   267,   268,   269,   270,     0,
       0,     0,     0,     0,   271,   272,   273,   274,   275,   276,
     277,   278,   279,   280,   281,     0,   928,   104,     0,     0,
    -425,  -425,     0,     0,   283,     0,     0,  -425,     0,     0,
       0,     0,   284,     0,   285,  -425,     0,     0,     0,     0,
    -425,     0,     0,     0,  -425,     0,  -425,     0,  -425,     0,
       0,  -425,  -425,  -425,  -425,  -425,  -425,     0,     0,     0,
       0,     0,     0,     0,  -425,     0,     0,  -425,     0,     0,
       0,  -425,  -425,     0,     0,     0,     0,     0,  -425,     0,
    -425,     0,  -425,  -425,  -425,  -425,  -425,  -425,  -425,  -425,
       0,     0,  -425,  -425,  -425,  -425,  -425,  -425,  -425,     0,
       0,  -425,  -425,  -425,  -425,  -425,  -425,  -425,  -425,  -425,
    -425,     0,     0,  -425,  -425,  -425,     0,  -425,  -425,  -425,
    -425,  -425,     0,     0,  -425,  -425,  -425,  -425,  -425,  -425,
    -425,  -425,  -425,     0,  -425,  -425,  -425,  -425,  -425,     0,
       0,     0,     0,     0,  -425,  -425,     0,     0,     0,     0,
    -425,  -425,  -425,  -425,  -425,  -425,     0,     0,  -425,     0,
       0,  -425,  -425,  -425,  -425,  -425,     0,  -425,  -425,  -425,
    -425,  -425,  -425,     0,  -425,  -425,  -425,  -425,     0,  -425,
    -425,     0,  -425,  -425,  -425,     0,     0,     0,     0,     0,
    -425,  -425,  -425,  -425,  -425,  -425,  -425,  -425,     0,  -425,
    -425,  -425,     0,     0,     0,  -425,     0,     0,     0,  -425,
    -425,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -425,  -425,  -425,     0,  -425,  -425,
    -425,     0,  -425,  -425,  -425,  -425,     0,  -425,  -425,     0,
       0,  -425,  -425,     0,  -425,     0,     0,     0,     0,     0,
       0,  -425,  -425,  -425,     0,  -425,  -425,  -425,  -425,  -425,
       0,     0,  -425,  -425,  -425,  -425,  -425,     0,     0,  -425,
    -425,  -425,  -425,  -425,     0,     0,     0,  -425,  -425,  -425,
    -425,  -425,     0,  -425,  -425,  -425,  -425,  -425,     0,     0,
       0,     0,     0,  -425,  -425,  -425,  -425,  -425,  -425,  -425,
    -425,  -425,  -425,  -425,   126,     0,  -425,     0,   127,     0,
       0,     0,     0,  -425,     0,     0,     0,     0,     0,     0,
       0,  -425,     0,  -425,     0,     0,     0,     0,    20,     0,
       0,     0,     0,     0,   132,     0,   133,     0,     0,    28,
      29,    30,    31,   134,   135,     0,     0,     0,     0,     0,
       0,     0,    39,     0,     0,   136,     0,     0,     0,   137,
     982,     0,     0,     0,     0,     0,   139,     0,   140,     0,
     141,   142,   143,   144,   145,   146,   147,   148,     0,     0,
       0,   150,   151,   152,   153,   154,   155,     0,     0,     0,
      50,   157,   158,   159,   160,   161,   162,   163,   164,     0,
       0,   165,   983,   167,     0,   168,   169,    53,   170,   171,
       0,     0,   172,   173,   174,   175,   176,     0,   178,   179,
     180,     0,   181,     0,   183,   184,   185,     0,     0,     0,
       0,     0,   186,   187,     0,     0,     0,     0,   188,   189,
       0,     0,   192,   193,     0,     0,     0,     0,     0,   195,
       0,   197,   198,   199,     0,   200,   201,   202,   203,   204,
     205,     0,   206,   207,   208,   209,     0,   210,    71,     0,
     211,   212,   213,     0,     0,     0,     0,     0,   984,   215,
       0,   217,   218,   219,     0,   221,     0,   222,   223,   985,
       0,     0,     0,   225,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   -58,   491,     0,     0,     0,
       0,     0,     0,   229,   230,     0,   231,   986,   233,     0,
       0,   235,   236,   237,   417,   238,   987,     0,     0,   240,
     241,     0,   242,     0,     0,     0,   -66,   555,     0,     0,
     244,   245,     0,   246,   247,   248,   249,   250,     0,     0,
     251,   252,   253,   254,   255,   417,   418,   256,   257,   258,
     259,   260,     0,  -803,   664,   261,    97,   263,   264,   265,
       0,   266,   267,   268,   269,   270,     0,     0,  -296,   632,
       0,   271,   272,   273,   274,   275,   276,   418,  -801,   419,
     420,   281,     0,     0,   104,     0,     0,   417,     0,  -801,
    -801,  -801,  -801,     0,     0,     0,     0,     0,     0,   284,
       0,   285,  -801,     0,     0,     0,   999,     0,     0,  1000,
     419,   420,     0,     0,  1001,     0,  1002,     0,     0,   418,
       0,     0,     0,     0,     0,     0,  1003,   421,     0,  1004,
    1005,     0,  1006,     0,     0,     0,  1007,     0,     0,     0,
    -801,  1008,  1009,  1010,  1011,     0,     0,     0,     0,     0,
       0,     0,     0,   422,     0,     0,  1259,     0,   421,     0,
       0,   423,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   424,     0,     0,     0,     0,     0,   425,     0,
     -58,     0,     0,  1305,   422,     0,     0,     0,     0,     0,
     426,     0,   423,     0,   -58,     0,     0,     0,     0,     0,
     421,     0,     0,   424,     0,     0,     0,     0,     0,   425,
       0,   -66,     0,     0,     0,     0,     0,     0,     0,   -58,
       0,   426,     0,  1012,  1013,   -66,   422,  1014,     0,     0,
     -58,     0,     0,  1015,   423,     0,     0,     0,  -803,     0,
       0,     0,     0,     0,     0,   424,  1308,     0,     0,     0,
     -66,   425,  -803,  -296,     0,     0,     0,     0,     0,     0,
       0,   -66,     0,   426,     0,     0,     0,  -296,     0,     0,
       0,     0,     0,     0,  1016,  1017,     0,  -803,     0,     0,
       0,     0,     0,     0,     0,     0,  1018,     0,  -803,     0,
     -58,     0,  -296,     0,   -58,     0,   -58,     0,     0,   -58,
       0,   -58,     0,  -296,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1019,  -801,     0,     0,     0,
       0,   -66,     0,     0,     0,   -66,     0,   -66,     0,     0,
     -66,     0,   -66,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -803,     0,
       0,     0,  -803,     0,  -803,     0,     0,  -803,   999,  -803,
       0,  1000,     0,  -296,     0,     0,  1001,  -296,  1002,  -296,
       0,     0,  -296,     0,  -296,     0,     0,     0,  1003,     0,
       0,  1004,  1005,     0,  1006,     0,     0,     0,  1007,     0,
       0,     0,  1031,  1008,  1009,  1010,  1011,     0,  1029,   381,
     382,     0,   383,   384,     0,   385,   386,   387,   388,   389,
     390,  1032,     0,     0,     0,     0,   391,   392,   393,   394,
     395,     0,   396,   397,   398,     0,   381,   382,     0,   383,
     384,     0,   385,   386,   387,   388,   389,   390,     0,     0,
       0,  1033,  1034,   391,   392,   393,   394,   395,     0,   396,
     397,   398,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1035,     0,     0,
       0,     0,     0,     0,     0,  1012,  1013,     0,     0,  1014,
       0,  1036,     0,     0,     0,  1015,     0,     0,     0,   381,
     382,     0,   383,   384,     0,   385,   386,   387,   388,   389,
     390,     0,     0,     0,  1037,     0,   391,   392,   393,   394,
     395,     0,   396,   397,   398,     0,   461,     0,     0,     0,
       0,  1038,     0,  1039,     0,  1040,  1016,  1017,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1049,  1018,     0,
    -548,     0,  1050,     0,     0,     0,  1041,     0,     0,     0,
       0,  -548,  -548,  -548,  -548,  1042,  1051,  1052,  1053,  1054,
       0,     0,  1029,     0,  -548,  1043,     0,  1019,   462,     0,
       0,     0,     0,     0,     0,  1055,   463,     0,   971,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     381,   382,   464,   383,   384,     0,   385,   386,   387,   388,
     389,   390,  -548,     0,     0,  1056,  1057,   391,   392,   393,
     394,   395,     0,   396,   397,   398,   465,     0,     0,     0,
       0,     0,     0,     0,     0,  1044,     0,     0,  1045,     0,
       0,  1058,     0,     0,     0,     0,     0,     0,     0,  1059,
       0,     0,     0,     0,     0,  1060,     0,     0,     0,   466,
       0,     0,     0,     0,     0,     0,   467,     0,     0,     0,
       0,     0,   468,     0,  1115,     0,     0,     0,  1061,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1062,     0,  1063,     0,  1064,
    1065,  1066,     0,     0,     0,     0,     0,   381,   382,     0,
     383,   384,  1067,   385,   386,   387,   388,   389,   390,     0,
    1068,     0,     0,     0,   391,   392,   393,   394,   395,  1069,
     396,   397,   398,     0,     0,     0,     0,   469,     0,  1070,
       0,  1135,     0,     0,     0,     0,   470,     0,     0,   808,
       0,     0,     0,  1071,  1072,   381,   382,     0,   383,   384,
       0,   385,   386,   387,   388,   389,   390,     0,     0,     0,
     471,   472,   391,   392,   393,   394,   395,   473,   396,   397,
     398,  1136,     0,     0,     0,     0,     0,     0,  -548,   808,
       0,     0,     0,     0,     0,   381,   382,     0,   383,   384,
       0,   385,   386,   387,   388,   389,   390,   380,     0,     0,
       0,     0,   391,   392,   393,   394,   395,     0,   396,   397,
     398,   381,   382,     0,   383,   384,     0,   385,   386,   387,
     388,   389,   390,   918,     0,     0,     0,     0,   391,   392,
     393,   394,   395,     0,   396,   397,   398,   381,   382,     0,
     383,   384,     0,   385,   386,   387,   388,   389,   390,  1077,
       0,     0,     0,     0,   391,   392,   393,   394,   395,     0,
     396,   397,   398,   381,   382,     0,   383,   384,     0,   385,
     386,   387,   388,   389,   390,  1081,     0,     0,     0,     0,
     391,   392,   393,   394,   395,     0,   396,   397,   398,   381,
     382,     0,   383,   384,     0,   385,   386,   387,   388,   389,
     390,  1085,     0,     0,     0,     0,   391,   392,   393,   394,
     395,     0,   396,   397,   398,   381,   382,     0,   383,   384,
       0,   385,   386,   387,   388,   389,   390,  1092,     0,     0,
       0,     0,   391,   392,   393,   394,   395,     0,   396,   397,
     398,   381,   382,     0,   383,   384,     0,   385,   386,   387,
     388,   389,   390,  1134,     0,     0,     0,     0,   391,   392,
     393,   394,   395,     0,   396,   397,   398,   381,   382,     0,
     383,   384,     0,   385,   386,   387,   388,   389,   390,  1142,
       0,     0,     0,     0,   391,   392,   393,   394,   395,     0,
     396,   397,   398,   381,   382,     0,   383,   384,     0,   385,
     386,   387,   388,   389,   390,  1153,     0,     0,     0,     0,
     391,   392,   393,   394,   395,     0,   396,   397,   398,   381,
     382,     0,   383,   384,     0,   385,   386,   387,   388,   389,
     390,  1154,     0,     0,     0,     0,   391,   392,   393,   394,
     395,     0,   396,   397,   398,   381,   382,     0,   383,   384,
       0,   385,   386,   387,   388,   389,   390,  1155,     0,     0,
       0,     0,   391,   392,   393,   394,   395,     0,   396,   397,
     398,   381,   382,     0,   383,   384,     0,   385,   386,   387,
     388,   389,   390,  1270,     0,     0,     0,     0,   391,   392,
     393,   394,   395,     0,   396,   397,   398,   381,   382,     0,
     383,   384,     0,   385,   386,   387,   388,   389,   390,     0,
       0,     0,     0,  1088,   391,   392,   393,   394,   395,     0,
     396,   397,   398,   381,   382,     0,   383,   384,     0,   385,
     386,   387,   388,   389,   390,     0,     0,     0,     0,  1098,
     391,   392,   393,   394,   395,     0,   396,   397,   398,   381,
     382,     0,   383,   384,     0,   385,   386,   387,   388,   389,
     390,     0,     0,     0,     0,  1163,   391,   392,   393,   394,
     395,     0,   396,   397,   398,   381,   382,     0,   383,   384,
       0,   385,   386,   387,   388,   389,   390,     0,     0,     0,
       0,  1206,   391,   392,   393,   394,   395,     0,   396,   397,
     398,   381,   382,     0,   383,   384,     0,   385,   386,   387,
     388,   389,   390,     0,     0,     0,     0,  1241,   391,   392,
     393,   394,   395,     0,   396,   397,   398,   381,   382,     0,
     383,   384,     0,   385,   386,   387,   388,   389,   390,     0,
       0,     0,     0,     0,   391,   392,   393,   394,   395,   808,
     396,   397,   398,     0,     0,   381,   382,     0,   383,   384,
       0,   385,   386,   387,   388,   389,   390,     0,     0,     0,
       0,     0,   391,   392,   393,   394,   395,  1076,   396,   397,
     398,     0,     0,   381,   382,     0,   383,   384,     0,   385,
     386,   387,   388,   389,   390,     0,     0,     0,     0,     0,
     391,   392,   393,   394,   395,  1093,   396,   397,   398,     0,
       0,   381,   382,     0,   383,   384,     0,   385,   386,   387,
     388,   389,   390,     0,     0,     0,     0,     0,   391,   392,
     393,   394,   395,  1094,   396,   397,   398,     0,     0,   381,
     382,     0,   383,   384,     0,   385,   386,   387,   388,   389,
     390,     0,     0,     0,     0,     0,   391,   392,   393,   394,
     395,  1157,   396,   397,   398,     0,     0,   381,   382,     0,
     383,   384,     0,   385,   386,   387,   388,   389,   390,     0,
       0,     0,     0,     0,   391,   392,   393,   394,   395,  1266,
     396,   397,   398,     0,     0,   381,   382,     0,   383,   384,
       0,   385,   386,   387,   388,   389,   390,     0,     0,     0,
       0,     0,   391,   392,   393,   394,   395,  1267,   396,   397,
     398,     0,     0,   381,   382,     0,   383,   384,     0,   385,
     386,   387,   388,   389,   390,     0,     0,     0,     0,     0,
     391,   392,   393,   394,   395,  1273,   396,   397,   398,     0,
       0,   381,   382,     0,   383,   384,     0,   385,   386,   387,
     388,   389,   390,     0,     0,     0,     0,     0,   391,   392,
     393,   394,   395,  1274,   396,   397,   398,     0,     0,   381,
     382,     0,   383,   384,     0,   385,   386,   387,   388,   389,
     390,     0,     0,     0,     0,     0,   391,   392,   393,   394,
     395,  1275,   396,   397,   398,     0,     0,   381,   382,     0,
     383,   384,     0,   385,   386,   387,   388,   389,   390,     0,
       0,     0,     0,     0,   391,   392,   393,   394,   395,  1277,
     396,   397,   398,     0,     0,   381,   382,     0,   383,   384,
       0,   385,   386,   387,   388,   389,   390,     0,     0,     0,
       0,     0,   391,   392,   393,   394,   395,  1294,   396,   397,
     398,     0,     0,   381,   382,     0,   383,   384,     0,   385,
     386,   387,   388,   389,   390,     0,     0,     0,     0,     0,
     391,   392,   393,   394,   395,  1295,   396,   397,   398,     0,
       0,   381,   382,     0,   383,   384,     0,   385,   386,   387,
     388,   389,   390,     0,     0,     0,     0,     0,   391,   392,
     393,   394,   395,  1296,   396,   397,   398,     0,     0,   381,
     382,     0,   383,   384,     0,   385,   386,   387,   388,   389,
     390,     0,     0,     0,     0,     0,   391,   392,   393,   394,
     395,  1316,   396,   397,   398,     0,     0,   381,   382,     0,
     383,   384,     0,   385,   386,   387,   388,   389,   390,     0,
       0,     0,     0,     0,   391,   392,   393,   394,   395,  1324,
     396,   397,   398,     0,     0,   381,   382,     0,   383,   384,
       0,   385,   386,   387,   388,   389,   390,     0,     0,     0,
       0,     0,   391,   392,   393,   394,   395,     0,   396,   397,
     398,   381,   382,     0,   383,   384,     0,   385,   386,   387,
     388,   389,   390,     0,     0,     0,     0,     0,   391,   392,
     393,   394,   395,     0,   396,   397,   398,     0,  1104,   381,
     382,  1205,   383,   384,     0,   385,   386,   387,   388,   389,
     390,     0,     0,     0,     0,     0,   391,   392,   393,   394,
     395,     0,   396,   397,   398,   381,   382,     0,   383,   384,
       0,   385,   386,   387,   388,   389,   390,     0,     0,     0,
       0,     0,   391,   392,   393,   394,   395,     0,   396,   397,
     398,   381,   382,     0,   383,   384,     0,   385,   386,   387,
     388,   389,   390,     0,     0,     0,     0,     0,   391,   392,
    1159,   394,   395,     0,   396,   397,   398
};

static const short int yycheck[] =
{
       4,   120,     1,     1,   122,   120,   128,   191,   130,   340,
       1,     1,   294,   344,   296,     1,   117,   299,   300,   320,
     321,    25,     1,     1,   500,   501,   148,     1,   504,     1,
      12,     1,     1,     0,   156,    64,     1,   766,     1,     1,
       1,    64,    64,     1,   228,    27,  1228,    51,    64,   164,
      12,   161,     5,     1,     1,     1,     1,    97,     1,   243,
       1,    12,     1,     1,    12,     1,     0,     1,   190,    27,
       1,     0,   174,     1,     1,    84,    85,     1,    29,     1,
       5,   296,   295,   174,   174,   298,     1,   903,   904,   905,
     624,    34,   626,   174,  1276,     3,     4,     1,     1,     1,
      58,   105,     1,   107,   108,   109,   110,     0,     5,   113,
       1,     0,   300,     1,   329,     1,     1,   239,    77,     1,
     304,   294,   306,   307,   300,   296,     1,   296,     1,     1,
     232,     1,    97,   137,     1,     1,   294,     1,   294,     1,
       1,     1,   232,   296,   292,   294,     9,   151,   188,   175,
      77,   155,     5,   157,   174,   324,   325,   174,     1,   232,
     329,   174,   166,     1,   117,   169,   306,   293,   172,   173,
     186,   175,   176,   293,   178,   174,   293,   332,   182,   235,
     138,   294,    97,   174,   188,   189,   296,   296,   306,   296,
     194,   195,   117,   197,   300,   199,   200,   201,   293,   138,
      51,   147,   317,     1,   319,   320,   321,   274,   174,   199,
     214,   174,   303,   274,    12,   296,   185,   147,   222,   223,
     117,    77,   303,   188,   175,   101,   230,   318,   232,   174,
     199,   203,   199,   232,   224,   239,   351,   318,    36,    37,
      38,    39,   293,   203,   248,    12,     1,     1,   196,   185,
     208,    49,   199,   299,   296,   224,   299,   224,   199,   263,
     203,   204,    29,   299,   117,   199,   270,   296,   235,   208,
     293,   293,   203,   188,   205,   279,   280,   224,    36,    37,
      38,    39,   225,   224,     1,   247,   290,   301,   235,   300,
     224,    49,   293,   301,   298,   301,   301,   301,   302,   303,
     329,   235,   300,   294,   605,   309,   310,   293,   199,   291,
     314,   301,   303,   301,   304,   301,  1045,   296,   296,   318,
     324,   325,   296,   296,   199,   301,   295,   318,   295,   660,
     300,    64,   299,   224,   301,   304,   301,   304,   342,   300,
     303,   294,   624,   296,   626,   349,   299,   300,   295,   224,
     329,   329,   356,   293,   358,   318,   360,   304,   303,   301,
     301,   295,   300,   304,   665,   299,   319,   320,   321,   294,
     304,   296,   300,   318,   299,   300,   300,   381,   300,   383,
     384,   385,   386,   387,   388,   389,   390,   391,   392,   393,
     394,   395,   396,   397,   398,   399,   300,   294,   300,   296,
     404,   300,   299,   300,   295,     0,     1,   301,   175,   740,
     741,   742,   300,   304,   300,   300,   297,   314,   300,   274,
     295,   174,   319,   320,   321,   274,   708,   300,   300,   304,
     300,   296,   300,   300,   300,     0,   300,   441,   300,   300,
     300,   294,   446,   296,   186,   293,   299,   300,   452,   299,
     454,   455,   456,   305,   297,   756,   353,   354,   175,   463,
     301,   314,   298,   467,   307,   308,   296,   310,   311,    77,
     313,   314,   315,   316,   317,   318,    64,    64,   147,   196,
      64,   324,   325,   326,   327,   328,   490,   330,   331,   332,
     605,   334,    64,   291,    64,    77,   147,    64,    64,   299,
     353,   354,    97,   400,    64,   299,   510,   326,   327,   328,
     514,   330,   331,   332,    64,     1,    64,   521,     0,     1,
     524,    64,    64,   297,   301,   293,    12,   294,   301,   299,
     247,   862,   536,   735,   294,   301,   540,   301,   391,   392,
     174,  1293,   297,    29,  1141,  1276,   926,   400,  1089,   754,
     665,    -1,   307,   308,   558,   310,   311,   858,   313,   314,
     315,   316,   317,   318,     1,    -1,    -1,     0,     1,   324,
     325,   326,   327,   328,    -1,   330,   331,   332,    -1,    -1,
      -1,    -1,    64,    -1,    -1,    -1,   590,    -1,    25,    -1,
     185,    77,    -1,   188,    -1,    -1,   697,     0,     1,    36,
      37,    38,    39,    -1,   199,    -1,     9,     1,    -1,   613,
      -1,    -1,    49,    -1,   618,    -1,     0,     1,    -1,    -1,
     902,    -1,    -1,    -1,    -1,     9,    -1,   631,    -1,   224,
      -1,    25,    -1,   915,   199,   754,    -1,   919,    -1,   754,
     235,   756,    36,    37,    38,    39,    -1,   929,   763,    -1,
      87,    -1,   605,    -1,  1130,    49,    -1,  1133,    -1,   224,
      -1,   147,   296,   945,   668,    19,   670,   104,   672,   303,
     235,   624,  1148,   626,    -1,    -1,    -1,    -1,    -1,   165,
     684,   685,   686,    -1,   318,    -1,   690,    -1,    -1,   175,
      -1,   177,    -1,    87,    -1,    -1,    -1,    51,    -1,   624,
     295,   626,    -1,   185,   299,    -1,   301,   711,   605,   304,
     196,   306,   665,   897,   898,   899,   900,   199,    -1,    -1,
    1021,   725,    -1,   727,   728,    -1,   730,   624,   165,   626,
     295,    -1,    -1,   917,   299,    -1,   301,    -1,    -1,   304,
      -1,    61,   224,   858,   697,    -1,    -1,    -1,  1030,    -1,
    1081,     1,  1083,   235,  1230,   708,    -1,    -1,    -1,   245,
     246,    -1,    -1,    -1,    -1,    -1,   199,    -1,   665,    -1,
      -1,   624,   697,   626,    -1,    25,    -1,    -1,   132,    29,
      -1,   678,   185,   708,    -1,    -1,    36,    37,    38,    39,
      -1,   224,    -1,    -1,   114,    -1,   199,    -1,   118,    49,
     697,   185,   235,   756,   158,    -1,    -1,    -1,   812,    -1,
      -1,   708,   166,   295,   296,   199,    -1,   299,   138,   301,
      -1,   224,   304,   177,   306,   678,   263,    77,    -1,   183,
      -1,    -1,   235,    -1,    -1,    -1,    -1,    87,   735,    12,
     224,   195,   324,   325,   697,   849,    -1,   329,   852,    -1,
      -1,   235,    -1,   857,   291,   708,    -1,    -1,    -1,   756,
      -1,    -1,   295,  1145,    -1,   987,   299,    -1,    -1,   263,
     874,   304,    -1,    -1,   727,   879,   880,   881,    -1,   997,
     981,    -1,   735,    -1,   888,    -1,   890,    -1,    61,   990,
      -1,    -1,   295,    -1,    -1,    -1,   299,    -1,   301,     1,
       1,   304,    -1,   306,    77,   858,  1021,    -1,    -1,    -1,
      -1,   295,    -1,    -1,    -1,   299,   236,   301,     0,     1,
     304,    -1,   306,    -1,    25,   245,  1044,    -1,    -1,    -1,
    1048,    -1,    34,     1,    -1,    36,    37,    38,    39,    -1,
      19,   114,    -1,    -1,   948,   118,    -1,    -1,    49,   902,
      -1,    -1,    -1,    -1,    -1,     1,    58,    25,    -1,    -1,
      -1,   858,   915,    -1,    -1,   138,   919,    -1,    36,    37,
      38,    39,    51,     1,   147,   979,   929,   902,    -1,    -1,
     984,    49,   986,   987,  1168,    -1,    87,    -1,    34,    -1,
     915,    -1,   945,    -1,   919,    -1,    -1,    25,    -1,    -1,
      -1,    -1,    -1,   104,   929,   902,    -1,    -1,    36,    37,
      38,    39,    58,   263,    60,    -1,    -1,    -1,   915,    87,
     945,    49,   919,    -1,    -1,  1029,    -1,    -1,   981,    -1,
       1,    -1,   929,    -1,    -1,    -1,   104,   990,    -1,    -1,
    1044,    12,    -1,    -1,  1048,    -1,    -1,    -1,   945,   902,
    1054,  1169,    -1,   132,    -1,  1059,   981,    -1,    29,    87,
      -1,  1245,   915,   236,   165,   990,   919,    -1,  1021,   171,
     172,   173,   245,  1077,    -1,    -1,   929,  1030,    -1,   158,
      -1,  1085,    -1,    -1,   981,  1089,   165,   166,  1092,    -1,
      -1,    -1,   945,   990,    -1,    -1,  1100,   165,   177,    -1,
    1104,   203,   204,   185,   183,  1030,    77,    -1,    -1,    -1,
      -1,    -1,    -1,     0,     1,    -1,   195,   199,  1122,  1123,
    1124,  1125,    -1,   225,  1021,   171,   172,   173,   981,    -1,
    1134,  1135,  1136,  1030,  1138,    -1,    -1,   990,  1142,  1240,
      -1,    -1,   224,    -1,    -1,    -1,    -1,     0,     1,  1153,
    1154,  1155,    -1,   235,    -1,  1159,    -1,   203,   204,   205,
      -1,    -1,   263,    -1,    -1,    -1,    19,     0,     1,    -1,
    1174,  1175,    -1,    -1,    -1,  1179,   147,  1030,  1182,   225,
    1184,    -1,  1186,    -1,  1188,    -1,    19,  1191,  1192,  1193,
     291,  1044,  1145,  1090,   165,   263,    -1,    -1,    51,    -1,
      -1,    -1,    -1,    -1,   175,    -1,   177,  1211,    -1,    -1,
      -1,    -1,   291,   295,   296,    -1,    -1,   299,    51,   301,
    1145,    -1,   304,   291,   306,   196,    -1,    -1,    -1,    -1,
      -1,    84,    85,    -1,    -1,   263,    -1,  1090,    -1,    -1,
    1244,    -1,  1246,    -1,    -1,  1249,  1250,  1251,  1145,  1253,
      -1,  1255,    -1,  1257,    -1,  1259,    -1,  1261,  1262,  1156,
      -1,    -1,    -1,  1160,  1161,    -1,    -1,  1271,    -1,    -1,
      -1,    -1,    -1,    -1,   245,   246,    -1,    -1,    -1,   132,
      -1,    -1,    -1,   128,    -1,   130,    -1,  1240,    -1,    -1,
     135,    -1,  1145,     0,     1,    -1,  1300,    -1,   185,   132,
      -1,  1305,    -1,  1156,  1308,   158,  1159,  1160,  1161,   154,
      -1,   156,   199,   166,    -1,  1240,   161,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   177,   158,   171,    -1,    -1,   174,
     183,    -1,   185,   166,    -1,    -1,    -1,   224,   183,   184,
      -1,     1,   195,  1240,   177,   190,   199,    -1,   235,    -1,
     183,    -1,   185,    -1,    -1,    -1,    -1,   202,   203,   204,
     205,    -1,   195,    -1,    -1,    25,   199,    -1,   213,    -1,
      -1,   224,    -1,    -1,    -1,    -1,    36,    37,    38,    39,
      -1,    25,   235,    -1,   229,    -1,   231,  1240,    -1,    49,
     235,   224,    36,    37,    38,    39,    -1,    -1,    -1,    -1,
    1297,     1,   235,    -1,    -1,    49,    -1,    -1,   295,    -1,
      -1,    -1,   299,   300,   301,    -1,    -1,   304,    -1,   306,
      -1,   266,    -1,    -1,    -1,    25,    -1,    87,    -1,    -1,
     275,    -1,    -1,    -1,    -1,    -1,    36,    37,    38,    39,
      -1,    -1,   295,    87,  1297,    -1,   299,    -1,   301,    49,
      -1,   304,    -1,   306,    -1,    -1,   301,    -1,    -1,    -1,
      -1,    -1,   295,    -1,   309,   310,   299,    -1,   301,   314,
      -1,   304,    -1,   306,    -1,    -1,    -1,    -1,   185,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,   334,
      -1,    -1,   199,    -1,    -1,    -1,    -1,     0,     1,    -1,
      -1,    -1,   347,    -1,   104,     8,     9,    10,    11,    12,
      -1,    14,    15,    -1,    17,    18,    -1,   224,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,   235,    32,
      -1,    34,    -1,    36,    37,    38,    39,    -1,    -1,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    -1,    -1,
      -1,    54,    55,    -1,    -1,    58,    59,    60,    61,    62,
      -1,    64,    -1,    66,    -1,   165,    -1,    -1,    -1,    -1,
      -1,    -1,    75,    76,    77,    -1,    -1,   422,     1,   324,
     325,   326,   327,   328,    87,   330,   331,   332,   295,    -1,
      -1,    -1,   299,    96,   301,    -1,    99,   304,    -1,   306,
      -1,   104,    25,   263,   107,   108,    29,    -1,    -1,    -1,
      -1,    -1,    -1,    36,    37,    38,    39,    -1,    -1,   263,
      -1,   124,   125,   126,   127,   128,    49,    -1,   131,    -1,
     133,    -1,    -1,    -1,    -1,   138,    -1,    -1,   141,   142,
      -1,   144,   145,    -1,   147,    -1,    -1,    -1,   151,   152,
      -1,    -1,    -1,    -1,    77,    -1,    -1,    -1,    -1,    -1,
     163,    -1,   165,   263,    87,    -1,    -1,   170,     0,     1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   185,   186,   187,    -1,   189,     1,   191,   192,
     193,   291,    -1,   196,    -1,    -1,   199,   200,   201,   202,
      -1,   204,    -1,    -1,   207,    -1,    -1,    -1,    -1,   212,
      -1,    25,    -1,   216,   217,    -1,    -1,    -1,   221,    -1,
      -1,   224,    36,    37,    38,    39,    -1,   230,   231,    -1,
     233,   576,   235,   236,    -1,    49,   239,    -1,    -1,    -1,
      -1,    -1,   245,   246,    -1,    -1,    -1,    61,    -1,    -1,
     253,    -1,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,
     263,    -1,    -1,    -1,   267,    -1,    -1,    -1,   613,    -1,
     273,   274,    -1,    87,   277,    -1,    -1,    -1,    -1,    -1,
      -1,   284,   627,    -1,   629,    -1,   289,   290,   291,    -1,
     293,   294,   295,   296,   297,    -1,   299,   300,   301,   302,
      -1,   304,   305,   306,   307,   308,   309,   310,   311,   312,
     313,   314,   315,   316,   317,   318,   319,   320,   321,   322,
     323,   324,   325,   326,   327,   328,    -1,   330,   331,   332,
     333,   334,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     263,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     0,     1,   185,    -1,   700,   701,    -1,   703,     8,
       9,    10,    11,    12,    -1,    14,    15,   199,    17,    18,
      -1,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    -1,    32,     1,    34,    -1,    36,    37,    38,
      39,    -1,   224,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    19,   235,    -1,    54,    55,    -1,    -1,    58,
      59,    60,    61,    62,    -1,    64,    -1,    66,    -1,    -1,
      -1,    -1,    -1,     1,    -1,    -1,    75,    76,    77,    -1,
      -1,   245,    -1,    -1,    51,    -1,    -1,    -1,    87,    -1,
      -1,    19,    -1,    -1,    -1,    -1,    -1,    96,    -1,   263,
      99,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,
      -1,    -1,    -1,   295,    -1,    -1,   280,   299,    -1,   301,
      -1,    -1,   304,    51,   306,   124,   125,   126,   127,   128,
      -1,    -1,   131,    -1,   133,    -1,    -1,    -1,    -1,   138,
      -1,    -1,   141,   142,    -1,   144,   145,    -1,   147,    -1,
      -1,    -1,   151,   152,     0,     1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   132,   165,    -1,    -1,    -1,
      -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   185,   186,   187,    -1,
     189,   158,   191,   192,   193,    -1,    -1,   196,    -1,   166,
     199,   200,   201,   202,   132,   204,    -1,   174,   207,    -1,
     177,    -1,    -1,   212,    -1,    -1,   183,   216,   217,    -1,
      -1,    -1,   221,   918,    -1,   224,    -1,    -1,   195,    -1,
     158,   230,   231,    -1,   233,    -1,   235,   236,   166,    -1,
     239,    -1,    -1,    -1,    -1,    -1,   245,   246,    -1,   177,
      -1,    -1,    -1,    -1,   253,   183,    -1,    -1,    -1,    -1,
     259,   260,   261,    -1,   263,    -1,    -1,   195,   267,    -1,
      -1,    -1,    -1,    -1,   273,   274,    -1,    -1,   277,    -1,
      -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,
     289,   290,   291,    -1,   293,   294,   295,   296,   297,    -1,
     299,   300,   301,   302,    -1,   304,   305,   306,   307,   308,
     309,   310,   311,   312,   313,   314,   315,   316,   317,   318,
     319,   320,   321,   322,   323,   324,   325,   326,   327,   328,
      -1,   330,   331,   332,   333,   334,    -1,     0,     1,   185,
      -1,    -1,    -1,    -1,    -1,     8,     9,    10,    11,    12,
      -1,    14,    15,   199,    17,    18,    -1,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    -1,    32,
      -1,    34,    -1,    36,    37,    38,    39,    -1,   224,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    -1,   235,
      -1,    54,    55,    -1,    -1,    58,    59,    60,    61,    62,
      -1,    64,    -1,    66,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    -1,    75,    76,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    87,    -1,    -1,    19,    -1,    -1,
      -1,    -1,    -1,    96,    -1,    -1,    99,    -1,    -1,    -1,
      -1,   104,    -1,    -1,   107,   108,    -1,    -1,    -1,   295,
      -1,    -1,    -1,   299,    -1,   301,    -1,    -1,   304,    51,
     306,   124,   125,   126,   127,   128,    -1,    -1,   131,    -1,
     133,    -1,    -1,    -1,    -1,   138,    -1,     1,   141,   142,
      -1,   144,   145,    -1,   147,    25,    -1,    -1,   151,   152,
      -1,    -1,    -1,    -1,    -1,    -1,    36,    37,    38,    39,
     163,    25,   165,    -1,    -1,    -1,    -1,   170,    -1,    49,
      -1,    -1,    36,    37,    38,    39,    -1,    -1,    -1,    -1,
      -1,    -1,   185,   186,   187,    49,   189,    -1,   191,   192,
     193,    -1,    -1,   196,    -1,    -1,   199,   200,   201,   202,
     132,   204,    -1,    -1,   207,    -1,    -1,    87,    -1,   212,
      -1,    -1,    -1,   216,   217,    -1,    -1,    -1,   221,    -1,
      -1,   224,    -1,    87,   104,    -1,   158,   230,   231,    -1,
     233,    -1,   235,   236,   166,  1270,   239,    -1,    -1,    -1,
      -1,    -1,   245,   246,    -1,   177,    -1,    -1,    -1,    -1,
     253,   183,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,
     263,    -1,    -1,   195,   267,    -1,    -1,    -1,    -1,    -1,
     273,   274,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,
      -1,   284,    -1,    -1,    -1,   165,   289,   290,   291,    -1,
     293,   294,   295,    -1,   297,    -1,   299,   300,   301,   302,
      -1,   304,   305,   306,   307,   308,   309,   310,   311,   312,
     313,   314,   315,   316,   317,   318,   319,   320,   321,   322,
     323,   324,   325,   326,   327,   328,    -1,   330,   331,   332,
     333,   334,     0,     1,    -1,    -1,    -1,    -1,    -1,    -1,
       8,     9,    10,    11,    12,    -1,    14,    15,    -1,    17,
      18,    -1,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    -1,    32,    -1,    34,    -1,    36,    37,
      38,    39,    -1,    -1,    42,    43,    44,    45,    46,    47,
      48,    49,    50,   263,    -1,    -1,     1,    55,    -1,    -1,
      58,    59,    60,    -1,    62,    -1,    -1,    -1,    66,   263,
      -1,    -1,    -1,    -1,    -1,    -1,     1,    75,    76,    77,
      25,   291,    -1,    -1,    -1,    -1,    84,    85,    -1,    87,
      -1,    36,    37,    38,    39,    -1,    -1,    -1,    96,    -1,
      25,    99,    -1,    -1,    49,    -1,   104,    -1,    -1,   107,
     108,    36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,     1,    49,    -1,   124,   125,   126,   127,
     128,    -1,    -1,   131,    -1,   133,    -1,    -1,    -1,    -1,
     138,    19,    87,    -1,   142,    25,   144,   145,    -1,   147,
      -1,    -1,    -1,   151,   152,    -1,    36,    37,    38,    39,
      -1,    -1,    87,    -1,    -1,   163,    -1,   165,    -1,    49,
      -1,    -1,   170,    51,     0,     1,   174,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   185,    -1,   187,
      -1,   189,     1,   191,   192,   193,    -1,    -1,   196,    -1,
      -1,   199,   200,   201,   202,    -1,   204,    87,    -1,   207,
      -1,    -1,    -1,    -1,    -1,    -1,    25,    -1,   216,   217,
      -1,    -1,    -1,    -1,   104,    -1,   224,    36,    37,    38,
      39,    -1,   230,   231,    -1,   233,    -1,   235,   236,    -1,
      49,   239,    -1,    -1,    -1,    -1,    -1,   245,   246,    -1,
      -1,    -1,    -1,    -1,   132,   253,    -1,    -1,    -1,    -1,
      -1,   259,   260,   261,    -1,   263,    -1,    -1,    -1,   267,
      -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,    87,   277,
     158,    -1,    -1,    -1,    -1,   165,   284,    -1,   166,     0,
       1,   289,   290,   291,    -1,    -1,    -1,   295,    -1,   177,
      -1,   299,   300,   301,   302,   183,   304,    -1,   306,    -1,
      -1,    -1,    -1,    -1,   312,    -1,    -1,   195,   263,     0,
       1,   319,   320,   321,   322,   323,    -1,     8,     9,    10,
      11,    12,    -1,    14,    15,   333,    17,    18,   263,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      -1,    32,     1,    34,    -1,    36,    37,    38,    39,   185,
      -1,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      19,    -1,    -1,   199,    55,    -1,    -1,    58,    59,    60,
      -1,    62,    -1,   263,    -1,    66,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    75,    76,    77,    -1,   224,    -1,
      -1,    -1,    51,    -1,    -1,    -1,    87,    -1,    -1,   235,
      -1,   291,    -1,    -1,    -1,    96,    -1,    25,    99,    -1,
      -1,    -1,    -1,   104,    -1,    -1,   107,   108,    36,    37,
      38,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       1,    49,    -1,   124,   125,   126,   127,   128,    -1,    -1,
     131,    -1,   133,    -1,   263,    -1,    -1,   138,    19,    -1,
      -1,   142,    -1,   144,   145,    -1,   147,    -1,    -1,   295,
     151,   152,    -1,   299,   185,   301,    -1,    -1,   304,    87,
     306,    -1,   163,   132,   165,    -1,    -1,    -1,   199,   170,
      51,     0,     1,   174,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   185,    -1,   187,    -1,   189,   158,
     191,   192,   193,   224,    -1,   196,    -1,   166,   199,   200,
     201,   202,    -1,   204,   235,    -1,   207,    -1,   177,    -1,
      -1,    -1,    -1,    -1,   183,   216,   217,    -1,    -1,    -1,
      -1,    -1,    -1,   224,    -1,    -1,   195,    -1,    -1,   230,
     231,   232,   233,    -1,   235,   236,    -1,    -1,   239,    -1,
      -1,    -1,    -1,    -1,   245,   246,    -1,    -1,    -1,    -1,
      -1,   132,   253,    -1,    -1,    -1,    -1,    -1,   259,   260,
     261,    -1,   263,    -1,   295,    -1,   267,    -1,   299,    -1,
     301,    -1,   273,   304,    -1,   306,   277,   158,    -1,    -1,
      -1,    -1,    -1,   284,    -1,   166,     0,     1,   289,   290,
     291,    -1,    -1,    -1,   295,    -1,   177,    -1,   299,   300,
     301,   302,   183,   304,    -1,   306,    -1,    -1,    -1,    -1,
      -1,   312,    -1,    -1,   195,    -1,     0,     1,   319,   320,
     321,   322,   323,    -1,     8,     9,    10,    11,    12,    -1,
      14,    15,   333,    17,    18,   263,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    -1,    32,    -1,
      34,    -1,    36,    37,    38,    39,   185,    -1,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    -1,    -1,    -1,
     199,    55,    -1,    -1,    58,    59,    60,    -1,    62,    -1,
      -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       1,    75,    76,    77,    -1,   224,    -1,    -1,    -1,    -1,
      84,    85,    -1,    87,    -1,    -1,   235,    -1,    -1,    -1,
      -1,    -1,    96,    -1,    25,    99,    -1,    -1,    -1,    -1,
     104,    -1,    -1,   107,   108,    36,    37,    38,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    49,    -1,
     124,   125,   126,   127,   128,    -1,    -1,   131,    -1,   133,
      -1,    -1,    -1,    -1,   138,    19,     1,    -1,   142,    -1,
     144,   145,    -1,   147,    -1,    -1,   295,   151,   152,    -1,
     299,   185,   301,    -1,    -1,   304,    87,   306,    -1,   163,
      25,   165,    -1,    -1,    -1,   199,   170,    51,     0,     1,
      -1,    36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,
       1,   185,    -1,   187,    49,   189,    -1,   191,   192,   193,
     224,    -1,   196,    -1,    -1,   199,   200,   201,   202,    -1,
     204,   235,    -1,   207,    25,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   216,   217,    -1,    36,    37,    38,    39,    -1,
     224,    -1,    87,    -1,    -1,    -1,   230,   231,    49,   233,
      -1,   235,   236,    -1,    -1,   239,    -1,    -1,    -1,    -1,
      -1,   245,   246,    -1,    -1,    -1,    -1,    -1,   132,   253,
      -1,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,   263,
      -1,   295,    -1,   267,    -1,   299,    87,   301,    -1,   273,
     304,    -1,   306,   277,   158,    -1,    -1,    -1,    -1,    -1,
     284,    -1,   166,     0,     1,   289,   290,   291,    -1,    -1,
      -1,   295,    -1,   177,    -1,   299,   300,   301,   302,   183,
     304,    -1,   306,    -1,    -1,    -1,    -1,    -1,   312,    -1,
      -1,   195,    -1,     0,     1,   319,   320,   321,   322,   323,
      -1,     8,     9,    10,    11,    12,    13,    14,    15,   333,
      17,    18,   263,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    -1,    32,    -1,    34,    -1,    36,
      37,    38,    39,   185,    -1,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    -1,    -1,    -1,   199,    55,    -1,
      -1,    58,    59,    60,    -1,    62,    -1,    -1,    -1,    66,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    75,    76,
      77,    -1,   224,    -1,    -1,    -1,    -1,    -1,   263,    -1,
      87,    -1,    -1,   235,    -1,    -1,    -1,    -1,    -1,    96,
      -1,    25,    99,    -1,    -1,    -1,    -1,   104,    -1,    -1,
     107,   108,    36,    37,    38,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    49,    -1,   124,   125,   126,
     127,   128,   263,    -1,   131,    -1,   133,    -1,    -1,    -1,
      -1,   138,    19,     1,    -1,   142,    -1,   144,   145,    -1,
     147,    -1,    -1,   295,   151,   152,    -1,   299,   185,   301,
      -1,    -1,   304,    87,   306,    -1,   163,    25,   165,    -1,
      -1,    -1,   199,   170,    51,     0,     1,    -1,    36,    37,
      38,    39,    -1,    -1,    -1,    -1,    -1,     1,   185,    -1,
     187,    49,   189,    -1,   191,   192,   193,   224,    -1,   196,
      -1,    -1,   199,   200,   201,   202,    -1,   204,   235,    -1,
     207,    25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,
     217,    -1,    36,    37,    38,    39,    -1,   224,    -1,    87,
      -1,    -1,    -1,   230,   231,    49,   233,    -1,   235,   236,
      -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,   245,   246,
      -1,    -1,    -1,    -1,    -1,   132,   253,    -1,    -1,    -1,
      -1,    -1,   259,   260,   261,    -1,   263,    -1,   295,    -1,
     267,    -1,   299,    87,   301,    -1,   273,   304,    -1,   306,
     277,   158,    -1,    -1,    -1,    -1,    -1,   284,    -1,   166,
       0,     1,   289,   290,   291,    -1,    -1,    -1,   295,    -1,
     177,    -1,   299,   300,   301,   302,   183,   304,    -1,   306,
      -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,   195,    -1,
       0,     1,   319,   320,   321,   322,   323,    -1,     8,     9,
      10,    11,    12,    13,    14,    15,   333,    17,    18,   263,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    -1,    32,    -1,    34,    -1,    36,    37,    38,    39,
     185,    -1,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    -1,    -1,    -1,   199,    55,    -1,    -1,    58,    59,
      60,    -1,    62,    -1,    -1,    -1,    66,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    75,    76,    77,    -1,   224,
      -1,    -1,    -1,    -1,    -1,   263,    -1,    87,    -1,    -1,
     235,    -1,    -1,    -1,    -1,    -1,    96,    -1,    25,    99,
      -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,    36,
      37,    38,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    49,    -1,   124,   125,   126,   127,   128,   263,
      -1,   131,    -1,   133,    -1,    -1,    -1,    -1,   138,    19,
       1,    -1,   142,    -1,   144,   145,    -1,   147,    -1,    -1,
     295,   151,   152,    -1,   299,   185,   301,    -1,    -1,   304,
      87,   306,    -1,   163,    25,   165,    -1,    -1,    -1,   199,
     170,    51,     0,     1,    -1,    36,    37,    38,    39,    -1,
      -1,    -1,    -1,    -1,     1,   185,    -1,   187,    49,   189,
      -1,   191,   192,   193,   224,    -1,   196,    -1,    -1,   199,
     200,   201,   202,    -1,   204,   235,    -1,   207,    25,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   216,   217,    -1,    36,
      37,    38,    39,    -1,   224,    -1,    87,    -1,    -1,    -1,
     230,   231,    49,   233,    -1,   235,   236,    -1,    -1,   239,
      -1,    -1,    -1,    -1,    -1,   245,   246,    -1,    -1,    -1,
      -1,    -1,   132,   253,    -1,    -1,    -1,     1,    -1,   259,
     260,   261,    -1,   263,    -1,   295,    -1,   267,    -1,   299,
      87,   301,    -1,   273,   304,    19,   306,   277,   158,    -1,
      -1,    -1,    -1,    -1,   284,    -1,   166,    -1,    -1,   289,
     290,   291,    -1,    -1,    -1,   295,    -1,   177,    -1,   299,
     300,   301,   302,   183,   304,    -1,   306,    51,    -1,    -1,
      -1,    -1,   312,    -1,    -1,   195,    -1,     0,     1,   319,
     320,   321,   322,   323,    -1,     8,     9,    10,    11,    12,
      13,    14,    15,   333,    17,    18,   263,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    -1,    32,
       1,    34,    -1,    36,    37,    38,    39,   185,    -1,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    19,    -1,
      -1,   199,    55,    -1,    -1,    58,    59,    60,    -1,    62,
      -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    75,    76,    77,    -1,   224,    -1,    -1,    -1,
      51,    -1,   263,    -1,    87,    -1,    -1,   235,    -1,    -1,
      -1,    -1,    -1,    96,   158,    -1,    99,    -1,    -1,    -1,
      12,   104,   166,    -1,   107,   108,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   177,    -1,    -1,    -1,    29,     1,   183,
      -1,   124,   125,   126,   127,   128,   263,    -1,   131,    -1,
     133,   195,    -1,    -1,    -1,   138,    19,    -1,    -1,   142,
      -1,   144,   145,    -1,   147,    -1,    -1,   295,   151,   152,
      -1,   299,    -1,   301,    -1,    -1,   304,    -1,   306,    -1,
     163,   132,   165,    -1,    -1,    77,    -1,   170,    51,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   185,    -1,   187,    -1,   189,   158,   191,   192,
     193,    -1,    -1,   196,    -1,   166,   199,   200,   201,   202,
      -1,   204,    -1,    -1,   207,    -1,   177,    -1,    -1,    -1,
      -1,    -1,   183,   216,   217,    -1,    -1,    -1,    -1,    -1,
      -1,   224,    -1,    -1,   195,    -1,    -1,   230,   231,    -1,
     233,    -1,   235,   236,    -1,   147,   239,    -1,    -1,    -1,
      -1,    -1,   245,   246,    -1,    -1,    -1,    -1,    -1,   132,
     253,    -1,    -1,   165,    -1,     1,   259,   260,   261,    -1,
     263,    -1,    -1,   175,   267,   177,    -1,    -1,    -1,    -1,
     273,    -1,    -1,    19,   277,   158,    -1,    -1,    -1,    -1,
      -1,   284,    -1,   166,   196,    -1,   289,   290,   291,    -1,
      -1,    -1,   295,    -1,   177,    -1,   299,   300,   301,   302,
     183,   304,    -1,   306,    -1,    51,    -1,    -1,    -1,   312,
      -1,    -1,   195,    -1,     0,     1,   319,   320,   321,   322,
     323,    -1,     8,     9,    10,    11,    12,    -1,    14,    15,
     333,    17,    18,   245,   246,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    -1,    32,     1,    34,    -1,
      36,    37,    38,    39,    -1,    -1,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    19,    -1,    -1,    -1,    55,
      -1,    -1,    58,    59,    60,    -1,    62,    -1,    -1,    -1,
      66,    -1,    -1,    -1,    -1,    -1,   132,    -1,     1,    75,
      76,    77,    -1,    -1,    -1,    -1,    -1,    51,    -1,    12,
      -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      96,    -1,   158,    99,    -1,    -1,    29,    -1,   104,    -1,
     166,   107,   108,    36,    37,    38,    39,    -1,    -1,    -1,
      -1,   177,    -1,    -1,    -1,     1,    49,   183,   124,   125,
     126,   127,   128,    -1,    -1,   131,    -1,   133,    61,   195,
      -1,    -1,   138,    19,    -1,    -1,   142,    -1,   144,   145,
      -1,   147,    -1,    -1,    -1,   151,   152,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,   132,   165,
      -1,    -1,    -1,    -1,   170,    51,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   185,
      -1,   187,    -1,   189,   158,   191,   192,   193,    -1,    -1,
     196,    -1,   166,   199,   200,   201,   202,    -1,   204,    -1,
      -1,   207,    -1,   177,    -1,    -1,    -1,    -1,   141,   183,
     216,   217,    -1,    -1,    -1,    -1,    -1,    -1,   224,    -1,
      -1,   195,    -1,    -1,   230,   231,   232,   233,    -1,   235,
     236,    -1,   165,   239,    -1,    -1,    -1,    -1,    -1,   245,
     246,    -1,    -1,    -1,   177,    -1,   132,   253,    -1,    -1,
      -1,     1,     1,   259,   260,   261,    -1,   263,    -1,    -1,
      -1,   267,    -1,    -1,    -1,    -1,    -1,   273,    -1,    19,
      19,   277,   158,    -1,    -1,    -1,    -1,    -1,   284,   212,
     166,    -1,    -1,   289,   290,   291,    -1,    -1,   221,   295,
      -1,   177,    -1,   299,   300,   301,   302,   183,   304,    -1,
     306,    51,    51,    -1,    -1,    -1,   312,    -1,    -1,   195,
      -1,     0,     1,   319,   320,   321,   322,   323,    -1,     8,
       9,    10,    11,    12,    -1,    14,    15,   333,    17,    18,
      -1,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    -1,    32,     1,    34,    -1,    36,    37,    38,
      39,    -1,    -1,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    19,    -1,    -1,    -1,    55,    -1,    -1,    58,
      59,    60,    -1,    62,    -1,    -1,    -1,    66,    -1,    -1,
      -1,    -1,   132,   132,    -1,    -1,    75,    76,    77,    -1,
      -1,    -1,    -1,    -1,    51,    -1,    -1,    -1,    87,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,   158,   158,
      99,    -1,    -1,    -1,    -1,   104,   166,   166,   107,   108,
      -1,    -1,     1,    -1,    -1,    -1,    -1,   177,   177,    -1,
      -1,    -1,    -1,   183,   183,   124,   125,   126,   127,   128,
      19,    -1,   131,    -1,   133,   195,   195,    -1,    -1,   138,
      -1,    -1,    -1,   142,    -1,   144,   145,    -1,   147,    -1,
      -1,    -1,   151,   152,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    51,    -1,   163,   132,   165,    -1,    -1,    -1,
      -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   185,    -1,   187,    -1,
     189,   158,   191,   192,   193,    -1,    -1,   196,    -1,   166,
     199,   200,   201,   202,    -1,   204,    -1,    -1,   207,    -1,
     177,    -1,    -1,    -1,    -1,    -1,   183,   216,   217,    -1,
      -1,    -1,    -1,    -1,    -1,   224,    -1,    -1,   195,    -1,
      -1,   230,   231,    -1,   233,    -1,   235,   236,    -1,    -1,
     239,    -1,    -1,   132,    -1,    -1,   245,   246,    -1,    -1,
      -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,
     259,   260,   261,    -1,   263,    -1,    -1,    -1,   267,   158,
      -1,    -1,    -1,    -1,   273,    -1,    -1,   166,   277,    -1,
      -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,   177,    -1,
     289,   290,   291,    -1,   183,    -1,   295,    -1,    -1,    -1,
     299,   300,   301,   302,    -1,   304,   195,   306,    -1,    -1,
      -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,     0,     1,
     319,   320,   321,   322,   323,    -1,     8,     9,    10,    11,
      12,    -1,    14,    15,   333,    17,    18,    -1,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    -1,
      32,    -1,    34,    -1,    36,    37,    38,    39,    -1,    -1,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    -1,
      -1,    -1,    -1,    55,    -1,    -1,    58,    59,    60,    -1,
      62,    -1,    -1,   311,    66,   313,   314,   315,   316,   317,
     318,    -1,    -1,    75,    76,    77,   324,   325,   326,   327,
     328,    -1,   330,   331,   332,    87,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    96,    -1,    -1,    99,    -1,    -1,
      -1,    -1,   104,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,   126,   127,   128,    -1,    -1,   131,
      -1,   133,    -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,
     142,    -1,   144,   145,    -1,   147,    -1,    -1,    -1,   151,
     152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,    -1,   165,    -1,    -1,    -1,    -1,   170,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   185,    -1,   187,    -1,   189,    -1,   191,
     192,   193,    -1,    -1,   196,    -1,    -1,   199,   200,   201,
     202,   308,   204,   310,   311,   207,   313,   314,   315,   316,
     317,   318,    -1,    -1,   216,   217,    -1,   324,   325,   326,
     327,   328,   224,   330,   331,   332,    -1,    -1,   230,   231,
      -1,   233,    -1,   235,   236,    -1,    -1,   239,    -1,    -1,
      -1,    -1,    -1,   245,   246,    -1,    -1,    -1,    -1,    -1,
      -1,   253,    -1,    -1,    -1,    -1,    -1,   259,   260,   261,
      -1,   263,    -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,
      -1,   273,    -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,
      -1,    -1,   284,    -1,    -1,    -1,    -1,   289,   290,   291,
      -1,    -1,    -1,   295,    -1,    -1,    -1,   299,   300,   301,
     302,    -1,   304,    -1,   306,    -1,    -1,    -1,    -1,    -1,
     312,    -1,    -1,    -1,    -1,     0,     1,   319,   320,   321,
     322,   323,    -1,     8,     9,    10,    11,    12,    -1,    14,
      15,   333,    17,    18,    -1,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    -1,    32,    -1,    34,
      -1,    36,    37,    38,    39,    -1,    -1,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    -1,    -1,    -1,    -1,
      55,    -1,    -1,    58,    59,    60,    -1,    62,    -1,    -1,
      -1,    66,   313,   314,   315,   316,   317,   318,    -1,    -1,
      75,    76,    77,   324,   325,   326,   327,   328,    -1,   330,
     331,   332,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    96,    -1,    -1,    99,    -1,    -1,    -1,    -1,   104,
      -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,   126,   127,   128,    -1,    -1,   131,    -1,   133,    -1,
      -1,    -1,    -1,   138,    -1,    -1,    -1,   142,    -1,   144,
     145,    -1,   147,    -1,    -1,    -1,   151,   152,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,
     165,    -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     185,    -1,   187,    -1,   189,    -1,   191,   192,   193,    -1,
      -1,   196,    -1,    -1,   199,   200,   201,   202,    -1,   204,
      -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   216,   217,    -1,    -1,    -1,    -1,    -1,    -1,   224,
      -1,    -1,    -1,    -1,    -1,   230,   231,    -1,   233,    -1,
     235,   236,    -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,
     245,   246,    -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,
      -1,    -1,    -1,    -1,   259,   260,   261,    -1,   263,    -1,
      -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,   273,    -1,
      -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,   284,
      -1,    -1,    -1,    -1,   289,   290,   291,    -1,    -1,    -1,
     295,    -1,    -1,    -1,   299,   300,   301,   302,    -1,   304,
      -1,   306,    -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,
      -1,    -1,     0,     1,   319,   320,   321,   322,   323,    -1,
       8,     9,    10,    11,    12,    -1,    14,    15,   333,    17,
      18,    -1,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    -1,    32,    -1,    34,    -1,    36,    37,
      38,    39,    -1,    -1,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    -1,    -1,    -1,    -1,    55,    -1,    -1,
      58,    59,    60,    -1,    62,    -1,    -1,    -1,    66,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,
      -1,    99,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,
     108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,
     128,    -1,    -1,   131,    -1,   133,    -1,    -1,    -1,    -1,
     138,    -1,    -1,    -1,   142,    -1,   144,   145,    -1,   147,
      -1,    -1,    -1,   151,   152,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,    -1,   165,    -1,    -1,
      -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   185,    -1,   187,
      -1,   189,    -1,   191,   192,   193,    -1,    -1,   196,    -1,
      -1,   199,   200,   201,   202,    -1,   204,    -1,    -1,   207,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,   217,
      -1,    -1,    -1,    -1,    -1,    -1,   224,    -1,    -1,    -1,
      -1,    -1,   230,   231,    -1,   233,    -1,   235,   236,    -1,
      -1,   239,    -1,    -1,    -1,    -1,    -1,   245,   246,    -1,
      -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,
      -1,   259,   260,   261,    -1,   263,    -1,    -1,    -1,   267,
      -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,   277,
      -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,
      -1,   289,   290,   291,    -1,    -1,    -1,   295,    -1,    -1,
      -1,   299,   300,   301,   302,    -1,   304,    -1,   306,    -1,
      -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,     0,
       1,   319,   320,   321,   322,   323,    -1,     8,     9,    10,
      11,    12,    -1,    14,    15,   333,    17,    18,    -1,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      -1,    32,    -1,    34,    -1,    36,    37,    38,    39,    -1,
      -1,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    59,    60,
      -1,    62,    -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    75,    76,    77,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    99,    -1,
      -1,    -1,    -1,   104,    -1,    -1,   107,   108,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,    -1,    -1,
     131,    -1,   133,    -1,    -1,    -1,    -1,   138,    -1,    -1,
      -1,   142,    -1,   144,   145,    -1,   147,    -1,    -1,    -1,
     151,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,   165,    -1,    -1,    -1,    -1,   170,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   185,    -1,   187,    -1,   189,    -1,
     191,   192,   193,    -1,    -1,   196,    -1,    -1,   199,   200,
     201,   202,    -1,   204,    -1,    -1,   207,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   216,   217,    -1,    -1,    -1,
      -1,    -1,    -1,   224,    -1,    -1,    -1,    -1,    -1,   230,
     231,    -1,   233,    -1,   235,   236,    -1,    -1,   239,    -1,
      -1,    -1,    -1,    -1,   245,   246,    -1,    -1,    -1,    -1,
      -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,   259,   260,
     261,    -1,   263,    -1,    -1,    -1,   267,    -1,    -1,    -1,
      -1,    -1,   273,    -1,    -1,    -1,   277,    -1,    -1,    -1,
      -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,   289,   290,
     291,    -1,    -1,    -1,   295,    -1,    -1,    -1,   299,   300,
     301,   302,    -1,   304,    -1,   306,    -1,    -1,    -1,    -1,
      -1,   312,    -1,    -1,    -1,    -1,     0,     1,   319,   320,
     321,   322,   323,    -1,     8,     9,    10,    11,    12,    -1,
      14,    15,   333,    17,    18,    -1,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    -1,    32,    -1,
      34,    -1,    36,    37,    38,    39,    -1,    -1,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    -1,    -1,    -1,
      -1,    55,    -1,    -1,    58,    59,    60,    -1,    62,    -1,
      -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    96,    -1,    -1,    99,    -1,    -1,    -1,    -1,
     104,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,    -1,    -1,   131,    -1,   133,
      -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,   142,    -1,
     144,   145,    -1,   147,    -1,    -1,    -1,   151,   152,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
      -1,   165,    -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   185,    -1,   187,    -1,   189,    -1,   191,   192,   193,
      -1,    -1,   196,    -1,    -1,   199,   200,   201,   202,    -1,
     204,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   216,   217,    -1,    -1,    -1,    -1,    -1,    -1,
     224,    -1,    -1,    -1,    -1,    -1,   230,   231,    -1,   233,
      -1,   235,   236,    -1,    -1,   239,    -1,    -1,    -1,    -1,
      -1,   245,   246,    -1,    -1,    -1,    -1,    -1,    -1,   253,
      -1,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,   263,
      -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,   273,
      -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,
     284,    -1,    -1,    -1,    -1,   289,   290,   291,    -1,    -1,
      -1,   295,    -1,    -1,    -1,   299,   300,   301,   302,    -1,
     304,    -1,   306,    -1,    -1,    -1,    -1,    -1,   312,    -1,
      -1,    -1,    -1,     0,     1,   319,   320,   321,   322,   323,
      -1,     8,     9,    10,    11,    12,    -1,    14,    15,   333,
      17,    18,    -1,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    -1,    32,    -1,    34,    -1,    36,
      37,    38,    39,    -1,    -1,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    -1,    -1,    -1,    -1,    55,    -1,
      -1,    58,    59,    60,    -1,    62,    -1,    -1,    -1,    66,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,
      -1,    -1,    99,    -1,    -1,    -1,    -1,   104,    -1,    -1,
     107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,    -1,    -1,   131,    -1,   133,    -1,    -1,    -1,
      -1,   138,    -1,    -1,    -1,   142,    -1,   144,   145,    -1,
     147,    -1,    -1,    -1,   151,   152,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,   165,    -1,
      -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   185,    -1,
     187,    -1,   189,    -1,   191,   192,   193,    -1,    -1,   196,
      -1,    -1,   199,   200,   201,   202,    -1,   204,    -1,    -1,
     207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,
     217,    -1,    -1,    -1,    -1,    -1,    -1,   224,    -1,    -1,
      -1,    -1,    -1,   230,   231,    -1,   233,    -1,   235,   236,
      -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,   245,   246,
      -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,
      -1,    -1,   259,   260,   261,    -1,   263,    -1,    -1,    -1,
     267,    -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,
     277,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,
      -1,    -1,   289,   290,   291,    -1,    -1,    -1,   295,    -1,
      -1,    -1,   299,   300,   301,   302,    -1,   304,    -1,   306,
      -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,
       0,     1,   319,   320,   321,   322,   323,    -1,     8,     9,
      10,    11,    12,    -1,    14,    15,   333,    17,    18,    -1,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    -1,    32,    -1,    34,    -1,    36,    37,    38,    39,
      -1,    -1,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    59,
      60,    -1,    62,    -1,    -1,    -1,    66,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    75,    76,    77,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    99,
      -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,    -1,
      -1,   131,    -1,   133,    -1,    -1,    -1,    -1,   138,    -1,
      -1,    -1,   142,    -1,   144,   145,    -1,   147,    -1,    -1,
      -1,   151,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   163,    -1,   165,    -1,    -1,    -1,    -1,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   185,    -1,   187,    -1,   189,
      -1,   191,   192,   193,    -1,    -1,   196,    -1,    -1,   199,
     200,   201,   202,    -1,   204,    -1,    -1,   207,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   216,   217,    -1,    -1,
      -1,    -1,    -1,    -1,   224,    -1,    -1,    -1,    -1,    -1,
     230,   231,    -1,   233,    -1,   235,   236,    -1,    -1,   239,
      -1,    -1,    -1,    -1,    -1,   245,   246,    -1,    -1,    -1,
      -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,   259,
     260,   261,    -1,   263,    -1,    -1,    -1,   267,    -1,    -1,
      -1,    -1,    -1,   273,    -1,    -1,    -1,   277,    -1,    -1,
      -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,   289,
     290,   291,    -1,    -1,    -1,   295,    -1,    -1,    -1,   299,
     300,   301,   302,    -1,   304,    -1,   306,    -1,    -1,    -1,
      -1,    -1,   312,    -1,    -1,    -1,    -1,     0,     1,   319,
     320,   321,   322,   323,    -1,     8,     9,    10,    11,    12,
      -1,    14,    15,   333,    17,    18,    -1,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    -1,    32,
      -1,    34,    -1,    36,    37,    38,    39,    -1,    -1,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    -1,    -1,
      -1,    -1,    55,    -1,    -1,    58,    59,    60,    -1,    62,
      -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    75,    76,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    96,    -1,    -1,    99,    -1,    -1,    -1,
      -1,   104,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,    -1,    -1,   131,    -1,
     133,    -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,   142,
      -1,   144,   145,    -1,   147,    -1,    -1,    -1,   151,   152,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     163,    -1,   165,    -1,    -1,    -1,    -1,   170,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   185,    -1,   187,    -1,   189,    -1,   191,   192,
     193,    -1,    -1,   196,    -1,    -1,   199,   200,   201,   202,
      -1,   204,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   216,   217,    -1,    -1,    -1,    -1,    -1,
      -1,   224,    -1,    -1,    -1,    -1,    -1,   230,   231,    -1,
     233,    -1,   235,   236,    -1,    -1,   239,    -1,    -1,    -1,
      -1,    -1,   245,   246,    -1,    -1,    -1,    -1,    -1,    -1,
     253,    -1,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,
     263,    -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,
     273,    -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,
      -1,   284,    -1,    -1,    -1,    -1,   289,   290,   291,    -1,
      -1,    -1,   295,    -1,    -1,    -1,   299,   300,   301,   302,
      -1,   304,    -1,   306,    -1,    -1,    -1,    -1,    -1,   312,
      -1,    -1,    -1,    -1,    -1,    -1,   319,   320,   321,   322,
     323,    -1,    -1,    -1,     1,    -1,    -1,    -1,     5,    -1,
     333,     8,     9,    10,    11,    12,    -1,    14,    15,    -1,
      17,    18,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    -1,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    55,    56,
      57,    58,    59,    60,    -1,    62,    63,    -1,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    -1,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      -1,    98,    99,   100,    -1,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,    -1,   115,   116,
     117,    -1,   119,    -1,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,   135,   136,
      -1,   138,   139,   140,    -1,   142,    -1,   144,   145,   146,
     147,   148,   149,   150,   151,   152,   153,   154,   155,   156,
     157,   158,   159,   160,   161,   162,   163,   164,   165,   166,
     167,   168,   169,   170,    -1,    -1,    -1,    -1,   175,   176,
     177,   178,   179,   180,    -1,   182,   183,   184,   185,   186,
     187,    -1,   189,   190,   191,   192,   193,    -1,   195,   196,
      -1,    -1,    -1,   200,   201,   202,    -1,   204,    -1,    -1,
     207,    -1,    -1,   210,   211,    -1,   213,   214,   215,   216,
     217,   218,   219,   220,    -1,   222,   223,    -1,    -1,   226,
     227,    -1,   229,   230,   231,    -1,   233,    -1,    -1,   236,
     237,   238,   239,   240,   241,   242,   243,   244,   245,   246,
     247,   248,   249,   250,   251,    -1,   253,   254,   255,   256,
     257,   258,   259,   260,   261,   262,   263,   264,   265,   266,
     267,   268,   269,   270,   271,   272,   273,    -1,    -1,    -1,
     277,   278,   279,   280,   281,   282,   283,   284,    -1,    -1,
      -1,   288,   289,   290,   291,    -1,    -1,    -1,    -1,    -1,
      -1,   298,    -1,   300,    -1,   302,    -1,    -1,    -1,   306,
      -1,   308,    -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,
      -1,    -1,   319,   320,   321,   322,   323,    -1,    -1,    -1,
       1,    -1,    -1,    -1,     5,    -1,   333,     8,     9,    10,
      11,    12,    -1,    14,    15,    -1,    17,    18,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    -1,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    56,    57,    58,    59,    60,
      -1,    62,    63,    -1,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    -1,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    -1,    98,    99,   100,
      -1,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,    -1,   115,   116,   117,    -1,   119,    -1,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,   135,   136,    -1,   138,   139,   140,
      -1,   142,    -1,   144,   145,   146,   147,   148,   149,   150,
     151,   152,   153,   154,   155,   156,   157,   158,   159,   160,
     161,   162,   163,   164,   165,   166,   167,   168,   169,   170,
      -1,    -1,    -1,    -1,   175,   176,   177,   178,   179,   180,
      -1,   182,   183,   184,   185,   186,   187,    -1,   189,   190,
     191,   192,   193,    -1,   195,   196,    -1,    -1,    -1,   200,
     201,   202,    -1,   204,    -1,    -1,   207,    -1,    -1,   210,
     211,    -1,   213,   214,   215,   216,   217,   218,   219,   220,
      -1,   222,   223,    -1,    -1,   226,   227,    -1,   229,   230,
     231,    -1,   233,    -1,    -1,   236,   237,   238,   239,   240,
     241,   242,   243,   244,   245,   246,   247,   248,   249,   250,
     251,    -1,   253,   254,   255,   256,   257,   258,   259,   260,
     261,   262,   263,   264,   265,   266,   267,   268,   269,   270,
     271,   272,   273,    -1,    -1,    -1,   277,   278,   279,   280,
     281,   282,   283,   284,    -1,    -1,    -1,   288,   289,   290,
     291,    -1,    -1,    -1,    -1,    -1,    -1,   298,    -1,   300,
      -1,   302,    -1,    -1,    -1,   306,    -1,   308,    -1,    -1,
      -1,   312,    -1,    -1,    -1,    -1,    -1,     1,   319,   320,
     321,   322,   323,    -1,     8,     9,    10,    11,    12,    -1,
      14,    15,   333,    17,    18,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    -1,    32,    -1,
      34,    -1,    36,    37,    38,    39,    -1,    -1,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    -1,    -1,
      -1,    55,    -1,    -1,    58,    59,    60,    -1,    62,    -1,
      -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    87,    88,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    96,    -1,    -1,    99,    -1,    -1,    -1,    -1,
     104,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,    -1,    -1,   131,   132,   133,
      -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,   142,    -1,
     144,   145,   146,   147,    -1,    -1,   150,   151,   152,    -1,
      -1,    -1,    -1,    -1,   158,    -1,    -1,    -1,    -1,   163,
      -1,   165,   166,    -1,    -1,    -1,   170,    -1,    -1,    -1,
      -1,   175,    -1,   177,    -1,    -1,    -1,    -1,    -1,   183,
      -1,    -1,    -1,   187,    -1,   189,    -1,   191,   192,   193,
      -1,   195,   196,    -1,    -1,    -1,   200,   201,   202,    -1,
     204,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,
     214,   215,   216,   217,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   230,   231,    -1,   233,
      -1,    -1,   236,    -1,    -1,   239,    -1,    -1,    -1,    -1,
      -1,   245,   246,   247,    -1,    -1,    -1,    -1,    -1,   253,
      -1,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,   263,
      -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,   273,
      -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,
     284,    -1,    -1,    -1,    -1,   289,   290,   291,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,   302,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,
      -1,    -1,    -1,    -1,     1,   319,   320,   321,   322,   323,
      -1,     8,     9,    10,    11,    12,    -1,    14,    15,   333,
      17,    18,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    -1,    32,    -1,    34,    -1,    36,
      37,    38,    39,    -1,    -1,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    -1,    -1,    -1,    55,    -1,
      -1,    58,    59,    60,    -1,    62,    -1,    -1,    -1,    66,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,
      -1,    -1,    99,    -1,    -1,    -1,    -1,   104,    -1,    -1,
     107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,    -1,    -1,   131,   132,   133,    -1,    -1,    -1,
      -1,   138,    -1,    -1,    -1,   142,    -1,   144,   145,    -1,
     147,    -1,    -1,    -1,   151,   152,    -1,    -1,    -1,    -1,
      -1,   158,    -1,    -1,    -1,    -1,   163,    -1,   165,   166,
      -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,
     177,    -1,    -1,    -1,    -1,    -1,   183,    -1,    -1,    -1,
     187,    -1,   189,    -1,   191,   192,   193,    -1,   195,   196,
      -1,    -1,    -1,   200,   201,   202,    -1,   204,    -1,    -1,
     207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,
     217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   230,   231,    -1,   233,    -1,    -1,   236,
      -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,   245,   246,
      -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,
      -1,    -1,   259,   260,   261,    -1,   263,    -1,    -1,    -1,
     267,    -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,
     277,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,
      -1,    -1,   289,   290,   291,    -1,    -1,    -1,    -1,    -1,
      -1,   298,    -1,   300,    -1,   302,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,
      -1,     1,   319,   320,   321,   322,   323,    -1,     8,     9,
      10,    11,    12,    -1,    14,    15,   333,    17,    18,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    -1,    32,    -1,    34,    -1,    36,    37,    38,    39,
      -1,    -1,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    -1,    -1,    -1,    55,    -1,    -1,    58,    59,
      60,    -1,    62,    -1,    -1,    -1,    66,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    75,    76,    77,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    99,
      -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,    -1,
      -1,   131,   132,   133,    -1,    -1,    -1,    -1,   138,    -1,
      -1,    -1,   142,    -1,   144,   145,    -1,   147,    -1,    -1,
      -1,   151,   152,    -1,    -1,    -1,    -1,    -1,   158,    -1,
      -1,    -1,    -1,   163,    -1,   165,   166,    -1,    -1,    -1,
     170,    -1,    -1,    -1,    -1,    -1,    -1,   177,    -1,    -1,
      -1,    -1,    -1,   183,    -1,    -1,    -1,   187,    -1,   189,
      -1,   191,   192,   193,    -1,   195,   196,    -1,    -1,    -1,
     200,   201,   202,    -1,   204,    -1,    -1,   207,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   216,   217,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     230,   231,    -1,   233,    -1,    -1,   236,    -1,    -1,   239,
      -1,    -1,    -1,    -1,    -1,   245,   246,    -1,    -1,    -1,
      -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,   259,
     260,   261,    -1,   263,    -1,    -1,    -1,   267,    -1,    -1,
      -1,    -1,    -1,   273,    -1,    -1,    -1,   277,    -1,    -1,
      -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,   289,
     290,   291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     300,    -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,     1,   319,
     320,   321,   322,   323,    -1,     8,     9,    10,    11,    12,
      -1,    14,    15,   333,    17,    18,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    -1,    32,
      -1,    34,    -1,    36,    37,    38,    39,    -1,    -1,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    -1,
      -1,    -1,    55,    -1,    -1,    58,    59,    60,    -1,    62,
      -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    75,    76,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    96,    -1,    -1,    99,    -1,    -1,    -1,
      -1,   104,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,    -1,    -1,   131,   132,
     133,    -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,   142,
      -1,   144,   145,    -1,   147,    -1,    -1,    -1,   151,   152,
      -1,    -1,    -1,    -1,    -1,   158,    -1,    -1,    -1,    -1,
     163,    -1,   165,   166,    -1,    -1,    -1,   170,    -1,    -1,
      -1,    -1,    -1,    -1,   177,    -1,    -1,    -1,    -1,    -1,
     183,    -1,    -1,    -1,   187,    -1,   189,    -1,   191,   192,
     193,    -1,   195,   196,    -1,    -1,    -1,   200,   201,   202,
      -1,   204,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   216,   217,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,   231,    -1,
     233,    -1,    -1,   236,    -1,    -1,   239,    -1,    -1,    -1,
      -1,    -1,   245,   246,    -1,    -1,    -1,    -1,    -1,    -1,
     253,    -1,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,
     263,    -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,
     273,    -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,
      -1,   284,    -1,    -1,    -1,    -1,   289,   290,   291,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,   302,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,
      -1,    -1,    -1,    -1,    -1,     1,   319,   320,   321,   322,
     323,    -1,     8,     9,    10,    11,    12,    -1,    14,    15,
     333,    17,    18,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    -1,    32,    -1,    34,    -1,
      36,    37,    38,    39,    -1,    -1,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    -1,    -1,    -1,    55,
      -1,    -1,    58,    59,    60,    -1,    62,    -1,    -1,    -1,
      66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,
      76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      96,    -1,    -1,    99,    -1,    -1,    -1,    -1,   104,    -1,
      -1,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,
     126,   127,   128,    -1,    -1,   131,   132,   133,    -1,    -1,
      -1,    -1,   138,    -1,    -1,    -1,   142,    -1,   144,   145,
      -1,   147,    -1,    -1,    -1,   151,   152,    -1,    -1,    -1,
      -1,    -1,   158,    -1,    -1,    -1,    -1,   163,    -1,   165,
     166,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,
      -1,   177,    -1,    -1,    -1,    -1,    -1,   183,    -1,    -1,
      -1,   187,    -1,   189,    -1,   191,   192,   193,    -1,   195,
     196,    -1,    -1,    -1,   200,   201,   202,    -1,   204,    -1,
      -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     216,   217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   230,   231,    -1,   233,    -1,    -1,
     236,    -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,   245,
     246,    -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,
      -1,    -1,    -1,   259,   260,   261,    -1,   263,    -1,    -1,
      -1,   267,    -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,
      -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,
      -1,    -1,    -1,   289,   290,   291,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   300,    -1,   302,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,
      -1,    -1,     1,   319,   320,   321,   322,   323,    -1,     8,
       9,    10,    11,    12,    -1,    14,    15,   333,    17,    18,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    -1,    32,    -1,    34,    -1,    36,    37,    38,
      39,    -1,    -1,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    -1,    -1,    -1,    55,    -1,    -1,    58,
      59,    60,    -1,    62,    -1,    -1,    -1,    66,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    77,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,
      99,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,
      -1,    -1,   131,   132,   133,    -1,    -1,    -1,    -1,   138,
      -1,    -1,    -1,   142,    -1,   144,   145,    -1,   147,    -1,
      -1,    -1,   151,   152,    -1,    -1,    -1,    -1,    -1,   158,
      -1,    -1,    -1,    -1,   163,    -1,   165,   166,    -1,    -1,
      -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,   177,    -1,
      -1,    -1,    -1,    -1,   183,    -1,    -1,    -1,   187,    -1,
     189,    -1,   191,   192,   193,    -1,   195,   196,    -1,    -1,
      -1,   200,   201,   202,    -1,   204,    -1,    -1,   207,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,   217,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   230,   231,    -1,   233,    -1,    -1,   236,    -1,    -1,
     239,    -1,    -1,    -1,    -1,    -1,   245,   246,    -1,    -1,
      -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,
     259,   260,   261,    -1,   263,    -1,    -1,    -1,   267,    -1,
      -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,   277,    -1,
      -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,
     289,   290,   291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   300,    -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,     1,
     319,   320,   321,   322,   323,    -1,     8,     9,    10,    11,
      12,    -1,    14,    15,   333,    17,    18,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    -1,
      32,    -1,    34,    -1,    36,    37,    38,    39,    -1,    -1,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      -1,    -1,    -1,    55,    -1,    -1,    58,    59,    60,    -1,
      62,    -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    96,    -1,    -1,    99,    -1,    -1,
      -1,    -1,   104,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,   126,   127,   128,    -1,    -1,   131,
     132,   133,    -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,
     142,    -1,   144,   145,    -1,   147,    -1,    -1,    -1,   151,
     152,    -1,    -1,    -1,    -1,    -1,   158,    -1,    -1,    -1,
      -1,   163,    -1,   165,   166,    -1,    -1,    -1,   170,    -1,
      -1,    -1,    -1,    -1,    -1,   177,    -1,    -1,    -1,    -1,
      -1,   183,    -1,    -1,    -1,   187,    -1,   189,    -1,   191,
     192,   193,    -1,   195,   196,    -1,    -1,    -1,   200,   201,
     202,    -1,   204,    -1,    -1,   207,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   216,   217,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,   231,
      -1,   233,    -1,    -1,   236,    -1,    -1,   239,    -1,    -1,
      -1,    -1,    -1,   245,   246,    -1,    -1,    -1,    -1,    -1,
      -1,   253,    -1,    -1,    -1,    -1,    -1,   259,   260,   261,
      -1,   263,    -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,
      -1,   273,    -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,
      -1,    -1,   284,    -1,    -1,    -1,    -1,   289,   290,   291,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,
     302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     312,    -1,    -1,    -1,    -1,    -1,     1,   319,   320,   321,
     322,   323,    -1,     8,     9,    10,    11,    12,    -1,    14,
      15,   333,    17,    18,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    -1,    32,    -1,    34,
      -1,    36,    37,    38,    39,    -1,    -1,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    -1,    -1,    -1,
      55,    -1,    -1,    58,    59,    60,    -1,    62,    -1,    -1,
      -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      75,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    96,    -1,    -1,    99,    -1,    -1,    -1,    -1,   104,
      -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,   126,   127,   128,    -1,    -1,   131,   132,   133,    -1,
      -1,    -1,    -1,   138,    -1,    -1,    -1,   142,    -1,   144,
     145,    -1,   147,    -1,    -1,    -1,   151,   152,    -1,    -1,
      -1,    -1,    -1,   158,    -1,    -1,    -1,    -1,   163,    -1,
     165,   166,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,
      -1,    -1,   177,    -1,    -1,    -1,    -1,    -1,   183,    -1,
      -1,    -1,   187,    -1,   189,    -1,   191,   192,   193,    -1,
     195,   196,    -1,    -1,    -1,   200,   201,   202,    -1,   204,
      -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   216,   217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   230,   231,    -1,   233,    -1,
      -1,   236,    -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,
     245,   246,    -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,
      -1,    -1,    -1,    -1,   259,   260,   261,    -1,   263,    -1,
      -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,   273,    -1,
      -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,   284,
      -1,    -1,    -1,    -1,   289,   290,   291,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   300,    -1,   302,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,
      -1,    -1,    -1,     1,   319,   320,   321,   322,   323,    -1,
       8,     9,    10,    11,    12,    -1,    14,    15,   333,    17,
      18,    -1,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    -1,    32,    -1,    34,    -1,    36,    37,
      38,    39,    -1,    -1,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    -1,    -1,    -1,    -1,    55,    -1,    -1,
      58,    59,    60,    -1,    62,    -1,    -1,    -1,    66,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,
      -1,    99,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,
     108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,
     128,    -1,    -1,   131,    -1,   133,    -1,    -1,    -1,    -1,
     138,    -1,    -1,    -1,   142,    -1,   144,   145,    -1,   147,
      -1,    -1,    -1,   151,   152,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,    -1,   165,    -1,    -1,
      -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,
      -1,   189,    -1,   191,   192,   193,    -1,    -1,   196,    -1,
      -1,    -1,   200,   201,   202,    -1,   204,    -1,    -1,   207,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,   217,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   230,   231,    -1,   233,    -1,    -1,   236,    -1,
      -1,   239,    -1,    -1,    -1,    -1,    -1,   245,   246,    -1,
      -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,
      -1,   259,   260,   261,    -1,   263,    -1,    -1,    -1,   267,
      -1,    -1,    -1,    -1,    -1,   273,    -1,   275,   276,   277,
      -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,
      -1,   289,   290,   291,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   300,    -1,   302,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,
       1,   319,   320,   321,   322,   323,    -1,     8,     9,    10,
      11,    12,    -1,    14,    15,   333,    17,    18,    -1,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      -1,    32,    -1,    34,    -1,    36,    37,    38,    39,    -1,
      -1,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    59,    60,
      -1,    62,    -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    75,    76,    77,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    99,    -1,
      -1,    -1,    -1,   104,    -1,    -1,   107,   108,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,    -1,    -1,
     131,    -1,   133,    -1,    -1,    -1,    -1,   138,    -1,    -1,
      -1,   142,    -1,   144,   145,    -1,   147,    -1,    -1,    -1,
     151,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,   165,    -1,    -1,    -1,    -1,   170,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,   189,    -1,
     191,   192,   193,    -1,    -1,   196,    -1,    -1,    -1,   200,
     201,   202,    -1,   204,    -1,    -1,   207,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   216,   217,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,
     231,    -1,   233,    -1,    -1,   236,    -1,    -1,   239,    -1,
      -1,    -1,    -1,    -1,   245,   246,    -1,    -1,    -1,    -1,
      -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,   259,   260,
     261,    -1,   263,    -1,    -1,    -1,   267,    -1,    -1,    -1,
      -1,    -1,   273,    -1,   275,   276,   277,    -1,    -1,    -1,
      -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,   289,   290,
     291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,
      -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   312,    -1,    -1,    -1,    -1,    -1,     1,   319,   320,
     321,   322,   323,    -1,     8,     9,    10,    11,    12,    -1,
      14,    15,   333,    17,    18,    -1,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    -1,    32,    -1,
      34,    -1,    36,    37,    38,    39,    -1,    -1,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    -1,    -1,    -1,
      -1,    55,    -1,    -1,    58,    59,    60,    -1,    62,    -1,
      -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    96,    -1,    -1,    99,    -1,    -1,    -1,    -1,
     104,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,    -1,    -1,   131,    -1,   133,
      -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,   142,    -1,
     144,   145,    -1,   147,    -1,    -1,    -1,   151,   152,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
      -1,   165,    -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,
     174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   187,    -1,   189,    -1,   191,   192,   193,
      -1,    -1,   196,    -1,    -1,    -1,   200,   201,   202,    -1,
     204,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   216,   217,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   230,   231,    -1,   233,
      -1,    -1,   236,    -1,    -1,   239,    -1,    -1,    -1,    -1,
      -1,   245,   246,    -1,    -1,    -1,    -1,    -1,    -1,   253,
      -1,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,   263,
      -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,   273,
      -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,
     284,    -1,    -1,    -1,    -1,   289,   290,   291,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,   302,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,
      -1,    -1,    -1,    -1,     1,   319,   320,   321,   322,   323,
      -1,     8,     9,    10,    11,    12,    -1,    14,    15,   333,
      17,    18,    -1,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    -1,    32,    -1,    34,    -1,    36,
      37,    38,    39,    -1,    -1,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    -1,    -1,    -1,    -1,    55,    -1,
      -1,    58,    59,    60,    -1,    62,    -1,    -1,    -1,    66,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,
      -1,    -1,    99,    -1,    -1,    -1,    -1,   104,    -1,    -1,
     107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,    -1,    -1,   131,    -1,   133,    -1,    -1,    -1,
      -1,   138,    -1,    -1,    -1,   142,    -1,   144,   145,    -1,
     147,    -1,    -1,    -1,   151,   152,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,   165,    -1,
      -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     187,    -1,   189,    -1,   191,   192,   193,    -1,    -1,   196,
      -1,    -1,    -1,   200,   201,   202,    -1,   204,    -1,    -1,
     207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,
     217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   230,   231,    -1,   233,    -1,    -1,   236,
      -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,   245,   246,
      -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,
      -1,    -1,   259,   260,   261,    -1,   263,    -1,    -1,    -1,
     267,    -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,
     277,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,
      -1,    -1,   289,   290,   291,    -1,    -1,    -1,   295,    -1,
      -1,    -1,    -1,   300,    -1,   302,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,
      -1,     1,   319,   320,   321,   322,   323,    -1,     8,     9,
      10,    11,    12,    -1,    14,    15,   333,    17,    18,    -1,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    -1,    32,    -1,    34,    -1,    36,    37,    38,    39,
      -1,    -1,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    59,
      60,    -1,    62,    -1,    -1,    -1,    66,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    75,    76,    77,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    99,
      -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,    -1,
      -1,   131,    -1,   133,    -1,    -1,    -1,    -1,   138,    -1,
      -1,    -1,   142,    -1,   144,   145,    -1,   147,    -1,    -1,
      -1,   151,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   163,    -1,   165,    -1,    -1,    -1,    -1,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,   189,
      -1,   191,   192,   193,    -1,    -1,   196,    -1,    -1,    -1,
     200,   201,   202,    -1,   204,    -1,    -1,   207,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   216,   217,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     230,   231,    -1,   233,    -1,    -1,   236,    -1,    -1,   239,
      -1,    -1,    -1,    -1,    -1,   245,   246,    -1,    -1,    -1,
      -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,   259,
     260,   261,    -1,   263,    -1,    -1,    -1,   267,    -1,    -1,
      -1,    -1,    -1,   273,    -1,    -1,    -1,   277,    -1,    -1,
      -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,   289,
     290,   291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     300,    -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,     1,   319,
     320,   321,   322,   323,    -1,     8,     9,    10,    11,    12,
      -1,    14,    15,   333,    17,    18,    -1,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    -1,    32,
      -1,    34,    -1,    36,    37,    38,    39,    -1,    -1,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    -1,    -1,
      -1,    -1,    55,    -1,    -1,    58,    59,    60,    -1,    62,
      -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    75,    76,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    96,    -1,    -1,    99,    -1,    -1,    -1,
      -1,   104,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,    -1,    -1,   131,    -1,
     133,    -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,   142,
      -1,   144,   145,    -1,   147,    -1,    -1,    -1,   151,   152,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     163,    -1,   165,    -1,    -1,    -1,    -1,   170,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   187,    -1,   189,    -1,   191,   192,
     193,    -1,    -1,   196,    -1,    -1,    -1,   200,   201,   202,
      -1,   204,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   216,   217,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,   231,    -1,
     233,    -1,    -1,   236,    -1,    -1,   239,    -1,    -1,    -1,
      -1,    -1,   245,   246,    -1,    -1,    -1,    -1,    -1,    -1,
     253,    -1,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,
     263,    -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,
     273,    -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,
      -1,   284,    -1,    -1,    -1,    -1,   289,   290,   291,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,   302,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,
      -1,    -1,    -1,    -1,    -1,     1,   319,   320,   321,   322,
     323,    -1,     8,     9,    10,    11,    12,    -1,    14,    15,
     333,    17,    18,    -1,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    -1,    32,    -1,    34,    -1,
      36,    37,    38,    39,    -1,    -1,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    -1,    -1,    -1,    -1,    55,
      -1,    -1,    58,    59,    60,    -1,    62,    -1,    -1,    -1,
      66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,
      76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      96,    -1,    -1,    99,    -1,    -1,    -1,    -1,   104,    -1,
      -1,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,
     126,   127,   128,    -1,    -1,   131,    -1,   133,    -1,    -1,
      -1,    -1,   138,    -1,    -1,    -1,   142,    -1,   144,   145,
      -1,   147,    -1,    -1,    -1,   151,   152,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,   165,
      -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   187,    -1,   189,    -1,   191,   192,   193,    -1,    -1,
     196,    -1,    -1,    -1,   200,   201,   202,    -1,   204,    -1,
      -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     216,   217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   230,   231,    -1,   233,    -1,    -1,
     236,    -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,   245,
     246,    -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,
      -1,    -1,    -1,   259,   260,   261,    -1,   263,    -1,    -1,
      -1,   267,    -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,
      -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,
      -1,    -1,    -1,   289,   290,   291,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   300,    -1,   302,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,
      -1,    -1,     1,   319,   320,   321,   322,   323,    -1,     8,
       9,    10,    11,    12,    -1,    14,    15,   333,    17,    18,
      -1,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    -1,    32,    -1,    34,    -1,    36,    37,    38,
      39,    -1,    -1,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    -1,    -1,    -1,    -1,    55,    -1,    -1,    58,
      59,    60,    -1,    62,    -1,    -1,    -1,    66,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    77,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,
      99,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,
      -1,    -1,   131,    -1,   133,    -1,    -1,    -1,    -1,   138,
      -1,    -1,    -1,   142,    -1,   144,   145,    -1,   147,    -1,
      -1,    -1,   151,   152,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,    -1,   165,    -1,    -1,    -1,
      -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,
     189,    -1,   191,   192,   193,    -1,    -1,   196,    -1,    -1,
      -1,   200,   201,   202,    -1,   204,    -1,    -1,   207,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,   217,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   230,   231,    -1,   233,    -1,    -1,   236,    -1,    -1,
     239,    -1,    -1,    -1,    -1,    -1,   245,   246,    -1,    -1,
      -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,
     259,   260,   261,    -1,   263,    -1,    -1,    -1,   267,    -1,
      -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,   277,    -1,
      -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,
     289,   290,   291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   300,    -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,     1,
     319,   320,   321,   322,   323,    -1,     8,     9,    10,    11,
      12,    -1,    14,    15,   333,    17,    18,    -1,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    -1,
      32,    -1,    34,    -1,    36,    37,    38,    39,    -1,    -1,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    -1,
      -1,    -1,    -1,    55,    -1,    -1,    58,    59,    60,    -1,
      62,    -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    96,    -1,    -1,    99,    -1,    -1,
      -1,    -1,   104,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,   126,   127,   128,    -1,    -1,   131,
      -1,   133,    -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,
     142,    -1,   144,   145,    -1,   147,    -1,    -1,    -1,   151,
     152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,    -1,   165,    -1,    -1,    -1,    -1,   170,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   187,    -1,   189,    -1,   191,
     192,   193,    -1,    -1,   196,    -1,    -1,    -1,   200,   201,
     202,    -1,   204,    -1,    -1,   207,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   216,   217,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,   231,
      -1,   233,    -1,    -1,   236,    -1,    -1,   239,    -1,    -1,
      -1,    -1,    -1,   245,   246,    -1,    -1,    -1,    -1,    -1,
      -1,   253,    -1,    -1,    -1,    -1,    -1,   259,   260,   261,
      -1,   263,    -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,
      -1,   273,    -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,
      -1,    -1,   284,    -1,    -1,    -1,    -1,   289,   290,   291,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,
     302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     312,    -1,    -1,    -1,    -1,    -1,     1,   319,   320,   321,
     322,   323,    -1,     8,     9,    10,    11,    12,    -1,    14,
      15,   333,    17,    18,    -1,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    -1,    32,    -1,    34,
      -1,    36,    37,    38,    39,    -1,    -1,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    -1,    -1,    -1,    -1,
      55,    -1,    -1,    58,    59,    60,    -1,    62,    -1,    -1,
      -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      75,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    96,    -1,    -1,    99,    -1,    -1,    -1,    -1,   104,
      -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,   126,   127,   128,    -1,    -1,   131,    -1,   133,    -1,
      -1,    -1,    -1,   138,    -1,    -1,    -1,   142,    -1,   144,
     145,    -1,   147,    -1,    -1,    -1,   151,   152,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,
     165,    -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   187,    -1,   189,    -1,   191,   192,   193,    -1,
      -1,   196,    -1,    -1,    -1,   200,   201,   202,    -1,   204,
      -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   216,   217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   230,   231,    -1,   233,    -1,
      -1,   236,    -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,
     245,   246,    -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,
      -1,    -1,    -1,    -1,   259,   260,   261,    -1,   263,    -1,
      -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,   273,    -1,
      -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,   284,
      -1,    -1,    -1,    -1,   289,   290,   291,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   300,    -1,   302,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,
      -1,    -1,    -1,     1,   319,   320,   321,   322,   323,    -1,
       8,     9,    10,    11,    12,    -1,    14,    15,   333,    17,
      18,    -1,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    -1,    32,    -1,    34,    -1,    36,    37,
      38,    39,    -1,    -1,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    -1,    -1,    -1,    -1,    55,    -1,    -1,
      58,    59,    60,    -1,    62,    -1,    -1,    -1,    66,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,
      -1,    99,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,
     108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,
     128,    -1,    -1,   131,    -1,   133,    -1,    -1,    -1,    -1,
     138,    -1,    -1,    -1,   142,    -1,   144,   145,    -1,   147,
      -1,    -1,    -1,   151,   152,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,    -1,   165,    -1,    -1,
      -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,
      -1,   189,    -1,   191,   192,   193,    -1,    -1,   196,    -1,
      -1,    -1,   200,   201,   202,    -1,   204,    -1,    -1,   207,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,   217,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   230,   231,    -1,   233,    -1,    -1,   236,    -1,
      -1,   239,    -1,    -1,    -1,    -1,    -1,   245,   246,    -1,
      -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,
      -1,   259,   260,   261,    -1,   263,    -1,    -1,    -1,   267,
      -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,   277,
      -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,
      -1,   289,   290,   291,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   300,    -1,   302,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,
       1,   319,   320,   321,   322,   323,    -1,     8,     9,    10,
      11,    12,    -1,    14,    15,   333,    17,    18,    -1,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      -1,    32,    -1,    34,    -1,    36,    37,    38,    39,    -1,
      -1,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    59,    60,
      -1,    62,    -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    75,    76,    77,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    99,    -1,
      -1,    -1,    -1,   104,    -1,    -1,   107,   108,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,    -1,    -1,
     131,    -1,   133,    -1,    -1,    -1,    -1,   138,    -1,    -1,
      -1,   142,    -1,   144,   145,    -1,   147,    -1,    -1,    -1,
     151,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,   165,    -1,    -1,    -1,    -1,   170,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,   189,    -1,
     191,   192,   193,    -1,    -1,   196,    -1,    -1,    -1,   200,
     201,   202,    -1,   204,    -1,    -1,   207,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   216,   217,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,
     231,    -1,   233,    -1,    -1,   236,    -1,    -1,   239,    -1,
      -1,    -1,    -1,    -1,   245,   246,    -1,    -1,    -1,    -1,
      -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,   259,   260,
     261,    -1,   263,    -1,    -1,    -1,   267,    -1,    -1,    -1,
      -1,    -1,   273,    -1,    -1,    -1,   277,    -1,    -1,    -1,
      -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,   289,   290,
     291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,
      -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   312,    -1,    -1,    -1,    -1,    -1,     1,   319,   320,
     321,   322,   323,    -1,     8,     9,    10,    11,    12,    -1,
      14,    15,   333,    17,    18,    -1,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    -1,    32,    -1,
      34,    -1,    36,    37,    38,    39,    -1,    -1,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    -1,    -1,    -1,
      -1,    55,    -1,    -1,    58,    59,    60,    -1,    62,    -1,
      -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    96,    -1,    -1,    99,    -1,    -1,    -1,    -1,
     104,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,    -1,    -1,   131,    -1,   133,
      -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,   142,    -1,
     144,   145,    -1,   147,    -1,    -1,    -1,   151,   152,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
      -1,   165,    -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   187,    -1,   189,    -1,   191,   192,   193,
      -1,    -1,   196,    -1,    -1,    -1,   200,   201,   202,    -1,
     204,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   216,   217,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   230,   231,    -1,   233,
      -1,    -1,   236,    -1,    -1,   239,    -1,    -1,    -1,    -1,
      -1,   245,   246,    -1,    -1,    -1,    -1,    -1,    -1,   253,
      -1,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,   263,
      -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,   273,
      -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,
     284,    -1,    -1,    -1,    -1,   289,   290,   291,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,   302,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,
      -1,    -1,    -1,    -1,     1,   319,   320,   321,   322,   323,
      -1,     8,     9,    10,    11,    12,    -1,    14,    15,   333,
      17,    18,    -1,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    -1,    32,    -1,    34,    -1,    36,
      37,    38,    39,    -1,    -1,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    -1,    -1,    -1,    -1,    55,    -1,
      -1,    58,    59,    60,    -1,    62,    -1,    -1,    -1,    66,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,
      -1,    -1,    99,    -1,    -1,    -1,    -1,   104,    -1,    -1,
     107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,    -1,    -1,   131,    -1,   133,    -1,    -1,    -1,
      -1,   138,    -1,    -1,    -1,   142,    -1,   144,   145,    -1,
     147,    -1,    -1,    -1,   151,   152,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,   165,    -1,
      -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     187,    -1,   189,    -1,   191,   192,   193,    -1,    -1,   196,
      -1,    -1,    -1,   200,   201,   202,    -1,   204,    -1,    -1,
     207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,
     217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   230,   231,    -1,   233,    -1,    -1,   236,
      -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,   245,   246,
      -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,
      -1,    -1,   259,   260,   261,    -1,   263,    -1,    -1,    -1,
     267,    -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,
     277,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,
      -1,    -1,   289,   290,   291,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   300,    -1,   302,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,
      -1,     1,   319,   320,   321,   322,   323,    -1,     8,     9,
      10,    11,    12,    -1,    14,    15,   333,    17,    18,    -1,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    -1,    32,    -1,    34,    -1,    36,    37,    38,    39,
      -1,    -1,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    59,
      60,    -1,    62,    -1,    -1,    -1,    66,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    75,    76,    77,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    99,
      -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,    -1,
      -1,   131,    -1,   133,    -1,    -1,    -1,    -1,   138,    -1,
      -1,    -1,   142,    -1,   144,   145,    -1,   147,    -1,    -1,
      -1,   151,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   163,    -1,   165,    -1,    -1,    -1,    -1,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,   189,
      -1,   191,   192,   193,    -1,    -1,   196,    -1,    -1,    -1,
     200,   201,   202,    -1,   204,    -1,    -1,   207,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   216,   217,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     230,   231,    -1,   233,    -1,    -1,   236,    -1,    -1,   239,
      -1,    -1,    -1,    -1,    -1,   245,   246,    -1,    -1,    -1,
      -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,   259,
     260,   261,    -1,   263,    -1,    -1,    -1,   267,    -1,    -1,
      -1,    -1,    -1,   273,    -1,    -1,    -1,   277,    -1,    -1,
      -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,   289,
     290,   291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     300,    -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,     1,   319,
     320,   321,   322,   323,    -1,     8,     9,    10,    11,    12,
      -1,    14,    15,   333,    17,    18,    -1,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    -1,    32,
      -1,    34,    -1,    36,    37,    38,    39,    -1,    -1,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    -1,    -1,
      -1,    -1,    55,    -1,    -1,    58,    59,    60,    -1,    62,
      -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    75,    76,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    96,    -1,    -1,    99,    -1,    -1,    -1,
      -1,   104,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,    -1,    -1,   131,    -1,
     133,    -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,   142,
      -1,   144,   145,    -1,   147,    -1,    -1,    -1,   151,   152,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     163,    -1,   165,    -1,    -1,    -1,    -1,   170,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   187,    -1,   189,    -1,   191,   192,
     193,    -1,    -1,   196,    -1,    -1,    -1,   200,   201,   202,
      -1,   204,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   216,   217,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,   231,    -1,
     233,    -1,    -1,   236,    -1,    -1,   239,    -1,    -1,    -1,
      -1,    -1,   245,   246,    -1,    -1,    -1,    -1,    -1,    -1,
     253,    -1,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,
     263,    -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,
     273,    -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,
      -1,   284,    -1,    -1,    -1,    -1,   289,   290,   291,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,   302,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,
      -1,    -1,    -1,    -1,    -1,     1,   319,   320,   321,   322,
     323,    -1,     8,     9,    10,    11,    12,    -1,    14,    15,
     333,    17,    18,    -1,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    -1,    32,    -1,    34,    -1,
      36,    37,    38,    39,    -1,    -1,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    -1,    -1,    -1,    -1,    55,
      -1,    -1,    58,    59,    60,    -1,    62,    -1,    -1,    -1,
      66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,
      76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      96,    -1,    -1,    99,    -1,    -1,    -1,    -1,   104,    -1,
      -1,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,
     126,   127,   128,    -1,    -1,   131,    -1,   133,    -1,    -1,
      -1,    -1,   138,    -1,    -1,    -1,   142,    -1,   144,   145,
      -1,   147,    -1,    -1,    -1,   151,   152,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,   165,
      -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   187,    -1,   189,    -1,   191,   192,   193,    -1,    -1,
     196,    -1,    -1,    -1,   200,   201,   202,    -1,   204,    -1,
      -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     216,   217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   230,   231,    -1,   233,    -1,    -1,
     236,    -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,   245,
     246,    -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,
      -1,    -1,    -1,   259,   260,   261,    -1,   263,    -1,    -1,
      -1,   267,    -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,
      -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,
      -1,    -1,    -1,   289,   290,   291,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   300,    -1,   302,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,
      -1,    -1,     1,   319,   320,   321,   322,   323,    -1,     8,
       9,    10,    11,    12,    -1,    14,    15,   333,    17,    18,
      -1,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    -1,    32,    -1,    34,    -1,    36,    37,    38,
      39,    -1,    -1,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    -1,    -1,    -1,    -1,    55,    -1,    -1,    58,
      59,    60,    -1,    62,    -1,    -1,    -1,    66,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    77,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,
      99,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,
      -1,    -1,   131,    -1,   133,    -1,    -1,    -1,    -1,   138,
      -1,    -1,    -1,   142,    -1,   144,   145,    -1,   147,    -1,
      -1,    -1,   151,   152,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,    -1,   165,    -1,    -1,    -1,
      -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,
     189,    -1,   191,   192,   193,    -1,    -1,   196,    -1,    -1,
      -1,   200,   201,   202,    -1,   204,    -1,    -1,   207,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,   217,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   230,   231,    -1,   233,    -1,    -1,   236,    -1,    -1,
     239,    -1,    -1,    -1,    -1,    -1,   245,   246,    -1,    -1,
      -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,
     259,   260,   261,    -1,   263,    -1,    -1,    -1,   267,    -1,
      -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,   277,    -1,
      -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,
     289,   290,   291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   300,    -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,     1,
     319,   320,   321,   322,   323,    -1,     8,     9,    10,    11,
      12,    -1,    14,    15,   333,    17,    18,    -1,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    -1,
      32,    -1,    34,    -1,    36,    37,    38,    39,    -1,    -1,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    -1,
      -1,    -1,    -1,    55,    -1,    -1,    58,    59,    60,    -1,
      62,    -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    96,    -1,    -1,    99,    -1,    -1,
      -1,    -1,   104,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,   126,   127,   128,    -1,    -1,   131,
      -1,   133,    -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,
     142,    -1,   144,   145,    -1,   147,    -1,    -1,    -1,   151,
     152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,    -1,   165,    -1,    -1,    -1,    -1,   170,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   187,    -1,   189,    -1,   191,
     192,   193,    -1,    -1,   196,    -1,    -1,    -1,   200,   201,
     202,    -1,   204,    -1,    -1,   207,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   216,   217,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,   231,
      -1,   233,    -1,    -1,   236,    -1,    -1,   239,    -1,    -1,
      -1,    -1,    -1,   245,   246,    -1,    -1,    -1,    -1,    -1,
      -1,   253,    -1,    -1,    -1,    -1,    -1,   259,   260,   261,
      -1,   263,    -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,
      -1,   273,    -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,
      -1,    -1,   284,    -1,    -1,    -1,    -1,   289,   290,   291,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,
     302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     312,    -1,    -1,    -1,    -1,    -1,     1,   319,   320,   321,
     322,   323,    -1,     8,     9,    10,    11,    12,    -1,    14,
      15,   333,    17,    18,    -1,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    -1,    32,    -1,    34,
      -1,    36,    37,    38,    39,    -1,    -1,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    -1,    -1,    -1,    -1,
      55,    -1,    -1,    58,    59,    60,    -1,    62,    -1,    -1,
      -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      75,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    96,    -1,    -1,    99,    -1,    -1,    -1,    -1,   104,
      -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,   126,   127,   128,    -1,    -1,   131,    -1,   133,    -1,
      -1,    -1,    -1,   138,    -1,    -1,    -1,   142,    -1,   144,
     145,    -1,   147,    -1,    -1,    -1,   151,   152,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,
     165,    -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   187,    -1,   189,    -1,   191,   192,   193,    -1,
      -1,   196,    -1,    -1,    -1,   200,   201,   202,    -1,   204,
      -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   216,   217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   230,   231,    -1,   233,    -1,
      -1,   236,    -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,
     245,   246,    -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,
      -1,    -1,    -1,    -1,   259,   260,   261,    -1,   263,    -1,
      -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,   273,    -1,
      -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,   284,
      -1,    -1,    -1,    -1,   289,   290,   291,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   300,    -1,   302,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,
      -1,    -1,    -1,     1,   319,   320,   321,   322,   323,    -1,
       8,     9,    10,    11,    12,    -1,    14,    15,   333,    17,
      18,    -1,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    -1,    32,    -1,    34,    -1,    36,    37,
      38,    39,    -1,    -1,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    -1,    -1,    -1,    -1,    55,    -1,    -1,
      58,    59,    60,    -1,    62,    -1,    -1,    -1,    66,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,
      -1,    99,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,
     108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,
     128,    -1,    -1,   131,    -1,   133,    -1,    -1,    -1,    -1,
     138,    -1,    -1,    -1,   142,    -1,   144,   145,    -1,   147,
      -1,    -1,    -1,   151,   152,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,    -1,   165,    -1,    -1,
      -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,
      -1,   189,    -1,   191,   192,   193,    -1,    -1,   196,    -1,
      -1,    -1,   200,   201,   202,    -1,   204,    -1,    -1,   207,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,   217,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   230,   231,    -1,   233,    -1,    -1,   236,    -1,
      -1,   239,    -1,    -1,    -1,    -1,    -1,   245,   246,    -1,
      -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,
      -1,   259,   260,   261,    -1,   263,    -1,    -1,    -1,   267,
      -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,   277,
      -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,
      -1,   289,   290,   291,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   300,    -1,   302,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,
       1,   319,   320,   321,   322,   323,    -1,     8,     9,    10,
      11,    12,    -1,    14,    15,   333,    17,    18,    -1,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      -1,    32,    -1,    34,    -1,    36,    37,    38,    39,    -1,
      -1,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    59,    60,
      -1,    62,    -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    75,    76,    77,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    99,    -1,
      -1,    -1,    -1,   104,    -1,    -1,   107,   108,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,    -1,    -1,
     131,    -1,   133,    -1,    -1,    -1,    -1,   138,    -1,    -1,
      -1,   142,    -1,   144,   145,    -1,   147,    -1,    -1,    -1,
     151,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,   165,    -1,    -1,    -1,    -1,   170,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,   189,    -1,
     191,   192,   193,    -1,    -1,   196,    -1,    -1,    -1,   200,
     201,   202,    -1,   204,    -1,    -1,   207,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   216,   217,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,
     231,    -1,   233,    -1,    -1,   236,    -1,    -1,   239,    -1,
      -1,    -1,    -1,    -1,   245,   246,    -1,    -1,    -1,    -1,
      -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,   259,   260,
     261,    -1,   263,    -1,    -1,    -1,   267,    -1,    -1,    -1,
      -1,    -1,   273,    -1,    -1,    -1,   277,    -1,    -1,    -1,
      -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,   289,   290,
     291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,
      -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   312,    -1,    -1,    -1,    -1,    -1,     1,   319,   320,
     321,   322,   323,    -1,     8,     9,    10,    11,    12,    -1,
      14,    15,   333,    17,    18,    -1,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    -1,    32,    -1,
      34,    -1,    36,    37,    38,    39,    -1,    -1,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    -1,    -1,    -1,
      -1,    55,    -1,    -1,    58,    59,    60,    -1,    62,    -1,
      -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    96,    -1,    -1,    99,    -1,    -1,    -1,    -1,
     104,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,    -1,    -1,   131,    -1,   133,
      -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,   142,    -1,
     144,   145,    -1,   147,    -1,    -1,    -1,   151,   152,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
      -1,   165,    -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   187,    -1,   189,    -1,   191,   192,   193,
      -1,    -1,   196,    -1,    -1,    -1,   200,   201,   202,    -1,
     204,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   216,   217,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   230,   231,    -1,   233,
      -1,    -1,   236,    -1,    -1,   239,    -1,    -1,    -1,    -1,
      -1,   245,   246,    -1,    -1,    -1,    -1,    -1,    -1,   253,
      -1,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,   263,
      -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,   273,
      -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,
     284,    -1,    -1,    -1,    -1,   289,   290,   291,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,   302,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,
      -1,    -1,    -1,    -1,     1,   319,   320,   321,   322,   323,
      -1,     8,     9,    10,    11,    12,    -1,    14,    15,   333,
      17,    18,    -1,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    -1,    32,    -1,    34,    -1,    36,
      37,    38,    39,    -1,    -1,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    -1,    -1,    -1,    -1,    55,    -1,
      -1,    58,    59,    60,    -1,    62,    -1,    -1,    -1,    66,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,
      -1,    -1,    99,    -1,    -1,    -1,    -1,   104,    -1,    -1,
     107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,    -1,    -1,   131,    -1,   133,    -1,    -1,    -1,
      -1,   138,    -1,    -1,    -1,   142,    -1,   144,   145,    -1,
     147,    -1,    -1,    -1,   151,   152,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,   165,    -1,
      -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     187,    -1,   189,    -1,   191,   192,   193,    -1,    -1,   196,
      -1,    -1,    -1,   200,   201,   202,    -1,   204,    -1,    -1,
     207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,
     217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   230,   231,    -1,   233,    -1,    -1,   236,
      -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,   245,   246,
      -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,
      -1,    -1,   259,   260,   261,    -1,   263,    -1,    -1,    -1,
     267,    -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,
     277,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,
      -1,    -1,   289,   290,   291,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   300,    -1,   302,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,
      -1,     1,   319,   320,   321,   322,   323,    -1,     8,     9,
      10,    11,    12,    -1,    14,    15,   333,    17,    18,    -1,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    -1,    32,    -1,    34,    -1,    36,    37,    38,    39,
      -1,    -1,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    59,
      60,    -1,    62,    -1,    -1,    -1,    66,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    75,    76,    77,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    99,
      -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,    -1,
      -1,   131,    -1,   133,    -1,    -1,    -1,    -1,   138,    -1,
      -1,    -1,   142,    -1,   144,   145,    -1,   147,    -1,    -1,
      -1,   151,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   163,    -1,   165,    -1,    -1,    -1,    -1,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,   189,
      -1,   191,   192,   193,    -1,    -1,   196,    -1,    -1,    -1,
     200,   201,   202,    -1,   204,    -1,    -1,   207,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   216,   217,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     230,   231,    -1,   233,    -1,    -1,   236,    -1,    -1,   239,
      -1,    -1,    -1,    -1,    -1,   245,   246,    -1,    -1,    -1,
      -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,   259,
     260,   261,    -1,   263,    -1,    -1,    -1,   267,    -1,    -1,
      -1,    -1,    -1,   273,    -1,    -1,    -1,   277,    -1,    -1,
      -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,   289,
     290,   291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     300,    -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,     1,   319,
     320,   321,   322,   323,    -1,     8,     9,    10,    11,    12,
      -1,    14,    15,   333,    17,    18,    -1,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    -1,    32,
      -1,    34,    -1,    36,    37,    38,    39,    -1,    -1,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    -1,    -1,
      -1,    -1,    55,    -1,    -1,    58,    59,    60,    -1,    62,
      -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    75,    76,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    96,    -1,    -1,    99,    -1,    -1,    -1,
      -1,   104,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,    -1,    -1,   131,    -1,
     133,    -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,   142,
      -1,   144,   145,    -1,   147,    -1,    -1,    -1,   151,   152,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     163,    -1,   165,    -1,    -1,    -1,    -1,   170,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   187,    -1,   189,    -1,   191,   192,
     193,    -1,    -1,   196,    -1,    -1,    -1,   200,   201,   202,
      -1,   204,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   216,   217,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,   231,    -1,
     233,    -1,    -1,   236,    -1,    -1,   239,    -1,    -1,    -1,
      -1,    -1,   245,   246,    -1,    -1,    -1,    -1,    -1,    -1,
     253,    -1,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,
     263,    -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,
     273,    -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,
      -1,   284,    -1,    -1,    -1,    -1,   289,   290,   291,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,   302,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,
      -1,    -1,    -1,    -1,    -1,     1,   319,   320,   321,   322,
     323,    -1,     8,     9,    10,    11,    12,    -1,    14,    15,
     333,    17,    18,    -1,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    -1,    32,    -1,    34,    -1,
      36,    37,    38,    39,    -1,    -1,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    -1,    -1,    -1,    -1,    55,
      -1,    -1,    58,    59,    60,    -1,    62,    -1,    -1,    -1,
      66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,
      76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      96,    -1,    -1,    99,    -1,    -1,    -1,    -1,   104,    -1,
      -1,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,
     126,   127,   128,    -1,    -1,   131,    -1,   133,    -1,    -1,
      -1,    -1,   138,    -1,    -1,    -1,   142,    -1,   144,   145,
      -1,   147,    -1,    -1,    -1,   151,   152,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,   165,
      -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   187,    -1,   189,    -1,   191,   192,   193,    -1,    -1,
     196,    -1,    -1,    -1,   200,   201,   202,    -1,   204,    -1,
      -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     216,   217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   230,   231,    -1,   233,    -1,    -1,
     236,    -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,   245,
     246,    -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,
      -1,    -1,    -1,   259,   260,   261,    -1,   263,    -1,    -1,
      -1,   267,    -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,
      -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,
      -1,    -1,    -1,   289,   290,   291,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   300,    -1,   302,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,
      -1,    -1,     1,   319,   320,   321,   322,   323,    -1,     8,
       9,    10,    11,    12,    -1,    14,    15,   333,    17,    18,
      -1,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    -1,    32,    -1,    34,    -1,    36,    37,    38,
      39,    -1,    -1,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    -1,    -1,    -1,    -1,    55,    -1,    -1,    58,
      59,    60,    -1,    62,    -1,    -1,    -1,    66,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    77,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,
      99,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,
      -1,    -1,   131,    -1,   133,    -1,    -1,    -1,    -1,   138,
      -1,    -1,    -1,   142,    -1,   144,   145,    -1,   147,    -1,
      -1,    -1,   151,   152,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,    -1,   165,    -1,    -1,    -1,
      -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,
     189,    -1,   191,   192,   193,    -1,    -1,   196,    -1,    -1,
      -1,   200,   201,   202,    -1,   204,    -1,    -1,   207,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,   217,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   230,   231,    -1,   233,    -1,    -1,   236,    -1,    -1,
     239,    -1,    -1,    -1,    -1,    -1,   245,   246,    -1,    -1,
      -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,
     259,   260,   261,    -1,   263,    -1,    -1,    -1,   267,    -1,
      -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,   277,    -1,
      -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,
     289,   290,   291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   300,    -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,     1,
     319,   320,   321,   322,   323,    -1,     8,     9,    10,    11,
      12,    -1,    14,    15,   333,    17,    18,    -1,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    -1,
      32,    -1,    34,    -1,    36,    37,    38,    39,    -1,    -1,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    -1,
      -1,    -1,    -1,    55,    -1,    -1,    58,    59,    60,    -1,
      62,    -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    96,    -1,    -1,    99,    -1,    -1,
      -1,    -1,   104,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,   126,   127,   128,    -1,    -1,   131,
      -1,   133,    -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,
     142,    -1,   144,   145,    -1,   147,    -1,    -1,    -1,   151,
     152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,    -1,   165,    -1,    -1,    -1,    -1,   170,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   187,    -1,   189,    -1,   191,
     192,   193,    -1,    -1,   196,    -1,    -1,    -1,   200,   201,
     202,    -1,   204,    -1,    -1,   207,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   216,   217,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,   231,
      -1,   233,    -1,    -1,   236,    -1,    -1,   239,    -1,    -1,
      -1,    -1,    -1,   245,   246,    -1,    -1,    -1,    -1,    -1,
      -1,   253,    -1,    -1,    -1,    -1,    -1,   259,   260,   261,
      -1,   263,    -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,
      -1,   273,    -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,
      -1,    -1,   284,    -1,    -1,    -1,    -1,   289,   290,   291,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,
     302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     312,    -1,    -1,    -1,    -1,    -1,     1,   319,   320,   321,
     322,   323,    -1,     8,     9,    10,    11,    12,    -1,    14,
      15,   333,    17,    18,    -1,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    -1,    32,    -1,    34,
      -1,    36,    37,    38,    39,    -1,    -1,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    -1,    -1,    -1,    -1,
      55,    -1,    -1,    58,    59,    60,    -1,    62,    -1,    -1,
      -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      75,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    96,    -1,    -1,    99,    -1,    -1,    -1,    -1,   104,
      -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,   126,   127,   128,    -1,    -1,   131,    -1,   133,    -1,
      -1,    -1,    -1,   138,    -1,    -1,    -1,   142,    -1,   144,
     145,    -1,   147,    -1,    -1,    -1,   151,   152,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,
     165,    -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   187,    -1,   189,    -1,   191,   192,   193,    -1,
      -1,   196,    -1,    -1,    -1,   200,   201,   202,    -1,   204,
      -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   216,   217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   230,   231,    -1,   233,    -1,
      -1,   236,    -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,
     245,   246,    -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,
      -1,    -1,    -1,    -1,   259,   260,   261,    -1,   263,    -1,
      -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,   273,    -1,
      -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,   284,
      -1,    -1,    -1,    -1,   289,   290,   291,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   300,    -1,   302,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,
      -1,    -1,    -1,     1,   319,   320,   321,   322,   323,    -1,
       8,     9,    10,    11,    12,    -1,    14,    15,   333,    17,
      18,    -1,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    -1,    32,    -1,    34,    -1,    36,    37,
      38,    39,    -1,    -1,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    -1,    -1,    -1,    -1,    55,    -1,    -1,
      58,    59,    60,    -1,    62,    -1,    -1,    -1,    66,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,
      -1,    99,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,
     108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,
     128,    -1,    -1,   131,    -1,   133,    -1,    -1,    -1,    -1,
     138,    -1,    -1,    -1,   142,    -1,   144,   145,    -1,   147,
      -1,    -1,    -1,   151,   152,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,    -1,   165,    -1,    -1,
      -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,
      -1,   189,    -1,   191,   192,   193,    -1,    -1,   196,    -1,
      -1,    -1,   200,   201,   202,    -1,   204,    -1,    -1,   207,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,   217,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   230,   231,    -1,   233,    -1,    -1,   236,    -1,
      -1,   239,    -1,    -1,    -1,    -1,    -1,   245,   246,    -1,
      -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,
      -1,   259,   260,   261,    -1,   263,    -1,    -1,    -1,   267,
      -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,   277,
      -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,
      -1,   289,   290,   291,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   300,    -1,   302,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,
       1,   319,   320,   321,   322,   323,    -1,     8,     9,    10,
      11,    12,    -1,    14,    15,   333,    17,    18,    -1,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      -1,    32,    -1,    34,    -1,    36,    37,    38,    39,    -1,
      -1,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    59,    60,
      -1,    62,    -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    75,    76,    77,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    99,    -1,
      -1,    -1,    -1,   104,    -1,    -1,   107,   108,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,    -1,    -1,
     131,    -1,   133,    -1,    -1,    -1,    -1,   138,    -1,    -1,
      -1,   142,    -1,   144,   145,    -1,   147,    -1,    -1,    -1,
     151,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,   165,    -1,    -1,    -1,    -1,   170,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,   189,    -1,
     191,   192,   193,    -1,    -1,   196,    -1,    -1,    -1,   200,
     201,   202,    -1,   204,    -1,    -1,   207,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   216,   217,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,
     231,    -1,   233,    -1,    -1,   236,    -1,    -1,   239,    -1,
      -1,    -1,    -1,    -1,   245,   246,    -1,    -1,    -1,    -1,
      -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,   259,   260,
     261,    -1,   263,    -1,    -1,    -1,   267,    -1,    -1,    -1,
      -1,    -1,   273,    -1,    -1,    -1,   277,    -1,    -1,    -1,
      -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,   289,   290,
     291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,
      -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   312,    -1,    -1,    -1,    -1,    -1,     1,   319,   320,
     321,   322,   323,    -1,     8,     9,    10,    11,    12,    -1,
      14,    15,   333,    17,    18,    -1,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    -1,    32,    -1,
      34,    -1,    36,    37,    38,    39,    -1,    -1,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    -1,    -1,    -1,
      -1,    55,    -1,    -1,    58,    59,    60,    -1,    62,    -1,
      -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    96,    -1,    -1,    99,    -1,    -1,    -1,    -1,
     104,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,    -1,    -1,   131,    -1,   133,
      -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,   142,    -1,
     144,   145,    -1,   147,    -1,    -1,    -1,   151,   152,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
      -1,   165,    -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   187,    -1,   189,    -1,   191,   192,   193,
      -1,    -1,   196,    -1,    -1,    -1,   200,   201,   202,    -1,
     204,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   216,   217,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   230,   231,    -1,   233,
      -1,    -1,   236,    -1,    -1,   239,    -1,    -1,    -1,    -1,
      -1,   245,   246,    -1,    -1,    -1,    -1,    -1,    -1,   253,
      -1,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,   263,
      -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,   273,
      -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,
     284,    -1,    -1,    -1,    -1,   289,   290,   291,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,   302,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,
      -1,    -1,    -1,    -1,     1,   319,   320,   321,   322,   323,
      -1,     8,     9,    10,    11,    12,    -1,    14,    15,   333,
      17,    18,    -1,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    -1,    32,    -1,    34,    -1,    36,
      37,    38,    39,    -1,    -1,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    -1,    -1,    -1,    -1,    55,    -1,
      -1,    58,    59,    60,    -1,    62,    -1,    -1,    -1,    66,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,
      -1,    -1,    99,    -1,    -1,    -1,    -1,   104,    -1,    -1,
     107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,    -1,    -1,   131,    -1,   133,    -1,    -1,    -1,
      -1,   138,    -1,    -1,    -1,   142,    -1,   144,   145,    -1,
     147,    -1,    -1,    -1,   151,   152,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,   165,    -1,
      -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     187,    -1,   189,    -1,   191,   192,   193,    -1,    -1,   196,
      -1,    -1,    -1,   200,   201,   202,    -1,   204,    -1,    -1,
     207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,
     217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   230,   231,    -1,   233,    -1,    -1,   236,
      -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,   245,   246,
      -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,
      -1,    -1,   259,   260,   261,    -1,   263,    -1,    -1,    -1,
     267,    -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,
     277,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,
      -1,    -1,   289,   290,   291,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   300,    -1,   302,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,
      -1,     1,   319,   320,   321,   322,   323,    -1,     8,     9,
      10,    11,    12,    -1,    14,    15,   333,    17,    18,    -1,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    -1,    32,    -1,    34,    -1,    36,    37,    38,    39,
      -1,    -1,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    59,
      60,    -1,    62,    -1,    -1,    -1,    66,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    75,    76,    77,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    99,
      -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,    -1,
      -1,   131,    -1,   133,    -1,    -1,    -1,    -1,   138,    -1,
      -1,    -1,   142,    -1,   144,   145,    -1,   147,    -1,    -1,
      -1,   151,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   163,    -1,   165,    -1,    -1,    -1,    -1,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,   189,
      -1,   191,   192,   193,    -1,    -1,   196,    -1,    -1,    -1,
     200,   201,   202,    -1,   204,    -1,    -1,   207,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   216,   217,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     230,   231,    -1,   233,    -1,    -1,   236,    -1,    -1,   239,
      -1,    -1,    -1,    -1,    -1,   245,   246,    -1,    -1,    -1,
      -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,   259,
     260,   261,    -1,   263,    -1,    -1,    -1,   267,    -1,    -1,
      -1,    -1,    -1,   273,    -1,    -1,    -1,   277,    -1,    -1,
      -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,   289,
     290,   291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     300,    -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,     1,   319,
     320,   321,   322,   323,    -1,     8,     9,    10,    11,    12,
      -1,    14,    15,   333,    17,    18,    -1,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    -1,    32,
      -1,    34,    -1,    36,    37,    38,    39,    -1,    -1,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    -1,    -1,
      -1,    -1,    55,    -1,    -1,    58,    59,    60,    -1,    62,
      -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    75,    76,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    96,    -1,    -1,    99,    -1,    -1,    -1,
      -1,   104,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,    -1,    -1,   131,    -1,
     133,    -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,   142,
      -1,   144,   145,    -1,   147,    -1,    -1,    -1,   151,   152,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     163,    -1,   165,    -1,    -1,    -1,    -1,   170,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   187,    -1,   189,    -1,   191,   192,
     193,    -1,    -1,   196,    -1,    -1,    -1,   200,   201,   202,
      -1,   204,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   216,   217,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,   231,    -1,
     233,    -1,    -1,   236,    -1,    -1,   239,    -1,    -1,    -1,
      -1,    -1,   245,   246,    -1,    -1,    -1,    -1,    -1,    -1,
     253,    -1,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,
     263,    -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,
     273,    -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,
      -1,   284,    -1,    -1,    -1,    -1,   289,   290,   291,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,   302,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,
      -1,    -1,    -1,    -1,    -1,     1,   319,   320,   321,   322,
     323,    -1,     8,     9,    10,    11,    12,    -1,    14,    15,
     333,    17,    18,    -1,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    -1,    32,    -1,    34,    -1,
      36,    37,    38,    39,    -1,    -1,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    -1,    -1,    -1,    -1,    55,
      -1,    -1,    58,    59,    60,    -1,    62,    -1,    -1,    -1,
      66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,
      76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      96,    -1,    -1,    99,    -1,    -1,    -1,    -1,   104,    -1,
      -1,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,
     126,   127,   128,    -1,    -1,   131,    -1,   133,    -1,    -1,
      -1,    -1,   138,    -1,    -1,    -1,   142,    -1,   144,   145,
      -1,   147,    -1,    -1,    -1,   151,   152,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,   165,
      -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   187,    -1,   189,    -1,   191,   192,   193,    -1,    -1,
     196,    -1,    -1,    -1,   200,   201,   202,    -1,   204,    -1,
      -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     216,   217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   230,   231,    -1,   233,    -1,    -1,
     236,    -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,   245,
     246,    -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,
      -1,    -1,    -1,   259,   260,   261,    -1,   263,    -1,    -1,
      -1,   267,    -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,
      -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,
      -1,    -1,    -1,   289,   290,   291,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   300,    -1,   302,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,
      -1,    -1,     1,   319,   320,   321,   322,   323,    -1,     8,
       9,    10,    11,    12,    -1,    14,    15,   333,    17,    18,
      -1,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    -1,    32,    -1,    34,    -1,    36,    37,    38,
      39,    -1,    -1,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    -1,    -1,    -1,    -1,    55,    -1,    -1,    58,
      59,    60,    -1,    62,    -1,    -1,    -1,    66,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    77,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,
      99,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,
      -1,    -1,   131,    -1,   133,    -1,    -1,    -1,    -1,   138,
      -1,    -1,    -1,   142,    -1,   144,   145,    -1,   147,    -1,
      -1,    -1,   151,   152,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,    -1,   165,    -1,    -1,    -1,
      -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,
     189,    -1,   191,   192,   193,    -1,    -1,   196,    -1,    -1,
      -1,   200,   201,   202,    -1,   204,    -1,    -1,   207,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,   217,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   230,   231,    -1,   233,    -1,    -1,   236,    -1,    -1,
     239,    -1,    -1,    -1,    -1,    -1,   245,   246,    -1,    -1,
      -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,
     259,   260,   261,    -1,   263,    -1,    -1,    -1,   267,    -1,
      -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,   277,    -1,
      -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,
     289,   290,   291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   300,    -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,     1,
     319,   320,   321,   322,   323,    -1,     8,     9,    10,    11,
      12,    -1,    14,    15,   333,    17,    18,    -1,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    -1,
      32,    -1,    34,    -1,    36,    37,    38,    39,    -1,    -1,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    -1,
      -1,    -1,    -1,    55,    -1,    -1,    58,    59,    60,    -1,
      62,    -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    96,    -1,    -1,    99,    -1,    -1,
      -1,    -1,   104,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,   126,   127,   128,    -1,    -1,   131,
      -1,   133,    -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,
     142,    -1,   144,   145,    -1,   147,    -1,    -1,    -1,   151,
     152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,    -1,   165,    -1,    -1,    -1,    -1,   170,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   187,    -1,   189,    -1,   191,
     192,   193,    -1,    -1,   196,    -1,    -1,    -1,   200,   201,
     202,    -1,   204,    -1,    -1,   207,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   216,   217,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,   231,
      -1,   233,    -1,    -1,   236,    -1,    -1,   239,    -1,    -1,
      -1,    -1,    -1,   245,   246,    -1,    -1,    -1,    -1,    -1,
      -1,   253,    -1,    -1,    -1,    -1,    -1,   259,   260,   261,
      -1,   263,    -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,
      -1,   273,    -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,
      -1,    -1,   284,    -1,    -1,    -1,    -1,   289,   290,   291,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,
     302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     312,    -1,    -1,    -1,    -1,    -1,     1,   319,   320,   321,
     322,   323,    -1,     8,     9,    10,    11,    12,    -1,    14,
      15,   333,    17,    18,    -1,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    -1,    32,    -1,    34,
      -1,    36,    37,    38,    39,    -1,    -1,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    -1,    -1,    -1,    -1,
      55,    -1,    -1,    58,    59,    60,    -1,    62,    -1,    -1,
      -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      75,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    96,    -1,    -1,    99,    -1,    -1,    -1,    -1,   104,
      -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,   126,   127,   128,    -1,    -1,   131,    -1,   133,    -1,
      -1,    -1,    -1,   138,    -1,    -1,    -1,   142,    -1,   144,
     145,    -1,   147,    -1,    -1,    -1,   151,   152,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,
     165,    -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   187,    -1,   189,    -1,   191,   192,   193,    -1,
      -1,   196,    -1,    -1,    -1,   200,   201,   202,    -1,   204,
      -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   216,   217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   230,   231,    -1,   233,    -1,
      -1,   236,    -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,
     245,   246,    -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,
      -1,    -1,    -1,    -1,   259,   260,   261,    -1,   263,    -1,
      -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,   273,    -1,
      -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,   284,
      -1,    -1,    -1,    -1,   289,   290,   291,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   300,    -1,   302,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,
      -1,    -1,    -1,     1,   319,   320,   321,   322,   323,    -1,
       8,     9,    10,    11,    12,    -1,    14,    15,   333,    17,
      18,    -1,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    -1,    32,    -1,    34,    -1,    36,    37,
      38,    39,    -1,    -1,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    -1,    -1,    -1,    -1,    55,    -1,    -1,
      58,    59,    60,    -1,    62,    -1,    -1,    -1,    66,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,
      -1,    99,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,
     108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,
     128,    -1,    -1,   131,    -1,   133,    -1,    -1,    -1,    -1,
     138,    -1,    -1,    -1,   142,    -1,   144,   145,    -1,   147,
      -1,    -1,    -1,   151,   152,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,    -1,   165,    -1,    -1,
      -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,
      -1,   189,    -1,   191,   192,   193,    -1,    -1,   196,    -1,
      -1,    -1,   200,   201,   202,    -1,   204,    -1,    -1,   207,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,   217,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   230,   231,    -1,   233,    -1,    -1,   236,    -1,
      -1,   239,    -1,    -1,    -1,    -1,    -1,   245,   246,    -1,
      -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,
      -1,   259,   260,   261,    -1,   263,    -1,    -1,    -1,   267,
      -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,   277,
      -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,
      -1,   289,   290,   291,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   300,    -1,   302,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,
       1,   319,   320,   321,   322,   323,    -1,     8,     9,    10,
      11,    12,    -1,    14,    15,   333,    17,    18,    -1,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      -1,    32,    -1,    34,    -1,    36,    37,    38,    39,    -1,
      -1,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    59,    60,
      -1,    62,    -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    75,    76,    77,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    99,    -1,
      -1,    -1,    -1,   104,    -1,    -1,   107,   108,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,    -1,    -1,
     131,    -1,   133,    -1,    -1,    -1,    -1,   138,    -1,    -1,
      -1,   142,    -1,   144,   145,    -1,   147,    -1,    -1,    -1,
     151,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,   165,    -1,    -1,    -1,    -1,   170,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,   189,    -1,
     191,   192,   193,    -1,    -1,   196,    -1,    -1,    -1,   200,
     201,   202,    -1,   204,    -1,    -1,   207,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   216,   217,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,
     231,    -1,   233,    -1,    -1,   236,    -1,    -1,   239,    -1,
      -1,    -1,    -1,    -1,   245,   246,    -1,    -1,    -1,    -1,
      -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,   259,   260,
     261,    -1,   263,    -1,    -1,    -1,   267,    -1,    -1,    -1,
      -1,    -1,   273,    -1,    -1,    -1,   277,    -1,    -1,    -1,
      -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,   289,   290,
     291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,
      -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   312,    -1,    -1,    -1,    -1,    -1,     1,   319,   320,
     321,   322,   323,    -1,     8,     9,    10,    11,    12,    -1,
      14,    15,   333,    17,    18,    -1,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    -1,    32,    -1,
      34,    -1,    36,    37,    38,    39,    -1,    -1,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    -1,    -1,    -1,
      -1,    55,    -1,    -1,    58,    59,    60,    -1,    62,    -1,
      -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    96,    -1,    -1,    99,    -1,    -1,    -1,    -1,
     104,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,    -1,    -1,   131,    -1,   133,
      -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,   142,    -1,
     144,   145,    -1,   147,    -1,    -1,    -1,   151,   152,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
      -1,   165,    -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   187,    -1,   189,    -1,   191,   192,   193,
      -1,    -1,   196,    -1,    -1,    -1,   200,   201,   202,    -1,
     204,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   216,   217,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   230,   231,    -1,   233,
      -1,    -1,   236,    -1,    -1,   239,    -1,    -1,    -1,    -1,
      -1,   245,   246,    -1,    -1,    -1,    -1,    -1,    -1,   253,
      -1,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,   263,
      -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,   273,
      -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,
     284,    -1,    -1,    -1,    -1,   289,   290,   291,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,   302,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,
      -1,    -1,    -1,    -1,     1,   319,   320,   321,   322,   323,
      -1,     8,     9,    10,    11,    12,    -1,    14,    15,   333,
      17,    18,    -1,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    -1,    32,    -1,    34,    -1,    36,
      37,    38,    39,    -1,    -1,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    -1,    -1,    -1,    -1,    55,    -1,
      -1,    58,    59,    60,    -1,    62,    -1,    -1,    -1,    66,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,
      -1,    -1,    99,    -1,    -1,    -1,    -1,   104,    -1,    -1,
     107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,    -1,    -1,   131,    -1,   133,    -1,    -1,    -1,
      -1,   138,    -1,    -1,    -1,   142,    -1,   144,   145,    -1,
     147,    -1,    -1,    -1,   151,   152,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,   165,    -1,
      -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     187,    -1,   189,    -1,   191,   192,   193,    -1,    -1,   196,
      -1,    -1,    -1,   200,   201,   202,    -1,   204,    -1,    -1,
     207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,
     217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   230,   231,    -1,   233,    -1,    -1,   236,
      -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,   245,   246,
      -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,
      -1,    -1,   259,   260,   261,    -1,   263,    -1,    -1,    -1,
     267,    -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,
     277,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,
      -1,    -1,   289,   290,   291,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   300,    -1,   302,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,
      -1,     1,   319,   320,   321,   322,   323,    -1,     8,     9,
      10,    11,    12,    -1,    14,    15,   333,    17,    18,    -1,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    -1,    32,    -1,    34,    -1,    36,    37,    38,    39,
      -1,    -1,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    59,
      60,    -1,    62,    -1,    -1,    -1,    66,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    75,    76,    77,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    99,
      -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,    -1,
      -1,   131,    -1,   133,    -1,    -1,    -1,    -1,   138,    -1,
      -1,    -1,   142,    -1,   144,   145,    -1,   147,    -1,    -1,
      -1,   151,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   163,    -1,   165,    -1,    -1,    -1,    -1,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,   189,
      -1,   191,   192,   193,    -1,    -1,   196,    -1,    -1,    -1,
     200,   201,   202,    -1,   204,    -1,    -1,   207,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   216,   217,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     230,   231,    -1,   233,    -1,    -1,   236,    -1,    -1,   239,
      -1,    -1,    -1,    -1,    -1,   245,   246,    -1,    -1,    -1,
      -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,   259,
     260,   261,    -1,   263,    -1,    -1,    -1,   267,    -1,    -1,
      -1,    -1,    -1,   273,    -1,    -1,    -1,   277,    -1,    -1,
      -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,   289,
     290,   291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     300,    -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,     1,   319,
     320,   321,   322,   323,    -1,     8,     9,    10,    11,    12,
      -1,    14,    15,   333,    17,    18,    -1,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    -1,    32,
      -1,    34,    -1,    36,    37,    38,    39,    -1,    -1,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    -1,    -1,
      -1,    -1,    55,    -1,    -1,    58,    59,    60,    -1,    62,
      -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    75,    76,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    96,    -1,    -1,    99,    -1,    -1,    -1,
      -1,   104,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,    -1,    -1,   131,    -1,
     133,    -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,   142,
      -1,   144,   145,    -1,   147,    -1,    -1,    -1,   151,   152,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     163,    -1,   165,    -1,    -1,    -1,    -1,   170,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   187,    -1,   189,    -1,   191,   192,
     193,    -1,    -1,   196,    -1,    -1,    -1,   200,   201,   202,
      -1,   204,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   216,   217,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,   231,    -1,
     233,    -1,    -1,   236,    -1,    -1,   239,    -1,    -1,    -1,
      -1,    -1,   245,   246,    -1,    -1,    -1,    -1,    -1,    -1,
     253,    -1,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,
     263,    -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,
     273,    -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,
      -1,   284,    -1,    -1,    -1,    -1,   289,   290,   291,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,   302,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,
      -1,    -1,    -1,    -1,    -1,     1,   319,   320,   321,   322,
     323,    -1,     8,     9,    10,    11,    12,    -1,    14,    15,
     333,    17,    18,    -1,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    -1,    32,    -1,    34,    -1,
      36,    37,    38,    39,    -1,    -1,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    -1,    -1,    -1,    -1,    55,
      -1,    -1,    58,    59,    60,    -1,    62,    -1,    -1,    -1,
      66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,
      76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      96,    -1,    -1,    99,    -1,    -1,    -1,    -1,   104,    -1,
      -1,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,
     126,   127,   128,    -1,    -1,   131,    -1,   133,    -1,    -1,
      -1,    -1,   138,    -1,    -1,    -1,   142,    -1,   144,   145,
      -1,   147,    -1,    -1,    -1,   151,   152,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,   165,
      -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   187,    -1,   189,    -1,   191,   192,   193,    -1,    -1,
     196,    -1,    -1,    -1,   200,   201,   202,    -1,   204,    -1,
      -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     216,   217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   230,   231,    -1,   233,    -1,    -1,
     236,    -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,   245,
     246,    -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,
      -1,    -1,    -1,   259,   260,   261,    -1,   263,    -1,    -1,
      -1,   267,    -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,
      -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,
      -1,    -1,    -1,   289,   290,   291,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   300,    -1,   302,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,
      -1,    -1,     1,   319,   320,   321,   322,   323,    -1,     8,
       9,    10,    11,    12,    -1,    14,    15,   333,    17,    18,
      -1,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    -1,    32,    -1,    34,    -1,    36,    37,    38,
      39,    -1,    -1,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    -1,    -1,    -1,    -1,    55,    -1,    -1,    58,
      59,    60,    -1,    62,    -1,    -1,    -1,    66,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    77,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,
      99,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,
      -1,    -1,   131,    -1,   133,    -1,    -1,    -1,    -1,   138,
      -1,    -1,    -1,   142,    -1,   144,   145,    -1,   147,    -1,
      -1,    -1,   151,   152,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,    -1,   165,    -1,    -1,    -1,
      -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,
     189,    -1,   191,   192,   193,    -1,    -1,   196,    -1,    -1,
      -1,   200,   201,   202,    -1,   204,    -1,    -1,   207,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,   217,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   230,   231,    -1,   233,    -1,    -1,   236,    -1,    -1,
     239,    -1,    -1,    -1,    -1,    -1,   245,   246,    -1,    -1,
      -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,
     259,   260,   261,    -1,   263,    -1,    -1,    -1,   267,    -1,
      -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,   277,    -1,
      -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,
     289,   290,   291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   300,    -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,     1,
     319,   320,   321,   322,   323,    -1,     8,     9,    10,    11,
      12,    -1,    14,    15,   333,    17,    18,    -1,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    -1,
      32,    -1,    34,    -1,    36,    37,    38,    39,    -1,    -1,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    -1,
      -1,    -1,    -1,    55,    -1,    -1,    58,    59,    60,    -1,
      62,    -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    96,    -1,    -1,    99,    -1,    -1,
      -1,    -1,   104,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,   126,   127,   128,    -1,    -1,   131,
      -1,   133,    -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,
     142,    -1,   144,   145,    -1,   147,    -1,    -1,    -1,   151,
     152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,    -1,   165,    -1,    -1,    -1,    -1,   170,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   187,    -1,   189,    -1,   191,
     192,   193,    -1,    -1,   196,    -1,    -1,    -1,   200,   201,
     202,    -1,   204,    -1,    -1,   207,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   216,   217,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,   231,
      -1,   233,    -1,    -1,   236,    -1,    -1,   239,    -1,    -1,
      -1,    -1,    -1,   245,   246,    -1,    -1,    -1,    -1,    -1,
      -1,   253,    -1,    -1,    -1,    -1,    -1,   259,   260,   261,
      -1,   263,    -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,
      -1,   273,    -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,
      -1,    -1,   284,    -1,    -1,    -1,    -1,   289,   290,   291,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,
     302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     312,    -1,    -1,    -1,    -1,    -1,     1,   319,   320,   321,
     322,   323,    -1,     8,     9,    10,    11,    12,    -1,    14,
      15,   333,    17,    18,    -1,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    -1,    32,    -1,    34,
      -1,    36,    37,    38,    39,    -1,    -1,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    -1,    -1,    -1,    -1,
      55,    -1,    -1,    58,    59,    60,    -1,    62,    -1,    -1,
      -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      75,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    96,    -1,    -1,    99,    -1,    -1,    -1,    -1,   104,
      -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,   126,   127,   128,    -1,    -1,   131,    -1,   133,    -1,
      -1,    -1,    -1,   138,    -1,    -1,    -1,   142,    -1,   144,
     145,    -1,   147,    -1,    -1,    -1,   151,   152,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,
     165,    -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   187,    -1,   189,    -1,   191,   192,   193,    -1,
      -1,   196,    -1,    -1,    -1,   200,   201,   202,    -1,   204,
      -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   216,   217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   230,   231,    -1,   233,    -1,
      -1,   236,    -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,
     245,   246,    -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,
      -1,    -1,    -1,    -1,   259,   260,   261,    -1,   263,    -1,
      -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,   273,    -1,
      -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,   284,
      -1,    -1,    -1,    -1,   289,   290,   291,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   300,    -1,   302,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,
      -1,    -1,    -1,     1,   319,   320,   321,   322,   323,    -1,
       8,     9,    10,    11,    12,    -1,    14,    15,   333,    17,
      18,    -1,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    -1,    32,    -1,    34,    -1,    36,    37,
      38,    39,    -1,    -1,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    -1,    -1,    -1,    -1,    55,    -1,    -1,
      58,    59,    60,    -1,    62,    -1,    -1,    -1,    66,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,
      -1,    99,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,
     108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,
     128,    -1,    -1,   131,    -1,   133,    -1,    -1,    -1,    -1,
     138,    -1,    -1,    -1,   142,    -1,   144,   145,    -1,   147,
      -1,    -1,    -1,   151,   152,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,    -1,   165,    -1,    -1,
      -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,
      -1,   189,    -1,   191,   192,   193,    -1,    -1,   196,    -1,
      -1,    -1,   200,   201,   202,    -1,   204,    -1,    -1,   207,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,   217,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   230,   231,    -1,   233,    -1,    -1,   236,    -1,
      -1,   239,    -1,    -1,    -1,    -1,    -1,   245,   246,    -1,
      -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,
      -1,   259,   260,   261,    -1,   263,    -1,    -1,    -1,   267,
      -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,   277,
      -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,
      -1,   289,   290,   291,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   300,    -1,   302,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,
       1,   319,   320,   321,   322,   323,    -1,     8,     9,    10,
      11,    12,    -1,    14,    15,   333,    17,    18,    -1,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      -1,    32,    -1,    34,    -1,    36,    37,    38,    39,    -1,
      -1,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    59,    60,
      -1,    62,    -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    75,    76,    77,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    99,    -1,
      -1,    -1,    -1,   104,    -1,    -1,   107,   108,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,    -1,    -1,
     131,    -1,   133,    -1,    -1,    -1,    -1,   138,    -1,    -1,
      -1,   142,    -1,   144,   145,    -1,   147,    -1,    -1,    -1,
     151,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,   165,    -1,    -1,    -1,    -1,   170,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,   189,    -1,
     191,   192,   193,    -1,    -1,   196,    -1,    -1,    -1,   200,
     201,   202,    -1,   204,    -1,    -1,   207,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   216,   217,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,
     231,    -1,   233,    -1,    -1,   236,    -1,    -1,   239,    -1,
      -1,    -1,    -1,    -1,   245,   246,    -1,    -1,    -1,    -1,
      -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,   259,   260,
     261,    -1,   263,    -1,    -1,    -1,   267,    -1,    -1,    -1,
      -1,    -1,   273,    -1,    -1,    -1,   277,    -1,    -1,    -1,
      -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,   289,   290,
     291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,
      -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   312,    -1,    -1,    -1,    -1,    -1,     1,   319,   320,
     321,   322,   323,    -1,     8,     9,    10,    11,    12,    -1,
      14,    15,   333,    17,    18,    -1,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    -1,    32,    -1,
      34,    -1,    36,    37,    38,    39,    -1,    -1,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    -1,    -1,    -1,
      -1,    55,    -1,    -1,    58,    59,    60,    -1,    62,    -1,
      -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    96,    -1,    -1,    99,    -1,    -1,    -1,    -1,
     104,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,    -1,    -1,   131,    -1,   133,
      -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,   142,    -1,
     144,   145,    -1,   147,    -1,    -1,    -1,   151,   152,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
      -1,   165,    -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   187,    -1,   189,    -1,   191,   192,   193,
      -1,    -1,   196,    -1,    -1,    -1,   200,   201,   202,    -1,
     204,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   216,   217,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   230,   231,    -1,   233,
      -1,    -1,   236,    -1,    -1,   239,    -1,    -1,    -1,    -1,
      -1,   245,   246,    -1,    -1,    -1,    -1,    -1,    -1,   253,
      -1,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,   263,
      -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,   273,
      -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,
     284,    -1,    -1,    -1,    -1,   289,   290,   291,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,   302,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,
      -1,    -1,    -1,    -1,     1,   319,   320,   321,   322,   323,
      -1,     8,     9,    10,    11,    12,    -1,    14,    15,   333,
      17,    18,    -1,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    -1,    32,    -1,    34,    -1,    36,
      37,    38,    39,    -1,    -1,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    -1,    -1,    -1,    -1,    55,    -1,
      -1,    58,    59,    60,    -1,    62,    -1,    -1,    -1,    66,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,
      -1,    -1,    99,    -1,    -1,    -1,    -1,   104,    -1,    -1,
     107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,    -1,    -1,   131,    -1,   133,    -1,    -1,    -1,
      -1,   138,    -1,    -1,    -1,   142,    -1,   144,   145,    -1,
     147,    -1,    -1,    -1,   151,   152,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,   165,    -1,
      -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     187,    -1,   189,    -1,   191,   192,   193,    -1,    -1,   196,
      -1,    -1,    -1,   200,   201,   202,    -1,   204,    -1,    -1,
     207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,
     217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   230,   231,    -1,   233,    -1,    -1,   236,
      -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,   245,   246,
      -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,
      -1,    -1,   259,   260,   261,    -1,   263,    -1,    -1,    -1,
     267,    -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,
     277,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,
      -1,    -1,   289,   290,   291,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   300,    -1,   302,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,
      -1,    -1,   319,   320,   321,   322,   323,    -1,     8,     9,
      10,    11,    12,    -1,    14,    15,   333,    17,    18,    -1,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    -1,    32,    -1,    34,    -1,    36,    37,    38,    39,
      -1,    -1,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    59,
      60,    -1,    62,    -1,    -1,    -1,    66,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    75,    76,    77,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    99,
      -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,    -1,
      -1,   131,    -1,   133,    -1,    -1,    -1,    -1,   138,    -1,
      -1,    -1,   142,    -1,   144,   145,    -1,   147,    -1,    -1,
      -1,   151,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   163,    -1,   165,    -1,    -1,    -1,    -1,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,   189,
      -1,   191,   192,   193,    -1,    -1,   196,    -1,    -1,    -1,
     200,   201,   202,    -1,   204,    -1,    -1,   207,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   216,   217,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     230,   231,    -1,   233,    -1,    -1,   236,    -1,    -1,   239,
      -1,    -1,    -1,    -1,    -1,   245,   246,    -1,    -1,    -1,
      -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,   259,
     260,   261,    -1,   263,    -1,    -1,    -1,   267,    -1,    -1,
      -1,    -1,    -1,   273,    -1,    -1,    -1,   277,    -1,    -1,
      -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,   289,
     290,   291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     300,    -1,   302,    -1,    -1,    -1,    -1,   307,   308,    -1,
     310,   311,   312,   313,   314,   315,   316,   317,   318,   319,
     320,   321,   322,   323,   324,   325,   326,   327,   328,    -1,
     330,   331,   332,   333,     8,     9,    10,    11,    12,    -1,
      14,    15,    -1,    17,    18,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    -1,    32,    -1,
      34,    -1,    36,    37,    38,    39,    -1,    -1,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    -1,    -1,
      -1,    55,    -1,    -1,    58,    59,    60,    -1,    62,    -1,
      -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    96,    -1,    -1,    99,    -1,    -1,    -1,    -1,
     104,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,    -1,    -1,   131,   132,   133,
      -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,   142,    -1,
     144,   145,    -1,   147,    -1,    -1,    -1,   151,   152,    -1,
      -1,    -1,    -1,    -1,   158,    -1,    -1,    -1,    -1,   163,
      -1,   165,   166,    -1,    -1,    -1,   170,    -1,    -1,    -1,
      -1,    -1,    -1,   177,    -1,    -1,    -1,    -1,    -1,   183,
      -1,    -1,    -1,   187,    -1,   189,    -1,   191,   192,   193,
      -1,   195,   196,    -1,    -1,    -1,   200,   201,   202,    -1,
     204,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   216,   217,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   230,   231,    -1,   233,
      -1,    -1,   236,    -1,    -1,   239,    -1,    -1,    -1,    -1,
      -1,   245,   246,    -1,    -1,    -1,    -1,    -1,    -1,   253,
      -1,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,   263,
      -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,   273,
      -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,
     284,    -1,    -1,    -1,    -1,   289,   290,   291,    -1,    -1,
      -1,    -1,    -1,    -1,   298,    -1,   300,    -1,   302,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,
      -1,    -1,    -1,    -1,    -1,   319,   320,   321,   322,   323,
      -1,     8,     9,    10,    11,    12,    -1,    14,    15,   333,
      17,    18,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    -1,    32,    -1,    34,    -1,    36,
      37,    38,    39,    -1,    -1,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    -1,    -1,    -1,    55,    -1,
      -1,    58,    59,    60,    -1,    62,    -1,    -1,    -1,    66,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,
      -1,    -1,    99,    -1,    -1,    -1,    -1,   104,    -1,    -1,
     107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,    -1,    -1,   131,   132,   133,    -1,    -1,    -1,
      -1,   138,    -1,    -1,    -1,   142,    -1,   144,   145,    -1,
     147,    -1,    -1,    -1,   151,   152,    -1,    -1,    -1,    -1,
      -1,   158,    -1,    -1,    -1,    -1,   163,    -1,   165,   166,
      -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,
     177,    -1,    -1,    -1,    -1,    -1,   183,    -1,    -1,    -1,
     187,    -1,   189,    -1,   191,   192,   193,    -1,   195,   196,
      -1,    -1,    -1,   200,   201,   202,    -1,   204,    -1,    -1,
     207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,
     217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   230,   231,    -1,   233,    -1,    -1,   236,
      -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,   245,   246,
      -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,
      -1,    -1,   259,   260,   261,    -1,   263,    -1,    -1,    -1,
     267,    -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,
     277,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,
      -1,    -1,   289,   290,   291,    -1,    -1,    -1,    -1,    -1,
      -1,   298,    -1,   300,    -1,   302,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,
      -1,    -1,   319,   320,   321,   322,   323,    -1,     8,     9,
      10,    11,    12,    -1,    14,    15,   333,    17,    18,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    -1,    32,    -1,    34,    -1,    36,    37,    38,    39,
      -1,    -1,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    -1,    -1,    -1,    55,    -1,    -1,    58,    59,
      60,    -1,    62,    -1,    -1,    -1,    66,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    75,    76,    77,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    99,
      -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,    -1,
      -1,   131,   132,   133,    -1,    -1,    -1,    -1,   138,    -1,
      -1,    -1,   142,    -1,   144,   145,    -1,   147,    -1,    -1,
      -1,   151,   152,    -1,    -1,    -1,    -1,    -1,   158,    -1,
      -1,    -1,    -1,   163,    -1,   165,   166,    -1,    -1,    -1,
     170,    -1,    -1,    -1,    -1,    -1,    -1,   177,    -1,    -1,
      -1,    -1,    -1,   183,    -1,    -1,    -1,   187,    -1,   189,
      -1,   191,   192,   193,    -1,   195,   196,    -1,    -1,    -1,
     200,   201,   202,    -1,   204,    -1,    -1,   207,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   216,   217,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     230,   231,    -1,   233,    -1,    -1,   236,    -1,    -1,   239,
      -1,    -1,    -1,    -1,    -1,   245,   246,    -1,    -1,    -1,
      -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,   259,
     260,   261,    -1,   263,    -1,    -1,    -1,   267,    -1,    -1,
      -1,    -1,    -1,   273,    -1,    -1,    -1,   277,    -1,    -1,
      -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,   289,
     290,   291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     300,   301,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,    -1,   319,
     320,   321,   322,   323,    -1,     8,     9,    10,    11,    12,
      -1,    14,    15,   333,    17,    18,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    -1,    32,
      -1,    34,    -1,    36,    37,    38,    39,    -1,    -1,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    -1,
      -1,    -1,    55,    -1,    -1,    58,    59,    60,    -1,    62,
      -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    75,    76,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    96,    -1,    -1,    99,    -1,    -1,    -1,
      -1,   104,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,    -1,    -1,   131,   132,
     133,    -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,   142,
      -1,   144,   145,    -1,   147,    -1,    -1,    -1,   151,   152,
      -1,    -1,    -1,    -1,    -1,   158,    -1,    -1,    -1,    -1,
     163,    -1,   165,   166,    -1,    -1,    -1,   170,    -1,    -1,
      -1,    -1,    -1,    -1,   177,    -1,    -1,    -1,    -1,    -1,
     183,    -1,    -1,    -1,   187,    -1,   189,    -1,   191,   192,
     193,    -1,   195,   196,    -1,    -1,    -1,   200,   201,   202,
      -1,   204,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   216,   217,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,   231,    -1,
     233,    -1,    -1,   236,    -1,    -1,   239,    -1,    -1,    -1,
      -1,    -1,   245,   246,    -1,    -1,    -1,    -1,    -1,    -1,
     253,    -1,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,
     263,    -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,
     273,    -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,
      -1,   284,    -1,    -1,    -1,    -1,   289,   290,   291,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,   301,   302,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,
      -1,    -1,    -1,    -1,    -1,    -1,   319,   320,   321,   322,
     323,    -1,     8,     9,    10,    11,    12,    -1,    14,    15,
     333,    17,    18,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    -1,    32,    -1,    34,    -1,
      36,    37,    38,    39,    -1,    -1,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    -1,    -1,    -1,    55,
      -1,    -1,    58,    59,    60,    -1,    62,    -1,    -1,    -1,
      66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,
      76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      96,    -1,    -1,    99,    -1,    -1,    -1,    -1,   104,    -1,
      -1,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,
     126,   127,   128,    -1,    -1,   131,   132,   133,    -1,    -1,
      -1,    -1,   138,    -1,    -1,    -1,   142,    -1,   144,   145,
      -1,   147,    -1,    -1,    -1,   151,   152,    -1,    -1,    -1,
      -1,    -1,   158,    -1,    -1,    -1,    -1,   163,    -1,   165,
     166,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,
      -1,   177,    -1,    -1,    -1,    -1,    -1,   183,    -1,    -1,
      -1,   187,    -1,   189,    -1,   191,   192,   193,    -1,   195,
     196,    -1,    -1,    -1,   200,   201,   202,    -1,   204,    -1,
      -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     216,   217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   230,   231,    -1,   233,    -1,    -1,
     236,    -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,   245,
     246,    -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,
      -1,    -1,    -1,   259,   260,   261,    -1,   263,    -1,    -1,
      -1,   267,    -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,
      -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,
      -1,    -1,    -1,   289,   290,   291,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   300,    -1,   302,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,
      -1,    -1,    -1,   319,   320,   321,   322,   323,    -1,     8,
       9,    10,    11,    12,    -1,    14,    15,   333,    17,    18,
      -1,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    -1,    32,    -1,    34,    -1,    36,    37,    38,
      39,    -1,    -1,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    -1,    -1,    -1,    -1,    55,    -1,    -1,    58,
      59,    60,    -1,    62,    -1,    -1,    -1,    66,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    77,    -1,
      -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    87,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,
      99,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,
      -1,    -1,   131,    -1,   133,    -1,    -1,    -1,    -1,   138,
      -1,    -1,    -1,   142,    -1,   144,   145,    -1,   147,    -1,
      -1,    -1,   151,   152,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,    -1,   165,    -1,    -1,    -1,
      -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,
     189,    -1,   191,   192,   193,    -1,    -1,   196,    -1,    -1,
      -1,   200,   201,   202,    -1,   204,    -1,    -1,   207,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,   217,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   230,   231,    -1,   233,    -1,    -1,   236,    -1,    -1,
     239,    -1,    -1,    -1,    -1,    -1,   245,   246,    -1,    -1,
      -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,
     259,   260,   261,    -1,   263,    -1,    -1,    -1,   267,    -1,
      -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,   277,    -1,
      -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,
     289,   290,   291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   300,    -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,    -1,
     319,   320,   321,   322,   323,    -1,     8,     9,    10,    11,
      12,    -1,    14,    15,   333,    17,    18,    -1,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    -1,
      32,    -1,    34,    -1,    36,    37,    38,    39,    -1,    -1,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    -1,
      -1,    -1,    -1,    55,    -1,    -1,    58,    59,    60,    -1,
      62,    -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    76,    77,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    96,    -1,    -1,    99,    -1,    -1,
      -1,    -1,   104,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,   126,   127,   128,    -1,    -1,   131,
      -1,   133,    -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,
     142,    -1,   144,   145,    -1,   147,    -1,    -1,    -1,   151,
     152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,    -1,   165,    -1,    -1,    -1,    -1,   170,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   187,    -1,   189,    -1,   191,
     192,   193,    -1,    -1,   196,    -1,    -1,    -1,   200,   201,
     202,    -1,   204,    -1,    -1,   207,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   216,   217,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,   231,
      -1,   233,    -1,    -1,   236,    -1,    -1,   239,    -1,    -1,
      -1,    -1,    -1,   245,   246,    -1,    -1,    -1,    -1,    -1,
      -1,   253,    -1,    -1,    -1,    -1,    -1,   259,   260,   261,
      -1,   263,    -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,
      -1,   273,    -1,   275,   276,   277,    -1,    -1,    -1,    -1,
      -1,    -1,   284,    -1,    -1,    -1,    -1,   289,   290,   291,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,
     302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     312,    -1,    -1,    -1,    -1,    -1,    -1,   319,   320,   321,
     322,   323,    -1,     8,     9,    10,    11,    12,    -1,    14,
      15,   333,    17,    18,    -1,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    -1,    32,    -1,    34,
      -1,    36,    37,    38,    39,    -1,    -1,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    -1,    -1,    -1,    -1,
      55,    -1,    -1,    58,    59,    60,    -1,    62,    -1,    -1,
      -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      75,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    96,    -1,    -1,    99,    -1,    -1,    -1,    -1,   104,
      -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,   126,   127,   128,    -1,    -1,   131,    -1,   133,    -1,
      -1,    -1,    -1,   138,    -1,    -1,    -1,   142,    -1,   144,
     145,    -1,   147,    -1,    -1,    -1,   151,   152,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,
     165,    -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   187,    -1,   189,    -1,   191,   192,   193,    -1,
      -1,   196,    -1,    -1,    -1,   200,   201,   202,    -1,   204,
      -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   216,   217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   230,   231,    -1,   233,    -1,
      -1,   236,    -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,
     245,   246,    -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,
      -1,    -1,    -1,    -1,   259,   260,   261,    -1,   263,    -1,
      -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,   273,    -1,
      -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,   284,
      -1,    -1,    -1,    -1,   289,   290,   291,    -1,    -1,    -1,
      -1,   296,    -1,    -1,    -1,   300,    -1,   302,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,
      -1,    -1,    -1,    -1,   319,   320,   321,   322,   323,    -1,
       8,     9,    10,    11,    12,    -1,    14,    15,   333,    17,
      18,    -1,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    -1,    32,    -1,    34,    -1,    36,    37,
      38,    39,    -1,    -1,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    -1,    -1,    -1,    -1,    55,    -1,    -1,
      58,    59,    60,    -1,    62,    -1,    -1,    -1,    66,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,
      -1,    99,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,
     108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,
     128,    -1,    -1,   131,    -1,   133,    -1,    -1,    -1,    -1,
     138,    -1,    -1,    -1,   142,    -1,   144,   145,    -1,   147,
      -1,    -1,    -1,   151,   152,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,    -1,   165,    -1,    -1,
      -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,
      -1,   189,    -1,   191,   192,   193,    -1,    -1,   196,    -1,
      -1,    -1,   200,   201,   202,    -1,   204,    -1,    -1,   207,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,   217,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   230,   231,    -1,   233,    -1,    -1,   236,    -1,
      -1,   239,    -1,    -1,    -1,    -1,    -1,   245,   246,    -1,
      -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,
      -1,   259,   260,   261,    -1,   263,    -1,    -1,    -1,   267,
      -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,   277,
      -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,
      -1,   289,   290,   291,    -1,    -1,    -1,    -1,   296,    -1,
      -1,    -1,   300,    -1,   302,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,
      -1,   319,   320,   321,   322,   323,    -1,     8,     9,    10,
      11,    12,    -1,    14,    15,   333,    17,    18,    -1,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      -1,    32,    -1,    34,    -1,    36,    37,    38,    39,    -1,
      -1,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    59,    60,
      -1,    62,    -1,    -1,    -1,    66,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    75,    76,    77,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    99,    -1,
      -1,    -1,    -1,   104,    -1,    -1,   107,   108,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,    -1,    -1,
     131,    -1,   133,    -1,    -1,    -1,    -1,   138,    -1,    -1,
      -1,   142,    -1,   144,   145,    -1,   147,    -1,    -1,    -1,
     151,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,   165,    -1,    -1,    -1,    -1,   170,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,   189,    -1,
     191,   192,   193,    -1,    -1,   196,    -1,    -1,    -1,   200,
     201,   202,    -1,   204,    -1,    -1,   207,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   216,   217,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,
     231,    -1,   233,    -1,    -1,   236,    -1,    -1,   239,    -1,
      -1,    -1,    -1,    -1,   245,   246,    -1,    -1,    -1,    -1,
      -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,   259,   260,
     261,    -1,   263,    -1,    -1,    -1,   267,    -1,    -1,    -1,
      -1,    -1,   273,    -1,    -1,    -1,   277,    -1,    -1,    -1,
      -1,    -1,    -1,   284,    -1,    -1,    -1,    -1,   289,   290,
     291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,
      -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   312,    -1,    -1,    -1,    -1,    -1,    -1,   319,   320,
     321,   322,   323,    -1,     8,     9,    10,    11,    12,    -1,
      14,    15,   333,    17,    18,    -1,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    -1,    32,    -1,
      34,    -1,    36,    37,    38,    39,    -1,    -1,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    -1,    -1,    -1,
      -1,    55,    -1,    -1,    58,    59,    60,    -1,    62,    -1,
      -1,    -1,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    75,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    96,    -1,    -1,    99,    -1,    -1,    -1,    -1,
     104,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,    -1,    -1,   131,    -1,   133,
      -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,   142,    -1,
     144,   145,    -1,   147,    -1,    -1,    -1,   151,   152,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,
      -1,   165,    -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   187,    -1,   189,    -1,   191,   192,   193,
      -1,    -1,   196,    -1,    -1,    -1,   200,   201,   202,    -1,
     204,    -1,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   216,   217,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   230,   231,    -1,   233,
      -1,    -1,   236,    -1,    -1,   239,    -1,    -1,    -1,    -1,
      -1,   245,   246,    -1,    -1,    -1,    -1,    -1,    -1,   253,
      -1,    -1,    -1,    -1,    -1,   259,   260,   261,    -1,   263,
      -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,   273,
      -1,    -1,    -1,   277,    -1,    -1,    -1,    -1,    -1,    -1,
     284,    -1,    -1,    -1,    -1,   289,   290,   291,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   300,    -1,   302,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,    -1,
      -1,    -1,    -1,    -1,    -1,   319,   320,   321,   322,   323,
      -1,     8,     9,    10,    11,    12,    -1,    14,    15,   333,
      17,    18,    -1,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    -1,    32,    -1,    34,    -1,    36,
      37,    38,    39,    -1,    -1,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    -1,    -1,    -1,    -1,    55,    -1,
      -1,    58,    59,    60,    -1,    62,    -1,    -1,    -1,    66,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,
      -1,    -1,    99,    -1,    -1,    -1,    -1,   104,    -1,    -1,
     107,   108,    -1,     0,     1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,    19,    -1,   131,    -1,   133,    -1,    -1,    -1,
      -1,   138,    -1,    -1,    -1,   142,    -1,   144,   145,    -1,
     147,    -1,    -1,    -1,   151,   152,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    51,    -1,   163,    -1,   165,    -1,
      -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    64,    -1,    -1,    -1,    -1,    -1,    -1,
     187,    -1,   189,    -1,   191,   192,   193,    84,    85,   196,
      -1,    -1,    -1,   200,   201,   202,    -1,   204,    -1,    -1,
     207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   216,
     217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      64,    -1,    -1,   230,   231,    -1,   233,    -1,    -1,   236,
      -1,    -1,   239,    -1,    -1,   132,    -1,    -1,   245,   246,
      -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,
      -1,    -1,   259,   260,   261,    -1,   263,    -1,    -1,    -1,
     267,   158,    -1,    -1,    64,    -1,   273,    -1,    -1,   166,
     277,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,
     177,    -1,   289,   290,   291,    -1,   183,    -1,   185,    -1,
      -1,    -1,    -1,   300,    -1,   302,    -1,    -1,   195,    -1,
      -1,    -1,   199,    -1,    -1,   312,     1,    -1,    -1,    -1,
      -1,    -1,   319,   320,   321,   322,   323,    12,    -1,    -1,
      -1,    -1,    -1,    18,    -1,    -1,   333,   224,    -1,    24,
      -1,    -1,    27,    28,    -1,    -1,   274,    32,   235,    34,
      -1,    36,    37,    38,    39,    -1,    -1,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    -1,    -1,    -1,    -1,
      55,    -1,    -1,    58,    59,    60,    -1,    62,    -1,   307,
     308,    66,   310,   311,    -1,   313,   314,   315,   316,   317,
     318,    76,    77,    -1,    -1,    -1,   324,   325,   326,   327,
     328,    -1,   330,   331,   332,    -1,    -1,    -1,   295,    -1,
      -1,    -1,   299,    -1,   301,    -1,    -1,   304,    -1,   306,
      -1,    -1,   107,   108,    -1,    -1,   307,   308,    -1,   310,
     311,    -1,   313,   314,   315,   316,   317,   318,    -1,    -1,
      -1,    -1,    -1,   324,   325,   326,   327,   328,   133,   330,
     331,   332,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
     145,    -1,   147,    -1,    -1,    -1,   151,    -1,    -1,    -1,
      -1,    -1,    -1,   307,   308,    -1,   310,   311,   163,   313,
     314,   315,   316,   317,   318,   170,    -1,    -1,    -1,    -1,
     324,   325,   326,   327,   328,    -1,   330,   331,   332,    -1,
      -1,    -1,   187,    -1,   189,    -1,   191,   192,   193,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   307,   308,   204,
     310,   311,    -1,   313,   314,   315,   316,   317,   318,    -1,
      -1,    -1,    -1,    -1,   324,   325,   326,   327,   328,    -1,
     330,   331,   332,    -1,    -1,    -1,   231,    -1,   233,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,
      -1,    -1,    -1,    -1,    -1,   260,   261,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   273,    -1,
       0,     1,    -1,    -1,    -1,     5,     6,    -1,    -1,    -1,
      -1,    -1,    12,    -1,    -1,    -1,   291,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    -1,    25,    -1,    -1,    -1,    29,
      -1,    31,    -1,    33,    -1,    -1,    36,    37,    38,    39,
      40,    41,    -1,    -1,   319,   320,   321,   322,   323,    49,
      -1,    -1,    52,    -1,    -1,    -1,    56,    57,    -1,    -1,
      -1,    -1,    -1,    63,    -1,    65,    -1,    67,    68,    69,
      70,    71,    72,    73,    74,    -1,    -1,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    -1,    -1,    98,    99,
     100,    -1,   102,   103,   104,   105,   106,    -1,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,   119,
     120,   121,   122,   123,    -1,    -1,    -1,    -1,    -1,   129,
     130,    -1,    -1,    -1,    -1,   135,   136,   137,   138,   139,
     140,    -1,    -1,   143,    -1,    -1,   146,   147,   148,   149,
     150,    -1,   152,   153,   154,   155,   156,   157,    -1,   159,
     160,   161,   162,    -1,   164,   165,    -1,   167,   168,   169,
      -1,    -1,    -1,    -1,    -1,   175,   176,   177,   178,   179,
     180,   181,   182,    -1,   184,   185,   186,    -1,    -1,    -1,
     190,    -1,    -1,    -1,   194,   195,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   209,
     210,   211,    -1,   213,   214,   215,    -1,   217,   218,   219,
     220,    -1,   222,   223,    -1,    -1,   226,   227,    -1,   229,
      -1,    -1,    -1,    -1,    -1,   235,   236,   237,   238,    -1,
     240,   241,   242,   243,   244,    -1,    -1,   247,   248,   249,
     250,   251,    -1,    -1,   254,   255,   256,   257,   258,    -1,
      -1,    -1,   262,   263,   264,   265,   266,    -1,   268,   269,
     270,   271,   272,    -1,    -1,    -1,    -1,    -1,   278,   279,
     280,   281,   282,   283,   284,   285,   286,   287,   288,    -1,
      -1,   291,     1,    -1,    -1,   295,     5,     6,   298,   299,
      -1,    -1,    -1,    12,    -1,    -1,   306,    -1,   308,    -1,
      -1,    20,    -1,    -1,    -1,    -1,    25,    -1,    -1,    -1,
      29,    -1,    31,    -1,    33,    -1,    -1,    36,    37,    38,
      39,    40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      49,    -1,    -1,    52,    -1,    -1,    -1,    56,    57,    -1,
      -1,    -1,    -1,    -1,    63,    -1,    65,    -1,    67,    68,
      69,    70,    71,    72,    73,    74,    -1,    -1,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    -1,    -1,    98,
      99,   100,    -1,   102,   103,   104,   105,   106,    -1,    -1,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
     119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,    -1,
     129,   130,    -1,    -1,    -1,    -1,   135,   136,   137,   138,
     139,   140,    -1,    -1,   143,    -1,    -1,   146,   147,   148,
     149,   150,    -1,   152,   153,   154,   155,   156,   157,    -1,
     159,   160,   161,   162,    -1,   164,   165,    -1,   167,   168,
     169,    -1,    -1,    -1,    -1,    -1,   175,   176,   177,   178,
     179,   180,   181,   182,    -1,   184,   185,   186,    -1,    -1,
      -1,   190,    -1,    -1,    -1,   194,   195,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     209,   210,   211,    -1,   213,   214,   215,    -1,   217,   218,
     219,   220,    -1,   222,   223,    -1,    -1,   226,   227,    -1,
     229,    -1,    -1,    -1,    -1,    -1,    -1,   236,   237,   238,
      -1,   240,   241,   242,   243,   244,    -1,    -1,   247,   248,
     249,   250,   251,    -1,    -1,   254,   255,   256,   257,   258,
      -1,    -1,    -1,   262,   263,   264,   265,   266,    -1,   268,
     269,   270,   271,   272,    -1,    -1,    -1,    -1,    -1,   278,
     279,   280,   281,   282,   283,   284,   285,   286,   287,   288,
      -1,    -1,   291,     1,    -1,    -1,   295,     5,     6,   298,
     299,    -1,    -1,    -1,    12,    -1,    -1,   306,    -1,   308,
      -1,    -1,    20,    -1,    -1,    -1,    -1,    25,    -1,    -1,
      -1,    29,    -1,    31,    -1,    33,    -1,    -1,    36,    37,
      38,    39,    40,    41,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    49,    -1,    -1,    52,    -1,    -1,    -1,    56,    57,
      -1,    -1,    -1,    -1,    -1,    63,    -1,    65,    -1,    67,
      68,    69,    70,    71,    72,    73,    74,    -1,    -1,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    -1,    -1,
      98,    99,   100,    -1,   102,   103,   104,   105,   106,    -1,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,   119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,
      -1,   129,   130,    -1,    -1,    -1,    -1,   135,   136,   137,
     138,   139,   140,    -1,    -1,   143,    -1,    -1,   146,   147,
     148,   149,   150,    -1,   152,   153,   154,   155,   156,   157,
      -1,   159,   160,   161,   162,    -1,   164,   165,    -1,   167,
     168,   169,    -1,    -1,    -1,    -1,    -1,   175,   176,   177,
     178,   179,   180,   181,   182,    -1,   184,   185,   186,    -1,
      -1,    -1,   190,    -1,    -1,    -1,   194,   195,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   209,   210,   211,    -1,   213,   214,   215,    -1,   217,
     218,   219,   220,    -1,   222,   223,    -1,    -1,   226,   227,
      -1,   229,    -1,    -1,    -1,    -1,    -1,    -1,   236,   237,
     238,    -1,   240,   241,   242,   243,   244,    -1,    -1,   247,
     248,   249,   250,   251,    -1,    -1,   254,   255,   256,   257,
     258,    -1,    -1,    -1,   262,   263,   264,   265,   266,    -1,
     268,   269,   270,   271,   272,    -1,    -1,    -1,    -1,    -1,
     278,   279,   280,   281,   282,   283,   284,   285,   286,   287,
     288,    -1,    -1,   291,     1,    -1,    -1,   295,     5,     6,
     298,    -1,    -1,    -1,    -1,    12,    -1,    -1,   306,    -1,
     308,    -1,    -1,    20,    -1,    -1,    -1,    -1,    25,    -1,
      -1,    -1,    29,    -1,    31,    -1,    33,    -1,    -1,    36,
      37,    38,    39,    40,    41,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    49,    -1,    -1,    52,    -1,    -1,    -1,    56,
      57,    -1,    -1,    -1,    -1,    -1,    63,    -1,    65,    -1,
      67,    68,    69,    70,    71,    72,    73,    74,    -1,    -1,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    -1,
      -1,    98,    99,   100,    -1,   102,   103,   104,   105,   106,
      -1,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   119,   120,   121,   122,   123,    -1,    -1,    -1,
      -1,    -1,   129,   130,    -1,    -1,    -1,    -1,   135,   136,
     137,   138,   139,   140,    -1,    -1,   143,    -1,    -1,   146,
     147,   148,   149,   150,    -1,   152,   153,   154,   155,   156,
     157,    -1,   159,   160,   161,   162,    -1,   164,   165,    -1,
     167,   168,   169,    -1,    -1,    -1,    -1,    -1,   175,   176,
     177,   178,   179,   180,   181,   182,    -1,   184,   185,   186,
      -1,    -1,    -1,   190,    -1,    -1,    -1,   194,   195,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   209,   210,   211,    -1,   213,   214,   215,    -1,
     217,   218,   219,   220,    -1,   222,   223,    -1,    -1,   226,
     227,    -1,   229,    -1,    -1,    -1,    -1,    -1,    -1,   236,
     237,   238,    -1,   240,   241,   242,   243,   244,    -1,    -1,
     247,   248,   249,   250,   251,    -1,    -1,   254,   255,   256,
     257,   258,    -1,    -1,    -1,   262,   263,   264,   265,   266,
      -1,   268,   269,   270,   271,   272,    -1,    -1,    -1,    -1,
      -1,   278,   279,   280,   281,   282,   283,   284,   285,   286,
     287,   288,    -1,    -1,   291,     1,    -1,    -1,   295,     5,
       6,   298,    -1,    -1,    -1,    -1,    12,    -1,    -1,   306,
      -1,   308,    -1,    -1,    20,    -1,    -1,    -1,    -1,    25,
      -1,    -1,    -1,    29,    -1,    31,    -1,    33,    -1,    -1,
      36,    37,    38,    39,    40,    41,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    -1,    -1,
      56,    57,    -1,    -1,    -1,    -1,    -1,    63,    -1,    65,
      -1,    67,    68,    69,    70,    71,    72,    73,    74,    -1,
      -1,    77,    78,    79,    80,    81,    82,    83,    -1,    -1,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      -1,    -1,    98,    99,   100,    -1,   102,   103,   104,   105,
     106,    -1,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,   119,   120,   121,   122,   123,    -1,    -1,
      -1,    -1,    -1,   129,   130,    -1,    -1,    -1,    -1,   135,
     136,   137,   138,   139,   140,    -1,    -1,   143,    -1,    -1,
     146,   147,   148,   149,   150,    -1,   152,   153,   154,   155,
     156,   157,    -1,   159,   160,   161,   162,    -1,   164,   165,
      -1,   167,   168,   169,    -1,    -1,    -1,    -1,    -1,   175,
     176,   177,   178,   179,   180,   181,   182,    -1,   184,   185,
     186,    -1,    -1,    -1,   190,    -1,    -1,    -1,   194,   195,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   209,   210,   211,    -1,   213,   214,   215,
      -1,   217,   218,   219,   220,    -1,   222,   223,    -1,    -1,
     226,   227,    -1,   229,    -1,    -1,    -1,    -1,    -1,    -1,
     236,   237,   238,    -1,   240,   241,   242,   243,   244,    -1,
      -1,   247,   248,   249,   250,   251,    -1,    -1,   254,   255,
     256,   257,   258,    -1,    -1,    -1,   262,   263,   264,   265,
     266,    -1,   268,   269,   270,   271,   272,    -1,    -1,    -1,
      -1,    -1,   278,   279,   280,   281,   282,   283,   284,   285,
     286,   287,   288,    -1,    -1,   291,     1,    -1,    -1,   295,
       5,     6,   298,    -1,    -1,    -1,    -1,    12,    -1,    -1,
     306,    -1,   308,    -1,    -1,    20,    -1,    -1,    -1,    -1,
      25,    -1,    -1,    -1,    29,    -1,    31,    -1,    33,    -1,
      -1,    36,    37,    38,    39,    40,    41,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    -1,
      -1,    56,    57,    -1,    -1,    -1,    -1,    -1,    63,    -1,
      65,    -1,    67,    68,    69,    70,    71,    72,    73,    74,
      -1,    -1,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    -1,    -1,    98,    99,   100,    -1,   102,   103,   104,
     105,   106,    -1,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,   119,   120,   121,   122,   123,    -1,
      -1,    -1,    -1,    -1,   129,   130,    -1,    -1,    -1,    -1,
     135,   136,   137,   138,   139,   140,    -1,    -1,   143,    -1,
      -1,   146,   147,   148,   149,   150,    -1,   152,   153,   154,
     155,   156,   157,    -1,   159,   160,   161,   162,    -1,   164,
     165,    -1,   167,   168,   169,    -1,    -1,    -1,    -1,    -1,
     175,   176,   177,   178,   179,   180,   181,   182,    -1,   184,
     185,   186,    -1,    -1,    -1,   190,    -1,    -1,    -1,   194,
     195,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   209,   210,   211,    -1,   213,   214,
     215,    -1,   217,   218,   219,   220,    -1,   222,   223,    -1,
      -1,   226,   227,    -1,   229,    -1,    -1,    -1,    -1,    -1,
      -1,   236,   237,   238,    -1,   240,   241,   242,   243,   244,
      -1,    -1,   247,   248,   249,   250,   251,    -1,    -1,   254,
     255,   256,   257,   258,    -1,    -1,    -1,   262,   263,   264,
     265,   266,    -1,   268,   269,   270,   271,   272,    -1,    -1,
      -1,    -1,    -1,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,    -1,    -1,   291,     1,    -1,    -1,
      -1,     5,     6,   298,    -1,    -1,   301,    -1,    12,    -1,
      -1,   306,    -1,   308,    -1,    -1,    20,    -1,    -1,    -1,
      -1,    25,    -1,    -1,    -1,    29,    -1,    31,    -1,    33,
      -1,    -1,    36,    37,    38,    39,    40,    41,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,
      -1,    -1,    56,    57,    -1,    -1,    -1,    -1,    -1,    63,
      -1,    65,    -1,    67,    68,    69,    70,    71,    72,    73,
      74,    -1,    -1,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    -1,    -1,    98,    99,   100,    -1,   102,   103,
     104,   105,   106,    -1,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,   119,   120,   121,   122,   123,
      -1,    -1,    -1,    -1,    -1,   129,   130,    -1,    -1,    -1,
      -1,   135,   136,   137,   138,   139,   140,    -1,    -1,   143,
      -1,    -1,   146,   147,   148,   149,   150,    -1,   152,   153,
     154,   155,   156,   157,    -1,   159,   160,   161,   162,    -1,
     164,   165,    -1,   167,   168,   169,    -1,    -1,    -1,    -1,
      -1,   175,   176,   177,   178,   179,   180,   181,   182,    -1,
     184,   185,   186,    -1,    -1,    -1,   190,    -1,    -1,    -1,
     194,   195,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   209,   210,   211,    -1,   213,
     214,   215,    -1,   217,   218,   219,   220,    -1,   222,   223,
      -1,    -1,   226,   227,    -1,   229,    -1,    -1,    -1,    -1,
      -1,    -1,   236,   237,   238,    -1,   240,   241,   242,   243,
     244,    -1,    -1,   247,   248,   249,   250,   251,    -1,    -1,
     254,   255,   256,   257,   258,    -1,    -1,    -1,   262,   263,
     264,   265,   266,    -1,   268,   269,   270,   271,   272,    -1,
      -1,    -1,    -1,    -1,   278,   279,   280,   281,   282,   283,
     284,   285,   286,   287,   288,    -1,    -1,   291,     1,    -1,
      -1,   295,     5,     6,   298,    -1,    -1,    -1,    -1,    12,
      -1,    -1,   306,    -1,   308,    -1,    -1,    20,    -1,    -1,
      -1,    -1,    25,    -1,    -1,    -1,    29,    -1,    31,    -1,
      33,    -1,    -1,    36,    37,    38,    39,    40,    41,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,
      -1,    -1,    -1,    56,    57,    -1,    -1,    -1,    -1,    -1,
      63,    -1,    65,    -1,    67,    68,    69,    70,    71,    72,
      73,    74,    -1,    -1,    77,    78,    79,    80,    81,    82,
      83,    -1,    -1,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    -1,    -1,    98,    99,   100,    -1,   102,
     103,   104,   105,   106,    -1,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,   119,   120,   121,   122,
     123,    -1,    -1,    -1,    -1,    -1,   129,   130,    -1,    -1,
      -1,    -1,   135,   136,   137,   138,   139,   140,    -1,    -1,
     143,    -1,    -1,   146,   147,   148,   149,   150,    -1,   152,
     153,   154,   155,   156,   157,    -1,   159,   160,   161,   162,
      -1,   164,   165,    -1,   167,   168,   169,    -1,    -1,    -1,
      -1,    -1,   175,   176,   177,   178,   179,   180,   181,   182,
      -1,   184,   185,   186,    -1,    -1,    -1,   190,    -1,    -1,
      -1,   194,   195,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   209,   210,   211,    -1,
     213,   214,   215,    -1,   217,   218,   219,   220,    -1,   222,
     223,    -1,    -1,   226,   227,    -1,   229,    -1,    -1,    -1,
      -1,    -1,    -1,   236,   237,   238,    -1,   240,   241,   242,
     243,   244,    -1,    -1,   247,   248,   249,   250,   251,    -1,
      -1,   254,   255,   256,   257,   258,    -1,    -1,    -1,   262,
     263,   264,   265,   266,    -1,   268,   269,   270,   271,   272,
      -1,    -1,    -1,    -1,    -1,   278,   279,   280,   281,   282,
     283,   284,   285,   286,   287,   288,    -1,     1,   291,    -1,
      -1,     5,     6,    -1,    -1,   298,    -1,    -1,    12,    -1,
      -1,    -1,    -1,   306,    -1,   308,    20,    -1,    -1,    -1,
      -1,    25,    -1,    -1,    -1,    29,    -1,    31,    -1,    33,
      -1,    -1,    36,    37,    38,    39,    40,    41,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,
      -1,    -1,    56,    57,    -1,    -1,    -1,    -1,    -1,    63,
      -1,    65,    -1,    67,    68,    69,    70,    71,    72,    73,
      74,    -1,    -1,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    -1,    -1,    98,    99,   100,    -1,   102,   103,
     104,   105,   106,    -1,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,   119,   120,   121,   122,   123,
      -1,    -1,    -1,    -1,    -1,   129,   130,    -1,    -1,    -1,
      -1,   135,   136,   137,   138,   139,   140,    -1,    -1,   143,
      -1,    -1,   146,   147,   148,   149,   150,    -1,   152,   153,
     154,   155,   156,   157,    -1,   159,   160,   161,   162,    -1,
     164,   165,    -1,   167,   168,   169,    -1,    -1,    -1,    -1,
      -1,   175,   176,   177,   178,   179,   180,   181,   182,    -1,
     184,   185,   186,    -1,    -1,    -1,   190,    -1,    -1,    -1,
     194,   195,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   209,   210,   211,    -1,   213,
     214,   215,    -1,   217,   218,   219,   220,    -1,   222,   223,
      -1,    -1,   226,   227,    -1,   229,    -1,    -1,    -1,    -1,
      -1,    -1,   236,   237,   238,    -1,   240,   241,   242,   243,
     244,    -1,    -1,   247,   248,   249,   250,   251,    -1,    -1,
     254,   255,   256,   257,   258,    -1,    -1,    -1,   262,   263,
     264,   265,   266,    -1,   268,   269,   270,   271,   272,    -1,
      -1,    -1,    -1,    -1,   278,   279,   280,   281,   282,   283,
     284,   285,   286,   287,   288,    -1,     1,   291,    -1,    -1,
       5,     6,    -1,    -1,   298,    -1,    -1,    12,    -1,    -1,
      -1,    -1,   306,    -1,   308,    20,    -1,    -1,    -1,    -1,
      25,    -1,    -1,    -1,    29,    -1,    31,    -1,    33,    -1,
      -1,    36,    37,    38,    39,    40,    41,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    -1,
      -1,    56,    57,    -1,    -1,    -1,    -1,    -1,    63,    -1,
      65,    -1,    67,    68,    69,    70,    71,    72,    73,    74,
      -1,    -1,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    -1,    -1,    98,    99,   100,    -1,   102,   103,   104,
     105,   106,    -1,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,   119,   120,   121,   122,   123,    -1,
      -1,    -1,    -1,    -1,   129,   130,    -1,    -1,    -1,    -1,
     135,   136,   137,   138,   139,   140,    -1,    -1,   143,    -1,
      -1,   146,   147,   148,   149,   150,    -1,   152,   153,   154,
     155,   156,   157,    -1,   159,   160,   161,   162,    -1,   164,
     165,    -1,   167,   168,   169,    -1,    -1,    -1,    -1,    -1,
     175,   176,   177,   178,   179,   180,   181,   182,    -1,   184,
     185,   186,    -1,    -1,    -1,   190,    -1,    -1,    -1,   194,
     195,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   209,   210,   211,    -1,   213,   214,
     215,    -1,   217,   218,   219,   220,    -1,   222,   223,    -1,
      -1,   226,   227,    -1,   229,    -1,    -1,    -1,    -1,    -1,
      -1,   236,   237,   238,    -1,   240,   241,   242,   243,   244,
      -1,    -1,   247,   248,   249,   250,   251,    -1,    -1,   254,
     255,   256,   257,   258,    -1,    -1,    -1,   262,   263,   264,
     265,   266,    -1,   268,   269,   270,   271,   272,    -1,    -1,
      -1,    -1,    -1,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,     1,    -1,   291,    -1,     5,    -1,
      -1,    -1,    -1,   298,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   306,    -1,   308,    -1,    -1,    -1,    -1,    25,    -1,
      -1,    -1,    -1,    -1,    31,    -1,    33,    -1,    -1,    36,
      37,    38,    39,    40,    41,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    49,    -1,    -1,    52,    -1,    -1,    -1,    56,
      57,    -1,    -1,    -1,    -1,    -1,    63,    -1,    65,    -1,
      67,    68,    69,    70,    71,    72,    73,    74,    -1,    -1,
      -1,    78,    79,    80,    81,    82,    83,    -1,    -1,    -1,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    -1,
      -1,    98,    99,   100,    -1,   102,   103,   104,   105,   106,
      -1,    -1,   109,   110,   111,   112,   113,    -1,   115,   116,
     117,    -1,   119,    -1,   121,   122,   123,    -1,    -1,    -1,
      -1,    -1,   129,   130,    -1,    -1,    -1,    -1,   135,   136,
      -1,    -1,   139,   140,    -1,    -1,    -1,    -1,    -1,   146,
      -1,   148,   149,   150,    -1,   152,   153,   154,   155,   156,
     157,    -1,   159,   160,   161,   162,    -1,   164,   165,    -1,
     167,   168,   169,    -1,    -1,    -1,    -1,    -1,   175,   176,
      -1,   178,   179,   180,    -1,   182,    -1,   184,   185,   186,
      -1,    -1,    -1,   190,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     0,     1,    -1,    -1,    -1,
      -1,    -1,    -1,   210,   211,    -1,   213,   214,   215,    -1,
      -1,   218,   219,   220,    19,   222,   223,    -1,    -1,   226,
     227,    -1,   229,    -1,    -1,    -1,     0,     1,    -1,    -1,
     237,   238,    -1,   240,   241,   242,   243,   244,    -1,    -1,
     247,   248,   249,   250,   251,    19,    51,   254,   255,   256,
     257,   258,    -1,     0,     1,   262,   263,   264,   265,   266,
      -1,   268,   269,   270,   271,   272,    -1,    -1,     0,     1,
      -1,   278,   279,   280,   281,   282,   283,    51,    25,    84,
      85,   288,    -1,    -1,   291,    -1,    -1,    19,    -1,    36,
      37,    38,    39,    -1,    -1,    -1,    -1,    -1,    -1,   306,
      -1,   308,    49,    -1,    -1,    -1,    24,    -1,    -1,    27,
      84,    85,    -1,    -1,    32,    -1,    34,    -1,    -1,    51,
      -1,    -1,    -1,    -1,    -1,    -1,    44,   132,    -1,    47,
      48,    -1,    50,    -1,    -1,    -1,    54,    -1,    -1,    -1,
      87,    59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   158,    -1,    -1,    64,    -1,   132,    -1,
      -1,   166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   177,    -1,    -1,    -1,    -1,    -1,   183,    -1,
     185,    -1,    -1,    64,   158,    -1,    -1,    -1,    -1,    -1,
     195,    -1,   166,    -1,   199,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,   177,    -1,    -1,    -1,    -1,    -1,   183,
      -1,   185,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   224,
      -1,   195,    -1,   141,   142,   199,   158,   145,    -1,    -1,
     235,    -1,    -1,   151,   166,    -1,    -1,    -1,   185,    -1,
      -1,    -1,    -1,    -1,    -1,   177,    64,    -1,    -1,    -1,
     224,   183,   199,   185,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   235,    -1,   195,    -1,    -1,    -1,   199,    -1,    -1,
      -1,    -1,    -1,    -1,   192,   193,    -1,   224,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   204,    -1,   235,    -1,
     295,    -1,   224,    -1,   299,    -1,   301,    -1,    -1,   304,
      -1,   306,    -1,   235,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   233,   263,    -1,    -1,    -1,
      -1,   295,    -1,    -1,    -1,   299,    -1,   301,    -1,    -1,
     304,    -1,   306,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   295,    -1,
      -1,    -1,   299,    -1,   301,    -1,    -1,   304,    24,   306,
      -1,    27,    -1,   295,    -1,    -1,    32,   299,    34,   301,
      -1,    -1,   304,    -1,   306,    -1,    -1,    -1,    44,    -1,
      -1,    47,    48,    -1,    50,    -1,    -1,    -1,    54,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    -1,    64,   307,
     308,    -1,   310,   311,    -1,   313,   314,   315,   316,   317,
     318,    77,    -1,    -1,    -1,    -1,   324,   325,   326,   327,
     328,    -1,   330,   331,   332,    -1,   307,   308,    -1,   310,
     311,    -1,   313,   314,   315,   316,   317,   318,    -1,    -1,
      -1,   107,   108,   324,   325,   326,   327,   328,    -1,   330,
     331,   332,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   133,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,   142,    -1,    -1,   145,
      -1,   147,    -1,    -1,    -1,   151,    -1,    -1,    -1,   307,
     308,    -1,   310,   311,    -1,   313,   314,   315,   316,   317,
     318,    -1,    -1,    -1,   170,    -1,   324,   325,   326,   327,
     328,    -1,   330,   331,   332,    -1,     1,    -1,    -1,    -1,
      -1,   187,    -1,   189,    -1,   191,   192,   193,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    39,   204,    -1,
      25,    -1,    44,    -1,    -1,    -1,   212,    -1,    -1,    -1,
      -1,    36,    37,    38,    39,   221,    58,    59,    60,    61,
      -1,    -1,    64,    -1,    49,   231,    -1,   233,    53,    -1,
      -1,    -1,    -1,    -1,    -1,    77,    61,    -1,   295,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     307,   308,    77,   310,   311,    -1,   313,   314,   315,   316,
     317,   318,    87,    -1,    -1,   107,   108,   324,   325,   326,
     327,   328,    -1,   330,   331,   332,   101,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   291,    -1,    -1,   294,    -1,
      -1,   133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,    -1,    -1,    -1,    -1,   147,    -1,    -1,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,
      -1,    -1,   147,    -1,   274,    -1,    -1,    -1,   170,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   187,    -1,   189,    -1,   191,
     192,   193,    -1,    -1,    -1,    -1,    -1,   307,   308,    -1,
     310,   311,   204,   313,   314,   315,   316,   317,   318,    -1,
     212,    -1,    -1,    -1,   324,   325,   326,   327,   328,   221,
     330,   331,   332,    -1,    -1,    -1,    -1,   212,    -1,   231,
      -1,   293,    -1,    -1,    -1,    -1,   221,    -1,    -1,   301,
      -1,    -1,    -1,   245,   246,   307,   308,    -1,   310,   311,
      -1,   313,   314,   315,   316,   317,   318,    -1,    -1,    -1,
     245,   246,   324,   325,   326,   327,   328,   252,   330,   331,
     332,   293,    -1,    -1,    -1,    -1,    -1,    -1,   263,   301,
      -1,    -1,    -1,    -1,    -1,   307,   308,    -1,   310,   311,
      -1,   313,   314,   315,   316,   317,   318,   293,    -1,    -1,
      -1,    -1,   324,   325,   326,   327,   328,    -1,   330,   331,
     332,   307,   308,    -1,   310,   311,    -1,   313,   314,   315,
     316,   317,   318,   293,    -1,    -1,    -1,    -1,   324,   325,
     326,   327,   328,    -1,   330,   331,   332,   307,   308,    -1,
     310,   311,    -1,   313,   314,   315,   316,   317,   318,   293,
      -1,    -1,    -1,    -1,   324,   325,   326,   327,   328,    -1,
     330,   331,   332,   307,   308,    -1,   310,   311,    -1,   313,
     314,   315,   316,   317,   318,   293,    -1,    -1,    -1,    -1,
     324,   325,   326,   327,   328,    -1,   330,   331,   332,   307,
     308,    -1,   310,   311,    -1,   313,   314,   315,   316,   317,
     318,   293,    -1,    -1,    -1,    -1,   324,   325,   326,   327,
     328,    -1,   330,   331,   332,   307,   308,    -1,   310,   311,
      -1,   313,   314,   315,   316,   317,   318,   293,    -1,    -1,
      -1,    -1,   324,   325,   326,   327,   328,    -1,   330,   331,
     332,   307,   308,    -1,   310,   311,    -1,   313,   314,   315,
     316,   317,   318,   293,    -1,    -1,    -1,    -1,   324,   325,
     326,   327,   328,    -1,   330,   331,   332,   307,   308,    -1,
     310,   311,    -1,   313,   314,   315,   316,   317,   318,   293,
      -1,    -1,    -1,    -1,   324,   325,   326,   327,   328,    -1,
     330,   331,   332,   307,   308,    -1,   310,   311,    -1,   313,
     314,   315,   316,   317,   318,   293,    -1,    -1,    -1,    -1,
     324,   325,   326,   327,   328,    -1,   330,   331,   332,   307,
     308,    -1,   310,   311,    -1,   313,   314,   315,   316,   317,
     318,   293,    -1,    -1,    -1,    -1,   324,   325,   326,   327,
     328,    -1,   330,   331,   332,   307,   308,    -1,   310,   311,
      -1,   313,   314,   315,   316,   317,   318,   293,    -1,    -1,
      -1,    -1,   324,   325,   326,   327,   328,    -1,   330,   331,
     332,   307,   308,    -1,   310,   311,    -1,   313,   314,   315,
     316,   317,   318,   293,    -1,    -1,    -1,    -1,   324,   325,
     326,   327,   328,    -1,   330,   331,   332,   307,   308,    -1,
     310,   311,    -1,   313,   314,   315,   316,   317,   318,    -1,
      -1,    -1,    -1,   297,   324,   325,   326,   327,   328,    -1,
     330,   331,   332,   307,   308,    -1,   310,   311,    -1,   313,
     314,   315,   316,   317,   318,    -1,    -1,    -1,    -1,   297,
     324,   325,   326,   327,   328,    -1,   330,   331,   332,   307,
     308,    -1,   310,   311,    -1,   313,   314,   315,   316,   317,
     318,    -1,    -1,    -1,    -1,   297,   324,   325,   326,   327,
     328,    -1,   330,   331,   332,   307,   308,    -1,   310,   311,
      -1,   313,   314,   315,   316,   317,   318,    -1,    -1,    -1,
      -1,   297,   324,   325,   326,   327,   328,    -1,   330,   331,
     332,   307,   308,    -1,   310,   311,    -1,   313,   314,   315,
     316,   317,   318,    -1,    -1,    -1,    -1,   297,   324,   325,
     326,   327,   328,    -1,   330,   331,   332,   307,   308,    -1,
     310,   311,    -1,   313,   314,   315,   316,   317,   318,    -1,
      -1,    -1,    -1,    -1,   324,   325,   326,   327,   328,   301,
     330,   331,   332,    -1,    -1,   307,   308,    -1,   310,   311,
      -1,   313,   314,   315,   316,   317,   318,    -1,    -1,    -1,
      -1,    -1,   324,   325,   326,   327,   328,   301,   330,   331,
     332,    -1,    -1,   307,   308,    -1,   310,   311,    -1,   313,
     314,   315,   316,   317,   318,    -1,    -1,    -1,    -1,    -1,
     324,   325,   326,   327,   328,   301,   330,   331,   332,    -1,
      -1,   307,   308,    -1,   310,   311,    -1,   313,   314,   315,
     316,   317,   318,    -1,    -1,    -1,    -1,    -1,   324,   325,
     326,   327,   328,   301,   330,   331,   332,    -1,    -1,   307,
     308,    -1,   310,   311,    -1,   313,   314,   315,   316,   317,
     318,    -1,    -1,    -1,    -1,    -1,   324,   325,   326,   327,
     328,   301,   330,   331,   332,    -1,    -1,   307,   308,    -1,
     310,   311,    -1,   313,   314,   315,   316,   317,   318,    -1,
      -1,    -1,    -1,    -1,   324,   325,   326,   327,   328,   301,
     330,   331,   332,    -1,    -1,   307,   308,    -1,   310,   311,
      -1,   313,   314,   315,   316,   317,   318,    -1,    -1,    -1,
      -1,    -1,   324,   325,   326,   327,   328,   301,   330,   331,
     332,    -1,    -1,   307,   308,    -1,   310,   311,    -1,   313,
     314,   315,   316,   317,   318,    -1,    -1,    -1,    -1,    -1,
     324,   325,   326,   327,   328,   301,   330,   331,   332,    -1,
      -1,   307,   308,    -1,   310,   311,    -1,   313,   314,   315,
     316,   317,   318,    -1,    -1,    -1,    -1,    -1,   324,   325,
     326,   327,   328,   301,   330,   331,   332,    -1,    -1,   307,
     308,    -1,   310,   311,    -1,   313,   314,   315,   316,   317,
     318,    -1,    -1,    -1,    -1,    -1,   324,   325,   326,   327,
     328,   301,   330,   331,   332,    -1,    -1,   307,   308,    -1,
     310,   311,    -1,   313,   314,   315,   316,   317,   318,    -1,
      -1,    -1,    -1,    -1,   324,   325,   326,   327,   328,   301,
     330,   331,   332,    -1,    -1,   307,   308,    -1,   310,   311,
      -1,   313,   314,   315,   316,   317,   318,    -1,    -1,    -1,
      -1,    -1,   324,   325,   326,   327,   328,   301,   330,   331,
     332,    -1,    -1,   307,   308,    -1,   310,   311,    -1,   313,
     314,   315,   316,   317,   318,    -1,    -1,    -1,    -1,    -1,
     324,   325,   326,   327,   328,   301,   330,   331,   332,    -1,
      -1,   307,   308,    -1,   310,   311,    -1,   313,   314,   315,
     316,   317,   318,    -1,    -1,    -1,    -1,    -1,   324,   325,
     326,   327,   328,   301,   330,   331,   332,    -1,    -1,   307,
     308,    -1,   310,   311,    -1,   313,   314,   315,   316,   317,
     318,    -1,    -1,    -1,    -1,    -1,   324,   325,   326,   327,
     328,   301,   330,   331,   332,    -1,    -1,   307,   308,    -1,
     310,   311,    -1,   313,   314,   315,   316,   317,   318,    -1,
      -1,    -1,    -1,    -1,   324,   325,   326,   327,   328,   301,
     330,   331,   332,    -1,    -1,   307,   308,    -1,   310,   311,
      -1,   313,   314,   315,   316,   317,   318,    -1,    -1,    -1,
      -1,    -1,   324,   325,   326,   327,   328,    -1,   330,   331,
     332,   307,   308,    -1,   310,   311,    -1,   313,   314,   315,
     316,   317,   318,    -1,    -1,    -1,    -1,    -1,   324,   325,
     326,   327,   328,    -1,   330,   331,   332,    -1,   334,   307,
     308,   309,   310,   311,    -1,   313,   314,   315,   316,   317,
     318,    -1,    -1,    -1,    -1,    -1,   324,   325,   326,   327,
     328,    -1,   330,   331,   332,   307,   308,    -1,   310,   311,
      -1,   313,   314,   315,   316,   317,   318,    -1,    -1,    -1,
      -1,    -1,   324,   325,   326,   327,   328,    -1,   330,   331,
     332,   307,   308,    -1,   310,   311,    -1,   313,   314,   315,
     316,   317,   318,    -1,    -1,    -1,    -1,    -1,   324,   325,
     326,   327,   328,    -1,   330,   331,   332
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned short int yystos[] =
{
       0,     3,     4,   336,   358,   337,     0,     8,     9,    10,
      11,    12,    14,    15,    17,    18,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    32,    34,    36,    37,
      38,    39,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    55,    58,    59,    60,    62,    66,    75,    76,    77,
      87,    96,    99,   104,   107,   108,   124,   125,   126,   127,
     128,   131,   133,   138,   142,   144,   145,   147,   151,   152,
     163,   165,   170,   187,   189,   191,   192,   193,   196,   200,
     201,   202,   204,   207,   216,   217,   230,   231,   233,   236,
     239,   245,   246,   253,   259,   260,   261,   263,   267,   273,
     277,   284,   289,   290,   291,   300,   302,   312,   319,   320,
     321,   322,   323,   333,   364,   365,   380,   405,   409,   411,
     412,   418,   419,   420,   421,   424,     1,     5,     6,    12,
      20,    29,    31,    33,    40,    41,    52,    56,    57,    63,
      65,    67,    68,    69,    70,    71,    72,    73,    74,    77,
      78,    79,    80,    81,    82,    83,    86,    88,    89,    90,
      91,    92,    93,    94,    95,    98,    99,   100,   102,   103,
     105,   106,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   119,   120,   121,   122,   123,   129,   130,   135,   136,
     137,   138,   139,   140,   143,   146,   147,   148,   149,   150,
     152,   153,   154,   155,   156,   157,   159,   160,   161,   162,
     164,   167,   168,   169,   175,   176,   177,   178,   179,   180,
     181,   182,   184,   185,   186,   190,   194,   195,   209,   210,
     211,   213,   214,   215,   217,   218,   219,   220,   222,   223,
     226,   227,   229,   236,   237,   238,   240,   241,   242,   243,
     244,   247,   248,   249,   250,   251,   254,   255,   256,   257,
     258,   262,   263,   264,   265,   266,   268,   269,   270,   271,
     272,   278,   279,   280,   281,   282,   283,   284,   285,   286,
     287,   288,   295,   298,   306,   308,   338,   339,   340,   341,
     342,   345,   346,   347,   348,   349,   351,   353,   355,   356,
     357,   361,   362,   363,   364,   379,   380,   381,   383,   386,
     387,   400,   401,   402,   404,   408,   411,   412,   425,   426,
     432,   439,     1,     1,   300,   300,    77,   294,     1,   365,
       1,   294,   365,     1,   300,     1,   294,     1,   300,     1,
     300,     1,   300,     1,   300,     1,     1,   300,     1,   296,
       1,   300,     1,   300,   300,     1,   300,     1,   300,     1,
     300,     1,   365,     1,   365,     1,   245,   365,     1,   245,
     365,     1,   246,   365,     1,    77,     1,   147,     1,   365,
     293,   307,   308,   310,   311,   313,   314,   315,   316,   317,
     318,   324,   325,   326,   327,   328,   330,   331,   332,   296,
     329,   377,     1,   341,   296,   294,     1,    12,    77,   147,
     291,   409,   420,   377,   430,     1,     1,    19,    51,    84,
      85,   132,   158,   166,   177,   183,   195,   414,   415,   416,
     417,     1,   174,   232,   318,     1,   414,   416,     1,   174,
     294,   303,   318,     1,   416,     1,    61,   245,   280,     1,
     365,    12,    61,    77,   114,   118,   138,   147,   236,   245,
       1,     1,    53,    61,    77,   101,   134,   141,   147,   212,
     221,   245,   246,   252,     1,     1,     1,     1,    29,    77,
       1,    29,    77,   414,   294,     1,   365,     1,   416,     1,
     365,     1,   414,   416,     1,    13,   365,     1,   416,     1,
      12,    29,    61,   141,   165,   177,   212,   221,   409,     1,
     174,   365,   414,     1,   300,     1,   365,     1,     1,   416,
       1,   300,   365,     1,   300,   365,     1,   416,     1,   365,
       1,   365,     1,   174,   303,   318,   436,     1,   365,     1,
     174,   365,     1,   416,     1,   416,   292,     1,     9,     1,
       9,     1,   365,     1,   365,     1,   414,   416,   436,     9,
       1,   365,     1,    13,   365,   294,     1,   365,     1,    13,
     365,     1,   365,     1,   365,     1,   174,   416,     1,   416,
       1,   416,     1,   416,     1,    97,   188,   366,   175,     1,
     300,     1,    12,    29,    77,   147,   165,   175,   177,   196,
     245,   246,   394,   395,     1,   428,     1,   416,     1,   174,
     232,   365,     1,   174,   232,     1,     1,     1,   174,     1,
     365,     1,   365,     1,   354,     1,   300,   174,   174,   232,
       1,   436,     1,   416,     1,   365,     1,   416,     1,   232,
     365,     1,    12,   247,   374,   232,     1,   416,     1,   185,
       1,     1,     1,   365,   414,     1,   436,     1,   365,     1,
     300,     1,     1,     1,     1,   427,     1,     1,   300,     1,
     300,     1,   300,   174,     1,   365,     1,     1,   300,     1,
     416,     1,   365,     1,   175,   196,   247,     1,   416,     1,
     300,     1,   365,     1,   365,     1,   299,   343,     1,     1,
     199,   224,   295,   304,     1,   365,   339,   346,   306,   339,
       1,   185,     1,   295,   365,     1,   301,   339,     1,   339,
     298,   365,   416,   365,   365,   436,   377,   436,   436,   161,
     296,   378,     1,   298,   365,   389,   416,   298,   365,   416,
     293,   293,   293,     1,    88,   146,   150,   175,   196,   214,
     215,   247,   365,   380,   412,   416,   431,   294,    12,   429,
     409,   410,   411,   412,   413,     1,   413,   413,   365,   365,
       1,    27,    58,   138,   208,     1,    34,    58,    60,   171,
     172,   173,   203,   204,   205,   225,   165,   291,   416,     1,
     203,   205,   301,   365,   399,   416,   399,   365,   399,   416,
     365,   409,     1,   380,   380,   365,   365,   365,   301,     1,
     365,     1,   422,     1,   365,     1,   365,     1,   365,     1,
     365,     1,   365,     1,   365,     1,   365,     1,   365,     1,
     365,   380,     1,   365,   380,     1,   365,     1,   365,     1,
     365,     1,   365,     1,   365,     1,   365,   365,   380,   296,
     235,     1,    30,   275,   276,   365,   407,   296,   300,     1,
     416,    51,   293,     1,     1,     1,   365,   245,   365,   274,
     388,   245,   365,     1,    34,    58,   171,   172,   173,   203,
     204,   225,     1,   365,     1,   365,     1,   365,   203,     1,
     360,   274,   365,   365,    77,   147,     1,    34,   203,   204,
     225,   365,   396,   366,   366,   366,   101,   365,   365,   365,
     365,   365,   365,     1,   365,   398,     1,   203,   293,   397,
     416,     1,    12,   196,   371,   365,   293,   413,     1,   392,
       1,   365,   416,   365,   350,   352,   339,   344,   345,     1,
     339,   345,   416,   416,   365,   393,   375,     1,   359,   301,
     399,   413,   365,   365,   365,     1,   380,   365,   365,   365,
     365,   299,   341,     1,   416,     1,   416,     1,   416,   339,
     365,   295,   301,   365,   365,   380,   365,   384,   365,   296,
     299,   390,    57,    99,   175,   186,   214,   223,   340,   299,
     391,     1,   399,     1,   399,     1,   399,   419,   413,    24,
      27,    32,    34,    44,    47,    48,    50,    54,    59,    60,
      61,    62,   141,   142,   145,   151,   192,   193,   204,   233,
     434,   300,    12,    29,   175,    12,    29,   175,   409,    64,
     186,    58,    77,   107,   108,   133,   147,   170,   187,   189,
     191,   212,   221,   231,   291,   294,   433,   434,   435,    39,
      44,    58,    59,    60,    61,    77,   107,   108,   133,   141,
     147,   170,   187,   189,   191,   192,   193,   204,   212,   221,
     231,   245,   246,   440,   441,   442,   301,   293,   301,   301,
     301,   293,   301,   293,   301,   293,   301,   301,   297,   296,
     293,   301,   293,   301,   301,   365,     1,     1,   297,   365,
     406,   275,   276,     1,   334,   297,     1,   365,   413,     1,
     399,   274,   274,   174,   274,   274,   365,   365,   365,   365,
     365,   365,   436,   436,   436,   436,   339,   378,   378,   378,
      12,    27,   291,   382,   293,   293,   293,   339,   436,   416,
     339,   372,   293,   395,   339,   305,   186,   339,   300,   368,
     369,   365,   301,   293,   293,   293,   293,   301,   299,   326,
     324,   325,   298,   297,   365,   341,   341,   403,   437,   438,
     413,     1,   365,   339,    64,    64,    77,   147,   365,   377,
     380,   434,    64,   365,   377,   365,    64,   365,    64,    77,
     147,    64,    64,    64,   365,     1,   399,     1,   399,   365,
     365,   407,     1,   380,   365,   309,   297,   365,   365,     1,
     297,   293,   365,   365,   365,   365,   366,   366,   365,   365,
     365,   365,   368,   365,   339,     1,   301,   366,   376,     1,
     293,   301,   365,   365,   365,     1,   380,   380,   380,   380,
     385,   297,   299,   299,   436,   377,   293,   365,   365,    64,
      64,    64,   365,    64,   365,    64,   365,    64,   365,    64,
     365,    64,    64,   365,   365,   365,   301,   301,   297,   301,
     293,   423,   365,   301,   301,   301,   373,   301,    12,    29,
      77,   147,   165,   175,   177,   196,   245,   246,   367,   295,
     342,   370,     1,   366,   301,   301,   301,   293,   341,   365,
     436,   365,   365,   365,   365,    64,   365,   365,    64,   365,
     365,   365,   365,   301,   416,   365,   301,   370,     1,   367,
       1,   380,   299,   365,   301,   365,   365,   301,   301
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (0)


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (N)								\
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (0)
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
              (Loc).first_line, (Loc).first_column,	\
              (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (&yylval, YYLEX_PARAM)
#else
# define YYLEX yylex (&yylval)
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (0)

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)		\
do {								\
  if (yydebug)							\
    {								\
      YYFPRINTF (stderr, "%s ", Title);				\
      yysymprint (stderr,					\
                  Type, Value);	\
      YYFPRINTF (stderr, "\n");					\
    }								\
} while (0)

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_stack_print (short int *bottom, short int *top)
#else
static void
yy_stack_print (bottom, top)
    short int *bottom;
    short int *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (/* Nothing. */; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_reduce_print (int yyrule)
#else
static void
yy_reduce_print (yyrule)
    int yyrule;
#endif
{
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu), ",
             yyrule - 1, yylno);
  /* Print the symbols being reduced, and their result.  */
  for (yyi = yyprhs[yyrule]; 0 <= yyrhs[yyi]; yyi++)
    YYFPRINTF (stderr, "%s ", yytname[yyrhs[yyi]]);
  YYFPRINTF (stderr, "-> %s\n", yytname[yyr1[yyrule]]);
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (Rule);		\
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      size_t yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

#endif /* YYERROR_VERBOSE */



#if YYDEBUG
/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yysymprint (FILE *yyoutput, int yytype, YYSTYPE *yyvaluep)
#else
static void
yysymprint (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);


# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  switch (yytype)
    {
      default:
        break;
    }
  YYFPRINTF (yyoutput, ")");
}

#endif /* ! YYDEBUG */
/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
        break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM);
# else
int yyparse ();
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */






/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM)
# else
int yyparse (YYPARSE_PARAM)
  void *YYPARSE_PARAM;
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int
yyparse (void)
#else
int
yyparse ()
    ;
#endif
#endif
{
  /* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;

  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  short int yyssa[YYINITDEPTH];
  short int *yyss = yyssa;
  short int *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK   (yyvsp--, yyssp--)

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* When reducing, the number of symbols on the RHS of the reduced
     rule.  */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short int *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	short int *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a look-ahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to look-ahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;


  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 3:

    { begin_local_scope(); ;}
    break;

  case 4:

    { end_local_scope(); ;}
    break;

  case 5:

    { (yyval).i = makenode(CMDLIST_,(yyvsp[0]).i,0); ;}
    break;

  case 6:

    { (yyval).i = (yyvsp[0]).i; /* for commands distinguishable by First tok */;}
    break;

  case 7:

    { int p = makenode(PIPE_,(yyvsp[0]).i,0);
            subtree_swap(&(yyvsp[-2]).i,&p); /* so pipe executed first */
            (yyval).i = makenode(PIPE_END_,p,(yyvsp[-2]).i);
          ;}
    break;

  case 8:

    {kb_error(2330,"Piping must be to quoted string or string expression.\n",Q_ERROR);;}
    break;

  case 9:

    { int p = makenode(REDIRECT_,(yyvsp[0]).i,0);
                            subtree_swap(&(yyvsp[-2]).i,&p); /* so file openedfirst */
                            (yyval).i = makenode(REDIRECT_END_,p,(yyvsp[-2]).i);
                            ;}
    break;

  case 10:

    {
  kb_error(2331,
     "Redirection must be to quoted string or string expression.\n",Q_ERROR);;}
    break;

  case 11:

    { int p = makenode(REDIRECTOVER_,(yyvsp[0]).i,0);
                subtree_swap(&(yyvsp[-2]).i,&p); /* so file openedfirst */
                (yyval).i = makenode(REDIRECT_END_,p,(yyvsp[-2]).i);
              ;}
    break;

  case 12:

    {
  kb_error(2332,
  "Redirection must be to quoted string or string expression.\n",Q_ERROR);;}
    break;

  case 13:

    { (yyval).i = makenode(SET_BREAKPOINT_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 14:

    { (yyval).i = makenode(SET_BREAKPOINT_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 15:

    { (yyval).i = makenode(SET_BREAKPOINT_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 16:

    { kb_error(5981,"Syntax: breakpoint procedurename linenumber\n",
                  Q_ERROR); ;}
    break;

  case 17:

    { (yyval).i = makenode(UNSET_BREAKPOINT_,0,0); ;}
    break;

  case 18:

    { (yyval).i = makenode(WHEREAMI_COMMAND_,0,0); ;}
    break;

  case 19:

    { kb_error(3988,"Illegal command syntax.\n", Q_ERROR); ;}
    break;

  case 20:

    { (yyval).i = makenode(PROCEDURE_,(yyvsp[0]).i,0); ;}
    break;

  case 21:

    { int init = makenode(REPEAT_INIT_,(yyvsp[0]).i,0);
                       (yyval).i = makenode(PROCEDURE_,(yyvsp[-1]).i,0);
                          (yyval).i = makenode(REPEAT_,init,(yyval).i); ;}
    break;

  case 22:

    {
    kb_error(3600,"Missing semicolon?",Q_ERROR);
           ;}
    break;

  case 23:

    { (yyval).i = makenode(PERM_PROCEDURE_,(yyvsp[0]).i,0); ;}
    break;

  case 24:

    { int init = makenode(REPEAT_INIT_,(yyvsp[0]).i,0);
                       (yyval).i = makenode(PERM_PROCEDURE_,(yyvsp[-1]).i,0);
                          (yyval).i = makenode(REPEAT_,init,(yyval).i); ;}
    break;

  case 25:

    {
    kb_error(3601,"Procedure has no arguments; can be followed by repetition count.",Q_ERROR);
           ;}
    break;

  case 26:

    { kb_error(2333,"Missing semicolon?\n",Q_ERROR); ;}
    break;

  case 27:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 28:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 29:

    { begin_local_scope(); ;}
    break;

  case 30:

    { end_local_scope(); (yyval).i = makenode(COMMAND_BLOCK_,(yyvsp[-1]).i,0); ;}
    break;

  case 31:

    { (yyval).i = makenode(NULLBLOCK_,0,0); ;}
    break;

  case 32:

    { kb_error(3602,"Error following '{'",Q_ERROR); ;}
    break;

  case 33:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 34:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 35:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 36:

    { int init = makenode(REPEAT_INIT_,(yyvsp[0]).i,0);
       subtree_swap(&(yyvsp[-1]).i,&init);
       (yyval).i = makenode(REPEAT_,init,(yyvsp[-1]).i); 
       ;}
    break;

  case 37:

    { kb_error(3603,
   "Error following command block; expected ';' or repetition count or nothing.",
   Q_ERROR);
  ;}
    break;

  case 38:

    { (yyval).i = makenode(NULLCMD_,0,0); ;}
    break;

  case 39:

    { (yyval).i = (yyvsp[-1]).i; ;}
    break;

  case 40:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 41:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 42:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 43:

    { (yyval).i = makenode(CMDLIST_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 44:

    { (yyval).i = makenode(CMDLIST_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 45:

    { (yyval).i = makenode(IFTEST_,(yyvsp[0]).i,0); ;}
    break;

  case 46:

    { (yyval).i = makenode(IF_,(yyvsp[-2]).i,(yyvsp[0]).i); ;}
    break;

  case 47:

    { kb_error(2334,"Syntax: IF rexpr THEN command [ ELSE command ]\n",Q_ERROR);;}
    break;

  case 48:

    {(yyval).i = makenode(ELSE_,(yyvsp[0]).i,0); ;}
    break;

  case 49:

    { (yyval).i = makenode(ELSE_,(yyvsp[-2]).i,(yyvsp[0]).i); ;}
    break;

  case 50:

    { kb_error(2335,"Illegal ELSE.  Don't use semicolon before ELSE.\n",Q_ERROR); ;}
    break;

  case 51:

    { (yyval).i = makenode(SINGLE_LETTER_,'?',0); ;}
    break;

  case 52:

    { (yyval).i = makenode(GEOMVIEW_,(yyvsp[0]).i,0); ;}
    break;

  case 53:

    { (yyval).i = makenode(GEOMVIEW_TOGGLE_,(yyvsp[0]).i,0); ;}
    break;

  case 54:

    { (yyval).i = makenode(GEOMVIEW_TOGGLE_,ON_,0); ;}
    break;

  case 55:

    { kb_error(2336,"Syntax: GEOMVIEW ON|OFF or GEOMVIEW \"geomview command\"\n",Q_ERROR); ;}
    break;

  case 56:

    { (yyval).i = makenode(GEOMPIPE_,(yyvsp[0]).i,0); ;}
    break;

  case 57:

    { (yyval).i = makenode(GEOMPIPE_TOGGLE_,(yyvsp[0]).i,0); ;}
    break;

  case 58:

    { (yyval).i = makenode(GEOMPIPE_TOGGLE_,ON_,0); ;}
    break;

  case 59:

    { kb_error(2337,"Syntax: GEOMPIPE ON|OFF or GEOMPIPE \"shell command\"\n",Q_ERROR); ;}
    break;

  case 60:

    { (yyval).i = makenode(LOGFILE_,(yyvsp[0]).i,0); ;}
    break;

  case 61:

    { (yyval).i = makenode(LOGFILE_TOGGLE_,(yyvsp[0]).i,0); ;}
    break;

  case 62:

    { (yyval).i = makenode(LOGFILE_TOGGLE_,ON_,0); ;}
    break;

  case 63:

    { kb_error(2338,"Syntax: LOGFILE ON|OFF or LOGFILE \"filename\"\n",Q_ERROR); ;}
    break;

  case 64:

    { (yyval).i = makenode(KEYLOGFILE_,(yyvsp[0]).i,0); ;}
    break;

  case 65:

    { (yyval).i = makenode(KEYLOGFILE_TOGGLE_,(yyvsp[0]).i,0); ;}
    break;

  case 66:

    { (yyval).i = makenode(KEYLOGFILE_TOGGLE_,ON_,0); ;}
    break;

  case 67:

    { kb_error(2419,"Syntax: KEYLOGFILE ON|OFF or KEYLOGFILE \"filename\"\n",Q_ERROR); ;}
    break;

  case 68:

    { (yyval).i = makenode( POSTSCRIPT_,(yyvsp[0]).i,0); ;}
    break;

  case 69:

    { kb_error(3361,"Syntax: POSTSCRIPT \"filename\"\n",Q_ERROR); ;}
    break;

  case 70:

    { (yyval).i = makenode( BINARY_OFF_FILE_,(yyvsp[0]).i,0); ;}
    break;

  case 71:

    { kb_error(4339,"Syntax: BINARY_OFF_FILE \"filename\"\n",Q_ERROR); ;}
    break;

  case 72:

    { (yyval).i = makenode( OOGLFILE_,(yyvsp[0]).i,0); ;}
    break;

  case 73:

    { kb_error(2339,"Syntax: OOGLFILE \"filename\"\n",Q_ERROR); ;}
    break;

  case 74:

    { (yyval).i = makenode(HISTORY_,0,0); ;}
    break;

  case 75:

    { kb_error(2340,"Syntax: HISTORY   (no arguments)\n",Q_ERROR); ;}
    break;

  case 76:

    { (yyval).i = makenode(RETURN_,0,0); ;}
    break;

  case 77:

    { (yyval).i = makenode(RETURN_,(yyvsp[0]).i,0); ;}
    break;

  case 78:

    { kb_error(2341,"Syntax: RETURN [expr] \n",Q_ERROR); ;}
    break;

  case 79:

    { (yyval).i = makenode(BREAK_,1,0); ;}
    break;

  case 80:

    { (yyval).i = makenode(BREAK_,(yyvsp[0]).i,0); ;}
    break;

  case 81:

    { kb_error(2342,"Syntax: BREAK   or  BREAK integer   (to break multiple levels)\n",Q_ERROR); ;}
    break;

  case 82:

    { (yyval).i = makenode(CONTINUE_,1,0); ;}
    break;

  case 83:

    { (yyval).i = makenode(CONTINUE_,(yyvsp[0]).i,0); ;}
    break;

  case 84:

    { kb_error(2343,"Syntax: CONTINUE   or  CONTINUE integer   (to continue in higher level loop)\n",Q_ERROR); ;}
    break;

  case 85:

    { 
                         (yyval).i = makenode(SINGLE_LETTER_,'g',0);
                     ;}
    break;

  case 86:

    {   int init,count,g;
                         real_val = (yyvsp[0]).i;
                         count = makenode(PUSHCONST,0,0);
                         init = makenode(REPEAT_INIT_,count,0);
                         g = makenode(SINGLE_LETTER_,'g',0);
                         (yyval).i = makenode(REPEAT_,init,g); 
                     ;}
    break;

  case 87:

    { int init,g;
                         init = makenode(REPEAT_INIT_,(yyvsp[0]).i,0);
                         g = makenode(SINGLE_LETTER_,'g',0);
                         (yyval).i = makenode(REPEAT_,init,g); 
                       ;}
    break;

  case 88:

    { kb_error(3666,"Syntax: GO count\n",Q_ERROR); ;}
    break;

  case 89:

    { (yyval).i = makenode(WHILE_TOP_,(yyvsp[0]).i,0); ;}
    break;

  case 90:

    { (yyval).i = (yyvsp[-1]).i; ;}
    break;

  case 91:

    { (yyval).i = makenode(WHILE_END_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 92:

    { kb_error(2344,"Syntax: WHILE rexpr DO command\n",Q_ERROR); ;}
    break;

  case 93:

    { (yyval).i = makenode(DO_ENTRY_,0,0);;}
    break;

  case 94:

    { (yyval).i = makenode(DO_TOP_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 95:

    { (yyval).i = makenode(DO_END_,(yyvsp[-2]).i,(yyvsp[0]).i); ;}
    break;

  case 96:

    { kb_error(4345,"Missing WHILE at end of DO statement.\n",Q_ERROR); ;}
    break;

  case 97:

    { kb_error(2345,"Syntax: DO command WHILE expr\n",Q_ERROR); ;}
    break;

  case 98:

    { (yyval).i = makenode(FOR_ENTRY_,(yyvsp[0]).i,0); ;}
    break;

  case 99:

    { (yyval).i = makenode(FOR_HEAD_,(yyvsp[-2]).i,(yyvsp[-1]).i); ;}
    break;

  case 100:

    { int tmp;
              real_val = 1;
              tmp = makenode(PUSHCONST,0,0);
              (yyval).i = makenode(FOR_HEAD_,(yyvsp[-1]).i,tmp); 
            ;}
    break;

  case 101:

    { (yyval).i = makenode(FOR_TOP_,(yyvsp[-2]).i,(yyvsp[-1]).i); ;}
    break;

  case 102:

    { int  tmp = makenode(NULLCMD_,0,0);
                          (yyval).i = makenode(FOR_TOP_,(yyvsp[-1]).i,tmp);
               ;}
    break;

  case 103:

    { (yyval).i = makenode(FOR_END_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 104:

    { kb_error(2514,
        "Syntax: FOR ( command ; rexpr ; command ) command\n",Q_ERROR); ;}
    break;

  case 105:

    { kb_error(3668,
        "Error in initializer of FOR loop.\n",Q_ERROR); ;}
    break;

  case 106:

    { kb_error(3669,
        "Error in test expression of FOR loop.\n",Q_ERROR); ;}
    break;

  case 107:

    { kb_error(3670,
        "Error in increment part of FOR loop.\n",Q_ERROR); ;}
    break;

  case 108:

    { kb_error(2844,
        "Bad FOR loop body.  Try starting body on same line as FOR header.\n",
     Q_ERROR); ;}
    break;

  case 109:

    { (yyval).i = makenode(SINGLE_LETTER_,(yyvsp[0]).i,0); ;}
    break;

  case 110:

    { (yyval).i = makenode(SINGLE_REDEFD_,(yyvsp[0]).i,0); ;}
    break;

  case 111:

    { int init = makenode(REPEAT_INIT_,(yyvsp[0]).i,0);
                       (yyval).i = makenode(SINGLE_REDEFD_,(yyvsp[-1]).i,0);
                        (yyval).i = makenode(REPEAT_,init,(yyval).i); ;}
    break;

  case 112:

    { kb_error(3671,
     "Expected repetition count after redefined single letter.\n",Q_ERROR); ;}
    break;

  case 113:

    { (yyval).i = makenode(SINGLE_LETTER_,(yyvsp[0]).i,0); ;}
    break;

  case 114:

    { int init = makenode(REPEAT_INIT_,(yyvsp[0]).i,0);
                       (yyval).i = makenode(SINGLE_LETTER_,(yyvsp[-1]).i,0);
                         (yyval).i = makenode(REPEAT_,init,(yyval).i); ;}
    break;

  case 115:

    { kb_error(3672,
     "Expected repetition count after single letter command.\n",Q_ERROR); ;}
    break;

  case 116:

    { assigntype = ASSIGN_;
           switch ((yyvsp[-1]).i)
             { case 't': (yyval).i = makenode(EDGEWEED_,(yyvsp[0]).i,0); break;
               case 'w': (yyval).i = makenode(AREAWEED_,(yyvsp[0]).i,0); break;
               case 'l': (yyval).i = makenode(EDGEDIVIDE_,(yyvsp[0]).i,0); break;
               case 'm': (yyval).i = makenode(SET_SCALE_,(yyvsp[0]).i,0); break;
               case 'n': (yyval).i = makenode(NOTCH_,(yyvsp[0]).i,0); break;
               case 'j': (yyval).i = makenode(JIGGLE_,(yyvsp[0]).i,0); break;
               case 'G': (yyval).i = makenode(SET_GRAVITY_,(yyvsp[0]).i,0); break;
               case 'P': (yyval).i = makenode(INVOKE_P_MENU_,(yyvsp[0]).i,0); break;
               case 'M': (yyval).i = makenode(SET_MODEL_,(yyvsp[0]).i,0); break;
               case 'y': (yyval).i = makenode(TORDUP_,(yyvsp[0]).i,0); break;
               case 'K': (yyval).i = makenode(SKINNY_,(yyvsp[0]).i,0); break;
               case 'k': (yyval).i = makenode(SET_GAP_CONSTANT_,(yyvsp[0]).i,0); break;
               case 'p': (yyval).i = makenode(SET_AMBIENT_PRESSURE_,(yyvsp[0]).i,0); break;
               default: kb_error(1884,"Extra expression after single letter command.\n",Q_ERROR);
             }
        ;}
    break;

  case 117:

    { kb_error(3660,
     "Expected argument after single letter command.\n",Q_ERROR); ;}
    break;

  case 118:

    { (yyval).i = makenode(NOP_,0,0); ;}
    break;

  case 119:

    { (yyval).i = makenode(READ_,(yyvsp[0]).i,0); ;}
    break;

  case 120:

    { kb_error(2346,"Syntax: READ \"filename\"   (need quoted string or string expression)\n",Q_ERROR);;}
    break;

  case 121:

    { (yyval).i = makenode(TRANSFORM_DEPTH_,(yyvsp[0]).i,0);;}
    break;

  case 122:

    { (yyval).i = makenode(TRANSFORM_DEPTH_,(yyvsp[0]).i,0);;}
    break;

  case 123:

    { kb_error(2348,"Syntax: TRANSFORM_DEPTH := integer\n",Q_ERROR);;}
    break;

  case 124:

    { (yyval).i = makenode(TRANSFORM_EXPR_,(yyvsp[0]).i,0);;}
    break;

  case 125:

    { (yyval).i = makenode(TRANSFORM_EXPR_,(yyvsp[0]).i,0);;}
    break;

  case 126:

    { kb_error(2349,"Syntax: TRANSFORM_EXPR := string    (quoted string or string expression) \n",Q_ERROR); ;}
    break;

  case 127:

    { (yyval).i = makenode(SYSTEM_,(yyvsp[0]).i,0); ;}
    break;

  case 128:

    { kb_error(2350,"Syntax: SYSTEM \"command\"   (need quoted string or string expression)\n",Q_ERROR);;}
    break;

  case 129:

    { (yyval).i = makenode(EXEC_,(yyvsp[0]).i,0); ;}
    break;

  case 130:

    { kb_error(2351,"Syntax: EXEC string (need quoted string or string expression)\n",Q_ERROR);;}
    break;

  case 131:

    { (yyval).i = makenode(PARALLEL_EXEC_,(yyvsp[0]).i,0); ;}
    break;

  case 132:

    { kb_error(3115,"Syntax: PARALLEL_EXEC string (need quoted string or string expression)\n",Q_ERROR);;}
    break;

  case 133:

    { (yyval).i = makenode(TASK_EXEC_,(yyvsp[-2]).i,(yyvsp[0]).i); ;}
    break;

  case 134:

    { kb_error(3119,"Syntax: TASK_EXEC nodenumber, string (need quoted string or string expression)\n",Q_ERROR);;}
    break;

  case 135:

    { (yyval).i = makenode(CHDIR_,(yyvsp[0]).i,0); ;}
    break;

  case 136:

    { kb_error(2352,"Syntax: CHDIR \"command\"   (need quoted string or string expression)\n",Q_ERROR);;}
    break;

  case 137:

    { (yyval).i = makenode(METIS_,(yyvsp[0]).i,0); ;}
    break;

  case 138:

    { kb_error(3236,"Syntax: METIS numparts\n",Q_ERROR); ;}
    break;

  case 139:

    { (yyval).i = makenode(KMETIS_,(yyvsp[0]).i,0); ;}
    break;

  case 140:

    { kb_error(2354,"Syntax: KMETIS numparts\n",Q_ERROR); ;}
    break;

  case 141:

    { (yyval).i = makenode(METIS_READJUST_,(yyvsp[0]).i,0); ;}
    break;

  case 142:

    { kb_error(3237,"Syntax: METIS_READJUST numparts\n",Q_ERROR); ;}
    break;

  case 143:

    { (yyval).i = makenode(BODY_METIS_,(yyvsp[0]).i,0); ;}
    break;

  case 144:

    { kb_error(3775,"Syntax: BODY_METIS numparts\n",Q_ERROR); ;}
    break;

  case 145:

    { (yyval).i = makenode(OMETIS_,(yyvsp[0]).i,0); ;}
    break;

  case 146:

    { (yyval).i = makenode(OMETIS_,0,0); ;}
    break;

  case 147:

    { kb_error(2355,"Syntax: OMETIS   or   OMETIS expr\n",Q_ERROR); ;}
    break;

  case 148:

    { (yyval).i = makenode(EDGEWEED_,(yyvsp[0]).i,0); ;}
    break;

  case 149:

    { kb_error(2356,"Syntax: EDGEWEED minlength\n",Q_ERROR);;}
    break;

  case 150:

    { (yyval).i = makenode(AREAWEED_,(yyvsp[0]).i,0); ;}
    break;

  case 151:

    { kb_error(2357,"Syntax: AREAWEED minarea\n",Q_ERROR);;}
    break;

  case 152:

    { (yyval).i = makenode(EDGEDIVIDE_,(yyvsp[0]).i,0); ;}
    break;

  case 153:

    { kb_error(2358,"Syntax: EDGE_DIVIDE maxlength\n",Q_ERROR);;}
    break;

  case 154:

    { (yyval).i = makenode(LANCZOS_,(yyvsp[-3]).i,(yyvsp[-1]).i); ;}
    break;

  case 155:

    { (yyval).i = makenode(LANCZOS_,(yyvsp[0]).i,0); ;}
    break;

  case 156:

    { kb_error(2359,"Syntax: lanczos rexpr   or   lanczos(expr,count) \n",Q_ERROR);;}
    break;

  case 157:

    { (yyval).i = makenode(RITZ_,(yyvsp[-3]).i,(yyvsp[-1]).i); ;}
    break;

  case 158:

    { kb_error(2360,"Syntax: RITZ(probe_value, number_of_eigenvalues)\n",Q_ERROR); ;}
    break;

  case 159:

    { (yyval).i = makenode(EIGENPROBE_,(yyvsp[-3]).i,(yyvsp[-1]).i); ;}
    break;

  case 160:

    { (yyval).i = makenode(EIGENPROBE_,(yyvsp[0]).i,0); ;}
    break;

  case 161:

    { kb_error(2361,
 "Syntax: EIGENPROBE probe_value   or  EIGENPROBE(probe_value, iterationmax)\n",Q_ERROR); ;}
    break;

  case 162:

    { (yyval).i = makenode(MOVE_,(yyvsp[0]).i,0); ;}
    break;

  case 163:

    { kb_error(2362,"Syntax: MOVE stepsize\n",Q_ERROR); ;}
    break;

  case 164:

    { (yyval).i = makenode(HESSIAN_SADDLE_,0,0); ;}
    break;

  case 165:

    { (yyval).i = makenode(HESSIAN_SADDLE_,(yyvsp[0]).i,0); ;}
    break;

  case 166:

    { kb_error(2363,"Syntax: SADDLE   or   SADDLE maxstepsize\n",
   Q_ERROR); ;}
    break;

  case 167:

    { (yyval).i = makenode(HESSIAN_SEEK_,0,0); ;}
    break;

  case 168:

    { (yyval).i = makenode(HESSIAN_SEEK_,(yyvsp[0]).i,0); ;}
    break;

  case 169:

    { kb_error(2364,
   "Syntax: HESSIAN_SEEK   or   HESSIAN_SEEK maxstepsize\n", Q_ERROR); ;}
    break;

  case 170:

    { (yyval).i = makenode(COUNTS_,0,0); ;}
    break;

  case 171:

    { (yyval).i = makenode(SINGLE_LETTER_,'q',0); ;}
    break;

  case 172:

    { (yyval).i = makenode(SUBCOMMAND_,'q',0); ;}
    break;

  case 173:

    { (yyval).i = makenode(ABORT_,'q',0); ;}
    break;

  case 174:

    { (yyval).i = makenode(SIMPLEX_TO_FE_,'q',0); ;}
    break;

  case 175:

    { (yyval).i = makenode(REORDER_STORAGE_,0,0); ;}
    break;

  case 176:

    { (yyval).i = makenode(RENUMBER_ALL_,0,0); ;}
    break;

  case 177:

    { (yyval).i = makenode(DUMP_MEMLIST_,0,0); ;}
    break;

  case 178:

    { (yyval).i = makenode(FREE_DISCARDS_,0,0); ;}
    break;

  case 179:

    { (yyval).i = makenode(REPARTITION_,0,0); ;}
    break;

  case 180:

    { (yyval).i = makenode(EXTRAPOLATE_,0,0); ;}
    break;

  case 181:

    { (yyval).i = makenode(REBODY_,0,0); ;}
    break;

  case 182:

    { (yyval).i = makenode(ZOOM_,0,0); ;}
    break;

  case 183:

    { (yyval).i = makenode(ZOOM_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 184:

    { kb_error(2365,"Syntax: ZOOM [ vertex_id radius ]\n",Q_ERROR);;}
    break;

  case 185:

    { (yyval).i = makenode(BURCHARD_,(yyvsp[0]).i,0); ;}
    break;

  case 186:

    { (yyval).i = makenode(LAGRANGE_,(yyvsp[0]).i,0); ;}
    break;

  case 187:

    { kb_error(2366,"Syntax: LAGRANGE order\n",Q_ERROR); ;}
    break;

  case 188:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 189:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 190:

    { (yyval).i = makenode(PRINT_PROFILING_,0,0); ;}
    break;

  case 191:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 192:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 193:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 194:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 195:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 196:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 197:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 198:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 199:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 200:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 201:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 202:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 203:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 204:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 205:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 206:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 207:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 208:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 209:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 210:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 211:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 212:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 213:

    { (yyval).i = makenode(SINGLE_LETTER_,'h',0); ;}
    break;

  case 214:

    { (yyval).i = makenode(tok,0,0); ;}
    break;

  case 215:

    { (yyval).i = makenode(TOPINFO_,0,0); ;}
    break;

  case 216:

    { (yyval).i = makenode(BOTTOMINFO_,0,0); ;}
    break;

  case 217:

    { (yyval).i = makenode(LIST_ATTRIBUTES_,0,0); ;}
    break;

  case 218:

    { (yyval).i = makenode(LIST_PROCS_,0,0); ;}
    break;

  case 219:

    { (yyval).i = makenode(LIST_BOUNDARY_,(yyvsp[0]).i,0);;}
    break;

  case 220:

    { int k = makenode(PUSHCONST,(yyvsp[0]).i,0);
              (yyval).i = makenode(LIST_BOUNDARY_,k,0);
              list[(yyval).i].op1.bdry_id = (yyvsp[0]).i;
            ;}
    break;

  case 221:

    { (yyval).i = makenode(LIST_CONSTRAINT_,(yyvsp[0]).i,0);;}
    break;

  case 222:

    { int k = makenode(PUSHCONST,(yyvsp[0]).i,0);
              (yyval).i = makenode(LIST_CONSTRAINT_,k,0);
              list[(yyval).i].op1.con_id = (yyvsp[0]).i;
            ;}
    break;

  case 223:

    { (yyval).i = makenode(LIST_QUANTITY_,(yyvsp[0]).i,0);;}
    break;

  case 224:

    { (yyval).i = makenode(LIST_QUANTITY_,(yyvsp[0]).i,0);;}
    break;

  case 225:

    { (yyval).i = makenode(LIST_METHOD_INSTANCE_,(yyvsp[0]).i,0);;}
    break;

  case 226:

    { (yyval).i = makenode(LIST_METHOD_INSTANCE_,(yyvsp[0]).i,0);;}
    break;

  case 227:

    { (yyval).i = makenode(CLOSE_SHOW_,0,0); ;}
    break;

  case 228:

    { (yyval).i = makenode((yyvsp[-1]).i,(yyvsp[0]).i,0); ;}
    break;

  case 229:

    { (yyval).i = makenode((yyvsp[0]).i,ON_,0); ;}
    break;

  case 230:

    { (yyval).i = makenode((yyvsp[-1]).i,(yyvsp[0]).i,0); ;}
    break;

  case 231:

    { (yyval).i = makenode((yyvsp[0]).i,ON_,0); ;}
    break;

  case 232:

    { (yyval).i = makenode((yyvsp[-1]).i,(yyvsp[0]).i,0); ;}
    break;

  case 233:

    { (yyval).i = makenode((yyvsp[0]).i,ON_,0); ;}
    break;

  case 234:

    { (yyval).i = makenode((yyvsp[0]).i,ON_,0); ;}
    break;

  case 235:

    {verb_flag=0;;}
    break;

  case 236:

    { YYACCEPT; ;}
    break;

  case 237:

    { YYACCEPT; ;}
    break;

  case 238:

    {verb_flag=0;;}
    break;

  case 239:

    { assigntype = (yyvsp[-2]).i; (yyval).i = makenode(SET_INTERNAL_,(yyvsp[-3]).i,(yyvsp[0]).i); ;}
    break;

  case 240:

    { kb_error(3673,"Expected expression to assign to internal variable.\n",Q_ERROR);;}
    break;

  case 241:

    {verb_flag=0;;}
    break;

  case 242:

    { assigntype = ASSIGN_; (yyval).i = makenode(SET_INTERNAL_,(yyvsp[-2]).i,(yyvsp[0]).i); ;}
    break;

  case 243:

    { kb_error(3661,"Expected expression for setting internal variable.\n",Q_ERROR);;}
    break;

  case 244:

    { (yyval).i = makenode(SET_GRAVITY_,(yyvsp[0]).i,0); ;}
    break;

  case 245:

    { kb_error(3675,"Expected expression for setting gravity.\n",Q_ERROR);;}
    break;

  case 246:

    {assigntype = (yyvsp[-1]).i; (yyval).i = makenode(SET_GRAVITY_,(yyvsp[0]).i,0); ;}
    break;

  case 247:

    { kb_error(2367,"Syntax: GRAVITY := rexpr \n       GRAVITY ON|OFF\n",Q_ERROR);;}
    break;

  case 248:

    { (yyval).i = makenode(SET_CONSTRAINT_GLOBAL,(yyvsp[-1]).i,0); ;}
    break;

  case 249:

    { (yyval).i = makenode(UNSET_CONSTRAINT_GLOBAL,(yyvsp[-1]).i,0); ;}
    break;

  case 250:

    { (yyval).i = makenode(SET_CONSTRAINT_NAME_GLOBAL,(yyvsp[-1]).i,0); ;}
    break;

  case 251:

    { (yyval).i = makenode(SET_CONSTRAINT_NAME_GLOBAL,(yyvsp[-1]).i,0); ;}
    break;

  case 252:

    { (yyval).i = makenode(SET_CONSTRAINT_NAME_GLOBAL,(yyvsp[-1]).i,0); ;}
    break;

  case 253:

    { (yyval).i = makenode(SET_CONSTRAINT_NAME_GLOBAL,(yyvsp[-1]).i,0); ;}
    break;

  case 254:

    { assigntype = ASSIGN_;  (yyval).i = makenode(SET_INTERNAL_,V_SCALE,(yyvsp[0]).i); ;}
    break;

  case 255:

    { kb_error(3676,"Syntax: SET SCALE expr\n",Q_ERROR);;}
    break;

  case 256:

    { assigntype = (yyvsp[-1]).i;  (yyval).i = makenode(SET_INTERNAL_,V_SCALE,(yyvsp[0]).i); ;}
    break;

  case 257:

    { kb_error(3677,"Syntax: SCALE := expr\n",Q_ERROR);;}
    break;

  case 258:

    { assigntype = ASSIGN_;  (yyval).i = makenode(SET_INTERNAL_,V_DIFFUSION,(yyvsp[0]).i); ;}
    break;

  case 259:

    { kb_error(3662,"Syntax: SET DIFFUSION expr\n",Q_ERROR);;}
    break;

  case 260:

    { assigntype = (yyvsp[-1]).i;  (yyval).i = makenode(SET_INTERNAL_,V_GAP_CONSTANT,(yyvsp[0]).i); ;}
    break;

  case 261:

    { kb_error(2369,"Syntax: GAP_CONSTANT := expr\n",Q_ERROR);;}
    break;

  case 262:

    { (yyval).i = makenode(SET_FIXED_AREA_,(yyvsp[0]).i,0); ;}
    break;

  case 263:

    { (yyval).i = makenode(NOTCH_,(yyvsp[0]).i,0); ;}
    break;

  case 264:

    { kb_error(2371,"Syntax: NOTCH maxangle\n",Q_ERROR);;}
    break;

  case 265:

    { (yyval).i = makenode(SET_AUTOCHOP_,(yyvsp[0]).i,0); ;}
    break;

  case 266:

    { (yyval).i = makenode(SET_AUTOCHOP_,(yyvsp[0]).i,0); ;}
    break;

  case 267:

    { kb_error(2372,
       "Syntax: AUTOCHOP ON|OFF  or AUTOCHOP choplength\n", Q_ERROR); ;}
    break;

  case 268:

    { assigntype = (yyvsp[-1]).i; (yyval).i = makenode(SET_QTARGET_,(yyvsp[0]).i,(yyvsp[-4]).i); ;}
    break;

  case 269:

    { assigntype = (yyvsp[-1]).i; (yyval).i = makenode(SET_QMODULUS_,(yyvsp[0]).i,(yyvsp[-4]).i); ;}
    break;

  case 270:

    { assigntype = (yyvsp[-1]).i; (yyval).i = makenode(SET_QTOLERANCE_,(yyvsp[0]).i,(yyvsp[-4]).i); ;}
    break;

  case 271:

    { assigntype = (yyvsp[-1]).i; (yyval).i = makenode(SET_MMODULUS_,(yyvsp[0]).i,(yyvsp[-4]).i); ;}
    break;

  case 272:

    { assigntype = (yyvsp[-1]).i; (yyval).i = makenode(SET_QVOLCONST_,(yyvsp[0]).i,(yyvsp[-4]).i); ;}
    break;

  case 273:

    { kb_error(3372,
    "Syntax: QUANTITY_NAME . TARGET|MODULUS|TOLERANCE|VOLCONST := expr\n", 
     Q_ERROR); ;}
    break;

  case 274:

    { kb_error(3379,
    "Syntax: METHOD_NAME . MODULUS\n", Q_ERROR); ;}
    break;

  case 275:

    { assigntype = ASSIGN_; (yyval).i = makenode(SET_QTARGET_,(yyvsp[0]).i,(yyvsp[-2]).i); ;}
    break;

  case 276:

    { assigntype = ASSIGN_; (yyval).i = makenode(SET_QMODULUS_,(yyvsp[0]).i,(yyvsp[-2]).i); ;}
    break;

  case 277:

    { assigntype = ASSIGN_; (yyval).i = makenode(SET_QTOLERANCE_,(yyvsp[0]).i,(yyvsp[-2]).i); ;}
    break;

  case 278:

    { assigntype = ASSIGN_; (yyval).i = makenode(SET_MMODULUS_,(yyvsp[0]).i,(yyvsp[-2]).i); ;}
    break;

  case 279:

    { assigntype = ASSIGN_; (yyval).i = makenode(SET_QVOLCONST_,(yyvsp[0]).i,(yyvsp[-2]).i); ;}
    break;

  case 280:

    { (yyval).i = makenode(SET_Q_FIXED_,(yyvsp[-1]).i,0); ;}
    break;

  case 281:

    { (yyval).i = makenode(SET_Q_INFO_,(yyvsp[-1]).i,0); ;}
    break;

  case 282:

    { (yyval).i = makenode(SET_Q_ENERGY_,(yyvsp[-1]).i,0); ;}
    break;

  case 283:

    { (yyval).i = makenode(SET_Q_CONSERVED_,(yyvsp[-1]).i,0); ;}
    break;

  case 284:

    { strcpy(errmsg,"Syntax:\n");
          strcat(errmsg,"  SET quantityname TARGET expr\n");
          strcat(errmsg,"  SET quantityname MODULUS expr\n");
          strcat(errmsg,"  SET quantityname TOLERANCE expr\n");
          strcat(errmsg,"  SET quantityname VOLCONST expr\n");
          strcat(errmsg,"  SET quantityname FIXED\n");
          strcat(errmsg,"  SET quantityname INFO_ONLY\n");
          strcat(errmsg,"  SET quantityname ENERGY\n");
          strcat(errmsg,"  SET quantityname CONSERVED\n");
          kb_error(3663,errmsg,Q_ERROR);
        ;}
    break;

  case 285:

    { (yyval).i = makenode(SUPPRESS_WARNING_,(yyvsp[0]).i,0); ;}
    break;

  case 286:

    { kb_error(3456,
              "Syntax: SUPPRESS_WARNING number\n",Q_ERROR) ;}
    break;

  case 287:

    { (yyval).i = makenode(UNSUPPRESS_WARNING_,(yyvsp[0]).i,0); ;}
    break;

  case 288:

    { kb_error(3457,
              "Syntax: UNSUPPRESS_WARNING number\n",Q_ERROR) ;}
    break;

  case 289:

    { (yyval).i = makenode(LOAD_,(yyvsp[0]).i,0); ;}
    break;

  case 290:

    { kb_error(2373,"Syntax: LOAD \"filename\"   (need quoted string or string expression)\n",Q_ERROR);;}
    break;

  case 291:

    { (yyval).i = makenode(ADDLOAD_,(yyvsp[0]).i,0); ;}
    break;

  case 292:

    { kb_error(3544,"Syntax: ADDLOAD \"filename\"   (need quoted string or string expression)\n",Q_ERROR);;}
    break;

  case 293:

    { (yyval).i = makenode(PERMLOAD_,(yyvsp[0]).i,0); ;}
    break;

  case 294:

    { kb_error(2544,"Syntax: PERMLOAD \"filename\"   (need quoted string or string expression)\n",Q_ERROR);;}
    break;

  case 295:

    { (yyval).i = makenode(DUMP_,(yyvsp[0]).i,0); ;}
    break;

  case 296:

    { (yyval).i = makenode(DUMP_,0,0); ;}
    break;

  case 297:

    { kb_error(2374,"Syntax: DUMP \"filename\"   (need quoted string or string expression)\n",Q_ERROR);;}
    break;

  case 298:

    { (yyval).i = makenode(SET_COLORMAP_,(yyvsp[0]).i,0); ;}
    break;

  case 299:

    {(yyval).i = makenode(SET_OPTIMIZE_,(yyvsp[0]).i,0);;}
    break;

  case 300:

    { kb_error(2375,"Syntax: OPTIMIZE maxscale\n",Q_ERROR); ;}
    break;

  case 301:

    { (yyval).i = (yyvsp[-1]).i; ;}
    break;

  case 302:

    { (yyval).i = (yyvsp[-1]).i; ;}
    break;

  case 303:

    {
         (yyval).i = makenode(SET_SGLOBAL_,(yyvsp[-2]).i,(yyvsp[0]).i); ;}
    break;

  case 304:

    {
         (yyval).i = makenode(SET_SGLOBAL_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 305:

    {
         (yyval).i = makenode(SET_GLOBAL_,(yyvsp[-2]).i,(yyvsp[0]).i); ;}
    break;

  case 306:

    {
         (yyval).i = makenode(SET_PERM_SGLOBAL_,(yyvsp[-2]).i,(yyvsp[0]).i); ;}
    break;

  case 307:

    { kb_error(2604,"Cannot make permanent assigment to nonpermanent variable.\n",Q_ERROR) ;}
    break;

  case 308:

    { kb_error(2603,"Cannot make nonpermanent assigment to permanent variable.\n",Q_ERROR) ;}
    break;

  case 309:

    { (yyval).i = makenode(SET_GLOBAL_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 310:

    { (yyval).i = makenode(SET_PERM_GLOBAL_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 311:

    { (yyval) = (yyvsp[-1]); ;}
    break;

  case 312:

    { (yyval).i = makenode(SET_ELEMENT_GLOBAL_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 313:

    { (yyval).i = makenode(PDELTA_LVALUE_,(yyvsp[-2]).i,0); ;}
    break;

  case 314:

    { (yyval).i = makenode(PSCALE_LVALUE_,(yyvsp[-2]).i,0); ;}
    break;

  case 315:

    { subtree_swap(&(yyvsp[-2]).i,&(yyvsp[0]).i);
               switch ( list[(yyvsp[-2]).i].type )
               { case PDELTA_LVALUE_:
                      list[(yyvsp[-2]).i].type = SET_DELTA_;
                      list[(yyvsp[-2]).i].left = (yyvsp[0]).i - (yyvsp[-2]).i;
                      break;
                 case PSCALE_LVALUE_:
                      list[(yyvsp[-2]).i].type = SET_PARAM_SCALE;
                      list[(yyvsp[-2]).i].left = (yyvsp[0]).i - (yyvsp[-2]).i;
                      break;
                 default:
                      sprintf(errmsg,"Internal error: lvalue type %d\n",
                          list[(yyvsp[-2]).i].type);
                      kb_error(2882,errmsg,COMMAND_ERROR);
               }
               list[(yyvsp[-2]).i].op2.assigntype = (yyvsp[-1]).i;
               list[(yyvsp[-2]).i].stack_delta = -1;
               (yyval) = (yyvsp[-2]);
            ;}
    break;

  case 316:

    { (yyval).i = makenode(PUSH_PARAM_FIXED,(yyvsp[-2]).i,0); ;}
    break;

  case 317:

    { switch ( list[(yyvsp[0]).i].type )
               { case PDELTA_LVALUE_:
                      list[(yyvsp[0]).i].type = PUSHDELTA_;
                      list[(yyvsp[0]).i].datatype = REAL_TYPE;
                      break;
                 case PSCALE_LVALUE_:
                      list[(yyvsp[0]).i].type = PUSH_PARAM_SCALE;
                      list[(yyvsp[0]).i].datatype = REAL_TYPE;
                      break;
                 default:
                      sprintf(errmsg,"Internal error: lvalue type %d\n",
                          list[(yyvsp[0]).i].type);
                      kb_error(2883,errmsg,COMMAND_ERROR);
               }
               list[(yyvsp[0]).i].stack_delta = 1;
               (yyval) = (yyvsp[0]);
            ;}
    break;

  case 318:

    { kb_error(3380,
    "Syntax: VARIABLE . PDELTA|PSCALE \n", Q_ERROR); ;}
    break;

  case 319:

    { (yyval).i = makenode((yyvsp[-1]).i,(yyvsp[-2]).i,(yyvsp[0]).i); ;}
    break;

  case 320:

    { kb_error(2376,"Syntax: variable := expr\n",Q_ERROR);;}
    break;

  case 321:

    { kb_error(3422,"Got '=' instead of the assignment operator ':='\n",
                Q_ERROR);
           ;}
    break;

  case 322:

    { kb_error(3424,"Got '=' instead of the assignment operator ':='\n",
                Q_ERROR);
           ;}
    break;

  case 323:

    { kb_error(2377,"Syntax: variable := expr\n",Q_ERROR);;}
    break;

  case 324:

    { (yyval).i = (yyvsp[0]).datatype; ;}
    break;

  case 325:

    { (yyval).i = STRING_TYPE; ;}
    break;

  case 326:

    { (yyval).i = add_local_var((yyvsp[0]).lexeme,1); ;}
    break;

  case 327:

    { (yyval).i = add_local_var((yyvsp[0]).lexeme,1);
                if ( shadow_warn_flag )
                { sprintf(errmsg,
                  "Argument \"%s\" shadows already declared variable.\n",
                     (yyvsp[0]).lexeme);
                   kb_error(2635,errmsg,WARNING); 
                }
              ;}
    break;

  case 328:

    { (yyval).i = add_local_var((yyvsp[0]).lexeme,1);
                if ( shadow_warn_flag )
                { sprintf(errmsg,
                    "Argument \"%s\" shadows already declared variable.\n",
                       (yyvsp[0]).lexeme);
                  kb_error(2636,errmsg,WARNING);
                }
             ;}
    break;

  case 329:

    { (yyval).i = add_local_var((yyvsp[0]).lexeme,1);
                if ( shadow_warn_flag )
                { sprintf(errmsg,
                    "Argument \"%s\" shadows already declared procedure.\n",
                        (yyvsp[0]).lexeme);
                  kb_error(2637,errmsg,WARNING);
                }
              ;}
    break;

  case 330:

    { (yyval).i = add_local_var((yyvsp[0]).lexeme,1);
                if ( shadow_warn_flag )
                { sprintf(errmsg,
                    "Argument \"%s\" shadows already declared function.\n",
                        (yyvsp[0]).lexeme);
                  kb_error(2638,errmsg,WARNING); 
                }
             ;}
    break;

  case 331:

    { (yyval).i = add_local_var((yyvsp[0]).lexeme,1);
                if ( shadow_warn_flag )
                { sprintf(errmsg,
                "Argument \"%s\" shadows already declared string variable.\n",
                    (yyvsp[0]).lexeme);
                  kb_error(2639,errmsg,WARNING); 
                }
               ;}
    break;

  case 332:

    { (yyval).i = add_local_var((yyvsp[0]).lexeme,1);
                if ( shadow_warn_flag )
                {  sprintf(errmsg,
                  "Argument \"%s\" shadows already declared quantity name.\n",
                     (yyvsp[0]).lexeme);
                  kb_error(2640,errmsg,WARNING);
                }
               ;}
    break;

  case 333:

    {(yyval).i = add_local_var((yyvsp[0]).lexeme,1);
                if ( shadow_warn_flag )
                { sprintf(errmsg,
                  "Argument \"%s\" shadows already declared method name.\n",
                    (yyvsp[0]).lexeme);
                     kb_error(2641,errmsg,WARNING); 
                }
              ;}
    break;

  case 334:

    {(yyval).i = add_local_var((yyvsp[0]).lexeme,1);
                if ( shadow_warn_flag )
                { sprintf(errmsg,
                  "Argument \"%s\" shadows already declared constraint.\n",
                     (yyvsp[0]).lexeme);
                  kb_error(2642,errmsg,WARNING); 
                } 
              ;}
    break;

  case 335:

    { (yyval).i = add_local_var((yyvsp[0]).lexeme,1);
                if ( shadow_warn_flag )
                { sprintf(errmsg,
                  "Argument \"%s\" shadows already declared boundary.\n",
                    (yyvsp[0]).lexeme);
                  kb_error(2643,errmsg,WARNING);
                }
              ;}
    break;

  case 336:

    { (yyval).i = makenode(ARGLIST_,0,0);  ;}
    break;

  case 337:

    {  int_val = (yyvsp[-1]).i; (yyval).i = makenode(ARGLIST_,0,(yyvsp[0]).i); ;}
    break;

  case 338:

    {  int_val = (yyvsp[-1]).i; (yyval).i = makenode(ARGLIST_,(yyvsp[-3]).i,(yyvsp[0]).i); ;}
    break;

  case 339:

    { if ( strcmp(yytext,"int") == 0 )
             kb_error(3604,"Expecting datatype or ')' after '('\n ('integer' is the Evolver datatype, not 'int'.\n",Q_ERROR);  
           else
             kb_error(3636,"Expecting datatype or ')' after '('\n",Q_ERROR);  
         ;}
    break;

  case 340:

    { kb_error(3605,"Expecting identifier after datatype.\n",Q_ERROR);  ;}
    break;

  case 341:

    { kb_error(3606,"Expecting datatype after ','\n",Q_ERROR);  ;}
    break;

  case 342:

    { kb_error(3625,"Expecting identifier after datatype.\n",Q_ERROR);  ;}
    break;

  case 343:

    { kb_error(3525,"Expecting comma or right parenthesis after argument.\n",Q_ERROR);  ;}
    break;

  case 344:

    { (yyval).i = (yyvsp[-1]).i ;}
    break;

  case 345:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 346:

    { (yyval).i = 0; ;}
    break;

  case 347:

    { kb_error(2624,"Missing function body, or ';' after prototype.\n",
               Q_ERROR);
            ;}
    break;

  case 348:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 349:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 350:

    { in_function = 1;
          if ( (yyvsp[0]).i == 0 ) (yyvsp[0]).i = add_global((yyvsp[0]).lexeme);
          init_local_scope((yyvsp[0]).i); begin_local_scope();
          (yyval).i = makenode(FUNCTION_DEF_START_,(yyvsp[0]).i,(yyvsp[-1]).i); ;}
    break;

  case 351:

    { (yyval).i = makenode(FUNCTION_HEAD_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 352:

    { int insize = inputbufferspot - (yyvsp[-6]).qnum;
           in_function = 0;
           globals((yyvsp[-4]).i)->attr.procstuff.proc_text = mycalloc(insize+1,1); 
           strncpy(globals((yyvsp[-4]).i)->attr.procstuff.proc_text,inputbuffer+(yyvsp[-4]).qnum,insize);
           globals((yyvsp[-4]).i)->attr.procstuff.proc_text[insize] = 0;
           list[(yyvsp[-3]).i].op5.locals = globals((yyvsp[-4]).i)->attr.procstuff.locals =
                localbase;
           if ( localbase )
             localbase->flags |= LL_IN_USE;
           int_val = (yyvsp[-5]).i;
           if ( (yyvsp[0]).i )
             (yyval).i = makenode(SET_FUNCTION_,(yyvsp[-1]).i,(yyvsp[0]).i); 
           else 
           { makenode(FUNCTION_PROTO_,(yyvsp[-1]).i,0);
             (yyval).i = 0;
           }
           exit_local_scope();
         ;}
    break;

  case 353:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 354:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 355:

    { in_function = 1; /* for lex*/
           if ( (yyvsp[0]).i == 0 ) (yyvsp[0]).i = add_global((yyvsp[0]).lexeme);
             init_local_scope((yyvsp[0]).i); begin_local_scope();
              (yyval).i = makenode(PROCEDURE_DEF_START_,(yyvsp[0]).i,0); ;}
    break;

  case 356:

    { (yyval).i = makenode(PROCEDURE_HEAD_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 357:

    { int insize = inputbufferspot - (yyvsp[-5]).qnum;
           in_function = 0;
           globals((yyvsp[-4]).i)->attr.procstuff.proc_text = mycalloc(insize+1,1); 
           strncpy(globals((yyvsp[-4]).i)->attr.procstuff.proc_text,inputbuffer+(yyvsp[-4]).qnum,insize);
           globals((yyvsp[-4]).i)->attr.procstuff.proc_text[insize] = 0;
           list[(yyvsp[-3]).i].op5.locals = globals((yyvsp[-4]).i)->attr.procstuff.locals = 
              localbase;
           if ( localbase )
             localbase->flags |= LL_IN_USE;
           if ( (yyvsp[0]).i )
             (yyval).i = makenode(SET_ARGSPROC_,(yyvsp[-1]).i,(yyvsp[0]).i); 
           else 
           { makenode(PROCEDURE_PROTO_,(yyvsp[-1]).i,0);
             (yyval).i = 0;
           }
           exit_local_scope();
         ;}
    break;

  case 358:

    { kb_error(3704,"Expected function name after datatype.\n",Q_ERROR); ;}
    break;

  case 359:

    { kb_error(3705,"Expected datatype for function.\n",Q_ERROR); ;}
    break;

  case 360:

    { kb_error(3706,"Expected name of procedure.\n",Q_ERROR); ;}
    break;

  case 361:

    { kb_error(3496,"Function returns a value; it's not a stand-alone command.\n",Q_ERROR); ;}
    break;

  case 362:

    { (yyval).i = makenode(DEFINE_IDENT_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 363:

    {int g; if ( (yyvsp[-1]).i == 0 )g = add_global((yyvsp[-1]).lexeme);
           else g = (yyvsp[-1]).i; /* local */
     (yyval).i = makenode(DEFINE_IDENT_,g,(yyvsp[0]).i); ;}
    break;

  case 364:

    { (yyval).i = makenode(DEFINE_IDENT_,(yyvsp[0]).i,REAL_TYPE); ;}
    break;

  case 365:

    {int g = (yyvsp[0]).i ? (yyvsp[0]).i : add_global((yyvsp[0]).lexeme); 
    (yyval).i = makenode(DEFINE_IDENT_,g,REAL_TYPE); ;}
    break;

  case 366:

    { (yyval).i = makenode(DEFINE_IDENT_,(yyvsp[0]).i,STRING_TYPE); ;}
    break;

  case 367:

    { (yyval).i = makenode(INDEXSET_,0,(yyvsp[-1]).i); ;}
    break;

  case 368:

    { (yyval).i = makenode(INDEXSET_,(yyvsp[-3]).i,(yyvsp[-1]).i); ;}
    break;

  case 369:

    { (yyval).i = makenode(DIMENSIONSET_,0,(yyvsp[-1]).i); ;}
    break;

  case 370:

    { (yyval).i = makenode(DIMENSIONSET_,(yyvsp[-3]).i,(yyvsp[-1]).i); ;}
    break;

  case 371:

    { 
          (yyval).qnum = (yyvsp[-2]).i ; (yyval).datatype = (yyvsp[-1]).i;
          int_val= (yyval).datatype;
          (yyval).i = makenode(DEFINE_ARRAY_,(yyvsp[-2]).i,(yyvsp[0]).i);
        ;}
    break;

  case 372:

    { (yyval).qnum = (yyvsp[-2]).i ? (yyvsp[-2]).i : add_global((yyvsp[-2]).lexeme);  
         (yyval).datatype = (yyvsp[-1]).i;
         int_val= (yyval).datatype;
         (yyval).i = makenode(DEFINE_ARRAY_,(yyval).qnum,(yyvsp[0]).i);
       ;}
    break;

  case 373:

    { 
          (yyval).qnum = (yyvsp[-2]).i ; (yyval).datatype = (yyvsp[-1]).i;
          int_val= (yyval).datatype;
          (yyval).i = makenode(DEFINE_ARRAY_,(yyvsp[-2]).i,(yyvsp[0]).i);
        ;}
    break;

  case 374:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 375:

    { (yyval).i = makenode(ARRAYIDENT_,(yyvsp[0]).i,0);
             ;}
    break;

  case 376:

    { /* for implicit generator */
               (yyval).i = makenode(ATTRIB_LVALUE_,0,0);
               list[(yyval).i].op1.localnum = 0;
               list[(yyval).i].op2.name_id = set_name_eltype(V_NORMAL_ATTR,VERTEX);
               list[(yyval).i].type = ARRAY_VERTEX_NORMAL_; 
               list[(yyval).i].op3.localnum = add_local_var(NULL,SDIM);
             ;}
    break;

  case 377:

    { /* for implicit generator */
               (yyval).i = makenode(ATTRIB_LVALUE_,0,0);
               list[(yyval).i].op1.localnum = 0;
               list[(yyval).i].op2.name_id = set_name_eltype((yyvsp[0]).qnum,(yyvsp[0]).etype);
               if ( ((yyvsp[0]).etype == VERTEX) && ((yyvsp[0]).qnum == V_NORMAL_ATTR) )
               { list[(yyval).i].type = ARRAY_VERTEX_NORMAL_; 
                 list[(yyval).i].op3.localnum = add_local_var(NULL,SDIM);
                 list[(yyval).i].flags |= IS_VIRTUAL_ATTR;
               }
               else if ( ((yyvsp[0]).etype == EDGE) && ((yyvsp[0]).qnum == E_VECTOR_ATTR) )
               { list[(yyval).i].type = ARRAY_EDGE_VECTOR_; 
                 list[(yyval).i].op3.localnum = add_local_var(NULL,SDIM);
                 list[(yyval).i].flags |= IS_VIRTUAL_ATTR;
               }
               else if ( ((yyvsp[0]).etype == FACET) && ((yyvsp[0]).qnum == F_NORMAL_ATTR) )
               { list[(yyval).i].type = ARRAY_FACET_NORMAL_; 
                 list[(yyval).i].op3.localnum = add_local_var(NULL,SDIM);
                 list[(yyval).i].flags |= IS_VIRTUAL_ATTR;
               }
             ;}
    break;

  case 378:

    { 
               if ( (yyvsp[0]).etype != (yyvsp[-1]).etype )
               { sprintf(errmsg, "\"%s\" is a %s attribute, not %s.\n",
                    EXTRAS((yyvsp[0]).etype)[(yyvsp[0]).qnum].name,typenames[(yyvsp[0]).etype],
                    typenames[(yyvsp[-1]).etype]);
                 kb_error(3678,errmsg,Q_ERROR);
               }
               (yyval).i = makenode(ATTRIB_LVALUE_,(yyvsp[-1]).i,0);
               list[(yyval).i].op1.localnum = list[(yyvsp[-1]).i].op2.localnum;
               list[(yyval).i].op2.name_id = set_name_eltype((yyvsp[0]).qnum,(yyvsp[0]).etype);
               if ( ((yyvsp[0]).etype == VERTEX) && ((yyvsp[0]).qnum == V_NORMAL_ATTR) )
               { list[(yyval).i].type = ARRAY_VERTEX_NORMAL_; 
                 list[(yyval).i].op3.localnum = add_local_var(NULL,SDIM);
                 list[(yyval).i].flags |= IS_VIRTUAL_ATTR;
               }
               else if ( ((yyvsp[0]).etype == EDGE) && ((yyvsp[0]).qnum == E_VECTOR_ATTR) )
               { list[(yyval).i].type = ARRAY_EDGE_VECTOR_; 
                 list[(yyval).i].op3.localnum = add_local_var(NULL,SDIM);
                 list[(yyval).i].flags |= IS_VIRTUAL_ATTR;
               }
               else if ( ((yyvsp[0]).etype == FACET) && ((yyvsp[0]).qnum == F_NORMAL_ATTR) )
               { list[(yyval).i].type = ARRAY_FACET_NORMAL_; 
                 list[(yyval).i].op3.localnum = add_local_var(NULL,SDIM);
                 list[(yyval).i].flags |= IS_VIRTUAL_ATTR;
               }
             ;}
    break;

  case 379:

    { sprintf(errmsg,"\"%s\" is not an attribute name.\n",(yyvsp[0]).lexeme);
         kb_error(3573,errmsg,Q_ERROR);
       ;}
    break;

  case 380:

    { sprintf(errmsg,"Missing attribute.\n");
         kb_error(3574,errmsg,Q_ERROR);
       ;}
    break;

  case 381:

    { (yyval).i = makenode(ARRAY_LVALUE_INDEXED_,(yyvsp[-1]).i,(yyvsp[0]).i);
           ;}
    break;

  case 382:

    { 
             (yyval).i = makenode(ARRAY_ASSIGNOP_SINGLE_,(yyvsp[-2]).i,(yyvsp[0]).i);
             list[(yyval).i].op1.assigntype = (yyvsp[-1]).i;
             list[(yyval).i].op2.name_id = list[(yyvsp[-2]).i].op2.name_id;
             list[(yyval).i].stack_delta = list[(yyvsp[-2]).i].op1.indexcount + 2;
           ;}
    break;

  case 383:

    { 
             (yyval).i = makenode(ARRAY_ASSIGNOP_ARRAY_,(yyvsp[-2]).i,(yyvsp[0]).i);
             list[(yyval).i].op1.assigntype = (yyvsp[-1]).i;
             list[(yyvsp[0]).i].flags |= IS_RVALUE;
           ;}
    break;

  case 384:

    { (yyval).i = makenode(ARRAY_ASSIGNOP_SCALAR_,(yyvsp[-2]).i,(yyvsp[0]).i);
             list[(yyval).i].op1.assigntype = (yyvsp[-1]).i;
           ;}
    break;

  case 385:

    { int k = makenode(ARRAY_RVALUE_,(yyvsp[-2]).i,(yyvsp[0]).i);
             list[k].op1.intval = '*';
             (yyval).i = makenode(ARRAY_ASSIGNOP_S_X_A_,(yyvsp[-4]).i,k);
             list[(yyval).i].op1.assigntype = (yyvsp[-3]).i;
             list[(yyval).i].op3.name_id = list[(yyvsp[0]).i].op2.name_id;
             list[(yyvsp[0]).i].flags |= IS_RVALUE;
             check_special_attr(list[(yyvsp[0]).i].op2.name_id);
             /* Can do dimension check now */
             if ( check_array_dims_same(list[(yyvsp[-4]).i].op2.name_id,
                       list[(yyvsp[0]).i].op2.name_id ) == 0 )
                kb_error(4379,"Arrays don't have same number of dimensions or types are different.\n",
                   COMMAND_ERROR);
           ;}
    break;

  case 386:

    { int k = makenode(ARRAY_RVALUE_,(yyvsp[-2]).i,(yyvsp[0]).i);
             list[k].op1.intval = '+';
             (yyval).i = makenode(ARRAY_ASSIGNOP_A_P_A_,(yyvsp[-4]).i,k);
             list[(yyval).i].op1.assigntype = (yyvsp[-3]).i;
             list[(yyval).i].op3.name_id = list[(yyvsp[-2]).i].op2.name_id;
             list[(yyval).i].op4.name_id = list[(yyvsp[0]).i].op2.name_id;
             list[(yyvsp[-2]).i].flags |= IS_RVALUE;
             list[(yyvsp[0]).i].flags |= IS_RVALUE;
             check_special_attr(list[(yyvsp[-2]).i].op2.name_id);
             check_special_attr(list[(yyvsp[0]).i].op2.name_id);
             /* Can do dimension check now */
             if ( check_array_dims_same(list[(yyvsp[-4]).i].op2.name_id,
                   list[(yyvsp[-2]).i].op2.name_id) == 0 )
                kb_error(4380,"Arrays don't have same number of dimensions or types are different.\n",
                   COMMAND_ERROR);
             if ( check_array_dims_same(list[(yyvsp[-4]).i].op2.name_id,
                   list[(yyvsp[0]).i].op2.name_id) == 0 )
                kb_error(4381,"Arrays don't have same number of dimensions or types are different.\n",
                   COMMAND_ERROR);
           ;}
    break;

  case 387:

    { int k = makenode(ARRAY_RVALUE_,(yyvsp[-2]).i,(yyvsp[0]).i);
             list[k].op1.intval = '-';
             (yyval).i = makenode(ARRAY_ASSIGNOP_A_S_A_,(yyvsp[-4]).i,k);
             list[(yyval).i].op1.assigntype = (yyvsp[-3]).i;
             list[(yyval).i].op3.name_id = list[(yyvsp[-2]).i].op2.name_id;
             list[(yyval).i].op4.name_id = list[(yyvsp[0]).i].op2.name_id;
             list[(yyvsp[-2]).i].flags |= IS_RVALUE;
             list[(yyvsp[0]).i].flags |= IS_RVALUE;
             check_special_attr(list[(yyvsp[-2]).i].op2.name_id);
             check_special_attr(list[(yyvsp[0]).i].op2.name_id);
             /* Can do dimension check now */
             if ( check_array_dims_same(list[(yyvsp[-4]).i].op2.name_id,
                   list[(yyvsp[-2]).i].op2.name_id) == 0 )
                kb_error(3030,"Arrays don't have same number of dimensions or types are different.\n",
                   COMMAND_ERROR);
             if ( check_array_dims_same(list[(yyvsp[-4]).i].op2.name_id,
                   list[(yyvsp[0]).i].op2.name_id) == 0 )
                kb_error(4383,"Arrays don't have same number of dimensions or types are different.\n",
                   COMMAND_ERROR);
           ;}
    break;

  case 388:

    { (yyval).i = makenode(DOT_,(yyvsp[-2]).i,(yyvsp[0]).i); 
           list[(yyvsp[-2]).i].flags |= IS_RVALUE;
           list[(yyvsp[0]).i].flags |= IS_RVALUE;
         ;}
    break;

  case 389:

    { (yyval).i = makenode(ARRAY_EVAL_,(yyvsp[-1]).i,(yyvsp[0]).i); 
           list[(yyvsp[-1]).i].flags |= IS_RVALUE;
         ;}
    break;

  case 390:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 391:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 392:

    { struct extra *ex;
            (yyval).qnum = (yyvsp[-1]).qnum; 
            (yyval).etype = (yyvsp[-1]).etype;
            if ( (yyvsp[-3]).i != (yyval).etype )
              kb_error(1885,"This extra attribute already defined on different element type.\n",COMMAND_ERROR);
            ex = EXTRAS((yyval).etype) + (yyval).qnum;
            if ( ex->type != (yyvsp[0]).i )
            { sprintf(errmsg,
                "Attribute %s already defined with different type, %s.\n",
                   ex->name,datatype_name[ex->type]);
              kb_error(1886,errmsg,COMMAND_ERROR);
            }
           (yyval).i = makenode(DEFINE_EXTRA_,0,(yyvsp[-3]).i); 
           list[(yyval).i].op1.extranum = (yyval).qnum;
          ;}
    break;

  case 393:

    { int attr_type=INTEGER_TYPE;
           if ( (yyvsp[-1]).i ) 
           { sprintf(errmsg,"Cannot use local variable \"%s\" as attribute.\n",
                (yyvsp[-1]).lexeme);
             kb_error(2615,errmsg,COMMAND_ERROR);
           }
           attr_type = (yyvsp[0]).i;
           (yyval).qnum = add_attribute((yyvsp[-3]).i,(yyvsp[-1]).lexeme,attr_type,0,NULL,DUMP_ATTR,NULL); 
           /* being a declaration, has effect when parsed */
           (yyval).i = makenode(DEFINE_EXTRA_,0,(yyvsp[-3]).i); 
           list[(yyval).i].op1.extranum = (yyval).qnum;
         ;}
    break;

  case 394:

    { (yyval).i = makenode(DEFINE_EXTRA_INDEX_,(yyvsp[-1]).i,(yyvsp[0]).i);
         ;}
    break;

  case 395:

    {
           begin_scope(); /* ended right below */
           elsym = symbol_add("self",list[(yyvsp[-1]).i].op2.eltype);
           (yyval).i = makenode(ATTR_FUNCTION_,(yyvsp[-1]).i,0);
          ;}
    break;

  case 396:

    { init_local_scope(0); begin_local_scope(); ;}
    break;

  case 397:

    { struct extra *ext;
           end_local_scope();
           (yyval).i = makenode(ATTR_FUNCTION_END_,(yyvsp[-4]).i,(yyvsp[-1]).i);
           list[(yyval).i].op1.extranum = list[(yyvsp[-6]).i].op1.extranum;  /* attr number */
           list[(yyval).i].op2.eltype = list[(yyvsp[-6]).i].op2.eltype;  /* element type */
           list[(yyvsp[-4]).i].op1.skipsize = (yyval).i - (yyvsp[-4]).i;
           ext = EXTRAS(list[(yyval).i].op2.eltype) + list[(yyval).i].op1.extranum;
           ext->code.locals = localbase;
           if ( localbase )
             localbase->flags |= LL_IN_USE;
           exit_local_scope();
           ext->flags |= FUNCTION_ATTR;
           end_scope();
         ;}
    break;

  case 398:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 399:

    { (yyval).i = makenode(DEFINE_QUANTITY_,0,0); ;}
    break;

  case 400:

    { (yyval).i = makenode(DEFINE_METHOD_INSTANCE_,0,0); ;}
    break;

  case 401:

    { (yyval).i = makenode(DEFINE_CONSTRAINT_,0,0); ;}
    break;

  case 402:

    { (yyval).i = makenode(DEFINE_BOUNDARY_,0,0); ;}
    break;

  case 403:

    { kb_error(2379,
   "Syntax: DEFINE name [REAL|INTEGER]\n or:    DEFINE elementtype ATTRIBUTE name REAL|INTEGER  [ dimension ]   \n",Q_ERROR); ;}
    break;

  case 404:

    { 
           strncpy((yyval).lexeme,(yyvsp[-1]).lexeme,31);
          /*  if ( $$.i == 0 )  ?? */
           (yyval).i = add_global((yyvsp[-1]).lexeme);  
          /*  else $$.i = $1.i; */ /* local ?? */
            (yyval).qnum = assignbacktrack();
          ;}
    break;

  case 405:

    { 
             (yyval).i = add_perm_global((yyvsp[-1]).lexeme);  
             perm_globals((yyval).i)->flags |= PERMANENT;   
             perm_flag++;
             (yyval).qnum = assignbacktrack();
            ;}
    break;

  case 406:

    { 
           /* if ( $$.i == 0 )  ??  */
                (yyval).i = add_global((yyvsp[0]).lexeme);  
           /*  else $$.i = $2.i; ?? */ /* local */
         ;}
    break;

  case 407:

    { (yyval).i = (yyvsp[-2]).i;(yyval).qnum = assignbacktrack(); ;}
    break;

  case 408:

    { 
            /* if ( $$.i == 0 )  ?? */
               (yyval).i = add_global((yyvsp[0]).lexeme);
             /* else $$.i = $2.i;  ?? */
             (yyval).qnum = assignbacktrack();
             strcpy((yyval).lexeme,(yyvsp[0]).lexeme); 
           ;}
    break;

  case 409:

    { kb_error(2380,"Syntax: variable := rexpr | {command} \n",Q_ERROR);;}
    break;

  case 410:

    { sprintf(errmsg,"Syntax error: Unexpected new identifier '%s'.\n",(yyvsp[-1]).lexeme);
       kb_error(2381,errmsg, Q_ERROR);;}
    break;

  case 411:

    { kb_error(2382,"Syntax: variable := rexpr | {command}     (braces needed around command) \n",Q_ERROR);;}
    break;

  case 412:

    { (yyval).i = makenode(SET_SGLOBAL_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 413:

    { (yyval).i = makenode(SET_GLOBAL_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 414:

    { sprintf(errmsg,"Illegal right side of assignment.\n");
       kb_error(3756,errmsg, Q_ERROR);;}
    break;

  case 415:

    { (yyval).i = makenode(SET_PERM_SGLOBAL_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 416:

    { (yyval).i = makenode(SET_PERM_GLOBAL_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 417:

    { init_local_scope((yyvsp[0]).i); 
             begin_local_scope(); ;}
    break;

  case 418:

    { int insize = inputbufferspot - (yyvsp[-2]).qnum;
           globals((yyvsp[-2]).i)->attr.procstuff.proc_text = mycalloc(insize+1,1); 
           strncpy(globals((yyvsp[-2]).i)->attr.procstuff.proc_text,inputbuffer+(yyvsp[-2]).qnum,insize);
           (yyval).i = makenode(SET_PROCEDURE_,(yyvsp[0]).i,(yyvsp[-2]).i); 
           globals((yyvsp[-2]).i)->attr.procstuff.proc_text[insize] = 0;
           exit_local_scope();
         ;}
    break;

  case 419:

    { init_local_scope((yyvsp[-1]).i); begin_local_scope(); ;}
    break;

  case 420:

    { int k,insize = inputbufferspot - (yyvsp[-4]).qnum;
           globals((yyvsp[-4]).i)->attr.procstuff.proc_text = mycalloc(insize+1,1); 
           strncpy(globals((yyvsp[-4]).i)->attr.procstuff.proc_text,inputbuffer+(yyvsp[-4]).qnum,insize);
           globals((yyvsp[-4]).i)->attr.procstuff.proc_text[insize] = 0;
           k = makenode(COMMAND_BLOCK_,(yyvsp[-1]).i,0);
           (yyval).i = makenode(SET_PROCEDURE_,k,(yyvsp[-4]).i); 
           exit_local_scope();
         ;}
    break;

  case 421:

    { int k = makenode(NULLBLOCK_,0,0);
           localbase = NULL;
           (yyval).i = makenode(SET_PROCEDURE_,k,(yyvsp[-2]).i); 
         ;}
    break;

  case 422:

    { init_local_scope((yyvsp[-1]).i); begin_local_scope(); ;}
    break;

  case 423:

    { int k,insize = inputbufferspot - (yyvsp[-4]).qnum;
           perm_globals((yyvsp[-4]).i)->attr.procstuff.proc_text = calloc(insize+1,1); 
           strncpy(perm_globals((yyvsp[-4]).i)->attr.procstuff.proc_text,inputbuffer+(yyvsp[-4]).qnum,insize);
           perm_globals((yyvsp[-4]).i)->attr.procstuff.proc_text[insize] = 0;
           k = makenode(COMMAND_BLOCK_,(yyvsp[-1]).i,0);
           (yyval).i = makenode(SET_PERM_PROCEDURE_,k,(yyvsp[-4]).i); 
           exit_local_scope();
         ;}
    break;

  case 424:

    { int k = makenode(NULLBLOCK_,0,0);
           localbase = NULL;
           (yyval).i = makenode(SET_PERM_PROCEDURE_,k,(yyvsp[-2]).i); 
         ;}
    break;

  case 425:

    { (yyval).qnum = assignbacktrack();
                     init_local_scope((yyvsp[-1]).i); begin_local_scope();
    ;}
    break;

  case 426:

    { int insize = inputbufferspot - (yyvsp[-1]).qnum;
           myfree(globals((yyvsp[-3]).i)->attr.procstuff.proc_text);
           globals((yyvsp[-3]).i)->attr.procstuff.proc_text = mycalloc(insize+1,1); 
           strncpy(globals((yyvsp[-3]).i)->attr.procstuff.proc_text,inputbuffer+(yyvsp[-1]).qnum,insize);
           globals((yyvsp[-3]).i)->attr.procstuff.proc_text[insize] = 0;
           (yyval).i = makenode(SET_PROCEDURE_,(yyvsp[0]).i,(yyvsp[-3]).i); 
           exit_local_scope();
         ;}
    break;

  case 427:

    { (yyval).i = perm_flag++; (yyval).qnum = assignbacktrack(); 
           init_local_scope((yyvsp[-1]).i); begin_local_scope(); ;}
    break;

  case 428:

    { int insize = inputbufferspot - (yyvsp[-1]).qnum;
           free(perm_globals((yyvsp[-3]).i)->attr.procstuff.proc_text);
           perm_globals((yyvsp[-3]).i)->attr.procstuff.proc_text = calloc(insize+1,1); 
           strncpy(perm_globals((yyvsp[-3]).i)->attr.procstuff.proc_text,inputbuffer+(yyvsp[-1]).qnum,insize);
           perm_globals((yyvsp[-3]).i)->attr.procstuff.proc_text[insize] = 0;
           perm_globals((yyvsp[-3]).i)->flags |= PERMANENT; 
           (yyval).i = makenode(SET_PERM_PROCEDURE_,(yyvsp[0]).i,(yyvsp[-3]).i); 
           perm_flag = (yyvsp[-1]).i; 
           exit_local_scope();
         ;}
    break;

  case 429:

    { kb_error(2383,"Syntax: procedure_name := {command} \n",Q_ERROR);;}
    break;

  case 430:

    { kb_error(2384,"Syntax: procedure_name ::= {command} \n",Q_ERROR);;}
    break;

  case 431:

    {  sprintf(errmsg,
              "'%s' is a variable; cannot be assigned a procedure.\n",
               globals((yyvsp[-1]).i)->name);
            kb_error(3899,errmsg,Q_ERROR);
         ;}
    break;

  case 432:

    { (yyval).i = makenode(LOCAL_LIST_START_,(yyvsp[0]).i,0); ;}
    break;

  case 433:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 434:

    { (yyval).i = (yyvsp[0]).i; list[(yyvsp[0]).i].left = -1; ;}
    break;

  case 435:

    { ident_t iid = add_local_var((yyvsp[0]).lexeme,1);
                (yyval).i = makenode(DECLARE_LOCAL_,iid,0); ;}
    break;

  case 436:

    { ident_t iid = add_local_var((yyvsp[0]).lexeme,1);
                (yyval).i = makenode(DECLARE_LOCAL_,iid,0);
                if ( shadow_warn_flag )
                { sprintf(errmsg,
                  "Local name \"%s\" shadows already declared variable.\n",
                   (yyvsp[0]).lexeme);
                  kb_error(2625,errmsg,WARNING); 
                }
             ;}
    break;

  case 437:

    { ident_t iid = add_local_var((yyvsp[0]).lexeme,1);
                (yyval).i = makenode(DECLARE_LOCAL_,iid,0); 
                if ( shadow_warn_flag )
                { sprintf(errmsg,
                  "Local name \"%s\" shadows already declared variable.\n",
                  (yyvsp[0]).lexeme);
                  kb_error(2626,errmsg,WARNING); 
                }
              ;}
    break;

  case 438:

    { ident_t iid = add_local_var((yyvsp[0]).lexeme,1);
               (yyval).i = makenode(DECLARE_LOCAL_,iid,0); 
               if ( shadow_warn_flag )
               { sprintf(errmsg,
                  "Local name \"%s\" shadows already declared procedure.\n",
                    (yyvsp[0]).lexeme);
                 kb_error(2627,errmsg,WARNING); 
               }
             ;}
    break;

  case 439:

    { ident_t iid = add_local_var((yyvsp[0]).lexeme,1);
               (yyval).i = makenode(DECLARE_LOCAL_,iid,0); 
               if ( shadow_warn_flag )
               { sprintf(errmsg,
                  "Local name \"%s\" shadows already declared function.\n",
                    (yyvsp[0]).lexeme);
                 kb_error(2628,errmsg,WARNING); 
               }
             ;}
    break;

  case 440:

    { ident_t iid = add_local_var((yyvsp[0]).lexeme,1);
               (yyval).i = makenode(DECLARE_LOCAL_,iid,0); 
               if ( shadow_warn_flag )
               { sprintf(errmsg,
                "Local name \"%s\" shadows already declared string variable.\n",
                  (yyvsp[0]).lexeme);
                 kb_error(2629,errmsg,WARNING); 
               }
              ;}
    break;

  case 441:

    {ident_t iid = add_local_var((yyvsp[0]).lexeme,1);
               (yyval).i = makenode(DECLARE_LOCAL_,iid,0); 
               if ( shadow_warn_flag )
               { sprintf(errmsg,
                  "Local name \"%s\" shadows already declared quantity name.\n",
                    (yyvsp[0]).lexeme);
                 kb_error(2630,errmsg,WARNING); 
               }
             ;}
    break;

  case 442:

    {ident_t iid = add_local_var((yyvsp[0]).lexeme,1);
               (yyval).i = makenode(DECLARE_LOCAL_,iid,0); 
               if ( shadow_warn_flag )
               { sprintf(errmsg,
                  "Local name \"%s\" shadows already declared method name.\n",
                    (yyvsp[0]).lexeme);
                 kb_error(2631,errmsg,WARNING); 
               }
             ;}
    break;

  case 443:

    {ident_t iid = add_local_var((yyvsp[0]).lexeme,1);
               (yyval).i = makenode(DECLARE_LOCAL_,iid,0); 
               if ( shadow_warn_flag )
               { sprintf(errmsg,
                  "Local name \"%s\" shadows already declared constraint.\n",
                    (yyvsp[0]).lexeme);
                 kb_error(2632,errmsg,WARNING); 
               }
             ;}
    break;

  case 444:

    {ident_t iid = add_local_var((yyvsp[0]).lexeme,1);
               (yyval).i = makenode(DECLARE_LOCAL_,iid,0); 
               if ( shadow_warn_flag )
               { sprintf(errmsg,
                  "Local name \"%s\" shadows already declared boundary.\n",
                    (yyvsp[0]).lexeme);
                 kb_error(2633,errmsg,WARNING); 
               }
             ;}
    break;

  case 445:

    { kb_error(2614,"Syntax: LOCAL varname; \n",Q_ERROR);;}
    break;

  case 446:

    { init_local_scope(0); begin_local_scope(); ;}
    break;

  case 447:

    { (yyval).i = makenode(REDEFINE_SINGLE_,(yyvsp[0]).i,(yyvsp[-3]).i);
            exit_local_scope();
          ;}
    break;

  case 448:

    { init_local_scope(0); begin_local_scope(); ;}
    break;

  case 449:

    {
            (yyval).i = makenode(REDEFINE_SINGLE_,(yyvsp[0]).i,(yyvsp[-3]).i);
            exit_local_scope();
          ;}
    break;

  case 450:

    { init_local_scope(0); begin_local_scope(); ;}
    break;

  case 451:

    {
            (yyval).i = makenode(REDEFINE_SINGLE_,(yyvsp[0]).i,(yyvsp[-3]).i);
            exit_local_scope();
          ;}
    break;

  case 452:

    { (yyval).i = makenode(UNREDEFINE_SINGLE_,0,(yyvsp[-1]).i); ;}
    break;

  case 453:

    { (yyval).i = makenode(UNREDEFINE_SINGLE_,0,(yyvsp[-1]).i); ;}
    break;

  case 454:

    { (yyval).i = makenode(EXPRLIST_,(yyvsp[-2]).i,(yyvsp[0]).i); ;}
    break;

  case 455:

    { kb_error(3801,"Missing expression after ','\n",
                      Q_ERROR); ;}
    break;

  case 456:

    { (yyval).i = makenode(EXPRLIST_,(yyvsp[0]).i,0); ;}
    break;

  case 457:

    { (yyval).i = makenode(EXPRLIST_,(yyvsp[-2]).i,(yyvsp[0]).i); ;}
    break;

  case 458:

    { kb_error(3891,"Missing expression after ','\n", Q_ERROR); ;}
    break;

  case 459:

    { (yyval).i = makenode(EXPRLIST_,(yyvsp[0]).i,0); ;}
    break;

  case 460:

    { (yyval).i = makenode(PRINTFHEAD_,(yyvsp[0]).i,0); ;}
    break;

  case 461:

    { kb_error(3892,"Missing format string after printf.\n", Q_ERROR); ;}
    break;

  case 462:

    { (yyval).i = makenode(BINARY_PRINTFHEAD_,(yyvsp[0]).i,0); ;}
    break;

  case 463:

    { kb_error(4892,"Missing format string after printf.\n", Q_ERROR); ;}
    break;

  case 464:

    { (yyval).i = makenode(ERRPRINTFHEAD_,(yyvsp[0]).i,0); ;}
    break;

  case 465:

    { kb_error(3802,"Missing format string after errprintf.\n", Q_ERROR); ;}
    break;

  case 466:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 467:

    { (yyval).i = makenode(PRINTF_,(yyvsp[-2]).i,(yyvsp[0]).i); ;}
    break;

  case 468:

    { kb_error(3893,"Missing expression after ','\n", Q_ERROR); ;}
    break;

  case 469:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 470:

    { (yyval).i = makenode(BINARY_PRINTF_,(yyvsp[-2]).i,(yyvsp[0]).i); ;}
    break;

  case 471:

    { kb_error(4893,"Missing expression after ','\n", Q_ERROR); ;}
    break;

  case 472:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 473:

    { (yyval).i = makenode(ERRPRINTF_,(yyvsp[-2]).i,(yyvsp[0]).i); ;}
    break;

  case 474:

    { (yyval).i = makenode(ERRPRINTF_,(yyvsp[-2]).i,(yyvsp[0]).i); ;}
    break;

  case 475:

    { kb_error(3803,"Missing expression after ','\n", Q_ERROR); ;}
    break;

  case 477:

    { (yyval).i = makenode(LIST_PROCS_,0,0); ;}
    break;

  case 478:

    { (yyval).i = makenode(STRPRINT_,(yyvsp[0]).i,0); ;}
    break;

  case 479:

    { (yyval).i = makenode(PRINT_PROCEDURE_,(yyvsp[0]).i,0); ;}
    break;

  case 480:

    { (yyval).i = makenode(PRINT_PROCEDURE_,(yyvsp[0]).i,0); ;}
    break;

  case 481:

    { (yyval).i = makenode(PRINT_PROCEDURE_,(yyvsp[0]).i,0); ;}
    break;

  case 482:

    { (yyval).i = makenode(PRINT_PERM_PROCEDURE_,(yyvsp[0]).i,0); ;}
    break;

  case 483:

    { (yyval).i = makenode(EXPRINT_PROCEDURE_,(yyvsp[0]).i,0); ;}
    break;

  case 484:

    { (yyval).i = makenode(PRINT_,(yyvsp[0]).i,0); ;}
    break;

  case 485:

    { (yyval).i = makenode(PRINT_ARRAY_LVALUE_,(yyvsp[0]).i,0);
       list[(yyvsp[0]).i].flags |= IS_RVALUE;
     ;}
    break;

  case 486:

    { int k;
       switch ( (yyvsp[0]).i )
       { case COORD_:
           switch ( (yyvsp[-1]).etype )
           { case VERTEX: case EDGE: case FACET:
               k = makenode(ATTRIBUTE,(yyvsp[0]).i,(yyvsp[0]).qnum); 
               list[k].op1.localnum = list[(yyvsp[-1]).i].op2.localnum;
               list[k].op2.coordnum = (yyvsp[0]).qnum - 1; /* 1-based indexing to 0 */
               k = makenode(QUALIFIED_ATTRIBUTE,(yyvsp[-1]).i,k); 
               (yyval).i = makenode(PRINT_,k,0);
               goto vnexit;

             default:
               sprintf(errmsg,"\"x\" is not a %s attribute.\n",
                typenames[(yyvsp[-1]).etype]);
               kb_error(2650,errmsg,COMMAND_ERROR);
           }
           break;
         case GET_VERTEXNORMAL_:
           if ( (yyvsp[-1]).etype != VERTEX )
           { sprintf(errmsg,"\"vertexnormal\" is vertex attribute; cannot be on %s.\n",
                typenames[(yyvsp[-1]).etype]);
             kb_error(2651,errmsg,COMMAND_ERROR);
           }
           (yyval).i = makenode(PRINT_VERTEXNORMAL_,(yyvsp[-1]).i,0);
           list[(yyval).i].op1.localnum = list[(yyvsp[-1]).i].op2.localnum;
           goto vnexit;
           break;
         case PARAM_:
           if ( (yyvsp[-1]).etype != VERTEX )
           { sprintf(errmsg,"\"p\" is %s attribute; cannot be on %s.\n",
                typenames[VERTEX], typenames[(yyvsp[-1]).etype]);
             kb_error(2652,errmsg,COMMAND_ERROR);
           }
           int_val = V_PARAM_ATTR;
           break;
         case GET_EXTRA_ATTR_:
           if ( (yyvsp[-1]).etype != (yyvsp[0]).etype )
           { sprintf(errmsg,"\"%s\" is %s attribute; cannot be on %s.\n",
                EXTRAS((yyvsp[0]).etype)[(yyvsp[0]).qnum & YYSHIFTMASK].name,
                   typenames[(yyvsp[0]).etype], typenames[(yyvsp[-1]).etype]);
             kb_error(2653,errmsg,COMMAND_ERROR);
           }
           int_val = (yyvsp[0]).qnum;
           break;
         default:
           k = makenode(ATTRIBUTE,(yyvsp[0]).i,(yyvsp[0]).qnum); 
           list[k].op1.localnum = list[(yyvsp[-1]).i].op2.localnum;
           k = makenode(QUALIFIED_ATTRIBUTE,(yyvsp[-1]).i,k); 
           (yyval).i = makenode(PRINT_,k,0);
           goto vnexit;
       }
       int_val |= ((yyvsp[-1]).etype << YYTYPESHIFT);
       (yyval).i = makenode(PRINT_ATTR_ARRAY_,(yyvsp[-1]).i,0);
vnexit: ;
     ;}
    break;

  case 487:

    { (yyval).i = makenode(PRINT_LETTER_,(yyvsp[0]).i,0); ;}
    break;

  case 488:

    { (yyval).i = makenode(PRINT_LETTER_,(yyvsp[0]).i,0); ;}
    break;

  case 489:

    { (yyval).i = makenode(PRINT_LETTER_,(yyvsp[0]).i,0); ;}
    break;

  case 490:

    { kb_error(2385,
  "Syntax: PRINT  procedure | expression | stringexpression \n",Q_ERROR ); ;}
    break;

  case 491:

    { (yyval).i = makenode(SHOW_TRANS_,(yyvsp[0]).i,0); ;}
    break;

  case 492:

    { kb_error(2386,"Syntax: SHOW_TRANS \"string\"\n",Q_ERROR);;}
    break;

  case 493:

    { backquote_flag = 1; (yyval).i = makenode(BACKQUOTE_START_,0,0);
            if ( local_nest_depth == 0 )
               init_local_scope(0);
            begin_local_scope();
            list[listtop++].type = SETUP_FRAME_; ;}
    break;

  case 494:

    { verb_flag = 0; backquote_flag = 0;  end_local_scope();
       (yyval).i = makenode(BACKQUOTE_END_,(yyvsp[-2]).i,(yyvsp[-1]).i);;}
    break;

  case 495:

    { (yyval).i = makenode(ACOMMANDEXPR_,(yyvsp[-1]).i,(yyvsp[0]).i);  ;}
    break;

  case 496:

    { 
     backquote_flag = 0;
     kb_error(3805,"Backquote syntax: ` commands ` , expression\n",
         Q_ERROR);
   ;}
    break;

  case 497:

    { (yyval).i = makenode(FUNCTION_CALL_,(yyvsp[-2]).i,0);
        (yyval).i = makenode(FUNCTION_CALL_RETURN_,(yyval).i,0);
      ;}
    break;

  case 498:

    { (yyval).i = makenode(FUNCTION_CALL_,(yyvsp[-3]).i,(yyvsp[-1]).i); 
        makenode(FUNCTION_CALL_RETURN_,(yyval).i,0);
      ;}
    break;

  case 499:

    { kb_error(2870, "Function call needs argument list.\n",Q_ERROR); ;}
    break;

  case 500:

    { (yyval).i = makenode(PROCEDURE_CALL_,(yyvsp[-2]).i,0); 
        (yyval).i = makenode(PROCEDURE_CALL_RETURN_,(yyval).i,0);
      ;}
    break;

  case 501:

    { (yyval).i = makenode(PROCEDURE_CALL_,(yyvsp[-3]).i,(yyvsp[-1]).i); 
        makenode(PROCEDURE_CALL_RETURN_,(yyval).i,0);
      ;}
    break;

  case 502:

    { kb_error(2871, "Procedure call needs argument list.\n",Q_ERROR); ;}
    break;

  case 503:

    { (yyval).i = makenode(WRAP_VERTEX_,(yyvsp[-3]).i,(yyvsp[-1]).i); ;}
    break;

  case 504:

    { kb_error(3808,"Syntax: wrap_vertex(vertex_number,wrap_code_number)\n",
          Q_ERROR); ;}
    break;

  case 505:

    {  (yyval).i = makenode(VIEW_TRANSFORM_PARITY_,(yyvsp[-1]).i,0); ;}
    break;

  case 506:

    { kb_error(2602,
          "view_transform_parity needs one index.\n", Q_ERROR); ;}
    break;

  case 507:

    { int nn = makenode(TEXT_SPOT_,(yyvsp[-5]).i,(yyvsp[-3]).i);
      (yyval).i = makenode(DISPLAY_TEXT_,nn,(yyvsp[-1]).i);
    ;}
    break;

  case 508:

    { kb_error(4683,"Syntax: text_id := DISPLAY_TEXT(x,y,string)\n",Q_ERROR );
    ;}
    break;

  case 509:

    { kb_error(3254,"Syntax: text_id := DISPLAY_TEXT(x,y,string)\n",Q_ERROR );
    ;}
    break;

  case 510:

    { (yyval).i = makenode(DELETE_TEXT_,(yyvsp[-1]).i,0);
   ;}
    break;

  case 511:

    { kb_error(4684,"Syntax: DELETE_TEXT(text_id)\n",Q_ERROR );
    ;}
    break;

  case 512:

    { (yyval).i = makenode(CREATE_VERTEX_,(yyvsp[-1]).i,0); ;}
    break;

  case 513:

    { kb_error(2387,"Syntax: NEW_VERTEX(x,y,...) \n",Q_ERROR); ;}
    break;

  case 514:

    { (yyval).i = makenode(CREATE_EDGE_,(yyvsp[-3]).i,(yyvsp[-1]).i); ;}
    break;

  case 515:

    { kb_error(2388,"Syntax: NEW_EDGE(tail_id,head_id) \n",Q_ERROR); ;}
    break;

  case 516:

    { (yyval).i = makenode(CREATE_FACET_,(yyvsp[-1]).i,0); ;}
    break;

  case 517:

    { kb_error(2389,"Syntax: NEW_FACET(edge1,edge2,...) \n",Q_ERROR); ;}
    break;

  case 518:

    { (yyval).i = makenode(CREATE_BODY_,0,0); ;}
    break;

  case 519:

    { kb_error(2390,"Syntax: NEW_BODY \n",Q_ERROR); ;}
    break;

  case 520:

    { (yyval).i = makenode(ELINDEX_,(yyvsp[0]).i,0); ;}
    break;

  case 521:

    { (yyval).i = makenode(ELINDEX_,(yyvsp[-2]).i,(yyvsp[0]).i); ;}
    break;

  case 522:

    { (yyval).i = makenode(PUSH_ELEMENT_ID_,(yyvsp[0]).i,(yyvsp[0]).qnum); ;}
    break;

  case 523:

    { (yyval).i = makenode(PUSH_ELEMENT_ID_,-(yyvsp[-1]).i,(yyvsp[-1]).qnum); ;}
    break;

  case 524:

    { (yyval).i = makenode(PUSH_ELEMENT_ID_,(yyvsp[0]).i,(yyvsp[0]).qnum); ;}
    break;

  case 525:

    { (yyval).i = makenode(PUSH_ELEMENT_ID_,-(yyvsp[-1]).i,(yyvsp[-1]).qnum); ;}
    break;

  case 526:

    { (yyval).i = makenode(VALID_ELEMENT_,(yyvsp[-4]).i,(yyvsp[-2]).i); ;}
    break;

  case 527:

    { kb_error(3904,"Syntax: valid_element(element_type[expr]) \n",Q_ERROR); ;}
    break;

  case 528:

    { (yyval).i = makenode(VALID_CONSTRAINT_,(yyvsp[-1]).i,0); ;}
    break;

  case 529:

    { kb_error(1901,"Syntax: valid_constraint(expr) \n",Q_ERROR); ;}
    break;

  case 530:

    { (yyval).i = makenode(VALID_BOUNDARY_,(yyvsp[-1]).i,0); ;}
    break;

  case 531:

    { kb_error(3029,"Syntax: valid_boundary(expr) \n",Q_ERROR); ;}
    break;

  case 532:

    { (yyval).i = makenode(MERGE_VERTEX_,(yyvsp[-3]).i,(yyvsp[-1]).i); ;}
    break;

  case 533:

    { kb_error(3885,"Syntax: VERTEX_MERGE(first_id,second_id) \n",Q_ERROR); ;}
    break;

  case 534:

    { (yyval).i = makenode(MERGE_EDGE_,(yyvsp[-3]).i,(yyvsp[-1]).i); ;}
    break;

  case 535:

    { kb_error(3637,"Syntax: EDGE_MERGE(first_oid,second_oid) \n",Q_ERROR); ;}
    break;

  case 536:

    { (yyval).i = makenode(MERGE_FACET_,(yyvsp[-3]).i,(yyvsp[-1]).i); ;}
    break;

  case 537:

    { kb_error(3607,"Syntax: FACET_MERGE(first_oid,second_oid) \n",Q_ERROR); ;}
    break;

  case 538:

    { int_val = (yyvsp[-1]).i; (yyval).i = makenode(MATRIX_MULTIPLY_,(yyvsp[-5]).i,(yyvsp[-3]).i); ;}
    break;

  case 539:

    { kb_error(3790,"matrix_multiply syntax: matrix_multiply(mat1,mat2,mat3)\n",
     COMMAND_ERROR);
   ;}
    break;

  case 540:

    { kb_error(3791,"matrix_multiply third argument is not an array.\n",
     COMMAND_ERROR);
   ;}
    break;

  case 541:

    { kb_error(3792,"matrix_multiply second argument is not an array.\n",
     COMMAND_ERROR);
   ;}
    break;

  case 542:

    { kb_error(3793,"matrix_multiply first argument is not an array.\n",
     COMMAND_ERROR);
   ;}
    break;

  case 543:

    { (yyval).i = makenode(MATRIX_DETERMINANT_,(yyvsp[-1]).i,0); ;}
    break;

  case 544:

    { (yyval).i = makenode(MATRIX_INVERSE_,(yyvsp[-3]).i,(yyvsp[-1]).i); ;}
    break;

  case 545:

    { kb_error(3794,"matrix_inverse syntax: matrix_inverse(mat1,mat2)\n",
     COMMAND_ERROR);
   ;}
    break;

  case 546:

    { kb_error(3795,"matrix_inverse second argument is not an array.\n",
     COMMAND_ERROR);
   ;}
    break;

  case 547:

    { kb_error(3796,"matrix_inverse first argument is not an array.\n",
     COMMAND_ERROR);
   ;}
    break;

  case 548:

    {  (yyval).i = LIST_; loopdepth++; ;}
    break;

  case 549:

    { erroutstring("Syntax: LIST element_gen [ name ] [ WHERE rexpr ]\n");
     erroutstring("        LIST TOPINFO\n");
     erroutstring("        LIST BOTTOMINFO\n");
     erroutstring("        LIST ATTRIBUTES\n");
     erroutstring("        LIST PROCEDURES\n");
     erroutstring("        LIST QUANTITY quantityname\n");
     erroutstring("        LIST METHOD_INSTANCE instancename\n");
     erroutstring("        LIST CONSTRAINT rexpr or name\n");
     erroutstring("        LIST BOUNDARY rexpr or name\n");
     kb_error(1899,NULL,Q_ERROR);
   ;}
    break;

  case 550:

    {   (yyval).i = DELETE_; loopdepth++; ;}
    break;

  case 551:

    { kb_error(2391,"Syntax: DELETE element_gen [ name ] [ WHERE rexpr ]\n",Q_ERROR); ;}
    break;

  case 552:

    {   (yyval).i = VERTEX_AVERAGE_; loopdepth++; ;}
    break;

  case 553:

    {   (yyval).i = RAW_VERTEX_AVERAGE_; loopdepth++; ;}
    break;

  case 554:

    {   (yyval).i = RAWEST_VERTEX_AVERAGE_; loopdepth++; ;}
    break;

  case 555:

    { kb_error(2392,"Syntax: VERTEX_AVERAGE element_gen [ name ] [ WHERE rexpr ]\n",Q_ERROR); ;}
    break;

  case 556:

    { kb_error(2393,"Syntax: RAW_VERTEX_AVERAGE element_gen [ name ] [ WHERE rexpr ]\n",Q_ERROR); ;}
    break;

  case 557:

    { kb_error(2394,"Syntax: RAWEST_VERTEX_AVERAGE element_gen [ name ] [ WHERE expr ]\n",Q_ERROR); ;}
    break;

  case 558:

    {   (yyval).i = DISSOLVE_; loopdepth++; ;}
    break;

  case 559:

    { kb_error(2395,"Syntax: DISSOLVE element_gen [ name ] [ WHERE rexpr ]\n",Q_ERROR); ;}
    break;

  case 560:

    {   (yyval).i = REVERSE_ORIENTATION_; loopdepth++; ;}
    break;

  case 561:

    { kb_error(3250,"Syntax: REVERSE_ORIENTATION element_gen [ name ] [ WHERE rexpr ]\n",Q_ERROR); ;}
    break;

  case 562:

    {   (yyval).i = REFINE_; loopdepth++; ;}
    break;

  case 563:

    { kb_error(2396,"Syntax: REFINE element_gen [ name ] [ WHERE rexpr ]\n",Q_ERROR); ;}
    break;

  case 564:

    {   (yyval).i = EDGESWAP_; loopdepth++; ;}
    break;

  case 565:

    { kb_error(2397,"Syntax: EDGESWAP edge_gen [ name ] [ WHERE rexpr ]\n",Q_ERROR); ;}
    break;

  case 566:

    {   (yyval).i = T1_EDGESWAP_; loopdepth++; ;}
    break;

  case 567:

    { kb_error(4009,"Syntax: T1_EDGESWAP edge_gen [ name ] [ WHERE rexpr ]\n",Q_ERROR); ;}
    break;

  case 568:

    {   (yyval).i = EQUIANGULATE_; loopdepth++; ;}
    break;

  case 569:

    { kb_error(2545,"Syntax: EQUIANGULATE_ edge_gen [ name ] [ WHERE rexpr ]\n",Q_ERROR); ;}
    break;

  case 570:

    {   (yyval).i = POP_; loopdepth++; ;}
    break;

  case 571:

    { kb_error(2431,"Syntax: POP element_gen [ name ] [ WHERE rexpr ]\n",Q_ERROR); ;}
    break;

  case 572:

    {   (yyval).i = POP_TRI_TO_EDGE_; loopdepth++; ;}
    break;

  case 573:

    { kb_error(2800,
     "Syntax: POP_TRI_TO_EDGE facet_gen [ name ] [ WHERE rexpr ]\n",Q_ERROR); ;}
    break;

  case 574:

    {   (yyval).i = POP_EDGE_TO_TRI_; loopdepth++; ;}
    break;

  case 575:

    { kb_error(2801,"Syntax: POP_EDGE_TO_TRI edge_gen [ name ] [ WHERE rexpr ]\n",Q_ERROR); ;}
    break;

  case 576:

    {   (yyval).i = POP_QUAD_TO_QUAD_; loopdepth++; ;}
    break;

  case 577:

    { kb_error(2802,"Syntax: POP_QUAD_TO_QUAD facet_gen [ name ] [ WHERE rexpr ]\n",Q_ERROR); ;}
    break;

  case 578:

    {   (yyval).i = FIX_; loopdepth++; ;}
    break;

  case 579:

    {  (yyval).i = UNFIX_; loopdepth++; ;}
    break;

  case 580:

    { (yyval).i = makenode(FIX_PARAMETER_,(yyvsp[0]).i,0); ;}
    break;

  case 581:

    { (yyval).i = makenode(UNFIX_PARAMETER_,(yyvsp[0]).i,0); ;}
    break;

  case 582:

    { (yyval).i = makenode(FIX_QUANTITY_,(yyvsp[0]).i,0);;}
    break;

  case 583:

    { (yyval).i = makenode(UNFIX_QUANTITY_,(yyvsp[0]).i,0);;}
    break;

  case 584:

    { kb_error(2398,"Syntax: FIX element_gen [ name ] [ WHERE rexpr ] or FIX quantity_name\n",
      Q_ERROR); ;}
    break;

  case 585:

    { kb_error(2399,"Syntax: UNFIX element_gen [ name ] [ WHERE rexpr ] or UNFIX quantity_name\n",
      Q_ERROR); ;}
    break;

  case 586:

    { if ( const_expr_flag ) YYABORT; (yyval).etype = (yyval).i = VERTEX; ;}
    break;

  case 587:

    { if ( const_expr_flag ) YYABORT; (yyval).etype = (yyval).i = EDGE   ; ;}
    break;

  case 588:

    { if ( const_expr_flag ) YYABORT; (yyval).etype = (yyval).i = FACET  ; ;}
    break;

  case 589:

    { if ( const_expr_flag ) YYABORT; (yyval).etype = (yyval).i = BODY  ; ;}
    break;

  case 590:

    { if ( const_expr_flag ) YYABORT; (yyval).etype = (yyval).i = FACETEDGE; ;}
    break;

  case 591:

    { int next;
         next = makenode(INIT_ELEMENT_,(yyvsp[0]).i,0); 
         (yyval).i = makenode(NEXT_ELEMENT_,next,0);
         (yyval).etype = (yyvsp[0]).i;
       ;}
    break;

  case 592:

    { int next;
         next = makenode(INIT_SUBELEMENT_,(yyvsp[-1]).i,(yyvsp[0]).i); 
         (yyval).i = makenode(NEXT_ELEMENT_,next,0);
         (yyval).etype = (yyvsp[0]).etype;
       ;}
    break;

  case 593:

    { verb_flag = 0; (yyval).i = makenode(INDEXED_ELEMENT_,(yyvsp[-3]).i,(yyvsp[-1]).i);
       (yyval).etype = (yyvsp[-3]).i; ;}
    break;

  case 594:

    { kb_error(3809,"Missing index of element.\n",Q_ERROR); ;}
    break;

  case 595:

    { kb_error(3832,"Missing right bracket after index expression.\n",Q_ERROR); ;}
    break;

  case 596:

    { verb_flag = 0; (yyval).i = makenode(SYMBOL_ELEMENT_,(yyvsp[0]).i,0);
                        (yyval).etype = list[(yyval).i].op1.eltype; ;}
    break;

  case 597:

    { verb_flag = 0;
     elsym = symbol_lookup("self");
     if ( elsym == NULL )
       kb_error(2400,"SELF not defined, since not in attribute function def.\n",
           COMMAND_ERROR);
     (yyval).i = makenode(SELF_ELEMENT_,elsym->type,0);
     (yyval).etype = elsym->type;
   ;}
    break;

  case 598:

    { verb_flag = 0; (yyval).i = makenode(ELEMENT_IDENT_,(yyvsp[0]).i,0);
                          list[(yyval).i].op1.eltype = (yyvsp[0]).etype;
                        (yyval).etype = (yyvsp[0]).etype; ;}
    break;

  case 599:

    { (yyval) = (yyvsp[-1]); ;}
    break;

  case 600:

    { (yyval) = (yyvsp[0]); ;}
    break;

  case 601:

    {  subtype = (yyvsp[-3]).i; (yyval).i = makenode(INDEXED_SUBTYPE_,(yyvsp[-4]).i,(yyvsp[-1]).i);
           (yyval).etype = (yyvsp[-3]).etype; ;}
    break;

  case 602:

    { kb_error(3810,"Missing index of element.\n",Q_ERROR); ;}
    break;

  case 603:

    { kb_error(3812,"Missing right bracket after index expression.\n",Q_ERROR); ;}
    break;

  case 604:

    { int type = list[(yyvsp[0]).i].op1.eltype;
          begin_scope(); /* ended at end of aggregate */
          elsym = symbol_add(default_name,type);
          elsym->localnum = list[(yyvsp[0]).i].op2.localnum;
          strcpy(last_name,default_name);
          (yyvsp[0]).symptr = elsym;
          (yyval).i = makenode(SINGLE_ELEMENT_,(yyvsp[0]).i,0);
          (yyval).symptr = elsym;
          (yyval).etype = (yyvsp[0]).etype;
        ;}
    break;

  case 605:

    { int type = list[(yyvsp[-1]).i].op1.eltype;
          begin_scope(); /* ended at end of aggregate */
          elsym = symbol_add((yyvsp[0]).lexeme,type);
          elsym->localnum = list[(yyvsp[-1]).i].op2.localnum;
          strcpy(last_name,(yyvsp[0]).lexeme);
          (yyval).i = makenode(SINGLE_ELEMENT_,(yyvsp[-1]).i,0);
          list[(yyvsp[-1]).i].op5.string =
            (char*)mycalloc(strlen(elsym->name)+1,1);
          list[(yyvsp[-1]).i].flags |= HAS_STRING_5;
          strcpy(list[(yyvsp[-1]).i].op5.string,elsym->name);
          (yyvsp[-1]).symptr = elsym;
          (yyval).symptr = elsym;
          elsym = symbol_add(default_name,type); /* current id as default */
          elsym->localnum = list[(yyvsp[-1]).i].op2.localnum;
          (yyval).etype = (yyvsp[-1]).etype;
        ;}
    break;

  case 606:

    { int type = list[(yyvsp[0]).i+list[(yyvsp[0]).i].left].op1.eltype;
          begin_scope(); /* ended at end of aggregate */
          elsym = symbol_add(default_name,type);
          elsym->localnum = list[(yyvsp[0]).i].op2.localnum;
          strcpy(last_name,default_name);
          list[(yyvsp[0]).i].op5.string =
            (char*)mycalloc(strlen(default_name)+1,1);
          list[(yyvsp[0]).i].flags |= HAS_STRING_5;
          strcpy(list[(yyvsp[0]).i].op5.string,default_name);
          (yyvsp[0]).symptr = elsym;
          (yyval).symptr = elsym;
          (yyval).i = (yyvsp[0]).i;
          (yyval).etype = (yyvsp[0]).etype;
        ;}
    break;

  case 607:

    { int type = list[(yyvsp[-1]).i+list[(yyvsp[-1]).i].left].op1.eltype;
          begin_scope(); /* ended at end of aggregate */
          elsym = symbol_add((yyvsp[0]).lexeme,type);
          elsym->localnum = list[(yyvsp[-1]).i].op2.localnum;
          strcpy(last_name,(yyvsp[0]).lexeme);
          list[(yyvsp[-1]).i].op5.string =
            (char*)mycalloc(strlen(elsym->name)+1,1);
          list[(yyvsp[-1]).i].flags |= HAS_STRING_5;
          strcpy(list[(yyvsp[-1]).i].op5.string,elsym->name);
          (yyvsp[-1]).symptr = elsym;
          (yyval).symptr = elsym;
          (yyval).i = (yyvsp[-1]).i;
          elsym = symbol_add(default_name,type); /* current id as default */
          elsym->localnum = list[(yyvsp[-1]).i].op2.localnum;
          (yyval).etype = (yyvsp[-1]).etype;
        ;}
    break;

  case 608:

    { kb_error(1890,"Name already in use as procedure name.\n",COMMAND_ERROR); ;}
    break;

  case 609:

    { kb_error(1891,"Name already in use as procedure name.\n",COMMAND_ERROR); ;}
    break;

  case 610:

    { kb_error(1892,"Name already in use as variable name.\n",COMMAND_ERROR); ;}
    break;

  case 611:

    { kb_error(1893,"Name already in use as variable name.\n",COMMAND_ERROR); ;}
    break;

  case 612:

    { (yyval).i = makenode(WHERE_,(yyvsp[-2]).i,(yyvsp[0]).i); 
           (yyval).symptr = (yyvsp[-2]).symptr;
           ;}
    break;

  case 613:

    { kb_error(3901,"Missing boolean expression after WHERE.\n",Q_ERROR); ;}
    break;

  case 614:

    { (yyval).i = ON_; ;}
    break;

  case 615:

    { (yyval).i = OFF_; ;}
    break;

  case 616:

    { (yyval).i = makenode(QUOTATION_,0,0); ;}
    break;

  case 617:

    { int size1 = strlen(list[(yyval).i].op1.string);
       int size2 = strlen(yytext);
       (yyval).i = (yyvsp[-1]).i;
       list[(yyval).i].op1.string = (char*)kb_realloc(list[(yyval).i].op1.string,
                                    size1+size2+1);
       strncpy(list[(yyval).i].op1.string+size1,yytext,size2);
     ;}
    break;

  case 618:

    { (yyval).i = (yyvsp[0]).i ;}
    break;

  case 619:

    { (yyval).i = makenode(STRINGGLOBAL_,(yyvsp[0]).i,0); ;}
    break;

  case 620:

    { (yyval).i = makenode(PERM_STRINGGLOBAL_,(yyvsp[0]).i,0); ;}
    break;

  case 621:

    { (yyval).i = makenode(DATAFILENAME_,(yyvsp[0]).i,0); ;}
    break;

  case 622:

    { (yyval).i = makenode(WARNING_MESSAGES_,(yyvsp[0]).i,0); ;}
    break;

  case 623:

    { (yyval).i = makenode(DATE_AND_TIME_,0,0); ;}
    break;

  case 624:

    { (yyval).i = makenode(GET_TRANSFORM_EXPR_,0,0); ;}
    break;

  case 625:

    { (yyval).i = makenode(SPRINTFHEAD_,(yyvsp[0]).i,0); ;}
    break;

  case 626:

    { kb_error(3894,"Missing format string after SPRINTF.\n",Q_ERROR); ;}
    break;

  case 627:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 628:

    { (yyval).i = makenode(SPRINTF_,(yyvsp[-2]).i,(yyvsp[0]).i); ;}
    break;

  case 629:

    { kb_error(3806,"Error in SPRINTF arguments.\n",Q_ERROR); ;}
    break;

  case 630:

    { (yyval).qnum = (yyvsp[0]).i; (yyval).i = COORD_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 631:

    { (yyval).qnum = (yyvsp[0]).i; (yyval).i = PARAM_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 632:

    { (yyval).i = GET_LENGTH_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 633:

    { (yyval).i = GET_MEANCURV_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 634:

    { (yyval).i = GET_SHOW_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 635:

    { (yyval).i = GET_ORIENTATION_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 636:

    { (yyval).i = GET_STAR_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 637:

    { (yyval).i = GET_SQ_MEAN_CURV_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 638:

    { (yyval).i = GET_DIHEDRAL_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 639:

    { (yyval).i = GET_VALENCE_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 640:

    { (yyval).i = GET_AREA_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 641:

    { (yyval).i = GET_VOLUME_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 642:

    { (yyval).i = GET_VOLCONST_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 643:

    { (yyval).i = GET_TARGET_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 644:

    { (yyval).i = GET_MPI_TASK_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 645:

    { (yyval).i = GET_DENSITY_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 646:

    { (yyval).i = GET_PRESSURE_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 647:

    { (yyval).i = GET_ID_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 648:

    { (yyval).i = GET_OID_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 649:

    { (yyval).i = GET_TAG_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 650:

    { (yyval).i = GET_COLOR_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 651:

    { (yyval).i = GET_FRONTCOLOR_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 652:

    { (yyval).i = GET_BACKCOLOR_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 653:

    { (yyval).i = GET_BACKBODY_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 654:

    { (yyval).i = GET_FRONTBODY_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 655:

    { (yyval).i = GET_ORIGINAL_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 656:

    { (yyval).i = GET_FIXED_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 657:

    { (yyval).i = GET_NO_REFINE_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 658:

    { (yyval).i = GET_HIT_PARTNER_; (yyval).datatype=REAL_TYPE;;}
    break;

  case 659:

    { (yyval).i = GET_NONCONTENT_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 660:

    { (yyval).i = GET_NO_DISPLAY_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 661:

    { (yyval).i = GET_FIXEDVOL_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 662:

    { (yyval).i = GET_AXIAL_POINT_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 663:

    { (yyval).i = GET_TRIPLE_PT_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 664:

    { (yyval).i = GET_TETRA_PT_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 665:

    { (yyval).i = GET_MIDV_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 666:

    { (yyval).i = GET_WRAP_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 667:

    { (yyval).i = GET_MID_EDGE_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 668:

    { (yyval).i = GET_MID_FACET_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 669:

    { (yyval).i = GET_BARE_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 670:

    { (yyval).i = GET_PHASE_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 671:

    { (yyval).qnum = (yyvsp[0]).i;  (yyval).i = GET_QUANTITY_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 672:

    { (yyval).qnum = (yyvsp[0]).i;  (yyval).i = GET_INSTANCE_; (yyval).datatype=REAL_TYPE; ;}
    break;

  case 673:

    { struct extra *ex;
            (yyval).i = GET_EXTRA_ATTR_ ;  
            (yyval).qnum = (yyvsp[0]).qnum + ((yyvsp[0]).etype << YYTYPESHIFT); 
            (yyval).etype = (yyvsp[0]).etype; 
            ex = EXTRAS((yyvsp[0]).etype) + (yyvsp[0]).qnum;
            (yyval).datatype= (ex->type <= MAX_NUMERIC_TYPE) ? REAL_TYPE : ex->type; 
          ;}
    break;

  case 674:

    { if ( const_expr_flag ) { YYABORT; /* illegal for const rexpr */ }
          (yyval)= (yyvsp[0]);
        ;}
    break;

  case 675:

    { 
        if ( (datafile_flag && boundary_expr_flag && ((yyvsp[0]).i==PARAM_))
          ||  ( datafile_flag &&  ((yyvsp[0]).i==COORD_)  ) )
             { coord_num = (yyvsp[0]).qnum; (yyval).i = makenode(PUSHPARAM,0,0); }
        else
        { 
          (yyval).i = makenode(ATTRIBUTE,(yyvsp[0]).i,(yyvsp[0]).qnum); 
          (yyval).datatype = list[(yyval).i].datatype = (yyvsp[0]).datatype;
        }
     ;}
    break;

  case 676:

    {
       if ( const_expr_flag ) { YYABORT; /* illegal for const rexpr */ }
       switch ( (yyvsp[-1]).i )
       { case COORD_:
           (yyval).i = makenode(INDEXED_COORD_,(yyvsp[0]).i,0);
           break;
         case GET_VERTEXNORMAL_:
           (yyval).i = makenode(GET_VERTEXNORMAL_,(yyvsp[0]).i,0);
           break;
         case PARAM_:
           if ( (yyvsp[-1]).etype == VERTEX )
           { (yyval).qnum = V_PARAM_ATTR;
             (yyval).i =
               makenode(INDEXED_ATTRIBUTE,(yyvsp[0]).i,(yyval).qnum+((yyvsp[-1]).etype<<YYTYPESHIFT));
           } else
           kb_error(1895,"Illegal subscript.\n",COMMAND_ERROR);
           break;
         case GET_EXTRA_ATTR_:
           (yyval).i = makenode(INDEXED_ATTRIBUTE,(yyvsp[0]).i,(yyvsp[-1]).qnum);
           (yyval).qnum = (yyvsp[-1]).qnum;
           break;
         default:
            kb_error(1498,"Illegal subscript.\n",COMMAND_ERROR);
       }
       (yyval).datatype = list[(yyval).i].datatype = (yyvsp[-1]).datatype;
    ;}
    break;

  case 677:

    {  (yyval).i = makenode(ON_CONSTRAINT_NAME,(yyvsp[0]).i,0);
        (yyval).datatype = REAL_TYPE;
     ;}
    break;

  case 678:

    {  (yyval).i = makenode((yyvsp[-1]).i,(yyvsp[0]).i,0);
        (yyval).datatype = REAL_TYPE;
     ;}
    break;

  case 679:

    { kb_error(3807,"Need constraint name or number after ON_CONSTRAINT\n",
         Q_ERROR); ;}
    break;

  case 680:

    {  (yyval).i = makenode(ON_BOUNDARY_NAME,(yyvsp[0]).i,0);
        (yyval).datatype = REAL_TYPE;
     ;}
    break;

  case 681:

    {  (yyval).i = makenode((yyvsp[-1]).i,(yyvsp[0]).i,0); 
        (yyval).datatype = REAL_TYPE;
     ;}
    break;

  case 682:

    { kb_error(3897,"Need constraint name or number after ON_BOUNDARY\n",
         Q_ERROR); ;}
    break;

  case 683:

    {  (yyval).i = makenode(HIT_CONSTRAINT_NAME,(yyvsp[0]).i,0); 
        (yyval).datatype = REAL_TYPE;
     ;}
    break;

  case 684:

    {  (yyval).i = makenode((yyvsp[-1]).i,(yyvsp[0]).i,0); 
        (yyval).datatype = REAL_TYPE;
     ;}
    break;

  case 685:

    { kb_error(3898,"Need constraint name or number after HIT_CONSTRAINT\n",
         Q_ERROR); ;}
    break;

  case 686:

    {  (yyval).i = makenode((yyvsp[-1]).i,(yyvsp[0]).i,0); 
        (yyval).datatype = REAL_TYPE;
     ;}
    break;

  case 687:

    { kb_error(4004,"Need constraint name or number after ON_QUANTITY\n",
         Q_ERROR); ;}
    break;

  case 688:

    {  (yyval).i = makenode((yyvsp[-1]).i,(yyvsp[0]).i,0); 
        (yyval).datatype = REAL_TYPE;
     ;}
    break;

  case 689:

    { kb_error(3813,"Need constraint name or number after ON_METHOD_INSTANCE_\n",
         Q_ERROR); ;}
    break;

  case 690:

    { 
        if ( datafile_flag ) (yyval).i = (yyvsp[0]).i;
        else
        {
          elsym = symbol_lookup(default_name);
          if ( elsym ) 
          {
            check_element_type(list[(yyvsp[0]).i].type,elsym->type);
            list[(yyvsp[0]).i].op1.localnum = elsym->localnum;
          }
          else kb_error(1896,"\nMissing element for attribute. (Get quantity value with name.value) \n",COMMAND_ERROR);
        }
        list[(yyvsp[0]).i].datatype = (yyvsp[0]).datatype;
      ;}
    break;

  case 691:

    { (yyval).i = makenode(QUALIFIED_ATTRIBUTE,(yyvsp[-1]).i,(yyvsp[0]).i); 
             list[(yyvsp[0]).i].op1.localnum = list[(yyvsp[-1]).i].op2.localnum;
             list[(yyval).i].datatype = (yyvsp[0]).datatype;
           ;}
    break;

  case 692:

    { sprintf(errmsg,"\"%s\" is not a attribute name.\n",(yyvsp[0]).lexeme);
           kb_error(3458,errmsg,Q_ERROR); 
         ;}
    break;

  case 693:

    { (yyval).i = makenode(IS_DEFINED_,(yyvsp[-1]).i,0); ;}
    break;

  case 694:

    { kb_error(3814,"Syntax: IS_DEFINED ( quoted_string )\n",Q_ERROR); ;}
    break;

  case 695:

    { kb_error(3815,"Missing closing parenthesis for IS_DEFINED\n",Q_ERROR); ;}
    break;

  case 696:

    { int etype;
             (yyval).qnum = (yyvsp[-1]).qnum; 
             etype = (yyvsp[-1]).etype;
             (yyval).i = makenode(SIZEOF_ATTR_,(yyval).qnum,etype); ;}
    break;

  case 697:

    { (yyval).i = makenode(SIZEOF_ARRAY_,(yyvsp[-1]).i,0); ;}
    break;

  case 698:

    { (yyval).i = makenode(SIZEOF_STRING_,(yyvsp[-1]).i,0); ;}
    break;

  case 699:

    { strcpy(errmsg,"Syntax: SIZEOF ( extra_attribute )\n");
       strcat(errmsg,"        SIZEOF ( array_name ) \n");
       strcat(errmsg,"        SIZEOF ( string_expr ) \n");
       kb_error(3816,errmsg,Q_ERROR);
     ;}
    break;

  case 700:

    { (yyval).i = makenode(TOGGLEVALUE,(yyvsp[0]).i,0); ;}
    break;

  case 701:

    { (yyval).i = makenode(TOGGLEVALUE,AUTOCHOP_,0); ;}
    break;

  case 702:

    { (yyval).i = makenode(TOGGLEVALUE,LAGRANGE_,0); ;}
    break;

  case 703:

    { (yyval).i = makenode(EPRINT_,(yyvsp[0]).i,0); ;}
    break;

  case 704:

    { kb_error(2886,"Syntax: EPRINT expression\n",Q_ERROR); ;}
    break;

  case 705:

    { (yyval).i = (yyvsp[-1]).i; ;}
    break;

  case 706:

    { kb_error(2401,"Missing closing parenthesis?\n",Q_ERROR); ;}
    break;

  case 707:

    { (yyval).i = makenode(GET_INTERNAL_,(yyvsp[0]).i,0); ;}
    break;

  case 708:

    { (yyval).i = makenode(GET_INTERNAL_,V_SCALE,0); ;}
    break;

  case 709:

    { (yyval).i = makenode(PUSHGLOBAL_,(yyvsp[0]).i,0); ;}
    break;

  case 710:

    { (yyval).i = makenode(PUSH_PERM_GLOBAL_,(yyvsp[0]).i,0); ;}
    break;

  case 711:

    { (yyval).i = makenode(PUSH_PARAM_EXTRA_,(yyvsp[-2]).i,(yyvsp[0]).i); ;}
    break;

  case 712:

    { kb_error(3817,"Permitted optimizing parameter attributes: pdelta pscale\n",
       Q_ERROR);
   ;}
    break;

  case 713:

    { (yyval).i = makenode(DYNAMIC_LOAD_FUNC_,(yyvsp[0]).i,0); ;}
    break;

  case 714:

    { (yyval).i = makenode(PUSHQVALUE_,(yyvsp[0]).i,0); ;}
    break;

  case 715:

    { (yyval).i = makenode(PUSHQPRESSURE_,(yyvsp[-2]).i,0); ;}
    break;

  case 716:

    { (yyval).i = makenode(PUSHQMODULUS_,(yyvsp[-2]).i,0); ;}
    break;

  case 717:

    { (yyval).i = makenode(PUSHQTOLERANCE_,(yyvsp[-2]).i,0); ;}
    break;

  case 718:

    { (yyval).i = makenode(PUSHMMODULUS_,(yyvsp[-2]).i,0); ;}
    break;

  case 719:

    { (yyval).i = makenode(PUSHQTARGET_,(yyvsp[-2]).i,0); ;}
    break;

  case 720:

    { (yyval).i = makenode(PUSHQVALUE_,(yyvsp[-2]).i,0); ;}
    break;

  case 721:

    { (yyval).i = makenode(PUSHMVALUE_,(yyvsp[-2]).i,0); ;}
    break;

  case 722:

    { (yyval).i = makenode(PUSHQVOLCONST_,(yyvsp[-2]).i,0); ;}
    break;

  case 723:

    { (yyval).i = makenode(PUSHQFIXED_,(yyvsp[-2]).i,0); ;}
    break;

  case 724:

    { (yyval).i = makenode(PUSHQENERGY_,(yyvsp[-2]).i,0); ;}
    break;

  case 725:

    { (yyval).i = makenode(PUSHQINFO_ONLY_,(yyvsp[-2]).i,0); ;}
    break;

  case 726:

    { (yyval).i = makenode(PUSHQCONSERVED_,(yyvsp[-2]).i,0); ;}
    break;

  case 727:

    { strcpy(errmsg,
       "Quantity name needs attribute.  Syntax: quantityname.attribute\n");
   strcat(errmsg,"Possible quantity attributes: \n");
   strcat(errmsg,"   value, modulus, pressure, target, tolerance, volconst,\n");
   strcat(errmsg,"   fixed, energy, info_only, conserved\n");
   kb_error(3818,errmsg,Q_ERROR);
  ;}
    break;

  case 728:

    { strcpy(errmsg,"Possible quantity attributes: \n");
   strcat(errmsg,"   value, modulus, pressure, target, tolerance, volconst,\n");
   strcat(errmsg,"   fixed, energy, info_only, conserved\n");
   kb_error(3819,errmsg,Q_ERROR);
  ;}
    break;

  case 729:

    { kb_error(3907,"Possible method instance attributes: value, modulus \n",
             Q_ERROR); 
   ;}
    break;

  case 730:

    { strcpy(errmsg,
     "Method instance name needs attribute.  Syntax: instancename.attribute\n");
   strcat(errmsg,"Possible method instance attributes: value, modulus \n");
   kb_error(3820,errmsg,Q_ERROR);
 ;}
    break;

  case 731:

    { real_val = (REAL)(yyvsp[0]).i; (yyval).i = makenode(PUSHCONST,0,0); ;}
    break;

  case 732:

    { 
          real_val = (REAL)(yyvsp[0]).i; (yyval).i = makenode(PUSHCONST,0,0); 
        ;}
    break;

  case 733:

    { real_val = (yyvsp[0]).r; (yyval).i = makenode(PUSHCONST,0,0); ;}
    break;

  case 734:

    { real_val = (yyvsp[0]).r; (yyval).i = makenode(PUSHCONST,0,0); ;}
    break;

  case 735:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 736:

    { (yyval).i = makenode(PUSHPI,0,0); ;}
    break;

  case 737:

    { (yyval).i = makenode(PUSHE,0,0); ;}
    break;

  case 738:

    { (yyval).i = makenode(PUSHG,0,0); ;}
    break;

  case 739:

    { int_val = (yyvsp[0]).i; (yyval).i = makenode(USERFUNC,0,0); ;}
    break;

  case 740:

    { (yyval).i = makenode((yyvsp[-3]).i,(NTYPE)(yyvsp[-1]).i,0); ;}
    break;

  case 741:

    { sprintf(errmsg,"Syntax: %s ( rexpr )\n",keywordname((yyvsp[-1]).i));
      kb_error(3821,errmsg,Q_ERROR);
    ;}
    break;

  case 742:

    { (yyval).i = makenode((yyvsp[-5]).i,(NTYPE)(yyvsp[-3]).i,(yyvsp[-1]).i); ;}
    break;

  case 743:

    { sprintf(errmsg,"Syntax: %s ( rexpr , rexpr )\n",keywordname((yyvsp[0]).i));
      kb_error(3822,errmsg,Q_ERROR);
    ;}
    break;

  case 744:

    { (yyval).i = makenode('+',(NTYPE)(yyvsp[-2]).i,(NTYPE)(yyvsp[0]).i); ;}
    break;

  case 745:

    { (yyval).i = makenode('-',(NTYPE)(yyvsp[-2]).i,(NTYPE)(yyvsp[0]).i); ;}
    break;

  case 746:

    { (yyval).i = makenode('=',(NTYPE)(yyvsp[-2]).i,(NTYPE)(yyvsp[0]).i); ;}
    break;

  case 747:

    { (yyval).i = makenode('/',(NTYPE)(yyvsp[-2]).i,(NTYPE)(yyvsp[0]).i); ;}
    break;

  case 748:

    { (yyval).i = makenode('*',(NTYPE)(yyvsp[-2]).i,(NTYPE)(yyvsp[0]).i); ;}
    break;

  case 749:

    { (yyval).i = makenode('%',(NTYPE)(yyvsp[-2]).i,(NTYPE)(yyvsp[0]).i); ;}
    break;

  case 750:

    { (yyval).i = makenode(IMOD_,(NTYPE)(yyvsp[-2]).i,(NTYPE)(yyvsp[0]).i); ;}
    break;

  case 751:

    { (yyval).i = makenode(IDIV_,(NTYPE)(yyvsp[-2]).i,(NTYPE)(yyvsp[0]).i); ;}
    break;

  case 752:

    { (yyval).i = makenode(POW,(NTYPE)(yyvsp[-2]).i,(NTYPE)(yyvsp[0]).i); ;}
    break;

  case 753:

    { (yyval).i = makenode(LT_,(NTYPE)(yyvsp[-2]).i,(NTYPE)(yyvsp[0]).i); ;}
    break;

  case 754:

    { (yyval).i = makenode(GT_,(NTYPE)(yyvsp[-2]).i,(NTYPE)(yyvsp[0]).i); ;}
    break;

  case 755:

    { (yyval).i = makenode(NE_,(NTYPE)(yyvsp[-2]).i,(NTYPE)(yyvsp[0]).i); ;}
    break;

  case 756:

    { (yyval).i = makenode(EQ_,(NTYPE)(yyvsp[-2]).i,(NTYPE)(yyvsp[0]).i); ;}
    break;

  case 757:

    { (yyval).i = makenode(LE_,(NTYPE)(yyvsp[-2]).i,(NTYPE)(yyvsp[0]).i); ;}
    break;

  case 758:

    { (yyval).i = makenode(GE_,(NTYPE)(yyvsp[-2]).i,(NTYPE)(yyvsp[0]).i); ;}
    break;

  case 759:

    { (yyval).i = makenode(AND_,(NTYPE)(yyvsp[-2]).i,(NTYPE)(yyvsp[0]).i); ;}
    break;

  case 760:

    { (yyval).i = makenode(OR_,(NTYPE)(yyvsp[-2]).i,(NTYPE)(yyvsp[0]).i); ;}
    break;

  case 761:

    { kb_error(3031,"Cannot add scalar and array\n",Q_ERROR);  ;}
    break;

  case 762:

    { kb_error(3027,"Cannot subtract scalar and array\n",Q_ERROR);  ;}
    break;

  case 763:

    { kb_error(3823,"Bad second expression after +\n",Q_ERROR);  ;}
    break;

  case 764:

    { kb_error(3825,"Bad second expression after -\n",Q_ERROR);  ;}
    break;

  case 765:

    { kb_error(3828,"Bad second expression after =\n",Q_ERROR);  ;}
    break;

  case 766:

    { kb_error(3829,"Bad second expression after /\n",Q_ERROR);  ;}
    break;

  case 767:

    { kb_error(3830,"Bad second expression after *\n",Q_ERROR);  ;}
    break;

  case 768:

    { kb_error(3831,"Bad second expression after %\n",Q_ERROR);  ;}
    break;

  case 769:

    { kb_error(2887,"Bad second expression after IMOD\n",Q_ERROR);  ;}
    break;

  case 770:

    { kb_error(3919,"Bad second expression after IDIV\n",Q_ERROR);  ;}
    break;

  case 771:

    { kb_error(3920,"Bad second expression after ^\n",Q_ERROR);  ;}
    break;

  case 772:

    { kb_error(3921,"Bad second expression after <\n",Q_ERROR);  ;}
    break;

  case 773:

    { kb_error(3922,"Bad second expression after >\n",Q_ERROR);  ;}
    break;

  case 774:

    { kb_error(3923,"Bad second expression after !=\n",Q_ERROR);  ;}
    break;

  case 775:

    { kb_error(3924,"Bad second expression after ==\n",Q_ERROR);  ;}
    break;

  case 776:

    { kb_error(3925,"Bad second expression after <=\n",Q_ERROR);  ;}
    break;

  case 777:

    { kb_error(3926,"Bad second expression after >=\n",Q_ERROR);  ;}
    break;

  case 778:

    { kb_error(3927,"Bad second expression after AND\n",Q_ERROR);  ;}
    break;

  case 779:

    { kb_error(3928,"Bad second expression after OR\n",Q_ERROR);  ;}
    break;

  case 780:

    { (yyval).i = makenode(CHS,(NTYPE)(yyvsp[0]).i,0); ;}
    break;

  case 781:

    { kb_error(3826,"Bad expression after unary minus.\n",Q_ERROR); ;}
    break;

  case 782:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 783:

    { (yyval).i = makenode(NOT_,(NTYPE)(yyvsp[0]).i,0); ;}
    break;

  case 784:

    { kb_error(3827,"Bad expression after NOT.\n",Q_ERROR); ;}
    break;

  case 785:

    { cond_expr_flag++; (yyval).i = makenode(COND_TEST_,(yyvsp[-1]).i,0); ;}
    break;

  case 786:

    { cond_expr_flag--; (yyval).i = makenode(COND_EXPR_,(yyvsp[-2]).i,(yyvsp[-1]).i); ;}
    break;

  case 787:

    { (yyval).i = makenode(COND_ELSE_,(yyvsp[-1]).i,(yyvsp[0]).i); ;}
    break;

  case 788:

    { kb_error(3824,"Conditional expression syntax:  expr1 ? expr2 : expr3\n",
       Q_ERROR);
   ;}
    break;

  case 789:

    { (yyval).i = MAX_;  ;}
    break;

  case 790:

    { (yyval).i = MIN_;  ;}
    break;

  case 791:

    { (yyval).i = SUM_;  ;}
    break;

  case 792:

    { (yyval).i = AVG_;  ;}
    break;

  case 793:

    { (yyval).i = COUNT_; ;}
    break;

  case 794:

    { (yyval).i = HISTOGRAM_;  ;}
    break;

  case 795:

    { (yyval).i = LOGHISTOGRAM_;  ;}
    break;

  case 796:

    { kb_error(2402,"Syntax: HISTOGRAM(element_gen,expression)\n", Q_ERROR); ;}
    break;

  case 797:

    { kb_error(2403,"Syntax: LOGHISTOGRAM(element_gen,expression)\n", Q_ERROR); ;}
    break;

  case 798:

    { aggrtype = FOREACH_; loopdepth++; 
                 (yyval).i = makenode(AGGREGATE_INIT_,0,0); ;}
    break;

  case 799:

    {  int aggr;
                 aggrtype = FOREACH_;
                 aggr = makenode(AGGREGATE_,(yyvsp[-2]).i,(yyvsp[0]).i); 
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-3]).i,aggr);
                 end_scope();
              ;}
    break;

  case 800:

    { kb_error(2404,
   "Syntax: FOREACH element_gen [ name ] [WHERE expr] DO command\n",Q_ERROR);;}
    break;

  case 801:

    { use_given_id = 1; /* in eval() */  ;}
    break;

  case 802:

    { (yyval).i = makenode(SHOW_,(yyvsp[0]).i,0);
                use_given_id = 0;
                end_scope();
              ;}
    break;

  case 803:

    { (yyval).i = makenode(SINGLE_LETTER_,'s',0); ;}
    break;

  case 804:

    { kb_error(2405,"Syntax: SHOW element_gen [ name ] [ WHERE rexpr ]\n",Q_ERROR);;}
    break;

  case 805:

    { use_given_id = 1; /* in eval() */ ;}
    break;

  case 806:

    { (yyval).i = makenode(SHOW_EXPR_,(yyvsp[0]).i,0);
                use_given_id = 0;
                end_scope();
              ;}
    break;

  case 807:

    { kb_error(2406,"Syntax: SHOW_EXPR element_gen [ name ] [ WHERE rexpr ]\n",Q_ERROR);;}
    break;

  case 808:

    { loopdepth++;
           aggrtype = (yyvsp[0]).i; (yyval).i = makenode(AGGREGATE_INIT_,0,0); ;}
    break;

  case 809:

    {  int aggr;
                 aggrtype = (yyvsp[-6]).i;
                 aggr = makenode(AGGREGATE_,(yyvsp[-3]).i,(yyvsp[-1]).i); 
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-5]).i,aggr);
                 end_scope();
              ;}
    break;

  case 810:

    { loopdepth++;
            aggrtype = (yyvsp[0]).i; (yyval).i = makenode(AGGREGATE_INIT_,0,0); ;}
    break;

  case 811:

    {  int aggr;
                 aggrtype = (yyvsp[-6]).i;
                 aggr = makenode(AGGREGATE_,(yyvsp[-3]).i,(yyvsp[-1]).i); 
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-5]).i,aggr);
                 end_scope();
              ;}
    break;

  case 812:

    {  aggrtype = (yyvsp[0]).i; 
                 (yyval).i = makenode(AGGREGATE_INIT_,0,0);  
              ;}
    break;

  case 813:

    {  int aggr;
                 aggrtype = (yyvsp[-2]).i; 
                 aggr = makenode(AGGREGATE_,(yyvsp[0]).i,0);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-1]).i,aggr);
                 end_scope();
              ;}
    break;

  case 814:

    {  (yyval).i = makenode(SET_INIT_,0,0); ;}
    break;

  case 815:

    { (yyval).i = SET_NO_REFINE_ ; ;}
    break;

  case 816:

    { (yyval).i = SET_HIT_PARTNER_ ; ;}
    break;

  case 817:

    { (yyval).i = SET_FIXED_ ; ;}
    break;

  case 818:

    { (yyval).i = SET_BARE_ ; ;}
    break;

  case 819:

    { (yyval).i = SET_NONCONTENT_ ; ;}
    break;

  case 820:

    { (yyval).i = SET_NO_DISPLAY_ ; ;}
    break;

  case 821:

    { (yyval).i = SET_AXIAL_POINT_ ; ;}
    break;

  case 822:

    { (yyval).i = SET_TETRA_PT_;  ;}
    break;

  case 823:

    { (yyval).i = SET_TRIPLE_PT_; ;}
    break;

  case 824:

    {  int aggr;
                 aggrtype = (yyvsp[0]).i;
                 aggr = makenode(AGGREGATE_,(yyvsp[-1]).i,0);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-2]).i,aggr);
                end_scope();
              ;}
    break;

  case 825:

    { int aggr,where; 
                aggrtype = (yyvsp[-2]).i;
                where = makenode(WHERE_,(yyvsp[-3]).i,(yyvsp[0]).i);
                aggr = makenode(AGGREGATE_,where,0);
                (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-4]).i,aggr);
                end_scope();
              ;}
    break;

  case 826:

    { (yyval).i = SET_FRONTBODY_; tok=0; /* for UMINUS */ ;}
    break;

  case 827:

    { (yyval).i = SET_BACKBODY_; tok=0; ;}
    break;

  case 828:

    { (yyval).i = SET_COLOR_; tok=0; ;}
    break;

  case 829:

    { (yyval).i = SET_FRONTCOLOR_; tok=0; ;}
    break;

  case 830:

    { (yyval).i = SET_BACKCOLOR_; tok=0; ;}
    break;

  case 831:

    { (yyval).i = SET_DENSITY_; tok=0; ;}
    break;

  case 832:

    { (yyval).i = SET_ORIGINAL_; tok=0; ;}
    break;

  case 833:

    { (yyval).i = SET_VOLCONST_; tok=0; ;}
    break;

  case 834:

    { (yyval).i = SET_VOLUME_; tok=0; ;}
    break;

  case 835:

    { (yyval).i = SET_TARGET_; tok=0; ;}
    break;

  case 836:

    { (yyval).i = SET_PRESSURE_; tok=0; ;}
    break;

  case 837:

    { (yyval).i = SET_OPACITY_; tok=0; ;}
    break;

  case 838:

    { (yyval).i = SET_CONSTRAINT_; tok=0; ;}
    break;

  case 839:

    { (yyval).i = SET_BOUNDARY_; tok=0; ;}
    break;

  case 840:

    { (yyval).i = SET_TAG_; tok=0; ;}
    break;

  case 841:

    { (yyval).i = SET_COORD_+(yyvsp[0]).i; tok = 0; /* UMINUS bug */ ;}
    break;

  case 842:

    { (yyval).i = SET_PARAM_+(yyvsp[0]).i; tok=0; ;}
    break;

  case 843:

    { (yyval).i = SET_PHASE_; tok=0; ;}
    break;

  case 844:

    { (yyval).i = SET_EXTRA_ATTR_ ; tok=0; 
            (yyval).qnum = (yyvsp[0]).qnum; 
            (yyval).etype = (yyvsp[0]).etype;
           strcpy(set_extra_name,EXTRAS((yyval).etype)[(yyval).qnum].name); ;}
    break;

  case 845:

    { (yyval).i = SET_ORIENTATION_ ; tok=0; ;}
    break;

  case 846:

    { (yyval).i = SET_WRAP_ ; tok = 0;;}
    break;

  case 847:

    { (yyval).i = (yyvsp[0]).i;;}
    break;

  case 848:

    { (yyval).i = (yyvsp[0]).i;;}
    break;

  case 849:

    { (yyval).i = ASSIGN_; ;}
    break;

  case 850:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 851:

    { kb_error(3415,"Expected assignment operator, got '='\n",
                  Q_ERROR);
               ;}
    break;

  case 852:

    { (yyval).i = makenode(SINGLE_ELEMENT_,(yyvsp[-2]).i,0); ;}
    break;

  case 853:

    {  int  mm;
                 int type = list[(yyvsp[-5]).i].op1.eltype;
                 begin_scope(); /* ended at end of aggregate */
                 elsym = symbol_add(default_name,type);
                 elsym->localnum = list[(yyvsp[-5]).i].op2.localnum;
                 strcpy(last_name,default_name);
                 (yyvsp[-5]).symptr = elsym; 
                 attr_kind = (yyvsp[-3]).i;
                 (yyvsp[-2]).symptr = 0; /* in case of WHERE */
                 assigntype = (yyvsp[-1]).i;
                 mm = makenode(SET_ATTRIBUTE_A,(yyvsp[0]).i,0); 
                 (yyval).i = makenode(SINGLE_ASSIGN_,(yyvsp[-2]).i,mm);
                end_scope();
              ;}
    break;

  case 854:

    { (yyval).i = makenode(SINGLE_ELEMENT_,(yyvsp[-2]).i,0); ;}
    break;

  case 855:

    {  int mm;
                 int type = list[(yyvsp[-6]).i].op1.eltype;
                 begin_scope(); /* ended at end of aggregate */
                 elsym = symbol_add(default_name,type);
                 elsym->localnum = list[(yyvsp[-6]).i].op2.localnum;
                 strcpy(last_name,default_name);
                 (yyvsp[-6]).symptr = elsym;
                 attr_kind = (yyvsp[-4]).i;
                 subtree_swap(&(yyvsp[-2]).i,&(yyvsp[0]).i);
                 (yyvsp[-3]).symptr = 0; /* in case of WHERE */
                 assigntype = (yyvsp[-1]).i;
                 mm = makenode(SET_ATTRIBUTE_A,(yyvsp[0]).i,(yyvsp[-2]).i);
                 (yyval).i = makenode(SINGLE_ASSIGN_,(yyvsp[-3]).i,mm);
                end_scope();
              ;}
    break;

  case 856:

    {  int aggr;
                 int nn;
                 aggrtype = SET_ATTRIBUTE_LOOP_;
                 attr_kind = (yyvsp[-1]).i;
                 elsym = (yyvsp[-2]).symptr;
                 (yyvsp[-2]).symptr = 0; /* in case of WHERE */
                 nn = makenode(SET_ATTRIBUTE_L,(yyvsp[0]).i,0); 
                 aggr = makenode(AGGREGATE_,(yyvsp[-2]).i,nn);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-3]).i,aggr);
                end_scope();
              ;}
    break;

  case 857:

    {  int aggr;
                 int nn;
                 aggrtype = SET_ATTRIBUTE_LOOP_;
                 attr_kind = (yyvsp[-2]).i;
                 subtree_swap(&(yyvsp[-1]).i,&(yyvsp[0]).i); /* get index eval in top of stack */
                 elsym = (yyvsp[-3]).symptr;
                 (yyvsp[-3]).symptr = 0; /* in case of WHERE */
                 nn = makenode(SET_ATTRIBUTE_L,(yyvsp[0]).i,(yyvsp[-1]).i); 
                 aggr = makenode(AGGREGATE_,(yyvsp[-3]).i,nn);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-4]).i,aggr);
                end_scope();
              ;}
    break;

  case 858:

    {  int aggr;
                 int nn,mm,kk;
                 aggrtype = SET_ATTRIBUTE_LOOP_;
                 elsym = (yyvsp[-3]).symptr;
                 (yyvsp[-3]).symptr = 0; /* in case of WHERE */
                 kk = makenode(ATTRIB_LVALUE_,0,0);
                 list[kk].op1.localnum = 0;
                 list[kk].op2.name_id = set_name_eltype((yyvsp[-2]).qnum,(yyvsp[-2]).etype);

                 subtree_swap(&(yyvsp[0]).i,&kk); /* so datastart before rexpr */
                 subtree_swap(&(yyvsp[-1]).i,&kk); /* so datastart before indexset */
                 mm  = makenode(ARRAY_LVALUE_INDEXED_,kk,(yyvsp[-1]).i);
                 nn = makenode(ARRAY_ASSIGNOP_SINGLE_,mm,(yyvsp[0]).i);
                 list[nn].flags |= SET_ASSIGNOP;
                 list[nn].op1.assigntype = ASSIGN_;
                 list[nn].op2.name_id = set_name_eltype((yyvsp[-2]).qnum,(yyvsp[-2]).etype);
                 aggr = makenode(AGGREGATE_,(yyvsp[-3]).i,nn);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-4]).i,aggr);
                 end_scope();
              ;}
    break;

  case 859:

    {  int aggr;
                 int nn,kk;
                 aggrtype = SET_ATTRIBUTE_LOOP_;
                 attr_kind = (yyvsp[-1]).i;
                 elsym = (yyvsp[-2]).symptr;
                 (yyvsp[-2]).symptr = 0; /* in case of WHERE */
                 kk = makenode(ATTRIB_LVALUE_,0,0);
                 list[kk].op1.localnum = 0;
                 list[kk].op2.name_id = set_name_eltype((yyvsp[-1]).qnum,(yyvsp[-1]).etype);
                 subtree_swap(&(yyvsp[0]).i,&kk); /* so lvalue before rvalue */
                 nn = makenode(ARRAY_ASSIGNOP_ARRAY_,kk,(yyvsp[0]).i);
                 list[nn].flags |= SET_ASSIGNOP;
                 list[nn].op1.assigntype = ASSIGN_;
                 list[nn].op2.name_id = set_name_eltype((yyvsp[-1]).qnum,(yyvsp[-1]).etype);
                 aggr = makenode(AGGREGATE_,(yyvsp[-2]).i,nn);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-3]).i,aggr);
                 end_scope();
              ;}
    break;

  case 860:

    {  int aggr;
                 int nn,kk;
                 aggrtype = SET_ATTRIBUTE_LOOP_;
                 attr_kind = (yyvsp[-1]).i;
                 elsym = (yyvsp[-2]).symptr;
                 (yyvsp[-2]).symptr = 0; /* in case of WHERE */
                 kk = makenode(ATTRIB_LVALUE_,0,0);
                 list[kk].op1.localnum = 0;
                 list[kk].op2.name_id = set_name_eltype((yyvsp[-1]).qnum,(yyvsp[-1]).etype);
                 subtree_swap(&(yyvsp[0]).i,&kk); /* so rexpr before datastart */
                 nn = makenode(ARRAY_ASSIGNOP_SCALAR_,kk,(yyvsp[0]).i);
                 list[nn].op1.assigntype = ASSIGN_;
                 list[nn].op2.name_id = set_name_eltype((yyvsp[-1]).qnum,(yyvsp[-1]).etype);
                 list[nn].flags |= SET_ASSIGNOP;
                 aggr = makenode(AGGREGATE_,(yyvsp[-2]).i,nn);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-3]).i,aggr);
                 end_scope();
              ;}
    break;

  case 861:

    {  int aggr,where;
                 int nn,kk;
                 aggrtype = SET_ATTRIBUTE_LOOP_;
                 attr_kind = (yyvsp[-3]).i;

                 /* splice in the WHERE clause */
                 subtree_swap(&(yyvsp[-2]).i,&(yyvsp[0]).i);
                 where = makenode(WHERE_,0,0);
                 list[where].left = 0;
                 list[where].right = 0;
                 subtree_swap(&(yyvsp[-2]).i,&where); /* get in proper linear order */
                 list[where].left = (yyvsp[-4]).i - where;
                 list[where].right = (yyvsp[0]).i - where;


                 elsym = (yyvsp[-4]).symptr;
                 (yyvsp[-4]).symptr = 0; /* in case of WHERE */
                 kk = makenode(ATTRIB_LVALUE_,0,0);
                 list[kk].op1.localnum = 0;
                 list[kk].op2.name_id = set_name_eltype((yyvsp[-3]).qnum,(yyvsp[-3]).etype);
                 subtree_swap(&(yyvsp[-2]).i,&kk); /* so rexpr before datastart */
                 nn = makenode(ARRAY_ASSIGNOP_SCALAR_,kk,(yyvsp[-2]).i);
                 list[nn].flags |= SET_ASSIGNOP;
                 list[nn].op2.name_id = set_name_eltype((yyvsp[-3]).qnum,(yyvsp[-3]).etype);
                 list[nn].op1.assigntype = ASSIGN_;
                 aggr = makenode(AGGREGATE_,(yyvsp[-4]).i,nn);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-5]).i,aggr);
                 end_scope();
              ;}
    break;

  case 862:

    {  int aggr,where;
                 int nn,mm,kk;
                 aggrtype = SET_ATTRIBUTE_LOOP_;

                 /* splice in the WHERE clause */
                 subtree_swap(&(yyvsp[-2]).i,&(yyvsp[0]).i);
                 subtree_swap(&(yyvsp[-3]).i,&(yyvsp[0]).i);
                 where = makenode(WHERE_,0,0);
                 list[where].left = 0;
                 list[where].right = 0;
                 subtree_swap(&(yyvsp[-2]).i,&where); /* get in proper linear order */
                 subtree_swap(&(yyvsp[-3]).i,&where); /* get in proper linear order */
                 list[where].left = (yyvsp[-5]).i - where;
                 list[where].right = (yyvsp[0]).i - where;

                 elsym = (yyvsp[-5]).symptr;
                 (yyvsp[-5]).symptr = 0; /* in case of WHERE */
                 kk = makenode(ATTRIB_LVALUE_,0,0);
                 list[kk].op1.localnum = 0;
                 list[kk].op2.name_id = set_name_eltype((yyvsp[-4]).qnum,(yyvsp[-4]).etype);

                 subtree_swap(&(yyvsp[-2]).i,&kk); /* so datastart before rexpr */
                 subtree_swap(&(yyvsp[-3]).i,&kk); /* so datastart before indexset */
                 mm  = makenode(ARRAY_LVALUE_INDEXED_,kk,(yyvsp[-3]).i);
                 nn = makenode(ARRAY_ASSIGNOP_SINGLE_,mm,(yyvsp[-2]).i);
                 list[nn].flags |= SET_ASSIGNOP;
                 list[nn].op1.assigntype = ASSIGN_;
                 aggr = makenode(AGGREGATE_,(yyvsp[-5]).i,nn);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-6]).i,aggr);
                 end_scope();
              ;}
    break;

  case 863:

    {  int aggr,where;
                 int nn,kk;
                 aggrtype = SET_ATTRIBUTE_LOOP_;
                 attr_kind = (yyvsp[-3]).i;

                 /* splice in the WHERE clause */
                 subtree_swap(&(yyvsp[-2]).i,&(yyvsp[0]).i);
                 where = makenode(WHERE_,0,0);
                 list[where].left = 0;
                 list[where].right = 0;
                 subtree_swap(&(yyvsp[-2]).i,&where); /* get in proper linear order */
                 list[where].left = (yyvsp[-4]).i - where;
                 list[where].right = (yyvsp[0]).i - where;

                 elsym = (yyvsp[-4]).symptr;
                 (yyvsp[-4]).symptr = 0; /* in case of WHERE */
                 kk = makenode(ATTRIB_LVALUE_,0,0);
                 list[kk].op1.localnum = 0;
                 list[kk].op2.name_id = set_name_eltype((yyvsp[-3]).qnum,(yyvsp[-3]).etype);
                 subtree_swap(&(yyvsp[-2]).i,&kk); /* so lvalue before rvalue */
                 nn = makenode(ARRAY_ASSIGNOP_ARRAY_,kk,(yyvsp[-2]).i);
                 list[nn].flags |= SET_ASSIGNOP;
                 list[nn].op1.assigntype = ASSIGN_;
                 aggr = makenode(AGGREGATE_,(yyvsp[-4]).i,nn);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-5]).i,aggr);
                 end_scope();
              ;}
    break;

  case 864:

    {  int aggr,where,nn; 
                 aggrtype = SET_ATTRIBUTE_LOOP_;
                 attr_kind = (yyvsp[-3]).i;

                 subtree_swap(&(yyvsp[-2]).i,&(yyvsp[0]).i);
                 where = makenode(WHERE_,0,0);
                 list[where].left = 0;
                 list[where].right = 0;
                 subtree_swap(&(yyvsp[-2]).i,&where); /* get in proper linear order */
                 list[where].left = (yyvsp[-4]).i - where;
                 list[where].right = (yyvsp[0]).i - where;

                 elsym = (yyvsp[-4]).symptr;
                 (yyvsp[-4]).symptr = 0; /* in case of WHERE */
                 nn = makenode(SET_ATTRIBUTE_L,(yyvsp[-2]).i,0); 
                 aggr = makenode(AGGREGATE_,where,nn);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-5]).i,aggr);
                end_scope();
              ;}
    break;

  case 865:

    {         int aggr,where,nn; 
                 aggrtype = SET_ATTRIBUTE_LOOP_;
                 attr_kind = (yyvsp[-4]).i;
                 subtree_swap(&(yyvsp[-3]).i,&(yyvsp[-2]).i); /* get index eval in top of stack */
                 subtree_swap(&(yyvsp[-3]).i,&(yyvsp[0]).i); /* get index eval in top of stack */
                 subtree_swap(&(yyvsp[-2]).i,&(yyvsp[0]).i);
                 where = makenode(WHERE_,0,0);
                 list[where].left = 0;
                 list[where].right = 0;
                 subtree_swap(&(yyvsp[-3]).i,&where); /* get in proper linear order */
                 subtree_swap(&(yyvsp[-2]).i,&where); /* get in proper linear order */
                 list[where].left = (yyvsp[-5]).i - where;
                 list[where].right = (yyvsp[0]).i - where;
                 elsym = (yyvsp[-5]).symptr;
                 (yyvsp[-5]).symptr = 0; /* in case of WHERE */
                 nn = makenode(SET_ATTRIBUTE_L,(yyvsp[-2]).i,(yyvsp[-3]).i); 
                 aggr = makenode(AGGREGATE_,where,nn);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-6]).i,aggr);
                end_scope();
              ;}
    break;

  case 866:

    {         int aggr,idnode; 
                 aggrtype = SET_NAMED_QUANTITY_;
                 idnode = makenode(PUSH_NAMED_QUANTITY,(yyvsp[0]).i,0);  
                 elsym = (yyvsp[-2]).symptr;
                 (yyvsp[-2]).symptr = 0; /* in case of WHERE */
                 aggr = makenode(AGGREGATE_,(yyvsp[-2]).i,idnode);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-3]).i,aggr);
                end_scope();
              ;}
    break;

  case 867:

    {         int aggr,idnode; 
                 aggrtype = UNSET_NAMED_QUANTITY_;
                 idnode = makenode(PUSH_NAMED_QUANTITY,(yyvsp[0]).i,0);  
                 elsym = (yyvsp[-2]).symptr;
                 (yyvsp[-2]).symptr = 0; /* in case of WHERE */
                 aggr = makenode(AGGREGATE_,(yyvsp[-2]).i,idnode);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-3]).i,aggr);
                end_scope();
              ;}
    break;

  case 868:

    {         int aggr,where,idnode; 
                 aggrtype = SET_NAMED_QUANTITY_;
                 idnode = makenode(PUSH_NAMED_QUANTITY,(yyvsp[-2]).i,0);  
                 where = makenode(WHERE_,0,0);
                 list[where].left = 0;
                 list[where].right = 0;
                 subtree_swap(&idnode,&where); /* get in proper linear order */
                 list[where].left = (yyvsp[-4]).i - where;
                 list[where].right = (yyvsp[0]).i - where;
                 elsym = (yyvsp[-4]).symptr;
                 (yyvsp[-4]).symptr = 0; /* in case of WHERE */
                 aggr = makenode(AGGREGATE_,where,idnode);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-5]).i,aggr);
                end_scope();
              ;}
    break;

  case 869:

    {         int aggr,where,idnode; 
                 aggrtype = UNSET_NAMED_QUANTITY_;
                 idnode = makenode(PUSH_NAMED_QUANTITY,(yyvsp[-2]).i,0);  
                 where = makenode(WHERE_,0,0);
                 list[where].left = 0;
                 list[where].right = 0;
                 subtree_swap(&idnode,&where); /* get in proper linear order */
                 list[where].left = (yyvsp[-4]).i - where;
                 list[where].right = (yyvsp[0]).i - where;
                 elsym = (yyvsp[-4]).symptr;
                 (yyvsp[-4]).symptr = 0; /* in case of WHERE */
                 aggr = makenode(AGGREGATE_,where,idnode);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-5]).i,aggr);
                end_scope();
              ;}
    break;

  case 870:

    {         int aggr,idnode; 
                 aggrtype = SET_METHOD_INSTANCE_;
                 idnode = makenode(PUSH_METHOD_INSTANCE_,(yyvsp[0]).i,0);  
                 elsym = (yyvsp[-2]).symptr;
                 (yyvsp[-2]).symptr = 0; /* in case of WHERE */
                 aggr = makenode(AGGREGATE_,(yyvsp[-2]).i,idnode);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-3]).i,aggr);
                end_scope();
              ;}
    break;

  case 871:

    {         int aggr,idnode; 
                 aggrtype = UNSET_METHOD_INSTANCE_;
                 idnode = makenode(PUSH_METHOD_INSTANCE_, (yyvsp[0]).i,0);  
                 elsym = (yyvsp[-2]).symptr;
                 (yyvsp[-2]).symptr = 0; /* in case of WHERE */
                 aggr = makenode(AGGREGATE_,(yyvsp[-2]).i,idnode);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-3]).i,aggr);
                end_scope();
              ;}
    break;

  case 872:

    {         int aggr,where,idnode; 
                 aggrtype = SET_METHOD_INSTANCE_;
                 idnode = makenode(PUSH_METHOD_INSTANCE_,(yyvsp[-2]).i,0);  
                 where = makenode(WHERE_,0,0);
                 list[where].left = 0;
                 list[where].right = 0;
                 subtree_swap(&idnode,&where); /* get in proper linear order */
                 list[where].left = (yyvsp[-4]).i - where;
                 list[where].right = (yyvsp[0]).i - where;
                 elsym = (yyvsp[-4]).symptr;
                 (yyvsp[-4]).symptr = 0; /* in case of WHERE */
                 aggr = makenode(AGGREGATE_,where,idnode);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-5]).i,aggr);
                end_scope();
              ;}
    break;

  case 873:

    {         int aggr,where,idnode; 
                 aggrtype = UNSET_METHOD_INSTANCE_;
                 idnode = makenode(PUSH_METHOD_INSTANCE_,(yyvsp[-2]).i,0);  
                 where = makenode(WHERE_,0,0);
                 list[where].left = 0;
                 list[where].right = 0;
                 subtree_swap(&idnode,&where); /* get in proper linear order */
                 list[where].left = (yyvsp[-4]).i - where;
                 list[where].right = (yyvsp[0]).i - where;
                 elsym = (yyvsp[-4]).symptr;
                 (yyvsp[-4]).symptr = 0; /* in case of WHERE */
                 aggr = makenode(AGGREGATE_,where,idnode);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-5]).i,aggr);
                end_scope();
              ;}
    break;

  case 874:

    {         int aggr,idnode; 
                 aggrtype = SET_NAMED_QUANTITY_;
                 idnode = makenode(PUSH_NAMED_QUANTITY,(yyvsp[0]).i,0);  
                 elsym = (yyvsp[-1]).symptr;
                 (yyvsp[-1]).symptr = 0; /* in case of WHERE */
                 aggr = makenode(AGGREGATE_,(yyvsp[-1]).i,idnode);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-2]).i,aggr);
                end_scope();
              ;}
    break;

  case 875:

    {         int aggr,idnode; 
                 aggrtype = UNSET_NAMED_QUANTITY_;
                 idnode = makenode(PUSH_NAMED_QUANTITY,(yyvsp[0]).i,0);  
                 elsym = (yyvsp[-1]).symptr;
                 (yyvsp[-1]).symptr = 0; /* in case of WHERE */
                 aggr = makenode(AGGREGATE_,(yyvsp[-1]).i,idnode);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-2]).i,aggr);
                end_scope();
              ;}
    break;

  case 876:

    {         int aggr,where,idnode; 
                 aggrtype = SET_NAMED_QUANTITY_;
                 idnode = makenode(PUSH_NAMED_QUANTITY,(yyvsp[-2]).i,0);  
                 where = makenode(WHERE_,0,0);
                 list[where].left = 0;
                 list[where].right = 0;
                 subtree_swap(&idnode,&where); /* get in proper linear order */
                 list[where].left = (yyvsp[-3]).i - where;
                 list[where].right = (yyvsp[0]).i - where;
                 elsym = (yyvsp[-3]).symptr;
                 (yyvsp[-3]).symptr = 0; /* in case of WHERE */
                 aggr = makenode(AGGREGATE_,where,idnode);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-4]).i,aggr);
                end_scope();
              ;}
    break;

  case 877:

    {         int aggr,where,idnode; 
                 aggrtype = UNSET_NAMED_QUANTITY_;
                 idnode = makenode(PUSH_NAMED_QUANTITY,(yyvsp[-2]).i,0);  
                 where = makenode(WHERE_,0,0);
                 list[where].left = 0;
                 list[where].right = 0;
                 subtree_swap(&idnode,&where); /* get in proper linear order */
                 list[where].left = (yyvsp[-3]).i - where;
                 list[where].right = (yyvsp[0]).i - where;
                 elsym = (yyvsp[-3]).symptr;
                 (yyvsp[-3]).symptr = 0; /* in case of WHERE */
                 aggr = makenode(AGGREGATE_,where,idnode);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-4]).i,aggr);
                end_scope();
              ;}
    break;

  case 878:

    {         int aggr,idnode; 
                 aggrtype = SET_METHOD_INSTANCE_;
                 idnode = makenode(PUSH_METHOD_INSTANCE_,(yyvsp[0]).i,0);  
                 elsym = (yyvsp[-1]).symptr;
                 (yyvsp[-1]).symptr = 0; /* in case of WHERE */
                 aggr = makenode(AGGREGATE_,(yyvsp[-1]).i,idnode);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-2]).i,aggr);
                end_scope();
              ;}
    break;

  case 879:

    {         int aggr,idnode; 
                 aggrtype = UNSET_METHOD_INSTANCE_;
                 idnode = makenode(PUSH_METHOD_INSTANCE_,(yyvsp[0]).i,0);  
                 elsym = (yyvsp[-1]).symptr;
                 (yyvsp[-1]).symptr = 0; /* in case of WHERE */
                 aggr = makenode(AGGREGATE_,(yyvsp[-1]).i,idnode);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-2]).i,aggr);
                end_scope();
              ;}
    break;

  case 880:

    {         int aggr,where,idnode; 
                 aggrtype = SET_METHOD_INSTANCE_;
                 idnode = makenode(PUSH_METHOD_INSTANCE_,(yyvsp[-2]).i,0);  
                 where = makenode(WHERE_,0,0);
                 list[where].left = 0;
                 list[where].right = 0;
                 subtree_swap(&idnode,&where); /* get in proper linear order */
                 list[where].left = (yyvsp[-3]).i - where;
                 list[where].right = (yyvsp[0]).i - where;
                 elsym = (yyvsp[-3]).symptr;
                 (yyvsp[-3]).symptr = 0; /* in case of WHERE */
                 aggr = makenode(AGGREGATE_,where,idnode);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-4]).i,aggr);
                end_scope();
              ;}
    break;

  case 881:

    {         int aggr,where,idnode; 
                 aggrtype = UNSET_METHOD_INSTANCE_;
                 idnode = makenode(PUSH_METHOD_INSTANCE_,(yyvsp[-2]).i,0);  
                 where = makenode(WHERE_,0,0);
                 list[where].left = 0;
                 list[where].right = 0;
                 subtree_swap(&idnode,&where); /* get in proper linear order */
                 list[where].left = (yyvsp[-3]).i - where;
                 list[where].right = (yyvsp[0]).i - where;
                 elsym = (yyvsp[-3]).symptr;
                 (yyvsp[-3]).symptr = 0; /* in case of WHERE */
                 aggr = makenode(AGGREGATE_,where,idnode);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-4]).i,aggr);
                end_scope();
              ;}
    break;

  case 882:

    {
     if ( tok == '-' )
       kb_error(1897,"Syntax kludge: cannot have leading minus sign after ]. Use parentheses.\n",
             Q_ERROR);
    else
 kb_error(2532,
   "Syntax: SET element_gen [ name ] attribute rexpr [ WHERE rexpr ]\n",Q_ERROR);
  ;}
    break;

  case 883:

    {  (yyval).i = makenode(SET_INIT_,0,0); ;}
    break;

  case 884:

    { (yyval).i = UNSET_FIXED_; ;}
    break;

  case 885:

    { (yyval).i = UNSET_HIT_PARTNER_; ;}
    break;

  case 886:

    { (yyval).i = UNSET_BARE_; ;}
    break;

  case 887:

    { (yyval).i = UNSET_NO_REFINE_; ;}
    break;

  case 888:

    { (yyval).i = UNSET_NONCONTENT_; ;}
    break;

  case 889:

    { (yyval).i = UNSET_NO_DISPLAY_; ;}
    break;

  case 890:

    { (yyval).i = UNSET_DENSITY_; ;}
    break;

  case 891:

    { (yyval).i = UNSET_TARGET_; ;}
    break;

  case 892:

    { (yyval).i = UNSET_TARGET_; ;}
    break;

  case 893:

    { (yyval).i = UNSET_PRESSURE_; ;}
    break;

  case 894:

    { (yyval).i = UNSET_TRIPLE_PT_; ;}
    break;

  case 895:

    { (yyval).i = UNSET_TETRA_PT_; ;}
    break;

  case 896:

    { (yyval).i = UNSET_AXIAL_POINT_; ;}
    break;

  case 897:

    { (yyval).i = UNSET_FRONTBODY_; ;}
    break;

  case 898:

    { (yyval).i = UNSET_BACKBODY_; ;}
    break;

  case 899:

    { (yyval).i = UNSET_FACET_BODY_; ;}
    break;

  case 900:

    {  int aggr;
                 aggrtype = (yyvsp[0]).i;
                 aggr = makenode(AGGREGATE_,(yyvsp[-1]).i,0);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-2]).i,aggr);
                end_scope();
              ;}
    break;

  case 901:

    {         int aggr,where; 
                 aggrtype = (yyvsp[-2]).i;
                 elsym = (yyvsp[-3]).symptr;
                 where = makenode(WHERE_,(yyvsp[-3]).i,(yyvsp[0]).i);
                 aggr = makenode(AGGREGATE_,where,0);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-4]).i,aggr);
                end_scope();
              ;}
    break;

  case 902:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 903:

    {  int aggr;
                 aggrtype = UNSET_CONSTRAINT_NAME;
                 elsym = (yyvsp[-1]).symptr;
                 aggr = makenode(AGGREGATE_,(yyvsp[-1]).i,0);
                 list[aggr].stack_delta = 0;
                 list[aggr].op3.connum = globals((yyvsp[0]).i)->value.cnum;
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-2]).i,aggr);
                end_scope();
              ;}
    break;

  case 904:

    { real_val = globals((yyvsp[0]).i)->value.cnum;
                           (yyval).i = makenode(PUSHCONST,0,0); ;}
    break;

  case 905:

    {  int aggr;
                 aggrtype = UNSET_CONSTRAINT_;
                 elsym = (yyvsp[-2]).symptr;
                 aggr = makenode(AGGREGATE_,(yyvsp[-2]).i,(yyvsp[0]).i);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-3]).i,aggr);
                end_scope();
              ;}
    break;

  case 906:

    {         int aggr,where; 
                 aggrtype = UNSET_CONSTRAINT_;
                 subtree_swap(&(yyvsp[-2]).i,&(yyvsp[0]).i);
                 where = makenode(WHERE_,0,0);
                 list[where].left = 0;
                 list[where].right = 0;
                 subtree_swap(&(yyvsp[-2]).i,&where); /* get in proper linear order */
                 list[where].left = (yyvsp[-4]).i - where;
                 list[where].right = (yyvsp[0]).i - where;
                 elsym = (yyvsp[-4]).symptr;
                 aggr = makenode(AGGREGATE_,where,(yyvsp[-2]).i);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-5]).i,aggr);
                end_scope();
              ;}
    break;

  case 907:

    {         int aggr,where; 
                 aggrtype = UNSET_CONSTRAINT_NAME;
                 where = makenode(WHERE_,0,0);
                 list[where].left = 0;
                 list[where].right = 0;
                 list[where].left = (yyvsp[-3]).i - where;
                 list[where].right = (yyvsp[0]).i - where;
                 elsym = (yyvsp[-3]).symptr;
                 aggr = makenode(AGGREGATE_,where,0);
                 list[aggr].op3.connum = globals((yyvsp[-2]).i)->value.cnum;
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-4]).i,aggr);
                end_scope();
              ;}
    break;

  case 908:

    { (yyval).i = (yyvsp[0]).i; ;}
    break;

  case 909:

    {  int aggr;
                 aggrtype = UNSET_BOUNDARY_NAME;
                 elsym = (yyvsp[-1]).symptr;
                 aggr = makenode(AGGREGATE_,(yyvsp[-1]).i,0);
                 list[aggr].stack_delta = 0;
                 list[aggr].op3.bdrynum = globals((yyvsp[0]).i)->value.bnum;
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-2]).i,aggr);
                end_scope();
              ;}
    break;

  case 910:

    { real_val = globals((yyvsp[0]).i)->value.bnum;
                           (yyval).i = makenode(PUSHCONST,0,0); ;}
    break;

  case 911:

    {  int aggr;
                 aggrtype = UNSET_BOUNDARY_;
                 elsym = (yyvsp[-2]).symptr;
                 aggr = makenode(AGGREGATE_,(yyvsp[-2]).i,(yyvsp[0]).i);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-3]).i,aggr);
                end_scope();
              ;}
    break;

  case 912:

    {         int aggr,where; 
                 aggrtype = UNSET_BOUNDARY_NAME;
                 where = makenode(WHERE_,0,0);
                 list[where].left = 0;
                 list[where].right = 0;
                 list[where].left = (yyvsp[-3]).i - where;
                 list[where].right = (yyvsp[0]).i - where;
                 elsym = (yyvsp[-3]).symptr;
                 aggr = makenode(AGGREGATE_,where,0);
                 list[aggr].op3.bdrynum = globals((yyvsp[-2]).i)->value.bnum;
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-4]).i,aggr);
                end_scope();
              ;}
    break;

  case 913:

    {         int aggr,where; 
                 aggrtype = UNSET_BOUNDARY_;
                 subtree_swap(&(yyvsp[-2]).i,&(yyvsp[0]).i);
                 where = makenode(WHERE_,0,0);
                 list[where].left = 0;
                 list[where].right = 0;
                 subtree_swap(&(yyvsp[-2]).i,&where); /* get in proper linear order */
                 list[where].left = (yyvsp[-4]).i - where;
                 list[where].right = (yyvsp[0]).i - where;
                 elsym = (yyvsp[-4]).symptr;
                 aggr = makenode(AGGREGATE_,where,(yyvsp[-2]).i);
                 (yyval).i = makenode(AGGREGATE_END_,(yyvsp[-5]).i,aggr);
                end_scope();
              ;}
    break;

  case 914:

    { kb_error(1898, "Syntax: UNSET element_gen attribute [ WHERE rexpr ]\n",Q_ERROR); ;}
    break;

  case 915:

    { YYABORT; /* no expression */ ;}
    break;

  case 916:

    { (yyval).i = makenode(HELP_KEYWORD,0,0); help_flag = 0; tok = 0; yyerrok; yyclearin ; ;}
    break;

  case 917:

    { (yyval).i = makenode(HELP_KEYWORD,0,0); help_flag = 0; tok = 0; yyerrok; yyclearin ; ;}
    break;


      default: break;
    }

/* Line 1126 of yacc.c.  */


  yyvsp -= yylen;
  yyssp -= yylen;


  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (YYPACT_NINF < yyn && yyn < YYLAST)
	{
	  int yytype = YYTRANSLATE (yychar);
	  YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
	  YYSIZE_T yysize = yysize0;
	  YYSIZE_T yysize1;
	  int yysize_overflow = 0;
	  char *yymsg = 0;
#	  define YYERROR_VERBOSE_ARGS_MAXIMUM 5
	  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
	  int yyx;

#if 0
	  /* This is so xgettext sees the translatable formats that are
	     constructed on the fly.  */
	  YY_("syntax error, unexpected %s");
	  YY_("syntax error, unexpected %s, expecting %s");
	  YY_("syntax error, unexpected %s, expecting %s or %s");
	  YY_("syntax error, unexpected %s, expecting %s or %s or %s");
	  YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
#endif
	  char *yyfmt;
	  char const *yyf;
	  static char const yyunexpected[] = "syntax error, unexpected %s";
	  static char const yyexpecting[] = ", expecting %s";
	  static char const yyor[] = " or %s";
	  char yyformat[sizeof yyunexpected
			+ sizeof yyexpecting - 1
			+ ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
			   * (sizeof yyor - 1))];
	  char const *yyprefix = yyexpecting;

	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  int yyxbegin = yyn < 0 ? -yyn : 0;

	  /* Stay within bounds of both yycheck and yytname.  */
	  int yychecklim = YYLAST - yyn;
	  int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
	  int yycount = 1;

	  yyarg[0] = yytname[yytype];
	  yyfmt = yystpcpy (yyformat, yyunexpected);

	  for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	      {
		if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
		  {
		    yycount = 1;
		    yysize = yysize0;
		    yyformat[sizeof yyunexpected - 1] = '\0';
		    break;
		  }
		yyarg[yycount++] = yytname[yyx];
		yysize1 = yysize + yytnamerr (0, yytname[yyx]);
		yysize_overflow |= yysize1 < yysize;
		yysize = yysize1;
		yyfmt = yystpcpy (yyfmt, yyprefix);
		yyprefix = yyor;
	      }

	  yyf = YY_(yyformat);
	  yysize1 = yysize + yystrlen (yyf);
	  yysize_overflow |= yysize1 < yysize;
	  yysize = yysize1;

	  if (!yysize_overflow && yysize <= YYSTACK_ALLOC_MAXIMUM)
	    yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg)
	    {
	      /* Avoid sprintf, as that infringes on the user's name space.
		 Don't have undefined behavior even if the translation
		 produced a string with the wrong number of "%s"s.  */
	      char *yyp = yymsg;
	      int yyi = 0;
	      while ((*yyp = *yyf))
		{
		  if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		    {
		      yyp += yytnamerr (yyp, yyarg[yyi++]);
		      yyf += 2;
		    }
		  else
		    {
		      yyp++;
		      yyf++;
		    }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    {
	      yyerror (YY_("syntax error"));
	      goto yyexhaustedlab;
	    }
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror (YY_("syntax error"));
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
        {
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
        }
      else
	{
	  yydestruct ("Error: discarding", yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (0)
     goto yyerrorlab;

yyvsp -= yylen;
  yyssp -= yylen;
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping", yystos[yystate], yyvsp);
      YYPOPSTACK;
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token. */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK;
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}





int yybegin()
{ int retval;

  PROF_START(yyparse);
  parse_errors = 0;
  perm_flag = 0;
  cond_expr_flag = 0;
  use_given_id = 0;
  parens = brace_depth = in_quote = 0;
  yylex_init();
  reset_inputbuffer();
  /* unput command start token */
  tok = COMMAND_START_; unput_tok();
  retval = yyparse();
  PROF_FINISH(yyparse);
  return retval;
}

int yyerror(s)
char *s;
{ char modmsg[1000];
  if ( help_flag ) return 0;
  parens = brace_depth = in_quote = 0;
  strncpy(modmsg,s,998);
  if ( modmsg[strlen(modmsg)-1] != '\n' )
    strcat(modmsg,"\n");
  if ( datafile_flag )
   {
     if ( listtop == 2 ) return 0;  /* no expression */
     kb_error(2407,modmsg,PARSE_ERROR);
   }
  else
    { /* fprintf(stderr,"tok = %d\n",tok); */
      kb_error(2408,modmsg,SYNTAX_ERROR);
    }   
  return 0;
}



/* to be appended to end of ytab.c so it knows about yytoks */

/******************************************************************
*
*  function: tokname()
*
*  purpose: find name of given token number. Uses yytoks[]
*              list used by debugging.
*
*/


char *tokname(toknum)
int toknum;
{ 
  char *name;

#ifndef NO_YACC_DEBUG
#ifdef YYBISON
  name = yytname[YYTRANSLATE(toknum)];

  return name;
#else
  int yy_i;

  for ( yy_i = 0; yytoks[yy_i].t_val >= 0; yy_i++ )
  if ( yytoks[yy_i].t_val == toknum )
      return yytoks[yy_i].t_name; 
  /* unfound */
  name = "                         ";
  sprintf(name,"%4d (unnamed)",toknum);
  return name;
#endif
#else
  /* unfound */
  name = "                         ";
  sprintf(name,"%4d (unnamed)",toknum);
  return name;
#endif

}

