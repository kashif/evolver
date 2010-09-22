/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 * 
*************************************************************/

/* for making FORTRAN local variables stack instead of static */
#define STATIC

/* sdrv.f -- translated by f2c (version of 8 February 1991  13:22:30).  */
/* Part of YSMP sparse matrix package */

#include "include.h"
#include "f2c.h" 

int sdrvmd_ ARGS(( integer *, integer *,integer *, integer *,integer *,REAL *,
          REAL *,REAL *, integer *,integer *,REAL *,integer *,
          integer *, integer *, REAL *));

 int ysmp_negvector ARGS((integer*, integer*, doublereal*,
 integer*, integer*, integer*, doublereal*, doublereal*, doublereal*, 
 doublereal*));

/* *********************************************************************** */
/* *********************************************************************** */
/*      Modified Routines  of the Yale Sparse Marix Package (YSMP): */

/*            SDRVMD    - a modified form of the SDRV driver. */
/*            SNFMOD    - a modified form of the SNF routine. */

/*                    copyright (c) 1990 by Tamar Schlick */

/* *********************************************************************** */
/*          Yale's SDRV solves a linear system for (symmetric) positive- */
/*    definite matrices. It calls the following routines: */
/*                      SSF (for symbolic factorization) */
/*                      SNF (for numerical factorization) and */
/*                      SNS (for numerical solution). */
/*          Our goal is to solve large sparse symmetric linear systems for */
/*    matrices that are not necessarily pos-def. Thus, we */
/*    replace SNF by SNFMOD so that a modified-Cholesky (MCF), rather than */
/*    a Cholesky, factorization is performed. In SDRV, we replace */
/*    the statememt "CALL SNF" by "CALL SNFMOD". */
/*          In Yale's SDRV, the diagnostic parameter FLAG is set to zero in */
/*    the non pos-def case (and control is returned to the main program). */
/*    Here, instead, we set FLAG in SNFMOD to a negative integer if the */
/*    matrix is not sufficiently pos-def. Specifically, FLAG is set to */
/*    minus the position of the diagonal element in the original matrix */
/*    whose modification was of the largest magnitude. Recall that in MCF */
/*    we produce matrices E,D, and U so that  M + E = UT-D-U  where E */
/*    and D are diagonal and U is unit upper-triangular. FLAG records */
/*    the index k for the largest modification in E: */
/*                    E(P(k)) = max over i {E(i)}. */

/*    All modifications to the original YSMP code are indicated. */
/*                                                                                     1/15/81 */
/* *********************************************************************** */
/*  SDRV -- DRIVER FOR SPARSE SYMMETRIC POSITIVE DEFINITE MATRIX ROUTINES */
/* *********************************************************************** */
/* ====================  change #1  (replacement) =====================1 */
/* WAS:  SUBROUTINE SDRV */
/* ===================================================================== */
/* Subroutine */ int sdrvmd_(n, p, ip, ia, ja, a, b, z, nsp, isp, rsp, esp, 
          path, flag_, emax)
integer *n, *p, *ip, *ia, *ja;
doublereal *a, *b, *z;
integer *nsp, *isp;
doublereal *rsp;
integer *esp, *path, *flag_;
doublereal *emax;
{
     /* Initialized data */

     STATIC integer ratio = 2;

     STATIC integer mark, umax, d, q, u, jumax, il, jl, iu, ju;
     STATIC integer iju;
     STATIC integer tmp;

/* ==================================================================end 
*/

/*  DESCRIPTION */

/* ====================  change #2  (replacement) =====================2 
*/
/* WAS: SDRV SOLVES SPARSE SYMMETRIC POSITIVE DEFINITE SYSTEMS OF LINEAR 
*/
/* ===================================================================== 
*/
/*     SDRVMD SOLVES SPARSE SYMMETRIC SYSTEMS OF LINEAR */
/* ==================================================================end 
*/
/*     EQUATIONS.  THE SOLUTION PROCESS IS DIVIDED INTO THREE STAGES -- */

/*        SSF - THE COEFFICIENT MATRIX M IS FACTORED SYMBOLICALLY TO */
/*                DETERMINE WHERE FILLIN WILL OCCUR DURING THE NUMERIC */
/*                FACTORIZATION. */

/* ====================  change #3  (replacement) =====================3 
*/
/* WAS: SNF - M IS FACTORED NUMERICALLY INTO THE PRODUCT UT-D-U, WHERE */
/* ===================================================================== 
*/
/*        SNFMOD - M+E IS FACTORED NUMERICALLY BY THE GILL/MURRAY/WRIGHT */
/*                MODIFIED CHOLESKY FACTORIZATION: M + E = UT-D-U, WHERE */
/*                E IS DIAGONAL, */
/* ==================================================================end 
*/
/*                D IS DIAGONAL AND U IS UNIT UPPER TRIANGULAR. */

/*        SNS - THE LINEAR SYSTEM  MX = B  IS SOLVED USING THE UT-D-U */
/*                FACTORIZATION FROM SNF. */

/*     FOR SEVERAL SYSTEMS WITH THE SAME COEFFICIENT MATRIX, SSF AND SNF */

/*     NEED BE DONE ONLY ONCE (FOR THE FIRST SYSTEM);  THEN SNS IS DONE */
/*     ONCE FOR EACH ADDITIONAL RIGHT-HAND SIDE.  FOR SEVERAL SYSTEMS */
/*     WHOSE COEFFICIENT MATRICES HAVE THE SAME NONZERO STRUCTURE, SSF */
/*     NEED BE DONE ONLY ONCE (FOR THE FIRST SYSTEM);  THEN SNF AND SNS */
/*     ARE DONE ONCE FOR EACH ADDITIONAL SYSTEM. */


/*  STORAGE OF SPARSE MATRICES */

/*     THE NONZERO ENTRIES OF THE MATRIX M ARE STORED ROW-BY-ROW IN THE */
/*     ARRAY A.  TO IDENTIFY THE INDIVIDUAL NONZERO ENTRIES IN EACH ROW, */

/*     WE NEED TO KNOW IN WHICH COLUMN EACH ENTRY LIES.  THESE COLUMN */
/*     INDICES ARE STORED IN THE ARRAY JA;  I.E., IF  A(K) = M(I,J),  THEN 
*/
/*     JA(K) = J.  TO IDENTIFY THE INDIVIDUAL ROWS, WE NEED TO KNOW WHERE 
*/
/*     EACH ROW STARTS.  THESE ROW POINTERS ARE STORED IN THE ARRAY IA; */
/*     I.E., IF M(I,J) IS THE FIRST NONZERO ENTRY (STORED) IN THE I-TH ROW 
*/
/*     AND  A(K) = M(I,J),  THEN  IA(I) = K.  MOREOVER, IA(N+1) POINTS TO 
*/
/*     THE FIRST LOCATION FOLLOWING THE LAST ELEMENT IN THE LAST ROW. */
/*     THUS, THE NUMBER OF ENTRIES IN THE I-TH ROW IS  IA(I+1) - IA(I), */
/*     THE NONZERO ENTRIES IN THE I-TH ROW ARE STORED CONSECUTIVELY IN */

/*                A(IA(I)),  A(IA(I)+1),  ..., A(IA(I+1)-1), */

/*     AND THE CORRESPONDING COLUMN INDICES ARE STORED CONSECUTIVELY IN */

/*                JA(IA(I)), JA(IA(I)+1), ..., JA(IA(I+1)-1). */

/*     SINCE THE COEFFICIENT MATRIX IS SYMMETRIC, ONLY THE NONZERO ENTRIES 
*/
/*     IN THE UPPER TRIANGLE NEED BE STORED, FOR EXAMPLE, THE MATRIX */

/*                 ( 1  0  2  3  0 ) */
/*                 ( 0  4  0  0  0 ) */
/*            M = ( 2  0  5  6  0 ) */
/*                 ( 3  0  6  7  8 ) */
/*                 ( 0  0  0  8  9 ) */

/*     COULD BE STORED AS */

/*                \ 1  2  3  4  5  6  7  8  9 10 11 12 13 */
/*            ---+-------------------------------------- */
/*            IA \ 1  4  5  8 12 14 */
/*            JA \ 1  3  4  2  1  3  4  1  3  4  5  4  5 */
/*             A \ 1  2  3  4  2  5  6  3  6  7  8  8  9 */

/*     OR (SYMMETRICALLY) AS */

/*                \ 1  2  3  4  5  6  7  8  9 */
/*            ---+-------------------------- */
/*            IA \ 1  4  5  7  9 10 */
/*            JA \ 1  3  4  2  3  4  4  5  5 */
/*             A \ 1  2  3  4  5  6  7  8  9             . */


/*  REORDERING THE ROWS AND COLUMNS OF M */

/*     A SYMMETRIC PERMUTATION OF THE ROWS AND COLUMNS OF THE COEFFICIENT 
*/
/*     MATRIX M (E.G., WHICH REDUCES FILLIN OR ENHANCES NUMERICAL */
/*     STABILITY) MUST BE SPECIFIED.  THE SOLUTION Z IS RETURNED IN THE */
/*     ORIGINAL ORDER. */

/*     TO SPECIFY THE TRIVIAL ORDERING (I.E., THE IDENTITY PERMUTATION), */

/*     SET  P(I) = IP(I) = I,  I=1,...,N.  IN THIS CASE, P AND IP CAN BE */

/*     THE SAME ARRAY. */

/*     IF A NONTRIVIAL ORDERING (I.E., NOT THE IDENTITY PERMUTATION) IS */
/*     SPECIFIED AND M IS STORED SYMMETRICALLY (I.E., NOT BOTH M(I,J) AND 
*/
/*     M(J,I) ARE STORED FOR I NE J), THEN ODRV SHOULD BE CALLED (WITH */
/*     PATH = 3 OR 5) TO SYMMETRICALLY REORDER (IA,JA,A) BEFORE CALLING */
/*     SDRV.  THIS IS TO ENSURE THAT IF M(I,J) WILL BE IN THE UPPER */
/*     TRIANGLE OF M WITH RESPECT TO THE NEW ORDERING, THEN M(I,J) IS */
/*     STORED IN ROW I (AND THUS M(J,I) IS NOT STORED);  WHEREAS IF M(I,J) 
*/
/*     WILL BE IN THE STRICT LOWER TRIANGLE OF M, THEN M(J,I) IS STORED IN 
*/
/*     ROW J (AND THUS M(I,J) IS NOT STORED). */


/*  PARAMETERS */

/*     N     - NUMBER OF VARIABLES/EQUATIONS */

/*     P     - INTEGER ONE-DIMENSIONAL ARRAY SPECIFYING A PERMUTATION OF */
/*              THE ROWS AND COLUMNS OF M;  DIMENSION = N */

/*     IP    - INTEGER ONE-DIMENSIONAL ARRAY CONTAINING THE INVERSE OF THE 
*/
/*              PERMUTATION SPECIFIED IN P;  I.E., IP(P(I)) = I, I=1,...,N; 
*/
/*              DIMENSION = N */

/*     IA    - INTEGER ONE-DIMENSIONAL ARRAY CONTAINING POINTERS TO DELIMIT 
*/
/*              ROWS IN JA AND A;  DIMENSION = N+1 */

/*     JA    - INTEGER ONE-DIMENSIONAL ARRAY CONTAINING THE COLUMN INDICES 
*/
/*              CORRESPONDING TO THE ELEMENTS OF A;  DIMENSION = NUMBER OF */

/*              NONZERO ENTRIES IN M STORED */

/*     A     - REAL ONE-DIMENSIONAL ARRAY CONTAINING THE NONZERO ENTRIES IN 
*/
/*              THE COEFFICIENT MATRIX M, STORED BY ROWS;  DIMENSION = */
/*              NUMBER OF NONZERO ENTRIES IN M STORED */

/*     B     - REAL ONE-DIMENSIONAL ARRAY CONTAINING THE RIGHT-HAND SIDE B; 
*/
/*              B AND Z CAN BE THE SAME ARRAY;  DIMENSION = N */

/*     Z     - REAL ONE-DIMENSIONAL ARRAY CONTAINING THE SOLUTION X;  Z AND 
*/
/*              B CAN BE THE SAME ARRAY;  DIMENSION = N */

/*     NSP  - DECLARED DIMENSION OF THE ONE-DIMENSIONAL ARRAYS ISP AND */
/*              RSP;  NSP MUST BE (SUBSTANTIALLY) LARGER THAN  3N+2K,  WHERE 
*/
/*              K = NUMBER OF NONZERO ENTRIES IN THE UPPER TRIANGLE OF M */

/*     ISP  - INTEGER ONE-DIMENSIONAL ARRAY USED FOR WORKING STORAGE;  ISP 
*/
/*              AND RSP SHOULD BE EQUIVALENCED;  DIMENSION = NSP */

/*     RSP  - REAL ONE-DIMENSIONAL ARRAY USED FOR WORKING STORAGE;  RSP */
/*              AND ISP SHOULD BE EQUIVALENCED;  DIMENSION = NSP */

/*     ESP  - INTEGER VARIABLE;  IF SUFFICIENT STORAGE WAS AVAILABLE TO */
/*              PERFORM THE SYMBOLIC FACTORIZATION (SSF), THEN ESP IS SET TO 
*/
/*              THE AMOUNT OF EXCESS STORAGE PROVIDED (NEGATIVE IF */
/*              INSUFFICIENT STORAGE WAS AVAILABLE TO PERFORM THE NUMERIC */
/*              FACTORIZATION (SNF)) */

/*     PATH - INTEGER PATH SPECIFICATION;  VALUES AND THEIR MEANINGS ARE - 
*/
/*                 1  PERFORM SSF, SNF, AND SNS */
/*                 2  PERFORM SNF AND SNS (ISP/RSP IS ASSUMED TO HAVE BEEN */
/*                        SET UP IN AN EARLIER CALL TO SDRV (FOR SSF)) */
/*                 3  PERFORM SNS ONLY (ISP/RSP IS ASSUMED TO HAVE BEEN SET */
/*                        UP IN AN EARLIER CALL TO SDRV (FOR SSF AND SNF)) */
/*                 4  PERFORM SSF */
/*                 5  PERFORM SSF AND SNF */
/*                 6  PERFORM SNF ONLY (ISP/RSP IS ASSUMED TO HAVE BEEN SET */
/*                        UP IN AN EARLIER CALL TO SDRV (FOR SSF)) */
/* K.B.          7  Perform semi-SNS to convert B from D coordinates to 
                            M coordinates.  Useful for finding downhill
                            vector for semidefinite matrix.    12/28/93  */

/*     FLAG - INTEGER ERROR FLAG;  VALUES AND THEIR MEANINGS ARE - */

/*                    0      NO ERRORS DETECTED */
/*                  2N+K    DUPLICATE ENTRY IN A  --  ROW = K */
/*                  6N+K    INSUFFICIENT STORAGE IN SSF  --  ROW = K */
/*                  7N+1    INSUFFICIENT STORAGE IN SNF */
/*                  8N+K    ZERO PIVOT  --  ROW = K */
/*                 10N+1    INSUFFICIENT STORAGE IN SDRV */
/*                 11N+1    ILLEGAL PATH SPECIFICATION */

/* ====================  change #4  (insertion) =======================4 */
/*                  <0      MATRIX NOT SUFF. POS-DEF (detected in SNFMOD) */
/*                            FLAG IS SET TO MINUS THE INDEX (IN THE ORIGINAL */
/*                            MATRIX) OF THE LARGEST ADDITION in E */
/*                        Flag is minus number of negative diagonals.  */
/* ==================================================================end */


/*  CONVERSION FROM REAL TO DOUBLE PRECISION */

/*     CHANGE THE REAL DECLARATIONS IN SDRV, SNF, AND SNS TO DOUBLE */
/*     PRECISION DECLARATIONS;  AND CHANGE THE VALUE IN THE DATA STATEMENT 
*/
/*     FOR THE INTEGER VARIABLE RATIO (IN SDRV) FROM 1 TO 2. */

/*  NOTE: FOR CRAY, SET RATIO to 1! */
/* -----------------------------------------------------------------------
 */

/*         REAL  A(1),  B(1),  Z(1),  RSP(1) */
/*         DATA  RATIO/1/ */
     /* Parameter adjustments */
     --rsp;
     --isp;
     --z;
     --b;
     --a;
     --ja;
     --ia;
     --ip;
     --p;

     /* Function Body */

/* ----VALIDATE PATH SPECIFICATION */
     if (*path < 1 || 7 < *path) {
          goto L111;
     }

/* ----ALLOCATE STORAGE AND FACTOR M SYMBOLICALLY TO DETERMINE FILL-IN */
     iju = 1;
     iu = iju + *n;
     jl = iu + *n + 1;
     ju = jl + *n;
     q = *nsp + 1 - *n;
     mark = q - *n;
     jumax = mark - ju;

     if ((*path - 1) * (*path - 4) * (*path - 5) != 0) {
          goto L1;
     }
     if (jumax <= 0) {
          goto L110;
     }
     ssf_(n, &p[1], &ip[1], &ia[1], &ja[1], &isp[iju], &isp[ju], &isp[iu], &
                jumax, &isp[q], &isp[mark], &isp[jl], flag_);
     if (*flag_ != 0) {
          goto L100;
     }

/* ----ALLOCATE STORAGE AND FACTOR M NUMERICALLY */
L1:
     il = ju + isp[iju + (*n - 1)];
     tmp = (il - 1 + (ratio - 1)) / ratio + 1;
     d = tmp + *n;
     u = d + *n;
     umax = *nsp + 1 - u;
     *esp = umax - (isp[iu + *n] - 1);

     if ((*path - 1) * (*path - 2) * (*path - 5) * (*path - 6) != 0) {
          goto L2;
     }
     if (umax <= 0) {
          goto L110;
     }
/* ====================  change #5  (replacement) =====================5 
*/
/*  WAS:    CALL SNF */
/* ==================================================================end 
*/
     snfmod_(n, &p[1], &ip[1], &ia[1], &ja[1], &a[1], &rsp[d], &isp[iju], &isp[
                ju], &isp[iu], &rsp[u], &umax, &isp[il], &isp[jl], flag_, emax);
/* ====================  change #6  (replacement) =====================6 
*/
/*  WAS:            IF (FLAG.NE.0)  GO TO 100 */
/* ==================================================================end 
*/
     if (*flag_ > 0) {
          goto L100;
     }

/* ----SOLVE SYSTEM OF LINEAR EQUATIONS  MX = B */
L2:
     if ((*path - 1) * (*path - 2) * (*path - 3) * (*path - 7) != 0) {
          goto L3;
     }
     if (umax <= 0) {
          goto L110;
     }
     if ( *path == 7 ) /* K.B. */
        ysmp_negvector(n, &p[1], &rsp[d], &isp[iju], &isp[ju], &isp[iu], 
            &rsp[u], &z[1], &b[1], &rsp[tmp]);
     else  sns_(n, &p[1], &rsp[d], &isp[iju], &isp[ju], &isp[iu], 
            &rsp[u], &z[1], &b[1], &rsp[tmp]);

L3:
     return 0;

/* ** ERROR -- ERROR DETECTED IN SSF, SNF, OR SNS */
L100:
     return 0;
/* ** ERROR -- INSUFFICIENT STORAGE */
L110:
     *flag_ = *n * 10 + 1;
     return 0;
/* ** ERROR -- ILLEGAL PATH SPECIFICATION */
L111:
     *flag_ = *n * 11 + 1;
     return 0;
} /* sdrvmd_ */


/* *********************************************************************** */
/* *********************************************************************** */
/* NUMERICAL FACTORIZATION OF SYMMETRIC MATRICES */
/* *********************************************************************** */

/* ====================  change #1  (replacement) =====================1 */
/* WAS: */
/*  SNF -- NUMERICAL UT-D-U FACTORIZATION OF SPARSE SYMMETRIC POSITIVE */
/*            DEFINITE MATRIX */
/*         SUBROUTINE  SNF */
/* ===================================================================== */

/*  SNFMOD -- NUMERICAL FACTORIZATION OF SPARSE SYMMETRIC MATRICES M BY */
/*            THE GILL/MURRAY/WRIGHT MODIFIED CHOLESKY FACTORIZATION (GMW */
/*            MCF) WITHOUT PIVOTING.  THE FACTORIZATION PRODUCES U,D, AND */
/*            E SO THAT    M + E = UT-D-U,  WHERE  E AND D ARE DIAGONAL */
/*            MATRICES. THIS ROUTINE IS A MODIFICATION OF THE YSMP */
/*            routine SNF. ALL CHANGES ARE INDICATED. */

/* Subroutine */ int snfmod_(n, p, ip, ia, ja, a, d, iju, ju, iu, u, umax, il,
            jl, flag_, emax)
integer *n, *p, *ip, *ia, *ja;
doublereal *a, *d;
integer *iju, *ju, *iu;
doublereal *u;
integer *umax, *il, *jl, *flag_;
doublereal *emax;
{
     /* System generated locals */
     integer i__1, i__2;
     doublereal d__1, d__2, d__3;

     /* Local variables */
     STATIC integer jmin, jmax;
     STATIC doublereal zero;
     STATIC integer irow, i, j, k;
     STATIC doublereal sgamma, ukidi, bound;
     STATIC integer kkmin, kkmax, nexti, jumuj;
     STATIC doublereal dk;
     STATIC integer kk, vj;
     STATIC doublereal xi;
     STATIC integer mu;
     STATIC doublereal eltnew, del;
     STATIC integer ili;
     STATIC doublereal elt, eps, xin, elt2, eps1;

     mat_index = 0 ;  /* number of negatives on diagonal. K.B.*/ 
     mat_null = 0 ;  /* number of zeroes on diagonal. K.B.*/ 
     pos_def_warning_flag = 0; /* K.B. */

/* ==================================================================end */

/*  ADDITIONAL PARAMETERS */

/*     IL     - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = N */

/*     JL     - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = N */

/*  DEFINITIONS OF INTERNAL PARAMETERS (DURING K-TH STAGE OF ELIMINATION) */

/*     (D(I),I=K,N) CONTAINS THE K-TH ROW OF U (EXPANDED) */

/*     IL(I) POINTS TO THE FIRST NONZERO ELEMENT IN COLUMNS K,...,N OF */
/*        ROW I OF U */

/*     JL CONTAINS LISTS OF ROWS TO BE ADDED TO UNELIMINATED ROWS -- */
/*        I GE K => JL(I) IS THE FIRST ROW TO BE ADDED TO ROW I */
/*        I LT K => JL(I) IS THE ROW FOLLOWING ROW I IN SOME LIST OF ROWS */

/*        IN EITHER CASE, JL(I) = 0 INDICATES THE END OF A LIST */

/*     EMAX is returned as max diagonal addition -- RFA, August 1991 */

/* ----------------------------------------------------------------------- */

/*         REAL  A(1),  D(1), U(1),  DK, UKIDI */

/* ====================  change #2  (insertion) =======================2 */
/*         real GAMMA,XI,XIN,EPS,DEL,EPS1,ELT,ELT2,ELTNEW, */
/*     *      BOUND,W,EMAX,EK,ZERO */
     /* Parameter adjustments */
     --jl; --il; --u; --iu; --ju; --iju; --d; --a; --ja; --ia; --ip; --p;

     /* Function Body */
     *flag_ = 0;
     zero = 0.;
     *emax = zero;
/* ==================================================================end */

/* ----CHECK FOR SUFFICIENT STORAGE FOR U */
     if (iu[*n + 1] - 1 > *umax) {
          goto L107;
     }
if ( !hessian_quiet_flag ) printf("ysmp fill: %d\n",iu[*n+1]);

/* ----INITIALIZATION */
     i__1 = *n;
     for (k = 1; k <= i__1; ++k) {
          d[k] = 0.;
/* L1: */
          jl[k] = 0;
     }
/* ====================  change #3  (insertion) =======================3 */
/* Calculate GAMMA and XI, the largest magnitudes of the diag. and  off- */
/* diag. elements, respectively. When the diag. elts. are stored first */
/* in A in each row (PATH = 4 or 5 in ODRV), GAMMA=max(GAMMA,A(IA(i))), */
/* i=1,...,IA(n+1)-1. We assume that this IS the case. (If this were */
/* later changed, then for each row I we would have to loop through KK */
/* = KKMIN,..., KKMAX  where KKMIN = IA(I), KKMAX = IA(I+1)-1, and test */
/* whether I = JA(KK), ie. row index = column index. If this equality */
/* holds, the element is a diagonal). Then calculate DEL and BOUND: */
/* DEL =  max ( max(XI,GAMMA)*EPS, EPS) where EPS is a small given */
/* number, and  BOUND = max ( XI/N, GAMMA, EPS). */
/* ===================================================================== 
*/
     eps = 1e-6;
     sgamma = zero;
     xi = zero;
     i__1 = *n;
     for (irow = 1; irow <= i__1; ++irow) {
/* Computing MAX */
          d__2 = sgamma, d__3 = (d__1 = a[ia[irow]], fabs(d__1));
          sgamma = max(d__2,d__3);
          kkmin = ia[irow] + 1;
          kkmax = ia[irow + 1] - 1;
          if (kkmin > kkmax) {
                goto L21;
          }
          i__2 = kkmax;
          for (kk = kkmin; kk <= i__2; ++kk) {
/* Computing MAX */
                d__2 = xi, d__3 = (d__1 = a[kk], fabs(d__1));
                xi = max(d__2,d__3);
/* L20: */
          }
L21:
          ;
     }
     eps1 = max(sgamma,xi) * eps;
     del = max(eps,eps1);
     xin = (doublereal) (*n);
     xin = xi / xin;
/* Computing MAX */
     d__1 = max(sgamma,xin);
     bound = max(d__1,eps);
/* ==================================================================end 
*/

/* ----FOR EACH ROW K */
     i__1 = *n;
     for (k = 1; k <= i__1; ++k) {

/* ------INITIALIZE K-TH ROW WITH ELEMENTS NONZERO IN ROW P(K) OF M */

/* L3: */
          jmin = ia[p[k]];
          jmax = ia[p[k] + 1] - 1;
          if (jmin > jmax) {
                goto L5;
          }
          i__2 = jmax;
          for (j = jmin; j <= i__2; ++j) {
                vj = ip[ja[j]];
                if (k <= vj) {
                     d[vj] = a[j];
                }
/* L4: */
          }

/* ------MODIFY K-TH ROW BY ADDING IN THOSE ROWS I WITH U(I,K) NE 0 */

/* ------FOR EACH ROW I TO BE ADDED IN */
L5:
          dk = d[k];
          i = jl[k];
L6:
          if (i == 0) {
                goto L9;
          }
          nexti = jl[i];

/* --------COMPUTE MULTIPLIER AND UPDATE DIAGONAL ELEMENT */
          ili = il[i];
          ukidi = -u[ili] * d[i];
          dk += ukidi * u[ili];
          u[ili] = ukidi;

/* --------ADD MULTIPLE OF ROW I TO K-TH ROW ... */
          jmin = ili + 1;
          jmax = iu[i + 1] - 1;
          if (jmin > jmax) {
                goto L8;
          }
          mu = iju[i] - iu[i];
          i__2 = jmax;
          for (j = jmin; j <= i__2; ++j) {
/* L7: */
                d[ju[mu + j]] += ukidi * u[j];
          }

/* --------... AND ADD I TO ROW LIST FOR NEXT NONZERO ENTRY */
          il[i] = jmin;
          j = ju[mu + jmin];
          jl[i] = jl[j];
          jl[j] = i;

L8:
          i = nexti;
          goto L6;

/* ====================  change #4  (replacement) =====================4 */
/* WAS: */
/* ------CHECK FOR ZERO PIVOT */
/*  9        IF (DK.EQ.0)  GO TO 108 */
/* ===================================================================== */
/* STATEMENT 9 ABOVE WILL BE MODIFIED TO RESET Dk IN THE EVENT THE */
/* THE MATRIX IS NOT SUFF. POSITIVE-DEFINITE. NOTE THAT EVEN WHEN Dk>0, */
/* IT MAY BE MODIFIED IF THE MATRIX IS NOT POS. DEF! */

/* Dk is set as:  Dk = MAX ( ABS(Dk), DEL, (ELT**2)/BOUND), where */
/* ELT is the largest magnitude among the elements in the Kth row of U. */
/* This restriction guarantees that all elts. of D are strictly positive */
/* and that the elts. of the factors satisfy a uniform bound. */
/* [    Recall that we work with the auxiliary quantities  Vik = Uik * Dk. */
/*      The bound we want to impose on the elts. of U, */
/*             ( max(Uik)**2 )  * Dk  <=  BOUND, is equivalent to */
/*             ( max(Vik)**2 )  / Dk  <=  BOUND, or */
/*             Dk    >=     (max(Vik)**2) / BOUND.) */
/*      The value for ELT = max(Vik), max over i for fixed k, is found by */
/*      looping through the appropriate elements of U. These elements */
/*      are currently stored in part of D.  ] */

/* ===================================================================== */

L9:
/* ===================================================================== */
          elt = zero;
          jmin = iu[k];
          jmax = iu[k + 1] - 1;
          mu = iju[k] - jmin;
          if (jmin > jmax) {
                goto L28;
          }
          i__2 = jmax;
          for (j = jmin; j <= i__2; ++j) {
                eltnew = (d__1 = d[ju[mu + j]], fabs(d__1));
                elt = max(elt,eltnew);
/* L26: */
          }
L28:

          elt2 = elt * elt;
#ifdef OLDWAY
/* Computing MAX */
          d__1 = fabs(dk), d__1 = max(d__1,del), d__2 = elt2 / bound;
          dk = max(d__1,d__2);
          ek = dk - w;
          if (ek > *emax) {
                *emax = ek;
                *flag_ = -p[k];
          }
#else
    /* don't try to make pos def; just keep diag nonzero */
    /* K.B. 8/7/93 */
    if ( fabs(dk) <= hessian_epsilon )
     {  
        sprintf(msg,"Internal error: sdrv: Diag[%d] = %g; max in row: %g; adding %g",k,(DOUBLE)dk,(DOUBLE)elt,(DOUBLE)hessian_epsilon);
        if ( !hessian_quiet_flag ) 
          kb_error(1647,msg,WARNING);
        dk = hessian_epsilon;

        mat_null++;
     }
    /* K.B. 12/28/93 */
    if ( dk < -hessian_epsilon )
     {
        mat_index++;
        sprintf(errmsg,"sdrv: Intermediate matrix not positive definite. Diag[%d] = %g;",k,(DOUBLE)dk);
        if ( dk < *emax ) { *emax = dk; *flag_ = -p[k];  }
        if ( make_pos_def_flag )
        {
          d__1 = fabs(dk), d__1 = max(d__1,del), d__2 = elt2 / bound;
          dk = max(d__1,d__2);
          sprintf(errmsg+strlen(msg)," Forcing positive to %g.",(DOUBLE)dk);
        }
        if (!pos_def_warning_flag) 
        if ( !hessian_quiet_flag )  
          kb_error(1648,errmsg,WARNING);

        pos_def_warning_flag = 1;
     }
#endif

/* =================================================================e
nd */
/* L30: */
/* ------SAVE DIAGONAL ELEMENT */
          d[k] = 1 / dk;

/* ------SAVE NONZERO ENTRIES IN K-TH ROW OF U ... */
          jmin = iu[k];
          jmax = iu[k + 1] - 1;
          if (jmin > jmax) {
                goto L11;
          }
          mu = iju[k] - jmin;
          i__2 = jmax;
          for (j = jmin; j <= i__2; ++j) {
                jumuj = ju[mu + j];
                u[j] = d[jumuj];
/* L10: */
                d[jumuj] = 0.;
          }

/* ------... AND ADD K TO ROW LIST FOR FIRST NONZERO ENTRY IN K-TH ROW
 */
          il[k] = jmin;
          i = ju[mu + jmin];
          jl[k] = jl[i];
          jl[i] = k;
L11:
          ;
     }

/* uncomment next line to print out the largest diagonal modification */
/*        IF (FLAG .LT. 0) WRITE (6,800) EMAX,-FLAG */
/* L800: */
/* ====================  change #5  (deletion) ========================5 
*/
/* WAS:      FLAG = 0 */
/* ==================================================================end 
*/
     return 0;

/* ** ERROR -- INSUFFICIENT STORAGE FOR U */
L107:
     *flag_ = *n * 7 + 1;
     return 0;
} /* snfmod_ */

/* *********************************************************************** */
/*  SSF --  SYMBOLIC UT-D-U FACTORIZATION OF SPARSE SYMMETRIC MATRIX */
/* *********************************************************************** */
/* Subroutine */ int ssf_(n, p, ip, ia, ja, iju, ju, iu, jumax, q, mark, jl, 
          flag_)
integer *n, *p, *ip, *ia, *ja, *iju, *ju, *iu, *jumax, *q, *mark, *jl, *flag_;
{ /* System generated locals */
     integer i__1, i__2, i__3;
     /* Local variables */
     STATIC integer jmin, jmax, lmax, i, j, k, m, jumin, juptr, vj, qm;
     STATIC logical clique;
     STATIC integer tag, lui, luk;

/*  ADDITIONAL PARAMETERS */
/*     Q      - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = N */
/*     MARK  - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = N */
/*     JL     - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = N */

/*  DEFINITIONS OF INTERNAL PARAMETERS (DURING K-TH STAGE OF ELIMINATION) */
/*     Q CONTAINS AN ORDERED LINKED LIST REPRESENTATION OF THE NONZERO      */
/*        STRUCTURE OF THE K-TH ROW OF U --                                            */
/*          Q(K) IS THE FIRST COLUMN WITH A NONZERO ENTRY                         */
/*          Q(I) IS THE NEXT COLUMN WITH A NONZERO ENTRY AFTER COLUMN I      */
/*        IN EITHER CASE, Q(I) = N+1 INDICATES THE END OF THE LIST             */
/*     JL CONTAINS LISTS OF ROWS TO BE MERGED INTO UNELIMINATED ROWS --     */
/*          I GE K => JL(I) IS THE FIRST ROW TO BE MERGED INTO ROW I          */
/*          I LT K => JL(I) IS THE ROW FOLLOWING ROW I IN SOME LIST OF ROWS */
/*        IN EITHER CASE, JL(I) = 0 INDICATES THE END OF A LIST                 */
/*     MARK(I) IS THE LAST ROW STORED IN JU FOR WHICH U(MARK(I),I) NE 0     */
/*     JUMIN AND JUPTR ARE THE INDICES IN JU OF THE FIRST AND LAST            */
/*        ELEMENTS IN THE LAST ROW SAVED IN JU                                        */
/*     LUK IS THE NUMBER OF NONZERO ENTRIES IN THE K-TH ROW                     */
/* -----------------------------------------------------------------------*/

/* ----INITIALIZATION */
     /* Parameter adjustments */
     --jl; --mark; --q; --iu; --ju; --iju; --ja; --ia; --ip; --p;

     /* Function Body */
     jumin = 1;
     juptr = 0;
     iu[1] = 1;
     i__1 = *n;
     for (k = 1; k <= i__1; ++k) {
          mark[k] = 0;
/* L1: */
          jl[k] = 0;
     }

/* ----FOR EACH ROW K */
     i__1 = *n;
     for (k = 1; k <= i__1; ++k) {
          luk = 0;
          q[k] = *n + 1;

          tag = mark[k];
          clique = FALSE_;
          if (jl[k] != 0) {
                clique = jl[jl[k]] == 0;
          }

/* ------INITIALIZE NONZERO STRUCTURE OF K-TH ROW TO ROW P(K) OF M */
          jmin = ia[p[k]];
          jmax = ia[p[k] + 1] - 1;
          if (jmin > jmax) {
                goto L4;
          }
          i__2 = jmax;
          for (j = jmin; j <= i__2; ++j) {
                vj = ip[ja[j]];
                if (vj <= k) {
                     goto L3;
                }

                qm = k;
L2:
                m = qm;
                qm = q[m];
                if (qm < vj) {
                     goto L2;
                }
                if (qm == vj) {
                     goto L102;
                }
                ++luk;
                q[m] = vj;
                q[vj] = qm;
                if (mark[vj] != tag) {
                     clique = FALSE_;
                }

L3:
                ;
          }

/* ------IF EXACTLY ONE ROW IS TO BE MERGED INTO THE K-TH ROW AND THERE IS */
/* ------A NONZERO ENTRY IN EVERY COLUMN IN THAT ROW IN WHICH THERE IS A */
/* ------NONZERO ENTRY IN ROW P(K) OF M, THEN DO NOT COMPUTE FILL-IN, JUST */
/* ------USE THE COLUMN INDICES FOR THE ROW WHICH WAS TO HAVE BEEN MERGED */
L4:
          if (! clique) {
                goto L5;
          }
          iju[k] = iju[jl[k]] + 1;
          luk = iu[jl[k] + 1] - (iu[jl[k]] + 1);
          goto L17;

/* ------MODIFY NONZERO STRUCTURE OF K-TH ROW BY COMPUTING FILL-IN */
/* ------FOR EACH ROW I TO BE MERGED IN */
L5:
          lmax = 0;
          iju[k] = juptr;

          i = k;
L6:
          i = jl[i];
          if (i == 0) {
                goto L10;
          }

/* --------MERGE ROW I INTO K-TH ROW */
          lui = iu[i + 1] - (iu[i] + 1);
          jmin = iju[i] + 1;
          jmax = iju[i] + lui;
          qm = k;

          i__2 = jmax;
          for (j = jmin; j <= i__2; ++j) {
                vj = ju[j];
L7:
                m = qm;
                qm = q[m];
                if (qm < vj) {
                     goto L7;
                }
                if (qm == vj) {
                     goto L8;
                }
                ++luk;
                q[m] = vj;
                q[vj] = qm;
                qm = vj;
L8:
                ;
          }

/* --------REMEMBER LENGTH AND POSITION IN JU OF LONGEST ROW MERGED */

          if (lui <= lmax) {
                goto L9;
          }
          lmax = lui;
          iju[k] = jmin;

L9:
          goto L6;

/* ------IF THE K-TH ROW IS THE SAME LENGTH AS THE LONGEST ROW MERGED,
 */
/* ------THEN USE THE COLUMN INDICES FOR THAT ROW */
L10:
          if (luk == lmax) {
                goto L17;
          }

/* ------IF THE TAIL OF THE LAST ROW SAVED IN JU IS THE SAME AS THE HEAD */
/* ------OF THE K-TH ROW, THEN OVERLAP THE TWO SETS OF COLUMN INDICES -- */
/* --------SEARCH LAST ROW SAVED FOR FIRST NONZERO ENTRY IN K-TH ROW ... */
          i = q[k];
          if (jumin > juptr) {
                goto L12;
          }
          i__2 = juptr;
          for (jmin = jumin; jmin <= i__2; ++jmin) {
                if ((i__3 = ju[jmin] - i) < 0) {
                     goto L11;
                } else if (i__3 == 0) {
                     goto L13;
                } else {
                     goto L12;
                }
L11:
                ;
          }
L12:
          goto L15;

/* --------... AND THEN TEST WHETHER TAIL MATCHES HEAD OF K-TH ROW */
L13:
          iju[k] = jmin;
          i__2 = juptr;
          for (j = jmin; j <= i__2; ++j) {
                if (ju[j] != i) {
                     goto L15;
                }
                i = q[i];
                if (i > *n) {
                     goto L17;
                }
/* L14: */
          }
          juptr = jmin - 1;

/* ------SAVE NONZERO STRUCTURE OF K-TH ROW IN JU */
L15:
          i = k;
          jumin = juptr + 1;
          juptr += luk;
          if (juptr > *jumax) {
                goto L106;
          }
          i__2 = juptr;
          for (j = jumin; j <= i__2; ++j) {
                i = q[i];
                ju[j] = i;
/* L16: */
                mark[i] = k;
          }
          iju[k] = jumin;

/* ------ADD K TO ROW LIST FOR FIRST NONZERO ELEMENT IN K-TH ROW */
L17:
          if (luk <= 1) {
                goto L18;
          }
          i = ju[iju[k]];
          jl[k] = jl[i];
          jl[i] = k;

L18:
          iu[k + 1] = iu[k] + luk;
     }

     *flag_ = 0;
     return 0;

/* ** ERROR -- DUPLICATE ENTRY IN A */
L102:
     *flag_ = (*n << 1) + p[k];
     return 0;
/* ** ERROR -- INSUFFICIENT STORAGE FOR JU */
L106:
     *flag_ = *n * 6 + k;
     return 0;
} /* ssf_ */


/* *********************************************************************** */
/*  SNS -- SOLUTION OF SPARSE SYMMETRIC POSITIVE DEFINITE SYSTEM OF */
/*            LINEAR EQUATIONS  MX = B  GIVEN UT-D-U FACTORIZATION OF M */
/* *********************************************************************** */
/* Subroutine */ int sns_(n, p, d, iju, ju, iu, u, z, b, tmp)
integer *n, *p;
doublereal *d;
integer *iju, *ju, *iu;
doublereal *u, *z, *b, *tmp;
{
     /* System generated locals */
     integer i__1, i__2;

     /* Local variables */
     STATIC integer jmin, jmax;
     STATIC doublereal tmpk;
     STATIC integer i, j, k, mu;
     STATIC doublereal sum;

/*         REAL  D(1), U(1),  Z(1), B(1),  TMP(1),  TMPK, SUM */

/*  ADDITIONAL PARAMETERS */

/*     TMP    - REAL ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = N */

/* -----------------------------------------------------------------------
 */

/* ----SET TMP TO PERMUTED B */
     /* Parameter adjustments */
     --tmp;
     --b;
     --z;
     --u;
     --iu;
     --ju;
     --iju;
     --d;
     --p;

     /* Function Body */
     i__1 = *n;
     for (k = 1; k <= i__1; ++k) {
/* L1: */
          tmp[k] = b[p[k]];
     }

/* ----SOLVE  UT D Y = B  BY FORWARD SUBSTITUTION */
     i__1 = *n;
     for (k = 1; k <= i__1; ++k) {
          tmpk = tmp[k];
          jmin = iu[k];
          jmax = iu[k + 1] - 1;
          if (jmin > jmax) {
                goto L3;
          }
          mu = iju[k] - jmin;
          i__2 = jmax;
          for (j = jmin; j <= i__2; ++j) {
/* L2: */
                tmp[ju[mu + j]] += u[j] * tmpk;
          }
L3:
          tmp[k] = tmpk * d[k];
     }

/* ----SOLVE  U X = Y  BY BACK SUBSTITUTION */
     k = *n;
     i__1 = *n;
     for (i = 1; i <= i__1; ++i) {
          sum = tmp[k];
          jmin = iu[k];
          jmax = iu[k + 1] - 1;
          if (jmin > jmax) {
                goto L5;
          }
          mu = iju[k] - jmin;
          i__2 = jmax;
          for (j = jmin; j <= i__2; ++j) {
/* L4: */
                sum += u[j] * tmp[ju[mu + j]];
          }
L5:
          tmp[k] = sum;
          z[p[k]] = sum;
/* L6: */
          --k;
     }

     return 0;
} /* sns_ */

/*************************************************************************
*  function: ysmp_negvector()
*
*  purpose: Find vector corresponding to diagonal element of D.    
*             GIVEN UT-D-U FACTORIZATION OF M
*              Actually just solves UZ = B.  Input B should have just
*              a 1 in spot corresponding to negative diagonal element.
*              Adapted from SNS_ by cutting out the UT D Y = B solution step.
* ************************************************************************/
/* Subroutine */ int ysmp_negvector(n, p, d, iju, ju, iu, u, z, b, tmp)
integer *n, *p;
doublereal *d;
integer *iju, *ju, *iu;
doublereal *u, *z, *b, *tmp;
{
     /* System generated locals */
     integer i__1, i__2;

     /* Local variables */
     STATIC integer jmin, jmax;
     STATIC integer i, j, k, mu;
     STATIC doublereal sum;

/*         REAL  D(1), U(1),  Z(1), B(1),  TMP(1),  TMPK, SUM */

/*  ADDITIONAL PARAMETERS */

/*     TMP    - REAL ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = N */

/* -----------------------------------------------------------------------
 */

/* ----SET TMP TO PERMUTED B */
     /* Parameter adjustments */
     --tmp;
     --b;
     --z;
     --u;
     --iu;
     --ju;
     --iju;
     --d;
     --p;

     /* Function Body */
     i__1 = *n;
     for (k = 1; k <= i__1; ++k) {
/* L1: */
          tmp[k] = b[p[k]];
     }

/* ----SOLVE  U X = Y  BY BACK SUBSTITUTION */
     k = *n;
     i__1 = *n;
     for (i = 1; i <= i__1; ++i) {
          sum = tmp[k];
          jmin = iu[k];
          jmax = iu[k + 1] - 1;
          if (jmin > jmax) {
                goto L5;
          }
          mu = iju[k] - jmin;
          i__2 = jmax;
          for (j = jmin; j <= i__2; ++j) {
/* L4: */
                sum += u[j] * tmp[ju[mu + j]];
          }
L5:
          tmp[k] = sum;
          z[p[k]] = sum;
/* L6: */
          --k;
     }

     return 0;
} /* ysmp_negvector */

