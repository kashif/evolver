/*************************************************************
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/* for making FORTRAN local variables stack instead of static */
#define STATIC

/************************************************************************/
/* odrv.f -- translated by f2c (version of 8 February 1991  13:22:30).  */
/* For YSMP sparse matrix package                                       */
/************************************************************************/

#include "include.h"
#include "f2c.h"

int odrv_ ARGS(( integer *, integer *,integer *,REAL *, integer *,integer *,
integer *,integer *, integer *, integer *));

/* *********************************************************************** */
/*                                                                                     1/15/81 */
/* *********************************************************************** */
/*  Function:  ODRV -- DRIVER FOR SPARSE MATRIX REORDERING ROUTINES */
/* *********************************************************************** */

int odrv_(n, ia, ja, a, p, ip, nsp, isp, path, flag_)
integer *n, *ia, *ja;
doublereal *a;
integer *p, *ip, *nsp, *isp, *path, *flag_;
{
  STATIC integer head, l;
  STATIC logical dflag;
  STATIC integer q, v;
  STATIC integer max_, tmp;
#ifdef NOPROTO
  extern /* Subroutine */ int md_();
  extern /* Subroutine */ int sro_();
#endif

/*  DESCRIPTION */

/*     ODRV FINDS A MINIMUM DEGREE ORDERING OF THE ROWS AND COLUMNS OF A   */
/*     SYMMETRIC MATRIX M STORED IN (IA,JA,A) FORMAT (SEE BELOW).  FOR THE */
/*     REORDERED MATRIX, THE WORK AND STORAGE REQUIRED TO PERFORM GAUSSIAN */
/*     ELIMINATION IS (USUALLY) SIGNIFICANTLY LESS.                        */

/*     IF ONLY THE NONZERO ENTRIES IN THE UPPER TRIANGLE OF M ARE BEING    */
/*     STORED, THEN ODRV SYMMETRICALLY REORDERS (IA,JA,A), (OPTIONALLY)    */
/*     WITH THE DIAGONAL ENTRIES PLACED FIRST IN EACH ROW.  THIS IS TO     */
/*     ENSURE THAT IF M(I,J) WILL BE IN THE UPPER TRIANGLE OF M WITH       */
/*     RESPECT TO THE NEW ORDERING, THEN M(I,J) IS STORED IN ROW I (AND    */
/*     THUS M(J,I) IS NOT STORED);  WHEREAS IF M(I,J) WILL BE IN THE       */
/*     STRICT LOWER TRIANGLE OF M, THEN M(J,I) IS STORED IN ROW J (AND     */
/*     THUS M(I,J) IS NOT STORED).                                         */


/*  STORAGE OF SPARSE MATRICES */

/*     THE NONZERO ENTRIES OF THE MATRIX M ARE STORED ROW-BY-ROW IN THE     */
/*     ARRAY A.  TO IDENTIFY THE INDIVIDUAL NONZERO ENTRIES IN EACH ROW,    */
/*     WE NEED TO KNOW IN WHICH COLUMN EACH ENTRY LIES.  THESE COLUMN       */
/*     INDICES ARE STORED IN THE ARRAY JA;  I.E., IF  A(K) = M(I,J),  THEN  */
/*     JA(K) = J.  TO IDENTIFY THE INDIVIDUAL ROWS, WE NEED TO KNOW WHERE   */
/*     EACH ROW STARTS.  THESE ROW POINTERS ARE STORED IN THE ARRAY IA;     */
/*     I.E., IF M(I,J) IS THE FIRST NONZERO ENTRY (STORED) IN THE I-TH ROW  */
/*     AND  A(K) = M(I,J),  THEN  IA(I) = K.  MOREOVER, IA(N+1) POINTS TO   */
/*     THE FIRST LOCATION FOLLOWING THE LAST ELEMENT IN THE LAST ROW.       */
/*     THUS, THE NUMBER OF ENTRIES IN THE I-TH ROW IS  IA(I+1) - IA(I),     */
/*     THE NONZERO ENTRIES IN THE I-TH ROW ARE STORED CONSECUTIVELY IN      */

/*                A(IA(I)),  A(IA(I)+1),  ..., A(IA(I+1)-1),                */

/*     AND THE CORRESPONDING COLUMN INDICES ARE STORED CONSECUTIVELY IN     */

/*                JA(IA(I)), JA(IA(I)+1), ..., JA(IA(I+1)-1).               */

/*     SINCE THE COEFFICIENT MATRIX IS SYMMETRIC, ONLY THE NONZERO ENTRIES  */
/*     IN THE UPPER TRIANGLE NEED BE STORED.  FOR EXAMPLE, THE MATRIX       */

/*                 ( 1  0  2  3  0 ) */
/*                 ( 0  4  0  0  0 ) */
/*             M = ( 2  0  5  6  0 ) */
/*                 ( 3  0  6  7  8 ) */
/*                 ( 0  0  0  8  9 ) */

/*     COULD BE STORED AS */

/*               \ 1  2  3  4  5  6  7  8  9 10 11 12 13 */
/*            ---+-------------------------------------- */
/*            IA \ 1  4  5  8 12 14                      */
/*            JA \ 1  3  4  2  1  3  4  1  3  4  5  4  5 */
/*             A \ 1  2  3  4  2  5  6  3  6  7  8  8  9 */

/*     OR (SYMMETRICALLY) AS */

/*                \ 1  2  3  4  5  6  7  8  9 */
/*            ---+-------------------------- */
/*            IA \ 1  4  5  7  9 10 */
/*            JA \ 1  3  4  2  3  4  4  5  5 */
/*             A \ 1  2  3  4  5  6  7  8  9             . */


/*  PARAMETERS */

/*     N     - ORDER OF THE MATRIX                                          */
/*     IA    - INTEGER ONE-DIMENSIONAL ARRAY CONTAINING POINTERS TO DELIMIT */
/*              ROWS IN JA AND A;  DIMENSION = N+1                          */
/*     JA    - INTEGER ONE-DIMENSIONAL ARRAY CONTAINING THE COLUMN INDICES  */
/*              CORRESPONDING TO THE ELEMENTS OF A;  DIMENSION = NUMBER OF  */
/*              NONZERO ENTRIES IN (THE UPPER TRIANGLE OF) M                */
/*     A     - REAL ONE-DIMENSIONAL ARRAY CONTAINING THE NONZERO ENTRIES IN */
/*              (THE UPPER TRIANGLE OF) M, STORED BY ROWS;  DIMENSION =     */
/*              NUMBER OF NONZERO ENTRIES IN (THE UPPER TRIANGLE OF) M      */
/*     P     - INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN THE PERMUTATION */
/*              OF THE ROWS AND COLUMNS OF M CORRESPONDING TO THE MINIMUM   */
/*              DEGREE ORDERING;  DIMENSION = N                             */
/*     IP    - INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN THE INVERSE OF  */
/*              THE PERMUTATION RETURNED IN P;  DIMENSION = N               */
/*     NSP  - DECLARED DIMENSION OF THE ONE-DIMENSIONAL ARRAY ISP;  NSP     */
/*              MUST BE AT LEAST  3N+4K, WHERE K IS THE NUMBER OF NONZEROES */
/*              IN THE STRICT UPPER TRIANGLE OF M                           */
/*     ISP  - INTEGER ONE-DIMENSIONAL ARRAY USED FOR WORKING STORAGE;       */
/*              DIMENSION = NSP                                             */
/*     PATH - INTEGER PATH SPECIFICATION;  VALUES AND THEIR MEANINGS ARE -  */
/*              1  FIND MINIMUM DEGREE ORDERING ONLY                        */
/*              2  FIND MINIMUM DEGREE ORDERING AND REORDER SYMMETRICALLY   */
/*                 STORED MATRIX (USED WHEN ONLY THE NONZERO ENTRIES IN     */
/*                 THE UPPER TRIANGLE OF M ARE BEING STORED)                */
/*              3  REORDER SYMMETRICALLY STORED MATRIX AS SPECIFIED BY      */
/*                 INPUT PERMUTATION (USED WHEN AN ORDERING HAS ALREADY     */
/*                 BEEN DETERMINED AND ONLY THE NONZERO ENTRIES IN THE      */
/*                 UPPER TRIANGLE OF M ARE BEING STORED)                    */
/*              4  SAME AS 2 BUT PUT DIAGONAL ENTRIES AT START OF EACH ROW  */
/*              5  SAME AS 3 BUT PUT DIAGONAL ENTRIES AT START OF EACH ROW  */

/*     FLAG - INTEGER ERROR FLAG;  VALUES AND THEIR MEANINGS ARE - */
/*                    0     NO ERRORS DETECTED */
/*                  9N+K  INSUFFICIENT STORAGE IN MD */
/*                 10N+1  INSUFFICIENT STORAGE IN ODRV */
/*                 11N+1  ILLEGAL PATH SPECIFICATION */


/*  CONVERSION FROM REAL TO DOUBLE PRECISION */

/*     CHANGE THE REAL DECLARATIONS IN ODRV AND SRO TO DOUBLE PRECISION */
/*     DECLARATIONS. */

/* ----------------------------------------------------------------------- */

/* ....    REAL  A(1) */
/* ----INITIALIZE ERROR FLAG AND VALIDATE PATH SPECIFICATION */
     /* Parameter adjustments */
     --isp;
     --ip;
     --p;
     --a;
     --ja;
     --ia;

     /* Function Body */
     *flag_ = 0;
     if (*path < 1 || 5 < *path) {
          goto L111;
     }


/* ----ALLOCATE STORAGE AND FIND MINIMUM DEGREE ORDERING */
     if ((*path - 1) * (*path - 2) * (*path - 4) != 0) {
          goto L1;
     }
     max_ = (*nsp - *n) / 2;
     v = 1;
     l = v + max_;
     head = l + max_;
     if (max_ < *n) {
          goto L110;
     }



     md_(n, &ia[1], &ja[1], &max_, &isp[v], &isp[l], &isp[head], &p[1], &ip[1],
                 &isp[v], flag_);


     if (*flag_ != 0) {
          goto L100;
     }

/* ----ALLOCATE STORAGE AND SYMMETRICALLY REORDER MATRIX */
L1:
     if ((*path - 2) * (*path - 3) * (*path - 4) * (*path - 5) != 0) {
          goto L2;
     }
     tmp = *nsp + 1 - *n;
     q = tmp - (ia[*n + 1] - 1);
     if (q < 1) {
          goto L110;
     }

     dflag = *path == 4 || *path == 5;

     sro_(n, &ip[1], &ia[1], &ja[1], &a[1], &isp[tmp], &isp[q], &dflag);


L2:
     return 0;

/* ** ERROR -- ERROR DETECTED IN MD */
L100:
     return 0;
/* ** ERROR -- INSUFFICIENT STORAGE */
L110:
     *flag_ = *n * 10 + 1;
     return 0;
/* ** ERROR -- ILLEGAL PATH SPECIFIED */
L111:
     *flag_ = *n * 11 + 1;
     return 0;
} /* odrv_ */


/* *********************************************************************** */
/* *********************************************************************** */
/*  Function: MD -- MINIMUM DEGREE ALGORITHM (BASED ON ELEMENT MODEL) */
/* *********************************************************************** */

int md_(n, ia, ja, max_, v, l, head, last, next, mark, flag_)

integer *n, *ia, *ja, *max_, *v, *l, *head, *last, *next, *mark, *flag_;
{
     /* System generated locals */
     integer i__1;
     STATIC integer equiv_0[1];

     /* Local variables */
     STATIC integer dmin_, tail, k;
#define ek (equiv_0)
#define vk (equiv_0)
     STATIC integer tag;
#ifdef NOPROTO
     extern /* Subroutine */ int mdi_();
     extern /* Subroutine */ int mdm_(), mdp_(), mdu_();
#endif


/*  DESCRIPTION */

/*     MD FINDS A MINIMUM DEGREE ORDERING OF THE ROWS AND COLUMNS OF A */
/*     SYMMETRIC MATRIX M STORED IN (IA,JA,A) FORMAT. */


/*  ADDITIONAL PARAMETERS */

/*     MAX  - DECLARED DIMENSION OF THE ONE-DIMENSIONAL ARRAYS V AND L; */
/*              MAX MUST BE AT LEAST  N+2K,  WHERE K IS THE NUMBER OF */
/*              NONZEROES IN THE STRICT UPPER TRIANGLE OF M */

/*     V     - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = MAX */

/*     L     - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = MAX */

/*     HEAD - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = N */

/*     LAST - INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN THE PERMUTATION */
/*              OF THE ROWS AND COLUMNS OF M CORRESPONDING TO THE MINIMUM */
/*              DEGREE ORDERING;  DIMENSION = N */

/*     NEXT - INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN THE INVERSE OF */
/*              THE PERMUTATION RETURNED IN LAST;  DIMENSION = N */

/*     MARK - INTEGER ONE-DIMENSIONAL WORK ARRAY (MAY BE THE SAME AS V); */

/*              DIMENSION = N */

/*     FLAG - INTEGER ERROR FLAG;  VALUES AND THEIR MEANINGS ARE - */
/*                 0        NO ERRORS DETECTED */
/*                 11N+1  INSUFFICIENT STORAGE IN MD */


/*  DEFINITIONS OF INTERNAL PARAMETERS */

/*     ---------+--------------------------------------------------------- */
/*     V(S)      \ VALUE FIELD OF LIST ENTRY */
/*     ---------+--------------------------------------------------------- */
/*     L(S)      \ LINK FIELD OF LIST ENTRY  (0 => END OF LIST) */
/*     ---------+--------------------------------------------------------- */
/*     L(VI)     \ POINTER TO ELEMENT LIST OF UNELIMINATED VERTEX VI */
/*     ---------+--------------------------------------------------------- */
/*     L(EJ)     \ POINTER TO BOUNDARY LIST OF ACTIVE ELEMENT EJ */
/*     ---------+--------------------------------------------------------- */
/*     HEAD(D)  \ VJ => VJ HEAD OF D-LIST D */
/*              \  0 => NO VERTEX IN D-LIST D */

/*              \                  VI UNELIMINATED VERTEX */
/*              \       VI IN EK              \         VI NOT IN EK */
/*     ---------+-----------------------------+--------------------------- */
/*     NEXT(VI) \ UNDEFINED BUT NONNEGATIVE    \ VJ => VJ NEXT IN D-LIST */
/*              \                              \  0 => VI TAIL OF D-LIST */
/*     ---------+-----------------------------+--------------------------- */
/*     LAST(VI) \ (NOT SET UNTIL MDP)           \ -D => VI HEAD OF D-LIST D */
/*              \-VK => COMPUTE DEGREE          \ VJ => VJ LAST IN D-LIST */
/*              \ EJ => VI PROTOTYPE OF EJ      \  0 => VI NOT IN ANY D-LIST */
/*              \  0 => DO NOT COMPUTE DEGREE   \ */
/*     ---------+-----------------------------+--------------------------- */
/*     MARK(VI) \ MARK(VK)                      \ NONNEGATIVE TAG < MARK(VK) */


/*                 \                    VI ELIMINATED VERTEX */
/*                 \  EI ACTIVE ELEMENT        \              OTHERWISE */
/*     ---------+-----------------------------+--------------------------- */
/*     NEXT(VI) \ -J => VI WAS J-TH VERTEX     \ -J => VI WAS J-TH VERTEX */
/*                 \   TO BE ELIMINATED        \         TO BE ELIMINATED */
/*     ---------+-----------------------------+--------------------------- */
/*     LAST(VI) \  M => SIZE OF EI = M          \ UNDEFINED */
/*     ---------+-----------------------------+--------------------------- */
/*     MARK(VI) \ -M => OVERLAP COUNT OF EI     \ UNDEFINED */
/*                 \         WITH EK = M        \ */
/*                 \ OTHERWISE NONNEGATIVE TAG  \ */
/*                 \         < MARK(VK)         \ */

/* -----------------------------------------------------------------------
 */


/* ----INITIALIZATION */
     /* Parameter adjustments */
     --mark;
     --next;
     --last;
     --head;
     --l;
     --v;
     --ja;
     --ia;

     /* Function Body */
     tag = 0;
     mdi_(n, &ia[1], &ja[1], max_, &v[1], &l[1], &head[1], &last[1], &next[1], 
                &mark[1], &tag, flag_);
     if (*flag_ != 0) {
          return 0;
     }

     k = 0;
     dmin_ = 1;

/* ----WHILE  K < N  DO */
L1:
     if (k >= *n) {
          goto L4;
     }

/* ------SEARCH FOR VERTEX OF MINIMUM DEGREE */
L2:
     if (head[dmin_] > 0) {
          goto L3;
     }
     ++dmin_;
     goto L2;

/* ------REMOVE VERTEX VK OF MINIMUM DEGREE FROM DEGREE LIST */
L3:
     *vk = head[dmin_];
     head[dmin_] = next[*vk];
     if (head[dmin_] > 0) {
          last[head[dmin_]] = -dmin_;
     }

/* ------NUMBER VERTEX VK, ADJUST TAG, AND TAG VK */
     ++k;
     next[*vk] = -k;
     last[*ek] = dmin_ - 1;
     tag += last[*ek];
     mark[*vk] = tag; 

/* ------FORM ELEMENT EK FROM UNELIMINATED NEIGHBORS OF VK */
     mdm_(vk, &tail, &v[1], &l[1], &last[1], &next[1], &mark[1]);

/* ------PURGE INACTIVE ELEMENTS AND DO MASS ELIMINATION */
     mdp_(&k, ek, &tail, &v[1], &l[1], &head[1], &last[1], &next[1], &mark[1]);


/* ------UPDATE DEGREES OF UNELIMINATED VERTICES IN EK */
     mdu_(ek, &dmin_, &v[1], &l[1], &head[1], &last[1], &next[1], &mark[1]);

     goto L1;

/* ----GENERATE INVERSE PERMUTATION FROM PERMUTATION */
L4:
     i__1 = *n;
     for (k = 1; k <= i__1; ++k) {
          next[k] = -next[k];
/* L5: */
          last[next[k]] = k;
     }

     return 0;
} /* md_ */

#undef vk
#undef ek



/* *********************************************************************** */
/*  MDI -- INITIALIZATION */
/* *********************************************************************** */
/* Subroutine */ int mdi_(n, ia, ja, max_, v, l, head, last, next, mark, tag, 
          flag_)
integer *n, *ia, *ja, *max_, *v, *l, *head, *last, *next, *mark, *tag, *flag_;

{
     /* System generated locals */
     integer i__1, i__2;

     /* Local variables */
     STATIC integer jmin, jmax, j, vi, vj, dvi, sfs;


/* ----INITIALIZE DEGREES, ELEMENT LISTS, AND DEGREE LISTS */
     /* Parameter adjustments */
     --mark;
     --next;
     --last;
     --head;
     --l;
     --v;
     --ja;
     --ia;

     /* Function Body */
     i__1 = *n;
     for (vi = 1; vi <= i__1; ++vi) {
          mark[vi] = 1;
          l[vi] = 0;
/* L1: */
          head[vi] = 0;
     }
     sfs = *n + 1;

/* ----CREATE NONZERO STRUCTURE */
/* ----FOR EACH NONZERO ENTRY A(VI,VJ) IN STRICT UPPER TRIANGLE */
     i__1 = *n;
     for (vi = 1; vi <= i__1; ++vi) {
          jmin = ia[vi];
          jmax = ia[vi + 1] - 1;
          if (jmin > jmax) {
                goto L3;
          }
          i__2 = jmax;
          for (j = jmin; j <= i__2; ++j) {
                vj = ja[j];
                if (vi >= vj) {
                     goto L2;
                }
                if (sfs >= *max_) {
                     goto L101;
                }

/* ------ENTER VJ IN ELEMENT LIST FOR VI */
                ++mark[vi];
                v[sfs] = vj;
                l[sfs] = l[vi];
                l[vi] = sfs;
                ++sfs;

/* ------ENTER VI IN ELEMENT LIST FOR VJ */
                ++mark[vj];
                v[sfs] = vi;
                l[sfs] = l[vj];
                l[vj] = sfs;
                ++sfs;
L2:
                ;
          }
L3:
          ;
     }

/* ----CREATE DEGREE LISTS AND INITIALIZE MARK VECTOR */
     i__1 = *n;
     for (vi = 1; vi <= i__1; ++vi) {
          dvi = mark[vi];
          next[vi] = head[dvi];
          head[dvi] = vi;
          last[vi] = -dvi;
          if (next[vi] > 0) {
                last[next[vi]] = vi;
          }
/* L4: */
          mark[vi] = *tag;
     }

     return 0;

/* ** ERROR -- INSUFFICIENT STORAGE */
L101:
     *flag_ = *n * 9 + vi;
     return 0;
} /* mdi_ */


/* *********************************************************************** */
/*  MDM -- FORM ELEMENT FROM UNELIMINATED NEIGHBORS OF VK */
/* *********************************************************************** */
/* Subroutine */ int mdm_(vk, tail, v, l, last, next, mark)
integer *vk, *tail, *v, *l, *last, *next, *mark;
{
     /* System generated locals */
     integer i__1;
     STATIC integer equiv_0[1];

     /* Local variables */
     STATIC integer b, s, lb;
#define es (equiv_0)
     STATIC integer vb, ls;
#define vs (equiv_0)
     STATIC integer blpmax, tag, blp;


/* ----INITIALIZE TAG AND LIST OF UNELIMINATED NEIGHBORS */
     /* Parameter adjustments */
     --mark;
     --next;
     --last;
     --l;
     --v;

     /* Function Body */
     tag = mark[*vk];
     *tail = *vk;

/* ----FOR EACH VERTEX/ELEMENT VS/ES IN ELEMENT LIST OF VK */
     ls = l[*vk];
L1:
     s = ls;
     if (s == 0) {
          goto L5;
     }
     ls = l[s];
     *vs = v[s];
     if (next[*vs] < 0) {
          goto L2;
     }

/* ------IF VS IS UNELIMINATED VERTEX, THEN TAG AND APPEND TO LIST OF */
/* ------UNELIMINATED NEIGHBORS */
     mark[*vs] = tag;
     l[*tail] = s;
     *tail = s;
     goto L4;

/* ------IF ES IS ACTIVE ELEMENT, THEN ... */
/* --------FOR EACH VERTEX VB IN BOUNDARY LIST OF ELEMENT ES */
L2:
     lb = l[*es];
     blpmax = last[*es];
     i__1 = blpmax;
     for (blp = 1; blp <= i__1; ++blp) {
          b = lb;
          lb = l[b];
          vb = v[b];

/* ----------IF VB IS UNTAGGED VERTEX, THEN TAG AND APPEND TO LIST OF 
*/
/* ----------UNELIMINATED NEIGHBORS */
          if (mark[vb] >= tag) {
                goto L3;
          }
          mark[vb] = tag;
          l[*tail] = b;
          *tail = b;
L3:
          ;
     }

/* --------MARK ES INACTIVE */
     mark[*es] = tag;

L4:
     goto L1;

/* ----TERMINATE LIST OF UNELIMINATED NEIGHBORS */
L5:
     l[*tail] = 0;

     return 0;
} /* mdm_ */

#undef vs
#undef es



/* *********************************************************************** */
/*  MDP -- PURGE INACTIVE ELEMENTS AND DO MASS ELIMINATION */
/* *********************************************************************** */
/* Subroutine */ int mdp_(k, ek, tail, v, l, head, last, next, mark)
integer *k, *ek, *tail, *v, *l, *head, *last, *next, *mark;
{
     /* System generated locals */
     integer i__1;

     /* Local variables */
     STATIC integer ofree, i, s, li, es, vi, ls, ilpmax, tag, evi, ilp, lvi;


/* ----INITIALIZE TAG */
     /* Parameter adjustments */
     --mark;
     --next;
     --last;
     --head;
     --l;
     --v;

     /* Function Body */
     tag = mark[*ek];

/* ----FOR EACH VERTEX VI IN EK */
     li = *ek;
     ilpmax = last[*ek];
     if (ilpmax <= 0) {
          goto L12;
     }
     i__1 = ilpmax;
     for (ilp = 1; ilp <= i__1; ++ilp) {
          i = li;
          li = l[i];
          vi = v[li];

/* ------REMOVE VI FROM DEGREE LIST */
          if (last[vi] == 0) {
                goto L3;
          }
          if (last[vi] > 0) {
                goto L1;
          }
          head[-last[vi]] = next[vi];
          goto L2;
L1:
          next[last[vi]] = next[vi];
L2:
          if (next[vi] > 0) {
                last[next[vi]] = last[vi];
          }

/* ------REMOVE INACTIVE ITEMS FROM ELEMENT LIST OF VI */
L3:
          ls = vi;
L4:
          s = ls;
          ls = l[s];
          if (ls == 0) {
                goto L6;
          }
          es = v[ls];
          if (mark[es] < tag) {
                goto L5;
          }
          ofree = ls;
          l[s] = l[ls];
          ls = s;
L5:
          goto L4;

/* ------IF VI IS INTERIOR VERTEX, THEN REMOVE FROM LIST AND ELIMINATE
 */
L6:
          lvi = l[vi];
          if (lvi != 0) {
                goto L7;
          }
          l[i] = l[li];
          li = i;

          ++(*k);
          next[vi] = -(*k);
          --last[*ek];
          goto L11;

/* ------ELSE ... */
/* --------CLASSIFY VERTEX VI */
L7:
          if (l[lvi] != 0) {
                goto L9;
          }
          evi = v[lvi];
          if (next[evi] >= 0) {
                goto L9;
          }
          if (mark[evi] < 0) {
                goto L8;
          }

/* ----------IF VI IS PROTOTYPE VERTEX, THEN MARK AS SUCH, INITIALIZE 
*/
/* ----------OVERLAP COUNT FOR CORRESPONDING ELEMENT, AND MOVE VI TO E
ND */
/* ----------OF BOUNDARY LIST */
          last[vi] = evi;
          mark[evi] = -1;
          l[*tail] = li;
          *tail = li;
          l[i] = l[li];
          li = i;
          goto L10;

/* ----------ELSE IF VI IS DUPLICATE VERTEX, THEN MARK AS SUCH AND ADJ
UST */
/* ----------OVERLAP COUNT FOR CORRESPONDING ELEMENT */
L8:
          last[vi] = 0;
          --mark[evi];
          goto L10;

/* ----------ELSE MARK VI TO COMPUTE DEGREE */
L9:
          last[vi] = -(*ek);

/* --------INSERT EK IN ELEMENT LIST OF VI */
L10:
          v[ofree] = *ek;
          l[ofree] = l[vi];
          l[vi] = ofree;
L11:
          ;
     }

/* ----TERMINATE BOUNDARY LIST */
L12:
     l[*tail] = 0;

     return 0;
} /* mdp_ */


/* *********************************************************************** */
/*  MDU -- UPDATE DEGREES OF UNELIMINATED VERTICES IN EK */
/* *********************************************************************** */
/* Subroutine */ int mdu_(ek, dmin_, v, l, head, last, next, mark)
integer *ek, *dmin_, *v, *l, *head, *last, *next, *mark;
{
     /* System generated locals */
     integer i__1, i__2;
     STATIC integer equiv_0[1];

     /* Local variables */
     STATIC integer b, i, s;
#define es (equiv_0)
     STATIC integer vb, vi;
#define vs (equiv_0)
     STATIC integer blpmax, ilpmax, tag, blp, dvi, evi, ilp;


/* ----INITIALIZE TAG */
     /* Parameter adjustments */
     --mark;
     --next;
     --last;
     --head;
     --l;
     --v;

     /* Function Body */
     tag = mark[*ek] - last[*ek];

/* ----FOR EACH VERTEX VI IN EK */
     i = *ek;
     ilpmax = last[*ek];
     if (ilpmax <= 0) {
          goto L11;
     }
     i__1 = ilpmax;
     for (ilp = 1; ilp <= i__1; ++ilp) {
          i = l[i];
          vi = v[i];
          if ((i__2 = last[vi]) < 0) {
                goto L1;
          } else if (i__2 == 0) {
                goto L10;
          } else {
                goto L8;
          }

/* ------IF VI NEITHER PROTOTYPE NOR DUPLICATE VERTEX, THEN MERGE ELEM
ENTS */
/* ------TO COMPUTE DEGREE */
L1:
          ++tag;
          dvi = last[*ek];

/* --------FOR EACH VERTEX/ELEMENT VS/ES IN ELEMENT LIST OF VI */
          s = l[vi];
L2:
          s = l[s];
          if (s == 0) {
                goto L9;
          }
          *vs = v[s];
          if (next[*vs] < 0) {
                goto L3;
          }

/* ----------IF VS IS UNELIMINATED VERTEX, THEN TAG AND ADJUST DEGREE 
*/
          mark[*vs] = tag;
          ++dvi;
          goto L5;

/* ----------IF ES IS ACTIVE ELEMENT, THEN EXPAND */
/* ------------CHECK FOR OUTMATCHED VERTEX */
L3:
          if (mark[*es] < 0) {
                goto L6;
          }

/* ------------FOR EACH VERTEX VB IN ES */
          b = *es;
          blpmax = last[*es];
          i__2 = blpmax;
          for (blp = 1; blp <= i__2; ++blp) {
                b = l[b];
                vb = v[b];

/* --------------IF VB IS UNTAGGED, THEN TAG AND ADJUST DEGREE */
                if (mark[vb] >= tag) {
                     goto L4;
                }
                mark[vb] = tag;
                ++dvi;
L4:
                ;
          }

L5:
          goto L2;

/* ------ELSE IF VI IS OUTMATCHED VERTEX, THEN ADJUST OVERLAPS BUT DO 
NOT */
/* ------COMPUTE DEGREE */
L6:
          last[vi] = 0;
          --mark[*es];
L7:
          s = l[s];
          if (s == 0) {
                goto L10;
          }
          *es = v[s];
          if (mark[*es] < 0) {
                --mark[*es];
          }
          goto L7;

/* ------ELSE IF VI IS PROTOTYPE VERTEX, THEN CALCULATE DEGREE BY */
/* ------INCLUSION/EXCLUSION AND RESET OVERLAP COUNT */
L8:
          evi = last[vi];
          dvi = last[*ek] + last[evi] + mark[evi];
          mark[evi] = 0;

/* ------INSERT VI IN APPROPRIATE DEGREE LIST */
L9:
          next[vi] = head[dvi];
          head[dvi] = vi;
          last[vi] = -dvi;
          if (next[vi] > 0) {
                last[next[vi]] = vi;
          }
          if (dvi < *dmin_) {
                *dmin_ = dvi;
          }

L10:
          ;
     }

L11:
     return 0;
} /* mdu_ */

#undef vs
#undef es


/* *********************************************************************** */
/* *********************************************************************** */
/* *********************************************************************** */
/*  SRO -- SYMMETRIC REORDERING OF SPARSE SYMMETRIC MATRIX */
/* *********************************************************************** */
/* Subroutine */ int sro_(n, ip, ia, ja, a, q, r, dflag)
integer *n, *ip, *ia, *ja;
doublereal *a;
integer *q, *r;
logical *dflag;
{
     /* System generated locals */
     integer i__1, i__2;

     /* Local variables */
     STATIC integer jmin, jmax, i, j, k, ilast;
     STATIC doublereal ak;
     STATIC integer jdummy, jak;


/*  DESCRIPTION */

/*     THE NONZERO ENTRIES OF THE MATRIX M ARE ASSUMED TO BE STORED */
/*     SYMMETRICALLY IN (IA,JA,A) FORMAT (I.E., NOT BOTH M(I,J) AND M(J,I) 
*/
/*     ARE STORED IF I NE J). */

/*     SRO DOES NOT REARRANGE THE ORDER OF THE ROWS, BUT DOES MOVE */
/*     NONZEROES FROM ONE ROW TO ANOTHER TO ENSURE THAT IF M(I,J) WILL BE 
*/
/*     IN THE UPPER TRIANGLE OF M WITH RESPECT TO THE NEW ORDERING, THEN */

/*     M(I,J) IS STORED IN ROW I (AND THUS M(J,I) IS NOT STORED);  WHEREAS 
*/
/*     IF M(I,J) WILL BE IN THE STRICT LOWER TRIANGLE OF M, THEN M(J,I) IS 
*/
/*     STORED IN ROW J (AND THUS M(I,J) IS NOT STORED). */


/*  ADDITIONAL PARAMETERS */

/*     Q      - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = N */

/*     R      - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = NUMBER OF 
*/
/*                NONZERO ENTRIES IN THE UPPER TRIANGLE OF M */

/*     DFLAG - LOGICAL VARIABLE;  IF DFLAG = .TRUE., THEN STORE NONZERO */
/*                DIAGONAL ELEMENTS AT THE BEGINNING OF THE ROW */

/* -----------------------------------------------------------------------
 */

/*         REAL  A(1),  AK */


/* --PHASE 1 -- FIND ROW IN WHICH TO STORE EACH NONZERO */
/* ----INITIALIZE COUNT OF NONZEROES TO BE STORED IN EACH ROW */
     /* Parameter adjustments */
     --r;
     --q;
     --a;
     --ja;
     --ia;
     --ip;

     /* Function Body */
     i__1 = *n;
     for (i = 1; i <= i__1; ++i) {
/* L1: */
          q[i] = 0;
     }

/* ----FOR EACH NONZERO ELEMENT A(J) */
     i__1 = *n;
     for (i = 1; i <= i__1; ++i) {
          jmin = ia[i];
          jmax = ia[i + 1] - 1;
          if (jmin > jmax) {
                goto L3;
          }
          i__2 = jmax;
          for (j = jmin; j <= i__2; ++j) {

/* --------FIND ROW (=R(J)) AND COLUMN (=JA(J)) IN WHICH TO STORE 
A(J) ... */
                k = ja[j];
                if (ip[k] < ip[i]) {
                     ja[j] = i;
                }
                if (ip[k] >= ip[i]) {
                     k = i;
                }
                r[j] = k;

/* --------... AND INCREMENT COUNT OF NONZEROES (=Q(R(J)) IN THAT 
ROW */
/* L2: */
                ++q[k];
          }
L3:
          ;
     }


/* --PHASE 2 -- FIND NEW IA AND PERMUTATION TO APPLY TO (JA,A) */
/* ----DETERMINE POINTERS TO DELIMIT ROWS IN PERMUTED (JA,A) */
     i__1 = *n;
     for (i = 1; i <= i__1; ++i) {
          ia[i + 1] = ia[i] + q[i];
/* L4: */
          q[i] = ia[i + 1];
     }

/* ----DETERMINE WHERE EACH (JA(J),A(J)) IS STORED IN PERMUTED (JA,A) */
/* ----FOR EACH NONZERO ELEMENT (IN REVERSE ORDER) */
     ilast = 0;
     jmin = ia[1];
     jmax = ia[*n + 1] - 1;
     j = jmax;
     i__1 = jmax;
     for (jdummy = jmin; jdummy <= i__1; ++jdummy) {
          i = r[j];
          if (! (*dflag) || ja[j] != i || i == ilast) {
                goto L5;
          }

/* ------IF DFLAG, THEN PUT DIAGONAL NONZERO AT BEGINNING OF ROW */
          r[j] = ia[i];
          ilast = i;
          goto L6;

/* ------PUT (OFF-DIAGONAL) NONZERO IN LAST UNUSED LOCATION IN ROW */
L5:
          --q[i];
          r[j] = q[i];

L6:
          --j;
     }


/* --PHASE 3 -- PERMUTE (JA,A) TO UPPER TRIANGULAR FORM (WRT NEW ORDERING)
 */
     i__1 = jmax;
     for (j = jmin; j <= i__1; ++j) {
L7:
          if (r[j] == j) {
                goto L8;
          }
          k = r[j];
          r[j] = r[k];
          r[k] = k;
          jak = ja[k];
          ja[k] = ja[j];
          ja[j] = jak;
          ak = a[k];
          a[k] = a[j];
          a[j] = ak;
          goto L7;
L8:
          ;
     }

     return 0;
} /* sro_ */

