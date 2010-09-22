/************************************************************* 
*  This file is part of the Surface Evolver source code.     *
*  Programmer:  Ken Brakke, brakke@susqu.edu                 *
*************************************************************/

/******************************************************************
*
*  File: help.c
* 
*  Purpose: Print help screens.
*/

#include "include.h"
#include "ytab.h"

void do_line ARGS(( char *));

/*******************************************************************
*
*  Function: main_help()
*
*  Purpose:  Help screen for main menu.
*/

/* strings to print 2 per line */
char *help_strings[] = {
" A  Set adjustable parameters.",
" b  Set body volumes or pressures.",
" C  Run consistency checks.",
" c  Report count of elements.",
" D  Toggle display every iteration.",
" d  Dump surface to datafile.",
" G  Set gravity.",
" g nnn     Go nnn iterations.",
" H,h,? This help screen.",
" i  Information on status.",
" l  Subdivide long edges.",
" m  Toggle fixed motion scale.",
" P  Graphics display file, geomview.",
" q,x    Exit.",
" r  Refine triangulation.",
" s  Built-in screen graphics.",
" t  Remove tiny edges.",
" U  Toggle conjugate gradient method.",
" u  Equiangulate.",
" V  Vertex averaging.",
" v  Report volumes.",
" w  Weed out small triangles."
 };

char *qhelp[]={
"\n",
"Operators: +,-,*,/,^n,**,==,!=,>,<,>=,<=,NOT,!,AND,&&,OR,||.\n",
"Colors: black,red,green,blue,cyan,magenta,brown,darkgray,lightgray,\n",
" lightblue,lightgreen,lightred,lightcyan,lightmagenta,yellow,white.\n",
"Examples of the full command language:\n",
"  list edges | \"more\"; list topinfo >>> \"topstuff\"\n",
"  refine edges where not fixed and length > .23\n",
"  set facet color green where original == 1 or original == 2\n",
"  list vertices where x^2 + y^2 > 1\n",
"  show facets where area < .01\n",
"\n",
 NULL};

/**********************************************************************
*
* function: main_help()
*
* purpose: handle 'h' command, print brief help page on screen.
*
*/

void main_help()
{
  int n = sizeof(help_strings)/sizeof(char *);
  int i,j;

  outstring("\nFor detailed help, use \"help topic\".  This will also\n");
  outstring("give a list of help topics containing your topic word.\n");
  outstring("Use quotes around your topic if necessary.\n");
  outstring("\nSome common one-letter commands: \n");
  for ( i = 0, j = (n+1)/2 ; i < (n+1)/2 ; i++,j++ )
     { sprintf(msg,"%-38.38s ",help_strings[i]);
        outstring(msg);
        if ( j < n )
          sprintf(msg,"%-38.38s\n",help_strings[j]);
        else sprintf(msg,"\n");
        outstring(msg);
     }

#ifdef NOPIPE
  /* fake `more' */
  for ( i = 0 ; qhelp[i] ; )
  {  outstring("Hit RETURN for more.");
      getstring(msg,msgmax);
      for ( j = 0 ; qhelp[i] && (j<23) ; i++,j++ )
         outstring(qhelp[i]);
  }
#else
  for ( i = 0 ; qhelp[i] ; i++ )
     outstring(qhelp[i]);
#endif
}

/*********************************************************************
*
*  Function: graph_help()
*
*  Purpose:  Display help screen for graphics mode.
*/

char *ghelp_strings[] =
{
"View transformation commands are strings of letters. Each letter ",
"causes an action and may be preceded by an integer repetition count, ",
"or a real value (if appropriate; use a decimal point):",
" u    Rotate up 6 degrees.",
" d    Rotate down 6 degrees",
" r    Rotate right 6 degrees.",
" l    Rotate left 6 degrees.",
" c    Rotate clockwise 6 degrees.",
" C    Rotate counterclockwise 6 degrees.",
" z    Zoom by factor of 1.2.",
" s    Shrink by factor of 1.2.",
" arrow keys - Translate image.",
" m    Center image.",
" R    Reset viewing parameters.",
" T    Toggle additional viewing transforms.",
" e    Toggle facet edge drawing.",
" B    Toggle display of boundary facets.",
" v    Toggle ridge and valley coloring.",
" w    Toggle facets with three vertices on constraints.",
" b    Draw bounding box.",
" +,- Increment, decrement fill color.",
" H    Toggle hidden surface removal.",
" t    Set clipping mode for torus.",
" ?,h help (this display)",
" x,q Exit to main menu."
};
                          
void graph_help()
{
  int n = sizeof(ghelp_strings)/sizeof(char *);
  int i;

  for ( i = 0 ; i < n ; i++ )
     { outstring(ghelp_strings[i]); outstring("\n"); }
}

/*********************************************************************************
*
*  function: printbuff()
*
*  purpose: massage and print HTML text on screen.
*
*/

static char buff[30000];  /* text buffer */
#define LINESIZE 75
static int pre_flag;
void printbuff ARGS((void));

void printbuff()
{ char *spot,*s,*d;
  /* do HTML special characters */
  if ( buff[0] == 0 ) return;
  spot = buff;
  while ( (spot = strchr(spot,'&')) != NULL )
  { s = spot + 1;
     if ( kb_strnicmp(spot+1,"lt;",3) == 0 ) { *spot = '<'; s = spot+4; }
     if ( kb_strnicmp(spot+1,"gt;",3) == 0 ) { *spot = '>'; s = spot+4; }
     if ( kb_strnicmp(spot+1,"amp;",4) == 0 ) { *spot = '&'; s = spot+5; }
     if ( kb_strnicmp(spot+1,"quot;",5) == 0 ) { *spot = '"'; s = spot+6; }
     for ( d = spot+1 ; *s ; s++,d++ ) *d = *s;
     *d = 0;
     spot++;
  }
  if ( pre_flag ) { outstring(buff); outstring("\n"); }
  else
  { char *cur;  /* cursor */
     char *b; /* searching for blank */

     char *src,*dest;

     /* compress whitespace */
     if ( pre_flag == 0 )
     {
        for ( src = buff; *src ; src++ ) if ( *src == '\t' ) *src = ' ';
        src = buff; while ( *src == ' ' ) src++ ;
        for ( dest = buff, src = buff; *src ; src++ )
        { if ( (*src == ' ') && (dest[-1] == ' ') ) continue;
          else *(dest++) = *src;
        }
     } else dest = buff + strlen(buff);
     *dest = 0; dest[1] = 0;

     /* print lines */
     for ( cur = buff; *cur ; cur = b+1 )
     { /* first, scan for newline */ 
        for ( b = cur; *b && (b < cur+LINESIZE) ; b++ )
          if ( *b == '\n' ) break;
        if ( *b == '\n' )
        { *b = 0; outstring(cur); outstring("\n"); continue; }

        /* now get chunk small enough to fit on a line */                    
        b = cur + LINESIZE;
        if ( b < dest ) 
        {
          while ( (*b != ' ') && (b > cur) ) b--;
          if ( b == cur ) { b = cur + LINESIZE; }
        }
        else b = dest;
        *b = 0;
        outstring(cur); outstring("\n");
     }
  }
  buff[0] = 0;
}

/******************************************************************
*
* function: do_line()
*
* purpose: add line to buffer, parsing and printing buffer if called for 
*
*/

static int ul_level;

void do_line(line)
char *line;
{ 
  char *spot,*c,*b,*bb;

  spot = line; 
  do
  { 
     b = strchr(spot,'<'); 
     if ( b ) 
     { bb = strchr(b+1,'>');
        if ( bb == NULL ) b = NULL;
        else if ( pre_flag && b[1] == ' ' ) b = NULL;  /* inequality sign */
        else *b = 0;
     }
     strcat(buff,spot);
     if ( b == NULL ) break;
     spot = b + 1;
     b = strchr(spot,'>'); 
     if ( b ) 
       *b = 0;
     c = strchr(spot,' ');
     if ( c ) *c = 0;
     if ( kb_stricmp(spot,"br") == 0 ) printbuff();
     else if (kb_stricmp(spot,"sub") == 0 ) {strcat(buff,"_"); }
     else if (kb_stricmp(spot,"sup") == 0 ) {strcat(buff,"^"); }
     else if (kb_stricmp(spot,"p") == 0 ) {strcat(buff,"\n"); printbuff();}
     else if (kb_strnicmp(spot,"/h",2) == 0 ) {strcat(buff,"\n"); printbuff();}
     else if (kb_stricmp(spot,"li") == 0 ) 
     { int n; 
       printbuff(); 
       strcat(buff,"  "); 
       for ( n = 0 ; n < ul_level ; n++ ) 
         strcat(buff,"> ");
     }
     else if (kb_stricmp(spot,"dt") == 0 ) {printbuff();}
     else if (kb_stricmp(spot,"dd") == 0 ) {strcat(buff,": ");}
     else if (kb_stricmp(spot,"ul") == 0 ) {ul_level++;}
     else if (kb_stricmp(spot,"/ul") == 0 ) 
         {ul_level--; strcat(buff,"\n"); printbuff();}
     else if (kb_stricmp(spot,"/ol") == 0 ) {strcat(buff,"\n"); printbuff();}
     else if (kb_stricmp(spot,"/dl") == 0 ) {strcat(buff,"\n"); printbuff();}
     else if ( kb_stricmp(spot,"pre") == 0 )
     { strcat(buff,"\n");
        printbuff();
        pre_flag = 1;
     }
     else if ( kb_stricmp(spot,"/pre") == 0 )
     { printbuff(); pre_flag = 0; }
     else if ( kb_stricmp(spot,"tr") == 0 ) printbuff();
     spot = b+1;
  }
  while ( *spot );
  if ( pre_flag ) strcat(buff,"\n");
}

/**************************************************************************
*
* function: old_html_help()
*
* Purpose: old fashioned extraction of help from html doc files,
*          hopefully superceded by new_text_help()
*
*/
#define RELMAX 30
static struct relinfo { char filename[20];
                      char name[40]; } info[RELMAX];

static char name[100] = "name=\"";

void old_html_help(keyword, /* string to look for */
                  found)   /* flag, set if previously found, and want related */
char *keyword;
int found;
{ 
  int printflag = 0;
  int relcount = 0;
  int linecount = 0;
  char helpfilename[PATHSIZE];
  FILE *fd;
  char line[1000];
  int n;

  /* now check html doc files */ 

  helpfilename[0] = 0;
  fd = path_open("index.htm",NOTDATAFILENAME);
  if ( fd == NULL )
  { erroutstring(
     "Cannot open index.htm.  Are the *.htm doc files on EVOLVERPATH?\n");
    return; 
  }

  /* skip header stuff */
  while ( fgets(line,sizeof(line)-1,fd) )
     if ( strstr(line,"<a name=\"A\">") ) break;

  while ( fgets(line,sizeof(line)-1,fd)  )
  { char *sharp = strchr(line,'#');
    if ( sharp == NULL ) continue;
    *strchr(sharp,'"') = 0;
    if ( stricmp(sharp+1,keyword) == 0 )
    { 
      *sharp = 0;
      strcpy(helpfilename,strchr(line,'"')+1);
    }
    else if ( strstr(sharp+1,keyword) && (relcount < RELMAX) )
    { strcpy(info[relcount].filename,strchr(line,'"')+1);
      strcpy(info[relcount].name,sharp+1);
      relcount++;
    }
  }
  fclose(fd);
  if ( found ) goto print_related;
  if ( helpfilename[0] == 0 )
  { sprintf(msg,"Cannot find help entry for \"%s\". \n", keyword);
    outstring(msg);
    goto print_related;
  }


  fd = path_open(helpfilename,NOTDATAFILENAME);
  if ( fd == NULL )
  { perror(helpfilename); return; }

  ul_level = 0;
  strcpy(name+6,keyword); strcat(name,"\"");
  while ( fgets(line,sizeof(line)-1,fd)  )
  {
     if ( strstr(line,name) ) printflag = 1;
     else if ( (printflag==1) && 
        ( ((strstr(line,"<h") || strstr(line,"<H")) && (linecount>2) )
              || strstr(line,"</dd>") || strstr(line,"EndName") ))
     { break; }

     strtok(line,"\r\n"); /* remove trailing newline */
     strcat(line," ");
     if ( printflag )
     { if ( !strstr(line,"<a name=") ) linecount++; 
        do_line(line); 
     }
  }
  printbuff();
  fclose(fd);

print_related:

  if ( relcount && (strlen(keyword) > 1) )
  { outstring("Possibly related entries:\n");
     for ( n = 0 ; n < relcount ; n+=2 ) 
     { if ( n < relcount-1 )
          sprintf(msg," %-32s %s\n",info[n].name,info[n+1].name);
       else sprintf(msg," %-32s \n",info[n].name);
       outstring(msg);
     }
  }
}

/***************************************************************************
*
* Function: new_text_help()
*
* Purpose: print help from text help file
*
* Return: 0 if can't find help file
*         1 if found help file
*/

char *text_help_file = "evhelp.txt";

int new_text_help(keyword, /* string to look for */
                  found)   /* flag, set if previously found, and want related */
char *keyword;
int found;
{ FILE *fd;
  int keylen = strlen(keyword);
  char line[1000];
  int i=0,n;
  int relcount = 0;

  /* now check text doc file */ 

  fd = path_open(text_help_file,NOTDATAFILENAME);
  if ( fd == NULL )
  { perror(text_help_file);
    kb_error(1902,"Is the doc subdirectory on EVOLVERPATH?\n",WARNING);
    return 0; 
  }

  /* Find keyword in  <--- keyword ---> line */
  while ( fgets(line,sizeof(line)-1,fd)  )
  { char *keyspot;
    if ( strncmp(line,"<---",4) != 0 ) continue;
#ifndef MPI_EVOLVER
    if ( strstr(line,"MPI Evolver") )
      continue;
#endif
    keyspot = strstr(line,keyword);
    if ( keyspot == NULL ) continue;

    if ( (keyspot[-2] == '-') && (keyspot[keylen+2] == '-') )
    { int lines_done = 0;
      int blanklines = 0;
      /* found exact match */
      found = 1;   

      /* print nice title bar */
      outstring("<");
      for ( i = 1 ; i < (70 - keylen)/2 ; i++ )
        outstring("-");
      outstring(" ");
      outstring(keyword);
      outstring(" ");
      for ( i = (70 + keylen)/2 ; i < 70 ;  i++ )
        outstring("-");
      outstring(">\n");
      
      /* read and echo info */
      while ( fgets(line,sizeof(line)-1,fd)  )
      { if ( line[0] != '<' )       
        { if ( line[1] == 0 )
          { blanklines++;
            if ( blanklines < 2 )
               outstring(line);
          }
          else 
          { outstring(line);
            blanklines = 0;
          }
          lines_done++;
        }
        else 
        { /* skip if just extra title */
          if ( lines_done > 0 )
             break;
        }        
      }

      /* next title line falls through to relevance recording */
    }
    if ( strstr(line,keyword) && (relcount < RELMAX) )
    { char *spot = strchr(line,' '); 
      int spaceflag = 0;
      if ( spot )
         for ( spot++, i = 0 ; !((spot[0] == ' ') && (spot[1] == '-')) ; spot++, i++ )
         { info[relcount].name[i] = *spot;
           if ( *spot == ' ' ) 
             spaceflag = 1;
         }
      if ( spaceflag ) /* enclose in quotes */
      { char c;
        c = info[relcount].name[0];
        info[relcount].name[0] = '"';
        for ( n = 1 ; n < i ; n++ )
        { char cc = info[relcount].name[n];
          info[relcount].name[n] = c;
          c = cc;
        }
        info[relcount].name[i] = c;
        info[relcount].name[i+1] = '"';
        i += 2;
      }
      info[relcount].name[i] = 0;
      relcount++;
    }
  }
  fclose(fd);
  if ( !found ) 
  { sprintf(msg,"Cannot find help entry for \"%s\". \n", keyword);
    outstring(msg);
  }

  if ( relcount && (strlen(keyword) > 1) )
  { outstring("Possibly related entries:\n");
    for ( n = 0 ; n < relcount ; n+=2 ) 
     { if ( n < relcount-1 )
          sprintf(msg," %-32s %s\n",info[n].name,info[n+1].name);
       else sprintf(msg," %-32s \n",info[n].name);
       outstring(msg);
     }
  }
  fclose(fd);
  return 1;
}

/***************************************************************************
*
* function: keyword_help()
*
* purpose: Print help for keyword.
*/

void keyword_help(keyword)
char *keyword;
{ 
  int type;
  int found = 0;
  int entry;
 
#ifndef NOPIPE
  if ( outfd == stdout )
  { outfd = popen("more","w");
    if ( outfd == NULL )
    { perror("more");
      outfd = stdout;
    }
  }
#endif

  if ( keyword == NULL ) 
  { erroutstring("Usage: help keyword or help \"phrase\" \n"); 
     goto help_exit; 
  }
  if ( isdigit(keyword[0]) ) { error_help(keyword); goto help_exit; }

  /* check for user-defined names */

  /* search symbol table */
  if ( symbol_lookup(keyword) )
  { sprintf(msg,"\n%s: user-defined element name.\n\n",keyword);
    outstring(msg);
    found = 1;
    goto help_exit;
  } 

  /* search parameter names */
  entry = lookup_global_hash(keyword,0,0,HASH_LOOK);
  if ( entry != -1 )
    switch   ( entry & NAMETYPEMASK )
    { 
      case VARIABLENAME:
      { struct global *gp = globals((entry & INDEXMASK)|EPHGLOBAL);
        if ( gp->flags & (SUBROUTINE|PROCEDURE_NAME) )
        {  sprintf(msg,"\n%s: user-defined procedure.  Prototype:\n\n   ",keyword);
           outstring(msg);
           list_procedure_proto(gp);
           outstring("\n");
        } 
        else if ( gp->flags & FUNCTION_NAME )
        {  sprintf(msg,"\n%s: user-defined function.  Prototype:\n\n   ",keyword);
           outstring(msg);
           list_function_proto(gp);
           outstring("\n");
        } 
        else if ( gp->flags & STRINGVAL )
        {  sprintf(msg,"\n%s: user-defined string variable.\n\n",keyword);
           outstring(msg);
        } 
        else if ( gp->flags & QUANTITY_NAME )
        {  sprintf(msg,"\n%s: user-defined named quantity.\n\n",keyword);
           outstring(msg);
        } 
        else if ( gp->flags & METHOD_NAME )
        {  sprintf(msg,"\n%s: user-defined named method.\n\n",keyword);
           outstring(msg);
        } 
        else if ( gp->flags & CONSTRAINT_NAME )
        {  sprintf(msg,"\n%s: user-defined named constraint.\n\n",keyword);
           outstring(msg);
        } 
        else if ( gp->flags & BOUNDARY_NAME )
        {  sprintf(msg,"\n%s: user-defined named boundary.\n\n",keyword);
           outstring(msg);
        } 
        else if ( gp->flags & DYNAMIC_LOAD_FUNC )
        {  sprintf(msg,"\n%s: dynamic load library function.\n\n",keyword);
           outstring(msg);
        } 
        else if ( gp->flags & ARRAY_PARAM )
        { struct array *a = gp->attr.arrayptr;
          if ( a )
          { sprintf(msg,"\n%s: user-defined array of type %s",
                     keyword,datatype_name[a->datatype]);
            outstring(msg);
            if ( a->dim > 1 || a->sizes[0] > 1 )
            { int i;
              for ( i = 0 ; i < a->dim ; i++ )
              { sprintf(msg,"[%d]",a->sizes[i]);
                outstring(msg);
              }
            }
            outstring(".\n\n");
          }
          else
          { sprintf(msg,"\n%s: user-defined array; declaration not yet executed.\n\n",
                     keyword);
            outstring(msg);
          }
        } 
        else 
        { if ( gp->flags & OPTIMIZING_PARAMETER )
           sprintf(msg,"\n%s: user-defined optimizing parameter\n\n", keyword);
          else if ( gp->type )
           sprintf(msg,"\n%s: user-defined parameter of type %s\n\n", keyword,
                datatype_name[gp->type]);
          else
           sprintf(msg,"\n%s: user-defined parameter (numeric variable)\n\n", 
              keyword);
          outstring(msg);
        }
        found = 1;
        goto help_exit;
      }

        break;
      
      case PERM_NAME:
        { struct global *gg = globals((entry & INDEXMASK)|PERMGLOBAL);
          if ( gg->flags & (SUBROUTINE|PROCEDURE_NAME) )
          {  sprintf(msg,"\n%s: permanent user-defined procedure. Prototype:\n\n   ",
               keyword);
             outstring(msg);
             list_procedure_proto(gg);
             outstring("\n");
          } 
          else if ( gg->flags & FUNCTION_NAME )
          {  sprintf(msg,"\n%s: permanent user-defined function. Prototype:\n\n   ",
               keyword);
             outstring(msg);
             list_function_proto(gg);
             outstring("\n");
          } 
          else if ( gg->flags & STRINGVAL )
          {  sprintf(msg,"\n%s: permanent user-defined string variable.\n\n",keyword);
             outstring(msg);
          } 
          else if ( gg->flags & INTERNAL_NAME )
          {  
             if ( gg->flags & ARRAY_PARAM )
             sprintf(msg,"\n%s: permanent internal %s array.\n\n",keyword,
                    datatype_name[gg->attr.arrayptr->datatype]);
             else sprintf(msg,"\n%s: permanent internal variable.\n\n",keyword);
             outstring(msg);
             found = 0; /* so prints out regular documentation */
             goto help_exit;
          } 
          else 
          { sprintf(msg,"\n%s: permanent user-defined parameter (numeric variable)\n\n",
               keyword);
            outstring(msg);
          }
          found = 1;
          goto help_exit;
        }
     break;

      case QUANTITYNAME:
      { struct gen_quant *g = GEN_QUANT(entry & INDEXMASK);
        sprintf(msg,"\n%s: user-defined named quantity.\n\n",g->name);
        outstring(msg);
        found = 1;
        goto help_exit;} 
        break;

      case METHODNAME:
      { struct method_instance *mi = METH_INSTANCE(entry & INDEXMASK);
        sprintf(msg,"\n%s: user-defined named method, belongs to quantity %s.\n\n",   
        keyword, GEN_QUANT(mi->quant)->name);
        outstring(msg);
        found = 1;
        goto help_exit;} 
        break;
    }
  
    /* search extra attributes */
  for ( type = 0 ; type <= BODY ; type++ )
  { struct extra *ex;
    int i,k;

    for ( i = 0, ex = EXTRAS(type) ;
             i < web.skel[type].extra_count ; i++ , ex++ )
      if ( stricmp(keyword, ex->name) == 0 )
      { sprintf(msg,"\n%s: %s attribute, type %s, ",
              keyword,typenames[type],datatype_name[ex->type]);
        if ( ex->array_spec.dim == 0 ) strcat(msg,"scalar");
        else strcat(msg," dimension ");
        for ( k = 0 ; k < ex->array_spec.dim ; k++ )
           sprintf(msg+strlen(msg),"[%d]",ex->array_spec.sizes[k]);
        strcat(msg,"\n\n");
        outstring(msg); 
        found = 1;
        goto help_exit;
      } 
  }
   
help_exit:

/* find_related: */

  /* Go to help file, even if found, for related entries */
 // if ( !found )
    if ( !new_text_help(keyword,found) && !found )
      old_html_help(keyword,found);


#ifndef NOPIPE
    pclose(outfd);
    outfd = stdout;
#else
  ;
#endif

} /* end keyword_help() */

/***************************************************************************
*
* function: error_help()
*
* purpose: Print help for given error number from error.hlp.
*
*  Nonfunctional yet.
*/
void error_help(keyword)
char *keyword;  /* with error number as string */
{ 
  outstring("Help by error number not available.\n");
#ifdef HAVEHLP
  FILE *fd;
  char line[200];
  int errnum = atoi(keyword);
  int flag = 0; /* if found */

  fd = path_open("error.hlp",NOTDATAFILENAME);
  if ( fd == NULL )
  { erroutstring("Cannot open error.hlp. Is error.hlp on EVOLVERPATH?\n");
     return;
  }

  while ( fgets(line,sizeof(line)-1,fd)  )
    if ( (strncmp(line,"Error ",6) == 0) && (atoi(line+6) == errnum) )
    { flag = 1;    break;
    }
  if ( flag )
  { outstring(line);
     while ( fgets(line,sizeof(line)-1,fd)  )
     if ( strncmp(line,"Error ",6) == 0 )
        break;
     else outstring(line);
  }
  else kb_error(1862,"Illegal error number.\n",WARNING);
  
  fclose(fd);
#endif
}
