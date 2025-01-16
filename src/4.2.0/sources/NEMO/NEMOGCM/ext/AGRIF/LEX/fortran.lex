/******************************************************************************/
/*                                                                            */
/*     CONV (converter) for Agrif (Adaptive Grid Refinement In Fortran)       */
/*                                                                            */
/* Copyright or   or Copr. Laurent Debreu (Laurent.Debreu@imag.fr)            */
/*                        Cyril Mazauric (Cyril_Mazauric@yahoo.fr)            */
/* This software is governed by the CeCILL-C license under French law and     */
/* abiding by the rules of distribution of free software.  You can  use,      */
/* modify and/ or redistribute the software under the terms of the CeCILL-C   */
/* license as circulated by CEA, CNRS and INRIA at the following URL          */
/* "http://www.cecill.info".                                                  */
/*                                                                            */
/* As a counterpart to the access to the source code and  rights to copy,     */
/* modify and redistribute granted by the license, users are provided only    */
/* with a limited warranty  and the software's author,  the holder of the     */
/* economic rights,  and the successive licensors  have only  limited         */
/* liability.                                                                 */
/*                                                                            */
/* In this respect, the user's attention is drawn to the risks associated     */
/* with loading,  using,  modifying and/or developing or reproducing the      */
/* software by the user in light of its specific status of free software,     */
/* that may mean  that it is complicated to manipulate,  and  that  also      */
/* therefore means  that it is reserved for developers  and  experienced      */
/* professionals having in-depth computer knowledge. Users are therefore      */
/* encouraged to load and test the software's suitability as regards their    */
/* requirements in conditions enabling the security of their systems and/or   */
/* data to be ensured and,  more generally, to use and operate it in the      */
/* same conditions as regards security.                                       */
/*                                                                            */
/* The fact that you are presently reading this means that you have had       */
/* knowledge of the CeCILL-C license and that you accept its terms.           */
/******************************************************************************/
/* version 1.7                                                                */
/******************************************************************************/
%option warn
%option noyywrap

%x parameter
%s character
%x donottreat
%x donottreat_interface
%x includestate
%s fortran77style
%s fortran90style
%{
#include <math.h>
#include <stdlib.h>
#include <string.h>
extern FILE * yyin;
#define MAX_INCLUDE_DEPTH 30
#define YY_BUF_SIZE 64000
YY_BUFFER_STATE include_stack[MAX_INCLUDE_DEPTH];
int line_num_input = 0;
int newlinef90 = 0;
int tmpc;

int lastwasendofstmt = 1;

extern char linebuf1[1024];
extern char linebuf2[1024];

int count_newlines(const char* str_in)
{
    int k, i = 0;
    for( k=0 ; k<strlen(str_in) ; k++)
        if (str_in[k] == '\n') i++;
    return i;
}

#define PRINT_LINE_NUM()   //  { fprintf(stderr,"== Parsing l.%4d...\n", line_num_input); }
#define INCREMENT_LINE_NUM() { line_num_input+=count_newlines(fortran_text) ; PRINT_LINE_NUM(); }
#define YY_USER_ACTION       { if (increment_nbtokens !=0) token_since_endofstmt++; increment_nbtokens = 1; if (token_since_endofstmt>=1) lastwasendofstmt=0; /*printf("VALLIJSDFLSD = %d %d %s \n",lastwasendofstmt,token_since_endofstmt,fortran_text); */ if (firstpass) { strcpy(linebuf1, linebuf2); strncpy(linebuf2, fortran_text,80);} \
                               else {my_position_before=setposcur();/*printf("muposition = %d\n",my_position_before);*/ECHO;} }
#define YY_BREAK {/*printf("VALL = %d %d\n",lastwasendofstmt,token_since_endofstmt);*/if (token_since_endofstmt>=1) lastwasendofstmt=0; break;}

void out_of_donottreat(void);

%}

SLASH       "/"
HEXA        Z\'[0-9a-fA-F]+\'
INTEGER     [0-9]+
NAME        [a-zA-Z][a-zA-Z0-9\_]*
EXPONENT    [edq][-+]?{INTEGER}

BEG_DNT         ^[C!]"$AGRIF_DO_NOT_TREAT"[ \t]*([ \t\n]*(!.*\n)*)+\n
END_DNT         ^[C!]"$AGRIF_END_DO_NOT_TREAT"[ \t]*([ \t\n]*(!.*\n)*)+\n

BEG_INTERFACE   ^[ \t]*interface
END_INTERFACE   ^[ \t]*end[ \t]*interface.*\n

ASSIGNTYPE      "assignment"[ \t]*"("[ \t]*[-+=]+[ \t]*")"

COMM_F77        ^[c*].*\n
COMM_F90_1      ^([ \t\n]*(!.*\n)*)+\n
COMM_F90_2      !.*
NEXTLINEF90     &([ \t\n]|(!.*\n))*
NEXTLINEF77     \n(([c*].*\n)|(([ \t]{0,4}|[ \t]{6,})!.*\n)|[\n])*[ ]{5}([a-z0-9&+$*.#/!;])
LABEL           ^[ 0-9]{1,5}[ \t]+

%%
  if (infixed) BEGIN(fortran77style) ;
  if (infree)  BEGIN(fortran90style) ;

subroutine                  { return TOK_SUBROUTINE; }
program                     { return TOK_PROGRAM; }
allocate                    { inallocate = 1; return TOK_ALLOCATE; }
continue                    { return TOK_CONTINUE; }
nullify			            { return TOK_NULLIFY; }
deallocate                  { inallocate = 1; return TOK_DEALLOCATE; }
result                      { return TOK_RESULT; }
function                    { return TOK_FUNCTION; }
end                         { strcpy(yylval.na,fortran_text); return TOK_ENDUNIT;}
include                     { pos_curinclude = setposcur()-9; BEGIN(includestate); }
use                         { return TOK_USE;}
rewind                      { return TOK_REWIND; }
implicit                    { return TOK_IMPLICIT; }
none                        { return TOK_NONE; }
call                        { return TOK_CALL; }
.true.                      { strcpy(yylval.na,fortran_text); return TOK_TRUE; }
.false.                     { strcpy(yylval.na,fortran_text); return TOK_FALSE; }
\=\>                        { return TOK_POINT_TO; }
{ASSIGNTYPE}                { strcpy(yylval.na,fortran_text); return TOK_ASSIGNTYPE;}
\*\*                        { strcpy(yylval.na,fortran_text); return TOK_DASTER; }
\.eqv\.               { strcpy(yylval.na,fortran_text); return TOK_EQV; }
\.[ \t]*eq[ \t]*\.                { strcpy(yylval.na,fortran_text); return TOK_EQ;  }
\.gt\.                { strcpy(yylval.na,fortran_text); return TOK_GT;  }
\.ge\.                { strcpy(yylval.na,fortran_text); return TOK_GE;  }
\.lt\.                { strcpy(yylval.na,fortran_text); return TOK_LT;  }
\.le\.                { strcpy(yylval.na,fortran_text); return TOK_LE;  }
\.neqv\.              { strcpy(yylval.na,fortran_text); return TOK_NEQV;}
\.[ \t]*ne[ \t]*\.                { strcpy(yylval.na,fortran_text); return TOK_NE;  }
\.not\.               { strcpy(yylval.na,fortran_text); return TOK_NOT; }
\.or\.                { strcpy(yylval.na,fortran_text); return TOK_OR;  }
\.[ \t]*xor\.               { strcpy(yylval.na,fortran_text); return TOK_XOR; }
\.and\.               { strcpy(yylval.na,fortran_text); return TOK_AND; }
\=\=                  { strcpy(yylval.na,fortran_text); return TOK_EQUALEQUAL; }
\/\=                  { strcpy(yylval.na,fortran_text); return TOK_SLASHEQUAL; }
\<\=                  { strcpy(yylval.na,fortran_text); return TOK_INFEQUAL; }
\>\=                  { strcpy(yylval.na,fortran_text); return TOK_SUPEQUAL; }
module                      { return TOK_MODULE; }
while                       { return TOK_WHILE; }
concurrent                  { return TOK_CONCURRENT; }
end[ \t]*do                 { return TOK_ENDDO; }
do[\ t]+{INTEGER}           { strcpy(yylval.na,&fortran_text[2]);
                              if (testandextractfromlist(&List_Do_labels,&fortran_text[2]) == 1)
                              {
                              return TOK_PLAINDO_LABEL_DJVIEW;
                              }
                              else
                              {
                              List_Do_labels=Insertname(List_Do_labels,yylval.na,1);
                              return TOK_PLAINDO_LABEL;
                             }
                             }
do                          { increment_nbtokens = 0; return TOK_PLAINDO;}
real                        { strcpy(yylval.na,fortran_text); return TOK_REAL; }
integer                     { strcpy(yylval.na,fortran_text); return TOK_INTEGER; }
logical                     { strcpy(yylval.na,fortran_text); return TOK_LOGICAL; }
character                   { strcpy(yylval.na,fortran_text); return TOK_CHARACTER; }
{HEXA}                      { strcpy(yylval.na,fortran_text); return TOK_HEXA;}
double[ \t]*precision       { strcpy(yylval.na,fortran_text); return TOK_DOUBLEPRECISION; }
double[ \t]*complex         { strcpy(yylval.na,fortran_text); return TOK_DOUBLECOMPLEX; }
complex                     { strcpy(yylval.na,fortran_text); return TOK_COMPLEX; }
allocatable                 { return TOK_ALLOCATABLE; }
contiguous                  { return TOK_CONTIGUOUS; }
close                       { return TOK_CLOSE; }
inquire                     { return TOK_INQUIRE; }
dimension                   { return TOK_DIMENSION; }
pause                       { return TOK_PAUSE; }
equivalence                 { return TOK_EQUIVALENCE; }
stop                        { return TOK_STOP; }
where                       { return TOK_WHERE; }
end[ \t]*where              { return TOK_ENDWHERE; }
else[ \t]*where[ \t]*\(     { return TOK_ELSEWHEREPAR; }
else[ \t]*where             { return TOK_ELSEWHERE; }
^[ \t]*contains             { return TOK_CONTAINS; }
only                        { return TOK_ONLY; }
parameter                   { return TOK_PARAMETER; }
recursive                   { return TOK_RECURSIVE; }
common                      { return TOK_COMMON; }
^[ \t]*global[ \t]+         { return TOK_GLOBAL; }
external                    { return TOK_EXTERNAL; }
intent                      { intent_spec = 1; return TOK_INTENT; }
pointer                     { return TOK_POINTER; }
optional                    { return TOK_OPTIONAL; }
save                        { return TOK_SAVE; }
^[ \t]*type[ \t]*\(         { pos_cur_decl = setposcur()-strlen(fortran_text); return TOK_TYPEPAR; }
^[ \t]*type/[ \t\,:]+       { return TOK_TYPE; }
end[ \t]*type               { return TOK_ENDTYPE; }
stat                        { if (inallocate == 1) return TOK_STAT; else { strcpy(yylval.na,fortran_text); return TOK_NAME; } }
open                        { return TOK_OPEN; }
return                      { return TOK_RETURN; }
exit                        { return TOK_EXIT; }
print                       { return TOK_PRINT; }
module[ \t]*procedure       { return TOK_PROCEDURE; }
read[ \t]*\(                { in_io_control_spec = 1; return TOK_READ_PAR; }
read                        { return TOK_READ; }
namelist                    { return TOK_NAMELIST; }
write[ \t]*\(               { in_io_control_spec = 1; return TOK_WRITE_PAR; }
write                       { return TOK_WRITE; }
flush                       { strcpy(yylval.na,fortran_text); return TOK_FLUSH; }
target                      { return TOK_TARGET; }
public                      { return TOK_PUBLIC; }
private                     { return TOK_PRIVATE; }
in                          { strcpy(yylval.na,fortran_text);
                               if (intent_spec==1)
                                {return TOK_IN; }
                              else
                              {
                              return TOK_NAME;
                              }
                            }
^[ \t]*data[ \t]+           { pos_curdata = setposcur()-strlen(fortran_text); /*Init_List_Data_Var();*/ return TOK_DATA; }
go[ \t]*to                  { return TOK_PLAINGOTO; }
out                         { strcpy(yylval.na,fortran_text);
                               if (intent_spec==1)
                                {return TOK_OUT; }
                              else
                              {
                              return TOK_NAME;
                              }
                            }
inout                       { strcpy(yylval.na,fortran_text);
                               if (intent_spec==1)
                                {return TOK_IN; }
                              else
                              {
                              return TOK_INOUT;
                              }
                            }
intrinsic                   { return TOK_INTRINSIC; }
then                        { return TOK_THEN; }
else[ \t]*if                { return TOK_ELSEIF; }
else                        { return TOK_ELSE; }
end[ \t]*if                 { return TOK_ENDIF; }
if[ \t]*\(/(.*\)[ \t]*[\=|\+|\-]+.*\))   {strcpy(yylval.na,fortran_text);
                            return TOK_LOGICALIF_PAR;
                            }
if/([ \t]*\([^(]*\)[ \t]*[\=|\+|\-]+)   {strcpy(yylval.na,fortran_text);
                            return TOK_NAME;
                            }
if[ \t]*\(                 {strcpy(yylval.na,fortran_text);
                            return TOK_LOGICALIF_PAR;
                            }
select[ \t]*case            { return TOK_SELECTCASE; }
^[ \t]*case[ \t]*           { if (in_select_case_stmt > 0) return TOK_CASE ; else return TOK_NAME;}
default                     { return TOK_DEFAULT; }
end[ \t]*select             { return TOK_ENDSELECT; }
file[ \t]*\=                { return TOK_FILE; }
access[ \t]*\=                { return TOK_ACCESS; }
action[ \t]*\=                { return TOK_ACTION; }
iolength[ \t]*\=                { return TOK_IOLENGTH; }
unit[ \t]*\=                { return TOK_UNIT; }
opened[ \t]*\=                { return TOK_OPENED; }
fmt[ \t]*\=                 { return TOK_FMT; }
nml[ \t]*\=                 { return TOK_NML; }
end[ \t]*\=                 { return TOK_END; }
eor[ \t]*\=                 { return TOK_EOR; }
len/([ \t]*\=)                 {
                            if (in_char_selector ==1)
                               return TOK_LEN;
                            else
                            {
                            strcpy(yylval.na,fortran_text); return TOK_NAME;
                            }
                            }
kind/([ \t]*\=)            {
                            if ((in_char_selector==1) || (in_kind_selector == 1))
                               return TOK_KIND;
                            else
                            {
                            strcpy(yylval.na,fortran_text); return TOK_NAME;
                            }
                            }
errmsg[ \t]*\=              { return TOK_ERRMSG; }
mold[ \t]*\=              { return TOK_MOLD; }
source[ \t]*\=              { return TOK_SOURCE; }
position[ \t]*\=            { return TOK_POSITION; }
iomsg[ \t]*\=               { return TOK_IOMSG; }
iostat[ \t]*\=              { return TOK_IOSTAT; }
err[ \t]*\=                 { return TOK_ERR; }
form[ \t]*\=                { return TOK_FORM; }
name/([ \t]*\=)             {
                            if (in_inquire==1)
                               return TOK_NAME_EQ;
                            else
                            {
                            strcpy(yylval.na,fortran_text); return TOK_NAME;
                            }
                            }
recl[ \t]*\=                { return TOK_RECL; }
rec/([ \t]*\=)              { if (in_io_control_spec == 1)
                              return TOK_REC;
                             else
                             {
                             strcpy(yylval.na,fortran_text); return TOK_NAME;
                             }
                             }
status/([ \t]*\=)           { if (close_or_connect == 1)
                              return TOK_STATUS;
                             else
                             {
                             strcpy(yylval.na,fortran_text); return TOK_NAME;
                             }
                             }
status                      { strcpy(yylval.na,fortran_text); return TOK_NAME;}
exist[ \t]*\=               { return TOK_EXIST; }
cycle                       { return TOK_CYCLE; }
backspace                   { return TOK_BACKSPACE; }
::                          { return TOK_FOURDOTS;  }
\/[ \t]*({NEXTLINEF90}|{NEXTLINEF77})*[ \t]*\/  { strcpy(yylval.na,fortran_text); return TOK_DSLASH; }
\({SLASH}                   { return TOK_LEFTAB; }
{SLASH}\)                   { return TOK_RIGHTAB; }
{SLASH}                     { strcpy(yylval.na,fortran_text); return TOK_SLASH; }
((\')[^']*&{0,1}\n[ \t]*&{0,1}[^']*(\'))+ {
                              INCREMENT_LINE_NUM() ; strcpy(yylval.na,fortran_text); return TOK_CHAR_CUT; }
<includestate>((\')[^']*(\'))+ {Add_Include_1(fortran_text);}
<includestate>[ \t]* {}
<includestate>\n {
                  if (inmoduledeclare == 0 )
                  {
                  pos_end=setposcur();
                  RemoveWordSET_0(fortran_out,pos_curinclude,pos_end-pos_curinclude);
                  }
                  out_of_donottreat();
                  }
((\')[^']*(\'))+               { strcpy(yylval.na,fortran_text);return TOK_CHAR_CONSTANT; }
((\")[^"]*(\"))+               { strcpy(yylval.na,fortran_text);return TOK_CHAR_MESSAGE; }
{BEG_INTERFACE}             { BEGIN(donottreat_interface); }
<donottreat_interface>{END_INTERFACE} { out_of_donottreat(); return '\n'; }
<donottreat_interface>.*\n            {INCREMENT_LINE_NUM() ; }
<fortran77style>{NAME}{NEXTLINEF77}[a-zA-Z0-9\_]+ {strcpy(yylval.na,fortran_text); removenewline(yylval.na);
                            return TOK_NAME; }
{NAME}                      { strcpy(yylval.na,fortran_text); return TOK_NAME; }
{INTEGER}\.[0-9]+           {strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
({INTEGER}\.[0-9]*)/[^"and."|"false."|"true."|"eq."|"or."|"gt."|"ge."|"lt."|"le."|"not."|"ne."] {  // REAL1
                              strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
(({INTEGER}\.[0-9]+|[0-9]*\.{INTEGER}){EXPONENT}?)|{INTEGER}(\.)?{EXPONENT}                     {  // REAL2
                              strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
{INTEGER}                   { strcpy(yylval.na,fortran_text);
                             if (lastwasendofstmt == 0)
                              return TOK_CSTINT;
                             else
                              if (testandextractfromlist(&List_Do_labels,fortran_text) == 1)
                              {
                              removefromlist(&List_Do_labels,yylval.na);
                              return TOK_LABEL_DJVIEW;
                              }
                              else
                              {
                              return TOK_LABEL;
                              }
                             }
\$                          {}
\.                          {}
\(/([ \t]*[\+\-]?[a-zA-Z0-9]+[\.]*[0-9]*(\_({INTEGER}|{NAME}))?[ \t]*\,[ \t]*[\+\-]?[a-zA-Z0-9]+[\.]*[0-9]*(\_({INTEGER}|{NAME}))?[ \t]*\)) {
                            in_complex_literal = -1;
                            return (int) *fortran_text;
                            }
\(|\)|:|\[|\]|\+|\-|\*|\_   { strcpy(yylval.na,fortran_text); return (int) *fortran_text; }
\%                          { strcpy(yylval.na,fortran_text); return (int) *fortran_text; }
\;                          { lastwasendofstmt=1; token_since_endofstmt = 0; return TOK_SEMICOLON; }
\,                          { if (in_complex_literal==-1) {return TOK_COMMACOMPLEX; in_complex_literal=0;} else; return (int) *fortran_text; }
\=                          { return (int) *fortran_text; }
\<                          { return (int) *fortran_text; }
\>                          { return (int) *fortran_text; }
\n                          { INCREMENT_LINE_NUM() ; lastwasendofstmt=1; token_since_endofstmt = 0; increment_nbtokens = 0; return '\n'; }
[ \t]+                      {increment_nbtokens = 0;}
<fortran77style>{LABEL}[ \t]*format[ \t]*\((.|{NEXTLINEF90}|{NEXTLINEF77})*\)  {
                              return TOK_LABEL_FORMAT; }
<fortran90style>^[ \t]*{INTEGER}[ \t]*format[ \t]*\((.|{NEXTLINEF90})*\) {return TOK_LABEL_FORMAT; }
{NEXTLINEF90}               { INCREMENT_LINE_NUM() ; newlinef90=1; }
<fortran77style>{NEXTLINEF77}               { INCREMENT_LINE_NUM() ;}

{BEG_DNT}                   {INCREMENT_LINE_NUM() ; BEGIN(donottreat); }
<donottreat>{END_DNT}       {out_of_donottreat(); return '\n'; }
<donottreat>.*\n            {INCREMENT_LINE_NUM() ; }
<fortran77style>{COMM_F77}  {INCREMENT_LINE_NUM() ; increment_nbtokens = 0;}
{COMM_F90_1}                {INCREMENT_LINE_NUM() ; increment_nbtokens = 0;}
{COMM_F90_2}                {increment_nbtokens = 0;}
<<EOF>>                     {endoffile = 1; yyterminate();}
%%

void out_of_donottreat ( void )
{
    BEGIN(INITIAL);
    if (infixed) BEGIN(fortran77style) ;
    if (infree)  BEGIN(fortran90style) ;
    INCREMENT_LINE_NUM() ;
}
