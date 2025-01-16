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
/* "http ://www.cecill.info".                                                  */
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

%{
#define YYMAXDEPTH 1000
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "decl.h"

extern int line_num_input;

char c_selectorname[LONG_M];
char ligne[LONG_M];
char truename[LONG_VNAME];
char identcopy[LONG_VNAME];
int c_selectorgiven=0;
listvar *curlistvar;
int in_select_case_stmt=0;
typedim c_selectordim;
listcouple *coupletmp;
int removeline=0;
int token_since_endofstmt = 0;
int increment_nbtokens = 1;
int in_complex_literal = 0;
int close_or_connect = 0;
int in_io_control_spec = 0;
int intent_spec = 0;
long int my_position;
long int my_position_before;
int suborfun = 0;
int indeclaration = 0;
int endoffile = 0;
int in_inquire = 0;
int in_char_selector = 0;
int in_kind_selector =0;
int char_length_toreset = 0;

typedim my_dim;

listvar *test;

char linebuf1[1024];
char linebuf2[1024];

int fortran_error(const char *s)
{
  if (endoffile == 1) 
  {
  endoffile = 0;
  return 0;
  }
    printf("%s line %d, file %s culprit = |%s|\n", s, line_num_input, cur_filename, strcat(linebuf1, linebuf2));
    exit(1);
}

%}

%union {
    char        na[LONG_M];
    listdim     *d;
    listvar     *l;
    listcouple  *lc;
    listname    *lnn;
    typedim     dim1;
    variable    *v;
}

%left ','
%nonassoc ':'
%right '='
%left TOK_EQV TOK_NEQV
%left TOK_OR TOK_XOR
%left TOK_AND
%left TOK_NOT
%nonassoc TOK_LT TOK_GT TOK_LE TOK_GE TOK_EQ TOK_NE
%left TOK_DSLASH
%left '+' '-'
%left '*' TOK_SLASH
%right TOK_DASTER

%token TOK_SEMICOLON
%token TOK_PARAMETER
%token TOK_RESULT
%token TOK_ONLY
%token TOK_INCLUDE
%token TOK_SUBROUTINE
%token TOK_PROGRAM
%token TOK_FUNCTION
%token TOK_LABEL_FORMAT
%token TOK_LABEL_CONTINUE
%token TOK_LABEL_END_DO
%token TOK_MAX
%token TOK_TANH
%token TOK_COMMENT
%token TOK_WHERE
%token TOK_ELSEWHEREPAR
%token TOK_ELSEWHERE
%token TOK_ENDWHERE
%token TOK_MAXVAL
%token TOK_TRIM
%token TOK_NULL_PTR
%token TOK_SUM
%token TOK_SQRT
%token TOK_CASE
%token TOK_SELECTCASE
%token TOK_FILE
%token TOK_REC
%token TOK_NAME_EQ
%token TOK_IOLENGTH
%token TOK_ACCESS
%token TOK_ACTION
%token TOK_FORM
%token TOK_RECL
%token TOK_STATUS
%token TOK_UNIT
%token TOK_OPENED
%token TOK_FMT
%token TOK_NML
%token TOK_END
%token TOK_EOR
%token TOK_EOF
%token TOK_ERR
%token TOK_POSITION
%token TOK_IOSTAT
%token TOK_IOMSG
%token TOK_EXIST
%token TOK_MIN
%token TOK_FLOAT
%token TOK_EXP
%token TOK_LEN
%token TOK_COS
%token TOK_COSH
%token TOK_ACOS
%token TOK_NINT
%token TOK_CYCLE
%token TOK_SIN
%token TOK_SINH
%token TOK_ASIN
%token TOK_EQUIVALENCE
%token TOK_BACKSPACE
%token TOK_LOG
%token TOK_TAN
%token TOK_ATAN
%token TOK_RECURSIVE
%token TOK_ABS
%token TOK_MOD
%token TOK_SIGN
%token TOK_MINLOC
%token TOK_MAXLOC
%token TOK_EXIT
%token TOK_KIND
%token TOK_MOLD
%token TOK_SOURCE
%token TOK_ERRMSG
%token TOK_MINVAL
%token TOK_PUBLIC
%token TOK_PRIVATE
%token TOK_ALLOCATABLE
%token TOK_CONTIGUOUS
%token TOK_RETURN
%token TOK_THEN
%token TOK_ELSEIF
%token TOK_ELSE
%token TOK_ENDIF
%token TOK_PRINT
%token TOK_PLAINGOTO
%token <na> TOK_LOGICALIF
%token <na> TOK_LOGICALIF_PAR
%token TOK_PLAINDO
%token TOK_CONTAINS
%token TOK_ENDDO
%token TOK_MODULE
%token TOK_ENDMODULE
%token TOK_WHILE
%token TOK_CONCURRENT
%token TOK_ALLOCATE
%token TOK_OPEN
%token TOK_CLOSE
%token TOK_INQUIRE
%token TOK_WRITE_PAR
%token TOK_WRITE
%token <na> TOK_FLUSH
%token TOK_READ_PAR
%token TOK_READ
%token TOK_REWIND
%token TOK_DEALLOCATE
%token TOK_NULLIFY
%token TOK_DIMENSION
%token TOK_ENDSELECT
%token TOK_EXTERNAL
%token TOK_INTENT
%token TOK_INTRINSIC
%token TOK_NAMELIST
%token TOK_DEFAULT
%token TOK_OPTIONAL
%token TOK_POINTER
%token TOK_CONTINUE
%token TOK_SAVE
%token TOK_TARGET
%token TOK_IMPLICIT
%token TOK_NONE
%token TOK_CALL
%token TOK_STAT
%token TOK_POINT_TO
%token TOK_COMMON
%token TOK_GLOBAL
%token TOK_LEFTAB
%token TOK_RIGHTAB
%token TOK_PAUSE
%token TOK_PROCEDURE
%token TOK_STOP
%token TOK_FOURDOTS
%token <na> TOK_HEXA
%token <na> TOK_ASSIGNTYPE
%token <na> TOK_OUT
%token <na> TOK_INOUT
%token <na> TOK_IN
%token <na> TOK_USE
%token <na> TOK_DSLASH
%token <na> TOK_DASTER
%token <na> TOK_EQ
%token <na> TOK_EQV
%token <na> TOK_GT
%token <na> TOK_LT
%token <na> TOK_GE
%token <na> TOK_NE
%token <na> TOK_NEQV
%token <na> TOK_LE
%token <na> TOK_OR
%token <na> TOK_XOR
%token <na> TOK_NOT
%token <na> TOK_AND
%token <na> TOK_EQUALEQUAL
%token <na> TOK_SLASHEQUAL
%token <na> TOK_INFEQUAL
%token <na> TOK_SUPEQUAL
%token <na> TOK_TRUE
%token <na> TOK_FALSE
%token <na> TOK_LABEL
%token <na> TOK_LABEL_DJVIEW
%token <na> TOK_PLAINDO_LABEL_DJVIEW
%token <na> TOK_PLAINDO_LABEL
%token <na> TOK_TYPE
%token <na> TOK_TYPEPAR
%token <na> TOK_ENDTYPE
%token TOK_COMMACOMPLEX
%token <na> TOK_REAL
%token <na> TOK_INTEGER
%token <na> TOK_LOGICAL
%token <na> TOK_DOUBLEPRECISION
%token <na> TOK_ENDSUBROUTINE
%token <na> TOK_ENDFUNCTION
%token <na> TOK_ENDPROGRAM
%token <na> TOK_ENDUNIT
%token <na> TOK_CHARACTER
%token <na> TOK_CHAR_CONSTANT
%token <na> TOK_CHAR_CUT
%token <na> TOK_DATA
%token <na> TOK_CHAR_MESSAGE
%token <na> TOK_CSTREAL
%token <na> TOK_COMPLEX
%token <na> TOK_DOUBLECOMPLEX
%token <na> TOK_NAME
%token <na> TOK_SLASH
%token <na> TOK_CSTINT
%token ','
%token ':'
%token '('
%token ')'
%token '<'
%token '>'
%type <l> dcl
%type <l> dimension
%type <l> array-name-spec-list
%type <l> paramlist
%type <l> args
%type <na> declaration-type-spec
%type <l> arglist
%type <lc> only_list
%type <lc> only-list
%type <lc> opt-only-list
%type <lc> only
%type <lc> only_name
%type <lc> rename-list
%type <lc> opt-rename-list
%type <lc> rename
%type <d> dims
%type <d> dimlist
%type <dim1> dim
%type <v> paramitem
%type <na> comblock
%type <na> name_routine
%type <na> type-param-value
%type <na> opt_name
%type <na> constant-expr
%type <na> ac-implied-do
%type <na> subroutine-name
%type <l> opt-dummy-arg-list-par
%type <l> opt-dummy-arg-list
%type <l> dummy-arg-list
%type <l> named-constant-def-list
%type <v> named-constant-def
%type <na> ac-do-variable
%type <na> data-i-do-variable
%type <na> data-stmt-constant
%type <na> do-variable
%type <na> ac-implied-do-control
%type <na> label
%type <na> opt-label
%type <na> label-djview
%type <na> opt-label-djview
%type <na> type
%type <na> real-literal-constant
%type <l> type-declaration-stmt
%type <d> array-spec
%type <d> assumed-shape-spec-list
%type <d> deferred-shape-spec-list
%type <d> assumed-size-spec
%type <d> implied-shape-spec-list
%type <na> typespec
%type <na> null-init
%type <na> initial-data-target
%type <na> intent-spec
%type <na> string_constant
%type <na> access-id
%type <na> dummy-arg-name
%type <na> common-block-name
%type <na> function-name
%type <na> dummy-arg
%type <na> lower-bound
%type <na> upper-bound
%type <na> scalar-constant-subobject
%type <na> opt-data-stmt-star
%type <na> simple_const
%type <na> opt-char-selector
%type <na> char-selector
%type <na> ident
%type <na> intent_spec
%type <na> kind-param
%type <na> signe
%type <na> scalar-int-constant-expr
%type <na> opt_signe
%type <dim1> explicit-shape-spec
%type <d> explicit-shape-spec-list
%type <dim1> assumed-shape-spec
%type <dim1> deferred-shape-spec
%type <na> filename
%type <na> attribute
%type <na> complex_const
%type <na> begin_array
%type <na> clause
%type <na> only-use-name
%type <na> generic-spec
%type <na> arg
%type <d> opt-array-spec-par
%type <d> opt-explicit-shape-spec-list-comma
%type <d> explicit-shape-spec-list-comma
%type <na> uexpr
%type <na> section_subscript_ambiguous
%type <na> minmaxlist
%type <na> subscript
%type <na> subscript-triplet
%type <na> vector-subscript
%type <na> lhs
%type <na> outlist
%type <na> other
%type <na> int-constant-expr
%type <na> dospec
%type <na> expr_data
%type <na> structure_component
%type <na> array_ele_substring_func_ref
%type <na> funarglist
%type <na> funarg
%type <na> funargs
%type <na> triplet
%type <na> substring
%type <na> opt_substring
%type <na> opt_expr
%type <na> optexpr
%type <v> entity-decl
%type <l> entity-decl-list
%type <lnn> data_stmt_value_list
%type <lnn> data-stmt-value-list
%type <lnn> access-id-list
%type <lnn> opt-access-id-list
%type <na> data-stmt-value
%type <l> data-stmt-object-list
%type <l> data-i-do-object-list
%type <v> data-stmt-object
%type <v> data-i-do-object
%type <lnn> datanamelist
%type <na> after_slash
%type <na> after_equal
%type <na> predefinedfunction
%type <na> equiv-op
%type <na> or-op
%type <na> and-op
%type <na> not-op
%type <na> equiv-operand
%type <na> or-operand
%type <na> and-operand
%type <na> mult-operand
%type <na> rel-op
%type <na> concat-op
%type <na> add-operand
%type <na> add-op
%type <na> power-op
%type <na> section-subscript-list
%type <na> opt-lower-bound-2points
%type <na> mult-op
%type <na> array-constructor
%type <na> expr
%type <na> function-reference
%type <na> literal-constant
%type <na> named-constant
%type <na> ac-value-list
%type <na> ac-value
%type <na> intrinsic-type-spec
%type <na> opt-kind-selector
%type <na> char-literal-constant
%type <na> logical-literal-constant
%type <na> real-part
%type <na> imag-part
%type <na> sign
%type <na> signed-int-literal-constant
%type <na> int-literal-constant
%type <na> signed-real-literal-constant
%type <na> complex-literal-constant
%type <na> actual-arg-spec-list
%type <na> procedure-designator
%type <na> constant
%type <na> data-ref
%type <v> structure-component
%type <v> scalar-structure-component
%type <na> int-expr
%type <na> ac-spec
%type <na> type-spec
%type <na> derived-type-spec
%type <v> part-ref
%type <na> opt-part-ref
%type <na> actual-arg-spec
%type <na> kind-selector
%type <na> actual-arg
%type <na> section-subscript
%type <na> keyword
%type <na> primary
%type <na> specification-expr
%type <v> variable
%type <v> data-implied-do
%type <na> substring-range
%type <v> designator
%type <na> object-name
%type <na> object-name-noident
%type <na> array-element
%type <na> array-section
%type <na> scalar-variable-name
%type <na> scalar-constant
%type <na> variable-name
%type <na> opt-subscript 
%type <na> stride
%type <na> opt-scalar-int-expr
%type <na> scalar-int-expr
%type <na> level-1-expr
%type <na> level-2-expr
%type <na> level-3-expr
%type <na> level-4-expr
%type <na> level-5-expr
%type <na> ubound
%type <na> operation
%type <na> proper_lengspec
%type <lnn> use_name_list
%type <lnn> public

%%
/* R201 : program */
/*program: line-break
     | program-unit
     | program program-unit
     ;
*/

input:
      | input line
      ;
line:  line-break
      | suite_line_list
      | error {yyerrok;yyclearin;}
      ;
line-break: '\n' fin_line
      {token_since_endofstmt = 0; increment_nbtokens = 0;}
      | TOK_SEMICOLON
      | TOK_EOF
      | line-break '\n' fin_line
      | line-break TOK_SEMICOLON
      ;
suite_line_list :
        suite_line
      | suite_line_list TOK_SEMICOLON '\n'
      | suite_line_list TOK_SEMICOLON suite_line
      ;
suite_line:program-unit
      | TOK_INCLUDE filename fin_line
        {
            if (inmoduledeclare == 0 )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curinclude,pos_end-pos_curinclude);
            }
        }
      | TOK_COMMENT
      ;
/*
suite_line:
        entry fin_line     subroutine, function, module                    
      | spec fin_line       declaration                                     
      | TOK_INCLUDE filename fin_line
        {
            if (inmoduledeclare == 0 )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curinclude,pos_end-pos_curinclude);
            }
        }
      | execution-part-construct
      ;
*/

fin_line: { pos_cur = setposcur(); }
      ;

/* R202 : program-unit */
program-unit: main-program
     | external-subprogram
     | module
     ;
 
/*R203 : external-subprogram */
external-subprogram: function-subprogram
     | subroutine-subprogram
     ;
     
opt_recursive :         { isrecursive = 0; }
      | TOK_RECURSIVE   { isrecursive = 1; }
      ;

opt_result :                                { is_result_present = 0; }
      | TOK_RESULT arglist_after_result     { is_result_present = 1; }
      ;

name_routine :  TOK_NAME    { strcpy($$, $1); strcpy(subroutinename, $1); }
      ;
filename :      TOK_CHAR_CONSTANT { Add_Include_1($1); }
      ;
arglist :               { if ( firstpass ) $$=NULL; }
      | '(' ')'         { if ( firstpass ) $$=NULL; }
      | '(' {in_complex_literal=0;} args ')'    { if ( firstpass ) $$=$3; }
      ;
arglist_after_result:
      | '(' ')'
      | '(' {in_complex_literal=0;} args ')'    { if ( firstpass ) Add_SubroutineArgument_Var_1($3); }
      ;
args :  arg
        {
            if ( firstpass == 1 )
            {
                strcpy(nameinttypenameback,nameinttypename);
                strcpy(nameinttypename,"");
                curvar = createvar($1,NULL);
                strcpy(nameinttypename,nameinttypenameback);
                curlistvar = insertvar(NULL,curvar);
                $$ = settype("",curlistvar);
            }
        }
      | args ',' arg
        {
            if ( firstpass == 1 )
            {
                strcpy(nameinttypenameback,nameinttypename);
                strcpy(nameinttypename,"");
                curvar = createvar($3,NULL);
                strcpy(nameinttypename,nameinttypenameback);
                $$ = insertvar($1,curvar);
            }
        }
      ;
arg : TOK_NAME  { strcpy($$,$1);  }
      | '*'     { strcpy($$,"*"); }
      ;

opt_spec :
      | access_spec
        {
            PublicDeclare = 0 ;
            PrivateDeclare = 0 ;
        }
      ;
name_intrinsic :
        TOK_SUM
      | TOK_TANH
      | TOK_MAXVAL
      | TOK_MIN
      | TOK_MINVAL
      | TOK_TRIM
      | TOK_SQRT
      | TOK_NINT
      | TOK_FLOAT
      | TOK_EXP
      | TOK_COS
      | TOK_COSH
      | TOK_ACOS
      | TOK_SIN
      | TOK_SINH
      | TOK_ASIN
      | TOK_LOG
      | TOK_TAN
      | TOK_ATAN
      | TOK_MOD
      | TOK_SIGN
      | TOK_MINLOC
      | TOK_MAXLOC
      | TOK_NAME
      ;
use_intrinsic_list :
                               name_intrinsic
      | use_intrinsic_list ',' name_intrinsic
      ;
list_couple :
                        '(' list_expr ')'
      | list_couple ',' '(' list_expr ')'
      ;
list_expr_equi :
                           expr_equi
      | list_expr_equi ',' expr_equi
      ;
expr_equi : '(' list_expr_equi1 ')'
      ;
list_expr_equi1 :
                            ident dims
      | list_expr_equi1 ',' ident dims
      ;
list_expr:
                      expr
      | list_expr ',' expr
      ;
opt_sep:
      | TOK_FOURDOTS
      ;

before_function :   TOK_FUNCTION    { functiondeclarationisdone = 1; }
      ;
before_parameter :  TOK_PARAMETER   {VariableIsParameter = 1; pos_curparameter = setposcur()-9; }
      ;

data_stmt :             /* R534 */
        TOK_DATA data_stmt_set_list

data_stmt_set_list :
        data_stmt_set
      | data_stmt_set_list opt_comma data_stmt_set

data_stmt_set :         /* R535 */
        TOK_NAME TOK_SLASH data_stmt_value_list TOK_SLASH
        {
            createstringfromlistname(ligne,$3);
            if (firstpass == 1) Add_Data_Var_1(&List_Data_Var,$1,ligne);
            else                Add_Data_Var_1(&List_Data_Var_Cur,$1,ligne);
        }
      | datanamelist TOK_SLASH data_stmt_value_list TOK_SLASH
        {
            if (firstpass == 1)  Add_Data_Var_Names_01(&List_Data_Var,$1,$3);
            else                 Add_Data_Var_Names_01(&List_Data_Var_Cur,$1,$3);
        }
      | '(' lhs ',' dospec ')' TOK_SLASH data_stmt_value_list TOK_SLASH
        {
            createstringfromlistname(ligne,$7);
            printf("###################################################################################################################\n");
            printf("## CONV Error : data_implied_do statements (R537) are not yet supported. Please complain to the proper authorities.\n");
            printf("l.%4d -- data_stmt_set : ( lhs , dospec ) /data_stmt_value_list/ -- lhs=|%s| dospec=|%s| data_stmt_value_list=|%s|\n",
                line_num_input,$2,$4,ligne);
            printf("## But, are you SURE you NEED a DATA construct ?\n");
            printf("###################################################################################################################\n");
            exit(1);
        }
      ;

data_stmt_value_list :
        expr_data                           { $$ = Insertname(NULL,$1,0); }
      | expr_data ',' data_stmt_value_list  { $$ = Insertname($3,$1,1);   }
      ;

save:  before_save varsave
      | before_save comblock varsave
      | save opt_comma comblock opt_comma varsave
      | save ',' varsave
      ;
before_save:
        TOK_SAVE        { pos_cursave = setposcur()-4; }
      ;
varsave :
      | TOK_NAME dims   { if ( ! inside_type_declare ) Add_Save_Var_1($1,$2); }
      ;
datanamelist :
        TOK_NAME                        { $$ = Insertname(NULL,$1,0); }
      | TOK_NAME '(' expr ')'           { printf("l.%4d -- INSTRUCTION NON TRAITEE : INITIALISATION DE DATA AVEC EXPRESSION\n",line_num_input); exit(0); }
      | datanamelist ',' datanamelist   { $$ = concat_listname($1,$3); }
      ;
expr_data :
        opt_signe simple_const      { sprintf($$,"%s%s",$1,$2);  }
      | expr_data '+' expr_data     { sprintf($$,"%s+%s",$1,$3); }
      | expr_data '-' expr_data     { sprintf($$,"%s-%s",$1,$3); }
      | expr_data '*' expr_data     { sprintf($$,"%s*%s",$1,$3); }
      | expr_data '/' expr_data     { sprintf($$,"%s/%s",$1,$3); }
      ;
opt_signe :     { strcpy($$,""); }
      | signe   { strcpy($$,$1); }
      ;
namelist :
        TOK_NAMELIST ident
      | TOK_NAMELIST comblock ident
      | namelist opt_comma comblock opt_comma ident
      | namelist ',' ident
      ;
before_dimension :
        TOK_DIMENSION
        {
            positioninblock = 0;
            pos_curdimension = setposcur()-9;
        }

dimension :
        before_dimension opt_comma TOK_NAME dims lengspec
        {
            printf("l.%4d -- dimension : before_dimension opt_comma TOK_NAME = |%s| -- MHCHECK\n",line_num_input,$3);
            if ( inside_type_declare ) break;
            curvar = createvar($3,$4);
            CreateAndFillin_Curvar("", curvar);
            curlistvar=insertvar(NULL, curvar);
            $$ = settype("",curlistvar);
            strcpy(vallengspec,"");
        }
      | dimension ',' TOK_NAME dims lengspec
        {
            printf("l.%4d -- dimension : dimension ',' TOK_NAME dims lengspec = |%s| -- MHCHECK\n",line_num_input,$3);
            if ( inside_type_declare ) break;
            curvar = createvar($3,$4);
            CreateAndFillin_Curvar("", curvar);
            curlistvar = insertvar($1, curvar);
            $$ = curlistvar;
            strcpy(vallengspec,"");
        }
      ;
private :
        TOK_PRIVATE '\n'
      | TOK_PRIVATE opt_sep use_name_list
      ;
public :
        TOK_PUBLIC '\n'                     { $$ = (listname *) NULL; }
      | TOK_PUBLIC opt_sep use_name_list    { $$ = $3; }
      ;
use_name_list :
        TOK_NAME                            { $$ = Insertname(NULL,$1,0); }
      | TOK_ASSIGNTYPE                      { $$ = Insertname(NULL,$1,0); }
      | use_name_list ',' TOK_NAME          { $$ = Insertname($1,$3,0);   }
      | use_name_list ',' TOK_ASSIGNTYPE    { $$ = Insertname($1,$3,0);   }
      ;
common :
        before_common var_common_list
        {
            if ( inside_type_declare ) break;
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_curcommon,pos_end-pos_curcommon);
        }
      | before_common comblock var_common_list
        {
            if ( inside_type_declare ) break;
            sprintf(charusemodule,"%s",$2);
            Add_NameOfCommon_1($2,subroutinename);
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_curcommon,pos_end-pos_curcommon);
        }
      | common opt_comma comblock opt_comma var_common_list
        {
            if ( inside_type_declare ) break;
            sprintf(charusemodule,"%s",$3);
            Add_NameOfCommon_1($3,subroutinename);
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_curcommon,pos_end-pos_curcommon);
        }
      ;
before_common :
        TOK_COMMON              { positioninblock = 0; pos_curcommon = setposcur()-6;   }
      | TOK_GLOBAL TOK_COMMON   { positioninblock = 0; pos_curcommon = setposcur()-6-7; }
      ;
var_common_list :
        var_common                      { if ( ! inside_type_declare ) Add_Common_var_1(); }
      | var_common_list ',' var_common  { if ( ! inside_type_declare ) Add_Common_var_1(); }
      ;
var_common :
        TOK_NAME dims
        {
            positioninblock = positioninblock + 1 ;
            strcpy(commonvar,$1);
            commondim = $2;
        }
      ;
comblock :
        TOK_DSLASH
        {
            strcpy($$,"");
            positioninblock=0;
            strcpy(commonblockname,"");
        }
      | TOK_SLASH TOK_NAME TOK_SLASH
        {
            strcpy($$,$2);
            positioninblock=0;
            strcpy(commonblockname,$2);
        }
      ;
opt_comma :
      | ','
      ;
paramlist :
        paramitem                   { $$=insertvar(NULL,$1); }
      | paramlist ',' paramitem     { $$=insertvar($1,$3);   }
      ;
paramitem :
        TOK_NAME '=' expr
        {
            if ( inside_type_declare ) break;
            curvar=(variable *) calloc(1,sizeof(variable));
            Init_Variable(curvar);
            curvar->v_VariableIsParameter = 1;
            strcpy(curvar->v_nomvar,$1);
            strcpy(curvar->v_subroutinename,subroutinename);
            strcpy(curvar->v_modulename,curmodulename);
            curvar->v_initialvalue=Insertname(curvar->v_initialvalue,$3,0);
            strcpy(curvar->v_commoninfile,cur_filename);
            Save_Length($3,14);
            $$ = curvar;
        }
      ;
module_proc_stmt :
        TOK_PROCEDURE proc_name_list
      ;
proc_name_list :
        TOK_NAME
      | proc_name_list ',' TOK_NAME
      ;
implicit :
        TOK_IMPLICIT TOK_NONE
        {
            if ( insubroutinedeclare == 1 )
            {
                Add_ImplicitNoneSubroutine_1();
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_end-13,13);
            }
        }
      ;
dcl:   options TOK_NAME dims lengspec initial_value
        {
            if ( ! inside_type_declare )
            {
                if (dimsgiven == 1) curvar = createvar($2,curdim);
                else                curvar = createvar($2,$3);
                CreateAndFillin_Curvar(DeclType, curvar);
                curlistvar = insertvar(NULL, curvar);
                if (!strcasecmp(DeclType,"character"))
                {
                    if (c_selectorgiven == 1)
                    {
                        strcpy(c_selectordim.first,"1");
                        strcpy(c_selectordim.last,c_selectorname);
                        Save_Length(c_selectorname,1);
                        change_dim_char(insertdim(NULL,c_selectordim),curlistvar);
                    }
                }
                $$=settype(DeclType,curlistvar);
            }
            strcpy(vallengspec,"");
        }
      | dcl ',' TOK_NAME dims lengspec initial_value
        {
            if ( ! inside_type_declare )
            {
                if (dimsgiven == 1) curvar = createvar($3, curdim);
                else                curvar = createvar($3, $4);
                CreateAndFillin_Curvar($1->var->v_typevar,curvar);
                strcpy(curvar->v_typevar, $1->var->v_typevar);
                curvar->v_catvar = get_cat_var(curvar);
                curlistvar = insertvar($1, curvar);
                if (!strcasecmp(DeclType,"character"))
                {
                    if (c_selectorgiven == 1)
                    {
                        strcpy(c_selectordim.first,"1");
                        strcpy(c_selectordim.last,c_selectorname);
                        Save_Length(c_selectorname,1);
                        change_dim_char(insertdim(NULL,c_selectordim),curlistvar);
                    }
                }
                $$=curlistvar;
            }
            strcpy(vallengspec,"");
        }
      ;
nodimsgiven : { dimsgiven = 0; }
      ;
type:  typespec selector               { strcpy(DeclType,$1);}
      | before_character c_selector     { strcpy(DeclType,"character");  }
      | typespec '*' TOK_CSTINT         { strcpy(DeclType,$1); strcpy(nameinttypename,$3);  }
      | TOK_TYPEPAR attribute ')'       { strcpy(DeclType,"type"); GlobalDeclarationType = 1;  }
      ;
c_selector :
      | '*' TOK_CSTINT              { c_selectorgiven = 1; strcpy(c_selectorname,$2); }
      | '*' '(' c_attribute ')'     { c_star = 1;}
      | '(' c_attribute ')'
      ;
c_attribute :
        TOK_NAME clause opt_clause
      | TOK_NAME '=' clause opt_clause
      | clause opt_clause
      ;
before_character : TOK_CHARACTER    { pos_cur_decl = setposcur()-9; }
      ;
typespec :
        TOK_INTEGER         { strcpy($$,"integer"); pos_cur_decl = setposcur()-7; }
      | TOK_LOGICAL         { strcpy($$,"logical"); pos_cur_decl = setposcur()-7; }
      | TOK_REAL            { strcpy($$,"real");    pos_cur_decl = setposcur()-4; }
      | TOK_COMPLEX         { strcpy($$,"complex"); pos_cur_decl = setposcur()-7; }
      | TOK_DOUBLECOMPLEX   { strcpy($$,"double complex"); pos_cur_decl = setposcur()-14; }
      | TOK_DOUBLEPRECISION { pos_cur_decl = setposcur()-16; strcpy($$,"real"); strcpy(nameinttypename,"8"); printf("OK1\n");}
      ;
lengspec :
      | '*' proper_lengspec {strcpy(vallengspec,$2);}
      ;
proper_lengspec :
        expr        { sprintf($$,"*%s",$1); }
      | '(' '*' ')' { strcpy($$,"*(*)"); }
      ;
selector :
      | '*' proper_selector
      | '(' attribute ')'
      ;
proper_selector : expr
      | '(' '*' ')'
      ;
attribute :
        TOK_NAME clause
      | TOK_NAME '=' clause
        {
            if ( strstr($3,"0.d0") )
            {
                strcpy(nameinttypename,"8");
                strcpy(NamePrecision,"");
            }
            else
                sprintf(NamePrecision,"%s = %s",$1,$3);
        }
      | TOK_NAME        { strcpy(NamePrecision,$1); }
      | TOK_CSTINT      { strcpy(NamePrecision,$1); }
      | TOK_ASSIGNTYPE  { strcpy(NamePrecision,$1); }
      ;
clause :
        expr   { strcpy(CharacterSize,$1);  strcpy($$,$1);  }
      | '*'    { strcpy(CharacterSize,"*"); strcpy($$,"*"); }
      | ':'    { strcpy(CharacterSize,":"); strcpy($$,":"); }
      ;
opt_clause :
      | ',' TOK_NAME clause
      ;
options:
      | TOK_FOURDOTS
      | ',' attr_spec_list TOK_FOURDOTS
      ;
attr_spec_list: attr_spec
      | attr_spec_list ',' attr_spec
      ;
attr_spec :
        TOK_PARAMETER       { VariableIsParameter = 1; }
      | access_spec
      | TOK_ALLOCATABLE     { Allocatabledeclare = 1; }
      | TOK_DIMENSION dims  { dimsgiven = 1; curdim = $2; }
      | TOK_EXTERNAL        { ExternalDeclare = 1; }
      | TOK_INTENT '(' intent_spec ')'
                            { strcpy(IntentSpec,$3); intent_spec = 0;}
      | TOK_INTRINSIC
      | TOK_CONTIGUOUS      { contiguousdeclare = 1 ; }
      | TOK_OPTIONAL        { optionaldeclare = 1 ; }
      | TOK_POINTER         { pointerdeclare = 1 ; }
      | TOK_SAVE            { SaveDeclare = 1 ; }
      | TOK_TARGET          { Targetdeclare = 1; }
      ;
intent_spec :
        TOK_IN          { strcpy($$,$1); }
      | TOK_OUT         { strcpy($$,$1); }
      | TOK_INOUT       { strcpy($$,$1); }
      ;
access_spec :
        TOK_PUBLIC      { PublicDeclare = 1;  }
      | TOK_PRIVATE     { PrivateDeclare = 1; }
      ;
dims :  { $$ = (listdim*) NULL; }
      | '(' {in_complex_literal=0;} dimlist ')'
        {
            $$ = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 )  $$=$3;
        }
      ;
dimlist :
        dim
        {
            $$ = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 )  $$=insertdim(NULL,$1);
        }
      | dimlist ',' dim
        {
            $$ = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( (!inside_type_declare) && created_dimensionlist == 1 ) $$=insertdim($1,$3);
        }
      ;
dim :   ubound              { strcpy($$.first,"1"); strcpy($$.last,$1); Save_Length($1,1); }
      | ':'                 { strcpy($$.first,"");  strcpy($$.last,"");                    }
      | expr ':'            { strcpy($$.first,$1);  Save_Length($1,2); strcpy($$.last,""); }
      | ':' expr            { strcpy($$.first,"");  strcpy($$.last,$2); Save_Length($2,1); }
      | expr ':' ubound     { strcpy($$.first,$1);  Save_Length($1,2); strcpy($$.last,$3); Save_Length($3,1); }
      ;
ubound :
        '*'                 { strcpy($$,"*"); }
      | expr                { strcpy($$,$1);  }
      ;
/*
expr:  uexpr               { strcpy($$,$1); }
      | complex_const       { strcpy($$,$1); }
      | predefinedfunction  { strcpy($$,$1); }
      | '(' expr ')'        { sprintf($$,"(%s)",$2); }
      ;
*/
predefinedfunction :
        TOK_SUM minmaxlist ')'          { sprintf($$,"SUM(%s)",$2);}
      | TOK_MAX minmaxlist ')'          { sprintf($$,"MAX(%s)",$2);}
      | TOK_TANH '(' minmaxlist ')'     { sprintf($$,"TANH(%s)",$3);}
      | TOK_MAXVAL '(' minmaxlist ')'   { sprintf($$,"MAXVAL(%s)",$3);}
      | TOK_MIN minmaxlist ')'          { sprintf($$,"MIN(%s)",$2);}
      | TOK_MINVAL '(' minmaxlist ')'   { sprintf($$,"MINVAL(%s)",$3);}
      | TOK_TRIM '(' expr ')'           { sprintf($$,"TRIM(%s)",$3);}
      | TOK_SQRT expr ')'               { sprintf($$,"SQRT(%s)",$2);}
      | TOK_REAL '(' minmaxlist ')'     { sprintf($$,"REAL(%s)",$3);}
      | TOK_NINT '(' expr ')'           { sprintf($$,"NINT(%s)",$3);}
      | TOK_FLOAT '(' expr ')'          { sprintf($$,"FLOAT(%s)",$3);}
      | TOK_EXP '(' expr ')'            { sprintf($$,"EXP(%s)",$3);}
      | TOK_COS '(' expr ')'            { sprintf($$,"COS(%s)",$3);}
      | TOK_COSH '(' expr ')'           { sprintf($$,"COSH(%s)",$3);}
      | TOK_ACOS '(' expr ')'           { sprintf($$,"ACOS(%s)",$3);}
      | TOK_SIN '(' expr ')'            { sprintf($$,"SIN(%s)",$3);}
      | TOK_SINH '(' expr ')'           { sprintf($$,"SINH(%s)",$3);}
      | TOK_ASIN '(' expr ')'           { sprintf($$,"ASIN(%s)",$3);}
      | TOK_LOG '(' expr ')'            { sprintf($$,"LOG(%s)",$3);}
      | TOK_TAN '(' expr ')'            { sprintf($$,"TAN(%s)",$3);}
      | TOK_ATAN '(' expr ')'           { sprintf($$,"ATAN(%s)",$3);}
      | TOK_ABS expr ')'                { sprintf($$,"ABS(%s)",$2);}
      | TOK_MOD '(' minmaxlist ')'      { sprintf($$,"MOD(%s)",$3);}
      | TOK_SIGN minmaxlist ')'         { sprintf($$,"SIGN(%s)",$2);}
      | TOK_MINLOC '(' minmaxlist ')'   { sprintf($$,"MINLOC(%s)",$3);}
      | TOK_MAXLOC '(' minmaxlist ')'   { sprintf($$,"MAXLOC(%s)",$3);}
      ;
minmaxlist : expr {strcpy($$,$1);}
      | minmaxlist ',' expr     { sprintf($$,"%s,%s",$1,$3); }
      ;
uexpr : lhs                     { strcpy($$,$1); }
      | simple_const            { strcpy($$,$1); }
      | expr operation          { sprintf($$,"%s%s",$1,$2); }
      | signe expr %prec '*'    { sprintf($$,"%s%s",$1,$2); }
      | TOK_NOT expr            { sprintf($$,"%s%s",$1,$2); }
      ;
signe : '+'        { strcpy($$,"+"); }
      | '-'        { strcpy($$,"-"); }
      ;

operation :
        '+' expr %prec '+'          { sprintf($$,"+%s",$2); }
      | '-' expr %prec '+'          { sprintf($$,"-%s",$2); }
      | '*' expr                    { sprintf($$,"*%s",$2); }
      | TOK_DASTER expr             { sprintf($$,"%s%s",$1,$2); }
      | TOK_EQ expr %prec TOK_EQ    { sprintf($$,"%s%s",$1,$2); }
      | TOK_EQV expr %prec TOK_EQV  { sprintf($$,"%s%s",$1,$2); }
      | TOK_GT expr %prec TOK_EQ    { sprintf($$,"%s%s",$1,$2); }
      | '>' expr %prec TOK_EQ       { sprintf($$," > %s",$2); }
      | '<' expr %prec TOK_EQ       { sprintf($$," < %s",$2); }
      | '>''=' expr %prec TOK_EQ    { sprintf($$," >= %s",$3); }
      | '<''=' expr %prec TOK_EQ    { sprintf($$," <= %s",$3); }
      | TOK_LT expr %prec TOK_EQ    { sprintf($$,"%s%s",$1,$2); }
      | TOK_GE expr %prec TOK_EQ    { sprintf($$,"%s%s",$1,$2); }
      | TOK_LE expr %prec TOK_EQ    { sprintf($$,"%s%s",$1,$2); }
      | TOK_NE expr %prec TOK_EQ    { sprintf($$,"%s%s",$1,$2); }
      | TOK_NEQV expr %prec TOK_EQV { sprintf($$,"%s%s",$1,$2); }
      | TOK_XOR expr                { sprintf($$,"%s%s",$1,$2); }
      | TOK_OR expr                 { sprintf($$,"%s%s",$1,$2); }
      | TOK_AND expr                { sprintf($$,"%s%s",$1,$2); }
      | TOK_SLASH after_slash       { sprintf($$,"%s",$2); }
      | '=' after_equal             { sprintf($$,"%s",$2); }

after_slash :                   { strcpy($$,""); }
      | expr                    { sprintf($$,"/%s",$1); }
      | '=' expr %prec TOK_EQ   { sprintf($$,"/= %s",$2);}
      | TOK_SLASH expr          { sprintf($$,"//%s",$2); }
      ;
after_equal :
        '=' expr %prec TOK_EQ   { sprintf($$,"==%s",$2); }
      | expr                    { sprintf($$,"= %s",$1); }
      ;

lhs :   ident                           { strcpy($$,$1); }
      | structure_component             { strcpy($$,$1); }
      | array_ele_substring_func_ref    { strcpy($$,$1); }
      ;

beforefunctionuse :
        {
            agrif_parentcall = 0;
            if ( !strcasecmp(identcopy, "Agrif_Parent") )   agrif_parentcall = 1;
            if ( Agrif_in_Tok_NAME(identcopy) )
            {
                inagrifcallargument = 1;
                Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
            }
        }
      ;
array_ele_substring_func_ref :
        begin_array                                         { strcpy($$,$1); if ( incalldeclare == 0 ) inagrifcallargument = 0;   }
      | begin_array substring                               { sprintf($$," %s %s ",$1,$2); }
      | structure_component '(' {in_complex_literal=0;} funarglist ')'              { sprintf($$," %s ( %s )",$1,$4); }
      | structure_component '(' {in_complex_literal=0;} funarglist ')' substring    { sprintf($$," %s ( %s ) %s ",$1,$4,$6); }
      ;
begin_array : TOK_LOGICALIF
      |  ident '(' {in_complex_literal=0;} funarglist ')'
        {
            if ( inside_type_declare ) break;
            sprintf($$," %s ( %s )",$1,$4);
            ModifyTheAgrifFunction_0($4);
            agrif_parentcall = 0;
        }
      ;
structure_component :
        lhs '%' declare_after_percent lhs
        {
            sprintf($$," %s %% %s ",$1,$4);
            if ( incalldeclare == 0 ) inagrifcallargument = 0;
        }
      ;
/*
vec :
        TOK_LEFTAB outlist TOK_RIGHTAB   { sprintf($$,"(/%s/)",$2); }
      ;
*/
funarglist :
        beforefunctionuse           { strcpy($$," "); }
      | beforefunctionuse funargs   { strcpy($$,$2); }
      ;
funargs :
        funarg              {  strcpy($$,$1); }
      | funargs ',' funarg  {  sprintf($$,"%s,%s",$1,$3); }
      ;
funarg :
        expr       {strcpy($$,$1);}
      | triplet    {strcpy($$,$1);}
      ;
triplet :
        expr ':' expr           {  sprintf($$,"%s :%s",$1,$3);}
      | expr ':' expr ':' expr  {  sprintf($$,"%s :%s :%s",$1,$3,$5);}
      | ':' expr ':' expr       {  sprintf($$,":%s :%s",$2,$4);}
      | ':' ':' expr            {  sprintf($$,": : %s",$3);}
      | ':' expr                {  sprintf($$,":%s",$2);}
      | expr ':'                {  sprintf($$,"%s :",$1);}
      | ':'                     {  sprintf($$,":");}
      ;
ident: TOK_NAME
        {
       //  if (indeclaration == 1) break;
            if ( afterpercent == 0 )
            {
                if ( Agrif_in_Tok_NAME($1) ) Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
                if ( !strcasecmp($1,"Agrif_Parent") )   agrif_parentcall = 1;
                if ( VariableIsFunction($1) )
                {
                    if ( inagrifcallargument == 1 )
                    {
                        if ( !strcasecmp($1,identcopy) )
                        {
                            strcpy(sameagrifname,identcopy);
                            sameagrifargument = 1;
                        }
                    }
                    strcpy(identcopy,$1);
                    pointedvar = 0;

                    if (variscoupled_0($1)) strcpy(truename, getcoupledname_0($1));
                    else                    strcpy(truename, $1);

                    if ( VarIsNonGridDepend(truename) == 0 && (! Variableshouldberemoved(truename)) )
                    {
                        if ( inagrifcallargument == 1 || varispointer_0(truename) == 1 )
                        {
                            if ( (IsinListe(List_UsedInSubroutine_Var,$1) == 1) || (inagrifcallargument == 1) )
                            {
                                if (varistyped_0(truename) == 0)    ModifyTheVariableName_0(truename,strlen($1));
                            }
                        }
                        if ( inagrifcallargument != 1 || sameagrifargument ==1 )
                        {
                            Add_UsedInSubroutine_Var_1(truename);
                        }
                    }
                    NotifyAgrifFunction_0(truename);
                }
            }
            else
            {
                afterpercent = 0;
            }
        }
      ;
simple_const :
        TOK_TRUE     { strcpy($$,".TRUE.");}
      | TOK_FALSE    { strcpy($$,".FALSE.");}
      | TOK_NULL_PTR { strcpy($$,"NULL()"); }
      | TOK_CSTINT   { strcpy($$,$1); }
      | TOK_CSTREAL  { strcpy($$,$1); }
      | TOK_HEXA     { strcpy($$,$1); }
      | simple_const TOK_NAME
                     { sprintf($$,"%s%s",$1,$2); }
      | string_constant opt_substring
      ;
string_constant :
        TOK_CHAR_CONSTANT                   { strcpy($$,$1);}
      | string_constant TOK_CHAR_CONSTANT
      | TOK_CHAR_MESSAGE                    { strcpy($$,$1);}
      | TOK_CHAR_CUT                        { strcpy($$,$1);}
      ;
opt_substring :     { strcpy($$," ");}
      | substring   { strcpy($$,$1);}
      ;
/*
substring :
        '(' optexpr ':' optexpr ')' { sprintf($$,"(%s :%s)",$2,$4);}
      ;
*/
optexpr :           { strcpy($$," ");}
      | expr        { strcpy($$,$1);}
      ;
opt_expr :          { strcpy($$," ");}
      | expr        { strcpy($$,$1);}
      ;
initial_value:     { InitialValueGiven = 0; }
      | '=' expr
        {
            if ( inside_type_declare ) break;
            strcpy(InitValue,$2);
            InitialValueGiven = 1;
        }
      | TOK_POINT_TO expr
        {
            if ( inside_type_declare ) break;
            strcpy(InitValue,$2);
            InitialValueGiven = 2;
        }
      ;
complex_const :
        '(' uexpr ',' uexpr ')' {sprintf($$,"(%s,%s)",$2,$4); }
      ;

only_list :
        only_name   {  $$ = $1; }
      | only_list ',' only_name
        {
            /* insert the variable in the list $1                 */
            $3->suiv = $1;
            $$ = $3;
        }
      ;
only_name :
        TOK_NAME TOK_POINT_TO TOK_NAME
        {
            coupletmp = (listcouple *)calloc(1,sizeof(listcouple));
            strcpy(coupletmp->c_namevar,$1);
            strcpy(coupletmp->c_namepointedvar,$3);
            coupletmp->suiv = NULL;
            $$ = coupletmp;
            pointedvar = 1;
            Add_UsedInSubroutine_Var_1($1);
        }
      | TOK_NAME
        {
            coupletmp = (listcouple *)calloc(1,sizeof(listcouple));
            strcpy(coupletmp->c_namevar,$1);
            strcpy(coupletmp->c_namepointedvar,"");
            coupletmp->suiv = NULL;
            $$ = coupletmp;
        }
      ;

/* R204 : specification-part */
/* opt-implicit-part removed but implicit-stmt and format-stmt added to declaration-construct */
specification-part: opt-use-stmt-list opt-declaration-construct-list
     ;

opt-use-stmt-list:
     |use-stmt-list
     ;
     
opt-implicit-part:
     |implicit-part
     ;

implicit-part: opt-implicit-part-stmt-list implicit-stmt
     ;
     
opt-implicit-part-stmt-list:
     | implicit-part-stmt-list
     ;
     
implicit-part-stmt-list: implicit-part-stmt
     | implicit-part-stmt-list implicit-part-stmt
     ;
     
/* R206: implicit-part-stmt */
implicit-part-stmt: implicit-stmt
     | parameter-stmt
     | format-stmt
     ;


opt-declaration-construct-list:
     |declaration-construct-list
     ;
     
declaration-construct-list:
        declaration-construct
      | declaration-construct-list declaration-construct
      ;
     
/* R207 : declaration-construct */
/* stmt-function-stmt replaced by assignment-stmt due to reduce conflicts */
/* because assignment-stmt has been added  */
/* Every statement that begins with a variable should be added */
/* This include : */
/* pointer-assignment-stmt, do-construct */
/* implicit-stmt and format-stmt added since implicit-part-stmt has been removed due to conflicts (see R204) */
/* ANOTHER SOLUTION TO THE PROBLEM OF STMT-FUNCTION IS NEEDED !!!! */
/* BECAUSE ALMOST ALL ACTION-STMT SHOULD BE INCLUDED HERE !!! */

declaration-construct: derived-type-def
     | parameter-stmt
     | format-stmt
     | implicit-stmt
     | other-specification-stmt
     | type-declaration-stmt
     | assignment-stmt
     | pointer-assignment-stmt
     | do-construct
     | if-construct
     | continue-stmt
     | return-stmt
     | print-stmt
     ;

opt-execution-part:
     | execution-part
     ;

/* R208 : execution-part */
execution-part: executable-construct opt-execution-part-construct-list
     ;

opt-execution-part-construct-list:
     |execution-part-construct-list
     ;

execution-part-construct-list:
        execution-part-construct
      | execution-part-construct-list execution-part-construct
      ;

/* R209 : execution-part-construct */
execution-part-construct: executable-construct
      | format-stmt
      ;

opt-internal-subprogram-part:
     | internal-subprogram-part
     ;
     
/* R120 : internal-subprogram-part */
internal-subprogram-part: TOK_CONTAINS line-break
      opt-internal-subprogram
     ;

opt-internal-subprogram:
     | internal-subprogram-list
     ;

internal-subprogram-list: internal-subprogram
     | internal-subprogram-list internal-subprogram
     ;

/* R211 : internal-subprogram */
internal-subprogram: function-subprogram
     | subroutine-subprogram
     ;

/* R212 : other-specification-stmt */
other-specification-stmt: access-stmt
     | common-stmt
     | data-stmt
     | dimension-stmt
     | equivalence-stmt
     | external-stmt
     | intrinsic-stmt
     | namelist-stmt
     | save-stmt
     ;

/* R213 : executable-construct */
executable-construct:
        action-stmt
      | do-construct
      | case-construct
      | if-construct
      | where-construct
      ;

/* R214 : action-stmt */

/* normal action-stmt */

action-stmt:
      allocate-stmt
      | assignment-stmt
      | call-stmt
      | close-stmt
      | continue-stmt
      | cycle-stmt
      | deallocate-stmt
      | goto-stmt
      | exit-stmt
      | flush-stmt
      | TOK_CYCLE opt_expr
      | TOK_NULLIFY '(' pointer_name_list ')'
      | TOK_ENDMODULE opt_name
        {
            /* if we never meet the contains keyword               */
            if ( firstpass == 0 )
            {
                RemoveWordCUR_0(fortran_out, strlen($2)+11);    // Remove word "end module"
                if ( inmoduledeclare && ! aftercontainsdeclare )
                {
                    Write_Closing_Module(1);
                }
                fprintf(fortran_out,"\n      end module %s\n", curmodulename);
                if ( module_declar && insubroutinedeclare == 0 )
                {
                    fclose(module_declar);
                }
            }
            inmoduledeclare = 0 ;
            inmodulemeet = 0 ;
            aftercontainsdeclare = 1;
            strcpy(curmodulename, "");
            GlobalDeclaration = 0 ;
        }
      | if-stmt
      | inquire-stmt
      | open-stmt
      | pointer-assignment-stmt
      | print-stmt
      | read-stmt
      | return-stmt
      | rewind-stmt
      | stop-stmt
      | where-stmt
      | write-stmt
      | arithmetic-if-stmt
      ;

/* R215 : keyword */
keyword: ident
     ;

scalar-constant: constant
    ;

/* R304 : constant */

constant: literal-constant
     | named-constant
     ;
     
/* R305 : literal-constant */
literal-constant: int-literal-constant
     | real-literal-constant
     | logical-literal-constant
     | complex-literal-constant
     {in_complex_literal=0;}
     | char-literal-constant
     ;
     
/* R306 : named-constant */
named-constant: ident
     ;

scalar-int-constant:int-constant
     ;

/* R307 : int-constant */
int-constant: int-literal-constant
     | named-constant
     ;
     
/*
constant: TOK_CSTINT
     | TOK_CSTREAL
     | ident
     ;
*/

opt-label:
     {strcpy($$,"");}
     | label
     ;

/* R312 : label */
label: TOK_LABEL
     | TOK_CSTINT
     ;

opt-label-djview:
     {strcpy($$,"");}
     | label-djview
     {strcpy($$,$1);}
     ;
     
label-djview: TOK_LABEL_DJVIEW
     ;

/* R401 : type-param-value */
type-param-value: scalar-int-expr
     | '*'
     | ':'
     ;

/* R402: type-spec */
type-spec: intrinsic-type-spec
     {strcpy($$,$1);}
     | derived-type-spec
     {strcpy($$,$1);}
     ;

/* R403 : declaration-type-spec */
declaration-type-spec: {pos_cur_decl=my_position_before;} intrinsic-type-spec
     {strcpy($$,$2);}
     | TOK_TYPEPAR intrinsic-type-spec ')'
     | TOK_TYPEPAR derived-type-spec ')'
     {strcpy(DeclType,"type"); GlobalDeclarationType = 1;  }
     ;

/* R404 : intrinsic-type-spec */
intrinsic-type-spec: TOK_INTEGER {in_kind_selector = 1;} opt-kind-selector
     {sprintf($$,"%s%s",$1,$[opt-kind-selector]);strcpy(DeclType,$1); in_kind_selector =0;}
     | TOK_REAL {in_kind_selector = 1;} opt-kind-selector
     {sprintf($$,"%s%s",$1,$[opt-kind-selector]);strcpy(DeclType,$1);in_kind_selector =0;}
     | TOK_DOUBLEPRECISION {in_kind_selector = 1;} opt-kind-selector
     {sprintf($$,"%s%s",$1,$[opt-kind-selector]);strcpy(DeclType,"real"); strcpy(NamePrecision,"8");in_kind_selector =0;}
     | TOK_COMPLEX {in_kind_selector = 1;} opt-kind-selector
     {sprintf($$,"%s%s",$1,$[opt-kind-selector]);strcpy(DeclType,$1);in_kind_selector =0;}
     | TOK_CHARACTER {in_char_selector = 1;} opt-char-selector
     {sprintf($$,"%s%s",$1,$[opt-char-selector]);strcpy(DeclType,$1);in_char_selector = 0;}
     | TOK_LOGICAL {in_kind_selector = 1;} opt-kind-selector
     {sprintf($$,"%s%s",$1,$[opt-kind-selector]);strcpy(DeclType,$1);in_kind_selector =0;}
     ;

opt-kind-selector:
     {strcpy($$,"");strcpy(NamePrecision,"");}
     |kind-selector
     {strcpy($$,$1);}
     ;
     
/* R405 : kind-selector */
/* Nonstandard extension : * INT */
kind-selector: '(' scalar-int-constant-expr ')'
     {sprintf($$,"(%s)",$2); strcpy(NamePrecision,$2);}
     | '(' TOK_KIND '=' scalar-int-constant-expr ')'
     {sprintf($$,"(KIND=%s)",$4); strcpy(NamePrecision,$4);}
     | '*' TOK_CSTINT
     {sprintf($$,"*%s",$2);strcpy(NamePrecision,$2);}
     ;

/* R406 : signed-int-literal-constant */
/* sign replaced by add-op */

signed-int-literal-constant:int-literal-constant
     | add-op int-literal-constant
     {sprintf($$,"%s%s",$1,$2);}
     ;
     
/* R407 : int-literal-constant */
int-literal-constant: TOK_CSTINT
     | TOK_CSTINT '_' kind-param
     {sprintf($$,"%s_%s",$1,$3);}
     ;

/*R408 : kind-param */
kind-param: TOK_CSTINT
     | TOK_NAME
     ;

opt-sign:
     | sign
     ;

/* R411 : sign */
sign:'+'
     {strcpy($$,"+");}
     | '-'
     {strcpy($$,"-");}
     ;

/* R412 : signed-real-literal-constant */
/* sign replaced by add-op */
signed-real-literal-constant:real-literal-constant
     | add-op real-literal-constant
     {sprintf($$,"%s%s",$1,$2);}
     ;

/* R413 : real-literal-constant */
real-literal-constant: TOK_CSTREAL
     | TOK_CSTREAL '_' kind-param
     {sprintf($$,"%s_%s",$1,$3);};
     ;

/* R417 : complex-literal-constant */
/* in-complex-literal is just here to change default precedence rules ... */

complex-literal-constant: '(' real-part TOK_COMMACOMPLEX imag-part ')'
     {sprintf($$,"(%s,%s)",$2,$4);}
     ;


/* R418 : real-part */
real-part: signed-int-literal-constant
     | signed-real-literal-constant
     | ident
     ;

/* R419 : imag-part */
imag-part: signed-int-literal-constant
     | signed-real-literal-constant
     | named-constant
     ;

opt-char_length-star:
     | '*' char-length
     {char_length_toreset = 1;}
     ;

opt-char-selector:
     {strcpy($$,"");}
    | char-selector
    {strcpy($$,"");}
    ;

/* R420 : char-selector */
char-selector:length-selector
    | '(' TOK_LEN '=' type-param-value ',' TOK_KIND '=' scalar-int-constant-expr ')'
    | '(' type-param-value ',' scalar-int-constant-expr ')'
    | '(' TOK_KIND '=' scalar-int-constant-expr ')'
    | '(' TOK_KIND '=' scalar-int-constant-expr ',' TOK_LEN '=' type-param-value ')'
    ;

/* R421 : length-selector */
length-selector: '(' type-param-value ')'
     {strcpy(CharacterSize,$2);}
     | '(' TOK_LEN '=' type-param-value ')'
     {strcpy(CharacterSize,$4);}
     | '*' char-length
     | '*' char-length ','
     ;

/* R422 : char-length */
char-length: '(' type-param-value ')'
     {c_star=1; strcpy(CharacterSize,$2);}
     | int-literal-constant
     {c_selectorgiven = 1; strcpy(c_selectorname,$1);}
     ;

/* R423 : char-literal-constant */
char-literal-constant: TOK_CHAR_CONSTANT
     | TOK_CHAR_MESSAGE
     | TOK_CHAR_CUT
     ;

/* R424 : logical-literal-constant */
logical-literal-constant: TOK_TRUE
     | TOK_FALSE
     ;

/* R425 : derived-type-def */
derived-type-def: derived-type-stmt { inside_type_declare = 1;} opt-component-part end-type-stmt
     { inside_type_declare = 0;}
     ;
     
/* R426 : derived-type-stmt */
derived-type-stmt: TOK_TYPE opt-type-attr-spec-list-comma-fourdots TOK_NAME line-break
     | TOK_TYPE opt-type-attr-spec-list-comma TOK_NAME '(' type-param-name-list ')' line-break
     ;

opt-type-attr-spec-list-comma-fourdots:
    | opt-type-attr-spec-list-comma TOK_FOURDOTS
    ;
 
 opt-type-attr-spec-list-comma:
     | ',' type-attr-spec-list
     ;

type-attr-spec-list: type-attr-spec
     | type-attr-spec-list ',' type-attr-spec
     ;

/* R427 : type-attr-spec */
type-attr-spec: access-spec
     ;

type-param-name-list: type-param-name
     | type-param-name-list ',' type-param-name
     ;
     
type-param-name: TOK_NAME
     ;

/* R429 : end-type-stmt */
end-type-stmt: TOK_ENDTYPE line-break
     | TOK_ENDTYPE TOK_NAME line-break
     ;

opt-component-part:
     | component-part
     ;

/* R434 : component-part */
component-part: component-def-stmt
    | component-part component-def-stmt
    ;

/* R435 : component-def-stmt */
component-def-stmt: data-component-def-stmt
    ;
    
/* R436 : data-component-def-stmt */
data-component-def-stmt: declaration-type-spec opt-component-attr-spec-list-comma-2points component-decl-list line-break
     ;

opt-component-attr-spec-list-comma-2points:
     | TOK_FOURDOTS
     | ',' component-attr-spec-list TOK_FOURDOTS
     ;

component-attr-spec-list: component-attr-spec
     | component-attr-spec-list ',' component-attr-spec
     ;
     
/* R437 : component-attr-spec */
component-attr-spec: access-spec
     | TOK_ALLOCATABLE
     | TOK_CONTIGUOUS
     | TOK_DIMENSION '(' {in_complex_literal=0;} component-array-spec ')'
     | TOK_POINTER
     ;

component-decl-list: component-decl
     | component-decl-list ',' component-decl
     ;

/* R438 : component-decl */
component-decl : ident opt-component-array-spec opt-char_length-star opt-component-initialization
       {
            PublicDeclare = 0;
            PrivateDeclare = 0;
            ExternalDeclare = 0;
            strcpy(NamePrecision,"");
            c_star = 0;
            InitialValueGiven = 0 ;
            strcpy(IntentSpec,"");
            VariableIsParameter =  0 ;
            Allocatabledeclare = 0 ;
            Targetdeclare = 0 ;
            SaveDeclare = 0;
            pointerdeclare = 0;
            contiguousdeclare = 0 ;
            optionaldeclare = 0 ;
            dimsgiven=0;
            c_selectorgiven=0;
            strcpy(nameinttypename,"");
            strcpy(c_selectorname,"");
            GlobalDeclarationType = 0;
         }
     ;

opt-component-array-spec:
     | '(' component-array-spec ')'
     ;

/* R439 : component-array-spec */
component-array-spec: explicit-shape-spec-list
     | deferred-shape-spec-list
     ;

opt-component-initialization:
     | component-initialization
     ;
     
/* R442 : component-initialization */
component-initialization: '=' constant-expr
      | TOK_POINT_TO null-init
      | TOK_POINT_TO initial-data-target
      ;

/* R443 initial-data-target */
initial-data-target: designator
     {strcpy(my_dim.last,"");}
     ;

/* R453 : derived-type-spec */
derived-type-spec: ident 
     {strcpy(NamePrecision,$1);}
     | ident '(' type-param-spec-list ')'
     ;
     
type-param-spec-list: type-param-spec
     | type-param-spec-list ',' type-param-spec
     ;

/* R454 : type-param-spec */
type-param-spec: type-param-value
    | keyword '=' type-param-value
    ;

/* R455 : structure-constructor */
structure-constructor: derived-type-spec '(' ')'
     | derived-type-spec '(' component-spec-list ')'
     ;
     
component-spec-list: component-spec
     | component-spec-list ',' component-spec
     ;
     
/* R456 : component-spec */
component-spec: component-data-source
     | keyword '=' component-data-source
     ;

/* R457 : component-data-source */
component-data-source: expr
     | data-target
     | proc-target
     ;

/* R468 : array-constructor */
array-constructor: TOK_LEFTAB ac-spec TOK_RIGHTAB
     { sprintf($$,"(/%s/)",$2);}
     | lbracket ac-spec rbracket
     { sprintf($$,"[%s]",$2); }
     ;
     
/* R469 : ac-spec */
/* type-spec TOK_FOURDOTS is removed due to conflicts with part-ref */

/*ac-spec: type-spec TOK_FOURDOTS
     {sprintf($$,"%s::",$1);}
     | ac-value-list
     | type-spec TOK_FOURDOTS ac-value-list
     {sprintf($$,"%s::%s",$1,$3);}
     ;
*/

ac-spec: ac-value-list
     ;
     
/* R470 : lbracket */
lbracket: '['
     ;

/* R471 : rbracket */
rbracket: ']'
     ;

ac-value-list:
        ac-value
      | ac-value-list ',' ac-value
      {sprintf($$,"%s,%s",$1,$3);}
      ;

/* R472 : ac-value */
ac-value: expr
      | ac-implied-do
      ;

/* R473 : ac-implied-do */
ac-implied-do: '(' ac-value-list ',' ac-implied-do-control ')'
     {sprintf($$,"(%s,%s)",$2,$4);}
     ;

/* R474 : ac-implied-do-control */
ac-implied-do-control: ac-do-variable '=' scalar-int-expr ',' scalar-int-expr
     {sprintf($$,"%s=%s,%s",$1,$3,$5);}
     | ac-do-variable '=' scalar-int-expr ',' scalar-int-expr ',' scalar-int-expr
     {sprintf($$,"%s=%s,%s,%s",$1,$3,$5,$7);}
     ;

/* R475 : ac-do-variable */
ac-do-variable: do-variable
     ;

/* R501 : type-declaration-stmt */
type-declaration-stmt: {indeclaration=1;} declaration-type-spec opt-attr-spec-construct entity-decl-list
        {
            /* if the variable is a parameter we can suppose that is*/
            /*    value is the same on each grid. It is not useless */
            /*    to create a copy of it on each grid               */
            if ( ! inside_type_declare )
            {
                pos_end = setposcur();
                //printf("POS = %d %d\n",pos_cur_decl,pos_end);
                RemoveWordSET_0(fortran_out,pos_cur_decl,pos_end-pos_cur_decl);
                ReWriteDeclarationAndAddTosubroutine_01($[entity-decl-list]);
                pos_cur_decl = setposcur();
                if ( firstpass == 0 && GlobalDeclaration == 0
                                    && insubroutinedeclare == 0 )
                {
                    fprintf(fortran_out,"\n#include \"Module_Declar_%s.h\"\n", curmodulename);
                    sprintf(ligne, "Module_Declar_%s.h", curmodulename);
                    module_declar = open_for_write(ligne);
                    GlobalDeclaration = 1 ;
                    pos_cur_decl = setposcur();
                }

                if ( firstpass )
                {
                    Add_Globliste_1($[entity-decl-list]);
                    if ( insubroutinedeclare )
                    {
                        if ( pointerdeclare ) Add_Pointer_Var_From_List_1($[entity-decl-list]);
                        Add_Parameter_Var_1($[entity-decl-list]);
                    }
                    else
                        Add_GlobalParameter_Var_1($[entity-decl-list]);

                    /* If there's a SAVE declaration in module's subroutines we should    */
                    /*    remove it from the subroutines declaration and add it in the    */
                    /*    global declarations                                             */
                                        
                    if ( aftercontainsdeclare && SaveDeclare )
                    {
                        if ( inmodulemeet ) Add_SubroutineDeclarationSave_Var_1($[entity-decl-list]);
                        else                Add_Save_Var_dcl_1($[entity-decl-list]);
                    }
                }
            }
            indeclaration = 0;
            PublicDeclare = 0;
            PrivateDeclare = 0;
            ExternalDeclare = 0;
            strcpy(NamePrecision,"");
            c_star = 0;
            InitialValueGiven = 0 ;
            strcpy(IntentSpec,"");
            VariableIsParameter =  0 ;
            Allocatabledeclare = 0 ;
            Targetdeclare = 0 ;
            SaveDeclare = 0;
            pointerdeclare = 0;
            contiguousdeclare = 0 ;
            optionaldeclare = 0 ;
            dimsgiven=0;
            c_selectorgiven=0;
            strcpy(nameinttypename,"");
            strcpy(c_selectorname,"");
            strcpy(DeclType,"");
            GlobalDeclarationType = 0;
        }
     line-break
     ;

opt-attr-spec-construct:
     | opt-attr-spec-comma-list TOK_FOURDOTS
     ;

opt-attr-spec-comma-list:
     | attr-spec-comma-list
     ;
     
attr-spec-comma-list:
        ',' attr-spec
      | attr-spec-comma-list ',' attr-spec
      ;

/* R502 : attr-spec */
attr-spec:access-spec
     | TOK_ALLOCATABLE
     { Allocatabledeclare = 1; }
     | TOK_CONTIGUOUS
     { contiguousdeclare = 1 ; }
     | TOK_DIMENSION '(' {in_complex_literal=0;} array-spec ')'
     { dimsgiven = 1; curdim = $4; }
     | TOK_EXTERNAL
     { ExternalDeclare = 1; }
     | TOK_INTENT '(' {in_complex_literal=0;} intent-spec ')'
     { strcpy(IntentSpec,$4); }
     | TOK_INTRINSIC
     | TOK_OPTIONAL
     { optionaldeclare = 1 ; }
     | TOK_PARAMETER
     {VariableIsParameter = 1; }
     | TOK_POINTER
     { pointerdeclare = 1 ; }
     | TOK_SAVE
     { SaveDeclare = 1 ; }
     | TOK_TARGET
     { Targetdeclare = 1; }
     ;


entity-decl-list: entity-decl
     {$$=insertvar(NULL,$1);}
     | entity-decl-list ',' entity-decl
     {$$=insertvar($1,$3);}
     ;

/* R503 : entity-decl */
entity-decl: object-name-noident opt-array-spec-par opt-char_length-star opt-initialization
        {
            if ( ! inside_type_declare )
            {
                if (dimsgiven == 1) curvar = createvar($1,curdim);
                else                curvar = createvar($1,$2);
                CreateAndFillin_Curvar(DeclType, curvar);
                strcpy(curvar->v_typevar,DeclType);
                curvar->v_catvar = get_cat_var(curvar);
                
                if (!strcasecmp(DeclType,"character"))
                {
                    if (c_selectorgiven == 1)
                    {
                        Save_Length(c_selectorname,1);
                        strcpy(curvar->v_dimchar,c_selectorname);
                    }
                }
            }
            strcpy(vallengspec,"");
            if (char_length_toreset == 1)
            {
            c_selectorgiven = 0;
            c_star = 0;
            strcpy(c_selectorname,"");
            strcpy(CharacterSize,"");
            char_length_toreset = 0;
            }
            $$=curvar;
        }
     ;


/* R504 : object-name */
object-name: ident
     ;

object-name-noident: TOK_NAME
     ;

opt-initialization: {InitialValueGiven = 0; }
     | initialization
     ;

/* R505 : initialization */
initialization: '=' constant-expr
        {
            if ( inside_type_declare ) break;
            strcpy(InitValue,$2);
            InitialValueGiven = 1;
        }
     | TOK_POINT_TO null-init
        {
            if ( inside_type_declare ) break;
            strcpy(InitValue,$2);
            InitialValueGiven = 2;
        }
     | TOK_POINT_TO initial-data-target
        {
            if ( inside_type_declare ) break;
            strcpy(InitValue,$2);
            InitialValueGiven = 2;
        }
     ;

/* R506 : null-init */
null-init: function-reference
     ;

/* R507 : access-spec */
access-spec: TOK_PUBLIC
     {PublicDeclare = 1;  }
     | TOK_PRIVATE
     {PrivateDeclare = 1;  }
     ;

opt-array-spec-par:
     {$$=NULL;}
     | '(' {in_complex_literal=0;} array-spec ')'
     {$$=$3;}
     ;

/* R514 : array-spec */
array-spec: explicit-shape-spec-list
     {$$=$1;}
     | assumed-shape-spec-list
     {$$=$1;}
     | deferred-shape-spec-list
     {$$=$1;}
     | assumed-size-spec
     {$$=$1;}
     | implied-shape-spec-list
     {$$=$1;}
     ;

explicit-shape-spec-list: explicit-shape-spec
        {
            $$ = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 )  $$=insertdim(NULL,$1);
        }
      | explicit-shape-spec-list ',' explicit-shape-spec
        {
            $$ = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( (!inside_type_declare) && created_dimensionlist == 1 ) $$=insertdim($1,$3);
        }
      ;
      
/* R516 : explicit-shape-spec */
explicit-shape-spec: lower-bound ':' upper-bound
     {strcpy($$.first,$1);  Save_Length($1,2); strcpy($$.last,$3); Save_Length($3,1); }
     |upper-bound 
     {strcpy($$.first,"1"); strcpy($$.last,$1); Save_Length($1,1);}
     ;
     
/* R517 : lower-bound */
lower-bound: specification-expr
     {strcpy($$,$1);}
     ;
     
/* R518 : upper-bound */
upper-bound: specification-expr
     ;

assumed-shape-spec-list:
        assumed-shape-spec
        {
            $$ = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 )  $$=insertdim(NULL,$1);
        }
      | assumed-shape-spec-list ',' assumed-shape-spec
        {
            $$ = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( (!inside_type_declare) && created_dimensionlist == 1 ) $$=insertdim($1,$3);
        }
      ;

/* R519 : assumed-shape-spec */
assumed-shape-spec : ':'
      { strcpy($$.first,"");  strcpy($$.last,"");  }
      | lower-bound ':'
      { strcpy($$.first,$1);  Save_Length($1,2); strcpy($$.last,""); }
      ;

deferred-shape-spec-list:
        deferred-shape-spec
        {
            $$ = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 )  $$=insertdim(NULL,$1);
        }
      | deferred-shape-spec-list ',' deferred-shape-spec
        {
            $$ = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( (!inside_type_declare) && created_dimensionlist == 1 ) $$=insertdim($1,$3);
        }
      ;

/* R520 : deferred-shape-spec */
deferred-shape-spec: ':'
     { strcpy($$.first,"");  strcpy($$.last,"");  }
     ;

/* R521 : assume-size-spec */
assumed-size-spec:opt-explicit-shape-spec-list-comma opt-lower-bound-2points '*'
        {
            $$ = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 ) 
            {
            if (!strcasecmp($2,""))
            {
            strcpy(my_dim.first,"1");
            }
            else
            {
            strcpy(my_dim.first,$2);
            }
            strcpy(my_dim.last,"*");
            $$=insertdim($1,my_dim);
            strcpy(my_dim.first,"");
            strcpy(my_dim.last,"");
            }
        }
     ;
     
opt-explicit-shape-spec-list-comma:
     {$$ = (listdim *) NULL;}
     | explicit-shape-spec-list ','
     {$$ = $1;}
     ;

explicit-shape-spec-list-comma: explicit-shape-spec ','
        {
            $$ = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 )  $$=insertdim(NULL,$1);
        }
     | explicit-shape-spec-list-comma explicit-shape-spec ','
        {
            $$ = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( (!inside_type_declare) && created_dimensionlist == 1 ) $$=insertdim($1,$2);
        }
     ;

opt-lower-bound-2points:
     {strcpy($$,"");}
     | lower-bound ':'
     {strcpy($$,$1);}
     ;

implied-shape-spec-list: implied-shape-spec
     | implied-shape-spec-list ',' implied-shape-spec
     ;

/* R522 : implied-shape-spec */
implied-shape-spec: opt-lower-bound-2points '*'
     ;

/* R523 : intent-spec */
intent-spec: TOK_IN
     { strcpy($$,$1); }
     | TOK_OUT
     { strcpy($$,$1); }
     | TOK_INOUT
     { strcpy($$,$1); }
     ;

/* R524 : access-stmt */
access-stmt: access-spec opt-access-id-list
     {
            if ((firstpass == 0) && (PublicDeclare == 1))
            {
                if ($2)
                {
                    removeglobfromlist(&($2));
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_cur,pos_end-pos_cur);
                    writelistpublic($2);
                }
            }
     PublicDeclare = 0;
     PrivateDeclare = 0;
     }
     line-break
     ;

opt-access-id-list:
     {$$=(listname *)NULL;}
     | opt-TOK_FOURDOTS access-id-list
     {$$=$2;}
     ;

access-id-list: access-id
     {$$=Insertname(NULL,$1,0);}
     | access-id-list ',' access-id
     {$$=Insertname($1,$3,0);}
     ;
     
/* R525 : access-id */
access-id: TOK_NAME
     | generic-spec
     ;
     
/* R534 : data-stmt */
data-stmt: TOK_DATA data-stmt-set opt-data-stmt-set-nlist
        {
            /* we should remove the data declaration                */
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_curdata,pos_end-pos_curdata);
            if ( aftercontainsdeclare == 1  && firstpass == 0 )
            {
                ReWriteDataStatement_0(fortran_out);
                pos_end = setposcur();
            }
            Init_List_Data_Var();
        }
        line-break
     ;

opt-data-stmt-set-nlist:
     | data-stmt-set-nlist
     ;

data-stmt-set-nlist: opt-comma data-stmt-set
     | data-stmt-set-nlist opt-comma data-stmt-set
     ;

/* R535 : data-stmt-set */
data-stmt-set: data-stmt-object-list TOK_SLASH data-stmt-value-list TOK_SLASH
        {
            if (firstpass == 1)  
            {
            Add_Data_Var_Names_01(&List_Data_Var,$1,$3);
            }
            else                 Add_Data_Var_Names_01(&List_Data_Var_Cur,$1,$3);
        }
     ;

data-stmt-object-list: data-stmt-object
     { $$=insertvar(NULL,$1); }
     | data-stmt-object-list ',' data-stmt-object
     {
     $$ = insertvar($1,$3);
     }
     ;

data-stmt-value-list: data-stmt-value
     {$$=Insertname(NULL,$1,0);}
     | data-stmt-value-list ',' data-stmt-value
     {$$ = Insertname($1,$3,1);   }
     ;
     
/* R536 : data-stmt-object */
data-stmt-object: variable
     | data-implied-do
     ;
 
/* R537 : data-implied-do */            
data-implied-do: '(' data-i-do-object-list ',' data-i-do-variable '=' scalar-int-constant-expr ',' scalar-int-constant-expr ')'
     {printf("DOVARIABLE = %s %s %s\n",$4,$6,$8);
     printf("AUTRE = %s %s\n",$2->var->v_nomvar,$2->var->v_initialvalue_array);
     Insertdoloop($2->var,$4,$6,$8,"");
     $$=$2->var;
     }
     | '(' data-i-do-object-list ',' data-i-do-variable '=' scalar-int-constant-expr ',' scalar-int-constant-expr ',' scalar-int-constant-expr ')'
     {
     Insertdoloop($2->var,$4,$6,$8,$10);
     $$=$2->var;
     }
     ;

data-i-do-object-list: data-i-do-object
     {$$=insertvar(NULL,$1);}
     | data-i-do-object-list ',' data-i-do-object
     {$$ = insertvar($1,$3);}
     ;

/* R538 : data-i-do-object */
data-i-do-object: array-element
     | scalar-structure-component
     {$$->v_initialvalue_array=Insertname($$->v_initialvalue_array,my_dim.last,0);
     strcpy(my_dim.last,"");
     }
     | data-implied-do
     ;

/* R539 : data-i-do-variable */
data-i-do-variable: do-variable
     ;

/* R540 : data-stmt-value */
/* data-stmt-repeat and first data-stmt-constant inlined */
data-stmt-value: scalar-constant-subobject opt-data-stmt-star
     {sprintf($$,"%s%s",$1,$2);}
     | int-literal-constant opt-data-stmt-star
     {sprintf($$,"%s%s",$1,$2);}
     | char-literal-constant opt-data-stmt-star
     {sprintf($$,"%s%s",$1,$2);}
     | signed-int-literal-constant
     | signed-real-literal-constant
     | null-init
     | initial-data-target
     | structure-constructor
     ;

opt-data-stmt-star:
     {strcpy($$,"");}
     | '*' data-stmt-constant
     {sprintf($$,"*%s",$2);}
     ;

opt-data-stmt-repeat-star:
     | data-stmt-repeat '*'
     ;

/* R541 : data-stmt-repeat */
/* scalar-int-constant inlined */

data-stmt-repeat: scalar-int-constant
     | scalar-int-constant-subobject
     ;

/* R542 : data-stmt-constant */
data-stmt-constant: scalar-constant
     | scalar-constant-subobject
     | signed-int-literal-constant
     | signed-real-literal-constant
     | null-init
     | initial-data-target
     | structure-constructor
     ;

scalar-int-constant-subobject: int-constant-subobject
     ;

scalar-constant-subobject: constant-subobject
     ;

/* R543 : int-constant-subobject */
int-constant-subobject: constant-subobject
     ;
     
/* R544 : constant-subobject */
constant-subobject: designator
     {strcpy(my_dim.last,"");}
     ;
     
/* R545 : dimension-stmt */
dimension-stmt: {positioninblock = 0; pos_curdimension = my_position_before;}
     TOK_DIMENSION opt-TOK_FOURDOTS array-name-spec-list
        {
            /* if the variable is a parameter we can suppose that is   */
            /*    value is the same on each grid. It is not useless to */
            /*    create a copy of it on each grid                     */
            if ( ! inside_type_declare )
            {
                if ( firstpass )
                {
                    Add_Globliste_1($4);
                    /* if variableparamlists has been declared in a subroutine   */
                    if ( insubroutinedeclare )     Add_Dimension_Var_1($4);
                    
                    /* Add it to the List_SubroutineDeclaration_Var list if not present */
                    /* NB: if not done, a variable declared with DIMENSION but with no type given */
                    /* will not be declared by the conv */
                    ReWriteDeclarationAndAddTosubroutine_01($4);
                }
                else
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_curdimension,pos_end-pos_curdimension);
                    ReWriteDeclarationAndAddTosubroutine_01($4);
                }
            }
            PublicDeclare = 0;
            PrivateDeclare = 0;
            ExternalDeclare = 0;
            strcpy(NamePrecision,"");
            c_star = 0;
            InitialValueGiven = 0 ;
            strcpy(IntentSpec,"");
            VariableIsParameter =  0 ;
            Allocatabledeclare = 0 ;
            Targetdeclare = 0 ;
            SaveDeclare = 0;
            pointerdeclare = 0;
            contiguousdeclare = 0 ;
            optionaldeclare = 0 ;
            dimsgiven=0;
            c_selectorgiven=0;
            strcpy(nameinttypename,"");
            strcpy(c_selectorname,"");
        }
     line-break
     ;
     
array-name-spec-list: TOK_NAME '(' {in_complex_literal = 0;} array-spec ')'
     {
        if ( inside_type_declare ) break;
        curvar = createvar($1,$4);
        CreateAndFillin_Curvar("", curvar);
        curlistvar=insertvar(NULL, curvar);
        $$ = settype("",curlistvar);
        strcpy(vallengspec,"");
     }
     | array-name-spec-list ',' TOK_NAME '(' {in_complex_literal = 0;} array-spec ')'
        {
        if ( inside_type_declare ) break;
        curvar = createvar($3,$6);
        CreateAndFillin_Curvar("", curvar);
        curlistvar = insertvar($1, curvar);
        $$ = curlistvar;
        strcpy(vallengspec,"");
        }
     ;


/* R548 : parameter-stmt */
parameter-stmt: TOK_PARAMETER { VariableIsParameter = 1; pos_curparameter = setposcur()-9; } '(' named-constant-def-list ')'
        {
            if ( ! inside_type_declare )
            {
                if ( firstpass )
                {
                    if ( insubroutinedeclare )  Add_Parameter_Var_1($4);
                    else                        Add_GlobalParameter_Var_1($4);
                }
                else
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out, pos_curparameter, pos_end-pos_curparameter);
                }
            }
            VariableIsParameter =  0 ;
        }
        line-break
     ;

named-constant-def-list: named-constant-def
     {$$=insertvar(NULL,$1);}
     | named-constant-def-list ',' named-constant-def
     {$$=insertvar($1,$3);}
     ;

/* R549 : named-constant-def */
named-constant-def: TOK_NAME '=' constant-expr
        {
            if ( inside_type_declare ) break;
            curvar=(variable *) calloc(1,sizeof(variable));
            Init_Variable(curvar);
            curvar->v_VariableIsParameter = 1;
            strcpy(curvar->v_nomvar,$1);
            strcpy(curvar->v_subroutinename,subroutinename);
            strcpy(curvar->v_modulename,curmodulename);
            curvar->v_initialvalue=Insertname(curvar->v_initialvalue,$3,0);
            strcpy(curvar->v_commoninfile,cur_filename);
            Save_Length($3,14);
            $$ = curvar;
        }
     ;

/* R553 : save-stmt */
save-stmt: {pos_cursave = my_position_before;} TOK_SAVE opt-TOK_FOURDOTS opt-saved-entity-list
     {
     pos_end = setposcur();
     RemoveWordSET_0(fortran_out,pos_cursave,pos_end-pos_cursave);
     }
     line-break
     ;

opt-TOK_FOURDOTS:
     | TOK_FOURDOTS
     ;

opt-saved-entity-list:
     | saved-entity-list
     ;

saved-entity-list: saved-entity
     | saved-entity-list ',' saved-entity
     ;

/* R554 : saved-entity */
saved-entity: object-name
     {if ( ! inside_type_declare ) Add_Save_Var_1($1,(listdim*) NULL); }
     | proc-pointer-name
     | common-block-name
     ;

/* R555 : proc-pointer-name */
proc-pointer-name: ident
     ;

get_my_position:
     {my_position = my_position_before;}
     ;

/* R560 : implicit-stmt */
implicit-stmt: get_my_position TOK_IMPLICIT implicit-spec-list line-break
    | get_my_position TOK_IMPLICIT TOK_NONE
        {
            if ( insubroutinedeclare == 1 )
            {
                Add_ImplicitNoneSubroutine_1();
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,my_position,pos_end-my_position);
            }
        }
    line-break
    ;

implicit-spec-list: implicit-spec
     | implicit-spec-list ',' implicit-spec
     ;

/*R561 implicit-spec */
implicit-spec: declaration-type-spec '(' letter-spec-list ')'
    ;

letter-spec-list:letter-spec
     | letter-spec-list ',' letter-spec
     ;
     
/* R562 : letter-spec */
letter-spec: TOK_NAME
     | TOK_NAME '-' TOK_NAME
     ;

/* R563 : namelist-stmt */
namelist-stmt: TOK_NAMELIST TOK_SLASH TOK_NAME TOK_SLASH namelist-group-object-list opt-namelist-other line-break
     ;

opt-namelist-other:
     | opt-namelist-other opt-comma TOK_SLASH TOK_NAME TOK_SLASH namelist-group-object-list

namelist-group-object-list:namelist-group-object
     | namelist-group-object-list ',' namelist-group-object
     ;

/* R564 : namelist-group-object */
namelist-group-object: variable-name
    ;

/* R565 : equivalence-stmt */
equivalence-stmt:  TOK_EQUIVALENCE equivalence-set-list line-break
     ;

equivalence-set-list:equivalence-set
     | equivalence-set-list ',' equivalence-set
     ;

/* R566 : equivalence-set */
equivalence-set: '(' {in_complex_literal=0;} equivalence-object ',' equivalence-object-list ')'
     ;

equivalence-object-list:equivalence-object
     | equivalence-object-list ',' equivalence-object
     ;

/* R567 : equivalence-object */     
equivalence-object: variable-name
     | array-element
     | substring
     ;


/* R568 : common-stmt */
common-stmt: TOK_COMMON { positioninblock = 0; pos_curcommon = my_position_before; indeclaration=1;} opt-common-block-name common-block-object-list opt-common-block-list
     {
            indeclaration = 0;
            if ( inside_type_declare ) break;
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_curcommon,pos_end-pos_curcommon);
     }
     line-break
     ;

opt-common-block-name:
     | common-block-name
     {
     if ( inside_type_declare ) break;
     sprintf(charusemodule,"%s",$1);
     Add_NameOfCommon_1($1,subroutinename);
     }
     ;
     
common-block-name:TOK_DSLASH
        {
            strcpy($$,"");
            positioninblock=0;
            strcpy(commonblockname,"");
        }
     | TOK_SLASH TOK_NAME TOK_SLASH
        {
            strcpy($$,$2);
            positioninblock=0;
            strcpy(commonblockname,$2);
        }
      ;

opt-comma:
     | ','
     ;

opt-common-block-list:
     | opt-common-block-list opt-comma common-block-name
     {
     if ( inside_type_declare ) break;
     sprintf(charusemodule,"%s",$3);
     Add_NameOfCommon_1($3,subroutinename);
     }
     common-block-object-list
     ;


common-block-object-list: common-block-object
     {if ( ! inside_type_declare ) Add_Common_var_1(); }
     | common-block-object-list ',' common-block-object
     {if ( ! inside_type_declare ) Add_Common_var_1(); }
     ;
  
/* R569 : common-block-object */
/* variable-name replaced by TOK_NAME */
/* because the corresponding variable do not have to be added to the listofsubroutine_used */

common-block-object: TOK_NAME
        {
            positioninblock = positioninblock + 1 ;
            strcpy(commonvar,$1);
            commondim = (listdim*) NULL;
        }
     | TOK_NAME '(' {in_complex_literal=0;} array-spec ')'
        {
            positioninblock = positioninblock + 1 ;
            strcpy(commonvar,$1);
            commondim = $4;
        }
     ;

/* R601 : designator */
designator: array-element
     | array-section
     | structure-component
     | substring
     {$$=createvar($1,NULL);}
     ;
/* R602 : variable */
/*variable: designator
       | expr
       ;
*/

scalar-variable: variable
     ;
     
variable: designator
       {if (strcmp(my_dim.last,""))
       {
       $$->v_initialvalue_array=Insertname(NULL,my_dim.last,0);
       }
       strcpy(my_dim.last,"");
       }
       ;
       
scalar-variable-name: variable-name
     ;

/* R603 : variable-name */
variable-name: ident
      ;

scalar-logical-variable: logical-variable
      ;

/* R604 : logical-variable */
logical-variable: variable
      ;

/* R605 : char-variable */
char-variable: variable
       ;

scalar-default-char-variable: default-char-variable
     ;
     
/* R606 : default-char-variable */
default-char-variable: variable
     ;

scalar-int-variable: int-variable
      ;
      
int-variable: variable
     ;

/* R608 : substring */
substring: data-ref
     | data-ref '(' substring-range ')'
     {sprintf($$,"%s(%s)",$1,$3);}
     | char-literal-constant '(' substring-range ')'
     {sprintf($$,"%s(%s)",$1,$3);}
     ;

/* R609 : parent-string */
/* IS INLINED IN SUBSTRING (R608) */
/*
parent-string: scalar-variable-name
     | array-element
     | scalar-structure-component
     | scalar-constant
     ;
*/

/* R610 : substring-range */
substring-range: opt-scalar-int-expr ':' opt-scalar-int-expr
     {sprintf($$,"%s:%s",$1,$3);}
     ;

/* R611: data-ref */
data-ref: part-ref opt-part-ref
     {sprintf($$,"%s%s",$1->v_nomvar,$2);}
     ;
     
opt-part-ref:
     {strcpy($$,"");}
     | opt-part-ref '%' part-ref
     {sprintf($$,"%s%%%s",$1,$3->v_nomvar);}
     ;

/* R612 : part-ref */
part-ref:ident
     {$$=createvar($1,NULL);}
     | ident '(' {in_complex_literal=0;} section-subscript-list ')'
     {sprintf(ligne,"%s(%s)",$1,$4);$$=createvar($1,NULL);strcpy(my_dim.last,$4);}
     ;
     
/* $$=createvar($1,insertdim(NULL,my_dim));
{strcpy(my_dim.first,"1");strcpy(my_dim.last,$4);$$=createvar($1,insertdim(NULL,my_dim));}
} */

/*part-name: ident
     ;
*/

scalar-structure-component: structure-component
     ;

/* R613 : structure-component */
structure-component: data-ref
     {strcpy(my_dim.last,"");}
     ;

/* R617 : array-element */
array-element: data-ref
      {strcpy(my_dim.last,"");}
      ;

/* R618 : array-section */
array-section: data-ref
     {strcpy(my_dim.last,"");}
     | data-ref '(' substring-range ')'
     {strcpy(my_dim.last,"");}
      ;

/* section-subscript-list can be empty ... */
/* in contradiction with the grammar ... */
section-subscript-list:
      {strcpy($$,"");}
      |  section-subscript
      {strcpy($$,$1);}
      | section-subscript-list ',' section-subscript
      {sprintf($$,"%s,%s",$1,$3);}
      ;

opt-subscript:
     {strcpy($$,"");}
     | subscript
     ;

/* R619 : subscript */
subscript: scalar-int-expr
     ;

/* R620 : section-subscript */
/*section-subscript: subscript
     | subscript-triplet
     | vector-subscript
     ;
*/

/* USE OpenFortranParser rules */

section-subscript: expr section_subscript_ambiguous
     {sprintf($$,"%s%s",$1,$2);}
     | ':'
     {strcpy($$,":");}
     | ':' expr 
     {sprintf($$,":%s",$2);}
     | ':' ':' expr
     {sprintf($$,": :%s",$3);}
     | ':' expr ':' expr
     {sprintf($$,":%s :%s",$2,$4);}
     | TOK_FOURDOTS expr
     {sprintf($$,"::%s",$2);}
     | vector-subscript
     | ident '=' expr
     {sprintf($$,"%s=%s",$1,$3);}
     | ident '=' '*' label
     {sprintf($$,"%s=*%s",$1,$4);}
     | '*' label
     {sprintf($$,"*%s",$2);}
     ;

section_subscript_ambiguous: ':'
     {strcpy($$,":");}
     | ':' expr
     {sprintf($$,":%s",$2);}
     | ':' ':' expr
     {sprintf($$,": :%s",$3);}
     | ':' expr ':' expr
     {sprintf($$,":%s :%s",$2,$4);}
     | TOK_FOURDOTS expr
     {sprintf($$,"::%s",$2);}
     |
     {strcpy($$,"");}
     ;
/* R621 : subscript-triplet */
subscript-triplet: opt-subscript ':' opt-subscript 
     {sprintf($$,"%s:%s",$1,$3);}
     | opt-subscript ':' opt-subscript ':' stride
     {sprintf($$,"%s:%s:%s",$1,$3,$5);}
     ;

/* R622 : stride */
stride: scalar-int-expr
     ;
     
/* R623 : vector-subscript */
vector-subscript: int-expr
     ;

/* R626 : allocate-stmt */
allocate-stmt: TOK_ALLOCATE '(' {in_complex_literal=0;} allocation-list opt-alloc-opt-list-comma ')'
     {inallocate = 0;}
     line-break
     ;

opt-type-spec-fourdots:
     | type-spec TOK_FOURDOTS
     ;

opt-alloc-opt-list-comma:
     | ',' alloc-opt-list
     ;

alloc-opt-list:
        alloc-opt
      | alloc-opt-list ',' alloc-opt
      ;
      
/* R627 : alloc-opt */
alloc-opt: TOK_ERRMSG errmsg-variable
     | TOK_STAT '=' stat-variable
     ;
     
/* R628 : stat-variable */
stat-variable: scalar-int-variable
     ;
     
/* R629 : errmsg-variable */
errmsg-variable: scalar-default-char-variable
    ;

allocation-list:
        allocation
      | allocation-list ',' allocation
      ;
 
/* R631 allocation */
allocation: allocate-object opt-allocate-shape-spec-list-par
     ;

/* R632 allocate-object */     
allocate-object: variable-name
     | structure-component
     ;

opt-allocate-shape-spec-list-par:
     | '(' allocate-shape-spec-list ')'
     ;

allocate-shape-spec-list:
        allocate-shape-spec
      | allocate-shape-spec-list ',' allocate-shape-spec
      ;

/* R633 : allocate-shape-spec */
allocate-shape-spec: opt-lower-bound-expr upper-bound-expr
     ;

opt-lower-bound-expr:
     | lower-bound-expr ':'
     ;

/* R634 : lower-bound-expr */
lower-bound-expr: scalar-int-expr
     ;

/* R634 : upper-bound-expr */
upper-bound-expr: scalar-int-expr
     ;
     
/* R640 : deallocate-stmt */
deallocate-stmt: TOK_DEALLOCATE '(' {in_complex_literal=0;} allocate-object-list opt-dealloc-opt-list-comma ')'
     {inallocate = 0;}
     line-break
     ;

allocate-object-list:
        allocate-object
      | allocate-object-list ',' allocate-object
      ;
      
opt-dealloc-opt-list-comma:
     | ',' dealloc-opt-list
     ;

dealloc-opt-list:
        dealloc-opt
      | dealloc-opt-list ',' dealloc-opt
      ;
      
/* R641 : dealloc-opt */
dealloc-opt: TOK_ERRMSG errmsg-variable
     | TOK_STAT '=' stat-variable
     ;

/* R701 : primary */
/* remove type-param-name */
/* constant replaced by literal-constant to avoid conflict with designato */
/* real-part is added because potential conflicts with complex-literal-constant */

primary: 
      designator
      {
      strcpy($$,$1->v_nomvar);
      if (strcasecmp(my_dim.last,""))
      {
      strcat($$,"(");
      strcat($$,my_dim.last);
      strcat($$,")");
      }
      }
      | literal-constant
      | array-constructor
      | function-reference
      | '(' expr ')'
     { sprintf($$,"(%s)",$2);}
     ;

/* R702 : level-1-expr */
level-1-expr: primary
      {strcpy(my_dim.last,"");}
     ;

/* R704 : mult-operand */
mult-operand: level-1-expr
     | level-1-expr power-op mult-operand
     {sprintf($$,"%s**%s",$1,$3);}
     ;
/* R705 : add-operand */
add-operand: mult-operand
     | add-operand mult-op mult-operand
     { sprintf($$,"%s%s%s",$1,$2,$3); }
     ;
     
/* R706 : level-2-expr */
/* add signed-int-literal-constant because potential reduce conflict with add-op add-operand */

level-2-expr: add-operand
     | add-op add-operand
     { sprintf($$,"%s%s",$1,$2); }
     | level-2-expr add-op add-operand
     { sprintf($$,"%s%s%s",$1,$2,$3); }
     | signed-int-literal-constant
     | level-2-expr signed-int-literal-constant
     { sprintf($$,"%s%s",$1,$2); }
     ;
     
/* R707 : power-op */
power-op : TOK_DASTER
     ;
     
/* R708 : mult-op */
mult-op : '*'
     {strcpy($$,"*");}
     | TOK_SLASH
     ;
     
/* R709 : add-op */
add-op : '+'
     {strcpy($$,"+");}
     | '-'
     {strcpy($$,"-");}     
     ;     

/* R710 : level-3-expr */
level-3-expr: level-2-expr
     | level-3-expr concat-op level-2-expr
     { sprintf($$,"%s%s%s",$1,$2,$3); }
     ;

/* R711 : concat-op */
concat-op : TOK_DSLASH
     ;
/* R712 : level-4-expr */
level-4-expr: level-3-expr
     | level-3-expr rel-op level-3-expr
     { sprintf($$,"%s%s%s",$1,$2,$3); }
     ;

/* R713 : rel-op */
rel-op : TOK_EQ
     | TOK_NE
     | TOK_LT
     | TOK_LE
     | TOK_GT
     | TOK_GE
     | TOK_EQUALEQUAL
     | TOK_SLASHEQUAL
     | '<'
     {strcpy($$,"<");}
     | TOK_INFEQUAL
     | '>'
     {strcpy($$,">");}
     | TOK_SUPEQUAL
     ;

/* R714 : and-operand */
/* level-4-expr inlined as level-3-expr */
and-operand: level-4-expr
     | not-op level-4-expr
     { sprintf($$,"%s%s",$1,$2); }
     ;


/* R715 : or-operand */
or-operand: and-operand
     | or-operand and-op and-operand
     { sprintf($$,"%s%s%s",$1,$2,$3); }
     ;


/* R716 : equiv-operand */
equiv-operand : or-operand
     | equiv-operand or-op or-operand
     { sprintf($$,"%s%s%s",$1,$2,$3); }
     ;

/* R717 : level-5-expr */
level-5-expr: equiv-operand
     | level-5-expr equiv-op equiv-operand
     { sprintf($$,"%s%s%s",$1,$2,$3); }
     ;

/* R718 : not-op */
not-op: TOK_NOT
     ;
     
/* R719 : and-op */
and-op: TOK_AND
     ;
     
/* R720 : or-op */
or-op: TOK_OR
     ;

/* R721 : equiv-op */
equiv-op: TOK_EQV
     | TOK_NEQV
     ;
     
/* R722 : expr */
expr: level-5-expr
     ;

scalar-default-char-expr: default-char-expr
     ;

/* R725 : default-char-expr */
default-char-expr : expr
       ;

/* R726 : int-expr */
int-expr: expr
       ;

opt-scalar-int-expr:
     {strcpy($$,"");}
     | scalar-int-expr
     ;

scalar-int-expr: int-expr
       ;

/* R728 : specification-expr */
specification-expr: scalar-int-expr
     {
     strcpy($$,$1);
     }
     ;

/* R729 : constant-expr */
constant-expr: expr
     {strcpy($$,$1);}
     ;

scalar-default-char-constant-expr: default-char-constant-expr
     ;
     
/* R730: default-char-constant-expr */
default-char-constant-expr: default-char-expr
     ;

scalar-int-constant-expr: int-constant-expr
     ;

/* R731 : int-constant-expr */
int-constant-expr: int-expr
     ;

/* R732 : assignment-stmt */
/* cannot use opt-label due to conflicts ... */

assignment-stmt: variable '=' expr line-break
      | label variable '=' expr line-break
      ;

/* R733 : pointer-assignment-stmt */

/* data-pointer-object and proc-pointer-object replaced by designator */
/*pointer-assignment-stmt: data-pointer-object opt-bounds-spec-list-par TOK_POINT_TO data-target line-break
     | data-pointer-object '(' bounds-remapping-list ')' TOK_POINT_TO data-target line-break
     | proc-pointer-object TOK_POINT_TO proc-target line-break
     ;
*/

pointer-assignment-stmt: designator opt-bounds-spec-list-par TOK_POINT_TO data-target line-break
     | designator '(' bounds-remapping-list ')' TOK_POINT_TO data-target line-break
     | designator TOK_POINT_TO proc-target line-break
     ;
     
/* R734 : data-pointer-object */
data-pointer-object: variable-name
     | scalar-variable '%' TOK_NAME
     ;

opt-bounds-spec-list-par:
     | '(' bounds-spec-list ')'
     ;

bounds-spec-list:
        bounds-spec
      | bounds-spec-list ',' bounds-spec
      ;

bounds-remapping-list:
        bounds-remapping
      | bounds-remapping-list ',' bounds-remapping
      ;
      
/* R735 : bounds-spec */
bounds-spec: lower-bound-expr ':'
     ;

/* R736 : bounds-remapping */
bounds-remapping: lower-bound-expr ':' upper-bound-expr
     ;
     
/* R737 : data-target */
data-target: variable
     ;

procedure-component-name: TOK_NAME
     ;

/* R738 : proc-pointer-object */
proc-pointer-object: proc-pointer-name
     | proc-component-ref
     ;

/* R739 : proc-component-ref */
proc-component-ref : scalar-variable '%' procedure-component-name
     ;
     
/* R740 : proc-target */
proc-target: expr
     | procedure-component-name
     | proc-component-ref
     ;

/* R741 : where-stmt */
where-stmt: TOK_WHERE '(' mask-expr ')' where-assignment-stmt
      ;

/* R742 : where-construct */
where-construct: where-construct-stmt opt-where-body-construct opt-masked-elsewhere-construct opt-elsewhere-construct end-where-stmt
      ;

opt-where-body-construct:
      | opt-where-body-construct where-body-construct
      ;

opt-masked-elsewhere-construct :
      | opt-masked-elsewhere-construct masked-elsewhere-stmt opt-where-body-construct
      ;

opt-elsewhere-construct:
      | opt-elsewhere-construct elsewhere-stmt opt-where-body-construct
      ;

/* R743 : where-construct-stmt */
where-construct-stmt: TOK_WHERE '(' mask-expr ')' line-break
      ;

/* R744 : where-body-construct */
where-body-construct: where-assignment-stmt
      | where-stmt
      | where-construct
      ;

/* R745 : where-assignment-stmt */
where-assignment-stmt: assignment-stmt
      ;

/* R746 : mask-expr */
mask-expr: expr
      ;

/* R747 : masked-elsewhere-stmt */
masked-elsewhere-stmt: TOK_ELSEWHEREPAR mask-expr ')' line-break
      | TOK_ELSEWHEREPAR mask-expr ')' TOK_NAME line-break
      ;

/* R748: elsewhere-stmt */
elsewhere-stmt: TOK_ELSEWHERE line-break
      | TOK_ELSEWHERE TOK_NAME line-break
      ;

/* R749: end-where-stmt */
end-where-stmt:
        TOK_ENDWHERE line-break
      | TOK_ENDWHERE TOK_NAME line-break
      ;

/* R752 : forall-header */
forall-header :
     ;

/* R801 : block */
block: opt-execution-part-construct
      ;

opt-execution-part-construct:
      | opt-execution-part-construct execution-part-construct
      ;

/* R813 : do-construct */
do-construct:
        block-do-construct
      | nonblock-do-construct
      ;

do-construct:
        block-do-construct
      ;
      
/* R814 : block-do-construct */

block-do-construct: label-do-stmt do-block end-do
      | nonlabel-do-stmt do-block end-do
      ;

/* R815 : do-stmt */
/*do-stmt:
        label-do-stmt
      | nonlabel-do-stmt
      ;
*/

/* R816 : label-do-stmt */
label-do-stmt: TOK_NAME ':' TOK_PLAINDO_LABEL line-break
      |              TOK_PLAINDO_LABEL line-break
      | TOK_NAME ':' TOK_PLAINDO_LABEL loop-control line-break
      |              TOK_PLAINDO_LABEL loop-control line-break
      ;
      
label-do-stmt-djview: TOK_NAME ':' TOK_PLAINDO_LABEL_DJVIEW line-break
      |              TOK_PLAINDO_LABEL_DJVIEW line-break
      | TOK_NAME ':' TOK_PLAINDO_LABEL_DJVIEW loop-control line-break
      |              TOK_PLAINDO_LABEL_DJVIEW loop-control line-break
      ;
      
/* R817 : nonlabel-do-stmt */
nonlabel-do-stmt: TOK_NAME ':' TOK_PLAINDO line-break
      |              TOK_PLAINDO line-break
      | TOK_NAME ':' TOK_PLAINDO loop-control line-break
      |              TOK_PLAINDO loop-control line-break
      ;

/* R818 : loop-control */
loop-control:
        opt_comma do-variable '=' expr ',' expr
      | opt_comma do-variable '=' expr ',' expr ',' expr
      | opt_comma TOK_WHILE '(' expr ')'
      | opt_comma TOK_CONCURRENT forall-header
      ;

/* R819 : do-variable */
do-variable: ident
     ;

/* R820 : do-block */
do-block: block
     ;

/* R821 : end-do */
/*end-do: end-do-stmt
     | do-term-action-stmt
     ;
*/

end-do: end-do-stmt
     | label-djview continue-stmt
     ;

/* R822 : end-do-stmt */
end-do-stmt: opt-label-djview TOK_ENDDO line-break
      | opt-label-djview TOK_ENDDO TOK_NAME line-break
      ;

/* R823 : nonblock-do-construct */
/* only outer-shared-do-construct is used */

/*
nonblock-do-construct: outer-shared-do-construct
      ;
*/

nonblock-do-construct: action-term-do-construct
      | outer-shared-do-construct
      ;


/* R824 : action-term-do-construct */

action-term-do-construct: label-do-stmt do-block do-term-action-stmt
      ;
      
/* R825 : do-body */

do-body :
      | execution-part-construct do-body
      ;

/* R826 : do-term-action-stmt */
do-term-action-stmt:  label-djview do-term-action-stmt-special
      ;

/* do-term-action-stmt-special */
do-term-action-stmt-special:
      allocate-stmt
      | assignment-stmt
      | call-stmt
      | close-stmt
      | deallocate-stmt
      | flush-stmt
      | goto-stmt
      | TOK_REWIND after_rewind
      | TOK_NULLIFY '(' pointer_name_list ')'
      | if-stmt
      | inquire-stmt
      | open-stmt
      | print-stmt
      | read-stmt
      | rewind-stmt
      | where-stmt
      | write-stmt
      ;


/* R827 : outer-shared-do-construct */
/* do-body is same as do-block 
we extend the definition of outer-shared-do-construct
a label-do-stmt statement must be followed by a label-do-stmt-djview statement
*/

outer-shared-do-construct : label-do-stmt do-block label-do-stmt-djview-do-block-list inner-shared-do-construct
       | label-do-stmt do-block inner-shared-do-construct
       ;

label-do-stmt-djview-do-block-list: label-do-stmt-djview do-block
       | label-do-stmt-djview-do-block-list label-do-stmt-djview do-block
       ;

/* R828 : shared-term-do-construct */

shared-term-do-construct: outer-shared-do-construct
      | inner-shared-do-construct
      ;
    
/* R829 : inner-shared-do-construct */
/* do-body is same as do-block */
inner-shared-do-construct: label-do-stmt-djview do-block do-term-shared-stmt
      ;
      
/* R830 : do-term-shared-stmt */

do-term-shared-stmt: label-djview action-stmt
      ;

opt-do-construct-name:
     | TOK_NAME
     ;

/* R831 : cycle-stmt */
cycle-stmt: TOK_CYCLE opt-do-construct-name line-break
     ;

/* R832 : if-construct */
if-construct: if-then-stmt block opt-else-if-stmt-block opt-else-stmt-block end-if-stmt
      ;
  
opt-else-if-stmt-block: 
      | else-if-stmt-block
      | opt-else-if-stmt-block else-if-stmt-block
      ;

else-if-stmt-block: else-if-stmt block
      ;

opt-else-stmt-block: 
      | else-stmt-block
      | opt-else-stmt-block else-if-stmt-block
      ;

else-stmt-block: else-stmt block
        ;

/* R833 : if-then-stmt */
if-then-stmt: TOK_NAME ':' TOK_LOGICALIF_PAR expr ')' TOK_THEN line-break
      | label TOK_NAME ':' TOK_LOGICALIF_PAR expr ')' TOK_THEN line-break
      | opt-label TOK_LOGICALIF_PAR expr ')' TOK_THEN line-break
      ;

/* R834 : else-if-stmt */
else-if-stmt:TOK_ELSEIF '(' expr ')' TOK_THEN line-break
      | TOK_ELSEIF '(' expr ')' TOK_THEN TOK_NAME line-break
      ;

/* R835 : else-stmt */
else-stmt:TOK_ELSE line-break
      | TOK_ELSE TOK_NAME line-break
      ;

/* R836 : end-if-stmt */
end-if-stmt:TOK_ENDIF line-break
      | TOK_ENDIF TOK_NAME line-break
      ;

/* R837 : if-stmt */
if-stmt: opt-label TOK_LOGICALIF_PAR expr ')' action-stmt
        ;

/* R838 : case-construct */
case-construct: select-case-stmt opt_case-stmt-block end-select-stmt
        ;

opt_case-stmt-block:
        | case-stmt-block
        | opt_case-stmt-block case-stmt-block
        ;

case-stmt-block: case-stmt block
        ;

/* R839 : select-case-stmt */
select-case-stmt :TOK_NAME ':' TOK_SELECTCASE '(' expr ')' {in_select_case_stmt++;} line-break
        |              TOK_SELECTCASE '(' expr ')' {in_select_case_stmt++;} line-break
        ;

/* R840 : case-stmt */
case-stmt:TOK_CASE case-selector line-break
        | TOK_CASE case-selector TOK_NAME line-break
        ;

/* R840 : end-select-stmt */
end-select-stmt: TOK_ENDSELECT {in_select_case_stmt--;} line-break
        | TOK_ENDSELECT TOK_NAME {in_select_case_stmt--;} line-break
        ;

/* R843 : case-selector */
case-selector:
          '(' {in_complex_literal=0;} case-value-range-list ')'
        | TOK_DEFAULT
        ;

case-value-range-list:
        case-value-range
      | case-value-range-list ',' case-value-range
      ;

/* R844: case-value-range */
case-value-range :
        case-value
      | case-value ':'
      | ':' case-value
      | case-value ':' case-value
      ;

/* R845 : case-value */
case-value: expr
        ;

/* R850 : exit-stmt */
exit-stmt: TOK_EXIT line-break
       | TOK_EXIT TOK_NAME line-break
       ;

/* R851 : goto-stmt */
goto-stmt: TOK_PLAINGOTO label line-break
     ;

/* R853 arithmetic-if-stmt */
arithmetic-if-stmt: opt-label TOK_LOGICALIF_PAR expr ')' label ',' label ',' label line-break
     ;

/* R854 : continue-stmt */
continue-stmt: opt-label TOK_CONTINUE line-break
        ;

/* R855 : stop-stmt */
stop-stmt: TOK_STOP line-break
     | TOK_STOP stop-code line-break
     ;

/* R857 : stop-code */
stop-code: scalar-default-char-constant-expr
    | scalar-int-constant-expr
    ;

/* R901 : io-unit */
io-unit : file-unit-number
        | '*'
        | internal-file-variable
        ;

/* R902 : file-unit-number */
file-unit-number : scalar-int-expr
        ;

/* R902 : internal-file-variable */
internal-file-variable : char-variable
        ;

/* R904 : open-stmt */
open-stmt: TOK_OPEN '(' {close_or_connect = 1;} connect-spec-list ')' {close_or_connect = 0;} line-break
        ;

connect-spec-list: connect-spec
         | connect-spec-list ',' connect-spec
         ;

/* R905 : connect-spec */
connect-spec: file-unit-number
      | TOK_UNIT file-unit-number
      | TOK_ACCESS scalar-default-char-expr
      | TOK_ACTION scalar-default-char-expr
      | TOK_ERR label
      | TOK_FILE file-name-expr
      | TOK_FORM scalar-default-char-expr
      | TOK_IOMSG iomsg-variable
      | TOK_IOSTAT scalar-int-variable
      | TOK_POSITION scalar-default-char-expr
      | TOK_RECL scalar-int-expr
      | TOK_STATUS '=' scalar-default-char-expr
      ;

/* R906 : file-name-expr */
file-name-expr: scalar-default-char-expr
     ;

/* R907 : iomsg-variable */
iomsg-variable: scalar-default-char-variable
     ;

/* R908 : close-stmt */
close-stmt: opt-label TOK_CLOSE '(' {close_or_connect = 1;} close-spec-list ')' line-break
        {close_or_connect = 0;}
        ;

close-spec-list: close-spec
         | close-spec-list ',' close-spec
         ;

/* R909 : close-spec */
close-spec: file-unit-number
       | TOK_UNIT file-unit-number
       | TOK_IOMSG iomsg-variable
       | TOK_IOSTAT scalar-int-variable
       | TOK_ERR label
       | TOK_STATUS '=' scalar-default-char-expr
       ;

/* R910 : read-stmt */
read-stmt: opt-label TOK_READ_PAR io-control-spec-list ')'
         {
         in_io_control_spec = 0;
         }
         line-break
        | opt-label TOK_READ_PAR io-control-spec-list ')' input-item-list
         {
         in_io_control_spec = 0;
         }
         line-break
        | opt-label TOK_READ format line-break
        | opt-label TOK_READ format ',' input-item-list line-break
        ;
        
/* R911 : write-stmt */
write-stmt: opt-label TOK_WRITE_PAR io-control-spec-list ')'
         {
         in_io_control_spec = 0;
         }
         line-break
        | opt-label TOK_WRITE_PAR io-control-spec-list ')'  output-item-list
         {
         in_io_control_spec = 0;
         }
         line-break
        ;

/* R912 : print-stmt */
print-stmt: opt-label TOK_PRINT format line-break
        | opt-label TOK_PRINT format ',' output-item-list line-break
        ;
io-control-spec-list: io-control-spec
         | io-control-spec-list ',' io-control-spec
         ;

namelist-group-name: TOK_NAME
         ;

/* R913 : io-control-spec */
io-control-spec: io-unit
         | TOK_UNIT io-unit
         | format
         | namelist-group-name
         | TOK_NML namelist-group-name
         | TOK_FMT format
         | TOK_END label
         | TOK_EOR label
         | TOK_ERR label
         | TOK_IOMSG iomsg-variable
         | TOK_IOSTAT scalar-int-variable
         | TOK_REC '=' scalar-int-expr
        ;

/* R915 : format */
format: default-char-expr
        | label
        | '*'
        ;
input-item-list:
         input-item
         | input-item-list ',' input-item
         ;
/* R916 : input-item */
input-item: variable
        | io-implied-do
        ;

output-item-list:
         output-item
         | output-item-list ',' output-item
         ;

/* R917 : output-item */
output-item: expr
        | io-implied-do
        ;

/* R918 : io-implied-do */
io-implied-do : '(' io-implied-do-object-list ',' io-implied-do-control ')'
        ;

io-implied-do-object-list: io-implied-do-object
         | io-implied-do-object-list ',' io-implied-do-object
         ;

/* R919 : io-implied-do-object */
/* input-item removed since possible conflicts (output-item can be variable) */
/* io-implied-do-object : input-item
        | output-item
        ;
*/

io-implied-do-object : output-item
        ;        

/* R920 : io-implied-do-control */
io-implied-do-control: do-variable '=' scalar-int-expr ',' scalar-int-expr
        | do-variable '=' scalar-int-expr ',' scalar-int-expr ',' scalar-int-expr
        ;

/* R926 : rewind-stmt */
rewind-stmt: TOK_REWIND file-unit-number line-break
     | TOK_REWIND '(' position-spec-list ')' line-break
     ;

position-spec-list:
        position-spec
      | position-spec-list ',' position-spec
      ;
      
/* R927 : position-spec */
position-spec: file-unit-number
     | TOK_UNIT file-unit-number
     | TOK_IOMSG iomsg-variable
     | TOK_IOSTAT scalar-int-variable
     | TOK_ERR label
     ;

/* R928 : flush-stmt */
flush-stmt: TOK_FLUSH file-unit-number line-break
     | TOK_FLUSH '(' flush-spec-list ')' line-break
     ;

flush-spec-list:
        flush-spec
      | flush-spec-list ',' flush-spec
      ;
      
/* R929 : flush-spec */
flush-spec: file-unit-number
     | TOK_UNIT file-unit-number
     | TOK_IOSTAT scalar-int-variable
     | TOK_IOMSG iomsg-variable
     | TOK_ERR label
     ;


/* R930 : inquire-stmt */
inquire-stmt: TOK_INQUIRE set_in_inquire '(' inquire-spec-list ')'
     {in_inquire=0;}
     line-break
     | TOK_INQUIRE set_in_inquire '(' TOK_IOLENGTH scalar-int-variable ')' output-item-list
     {in_inquire=0;}
     line-break
     ;

set_in_inquire: {in_inquire=1;}  
     ;

inquire-spec-list:
        inquire-spec
      | inquire-spec-list ',' inquire-spec
      ;
      
/* R931 : inquire-spec */
inquire-spec: file-unit-number
     | TOK_UNIT file-unit-number
     | TOK_FILE file-name-expr
     | TOK_ACCESS scalar-default-char-variable
     | TOK_ACTION scalar-default-char-variable
     | TOK_ERR label
     | TOK_EXIST scalar-logical-variable
     | TOK_IOMSG iomsg-variable
     | TOK_IOSTAT scalar-int-variable
     | TOK_NAME_EQ '=' scalar-default-char-variable
     | TOK_OPENED scalar-logical-variable
     | TOK_RECL scalar-int-variable
     ;

/* R1001 : format-stmt */
format-stmt: TOK_LABEL_FORMAT line-break
        ;

/* R1104 : module */
module:module-stmt opt-specification-part opt-module-subprogram-part {pos_endsubroutine=setposcur();} end-module-stmt
     ;

opt-module-subprogram-part:
     | module-subprogram-part
     ;

/* R1105 : module-stmt */
module-stmt : TOK_MODULE TOK_NAME
        {
            GlobalDeclaration = 0;
            strcpy(curmodulename,$2);
            strcpy(subroutinename,"");
            Add_NameOfModule_1($2);
            if ( inmoduledeclare == 0 )
            {
                /* To know if there are in the module declaration    */
                inmoduledeclare = 1;
                /* to know if a module has been met                  */
                inmodulemeet = 1;
                /* to know if we are after the keyword contains      */
                aftercontainsdeclare = 0 ;
            }
        }
        line-break
     ;

/* R1106 : end-module-stmt */
end-module-stmt: get_my_position TOK_ENDUNIT opt-tok-module opt-ident
        {
            /* if we never meet the contains keyword               */
            if ( firstpass == 0 )
            {
                RemoveWordCUR_0(fortran_out, setposcur()-my_position);    // Remove word "end module"
                if ( inmoduledeclare && ! aftercontainsdeclare )
                {
                    Write_Closing_Module(1);
                }
                fprintf(fortran_out,"\n      end module %s\n", curmodulename);
                if ( module_declar && insubroutinedeclare == 0 )
                {
                    fclose(module_declar);
                }
            }
            inmoduledeclare = 0 ;
            inmodulemeet = 0 ;
            aftercontainsdeclare = 1;
            strcpy(curmodulename, "");
            GlobalDeclaration = 0 ;
        }
        line-break
     ;

opt-tok-module:
     | TOK_MODULE
     ;

opt-ident:
     | TOK_NAME
     ;
/* R1107 : module-subprogram-part */
module-subprogram-part:contains-stmt opt-module-subprogram-list
     ;
     
opt-module-subprogram-list:
     | module-subprogram-list
     ;
     
module-subprogram-list: module-subprogram
     | module-subprogram-list module-subprogram
     ;

module-subprogram: function-subprogram
     | subroutine-subprogram
     ;

use-stmt-list:use-stmt
     | use-stmt-list use-stmt
     ;

save_olduse:
     {if (firstpass == 0 && oldfortran_out) pos_curuseold = setposcurname(oldfortran_out);}
     ;
     
/* R1109 use-stmt */
use-stmt: get_my_position TOK_USE save_olduse opt-module-nature-2points TOK_NAME opt-rename-list
    {
            if ( firstpass )
            {
                if ( insubroutinedeclare )
                {
                    if ($6) {
                      Add_CouplePointed_Var_1($5,$6);
                      coupletmp = $6;
                      strcpy(ligne,"");
                      while ( coupletmp )
                      {
                        strcat(ligne, coupletmp->c_namevar);
                        strcat(ligne, " => ");
                        strcat(ligne, coupletmp->c_namepointedvar);
                        coupletmp = coupletmp->suiv;
                        if ( coupletmp ) strcat(ligne,",");
                      }
                      }
                  sprintf(charusemodule,"%s",$5);
                }
                Add_NameOfModuleUsed_1($5);
            }
            else
            {
                if ( insubroutinedeclare )
                {
                  copyuse_0($5);
                    }

                if ( inmoduledeclare == 0 )
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,my_position,pos_end-my_position);
                }
            }
    }
    line-break
    | get_my_position TOK_USE save_olduse opt-module-nature-2points TOK_NAME ',' TOK_ONLY ':' opt-only-list
    {
            if ( firstpass )
            {
                if ( insubroutinedeclare )
                {
                  if ($9)
                  {
                    Add_CouplePointed_Var_1($5,$9);
                    coupletmp = $9;
                    strcpy(ligne,"");
                    while ( coupletmp )
                    {
                        strcat(ligne,coupletmp->c_namevar);
                        if ( strcasecmp(coupletmp->c_namepointedvar,"") )   strcat(ligne," => ");
                        strcat(ligne,coupletmp->c_namepointedvar);
                        coupletmp = coupletmp->suiv;
                        if ( coupletmp ) strcat(ligne,",");
                    }
                  }
                  sprintf(charusemodule,"%s",$5);
                }
                Add_NameOfModuleUsed_1($5);
            }
            else
            {
                if ( insubroutinedeclare )
                    copyuseonly_0($5);

                if ( inmoduledeclare == 0 )
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,my_position,pos_end-my_position);
                    if ($9)
                    {
                    if (oldfortran_out)  variableisglobalinmodule($9,$5,oldfortran_out,pos_curuseold);
                    }
                }
                else
                {
                  if ($9)
                  {
                    /* if we are in the module declare and if the    */
                    /* onlylist is a list of global variable         */
                    variableisglobalinmodule($9, $5, fortran_out,my_position);
                  }
                }
            }
    }
    line-break
    ;

opt-module-nature-2points:
    | TOK_FOURDOTS
    | ',' module-nature TOK_FOURDOTS
    ;

opt-only-list:
    {$$=NULL;}
    | only-list
    {$$=$1;}
    ;

/* R1101 : main-program */
main-program: program-stmt opt-specification-part opt-execution-part opt-internal-subprogram-part end-program-stmt
     ;

opt-specification-part:
     | specification-part
     ;

opt-execution-part:
     | execution-part
     ;

/* R1102 : program-stmt */
program-stmt: TOK_PROGRAM TOK_NAME 
        {
            strcpy(subroutinename,$2);
            insubroutinedeclare = 1;
            inprogramdeclare = 1;
            /* in the second step we should write the head of       */
            /*    the subroutine sub_loop_<subroutinename>          */
            if ( ! firstpass )
                WriteBeginof_SubLoop();
        }
        line-break
     ;

/* R1103 : end-program-stmt */
end-program-stmt: {pos_endsubroutine=my_position_before;} TOK_ENDUNIT opt-tok-program opt-tok-name
     {
            insubroutinedeclare = 0;
            inprogramdeclare = 0;
            pos_cur = setposcur();
            closeandcallsubloopandincludeit_0(3);
            functiondeclarationisdone = 0;
            strcpy(subroutinename,"");     
     }     
     line-break
     ;

opt-tok-program:
     | TOK_PROGRAM
     ;
opt-tok-name:
     | TOK_NAME
     ;
/* R1110 : module-nature */
module-nature: TOK_INTRINSIC
    ;

opt-rename-list:
    {
    $$=NULL;
    }
    | ',' rename-list
    {
    $$=$2;
    }
    ;
    
rename-list: rename
     {
     $$=$1;
     }
     | rename-list ',' rename
     {
     /* insert the variable in the list $1                 */
     $3->suiv = $1;
     $$=$3;
     }
     ;

/* R1111: rename */
rename: TOK_NAME TOK_POINT_TO TOK_NAME
        {
            coupletmp = (listcouple *) calloc(1,sizeof(listcouple));
            strcpy(coupletmp->c_namevar,$1);
            strcpy(coupletmp->c_namepointedvar,$3);
            coupletmp->suiv = NULL;
            $$ = coupletmp;
        }
     ;

only-list:only
     {$$=$1;}
     | only-list ',' only
        {
            /* insert the variable in the list $1                 */
            $3->suiv = $1;
            $$ = $3;
        }
     ;

/* R1112: only */
only:generic-spec
        {
            coupletmp = (listcouple *)calloc(1,sizeof(listcouple));
            strcpy(coupletmp->c_namevar,$1);
            strcpy(coupletmp->c_namepointedvar,"");
            coupletmp->suiv = NULL;
            $$ = coupletmp;
        }
     | only-use-name
        {
            coupletmp = (listcouple *)calloc(1,sizeof(listcouple));
            strcpy(coupletmp->c_namevar,$1);
            strcpy(coupletmp->c_namepointedvar,"");
            coupletmp->suiv = NULL;
            $$ = coupletmp;
        }
     | rename
     {
     $$=$1;
     pointedvar = 1;
      Add_UsedInSubroutine_Var_1($1->c_namevar);
     }
     ;
/* R1113 : only-use-name */
only-use-name: TOK_NAME
     ;

/* R1207: generic-spec */
generic-spec: TOK_NAME
     ;

/* R1210 : external-stmt */
external-stmt: TOK_EXTERNAL external-name-list line-break
     | TOK_EXTERNAL TOK_FOURDOTS external-name-list line-break
     ;
     
external-name-list: external-name
     | external-name-list ',' external-name
     ;
     
external-name: TOK_NAME
     ;

/* R1218 : intrinsic-stmt */
intrinsic-stmt: TOK_INTRINSIC opt-TOK_FOURDOTS intrinsic-procedure-name-list line-break
     ;

intrinsic-procedure-name-list:
        intrinsic-procedure-name
      | intrinsic-procedure-name-list ',' intrinsic-procedure-name
      ;
      
intrinsic-procedure-name: TOK_NAME
     ;

/* R1219 : function-reference */
function-reference: procedure-designator '(' ')'
     | procedure-designator '(' {in_complex_literal=0;} actual-arg-spec-list ')'
     {sprintf($$,"%s(%s)",$[procedure-designator],$[actual-arg-spec-list]);}
     ;

/* R1220 : 
*/
call-stmt: before-call-stmt
             {
            inagrifcallargument = 0 ;
            incalldeclare=0;
            if ( oldfortran_out && (callagrifinitgrids == 1) && (firstpass == 0) )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curcall,pos_end-pos_curcall);
                strcpy(subofagrifinitgrids,subroutinename);
            }
            Instanciation_0(sameagrifname);
        }
        line-break
     | before-call-stmt '(' ')'
             {
            inagrifcallargument = 0 ;
            incalldeclare=0;
            if ( oldfortran_out && (callagrifinitgrids == 1) && (firstpass == 0) )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curcall,pos_end-pos_curcall);
                strcpy(subofagrifinitgrids,subroutinename);
            }
            Instanciation_0(sameagrifname);
        }
        line-break
     | before-call-stmt '(' {in_complex_literal=0;} actual-arg-spec-list ')'
        {
            inagrifcallargument = 0 ;
            incalldeclare=0;
            if ( oldfortran_out && (callagrifinitgrids == 1) && (firstpass == 0) )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curcall,pos_end-pos_curcall);
                strcpy(subofagrifinitgrids,subroutinename);
            }
            Instanciation_0(sameagrifname);
        }
        line-break
     ;

before-call-stmt: opt-label TOK_CALL {pos_curcall=my_position_before-strlen($[opt-label])-4;} procedure-designator
             {
            if (!strcasecmp($[procedure-designator],"MPI_Init") )    callmpiinit = 1;
            else                                callmpiinit = 0;

            if (!strcasecmp($[procedure-designator],"Agrif_Init_Grids") )
            {
                callagrifinitgrids = 1;
                strcpy(meetagrifinitgrids,subroutinename);
            }
            else
            {
                callagrifinitgrids = 0;
            }
            if ( Vartonumber($[procedure-designator]) == 1 )
            {
                incalldeclare = 0;
                inagrifcallargument = 0 ;
                Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
            }
        }
        ;

/* R1221 : procedure-designator */
procedure-designator: ident
     | TOK_FLUSH
     | TOK_REAL
     ;

actual-arg-spec-list:
        actual-arg-spec
      | actual-arg-spec-list ',' actual-arg-spec
      {sprintf($$,"%s,%s",$1,$[actual-arg-spec]);}
      ;

/* R1222 : actual-arg-spec */
actual-arg-spec: actual-arg
        {
            if ( callmpiinit == 1 )
            {
                strcpy(mpiinitvar,$1);
                if ( firstpass == 1 )  Add_UsedInSubroutine_Var_1 (mpiinitvar);
            }
        }     
     | keyword '=' actual-arg
     {sprintf($$,"%s = %s",$1,$3);
                 if ( callmpiinit == 1 )
            {
                strcpy(mpiinitvar,$3);
                if ( firstpass == 1 )  Add_UsedInSubroutine_Var_1 (mpiinitvar);
            }
            }
     ;

/* R1223 : actual-arg */
actual-arg: expr
     | variable
     {
     strcpy($$,$1->v_nomvar);
     if ($1->v_initialvalue_array)
     {
     strcat($$,"(");
     strcat($$,$1->v_initialvalue_array->n_name);
     strcat($$,")");
     }
     }
     | ident
     ;

opt-prefix:     {isrecursive = 0;}
     | prefix
     ;
     
/* R1225 : prefix */
prefix: prefix-spec
     | prefix prefix-spec
     ;

/* R1226 prefix-spec */
prefix-spec: declaration-type-spec
     {isrecursive = 0; functiondeclarationisdone = 1;}
     | TOK_MODULE
     {isrecursive = 0;}
     | TOK_RECURSIVE
     {isrecursive = 1;}
     ;

/*R1227 : function-subprogram */
function-subprogram: function-stmt opt-specification-part opt-execution-part opt-internal-subprogram-part end-function-stmt
     ;

/* R1228 : function-stmt */
function-stmt: opt-prefix TOK_FUNCTION
     function-name '(' {in_complex_literal=0;} opt-dummy-arg-list ')' opt-suffix
     {
            insubroutinedeclare = 1;
            suborfun = 0;
            /* we should to list of the subroutine argument the  */
            /*    name of the function which has to be defined   */
            if ( firstpass )
            {
                Add_SubroutineArgument_Var_1($[opt-dummy-arg-list]);
                if ( ! is_result_present )
                    Add_FunctionType_Var_1($[function-name]);
            }
            else
            /* in the second step we should write the head of    */
            /*    the subroutine sub_loop_<subroutinename>       */
               {
                if (todebug == 1) fprintf(fortran_out,"      !DEBUG: Avant Writebeginof subloop\n");
                WriteBeginof_SubLoop();
                if (todebug == 1) fprintf(fortran_out,"      !DEBUG: Apres Writebeginof subloop\n");
                }
                strcpy(NamePrecision,"");
     }
     line-break
     ;

function-name: TOK_NAME
     {
     if (strcmp(subroutinename,""))
     {
     strcpy(old_subroutinename,subroutinename); // can occur in internal-subprogram
     old_oldfortran_out=oldfortran_out;
     }
     else
     {
     old_oldfortran_out=(FILE *)NULL;
     }
     strcpy($$,$1);strcpy(subroutinename,$1);
     }
     ;

opt-dummy-arg-name-list:
     | dummy-arg-name-list
     ;

dummy-arg-name-list:
        dummy-arg-name
      | dummy-arg-name-list ',' dummy-arg-name
      ;

/* R1230 : dummy-arg-name */
dummy-arg-name: TOK_NAME
     {strcpy($$,$1);}
     ;

opt-suffix:
     {is_result_present = 0; }
     | suffix
     ;
     
/* R1231 : suffix */
suffix: TOK_RESULT '(' TOK_NAME ')'
     {is_result_present = 1;
                 if ( firstpass == 1 )
            {
                strcpy(nameinttypenameback,nameinttypename);
                strcpy(nameinttypename,"");
                curvar = createvar($3,NULL);
                strcpy(nameinttypename,nameinttypenameback);
                strcpy(curvar->v_typevar,"");
                curlistvar = insertvar(NULL,curvar);
                Add_SubroutineArgument_Var_1(curlistvar);
            }
     }
     ;

/* R1232 : end-function-stmt */
end-function-stmt: get_my_position TOK_ENDUNIT opt-tok-function opt-ident close_subroutine
     {strcpy(DeclType, "");}
     line-break
     ;

opt-tok-function:
     | TOK_FUNCTION
     ;

/*R1233 : subroutine-subprogram */
subroutine-subprogram: subroutine-stmt opt-specification-part opt-execution-part opt-internal-subprogram-part end-subroutine-stmt
     ;
     
/* R1234 : subroutine-stmt */
subroutine-stmt: opt-prefix TOK_SUBROUTINE subroutine-name opt-dummy-arg-list-par
        {
            insubroutinedeclare = 1;
            suborfun = 1;
            if ( firstpass )
                Add_SubroutineArgument_Var_1($4);
            else
              {
                WriteBeginof_SubLoop();
              }
        }
        line-break
     ;


subroutine-name: TOK_NAME
     {
     if (strcmp(subroutinename,""))
     {
     strcpy(old_subroutinename,subroutinename); // can occur in internal-subprogram
     old_oldfortran_out=oldfortran_out;
     }
     else
     {
     old_oldfortran_out=(FILE *)NULL;
     }
     strcpy($$,$1);strcpy(subroutinename,$1);
     }
     ;

/* R1236 : end-subroutine-stmt */

end-subroutine-stmt: get_my_position TOK_ENDUNIT opt-tok-subroutine opt-ident close_subroutine
     line-break
     ;

close_subroutine:
          {pos_endsubroutine = my_position;
            GlobalDeclaration = 0 ;
            if ( firstpass == 0 && strcasecmp(subroutinename,"") )
            {
                if ( module_declar && insubroutinedeclare == 0 )    fclose(module_declar);
            }
            if ( strcasecmp(subroutinename,"") )
            {
                if ( inmodulemeet == 1 )
                {
                    /* we are in a module                                */
                    if ( insubroutinedeclare == 1 )
                    {
                        /* it is like an end subroutine <name>            */
                        insubroutinedeclare = 0 ;
                        pos_cur = setposcur();
                        closeandcallsubloopandincludeit_0(suborfun);
                        functiondeclarationisdone = 0;
                    }
                    else
                    {
                        /* it is like an end module <name>                */
                        inmoduledeclare = 0 ;
                        inmodulemeet = 0 ;
                    }
                }
                else
                {
                    insubroutinedeclare = 0;
                    pos_cur = setposcur();
                    closeandcallsubloopandincludeit_0(2);
                    functiondeclarationisdone = 0;
                }
            }
            strcpy(subroutinename,"");
            if (strcmp(old_subroutinename,""))
            {
            strcpy(subroutinename,old_subroutinename);
            strcpy(old_subroutinename,"");
            oldfortran_out=old_oldfortran_out;
            insubroutinedeclare=1;
            }
        }
        ;
opt-tok-subroutine:
     | TOK_SUBROUTINE
     ;

opt-dummy-arg-list-par:
     {if (firstpass) $$=NULL;}
     | '(' {in_complex_literal=0;} opt-dummy-arg-list ')'
     {if (firstpass) $$=$3;}
     ;

opt-dummy-arg-list:
     {if (firstpass) $$=NULL;}
     | dummy-arg-list
     {if (firstpass) $$=$1;}
     ;
     
dummy-arg-list:
        dummy-arg
        {
            if ( firstpass == 1 )
            {
                strcpy(nameinttypenameback,nameinttypename);
                strcpy(nameinttypename,"");
                curvar = createvar($1,NULL);
                strcpy(nameinttypename,nameinttypenameback);
                curlistvar = insertvar(NULL,curvar);
                $$ = settype("",curlistvar);
            }
        }
      | dummy-arg-list ',' dummy-arg
        {
            if ( firstpass == 1 )
            {
                strcpy(nameinttypenameback,nameinttypename);
                strcpy(nameinttypename,"");
                curvar = createvar($3,NULL);
                strcpy(nameinttypename,nameinttypenameback);
                $$ = insertvar($1,curvar);
            }
        }
      ;
      
/* R1235: dummy-arg */
dummy-arg: dummy-arg-name
      {strcpy($$,$1);}
      | '*'
      {strcpy($$,"*");}
      ;
      
/* R1241 : return-stmt */
return-stmt : opt-label TOK_RETURN line-break
     | opt-label TOK_RETURN scalar-int-expr line-break
     ;

/* R1242 : contains-stmt */
contains-stmt: opt-label TOK_CONTAINS
        {
            if ( inside_type_declare ) break;
            if ( inmoduledeclare )
            {
                if ( firstpass == 0 )
                {
                    RemoveWordCUR_0(fortran_out,9);   // Remove word 'contains'
                    Write_Closing_Module(0);
                }
                inmoduledeclare = 0 ;
                aftercontainsdeclare = 1;
            }
            else if ( insubroutinedeclare )
            {
                incontainssubroutine = 1;
                insubroutinedeclare  = 0;
                incontainssubroutine = 0;
                functiondeclarationisdone = 0;

                if ( firstpass )
                    List_ContainsSubroutine = Addtolistnom(subroutinename, List_ContainsSubroutine, 0);
                else
                    closeandcallsubloop_contains_0();

                strcpy(subroutinename, "");
            }
            else printf("l.%4d -- TOK_CONTAINS -- MHCHECK\n",line_num_input);
        }
        line-break
     ;

/* R1243 : stmt-function-stmt */
stmt-function-stmt: TOK_NAME '(' opt-dummy-arg-name-list ')' '=' expr line-break
     ;

opt_name : '\n'  {strcpy($$,"");}
      | TOK_NAME {strcpy($$,$1);}
      ;

before_dims : { created_dimensionlist = 0; }
      ;
ident_dims :
        ident before_dims dims dims
        {
            created_dimensionlist = 1;
            if ( ($3 == NULL) || ($4 == NULL) ) break;
            if  ( agrif_parentcall == 1 )
            {
                ModifyTheAgrifFunction_0($3->dim.last);
                agrif_parentcall = 0;
                fprintf(fortran_out," = ");
            }
        }
      | ident_dims '%' declare_after_percent ident before_dims dims dims
        {
            created_dimensionlist = 1;
        }
      ;
int_list :
        TOK_CSTINT
      | int_list ',' TOK_CSTINT
      ;
after_ident_dims :
        '=' expr
      | TOK_POINT_TO expr
      ;
call :  keywordcall opt_call
        {
            inagrifcallargument = 0 ;
            incalldeclare=0;
            if ( oldfortran_out && (callagrifinitgrids == 1) && (firstpass == 0) )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curcall,pos_end-pos_curcall);
                strcpy(subofagrifinitgrids,subroutinename);
            }
            Instanciation_0(sameagrifname);
        }
      ;
opt_call :
      | '(' opt_callarglist  ')'
      ;
opt_callarglist :
      | callarglist
      ;
keywordcall:
        before_call TOK_FLUSH
      | before_call TOK_NAME
        {
            if (!strcasecmp($2,"MPI_Init") )    callmpiinit = 1;
            else                                callmpiinit = 0;

            if (!strcasecmp($2,"Agrif_Init_Grids") )
            {
                callagrifinitgrids = 1;
                strcpy(meetagrifinitgrids,subroutinename);
            }
            else
            {
                callagrifinitgrids = 0;
            }
            if ( Vartonumber($2) == 1 )
            {
                incalldeclare = 1;
                inagrifcallargument = 1 ;
                Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
            }
        }
      ;
before_call : TOK_CALL  { pos_curcall=setposcur()-4; }
      | label TOK_CALL  { pos_curcall=setposcur()-4; }
      ;
callarglist :
        callarg
      | callarglist ',' callarg
      ;
callarg :
        expr
        {
            if ( callmpiinit == 1 )
            {
                strcpy(mpiinitvar,$1);
                if ( firstpass == 1 )  Add_UsedInSubroutine_Var_1 (mpiinitvar);
            }
        }
      | '*' TOK_CSTINT
      ;

stop :  TOK_PAUSE
      | TOK_STOP
      ;

option_io_1 :
        infmt ',' inlist
      | infmt

option_io_2 :
        ioctl outlist
      | ioctl

ioctl : '(' ctllist ')'
      ;
after_rewind :
        '(' ident ')'
      | '(' TOK_CSTINT ')'
      | TOK_CSTINT
      | '(' uexpr ')'
      | TOK_NAME
      ;
ctllist :
        ioclause
      | ctllist ',' ioclause
      ;
ioclause :
        fexpr
      | '*'
      | TOK_DASTER
      | ident expr dims
      | ident expr '%' declare_after_percent ident_dims
      | ident '(' triplet ')'
      | ident '*'
      | ident TOK_DASTER
      ;

declare_after_percent:      { afterpercent = 1; }
      ;
iofctl :
      TOK_FLUSH
      ;
infmt :  unpar_fexpr
      | '*'
      ;

write_or_inq :
        TOK_WRITE
      ;

fexpr : unpar_fexpr
      | '(' fexpr ')'
      ;
unpar_fexpr :
        lhs
      | simple_const
      | fexpr addop fexpr %prec '+'
      | fexpr '*' fexpr
      | fexpr TOK_SLASH fexpr
      | fexpr TOK_DASTER fexpr
      | addop fexpr %prec '*'
      | fexpr TOK_DSLASH fexpr
      | TOK_FILE expr
      | TOK_UNIT expr
      | TOK_NML expr
      | TOK_FMT expr
      | TOK_EXIST expr
      | TOK_ERR expr
      | TOK_END expr
      | TOK_NAME '=' expr
      | predefinedfunction
      ;
addop : '+'
      | '-'
      ;
inlist : inelt
      | inlist ',' inelt
      ;
// opt_lhs :
//       | lhs
//       ;
inelt : //opt_lhs opt_operation
        lhs opt_operation
      | '(' inlist ')' opt_operation
      | predefinedfunction opt_operation
      | simple_const opt_operation
      | '(' inlist ',' dospec ')'
      ;
opt_operation :
      | operation
      | opt_operation operation
      ;
outlist :
        complex_const       { strcpy($$,$1); }
      | predefinedfunction  { strcpy($$,$1); }
      | uexpr               { strcpy($$,$1); }
      | other               { strcpy($$,$1); }
      | uexpr   ',' expr    { sprintf($$,"%s,%s",$1,$3); }
      | uexpr   ',' other   { sprintf($$,"%s,%s",$1,$3); }
      | other   ',' expr    { sprintf($$,"%s,%s",$1,$3); }
      | other   ',' other   { sprintf($$,"%s,%s",$1,$3); }
      | outlist ',' expr    { sprintf($$,"%s,%s",$1,$3); }
      | outlist ',' other   { sprintf($$,"%s,%s",$1,$3); }
      ;
other :
        '(' uexpr   ',' dospec ')'    { sprintf($$,"(%s,%s)",$2,$4); }
      | '(' outlist ',' dospec ')'    { sprintf($$,"(%s,%s)",$2,$4); }
      | '(' other   ',' dospec ')'    { sprintf($$,"(%s,%s)",$2,$4); }
dospec :
        TOK_NAME '=' expr ',' expr           { sprintf($$,"%s=%s,%s)",$1,$3,$5);}
      | TOK_NAME '=' expr ',' expr ',' expr  { sprintf($$,"%s=%s,%s,%s)",$1,$3,$5,$7);}
      ;
goto :  TOK_PLAINGOTO '(' expr ',' expr ')' ',' expr
      | TOK_PLAINGOTO TOK_CSTINT
      ;
allocation_list :
        allocate_object
      | allocation_list ',' allocate_object
      ;
allocate_object :
        lhs     { Add_Allocate_Var_1($1,curmodulename); }
      ;
allocate_object_list :
        allocate_object
      | allocate_object_list ',' allocate_object
      ;
opt_stat_spec :
      | ',' TOK_STAT '=' lhs
      ;
pointer_name_list :
        ident
      | pointer_name_list ',' ident
      ;

%%

void process_fortran(const char *input_file)
{
    extern FILE *fortran_in;
    extern FILE *fortran_out;

    char output_file[LONG_FNAME];
    char input_fullpath[LONG_FNAME];

    if ( todebug == 1 ) printf("Firstpass == %d \n", firstpass);

     yydebug=0;
/******************************************************************************/
/*  1-  Open input file                                                       */
/******************************************************************************/

    strcpy(cur_filename, input_file);
    sprintf(input_fullpath, "%s/%s", input_dir, input_file);

    fortran_in = fopen(input_fullpath, "r");
    if (! fortran_in)
    {
        printf("Error : File %s does not exist\n", input_fullpath);
        exit(1);
    }

/******************************************************************************/
/*  2-  Variables initialization                                              */
/******************************************************************************/

    line_num_input = 1;
    PublicDeclare = 0;
    PrivateDeclare = 0;
    ExternalDeclare = 0;
    SaveDeclare = 0;
    pointerdeclare = 0;
    contiguousdeclare = 0;
    optionaldeclare = 0;
    incalldeclare = 0;
    inside_type_declare = 0;
    Allocatabledeclare = 0 ;
    Targetdeclare = 0 ;
    VariableIsParameter =  0 ;
    strcpy(NamePrecision,"");
    c_star = 0 ;
    functiondeclarationisdone = 0;
    insubroutinedeclare = 0 ;
    strcpy(subroutinename," ");
    isrecursive = 0;
    InitialValueGiven = 0 ;
    GlobalDeclarationType = 0;
    inmoduledeclare = 0;
    incontainssubroutine = 0;
    afterpercent = 0;
    aftercontainsdeclare = 1;
    strcpy(nameinttypename,"");

/******************************************************************************/
/*  3-  Parsing of the input file (1 time)                                    */
/******************************************************************************/

    sprintf(output_file, "%s/%s", output_dir, input_file);

    if (firstpass == 0) fortran_out = fopen(output_file,"w");

    fortran_parse();

    if (firstpass == 0) NewModule_Creation_0();
    if (firstpass == 0) fclose(fortran_out);
}
