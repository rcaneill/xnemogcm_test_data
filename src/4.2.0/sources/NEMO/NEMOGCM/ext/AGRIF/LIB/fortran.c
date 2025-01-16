/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.5.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1


/* Substitute the variable and function names.  */
#define yyparse         fortran_parse
#define yylex           fortran_lex
#define yyerror         fortran_error
#define yydebug         fortran_debug
#define yynerrs         fortran_nerrs
#define yylval          fortran_lval
#define yychar          fortran_char

/* First part of user prologue.  */
#line 36 "fortran.y"

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


#line 132 "fortran.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int fortran_debug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    TOK_EQV = 258,
    TOK_NEQV = 259,
    TOK_OR = 260,
    TOK_XOR = 261,
    TOK_AND = 262,
    TOK_NOT = 263,
    TOK_LT = 264,
    TOK_GT = 265,
    TOK_LE = 266,
    TOK_GE = 267,
    TOK_EQ = 268,
    TOK_NE = 269,
    TOK_DSLASH = 270,
    TOK_SLASH = 271,
    TOK_DASTER = 272,
    TOK_SEMICOLON = 273,
    TOK_PARAMETER = 274,
    TOK_RESULT = 275,
    TOK_ONLY = 276,
    TOK_INCLUDE = 277,
    TOK_SUBROUTINE = 278,
    TOK_PROGRAM = 279,
    TOK_FUNCTION = 280,
    TOK_LABEL_FORMAT = 281,
    TOK_LABEL_CONTINUE = 282,
    TOK_LABEL_END_DO = 283,
    TOK_MAX = 284,
    TOK_TANH = 285,
    TOK_COMMENT = 286,
    TOK_WHERE = 287,
    TOK_ELSEWHEREPAR = 288,
    TOK_ELSEWHERE = 289,
    TOK_ENDWHERE = 290,
    TOK_MAXVAL = 291,
    TOK_TRIM = 292,
    TOK_NULL_PTR = 293,
    TOK_SUM = 294,
    TOK_SQRT = 295,
    TOK_CASE = 296,
    TOK_SELECTCASE = 297,
    TOK_FILE = 298,
    TOK_REC = 299,
    TOK_NAME_EQ = 300,
    TOK_IOLENGTH = 301,
    TOK_ACCESS = 302,
    TOK_ACTION = 303,
    TOK_FORM = 304,
    TOK_RECL = 305,
    TOK_STATUS = 306,
    TOK_UNIT = 307,
    TOK_OPENED = 308,
    TOK_FMT = 309,
    TOK_NML = 310,
    TOK_END = 311,
    TOK_EOR = 312,
    TOK_EOF = 313,
    TOK_ERR = 314,
    TOK_POSITION = 315,
    TOK_IOSTAT = 316,
    TOK_IOMSG = 317,
    TOK_EXIST = 318,
    TOK_MIN = 319,
    TOK_FLOAT = 320,
    TOK_EXP = 321,
    TOK_LEN = 322,
    TOK_COS = 323,
    TOK_COSH = 324,
    TOK_ACOS = 325,
    TOK_NINT = 326,
    TOK_CYCLE = 327,
    TOK_SIN = 328,
    TOK_SINH = 329,
    TOK_ASIN = 330,
    TOK_EQUIVALENCE = 331,
    TOK_BACKSPACE = 332,
    TOK_LOG = 333,
    TOK_TAN = 334,
    TOK_ATAN = 335,
    TOK_RECURSIVE = 336,
    TOK_ABS = 337,
    TOK_MOD = 338,
    TOK_SIGN = 339,
    TOK_MINLOC = 340,
    TOK_MAXLOC = 341,
    TOK_EXIT = 342,
    TOK_KIND = 343,
    TOK_MOLD = 344,
    TOK_SOURCE = 345,
    TOK_ERRMSG = 346,
    TOK_MINVAL = 347,
    TOK_PUBLIC = 348,
    TOK_PRIVATE = 349,
    TOK_ALLOCATABLE = 350,
    TOK_CONTIGUOUS = 351,
    TOK_RETURN = 352,
    TOK_THEN = 353,
    TOK_ELSEIF = 354,
    TOK_ELSE = 355,
    TOK_ENDIF = 356,
    TOK_PRINT = 357,
    TOK_PLAINGOTO = 358,
    TOK_LOGICALIF = 359,
    TOK_LOGICALIF_PAR = 360,
    TOK_PLAINDO = 361,
    TOK_CONTAINS = 362,
    TOK_ENDDO = 363,
    TOK_MODULE = 364,
    TOK_ENDMODULE = 365,
    TOK_WHILE = 366,
    TOK_CONCURRENT = 367,
    TOK_ALLOCATE = 368,
    TOK_OPEN = 369,
    TOK_CLOSE = 370,
    TOK_INQUIRE = 371,
    TOK_WRITE_PAR = 372,
    TOK_WRITE = 373,
    TOK_FLUSH = 374,
    TOK_READ_PAR = 375,
    TOK_READ = 376,
    TOK_REWIND = 377,
    TOK_DEALLOCATE = 378,
    TOK_NULLIFY = 379,
    TOK_DIMENSION = 380,
    TOK_ENDSELECT = 381,
    TOK_EXTERNAL = 382,
    TOK_INTENT = 383,
    TOK_INTRINSIC = 384,
    TOK_NAMELIST = 385,
    TOK_DEFAULT = 386,
    TOK_OPTIONAL = 387,
    TOK_POINTER = 388,
    TOK_CONTINUE = 389,
    TOK_SAVE = 390,
    TOK_TARGET = 391,
    TOK_IMPLICIT = 392,
    TOK_NONE = 393,
    TOK_CALL = 394,
    TOK_STAT = 395,
    TOK_POINT_TO = 396,
    TOK_COMMON = 397,
    TOK_GLOBAL = 398,
    TOK_LEFTAB = 399,
    TOK_RIGHTAB = 400,
    TOK_PAUSE = 401,
    TOK_PROCEDURE = 402,
    TOK_STOP = 403,
    TOK_FOURDOTS = 404,
    TOK_HEXA = 405,
    TOK_ASSIGNTYPE = 406,
    TOK_OUT = 407,
    TOK_INOUT = 408,
    TOK_IN = 409,
    TOK_USE = 410,
    TOK_EQUALEQUAL = 411,
    TOK_SLASHEQUAL = 412,
    TOK_INFEQUAL = 413,
    TOK_SUPEQUAL = 414,
    TOK_TRUE = 415,
    TOK_FALSE = 416,
    TOK_LABEL = 417,
    TOK_LABEL_DJVIEW = 418,
    TOK_PLAINDO_LABEL_DJVIEW = 419,
    TOK_PLAINDO_LABEL = 420,
    TOK_TYPE = 421,
    TOK_TYPEPAR = 422,
    TOK_ENDTYPE = 423,
    TOK_COMMACOMPLEX = 424,
    TOK_REAL = 425,
    TOK_INTEGER = 426,
    TOK_LOGICAL = 427,
    TOK_DOUBLEPRECISION = 428,
    TOK_ENDSUBROUTINE = 429,
    TOK_ENDFUNCTION = 430,
    TOK_ENDPROGRAM = 431,
    TOK_ENDUNIT = 432,
    TOK_CHARACTER = 433,
    TOK_CHAR_CONSTANT = 434,
    TOK_CHAR_CUT = 435,
    TOK_DATA = 436,
    TOK_CHAR_MESSAGE = 437,
    TOK_CSTREAL = 438,
    TOK_COMPLEX = 439,
    TOK_DOUBLECOMPLEX = 440,
    TOK_NAME = 441,
    TOK_CSTINT = 442
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 91 "fortran.y"

    char        na[LONG_M];
    listdim     *d;
    listvar     *l;
    listcouple  *lc;
    listname    *lnn;
    typedim     dim1;
    variable    *v;

#line 379 "fortran.tab.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE fortran_lval;

int fortran_parse (void);





#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))

/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   4842

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  204
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  524
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1076
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1744

#define YYUNDEFTOK  2
#define YYMAXUTOK   442


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     198,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,   200,     2,     2,
     194,   195,    21,    19,     3,    20,     2,   199,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     4,     2,
     196,     5,   197,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   202,     2,   203,     2,   201,     2,     2,     2,     2,
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
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   143,   144,   145,   146,   147,   148,   149,   150,
     151,   152,   153,   154,   155,   156,   157,   158,   159,   160,
     161,   162,   163,   164,   165,   166,   167,   168,   169,   170,
     171,   172,   173,   174,   175,   176,   177,   178,   179,   180,
     181,   182,   183,   184,   185,   186,   187,   188,   189,   190,
     191,   192,   193
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   515,   515,   516,   518,   519,   520,   522,   524,   525,
     526,   527,   530,   531,   532,   534,   535,   543,   561,   565,
     566,   567,   571,   572,   585,   853,   854,  1107,  1108,  1109,
    1110,  1111,  1113,  1114,  1118,  1119,  1120,  1121,  1122,  1123,
    1124,  1125,  1126,  1127,  1128,  1129,  1130,  1131,  1132,  1133,
    1134,  1135,  1136,  1137,  1138,  1140,  1141,  1142,  1143,  1146,
    1147,  1150,  1151,  1152,  1156,  1167,  1168,  1169,  1169,  1170,
    1170,  1172,  1173,  1173,  1182,  1194,  1195,  1198,  1199,  1202,
    1203,  1206,  1207,  1208,  1209,  1210,  1211,  1212,  1214,  1261,
    1262,  1263,  1264,  1265,  1266,  1267,  1269,  1272,  1273,  1274,
    1275,  1277,  1278,  1288,  1289,  1341,  1344,  1345,  1370,  1371,
    1375,  1376,  1389,  1390,  1391,  1392,  1393,  1394,  1395,  1396,
    1397,  1398,  1399,  1400,  1401,  1404,  1405,  1409,  1412,  1413,
    1417,  1418,  1422,  1423,  1426,  1427,  1431,  1435,  1436,  1439,
    1440,  1444,  1445,  1449,  1450,  1451,  1452,  1453,  1454,  1455,
    1456,  1457,  1462,  1463,  1464,  1465,  1466,  1474,  1475,  1476,
    1477,  1478,  1479,  1480,  1481,  1482,  1483,  1484,  1485,  1486,
    1508,  1509,  1510,  1511,  1512,  1513,  1514,  1515,  1516,  1517,
    1518,  1519,  1523,  1526,  1531,  1532,  1536,  1537,  1538,  1539,
    1541,  1545,  1564,  1565,  1569,  1570,  1574,  1575,  1579,  1583,
    1584,  1585,  1596,  1596,  1598,  1599,  1604,  1604,  1606,  1606,
    1608,  1608,  1610,  1610,  1612,  1612,  1614,  1614,  1619,  1620,
    1626,  1628,  1630,  1637,  1638,  1643,  1644,  1649,  1650,  1666,
    1667,  1672,  1673,  1680,  1686,  1687,  1688,  1692,  1693,  1694,
    1697,  1698,  1703,  1704,  1709,  1710,  1711,  1712,  1713,  1717,
    1719,  1721,  1722,  1726,  1728,  1733,  1734,  1735,  1739,  1740,
    1744,  1744,  1749,  1750,  1753,  1754,  1757,  1758,  1761,  1762,
    1766,  1769,  1770,  1773,  1777,  1778,  1781,  1782,  1786,  1787,
    1791,  1795,  1798,  1799,  1800,  1803,  1804,  1808,  1809,  1810,
    1811,  1811,  1812,  1815,  1816,  1820,  1844,  1845,  1849,  1850,
    1853,  1854,  1858,  1859,  1860,  1864,  1869,  1871,  1874,  1875,
    1879,  1880,  1884,  1885,  1888,  1889,  1893,  1894,  1898,  1899,
    1900,  1904,  1906,  1921,  1925,  1929,  1933,  1934,  1939,  1940,
    1944,  1949,  1951,  1956,  1960,  1961,  1960,  2029,  2030,  2033,
    2034,  2038,  2039,  2043,  2044,  2046,  2048,  2048,  2050,  2052,
    2052,  2054,  2055,  2057,  2059,  2061,  2063,  2068,  2070,  2075,
    2109,  2112,  2115,  2116,  2120,  2126,  2132,  2141,  2145,  2147,
    2152,  2153,  2153,  2158,  2160,  2162,  2164,  2166,  2170,  2176,
    2185,  2187,  2192,  2197,  2201,  2207,  2216,  2218,  2223,  2229,
    2238,  2243,  2266,  2267,  2286,  2287,  2291,  2292,  2296,  2300,
    2302,  2304,  2310,  2309,  2328,  2329,  2333,  2335,  2340,  2341,
    2346,  2345,  2360,  2361,  2364,  2365,  2369,  2379,  2381,  2387,
    2389,  2394,  2395,  2399,  2405,  2412,  2414,  2419,  2420,  2424,
    2428,  2433,  2435,  2437,  2439,  2440,  2441,  2442,  2443,  2447,
    2448,  2464,  2465,  2466,  2467,  2468,  2469,  2470,  2476,  2484,
    2489,  2491,  2489,  2537,  2537,  2546,  2546,  2559,  2560,  2559,
    2579,  2581,  2586,  2603,  2604,  2603,  2611,  2612,  2615,  2616,
    2619,  2620,  2624,  2626,  2627,  2631,  2635,  2639,  2641,  2640,
    2652,  2653,  2657,  2660,  2661,  2665,  2666,  2670,  2673,  2674,
    2676,  2677,  2681,  2685,  2688,  2689,  2693,  2693,  2696,  2697,
    2701,  2702,  2703,  2708,  2709,  2708,  2718,  2719,  2727,  2733,
    2741,  2742,  2745,  2747,  2746,  2756,  2758,  2766,  2772,  2772,
    2781,  2782,  2783,  2784,  2793,  2796,  2809,  2812,  2816,  2820,
    2823,  2827,  2830,  2833,  2837,  2838,  2840,  2855,  2860,  2865,
    2866,  2871,  2873,  2873,  2885,  2889,  2894,  2899,  2901,  2908,
    2909,  2911,  2933,  2935,  2937,  2939,  2941,  2943,  2945,  2946,
    2948,  2950,  2954,  2956,  2958,  2960,  2962,  2965,  2979,  2983,
    2984,  2983,  2992,  2993,  2997,  2998,  3002,  3003,  3007,  3011,
    3015,  3016,  3020,  3024,  3025,  3028,  3029,  3033,  3034,  3038,
    3041,  3042,  3046,  3050,  3054,  3055,  3054,  3060,  3061,  3064,
    3065,  3069,  3070,  3074,  3075,  3084,  3094,  3095,  3096,  3097,
    3102,  3107,  3108,  3112,  3113,  3120,  3121,  3123,  3125,  3126,
    3131,  3135,  3137,  3141,  3143,  3148,  3149,  3154,  3157,  3158,
    3163,  3164,  3165,  3166,  3167,  3168,  3169,  3170,  3171,  3173,
    3174,  3176,  3181,  3182,  3188,  3189,  3195,  3196,  3201,  3202,
    3207,  3211,  3215,  3219,  3220,  3224,  3227,  3231,  3235,  3239,
    3240,  3243,  3247,  3254,  3258,  3262,  3265,  3269,  3275,  3276,
    3288,  3289,  3290,  3298,  3299,  3303,  3304,  3308,  3309,  3313,
    3317,  3321,  3324,  3333,  3337,  3338,  3339,  3343,  3347,  3350,
    3351,  3354,  3355,  3358,  3359,  3363,  3367,  3368,  3369,  3373,
    3377,  3381,  3382,  3386,  3387,  3392,  3393,  3397,  3401,  3404,
    3405,  3410,  3411,  3415,  3420,  3421,  3432,  3433,  3434,  3435,
    3438,  3439,  3440,  3441,  3445,  3446,  3447,  3448,  3453,  3454,
    3455,  3456,  3460,  3464,  3473,  3474,  3478,  3479,  3490,  3491,
    3497,  3507,  3512,  3513,  3514,  3515,  3516,  3517,  3518,  3519,
    3520,  3521,  3522,  3523,  3524,  3525,  3526,  3527,  3528,  3538,
    3539,  3542,  3543,  3554,  3559,  3562,  3563,  3567,  3571,  3574,
    3575,  3576,  3579,  3582,  3583,  3584,  3587,  3591,  3592,  3593,
    3597,  3598,  3602,  3603,  3607,  3608,  3612,  3616,  3619,  3620,
    3621,  3624,  3628,  3628,  3629,  3629,  3633,  3634,  3638,  3638,
    3639,  3639,  3644,  3644,  3645,  3649,  3650,  3655,  3656,  3657,
    3658,  3662,  3666,  3667,  3671,  3675,  3679,  3683,  3684,  3688,
    3689,  3693,  3694,  3695,  3699,  3703,  3707,  3707,  3707,  3710,
    3711,  3715,  3716,  3717,  3718,  3719,  3720,  3721,  3722,  3723,
    3724,  3725,  3726,  3730,  3734,  3738,  3738,  3742,  3743,  3747,
    3748,  3749,  3750,  3751,  3752,  3757,  3756,  3762,  3761,  3766,
    3767,  3772,  3771,  3777,  3776,  3784,  3785,  3787,  3788,  3791,
    3795,  3796,  3797,  3798,  3799,  3800,  3801,  3802,  3803,  3804,
    3805,  3806,  3810,  3811,  3812,  3815,  3816,  3819,  3820,  3824,
    3825,  3829,  3830,  3834,  3837,  3838,  3848,  3852,  3853,  3857,
    3858,  3862,  3863,  3867,  3868,  3869,  3870,  3871,  3875,  3876,
    3880,  3881,  3885,  3886,  3887,  3888,  3889,  3895,  3894,  3898,
    3897,  3902,  3906,  3907,  3911,  3912,  3913,  3914,  3915,  3916,
    3917,  3918,  3919,  3920,  3921,  3922,  3926,  3930,  3930,  3933,
    3934,  3939,  3938,  3959,  3958,  3983,  3984,  3987,  3988,  3991,
    3994,  3995,  3998,  3999,  4002,  4003,  4006,  4007,  4011,  4016,
    4015,  4054,  4053,  4105,  4106,  4107,  4111,  4112,  4117,  4120,
    4121,  4124,  4125,  4130,  4129,  4143,  4144,  4143,  4155,  4156,
    4158,  4159,  4162,  4166,  4169,  4175,  4179,  4188,  4198,  4200,
    4209,  4217,  4225,  4233,  4237,  4241,  4242,  4245,  4246,  4249,
    4253,  4257,  4258,  4261,  4265,  4266,  4266,  4273,  4272,  4286,
    4285,  4298,  4299,  4298,  4313,  4313,  4337,  4338,  4339,  4343,
    4344,  4349,  4357,  4368,  4369,  4379,  4382,  4383,  4387,  4388,
    4392,  4394,  4396,  4401,  4406,  4407,  4405,  4431,  4456,  4461,
    4462,  4466,  4483,  4482,  4487,  4488,  4492,  4497,  4496,  4511,
    4528,  4533,  4577,  4578,  4582,  4583,  4583,  4588,  4589,  4594,
    4606,  4620,  4622,  4627,  4628,  4633,  4632,  4668,  4669,  4776,
    4777,  4778,  4779,  4780,  4797,  4890,  4891
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "','", "':'", "'='", "TOK_EQV",
  "TOK_NEQV", "TOK_OR", "TOK_XOR", "TOK_AND", "TOK_NOT", "TOK_LT",
  "TOK_GT", "TOK_LE", "TOK_GE", "TOK_EQ", "TOK_NE", "TOK_DSLASH", "'+'",
  "'-'", "'*'", "TOK_SLASH", "TOK_DASTER", "TOK_SEMICOLON",
  "TOK_PARAMETER", "TOK_RESULT", "TOK_ONLY", "TOK_INCLUDE",
  "TOK_SUBROUTINE", "TOK_PROGRAM", "TOK_FUNCTION", "TOK_LABEL_FORMAT",
  "TOK_LABEL_CONTINUE", "TOK_LABEL_END_DO", "TOK_MAX", "TOK_TANH",
  "TOK_COMMENT", "TOK_WHERE", "TOK_ELSEWHEREPAR", "TOK_ELSEWHERE",
  "TOK_ENDWHERE", "TOK_MAXVAL", "TOK_TRIM", "TOK_NULL_PTR", "TOK_SUM",
  "TOK_SQRT", "TOK_CASE", "TOK_SELECTCASE", "TOK_FILE", "TOK_REC",
  "TOK_NAME_EQ", "TOK_IOLENGTH", "TOK_ACCESS", "TOK_ACTION", "TOK_FORM",
  "TOK_RECL", "TOK_STATUS", "TOK_UNIT", "TOK_OPENED", "TOK_FMT", "TOK_NML",
  "TOK_END", "TOK_EOR", "TOK_EOF", "TOK_ERR", "TOK_POSITION", "TOK_IOSTAT",
  "TOK_IOMSG", "TOK_EXIST", "TOK_MIN", "TOK_FLOAT", "TOK_EXP", "TOK_LEN",
  "TOK_COS", "TOK_COSH", "TOK_ACOS", "TOK_NINT", "TOK_CYCLE", "TOK_SIN",
  "TOK_SINH", "TOK_ASIN", "TOK_EQUIVALENCE", "TOK_BACKSPACE", "TOK_LOG",
  "TOK_TAN", "TOK_ATAN", "TOK_RECURSIVE", "TOK_ABS", "TOK_MOD", "TOK_SIGN",
  "TOK_MINLOC", "TOK_MAXLOC", "TOK_EXIT", "TOK_KIND", "TOK_MOLD",
  "TOK_SOURCE", "TOK_ERRMSG", "TOK_MINVAL", "TOK_PUBLIC", "TOK_PRIVATE",
  "TOK_ALLOCATABLE", "TOK_CONTIGUOUS", "TOK_RETURN", "TOK_THEN",
  "TOK_ELSEIF", "TOK_ELSE", "TOK_ENDIF", "TOK_PRINT", "TOK_PLAINGOTO",
  "TOK_LOGICALIF", "TOK_LOGICALIF_PAR", "TOK_PLAINDO", "TOK_CONTAINS",
  "TOK_ENDDO", "TOK_MODULE", "TOK_ENDMODULE", "TOK_WHILE",
  "TOK_CONCURRENT", "TOK_ALLOCATE", "TOK_OPEN", "TOK_CLOSE", "TOK_INQUIRE",
  "TOK_WRITE_PAR", "TOK_WRITE", "TOK_FLUSH", "TOK_READ_PAR", "TOK_READ",
  "TOK_REWIND", "TOK_DEALLOCATE", "TOK_NULLIFY", "TOK_DIMENSION",
  "TOK_ENDSELECT", "TOK_EXTERNAL", "TOK_INTENT", "TOK_INTRINSIC",
  "TOK_NAMELIST", "TOK_DEFAULT", "TOK_OPTIONAL", "TOK_POINTER",
  "TOK_CONTINUE", "TOK_SAVE", "TOK_TARGET", "TOK_IMPLICIT", "TOK_NONE",
  "TOK_CALL", "TOK_STAT", "TOK_POINT_TO", "TOK_COMMON", "TOK_GLOBAL",
  "TOK_LEFTAB", "TOK_RIGHTAB", "TOK_PAUSE", "TOK_PROCEDURE", "TOK_STOP",
  "TOK_FOURDOTS", "TOK_HEXA", "TOK_ASSIGNTYPE", "TOK_OUT", "TOK_INOUT",
  "TOK_IN", "TOK_USE", "TOK_EQUALEQUAL", "TOK_SLASHEQUAL", "TOK_INFEQUAL",
  "TOK_SUPEQUAL", "TOK_TRUE", "TOK_FALSE", "TOK_LABEL", "TOK_LABEL_DJVIEW",
  "TOK_PLAINDO_LABEL_DJVIEW", "TOK_PLAINDO_LABEL", "TOK_TYPE",
  "TOK_TYPEPAR", "TOK_ENDTYPE", "TOK_COMMACOMPLEX", "TOK_REAL",
  "TOK_INTEGER", "TOK_LOGICAL", "TOK_DOUBLEPRECISION", "TOK_ENDSUBROUTINE",
  "TOK_ENDFUNCTION", "TOK_ENDPROGRAM", "TOK_ENDUNIT", "TOK_CHARACTER",
  "TOK_CHAR_CONSTANT", "TOK_CHAR_CUT", "TOK_DATA", "TOK_CHAR_MESSAGE",
  "TOK_CSTREAL", "TOK_COMPLEX", "TOK_DOUBLECOMPLEX", "TOK_NAME",
  "TOK_CSTINT", "'('", "')'", "'<'", "'>'", "'\\n'", "'/'", "'%'", "'_'",
  "'['", "']'", "$accept", "input", "line", "line-break",
  "suite_line_list", "suite_line", "fin_line", "program-unit",
  "external-subprogram", "filename", "opt_comma", "uexpr", "signe",
  "operation", "after_slash", "after_equal", "lhs", "beforefunctionuse",
  "array_ele_substring_func_ref", "$@4", "$@5", "begin_array", "$@6",
  "structure_component", "funarglist", "funargs", "funarg", "triplet",
  "ident", "simple_const", "string_constant", "opt_substring", "opt_expr",
  "specification-part", "opt-use-stmt-list",
  "opt-declaration-construct-list", "declaration-construct-list",
  "declaration-construct", "opt-execution-part", "execution-part",
  "opt-execution-part-construct-list", "execution-part-construct-list",
  "execution-part-construct", "opt-internal-subprogram-part",
  "internal-subprogram-part", "opt-internal-subprogram",
  "internal-subprogram-list", "internal-subprogram",
  "other-specification-stmt", "executable-construct", "action-stmt",
  "keyword", "scalar-constant", "constant", "literal-constant",
  "named-constant", "opt-label", "label", "opt-label-djview",
  "label-djview", "type-param-value", "declaration-type-spec", "$@7",
  "intrinsic-type-spec", "$@8", "$@9", "$@10", "$@11", "$@12", "$@13",
  "opt-kind-selector", "kind-selector", "signed-int-literal-constant",
  "int-literal-constant", "kind-param", "signed-real-literal-constant",
  "real-literal-constant", "complex-literal-constant", "real-part",
  "imag-part", "opt-char_length-star", "opt-char-selector",
  "char-selector", "length-selector", "char-length",
  "char-literal-constant", "logical-literal-constant", "derived-type-def",
  "$@14", "derived-type-stmt", "opt-type-attr-spec-list-comma-fourdots",
  "opt-type-attr-spec-list-comma", "type-attr-spec-list", "type-attr-spec",
  "type-param-name-list", "type-param-name", "end-type-stmt",
  "opt-component-part", "component-part", "component-def-stmt",
  "data-component-def-stmt", "opt-component-attr-spec-list-comma-2points",
  "component-attr-spec-list", "component-attr-spec", "$@15",
  "component-decl-list", "component-decl", "opt-component-array-spec",
  "component-array-spec", "opt-component-initialization",
  "component-initialization", "initial-data-target", "derived-type-spec",
  "type-param-spec-list", "type-param-spec", "structure-constructor",
  "component-spec-list", "component-spec", "component-data-source",
  "array-constructor", "ac-spec", "lbracket", "rbracket", "ac-value-list",
  "ac-value", "ac-implied-do", "ac-implied-do-control", "ac-do-variable",
  "type-declaration-stmt", "$@16", "$@17", "opt-attr-spec-construct",
  "opt-attr-spec-comma-list", "attr-spec-comma-list", "attr-spec", "$@18",
  "$@19", "entity-decl-list", "entity-decl", "object-name",
  "object-name-noident", "opt-initialization", "initialization",
  "null-init", "access-spec", "opt-array-spec-par", "$@20", "array-spec",
  "explicit-shape-spec-list", "explicit-shape-spec", "lower-bound",
  "upper-bound", "assumed-shape-spec-list", "assumed-shape-spec",
  "deferred-shape-spec-list", "deferred-shape-spec", "assumed-size-spec",
  "opt-explicit-shape-spec-list-comma", "opt-lower-bound-2points",
  "implied-shape-spec-list", "implied-shape-spec", "intent-spec",
  "access-stmt", "$@21", "opt-access-id-list", "access-id-list",
  "access-id", "data-stmt", "$@22", "opt-data-stmt-set-nlist",
  "data-stmt-set-nlist", "data-stmt-set", "data-stmt-object-list",
  "data-stmt-value-list", "data-stmt-object", "data-implied-do",
  "data-i-do-object-list", "data-i-do-object", "data-i-do-variable",
  "data-stmt-value", "opt-data-stmt-star", "data-stmt-constant",
  "scalar-constant-subobject", "constant-subobject", "dimension-stmt",
  "$@23", "$@24", "array-name-spec-list", "$@25", "$@26", "parameter-stmt",
  "$@27", "$@28", "named-constant-def-list", "named-constant-def",
  "save-stmt", "$@29", "$@30", "opt-TOK_FOURDOTS", "opt-saved-entity-list",
  "saved-entity-list", "saved-entity", "proc-pointer-name",
  "get_my_position", "implicit-stmt", "$@31", "implicit-spec-list",
  "implicit-spec", "letter-spec-list", "letter-spec", "namelist-stmt",
  "opt-namelist-other", "namelist-group-object-list",
  "namelist-group-object", "equivalence-stmt", "equivalence-set-list",
  "equivalence-set", "$@32", "equivalence-object-list",
  "equivalence-object", "common-stmt", "$@33", "$@34",
  "opt-common-block-name", "common-block-name", "opt-comma",
  "opt-common-block-list", "$@35", "common-block-object-list",
  "common-block-object", "$@36", "designator", "scalar-variable",
  "variable", "variable-name", "scalar-logical-variable",
  "logical-variable", "char-variable", "scalar-default-char-variable",
  "default-char-variable", "scalar-int-variable", "int-variable",
  "substring", "substring-range", "data-ref", "opt-part-ref", "part-ref",
  "$@37", "scalar-structure-component", "structure-component",
  "array-element", "array-section", "section-subscript-list",
  "section-subscript", "section_subscript_ambiguous", "vector-subscript",
  "allocate-stmt", "$@38", "$@39", "opt-alloc-opt-list-comma",
  "alloc-opt-list", "alloc-opt", "stat-variable", "errmsg-variable",
  "allocation-list", "allocation", "allocate-object",
  "opt-allocate-shape-spec-list-par", "allocate-shape-spec-list",
  "allocate-shape-spec", "opt-lower-bound-expr", "lower-bound-expr",
  "upper-bound-expr", "deallocate-stmt", "$@40", "$@41",
  "allocate-object-list", "opt-dealloc-opt-list-comma", "dealloc-opt-list",
  "dealloc-opt", "primary", "level-1-expr", "mult-operand", "add-operand",
  "level-2-expr", "power-op", "mult-op", "add-op", "level-3-expr",
  "concat-op", "level-4-expr", "rel-op", "and-operand", "or-operand",
  "equiv-operand", "level-5-expr", "not-op", "and-op", "or-op", "equiv-op",
  "expr", "scalar-default-char-expr", "default-char-expr", "int-expr",
  "opt-scalar-int-expr", "scalar-int-expr", "specification-expr",
  "constant-expr", "scalar-default-char-constant-expr",
  "default-char-constant-expr", "scalar-int-constant-expr",
  "int-constant-expr", "assignment-stmt", "pointer-assignment-stmt",
  "opt-bounds-spec-list-par", "bounds-spec-list", "bounds-remapping-list",
  "bounds-spec", "bounds-remapping", "data-target",
  "procedure-component-name", "proc-component-ref", "proc-target",
  "where-stmt", "where-construct", "opt-where-body-construct",
  "opt-masked-elsewhere-construct", "opt-elsewhere-construct",
  "where-construct-stmt", "where-body-construct", "where-assignment-stmt",
  "mask-expr", "masked-elsewhere-stmt", "elsewhere-stmt", "end-where-stmt",
  "forall-header", "block", "opt-execution-part-construct", "do-construct",
  "block-do-construct", "label-do-stmt", "label-do-stmt-djview",
  "nonlabel-do-stmt", "loop-control", "do-variable", "do-block", "end-do",
  "end-do-stmt", "nonblock-do-construct", "action-term-do-construct",
  "do-term-action-stmt", "do-term-action-stmt-special",
  "outer-shared-do-construct", "label-do-stmt-djview-do-block-list",
  "inner-shared-do-construct", "do-term-shared-stmt",
  "opt-do-construct-name", "cycle-stmt", "if-construct",
  "opt-else-if-stmt-block", "else-if-stmt-block", "opt-else-stmt-block",
  "else-stmt-block", "if-then-stmt", "else-if-stmt", "else-stmt",
  "end-if-stmt", "if-stmt", "case-construct", "opt_case-stmt-block",
  "case-stmt-block", "select-case-stmt", "$@42", "$@43", "case-stmt",
  "end-select-stmt", "$@44", "$@45", "case-selector", "$@46",
  "case-value-range-list", "case-value-range", "case-value", "exit-stmt",
  "goto-stmt", "arithmetic-if-stmt", "continue-stmt", "stop-stmt",
  "stop-code", "io-unit", "file-unit-number", "internal-file-variable",
  "open-stmt", "$@47", "$@48", "connect-spec-list", "connect-spec",
  "file-name-expr", "iomsg-variable", "close-stmt", "$@49",
  "close-spec-list", "close-spec", "read-stmt", "$@50", "$@51",
  "write-stmt", "$@52", "$@53", "print-stmt", "io-control-spec-list",
  "namelist-group-name", "io-control-spec", "format", "input-item-list",
  "input-item", "output-item-list", "output-item", "io-implied-do",
  "io-implied-do-object-list", "io-implied-do-object",
  "io-implied-do-control", "rewind-stmt", "position-spec-list",
  "position-spec", "flush-stmt", "flush-spec-list", "flush-spec",
  "inquire-stmt", "$@54", "$@55", "set_in_inquire", "inquire-spec-list",
  "inquire-spec", "format-stmt", "module", "$@56",
  "opt-module-subprogram-part", "module-stmt", "$@57", "end-module-stmt",
  "$@58", "opt-tok-module", "opt-ident", "module-subprogram-part",
  "opt-module-subprogram-list", "module-subprogram-list",
  "module-subprogram", "use-stmt-list", "save_olduse", "use-stmt", "$@59",
  "$@60", "opt-module-nature-2points", "opt-only-list", "main-program",
  "opt-specification-part", "program-stmt", "$@61", "end-program-stmt",
  "$@62", "$@63", "opt-tok-program", "opt-tok-name", "module-nature",
  "opt-rename-list", "rename-list", "rename", "only-list", "only",
  "only-use-name", "generic-spec", "external-stmt", "external-name-list",
  "external-name", "intrinsic-stmt", "intrinsic-procedure-name-list",
  "intrinsic-procedure-name", "function-reference", "$@64", "call-stmt",
  "$@65", "$@66", "$@67", "$@68", "before-call-stmt", "$@69",
  "procedure-designator", "actual-arg-spec-list", "actual-arg-spec",
  "actual-arg", "opt-prefix", "prefix", "prefix-spec",
  "function-subprogram", "function-stmt", "$@70", "$@71", "function-name",
  "dummy-arg-name", "opt-suffix", "suffix", "end-function-stmt", "$@72",
  "opt-tok-function", "subroutine-subprogram", "subroutine-stmt", "$@73",
  "subroutine-name", "end-subroutine-stmt", "close_subroutine",
  "opt-tok-subroutine", "opt-dummy-arg-list-par", "$@74",
  "opt-dummy-arg-list", "dummy-arg-list", "dummy-arg", "return-stmt",
  "contains-stmt", "$@75", "opt_name", "after_rewind",
  "declare_after_percent", "pointer_name_list", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   257,    44,    58,    61,   258,   259,   260,   261,
     262,   263,   264,   265,   266,   267,   268,   269,   270,    43,
      45,    42,   271,   272,   273,   274,   275,   276,   277,   278,
     279,   280,   281,   282,   283,   284,   285,   286,   287,   288,
     289,   290,   291,   292,   293,   294,   295,   296,   297,   298,
     299,   300,   301,   302,   303,   304,   305,   306,   307,   308,
     309,   310,   311,   312,   313,   314,   315,   316,   317,   318,
     319,   320,   321,   322,   323,   324,   325,   326,   327,   328,
     329,   330,   331,   332,   333,   334,   335,   336,   337,   338,
     339,   340,   341,   342,   343,   344,   345,   346,   347,   348,
     349,   350,   351,   352,   353,   354,   355,   356,   357,   358,
     359,   360,   361,   362,   363,   364,   365,   366,   367,   368,
     369,   370,   371,   372,   373,   374,   375,   376,   377,   378,
     379,   380,   381,   382,   383,   384,   385,   386,   387,   388,
     389,   390,   391,   392,   393,   394,   395,   396,   397,   398,
     399,   400,   401,   402,   403,   404,   405,   406,   407,   408,
     409,   410,   411,   412,   413,   414,   415,   416,   417,   418,
     419,   420,   421,   422,   423,   424,   425,   426,   427,   428,
     429,   430,   431,   432,   433,   434,   435,   436,   437,   438,
     439,   440,   441,   442,    40,    41,    60,    62,    10,    47,
      37,    95,    91,    93
};
# endif

#define YYPACT_NINF (-1417)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1028)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
   -1417,  1551, -1417, -1417, -1417,   -21,    26, -1417, -1417, -1417,
      36,   734, -1417, -1417,    81,   230, -1417, -1417, -1417, -1417,
     801, -1417,   252, -1417,   252,   338,   602, -1417, -1417,   252,
   -1417,   252, -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417,
   -1417, -1417, -1417,   231,   263,   294, -1417, -1417, -1417,   810,
   -1417, -1417,  4236,   373,   252, -1417,   523,  4536,   385,   401,
   -1417, -1417,  4536,  4536, -1417,   188,   188,    80,    80,    80,
      80,   147,    80,  1612, -1417, -1417, -1417, -1417, -1417, -1417,
     188,   412, -1417, -1417,    94,   316,   481,   619, -1417, -1417,
      94,   112, -1417, -1417,   910, -1417,   645, -1417,   460, -1417,
    4236, -1417, -1417,   515,   922,   490, -1417, -1417, -1417,   526,
     360, -1417, -1417, -1417,   587, -1417, -1417,   604,   615, -1417,
   -1417, -1417, -1417,   370,   763, -1417,   612, -1417, -1417, -1417,
   -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417,
   -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417,
     685, -1417, -1417, -1417,   873,   616,   633,  3106,   222,   494,
     -44,   660,   662, -1417,  3951,  3996,   674,   678,  3732,   800,
     700, -1417,  4423, -1417,  1168, -1417, -1417, -1417, -1417, -1417,
   -1417, -1417, -1417, -1417, -1417, -1417, -1417,   830, -1417, -1417,
   -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417,
   -1417, -1417,   691, -1417, -1417,   693, -1417,   694,   700,   700,
      81,    81,   686,  3806, -1417, -1417, -1417, -1417, -1417,   319,
    1072, -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417,
   -1417,  4007, -1417, -1417, -1417,   688,   705,  4062, -1417,    93,
     896, -1417, -1417, -1417,   732, -1417, -1417,   490, -1417,   105,
   -1417, -1417,  4007, -1417, -1417,   885, -1417,   598,   240,  1001,
     878, -1417, -1417,   905,   908,   701,   472, -1417, -1417, -1417,
   -1417,   728,   737,    81, -1417,   103, -1417, -1417,    81,   450,
     188,   747, -1417,   117, -1417, -1417,   751,   752,   664,    81,
     188,   625,   753,   480,   394,   121,   525, -1417, -1417, -1417,
   -1417,   323, -1417, -1417,  3732,  3772,  4062,   188,   944,   945,
    4062,   560,   221, -1417,   759,   481,   481,   224,  4086,  4062,
     805,  4062,  4062,   754, -1417,  4338,   238,   787,   852,   262,
   -1417, -1417, -1417,   911, -1417, -1417, -1417,  4062,  4062,   313,
     460, -1417, -1417,   188,   188,    81,   188, -1417, -1417, -1417,
   -1417, -1417,   765,  3006, -1417,   188,  3682,   188, -1417,   774,
      81, -1417, -1417, -1417, -1417, -1417, -1417, -1417,   188,   411,
     188, -1417, -1417, -1417,  4451, -1417, -1417, -1417,  4062,   779,
    3400,  3400,  3772, -1417,   804,     5,    89, -1417, -1417,   775,
     188, -1417, -1417, -1417, -1417, -1417, -1417,   970,   786,  1612,
   -1417, -1417,   979,   982,   109,  4007,   842,   991, -1417, -1417,
   -1417,   586,   586,   518,   826, -1417,   832,   835,  1001,   817,
    1612,  1612, -1417,   802, -1417,  1001, -1417, -1417,  1001, -1417,
   -1417,  1001,   840,   598, -1417, -1417, -1417, -1417, -1417, -1417,
   -1417, -1417, -1417, -1417, -1417, -1417, -1417,   472,   472, -1417,
    4062, -1417,  4062, -1417, -1417,  4062, -1417,   824,   838,   927,
     412,    81,   833, -1417, -1417,  1028,    81,   117,   747,    81,
   -1417,   122, -1417,  1012, -1417,   844,   845, -1417,    81,  1035,
   -1417, -1417,   188, -1417,   851, -1417,  1044, -1417, -1417, -1417,
   -1417, -1417, -1417, -1417,   125,   910,   910,   584,  4062,    94,
      94,  1581,    81,   188, -1417,   111, -1417, -1417, -1417,   131,
     854,    81,   939,  4062,   868,  1061, -1417,   266,   892,   609,
   -1417, -1417,  1222,   880,   915,  1070,   188, -1417,  1074, -1417,
   -1417,   887,   244, -1417,   891,   159, -1417, -1417,   107,   881,
   -1417, -1417, -1417, -1417,   188,  1082, -1417,   123,   127, -1417,
   -1417,   927,   188,   893,   774, -1417, -1417,    94,  1083,   985,
    1850, -1417, -1417, -1417, -1417,   -10, -1417,   432, -1417,   909,
     716, -1417, -1417,   974, -1417,   913,   188,   934, -1417, -1417,
   -1417,   929,   930,    81,    81,    81,   774,  3542,  3095,  4062,
     494,   927,   927,   817, -1417,   129, -1417,    81,  4062,   494,
     927,   927, -1417,   130, -1417,    81,   774, -1417,   136,    81,
     936,   563, -1417,   949, -1417,   946, -1417, -1417,  1137,  3843,
    3772,   954,   494,   494,   494,   927,   927, -1417, -1417, -1417,
   -1417, -1417, -1417,   137, -1417, -1417, -1417,   141,   167,   382,
     927, -1417, -1417, -1417,  1110, -1417, -1417, -1417, -1417,   363,
     962, -1417, -1417, -1417, -1417,  4062,    81,   223,   188,   223,
     972, -1417,   981, -1417,  4062, -1417,   975,  1612,  4062,  4062,
   -1417,  1162,   817, -1417,  4007, -1417, -1417, -1417, -1417,    84,
     994, -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417,   598,
     240,  1153, -1417,   905,   908, -1417,  4062,  1167,   144, -1417,
     452,  1171, -1417, -1417,   984, -1417, -1417,  4062, -1417,  4062,
      81, -1417,   751,    81,   774,  1158,   988,  1185, -1417,   625,
      81,   999,   394,   188,   910, -1417, -1417,   998, -1417,  1175,
   -1417, -1417,   215, -1417,  1004, -1417, -1417,   692, -1417,  1175,
   -1417,  1178,   542, -1417,  1005,    81,   188,    81,   188,  1741,
     494,  4062,   116,   154, -1417, -1417,   115, -1417,    81,  4115,
      81,  1097,  4062,   188, -1417,  4062,   958, -1417,   774,   404,
   -1417, -1417, -1417, -1417, -1417,  1008, -1417,  1010, -1417, -1417,
   -1417, -1417, -1417, -1417, -1417, -1417,  1202, -1417,  1013, -1417,
    1222,    81,   759,  1015,  1203, -1417, -1417, -1417,  1207, -1417,
   -1417, -1417,   188,  1019,   526,    81,  1020,    81,  4062,  4062,
   -1417,  4062,  1068, -1417,   188,    81, -1417, -1417,    81,   188,
    1046,   418,  1024,  4145,  1027,  1036,   927, -1417, -1417, -1417,
   -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417,
   -1417, -1417, -1417, -1417,   787, -1417, -1417,  1084,  4062,   442,
   -1417,   649, -1417, -1417, -1417, -1417,  1071,  1224,    81,  1113,
     603, -1417, -1417, -1417, -1417,  1227, -1417,  1031,  4062,  4062,
    4062,  4062,  4062,  1226,  4062,   494,  4062,   927,   927, -1417,
     166, -1417,  4062,  1228,   927,   927,   927,   927,  4062,   927,
     494,   927,   927,   927, -1417,   169, -1417, -1417, -1417, -1417,
   -1417, -1417, -1417, -1417, -1417, -1417,  3006,   188, -1417, -1417,
   -1417, -1417,  3682,   188, -1417,  1229,   774, -1417,  4062, -1417,
     938, -1417, -1417, -1417,  1204,  4564,  2184,  4062, -1417, -1417,
   -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417,  3400,  4115,
     943,   943,    81, -1417, -1417,  4062,   783, -1417,  1827,   188,
      81, -1417,   188,   188,    93,  1230, -1417, -1417,   171, -1417,
   -1417, -1417, -1417, -1417,  1041,  1234, -1417,    81,  1045,  1211,
    1214,  1049, -1417,   175,   176,  1050,  4007, -1417, -1417, -1417,
   -1417, -1417,  1051,   189,  4062,   838, -1417,   927,  4062,  1055,
    1248, -1417, -1417,  1249, -1417, -1417, -1417, -1417,   845,   670,
   -1417, -1417,   195, -1417,   132, -1417,  1250, -1417,    81, -1417,
    1612,   534, -1417, -1417,  3502,   584, -1417, -1417, -1417, -1417,
    1152,    81,    81,  4062,  1255, -1417, -1417,  3872,  1581, -1417,
    1873,  4062, -1417,  4115, -1417,   185, -1417, -1417,   188,  1075,
      81, -1417, -1417, -1417,  1073, -1417,   269, -1417, -1417,  1078,
     187, -1417,   188,    81, -1417, -1417,   880,   188, -1417,  1252,
   -1417, -1417, -1417,  1077,   188,   188,   244,    81,  1258,   198,
   -1417, -1417, -1417, -1417, -1417, -1417,  1271, -1417,  1279, -1417,
     927,    81,    81,    94,   188,    81,  4062,  3336,  3039,  3645,
   -1417,   774,  4062,  4649, -1417,   787,  1090,   188,    81,   445,
   -1417, -1417, -1417, -1417,    96, -1417, -1417,  1094,    81, -1417,
     188,   350,  1098,  4062, -1417, -1417, -1417, -1417, -1417, -1417,
   -1417, -1417,  4062, -1417, -1417, -1417, -1417, -1417,  3542, -1417,
   -1417,   927,  1101, -1417, -1417, -1417, -1417, -1417, -1417, -1417,
   -1417, -1417, -1417, -1417,  3592, -1417, -1417,    81, -1417,    81,
     419,  1102, -1417,  1103, -1417, -1417,  1100, -1417,  1365,   258,
    1285,  4062,   494,   927,   927, -1417,   201, -1417, -1417, -1417,
     188,  1296,  4115, -1417,   188,  1297, -1417, -1417,   191,  1107,
     471,   506, -1417, -1417,   804,  4062, -1417,   203, -1417,  1299,
      81,   188,    81,    81,  4062,  4062, -1417, -1417,   223,  1278,
   -1417,  1094, -1417,  1094, -1417,  1212, -1417,  1232, -1417, -1417,
     132,  1112,  1304, -1417, -1417, -1417, -1417, -1417, -1417,   188,
     205, -1417,  1116, -1417,  4062,   774,   193,  2068, -1417,   188,
     664,   999,   188,  4062,    84,   521, -1417, -1417, -1417, -1417,
   -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417,  1307,   207,
   -1417, -1417,  1115, -1417, -1417, -1417, -1417,   188, -1417,  4062,
     494, -1417, -1417,  4062,  1312, -1417,   817, -1417,  1317, -1417,
    4115,    81,    81,  1223, -1417,   958, -1417,  2169,  1252,   774,
      81,    81,  2068,   634, -1417,    81,  2068,   319,   124,  2068,
    1132,    81,    81, -1417,  1136,  1019, -1417, -1417,  4062,   188,
      81,   188,    81,  1135,  4062, -1417, -1417, -1417, -1417, -1417,
     558,   571,   776,   828,   876,   537,   610,  1138,  4062,  1134,
   -1417,   927,  1141,   476,  1140,   948,  1676,   210,  1142, -1417,
    1236,    81,   188,    81,  1332,  1194,  1340, -1417,   188, -1417,
   -1417,    81,   927,  1341,  1342, -1417, -1417, -1417,   211, -1417,
    4062,  1345, -1417, -1417,   188, -1417,  4115, -1417,   188,   927,
    1346, -1417,  1347, -1417, -1417, -1417, -1417, -1417,  4062,   494,
    4062, -1417, -1417, -1417, -1417,  2184,   188,    81,   188,    81,
     943,   188,    81,   545,   188,    81,   188,    81,   804, -1417,
    1827, -1417,  4062,    81,   460, -1417, -1417,   188, -1417,  1165,
   -1417, -1417, -1417, -1417,  1357,  1360, -1417,  4062,    81,   927,
   -1417, -1417,  1349, -1417,    81,  1344, -1417,  1172,  1369, -1417,
    1370, -1417,  1373, -1417,  1376, -1417, -1417,  4062,  1352,  1377,
   -1417, -1417,  1379,    81, -1417, -1417,    81,  1378, -1417, -1417,
    4086,  4086, -1417,    81, -1417, -1417, -1417,  4062,  4115, -1417,
     188,  2169, -1417, -1417,  1189,  1383,  1384,  1376,   228, -1417,
    1192, -1417, -1417, -1417,  1195,  1196, -1417,  4062,   640, -1417,
   -1417,  1198, -1417, -1417, -1417,    81,    81,   735, -1417, -1417,
   -1417, -1417, -1417, -1417,   984, -1417,   261, -1417, -1417,  1200,
   -1417, -1417,  2694,  4062,  4062,  4062,  4062,  4062,  4062,  4062,
    4062,  4062,  4062,  4062,  4062,  4062,  4062,  2635,  4062,  2760,
    2919, -1417, -1417,  4649,   589,    81,  1197,  1208,  1209,    81,
     188, -1417, -1417,   927,    19,   188,  4062, -1417, -1417, -1417,
      81,  1296,    81, -1417,   927,    25,   188,   188,   188,  1201,
    1392, -1417, -1417,    81,    81, -1417,    81,   188,    81,    81,
      81, -1417, -1417,    81,  1213,   188, -1417,   188,  4062,  1612,
    1400, -1417,  4062,  1215, -1417,  4062,  3922,  2318,  1402,  1404,
    1389, -1417, -1417,  4062,   845,  4062, -1417, -1417, -1417,  1399,
   -1417,  1216,    81,  1217, -1417,  4062,  4062,  4062,   640, -1417,
   -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417,  2068,
      28, -1417, -1417, -1417,  4062, -1417, -1417, -1417, -1417, -1417,
   -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417,
   -1417,  4062,  4062, -1417, -1417, -1417,  4062, -1417,  4062, -1417,
     188,    81,  1194, -1417, -1417,  1410, -1417, -1417, -1417, -1417,
   -1417,    81, -1417, -1417, -1417,    81, -1417,   188, -1417, -1417,
      81,    81,    81,  4649,   494,    81,  1219,    81,   188,    81,
    1220,  1221,  4062, -1417,  1395, -1417, -1417, -1417, -1417,  1414,
   -1417, -1417, -1417, -1417, -1417,  1185,   216,  4062, -1417, -1417,
   -1417, -1417, -1417,  1233,  1134,  1235,  2355,  1237,  1238,  1239,
   -1417, -1417, -1417, -1417, -1417,    81,   188,  1197,    81,   188,
   -1417,    81, -1417, -1417,  1416,   774, -1417,  4062, -1417,  1417,
   -1417, -1417,  2410,  1418, -1417, -1417,  1419, -1417,   927, -1417,
      81, -1417,    81,  4062, -1417,  1240,  4062,  4062,  1420,  2355,
    4062, -1417, -1417, -1417,  1437, -1417,  4062, -1417,  1438,  4062,
   -1417,  4062, -1417, -1417
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,     0,     1,     6,     8,     0,     0,    17,     9,  1032,
    1031,     0,    18,     3,     4,     5,    12,    15,    20,  1030,
       0,    21,   106,    19,   106,     0,   202,  1028,    22,   106,
      23,   106,    24,    18,   973,   941,   208,   206,   216,   210,
     214,   212,    88,   306,     0,     0,     7,    11,    18,   202,
     203,   970,   108,     0,   107,   956,   192,   192,     0,     0,
    1031,  1029,   192,   192,    16,     0,     0,   218,   218,   218,
     218,   242,   218,     0,   204,   205,    10,    13,    14,   457,
       0,     0,   368,   369,    25,     0,   466,     0,   503,   194,
      25,   264,   255,   257,     0,   256,    88,   195,   541,   105,
     109,   110,   116,     0,   193,     0,   112,   260,   117,   202,
     404,   143,   145,   146,     0,   113,   151,     0,     0,   115,
     150,   147,   144,   525,     0,   523,   534,   539,   522,   520,
     521,   118,   119,   120,   711,   709,   709,   712,   738,   739,
     121,   709,   122,   124,   114,   148,   149,   123,   958,   957,
       0,   193,   937,   940,   202,     0,     0,   103,     0,     0,
       0,     0,     0,   921,     0,     0,     0,     0,     0,    88,
     134,   126,   192,   152,     0,   157,   163,   158,   173,   179,
     156,   689,   153,   162,   155,   170,   154,   788,   165,   164,
     181,   161,   178,   172,   160,   175,   180,   174,   177,   166,
     171,   159,  1007,   176,  1049,  1054,  1037,     0,   134,   134,
     974,   942,     0,     0,   209,   219,   207,   217,   211,     0,
       0,   215,   243,   244,   213,   201,   650,   623,   624,   200,
    1017,     0,   258,   259,  1018,   231,   225,     0,   324,   541,
       0,   606,   310,   618,   186,   187,   189,   190,   188,     0,
     308,   607,     0,   605,   610,   611,   613,   615,   625,     0,
     628,   642,   644,   646,   648,   655,     0,   658,   661,   199,
     608,     0,     0,   936,   496,     0,   494,    26,   725,     0,
       0,     0,   999,     0,   997,   467,     0,     0,   506,   717,
       0,     0,     0,     0,     0,   510,     0,   417,   422,   525,
     421,     0,   542,   111,     0,     0,     0,     0,    88,     0,
     659,   202,   337,   402,     0,   466,   466,   202,     0,     0,
       0,     0,   659,   538,   733,   192,   196,   196,   769,   963,
    1065,   476,   949,   202,   952,   954,   955,     0,     0,    88,
     541,   167,   104,     0,     0,   812,     0,  1068,  1067,   169,
     569,   826,     0,     0,   824,     0,     0,     0,   594,     0,
     817,   657,   665,   667,   819,   664,   820,   666,     0,     0,
       0,   975,   135,   127,   192,   130,   132,   133,     0,     0,
       0,     0,     0,  1014,   691,     0,     0,   789,   709,  1011,
       0,  1055,  1047,  1034,   476,   476,   222,     0,     0,     0,
     254,   251,     0,     0,     0,     0,     0,   323,   326,   329,
     328,     0,     0,   541,   618,   235,   187,     0,     0,     0,
       0,     0,   307,     0,   620,     0,   621,   622,     0,   619,
     223,     0,   186,   616,   632,   634,   633,   635,   630,   631,
     627,   636,   637,   639,   641,   638,   640,     0,     0,   651,
       0,   652,     0,   653,   654,     0,   643,  1005,     0,     0,
       0,   493,     0,   707,   732,     0,   727,     0,     0,   995,
    1003,     0,  1001,     0,   508,     0,     0,   507,   719,   267,
     268,   270,     0,   265,     0,   429,     0,   425,   545,   428,
     544,   427,   511,   410,   510,     0,     0,     0,     0,    25,
      25,   549,  1063,     0,   884,   225,   883,   657,   882,     0,
       0,   816,     0,     0,     0,     0,   660,   282,     0,   202,
     278,   280,     0,     0,     0,   340,     0,   408,   405,   406,
     409,     0,   468,   478,     0,     0,   480,    88,   605,     0,
     524,   684,   685,   686,     0,     0,   592,     0,     0,   675,
     677,     0,     0,     0,     0,   710,   198,    25,     0,     0,
     192,   709,   714,   734,   740,     0,   760,   192,   715,     0,
     773,   770,   709,     0,   964,     0,     0,     0,   938,   953,
     700,     0,     0,   767,   813,   814,     0,     0,     0,     0,
       0,     0,     0,   658,   912,     0,   910,   908,     0,     0,
       0,     0,   903,     0,   901,   899,     0,  1075,     0,   818,
       0,   202,   968,     0,   131,     0,   845,   822,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    88,   529,   825,
     870,   821,   823,     0,   873,   867,   872,     0,     0,     0,
       0,   699,   697,   698,   693,   690,   696,   804,   802,     0,
     798,   790,   787,   791,  1009,     0,  1008,  1057,     0,  1057,
       0,  1033,     0,  1046,     0,   220,     0,     0,     0,     0,
     249,     0,   328,   321,     0,   228,   227,   232,   226,     0,
     187,   609,   311,   309,   325,   322,   186,   612,   614,   617,
     626,   629,   645,   647,   649,  1004,     0,     0,     0,   460,
     526,     0,   500,   502,   534,   501,   495,     0,   731,     0,
     996,   998,     0,  1000,     0,     0,   517,   512,   515,     0,
     262,     0,     0,     0,     0,   414,   418,   541,   434,   223,
     435,   229,   439,   437,     0,   438,   436,     0,   419,   439,
     448,   305,     0,   367,     0,   724,     0,   716,     0,   553,
       0,     0,   541,     0,   550,   558,   567,   568,  1064,     0,
     865,     0,     0,     0,   536,   659,     0,   283,     0,     0,
     261,   279,   353,   344,   345,     0,   348,     0,   351,   352,
     354,   355,   356,   341,   343,   361,   335,   357,   370,   338,
       0,   403,     0,     0,   451,   360,   472,   464,   469,   470,
     473,   474,     0,     0,   202,   477,     0,   672,   679,     0,
     674,     0,     0,   681,     0,   668,   535,   540,   721,     0,
       0,     0,     0,     0,     0,     0,   193,   742,   746,   743,
     757,   741,   751,   748,   735,   753,   745,   755,   758,   754,
     756,   747,   752,   744,   761,   709,   759,     0,     0,     0,
     771,     0,   774,   709,   772,   982,     0,   983,  1066,   945,
       0,   794,   583,   545,   584,   572,   580,   585,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   831,
       0,   829,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   924,     0,   922,   913,   916,   533,
     914,   532,   531,   844,   530,   915,     0,     0,   904,   907,
     906,   905,     0,     0,   597,   599,     0,   168,     0,   136,
     202,   139,   141,   142,   978,   192,     0,     0,   822,   871,
     875,   869,   874,   876,   877,   878,   880,   879,     0,   861,
     855,     0,   859,  1016,  1015,     0,     0,   689,     0,     0,
     796,   800,     0,     0,   541,     0,  1024,  1023,     0,  1019,
    1021,  1062,  1038,  1061,     0,  1058,  1059,  1048,     0,  1044,
    1052,     0,   253,     0,     0,     0,     0,   327,   191,   239,
     237,   238,     0,     0,     0,     0,   458,     0,   659,     0,
       0,  1002,   526,   488,   490,   492,   509,   518,     0,   504,
     269,   273,     0,   271,   541,   426,     0,   430,   411,   415,
     542,     0,   432,   433,     0,     0,   416,   431,   224,   230,
       0,   726,   718,     0,   554,   561,   557,     0,     0,   543,
     562,     0,   552,     0,   891,     0,   889,   892,     0,     0,
     669,   537,   288,   289,     0,   292,     0,   285,   287,   296,
       0,   293,     0,   274,   346,   349,     0,     0,   371,   240,
     342,   407,   453,     0,     0,     0,     0,   479,   485,     0,
     483,   481,   682,   683,   680,   593,     0,   676,     0,   678,
       0,   670,   723,    25,     0,   736,     0,  1073,  1071,     0,
     749,     0,     0,   192,   763,   762,     0,     0,   782,     0,
     775,   768,   776,   965,     0,   959,   946,   947,   695,   687,
       0,     0,     0,     0,   582,   843,   656,   836,   833,   834,
     837,   841,     0,   832,   835,   840,   839,   838,     0,   827,
     926,     0,     0,   927,   928,   935,   925,   528,   934,   527,
     929,   932,   931,   930,     0,   917,   911,   909,   902,   900,
       0,     0,  1076,     0,   140,   979,   980,   786,     0,   193,
       0,     0,     0,     0,     0,   849,     0,   847,   881,   868,
       0,   863,     0,   887,     0,   857,   885,   888,     0,     0,
       0,     0,   689,   688,   692,     0,   811,     0,   805,   807,
     797,     0,   799,  1010,     0,     0,  1012,  1056,     0,  1039,
    1045,   947,  1053,   947,   221,     0,   250,     0,   247,   246,
     541,     0,     0,   333,   233,  1006,   663,   462,   461,     0,
       0,   498,     0,   730,     0,     0,   510,   392,   516,     0,
       0,     0,     0,     0,     0,   191,   441,   183,   184,   185,
     443,   444,   446,   447,   445,   440,   442,   312,     0,     0,
     314,   316,   681,   318,   319,   320,   420,     0,   555,     0,
       0,   559,   551,     0,   563,   566,   891,   896,     0,   894,
       0,   866,   779,     0,   290,     0,   284,     0,   240,     0,
     281,   275,   392,     0,   358,   336,   392,     0,   362,   392,
       0,   452,   465,   471,     0,     0,   482,   679,     0,     0,
     720,     0,   737,     0,     0,    32,    33,    91,    71,    94,
     258,   259,   255,   257,   256,   231,   225,     0,     0,    27,
      63,    65,    62,   541,    28,   101,   658,     0,     0,   764,
       0,   783,     0,   784,     0,     0,   984,   985,     0,   948,
     943,   795,     0,     0,   573,   574,   581,   570,     0,   587,
       0,     0,   842,   830,     0,   933,     0,   923,     0,     0,
       0,   598,   600,   601,   595,   792,   981,   976,     0,     0,
       0,   850,   853,   852,   851,     0,     0,   862,     0,   856,
       0,     0,   860,     0,     0,   703,     0,   705,   694,   809,
       0,   803,   808,   801,   541,  1022,  1020,     0,  1060,     0,
    1035,  1040,  1051,  1051,     0,     0,   330,     0,   459,     0,
     497,   535,   728,   491,   487,     0,   386,     0,   373,   378,
       0,   381,   374,   384,   375,   388,   376,   394,     0,   377,
     396,   662,   383,   505,   513,   272,   263,     0,   236,   234,
       0,     0,   313,   777,   556,   560,   564,     0,     0,   890,
       0,     0,   286,   390,     0,   298,     0,   299,   300,   294,
       0,   400,   401,   399,     0,     0,   241,     0,     0,   359,
     363,     0,   455,   486,   484,   671,   722,     0,    31,  1070,
    1072,    30,  1074,    66,   534,    67,    72,  1069,    95,    98,
      96,   102,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    55,     0,     0,
       0,    29,   750,   192,     0,   785,   966,     0,     0,   960,
       0,   579,   576,     0,     0,     0,     0,   586,   589,   591,
     828,   919,   918,   603,     0,     0,     0,     0,     0,     0,
       0,   854,   848,   846,   864,   886,   858,     0,   701,   704,
     706,   806,   810,  1013,     0,     0,  1042,     0,     0,     0,
       0,   499,     0,     0,   519,   393,   387,     0,     0,     0,
       0,   382,   398,   394,     0,     0,   317,   315,   565,     0,
     895,     0,   778,     0,   297,     0,     0,     0,     0,   295,
     301,   347,   350,   372,   364,   366,   365,   305,   454,   392,
       0,    64,    64,    64,     0,    54,    60,    39,    49,    51,
      50,    52,    45,    40,    47,    46,    38,    48,    34,    35,
      36,     0,     0,    53,    56,    37,     0,    42,     0,    41,
       0,   780,   993,   961,   992,   967,   988,   991,   990,   987,
     986,   944,   578,   577,   575,   571,   588,     0,   604,   602,
     596,   793,   977,   192,     0,   702,     0,  1036,     0,  1050,
       0,     0,     0,   729,     0,   379,   380,   383,   386,     0,
     385,   389,   395,   391,   397,   514,     0,     0,   893,   291,
     302,   304,   303,     0,    74,    61,    75,     0,     0,     0,
      59,    57,    58,    44,    43,   781,     0,     0,   920,     0,
    1041,  1043,   245,   248,   331,     0,   387,     0,   423,     0,
     456,    72,    87,    76,    77,    80,    79,    68,     0,    73,
     962,   989,   815,     0,   489,     0,     0,     0,    85,     0,
      86,    70,   332,   424,   897,    84,     0,    78,    81,     0,
      83,     0,   898,    82
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1417, -1417, -1417,  1095, -1417,  1394,   642, -1417, -1417, -1417,
   -1417, -1417, -1417, -1417, -1417, -1417,  -178, -1417, -1417, -1417,
   -1417, -1417, -1417, -1417,  -769, -1417,  -285, -1417,   -11, -1417,
   -1417, -1417, -1417, -1417, -1417, -1417, -1417,  1348,   798, -1417,
   -1417, -1417,   152,   655, -1417, -1417, -1417,   527, -1417,   -85,
    -906,  -632, -1417, -1417,   435,   438,   -42,    57, -1417,   544,
    -215,   -74, -1417,  1431, -1417, -1417, -1417, -1417, -1417, -1417,
     866, -1417,  -203,  -186,  1042,  -432,  -180, -1417, -1417, -1417,
     177, -1417, -1417, -1417,   172,   -41, -1417, -1417, -1417, -1417,
   -1417, -1417, -1417,   739, -1417,   225, -1417, -1417, -1417,   941,
   -1417, -1417, -1417,   178, -1417, -1417,   182, -1417,    11, -1417,
   -1417,  -980,  1453, -1417,  1048,   456, -1417,    29,    34, -1417,
    1231, -1417, -1417,  1076,  -626, -1417, -1417, -1417, -1417, -1417,
   -1417, -1417, -1417, -1417,   687, -1417, -1417, -1417,   422, -1417,
   -1417, -1417, -1417,  -967,  -266, -1417, -1417, -1187, -1166, -1186,
   -1193, -1135, -1417,   -92, -1160,   -89, -1417, -1417,    53, -1417,
     -86, -1417, -1417, -1417, -1417, -1417,   697, -1417, -1417, -1417,
   -1417,  -416, -1417, -1417,  1003,  -249, -1417,   762, -1417,   479,
    -576, -1417,   484, -1417, -1417, -1417, -1417, -1417, -1417, -1417,
   -1417, -1417, -1417, -1417,   516, -1417, -1417, -1417,   -20, -1417,
   -1417,   434, -1417,     9, -1417, -1417, -1417,   702, -1417,   212,
   -1417, -1417,  -197,   284, -1417, -1417,  1053, -1417, -1417,  -931,
   -1417, -1417, -1417, -1417,  -281,  -467, -1417, -1417,   -63,   517,
   -1417,  1225, -1417,  2143,  -451,   621, -1417, -1417,  -815, -1417,
    -531, -1417,  -456,  -286,  -293, -1417,   964, -1417, -1417,  -252,
    -282, -1417, -1417,   491, -1417, -1417,   960, -1417, -1417, -1417,
   -1417,    -3,    -9,   163, -1417,   413,  -577, -1417, -1417,     0,
   -1417,  -270,   179,   967, -1417, -1417, -1417, -1417, -1417,    -7,
   -1417, -1417,   339,   -30,  1085, -1417, -1417,   -78,  1087, -1417,
    1265, -1417,  1086,  1081,  1089, -1417, -1417, -1417, -1417, -1417,
    1819,  -794,   -83,  -166,   772,   -69,  -995, -1108, -1417, -1417,
    -204, -1417,   -46,   100, -1417, -1417, -1417,   729,   731,  -514,
     733, -1417,  1241,  -367,  -364,  -861, -1417, -1417, -1417, -1417,
    -832,  -838, -1417, -1417, -1417, -1417,   -98, -1417,   295, -1417,
   -1417,   980, -1417,   -77,  -696,  -104,  1242, -1417, -1417, -1417,
   -1417, -1417, -1417, -1417,   983, -1417, -1417, -1417,   326, -1417,
    -496, -1417, -1417, -1417, -1417, -1417, -1417,   986, -1417, -1417,
    1154, -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1417,
   -1417,   164, -1089, -1417,   989, -1417,   -22, -1417, -1417,   937,
    -126, -1417,   993, -1417, -1417, -1417,   427,   675,  -534,  1000,
   -1417, -1417,   183,  1007, -1417, -1417,  1011, -1417, -1417,   -12,
    1180,   942,   626,  -239,   624,   192,  -889,  -965,  -860, -1417,
     120, -1417,  1014, -1417,   661,  1016, -1417,   672,  1023, -1417,
   -1417, -1417, -1417,   440,   397, -1417, -1417, -1417, -1417, -1417,
   -1417, -1417, -1417,  -404, -1417, -1417, -1417,  1253, -1417, -1417,
    1533, -1417, -1417, -1417, -1417, -1417,   720, -1417, -1417, -1417,
   -1417, -1417, -1417, -1417, -1417, -1417, -1417, -1013, -1417,  -107,
   -1417, -1416, -1417,  1310,  1128, -1417, -1417,   895,  -482, -1417,
    1037, -1417, -1417, -1417, -1417, -1417, -1417,   971,   907,   423,
     420, -1417, -1417,  1587,  -136, -1417, -1417, -1417, -1417, -1417,
   -1417, -1417, -1417, -1417, -1417,  -130, -1417, -1417, -1417, -1417,
     214, -1417, -1417, -1417,   961, -1417,   421,   505, -1417, -1417,
   -1417, -1417, -1417,   531
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,    13,    14,    15,    16,    46,    17,    18,    33,
     279,  1317,  1318,  1511,  1623,  1605,  1319,  1686,  1320,  1601,
    1602,  1321,  1603,  1322,  1687,  1713,  1714,  1715,   340,  1324,
    1325,  1490,   341,    51,    52,    99,   100,   101,   170,   171,
     373,   374,   375,   371,   372,   919,   920,   921,   102,   172,
     173,   240,  1236,  1237,   241,   979,   174,   104,   559,  1093,
     242,    19,    20,    44,    68,    67,    70,    72,    71,    69,
     214,   215,   243,   244,   677,   415,   245,   246,   417,   982,
    1288,   221,   222,   223,   401,   247,   248,   106,   311,   107,
     292,   293,   479,   480,  1002,  1003,   770,   518,   519,   520,
     521,   768,  1046,  1047,  1451,  1050,  1051,  1278,  1454,  1589,
    1590,   733,   734,   249,   250,   735,  1249,  1250,  1251,   251,
     406,   252,   685,   407,   408,   409,  1211,  1212,   108,   109,
    1057,   523,   524,   525,   783,  1282,  1283,   786,   787,   796,
     788,  1469,  1470,   736,   110,  1059,  1286,  1417,  1418,  1419,
    1420,  1421,  1422,  1423,  1424,  1425,  1426,  1427,  1428,  1429,
    1430,  1464,   111,   526,   313,   528,   529,   112,   723,   493,
     494,   295,   296,   737,   297,   298,   486,   487,  1006,   738,
    1012,  1245,   739,   740,   113,   114,  1064,   794,  1289,  1599,
     115,   272,  1219,   698,   699,   116,   117,  1065,   286,   797,
     798,   799,   800,    53,   119,   802,   535,   536,  1069,  1070,
     120,  1226,   993,   994,   121,   275,   276,   459,  1220,   701,
     122,   288,  1229,   476,   801,   495,   999,  1574,   717,   718,
    1227,   253,   539,   124,   862,  1138,  1139,   629,   903,   904,
    1642,   901,   125,   514,   126,   323,   127,   501,   489,   128,
     129,   130,   753,   754,  1032,   755,   175,   586,  1525,  1112,
    1344,  1345,  1643,  1522,   865,   866,   867,  1114,  1348,  1349,
    1350,  1351,  1074,   176,   606,  1536,   915,  1151,  1362,  1363,
     254,   255,   256,   257,   258,   425,   428,   259,   260,   447,
     261,   448,   262,   263,   264,   265,   266,   450,   452,   455,
     267,  1115,  1116,   268,   515,   354,  1432,  1217,   364,   365,
     366,   367,   177,   178,   320,   547,   548,   549,   550,  1254,
     542,   543,  1255,   179,   180,   384,   644,   946,   181,   645,
     646,   581,   947,  1182,  1183,   708,   324,   325,   182,   134,
     135,   561,   136,   280,   465,   326,   562,   563,   137,   138,
     564,   831,   139,   565,   566,  1094,   343,   183,   184,   570,
     571,   851,   852,   141,   572,   853,  1101,   185,   186,   386,
     387,   187,  1537,  1110,   388,   652,   952,  1191,   649,   948,
    1187,  1188,  1189,   188,   189,   190,   191,   192,   368,   630,
     631,   632,   193,   587,  1354,   880,   881,  1117,   905,   194,
     926,  1166,  1167,   195,  1174,  1381,   196,  1170,  1378,   197,
     633,   634,   635,   636,  1175,  1176,  1035,  1036,  1037,  1268,
    1269,  1581,   198,   603,   604,   199,   595,   596,   200,  1358,
    1647,   352,   895,   896,   377,    21,   331,   152,    22,    66,
     578,  1520,  1107,  1340,   153,   332,   333,   334,    54,   329,
      55,  1338,  1696,   575,  1633,    23,    56,    24,    65,   612,
     613,  1538,  1156,  1367,   856,  1105,  1336,  1634,  1635,  1636,
    1637,   530,   145,   283,   284,   146,   471,   472,   270,   696,
     201,   390,   953,   655,  1397,   202,   639,   271,   958,   959,
     960,    25,    26,    27,    28,    29,   659,  1555,   207,   963,
    1400,  1401,   661,  1658,  1201,    30,    31,   658,   205,   663,
    1556,  1203,   392,   657,   964,   965,   966,   203,   154,   576,
     349,  1090,  1600,   608
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      43,   488,   363,   703,   269,   404,   131,   477,   702,   398,
     103,   105,   491,   290,   150,   743,   105,   642,   335,  1157,
     643,   105,   105,   955,   336,   481,  1007,   724,  1109,   914,
     142,  1242,   327,   400,   414,   312,   553,   814,   355,   357,
     143,    98,   490,   328,  1244,   485,    98,   363,   977,   545,
    1171,    98,    98,   105,   131,   429,  1221,   416,   103,   105,
     900,   118,   239,   105,   955,   730,   509,   911,  1267,   910,
    1133,  1134,   430,   432,   850,  1118,  1119,  1120,   142,   725,
    1177,  1177,  1125,    98,  1456,   362,  1184,   376,   143,    98,
     314,  1337,   937,    98,   936,  1460,  1389,   277,  -182,  1465,
    1638,   212,  1471,   227,   228,    47,   460,  1179,   421,   118,
    -525,  1455,   669,   151,  -195,   291,  1342,  1457,     4,  1030,
     468,  1027,  1359,  1334,   492,   712,   809,     4,   492,  1467,
     811,   105,   906,   912,   759,  -195,   385,  -732,  1308,   916,
     938,     4,   647,   638,   938,  -412,     4,   985,   347,  -413,
     414,   269,   132,   414,   348,     4,  1013,  1028,     8,   418,
     557,    98,   804,  1017,    32,  1343,   704,     8,   219,  1128,
     941,  1360,  1144,   416,  1195,  -195,   416,   705,  1205,  1207,
     431,     8,   558,     4,   666,  -412,     8,  1329,  1270,  -413,
    1279,     4,  1195,   830,  1380,     8,   492,   335,  1231,   648,
     132,  1295,   414,   336,  1375,   682,  1390,  1267,  1409,     4,
    1441,     4,     4,   916,  1526,     4,   346,     4,    34,  1707,
      42,   650,   508,     8,   522,   416,   413,   594,    35,   433,
     602,     8,   432,  1587,  1569,   503,  1011,   517,   680,   686,
     376,   516,   686,   534,   961,   432,     4,   981,  1303,     8,
     546,     8,     8,   516,    49,     8,   784,     8,  1456,   227,
     228,  1369,   474,   995,  -542,   573,   475,  -266,   464,   766,
    1031,  1468,  1275,   235,   213,   418,    42,   236,   418,    48,
    1213,  1638,  -542,    98,   105,  1455,     8,   302,  1335,   376,
     653,  1457,    12,   863,   728,   531,   532,   508,   508,   508,
     422,    12,  -525,  1552,   670,  1449,  -195,  -525,  1009,  -195,
     302,   729,   412,   863,    98,    12,  1355,   731,   810,  -412,
      12,  1388,   812,  -413,   907,   913,   302,   418,  1352,    12,
     269,   917,   939,   105,   864,   757,   940,  -766,   641,   986,
     577,   220,   413,   105,  1127,   413,  1126,   133,   607,  1029,
     977,   269,   269,  1132,   864,  1100,  1135,    12,  1142,  1594,
    1141,  1129,   506,    98,  1145,    12,  1196,    58,   533,    59,
    1206,  1208,  1456,    98,  1669,  1588,  -339,  -766,   140,  1665,
    1569,   930,  1248,    12,  1215,    12,    12,     4,   433,    12,
    1232,    12,  1456,  1296,   413,   133,  1376,    11,  1391,  1665,
    1410,   689,  1442,   660,   662,  1512,  1527,   556,   557,   310,
     239,  1708,  1683,  -476,   344,   962,  -542,   574,   105,   742,
      12,   767,   746,   748,  1276,    73,   140,     8,     4,   488,
     558,  1666,  1571,   236,   498,   499,    42,   506,   506,   506,
     491,   640,     4,    92,    93,   517,    95,  1342,   700,   144,
      42,  1666,   973,   481,   105,   105,   732,   844,    74,   610,
     971,   879,   894,   897,   974,   975,     4,  1531,     8,     4,
     490,   281,   908,   485,   854,   922,   980,   555,  1561,  1680,
     819,   923,     8,  1267,    98,    98,   727,   429,  1595,    75,
     752,   227,   228,   430,   500,     4,  1343,   144,   363,   731,
    1048,  1596,   363,   363,   430,  1640,     8,   230,   282,     8,
     105,  -766,   236,   399,   829,   285,  1359,   318,   825,   105,
    1177,   795,   498,   499,   784,   847,   614,  1521,   496,   743,
       4,   703,  1230,   743,   148,     8,   702,   508,   834,  1076,
      98,  1078,    42,    98,  1521,   834,  -197,   497,   839,    98,
     105,   105,  -466,   227,   228,   949,  1018,   147,   234,   105,
     105,    12,  1019,   955,   319,  1360,  1299,   462,   463,     4,
       8,  1667,  1571,  1361,    42,   700,  1541,   204,  1571,  1241,
      98,    98,   500,   730,   105,   105,    42,    47,   294,    98,
      98,  1667, -1026,   206, -1026,   700,  1052,   230,   269,   105,
      89,   742,    12,   227,   228,   147,   274,  1157,  1681,     8,
    1084,    42,   431,     4,    98,    98,    12,   826,   304,   426,
     427,  1682,   231,   305,   151,    97,   306,     4,   943,    98,
    1374, -1027,  1373, -1027,  1097,   483,   285,  1332,   232,   233,
      12,   287,    42,    12,   954,  1109,   302,   898,   234,   301,
       9,  -236,  -541,     8,   302,   307,   909,    92,    93,   230,
      95,   235,    89,  1384,    42,   236,   237,     8,   978,    12,
    1486,  1487,   484,   492,   238,    64,   -61,   506,    60,   933,
     934,   935,   474,   105,   310,   954,   475,    97,  -510,     9,
      76,    89,  -510,  -236,   704,  1015,   516,   357,  1386,    11,
     232,   233,  1222,   992,    12,   705,  -939,   453,   454,   230,
     234,  1004,   302,    98,  1016,  1010,    97,    60,   315,    92,
      93,  -541,    95,   235,    82,    83,    42,   236,  1234,   -93,
     534,   235,   -93,    11,  -276,   236,    11,  1547,   411,  1075,
     546,  1095,   546,    12,    57,   316,  -137,  1157,  1123,    62,
     -89,    63,  1579,   -89,   569,  1102,  1099,  1049,   317,  1415,
     234,    48,  1136,   -90,   687,   230,   -90,   688,   321,    92,
      93,    89,    95,   235,   995,    11,    42,   236,   675,   676,
     594,  1630,    11,  -277,   922,   105,   602,    12,    92,    93,
     923,    95,  1461,  1462,  1463,    42,    97,  1402,   330,  1403,
    1165,    12,   -92,  1121,   369,  1479,   322,  1025,  1240,  1248,
     337,   412,   728,   370,   641,    98,   234,   642,   863,   105,
     643,   569,   849,  1180,  1181,    92,    93,   338,    95,   729,
     414,   416,    42,  1688,  1689,   731,   105,   105,     5, -1026,
       6, -1026,   155,   105,   105,   105,   105,     7,   105,    98,
     105,   105,   105,   416,   350,   508,   351,   863,  1168,   864,
     208,   209,   757,   394,   395,  1483,    98,    98,   358,  1491,
     560,   567,   359,    98,    98,    98,    98,   385,    98,   396,
      98,    98,    98,  1158,   105,   389,   414,   391,   393,   411,
     434,   435,   436,   437,   438,   439,   440,     9,   864,   105,
     105,   420, -1026,    89, -1026,  1152,   412,  -223,   424,   416,
      36,    37,    38,    39,    98,   449,   451,   640,    40,   516,
      92,    93,   457,    95,    41,    10,    42,    42,    97,    98,
      98,   458,  1124,   742,   216,   217,   218,   742,   224,   282,
   -1026,   269, -1026,   470,   473,   482,   105,  1140,   512,  1434,
     513,   527,   551,   703,   554,   418,   556,   569,   702,   588,
       9,   -97,   -97,   602,   -97,  1210,    42, -1026,   -97, -1026,
     654,   -97,    89,   616,   732,   664,   700,    36,    37,    38,
      39,   665,  1159,    11,   667,    40,   743,   668,    60,    92,
      93,    41,    95,   673,   674,   506,    42,    97,     9,   239,
    1235,  -234,   879,   239,   727,   684,  1301,  -229,    77,  1048,
     679,   418,   681,  -100,  -100,  -224,  -100,   752,   894,   695,
    -100,   642,   413,  -100,   643,     9,    60,   707,  1484,  1437,
     697,  1439,  1484,   709,   714,  1371,   715,   716,   719,   105,
     441,   442,   443,   444,   546,   721,    11,   722,   430,   761,
     762,  1158,   105,    60,   731,   795,  -950,    82,    83,  1042,
    1043,   -99,   -99,   764,   -99,   765,   769,   363,   -99,    98,
     789,   -99,   785,   790,   445,   446,   225,   792,  1323,   793,
     607,   806,    98,   226,    11,   803,   808,   820,   816,  1044,
     105,   227,   228,   229,  -951,    92,    93,  1045,    95,   821,
     700,   400,    42,   848,   294,   857,   743,    92,    93,   855,
      95,    11,    92,    93,   308,    95,   704,   859,   105,    42,
      98,  -138,   105,   105,   860,   861,   230,   705,    92,    93,
     918,    95,   924,  1489,    93,    42,    95,  1172,   641,   700,
      42,   925,   927,   105,   305,   402,   931,  1092,    98,   945,
     826,   231,    98,    98,   951,   969,   742,   379,  1431,   380,
     210,   211,   381,   382,   970,   976,   403,   232,   233,  -230,
     972,   440,   984,    98,   987,   273,   307,   234,   988,   278,
     996,   383,   997,  1394,   954,   289,    92,    93,   998,    95,
     235,  1001,  1010,    42,   236,   237,  1011,   230,  1014,  -449,
    1020,  1038,  1054,   238,  1055,  1056,  1063,  1058,  1431,  1062,
    1066,  1068,  1072,  1431,   992,  1080,  1083,  1431,  1086,  1372,
    1431,  1091,   231,  1438,   307,  1113,  1103,  1104,  1106,  1075,
    1111,  1122,  1150,  1131,  1155,  1194,  1197,  1198,   232,   233,
    1199,   640,  1200,  1202,  1204,  1209,  1214,   772,   234,  1165,
    1223,  1224,  1225,   345,   995,  1233,  1257,    92,    93,  1259,
      95,   235,  1731,   360,    42,   236,   237,  1274,  1049,  1290,
    1273,   304,  1277,  1287,   238,  1297,   305,   123,  1294,   378,
     105,  1075,   123,  1298,   105,  1330,  1339,   123,   123,   379,
    1370,   380,  1366,  1347,   381,   382,  1356,  1364,  1365,  1270,
    1380,   105,  1383,  1392,  1399,  1405,  1404,  1406,   307,  1407,
      98,  1411,  1440,   383,    98,  -524,  1447,  1445,   105,   299,
    1448,    82,    83,   773,   774,   123,  1472,  1450,  1473,   299,
    1477,    98,  1488,  1480,  1482,  1485,  1516,  1513,  1560,   105,
    1514,  1517,   641,  1518,  1661,  1524,  1523,   105,    98,  1529,
    1535,  1534,  1562,   775,  1660,   776,   777,   778,  1431,  1554,
     779,   780,  1558,   781,   782,  1559,  1563,  1564,   105,    98,
     461,  1676,  1565,  1572,  1566,   466,  1567,    98,   469,  1568,
    1573,  1575,  1431,  -382,  1584,   478,  1585,  1591,  1586,  1632,
    1592,  1593,   363,  1598,  -255,  1654,  1653,   123,   700,   502,
    1639,  1335,   511,  1662,  1677,  1656,  1453,  1664,  1672,   363,
    1673,  1678,  1679,  1697,  1700,  1702,  1703,  1705,  1706,  1723,
    1726,  1729,  1684,  1730,  1736,  1484,  1540,   105,  1710,  1711,
     239,   641,  1717,  1718,  1719,  1733,   105,  1210,   583,   584,
    1739,   585,  1741,    78,  1737,   640,  1238,  1154,   303,  1239,
     597,    50,   605,  1452,   678,  1458,  1435,   546,  1000,  1466,
     771,  1459,  1583,   609,    45,   611,    98,  1243,   304,   683,
    1577,  1158,   105,   305,  1576,  1670,  1368,  1060,  1284,  1671,
    1570,   671,   105,   423,  1005,   656,   379,  1674,   380,  1061,
     269,   381,   382,   105,  1256,  1246,  1431,  1431,  1431,   726,
    1293,  1218,    98,  1725,  1431,   307,  1071,  1474,  1724,  1413,
     383,  1675,    98,   706,  1143,  1228,  1431,  1431,   817,  1262,
     827,  1644,  1533,    98,  1346,  1648,  1646,   828,  1649,  1528,
    1431,   456,   690,   693,   640,   691,   692,  1041,  1077,  1073,
     651,   363,  1079,   538,   694,   845,   832,   105,   846,   833,
     123,     2,     3,   835,  1551,  1353,   929,  1130,  1542,   544,
     836,   637,   710,   932,  1169,  1178,   713,   837,  1580,   568,
     826,   838,  1545,  1148,   840,     4,   841,   720,  1146,     5,
   -1026,     6, -1026,   842,  1357,   749,   579,   149,     7,  1685,
    1721,   467,   226,  1704,   745,   747,   711,   843,   758,   123,
     227,   228,   750,   983,   760,   538,   538,   991,  1709,   299,
     944,  1158,   105,    61,  1395,     8,   225,  1557,  1396,  1398,
     968,   791,  1327,   226,     0,     0,     0,     0,     0,     0,
     805,   227,   228,   229,     0,     0,     0,     0,     9,   807,
       0,     0,    98,     0,     0,     0,     0,   815,     0,     0,
       0,     0,   818,     0,  1732,     0,     0,  1734,     0,     0,
       0,     0,     0,     0,     0,     0,    10,     0,     0,     0,
    1742,   858,     0,     0,     0,     0,     0,   105,     0,     0,
       0,  1492,  1493,  1494,  1495,  1496,  1497,     0,  1498,  1499,
    1500,  1501,  1502,  1503,   992,  1504,  1505,  1506,  1507,  1508,
       0,     0,     0,     0,     0,     0,   230,    98,     0,     0,
    1159,  1699,     0,     0,     0,     0,     0,     0,     0,     0,
     299,   299,   741,     0,    11,     0,     0,  -202,  -202,  -202,
    -202,   231,     0,   942,     0,  -202,   751,   230,     0,     0,
       0,  -202,     0,     0,   950,  1023,     0,   232,   233,    12,
       0,     0,   226,   967,     0,     0,     0,   234,     0,     0,
     227,   228,   231,     0,     0,     0,    92,    93,     0,    95,
     235,     0,     0,    42,   236,   237,   299,     0,   232,   233,
       0,     0,     0,   238,     0,   299,     0,     0,   234,     0,
       0,     0,     0,     0,     0,     0,     0,    92,    93,     0,
      95,   235,     0,     0,    42,   236,   237,     0,     0,     0,
       0,     0,     0,     0,   238,     0,   299,   299,  1008,     0,
       0,     0,     0,     0,     0,   299,   299,     0,     0,     0,
       0,  1185,     0,     0,     0,     0,     0,     0,   226,     0,
       0,  1021,     0,  1022,   538,     0,   227,   228,     0,     0,
     299,   299,     0,     0,     0,     0,     0,     0,  1040,     0,
       0,     0,     0,     0,  1053,   299,   230,     0,     0,     0,
       0,   681,  1509,  1510,     0,     0,     0,  1263,     0,     0,
     538,     0,     0,     0,   226,     0,     0,     0,   822,     0,
       0,   231,   227,   228,     0,     0,     0,  1067,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   232,   233,  1081,
       0,     0,     0,     0,  1082,     0,  1085,   234,     0,     0,
       0,   538,     0,     0,     0,     0,    92,    93,     0,    95,
     235,     0,     0,    42,   236,   237,     0,     0,     0,     0,
       0,     0,     0,   238,  1098,     0,     0,     0,     0,   299,
       0,     0,   230,     0,     0,  1108,     0,     0,     0,   159,
       0,     0,     0,     0,  -197,     0,     0,     0,     0,   161,
     162,     0,   163,     0,     0,   164,   342,   231,   823,   166,
     824,     0,     0,     0,     0,     0,     0,   361,     0,     0,
       0,     0,     0,   232,   233,     0,     0,     0,   230,     0,
       0,     0,  1147,   234,     0,     0,     0,     0,  1149,     0,
       0,     0,    92,    93,     0,    95,   235,     0,    89,    42,
     236,   237,     0,   231,     0,     0,     0,     0,     0,   238,
       0,     0,     0,     0,     0,    92,    93,     0,    95,   232,
     233,     0,    42,    97,  1190,     0,     0,  1192,  1193,   234,
     410,   299,     0,     0,     0,     0,   419,     0,    92,    93,
       0,    95,   235,     0,     0,    42,   236,   237,     0,     0,
       0,   410,  1416,     0,     0,   238,     0,     0,     0,   226,
       0,     0,     0,     0,     0,   299,     0,   227,   228,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   299,   299,     0,     0,     0,     0,     0,   299,
     299,   299,   299,     0,   299,     0,   299,   299,   299,     0,
       0,     0,     0,     0,   507,   510,     0,     0,     0,     0,
    1271,     0,     0,  1272,     0,     0,     0,   541,     0,     0,
     552,     0,     0,     0,     0,  1280,     0,  1281,     0,     0,
     123,     0,  1285,     0,     0,     0,   580,   582,     0,  1291,
    1292,     0,     0,   538,     0,   299,   299,     0,     0,     0,
       0,     0,   593,  1453,     0,   593,     0,     0,  1300,  1302,
     226,     0,     0,     0,     0,     0,     0,     0,   227,   228,
       0,     0,  1331,   230,  1333,   226,     0,   615,     0,   361,
     361,   507,     0,   227,   228,  1341,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   231,     0,
       0,     0,     0,     0,   672,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   232,   233,   741,   300,     0,   538,
     741,  1160,  1161,     0,   234,     0,     0,   309,     0,  1162,
       0,  1163,  1164,    92,    93,     0,    95,   235,     0,     0,
      42,   236,   237,     0,     0,  1377,     0,     0,     0,  1379,
     238,     0,     0,  1382,     0,  1385,  1387,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1393,     0,     0,     0,
       0,     0,     0,     0,   230,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   299,     0,     0,     0,   230,
       0,     0,     0,     0,  1408,     0,     0,   744,   123,   231,
     756,  1414,  1668,     0,  1433,     0,     0,  1436,     0,   226,
       0,     0,   763,     0,   231,   232,   233,   227,   228,     0,
       0,     0,     0,     0,     0,   234,     0,     0,     0,     0,
     232,   233,  1443,     0,    92,    93,   299,    95,   235,  1712,
     234,    42,   236,   237,     0,     0,   226,     0,     0,    92,
      93,   238,    95,   235,   227,   228,    42,   236,   237,     0,
       0,     0,     0,     0,   299,     0,   238,     0,   299,   299,
       0,     0,     0,     0,  1475,     0,  1476,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   299,
       0,     0,     0,     0,  1727,     0,     0,     0,     0,   538,
     538,   226,     0,     0,     0,     0,     0,  1515,     0,   227,
     228,     0,     0,  1519,     0,     0,     0,     0,     0,   507,
       0,     0,     0,   230,     0,     0,     0,     0,     0,  1530,
       0,     0,     0,  1532,     0,     0,     0,     0,     0,     0,
       0,   540,     0,     0,     0,     0,     0,     0,   231,     0,
       0,  1543,     0,  1544,   957,     0,  1546,     0,  1548,  1549,
     230,  1550,     0,     0,   232,   233,     0,     0,     0,     0,
       0,     0,  1553,   410,   234,     0,     0,     0,     0,     0,
       0,     0,     0,    92,    93,   231,    95,   235,     0,     0,
      42,   236,   237,     0,     0,   957,     0,     0,     0,     0,
     238,   232,   233,   628,   628,     0,   989,     0,   990,     0,
       0,   234,     0,     0,     0,   230,     0,     0,     0,     0,
      92,    93,     0,    95,   235,  1582,     0,    42,   236,   237,
       0,     0,     0,     0,     0,     0,     0,   238,     0,     0,
     231,     0,     0,     0,     0,     0,     0,   299,  1024,     0,
    1026,     0,     0,     0,     0,     0,   232,   233,  1034,     0,
       0,  1039,     0,     0,   299,     0,   234,     0,     0,     0,
       0,     0,     0,     0,     0,    92,    93,     0,    95,   235,
       0,     0,    42,   236,   237,   299,     0,     0,     0,  1631,
       0,     0,   238,   299,     0,  1641,     0,     0,     0,     0,
    1645,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1650,  1651,  1652,     0,     0,     0,     0,   300,   300,
    1621,     0,  1655,     0,     0,     0,   226,     0,     0,     0,
    1657,     0,  1659,     0,   227,   228,     0,  1622,     0,     0,
       0,     0,     0,     0,     0,   538,   538,  1096,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   507,   507,   507,
     507,     0,     0,  1597,   813,   507,     0,     0,     0,  1604,
       0,   507,   299,     0,     0,   226,     0,     0,     0,     0,
       0,     0,     0,   227,   228,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1695,     0,     0,     0,     0,
       0,     0,     0,     0,   899,   902,     0,  1153,   123,     0,
       0,     0,  1698,   899,   902,     0,     0,     0,   299,     0,
       0,     0,     0,  1701,     0,     0,     0,   361,  1034,   299,
     230,     0,   628,     0,   580,  1626,     0,  1186,   899,   902,
       0,   226,     0,     0,     0,     0,     0,     0,     0,   227,
     228,     0,     0,   309,     0,   231,     0,     0,     0,     0,
       0,  1720,     0,     0,  1722,   410,     0,     0,   956,     0,
       0,   232,   233,  1216,     0,     0,     0,     0,     0,     0,
       0,   234,     0,  1597,     0,     0,     0,     0,     0,   230,
      92,    93,     0,    95,   235,     0,     0,    42,   236,   237,
       0,     0,     0,  1253,     0,     0,     0,   238,     0,   956,
       0,     0,  1258,     0,   231,     0,  1261,   756,     0,  1264,
    1265,     0,  1266,     0,     0,     0,     0,     0,     0,     0,
     232,   233,     0,     0,     0,     0,     0,   300,     0,     0,
     234,     0,     0,     0,     0,     0,     0,     0,   123,    92,
      93,     0,    95,   235,     0,   230,    42,   236,   237,     0,
       0,     0,     0,     0,     0,     0,   238,     0,     0,     0,
       0,     0,     0,     0,     0,   580,     0,     0,  1326,     0,
     231,  1328,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1628,     0,   232,   233,     0,     0,
     226,     0,     0,     0,     0,     0,   234,     0,   227,   228,
       0,   507,     0,     0,     0,    92,    93,     0,    95,   235,
       0,     0,    42,   236,   237,     0,     0,     0,     0,     0,
       0,     0,   238,     0,     0,     0,     0,     0,     0,   309,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1034,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1186,     0,     0,     0,     0,     0,
       0,     0,     0,   957,   957,     0,     0,   226,     0,     0,
     899,   902,     0,     0,     0,   227,   228,   899,   902,   902,
     899,     0,  1137,     0,   899,   902,  1137,     0,     0,     0,
       0,     0,     0,  1412,   230,  -225,  -225,  -225,     0,  -225,
       0,  -225,  -225,  -225,  -225,  -225,  -225,  -225,  -225,  -225,
    -225,  -225,  -225,  -225,   589,     0,     0,     0,     0,   231,
       0,   590,     0,   591,   592,     0,     0,     0,  1444,     0,
       0,   628,  1446,  1173,  1173,   232,   233,     0,     0,  1034,
       0,     0,     0,     0,     0,   234,     0,     0,     0,     0,
       0,     0,     0,  -225,    92,    93,   226,    95,   235,     0,
       0,    42,   236,   237,   227,   228,     0,   226,     0,     0,
       0,   238,     0,  1478,     0,   227,   228,     0,     0,     0,
    -765,   230,     0,     0,     0,     0,     0,  1481,     0,     0,
       0,     0,     0,     0,   882,     0,   883,   884,   885,   886,
       0,   887,     0,   888,   889,     0,   231,  1252,     0,     0,
     890,     0,   891,   892,   893,     0,     0,     0,     0,     0,
    -765,     0,   232,   233,     0,  1034,     0,     0,     0,     0,
       0,     0,   234,     0,     0,     0,     0,  1539,     0,   507,
       0,    92,    93,     0,    95,   235,     0,     0,    42,   236,
     237,  -225,  -225,  -225,  -225,     0,     0,     0,   238,  1186,
       0,  1186,     0,     0,     0,     0,     0,     0,     0,     0,
     230,     0,     0,   813,     0,     0,     0,     0,     0,     0,
       0,   230,  -225,     0,     0,  -225,  -225,  -225,     0,     0,
     412,     0,     0,     0,     0,   231,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   231,     0,     0,  1253,
    1253,   232,   233,     0,     0,     0,  1578,  1034,     0,     0,
       0,   234,   232,   233,   902,     0,     0,     0,     0,     0,
      92,    93,   234,    95,   235,     0,  1216,    42,   236,   237,
       0,    92,    93,     0,    95,   235,     0,   238,   339,   236,
     237,     0,   309,     0,  -765,     0,   899,   902,   238,     0,
       0,  1606,  1607,  1608,  1609,  1610,  1611,  1612,  1613,  1614,
    1615,  1616,  1617,  1618,  1619,  1620,  1624,  1625,  1627,  1629,
       0,     0,     0,     0,     0,     0,     0,   956,   956,     0,
       0,     0,   -88,   -88,   -88,     0,   -88,     0,   -88,   -88,
     -88,   -88,   -88,   -88,   -88,   -88,   -88,   -88,   -88,   -88,
     -88,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1663,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     -88,     0,     0,     0,     0,     0,  1216,     0,     0,     0,
       0,   226,     0,     0,     0,     0,     0,     0,     0,   227,
     228,   617,     0,  1690,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1691,  1692,     0,     0,     0,  1693,     0,  1694,     0,     0,
     618,     0,     0,     0,     0,     0,     0,     0,   619,     0,
     620,   621,   622,   623,     0,   624,     0,   625,   626,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   902,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   -88,   -88,
     -88,   -88,   902,     0,     0,  1716,     0,     0,     0,     0,
       0,     0,     0,   226,     0,     0,     0,     0,     0,     0,
       0,   227,   228,  1173,     0,   230,     0,     0,     0,   -88,
     -88,  1728,   -88,   -88,   -88,     0,   -88,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1735,     0,  1716,  1738,
     231,     0,     0,   226,     0,  1740,     0,     0,     0,     0,
    1743,   227,   228,     0,     0,     0,   232,   233,    89,     0,
       0,     0,     0,     0,     0,     0,   234,     0,     0,     0,
       0,     0,     0,  1252,  1252,    92,    93,     0,    95,   235,
       0,   868,   627,   505,   237,   869,   870,   871,   872,   873,
     874,     0,   238,   226,     0,     0,     0,   875,   876,   877,
     878,   227,   228,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   230,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   882,     0,   883,     0,   885,   886,     0,   887,     0,
     888,   889,   231,     0,     0,     0,  1304,   890,     0,   891,
     892,   893,     0,     0,  1305,  1306,   899,   230,   232,   233,
       0,     0,     0,     0,     0,     0,     0,   899,   234,     0,
       0,     0,     0,     0,     0,     0,     0,    92,    93,  1307,
      95,   235,   231,   226,   537,   236,   237,  1247,     0,     0,
       0,   227,   228,   598,   238,     0,     0,     0,   232,   233,
     599,     0,   600,   601,     0,     0,     0,   230,   234,     0,
       0,     0,     0,     0,     0,     0,     0,    92,    93,     0,
      95,   235,     0,     0,    42,   236,   237,     0,     0,     0,
     598,     0,   231,   226,   238,     0,     0,   599,     0,   600,
     601,   227,   228,     0,     0,  1308,     4,     0,   232,   233,
       0,     0,     0,     0,     0,     0,     0,     0,   234,     0,
     230,     0,     0,     0,     0,     0,     0,    92,    93,     0,
      95,   235,     0,   226,    42,   236,   237,     0,     0,     0,
       0,   227,   228,   504,   238,   231,     8,     0,     0,     0,
       0,  1309,     0,     0,     0,     0,     0,   230,     0,     0,
       0,  1310,  1311,     0,     0,     0,     0,   226,     0,     0,
       0,   234,     0,     0,     0,   227,   228,     0,     0,     0,
    1312,  1313,   231,  1314,  1315,     0,     0,    42,  1316,   237,
       0,     0,     0,     0,     0,     0,     0,   238,   232,   233,
       0,     0,     0,     0,   226,     0,     0,   230,   234,     0,
       0,     0,   227,   228,   928,     0,     0,    92,    93,     0,
      95,   235,     0,     0,    42,   236,   237,     0,     0,     0,
       0,     0,   231,   226,   238,     0,     0,     0,     0,     0,
       0,   227,   228,  1260,     0,     0,     0,   230,   232,   233,
     397,     0,     0,     0,     0,     0,     0,     0,   234,     0,
       0,     0,     0,     0,     0,     0,     0,    92,    93,     0,
      95,   235,   231,     0,    42,   236,   237,     0,     0,     0,
      12,   230,     0,   226,   238,     0,     0,     0,   232,   233,
      89,   227,   228,  -395,     0,     0,     0,     0,   234,     0,
       0,     0,     0,     0,     0,     0,   231,    92,    93,     0,
      95,   235,   226,     0,    42,   505,   237,     0,   230,     0,
     227,   228,   232,   233,   238,     0,     0,     0,     0,     0,
       0,     0,   234,     0,     0,     0,     0,     0,     0,     0,
       0,    92,    93,   231,    95,   235,     0,   230,    42,   236,
     237,     0,     0,     0,     0,     0,     0,   226,   238,   232,
     233,     0,     0,     0,     0,   227,   228,     0,   226,   234,
       0,     0,   231,     0,     0,     0,   227,   228,    92,    93,
       0,    95,   235,     0,     0,    42,   236,   237,   232,   233,
       0,     0,     0,     0,     0,   238,     0,   230,   234,     0,
       0,     0,     0,     0,     0,     0,     0,    92,    93,     0,
      95,   235,     0,     0,    42,   236,   237,     0,     0,     0,
       0,     0,   231,   226,   238,     0,   230,     0,     0,     0,
       0,   227,   228,     0,     0,     0,     0,     0,   232,   233,
       0,     0,     0,     0,     0,     0,     0,   226,   234,     0,
       0,   231,     0,     0,     0,   227,   228,    92,    93,     0,
      95,   235,     0,     0,    42,   236,   237,   232,   233,     0,
       0,   230,     0,     0,   238,     0,   226,   234,     0,     0,
       0,     0,   230,     0,   227,   228,    92,    93,     0,    95,
     235,     0,     0,    42,   236,   353,   231,     0,     0,     0,
       0,     0,     0,   238,     0,     0,   226,   231,     0,     0,
       0,     0,   232,   233,   227,   228,     0,     0,     0,     0,
       0,     0,   234,   232,   233,     0,     0,     0,     0,     0,
       0,    92,    93,   234,    95,   235,     0,   230,    42,   236,
     356,     0,    92,    93,     0,    95,   235,     0,   238,    42,
     236,   405,     0,     0,     0,     0,     0,     0,     0,   238,
       0,   230,   231,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   232,   233,
       0,     0,     0,     0,     0,     0,   231,     0,   234,     0,
     230,     0,     0,     0,     0,     0,     0,    92,    93,     0,
      95,   235,   232,   233,    42,   236,   237,     0,     0,     0,
       0,    79,   234,     0,   238,   231,     0,     0,    80,     0,
     230,    92,    93,     0,    95,   235,     0,     0,   537,   236,
     237,   232,   233,     0,     0,     0,     0,     0,   238,     0,
       0,   234,     0,     0,     0,   231,     0,     0,     0,     0,
      92,    93,     0,    95,   235,     0,     0,    42,   236,  1033,
       0,   232,   233,     0,     0,     0,     0,   238,    81,     0,
       0,   234,     0,     0,     0,     0,     0,     0,     0,     0,
      92,    93,     0,    95,   235,    82,    83,  1087,  1088,  1089,
       0,     0,     0,     0,     0,     0,     0,   238,    84,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -450,     0,    85,
      80,    86,    87,     0,     0,     0,   155,  -463,     0,  -476,
       0,     0,     0,     0,    88,  -708,   156,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    89,     0,     0,    90,    91,  -334,
       0,     0,  -334,  -334,  -334,  -334,   157,     0,     0,     0,
    -334,    92,    93,    94,    95,     0,  -334,     0,    96,    97,
       0,   158,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -708,  -708,  -708,     0,   159,     0,     0,
      84,     0,  -708,     0,   160,    80,     0,   161,   162,     0,
     163,   155,     0,   164,     0,     0,   165,   166,   167,     0,
    -708,   156,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,   155,
       0,     0,   168,     0,     0,     0,     0,     0,     0,   156,
       0,   157,     0,     0,     0,     0,    89,  -708,  -708,    90,
       0,     0,     0,     0,     0,     0,   158,     0,     0,     0,
       0,     0,     0,    92,    93,     0,    95,     0,     0,   157,
     169,    97,   159,     0,     0,    84,  -128,     0,     0,   160,
       0,     0,   161,   162,   158,   163,     0,     0,   164,     0,
       0,   165,   166,   167,     0,     0,     0,     0,     0,     0,
     159,     0,     0,    84,  -129,     0,     0,   160,     0,     0,
     161,   162,     0,   163,   155,     0,   164,   168,     0,   165,
     166,   167,     0,     0,   156,     0,     0,     0,     0,     0,
       0,    89,     0,     0,    90,     0,     0,     0,     0,     0,
       0,     0,   822,     0,     0,   168,  -128,     0,    92,    93,
       0,    95,     0,     0,   157,   169,    97,     0,     0,    89,
       0,     0,    90,     0,     0,     0,     0,     0,     0,   158,
       0,     0,     0,     0,  -129,     0,    92,    93,     0,    95,
       0,     0,   157,   169,    97,   159,     0,     0,    84,  -125,
       0,     0,   160,     0,     0,   161,   162,   158,   163,     0,
       0,   164,     0,     0,   165,   166,   167,     0,  1038,     0,
       0,     0,     0,   159,     0,     0,     0,     0,     0,     0,
     160,     0,     0,   161,   162,     0,   163,   822,     0,   164,
     168,     0,   165,   166,   167,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    89,     0,     0,    90,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   168,  -125,
       0,    92,    93,     0,    95,     0,     0,   157,   169,    97,
       0,     0,    89,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   158,     0,     0,     0,     0,     0,     0,    92,
      93,     0,    95,     0,     0,     0,    42,    97,   159,     0,
       0,     0,     0,     0,     0,   160,     0,     0,   161,   162,
       0,   163,     0,     0,   164,     0,     0,   165,   166,   167,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   168,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    89,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    92,    93,     0,    95,     0,     0,
       0,    42,    97
};

static const yytype_int16 yycheck[] =
{
      11,   294,   168,   459,    73,   220,    52,   288,   459,   213,
      52,    52,   294,    90,    56,   497,    57,   384,   154,   925,
     384,    62,    63,   655,   154,   291,   722,   494,   860,   606,
      52,  1011,   136,   219,   237,   109,   322,   551,   164,   165,
      52,    52,   294,   141,  1011,   294,    57,   213,   674,   319,
     939,    62,    63,    94,   100,   258,   987,   237,   100,   100,
     591,    52,    73,   104,   696,   497,   305,   601,  1033,   600,
     885,   886,   258,   259,   570,   869,   870,   871,   100,   495,
     940,   941,   876,    94,  1277,   168,   947,   172,   100,   100,
     110,  1104,   626,   104,   625,  1282,  1185,     3,     5,  1286,
    1516,    21,  1289,    19,    20,    24,     3,   945,     3,   100,
       3,  1277,     3,    56,     3,     3,    97,  1277,    24,     4,
       3,     5,    97,    27,     3,     3,     3,    24,     3,     5,
       3,   172,     3,     3,     3,    24,    47,     5,   110,     3,
       3,    24,   137,   382,     3,    24,    24,     3,   192,    24,
     353,   220,    52,   356,   198,    24,   732,     3,    64,   237,
     170,   172,     3,   739,   185,   146,   459,    64,    21,     3,
       3,   146,     3,   353,     3,    64,   356,   459,     3,     3,
     258,    64,   192,    24,   399,    64,    64,  1093,     3,    64,
       3,    24,     3,   560,     3,    64,     3,   333,     3,   194,
     100,     3,   405,   333,     3,   420,     3,  1172,     3,    24,
       3,    24,    24,     3,     3,    24,   159,    24,   192,     3,
     192,   132,   305,    64,     3,   405,   237,   353,   192,   259,
     356,    64,   418,     5,  1427,   304,    21,   311,   418,   425,
     325,   310,   428,   317,    21,   431,    24,   679,  1086,    64,
     319,    64,    64,   322,    24,    64,   522,    64,  1451,    19,
      20,     3,    18,   714,     3,     3,    22,   155,   279,     3,
     155,   147,     3,   189,   194,   353,   192,   193,   356,   198,
     976,  1697,    21,   294,   325,  1451,    64,   194,   192,   374,
     388,  1451,   198,   586,   497,   315,   316,   380,   381,   382,
     195,   198,   195,  1392,   195,  1270,   195,   200,   724,   198,
     194,   497,   201,   606,   325,   198,  1131,   497,   195,   198,
     198,  1182,   195,   198,   195,   195,   194,   405,  1122,   198,
     399,   195,   195,   374,   586,   501,   195,    24,   384,   195,
     331,   194,   353,   384,   878,   356,   877,    52,   359,   195,
     976,   420,   421,   884,   606,   851,   887,   198,   892,  1467,
     891,   195,   305,   374,   195,   198,   195,    29,   144,    31,
     195,   195,  1565,   384,  1567,   147,   155,    64,    52,  1565,
    1573,   620,  1014,   198,   195,   198,   198,    24,   418,   198,
     195,   198,  1585,   195,   405,   100,   195,   173,   195,  1585,
     195,   431,   195,   394,   395,   195,   195,   169,   170,   194,
     421,   195,  1599,   161,   192,   192,   155,   155,   459,   497,
     198,   155,   499,   500,   155,   194,   100,    64,    24,   722,
     192,  1566,  1427,   193,   111,   112,   192,   380,   381,   382,
     722,   384,    24,   185,   186,   519,   188,    97,   459,    52,
     192,  1586,   667,   719,   495,   496,   497,   561,   195,    48,
     664,   587,   588,   589,   668,   669,    24,  1356,    64,    24,
     722,   155,   598,   722,   572,   611,   679,   325,  1409,  1587,
     557,   611,    64,  1448,   495,   496,   497,   690,  1468,   195,
     501,    19,    20,   679,   171,    24,   146,   100,   664,   679,
     766,  1468,   668,   669,   690,  1518,    64,   125,   192,    64,
     551,   198,   193,   194,   560,   155,    97,   147,   560,   560,
    1380,   532,   111,   112,   790,   567,   374,  1342,     3,  1011,
      24,   987,   999,  1015,   161,    64,   987,   620,   560,   809,
     551,   811,   192,   554,  1359,   567,   114,    22,   560,   560,
     591,   592,   192,    19,    20,   192,   742,    52,   176,   600,
     601,   198,   742,  1195,   194,   146,  1080,   117,   118,    24,
      64,  1566,  1567,  1150,   192,   586,  1370,   192,  1573,  1011,
     591,   592,   171,  1015,   625,   626,   192,    24,   194,   600,
     601,  1586,    29,   192,    31,   606,   192,   125,   667,   640,
     168,   679,   198,    19,    20,   100,   194,  1513,  1588,    64,
     192,   192,   690,    24,   625,   626,   198,   560,   103,    21,
      22,  1588,   150,   108,   567,   193,   111,    24,   639,   640,
    1164,    29,  1163,    31,   192,   155,   155,   192,   166,   167,
     198,    22,   192,   198,   655,  1477,   194,   590,   176,     4,
      87,   175,   200,    64,   194,   140,   599,   185,   186,   125,
     188,   189,   168,   192,   192,   193,   194,    64,   679,   198,
     194,   195,   192,     3,   202,    33,   200,   620,   115,   622,
     623,   624,    18,   724,   194,   696,    22,   193,    18,    87,
      48,   168,    22,   175,   987,     3,   765,   823,   192,   173,
     166,   167,   988,   714,   198,   987,   183,     6,     7,   125,
     176,   722,   194,   724,    22,   194,   193,   115,   131,   185,
     186,   200,   188,   189,    99,   100,   192,   193,   194,   192,
     804,   189,   195,   173,   174,   193,   173,   192,   201,   808,
     809,   845,   811,   198,    24,   141,   183,  1653,   874,    29,
     192,    31,  1448,   195,   105,   853,   107,   768,   143,  1226,
     176,   198,   888,   192,   425,   125,   195,   428,     5,   185,
     186,   168,   188,   189,  1225,   173,   192,   193,   192,   193,
     906,   192,   173,   174,   920,   826,   912,   198,   185,   186,
     920,   188,   158,   159,   160,   192,   193,  1201,   113,  1203,
     926,   198,   192,   872,     4,   195,   194,   750,  1011,  1441,
     194,   201,  1015,   113,   860,   826,   176,  1184,  1111,   860,
    1184,   105,   106,    40,    41,   185,   186,   194,   188,  1015,
    1033,  1011,   192,  1602,  1603,  1015,   877,   878,    28,    29,
      30,    31,    38,   884,   885,   886,   887,    37,   889,   860,
     891,   892,   893,  1033,   194,   938,   194,  1150,   927,  1111,
      62,    63,  1028,   208,   209,  1321,   877,   878,   194,  1325,
     326,   327,   194,   884,   885,   886,   887,    47,   889,   193,
     891,   892,   893,   925,   925,   194,  1089,   194,   194,   201,
      12,    13,    14,    15,    16,    17,    18,    87,  1150,   940,
     941,     5,    29,   168,    31,   916,   201,   175,    23,  1089,
     176,   177,   178,   179,   925,    10,     8,   860,   184,   988,
     185,   186,   194,   188,   190,   115,   192,   192,   193,   940,
     941,   194,   875,  1011,    68,    69,    70,  1015,    72,   192,
      29,  1010,    31,   192,   192,   192,   987,   890,     4,  1230,
       5,   192,   147,  1409,   200,  1033,   169,   105,  1409,   194,
      87,   185,   186,  1089,   188,   976,   192,    29,   192,    31,
     195,   195,   168,   194,  1015,     5,   987,   176,   177,   178,
     179,   195,   925,   173,     5,   184,  1468,     5,   115,   185,
     186,   190,   188,   151,     3,   938,   192,   193,    87,  1010,
    1011,   175,  1128,  1014,  1015,   203,  1083,   175,   198,  1275,
     175,  1089,   195,   185,   186,   175,   188,  1028,  1144,   195,
     192,  1388,  1033,   195,  1388,    87,   115,   194,  1321,  1233,
     192,  1234,  1325,     5,    22,  1161,   192,   192,     3,  1080,
     162,   163,   164,   165,  1113,   194,   173,     3,  1234,   195,
     111,  1093,  1093,   115,  1234,  1066,   183,    99,   100,   101,
     102,   185,   186,   195,   188,     4,   174,  1233,   192,  1080,
     155,   195,   192,     3,   196,   197,     4,     3,  1089,   192,
    1091,   200,  1093,    11,   173,   194,     4,     4,   195,   131,
    1131,    19,    20,    21,   183,   185,   186,   139,   188,   114,
    1111,  1287,   192,   194,   194,   192,  1588,   185,   186,   135,
     188,   173,   185,   186,   192,   188,  1409,   183,  1159,   192,
    1131,   183,  1163,  1164,   195,   195,   125,  1409,   185,   186,
     194,   188,   183,   185,   186,   192,   188,   194,  1184,  1150,
     192,   195,     5,  1184,   108,    73,   192,   111,  1159,    39,
    1093,   150,  1163,  1164,   192,   183,  1234,   121,  1227,   123,
      65,    66,   126,   127,   183,     3,    94,   166,   167,   175,
     195,    18,     5,  1184,     3,    80,   140,   176,   194,    84,
      22,   145,   194,  1194,  1195,    90,   185,   186,     3,   188,
     189,   192,   194,   192,   193,   194,    21,   125,   194,    21,
     195,   104,   194,   202,   194,     3,     3,   194,  1277,   194,
       3,   192,   192,  1282,  1225,   147,   170,  1286,   194,  1162,
    1289,   194,   150,  1234,   140,   194,   155,     3,   115,  1298,
       3,     5,     3,     5,    30,     5,   195,     3,   166,   167,
     195,  1184,    31,    29,   195,   195,   195,    25,   176,  1375,
     195,     3,     3,   158,  1705,     5,   104,   185,   186,     4,
     188,   189,  1718,   168,   192,   193,   194,   194,  1279,   192,
     195,   103,   194,    21,   202,     4,   108,    52,    20,   111,
    1321,  1350,    57,     4,  1325,   195,   192,    62,    63,   121,
       5,   123,   192,   195,   126,   127,   195,   195,   195,     3,
       3,  1342,   195,     4,    26,    73,    94,   195,   140,     5,
    1321,   195,     5,   145,  1325,   200,     4,  1260,  1359,    94,
       3,    99,   100,   101,   102,   100,   194,   104,   192,   104,
     195,  1342,   192,   195,   200,   194,     4,   195,  1407,  1380,
     104,   147,  1388,     3,  1559,     3,     5,  1388,  1359,     4,
       3,     5,     3,   131,  1558,   133,   134,   135,  1427,   194,
     138,   139,     5,   141,   142,     5,    22,   195,  1409,  1380,
     275,  1575,     3,    21,     4,   280,     3,  1388,   283,     3,
       3,     3,  1451,     4,   195,   290,     3,   195,     4,   192,
     195,   195,  1558,   195,   194,     3,   195,   172,  1409,   304,
     192,   192,   307,     3,     5,   192,     4,   192,     4,  1575,
      21,   195,   195,     3,   195,   195,   195,    22,     4,     3,
       3,     3,  1600,     4,     4,  1718,  1369,  1468,   195,   194,
    1441,  1477,   195,   195,   195,   195,  1477,  1448,   343,   344,
       3,   346,     4,    49,  1729,  1388,  1011,   920,   100,  1011,
     355,    20,   357,  1275,   412,  1278,  1231,  1526,   719,  1287,
     519,  1279,  1451,   368,    11,   370,  1477,  1011,   103,   421,
    1441,  1513,  1513,   108,  1440,  1567,   111,   790,  1056,  1568,
    1427,   405,  1523,   252,   722,   390,   121,  1573,   123,   792,
    1559,   126,   127,  1534,  1015,  1011,  1565,  1566,  1567,   496,
    1066,   985,  1513,  1707,  1573,   140,   804,  1295,  1705,  1225,
     145,  1574,  1523,   460,   893,   998,  1585,  1586,   554,  1028,
     560,  1524,  1359,  1534,  1111,  1534,  1526,   560,  1535,  1350,
    1599,   266,   447,   452,  1477,   448,   450,   765,   809,   806,
     386,  1707,   811,   318,   455,   565,   560,  1588,   565,   560,
     325,     0,     1,   560,  1390,  1128,   619,   882,  1375,   318,
     560,   381,   467,   621,   938,   941,   471,   560,  1448,   327,
    1513,   560,  1380,   912,   560,    24,   560,   482,   906,    28,
      29,    30,    31,   560,  1144,     4,   333,    54,    37,  1600,
    1697,   281,    11,  1662,   499,   500,   468,   560,   503,   374,
      19,    20,    21,   696,   509,   380,   381,   712,  1677,   384,
     639,  1653,  1653,    26,  1194,    64,     4,  1403,  1195,  1198,
     659,   526,  1091,    11,    -1,    -1,    -1,    -1,    -1,    -1,
     535,    19,    20,    21,    -1,    -1,    -1,    -1,    87,   544,
      -1,    -1,  1653,    -1,    -1,    -1,    -1,   552,    -1,    -1,
      -1,    -1,   557,    -1,  1723,    -1,    -1,  1726,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,    -1,
    1739,   576,    -1,    -1,    -1,    -1,    -1,  1718,    -1,    -1,
      -1,     5,     6,     7,     8,     9,    10,    -1,    12,    13,
      14,    15,    16,    17,  1705,    19,    20,    21,    22,    23,
      -1,    -1,    -1,    -1,    -1,    -1,   125,  1718,    -1,    -1,
    1653,  1654,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     495,   496,   497,    -1,   173,    -1,    -1,   176,   177,   178,
     179,   150,    -1,   638,    -1,   184,   155,   125,    -1,    -1,
      -1,   190,    -1,    -1,   649,     4,    -1,   166,   167,   198,
      -1,    -1,    11,   658,    -1,    -1,    -1,   176,    -1,    -1,
      19,    20,   150,    -1,    -1,    -1,   185,   186,    -1,   188,
     189,    -1,    -1,   192,   193,   194,   551,    -1,   166,   167,
      -1,    -1,    -1,   202,    -1,   560,    -1,    -1,   176,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   185,   186,    -1,
     188,   189,    -1,    -1,   192,   193,   194,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   202,    -1,   591,   592,   723,    -1,
      -1,    -1,    -1,    -1,    -1,   600,   601,    -1,    -1,    -1,
      -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    11,    -1,
      -1,   746,    -1,   748,   619,    -1,    19,    20,    -1,    -1,
     625,   626,    -1,    -1,    -1,    -1,    -1,    -1,   763,    -1,
      -1,    -1,    -1,    -1,   769,   640,   125,    -1,    -1,    -1,
      -1,   195,   196,   197,    -1,    -1,    -1,     4,    -1,    -1,
     655,    -1,    -1,    -1,    11,    -1,    -1,    -1,    38,    -1,
      -1,   150,    19,    20,    -1,    -1,    -1,   802,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,   814,
      -1,    -1,    -1,    -1,   819,    -1,   821,   176,    -1,    -1,
      -1,   696,    -1,    -1,    -1,    -1,   185,   186,    -1,   188,
     189,    -1,    -1,   192,   193,   194,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   202,   849,    -1,    -1,    -1,    -1,   724,
      -1,    -1,   125,    -1,    -1,   860,    -1,    -1,    -1,   109,
      -1,    -1,    -1,    -1,   114,    -1,    -1,    -1,    -1,   119,
     120,    -1,   122,    -1,    -1,   125,   157,   150,   128,   129,
     130,    -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,
      -1,    -1,    -1,   166,   167,    -1,    -1,    -1,   125,    -1,
      -1,    -1,   907,   176,    -1,    -1,    -1,    -1,   913,    -1,
      -1,    -1,   185,   186,    -1,   188,   189,    -1,   168,   192,
     193,   194,    -1,   150,    -1,    -1,    -1,    -1,    -1,   202,
      -1,    -1,    -1,    -1,    -1,   185,   186,    -1,   188,   166,
     167,    -1,   192,   193,   949,    -1,    -1,   952,   953,   176,
     231,   826,    -1,    -1,    -1,    -1,   237,    -1,   185,   186,
      -1,   188,   189,    -1,    -1,   192,   193,   194,    -1,    -1,
      -1,   252,     4,    -1,    -1,   202,    -1,    -1,    -1,    11,
      -1,    -1,    -1,    -1,    -1,   860,    -1,    19,    20,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   877,   878,    -1,    -1,    -1,    -1,    -1,   884,
     885,   886,   887,    -1,   889,    -1,   891,   892,   893,    -1,
      -1,    -1,    -1,    -1,   305,   306,    -1,    -1,    -1,    -1,
    1035,    -1,    -1,  1038,    -1,    -1,    -1,   318,    -1,    -1,
     321,    -1,    -1,    -1,    -1,  1050,    -1,  1052,    -1,    -1,
     925,    -1,  1057,    -1,    -1,    -1,   337,   338,    -1,  1064,
    1065,    -1,    -1,   938,    -1,   940,   941,    -1,    -1,    -1,
      -1,    -1,   353,     4,    -1,   356,    -1,    -1,  1083,  1084,
      11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,    20,
      -1,    -1,  1097,   125,  1099,    11,    -1,   378,    -1,   380,
     381,   382,    -1,    19,    20,  1110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,
      -1,    -1,    -1,    -1,   405,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   166,   167,  1011,    94,    -1,  1014,
    1015,    57,    58,    -1,   176,    -1,    -1,   104,    -1,    65,
      -1,    67,    68,   185,   186,    -1,   188,   189,    -1,    -1,
     192,   193,   194,    -1,    -1,  1170,    -1,    -1,    -1,  1174,
     202,    -1,    -1,  1178,    -1,  1180,  1181,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1191,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1080,    -1,    -1,    -1,   125,
      -1,    -1,    -1,    -1,  1219,    -1,    -1,   498,  1093,   150,
     501,  1226,     4,    -1,  1229,    -1,    -1,  1232,    -1,    11,
      -1,    -1,   513,    -1,   150,   166,   167,    19,    20,    -1,
      -1,    -1,    -1,    -1,    -1,   176,    -1,    -1,    -1,    -1,
     166,   167,  1257,    -1,   185,   186,  1131,   188,   189,     4,
     176,   192,   193,   194,    -1,    -1,    11,    -1,    -1,   185,
     186,   202,   188,   189,    19,    20,   192,   193,   194,    -1,
      -1,    -1,    -1,    -1,  1159,    -1,   202,    -1,  1163,  1164,
      -1,    -1,    -1,    -1,  1299,    -1,  1301,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1184,
      -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,  1194,
    1195,    11,    -1,    -1,    -1,    -1,    -1,  1332,    -1,    19,
      20,    -1,    -1,  1338,    -1,    -1,    -1,    -1,    -1,   620,
      -1,    -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,  1354,
      -1,    -1,    -1,  1358,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   318,    -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,
      -1,  1376,    -1,  1378,   655,    -1,  1381,    -1,  1383,  1384,
     125,  1386,    -1,    -1,   166,   167,    -1,    -1,    -1,    -1,
      -1,    -1,  1397,   674,   176,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   185,   186,   150,   188,   189,    -1,    -1,
     192,   193,   194,    -1,    -1,   696,    -1,    -1,    -1,    -1,
     202,   166,   167,   380,   381,    -1,   707,    -1,   709,    -1,
      -1,   176,    -1,    -1,    -1,   125,    -1,    -1,    -1,    -1,
     185,   186,    -1,   188,   189,  1450,    -1,   192,   193,   194,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   202,    -1,    -1,
     150,    -1,    -1,    -1,    -1,    -1,    -1,  1342,   749,    -1,
     751,    -1,    -1,    -1,    -1,    -1,   166,   167,   759,    -1,
      -1,   762,    -1,    -1,  1359,    -1,   176,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   185,   186,    -1,   188,   189,
      -1,    -1,   192,   193,   194,  1380,    -1,    -1,    -1,  1514,
      -1,    -1,   202,  1388,    -1,  1520,    -1,    -1,    -1,    -1,
    1525,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1536,  1537,  1538,    -1,    -1,    -1,    -1,   495,   496,
       5,    -1,  1547,    -1,    -1,    -1,    11,    -1,    -1,    -1,
    1555,    -1,  1557,    -1,    19,    20,    -1,    22,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1440,  1441,   848,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   868,   869,   870,
     871,    -1,    -1,  1468,   551,   876,    -1,    -1,    -1,     5,
      -1,   882,  1477,    -1,    -1,    11,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    19,    20,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1630,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   591,   592,    -1,   918,  1513,    -1,
      -1,    -1,  1647,   600,   601,    -1,    -1,    -1,  1523,    -1,
      -1,    -1,    -1,  1658,    -1,    -1,    -1,   938,   939,  1534,
     125,    -1,   619,    -1,   945,     5,    -1,   948,   625,   626,
      -1,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,
      20,    -1,    -1,   640,    -1,   150,    -1,    -1,    -1,    -1,
      -1,  1696,    -1,    -1,  1699,   976,    -1,    -1,   655,    -1,
      -1,   166,   167,   984,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   176,    -1,  1588,    -1,    -1,    -1,    -1,    -1,   125,
     185,   186,    -1,   188,   189,    -1,    -1,   192,   193,   194,
      -1,    -1,    -1,  1014,    -1,    -1,    -1,   202,    -1,   696,
      -1,    -1,  1023,    -1,   150,    -1,  1027,  1028,    -1,  1030,
    1031,    -1,  1033,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     166,   167,    -1,    -1,    -1,    -1,    -1,   724,    -1,    -1,
     176,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1653,   185,
     186,    -1,   188,   189,    -1,   125,   192,   193,   194,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   202,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1086,    -1,    -1,  1089,    -1,
     150,  1092,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     5,    -1,   166,   167,    -1,    -1,
      11,    -1,    -1,    -1,    -1,    -1,   176,    -1,    19,    20,
      -1,  1122,    -1,    -1,    -1,   185,   186,    -1,   188,   189,
      -1,    -1,   192,   193,   194,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   202,    -1,    -1,    -1,    -1,    -1,    -1,   826,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1172,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1185,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1194,  1195,    -1,    -1,    11,    -1,    -1,
     877,   878,    -1,    -1,    -1,    19,    20,   884,   885,   886,
     887,    -1,   889,    -1,   891,   892,   893,    -1,    -1,    -1,
      -1,    -1,    -1,  1224,   125,     6,     7,     8,    -1,    10,
      -1,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    58,    -1,    -1,    -1,    -1,   150,
      -1,    65,    -1,    67,    68,    -1,    -1,    -1,  1259,    -1,
      -1,   938,  1263,   940,   941,   166,   167,    -1,    -1,  1270,
      -1,    -1,    -1,    -1,    -1,   176,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    64,   185,   186,    11,   188,   189,    -1,
      -1,   192,   193,   194,    19,    20,    -1,    11,    -1,    -1,
      -1,   202,    -1,  1304,    -1,    19,    20,    -1,    -1,    -1,
      24,   125,    -1,    -1,    -1,    -1,    -1,  1318,    -1,    -1,
      -1,    -1,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      -1,    56,    -1,    58,    59,    -1,   150,  1014,    -1,    -1,
      65,    -1,    67,    68,    69,    -1,    -1,    -1,    -1,    -1,
      64,    -1,   166,   167,    -1,  1356,    -1,    -1,    -1,    -1,
      -1,    -1,   176,    -1,    -1,    -1,    -1,  1368,    -1,  1370,
      -1,   185,   186,    -1,   188,   189,    -1,    -1,   192,   193,
     194,   162,   163,   164,   165,    -1,    -1,    -1,   202,  1390,
      -1,  1392,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     125,    -1,    -1,  1080,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   125,   193,    -1,    -1,   196,   197,   198,    -1,    -1,
     201,    -1,    -1,    -1,    -1,   150,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,  1440,
    1441,   166,   167,    -1,    -1,    -1,  1447,  1448,    -1,    -1,
      -1,   176,   166,   167,  1131,    -1,    -1,    -1,    -1,    -1,
     185,   186,   176,   188,   189,    -1,  1467,   192,   193,   194,
      -1,   185,   186,    -1,   188,   189,    -1,   202,   192,   193,
     194,    -1,  1159,    -1,   198,    -1,  1163,  1164,   202,    -1,
      -1,  1492,  1493,  1494,  1495,  1496,  1497,  1498,  1499,  1500,
    1501,  1502,  1503,  1504,  1505,  1506,  1507,  1508,  1509,  1510,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1194,  1195,    -1,
      -1,    -1,     6,     7,     8,    -1,    10,    -1,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1562,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      64,    -1,    -1,    -1,    -1,    -1,  1587,    -1,    -1,    -1,
      -1,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,
      20,    21,    -1,  1604,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1621,  1622,    -1,    -1,    -1,  1626,    -1,  1628,    -1,    -1,
      50,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    58,    -1,
      60,    61,    62,    63,    -1,    65,    -1,    67,    68,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1342,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,  1359,    -1,    -1,  1686,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    11,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    19,    20,  1380,    -1,   125,    -1,    -1,    -1,   193,
     194,  1712,   196,   197,   198,    -1,   200,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1727,    -1,  1729,  1730,
     150,    -1,    -1,    11,    -1,  1736,    -1,    -1,    -1,    -1,
    1741,    19,    20,    -1,    -1,    -1,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   176,    -1,    -1,    -1,
      -1,    -1,    -1,  1440,  1441,   185,   186,    -1,   188,   189,
      -1,    49,   192,   193,   194,    53,    54,    55,    56,    57,
      58,    -1,   202,    11,    -1,    -1,    -1,    65,    66,    67,
      68,    19,    20,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    49,    -1,    51,    -1,    53,    54,    -1,    56,    -1,
      58,    59,   150,    -1,    -1,    -1,    11,    65,    -1,    67,
      68,    69,    -1,    -1,    19,    20,  1523,   125,   166,   167,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1534,   176,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   185,   186,    44,
     188,   189,   150,    11,   192,   193,   194,   195,    -1,    -1,
      -1,    19,    20,    58,   202,    -1,    -1,    -1,   166,   167,
      65,    -1,    67,    68,    -1,    -1,    -1,   125,   176,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   185,   186,    -1,
     188,   189,    -1,    -1,   192,   193,   194,    -1,    -1,    -1,
      58,    -1,   150,    11,   202,    -1,    -1,    65,    -1,    67,
      68,    19,    20,    -1,    -1,   110,    24,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   176,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,   185,   186,    -1,
     188,   189,    -1,    11,   192,   193,   194,    -1,    -1,    -1,
      -1,    19,    20,    21,   202,   150,    64,    -1,    -1,    -1,
      -1,   156,    -1,    -1,    -1,    -1,    -1,   125,    -1,    -1,
      -1,   166,   167,    -1,    -1,    -1,    -1,    11,    -1,    -1,
      -1,   176,    -1,    -1,    -1,    19,    20,    -1,    -1,    -1,
     185,   186,   150,   188,   189,    -1,    -1,   192,   193,   194,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   202,   166,   167,
      -1,    -1,    -1,    -1,    11,    -1,    -1,   125,   176,    -1,
      -1,    -1,    19,    20,    21,    -1,    -1,   185,   186,    -1,
     188,   189,    -1,    -1,   192,   193,   194,    -1,    -1,    -1,
      -1,    -1,   150,    11,   202,    -1,    -1,    -1,    -1,    -1,
      -1,    19,    20,    21,    -1,    -1,    -1,   125,   166,   167,
      94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   176,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   185,   186,    -1,
     188,   189,   150,    -1,   192,   193,   194,    -1,    -1,    -1,
     198,   125,    -1,    11,   202,    -1,    -1,    -1,   166,   167,
     168,    19,    20,    21,    -1,    -1,    -1,    -1,   176,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   150,   185,   186,    -1,
     188,   189,    11,    -1,   192,   193,   194,    -1,   125,    -1,
      19,    20,   166,   167,   202,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   176,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   185,   186,   150,   188,   189,    -1,   125,   192,   193,
     194,    -1,    -1,    -1,    -1,    -1,    -1,    11,   202,   166,
     167,    -1,    -1,    -1,    -1,    19,    20,    -1,    11,   176,
      -1,    -1,   150,    -1,    -1,    -1,    19,    20,   185,   186,
      -1,   188,   189,    -1,    -1,   192,   193,   194,   166,   167,
      -1,    -1,    -1,    -1,    -1,   202,    -1,   125,   176,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   185,   186,    -1,
     188,   189,    -1,    -1,   192,   193,   194,    -1,    -1,    -1,
      -1,    -1,   150,    11,   202,    -1,   125,    -1,    -1,    -1,
      -1,    19,    20,    -1,    -1,    -1,    -1,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    11,   176,    -1,
      -1,   150,    -1,    -1,    -1,    19,    20,   185,   186,    -1,
     188,   189,    -1,    -1,   192,   193,   194,   166,   167,    -1,
      -1,   125,    -1,    -1,   202,    -1,    11,   176,    -1,    -1,
      -1,    -1,   125,    -1,    19,    20,   185,   186,    -1,   188,
     189,    -1,    -1,   192,   193,   194,   150,    -1,    -1,    -1,
      -1,    -1,    -1,   202,    -1,    -1,    11,   150,    -1,    -1,
      -1,    -1,   166,   167,    19,    20,    -1,    -1,    -1,    -1,
      -1,    -1,   176,   166,   167,    -1,    -1,    -1,    -1,    -1,
      -1,   185,   186,   176,   188,   189,    -1,   125,   192,   193,
     194,    -1,   185,   186,    -1,   188,   189,    -1,   202,   192,
     193,   194,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   202,
      -1,   125,   150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,
      -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,   176,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,   185,   186,    -1,
     188,   189,   166,   167,   192,   193,   194,    -1,    -1,    -1,
      -1,    25,   176,    -1,   202,   150,    -1,    -1,    32,    -1,
     125,   185,   186,    -1,   188,   189,    -1,    -1,   192,   193,
     194,   166,   167,    -1,    -1,    -1,    -1,    -1,   202,    -1,
      -1,   176,    -1,    -1,    -1,   150,    -1,    -1,    -1,    -1,
     185,   186,    -1,   188,   189,    -1,    -1,   192,   193,   194,
      -1,   166,   167,    -1,    -1,    -1,    -1,   202,    82,    -1,
      -1,   176,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     185,   186,    -1,   188,   189,    99,   100,   192,   193,   194,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   202,   112,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,   133,
      32,   135,   136,    -1,    -1,    -1,    38,   141,    -1,   143,
      -1,    -1,    -1,    -1,   148,    47,    48,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   168,    -1,    -1,   171,   172,   173,
      -1,    -1,   176,   177,   178,   179,    78,    -1,    -1,    -1,
     184,   185,   186,   187,   188,    -1,   190,    -1,   192,   193,
      -1,    93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   105,   106,   107,    -1,   109,    -1,    -1,
     112,    -1,   114,    -1,   116,    32,    -1,   119,   120,    -1,
     122,    38,    -1,   125,    -1,    -1,   128,   129,   130,    -1,
     132,    48,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    32,    -1,    -1,    -1,    -1,    -1,    38,
      -1,    -1,   154,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      -1,    78,    -1,    -1,    -1,    -1,   168,   169,   170,   171,
      -1,    -1,    -1,    -1,    -1,    -1,    93,    -1,    -1,    -1,
      -1,    -1,    -1,   185,   186,    -1,   188,    -1,    -1,    78,
     192,   193,   109,    -1,    -1,   112,   113,    -1,    -1,   116,
      -1,    -1,   119,   120,    93,   122,    -1,    -1,   125,    -1,
      -1,   128,   129,   130,    -1,    -1,    -1,    -1,    -1,    -1,
     109,    -1,    -1,   112,   113,    -1,    -1,   116,    -1,    -1,
     119,   120,    -1,   122,    38,    -1,   125,   154,    -1,   128,
     129,   130,    -1,    -1,    48,    -1,    -1,    -1,    -1,    -1,
      -1,   168,    -1,    -1,   171,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    38,    -1,    -1,   154,   183,    -1,   185,   186,
      -1,   188,    -1,    -1,    78,   192,   193,    -1,    -1,   168,
      -1,    -1,   171,    -1,    -1,    -1,    -1,    -1,    -1,    93,
      -1,    -1,    -1,    -1,   183,    -1,   185,   186,    -1,   188,
      -1,    -1,    78,   192,   193,   109,    -1,    -1,   112,   113,
      -1,    -1,   116,    -1,    -1,   119,   120,    93,   122,    -1,
      -1,   125,    -1,    -1,   128,   129,   130,    -1,   104,    -1,
      -1,    -1,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,
     116,    -1,    -1,   119,   120,    -1,   122,    38,    -1,   125,
     154,    -1,   128,   129,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   168,    -1,    -1,   171,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   183,
      -1,   185,   186,    -1,   188,    -1,    -1,    78,   192,   193,
      -1,    -1,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    93,    -1,    -1,    -1,    -1,    -1,    -1,   185,
     186,    -1,   188,    -1,    -1,    -1,   192,   193,   109,    -1,
      -1,    -1,    -1,    -1,    -1,   116,    -1,    -1,   119,   120,
      -1,   122,    -1,    -1,   125,    -1,    -1,   128,   129,   130,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   154,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   185,   186,    -1,   188,    -1,    -1,
      -1,   192,   193
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   205,     0,     1,    24,    28,    30,    37,    64,    87,
     115,   173,   198,   206,   207,   208,   209,   211,   212,   265,
     266,   639,   642,   659,   661,   695,   696,   697,   698,   699,
     709,   710,   185,   213,   192,   192,   176,   177,   178,   179,
     184,   190,   192,   232,   267,   316,   210,    24,   198,    24,
     267,   237,   238,   407,   652,   654,   660,   660,    29,    31,
     115,   697,   660,   660,   210,   662,   643,   269,   268,   273,
     270,   272,   271,   194,   195,   195,   210,   198,   209,    25,
      32,    82,    99,   100,   112,   133,   135,   136,   148,   168,
     171,   172,   185,   186,   187,   188,   192,   193,   232,   239,
     240,   241,   252,   260,   261,   289,   291,   293,   332,   333,
     348,   366,   371,   388,   389,   394,   399,   400,   407,   408,
     414,   418,   424,   435,   437,   446,   448,   450,   453,   454,
     455,   516,   517,   542,   543,   544,   546,   552,   553,   556,
     562,   567,   590,   613,   638,   676,   679,   721,   161,   654,
     260,   261,   641,   648,   722,    38,    48,    78,    93,   109,
     116,   119,   120,   122,   125,   128,   129,   130,   154,   192,
     242,   243,   253,   254,   260,   460,   477,   516,   517,   527,
     528,   532,   542,   561,   562,   571,   572,   575,   587,   588,
     589,   590,   591,   596,   603,   607,   610,   613,   626,   629,
     632,   684,   689,   721,   192,   712,   192,   702,   242,   242,
     207,   207,    21,   194,   274,   275,   274,   274,   274,    21,
     194,   285,   286,   287,   274,     4,    11,    19,    20,    21,
     125,   150,   166,   167,   176,   189,   193,   194,   202,   232,
     255,   258,   264,   276,   277,   280,   281,   289,   290,   317,
     318,   323,   325,   435,   484,   485,   486,   487,   488,   491,
     492,   494,   496,   497,   498,   499,   500,   504,   507,   509,
     682,   691,   395,   207,   194,   419,   420,     3,   207,   214,
     547,   155,   192,   677,   678,   155,   402,    22,   425,   207,
     547,     3,   294,   295,   194,   375,   376,   378,   379,   435,
     437,     4,   194,   241,   103,   108,   111,   140,   192,   437,
     194,   292,   265,   368,   402,   131,   141,   143,   147,   194,
     518,     5,   194,   449,   540,   541,   549,   549,   540,   653,
     113,   640,   649,   650,   651,   698,   709,   194,   194,   192,
     232,   236,   504,   560,   192,   207,   261,   192,   198,   724,
     194,   194,   635,   194,   509,   594,   194,   594,   194,   194,
     207,   504,   506,   507,   512,   513,   514,   515,   592,     4,
     113,   247,   248,   244,   245,   246,   253,   638,   111,   121,
     123,   126,   127,   145,   529,    47,   573,   574,   578,   194,
     685,   194,   716,   194,   247,   247,   193,    94,   514,   194,
     277,   288,    73,    94,   264,   194,   324,   327,   328,   329,
     504,   201,   201,   232,   276,   279,   280,   282,   491,   504,
       5,     3,   195,   324,    23,   489,    21,    22,   490,   276,
     277,   491,   277,   487,    12,    13,    14,    15,    16,    17,
      18,   162,   163,   164,   165,   196,   197,   493,   495,    10,
     501,     8,   502,     6,     7,   503,   494,   194,   194,   421,
       3,   207,   117,   118,   232,   548,   207,   677,     3,   207,
     192,   680,   681,   192,    18,    22,   427,   428,   207,   296,
     297,   348,   192,   155,   192,   379,   380,   381,   448,   452,
     453,   454,     3,   373,   374,   429,     3,    22,   111,   112,
     171,   451,   207,   509,    21,   193,   261,   504,   506,   617,
     504,   207,     4,     5,   447,   508,   509,   265,   301,   302,
     303,   304,     3,   335,   336,   337,   367,   192,   369,   370,
     675,   402,   402,   144,   265,   410,   411,   192,   435,   436,
     437,   504,   524,   525,   526,   475,   509,   519,   520,   521,
     522,   147,   504,   447,   200,   246,   169,   170,   192,   262,
     263,   545,   550,   551,   554,   557,   558,   263,   550,   105,
     563,   564,   568,     3,   155,   657,   723,   407,   644,   651,
     504,   535,   504,   207,   207,   207,   461,   597,   194,    58,
      65,    67,    68,   504,   594,   630,   631,   207,    58,    65,
      67,    68,   594,   627,   628,   207,   478,   232,   727,   207,
      48,   207,   663,   664,   246,   504,   194,    21,    50,    58,
      60,    61,    62,    63,    65,    67,    68,   192,   437,   441,
     593,   594,   595,   614,   615,   616,   617,   614,   617,   690,
     261,   516,   527,   528,   530,   533,   534,   137,   194,   582,
     132,   574,   579,   540,   195,   687,   207,   717,   711,   700,
     407,   706,   407,   713,     5,   195,   264,     5,     5,     3,
     195,   327,   504,   151,     3,   192,   193,   278,   278,   175,
     280,   195,   264,   318,   203,   326,   277,   486,   486,   487,
     488,   492,   496,   497,   498,   195,   683,   192,   397,   398,
     232,   423,   438,   446,   448,   454,   420,   194,   539,     5,
     207,   678,     3,   207,    22,   192,   192,   432,   433,     3,
     207,   194,     3,   372,   429,   375,   378,   232,   276,   277,
     279,   280,   289,   315,   316,   319,   347,   377,   383,   386,
     387,   435,   491,   682,   504,   207,   547,   207,   547,     4,
      21,   155,   232,   456,   457,   459,   504,   507,   207,     3,
     207,   195,   111,   504,   195,     4,     3,   155,   305,   174,
     300,   303,    25,   101,   102,   131,   133,   134,   135,   138,
     139,   141,   142,   338,   348,   192,   341,   342,   344,   155,
       3,   207,     3,   192,   391,   232,   343,   403,   404,   405,
     406,   428,   409,   194,     3,   207,   200,   207,     4,     3,
     195,     3,   195,   437,   523,   207,   195,   450,   207,   547,
       4,   114,    38,   128,   130,   260,   261,   460,   477,   516,
     527,   555,   571,   588,   590,   596,   603,   607,   610,   613,
     626,   629,   632,   684,   549,   545,   558,   260,   194,   106,
     564,   565,   566,   569,   540,   135,   668,   192,   207,   183,
     195,   195,   438,   448,   453,   468,   469,   470,    49,    53,
      54,    55,    56,    57,    58,    65,    66,    67,    68,   594,
     599,   600,    49,    51,    52,    53,    54,    56,    58,    59,
      65,    67,    68,    69,   594,   636,   637,   594,   261,   437,
     444,   445,   437,   442,   443,   602,     3,   195,   594,   261,
     444,   602,     3,   195,   470,   480,     3,   195,   194,   249,
     250,   251,   698,   709,   183,   195,   604,     5,    21,   593,
     617,   192,   615,   261,   261,   261,   444,   602,     3,   195,
     195,     3,   207,   232,   691,    39,   531,   536,   583,   192,
     207,   192,   580,   686,   232,   255,   437,   504,   692,   693,
     694,    21,   192,   703,   718,   719,   720,   207,   718,   183,
     183,   514,   195,   264,   514,   514,     3,   328,   232,   259,
     276,   279,   283,   692,     5,     3,   195,     3,   194,   504,
     504,   681,   232,   416,   417,   438,    22,   194,     3,   430,
     297,   192,   298,   299,   232,   381,   382,   548,   207,   375,
     194,    21,   384,   384,   194,     3,    22,   384,   277,   280,
     195,   207,   207,     4,   504,   261,   504,     5,     3,   195,
       4,   155,   458,   194,   504,   620,   621,   622,   104,   504,
     207,   508,   101,   102,   131,   139,   306,   307,   348,   232,
     309,   310,   192,   207,   194,   194,     3,   334,   194,   349,
     338,   370,   194,     3,   390,   401,     3,   207,   192,   412,
     413,   411,   192,   524,   476,   509,   475,   521,   475,   522,
     147,   207,   207,   170,   192,   207,   194,   192,   193,   194,
     725,   194,   111,   263,   559,   549,   504,   192,   207,   107,
     564,   570,   540,   155,     3,   669,   115,   646,   207,   534,
     577,     3,   463,   194,   471,   505,   506,   601,   505,   505,
     505,   509,     5,   594,   261,   505,   444,   602,     3,   195,
     601,     5,   444,   442,   442,   444,   594,   437,   439,   440,
     261,   444,   602,   439,     3,   195,   631,   207,   628,   207,
       3,   481,   232,   504,   251,    30,   666,   254,   260,   261,
      57,    58,    65,    67,    68,   594,   605,   606,   509,   616,
     611,   620,   194,   437,   608,   618,   619,   622,   618,   535,
      40,    41,   537,   538,   529,     4,   504,   584,   585,   586,
     207,   581,   207,   207,     5,     3,   195,   195,     3,   195,
      31,   708,    29,   715,   195,     3,   195,     3,   195,   195,
     232,   330,   331,   548,   195,   195,   504,   511,   398,   396,
     422,   423,   447,   195,     3,     3,   415,   434,   433,   426,
     429,     3,   195,     5,   194,   232,   256,   257,   258,   259,
     276,   279,   315,   319,   347,   385,   386,   195,   255,   320,
     321,   322,   437,   504,   523,   526,   383,   104,   504,     4,
      21,   504,   457,     4,   504,   504,   504,   621,   623,   624,
       3,   207,   207,   195,   194,     3,   155,   194,   311,     3,
     207,   207,   339,   340,   342,   207,   350,    21,   284,   392,
     192,   207,   207,   405,    20,     3,   195,     4,     4,   523,
     207,   547,   207,   535,    11,    19,    20,    44,   110,   156,
     166,   167,   185,   186,   188,   189,   193,   215,   216,   220,
     222,   225,   227,   232,   233,   234,   504,   727,   504,   254,
     195,   207,   192,   207,    27,   192,   670,   671,   655,   192,
     647,   207,    97,   146,   464,   465,   469,   195,   472,   473,
     474,   475,   505,   600,   598,   442,   195,   637,   633,    97,
     146,   470,   482,   483,   195,   195,   192,   667,   111,     3,
       5,   594,   261,   444,   602,     3,   195,   207,   612,   207,
       3,   609,   207,   195,   192,   207,   192,   207,   529,   586,
       3,   195,     4,   207,   232,   694,   693,   688,   720,    26,
     704,   705,   647,   647,    94,    73,   195,     5,   207,     3,
     195,   195,   504,   417,   207,   429,     4,   351,   352,   353,
     354,   355,   356,   357,   358,   359,   360,   361,   362,   363,
     364,   509,   510,   207,   428,   299,   207,   514,   232,   276,
       5,     3,   195,   207,   504,   261,   504,     4,     3,   621,
     104,   308,   307,     4,   312,   352,   354,   358,   284,   310,
     351,   158,   159,   160,   365,   351,   288,     5,   147,   345,
     346,   351,   194,   192,   413,   207,   207,   195,   504,   195,
     195,   504,   200,   446,   448,   194,   194,   195,   192,   185,
     235,   446,     5,     6,     7,     8,     9,    10,    12,    13,
      14,    15,    16,    17,    19,    20,    21,    22,    23,   196,
     197,   217,   195,   195,   104,   207,     4,   147,     3,   207,
     645,   442,   467,     5,     3,   462,     3,   195,   476,     4,
     207,   620,   207,   467,     5,     3,   479,   576,   665,   504,
     261,   505,   606,   207,   207,   619,   207,   192,   207,   207,
     207,   585,   586,   207,   194,   701,   714,   714,     5,     5,
     509,   423,     3,    22,   195,     3,     4,     3,     3,   354,
     362,   510,    21,     3,   431,     3,   322,   321,   504,   548,
     624,   625,   207,   312,   195,     3,     4,     5,   147,   313,
     314,   195,   195,   195,   511,   315,   347,   435,   195,   393,
     726,   223,   224,   226,     5,   219,   504,   504,   504,   504,
     504,   504,   504,   504,   504,   504,   504,   504,   504,   504,
     504,     5,    22,   218,   504,   504,     5,   504,     5,   504,
     192,   207,   192,   658,   671,   672,   673,   674,   675,   192,
     671,   207,   444,   466,   465,   207,   473,   634,   466,   483,
     207,   207,   207,   195,     3,   207,   192,   207,   707,   207,
     514,   264,     3,   504,   192,   353,   355,   510,     4,   354,
     357,   359,     4,    21,   364,   432,   514,     5,   195,   195,
     511,   315,   347,   351,   220,   232,   221,   228,   228,   228,
     504,   504,   504,   504,   504,   207,   656,     3,   207,   261,
     195,   207,   195,   195,   509,    22,     4,     3,   195,   509,
     195,   194,     4,   229,   230,   231,   504,   195,   195,   195,
     207,   673,   207,     3,   416,   514,     3,     4,   504,     3,
       4,   446,   509,   195,   509,   504,     4,   230,   504,     3,
     504,     4,   509,   504
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   204,   205,   205,   206,   206,   206,   207,   207,   207,
     207,   207,   208,   208,   208,   209,   209,   209,   210,   211,
     211,   211,   212,   212,   213,   214,   214,   215,   215,   215,
     215,   215,   216,   216,   217,   217,   217,   217,   217,   217,
     217,   217,   217,   217,   217,   217,   217,   217,   217,   217,
     217,   217,   217,   217,   217,   218,   218,   218,   218,   219,
     219,   220,   220,   220,   221,   222,   222,   223,   222,   224,
     222,   225,   226,   225,   227,   228,   228,   229,   229,   230,
     230,   231,   231,   231,   231,   231,   231,   231,   232,   233,
     233,   233,   233,   233,   233,   233,   233,   234,   234,   234,
     234,   235,   235,   236,   236,   237,   238,   238,   239,   239,
     240,   240,   241,   241,   241,   241,   241,   241,   241,   241,
     241,   241,   241,   241,   241,   242,   242,   243,   244,   244,
     245,   245,   246,   246,   247,   247,   248,   249,   249,   250,
     250,   251,   251,   252,   252,   252,   252,   252,   252,   252,
     252,   252,   253,   253,   253,   253,   253,   254,   254,   254,
     254,   254,   254,   254,   254,   254,   254,   254,   254,   254,
     254,   254,   254,   254,   254,   254,   254,   254,   254,   254,
     254,   254,   255,   256,   257,   257,   258,   258,   258,   258,
     258,   259,   260,   260,   261,   261,   262,   262,   263,   264,
     264,   264,   266,   265,   265,   265,   268,   267,   269,   267,
     270,   267,   271,   267,   272,   267,   273,   267,   274,   274,
     275,   275,   275,   276,   276,   277,   277,   278,   278,   279,
     279,   280,   280,   281,   282,   282,   282,   283,   283,   283,
     284,   284,   285,   285,   286,   286,   286,   286,   286,   287,
     287,   287,   287,   288,   288,   289,   289,   289,   290,   290,
     292,   291,   293,   293,   294,   294,   295,   295,   296,   296,
     297,   298,   298,   299,   300,   300,   301,   301,   302,   302,
     303,   304,   305,   305,   305,   306,   306,   307,   307,   307,
     308,   307,   307,   309,   309,   310,   311,   311,   312,   312,
     313,   313,   314,   314,   314,   315,   316,   316,   317,   317,
     318,   318,   319,   319,   320,   320,   321,   321,   322,   322,
     322,   323,   323,   324,   325,   326,   327,   327,   328,   328,
     329,   330,   330,   331,   333,   334,   332,   335,   335,   336,
     336,   337,   337,   338,   338,   338,   339,   338,   338,   340,
     338,   338,   338,   338,   338,   338,   338,   341,   341,   342,
     343,   344,   345,   345,   346,   346,   346,   347,   348,   348,
     349,   350,   349,   351,   351,   351,   351,   351,   352,   352,
     353,   353,   354,   355,   356,   356,   357,   357,   358,   358,
     359,   360,   361,   361,   362,   362,   363,   363,   364,   365,
     365,   365,   367,   366,   368,   368,   369,   369,   370,   370,
     372,   371,   373,   373,   374,   374,   375,   376,   376,   377,
     377,   378,   378,   379,   379,   380,   380,   381,   381,   381,
     382,   383,   383,   383,   383,   383,   383,   383,   383,   384,
     384,   385,   385,   385,   385,   385,   385,   385,   386,   387,
     389,   390,   388,   392,   391,   393,   391,   395,   396,   394,
     397,   397,   398,   400,   401,   399,   402,   402,   403,   403,
     404,   404,   405,   405,   405,   406,   407,   408,   409,   408,
     410,   410,   411,   412,   412,   413,   413,   414,   415,   415,
     416,   416,   417,   418,   419,   419,   421,   420,   422,   422,
     423,   423,   423,   425,   426,   424,   427,   427,   428,   428,
     429,   429,   430,   431,   430,   432,   432,   433,   434,   433,
     435,   435,   435,   435,   436,   437,   438,   439,   440,   441,
     442,   443,   444,   445,   446,   446,   446,   447,   448,   449,
     449,   450,   451,   450,   452,   453,   454,   455,   455,   456,
     456,   456,   457,   457,   457,   457,   457,   457,   457,   457,
     457,   457,   458,   458,   458,   458,   458,   458,   459,   461,
     462,   460,   463,   463,   464,   464,   465,   465,   466,   467,
     468,   468,   469,   470,   470,   471,   471,   472,   472,   473,
     474,   474,   475,   476,   478,   479,   477,   480,   480,   481,
     481,   482,   482,   483,   483,   484,   484,   484,   484,   484,
     485,   486,   486,   487,   487,   488,   488,   488,   488,   488,
     489,   490,   490,   491,   491,   492,   492,   493,   494,   494,
     495,   495,   495,   495,   495,   495,   495,   495,   495,   495,
     495,   495,   496,   496,   497,   497,   498,   498,   499,   499,
     500,   501,   502,   503,   503,   504,   505,   506,   507,   508,
     508,   509,   510,   511,   512,   513,   514,   515,   516,   516,
     517,   517,   517,   518,   518,   519,   519,   520,   520,   521,
     522,   523,   524,   525,   526,   526,   526,   527,   528,   529,
     529,   530,   530,   531,   531,   532,   533,   533,   533,   534,
     535,   536,   536,   537,   537,   538,   538,   539,   540,   541,
     541,   542,   542,   542,   543,   543,   544,   544,   544,   544,
     545,   545,   545,   545,   546,   546,   546,   546,   547,   547,
     547,   547,   548,   549,   550,   550,   551,   551,   552,   552,
     553,   554,   555,   555,   555,   555,   555,   555,   555,   555,
     555,   555,   555,   555,   555,   555,   555,   555,   555,   556,
     556,   557,   557,   558,   559,   560,   560,   561,   562,   563,
     563,   563,   564,   565,   565,   565,   566,   567,   567,   567,
     568,   568,   569,   569,   570,   570,   571,   572,   573,   573,
     573,   574,   576,   575,   577,   575,   578,   578,   580,   579,
     581,   579,   583,   582,   582,   584,   584,   585,   585,   585,
     585,   586,   587,   587,   588,   589,   590,   591,   591,   592,
     592,   593,   593,   593,   594,   595,   597,   598,   596,   599,
     599,   600,   600,   600,   600,   600,   600,   600,   600,   600,
     600,   600,   600,   601,   602,   604,   603,   605,   605,   606,
     606,   606,   606,   606,   606,   608,   607,   609,   607,   607,
     607,   611,   610,   612,   610,   613,   613,   614,   614,   615,
     616,   616,   616,   616,   616,   616,   616,   616,   616,   616,
     616,   616,   617,   617,   617,   618,   618,   619,   619,   620,
     620,   621,   621,   622,   623,   623,   624,   625,   625,   626,
     626,   627,   627,   628,   628,   628,   628,   628,   629,   629,
     630,   630,   631,   631,   631,   631,   631,   633,   632,   634,
     632,   635,   636,   636,   637,   637,   637,   637,   637,   637,
     637,   637,   637,   637,   637,   637,   638,   640,   639,   641,
     641,   643,   642,   645,   644,   646,   646,   647,   647,   648,
     649,   649,   650,   650,   651,   651,   652,   652,   653,   655,
     654,   656,   654,   657,   657,   657,   658,   658,   659,   660,
     660,   242,   242,   662,   661,   664,   665,   663,   666,   666,
     667,   667,   668,   669,   669,   670,   670,   671,   672,   672,
     673,   673,   673,   674,   675,   676,   676,   677,   677,   678,
     679,   680,   680,   681,   682,   683,   682,   685,   684,   686,
     684,   687,   688,   684,   690,   689,   691,   691,   691,   692,
     692,   693,   693,   694,   694,   694,   695,   695,   696,   696,
     697,   697,   697,   698,   700,   701,   699,   702,   703,   704,
     704,   705,   707,   706,   708,   708,   709,   711,   710,   712,
     713,   714,   715,   715,   716,   717,   716,   718,   718,   719,
     719,   720,   720,   721,   721,   723,   722,   724,   724,   725,
     725,   725,   725,   725,   726,   727,   727
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     2,     1,     1,     1,     2,     1,     1,
       3,     2,     1,     3,     3,     1,     3,     1,     0,     1,
       1,     1,     1,     1,     1,     0,     1,     1,     1,     2,
       2,     2,     1,     1,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     3,     3,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     0,     1,     2,     2,     2,
       1,     1,     1,     1,     0,     1,     2,     0,     5,     0,
       6,     1,     0,     5,     4,     1,     2,     1,     3,     1,
       1,     3,     5,     4,     3,     2,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     1,     2,     1,
       1,     0,     1,     0,     1,     2,     0,     1,     0,     1,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     1,     2,     0,     1,
       1,     2,     1,     1,     0,     1,     3,     0,     1,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     4,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     1,     1,     1,     0,     1,     1,     1,
       1,     1,     0,     2,     3,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     1,
       3,     5,     2,     1,     2,     1,     3,     1,     1,     1,
       2,     1,     3,     5,     1,     1,     1,     1,     1,     1,
       0,     2,     0,     1,     1,     9,     5,     5,     9,     3,
       5,     2,     3,     3,     1,     1,     1,     1,     1,     1,
       0,     4,     4,     7,     0,     2,     0,     2,     1,     3,
       1,     1,     3,     1,     2,     3,     0,     1,     1,     2,
       1,     4,     0,     1,     3,     1,     3,     1,     1,     1,
       0,     5,     1,     1,     3,     4,     0,     3,     1,     1,
       0,     1,     2,     2,     2,     1,     1,     4,     1,     3,
       1,     3,     3,     4,     1,     3,     1,     3,     1,     1,
       1,     3,     3,     1,     1,     1,     1,     3,     1,     1,
       5,     5,     7,     1,     0,     0,     6,     0,     2,     0,
       1,     2,     3,     1,     1,     1,     0,     5,     1,     0,
       5,     1,     1,     1,     1,     1,     1,     1,     3,     4,
       1,     1,     0,     1,     2,     2,     2,     1,     1,     1,
       0,     0,     4,     1,     1,     1,     1,     1,     1,     3,
       3,     1,     1,     1,     1,     3,     1,     2,     1,     3,
       1,     3,     0,     2,     0,     2,     1,     3,     2,     1,
       1,     1,     0,     4,     0,     2,     1,     3,     1,     1,
       0,     5,     0,     1,     2,     3,     4,     1,     3,     1,
       3,     1,     1,     9,    11,     1,     3,     1,     1,     1,
       1,     2,     2,     2,     1,     1,     1,     1,     1,     0,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       0,     0,     6,     0,     5,     0,     7,     0,     0,     7,
       1,     3,     3,     0,     0,     6,     0,     1,     0,     1,
       1,     3,     1,     1,     1,     1,     0,     4,     0,     5,
       1,     3,     4,     1,     3,     1,     3,     7,     0,     6,
       1,     3,     1,     3,     1,     3,     0,     6,     1,     3,
       1,     1,     1,     0,     0,     7,     0,     1,     1,     3,
       0,     1,     0,     0,     5,     1,     3,     1,     0,     5,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     4,     4,     3,     2,     0,
       3,     1,     0,     5,     1,     1,     1,     1,     4,     0,
       1,     3,     2,     1,     2,     3,     4,     2,     1,     3,
       4,     2,     1,     2,     3,     4,     2,     0,     1,     0,
       0,     8,     0,     2,     1,     3,     2,     3,     1,     1,
       1,     3,     2,     1,     1,     0,     3,     1,     3,     2,
       0,     2,     1,     1,     0,     0,     8,     1,     3,     0,
       2,     1,     3,     2,     3,     1,     1,     1,     1,     3,
       1,     1,     3,     1,     3,     1,     2,     3,     1,     2,
       1,     1,     1,     1,     1,     1,     3,     1,     1,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     1,     3,     1,     3,     1,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       1,     1,     1,     1,     1,     1,     1,     1,     4,     5,
       5,     7,     4,     0,     3,     1,     3,     1,     3,     2,
       3,     1,     1,     3,     1,     1,     1,     5,     5,     0,
       2,     0,     3,     0,     3,     5,     1,     1,     1,     1,
       1,     4,     5,     2,     3,     2,     3,     0,     1,     0,
       2,     1,     1,     1,     3,     3,     4,     2,     5,     3,
       4,     2,     5,     3,     4,     2,     5,     3,     6,     8,
       5,     3,     1,     1,     1,     2,     3,     4,     1,     1,
       3,     2,     1,     1,     1,     1,     1,     1,     1,     2,
       4,     1,     1,     1,     1,     1,     1,     1,     1,     4,
       3,     2,     3,     3,     2,     0,     1,     3,     5,     0,
       1,     2,     2,     0,     1,     2,     2,     7,     8,     6,
       6,     7,     2,     3,     2,     3,     5,     3,     0,     1,
       2,     2,     0,     8,     0,     6,     3,     4,     0,     3,
       0,     4,     0,     4,     1,     1,     3,     1,     2,     2,
       3,     1,     2,     3,     3,    10,     3,     2,     3,     1,
       1,     1,     1,     1,     1,     1,     0,     0,     7,     1,
       3,     1,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     3,     1,     1,     0,     7,     1,     3,     1,
       2,     2,     2,     2,     3,     0,     6,     0,     7,     4,
       6,     0,     6,     0,     7,     4,     6,     1,     3,     1,
       1,     2,     1,     1,     2,     2,     2,     2,     2,     2,
       2,     3,     1,     1,     1,     1,     3,     1,     1,     1,
       3,     1,     1,     5,     1,     3,     1,     5,     7,     3,
       5,     1,     3,     1,     2,     2,     2,     2,     3,     5,
       1,     3,     1,     2,     2,     2,     2,     0,     7,     0,
       9,     0,     1,     3,     1,     2,     2,     2,     2,     2,
       2,     2,     2,     3,     2,     2,     2,     0,     5,     0,
       1,     0,     4,     0,     6,     0,     1,     0,     1,     2,
       0,     1,     1,     2,     1,     1,     1,     2,     0,     0,
       8,     0,    11,     0,     1,     3,     0,     1,     5,     0,
       1,     0,     1,     0,     4,     0,     0,     6,     0,     1,
       0,     1,     1,     0,     2,     1,     3,     3,     1,     3,
       1,     1,     1,     1,     1,     3,     4,     1,     3,     1,
       4,     1,     3,     1,     3,     0,     5,     0,     3,     0,
       5,     0,     0,     7,     0,     4,     1,     1,     1,     1,
       3,     1,     3,     1,     1,     1,     0,     1,     1,     2,
       1,     1,     1,     5,     0,     0,    10,     1,     1,     0,
       1,     4,     0,     7,     0,     1,     5,     0,     6,     1,
       6,     0,     0,     1,     0,     0,     4,     0,     1,     1,
       3,     1,     1,     3,     4,     0,     4,     1,     1,     3,
       3,     1,     3,     1,     0,     1,     3
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yytype], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyo, yytype, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[+yyssp[yyi + 1 - yynrhs]],
                       &yyvsp[(yyi + 1) - (yynrhs)]
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
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
#ifndef YYINITDEPTH
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
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
#  else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
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
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
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
            else
              goto append;

          append:
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

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                yy_state_t *yyssp, int yytoken)
{
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Actual size of YYARG. */
  int yycount = 0;
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[+*yyssp];
      YYPTRDIFF_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
      yysize = yysize0;
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYPTRDIFF_T yysize1
                    = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
                    yysize = yysize1;
                  else
                    return 2;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    /* Don't count the "%s"s in the final size, but reserve room for
       the terminator.  */
    YYPTRDIFF_T yysize1 = yysize + (yystrlen (yyformat) - 2 * yycount) + 1;
    if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
      yysize = yysize1;
    else
      return 2;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss;
    yy_state_t *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYPTRDIFF_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
# undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
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
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
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
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 6:
#line 520 "fortran.y"
              {yyerrok;yyclearin;}
#line 3681 "fortran.tab.c"
    break;

  case 7:
#line 523 "fortran.y"
      {token_since_endofstmt = 0; increment_nbtokens = 0;}
#line 3687 "fortran.tab.c"
    break;

  case 16:
#line 536 "fortran.y"
        {
            if (inmoduledeclare == 0 )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curinclude,pos_end-pos_curinclude);
            }
        }
#line 3699 "fortran.tab.c"
    break;

  case 18:
#line 561 "fortran.y"
          { pos_cur = setposcur(); }
#line 3705 "fortran.tab.c"
    break;

  case 24:
#line 585 "fortran.y"
                                  { Add_Include_1((yyvsp[0].na)); }
#line 3711 "fortran.tab.c"
    break;

  case 27:
#line 1107 "fortran.y"
                                { strcpy((yyval.na),(yyvsp[0].na)); }
#line 3717 "fortran.tab.c"
    break;

  case 28:
#line 1108 "fortran.y"
                                { strcpy((yyval.na),(yyvsp[0].na)); }
#line 3723 "fortran.tab.c"
    break;

  case 29:
#line 1109 "fortran.y"
                                { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3729 "fortran.tab.c"
    break;

  case 30:
#line 1110 "fortran.y"
                                { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3735 "fortran.tab.c"
    break;

  case 31:
#line 1111 "fortran.y"
                                { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3741 "fortran.tab.c"
    break;

  case 32:
#line 1113 "fortran.y"
                   { strcpy((yyval.na),"+"); }
#line 3747 "fortran.tab.c"
    break;

  case 33:
#line 1114 "fortran.y"
                   { strcpy((yyval.na),"-"); }
#line 3753 "fortran.tab.c"
    break;

  case 34:
#line 1118 "fortran.y"
                                    { sprintf((yyval.na),"+%s",(yyvsp[0].na)); }
#line 3759 "fortran.tab.c"
    break;

  case 35:
#line 1119 "fortran.y"
                                    { sprintf((yyval.na),"-%s",(yyvsp[0].na)); }
#line 3765 "fortran.tab.c"
    break;

  case 36:
#line 1120 "fortran.y"
                                    { sprintf((yyval.na),"*%s",(yyvsp[0].na)); }
#line 3771 "fortran.tab.c"
    break;

  case 37:
#line 1121 "fortran.y"
                                    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3777 "fortran.tab.c"
    break;

  case 38:
#line 1122 "fortran.y"
                                    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3783 "fortran.tab.c"
    break;

  case 39:
#line 1123 "fortran.y"
                                    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3789 "fortran.tab.c"
    break;

  case 40:
#line 1124 "fortran.y"
                                    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3795 "fortran.tab.c"
    break;

  case 41:
#line 1125 "fortran.y"
                                    { sprintf((yyval.na)," > %s",(yyvsp[0].na)); }
#line 3801 "fortran.tab.c"
    break;

  case 42:
#line 1126 "fortran.y"
                                    { sprintf((yyval.na)," < %s",(yyvsp[0].na)); }
#line 3807 "fortran.tab.c"
    break;

  case 43:
#line 1127 "fortran.y"
                                    { sprintf((yyval.na)," >= %s",(yyvsp[0].na)); }
#line 3813 "fortran.tab.c"
    break;

  case 44:
#line 1128 "fortran.y"
                                    { sprintf((yyval.na)," <= %s",(yyvsp[0].na)); }
#line 3819 "fortran.tab.c"
    break;

  case 45:
#line 1129 "fortran.y"
                                    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3825 "fortran.tab.c"
    break;

  case 46:
#line 1130 "fortran.y"
                                    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3831 "fortran.tab.c"
    break;

  case 47:
#line 1131 "fortran.y"
                                    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3837 "fortran.tab.c"
    break;

  case 48:
#line 1132 "fortran.y"
                                    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3843 "fortran.tab.c"
    break;

  case 49:
#line 1133 "fortran.y"
                                    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3849 "fortran.tab.c"
    break;

  case 50:
#line 1134 "fortran.y"
                                    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3855 "fortran.tab.c"
    break;

  case 51:
#line 1135 "fortran.y"
                                    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3861 "fortran.tab.c"
    break;

  case 52:
#line 1136 "fortran.y"
                                    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3867 "fortran.tab.c"
    break;

  case 53:
#line 1137 "fortran.y"
                                    { sprintf((yyval.na),"%s",(yyvsp[0].na)); }
#line 3873 "fortran.tab.c"
    break;

  case 54:
#line 1138 "fortran.y"
                                    { sprintf((yyval.na),"%s",(yyvsp[0].na)); }
#line 3879 "fortran.tab.c"
    break;

  case 55:
#line 1140 "fortran.y"
                                { strcpy((yyval.na),""); }
#line 3885 "fortran.tab.c"
    break;

  case 56:
#line 1141 "fortran.y"
                                { sprintf((yyval.na),"/%s",(yyvsp[0].na)); }
#line 3891 "fortran.tab.c"
    break;

  case 57:
#line 1142 "fortran.y"
                                { sprintf((yyval.na),"/= %s",(yyvsp[0].na));}
#line 3897 "fortran.tab.c"
    break;

  case 58:
#line 1143 "fortran.y"
                                { sprintf((yyval.na),"//%s",(yyvsp[0].na)); }
#line 3903 "fortran.tab.c"
    break;

  case 59:
#line 1146 "fortran.y"
                                { sprintf((yyval.na),"==%s",(yyvsp[0].na)); }
#line 3909 "fortran.tab.c"
    break;

  case 60:
#line 1147 "fortran.y"
                                { sprintf((yyval.na),"= %s",(yyvsp[0].na)); }
#line 3915 "fortran.tab.c"
    break;

  case 61:
#line 1150 "fortran.y"
                                        { strcpy((yyval.na),(yyvsp[0].na)); }
#line 3921 "fortran.tab.c"
    break;

  case 62:
#line 1151 "fortran.y"
                                        { strcpy((yyval.na),(yyvsp[0].na)); }
#line 3927 "fortran.tab.c"
    break;

  case 63:
#line 1152 "fortran.y"
                                        { strcpy((yyval.na),(yyvsp[0].na)); }
#line 3933 "fortran.tab.c"
    break;

  case 64:
#line 1156 "fortran.y"
        {
            agrif_parentcall = 0;
            if ( !strcasecmp(identcopy, "Agrif_Parent") )   agrif_parentcall = 1;
            if ( Agrif_in_Tok_NAME(identcopy) )
            {
                inagrifcallargument = 1;
                Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
            }
        }
#line 3947 "fortran.tab.c"
    break;

  case 65:
#line 1167 "fortran.y"
                                                            { strcpy((yyval.na),(yyvsp[0].na)); if ( incalldeclare == 0 ) inagrifcallargument = 0;   }
#line 3953 "fortran.tab.c"
    break;

  case 66:
#line 1168 "fortran.y"
                                                            { sprintf((yyval.na)," %s %s ",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3959 "fortran.tab.c"
    break;

  case 67:
#line 1169 "fortran.y"
                                {in_complex_literal=0;}
#line 3965 "fortran.tab.c"
    break;

  case 68:
#line 1169 "fortran.y"
                                                                                    { sprintf((yyval.na)," %s ( %s )",(yyvsp[-4].na),(yyvsp[-1].na)); }
#line 3971 "fortran.tab.c"
    break;

  case 69:
#line 1170 "fortran.y"
                                {in_complex_literal=0;}
#line 3977 "fortran.tab.c"
    break;

  case 70:
#line 1170 "fortran.y"
                                                                                    { sprintf((yyval.na)," %s ( %s ) %s ",(yyvsp[-5].na),(yyvsp[-2].na),(yyvsp[0].na)); }
#line 3983 "fortran.tab.c"
    break;

  case 72:
#line 1173 "fortran.y"
                   {in_complex_literal=0;}
#line 3989 "fortran.tab.c"
    break;

  case 73:
#line 1174 "fortran.y"
        {
            if ( inside_type_declare ) break;
            sprintf((yyval.na)," %s ( %s )",(yyvsp[-4].na),(yyvsp[-1].na));
            ModifyTheAgrifFunction_0((yyvsp[-1].na));
            agrif_parentcall = 0;
        }
#line 4000 "fortran.tab.c"
    break;

  case 74:
#line 1183 "fortran.y"
        {
            sprintf((yyval.na)," %s %% %s ",(yyvsp[-3].na),(yyvsp[0].na));
            if ( incalldeclare == 0 ) inagrifcallargument = 0;
        }
#line 4009 "fortran.tab.c"
    break;

  case 75:
#line 1194 "fortran.y"
                                    { strcpy((yyval.na)," "); }
#line 4015 "fortran.tab.c"
    break;

  case 76:
#line 1195 "fortran.y"
                                    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 4021 "fortran.tab.c"
    break;

  case 77:
#line 1198 "fortran.y"
                            {  strcpy((yyval.na),(yyvsp[0].na)); }
#line 4027 "fortran.tab.c"
    break;

  case 78:
#line 1199 "fortran.y"
                            {  sprintf((yyval.na),"%s,%s",(yyvsp[-2].na),(yyvsp[0].na)); }
#line 4033 "fortran.tab.c"
    break;

  case 79:
#line 1202 "fortran.y"
                   {strcpy((yyval.na),(yyvsp[0].na));}
#line 4039 "fortran.tab.c"
    break;

  case 80:
#line 1203 "fortran.y"
                   {strcpy((yyval.na),(yyvsp[0].na));}
#line 4045 "fortran.tab.c"
    break;

  case 81:
#line 1206 "fortran.y"
                                {  sprintf((yyval.na),"%s :%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 4051 "fortran.tab.c"
    break;

  case 82:
#line 1207 "fortran.y"
                                {  sprintf((yyval.na),"%s :%s :%s",(yyvsp[-4].na),(yyvsp[-2].na),(yyvsp[0].na));}
#line 4057 "fortran.tab.c"
    break;

  case 83:
#line 1208 "fortran.y"
                                {  sprintf((yyval.na),":%s :%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 4063 "fortran.tab.c"
    break;

  case 84:
#line 1209 "fortran.y"
                                {  sprintf((yyval.na),": : %s",(yyvsp[0].na));}
#line 4069 "fortran.tab.c"
    break;

  case 85:
#line 1210 "fortran.y"
                                {  sprintf((yyval.na),":%s",(yyvsp[0].na));}
#line 4075 "fortran.tab.c"
    break;

  case 86:
#line 1211 "fortran.y"
                                {  sprintf((yyval.na),"%s :",(yyvsp[-1].na));}
#line 4081 "fortran.tab.c"
    break;

  case 87:
#line 1212 "fortran.y"
                                {  sprintf((yyval.na),":");}
#line 4087 "fortran.tab.c"
    break;

  case 88:
#line 1215 "fortran.y"
        {
       //  if (indeclaration == 1) break;
            if ( afterpercent == 0 )
            {
                if ( Agrif_in_Tok_NAME((yyvsp[0].na)) ) Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
                if ( !strcasecmp((yyvsp[0].na),"Agrif_Parent") )   agrif_parentcall = 1;
                if ( VariableIsFunction((yyvsp[0].na)) )
                {
                    if ( inagrifcallargument == 1 )
                    {
                        if ( !strcasecmp((yyvsp[0].na),identcopy) )
                        {
                            strcpy(sameagrifname,identcopy);
                            sameagrifargument = 1;
                        }
                    }
                    strcpy(identcopy,(yyvsp[0].na));
                    pointedvar = 0;

                    if (variscoupled_0((yyvsp[0].na))) strcpy(truename, getcoupledname_0((yyvsp[0].na)));
                    else                    strcpy(truename, (yyvsp[0].na));

                    if ( VarIsNonGridDepend(truename) == 0 && (! Variableshouldberemoved(truename)) )
                    {
                        if ( inagrifcallargument == 1 || varispointer_0(truename) == 1 )
                        {
                            if ( (IsinListe(List_UsedInSubroutine_Var,(yyvsp[0].na)) == 1) || (inagrifcallargument == 1) )
                            {
                                if (varistyped_0(truename) == 0)    ModifyTheVariableName_0(truename,strlen((yyvsp[0].na)));
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
#line 4136 "fortran.tab.c"
    break;

  case 89:
#line 1261 "fortran.y"
                     { strcpy((yyval.na),".TRUE.");}
#line 4142 "fortran.tab.c"
    break;

  case 90:
#line 1262 "fortran.y"
                     { strcpy((yyval.na),".FALSE.");}
#line 4148 "fortran.tab.c"
    break;

  case 91:
#line 1263 "fortran.y"
                     { strcpy((yyval.na),"NULL()"); }
#line 4154 "fortran.tab.c"
    break;

  case 92:
#line 1264 "fortran.y"
                     { strcpy((yyval.na),(yyvsp[0].na)); }
#line 4160 "fortran.tab.c"
    break;

  case 93:
#line 1265 "fortran.y"
                     { strcpy((yyval.na),(yyvsp[0].na)); }
#line 4166 "fortran.tab.c"
    break;

  case 94:
#line 1266 "fortran.y"
                     { strcpy((yyval.na),(yyvsp[0].na)); }
#line 4172 "fortran.tab.c"
    break;

  case 95:
#line 1268 "fortran.y"
                     { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 4178 "fortran.tab.c"
    break;

  case 97:
#line 1272 "fortran.y"
                                            { strcpy((yyval.na),(yyvsp[0].na));}
#line 4184 "fortran.tab.c"
    break;

  case 99:
#line 1274 "fortran.y"
                                            { strcpy((yyval.na),(yyvsp[0].na));}
#line 4190 "fortran.tab.c"
    break;

  case 100:
#line 1275 "fortran.y"
                                            { strcpy((yyval.na),(yyvsp[0].na));}
#line 4196 "fortran.tab.c"
    break;

  case 101:
#line 1277 "fortran.y"
                    { strcpy((yyval.na)," ");}
#line 4202 "fortran.tab.c"
    break;

  case 102:
#line 1278 "fortran.y"
                    { strcpy((yyval.na),(yyvsp[0].na));}
#line 4208 "fortran.tab.c"
    break;

  case 103:
#line 1288 "fortran.y"
                    { strcpy((yyval.na)," ");}
#line 4214 "fortran.tab.c"
    break;

  case 104:
#line 1289 "fortran.y"
                    { strcpy((yyval.na),(yyvsp[0].na));}
#line 4220 "fortran.tab.c"
    break;

  case 169:
#line 1487 "fortran.y"
        {
            /* if we never meet the contains keyword               */
            if ( firstpass == 0 )
            {
                RemoveWordCUR_0(fortran_out, strlen((yyvsp[0].na))+11);    // Remove word "end module"
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
#line 4246 "fortran.tab.c"
    break;

  case 189:
#line 1540 "fortran.y"
     {in_complex_literal=0;}
#line 4252 "fortran.tab.c"
    break;

  case 192:
#line 1564 "fortran.y"
     {strcpy((yyval.na),"");}
#line 4258 "fortran.tab.c"
    break;

  case 196:
#line 1574 "fortran.y"
     {strcpy((yyval.na),"");}
#line 4264 "fortran.tab.c"
    break;

  case 197:
#line 1576 "fortran.y"
     {strcpy((yyval.na),(yyvsp[0].na));}
#line 4270 "fortran.tab.c"
    break;

  case 202:
#line 1596 "fortran.y"
                       {pos_cur_decl=my_position_before;}
#line 4276 "fortran.tab.c"
    break;

  case 203:
#line 1597 "fortran.y"
     {strcpy((yyval.na),(yyvsp[0].na));}
#line 4282 "fortran.tab.c"
    break;

  case 205:
#line 1600 "fortran.y"
     {strcpy(DeclType,"type"); GlobalDeclarationType = 1;  }
#line 4288 "fortran.tab.c"
    break;

  case 206:
#line 1604 "fortran.y"
                                 {in_kind_selector = 1;}
#line 4294 "fortran.tab.c"
    break;

  case 207:
#line 1605 "fortran.y"
     {sprintf((yyval.na),"%s%s",(yyvsp[-2].na),(yyvsp[0].na));strcpy(DeclType,(yyvsp[-2].na)); in_kind_selector =0;}
#line 4300 "fortran.tab.c"
    break;

  case 208:
#line 1606 "fortran.y"
                {in_kind_selector = 1;}
#line 4306 "fortran.tab.c"
    break;

  case 209:
#line 1607 "fortran.y"
     {sprintf((yyval.na),"%s%s",(yyvsp[-2].na),(yyvsp[0].na));strcpy(DeclType,(yyvsp[-2].na));in_kind_selector =0;}
#line 4312 "fortran.tab.c"
    break;

  case 210:
#line 1608 "fortran.y"
                           {in_kind_selector = 1;}
#line 4318 "fortran.tab.c"
    break;

  case 211:
#line 1609 "fortran.y"
     {sprintf((yyval.na),"%s%s",(yyvsp[-2].na),(yyvsp[0].na));strcpy(DeclType,"real"); strcpy(NamePrecision,"8");in_kind_selector =0;}
#line 4324 "fortran.tab.c"
    break;

  case 212:
#line 1610 "fortran.y"
                   {in_kind_selector = 1;}
#line 4330 "fortran.tab.c"
    break;

  case 213:
#line 1611 "fortran.y"
     {sprintf((yyval.na),"%s%s",(yyvsp[-2].na),(yyvsp[0].na));strcpy(DeclType,(yyvsp[-2].na));in_kind_selector =0;}
#line 4336 "fortran.tab.c"
    break;

  case 214:
#line 1612 "fortran.y"
                     {in_char_selector = 1;}
#line 4342 "fortran.tab.c"
    break;

  case 215:
#line 1613 "fortran.y"
     {sprintf((yyval.na),"%s%s",(yyvsp[-2].na),(yyvsp[0].na));strcpy(DeclType,(yyvsp[-2].na));in_char_selector = 0;}
#line 4348 "fortran.tab.c"
    break;

  case 216:
#line 1614 "fortran.y"
                   {in_kind_selector = 1;}
#line 4354 "fortran.tab.c"
    break;

  case 217:
#line 1615 "fortran.y"
     {sprintf((yyval.na),"%s%s",(yyvsp[-2].na),(yyvsp[0].na));strcpy(DeclType,(yyvsp[-2].na));in_kind_selector =0;}
#line 4360 "fortran.tab.c"
    break;

  case 218:
#line 1619 "fortran.y"
     {strcpy((yyval.na),"");strcpy(NamePrecision,"");}
#line 4366 "fortran.tab.c"
    break;

  case 219:
#line 1621 "fortran.y"
     {strcpy((yyval.na),(yyvsp[0].na));}
#line 4372 "fortran.tab.c"
    break;

  case 220:
#line 1627 "fortran.y"
     {sprintf((yyval.na),"(%s)",(yyvsp[-1].na)); strcpy(NamePrecision,(yyvsp[-1].na));}
#line 4378 "fortran.tab.c"
    break;

  case 221:
#line 1629 "fortran.y"
     {sprintf((yyval.na),"(KIND=%s)",(yyvsp[-1].na)); strcpy(NamePrecision,(yyvsp[-1].na));}
#line 4384 "fortran.tab.c"
    break;

  case 222:
#line 1631 "fortran.y"
     {sprintf((yyval.na),"*%s",(yyvsp[0].na));strcpy(NamePrecision,(yyvsp[0].na));}
#line 4390 "fortran.tab.c"
    break;

  case 224:
#line 1639 "fortran.y"
     {sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na));}
#line 4396 "fortran.tab.c"
    break;

  case 226:
#line 1645 "fortran.y"
     {sprintf((yyval.na),"%s_%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 4402 "fortran.tab.c"
    break;

  case 230:
#line 1668 "fortran.y"
     {sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na));}
#line 4408 "fortran.tab.c"
    break;

  case 232:
#line 1674 "fortran.y"
     {sprintf((yyval.na),"%s_%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 4414 "fortran.tab.c"
    break;

  case 233:
#line 1681 "fortran.y"
     {sprintf((yyval.na),"(%s,%s)",(yyvsp[-3].na),(yyvsp[-1].na));}
#line 4420 "fortran.tab.c"
    break;

  case 241:
#line 1699 "fortran.y"
     {char_length_toreset = 1;}
#line 4426 "fortran.tab.c"
    break;

  case 242:
#line 1703 "fortran.y"
     {strcpy((yyval.na),"");}
#line 4432 "fortran.tab.c"
    break;

  case 243:
#line 1705 "fortran.y"
    {strcpy((yyval.na),"");}
#line 4438 "fortran.tab.c"
    break;

  case 249:
#line 1718 "fortran.y"
     {strcpy(CharacterSize,(yyvsp[-1].na));}
#line 4444 "fortran.tab.c"
    break;

  case 250:
#line 1720 "fortran.y"
     {strcpy(CharacterSize,(yyvsp[-1].na));}
#line 4450 "fortran.tab.c"
    break;

  case 253:
#line 1727 "fortran.y"
     {c_star=1; strcpy(CharacterSize,(yyvsp[-1].na));}
#line 4456 "fortran.tab.c"
    break;

  case 254:
#line 1729 "fortran.y"
     {c_selectorgiven = 1; strcpy(c_selectorname,(yyvsp[0].na));}
#line 4462 "fortran.tab.c"
    break;

  case 260:
#line 1744 "fortran.y"
                                    { inside_type_declare = 1;}
#line 4468 "fortran.tab.c"
    break;

  case 261:
#line 1745 "fortran.y"
     { inside_type_declare = 0;}
#line 4474 "fortran.tab.c"
    break;

  case 290:
#line 1811 "fortran.y"
                         {in_complex_literal=0;}
#line 4480 "fortran.tab.c"
    break;

  case 295:
#line 1821 "fortran.y"
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
#line 4506 "fortran.tab.c"
    break;

  case 305:
#line 1865 "fortran.y"
     {strcpy(my_dim.last,"");}
#line 4512 "fortran.tab.c"
    break;

  case 306:
#line 1870 "fortran.y"
     {strcpy(NamePrecision,(yyvsp[0].na));}
#line 4518 "fortran.tab.c"
    break;

  case 321:
#line 1905 "fortran.y"
     { sprintf((yyval.na),"(/%s/)",(yyvsp[-1].na));}
#line 4524 "fortran.tab.c"
    break;

  case 322:
#line 1907 "fortran.y"
     { sprintf((yyval.na),"[%s]",(yyvsp[-1].na)); }
#line 4530 "fortran.tab.c"
    break;

  case 327:
#line 1935 "fortran.y"
      {sprintf((yyval.na),"%s,%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 4536 "fortran.tab.c"
    break;

  case 330:
#line 1945 "fortran.y"
     {sprintf((yyval.na),"(%s,%s)",(yyvsp[-3].na),(yyvsp[-1].na));}
#line 4542 "fortran.tab.c"
    break;

  case 331:
#line 1950 "fortran.y"
     {sprintf((yyval.na),"%s=%s,%s",(yyvsp[-4].na),(yyvsp[-2].na),(yyvsp[0].na));}
#line 4548 "fortran.tab.c"
    break;

  case 332:
#line 1952 "fortran.y"
     {sprintf((yyval.na),"%s=%s,%s,%s",(yyvsp[-6].na),(yyvsp[-4].na),(yyvsp[-2].na),(yyvsp[0].na));}
#line 4554 "fortran.tab.c"
    break;

  case 334:
#line 1960 "fortran.y"
                       {indeclaration=1;}
#line 4560 "fortran.tab.c"
    break;

  case 335:
#line 1961 "fortran.y"
        {
            /* if the variable is a parameter we can suppose that is*/
            /*    value is the same on each grid. It is not useless */
            /*    to create a copy of it on each grid               */
            if ( ! inside_type_declare )
            {
                pos_end = setposcur();
                //printf("POS = %d %d\n",pos_cur_decl,pos_end);
                RemoveWordSET_0(fortran_out,pos_cur_decl,pos_end-pos_cur_decl);
                ReWriteDeclarationAndAddTosubroutine_01((yyvsp[0].l));
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
                    Add_Globliste_1((yyvsp[0].l));
                    if ( insubroutinedeclare )
                    {
                        if ( pointerdeclare ) Add_Pointer_Var_From_List_1((yyvsp[0].l));
                        Add_Parameter_Var_1((yyvsp[0].l));
                    }
                    else
                        Add_GlobalParameter_Var_1((yyvsp[0].l));

                    /* If there's a SAVE declaration in module's subroutines we should    */
                    /*    remove it from the subroutines declaration and add it in the    */
                    /*    global declarations                                             */
                                        
                    if ( aftercontainsdeclare && SaveDeclare )
                    {
                        if ( inmodulemeet ) Add_SubroutineDeclarationSave_Var_1((yyvsp[0].l));
                        else                Add_Save_Var_dcl_1((yyvsp[0].l));
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
#line 4630 "fortran.tab.c"
    break;

  case 344:
#line 2045 "fortran.y"
     { Allocatabledeclare = 1; }
#line 4636 "fortran.tab.c"
    break;

  case 345:
#line 2047 "fortran.y"
     { contiguousdeclare = 1 ; }
#line 4642 "fortran.tab.c"
    break;

  case 346:
#line 2048 "fortran.y"
                         {in_complex_literal=0;}
#line 4648 "fortran.tab.c"
    break;

  case 347:
#line 2049 "fortran.y"
     { dimsgiven = 1; curdim = (yyvsp[-1].d); }
#line 4654 "fortran.tab.c"
    break;

  case 348:
#line 2051 "fortran.y"
     { ExternalDeclare = 1; }
#line 4660 "fortran.tab.c"
    break;

  case 349:
#line 2052 "fortran.y"
                      {in_complex_literal=0;}
#line 4666 "fortran.tab.c"
    break;

  case 350:
#line 2053 "fortran.y"
     { strcpy(IntentSpec,(yyvsp[-1].na)); }
#line 4672 "fortran.tab.c"
    break;

  case 352:
#line 2056 "fortran.y"
     { optionaldeclare = 1 ; }
#line 4678 "fortran.tab.c"
    break;

  case 353:
#line 2058 "fortran.y"
     {VariableIsParameter = 1; }
#line 4684 "fortran.tab.c"
    break;

  case 354:
#line 2060 "fortran.y"
     { pointerdeclare = 1 ; }
#line 4690 "fortran.tab.c"
    break;

  case 355:
#line 2062 "fortran.y"
     { SaveDeclare = 1 ; }
#line 4696 "fortran.tab.c"
    break;

  case 356:
#line 2064 "fortran.y"
     { Targetdeclare = 1; }
#line 4702 "fortran.tab.c"
    break;

  case 357:
#line 2069 "fortran.y"
     {(yyval.l)=insertvar(NULL,(yyvsp[0].v));}
#line 4708 "fortran.tab.c"
    break;

  case 358:
#line 2071 "fortran.y"
     {(yyval.l)=insertvar((yyvsp[-2].l),(yyvsp[0].v));}
#line 4714 "fortran.tab.c"
    break;

  case 359:
#line 2076 "fortran.y"
        {
            if ( ! inside_type_declare )
            {
                if (dimsgiven == 1) curvar = createvar((yyvsp[-3].na),curdim);
                else                curvar = createvar((yyvsp[-3].na),(yyvsp[-2].d));
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
            (yyval.v)=curvar;
        }
#line 4748 "fortran.tab.c"
    break;

  case 362:
#line 2115 "fortran.y"
                    {InitialValueGiven = 0; }
#line 4754 "fortran.tab.c"
    break;

  case 364:
#line 2121 "fortran.y"
        {
            if ( inside_type_declare ) break;
            strcpy(InitValue,(yyvsp[0].na));
            InitialValueGiven = 1;
        }
#line 4764 "fortran.tab.c"
    break;

  case 365:
#line 2127 "fortran.y"
        {
            if ( inside_type_declare ) break;
            strcpy(InitValue,(yyvsp[0].na));
            InitialValueGiven = 2;
        }
#line 4774 "fortran.tab.c"
    break;

  case 366:
#line 2133 "fortran.y"
        {
            if ( inside_type_declare ) break;
            strcpy(InitValue,(yyvsp[0].na));
            InitialValueGiven = 2;
        }
#line 4784 "fortran.tab.c"
    break;

  case 368:
#line 2146 "fortran.y"
     {PublicDeclare = 1;  }
#line 4790 "fortran.tab.c"
    break;

  case 369:
#line 2148 "fortran.y"
     {PrivateDeclare = 1;  }
#line 4796 "fortran.tab.c"
    break;

  case 370:
#line 2152 "fortran.y"
     {(yyval.d)=NULL;}
#line 4802 "fortran.tab.c"
    break;

  case 371:
#line 2153 "fortran.y"
           {in_complex_literal=0;}
#line 4808 "fortran.tab.c"
    break;

  case 372:
#line 2154 "fortran.y"
     {(yyval.d)=(yyvsp[-1].d);}
#line 4814 "fortran.tab.c"
    break;

  case 373:
#line 2159 "fortran.y"
     {(yyval.d)=(yyvsp[0].d);}
#line 4820 "fortran.tab.c"
    break;

  case 374:
#line 2161 "fortran.y"
     {(yyval.d)=(yyvsp[0].d);}
#line 4826 "fortran.tab.c"
    break;

  case 375:
#line 2163 "fortran.y"
     {(yyval.d)=(yyvsp[0].d);}
#line 4832 "fortran.tab.c"
    break;

  case 376:
#line 2165 "fortran.y"
     {(yyval.d)=(yyvsp[0].d);}
#line 4838 "fortran.tab.c"
    break;

  case 377:
#line 2167 "fortran.y"
     {(yyval.d)=(yyvsp[0].d);}
#line 4844 "fortran.tab.c"
    break;

  case 378:
#line 2171 "fortran.y"
        {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 )  (yyval.d)=insertdim(NULL,(yyvsp[0].dim1));
        }
#line 4854 "fortran.tab.c"
    break;

  case 379:
#line 2177 "fortran.y"
        {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( (!inside_type_declare) && created_dimensionlist == 1 ) (yyval.d)=insertdim((yyvsp[-2].d),(yyvsp[0].dim1));
        }
#line 4864 "fortran.tab.c"
    break;

  case 380:
#line 2186 "fortran.y"
     {strcpy((yyval.dim1).first,(yyvsp[-2].na));  Save_Length((yyvsp[-2].na),2); strcpy((yyval.dim1).last,(yyvsp[0].na)); Save_Length((yyvsp[0].na),1); }
#line 4870 "fortran.tab.c"
    break;

  case 381:
#line 2188 "fortran.y"
     {strcpy((yyval.dim1).first,"1"); strcpy((yyval.dim1).last,(yyvsp[0].na)); Save_Length((yyvsp[0].na),1);}
#line 4876 "fortran.tab.c"
    break;

  case 382:
#line 2193 "fortran.y"
     {strcpy((yyval.na),(yyvsp[0].na));}
#line 4882 "fortran.tab.c"
    break;

  case 384:
#line 2202 "fortran.y"
        {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 )  (yyval.d)=insertdim(NULL,(yyvsp[0].dim1));
        }
#line 4892 "fortran.tab.c"
    break;

  case 385:
#line 2208 "fortran.y"
        {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( (!inside_type_declare) && created_dimensionlist == 1 ) (yyval.d)=insertdim((yyvsp[-2].d),(yyvsp[0].dim1));
        }
#line 4902 "fortran.tab.c"
    break;

  case 386:
#line 2217 "fortran.y"
      { strcpy((yyval.dim1).first,"");  strcpy((yyval.dim1).last,"");  }
#line 4908 "fortran.tab.c"
    break;

  case 387:
#line 2219 "fortran.y"
      { strcpy((yyval.dim1).first,(yyvsp[-1].na));  Save_Length((yyvsp[-1].na),2); strcpy((yyval.dim1).last,""); }
#line 4914 "fortran.tab.c"
    break;

  case 388:
#line 2224 "fortran.y"
        {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 )  (yyval.d)=insertdim(NULL,(yyvsp[0].dim1));
        }
#line 4924 "fortran.tab.c"
    break;

  case 389:
#line 2230 "fortran.y"
        {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( (!inside_type_declare) && created_dimensionlist == 1 ) (yyval.d)=insertdim((yyvsp[-2].d),(yyvsp[0].dim1));
        }
#line 4934 "fortran.tab.c"
    break;

  case 390:
#line 2239 "fortran.y"
     { strcpy((yyval.dim1).first,"");  strcpy((yyval.dim1).last,"");  }
#line 4940 "fortran.tab.c"
    break;

  case 391:
#line 2244 "fortran.y"
        {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 ) 
            {
            if (!strcasecmp((yyvsp[-1].na),""))
            {
            strcpy(my_dim.first,"1");
            }
            else
            {
            strcpy(my_dim.first,(yyvsp[-1].na));
            }
            strcpy(my_dim.last,"*");
            (yyval.d)=insertdim((yyvsp[-2].d),my_dim);
            strcpy(my_dim.first,"");
            strcpy(my_dim.last,"");
            }
        }
#line 4964 "fortran.tab.c"
    break;

  case 392:
#line 2266 "fortran.y"
     {(yyval.d) = (listdim *) NULL;}
#line 4970 "fortran.tab.c"
    break;

  case 393:
#line 2268 "fortran.y"
     {(yyval.d) = (yyvsp[-1].d);}
#line 4976 "fortran.tab.c"
    break;

  case 394:
#line 2286 "fortran.y"
     {strcpy((yyval.na),"");}
#line 4982 "fortran.tab.c"
    break;

  case 395:
#line 2288 "fortran.y"
     {strcpy((yyval.na),(yyvsp[-1].na));}
#line 4988 "fortran.tab.c"
    break;

  case 399:
#line 2301 "fortran.y"
     { strcpy((yyval.na),(yyvsp[0].na)); }
#line 4994 "fortran.tab.c"
    break;

  case 400:
#line 2303 "fortran.y"
     { strcpy((yyval.na),(yyvsp[0].na)); }
#line 5000 "fortran.tab.c"
    break;

  case 401:
#line 2305 "fortran.y"
     { strcpy((yyval.na),(yyvsp[0].na)); }
#line 5006 "fortran.tab.c"
    break;

  case 402:
#line 2310 "fortran.y"
     {
            if ((firstpass == 0) && (PublicDeclare == 1))
            {
                if ((yyvsp[0].lnn))
                {
                    removeglobfromlist(&((yyvsp[0].lnn)));
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_cur,pos_end-pos_cur);
                    writelistpublic((yyvsp[0].lnn));
                }
            }
     PublicDeclare = 0;
     PrivateDeclare = 0;
     }
#line 5025 "fortran.tab.c"
    break;

  case 404:
#line 2328 "fortran.y"
     {(yyval.lnn)=(listname *)NULL;}
#line 5031 "fortran.tab.c"
    break;

  case 405:
#line 2330 "fortran.y"
     {(yyval.lnn)=(yyvsp[0].lnn);}
#line 5037 "fortran.tab.c"
    break;

  case 406:
#line 2334 "fortran.y"
     {(yyval.lnn)=Insertname(NULL,(yyvsp[0].na),0);}
#line 5043 "fortran.tab.c"
    break;

  case 407:
#line 2336 "fortran.y"
     {(yyval.lnn)=Insertname((yyvsp[-2].lnn),(yyvsp[0].na),0);}
#line 5049 "fortran.tab.c"
    break;

  case 410:
#line 2346 "fortran.y"
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
#line 5065 "fortran.tab.c"
    break;

  case 416:
#line 2370 "fortran.y"
        {
            if (firstpass == 1)  
            {
            Add_Data_Var_Names_01(&List_Data_Var,(yyvsp[-3].l),(yyvsp[-1].lnn));
            }
            else                 Add_Data_Var_Names_01(&List_Data_Var_Cur,(yyvsp[-3].l),(yyvsp[-1].lnn));
        }
#line 5077 "fortran.tab.c"
    break;

  case 417:
#line 2380 "fortran.y"
     { (yyval.l)=insertvar(NULL,(yyvsp[0].v)); }
#line 5083 "fortran.tab.c"
    break;

  case 418:
#line 2382 "fortran.y"
     {
     (yyval.l) = insertvar((yyvsp[-2].l),(yyvsp[0].v));
     }
#line 5091 "fortran.tab.c"
    break;

  case 419:
#line 2388 "fortran.y"
     {(yyval.lnn)=Insertname(NULL,(yyvsp[0].na),0);}
#line 5097 "fortran.tab.c"
    break;

  case 420:
#line 2390 "fortran.y"
     {(yyval.lnn) = Insertname((yyvsp[-2].lnn),(yyvsp[0].na),1);   }
#line 5103 "fortran.tab.c"
    break;

  case 423:
#line 2400 "fortran.y"
     {printf("DOVARIABLE = %s %s %s\n",(yyvsp[-5].na),(yyvsp[-3].na),(yyvsp[-1].na));
     printf("AUTRE = %s %s\n",(yyvsp[-7].l)->var->v_nomvar,(yyvsp[-7].l)->var->v_initialvalue_array);
     Insertdoloop((yyvsp[-7].l)->var,(yyvsp[-5].na),(yyvsp[-3].na),(yyvsp[-1].na),"");
     (yyval.v)=(yyvsp[-7].l)->var;
     }
#line 5113 "fortran.tab.c"
    break;

  case 424:
#line 2406 "fortran.y"
     {
     Insertdoloop((yyvsp[-9].l)->var,(yyvsp[-7].na),(yyvsp[-5].na),(yyvsp[-3].na),(yyvsp[-1].na));
     (yyval.v)=(yyvsp[-9].l)->var;
     }
#line 5122 "fortran.tab.c"
    break;

  case 425:
#line 2413 "fortran.y"
     {(yyval.l)=insertvar(NULL,(yyvsp[0].v));}
#line 5128 "fortran.tab.c"
    break;

  case 426:
#line 2415 "fortran.y"
     {(yyval.l) = insertvar((yyvsp[-2].l),(yyvsp[0].v));}
#line 5134 "fortran.tab.c"
    break;

  case 428:
#line 2421 "fortran.y"
     {(yyval.v)->v_initialvalue_array=Insertname((yyval.v)->v_initialvalue_array,my_dim.last,0);
     strcpy(my_dim.last,"");
     }
#line 5142 "fortran.tab.c"
    break;

  case 431:
#line 2434 "fortran.y"
     {sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na));}
#line 5148 "fortran.tab.c"
    break;

  case 432:
#line 2436 "fortran.y"
     {sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na));}
#line 5154 "fortran.tab.c"
    break;

  case 433:
#line 2438 "fortran.y"
     {sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na));}
#line 5160 "fortran.tab.c"
    break;

  case 439:
#line 2447 "fortran.y"
     {strcpy((yyval.na),"");}
#line 5166 "fortran.tab.c"
    break;

  case 440:
#line 2449 "fortran.y"
     {sprintf((yyval.na),"*%s",(yyvsp[0].na));}
#line 5172 "fortran.tab.c"
    break;

  case 449:
#line 2485 "fortran.y"
     {strcpy(my_dim.last,"");}
#line 5178 "fortran.tab.c"
    break;

  case 450:
#line 2489 "fortran.y"
                {positioninblock = 0; pos_curdimension = my_position_before;}
#line 5184 "fortran.tab.c"
    break;

  case 451:
#line 2491 "fortran.y"
        {
            /* if the variable is a parameter we can suppose that is   */
            /*    value is the same on each grid. It is not useless to */
            /*    create a copy of it on each grid                     */
            if ( ! inside_type_declare )
            {
                if ( firstpass )
                {
                    Add_Globliste_1((yyvsp[0].l));
                    /* if variableparamlists has been declared in a subroutine   */
                    if ( insubroutinedeclare )     Add_Dimension_Var_1((yyvsp[0].l));
                    
                    /* Add it to the List_SubroutineDeclaration_Var list if not present */
                    /* NB: if not done, a variable declared with DIMENSION but with no type given */
                    /* will not be declared by the conv */
                    ReWriteDeclarationAndAddTosubroutine_01((yyvsp[0].l));
                }
                else
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_curdimension,pos_end-pos_curdimension);
                    ReWriteDeclarationAndAddTosubroutine_01((yyvsp[0].l));
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
#line 5232 "fortran.tab.c"
    break;

  case 453:
#line 2537 "fortran.y"
                                   {in_complex_literal = 0;}
#line 5238 "fortran.tab.c"
    break;

  case 454:
#line 2538 "fortran.y"
     {
        if ( inside_type_declare ) break;
        curvar = createvar((yyvsp[-4].na),(yyvsp[-1].d));
        CreateAndFillin_Curvar("", curvar);
        curlistvar=insertvar(NULL, curvar);
        (yyval.l) = settype("",curlistvar);
        strcpy(vallengspec,"");
     }
#line 5251 "fortran.tab.c"
    break;

  case 455:
#line 2546 "fortran.y"
                                             {in_complex_literal = 0;}
#line 5257 "fortran.tab.c"
    break;

  case 456:
#line 2547 "fortran.y"
        {
        if ( inside_type_declare ) break;
        curvar = createvar((yyvsp[-4].na),(yyvsp[-1].d));
        CreateAndFillin_Curvar("", curvar);
        curlistvar = insertvar((yyvsp[-6].l), curvar);
        (yyval.l) = curlistvar;
        strcpy(vallengspec,"");
        }
#line 5270 "fortran.tab.c"
    break;

  case 457:
#line 2559 "fortran.y"
                              { VariableIsParameter = 1; pos_curparameter = setposcur()-9; }
#line 5276 "fortran.tab.c"
    break;

  case 458:
#line 2560 "fortran.y"
        {
            if ( ! inside_type_declare )
            {
                if ( firstpass )
                {
                    if ( insubroutinedeclare )  Add_Parameter_Var_1((yyvsp[-1].l));
                    else                        Add_GlobalParameter_Var_1((yyvsp[-1].l));
                }
                else
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out, pos_curparameter, pos_end-pos_curparameter);
                }
            }
            VariableIsParameter =  0 ;
        }
#line 5297 "fortran.tab.c"
    break;

  case 460:
#line 2580 "fortran.y"
     {(yyval.l)=insertvar(NULL,(yyvsp[0].v));}
#line 5303 "fortran.tab.c"
    break;

  case 461:
#line 2582 "fortran.y"
     {(yyval.l)=insertvar((yyvsp[-2].l),(yyvsp[0].v));}
#line 5309 "fortran.tab.c"
    break;

  case 462:
#line 2587 "fortran.y"
        {
            if ( inside_type_declare ) break;
            curvar=(variable *) calloc(1,sizeof(variable));
            Init_Variable(curvar);
            curvar->v_VariableIsParameter = 1;
            strcpy(curvar->v_nomvar,(yyvsp[-2].na));
            strcpy(curvar->v_subroutinename,subroutinename);
            strcpy(curvar->v_modulename,curmodulename);
            curvar->v_initialvalue=Insertname(curvar->v_initialvalue,(yyvsp[0].na),0);
            strcpy(curvar->v_commoninfile,cur_filename);
            Save_Length((yyvsp[0].na),14);
            (yyval.v) = curvar;
        }
#line 5327 "fortran.tab.c"
    break;

  case 463:
#line 2603 "fortran.y"
           {pos_cursave = my_position_before;}
#line 5333 "fortran.tab.c"
    break;

  case 464:
#line 2604 "fortran.y"
     {
     pos_end = setposcur();
     RemoveWordSET_0(fortran_out,pos_cursave,pos_end-pos_cursave);
     }
#line 5342 "fortran.tab.c"
    break;

  case 472:
#line 2625 "fortran.y"
     {if ( ! inside_type_declare ) Add_Save_Var_1((yyvsp[0].na),(listdim*) NULL); }
#line 5348 "fortran.tab.c"
    break;

  case 476:
#line 2635 "fortran.y"
     {my_position = my_position_before;}
#line 5354 "fortran.tab.c"
    break;

  case 478:
#line 2641 "fortran.y"
        {
            if ( insubroutinedeclare == 1 )
            {
                Add_ImplicitNoneSubroutine_1();
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,my_position,pos_end-my_position);
            }
        }
#line 5367 "fortran.tab.c"
    break;

  case 496:
#line 2693 "fortran.y"
                     {in_complex_literal=0;}
#line 5373 "fortran.tab.c"
    break;

  case 503:
#line 2708 "fortran.y"
                        { positioninblock = 0; pos_curcommon = my_position_before; indeclaration=1;}
#line 5379 "fortran.tab.c"
    break;

  case 504:
#line 2709 "fortran.y"
     {
            indeclaration = 0;
            if ( inside_type_declare ) break;
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_curcommon,pos_end-pos_curcommon);
     }
#line 5390 "fortran.tab.c"
    break;

  case 507:
#line 2720 "fortran.y"
     {
     if ( inside_type_declare ) break;
     sprintf(charusemodule,"%s",(yyvsp[0].na));
     Add_NameOfCommon_1((yyvsp[0].na),subroutinename);
     }
#line 5400 "fortran.tab.c"
    break;

  case 508:
#line 2728 "fortran.y"
        {
            strcpy((yyval.na),"");
            positioninblock=0;
            strcpy(commonblockname,"");
        }
#line 5410 "fortran.tab.c"
    break;

  case 509:
#line 2734 "fortran.y"
        {
            strcpy((yyval.na),(yyvsp[-1].na));
            positioninblock=0;
            strcpy(commonblockname,(yyvsp[-1].na));
        }
#line 5420 "fortran.tab.c"
    break;

  case 513:
#line 2747 "fortran.y"
     {
     if ( inside_type_declare ) break;
     sprintf(charusemodule,"%s",(yyvsp[0].na));
     Add_NameOfCommon_1((yyvsp[0].na),subroutinename);
     }
#line 5430 "fortran.tab.c"
    break;

  case 515:
#line 2757 "fortran.y"
     {if ( ! inside_type_declare ) Add_Common_var_1(); }
#line 5436 "fortran.tab.c"
    break;

  case 516:
#line 2759 "fortran.y"
     {if ( ! inside_type_declare ) Add_Common_var_1(); }
#line 5442 "fortran.tab.c"
    break;

  case 517:
#line 2767 "fortran.y"
        {
            positioninblock = positioninblock + 1 ;
            strcpy(commonvar,(yyvsp[0].na));
            commondim = (listdim*) NULL;
        }
#line 5452 "fortran.tab.c"
    break;

  case 518:
#line 2772 "fortran.y"
                    {in_complex_literal=0;}
#line 5458 "fortran.tab.c"
    break;

  case 519:
#line 2773 "fortran.y"
        {
            positioninblock = positioninblock + 1 ;
            strcpy(commonvar,(yyvsp[-4].na));
            commondim = (yyvsp[-1].d);
        }
#line 5468 "fortran.tab.c"
    break;

  case 523:
#line 2785 "fortran.y"
     {(yyval.v)=createvar((yyvsp[0].na),NULL);}
#line 5474 "fortran.tab.c"
    break;

  case 525:
#line 2797 "fortran.y"
       {if (strcmp(my_dim.last,""))
       {
       (yyval.v)->v_initialvalue_array=Insertname(NULL,my_dim.last,0);
       }
       strcpy(my_dim.last,"");
       }
#line 5485 "fortran.tab.c"
    break;

  case 535:
#line 2839 "fortran.y"
     {sprintf((yyval.na),"%s(%s)",(yyvsp[-3].na),(yyvsp[-1].na));}
#line 5491 "fortran.tab.c"
    break;

  case 536:
#line 2841 "fortran.y"
     {sprintf((yyval.na),"%s(%s)",(yyvsp[-3].na),(yyvsp[-1].na));}
#line 5497 "fortran.tab.c"
    break;

  case 537:
#line 2856 "fortran.y"
     {sprintf((yyval.na),"%s:%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 5503 "fortran.tab.c"
    break;

  case 538:
#line 2861 "fortran.y"
     {sprintf((yyval.na),"%s%s",(yyvsp[-1].v)->v_nomvar,(yyvsp[0].na));}
#line 5509 "fortran.tab.c"
    break;

  case 539:
#line 2865 "fortran.y"
     {strcpy((yyval.na),"");}
#line 5515 "fortran.tab.c"
    break;

  case 540:
#line 2867 "fortran.y"
     {sprintf((yyval.na),"%s%%%s",(yyvsp[-2].na),(yyvsp[0].v)->v_nomvar);}
#line 5521 "fortran.tab.c"
    break;

  case 541:
#line 2872 "fortran.y"
     {(yyval.v)=createvar((yyvsp[0].na),NULL);}
#line 5527 "fortran.tab.c"
    break;

  case 542:
#line 2873 "fortran.y"
                 {in_complex_literal=0;}
#line 5533 "fortran.tab.c"
    break;

  case 543:
#line 2874 "fortran.y"
     {sprintf(ligne,"%s(%s)",(yyvsp[-4].na),(yyvsp[-1].na));(yyval.v)=createvar((yyvsp[-4].na),NULL);strcpy(my_dim.last,(yyvsp[-1].na));}
#line 5539 "fortran.tab.c"
    break;

  case 545:
#line 2890 "fortran.y"
     {strcpy(my_dim.last,"");}
#line 5545 "fortran.tab.c"
    break;

  case 546:
#line 2895 "fortran.y"
      {strcpy(my_dim.last,"");}
#line 5551 "fortran.tab.c"
    break;

  case 547:
#line 2900 "fortran.y"
     {strcpy(my_dim.last,"");}
#line 5557 "fortran.tab.c"
    break;

  case 548:
#line 2902 "fortran.y"
     {strcpy(my_dim.last,"");}
#line 5563 "fortran.tab.c"
    break;

  case 549:
#line 2908 "fortran.y"
      {strcpy((yyval.na),"");}
#line 5569 "fortran.tab.c"
    break;

  case 550:
#line 2910 "fortran.y"
      {strcpy((yyval.na),(yyvsp[0].na));}
#line 5575 "fortran.tab.c"
    break;

  case 551:
#line 2912 "fortran.y"
      {sprintf((yyval.na),"%s,%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 5581 "fortran.tab.c"
    break;

  case 552:
#line 2934 "fortran.y"
     {sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na));}
#line 5587 "fortran.tab.c"
    break;

  case 553:
#line 2936 "fortran.y"
     {strcpy((yyval.na),":");}
#line 5593 "fortran.tab.c"
    break;

  case 554:
#line 2938 "fortran.y"
     {sprintf((yyval.na),":%s",(yyvsp[0].na));}
#line 5599 "fortran.tab.c"
    break;

  case 555:
#line 2940 "fortran.y"
     {sprintf((yyval.na),": :%s",(yyvsp[0].na));}
#line 5605 "fortran.tab.c"
    break;

  case 556:
#line 2942 "fortran.y"
     {sprintf((yyval.na),":%s :%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 5611 "fortran.tab.c"
    break;

  case 557:
#line 2944 "fortran.y"
     {sprintf((yyval.na),"::%s",(yyvsp[0].na));}
#line 5617 "fortran.tab.c"
    break;

  case 559:
#line 2947 "fortran.y"
     {sprintf((yyval.na),"%s=%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 5623 "fortran.tab.c"
    break;

  case 560:
#line 2949 "fortran.y"
     {sprintf((yyval.na),"%s=*%s",(yyvsp[-3].na),(yyvsp[0].na));}
#line 5629 "fortran.tab.c"
    break;

  case 561:
#line 2951 "fortran.y"
     {sprintf((yyval.na),"*%s",(yyvsp[0].na));}
#line 5635 "fortran.tab.c"
    break;

  case 562:
#line 2955 "fortran.y"
     {strcpy((yyval.na),":");}
#line 5641 "fortran.tab.c"
    break;

  case 563:
#line 2957 "fortran.y"
     {sprintf((yyval.na),":%s",(yyvsp[0].na));}
#line 5647 "fortran.tab.c"
    break;

  case 564:
#line 2959 "fortran.y"
     {sprintf((yyval.na),": :%s",(yyvsp[0].na));}
#line 5653 "fortran.tab.c"
    break;

  case 565:
#line 2961 "fortran.y"
     {sprintf((yyval.na),":%s :%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 5659 "fortran.tab.c"
    break;

  case 566:
#line 2963 "fortran.y"
     {sprintf((yyval.na),"::%s",(yyvsp[0].na));}
#line 5665 "fortran.tab.c"
    break;

  case 567:
#line 2965 "fortran.y"
     {strcpy((yyval.na),"");}
#line 5671 "fortran.tab.c"
    break;

  case 569:
#line 2983 "fortran.y"
                                {in_complex_literal=0;}
#line 5677 "fortran.tab.c"
    break;

  case 570:
#line 2984 "fortran.y"
     {inallocate = 0;}
#line 5683 "fortran.tab.c"
    break;

  case 594:
#line 3054 "fortran.y"
                                    {in_complex_literal=0;}
#line 5689 "fortran.tab.c"
    break;

  case 595:
#line 3055 "fortran.y"
     {inallocate = 0;}
#line 5695 "fortran.tab.c"
    break;

  case 605:
#line 3085 "fortran.y"
      {
      strcpy((yyval.na),(yyvsp[0].v)->v_nomvar);
      if (strcasecmp(my_dim.last,""))
      {
      strcat((yyval.na),"(");
      strcat((yyval.na),my_dim.last);
      strcat((yyval.na),")");
      }
      }
#line 5709 "fortran.tab.c"
    break;

  case 609:
#line 3098 "fortran.y"
     { sprintf((yyval.na),"(%s)",(yyvsp[-1].na));}
#line 5715 "fortran.tab.c"
    break;

  case 610:
#line 3103 "fortran.y"
      {strcpy(my_dim.last,"");}
#line 5721 "fortran.tab.c"
    break;

  case 612:
#line 3109 "fortran.y"
     {sprintf((yyval.na),"%s**%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 5727 "fortran.tab.c"
    break;

  case 614:
#line 3114 "fortran.y"
     { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5733 "fortran.tab.c"
    break;

  case 616:
#line 3122 "fortran.y"
     { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5739 "fortran.tab.c"
    break;

  case 617:
#line 3124 "fortran.y"
     { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5745 "fortran.tab.c"
    break;

  case 619:
#line 3127 "fortran.y"
     { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5751 "fortran.tab.c"
    break;

  case 621:
#line 3136 "fortran.y"
     {strcpy((yyval.na),"*");}
#line 5757 "fortran.tab.c"
    break;

  case 623:
#line 3142 "fortran.y"
     {strcpy((yyval.na),"+");}
#line 5763 "fortran.tab.c"
    break;

  case 624:
#line 3144 "fortran.y"
     {strcpy((yyval.na),"-");}
#line 5769 "fortran.tab.c"
    break;

  case 626:
#line 3150 "fortran.y"
     { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5775 "fortran.tab.c"
    break;

  case 629:
#line 3159 "fortran.y"
     { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5781 "fortran.tab.c"
    break;

  case 638:
#line 3172 "fortran.y"
     {strcpy((yyval.na),"<");}
#line 5787 "fortran.tab.c"
    break;

  case 640:
#line 3175 "fortran.y"
     {strcpy((yyval.na),">");}
#line 5793 "fortran.tab.c"
    break;

  case 643:
#line 3183 "fortran.y"
     { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5799 "fortran.tab.c"
    break;

  case 645:
#line 3190 "fortran.y"
     { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5805 "fortran.tab.c"
    break;

  case 647:
#line 3197 "fortran.y"
     { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5811 "fortran.tab.c"
    break;

  case 649:
#line 3203 "fortran.y"
     { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5817 "fortran.tab.c"
    break;

  case 659:
#line 3239 "fortran.y"
     {strcpy((yyval.na),"");}
#line 5823 "fortran.tab.c"
    break;

  case 662:
#line 3248 "fortran.y"
     {
     strcpy((yyval.na),(yyvsp[0].na));
     }
#line 5831 "fortran.tab.c"
    break;

  case 663:
#line 3255 "fortran.y"
     {strcpy((yyval.na),(yyvsp[0].na));}
#line 5837 "fortran.tab.c"
    break;

  case 792:
#line 3628 "fortran.y"
                                                           {in_select_case_stmt++;}
#line 5843 "fortran.tab.c"
    break;

  case 794:
#line 3629 "fortran.y"
                                                   {in_select_case_stmt++;}
#line 5849 "fortran.tab.c"
    break;

  case 798:
#line 3638 "fortran.y"
                               {in_select_case_stmt--;}
#line 5855 "fortran.tab.c"
    break;

  case 800:
#line 3639 "fortran.y"
                                 {in_select_case_stmt--;}
#line 5861 "fortran.tab.c"
    break;

  case 802:
#line 3644 "fortran.y"
              {in_complex_literal=0;}
#line 5867 "fortran.tab.c"
    break;

  case 826:
#line 3707 "fortran.y"
                        {close_or_connect = 1;}
#line 5873 "fortran.tab.c"
    break;

  case 827:
#line 3707 "fortran.y"
                                                                      {close_or_connect = 0;}
#line 5879 "fortran.tab.c"
    break;

  case 845:
#line 3738 "fortran.y"
                                    {close_or_connect = 1;}
#line 5885 "fortran.tab.c"
    break;

  case 846:
#line 3739 "fortran.y"
        {close_or_connect = 0;}
#line 5891 "fortran.tab.c"
    break;

  case 855:
#line 3757 "fortran.y"
         {
         in_io_control_spec = 0;
         }
#line 5899 "fortran.tab.c"
    break;

  case 857:
#line 3762 "fortran.y"
         {
         in_io_control_spec = 0;
         }
#line 5907 "fortran.tab.c"
    break;

  case 861:
#line 3772 "fortran.y"
         {
         in_io_control_spec = 0;
         }
#line 5915 "fortran.tab.c"
    break;

  case 863:
#line 3777 "fortran.y"
         {
         in_io_control_spec = 0;
         }
#line 5923 "fortran.tab.c"
    break;

  case 917:
#line 3895 "fortran.y"
     {in_inquire=0;}
#line 5929 "fortran.tab.c"
    break;

  case 919:
#line 3898 "fortran.y"
     {in_inquire=0;}
#line 5935 "fortran.tab.c"
    break;

  case 921:
#line 3902 "fortran.y"
                {in_inquire=1;}
#line 5941 "fortran.tab.c"
    break;

  case 937:
#line 3930 "fortran.y"
                                                                     {pos_endsubroutine=setposcur();}
#line 5947 "fortran.tab.c"
    break;

  case 941:
#line 3939 "fortran.y"
        {
            GlobalDeclaration = 0;
            strcpy(curmodulename,(yyvsp[0].na));
            strcpy(subroutinename,"");
            Add_NameOfModule_1((yyvsp[0].na));
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
#line 5967 "fortran.tab.c"
    break;

  case 943:
#line 3959 "fortran.y"
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
#line 5993 "fortran.tab.c"
    break;

  case 958:
#line 4011 "fortran.y"
     {if (firstpass == 0 && oldfortran_out) pos_curuseold = setposcurname(oldfortran_out);}
#line 5999 "fortran.tab.c"
    break;

  case 959:
#line 4016 "fortran.y"
    {
            if ( firstpass )
            {
                if ( insubroutinedeclare )
                {
                    if ((yyvsp[0].lc)) {
                      Add_CouplePointed_Var_1((yyvsp[-1].na),(yyvsp[0].lc));
                      coupletmp = (yyvsp[0].lc);
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
                  sprintf(charusemodule,"%s",(yyvsp[-1].na));
                }
                Add_NameOfModuleUsed_1((yyvsp[-1].na));
            }
            else
            {
                if ( insubroutinedeclare )
                {
                  copyuse_0((yyvsp[-1].na));
                    }

                if ( inmoduledeclare == 0 )
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,my_position,pos_end-my_position);
                }
            }
    }
#line 6040 "fortran.tab.c"
    break;

  case 961:
#line 4054 "fortran.y"
    {
            if ( firstpass )
            {
                if ( insubroutinedeclare )
                {
                  if ((yyvsp[0].lc))
                  {
                    Add_CouplePointed_Var_1((yyvsp[-4].na),(yyvsp[0].lc));
                    coupletmp = (yyvsp[0].lc);
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
                  sprintf(charusemodule,"%s",(yyvsp[-4].na));
                }
                Add_NameOfModuleUsed_1((yyvsp[-4].na));
            }
            else
            {
                if ( insubroutinedeclare )
                    copyuseonly_0((yyvsp[-4].na));

                if ( inmoduledeclare == 0 )
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,my_position,pos_end-my_position);
                    if ((yyvsp[0].lc))
                    {
                    if (oldfortran_out)  variableisglobalinmodule((yyvsp[0].lc),(yyvsp[-4].na),oldfortran_out,pos_curuseold);
                    }
                }
                else
                {
                  if ((yyvsp[0].lc))
                  {
                    /* if we are in the module declare and if the    */
                    /* onlylist is a list of global variable         */
                    variableisglobalinmodule((yyvsp[0].lc), (yyvsp[-4].na), fortran_out,my_position);
                  }
                }
            }
    }
#line 6093 "fortran.tab.c"
    break;

  case 966:
#line 4111 "fortran.y"
    {(yyval.lc)=NULL;}
#line 6099 "fortran.tab.c"
    break;

  case 967:
#line 4113 "fortran.y"
    {(yyval.lc)=(yyvsp[0].lc);}
#line 6105 "fortran.tab.c"
    break;

  case 973:
#line 4130 "fortran.y"
        {
            strcpy(subroutinename,(yyvsp[0].na));
            insubroutinedeclare = 1;
            inprogramdeclare = 1;
            /* in the second step we should write the head of       */
            /*    the subroutine sub_loop_<subroutinename>          */
            if ( ! firstpass )
                WriteBeginof_SubLoop();
        }
#line 6119 "fortran.tab.c"
    break;

  case 975:
#line 4143 "fortran.y"
                  {pos_endsubroutine=my_position_before;}
#line 6125 "fortran.tab.c"
    break;

  case 976:
#line 4144 "fortran.y"
     {
            insubroutinedeclare = 0;
            inprogramdeclare = 0;
            pos_cur = setposcur();
            closeandcallsubloopandincludeit_0(3);
            functiondeclarationisdone = 0;
            strcpy(subroutinename,"");     
     }
#line 6138 "fortran.tab.c"
    break;

  case 983:
#line 4166 "fortran.y"
    {
    (yyval.lc)=NULL;
    }
#line 6146 "fortran.tab.c"
    break;

  case 984:
#line 4170 "fortran.y"
    {
    (yyval.lc)=(yyvsp[0].lc);
    }
#line 6154 "fortran.tab.c"
    break;

  case 985:
#line 4176 "fortran.y"
     {
     (yyval.lc)=(yyvsp[0].lc);
     }
#line 6162 "fortran.tab.c"
    break;

  case 986:
#line 4180 "fortran.y"
     {
     /* insert the variable in the list $1                 */
     (yyvsp[0].lc)->suiv = (yyvsp[-2].lc);
     (yyval.lc)=(yyvsp[0].lc);
     }
#line 6172 "fortran.tab.c"
    break;

  case 987:
#line 4189 "fortran.y"
        {
            coupletmp = (listcouple *) calloc(1,sizeof(listcouple));
            strcpy(coupletmp->c_namevar,(yyvsp[-2].na));
            strcpy(coupletmp->c_namepointedvar,(yyvsp[0].na));
            coupletmp->suiv = NULL;
            (yyval.lc) = coupletmp;
        }
#line 6184 "fortran.tab.c"
    break;

  case 988:
#line 4199 "fortran.y"
     {(yyval.lc)=(yyvsp[0].lc);}
#line 6190 "fortran.tab.c"
    break;

  case 989:
#line 4201 "fortran.y"
        {
            /* insert the variable in the list $1                 */
            (yyvsp[0].lc)->suiv = (yyvsp[-2].lc);
            (yyval.lc) = (yyvsp[0].lc);
        }
#line 6200 "fortran.tab.c"
    break;

  case 990:
#line 4210 "fortran.y"
        {
            coupletmp = (listcouple *)calloc(1,sizeof(listcouple));
            strcpy(coupletmp->c_namevar,(yyvsp[0].na));
            strcpy(coupletmp->c_namepointedvar,"");
            coupletmp->suiv = NULL;
            (yyval.lc) = coupletmp;
        }
#line 6212 "fortran.tab.c"
    break;

  case 991:
#line 4218 "fortran.y"
        {
            coupletmp = (listcouple *)calloc(1,sizeof(listcouple));
            strcpy(coupletmp->c_namevar,(yyvsp[0].na));
            strcpy(coupletmp->c_namepointedvar,"");
            coupletmp->suiv = NULL;
            (yyval.lc) = coupletmp;
        }
#line 6224 "fortran.tab.c"
    break;

  case 992:
#line 4226 "fortran.y"
     {
     (yyval.lc)=(yyvsp[0].lc);
     pointedvar = 1;
      Add_UsedInSubroutine_Var_1((yyvsp[0].lc)->c_namevar);
     }
#line 6234 "fortran.tab.c"
    break;

  case 1005:
#line 4266 "fortran.y"
                                {in_complex_literal=0;}
#line 6240 "fortran.tab.c"
    break;

  case 1006:
#line 4267 "fortran.y"
     {sprintf((yyval.na),"%s(%s)",(yyvsp[-4].na),(yyvsp[-1].na));}
#line 6246 "fortran.tab.c"
    break;

  case 1007:
#line 4273 "fortran.y"
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
#line 6262 "fortran.tab.c"
    break;

  case 1009:
#line 4286 "fortran.y"
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
#line 6278 "fortran.tab.c"
    break;

  case 1011:
#line 4298 "fortran.y"
                            {in_complex_literal=0;}
#line 6284 "fortran.tab.c"
    break;

  case 1012:
#line 4299 "fortran.y"
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
#line 6300 "fortran.tab.c"
    break;

  case 1014:
#line 4313 "fortran.y"
                                     {pos_curcall=my_position_before-strlen((yyvsp[-1].na))-4;}
#line 6306 "fortran.tab.c"
    break;

  case 1015:
#line 4314 "fortran.y"
             {
            if (!strcasecmp((yyvsp[0].na),"MPI_Init") )    callmpiinit = 1;
            else                                callmpiinit = 0;

            if (!strcasecmp((yyvsp[0].na),"Agrif_Init_Grids") )
            {
                callagrifinitgrids = 1;
                strcpy(meetagrifinitgrids,subroutinename);
            }
            else
            {
                callagrifinitgrids = 0;
            }
            if ( Vartonumber((yyvsp[0].na)) == 1 )
            {
                incalldeclare = 0;
                inagrifcallargument = 0 ;
                Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
            }
        }
#line 6331 "fortran.tab.c"
    break;

  case 1020:
#line 4345 "fortran.y"
      {sprintf((yyval.na),"%s,%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 6337 "fortran.tab.c"
    break;

  case 1021:
#line 4350 "fortran.y"
        {
            if ( callmpiinit == 1 )
            {
                strcpy(mpiinitvar,(yyvsp[0].na));
                if ( firstpass == 1 )  Add_UsedInSubroutine_Var_1 (mpiinitvar);
            }
        }
#line 6349 "fortran.tab.c"
    break;

  case 1022:
#line 4358 "fortran.y"
     {sprintf((yyval.na),"%s = %s",(yyvsp[-2].na),(yyvsp[0].na));
                 if ( callmpiinit == 1 )
            {
                strcpy(mpiinitvar,(yyvsp[0].na));
                if ( firstpass == 1 )  Add_UsedInSubroutine_Var_1 (mpiinitvar);
            }
            }
#line 6361 "fortran.tab.c"
    break;

  case 1024:
#line 4370 "fortran.y"
     {
     strcpy((yyval.na),(yyvsp[0].v)->v_nomvar);
     if ((yyvsp[0].v)->v_initialvalue_array)
     {
     strcat((yyval.na),"(");
     strcat((yyval.na),(yyvsp[0].v)->v_initialvalue_array->n_name);
     strcat((yyval.na),")");
     }
     }
#line 6375 "fortran.tab.c"
    break;

  case 1026:
#line 4382 "fortran.y"
                {isrecursive = 0;}
#line 6381 "fortran.tab.c"
    break;

  case 1030:
#line 4393 "fortran.y"
     {isrecursive = 0; functiondeclarationisdone = 1;}
#line 6387 "fortran.tab.c"
    break;

  case 1031:
#line 4395 "fortran.y"
     {isrecursive = 0;}
#line 6393 "fortran.tab.c"
    break;

  case 1032:
#line 4397 "fortran.y"
     {isrecursive = 1;}
#line 6399 "fortran.tab.c"
    break;

  case 1034:
#line 4406 "fortran.y"
                       {in_complex_literal=0;}
#line 6405 "fortran.tab.c"
    break;

  case 1035:
#line 4407 "fortran.y"
     {
            insubroutinedeclare = 1;
            suborfun = 0;
            /* we should to list of the subroutine argument the  */
            /*    name of the function which has to be defined   */
            if ( firstpass )
            {
                Add_SubroutineArgument_Var_1((yyvsp[-2].l));
                if ( ! is_result_present )
                    Add_FunctionType_Var_1((yyvsp[-5].na));
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
#line 6431 "fortran.tab.c"
    break;

  case 1037:
#line 4432 "fortran.y"
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
     strcpy((yyval.na),(yyvsp[0].na));strcpy(subroutinename,(yyvsp[0].na));
     }
#line 6448 "fortran.tab.c"
    break;

  case 1038:
#line 4457 "fortran.y"
     {strcpy((yyval.na),(yyvsp[0].na));}
#line 6454 "fortran.tab.c"
    break;

  case 1039:
#line 4461 "fortran.y"
     {is_result_present = 0; }
#line 6460 "fortran.tab.c"
    break;

  case 1041:
#line 4467 "fortran.y"
     {is_result_present = 1;
                 if ( firstpass == 1 )
            {
                strcpy(nameinttypenameback,nameinttypename);
                strcpy(nameinttypename,"");
                curvar = createvar((yyvsp[-1].na),NULL);
                strcpy(nameinttypename,nameinttypenameback);
                strcpy(curvar->v_typevar,"");
                curlistvar = insertvar(NULL,curvar);
                Add_SubroutineArgument_Var_1(curlistvar);
            }
     }
#line 6477 "fortran.tab.c"
    break;

  case 1042:
#line 4483 "fortran.y"
     {strcpy(DeclType, "");}
#line 6483 "fortran.tab.c"
    break;

  case 1047:
#line 4497 "fortran.y"
        {
            insubroutinedeclare = 1;
            suborfun = 1;
            if ( firstpass )
                Add_SubroutineArgument_Var_1((yyvsp[0].l));
            else
              {
                WriteBeginof_SubLoop();
              }
        }
#line 6498 "fortran.tab.c"
    break;

  case 1049:
#line 4512 "fortran.y"
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
     strcpy((yyval.na),(yyvsp[0].na));strcpy(subroutinename,(yyvsp[0].na));
     }
#line 6515 "fortran.tab.c"
    break;

  case 1051:
#line 4533 "fortran.y"
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
#line 6563 "fortran.tab.c"
    break;

  case 1054:
#line 4582 "fortran.y"
     {if (firstpass) (yyval.l)=NULL;}
#line 6569 "fortran.tab.c"
    break;

  case 1055:
#line 4583 "fortran.y"
           {in_complex_literal=0;}
#line 6575 "fortran.tab.c"
    break;

  case 1056:
#line 4584 "fortran.y"
     {if (firstpass) (yyval.l)=(yyvsp[-1].l);}
#line 6581 "fortran.tab.c"
    break;

  case 1057:
#line 4588 "fortran.y"
     {if (firstpass) (yyval.l)=NULL;}
#line 6587 "fortran.tab.c"
    break;

  case 1058:
#line 4590 "fortran.y"
     {if (firstpass) (yyval.l)=(yyvsp[0].l);}
#line 6593 "fortran.tab.c"
    break;

  case 1059:
#line 4595 "fortran.y"
        {
            if ( firstpass == 1 )
            {
                strcpy(nameinttypenameback,nameinttypename);
                strcpy(nameinttypename,"");
                curvar = createvar((yyvsp[0].na),NULL);
                strcpy(nameinttypename,nameinttypenameback);
                curlistvar = insertvar(NULL,curvar);
                (yyval.l) = settype("",curlistvar);
            }
        }
#line 6609 "fortran.tab.c"
    break;

  case 1060:
#line 4607 "fortran.y"
        {
            if ( firstpass == 1 )
            {
                strcpy(nameinttypenameback,nameinttypename);
                strcpy(nameinttypename,"");
                curvar = createvar((yyvsp[0].na),NULL);
                strcpy(nameinttypename,nameinttypenameback);
                (yyval.l) = insertvar((yyvsp[-2].l),curvar);
            }
        }
#line 6624 "fortran.tab.c"
    break;

  case 1061:
#line 4621 "fortran.y"
      {strcpy((yyval.na),(yyvsp[0].na));}
#line 6630 "fortran.tab.c"
    break;

  case 1062:
#line 4623 "fortran.y"
      {strcpy((yyval.na),"*");}
#line 6636 "fortran.tab.c"
    break;

  case 1065:
#line 4633 "fortran.y"
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
#line 6669 "fortran.tab.c"
    break;

  case 1067:
#line 4668 "fortran.y"
                 {strcpy((yyval.na),"");}
#line 6675 "fortran.tab.c"
    break;

  case 1068:
#line 4669 "fortran.y"
                 {strcpy((yyval.na),(yyvsp[0].na));}
#line 6681 "fortran.tab.c"
    break;

  case 1074:
#line 4797 "fortran.y"
                            { afterpercent = 1; }
#line 6687 "fortran.tab.c"
    break;


#line 6691 "fortran.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *, YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
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


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
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


#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif


/*-----------------------------------------------------.
| yyreturn -- parsing is finished, return the result.  |
`-----------------------------------------------------*/
yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[+*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 4894 "fortran.y"


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
#line 2 "fortran.yy.c"

#line 4 "fortran.yy.c"

#define  YY_INT_ALIGNED short int

/* A lexical scanner generated by flex */

#define yy_create_buffer fortran__create_buffer
#define yy_delete_buffer fortran__delete_buffer
#define yy_scan_buffer fortran__scan_buffer
#define yy_scan_string fortran__scan_string
#define yy_scan_bytes fortran__scan_bytes
#define yy_init_buffer fortran__init_buffer
#define yy_flush_buffer fortran__flush_buffer
#define yy_load_buffer_state fortran__load_buffer_state
#define yy_switch_to_buffer fortran__switch_to_buffer
#define yypush_buffer_state fortran_push_buffer_state
#define yypop_buffer_state fortran_pop_buffer_state
#define yyensure_buffer_stack fortran_ensure_buffer_stack
#define yy_flex_debug fortran__flex_debug
#define yyin fortran_in
#define yyleng fortran_leng
#define yylex fortran_lex
#define yylineno fortran_lineno
#define yyout fortran_out
#define yyrestart fortran_restart
#define yytext fortran_text
#define yywrap fortran_wrap
#define yyalloc fortran_alloc
#define yyrealloc fortran_realloc
#define yyfree fortran_free

#define FLEX_SCANNER
#define YY_FLEX_MAJOR_VERSION 2
#define YY_FLEX_MINOR_VERSION 6
#define YY_FLEX_SUBMINOR_VERSION 4
#if YY_FLEX_SUBMINOR_VERSION > 0
#define FLEX_BETA
#endif

#ifdef yy_create_buffer
#define fortran__create_buffer_ALREADY_DEFINED
#else
#define yy_create_buffer fortran__create_buffer
#endif

#ifdef yy_delete_buffer
#define fortran__delete_buffer_ALREADY_DEFINED
#else
#define yy_delete_buffer fortran__delete_buffer
#endif

#ifdef yy_scan_buffer
#define fortran__scan_buffer_ALREADY_DEFINED
#else
#define yy_scan_buffer fortran__scan_buffer
#endif

#ifdef yy_scan_string
#define fortran__scan_string_ALREADY_DEFINED
#else
#define yy_scan_string fortran__scan_string
#endif

#ifdef yy_scan_bytes
#define fortran__scan_bytes_ALREADY_DEFINED
#else
#define yy_scan_bytes fortran__scan_bytes
#endif

#ifdef yy_init_buffer
#define fortran__init_buffer_ALREADY_DEFINED
#else
#define yy_init_buffer fortran__init_buffer
#endif

#ifdef yy_flush_buffer
#define fortran__flush_buffer_ALREADY_DEFINED
#else
#define yy_flush_buffer fortran__flush_buffer
#endif

#ifdef yy_load_buffer_state
#define fortran__load_buffer_state_ALREADY_DEFINED
#else
#define yy_load_buffer_state fortran__load_buffer_state
#endif

#ifdef yy_switch_to_buffer
#define fortran__switch_to_buffer_ALREADY_DEFINED
#else
#define yy_switch_to_buffer fortran__switch_to_buffer
#endif

#ifdef yypush_buffer_state
#define fortran_push_buffer_state_ALREADY_DEFINED
#else
#define yypush_buffer_state fortran_push_buffer_state
#endif

#ifdef yypop_buffer_state
#define fortran_pop_buffer_state_ALREADY_DEFINED
#else
#define yypop_buffer_state fortran_pop_buffer_state
#endif

#ifdef yyensure_buffer_stack
#define fortran_ensure_buffer_stack_ALREADY_DEFINED
#else
#define yyensure_buffer_stack fortran_ensure_buffer_stack
#endif

#ifdef yylex
#define fortran_lex_ALREADY_DEFINED
#else
#define yylex fortran_lex
#endif

#ifdef yyrestart
#define fortran_restart_ALREADY_DEFINED
#else
#define yyrestart fortran_restart
#endif

#ifdef yylex_init
#define fortran_lex_init_ALREADY_DEFINED
#else
#define yylex_init fortran_lex_init
#endif

#ifdef yylex_init_extra
#define fortran_lex_init_extra_ALREADY_DEFINED
#else
#define yylex_init_extra fortran_lex_init_extra
#endif

#ifdef yylex_destroy
#define fortran_lex_destroy_ALREADY_DEFINED
#else
#define yylex_destroy fortran_lex_destroy
#endif

#ifdef yyget_debug
#define fortran_get_debug_ALREADY_DEFINED
#else
#define yyget_debug fortran_get_debug
#endif

#ifdef yyset_debug
#define fortran_set_debug_ALREADY_DEFINED
#else
#define yyset_debug fortran_set_debug
#endif

#ifdef yyget_extra
#define fortran_get_extra_ALREADY_DEFINED
#else
#define yyget_extra fortran_get_extra
#endif

#ifdef yyset_extra
#define fortran_set_extra_ALREADY_DEFINED
#else
#define yyset_extra fortran_set_extra
#endif

#ifdef yyget_in
#define fortran_get_in_ALREADY_DEFINED
#else
#define yyget_in fortran_get_in
#endif

#ifdef yyset_in
#define fortran_set_in_ALREADY_DEFINED
#else
#define yyset_in fortran_set_in
#endif

#ifdef yyget_out
#define fortran_get_out_ALREADY_DEFINED
#else
#define yyget_out fortran_get_out
#endif

#ifdef yyset_out
#define fortran_set_out_ALREADY_DEFINED
#else
#define yyset_out fortran_set_out
#endif

#ifdef yyget_leng
#define fortran_get_leng_ALREADY_DEFINED
#else
#define yyget_leng fortran_get_leng
#endif

#ifdef yyget_text
#define fortran_get_text_ALREADY_DEFINED
#else
#define yyget_text fortran_get_text
#endif

#ifdef yyget_lineno
#define fortran_get_lineno_ALREADY_DEFINED
#else
#define yyget_lineno fortran_get_lineno
#endif

#ifdef yyset_lineno
#define fortran_set_lineno_ALREADY_DEFINED
#else
#define yyset_lineno fortran_set_lineno
#endif

#ifdef yywrap
#define fortran_wrap_ALREADY_DEFINED
#else
#define yywrap fortran_wrap
#endif

#ifdef yyalloc
#define fortran_alloc_ALREADY_DEFINED
#else
#define yyalloc fortran_alloc
#endif

#ifdef yyrealloc
#define fortran_realloc_ALREADY_DEFINED
#else
#define yyrealloc fortran_realloc
#endif

#ifdef yyfree
#define fortran_free_ALREADY_DEFINED
#else
#define yyfree fortran_free
#endif

#ifdef yytext
#define fortran_text_ALREADY_DEFINED
#else
#define yytext fortran_text
#endif

#ifdef yyleng
#define fortran_leng_ALREADY_DEFINED
#else
#define yyleng fortran_leng
#endif

#ifdef yyin
#define fortran_in_ALREADY_DEFINED
#else
#define yyin fortran_in
#endif

#ifdef yyout
#define fortran_out_ALREADY_DEFINED
#else
#define yyout fortran_out
#endif

#ifdef yy_flex_debug
#define fortran__flex_debug_ALREADY_DEFINED
#else
#define yy_flex_debug fortran__flex_debug
#endif

#ifdef yylineno
#define fortran_lineno_ALREADY_DEFINED
#else
#define yylineno fortran_lineno
#endif

/* First, we deal with  platform-specific or compiler-specific issues. */

/* begin standard C headers. */
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>

/* end standard C headers. */

/* flex integer type definitions */

#ifndef FLEXINT_H
#define FLEXINT_H

/* C99 systems have <inttypes.h>. Non-C99 systems may or may not. */

#if defined (__STDC_VERSION__) && __STDC_VERSION__ >= 199901L

/* C99 says to define __STDC_LIMIT_MACROS before including stdint.h,
 * if you want the limit (max/min) macros for int types. 
 */
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS 1
#endif

#include <inttypes.h>
typedef int8_t flex_int8_t;
typedef uint8_t flex_uint8_t;
typedef int16_t flex_int16_t;
typedef uint16_t flex_uint16_t;
typedef int32_t flex_int32_t;
typedef uint32_t flex_uint32_t;
#else
typedef signed char flex_int8_t;
typedef short int flex_int16_t;
typedef int flex_int32_t;
typedef unsigned char flex_uint8_t; 
typedef unsigned short int flex_uint16_t;
typedef unsigned int flex_uint32_t;

/* Limits of integral types. */
#ifndef INT8_MIN
#define INT8_MIN               (-128)
#endif
#ifndef INT16_MIN
#define INT16_MIN              (-32767-1)
#endif
#ifndef INT32_MIN
#define INT32_MIN              (-2147483647-1)
#endif
#ifndef INT8_MAX
#define INT8_MAX               (127)
#endif
#ifndef INT16_MAX
#define INT16_MAX              (32767)
#endif
#ifndef INT32_MAX
#define INT32_MAX              (2147483647)
#endif
#ifndef UINT8_MAX
#define UINT8_MAX              (255U)
#endif
#ifndef UINT16_MAX
#define UINT16_MAX             (65535U)
#endif
#ifndef UINT32_MAX
#define UINT32_MAX             (4294967295U)
#endif

#ifndef SIZE_MAX
#define SIZE_MAX               (~(size_t)0)
#endif

#endif /* ! C99 */

#endif /* ! FLEXINT_H */

/* begin standard C++ headers. */

/* TODO: this is always defined, so inline it */
#define yyconst const

#if defined(__GNUC__) && __GNUC__ >= 3
#define yynoreturn __attribute__((__noreturn__))
#else
#define yynoreturn
#endif

/* Returned upon end-of-file. */
#define YY_NULL 0

/* Promotes a possibly negative, possibly signed char to an
 *   integer in range [0..255] for use as an array index.
 */
#define YY_SC_TO_UI(c) ((YY_CHAR) (c))

/* Enter a start condition.  This macro really ought to take a parameter,
 * but we do it the disgusting crufty way forced on us by the ()-less
 * definition of BEGIN.
 */
#define BEGIN (yy_start) = 1 + 2 *
/* Translate the current start state into a value that can be later handed
 * to BEGIN to return to the state.  The YYSTATE alias is for lex
 * compatibility.
 */
#define YY_START (((yy_start) - 1) / 2)
#define YYSTATE YY_START
/* Action number for EOF rule of a given start state. */
#define YY_STATE_EOF(state) (YY_END_OF_BUFFER + state + 1)
/* Special action meaning "start processing a new file". */
#define YY_NEW_FILE yyrestart( yyin  )
#define YY_END_OF_BUFFER_CHAR 0

/* Size of default input buffer. */
#ifndef YY_BUF_SIZE
#ifdef __ia64__
/* On IA-64, the buffer size is 16k, not 8k.
 * Moreover, YY_BUF_SIZE is 2*YY_READ_BUF_SIZE in the general case.
 * Ditto for the __ia64__ case accordingly.
 */
#define YY_BUF_SIZE 32768
#else
#define YY_BUF_SIZE 16384
#endif /* __ia64__ */
#endif

/* The state buf must be large enough to hold one state per character in the main buffer.
 */
#define YY_STATE_BUF_SIZE   ((YY_BUF_SIZE + 2) * sizeof(yy_state_type))

#ifndef YY_TYPEDEF_YY_BUFFER_STATE
#define YY_TYPEDEF_YY_BUFFER_STATE
typedef struct yy_buffer_state *YY_BUFFER_STATE;
#endif

#ifndef YY_TYPEDEF_YY_SIZE_T
#define YY_TYPEDEF_YY_SIZE_T
typedef size_t yy_size_t;
#endif

extern int yyleng;

extern FILE *yyin, *yyout;

#define EOB_ACT_CONTINUE_SCAN 0
#define EOB_ACT_END_OF_FILE 1
#define EOB_ACT_LAST_MATCH 2
    
    #define YY_LESS_LINENO(n)
    #define YY_LINENO_REWIND_TO(ptr)
    
/* Return all but the first "n" matched characters back to the input stream. */
#define yyless(n) \
	do \
		{ \
		/* Undo effects of setting up yytext. */ \
        int yyless_macro_arg = (n); \
        YY_LESS_LINENO(yyless_macro_arg);\
		*yy_cp = (yy_hold_char); \
		YY_RESTORE_YY_MORE_OFFSET \
		(yy_c_buf_p) = yy_cp = yy_bp + yyless_macro_arg - YY_MORE_ADJ; \
		YY_DO_BEFORE_ACTION; /* set up yytext again */ \
		} \
	while ( 0 )
#define unput(c) yyunput( c, (yytext_ptr)  )

#ifndef YY_STRUCT_YY_BUFFER_STATE
#define YY_STRUCT_YY_BUFFER_STATE
struct yy_buffer_state
	{
	FILE *yy_input_file;

	char *yy_ch_buf;		/* input buffer */
	char *yy_buf_pos;		/* current position in input buffer */

	/* Size of input buffer in bytes, not including room for EOB
	 * characters.
	 */
	int yy_buf_size;

	/* Number of characters read into yy_ch_buf, not including EOB
	 * characters.
	 */
	int yy_n_chars;

	/* Whether we "own" the buffer - i.e., we know we created it,
	 * and can realloc() it to grow it, and should free() it to
	 * delete it.
	 */
	int yy_is_our_buffer;

	/* Whether this is an "interactive" input source; if so, and
	 * if we're using stdio for input, then we want to use getc()
	 * instead of fread(), to make sure we stop fetching input after
	 * each newline.
	 */
	int yy_is_interactive;

	/* Whether we're considered to be at the beginning of a line.
	 * If so, '^' rules will be active on the next match, otherwise
	 * not.
	 */
	int yy_at_bol;

    int yy_bs_lineno; /**< The line count. */
    int yy_bs_column; /**< The column count. */

	/* Whether to try to fill the input buffer when we reach the
	 * end of it.
	 */
	int yy_fill_buffer;

	int yy_buffer_status;

#define YY_BUFFER_NEW 0
#define YY_BUFFER_NORMAL 1
	/* When an EOF's been seen but there's still some text to process
	 * then we mark the buffer as YY_EOF_PENDING, to indicate that we
	 * shouldn't try reading from the input source any more.  We might
	 * still have a bunch of tokens to match, though, because of
	 * possible backing-up.
	 *
	 * When we actually see the EOF, we change the status to "new"
	 * (via yyrestart()), so that the user can continue scanning by
	 * just pointing yyin at a new input file.
	 */
#define YY_BUFFER_EOF_PENDING 2

	};
#endif /* !YY_STRUCT_YY_BUFFER_STATE */

/* Stack of input buffers. */
static size_t yy_buffer_stack_top = 0; /**< index of top of stack. */
static size_t yy_buffer_stack_max = 0; /**< capacity of stack. */
static YY_BUFFER_STATE * yy_buffer_stack = NULL; /**< Stack as an array. */

/* We provide macros for accessing buffer states in case in the
 * future we want to put the buffer states in a more general
 * "scanner state".
 *
 * Returns the top of the stack, or NULL.
 */
#define YY_CURRENT_BUFFER ( (yy_buffer_stack) \
                          ? (yy_buffer_stack)[(yy_buffer_stack_top)] \
                          : NULL)
/* Same as previous macro, but useful when we know that the buffer stack is not
 * NULL or when we need an lvalue. For internal use only.
 */
#define YY_CURRENT_BUFFER_LVALUE (yy_buffer_stack)[(yy_buffer_stack_top)]

/* yy_hold_char holds the character lost when yytext is formed. */
static char yy_hold_char;
static int yy_n_chars;		/* number of characters read into yy_ch_buf */
int yyleng;

/* Points to current character in buffer. */
static char *yy_c_buf_p = NULL;
static int yy_init = 0;		/* whether we need to initialize */
static int yy_start = 0;	/* start state number */

/* Flag which is used to allow yywrap()'s to do buffer switches
 * instead of setting up a fresh yyin.  A bit of a hack ...
 */
static int yy_did_buffer_switch_on_eof;

void yyrestart ( FILE *input_file  );
void yy_switch_to_buffer ( YY_BUFFER_STATE new_buffer  );
YY_BUFFER_STATE yy_create_buffer ( FILE *file, int size  );
void yy_delete_buffer ( YY_BUFFER_STATE b  );
void yy_flush_buffer ( YY_BUFFER_STATE b  );
void yypush_buffer_state ( YY_BUFFER_STATE new_buffer  );
void yypop_buffer_state ( void );

static void yyensure_buffer_stack ( void );
static void yy_load_buffer_state ( void );
static void yy_init_buffer ( YY_BUFFER_STATE b, FILE *file  );
#define YY_FLUSH_BUFFER yy_flush_buffer( YY_CURRENT_BUFFER )

YY_BUFFER_STATE yy_scan_buffer ( char *base, yy_size_t size  );
YY_BUFFER_STATE yy_scan_string ( const char *yy_str  );
YY_BUFFER_STATE yy_scan_bytes ( const char *bytes, int len  );

void *yyalloc ( yy_size_t  );
void *yyrealloc ( void *, yy_size_t  );
void yyfree ( void *  );

#define yy_new_buffer yy_create_buffer
#define yy_set_interactive(is_interactive) \
	{ \
	if ( ! YY_CURRENT_BUFFER ){ \
        yyensure_buffer_stack (); \
		YY_CURRENT_BUFFER_LVALUE =    \
            yy_create_buffer( yyin, YY_BUF_SIZE ); \
	} \
	YY_CURRENT_BUFFER_LVALUE->yy_is_interactive = is_interactive; \
	}
#define yy_set_bol(at_bol) \
	{ \
	if ( ! YY_CURRENT_BUFFER ){\
        yyensure_buffer_stack (); \
		YY_CURRENT_BUFFER_LVALUE =    \
            yy_create_buffer( yyin, YY_BUF_SIZE ); \
	} \
	YY_CURRENT_BUFFER_LVALUE->yy_at_bol = at_bol; \
	}
#define YY_AT_BOL() (YY_CURRENT_BUFFER_LVALUE->yy_at_bol)

/* Begin user sect3 */

#define fortran_wrap() (/*CONSTCOND*/1)
#define YY_SKIP_YYWRAP
typedef flex_uint8_t YY_CHAR;

FILE *yyin = NULL, *yyout = NULL;

typedef int yy_state_type;

extern int yylineno;
int yylineno = 1;

extern char *yytext;
#ifdef yytext_ptr
#undef yytext_ptr
#endif
#define yytext_ptr yytext

static yy_state_type yy_get_previous_state ( void );
static yy_state_type yy_try_NUL_trans ( yy_state_type current_state  );
static int yy_get_next_buffer ( void );
static void yynoreturn yy_fatal_error ( const char* msg  );

/* Done after the current pattern has been matched and before the
 * corresponding action - sets up yytext.
 */
#define YY_DO_BEFORE_ACTION \
	(yytext_ptr) = yy_bp; \
	yyleng = (int) (yy_cp - yy_bp); \
	(yy_hold_char) = *yy_cp; \
	*yy_cp = '\0'; \
	(yy_c_buf_p) = yy_cp;
#define YY_NUM_RULES 179
#define YY_END_OF_BUFFER 180
/* This struct is not used in this scanner,
   but its presence is necessary. */
struct yy_trans_info
	{
	flex_int32_t yy_verify;
	flex_int32_t yy_nxt;
	};
static const flex_int16_t yy_acclist[1606] =
    {   0,
      144,  144,  180,  179,  168,  179,  167,  179,  178,  179,
      179,  157,  179,  161,  179,  171,  179,  179,  160,  179,
      160,  179,  160,  179,  163,  179,  158,  179,  141,  179,
      156,  179,  160,  179,  162,  179,  165,  179,  164,  179,
      166,  179,  152,  179,  152,  179,  152,  179,  152,  179,
      152,  179,  152,  179,  152,  179,  152,  179,  152,  179,
      152,  179,  152,  179,  152,  179,  152,  179,  152,  179,
      152,  179,  152,  179,  152,  179,  152,  179,  152,  179,
      152,  179,  152,  179,  168,  179,  167,  177,  179,  178,
      179,  152,  179,  152,  179,  152,  179,  152,  179,  152,

      179,  179,  179,  175,  179,  179,  179,  150,  179,  179,
      179,  144,  179,  145,  179,  179,  167,  179,  152,  179,
      152,  179,  152,  179,  152,  179,  152,  179,  152,  179,
      152,  179,  152,  179,  152,  179,  152,  179,  152,  179,
      152,  179,  152,  179,  152,  179,  152,  179,  152,  179,
      152,  179,  152,  179,  152,  179,  152,  179,  152,  179,
      167,  177,  179,  168,  179,  160,  179,  156,  179,  152,
      179,  152,  179,  152,  179,  152,  179,  152,  179,  168,
      179,  156,  179,  168,  178,  178,  178,  147,  171,  146,
      139,   20,  155,  140,  138,   34,  156,  137,   35,   33,

       18,   36,  152,  152,  152,  152,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,   42,  152,  152,  152,
      152,  152,  152,  152,  152,  152,  152,  152,  152,  152,
      152,  152,   92,  152,  152,  152,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,  152,  152,  152,  152,
      152,  152,  168,  177,  178,  178,  178,  178,  152,  152,
      152,  152,   92,  152,  152,  175,  150,  144,  143,  152,
      152,  152,  152,  152,  152,  152,  152,  152,  152,  152,
      152,  152,  152,   42,  152,  152,  152,  152,  152,  152,

      152,  152,  152,  152,  152,  152,  152,  152,  152,   92,
      152,  152,  152,  152,  152,  152,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,  152,  152,  152,  177,
      168,  168,  176,   20,  156,  176,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,   92,  152,  152,  168,
      156,  178,  178,  142,  146,  154,  153,  154,  155,  155,
      152,  152,  152,  152,  152,  152,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,  152,  152,  152,    9,
      152,  152,  152,  152,  152,  152,  152,  152,  152,  152,

      152,  152,  104,16486,  152,  152,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,  152,  152,  152,  152,
      152,  152,  152,  152,   95,  152,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,  152,   11,  152,  152,
      152,  152,  178,  178,  178,  152,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,  152,  152,  152,  152,
        9,  152,  152,  152,  152,  152,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,  152,  152,  152,  152,

      152,  152,  152,  152,  152,  152,  152,  152,  152,  152,
      152,  152,  152,   95,  152,  152,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,   11,  152,  152,  152,
      152,  168,  168,  156,  152,  152,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,  152,  178,  178,  155,
       22,   24,   23,   26,   25,   28,   30,  152,  152,  152,
      152,  152,  152,  152,   15,  152,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,   41,   41,  152,  152,
      100,  152,  117,  152,  152,  152,  152,  152,  118,  152,

      127,  152,  152,   80,  152,  152,  152,  152,  115,  152,
      152,   94,  152,  152,  152,  152,  152,  152,  152,  152,
      152,  152,  152,  119,  152,  152,  152,  152,  116,   14,
      152,  152,   64,  152,   78,  152,  152,  152,  152,  152,
      152,  152,  152,  152,  152,   84,  152,   43,  152,  131,
      152,  152,  152,  152,  152,   73,  152,  152,  152,   77,
      152,   58,  152,  152,  152,   98,  152,  152,  152,  152,
      152,   47,  178,  178,  178,  106,  152,  152,  152,  152,
      152,  152,16459,  152,  152,  152,  152,  152,  152,  152,
       15,  152,  152,  152,  152,  152,  152,  152,  152,  152,

      152,  152,   41,  152,  152,  100,  152,  152,  152,  152,
      152,  152,  152,  152,  152,   80,  152,  152,  152,  152,
      152,  152,   94,  152,  152,  152,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,  152,  152,   14,  152,
      152,   64,  152,   78,  152,  152,  152,  152,  152,  152,
      152,  152,  152,  152,   84,  152,   43,  152,  152,  152,
      152,  152,  152,   73,  152,  152,  152,   77,  152,   58,
      152,  152,  152,   98,  152,  152,  152,  152,  152,  168,
      156,   15,  152,  106,  152,  152,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,  152,  152,16459,  178,

      178,  159,   32,   21,   29,   31,  152,  152,  152,  152,
      152,  152,  152,  152,   53,  152,  152,  152,  152,  152,
      135,  152,  152,  152,  152,  152,  152,  152,   40,  152,
      101,  152,  152,  152,  152,  152,  152,  152,  152,  109,
       88,  152,  128,  152,   94,  103,  152,  152,   96,  152,
      152,  152,  152,  152,  152,  152,  152,  120,  152,  152,
      122,  129,  152,  152,  152,  152,  152,   56,  152,  152,
      152,   81,  152,  152,  152,  152,   83,  130,  152,  152,
      152,  152,  152,  152,  152,  152,  152,  113,   59,  152,
       38,  152,   87,  152,  106,16459,  178,  178,  178,  106,

      152,   93,  152,  152, 8267,   74, 8267,  152,  152,  152,
      152,  152,  152,  152,  152,   53,  152,  152,  152,  152,
      152,  135,  152,  152,  152,  152,  152,  152,  152,   40,
      152,  101,  152,  152,  152,  152,  152,  152,  152,  152,
       88,  152,  152,  152,  152,   96,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,  152,  152,  152,  152,
      152,   56,  152,  152,  152,   81,  152,  152,  152,  152,
      152,  152,  152,  152,  152,  152,  152,  152,  152,   59,
      152,   38,  152,   87,  152,  168,  156,  106,  152,  152,
       53,  152,  152,  152,  152,  152,  152,  152,  135,  152,

      152,  152,   16,  178,   16,  178,   16,   16,  147,   16,
       16,   16,  146,   16,   16,   16,   16,   16,   16,   27,
      152,  152,  152,  152,  152,   16,  152,  152,  152,   67,
      152,  152,  152,  152,  152,  152,  152,  152,  152,   99,
      152,  152,   40,  101,  152,  152,  152,  152,  152,  134,
      152,  152,  103, 8294,  103,  152,  152,  152,  152,   70,
      152,  152,  152,  125,  152,  152,   37,  152,  152,  152,
      152,  152,  152,  152,  152,  152,  152,   90,  152,  152,
        7,  152,   79,  152,   12,  152,  152,  152,  133,  152,
      152,   89,  152,   86,  178,  178,   16,  178,  152,  152,

      152,  152,  152,  152,  152,  152,   16,  152,  152,  152,
       67,  152,  152,  152,  152,  152,  152,  152,  152,  152,
       99,  152,  152,  152,  152,  152,  152,  152,  152,  152,
      152,  152,  152,  152,   70,  152,  152,  152,  152,  152,
       37,  152,  152,  152,  152,  152,  152,  152,  152,  152,
      152,   90,  152,  152,    7,  152,   79,  152,   12,  152,
      152,  152,  133,  152,  152,   89,  152,   16,  152,  152,
       67,  152,  152,  152,  152,  152,  152,   16,  152,  152,
      152,   17,   17,  178,   17,   17,  147,   17,   17,   17,
      146,   17,   17,   17,   17,   17,   17,  110,  111,   17,

      152,  152,  152,  152,  152,   50,  152,  152,  152,  152,
      152,  107,  152,  152,  152,  152,   99,  152,  152,   76,
      152,  152,  152,  121,  152,  152, 8294,  152,   10,  152,
       54,  152,   44,  152,  152,  152,  126,   45,  152,  152,
      152,    5,  152,  114,  152,  152,   71,  152,  152,   91,
      152,    2,  152,  152,  152,  123,  132,  152,  178,   17,
      178,  152,   68,  152,  172,   17,  152,  152,  152,  152,
      152,   50,  152,  152,  152,  152,  152,  107,  152,  152,
      152,  152,  152,  152,   76,  152,  152,  152,  152,  152,
      152,   10,  152,   54,  152,   44,  152,  152,  152,   45,

      152,  152,  152,    5,  152,  152,  152,   71,  152,  152,
       91,  152,    2,  152,  152,  152,  152,  172,   17,   17,
      152,  152,   50,  152,  152,  152,  152,  152,  152,  152,
        3,  152,  152,  152,  152,  152,  152,    4,  152,  152,
      152,  152,  152,  152,   76,  152,   60,  152,  152,   69,
      152,    8,  152,   13,  152,  152,  152,  152,   85,  152,
       72,  152,  152,  152,  152,  152,  152,  178,   63,  152,
      152,  152,    3,  152,  152,  152,  152,  152,  152,    4,
      152,  152,  152,  152,  152,  152,  152,   60,  152,  152,
       69,  152,    8,  152,   13,  152,  152,  152,  152,   85,

      152,   72,  152,  152,  152,  152,  152,  152,  152,  152,
       63,  152,  152,    4,  152,  152,  138,  152,  152,  136,
      152,   46,  152,  152,  152,  152,   55,  152,  152,  152,
       62,  152,   60,  108,  152,  152,   97,  152,  112,  152,
       65,  152,  124,   66,  152,  152,  152,   63,  178,  148,
      152,  151,  152,  152,  136,  152,   46,  152,  152,  152,
      152,   55,  152,  152,  152,   62,  152,  108,  152,  152,
       97,  152,  152,   65,  152,   66,  152,  152,  152,   46,
      152,  152,  152,  148,  152,  170,  138,  152,  152,   39,
      152,   52,  152,    6,  152,  152,  152,   62,   61,  108,

      152,  152,  105,  152,    1,  152,  148,  178,  152,  152,
       39,  152,   52,  152,    6,  152,  152,  152,  152,  152,
      105,  152,    1,  152,  169,   39,  152,   52,  152,   51,
      152,  152,  152,   57,  152,  152,  105,  178,   51,  152,
      152,  152,   57,  152,  152,  170,  152,  152,  152,  178,
      152,  152,  152,  169,   19,   49,  152,  152,  152,  178,
      149,  150,   49,  152,  152,  152,  169,  169,   49,  152,
      152,  178,  152,  152,   48,  152,   82,  152,  178,   48,
      152,   82,  152,  169,   48,   82,  178,  178,  178,  178,
      178,  178,  173,  178,  173,  173,  176,  173,  177,  178,

      176,  174,  175,  174,  175
    } ;

static const flex_int16_t yy_accept[1899] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    2,    3,    3,    3,    3,    3,    4,    5,    7,
        9,   11,   12,   14,   16,   18,   19,   21,   23,   25,
       27,   29,   31,   33,   35,   37,   39,   41,   43,   45,
       47,   49,   51,   53,   55,   57,   59,   61,   63,   65,
       67,   69,   71,   73,   75,   77,   79,   81,   83,   85,
       87,   90,   92,   94,   96,   98,  100,  102,  103,  104,
      106,  107,  108,  110,  111,  112,  114,  116,  117,  119,
      121,  123,  125,  127,  129,  131,  133,  135,  137,  139,
      141,  143,  145,  147,  149,  151,  153,  155,  157,  159,

      161,  164,  166,  168,  170,  172,  174,  176,  178,  180,
      182,  184,  184,  184,  185,  186,  187,  188,  188,  189,
      189,  189,  190,  190,  190,  190,  190,  191,  191,  191,
      191,  191,  192,  192,  192,  192,  193,  193,  194,  194,
      194,  194,  194,  194,  194,  194,  194,  194,  194,  195,
      196,  197,  197,  198,  198,  199,  200,  201,  202,  203,
      204,  205,  206,  207,  208,  209,  210,  211,  212,  213,
      214,  215,  216,  217,  219,  220,  221,  222,  223,  224,
      225,  226,  227,  228,  229,  230,  231,  232,  233,  235,
      236,  237,  238,  239,  240,  241,  242,  243,  244,  245,

      246,  247,  248,  249,  250,  251,  252,  253,  254,  255,
      256,  257,  258,  259,  260,  261,  262,  263,  263,  264,
      265,  265,  265,  265,  265,  265,  265,  265,  266,  266,
      267,  268,  269,  269,  270,  271,  272,  273,  275,  276,
      276,  277,  277,  277,  278,  278,  278,  278,  279,  279,
      280,  280,  280,  280,  280,  280,  280,  281,  282,  283,
      284,  285,  286,  287,  288,  289,  290,  291,  292,  293,
      294,  296,  297,  298,  299,  300,  301,  302,  303,  304,
      305,  306,  307,  308,  309,  310,  312,  313,  314,  315,
      316,  317,  318,  319,  320,  321,  322,  323,  324,  325,

      326,  327,  328,  329,  330,  331,  332,  333,  334,  335,
      336,  337,  338,  339,  340,  340,  341,  341,  341,  342,
      343,  343,  343,  344,  345,  345,  345,  345,  345,  346,
      347,  347,  348,  349,  350,  351,  352,  353,  354,  355,
      356,  357,  359,  360,  361,  361,  361,  362,  362,  362,
      362,  363,  364,  364,  364,  364,  364,  364,  364,  364,
      364,  366,  366,  366,  366,  366,  366,  366,  366,  366,
      366,  366,  366,  366,  366,  366,  366,  366,  366,  366,
      366,  366,  366,  366,  366,  366,  366,  366,  366,  366,
      366,  366,  366,  367,  370,  370,  371,  372,  373,  374,

      375,  376,  377,  378,  379,  380,  381,  382,  383,  384,
      385,  386,  387,  387,  388,  389,  390,  392,  393,  394,
      395,  396,  397,  398,  399,  400,  401,  402,  402,  403,
      403,  405,  406,  407,  408,  409,  410,  411,  412,  413,
      414,  415,  416,  417,  418,  419,  420,  421,  422,  423,
      424,  425,  427,  428,  429,  430,  431,  432,  433,  434,
      435,  436,  437,  438,  439,  440,  441,  442,  443,  444,
      445,  446,  447,  448,  450,  451,  452,  453,  453,  453,
      453,  453,  453,  453,  453,  453,  453,  454,  455,  456,
      456,  457,  458,  459,  460,  461,  462,  462,  462,  462,

      462,  462,  462,  462,  462,  462,  462,  462,  462,  463,
      464,  465,  466,  467,  468,  469,  470,  471,  472,  473,
      474,  475,  476,  477,  478,  479,  480,  481,  483,  484,
      485,  486,  487,  488,  489,  490,  491,  492,  493,  494,
      495,  496,  497,  498,  499,  500,  501,  502,  503,  504,
      505,  506,  507,  508,  509,  510,  511,  512,  513,  514,
      516,  517,  518,  519,  520,  521,  522,  523,  524,  525,
      526,  527,  528,  529,  530,  531,  532,  533,  534,  535,
      536,  537,  539,  540,  541,  542,  542,  542,  542,  542,
      543,  543,  544,  544,  544,  544,  544,  544,  544,  545,

      545,  546,  547,  548,  549,  550,  551,  552,  553,  554,
      555,  556,  557,  558,  558,  558,  558,  558,  559,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  561,  561,
      561,  562,  562,  563,  564,  565,  566,  566,  567,  567,
      567,  568,  568,  568,  568,  568,  568,  568,  568,  568,
      568,  568,  568,  569,  570,  571,  572,  573,  574,  575,
      577,  578,  579,  580,  581,  582,  583,  584,  585,  586,
      587,  588,  590,  591,  593,  593,  594,  595,  596,  597,
      598,  599,  599,  600,  601,  601,  602,  603,  604,  606,

      607,  608,  609,  609,  610,  611,  612,  612,  614,  614,
      614,  614,  614,  615,  616,  617,  618,  619,  620,  621,
      622,  623,  624,  624,  625,  626,  627,  628,  629,  629,
      630,  632,  633,  635,  637,  638,  639,  640,  641,  642,
      643,  644,  645,  646,  648,  650,  650,  651,  652,  653,
      654,  655,  656,  658,  659,  660,  662,  664,  665,  666,
      668,  669,  670,  671,  672,  673,  673,  673,  673,  673,
      673,  673,  674,  675,  676,  676,  678,  679,  680,  681,
      682,  684,  684,  684,  684,  684,  684,  684,  684,  684,
      684,  685,  686,  687,  688,  689,  690,  691,  693,  694,

      695,  696,  697,  698,  699,  700,  701,  702,  703,  705,
      706,  708,  709,  710,  711,  712,  713,  714,  715,  716,
      718,  719,  720,  721,  722,  723,  725,  726,  727,  728,
      729,  730,  731,  732,  733,  734,  735,  736,  737,  738,
      739,  741,  742,  744,  746,  747,  748,  749,  750,  751,
      752,  753,  754,  755,  757,  759,  760,  761,  762,  763,
      764,  766,  767,  768,  770,  772,  773,  774,  776,  777,
      778,  779,  780,  780,  780,  780,  781,  781,  781,  781,
      781,  781,  782,  782,  784,  786,  787,  788,  789,  790,
      791,  792,  793,  794,  795,  796,  797,  798,  800,  800,

      800,  800,  801,  802,  802,  802,  802,  802,  802,  803,
      803,  803,  803,  803,  803,  803,  804,  805,  805,  806,
      807,  807,  807,  807,  807,  807,  807,  808,  809,  810,
      811,  812,  813,  814,  815,  817,  818,  819,  820,  821,
      823,  824,  825,  826,  827,  827,  828,  829,  829,  829,
      829,  829,  829,  831,  833,  834,  835,  836,  837,  838,
      839,  840,  840,  841,  843,  843,  844,  845,  846,  846,
      846,  846,  847,  848,  849,  851,  852,  853,  854,  855,
      856,  857,  858,  858,  859,  860,  861,  861,  862,  862,
      863,  864,  865,  866,  867,  868,  870,  871,  872,  874,

      875,  876,  877,  877,  878,  878,  879,  880,  881,  882,
      883,  884,  885,  886,  887,  888,  888,  889,  891,  893,
      895,  896,  896,  896,  896,  896,  897,  898,  899,  900,
      900,  901,  902,  903,  904,  905,  906,  907,  908,  908,
      908,  908,  908,  908,  908,  909,  910,  911,  912,  913,
      914,  915,  916,  918,  919,  920,  921,  922,  924,  925,
      926,  927,  928,  929,  930,  932,  934,  935,  936,  937,
      938,  939,  940,  941,  943,  944,  945,  946,  948,  949,
      950,  951,  952,  953,  954,  955,  956,  957,  958,  959,
      960,  961,  962,  964,  965,  966,  968,  969,  970,  971,

      972,  973,  974,  975,  976,  977,  978,  979,  980,  982,
      984,  986,  986,  986,  986,  987,  987,  987,  987,  987,
      987,  988,  988,  989,  990,  991,  993,  994,  995,  996,
      997,  998,  999, 1001, 1002, 1003, 1003, 1003, 1004, 1005,
     1007, 1007, 1008, 1010, 1010, 1011, 1012, 1014, 1014, 1014,
     1014, 1014, 1015, 1016, 1017, 1018, 1019, 1020, 1021, 1021,
     1021, 1021, 1021, 1022, 1023, 1024, 1025, 1026, 1028, 1029,
     1030, 1032, 1033, 1034, 1035, 1036, 1037, 1038, 1039, 1040,
     1040, 1040, 1042, 1043, 1044, 1045, 1045, 1045, 1045, 1046,
     1047, 1048, 1049, 1050, 1050, 1051, 1052, 1053, 1053, 1054,

     1054, 1054, 1054, 1054, 1055, 1056, 1057, 1058, 1059, 1060,
     1062, 1063, 1064, 1064, 1065, 1066, 1067, 1069, 1070, 1071,
     1072, 1073, 1074, 1075, 1076, 1077, 1078, 1080, 1081, 1083,
     1085, 1087, 1088, 1089, 1091, 1092, 1094, 1094, 1095, 1095,
     1095, 1095, 1096, 1097, 1099, 1099, 1100, 1101, 1102, 1102,
     1102, 1102, 1102, 1102, 1102, 1103, 1104, 1105, 1106, 1107,
     1109, 1110, 1111, 1113, 1114, 1115, 1116, 1117, 1118, 1119,
     1120, 1121, 1123, 1124, 1125, 1126, 1127, 1128, 1129, 1130,
     1131, 1132, 1133, 1134, 1135, 1137, 1138, 1139, 1140, 1141,
     1143, 1144, 1145, 1146, 1147, 1148, 1149, 1150, 1151, 1152,

     1154, 1155, 1157, 1159, 1161, 1162, 1163, 1165, 1166, 1168,
     1168, 1168, 1168, 1168, 1169, 1169, 1170, 1171, 1173, 1174,
     1175, 1176, 1177, 1178, 1180, 1181, 1182, 1182, 1183, 1185,
     1186, 1188, 1189, 1190, 1192, 1192, 1193, 1194, 1195, 1196,
     1197, 1198, 1198, 1198, 1198, 1198, 1198, 1199, 1199, 1200,
     1202, 1203, 1204, 1205, 1206, 1208, 1209, 1210, 1211, 1212,
     1214, 1215, 1215, 1216, 1217, 1218, 1218, 1219, 1219, 1219,
     1219, 1220, 1222, 1223, 1224, 1224, 1225, 1226, 1227, 1228,
     1228, 1228, 1229, 1231, 1233, 1235, 1236, 1237, 1237, 1238,
     1240, 1240, 1241, 1242, 1244, 1244, 1245, 1246, 1247, 1249,

     1250, 1252, 1254, 1255, 1255, 1256, 1256, 1257, 1257, 1258,
     1259, 1259, 1259, 1259, 1260, 1262, 1262, 1263, 1264, 1265,
     1265, 1265, 1265, 1266, 1266, 1266, 1268, 1269, 1270, 1271,
     1272, 1274, 1275, 1276, 1277, 1278, 1280, 1281, 1282, 1283,
     1284, 1285, 1287, 1288, 1289, 1290, 1291, 1292, 1294, 1296,
     1298, 1299, 1300, 1302, 1303, 1304, 1306, 1307, 1308, 1310,
     1311, 1313, 1315, 1316, 1317, 1318, 1318, 1319, 1319, 1320,
     1320, 1322, 1323, 1325, 1326, 1327, 1328, 1329, 1330, 1330,
     1330, 1330, 1330, 1330, 1331, 1333, 1334, 1335, 1336, 1337,
     1338, 1340, 1341, 1342, 1342, 1342, 1343, 1344, 1344, 1345,

     1345, 1346, 1346, 1347, 1349, 1350, 1352, 1354, 1354, 1356,
     1357, 1358, 1358, 1359, 1361, 1363, 1364, 1365, 1366, 1366,
     1367, 1368, 1368, 1368, 1369, 1369, 1371, 1372, 1372, 1372,
     1372, 1372, 1372, 1373, 1375, 1376, 1377, 1378, 1379, 1380,
     1382, 1383, 1384, 1385, 1386, 1387, 1388, 1390, 1391, 1393,
     1395, 1397, 1398, 1399, 1400, 1402, 1404, 1405, 1406, 1407,
     1408, 1409, 1409, 1409, 1409, 1410, 1411, 1413, 1414, 1416,
     1417, 1417, 1417, 1417, 1417, 1417, 1417, 1417, 1418, 1419,
     1420, 1422, 1424, 1425, 1426, 1427, 1429, 1429, 1429, 1430,
     1431, 1431, 1433, 1433, 1434, 1436, 1437, 1439, 1439, 1440,

     1440, 1441, 1443, 1443, 1444, 1446, 1446, 1447, 1448, 1449,
     1449, 1450, 1450, 1452, 1452, 1452, 1452, 1453, 1454, 1455,
     1457, 1459, 1460, 1461, 1462, 1464, 1465, 1466, 1468, 1470,
     1471, 1473, 1474, 1476, 1478, 1479, 1480, 1480, 1480, 1480,
     1482, 1483, 1484, 1486, 1486, 1486, 1487, 1487, 1487, 1487,
     1488, 1489, 1490, 1492, 1494, 1496, 1496, 1496, 1497, 1498,
     1499, 1499, 1500, 1501, 1502, 1502, 1503, 1503, 1505, 1507,
     1508, 1509, 1509, 1509, 1509, 1510, 1511, 1513, 1515, 1517,
     1518, 1519, 1520, 1521, 1523, 1525, 1525, 1525, 1525, 1526,
     1526, 1528, 1530, 1530, 1530, 1530, 1530, 1530, 1530, 1532,

     1532, 1532, 1532, 1532, 1533, 1534, 1536, 1536, 1537, 1538,
     1539, 1539, 1539, 1539, 1541, 1542, 1543, 1545, 1546, 1546,
     1546, 1546, 1546, 1546, 1546, 1546, 1546, 1546, 1546, 1546,
     1547, 1547, 1547, 1547, 1547, 1548, 1549, 1549, 1550, 1551,
     1551, 1551, 1551, 1552, 1553, 1554, 1554, 1554, 1554, 1554,
     1554, 1554, 1554, 1554, 1554, 1554, 1554, 1555, 1555, 1555,
     1555, 1555, 1555, 1556, 1556, 1556, 1558, 1559, 1559, 1560,
     1561, 1561, 1561, 1561, 1563, 1565, 1566, 1567, 1567, 1567,
     1567, 1567, 1567, 1567, 1568, 1568, 1568, 1568, 1569, 1569,
     1569, 1569, 1569, 1570, 1570, 1571, 1571, 1572, 1573, 1573,

     1573, 1574, 1575, 1575, 1575, 1575, 1575, 1575, 1575, 1575,
     1575, 1575, 1575, 1575, 1577, 1577, 1579, 1580, 1580, 1580,
     1582, 1584, 1584, 1584, 1584, 1584, 1584, 1584, 1585, 1585,
     1586, 1587, 1588, 1588, 1588, 1588, 1588, 1588, 1588, 1588,
     1588, 1589, 1589, 1589, 1589, 1589, 1589, 1589, 1589, 1589,
     1590, 1590, 1590, 1590, 1590, 1591, 1591, 1591, 1591, 1592,
     1592, 1592, 1592, 1593, 1594, 1595, 1595, 1596, 1596, 1596,
     1596, 1598, 1598, 1598, 1600, 1600, 1601, 1601, 1601, 1601,
     1601, 1601, 1601, 1602, 1602, 1602, 1602, 1602, 1604, 1604,
     1604, 1605, 1605, 1605, 1606, 1606, 1606, 1606

    } ;

static const YY_CHAR yy_ec[256] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    4,    5,    6,    7,    8,    9,   10,   11,   12,
       13,   14,   15,   16,   17,   18,   19,   20,   20,   20,
       20,   20,   20,   20,   20,   20,   20,   21,   22,   23,
       24,   25,    1,    1,   26,   27,   28,   29,   30,   31,
       32,   33,   34,   35,   36,   37,   38,   39,   40,   41,
       42,   43,   44,   45,   46,   47,   48,   49,   50,   51,
       52,    1,   53,    1,   54,    1,   55,   56,   57,   58,

       59,   60,   61,   62,   63,   35,   64,   65,   66,   67,
       68,   69,   70,   71,   72,   73,   74,   75,   76,   77,
       78,   79,    1,   80,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,

        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1
    } ;

static const YY_CHAR yy_meta[81] =
    {   0,
        1,    2,    3,    2,    4,    5,    4,    4,    1,    4,
        6,    7,    8,    4,    9,   10,   11,   12,   13,   14,
        1,    4,    1,    1,    1,   15,   14,   14,   14,   14,
       15,   16,   17,   17,   17,   17,   16,   17,   16,   16,
       17,   17,   16,   16,   16,   16,   17,   17,   17,   17,
       17,    1,    1,   18,   15,   14,   14,   14,   14,   15,
       16,   17,   17,   17,   16,   17,   16,   16,   17,   17,
       16,   16,   16,   16,   17,   17,   17,   17,   17,    5
    } ;

static const flex_int16_t yy_base[2066] =
    {   0,
        0,   79,    0,    0,    0,  151, 2917,   82, 2913,   86,
       89,   92,  224,  303,    0,  375, 2898,   70,  102, 9640,
       78,  113,   86,   90,  308,  311,  355,  129,  147,  137,
      447,  386,  440,  145,  146,  285,  302,  361,  444,  356,
      499,  497,  547,  594,  382,  352,  535,  495,  503,  582,
      618,  630,  639,  657,  688,  667,  692,  708,  445,  780,
      123,  538,  583,  760,  756,  811,  813, 9640, 2882, 9640,
       94, 2872, 9640,  491,  102,  110, 9640, 2860,  862,  852,
      772,  923,  871,  972,  921,  154,  729,  854,  384,  870,
      873,  992, 1022,  926,  989, 1039, 1073,  126, 1072,  968,

     1122,  316, 1056,  347, 1179,   94, 1107,   90, 1234,  433,
      437,  161,  128,  130,    0,  289,  281, 2862, 2857,  448,
      322,  459, 2855,  542, 1149,  594, 2825,  626,  735,  631,
      740, 9640, 1260, 1277, 1302, 9640, 1303, 1051,  302,  321,
      676,  917,  736,  355,  362, 1152, 1321, 1334, 9640, 9640,
     9640, 1271, 1330,  323, 9640, 9640, 9640, 9640, 9640,    0,
      638,  299,  446,  447,  480,  366,  545,  590,  553,  821,
      613,  922,  581,  971,  636,  614,  653,  680,  726, 1085,
      734,  761,  781,  791,  831, 1001, 1261,  854, 1326, 1320,
      876,  879,  731, 1025,  899,  905,  969, 1013, 1027, 1331,

     1027, 1068, 1321, 1306,  791, 1353, 1093, 1112, 1224, 1369,
      825, 1239,  876,  894,  915, 1357,  964,    0, 1372, 1431,
     2800, 1387,  978, 1246, 1276, 1368, 1445, 2797, 1449, 1402,
     1404, 1372, 1112, 1419, 1422, 1393, 1401, 1430, 1421, 2784,
     9640, 1418, 2727, 9640, 1474, 1428, 1305,  199, 2712, 2707,
     1118, 1478, 1159, 2710, 2707, 1506, 1476, 1500, 1468, 1519,
     1525, 1534, 1473, 1543, 1557, 1561, 1565, 1566, 1609, 1605,
     1613, 1600, 1618, 1608, 1639, 1649, 1669, 1662, 1672, 1676,
     1680, 1702, 1732, 1736, 1711, 1743, 1729, 1751, 1775, 1778,
     1799, 1793, 1789, 1813, 1832, 1837, 1833, 1742, 1855, 1851,

     1859, 1890, 1893, 1897, 1923, 1928, 1936, 1930, 1949, 1954,
     1955, 1980, 1987, 1995, 1622, 2031, 2052, 2702, 1495,  144,
     1570, 2648, 9640, 2644, 1526, 1516, 2012, 2045, 2060, 2079,
     1593, 2136, 2216, 2048, 2060, 1803, 2057, 2091, 2136, 2137,
     2138, 2139, 2214, 1691, 2067, 2183, 2243, 1634, 1428, 1428,
     1485, 1491, 2629, 1521, 1609, 2618, 1258, 2106, 2166, 1610,
     2598, 2594, 2241, 2246, 1851, 1761, 2246, 2273, 2538, 2303,
     2306, 1610, 1347, 1916, 1503, 2307, 2532, 2523, 2443, 2434,
     2313, 1643, 2433, 1672, 1993, 2330, 2170, 2445, 2444, 2343,
     2362, 2403, 9640, 2355, 2371, 2323, 1774, 1850, 1707, 1714,

     1857, 1751, 1809, 1844, 1868, 1871, 2229, 2343, 2001, 2005,
     1904, 1919, 2357, 2359, 1934, 1989, 2431, 1084, 2043, 2312,
     2297, 2141, 2156, 2007, 2280, 2025, 2208, 2347, 2218, 2201,
     2325, 2253, 2336, 2343, 2347, 2356, 2300, 2350, 2350, 2345,
     2394, 2371, 2362, 2380, 2380, 2422, 2381, 2391, 2379, 2392,
     2378,    0, 2411, 2398, 2404, 2410, 2411, 2413, 2412, 2425,
     2478, 2407, 2417, 2430, 2437, 2438, 2429, 2439, 2444, 2445,
     2437, 2453, 2450,    0, 2456, 2464, 2460, 2328, 2456, 2326,
     2464, 2470, 2465, 2471, 2469, 2476, 2515, 2484, 2516, 2490,
     2495, 2512, 2500, 2503, 2501, 2502, 2532, 2563, 2301, 2566,

     2573, 2291, 2277, 2577, 2584, 2590, 2276, 2273, 2543, 2550,
     2571, 2560, 2577, 2545, 2580, 2593, 2596, 2598, 2607, 2604,
     2600, 2601, 2617, 2603, 2611, 2619, 2620, 2683, 2650, 2626,
     2678, 2624, 2660, 2661, 2654, 2690, 2668, 2631, 2615, 2692,
     2696, 2675, 2697, 2705, 2706, 2698, 2700, 2708, 2736, 2719,
     2701, 2721, 2722, 2765, 2748, 2751, 2712, 2755, 2729, 2251,
     2769, 2771, 2774, 2777, 2780, 2788, 2781, 2794, 2802, 2782,
     2783, 2798, 2795, 2805, 2799, 2793, 2796, 2807, 2830, 2806,
     2827, 2243, 2831, 2814, 2841, 2878, 2885, 2196, 2890, 2946,
     2488,  330, 1951, 2895, 2866, 2850, 2903, 2907, 2940, 2874,

     3019, 3099, 2871, 2882, 2893, 2902, 3017, 2926, 2880, 2961,
     2857, 2943, 2954, 2753, 2919, 2922, 2924, 2945, 2929, 2995,
     3067, 1819, 2187, 2181, 3047, 3124, 3044, 3049, 3126, 3078,
     2977, 2110, 3143, 3146, 3050, 3061, 2171, 2101, 2093, 3138,
     9640, 2082, 9640, 9640, 9640, 9640, 3151, 9640, 3002, 2081,
     9640, 2049, 2917, 3081, 2058, 2038, 3188, 3199, 3218, 2028,
     2027, 3228, 3016, 3097, 3105, 2948, 3064, 3101, 3113,    0,
     3112, 3140, 3128, 3141, 3136, 3154, 3165, 3147, 3140, 3169,
     1985, 1947, 3174, 3238, 3313, 9640, 3176, 3188, 3195, 3179,
     3193, 3170, 9640, 3187, 3239, 9640, 3201, 3201,    0, 3206,

     3256, 3202, 3257, 9640, 3263, 3205, 3211,    0, 3263, 1939,
     1932, 3280, 3235, 3222, 3226, 3255, 3268, 3256, 3246, 3259,
     3268, 3301, 3304, 9640, 3274, 3251, 3308, 3322, 3332, 9640,
        0, 3275,    0, 3281, 3281, 3284, 3303, 3282, 3294, 3296,
     3317, 3302, 3314, 3169,    0, 3350, 9640, 3364, 3308, 3316,
     3320, 3325,    0, 3337, 3342, 3329,    0, 3338, 3350,    0,
     3380, 3353, 3360, 3363, 9640, 3365, 3352, 3372, 3373, 3371,
     3372, 3393, 3402, 3404, 3367,  219, 3382,  577, 3385, 3389,
     3431, 3410, 3455, 3412, 3437, 3463, 3467, 3471, 1937, 1931,
     3411, 3414, 3441, 3420, 3417, 3458, 3459, 1904, 3467, 3477,

     3481, 3463, 3483, 3489, 3484, 3488, 3487, 3491,  326, 3495,
     3493, 3498, 3503, 3496, 3501, 3502, 3499, 3506, 3509, 1900,
     3505, 3568, 3534, 3581, 3517, 1873, 3539, 3513, 3536, 3541,
     3562, 3565, 3574, 3583, 3577, 3614, 3586, 3585, 3622, 3631,
     1816, 3597, 1809, 3590, 3605, 3592, 3607, 3594, 3603, 3608,
     3616, 3604, 3648, 3660, 1804, 3675, 3609, 3624, 3651, 3618,
     1789, 3656, 3662, 3666, 1755, 3653, 3667, 1752, 3684, 3671,
     3680, 3688, 3700, 3712, 3476, 3746, 3705, 3725, 3732, 3689,
     3725, 3740, 3729, 3819, 3899, 3739, 3733, 3860, 3777, 3810,
     3793, 3923, 3924, 3928, 3846, 3750, 3928, 3849, 3657, 3481,

        0, 3708,    0, 3756,  532, 3926, 2614, 3853, 9640, 3946,
     3978, 1730, 3785, 3972, 4032, 9640, 9640, 1713, 9640, 9640,
     3854, 3872, 3876, 3936, 1725, 4001, 3721, 3729, 3743, 3828,
     3751, 4112, 3819, 3835,    0, 3829, 3839, 3890, 4027,    0,
     3894, 3898, 3898, 3913, 4026, 3913, 3932, 3907, 3937, 3951,
     3934, 3960,    0,    0, 3970, 4021, 4033, 4038, 4035, 3696,
     4030, 4071, 9640,    0, 4074, 9640, 4036, 9640, 4136, 4137,
     4154, 4160, 4043, 4043,    0, 4034, 4049, 4036, 4043, 4071,
     4144, 4038, 4172, 9640, 4089, 4129, 4177, 9640, 4178, 9640,
     4128, 4139, 4154, 4146, 4156,    0, 4159, 4156,    0, 4148,

     4168, 4167, 3885, 9640, 4196, 9640, 4153, 4158, 4165, 4177,
     4160, 4177, 4165, 4164, 4166, 4224, 9640,    0,    0, 4012,
     1172, 4188, 1284, 4196, 4184, 4240, 4226, 4227, 1721, 4207,
     2022, 4211, 2071, 4210, 4218, 4260, 9640, 4261, 4250, 4243,
     3880, 3889, 4102, 4264, 4247, 4251, 4255, 4268, 4264, 4331,
     4256, 4270, 1706, 4290, 4292, 4317, 4355, 1702, 4321, 4325,
     4323, 4356, 4360, 4363, 1698, 1695, 4327, 4362, 4367, 4372,
     4368, 4297, 4365, 1665, 4371, 4373, 4370, 1658, 4366, 4376,
     4369, 4374, 4375, 4378, 4405, 4407, 4409, 4390, 4414, 4415,
     4382, 4416, 1653, 4435, 4420, 1646, 4437, 4440, 4444, 4445,

     4448, 4449, 4455, 4452, 4456, 4454, 4453, 4457, 1639, 1606,
     4015, 4302, 4306, 4249, 4529, 2129, 4459, 4475, 4461, 1592,
     4535, 4466, 4501, 4605, 4685, 4255, 4457, 4479, 4513, 4512,
     4515, 4765, 4494, 4508, 4565, 4497,    0, 9640,    0,    0,
      582, 1572, 1548, 3998, 4539, 4556, 1540, 4629, 4630, 4634,
     4845, 4579, 4637, 4651, 4657, 4658, 1497, 9640, 4595, 4599,
     4712, 4716, 4642, 4721, 4925, 4435, 4599,    0, 4536, 4595,
        0, 4602, 4613, 4678, 4681, 4700, 4685, 4698, 4917, 4702,
     4701,    0, 4726, 9640, 9640, 4699, 4750, 4762, 4763, 4764,
     4753, 4760, 4796, 4802, 9640, 4773, 4761, 4871, 4736, 4872,

     4820, 4669, 1499, 4889, 4951, 4768, 4773, 4775, 4765,    0,
     4766, 4766, 4879, 9640, 4955, 4775, 4839, 4770, 4827, 4958,
     4852, 4837, 4847, 4854, 4894, 4890,    0, 4921,    0,    0,
        0, 4963, 4968, 4969, 4916,    0, 4143, 9640, 4946, 4939,
     4950, 4980, 1502, 1483, 4954, 4948, 2200, 4962, 4987, 4991,
     2295, 2810, 4739, 4907, 4995, 4998, 5050, 5003, 5004, 1482,
     5023, 5032, 1476, 5030, 5038, 5034, 5036, 5083, 5040, 5080,
     5087, 1468, 5089, 5090, 5091, 5041, 5092, 5021, 5096, 5084,
     5097, 5095, 5103, 5093, 1465, 5094, 5043, 5143, 5098, 5166,
     5114, 5115, 5170, 5129, 5131, 5134, 5138, 5123, 5158, 1461,

     5156, 1420, 1411, 1379, 5185, 5177, 5196, 5172, 1364, 4912,
     5025, 4977, 1344, 1325, 5180, 5245, 5325, 5405, 4959, 5152,
     5158, 5156, 5157,    0, 4576, 5182, 5165, 9640,    0, 1321,
     1315, 4736, 5211, 1298, 4746, 4811, 5224, 5269, 5270, 5291,
     1270, 3085, 3091, 5273, 5298, 5280, 9640, 5292, 9640,    0,
     5325, 5112, 5187, 5206,    0, 5251, 5251, 5322, 5327,    0,
     5317, 5397, 5318, 5316, 9640, 5324, 5317, 5331, 5332, 5321,
     5337,    0, 5336, 5337, 5369, 9640, 5332, 5333, 5429, 5419,
     5447, 5329,    0,    0,    0, 5341, 5406, 5443, 9640,    0,
     5433, 5398, 5403,    0, 5459, 9640, 5415, 5425,    0, 5426,

        0,    0, 5423, 5473, 5452, 5484, 9640, 5487, 9640, 5447,
     5433, 4019, 5461,  671, 1282, 1218, 5446, 4805, 5464,  777,
     5453, 5310, 9640, 5214, 5225, 1263, 5502, 5476, 5491, 5492,
     1175, 5501, 5494, 5509, 5500, 1170, 5510, 5512, 5511, 5520,
     5514, 1166, 5517, 5526, 5522, 5530, 5534, 1162, 1154, 1128,
     5533, 5537, 1094, 5538, 5541, 1074, 5535, 5543, 1053, 5550,
     1031, 1021, 5546, 5540, 5571, 5608, 1017, 5519,  990,  807,
        0, 5535,    0, 5545, 5544, 5550, 5547, 5580, 5618, 5316,
     5621, 5639, 5643, 5571,    0, 5562, 5573, 5583, 5589, 5587,
        0, 5590, 5597, 5598, 5596, 5612, 5621, 5612, 5622, 5629,

     9640, 5629, 5620,    0, 5629,    0,    0, 5685,    0, 5642,
     5667, 5630, 5635,    0,    0, 5633, 5670, 5647, 5653, 5638,
     5645, 5641, 5662, 5689, 5666,    0, 5667, 5702, 5704, 5712,
     5718,    0, 5712,  947, 5718, 5708, 5726, 5724, 5725,  937,
     5727, 5731, 5722, 5728, 5743, 5730,  905, 5737,  879,  845,
      833, 5738, 5750, 5739,  827,  819, 5734, 5790, 5752, 5740,
     5741, 5746, 5811, 5780, 5719, 5739,    0, 5743,    0, 5798,
     5814,  859, 5817, 5823, 5827, 5831, 5849, 5845, 5759, 5794,
        0,    0, 5798, 5800, 5751,    0, 5807, 5806, 5815, 5812,
     5828, 5858, 5816, 9640,    0, 5835,    0, 5872, 9640, 5826,

     5847,    0, 5875, 9640,    0, 5834, 5850, 5851, 9640, 5852,
     5880, 5845,    0, 5883, 5888, 5893,    0, 5887, 5885,  810,
      785, 5890, 5897, 5898,  774, 5899, 5904, 5913,  770, 5905,
      769, 5909,  764,  755, 5915, 5916, 5919, 3005, 5902,    0,
     5881, 5883,  747,  997, 5974, 1409, 5978, 5987, 5990,  717,
     5906, 5947,    0,    0,    0, 5898, 5916, 5934, 5922, 5999,
     6000, 9640, 9640, 5956, 5968, 5968, 5975,    0,    0, 9640,
     1348,  661, 6004, 6007, 6010, 6014,  706,  698,  686, 6005,
     6011, 6016, 6017,  650,  627, 6018, 6049, 6055, 6019, 1521,
        0,    0, 6076, 6080, 6084, 6097, 6107,  623,    0, 6037,

     6116, 6001, 6009, 6014, 6023,    0, 6066, 6069, 9640, 6052,
     6065, 1545, 6085,  603, 6100, 6102,  597, 6105, 6134, 6147,
     6143,  596,  591, 6152, 6166, 6174, 6119, 6175, 6187, 6192,
     6194, 6197, 6083, 6109, 6154, 6080, 6108, 6160, 6179, 6167,
     6164,  544, 6180, 6205, 6169, 6222, 6228,  519,  477, 6246,
     6250, 6267, 6263, 6271, 6272, 6284, 6280, 6255, 6296, 6300,
     6305, 5230, 9640, 6164, 6182,    0, 6197, 6245, 6249, 6276,
     6251, 6308,  463, 9640,  462, 6311, 6309, 6323, 6327, 6331,
     6351, 6327, 6355, 6359, 6360, 6379, 6384, 6364, 6396, 6400,
     6382, 6372, 9640, 6277, 6283, 6295, 6187, 1707,  391, 1795,

     6387, 6388, 6417, 6425, 6429, 6446, 6458, 6462, 6467, 6479,
     6475, 1834, 6304,    0, 6208,    0, 6413, 6351, 6434,  441,
      437, 6046, 6519, 6492, 6504, 6490, 6543, 6524, 6466, 9640,
     9640, 6495, 6369, 6375, 6508, 6547, 6599, 6565, 6551, 6512,
     6555, 6393, 6562, 6572, 6577, 6581, 6583, 6623, 6559, 6595,
     6461, 2353, 6627, 6616, 6589, 6380, 6594, 6603, 6641, 6652,
     6635, 6656, 6661, 6670,  416, 6678, 6682,  405, 6638, 6686,
     6690,  370, 6696, 6700,  352,  316, 6705, 6709,  212, 6713,
     6693,  208, 6718,  201, 6721, 6723, 6727, 6731,  178, 6735,
     6739,  119,  115, 6747,   83, 6751, 9640, 6794, 6812, 6830,

     6848, 6866, 6884, 6901, 6905, 6923, 6941, 6959, 6977, 6993,
     7011, 7029, 7047, 7065, 7083, 7101, 7118, 7135, 7140,   84,
     7158, 7176, 7194, 7212, 7230, 7248, 7266, 7284, 7302, 7320,
     7338, 7356, 7374, 7392, 7410, 7428, 7445, 7461, 7466, 7483,
     7501, 7519, 7537, 7542, 7560, 7573, 7588, 7606, 7624, 7642,
     7660, 7678, 7696, 7714, 7732, 7748, 7766, 7784, 7802, 7820,
     7838, 7856, 7874, 7892, 7909, 7925, 7942, 7960, 7978, 7996,
     8014, 8019, 8037, 8055, 8073, 8091, 8109, 8127, 8145, 8163,
     8181, 8199, 8217, 8235, 8253, 8271, 8289, 8307, 8325, 8342,
     8347, 8363, 8380, 8398, 8416, 8434, 8452, 8470, 8488, 8506,

     8524, 8542, 8560, 8578, 8596, 8614, 8632, 8650, 8668, 8686,
     8704, 8722, 8740, 8758, 8776, 8794, 8811, 8829, 8846, 8862,
     8867, 8884, 8902, 8920, 8938, 8956, 8974, 8992, 9010, 9028,
     9045, 9062, 9080, 9098, 9116, 9134, 9152, 9170, 9188, 9205,
     9222, 9238, 9243, 9259, 9275, 9292, 9297, 9315, 9333, 9351,
     9369, 9387, 9405, 9423, 9441, 9459, 9477, 9495, 9513, 9531,
     9549, 9567, 9585, 9603, 9621
    } ;

static const flex_int16_t yy_def[2066] =
    {   0,
     1897,    1, 1898, 1898,    1,    1, 1899, 1899, 1900, 1900,
     1898, 1898, 1897,   13,    1,    1, 1897, 1897, 1897, 1897,
     1901, 1902, 1897, 1897, 1897, 1903, 1904, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1905, 1905,
     1905, 1905, 1905, 1905, 1905, 1905, 1905, 1905, 1905, 1905,
     1905, 1905,   51, 1905, 1905, 1905, 1905, 1905, 1905, 1897,
     1897, 1906,   41, 1905, 1905, 1905, 1905, 1897, 1907, 1897,
     1907, 1908, 1897, 1908, 1908, 1897, 1897, 1909, 1897, 1910,
     1910, 1910, 1910,   83,   83,   83, 1910, 1910,   83,   83,
       83,   83, 1910,   92,   83,   83, 1910,   93, 1910, 1910,

     1897,   60, 1911,   33, 1897,   83,   83,   88,   82,   60,
       33, 1897, 1897, 1897, 1912, 1912, 1912, 1913, 1897, 1913,
     1913, 1897, 1914, 1915, 1916, 1915, 1897, 1915, 1915, 1917,
     1917, 1897, 1917, 1917, 1917, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1918, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1919,
     1919, 1919, 1919, 1919, 1919, 1919, 1919, 1919, 1919, 1919,
     1919, 1919, 1919, 1919, 1919, 1919, 1919, 1919, 1919, 1919,
     1919, 1919, 1919, 1919, 1919, 1919, 1919, 1919, 1919, 1919,
     1919, 1919, 1919, 1919, 1919, 1919, 1919, 1919, 1919, 1919,

     1919, 1919, 1919, 1919, 1919, 1919, 1919, 1919, 1919, 1919,
     1919, 1919, 1919, 1919, 1919, 1919, 1919, 1920,   60, 1897,
     1921, 1897, 1897, 1897, 1897, 1897, 1897, 1922, 1897, 1922,
     1922, 1922, 1897, 1919, 1919, 1919, 1919, 1919, 1919, 1923,
     1897, 1923, 1924, 1897, 1924, 1924, 1924, 1897, 1925, 1897,
     1897, 1897, 1897, 1926, 1927, 1897,   88,   88,  258,  258,
      258,  258,  258,  258,  258,  258,  258,  258,  258,  258,
      258,  258,  258,  258,  258,  258,  258,  258,  258,  258,
      258,  258,  258,  258,  258,  258,  258,  258,  258,  258,
      258,  258,  258,  258,  258,  258,  258,  258,  258,  258,

      258,  258,  258,  258,  258,  258,  258,  258,  258,  258,
      258,  258,  258,  258, 1897, 1897, 1897, 1928,  219,  319,
     1897, 1929, 1897, 1929, 1929, 1929, 1897, 1897, 1897, 1897,
     1929, 1930, 1930,  333,  333,  333,  333,  333,  333,  258,
      258,  258,  258,  219, 1897, 1897, 1897, 1897, 1897, 1897,
     1931, 1931, 1932, 1932, 1932, 1933, 1934, 1934, 1934, 1934,
     1897, 1935, 1936, 1936, 1897, 1937, 1897, 1938, 1939, 1938,
     1938, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1940, 1941, 1897,
     1897, 1942, 1897, 1943, 1897, 1897, 1944, 1944, 1944, 1944,

     1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944,
     1944, 1944, 1897, 1944, 1944, 1944, 1944, 1944, 1944, 1944,
     1944, 1944, 1944, 1944, 1944, 1944, 1944, 1897, 1944, 1897,
     1945, 1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944,
     1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944,
     1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944,
     1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944,
     1944, 1944, 1944, 1944, 1944, 1944, 1944, 1946, 1897, 1947,
     1897, 1897, 1897, 1897, 1897, 1897, 1948, 1948, 1948, 1897,
     1944, 1944, 1944, 1944, 1944, 1944, 1949, 1950, 1951, 1897,

     1897, 1952, 1953, 1897, 1897, 1897, 1954, 1955, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1897, 1897, 1957, 1897, 1897,
     1897,  590, 1897, 1897, 1958, 1958, 1897, 1897, 1897, 1958,

     1959, 1959,  602,  602,  602,  602,  602,  602,  602, 1956,
     1956, 1956, 1956, 1897, 1897, 1897, 1897, 1960, 1960, 1961,
     1961, 1962, 1963, 1964, 1963, 1963, 1965, 1965, 1965, 1897,
     1897, 1966, 1967, 1967, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1968, 1969, 1897, 1897, 1897, 1970,
     1971, 1897, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972,
     1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972,
     1897, 1972, 1972, 1972, 1897, 1897, 1972, 1972, 1972, 1972,
     1972, 1897, 1897, 1972, 1897, 1897, 1972, 1972, 1972, 1972,

     1972, 1972, 1897, 1897, 1972, 1972, 1897, 1972, 1973, 1974,
     1975, 1973, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972,
     1972, 1972, 1897, 1897, 1972, 1972, 1972, 1972, 1897, 1897,
     1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972,
     1972, 1972, 1972, 1972, 1972, 1897, 1897, 1972, 1972, 1972,
     1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972,
     1972, 1972, 1972, 1972, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1976, 1976, 1976, 1897, 1972, 1972, 1972, 1972, 1972,
     1972, 1977, 1978, 1978, 1897, 1897, 1897, 1897, 1979, 1980,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,

     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1981,
     1981, 1897, 1981, 1982, 1982,  885,  885,  885,  885,  885,
      885,  885,  885,  885, 1956, 1956, 1956, 1956, 1897, 1897,

     1983, 1984, 1985, 1986, 1987, 1988, 1989, 1897, 1897, 1897,
     1990, 1991, 1992, 1993, 1994, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1995, 1897, 1972, 1972, 1972, 1972,
     1972, 1996, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972,
     1972, 1972, 1972, 1972, 1897, 1972, 1972, 1897, 1897, 1897,
     1897, 1897, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972,
     1972, 1897, 1897, 1972, 1897, 1897, 1972, 1897, 1997, 1998,
     1999, 2000, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972,
     1972, 1972, 1897, 1897, 1972, 1972, 1897, 1897, 1897, 1897,
     1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972,

     1972, 1972, 1897, 1897, 1897, 1897, 1972, 1972, 1972, 1972,
     1972, 1972, 1972, 1972, 1972, 1897, 1897, 1972, 1972, 1972,
     1897, 1897, 1897, 1897, 1897, 1897, 2001, 2001, 2002, 1897,
     1897, 1972, 1897, 1972, 1972, 1897, 1897, 1897, 2003, 2004,
     1897, 1897, 1897, 1897, 1956, 1956, 1956, 1956, 1956, 2005,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,

     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 2006, 2007,
     1897, 2006, 2006, 2008, 2008, 1125, 1125, 1125, 1125, 1125,
     1125, 2009, 1125, 1956, 1956, 1897, 2010, 1897, 2011, 2012,
     2013, 2014, 1897, 2015, 2016, 2016, 1897, 1897, 1897, 2017,
     2018, 1897, 2019, 1897, 2020, 2020, 2021, 1897, 1897, 1897,
     1897, 1897, 1972, 1972, 2022, 1972, 1972, 1972, 1972, 1972,
     1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1897,
     1897, 1972, 1972, 1897, 1897, 1897, 1897, 1897, 1972, 1972,
     1972, 1972, 1972, 1897, 1897, 1972, 1972, 2023, 2023, 2024,

     2025, 2026, 2025, 2026, 2026, 1972, 1972, 1972, 1972, 1972,
     1972, 1972, 1897, 1897, 1972, 1972, 1972, 1972, 1972, 1972,
     1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972,
     1972, 1972, 1972, 1972, 1972, 1972, 1897, 1897, 1897, 1897,
     1897, 2027, 2028, 2027, 1897, 1972, 1972, 1972, 2029, 2030,
     1897, 2031, 1897, 1897, 1956, 1956, 2032, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,

     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1897,
     2031, 1897, 2033, 2034, 2034, 2035, 2036, 2036, 1318, 1318,
     1318, 1318, 1318, 1318, 1956, 1956, 1897, 1897, 2037, 2038,
     1897, 2039, 2039, 1897, 2040, 1897, 2041, 1897, 2042, 2042,
     2043, 1897, 2044, 1897, 1897, 1897, 1897, 1897, 1897, 1972,
     1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972,
     1972, 1897, 1972, 1972, 1897, 1897, 1972, 1897, 1897, 1897,
     1972, 1972, 1972, 1972, 1897, 1897, 1972, 1972, 2025, 2025,
     2026, 1972, 1972, 1972, 1972, 1972, 1972, 1897, 1897, 1972,
     1897, 1972, 1972, 1972, 1897, 1897, 1972, 1972, 1972, 1972,

     1972, 1972, 1972, 1897, 1972, 1897, 1897, 1897, 1897, 1972,
     1897, 1897, 1897, 2027, 2027, 1897, 1972, 1897, 1972, 2029,
     2030, 1897, 1897, 1897, 2045, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1897, 2046, 1897, 2034, 2034,
     1318, 1318, 1318, 1318, 1318, 1318, 1318, 1956, 1897, 1897,
     1897, 1897, 2044, 1972, 1972, 1972, 1972, 1972, 1972, 1972,
     1972, 1972, 1972, 1897, 1897, 1972, 1972, 1897, 1972, 1897,

     1897, 1897, 1972, 1972, 1972, 1972, 1972, 2025, 1972, 1972,
     1972, 1897, 1972, 1972, 1972, 1972, 1972, 1972, 1897, 1972,
     1972, 1897, 1897, 2027, 1897, 1972, 1972, 2029, 2030, 1897,
     1897, 2047, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1897, 1897, 2034, 1318, 1318, 1318, 1318, 1318, 1956,
     1897, 2048, 1897, 1897, 1897, 2049, 1897, 1897, 1972, 1972,
     1972, 1972, 1972, 1972, 1972, 1972, 1897, 1897, 1972, 1972,
     1897, 1972, 1897, 1897, 1972, 1972, 1972, 1897, 1897, 1897,

     1972, 1972, 1897, 1897, 1972, 1897, 1972, 1972, 1897, 1897,
     2027, 1897, 1972, 2029, 2030, 1897, 2047, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1897, 2050, 2034, 1318,
     1318, 1318, 1956, 2048, 2048, 2048, 1897, 2049, 2049, 2049,
     1972, 1972, 1972, 1972, 1972, 1897, 1897, 1972, 1972, 1897,
     1897, 1897, 1897, 1972, 1897, 1972, 1897, 1972, 1972, 1897,
     2027, 1897, 2029, 2030, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 2050, 1897, 2050, 2050, 2034,
     1318, 1318, 2048, 2048, 2051, 2049, 1897, 2049, 1972, 1897,

     1897, 1897, 1897, 1972, 1972, 1972, 1897, 1972, 1897, 2027,
     1897, 2029, 2030, 1956, 1956, 1956, 1956, 1956, 1897, 1897,
     1897, 2052, 2053, 2050, 2050, 2054, 2034, 2051, 2051, 2051,
     1897, 1897, 1897, 1897, 1972, 1972, 1897, 1972, 2027, 1897,
     2029, 2055, 1956, 1956, 1956, 1897, 1897, 2052, 2053, 2050,
     2050, 2050, 2056, 2057, 2054, 2054, 2054, 2034, 2051, 2048,
     2051, 1897, 1897, 1897, 1897, 1972, 1972, 1897, 1972, 2027,
     1897, 2029, 2055, 1897, 1956, 1956, 1956, 1897, 1897, 2050,
     2050, 2056, 2056, 2056, 2057, 1897, 2057, 2057, 2054, 2050,
     2054, 2034, 1897, 1897, 1972, 1897, 1972, 2027, 1897, 2029,

     1956, 1956, 1897, 1897, 2050, 2050, 2056, 2050, 2056, 2057,
     2058, 2034, 1897, 1972, 1897, 1972, 2027, 1897, 2029, 1956,
     1956, 1897, 2050, 2050, 2050, 2058, 2058, 2058, 2034, 1897,
     1897, 2027, 1897, 2029, 1897, 2050, 2059, 2058, 2058, 2034,
     2027, 1897, 2029, 1897, 2050, 2054, 2050, 2050, 2034, 2027,
     1897, 2029, 2050, 2034, 2027, 1897, 2029, 2034, 2027, 1897,
     2029, 2034, 2027, 1897, 2060, 1897, 1897, 2061, 2029, 2034,
     1897, 2062, 1897, 1897, 2063, 2060, 1897, 1897, 2061, 1897,
     2029, 2062, 1897, 2063, 2029, 2029, 2029, 1897, 2064, 1897,
     1897, 2065, 2064, 1897, 2065, 1897,    0, 1897, 1897, 1897,

     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,

     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897
    } ;

static const flex_int16_t yy_nxt[9721] =
    {   0,
       18,   19,   20,   19,   21,   22,   18,   23,   24,   25,
       26,   27,   28,   29,   28,   30,   28,   31,   32,   33,
       34,   35,   36,   37,   38,   39,   40,   41,   42,   43,
       44,   45,   46,   47,   46,   48,   49,   50,   51,   52,
       53,   46,   54,   55,   56,   57,   46,   58,   46,   46,
       59,   28,   28,   28,   39,   40,   41,   42,   43,   44,
       45,   46,   47,   48,   49,   50,   51,   52,   53,   46,
       54,   55,   56,   57,   46,   58,   46,   46,   59,   18,
       60,   61,   60,   62,   70, 1896,   71,   74,   73,   74,
       76,   77,   76,   76,   77,   76,  241,  478,  478,   78,

      112,  242,   78,  114,  244,  114,   63,   64,  116,   71,
       65,  248,   66,  248,  113,   75,  112, 1894,  119,  340,
      112, 1896,  117,   67,  227,  220,  227,  221,  342,  112,
      113,  114,  112,  114,  113,   63,   64,  116,   71,   65,
      247,   66,  113,  120,   75,  112,  113,  592,  340,  112,
      117,   67,   60,   61,   60,   62,  342,  121,  113,  112,
      136,  112,  113,  593,  311,  155,  257,  112,  247,  312,
      350,  257,  120,  113,  113,  112,  112,  112,   63,   64,
     1894,  113,   65,  257,   66,  121,  349,  257,  112,  113,
      113,  113,  311,  283,  257,   67,  112,  312,  350,  257,

      248,  113,  248, 1877,  112,  112,  112,   63,   64,  113,
     1883,   65,  257,   66, 1880,  349,  257,  113,  113,  113,
     1031,  283, 1031,   67,   18,   19,   79,   19,   21,   22,
       18,   23,   24,   25,   26,   27,   28,   29,   28,   30,
       28,   31,   32,   33,   34,   35,   36,   37,   38,   80,
       81,   82,   83,   84,   85,   86,   87,   88,   87,   89,
       90,   91,   92,   93,   94,   87,   95,   96,   97,   98,
       87,   99,   87,   87,  100,   28,   28,   28,   80,   81,
       82,   83,   84,   85,   86,   87,   88,   89,   90,   91,
       92,   93,   94,   87,   95,   96,   97,   98,   87,   99,

       87,   87,  100,   18,   60,  101,  102,   62,  156,  122,
      122,  122,  123,  125,  351,  112,  103,  319, 1877,  320,
      126,  127,  104,  352,  399,  157,  158,  119,  256,  113,
      105,  106,  112,  876,  107,  321,  108,  395,  112,  395,
      375,  128,  396,  351,  112,  809,  113,  109,  327,  877,
      328,  352,  113,  399, 1877,  129,  130,  113,  130,  105,
      106,  112,  376,  107,  355,  108,  329,  112,  375,  131,
      128,  131, 1883,  132,  113,  109,  110,   61,  110,   62,
      113,  166,  162,  129,  159,  134,  162,  146,  147,  146,
      376,  112,  355,  403,  111,  148,  165,  383,  149,  135,

      165,  384,   63,   64,  150,  113,   65, 1880,   66,  151,
      166,  162,  162,  257,  134,  162,  112,  288, 1877,   67,
      112,  186,  403,  257,  165,  383,  165,  135,  165,  384,
      113,   63,   64,  113,  344,   65,  344,   66,  346,  256,
      346,  162,  257,  256, 1818,  112,  288,   67,  137,  186,
      137,  257,  345,  119,  165,  218,  347,  152,  113,  153,
      122,  122,  122,  123,  256, 1774,  138,  348,  154,  154,
      112,  161,  139,  354,  162,  162,  140,  112,  141, 1720,
      163,  154,  400,  142,  113,  143,  144,  164,  165,  165,
      401,  113,  245,  244,  245,  145,  348,  154,  154,  112,

      161,  139,  354,  162,  162,  140,  112,  141,  163,  154,
      400,  142,  113,  143,  144,  164,  165,  165,  401,  113,
      246, 1720,  402,  145,  167,  162,  172,  162,  191,  162,
      173,  168,  192,  162,  118,  169,  174, 1143,  170,  165,
      229,  165,  193,  165,  125,  230, 1774,  165,  171,  246,
      402,  126,  127,  167,  162,  172,  162,  191,  162,  173,
      168,  192,  162,  169,  174,  187,  170,  165,  231,  165,
      193,  165,  188,  189,  190,  165,  171,  162, 1033,  165,
     1033,  404,  232,  175,  118,  176,  177, 1331,  178,  179,
      233,  165,  406, 1720,  187,  180,  125,  231, 1720,  256,

      188,  189,  190,  126,  127,  256,  162,  165,  234,  404,
      232,  175,  162,  176,  177,  405,  178,  179,  412,  165,
      406,  194,  235,  180,  162,  662,  165,  181,  125,  256,
      182,  183,  130,  184,  130,  126,  127,  234,  165,  185,
      409,  162,  417,  195,  405,  131,  412,  131,  162,  194,
      235,  363,  256,  162,  165,  196,  181,  197,  182,  183,
      162,  184,  165,  198,  202,  397,  165,  185,  199,  409,
      200,  417,  195,  229,  165,  201,  160,  162,  203,  416,
      363,  204,  398,  196,  205,  197,  206,  162,  256,  162,
      165,  198,  212,  202,  397,  418,  199,  162,  200,  213,

      256,  165,  165,  201,  160,  377,  203,  416,  256,  204,
      398,  165,  205,  207, 1711,  206,  162,  208,  162,  662,
      378,  212,  162,  418, 1524,  419,  162,  209,  213,  165,
      214,  256,  210,  211,  377,  215,  165,  125,  162,  165,
      216, 1897,  207, 1897,  126,  127,  208,  162,  378,  256,
      217,  162,  165,  419, 1897,  209, 1897,  256,  214,  259,
      210,  211,  442,  215,  165,  381,  256,  162,  420,  216,
      423,  256,  256,  262,  256,  382,  256,  364,  217,  241,
      165,  219,  220,  219,  221,  236,  162,  256,  259,  172,
      162,  442,  237,  173,  381,  186,  420,  263,  423,  174,

      165,  262,  259,  382,  165,  364,  424,  222,  223,  323,
      112,  224,  256,  225,  236,  162,  262,  459,  172,  162,
      237,  256,  173,  186,  226,  425,  263,  174,  165,  256,
     1528,  259,  165,  426,  424,  256,  222,  223,  212,  112,
      224,  187,  225,  162,  262,  213,  459,  256,  188,  238,
      190,  470,  226,  425,  256,  165,  256,  165,  407,  408,
     1564,  426,  239,  251,  252,  253,  254,  212, 1645,  427,
      187, 1646,  162,  256,  213,  255,  188,  238,  190,  258,
      470,  256,  259,  165,  284,  165,  407,  408,  260,  255,
      239,  285,  286,  287,  432,  261,  262,  427,  262,  289,

      269,  259,  257,  257,  270,  472,  257,  256,  258,  290,
      271,  259,  291,  284,  440,  262,  260,  441,  255,  285,
      286,  287,  432,  261,  262,  256,  262,  473,  289,  269,
      259,  257,  257,  270,  472,  257,  445,  290,  271,  256,
      291,  446,  440,  262,  474,  441,  379,  410,  264,  256,
      257,  299,  411,  259,  278,  265,  473,  279,  280,  266,
      281,  380,  267,  257,  445,  300,  282,  262,  301,  446,
      256,  302,  268,  474,  413,  379,  410,  264,  218,  257,
      299,  411,  259,  278,  265,  279,  280,  266,  281,  380,
      267,  257,  323,  300,  282,  262,  301,  477,  259,  302,

      268,  257,  428,  483,  428,  257, 1645,  447,  272, 1646,
      273,  274,  262,  275,  276,  414,  415,  292,  303,  229,
      277,  257,  257,  256,  256,  257,  477,  259,  257,  293,
      257,  294,  483,  256,  257,  447,  272,  295,  273,  274,
      262,  275,  276,  414,  415,  429,  292,  303,  277,  448,
      257,  257,  259,  443,  257,  256,  257,  293,  323,  294,
      296,  444,  297,  449,  304,  295,  262,  298,  305,  324,
      138,  452,  257,  429,  256,  256,  256,  448,  306,  374,
      374,  259,  443,  307,  308,  692,  325,  692,  296,  444,
      297,  449,  374,  304,  262,  298,  256,  305,  309,  452,

      326,  257,  259,  259,  313,  310,  306,  693,  374,  374,
      453,  307,  308,  454,  314,  325,  262,  262,  421,  500,
      374,  500,  254,  315,  316,  317,  318,  309,  326,  422,
      256,  259,  259,  313,  310,  255,  257,  490,  453,  465,
      257,  454,  314,  341,  262,  262,  283,  421,  466,  255,
      358,  359,  358,  146,  147,  146,  256,  422,  360,  361,
      500,  148,  501,  254,  256,  257,  490,  465,  256,  257,
      150,  341,  256, 1031,  283, 1031,  466,  256,  255,  322,
      322,  330,  322,  322,  322,  322,  331,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  332,  322,

      322,  322,  322,  322,  333,  332,  332,  332,  332,  334,
      332,  335,  332,  332,  332,  336,  332,  332,  337,  332,
      332,  332,  332,  338,  332,  332,  332,  332,  339,  332,
      322,  322,  332,  333,  332,  332,  332,  332,  334,  332,
      335,  332,  332,  336,  332,  332,  337,  332,  332,  332,
      332,  338,  332,  332,  332,  332,  339,  332,  322,  309,
      359,  365,  430,  365,  430,  256,  310,  622,  361,  467,
      257, 1525,  431,  257, 1897,  366, 1897,  367,  365,  368,
      365,  471,  484,  343,  229, 1033,  399, 1033,  309,  631,
      394, 1897,  366, 1897,  367,  310,  368,  467,  257,  154,

      154,  257,  370,  365,  137,  365,  137,  244,  362,  471,
      484,  343,  154,  369,  485,  399, 1897,  366, 1897,  367,
      353,  368,  385,  386,  387,  388,  119,  323,  154,  154,
      369,  370,  372,  498,  389,  390,  391,  390,  392,  457,
      154,  373,  485,  148,  371,  458,  323,  152,  389,  153,
      229,  145,  150,  433,  455,  369,  437,  438,  154,  154,
      450,  372,  498,  439,  456,  434,  256,  435,  457,  373,
      436,  154,  371,  458,  229,  451,  636,  389,  460,  145,
      461,  256,  433,  455,  437,  438,  475,  154,  154,  450,
      476,  439,  456,  434,  468,  435,  462,  463,  436,  154,

      464, 1710, 1897,  451,  229,  636,  229,  460,  469,  461,
      350,  402,  481,  256,  489,  475,  479,  486, 1645,  476,
      241, 1646,  256,  468,  462,  463,  482,  487,  464,  488,
      244, 1897,  227,  220,  227,  221,  469,  493,  350,  402,
      494,  481,  489,  497,  479,  486,  227,  220,  227,  221,
      227,  220,  227,  221,  482,  404,  487,  433,  488,  407,
      492,  496,  491,  256,  616,  493,  247,  256,  494,  434,
      256,  435,  497,  617,  495,  245,  244,  245,  256,  251,
      252,  253,  254,  404,  256,  229,  433,  407,  492,  496,
      491,  255,  616,  511,  247,  257,  590,  434,  590,  435,

      515,  617,  495,  246,  229,  255,  257,  504,  505,  506,
      507, 1379,  257,  257,  257,  257,  631,  257,  323,  508,
      257,  618,  511,  323,  257,  591,  119,  509,  323,  515,
      257,  639,  246,  508,  255,  257,  619,  257,  257,  257,
      257,  257,  257,  257,  510,  257,  257,  241,  257,  618,
      362,  595,  257,  353,  591,  512,  509,  620,  596,  257,
      639,  257,  508,  257,  619,  257,  257,  257,  513,  257,
      257,  327,  510,  594, 1727,  257,  514,  119,  257,  516,
      595,  257,  517,  512,  257,  620,  596,  257,  257,  593,
      257,  257,  257,  521,  323,  323,  513,  257, 1741,  257,

      518,  257,  519,  520,  514,  257,  257,  516,  256,  257,
      257,  517,  359,  257,  119,  257,  413,  257,  600,  622,
      361,  257,  521,  586,  220,  586,  318,  257,  518,  257,
      519,  520,  257,  257,  522,  257,  257,  257,  257,  523,
      257,  256,  524,  527,  257,  257,  528,  600,  256,  257,
      529,  635,  257,  257,  621,  256,  257,  525,  526,  349,
      256,  257,  257,  522,  257,  257,  257,  256,  523,  257,
      524,  527,  257,  615,  257,  528,  257,  257,  529,  635,
      257,  257,  621,  257,  530,  525,  526,  650,  349,  257,
      257,  531,  344,  257,  344,  257,  257,  256,  534,  257,

      256,  615,  532,  257,  256,  257,  257,  257,  256,  229,
      345,  257,  530,  533,  652,  650,  257,  535,  257,  531,
      536,  257,  537,  229,  257,  257,  534,  926,  257,  257,
     1158,  532,  257,  428,  257,  428,  257,  430,  257,  430,
      538,  533,  652,  665,  257,  535,  257,  431,  536, 1149,
      537,  540,  257,  666,  256,  257,  257,  256,  257,  257,
     1817,  511,  627,  257,  627,  545,  546,  257,  538,  257,
      541,  665,  547,  257,  257,  628,  539,  628,  257,  540,
      257,  666,  542,  257,  543,  257,  560,  544,  257,  548,
      511,  256,  257,  545,  546,  257,  668,  241,  257,  541,

      547,  257,  257,  663,  539,  257,  256,  257,  257,  550,
      542,  256,  543,  549,  560,  544,  257,  548,  256,  257,
      257,  359,  257,  257,  668,  554,  257,  551,  622,  361,
      553,  257,  663,  257,  257,  552,  323,  257,  550,  332,
      257,  549,  605,  257,  669,  257,  332,  257, 1819,  257,
      257,  555,  365,  554,  365,  257,  551,  257,  553,  257,
      257,  257,  558,  552,  257,  257,  366,  332,  556,  257,
      605,  257,  669,  557,  332,  256,  257,  559,  257,  555,
      670,  257,  257,  664,  563,  257,  257, 1829,  257,  257,
      667,  558,  565,  257,  564,  257,  556,  561,  566,  257,

      562,  557,  256,  257,  257,  559,  256,  257,  670,  257,
      671,  257,  664,  563,  672,  257,  567,  257,  568,  667,
      569,  565,  564,  257,  257,  561,  566,  257,  562,  679,
      637,  257,  637,  505,  257,  638,  570,  571,  671,  505,
      572,  257,  672,  573,  970,  567,  257,  568,  680,  569,
      257,  969,  327,  257,  878,  257,  578,  257,  679,  574,
      683,  576,  257,  257,  570,  571,  682,  257,  572,  257,
      877,  573,  257,  575,  257,  577,  257,  680,  514,  257,
      257,  257,  257,  580,  257,  578,  257,  574,  581,  683,
      576,  579,  257,  257,  653,  257,  653,  388,  257,  257,

      257,  575,  257,  577,  681,  257,  514,  257,  257,  582,
      257,  257,  580,  597,  257,  597,  583,  581,  684,  579,
      584,  257,  257, 1031,  257, 1031,  257,  257,  585,  662,
      926,  257,  315,  316,  317,  318,  257,  677,  582,  257,
      386,  678,  591,  257,  255,  583,  597,  684,  598,  584,
      702,  257,  257,  586,  220,  587,  318,  585,  255,  257,
      386,  327,  705,  594,  593,  677,  920,  257,  346,  678,
      346,  591, 1033,  603, 1033,  591,  694,  152,  702,  599,
      504,  505,  506,  507,  332,  604,  345,  255,  154,  154,
      705,  332,  508,  332,  606,  607,  332,  614,  919,  917,

      332,  154,  603,  332,  591,  694,  508,  358,  359,  358,
      916,  365,  332,  365,  604,  360,  361,  154,  154,  332,
      638,  332,  606,  607,  332,  366,  614,  332,  332,  154,
      327,  332,  327,  608,  332,  508,  322,  322,  330,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  332,  322,  322,  322,  322,
      322,  608,  332,  609,  257,  257,  541,  358,  359,  358,
      700,  653,  332,  654,  388,  360,  361,  611,  542,  332,
      543,  610,  257,  612,  346,  701,  346,  322,  322,  125,
      638,  127,  609,  257,  257,  541,  126,  127,  589,  700,

      332, 1418,  430, 1418,  430,  611,  542,  332,  543,  610,
      257,  612,  431,  614,  701,  322,  322,  322,  330,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  706,  322,  322,  322,  322,
      322,  257,  614,  125,  346,  256,  346,  365,  125,  365,
      126,  127,  601,  256,  613,  126,  127,  708,  257,  602,
      152,  366,  347,  367,  706,  630,  673,  322,  322,  674,
      257,  154,  154,  614,  365,  505,  365,  625,  505,  252,
      601,  703,  613,  703,  154,  708,  257,  602,  366,  713,
      367,  626,  368,  252,  673,  322, 1422,  674, 1422,  369,

      154,  154,  614,  704,  365,  625,  365,  365,  640,  365,
      640,  250,  154,  695,  647,  695,  647,  713,  366,  626,
      367,  366,  368,  367,  641,  368,  369,  710,  229,  719,
      648,  385,  386,  387,  388,  696,  711,  712,  765,  633,
      698,  699,  396,  389,  390,  391,  390,  392,  428,  697,
      428,  634,  148,  642,  649,  241,  369,  389,  719,  369,
      413,  150,  413,  657,  658,  659,  660,  633,  698,  699,
      675,  148,  714,  722,  394,  389,  681,  697,  682,  634,
      150,  642,  649,  374,  374,  717,  389,  676,  715,  389,
      396,  707,  716,  720,  721,  723,  374,  723,  718,  675,

      714,  413,  722,  414,  725,  662, 1857,  726,  727,  728,
      731,  735,  374,  374,  717,  676,  715,  724,  389,  707,
      716,  720,  721,  729,  374,  729,  718,  732,  733,  413,
      734,  414,  685,  725,  685,  726,  736,  727,  728,  731,
      735,  737,  738,  739,  742,  730,  386,  386,  743,  740,
      651,  646,  750,  744,  686,  732,  733,  741,  734,  687,
      645,  745,  751,  752,  688,  736,  753,  754,  759,  737,
      738,  755,  739,  742,  689,  690,  743,  740,  691,  746,
      750,  746,  744,  756,  757,  741,  229,  758,  687,  745,
      751,  760,  752,  688,  761,  753,  754,  759,  762,  755,

      763,  747,  689,  690,  764,  486,  691,  766,  767,  768,
      769,  756,  757,  770,  748,  758,  771,  229,  229,  760,
      773,  775,  761,  749,  776,  778,  762,  875,  763,  779,
      780,  781,  764,  486,  241,  766,  767,  768,  769,  675,
      644,  770,  748,  718,  771,  256,  772,  256,  773,  643,
      775,  749,  256,  776,  778,  875,  777,  631,  779,  780,
      781,  774,  256,  782,  783,  244,  783,  785,  675,  785,
      254,  718,  791,  256,  785,  772,  786,  254,  787,  256,
      787,  507,  256,  792,  777,  504,  505,  506,  507,  774,
      796,  787,  782,  788,  507,  256,  784,  508,  256,  794,

      256,  791,  256,  256,  127,  256,  256,  793,  623,  256,
      795,  508,  792,  256,  413,  797,  125,  256,  796,  256,
      122,  256,  256, 1146, 1147,  784,  256,  794,  256,  798,
      809,  803,  808,  256,  119,  793,  805,  806,  799,  795,
      508,  800,  807,  797,  801,  810,  323,  802,  804,  811,
      323,  692,  256,  692,  826,  525,  256,  798,  825,  817,
      803,  808,  256,  256,  805,  806,  799,  819,  820,  800,
      256,  807,  801,  693,  810,  802,  804,  256,  811,  695,
      256,  695,  826,  525,  685,  256,  685,  825,  817,  821,
      822,  703,  256,  703,  256,  819,  820,  823,  256,  256,

      256,  696,  256,  256,  589,  824,  686,  256,  256,  252,
      256,  812,  252,  704,  256,  818,  813,  499,  821,  822,
      829,  256,  250,  256,  256,  823,  814,  815,  827,  244,
      816,  256,  828,  824,  831,  833,  836,  723,  256,  723,
      812,  834,  830,  818,  835,  813,  838,  832,  829,  839,
      256,  840,  837,  256,  814,  815,  827,  256,  816,  724,
      828,  843,  845,  831,  833,  836,  729,  256,  729,  834,
      830,  256,  835,  256,  838,  832,  256,  841,  839,  256,
      840,  837,  256,  256,  256,  256,  241,  842,  730,  843,
      256,  845,  615,  844,  846,  256,  256,  256,  256,  229,

      256,  256,  229,  746,  256,  746,  841,  256,  256,  256,
      849, 1422,  848, 1422,  847,  842,  256,  853,  850,  852,
      615,  844,  854,  846,  861,  747,  851,  858,  859,  256,
      855,  860,  256,  256,  862,  362,  865,  864,  856,  849,
      848,  863,  847,  256,  868,  853,  850,  857,  852,  866,
      871,  854,  323,  861,  851,  858,  859,  122,  855,  256,
      860,  867,  353,  862,  865,  864,  856,  119,  323,  863,
      250,  869,  868,  870,  244,  857,  323,  866,  871,  873,
      220,  873,  318,  896,  241,  872,  873,  220,  874,  318,
      867,  315,  316,  317,  318,  881,  597, 1897,  879,  869,

      332,  870,  880,  255,  597,  883,  597,  886,  597,  332,
      879,  332,  896,  872,  877,   73,  894,  255,  921,   70,
      921,  388,  332,  881,  887,  591,  877, 1897, 1897,  332,
      880,  332, 1897,  591,  883,  886,  888,  591,  332,  889,
      332,  327,  890,  878,  894,  256,  255,  590,  220,  590,
      221,  332,  887,  901,  591,  332,  256,  152,  903,  882,
      332,  899,  591,  256,  888,  900,  591,  889,  154,  154,
      890,  893,  897,  222,  223,  930,  591,  224,  365,  225,
      365,  154,  901,  898,  332,  832,  895,  903,  902,  899,
      479, 1897,  366,  900, 1897, 1897,  631,  154,  154,  893,

      119,  897,  222,  223,  930,  591,  224, 1687,  225,  154,
     1897, 1897,  898,  832, 1688,  895,  902, 1689,  479,  322,
      322,  330,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  904,  322,
      322,  322,  322,  322,  891,  627,  332,  627,  918,  125,
     1897,  640, 1897,  640, 1897,  884,  126,  127,  628,  927,
      628,  892,  647, 1897,  647, 1897,  904,  641, 1897, 1897,
      322,  322,  119,  891, 1897,  332,  918, 1897,  648,  365,
     1897,  365,  921,  884,  922,  388, 1480,  927, 1480,  892,
      906, 1897, 1480,  366, 1480,  931,  905,  630,  322,  322,

      322,  330,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  906,  322,
      322,  322,  322,  322,  931,  905,  125,  908,  885,  908,
      932,  369, 1897,  126,  127, 1897,  928,  934,  909,  640,
     1897,  640, 1897,  910,  365,  911,  365,  365,  929,  365,
      322,  322,  647,  907,  647,  641,  933,  885,  366,  932,
      367,  366,  368,  367,  928,  368,  934,  936,  648,  935,
     1003,  692, 1003,  692, 1897,  915,  929,  937,  322,  912,
     1004,  938,  907,  941,  933,  942,  914,  939, 1897,  923,
      391,  923,  660,  693,  940,  936,  369,  148,  935,  369,

      657,  658,  659,  660,  915,  937,  150,  943,  148,  938,
      944,  941,  389,  942,  914,  953,  939,  150,  954,  923,
      391,  924,  660,  940,  955,  957,  389,  148,  956,  390,
      391,  390,  392,  958,  964,  943,  150,  148,  944,  945,
      695,  945,  695,  953,  959,  960,  150,  954,  961,  967,
      968, 1897, 1897,  955,  957,  389,  956,  962,  703,  962,
      703,  958,  696,  964,  965,  710,  965,  974,  973, 1897,
      975,  946,  959,  960,  711,  712,  961,  967,  968,  963,
      704,  971,  710,  971,  980,  947,  966,  986,  976,  979,
      981,  711,  712,  982,  972,  974,  972,  973,  975,  977,

      946,  985,  983,  972,  983,  723,  978,  723,  992,  987,
      993,  987,  980,  947,  685,  986,  685,  976,  979,  981,
      994,  995,  982,  989,  984,  989,  997,  724,  977, 1897,
      985,  988,  996,  729,  978,  729,  686,  992,  998,  993,
      999,  948, 1000, 1897, 1001,  990,  949, 1002,  994,  995,
     1007,  746, 1008,  746,  997,  730,  950,  951,  991,  972,
      952,  996, 1009, 1010, 1011, 1005,  998, 1005,  999, 1012,
      948, 1000, 1001,  747, 1013,  949, 1002, 1014, 1007, 1015,
     1008, 1016, 1018, 1016,  950,  951,  991, 1006,  952, 1019,
     1009, 1010, 1020, 1011, 1021,  229, 1022, 1023, 1012, 1024,

     1025, 1026, 1013, 1017,  229, 1014,  229, 1032, 1015, 1030,
     1034, 1018,  241,  256,  244,  939,  256, 1897, 1019,  256,
      977, 1020,  256, 1021, 1022, 1897, 1023,  978, 1024, 1025,
     1026, 1035, 1036, 1029, 1036, 1027, 1032, 1030, 1041, 1034,
     1041,  254, 1037,  256,  939, 1028, 1038, 1048, 1049,  977,
     1040, 1038, 1039, 1046, 1045,  978,  783,  244,  783, 1035,
      256,  256, 1029, 1027, 1041,  256, 1042,  254, 1043,  256,
     1043,  507, 1043, 1028, 1044,  507, 1048, 1049, 1040,  256,
     1039, 1046, 1045,  256, 1047,  256,  256, 1050,  784,  256,
      256,  256, 1052,  256,  945,  256,  945,  256,  256, 1055,

      256,  256, 1051,  256,  256,  256, 1053,  256,  256, 1897,
     1137,  256, 1047, 1058, 1897,  256, 1050,  784, 1114,  256,
     1054, 1052, 1057, 1897, 1059, 1067, 1063, 1055, 1056, 1061,
     1051, 1062, 1060, 1066, 1069, 1053,  256, 1065,  256, 1137,
     1064,  256, 1058,  256, 1897, 1070, 1114, 1073, 1054, 1071,
     1068, 1057, 1059, 1072, 1067, 1063, 1056, 1061, 1077, 1062,
     1060, 1075, 1066, 1069,  256, 1065, 1074,  256, 1064,  962,
      256,  962, 1076, 1070, 1079, 1073,  256, 1071, 1068,  256,
     1078, 1072,  965,  256,  965,  256, 1077,  256,  256, 1075,
     1897,  963,  256, 1080,  256, 1074,  256, 1897, 1082,  256,

     1081, 1076, 1085, 1079,  966,  256,  256,  256, 1078,  256,
      256,  256, 1083, 1086, 1084,  983,  256,  983,  256, 1090,
      256, 1087, 1080,  987,  256,  987,  256, 1082, 1081, 1092,
     1089, 1085,  989,  256,  989, 1897, 1093,  984, 1094, 1897,
     1083, 1097, 1086, 1084, 1091,  988, 1098, 1095, 1090, 1087,
      256, 1100, 1096,  256,  990,  256, 1103, 1092,  256, 1089,
     1101, 1003,  256, 1003,  256, 1093, 1094, 1088,  256,  256,
     1097, 1004, 1091,  256, 1098, 1095, 1005,  256, 1005, 1100,
     1096, 1099,  256, 1104, 1103, 1016,  256, 1016, 1101, 1105,
      256,  323, 1107, 1102, 1136, 1088, 1108, 1194, 1006, 1194,

     1109, 1112,  220, 1112,  318, 1897,  327, 1017, 1117, 1110,
     1099, 1106, 1104, 1112,  220, 1113,  318, 1111, 1105, 1195,
     1107, 1102, 1136, 1897, 1116, 1108,  597,  323, 1118, 1109,
     1897,  323, 1119,  597,  322, 1118,  322, 1139, 1110, 1106,
      322,  327,  322, 1117, 1116, 1897, 1111,  590,  220, 1115,
      221, 1116,  256, 1897, 1120,  591, 1897,  152, 1125, 1121,
     1119,  119,  591, 1897, 1163, 1116, 1139, 1164,  154,  154,
     1897, 1122, 1165,  222,  223, 1134,  591,  224,  322,  225,
      322,  154, 1124, 1120,  591, 1141,  365, 1125,  365, 1167,
      479,  591, 1163, 1897,  322, 1164,  322,  154,  154, 1122,

      366, 1165,  222,  223, 1134,  591,  224, 1897,  225,  154,
     1124,  322, 1897,  322, 1141, 1897, 1127, 1167,  479,  322,
      322,  330,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322, 1129,  322,
      322,  322,  322,  322, 1127, 1897, 1128, 1033,  256, 1033,
     1036,  256, 1036, 1166,  908, 1159,  908, 1159,  388, 1169,
     1037,  322, 1170,  322, 1038,  909, 1129, 1171, 1172, 1038,
      322,  322, 1897, 1159, 1128, 1160,  388, 1161,  391, 1161,
      660, 1251, 1166, 1251,  254,  148, 1003, 1169, 1003, 1126,
     1251, 1170, 1252,  254,  150, 1171, 1004, 1172,  322,  322,

     1123,  330, 1123,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322, 1126,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  125,  322,
      256,  322, 1173, 1176, 1177,  126,  127, 1161,  391, 1162,
      660, 1178, 1179, 1182, 1897,  148, 1184,  908, 1130,  908,
      322,  322, 1897, 1132,  150, 1144, 1131, 1133,  909, 1080,
     1173, 1176, 1177,  910, 1183, 1148, 1081, 1185, 1897, 1178,
     1135, 1179, 1182,  365, 1184,  365, 1897, 1130,  322,  908,
     1186,  908, 1132, 1187, 1144, 1131, 1133,  366, 1080,  367,
      909,  368, 1188, 1183, 1081,  910, 1185,  911, 1135,  912,

      125, 1151,  657,  658,  659,  660, 1189, 1333, 1334, 1186,
      148, 1187, 1897, 1237,  389, 1237, 1237,  256, 1237,  150,
     1418, 1188, 1418, 1238, 1897,  369, 1238,  945,  389,  945,
     1151,  912, 1138, 1152, 1189, 1152, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1153, 1138, 1154,
     1138, 1155, 1138, 1138, 1138, 1138, 1138,  389, 1174, 1180,
     1897, 1190, 1191, 1192, 1897, 1175, 1193, 1897, 1196, 1197,
     1206, 1207,  962, 1181,  962,  965, 1208,  965, 1209, 1897,
     1210, 1211, 1215, 1138, 1138, 1157, 1897, 1174, 1180, 1190,
     1897, 1191, 1192, 1175,  963, 1193, 1196,  966, 1197, 1206,

     1207, 1181, 1212, 1253, 1208, 1253,  507, 1209, 1210, 1211,
     1215, 1138, 1138, 1138, 1216, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1212, 1138, 1138, 1138, 1138, 1138, 1198, 1200, 1198,
     1200, 1897, 1897, 1216, 1237, 1213, 1237, 1213,  969,  970,
     1199, 1201, 1199, 1201, 1238,  971,  710,  971, 1217, 1199,
     1201, 1218,  710, 1138, 1138,  711,  712, 1214,  972, 1219,
      972, 1203, 1204,  983, 1205,  983, 1205,  972,  987,  989,
      987,  989, 1220, 1205, 1221, 1222, 1897, 1217, 1223, 1224,
     1218, 1138, 1225, 1226, 1227,  984, 1228, 1005, 1219, 1005,

      988,  990, 1229, 1230, 1232, 1231, 1233, 1897, 1234, 1235,
     1236, 1220, 1221, 1239, 1222, 1199, 1201, 1223, 1224, 1006,
     1225, 1240, 1226, 1227, 1228, 1016, 1241, 1016,  229,  229,
     1229, 1230, 1232,  972, 1231, 1233, 1234, 1235, 1236, 1205,
     1245, 1036, 1239, 1036, 1246,  244, 1247, 1017, 1248,  256,
     1240, 1037,  241,  256, 1241, 1038, 1243,  256,  256, 1242,
     1038, 1036, 1038, 1036, 1038, 1253,  256, 1254,  507, 1245,
      256, 1037,  256, 1246, 1247, 1038, 1038, 1248, 1897, 1897,
     1038, 1038,  332, 1249, 1257, 1243, 1312, 1250, 1242, 1256,
     1255, 1897,  256, 1258,  256, 1897, 1261, 1262, 1194,  256,

     1194, 1897, 1259, 1310,  220, 1310,  318, 1310,  220, 1311,
      318,  332, 1249, 1257, 1312, 1250, 1897, 1256, 1255,  256,
     1195, 1264, 1258,  256, 1261,  256, 1262,  256, 1263,  256,
     1259, 1138, 1138,  256, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1264, 1138, 1138, 1138, 1138, 1138, 1263,  256,  256, 1265,
     1268, 1269,  256, 1274,  256,  256, 1270,  256,  256,  256,
      256,  256,  256,  256,  256,  256,  256,  256,  256, 1213,
      256, 1213, 1138, 1138,  256, 1271, 1266, 1265, 1268, 1269,
     1272, 1274,  256, 1267, 1270, 1273, 1276, 1277, 1282, 1278,

     1281, 1214, 1275, 1279, 1280, 1284, 1287,  256, 1283,  256,
     1138,  256, 1286, 1285, 1271, 1266,  256,  256,  256, 1272,
     1294, 1267,  256, 1291, 1273, 1276, 1277, 1282, 1278, 1281,
     1275, 1279, 1289, 1280, 1284, 1287, 1283,  256, 1290,  256,
     1286, 1285,  256, 1293, 1292, 1295,  256,  256, 1294, 1288,
      256,  256, 1291, 1297,  256,  256,  256,  256,  256,  256,
      597, 1289,  597,  323, 1296, 1299, 1897, 1290,  323, 1897,
     1897, 1300, 1293, 1292, 1295, 1897,  597, 1288,  597, 1351,
     1897, 1298, 1297, 1304,  332, 1306, 1897, 1303, 1301,  591,
     1313, 1897, 1302, 1296, 1299, 1318, 1305, 1307, 1308, 1315,

     1300, 1309, 1123,  323, 1123,  591,  332, 1351, 1319, 1298,
      256, 1897, 1304,  332, 1306, 1303, 1301, 1897,  591, 1313,
     1302,  332, 1327, 1318, 1305, 1307, 1308, 1897, 1315, 1309,
      590,  220,  590,  221,  591,  332,  327, 1319,  327,  332,
      332,  125,  332, 1897, 1325, 1321, 1322, 1897,  126,  127,
      332, 1327,  152, 1323,  153, 1320,  222,  223,  125,  591,
      224, 1353,  225,  154,  154,  126,  127,  256,  332,  332,
     1897,  332, 1325,  479, 1321, 1322,  154, 1418,  256, 1418,
      365, 1323,  365, 1320, 1897,  222,  223, 1897,  591,  224,
     1353,  225,  154,  154,  366, 1326, 1342, 1897, 1342,  388,

     1342,  479, 1343,  388,  154,  322,  322,  330,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322, 1326,  322,  322,  322,  322,  322,
      908,  908,  908,  908, 1316,  908, 1352,  908,  627, 1354,
      627,  909,  909, 1346, 1897, 1346,  909, 1897, 1148, 1149,
     1355,  628,  365,  628,  365, 1356,  322,  322,  365,  365,
      365,  365, 1897, 1316, 1352, 1347,  366, 1354,  367, 1897,
      630,  710,  366,  366,  367,  367,  368,  368, 1355, 1897,
     1203, 1204,  912, 1356,  322,  322,  322,  330,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,

      322,  322,  322,  322,  369,  322,  322,  322,  322,  322,
      369,  369, 1317, 1344,  391, 1344,  660, 1344,  391, 1345,
      660,  148, 1348, 1357, 1348,  148, 1358, 1359, 1897, 1360,
      150, 1361, 1365, 1366,  150, 1368,  322,  322,  125, 1897,
     1424, 1317, 1424,  507, 1349,  126,  127,  908,  969,  908,
     1199, 1357, 1199, 1897, 1358, 1367, 1359, 1360,  909, 1199,
     1361, 1365, 1366, 1368,  322, 1314, 1314,  330, 1314, 1314,
     1314, 1314, 1314, 1314, 1314, 1314, 1314, 1314, 1314, 1314,
     1314, 1314, 1314, 1314, 1367, 1314, 1314, 1314, 1314, 1314,
     1369, 1370, 1371, 1372, 1897, 1373, 1374, 1375, 1377, 1375,

     1378, 1382, 1383, 1194, 1384, 1194, 1418, 1385, 1418, 1386,
     1387, 1390,  365, 1393,  365, 1199, 1314, 1314, 1369, 1376,
     1370, 1371, 1372, 1373, 1374, 1195,  366, 1377, 1378, 1897,
     1382, 1383, 1379, 1384, 1380, 1385, 1380, 1386, 1387, 1390,
     1391, 1393, 1391, 1380, 1314, 1328, 1336, 1897, 1336, 1328,
     1328, 1328, 1328, 1328, 1328, 1328, 1328, 1328, 1328, 1328,
     1337, 1328, 1338, 1328, 1339, 1328, 1328, 1328, 1328, 1328,
     1897, 1897, 1198, 1200, 1198, 1200, 1394, 1397, 1897, 1392,
     1213, 1398, 1213,  969,  970, 1199, 1201, 1199, 1201, 1399,
     1381,  710, 1381, 1400, 1199, 1201, 1328, 1328, 1341, 1380,

     1203, 1204, 1214, 1205, 1394, 1205, 1397, 1392, 1424, 1398,
     1425,  507, 1205, 1466,  220, 1466,  221, 1399, 1362, 1897,
     1362, 1400, 1897, 1401, 1328, 1328, 1328, 1402, 1328, 1328,
     1328, 1328, 1328, 1328, 1328, 1328, 1328, 1328, 1328, 1328,
     1328, 1328, 1328, 1328, 1363, 1328, 1328, 1328, 1328, 1328,
     1199, 1201, 1401,  710, 1403, 1402, 1388, 1364, 1388, 1395,
     1410, 1395, 1203, 1204, 1404, 1205, 1404, 1205, 1205, 1406,
     1408, 1406, 1408, 1363, 1205, 1412, 1328, 1328, 1389, 1411,
     1413, 1396,  229, 1403, 1416, 1364, 1417, 1419, 1410,  241,
     1405, 1407, 1409,  244, 1897, 1897, 1346,  256, 1346, 1348,

      256, 1348, 1468, 1412, 1328,  256,  256, 1473, 1411, 1413,
     1414, 1897, 1897, 1416, 1417, 1897, 1419, 1420, 1347, 1405,
     1421, 1349, 1375,  256, 1375,  256, 1466,  220, 1466, 1467,
     1205, 1468,  256, 1897,  256, 1473,  256, 1897,  256, 1414,
      256, 1428,  256,  256, 1376,  256, 1420, 1427, 1429, 1421,
     1328, 1328,  256, 1328, 1328, 1328, 1328, 1328, 1328, 1328,
     1328, 1328, 1328, 1328, 1328, 1328, 1328, 1328, 1328, 1428,
     1328, 1328, 1328, 1328, 1328, 1427, 1430, 1429, 1431, 1433,
     1432, 1434,  256, 1443, 1436,  256,  256, 1452, 1362,  256,
     1362,  256,  256,  256,  256,  256,  256,  256,  256,  256,

      256, 1328, 1328, 1897, 1430,  256, 1431, 1433, 1432, 1434,
     1435, 1443, 1436, 1437, 1438, 1452,  256,  256, 1440, 1441,
     1442, 1445, 1897, 1446, 1448,  256, 1897, 1439, 1444, 1328,
     1447,  256, 1449,  256, 1453, 1450,  256, 1451, 1897, 1435,
      256, 1486, 1437, 1438, 1388,  256, 1388, 1440, 1441, 1442,
     1445, 1446, 1461, 1448, 1457, 1439, 1444, 1455,  256, 1447,
      256, 1449, 1453, 1450, 1456, 1451, 1389, 1391,  256, 1391,
     1486, 1395,  256, 1395,  256, 1458, 1459, 1460, 1406,  256,
     1406, 1461,  323, 1457,  256, 1455, 1404,  256, 1404, 1463,
     1897, 1897, 1456, 1396, 1474, 1462, 1475, 1408,  256, 1408,

     1407, 1476, 1477, 1458, 1459, 1460, 1454, 1478, 1897, 1479,
     1470, 1897, 1464,  125, 1487, 1531, 1465, 1531, 1463, 1409,
      126,  127, 1474, 1462, 1475,  627, 1531,  627, 1531, 1476,
     1477, 1762, 1897, 1762, 1454, 1488, 1478, 1479,  628, 1470,
      628, 1464, 1763, 1487, 1465, 1469, 1469,  330, 1469, 1469,
     1469, 1469, 1469, 1469, 1469, 1469, 1469, 1469, 1469, 1469,
     1469, 1469, 1469, 1469, 1488, 1469, 1469, 1469, 1469, 1469,
      365,  365,  365,  365, 1482,  391, 1482,  660, 1897, 1897,
     1489, 1346,  148, 1346,  366,  366,  367,  367,  630,  368,
     1490,  150,  365, 1348,  365, 1348, 1469, 1469, 1897, 1482,

      391, 1483,  660, 1347, 1897, 1897,  366,  148,  367, 1489,
      368, 1530, 1897, 1530,  254, 1349,  150, 1573, 1490, 1573,
      388, 1897,  369,  369, 1469,  322,  322,  330,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  369,  322,  322,  322,  322,  322,
     1484, 1491, 1492, 1498, 1485, 1897, 1493, 1496, 1497, 1499,
     1500, 1501, 1897, 1502, 1503, 1504, 1505, 1897, 1506, 1472,
     1375, 1507, 1375, 1509, 1510, 1897,  322,  322, 1897, 1484,
     1491, 1492, 1498, 1485, 1493, 1496, 1497, 1499, 1897, 1500,
     1501, 1502, 1376, 1503, 1504, 1505, 1506, 1472, 1362, 1507,

     1362, 1509, 1897, 1510,  322,  322,  322,  330,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322, 1494,  322,  322,  322,  322,  322,
     1508, 1379, 1508, 1380, 1391, 1380, 1391, 1495, 1511, 1897,
     1513, 1379, 1380, 1380, 1388, 1380, 1388, 1514, 1381,  710,
     1381, 1515, 1380, 1494, 1516,  244,  322,  322, 1203, 1204,
     1395, 1205, 1395, 1205, 1517, 1495, 1389, 1511, 1513, 1518,
     1205, 1522, 1897, 1512, 1404, 1514, 1404, 1520,  256, 1515,
     1521, 1897, 1396, 1516,  322, 1406, 1523, 1406, 1408, 1526,
     1408, 1527, 1517,  256,  256, 1529,  256, 1518, 1380, 1522,

     1519, 1512,  256,  256,  256, 1535, 1520, 1407, 1380, 1521,
     1409,  256,  256,  256,  256, 1523,  256, 1526, 1536,  256,
     1527, 1537,  256, 1529,  256, 1541, 1205, 1533,  256, 1519,
     1538, 1534,  256, 1539, 1535,  256,  256,  256, 1540,  256,
      256, 1546,  256,  256, 1897,  256, 1547, 1536,  256, 1542,
     1537, 1543,  256, 1544, 1541, 1548, 1533, 1897, 1549, 1538,
     1534, 1539, 1545, 1563, 1565, 1560, 1552, 1540, 1550, 1553,
     1546, 1556, 1557,  256, 1566, 1547, 1569, 1542, 1551, 1543,
     1554, 1544,  256, 1897, 1548, 1555, 1549, 1567, 1558, 1568,
     1545, 1563, 1559, 1565, 1560, 1552, 1550, 1579, 1553, 1556,

     1580, 1557, 1581, 1566, 1561, 1569, 1551, 1570, 1554, 1562,
      220, 1562,  318, 1555, 1897, 1567, 1558, 1568, 1897, 1571,
     1559, 1571, 1574,  147, 1574, 1582, 1579, 1583, 1580, 1572,
      148, 1581, 1584, 1561, 1585, 1586, 1570, 1587, 1588,  150,
     1575,  391, 1575,  392, 1575,  391, 1575, 1576,  148, 1589,
     1590, 1592, 1577, 1582, 1591, 1583, 1593,  150, 1594, 1897,
     1584, 1578, 1585, 1586, 1595, 1587, 1588, 1596, 1598, 1597,
     1598, 1603, 1600, 1603, 1601, 1602, 1605, 1589, 1606, 1590,
     1592, 1607, 1591, 1608, 1609, 1593, 1508, 1594, 1508, 1610,
     1599,  229, 1595, 1604, 1612, 1596, 1613, 1379, 1597, 1380,

     1600, 1380, 1601, 1602,  241, 1605,  244, 1606, 1380, 1607,
      256, 1608, 1609, 1530,  256, 1530,  254, 1611, 1610, 1616,
      256, 1616,  507, 1612,  256, 1613,  256,  256,  256,  256,
      256, 1614,  256,  256, 1615, 1897,  256, 1620, 1618,  256,
      256,  256,  256,  256, 1897,  256, 1611, 1562,  220, 1562,
      318, 1598,  256, 1598,  256, 1897, 1619, 1627, 1897, 1626,
     1614, 1640, 1622, 1615, 1380, 1631, 1620, 1618, 1621, 1625,
     1623, 1624, 1628, 1599, 1629, 1630, 1633, 1641, 1632, 1636,
     1655, 1634,  323, 1635, 1619, 1897, 1627, 1626, 1642, 1640,
     1622, 1603,  256, 1603, 1631, 1651, 1621, 1625, 1623, 1624,

      256, 1628, 1629, 1630, 1633, 1641, 1632, 1636, 1639, 1655,
     1634, 1635, 1637, 1604, 1637, 1571, 1642, 1571, 1573, 1897,
     1573,  388, 1638, 1651, 1574, 1572, 1574, 1643, 1647,  391,
     1647,  660, 1648,  391, 1648, 1657,  148, 1639, 1652, 1659,
     1649,  150, 1653, 1654, 1656,  150, 1574,  147, 1574, 1650,
      390,  391,  390,  392,  148, 1658, 1643, 1660,  148, 1661,
     1663, 1661, 1664,  150, 1657, 1665, 1652,  150, 1659, 1662,
     1653, 1654, 1656, 1598, 1666, 1598, 1603, 1667, 1603, 1668,
     1669, 1670,  229, 1658, 1672,  241, 1660,  256, 1663,  256,
      244, 1664,  256, 1665, 1616, 1599, 1616,  507, 1604,  256,

      256,  256, 1897, 1666,  323, 1667,  256,  256, 1668, 1669,
     1670,  256, 1672, 1674, 1661,  256, 1661,  256,  256, 1671,
     1637, 1673, 1637, 1675, 1662, 1691, 1692, 1679, 1897, 1676,
     1638, 1681, 1682, 1897, 1677, 1699, 1683, 1897, 1702, 1680,
     1678, 1690, 1674, 1703, 1684, 1685, 1897, 1671, 1700, 1673,
     1700, 1675, 1897, 1691, 1692, 1705, 1679, 1676, 1701, 1897,
     1681, 1682, 1677, 1897, 1699, 1683, 1702, 1680, 1678, 1690,
     1704, 1897, 1703, 1684, 1685, 1693, 1694, 1693, 1695, 1647,
      391, 1647,  660, 1645, 1705, 1706, 1646,  148, 1648,  662,
     1648, 1696, 1697, 1696, 1698, 1707,  150, 1708, 1704, 1649,

     1661, 1661, 1661, 1661, 1709, 1650,  241,  256, 1650,  244,
     1662, 1662,  256,  256, 1706, 1700,  256, 1700,  256,  256,
     1687, 1687, 1897, 1897, 1707, 1701, 1708, 1688, 1688, 1897,
     1689, 1689, 1712, 1709, 1713, 1897, 1897, 1733, 1700, 1714,
     1700, 1715, 1734, 1735, 1716, 1717, 1718, 1835, 1701, 1835,
     1719, 1720, 1721, 1722,  229, 1897, 1724, 1725, 1724, 1726,
     1897, 1712, 1723, 1713, 1688, 1733, 1736, 1689, 1714, 1715,
     1897, 1734, 1735, 1716, 1717, 1718, 1723, 1693, 1694, 1693,
     1695, 1693, 1694, 1693, 1695, 1645, 1694,  244, 1646, 1645,
     1739, 1897, 1646, 1729, 1736, 1737, 1730, 1738, 1696, 1697,

     1696, 1698,  256, 1740,  256, 1723, 1649,  256,  657,  658,
      659,  660, 1764, 1767, 1742, 1650,  148, 1731, 1739, 1731,
      389,  323, 1897, 1897, 1737,  150, 1738, 1897, 1897, 1743,
     1732, 1740, 1732, 1745,  389, 1746, 1768, 1746, 1722, 1732,
     1897, 1764, 1767, 1742, 1746, 1744, 1747, 1722, 1719, 1720,
     1721, 1722, 1765, 1724, 1725, 1724, 1726, 1758, 1743, 1897,
     1723, 1688, 1745,  389, 1689, 1768,  241, 1750, 1751, 1752,
     1753,  256, 1897, 1744, 1723, 1688, 1725, 1694, 1689, 1754,
     1765,  229,  256, 1756, 1729, 1758, 1757, 1730, 1759, 1760,
     1759, 1761, 1772, 1754, 1694, 1731, 1729, 1731, 1762, 1730,

     1762, 1729, 1766, 1723, 1730, 1769, 1771,  256, 1732, 1763,
     1732, 1732, 1793, 1732, 1777, 1794, 1816, 1732, 1770, 1897,
     1732, 1772, 1754, 1778, 1897, 1778, 1722, 1897, 1775, 1778,
     1766, 1779, 1722, 1769, 1771, 1897, 1795, 1831, 1776, 1897,
     1793, 1897, 1777, 1897, 1794, 1816, 1770, 1780, 1725, 1780,
     1753, 1750, 1751, 1752, 1753, 1688, 1775,  323, 1689, 1688,
     1897, 1897, 1689, 1754, 1795, 1751, 1831, 1776, 1780, 1725,
     1781, 1753, 1783, 1786, 1725, 1784, 1688, 1754,  229, 1689,
     1787, 1756, 1725, 1788, 1757, 1789, 1790, 1789, 1791, 1756,
     1796, 1797, 1757, 1756, 1792, 1799, 1757, 1759, 1760, 1759,

     1761, 1693, 1694, 1693, 1695, 1729, 1754, 1694, 1730, 1645,
      241,  256, 1646,  256, 1729, 1897, 1813, 1730, 1796, 1797,
     1798, 1814, 1792, 1799, 1803, 1897, 1803, 1722, 1803, 1751,
     1804, 1722, 1805, 1725, 1805, 1753, 1783, 1815, 1897, 1784,
     1688, 1897, 1830, 1689, 1813, 1897, 1897, 1800, 1798, 1814,
     1801, 1802, 1805, 1725, 1806, 1753, 1807, 1808, 1807, 1809,
     1688, 1751, 1786, 1689, 1783, 1815, 1786, 1784, 1783, 1787,
     1830, 1784, 1788, 1787,  323, 1800, 1788,  241, 1801, 1802,
     1719, 1720, 1721, 1722, 1725, 1810, 1751, 1810, 1811,  256,
      256, 1756, 1723, 1787, 1757, 1833, 1788, 1789, 1790, 1789,

     1791, 1750, 1751, 1752, 1753, 1756, 1723, 1897, 1757, 1688,
     1897, 1842, 1689, 1754, 1843,  229, 1812, 1821, 1822, 1897,
     1822, 1722, 1851, 1833, 1860, 1820, 1822, 1754, 1823, 1722,
     1824, 1725, 1824, 1753, 1897, 1723,  241, 1897, 1688, 1842,
     1897, 1689, 1843, 1897, 1812, 1897, 1821, 1824, 1725, 1825,
     1753, 1851, 1860, 1820, 1897, 1688, 1754, 1832, 1689, 1807,
     1808, 1807, 1809, 1750, 1751, 1752, 1753, 1783,  323, 1751,
     1784, 1688, 1834, 1897, 1689, 1754, 1783, 1751, 1897, 1784,
     1810, 1751, 1810, 1811, 1827, 1832, 1856, 1828, 1787, 1754,
     1897, 1788, 1751, 1836, 1725, 1836, 1753,  229, 1897, 1827,

     1834, 1688, 1828, 1897, 1689, 1836, 1725, 1837, 1753, 1844,
     1840, 1844, 1722, 1688,  323, 1856, 1689, 1897, 1754, 1897,
     1835, 1897, 1835, 1897, 1897, 1897, 1751, 1897, 1897, 1897,
     1897, 1897, 1897, 1827, 1897, 1897, 1828, 1841, 1840, 1897,
     1897, 1897, 1897, 1897, 1838, 1808, 1838, 1839, 1845, 1725,
     1845, 1726, 1827, 1751, 1849, 1828, 1688,  229, 1897, 1689,
     1827,  323, 1897, 1828,  241, 1841, 1838, 1808, 1838, 1839,
     1897, 1897, 1897, 1844, 1827, 1844, 1722, 1828, 1853, 1725,
     1853, 1753, 1849, 1725, 1850, 1687, 1688, 1897, 1854, 1689,
     1756,  229, 1688, 1757, 1897, 1689,  241,  229, 1897, 1686,

     1845, 1725, 1845, 1846, 1686,  323, 1852, 1686, 1848, 1686,
     1686, 1689, 1897, 1850, 1686, 1686, 1897, 1854,  323, 1686,
     1855, 1686, 1686, 1686, 1724, 1725, 1724, 1726, 1853, 1725,
     1853, 1753, 1688, 1859, 1852, 1689, 1688,  241, 1861, 1689,
      241, 1858, 1863, 1864, 1863, 1865, 1897, 1862, 1897, 1855,
     1686, 1686, 1686, 1866, 1867, 1866, 1868, 1870, 1871, 1870,
     1872, 1859, 1863, 1864, 1863, 1865, 1861, 1881, 1897, 1897,
     1858, 1873, 1874, 1873, 1875, 1862, 1897, 1869, 1686, 1866,
     1867, 1866, 1868, 1878, 1867, 1878, 1868, 1870, 1871, 1870,
     1872, 1878, 1867, 1878, 1868,  241, 1881, 1873, 1874, 1873,

     1875, 1873, 1874, 1873, 1875, 1869, 1873, 1874, 1873, 1875,
     1878, 1867, 1878, 1868, 1878, 1867, 1878, 1868, 1885, 1878,
     1867, 1878, 1868,  241, 1887, 1888, 1887, 1889, 1887, 1888,
     1887, 1889, 1890, 1891, 1890, 1892, 1890, 1891, 1890, 1892,
     1890, 1891, 1890, 1892, 1897, 1897, 1897, 1885, 1890, 1891,
     1890, 1892, 1890, 1891, 1890, 1892, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1886, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1886,   68,   68,   68,   68,   68,   68,

       68,   68,   68,   68,   68,   68,   68,   68,   68,   68,
       68,   68,   69,   69,   69,   69,   69,   69,   69,   69,
       69,   69,   69,   69,   69,   69,   69,   69,   69,   69,
       72,   72,   72,   72,   72,   72,   72,   72,   72,   72,
       72,   72,   72,   72,   72,   72,   72,   72,  115,  115,
     1897,  115,  115,  115,  115,  115,  115,  115,  115,  115,
      115,  115,  115,  115,  115,  115,  118,  118,  118,  118,
      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      118,  118,  118,  118,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,

      124,  124,  133, 1897, 1897, 1897, 1897, 1897, 1897,  133,
     1897,  133, 1897,  133,  133,  133,  133,  133,  160,  160,
      160,  160,  160,  228,  228,  228,  228,  228,  228,  228,
      228,  228,  228,  228,  228,  228,  228,  228,  228,  228,
      228,  240,  240,  240,  240,  240,  240,  240,  240,  240,
      240,  240,  240,  240,  240,  240,  240,  240,  240,  243,
      243,  243,  243,  243,  243,  243,  243,  243,  243,  243,
      243,  243,  243,  243,  243,  243,  243,  249,  249,  249,
      249,  249,  249,  249,  249,  249,  249,  249,  249,  249,
      249,  249,  249,  249,  249,  257, 1897, 1897, 1897, 1897,

     1897, 1897, 1897, 1897, 1897, 1897,  257,  257,  257,  257,
      257,  322,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  115,
      115, 1897,  115,  115,  115,  115,  115,  115,  115,  115,
      115,  115,  115,  115,  115,  115,  115,  118,  118,  118,
      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      118,  118,  118,  118,  118,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,

      124,  357,  357,  357,  357,  357,  357,  357,  357,  357,
      357,  357,  357,  357,  357,  357,  357,  357,  357,  133,
     1897, 1897, 1897, 1897, 1897, 1897,  133, 1897,  133, 1897,
     1897,  133,  133,  133,  133,  393,  393,  393,  393, 1897,
      393,  393,  393,  393,  393,  393, 1897,  393,  393, 1897,
     1897,  393,  393,  160,  160,  160,  160,  160,  480,  480,
      480,  480,  480,  480,  480,  480,  480,  480,  480,  480,
      480,  480,  480,  480,  480,  480,  228,  228,  228,  228,
      228,  228,  228,  228,  228,  228,  228,  228,  228,  228,
      228,  228,  228,  228,  240,  240,  240,  240,  240,  240,

      240,  240,  240,  240,  240,  240,  240,  240,  240,  240,
      240,  240,  243,  243,  243,  243,  243,  243,  243,  243,
      243,  243,  243,  243,  243,  243,  243,  243,  243,  243,
      249,  249,  249,  249,  249,  249,  249,  249,  249,  249,
      249,  249,  249,  249,  249,  249,  249,  249,  502,  502,
      502,  502,  502,  502,  502,  502,  502,  502,  502,  502,
      502,  502,  502,  502,  502,  502,  503,  503,  503,  503,
      503,  503,  503,  503,  503,  503,  503,  503,  503,  503,
      503,  503,  503,  503,  588,  588,  588,  588,  588,  588,
      588,  588,  588,  588,  588,  588,  588,  588,  588,  588,

      588,  588,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,
      332,  332,  332,  332,  332,  332,  332,  332,  332,  332,
      332,  332,  332,  332,  332,  332,  332,  332,  115,  115,
     1897,  115,  115,  115,  115,  115,  115,  115,  115,  115,
      115,  115,  115,  115,  115,  115,  118,  118,  118,  118,
      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      118,  118,  118,  118,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  357,  357,  357,  357,  357,  357,  357,  357,

      357,  357,  357,  357,  357,  357,  357,  357,  357,  357,
      624,  624,  624,  624,  624,  624,  624,  624,  624,  624,
      624,  624,  624,  624,  624,  624,  624,  624,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  629, 1897, 1897, 1897,
     1897, 1897, 1897,  629, 1897,  629, 1897, 1897,  629,  629,
      629,  629,  133, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
      133, 1897,  133, 1897,  133,  133,  133,  133,  133,  632,
      632,  632,  632,  655,  655,  655,  655,  655,  655,  655,
      655,  655,  655,  655,  655,  655,  655,  655,  655,  655,

      655,  656,  656,  656,  656,  656,  656,  656,  656,  656,
      656,  656,  656,  656,  656,  656,  656,  656,  656,  661,
      661,  661,  661,  661,  661,  661,  661,  661,  661,  661,
      661,  661,  661,  661,  661,  661,  661,  393,  393,  393,
      393, 1897,  393,  393,  393,  393,  393,  393, 1897,  393,
      393, 1897, 1897,  393,  393,  160,  160,  160,  160,  160,
      709,  709,  709,  709,  709,  709,  709,  709,  709,  709,
      709,  709,  709,  709,  709,  709,  709,  709,  478, 1897,
     1897, 1897, 1897, 1897, 1897, 1897,  478,  478,  480,  480,
      480,  480,  480,  480,  480,  480,  480,  480,  480,  480,

      480,  480,  480,  480,  480,  480,  228,  228,  228,  228,
      228,  228,  228,  228,  228,  228,  228,  228,  228,  228,
      228,  228,  228,  228,  240,  240,  240,  240,  240,  240,
      240,  240,  240,  240,  240,  240,  240,  240,  240,  240,
      240,  240,  243,  243,  243,  243,  243,  243,  243,  243,
      243,  243,  243,  243,  243,  243,  243,  243,  243,  243,
      249,  249,  249,  249,  249,  249,  249,  249,  249,  249,
      249,  249,  249,  249,  249,  249,  249,  249,  502,  502,
      502,  502,  502,  502,  502,  502,  502,  502,  502,  502,
      502,  502,  502,  502,  502,  502,  503,  503,  503,  503,

      503,  503,  503,  503,  503,  503,  503,  503,  503,  503,
      503,  503,  503,  503,  789,  789,  789,  789,  789,  789,
      789,  789,  789,  789,  789,  789,  789,  789,  789,  789,
      789,  789,  790,  790,  790,  790,  790,  790,  790,  790,
      790,  790,  790,  790,  790,  790,  790,  790,  790,  790,
      257, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897,  257,  257,  257,  257,  257,  588,  588,  588,  588,
      588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
      588,  588,  588,  588,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,

      322,  322,  332,  332,  332,  332,  332,  332,  332,  332,
      332,  332,  332,  332,  332,  332,  332,  332,  332,  332,
      115,  115, 1897,  115,  115,  115,  115,  115,  115,  115,
      115,  115,  115,  115,  115,  115,  115,  115,  118,  118,
      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      118,  118,  118,  118,  118,  118,  357,  357,  357,  357,
      357,  357,  357,  357,  357,  357,  357,  357,  357,  357,
      357,  357,  357,  357,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  624,  624,  624,  624,  624,  624,  624,  624,

      624,  624,  624,  624,  624,  624,  624,  624,  624,  624,
      629, 1897, 1897, 1897, 1897, 1897, 1897,  629, 1897,  629,
     1897, 1897,  629,  629,  629,  629,  913, 1897, 1897, 1897,
     1897, 1897, 1897, 1897,  913, 1897, 1897, 1897,  913,  913,
      913,  913,  913,  133, 1897, 1897, 1897, 1897, 1897, 1897,
     1897,  133, 1897,  133, 1897,  133,  133,  133,  133,  133,
      655,  655,  655,  655,  655,  655,  655,  655,  655,  655,
      655,  655,  655,  655,  655,  655,  655,  655,  656,  656,
      656,  656,  656,  656,  656,  656,  656,  656,  656,  656,
      656,  656,  656,  656,  656,  656,  925,  925,  925,  925,

      925,  925,  925,  925,  925,  925,  925,  925,  925,  925,
      925,  925,  925,  925,  661,  661,  661,  661,  661,  661,
      661,  661,  661,  661,  661,  661,  661,  661,  661,  661,
      661,  661,  160,  160,  160,  160,  160,  709,  709,  709,
      709,  709,  709,  709,  709,  709,  709,  709,  709,  709,
      709,  709,  709,  709,  709,  710,  710,  710,  710,  710,
      710, 1897,  710,  710,  710,  710,  710,  710,  710,  710,
      710,  710,  710,  711,  711, 1897,  711,  711,  711,  711,
      711,  711,  711,  711,  711,  711,  711,  711,  711,  711,
      711,  228,  228,  228,  228,  228,  228,  228,  228,  228,

      228,  228,  228,  228,  228,  228,  228,  228,  228,  240,
      240,  240,  240,  240,  240,  240,  240,  240,  240,  240,
      240,  240,  240,  240,  240,  240,  240,  243,  243,  243,
      243,  243,  243,  243,  243,  243,  243,  243,  243,  243,
      243,  243,  243,  243,  243,  789,  789,  789,  789,  789,
      789,  789,  789,  789,  789,  789,  789,  789,  789,  789,
      789,  789,  789,  790,  790,  790,  790,  790,  790,  790,
      790,  790,  790,  790,  790,  790,  790,  790,  790,  790,
      790,  322,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  332,

      332,  332,  332,  332,  332,  332,  332,  332,  332,  332,
      332,  332,  332,  332,  332,  332,  332, 1138, 1138, 1897,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138,  115,  115, 1897,  115,  115,
      115,  115,  115,  115,  115,  115,  115,  115,  115,  115,
      115,  115,  115, 1140, 1140, 1897, 1140, 1140, 1140, 1140,
     1140, 1140, 1140, 1140, 1140, 1140, 1140, 1140, 1140, 1140,
     1140,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      118,  118,  118,  118,  118,  118,  118,  118,  118, 1142,
     1142, 1142, 1142, 1142, 1142, 1142, 1142, 1142, 1142, 1142,

     1142, 1142, 1142, 1142, 1142, 1142, 1142,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  124, 1145, 1145, 1145, 1145, 1145,
     1145, 1145, 1145, 1145, 1145, 1145, 1145, 1145, 1145, 1145,
     1145, 1145, 1145,  629, 1897, 1897, 1897, 1897, 1897,  629,
     1897, 1897, 1897,  629, 1897,  629,  629,  629,  629,  629,
     1150, 1150, 1150, 1150,  913, 1897, 1897, 1897, 1897, 1897,
     1897, 1897,  913, 1897, 1897, 1897,  913,  913,  913,  913,
      913,  133, 1897, 1897, 1897, 1897, 1897, 1897, 1897,  133,
     1897,  133, 1897,  133,  133,  133,  133,  133, 1156, 1156,

     1897, 1156, 1156, 1156, 1156, 1156, 1156, 1156, 1156, 1156,
     1156, 1156, 1156, 1156, 1156, 1156,  925,  925,  925,  925,
      925,  925,  925,  925,  925,  925,  925,  925,  925,  925,
      925,  925,  925,  925, 1168, 1168, 1897, 1168, 1168, 1168,
     1168, 1168, 1168, 1168, 1168, 1168, 1168, 1168, 1168, 1168,
     1168, 1168,  710,  710,  710,  710,  710,  710, 1897,  710,
      710,  710,  710,  710,  710,  710,  710,  710,  710,  710,
      711,  711, 1897,  711,  711,  711,  711,  711,  711,  711,
      711,  711,  711,  711,  711,  711,  711,  711,  709,  709,
      709,  709,  709,  709,  709,  709,  709,  709,  709,  709,

      709,  709,  709,  709,  709,  709, 1202, 1202, 1202, 1202,
     1202, 1202, 1202, 1202, 1202, 1202, 1202, 1202, 1202, 1202,
     1202, 1202, 1202, 1202,  228,  228,  228,  228,  228,  228,
      228,  228,  228,  228,  228,  228,  228,  228,  228,  228,
      228,  228, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
     1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244, 1244,
      240,  240,  240,  240,  240,  240,  240,  240,  240,  240,
      240,  240,  240,  240,  240,  240,  240,  240,  243,  243,
      243,  243,  243,  243,  243,  243,  243,  243,  243,  243,
      243,  243,  243,  243,  243,  243, 1260, 1260, 1260, 1260,

     1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260,
     1260, 1260, 1260, 1260,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322, 1314, 1314, 1314, 1314, 1314, 1314, 1314, 1314,
     1314, 1314, 1314, 1314, 1314, 1314, 1314, 1314, 1314, 1314,
      332,  332,  332,  332,  332,  332,  332,  332,  332,  332,
      332,  332,  332,  332,  332,  332,  332,  332, 1324, 1324,
     1324, 1324, 1324, 1324, 1324, 1324, 1324, 1324, 1324, 1324,
     1324, 1324, 1324, 1324, 1324, 1324, 1328, 1328, 1897, 1328,
     1328, 1328, 1328, 1328, 1328, 1328, 1328, 1328, 1328, 1328,

     1328, 1328, 1328, 1328, 1329, 1329, 1897, 1329, 1329, 1329,
     1329, 1329, 1329, 1329, 1329, 1329, 1329, 1329, 1329, 1329,
     1329, 1329,  115,  115, 1897,  115,  115,  115,  115,  115,
      115,  115,  115,  115,  115,  115,  115,  115,  115,  115,
     1330, 1330, 1330, 1330, 1330, 1330, 1330, 1330, 1330, 1330,
     1330, 1330, 1330, 1330, 1330, 1330, 1330, 1330,  118,  118,
      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      118,  118,  118,  118,  118,  118, 1332, 1332, 1332, 1332,
     1332, 1332, 1332, 1332, 1332, 1332, 1332, 1332, 1332, 1332,
     1332, 1332, 1332, 1332,  124,  124,  124,  124,  124,  124,

      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124, 1335, 1897, 1897, 1897, 1897, 1897, 1335, 1897,
     1897, 1897, 1897, 1897, 1335, 1335, 1335, 1335, 1335, 1340,
     1340, 1897, 1340, 1340, 1340, 1340, 1340, 1340, 1340, 1340,
     1340, 1340, 1340, 1340, 1340, 1340, 1340,  629, 1897, 1897,
     1897, 1897, 1897, 1897,  629, 1897,  629, 1897, 1897,  629,
      629,  629,  629,  133, 1897, 1897, 1897, 1897, 1897, 1897,
     1897,  133, 1897,  133, 1897,  133,  133,  133,  133,  133,
      632,  632,  632,  632, 1350, 1350, 1897, 1350, 1350, 1350,
     1350, 1350, 1350, 1350, 1350, 1350, 1350, 1350, 1350, 1350,

     1350, 1350,  710,  710,  710,  710,  710,  710, 1897,  710,
      710,  710,  710,  710,  710,  710,  710,  710,  710,  710,
      711,  711, 1897,  711,  711,  711,  711,  711,  711,  711,
      711,  711,  711,  711,  711,  711,  711,  711, 1203, 1203,
     1897, 1203, 1203, 1203, 1203, 1203, 1203, 1203, 1203, 1203,
     1203, 1203, 1203, 1203, 1203, 1203, 1202, 1202, 1202, 1202,
     1202, 1202, 1202, 1202, 1202, 1202, 1202, 1202, 1202, 1202,
     1202, 1202, 1202, 1202,  228,  228,  228,  228,  228,  228,
      228,  228,  228,  228,  228,  228,  228,  228,  228,  228,
      228,  228, 1415, 1415, 1415, 1415, 1415, 1415, 1415, 1415,

     1415, 1415, 1415, 1415, 1415, 1415, 1415, 1415, 1415, 1415,
      240,  240,  240,  240,  240,  240,  240,  240,  240,  240,
      240,  240,  240,  240,  240,  240,  240,  240,  243,  243,
      243,  243,  243,  243,  243,  243,  243,  243,  243,  243,
      243,  243,  243,  243,  243,  243, 1423, 1897, 1423, 1897,
     1897, 1897, 1897, 1423, 1897, 1897, 1423, 1423, 1423, 1423,
     1423, 1423, 1426, 1426, 1426, 1426, 1426, 1426, 1426, 1426,
     1426, 1426, 1426, 1426, 1426, 1426, 1426, 1426, 1426, 1426,
     1469, 1469, 1469, 1469, 1469, 1469, 1469, 1469, 1469, 1469,
     1469, 1469, 1469, 1469, 1469, 1469, 1469, 1469,  322,  322,

      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322, 1471, 1471, 1471, 1471,
     1471, 1471, 1471, 1471, 1471, 1471, 1471, 1471, 1471, 1471,
     1471, 1471, 1471, 1471,  332,  332,  332,  332,  332,  332,
      332,  332,  332,  332,  332,  332,  332,  332,  332,  332,
      332,  332,  115,  115, 1897,  115,  115,  115,  115,  115,
      115,  115,  115,  115,  115,  115,  115,  115,  115,  115,
      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      118,  118,  118,  118,  118,  118,  118,  118,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,

      124,  124,  124,  124,  124,  124, 1335, 1897, 1897, 1897,
     1897, 1897, 1335, 1897, 1897, 1897, 1897, 1897, 1335, 1335,
     1335, 1335, 1335,  629, 1897, 1897, 1897, 1897, 1897, 1897,
      629, 1897,  629, 1897, 1897,  629,  629,  629,  629,  133,
     1897, 1897, 1897, 1897, 1897, 1897, 1897,  133, 1897,  133,
     1897,  133,  133,  133,  133,  133,  632,  632,  632,  632,
     1481, 1897, 1481, 1897, 1897, 1897, 1897, 1481, 1897, 1897,
     1481, 1481, 1481, 1481, 1481, 1481, 1532, 1897, 1532, 1897,
     1897, 1897, 1897, 1532, 1897, 1897, 1532, 1532, 1532, 1532,
     1532, 1532,  480,  480,  480,  480,  480,  480,  480,  480,

      480,  480,  480,  480,  480,  480,  480,  480,  480,  480,
     1617, 1617, 1617, 1617, 1617, 1644, 1644, 1897, 1644, 1644,
     1644, 1644, 1644, 1644, 1644, 1644, 1644, 1644, 1644, 1644,
     1644, 1644, 1644,  661,  661,  661,  661,  661,  661,  661,
      661,  661,  661,  661,  661,  661,  661,  661,  661,  661,
      661, 1686, 1686, 1686, 1686, 1686, 1686, 1686, 1686, 1686,
     1686, 1686, 1686, 1686, 1686, 1686, 1686, 1686, 1686, 1728,
     1728, 1728, 1728, 1728, 1728, 1728, 1728, 1728, 1728, 1728,
     1728, 1728, 1728, 1728, 1728, 1728, 1728, 1748, 1748, 1748,
     1748, 1748, 1748, 1748, 1748, 1748, 1748, 1748, 1748, 1748,

     1748, 1748, 1748, 1748, 1748, 1749, 1749, 1749, 1749, 1749,
     1749, 1749, 1749, 1749, 1749, 1749, 1749, 1749, 1749, 1749,
     1749, 1749, 1749, 1755, 1755, 1755, 1755, 1755, 1755, 1755,
     1755, 1755, 1755, 1755, 1755, 1755, 1755, 1755, 1755, 1755,
     1755, 1773, 1773, 1773, 1773, 1773, 1773, 1773, 1773, 1773,
     1773, 1773, 1773, 1773, 1773, 1773, 1773, 1773, 1773, 1782,
     1782, 1782, 1782, 1782, 1782, 1782, 1782, 1782, 1782, 1782,
     1782, 1782, 1782, 1782, 1782, 1782, 1782, 1785, 1785, 1785,
     1785, 1785, 1785, 1785, 1785, 1785, 1785, 1785, 1785, 1785,
     1785, 1785, 1785, 1785, 1785, 1826, 1826, 1826, 1826, 1826,

     1826, 1826, 1826, 1826, 1826, 1826, 1826, 1826, 1826, 1826,
     1826, 1826, 1826, 1847, 1847, 1847, 1847, 1847, 1847, 1847,
     1847, 1847, 1847, 1847, 1847, 1847, 1847, 1847, 1847, 1847,
     1847, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1879,
     1879, 1879, 1879, 1879, 1879, 1879, 1879, 1879, 1879, 1879,
     1879, 1879, 1879, 1879, 1879, 1879, 1879, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1884, 1884, 1884, 1884, 1884,
     1884, 1884, 1884, 1884, 1884, 1884, 1884, 1884, 1884, 1884,

     1884, 1884, 1884, 1893, 1893, 1893, 1893, 1893, 1893, 1893,
     1893, 1893, 1893, 1893, 1893, 1893, 1893, 1893, 1893, 1893,
     1893, 1895, 1895, 1895, 1895, 1895, 1895, 1895, 1895, 1895,
     1895, 1895, 1895, 1895, 1895, 1895, 1895, 1895, 1895,   17,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,

     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897
    } ;

static const flex_int16_t yy_chk[9721] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        2,    2,    2,    2,    8, 1895,    8,   10,   10,   10,
       11,   11,   11,   12,   12,   12,   71, 1920, 1920,   11,

       18,   71,   12,   19,   75,   19,    2,    2,   21,    8,
        2,   76,    2,   76,   18,   10,   23, 1893,   22,  106,
       24, 1892,   21,    2,   61,   61,   61,   61,  108,   18,
       23,  114,   19,  114,   24,    2,    2,   21,    8,    2,
       75,    2,   18,   22,   10,   23,   19,  320,  106,   24,
       21,    2,    6,    6,    6,    6,  108,   22,   23,   28,
       29,   19,   24,  320,   98,   34,   98,   30,   75,   98,
      113,   98,   22,   28,   19,   34,   35,   29,    6,    6,
     1889,   30,    6,   86,    6,   22,  112,   86,   28,   34,
       35,   29,   98,   86,   98,    6,   30,   98,  113,   98,

      248,   28,  248, 1884,   34,   35,   29,    6,    6,   30,
     1882,    6,   86,    6, 1879,  112,   86,   34,   35,   29,
      776,   86,  776,    6,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,

       13,   13,   13,   13,   14,   14,   14,   14,   36,   25,
       25,   25,   25,   26,  116,   36,   14,  102, 1876,  102,
       26,   26,   14,  117,  162,   37,   37,  121,  809,   36,
       14,   14,   37,  592,   14,  102,   14,  154,   25,  154,
      139,   26,  154,  116,   36,  809,   37,   14,  104,  592,
      104,  117,   25,  162, 1875,   26,   27,   36,   27,   14,
       14,   37,  140,   14,  121,   14,  104,   25,  139,   27,
       26,   27, 1872,   27,   37,   14,   16,   16,   16,   16,
       25,   40,   46,   26,   38,   27,   40,   32,   32,   32,
      140,   38,  121,  166,   16,   32,   46,  144,   32,   27,

       40,  145,   16,   16,   32,   38,   16, 1868,   16,   32,
       40,   46,   45,   89,   27,   40,   32,   89, 1865,   16,
       38,   45,  166,   89,   46,  144,   45,   27,   40,  145,
       32,   16,   16,   38,  110,   16,  110,   16,  111, 1821,
      111,   45,   89, 1820, 1799,   32,   89,   16,   31,   45,
       31,   89,  110,  120,   45,   59,  111,   33,   32,   33,
      122,  122,  122,  122, 1775, 1773,   31,  111,   33,   33,
       33,   39,   31,  120,   39,   59,   31,   31,   31, 1749,
       39,   33,  163,   31,   33,   31,   31,   39,   39,   59,
      164,   31,   74,   74,   74,   31,  111,   33,   33,   33,

       39,   31,  120,   39,   59,   31,   31,   31,   39,   33,
      163,   31,   33,   31,   31,   39,   39,   59,  164,   31,
       74, 1748,  165,   31,   41,   48,   42,   42,   48,   41,
       42,   41,   49,   49,  905,   41,   42,  905,   41,   48,
       62,   42,   49,   41,  124,   62, 1742,   49,   41,   74,
      165,  124,  124,   41,   48,   42,   42,   48,   41,   42,
       41,   49,   49,   41,   42,   47,   41,   48,   62,   42,
       49,   41,   47,   47,   47,   49,   41,   43,  778,   47,
      778,  167,   62,   43, 1141,   43,   43, 1141,   43,   43,
       63,   43,  169, 1723,   47,   43,  126,   62, 1722, 1717,

       47,   47,   47,  126,  126, 1714,   43,   47,   63,  167,
       62,   43,   50,   43,   43,  168,   43,   43,  173,   43,
      169,   50,   63,   43,   44, 1698,   50,   44,  128, 1685,
       44,   44,  130,   44,  130,  128,  128,   63,   44,   44,
      171,   50,  176,   51,  168,  130,  173,  130,   51,   50,
       63,  128, 1684,   44,   50,   51,   44,   51,   44,   44,
       52,   44,   51,   51,   53,  161,   44,   44,   52,  171,
       52,  176,   51, 1414,   52,   52,   53,   51,   53,  175,
      128,   53,  161,   51,   53,   51,   54,   54, 1679,   52,
       51,   51,   56,   53,  161,  177,   52,   56,   52,   56,

     1678,   54,   52,   52,   53,  141,   53,  175, 1677,   53,
      161,   56,   53,   55, 1672,   54,   54,   55,   55, 1650,
      141,   56,   57,  177, 1414,  178,   56,   55,   56,   54,
       57,   87,   55,   55,  141,   57,   57,  129,   58,   56,
       58,  131,   55,  131,  129,  129,   55,   55,  141, 1643,
       58,   57,   58,  178,  131,   55,  131, 1634,   57,   87,
       55,   55,  193,   57,   57,  143, 1633,   58,  179,   58,
      181, 1631, 1629,   87,   81,  143, 1625,  129,   58, 1420,
       58,   60,   60,   60,   60,   64,   65, 1621,   87,   64,
       64,  193,   65,   64,  143,   65,  179,   81,  181,   64,

       65,   87,   81,  143,   64,  129,  182,   60,   60, 1470,
       60,   60, 1620,   60,   64,   65,   81,  205,   64,   64,
       65, 1556,   64,   65,   60,  183,   81,   64,   65, 1555,
     1420,   81,   64,  184,  182, 1551,   60,   60,   67,   60,
       60,   66,   60,   67,   81,   67,  205, 1550,   66,   66,
       66,  211,   60,  183,   80,   66,   88,   67,  170,  170,
     1470,  184,   67,   79,   79,   79,   79,   67, 1572,  185,
       66, 1572,   67,   83,   67,   79,   66,   66,   66,   80,
      211, 1549,   80,   66,   88,   67,  170,  170,   80,   79,
       67,   88,   88,   88,  188,   80,   80,  185,   88,   90,

       83,   83,   91,   90,   83,  213,   91, 1547,   80,   90,
       83,   80,   91,   88,  191,   83,   80,  192,   79,   88,
       88,   88,  188,   80,   80,   82,   88,  214,   90,   83,
       83,   91,   90,   83,  213,   91,  195,   90,   83, 1540,
       91,  196,  191,   83,  215,  192,  142,  172,   82, 1534,
       85,   94,  172,   82,   85,   82,  214,   85,   85,   82,
       85,  142,   82,   94,  195,   94,   85,   82,   94,  196,
      100,   94,   82,  215,  174,  142,  172,   82,  100,   85,
       94,  172,   82,   85,   82,   85,   85,   82,   85,  142,
       82,   94, 1469,   94,   85,   82,   94,  217,  100,   94,

       82,   84,  186,  223,  186,   84, 1644,  197,   84, 1644,
       84,   84,  100,   84,   84,  174,  174,   92,   95, 1467,
       84,   92,   95, 1462,   93,   92,  217,  100,   95,   92,
       84,   92,  223, 1461,   84,  197,   84,   92,   84,   84,
      100,   84,   84,  174,  174,  186,   92,   95,   84,  198,
       92,   95,   93,  194,   92, 1459,   95,   92,  103,   92,
       93,  194,   93,  199,   96,   92,   93,   93,   96,  103,
      138,  201,   96,  186,   99,   97, 1456,  198,   96,  138,
      138,   93,  194,   96,   96,  418,  103,  418,   93,  194,
       93,  199,  138,   96,   93,   93, 1453,   96,   97,  201,

      103,   96,   99,   97,   99,   97,   96,  418,  138,  138,
      202,   96,   96,  202,   99,  103,   99,   97,  180,  251,
      138,  251,  251,  101,  101,  101,  101,   97,  103,  180,
     1450,   99,   97,   99,   97,  101,  107,  233,  202,  207,
      107,  202,   99,  107,   99,   97,  107,  180,  208,  101,
      125,  125,  125,  146,  146,  146, 1449,  180,  125,  125,
      253,  146,  253,  253, 1448,  107,  233,  207, 1442,  107,
      146,  107, 1436, 1021,  107, 1021,  208, 1431,  101,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,

      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  109,
      357,  133,  187,  133,  187, 1426,  109,  357,  357,  209,
      109, 1416,  187,  109,  133,  133,  133,  133,  134,  133,
      134,  212,  224,  109, 1415, 1023,  187, 1023,  109, 1341,
      152,  134,  134,  134,  134,  109,  134,  209,  109,  152,

      152,  109,  134,  135,  137,  135,  137,  247, 1334,  212,
      224,  109,  152,  133,  225,  187,  135,  135,  135,  135,
     1331,  135,  147,  147,  147,  147, 1330, 1314,  152,  152,
      134,  134,  137,  247,  147,  148,  148,  148,  148,  204,
      152,  137,  225,  148,  135,  204, 1313,  153,  147,  153,
     1671,  137,  148,  189,  203,  135,  190,  190,  153,  153,
      200,  137,  247,  190,  203,  189, 1309,  189,  204,  137,
      189,  153,  135,  204,  232,  200,  373,  147,  206,  137,
      206, 1304,  189,  203,  190,  190,  216,  153,  153,  200,
      216,  190,  203,  189,  210,  189,  206,  206,  189,  153,

      206, 1671,  219,  200,  230,  373,  231,  206,  210,  206,
      226,  210,  222, 1303,  232,  216,  219,  226, 1646,  216,
      242, 1646, 1302,  210,  206,  206,  222,  230,  206,  231,
      246,  219,  220,  220,  220,  220,  210,  236,  226,  210,
      237,  222,  232,  242,  219,  226,  227,  227,  227,  227,
      229,  229,  229,  229,  222,  234,  230,  238,  231,  235,
      235,  239,  234, 1300,  349,  236,  246, 1285,  237,  238,
     1272,  238,  242,  350,  238,  245,  245,  245, 1263,  252,
      252,  252,  252,  234, 1260, 1244,  238,  235,  235,  239,
      234,  252,  349,  259,  246,  259,  319,  238,  319,  238,

      263,  350,  238,  245, 1243,  252,  257,  256,  256,  256,
      256, 1203,  259,  257,  257,  257, 1157,  263,  326,  256,
      257,  351,  259, 1690,  259,  319,  354,  258,  325,  263,
      258,  375,  245,  256,  252,  257,  352,  258,  258,  258,
      259,  257,  257,  257,  258,  263,  260, 1712,  257,  351,
     1147,  325,  261, 1143,  319,  260,  258,  354,  326,  258,
      375,  262,  256,  260,  352,  258,  258,  258,  261,  261,
      264,  321,  258,  321, 1690,  260,  262, 1142,  262,  264,
      325,  261,  265,  260,  265,  354,  326,  264,  266,  321,
      262,  260,  267,  268, 1120,  331,  261,  261, 1712,  264,

      266,  265,  267,  267,  262,  266,  262,  264, 1110,  267,
      268,  265,  360,  265,  355,  264,  271,  266,  331,  360,
      360,  267,  268,  315,  315,  315,  315,  272,  266,  265,
      267,  267,  270,  266,  269,  274,  269,  267,  268,  269,
      271, 1109,  270,  272,  272,  273,  273,  331, 1096,  270,
      274,  372,  274,  269,  355, 1093,  272,  271,  271,  348,
     1078,  270,  273,  269,  274,  269,  275, 1074,  269,  271,
      270,  272,  272,  348,  273,  273,  276,  270,  274,  372,
      274,  269,  355,  275,  275,  271,  271,  382,  348,  278,
      273,  276,  344,  276,  344,  275,  277, 1066,  278,  279,

     1065,  348,  277,  280, 1058,  276,  278,  281, 1053, 1798,
      344,  275,  275,  277,  384,  382,  279,  279,  278,  276,
      280,  276,  281, 1029,  281,  277,  278,  925,  279,  282,
      918,  277,  280,  283,  278,  283,  281,  284,  285,  284,
      282,  277,  384,  399,  279,  279,  282,  284,  280,  912,
      281,  285,  281,  400,  868,  285,  287,  865,  282,  283,
     1798,  284,  366,  284,  366,  287,  287,  285,  282,  298,
      286,  399,  287,  287,  282,  366,  283,  366,  288,  285,
      284,  400,  286,  285,  286,  287,  298,  286,  283,  288,
      284,  861,  284,  287,  287,  288,  402, 1800,  298,  286,

      287,  287,  289,  397,  283,  290,  855,  288,  284,  290,
      286,  843,  286,  289,  298,  286,  293,  288,  841,  289,
      292,  622,  290,  288,  402,  293,  291,  291,  622,  622,
      292,  289,  397,  293,  290,  291, 1812,  292,  290,  336,
      294,  289,  336,  291,  403,  293,  336,  289, 1800,  292,
      290,  294,  365,  293,  365,  291,  291,  294,  292,  295,
      297,  293,  297,  291,  296,  292,  365,  336,  295,  294,
      336,  291,  403,  296,  336,  826,  295,  297,  300,  294,
      404,  296,  299,  398,  300,  294,  301, 1812,  295,  297,
      401,  297,  301,  296,  300,  300,  295,  299,  301,  299,

      299,  296,  820,  301,  295,  297,  798,  300,  404,  296,
      405,  299,  398,  300,  406,  301,  302,  302,  303,  401,
      303,  301,  300,  300,  304,  299,  301,  299,  299,  411,
      374,  301,  374,  790,  302,  374,  303,  303,  405,  789,
      303,  304,  406,  304,  711,  302,  302,  303,  412,  303,
      305,  710,  593,  304,  593,  306,  308,  308,  411,  305,
      415,  307,  302,  307,  303,  303,  682,  305,  303,  304,
      593,  304,  306,  306,  308,  307,  309,  412,  307,  305,
      307,  310,  311,  310,  306,  308,  308,  305,  311,  415,
      307,  309,  307,  309,  385,  305,  385,  385,  310,  311,

      306,  306,  308,  307,  681,  309,  307,  312,  307,  312,
      310,  311,  310,  327,  313,  327,  313,  311,  416,  309,
      313,  309,  314, 1031,  312, 1031,  310,  311,  314,  661,
      660,  313,  316,  316,  316,  316,  312,  409,  312,  314,
      656,  410,  327,  313,  316,  313,  328,  416,  328,  313,
      424,  314,  312,  317,  317,  317,  317,  314,  316,  313,
      655,  329,  426,  329,  328,  409,  652,  314,  345,  410,
      345,  327, 1033,  334, 1033,  328,  419,  329,  424,  329,
      330,  330,  330,  330,  334,  335,  345,  316,  329,  329,
      426,  334,  330,  337,  337,  337,  335,  345,  650,  642,

      337,  329,  334,  335,  328,  419,  330,  358,  358,  358,
      639,  632,  334,  632,  335,  358,  358,  329,  329,  334,
      638,  337,  337,  337,  335,  632,  345,  338,  337,  329,
     1116,  335, 1116,  338,  338,  330,  332,  332,  332,  332,
      332,  332,  332,  332,  332,  332,  332,  332,  332,  332,
      332,  332,  332,  332,  332,  338,  332,  332,  332,  332,
      332,  338,  338,  339,  340,  341,  342,  359,  359,  359,
      422,  387,  339,  387,  387,  359,  359,  341,  342,  339,
      342,  340,  341,  342,  346,  423,  346,  332,  332,  623,
      637,  624,  339,  340,  341,  342,  623,  623,  588,  422,

      339, 1247,  430, 1247,  430,  341,  342,  339,  342,  340,
      341,  342,  430,  346,  423,  332,  333,  333,  333,  333,
      333,  333,  333,  333,  333,  333,  333,  333,  333,  333,
      333,  333,  333,  333,  333,  427,  333,  333,  333,  333,
      333,  343,  346,  363,  347,  582,  347,  367,  364,  367,
      363,  363,  333,  560,  343,  364,  364,  429,  343,  333,
      347,  367,  347,  367,  427,  367,  407,  333,  333,  407,
      343,  347,  347,  347,  368,  508,  368,  363,  507,  503,
      333,  425,  343,  425,  347,  429,  343,  333,  368,  432,
      368,  364,  368,  502,  407,  333, 1251,  407, 1251,  367,

      347,  347,  347,  425,  370,  363,  370,  371,  376,  371,
      376,  499,  347,  420,  381,  420,  381,  432,  370,  364,
      370,  371,  370,  371,  376,  371,  368,  431,  480,  437,
      381,  386,  386,  386,  386,  420,  431,  431,  478,  370,
      421,  421,  396,  386,  390,  390,  390,  390,  428,  420,
      428,  371,  390,  376,  381, 1852,  370,  386,  437,  371,
      413,  390,  414,  391,  391,  391,  391,  370,  421,  421,
      408,  391,  433,  440,  394,  391,  413,  420,  414,  371,
      391,  376,  381,  394,  394,  436,  386,  408,  434,  391,
      395,  428,  435,  438,  439,  441,  394,  441,  436,  408,

      433,  413,  440,  414,  442,  392, 1852,  443,  444,  445,
      447,  451,  394,  394,  436,  408,  434,  441,  391,  428,
      435,  438,  439,  446,  394,  446,  436,  448,  449,  413,
      450,  414,  417,  442,  417,  443,  453,  444,  445,  447,
      451,  454,  455,  456,  458,  446,  389,  388,  459,  457,
      383,  380,  462,  460,  417,  448,  449,  457,  450,  417,
      379,  460,  463,  464,  417,  453,  465,  466,  471,  454,
      455,  467,  456,  458,  417,  417,  459,  457,  417,  461,
      462,  461,  460,  468,  469,  457,  488,  470,  417,  460,
      463,  472,  464,  417,  473,  465,  466,  471,  475,  467,

      476,  461,  417,  417,  477,  479,  417,  481,  482,  483,
      484,  468,  469,  485,  461,  470,  486,  487,  489,  472,
      488,  490,  473,  461,  491,  493,  475,  591,  476,  494,
      495,  496,  477,  479,  497,  481,  482,  483,  484,  492,
      378,  485,  461,  495,  486,  509,  487,  514,  488,  377,
      490,  461,  510,  491,  493,  591,  492,  369,  494,  495,
      496,  489,  512,  497,  498,  498,  498,  500,  492,  500,
      500,  495,  509,  511,  501,  487,  501,  501,  504,  513,
      504,  504,  515,  510,  492,  505,  505,  505,  505,  489,
      514,  506,  497,  506,  506,  516,  498,  505,  517,  512,

      518,  509,  521,  522,  362,  524,  520,  511,  361,  519,
      513,  505,  510,  525,  525,  515,  907,  539,  514,  523,
      356,  526,  527,  907,  907,  498,  532,  512,  530,  516,
      525,  520,  524,  538,  353,  511,  521,  522,  517,  513,
      505,  518,  523,  515,  519,  526,  324,  519,  520,  527,
      322,  529,  529,  529,  539,  525,  535,  516,  538,  530,
      520,  524,  533,  534,  521,  522,  517,  532,  532,  518,
      537,  523,  519,  529,  526,  519,  520,  542,  527,  531,
      531,  531,  539,  525,  528,  528,  528,  538,  530,  533,
      534,  536,  536,  536,  540,  532,  532,  535,  541,  543,

      546,  531,  547,  551,  318,  537,  528,  544,  545,  255,
      548,  528,  254,  536,  557,  531,  528,  250,  533,  534,
      542,  550,  249,  552,  553,  535,  528,  528,  540,  243,
      528,  559,  541,  537,  544,  545,  548,  549,  549,  549,
      528,  546,  543,  531,  547,  528,  551,  544,  542,  552,
      555,  553,  550,  556,  528,  528,  540,  558,  528,  549,
      541,  557,  559,  544,  545,  548,  554,  554,  554,  546,
      543,  561,  547,  562,  551,  544,  563,  555,  552,  564,
      553,  550,  565,  567,  570,  571,  240,  556,  554,  557,
      566,  559,  614,  558,  561,  576,  568,  573,  577,  228,

      572,  575,  221,  569,  569,  569,  555,  574,  580,  578,
      564, 1252,  563, 1252,  562,  556,  584,  567,  565,  566,
      614,  558,  568,  561,  573,  569,  565,  570,  571,  581,
      568,  572,  579,  583,  574,  127,  577,  576,  569,  564,
      563,  575,  562,  585,  580,  567,  565,  569,  566,  578,
      584,  568,  596,  573,  565,  570,  571,  123,  568,  611,
      572,  579,  119,  574,  577,  576,  569,  118,  595,  575,
       78,  581,  580,  583,   72,  569,  600,  578,  584,  586,
      586,  586,  586,  611,   69,  585,  587,  587,  587,  587,
      579,  589,  589,  589,  589,  596,  594,   17,  594,  581,

      603,  583,  595,  589,  597,  600,  597,  603,  598,  609,
      598,  604,  611,  585,  594,    9,  609,  589,  653,    7,
      653,  653,  605,  596,  604,  594,  598,    0,    0,  603,
      595,  606,    0,  597,  600,  603,  605,  598,  609,  606,
      604,  599,  606,  599,  609,  612,  589,  590,  590,  590,
      590,  605,  604,  617,  594,  608,  613,  599,  619,  599,
      606,  615,  597,  610,  605,  616,  598,  606,  599,  599,
      606,  608,  612,  590,  590,  666,  590,  590,  631,  590,
      631,  599,  617,  613,  608,  612,  610,  619,  618,  615,
      590,    0,  631,  616,    0,    0,  631,  599,  599,  608,

      620,  612,  590,  590,  666,  590,  590, 1638,  590,  599,
        0,    0,  613,  612, 1638,  610,  618, 1638,  590,  601,
      601,  601,  601,  601,  601,  601,  601,  601,  601,  601,
      601,  601,  601,  601,  601,  601,  601,  601,  620,  601,
      601,  601,  601,  601,  607,  627,  607,  627,  649,  625,
      628,  635,  628,  635,    0,  601,  625,  625,  627,  663,
      627,  607,  636,  628,  636,  628,  620,  635,    0,    0,
      601,  601,  621,  607,    0,  607,  649,    0,  636,  630,
        0,  630,  654,  601,  654,  654, 1342,  663, 1342,  607,
      625,    0, 1343,  630, 1343,  667,  621,  630,  601,  602,

      602,  602,  602,  602,  602,  602,  602,  602,  602,  602,
      602,  602,  602,  602,  602,  602,  602,  602,  625,  602,
      602,  602,  602,  602,  667,  621,  626,  629,  602,  629,
      668,  630,    0,  626,  626,    0,  664,  671,  629,  640,
      629,  640,  629,  629,  633,  629,  633,  634,  665,  634,
      602,  602,  647,  626,  647,  640,  669,  602,  633,  668,
      633,  634,  633,  634,  664,  634,  671,  673,  647,  672,
      744,  692,  744,  692,    0,  634,  665,  674,  602,  629,
      744,  675,  626,  678,  669,  679,  633,  676,    0,  657,
      657,  657,  657,  692,  677,  673,  633,  657,  672,  634,

      658,  658,  658,  658,  634,  674,  657,  680,  658,  675,
      683,  678,  658,  679,  633,  687,  676,  658,  688,  659,
      659,  659,  659,  677,  689,  691,  658,  659,  690,  662,
      662,  662,  662,  694,  702,  680,  659,  662,  683,  684,
      695,  684,  695,  687,  697,  698,  662,  688,  700,  706,
      707,    0,    0,  689,  691,  658,  690,  701,  703,  701,
      703,  694,  695,  702,  705,  709,  705,  714,  713,    0,
      715,  684,  697,  698,  709,  709,  700,  706,  707,  701,
      703,  712,  712,  712,  719,  684,  705,  726,  716,  718,
      720,  712,  712,  721,  712,  714,  712,  713,  715,  717,

      684,  725,  722,  712,  722,  723,  717,  723,  732,  727,
      734,  727,  719,  684,  685,  726,  685,  716,  718,  720,
      735,  736,  721,  728,  722,  728,  738,  723,  717,    0,
      725,  727,  737,  729,  717,  729,  685,  732,  739,  734,
      740,  685,  741,    0,  742,  728,  685,  743,  735,  736,
      749,  746,  750,  746,  738,  729,  685,  685,  728,  712,
      685,  737,  751,  752,  754,  748,  739,  748,  740,  755,
      685,  741,  742,  746,  756,  685,  743,  758,  749,  759,
      750,  761,  762,  761,  685,  685,  728,  748,  685,  763,
      751,  752,  764,  754,  766,  772,  767,  768,  755,  769,

      770,  771,  756,  761,  773,  758,  774,  777,  759,  775,
      779,  762,  782,  791,  784,  777,  792,    0,  763,  795,
      780,  764,  794,  766,  767,    0,  768,  780,  769,  770,
      771,  780,  781,  774,  781,  772,  777,  775,  785,  779,
      785,  785,  781,  793,  777,  773,  781,  794,  795,  780,
      784,  781,  782,  792,  791,  780,  783,  783,  783,  780,
      796,  797,  774,  772,  786,  802,  786,  786,  787,  799,
      787,  787,  788,  773,  788,  788,  794,  795,  784,  800,
      782,  792,  791,  801,  793,  803,  805,  796,  783,  807,
      806,  804,  799,  808,  811,  811,  811,  810,  814,  802,

      812,  817,  797,  815,  816,  813,  800,  821,  818,    0,
      900,  819,  793,  805,    0,  828,  796,  783,  875,  825,
      801,  799,  804,    0,  806,  814,  811,  802,  803,  808,
      797,  810,  807,  813,  816,  800,  823,  812,  829,  900,
      811,  827,  805,  830,    0,  817,  875,  821,  801,  818,
      815,  804,  806,  819,  814,  811,  803,  808,  828,  810,
      807,  825,  813,  816,  831,  812,  823,  832,  811,  822,
      822,  822,  827,  817,  830,  821,  833,  818,  815,  835,
      829,  819,  824,  824,  824,  834,  828,  838,  837,  825,
        0,  822,  844,  831,  846,  823,  848,    0,  832,  842,

      831,  827,  835,  830,  824,  849,  852,  845,  829,  847,
      850,  857,  833,  837,  834,  836,  836,  836,  851,  844,
      860,  838,  831,  839,  839,  839,  858,  832,  831,  846,
      842,  835,  840,  840,  840,    0,  847,  836,  848,    0,
      833,  851,  837,  834,  845,  839,  852,  849,  844,  838,
      853,  857,  850,  859,  840,  866,  860,  846,  862,  842,
      858,  854,  854,  854,  863,  847,  848,  840,  864,  867,
      851,  854,  845,  870,  852,  849,  856,  856,  856,  857,
      850,  853,  871,  862,  860,  869,  869,  869,  858,  863,
      872,  880,  866,  859,  899,  840,  867,  960,  856,  960,

      870,  873,  873,  873,  873,    0,  877,  869,  877,  871,
      853,  864,  862,  874,  874,  874,  874,  872,  863,  960,
      866,  859,  899,    0,  877,  867,  878,  881,  878,  870,
        0,  883,  880,  879,  887,  879,  887,  902,  871,  864,
      886,  882,  886,  882,  878,    0,  872,  876,  876,  876,
      876,  879,  896,    0,  881,  878,    0,  882,  887,  882,
      880,  904,  879,    0,  927,  876,  902,  928,  882,  882,
        0,  883,  929,  876,  876,  896,  876,  876,  889,  876,
      889,  882,  886,  881,  878,  904,  913,  887,  913,  931,
      876,  879,  927,    0,  891,  928,  891,  882,  882,  883,

      913,  929,  876,  876,  896,  876,  876,    0,  876,  882,
      886,  890,    0,  890,  904,    0,  889,  931,  876,  884,
      884,  884,  884,  884,  884,  884,  884,  884,  884,  884,
      884,  884,  884,  884,  884,  884,  884,  884,  891,  884,
      884,  884,  884,  884,  889,    0,  890,  895,  895,  895,
      898,  898,  898,  930,  908,  921,  908,  921,  921,  933,
      898,  888,  934,  888,  898,  908,  891,  936,  937,  898,
      884,  884,    0,  922,  890,  922,  922,  923,  923,  923,
      923, 1041,  930, 1041, 1041,  923, 1003,  933, 1003,  888,
     1042,  934, 1042, 1042,  923,  936, 1003,  937,  884,  885,

      885,  885,  885,  885,  885,  885,  885,  885,  885,  885,
      885,  885,  885,  885,  885,  885,  885,  885,  888,  885,
      885,  885,  885,  885,  892,  893,  892,  893,  906,  894,
      897,  894,  938,  941,  942,  906,  906,  924,  924,  924,
      924,  943,  944,  946,    0,  924,  948,  910,  892,  910,
      885,  885,    0,  893,  924,  906,  892,  894,  910,  897,
      938,  941,  942,  910,  947,  910,  897,  949,    0,  943,
      897,  944,  946,  914,  948,  914,    0,  892,  885,  911,
      950,  911,  893,  951,  906,  892,  894,  914,  897,  914,
      911,  914,  952,  947,  897,  911,  949,  911,  897,  910,

     1144,  914,  926,  926,  926,  926,  955, 1144, 1144,  950,
      926,  951,    0, 1020,  926, 1020, 1111, 1111, 1111,  926,
     1412,  952, 1412, 1020,    0,  914, 1111,  945,  926,  945,
      914,  911,  915,  915,  955,  915,  915,  915,  915,  915,
      915,  915,  915,  915,  915,  915,  915,  915,  915,  915,
      915,  915,  915,  915,  915,  915,  915,  926,  939,  945,
        0,  956,  957,  958,    0,  939,  959,    0,  961,  967,
      973,  974,  962,  945,  962,  965,  976,  965,  977,    0,
      978,  979,  982,  915,  915,  915,    0,  939,  945,  956,
        0,  957,  958,  939,  962,  959,  961,  965,  967,  973,

      974,  945,  980, 1043,  976, 1043, 1043,  977,  978,  979,
      982,  915,  932,  932,  985,  932,  932,  932,  932,  932,
      932,  932,  932,  932,  932,  932,  932,  932,  932,  932,
      932,  980,  932,  932,  932,  932,  932,  969,  970,  969,
      970,    0,    0,  985, 1237,  981, 1237,  981,  969,  970,
      969,  970,  969,  970, 1237,  971,  971,  971,  986,  969,
      970,  991,  972,  932,  932,  971,  971,  981,  971,  992,
      971,  972,  972,  983,  972,  983,  972,  971,  987,  989,
      987,  989,  993,  972,  994,  995,    0,  986,  997,  998,
      991,  932, 1000, 1001, 1002,  983, 1007, 1005,  992, 1005,

      987,  989, 1008, 1009, 1011, 1010, 1012,    0, 1013, 1014,
     1015,  993,  994, 1022,  995,  969,  970,  997,  998, 1005,
     1000, 1024, 1001, 1002, 1007, 1016, 1025, 1016, 1027, 1028,
     1008, 1009, 1011,  971, 1010, 1012, 1013, 1014, 1015,  972,
     1030, 1026, 1022, 1026, 1032, 1040, 1034, 1016, 1035, 1045,
     1024, 1026, 1039, 1046, 1025, 1026, 1028, 1047, 1051, 1027,
     1026, 1036, 1038, 1036, 1038, 1044, 1049, 1044, 1044, 1030,
     1048, 1036, 1052, 1032, 1034, 1036, 1038, 1035,    0,    0,
     1036, 1038, 1126, 1039, 1047, 1028, 1114, 1040, 1027, 1046,
     1045,    0, 1054, 1048, 1055,    0, 1051, 1052, 1072, 1072,

     1072,    0, 1049, 1112, 1112, 1112, 1112, 1113, 1113, 1113,
     1113, 1126, 1039, 1047, 1114, 1040,    0, 1046, 1045, 1056,
     1072, 1055, 1048, 1059, 1051, 1061, 1052, 1060, 1054, 1067,
     1049, 1050, 1050, 1050, 1050, 1050, 1050, 1050, 1050, 1050,
     1050, 1050, 1050, 1050, 1050, 1050, 1050, 1050, 1050, 1050,
     1055, 1050, 1050, 1050, 1050, 1050, 1054, 1057, 1062, 1056,
     1059, 1060, 1063, 1067, 1068, 1064, 1061, 1073, 1079, 1069,
     1071, 1081, 1077, 1075, 1070, 1076, 1082, 1083, 1080, 1084,
     1084, 1084, 1050, 1050, 1091, 1062, 1057, 1056, 1059, 1060,
     1063, 1067, 1088, 1057, 1061, 1064, 1069, 1070, 1077, 1071,

     1076, 1084, 1068, 1073, 1075, 1080, 1083, 1085, 1079, 1086,
     1050, 1087, 1082, 1081, 1062, 1057, 1089, 1090, 1092, 1063,
     1091, 1057, 1095, 1088, 1064, 1069, 1070, 1077, 1071, 1076,
     1068, 1073, 1086, 1075, 1080, 1083, 1079, 1094, 1087, 1097,
     1082, 1081, 1098, 1090, 1089, 1092, 1099, 1100, 1091, 1085,
     1101, 1102, 1088, 1095, 1104, 1107, 1106, 1103, 1105, 1108,
     1117, 1086, 1117, 1119, 1094, 1098,    0, 1087, 1122,    0,
        0, 1099, 1090, 1089, 1092,    0, 1118, 1085, 1118, 1166,
        0, 1097, 1095, 1103, 1127, 1105,    0, 1102, 1100, 1117,
     1119,    0, 1101, 1094, 1098, 1127, 1104, 1106, 1107, 1122,

     1099, 1108, 1123, 1123, 1123, 1118, 1128, 1166, 1128, 1097,
     1134,    0, 1103, 1127, 1105, 1102, 1100,    0, 1117, 1119,
     1101, 1133, 1136, 1127, 1104, 1106, 1107,    0, 1122, 1108,
     1115, 1115, 1115, 1115, 1118, 1128, 1121, 1128, 1121, 1130,
     1129, 1145, 1131,    0, 1134, 1130, 1131,    0, 1145, 1145,
     1133, 1136, 1121, 1131, 1121, 1129, 1115, 1115, 1146, 1115,
     1115, 1169, 1115, 1121, 1121, 1146, 1146, 1135, 1130, 1129,
        0, 1131, 1134, 1115, 1130, 1131, 1121, 1325, 1325, 1325,
     1152, 1131, 1152, 1129,    0, 1115, 1115,    0, 1115, 1115,
     1169, 1115, 1121, 1121, 1152, 1135, 1159,    0, 1159, 1159,

     1160, 1115, 1160, 1160, 1121, 1124, 1124, 1124, 1124, 1124,
     1124, 1124, 1124, 1124, 1124, 1124, 1124, 1124, 1124, 1124,
     1124, 1124, 1124, 1124, 1135, 1124, 1124, 1124, 1124, 1124,
     1148, 1149, 1148, 1149, 1124, 1150, 1167, 1150, 1153, 1170,
     1153, 1148, 1149, 1163,    0, 1163, 1150,    0, 1148, 1149,
     1172, 1153, 1154, 1153, 1154, 1173, 1124, 1124, 1155, 1156,
     1155, 1156,    0, 1124, 1167, 1163, 1154, 1170, 1154,    0,
     1154, 1202, 1155, 1156, 1155, 1156, 1155, 1156, 1172,    0,
     1202, 1202, 1148, 1173, 1124, 1125, 1125, 1125, 1125, 1125,
     1125, 1125, 1125, 1125, 1125, 1125, 1125, 1125, 1125, 1125,

     1125, 1125, 1125, 1125, 1154, 1125, 1125, 1125, 1125, 1125,
     1155, 1156, 1125, 1161, 1161, 1161, 1161, 1162, 1162, 1162,
     1162, 1161, 1164, 1174, 1164, 1162, 1175, 1176,    0, 1177,
     1161, 1178, 1180, 1181, 1162, 1186, 1125, 1125, 1332,    0,
     1253, 1125, 1253, 1253, 1164, 1332, 1332, 1335, 1199, 1335,
     1199, 1174, 1199,    0, 1175, 1183, 1176, 1177, 1335, 1199,
     1178, 1180, 1181, 1186, 1125, 1132, 1132, 1132, 1132, 1132,
     1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132,
     1132, 1132, 1132, 1132, 1183, 1132, 1132, 1132, 1132, 1132,
     1187, 1188, 1189, 1190,    0, 1191, 1192, 1193, 1196, 1193,

     1197, 1206, 1207, 1194, 1208, 1194, 1418, 1209, 1418, 1211,
     1212, 1216, 1336, 1218, 1336, 1199, 1132, 1132, 1187, 1193,
     1188, 1189, 1190, 1191, 1192, 1194, 1336, 1196, 1197,    0,
     1206, 1207, 1201, 1208, 1201, 1209, 1201, 1211, 1212, 1216,
     1217, 1218, 1217, 1201, 1132, 1151, 1151,    0, 1151, 1151,
     1151, 1151, 1151, 1151, 1151, 1151, 1151, 1151, 1151, 1151,
     1151, 1151, 1151, 1151, 1151, 1151, 1151, 1151, 1151, 1151,
        0,    0, 1198, 1200, 1198, 1200, 1219, 1221,    0, 1217,
     1213, 1222, 1213, 1198, 1200, 1198, 1200, 1198, 1200, 1223,
     1204, 1204, 1204, 1224, 1198, 1200, 1151, 1151, 1151, 1201,

     1204, 1204, 1213, 1204, 1219, 1204, 1221, 1217, 1254, 1222,
     1254, 1254, 1204, 1310, 1310, 1310, 1310, 1223, 1179,    0,
     1179, 1224,    0, 1225, 1151, 1165, 1165, 1226, 1165, 1165,
     1165, 1165, 1165, 1165, 1165, 1165, 1165, 1165, 1165, 1165,
     1165, 1165, 1165, 1165, 1179, 1165, 1165, 1165, 1165, 1165,
     1198, 1200, 1225, 1205, 1228, 1226, 1215, 1179, 1215, 1220,
     1235, 1220, 1205, 1205, 1232, 1205, 1232, 1205, 1204, 1233,
     1234, 1233, 1234, 1179, 1205, 1240, 1165, 1165, 1215, 1239,
     1241, 1220, 1242, 1228, 1245, 1179, 1246, 1248, 1235, 1249,
     1232, 1233, 1234, 1250,    0,    0, 1255, 1255, 1255, 1256,

     1256, 1256, 1312, 1240, 1165, 1258, 1259, 1319, 1239, 1241,
     1242,    0,    0, 1245, 1246,    0, 1248, 1249, 1255, 1232,
     1250, 1256, 1278, 1278, 1278, 1261, 1311, 1311, 1311, 1311,
     1205, 1312, 1264,    0, 1262, 1319, 1266,    0, 1267, 1242,
     1265, 1259, 1269, 1276, 1278, 1287, 1249, 1258, 1261, 1250,
     1257, 1257, 1257, 1257, 1257, 1257, 1257, 1257, 1257, 1257,
     1257, 1257, 1257, 1257, 1257, 1257, 1257, 1257, 1257, 1259,
     1257, 1257, 1257, 1257, 1257, 1258, 1262, 1261, 1264, 1266,
     1265, 1267, 1270, 1276, 1269, 1268, 1280, 1287, 1271, 1271,
     1271, 1273, 1274, 1275, 1277, 1284, 1286, 1282, 1279, 1281,

     1289, 1257, 1257,    0, 1262, 1283, 1264, 1266, 1265, 1267,
     1268, 1276, 1269, 1270, 1271, 1287, 1291, 1292, 1273, 1274,
     1275, 1279,    0, 1280, 1282, 1298,    0, 1271, 1277, 1257,
     1281, 1294, 1283, 1295, 1289, 1284, 1296, 1286,    0, 1268,
     1297, 1352, 1270, 1271, 1288, 1288, 1288, 1273, 1274, 1275,
     1279, 1280, 1298, 1282, 1294, 1271, 1277, 1291, 1301, 1281,
     1299, 1283, 1289, 1284, 1292, 1286, 1288, 1290, 1290, 1290,
     1352, 1293, 1293, 1293, 1308, 1295, 1296, 1297, 1306, 1306,
     1306, 1298, 1315, 1294, 1326, 1291, 1305, 1305, 1305, 1301,
        0,    0, 1292, 1293, 1320, 1299, 1321, 1307, 1307, 1307,

     1306, 1322, 1323, 1295, 1296, 1297, 1290, 1326,    0, 1327,
     1315,    0, 1305, 1333, 1353, 1424, 1308, 1424, 1301, 1307,
     1333, 1333, 1320, 1299, 1321, 1337, 1425, 1337, 1425, 1322,
     1323, 1762,    0, 1762, 1290, 1354, 1326, 1327, 1337, 1315,
     1337, 1305, 1762, 1353, 1308, 1316, 1316, 1316, 1316, 1316,
     1316, 1316, 1316, 1316, 1316, 1316, 1316, 1316, 1316, 1316,
     1316, 1316, 1316, 1316, 1354, 1316, 1316, 1316, 1316, 1316,
     1338, 1339, 1338, 1339, 1344, 1344, 1344, 1344,    0,    0,
     1356, 1346, 1344, 1346, 1338, 1339, 1338, 1339, 1338, 1339,
     1357, 1344, 1340, 1348, 1340, 1348, 1316, 1316,    0, 1345,

     1345, 1345, 1345, 1346,    0,    0, 1340, 1345, 1340, 1356,
     1340, 1422,    0, 1422, 1422, 1348, 1345, 1480, 1357, 1480,
     1480,    0, 1338, 1339, 1316, 1317, 1317, 1317, 1317, 1317,
     1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317, 1317,
     1317, 1317, 1317, 1317, 1340, 1317, 1317, 1317, 1317, 1317,
     1351, 1358, 1359, 1366, 1351,    0, 1361, 1363, 1364, 1367,
     1368, 1369,    0, 1370, 1371, 1373, 1374,    0, 1377, 1317,
     1375, 1378, 1375, 1382, 1386,    0, 1317, 1317,    0, 1351,
     1358, 1359, 1366, 1351, 1361, 1363, 1364, 1367,    0, 1368,
     1369, 1370, 1375, 1371, 1373, 1374, 1377, 1317, 1362, 1378,

     1362, 1382,    0, 1386, 1317, 1318, 1318, 1318, 1318, 1318,
     1318, 1318, 1318, 1318, 1318, 1318, 1318, 1318, 1318, 1318,
     1318, 1318, 1318, 1318, 1362, 1318, 1318, 1318, 1318, 1318,
     1379, 1380, 1379, 1380, 1391, 1380, 1391, 1362, 1387,    0,
     1392, 1379, 1380, 1379, 1388, 1379, 1388, 1393, 1381, 1381,
     1381, 1397, 1379, 1362, 1398, 1421, 1318, 1318, 1381, 1381,
     1395, 1381, 1395, 1381, 1400, 1362, 1388, 1387, 1392, 1403,
     1381, 1411,    0, 1391, 1404, 1393, 1404, 1405, 1428, 1397,
     1410,    0, 1395, 1398, 1318, 1406, 1413, 1406, 1408, 1417,
     1408, 1419, 1400, 1429, 1430, 1421, 1433, 1403, 1380, 1411,

     1404, 1391, 1435, 1432, 1427, 1428, 1405, 1406, 1379, 1410,
     1408, 1434, 1437, 1439, 1438, 1413, 1441, 1417, 1429, 1443,
     1419, 1430, 1440, 1421, 1445, 1435, 1381, 1427, 1444, 1404,
     1432, 1427, 1446, 1433, 1428, 1451, 1447, 1457, 1434, 1452,
     1454, 1441, 1464, 1455,    0, 1458, 1443, 1429, 1463, 1437,
     1430, 1438, 1460, 1439, 1435, 1444, 1427,    0, 1445, 1432,
     1427, 1433, 1440, 1468, 1472, 1464, 1451, 1434, 1446, 1452,
     1441, 1457, 1458, 1465, 1474, 1443, 1477, 1437, 1447, 1438,
     1454, 1439, 1478,    0, 1444, 1455, 1445, 1475, 1460, 1476,
     1440, 1468, 1463, 1472, 1464, 1451, 1446, 1484, 1452, 1457,

     1486, 1458, 1487, 1474, 1465, 1477, 1447, 1478, 1454, 1466,
     1466, 1466, 1466, 1455,    0, 1475, 1460, 1476,    0, 1479,
     1463, 1479, 1481, 1481, 1481, 1488, 1484, 1489, 1486, 1479,
     1481, 1487, 1490, 1465, 1492, 1493, 1478, 1494, 1495, 1481,
     1482, 1482, 1482, 1482, 1483, 1483, 1483, 1483, 1482, 1496,
     1497, 1499, 1483, 1488, 1498, 1489, 1500, 1482, 1502,    0,
     1490, 1483, 1492, 1493, 1503, 1494, 1495, 1505, 1511, 1510,
     1511, 1517, 1512, 1517, 1513, 1516, 1518, 1496, 1519, 1497,
     1499, 1520, 1498, 1521, 1522, 1500, 1508, 1502, 1508, 1523,
     1511, 1524, 1503, 1517, 1525, 1505, 1527, 1508, 1510, 1508,

     1512, 1508, 1513, 1516, 1528, 1518, 1529, 1519, 1508, 1520,
     1536, 1521, 1522, 1530, 1533, 1530, 1530, 1524, 1523, 1531,
     1535, 1531, 1531, 1525, 1543, 1527, 1538, 1539, 1537, 1541,
     1544, 1528, 1546, 1542, 1529,    0, 1557, 1536, 1533, 1548,
     1552, 1554, 1560, 1561,    0, 1545, 1524, 1562, 1562, 1562,
     1562, 1553, 1553, 1553, 1559,    0, 1535, 1544,    0, 1543,
     1528, 1565, 1538, 1529, 1508, 1552, 1536, 1533, 1537, 1542,
     1539, 1541, 1545, 1553, 1546, 1548, 1557, 1566, 1554, 1561,
     1585, 1559, 1564, 1560, 1535,    0, 1544, 1543, 1568, 1565,
     1538, 1558, 1558, 1558, 1552, 1579, 1537, 1542, 1539, 1541,

     1570, 1545, 1546, 1548, 1557, 1566, 1554, 1561, 1564, 1585,
     1559, 1560, 1563, 1558, 1563, 1571, 1568, 1571, 1573,    0,
     1573, 1573, 1563, 1579, 1574, 1571, 1574, 1570, 1575, 1575,
     1575, 1575, 1576, 1576, 1576, 1588, 1575, 1564, 1580, 1590,
     1576, 1574, 1583, 1584, 1587, 1575, 1578, 1578, 1578, 1576,
     1577, 1577, 1577, 1577, 1578, 1589, 1570, 1591, 1577, 1592,
     1593, 1592, 1596, 1578, 1588, 1600, 1580, 1577, 1590, 1592,
     1583, 1584, 1587, 1598, 1601, 1598, 1603, 1606, 1603, 1607,
     1608, 1610, 1611, 1589, 1612, 1614, 1591, 1619, 1593, 1618,
     1615, 1596, 1622, 1600, 1616, 1598, 1616, 1616, 1603, 1623,

     1624, 1626,    0, 1601, 1639, 1606, 1627, 1630, 1607, 1608,
     1610, 1632, 1612, 1615, 1628, 1628, 1628, 1635, 1636, 1611,
     1637, 1614, 1637, 1618, 1628, 1641, 1642, 1624,    0, 1619,
     1637, 1627, 1630,    0, 1622, 1651, 1632,    0, 1656, 1626,
     1623, 1639, 1615, 1657, 1635, 1636,    0, 1611, 1652, 1614,
     1652, 1618,    0, 1641, 1642, 1659, 1624, 1619, 1652,    0,
     1627, 1630, 1622,    0, 1651, 1632, 1656, 1626, 1623, 1639,
     1658,    0, 1657, 1635, 1636, 1645, 1645, 1645, 1645, 1647,
     1647, 1647, 1647, 1645, 1659, 1664, 1645, 1647, 1648, 1648,
     1648, 1649, 1649, 1649, 1649, 1665, 1647, 1666, 1658, 1649,

     1660, 1661, 1660, 1661, 1667, 1648, 1673, 1680, 1649, 1674,
     1660, 1661, 1675, 1681, 1664, 1676, 1676, 1676, 1682, 1683,
     1686, 1689,    0,    0, 1665, 1676, 1666, 1686, 1689,    0,
     1686, 1689, 1673, 1667, 1674,    0,    0, 1702, 1700, 1675,
     1700, 1680, 1703, 1704, 1681, 1682, 1683, 1822, 1700, 1822,
     1687, 1687, 1687, 1687, 1710,    0, 1688, 1688, 1688, 1688,
        0, 1673, 1687, 1674, 1688, 1702, 1705, 1688, 1675, 1680,
        0, 1703, 1704, 1681, 1682, 1683, 1687, 1693, 1693, 1693,
     1693, 1694, 1694, 1694, 1694, 1693, 1695, 1713, 1693, 1694,
     1710,    0, 1694, 1695, 1705, 1707, 1695, 1708, 1696, 1696,

     1696, 1696, 1715, 1711, 1716, 1687, 1696, 1718, 1697, 1697,
     1697, 1697, 1733, 1736, 1713, 1696, 1697, 1701, 1710, 1701,
     1697, 1727,    0,    0, 1707, 1697, 1708,    0,    0, 1715,
     1701, 1711, 1701, 1718, 1697, 1719, 1737, 1719, 1719, 1701,
        0, 1733, 1736, 1713, 1721, 1716, 1721, 1721, 1720, 1720,
     1720, 1720, 1734, 1724, 1724, 1724, 1724, 1727, 1715,    0,
     1720, 1724, 1718, 1697, 1724, 1737, 1741, 1725, 1725, 1725,
     1725, 1745,    0, 1716, 1720, 1725, 1726, 1728, 1725, 1725,
     1734, 1739, 1743, 1726, 1728, 1727, 1726, 1728, 1729, 1729,
     1729, 1729, 1741, 1725, 1730, 1731, 1729, 1731, 1732, 1729,

     1732, 1730, 1735, 1720, 1730, 1738, 1740, 1744, 1731, 1732,
     1731, 1732, 1764, 1732, 1745, 1765, 1797, 1731, 1739,    0,
     1732, 1741, 1725, 1746,    0, 1746, 1746,    0, 1743, 1747,
     1735, 1747, 1747, 1738, 1740,    0, 1767, 1815, 1744,    0,
     1764,    0, 1745,    0, 1765, 1797, 1739, 1750, 1750, 1750,
     1750, 1751, 1751, 1751, 1751, 1750, 1743, 1758, 1750, 1751,
        0,    0, 1751, 1751, 1767, 1753, 1815, 1744, 1752, 1752,
     1752, 1752, 1753, 1754, 1755, 1753, 1752, 1751, 1770, 1752,
     1754, 1755, 1757, 1754, 1755, 1756, 1756, 1756, 1756, 1757,
     1768, 1769, 1757, 1756, 1758, 1771, 1756, 1759, 1759, 1759,

     1759, 1760, 1760, 1760, 1760, 1759, 1751, 1761, 1759, 1760,
     1772, 1777, 1760, 1776, 1761,    0, 1794, 1761, 1768, 1769,
     1770, 1795, 1758, 1771, 1778,    0, 1778, 1778, 1779, 1782,
     1779, 1779, 1780, 1780, 1780, 1780, 1782, 1796,    0, 1782,
     1780,    0, 1813, 1780, 1794,    0,    0, 1772, 1770, 1795,
     1776, 1777, 1781, 1781, 1781, 1781, 1783, 1783, 1783, 1783,
     1781, 1784, 1785, 1781, 1783, 1796, 1788, 1783, 1784, 1785,
     1813, 1784, 1785, 1788, 1792, 1772, 1788, 1834, 1776, 1777,
     1786, 1786, 1786, 1786, 1791, 1787, 1787, 1787, 1787, 1801,
     1802, 1791, 1786, 1787, 1791, 1818, 1787, 1789, 1789, 1789,

     1789, 1790, 1790, 1790, 1790, 1789, 1786,    0, 1789, 1790,
        0, 1833, 1790, 1790, 1834, 1817, 1792, 1802, 1803,    0,
     1803, 1803, 1842, 1818, 1856, 1801, 1804, 1790, 1804, 1804,
     1805, 1805, 1805, 1805,    0, 1786, 1819,    0, 1805, 1833,
        0, 1805, 1834,    0, 1792,    0, 1802, 1806, 1806, 1806,
     1806, 1842, 1856, 1801,    0, 1806, 1790, 1817, 1806, 1807,
     1807, 1807, 1807, 1808, 1808, 1808, 1808, 1807, 1829, 1809,
     1807, 1808, 1819,    0, 1808, 1808, 1809, 1811,    0, 1809,
     1810, 1810, 1810, 1810, 1811, 1817, 1851, 1811, 1810, 1808,
        0, 1810, 1826, 1824, 1824, 1824, 1824, 1832,    0, 1826,

     1819, 1824, 1826,    0, 1824, 1825, 1825, 1825, 1825, 1835,
     1829, 1835, 1835, 1825, 1840, 1851, 1825,    0, 1808, 1823,
     1823, 1823, 1823,    0, 1823,    0, 1828, 1823,    0, 1823,
     1823, 1823,    0, 1828, 1823, 1823, 1828, 1832, 1829, 1823,
        0, 1823, 1823, 1823, 1827, 1827, 1827, 1827, 1836, 1836,
     1836, 1836, 1827, 1839, 1840, 1827, 1836, 1841,    0, 1836,
     1839, 1849,    0, 1839, 1843, 1832, 1838, 1838, 1838, 1838,
     1823, 1823, 1823, 1844, 1838, 1844, 1844, 1838, 1845, 1845,
     1845, 1845, 1840, 1846, 1841, 1847, 1845,    0, 1849, 1845,
     1846, 1855, 1847, 1846,    0, 1847, 1857, 1850, 1823, 1837,

     1837, 1837, 1837, 1837, 1837, 1858, 1843, 1837, 1837, 1837,
     1837, 1837,    0, 1841, 1837, 1837,    0, 1849, 1854, 1837,
     1850, 1837, 1837, 1837, 1848, 1848, 1848, 1848, 1853, 1853,
     1853, 1853, 1848, 1855, 1843, 1848, 1853, 1861, 1857, 1853,
     1869, 1854, 1859, 1859, 1859, 1859,    0, 1858,    0, 1850,
     1837, 1837, 1837, 1860, 1860, 1860, 1860, 1862, 1862, 1862,
     1862, 1855, 1863, 1863, 1863, 1863, 1857, 1869,    0,    0,
     1854, 1864, 1864, 1864, 1864, 1858,    0, 1861, 1837, 1866,
     1866, 1866, 1866, 1867, 1867, 1867, 1867, 1870, 1870, 1870,
     1870, 1871, 1871, 1871, 1871, 1881, 1869, 1873, 1873, 1873,

     1873, 1874, 1874, 1874, 1874, 1861, 1877, 1877, 1877, 1877,
     1878, 1878, 1878, 1878, 1880, 1880, 1880, 1880, 1881, 1883,
     1883, 1883, 1883, 1885, 1886, 1886, 1886, 1886, 1887, 1887,
     1887, 1887, 1888, 1888, 1888, 1888, 1890, 1890, 1890, 1890,
     1891, 1891, 1891, 1891,    0,    0,    0, 1881, 1894, 1894,
     1894, 1894, 1896, 1896, 1896, 1896,    0,    0,    0,    0,
        0,    0,    0,    0,    0, 1885,    0,    0,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,    0, 1885, 1898, 1898, 1898, 1898, 1898, 1898,

     1898, 1898, 1898, 1898, 1898, 1898, 1898, 1898, 1898, 1898,
     1898, 1898, 1899, 1899, 1899, 1899, 1899, 1899, 1899, 1899,
     1899, 1899, 1899, 1899, 1899, 1899, 1899, 1899, 1899, 1899,
     1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900,
     1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900, 1901, 1901,
        0, 1901, 1901, 1901, 1901, 1901, 1901, 1901, 1901, 1901,
     1901, 1901, 1901, 1901, 1901, 1901, 1902, 1902, 1902, 1902,
     1902, 1902, 1902, 1902, 1902, 1902, 1902, 1902, 1902, 1902,
     1902, 1902, 1902, 1902, 1903, 1903, 1903, 1903, 1903, 1903,
     1903, 1903, 1903, 1903, 1903, 1903, 1903, 1903, 1903, 1903,

     1903, 1903, 1904,    0,    0,    0,    0,    0,    0, 1904,
        0, 1904,    0, 1904, 1904, 1904, 1904, 1904, 1905, 1905,
     1905, 1905, 1905, 1906, 1906, 1906, 1906, 1906, 1906, 1906,
     1906, 1906, 1906, 1906, 1906, 1906, 1906, 1906, 1906, 1906,
     1906, 1907, 1907, 1907, 1907, 1907, 1907, 1907, 1907, 1907,
     1907, 1907, 1907, 1907, 1907, 1907, 1907, 1907, 1907, 1908,
     1908, 1908, 1908, 1908, 1908, 1908, 1908, 1908, 1908, 1908,
     1908, 1908, 1908, 1908, 1908, 1908, 1908, 1909, 1909, 1909,
     1909, 1909, 1909, 1909, 1909, 1909, 1909, 1909, 1909, 1909,
     1909, 1909, 1909, 1909, 1909, 1910,    0,    0,    0,    0,

        0,    0,    0,    0,    0,    0, 1910, 1910, 1910, 1910,
     1910, 1911, 1911, 1911, 1911, 1911, 1911, 1911, 1911, 1911,
     1911, 1911, 1911, 1911, 1911, 1911, 1911, 1911, 1911, 1912,
     1912,    0, 1912, 1912, 1912, 1912, 1912, 1912, 1912, 1912,
     1912, 1912, 1912, 1912, 1912, 1912, 1912, 1913, 1913, 1913,
     1913, 1913, 1913, 1913, 1913, 1913, 1913, 1913, 1913, 1913,
     1913, 1913, 1913, 1913, 1913, 1914, 1914, 1914, 1914, 1914,
     1914, 1914, 1914, 1914, 1914, 1914, 1914, 1914, 1914, 1914,
     1914, 1914, 1914, 1915, 1915, 1915, 1915, 1915, 1915, 1915,
     1915, 1915, 1915, 1915, 1915, 1915, 1915, 1915, 1915, 1915,

     1915, 1916, 1916, 1916, 1916, 1916, 1916, 1916, 1916, 1916,
     1916, 1916, 1916, 1916, 1916, 1916, 1916, 1916, 1916, 1917,
        0,    0,    0,    0,    0,    0, 1917,    0, 1917,    0,
        0, 1917, 1917, 1917, 1917, 1918, 1918, 1918, 1918,    0,
     1918, 1918, 1918, 1918, 1918, 1918,    0, 1918, 1918,    0,
        0, 1918, 1918, 1919, 1919, 1919, 1919, 1919, 1921, 1921,
     1921, 1921, 1921, 1921, 1921, 1921, 1921, 1921, 1921, 1921,
     1921, 1921, 1921, 1921, 1921, 1921, 1922, 1922, 1922, 1922,
     1922, 1922, 1922, 1922, 1922, 1922, 1922, 1922, 1922, 1922,
     1922, 1922, 1922, 1922, 1923, 1923, 1923, 1923, 1923, 1923,

     1923, 1923, 1923, 1923, 1923, 1923, 1923, 1923, 1923, 1923,
     1923, 1923, 1924, 1924, 1924, 1924, 1924, 1924, 1924, 1924,
     1924, 1924, 1924, 1924, 1924, 1924, 1924, 1924, 1924, 1924,
     1925, 1925, 1925, 1925, 1925, 1925, 1925, 1925, 1925, 1925,
     1925, 1925, 1925, 1925, 1925, 1925, 1925, 1925, 1926, 1926,
     1926, 1926, 1926, 1926, 1926, 1926, 1926, 1926, 1926, 1926,
     1926, 1926, 1926, 1926, 1926, 1926, 1927, 1927, 1927, 1927,
     1927, 1927, 1927, 1927, 1927, 1927, 1927, 1927, 1927, 1927,
     1927, 1927, 1927, 1927, 1928, 1928, 1928, 1928, 1928, 1928,
     1928, 1928, 1928, 1928, 1928, 1928, 1928, 1928, 1928, 1928,

     1928, 1928, 1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929,
     1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929,
     1930, 1930, 1930, 1930, 1930, 1930, 1930, 1930, 1930, 1930,
     1930, 1930, 1930, 1930, 1930, 1930, 1930, 1930, 1931, 1931,
        0, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1932, 1932, 1932, 1932,
     1932, 1932, 1932, 1932, 1932, 1932, 1932, 1932, 1932, 1932,
     1932, 1932, 1932, 1932, 1933, 1933, 1933, 1933, 1933, 1933,
     1933, 1933, 1933, 1933, 1933, 1933, 1933, 1933, 1933, 1933,
     1933, 1933, 1934, 1934, 1934, 1934, 1934, 1934, 1934, 1934,

     1934, 1934, 1934, 1934, 1934, 1934, 1934, 1934, 1934, 1934,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1936, 1936,
     1936, 1936, 1936, 1936, 1936, 1936, 1936, 1936, 1936, 1936,
     1936, 1936, 1936, 1936, 1936, 1936, 1937,    0,    0,    0,
        0,    0,    0, 1937,    0, 1937,    0,    0, 1937, 1937,
     1937, 1937, 1938,    0,    0,    0,    0,    0,    0,    0,
     1938,    0, 1938,    0, 1938, 1938, 1938, 1938, 1938, 1939,
     1939, 1939, 1939, 1940, 1940, 1940, 1940, 1940, 1940, 1940,
     1940, 1940, 1940, 1940, 1940, 1940, 1940, 1940, 1940, 1940,

     1940, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1942,
     1942, 1942, 1942, 1942, 1942, 1942, 1942, 1942, 1942, 1942,
     1942, 1942, 1942, 1942, 1942, 1942, 1942, 1943, 1943, 1943,
     1943,    0, 1943, 1943, 1943, 1943, 1943, 1943,    0, 1943,
     1943,    0,    0, 1943, 1943, 1944, 1944, 1944, 1944, 1944,
     1945, 1945, 1945, 1945, 1945, 1945, 1945, 1945, 1945, 1945,
     1945, 1945, 1945, 1945, 1945, 1945, 1945, 1945, 1946,    0,
        0,    0,    0,    0,    0,    0, 1946, 1946, 1947, 1947,
     1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947,

     1947, 1947, 1947, 1947, 1947, 1947, 1948, 1948, 1948, 1948,
     1948, 1948, 1948, 1948, 1948, 1948, 1948, 1948, 1948, 1948,
     1948, 1948, 1948, 1948, 1949, 1949, 1949, 1949, 1949, 1949,
     1949, 1949, 1949, 1949, 1949, 1949, 1949, 1949, 1949, 1949,
     1949, 1949, 1950, 1950, 1950, 1950, 1950, 1950, 1950, 1950,
     1950, 1950, 1950, 1950, 1950, 1950, 1950, 1950, 1950, 1950,
     1951, 1951, 1951, 1951, 1951, 1951, 1951, 1951, 1951, 1951,
     1951, 1951, 1951, 1951, 1951, 1951, 1951, 1951, 1952, 1952,
     1952, 1952, 1952, 1952, 1952, 1952, 1952, 1952, 1952, 1952,
     1952, 1952, 1952, 1952, 1952, 1952, 1953, 1953, 1953, 1953,

     1953, 1953, 1953, 1953, 1953, 1953, 1953, 1953, 1953, 1953,
     1953, 1953, 1953, 1953, 1954, 1954, 1954, 1954, 1954, 1954,
     1954, 1954, 1954, 1954, 1954, 1954, 1954, 1954, 1954, 1954,
     1954, 1954, 1955, 1955, 1955, 1955, 1955, 1955, 1955, 1955,
     1955, 1955, 1955, 1955, 1955, 1955, 1955, 1955, 1955, 1955,
     1956,    0,    0,    0,    0,    0,    0,    0,    0,    0,
        0, 1956, 1956, 1956, 1956, 1956, 1957, 1957, 1957, 1957,
     1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957,
     1957, 1957, 1957, 1957, 1958, 1958, 1958, 1958, 1958, 1958,
     1958, 1958, 1958, 1958, 1958, 1958, 1958, 1958, 1958, 1958,

     1958, 1958, 1959, 1959, 1959, 1959, 1959, 1959, 1959, 1959,
     1959, 1959, 1959, 1959, 1959, 1959, 1959, 1959, 1959, 1959,
     1960, 1960,    0, 1960, 1960, 1960, 1960, 1960, 1960, 1960,
     1960, 1960, 1960, 1960, 1960, 1960, 1960, 1960, 1961, 1961,
     1961, 1961, 1961, 1961, 1961, 1961, 1961, 1961, 1961, 1961,
     1961, 1961, 1961, 1961, 1961, 1961, 1962, 1962, 1962, 1962,
     1962, 1962, 1962, 1962, 1962, 1962, 1962, 1962, 1962, 1962,
     1962, 1962, 1962, 1962, 1963, 1963, 1963, 1963, 1963, 1963,
     1963, 1963, 1963, 1963, 1963, 1963, 1963, 1963, 1963, 1963,
     1963, 1963, 1964, 1964, 1964, 1964, 1964, 1964, 1964, 1964,

     1964, 1964, 1964, 1964, 1964, 1964, 1964, 1964, 1964, 1964,
     1965,    0,    0,    0,    0,    0,    0, 1965,    0, 1965,
        0,    0, 1965, 1965, 1965, 1965, 1966,    0,    0,    0,
        0,    0,    0,    0, 1966,    0,    0,    0, 1966, 1966,
     1966, 1966, 1966, 1967,    0,    0,    0,    0,    0,    0,
        0, 1967,    0, 1967,    0, 1967, 1967, 1967, 1967, 1967,
     1968, 1968, 1968, 1968, 1968, 1968, 1968, 1968, 1968, 1968,
     1968, 1968, 1968, 1968, 1968, 1968, 1968, 1968, 1969, 1969,
     1969, 1969, 1969, 1969, 1969, 1969, 1969, 1969, 1969, 1969,
     1969, 1969, 1969, 1969, 1969, 1969, 1970, 1970, 1970, 1970,

     1970, 1970, 1970, 1970, 1970, 1970, 1970, 1970, 1970, 1970,
     1970, 1970, 1970, 1970, 1971, 1971, 1971, 1971, 1971, 1971,
     1971, 1971, 1971, 1971, 1971, 1971, 1971, 1971, 1971, 1971,
     1971, 1971, 1972, 1972, 1972, 1972, 1972, 1973, 1973, 1973,
     1973, 1973, 1973, 1973, 1973, 1973, 1973, 1973, 1973, 1973,
     1973, 1973, 1973, 1973, 1973, 1974, 1974, 1974, 1974, 1974,
     1974,    0, 1974, 1974, 1974, 1974, 1974, 1974, 1974, 1974,
     1974, 1974, 1974, 1975, 1975,    0, 1975, 1975, 1975, 1975,
     1975, 1975, 1975, 1975, 1975, 1975, 1975, 1975, 1975, 1975,
     1975, 1976, 1976, 1976, 1976, 1976, 1976, 1976, 1976, 1976,

     1976, 1976, 1976, 1976, 1976, 1976, 1976, 1976, 1976, 1977,
     1977, 1977, 1977, 1977, 1977, 1977, 1977, 1977, 1977, 1977,
     1977, 1977, 1977, 1977, 1977, 1977, 1977, 1978, 1978, 1978,
     1978, 1978, 1978, 1978, 1978, 1978, 1978, 1978, 1978, 1978,
     1978, 1978, 1978, 1978, 1978, 1979, 1979, 1979, 1979, 1979,
     1979, 1979, 1979, 1979, 1979, 1979, 1979, 1979, 1979, 1979,
     1979, 1979, 1979, 1980, 1980, 1980, 1980, 1980, 1980, 1980,
     1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980,
     1980, 1981, 1981, 1981, 1981, 1981, 1981, 1981, 1981, 1981,
     1981, 1981, 1981, 1981, 1981, 1981, 1981, 1981, 1981, 1982,

     1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982,
     1982, 1982, 1982, 1982, 1982, 1982, 1982, 1983, 1983,    0,
     1983, 1983, 1983, 1983, 1983, 1983, 1983, 1983, 1983, 1983,
     1983, 1983, 1983, 1983, 1983, 1984, 1984,    0, 1984, 1984,
     1984, 1984, 1984, 1984, 1984, 1984, 1984, 1984, 1984, 1984,
     1984, 1984, 1984, 1985, 1985,    0, 1985, 1985, 1985, 1985,
     1985, 1985, 1985, 1985, 1985, 1985, 1985, 1985, 1985, 1985,
     1985, 1986, 1986, 1986, 1986, 1986, 1986, 1986, 1986, 1986,
     1986, 1986, 1986, 1986, 1986, 1986, 1986, 1986, 1986, 1987,
     1987, 1987, 1987, 1987, 1987, 1987, 1987, 1987, 1987, 1987,

     1987, 1987, 1987, 1987, 1987, 1987, 1987, 1988, 1988, 1988,
     1988, 1988, 1988, 1988, 1988, 1988, 1988, 1988, 1988, 1988,
     1988, 1988, 1988, 1988, 1988, 1989, 1989, 1989, 1989, 1989,
     1989, 1989, 1989, 1989, 1989, 1989, 1989, 1989, 1989, 1989,
     1989, 1989, 1989, 1990,    0,    0,    0,    0,    0, 1990,
        0,    0,    0, 1990,    0, 1990, 1990, 1990, 1990, 1990,
     1991, 1991, 1991, 1991, 1992,    0,    0,    0,    0,    0,
        0,    0, 1992,    0,    0,    0, 1992, 1992, 1992, 1992,
     1992, 1993,    0,    0,    0,    0,    0,    0,    0, 1993,
        0, 1993,    0, 1993, 1993, 1993, 1993, 1993, 1994, 1994,

        0, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1995, 1995, 1995, 1995,
     1995, 1995, 1995, 1995, 1995, 1995, 1995, 1995, 1995, 1995,
     1995, 1995, 1995, 1995, 1996, 1996,    0, 1996, 1996, 1996,
     1996, 1996, 1996, 1996, 1996, 1996, 1996, 1996, 1996, 1996,
     1996, 1996, 1997, 1997, 1997, 1997, 1997, 1997,    0, 1997,
     1997, 1997, 1997, 1997, 1997, 1997, 1997, 1997, 1997, 1997,
     1998, 1998,    0, 1998, 1998, 1998, 1998, 1998, 1998, 1998,
     1998, 1998, 1998, 1998, 1998, 1998, 1998, 1998, 1999, 1999,
     1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999,

     1999, 1999, 1999, 1999, 1999, 1999, 2000, 2000, 2000, 2000,
     2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000,
     2000, 2000, 2000, 2000, 2001, 2001, 2001, 2001, 2001, 2001,
     2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001,
     2001, 2001, 2002, 2002, 2002, 2002, 2002, 2002, 2002, 2002,
     2002, 2002, 2002, 2002, 2002, 2002, 2002, 2002, 2002, 2002,
     2003, 2003, 2003, 2003, 2003, 2003, 2003, 2003, 2003, 2003,
     2003, 2003, 2003, 2003, 2003, 2003, 2003, 2003, 2004, 2004,
     2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004,
     2004, 2004, 2004, 2004, 2004, 2004, 2005, 2005, 2005, 2005,

     2005, 2005, 2005, 2005, 2005, 2005, 2005, 2005, 2005, 2005,
     2005, 2005, 2005, 2005, 2006, 2006, 2006, 2006, 2006, 2006,
     2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006,
     2006, 2006, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007,
     2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007,
     2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008,
     2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2009, 2009,
     2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009,
     2009, 2009, 2009, 2009, 2009, 2009, 2010, 2010,    0, 2010,
     2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010,

     2010, 2010, 2010, 2010, 2011, 2011,    0, 2011, 2011, 2011,
     2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011,
     2011, 2011, 2012, 2012,    0, 2012, 2012, 2012, 2012, 2012,
     2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012,
     2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013,
     2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2014, 2014,
     2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014,
     2014, 2014, 2014, 2014, 2014, 2014, 2015, 2015, 2015, 2015,
     2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015,
     2015, 2015, 2015, 2015, 2016, 2016, 2016, 2016, 2016, 2016,

     2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016,
     2016, 2016, 2017,    0,    0,    0,    0,    0, 2017,    0,
        0,    0,    0,    0, 2017, 2017, 2017, 2017, 2017, 2018,
     2018,    0, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018,
     2018, 2018, 2018, 2018, 2018, 2018, 2018, 2019,    0,    0,
        0,    0,    0,    0, 2019,    0, 2019,    0,    0, 2019,
     2019, 2019, 2019, 2020,    0,    0,    0,    0,    0,    0,
        0, 2020,    0, 2020,    0, 2020, 2020, 2020, 2020, 2020,
     2021, 2021, 2021, 2021, 2022, 2022,    0, 2022, 2022, 2022,
     2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022,

     2022, 2022, 2023, 2023, 2023, 2023, 2023, 2023,    0, 2023,
     2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023,
     2024, 2024,    0, 2024, 2024, 2024, 2024, 2024, 2024, 2024,
     2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024, 2025, 2025,
        0, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025,
     2025, 2025, 2025, 2025, 2025, 2025, 2026, 2026, 2026, 2026,
     2026, 2026, 2026, 2026, 2026, 2026, 2026, 2026, 2026, 2026,
     2026, 2026, 2026, 2026, 2027, 2027, 2027, 2027, 2027, 2027,
     2027, 2027, 2027, 2027, 2027, 2027, 2027, 2027, 2027, 2027,
     2027, 2027, 2028, 2028, 2028, 2028, 2028, 2028, 2028, 2028,

     2028, 2028, 2028, 2028, 2028, 2028, 2028, 2028, 2028, 2028,
     2029, 2029, 2029, 2029, 2029, 2029, 2029, 2029, 2029, 2029,
     2029, 2029, 2029, 2029, 2029, 2029, 2029, 2029, 2030, 2030,
     2030, 2030, 2030, 2030, 2030, 2030, 2030, 2030, 2030, 2030,
     2030, 2030, 2030, 2030, 2030, 2030, 2031,    0, 2031,    0,
        0,    0,    0, 2031,    0,    0, 2031, 2031, 2031, 2031,
     2031, 2031, 2032, 2032, 2032, 2032, 2032, 2032, 2032, 2032,
     2032, 2032, 2032, 2032, 2032, 2032, 2032, 2032, 2032, 2032,
     2033, 2033, 2033, 2033, 2033, 2033, 2033, 2033, 2033, 2033,
     2033, 2033, 2033, 2033, 2033, 2033, 2033, 2033, 2034, 2034,

     2034, 2034, 2034, 2034, 2034, 2034, 2034, 2034, 2034, 2034,
     2034, 2034, 2034, 2034, 2034, 2034, 2035, 2035, 2035, 2035,
     2035, 2035, 2035, 2035, 2035, 2035, 2035, 2035, 2035, 2035,
     2035, 2035, 2035, 2035, 2036, 2036, 2036, 2036, 2036, 2036,
     2036, 2036, 2036, 2036, 2036, 2036, 2036, 2036, 2036, 2036,
     2036, 2036, 2037, 2037,    0, 2037, 2037, 2037, 2037, 2037,
     2037, 2037, 2037, 2037, 2037, 2037, 2037, 2037, 2037, 2037,
     2038, 2038, 2038, 2038, 2038, 2038, 2038, 2038, 2038, 2038,
     2038, 2038, 2038, 2038, 2038, 2038, 2038, 2038, 2039, 2039,
     2039, 2039, 2039, 2039, 2039, 2039, 2039, 2039, 2039, 2039,

     2039, 2039, 2039, 2039, 2039, 2039, 2040,    0,    0,    0,
        0,    0, 2040,    0,    0,    0,    0,    0, 2040, 2040,
     2040, 2040, 2040, 2041,    0,    0,    0,    0,    0,    0,
     2041,    0, 2041,    0,    0, 2041, 2041, 2041, 2041, 2042,
        0,    0,    0,    0,    0,    0,    0, 2042,    0, 2042,
        0, 2042, 2042, 2042, 2042, 2042, 2043, 2043, 2043, 2043,
     2044,    0, 2044,    0,    0,    0,    0, 2044,    0,    0,
     2044, 2044, 2044, 2044, 2044, 2044, 2045,    0, 2045,    0,
        0,    0,    0, 2045,    0,    0, 2045, 2045, 2045, 2045,
     2045, 2045, 2046, 2046, 2046, 2046, 2046, 2046, 2046, 2046,

     2046, 2046, 2046, 2046, 2046, 2046, 2046, 2046, 2046, 2046,
     2047, 2047, 2047, 2047, 2047, 2048, 2048,    0, 2048, 2048,
     2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048,
     2048, 2048, 2048, 2049, 2049, 2049, 2049, 2049, 2049, 2049,
     2049, 2049, 2049, 2049, 2049, 2049, 2049, 2049, 2049, 2049,
     2049, 2050, 2050, 2050, 2050, 2050, 2050, 2050, 2050, 2050,
     2050, 2050, 2050, 2050, 2050, 2050, 2050, 2050, 2050, 2051,
     2051, 2051, 2051, 2051, 2051, 2051, 2051, 2051, 2051, 2051,
     2051, 2051, 2051, 2051, 2051, 2051, 2051, 2052, 2052, 2052,
     2052, 2052, 2052, 2052, 2052, 2052, 2052, 2052, 2052, 2052,

     2052, 2052, 2052, 2052, 2052, 2053, 2053, 2053, 2053, 2053,
     2053, 2053, 2053, 2053, 2053, 2053, 2053, 2053, 2053, 2053,
     2053, 2053, 2053, 2054, 2054, 2054, 2054, 2054, 2054, 2054,
     2054, 2054, 2054, 2054, 2054, 2054, 2054, 2054, 2054, 2054,
     2054, 2055, 2055, 2055, 2055, 2055, 2055, 2055, 2055, 2055,
     2055, 2055, 2055, 2055, 2055, 2055, 2055, 2055, 2055, 2056,
     2056, 2056, 2056, 2056, 2056, 2056, 2056, 2056, 2056, 2056,
     2056, 2056, 2056, 2056, 2056, 2056, 2056, 2057, 2057, 2057,
     2057, 2057, 2057, 2057, 2057, 2057, 2057, 2057, 2057, 2057,
     2057, 2057, 2057, 2057, 2057, 2058, 2058, 2058, 2058, 2058,

     2058, 2058, 2058, 2058, 2058, 2058, 2058, 2058, 2058, 2058,
     2058, 2058, 2058, 2059, 2059, 2059, 2059, 2059, 2059, 2059,
     2059, 2059, 2059, 2059, 2059, 2059, 2059, 2059, 2059, 2059,
     2059, 2060, 2060, 2060, 2060, 2060, 2060, 2060, 2060, 2060,
     2060, 2060, 2060, 2060, 2060, 2060, 2060, 2060, 2060, 2061,
     2061, 2061, 2061, 2061, 2061, 2061, 2061, 2061, 2061, 2061,
     2061, 2061, 2061, 2061, 2061, 2061, 2061, 2062, 2062, 2062,
     2062, 2062, 2062, 2062, 2062, 2062, 2062, 2062, 2062, 2062,
     2062, 2062, 2062, 2062, 2062, 2063, 2063, 2063, 2063, 2063,
     2063, 2063, 2063, 2063, 2063, 2063, 2063, 2063, 2063, 2063,

     2063, 2063, 2063, 2064, 2064, 2064, 2064, 2064, 2064, 2064,
     2064, 2064, 2064, 2064, 2064, 2064, 2064, 2064, 2064, 2064,
     2064, 2065, 2065, 2065, 2065, 2065, 2065, 2065, 2065, 2065,
     2065, 2065, 2065, 2065, 2065, 2065, 2065, 2065, 2065, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,

     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897
    } ;

extern int yy_flex_debug;
int yy_flex_debug = 0;

static yy_state_type *yy_state_buf=0, *yy_state_ptr=0;
static char *yy_full_match;
static int yy_lp;
static int yy_looking_for_trail_begin = 0;
static int yy_full_lp;
static int *yy_full_state;
#define YY_TRAILING_MASK 0x2000
#define YY_TRAILING_HEAD_MASK 0x4000
#define REJECT \
{ \
*yy_cp = (yy_hold_char); /* undo effects of setting up yytext */ \
yy_cp = (yy_full_match); /* restore poss. backed-over text */ \
(yy_lp) = (yy_full_lp); /* restore orig. accepting pos. */ \
(yy_state_ptr) = (yy_full_state); /* restore orig. state */ \
yy_current_state = *(yy_state_ptr); /* restore curr. state */ \
++(yy_lp); \
goto find_rule; \
}

#define yymore() yymore_used_but_not_detected
#define YY_MORE_ADJ 0
#define YY_RESTORE_YY_MORE_OFFSET
char *yytext;
#line 1 "fortran.lex"
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







#line 46 "fortran.lex"
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

#line 3774 "fortran.yy.c"
#line 3775 "fortran.yy.c"

#define INITIAL 0
#define parameter 1
#define character 2
#define donottreat 3
#define donottreat_interface 4
#define includestate 5
#define fortran77style 6
#define fortran90style 7

#ifndef YY_NO_UNISTD_H
/* Special case for "unistd.h", since it is non-ANSI. We include it way
 * down here because we want the user's section 1 to have been scanned first.
 * The user has a chance to override it with an option.
 */
#include <unistd.h>
#endif

#ifndef YY_EXTRA_TYPE
#define YY_EXTRA_TYPE void *
#endif

static int yy_init_globals ( void );

/* Accessor methods to globals.
   These are made visible to non-reentrant scanners for convenience. */

int yylex_destroy ( void );

int yyget_debug ( void );

void yyset_debug ( int debug_flag  );

YY_EXTRA_TYPE yyget_extra ( void );

void yyset_extra ( YY_EXTRA_TYPE user_defined  );

FILE *yyget_in ( void );

void yyset_in  ( FILE * _in_str  );

FILE *yyget_out ( void );

void yyset_out  ( FILE * _out_str  );

			int yyget_leng ( void );

char *yyget_text ( void );

int yyget_lineno ( void );

void yyset_lineno ( int _line_number  );

/* Macros after this point can all be overridden by user definitions in
 * section 1.
 */

#ifndef YY_SKIP_YYWRAP
#ifdef __cplusplus
extern "C" int yywrap ( void );
#else
extern int yywrap ( void );
#endif
#endif

#ifndef YY_NO_UNPUT
    
    static void yyunput ( int c, char *buf_ptr  );
    
#endif

#ifndef yytext_ptr
static void yy_flex_strncpy ( char *, const char *, int );
#endif

#ifdef YY_NEED_STRLEN
static int yy_flex_strlen ( const char * );
#endif

#ifndef YY_NO_INPUT
#ifdef __cplusplus
static int yyinput ( void );
#else
static int input ( void );
#endif

#endif

/* Amount of stuff to slurp up with each read. */
#ifndef YY_READ_BUF_SIZE
#ifdef __ia64__
/* On IA-64, the buffer size is 16k, not 8k */
#define YY_READ_BUF_SIZE 16384
#else
#define YY_READ_BUF_SIZE 8192
#endif /* __ia64__ */
#endif

/* Copy whatever the last rule matched to the standard output. */
#ifndef ECHO
/* This used to be an fputs(), but since the string might contain NUL's,
 * we now use fwrite().
 */
#define ECHO do { if (fwrite( yytext, (size_t) yyleng, 1, yyout )) {} } while (0)
#endif

/* Gets input and stuffs it into "buf".  number of characters read, or YY_NULL,
 * is returned in "result".
 */
#ifndef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( YY_CURRENT_BUFFER_LVALUE->yy_is_interactive ) \
		{ \
		int c = '*'; \
		int n; \
		for ( n = 0; n < max_size && \
			     (c = getc( yyin )) != EOF && c != '\n'; ++n ) \
			buf[n] = (char) c; \
		if ( c == '\n' ) \
			buf[n++] = (char) c; \
		if ( c == EOF && ferror( yyin ) ) \
			YY_FATAL_ERROR( "input in flex scanner failed" ); \
		result = n; \
		} \
	else \
		{ \
		errno=0; \
		while ( (result = (int) fread(buf, 1, (yy_size_t) max_size, yyin)) == 0 && ferror(yyin)) \
			{ \
			if( errno != EINTR) \
				{ \
				YY_FATAL_ERROR( "input in flex scanner failed" ); \
				break; \
				} \
			errno=0; \
			clearerr(yyin); \
			} \
		}\
\

#endif

/* No semi-colon after return; correct usage is to write "yyterminate();" -
 * we don't want an extra ';' after the "return" because that will cause
 * some compilers to complain about unreachable statements.
 */
#ifndef yyterminate
#define yyterminate() return YY_NULL
#endif

/* Number of entries by which start-condition stack grows. */
#ifndef YY_START_STACK_INCR
#define YY_START_STACK_INCR 25
#endif

/* Report a fatal error. */
#ifndef YY_FATAL_ERROR
#define YY_FATAL_ERROR(msg) yy_fatal_error( msg )
#endif

/* end tables serialization structures and prototypes */

/* Default declaration of generated scanner - a define so the user can
 * easily add parameters.
 */
#ifndef YY_DECL
#define YY_DECL_IS_OURS 1

extern int yylex (void);

#define YY_DECL int yylex (void)
#endif /* !YY_DECL */

/* Code executed at the beginning of each rule, after yytext and yyleng
 * have been set up.
 */
#ifndef YY_USER_ACTION
#define YY_USER_ACTION
#endif

/* Code executed at the end of each rule. */
#ifndef YY_BREAK
#define YY_BREAK /*LINTED*/break;
#endif

#define YY_RULE_SETUP \
	if ( yyleng > 0 ) \
		YY_CURRENT_BUFFER_LVALUE->yy_at_bol = \
				(yytext[yyleng - 1] == '\n'); \
	YY_USER_ACTION

/** The main scanner function which does all the work.
 */
YY_DECL
{
	yy_state_type yy_current_state;
	char *yy_cp, *yy_bp;
	int yy_act;
    
	if ( !(yy_init) )
		{
		(yy_init) = 1;

#ifdef YY_USER_INIT
		YY_USER_INIT;
#endif

        /* Create the reject buffer large enough to save one state per allowed character. */
        if ( ! (yy_state_buf) )
            (yy_state_buf) = (yy_state_type *)yyalloc(YY_STATE_BUF_SIZE  );
            if ( ! (yy_state_buf) )
                YY_FATAL_ERROR( "out of dynamic memory in yylex()" );

		if ( ! (yy_start) )
			(yy_start) = 1;	/* first start state */

		if ( ! yyin )
			yyin = stdin;

		if ( ! yyout )
			yyout = stdout;

		if ( ! YY_CURRENT_BUFFER ) {
			yyensure_buffer_stack ();
			YY_CURRENT_BUFFER_LVALUE =
				yy_create_buffer( yyin, YY_BUF_SIZE );
		}

		yy_load_buffer_state(  );
		}

	{
#line 101 "fortran.lex"

#line 103 "fortran.lex"
  if (infixed) BEGIN(fortran77style) ;
  if (infree)  BEGIN(fortran90style) ;

#line 4014 "fortran.yy.c"

	while ( /*CONSTCOND*/1 )		/* loops until end-of-file is reached */
		{
		yy_cp = (yy_c_buf_p);

		/* Support of yytext. */
		*yy_cp = (yy_hold_char);

		/* yy_bp points to the position in yy_ch_buf of the start of
		 * the current run.
		 */
		yy_bp = yy_cp;

		yy_current_state = (yy_start);
		yy_current_state += YY_AT_BOL();

		(yy_state_ptr) = (yy_state_buf);
		*(yy_state_ptr)++ = yy_current_state;

yy_match:
		do
			{
			YY_CHAR yy_c = yy_ec[YY_SC_TO_UI(*yy_cp)] ;
			while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
				{
				yy_current_state = (int) yy_def[yy_current_state];
				if ( yy_current_state >= 1898 )
					yy_c = yy_meta[yy_c];
				}
			yy_current_state = yy_nxt[yy_base[yy_current_state] + yy_c];
			*(yy_state_ptr)++ = yy_current_state;
			++yy_cp;
			}
		while ( yy_base[yy_current_state] != 9640 );

yy_find_action:
		yy_current_state = *--(yy_state_ptr);
		(yy_lp) = yy_accept[yy_current_state];
find_rule: /* we branch to this label when backing up */
		for ( ; ; ) /* until we find what rule we matched */
			{
			if ( (yy_lp) && (yy_lp) < yy_accept[yy_current_state + 1] )
				{
				yy_act = yy_acclist[(yy_lp)];
				if ( yy_act & YY_TRAILING_HEAD_MASK ||
				     (yy_looking_for_trail_begin) )
					{
					if ( yy_act == (yy_looking_for_trail_begin) )
						{
						(yy_looking_for_trail_begin) = 0;
						yy_act &= ~YY_TRAILING_HEAD_MASK;
						break;
						}
					}
				else if ( yy_act & YY_TRAILING_MASK )
					{
					(yy_looking_for_trail_begin) = yy_act & ~YY_TRAILING_MASK;
					(yy_looking_for_trail_begin) |= YY_TRAILING_HEAD_MASK;
					}
				else
					{
					(yy_full_match) = yy_cp;
					(yy_full_state) = (yy_state_ptr);
					(yy_full_lp) = (yy_lp);
					break;
					}
				++(yy_lp);
				goto find_rule;
				}
			--yy_cp;
			yy_current_state = *--(yy_state_ptr);
			(yy_lp) = yy_accept[yy_current_state];
			}

		YY_DO_BEFORE_ACTION;

do_action:	/* This label is used only to access EOF actions. */

		switch ( yy_act )
	{ /* beginning of action switch */
case 1:
YY_RULE_SETUP
#line 106 "fortran.lex"
{ return TOK_SUBROUTINE; }
	YY_BREAK
case 2:
YY_RULE_SETUP
#line 107 "fortran.lex"
{ return TOK_PROGRAM; }
	YY_BREAK
case 3:
YY_RULE_SETUP
#line 108 "fortran.lex"
{ inallocate = 1; return TOK_ALLOCATE; }
	YY_BREAK
case 4:
YY_RULE_SETUP
#line 109 "fortran.lex"
{ return TOK_CONTINUE; }
	YY_BREAK
case 5:
YY_RULE_SETUP
#line 110 "fortran.lex"
{ return TOK_NULLIFY; }
	YY_BREAK
case 6:
YY_RULE_SETUP
#line 111 "fortran.lex"
{ inallocate = 1; return TOK_DEALLOCATE; }
	YY_BREAK
case 7:
YY_RULE_SETUP
#line 112 "fortran.lex"
{ return TOK_RESULT; }
	YY_BREAK
case 8:
YY_RULE_SETUP
#line 113 "fortran.lex"
{ return TOK_FUNCTION; }
	YY_BREAK
case 9:
YY_RULE_SETUP
#line 114 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_ENDUNIT;}
	YY_BREAK
case 10:
YY_RULE_SETUP
#line 115 "fortran.lex"
{ pos_curinclude = setposcur()-9; BEGIN(includestate); }
	YY_BREAK
case 11:
YY_RULE_SETUP
#line 116 "fortran.lex"
{ return TOK_USE;}
	YY_BREAK
case 12:
YY_RULE_SETUP
#line 117 "fortran.lex"
{ return TOK_REWIND; }
	YY_BREAK
case 13:
YY_RULE_SETUP
#line 118 "fortran.lex"
{ return TOK_IMPLICIT; }
	YY_BREAK
case 14:
YY_RULE_SETUP
#line 119 "fortran.lex"
{ return TOK_NONE; }
	YY_BREAK
case 15:
YY_RULE_SETUP
#line 120 "fortran.lex"
{ return TOK_CALL; }
	YY_BREAK
case 16:
YY_RULE_SETUP
#line 121 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_TRUE; }
	YY_BREAK
case 17:
YY_RULE_SETUP
#line 122 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_FALSE; }
	YY_BREAK
case 18:
YY_RULE_SETUP
#line 123 "fortran.lex"
{ return TOK_POINT_TO; }
	YY_BREAK
case 19:
YY_RULE_SETUP
#line 124 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_ASSIGNTYPE;}
	YY_BREAK
case 20:
YY_RULE_SETUP
#line 125 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_DASTER; }
	YY_BREAK
case 21:
YY_RULE_SETUP
#line 126 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_EQV; }
	YY_BREAK
case 22:
YY_RULE_SETUP
#line 127 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_EQ;  }
	YY_BREAK
case 23:
YY_RULE_SETUP
#line 128 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_GT;  }
	YY_BREAK
case 24:
YY_RULE_SETUP
#line 129 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_GE;  }
	YY_BREAK
case 25:
YY_RULE_SETUP
#line 130 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_LT;  }
	YY_BREAK
case 26:
YY_RULE_SETUP
#line 131 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_LE;  }
	YY_BREAK
case 27:
YY_RULE_SETUP
#line 132 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NEQV;}
	YY_BREAK
case 28:
YY_RULE_SETUP
#line 133 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NE;  }
	YY_BREAK
case 29:
YY_RULE_SETUP
#line 134 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NOT; }
	YY_BREAK
case 30:
YY_RULE_SETUP
#line 135 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_OR;  }
	YY_BREAK
case 31:
YY_RULE_SETUP
#line 136 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_XOR; }
	YY_BREAK
case 32:
YY_RULE_SETUP
#line 137 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_AND; }
	YY_BREAK
case 33:
YY_RULE_SETUP
#line 138 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_EQUALEQUAL; }
	YY_BREAK
case 34:
YY_RULE_SETUP
#line 139 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_SLASHEQUAL; }
	YY_BREAK
case 35:
YY_RULE_SETUP
#line 140 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_INFEQUAL; }
	YY_BREAK
case 36:
YY_RULE_SETUP
#line 141 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_SUPEQUAL; }
	YY_BREAK
case 37:
YY_RULE_SETUP
#line 142 "fortran.lex"
{ return TOK_MODULE; }
	YY_BREAK
case 38:
YY_RULE_SETUP
#line 143 "fortran.lex"
{ return TOK_WHILE; }
	YY_BREAK
case 39:
YY_RULE_SETUP
#line 144 "fortran.lex"
{ return TOK_CONCURRENT; }
	YY_BREAK
case 40:
YY_RULE_SETUP
#line 145 "fortran.lex"
{ return TOK_ENDDO; }
	YY_BREAK
case 41:
YY_RULE_SETUP
#line 146 "fortran.lex"
{ strcpy(yylval.na,&fortran_text[2]);
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
	YY_BREAK
case 42:
YY_RULE_SETUP
#line 157 "fortran.lex"
{ increment_nbtokens = 0; return TOK_PLAINDO;}
	YY_BREAK
case 43:
YY_RULE_SETUP
#line 158 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_REAL; }
	YY_BREAK
case 44:
YY_RULE_SETUP
#line 159 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_INTEGER; }
	YY_BREAK
case 45:
YY_RULE_SETUP
#line 160 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_LOGICAL; }
	YY_BREAK
case 46:
YY_RULE_SETUP
#line 161 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_CHARACTER; }
	YY_BREAK
case 47:
YY_RULE_SETUP
#line 162 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_HEXA;}
	YY_BREAK
case 48:
YY_RULE_SETUP
#line 163 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_DOUBLEPRECISION; }
	YY_BREAK
case 49:
YY_RULE_SETUP
#line 164 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_DOUBLECOMPLEX; }
	YY_BREAK
case 50:
YY_RULE_SETUP
#line 165 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_COMPLEX; }
	YY_BREAK
case 51:
YY_RULE_SETUP
#line 166 "fortran.lex"
{ return TOK_ALLOCATABLE; }
	YY_BREAK
case 52:
YY_RULE_SETUP
#line 167 "fortran.lex"
{ return TOK_CONTIGUOUS; }
	YY_BREAK
case 53:
YY_RULE_SETUP
#line 168 "fortran.lex"
{ return TOK_CLOSE; }
	YY_BREAK
case 54:
YY_RULE_SETUP
#line 169 "fortran.lex"
{ return TOK_INQUIRE; }
	YY_BREAK
case 55:
YY_RULE_SETUP
#line 170 "fortran.lex"
{ return TOK_DIMENSION; }
	YY_BREAK
case 56:
YY_RULE_SETUP
#line 171 "fortran.lex"
{ return TOK_PAUSE; }
	YY_BREAK
case 57:
YY_RULE_SETUP
#line 172 "fortran.lex"
{ return TOK_EQUIVALENCE; }
	YY_BREAK
case 58:
YY_RULE_SETUP
#line 173 "fortran.lex"
{ return TOK_STOP; }
	YY_BREAK
case 59:
YY_RULE_SETUP
#line 174 "fortran.lex"
{ return TOK_WHERE; }
	YY_BREAK
case 60:
YY_RULE_SETUP
#line 175 "fortran.lex"
{ return TOK_ENDWHERE; }
	YY_BREAK
case 61:
YY_RULE_SETUP
#line 176 "fortran.lex"
{ return TOK_ELSEWHEREPAR; }
	YY_BREAK
case 62:
YY_RULE_SETUP
#line 177 "fortran.lex"
{ return TOK_ELSEWHERE; }
	YY_BREAK
case 63:
YY_RULE_SETUP
#line 178 "fortran.lex"
{ return TOK_CONTAINS; }
	YY_BREAK
case 64:
YY_RULE_SETUP
#line 179 "fortran.lex"
{ return TOK_ONLY; }
	YY_BREAK
case 65:
YY_RULE_SETUP
#line 180 "fortran.lex"
{ return TOK_PARAMETER; }
	YY_BREAK
case 66:
YY_RULE_SETUP
#line 181 "fortran.lex"
{ return TOK_RECURSIVE; }
	YY_BREAK
case 67:
YY_RULE_SETUP
#line 182 "fortran.lex"
{ return TOK_COMMON; }
	YY_BREAK
case 68:
YY_RULE_SETUP
#line 183 "fortran.lex"
{ return TOK_GLOBAL; }
	YY_BREAK
case 69:
YY_RULE_SETUP
#line 184 "fortran.lex"
{ return TOK_EXTERNAL; }
	YY_BREAK
case 70:
YY_RULE_SETUP
#line 185 "fortran.lex"
{ intent_spec = 1; return TOK_INTENT; }
	YY_BREAK
case 71:
YY_RULE_SETUP
#line 186 "fortran.lex"
{ return TOK_POINTER; }
	YY_BREAK
case 72:
YY_RULE_SETUP
#line 187 "fortran.lex"
{ return TOK_OPTIONAL; }
	YY_BREAK
case 73:
YY_RULE_SETUP
#line 188 "fortran.lex"
{ return TOK_SAVE; }
	YY_BREAK
case 74:
YY_RULE_SETUP
#line 189 "fortran.lex"
{ pos_cur_decl = setposcur()-strlen(fortran_text); return TOK_TYPEPAR; }
	YY_BREAK
case 75:
YY_RULE_SETUP
#line 190 "fortran.lex"
{ return TOK_TYPE; }
	YY_BREAK
case 76:
YY_RULE_SETUP
#line 191 "fortran.lex"
{ return TOK_ENDTYPE; }
	YY_BREAK
case 77:
YY_RULE_SETUP
#line 192 "fortran.lex"
{ if (inallocate == 1) return TOK_STAT; else { strcpy(yylval.na,fortran_text); return TOK_NAME; } }
	YY_BREAK
case 78:
YY_RULE_SETUP
#line 193 "fortran.lex"
{ return TOK_OPEN; }
	YY_BREAK
case 79:
YY_RULE_SETUP
#line 194 "fortran.lex"
{ return TOK_RETURN; }
	YY_BREAK
case 80:
YY_RULE_SETUP
#line 195 "fortran.lex"
{ return TOK_EXIT; }
	YY_BREAK
case 81:
YY_RULE_SETUP
#line 196 "fortran.lex"
{ return TOK_PRINT; }
	YY_BREAK
case 82:
YY_RULE_SETUP
#line 197 "fortran.lex"
{ return TOK_PROCEDURE; }
	YY_BREAK
case 83:
YY_RULE_SETUP
#line 198 "fortran.lex"
{ in_io_control_spec = 1; return TOK_READ_PAR; }
	YY_BREAK
case 84:
YY_RULE_SETUP
#line 199 "fortran.lex"
{ return TOK_READ; }
	YY_BREAK
case 85:
YY_RULE_SETUP
#line 200 "fortran.lex"
{ return TOK_NAMELIST; }
	YY_BREAK
case 86:
YY_RULE_SETUP
#line 201 "fortran.lex"
{ in_io_control_spec = 1; return TOK_WRITE_PAR; }
	YY_BREAK
case 87:
YY_RULE_SETUP
#line 202 "fortran.lex"
{ return TOK_WRITE; }
	YY_BREAK
case 88:
YY_RULE_SETUP
#line 203 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_FLUSH; }
	YY_BREAK
case 89:
YY_RULE_SETUP
#line 204 "fortran.lex"
{ return TOK_TARGET; }
	YY_BREAK
case 90:
YY_RULE_SETUP
#line 205 "fortran.lex"
{ return TOK_PUBLIC; }
	YY_BREAK
case 91:
YY_RULE_SETUP
#line 206 "fortran.lex"
{ return TOK_PRIVATE; }
	YY_BREAK
case 92:
YY_RULE_SETUP
#line 207 "fortran.lex"
{ strcpy(yylval.na,fortran_text);
                               if (intent_spec==1)
                                {return TOK_IN; }
                              else
                              {
                              return TOK_NAME;
                              }
                            }
	YY_BREAK
case 93:
YY_RULE_SETUP
#line 215 "fortran.lex"
{ pos_curdata = setposcur()-strlen(fortran_text); /*Init_List_Data_Var();*/ return TOK_DATA; }
	YY_BREAK
case 94:
YY_RULE_SETUP
#line 216 "fortran.lex"
{ return TOK_PLAINGOTO; }
	YY_BREAK
case 95:
YY_RULE_SETUP
#line 217 "fortran.lex"
{ strcpy(yylval.na,fortran_text);
                               if (intent_spec==1)
                                {return TOK_OUT; }
                              else
                              {
                              return TOK_NAME;
                              }
                            }
	YY_BREAK
case 96:
YY_RULE_SETUP
#line 225 "fortran.lex"
{ strcpy(yylval.na,fortran_text);
                               if (intent_spec==1)
                                {return TOK_IN; }
                              else
                              {
                              return TOK_INOUT;
                              }
                            }
	YY_BREAK
case 97:
YY_RULE_SETUP
#line 233 "fortran.lex"
{ return TOK_INTRINSIC; }
	YY_BREAK
case 98:
YY_RULE_SETUP
#line 234 "fortran.lex"
{ return TOK_THEN; }
	YY_BREAK
case 99:
YY_RULE_SETUP
#line 235 "fortran.lex"
{ return TOK_ELSEIF; }
	YY_BREAK
case 100:
YY_RULE_SETUP
#line 236 "fortran.lex"
{ return TOK_ELSE; }
	YY_BREAK
case 101:
YY_RULE_SETUP
#line 237 "fortran.lex"
{ return TOK_ENDIF; }
	YY_BREAK
case 102:
YY_RULE_SETUP
#line 238 "fortran.lex"
{strcpy(yylval.na,fortran_text);
                            return TOK_LOGICALIF_PAR;
                            }
	YY_BREAK
case 103:
/* rule 103 can match eol */
*yy_cp = (yy_hold_char); /* undo effects of setting up yytext */
YY_LINENO_REWIND_TO(yy_bp + 2);
(yy_c_buf_p) = yy_cp = yy_bp + 2;
YY_DO_BEFORE_ACTION; /* set up yytext again */
YY_RULE_SETUP
#line 241 "fortran.lex"
{strcpy(yylval.na,fortran_text);
                            return TOK_NAME;
                            }
	YY_BREAK
case 104:
YY_RULE_SETUP
#line 244 "fortran.lex"
{strcpy(yylval.na,fortran_text);
                            return TOK_LOGICALIF_PAR;
                            }
	YY_BREAK
case 105:
YY_RULE_SETUP
#line 247 "fortran.lex"
{ return TOK_SELECTCASE; }
	YY_BREAK
case 106:
YY_RULE_SETUP
#line 248 "fortran.lex"
{ if (in_select_case_stmt > 0) return TOK_CASE ; else return TOK_NAME;}
	YY_BREAK
case 107:
YY_RULE_SETUP
#line 249 "fortran.lex"
{ return TOK_DEFAULT; }
	YY_BREAK
case 108:
YY_RULE_SETUP
#line 250 "fortran.lex"
{ return TOK_ENDSELECT; }
	YY_BREAK
case 109:
YY_RULE_SETUP
#line 251 "fortran.lex"
{ return TOK_FILE; }
	YY_BREAK
case 110:
YY_RULE_SETUP
#line 252 "fortran.lex"
{ return TOK_ACCESS; }
	YY_BREAK
case 111:
YY_RULE_SETUP
#line 253 "fortran.lex"
{ return TOK_ACTION; }
	YY_BREAK
case 112:
YY_RULE_SETUP
#line 254 "fortran.lex"
{ return TOK_IOLENGTH; }
	YY_BREAK
case 113:
YY_RULE_SETUP
#line 255 "fortran.lex"
{ return TOK_UNIT; }
	YY_BREAK
case 114:
YY_RULE_SETUP
#line 256 "fortran.lex"
{ return TOK_OPENED; }
	YY_BREAK
case 115:
YY_RULE_SETUP
#line 257 "fortran.lex"
{ return TOK_FMT; }
	YY_BREAK
case 116:
YY_RULE_SETUP
#line 258 "fortran.lex"
{ return TOK_NML; }
	YY_BREAK
case 117:
YY_RULE_SETUP
#line 259 "fortran.lex"
{ return TOK_END; }
	YY_BREAK
case 118:
YY_RULE_SETUP
#line 260 "fortran.lex"
{ return TOK_EOR; }
	YY_BREAK
case 119:
*yy_cp = (yy_hold_char); /* undo effects of setting up yytext */
(yy_c_buf_p) = yy_cp = yy_bp + 3;
YY_DO_BEFORE_ACTION; /* set up yytext again */
YY_RULE_SETUP
#line 261 "fortran.lex"
{
                            if (in_char_selector ==1)
                               return TOK_LEN;
                            else
                            {
                            strcpy(yylval.na,fortran_text); return TOK_NAME;
                            }
                            }
	YY_BREAK
case 120:
*yy_cp = (yy_hold_char); /* undo effects of setting up yytext */
(yy_c_buf_p) = yy_cp = yy_bp + 4;
YY_DO_BEFORE_ACTION; /* set up yytext again */
YY_RULE_SETUP
#line 269 "fortran.lex"
{
                            if ((in_char_selector==1) || (in_kind_selector == 1))
                               return TOK_KIND;
                            else
                            {
                            strcpy(yylval.na,fortran_text); return TOK_NAME;
                            }
                            }
	YY_BREAK
case 121:
YY_RULE_SETUP
#line 277 "fortran.lex"
{ return TOK_ERRMSG; }
	YY_BREAK
case 122:
YY_RULE_SETUP
#line 278 "fortran.lex"
{ return TOK_MOLD; }
	YY_BREAK
case 123:
YY_RULE_SETUP
#line 279 "fortran.lex"
{ return TOK_SOURCE; }
	YY_BREAK
case 124:
YY_RULE_SETUP
#line 280 "fortran.lex"
{ return TOK_POSITION; }
	YY_BREAK
case 125:
YY_RULE_SETUP
#line 281 "fortran.lex"
{ return TOK_IOMSG; }
	YY_BREAK
case 126:
YY_RULE_SETUP
#line 282 "fortran.lex"
{ return TOK_IOSTAT; }
	YY_BREAK
case 127:
YY_RULE_SETUP
#line 283 "fortran.lex"
{ return TOK_ERR; }
	YY_BREAK
case 128:
YY_RULE_SETUP
#line 284 "fortran.lex"
{ return TOK_FORM; }
	YY_BREAK
case 129:
*yy_cp = (yy_hold_char); /* undo effects of setting up yytext */
(yy_c_buf_p) = yy_cp = yy_bp + 4;
YY_DO_BEFORE_ACTION; /* set up yytext again */
YY_RULE_SETUP
#line 285 "fortran.lex"
{
                            if (in_inquire==1)
                               return TOK_NAME_EQ;
                            else
                            {
                            strcpy(yylval.na,fortran_text); return TOK_NAME;
                            }
                            }
	YY_BREAK
case 130:
YY_RULE_SETUP
#line 293 "fortran.lex"
{ return TOK_RECL; }
	YY_BREAK
case 131:
*yy_cp = (yy_hold_char); /* undo effects of setting up yytext */
(yy_c_buf_p) = yy_cp = yy_bp + 3;
YY_DO_BEFORE_ACTION; /* set up yytext again */
YY_RULE_SETUP
#line 294 "fortran.lex"
{ if (in_io_control_spec == 1)
                              return TOK_REC;
                             else
                             {
                             strcpy(yylval.na,fortran_text); return TOK_NAME;
                             }
                             }
	YY_BREAK
case 132:
*yy_cp = (yy_hold_char); /* undo effects of setting up yytext */
(yy_c_buf_p) = yy_cp = yy_bp + 6;
YY_DO_BEFORE_ACTION; /* set up yytext again */
YY_RULE_SETUP
#line 301 "fortran.lex"
{ if (close_or_connect == 1)
                              return TOK_STATUS;
                             else
                             {
                             strcpy(yylval.na,fortran_text); return TOK_NAME;
                             }
                             }
	YY_BREAK
case 133:
YY_RULE_SETUP
#line 308 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NAME;}
	YY_BREAK
case 134:
YY_RULE_SETUP
#line 309 "fortran.lex"
{ return TOK_EXIST; }
	YY_BREAK
case 135:
YY_RULE_SETUP
#line 310 "fortran.lex"
{ return TOK_CYCLE; }
	YY_BREAK
case 136:
YY_RULE_SETUP
#line 311 "fortran.lex"
{ return TOK_BACKSPACE; }
	YY_BREAK
case 137:
YY_RULE_SETUP
#line 312 "fortran.lex"
{ return TOK_FOURDOTS;  }
	YY_BREAK
case 138:
/* rule 138 can match eol */
YY_RULE_SETUP
#line 313 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_DSLASH; }
	YY_BREAK
case 139:
YY_RULE_SETUP
#line 314 "fortran.lex"
{ return TOK_LEFTAB; }
	YY_BREAK
case 140:
YY_RULE_SETUP
#line 315 "fortran.lex"
{ return TOK_RIGHTAB; }
	YY_BREAK
case 141:
YY_RULE_SETUP
#line 316 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_SLASH; }
	YY_BREAK
case 142:
/* rule 142 can match eol */
YY_RULE_SETUP
#line 317 "fortran.lex"
{
                              INCREMENT_LINE_NUM() ; strcpy(yylval.na,fortran_text); return TOK_CHAR_CUT; }
	YY_BREAK
case 143:
/* rule 143 can match eol */
YY_RULE_SETUP
#line 319 "fortran.lex"
{Add_Include_1(fortran_text);}
	YY_BREAK
case 144:
YY_RULE_SETUP
#line 320 "fortran.lex"
{}
	YY_BREAK
case 145:
/* rule 145 can match eol */
YY_RULE_SETUP
#line 321 "fortran.lex"
{
                  if (inmoduledeclare == 0 )
                  {
                  pos_end=setposcur();
                  RemoveWordSET_0(fortran_out,pos_curinclude,pos_end-pos_curinclude);
                  }
                  out_of_donottreat();
                  }
	YY_BREAK
case 146:
/* rule 146 can match eol */
YY_RULE_SETUP
#line 329 "fortran.lex"
{ strcpy(yylval.na,fortran_text);return TOK_CHAR_CONSTANT; }
	YY_BREAK
case 147:
/* rule 147 can match eol */
YY_RULE_SETUP
#line 330 "fortran.lex"
{ strcpy(yylval.na,fortran_text);return TOK_CHAR_MESSAGE; }
	YY_BREAK
case 148:
YY_RULE_SETUP
#line 331 "fortran.lex"
{ BEGIN(donottreat_interface); }
	YY_BREAK
case 149:
/* rule 149 can match eol */
YY_RULE_SETUP
#line 332 "fortran.lex"
{ out_of_donottreat(); return '\n'; }
	YY_BREAK
case 150:
/* rule 150 can match eol */
YY_RULE_SETUP
#line 333 "fortran.lex"
{INCREMENT_LINE_NUM() ; }
	YY_BREAK
case 151:
/* rule 151 can match eol */
YY_RULE_SETUP
#line 334 "fortran.lex"
{strcpy(yylval.na,fortran_text); removenewline(yylval.na);
                            return TOK_NAME; }
	YY_BREAK
case 152:
YY_RULE_SETUP
#line 336 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NAME; }
	YY_BREAK
case 153:
YY_RULE_SETUP
#line 337 "fortran.lex"
{strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
	YY_BREAK
case 154:
/* rule 154 can match eol */
*yy_cp = (yy_hold_char); /* undo effects of setting up yytext */
YY_LINENO_REWIND_TO(yy_cp - 1);
(yy_c_buf_p) = yy_cp -= 1;
YY_DO_BEFORE_ACTION; /* set up yytext again */
YY_RULE_SETUP
#line 338 "fortran.lex"
{  // REAL1
                              strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
	YY_BREAK
case 155:
YY_RULE_SETUP
#line 340 "fortran.lex"
{  // REAL2
                              strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
	YY_BREAK
case 156:
YY_RULE_SETUP
#line 342 "fortran.lex"
{ strcpy(yylval.na,fortran_text);
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
	YY_BREAK
case 157:
YY_RULE_SETUP
#line 356 "fortran.lex"
{}
	YY_BREAK
case 158:
YY_RULE_SETUP
#line 357 "fortran.lex"
{}
	YY_BREAK
case 159:
*yy_cp = (yy_hold_char); /* undo effects of setting up yytext */
(yy_c_buf_p) = yy_cp = yy_bp + 1;
YY_DO_BEFORE_ACTION; /* set up yytext again */
YY_RULE_SETUP
#line 358 "fortran.lex"
{
                            in_complex_literal = -1;
                            return (int) *fortran_text;
                            }
	YY_BREAK
case 160:
YY_RULE_SETUP
#line 362 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return (int) *fortran_text; }
	YY_BREAK
case 161:
YY_RULE_SETUP
#line 363 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return (int) *fortran_text; }
	YY_BREAK
case 162:
YY_RULE_SETUP
#line 364 "fortran.lex"
{ lastwasendofstmt=1; token_since_endofstmt = 0; return TOK_SEMICOLON; }
	YY_BREAK
case 163:
YY_RULE_SETUP
#line 365 "fortran.lex"
{ if (in_complex_literal==-1) {return TOK_COMMACOMPLEX; in_complex_literal=0;} else; return (int) *fortran_text; }
	YY_BREAK
case 164:
YY_RULE_SETUP
#line 366 "fortran.lex"
{ return (int) *fortran_text; }
	YY_BREAK
case 165:
YY_RULE_SETUP
#line 367 "fortran.lex"
{ return (int) *fortran_text; }
	YY_BREAK
case 166:
YY_RULE_SETUP
#line 368 "fortran.lex"
{ return (int) *fortran_text; }
	YY_BREAK
case 167:
/* rule 167 can match eol */
YY_RULE_SETUP
#line 369 "fortran.lex"
{ INCREMENT_LINE_NUM() ; lastwasendofstmt=1; token_since_endofstmt = 0; increment_nbtokens = 0; return '\n'; }
	YY_BREAK
case 168:
YY_RULE_SETUP
#line 370 "fortran.lex"
{increment_nbtokens = 0;}
	YY_BREAK
case 169:
/* rule 169 can match eol */
YY_RULE_SETUP
#line 371 "fortran.lex"
{
                              return TOK_LABEL_FORMAT; }
	YY_BREAK
case 170:
/* rule 170 can match eol */
YY_RULE_SETUP
#line 373 "fortran.lex"
{return TOK_LABEL_FORMAT; }
	YY_BREAK
case 171:
/* rule 171 can match eol */
YY_RULE_SETUP
#line 374 "fortran.lex"
{ INCREMENT_LINE_NUM() ; newlinef90=1; }
	YY_BREAK
case 172:
/* rule 172 can match eol */
YY_RULE_SETUP
#line 375 "fortran.lex"
{ INCREMENT_LINE_NUM() ;}
	YY_BREAK
case 173:
/* rule 173 can match eol */
YY_RULE_SETUP
#line 377 "fortran.lex"
{INCREMENT_LINE_NUM() ; BEGIN(donottreat); }
	YY_BREAK
case 174:
/* rule 174 can match eol */
YY_RULE_SETUP
#line 378 "fortran.lex"
{out_of_donottreat(); return '\n'; }
	YY_BREAK
case 175:
/* rule 175 can match eol */
YY_RULE_SETUP
#line 379 "fortran.lex"
{INCREMENT_LINE_NUM() ; }
	YY_BREAK
case 176:
/* rule 176 can match eol */
YY_RULE_SETUP
#line 380 "fortran.lex"
{INCREMENT_LINE_NUM() ; increment_nbtokens = 0;}
	YY_BREAK
case 177:
/* rule 177 can match eol */
YY_RULE_SETUP
#line 381 "fortran.lex"
{INCREMENT_LINE_NUM() ; increment_nbtokens = 0;}
	YY_BREAK
case 178:
YY_RULE_SETUP
#line 382 "fortran.lex"
{increment_nbtokens = 0;}
	YY_BREAK
case YY_STATE_EOF(INITIAL):
case YY_STATE_EOF(parameter):
case YY_STATE_EOF(character):
case YY_STATE_EOF(donottreat):
case YY_STATE_EOF(donottreat_interface):
case YY_STATE_EOF(includestate):
case YY_STATE_EOF(fortran77style):
case YY_STATE_EOF(fortran90style):
#line 383 "fortran.lex"
{endoffile = 1; yyterminate();}
	YY_BREAK
case 179:
YY_RULE_SETUP
#line 384 "fortran.lex"
ECHO;
	YY_BREAK
#line 5146 "fortran.yy.c"

	case YY_END_OF_BUFFER:
		{
		/* Amount of text matched not including the EOB char. */
		int yy_amount_of_matched_text = (int) (yy_cp - (yytext_ptr)) - 1;

		/* Undo the effects of YY_DO_BEFORE_ACTION. */
		*yy_cp = (yy_hold_char);
		YY_RESTORE_YY_MORE_OFFSET

		if ( YY_CURRENT_BUFFER_LVALUE->yy_buffer_status == YY_BUFFER_NEW )
			{
			/* We're scanning a new file or input source.  It's
			 * possible that this happened because the user
			 * just pointed yyin at a new source and called
			 * yylex().  If so, then we have to assure
			 * consistency between YY_CURRENT_BUFFER and our
			 * globals.  Here is the right place to do so, because
			 * this is the first action (other than possibly a
			 * back-up) that will match for the new input source.
			 */
			(yy_n_chars) = YY_CURRENT_BUFFER_LVALUE->yy_n_chars;
			YY_CURRENT_BUFFER_LVALUE->yy_input_file = yyin;
			YY_CURRENT_BUFFER_LVALUE->yy_buffer_status = YY_BUFFER_NORMAL;
			}

		/* Note that here we test for yy_c_buf_p "<=" to the position
		 * of the first EOB in the buffer, since yy_c_buf_p will
		 * already have been incremented past the NUL character
		 * (since all states make transitions on EOB to the
		 * end-of-buffer state).  Contrast this with the test
		 * in input().
		 */
		if ( (yy_c_buf_p) <= &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)] )
			{ /* This was really a NUL. */
			yy_state_type yy_next_state;

			(yy_c_buf_p) = (yytext_ptr) + yy_amount_of_matched_text;

			yy_current_state = yy_get_previous_state(  );

			/* Okay, we're now positioned to make the NUL
			 * transition.  We couldn't have
			 * yy_get_previous_state() go ahead and do it
			 * for us because it doesn't know how to deal
			 * with the possibility of jamming (and we don't
			 * want to build jamming into it because then it
			 * will run more slowly).
			 */

			yy_next_state = yy_try_NUL_trans( yy_current_state );

			yy_bp = (yytext_ptr) + YY_MORE_ADJ;

			if ( yy_next_state )
				{
				/* Consume the NUL. */
				yy_cp = ++(yy_c_buf_p);
				yy_current_state = yy_next_state;
				goto yy_match;
				}

			else
				{
				yy_cp = (yy_c_buf_p);
				goto yy_find_action;
				}
			}

		else switch ( yy_get_next_buffer(  ) )
			{
			case EOB_ACT_END_OF_FILE:
				{
				(yy_did_buffer_switch_on_eof) = 0;

				if ( yywrap(  ) )
					{
					/* Note: because we've taken care in
					 * yy_get_next_buffer() to have set up
					 * yytext, we can now set up
					 * yy_c_buf_p so that if some total
					 * hoser (like flex itself) wants to
					 * call the scanner after we return the
					 * YY_NULL, it'll still work - another
					 * YY_NULL will get returned.
					 */
					(yy_c_buf_p) = (yytext_ptr) + YY_MORE_ADJ;

					yy_act = YY_STATE_EOF(YY_START);
					goto do_action;
					}

				else
					{
					if ( ! (yy_did_buffer_switch_on_eof) )
						YY_NEW_FILE;
					}
				break;
				}

			case EOB_ACT_CONTINUE_SCAN:
				(yy_c_buf_p) =
					(yytext_ptr) + yy_amount_of_matched_text;

				yy_current_state = yy_get_previous_state(  );

				yy_cp = (yy_c_buf_p);
				yy_bp = (yytext_ptr) + YY_MORE_ADJ;
				goto yy_match;

			case EOB_ACT_LAST_MATCH:
				(yy_c_buf_p) =
				&YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)];

				yy_current_state = yy_get_previous_state(  );

				yy_cp = (yy_c_buf_p);
				yy_bp = (yytext_ptr) + YY_MORE_ADJ;
				goto yy_find_action;
			}
		break;
		}

	default:
		YY_FATAL_ERROR(
			"fatal flex scanner internal error--no action found" );
	} /* end of action switch */
		} /* end of scanning one token */
	} /* end of user's declarations */
} /* end of yylex */

/* yy_get_next_buffer - try to read in a new buffer
 *
 * Returns a code representing an action:
 *	EOB_ACT_LAST_MATCH -
 *	EOB_ACT_CONTINUE_SCAN - continue scanning from current position
 *	EOB_ACT_END_OF_FILE - end of file
 */
static int yy_get_next_buffer (void)
{
    	char *dest = YY_CURRENT_BUFFER_LVALUE->yy_ch_buf;
	char *source = (yytext_ptr);
	int number_to_move, i;
	int ret_val;

	if ( (yy_c_buf_p) > &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars) + 1] )
		YY_FATAL_ERROR(
		"fatal flex scanner internal error--end of buffer missed" );

	if ( YY_CURRENT_BUFFER_LVALUE->yy_fill_buffer == 0 )
		{ /* Don't try to fill the buffer, so this is an EOF. */
		if ( (yy_c_buf_p) - (yytext_ptr) - YY_MORE_ADJ == 1 )
			{
			/* We matched a single character, the EOB, so
			 * treat this as a final EOF.
			 */
			return EOB_ACT_END_OF_FILE;
			}

		else
			{
			/* We matched some text prior to the EOB, first
			 * process it.
			 */
			return EOB_ACT_LAST_MATCH;
			}
		}

	/* Try to read more data. */

	/* First move last chars to start of buffer. */
	number_to_move = (int) ((yy_c_buf_p) - (yytext_ptr) - 1);

	for ( i = 0; i < number_to_move; ++i )
		*(dest++) = *(source++);

	if ( YY_CURRENT_BUFFER_LVALUE->yy_buffer_status == YY_BUFFER_EOF_PENDING )
		/* don't do the read, it's not guaranteed to return an EOF,
		 * just force an EOF
		 */
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars) = 0;

	else
		{
			int num_to_read =
			YY_CURRENT_BUFFER_LVALUE->yy_buf_size - number_to_move - 1;

		while ( num_to_read <= 0 )
			{ /* Not enough room in the buffer - grow it. */

			YY_FATAL_ERROR(
"input buffer overflow, can't enlarge buffer because scanner uses REJECT" );

			}

		if ( num_to_read > YY_READ_BUF_SIZE )
			num_to_read = YY_READ_BUF_SIZE;

		/* Read in more data. */
		YY_INPUT( (&YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[number_to_move]),
			(yy_n_chars), num_to_read );

		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars);
		}

	if ( (yy_n_chars) == 0 )
		{
		if ( number_to_move == YY_MORE_ADJ )
			{
			ret_val = EOB_ACT_END_OF_FILE;
			yyrestart( yyin  );
			}

		else
			{
			ret_val = EOB_ACT_LAST_MATCH;
			YY_CURRENT_BUFFER_LVALUE->yy_buffer_status =
				YY_BUFFER_EOF_PENDING;
			}
		}

	else
		ret_val = EOB_ACT_CONTINUE_SCAN;

	if (((yy_n_chars) + number_to_move) > YY_CURRENT_BUFFER_LVALUE->yy_buf_size) {
		/* Extend the array by 50%, plus the number we really need. */
		int new_size = (yy_n_chars) + number_to_move + ((yy_n_chars) >> 1);
		YY_CURRENT_BUFFER_LVALUE->yy_ch_buf = (char *) yyrealloc(
			(void *) YY_CURRENT_BUFFER_LVALUE->yy_ch_buf, (yy_size_t) new_size  );
		if ( ! YY_CURRENT_BUFFER_LVALUE->yy_ch_buf )
			YY_FATAL_ERROR( "out of dynamic memory in yy_get_next_buffer()" );
		/* "- 2" to take care of EOB's */
		YY_CURRENT_BUFFER_LVALUE->yy_buf_size = (int) (new_size - 2);
	}

	(yy_n_chars) += number_to_move;
	YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)] = YY_END_OF_BUFFER_CHAR;
	YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars) + 1] = YY_END_OF_BUFFER_CHAR;

	(yytext_ptr) = &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[0];

	return ret_val;
}

/* yy_get_previous_state - get the state just before the EOB char was reached */

    static yy_state_type yy_get_previous_state (void)
{
	yy_state_type yy_current_state;
	char *yy_cp;
    
	yy_current_state = (yy_start);
	yy_current_state += YY_AT_BOL();

	(yy_state_ptr) = (yy_state_buf);
	*(yy_state_ptr)++ = yy_current_state;

	for ( yy_cp = (yytext_ptr) + YY_MORE_ADJ; yy_cp < (yy_c_buf_p); ++yy_cp )
		{
		YY_CHAR yy_c = (*yy_cp ? yy_ec[YY_SC_TO_UI(*yy_cp)] : 1);
		while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
			{
			yy_current_state = (int) yy_def[yy_current_state];
			if ( yy_current_state >= 1898 )
				yy_c = yy_meta[yy_c];
			}
		yy_current_state = yy_nxt[yy_base[yy_current_state] + yy_c];
		*(yy_state_ptr)++ = yy_current_state;
		}

	return yy_current_state;
}

/* yy_try_NUL_trans - try to make a transition on the NUL character
 *
 * synopsis
 *	next_state = yy_try_NUL_trans( current_state );
 */
    static yy_state_type yy_try_NUL_trans  (yy_state_type yy_current_state )
{
	int yy_is_jam;
    
	YY_CHAR yy_c = 1;
	while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
		{
		yy_current_state = (int) yy_def[yy_current_state];
		if ( yy_current_state >= 1898 )
			yy_c = yy_meta[yy_c];
		}
	yy_current_state = yy_nxt[yy_base[yy_current_state] + yy_c];
	yy_is_jam = (yy_current_state == 1897);
	if ( ! yy_is_jam )
		*(yy_state_ptr)++ = yy_current_state;

		return yy_is_jam ? 0 : yy_current_state;
}

#ifndef YY_NO_UNPUT

    static void yyunput (int c, char * yy_bp )
{
	char *yy_cp;
    
    yy_cp = (yy_c_buf_p);

	/* undo effects of setting up yytext */
	*yy_cp = (yy_hold_char);

	if ( yy_cp < YY_CURRENT_BUFFER_LVALUE->yy_ch_buf + 2 )
		{ /* need to shift things up to make room */
		/* +2 for EOB chars. */
		int number_to_move = (yy_n_chars) + 2;
		char *dest = &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[
					YY_CURRENT_BUFFER_LVALUE->yy_buf_size + 2];
		char *source =
				&YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[number_to_move];

		while ( source > YY_CURRENT_BUFFER_LVALUE->yy_ch_buf )
			*--dest = *--source;

		yy_cp += (int) (dest - source);
		yy_bp += (int) (dest - source);
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars =
			(yy_n_chars) = (int) YY_CURRENT_BUFFER_LVALUE->yy_buf_size;

		if ( yy_cp < YY_CURRENT_BUFFER_LVALUE->yy_ch_buf + 2 )
			YY_FATAL_ERROR( "flex scanner push-back overflow" );
		}

	*--yy_cp = (char) c;

	(yytext_ptr) = yy_bp;
	(yy_hold_char) = *yy_cp;
	(yy_c_buf_p) = yy_cp;
}

#endif

#ifndef YY_NO_INPUT
#ifdef __cplusplus
    static int yyinput (void)
#else
    static int input  (void)
#endif

{
	int c;
    
	*(yy_c_buf_p) = (yy_hold_char);

	if ( *(yy_c_buf_p) == YY_END_OF_BUFFER_CHAR )
		{
		/* yy_c_buf_p now points to the character we want to return.
		 * If this occurs *before* the EOB characters, then it's a
		 * valid NUL; if not, then we've hit the end of the buffer.
		 */
		if ( (yy_c_buf_p) < &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)] )
			/* This was really a NUL. */
			*(yy_c_buf_p) = '\0';

		else
			{ /* need more input */
			int offset = (int) ((yy_c_buf_p) - (yytext_ptr));
			++(yy_c_buf_p);

			switch ( yy_get_next_buffer(  ) )
				{
				case EOB_ACT_LAST_MATCH:
					/* This happens because yy_g_n_b()
					 * sees that we've accumulated a
					 * token and flags that we need to
					 * try matching the token before
					 * proceeding.  But for input(),
					 * there's no matching to consider.
					 * So convert the EOB_ACT_LAST_MATCH
					 * to EOB_ACT_END_OF_FILE.
					 */

					/* Reset buffer status. */
					yyrestart( yyin );

					/*FALLTHROUGH*/

				case EOB_ACT_END_OF_FILE:
					{
					if ( yywrap(  ) )
						return 0;

					if ( ! (yy_did_buffer_switch_on_eof) )
						YY_NEW_FILE;
#ifdef __cplusplus
					return yyinput();
#else
					return input();
#endif
					}

				case EOB_ACT_CONTINUE_SCAN:
					(yy_c_buf_p) = (yytext_ptr) + offset;
					break;
				}
			}
		}

	c = *(unsigned char *) (yy_c_buf_p);	/* cast for 8-bit char's */
	*(yy_c_buf_p) = '\0';	/* preserve yytext */
	(yy_hold_char) = *++(yy_c_buf_p);

	YY_CURRENT_BUFFER_LVALUE->yy_at_bol = (c == '\n');

	return c;
}
#endif	/* ifndef YY_NO_INPUT */

/** Immediately switch to a different input stream.
 * @param input_file A readable stream.
 * 
 * @note This function does not reset the start condition to @c INITIAL .
 */
    void yyrestart  (FILE * input_file )
{
    
	if ( ! YY_CURRENT_BUFFER ){
        yyensure_buffer_stack ();
		YY_CURRENT_BUFFER_LVALUE =
            yy_create_buffer( yyin, YY_BUF_SIZE );
	}

	yy_init_buffer( YY_CURRENT_BUFFER, input_file );
	yy_load_buffer_state(  );
}

/** Switch to a different input buffer.
 * @param new_buffer The new input buffer.
 * 
 */
    void yy_switch_to_buffer  (YY_BUFFER_STATE  new_buffer )
{
    
	/* TODO. We should be able to replace this entire function body
	 * with
	 *		yypop_buffer_state();
	 *		yypush_buffer_state(new_buffer);
     */
	yyensure_buffer_stack ();
	if ( YY_CURRENT_BUFFER == new_buffer )
		return;

	if ( YY_CURRENT_BUFFER )
		{
		/* Flush out information for old buffer. */
		*(yy_c_buf_p) = (yy_hold_char);
		YY_CURRENT_BUFFER_LVALUE->yy_buf_pos = (yy_c_buf_p);
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars);
		}

	YY_CURRENT_BUFFER_LVALUE = new_buffer;
	yy_load_buffer_state(  );

	/* We don't actually know whether we did this switch during
	 * EOF (yywrap()) processing, but the only time this flag
	 * is looked at is after yywrap() is called, so it's safe
	 * to go ahead and always set it.
	 */
	(yy_did_buffer_switch_on_eof) = 1;
}

static void yy_load_buffer_state  (void)
{
    	(yy_n_chars) = YY_CURRENT_BUFFER_LVALUE->yy_n_chars;
	(yytext_ptr) = (yy_c_buf_p) = YY_CURRENT_BUFFER_LVALUE->yy_buf_pos;
	yyin = YY_CURRENT_BUFFER_LVALUE->yy_input_file;
	(yy_hold_char) = *(yy_c_buf_p);
}

/** Allocate and initialize an input buffer state.
 * @param file A readable stream.
 * @param size The character buffer size in bytes. When in doubt, use @c YY_BUF_SIZE.
 * 
 * @return the allocated buffer state.
 */
    YY_BUFFER_STATE yy_create_buffer  (FILE * file, int  size )
{
	YY_BUFFER_STATE b;
    
	b = (YY_BUFFER_STATE) yyalloc( sizeof( struct yy_buffer_state )  );
	if ( ! b )
		YY_FATAL_ERROR( "out of dynamic memory in yy_create_buffer()" );

	b->yy_buf_size = size;

	/* yy_ch_buf has to be 2 characters longer than the size given because
	 * we need to put in 2 end-of-buffer characters.
	 */
	b->yy_ch_buf = (char *) yyalloc( (yy_size_t) (b->yy_buf_size + 2)  );
	if ( ! b->yy_ch_buf )
		YY_FATAL_ERROR( "out of dynamic memory in yy_create_buffer()" );

	b->yy_is_our_buffer = 1;

	yy_init_buffer( b, file );

	return b;
}

/** Destroy the buffer.
 * @param b a buffer created with yy_create_buffer()
 * 
 */
    void yy_delete_buffer (YY_BUFFER_STATE  b )
{
    
	if ( ! b )
		return;

	if ( b == YY_CURRENT_BUFFER ) /* Not sure if we should pop here. */
		YY_CURRENT_BUFFER_LVALUE = (YY_BUFFER_STATE) 0;

	if ( b->yy_is_our_buffer )
		yyfree( (void *) b->yy_ch_buf  );

	yyfree( (void *) b  );
}

/* Initializes or reinitializes a buffer.
 * This function is sometimes called more than once on the same buffer,
 * such as during a yyrestart() or at EOF.
 */
    static void yy_init_buffer  (YY_BUFFER_STATE  b, FILE * file )

{
	int oerrno = errno;
    
	yy_flush_buffer( b );

	b->yy_input_file = file;
	b->yy_fill_buffer = 1;

    /* If b is the current buffer, then yy_init_buffer was _probably_
     * called from yyrestart() or through yy_get_next_buffer.
     * In that case, we don't want to reset the lineno or column.
     */
    if (b != YY_CURRENT_BUFFER){
        b->yy_bs_lineno = 1;
        b->yy_bs_column = 0;
    }

        b->yy_is_interactive = file ? (isatty( fileno(file) ) > 0) : 0;
    
	errno = oerrno;
}

/** Discard all buffered characters. On the next scan, YY_INPUT will be called.
 * @param b the buffer state to be flushed, usually @c YY_CURRENT_BUFFER.
 * 
 */
    void yy_flush_buffer (YY_BUFFER_STATE  b )
{
    	if ( ! b )
		return;

	b->yy_n_chars = 0;

	/* We always need two end-of-buffer characters.  The first causes
	 * a transition to the end-of-buffer state.  The second causes
	 * a jam in that state.
	 */
	b->yy_ch_buf[0] = YY_END_OF_BUFFER_CHAR;
	b->yy_ch_buf[1] = YY_END_OF_BUFFER_CHAR;

	b->yy_buf_pos = &b->yy_ch_buf[0];

	b->yy_at_bol = 1;
	b->yy_buffer_status = YY_BUFFER_NEW;

	if ( b == YY_CURRENT_BUFFER )
		yy_load_buffer_state(  );
}

/** Pushes the new state onto the stack. The new state becomes
 *  the current state. This function will allocate the stack
 *  if necessary.
 *  @param new_buffer The new state.
 *  
 */
void yypush_buffer_state (YY_BUFFER_STATE new_buffer )
{
    	if (new_buffer == NULL)
		return;

	yyensure_buffer_stack();

	/* This block is copied from yy_switch_to_buffer. */
	if ( YY_CURRENT_BUFFER )
		{
		/* Flush out information for old buffer. */
		*(yy_c_buf_p) = (yy_hold_char);
		YY_CURRENT_BUFFER_LVALUE->yy_buf_pos = (yy_c_buf_p);
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars);
		}

	/* Only push if top exists. Otherwise, replace top. */
	if (YY_CURRENT_BUFFER)
		(yy_buffer_stack_top)++;
	YY_CURRENT_BUFFER_LVALUE = new_buffer;

	/* copied from yy_switch_to_buffer. */
	yy_load_buffer_state(  );
	(yy_did_buffer_switch_on_eof) = 1;
}

/** Removes and deletes the top of the stack, if present.
 *  The next element becomes the new top.
 *  
 */
void yypop_buffer_state (void)
{
    	if (!YY_CURRENT_BUFFER)
		return;

	yy_delete_buffer(YY_CURRENT_BUFFER );
	YY_CURRENT_BUFFER_LVALUE = NULL;
	if ((yy_buffer_stack_top) > 0)
		--(yy_buffer_stack_top);

	if (YY_CURRENT_BUFFER) {
		yy_load_buffer_state(  );
		(yy_did_buffer_switch_on_eof) = 1;
	}
}

/* Allocates the stack if it does not exist.
 *  Guarantees space for at least one push.
 */
static void yyensure_buffer_stack (void)
{
	yy_size_t num_to_alloc;
    
	if (!(yy_buffer_stack)) {

		/* First allocation is just for 2 elements, since we don't know if this
		 * scanner will even need a stack. We use 2 instead of 1 to avoid an
		 * immediate realloc on the next call.
         */
      num_to_alloc = 1; /* After all that talk, this was set to 1 anyways... */
		(yy_buffer_stack) = (struct yy_buffer_state**)yyalloc
								(num_to_alloc * sizeof(struct yy_buffer_state*)
								);
		if ( ! (yy_buffer_stack) )
			YY_FATAL_ERROR( "out of dynamic memory in yyensure_buffer_stack()" );

		memset((yy_buffer_stack), 0, num_to_alloc * sizeof(struct yy_buffer_state*));

		(yy_buffer_stack_max) = num_to_alloc;
		(yy_buffer_stack_top) = 0;
		return;
	}

	if ((yy_buffer_stack_top) >= ((yy_buffer_stack_max)) - 1){

		/* Increase the buffer to prepare for a possible push. */
		yy_size_t grow_size = 8 /* arbitrary grow size */;

		num_to_alloc = (yy_buffer_stack_max) + grow_size;
		(yy_buffer_stack) = (struct yy_buffer_state**)yyrealloc
								((yy_buffer_stack),
								num_to_alloc * sizeof(struct yy_buffer_state*)
								);
		if ( ! (yy_buffer_stack) )
			YY_FATAL_ERROR( "out of dynamic memory in yyensure_buffer_stack()" );

		/* zero only the new slots.*/
		memset((yy_buffer_stack) + (yy_buffer_stack_max), 0, grow_size * sizeof(struct yy_buffer_state*));
		(yy_buffer_stack_max) = num_to_alloc;
	}
}

/** Setup the input buffer state to scan directly from a user-specified character buffer.
 * @param base the character buffer
 * @param size the size in bytes of the character buffer
 * 
 * @return the newly allocated buffer state object.
 */
YY_BUFFER_STATE yy_scan_buffer  (char * base, yy_size_t  size )
{
	YY_BUFFER_STATE b;
    
	if ( size < 2 ||
	     base[size-2] != YY_END_OF_BUFFER_CHAR ||
	     base[size-1] != YY_END_OF_BUFFER_CHAR )
		/* They forgot to leave room for the EOB's. */
		return NULL;

	b = (YY_BUFFER_STATE) yyalloc( sizeof( struct yy_buffer_state )  );
	if ( ! b )
		YY_FATAL_ERROR( "out of dynamic memory in yy_scan_buffer()" );

	b->yy_buf_size = (int) (size - 2);	/* "- 2" to take care of EOB's */
	b->yy_buf_pos = b->yy_ch_buf = base;
	b->yy_is_our_buffer = 0;
	b->yy_input_file = NULL;
	b->yy_n_chars = b->yy_buf_size;
	b->yy_is_interactive = 0;
	b->yy_at_bol = 1;
	b->yy_fill_buffer = 0;
	b->yy_buffer_status = YY_BUFFER_NEW;

	yy_switch_to_buffer( b  );

	return b;
}

/** Setup the input buffer state to scan a string. The next call to yylex() will
 * scan from a @e copy of @a str.
 * @param yystr a NUL-terminated string to scan
 * 
 * @return the newly allocated buffer state object.
 * @note If you want to scan bytes that may contain NUL values, then use
 *       yy_scan_bytes() instead.
 */
YY_BUFFER_STATE yy_scan_string (const char * yystr )
{
    
	return yy_scan_bytes( yystr, (int) strlen(yystr) );
}

/** Setup the input buffer state to scan the given bytes. The next call to yylex() will
 * scan from a @e copy of @a bytes.
 * @param yybytes the byte buffer to scan
 * @param _yybytes_len the number of bytes in the buffer pointed to by @a bytes.
 * 
 * @return the newly allocated buffer state object.
 */
YY_BUFFER_STATE yy_scan_bytes  (const char * yybytes, int  _yybytes_len )
{
	YY_BUFFER_STATE b;
	char *buf;
	yy_size_t n;
	int i;
    
	/* Get memory for full buffer, including space for trailing EOB's. */
	n = (yy_size_t) (_yybytes_len + 2);
	buf = (char *) yyalloc( n  );
	if ( ! buf )
		YY_FATAL_ERROR( "out of dynamic memory in yy_scan_bytes()" );

	for ( i = 0; i < _yybytes_len; ++i )
		buf[i] = yybytes[i];

	buf[_yybytes_len] = buf[_yybytes_len+1] = YY_END_OF_BUFFER_CHAR;

	b = yy_scan_buffer( buf, n );
	if ( ! b )
		YY_FATAL_ERROR( "bad buffer in yy_scan_bytes()" );

	/* It's okay to grow etc. this buffer, and we should throw it
	 * away when we're done.
	 */
	b->yy_is_our_buffer = 1;

	return b;
}

#ifndef YY_EXIT_FAILURE
#define YY_EXIT_FAILURE 2
#endif

static void yynoreturn yy_fatal_error (const char* msg )
{
			fprintf( stderr, "%s\n", msg );
	exit( YY_EXIT_FAILURE );
}

/* Redefine yyless() so it works in section 3 code. */

#undef yyless
#define yyless(n) \
	do \
		{ \
		/* Undo effects of setting up yytext. */ \
        int yyless_macro_arg = (n); \
        YY_LESS_LINENO(yyless_macro_arg);\
		yytext[yyleng] = (yy_hold_char); \
		(yy_c_buf_p) = yytext + yyless_macro_arg; \
		(yy_hold_char) = *(yy_c_buf_p); \
		*(yy_c_buf_p) = '\0'; \
		yyleng = yyless_macro_arg; \
		} \
	while ( 0 )

/* Accessor  methods (get/set functions) to struct members. */

/** Get the current line number.
 * 
 */
int yyget_lineno  (void)
{
    
    return yylineno;
}

/** Get the input stream.
 * 
 */
FILE *yyget_in  (void)
{
        return yyin;
}

/** Get the output stream.
 * 
 */
FILE *yyget_out  (void)
{
        return yyout;
}

/** Get the length of the current token.
 * 
 */
int yyget_leng  (void)
{
        return yyleng;
}

/** Get the current token.
 * 
 */

char *yyget_text  (void)
{
        return yytext;
}

/** Set the current line number.
 * @param _line_number line number
 * 
 */
void yyset_lineno (int  _line_number )
{
    
    yylineno = _line_number;
}

/** Set the input stream. This does not discard the current
 * input buffer.
 * @param _in_str A readable stream.
 * 
 * @see yy_switch_to_buffer
 */
void yyset_in (FILE *  _in_str )
{
        yyin = _in_str ;
}

void yyset_out (FILE *  _out_str )
{
        yyout = _out_str ;
}

int yyget_debug  (void)
{
        return yy_flex_debug;
}

void yyset_debug (int  _bdebug )
{
        yy_flex_debug = _bdebug ;
}

static int yy_init_globals (void)
{
        /* Initialization is the same as for the non-reentrant scanner.
     * This function is called from yylex_destroy(), so don't allocate here.
     */

    (yy_buffer_stack) = NULL;
    (yy_buffer_stack_top) = 0;
    (yy_buffer_stack_max) = 0;
    (yy_c_buf_p) = NULL;
    (yy_init) = 0;
    (yy_start) = 0;

    (yy_state_buf) = 0;
    (yy_state_ptr) = 0;
    (yy_full_match) = 0;
    (yy_lp) = 0;

/* Defined in main.c */
#ifdef YY_STDINIT
    yyin = stdin;
    yyout = stdout;
#else
    yyin = NULL;
    yyout = NULL;
#endif

    /* For future reference: Set errno on error, since we are called by
     * yylex_init()
     */
    return 0;
}

/* yylex_destroy is for both reentrant and non-reentrant scanners. */
int yylex_destroy  (void)
{
    
    /* Pop the buffer stack, destroying each element. */
	while(YY_CURRENT_BUFFER){
		yy_delete_buffer( YY_CURRENT_BUFFER  );
		YY_CURRENT_BUFFER_LVALUE = NULL;
		yypop_buffer_state();
	}

	/* Destroy the stack itself. */
	yyfree((yy_buffer_stack) );
	(yy_buffer_stack) = NULL;

    yyfree ( (yy_state_buf) );
    (yy_state_buf)  = NULL;

    /* Reset the globals. This is important in a non-reentrant scanner so the next time
     * yylex() is called, initialization will occur. */
    yy_init_globals( );

    return 0;
}

/*
 * Internal utility routines.
 */

#ifndef yytext_ptr
static void yy_flex_strncpy (char* s1, const char * s2, int n )
{
		
	int i;
	for ( i = 0; i < n; ++i )
		s1[i] = s2[i];
}
#endif

#ifdef YY_NEED_STRLEN
static int yy_flex_strlen (const char * s )
{
	int n;
	for ( n = 0; s[n]; ++n )
		;

	return n;
}
#endif

void *yyalloc (yy_size_t  size )
{
			return malloc(size);
}

void *yyrealloc  (void * ptr, yy_size_t  size )
{
		
	/* The cast to (char *) in the following accommodates both
	 * implementations that use char* generic pointers, and those
	 * that use void* generic pointers.  It works with the latter
	 * because both ANSI C and C++ allow castless assignment from
	 * any pointer type to void*, and deal with argument conversions
	 * as though doing an assignment.
	 */
	return realloc(ptr, size);
}

void yyfree (void * ptr )
{
			free( (char *) ptr );	/* see yyrealloc() for (char *) cast */
}

#define YYTABLES_NAME "yytables"

#line 384 "fortran.lex"


void out_of_donottreat ( void )
{
    BEGIN(INITIAL);
    if (infixed) BEGIN(fortran77style) ;
    if (infree)  BEGIN(fortran90style) ;
    INCREMENT_LINE_NUM() ;
}

