LEX		= flex -i
YACC	= /usr/bin/bison -t -v -g --graph
#YACC = byacc -t -v

all: main.c fortran.c

main.c : convert.tab.c convert.yy.c
	cat   convert.tab.c convert.yy.c > main.c
	$(RM) convert.tab.c convert.yy.c

fortran.c : fortran.tab.c fortran.yy.c
	cat   fortran.tab.c fortran.yy.c > fortran.c
	$(RM) fortran.tab.c fortran.yy.c

convert.tab.c : convert.y decl.h
	$(YACC) -p convert_ convert.y

fortran.tab.c : fortran.y decl.h
	$(YACC) -p fortran_ fortran.y

convert.yy.c : convert.lex
	$(LEX) -P convert_ -o convert.yy.c convert.lex

fortran.yy.c : fortran.lex
	$(LEX) -P fortran_ -o fortran.yy.c fortran.lex

clean:
	$(RM) convert.yy.c convert.tab.c convert.output convert.vcg convert.dot	\
		  fortran.yy.c fortran.tab.c fortran.output fortran.vcg fortran.dot

clean-all: clean
	$(RM) main.c fortran.c
