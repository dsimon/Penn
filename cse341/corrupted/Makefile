CC = gcc
PROGNAME = ddcc
SRCS =  main.c symtab.c y.y lexical.l hash.c mknode.c newlabel.c check2.c register.c codegen.c expression.c if.c
OBJS =  main.o symtab.o lex.yy.o hash.o y.tab.o mknode.o newlabel.o check2.o register.o codegen.o expression.o if.o
CFLAGS = -g -DDEBUG
CLIBS = -lm

all: $(PROGNAME)

y.tab.h: y.y
	bison -d y.y

lex.yy.o: lexical.l y.tab.h
	flex lexical.l
	gcc -g -c $(CFLAGS) lex.yy.c

y.tab.c: y.y
	bison y.y

y.tab.o: y.y
	bison y.y
	gcc -g -c $(CFLAGS) y.tab.c

check.o:
	gcc -g -c $(CFLAGS) check2.c

$(PROGNAME): $(OBJS) main.o
	$(CC) $(CFLAGS) -DINIT -o $(PROGNAME) $(OBJS) $(CLIBS)

clean:
	rm -f core *.o

### Dependencies

main.o: globals.h proto.h main.c
symtab.o: globals.h symtab.h y.tab.h symtab.c 
lex.yy.o: globals.h proto.h y.tab.h lexical.l check2.o
mknode.o: globals.h y.tab.h symtab.h mknode.c
check.o: globals.h proto.h y.tab.h symtab.h check.h check2.c
expression.o: globals.h proto.h expression.h symtab.h y.tab.h
if.o: globals.h proto.h symtab.h expression.h y.tab.h



