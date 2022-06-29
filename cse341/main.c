#include <stdio.h>
#include "globals.h"
#include "proto.h"
#include "y.tab.h"
#include "symtab.h"

extern FILE *yyin;
extern char *yytext;
extern FILE *yyout;

main(int argc, char **argv)
{
  unsigned int i;

  scope = 0;
  for(i=0; i <= PRIME; i++)
    bucket[i] = (struct symtab_type *)NULL;

  fprintf(stderr,"Doug & Derron's C Compiler, copyright (c) 1993 Doug Bellew & Derron Simon\n");
  if (argc > 1)
    {
      if ((yyin = fopen(argv[1],"r")) == NULL) {
	fprintf(stderr,"Can't open file %s\n",argv[1]);
	exit(1);
      }
    }

  init_allocator();

  if (yyparse())
    {
      fprintf(stderr,"CS Parse failed!\n");
      exit(1);
    }

  make_code();
  exit(0);
}


