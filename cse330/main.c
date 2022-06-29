#include <stdio.h>
#include <stdlib.h>
#define MAIN 1
#include "btree.h"

FILE *in,*runlog;
int run = 0;
int ops = 0;

void printstats( void );

void done(void)
{
  fflush(runlog);
  fflush(stdout);
  fclose(runlog);
}

main(int argc, char **argv)
{
  char c;
  int data;
  char strbuf[4];
  int finished;

  tree_head = (struct btree_type *) NULL;

  if (argc < 2) 
    {
      fprintf(stderr,"Usage: bplus datafile\n");
      exit(1);
    }

  if ((in = fopen(argv[1],"r")) == NULL)
    {
      fprintf(stderr,"Cannot open file %s\n",argv[1]);
      exit(1);
    }

  if ((runlog = fopen("run.log","w")) == NULL)
    {
      fprintf(stderr,"Cannot create file 'run.log'\n");
      exit(1);
    }

  ops = 0;

  finished = 0;

  while (!finished)
    {
      c = fgetc(in);

      switch (c)
	{
	case 'I':
	  ops++;
	  fscanf(in,"%d",&data);
	  printf("%c",(insert(data)?'y':'n'));
	  break;
	case 'M':
	  ops++;
	  fscanf(in,"%d",&data);
	  printf("%c",(member(data)?'y':'n'));
	  break;
	case 'D':
	  ops++;
	  fscanf(in,"%d",&data);
	  printf("%c",(delete(data)?'y':'n'));
	  break;
	case ' ':
	  break;
	case '\n':
	  c = fgetc(in);
	  if (c != '\n')
	    {
	      printf("Error, only 1 newline found!");
	      exit(-1);
	    }
	  printf("\n\n");
	  printstats();
	  comps = 0;
	  ops = 0;
	  tree_head = NULL;
	  break;
	case EOF:
	  printf("\n\nFinished input\n");
	  done();
	  finished = 1;
	  break;
	default:
	  printf("What is this? '%c'\n",c);
	  break;
	}
    }
}

void printstats( void )
{
  run++;

  fprintf( runlog, "run: %d\toperations: %d\tcomparisons: %d\n", run, ops, comps );
  fflush( runlog );
}







