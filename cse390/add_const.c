/*****************************************************************************/
/*									     */
/* NAME: add_const							     */
/*									     */
/* USAGE: add_const [-o outfile] -t int [infile]			     */
/*									     */
/* FUNCTION: adds a constant, int, to each pixel of a PM image		     */
/*									     */
/* COMPILE: cc -O -o add_const -I/usr/local/include -I/pkg/include           */
/*	       add_const.c -lpm				     		     */
/*									     */
/*****************************************************************************/

#include <stdio.h>
#include <pm.h>

extern int optind;
extern char *optarg;
#define OPTIONS "o:t:"		/* : after character means flag has an
				   argument */
#define NIF 1			/* maximal number of input files */
char *cmd, *cmt, *usage = "[-o outfile] -t int [infile]";

/*---------------------------------------------------------------------------*/

main(argc, argv)
     int argc;
     char **argv;
{
  FILE *ifs = stdin, *ofs = stdout; /* file streams, default stdio */
  int errflag = 0, tflag = 0, oflag = 0, tvalue;
  char c, *ofile = NULL, *ifile = NULL; /* output and input file names */

  cmd = argv[0];		/* save name of program */
  cmt = pm_cmt(argc, argv);	/* save command line for later comments */

  /* parse command line */
  while ((c = getopt(argc, argv, OPTIONS)) != EOF)
    switch (c) {
    case 'o':
      if (!oflag) {
	if ((ofs = fopen(optarg, "w")) == NULL) {
	  fprintf(stderr, "%s: Cannot open output file\n", cmd);
	  exit(1);
	}
	oflag = 1;
      }
      else
	errflag = 1;
      break;
    case 't':
      if (!tflag) {
	tvalue = atoi(optarg);
	tflag = 1;
      }
      else
	errflag = 1;
      break;
    case '?':
      errflag = 1;
      break;
    }

  /* check for error in command line */
  if (errflag			/* error in options */
      || !tflag			/* -t not specified */
      || argc > optind + NIF) {	/* too many input files */
    fprintf(stderr, "Usage: %s %s\n", cmd, usage);
    exit(1);
  }

  /* get input file */
  if (optind == argc - NIF)	/* one input file argument */
    if ((ifs = fopen(argv[optind], "r")) == NULL) {
      fprintf(stderr, "%s: Cannot open input file\n", cmd);
      exit(1);
    }

  /* do the image processing */
  do_it(ifs, ofs, tvalue);

  /* clean up */
  fclose(ifs);
  fclose(ofs);
}

/*---------------------------------------------------------------------------*/

do_it(ifs, ofs, t)		/* the function that manipulates the pics */
     FILE *ifs, *ofs;
     int t;
{
  pmpic *pm1, *pm2;		/* Let there be pm's */
  int i, j, nrow, ncol;

  pm1 = pm_read(ifs, NULL);	/* read in image */
  pm2 = pm_prep(pm1, pm1);	/* allocate enough space for output image */

  nrow = pm1->pm_nrow;
  ncol = pm1->pm_ncol;

  for (i = 0; i < nrow; i++)	/* step through image */
    for (j = 0; j < ncol; j++)
      pm2->pm_image[i * ncol + j] = pm1->pm_image[i * ncol + j] + t;

  pm_addcmt(pm2, pm1->pm_cmt);	/* copy comments of input image */
  pm_addcmt(pm2, cmt);		/* add command line as a comment */
  pm_write(ofs, pm2);		/* write out output */

  pm_free(pm1);			/* free images */
  pm_free(pm2);
}

/*****************************************************************************/
