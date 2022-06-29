/* pmthresh.c by derron simon for cse390 */

#include <stdio.h>
#include <math.h>
#include </usr/local/include/pm.h>

#define OPTIONS "o:t:"

extern int optind;
extern char *optarg;

void thresh(FILE *in, FILE *out, unsigned char t);

main(int argc, char **argv)
{
  FILE *in=stdin,*out=stdout;
  int errflag=0, tflag=0, c;
  char *ofile=NULL,*ifile=NULL;

  while ((c = getopt(argc, argv, OPTIONS)) != EOF)
    switch (c)
      {
      case 'o': if ((out = fopen(optarg, "w")) == NULL)
	exit(-1);
	break;
      case 't':
	tflag = atoi(optarg);
	break;
      case '?': 
	++errflag;
	break;
      }
  if (optind) if ((in = fopen(argv[optind],"r")) == NULL)
    exit(-1);
  if (errflag) 
    exit(-1);

  thresh(in, out, tflag);

  fclose(in);
  fclose(out);
  exit(0);
}

void thresh(FILE *in, FILE *out, unsigned char t)
{
  pmpic *pm1, *pm2;
  int i,j,ncol;

  pm1 = pm_read(in,pm1);
  pm2=pm_prep(pm1,pm1);
  
  for (i=0; i < pm1->pm_nrow; i++)
    for (j=0; j < (ncol = pm1->pm_ncol); j++)
      if ((unsigned char)*(pm1->pm_image + i*ncol + j) > (unsigned char) t)
	*(pm2->pm_image + i*ncol + j) = (unsigned char) 255;
      else
	*(pm2->pm_image + i*ncol + j) = (unsigned char) 0;
  pm_write(out,pm2);
  pm_free(pm1);
  pm_free(pm2);
}

