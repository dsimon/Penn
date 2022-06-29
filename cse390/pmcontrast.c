/* pmcontrast.c by derron simon for cse390 */

#include <stdio.h>
#include <math.h>
#include </usr/local/include/pm.h>

#define OPTIONS "o:"

extern int optind;
extern char *optarg;

void contrast(FILE *in, FILE *out);

main(int argc, char **argv)
{
  FILE *in=stdin,*out=stdout;
  int errflag=0, c;
  char *ofile=NULL,*ifile=NULL;

  while ((c = getopt(argc, argv, OPTIONS)) != EOF)
    switch (c)
      {
      case 'o': if ((out = fopen(optarg, "w")) == NULL)
	exit(-1);
	break;
      case '?': 
	++errflag;
	break;
      }
  if (optind) if ((in = fopen(argv[optind],"r")) == NULL)
    exit(-1);
  if (errflag) 
    exit(-1);

  contrast(in, out);

  fclose(in);
  fclose(out);
  exit(0);
}

void contrast(FILE *in, FILE *out)
{
  pmpic *pm1, *pm2;
  int i,j,ncol;
  int max, min;
  float inc;

  pm1 = pm_read(in,pm1);
  pm2 = pm_prep(pm1,pm1);
  
  max = 0;
  min = 255;

  for (i=0; i < pm1->pm_nrow; i++)
    for (j=0; j < (ncol = pm1->pm_ncol); j++)
      {
	if (*(pm1->pm_image + i*ncol + j) > max)
	  max = *(pm1->pm_image + i*ncol + j);
	if (*(pm1->pm_image + i*ncol + j) < min)
	  min = *(pm1->pm_image + i*ncol + j);
      }

  inc = (float) (256/(max - min));

  for (i=0; i < pm1->pm_nrow; i++)
    for (j=0; j < (ncol = pm1->pm_ncol); j++)
      {
	*(pm2->pm_image + i*ncol + j) = nint((float)(*(pm1->pm_image + i*ncol + j) - min) * inc);
      }

  pm_write(out,pm2);
  pm_free(pm1);
  pm_free(pm2);
}

