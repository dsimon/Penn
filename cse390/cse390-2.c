/* CSE390 Manipulation Assignment #2 */
/* Derron Simon */

#include <stdio.h>
#include <stdlib.h>
#include <float.h>

struct transform {
  double matrix[4][4];
} t;

struct transform * trans_mul(struct transform *T1, struct transform *T2)
{
  struct transform *TEMP;
  int i,j,x,y;

  if ((TEMP = (struct transform *)malloc(sizeof(struct transform))) == NULL) {
    fprintf(stderr,"Not enough memory\n");
    exit(1);
  }

  for (i=0; i < 4; i++)
    for (j=0; j < 4; j++)
      TEMP->matrix[i][j] = 0.0;

  for (i=0; i < 4; i++) 
    for (j=0; j < 4; j++) 
      for (x=0; x < 4; x++)
	TEMP->matrix[i][j] += (T1->matrix[x][j] * T2->matrix[i][x]);

  return TEMP;
}

void trans_print(struct transform *T)
{
  int i,j;

  for (i=0; i < 4; i++)
    {
      for (j=0; j < 4; j++)
	printf("\t%f",T->matrix[i][j]);
      printf("\n");
    }
}

struct transform *trans_inv(struct transform *T1)
{
  struct transform *TEMP;
  int i,j,x;

  if ((TEMP = (struct transform *)malloc(sizeof(struct transform))) == NULL) 
    {
      fprintf(stderr,"Not enough memory\n");
      exit(1);
    }

  for (i=0; i < 4; i++)
    for (j=0; j < 4; j++)
      TEMP->matrix[i][j] = 0.0;

  /* since the inverse of a 3x3 transform is it's transpose, I'll */
  /* transpose the 3x3 matrix */

  for (i=0; i < 3; i++) 
    for (j=0; j < 3; j++)
      TEMP->matrix[i][j] = T1->matrix[j][i];

  /* now I need to calculate the new origin */

  for (x=0; x < 3; x++)
    for (i=0; i < 3; i++)
      TEMP->matrix[x][3] += T1->matrix[i][3] * T1->matrix[i][x];

  for (x=0; x < 3; x++)
    TEMP->matrix[x][3] = -TEMP->matrix[x][3];


  /* set up bottom row of homogenous transform */

  TEMP->matrix[3][0] = 0.0;
  TEMP->matrix[3][1] = 0.0;
  TEMP->matrix[3][2] = 0.0;
  TEMP->matrix[3][3] = 1.0;

  return TEMP;
}


main()
{
  struct transform *T, *T2, *OUTT;
  int i,j;

  if ((T = (struct transform *)malloc(sizeof(struct transform))) == NULL) {
    fprintf(stderr,"Not enough memory\n");
    exit(1);
  }

  if ((T2 = (struct transform *)malloc(sizeof(struct transform))) == NULL) {
    fprintf(stderr,"Not enough memory\n");
    exit(1);
  }

  for (i=0; i < 4; i++)
    for (j=0; j < 4; j++)
      {
	T->matrix[i][j] = 0.0;
	T2->matrix[i][j] = 0.0;
      }

  T->matrix[0][0] = 1.0;
  T->matrix[1][1] = 1.0;
  T->matrix[2][2] = 1.0;
  T->matrix[3][3] = 1.0;

  T2->matrix[0][0] = 0.866;
  T2->matrix[0][1] = -0.5;
  T2->matrix[0][2] = 0.0;
  T2->matrix[0][3] = 4.0;

  T2->matrix[1][0] = 0.5;
  T2->matrix[1][1] = 0.866;
  T2->matrix[1][2] = 0.0;
  T2->matrix[1][3] = 3.0;
  
  T2->matrix[2][0] = 0.0;
  T2->matrix[2][1] = 0.0;
  T2->matrix[2][2] = 1.0;
  T2->matrix[2][3] = 0.0;

  T2->matrix[3][0] = 0.0;
  T2->matrix[3][1] = 0.0;
  T2->matrix[3][2] = 0.0;
  T2->matrix[3][3] = 1.0;

  printf("T:\n");
  trans_print(T);
  printf("T2:\n");
  trans_print(T2);

  OUTT = trans_mul(T,T2);

  printf("T * T2:\n");
  trans_print(OUTT);
  
  free(OUTT);

  OUTT = trans_inv(T2);

  printf("INV(T2):\n");
  trans_print(OUTT);

}





