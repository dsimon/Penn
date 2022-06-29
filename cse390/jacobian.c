#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define L3      1.8
#define L2      1.5
#define L1      2.0

struct jacobian 
{
  float matrix[6][3];
};

struct jacobian *jacobian_t(float angle1, float angle2, float angle3)
{
  struct jacobian *J;

  J = (struct jacobian *) malloc(sizeof( struct jacobian ));
  
  if (J == NULL)
    {
      fprintf(stderr,"Not enough memory\n");
      exit(1);
    }

  J->matrix[0][0] = 0.0;
  J->matrix[1][0] = -(cos(angle2) * sin(angle3) * L3) - (cos(angle2) * L2) - (sin(angle2) * cos(angle3) * L3);
  J->matrix[2][0] = 0.0;
  J->matrix[3][0] = (sin(angle2) * sin(angle3)) + (cos(angle2) * cos(angle3));
  J->matrix[4][0] = 0.0;
  J->matrix[5][0] = (sin(angle2) * sin(angle3)) - (cos(angle2) * cos(angle3));

  J->matrix[0][1] = (sin(angle3) * sin(angle3) * L3) + (sin(angle3) * L2) + (cos(angle3) * cos(angle3) * L3);
  J->matrix[1][1] = 0.0;
  J->matrix[2][1] = -(cos(angle3) * sin(angle3) * L3) - (cos(angle3) * L2);
  J->matrix[3][1] = 0.0;
  J->matrix[4][1] = 1.0;
  J->matrix[5][1] = 0.0;

  J->matrix[0][2] = L3;
  J->matrix[1][2] = 0.0;
  J->matrix[2][2] = 0.0;
  J->matrix[3][2] = 0.0;
  J->matrix[4][2] = 1.0;
  J->matrix[5][2] = 0.0;

  return J;
}

