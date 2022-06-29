/*debug(trace_types_on)*/

float f(float x);
float f_prime(float x);
int dprint(char *s,float d);

float newton(float start)
  {
    float x[100];
    int i;

    i = 0;
    x[i] = start;
    for(i=1;i<100;i++)
      { 
	x[i] = x[i-1] - f(x[i-1])/f_prime(x[i-1]);
      }

    return x[i-1];
  }
/*debug(trace_types_off)*/


