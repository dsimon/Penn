#include <stdio.h>
#include <stdlib.h>

int fib(int);

main()
{
  int i, j;

  i = 6;
  j = fib(i);
  printf("The result is: %d\n",j);
}
