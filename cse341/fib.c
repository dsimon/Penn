int fib(int num)
{
  int i,j,k,l;

  j = 1;
  i = num;
  if (i < 2) return (j);
  k = fib(i - 1);
  l = fib(i - 2);
  return(k + l);
}
