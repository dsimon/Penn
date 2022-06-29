int fib(int num)
{
  int i,j,k,l;

  k = 1;
  l = 1;
  j = 2;
  if (num < 2) return(1);

  for(num = num - 2; num > 0; num = num - 1)
    {
      k = l;
      l = j;
      j = k + l;
    }
  return(j);
}
