int *malloc(int i);

int test(void)
{
  int a[10], *b, *c;

  b = malloc(20);
  *b = 0;

  return(b);
}

