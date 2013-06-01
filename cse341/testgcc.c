int i;

int test(void)
{
  int j;

  j = i;

  return j;

  i = 0x7fffffff;

  printf("Hello world\n");

  return i;
}
