/* Hacked up quick sort, from 2nd ed. K&R, page 87. */

float left;	/* Should be hidden by parameter left       */
char i;		/* Should be hidden by internal declaration */
int ext,ext2;	/* Should not be hidden by anything         */
int dummy(void);
void swap(int *v, int i, int j);

void qsort(int *v,int left,int right)
  {
    register int i,last;
    int new_partition_element;
    int *ext_ptr;
    int everything_a_ok, bad_news;
    float array[64];

    if (left >= right)
      return;

/*debug(token_listing_on) */
    new_partition_element = (left+right)/2;
/*debug(token_listing_off) */
    swap(v,left,new_partition_element);
    last = left;
    for (i=left+1;i<=right;i++)
      if (v[i] < v[left])
	swap(v,++last,i);
    swap(v,left,last);
    qsort(v,left,last-1);
    qsort(v,last+1,right);

    return;

    /* Some hokey statements: */

    ext = 34;
    while(--ext)
      { /* do nothing */
      }   

    ext_ptr = &ext;
    *ext_ptr = 34;
    do {
         *ext_ptr--;
       } while(ext_ptr);

    everything_a_ok = bad_news = 0;
    if (ext == ext_ptr[0])
      everything_a_ok = 1;
    else
      bad_news = 1;

    if (5*(bad_news > 9-13-64+68))
      return dummy();
/*debug(symtab_dump)*/
  }
/*debug(variable_dump)*/
/*debug(statement_dump)*/
