/* Quick sort, from 2nd ed. K&R, page 87. */

/*debug(trace_types_on)*/

void swap(int *v, int i, int j);

void qsort(int *v,int left,int right)
  {
    int i,last;
    int new_partition_element;

    if (left >= right)
      return;

    new_partition_element = (left+right)/2;
    swap(v,left,new_partition_element);
    last = left;
    for (i=left+1;i<=right;i++)
      if (v[i] < v[left])
	swap(v,++last,i);
    swap(v,left,last);
    qsort(v,left,last-1);
    qsort(v,last+1,right);
  }
     
/*debug(trace_types_off)*/


