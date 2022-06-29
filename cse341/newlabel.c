#include <stdio.h>
#include <stdlib.h>
#include "proto.h"


 /* function newlabel returns a new label consisting of a single */
 /* character head followed by a four digit string, incremented by 1 */
 /* each time newlable is called with the same head. Function is */
 /* called with a pointer to a char array [6] to hold label and the */
 /* head char.  Function creates new label, storing it in the char */
 /* array and returning a pointer to the array */ 

char *
newlabel(char head)
{
  static unsigned tails [128];	/* one for each ASCII char, last tail */
  register char *p;	        /* save pointer to label to return */
  char *label;

  if ((label = malloc(sizeof(char) * 6)) == NULL)
    {
      fprintf(stderr,"Error allocating memory in newlabel()\n");
      exit(1);
    }
  
  p = label;

  *label++ = head;		/* start with head char */
  sprintf(label, "%04d", ++tails[head]); /* create new tail */
      label[5] = '\0';		/* add null */
      return p;			/* points to label */
}

