#include <stdio.h>
#include <stdlib.h>
#include "btree.h"


struct btree_type *mknode(void)
{
  struct btree_type *ptr;

  if ((ptr = (struct btree_type *)malloc(sizeof(struct btree_type))) == NULL)
    {
      fprintf(stderr,"Error allocating memory\n");
      exit(1);
    }

  ptr->left = NULL;
  ptr->right = NULL;
  ptr->middle = NULL;
  ptr->middleright = NULL;
  ptr->parent = NULL;

  ptr->leftdata = NOVALUE;
  ptr->middledata = NOVALUE;
  ptr->rightdata = NOVALUE;

  return ptr;
}

