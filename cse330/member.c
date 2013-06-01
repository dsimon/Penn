#include <stdio.h>
#include <stdlib.h>
#include "btree.h"
#include "proto.h"

int lookup(struct btree_type *ptr, int key);

int member(int key)
{
  return( lookup(tree_head, key) );
}

int lookup(struct btree_type *ptr, int key)
{
  if (ptr == NULL)
    return FALSE;

  if ((key == ptr->leftdata) || (key == ptr->rightdata))
    {
      comps+=2;
      return TRUE;
    }

  else if ((ptr->left == NULL) && (ptr->middle == NULL) && (ptr->right == NULL))
    {
      if ((ptr->leftdata == key) || (ptr->rightdata == key))
	{
	  comps+=2;
	  return TRUE;
	}
      comps+=2;
      return FALSE;
    }

  else if (ptr->rightdata != NOVALUE)
    {
      if (key < ptr->leftdata)
	{
	  comps++;
	  return lookup(ptr->left, key);
	}
      else if ((key > ptr->leftdata) && (key < ptr->rightdata))
	{
	  comps+=2;
	  return lookup(ptr->middle, key);
	}      
      else
	{
	  comps+=2;
	  return lookup(ptr->right, key);
	}    
    }
  else
    {
      if (key < ptr->leftdata)
	{
	  comps++;
	  return lookup(ptr->left, key);
	}
      if (key > ptr->leftdata)
	{
	  comps+=2;
	  return lookup(ptr->middle, key);
	}
    }
}

