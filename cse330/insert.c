#include <stdio.h>
#include <stdlib.h>
#include "btree.h"
#include "proto.h"

void additem(struct btree_type *ptr, int data);
struct btree_type *find_location(struct btree_type *T, int key);
void split(struct btree_type *n);
struct btree_type *mknode(void);

int insert(int data)
{
  struct btree_type *ptr;

  if (tree_head == (struct btree_type *)NULL)
    {
      tree_head = ptr = mknode();
    }

  ptr = find_location(tree_head, data);

  if (ptr == NULL)
    return 0;
  
  additem(ptr, data);
  comps++;

  if ((ptr->leftdata != NOVALUE) && (ptr->rightdata != NOVALUE) && (ptr->middledata != NOVALUE))
    split(ptr);
  return 1;
}

void additem(struct btree_type *ptr, int data)
{
  if (ptr->rightdata == NOVALUE)
    {
      if (ptr->leftdata == NOVALUE)
	{
	  ptr->leftdata = data;
	}
      else if (data < ptr->leftdata)
	{
	  ptr->rightdata = ptr->leftdata;
	  ptr->leftdata = data;
	}
      else
	{
	  ptr->rightdata = data;
	}
    }
  else
    {
      if (data < ptr->leftdata)
	{
	  ptr->middledata = ptr->leftdata;
	  ptr->leftdata = data;
	}
      else if ((data > ptr->leftdata) && (data < ptr->rightdata))
	{
	  ptr->middledata = data;
	}
      else
	{
	  ptr->middledata = ptr->rightdata;
	  ptr->rightdata = data;
	}
    }
}

struct btree_type *find_location(struct btree_type *T, int key)
{
  struct btree_type *ptr;

  ptr = T;

  if (ptr == NULL)
    return ptr;

  if ((key == ptr->leftdata) || (key == ptr->rightdata))
    {
      comps+=2;
      return NULL;
    }

  else if ((ptr->left == NULL) && (ptr->middle == NULL) && (ptr->right == NULL))
    {
      return ptr;
    }

  else if (ptr->rightdata != NOVALUE)
    {
      if (key < ptr->leftdata)
	{
	  comps++;
	  return find_location(ptr->left, key);
	}
      else if ((key > ptr->leftdata) && (key < ptr->rightdata))
	{
	  comps+=2;
	  return find_location(ptr->middle, key);
	}      
      else
	{
	  comps+=2;
	  return find_location(ptr->right, key);
	}
    }
  
  else
    {
      if (key < ptr->leftdata)
	{
	  comps++;
	  return find_location(ptr->left, key);
	}
      if (key > ptr->leftdata)
	{
	  comps+=2;
	  return find_location(ptr->middle, key);
	}
    }
}

void split(struct btree_type *n)
{
  struct btree_type *p, *n1, *n2;

  comps++;

  p = n->parent;

  if (p == NULL)
    {
      p = mknode();
      tree_head = p;
    }

  n1 = mknode();
  n2 = mknode();

  n1->leftdata = n->leftdata;
  n2->leftdata = n->rightdata;

  n1->parent = p; /* n->parent */
  n2->parent = p;

  additem(p, n->middledata);

  if (p->rightdata == NOVALUE)
    {
      p->left = n1;
      p->middle = n2;
    }
  else if (p->middledata == NOVALUE)
    {
      if (n1->leftdata < p->leftdata)
	{
	  p->left = n1;
	  p->right = p->middle;
	  p->middle = n2;
	}
      else if ((n1->leftdata > p->leftdata) && (n1->leftdata < p->rightdata))
	{
	  p->middle = n1;
	  p->right = n2;
	}
      else 
	printf("Never should have gotten here\n");
    }
  else if (n1->leftdata < p->leftdata)
    {
      p->left = n1;
      p->middleright = p->middle;
      p->middle = n2;
    }
  else if ((n1->leftdata > p->leftdata) && (n1->leftdata < p->middledata))
    {
      p->middle = n1;
      p->middleright = n2;
    }
  else if ((n1->leftdata > p->middledata) && (n1->leftdata < p->rightdata))
    {
      p->middleright = n1;
      p->right = n2;
    }
  else
    {
      printf("Never shoulda gotten here!\n");
    }

  if ((n->left != NULL) || (n->middle != NULL) || (n->right != NULL)) /* is internal node */
    {
      n1->left = n->left;
      n->left->parent = n1;

      n1->middle = n->middle;
      n->middle->parent = n1;

      n2->left = n->middleright;
      n->middleright->parent = n2;

      n2->middle = n->right;
      n->right->parent = n2;
    }
  
/*
  if (p->middleright != NULL)
    {
      p->middle = p->middleright;
      p->middleright = NULL;
    }
*/

  if ((p->leftdata != NOVALUE) && (p->rightdata != NOVALUE) && (p->middledata != NOVALUE))
      split(p);

  /* free(n) */

  return;
}



