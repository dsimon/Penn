#include <stdio.h>
#include <stdlib.h>
#include "btree.h"

struct btree_type *locate(struct btree_type *T, int key);
void fix(struct btree_type *n);
void delete_item(struct btree_type *ptr, int data);

int delete(int data)
{
  struct btree_type *ptr,*top;
  int tempval;

  ptr = locate(tree_head, data);

  if (ptr == NULL)
    return 0;
  
  top = ptr;

  if ((ptr->left == NULL) && (ptr->middle == NULL) && (ptr->right == NULL))
    delete_item(ptr, data);
  else
    {
      if (data == ptr->leftdata)
	ptr = ptr->middle;
      else if (data == ptr->rightdata)
	ptr = ptr->right;
      else
	fprintf(stderr,"data is not equal to left or right!\n");

      while ((ptr->left != NULL) || (ptr->middle != NULL) || (ptr->right != NULL))
	ptr = ptr->left;

      if (data == top->leftdata)
	top->leftdata = ptr->leftdata;
      else 
	top->rightdata = ptr->leftdata;

      ptr->leftdata = ptr->rightdata;
      ptr->rightdata = NOVALUE;
    }
  
  if ((ptr->leftdata == NOVALUE) && (ptr->rightdata == NOVALUE))
    fix(ptr);
  return 1;
}
  
void fix(struct btree_type *n)
{
  struct btree_type *p,*s;

  comps++;

  p = n->parent;
  
  if (p == NULL)
    {
      if (n->left != NULL)
	tree_head = n->left;
      else if (n->middle != NULL)
	tree_head = n->middle;
      if (n->right != NULL)
	tree_head = n->right;
      
      tree_head->parent = NULL;
      
      /* free(n); */ 
      
      return;
    }

  if (p->left->leftdata == NOVALUE)
    {
      if (p->middle->rightdata != NOVALUE)
	{	
	  p->left->leftdata = p->leftdata;
	  p->leftdata = p->middle->leftdata;
	  p->middle->leftdata = p->middle->rightdata;

	  p->left->middle = p->middle->left;
	  p->middle->left = p->middle->middle;
	  p->middle->middle = p->middle->right;
	  p->middle->right = NULL;

	  if (p->left->middle != NULL)
	    p->left->middle->parent = p->left;

	  p->middle->rightdata = NOVALUE;
	}
      else if (p->rightdata != NOVALUE)
	{
	  if (p->right->rightdata != NOVALUE)
	    {
	      p->left->leftdata = p->leftdata;
	      p->leftdata = p->middle->leftdata;
	      p->middle->leftdata = p->rightdata;
	      p->rightdata = p->right->leftdata;
	      p->right->leftdata = p->right->rightdata;

	      p->left->middle = p->middle->left;
	      p->middle->left = p->middle->middle;
	      p->middle->middle = p->right->left;
	      p->right->left = p->right->middle;
	      p->right->middle = p->right->right;
	      p->right->right = NULL;

	      if (p->left->middle != NULL)
		p->left->middle->parent = p->left;
	      if (p->middle->middle != NULL)
		p->middle->middle->parent = p->middle;

	      p->right->rightdata = NOVALUE;
	    }
	  else goto merge_node;
	}
      else
	goto merge_node;
    }
  else if (p->middle->leftdata == NOVALUE)
    {
      if (p->left->rightdata != NOVALUE)
	{
	  p->middle->leftdata = p->leftdata;
	  p->leftdata = p->left->rightdata;
	  p->left->rightdata = NOVALUE;

	  p->middle->middle = p->middle->left;
	  p->middle->left = p->left->right;

	  if (p->middle->left != NULL)
	    p->middle->left->parent = p->middle;

	  p->left->right = NULL;
	}
      else if (p->rightdata != NOVALUE)
	{
	  if (p->right->rightdata != NOVALUE)
	    {
	      p->middle->leftdata = p->rightdata;
	      p->rightdata = p->right->leftdata;
	      p->right->leftdata = p->right->rightdata;
	      
	      p->middle->middle = p->right->left;
	      p->right->left = p->right->middle;
	      p->right->middle = p->right->right;
	      p->right->right = NULL;

	      if (p->middle->middle != NULL)
		p->middle->middle->parent = p->middle;

	      p->right->rightdata = NOVALUE;
	    }
	  else goto merge_node;
	}
      else
	goto merge_node;
    }
  else if (p->rightdata != NOVALUE)
    {
      if (p->right->leftdata == NOVALUE)
	{
	  if (p->middle->rightdata != NOVALUE)
	    {
	      p->right->leftdata = p->rightdata;
	      p->rightdata = p->middle->rightdata;
	      p->middle->rightdata = NOVALUE;

	      p->right->middle = p->right->left;
	      p->right->left = p->middle->right;
	      p->middle->right = NULL;

	      if (p->right->left != NULL)
		p->right->left->parent = p->right;
	    }
	  else if (p->left->rightdata != NOVALUE)
	    {
	      p->right->leftdata = p->rightdata;
	      p->rightdata = p->middle->leftdata;
	      p->middle->leftdata = p->leftdata;
	      p->leftdata = p->left->rightdata;
	      p->left->rightdata = NOVALUE;

	      p->right->middle = p->right->left;
	      p->right->left = p->middle->middle;
	      p->middle->middle = p->middle->left;
	      p->middle->left = p->left->right;
	      p->left->right = NULL;

	      if (p->right->left != NULL)
		p->right->left->parent = p->right;
	      if (p->middle->left != NULL) /***/
		p->middle->left->parent = p->middle;
	    }
	  else
	    goto merge_node;
	}
    }
  else
    {
merge_node:
      if (p->left->leftdata == NOVALUE)
	{
	  if (p->middle->leftdata != NOVALUE)
	    {
	      p->middle->rightdata = p->middle->leftdata;
	      p->middle->leftdata = p->leftdata;
	      p->leftdata = p->rightdata;
	      p->rightdata = NOVALUE;

	      p->middle->right = p->middle->middle;     /* move children of S around */
	      p->middle->middle = p->middle->left;
	      p->middle->left = n->left;

	      if (p->middle->left != NULL)
		p->middle->left->parent = p->middle;

	      p->left = p->middle;
	      p->middle = p->right;
	      p->right = NULL;
	    }
	}
      else if (p->middle->leftdata == NOVALUE)
	{
	  if (p->rightdata != NOVALUE)
	    {
	      p->right->rightdata = p->right->leftdata;
	      p->right->leftdata = p->rightdata;
	      p->rightdata = NOVALUE;

	      p->right->right = p->right->middle;
	      p->right->middle = p->right->left;
	      p->right->left = n->left;

	      if (p->right->left != NULL)
		p->right->left->parent = p->right;

	      p->middle = p->right;
	      p->right = NULL;
	    }
	  else
	    {
	      p->left->rightdata = p->leftdata;
	      p->leftdata = NOVALUE;

	      p->left->right = n->left;
	      
	      if (p->left->right != NULL)
		p->left->right->parent = p->left;

	      p->middle = NULL;
	    }
	}
      else if (p->right->leftdata == NOVALUE)
	{
	  p->middle->rightdata = p->rightdata;
	  p->rightdata = NOVALUE;

	  p->middle->right = n->left;

	  if (p->middle->right != NULL)
	    p->middle->right->parent = p->middle;

	  p->right = NULL;
	}
      
      /* free(n); */

      if ((p->leftdata == NOVALUE) && (p->rightdata == NOVALUE))
	fix(p);
    }
}

struct btree_type *locate(struct btree_type *T, int key)
{
  struct btree_type *ptr;

  ptr = T;

  if (ptr == NULL)
    return NULL;

  if ((key == ptr->leftdata) || (key == ptr->rightdata))
    {
      comps+=2;
      return ptr;
    }

  else if ((ptr->left == NULL) && (ptr->middle == NULL) && (ptr->right == NULL))
    {
      comps+=2;
      if ((ptr->leftdata == key) || (ptr->rightdata == key))
	return ptr;
      return NULL;
    }

  else if (ptr->rightdata != NOVALUE)
    {
      if (key < ptr->leftdata)
	{
	  comps++;
	  return locate(ptr->left, key);
	}
      else if ((key > ptr->leftdata) && (key < ptr->rightdata))
	{	
	  comps+=2;
	  return locate(ptr->middle, key);
	}
      else
	{
	  comps+=2;
	  return locate(ptr->right, key);
	}
    }
  
  else
    {
      if (key < ptr->leftdata)
	{
	  comps++;
	  return locate(ptr->left, key);
	}
      if (key > ptr->leftdata)
	{
	  comps+=2;
	  return locate(ptr->middle, key);
	}
    }
}

void delete_item(struct btree_type *ptr, int data)
{
  if (ptr->rightdata == NOVALUE)
    {
      if (data == ptr->leftdata)
	ptr->leftdata = NOVALUE;
      else
	fprintf(stderr,"got to delete_item but never found data!\n");
    }
  else
    {
      if (data == ptr->leftdata)
	{
	  ptr->leftdata = ptr->rightdata;
	  ptr->rightdata = NOVALUE;
	}
      else
	{
	  ptr->rightdata = NOVALUE;
	}
    }
}
	  





