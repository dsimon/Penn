#include <stdio.h>
#include "proto.h"
#include "globals.h"
#include "symtab.h"
#include "y.tab.h"
#include "check.h"

void overchk(void)
{
  int i,j;
  struct symtab_type *p, *q;


  for (i = 0; i <= PRIME; i++) 
    {
      p = bucket[i];
      while (p)
	{
	  if ((p->symbol_type == SYM_VARIABLE) ||
	      (p->symbol_type == SYM_FUNCTION))
	    {
	      for (j = i; j <= PRIME; j++) 
		{
		  q = bucket[j];
		  if (j == i)
		    q = p->next;
		  while (q)
		    {
		      if ((q->symbol_type == SYM_VARIABLE) ||
			  (q->symbol_type == SYM_FUNCTION))
			{
			  if ((!strcmp(p->name,q->name)) &&
			      (p->scope == q->scope))
			    fprintf(stderr, "Variable %s multiply defined in scope %d. \n",
				    p->name,p->scope);
			}
		      q = q->next;
		    }
		}
	    }
	  p = p->next;
	}
    }
}


struct return_type *parse_expression(int value,
				     struct nodetype **left,
				     struct nodetype **right)
{
  struct return_type *returning;

  returning = (struct return_type *)malloc(sizeof(struct return_type));

  if (*left == NULL)
    return(NULL);
  if (*right == NULL)
    return (NULL);

  switch (value)
    {
    case '+':
    case '-':
    case '*':
    case '/':
    case '%':
      {
	if ((*left)->ispointer)
	  if ((*right)->ispointer)
	    {
	      returning->is_pointer = 1;
	      returning->rttype = (*left)->type;
	    }
	  else
	    if ((*right)->type == INT)
	      {
		returning->is_pointer = 1;
		returning->rttype = (*left)->type;
	      }
	    else
	      return(NULL);
	else{
	  returning->is_pointer = 0;
	  switch((*left)->type)
	    {
	    case INT:
	      switch((*right)->type)
		{
		case INT: {returning->rttype = INT;break;}
		case FLOAT: {int2float(left); returning->rttype = FLOAT;break;}
		case CHAR:  {char2int(right); returning->rttype = INT;break;}
		case POINTER: {fprintf(stderr,"Makes pointer to integer without cast\n"); returning->rttype = -1; break;}
		case VOID: {fprintf(stderr,"Improper conversion of void to integer\n");returning->rttype = -1;  break;}
		default: {fprintf(stderr,"General Type Error.\n"); returning->rttype = -1; break;}
		}
	      break;
	    case FLOAT:
	      switch((*right)->type)
		{
		case FLOAT: {returning->rttype = FLOAT; break;}
		case INT: {int2float(right); returning->rttype = FLOAT;break;}
		case CHAR:  {char2int(right); int2float(right); returning->rttype = FLOAT; break;}
		case POINTER: {fprintf(stderr,"Illegal conversion of pointer to float\n"); returning->rttype = -1; break;}
		case VOID: {fprintf(stderr,"Improper conversion of void to float\n"); returning->rttype = -1; break;}
		default: {fprintf(stderr,"General Type Error.\n"); returning->rttype = -1; break;}
		}
	      break;
	    case CHAR:
	      switch((*right)->type)
		{
		case CHAR: {returning->rttype = CHAR;break;}
		case INT: {char2int(left); returning->rttype = INT;break;}
		case FLOAT:  {char2int(left);int2float(left); returning->rttype = FLOAT;break;}
		case POINTER: {fprintf(stderr,"Illegal conversion of pointer to char\n"); returning->rttype = -1; break;}
		case VOID: {fprintf(stderr,"Improper conversion of void to char\n"); returning->rttype = -1; break;}
		default: {fprintf(stderr,"General Type Error.\n"); returning->rttype = -1; break;}
		}
	      break;
	    case VOID:
	      switch((*right)->type)
		{
		case CHAR: {fprintf(stderr,"Improper conversion of void to char\n"); returning->rttype = -1; break;}
		case INT: {fprintf(stderr,"Improper conversion of void to int\n"); returning->rttype = -1; break;}
		case FLOAT:  {fprintf(stderr,"Improper conversion of void to float\n"); returning->rttype = -1; break;}
		case POINTER: {fprintf(stderr,"Improper conversion of void to pointer\n"); returning->rttype = -1; break;}
		case VOID: {returning->rttype = -1; break;}
		default: {fprintf(stderr,"General Type Error.\n"); returning->rttype = -1; break;}
		}
	    }
	}
	break;}
    case AND_OP:
    case OR_OP:
    case EQ_OP:
    case NE_OP:
    case '<':
    case '>':
    case LE_OP:
    case GE_OP:
      {
	if ((*left)->ispointer){ 
	  if ((*right)->ispointer){
	    returning->is_pointer = 0;
	    returning->rttype = INT;
	  }
	  else
	    switch((*right)->type)
	      {
	      case INT: {fprintf(stderr,"Makes and integer a pointer without a cast.\n\n"); returning->is_pointer = 0;
			 returning->rttype = INT; break;}
	      default: {fprintf(stderr,"Illegal conversion to type POINTER. \n"); returning->is_pointer = 0;
			returning->rttype = INT; break;}
	      }
	}
	else {
	  returning->is_pointer = 0;
	  switch((*left)->type)
		  {
		  case INT:
		    switch((*right)->type)
		      {
		      case INT: {returning->rttype = INT;break;}
		      case FLOAT: {int2float(left); returning->rttype = INT;break;}
		      case CHAR:  {char2int(right); returning->rttype = INT;break;}
		      case POINTER: {fprintf(stderr,"Makes pointer to integer without cast\n"); returning->rttype = INT; break;}
		      case VOID: {fprintf(stderr,"Improper conversion of void to integer\n");returning->rttype = -1;  break;}
		      default: {fprintf(stderr,"General Type Error.\n"); returning->rttype = -1; break;}
		      }
		    break;
		  case FLOAT:
		    switch((*right)->type)
		      {
		      case FLOAT: {returning->rttype = INT; break;}
		      case INT: {int2float(right); returning->rttype = INT;break;}
		      case CHAR:  {char2int(right);int2float(right); returning->rttype = INT;break;}
		      case POINTER: {fprintf(stderr,"Illegal conversion of pointer to float\n"); returning->rttype = -1; break;}
		      case VOID: {fprintf(stderr,"Improper conversion of void to float\n"); returning->rttype = -1; break;}
		      default: {fprintf(stderr,"General Type Error.\n"); returning->rttype = -1; break;}
		      }
		    break;
		  case CHAR:
		    switch((*right)->type)
		      {
		      case CHAR: {returning->rttype = INT;break;}
		      case INT: {char2int(left); returning->rttype = INT;break;}
		      case FLOAT:  {char2int(left);int2float(left); returning->rttype = INT;break;}
		      case POINTER: {fprintf(stderr,"Illegal conversion of pointer to char\n"); returning->rttype = -1; break;}
		      case VOID: {fprintf(stderr,"Improper conversion of void to char\n"); returning->rttype = -1; break;}
		      default: {fprintf(stderr,"General Type Error.\n"); returning->rttype = -1; break;}
		      }
		    break;
		  case VOID:
		    switch((*right)->type)
		      {
		      case CHAR: {fprintf(stderr,"Improper conversion of void to char\n"); returning->rttype = -1; break;}
		      case INT: {fprintf(stderr,"Improper conversion of void to int\n"); returning->rttype = -1; break;}
		      case FLOAT:  {fprintf(stderr,"Improper conversion of void to float\n"); returning->rttype = -1; break;}
		      case POINTER: {fprintf(stderr,"Improper conversion of void to pointer\n"); returning->rttype = -1; break;}
		      case VOID: {returning->rttype = -1; break;}
		      default: {fprintf(stderr,"General Type Error.\n"); returning->rttype = -1; break;}
		      }
		  }
	      }
	      break;}
	case ASSIGNMENT:
	  {
	    if (!chklval((*left)))
	      {
		fprintf(stderr,"left hand side not an Lval.\n"); 
		returning->rttype = -1;
		returning->is_pointer = -1;
	      }
	    else
	      {
		if ((*left)->ispointer){ 
		  if ((*right)->ispointer){
		    returning->is_pointer = 0;
		     returning->rttype = INT;
		  }
		  else
		    switch((*right)->type)
		      {
		      case INT: {fprintf(stderr,"Makes and integer a pointer without a cast.\n"); returning->is_pointer = 0;
				 returning->rttype = INT; break;}
		      default: {fprintf(stderr,"Illegal conversion to type POINTER. \n"); returning->is_pointer = 0;
				returning->rttype = -1; break;}
		      }
		}
		else {
		  returning->is_pointer = 0;
		  switch((*left)->type)
		    {
		    case INT:
		      switch((*right)->type)
			{
			case INT: {returning->rttype = INT;break;}
			case FLOAT: {int2float(left); returning->rttype = FLOAT;break;}
			case CHAR:  {char2int(right); returning->rttype = INT;break;}
			case POINTER: {fprintf(stderr,"Makes pointer to integer without cast\n"); returning->rttype = INT; break;}
			case VOID: {fprintf(stderr,"Improper conversion of void to integer\n");returning->rttype = -1;  break;}
			default: {fprintf(stderr,"General Type Error.\n"); returning->rttype = -1; break;}
			}
		      break;
		    case FLOAT:
		      switch((*right)->type)
			{
			case FLOAT: {returning->rttype = FLOAT; break;}
			case INT: {int2float(right); returning->rttype = FLOAT;break;}
			case CHAR:  {char2int(right); int2float(right);returning->rttype = FLOAT;break;}
			case POINTER: {fprintf(stderr,"Illegal conversion of pointer to float\n"); returning->rttype = -1; break;}
			case VOID: {fprintf(stderr,"Improper conversion of void to float\n"); returning->rttype = -1; break;}
			default: {fprintf(stderr,"General Type Error.\n"); returning->rttype = -1; break;}
			}
		      break;
		    case CHAR:
		      switch((*right)->type)
			{
			case CHAR: {returning->rttype = CHAR;break;}
			case INT: {char2int(left); returning->rttype = INT;break;}
			case FLOAT:  {char2int(left);int2float(left); returning->rttype = FLOAT;break;}
			case POINTER: {fprintf(stderr,"Illegal conversion of pointer to char\n"); returning->rttype = -1; break;}
			case VOID: {fprintf(stderr,"Improper conversion of void to char\n"); returning->rttype = -1; break;}
			default: {fprintf(stderr,"General Type Error.\n"); returning->rttype = -1; break;}
			}
			  break;
		    case VOID:
		      switch((*right)->type)
			{
			case CHAR: {fprintf(stderr,"Improper conversion of void to char\n"); returning->rttype = -1; break;}
			case INT: {fprintf(stderr,"Improper conversion of void to int\n"); returning->rttype = -1; break;}
			case FLOAT:  {fprintf(stderr,"Improper conversion of void to float\n"); returning->rttype = -1; break;}
			case POINTER: {fprintf(stderr,"Improper conversion of void to pointer\n"); returning->rttype = -1; break;}
			case VOID: {fprintf(stderr,"void type in an expression.  This should cause an error.\n");returning->rttype = -1; break;}
			default: {fprintf(stderr,"General Type Error.\n"); returning->rttype = -1; break;}
			}
		    }
		}
	      }
	    break;}
    break;}
  return(returning);
}

int chklval(struct nodetype *node)
{
  if (node != NULL)
    switch (node->nodeval)
      {
      case ID:
	return(1);
	break;
      case '*':
	if (node->left != NULL)
	  if (node->left->nodeval != ID)
	    return(0);
	  else
	    return(1);
	else
	  return(0);
	break;
      default:
	return(1);
	break;
      }
  return(0);
}      


void char2int(struct nodetype **node)
{
  extern int debug_typecheck;

  if (*node != NULL)
    {
      *node = mknode(CHAR_TO_INT,*node,NULL);
      if (debug_typecheck)
	fprintf(stdout,"Changing a character to an int.\n");
    }
  else
    fprintf(stderr,"OH BOY! THIS is UGLY! inserting a node at a null pointer! Ugh!\n\n");
}

void int2float(struct nodetype **node)
{
  extern int debug_typecheck;

  if (*node != NULL)
      {
	*node = mknode(INT_TO_FLOAT,*node,NULL);
	if (debug_typecheck)
	  fprintf(stdout,"Changing a int to a float.\n");
      }
  else
    fprintf(stderr,"OH BOY! THIS is UGLY! inserting a node at a null pointer! Ugh!\n");
}

