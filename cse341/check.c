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




struct return_type *parse_expression(struct nodetype *node)
{

  struct return_type *leftside, *rightside, *returning;
  struct symtab_type *p;
  struct funct_parms *q;
  int n;

  returning = (struct return_type *)malloc(sizeof(struct return_type));

  if (node != NULL)
    {
      fprintf(stderr, "entering the typecheck function.");
      switch(node->nodeval)
	{
	case '+':
	case '*':
	case '/':
	case '-': 
	case '%':
	  {
	    leftside = parse_expression(node->left);
	    rightside = parse_expression(node->right);
	    
	    if ((leftside == NULL) || (rightside == NULL))
	      {
		returning->rttype = -1;
		returning->is_pointer = -1;
		fprintf(stderr, "Operating on an unknown type. This is bad. \n");
	      }
	    else
	      if (leftside->is_pointer) 
		if (rightside->is_pointer){
		  returning->is_pointer = 1;
		  returning->rttype = POINTER;
		}
		else
		  switch(rightside->rttype)
		    {
		    case INT: {fprintf(stderr,"Makes and integer a pointer without a cast.\n"); returning->is_pointer = 1;
			       returning->rttype = POINTER; break;}
		    default: {fprintf(stderr,"Illegal conversion to type POINTER. \n"); returning->is_pointer - 1;
			      returning->rttype = POINTER; break;}
		    }
	      else{
		returning->is_pointer = 0;
		switch(leftside->rttype)
		  {
		  case INT:
		    switch(rightside->rttype)
		      {
		      case INT: {returning->rttype = INT;break;}
		      case FLOAT: {int2float(0,node); returning->rttype = FLOAT;break;}
		      case CHAR:  {char2int(1,node); returning->rttype = INT;break;}
		      case POINTER: {fprintf(stderr,"Makes pointer to integer without cast"); returning->rttype = -1; break;}
		      case VOID: {fprintf(stderr,"Improper conversion of void to integer");returning->rttype = -1;  break;}
		      default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
		      }
		  break;
		  case FLOAT:
		    switch(rightside->rttype)
		      {
		      case FLOAT: {returning->rttype = FLOAT; break;}
		      case INT: {int2float(1,node); returning->rttype = FLOAT;break;}
		      case CHAR:  {char2int(1,node);int2float(1,node); returning->rttype = CHAR;break;}
		      case POINTER: {fprintf(stderr,"Illegal conversion of pointer to float"); returning->rttype = -1; break;}
		      case VOID: {fprintf(stderr,"Improper conversion of void to float"); returning->rttype = -1; break;}
		      default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
		      }
		    break;
		  case CHAR:
		    switch(rightside->rttype)
		      {
		      case CHAR: {returning->rttype = CHAR;break;}
		      case INT: {char2int(0,node); returning->rttype = INT;break;}
		      case FLOAT:  {char2int(0,node);int2float(0,node); returning->rttype = FLOAT;break;}
		      case POINTER: {fprintf(stderr,"Illegal conversion of pointer to char"); returning->rttype = -1; break;}
		      case VOID: {fprintf(stderr,"Improper conversion of void to char"); returning->rttype = -1; break;}
		      default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
		      }
		    break;
		  case VOID:
		    switch(rightside->rttype)
		      {
		      case CHAR: {fprintf(stderr,"Improper conversion of void to char"); returning->rttype = -1; break;}
		      case INT: {fprintf(stderr,"Improper conversion of void to int"); returning->rttype = -1; break;}
		      case FLOAT:  {fprintf(stderr,"Improper conversion of void to float"); returning->rttype = -1; break;}
		      case POINTER: {fprintf(stderr,"Improper conversion of void to pointer"); returning->rttype = -1; break;}
		      case VOID: {returning->rttype = -1; break;}
		      default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
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
	      leftside = parse_expression(node->left);
	      rightside = parse_expression(node->right);
	      
	      if ((leftside == NULL) || (rightside == NULL))
		{
		  returning->rttype = -1;
		  returning->is_pointer = -1;
		  fprintf(stderr, "Comparing an unknown type. This is bad. \n");
		}
	      else
		if (leftside->is_pointer){ 
		  if (rightside->is_pointer){
		    returning->is_pointer = 0;
		    returning->rttype = INT;
		  }
		  else
		    switch(rightside->rttype)
		      {
		      case INT: {fprintf(stderr,"Makes and integer a pointer without a cast.\n"); returning->is_pointer = 0;
				 returning->rttype = INT; break;}
		      default: {fprintf(stderr,"Illegal conversion to type POINTER. \n"); returning->is_pointer = 0;
				returning->rttype = INT; break;}
		      }
		}
	      else {
		returning->is_pointer = 0;
		switch(leftside->rttype)
		  {
		  case INT:
		    switch(rightside->rttype)
		      {
		      case INT: {returning->rttype = INT;break;}
		      case FLOAT: {int2float(0,node); returning->rttype = INT;break;}
		      case CHAR:  {char2int(1,node); returning->rttype = INT;break;}
		      case POINTER: {fprintf(stderr,"Makes pointer to integer without cast"); returning->rttype = INT; break;}
		      case VOID: {fprintf(stderr,"Improper conversion of void to integer");returning->rttype = -1;  break;}
		      default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
		      }
		  case FLOAT:
		    switch(rightside->rttype)
		      {
		      case FLOAT: {returning->rttype = INT; break;}
		      case INT: {int2float(1,node); returning->rttype = INT;break;}
		      case CHAR:  {char2int(1,node);int2float(1,node); returning->rttype = INT;break;}
		      case POINTER: {fprintf(stderr,"Illegal conversion of pointer to float"); returning->rttype = -1; break;}
		      case VOID: {fprintf(stderr,"Improper conversion of void to float"); returning->rttype = -1; break;}
		      default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
		      }
		  case CHAR:
		    switch(rightside->rttype)
		      {
		      case CHAR: {returning->rttype = INT;break;}
		      case INT: {char2int(0,node); returning->rttype = INT;break;}
		      case FLOAT:  {char2int(0,node);int2float(0,node); returning->rttype = INT;break;}
		      case POINTER: {fprintf(stderr,"Illegal conversion of pointer to char"); returning->rttype = -1; break;}
		      case VOID: {fprintf(stderr,"Improper conversion of void to char"); returning->rttype = -1; break;}
		      default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
		      }
		  case VOID:
		    switch(rightside->rttype)
		      {
		      case CHAR: {fprintf(stderr,"Improper conversion of void to char"); returning->rttype = -1; break;}
		      case INT: {fprintf(stderr,"Improper conversion of void to int"); returning->rttype = -1; break;}
		      case FLOAT:  {fprintf(stderr,"Improper conversion of void to float"); returning->rttype = -1; break;}
		      case POINTER: {fprintf(stderr,"Improper conversion of void to pointer"); returning->rttype = -1; break;}
		      case VOID: {returning->rttype = -1; break;}
		      default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
		      }
		  }
	      }
	      break;}
	case UNARY_EXPR:
	    {
	      temp_node = node->left;
	      switch(temp_node->nodeval)
		{
		case '!':
		  {
		    leftside = parse_expression(temp_node->left);
		    
		    if (leftside == NULL) 
		      {
			returning->rttype = -1;
			returning->is_pointer = -1;
			fprintf(stderr, "Negating an unknown type. This is bad. \n");
		      }
		    else
		      {
			if (leftside->is_pointer) 
			  {
			    returning->is_pointer = 0;
			    returning->rttype = INT;
			  }
			else
			  returning->is_pointer = 0;
			  switch(leftside->rttype)
			    {
			    case INT: {returning->rttype = INT;break;}
			    case FLOAT: {returning->rttype = INT; break;}
			    case CHAR: {returning->rttype = INT;break;}
			    case VOID: {returning->rttype = -1; break;}
			    default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
			    }
		      }
		    break;
		  }
		case '+':
		case '-':
		  {
		    leftside = parse_expression(temp_node->left);
		    
		    if (leftside == NULL) 
		      {
			returning->rttype = -1;
			returning->is_pointer = -1;
			fprintf(stderr, "Unary operation on an unknown type. This is bad. \n");
		      }
		    else
		      if (leftside->is_pointer) {
			returning->is_pointer = -1;
			returning->rttype = -1;
		      }
		      else{
			returning->is_pointer = 0;
			switch(leftside->rttype)
			  {
			  case INT: {returning->rttype = INT;break;}
			  case FLOAT: {returning->rttype = FLOAT; break;}
			  case CHAR: {returning->rttype = CHAR;break;}
			  case VOID: {returning->rttype = -1; break;}
			  default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
			  }
		      }
		    break;}
		case INC_OP:
		case DEC_OP:
		  {
		    leftside = parse_expression(temp_node->left);
		    
		    if (leftside == NULL) 
		      {
			rightside = parse_expression(temp_node->left);
			
			if (rightside == NULL){
			  returning->rttype = -1;
			  returning->is_pointer = -1;
			  fprintf(stderr, "Unary increment/decrement on an unknown type. This is bad. \n");
			}
			else
			  if (rightside->is_pointer) {
			    returning->is_pointer = 1;
			    returning->rttype = POINTER;
			  }
			  else{
			    returning->is_pointer = 0;
			    switch(rightside->rttype)
			      {
			      case INT: {returning->rttype = INT;break;}
			      case FLOAT: {returning->rttype = -1;fprintf(stderr,"INC/DEC Type Error."); break;}
			      case CHAR: {returning->rttype = CHAR;break;}
			      case VOID: {returning->rttype = -1; fprintf(stderr,"INC/DEC Type Error.");break;}
			      default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
			      }
			  }
		      }
		    else
		      if (leftside->is_pointer) {
			returning->is_pointer = 1;
			returning->rttype = POINTER;
		      }
		      else{
			returning->is_pointer = 0;
			switch(leftside->rttype)
			  {
			  case INT: {returning->rttype = INT;break;}
			  case FLOAT: {returning->rttype = -1;fprintf(stderr,"INC/DEC Type Error."); break;}
			  case CHAR: {returning->rttype = CHAR;break;}
			  case VOID: {returning->rttype = -1; fprintf(stderr,"INC/DEC Type Error.");break;}
			  default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
			  }
		      }
		    break;}
		case '&':
		  {
		    leftside = parse_expression(temp_node->left);
		    
		    if (leftside == NULL) 
		      {
			returning->rttype = -1;
			returning->is_pointer = -1;
			fprintf(stderr, "Negating an unknown type. This is bad. \n");
		      }
		    else
		      {
			returning->is_pointer = 1;
			if (leftside->is_pointer) {
			  returning->is_pointer = -1;
			  returning->rttype = -1;
			  fprintf(stderr,"Accessing the & operator with a pointer is not a good idea.  Try again.");
			}
			else
			  switch(leftside->rttype)
			    {
			    case INT: {returning->rttype = POINTER;break;}
			    case FLOAT: {returning->rttype = POINTER; break;}
			    case CHAR: {returning->rttype = POINTER;break;}
			    case VOID: {returning->rttype = -1; break;}
			    default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
			    }
		      }
		    break;}
		  
		case '*':
		  {
		    leftside = parse_expression(temp_node->left);
		    
		    if (leftside == NULL) 
		      {
			returning->rttype = -1;
			returning->is_pointer = -1;
			fprintf(stderr, "Deferring  an unknown type. This is bad. \n");
		      }
		    else
		      {
			returning->is_pointer = 1;
			if (leftside->is_pointer) {
			  returning->is_pointer = -1;
			  returning->rttype = 0;
			  fprintf(stderr,"Acsessing the * operator without a pointer is not a good idea.  Try again.");
			}
			else
			  switch(leftside->rttype)
			    {
			    case INT: {returning->rttype = INT;break;}
			    case FLOAT: {returning->rttype = FLOAT; break;}
			    case CHAR: {returning->rttype = CHAR;break;}
			    case VOID: {returning->rttype = VOID; break;}
			    default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
			    }
		      }
		    break;}
		}
	      break;}
	case FUNCTION_CALL:
	  { 
	    temp_node = node;
	    if (temp_node->left == NULL) 
	      fprintf(stderr,"Illegal Function Call, parse.\n");
	    else
	      {
		n = hash(temp_node->left->name);
		p = bucket[n];
		if (p == NULL)
		  {
		    fprintf(stderr, "That funciton name is UNDEFINED! ARGH!");
		    returning->rttype = -1;
		    returning->is_pointer = -1;
		    break;
		  }
		while ((p != NULL) && (p->name != temp_node->left->name))
		  p=p->next;
		if (p != NULL)
		  q = p->u.function.parm;
		while ((q != NULL) && (temp_node->right != NULL))
		  {
		    returning = parse_expression(temp_node->right);
		    if (returning->rttype != p->u.variable.type)
		      fprintf(stderr,"Illegal function Call. Operands do not match in type.\n");
		    else
		      if (q->next == NULL)
			if (temp_node->right->right == NULL)
			  {
			    returning->rttype = p->u.function.returns;
			    returning->is_pointer = 0;
			  }
			else
			  fprintf(stderr,"Illegal function call, to many operands.\n");
		      else
			if (temp_node->right->right == NULL)
			  fprintf(stderr,"Illegal function call, to few operands. \n");
			else
			  {
			    q = q->next;
			    temp_node = temp_node->right;
			  }
		  }
	      }
	    break;
	  }
	case ID:
	  {
	    n = hash(node->name);
	    p = bucket[n];
	    while ((p != NULL) && (p->name != node->name))
	      p=p->next;
	    if (p != NULL)
	      {
		returning->rttype = p->u.variable.type;
		returning->is_pointer = p->u.variable.ispointer;
	      }
	    break;
	  }
	case ASSIGNMENT:
	  {
	    if (!chklval(node))
	      {
		fprintf(stderr,"left hand side not an Lval.\n"); 
		returning->rttype = -1;
		returning->is_pointer = -1;
	      }
	    else
	      {
		leftside = parse_expression(node->left);
		rightside = parse_expression(node->right);
		
		if ((leftside == NULL) || (rightside == NULL))
		  {
		    returning->rttype = -1;
		    returning->is_pointer = -1;
		    fprintf(stderr, "Assigning unknown types. This is bad. \n");
		  }
		else
		  if (leftside->is_pointer){ 
		    if (rightside->is_pointer){
		      returning->is_pointer = 0;
		      returning->rttype = INT;
		    }
		    else
		      switch(rightside->rttype)
			{
			case INT: {fprintf(stderr,"Makes and integer a pointer without a cast.\n"); returning->is_pointer = 0;
				   returning->rttype = INT; break;}
			default: {fprintf(stderr,"Illegal conversion to type POINTER. \n"); returning->is_pointer = 0;
				  returning->rttype = -1; break;}
			}
		  }
		  else {
		    returning->is_pointer = 0;
		    switch(leftside->rttype)
		      {
		      case INT:
			switch(rightside->rttype)
			  {
			  case INT: {returning->rttype = INT;break;}
			  case FLOAT: {int2float(0,node); returning->rttype = FLOAT;break;}
			  case CHAR:  {char2int(1,node); returning->rttype = INT;break;}
			  case POINTER: {fprintf(stderr,"Makes pointer to integer without cast"); returning->rttype = INT; break;}
			  case VOID: {fprintf(stderr,"Improper conversion of void to integer");returning->rttype = -1;  break;}
			  default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
			  }
		      case FLOAT:
			switch(rightside->rttype)
			  {
			  case FLOAT: {returning->rttype = FLOAT; break;}
			  case INT: {int2float(1,node); returning->rttype = FLOAT;break;}
			  case CHAR:  {char2int(1,node); int2float(1,node);returning->rttype = FLOAT;break;}
			  case POINTER: {fprintf(stderr,"Illegal conversion of pointer to float"); returning->rttype = -1; break;}
			  case VOID: {fprintf(stderr,"Improper conversion of void to float"); returning->rttype = -1; break;}
			  default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
			  }
		      case CHAR:
			switch(rightside->rttype)
			  {
			  case CHAR: {returning->rttype = CHAR;break;}
			  case INT: {char2int(0,node); returning->rttype = INT;break;}
			  case FLOAT:  {char2int(0,node);int2float(0,node); returning->rttype = FLOAT;break;}
			  case POINTER: {fprintf(stderr,"Illegal conversion of pointer to char"); returning->rttype = -1; break;}
			  case VOID: {fprintf(stderr,"Improper conversion of void to char"); returning->rttype = -1; break;}
			  default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
			  }
		      case VOID:
			switch(rightside->rttype)
			  {
			  case CHAR: {fprintf(stderr,"Improper conversion of void to char"); returning->rttype = -1; break;}
			  case INT: {fprintf(stderr,"Improper conversion of void to int"); returning->rttype = -1; break;}
			  case FLOAT:  {fprintf(stderr,"Improper conversion of void to float"); returning->rttype = -1; break;}
			  case POINTER: {fprintf(stderr,"Improper conversion of void to pointer"); returning->rttype = -1; break;}
			  case VOID: {returning->rttype = -1; break;}
			  default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
			  }
		      }
		  }
		break;}
	    
	    break;}
	case EXPRESSION:
	  {
	    leftside = parse_expression(node->left);
	    rightside = parse_expression(node->right);
	      
	    if ((leftside == NULL) || (rightside == NULL))
		{
		  if ((leftside == NULL) && (rightside != NULL))
		    {
		      returning->rttype = rightside->rttype;
		      returning->is_pointer = rightside->is_pointer;
		    }
		  else
		    {
		      if ((leftside != NULL) && (rightside == NULL))
			{
			  returning->rttype = leftside->rttype;
			  returning->is_pointer = leftside->is_pointer;
			}
		      else
			{
			  returning->rttype = -1;
			  returning->is_pointer = -1;
			  fprintf(stderr, "Expressioning an unknown type. This is bad. \n");
			}
		    }
		}
	      else
		if (leftside->is_pointer){ 
		  if (rightside->is_pointer){
		    returning->is_pointer = 1;
		    returning->rttype = POINTER;
		  }
		  else
		    switch(rightside->rttype)
		      {
		      case INT: {fprintf(stderr,"Makes and integer a pointer without a cast.\n"); returning->is_pointer = 1;
				 returning->rttype = POINTER; break;}
		      default: {fprintf(stderr,"Illegal conversion to type POINTER. \n"); returning->is_pointer = -1;
				returning->rttype = -1; break;}
		      }
		}
	      else {
		returning->is_pointer = 0;
		switch(leftside->rttype)
		  {
		  case INT:
		    switch(rightside->rttype)
		      {
		      case INT: {returning->rttype = INT;break;}
		      case FLOAT: {int2float(0,node); returning->rttype = FLOAT;break;}
		      case CHAR:  {char2int(1,node); returning->rttype = INT;break;}
		      case POINTER: {fprintf(stderr,"Makes pointer to integer without cast"); returning->rttype = INT; break;}
		      case VOID: {fprintf(stderr,"Improper conversion of void to integer");returning->rttype = -1;  break;}
		      default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
		      }
		  case FLOAT:
		    switch(rightside->rttype)
		      {
		      case FLOAT: {returning->rttype = FLOAT; break;}
		      case INT: {int2float(1,node); returning->rttype = FLOAT;break;}
		      case CHAR:  {char2int(1,node);int2float(1,node); returning->rttype = FLOAT;break;}
		      case POINTER: {fprintf(stderr,"Illegal conversion of pointer to float"); returning->rttype = -1; break;}
		      case VOID: {fprintf(stderr,"Improper conversion of void to float"); returning->rttype = -1; break;}
		      default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
		      }
		  case CHAR:
		    switch(rightside->rttype)
		      {
		      case CHAR: {returning->rttype = CHAR;break;}
		      case INT: {char2int(0,node); returning->rttype = INT;break;}
		      case FLOAT:  {char2int(0,node);int2float(0,node); returning->rttype = FLOAT;break;}
		      case POINTER: {fprintf(stderr,"Illegal conversion of pointer to char"); returning->rttype = -1; break;}
		      case VOID: {fprintf(stderr,"Improper conversion of void to char"); returning->rttype = -1; break;}
		      default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
		      }
		  case VOID:
		    switch(rightside->rttype)
		      {
		      case CHAR: {fprintf(stderr,"Improper conversion of void to char"); returning->rttype = -1; break;}
		      case INT: {fprintf(stderr,"Improper conversion of void to int"); returning->rttype = -1; break;}
		      case FLOAT:  {fprintf(stderr,"Improper conversion of void to float"); returning->rttype = -1; break;}
		      case POINTER: {fprintf(stderr,"Improper conversion of void to pointer"); returning->rttype = -1; break;}
		      case VOID: {returning->rttype = VOID; break;}
		      default: {fprintf(stderr,"General Type Error."); returning->rttype = -1; break;}
		      }
		  }
	      }
	      break;}

	  
	  free(leftside);
	  free(rightside);
	  return(returning);
	}
    }
  else
    return(NULL);
}
      

int chklval(struct nodetype *node)
{
  if (node != NULL)
      if (node->left != NULL)
	  switch (node->left->nodeval)
	    {
	    case ID:
	      return(1);
	      break;
	    case '*':
	      if (node->left->left != NULL)
		if (node->left->left->nodeval != ID)
		  return(0);
		else
		  return(1);
	      else
		return(0);
	      break;
	    default:
		return(0);
		break;
	    }
  return(0);
}      

void findnodes(struct nodetype *tree)
{
  struct return_type *leftside, *rightside;

  if (tree != NULL)
    if ((tree != NULL) && (tree->nodeval != EXPRESSION))
      {  
	findnodes(tree->left);
	findnodes(tree->right);
      }
    else
      { 

	if (tree->nodeval == EXPRESSION)
	  parse_expression(tree);
      }
}

void char2int(int whichway, struct nodetype *node)
{
  extern int debug_typecheck;

  if (node != NULL)
    if (whichway == 1)
      {
	node->left = mknode(CHAR_TO_INT,node->left,NULL);
	if (debug_typecheck)
	  fprintf(stdout,"Changing a character to an int for node %s for the right child.", node->name);
      }
    else
      {
	node->right = mknode(CHAR_TO_INT,node->right,NULL);
	if (debug_typecheck)
	  fprintf(stdout,"Changing a character to an int for node %s for the left  child.", node->name);
      }
  else
    fprintf(stderr,"OH BOY! THIS is UGLY! inserting a node at a null pointer! Ugh!\n");
}

void int2float(int whichway, struct nodetype *node)
{
  extern int debug_typecheck;

  if (node != NULL)
    if (whichway == 1)
      {
	node->left = mknode(INT_TO_FLOAT,node->left,NULL);
	if (debug_typecheck)
	  fprintf(stdout,"Changing a character to an int for node %s for the right child.", node->name);
      }
    else
      {
	node->right = mknode(INT_TO_FLOAT,node->right,NULL);
	if (debug_typecheck)
	  fprintf(stdout,"Changing a character to an int for node %s for the right child.", node->name);
      }
  else
    fprintf(stderr,"OH BOY! THIS is UGLY! inserting a node at a null pointer! Ugh!\n");
}

