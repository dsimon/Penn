#include <stdio.h>
#include <string.h>
#include "globals.h"
#include "y.tab.h"
#include "proto.h"

struct nodetype * mknode(int nodeval, struct nodetype *first, 
			   struct nodetype *second)
{
  struct nodetype *ptr;

  ptr = (struct nodetype *)malloc(sizeof(struct nodetype));
  if (ptr == NULL) 
    {
      printf("Cannot malloc memory in mknode\n");
      return(NULL);
    }  
  else
    {
      ptr->nodeleaf = 0;
      ptr->nodeval = nodeval;
      ptr->type = 0;
      ptr->ispointer = 0;
      ptr->tempvar = NULL;
      ptr->left = first;
      ptr->right = second;
    }
  return ptr;
}

void node_type(struct nodetype *node, int type)
{
  node->type = type;
  return;
}

struct nodetype * mkleaf(int nodeval, char *value)
{
  struct nodetype *ptr;

  ptr = (struct nodetype *)malloc(sizeof(struct nodetype));

  if (ptr == NULL) 
    {
      printf("Cannot malloc memory in mkleaf\n");
      return(NULL);
    }
  else
    {
      ptr->nodeleaf = 1;
      ptr->type = 0;
      ptr->ispointer = 0;
      ptr->tempvar = NULL;
      ptr->nodeval = nodeval;
      ptr->left = NULL;
      ptr->right = NULL;
      ptr->name = value;
    }
  return ptr;
}

void printtree(struct nodetype * tree)
{ 
  printentry(tree,0,1);
  return;

}

char * tokentostring(int num)
{
  static char buf[2];

  if (num <= 0xff)
    {
      sprintf(buf,"%c",num);
      return(buf);
    }

  switch (num)
    {
    case CHAR: return("char");
    case  DO:  return("do");
    case  ELSE: return("else");
    case  EXTERN: return("extern");
    case  FLOAT: return("float");
    case  FOR: return("for");
    case  IF: return("if");
    case  INT: return("int");
    case  REGISTER: return("register");
    case  VOID: return("void");
    case  RETURN: return("return");
    case  WHILE: return("while");
    case  AND_OP: return("&&");
    case  OR_OP: return("||");
    case  INC_OP: return("++");
    case  DEC_OP: return("--");
    case  EQ_OP: return("==");
    case  NE_OP: return("!=");
    case  LE_OP: return("<=");
    case  GE_OP: return(">=");
    case  DECLARATION: return("declaration");
    case  DECLARATION_STUFF: return("declaration_stuff");
    case  TYPE_NAME: return("typename");
    case  FUNCTION_HDR: return("function_hdr");
    case  FUNCTION_HDR_STUFF: return("function_hdr_stuff");
    case  PARM_TYPE_LIST: return("param_list");
    case  FUNCTION_BODY: return("function_body");
    case  INTERNAL_DECLS: return("internal_decls");
    case  STATEMENT_LIST: return("statement_list");
    case  ARRAY_VAR: return("assay_var");
    case  ARRAY_DECL: return("array_decl");
    case  POINTER: return("pointer");
    case  ASSIGNMENT: return("assignment");
    case  UNARY_EXPR: return("unary_expr");
    case  ARGUMENT_LIST: return("argument_list");
    case  EXTERNAL_DECLS: return("external_decls");
    case FUNCTION_DEF: return("function_def");
    case PARM_DECL: return("parm_decl");
    case FOR2: return("for_arg_2");
    case FOR3: return("for_arg_3");
    case FUNCTION_CALL: return("function_call");
    case VAR_LIST: return("var_list");
    case EXPRESSION: return("expression");
    case INT_TO_FLOAT: return("Integer to Float");
    case CHAR_TO_INT: return("Character to Integer");
    default: printf("unknown type: %d\n",num); return("unknown");
    }
  return(NULL); /*should never get reached!*/
}

void printentry(struct nodetype * node, int i, int left)
{
  int j;

  for(j = 0; j < i; j++)
    fprintf(stdout," ");  
  
  if (node == NULL) 
    {
      fprintf(stdout,"%c->NULL\n",(left ? 'L' : 'R'));
      return;
    }
  else
    {
      if (node->nodeleaf)
	{
	  fprintf(stdout, "%c->LEAF=%s\n", 
		  (left ? 'L' : 'R'), node->name);
	}
      else
	{
	  fprintf(stdout,"%c->NODE=%s\n",(left ? 'L':'R'), tokentostring(node->nodeval));
      
	  printentry(node->left,i+1,1);
	  printentry(node->right,i+1,0);
	}
    }
}




