/* this file contains all the code generation routines.  all SPARC output */
/* comes from this file and from register.c.  because of register.c's */
/* efficient handling of registers this code is very simple */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "proto.h"
#include "globals.h"
#include "y.tab.h"

void end_function(void);
void start_function(char *, struct nodetype *);
void start_function_locals(struct nodetype *);
void return_statement(struct nodetype *);

int make_code(void)
{
  int i;

  i = generate_code(statement_tree);
  end_function();

  return i;
}

int generate_code(struct nodetype *node)
{
  if (node == NULL)
    return;

  switch (node->nodeval)
    {
    case FUNCTION_HDR:
      if (node->left->nodeval != POINTER)
	start_function(node->left->name, node->right);
      else
	start_function(node->left->left->name, node->right);
      break;

    case IF:
      generate_if(node);
      break;

    case WHILE:
      generate_while(node);
      break;

    case FOR:
      generate_for(node);
      break;

    case FUNCTION_BODY:
      start_function_locals(node->left);
      generate_code(node->right);
      break;

    case EXPRESSION:
      generate_expression(node);
      break;

    case RETURN:
      return_statement(node->left);
      break;

    default:
      generate_code(node->left);
      generate_code(node->right);
    }      
}

static char *stack_label;
char *end_label;

void start_function(char *fun_name, struct nodetype *header_stuff)
{
  struct nodetype *nodeptr;
  struct symtab_type *symbol;
  struct funct_parms *parms;

  fprintf(stdout,".text\n");
  fprintf(stdout,"\t.align 4\n");
  fprintf(stdout,"\t.global _%s\n", fun_name);

  fprintf(stdout,"_%s:\n",fun_name);
  fprintf(stdout,"\tsave %%sp, %s, %%sp\n", 
	  (stack_label = newlabel('S')));
  
  symbol = symtab_lookup(fun_name);

  if ((symbol == NULL) || (symbol->symbol_type != SYM_FUNCTION))
    {
      fprintf(stderr,"fun_name is not a function is start_function()\n");
      exit(1);
    }

  nodeptr = header_stuff->right;

  while (nodeptr != NULL)
    {
      if ((nodeptr->left == NULL) && (nodeptr->right == NULL))
	break;
      
#ifdef DEBUG
      fprintf(stderr,"DEBUG: allocating input reg for %s\n",
	      nodeptr->left->right->name);
#endif
      allocate_input_reg(nodeptr->left->right->name);

      nodeptr = nodeptr->right;
    }

  end_label = newlabel('L');

  return;
}

void end_function(void)
{
  fprintf(stdout, "\tba %s\n", end_label);
  fprintf(stdout, "\tnop\n");
  fprintf(stdout, "%s:\n", end_label);
  dump_regs();
  fprintf(stdout, "\tret\n");
  fprintf(stdout, "\trestore\n");
  fprintf(stdout, "\n%s = %s\n\n", stack_label, get_stacksize());
}
  
void start_function_locals(struct nodetype *node)
{
  struct nodetype *internal_ptr, *var_ptr;

  internal_ptr = node;

  while (internal_ptr != NULL)
    {
      var_ptr = internal_ptr->left->right->right;     /* point to VAR_LIST */
      while (var_ptr != NULL)
	{
#ifdef DEBUG
	  fprintf(stderr,"start_function_locals(): allocating local var %s\n",
		  var_ptr->left->name);
#endif
	  if (var_ptr->left->nodeval == POINTER)
	    allocate_local_var(var_ptr->left->left->name);
	  else if (var_ptr->left->nodeval == ARRAY_VAR)
	    allocate_local_var(var_ptr->left->left->name);
	  else
	    allocate_local_var(var_ptr->left->name);
	  var_ptr = var_ptr->right;
	}
      internal_ptr = internal_ptr->right;
    }
}

void return_statement(struct nodetype *node)
{
  char *value;

  generate_expression(node);
  value = make_local_var(node->tempvar);

  fprintf(stdout,"\tor\t%s, %%g0, %%i0\n", value);
  fprintf(stdout,"\tba\t%s\n", end_label);
  fprintf(stdout,"\tnop\n");

  return;
}
  
void function_call(struct nodetype *node)
{
  struct nodetype *tmp;
  char *funct_name;
  int i;

  funct_name = node->left->name;
  tmp = node->right;
  i=0;

  while (tmp != NULL)
    {
      generate_expression(tmp->left);
      fprintf(stdout,"\tmov\t%s, %%o%d\t\t! argument %d\n", 
	      make_local_var(tmp->left->tempvar),
	      i, i);
      i++;
      tmp = tmp->right;
    }

  fprintf(stdout,"\tcall _%s\n", funct_name);
  fprintf(stdout,"\tnop\n");
  
  fprintf(stdout,"\tmov\t%%o0, %s\n", make_local_var(node->tempvar));
  return;
}

