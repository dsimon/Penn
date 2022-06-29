#include <stdio.h>
#include<stdlib.h>

#include "globals.h"
#include "proto.h"
#include "expression.h"
#include "symtab.h"
#include "y.tab.h"

void generate_if(struct nodetype *node)
{
  char *op1,*op2;

  if (node == NULL)
    fprintf(stderr, "tried to generate a null if statment! Were Hosed!\n");
  else
    {
      dump_regs();
      generate_expression(node->left);
      fprintf(stdout,"\tsubcc\t%s,%%g0,%%g0\t\t!Test expression value.\n",
	      make_local_var(node->left->tempvar));

      if (node->right->nodeval == ELSE)
	{
	  op1 = newlabel('L');
	  op2 = newlabel('L');
	  fprintf(stdout,"\tbeq\t%s\n",op1);
	  fprintf(stdout,"\tnop\n");
	  generate_code(node->right->left);
	  fprintf(stdout,"\tba\t%s\n",op2);
	  fprintf(stdout,"\tnop\n");
	  fprintf(stdout,"%s:\tnop\t\t\t\t!This is the else statement.\n",
		  op1);
	  generate_code(node->right->right);
	  fprintf(stdout,"%s:\tnop\t\t\t\t!This is the end of the if.\n",
		  op2);
	}
      else
	{
	  op1 = newlabel('L');
	  fprintf(stdout,"\tbeq\t%s\n",op1);
	  fprintf(stdout,"\tnop\n");
	  generate_code(node->right);
	  fprintf(stdout,"%s:\tnop\t\t\t\t!This is the end of the if.\n",
		  op1);
	}
    }
}



void generate_while(struct nodetype *node)
{
  char *op1,*op2;

  if (node == NULL)
    fprintf(stderr, "tried to generate a null while statment! Were Hosed!\n");
  else
    {
      op1 = newlabel('L');
      op2 = newlabel('L');
      dump_regs();
      fprintf(stdout,"\tba\t%s\n",op1);
      fprintf(stdout,"\tnop\n");

      fprintf(stdout,"%s:\tnop\t\t\t\t!This is the WHILE statement.\n",
	      op2);
      generate_code(node->right);

      fprintf(stdout,"%s:\tnop\t\t\t\t!This is the WHILE test.\n",
	      op1);

      generate_expression(node->left);
      fprintf(stdout,"\tsubcc\t%s,%%g0,%%g0\t\t!Test expression value.\n",
	      make_local_var(node->left->tempvar));

      fprintf(stdout,"\tbne\t%s\n",op2);
      fprintf(stdout,"\tnop\n");

    }
}

void generate_do(struct nodetype *node)
{
  char *op1,*op2;

  if (node == NULL)
    fprintf(stderr, "tried to generate a null while statment! Were Hosed!\n");
  else
    {
      op1 = newlabel('L');
      op2 = newlabel('L');

      fprintf(stdout,"%s:\tnop\t\t\t\t!This is the DO statement.\n",
	      op2);
      generate_code(node->left);

      fprintf(stdout,"%s:\tnop\t\t\t\t!This is the DO test.\n",
	      op1);

      generate_expression(node->right);
      fprintf(stdout,"\tsubcc\t%s,%%g0,%%g0\t\t!Test expression value.\n",
	      make_local_var(node->left->tempvar));

      fprintf(stdout,"\tbne\t%s\n",op2);
      fprintf(stdout,"\tnop\n");

    }
}

void generate_for(struct nodetype *node)
{
  char *op1,*op2;
  
  if (node == NULL)
    fprintf(stderr, "tried to generate a null for statment! Were Hosed!\n");
  else
    {
      op1 = newlabel('L');
      op2 = newlabel('L');

      fprintf(stdout,"\tnop\t\t\t\t!This is the start of a FOR statement.\n");
      generate_expression(node->left);
      dump_regs();
      fprintf(stdout,"\tba\t%s\n",op1);
      fprintf(stdout,"\tnop\n");

      fprintf(stdout,"%s:\tnop\t\t\t\t!This is the FOR statement.\n",
	      op2);
      generate_code(node->right->right->right);
      generate_expression(node->right->right->left);
      
      fprintf(stdout,"%s:\tnop\t\t\t\t!This is the FOR test.\n",
	      op1);
      generate_expression(node->right->left);
      fprintf(stdout,"\tsubcc\t%s,%%g0,%%g0\t\t!Test expression value.\n",
	      make_local_var(node->left->tempvar));
      fprintf(stdout,"\tbne\t%s\n",op2);
      fprintf(stdout,"\tnop\n");

    }
}

