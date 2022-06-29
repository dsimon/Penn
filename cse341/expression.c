#include <stdio.h>
#include<stdlib.h>

#include "globals.h"
#include "proto.h"
#include "expression.h"
#include "symtab.h"
#include "y.tab.h"

void generate_expression(struct nodetype *node)
{
  char *op1,*op2,*op3;

  if (node == NULL)
    return;
  else
    switch(node->nodeval)
      {
      case '+':
	{
	  generate_expression(node->left);
	  generate_expression(node->right);
	  allocate_local_var(node->tempvar);
	  fprintf(stdout,"\tadd\t%s,%s,%s\n",
		  make_local_var(node->left->tempvar),
		  make_local_var(node->right->tempvar),
		  make_local_var(node->tempvar));
	  
	  break;
	}

/* Oh, remember, we can't call M_L_V more than 5 times in a row for a 
   a single statement, otherwise, we'll get incorrect registers.
   i.e. We'll swap out the data before we can use it. 
*/
	
      case '-':
	{
	  generate_expression(node->left);
	  generate_expression(node->right);
	  allocate_local_var(node->tempvar);
	  fprintf(stdout,"\tsub\t%s,%s,%s\n",
		  make_local_var(node->left->tempvar), 
		  make_local_var(node->right->tempvar),
		  make_local_var(node->tempvar));
	  break;
	}
      case '*':
	{
	  generate_expression(node->left);
	  generate_expression(node->right);
	  allocate_local_var(node->tempvar);
	  fprintf(stdout,"\tor\t%s,%%g0,%%o0\t\t!Move first arg into %%o0\n",
		  make_local_var(node->left->tempvar));
	  fprintf(stdout,"\tor\t%s,%%g0,%%o1\t\t!Move second arg into %%o1\n",
		  make_local_var(node->right->tempvar));
	  fprintf(stdout,"\tcall\t.mul\n");
	  fprintf(stdout,"\tnop\t\t\t\t!Don't let the delay slot fool you!\n");
	  fprintf(stdout,"\tor\t%%o0,%%g0,%s\t\t!Move into proper storage\n",
		  make_local_var(node->tempvar));
	  break;
	}
      case '/':
	{
	  generate_expression(node->left);
	  generate_expression(node->right);
	  allocate_local_var(node->tempvar);
	  fprintf(stdout,"\tor\t%%g0,%%g0,%%o1\t\t!Clear upper dividend reg.\n");
	  fprintf(stdout,"\tor\t%s,%%g0,%%o0\t\t!Get value of Lower Dividend.\n",
		  make_local_var(node->left->tempvar));
	  fprintf(stdout,"\tor\t%s,%%g0,%%o2\t\t!Get Divisor.\n",
		  make_local_var(node->right->tempvar));
	  fprintf(stdout,"\tcall\t.div\n");
	  fprintf(stdout,"\tnop\t\t\t\t!Don't let the delay slot fool you!\n");
	  fprintf(stdout,"\tor\t%%o0,%%g0,%s\t\t!Store Result.\n",
		  make_local_var(node->tempvar));
	  break;
	}
      case '%':
	{
	  generate_expression(node->left);
	  generate_expression(node->right);
	  allocate_local_var(node->tempvar);
	  fprintf(stdout,"\tor\t%%g0,%%g0,%%o1\t\t!Clear upper dividend reg.\n");
	  fprintf(stdout,"\tor\t%s,%%g0,%%o0\t\t!Get value of Lower Dividend.\n",
		  make_local_var(node->left->tempvar));
	  fprintf(stdout,"\tor\t%s,%%g0,%%o2\t\t!Get Divisor.\n",
		  make_local_var(node->right->tempvar));
	  fprintf(stdout,"\tcall\t.div\n");
	  fprintf(stdout,"\tnop\t\t\t\t!Don't let the delay slot fool you!\n");
	  fprintf(stdout,"\tor\t%%o0,%%g0,%s\t\t!Store Result.\n",
		  make_local_var(node->tempvar));
	  
	  break;
	}
      case INTEGER_VALUE:
      case FLOAT_VALUE:
      case CHAR_VALUE:
      case STRING_VALUE:
      case ID:
	{
	  node->tempvar = node->name;
	  break;
	}
      case EXPRESSION:
	{
	  generate_expression(node->left);
	  node->tempvar = node->left->tempvar;
	  break;
	}
      case ASSIGNMENT:
	{
	  generate_expression(node->left);
	  generate_expression(node->right);
	  allocate_local_var(node->tempvar);
	  fprintf(stdout,"\tor\t%s,%%g0,%s\t\t!Move value into other value.\n",
		  make_local_var(node->right->tempvar),
		  make_local_var(node->left->tempvar));
	  fprintf(stdout,"\tor\t%s,%%g0,%s\t\t!Is the value of the '='.\n",
		  make_local_var(node->left->tempvar),
		  make_local_var(node->tempvar));
	  break;
	}
      case AND_OP:
	{
	  generate_expression(node->left);
	  generate_expression(node->right);
	  allocate_local_var(node->tempvar);
	  fprintf(stdout,"\tand\t%s,%s,%s\t\t!Op1 & Op2 go to temp Variable\n",
		  make_local_var(node->left->tempvar),
		  make_local_var(node->right->tempvar),
		  make_local_var(node->tempvar));
	  break;
	}
      case OR_OP:
	{
	  generate_expression(node->left);
	  generate_expression(node->right);
	  allocate_local_var(node->tempvar);
	  fprintf(stdout,"\tor\t%s,%s,%s\t\t!Op1 & Op2 go to temp Variable\n",
		  make_local_var(node->left->tempvar),
		  make_local_var(node->right->tempvar),
		  make_local_var(node->tempvar));
	  break;
	}
      case NE_OP:
	{
	  dest1 = newlabel('L');
	  dest2 = newlabel('L');
	  generate_expression(node->left);
	  generate_expression(node->right);
	  allocate_local_var(node->tempvar);
	  fprintf(stdout,"\tsubcc\t%s,%s,%%g0\t\t!Compare op1 with op2.\n",
		  make_local_var(node->left->tempvar),
		  make_local_var(node->right->tempvar));
	  fprintf(stdout,"\tbne\t%s\n",dest1);
	  fprintf(stdout,"\tnop\n");
	  fprintf(stdout,"\tor\t0,%%g0,%s\t\t!The things _are_ equal.\n",
		  make_local_var(node->tempvar));
	  fprintf(stdout,"\tba\t%s\n",dest2);
	  fprintf(stdout,"\tnop\n");
	  fprintf(stdout,"%s:\tor\t1,%%g0,%s\t\t!The things aren't equal.\n",
		  dest1,make_local_var(node->tempvar));
	  fprintf(stdout,"%s:\tnop\n",dest2);
	  break;
	}
      case EQ_OP:
	{
	  dest1 = newlabel('L');
	  dest2 = newlabel('L');
	  generate_expression(node->left);
	  generate_expression(node->right);
	  allocate_local_var(node->tempvar);
	  fprintf(stdout,"\tsubcc\t%s,%s,%%g0\t\t!Compare op1 with op2.\n",
		  make_local_var(node->left->tempvar),
		  make_local_var(node->right->tempvar));
	  fprintf(stdout,"\tbeq\t%s\n",dest1);
	  fprintf(stdout,"\tnop\n");
	  fprintf(stdout,"\tor\t0,%%g0,%s\t\t!The things aren't equal.\n",
		  make_local_var(node->tempvar));
	  fprintf(stdout,"\tba\t%s\n",dest2);
	  fprintf(stdout,"\tnop\n");
	  fprintf(stdout,"%s:\tor\t1,%%g0,%s\t\t!The things are equal.\n",
		  dest1,make_local_var(node->tempvar));
	  fprintf(stdout,"%s:\tnop\n",dest2);
	  break;
	}
      case LE_OP:
	{
	  dest1 = newlabel('L');
	  dest2 = newlabel('L');
	  generate_expression(node->left);
	  generate_expression(node->right);
	  allocate_local_var(node->tempvar);
	  fprintf(stdout,"\tsubcc\t%s,%s,%%g0\t\t!Compare op1 with op2.\n",
		  make_local_var(node->left->tempvar),
		  make_local_var(node->right->tempvar));
	  fprintf(stdout,"\tbgt\t%s\n",dest1);
	  fprintf(stdout,"\tnop\n");
	  fprintf(stdout,"\tor\t1,%%g0,%s\t\t!It is Less than or equal.\n",
		  make_local_var(node->tempvar));
	  fprintf(stdout,"\tba\t%s\n",dest2);
	  fprintf(stdout,"\tnop\n");
	  fprintf(stdout,"%s:\tor\t0,%%g0,%s\t\t!It is greater than.\n",
		  dest1,make_local_var(node->tempvar));
	  fprintf(stdout,"%s:\tnop\n",dest2);
	  break;
	}

      case GE_OP:
	{
	  dest1 = newlabel('L');
	  dest2 = newlabel('L');
	  generate_expression(node->left);
	  generate_expression(node->right);
	  allocate_local_var(node->tempvar);
	  fprintf(stdout,"\tsubcc\t%s,%s,%%g0\t\t!Compare op1 with op2.\n",
		  make_local_var(node->left->tempvar),
		  make_local_var(node->right->tempvar));
	  fprintf(stdout,"\tblt\t%s\n",dest1);
	  fprintf(stdout,"\tnop\n");
	  fprintf(stdout,"\tor\t1,%%g0,%s\t\t!It is Greater than or equal.\n",
		  make_local_var(node->tempvar));
	  fprintf(stdout,"\tba\t%s\n",dest2);
	  fprintf(stdout,"\tnop\n");
	  fprintf(stdout,"%s:\tor\t0,%%g0,%s\t\t!It is Less than.\n",
		  dest1,make_local_var(node->tempvar));
	  fprintf(stdout,"%s:\tnop\n",dest2);
	  break;
	}
      case '<':
	{
	  dest1 = newlabel('L');
	  dest2 = newlabel('L');
	  generate_expression(node->left);
	  generate_expression(node->right);
	  allocate_local_var(node->tempvar);
	  fprintf(stdout,"\tsubcc\t%s,%s,%%g0\t\t!Compare op1 with op2.\n",
		  make_local_var(node->left->tempvar),
		  make_local_var(node->right->tempvar));
	  fprintf(stdout,"\tbge\t%s\n",dest1);
	  fprintf(stdout,"\tnop\n");
	  fprintf(stdout,"\tor\t1,%%g0,%s\t\t!It is Less than .\n",
		  make_local_var(node->tempvar));
	  fprintf(stdout,"\tba\t%s\n",dest2);
	  fprintf(stdout,"\tnop\n");
	  fprintf(stdout,"%s:\tor\t0,%%g0,%s\t\t!It is Greater than or equal.\n",
		  dest1,make_local_var(node->tempvar));
	  fprintf(stdout,"%s:\tnop\n",dest2);
	  break;
	}
      case '>':
	{
	  dest1 = newlabel('L');
	  dest2 = newlabel('L');
	  generate_expression(node->left);
	  generate_expression(node->right);
	  allocate_local_var(node->tempvar);
	  fprintf(stdout,"\tsubcc\t%s,%s,%%g0\t\t!Compare op1 with op2.\n",
		  make_local_var(node->left->tempvar),
		  make_local_var(node->right->tempvar));
	  fprintf(stdout,"\tble\t%s\n",dest1);
	  fprintf(stdout,"\tnop\n");
	  fprintf(stdout,"\tor\t1,%%g0,%s\t\t!It is Greater than .\n",
		  make_local_var(node->tempvar));
	  fprintf(stdout,"\tba\t%s\n",dest2);
	  fprintf(stdout,"\tnop\n");
	  fprintf(stdout,"%s:\tor\t0,%%g0,%s\t\t!It is Less than or equal.\n",
		  dest1,make_local_var(node->tempvar));
	  fprintf(stdout,"%s:\tnop\n",dest2);
	  break;
	}
      case UNARY_EXPR:
	{
	  allocate_local_var(node->tempvar);
	  switch(node->left->nodeval)
	    {
	    case '+':
	      {
		generate_expression(node->left->left);
		generate_expression(node->left->right);
		allocate_local_var(node->left->tempvar);
		fprintf(stdout,"\tnop\t\t\t\t!This is unary plus.\n");
		node->left->tempvar = node->left->left->tempvar;
		break;
	      }
	    case '-':
	      {
		generate_expression(node->left->left);
		generate_expression(node->left->right);
		allocate_local_var(node->left->tempvar);
		fprintf(stdout,"\tneg\t%s,%s\t\t!Unary Minus.\n",
			make_local_var(node->left->left->tempvar),
			make_local_var(node->left->tempvar));
		break;
	      }
	    case '&':
	      allocate_local_var(node->left->tempvar);
	      generate_expression(node->left->left);
	      dump_regs();
	      
	      if (isglobal(node->left->left->tempvar))
		{
		  fprintf(stdout, "\tsethi\t%%hi(_%s), %s\n", 
			  node->left->left->tempvar,
			  make_local_var(node->left->tempvar));
		  fprintf(stdout, "\tor\t%s, %%lo(_%s), %s\n",
			  make_local_var(node->left->tempvar),
			  node->left->left->tempvar,
			  make_local_var(node->left->tempvar));
		}
	      else
		{
		  fprintf(stdout, "\tadd\t%%fp, %d, %s\n", 
			  get_local_var_location(node->left->left->tempvar), 
			  make_local_var(node->left->tempvar));
		}
              break;
	    case '*':
	      allocate_local_var(node->left->tempvar);
	      generate_expression(node->left->left);
	      dump_regs();

	      fprintf(stdout, "\tld\t[%s], %s\n", 
		      make_local_var(node->left->left->tempvar),
		      make_local_var(node->left->tempvar));
	      break;
	    }
	  node->tempvar = node->left->tempvar;
	  break;
	}
	break;
/*these will go in here so we can do a real tree.  I guess we'll fix these later. */

      case INT_TO_FLOAT:
      case CHAR_TO_INT:
	{
	  generate_expression(node->left);
	  generate_expression(node->right);
	  allocate_local_var(node->tempvar);
	  node->tempvar = node->left->tempvar;
	  fprintf(stdout,"\tnopt\t\t\t!We hit a convert routine. Oops.\n");
	  break;
	}
	  
/* ----------------------------------------------------------------------------- */
      case FUNCTION_CALL:
	{
	  allocate_local_var(node->tempvar);
	  function_call(node);
	  break;
	}
      default:
	{
	  return;
	  break;
	}
	
      }
}

