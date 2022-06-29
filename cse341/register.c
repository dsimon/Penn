/* register allocation routines */
/* CS compiler, cse341 */
/* Derron Simon and Doug Bellew */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "proto.h"
#include "symtab.h"
#include "y.tab.h"
#include "globals.h"

#define REG_NUMS 8

void dump_register(int i);

struct reg_list
{
  int valid;
  int permanent;
  int order_num;
  char *name;
};

struct var_list
{
  int valid;
  int size;
  int location;
  char *name;
  struct var_list *next;
};

static long reg_count;

static char buffer[256];

struct reg_list local[REG_NUMS];
struct reg_list input[REG_NUMS];
struct reg_list output[REG_NUMS];
struct reg_list global[REG_NUMS];

struct var_list *stack;
struct var_list *globals;
int stack_size;

struct symtab_type *symbol;

/* set up register lists and mark system registers as unusable */

void init_allocator(void)
{
  int i;

  for (i=0; i < REG_NUMS; i++)
    {
      local[i].valid = 0;
      input[i].valid = 0;
      output[i].valid = 0;
      global[i].valid = 0;

      local[i].permanent = 0;
      input[i].permanent = 0;
      output[i].permanent = 0;
      global[i].permanent = 0;
      
      local[i].name = NULL;
      input[i].name = NULL;
      output[i].name = NULL;
      global[i].name = NULL;
    }

  reg_count++;
  
  global[0].valid = 1;
  global[0].permanent = 1;

  input[7].valid = 1;
  input[7].permanent = 1;

  input[6].valid = 1;
  input[6].permanent = 1;

  output[7].valid = 1;
  output[7].permanent = 1;

  output[6].valid = 1;
  output[6].permanent = 1;

  stack = NULL;
  stack_size = -64;

  globals = NULL;

  return;
}

/* return TRUE if *name is found in the symbol table and is a variable */
/* of scope 0 */

int isglobal(char *name)
{
  struct symtab_type *symbol;

  symbol = symtab_lookup(name);

  if (symbol->symbol_type != SYM_VARIABLE)
    {
      return 0;
    }

  if (symbol->scope == 0)
    return 1;
  return 0;
}

/* returns TRUE if *name is found in symbol table and is a constant */

int isconstant(char *name)
{
  struct symtab_type *symbol;

  symbol = symtab_lookup(name);
  
  if (symbol->symbol_type == SYM_CONSTANT)
    return 1;
  return 0;
}

/* make space on stack for a local variable, looks up size and type in */
/* symbol table */

int allocate_local_var(char *name)
{
  struct var_list *ptr,*t;

  if ((ptr = (struct var_list *)malloc(sizeof(struct var_list))) == NULL)
    {
      fprintf(stderr,"Error allocating memory in allocate_local_var()\n");
      exit(1);
    }
  
  symbol = symtab_lookup(name);

  if (symbol == NULL)
    {
      fprintf(stderr,"allocate_local_var() called with unknown symbol %s\n",name);
      exit(1);
    }

  if (symbol->symbol_type != SYM_VARIABLE)
    {
      fprintf(stderr,"allocate_local_var(): symbol %s is not type variable\n",name);
      exit(1);
    }

#ifndef LARGE_CHAR
  if (symbol->u.variable.type != CHAR)    /* align if not char */
    stack_size = (stack_size & -4);
#endif

  if (symbol->u.variable.dimension)       /* if name is an array */
    {
      ptr->next = NULL;
      ptr->name = name;

      ptr->location = stack_size;
      stack_size -= (symbol->u.variable.size * symbol->u.variable.dimension);
    }
  else  
    {
      ptr->next = NULL;
      ptr->name = name;
      ptr->location = stack_size;

      stack_size -= symbol->u.variable.size;
    }

  if (stack == NULL)
    stack = ptr;
  else
    {
      t = stack;
      while (t->next != NULL)
	t = t->next;
      t->next = ptr;
    }
}

/* find a local variable in the *stack list of local variables and return */
/* it's location in the stack */

int get_local_var_location(char *name)
{
  struct var_list *t;

  t = stack;

  while (t != NULL)
    {
      if (!strcmp(t->name, name)) /* if they are equivalent */
	return t->location;
      t = t->next;
    }
  
  return 0;
}

/* allocate a local register for the variable *name and swap other name */
/* out of register if need be */

int allocate_local_reg(char *name)
{
  int i, min_i;
  long min;
  int tmp_loc;
  char *tmp_ptr, *t;

  min = LONG_MAX;

  for (i=0; i < REG_NUMS; i++)
    {
      if ((local[i].order_num < min) && (!local[i].permanent))
	{
	  min_i = i;
	  min = local[i].order_num;
	}
      if (local[i].valid == 0)   /* if we found an empty register */
	{
	  local[i].name = strdup(name);
	  local[i].valid = 1;
	  local[i].permanent = 0;
	  local[i].order_num = reg_count++;
	  
	  tmp_loc = get_local_var_location(name);

	  if (tmp_loc != 0)  /* ie. name is a local */
	    {
	      fprintf(stdout, "\tld\t[%%fp + %d], %%l%d\t! get %s from stack\n", 
		      tmp_loc, i, name);
	    }
	  else if (isglobal(name))
	    {
	      fprintf(stdout, "\tsethi\t%%hi(_%s), %%l%d\t\n", name, i);
	      fprintf(stdout, "\tld\t[%%l%d + %%lo(_%s)], %%l%d\n", i, 
		      name, i);
	    }
	  else if (isconstant(name))
	    {
	      fprintf(stdout, ".data\n");
	      fprintf(stdout, ".align 4\n");
	      fprintf(stdout, "%s:\n", tmp_ptr = newlabel('C'));
	     
	      switch (symtab_lookup(name)->u.constant.lexval)
		{
		case STRING_VALUE:
		  fprintf(stdout, "\t.asciz %s\n", name);
		  fprintf(stdout, ".align 4\n");
		  fprintf(stdout, "%s:\n", t = newlabel('P'));
		  fprintf(stdout, "\t.word %s\n", tmp_ptr);

		  fprintf(stdout,".text\n");
		  fprintf(stdout,".align 4\n");
		  fprintf(stdout,"\tset\t%s, %%g1\n", t);
		  fprintf(stdout,"\tld\t[%%g1], %%l%d\n", i);
/*
		  fprintf(stdout, "\tsethi\t%%hi(%s), %%g1\n", tmp_ptr);
		  fprintf(stdout, "\tor\t%%g1, %%lo(%s), %%l%d\n", tmp_ptr, i);
*/		  
		  break;
		case CHAR_VALUE:
		  fprintf(stdout, "\t.byte \"%s\"\n", name);
		  fprintf(stdout,".text\n");
		  fprintf(stdout,".align 4\n");
		  fprintf(stdout, "\tsethi\t%%hi(%s), %%g1\n", tmp_ptr);
		  fprintf(stdout, "\tor\t%%g1, %%lo(%s), %%l%d\n", tmp_ptr, i);
		  break;
		case INTEGER_VALUE:
/*
		  fprintf(stdout, "\t.word %s\n", name);
		  fprintf(stdout,".text\n");
		  fprintf(stdout,".align 4\n");
		  fprintf(stdout, "\tsethi\t%%hi(%s), %%l%d\n", tmp_ptr, i);
		  fprintf(stdout, "\tor\t%%l%d, %%lo(%s), %%l%d\n", i, tmp_ptr, i);
*/
		  fprintf(stdout,".text\n");
		  fprintf(stdout,".align 4\n");
		  if (atoi(name) > 256)
		    {
		      fprintf(stdout, "\tsethi\t%%hi(%d), %%l%d\n", atoi(name), i);
		      fprintf(stdout, "\tor\t%%l%d, %%lo(%d), %%l%d\n", i, atoi(name), i);
		    }
		  else
		    {
		      fprintf(stdout, "\tmov\t%d, %%l%d\n", atoi(name), i);
		    }
		  break;
		}
	    }
	  else
	    {
	      fprintf(stderr, "Something's really messed up!  We have %s - "
		      "not\na global, local or constant!\n", name);
	      exit(1);
	    }

	  return i;  /* return it's local register number */
	}
    }
  
  /* swap a variable out of a register because we need the space */

  dump_register(min_i);

/*
  fprintf(stdout, "\tst\t%%l%d, [%%fp + %d]\t\t! return %s to stack\n", min_i, 
	  get_local_var_location(local[min_i].name), local[min_i].name);
*/

  local[min_i].name = strdup(name);
  local[min_i].valid = 1;
  local[min_i].order_num = reg_count++;

  tmp_loc = get_local_var_location(name);

  if (tmp_loc != 0)
    {
      fprintf(stdout, "\tld\t[%%fp + %d], %%l%d\t! get %s from stack\n", 
	      tmp_loc, min_i, name);
    }
  else if (isglobal(name))
    {
      fprintf(stdout, "\tsethi\t%%hi(_%s), %%l%d\t\n", name, min_i);
      fprintf(stdout, "\tld\t[%%l%d + %%lo(_%s)], %%l%d\n", min_i, 
	      name, min_i);
    }
  else if (isconstant(name))
    {
      fprintf(stdout, ".data\n");
      fprintf(stdout, ".align 4\n");
      fprintf(stdout, "%s:\n", tmp_ptr = newlabel('C'));
	     
      switch (symtab_lookup(name)->u.constant.lexval)
	{
	case STRING_VALUE:
	  fprintf(stdout, "\t.asciz %s\n", name);
	  fprintf(stdout, ".align 4\n");
	  fprintf(stdout, "%s:\n", t = newlabel('P'));
	  fprintf(stdout, "\t.word %s\n", tmp_ptr);

	  fprintf(stdout,".text\n");
	  fprintf(stdout,".align 4\n");
	  fprintf(stdout,"\tset\t%s, %%g1\n", t);
	  fprintf(stdout,"\tld\t[%%g1], %%l%d\n", min_i);
	  break;
	case CHAR_VALUE:
	  fprintf(stdout, "\t.byte \"%s\"\n", name);
	  fprintf(stdout,".text\n");
	  fprintf(stdout,".align 4\n");
	  fprintf(stdout, "\tsethi\t%%hi(%s), %%g1\n", tmp_ptr);
	  fprintf(stdout, "\tor\t%%g1, %%lo(%s), %%l%d\n", tmp_ptr, min_i);
	  break;
	case INTEGER_VALUE:
	  fprintf(stdout,".text\n");
	  fprintf(stdout,".align 4\n");
	  if (atoi(name) > 256)
	    {
	      fprintf(stdout, "\tsethi\t%%hi(%d), %%l%d\n", atoi(name), min_i);
	      fprintf(stdout, "\tor\t%%l%d, %%lo(%d), %%l%d\n", min_i, atoi(name), min_i);
	    }
	  else
	    {
	      fprintf(stdout, "\tmov\t%d, %%l%d\n", atoi(name), min_i);
	    }
	  break;
	}
    }
  else
    {
      fprintf(stderr, "Something's really messed up!  We have %s not\n"
	      "a global, local or constant!\n", name);
      exit(1);
    }

  return min_i;  /* return local register number */
}

/* this associates a name with an input register, it is important to */
/* call it in the order of input parameters to the function */

int allocate_input_reg(char *name)
{
  int i;

  for (i=0; i < REG_NUMS; i++)
    {
      if (input[i].valid == 0)   /* if we found an empty register */
	{
	  input[i].name = strdup(name);
	  input[i].valid = 1;
	  input[i].permanent = 0;
	  input[i].order_num = 0;
	  return i;
	}
    }

  fprintf(stderr,"Not enough space for input registers!\n");
  exit(1);
}

/* get_reg searches for a register associated with *name and returns it */
/* as a string of the form %l1 or %i2 */

char *get_reg(char *name)
{
  int i;

  for (i=0; i < REG_NUMS; i++)
    {
      if (local[i].valid && (!local[i].permanent))
	if (!strcmp(local[i].name, name))
	  {
	    local[i].order_num = reg_count++;
	    return (sprintf(buffer,"%%l%d", i));
	  }
    }

  for (i=0; i < REG_NUMS; i++)
    {
      if (input[i].valid && (!input[i].permanent))
	if (!strcmp(input[i].name, name))
	  {
	    return (sprintf(buffer,"%%i%d", i));
	  }
    }

  return NULL;
}

/* make_local_var checks if a register is already allocated for *name and */
/* allocates it if not. */

char *make_local_var(char *name)
{
  char *text;

  if ((text = get_reg(name)) == NULL)
    {
      allocate_local_reg(name);
      
      text = get_reg(name);

      if (text == NULL)
	{
	  fprintf(stderr,"get_reg() followed allocate_local_reg() and didn't find %s\n",name);
	  exit(1);
	}
    }
  return (strdup(text));
}

/* returns the current size of the local stack */

char *get_stacksize(void)
{
  return (sprintf(buffer, "%d", stack_size & -8));
}


void dump_regs(void)
{
  int i;

  for (i=0; i < REG_NUMS; i++)
    {
      if (local[i].valid != 0)   /* if we found a full register */
	{
	  dump_register(i);
	  local[i].valid = 0;
	}
    }
}

void dump_register(int i)
{
  char *name;

  name = local[i].name;

  if (get_local_var_location(name))
    {
      fprintf(stdout, "\tst\t%%l%d, [%%fp + %d]\t\t! return %s to stack\n", i, 
	      get_local_var_location(local[i].name), local[i].name);
    }
  else if (isglobal(name))
    {
      fprintf(stdout, "\tset\t_%s, %%g1\n", name);
      fprintf(stdout, "\tst\t%%l%d, [%%g1]\t\t\t! return _%s to memory\n", i, name, name);
    }
  return;
}

#ifdef DEBUG_MAIN
main()
{
  init_allocator();

  allocate_input_reg("InA");
  allocate_input_reg("InB");

  allocate_local_var("varA");
  allocate_local_var("varB");
  allocate_local_var("varC");
  allocate_local_var("varD");
  allocate_local_var("varE");
  allocate_local_var("varF");
  allocate_local_var("varG");
  allocate_local_var("varH");
  allocate_local_var("varI");
  allocate_local_var("varJ");
  allocate_local_var("varK");
  allocate_local_var("varL");

  printf("%s\n",make_local_var("varA"));
  printf("%s\n",make_local_var("varB"));
  printf("%s\n",make_local_var("varC"));
  printf("%s\n",make_local_var("varD"));
  printf("%s\n",make_local_var("varE"));
  printf("%s\n",make_local_var("varF"));
  printf("%s\n",make_local_var("varG"));
  printf("%s\n",make_local_var("varH"));
  printf("%s\n",make_local_var("varI"));
  printf("%s\n",make_local_var("varJ"));
  printf("%s\n",make_local_var("varB"));
  printf("%s\n",make_local_var("varK"));
  printf("%s\n",make_local_var("varL"));
  printf("%s\n",make_local_var("InA"));
  printf("%s\n",make_local_var("InB"));

  printf("stacksize = %s\n",get_stacksize());
}
#endif
