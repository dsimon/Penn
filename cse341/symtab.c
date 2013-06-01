/* Symtab.C
 * This file contains all the functions dealing with the symbol 
 * table of Derron Simon & Doug Bellew's Spring 93 cse341 
 * compiler.
 */

#include <stdio.h>
#include "globals.h"
#include "symtab.h"
#include "y.tab.h"

int scope;
unsigned hash(char *s);

void debug_symtab_stats(void)
{
  fprintf(stdout, "Number of symbols in the table: %d\n ", symnum_items_table);
  fprintf(stdout, "Number of Unique identifiers  : %d\n ", symnum_ids);
  fprintf(stdout, "Number of Integer Constants   : %d\n ", symnum_int_consts);
  fprintf(stdout, "Number of Character Constants : %d\n ", symnum_char_consts);
  fprintf(stdout, "Number of Floating Constants  : %d\n ", symnum_float_consts);
  fprintf(stdout, "Number of String Constants    : %d\n ", symnum_string_consts);
  fprintf(stdout, "\n");
  fprintf(stdout, "Total number of table lookups : %d\n ", symnum_lookups);
  fprintf(stdout, "Average items searched/lookup : %f\n ", 
	  (symnum_items_searched + 0.0) / symnum_lookups);
}

void debug_symtab_dump(void)
{
  int i;
  struct symtab_type *p;

  for (i = 0; i <= PRIME; i++) 
    {
      p = bucket[i];
      while (p)
	{
	  if (p->exist)
	    {
	      if (p->symbol_type == SYM_VARIABLE)
		fprintf(stdout, "bucket number=%d symbol_type=VAR name=%s" \
			" size=%d type=%d dim=%d" \
			" scope=%d isextern=%d ispointer=%d\n",
			i, p->name, p->u.variable.size, p->u.variable.type, 
			p->u.variable.dimension, p->scope, 
			p->u.variable.isextern, p->u.variable.ispointer);
	      else if (p->symbol_type == SYM_FUNCTION)
		fprintf(stdout, "bucket number=%d symbol_type=FUNCT name=%s" \
			" returns=%d isextern=%d\n", i, p->name, 
			p->u.function.returns, p->u.function.isextern);
	      else
		fprintf(stdout, "bucket number=%d symbol_type=CONST name=%s" \
			" lexval=%d\n", i, p->name, p->u.constant.lexval);
	    }
	  p = p -> next;
	}
    }
}



void symtab_const_insert(int lex_val, char *incoming)
{
  struct symtab_type *new;
  unsigned n;

  if (lex_val == INTEGER_VALUE) 
    symnum_int_consts++;
  else if (lex_val == FLOAT_VALUE) 
    symnum_float_consts++;
  else if (lex_val == CHAR_VALUE) 
    symnum_char_consts++;
  else if (lex_val == STRING_VALUE) 
    symnum_string_consts++;
  else if (lex_val == ID) 
    symnum_ids++;
  
  n = hash(incoming);
  new = bucket[n];
  
  while(new)
    {
      if (!strcmp(new->name,incoming))
	return;
      new = new->next;
    }
  
  new = (struct symtab_type *) malloc (sizeof (struct symtab_type));
  if (new == (struct symtab_type *) NULL)
    {
      fprintf(stderr,"Cannot allocate memory for symbol table\n");
      exit(1);
    }
  
  new->exist = 1;
  new->scope = 0;
  new->name = incoming;
  new->symbol_type = SYM_CONSTANT;

  new->u.constant.lexval = lex_val;

  new->next = bucket[n];
  bucket[n] = new;
  symnum_items_table++;

  return;
}

void symtab_var_insert(int type, int scope, char *name, int size, int dim, int isextern, int ispointer)
{
  struct symtab_type *new;
  unsigned n;
  
  n = hash(name);

/*  new = bucket[n];

  while(new)
    {
      if (!strcmp(new->name,name))
	return;
      new = new->next;
    }
*/

  new = (struct symtab_type *) malloc (sizeof (struct symtab_type));
  if (new == (struct symtab_type *)NULL)
    {
      fprintf(stderr,"Not enough memory to allocate symbol\n");
      exit(1);
    }
  
  new->symbol_type = SYM_VARIABLE;
  new->name = name;
  new->exist = 1;
  new->u.variable.type = type;
  new->scope = scope;
  new->u.variable.dimension = dim;
  new->u.variable.isextern = isextern;
  new->u.variable.ispointer = ispointer;

  switch(type)
    {
    case INT:
      new->u.variable.size = INT_SIZE;
      break;
    case FLOAT:
      new->u.variable.size = FLOAT_SIZE;
      break;
    case CHAR:
      new->u.variable.size = CHAR_SIZE;
      break;
    }

  new->next = bucket[n];
  bucket[n] = new;
  symnum_items_table++;

  return;
}


void symtab_funct_insert(char *name, int returns, int isextern)
{
  struct symtab_type *new;
  unsigned n;
  
  n = hash(name);
  new = bucket[n];

  while(new)
    {
      if (!strcmp(new->name,name))
	return;
      new = new->next;
    }

  new = (struct symtab_type *) malloc (sizeof (struct symtab_type));
  if (new == (struct symtab_type *)NULL)
    {
      fprintf(stderr,"Not enough memory to allocate symbol\n");
      exit(1);
    }
  
  new->symbol_type = SYM_FUNCTION;
  new->name = name;
  new->exist = 1;
  new->scope = scope;
  new->u.function.returns = returns;
  new->u.function.isextern = isextern;
  new->u.function.parm = (struct funct_parms *) NULL;
  new->u.function.numparms = 0;

  new->next = bucket[n];
  bucket[n] = new;
  symnum_items_table++;

  return;
}


int symtab_funct_var_insert(char *funct_name, int type, int ispointer)
{
  struct symtab_type *checking;
  struct funct_parms *parm_ptr, *parm_new;
  unsigned n;

  symnum_lookups++;
  n = hash(funct_name);
  checking = bucket[n];
  
  while (checking)
    {
      symnum_items_searched++;
      
      if (!strcmp(checking->name, funct_name)) 
	break;
      else
	checking = checking->next;
    }
     
  if (checking == NULL)
    {
      fprintf(stderr,"ERROR: Adding variable declarations to unknown function\n");
      return -1;
    }

  checking->u.function.numparms++;
  
  parm_ptr = checking->u.function.parm;

  if ((parm_new = (struct funct_parms *)
       malloc(sizeof(struct funct_parms))) == NULL)
    {
      fprintf(stderr,"Error allocating memory\n");
      exit(-1);
    }

  parm_new->next = checking->u.function.parm;
  checking->u.function.parm = parm_new;

  parm_new->type = type;
  parm_new->ispointer = ispointer;

  return 0;
}




struct symtab_type * symtab_lookup(char *value)
{
  struct symtab_type *checking;
  unsigned n;
  
  symnum_lookups++;
  n = hash(value);
  checking = bucket[n];

  while (checking)
    {
      symnum_items_searched++;

      if (!strcmp(checking->name, value) && checking->exist) 
	return(checking);
      else
	checking = checking->next;
    }
  return(NULL);
}



void debug_variable_dump(void)
{
  int i,j;
  struct symtab_type *p;
  struct funct_parms *parm;

  for (i = 0; i <= PRIME; i++) 
    {
      p = bucket[i];
      while (p)
	{
	  if (p->symbol_type == SYM_VARIABLE && p->exist) 
	    fprintf(stdout, "Variable: name=%s size=%d type=%d dim=%d scope=%d isextern=%d " \
		    "ispointer=%d\n",
		    p->name, p->u.variable.size, p->u.variable.type, p->u.variable.dimension,
		    p->scope, p->u.variable.isextern, p->u.variable.ispointer);
	  if (p->symbol_type == SYM_FUNCTION && p->exist) 
	    {
	      fprintf(stdout, "Function: name=%s returns=%d isextern=%d scope=%d \n",
		      p->name, p->u.function.returns, p->u.function.isextern,
		      p->scope);
	      parm = p->u.function.parm;
	      
	      j = 1;
	      parm = p->u.function.parm;

	      while (parm)
		{
		  fprintf(stdout,"    Parameter %d: type=%d ispointer=%d\n",
			  j, parm->type, parm->ispointer);
		  j++;
		  parm = parm->next;
		}
	    }
	    p = p->next;
	}
    }
}

#ifdef STUPID
void trim_table(int scope)
{
  struct symtab_type *checking;
  unsigned n;

  for (n = 0; n < PRIME; n++)
    {
      checking = bucket[n];

      while (checking)
	{
	  if (checking->scope > scope) 
	    checking->exist = 0;

	  checking = checking->next;
	}
    }
  return;
}
#endif
