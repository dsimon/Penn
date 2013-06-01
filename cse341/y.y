%{
/* grammar for CS compiler */
/* Derron Simon and Douglas Bellew */

/* y.y does all the parsing of the CS language and calls the type check */
/* functions as it parses as well as generates temporary variables for */
/* expressions. */

/* a parse tree is generated for the entire grammar.  each non-terminal */
/* rule is a node in the tree and is declared as nodetype in the %union */
/* structure.  because of this the entire source file can be parsed.  */

#include <stdio.h>
#include "globals.h"
#include "proto.h"
#include "check.h"

struct return_type *returned;
struct nodetype *statement_tree;
struct nodetype *tmp;
struct symtab_type *tptr;
char *name;
int size;
extern FILE *yyout;

int funct_return_type;

extern int yylineno;
extern char *yytext;

int yyerror(char *s)
{
  fprintf(stderr,"line %d: %s at '%s'\n", yylineno, s, yytext);
}

void generate_temp(struct nodetype *node)
{
  node->tempvar = newlabel('T');

  if (node->ispointer == 1)
    symtab_var_insert(node->type,scope,node->tempvar,POINTER_SIZE,0,0,1);
  else  
    switch(node->type)
      {
      case INT:
	symtab_var_insert(node->type,scope,node->tempvar,INT_SIZE,0,0,0);
	break;
      case FLOAT:
	symtab_var_insert(node->type,scope,node->tempvar,FLOAT_SIZE,0,0,0);
	break;
      case CHAR:
	symtab_var_insert(node->type,scope,node->tempvar,CHAR_SIZE,0,0,0);
	break;
      case VOID:
	symtab_var_insert(node->type,scope,node->tempvar,0,0,0,0);
	break;
      default:
	fprintf(stderr,"Never should reach here (generate_tmp with %s)\n",node->tempvar);
      }
}
%}

%union {
  int tokenid;
  char *stringval;
  struct nodetype *node;
}

%token <tokenid> CHAR
%token <tokenid> DO          
%token <tokenid> EXTERN      
%token <tokenid> FLOAT       
%token <tokenid> FOR         
%token <tokenid> IF          
%token <tokenid> ELSE
%token <tokenid> INT         
%token <tokenid> REGISTER    
%token <tokenid> VOID        
%token <tokenid> RETURN      
%token <tokenid> WHILE       

%token <tokenid> AND_OP      
%token <tokenid> OR_OP       
%token <tokenid> INC_OP      
%token <tokenid> DEC_OP      
%token <tokenid> EQ_OP       
%token <tokenid> NE_OP       
%token <tokenid> LE_OP       
%token <tokenid> GE_OP       

%token <stringval> INTEGER_VALUE  
%token <stringval> FLOAT_VALUE    
%token <stringval> CHAR_VALUE     
%token <stringval> STRING_VALUE    
%token <stringval> ID         

%type <node> program
%type <node> external_decls
%type <node> declaration
%type <node> type_name
%type <node> var_list
%type <node> var_item
%type <node> array_var
%type <node> scalar_var
%type <node> function_def
%type <node> function_hdr
%type <node> parm_type_list
%type <node> parm_list
%type <node> parm_decl
%type <node> function_body
%type <node> internal_decls
%type <node> statement_list
%type <node> statement
%type <node> compound_stmt
%type <node> if_stmt
%type <node> for_stmt
%type <node> while_stmt
%type <node> dowhile_stmt
%type <node> return_stmt
%type <node> expression_stmt
%type <node> expression
%type <node> assignment_expr
%type <node> unary_expr
%type <node> binary_expr
%type <node> postfix_expr
%type <node> primary_expr
%type <node> argument_list
%type <node> null_stmt

%token <tokenid> DECLARATION
%token <tokenid> EXTERNAL_DECLS
%token <tokenid> DECLARATION_STUFF
%token <tokenid> TYPE_NAME
%token <tokenid> FUNCTION_HDR
%token <tokenid> FUNCTION_HDR_PROTO
%token <tokenid> FUNCTION_HDR_STUFF
%token <tokenid> FUNCTION_DEF
%token <tokenid> PARM_TYPE_LIST
%token <tokenid> PARM_DECL
%token <tokenid> FOR2
%token <tokenid> FOR3
%token <tokenid> FUNCTION_CALL
%token <tokenid> FUNCTION_BODY
%token <tokenid> INTERNAL_DECLS
%token <tokenid> STATEMENT_LIST
%token <tokenid> ARRAY_VAR
%token <tokenid> ARRAY_DECL
%token <tokenid> POINTER
%token <tokenid> ASSIGNMENT
%token <tokenid> UNARY_EXPR
%token <tokenid> ARGUMENT_LIST
%token <tokenid> VAR_LIST
%token <tokenid> EXPRESSION

%token <tokenid> CHAR_TO_INT
%token <tokenid> INT_TO_FLOAT

%token ERROR      
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%left ','
%right  '='
%left OR_OP
%left AND_OP
%left EQ_OP NE_OP
%left '<' LE_OP '>' GE_OP
%left '+' '-'
%left '*' '/' '%'
%right '&' INC_OP DEC_OP
%left '(' ')' '[' ']'

%%

program:            external_decls { statement_tree = (struct nodetype *)$1; }
                     ;
 
external_decls:     declaration external_decls 
                      {
			$$ = (struct nodetype *)mknode(EXTERNAL_DECLS,$1,$2); 
		      }
                     | function_def { $$ = $1; }  
                     ;

declaration:        EXTERN type_name var_list ';' 
                      { 
			$$ = mknode(DECLARATION, mknode(EXTERN, NULL, NULL),
			  mknode(DECLARATION_STUFF, $2, $3));
 
			tmp = $3;			/* tmp is var_list->left */
			switch ($2->nodeval)
			{
			  case FLOAT:
			    size = FLOAT_SIZE;
			    break;
			  case INT:
			    size = INT_SIZE;
			    break;
			  case CHAR:
			    size = CHAR_SIZE;
			    break;
			}

			while (tmp != (struct nodetype *)NULL)
			  {
			    if (tmp->left->nodeval == POINTER)
			      {
				symtab_var_insert($2->nodeval, scope, tmp->left->left->name,
						POINTER_SIZE, 0, 1, 1);
				if (scope == 0) 
				  {
				    fprintf(stdout, ".global _%s\n",tmp->left->left->name);
				    fprintf(stdout,".common _%s, %d, \"bss\"\n",tmp->left->left->name, 
					    POINTER_SIZE);
				  }
			      }
			    if (tmp->left->nodeval == ID)
			      {
				symtab_var_insert($2->nodeval, scope, tmp->left->name, size, 
						  0, 1, 0);
				if (scope == 0)
				  {
				    fprintf(stdout,".global _%s\n", tmp->left->name);
				    fprintf(stdout,".common _%s, %d, \"bss\"\n",tmp->left->name,
					    size);
				  }
			      }
			    if (tmp->left->nodeval == ARRAY_VAR)
			      {
				symtab_var_insert($2->nodeval, scope, tmp->left->left->name, 
						  size, atoi(tmp->left->right->name), 1, 1);
				if (scope == 0)
				  {
				    fprintf(stdout,".global _%s\n",tmp->left->left->name);
				    fprintf(stdout,".common _%s, %d, \"bss\"\n", tmp->left->left->name,
					    size * (atoi(tmp->left->left->right->name)));
				  }
			      }
			    tmp = tmp->right;
			  }
		      }
		    | REGISTER type_name var_list ';' 
                      { 
			$$ = mknode(DECLARATION, mknode(REGISTER,NULL,NULL),
			  mknode(DECLARATION_STUFF, $2, $3));
 
			tmp = $3;			/* tmp is var_list->left */
			switch ($2->nodeval)
			{
			  case FLOAT:
			    size = FLOAT_SIZE;
			    break;
			  case INT:
			    size = INT_SIZE;
			    break;
			  case CHAR:
			    size = CHAR_SIZE;
			    break;
			}

			while (tmp != (struct nodetype *)NULL)
			  {
			    if (tmp->left->nodeval == POINTER)
			      {
				symtab_var_insert($2->nodeval, scope, tmp->left->left->name,
						  POINTER_SIZE, 0, 0, 1);
				if (scope == 0)
				  {
				    fprintf(stdout, ".global _%s\n",tmp->left->left->name);
				    fprintf(stdout, ".common _%s, %d, \"bss\"\n",tmp->left->left->name,
					    POINTER_SIZE);
				  }
			      }
			    if (tmp->left->nodeval == ID)
			      {
				symtab_var_insert($2->nodeval, scope, tmp->left->name, size, 
						0, 0, 0);
				if (scope == 0)
				  {
				    fprintf(stdout, ".global _%s\n",tmp->left->name);
				    fprintf(stdout, ".common _%s, %d\n, \"bss\"\n",tmp->left->name,
					    size);
				  }
			      }

			    if (tmp->left->nodeval == ARRAY_VAR)
			      {
				symtab_var_insert($2->nodeval, scope, tmp->left->left->name, 
						  size, atoi(tmp->left->right->name), 0, 1);
				if (scope == 0)
				  {
				    fprintf(stdout, ".global _%s\n", tmp->left->left->name);
				    fprintf(stdout, ".common _%s, %d, \"bss\"\n",tmp->left->left->name,
					    size * (atoi(tmp->left->right->name)));
				  }
			      }
			    tmp = tmp->right;
			  }
		      }
                    | type_name var_list ';'
                      {
			$$ = mknode(DECLARATION, NULL,
			  mknode(DECLARATION_STUFF, $1, $2));
 
			tmp = $2;			/* tmp is var_list->left */
			switch ($1->nodeval)
			{
			  case FLOAT:
			    size = FLOAT_SIZE;
			    break;
			  case INT:
			    size = INT_SIZE;
			    break;
			  case CHAR:
			    size = CHAR_SIZE;
			    break;
			}

			while (tmp != (struct nodetype *)NULL)
			  {
			    if (tmp->left->nodeval == POINTER)
			      {
				symtab_var_insert($1->nodeval, scope, tmp->left->left->name,
						  POINTER_SIZE, 0, 0, 1);
				if (scope == 0)
				  {
				    fprintf(stdout, ".global _%s\n", tmp->left->left->name);
				    fprintf(stdout, ".common _%s, %d, \"bss\"\n",tmp->left->left->name,
					    POINTER_SIZE);
				  }
			      }
			    if (tmp->left->nodeval == ID)
			      {
				symtab_var_insert($1->nodeval, scope, tmp->left->name, size, 
						  0, 0, 0);
				if (scope == 0)
				  {
				    fprintf(stdout, ".global _%s\n", tmp->left->name);
				    fprintf(stdout, ".common _%s, %d, \"bss\"\n",tmp->left->name,
					    size);
				  }
			      }
			    if (tmp->left->nodeval == ARRAY_VAR)
			      {
				symtab_var_insert($1->nodeval, scope, tmp->left->left->name, 
						  size, atoi(tmp->left->right->name), 0, 1);
				if (scope == 0)
				  {
				    fprintf(stdout, ".global _%s\n", tmp->left->right->name);
				    fprintf(stdout, ".common _%s, %d, \"bss\"\n",tmp->left->right->name,
					    size * (atoi(tmp->left->right->name)));
				  }			     
			      }
			    tmp = tmp->right;
			  }
		      }
                    ;
 
type_name:          VOID { $$ = mknode(VOID,NULL,NULL); } 
                     | INT { $$ = mknode(INT,NULL,NULL); }
                     | FLOAT { $$ = mknode(FLOAT,NULL,NULL); }
                     | CHAR { $$ = mknode(CHAR,NULL,NULL); }
                     ;

var_list:           var_list ',' var_item { $$ = mknode(VAR_LIST, $3, $1); }
                     | var_item { $$ = mknode(VAR_LIST, $1, NULL); }
                     ;

var_item:           array_var { $$ = $1; } 
                     | scalar_var { $$ = $1; }
                     | '*' scalar_var { $$ = mknode(POINTER, $2, NULL); }
                     | function_hdr 
                       {
                         $$ = $1;
                       } 
                     ;

array_var:          ID '[' INTEGER_VALUE ']'
                      { 
			$$ = mknode(ARRAY_VAR, mkleaf(ID, $1), mkleaf(ID,$3)); 
		      }
                     ;

scalar_var:         ID 
                      { 
			$$ = mkleaf(ID, $1); 
		      }
                     ;

function_def:       function_hdr '{' function_body '}' 
                      { 
                        $$ = mknode(FUNCTION_DEF, $1, $3); 
                        scope--;
#ifdef STUPID
                        trim_table(scope);
#endif
                      }
                     ;

function_hdr:       type_name ID '(' parm_type_list ')' 
                        { 
                          $$ = mknode(FUNCTION_HDR, mkleaf(ID,$2), 
			    mknode(FUNCTION_HDR_STUFF, $1, $4));
			  scope--;
                          funct_return_type = $1->nodeval;
                          symtab_funct_insert($2, $1->nodeval, 0);
                          scope++;

                          tmp = $4;

                          while (tmp)
                            {
			      if ((tmp->left == NULL) && (tmp->right == NULL))
                                {
				  symtab_funct_var_insert($2, VOID, 0);
				  break;
				}
			      else 
				{
				  if (tmp->left->right->nodeval == POINTER)
				    {
				      symtab_funct_var_insert($2, 
					    tmp->left->left->nodeval, 1);
				    }				  
				  else
				    {
				      symtab_funct_var_insert($2, 
                                            tmp->left->left->nodeval, 0);
				    }
				}
			      tmp = tmp->right;
			    }
                        } 
                    | type_name '*' ID '(' parm_type_list ')'
                        {
			  $$ = mknode(FUNCTION_HDR, mknode(POINTER, mkleaf(ID,$3), NULL),
			    mknode(FUNCTION_HDR_STUFF, $1, $5));
			  scope--;
                          funct_return_type = $1->nodeval;
                          symtab_funct_insert($3, $1->nodeval, 0);
                          scope++;

                          tmp = $5;

                          while (tmp)
                            {
			      if ((tmp->left == NULL) && (tmp->left == NULL))
                                {
                                  symtab_funct_var_insert($3, VOID, 0);
				  break;
				}
			      else 
				{
				  if (tmp->left->right->nodeval == POINTER)
				    {
				      symtab_funct_var_insert($3, tmp->left->left->nodeval, 1);
				    }
				  else
				    {
				      symtab_funct_var_insert($3, tmp->left->left->nodeval, 0);
				    }
				}
			      tmp = tmp->right;
			    }
			}
                    | ID '(' parm_type_list ')'
                        {
			  $$ = mknode(FUNCTION_HDR_PROTO, mkleaf(ID, $1),
			    mknode(FUNCTION_HDR_STUFF, mknode(INT,NULL,NULL), $3));
			  scope--;
                          funct_return_type = INT;
                          symtab_funct_insert($1, INT, 0);	/* since default is integer */

                          tmp = $3;

                          while (tmp)
                            {
			      if ((tmp->left == NULL) && (tmp->left == NULL))
                                {				
                                  symtab_funct_var_insert($1, VOID, 0);
				  break;
				}
			      else 
				{
				  if (tmp->left->right->nodeval == POINTER)
				    symtab_funct_var_insert($1, tmp->left->left->nodeval, 1);
				  else
  				    symtab_funct_var_insert($1, tmp->left->left->nodeval, 0);
				}
			      tmp = tmp->right;
			    }			
                        }
                     ;

parm_type_list:     VOID 
                      { 
                        scope++;
                        $$ = mknode(PARM_TYPE_LIST, NULL, NULL); 
                      } 
                    | parm_list 
                      { 
                        scope++;
                        $$ = $1;
                      }
                     ;

parm_list:          parm_list ',' parm_decl { $$ = mknode(PARM_TYPE_LIST, $3, $1); }
                     | parm_decl { $$ = mknode(PARM_TYPE_LIST, $1, NULL); }
                     ;

parm_decl:          type_name ID 
                      { 
                        scope++;

			switch ($1->nodeval)
			{
			  case FLOAT:
			    size = FLOAT_SIZE;
			    break;
			  case INT:
			    size = INT_SIZE;
			    break;
			  case CHAR:
			    size = CHAR_SIZE;
			    break;
			}

                        symtab_var_insert($1->nodeval, scope, $2, size, 0, 0, 0);
                        $$ = mknode(PARM_DECL, $1, mkleaf(ID,$2));
                        scope--;
		      } 
                     | type_name '*' ID 
                      {
                        scope++;

			switch ($1->nodeval)
			{
			  case FLOAT:
			    size = FLOAT_SIZE;
			    break;
			  case INT:
			    size = INT_SIZE;
			    break;
			  case CHAR:
			    size = CHAR_SIZE;
			    break;
			}

                        symtab_var_insert($1->nodeval, scope, $3, size, 0, 0, 1);
			$$ = mknode(PARM_DECL, $1, mknode(POINTER, mkleaf(ID,$3), NULL)); 
		        scope--;
                      }
                     ;

function_body:      internal_decls statement_list { $$ = mknode(FUNCTION_BODY, $1, $2); }
                     ;

internal_decls:     declaration internal_decls { $$ = mknode(INTERNAL_DECLS, $1, $2); } 
                     | { $$ = (struct nodetype *)NULL; }
                     ;

statement_list:     statement statement_list { $$ = mknode(STATEMENT_LIST, $1, $2); }
                     | { $$ = (struct nodetype *)NULL; }
                     ;

statement:          compound_stmt { $$ = $1; } 
                     | null_stmt { $$ = $1; }
                     | expression_stmt { $$ = $1; }
                     | if_stmt { $$ = $1; }
                     | for_stmt { $$ = $1; }
                     | while_stmt { $$ = $1; }
                     | dowhile_stmt { $$ = $1; }
                     | return_stmt { $$ = $1; }
                     ;

compound_stmt:      '{' statement_list '}' { $$ = $2; }
                     ;

null_stmt:          ';' { $$ = NULL; }
                    ;

expression_stmt:    expression ';' { $$ = $1; }
                     ;

if_stmt:            IF '(' expression ')' statement  %prec LOWER_THAN_ELSE
                        {
			  $$ = mknode(IF, $3, $5);
	                }
                    | IF '(' expression ')' statement ELSE statement
                        {
			  $$ = mknode(IF, $3, mknode(ELSE, $5, $7));
			}
                     ;


for_stmt:           FOR '(' expression ';' expression ';' expression ')' statement 
                     { 
		       $$ = mknode(FOR, $3, 
			 mknode(FOR2, $5,
			   mknode(FOR3, $7, $9)));
		     }
                     ;

while_stmt:         WHILE '(' expression ')' statement { $$ = mknode(WHILE, $3, $5); }
                     ;

dowhile_stmt:       DO statement WHILE '(' expression ')' ';' { $$ = mknode(DO, $2, $5); }
                     ;

return_stmt:        RETURN expression ';' 
                       { 
                         $$ = mknode(RETURN, $2, NULL); 
                         if ($2->type != funct_return_type)
                           {
			     fprintf(stderr,"function definition and return type do not match" 
				     "%d != %d\n", $2->type, funct_return_type);
			     exit(1);
			   }
/* take care of pointers */
                       }
                     | RETURN ';' 
                        { 
                          $$ = mknode(RETURN, NULL, NULL); 
                          if (funct_return_type != VOID)
                            {
                              fprintf(stderr,"function should return a value\n");
			      exit(1);
			    }
                        } 
                     ;

expression:         assignment_expr 
                       { 
                         $$ = mknode(EXPRESSION, $1, NULL); 
                         $$->type = $1->type;
                         $$->ispointer = $1->ispointer;
                         generate_temp($$);
                       }
                     ;

assignment_expr:    unary_expr '=' expression 
                      { 
                        returned = parse_expression(ASSIGNMENT,&$1,&$3);
                        $$ = mknode(ASSIGNMENT, $1, $3);
  
                        if (returned != NULL)
                          {
                            $$->ispointer = returned->is_pointer;
                            $$->type = returned->rttype;
			    generate_temp($$);
			  }
                        else
                          {
			    fprintf(stderr,"parse_expression failed!\n");
			    exit(1);
                          }
                      }
                     | binary_expr { $$ = $1; }
                     ;

binary_expr:        binary_expr AND_OP unary_expr 
                      { 
                        returned = parse_expression(AND_OP,&$1,&$3);
                        $$ = mknode(AND_OP, $1, $3);

                        if (returned != NULL)
                          {
                            $$->ispointer = returned->is_pointer;
                            $$->type = returned->rttype;
			    generate_temp($$);
                          }
                        else
                          {
			    fprintf(stderr,"parse_expression failed!\n");
			    exit(1);
                          }
                      } 
                     | binary_expr OR_OP unary_expr 
                        { 
                        returned = parse_expression(OR_OP,&$1,&$3);
                          $$ = mknode(OR_OP, $1, $3); 
                        if (returned != NULL)
                          {
                            $$->ispointer = returned->is_pointer;
                            $$->type = returned->rttype;
			    generate_temp($$);
                          }
                        else
                          {
                            $$->ispointer = -1;
                            $$->type      = -1;
                          }
                        }
                     | binary_expr EQ_OP unary_expr 
                        { 
                        returned = parse_expression(EQ_OP,&$1,&$3);
                          $$ = mknode(EQ_OP, $1, $3); 
                        if (returned != NULL)
                          {
                            $$->ispointer = returned->is_pointer;
                            $$->type = returned->rttype;
			    generate_temp($$);
                          }
                        else
                          {
                            $$->ispointer = -1;
                            $$->type      = -1;
                          }
                        }
                     | binary_expr NE_OP unary_expr 
                        { 
                        returned = parse_expression(NE_OP,&$1,&$3);
                        $$ = mknode(NE_OP, $1, $3); 
                        if (returned != NULL)
                          {
                            $$->ispointer = returned->is_pointer;
                            $$->type = returned->rttype;
			    generate_temp($$);
                          }
                        else
                          {
                            $$->ispointer = -1;
                            $$->type      = -1;
                          }
                        }
                     | binary_expr '<' unary_expr 
                        { 
                        returned = parse_expression((int)'<',&$1,&$3);
                          $$ = mknode((int)'<', $1, $3); 
                        if (returned != NULL)
                          {
                            $$->ispointer = returned->is_pointer;
                            $$->type = returned->rttype;
			    generate_temp($$);
                          }
                        else
                          {
                            $$->ispointer = -1;
                            $$->type      = -1;
                          }
                        }
                     | binary_expr '>' unary_expr 
                        { 
                        returned = parse_expression((int)'>',&$1,&$3);
                          $$ = mknode((int)'>', $1, $3); 
                        if (returned != NULL)
                          {
                            $$->ispointer = returned->is_pointer;
                            $$->type = returned->rttype;
			    generate_temp($$);
                          }
                        else
                          {
                            $$->ispointer = -1;
                            $$->type      = -1;
                          }
                        }
                     | binary_expr LE_OP unary_expr 
                        { 
                        returned = parse_expression(LE_OP,&$1,&$3);
                         $$ = mknode(LE_OP, $1, $3); 
                        if (returned != NULL)
                          {
                            $$->ispointer = returned->is_pointer;
                            $$->type = returned->rttype;
			    generate_temp($$);
                          }
                        else
                          {
                            $$->ispointer = -1;
                            $$->type      = -1;
                          }
                        }
                     | binary_expr GE_OP unary_expr 
                        { 
                        returned = parse_expression(GE_OP,&$1,&$3);
                          $$ = mknode(GE_OP, $1, $3); 
                        if (returned != NULL)
                          {
                            $$->ispointer = returned->is_pointer;
                            $$->type = returned->rttype;
			    generate_temp($$);
                          }
                        else
                          {
                            $$->ispointer = -1;
                            $$->type      = -1;
                          }
                        }
                     | binary_expr '+' unary_expr 
                        { 
                        returned = parse_expression((int)'+',&$1,&$3);
                          $$ = mknode((int)'+', $1, $3); 
                        if (returned != NULL)
                          {
                            $$->ispointer = returned->is_pointer;
                            $$->type = returned->rttype;
			    generate_temp($$);
                          }
                        else
                          {
                            $$->ispointer = -1;
                            $$->type      = -1;
                          }
                        }
                     | binary_expr '-' unary_expr 
                        { 
                        returned = parse_expression((int)'-',&$1,&$3);
                          $$ = mknode((int)'-', $1, $3); 
                        if (returned != NULL)
                          {
                            $$->ispointer = returned->is_pointer;
                            $$->type = returned->rttype;	
			    generate_temp($$);
                          }
                        else
                          {
                            $$->ispointer = -1;
                            $$->type      = -1;
                          }
                        }
                     | binary_expr '*' unary_expr 
                        { 
                        returned = parse_expression((int)'*',&$1,&$3);
                          $$ = mknode((int)'*', $1, $3); 
                        if (returned != NULL)
                          {
                            $$->ispointer = returned->is_pointer;
                            $$->type = returned->rttype;
			    generate_temp($$);
                          }
                        else
                          {
                            $$->ispointer = -1;
                            $$->type      = -1;
                          }
                        }
                     | binary_expr '/' unary_expr 
                        { 
                        returned = parse_expression((int)'/',&$1,&$3);
                          $$ = mknode((int)'/', $1, $3); 
                        if (returned != NULL)
                          {
                            $$->ispointer = returned->is_pointer;
                            $$->type = returned->rttype;
			    generate_temp($$);
                          }
                        else
                          {
                            $$->ispointer = -1;
                            $$->type      = -1;
                          }
                        }
                     | binary_expr '%' unary_expr 
                        { 
                        returned = parse_expression((int)'%',&$1,&$3);
                          $$ = mknode((int)'%', $1, $3); 
                        if (returned != NULL)
                          {
                            $$->ispointer = returned->is_pointer;
                            $$->type = returned->rttype;
			    generate_temp($$);
                          }
                        else
                          {
                            $$->ispointer = -1;
                            $$->type      = -1;
                          }
                        }
                     | unary_expr { $$ = $1; }
                     ;

unary_expr:         '!' unary_expr 
                      { 
                        $$ = mknode(UNARY_EXPR, mknode((int)'!',$2,NULL), NULL); 
                        $$->type = $2->type;
                        generate_temp($$);
                        generate_temp($$->left);

                        if ($2->ispointer)
                          {
			    fprintf(stderr,"unary '!' operation invalid on pointer at line\n");
			    exit(1);
			  }
                      }
                     | '+' unary_expr 
                      { 
                        $$ = mknode(UNARY_EXPR, mknode((int)'+',$2,NULL), NULL);
                        $$->type = $$->left->type = $2->type;
                        generate_temp($$);
                        generate_temp($$->left);


                        if ($2->ispointer)
                          {
			    fprintf(stderr,"unary '+' operation invalid on pointer at line\n");
			    exit(1);
			  }
                      }
                     | '-' unary_expr 
                      { 
                        $$ = mknode(UNARY_EXPR, mknode((int)'-',$2,NULL), NULL); 
                        $$->type = $$->left->type = $2->type;
                        generate_temp($$);
                        generate_temp($$->left);

                        if ($2->ispointer)
                          {
			    fprintf(stderr,"unary '-' operation invalid on pointer at line\n");
			    exit(1);
			  }
                      }
                     | INC_OP unary_expr 
                      { 
                        $$ = mknode(UNARY_EXPR, mknode(INC_OP,$2,NULL), NULL); 
                        $$->type = $2->type;
                        $$->ispointer = $2->ispointer;
                        generate_temp($$);
                        generate_temp($$->left);

                      } 
                     | DEC_OP unary_expr 
                      { 
                        $$ = mknode(UNARY_EXPR, mknode(DEC_OP,$2,NULL), NULL); 
                        $$->type = $2->type;
                        $$->ispointer = $2->ispointer;
                        generate_temp($$);
                        generate_temp($$->left);

                      }
                     | '&' unary_expr 
                      { 
                        $$ = mknode(UNARY_EXPR, mknode((int)'&',$2,NULL), NULL); 
                        $$->type = $$->left->type = $2->type;
                        $$->ispointer = 1;
                        generate_temp($$);
                        generate_temp($$->left);

                      }
                     | '*' unary_expr 
                      { 
                        $$ = mknode(UNARY_EXPR, mknode((int)'*',$2,NULL), NULL); 
                        if (!$2->ispointer)
                          {
			    fprintf(stderr,"Dereferencing non-pointer at line\n");
			    exit(1);
			  }
                        $$->type = $$->left->type = $2->type;
                        $$->ispointer = 0;
                        generate_temp($$->left);
                        generate_temp($$);

                      }
                     | postfix_expr { $$ = $1; }
                    ;

postfix_expr:       postfix_expr '[' expression ']' 
                       {  /* converting into pointer arithmetic */
                          $$ = mknode(EXPRESSION, mknode(UNARY_EXPR, mknode((int)'*', \
				      mknode(EXPRESSION,mknode((int)'+',$3, $1), NULL), NULL), \
				      NULL), NULL);
                          
                          returned = parse_expression(EXPRESSION, &$$->left, &$$->right);
 
                          if (returned != NULL)
                          {
                            $$->ispointer = returned->is_pointer;
                            $$->type = returned->rttype;
			    generate_temp($$);
                          }
                          else
                          {
                            $$->ispointer = -1;
                            $$->type      = -1;
			    fprintf(stderr,"returned -1 is postfix expression in arrays!\n");
			  }

                          $$->type = $1->type;
                          $$->ispointer = $1->ispointer;
                          generate_temp($$);
/*                        $$ = mknode(ARRAY_DECL, $1, $3);  */
                       } 
                     | ID '(' argument_list ')' 
                       { 
			 $$ = mknode(FUNCTION_CALL, mkleaf(ID,$1), $3); 
                         tptr = symtab_lookup($1);
                         if (tptr == NULL)
                           {
			     fprintf(stderr,"Function %s called before it was declared\n",$1);
			     exit(1);
			   }
                         if (tptr->symbol_type != SYM_FUNCTION)
                           {
			     fprintf(stderr,"Variable name %s is type %s\n",
				    ($1,tptr->symbol_type == 1) ? "VARIABLE" : "CONSTANT");
			   }
                         $$->type = tptr->u.function.returns;
                         generate_temp($$);
		       } 
                     | ID '(' ')' 
                       { 
			 $$ = mknode(FUNCTION_CALL, mkleaf(ID,$1), NULL); 

                         tptr = symtab_lookup($1);
                         if (tptr == NULL)
                           {
			     fprintf(stderr,"Function %s called before it was declared\n");
			   }
                         if (tptr->symbol_type != SYM_FUNCTION)
                           {
			     fprintf(stderr,"Variable name %s is type %s\n",
				    ($1,tptr->symbol_type == 1) ? "VARIABLE" : "CONSTANT");
			   }
                         $$->type = tptr->u.function.returns;
                         generate_temp($$);
		       }
                     | postfix_expr INC_OP 
                       { 
                         $$ = mknode(INC_OP, NULL, $1); 
                         $$->type = $1->type;
                         $$->ispointer = $1->ispointer;
                         generate_temp($$);
                       }
                     | postfix_expr DEC_OP 
                       { 
                         $$ = mknode(DEC_OP, NULL, $1); 
                         $$->type = $1->type;
                         $$->ispointer = $1->ispointer;
                       }
                     | primary_expr { $$ = $1; }
                     ;

primary_expr:        INTEGER_VALUE 
                      { 
			$$ = mkleaf(INTEGER_VALUE, $1);
			symtab_const_insert(INTEGER_VALUE, $1);
                        $$->type = INT;
                        $$->ispointer = 0;
		      } 
                     | FLOAT_VALUE 
                      { 
			$$ = mkleaf(FLOAT_VALUE, $1);
			symtab_const_insert(FLOAT_VALUE, $1);
                        $$->type = FLOAT;
                        $$->ispointer = 0;
		      }
                     | CHAR_VALUE 
                      { 
			$$ = mkleaf(CHAR_VALUE, $1);
			symtab_const_insert(CHAR_VALUE, $1);
                        $$->type = CHAR;
                        $$->ispointer = 0;
		      }
                     | STRING_VALUE 
                      { 
			$$ = mkleaf(STRING_VALUE, $1);
			symtab_const_insert(STRING_VALUE, $1);
                        $$->type = CHAR;
                        $$->ispointer = 1;
		      } 
                     | ID 
                      { 
			$$ = mkleaf(ID, $1);
                        
                        tptr = symtab_lookup($1);
                        if (tptr == NULL)
                          {
			    fprintf(stderr,"Variable used before declared %s\n",$1);
			  }
                        if (tptr->symbol_type != SYM_VARIABLE)
                          {
			    fprintf(stderr,"Variable name %s is type %s\n",
				    ($1,tptr->symbol_type == 2) ? "CONSTANT" : "FUNCTION");
			  }
                        
                        if (tptr->u.variable.ispointer)
                          $$->ispointer = 1;
                        else
                          $$->ispointer = 0;
                        
                        $$->type = tptr->u.variable.type;
		      } 
                     | '(' expression ')' 
                      { 
                        $$ = $2; 
                      }
                     ;

argument_list:      argument_list ',' expression { $$ = mknode(ARGUMENT_LIST, $3, $1); } 
                     | expression { $$ = $1; }
                     ;

%%







