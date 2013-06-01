extern int debug_token_listing;
extern int scope;
extern int funct_return_type;

#define SYM_VARIABLE    1
#define SYM_CONSTANT    2
#define SYM_FUNCTION    3

#define INT_SIZE        4
#define FLOAT_SIZE      4
#define CHAR_SIZE       1
#define POINTER_SIZE    4

struct funct_parms
{
  int type;
  int ispointer;
  struct funct_parms *next;
};

struct symtab_type {
  int exist;
  int scope;
  char *name;
  int symbol_type;

  union u_type {
    struct var_type {
      int size;
      int type;
      int dimension;
      int isextern;
      int ispointer;
    } variable;
    struct const_type {
      int lexval;
    } constant;
    struct funct_type {
      int returns;
      int isextern;
      int numparms;
      struct funct_parms *parm;
    } function;
  } u;
  
  struct symtab_type *next;
};

struct nodetype
{
  struct nodetype *left;
  struct nodetype *right;
  int nodeval;
  int type;
  int ispointer;
  char *tempvar;
  char *name;
  int nodeleaf; /* true if it's a leaf */
};

extern struct nodetype *statement_tree;
