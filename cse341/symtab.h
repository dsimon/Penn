/* Symtab.h
 * This contains all the variable declerations for the "symtab.c" file
 * Derron Simon & Doug Bellew's Spring 93 cse341 compiler
 */

#define PRIME 211
struct symtab_type *bucket[PRIME];

int symnum_lookups;
int symnum_items_searched;
int symnum_ids;
int symnum_int_consts;
int symnum_char_consts;
int symnum_string_consts;
int symnum_float_consts;
int symnum_items_table;
int symnum_lookups;



