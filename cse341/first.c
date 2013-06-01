/* null.c
 *
 * This is a 20 point exercise.  There will be no penalty for submitting
 * it late, but you must submit correct output for this exercise to get
 * credit.
 *
 * You are expected to generate:
 *	  i. Declarations for the external variables
 *	 ii. A .global declaration for the entry point
 *	iii. Appropriate code to adjust the stack upon entry, taking into
 *	     account the space needed for automatic variables
 *	 iv. A ret/return instruction pair
 *
 * In fact, just run gcc -S on it, and that's [very] roughly what you're
 * expected to produce.
 * 
 */

int a,c;		/* Some simple definitions */
char b,e,g;
char *h;

extern int shared;	/* Declare, but don't define */

int array[64];		/* Allocate pointer, and array */
char string[16];

void null(int i,char k)
  {
    int l;
    char array2[128];
    int m,n,o;
    char *p,q;

    return;
  }


