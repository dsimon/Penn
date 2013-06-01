#include "symtab.h"

 unsigned hash(char *s)
 {
   char *p = s;
   unsigned h = 0, g;
   while (*p) {
     h = ( h << 4) + *p++;
     if (g = h & 0xf0000000) {	
       h = h ^ (g >> 24);	
       h = h ^ g;		
    }
   }
   return h % PRIME;
 }
