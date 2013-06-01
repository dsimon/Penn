DIGIT        [0-9]
LETTER       [A-Za-z]
HEX_INT      0[X|x][0-9A-Fa-f]+
OCTAL_INT    0(0-7)*
DECIMAL_INT  [0-9]+
REAL         [0-9]*\.[0-9]*([E|e][+\-]?[0-9]+)?
EXPONENT     [Ee][+\-]?{DIGIT}*
WHITESPACE   [\t\n\f ]
ID           {LETTER}({LETTER}|{DIGIT}|_)*

%{
/* lexical analyzer for CS compiler */
/* Derron Simon & Douglas Bellew */

/* lexically matches all tokens in the CS language */

#include <stdio.h>
#include "proto.h"
#include "globals.h"
#include "y.tab.h"
#include "string.h"

void remove_comment();

int debug_token_listing = 0;
int debug_typecheck = 0;
int yylineno = 1;

%}

%%

"\n"         { yylineno++; }

<<EOF>>      { return( EOF ); }

{WHITESPACE} { /* do nothing */ }

"/*debug(token_listing_on)"      { 
                                     fflush(yyout);
                                     fprintf(stderr,"debug: token_listing_on\n");
                                     debug_token_listing = 1; 
                                     remove_comment(); 
                                 }
"/*debug(token_listing_off)"     { 
                                     fflush(yyout);
                                     fprintf(stderr,"debug: token_listing_off\n");
                                     debug_token_listing = 0; 
                                     remove_comment(); 
                                 }
"/*debug(symtab_stats)"          { 
                                     fflush(yyout);
                                     fprintf(stderr,"debug: symtab_stats\n");
                                     debug_symtab_stats(); 
                                     remove_comment(); 
                                 }
"/*debug(symtab_dump)"           { 
                                     fflush(yyout);
                                     fprintf(stderr,"debug: symtab_dump\n");
                                     debug_symtab_dump(); 
                                     remove_comment(); 
                                 }
"/*debug(halt)"                  {   
                                     fflush(yyout);
                               	     fprintf(stderr,"debug: halt\n");
                                     exit(255);
                                 }
"/*debug(statement_dump)"        {
                                      fflush(yyout);
                                      fprintf(stderr,"debug: statement dump\n");
                                      printtree(statement_tree);
                                      remove_comment();
                                 }
"/*debug(variable_dump)"	 {
				      fflush(yyout);
				      fprintf(stderr,"debug: variable dump\n");
				      debug_variable_dump();
				      remove_comment();
				 } 

"/*debug(trace_types_on)"        {
                                      fflush(yyout);
                                      fprintf(stderr,"debug: trace types on\n");
                                      debug_typecheck = 1;
                                      remove_comment();
                                 }
"/*debug(trace_types_off)"       {
                                      fflush(yyout);
                                      fprintf(stderr,"debug: trace types off\n");
                                      debug_typecheck = 0;
                                      remove_comment();
                                 }

"/*"         { remove_comment( ); }

"char"       { return( CHAR );
               if (debug_token_listing)
                   fprintf(stderr,"Token(char) "); }
"do"         { return( DO );  
               if (debug_token_listing)
                   fprintf(stderr,"Token(do) "); }
"extern"     { return( EXTERN ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(extern) "); }
"float"      { return( FLOAT );
               if (debug_token_listing)
                   fprintf(stderr,"Token(float) "); }
"for"        { return( FOR ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(for) "); }
"if"         { return( IF ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(if) "); }
"int"        { return( INT ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(int) "); }
"register"   { return( REGISTER ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(register) "); }
"return"     { return( RETURN ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(return) "); }
"void"       { return( VOID ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(void) "); }
"while"      { return( WHILE ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(while) "); }

"&&"         { return( AND_OP ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(&&) "); }
"||"         { return( OR_OP ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(||) "); }
"++"         { return( INC_OP ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(++) "); }
"--"         { return( DEC_OP ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(--) "); }
"=="         { return( EQ_OP ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(==) "); }
"!="         { return( NE_OP );
               if (debug_token_listing)
                   fprintf(stderr,"Token(!=) "); }
"<="         { return( LE_OP ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(<=) "); }
">="         { return( GE_OP ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(>=) "); }

";"          { return( ';' ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(;) "); }
","          { return( ',' ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(,) "); }
":"          { return( ':' ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(:) "); }
"&"          { return( '&' ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(&) "); }
"="          { return( '=' ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(=) "); }
"+"          { return( '+' ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(+) "); }
"-"          { return( '-' ); 
               if (debug_token_listing)
                   fprintf(stderr,"Token(-) "); }
"*"          { return( '*' );
               if (debug_token_listing)
                   fprintf(stderr,"Token(*) "); }
"/"          { return( '/' );
               if (debug_token_listing)
                   fprintf(stderr,"Token(/) "); }
"%"          { return( '%' );
               if (debug_token_listing)
                   fprintf(stderr,"Token(%%) "); }
"<"          { return( '<' );
               if (debug_token_listing)
                   fprintf(stderr,"Token(<) "); }
">"          { return( '>' );
               if (debug_token_listing)
                   fprintf(stderr,"Token(>) "); }
"("          { return( '(' );
               if (debug_token_listing)
                   fprintf(stderr,"Token(() "); }
")"          { return( ')' );
               if (debug_token_listing)
                   fprintf(stderr,"Token()) "); }
"{"          { return( '{' );
               if (debug_token_listing)
                   fprintf(stderr,"Token({) "); }
"}"          { return( '}' );
               if (debug_token_listing)
                   fprintf(stderr,"Token(}) "); }
"["          { return( '[' );
               if (debug_token_listing)
                   fprintf(stderr,"Token([) "); }
"]"          { return( ']' );
               if (debug_token_listing)
                   fprintf(stderr,"Token(]) "); }
"!"          { return( '!' );
               if (debug_token_listing)
                   fprintf(stderr,"Token(!) "); }

{REAL}         { yylval.stringval = strdup(yytext);
                 if (debug_token_listing)
                   fprintf(stderr,"Token(FLOAT_VALUE) ");
                 return( FLOAT_VALUE ); 
               }
{HEX_INT}      { yylval.stringval = strdup(yytext);
                 if (debug_token_listing)
                   fprintf(stderr,"Token(INTEGER_VALUE) ");
                 return( INTEGER_VALUE ); 
               }
{OCTAL_INT}    { yylval.stringval = strdup(yytext);
                 if (debug_token_listing)
                   fprintf(stderr,"Token(INTEGER_VALUE) ");
                 return( INTEGER_VALUE ); 
               }
{DECIMAL_INT}  { yylval.stringval = strdup(yytext);
                 if (debug_token_listing)
                   fprintf(stderr,"Token(INTEGER_VALUE) ");
                 return( INTEGER_VALUE ); 
               }
'(\\.|[^\\'])+'     { yylval.stringval = strdup(yytext);
                      if (debug_token_listing)
                        fprintf(stderr,"Token(CHAR_VALUE) ");
                      return( CHAR_VALUE ); 
                    }
\"(\\.|[^\\"])*\"   {  yylval.stringval = strdup(yytext);
                       if (debug_token_listing)
                         fprintf(stderr,"Token(STRING_VALUE) ");
                       return( STRING_VALUE ); 
                    }

{ID}          {  yylval.stringval = strdup(yytext);
                 if (debug_token_listing)
                   fprintf(stderr,"Token(ID) ");
                 return( ID ); }

.            { return( ERROR ); }

%%

void remove_comment()
{
     char c;     

     while(1)
     {
          while ((c = input()) != '*') 
               ;
          c = input();
          if (c == '/')
               return;
          else
               ungetc(c,yyin);
     }
}











