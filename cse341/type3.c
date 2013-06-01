/*debug(trace_types_on)*/

void foo(void);
int bar(int i,float f,char c);
float baz(int *ip,float *fp,char *cp);
char qux(int i,float f);

int quux[32];
float corge[64];
char grault[128];

float garply(int ivar,float fvar,char cvar,int *ipvar,float *fpvar)
  { int i,j,k,l,m,n;
    float f,g,h;
    char a,b;

    /* 1. (c*i)->((c->i)*i)
	  (i*f)->((i->f)*f)
	  (i=f)->(i=(f->i))->i
	  (c=i)->(c=(i->c))->i
     */

    cvar = ivar = 'x' * 1 * 3.5;
    cvar = ivar = cvar * ivar * fvar;
    cvar = ivar = ivar * cvar * fvar;

    /* 2. A-OK
     */

    foo();   

    /* 3. (i,f,c)\(c,i,f)->(i,f,c)\((c->i),(i->f),(f->c))
	  and result is ignored.
     */

    bar('x',3,32.0);

    /* 4. A-OK
     */

    if ((0.5 && i) - 1)
      { /* 4.1 (i->f)
         */
        return 0;
      }
    else
      { /* 4.2 (c-c)->((c->i)-(c->i))->(((c->i)-(c->i))->f)
	 */
	return '0'-'0';
      }

    /* 5. A-OK
     */

    while(qux(bar(*quux,*corge,*grault),baz(&ivar,&fvar,&cvar)))
      { /* 5.1 A-OK
	 */

	ivar++;
	++fvar;
	++cvar++;

	/* 5.2 lhs: (*((fp+i)->fp))->i
	       rhs: (((&i)->ip)[]i)->i
	       (i=i)->i
	 */

	*(corge+34) = (&ivar)[34];
      }

    /* X. O-ton-o errors.  Catch at least one and halt.
     */

    return 1+2.0+foo(ipvar % 10.0);
  }
/*debug(trace_types_off)*/


