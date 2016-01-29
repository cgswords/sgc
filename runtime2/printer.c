#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <signal.h>
#include <ucontext.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include "tags.h"
#include "printer.h"

#define _XOPEN_SOURCE 700
#define SCHEME_PRINTER

#ifdef SCHEME_PRINTER

#define MAXDEPTH 100
#define MAXLENGTH 1000

void fprint1(FILE *fp, ptr x, int d) {
  if (TAG(x, mask_fixnum) == tag_fixnum) {
    fprintf(fp, "%ld", (long)UNFIX(x));
  } else if (TAG(x, mask_pair) == tag_pair) {
    int len = 0;
    ptr y;
    
    if (d > MAXDEPTH) {
      fprintf(fp, "(...)");
      return;
    }
    fprintf(fp, "(");
    fprint1(fp, CAR(x), d+1);
    y = CDR(x);
    while (TAG(y, mask_pair) == tag_pair && (len < MAXLENGTH-1)) {
      fprintf(fp, " ");
      fprint1(fp, CAR(y), d+1);
      y = CDR(y);
      len++;
    }
    if (y != _nil){
      if (len == MAXLENGTH-1)
        fprintf(fp, " ...");
      else {
        fprintf(fp, " . ");
        fprint1(fp, y, d+1);
      }
    }
    fprintf(fp, ")");
  } else if (TAG(x, mask_vector) == tag_vector) {
    long i, n;
    ptr *p;
    if (d > MAXDEPTH) {
      fprintf(fp, "#(...)");
      return;
    }
    fprintf(fp, "#(");
    n = UNFIX(VECTORLENGTH(x));
    p = (ptr *)VECTORDATA(x);
    i = n > MAXLENGTH ? MAXLENGTH : n;
    if (i != 0) {
      fprint1(fp, *p, d+1);
      while (--i) {
        fprintf(fp, " ");
        fprint1(fp, *++p, d+1);
      }
    }
    if (n > MAXLENGTH) fprintf(fp, " ..."); 
    fprintf(fp, ")");
  } else if (TAG(x, mask_procedure) == tag_procedure)   {
    long i, n, max;
    ptr *p;
    if (d > MAXDEPTH) {
      fprintf(fp, "#<procedure>");
      return;
    }
    fprintf(fp, "#<procedure ");
    n = UNFIX( PROCLENGTH(x) );
    fprintf(fp, "fc: %ld fv: ", n);
    p = PROCDATA(x);
    max = n > MAXLENGTH ? MAXLENGTH : n;
    for (i = 0; i < max; i++) {
      if (TAG(p[i], mask_procedure)) {
        fprintf(fp," #<proc>");
      }
      else { 
        fprintf(fp," ");
        fprint1(fp, p[i], d+1);
      }
    }
    if (n > MAXLENGTH) fprintf(fp, " ..."); 
    fprintf(fp, ">");
  } else if (x == _contmark) {
    fprintf(fp, "#<continuation mark>");
  } else if (x == _false) {
    fprintf(fp, "#f");
  } else if (x == _true) {
    fprintf(fp, "#t");
  } else if (x == _nil) {
    fprintf(fp, "()");
  } else if (x == _void) {
    fprintf(fp, "#<void>");
  } else {
    fprintf(fp, "print (runtime.c): invalid ptr #x%x\n", (unsigned int) x);
    exit(1);
  }
  fflush(fp);
}

void print1(ptr x, int d) {
  fprint1(stdout, x, d);
}

void fprint(FILE *fp, ptr x) {
  fprint1(fp, x, 0);
}

void print_mem(FILE *fp, ptr* head, ptr* base) {
  fprintf(fp, "+-----PRINTING MEMORY-------------------------------------------------+\n");
  fprintf(fp, "| Memory head: %p \tbase: %p                  |\n", (void*)head, (void*)base); 
  fprintf(fp, "+---------------------------------------------------------------------+\n");
  fprintf(fp, "| Addr\t\t | Value: \t\t\t\t\t      |\n");
  fprintf(fp, "+----------------+----------------------------------------------------+\n");

  while(head > base){
    print_mem_entry(fp, head);
    --head;
  }
  fprintf(fp, "+----------------+----------------------------------------------------+\n");
  
  return;
}

void print_mem_entry(FILE *fp, ptr *loc) {
  fprintf(fp, "  ");
  if (TAG(*loc, mask_pair) == tag_pair) {
      fprintf(fp, "%p | #<pair %p> -> ", (void*)loc, (void*)UNTAG(*loc, mask_pair));
      fprint(fp, *loc);
      fprintf(fp, "\n");
  } else if (TAG(*loc, mask_vector) == tag_vector) {
    fprintf(fp, "%p | #<vector %p> -> ", (void*) loc, (void*) UNTAG(*loc, mask_vector));
    fprint(fp, *loc);
    fprintf(fp, "\n"); 
  } else if (TAG(*loc, mask_procedure) == tag_procedure) {
    fprintf(fp, "%p | #<procedure %p - %p> -> ", (void*) loc, (void*)*loc, (void*) PROCCODE(*loc));
    fprintf(fp, "\n                 |   ");
    fprint(fp, *loc);
    fprintf(fp, "\n");
  } else if (TAG(*loc , mask_contmark) == tag_contmark) {
    fprintf(fp, "=======================================\n");
  } else if (TAG(*loc , mask_fixnum) == tag_fixnum) {
    fprintf(fp, "%p | #<fixnum %ld>\n", (void*) loc, UNFIX(*loc));
  } else if (*loc == _false) {
    fprintf(fp, "%p | #f\n", (void*) loc);
  } else if (*loc == _true) {
    fprintf(fp, "%p | #t\n", (void*) loc);
  } else if (*loc == _nil) {
    fprintf(fp, "%p | '()\n", (void*) loc);
  } else if (*loc == _void) {
    fprintf(fp, "%p | #<void>\n", (void*) loc);
  } else {
    fprintf(fp, "%p | %lX\n", (void*) loc, *loc);
  }
  fflush(fp);
}

void print(ptr x) {
  print1(x, 0);
}

#else /* SCHEME_PRINTER */

void print(long x) {
    printf("%ld", x);
} 

#endif /* SCHEME_PRINTER */


