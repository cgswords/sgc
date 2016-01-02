#include <stdio.h>
#include <stdlib.h>
#include "tags.h"

/* 
   This library will hopefully be the minimal subset that the assembly 
   calls off to in order to support exposed user functions
*/

void rt_error(char *m){
  puts(m);
  exit(-1);
}

void rt_exit(PTR n){
  exit(UNFIX(n));
}

