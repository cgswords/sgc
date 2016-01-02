#ifndef COLLECTOR_H
#define COLLECTOR_H

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

typedef struct {
  ptr *heap;
  ptr *hp;
  ptr *limit;
  ptr *stack;
  ptr *sp;
} memModel;

void gc_init(void); 
void gc_cleanup(void);
ptr* getStack(void); 
ptr* getHeap(void);
ptr* getLimit(void);
void checkAddr(ptr* addr);

/* The interface for the generated code */
memModel *mem;
ptr* collect(ptr* sp, ptr* ap, long size);

#endif //COLLECTOR_H
