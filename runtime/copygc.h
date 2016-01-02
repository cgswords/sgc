#ifndef COPYGC_H
#define COPYGC_H

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
#include "collector.h"
#include "stack.h"
#include "printer.h"
#include "tags.h"

typedef struct {
  // The first fields must be identical to those exposed by collector.h
  // in the memModel
  ptr *heap;     
  ptr *hp;
  ptr *limit;
  ptr *stack;
  ptr *sp;
  ptr *tospace;
  ptr *next;
  long pagesize;
  long heapsize;
  long stacksize;
  FILE *log;
} gcModel;

node *scan_stack(ptr* head, ptr* base, node* live_set); 

long inBounds (ptr *base, long size, long addr); 

long constant(ptr val); 

ptr *copyCollect (node *live_set, ptr *heap, long heapsize, ptr *tospace); 

ptr *gc(ptr *stack, ptr *heap, ptr *sp, ptr *hp, ptr *limit, long heapsize, long pagesize);
  
#endif
