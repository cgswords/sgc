#ifndef COLLECTOR_H
#define COLLECTOR_H
#include "tags.h"

typedef struct {
  ptr *heap;
  ptr *hp;
  ptr *limit;
  ptr *stack;
  ptr *sp;
} memModel;

/* This is the interface that the runtime is allowed to use */
void gc_init(void); 
void gc_cleanup(void); 
ptr* getStack(void); 
ptr* getHeap(void);
ptr* getLimit(void);
void checkAddr(ptr* addr);

/* This is the interface for the compiled code */
memModel *mem;
ptr* collect(ptr *sp, ptr *ap, long size);

#endif
