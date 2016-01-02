#ifndef COLLECTOR_H
#define COLLECTOR_H
#include "tags.h"

/* This is the interface that the runtime is allowed to use */
void gc_init(void); 
ptr* getStack(void); 
ptr* getHeap(void);
void checkAddr(ptr* addr);

/* This is the interface for the compiled code */
ptr* collect(ptr *sp, ptr *ap, long size);

#endif
