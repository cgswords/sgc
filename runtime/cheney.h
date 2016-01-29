#ifndef CHENEY_H
#define CHENEY_H
#include "tags.h"



ptr* guarded_area(long n);
ptr* mallocHeap(void);
void setHeap(ptr* newHeap);
void freeHeap(void);

void forward(ptr* start, ptr **stop); 
ptr  forward_pair(rt_ptr p);
ptr  forward_procedure(rt_ptr p);
ptr  forward_vector(rt_ptr p);



#endif
