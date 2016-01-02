#include <signal.h>
#include <ucontext.h>
#include <time.h>
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
#include "printer.h"
#include "tags.h"
#include "metrics.h"
#include "copygc.h"

/*
  gcModel is the internal book keeping that needs to be
  kept between collections.

  mem is the exposed fields of gcMem that the runtime is able
  to see.
  !!! They share the same memory bytes so are different views of the same data !!!
*/
static gcModel *model;
memModel *mem;


/* Returns a ptr that points to the location of the new object */
ptr forward_pair(rt_ptr p) {
  metric_pair_copied();
  /* Copy pair to next allocation point */
  rt_pair *n = (rt_pair*) (model->next);
  n->car = p->pair.car;
  n->cdr = p->pair.cdr;

  /* Set the forward pointer */
  p->fwd_ptr = (long) (model->next) + tag_pair;

  /* bump the allocation pointer */
  model->next = model->next + size_pair;
  return p->fwd_ptr;
}

ptr forward_procedure(rt_ptr p) {
  metric_procedure_copied();
  /* Copy procedure to next allocation point */
  rt_procedure *n = (rt_procedure*) (model->next);
  n->code   = p->procedure.code;
  n->length = p->procedure.length;
  
  int i;
  int length = UNFIX(p->procedure.length);
  for(i = 0; i < length; i++) n->data[i] = p->procedure.data[i];
  
  /* Set the forward pointer */
  ptr fwd_ptr = (long) (model->next) + tag_procedure; 
  p->fwd_ptr = fwd_ptr;

  /* bump the allocation pointer  code + length + data */
  model->next = model-> next + 2 + length;
  return fwd_ptr;
}

ptr forward_vector(rt_ptr p) {
  metric_vector_copied();
  /* Copy vector to next allocation point */
  rt_vector *n = (rt_vector*) (model->next);
  n->length = p->vector.length;
  
  int i;
  int length = UNFIX(p->vector.length);
  for(i = 0; i < length; i++) n->data[i] = p->vector.data[i];
  
  /* Set the forward pointer */
  ptr fwd_ptr = (long) (model->next) + tag_vector; 
  p->fwd_ptr = fwd_ptr;

  /* bump the allocation pointer length + data */
  model->next = model->next + 1 + length;
  return fwd_ptr;
}

int forwarded(rt_ptr p) {
  return (p->fwd_p >= model->tospace && p->fwd_p < model->limit);
}

/*
  The pointer to the pointer to the stop allows the algorithm
  to seamlessly update the stop point without having to pass
  stop around. As if this was a value that has been captured
  by a closure.
*/
void forward(ptr *scan, ptr **stop){
  for(;scan < *stop; scan++){
    switch (TAG(*scan, basic_ptr_tag_mask)) {
      case tag_fixnum:
        break;
      case tag_pair: {
          rt_ptr p = AS_RT_PTR(*scan);
          if (forwarded(p))
            *scan = p->fwd_ptr;
          else
            *scan = forward_pair(p); 
        }
        break;
      case tag_procedure : {
          rt_ptr p = AS_RT_PTR(*scan);
          if (forwarded(p)) 
            *scan = p->fwd_ptr;
          else
            *scan = forward_procedure(p); 
        }
        break;
      case tag_vector: {
          rt_ptr p = AS_RT_PTR(*scan);
          if (forwarded(p))
            *scan = p->fwd_ptr;
          else
            *scan = forward_vector(p); 
        }
        break;
      case tag_contmark:
        break;
      case tag_boolean:
        break;
      default : 
        printf ("TODO handle default case for pointer forwarding.\n");
        exit(EXIT_FAILURE);
    }
  }
}

ptr* getStack(void) {
  return model->stack;
}

void setHeap(ptr* newHeap) {
  model->heap = newHeap;
  model->limit = (ptr*) ((char*) newHeap + model->heapsize - 61440);
  return;
}


void freeHeap(void) {
  
  return;
}

ptr* getHeap(void) {
  return model->heap;
}

ptr* getLimit(void) {
  return model->limit;
}

/* allocate a chunk of memory with a guard page on either end */
static ptr *guarded_area(long n) {  /* n must be page aligned */
  char *addr;

 /* allocate, leaving room for guard pages */
  addr = (char *)mmap(NULL,
                      (size_t)(n + (2 * model->pagesize)),
                      PROT_READ | PROT_WRITE,
                      MAP_PRIVATE | MAP_ANON,
                      -1, 0);
  if (addr == (char *)-1) {
    fprintf(stderr, "mmap failed: %s\n", strerror(errno));
    exit(2);
  }

 /* remove access rights from the guard pages */
  if (mprotect(addr, (size_t)model->pagesize, PROT_NONE) ||
    mprotect(addr + model->pagesize + n, (size_t)model->pagesize, PROT_NONE)) {
    fprintf(stderr, "mprotect failed: %s\n", strerror(errno));
    exit(3);
  }

  return (ptr*) (addr + model->pagesize);
}

ptr *mallocHeap(void) {
  return guarded_area(model->heapsize);
}

void gc_init(void) {
  model = (gcModel*) malloc(sizeof(gcModel));
  mem = (memModel*) model;
  model->log = fopen("gc.log", "w");
  metrics_init();

  model->pagesize = sysconf(_SC_PAGESIZE);
  model->stacksize = default_stacksize;
  model->heapsize = default_heapsize;

  /* round stack and heap sizes to even pages */
  model->stacksize = ((model->stacksize + model->pagesize - 1) / model->pagesize) * model->pagesize;
  model->heapsize = ((model->heapsize + model->pagesize - 1) / model->pagesize) * model->pagesize;

  
  model->stack = guarded_area(model->stacksize);
  model->tospace = guarded_area(model->stacksize);
  model->heap = guarded_area(model->heapsize);
  model->limit = (ptr *)((char*) model->heap + model->heapsize);
  model->next = model->tospace;
  return;
}

void gc_cleanup(void) {
  print_metrics();
  metrics_cleanup();
  munmap((model->stack) - model->pagesize, (size_t)(model->stacksize + 2 * model->pagesize));
  munmap(((char*)(model->heap)) - model->pagesize, (size_t)(model->heapsize + 2 * model->pagesize));
  munmap(((char*)(model->tospace)) - model->pagesize, (size_t)(model->heapsize + 2 * model->pagesize));
  free(model);
  return;
}

void checkAddr(ptr* addr) {
  ptr *heap = model->heap; 
  ptr *stack = model->stack;
  if (heap-model->pagesize <= addr && addr < heap) {
    fprintf(stderr,"invalid access just below the heap\n");
  } else if (heap+model->heapsize <= addr && addr <= heap+model->heapsize+model->pagesize) {
    fprintf(stderr,"invalid access just above the heap\n");
  } else if (stack-model->pagesize <= addr && addr < stack) {
    fprintf(stderr,"invalid access just below the stack\n");
  } else if (stack+model->stacksize <= addr && addr < stack+model->stacksize+model->pagesize) {
    fprintf(stderr,"invalid access just above the stack\n");
  } else {
    fprintf(stderr, "| Segmentation violation at %p\n", addr);
  }
}

ptr* collect(ptr* sp, ptr* hp, long size) {

  if (hp + (size / sizeof(ptr)) <= model->limit) {
    return hp;
  }

  start_collect_metrics();
  /*
    There is an invarient that the gc must be ready to collect at
    the end of this function. IE No need to adjust anything except
    values that the mutator is allowed to change/lookat.
  */
  model->sp = sp;
  model->hp = hp;
  model->limit = (ptr *)((char*) model->tospace + model->heapsize)
; 
  /* Collect */
  forward(model->stack, &sp); //forward the stack
  forward(model->tospace, &(model->next)); //forward the heap
  hp = model->next;
  
  /* Free the heap and tidy up the state for the next collection*/
  model->next = model->heap;
  model->heap = model->tospace;
  model->tospace = model->next;
  
  metric_post_gc_size((long) hp - (long) model->tospace);
  
  if (!(hp + (size / sizeof(ptr)) <= model->limit)) {
    fprintf(stderr, "Error: Not enough room after collection\n");
    fflush(stderr);
    exit(EXIT_FAILURE);
  }

  end_collect_metrics();
  return hp;
}
