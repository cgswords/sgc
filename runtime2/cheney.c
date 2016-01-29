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

#include "printer.h"
#include "tags.h"
#include "cheney.h"
#include "collector.h"

static ptr *stack;
static ptr *heap;
static ptr *tospace;
static ptr *next;
static ptr *limit;
static long heapsize;
static long stacksize;
static long pagesize;
static FILE *log;

void gc_init(void) {
  /* Create a log file */
  log = fopen("gc.log", "w");

  pagesize = sysconf(_SC_PAGESIZE);
  stacksize = default_stacksize;
  heapsize = default_heapsize;
  
  /* round stack and heap sizes to even pages */
  stacksize = (((stacksize + pagesize - 1) /
                pagesize) * pagesize);
  heapsize = (((heapsize + pagesize - 1) /
               pagesize) * pagesize);;

  stack = guarded_area(stacksize);
  heap = guarded_area(heapsize);
  limit = (ptr*) ((long) heap + heapsize); 
  
  return;
}

ptr* getStack(){
  return stack;
}

void setHeap(ptr *newHeap) {
  heap = newHeap;
  return;
}

void freeHeap() {
  /* Convert to character pointer because page size is in terms of bytes */
  munmap(((char*) heap) - pagesize, (size_t)(heapsize + 2 * pagesize));
  return;
}

ptr* getHeap(void) {
  return heap;
}

/* allocate a chunk of memory with a guard page on either end */
ptr* guarded_area(long n) {  /* n must be page aligned */
  char *addr;

  /* allocate, leaving room for guard pages */
  addr = (char *)mmap(NULL, (size_t)(n + (2 * pagesize)),
                      PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS,
                      -1, 0);
  
  if (addr == (char *)-1) {
    fprintf(stderr, "mmap failed: %s\n", strerror(errno));
    exit(2);
  }

  /* remove access rights from the guard pages */
  if (mprotect(addr, (size_t)pagesize, PROT_NONE) ||
      mprotect(addr + pagesize + n, (size_t)pagesize, PROT_NONE)) {
    fprintf(stderr, "mprotect failed: %s\n", strerror(errno));
    exit(3);
  }

  return (ptr*) (addr + pagesize);
}

ptr* mallocHeap(void) {
  return guarded_area(heapsize);
}


void checkAddr(ptr* addr) {
  long ps = pagesize / sizeof(ptr*);
  long hs = heapsize / sizeof(ptr*);
  long ss = stacksize / sizeof(ptr*);
  if (heap - ps <= addr && addr < heap) {
    fprintf(stderr,"invalid access just below the heap\n");
  } else if (heap + hs <= addr && addr <= heap + hs + ps) {
    fprintf(stderr,"invalid access just above the heap\n");
  }else if (heap <= addr && addr < limit) {
    fprintf(stderr,"Error thrown for access in the heap\n");
  } else if (stack - ps <= addr && addr < stack) {
    fprintf(stderr,"invalid access just below the stack\n");
  } else if (stack + ss <= addr && addr < stack + ss + ps) {
    fprintf(stderr,"invalid access just above the stack\n");
  }else if (stack <= addr && addr < stack + ss) {
    fprintf(stderr,"Error thrown for access in the stack\n");
  } else {
    fprintf(stderr, "| Segmentation violation at %p\n", (void*) addr);
  }
}
 

ptr* collect(ptr *sp, ptr *hp, long alloc_size){
  
  fprintf(log,
          "GC collect called \n"
          "\tStack head: %p\tStack base: %p \n"
          "\tHeap head: %p\tHeap base: %p \n",
	  (void*) sp, (void*) stack, (void*) hp, (void*) heap);
  fflush(log);

  /* Check if the alloc will overflow this should be done in assembly */
  /* Cast to divide by sizeof(rt_ptr) because alloc_size is terms of bytes */
  if (hp + (alloc_size / sizeof(ptr)) <= limit) {
    /* there is enough room to allocate */
 /*       fprintf(stdout, "\tHeap head: %p\tHeap limit: %p \n", (void*) hp, (void*) limit);
      fprintf(stdout, "\tHeap limit: %p\t Alloc Size: %p \n", (void*) limit, (void*) ((long)hp + (alloc_size / sizeof(ptr)))); */
    return hp;
  }else{
    fprintf(log, "COLLECTING!\n\tHeap head: %p\tHeap limit: %p \t Page: %ld\n", 
          (void*) hp, (void*) limit, pagesize);
    
    /* Setup for a collection */
    tospace = mallocHeap();
    next = tospace;
    /* heapsize is in terms of bytes */
    limit = (ptr*) ((char*) tospace + heapsize);
    
    /* Collect */
    /* Forward the stack */
    fprintf(stdout, "Forwarding the stack!\n");
    forward(stack, &sp);
    /* Forward the heap */
    fprintf(stdout, "Forwarding the heap!\n");
    forward(tospace, &next);
    hp = next;
    
    /* Free the heap and tidy up the state for the next collection*/
    freeHeap();  
    heap = tospace;
    
    /* Check if enough memory was freed to allocate the object */        
    if (! (hp + (alloc_size / sizeof(ptr*)) <= limit)) {
      fprintf(stderr, "Error: Not enough room after collection\n");
      fflush(stderr);
      exit(EXIT_FAILURE);
    }
  }
   
    fprintf(log,
            "GC collect returning \n"
            "\tStack head: %p\tStack base: %p \n"
            "\tHeap head: %p\tHeap base: %p \n",
            (void*) sp, (void*) stack, (void*) hp, (void*) heap);
    fflush(log);

    return hp;
}

/*
  The pointer to the pointer to the stop allows the algorithm
  to seamlessly update the stop point without having to pass
  stop around. As if this was a value that has been captured
  by a closure.
*/
void forward(ptr *scan, ptr **stop){
  for(;scan < *stop; scan++){
    fprintf(stdout, "Forwarding %p", scan);
    fprintf(stdout, "   Value %p\n", *scan);
    switch (TAG(*scan, basic_ptr_tag_mask)) {
      case tag_fixnum:
        break;
      case tag_pair:
        {
          rt_ptr p = AS_RT_PTR(*scan);
          if(p->fwd_p >= tospace && p->fwd_p < limit)
            *scan = p->fwd_ptr;
          else
            *scan = forward_pair(p); 
        }
        break;
      case tag_procedure :
        {
          rt_ptr p = AS_RT_PTR(*scan);
          if(p->fwd_p >= tospace && p->fwd_p < limit)
            *scan = p->fwd_ptr;
          else
            *scan = forward_procedure(p); 
        }
        break;
      case tag_vector:
        {
          rt_ptr p = AS_RT_PTR(*scan);
          if(p->fwd_p >= tospace && p->fwd_p < limit)
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

/* Returns a ptr that points to the location of the new object */
ptr forward_pair(rt_ptr p){
  /* Copy pair to next allocation point */
  rt_pair *n = (rt_pair*) next;
  n->car = p->pair.car;
  n->cdr = p->pair.cdr;
  /* Set the forward pointer */
  p->fwd_ptr = (long) next + tag_pair;
  /* bump the allocation pointer */
  next += size_pair;
  return p->fwd_ptr;
}

ptr forward_procedure(rt_ptr p){
  /* Copy procedure to next allocation point */
  rt_procedure *n = (rt_procedure*) next;
  n->code   = p->procedure.code;
  int length = p->procedure.length;
  n->length = length;
  length = UNFIX(length);
  long i;
  for(i = 0; i < length; i++)
    n->data[i] = p->procedure.data[i];
  
  /* Set the forward pointer */
  ptr fwd_ptr = (long) next + tag_procedure; 
  p->fwd_ptr = fwd_ptr;

  /* bump the allocation pointer  code + length + data[] */
  next += 2 + length;
  return fwd_ptr;
}

ptr forward_vector(rt_ptr p){
  /* Copy vector to next allocation point */
  rt_vector *n = (rt_vector*) next;
  int length = p->vector.length;
  n->length = length;
  length = UNFIX(length);
  long i;
  for(i = 0; i < length; i++)
    n->data[i] = p->vector.data[i];
  
  /* Set the forward pointer */
  ptr fwd_ptr = (long) next + tag_vector; 
  p->fwd_ptr = fwd_ptr;

  /* bump the allocation pointer length + data[] */
  next += 2 + length;
  return fwd_ptr;
}
