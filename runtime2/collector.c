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

#define COPYCG 1

#include "copygc.h"

memModel *mem;

long pagesize;
long heapsize;
long stacksize;

/* bout and aout are initialized in gc init */
static FILE *bout, *aout, *log;

ptr* getStack(void) {
  return mem->stack;
}

void setHeap(ptr* newHeap) {
  mem->heap = newHeap;
  return;
}

void freeHeap(void) {
  munmap((mem->heap) - pagesize, (size_t)(heapsize + 2 * pagesize));
  return;
}

ptr* getHeap(void) {
  return mem->heap;
}

/* allocate a chunk of memory with a guard page on either end */
static ptr *guarded_area(long n) {  /* n must be page aligned */
  char *addr;

 /* allocate, leaving room for guard pages */
  addr = (char *)mmap(NULL,
                      (size_t)(n + 2 * pagesize),
                      PROT_READ | PROT_WRITE,
                      MAP_PRIVATE | MAP_ANON,
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

  return (ptr*) addr + pagesize;
}

ptr *mallocHeap(void) {
  return (ptr*) guarded_area(heapsize);
}

void gc_init(void) {
  
  bout = fopen("stackb.log", "w");
  aout = fopen("stacka.log", "w");
  log = fopen("gc.log", "w");

  pagesize = sysconf(_SC_PAGESIZE);
  stacksize = default_stacksize;
  heapsize = default_heapsize;

  /* round stack and heap sizes to even pages */
  stacksize = ((stacksize + pagesize - 1) / pagesize) * pagesize;
  heapsize = ((heapsize + pagesize - 1) / pagesize) * pagesize;

  mem = malloc(sizeof(memModel));
  mem->stack = guarded_area(stacksize);
  mem->heap = guarded_area(heapsize);

  return;
}


void checkAddr(ptr* addr) {
  ptr *heap = mem->heap; 
  ptr *stack = mem->stack;
  if (heap-pagesize <= addr && addr < heap) {
    fprintf(stderr,"invalid access just below the heap\n");
  } else if (heap+heapsize <= addr && addr <= heap+heapsize+pagesize) {
    fprintf(stderr,"invalid access just above the heap\n");
  } else if (stack-pagesize <= addr && addr < stack) {
    fprintf(stderr,"invalid access just below the stack\n");
  } else if (stack+stacksize <= addr && addr < stack+stacksize+pagesize) {
    fprintf(stderr,"invalid access just above the stack\n");
  } else {
    fprintf(stderr, "| Segmentation violation at %p\n", addr);
  }
}

ptr* collect(ptr* sp, ptr* hp, long size) {

  fprintf(log,
          "GC collect called \n"
          "\tStack head: %p\tStack base: %p \n"
          "\tHeap head: %p\tHeap base: %p \n",
	  (void*) sp, (void*) mem->stack, (void*) hp, (void*) mem->heap);
  fflush(log);
  
  /* print_stack(bout, sp, stack); */

  long limit = (heapsize / 8) - (pagesize*8*8*2);
  /* long limit = 0; */

  if (hp < heap + limit) {
    
    return hp;
  }
  
  ptr* new_hp = gc(sp);

  if (new_hp < heap + limit) {
    fprint(stderr, "Error: Not enough room after collection\n");
    fflush(stderr);
    exit(EXIT_FAILURE);
  }

  /* print_stack(aout, sp, stack); */
  fprintf(log,
          "GC collect returning \n"
          "\tStack head: %p\tStack base: %p \n"
          "\tOld Heap head: %p\n|\tHeap head: %p\tHeap base: %p \n",
	  (void*) sp, (void*) mem->stack, (void*) hp, new_hp, (void*) mem->heap);
  fflush(log);

  return new_hp;
}

