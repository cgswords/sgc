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
#include <time.h>
#include "collector.h"
#include "printer.h"
#include "tags.h"
#include "metrics.h"

#define COPYCG 1

#include "copygc.h"

static memModel *mem;

static long pagesize;
static long heapsize;
static long stacksize;
static FILE *log;

ptr* getStack(void) {
  return mem->stack;
}

void setHeap(ptr* newHeap) {
  mem->heap = newHeap;
  mem->limit = (ptr*) ((char*) newHeap + heapsize - 61440);
  return;
}

void freeHeap(void) {
  munmap((mem->heap) - pagesize, (size_t)(heapsize + 2 * pagesize));
  return;
}

ptr* getHeap(void) {
  return mem->heap;
}

ptr* getLimit(void) {
  return mem->limit;
}

/* allocate a chunk of memory with a guard page on either end */
static ptr *guarded_area(long n) {  /* n must be page aligned */
  char *addr;

 /* allocate, leaving room for guard pages */
  addr = (char *)mmap(NULL,
                      (size_t)(n + (2 * pagesize)),
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

  return (ptr*) (addr + pagesize);
}

ptr *mallocHeap(void) {
  return guarded_area(heapsize);
}

void gc_init(void) {
  log = fopen("gc.log", "w");
  metrics_init();

  pagesize = sysconf(_SC_PAGESIZE);
  stacksize = default_stacksize;
  heapsize = default_heapsize;

  /* round stack and heap sizes to even pages */
  stacksize = ((stacksize + pagesize - 1) / pagesize) * pagesize;
  heapsize = ((heapsize + pagesize - 1) / pagesize) * pagesize;

  mem = malloc(sizeof(memModel));
  mem->stack = guarded_area(stacksize);
  mem->heap = guarded_area(heapsize);
  mem->limit = (ptr *)((char*) mem->heap + heapsize);
  return;
}

void gc_cleanup(void) {
  print_metrics();
  metrics_cleanup();

  munmap((mem->stack) - pagesize, (size_t)(stacksize + 2 * pagesize));
  freeHeap();
  free(mem);
  
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

  /* fprintf(log, "Stack:\n"); */
  /* print_mem(log, sp, mem->stack); */
  /* fprintf(log, "Heap:\n"); */
  /* print_mem(log, hp, mem->heap); */


  if (hp + (size / sizeof(ptr)) <= mem->limit) {
    return hp;
  }

  start_collect_metrics();

  ptr* new_hp = 
    gc(mem->stack, mem->heap, sp, hp, mem->limit, heapsize, pagesize); 


  if (!(new_hp + (size / sizeof(ptr)) <= mem->limit)) {
    fprintf(stderr, "Error: Not enough room after collection\n");
    fflush(stderr);
    exit(EXIT_FAILURE);
  }


  /* fprintf(log, "Stack:\n"); */
  /* print_mem(log, sp, mem->stack); */
  /* fprintf(log, "Heap:\n"); */
  /* print_mem(log, hp, mem->heap); */
  /* fflush(log); */
  
  
  end_collect_metrics();
  return new_hp;
}

