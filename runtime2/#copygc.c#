#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/queue.h>
#include <signal.h>
#include <ucontext.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include "collector.h"
#include "printer.h"
#include "tags.h"
#include "stack.h"
#include "util.h"
#include "copygc.h"


node* scan_stack(ptr* head, ptr* base, node* live_set) {
  printf("[=============================== STACK ===============================]\n");
  printf("| Stack Scan     | %p to %p                   |\n", (void*)head, (void*)base);

  while (base < head) {
    ptr value = *head;

    print_mem_entry(stdout, head);

    if (TAG(value, mask_pair) == tag_pair        ||
        TAG(value, mask_vector) == tag_vector    ||
        TAG(value, mask_procedure) == tag_procedure) {
      live_set = push(live_set, head);
      printf("\t\t  | -- Added to the live set!\n");
    }

    head--;
  }

  return live_set;
}

void fnprint (ptr *fn) {
  ptr code  = PROCCODE(fn);
  long size = PROCSIZE(fn);
  ptr *data = (ptr *)PROCDATA(fn);
  printf("+------------+------------------+------------------+------------------+\n");
  gcprintsingle(" FUNC "  , (void *)fn);
  gcprintsingle(" [code] ", (void *)code);
  gcprintsingle(" [size] ", (void *)size);
  gcprintsingle(" [data] ", (void *)data);
  print_mem_entry(stdout, fn);
  /* print_mem(stdout, loc+size, loc-1); */
}

ptr *save_proc(gcModel *model, ptr *loc) {
  long  i; 
  long  size =  PROCSIZE(loc);
  size_t byt = (size)*word_size*sizeof(ptr);
  ptr  *next = model->next;
  ptr  *uloc = (ptr *)(UNTAG((long)loc, tag_procedure));
  ptr  *ret  = (ptr *)((long)next + tag_procedure);

  gcprintsingle(" LOC ", loc);
  fnprint(loc);
  
  /* else for each field fi of p
   *    next.fi := p.fi          */
  gcprintsingle(" BYTES ", (void *)byt);
  memcpy(next, uloc, byt);
  gcprintsingle(" NEXT ", next);
  gcprintsingle(" RET ", ret);
  fnprint(ret);

  /* p.f1 := next */
  *loc = (ptr)ret;
 
  /* next := next + (size of *p) */
  model->next = next + size + 2;
  
  printf("+------------+------------------+------------------+------------------+\n");
  gcprintsingle(" [NEXT] ", (void*)model->next);
  gcprintsingle(" [VALU] ", (void*)*next);
  gcprintsingle(" [P.F1] ", (void*)loc);
  gcprintsingle(" [F1.V] ", (void*)*loc);
  gcprintsingle(" [RETN] ", (void*)ret);
  printf("+------------+------------------+------------------+------------------+\n");
 
  /* return p.f1                 */ 
  return (ptr *)(((long)next) + tag_procedure);
}

ptr *save_pair(gcModel *model, ptr *loc) {
  ptr *next = model->next;
  ptr val = *loc;
  
  gcprint("pair", loc, next, val);

  memcpy(next, loc, size_pair*word_size);

  /* p.f1 := next */
  *loc = ((long)next) + tag_pair;
  
  /* next := next + (size of *p) */
  model->next = next + size_pair;

  /* return p.f1                 */
  return (ptr *)*loc;
}

ptr *save_vec(gcModel *model, ptr *loc) {
  long i;
  ptr *next = model->next;
  ptr val = *loc;
  long size = VECSIZE(val);
  
  gcprint("vector", loc, next, val);
  
  memcpy(next, loc, size*word_size);

  /* p.f1 := next */
  *loc = ((long)next) + tag_vector;

  /* next := next + (size of *p) */
  model->next = next + size;

  /* return p.f1                 */
  return (ptr *)*loc;
}

/* forward (p) {
 * if p points t from-space
 *    then if p.f1 points to to-space
 *               then return p.f1
 *    else for each field fi of p
 *              next.fi := p.fi
 *           p.f1 := next
 *           next := next + (size of *p)
 *           return p.f1
 * else return p                         */
ptr *forward(gcModel *model, ptr *loc) {
  long heapsize = model->heapsize;
  long tag = TAG((long)loc, mask_pair);
  if (!inBounds(model->tospace, heapsize, *loc)) {
    switch (tag) {
      case tag_procedure : {
        gcprintsingle(" GCLOC ", loc);
        gcprintsingle(" GCPTR ", *loc);
        if (inBounds(model->tospace, heapsize, *loc)) {
          printf("Returning inbounds value: %p\n", *loc);
          return (ptr *)*loc;
        }
        return save_proc(model, loc);
      };
      case tag_pair : {
        ptr car = CAR(loc);
        if (inBounds(model->tospace, heapsize, car))
          return (ptr *)car;
        return save_pair(model, loc);
      };
      case tag_vector : {
        ptr vlen = VECTORLENGTH(loc);
        if (inBounds(model->tospace, heapsize, vlen))
          return (ptr *)vlen;
        return save_vec(model, loc);
      }
      break;
      case tag_fixnum : {
        ptr *next = model->next;
        *next = (ptr)loc;
        model->next = (ptr *)(((long)model->next) + size_fixnum);
        return (ptr *)next; 
      };
      default : 
        printf ("TODO handle default case for pointer forwarding.\n");
        printf ("TAG was %d\n", tag);
        printf ("FAILED with pointer: %p\n", loc);
        exit(EXIT_FAILURE);
    }
  }
  gcprintsingle(" NCLOC ", loc);
  gcprintsingle(" NCPTR ", *loc);
  return *loc;
}

void forward_pair(gcModel *model, ptr *loc) {
  *loc = (ptr)(forward(model, (ptr *)loc));
  *(loc+1) = (ptr)(forward(model, (ptr *)(loc+1)));
}

void forward_vec(gcModel *model, ptr *loc) {
  exit(EXIT_FAILURE);
}

/* scan := begin-of-to-space
 * next := scan
 * for each root r
 *     r := forward(r)
 * while scan < next
 *     for each field fi of *scan
 *            scan.fi := forward(scan.fi)
 *     scan := scan + (size of *scan)     */

gcModel *copyCollect(gcModel *model) {
  node *roots = scan_stack(model->sp, model->stack, NULL);

  printf("\n");
  printf("+-------------------------------+-------------------------------------+\n");
  printf("|  From Space: %p \t| To Space: %p \t      |\n", model->heap, model->tospace);

  printf("+------------+------------------+------------------+------------------+\n");
  printf("| DATA       | FROM             | TO               | VALUE            |\n");
  printf("+------------+------------------+------------------+------------------+\n");

  while (roots != NULL) {
    ptr *root = peek(roots);
    ptr *res;
    gcprintsingle("*ROOT*", (void *)*root);
    *root = (ptr)forward(model, (ptr *)*root);
    print_mem_entry(stdout, (void *)root);
    roots = pop(roots);
    fprintf(stdout, "+------------+--------------------------------------------------------+\n");
    gcprintsingle("*ROOT*", (void *)*root);
    gcprintsingle("*POPR*", (void *)roots);
    fprintf(stdout, "+------------+--------------------------------------------------------+\n");
    fprintf(stdout, "\n\n");
  }

  fprintf(stdout, " BEFORE SCAN : %p %p %p\n", model->tospace, model->scan, model->next);

  ptr *scan = model->scan;

  while (scan < model->next) {
    fprintf(stdout, "[ CHECKING SCAN                                                       ]\n");
    fprintf(stdout, "| BEFORE     | Ptr: %p, Val: %p\n", scan, *(scan));
    ptr val = *scan;
    if (TAG(val, mask_pair) == tag_pair) {
      forward_pair(model, scan);
      scan = scan+size_pair;
    } else if (TAG(val, mask_vector) == tag_vector) {
      long size = VECSIZE(val); 
      forward_vec(model, scan); 
      scan = scan+size;
    } else if (TAG(val, mask_procedure) == tag_procedure) {
      gcprint("procedure", scan, model->next, val);
      ptr *pval = (ptr *)val;
      long size = PROCSIZE(val);
      gcprint("procedure", pval, model->next, *pval);
      ptr *res = forward(model, pval);
      printf("| Forwarding | -> %p\n", res);
      *scan = (ptr)res;
      fprintf(stdout, "| AFTER      | Ptr: %p, Val: %p\n", scan, *(scan));
      scan = scan+size;
    } else {
      scan += 1;
    }
  }

  model->scan = scan;
  fprintf(stdout, " AFTER SCAN  : %p %p %p\n", model->tospace, model->scan, model->next);

}




