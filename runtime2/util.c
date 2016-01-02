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
#include "tags.h"

long inBounds (ptr *base, long size, ptr addr) {
  if (((long) base) < ((long) addr) &&
      ((long) addr) < ((long) base + size)) return 1;
  return 0;
}

int constant(ptr val) {
  if (val == _contmark ||
      val == _true || val == _false ||
      val == _nil || val == _void ||
      TAG(val, mask_fixnum) == tag_fixnum) {
    return 1;
  }
  return 0;
}

void gcprint(char *name, ptr *loc, ptr *next, ptr val) {
    printf("| %-10s | %16p | %16p | %16p |\n", name, loc, next, val);
}
void gcprintsingle(char *name, void * val) {
    printf("| %-10s |                                       %16p |\n", name, val);
}

int getTag(ptr val) {
  return (int)(TAG(val, mask_pair));
}

