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

long inBounds (ptr *base, long size, ptr addr); 
int constant(ptr val);
void gcprint(char *name, ptr *loc, ptr *next, ptr val); 
void gcprintsingle(char *name, void * val); 
int getTag(ptr val); 

