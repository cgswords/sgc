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
#include "tags.h"

void print(long x);
void fprint(FILE *fp, long x);

void print_mem(FILE *fp, ptr *head, ptr *base);

void print_mem_entry(FILE *fp, ptr * loc);

