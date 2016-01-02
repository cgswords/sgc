#include <stdlib.h>
#include <stdio.h>


#include "metrics.h"

long current_timestamp() {
  struct timeval te; 
  gettimeofday(&te, NULL); // get current time
  long milliseconds = te.tv_sec*1000LL + te.tv_usec/1000; // caculate milliseconds
  return milliseconds;
}

static metricModel *metrics;

void metric_pair_copied(void)        { metrics->pairs_copied += 1; }

void metric_procedure_copied(void)   { metrics->procedures_copied += 1; }

void metric_vector_copied(void)      { metrics->vectors_copied += 1; }

void metric_post_gc_size(long size)  { metrics->size_allocated += size; }

void start_collect_metrics(void) { 
  metrics->collections += 1;
  metrics->stopwatch = current_timestamp();
}

void end_collect_metrics(void) {
  metrics->total_time = metrics->total_time + current_timestamp() - metrics->stopwatch;
}

void metrics_init(void) {
  metrics = malloc(sizeof(metricModel)); 
  metrics->procedures_copied = 0;
  metrics->pairs_copied = 0;
  metrics->vectors_copied = 0;
  metrics->size_allocated = 0;
  metrics->collections = 0;
  metrics->total_time = 0;
  metrics->stopwatch = current_timestamp();
}

void metrics_cleanup(void) {
  free(metrics);
}

void print_metrics(void) {
  printf("+----------------------------------------------------------------------+\n");
  printf("| GC METRICS                                                           |\n");
  printf("+-------------------+--------------------------------------------------+\n");
  printf("| Metric            | Value                                            |\n");
  printf("+-------------------+--------------------------------------------------+\n");
  printf("| procedures_copied | %48ld |\n", metrics->procedures_copied);
  printf("| pairs_copied      | %48ld |\n", metrics->pairs_copied);
  printf("| vectors_copied    | %48ld |\n", metrics->vectors_copied);
  printf("| size_allocated    | %48ld |\n", metrics->size_allocated);
  printf("| collections       | %48ld |\n", metrics->collections);
  printf("| total_time        | %48ld |\n", metrics->total_time);
  printf("| stopwatch         | %48ld |\n", metrics->stopwatch);
  printf("+-------------------+--------------------------------------------------+\n");
}
