#include <sys/time.h>

typedef struct {
  long procedures_copied;
  long pairs_copied;
  long vectors_copied;
  long size_allocated;
  long collections;
  long total_time;
  long stopwatch;
} metricModel;

void metric_pair_copied(void);
void metric_procedure_copied(void);
void metric_vector_copied(void);
void metric_post_gc_size(long size);
void start_collect_metrics(void);
void end_collect_metrics(void);
void metrics_init(void);
void metrics_cleanup(void);
void print_metrics(void);


