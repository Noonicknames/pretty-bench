#include <inttypes.h>
#include <string.h>

#ifndef PRETTY_BENCH
#define PRETTY_BENCH

typedef struct Str {
    const char* ptr;
    uint64_t len;
} Str;

typedef Str ArcStr;

typedef struct PrettyBench {
    const void* inner;
} PrettyBench;

void pb_sleep(long long nanos);

ArcStr arc_str_new(const char* ptr, uint64_t len);
ArcStr arc_str_clone(ArcStr arc_str);
void arc_str_drop(ArcStr* arc_str);

PrettyBench pb_new();

void pb_new_group_individual(PrettyBench pretty_bench, ArcStr name);
uint64_t pb_start_bench(PrettyBench pretty_bench, Str name);
void pb_end_bench(PrettyBench pretty_bench, Str name, uint64_t id);
void pb_import_from_file(PrettyBench pretty_bench, Str source);
void pb_serialise_to_file(PrettyBench pretty_bench, Str dest);
void pb_serialise_append_to_file(PrettyBench pretty_bench, Str dest);
void pb_print_histograms(PrettyBench pretty_bench);
void pb_new_group_bucketed(PrettyBench pretty_bench, ArcStr name, uint64_t bucket_width_nanos);
PrettyBench pb_clone(PrettyBench pretty_bench);
void pb_drop(PrettyBench* pretty_bench);

Str str_from_c_str(const char* c_str) {
    return (struct Str){
        .ptr = c_str,
        .len = strlen(c_str),
    };
}

ArcStr arc_str_from_c_str(const char* c_str) {
    return arc_str_new(c_str, strlen(c_str));
}

#endif