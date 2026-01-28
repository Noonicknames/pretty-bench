#include <inttypes.h>
#include <string.h>

#ifndef PRETTY_BENCH
#define PRETTY_BENCH

typedef struct Duration {
    uint32_t nanos;
    uint64_t secs;
} Duration;

typedef struct Str {
    const char* ptr;
    uint64_t len;
} Str;

typedef Str ArcStr;

typedef struct PrettyBench {
    const void* inner;
} PrettyBench;

Duration duration_nanos(uint64_t nanos) {
    return (struct Duration){
        .nanos = nanos % 1000000000,
        .secs = nanos / 1000000000,
    };
}

Duration duration_micros(uint64_t micros) {
    return duration_nanos(micros * 1000);
}

Duration duration_millis(uint64_t millis) {
    return duration_nanos(millis * 1000000);
}

Duration duration_secs(uint64_t secs) {
    return (struct Duration){
        .nanos = 0,
        .secs = secs,
    };
}

void pb_sleep(Duration duration);

ArcStr arc_str_new(const char* ptr, uint64_t len);
ArcStr arc_str_clone(ArcStr arc_str);
void arc_str_drop(ArcStr* arc_str);

PrettyBench pb_new();

void pb_new_group_individual(PrettyBench pretty_bench, ArcStr name);
Duration pb_start_bench(PrettyBench pretty_bench);
void pb_end_bench(PrettyBench pretty_bench, Str name, Duration id);
void pb_import_from_file(PrettyBench pretty_bench, Str source);
void pb_serialise_to_file(PrettyBench pretty_bench, Str dest);
void pb_serialise_append_to_file(PrettyBench pretty_bench, Str dest);
void pb_print_histograms(PrettyBench pretty_bench);
void pb_new_group_bucketed(PrettyBench pretty_bench, ArcStr name, Duration bucket_width);
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