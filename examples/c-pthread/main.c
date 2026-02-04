#include "../../bindings/headers/pretty_bench.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

typedef struct ThreadData {
    int repeats;
    ArcStr bench_group_name;
    PrettyBench pb;
} ThreadData;

// Example benchmark
void *bench_thread(void *arg);

void main() {
    long max_concurrency = sysconf(_SC_NPROCESSORS_ONLN);

    printf("pthread max concurrency: %d\n", max_concurrency);
    // Creation of an ArcStr which can be efficiently copied.
    ArcStr bench_group_name =
        arc_str_from_c_str("Diagonalisation of a 0x0 matrix");

    // ArcStr is not null terminated, this means it must be printed this way.
    printf("%.*s\n", bench_group_name.len, bench_group_name.ptr);

    // Create PrettyBench struct
    PrettyBench pb = pb_new();

    // Ensure to clone since we want to keep a copy of the group name ourselves.
    pb_new_group_bucketed(pb, arc_str_clone(bench_group_name),
                          duration_nanos(10));
    // pb_new_group_individual(pb, arc_str_clone(bench_group_name));

    // Import previous benchmarks
    pb_import_from_file(pb, str_from_c_str("test.bench"));

    const long TOTAL_REPEATS = 1000;

    pthread_t *threads =
        (pthread_t *)malloc(max_concurrency * sizeof(pthread_t));

    ThreadData thread_data_first = (struct ThreadData){
        .repeats =
            TOTAL_REPEATS / max_concurrency + TOTAL_REPEATS % max_concurrency,
        .bench_group_name = bench_group_name,
        .pb = pb,
    };

    ThreadData thread_data_other = (struct ThreadData){
        .repeats = TOTAL_REPEATS / max_concurrency,
        .bench_group_name = bench_group_name,
        .pb = pb,
    };

    pthread_create(&threads[0], NULL, bench_thread, &thread_data_first);
    for (int thread = 1; thread < max_concurrency; thread++) {
        pthread_create(&threads[thread], NULL, bench_thread,
                       &thread_data_other);
    }

    for (int thread = 0; thread < max_concurrency; thread++) {
        pthread_join(threads[thread], NULL);
    }
    free(threads);

    // You can drop the ArcStr now, this will not delete pb's copy.
    arc_str_drop(&bench_group_name);

    // Output to file
    pb_serialise_to_file(pb, str_from_c_str("test.bench"));

    // Print out histograms
    pb_print_histograms(pb);

    // Clean up resources
    pb_drop(&pb);
}

// Example benchmark
void *bench_thread(void *arg) {
    ThreadData *thread_data = (ThreadData *)arg;

    for (int i = 0; i < thread_data->repeats; i++) {
        auto bench_id = pb_start_bench(thread_data->pb);
        pb_sleep(duration_micros(100));
        pb_end_bench(thread_data->pb, thread_data->bench_group_name, bench_id);
    }
    return NULL;
}