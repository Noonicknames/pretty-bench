#include "../../bindings/headers/pretty_bench.h"
#include <stdio.h>

void main() {
    // Creation of an ArcStr which can be efficiently copied.
    ArcStr bench_group_name =
        arc_str_from_c_str("Diagonalisation of a 0x0 matrix");

    // ArcStr is not null terminated, this means it must be printed this way.
    printf("%.*s\n", bench_group_name.len, bench_group_name.ptr);

    // Create PrettyBench struct
    PrettyBench pb = pb_new();
    
    // Ensure to clone since we want to keep a copy of the group name ourselves.
    pb_new_group_individual(pb, arc_str_clone(bench_group_name));

    // Import previous benchmarks
    pb_import_from_file(pb, str_from_c_str("test.bench"));

    // Example benchmark
    for (auto i = 0; i < 1000; i++) {
        auto bench_id = pb_start_bench(pb, bench_group_name);
        pb_sleep(100000);
        pb_end_bench(pb, bench_group_name, bench_id);
    }

    // You can drop the ArcStr now, this will not delete pb's copy.
    arc_str_drop(&bench_group_name);

    // Output to file
    pb_serialise_to_file(pb, str_from_c_str("test.bench"));

    // Print out histograms
    pb_print_histograms(pb);

    // Clean up resources
    pb_drop(&pb);
}