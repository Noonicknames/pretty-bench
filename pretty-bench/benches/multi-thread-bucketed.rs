use criterion::{Criterion, criterion_group, criterion_main};
use pretty_bench::PrettyBench;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::{
    sync::Arc,
    time::{Duration, Instant},
};

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("multi-thread-bucketed");

    group.bench_function("benching", |b| {
        b.iter_custom(|iters| {
            let pretty_bench = PrettyBench::new();
            let name = "Example Name".into();
            pretty_bench.create_bench_group_bucketed(Arc::clone(&name), Duration::from_nanos(1));
            let bench_start = Instant::now();
            (0..iters).into_par_iter().for_each(|_| {
                let start = pretty_bench.start_bench();
                pretty_bench.end_bench(name.as_ref(), start).unwrap();
            });
            bench_start.elapsed()
        });
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
