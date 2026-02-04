use criterion::{Criterion, criterion_group, criterion_main};
use pretty_bench::PrettyBench;
use std::{
    hint::black_box,
    sync::Arc,
    time::{Duration, Instant},
};

fn criterion_benchmark(c: &mut Criterion) {
    const BATCH_SIZE: usize = 64;
    let mut group = c.benchmark_group("single-thread-individual");

    group.bench_function("benching", |b| {
        b.iter_custom(|iters| {
            let pretty_bench = PrettyBench::new();
            let name = "Example Name".into();
            pretty_bench.create_bench_group_individual(Arc::clone(&name));
            let bench_start = Instant::now();
            for _ in 0..iters {
                let start = pretty_bench.start_bench();
                pretty_bench.end_bench(name.as_ref(), start).unwrap();
            }
            bench_start.elapsed()
        });
    });

    const BENCHES: usize = 100_000;

    let pretty_bench = PrettyBench::new();
    let name = "Example Name".into();
    pretty_bench.create_bench_group_individual(Arc::clone(&name));

    for _ in 0..BENCHES {
        unsafe {
            let start = pretty_bench.start_bench();
            pretty_bench
                .end_bench(name.as_ref(), start)
                .unwrap_unchecked();
        }
    }

    let mut buf = Vec::new();

    pretty_bench.serialise(&mut buf).unwrap();

    let serialise_len = buf.len();
    buf.reserve(serialise_len * BATCH_SIZE);

    group.bench_function("serialisation", |b| {
        b.iter_custom(|iters| {
            let mut total_time = Duration::ZERO;

            for _ in 0..iters / BATCH_SIZE as u64 {
                buf.clear();
                let bench_start = Instant::now();
                for _ in 0..BATCH_SIZE {
                    unsafe {
                        pretty_bench
                            .serialise(black_box(&mut buf))
                            .unwrap_unchecked();
                    }
                }
                total_time += bench_start.elapsed();
            }
            // Remainder
            {
                buf.clear();
                let bench_start = Instant::now();
                for _ in 0..iters % BATCH_SIZE as u64 {
                    unsafe {
                        pretty_bench
                            .serialise(black_box(&mut buf))
                            .unwrap_unchecked();
                    }
                }
                total_time += bench_start.elapsed();
            }
            total_time
        });
    });

    buf.clear();
    drop(buf);
    let mut buf = Vec::new();
    pretty_bench.serialise(&mut buf).unwrap();
    drop((pretty_bench, name));

    group.bench_function("deserialisation", |b| {
        b.iter_custom(|iters| {
            let mut total_time = Duration::ZERO;

            for _ in 0..iters / BATCH_SIZE as u64 {
                let pretty_bench = PrettyBench::new();
                let bench_start = Instant::now();
                for _ in 0..BATCH_SIZE {
                    unsafe {
                        pretty_bench
                            .deserialise_import(black_box(&*buf))
                            .unwrap_unchecked();
                    }
                }
                total_time += bench_start.elapsed();
            }

            // Remainder
            {
                let pretty_bench = PrettyBench::new();
                let bench_start = Instant::now();
                for _ in 0..iters % BATCH_SIZE as u64 {
                    unsafe {
                        pretty_bench
                            .deserialise_import(black_box(&*buf))
                            .unwrap_unchecked();
                    }
                }
                total_time += bench_start.elapsed();
            }

            total_time
        });
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
