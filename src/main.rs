use std::{sync::Arc, time::{Duration, Instant}};

use pretty_bench::PrettyBench;

fn main() {
    let pb = PrettyBench::new();

    let name = "Diagonalisation of a 0x0 matrix".into();
    pb.inner.create_bench_group_individual(Arc::clone(&name));

    for _ in 0..1000 {
        let start = Instant::now();
        spin_sleep::sleep(Duration::from_nanos(100_000));
        println!("{:.5e}", start.elapsed().as_secs_f32())
    }

    pb.inner.print_histograms();
}