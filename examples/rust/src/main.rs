extern crate pretty_bench;

use pretty_bench::PrettyBench;
use std::{fs::OpenOptions, io, sync::Arc, time::Duration};

fn main() -> io::Result<()> {
    let bench_group_name: Arc<str> = "Diagonalisation of a 0x0 matrix".into();

    println!("{}", bench_group_name);

    let pb = PrettyBench::new();

    pb.create_bench_group_individual(Arc::clone(&bench_group_name));

    match OpenOptions::new().read(true).open("test.bench") {
        Ok(file) => pb.deserialise_import(file)?,
        Err(err) if err.kind() == io::ErrorKind::NotFound => (),
        Err(err) => {
            eprintln!("Failed to open file: {}", err);
        }
    };

    // Example benchmark
    for _ in 0..1000 {
        let bench_id = pb.start_bench();
        pretty_bench::sleep(Duration::from_micros(10));
        _ = pb.end_bench(bench_group_name.as_ref(), bench_id);
    }

    pb.print_histograms();

    let file = io::BufWriter::new(
        OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open("test.bench")?,
    );
    pb.serialise(file)?;

    Ok(())
}
