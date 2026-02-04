use std::{
    io::{Read, Write},
    sync::Arc,
    time::Instant,
};

use core::{
    sync::atomic::AtomicU64,
    time::Duration,
};

use crate::BenchGroup;

#[derive(Clone)]
pub struct PrettyBench {
    pub(crate) inner: Arc<PrettyBenchInner>,
}

impl PrettyBench {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(PrettyBenchInner {
                start_instant: Instant::now(),
                benchgroups: papaya::HashMap::new(),
            }),
        }
    }
    pub fn start_instant(&self) -> Instant {
        self.inner.start_instant()
    }
    pub fn create_bench_group_individual(&self, name: Arc<str>) {
        self.inner.create_bench_group_individual(name);
    }
    pub fn create_bench_group_bucketed(&self, name: Arc<str>, bucket_width: Duration) {
        self.inner.create_bench_group_bucketed(name, bucket_width);
    }
    pub fn serialise(&self, output: impl Write) -> std::io::Result<usize> {
        self.inner.serialise(output)
    }
    pub fn print_histograms(&self) {
        self.inner.print_histograms();
    }

    pub fn deserialise_import(&self, input: impl Read) -> std::io::Result<()> {
        self.inner.deserialise_import(input)
    }

    pub fn start_bench(&self) -> Instant {
        self.inner.start_bench()
    }

    pub fn end_bench(&self, name: impl AsRef<str>, start: Instant) -> Result<Duration, ()> {
        self.inner.end_bench(name, start)
    }

    pub fn end_bench_with_instant(
        &self,
        name: impl AsRef<str>,
        start: Instant,
        end_time: Instant,
    ) -> Result<Duration, ()> {
        self.inner.end_bench_with_end_instant(name, start, end_time)
    }
}

pub struct PrettyBenchInner {
    start_instant: Instant,
    benchgroups: papaya::HashMap<Arc<str>, BenchGroup>,
}

impl PrettyBenchInner {
    pub fn start_instant(&self) -> Instant {
        self.start_instant
    }
    pub fn create_bench_group_individual(&self, name: Arc<str>) {
        self.benchgroups
            .pin()
            .insert(Arc::clone(&name), BenchGroup::new_individual(name));
    }
    pub fn create_bench_group_bucketed(&self, name: Arc<str>, bucket_width: Duration) {
        self.benchgroups.pin().insert(
            Arc::clone(&name),
            BenchGroup::new_bucketed(name, bucket_width),
        );
    }
    pub fn serialise(&self, mut output: impl Write) -> std::io::Result<usize> {
        let mut written_bytes = 0;
        let guard = self.benchgroups.guard();
        for (_, benchgroup) in self.benchgroups.iter(&guard) {
            written_bytes += benchgroup.serialise(&mut output)?;
        }
        Ok(written_bytes)
    }
    pub fn print_histograms(&self) {
        let guard = self.benchgroups.guard();
        for (name, benchgroup) in self.benchgroups.iter(&guard) {
            println!("Histogram for `{}`", name);
            benchgroup.print_histogram();
        }
    }
    pub fn deserialise_import(&self, mut input: impl Read) -> std::io::Result<()> {
        let guard = self.benchgroups.guard();
        loop {
            match BenchGroup::deserialise(&mut input) {
                Ok(de_benchgroup) => {
                    let benchgroup = self.benchgroups.get_or_insert_with(
                        Arc::clone(de_benchgroup.name()),
                        || de_benchgroup.empty_clone(),
                        &guard,
                    );
                    // Ideally I could skip deserialising and importing and just directly read into benchgroup.
                    // However, if performance is not an issue here its fine.
                    benchgroup.import(&de_benchgroup);
                }
                Err(why) if why.kind() == std::io::ErrorKind::UnexpectedEof => {
                    break;
                }
                Err(why) => {
                    eprintln!("{}", why);
                }
            }
        }

        Ok(())
    }
    pub fn start_bench(&self) -> Instant {
        Instant::now()
    }

    pub fn end_bench(&self, name: impl AsRef<str>, start: Instant) -> Result<Duration, ()> {
        // Record time at the start, we don't want to record the time it takes to lock the mutex.
        let end_time = Instant::now();
        self.end_bench_with_end_instant(name, start, end_time)
    }

    pub fn end_bench_with_end_instant(
        &self,
        name: impl AsRef<str>,
        start: Instant,
        end: Instant,
    ) -> Result<Duration, ()> {
        let benches_guard = self.benchgroups.guard();
        let bench = self
            .benchgroups
            .get(name.as_ref(), &benches_guard)
            .ok_or(())?;

        let duration = end - start;

        match bench {
            BenchGroup::Bucketed {
                buckets,
                bucket_width,
                ..
            } => {
                let nanos = duration.as_nanos();
                let bucket_idx = (nanos / bucket_width.get() as u128) as u64;

                let bucket_guard = buckets.guard();
                let count = buckets.get_or_insert(bucket_idx, AtomicU64::new(0), &bucket_guard);
                count.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            }
            BenchGroup::Individual { samples, .. } => {
                let nanos = duration.as_nanos();
                samples.lock().unwrap().push(nanos as u64);
            }
        }

        Ok(duration)
    }
}