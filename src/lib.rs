use std::{
    io::Write, num::NonZeroU64, sync::{Arc, Mutex, atomic::AtomicU64}, time::{Duration, Instant}
};

use papaya::HashMap;

pub(crate) mod ffi;

#[repr(C)]
#[derive(Clone)]
pub struct PrettyBench {
    inner: Arc<PrettyBenchInner>,
}

pub struct PrettyBenchInner {
    benches: papaya::HashMap<Arc<str>, Bench>,
}

impl PrettyBenchInner {
    pub fn output_bench(&self, mut output: impl Write) -> std::io::Result<()> {
        write!(output, "wow")?;
        Ok(())
    }
    pub fn start_bench(&self, name: Arc<str>) -> u64 {
        let benches_guard = self.benches.guard();

        let bench = self.benches.get_or_insert_with(
            Arc::clone(&name),
            || Bench::Individual {
                idx_counter: AtomicU64::new(0),
                pending: HashMap::new(),
                samples: Mutex::new(Vec::new()),
            },
            &benches_guard,
        );

        let (Bench::Individual {
            idx_counter,
            pending,
            ..
        }
        | Bench::Bucketed {
            idx_counter,
            pending,
            ..
        }) = bench;

        let idx = idx_counter.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        pending.pin().insert(idx, Instant::now());
        idx
    }

    pub fn end_bench(&self, name: Arc<str>, idx: u64) -> Result<Duration, ()> {
        // Record time at the start, we don't want to record the time it takes to lock the mutex.
        let end_time = Instant::now();

        let benches_guard = self.benches.guard();
        let bench = self.benches.get(name.as_ref(), &benches_guard).ok_or(())?;

        let (Bench::Individual { pending, .. } | Bench::Bucketed { pending, .. }) = bench;

        let start_time = pending.pin().remove(&idx).ok_or(())?.clone();
        let duration = end_time - start_time;

        match bench {
            Bench::Bucketed {
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
            Bench::Individual { samples, .. } => {
                let nanos = duration.as_nanos();
                samples.lock().unwrap().push(nanos as u64);
            }
        }

        Ok(duration)
    }
}

impl PrettyBench {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(PrettyBenchInner {
                benches: HashMap::new(),
            }),
        }
    }
}

// Use cases:
//     - Track total time spent on each category of a piece of code.
//     - If there are multiple runs, I could show a distribution of the amount of time it takes to run them.
//
// IMPORTANT:
//     MPI is probably going to destroy this library even though it is thread safe.
//     The current idea is to collect all benchmark data, then afterwards figure out a way to coallesce all the data together after a given call.
//     Updated idea is that I am going to use a simple appendable format which is then going to be able to
#[derive(Debug)]
pub enum Bench {
    Individual {
        idx_counter: AtomicU64,
        pending: papaya::HashMap<u64, Instant>,
        samples: Mutex<Vec<u64>>, // Mutexes are probably ideal
    },
    Bucketed {
        idx_counter: AtomicU64,
        pending: papaya::HashMap<u64, Instant>,
        bucket_width: NonZeroU64,
        buckets: papaya::HashMap<u64, AtomicU64>,
    },
}

#[repr(u8)]
pub enum SampleType {
    Individual = 0,
    Bucketed = 1,
}

impl Bench {
    /// The format is composed of a header,
    /// `| length (8 bytes) | date (16 bytes) | type (1 byte) | bucket_width (8 bytes) | | name_length (8 bytes) | name (name_length bytes) |`
    ///
    /// followed by the samples data, which is either the following for individual sampling,
    /// `| sample1_nanos (8 bytes) | sample2_nanos (8 bytes) | sample3_nanos (8 bytes) | ...`
    ///
    /// or the following for bucketed sampling,
    ///
    /// `| bucket_idx (8 bytes) | count (8 bytes) | ...`
    pub fn serialise(&self, mut output: impl Write) -> std::io::Result<usize> {
        let date = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("Surely you aren't running a Rust program before 1970 (Unix epoch)");

        match self {
            Bench::Individual { samples, .. } => {
                let samples = samples.lock().unwrap();

                // Size of header + samples
                let length =
                    size_of::<u64>() + size_of::<u128>() + samples.len() * size_of::<u64>();
                output.write(&length.to_be_bytes())?;
                output.write(&date.as_nanos().to_be_bytes())?;

                for time in samples.iter() {
                    // Max time stored for a u64 is 584.5 years, should be enough.
                    output.write(&time.to_be_bytes())?;
                }
                Ok(length)
            }
            Bench::Bucketed {
                bucket_width,
                buckets,
                ..
            } => {
                let buckets_guard = buckets.guard();
                // Call before calling .len() since it will block resizes.
                let buckets_iter = buckets.iter(&buckets_guard);
                // Size of header + buckets, hopefully papaya guarantees length does not change.
                let length = size_of::<u64>()
                    + size_of::<u128>()
                    + size_of::<u64>()
                    + buckets.len() * size_of::<u64>();

                output.write(&length.to_be_bytes())?;
                output.write(&date.as_nanos().to_be_bytes())?;

                output.write(&bucket_width.get().to_be_bytes())?;

                for (bucket_idx, count) in buckets_iter {
                    output.write(&bucket_idx.to_be_bytes())?;
                    output.write(
                        &count
                            .load(std::sync::atomic::Ordering::Relaxed)
                            .to_be_bytes(),
                    )?;
                }

                Ok(length)
            }
        }
    }
    pub fn total_time(&self) -> Duration {
        match self {
            Bench::Bucketed {
                bucket_width,
                buckets,
                ..
            } => Duration::from_nanos(
                buckets
                    .pin()
                    .iter()
                    .map(|(idx, count)| {
                        idx * bucket_width.get() * count.load(std::sync::atomic::Ordering::Relaxed)
                    })
                    .sum(),
            ),
            Bench::Individual { samples, .. } => {
                let samples = samples.lock().unwrap();
                Duration::from_nanos(samples.iter().cloned().sum())
            }
        }
    }
    pub fn average_time(&self) -> Duration {
        match self {
            Bench::Bucketed {
                bucket_width,
                buckets,
                ..
            } => {
                let mut count_total = 0;
                let mut nanos_total = 0;
                buckets.pin().iter()
                    .for_each(|(idx, count)| {
                        let count = count.load(std::sync::atomic::Ordering::Relaxed);
                        count_total += count;
                        nanos_total += idx * bucket_width.get() * count;
                    });

                Duration::from_nanos(
                    nanos_total / count_total.max(1) // prevent divide by zero
                )
            },
            Bench::Individual { samples, .. } => {
                let samples = samples.lock().unwrap();
                Duration::from_nanos(samples.iter().cloned().sum::<u64>() / samples.len().max(1) as u64) // prevent divide by zero
            }
        }
    }
}
