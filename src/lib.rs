use std::{
    io::{Read, Write},
    num::NonZeroU64,
    str,
    sync::{Arc, Mutex, atomic::AtomicU64},
    time::{Duration, Instant, SystemTime},
};

pub(crate) mod ffi;

#[repr(C)]
#[derive(Clone)]
pub struct PrettyBench {
    pub inner: Arc<PrettyBenchInner>,
}

pub struct PrettyBenchInner {
    benchgroups: papaya::HashMap<Arc<str>, BenchGroup>,
}

impl PrettyBenchInner {
    pub fn output_bench(&self, mut output: impl Write) -> std::io::Result<()> {
        write!(output, "wow")?;
        Ok(())
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
    pub fn start_bench(&self, name: impl AsRef<str>) -> u64 {
        let name = name.as_ref();
        let benches_guard = self.benchgroups.guard();

        let bench = self
            .benchgroups
            .get(name, &benches_guard)
            .unwrap_or_else(|| {
                let name: Arc<str> = name.into();
                self.benchgroups.get_or_insert_with(
                    Arc::clone(&name),
                    || BenchGroup::Individual {
                        name,
                        date: SystemTime::now(),
                        idx_counter: AtomicU64::new(0),
                        pending: papaya::HashMap::new(),
                        samples: Mutex::new(Vec::new()),
                    },
                    &benches_guard,
                )
            });

        let (BenchGroup::Individual {
            idx_counter,
            pending,
            ..
        }
        | BenchGroup::Bucketed {
            idx_counter,
            pending,
            ..
        }) = bench;

        let idx = idx_counter.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        pending.pin().insert(idx, Instant::now());
        idx
    }

    pub fn end_bench(&self, name: impl AsRef<str>, idx: u64) -> Result<Duration, ()> {
        // Record time at the start, we don't want to record the time it takes to lock the mutex.
        let end_time = Instant::now();
        self.end_bench_with_instant(name, idx, end_time)
    }

    pub fn end_bench_with_instant(&self, name: impl AsRef<str>, idx: u64, end_time: Instant) -> Result<Duration, ()> {
        let benches_guard = self.benchgroups.guard();
        let bench = self
            .benchgroups
            .get(name.as_ref(), &benches_guard)
            .ok_or(())?;

        let (BenchGroup::Individual { pending, .. } | BenchGroup::Bucketed { pending, .. }) = bench;

        let start_time = pending.pin().remove(&idx).ok_or(())?.clone();
        let duration = end_time - start_time;

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

impl PrettyBench {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(PrettyBenchInner {
                benchgroups: papaya::HashMap::new(),
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
pub enum BenchGroup {
    Individual {
        name: Arc<str>,
        date: SystemTime,
        idx_counter: AtomicU64,
        pending: papaya::HashMap<u64, Instant>,
        samples: Mutex<Vec<u64>>, // Mutexes are probably ideal
    },
    Bucketed {
        name: Arc<str>,
        date: SystemTime,
        idx_counter: AtomicU64,
        pending: papaya::HashMap<u64, Instant>,
        bucket_width: NonZeroU64,
        buckets: papaya::HashMap<u64, AtomicU64>,
    },
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SampleType {
    Individual = 0,
    Bucketed = 1,
}

impl TryFrom<u8> for SampleType {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Individual),
            1 => Ok(Self::Bucketed),
            _ => Err(()),
        }
    }
}

impl BenchGroup {
    pub fn new_individual(name: Arc<str>) -> Self {
        Self::Individual {
            name,
            date: SystemTime::now(),
            idx_counter: AtomicU64::new(0),
            pending: papaya::HashMap::new(),
            samples: Mutex::new(Vec::new()),
        }
    }
    pub fn new_bucketed(name: Arc<str>, bucket_width: Duration) -> Self {
        Self::Bucketed {
            name,
            date: SystemTime::now(),
            idx_counter: AtomicU64::new(0),
            pending: papaya::HashMap::new(),
            bucket_width: NonZeroU64::new(bucket_width.as_nanos() as u64).unwrap(),
            buckets: papaya::HashMap::new(),
        }
    }
    pub fn empty_clone(&self) -> Self {
        match self {
            Self::Bucketed {
                name,
                date,
                bucket_width,
                ..
            } => Self::Bucketed {
                name: Arc::clone(name),
                date: *date,
                idx_counter: AtomicU64::new(0),
                pending: papaya::HashMap::new(),
                bucket_width: *bucket_width,
                buckets: papaya::HashMap::new(),
            },
            Self::Individual { name, date, .. } => Self::Individual {
                name: Arc::clone(name),
                date: *date,
                idx_counter: AtomicU64::new(0),
                pending: papaya::HashMap::new(),
                samples: Mutex::new(Vec::new()),
            },
        }
    }
    pub fn print_histogram(&self) {
        const BINS: usize = 32;
        const MAX_HIST_HEIGHT: usize = 150;
        match self {
            Self::Bucketed { .. } => {
                todo!()
            }
            Self::Individual { samples, .. } => {
                let samples = samples.lock().unwrap();

                if samples.is_empty() {
                    println!("No data.");
                    return;
                }

                let mut time_min = u64::MAX;
                let mut time_max = u64::MIN;

                // Get mins and maxes
                for &sample in samples.iter() {
                    time_min = time_min.min(sample);
                    time_max = time_max.max(sample);
                }

                // Since we checked samples is not empty, it is safe to assume time_min and time_max are not still u64::MAX and u64::MIN.
                // time_min <= time_max

                let Some(range) = NonZeroU64::new(time_max - time_min) else {
                    println!(
                        "{:.5e} | {:#<width$}",
                        time_min as f32 * 1e-9,
                        "",
                        width = samples.len()
                    );
                    return;
                };

                let mut bins = vec![0u64; BINS];

                for &sample in samples.iter() {
                    bins[((sample - time_min) * BINS as u64 / (range.get() + 1)) as usize] += 1;
                }

                let tallest_bin = bins.iter().cloned().max().unwrap();

                for (idx, &bin) in bins.iter().enumerate() {
                    let time = time_min
                        + idx as u64 * range.get() / BINS as u64
                        + range.get() / BINS as u64 / 2;

                    let hist_height = if tallest_bin > MAX_HIST_HEIGHT as u64 {
                        (bin * MAX_HIST_HEIGHT as u64) / tallest_bin
                    } else {
                        bin
                    };
                    println!(
                        "{:.5e} | {:#<width$}",
                        time as f32 * 1e-9,
                        "",
                        width = hist_height as usize
                    );
                }
            }
        }
    }
    pub fn name(&self) -> &Arc<str> {
        match self {
            Self::Bucketed { name, .. } | Self::Individual { name, .. } => &name,
        }
    }
    pub fn sample_type(&self) -> SampleType {
        match self {
            Self::Bucketed { .. } => SampleType::Bucketed,
            Self::Individual { .. } => SampleType::Individual,
        }
    }
    pub fn date(&self) -> SystemTime {
        match self {
            Self::Bucketed { date, .. } => *date,
            Self::Individual { date, .. } => *date,
        }
    }
    /// The format is composed of a header,
    /// `| length (8 bytes) | date (16 bytes) | type (1 byte) | bucket_width? (8 bytes) | name_length (2 bytes) | name (name_length bytes) |`
    ///
    /// followed by the samples data, which is either the following for individual sampling,
    /// `| sample1_nanos (8 bytes) | sample2_nanos (8 bytes) | sample3_nanos (8 bytes) | ...`
    ///
    /// or the following for bucketed sampling,
    ///
    /// `| bucket_idx (8 bytes) | count (8 bytes) | ...`
    pub fn serialise(&self, mut output: impl Write) -> std::io::Result<usize> {
        let date_since_epoch = self
            .date()
            .duration_since(std::time::SystemTime::UNIX_EPOCH)
            .expect("Surely this code isn't being run before 1970.")
            .as_nanos();
        match self {
            BenchGroup::Individual { name, samples, .. } => {
                let samples = samples.lock().unwrap();

                let name_length = name.len();

                // Size of header + samples
                let length = size_of::<u64>() // length
                    + size_of::<u128>() // date
                    + size_of::<u8>() // type
                    + size_of::<u16>() // name length
                    + name_length // name
                    + samples.len() * size_of::<u64>(); // samples

                // Length, date, type
                output.write(&length.to_be_bytes())?;
                output.write(&date_since_epoch.to_be_bytes())?;
                output.write(&(self.sample_type() as u8).to_be_bytes())?;

                // Write name
                let name_length = name_length as u16;
                output.write(&name_length.to_be_bytes())?;
                output.write(name.as_bytes())?;

                for time in samples.iter() {
                    // Max time stored for a u64 is 584.5 years, should be enough.
                    output.write(&time.to_be_bytes())?;
                }
                Ok(length)
            }
            BenchGroup::Bucketed {
                name,
                bucket_width,
                buckets,
                ..
            } => {
                let buckets_guard = buckets.guard();
                // Call before calling .len() since it will block resizes.
                let buckets_iter = buckets.iter(&buckets_guard);
                let name_length = name.len();
                // Size of header + buckets, hopefully papaya guarantees length does not change.
                let length = size_of::<u64>() // length
                    + size_of::<u128>() // date
                    + size_of::<u8>() // type
                    + size_of::<u64>() // bucket width
                    + size_of::<u16>() // name length
                    + name_length // name
                    + buckets.len() * (size_of::<u64>() + size_of::<u64>()); // buckets

                // length, date, type, bucket width
                output.write(&length.to_be_bytes())?;
                output.write(&date_since_epoch.to_be_bytes())?;
                output.write(&(self.sample_type() as u8).to_be_bytes())?;
                output.write(&bucket_width.get().to_be_bytes())?;

                // Write name
                let name_length = name_length as u16;
                output.write(&name_length.to_be_bytes())?;
                output.write(name.as_bytes())?;

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

    pub fn import(&self, bench_group: &Self) {
        match (self, bench_group) {
            (
                Self::Bucketed {
                    bucket_width,
                    buckets,
                    ..
                },
                Self::Bucketed {
                    bucket_width: other_bucket_width,
                    buckets: other_buckets,
                    ..
                },
            ) => {
                let buckets = buckets.pin();
                let other_buckets = other_buckets.pin();

                for (idx, other_count) in other_buckets.iter() {
                    let count = buckets.get_or_insert(
                        (*idx * other_bucket_width.get() + other_bucket_width.get() / 2)
                            / bucket_width.get(),
                        AtomicU64::new(0),
                    );
                    count.fetch_add(
                        other_count.load(std::sync::atomic::Ordering::Relaxed),
                        std::sync::atomic::Ordering::Relaxed,
                    );
                }
            }
            (
                Self::Bucketed {
                    bucket_width,
                    buckets,
                    ..
                },
                Self::Individual {
                    samples: other_samples,
                    ..
                },
            ) => {
                let buckets = buckets.pin();
                // I might want to rework this into collecting into a hashmap with counts first.
                for sample_time in other_samples.lock().unwrap().iter() {
                    let count =
                        buckets.get_or_insert(*sample_time / bucket_width.get(), AtomicU64::new(0));
                    count.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                }
            }
            (
                Self::Individual { samples, .. },
                Self::Bucketed {
                    bucket_width: other_bucket_width,
                    buckets: other_buckets,
                    ..
                },
            ) => {
                let other_buckets = other_buckets.pin();
                let other_buckets_iter = other_buckets.iter(); // Since this might block
                let mut samples = samples.lock().unwrap();
                for (idx, other_count) in other_buckets_iter {
                    let time_nanos = *idx * other_bucket_width.get() + other_bucket_width.get() / 2;
                    (0..other_count.load(std::sync::atomic::Ordering::Relaxed))
                        .for_each(|_| samples.push(time_nanos));
                }
            }
            (
                Self::Individual { samples, .. },
                Self::Individual {
                    samples: other_samples,
                    ..
                },
            ) => {
                let mut samples = samples.lock().unwrap();
                let other_samples = other_samples.lock().unwrap();
                samples.extend_from_slice(&other_samples);
            }
        }
    }

    pub fn deserialise(mut input: impl Read) -> std::io::Result<Self> {
        const HEADER_SIZE: usize = 25;
        let mut buf = [0u8; 32];

        input.read_exact(&mut buf[..HEADER_SIZE])?;
        let length = u64::from_be_bytes(buf[0..size_of::<u64>()].try_into().unwrap());
        let mut input = input.take(length - HEADER_SIZE as u64);
        let date = u128::from_be_bytes(
            buf[size_of::<u64>()..size_of::<u64>() + size_of::<u128>()]
                .try_into()
                .unwrap(),
        );
        let date = std::time::UNIX_EPOCH
            + std::time::Duration::new(
                (date / 1_000_000_000u128) as u64,
                (date % 1_000_000_000u128) as u32,
            );
        let sample_type_num = u8::from_be_bytes(
            buf[size_of::<u64>() + size_of::<u128>()
                ..size_of::<u64>() + size_of::<u128>() + size_of::<u8>()]
                .try_into()
                .unwrap(),
        );
        let sample_type: SampleType = sample_type_num.try_into().map_err(|_| {
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!(
                    "Invalid sampling type, expected 0 or 1, got {}",
                    sample_type_num
                ),
            )
        })?;

        let bench;

        match sample_type {
            SampleType::Bucketed => {
                input.read_exact(&mut buf[..size_of::<u64>()])?;
                let bucket_width = NonZeroU64::new(u64::from_be_bytes(
                    buf[..size_of::<u64>()].try_into().unwrap(),
                ))
                .ok_or_else(|| {
                    std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        "Bucket width cannot be zero.",
                    )
                })?;

                input.read_exact(&mut buf[..size_of::<u16>()])?;
                let name_length = u16::from_be_bytes(buf[..size_of::<u16>()].try_into().unwrap());
                let mut name_bytes =
                    unsafe { Arc::<[u8]>::new_uninit_slice(name_length as usize).assume_init() };
                input.read_exact(unsafe { Arc::get_mut(&mut name_bytes).unwrap_unchecked() })?;
                let name = unsafe {
                    Arc::<str>::from_raw(
                        std::str::from_utf8(&name_bytes).map_err(std::io::Error::other)?,
                    )
                };
                _ = Arc::into_raw(name_bytes);

                let buckets = papaya::HashMap::new();
                let buckets_guard = buckets.guard();

                let mut buf_start = 0;
                loop {
                    let read_length = input.read(&mut buf[buf_start..])?;
                    if read_length == 0 {
                        break;
                    }

                    let (buckets_bytes, remainder) =
                        buf[0..buf_start + read_length].as_chunks::<16>();

                    for bucket_bytes in buckets_bytes {
                        let ([idx_bytes, count_bytes], []) = bucket_bytes.as_chunks() else {
                            unreachable!()
                        };
                        let idx = u64::from_be_bytes(*idx_bytes);
                        let count = u64::from_be_bytes(*count_bytes);

                        buckets.insert(idx, AtomicU64::new(count), &buckets_guard);
                    }
                    let read_len = buckets_bytes.len() * 8;
                    let remainder_len = remainder.len();
                    buf.copy_within(remainder_len..remainder_len + read_len, 0);
                    buf_start = remainder_len;
                }

                drop(buckets_guard);
                bench = BenchGroup::Bucketed {
                    name,
                    date,
                    idx_counter: AtomicU64::new(0),
                    pending: papaya::HashMap::new(),
                    bucket_width,
                    buckets,
                };
            }
            SampleType::Individual => {
                input.read_exact(&mut buf[..size_of::<u16>()])?;
                let name_length = u16::from_be_bytes(buf[..size_of::<u16>()].try_into().unwrap());
                let mut name_bytes =
                    unsafe { Arc::<[u8]>::new_uninit_slice(name_length as usize).assume_init() };
                input.read_exact(unsafe { Arc::get_mut(&mut name_bytes).unwrap_unchecked() })?;
                let name = unsafe {
                    Arc::<str>::from_raw(
                        std::str::from_utf8(&name_bytes).map_err(std::io::Error::other)?,
                    )
                };
                _ = Arc::into_raw(name_bytes);

                let mut samples_vec = Vec::new();

                let mut buf_start = 0;
                loop {
                    let read_length = input.read(&mut buf[buf_start..])?;
                    if read_length == 0 {
                        break;
                    }

                    let (samples, remainder) = buf[0..buf_start + read_length].as_chunks::<8>();

                    for sample in samples {
                        let sample = u64::from_be_bytes(*sample);
                        samples_vec.push(sample)
                    }
                    let read_len = samples.len() * 8;
                    let remainder_len = remainder.len();
                    buf.copy_within(remainder_len..remainder_len + read_len, 0);
                    buf_start = remainder_len;
                }

                bench = BenchGroup::Individual {
                    name,
                    date,
                    idx_counter: AtomicU64::new(0),
                    pending: papaya::HashMap::new(),
                    samples: Mutex::new(samples_vec),
                };
            }
        }

        Ok(bench)
    }
    pub fn total_time(&self) -> Duration {
        match self {
            BenchGroup::Bucketed {
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
            BenchGroup::Individual { samples, .. } => {
                let samples = samples.lock().unwrap();
                Duration::from_nanos(samples.iter().cloned().sum())
            }
        }
    }
    pub fn average_time(&self) -> Duration {
        match self {
            BenchGroup::Bucketed {
                bucket_width,
                buckets,
                ..
            } => {
                let mut count_total = 0;
                let mut nanos_total = 0;
                buckets.pin().iter().for_each(|(idx, count)| {
                    let count = count.load(std::sync::atomic::Ordering::Relaxed);
                    count_total += count;
                    nanos_total += (idx * bucket_width.get() + bucket_width.get() / 2) * count;
                });

                Duration::from_nanos(
                    nanos_total / count_total.max(1), // prevent divide by zero
                )
            }
            BenchGroup::Individual { samples, .. } => {
                let samples = samples.lock().unwrap();
                Duration::from_nanos(
                    samples.iter().cloned().sum::<u64>() / samples.len().max(1) as u64,
                ) // prevent divide by zero
            }
        }
    }
}
