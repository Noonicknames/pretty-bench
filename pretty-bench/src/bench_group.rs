use std::{
    io::{Read, Write},
    sync::{Arc, Mutex},
    time::SystemTime,
};

use core::{
    convert::{TryFrom, TryInto},
    num::NonZeroU64,
    sync::atomic::AtomicU64,
    time::Duration,
};

#[derive(Debug)]
pub enum BenchGroup {
    Individual {
        name: Arc<str>,
        date: SystemTime,
        samples: Mutex<Vec<u64>>, // Mutexes are probably ideal
    },
    Bucketed {
        name: Arc<str>,
        date: SystemTime,
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
            samples: Mutex::new(Vec::new()),
        }
    }
    pub fn new_bucketed(name: Arc<str>, bucket_width: Duration) -> Self {
        Self::Bucketed {
            name,
            date: SystemTime::now(),
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
                bucket_width: *bucket_width,
                buckets: papaya::HashMap::new(),
            },
            Self::Individual { name, date, .. } => Self::Individual {
                name: Arc::clone(name),
                date: *date,
                samples: Mutex::new(Vec::new()),
            },
        }
    }
    pub fn print_histogram(&self) {
        const BINS: usize = 32;
        const MAX_HIST_HEIGHT: usize = 150;
        const VIEW_STANDARD_DEVIATIONS: f64 = 1.0;
        const OUTLIER_STANDARD_DEVIATIONS: f64 = 3.0;

        let mut average: f64;
        let mut standard_deviation: f64;
        let mut sample_count: u64;
        let mut outlier_count: u64;
        let lower_time: u64;
        let upper_time: u64;
        let time_range: NonZeroU64;
        let tallest_bin: u64;
        let mut bins: Vec<u64>;

        match self {
            Self::Bucketed {
                bucket_width,
                buckets,
                ..
            } => {
                let buckets_guard = buckets.guard();
                let buckets = buckets
                    .iter(&buckets_guard)
                    .map(|(idx, bucket)| {
                        (
                            *idx * bucket_width.get() + bucket_width.get() / 2,
                            bucket.load(std::sync::atomic::Ordering::Relaxed),
                        )
                    })
                    .collect::<Vec<_>>();

                if buckets.is_empty() {
                    println!("No data.");
                    return;
                }

                sample_count = 0;
                average = 0.0;
                standard_deviation = 0.0;

                // Get average, sample_count and standard deviation
                for &(time, count) in buckets.iter() {
                    average += (time * count) as f64;
                    sample_count += count;
                    standard_deviation += (time as f64) * (time as f64) * count as f64;
                }
                average = average / sample_count as f64;
                standard_deviation =
                    f64::sqrt(standard_deviation / sample_count as f64 - average * average);

                let lower_outlier = (average - standard_deviation * OUTLIER_STANDARD_DEVIATIONS)
                    .floor()
                    .max(0.0) as u64;
                let upper_outlier = (average + standard_deviation * OUTLIER_STANDARD_DEVIATIONS)
                    .floor()
                    .max(0.0) as u64;

                outlier_count = 0;
                sample_count = 0;
                average = 0.0;
                standard_deviation = 0.0;
                // Get average, sample_count and standard deviation without outliers
                for &(time, count) in buckets.iter() {
                    if (lower_outlier..=upper_outlier).contains(&time) {
                        average += (time * count) as f64;
                        sample_count += count;
                        standard_deviation += (time as f64) * (time as f64) * count as f64;
                    } else {
                        outlier_count += count;
                    }
                }
                average = average / sample_count as f64;
                standard_deviation =
                    f64::sqrt(standard_deviation / sample_count as f64 - average * average);

                lower_time = (average - standard_deviation * VIEW_STANDARD_DEVIATIONS)
                    .floor()
                    .max(0.0) as u64;
                upper_time = (average + standard_deviation * VIEW_STANDARD_DEVIATIONS)
                    .floor()
                    .max(0.0) as u64;
                time_range = unsafe { NonZeroU64::new_unchecked(upper_time - lower_time + 1) };

                bins = vec![0u64; BINS];

                for &(time, count) in buckets.iter() {
                    if (lower_time..=upper_time).contains(&time) {
                        bins[((time - lower_time) * BINS as u64 / time_range) as usize] += count;
                    }
                }

                tallest_bin = bins.iter().cloned().max().unwrap();
            }
            Self::Individual { samples, .. } => {
                let samples = samples.lock().unwrap();

                if samples.is_empty() {
                    println!("No data.");
                    return;
                }

                sample_count = samples.len() as u64;
                average = 0.0;
                standard_deviation = 0.0;

                // Get average, sample_count and standard deviation
                for &sample_time in samples.iter() {
                    average += sample_time as f64;
                    standard_deviation += (sample_time as f64) * (sample_time as f64);
                }
                average = average / sample_count as f64;
                standard_deviation =
                    f64::sqrt(standard_deviation / sample_count as f64 - average * average);

                let lower_outlier = (average - standard_deviation * OUTLIER_STANDARD_DEVIATIONS)
                    .floor()
                    .max(0.0) as u64;
                let upper_outlier = (average + standard_deviation * OUTLIER_STANDARD_DEVIATIONS)
                    .floor()
                    .max(0.0) as u64;

                outlier_count = 0;
                sample_count = 0;
                average = 0.0;
                standard_deviation = 0.0;
                // Get average, sample_count and standard deviation without outliers
                for &sample_time in samples.iter() {
                    if (lower_outlier..=upper_outlier).contains(&sample_time) {
                        average += sample_count as f64;
                        sample_count += 1;
                        standard_deviation += (sample_time as f64) * (sample_time as f64);
                    } else {
                        outlier_count += 1;
                    }
                }
                average = average / sample_count as f64;
                standard_deviation =
                    f64::sqrt(standard_deviation / sample_count as f64 - average * average);

                lower_time = (average - standard_deviation * VIEW_STANDARD_DEVIATIONS)
                    .floor()
                    .max(0.0) as u64;
                upper_time = (average + standard_deviation * VIEW_STANDARD_DEVIATIONS)
                    .floor()
                    .max(0.0) as u64;
                time_range = unsafe { NonZeroU64::new_unchecked(upper_time - lower_time + 1) };

                bins = vec![0u64; BINS];

                for &sample_time in samples.iter() {
                    if (lower_time..=upper_time).contains(&sample_time) {
                        bins[((sample_time - lower_time) * BINS as u64 / time_range.get())
                            as usize] += 1;
                    }
                }

                tallest_bin = bins.iter().cloned().max().unwrap();
            }
        }

        println!(
            "   Average: {:.4e}, Standard deviation: {:.4e}, Outliers: {}",
            standard_deviation * 1e-9,
            average * 1e-9,
            outlier_count,
        );

        for (idx, &bin) in bins.iter().enumerate() {
            let time = lower_time
                + idx as u64 * time_range.get() / BINS as u64
                + time_range.get() / BINS as u64 / 2;

            let hist_height = if tallest_bin > MAX_HIST_HEIGHT as u64 {
                (bin * MAX_HIST_HEIGHT as u64) / tallest_bin
            } else {
                bin
            };
            println!(
                "{:.4e} | {:#<width$}",
                time as f32 * 1e-9,
                "",
                width = hist_height as usize
            );
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
                output.write(&length.to_le_bytes())?;
                output.write(&date_since_epoch.to_le_bytes())?;
                output.write(&(self.sample_type() as u8).to_le_bytes())?;

                // Write name
                let name_length = name_length as u16;
                output.write(&name_length.to_le_bytes())?;
                output.write(name.as_bytes())?;

                for time in samples.iter() {
                    // Max time stored for a u64 is 584.5 years, should be enough.
                    output.write(&time.to_le_bytes())?;
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
                output.write(&length.to_le_bytes())?;
                output.write(&date_since_epoch.to_le_bytes())?;
                output.write(&(self.sample_type() as u8).to_le_bytes())?;
                output.write(&bucket_width.get().to_le_bytes())?;

                // Write name
                let name_length = name_length as u16;
                output.write(&name_length.to_le_bytes())?;
                output.write(name.as_bytes())?;

                for (bucket_idx, count) in buckets_iter {
                    output.write(&bucket_idx.to_le_bytes())?;
                    output.write(
                        &count
                            .load(std::sync::atomic::Ordering::Relaxed)
                            .to_le_bytes(),
                    )?;
                }
                Ok(length)
            }
        }
    }

    // There seems to be a bug with importing bucketed data with different bucket widths
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

                for (other_idx, other_count) in other_buckets.iter() {
                    let count = buckets.get_or_insert(
                        (*other_idx * other_bucket_width.get() + other_bucket_width.get() / 2)
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
        let length = u64::from_le_bytes(buf[0..size_of::<u64>()].try_into().unwrap());
        let mut input = input.take(length - HEADER_SIZE as u64);
        let date = u128::from_le_bytes(
            buf[size_of::<u64>()..size_of::<u64>() + size_of::<u128>()]
                .try_into()
                .unwrap(),
        );
        let date = std::time::UNIX_EPOCH
            + std::time::Duration::new(
                (date / 1_000_000_000u128) as u64,
                (date % 1_000_000_000u128) as u32,
            );
        let sample_type_num = u8::from_le_bytes(
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
                let bucket_width = NonZeroU64::new(u64::from_le_bytes(
                    buf[..size_of::<u64>()].try_into().unwrap(),
                ))
                .ok_or_else(|| {
                    std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        "Bucket width cannot be zero.",
                    )
                })?;

                input.read_exact(&mut buf[..size_of::<u16>()])?;
                let name_length = u16::from_le_bytes(buf[..size_of::<u16>()].try_into().unwrap());
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
                        let idx = u64::from_le_bytes(*idx_bytes);
                        let count = u64::from_le_bytes(*count_bytes);

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
                    bucket_width,
                    buckets,
                };
            }
            SampleType::Individual => {
                input.read_exact(&mut buf[..size_of::<u16>()])?;
                let name_length = u16::from_le_bytes(buf[..size_of::<u16>()].try_into().unwrap());
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
                        let sample = u64::from_le_bytes(*sample);
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
