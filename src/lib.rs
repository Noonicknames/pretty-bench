use std::{sync::Arc, time::Duration};

use dashmap::DashMap;

pub(crate) mod ffi;

#[repr(C)]
#[derive(Clone)]
pub struct PrettyBench {
    inner: Arc<PrettyBenchInner>,
}

pub struct PrettyBenchInner {
    benches: DashMap<Arc<str>, Arc<Bench>>,
}

impl PrettyBenchInner {
    pub fn start_bench(&self, name: Arc<str>) -> (Arc<str>, usize) {
        let mut bench = self.benches.entry(name).or_insert_with(|| Arc::new(Bench {
            name: Arc::clone(&name),
            samples: Vec::new(),
        }));

        bench.samples.push(Duration::from_secs(1));

        (name, )
    }
    pub fn insert_bench(&self, name: Arc<str>) {
        let bench = Arc::new(Bench {
            name: Arc::clone(&name),
            samples: Vec::new(),
        });

        self.benches.insert(name, bench);
    }
}

impl PrettyBench {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(PrettyBenchInner {
                benches: DashMap::new(),
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
#[derive(Clone, Debug, Default)]
pub struct Bench {
    pub name: Arc<str>,
    pub samples: Vec<Duration>,
}

impl Bench {
    pub fn total_time(&self) -> Duration {
        self.samples.iter().sum()
    }
}
