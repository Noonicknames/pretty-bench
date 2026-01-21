use std::{sync::Arc, time::Duration};

use dashmap::DashMap;

#[repr(C)]
#[derive(Clone)]
pub struct PrettyBench {
    inner: Arc<PrettyBenchInner>,
} 

impl PrettyBenchFFI {
    pub unsafe fn into_rust(self) -> PrettyBench {
        PrettyBench {
            inner: unsafe { Arc::from_raw(self.inner) },
        }
    }
}

impl Into<PrettyBenchFFI> for PrettyBench {
    fn into(self) -> PrettyBenchFFI {
        PrettyBenchFFI {
            inner: Arc::into_raw(self.inner),
        }
    }
}


/// FFI version of [PrettyBench]
#[repr(C)]
pub struct PrettyBenchFFI {
    inner: *const PrettyBenchInner,
}

unsafe impl Send for PrettyBenchFFI {}
unsafe impl Sync for PrettyBenchFFI {}

impl PrettyBenchFFI {
    pub fn raw_copy(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

pub struct PrettyBenchInner {
    benches: DashMap<Arc<str>, Arc<Bench>>,
}

impl PrettyBenchInner {
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

#[unsafe(no_mangle)]
pub extern "C" fn pretty_bench_init() -> PrettyBenchFFI {
    println!("Bro just initialised pretty bench");

    let pretty_bench = PrettyBench::new();

    pretty_bench.into()
}

#[unsafe(no_mangle)]
pub extern "C" fn pretty_bench_destroy(pretty_bench: *mut PrettyBenchFFI) {
    unsafe {
        pretty_bench.as_ref().unwrap().raw_copy().into_rust(); // Then gets dropped as an Arc
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn pretty_bench_clone(pretty_bench: *const PrettyBenchFFI) -> PrettyBenchFFI {
    let pretty_bench = unsafe { pretty_bench.as_ref().unwrap().raw_copy().into_rust() };
    let pretty_bench_clone = pretty_bench.clone();

    let _: PrettyBenchFFI = pretty_bench.into(); // Basically leak it again.

    pretty_bench_clone.into()
}
