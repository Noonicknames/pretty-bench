use std::sync::Arc;

use crate::{PrettyBench, PrettyBenchInner};

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
