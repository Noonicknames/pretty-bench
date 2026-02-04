use std::{
    sync::Arc,
    time::Instant,
};

use core::time::Duration;

use crate::{PrettyBench, PrettyBenchInner};

#[repr(C)]
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct DurationFFI {
    nanos: u32,
    secs: u64,
}

impl From<Duration> for DurationFFI {
    fn from(value: Duration) -> Self {
        Self {
            nanos: value.subsec_nanos(),
            secs: value.as_secs(),
        }
    }
}

impl Into<Duration> for DurationFFI {
    fn into(self) -> Duration {
        Duration::new(self.secs, self.nanos)
    }
}

/// FFI version of [PrettyBench]
#[repr(C)]
pub struct PrettyBenchFFI {
    inner: *const PrettyBenchInner,
}

impl PrettyBenchFFI {
    pub unsafe fn into_rust(self) -> PrettyBench {
        PrettyBench {
            inner: unsafe { Arc::from_raw(self.inner) },
        }
    }
    pub fn raw_copy(&self) -> Self {
        Self {
            inner: self.inner.clone(),
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

#[repr(C)]
#[derive(Clone)]
pub struct StrFFI {
    ptr: *const u8,
    len: u64,
}

impl From<&str> for StrFFI {
    fn from(value: &str) -> Self {
        let ptr = value.as_ptr();
        let len = value.len() as u64;
        StrFFI { ptr, len }
    }
}

impl<'a> TryInto<&'a str> for StrFFI {
    type Error = ();
    fn try_into(self) -> Result<&'a str, Self::Error> {
        let StrFFI { ptr, len } = self;
        if ptr.is_null() {
            return Err(());
        }

        unsafe {
            let slice = std::slice::from_raw_parts(ptr, len as usize);
            Ok(std::str::from_utf8(slice).unwrap_or("Invalid utf8"))
        }
    }
}

#[repr(C)]
#[derive(Clone)]
pub struct ArcStrFFI {
    ptr: *const u8,
    len: u64,
}

impl From<ArcStrFFI> for StrFFI {
    fn from(value: ArcStrFFI) -> Self {
        Self {
            ptr: value.ptr,
            len: value.len,
        }
    }
}

impl From<Arc<str>> for ArcStrFFI {
    fn from(value: Arc<str>) -> Self {
        let ptr = value.as_ptr();
        let len = value.len() as u64;

        _ = Arc::into_raw(value);

        ArcStrFFI { ptr, len }
    }
}

impl TryInto<Arc<str>> for ArcStrFFI {
    type Error = ();
    fn try_into(self) -> Result<Arc<str>, Self::Error> {
        let ArcStrFFI { ptr, len } = self;
        if ptr.is_null() {
            return Err(());
        }

        let arc_str = unsafe {
            let slice = std::slice::from_raw_parts(ptr, len as usize);
            std::str::from_utf8(slice).unwrap_or("Invalid utf8")
        };
        Ok(unsafe { Arc::from_raw(arc_str) })
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn pb_sleep(duration: DurationFFI) {
    spin_sleep::sleep(duration.into());
}

#[unsafe(no_mangle)]
pub extern "C" fn arc_str_new(ptr: *const u8, len: u64) -> ArcStrFFI {
    let arc_str = unsafe {
        let slice = std::slice::from_raw_parts(ptr, len as usize);
        std::str::from_utf8(slice).unwrap_or("Invalid utf8")
    };
    let arc_str: Arc<str> = arc_str.into();
    arc_str.into()
}

#[unsafe(no_mangle)]
pub extern "C" fn arc_str_clone(arc_str: ArcStrFFI) -> ArcStrFFI {
    let arc_str: Arc<str> = arc_str
        .try_into()
        .expect("ArcStr is null, perhaps it is being used after being dropped.");
    let copy = Arc::clone(&arc_str);
    let _: ArcStrFFI = arc_str.into();
    copy.into()
}

#[unsafe(no_mangle)]
pub extern "C" fn arc_str_drop(arc_str: *mut ArcStrFFI) {
    let arc_str_mut = unsafe { arc_str.as_mut() }.expect("Pointer to ArcStr is null.");
    let arc_str: Arc<str> = arc_str_mut
        .clone()
        .try_into()
        .expect("Tried to drop a null ArcStr, perhaps you are dropping it twice?.");
    arc_str_mut.len = 0;
    arc_str_mut.ptr = std::ptr::null();

    drop(arc_str);
}

#[unsafe(no_mangle)]
pub extern "C" fn pb_new() -> PrettyBenchFFI {
    let pretty_bench = PrettyBench::new();
    pretty_bench.into()
}

#[unsafe(no_mangle)]
pub extern "C" fn pb_new_group_individual(pretty_bench: PrettyBenchFFI, name: ArcStrFFI) {
    let pretty_bench = unsafe { pretty_bench.into_rust() };
    let name: Arc<str> = name
        .try_into()
        .expect("ArcStr is null, perhaps it is being used after being dropped.");
    pretty_bench.create_bench_group_individual(name.into());

    let _: PrettyBenchFFI = pretty_bench.into(); // Basically leak it again.
}

#[unsafe(no_mangle)]
pub extern "C" fn pb_start_bench(pretty_bench: PrettyBenchFFI) -> DurationFFI {
    let pretty_bench = unsafe { pretty_bench.into_rust() };
    let start_instant = pretty_bench.start_instant();

    let _: PrettyBenchFFI = pretty_bench.into(); // Basically leak it again.

    (Instant::now() - start_instant).into()
}

#[unsafe(no_mangle)]
pub extern "C" fn pb_end_bench(pretty_bench: PrettyBenchFFI, name: StrFFI, start: DurationFFI) {
    let now = Instant::now();
    let pretty_bench = unsafe { pretty_bench.into_rust() };
    let name: &str = name
        .try_into()
        .expect("ArcStr is null, perhaps it is being used after being dropped.");
    let start: Duration = start.into();
    let start_instant = pretty_bench.start_instant();
    pretty_bench.end_bench_with_instant(name, start_instant + start, now).unwrap();

    let _: PrettyBenchFFI = pretty_bench.into(); // Basically leak it again.
}

#[unsafe(no_mangle)]
pub extern "C" fn pb_import_from_file(pretty_bench: PrettyBenchFFI, src: StrFFI) {
    let pretty_bench = unsafe { pretty_bench.into_rust() };
    let src: &str = src.try_into().unwrap();
    let file = match std::fs::OpenOptions::new().read(true).open(src) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("{}", err);
            let _: PrettyBenchFFI = pretty_bench.into();
            return;
        }
    };

    let mut file = std::io::BufReader::new(file);
    if let Err(err) = pretty_bench.deserialise_import(&mut file) {
        eprintln!("{}", err);
    }
    let _: PrettyBenchFFI = pretty_bench.into();
}

#[unsafe(no_mangle)]
pub extern "C" fn pb_serialise_to_file(pretty_bench: PrettyBenchFFI, dest: StrFFI) {
    let pretty_bench = unsafe { pretty_bench.into_rust() };
    let dest: &str = dest.try_into().unwrap();
    let mut file = std::io::BufWriter::new(
        std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(dest)
            .unwrap(),
    );
    if let Err(err) = pretty_bench.serialise(&mut file) {
        eprintln!("{}", err);
    }
    let _: PrettyBenchFFI = pretty_bench.into();
}

#[unsafe(no_mangle)]
pub extern "C" fn pb_serialise_append_to_file(pretty_bench: PrettyBenchFFI, dest: StrFFI) {
    let pretty_bench = unsafe { pretty_bench.into_rust() };
    let dest: &str = dest.try_into().unwrap();
    let mut file = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(dest)
        .unwrap();
    if let Err(err) = pretty_bench.serialise(&mut file) {
        eprintln!("{}", err);
    }

    let _: PrettyBenchFFI = pretty_bench.into();
}

#[unsafe(no_mangle)]
pub extern "C" fn pb_print_histograms(pretty_bench: PrettyBenchFFI) {
    let pretty_bench = unsafe { pretty_bench.into_rust() };

    pretty_bench.print_histograms();

    let _: PrettyBenchFFI = pretty_bench.into();
}

#[unsafe(no_mangle)]
pub extern "C" fn pb_new_group_bucketed(
    pretty_bench: PrettyBenchFFI,
    name: ArcStrFFI,
    bucket_width: DurationFFI,
) {
    let pretty_bench = unsafe { pretty_bench.into_rust() };
    let name: Arc<str> = name
        .try_into()
        .expect("ArcStr is null, perhaps it is being used after being dropped.");

    pretty_bench.create_bench_group_bucketed(name, bucket_width.into());

    let _: PrettyBenchFFI = pretty_bench.into(); // Basically leak it again.
}

#[unsafe(no_mangle)]
pub extern "C" fn pb_drop(pretty_bench: *mut PrettyBenchFFI) {
    unsafe {
        pretty_bench.read().into_rust(); // Then gets dropped as an Arc
        pretty_bench.as_mut().expect("PrettyBench pointer is null, perhaps it is being used after being dropped or dropped twice?").inner = std::ptr::null_mut();
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn pb_clone(pretty_bench: PrettyBenchFFI) -> PrettyBenchFFI {
    let pretty_bench = unsafe { pretty_bench.into_rust() };
    let pb_clone = pretty_bench.clone();

    let _: PrettyBenchFFI = pretty_bench.into(); // Basically leak it again.

    pb_clone.into()
}
