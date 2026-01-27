//! Use cases:
//!     - Track total time spent on each category of a piece of code.
//!     - If there are multiple runs, I could show a distribution of the amount of time it takes to run them.
//!
//! IMPORTANT:
//!     MPI is probably going to destroy this library even though it is thread safe.
//!     The current idea is to collect all benchmark data, then afterwards figure out a way to coallesce all the data together after a given call.
//!     Updated idea is that I am going to use a simple appendable format which is then going to be able to

mod bench_group;
mod pretty_bench;

#[cfg(feature = "ffi")]
pub mod ffi;

pub use self::{bench_group::*, pretty_bench::*};