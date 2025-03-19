#[cfg(unix)]
mod unix;
#[cfg(windows)]
#[macro_use]
mod windows;

#[cfg(unix)]
pub use unix::*;
#[cfg(windows)]
pub use windows::*;
