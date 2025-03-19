@echo off

rem Avoid linking with vcruntime140.dll by statically linking everything,
rem and then explicitly linking with ucrtbase.dll dynamically.
rem We do this, because vcruntime140.dll is an optional Windows component.
set RUSTFLAGS=-Ctarget-feature=+crt-static -Clink-args=/DEFAULTLIB:ucrt.lib -Clink-args=/NODEFAULTLIB:vcruntime.lib -Clink-args=/NODEFAULTLIB:msvcrt.lib -Clink-args=/NODEFAULTLIB:libucrt.lib

rem The backtrace code for panics in Rust is almost as large as the entire editor.
rem = Huge reduction in binary size by removing all that.
rem cargo build --release -Zbuild-std=std,panic_abort -Zbuild-std-features=panic_immediate_abort %*
cargo build --release %*
