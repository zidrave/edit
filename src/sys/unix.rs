use crate::apperr;
use std::ffi::{CStr, c_int, c_void};
use std::fs::File;
use std::io::{ErrorKind, Read, Write};
use std::mem::{self, ManuallyDrop, MaybeUninit};
use std::os::fd::FromRawFd;
use std::ptr::{null, null_mut};
use std::thread;
use std::time;

pub fn preferred_languages() -> Vec<String> {
    let mut locales = Vec::new();

    for key in ["LANGUAGE", "LC_ALL", "LANG"] {
        if let Ok(val) = std::env::var(key) {
            locales.extend(
                val.split(':')
                    .filter(|val| !val.is_empty())
                    .map(String::from),
            );
        }
    }

    locales
}

extern "C" fn sigwinch_handler(_: libc::c_int) {
    unsafe {
        STATE.inject_resize = true;
    }
}

pub fn init() -> apperr::Result<()> {
    unsafe {
        // Reopen stdin/stdout if they're redirected.
        if libc::isatty(STATE.stdin) == 0 {
            STATE.stdin = check_int_return(libc::open(c"/dev/tty".as_ptr(), libc::O_RDONLY))?;
        }
        if libc::isatty(STATE.stdout) == 0 {
            STATE.stdout = check_int_return(libc::open(c"/dev/tty".as_ptr(), libc::O_WRONLY))?;
        }

        check_int_return(libc::tcgetattr(
            STATE.stdout,
            &raw mut STATE.stdout_initial_termios,
        ))?;

        let mut termios = STATE.stdout_initial_termios;
        termios.c_lflag &= !(libc::ICANON | libc::ECHO);
        check_int_return(libc::tcsetattr(STATE.stdout, libc::TCSANOW, &termios))?;

        // Set STATE.inject_resize to true whenever we get a SIGWINCH.
        let mut sigwinch_action: libc::sigaction = mem::zeroed();
        sigwinch_action.sa_sigaction = sigwinch_handler as libc::sighandler_t;
        check_int_return(libc::sigaction(
            libc::SIGWINCH,
            &sigwinch_action,
            null_mut(),
        ))?;

        Ok(())
    }
}

pub fn deinit() {
    unsafe {
        libc::tcsetattr(
            STATE.stdout,
            libc::TCSANOW,
            &raw mut STATE.stdout_initial_termios,
        );
    }
}

pub fn inject_window_size_into_stdin() {
    unsafe {
        STATE.inject_resize = true;
    }
}

fn get_window_size() -> (u16, u16) {
    let mut w = 0;
    let mut h = 0;

    for attempt in 1.. {
        let winsz = unsafe {
            let mut winsz: libc::winsize = mem::zeroed();
            libc::ioctl(STATE.stdout, libc::TIOCGWINSZ, &raw mut winsz);
            winsz
        };

        w = winsz.ws_col;
        h = winsz.ws_row;
        if w != 0 && h != 0 {
            break;
        }

        if attempt == 10 {
            w = 80;
            h = 24;
            break;
        }

        // Some terminals are bad emulators and don't report TIOCGWINSZ immediately.
        thread::sleep(time::Duration::from_millis(10 * attempt));
    }

    (w, h)
}

struct State {
    stdin: libc::c_int,
    stdout: libc::c_int,
    stdout_initial_termios: libc::termios,
    inject_resize: bool,
    // Buffer for incomplete UTF-8 sequences (max 4 bytes needed)
    utf8_buf: [u8; 4],
    utf8_len: usize,
}

static mut STATE: State = State {
    stdin: libc::STDIN_FILENO,
    stdout: libc::STDOUT_FILENO,
    stdout_initial_termios: unsafe { mem::zeroed() },
    inject_resize: false,
    utf8_buf: [0; 4],
    utf8_len: 0,
};

/// Reads from stdin.
///
/// Returns `None` if there was an error reading from stdin.
/// Returns `Some("")` if the given timeout was reached.
/// Otherwise, it returns the read, non-empty string.
pub fn read_stdin(timeout: Option<time::Duration>) -> Option<String> {
    unsafe {
        if let Some(timeout) = timeout {
            let mut pollfd = libc::pollfd {
                fd: STATE.stdin,
                events: libc::POLLIN,
                revents: 0,
            };
            let ts = libc::timespec {
                tv_sec: timeout.as_secs() as libc::time_t,
                tv_nsec: timeout.subsec_nanos() as libc::c_long,
            };
            let ret = libc::ppoll(&mut pollfd, 1, &ts, null());
            if ret < 0 {
                return None;
            }
            if ret == 0 {
                return Some(String::new());
            }
        }

        #[allow(invalid_value)]
        let mut buf: [u8; 1024] = MaybeUninit::uninit().assume_init();
        let mut read = 0;

        if STATE.utf8_len != 0 {
            read = STATE.utf8_len;
            input[..read].copy_from_slice(&STATE.utf8_buf[..read]);
        }

        loop {
            if STATE.inject_resize {
                STATE.inject_resize = false;
                let (w, h) = get_window_size();
                return Some(format!("\x1b[8;{};{}t", h, w));
            }

            // Read new data
            let n = loop {
                let ret = libc::read(STATE.stdin, buf.as_mut_ptr() as *mut _, buf.len());
                if ret > 0 {
                    break ret as usize;
                }
                if ret == 0 {
                    return None;
                }
                if *libc::__errno_location() != libc::EINTR {
                    return None;
                }
            };

            // Prepend any cached incomplete UTF-8 sequence
            let input = if STATE.utf8_len > 0 {
                let total = STATE.utf8_len + n;
                let mut combined = Vec::with_capacity(total);
                combined.extend_from_slice(&STATE.utf8_buf[..STATE.utf8_len]);
                combined.extend_from_slice(&buf[..n]);
                STATE.utf8_len = 0;
                combined
            } else {
                buf[..n].to_vec()
            };

            // Find last complete UTF-8 sequence
            let mut valid_end = input.len();
            while valid_end > 0 && (input[valid_end - 1] & 0xC0) == 0x80 {
                valid_end -= 1;
                if input.len() - valid_end >= 4 || valid_end == 0 {
                    // Either too many trail bytes or all trail bytes - invalid UTF-8
                    valid_end = input.len();
                    break;
                }
            }

            // Cache incomplete sequence if any
            if valid_end < input.len() {
                let remaining = input.len() - valid_end;
                STATE.utf8_buf[..remaining].copy_from_slice(&input[valid_end..]);
                STATE.utf8_len = remaining;
            }

            // Convert valid portion to string
            if let Ok(s) = String::from_utf8(input[..valid_end].to_vec()) {
                if !s.is_empty() {
                    return Some(s);
                }
            }
        }
    }
}

pub fn write_stdout(text: &str) {
    let buf = text.as_bytes();
    let mut written = 0;

    while written < buf.len() {
        let w = &buf[written..];
        let n = unsafe { libc::write(STATE.stdout, w.as_ptr() as *const _, w.len()) };

        if n >= 0 {
            written += n as usize;
            continue;
        }

        let err = unsafe { *libc::__errno_location() };
        if err != libc::EINTR {
            return;
        }
    }
}

pub fn open_stdin_if_redirected() -> Option<File> {
    unsafe {
        if libc::isatty(libc::STDIN_FILENO) == 0 {
            Some(File::from_raw_fd(libc::STDIN_FILENO))
        } else {
            None
        }
    }
}

pub unsafe fn virtual_reserve(size: usize) -> apperr::Result<*mut u8> {
    unsafe {
        let ptr = libc::mmap(
            null_mut(),
            size,
            libc::PROT_NONE,
            libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
            -1,
            0,
        );
        if ptr == libc::MAP_FAILED {
            Err(apperr::Error::new(libc::ENOMEM as u32))
        } else {
            Ok(ptr as *mut u8)
        }
    }
}

pub unsafe fn virtual_release(base: *mut u8, size: usize) {
    unsafe {
        libc::munmap(base as *mut libc::c_void, size);
    }
}

pub unsafe fn virtual_commit(base: *mut u8, size: usize) -> apperr::Result<()> {
    unsafe {
        let status = libc::mprotect(
            base as *mut libc::c_void,
            size,
            libc::PROT_READ | libc::PROT_WRITE,
        );
        if status != 0 {
            Err(apperr::Error::new(libc::ENOMEM as u32))
        } else {
            Ok(())
        }
    }
}

unsafe fn load_library(name: &CStr) -> apperr::Result<*mut c_void> {
    unsafe {
        let handle = libc::dlopen(name.as_ptr(), libc::RTLD_LAZY);
        if handle.is_null() {
            Err(apperr::Error::new(libc::ELIBACC as u32))
        } else {
            Ok(handle)
        }
    }
}

// It'd be nice to constrain T to std::marker::FnPtr, but that's unstable.
pub unsafe fn get_proc_address<T>(handle: *mut c_void, name: &CStr) -> apperr::Result<T> {
    unsafe {
        let sym = libc::dlsym(handle, name.as_ptr());
        if sym.is_null() {
            Err(apperr::Error::new(libc::ELIBACC as u32))
        } else {
            Ok(mem::transmute_copy(&sym))
        }
    }
}

pub unsafe fn load_icu() -> apperr::Result<*mut c_void> {
    unsafe { load_library(c"icu.dll") }
}

#[inline]
pub fn io_error_to_apperr(err: std::io::Error) -> apperr::Error {
    unsafe { apperr::Error::new(err.raw_os_error().unwrap_or(0) as u32) }
}

pub fn format_error(err: apperr::Error) -> String {
    let errno = err.value() & 0xFFFF;
    let mut result = format!("Error {}", errno);

    unsafe {
        let ptr = libc::strerror(errno as i32);
        if !ptr.is_null() {
            let msg = CStr::from_ptr(ptr).to_string_lossy();
            result.push_str(": ");
            result.push_str(&msg);
        }
    }

    result
}

fn errno_to_apperr(no: c_int) -> apperr::Error {
    unsafe { apperr::Error::new(no.max(1) as u32) }
}

fn check_int_return(ret: libc::c_int) -> apperr::Result<libc::c_int> {
    if ret < 0 {
        Err(errno_to_apperr(unsafe { *libc::__errno_location() }))
    } else {
        Ok(ret)
    }
}
