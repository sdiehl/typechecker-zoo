//! Runtime support functions for System F-ω
//! 
//! This file is automatically compiled by build.rs during the build process.

#![no_std]
#![no_main]

// Simple bump allocator for runtime
static mut HEAP: [u8; 1024 * 1024] = [0; 1024 * 1024]; // 1MB heap
static mut HEAP_PTR: usize = 0;

// Out of memory error string
static OOM: &[u8] = b"Out of memory!\n";

/// Runtime allocator function
#[no_mangle]
pub unsafe extern "C" fn rt_alloc(size: usize) -> *mut u8 {
    // Align to 8 bytes
    let aligned_size = (size + 7) & !7;
    
    if HEAP_PTR + aligned_size > HEAP.len() {
        // Just write to stderr directly
        libc::write(2, OOM.as_ptr(), OOM.len());
        libc::exit(1);
    }
    
    let result = HEAP.as_mut_ptr().add(HEAP_PTR);
    HEAP_PTR += aligned_size;
    result
}

/// Runtime print function - just use printf from libc
#[no_mangle]
pub unsafe extern "C" fn rt_print_int(value: i64) -> u64 {
    libc::printf(b"%lld\n\0".as_ptr() as *const i8, value);
    0 // Return Unit (tagged 0)
}

// Minimal panic handler for no_std
#[panic_handler]
fn panic(_: &core::panic::PanicInfo) -> ! {
    unsafe { libc::exit(1) }
}

// Link with libc
mod libc {
    extern "C" {
        pub fn printf(format: *const i8, ...) -> i32;
        pub fn write(fd: i32, buf: *const u8, count: usize) -> isize;
        pub fn exit(status: i32) -> !;
    }
}
