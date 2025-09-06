//! Runtime support functions for System F-ω
//! 
//! Compile with: rustc --crate-type=staticlib -o runtime_support.o --emit=obj runtime_support.rs

#![no_std]
#![no_main]
#![no_builtins]

// Simple bump allocator for runtime
static mut HEAP: [u8; 1024 * 1024] = [0; 1024 * 1024]; // 1MB heap
static mut HEAP_PTR: usize = 0;

/// Runtime allocator function
#[no_mangle]
pub unsafe extern "C" fn rt_alloc(size: usize) -> *mut u8 {
    // Align to 8 bytes
    let aligned_size = (size + 7) & !7;
    
    if HEAP_PTR + aligned_size > HEAP.len() {
        // Simple error handling without std
        let _ = libc::write(2, b"Out of memory!\n".as_ptr() as *const _, 15);
        libc::exit(1);
    }
    
    let result = HEAP.as_mut_ptr().add(HEAP_PTR);
    HEAP_PTR += aligned_size;
    result
}

/// Runtime print function
#[no_mangle]
pub unsafe extern "C" fn rt_print_int(value: i64) -> u64 {
    // Convert number to string using pointer arithmetic
    let mut buffer = [0u8; 32];
    let mut num = value;
    let mut ptr = buffer.as_mut_ptr();
    let end_ptr = ptr.add(32);
    let negative = num < 0;
    
    if negative {
        num = -num;
    }
    
    // Handle zero case
    if num == 0 {
        *ptr = b'0';
        ptr = ptr.add(1);
    } else {
        // Convert digits (in reverse order)
        let mut temp_buffer = [0u8; 32];
        let mut temp_ptr = temp_buffer.as_mut_ptr();
        let mut digit_count = 0;
        
        while num > 0 && digit_count < 32 {
            *temp_ptr.add(digit_count) = b'0' + (num % 10) as u8;
            num /= 10;
            digit_count += 1;
        }
        
        // Add minus sign if negative
        if negative && ptr < end_ptr {
            *ptr = b'-';
            ptr = ptr.add(1);
        }
        
        // Copy digits in correct order
        while digit_count > 0 && ptr < end_ptr {
            digit_count -= 1;
            *ptr = *temp_ptr.add(digit_count);
            ptr = ptr.add(1);
        }
    }
    
    // Add newline if there's space
    if ptr < end_ptr {
        *ptr = b'\n';
        ptr = ptr.add(1);
    }
    
    // Calculate length
    let len = ptr.offset_from(buffer.as_ptr()) as usize;
    
    // Write to stdout
    libc::write(1, buffer.as_ptr() as *const _, len);
    
    0 // Return Unit (tagged 0)
}

// Minimal panic handler for no_std
#[panic_handler]
fn panic(_: &core::panic::PanicInfo) -> ! {
    unsafe { libc::exit(1) }
}

// Link with libc for system calls
mod libc {
    extern "C" {
        pub fn write(fd: i32, buf: *const u8, count: usize) -> isize;
        pub fn exit(status: i32) -> !;
    }
}
