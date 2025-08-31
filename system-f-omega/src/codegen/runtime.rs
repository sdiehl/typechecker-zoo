//! Runtime system for memory allocation and function calls
//!
//! For simplicity, we use manual memory allocation with a simple bump allocator
//! instead of Boehm GC (which would require C bindings)

#![allow(unsafe_code)]

use std::alloc::{alloc, Layout};
use std::sync::atomic::{AtomicUsize, Ordering};

use super::value::{Closure, Tag, Value};

/// Simple thread-local bump allocator
pub struct Allocator {
    heap: *mut u8,
    heap_size: usize,
    offset: AtomicUsize,
}

impl Allocator {
    const HEAP_SIZE: usize = 64 * 1024 * 1024; // 64MB heap

    pub fn new() -> Self {
        let layout = Layout::from_size_align(Self::HEAP_SIZE, 8).unwrap();
        let heap = unsafe { alloc(layout) };

        Allocator {
            heap,
            heap_size: Self::HEAP_SIZE,
            offset: AtomicUsize::new(0),
        }
    }

    /// Allocate n bytes with 8-byte alignment
    pub fn alloc(&self, size: usize) -> *mut u8 {
        // Round up to 8-byte alignment
        let size = (size + 7) & !7;

        let offset = self.offset.fetch_add(size, Ordering::SeqCst);
        if offset + size > self.heap_size {
            panic!("Out of memory");
        }

        unsafe { self.heap.add(offset) }
    }
}

thread_local! {
    static ALLOCATOR: Allocator = Allocator::new();
}

/// Allocate memory for runtime values
pub fn gc_alloc(size: usize) -> *mut u8 {
    ALLOCATOR.with(|a| a.alloc(size))
}

/// Create a closure value
pub fn make_closure(code_ptr: *const u8, env_values: &[Value]) -> Value {
    let env_size = env_values.len();
    let closure_size = std::mem::size_of::<Closure>() + env_size * std::mem::size_of::<Value>();

    let ptr = gc_alloc(closure_size);
    unsafe {
        let closure = ptr as *mut Closure;
        (*closure).code_ptr = code_ptr;
        (*closure).env_size = env_size;

        // Copy environment values
        let env_ptr = ptr.add(std::mem::size_of::<Closure>()) as *mut Value;
        for (i, value) in env_values.iter().enumerate() {
            *env_ptr.add(i) = *value;
        }
    }

    Value::new(ptr, Tag::Closure)
}

/// Project a value from a closure's environment
pub fn project_env(closure: Value, index: usize) -> Value {
    let closure = closure.as_closure();
    assert!(index < closure.env_size, "Environment index out of bounds");

    unsafe {
        let closure_ptr = closure as *const Closure as *const u8;
        let env_ptr = closure_ptr.add(std::mem::size_of::<Closure>()) as *const Value;
        *env_ptr.add(index)
    }
}

/// Apply a closure to an argument
/// This is called from generated code
pub extern "C" fn apply(closure: Value, _arg: Value) -> Value {
    assert!(closure.is_closure() || closure.is_type_abstraction());

    if closure.is_type_abstraction() {
        // Type abstractions just return their body when applied
        closure
    } else {
        // For actual function application, we need to call the generated code
        // This will be implemented when we have the code generator
        todo!("Function application not yet implemented")
    }
}

/// Runtime type error handler
#[allow(improper_ctypes_definitions)]
pub extern "C" fn type_error(expected: &str, got: Value) -> ! {
    panic!("Type error: expected {}, got {:?}", expected, got);
}
