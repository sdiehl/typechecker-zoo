//! Runtime value representation for the System F-ω compiler
//!
//! All values are represented as tagged 64-bit pointers:
//! - Low 3 bits are used for tags
//! - Upper 61 bits contain the pointer/immediate value

#![allow(unsafe_code)]

use std::fmt;

/// Tag values for runtime type discrimination
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tag {
    Closure = 0b000,
    Integer = 0b001,
    Boolean = 0b010,
    TypeAbstraction = 0b011,
}

/// A tagged runtime value
#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct Value(pub u64);

/// Memory layout for a closure
#[repr(C)]
pub struct Closure {
    pub code_ptr: *const u8,
    pub env_size: usize,
    // Environment values follow
}

impl Value {
    const TAG_MASK: u64 = 0b111;
    const PTR_MASK: u64 = !0b111;

    /// Create a new tagged value
    pub fn new(ptr: *mut u8, tag: Tag) -> Self {
        let addr = ptr as u64;
        debug_assert_eq!(addr & Self::TAG_MASK, 0, "Pointer must be 8-byte aligned");
        Value(addr | (tag as u64))
    }

    /// Create an integer value
    pub fn int(n: i64) -> Self {
        // Store integer in upper bits, tag in lower bits
        Value(((n as u64) << 3) | Tag::Integer as u64)
    }

    /// Create a boolean value
    pub fn bool(b: bool) -> Self {
        Value(((b as u64) << 3) | Tag::Boolean as u64)
    }

    /// Get the tag of this value
    pub fn tag(self) -> Tag {
        match self.0 & Self::TAG_MASK {
            0b000 => Tag::Closure,
            0b001 => Tag::Integer,
            0b010 => Tag::Boolean,
            0b011 => Tag::TypeAbstraction,
            _ => unreachable!(),
        }
    }

    /// Get the pointer from this value (assumes it's a pointer type)
    pub fn as_ptr(self) -> *mut u8 {
        (self.0 & Self::PTR_MASK) as *mut u8
    }

    /// Get integer value (assumes tag is Integer)
    pub fn as_int(self) -> i64 {
        debug_assert_eq!(self.tag(), Tag::Integer);
        (self.0 >> 3) as i64
    }

    /// Get boolean value (assumes tag is Boolean)
    pub fn as_bool(self) -> bool {
        debug_assert_eq!(self.tag(), Tag::Boolean);
        (self.0 >> 3) != 0
    }

    /// Get as closure (assumes tag is Closure)
    pub fn as_closure(self) -> &'static Closure {
        debug_assert_eq!(self.tag(), Tag::Closure);
        unsafe { &*(self.as_ptr() as *const Closure) }
    }

    /// Check if this is a closure
    pub fn is_closure(self) -> bool {
        self.tag() == Tag::Closure
    }

    /// Check if this is a type abstraction
    pub fn is_type_abstraction(self) -> bool {
        self.tag() == Tag::TypeAbstraction
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.tag() {
            Tag::Integer => write!(f, "Int({})", self.as_int()),
            Tag::Boolean => write!(f, "Bool({})", self.as_bool()),
            Tag::Closure => write!(f, "Closure({:p})", self.as_ptr()),
            Tag::TypeAbstraction => write!(f, "TypeAbs({:p})", self.as_ptr()),
        }
    }
}
