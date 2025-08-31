// Runtime support functions for System F-ω
// This would need to be compiled and linked with the generated object files

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

// Value representation matches Rust side
typedef uint64_t Value;

// Tag constants
#define TAG_CLOSURE     0
#define TAG_INTEGER     1
#define TAG_BOOLEAN     2
#define TAG_TYPE_ABS    3

#define TAG_MASK        7
#define PTR_MASK        (~7ULL)

// Runtime functions that need to be linked

extern Value make_closure(void* code_ptr, ...);
extern Value project_env(Value closure, int64_t index);
extern Value apply(Value closure, Value arg);
extern Value make_int(int64_t n);
extern Value make_bool(int64_t b);

// Example implementation (would need proper implementation)
Value make_int(int64_t n) {
    return (n << 3) | TAG_INTEGER;
}

Value make_bool(int64_t b) {
    return (b << 3) | TAG_BOOLEAN;
}

// Print a value for debugging
void print_value(Value v) {
    switch (v & TAG_MASK) {
        case TAG_INTEGER:
            printf("%lld", v >> 3);
            break;
        case TAG_BOOLEAN:
            printf(v >> 3 ? "true" : "false");
            break;
        case TAG_CLOSURE:
            printf("<closure>");
            break;
        case TAG_TYPE_ABS:
            printf("<type-abs>");
            break;
    }
}