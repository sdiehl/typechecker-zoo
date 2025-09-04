#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

// Simple bump allocator for runtime
static uint8_t heap[1024 * 1024]; // 1MB heap
static size_t heap_ptr = 0;

// Runtime allocator function
void* rt_alloc(size_t size) {
    // Align to 8 bytes
    size = (size + 7) & ~7;
    
    if (heap_ptr + size > sizeof(heap)) {
        fprintf(stderr, "Out of memory!\n");
        exit(1);
    }
    
    void* result = &heap[heap_ptr];
    heap_ptr += size;
    return result;
}

// Runtime print function
uint64_t rt_print_int(int64_t value) {
    printf("%lld\n", value);
    fflush(stdout);
    return 0; // Return Unit (tagged 0)
}

// Our compiled main function (renamed to avoid conflict)
extern uint64_t main_compiled();

// Extract integer value from tagged pointer
int64_t extract_int(uint64_t tagged) {
    return (int64_t)(tagged >> 3);
}

int main() {
    main_compiled();
    return 0;
}
