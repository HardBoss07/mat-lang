/// Manages Boehm GC allocator symbols (@GC_malloc) and runtime bindings
pub struct RuntimeBindings;

impl RuntimeBindings {
    pub fn gc_malloc_fn_name() -> &'static str {
        "GC_malloc"
    }
}
