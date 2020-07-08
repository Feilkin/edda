//! Errors related to compilation and execution

#[derive(Debug)]
pub struct OutOfMemory {
    pub tried_to_allocate: usize,
}