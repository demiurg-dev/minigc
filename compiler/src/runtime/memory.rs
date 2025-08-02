use std::alloc::{self, Layout};

const ALIGN: u64 = std::mem::align_of::<u64>() as u64;

#[repr(align(8))]
pub(super) struct Memory {
    layout: Layout,
    heap: *mut u8,
    allocated: usize,
}

impl Memory {
    pub fn new(size: usize) -> Self {
        let layout = Layout::from_size_align(size, ALIGN as usize).unwrap();
        let heap = unsafe { alloc::alloc(layout) };
        let allocated = 0;
        Self { layout, heap, allocated }
    }
}

#[inline(always)]
fn align_size(size: u64) -> u64 {
    size.div_ceil(ALIGN) * ALIGN
}

pub(super) extern "C" fn managed_alloc(memory: *mut Memory, size: u64) -> *const u8 {
    let size = align_size(size);
    unsafe {
        let memory = &mut *memory;
        let allocated = memory.allocated;
        let new_allocated = allocated + (size as usize);
        if new_allocated < memory.layout.size() {
            memory.allocated = new_allocated;
            memory.heap.add(allocated)
        } else {
            unimplemented!("GC")
        }
    }
}

pub(super) extern "C" fn managed_size(memory: *const Memory) -> u64 {
    unsafe { (&*memory).allocated as u64 }
}

impl Drop for Memory {
    fn drop(&mut self) {
        unsafe {
            alloc::dealloc(self.heap, self.layout);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::runtime::memory::{ALIGN, align_size};

    #[test]
    fn check_aligned_size() {
        for size in 1..1024_u64 {
            let aligned = align_size(size);
            assert!(aligned % ALIGN == 0);
            assert!(aligned.checked_sub(size).unwrap() < ALIGN);
        }
    }
}
