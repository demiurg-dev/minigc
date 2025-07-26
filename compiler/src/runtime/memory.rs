use std::{alloc::{self, Layout}, ops::Not};

pub(super) struct Memory {
    layout: Layout,
    size: usize,
    heaps: [*mut u8; 2],
    active_heap: HeapIndex,
    allocated: usize,
}

impl Memory {
    pub fn new(size: usize) -> Self {
        let layout = Layout::from_size_align(size, std::mem::size_of::<usize>()).unwrap();
        let heaps = unsafe {
            let heap1 = alloc::alloc(layout);
            if heap1.is_null() {
                panic!("Could not allocate heap #1");
            }
            let heap2 = alloc::alloc(layout);
            if heap2.is_null() {
                alloc::dealloc(heap1, layout);
                panic!("Could not allocate heap #2")
            }
            [heap1, heap2]
        };
        Self {
            layout, size, heaps, active_heap: HeapIndex::First, allocated: 0
        }
    }
}

#[unsafe(no_mangle)]
extern "C" fn minigc_alloc(memory: *mut Memory, size: u64) -> *const u8 {
    unsafe {
        let memory = &mut *memory;
        let allocated = memory.allocated;
        let new_allocated = allocated + (size as usize);
        if new_allocated < memory.size {
            // TODO: Alignment
            (&mut *memory).allocated = new_allocated;
            let active_heap = memory.heaps[memory.active_heap as usize];
            active_heap.add(allocated)
        } else {
            unimplemented!("GC")
        }
    }
}

impl Drop for Memory {
    fn drop(&mut self) {
        unsafe {
            alloc::dealloc(self.heaps[0], self.layout);
            alloc::dealloc(self.heaps[1], self.layout);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
enum HeapIndex {
    First = 0,
    Second = 1,
}

impl Not for HeapIndex {
    type Output = Self;

    #[inline(always)]
    fn not(self) -> Self::Output {
        match self {
            Self::First => Self::Second,
            Self::Second => Self::First,
        }
    }
}
