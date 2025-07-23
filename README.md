# minigc

**This is work in progress**

Implementation of a very small language with automated memory management (GC), using LLVM and Rust.

The idea of a project is to test how simple GC technique could be implemented using LLVM.

For usage/examples see [tests in compiler library](compiler/src/lib.rs).
The tests are currently using proc-macro over Rust syntax as the language frontend.