[![CI Status](https://github.com/demiurg-dev/minigc/actions/workflows/build-check-test.yml/badge.svg)](https://github.com/demiurg-dev/minigc/actions/workflows/build-check-test.yml)

# minigc

**This is work in progress**

Implementation of a very small language with automated memory management (GC), using LLVM and Rust.

The idea of a project is to test how simple GC technique could be implemented using LLVM.

For usage/examples see [tests in the compiler library](compiler/tests).
The tests are currently using proc-macro over Rust syntax as the language frontend.