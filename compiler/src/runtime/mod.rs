use inkwell::OptimizationLevel;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{CodeModel, RelocMode, Target, TargetMachine};

use crate::codegen::{MINIGC_ALLOC_INTERNAL, MINIGC_MEMORY, MINIGC_MEMSIZE_INTERNAL};
use crate::runtime::memory::{Memory, managed_alloc, managed_size};

mod memory;

const DEFAULT_MEM_SIZE: usize = 10 * 1024 * 1024; // 10 MB

pub struct Runtime<'a> {
    module: Module<'a>,
    engine: ExecutionEngine<'a>,
    _memory: Box<Memory>,
}

impl<'a> Runtime<'a> {
    pub(crate) fn new(module: Module<'a>, opt_level: OptimizationLevel) -> Self {
        let memory = Box::new(Memory::new(DEFAULT_MEM_SIZE));

        let engine = module.create_jit_execution_engine(opt_level).unwrap();
        engine.add_global_mapping(&module.get_function(MINIGC_ALLOC_INTERNAL).unwrap(), managed_alloc as usize);
        engine.add_global_mapping(&module.get_function(MINIGC_MEMSIZE_INTERNAL).unwrap(), managed_size as usize);
        engine.add_global_mapping(&module.get_global(MINIGC_MEMORY).unwrap(), (&*memory as *const Memory) as usize);

        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).unwrap();
        let features = TargetMachine::get_host_cpu_features();
        let features = features.to_str().unwrap();
        let level = OptimizationLevel::Default;
        let reloc_mode = RelocMode::Static;
        let code_model = CodeModel::JITDefault;
        let cpu = TargetMachine::get_host_cpu_name();
        let cpu = cpu.to_str().unwrap();

        let machine = target
            .create_target_machine(&triple, cpu, features, level, reloc_mode, code_model)
            .unwrap();
        module
            .run_passes("default<O1>", &machine, PassBuilderOptions::create())
            .unwrap();
        let mod_str = module.print_to_string();
        eprintln!("----------------------- After optimization -----------------------");
        eprintln!("{}", mod_str.to_str().unwrap());

        // TODO: Init
        Self { module, engine, _memory: memory }
    }

    pub fn get_name(&self) -> &str {
        self.module.get_name().to_str().unwrap()
    }

    pub fn get_fn0<R>(&self, name: &str) -> extern "C" fn() -> R {
        unsafe { std::mem::transmute(self.engine.get_function_address(name).unwrap()) }
    }

    pub fn get_fn1<T, R>(&self, name: &str) -> extern "C" fn(T) -> R {
        unsafe { std::mem::transmute(self.engine.get_function_address(name).unwrap()) }
    }

    pub fn get_fn2<T1, T2, R>(&self, name: &str) -> extern "C" fn(T1, T2) -> R {
        unsafe { std::mem::transmute(self.engine.get_function_address(name).unwrap()) }
    }

    pub fn get_fn3<T1, T2, T3, R>(&self, name: &str) -> extern "C" fn(T1, T2, T3) -> R {
        unsafe { std::mem::transmute(self.engine.get_function_address(name).unwrap()) }
    }

    pub fn get_fn4<T1, T2, T3, T4, R>(&self, name: &str) -> extern "C" fn(T1, T2, T3, T4) -> R {
        unsafe { std::mem::transmute(self.engine.get_function_address(name).unwrap()) }
    }
}

impl<'a> std::fmt::Debug for Runtime<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Runtime")
            .field("module", &self.module.get_name().to_str().unwrap())
            .finish()
    }
}
