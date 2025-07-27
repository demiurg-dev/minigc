use inkwell::OptimizationLevel;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;

pub struct Runtime<'a> {
    module: Module<'a>,
    engine: ExecutionEngine<'a>,
}

impl<'a> Runtime<'a> {
    pub(crate) fn new(module: Module<'a>) -> Self {
        let engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        // TODO: Init
        Self { module, engine }
    }

    pub fn get_name(&self) -> &str {
        self.module.get_name().to_str().unwrap()
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
