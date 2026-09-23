use inkwell::context::Context;

fn main() {
    // 1. Initialize the global LLVM context and a new module
    let context = Context::create();
    let module = context.create_module("test_module");
    let builder = context.create_builder();

    // 2. Define function signature: fn get_number() -> i32
    let i32_type = context.i32_type();
    let fn_type = i32_type.fn_type(&[], false);
    let function = module.add_function("get_number", fn_type, None);

    // 3. Create basic block and append a return statement (return 42)
    let basic_block = context.append_basic_block(function, "entry");
    builder.position_at_end(basic_block);
    
    let return_val = i32_type.const_int(42, false);
    builder.build_return(Some(&return_val)).unwrap();

    // 4. Verify and print generated LLVM IR
    println!("=== Successfully generated LLVM IR ===");
    println!("{}", module.print_to_string().to_string());
}