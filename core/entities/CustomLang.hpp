#ifndef SMALLPARSER_CUSTOMLANG_HPP
#define SMALLPARSER_CUSTOMLANG_HPP

#include <string>
#include <memory>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include "Parser.h"

/**
 * Small implementation of parser with llvm
 */
class CustomLang {
public:
    ~CustomLang() = default;

    CustomLang() {
        fun = nullptr;
        module_init();
        get_external_functions();
    }

    llvm::Value *exec(const std::string &program) {

        auto ast = parser->parse(program);

        compile(ast);

        saveModuleToFile("./out.ll");

        auto str_arg = builder->CreateGlobalString("hello");
        auto print_fn = module->getFunction("printf");
        std::vector<llvm::Value *> arguments = {str_arg};
        return builder->CreateCall(print_fn, arguments);
    }

    void saveModuleToFile(const std::string &file_name) {
        std::error_code code;
        llvm::raw_fd_ostream output(file_name, code);
        module->print(output, nullptr);
    }

private:
    std::unique_ptr<llvm::LLVMContext> ctx;
    std::unique_ptr<llvm::Module> module;
    std::unique_ptr<llvm::IRBuilder<>> builder;
    std::unique_ptr<syntax::Parser> parser;
    llvm::Function *fun;

    void module_init() {
        ctx = std::make_unique<llvm::LLVMContext>();
        module = std::make_unique<llvm::Module>("SmallParser", *ctx);
        builder = std::make_unique<llvm::IRBuilder<>>(*ctx);
        parser = std::make_unique<syntax::Parser>();
    }

    void compile(const Exp ast) {
        auto fun = create_function("main", llvm::FunctionType::get(builder->getInt32Ty(), false));
        auto result = gen();
        auto int_result = builder->CreateIntCast(result, builder->getInt32Ty(), true);
        builder->CreateRet(int_result);
    }

    llvm::Function *create_function(const std::string &fun_name, llvm::FunctionType *fun_type) {
        auto fn = module->getFunction(fun_name);
        if (fn == nullptr) {
            create_function_proto(fun_name, fun_type);
        }
        create_fun_block(fn);
        return fn;
    }

    llvm::Function *create_function_proto(const std::string &fun_name, llvm::FunctionType *fn_type) {
        auto fn_to_return = llvm::Function::Create(fn_type, llvm::Function::ExternalLinkage, fun_name, *module);
        verifyFunction(*fn_to_return);
        return fn_to_return;
    }

    void create_fun_block(llvm::Function *func) {
        auto entry_point = create_basic_block("entry", func);
        builder->SetInsertPoint(entry_point);
    }

    llvm::BasicBlock *create_basic_block(const std::string &fun_name, llvm::Function *func = nullptr) {
        return llvm::BasicBlock::Create(*ctx, fun_name, func);
    }

    llvm::Value *gen() {
        builder->getInt32(42);
    }

    void get_external_functions() {
        auto byteptr = builder->getInt8Ty()->getPointerTo();
        module->getOrInsertFunction(
                "printf",
                llvm::FunctionType::get(
                        builder->getInt32Ty(),
                        byteptr,
                        true
                )
        );
    }
};

#endif
