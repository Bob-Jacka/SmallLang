#ifndef SMALLPARSER_CUSTOMLANG_HPP
#define SMALLPARSER_CUSTOMLANG_HPP

#include <string>
#include <memory>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include "Parser.h"
#include "Environment.hpp"

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
        setup_global();
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

    void setup_global() {
        std::map<std::string, llvm::Value *> global{
                {"Version", builder->getInt32(42)}
        };

        std::map<std::string, llvm::Value *> global_record{};
        for (auto entry: global) {
            global_record[entry.first] = create_glob_val(entry.first, static_cast<llvm::Constant *>(entry.second));
        }

        global_env = std::make_shared<Environment>(global_record, nullptr);
    }

    void saveModuleToFile(const std::string &file_name) {
        std::error_code code;
        llvm::raw_fd_ostream output(file_name, code);
        module->print(output, nullptr);
    }

    llvm::GlobalVariable *create_glob_val(const std::string &name, llvm::Constant *init) {
        module->getOrInsertGlobal(name, init->getType());
        auto var = module->getNamedGlobal(name);
        var->setAlignment(llvm::MaybeAlign());
        var->setConstant(false);
        var->setInitializer(init);
        return var;
    }

private:
    std::unique_ptr<llvm::LLVMContext> ctx;
    std::unique_ptr<llvm::Module> module;
    std::unique_ptr<llvm::IRBuilder<>> builder;
    std::unique_ptr<syntax::Parser> parser;
    std::shared_ptr<Environment> global_env;
    llvm::Function *fun;

    void module_init() {
        ctx = std::make_unique<llvm::LLVMContext>();
        module = std::make_unique<llvm::Module>("SmallParser", *ctx);
        builder = std::make_unique<llvm::IRBuilder<>>(*ctx);
        parser = std::make_unique<syntax::Parser>();
    }

    void compile(const Exp &ast) {
        auto fun = create_function("main",
                                   llvm::FunctionType::get(
                                           builder->getInt32Ty(),
                                           false
                                   ), global_env
        );
        gen(ast, global_env);
        builder->CreateRet(builder->getInt32(42)); //mean of life
    }

    llvm::Function *
    create_function(const std::string &fun_name, llvm::FunctionType *fun_type, std::shared_ptr<Environment> env) {
        auto fn = module->getFunction(fun_name);
        if (fn == nullptr) {
            create_function_proto(fun_name, fun_type, env);
        }
        create_fun_block(fn);
        return fn;
    }

    llvm::Function *
    create_function_proto(const std::string &fun_name, llvm::FunctionType *fn_type, std::shared_ptr<Environment> env) {
        auto fn_to_return = llvm::Function::Create(fn_type, llvm::Function::ExternalLinkage, fun_name, *module);
        verifyFunction(*fn_to_return);
        env->define_var(fun_name, fn_to_return);
        return fn_to_return;
    }

    void create_fun_block(llvm::Function *func) {
        auto entry_point = create_basic_block("entry", func);
        builder->SetInsertPoint(entry_point);
    }

    llvm::BasicBlock *create_basic_block(const std::string &fun_name, llvm::Function *func = nullptr) {
        return llvm::BasicBlock::Create(*ctx, fun_name, func);
    }

    llvm::Value *gen(const Exp &exp, std::shared_ptr<Environment> env) {
        switch (exp.type) {
            case ExpType::NUMBER:
                return builder->getInt32(exp.number);
            case ExpType::STRING: {
                auto reg = std::regex("\\\\n");
                auto reg_str = std::regex_replace(exp.string, reg, "\n");
                return builder->CreateGlobalString(reg_str);
            }
            case ExpType::SYMBOL:
                if (exp.string == "true" or exp.string == "false") {
                    return builder->getInt1(exp.string == "true");
                } else {
                    auto variable_name = exp.string;
                    auto variable_value = env->get_val(variable_name);
                    //local

                    //global
                    if (auto global_var = llvm::dyn_cast<llvm::GlobalVariable>(variable_value)) {
                        return builder->CreateLoad(global_var->getInitializer()->getType(),
                                                   global_var,
                                                   variable_name.c_str());
                    }
                }
            case ExpType::LIST: {
                auto list_tag = exp.list[0];
                if (list_tag.type == ExpType::SYMBOL) {
                    auto opcode = list_tag.string;
                    if (opcode == "var") {
                        auto variable_name = exp.list[1].string;
                        auto init = gen(exp.list[2], env);
                        create_glob_val(variable_name, static_cast<llvm::Constant *>(init));
                    } else if (opcode == "printf") { //react on printf function opcode
                        auto printf_fun = module->getFunction("printf");
                        std::vector<llvm::Value *> args{};
                        for (int i = 0; i < exp.list.size(); ++i) {
                            args.push_back(gen(exp.list[i], env));
                        }
                        return builder->CreateCall(printf_fun, args);
                    } else if (opcode == "begin") {
                        llvm::Value *block_res;
                        for (int i = 0; i < exp.list.size(); ++i) {
                            block_res = gen(exp.list[i], env);
                        }
                        return block_res;
                    }
                }
            }
            default:
                throw;
        }
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
