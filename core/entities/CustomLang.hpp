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

#include "../data/Constants.hpp"
#include "CustomLangException.hpp"

#define GEN_BINARY_OP(Op, varName)         \
  do {                                     \
    auto op1 = gen(exp.list[1], env);      \
    auto op2 = gen(exp.list[2], env);      \
    return builder->Op(op1, op2, varName); \
  } while (false)

#ifdef CLASS

struct ClassInfo {
    llvm::StructType current_class;
    llvm::StructType super_class;
    std::map<std::string, llvm::Type *> class_parameters;
    std::map<std::string, llvm::Function *> class_methods;

    ClassInfo() = default;

    ~ClassInfo() = default;
};

#endif

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
    std::unique_ptr<llvm::IRBuilder<>> variable_builder;

    std::unique_ptr<syntax::Parser> parser;
    std::shared_ptr<Environment> global_env;

#ifdef CLASS
    llvm::StructType *cls = nullptr;
    std::map<std::string, ClassInfo> classMap;
#endif
    llvm::Function *fun;

    void module_init() {
        ctx = std::make_unique<llvm::LLVMContext>();
        module = std::make_unique<llvm::Module>("SmallParser", *ctx);
        variable_builder = std::make_unique<llvm::IRBuilder<>>(*ctx); ///for allocation variables
        builder = std::make_unique<llvm::IRBuilder<>>(*ctx);
        parser = std::make_unique<syntax::Parser>();
    }

    void compile(const Exp &ast) {
        fun = create_function("main",
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

    std::string extract_variable_name(const Exp &expression) {
        return expression.type == ExpType::LIST ? expression.list[0].string : expression.string;
    }

    llvm::Type *extract_variable_type(const Exp &expression) {
        return expression.type == ExpType::LIST ? convert_str_2_type(expression.list[1].string) : builder->getInt32Ty();
    }

    llvm::Value *allocate_variable(const std::string &name, llvm::Type *type, std::shared_ptr<Environment> env) {
        variable_builder->SetInsertPoint(&fun->getEntryBlock());
        auto variable_alloc = variable_builder->CreateAlloca(type, 0, name.c_str());
        env->define_var(name, variable_alloc);
        return variable_alloc;
    }

    /**
    * Returns LLVM type from string representation.
    */
    llvm::Type *convert_str_2_type(const std::string &type_) {
        // number -> i32
        if (type_ == "number") {
            return builder->getInt32Ty();
        }

        // string -> i8* (aka char*)
        if (type_ == "string") {
            return builder->getInt8Ty()->getPointerTo();
        }

#ifdef CLASS
        // Classes:
        return classMap[type_].current_class.getPointerTo();
#endif
    }

    llvm::BasicBlock *create_basic_block(const std::string &fun_name, llvm::Function *func = nullptr) {
        return llvm::BasicBlock::Create(*ctx, fun_name, func);
    }

#if defined(CLASS) && defined(FUNC)

    llvm::StructType *get_class_by_name(const std::string &class_name) {
        return llvm::StructType::getTypeByName(*ctx, class_name);
    }

    void inherit_from_super_class(llvm::StructType *new_class, llvm::StructType *super_class) {
        //
    }

    void build_class_info(llvm::StructType *new_class, const Exp &exp, std::shared_ptr<Environment> env) {
        auto class_name = exp.list[1].string;
        auto class_info = &classMap[class_name];

        auto class_body = exp.list[3];
        for (int i = 1; i < class_body.list.size(); ++i) {
            auto inner_exp = class_body.list[i];

            if (is_var(inner_exp)) {
                auto var_name = inner_exp.list[1];
                auto field_name = extract_variable_name(var_name);
                auto field_type = extract_variable_type(var_name);
                class_info->class_parameters[field_name] = field_type;
            } else if (is_def(inner_exp)) {
                auto method_name = inner_exp.list[1].string;
                auto alt_method_name = class_name + "_" + method_name;
                class_info->class_methods[method_name] = create_function_proto(
                        alt_method_name,
                        extract_function_type(inner_exp),
                        env
                );
            }
        }
        build_class_body(cls);
    }

    void build_class_body(llvm::StructType *new_class) {
        auto class_name = new_class->getName().data();
        auto class_info = &classMap[class_name];

        auto class_fields = std::vector<llvm::Type *>{};
        for (const auto &field: class_info->class_parameters) {
            class_fields.push_back(field.second);

        }
        new_class->setBody(class_fields, false);
    }

    bool is_tagged_list(const Exp &exp, const std::string &tag_name) {
        return exp.type == ExpType::LIST and exp.list[0].type == ExpType::SYMBOL and exp.list[0].string == tag;
    }

    bool is_var(const Exp &exp) { return is_tagged_list(exp, "var"); }

    bool is_def(const Exp &exp) { return is_tagged_list(exp, "var"); }

#endif

#ifdef FUNC

    llvm::Type *get_type_from_string(const std::string &type) {
        if (type == "number") {
            return builder->getInt32Ty();
        }

        if (type == "string") {
            return builder->getInt8Ty()->getPointerTo();
        }
        return classMap[type].current_class.getPointerTo();
    }

    bool has_return_type(const Exp &exp) {
        return exp.list[3].type == ExpType::SYMBOL and exp.list[3].string == "->";
    }

    llvm::FunctionType *extract_function_type(const Exp &exp) {
        auto func_parameters = exp.list[2];
        auto func_ret_type = has_return_type(exp)
                             ? get_type_from_string(exp.list[4].string)
                             : builder->getInt32Ty();

        std::vector<llvm::Type *> params{};
        for (auto &param: func_parameters.list) {
            auto param_name = extract_variable_name(param);
            auto param_type = extract_variable_type(param);
            params.push_back(
                    param_name == "self" ? (llvm::Type *) cls->getPointerTo() : param_type
            );
        }

        return llvm::FunctionType::get(func_ret_type, params, false);
    }

    llvm::Value *compile_function(const Exp &exp, std::string &func_name, std::shared_ptr<Environment> env) {
        auto func_params = exp.list[2];
        auto func_body = has_return_type(exp) ? exp.list[5] : exp.list[3];

        auto original_func_name = func_name;

        if (cls != nullptr) {
            func_name = std::string(cls->getName().data()) + "_" + func_name;
        }

        auto previous_fun = fun;
        auto previous_block = builder->GetInsertBlock();
        auto new_fun = create_function(func_name, extract_function_type(exp), env);
        fun = new_fun;

        auto idx = 0;
        auto func_env = std::make_shared<Environment>(
                std::map<std::string, llvm::Value *>{}, env
        );
        for (auto &func_arg: fun->args()) {
            auto param = func_params.list[idx++];
            auto arg_name = extract_variable_name(param);
            func_arg.setName(arg_name);

            auto func_binding = allocate_variable(arg_name, func_arg.getType(), env);
            builder->CreateStore(&func_arg, func_binding);
        }
        builder->CreateRet(gen(func_body, env));

        builder->SetInsertPoint(previous_block);
        fun = previous_fun;
        return new_fun;
    }

#endif

    llvm::Value *gen(const Exp &exp, const std::shared_ptr<Environment> &env) {
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
                    if (auto local_var = llvm::dyn_cast<llvm::AllocaInst>(variable_value)) {
                        return builder->CreateLoad(local_var->getAllocatedType(), local_var, variable_name.c_str());
                    }

                    //global
                    if (auto global_var = llvm::dyn_cast<llvm::GlobalVariable>(variable_value)) {
                        return builder->CreateLoad(global_var->getInitializer()->getType(),
                                                   global_var,
                                                   variable_name.c_str());
                    }
#ifdef FUNC
                        //function call
                    else {
                        return variable_value;
                    }
#endif
                }
            case ExpType::LIST: {
                auto list_tag = exp.list[0];
                if (list_tag.type == ExpType::SYMBOL) {
                    auto opcode = list_tag.string;

                    if (opcode == "+") {
                        GEN_BINARY_OP(CreateAdd, "tmpadd");
                    } else if (opcode == "-") {
                        GEN_BINARY_OP(CreateSub, "tmpsub");
                    } else if (opcode == "*") {
                        GEN_BINARY_OP(CreateMul, "tmpmul");
                    } else if (opcode == "/") {
                        GEN_BINARY_OP(CreateFDiv, "tmpdiv");
                    }
                        //comparisons operators:
                    else if (opcode == ">") {
                        GEN_BINARY_OP(CreateICmpUGT, "tmpcmp");
                    }

                        // ULT - unsigned, less than
                    else if (opcode == "<") {
                        GEN_BINARY_OP(CreateICmpULT, "tmpcmp");
                    }

                        // EQ - equal
                    else if (opcode == "==") {
                        GEN_BINARY_OP(CreateICmpEQ, "tmpcmp");
                    }

                        // NE - not equal
                    else if (opcode == "!=") {
                        GEN_BINARY_OP(CreateICmpNE, "tmpcmp");
                    }

                        // UGE - greater or equal
                    else if (opcode == ">=") {
                        GEN_BINARY_OP(CreateICmpUGE, "tmpcmp");
                    }

                        // ULE - less or equal
                    else if (opcode == "<=") {
                        GEN_BINARY_OP(CreateICmpULE, "tmpcmp");
                    }

#ifdef IF
                        //branching operators:
                    else if (opcode == "if") {
                        auto condition = gen(exp.list[1], env);

                        auto then_block = create_basic_block("then", fun);
                        auto else_block = create_basic_block("else");
                        auto end_block = create_basic_block("ifend");

                        builder->CreateCondBr(condition, then_block, else_block);

                        //then branch:
                        builder->SetInsertPoint(then_block);
                        auto then_br_res = gen(exp.list[2], env);
                        builder->CreateBr(end_block);

                        then_block = builder->GetInsertBlock();

                        //else branch:
                        fun->getBasicBlockList().push_back(else_block);
                        builder->SetInsertPoint(else_block);
                        auto else_br_res = gen(exp.list[3], env);
                        builder->CreateBr(end_block);

                        else_block = builder->GetInsertBlock();

                        //if end block:
                        fun->getBasicBlockList().push_back(end_block);
                        builder->SetInsertPoint(end_block);

                        auto phi = builder->CreatePHI(then_br_res->getType(), 2, "tmpif");

                        phi->addIncoming(then_br_res, then_block);
                        phi->addIncoming(else_br_res, else_block);
                        return phi;
                    }
#endif
#ifdef WHILE
                        /**
                        * (while <cond> <body>)
                        */
                    else if (opcode == "while") {
                        auto condition_blc = create_basic_block("while", fun);
                        builder->CreateBr(condition_blc);

                        auto body_blc = create_basic_block("body", fun);
                        auto loop_end_blc = create_basic_block("loopend");

                        builder->SetInsertPoint(condition_blc);
                        auto condition = gen(exp.list[1], env);

                        builder->CreateCondBr(condition, body_blc, loop_end_blc);
                        fun->getBasicBlockList().push_back(body_blc);
                        builder->SetInsertPoint(body_blc);
                        gen(exp.list[2], env);
                        builder->CreateBr(condition_blc);

                        fun->getBasicBlockList().push_back(loop_end_blc);
                        builder->SetInsertPoint(loop_end_blc);

                        return builder->getInt32(0);
                    }
#endif

#ifdef FUNC
                        // --------------------------------------------
                        // Function declaration: (def <name> <params> <body>)
                        //
                    else if (opcode == "def") {
                        return compile_function(exp, exp.list[1].string, env);
                    }
#endif
                    if (opcode == "var") {
#ifdef CLASS
                        if (cls != nullptr) {
                            return builder->getInt32(0);
                        }
#endif
                        auto variable_name = exp.list[1].string;
                        auto init = gen(exp.list[2], env);

                        auto type = extract_variable_type(variable_name);
                        auto binding = allocate_variable(variable_name, type, env);
                    } else if (opcode == "printf") { //react on printf function opcode
                        auto printf_fun = module->getFunction("printf");
                        std::vector<llvm::Value *> args{};
                        for (const auto &i: exp.list) {
                            args.push_back(gen(i, env));
                        }
                        return builder->CreateCall(printf_fun, args);
                    } else if (opcode == "set") {
                        auto value = gen(exp.list[2], env);
                        auto variable_name = exp.list[1].string;
                        auto variable_binding = env->get_val(variable_name);

                        builder->CreateStore(value, variable_binding);
                        return value;
                    } else if (opcode == "begin") {
                        auto inner_block = std::make_shared<Environment>(
                                std::map<std::string, llvm::Value *>{}, env
                        );
                        llvm::Value *block_res;
                        for (const auto &i: exp.list) {
                            block_res = gen(i, inner_block);
                        }
                        return block_res;
                    }
#ifdef CLASS
                    else if (opcode == "class") {
                        auto class_name = exp.list[1].string;

                        auto super_class =
                                exp.list[2].string == "null" ? nullptr : get_class_by_name(exp.list[2].string);
                        cls = llvm::StructType::create(*ctx, class_name);
                        if (super_class != nullptr) {
                            inherit_from_super_class(cls, super_class);
                        } else {
                            classMap[class_name] = {
                                    cls,
                                    super_class,
                                    {}, //class parameters
                                    {} //class methods
                            };
                        }
                        build_class_info(cls, exp, env);
                        gen(exp.list[3], env);
                        cls = nullptr; //reset cls pointer
                        return builder->getInt32(0);
                    }
#endif
#ifdef FUNC
                    else {
                        auto callable_obj = gen(exp.list[0], env);
                        std::vector<llvm::Value *> arguments{};
                        for (int i = 1; i < exp.list.size(); ++i) {
                            arguments.push_back(gen(exp.list[i], env));
                        }

                        auto local_fun_call = static_cast<llvm::Function *>(callable_obj);
                        return builder->CreateCall(local_fun_call, arguments);
                    }
#endif
                }
            }
            default:
                throw CustomLangException(__LINE__, "Cannot generate", __FILE_NAME__);
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
