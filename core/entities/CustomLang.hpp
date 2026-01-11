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
    llvm::StructType *current_class{};
    llvm::StructType *super_class{};
    std::map<std::string, llvm::Type *> class_parameters;
    std::map<std::string, llvm::Function *> class_methods;

    ClassInfo &operator=(const ClassInfo &rhs) {
        if (this != &rhs) {
            current_class = rhs.current_class;
            super_class = rhs.super_class;
            class_parameters = rhs.class_parameters;
            class_methods = rhs.class_methods;
        }
        throw;
    }

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
        setup_target_triple();
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

    void setup_target_triple() {
        auto target = llvm::Triple();
        //TODO HARDCODE!!
        target.setArch(llvm::Triple::ArchType::x86_64);
        target.setOS(llvm::Triple::OSType::Linux);
        module->setTargetTriple(target);
    }

    void setup_global() {
        std::map<std::string, llvm::Value *> global{
                {"Version", builder->getInt32(42)}
        };

        std::map<std::string, llvm::Value *> global_record{};
        for (const auto &entry: global) {
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
        return classMap[type_].current_class->getPointerTo();
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
        auto base_class_info = &classMap[super_class->getName().data()];

        classMap[super_class->getName().data()] = {
                new_class,
                super_class,
                base_class_info->class_parameters,
                base_class_info->class_methods,
        };
    }

    void build_class_info(llvm::StructType *new_class, const Exp &exp, const std::shared_ptr<Environment> &env) {
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
        auto class_name = std::string(new_class->getName().data());
        auto class_info = &classMap[class_name];

        auto vtable_name = class_name + "_vTable";
        auto vtableTy = llvm::StructType::create(*ctx, vtable_name);

        auto class_fields = std::vector<llvm::Type *>{
                vtableTy->getPointerTo()
        };
        for (const auto &field: class_info->class_parameters) {
            class_fields.push_back(field.second);

        }
        new_class->setBody(class_fields, false);

        build_virt_table(new_class);
    }

    void build_virt_table(llvm::StructType *new_class) {
        auto class_name = std::string(new_class->getName());
        auto vtable_name = class_name + "_vTable";

        auto vtableTy = llvm::StructType::getTypeByName(*ctx, vtable_name);
        std::vector<llvm::Constant *> vtable_methods;
        std::vector<llvm::Type *> vtable_methods_tys;

        for (auto &method_info: classMap[class_name].class_methods) {
            auto method = method_info.second;
            vtable_methods.push_back(method);
            vtable_methods_tys.push_back(method->getType());
        }

        vtableTy->setBody(vtable_methods_tys);
        auto vtable_value = llvm::ConstantStruct::get(vtableTy, vtable_methods);
        create_glob_val(vtable_name, vtable_value);
    }

    size_t get_method_idx(llvm::StructType *class_ent, const std::string &method_name) {
        auto methods = &classMap[class_ent->getName().data()].class_methods;
        auto iter = methods->find(method_name);
        return std::distance(methods->begin(), iter);
    }

    bool is_tagged_list(const Exp &exp, const std::string &tag_name) {
        return exp.type == ExpType::LIST and exp.list[0].type == ExpType::SYMBOL and exp.list[0].string == tag_name;
    }

    bool is_var(const Exp &exp) { return is_tagged_list(exp, "var"); }

    bool is_def(const Exp &exp) { return is_tagged_list(exp, "var"); }

    bool is_new(const Exp &exp) { return is_tagged_list(exp, "new"); }

    bool is_property(const Exp &exp) { return is_tagged_list(exp, "prop"); }

    bool is_super_class(const Exp &exp) { return is_tagged_list(exp, "super"); }

    llvm::Value *
    create_class_instance(const Exp &exp, const std::shared_ptr<Environment> &env, const std::string &name) {
        auto class_name = exp.list[1].string;
        auto local_cls = get_class_by_name(class_name);

        if (local_cls == nullptr) {
            throw;
        }
#ifdef STACK_ALLOC
        auto instance = name.empty() ? builder->CreateAlloca(local_cls)
                                     : builder->CreateAlloca(local_cls, nullptr,
                                                             name);
#elifdef HEAP_ALLOC
        auto instance = malloc_instance(cls, name);
#endif
        auto constructor = module->getFunction(class_name + "_constructor");
        std::vector<llvm::Value *> arguments{instance};
        for (int i = 2; i < exp.list.size(); ++i) {
            arguments.push_back(gen(exp.list[i], env));
        }
        builder->CreateCall(constructor, arguments);
        return instance;
    }

    size_t get_field_index(llvm::StructType *cls_ent, const std::string &name) {
        auto fields = &classMap[cls_ent->getName().data()].class_parameters;
        auto iter = fields->find(name);
        return std::distance(fields->begin(), iter) + RESERVED_FIELDS_COUNT; //+1 for vtable
    }

#if defined(CLASS) && defined(HEAP_ALLOC)
    llvm::Value* malloc_instance(llvm::StructType* cls, const std::string& name) {
        auto type_size = builder->getInt64(get_type_size(cls));
        auto malloc_ptr = builder->CreateCall(module->getFunction("GC_malloc"), type_size, name);

        auto instance = builder->CreatePointerCast(malloc_ptr, cls->getPointerTo());
        auto class_name = std::string(cls.getName().data());
        auto vtable_name = class_name + "_vTable";
        auto vtable_address = builder->CreateStructGEP(cls, instance, VTABLE_INDEX);
        auto vtable = module->GetNamedGlobal(vtable_name);
        builder->CreateStore(vtable, vtable_address);

        return instance;
    }

    size_t get_type_size(llvm::Type* type) {
        return module->getDataLayout().getTypeAllocSize(type);
    }
#endif

#endif

#ifdef FUNC

    llvm::Type *get_type_from_string(const std::string &type) {
        if (type == "number") {
            return builder->getInt32Ty();
        }

        if (type == "string") {
            return builder->getInt8Ty()->getPointerTo();
        }
        return classMap[type].current_class->getPointerTo();
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
                        return compile_function(exp, const_cast<std::string &>(exp.list[1].string), env);
                    } else if (opcode == "method") {
                        auto method_name = exp.list[2].string;

                        llvm::StructType *class_ent;
                        llvm::Value *vtable;
                        llvm::StructType *vtable_ty;

                        if (is_super_class(exp.list[1])) {
                            auto class_name = exp.list[1].list[1].string;
                            class_ent = (llvm::StructType *) &classMap[class_name].super_class;
                            auto parent_name = std::string{cls->getName().data()};
                            vtable = module->getNamedGlobal(parent_name + "_vTable");
                            vtable_ty = llvm::StructType::getTypeByName(*ctx, parent_name + "_vTable");

                        } else {
                            auto instance = gen(exp.list[1], env);
                            class_ent = (llvm::StructType *) (instance->getType()->getContainedType(0));

                            auto vtable_address = builder->CreateStructGEP(class_ent, instance, VTABLE_INDEX);

                            vtable = builder->CreateLoad(class_ent->getElementType(VTABLE_INDEX), vtable_address, "vt");
                            vtable_ty = (llvm::StructType *) (vtable->getType()->getContainedType(0));
                        }
                        auto method_index = get_method_idx(class_ent, method_name);
                        auto method_ty = (llvm::StructType *) vtable_ty->getElementType(method_index);
                        auto method_address = builder->CreateStructGEP(vtable_ty, vtable, method_index);
                        return builder->CreateLoad(method_ty, method_address);
                    }
#endif
                    if (opcode == "var") {
#ifdef CLASS
                        if (cls != nullptr) {
                            return builder->getInt32(0);
                        }
#endif
                        auto variable_name = exp.list[1].string;
                        if (is_new(exp.list[2])) {
                            auto instance = create_class_instance(exp.list[2], env, variable_name);
                            return env->define_var(variable_name, instance);
                        }

                        auto init = gen(exp.list[2], env);

                        auto type = extract_variable_type(variable_name);
                        auto binding = allocate_variable(variable_name, type, env);
                        return builder->CreateStore(init, binding);
                    } else if (opcode == "prop") {
                        auto instance = gen(exp.list[1], env);
                        auto field_name = exp.list[2].string;

                        auto ptr_field = std::string("p") + field_name;
                        auto class_entity = (llvm::StructType *) instance->getType()->getContainedType(0);
                        auto field_index = get_field_index(class_entity, field_name);
                        auto address = builder->CreateStructGEP(class_entity, instance, field_index, ptr_field);
                        return builder->CreateLoad(class_entity->getElementType(field_index), address, field_name);
                    } else if (opcode == "new") {
                        create_class_instance(exp, env, "");
                    } else if (opcode == "printf") { //react on printf function opcode
                        auto printf_fun = module->getFunction("printf");
                        std::vector<llvm::Value *> args{};
                        for (const auto &i: exp.list) {
                            args.push_back(gen(i, env));
                        }
                        return builder->CreateCall(printf_fun, args);
                    } else if (opcode == "set") {
                        auto value = gen(exp.list[2], env);

                        if (is_property(exp.list[1])) {
                            auto instance = gen(exp.list[1].list[1], env);
                            auto field_name = exp.list[1].list[2].string;

                            auto ptr_field = std::string("p") + field_name;
                            auto class_entity = (llvm::StructType *) instance->getType()->getContainedType(0);
                            auto field_index = get_field_index(class_entity, field_name);
                            auto address = builder->CreateStructGEP(class_entity, instance, field_index, ptr_field);
                            builder->CreateStore(value, address);
                            return value;

                        } else {
                            auto variable_name = exp.list[1].string;
                            auto variable_binding = env->get_val(variable_name);

                            builder->CreateStore(value, variable_binding);
                            return value;
                        }

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
                        auto func = (llvm::Function *) (callable_obj);
                        std::vector<llvm::Value *> arguments{};
                        unsigned int arg_idx = 0;
                        for (int i = 1; i < exp.list.size(); ++arg_idx) {
                            auto arg_val = gen(exp.list[i], env);
                            auto func_ty = func->getArg(arg_idx)->getType();
                            auto casted_val = builder->CreateBitCast(arg_val, func_ty);

                            arguments.push_back(casted_val);
                        }

                        return builder->CreateCall(func, arguments);
                    }
#endif
                } else {
                    auto loaded_method = (llvm::LoadInst *) gen(exp.list[0], env);
                    auto func_ty = (llvm::FunctionType *) (loaded_method
                            ->getPointerOperand()
                            ->getType()->getContainedType(0)
                            ->getContainedType(0));

                    std::vector<llvm::Value *> arguments{};
                    for (int i = 1; i < exp.list.size(); ++i) {
                        auto arg_val = gen(exp.list[1], env);
                        auto param_ty = func_ty->getParamType(i - 1);
                        if (arg_val->getType() != param_ty) {
                            auto casted_val = builder->CreateBitCast(arg_val, param_ty);
                            arguments.push_back(casted_val);
                        } else {
                            arguments.push_back(arg_val);
                        }
                    }
                    return builder->CreateCall(func_ty, loaded_method, arguments);
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

        module->getOrInsertFunction("GC_malloc", llvm::FunctionType::get(
                byteptr, builder->getInt64Ty(), false)
        );
    }
};

#endif
