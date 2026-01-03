#ifndef SMALLPARSER_ENVIRONMENT_HPP
#define SMALLPARSER_ENVIRONMENT_HPP

#include <map>
#include <memory>
#include "llvm/IR/Value.h"

class Environment {
public:
    Environment(std::map<std::string, llvm::Value *> record, std::shared_ptr<Environment> parent) : storage(record),
                                                                                                    parent(parent) {

    }

    ~Environment() = default;

    llvm::Value *define_var(const std::string &var_name, llvm::Value *value) {
        storage[var_name] = value;
        return value;
    }

    llvm::Value *get_val(const std::string &var_name) {
        return resolve(var_name)->storage[var_name];
    }

    std::shared_ptr<Environment> resolve(const std::string &name) {
        if (storage.count(name) != 0) {
            return std::make_shared<Environment>(*this);
        }

        if (parent == nullptr) {
            throw;
        }
        return parent->resolve(name);
    }

private:
    std::map<std::string, llvm::Value *> storage;
    std::shared_ptr<Environment> parent;
};

#endif
