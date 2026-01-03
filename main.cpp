#include <iostream>

#include "core/entities/CustomLang.hpp"

int main() {
    std::string program = R"(
        42
    )";

    CustomLang parser;
    parser.exec(program);
}
