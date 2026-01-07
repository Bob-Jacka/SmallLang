#ifndef SMALLPARSER_CUSTOMLANGEXCEPTION_HPP
#define SMALLPARSER_CUSTOMLANGEXCEPTION_HPP

#include <exception>
#include <iostream>

class CustomLangException : public std::exception {
public:
    explicit CustomLangException(int line, const std::string &msg, const char *filename);
};

inline CustomLangException::CustomLangException(const int line, const std::string &msg, const char *filename) {
    std::cout << "Error: " << msg << " at line " << line << " at file " << filename << std::endl;
}

#endif
