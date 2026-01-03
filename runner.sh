clang++ -o parser `llvm-config --cxxflags --ldflags --system-libs --libs core` main.cpp -fexceptions

cd cmake-build-debug

./parser

lli ./out.ll