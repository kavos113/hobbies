#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <cstdio>

void stderr_output_function()
{
    std::cerr << "output to cerr" << std::endl;
}

void c_stderr_output_function()
{
    fprintf(stderr, "output to stderr\n");
}

int main()
{
    std::streambuf *original_cerr = std::cerr.rdbuf();

    std::stringstream errors;
    std::cerr.rdbuf(errors.rdbuf());

    stderr_output_function();
    c_stderr_output_function();

    std::cerr.rdbuf(original_cerr);

    std::cout << "captured: " << errors.str() << std::endl;

    return 0;
}