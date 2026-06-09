#include <iostream>
#include <iomanip>
#include <format>
#include <fstream>
#include <string>
#include <vector>

#include <clang-c/Index.h>
#include <clang-c/CXCompilationDatabase.h>

CXChildVisitResult baseClassVisitor(CXCursor cursor, CXCursor parent, CXClientData client_data) {
    CXCursorKind kind = clang_getCursorKind(cursor);

    if (kind == CXCursor_CXXBaseSpecifier) {
        CXString name = clang_getCursorDisplayName(cursor);

        auto* base_classes = static_cast<std::vector<std::string>*>(client_data);
        base_classes->emplace_back(clang_getCString(name));

        clang_disposeString(name);
    }

    return CXChildVisit_Continue;
}

std::string indent(int level) {
    return std::string(level * 2, ' ');
}

CXChildVisitResult dumpVisitor(CXCursor cursor, CXCursor parent, CXClientData client_data) {
    CXSourceLocation location = clang_getCursorLocation(cursor);
    if (clang_Location_isFromMainFile(location) == 0) {
        return CXChildVisit_Continue;
    }

    int* depth = static_cast<int*>(client_data);

    CXCursorKind kind = clang_getCursorKind(cursor);
    CXString kind_name = clang_getCursorKindSpelling(kind);
    CXString name = clang_getCursorDisplayName(cursor);

    std::cout << indent(*depth) << clang_getCString(kind_name) << ": " << clang_getCString(name) << std::endl;

    clang_disposeString(kind_name);
    clang_disposeString(name);

    int next_depth = *depth + 1;
    clang_visitChildren(cursor, dumpVisitor, &next_depth);

    return CXChildVisit_Continue;
}

CXChildVisitResult visitor(CXCursor cursor, CXCursor parent, CXClientData client_data) {
    CXSourceLocation location = clang_getCursorLocation(cursor);
    if (clang_Location_isFromMainFile(location) == 0) {
        return CXChildVisit_Continue;
    }

    CXCursorKind kind = clang_getCursorKind(cursor);

    if (kind == CXCursor_ClassDecl || kind == CXCursor_StructDecl) {
        if (!clang_isCursorDefinition(cursor)) {
            return CXChildVisit_Continue;
        }

        CXString name = clang_getCursorDisplayName(cursor);
        std::cout << "class: " << clang_getCString(name) << std::endl;
        clang_disposeString(name);

        std::vector<std::string> base_classes;
        clang_visitChildren(cursor, baseClassVisitor, &base_classes);

        if (!base_classes.empty()) {
            std::cout << "  base classes: ";
            for (const auto& base : base_classes) {
                std::cout << base << " ";
            }
            std::cout << std::endl;
        }
    }

    return CXChildVisit_Recurse;
}

void usage() {
    std::cerr << "Usage: cpp_visu [source_file] [compile_command.json path]" << std::endl;
}

int main(int argc, char* argv[]) {
    std::cout << "C++ Code Visualizer" << std::endl;

    if (argc != 3) {
        usage();
        return 1;
    }

    std::cout << "Parsing source file: " << argv[1] << std::endl;

    std::string source_file = argv[1];

    std::ifstream file(source_file);
    if (!file.is_open())
    {
        std::cerr << "failed to open source file: " << source_file << std::endl;
        return 1;
    }

    std::string source_code((std::istreambuf_iterator(file)), std::istreambuf_iterator<char>());

    CXCompilationDatabase_Error error;
    CXCompilationDatabase comp_db = clang_CompilationDatabase_fromDirectory(argv[2], &error);
    if (error != CXCompilationDatabase_NoError) {
        std::cerr << "failed to load compilation database: " << error << std::endl;
        return 1;
    }

    CXCompileCommands compile_commands = clang_CompilationDatabase_getCompileCommands(comp_db, source_file.c_str());
    unsigned int num_commands = clang_CompileCommands_getSize(compile_commands);
    if (num_commands == 0)
    {
        std::cerr << "no compile commands found for source file: " << source_file << std::endl;
        clang_CompileCommands_dispose(compile_commands);
        clang_CompilationDatabase_dispose(comp_db);
        return 1;
    }

    std::vector<const char*> args;
    std::vector<CXString> allocated_strings;

    CXCompileCommand compile_command = clang_CompileCommands_getCommand(compile_commands, 0);
    unsigned int num_args = clang_CompileCommand_getNumArgs(compile_command);
    for (unsigned int i = 1; i < num_args; ++i)
    {
        CXString arg = clang_CompileCommand_getArg(compile_command, i);
        std::string arg_str = clang_getCString(arg);
        allocated_strings.push_back(arg);

        if (arg_str == "--")
        {
            continue;
        }

        if (arg_str == source_file || arg_str.ends_with(".cpp") || arg_str.ends_with(".h") || arg_str.ends_with(".hpp") || arg_str.ends_with(".c") || arg_str.ends_with(".cc"))
        {
            continue;
        }

        if (arg_str == "/c" || arg_str == "-c" || arg_str == "-TP" || arg_str == "-TP" || arg_str.starts_with("/Fd") || arg_str.starts_with("-Fd"))
        {
            continue;
        }

        if (arg_str.starts_with("-external:I"))
        {
            std::string include_path = "-I" + arg_str.substr(11);

            char *include_path_cstr = new char[include_path.size() + 1];
            std::strcpy(include_path_cstr, include_path.c_str());

            args.push_back(include_path_cstr);
            continue;
        }

        if (arg_str.starts_with("-external:"))
        {
            continue;
        }

        args.push_back(clang_getCString(arg));
    }

    std::cout << "compile command arguments:" << std::endl;
    for (const auto& arg : args)
    {
        std::cout << "  " << arg << std::endl;
    }

    args.push_back("-x");
    args.push_back("c++");
    args.push_back("-fms-compatibility");
    args.push_back("-fms-extensions");

    CXIndex index = clang_createIndex(0, 0);

    CXTranslationUnit tu = clang_parseTranslationUnit(
        index,
        source_file.c_str(),
        args.data(), static_cast<int>(args.size()),
        nullptr, 0,
        CXTranslationUnit_None
    );

    if (tu == nullptr) {
        std::cerr << "failed to parse translation unit" << std::endl;
        for (const auto& str : allocated_strings)
        {
            clang_disposeString(str);
        }
        clang_CompileCommands_dispose(compile_commands);
        clang_CompilationDatabase_dispose(comp_db);
        clang_disposeIndex(index);
        return 1;
    }

    CXCursor root_cursor = clang_getTranslationUnitCursor(tu);
    clang_visitChildren(root_cursor, visitor, nullptr);

    clang_CompileCommands_dispose(compile_commands);
    clang_CompilationDatabase_dispose(comp_db);
    for (const auto& str : allocated_strings)
        {
        clang_disposeString(str);
    }
    clang_disposeTranslationUnit(tu);
    clang_disposeIndex(index);

    return 0;
}