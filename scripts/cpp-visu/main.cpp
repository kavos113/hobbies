#include <iostream>
#include <iomanip>
#include <format>
#include <fstream>
#include <string>
#include <vector>
#include <clang-c/Index.h>

CXChildVisitResult baseClassVisitor(CXCursor cursor, CXCursor parent, CXClientData client_data) {
    CXSourceLocation location = clang_getCursorLocation(cursor);
    if (clang_Location_isFromMainFile(location) == 0) {
        return CXChildVisit_Continue;
    }

    CXCursorKind kind = clang_getCursorKind(cursor);

    if (kind == CXCursor_CXXBaseSpecifier) {
        CXString name = clang_getCursorDisplayName(cursor);

        std::cout << "  found base class: " << clang_getCString(name) << std::endl;

        auto* base_classes = static_cast<std::vector<std::string>*>(client_data);
        base_classes->emplace_back(clang_getCString(name));

        clang_disposeString(name);
    }

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
    std::cerr << "Usage: cpp_visu [source_file]" << std::endl;
}

int main(int argc, char* argv[]) {
    std::cout << "C++ Code Visualizer" << std::endl;

    CXIndex index = clang_createIndex(0, 1);

    if (argc != 2) {
        usage();
        clang_disposeIndex(index);
        return 1;
    }

    std::cout << "Parsing source file: " << argv[1] << std::endl;

    const char* source_file = argv[1];

    std::ifstream file(source_file);
    if (!file.is_open())
    {
        std::cerr << "failed to open source file: " << source_file << std::endl;
        clang_disposeIndex(index);
        return 1;
    }

    std::string source_code((std::istreambuf_iterator(file)), std::istreambuf_iterator<char>());

    CXUnsavedFile unsaved_file;
    unsaved_file.Filename = source_file;
    unsaved_file.Contents = source_code.c_str();
    unsaved_file.Length = std::string(source_code).length();

    const char* args[] = {
        "-x", "c++",
        "-std=c++17",
        "--driver-mode=g++",
        "-fms-compatibility",
        "-fms-extensions",
    };

    CXTranslationUnit tu = clang_parseTranslationUnit(
        index,
        source_file,
        args, std::size(args),
        &unsaved_file, 1,
        CXTranslationUnit_None
    );

    if (tu == nullptr) {
        std::cerr << "failed to parse translation unit" << std::endl;
        clang_disposeIndex(index);
        return 1;
    }

    CXCursor root_cursor = clang_getTranslationUnitCursor(tu);

    clang_visitChildren(root_cursor, visitor, nullptr);

    clang_disposeTranslationUnit(tu);
    clang_disposeIndex(index);

    return 0;
}