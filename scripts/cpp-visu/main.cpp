#include <iostream>
#include <format>
#include <string>
#include <vector>
#include <set>
#include <fstream>
#include <future>
#include <atomic>
#include <mutex>
#include <filesystem>

#include <clang-c/Index.h>
#include <clang-c/CXCompilationDatabase.h>

struct ClassInfo
{
    std::string name;
    std::vector<std::string> base_classes;
};

std::string project_root = "";
std::string build_dir = "";

CXChildVisitResult baseClassVisitor(CXCursor cursor, CXCursor parent, CXClientData client_data)
{
    CXCursorKind kind = clang_getCursorKind(cursor);

    if (kind == CXCursor_CXXBaseSpecifier)
    {
        CXString name = clang_getCursorDisplayName(cursor);

        auto* base_classes = static_cast<std::vector<std::string>*>(client_data);
        base_classes->emplace_back(clang_getCString(name));

        clang_disposeString(name);
    }

    return CXChildVisit_Continue;
}

std::string indent(int level)
{
    return std::string(level * 2, ' ');
}

CXChildVisitResult dumpVisitor(CXCursor cursor, CXCursor parent, CXClientData client_data)
{
    CXSourceLocation location = clang_getCursorLocation(cursor);
    if (clang_Location_isFromMainFile(location) == 0)
    {
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

CXChildVisitResult visitor(CXCursor cursor, CXCursor parent, CXClientData client_data)
{
    CXSourceLocation location = clang_getCursorLocation(cursor);
    CXFile file;
    clang_getSpellingLocation(location, &file, nullptr, nullptr, nullptr);
    if (file == nullptr)
    {
        return CXChildVisit_Continue;
    }

    CXString file_name_cxstr = clang_getFileName(file);
    std::string file_name = clang_getCString(file_name_cxstr);
    clang_disposeString(file_name_cxstr);

    if (file_name.find(project_root) == std::string::npos)
    {
        return CXChildVisit_Continue;
    }

    if (file_name.find(build_dir) != std::string::npos)
    {
        return CXChildVisit_Continue;
    }

    CXCursorKind kind = clang_getCursorKind(cursor);

    if (kind == CXCursor_ClassDecl || kind == CXCursor_StructDecl)
    {
        if (!clang_isCursorDefinition(cursor))
        {
            return CXChildVisit_Continue;
        }

        CXString name = clang_getCursorDisplayName(cursor);
        std::string class_name = clang_getCString(name);
        clang_disposeString(name);

        if (class_name.find("unnamed") != std::string::npos || class_name.empty())
        {
            return CXChildVisit_Continue;
        }

        // if (visited_classes.contains(class_name))
        // {
        //     return CXChildVisit_Continue;
        // }
        // visited_classes.insert(class_name);
        // output_file << "class \"" << class_name << "\"" << std::endl;

        std::vector<std::string> base_classes;
        clang_visitChildren(cursor, baseClassVisitor, &base_classes);

        auto* classes = static_cast<std::vector<ClassInfo>*>(client_data);
        classes->push_back({ class_name, base_classes });

        // if (!base_classes.empty())
        // {
        //     for (const auto& base : base_classes)
        //     {
        //         // output_file << " \"" << base << "\" <|-- \"" << class_name << "\"" << std::endl;
        //     }
        //     std::cout << std::endl;
        // }
    }

    return CXChildVisit_Recurse;
}

std::vector<ClassInfo> parseSingleFile(CXIndex index, CXCompileCommand compile_command, const std::string& source_file)
{
    std::vector<const char*> args;
    std::vector<std::string> allocated_strings;
    unsigned int num_args = clang_CompileCommand_getNumArgs(compile_command);
    for (unsigned int i = 1; i < num_args; ++i)
    {
        CXString arg = clang_CompileCommand_getArg(compile_command, i);
        std::string arg_str = clang_getCString(arg);

        if (arg_str == "--")
        {
            clang_disposeString(arg);
            continue;
        }

        if (arg_str == source_file || arg_str.ends_with(".cpp") || arg_str.ends_with(".h") || arg_str.ends_with(".hpp") ||
            arg_str.ends_with(".c") || arg_str.ends_with(".cc"))
        {
            clang_disposeString(arg);
            continue;
        }

        if (arg_str == "/c" || arg_str == "-c" || arg_str == "-TP" || arg_str == "-TP" || arg_str.starts_with("/Fd") ||
            arg_str.starts_with("-Fd"))
        {
            clang_disposeString(arg);
            continue;
        }

        if (arg_str.starts_with("-external:I"))
        {
            std::string include_path = "-I" + arg_str.substr(11);
            allocated_strings.push_back(include_path);
            clang_disposeString(arg);
            continue;
        }

        if (arg_str.starts_with("-external:"))
        {
            clang_disposeString(arg);
            continue;
        }

        if (arg_str == "/showIncludes")
        {
            clang_disposeString(arg);
            continue;
        }

        allocated_strings.push_back(arg_str);
        clang_disposeString(arg);
    }

    for (const auto& str : allocated_strings)
    {
        args.push_back(str.c_str());
    }

    args.push_back("-x");
    args.push_back("c++");
    args.push_back("-fms-compatibility");
    args.push_back("-fms-extensions");
    args.push_back("-w");
    args.push_back("-Wno-everything");

    CXTranslationUnit tu = clang_parseTranslationUnit(
        index,
        source_file.c_str(),
        args.data(), static_cast<int>(args.size()),
        nullptr, 0,
        CXTranslationUnit_None
    );

    std::vector<ClassInfo> classes;

    if (tu != nullptr)
    {
        CXCursor root_cursor = clang_getTranslationUnitCursor(tu);
        clang_visitChildren(root_cursor, visitor, &classes);
        clang_disposeTranslationUnit(tu);
    }
    else
    {
        // std::cerr << "failed to parse translation unit for file: " << source_file << std::endl;
    }

    return classes;
}

void usage()
{
    std::cerr << "Usage: cpp_visu [compile_command.json path] [project_root] [build_dir] [out_file]" << std::endl;
}

int main(int argc, char* argv[])
{
    std::cout << "C++ Code Visualizer" << std::endl;

    if (argc != 5)
    {
        usage();
        return 1;
    }

    project_root = argv[2];
    build_dir = argv[3];

    std::string out_file_path = argv[4];
    std::ofstream output_file(out_file_path);
    if (!output_file.is_open())
    {
        std::cerr << "failed to open output file: " << out_file_path << std::endl;
        return 1;
    }

    std::filesystem::path json_path(argv[1]);
    if (!std::filesystem::exists(json_path))
    {
        std::cerr << "compilation database file does not exist: " << argv[1] << std::endl;
        return 1;
    }
    if (std::filesystem::is_directory(json_path))
    {
        json_path /= "compile_commands.json";
    }

    std::string json_dir = json_path.parent_path().string();

    std::cout << "loading compilation database from: " << json_dir << std::endl;

    CXCompilationDatabase_Error error;
    CXCompilationDatabase comp_db = clang_CompilationDatabase_fromDirectory(json_dir.c_str(), &error);
    if (error != CXCompilationDatabase_NoError)
    {
        std::cerr << "failed to load compilation database: " << error << std::endl;
        return 1;
    }

    CXCompileCommands compile_commands = clang_CompilationDatabase_getAllCompileCommands(comp_db);
    unsigned int num_commands = clang_CompileCommands_getSize(compile_commands);
    if (num_commands == 0)
    {
        std::cerr << "no compile commands found in the database" << std::endl;
        clang_CompileCommands_dispose(compile_commands);
        clang_CompilationDatabase_dispose(comp_db);
        return 1;
    }

    std::cerr << "found " << num_commands << " compile commands in the database" << std::endl;

    CXIndex index = clang_createIndex(0, 0);

    std::atomic<unsigned int> files_processed = 0;
    std::mutex cerr_mutex;
    std::vector<std::future<std::vector<ClassInfo>>> futures;

    for (unsigned int f = 0; f < num_commands; ++f)
    {
        CXCompileCommand compile_command = clang_CompileCommands_getCommand(compile_commands, f);

        CXString source_file_cxstr = clang_CompileCommand_getFilename(compile_command);
        std::string source_file = clang_getCString(source_file_cxstr);
        clang_disposeString(source_file_cxstr);

        std::string absolute_source_file = std::filesystem::absolute(source_file).string();

        futures.push_back(std::async(std::launch::async, [compile_command, absolute_source_file, &files_processed, &cerr_mutex, num_commands]
        {
            CXIndex index = clang_createIndex(0, 0);
            std::vector<ClassInfo> classes = parseSingleFile(index, compile_command, absolute_source_file);
            clang_disposeIndex(index);

            unsigned int processed = ++files_processed;
            {
                std::lock_guard lock(cerr_mutex);
                std::cerr << std::format("[{}/{}] processed file: {}", processed, num_commands, absolute_source_file) << std::endl;
            }

            return classes;
        }));
    }

    std::vector<ClassInfo> classes;
    for (auto& future : futures)
    {
        std::vector<ClassInfo> file_classes = future.get();
        classes.insert(classes.end(), file_classes.begin(), file_classes.end());
    }

    output_file << "@startuml" << std::endl;
    std::set<std::string> unique_classes;
    for (const auto& class_info : classes)
    {
        if (unique_classes.contains(class_info.name))
        {
            continue;
        }
        unique_classes.insert(class_info.name);

        output_file << "class \"" << class_info.name << "\"" << std::endl;

        for (const auto& base : class_info.base_classes)
        {
            output_file << " \"" << base << "\" <|-- \"" << class_info.name << "\"" << std::endl;
        }
    }
    output_file << "@enduml" << std::endl;

    clang_CompileCommands_dispose(compile_commands);
    clang_CompilationDatabase_dispose(comp_db);
    clang_disposeIndex(index);

    output_file.close();

    return 0;
}
