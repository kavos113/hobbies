cmake_minimum_required(VERSION 3.16)

project(MyQtQuickApp LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Qt6 6.2 REQUIRED COMPONENTS Quick)

qt_standard_project_setup()

qt_add_executable(appMyQtQuickApp
    main.cpp
)

qt_add_qml_module(appMyQtQuickApp
    URI "Main"
    VERSION 1.0
    QML_FILES Main.qml
)

target_link_libraries(appMyQtQuickApp
    PRIVATE Qt6::Quick
)