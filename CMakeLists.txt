cmake_minimum_required(VERSION 3.20)

project(aquila LANGUAGES Fortran C CXX)

set(CMAKE_CXX_STANDARD 20)       # request C++20
set(CMAKE_CXX_STANDARD_REQUIRED ON)  # fail if compiler does not support it
set(CMAKE_CXX_EXTENSIONS OFF)    # turn off compiler-specific extensions like gnu++20

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(GNUInstallDirs)
set(CMAKE_INSTALL_LIBDIR ${CMAKE_INSTALL_LIBDIR}/aquila)
set(CMAKE_INSTALL_INCLUDEDIR ${CMAKE_INSTALL_INCLUDEDIR}/aquila)
set(CMAKE_INSTALL_DATADIR ${CMAKE_INSTALL_DATADIR}/aquila)
set(CMAKE_INSTALL_RPATH "$ORIGIN/../${CMAKE_INSTALL_LIBDIR}:$ORIGIN")

if (CMAKE_C_COMPILER_ID STREQUAL "GNU")
    set (opt_debug "-Og")
else ()
    set (opt_debug "-O0")
endif ()
set (CMAKE_C_FLAGS_DEBUG "-g ${opt_debug}")
set (CMAKE_CXX_FLAGS_DEBUG "-g ${opt_debug}")
set (CMAKE_Fortran_FLAGS_DEBUG "-g ${opt_debug}")
set (CMAKE_C_FLAGS_RELEASE "-g -O2")
set (CMAKE_CXX_FLAGS_RELEASE "-g -O2")
set (CMAKE_Fortran_FLAGS_RELEASE "-g -O2")

if (CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64|AMD64")
    if(NOT DEFINED AQUILA_MARCH OR AQUILA_MARCH STREQUAL "")
        set(AQUILA_MARCH "-march=x86-64-v2")
    endif()
endif()

add_compile_options(${AQUILA_MARCH})

add_compile_options($<$<COMPILE_LANG_AND_ID:Fortran,IntelLLVM>:-heap-arrays>)
add_compile_options($<$<NOT:$<CONFIG:DEBUG>>:-DNDEBUG>)

find_package(PkgConfig REQUIRED)

set(BUILD_SHARED_LIBS ON CACHE BOOL "shared CFITSIO" FORCE)
set(USE_CURL OFF CACHE BOOL "Disable CURL in CFITSIO" FORCE)
set(USE_PTHREADS ON CACHE BOOL "Enable pthreads in CFITSIO" FORCE)
set(TESTS OFF CACHE BOOL "Disable CFITSIO tests" FORCE)
set(UTILS OFF CACHE BOOL "Disable CFITSIO utils" FORCE)
add_subdirectory(deps/cfitsio)

option (AQUILA_SYSTEM_LIBPNG "Use system libPNG" Off)
add_library(png_target INTERFACE)
if (AQUILA_SYSTEM_LIBPNG)
    find_package(PNG REQUIRED)
    target_link_libraries(png_target INTERFACE PNG::PNG)
else()
    set(PNG_TESTS OFF CACHE BOOL "Disable libpng tests" FORCE)
    set(PNG_SHARED ON CACHE BOOL "Build static library only" FORCE)
    set(PNG_STATIC OFF CACHE BOOL "Enable static library" FORCE)
    set(PNG_TOOLS OFF CACHE BOOL "Disable building executables" FORCE)
    add_subdirectory(deps/libpng)
    target_link_libraries(png_target INTERFACE png_shared)
endif()

option(AQUILA_TUI "aquila tui" Off)
if (AQUILA_TUI)
add_subdirectory(deps/FTXUI)
endif()

option(AQUILA_TESTS "build tests and examples" On)
if (AQUILA_TESTS)
enable_testing()
endif()

set(CXX_WARN "-Wall -Wextra -pedantic -Wpedantic -Werror=return-type -Werror=uninitialized -Werror=sign-compare -Werror=maybe-uninitialized -Wno-unused-parameter")
set(FORT_WARN "-Wall -Wextra -pedantic -Wno-unused-dummy-argument")
add_compile_options("$<$<AND:$<CONFIG:Debug>,$<COMPILE_LANG_AND_ID:Fortran,GNU>>:SHELL:-fcheck=all ${FORT_WARN}>")
add_compile_options("$<$<AND:$<CONFIG:Debug>,$<COMPILE_LANG_AND_ID:CXX,GNU>>:SHELL:${CXX_WARN}>")
add_compile_options("$<$<AND:$<CONFIG:Debug>,$<COMPILE_LANG_AND_ID:Fortran,IntelLLVM>>:SHELL:-check all -warn all>")
add_compile_options("$<$<AND:$<CONFIG:Debug>,$<COMPILE_LANG_AND_ID:CXX,IntelLLVM>>:SHELL:-warn all>")
#add_compile_options($<$<AND:$<CONFIG:Debug>,$<NOT:$<COMPILE_LANGUAGE:Fortran>>>:-fsanitize=address>)
#add_link_options($<$<AND:$<CONFIG:Debug>,$<NOT:$<COMPILE_LANGUAGE:Fortran>>>:-fsanitize=address>)
add_compile_options($<$<CONFIG:RELEASE>:-fno-math-errno>)
add_compile_options($<$<CONFIG:RELEASE>:-funsafe-math-optimizations>)

option(AQUILA_PARALLEL "use OpenMP in build" Off)

if (AQUILA_PARALLEL)
find_package(OpenMP)
endif()

pkg_check_modules(READLINE REQUIRED readline)

file(GLOB_RECURSE AQUILA_SOURCES CONFIGURE_DEPENDS
    src/core/*.f90
    src/core/*.F90
    src/core/*.c
    src/framehandling/*.f90
    src/framehandling/*.F90
    src/framehandling/*.c
    src/globals/*.f90
    src/globals/*.F90
    src/globals/*.c
    src/io/*.f90
    src/io/*.F90
    src/io/*.c
    src/utils/*.f90
    src/utils/*.F90
    src/utils/*.c
    src/c_binding/*.hpp
    src/c_binding/*.f90
    src/c_binding/*.F90
)

add_library(aquila SHARED ${AQUILA_SOURCES})
target_include_directories(aquila PUBLIC src/c_binding)
target_compile_options(aquila PRIVATE $<$<CONFIG:RELEASE>:-O3>)
target_link_libraries(aquila PUBLIC png_target cfitsio)

if (AQUILA_PARALLEL)
    target_link_libraries(aquila PRIVATE OpenMP::OpenMP_Fortran OpenMP::OpenMP_C)
    target_compile_definitions(aquila PRIVATE AQUILA_PARALLEL)
endif ()

install(TARGETS aquila LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})
install(FILES src/c_binding/aquila.h DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

if (AQUILA_TESTS)

file(GLOB TEST_CORE_SOURCES CONFIGURE_DEPENDS test_core/*.[fF]90)
foreach(test_src ${TEST_CORE_SOURCES})
    get_filename_component(test_name ${test_src} NAME_WE)
    add_executable(${test_name} ${test_src})
    target_link_libraries(${test_name} aquila)
    add_test(NAME "aquila_core_${test_name}" COMMAND ${test_name})
endforeach()

endif()

add_executable(aqstack app/aqstack.f90)
target_link_libraries(aqstack PRIVATE aquila)
target_compile_options(aqstack PRIVATE $<$<CONFIG:RELEASE>:-O3>)

add_executable(aqlrgb app/aqlrgb.F90)
target_link_libraries(aqlrgb PRIVATE aquila)
target_compile_options(aqlrgb PRIVATE $<$<CONFIG:RELEASE>:-O3>)

if (AQUILA_PARALLEL)
    target_link_libraries(aqstack PRIVATE OpenMP::OpenMP_Fortran)
    target_link_libraries(aqlrgb PRIVATE OpenMP::OpenMP_Fortran)
endif ()

install(TARGETS aqstack aqlrgb RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

file(GLOB_RECURSE aqscript_SOURCES CONFIGURE_DEPENDS
    src_cpp/interpreter/*.cpp
    src_cpp/interpreter/*.hpp
)

add_library(aqtask_dispatch SHARED ${aqscript_SOURCES})
target_include_directories(aqtask_dispatch PUBLIC src_cpp/interpreter)

if (AQUILA_PARALLEL)
    target_compile_definitions(aqtask_dispatch PRIVATE AQUILA_PARALLEL)
endif ()

install(TARGETS aqtask_dispatch LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})

if (AQUILA_TESTS)

file(GLOB TEST_TASK_DISPATCH_SOURCES CONFIGURE_DEPENDS test_task_dispatch/*.cpp)
foreach(test_src ${TEST_TASK_DISPATCH_SOURCES})
    get_filename_component(test_name ${test_src} NAME_WE)
    add_executable(${test_name} ${test_src})
    target_link_libraries(${test_name} PRIVATE aqtask_dispatch)
    add_test(NAME "aquila_task_${test_name}" COMMAND ${test_name})
endforeach()

endif()

file(GLOB_RECURSE libaq2_SOURCES CONFIGURE_DEPENDS
    src_cpp/lib/*.f90
    src_cpp/lib/*.F90
    src_cpp/lib/*.cpp
    src_cpp/lib/*.hpp
)

add_library(aquila2 SHARED ${libaq2_SOURCES})
target_include_directories(aquila2 PUBLIC src_cpp/lib)
target_compile_options(aquila2 PRIVATE $<$<CONFIG:RELEASE>:-O3>)

if (AQUILA_PARALLEL)
    target_link_libraries(aquila2 PRIVATE OpenMP::OpenMP_CXX)
    target_compile_definitions(aquila2 PRIVATE AQUILA_PARALLEL)
endif ()

target_link_libraries(aquila2 PUBLIC aqtask_dispatch aquila)

install(TARGETS aquila2 LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})

add_executable(aqfindstar src_cpp/app/aqfindstar.cpp)
target_link_libraries(aqfindstar aquila2)

add_executable(aqcli src_cpp/app/aq2cpp.cpp)
target_link_libraries(aqcli 
    -Wl,--no-as-needed aqtask_dispatch aquila2 -Wl,--as-needed 
    ${READLINE_LIBRARIES})

install(TARGETS aqfindstar aqcli RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

if (AQUILA_TESTS)

add_executable(bench_convol test_interactive/bench_convol.F90)
target_link_libraries(bench_convol PRIVATE aquila)

add_executable(bench_stack test_interactive/bench_stack.f90)
target_link_libraries(bench_stack PRIVATE aquila)

add_executable(run_from_arg test_interactive/run_from_arg.cpp)
target_link_libraries(run_from_arg PRIVATE
        -Wl,--no-as-needed aqtask_dispatch aquila2 -Wl,--as-needed) 

if (AQUILA_TUI)
    add_executable(parser_console test_interactive/parser_console.cpp)
    target_link_libraries(parser_console PRIVATE screen dom component
        -Wl,--no-as-needed aqtask_dispatch aquila2 -Wl,--as-needed) 
endif()

endif()
