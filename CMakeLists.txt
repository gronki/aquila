cmake_minimum_required(VERSION 3.20)

project(aquila VERSION "260512" LANGUAGES Fortran C CXX)

set(CMAKE_CXX_STANDARD 20)       # request C++20
set(CMAKE_CXX_STANDARD_REQUIRED ON)  # fail if compiler does not support it
set(CMAKE_CXX_EXTENSIONS OFF)    # turn off compiler-specific extensions like gnu++20

include(GNUInstallDirs)
set(CMAKE_INSTALL_LIBDIR ${CMAKE_INSTALL_LIBDIR}/aquila)
set(CMAKE_INSTALL_INCLUDEDIR ${CMAKE_INSTALL_INCLUDEDIR}/aquila)
set(CMAKE_INSTALL_DATADIR ${CMAKE_INSTALL_DATADIR}/aquila)
set(CMAKE_INSTALL_RPATH "$ORIGIN/../${CMAKE_INSTALL_LIBDIR}:$ORIGIN")

set(CMAKE_C_FLAGS_RELEASE "-O2 -DNDEBUG -g1")
set(CMAKE_CXX_FLAGS_RELEASE "-O2 -DNDEBUG -g1")
set(CMAKE_Fortran_FLAGS_RELEASE "-O2 -DNDEBUG -g1")

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

option(AQUILA_BUILD_TESTS "build tests and examples" On)
if (AQUILA_BUILD_TESTS)
enable_testing()
endif()

set(CXX_WARN "-Wall -Wextra -pedantic -Wpedantic -Werror=return-type -Werror=uninitialized -Werror=sign-compare -Werror=maybe-uninitialized -Wno-unused-parameter")
set(FORT_WARN "-Wall -Wextra -pedantic -Wno-unused-dummy-argument")
set(CMAKE_C_FLAGS_DEBUG "-g -Og ${CXX_WARN}")
set(CMAKE_CXX_FLAGS_DEBUG "-g -Og ${CXX_WARN}")
set(CMAKE_Fortran_FLAGS_DEBUG "-g -Og -fcheck=all ${FORT_WARN}")

add_library(fast_math INTERFACE)
target_compile_options(fast_math INTERFACE
    $<$<CONFIG:Release>:
        -O3 $<IF:$<COMPILE_LANGUAGE:Fortran>,-funsafe-math-optimizations,-ffast-math>
    >
)

option(AQUILA_OPENMP "use OpenMP in build" Off)

if (AQUILA_OPENMP)
find_package(OpenMP)
endif()

pkg_check_modules(READLINE REQUIRED readline)

include_directories(src_cpp/global)

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
target_link_libraries(aquila PRIVATE fast_math)
target_include_directories(aquila PUBLIC src/c_binding)
target_link_libraries(aquila PUBLIC png_target cfitsio)

if (AQUILA_OPENMP)
    target_link_libraries(aquila PRIVATE OpenMP::OpenMP_Fortran OpenMP::OpenMP_C)
endif ()

install(TARGETS aquila LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})
install(FILES src/c_binding/aquila.h DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

if (AQUILA_BUILD_TESTS)

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

add_executable(aqlrgb app/aqlrgb.F90)
target_link_libraries(aqlrgb PRIVATE aquila)

if (AQUILA_OPENMP)
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

install(TARGETS aqtask_dispatch LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})

if (AQUILA_BUILD_TESTS)

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
target_include_directories(aquila2 PUBLIC 
    src_cpp/lib/buffer src_cpp/lib/io
    src_cpp/lib/operations src_cpp/lib/values src_cpp/lib/utils)

if (AQUILA_OPENMP)
    target_link_libraries(aquila2 PRIVATE OpenMP::OpenMP_CXX)
endif ()

target_link_libraries(aquila2 PUBLIC aqtask_dispatch aquila)
target_link_libraries(aquila2 PRIVATE fast_math)

install(TARGETS aquila2 LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})

add_executable(aqfindstar src_cpp/app/aqfindstar.cpp)
target_link_libraries(aqfindstar aquila2)

add_executable(aqcli src_cpp/app/aq2cpp.cpp)
target_link_libraries(aqcli 
    -Wl,--no-as-needed aqtask_dispatch aquila2 -Wl,--as-needed 
    ${READLINE_LIBRARIES})

install(TARGETS aqfindstar aqcli RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

if (AQUILA_BUILD_TESTS)

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
