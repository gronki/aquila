cmake_minimum_required(VERSION 3.16)

project(aquila LANGUAGES Fortran C CXX)


set(CMAKE_CXX_STANDARD 20)       # request C++20
set(CMAKE_CXX_STANDARD_REQUIRED ON)  # fail if compiler does not support it
set(CMAKE_CXX_EXTENSIONS OFF)    # turn off compiler-specific extensions like gnu++20

enable_testing()
include(GNUInstallDirs)

find_package(PkgConfig REQUIRED)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build." FORCE)
endif()

if(NOT DEFINED ENV{CFLAGS})
    set(CMAKE_C_FLAGS_DEBUG   "-g3 -O0 -Wall -Wextra -Wpedantic -Werror=return-type -Werror=uninitialized -Werror=maybe-uninitialized -Wno-unused-parameter")
    set(CMAKE_C_FLAGS_RELEASE "-O3 -ffast-math -march=native -DNDEBUG -g1")
endif()
if(NOT DEFINED ENV{CXXFLAGS})
    set(CMAKE_CXX_FLAGS_DEBUG   "-g3 -O0 -Wall -Wextra -Wpedantic -Werror=return-type -Werror=uninitialized -Werror=maybe-uninitialized -Werror=delete-incomplete -Wnon-virtual-dtor -Wno-unused-parameter")
    set(CMAKE_CXX_FLAGS_RELEASE "-O3 -ffast-math -march=native -DNDEBUG -g1")
endif()
if(NOT DEFINED ENV{FFLAGS})
    set(CMAKE_Fortran_FLAGS_DEBUG   "-g3 -O0 -fcheck=all -Wall -Wextra -Wno-unused-dummy-argument")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -funsafe-math-optimizations -march=native -DNDEBUG -g1")
endif()

option(OPENMP "use OpenMP in build" OFF)
if (OPENMP)
add_compile_options(-fopenmp)
endif ()

add_subdirectory(deps/task-dispatch)

find_package(PNG REQUIRED)
pkg_check_modules(CFITSIO REQUIRED cfitsio)
pkg_check_modules(READLINE REQUIRED readline)

link_libraries(${CFITSIO_LIBRARIES})
include_directories(${CFITSIO_INCLUDE_DIRS})
add_compile_options(${CFITSIO_CFLAGS_OTHER})
link_libraries(PNG::PNG)

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
)

add_library(aquila SHARED ${AQUILA_SOURCES})
target_include_directories(aquila PUBLIC src/c_binding)
install(TARGETS aquila DESTINATION ${CMAKE_INSTALL_LIBDIR})
install(FILES src/c_binding/aquila.hpp DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

add_library(aquila_static STATIC ${AQUILA_SOURCES})
target_include_directories(aquila_static PUBLIC src/c_binding)
link_libraries(aquila_static)

file(GLOB_RECURSE SCRIPT_SOURCES CONFIGURE_DEPENDS
    src/script/*.f90
    src/script/*.F90
    src/script/*.c
)

add_executable(aqstack app/aqstack.f90)
add_executable(aqlrgb app/aqlrgb.F90)

add_executable(aq2 app/aq2/aq2.f90 app/aq2/operation_imports.f90 
    app/aq2/readline_binding.c app/aq2/readline_prompt.F90 ${SCRIPT_SOURCES})
target_link_libraries(aq2 PRIVATE task_dispatch_static)
target_link_libraries(aq2 PRIVATE ${READLINE_LIBRARIES})

install(TARGETS aqstack aqlrgb aq2 DESTINATION ${CMAKE_INSTALL_BINDIR})

file(GLOB_RECURSE AQNEW_SOURCES CONFIGURE_DEPENDS
    src_cpp/lib/*.f90
    src_cpp/lib/*.F90
    src_cpp/lib/*.cpp
    src_cpp/lib/*.hpp
)
add_library(aqnew SHARED ${AQNEW_SOURCES})
target_include_directories(aqnew PUBLIC 
    src_cpp/lib/buffer src_cpp/lib/io src_cpp/lib/interpreter 
    src_cpp/lib/operations src_cpp/lib/values src_cpp/lib/utils)
link_libraries(aqnew)

add_executable(aqneo src_cpp/app/aqneo.cpp)
add_executable(aqfindstar src_cpp/app/aqfindstar.cpp)
add_executable(aq2cpp src_cpp/app/aq2cpp.cpp)
install(TARGETS aqneo aqfindstar aq2cpp DESTINATION ${CMAKE_INSTALL_BINDIR})
target_link_libraries(aq2cpp PRIVATE ${READLINE_LIBRARIES})

file(GLOB TEST_SOURCES CONFIGURE_DEPENDS test/*.cpp)

foreach(test_src ${TEST_SOURCES})
    get_filename_component(test_name ${test_src} NAME_WE)
    add_executable(${test_name} ${test_src})
    add_test(NAME ${test_name} COMMAND ${test_name})
endforeach()