cmake_minimum_required(VERSION 3.16)
project(aquila LANGUAGES Fortran C)
enable_testing()

add_subdirectory(deps/task-dispatch)
# add_subdirectory(deps/fastconv)

find_package(PNG REQUIRED)
find_library(CFITSIO_LIBRARY cfitsio)
find_library(READLINE_LIBRARY readline)

file(GLOB_RECURSE SRC_SOURCES CONFIGURE_DEPENDS
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
)

file(GLOB_RECURSE SCRIPT_SOURCES CONFIGURE_DEPENDS
    src/script/*.f90
    src/script/*.F90
    src/script/*.c
)

add_executable(aq2 app/aq2/aq2.f90 app/aq2/operation_imports.f90 
    app/aq2/readline_binding.c app/aq2/readline_prompt.F90 
    ${SRC_SOURCES} ${SCRIPT_SOURCES})
target_link_libraries(aq2 PRIVATE task_dispatch_static)
target_link_libraries(aq2 PRIVATE PNG::PNG)
target_link_libraries(aq2 PRIVATE ${CFITSIO_LIBRARY})
target_link_libraries(aq2 PRIVATE ${READLINE_LIBRARY})

add_executable(aqstack app/aqstack.f90  ${SRC_SOURCES})
target_link_libraries(aqstack PRIVATE PNG::PNG)
target_link_libraries(aqstack PRIVATE ${CFITSIO_LIBRARY})

add_executable(aqlrgb app/aqlrgb.F90  ${SRC_SOURCES})
target_link_libraries(aqlrgb PRIVATE PNG::PNG)
target_link_libraries(aqlrgb PRIVATE ${CFITSIO_LIBRARY})

option(ENABLE_OPENMP "Enable OpenMP support" ON)

# Check the compiler
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU|Clang")
    # GCC or Clang
    set(DEBUG_FLAGS "-g -O0 -Wall -Wextra -Wno-unused-dummy-argument -Wno-compare-reals -Wno-conversion -fcheck=all -fbacktrace -pedantic")
    set(RELEASE_FLAGS "-O3 -march=native -funsafe-math-optimizations")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    # Intel Fortran
    set(DEBUG_FLAGS "-g -O0 -traceback -check=all,nouninit -pedantic")
    set(RELEASE_FLAGS "-O3 -xHost")
endif()

# Apply flags depending on build type
set(CMAKE_Fortran_FLAGS_DEBUG "${DEBUG_FLAGS}" CACHE STRING "Debug flags" FORCE)
set(CMAKE_Fortran_FLAGS_RELEASE "${RELEASE_FLAGS}" CACHE STRING "Release flags" FORCE)


# target_link_libraries(aq2 PRIVATE fastconv_static)
# file(GLOB TEST_SOURCES CONFIGURE_DEPENDS test/*.f90 test/*.F90)

# foreach(test_src ${TEST_SOURCES})
#     get_filename_component(test_name ${test_src} NAME_WE)
#     add_executable(${test_name} ${test_src} test/utils/assert.f90)
#     target_link_libraries(${test_name} PRIVATE task_dispatch_static)
#     add_test(NAME ${test_name} COMMAND ${test_name})
# endforeach()
