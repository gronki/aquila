cmake_minimum_required(VERSION 3.16)
project(aquila LANGUAGES Fortran C CXX)
enable_testing()

find_package(PkgConfig REQUIRED)

set(CMAKE_C_FLAGS_DEBUG   "-g3 -O0 -Wall")
set(CMAKE_C_FLAGS_RELEASE "-O3 -ffast-math -march=native -fopenmp -DNDEBUG -g1")
set(CMAKE_CXX_FLAGS_DEBUG   "-g3 -O0 -Wall")
set(CMAKE_CXX_FLAGS_RELEASE "-O3 -ffast-math -march=native -fopenmp -DNDEBUG -g1")
set(CMAKE_Fortran_FLAGS_DEBUG   "-g3 -O0 -fcheck=all")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -ffast-math -march=native -fopenmp -DNDEBUG -g1")

add_subdirectory(deps/task-dispatch)

find_package(PNG REQUIRED)
pkg_check_modules(CFITSIO REQUIRED cfitsio)
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
)
add_library(aquilalib STATIC ${AQUILA_SOURCES})
target_include_directories(aquilalib PUBLIC src/c_binding)

link_libraries(aquilalib)
link_libraries(${CFITSIO_LIBRARIES})
include_directories(${CFITSIO_INCLUDE_DIRS})
add_compile_options(${CFITSIO_CFLAGS_OTHER})
link_libraries(PNG::PNG)

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

install(TARGETS aqstack aqlrgb aq2 DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)

file(GLOB_RECURSE AQNEW_SOURCES CONFIGURE_DEPENDS
    src_cpp/lib/*.f90
    src_cpp/lib/*.F90
    src_cpp/lib/*.cpp
    src_cpp/lib/*.hpp
)
add_library(aqnew STATIC ${AQNEW_SOURCES})
target_include_directories(aqnew PUBLIC src_cpp/lib/buffer src_cpp/lib/io)
link_libraries(aqnew)

add_executable(aqneo src_cpp/app/aqneo.cpp)
add_executable(aqfindstar src_cpp/app/aqfindstar.cpp)
install(TARGETS aqneo aqfindstar DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)

# target_link_libraries(aq2 PRIVATE fastconv_static)
# file(GLOB TEST_SOURCES CONFIGURE_DEPENDS test/*.f90 test/*.F90)

# foreach(test_src ${TEST_SOURCES})
#     get_filename_component(test_name ${test_src} NAME_WE)
#     add_executable(${test_name} ${test_src} test/utils/assert.f90)
#     target_link_libraries(${test_name} PRIVATE task_dispatch_static)
#     add_test(NAME ${test_name} COMMAND ${test_name})
# endforeach()
