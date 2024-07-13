cmake_minimum_required (VERSION 3.13)

project (FangRuntime
  LANGUAGES C)

set (FangRuntime_COMPILE_OPTIONS
  ""
  CACHE
  STRING
  ";-separated list of compilation options.")

set (FangRuntime_LINK_OPTIONS
  ""
  CACHE
  STRING
  ";-separated list of linking options.")

add_library (fang_runtime
  include/fang_rt/alloc.h
  include/fang_rt/error.h
  include/fang_rt/io.h
  include/fang_rt/string.h
  include/fang_rt/types.h
  src/alloc.c
  src/error.c
  src/io.c
  src/main.c
  src/string.c
  src/types.c)

target_compile_features (fang_runtime
  PUBLIC c_std_11)

target_compile_options (fang_runtime
  PRIVATE "${FangRuntime_COMPILE_OPTIONS}")

target_link_options (fang_runtime
  PRIVATE "${FangRuntime_LINK_OPTIONS}")

target_include_directories (fang_runtime
  PUBLIC
    $<INSTALL_INTERFACE:include>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src)
