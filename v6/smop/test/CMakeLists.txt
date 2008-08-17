cmake_minimum_required(VERSION 2.6)
include_directories (../include)
link_directories (../src)
add_executable (01_smop_lowlevel.t 01_smop_lowlevel.c)

target_link_libraries (01_smop_lowlevel.t smop)


