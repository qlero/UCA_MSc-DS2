# This is a minimal CMakeLists file 
cmake_minimum_required (VERSION 3.16)

project (ID_estimation)

SET(CMAKE_C_COMPILER C:/MinGW/bin/gcc)
SET(CMAKE_CXX_COMPILER C:/MinGW/bin/g++)

find_package(Gnuplot REQUIRED)

add_executable (main main.cpp)
