cmake_minimum_required(VERSION 3.18.0)
project(corba-example)

find_package(PkgConfig REQUIRED)
pkg_check_modules(OMNIORB REQUIRED omniORB4)

message("omniORB4 INCLUDE_DIRS = " ${OMNIORB_INLCUDE_DIRS})
message("omniORB4 CFLAGS = " ${OMNIORB_CFLAGS})
message("omniORB4 LINK_LIBRARIES = " ${OMNIORB_LINK_LIBRARIES})
