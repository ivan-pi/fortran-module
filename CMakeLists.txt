cmake_minimum_required(VERSION 3.0)
project(precice-fortran-api Fortran CXX)
find_package(precice REQUIRED CONFIG)
IF(precice_FOUND)
    MESSAGE("Found precice.")

    get_property(propVal TARGET precice::precice PROPERTY INCLUDE_DIRECTORIES)

    MESSAGE("precice include dir is: ${propVal}")

ENDIF(precice_FOUND)

add_library(precice_pfapi src/precice_participant_api_impl.cpp)
target_include_directories(precice_pfapi PRIVATE /usr/local/include)
target_link_libraries(precice_pfapi PUBLIC /usr/local/lib/libprecice.dylib)

add_library(precice-fortran src/precice_participant_api.F90)
target_link_libraries(precice-fortran PRIVATE precice_pfapi)

add_executable(precice_demo src/precice_demo.f90)
target_link_libraries(precice_demo PUBLIC precice-fortran)


add_executable(solverdummy_v2 examples/solverdummy/solverdummy_v2.f90)
target_link_libraries(solverdummy_v2 PUBLIC precice-fortran)
