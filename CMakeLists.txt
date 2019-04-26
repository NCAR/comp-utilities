project(comp-utilities)
set(VERSION_MAJOR "0")
set(VERSION_MINOR "1")
set(VERSION_PATCH "0")
set(VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}")

# required version of CMake
cmake_minimum_required(VERSION 3.1.3)
cmake_policy(SET CMP0017 NEW)
if (POLICY CMP0042)
  cmake_policy(SET CMP0042 NEW)
endif ()

set(CMAKE_MODULE_PATH
  ${CMAKE_SOURCE_DIR}/cmake/Modules
  ${CMAKE_ROOT}/Modules
  ${CMAKE_MODULE_PATH}
)

find_package(Git)
if (GIT_FOUND)
  execute_process(
    COMMAND ${GIT_EXECUTABLE} log -1 --pretty=format:%h
    OUTPUT_VARIABLE GIT_REVISION
  )
  execute_process(
    COMMAND ${GIT_EXECUTABLE} rev-parse --abbrev-ref HEAD
    OUTPUT_STRIP_TRAILING_WHITESPACE
    OUTPUT_VARIABLE GIT_BRANCH
  )
else ()
  set(GIT_REVISION "")
  set(GIT_BRANCH "")
endif ()

include(${CMAKE_SOURCE_DIR}/cmake/GetDate.cmake)
LONG_TODAY(comp_utilities_BUILD_DATE)
message(STATUS "Build date = ${comp_utilities_BUILD_DATE}")

find_package(IDL REQUIRED)
find_package(IDLdoc)

include_directories(${IDL_INCLUDE_DIR})

add_subdirectory(lib)
add_subdirectory(src)
add_subdirectory(ssw)


if (WIN32 OR CYGWIN)
  set(SEP_CHAR ";")
else ()
  set(SEP_CHAR ":")
endif ()

if (IDLdoc_FOUND)
  add_custom_target(doc
    COMMAND ${IDL_EXECUTABLE} -IDL_STARTUP "\"\"" -IDL_PATH "${CMAKE_CURRENT_SOURCE_DIR}/lib${SEP_CHAR}${CMAKE_CURRENT_SOURCE_DIR}${SEP_CHAR}+${CMAKE_CURRENT_SOURCE_DIR}/src${SEP_CHAR}+${IDLdoc_DIR}${SEP_CHAR}\"<IDL_DEFAULT>\"" -e comp_util_make_docs -args ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT build developer API documentation
  )
  add_custom_target(userdoc
    COMMAND ${IDL_EXECUTABLE} -IDL_STARTUP "\"\"" -IDL_PATH "${CMAKE_CURRENT_SOURCE_DIR}/lib${SEP_CHAR}${CMAKE_CURRENT_SOURCE_DIR}${SEP_CHAR}+${CMAKE_CURRENT_SOURCE_DIR}/src${SEP_CHAR}+${IDLdoc_DIR}${SEP_CHAR}\"<IDL_DEFAULT>\"" -e comp_util_make_userdocs -args ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT build user API documentation
  )
endif ()

set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES "api-docs;api-userdocs")

# setup installer

if (WIN32)
  set(CPACK_GENERATOR ZIP)
else ()
  set(CPACK_GENERATOR TGZ)
endif ()

string(TOLOWER "${CMAKE_SYSTEM_NAME}" COMP_UTILITIES_SYSTEM_NAME)
set(COMP_UTILITIES_PACKAGE_VERSION "${VERSION}.${GIT_REVISION}")
set(CPACK_PACKAGE_FILE_NAME "${CMAKE_PROJECT_NAME}-${COMP_UTILITIES_PACKAGE_VERSION}-${COMP_UTILITIES_SYSTEM_NAME}")

include(CPack)

install(FILES
          README.md
        DESTINATION .)

if (IDLdoc_FOUND)
  install(DIRECTORY ${CMAKE_BINARY_DIR}/api-userdocs DESTINATION . OPTIONAL)
endif ()

add_custom_target(dist
  COMMAND ${CMAKE_MAKE_PROGRAM} package
  COMMENT "custom target to create distribution")

if (IDLdoc_FOUND)
  add_dependencies(dist userdoc)
endif ()

