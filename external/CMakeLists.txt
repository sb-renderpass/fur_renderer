cmake_minimum_required(VERSION 3.26)

###############################################################################
# glfw3
###############################################################################

add_library(glfw3 INTERFACE)

target_include_directories(glfw3
	INTERFACE
		glfw/include)

target_link_directories(glfw3
	INTERFACE
		glfw/lib-vc2022)

target_link_libraries(glfw3
	INTERFACE
		glfw3.lib)

###############################################################################
# glad
###############################################################################

add_library(glad STATIC)

target_include_directories(glad
	PUBLIC
		glad/include)

target_sources(glad
	PRIVATE
		glad/src/gl.c)

###############################################################################
# tinygltf
###############################################################################

add_library(tinygltf INTERFACE)

target_include_directories(tinygltf
	INTERFACE
		tinygltf)

###############################################################################
# glm
###############################################################################

add_library(glm INTERFACE)

target_include_directories(glm
	INTERFACE
		glm)
