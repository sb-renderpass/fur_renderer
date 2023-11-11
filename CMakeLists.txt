cmake_minimum_required(VERSION 3.26)

project(renderer)

add_subdirectory(external)

add_executable(renderer)

target_compile_features(renderer
	PRIVATE
		cxx_std_20)

target_sources(renderer
	PRIVATE
		main.cpp
		gl.cpp)

target_link_libraries(renderer
	PRIVATE
		glfw3
		glad
		tinygltf
		glm)

add_custom_target(
	copy-assets ALL
	COMMAND ${CMAKE_COMMAND}
	-E copy_directory_if_different
	${CMAKE_SOURCE_DIR}/assets
	$<TARGET_FILE_DIR:${PROJECT_NAME}>)
