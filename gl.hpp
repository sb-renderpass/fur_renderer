#pragma once

#include <optional>
#include <vector>

#include <glad/gl.h>

#include "types.hpp"

template <typename T>
using gl_resource_t = resource_t<T, GLuint>;

#define DECLARE_GL_RESOURCE_CLASS(cls, ...)	\
DECLARE_RESOURCE_CLASS(cls, GLuint, __VA_ARGS__)

#define DECLARE_GL_RESOURCE_CLASS_MEMBERS(cls, ...)	\
DECLARE_RESOURCE_CLASS_MEMBERS(cls, GLuint, __VA_ARGS__)

namespace gl
{

struct texture_parameters_t
{
	GLenum wrap_s			{GL_REPEAT};
	GLenum wrap_t			{GL_REPEAT};
	GLenum mag_filter		{GL_LINEAR};
	GLenum min_filter		{GL_LINEAR};
	float  max_anisotropy	{0.0F};
};

template <
	GLenum	wrap_s			= GL_REPEAT,
	GLenum	wrap_t 			= GL_REPEAT,
	GLenum	mag_filter		= GL_LINEAR,
	GLenum	min_filter		= GL_LINEAR,
	float	max_anisotropy	= 0.0F
>
static texture_parameters_t static_texture_parameters
{
	.wrap_s			= wrap_s,
	.wrap_t			= wrap_t,
	.mag_filter		= mag_filter,
	.min_filter		= min_filter,
	.max_anisotropy	= max_anisotropy,
};

struct texture_create_info_t
{
	int width  {0};
	int height {0};

	GLenum sized_format {GL_RGBA8};

	const void* data {nullptr};

	bool is_mipmapped {false};

	texture_parameters_t parameters;
};

DECLARE_GL_RESOURCE_CLASS(
	buffer_t,
	size_t size,
	const void* data = nullptr,
	GLbitfield flags = GL_DYNAMIC_STORAGE_BIT)

DECLARE_GL_RESOURCE_CLASS(
	shader_t,
	GLenum type,
	const std::vector<const GLchar*>& sources)

DECLARE_GL_RESOURCE_CLASS(
	program_t,
	const std::vector<shader_t>& shaders)

namespace details
{

DECLARE_GL_RESOURCE_CLASS(
	texture_resource_t,
	const texture_create_info_t& info)

} // namespace gl::details

struct texel_format_t
{
	GLenum sized_format	{GL_RGBA8};
	GLenum base_format	{GL_RGBA};
	GLenum element_type	{GL_UNSIGNED_BYTE};
};

constexpr auto get_texel_format(GLenum sized_format) -> texel_format_t;

auto calculate_mip_levels(int dim) -> int;

struct texture_t : details::texture_resource_t
{
	texture_t() = default;

	texture_t(const texture_create_info_t& info);

	int width  {0};
	int height {0};

	texel_format_t texel_format;
};

auto create_graphics_program(const char* vs_source, const char* fs_source) -> program_t;

enum program_hash_flags_t
{
	e_textured		= (1 << 0),
	e_masked		= (1 << 1),
	e_double_sided	= (1 << 2),
};

auto create_program(
	const char*	vs_source,
	const char*	gs_source,
	const char*	fs_source,
	hash_t		hash
) -> program_t;

} // namespace gl
