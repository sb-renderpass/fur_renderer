#include "gl.hpp"

#include <array>
#include <iostream>

namespace gl
{

///////////////////////////////////////////////////////////////////////////////
// Buffer
///////////////////////////////////////////////////////////////////////////////

buffer_t::buffer_t(size_t size, const void* data, GLbitfield flags)
: resource_t {size, data, flags}
{
}

auto buffer_t::create(size_t size, const void* data, GLbitfield flags) -> GLuint
{
	GLuint handle {GL_NONE};
	glCreateBuffers(1, &handle);
	glNamedBufferStorage(handle, size, data, flags);
	return handle;
}

auto buffer_t::destroy(GLuint* handle) -> void
{
	if (handle) glDeleteBuffers(1, handle);
}

///////////////////////////////////////////////////////////////////////////////
// Shader
///////////////////////////////////////////////////////////////////////////////

shader_t::shader_t(GLenum type, const std::vector<const GLchar*>& sources)
: resource_t {type, sources}
{
}

auto shader_t::create(GLenum type, const std::vector<const GLchar*>& sources) -> GLuint
{
	const auto handle = glCreateShader(type);
	glShaderSource(handle, sources.size(), sources.data(), nullptr);
	glCompileShader(handle);
	GLint success {GL_TRUE};
	glGetShaderiv(handle, GL_COMPILE_STATUS, &success);
	if (!success)
	{
		std::array<char, 1024> log;
		glGetShaderInfoLog(handle, log.size(), nullptr, log.data());
		std::cerr << "Shader build error!\n" << log.data() << '\n';
		return GL_NONE;
	}
	return handle;
}

auto shader_t::destroy(GLuint* handle) -> void
{
	if (handle) glDeleteShader(*handle);
}

///////////////////////////////////////////////////////////////////////////////
// Program
///////////////////////////////////////////////////////////////////////////////

program_t::program_t(const std::vector<shader_t>& shaders)
: resource_t {shaders}
{
}

auto program_t::create(const std::vector<shader_t>& shaders) -> GLuint
{
	const auto handle = glCreateProgram();
	for (const auto& shader : shaders)
	{
		glAttachShader(handle, shader.get());
	}
	glLinkProgram(handle);
	GLint success {GL_TRUE};
	glGetProgramiv(handle, GL_LINK_STATUS, &success);
	if (!success)
	{
		std::array<char, 1024> log;
		glGetProgramInfoLog(handle, log.size(), nullptr, log.data());
		std::cerr << "Program link error!\n" << log.data() << '\n';
		return GL_NONE;
	}
	return handle;
}

auto program_t::destroy(GLuint* handle) -> void
{
	if (handle) glDeleteProgram(*handle);
}

auto create_program(
	const char*	vs_source,
	const char*	gs_source,
	const char*	fs_source,
	hash_t		hash
) -> program_t
{
	std::vector<const GLchar*> header;
	header.push_back("#version 460 core\n\n");
	header.push_back(hash & program_hash_flags_t::e_textured     ? "#define TEXTURED     1\n" : "#define TEXTURED     0\n");
	header.push_back(hash & program_hash_flags_t::e_masked       ? "#define MASKED       1\n" : "#define MASKED       0\n");
	header.push_back(hash & program_hash_flags_t::e_double_sided ? "#define DOUBLE_SIDED 1\n" : "#define DOUBLE_SIDED 0\n");
	header.push_back("\n");

	auto vs_sources = header;
	auto gs_sources = header;
	auto fs_sources = header;
	vs_sources.push_back(vs_source);
	gs_sources.push_back(gs_source);
	fs_sources.push_back(fs_source);
	return {{
		shader_t {GL_VERTEX_SHADER,   vs_sources},
		shader_t {GL_GEOMETRY_SHADER, gs_sources},
		shader_t {GL_FRAGMENT_SHADER, fs_sources},
	}};
}

///////////////////////////////////////////////////////////////////////////////
// Texture
///////////////////////////////////////////////////////////////////////////////

namespace details
{

texture_resource_t::texture_resource_t(const texture_create_info_t& info)
: resource_t {info}
{
}

auto texture_resource_t::create(const texture_create_info_t& info) -> GLuint
{
	const auto texel_format = get_texel_format(info.sized_format);

	const auto mip_levels = info.is_mipmapped ?
		calculate_mip_levels(std::max(info.width, info.height)) :
		1;

	GLuint handle {GL_NONE};
	glCreateTextures(GL_TEXTURE_2D, 1, &handle);
	glTextureStorage2D(handle, mip_levels, texel_format.sized_format, info.width, info.height);

	glTextureParameteri(handle, GL_TEXTURE_WRAP_S,		info.parameters.wrap_s);
	glTextureParameteri(handle, GL_TEXTURE_WRAP_T,		info.parameters.wrap_t);
	glTextureParameteri(handle, GL_TEXTURE_MAG_FILTER,	info.parameters.mag_filter);
	glTextureParameteri(handle, GL_TEXTURE_MIN_FILTER,	info.parameters.min_filter);
	if (info.parameters.max_anisotropy > 0)
	{
		glTextureParameterf(handle, GL_TEXTURE_MAX_ANISOTROPY,	info.parameters.max_anisotropy);
	}

	if (info.data)
	{
		glTextureSubImage2D(
			handle,
			0,
			0, 0,
			info.width, info.height,
			texel_format.base_format,
			texel_format.element_type,
			info.data);

		if (info.is_mipmapped) glGenerateTextureMipmap(handle);
	}

	return handle;
}

auto texture_resource_t::destroy(GLuint* handle) -> void
{
	if (handle) glDeleteTextures(1, handle);
}

} // namespace gl::details

#define ADD_TEXEL_FORMAT(sized, base, type) \
case sized: return {sized, base, type};

#define ADD_TEXEL_FORMAT_TYPE(type, suffix)			\
ADD_TEXEL_FORMAT(GL_RGBA##suffix, GL_RGBA, type)	\
ADD_TEXEL_FORMAT(GL_RGB##suffix,  GL_RGB,  type)	\
ADD_TEXEL_FORMAT(GL_RG##suffix,   GL_RG,   type)	\
ADD_TEXEL_FORMAT(GL_R##suffix,    GL_RED,  type)

constexpr auto get_texel_format(GLenum sized_format) -> texel_format_t
{
	switch (sized_format)
	{
		ADD_TEXEL_FORMAT_TYPE(GL_UNSIGNED_BYTE,		8)
		ADD_TEXEL_FORMAT_TYPE(GL_UNSIGNED_INT,		8UI)
		ADD_TEXEL_FORMAT_TYPE(GL_INT,				8I)

		ADD_TEXEL_FORMAT_TYPE(GL_UNSIGNED_SHORT,	16)
		ADD_TEXEL_FORMAT_TYPE(GL_FLOAT,				16F)
		ADD_TEXEL_FORMAT_TYPE(GL_UNSIGNED_INT,		16UI)
		ADD_TEXEL_FORMAT_TYPE(GL_INT,				16I)

		ADD_TEXEL_FORMAT_TYPE(GL_FLOAT,				32F)
		ADD_TEXEL_FORMAT_TYPE(GL_UNSIGNED_INT,		32UI)
		ADD_TEXEL_FORMAT_TYPE(GL_INT,				32I)
	};
}

#undef ADD_TEXEL_FORMAT
#undef ADD_TEXEL_FORMAT_TYPE

auto calculate_mip_levels(int dim) -> int
{
	return static_cast<int>(std::ceil(std::log2(dim))) + 1;
}

texture_t::texture_t(const texture_create_info_t& info)
: details::texture_resource_t {info}
, width			{width}
, height		{height}
, texel_format	{get_texel_format(info.sized_format)}
{
}

} // namespace gl
