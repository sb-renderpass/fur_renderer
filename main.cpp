#include <algorithm>
#include <cassert>
#include <iostream>
#include <stack>
#include <string>
#include <vector>

#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>

#define TINYGLTF_IMPLEMENTATION
#define STB_IMAGE_IMPLEMENTATION
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include <tiny_gltf.h>

#include <glm/glm.hpp>
#include <glm/gtc/random.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <glm/gtx/matrix_decompose.hpp>
#include <glm/gtx/quaternion.hpp>
#include <glm/gtx/rotate_vector.hpp>
#include <glm/gtx/string_cast.hpp>

#include "gl.hpp"
#include "shaders.hpp"
#include "types.hpp"

namespace settings
{

constexpr auto xdim  = 512;
constexpr auto ydim  = 512;
constexpr auto yfov  = 60.0F;
constexpr auto znear = 0.05F;
constexpr auto zfar  = 100.0F;

constexpr auto aspect_ratio = static_cast<float>(xdim) / static_cast<float>(ydim);

constexpr auto base_shell_scale = 0.1F;
constexpr auto shell_gap_count = 64;

constexpr auto fur_texture_size = 64;

} // namespace settings

template <typename T>
auto has_property(T value) -> bool
{
	if constexpr (std::is_same_v<T, int>)
	{
		return value != -1;
	}
	else if (std::is_same_v<T, std::string> || std::is_same_v<T, std::vector<double>>)
	{
		return !value.empty();
	}
}

template <typename T>
auto get_valid_name(const T& obj, const std::string& fallback = "<NO NAME>") -> std::string
{
	return has_property(obj.name) ? obj.name : fallback;
}

using tinygltf_matrix_t = decltype(tinygltf::Node::matrix);

auto to_mat4(const tinygltf_matrix_t& m) -> glm::mat4
{
	return glm::make_mat4(m.data());
}

using tinygltf_vector_t = decltype(tinygltf::Node::scale);

auto to_vec3(const tinygltf_vector_t& v) -> glm::vec3
{
	return glm::make_vec3(v.data());
}

auto to_vec4(const tinygltf_vector_t& v) -> glm::vec4
{
	return glm::make_vec4(v.data());
}

auto to_quat(const tinygltf_vector_t& v) -> glm::quat
{
	return glm::make_quat(v.data());
}

using tinygltf_color_t = decltype(tinygltf::Material::pbrMetallicRoughness.baseColorFactor);

auto to_color(const tinygltf_color_t& c) -> color_t
{
	assert(c.size() == 4);
	return {
		static_cast<float>(c[0]),
		static_cast<float>(c[1]),
		static_cast<float>(c[2]),
		static_cast<float>(c[3]),
	};
}

auto to_vector(const glm::mat4& m) -> tinygltf_matrix_t
{
	tinygltf_matrix_t result (16, 0);
	std::copy_n(glm::value_ptr(m), result.size(), std::begin(result));
	return result;
}

struct buffer_view_t
{
	size_t byte_stride {0};

	gl::buffer_t buffer;

	static auto create(
		size_t byte_stride,
		size_t byte_length,
		const void* data
	) -> buffer_view_t;
};

auto buffer_view_t::create(
	size_t byte_stride,
	size_t byte_length,
	const void* data
) -> buffer_view_t
{
	return {byte_stride, gl::buffer_t {byte_length, data}};
}

using buffer_view_map_t = std::unordered_map<int, buffer_view_t>;

struct accessor_t
{
	int    buffer_view    {-1};
	int    component_type {GL_NONE};
	int    num_components {0};
	int    component_size {0};
	bool   is_normalized  {false};
	size_t byte_offset    {0};
	size_t count          {0};

	using minmax_t = glm::vec3;
	minmax_t min_values {0, 0, 0};
	minmax_t max_values {0, 0, 0};

	explicit operator bool() const noexcept { return buffer_view >= 0; }

	// See tinygltf::accessor_t::ByteStride()
	auto calculate_byte_stride(const buffer_view_t& bv) const -> size_t
	{
		return bv.byte_stride > 0 ? bv.byte_stride : num_components * component_size;
	}

	auto bind(GLuint location, GLuint vao, const buffer_view_t& bv) const -> void
	{
		glVertexArrayVertexBuffer (vao, location, bv.buffer.get(), byte_offset, calculate_byte_stride(bv));
		glEnableVertexArrayAttrib (vao, location);
		glVertexArrayAttribFormat (vao, location, num_components, component_type, is_normalized, 0);
		glVertexArrayAttribBinding(vao, location, location);
	}
};

using tinygltf_accessor_minmax_t = decltype(tinygltf::Accessor::minValues);

// NOTE: Restricted to SCALAR, VEC2, VEC3 types only
auto get_minmax_values(const tinygltf_accessor_minmax_t& v) -> accessor_t::minmax_t
{
	constexpr auto expected_components = 3;
	const auto min_components = std::min<int>(v.size(), expected_components);
	accessor_t::minmax_t result;
	for (auto i = 0; i < expected_components; i++)
	{
		result[i] = i < min_components ? v[i] : 0;
	}
	return result;
}

auto make_accessor(const tinygltf::Accessor& a) -> accessor_t
{
	return
	{
		.buffer_view    = a.bufferView,
		.component_type = a.componentType,
		.num_components = tinygltf::GetNumComponentsInType(a.type),
		.component_size = tinygltf::GetComponentSizeInBytes(a.componentType),
		.is_normalized  = a.normalized,
		.byte_offset    = a.byteOffset,
		.count          = a.count,
		.min_values		= get_minmax_values(a.minValues),
		.max_values		= get_minmax_values(a.maxValues),
	};
}

struct material_t
{
	color_t base_color_factor	{1, 1, 1, 1};
	int     base_color_texture	{-1};
	float   alpha_cutoff        {0.5F};
	hash_t  hash                {0};

	auto create_program() const -> gl::program_t
	{
		return gl::create_program(shaders::main_vs, shaders::main_gs, shaders::main_fs, hash);
	}
};

struct primitive_t
{
	accessor_t indices;
	accessor_t position;
	accessor_t normal;
	accessor_t texcoord;

	material_t material;

	GLuint vao {GL_NONE};

	static auto create(
		const accessor_t& indices,
		const accessor_t& position,
		const accessor_t& normal,
		const accessor_t& texcoord,
		const material_t& material,
		const buffer_view_map_t& buffer_view_map
	) -> primitive_t;
};

auto primitive_t::create(
	const accessor_t& indices,
	const accessor_t& position,
	const accessor_t& normal,
	const accessor_t& texcoord,
	const material_t& material,
	const buffer_view_map_t& buffer_view_map
) -> primitive_t
{

	GLuint vao {GL_NONE};
	glCreateVertexArrays(1, &vao);

	const auto& indices_buffer_view = buffer_view_map.at(indices.buffer_view);
	glVertexArrayElementBuffer(vao, indices_buffer_view.buffer.get());

	position.bind(0, vao, buffer_view_map.at(position.buffer_view));
	normal  .bind(1, vao, buffer_view_map.at(normal.buffer_view));

	if (texcoord)
	{
		texcoord.bind(2, vao, buffer_view_map.at(texcoord.buffer_view));
	}

	return {indices, position, normal, texcoord, material, vao};
}
	
struct mesh_t
{
	glm::mat4 transform {glm::identity<glm::mat4>()};
	std::vector<primitive_t> primitives;
};

class orbit_camera_t
{
public:
	orbit_camera_t()
	: m_view_mat {glm::lookAt(glm::vec3 {m_position}, glm::vec3 {m_look_at_position}, m_up_direction)}
	, m_proj_mat {glm::perspective(m_yfov, m_aspect_ratio, m_znear, m_zfar)}
	{
	}

	orbit_camera_t(float aspect_ratio, float yfov, float znear, float zfar)
	: m_aspect_ratio	{aspect_ratio > 0 ? aspect_ratio : settings::aspect_ratio}
	, m_yfov			{yfov}
	, m_znear			{znear}
	, m_zfar			{zfar}
	, m_view_mat		{glm::lookAt(glm::vec3 {m_position}, glm::vec3 {m_look_at_position}, m_up_direction)}
	, m_proj_mat
	{
		zfar > 0 ?
			glm::perspective(yfov, aspect_ratio, znear, zfar) :
			glm::infinitePerspective(yfov, aspect_ratio, znear)
	}
	{
	}

	// Reference: https://asliceofrendering.com/camera/2019/11/30/ArcballCamera/
	auto rotate(float angle_delta_x, float angle_delta_y) -> void
	{
		// Lock y-axis rotation if the camera faces up or down
		if (glm::dot(front_direction(), m_up_direction) * glm::sign(angle_delta_y) > 0.99F)
		{
			angle_delta_y = 0;
		}

		const auto rot_mat_x = glm::rotate(glm::identity<glm::mat4>(), angle_delta_x, m_up_direction);
		const auto rot_mat_y = glm::rotate(glm::identity<glm::mat4>(), angle_delta_y, right_direction());
		m_position = (rot_mat_y * rot_mat_x * (m_position - m_look_at_position)) + m_look_at_position;
		update_view_mat();
	}

	auto zoom(float delta) -> void
	{
		m_position += glm::vec4 {delta * front_direction(), 0};
		update_view_mat();
	}

	auto front_direction() const -> glm::vec3 { return -glm::transpose(m_view_mat)[2]; }
	auto right_direction() const -> glm::vec3 { return  glm::transpose(m_view_mat)[0]; }

	auto view_mat() const { return m_view_mat; }
	auto proj_mat() const { return m_proj_mat; }

private:
	auto update_view_mat() -> void
	{
		m_view_mat = glm::lookAt(glm::vec3 {m_position}, glm::vec3 {m_look_at_position}, m_up_direction);
	}

	float m_aspect_ratio {settings::aspect_ratio};
	float m_yfov         {glm::radians(settings::yfov)};
	float m_znear        {settings::znear};
	float m_zfar         {settings::zfar};

	glm::vec4 m_position  {0, 0, 3, 1};

	glm::mat4 m_view_mat  {1};
	glm::mat4 m_proj_mat  {1};

	static constexpr auto m_up_direction     = glm::vec3 {0, 1, 0};
	static constexpr auto m_look_at_position = glm::vec4 {0, 0, 0, 1};
};

struct scene_t
{
	std::unordered_map<int,    buffer_view_t> buffer_view_map;
	std::unordered_map<int,    gl::texture_t> texture_map;
	std::unordered_map<hash_t, gl::program_t> program_map;

	std::vector<mesh_t> meshes;
	std::vector<orbit_camera_t> cameras;

	static auto create(const std::string& gltf_path) -> scene_t;
};

auto scene_t::create(const std::string& gltf_path) -> scene_t
{
	tinygltf::TinyGLTF	loader;
	tinygltf::Model		model;
	std::string			err;
	std::string			warn;

	const auto ret = loader.LoadASCIIFromFile(&model, &err, &warn, gltf_path);

	if (!warn.empty())
	{
		std::cerr << "GLTF warning: " << warn << '\n';
	}
	if (!err.empty())
	{
		std::cerr << "GLTF error: " << err << '\n';
	}
	if (!ret)
	{
		std::cerr << "Failed to load " << gltf_path << '\n';
	}

	std::clog << model.scenes.size()      << " scene(s)\n";
	std::clog << model.buffers.size()     << " buffer(s)\n";
	std::clog << model.bufferViews.size() << " buffer view(s)\n";

	if (model.scenes.empty()) return {};
	const auto root_scene = has_property(model.defaultScene) ? model.defaultScene : 0;

	std::clog << "Scene: " << get_valid_name(model.scenes[root_scene]) << '\n';

	scene_t scene;

	const auto add_node_matrix_if_none = [&](tinygltf::Node& node_obj)
	{
		if (!has_property(node_obj.matrix))
		{
			node_obj.matrix = to_vector(glm::identity<glm::dmat4>());
		}
	};

	const auto try_emplace_buffer_view = [&](int buffer_view)
	{
		const auto& bv  = model.bufferViews[buffer_view];
		const auto data = model.buffers[bv.buffer].data.data() + bv.byteOffset;
		scene.buffer_view_map.try_emplace(buffer_view, buffer_view_t::create(bv.byteStride, bv.byteLength, data));
	};

	// TODO: Generalize for any texture type (assumes base color texturew)
	const auto try_emplace_texture = [&](int texture_id)
	{
		const auto& obj = model.images[texture_id];
		const auto info = gl::texture_create_info_t
		{
			.width			= obj.width,
			.height			= obj.height,
			.sized_format	= GL_RGBA8,
			.data			= obj.image.data(),
			.is_mipmapped	= true,
			.parameters		= gl::static_texture_parameters<GL_REPEAT, GL_REPEAT, GL_LINEAR, GL_LINEAR_MIPMAP_LINEAR>,
		};
		scene.texture_map.try_emplace(texture_id, gl::texture_t {info});
	};

	const auto try_emplace_program = [&](const material_t& material)
	{
		scene.program_map.try_emplace(material.hash, material.create_program());
	};

	const auto& root_nodes = model.scenes[root_scene].nodes;
	std::stack<int, std::vector<int>> nodes {root_nodes};
	while (!nodes.empty())
	{
		const auto node = nodes.top();
		nodes.pop();

		auto& node_obj = model.nodes[node];
		//std::clog << "Node: " << get_valid_name(node_obj.name) << '\n';

		add_node_matrix_if_none(node_obj);
		auto node_matrix = to_mat4(node_obj.matrix);
		//std::clog << "- Matrix: " << glm::to_string(to_mat4(node_obj.matrix)) << '\n';

		if (has_property(node_obj.scale))
		{
			const auto S = glm::scale(glm::mat4 {1}, to_vec3(node_obj.scale));
			node_matrix = node_matrix * S;
		}
		if (has_property(node_obj.rotation))
		{
			const auto R = glm::mat4_cast(to_quat(node_obj.rotation));
			node_matrix = node_matrix * R;
		}
		if (has_property(node_obj.translation))
		{
			const auto T = glm::translate(glm::mat4 {1}, to_vec3(node_obj.translation));
			node_matrix = node_matrix * T;
		}
		node_obj.matrix = to_vector(node_matrix);

		if (has_property(node_obj.camera))
		{
			const auto& camera_obj = model.cameras[node_obj.camera];
			std::clog << "- Camera: " << get_valid_name(camera_obj) << '\n';

			using namespace std::string_literals;
			if (camera_obj.type == "perspective"s)
			{
				// TODO: Use node_matrix as well
				const auto& cam = camera_obj.perspective;
				scene.cameras.emplace_back(
					static_cast<float>(cam.aspectRatio),
					static_cast<float>(cam.yfov),
					static_cast<float>(cam.znear),
					static_cast<float>(cam.zfar));
			}
		}

		if (has_property(node_obj.mesh))
		{
			const auto& mesh_obj = model.meshes[node_obj.mesh];
			std::clog << "- Mesh: " << get_valid_name(mesh_obj) << '\n';

			mesh_t mesh;
			mesh.transform = node_matrix;
			for (const auto& primitive : mesh_obj.primitives)
			{
				if (has_property(primitive.indices)
					&& primitive.attributes.contains("POSITION")
					&& primitive.attributes.contains("NORMAL"))
				{
					material_t material;

					const auto indices  = make_accessor(model.accessors[primitive.indices]);
					const auto position = make_accessor(model.accessors[primitive.attributes.at("POSITION")]);
					const auto normal   = make_accessor(model.accessors[primitive.attributes.at("NORMAL")]);

					try_emplace_buffer_view(indices.buffer_view);
					try_emplace_buffer_view(position.buffer_view);
					try_emplace_buffer_view(normal.buffer_view);

					accessor_t texcoord;
					if (primitive.attributes.contains("TEXCOORD_0"))
					{
						texcoord = make_accessor(model.accessors[primitive.attributes.at("TEXCOORD_0")]);
						try_emplace_buffer_view(texcoord.buffer_view);
						material.hash |= gl::program_hash_flags_t::e_textured;
					}

					std::clog << "-- Primitive: " << (indices.count / 3) << " triangles\n";

					if (has_property(primitive.material))
					{
						const auto& material_obj = model.materials[primitive.material];
						//std::clog << "--- Material: " << get_valid_name(material_obj) << '\n';

						if (material_obj.alphaMode == "MASK")
						{
							material.alpha_cutoff = material_obj.alphaCutoff;
							material.hash |= gl::program_hash_flags_t::e_masked;
						}

						if (material_obj.doubleSided)
						{
							material.hash |= gl::program_hash_flags_t::e_double_sided;
						}

						// Force double-sided for fur materials
						material.hash |= gl::program_hash_flags_t::e_double_sided;

						const auto& pbr_obj = material_obj.pbrMetallicRoughness;
						material.base_color_factor = to_color(pbr_obj.baseColorFactor);

						// TODO: Use fallback texture if none is specified
						if (has_property(pbr_obj.baseColorTexture.index))
						{
							const auto& texture_obj = model.textures[pbr_obj.baseColorTexture.index];
							//std::clog << "---- Texture: " << get_valid_name(texture_obj) << '\n';

							if (has_property(texture_obj.source))
							{
								const auto& image_obj = model.images[texture_obj.source];
								std::clog << "----- Image: "
									<< get_valid_name(image_obj) << ' '
									<< '[' << image_obj.width << 'x' << image_obj.height << 'x' << image_obj.component << "] "
									<< image_obj.uri
									<< '\n';

								const auto is_rgba8 =
									image_obj.bits       == 8 &&
									image_obj.component  == 4 &&
									image_obj.pixel_type == TINYGLTF_COMPONENT_TYPE_UNSIGNED_BYTE;

								if (is_rgba8)
								{
									material.base_color_texture = texture_obj.source;
									try_emplace_texture(texture_obj.source);
								}
							}
						}
					} // primitive.material

					try_emplace_program(material);
	
					mesh.primitives.emplace_back(
						primitive_t::create(
							indices,
							position,
							normal,
							texcoord,
							material,
							scene.buffer_view_map));
				}
				else
				{
					std::cerr << "Primitive without indices, positions, or normals!\n";
				}
			}
			if (!mesh.primitives.empty())
			{
				scene.meshes.push_back(mesh);
			}
		}

		for (const auto child : node_obj.children)
		{
			auto& child_obj = model.nodes[child];
			add_node_matrix_if_none(child_obj);
			child_obj.matrix = to_vector(node_matrix * to_mat4(child_obj.matrix));
			nodes.push(child);
		}
	}

	return scene;
}

struct state_t
{
	orbit_camera_t camera;
	float shell_scale {0.1};
};

auto key_callback(GLFWwindow* window, int key, int scancode, int action, int mods) -> void
{
	auto state = reinterpret_cast<state_t*>(glfwGetWindowUserPointer(window));

	if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS)
	{
		glfwSetWindowShouldClose(window, GLFW_TRUE);
	}

	if (glfwGetKey(window, GLFW_KEY_J) == GLFW_PRESS)
	{
		state->shell_scale += 0.1;
		std::cout << "shell_scale=" << state->shell_scale << '\n';
	}
	if (glfwGetKey(window, GLFW_KEY_K) == GLFW_PRESS)
	{
		state->shell_scale = std::max(0.001F, state->shell_scale - 0.1F);
		std::cout << "shell_scale=" << state->shell_scale << '\n';
	}
}

auto scroll_callback(GLFWwindow* window, double xoffset, double yoffset) -> void
{
	auto state = reinterpret_cast<state_t*>(glfwGetWindowUserPointer(window));

	constexpr auto zoom_scale = 0.1F;
	state->camera.zoom(yoffset * zoom_scale);
}

auto cursor_position_callback(GLFWwindow* window, double xpos, double ypos) -> void
{
	auto state = reinterpret_cast<state_t*>(glfwGetWindowUserPointer(window));

	static auto prev_xpos = 0.0;
	static auto prev_ypos = 0.0;

	const auto angle_delta_x = (prev_xpos - xpos) * (glm::pi<float>() / settings::xdim) * 2;
	const auto angle_delta_y = (prev_ypos - ypos) * (glm::pi<float>() / settings::ydim);
	prev_xpos = xpos;
	prev_ypos = ypos;

	if (glfwGetMouseButton(window, GLFW_MOUSE_BUTTON_RIGHT) == GLFW_PRESS)
	{
		state->camera.rotate(angle_delta_x, angle_delta_y);
	}
}

auto mouse_button_callback(GLFWwindow* window, int button, int action, int mods) -> void
{
	if (button == GLFW_MOUSE_BUTTON_RIGHT)
	{
		glfwSetInputMode(window, GLFW_CURSOR, action == GLFW_PRESS ? GLFW_CURSOR_HIDDEN : GLFW_CURSOR_NORMAL);
	}
}

auto show_gl_info(bool show_extensions = false) -> void
{
	std::cout << "GPU: "    << glGetString(GL_RENDERER) << '\n';
	std::cout << "OpenGL: " << glGetString(GL_VERSION) << '\n';
	std::cout << "GLSL: "   << glGetString(GL_SHADING_LANGUAGE_VERSION) << '\n';
	if (show_extensions)
	{
		auto gl_num_extensions = 0;
		glGetIntegerv(GL_NUM_EXTENSIONS, &gl_num_extensions);
		std::cout << gl_num_extensions << " extensions:\n";
		for (auto i = 0; i < gl_num_extensions; i++)
		{
			std::cout << "- " << glGetStringi(GL_EXTENSIONS, i) << '\n';
		}
	}
}

auto create_fur_texture(int xdim, int ydim, float min = 0, float max = 1) -> gl::texture_t
{
	auto texels = std::vector<float> (xdim * ydim, 1);
	constexpr auto shell_count = settings::shell_gap_count + 1;
	const auto min_shell_index = static_cast<int>(min * shell_count);
	const auto max_shell_index = static_cast<int>(max * shell_count);
	std::ranges::generate(texels, [&]() -> float { return glm::linearRand(min_shell_index, max_shell_index); });
	const auto info = gl::texture_create_info_t
	{
		.width			= xdim,
		.height			= ydim,
		.sized_format	= GL_R32F,
		.data			= texels.data(),
		.is_mipmapped	= false,
		.parameters		= gl::static_texture_parameters<GL_REPEAT, GL_REPEAT, GL_NEAREST, GL_NEAREST>,
	};
	return gl::texture_t {info};
}

auto calculate_shell_scale(const glm::vec3& aabb_min, const glm::vec3& aabb_max) -> float
{
	const auto range = aabb_max - aabb_min;
	const auto avg_range = (range.x + range.y + range.z) / 3.0F;
	return settings::base_shell_scale * avg_range;
}

auto main(int argc, char** argv) -> int
{
	if (!glfwInit())
	{
		return -1;
	}

	glfwWindowHint(GLFW_SAMPLES,	16);
	glfwWindowHint(GLFW_VISIBLE,	GLFW_FALSE);
	glfwWindowHint(GLFW_RESIZABLE,	GLFW_FALSE);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 6);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

	const auto window = glfwCreateWindow(settings::xdim, settings::ydim, "Renderer", nullptr, nullptr);
	if (!window)
	{
		glfwTerminate();
		return -1;
	}

	state_t state;

	glfwSetWindowUserPointer(window, &state);
	glfwSetKeyCallback(window, key_callback);
	glfwSetScrollCallback(window, scroll_callback);
	glfwSetCursorPosCallback(window, cursor_position_callback);
	glfwSetMouseButtonCallback(window, mouse_button_callback);

	glfwMakeContextCurrent(window);
	glfwSwapInterval(1); // Enable vsync

	gladLoadGL(glfwGetProcAddress);

	show_gl_info();

	const auto fur_texture = create_fur_texture(settings::fur_texture_size, settings::fur_texture_size, 0.3);

	const auto gltf_path = argc > 1 ? argv[1] : "models/box_textured/BoxTextured.gltf";
	auto scene = scene_t::create(gltf_path);

	std::cout << "Loaded " << scene.buffer_view_map.size() << " buffer view(s)\n";
	std::cout << "Loaded " << scene.meshes.size()  << " mesh(es)\n";
	std::cout << "Loaded " << scene.cameras.size() << " camera(s)\n";

	state.camera = !scene.cameras.empty() ? scene.cameras[0] : orbit_camera_t {};

	glfwSetCursorPos(window, 0, 0);
	glfwShowWindow(window);

	while (!glfwWindowShouldClose(window))
	{
		const auto time = glfwGetTime();
		const auto displacement = glm::vec3 {0, -0.1, 0} + glm::vec3 {0.1 * std::sin(2 *time), 0, 0};
		//constexpr auto displacement = glm::vec3 {0, -0.1, 0};

		glClearColor(0.25, 0.5, 1, 1);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		glEnable(GL_DEPTH_TEST);
		glEnable(GL_MULTISAMPLE);

		for (auto& mesh : scene.meshes)
		{
			for (const auto& primitive : mesh.primitives)
			{
				const auto& material = primitive.material;
				glUseProgram(scene.program_map.at(material.hash).get());

				glUniformMatrix4fv(0, 1, GL_FALSE, glm::value_ptr(state.camera.proj_mat()));
				glUniformMatrix4fv(1, 1, GL_FALSE, glm::value_ptr(state.camera.view_mat()));
				glUniformMatrix4fv(2, 1, GL_FALSE, glm::value_ptr(mesh.transform));

				glUniform4fv(3, 1, material.base_color_factor.data);

				if (material.hash & gl::program_hash_flags_t::e_masked)
				{
					glUniform1f(4, material.alpha_cutoff);
				}

				if (material.hash & gl::program_hash_flags_t::e_double_sided)
				{
					glDisable(GL_CULL_FACE);
				}
				else
				{
					glEnable(GL_CULL_FACE);
				}

				if (material.base_color_texture >= 0)
				{
					const auto& base_color_texture = scene.texture_map.at(material.base_color_texture);
					glBindTextureUnit(0, base_color_texture.get());
				}

				glBindVertexArray(primitive.vao);

				const auto shell_scale = calculate_shell_scale(
					primitive.position.min_values,
					primitive.position.max_values);
				const auto shell_height_delta = shell_scale / settings::shell_gap_count; 

				const auto cone_radius_delta = 0.5F / (settings::fur_texture_size * settings::shell_gap_count);

				constexpr auto shell_count = settings::shell_gap_count + 1;
				for (auto shell_index = settings::shell_gap_count; shell_index >= 0; shell_index--)
				{
					glUniform1i (10, shell_count);
					glUniform1i (11, shell_index);
					glUniform1i (12, settings::fur_texture_size);
					glUniform1f (13, shell_height_delta);
					glUniform1f (14, cone_radius_delta);
					glUniform3fv(15, 1, glm::value_ptr(displacement));

					glBindTextureUnit(10, fur_texture.get());

					glDrawElements(
						GL_TRIANGLES,
						primitive.indices.count,
						primitive.indices.component_type,
						reinterpret_cast<const GLvoid*>(primitive.indices.byte_offset));
				}
			}
		}

		glfwSwapBuffers(window);
		glfwPollEvents();
	}

	glfwDestroyWindow(window);
	glfwTerminate();

	return 0;
}
