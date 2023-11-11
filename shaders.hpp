#pragma once

namespace shaders
{

///////////////////////////////////////////////////////////////////////////////
// VS
///////////////////////////////////////////////////////////////////////////////

constexpr auto main_vs = R"glsl(

layout(location = 0) in vec3 in_position;
layout(location = 1) in vec3 in_normal;

#if TEXTURED
layout(location = 2) in vec2 in_texcoord;
#endif

layout(location = 0) uniform mat4 u_proj_mat;
layout(location = 1) uniform mat4 u_view_mat;
layout(location = 2) uniform mat4 u_model_mat;

layout(location = 10) uniform int   u_shell_count;
layout(location = 11) uniform int   u_shell_index;
layout(location = 12) uniform int   u_fur_texture_size;
layout(location = 13) uniform float u_shell_height_delta;
layout(location = 14) uniform float u_cone_radius_delta;
layout(location = 15) uniform vec3  u_displacement;

layout(binding = 10) uniform sampler2D u_fur_texture;

out VS_TO_GS
{
	vec3 view_space_position;
	vec3 view_space_normal;
	vec3 world_space_normal;

#if TEXTURED
	vec2 texcoord;
#endif

	vec3  base_world_space_position;
} vs_to_gs;

vec3 get_shell_position()
{
	const float shell_height = u_shell_height_delta * u_shell_index;
	return in_position + shell_height * in_normal;
}

#define FUR_CURVATURE 2

vec3 get_perturbation()
{
	const float norm_shell_index = float(u_shell_index) / float(u_shell_count);
	return u_displacement * pow(norm_shell_index, FUR_CURVATURE);
}

void main()
{
	const vec3 shell_position = get_shell_position();
	const vec3 perturbation   = get_perturbation();

	const vec3 world_space_shell_position = vec3(u_model_mat * vec4(shell_position, 1)) + perturbation;
	const vec3 world_space_shell_normal = normalize(vec3(u_model_mat * vec4(in_normal, 0)) + perturbation);

	const vec4 position = u_proj_mat * u_view_mat * vec4(world_space_shell_position, 1);
	gl_Position = position;

	vs_to_gs.view_space_position = vec3(u_view_mat * vec4(world_space_shell_position, 1));
	vs_to_gs.view_space_normal   = vec3(u_view_mat * vec4(world_space_shell_normal,   0));
	vs_to_gs.world_space_normal  = world_space_shell_normal;

#if TEXTURED
	vs_to_gs.texcoord = in_texcoord;
#endif

	vs_to_gs.base_world_space_position = vec3(u_model_mat * vec4(in_position, 1));
}
)glsl";

///////////////////////////////////////////////////////////////////////////////
// GS
///////////////////////////////////////////////////////////////////////////////

constexpr auto main_gs = R"glsl(

layout(triangles) in;
layout(triangle_strip, max_vertices=3) out;

in VS_TO_GS
{
	vec3 view_space_position;
	vec3 view_space_normal;
	vec3 world_space_normal;

#if TEXTURED
	vec2 texcoord;
#endif

	vec3  base_world_space_position;
} vs_to_gs[];

out GS_TO_FS
{
	vec3 view_space_position;
	vec3 view_space_normal;
	vec3 world_space_normal;

#if TEXTURED
	vec2 texcoord;
#endif

	vec2 uniform_texcoord;
	mat3 texture_mat;
} gs_to_fs;

layout(location = 0) uniform mat4 u_proj_mat;
layout(location = 1) uniform mat4 u_view_mat;
layout(location = 2) uniform mat4 u_model_mat;

layout(location = 10) uniform int   u_shell_count;
layout(location = 11) uniform int   u_shell_index;
layout(location = 12) uniform int   u_fur_texture_size;
layout(location = 13) uniform float u_shell_height_delta;
layout(location = 14) uniform float u_cone_radius_delta;
layout(location = 15) uniform vec3  u_displacement;

layout(binding = 10) uniform sampler2D u_fur_texture;

// Reference: https://stackoverflow.com/questions/46345491/converting-3d-triangle-to-2d-in-orthonormal-basis
// Calculates the vertices of 3D triangle in a local 2D orthonormal basis of the triangle's plane
void project_triangle(
	in  vec3 P0, in  vec3 P1, in  vec3 P2,
	out vec2 Q0, out vec2 Q1, out vec2 Q2)
{
	const vec3 X = normalize(P1 - P0);
	const vec3 Z = normalize(cross(X, P2 - P0));
	const vec3 Y = cross(Z, X);

	Q0 = vec2(0, 0);
	Q1 = vec2(length(P1 - P0), 0);
	Q2 = vec2(dot(P2 - P0, X), dot(P2 - P0, Y));
}

void main()
{
	vec2 Q0, Q1, Q2;
	project_triangle(
		vs_to_gs[0].base_world_space_position,
		vs_to_gs[1].base_world_space_position,
		vs_to_gs[2].base_world_space_position,
		Q0, Q1, Q2);

	const float texel_density = u_fur_texture_size; // #texels per unit world-space (can be adjusted)
	const float uv_x = (Q1.x - Q0.x) * texel_density / u_fur_texture_size;
	const float uv_y = (Q2.y - Q0.y) * texel_density / u_fur_texture_size;

	const vec2 uniform_texcoord[] = vec2[](
		vec2(0,  0),
		vec2(uv_x, 0),
		vec2(0, uv_y)
	);

	vec3 V10 = normalize(vs_to_gs[1].view_space_position - vs_to_gs[0].view_space_position);
	vec3 V20 = normalize(vs_to_gs[2].view_space_position - vs_to_gs[0].view_space_position);
	vec3 T = V10;
	vec3 N = cross(V10, V20);
	vec3 B = cross(N, T);
	mat3 texture_mat = mat3(T, B, N);

	for (uint i = 0; i < 3; i++)
	{
		gl_Position = gl_in[i].gl_Position;

		gs_to_fs.view_space_position = vs_to_gs[i].view_space_position;
		gs_to_fs.view_space_normal   = vs_to_gs[i].view_space_normal;
		gs_to_fs.world_space_normal  = vs_to_gs[i].world_space_normal;

#if TEXTURED
		gs_to_fs.texcoord = vs_to_gs[i].texcoord;
#endif

		gs_to_fs.uniform_texcoord = uniform_texcoord[i];
		gs_to_fs.texture_mat = texture_mat;

		EmitVertex();
	}
	EndPrimitive();
}
)glsl";

///////////////////////////////////////////////////////////////////////////////
// FS
///////////////////////////////////////////////////////////////////////////////

constexpr auto main_fs = R"glsl(

in GS_TO_FS
{
	vec3 view_space_position;
	vec3 view_space_normal;
	vec3 world_space_normal;

#if TEXTURED
	vec2 texcoord;
#endif

	vec2 uniform_texcoord;
	mat3 texture_mat;
} gs_to_fs;

layout(location = 3) uniform vec4  u_base_color_factor;

#if MASKED
layout(location = 4) uniform float u_alpha_cutoff;
#endif

#if TEXTURED
layout(binding = 0) uniform sampler2D u_base_color_texture;
#endif

layout(location = 1) uniform mat4 u_view_mat;
layout(location = 2) uniform mat4 u_model_mat;

layout(location = 10) uniform int   u_shell_count;
layout(location = 11) uniform int   u_shell_index;
layout(location = 12) uniform int   u_fur_texture_size;
layout(location = 13) uniform float u_shell_height_delta;
layout(location = 14) uniform float u_cone_radius_delta;
layout(location = 15) uniform vec3  u_displacement;

layout(binding = 10) uniform sampler2D u_fur_texture;

out vec4 out_color;

#define CONE_RADIUS_SCALE 1.41421356237

float calculate_half_lambert_diffuse(float NdotL)
{
	return pow(NdotL * 0.5 + 0.5, 2.0);
}

float calculate_light_attentuation(float norm_height)
{
	return norm_height; // simple linear scaling
}

vec3 calculate_fur_normal(
	float cone_height,
	float cone_radius,
	vec3  cone_axis_dir,
	vec2  local_space_position,
	mat3  local_space_mat)
{
	vec3 cone_axis_vec = cone_axis_dir * cone_height;

	vec3 cone_radius_dir = normalize(local_space_mat * vec3(local_space_position, 0));
	vec3 cone_radius_vec = cone_radius_dir * cone_radius;

	vec3 cone_surface_vec = cone_axis_vec - cone_radius_vec;
	vec3 tangent_dir = cross(cone_axis_dir, cone_radius_dir);

	const vec3 cone_surface_normal = normalize(cross(tangent_dir, cone_surface_vec));
	return cone_surface_normal;
}

vec3 calculate_fur_normal_approx(float cone_height, float cone_radius, vec3 cone_axis_dir, vec3 view_dir)
{
	const vec3 cone_axis_vec = cone_axis_dir * cone_height;

	const vec3 tangent_dir = cross(cone_axis_dir, view_dir);

	const vec3 cone_radius_dir = cross(tangent_dir, cone_axis_dir);
	const vec3 cone_radius_vec = cone_radius_dir * cone_radius;

	const vec3 cone_surface_vec = cone_axis_vec - cone_radius_vec;
	const vec3 cone_surface_normal = normalize(cross(tangent_dir, cone_surface_vec));

	return cone_surface_normal;
}

void main()
{
	// Point light
	//const vec3 light_pos = vec3(0, 0, 0);
	//const vec3 light_dir = normalize(light_pos - gs_to_fs.view_space_position);

	// Directional light
	const vec3 light_pos = vec3(1, 1, 1);
	const vec3 light_dir = normalize(light_pos);

	const vec3 view_dir = -normalize(gs_to_fs.view_space_position);

	vec3 normal = normalize(gs_to_fs.view_space_normal);
	float NdotL = dot(normal, light_dir);

#if DOUBLE_SIDED
	if (NdotL < 0)
	{
		normal = -normal;
		NdotL  = -NdotL;
	}
#else
	NdotL = max(0, NdotL);
#endif

	const float diffuse = calculate_half_lambert_diffuse(NdotL);

	vec4 base_color = u_base_color_factor;

#if TEXTURED
	base_color *= texture(u_base_color_texture, gs_to_fs.texcoord);
#endif

#if MASKED
	if (base_color.a - u_alpha_cutoff < 0.0) discard;
#endif

	const vec3 color = diffuse * base_color.rgb;
	out_color = vec4(color, 1);

	const int frag_shell_index = int(texture(u_fur_texture, gs_to_fs.uniform_texcoord).r);

	if (u_shell_index > frag_shell_index) discard;

	const vec2  local_space_position = fract(gs_to_fs.uniform_texcoord * u_fur_texture_size) * 2 - 1;

	const float norm_shell_height = float(u_shell_index) / float(frag_shell_index);
	const float local_space_cone_radius = CONE_RADIUS_SCALE * (1.0 - norm_shell_height);

	if (length(local_space_position) > local_space_cone_radius && u_shell_index > 0) discard;

	const vec3 fur_normal = calculate_fur_normal(
		u_shell_height_delta,
		u_cone_radius_delta * CONE_RADIUS_SCALE,
		gs_to_fs.view_space_normal,
		local_space_position,
		gs_to_fs.texture_mat);

	/*
	const vec3 fur_normal = calculate_fur_normal_approx(
		u_shell_height_delta, u_cone_radius_delta, view_dir, gs_to_fs.view_space_normal);
	*/

	const float fur_NdotL = max(0, dot(fur_normal, light_dir));
	const float fur_diffuse = calculate_half_lambert_diffuse(fur_NdotL);
	const float fur_attenuation = calculate_light_attentuation(norm_shell_height);
	const vec3  fur_color = base_color.rgb * fur_diffuse * fur_attenuation;

	if (base_color.a == 0)
	{
		out_color = vec4(fur_color, 1);
	}
	else
	{
		if (u_shell_index > 0) discard;
		out_color = vec4(color, 1);
	}

	//out_color = vec4(gs_to_fs.uniform_texcoord, 0, 1);
	//out_color = vec4(vec3(fur_diffuse), 1);
	//out_color = vec4(vec3(NdotL), 1);
	//out_color = vec4(fur_normal * 0.5 + 0.5, 1);
}
)glsl";

} // namespace shaders
