#pragma once

#include <memory>
#include <optional>

template <typename T, typename H>
struct resource_t
{
	using handle_t = H;
	std::shared_ptr<H> handle;

	resource_t() = default;

	template <typename... Args>
	resource_t(Args&&... args)
	: handle {std::shared_ptr<H> {new H {T::create(std::forward<Args>(args)...)}, T::destroy}}
	{
	}
};

#define DECLARE_RESOURCE_CLASS_MEMBERS(cls, H, ...)		\
	cls() = default;									\
	cls(__VA_ARGS__);									\
	static auto create(__VA_ARGS__) -> H;				\
	static auto destroy(H* handle) -> void;				\
	explicit operator bool() const { return !!handle; }	\
	auto get() const -> H {								\
		if (!handle) throw;								\
		return *handle.get();							\
	}													\
	auto try_get() const -> std::optional<H> {			\
		return handle									\
			? std::make_optional(*handle.get())			\
			: std::nullopt;								\
	}

#define DECLARE_RESOURCE_CLASS(cls, H, ...)				\
	struct cls : resource_t<cls, H> {					\
	DECLARE_RESOURCE_CLASS_MEMBERS(cls, H, __VA_ARGS__)	\
	};

using hash_t = uint32_t;

struct color_t
{
	union
	{
		float data[4] = {0, 0, 0, 1};
		struct
		{
			float r;
			float g;
			float b;
			float a;
		};
	};
};
