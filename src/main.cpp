#include <windows.h>
#include <projectedfslib.h>
#include <stdint.h>
#include <map>
#include <fstream>
#include <string>
#include <vector>

#include <zlib.h>

#include <memory>
#include <algorithm>

/*

concept Read
{
	size_t read(uint8_t * p, size_t size);
};

concept Blob
{
	typename offset_type;

	size_t read_at(offset_type offset, uint8_t * p, size_t size);
};

*/

struct readable
{
	virtual size_t read(uint8_t * p, size_t size) = 0;
	virtual void seek(uint64_t offset) = 0;
	virtual void skip(size_t count) = 0;

	void read_exact(uint8_t * p, size_t size)
	{
		while (size)
		{
			size_t r = this->read(p, size);
			if (r == 0)
				throw std::runtime_error("premature eof");
			p += r;
			size -= r;
		}
	}
};

template <typename B>
struct blob_reader
	: readable
{
	explicit blob_reader(B * blob, uint64_t offset = 0)
		: _blob(blob), _offset(offset)
	{
	}

	size_t read(uint8_t * p, size_t size) override
	{
		size_t r = _blob->read_at(_offset, p, size);
		_offset += r;
		return r;
	}

	void seek(uint64_t offset) override
	{
		_offset = offset;
	}

	void skip(size_t count) override
	{
		_offset += count;
	}

private:
	B * _blob;
	uint64_t _offset;
};

struct buf_readable
	: readable
{
	size_t read(uint8_t * p, size_t size) override
	{
		if (_first == _last)
			this->_read_buf();

		size = (std::min)(size, (size_t)(_last - _first));
		memcpy(p, _first, size);
		_first += size;
		return size;
	}

	uint8_t getch()
	{
		if (_first == _last)
			this->_read_buf();

		if (_first == _last)
			throw std::runtime_error("premature eof");

		return *_first++;
	}

	std::string read_line()
	{
		return this->read_until('\n');
	}



	void skip_until(uint8_t byte)
	{
		if (_first == _last)
			this->_read_buf();

		while (_first != _last)
		{
			auto it = std::find(_first, _last, byte);
			if (it != _last)
			{
				_first = it + 1;
				return;
			}

			this->_read_buf();
		}
	}

	std::string_view read_until(uint8_t byte, uint8_t * buf, size_t size)
	{
		if (_first == _last)
			this->_read_buf();

		uint8_t * p = buf;
		uint8_t * last = buf + size;

		while (_first != _last)
		{
			size_t rem = last - p;

			auto it = std::find(_first, _last, byte);
			if (it != _last)
			{
				size_t len = it - _first + 1;
				if (len > rem)
					len = rem;

				memcpy(p, _first, len);
				_first += len;
				return { (char const *)buf, len };
			}
			else
			{
				size_t len = _last - _first;
				if (len > rem)
				{
					memcpy(p, _first, rem);
					_first += rem;
					return { (char const *)buf, rem };
				}

				memcpy(p, _first, len);
				p += len;
				this->_read_buf();
			}
		}

		return { (char const *)buf, size_t(p - buf) };
	}

	std::string read_until(uint8_t byte)
	{
		std::string r;

		if (_first == _last)
			this->_read_buf();

		while (_first != _last)
		{
			auto it = std::find(_first, _last, byte);
			if (it != _last)
			{
				r.insert(r.end(), _first, it + 1);
				_first = it + 1;
				break;
			}

			r.insert(r.end(), _first, _last);
			this->_read_buf();
		}

		return r;
	}

	std::string read_all()
	{
		std::string r;

		if (_first == _last)
			this->_read_buf();

		while (_first != _last)
		{
			r.insert(r.end(), _first, _last);
			this->_read_buf();
		}

		return r;
	}

	template <typename T>
	T read_be()
	{
		uint8_t buf[sizeof(T)];
		this->read_exact(buf, sizeof buf);

		T r = 0;
		for (uint8_t b: buf)
			r = (r << 8) | b;
		return r;
	}

protected:
	virtual void _read_buf() = 0;

	uint8_t * _first = nullptr;
	uint8_t * _last = nullptr;
};

template <typename Readable, size_t BufferSize = 128>
struct buffered_reader final
	: buf_readable
{
	explicit buffered_reader(Readable * readable)
		: _readable(readable)
	{
	}

	void seek(uint64_t offset) override
	{
		_readable->seek(offset);
		_first = _last = nullptr;
	}

	void skip(size_t count) override
	{
		if (_last - _first >= count)
		{
			_first += count;
		}
		else
		{
			count -= _last - _first;
			_readable->skip(count);
			_first = _last;
		}
	}

private:
	void _read_buf() override
	{
		_first = _buf;
		_last = _buf + _readable->read(_buf, sizeof _buf);
	}

	Readable * _readable;
	uint8_t _buf[BufferSize];
};

template <typename T, typename... An>
std::shared_ptr<buffered_reader<T>> make_buffered(An &&... an)
{
	struct X
	{
		T readable;
		buffered_reader<T> br;

		X(An &&... an)
			: readable(std::forward<An>(an)...), br(&readable)
		{
		}
	};

	auto ptr = std::make_shared<X>(std::forward<An>(an)...);
	return { ptr, &ptr->br };
}

#include <filesystem>

struct win32_file final
	: readable
{
	using offset_type = uint64_t;
	size_t read_at(offset_type offset, uint8_t * p, size_t size)
	{
		DWORD dwRead;
		OVERLAPPED ov = {};
		ov.Offset = static_cast<uint32_t>(offset);
		ov.OffsetHigh = offset >> 32;
		if (!ReadFile(_handle, p, (DWORD)size, &dwRead, &ov))
			throw std::system_error(GetLastError(), std::system_category());

		return dwRead;
	}

	size_t read(uint8_t * p, size_t size) override
	{
		DWORD dwRead;
		if (!ReadFile(_handle, p, (DWORD)size, &dwRead, nullptr))
			throw std::system_error(GetLastError(), std::system_category());
		return dwRead;
	}

	void seek(uint64_t offset) override
	{
		LARGE_INTEGER off;
		off.QuadPart = offset;
		if (!SetFilePointerEx(_handle, off, nullptr, FILE_BEGIN))
			throw std::system_error(GetLastError(), std::system_category());
	}

	void skip(size_t count) override
	{
		LARGE_INTEGER off;
		off.QuadPart = count;
		if (!SetFilePointerEx(_handle, off, nullptr, FILE_CURRENT))
			throw std::system_error(GetLastError(), std::system_category());
	}

	win32_file()
	{
	}

	explicit win32_file(std::filesystem::path const & path)
	{
		this->open(path);
	}

	win32_file(win32_file && o)
		: _handle(o._handle)
	{
		o._handle = INVALID_HANDLE_VALUE;
	}

	~win32_file()
	{
		this->close();
	}

	win32_file & operator=(win32_file o)
	{
		std::swap(_handle, o._handle);
		return *this;
	}

	std::error_code open(std::filesystem::path const & path) noexcept
	{
		HANDLE h = CreateFileW(path.c_str(), GENERIC_READ, FILE_SHARE_READ, nullptr, OPEN_EXISTING, 0, nullptr);
		if (h == INVALID_HANDLE_VALUE)
			return std::error_code(GetLastError(), std::system_category());

		this->close();
		_handle = h;
		return {};
	}

	void close()
	{
		if (_handle != INVALID_HANDLE_VALUE)
		{
			CloseHandle(_handle);
			_handle = INVALID_HANDLE_VALUE;
		}
	}

	void * map()
	{
		HANDLE hSection = CreateFileMappingW(_handle, nullptr, PAGE_READONLY, 0, 0, nullptr);
		if (!hSection)
			throw std::system_error(GetLastError(), std::system_category());

		void * p = MapViewOfFile(hSection, FILE_MAP_READ, 0, 0, 0);
		if (!p)
		{
			DWORD err = GetLastError();
			CloseHandle(hSection);
			throw std::system_error(err, std::system_category());
		}

		CloseHandle(hSection);
		return p;
	}

private:
	HANDLE _handle = INVALID_HANDLE_VALUE;
};

struct _zlib_category final
	: std::error_category
{
	char const * name() const noexcept override { return "zlib"; }
	std::string message(int ec) const override { return {}; }
};

_zlib_category zlib_category;

template <typename R, size_t BufferSize>
struct inflater final
	: readable
{
	explicit inflater(R & r)
		: _r(r)
	{
		_str.opaque = this;
		_str.zalloc = nullptr;
		_str.zfree = nullptr;

		this->_read_buf();

		int ec = inflateInit(&_str);
		if (ec != Z_OK)
			throw std::system_error(ec, zlib_category);
	}

	~inflater()
	{
		inflateEnd(&_str);
	}

	size_t read(uint8_t * p, size_t size) override
	{
		_str.next_out = p;
		_str.avail_out = (uInt)size;

		while (_str.avail_out)
		{
			if (!_str.avail_in)
				this->_read_buf();

			int ec = inflate(&_str, Z_NO_FLUSH);
			if (ec == Z_STREAM_END)
				break;

			if (ec != Z_OK)
				throw std::system_error(ec, zlib_category);
		}

		return size - _str.avail_out;
	}

	void seek(uint64_t offset) override
	{
		throw std::runtime_error("unseekable");
	}

	void skip(size_t count) override
	{
		throw std::runtime_error("unseekable");
	}

private:
	void _read_buf()
	{
		_str.avail_in = (uInt)_r.read(_buf, sizeof _buf);
		_str.next_in = _buf;
	}

	R & _r;
	uint8_t _buf[BufferSize];
	z_stream _str;
};

struct git_object_stream
	: std::streambuf
{
};

static std::string inflate(uint8_t const * bytes, size_t size)
{
	z_stream str;
	str.next_in = (uint8_t *)bytes;
	str.avail_in = (uInt)size;
	str.zalloc = nullptr;
	str.zfree = nullptr;
	str.opaque = nullptr;
	if (inflateInit(&str) != Z_OK)
		throw std::runtime_error("can't inflate");

	std::string r;

	for (;;)
	{
		r.resize((str.total_out & ~0xfff) + 0x1000);
		str.next_out = (uint8_t *)(&r[0] + str.total_out);
		str.avail_out = (uInt)r.size();

		auto ec = inflate(&str, Z_NO_FLUSH);
		if (ec == Z_STREAM_END)
			break;

		if (ec != Z_OK)
			throw std::runtime_error("failed to inflate");
	}

	r.resize(str.total_out);
	inflateEnd(&str);

	return r;
}

static std::string to_hex_string(uint8_t const * data, size_t length)
{
	std::string r;
	r.reserve(length * 2);

	for (size_t i = 0; i != length; ++i)
	{
		static constexpr char digits[] = "0123456789abcdef";
		uint8_t d = data[i];
		r.push_back(digits[d >> 4]);
		r.push_back(digits[d & 0xf]);
	}

	return r;
}

static std::string to_hex_string(uint8_t byte)
{
	return to_hex_string(&byte, 1);
}

static constexpr uint8_t _digit_to_byte(char d)
{
	if ('0' <= d && d <= '9')
		return d - '0';
	if ('a' <= d && d <= 'f')
		return d - 'a' + 10;
	if ('A' <= d && d <= 'F')
		return d - 'A' + 10;

	throw;
}

static constexpr uint8_t _hex_to_byte(char const str[2])
{
	return (_digit_to_byte(str[0]) << 4) | _digit_to_byte(str[1]);
}

struct git_object_id
{
	git_object_id()
	{
	}

	constexpr git_object_id(std::string_view hex_string)
		: git_object_id(hex_string.data())
	{
	}

	constexpr git_object_id(char const * hex_string)
		: _bytes{
		_hex_to_byte(hex_string + 0),
		_hex_to_byte(hex_string + 2),
		_hex_to_byte(hex_string + 4),
		_hex_to_byte(hex_string + 6),
		_hex_to_byte(hex_string + 8),
		_hex_to_byte(hex_string + 10),
		_hex_to_byte(hex_string + 12),
		_hex_to_byte(hex_string + 14),
		_hex_to_byte(hex_string + 16),
		_hex_to_byte(hex_string + 18),
		_hex_to_byte(hex_string + 20),
		_hex_to_byte(hex_string + 22),
		_hex_to_byte(hex_string + 24),
		_hex_to_byte(hex_string + 26),
		_hex_to_byte(hex_string + 28),
		_hex_to_byte(hex_string + 30),
		_hex_to_byte(hex_string + 32),
		_hex_to_byte(hex_string + 34),
		_hex_to_byte(hex_string + 36),
		_hex_to_byte(hex_string + 38),
	}
	{
	}

	constexpr uint8_t operator[](size_t idx) const
	{
		return _bytes[idx];
	}

	constexpr uint8_t const * bytes() const
	{
		return _bytes;
	}

	constexpr uint8_t * bytes()
	{
		return _bytes;
	}

	std::string to_hex() const
	{
		return to_hex_string(_bytes, sizeof _bytes);
	}

private:
	uint8_t _bytes[20];
};

enum class git_object_type
{
	invalid,
	unknown,
	commit,
	tree,
	blob,
	tag,
};

struct git_object
{
	git_object_type type;
	uint32_t size;
	std::shared_ptr<buf_readable> contents;
};

std::string_view read_line(std::string_view & data)
{
	std::string_view r;

	size_t pos = data.find('\n');
	if (pos == std::string_view::npos)
	{
		r = data;
		data = {};
	}
	else
	{
		r = data.substr(0, pos);
		data.remove_prefix(pos + 1);
	}

	return r;
}

struct git_object_reader
{
	explicit git_object_reader(std::filesystem::path path)
		: _path(std::move(path))
	{
	}

	std::shared_ptr<buf_readable> get_tree(git_object_id const & oid)
	{
		auto obj = this->_get_object_stream(oid);

		if (obj.type == git_object_type::commit)
		{
			for (;;)
			{
				uint8_t buf[64];

				std::string_view line = obj.contents->read_until('\n', buf, sizeof buf);
				if (line.empty())
					throw std::runtime_error("invalid commit object");

				if (line.back() != '\n')
				{
					obj.contents->skip_until('\n');
					continue;
				}

				if (line._Starts_with("tree ") && line.size() >= 45)
				{
					obj = this->_get_object_stream(git_object_id(line.data() + 5));
					break;
				}
			}
		}

		if (obj.type != git_object_type::tree)
			throw std::runtime_error("invalid object type (expected tree)");

		return obj.contents;
	}

	git_object get_blob(git_object_id const & oid)
	{
		auto obj = this->_get_object_stream(oid);
		if (obj.type != git_object_type::blob && obj.type != git_object_type::unknown)
			throw std::runtime_error("not a blob object");
		return obj;
	}


private:
	git_object_id _parse_commit_tree(std::string_view commit_data)
	{
		while (!commit_data.empty())
		{
			auto line = read_line(commit_data);
			if (line._Starts_with("tree "))
			{
				return git_object_id(line.substr(5));
			}
		}

		return {};
	}

	git_object _get_object_stream(git_object_id const & oid)
	{
		auto loose_object_path = _path / "objects" / to_hex_string(oid[0]) / to_hex_string(oid.bytes() + 1, 19);

		win32_file loose_obj;
		if (!loose_obj.open(loose_object_path))
		{
			struct loose_object_reader
			{
				win32_file file;
				inflater<win32_file, 128> inf;
				buffered_reader<inflater<win32_file, 128>, 128> br;

				loose_object_reader(win32_file f)
					: file(std::move(f)), inf(file), br(&inf)
				{
				}
			};

			git_object obj;

			auto r = std::make_shared<loose_object_reader>(std::move(loose_obj));

			uint8_t buf[16];

			{
				auto type = r->br.read_until(' ', buf, sizeof buf);
				if (type == "commit ")
					obj.type = git_object_type::commit;
				else if (type == "tree ")
					obj.type = git_object_type::tree;
				else if (type == "blob ")
					obj.type = git_object_type::blob;
				else if (type == "tag ")
					obj.type = git_object_type::tag;
				else
					throw std::runtime_error("invalid object type");
			}

			{
				auto size_str = r->br.read_until('\0', buf, sizeof buf);
				if (size_str.empty() || size_str.back() != '\0')
					throw std::runtime_error("invalid object format");

				size_str.remove_suffix(1);

				uint64_t obj_size = 0;
				for (char ch: size_str)
				{
					if ('0' <= ch && ch <= '9')
					{
						if (obj_size == 0 && ch == '0')
							throw std::runtime_error("invalid object format");

						obj_size = obj_size * 10 + (ch - '0');
					}
					else
					{
						throw std::runtime_error("invalid object format");
					}
				}

				obj.size = obj_size;
			}

			obj.contents = { r, &r->br };
			return obj;
		}

		this->_read_packs();

		auto rng = std::equal_range(_packed_objects.begin(), _packed_objects.end(), oid, [](git_object_id const & lhs, git_object_id const & rhs) {
			return memcmp(&lhs, &rhs, sizeof lhs) < 0;
		});

		if (rng.first == rng.second)
			return {};

		uint64_t offset = rng.first->offset;
		blob_reader<win32_file> blr(&*_pack_files[rng.first->pack_file_idx], offset);
		buffered_reader<blob_reader<win32_file>> br(&blr);

		git_object obj;

		uint8_t b = br.getch();
		++offset;

		uint8_t type_val = (b >> 4) & 7;

		static constexpr git_object_type types[] = {
			git_object_type::invalid,
			git_object_type::commit,
			git_object_type::tree,
			git_object_type::blob,
			git_object_type::tag,
			git_object_type::invalid,
			git_object_type::unknown,
			git_object_type::unknown,
		};

		obj.type = types[type_val];

		size_t size = b & 0xf;
		int shift = 4;
		while (b & 0x80)
		{
			b = br.getch();
			++offset;
			size |= (b & 0x7f) << shift;
			shift += 7;
		}

		auto get_delta_size = [&]() {
			uint64_t r = 0;

			while (size)
			{
				uint8_t b = br.getch();
				--size;
				if (b & 0x80)
				{
					static constexpr uint8_t popcnt[16] = { 0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4 };

					size_t hdr_size = popcnt[b & 0xf] | popcnt[(b >> 4) & 0x7];
					if (size < hdr_size)
						throw std::runtime_error("invalid object");

					br.skip(popcnt[b & 0xf]);

					uint32_t chunk_size = 0;
					if (b & 0x10)
						chunk_size |= br.getch();
					if (b & 0x20)
						chunk_size |= br.getch() << 8;
					if (b & 0x40)
						chunk_size |= br.getch() << 16;

					if (chunk_size == 0)
						chunk_size = 0x10000;

					if (size < chunk_size)
						throw std::runtime_error("invalid object");

					size -= chunk_size;
					r += chunk_size;
				}
				else if (b == 0)
				{
					throw std::runtime_error("invalid object");
				}
				else
				{
					if (size < b)
						throw std::runtime_error("invalid object");

					r += b;
				}
			}

			return r;
		};

		switch (type_val)
		{
		case 6:
			// ofs_delta
			if (size == 0)
				throw std::runtime_error("invalid object");

			while (br.getch() & 0x80)
			{
			}

			obj.size = get_delta_size();
			break;

		case 7:
			// ref_delta
			br.skip(20);
			obj.size = get_delta_size();
			break;

		default:
			obj.size = size;
		}

#if 0
		struct packed_object_reader
		{
			std::shared_ptr<win32_file> pack_file;
			blob_reader<win32_file> blr;
			inflater<blob_reader<win32_file>, 128> inf;
			buffered_reader<inflater<blob_reader<win32_file>, 128>> br;

			packed_object_reader(std::shared_ptr<win32_file> pf, uint64_t offset)
				: pack_file(std::move(pf)), blr(&*pack_file, offset), inf(blr), br(&inf)
			{
			}
		};

		auto contents = std::make_shared<packed_object_reader>(_pack_files[rng.first->pack_file_idx], offset);
		obj.contents = { contents, &contents->br };
#endif

		return obj;
	}

	void _read_packs()
	{
		if (!_packed_objects.empty())
			return;

		auto pack_file_mask = _path / "objects" / "pack" / "*.pack";

		WIN32_FIND_DATAW wfd;
		HANDLE hFind = FindFirstFileW(pack_file_mask.c_str(), &wfd);
		if (hFind == nullptr)
		{
			DWORD err = GetLastError();
			if (err != ERROR_NO_MORE_FILES)
				throw std::system_error(err, std::system_category());
			return;
		}

		for (;;)
		{
			if ((wfd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == 0)
				this->_load_pack(_path / "objects" / "pack" / wfd.cFileName);

			if (!FindNextFileW(hFind, &wfd))
			{
				DWORD err = GetLastError();
				FindClose(hFind);
				if (err != ERROR_NO_MORE_FILES)
					throw std::system_error(err, std::system_category());
				break;
			}
		}

		std::sort(_packed_objects.begin(), _packed_objects.end(), [](object_ref const & lhs, object_ref const & rhs) {
			return memcmp(&lhs.oid, &rhs.oid, sizeof lhs.oid) < 0;
		});
	}

	void _load_pack(std::filesystem::path const & path)
	{
		auto pack_file = std::make_shared<win32_file>(path);
		size_t pack_file_idx = _pack_files.size();
		_pack_files.push_back(pack_file);

		auto idx_path = path;
		idx_path.replace_extension(".idx");

		win32_file idx(idx_path);
		buffered_reader<win32_file> br(&idx);

		if (br.read_be<uint32_t>() != 0xff744f63)
			throw std::runtime_error("invalid idx file singature");

		if (br.read_be<uint32_t>() != 2)
			throw std::runtime_error("invalid idx file version");

		br.seek(8 + 4 * 255);

		uint32_t object_count = br.read_be<uint32_t>();

		size_t first_po_idx = _packed_objects.size();

		for (size_t i = 0; i != object_count; ++i)
		{
			object_ref oref = {};
			oref.pack_file_idx = pack_file_idx;
			br.read_exact(oref.oid.bytes(), sizeof oref.oid);
			_packed_objects.push_back(oref);
		}

		br.seek(8 + 4 * 256 + 24 * object_count);
		for (size_t i = 0; i != object_count; ++i)
		{
			_packed_objects[i + first_po_idx].offset = br.read_be<uint32_t>();
		}
	}

	struct object_ref
	{
		git_object_id oid;
		size_t pack_file_idx;
		uint64_t offset;

		operator git_object_id const &() const
		{
			return oid;
		}
	};

	std::vector<std::shared_ptr<win32_file>> _pack_files;
	std::vector<object_ref> _packed_objects;

	std::filesystem::path _path;
};

git_object_reader git("c:\\devel\\checkouts\\bazel\\.git");

static void to_version_info(PRJ_PLACEHOLDER_VERSION_INFO & vi, git_object_id const & obj_id)
{
	memcpy(vi.ContentID, &obj_id, sizeof obj_id);
}

static git_object_id to_oid(PRJ_PLACEHOLDER_VERSION_INFO const & vi)
{
	git_object_id r;
	memcpy(&r, vi.ContentID, sizeof r);
	return r;
}

struct memcmp_less
{
	template <typename T>
	bool operator()(T const & lhs, T const & rhs) const
	{
		return memcmp(&lhs, &rhs, sizeof lhs);
	}
};

struct open_enum
{
	std::shared_ptr<buf_readable> tree_obj;
};

std::map<GUID, open_enum, memcmp_less> _enum_sessions;

static uint8_t const filedata[] = "ahoj";

HRESULT CALLBACK start_directory_enum(
	const PRJ_CALLBACK_DATA* callbackData,
	const GUID* enumerationId)
{
	_enum_sessions[*enumerationId].tree_obj = git.get_tree(to_oid(*callbackData->VersionInfo));
	return 0;
}

HRESULT CALLBACK end_directory_enum(
	const PRJ_CALLBACK_DATA* callbackData,
	const GUID* enumerationId)
{
	_enum_sessions.erase(*enumerationId);
	return 0;
}

HRESULT CALLBACK get_directory_enum(
	const PRJ_CALLBACK_DATA* callbackData,
	const GUID* enumerationId,
	PCWSTR searchExpression,
	PRJ_DIR_ENTRY_BUFFER_HANDLE dirEntryBufferHandle
)
{
	open_enum & oe = _enum_sessions[*enumerationId];

	for (;;)
	{
		std::string flags_str = oe.tree_obj->read_until(' ');
		if (flags_str.empty())
			break;

		int flags = std::stoi(flags_str, nullptr, 8);

		std::string name = oe.tree_obj->read_until('\0');
		git_object_id oid;
		oe.tree_obj->read_exact(oid.bytes(), 20);

		std::filesystem::path p(name.c_str());
		PRJ_FILE_BASIC_INFO bi = {};

		if ((flags & 040000) == 0)
		{
			git_object blob = git.get_blob(oid);
			bi.FileSize = blob.size;
		}
		else
		{
			bi.IsDirectory = true;
		}

		HRESULT hr = PrjFillDirEntryBuffer(p.c_str(), &bi, dirEntryBufferHandle);
		if (hr == HRESULT_FROM_WIN32(ERROR_INSUFFICIENT_BUFFER))
			break;
	}

	return S_OK;
}

HRESULT CALLBACK get_placeholder_info(const PRJ_CALLBACK_DATA* callbackData)
{
	if (PrjFileNameMatch(L"ahoj.txt", callbackData->FilePathName))
	{
		PRJ_PLACEHOLDER_INFO pi = {};
		pi.FileBasicInfo.FileSize = sizeof filedata;
		return PrjWritePlaceholderInfo(callbackData->NamespaceVirtualizationContext, L"ahoj.txt", &pi, sizeof pi);
	}

	return 0;
}

HRESULT CALLBACK get_file_data(
	const PRJ_CALLBACK_DATA* callbackData,
	UINT64 byteOffset,
	UINT32 length
)
{
	return PrjWriteFileData(callbackData->NamespaceVirtualizationContext, &callbackData->DataStreamId, (void *)(filedata + byteOffset), byteOffset, length);
}

int main()
{



#if 0
	union {
		REPARSE_GUID_DATA_BUFFER _buf;
		uint8_t data[0x1000];
	} buf;

	HANDLE h = CreateFileW(L"c:\\devel\\checkouts\\_test", GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, nullptr, OPEN_EXISTING, FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_BACKUP_SEMANTICS, nullptr);
	
	DWORD dwReturned;
	DeviceIoControl(h, FSCTL_GET_REPARSE_POINT, nullptr, 0, &buf, sizeof buf, &dwReturned, nullptr);

	buf._buf.ReparseDataLength = 0;
	DeviceIoControl(h, FSCTL_DELETE_REPARSE_POINT, &buf, REPARSE_GUID_DATA_BUFFER_HEADER_SIZE, nullptr, 0, &dwReturned, nullptr);
#endif

	// {711FBD5E-62FC-4D58-9E74-B79734F80295}
	static const GUID instance_guid =
	{ 0x711fbd5e, 0x62fc, 0x4d58, { 0x9e, 0x74, 0xb7, 0x97, 0x34, 0xf8, 0x2, 0x95 } };

	PRJ_PLACEHOLDER_VERSION_INFO root_vi = {};
	to_version_info(root_vi, "f8ef9c80244474b924c7f4873d1a2bc5390cfca2");

	HRESULT hr = PrjMarkDirectoryAsPlaceholder(L"c:\\devel\\checkouts\\_test", nullptr, &root_vi, &instance_guid);

	PRJ_CALLBACKS cbs = {};
	cbs.StartDirectoryEnumerationCallback = &start_directory_enum;
	cbs.EndDirectoryEnumerationCallback = &end_directory_enum;
	cbs.GetDirectoryEnumerationCallback = &get_directory_enum;
	cbs.GetPlaceholderInfoCallback = &get_placeholder_info;
	cbs.GetFileDataCallback = &get_file_data;

	PRJ_NAMESPACE_VIRTUALIZATION_CONTEXT ctx;
	hr = PrjStartVirtualizing(L"c:\\devel\\checkouts\\_test", &cbs, nullptr, nullptr, &ctx);

	for (;;)
		Sleep(1000);

	return 0;
}
