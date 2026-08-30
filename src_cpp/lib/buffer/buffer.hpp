#pragma once

#include <cstdint>
#include <iomanip>
#include <iostream>
#include <vector>

#include "check.hpp"
#include <aquila.h>

namespace aquila
{

inline std::int64_t wrap_idx(std::int64_t i, std::size_t idxmax)
{
    check(i <= static_cast<std::int64_t>(idxmax));
    check(i >= -static_cast<std::int64_t>(idxmax));
    return i >= 0 ? i : i + idxmax + 1;
}

template <typename T>
class View;

template <typename T>
class MutableView;

template <typename T>
class Buffer
{
    std::vector<T> buffer;
    std::int64_t nx, ny;

public:
    Buffer(std::int64_t nx, std::int64_t ny) : buffer(nx * ny), nx(nx), ny(ny) {}

    Buffer(std::int64_t nx, std::int64_t ny, T value) : buffer(nx * ny), nx(nx), ny(ny)
    {
        std::fill(buffer.begin(), buffer.end(), value);
    }

    Buffer(const View<T> &view) : buffer(view.nx * view.ny), nx(view.nx), ny(view.ny)
    {
        for (std::int64_t ivec = 0; ivec < view.vecs(); ivec++)
        {
            const T *srcrow = view.vec(ivec);
            T *dstrow = vec(ivec);
            std::copy(srcrow, srcrow + view.nvec(), dstrow);
        }
    }

    Buffer(const Buffer &other) :
        buffer(other.nx * other.ny), nx(other.nx), ny(other.ny)
    {
        std::copy(other.buffer.begin(), other.buffer.end(), buffer.begin());
    }

    T &operator[](const std::int64_t i) noexcept
    {
        check(i >= 0);
        check(i < buffer.size());
        return buffer[i];
    }

    const T &operator[](const std::int64_t i) const noexcept
    {
        check(i >= 0);
        check(i < buffer.size());
        return buffer[i];
    }

    std::int64_t cols() const noexcept { return nx; }
    std::int64_t rows() const noexcept { return ny; }
    std::int64_t size() const noexcept { return nx * ny; }
    T *data() noexcept { return buffer.data(); }
    const T *data() const noexcept { return buffer.data(); }

    std::int64_t vecs() const noexcept { return ny; }
    std::int64_t nvec() const noexcept { return nx; }

    T *vec(std::int64_t ivec) noexcept
    {
        check(ivec >= 0);
        check(ivec < vecs());
        return buffer.data() + nvec() * ivec;
    }
    const T *vec(std::int64_t ivec) const noexcept
    {
        check(ivec >= 0);
        check(ivec < vecs());
        return buffer.data() + nvec() * ivec;
    }

    View<T> view() const { return {*this}; }
    View<T> view(
        std::int64_t ix_lo, std::int64_t ix_hi, std::int64_t iy_lo, std::int64_t iy_hi) const
    {
        return {*this, ix_lo, ix_hi, iy_lo, iy_hi};
    }

    View<T> mut_view() { return {*this}; }
    View<T> mut_view(
        std::int64_t ix_lo, std::int64_t ix_hi, std::int64_t iy_lo, std::int64_t iy_hi)
    {
        return {*this, ix_lo, ix_hi, iy_lo, iy_hi};
    }

    bool matches_shape(const View<T> &other) const noexcept
    {
        return (rows() == other.rows()) && (cols() == other.cols());
    }

    friend class View<T>;
    friend class MutableView<T>;
};

template <typename T>
class View
{
    const Buffer<T> *buf = nullptr;
    const std::int64_t off_x, off_y, nx, ny, buf_nx, buf_ny;

public:
    View(const Buffer<T> &buf) :
        buf(&buf), off_x(0), off_y(0), nx(buf.nx), ny(buf.ny), buf_nx(buf.nx),
        buf_ny(buf.ny)
    {
    }

    View(const Buffer<T> &buf,
        std::int64_t ix_lo,
        std::int64_t ix_hi,
        std::int64_t iy_lo,
        std::int64_t iy_hi) :
        buf(&buf), off_x(wrap_idx(ix_lo, buf.cols())),
        off_y(wrap_idx(iy_lo, buf.rows())), nx(wrap_idx(ix_hi, buf.cols()) - off_x),
        ny(wrap_idx(iy_hi, buf.rows()) - off_y), buf_nx(buf.nx), buf_ny(buf.ny)
    {
#ifndef NDEBUG
        std::cout << "requested slice " << ix_lo << ":" << ix_hi << " " << iy_lo << ":"
                  << iy_hi << " pointing at " << off_x << ":" << off_x + nx << " "
                  << off_y << ":" << off_y + ny << " (" << nx << "x" << ny << ")"
                  << " from frame " << buf.cols() << "x" << buf.rows() << std::endl;
#endif

        check(nx > 0);
        check(ny > 0);
    }

    View(const View<T> &view,
        std::int64_t ix_lo,
        std::int64_t ix_hi,
        std::int64_t iy_lo,
        std::int64_t iy_hi) :
        buf(view.buf), off_x(view.off_x + wrap_idx(ix_lo, view.cols())),
        off_y(view.off_y + wrap_idx(iy_lo, view.rows())),
        nx(wrap_idx(ix_hi, view.cols()) - wrap_idx(ix_lo, view.cols())),
        ny(wrap_idx(iy_hi, view.rows()) - wrap_idx(iy_lo, view.rows())),
        buf_nx(view.buf_nx), buf_ny(view.buf_ny)
    {
#ifndef NDEBUG
        std::cout << "requested slice " << ix_lo << ":" << ix_hi << " " << iy_lo << ":"
                  << iy_hi << " pointing at " << off_x << ":" << off_x + nx << " "
                  << off_y << ":" << off_y + ny << " (" << nx << "x" << ny << ")"
                  << " from frame " << view.buf_nx << "x" << view.buf_ny << std::endl;
#endif

        check(nx > 0);
        check(ny > 0);
    }
    View(const MutableView<T> &view) :
        buf(view.buf), off_x(view.off_x), off_y(view.off_y), nx(view.nx), ny(view.ny),
        buf_nx(view.buf_nx), buf_ny(view.buf_ny)
    {
    }

    const T &operator()(const std::int64_t ix, const std::int64_t iy) const noexcept
    {
        check(ix >= 0);
        check(ix < nx);
        check(iy >= 0);
        check(iy < ny);
        return buf->buffer[buf_nx * (iy + off_y) + (ix + off_x)];
    }

    std::int64_t cols() const noexcept { return nx; }
    std::int64_t rows() const noexcept { return ny; }
    std::int64_t size() const noexcept { return nx * ny; }
    bool is_contiguous() const noexcept { return (ny == buf_ny) && (off_y == 0); }

    std::int64_t vecs() const noexcept { return ny; }
    std::int64_t nvec() const noexcept { return nx; }
    const T *vec(std::int64_t ivec) const noexcept
    {
        check(ivec < vecs());
        return &(buf->buffer[off_x + buf_nx * (ivec + off_y)]);
    }
    const T *data() const noexcept
    {
        if (!is_contiguous())
            return nullptr;
        return vec(0);
    }

    View<T> view(
        std::int64_t ix_lo, std::int64_t ix_hi, std::int64_t iy_lo, std::int64_t iy_hi)
    {
        return {*this, ix_lo, ix_hi, iy_lo, iy_hi};
    }

    bool matches_shape(const View<T> &other) const noexcept
    {
        return (rows() == other.rows()) && (cols() == other.cols());
    }

    friend class Buffer<T>;
    template <typename U>
    friend std::ostream &operator<<(std::ostream &os, const View<U> &buf);
};

template <typename T>
class MutableView
{
    Buffer<T> *buf = nullptr;
    const std::int64_t off_x, off_y, nx, ny, buf_nx, buf_ny;

public:
    MutableView(Buffer<T> &buf) :
        buf(&buf), off_x(0), off_y(0), nx(buf.nx), ny(buf.ny), buf_nx(buf.nx),
        buf_ny(buf.ny)
    {
    }

    MutableView(Buffer<T> &buf,
        std::int64_t ix_lo,
        std::int64_t ix_hi,
        std::int64_t iy_lo,
        std::int64_t iy_hi) :
        buf(&buf), off_x(wrap_idx(ix_lo, buf.cols())),
        off_y(wrap_idx(iy_lo, buf.rows())), nx(wrap_idx(ix_hi, buf.cols()) - off_x),
        ny(wrap_idx(iy_hi, buf.rows()) - off_y), buf_nx(buf.nx), buf_ny(buf.ny)
    {
#ifndef NDEBUG
        std::cout << "requested slice " << ix_lo << ":" << ix_hi << " " << iy_lo << ":"
                  << iy_hi << " pointing at " << off_x << ":" << off_x + nx << " "
                  << off_y << ":" << off_y + ny << " (" << nx << "x" << ny << ")"
                  << " from frame " << buf.cols() << "x" << buf.rows() << std::endl;
#endif

        check(nx > 0);
        check(ny > 0);
    }

    MutableView(const MutableView<T> &view,
        std::int64_t ix_lo,
        std::int64_t ix_hi,
        std::int64_t iy_lo,
        std::int64_t iy_hi) :
        buf(view.buf), off_x(view.off_x + wrap_idx(ix_lo, view.cols())),
        off_y(view.off_y + wrap_idx(iy_lo, view.rows())),
        nx(wrap_idx(ix_hi, view.cols()) - wrap_idx(ix_lo, view.cols())),
        ny(wrap_idx(iy_hi, view.rows()) - wrap_idx(iy_lo, view.rows())),
        buf_nx(view.buf_nx), buf_ny(view.buf_ny)
    {
#ifndef NDEBUG
        std::cout << "requested slice " << ix_lo << ":" << ix_hi << " " << iy_lo << ":"
                  << iy_hi << " pointing at " << off_x << ":" << off_x + nx << " "
                  << off_y << ":" << off_y + ny << " (" << nx << "x" << ny << ")"
                  << " from frame " << view.buf_nx << "x" << view.buf_ny << std::endl;
#endif

        check(nx > 0);
        check(ny > 0);
    }

    T &operator()(const std::int64_t ix, const std::int64_t iy) noexcept
    {
        check(ix >= 0);
        check(ix < nx);
        check(iy >= 0);
        check(iy < ny);
        return buf->buffer[buf_nx * (iy + off_y) + (ix + off_x)];
    }

    const T &operator()(const std::int64_t ix, const std::int64_t iy) const noexcept
    {
        check(ix >= 0);
        check(ix < nx);
        check(iy >= 0);
        check(iy < ny);
        return buf->buffer[buf_nx * (iy + off_y) + (ix + off_x)];
    }

    std::int64_t cols() const noexcept { return nx; }
    std::int64_t rows() const noexcept { return ny; }
    std::int64_t size() const noexcept { return nx * ny; }
    bool is_contiguous() const noexcept { return (ny == buf_ny) && (off_y == 0); }

    std::int64_t vecs() const noexcept { return ny; }
    std::int64_t nvec() const noexcept { return nx; }
    T *vec(std::int64_t ivec) noexcept
    {
        check(ivec < vecs());
        return &(buf->buffer[off_x + buf_nx * (ivec + off_y)]);
    }
    const T *vec(std::int64_t ivec) const noexcept
    {
        check(ivec < vecs());
        return &(buf->buffer[off_x + buf_nx * (ivec + off_y)]);
    }
    T *data() noexcept
    {
        if (!is_contiguous())
            return nullptr;
        return vec(0);
    }
    const T *data() const noexcept
    {
        if (!is_contiguous())
            return nullptr;
        return vec(0);
    }

    MutableView<T> &operator=(T val)
    {
        for (std::int64_t irow = 0; irow < vecs(); irow++)
        {
            T *onerow = vec(irow);
            std::fill(onerow, onerow + nvec(), val);
        }
        return *this;
    }

    MutableView<T> &operator=(const View<T> &other)
    {
        check(rows() == other.rows());
        check(cols() == other.cols());
        for (std::int64_t irow = 0; irow < vecs(); irow++)
        {
            T *myrow = vec(irow);
            const T *otherrow = other.vec(irow);
            std::copy(otherrow, otherrow + nvec(), myrow);
        }
        return *this;
    }

    MutableView<T> view(
        std::int64_t ix_lo, std::int64_t ix_hi, std::int64_t iy_lo, std::int64_t iy_hi)
    {
        return {*this, ix_lo, ix_hi, iy_lo, iy_hi};
    }

    bool matches_shape(const View<T> &other) const noexcept
    {
        return (rows() == other.rows()) && (cols() == other.cols());
    }

    friend class Buffer<T>;
};

template <typename U>
std::ostream &operator<<(std::ostream &os, const View<U> &buf)
{
    for (std::int64_t nj = 0; nj < buf.rows(); nj++)
    {
        for (std::int64_t ni = 0; ni < buf.cols(); ni++)
        {
            std::cout << std::setw(9) << buf(ni, nj);
        }
        std::cout << std::endl;
    }
    return os;
}

inline buffer_descriptor_r32_t c_buf(Buffer<float> &buf)
{
    return {buf.data(), buf.cols(), buf.rows()};
}

inline const_buffer_descriptor_r32_t c_const_buf(const Buffer<float> &buf)
{
    return {buf.data(), buf.cols(), buf.rows()};
}

} // namespace aquila
