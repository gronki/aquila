#pragma once

#include <vector>
#include <set>
#include <check.h>
#include <concepts>
#include <iostream>
#include <iomanip>
#include <stacktrace>

#include "check.h"

using Index = int64_t;
inline Index wrap_idx(Index i, std::size_t idxmax)
{
    check(i <= static_cast<Index>(idxmax));
    check(i >= -static_cast<Index>(idxmax));
    return i >= 0 ? i : i + idxmax + 1;
}

template <typename T>
class View;

template <typename T>
class Buffer
{
    std::vector<T> buffer;
    const Index nx, ny;

public:
    Buffer(Index nx, Index ny) : nx(nx), ny(ny)
    {
        buffer.resize(nx * ny);
    }

    Buffer(Index nx, Index ny, T value) : nx(nx), ny(ny)
    {
        buffer.resize(nx * ny);
        std::fill(buffer.begin(), buffer.end(), value);
    }

    Buffer(const View<T> &view) : nx(view.nx), ny(view.ny)
    {
        buffer.resize(nx * ny);
        for (Index ivec = 0; ivec < view.vecs(); ivec++)
        {
            const T *srcrow = view.vec(ivec);
            T *dstrow = vec(ivec);
            std::copy(srcrow, srcrow + view.nvec(), dstrow);
        }
    }

    T &operator[](const Index i) noexcept
    {
        check(i >= 0);
        check(i < buffer.size());
        return buffer[i];
    }

    const T &operator[](const Index i) const noexcept
    {
        check(i >= 0);
        check(i < buffer.size());
        return buffer[i];
    }

    Index cols() const noexcept { return nx; }
    Index rows() const noexcept { return ny; }
    Index size() const noexcept { return nx * ny; }
    T *data() noexcept { return buffer.data(); }
    const T *data() const noexcept { return buffer.data(); }

    std::size_t vecs() const noexcept { return ny; }
    std::size_t nvec() const noexcept { return nx; }

    T *vec(Index ivec) noexcept
    {
        check(ivec >= 0);
        check(ivec < vecs());
        return buffer.data() + nvec() * ivec;
    }
    const T *vec(Index ivec) const noexcept
    {
        check(ivec >= 0);
        check(ivec < vecs());
        return buffer.data() + nvec() * ivec;
    }

    View<T> view() { return {*this}; }
    View<T> view(Index ix_lo, Index ix_hi, Index iy_lo, Index iy_hi)
    {
        return {*this, ix_lo, ix_hi, iy_lo, iy_hi};
    }

    friend class View<T>;
};

template <typename T>
Buffer<T> operator+(const View<T> &one, const View<T> &other);

template <typename T>
class View
{
    Buffer<T> *buf = nullptr;
    const Index off_x, off_y, nx, ny, buf_nx, buf_ny;

public:
    View(Buffer<T> &buf)
        : buf(&buf), off_x(0), off_y(0), nx(buf.nx),
          buf_nx(buf.nx), ny(buf.ny), buf_ny(buf.ny) {}

    View(Buffer<T> &buf, Index ix_lo, Index ix_hi, Index iy_lo, Index iy_hi)
        : buf(&buf), buf_nx(buf.nx), buf_ny(buf.ny),
          off_x(wrap_idx(ix_lo, buf.cols())),
          off_y(wrap_idx(iy_lo, buf.rows())),
          nx(wrap_idx(ix_hi, buf.cols()) - off_x),
          ny(wrap_idx(iy_hi, buf.rows()) - off_y)
    {
#ifndef NDEBUG
        std::cout << "requested slice "
                  << ix_lo << ":" << ix_hi << " " << iy_lo << ":" << iy_hi
                  << " pointing at "
                  << off_x << ":" << off_x + nx << " "
                  << off_y << ":" << off_y + ny << " (" << nx << "x" << ny << ")"
                  << " from frame " << buf.cols() << "x" << buf.rows() << std::endl;
#endif

        check(nx > 0);
        check(ny > 0);
    }

    View(View<T> &view, Index ix_lo, Index ix_hi, Index iy_lo, Index iy_hi)
        : buf(view.buf), buf_nx(view.buf_nx), buf_ny(view.buf_ny),
          off_x(view.off_x + wrap_idx(ix_lo, view.cols())),
          off_y(view.off_y + wrap_idx(iy_lo, view.rows())),
          nx(wrap_idx(ix_hi, view.cols()) - wrap_idx(ix_lo, view.cols())),
          ny(wrap_idx(iy_hi, view.rows()) - wrap_idx(iy_lo, view.rows()))
    {
#ifndef NDEBUG
        std::cout << "requested slice "
                  << ix_lo << ":" << ix_hi << " " << iy_lo << ":" << iy_hi
                  << " pointing at "
                  << off_x << ":" << off_x + nx << " " << off_y << ":" << off_y + ny
                  << " (" << nx << "x" << ny << ")"
                  << " from frame " << view.buf_nx << "x" << view.buf_ny << std::endl;
#endif

        check(nx > 0);
        check(ny > 0);
    }

    T &operator()(const Index ix, const Index iy) noexcept
    {
        check(ix < nx);
        check(iy < ny);
        return buf->buffer[buf_ny * (ix + off_x) + (iy + off_y)];
    }

    const T &operator()(const Index ix, const Index iy) const noexcept
    {
        check(ix < nx);
        check(iy < ny);
        return buf->buffer[buf_ny * (ix + off_x) + (iy + off_y)];
    }

    Index cols() const noexcept { return nx; }
    Index rows() const noexcept { return ny; }
    Index size() const noexcept { return nx * ny; }
    bool is_contiguous() const noexcept
    {
        return (ny == buf_ny) && (off_y == 0);
    }

    std::size_t vecs() const noexcept { return ny; }
    std::size_t nvec() const noexcept { return nx; }
    T *vec(Index ivec) noexcept
    {
        check(ivec < vecs());
        return &(buf->buffer[off_y + buf_ny * (ivec + off_x)]);
    }
    const T *vec(Index ivec) const noexcept
    {
        check(ivec < vecs());
        return &(buf->buffer[off_y + buf_ny * (ivec + off_x)]);
    }

    View<T> &operator=(T val)
    {
        for (Index irow = 0; irow < vecs(); irow++)
        {
            T *onerow = vec(irow);
            std::fill(onerow, onerow + nvec(), val);
        }
        return *this;
    }

    View<T> &operator=(const View<T> &other)
    {
        check(rows() == other.rows());
        check(cols() == other.cols());
        for (Index irow = 0; irow < vecs(); irow++)
        {
            T *myrow = vec(irow);
            const T *otherrow = other.vec(irow);
            std::copy(otherrow, otherrow + nvec(), myrow);
        }
        return *this;
    }

    View<T> view(Index ix_lo, Index ix_hi, Index iy_lo, Index iy_hi)
    {
        return {*this, ix_lo, ix_hi, iy_lo, iy_hi};
    }

    friend class Buffer<T>;
    template <typename U>
    friend Buffer<U> operator+(const View<U> &one, const View<U> &other);
    template <typename U>
    friend Buffer<U> operator+(const View<U> &one, U other);
    template <typename U>
    friend Buffer<U> operator+(U other, const View<U> &one);
    template <typename U>
    friend std::ostream &operator<<(std::ostream &os, const View<U> &buf);
};

extern "C"
{
    void add_double_vec_vec(const double *x, const double *y, double *z, std::size_t n);
    void add_double_vec_const(const double *x, double y, double *z, std::size_t n);
    void add_float_vec_vec(const float *x, const float *y, float *z, std::size_t n);
    void add_float_vec_const(const float *x, float y, float *z, std::size_t n);
}

void add_ftn(const double *x, const double *y, double *z, std::size_t n) { add_double_vec_vec(x, y, z, n); }
void add_ftn(const double *x, double y, double *z, std::size_t n) { add_double_vec_const(x, y, z, n); }
void add_ftn(const float *x, const float *y, float *z, std::size_t n) { add_float_vec_vec(x, y, z, n); }
void add_ftn(const float *x, float y, float *z, std::size_t n) { add_float_vec_const(x, y, z, n); }

template <typename T>
inline Buffer<T> operator+(const View<T> &one, const View<T> &other)
{
    Index nx = one.nx;
    Index ny = one.ny;
    check(nx == other.nx);
    check(ny == other.ny);
    Buffer<T> result(nx, ny);

    for (Index ivec = 0; ivec < one.vecs(); ivec++)
    {
        add_ftn(one.vec(ivec), other.vec(ivec), result.vec(ivec), one.nvec());
    }
    return result;
}

template <typename T>
inline Buffer<T> operator+(const View<T> &one, T other)
{
    Index nx = one.nx;
    Index ny = one.ny;
    Buffer<T> result(nx, ny);

    for (Index ivec = 0; ivec < one.vecs(); ivec++)
    {
        add_ftn(one.vec(ivec), other, result.vec(ivec), one.nvec());
    }
    return result;
}

template <typename T>
inline Buffer<T> operator+(T other, const View<T> &one) { return operator+ <T>(one, other); }

template <typename U>
std::ostream &operator<<(std::ostream &os, const View<U> &buf)
{
    for (Index nj = 0; nj < buf.rows(); nj++)
    {
        for (Index ni = 0; ni < buf.cols(); ni++)
        {
            std::cout << std::setw(9) << buf(ni, nj);
        }
        std::cout << std::endl;
    }
    return os;
}
