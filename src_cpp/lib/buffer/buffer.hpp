#pragma once

#include <vector>
#include <set>
#include <check.hpp>
#include <types.hpp>
#include <concepts>
#include <iostream>
#include <iomanip>
#include <stacktrace>
#include <functional>

namespace aquila {

inline Int wrap_idx(Int i, std::size_t idxmax)
{
    check(i <= static_cast<Int>(idxmax));
    check(i >= -static_cast<Int>(idxmax));
    return i >= 0 ? i : i + idxmax + 1;
}

template <typename T>
class View;

template <typename T>
class Buffer
{
    std::vector<T> buffer;
    const Int nx, ny;

public:
    Buffer(Int nx, Int ny) : nx(nx), ny(ny)
    {
        buffer.resize(nx * ny);
    }

    Buffer(Int nx, Int ny, T value) : nx(nx), ny(ny)
    {
        buffer.resize(nx * ny);
        std::fill(buffer.begin(), buffer.end(), value);
    }

    Buffer(const View<T> &view) : nx(view.nx), ny(view.ny)
    {
        buffer.resize(nx * ny);
        for (Int ivec = 0; ivec < view.vecs(); ivec++)
        {
            const T *srcrow = view.vec(ivec);
            T *dstrow = vec(ivec);
            std::copy(srcrow, srcrow + view.nvec(), dstrow);
        }
    }

    T &operator[](const Int i) noexcept
    {
        check(i >= 0);
        check(i < buffer.size());
        return buffer[i];
    }

    const T &operator[](const Int i) const noexcept
    {
        check(i >= 0);
        check(i < buffer.size());
        return buffer[i];
    }

    Int cols() const noexcept { return nx; }
    Int rows() const noexcept { return ny; }
    Int size() const noexcept { return nx * ny; }
    T *data() noexcept { return buffer.data(); }
    const T *data() const noexcept { return buffer.data(); }

    Int vecs() const noexcept { return nx; }
    Int nvec() const noexcept { return ny; }

    T *vec(Int ivec) noexcept
    {
        check(ivec >= 0);
        check(ivec < vecs());
        return buffer.data() + nvec() * ivec;
    }
    const T *vec(Int ivec) const noexcept
    {
        check(ivec >= 0);
        check(ivec < vecs());
        return buffer.data() + nvec() * ivec;
    }

    View<T> view() { return {*this}; }
    View<T> view(Int ix_lo, Int ix_hi, Int iy_lo, Int iy_hi)
    {
        return {*this, ix_lo, ix_hi, iy_lo, iy_hi};
    }

    friend class View<T>;
};

template <typename T>
class View
{
    Buffer<T> *buf = nullptr;
    const Int off_x, off_y, nx, ny, buf_nx, buf_ny;

public:
    View(Buffer<T> &buf)
        : buf(&buf), off_x(0), off_y(0), nx(buf.nx),
          buf_nx(buf.nx), ny(buf.ny), buf_ny(buf.ny) {}

    View(Buffer<T> &buf, Int ix_lo, Int ix_hi, Int iy_lo, Int iy_hi)
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

    View(View<T> &view, Int ix_lo, Int ix_hi, Int iy_lo, Int iy_hi)
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

    T &operator()(const Int ix, const Int iy) noexcept
    {
        check(ix >= 0);
        check(ix < nx);
        check(iy >= 0);
        check(iy < ny);
        return buf->buffer[buf_ny * (ix + off_x) + (iy + off_y)];
    }

    const T &operator()(const Int ix, const Int iy) const noexcept
    {
        check(ix >= 0);
        check(ix < nx);
        check(iy >= 0);
        check(iy < ny);
        return buf->buffer[buf_ny * (ix + off_x) + (iy + off_y)];
    }

    Int cols() const noexcept { return nx; }
    Int rows() const noexcept { return ny; }
    Int size() const noexcept { return nx * ny; }
    bool is_contiguous() const noexcept
    {
        return (ny == buf_ny) && (off_y == 0);
    }

    Int vecs() const noexcept { return nx; }
    Int nvec() const noexcept { return ny; }
    T *vec(Int ivec) noexcept
    {
        check(ivec < vecs());
        return &(buf->buffer[off_y + buf_ny * (ivec + off_x)]);
    }
    const T *vec(Int ivec) const noexcept
    {
        check(ivec < vecs());
        return &(buf->buffer[off_y + buf_ny * (ivec + off_x)]);
    }

    View<T> &operator=(T val)
    {
        for (Int irow = 0; irow < vecs(); irow++)
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
        for (Int irow = 0; irow < vecs(); irow++)
        {
            T *myrow = vec(irow);
            const T *otherrow = other.vec(irow);
            std::copy(otherrow, otherrow + nvec(), myrow);
        }
        return *this;
    }

    View<T> view(Int ix_lo, Int ix_hi, Int iy_lo, Int iy_hi)
    {
        return {*this, ix_lo, ix_hi, iy_lo, iy_hi};
    }

    friend class Buffer<T>;
    template <typename U>
    friend std::ostream &operator<<(std::ostream &os, const View<U> &buf);
};

template <typename U>
std::ostream &operator<<(std::ostream &os, const View<U> &buf)
{
    for (Int nj = 0; nj < buf.rows(); nj++)
    {
        for (Int ni = 0; ni < buf.cols(); ni++)
        {
            std::cout << std::setw(9) << buf(ni, nj);
        }
        std::cout << std::endl;
    }
    return os;
}

template <typename T, typename F>
inline auto apply(const View<T> &one, F f)
{
    Int nx = one.cols();
    Int ny = one.rows();

    using U = decltype(f(std::declval<T>()));
    Buffer<U> result(nx, ny);

    for (Int ivec = 0; ivec < one.vecs(); ivec++)
    {
        const T *__restrict one_vec = one.vec(ivec);
        U *__restrict result_vec = result.vec(ivec);
#pragma omp simd
        for (Int lvec = 0; lvec < one.nvec(); lvec++)
        {
            result_vec[lvec] = f(one_vec[lvec]);
        }
    }
    return result;
}

template <typename T, typename U, typename F>
inline auto apply(const View<T> &one, const View<U> &other, F f)
{
    Int nx = one.cols();
    Int ny = one.rows();
    check(nx == other.cols());
    check(ny == other.rows());

    using V = decltype(f(std::declval<T>(), std::declval<U>()));
    Buffer<V> result(nx, ny);

    for (Int ivec = 0; ivec < one.vecs(); ivec++)
    {
        const T *__restrict one_vec = one.vec(ivec);
        const U *__restrict other_vec = other.vec(ivec);
        V *__restrict result_vec = result.vec(ivec);
#pragma omp simd
        for (Int lvec = 0; lvec < one.nvec(); lvec++)
        {
            result_vec[lvec] = f(one_vec[lvec], other_vec[lvec]);
        }
    }
    return result;
}

template <typename T, typename U, typename V, typename F>
inline auto apply(const View<T> &one, const View<U> &other, const View<V> &onemore, F f)
{
    Int nx = one.cols();
    Int ny = one.rows();
    check(nx == other.cols());
    check(ny == other.rows());
    check(nx == onemore.cols());
    check(ny == onemore.rows());

    using W = decltype(f(std::declval<T>(), std::declval<U>(), std::declval<V>()));
    Buffer<W> result(nx, ny);

    for (Int ivec = 0; ivec < one.vecs(); ivec++)
    {
        const T *__restrict one_vec = one.vec(ivec);
        const U *__restrict other_vec = other.vec(ivec);
        const V *__restrict onemore_vec = onemore.vec(ivec);
        W *__restrict result_vec = result.vec(ivec);
#pragma omp simd
        for (Int lvec = 0; lvec < one.nvec(); lvec++)
        {
            result_vec[lvec] = f(one_vec[lvec], other_vec[lvec], onemore_vec[lvec]);
        }
    }
    return result;
}

};