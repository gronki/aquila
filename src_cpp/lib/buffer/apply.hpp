#pragma once

#include "buffer.hpp"

namespace aquila
{

template <typename T, typename F>
inline auto apply(const View<T> &one, F f)
{
    std::int64_t nx = one.cols();
    std::int64_t ny = one.rows();

    using U = decltype(f(std::declval<T>()));
    Buffer<U> result(nx, ny);

    if (one.is_contiguous())
    {
        const T *__restrict one_data = one.data();
        U *__restrict result_data = result.data();
        // #pragma omp simd
        for (std::int64_t i = 0; i < result.size(); i++)
        {
            result_data[i] = f(one_data[i]);
        }
        return result;
    }

    for (std::int64_t ivec = 0; ivec < one.vecs(); ivec++)
    {
        const T *__restrict one_vec = one.vec(ivec);
        U *__restrict result_vec = result.vec(ivec);
        // #pragma omp simd
        for (std::int64_t lvec = 0; lvec < one.nvec(); lvec++)
        {
            result_vec[lvec] = f(one_vec[lvec]);
        }
    }
    return result;
}

template <typename T, typename U, typename F>
inline auto apply(const View<T> &one, const View<U> &other, F f)
{
    std::int64_t nx = one.cols();
    std::int64_t ny = one.rows();
    check(nx == other.cols());
    check(ny == other.rows());

    using V = decltype(f(std::declval<T>(), std::declval<U>()));
    Buffer<V> result(nx, ny);

    if (one.is_contiguous() && other.is_contiguous())
    {
        const T *__restrict one_data = one.data();
        const U *__restrict other_data = other.data();
        V *__restrict result_data = result.data();
        // #pragma omp simd
        for (std::int64_t i = 0; i < result.size(); i++)
        {
            result_data[i] = f(one_data[i], other_data[i]);
        }
        return result;
    }

    for (std::int64_t ivec = 0; ivec < one.vecs(); ivec++)
    {
        const T *__restrict one_vec = one.vec(ivec);
        const U *__restrict other_vec = other.vec(ivec);
        V *__restrict result_vec = result.vec(ivec);
        // #pragma omp simd
        for (std::int64_t lvec = 0; lvec < one.nvec(); lvec++)
        {
            result_vec[lvec] = f(one_vec[lvec], other_vec[lvec]);
        }
    }
    return result;
}

template <typename T, typename U, typename V, typename F>
inline auto apply(const View<T> &one, const View<U> &other, const View<V> &onemore, F f)
{
    std::int64_t nx = one.cols();
    std::int64_t ny = one.rows();
    check(nx == other.cols());
    check(ny == other.rows());
    check(nx == onemore.cols());
    check(ny == onemore.rows());

    using W = decltype(f(std::declval<T>(), std::declval<U>(), std::declval<V>()));
    Buffer<W> result(nx, ny);

    if (one.is_contiguous() && other.is_contiguous() && onemore.is_contiguous())
    {
        const T *__restrict one_data = one.data();
        const U *__restrict other_data = other.data();
        const V *__restrict onemore_data = onemore.data();
        W *__restrict result_data = result.data();
        // #pragma omp simd
        for (std::int64_t i = 0; i < result.size(); i++)
        {
            result_data[i] = f(one_data[i], other_data[i], onemore_data[i]);
        }
        return result;
    }

    for (std::int64_t ivec = 0; ivec < one.vecs(); ivec++)
    {
        const T *__restrict one_vec = one.vec(ivec);
        const U *__restrict other_vec = other.vec(ivec);
        const V *__restrict onemore_vec = onemore.vec(ivec);
        W *__restrict result_vec = result.vec(ivec);
        // #pragma omp simd
        for (std::int64_t lvec = 0; lvec < one.nvec(); lvec++)
        {
            result_vec[lvec] = f(one_vec[lvec], other_vec[lvec], onemore_vec[lvec]);
        }
    }
    return result;
}

} // namespace aquila
