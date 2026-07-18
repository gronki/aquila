#pragma once

#include <SDL2/SDL.h>
#include <atomic>
#include <thread>
#include <value.hpp>
#include <values/frame.hpp>

namespace aquila::app
{

struct view_param_t
{
    float center_x = 0, center_y = 0, scale = 1;
    size_t width, height;
};

struct image_payload_t
{
    std::unique_ptr<Buffer<real_buf_t>> buf;
    size_t width, height;
    float im_av = 0, im_sd = 1;
};

class AquilaWindow
{
    struct WindowThread
    {
        WindowThread();
        ~WindowThread();
        std::thread window_thread;
        std::atomic_bool done{false};
        std::mutex payload_mutex;
        image_payload_t payload;
        void update(const values::BufferValue *);
    };

    std::unique_ptr<WindowThread> thread;

public:
    void update(const Value *);
};

}; // namespace aquila::app
