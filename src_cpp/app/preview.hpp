#pragma once

#include <SDL2/SDL.h>
#include <atomic>
#include <thread>
#include <value.hpp>
#include <values/frame.hpp>

namespace aquila::app
{

enum ScalingMode
{
    ABSOLUTE,
    SIGMA
};
enum DisplayMode
{
    MONO,
    RGB
};
struct view_param_t
{
    float center_x = 0, center_y = 0, scale = 1;
    float lo = 0, hi = 1;
    size_t current_buf = 0;
    size_t width, height;
    ScalingMode scaling{ABSOLUTE};
    DisplayMode display{MONO};
};

struct image_payload_t
{
    std::vector<Buffer<real_buf_t>> bufs;
    float im_av = 0, im_sd = 1;
    bool matching_size;
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
        void update(const std::vector<const values::BufferValue *> &);
    };

    std::unique_ptr<WindowThread> thread;

public:
    void update(const Value *);
};

}; // namespace aquila::app
