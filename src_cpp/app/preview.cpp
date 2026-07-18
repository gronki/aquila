#include "preview.hpp"
#include <SDL2/SDL.h>
#include <SDL2/SDL_events.h>
#include <SDL2/SDL_keycode.h>
#include <SDL2/SDL_render.h>
#include <SDL2/SDL_video.h>
#include <SDL_pixels.h>
#include <aquila.h>
#include <cmath>
#include <iostream>

namespace aquila::app
{

enum
{
    REQUEST_REDRAW,
    REQUEST_QUIT
};

std::vector<uint8_t> render_grayscale(const Buffer<real_buf_t> &buf, float lo, float hi)
{
    std::vector<uint8_t> result(3 * buf.size());
    const auto *data = buf.data();
    for (int64_t i = 0; i < buf.size(); i++)
    {
        const auto ib = i * 3;
        auto scaled = (data[i] - lo) / (hi - lo);
        scaled = scaled < float(0) ? float(0) : scaled;
        scaled = scaled > float(1) ? float(1) : scaled;
        const auto bit8 = uint8_t(scaled * 255);
        result[ib] = bit8;
        result[ib + 1] = bit8;
        result[ib + 2] = bit8;
    }
    return result;
}

static std::tuple<SDL_Rect, SDL_Rect> compute_src_dst_rect(
    const image_payload_t &payload, const view_param_t &view)
{

    const float center_img_x = (float(payload.width) - 1) / 2;
    const float center_img_y = (float(payload.height) - 1) / 2;
    const float center_screen_x = (float(view.width) - 1) / 2;
    const float center_screen_y = (float(view.height) - 1) / 2;

    const float pos_im_scr_left =
        center_screen_x + view.scale * (view.center_x - center_img_x);
    const float pos_im_scr_right =
        center_screen_x + view.scale * (view.center_x + center_img_x);
    const float pos_im_scr_top =
        center_screen_y + view.scale * (view.center_y - center_img_y);
    const float pos_im_scr_bottom =
        center_screen_y + view.scale * (view.center_y + center_img_y);

    SDL_Rect dst{
        .x = int(std::max(pos_im_scr_left, 0.f)),
        .y = int(std::max(pos_im_scr_top, 0.f)),
        .w = int(std::min(pos_im_scr_right, float(view.width) - 1)
            - std::max(pos_im_scr_left, 0.f)),
        .h = int(std::min(pos_im_scr_bottom, float(view.height) - 1)
            - std::max(pos_im_scr_top, 0.f)),
    };

    const float pos_scr_im_left =
        center_img_x - view.center_x - center_screen_x / view.scale;
    const float pos_scr_im_right =
        center_img_x - view.center_x + center_screen_x / view.scale;
    const float pos_scr_im_top = center_img_y - view.center_y - center_screen_y / view.scale;
    const float pos_scr_im_bottom =
        center_img_y - view.center_y + center_screen_y / view.scale;

    SDL_Rect src{
        .x = int(std::max(pos_scr_im_left, 0.f)),
        .y = int(std::max(pos_scr_im_top, 0.f)),
        .w = int(std::min(pos_scr_im_right, float(payload.width) - 1)
            - std::max(pos_scr_im_left, 0.f)),
        .h = int(std::min(pos_scr_im_bottom, float(payload.height) - 1)
            - std::max(pos_scr_im_top, 0.f)),
    };
    return {src, dst};
}

void window_thread_proc(image_payload_t *payload, std::mutex *mtx, std::atomic_bool *done)
{

    SDL_Init(SDL_INIT_VIDEO);

    std::unique_ptr<SDL_Window, void (*)(SDL_Window *)> window{
        SDL_CreateWindow(
            "Aquila Preview", 0, 0, 1200, 800, SDL_WINDOW_RESIZABLE | SDL_WINDOW_ALWAYS_ON_TOP),
        [](SDL_Window *w)
        {
            if (w)
                SDL_DestroyWindow(w);
        }};

    if (!window)
    {
        std::cerr << "Creating window failed" << std::endl;
        return;
    }

    std::unique_ptr<SDL_Renderer, void (*)(SDL_Renderer *)> renderer{
        SDL_CreateRenderer(window.get(), -1, SDL_RENDERER_SOFTWARE),
        [](SDL_Renderer *r)
        {
            if (r)
                SDL_DestroyRenderer(r);
        }};

    using TexturePtr = std::unique_ptr<SDL_Texture, void (*)(SDL_Texture *)>;
    auto texture_destroy = [](SDL_Texture *t)
    {
        if (t)
            SDL_DestroyTexture(t);
    };
    TexturePtr texture{nullptr, texture_destroy};

    view_param_t view;
    int w_, h_;
    SDL_GetWindowSize(window.get(), &w_, &h_);
    view.width = w_;
    view.height = h_;

    if (!window)
    {
        std::cerr << "Creating window failed" << std::endl;
        return;
    }
    while (!*done)
    {
        SDL_Event event;

        while (!*done && SDL_WaitEventTimeout(&event, 10))
        {
            //  std::cout << "event! " << event.type << std::endl;
            switch (event.type)
            {
            case SDL_KEYDOWN:
                switch (event.key.keysym.sym)
                {
                case SDLK_LEFT:
                case SDLK_h:
                    view.center_x += 50 / view.scale;
                    break;
                case SDLK_RIGHT:
                case SDLK_l:
                    view.center_x -= 50 / view.scale;
                    break;
                case SDLK_UP:
                case SDLK_k:
                    view.center_y += 50 / view.scale;
                    break;
                case SDLK_DOWN:
                case SDLK_j:
                    view.center_y -= 50 / view.scale;
                    break;
                case SDLK_PAGEUP:
                    view.scale /= std::sqrt(2);
                    break;
                case SDLK_PAGEDOWN:
                    view.scale *= std::sqrt(2);
                    break;
                case SDLK_0:
                    view.center_x = 0;
                    view.center_y = 0;
                    view.scale = std::min(view.height / float(payload->height),
                        view.width / float(payload->width));
                    break;
                case SDLK_1:
                    view.scale = 1;
                    break;
                }
                break;
            case SDL_WINDOWEVENT:
                if (event.window.event == SDL_WINDOWEVENT_RESIZED)
                {
                    view.width = event.window.data1;
                    view.height = event.window.data2;

                    /*  std::cout << "Window size: " << view.width
                                << " height: " << view.height << std::endl;*/
                }
                break;
            case SDL_USEREVENT:
                if (event.user.code == REQUEST_REDRAW)
                {
                    // std::cout << "Redraw requested" << std::endl;
                    auto rendered = render_grayscale(*payload->buf, 0, 1);
                    texture = TexturePtr{SDL_CreateTexture(renderer.get(),
                                             SDL_PIXELFORMAT_RGB24,
                                             SDL_TEXTUREACCESS_STATIC,
                                             payload->width,
                                             payload->height),
                        texture_destroy};
                    SDL_UpdateTexture(
                        texture.get(), nullptr, rendered.data(), payload->width * 3);
                }
                break;
            case SDL_QUIT:
                *done = true;
                break;
            }
        }

        SDL_RenderClear(renderer.get());
        if (texture)
        {
            auto [src_rect, dst_rect] = compute_src_dst_rect(*payload, view);
            /*  std::cout << "src  x=" << src_rect.x << " y=" << src_rect.y
                        << " w=" << src_rect.w << " h=" << src_rect.h << std::endl;
              std::cout << "dst  x=" << dst_rect.x << " y=" << dst_rect.y
                        << " w=" << dst_rect.w << " h=" << dst_rect.h << std::endl;*/
            SDL_RenderCopy(renderer.get(), texture.get(), &src_rect, &dst_rect);
        }
        SDL_RenderPresent(renderer.get());
    }
}

AquilaWindow::WindowThread::WindowThread() :
    window_thread(window_thread_proc, &payload, &payload_mutex, &done)
{
}

AquilaWindow::WindowThread::~WindowThread()
{
    done = true;
    window_thread.join();
}

void AquilaWindow::update(const Value *v)
{
    std::cout << "updating with value" << (v ? v->str().c_str() : "(null)") << std::endl;
    const auto *imval = value_cast<values::BufferValue>(v);
    if (!imval)
        return;
    if (!thread || thread->done)
        thread = std::make_unique<WindowThread>();
    thread->update(imval);
}

void AquilaWindow::WindowThread::update(const values::BufferValue *imval)
{
    std::lock_guard lock(payload_mutex);
    payload.buf = std::make_unique<Buffer<real_buf_t>>(imval->buffer);
    payload.width = payload.buf->cols();
    payload.height = payload.buf->rows();
    avsd_2d(c_const_buf(*payload.buf), &payload.im_av, &payload.im_sd);

    SDL_Event event;
    event.type = SDL_USEREVENT;
    event.user.code = REQUEST_REDRAW;
    SDL_PushEvent(&event);
}

}; // namespace aquila::app
