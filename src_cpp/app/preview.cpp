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

static uint8_t scale_f_u8(real_buf_t x, real_buf_t lo, real_buf_t hi)
{
    auto scaled = (x - lo) / (hi - lo);
    scaled = scaled < real_buf_t(0) ? real_buf_t(0) : scaled;
    scaled = scaled > real_buf_t(1) ? real_buf_t(1) : scaled;
    return uint8_t(scaled * 255);
}

std::vector<uint8_t> render_grayscale(const Buffer<real_buf_t> &buf, float lo, float hi)
{
    std::vector<uint8_t> result(3 * buf.size());
    const auto *data = buf.data();

#pragma omp parallel for
    for (int64_t i = 0; i < buf.size(); i++)
    {
        const auto ib = i * 3;
        auto bit8 = scale_f_u8(data[i], lo, hi);
        result[ib] = bit8;
        result[ib + 1] = bit8;
        result[ib + 2] = bit8;
    }
    return result;
}

std::vector<uint8_t> render_rgb(const Buffer<real_buf_t> &buf_r,
    const Buffer<real_buf_t> &buf_g,
    const Buffer<real_buf_t> &buf_b,
    float lo,
    float hi)
{
    std::vector<uint8_t> result(3 * buf_r.size());
    const auto *data_r = buf_r.data();
    const auto *data_g = buf_g.data();
    const auto *data_b = buf_b.data();

#pragma omp parallel for
    for (int64_t i = 0; i < buf_r.size(); i++)
    {
        const auto ib = i * 3;
        result[ib] = scale_f_u8(data_r[i], lo, hi);
        result[ib + 1] = scale_f_u8(data_g[i], lo, hi);
        result[ib + 2] = scale_f_u8(data_b[i], lo, hi);
    }
    return result;
}

static std::tuple<SDL_Rect, SDL_Rect> compute_src_dst_rect(
    int64_t image_width, int64_t image_height, const view_param_t &view)
{

    const float center_img_x = (float(image_width) - 1) / 2;
    const float center_img_y = (float(image_height) - 1) / 2;
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
        .w = int(std::min(pos_scr_im_right, float(image_width) - 1)
            - std::max(pos_scr_im_left, 0.f)),
        .h = int(std::min(pos_scr_im_bottom, float(image_height) - 1)
            - std::max(pos_scr_im_top, 0.f)),
    };
    return {src, dst};
}

using TexturePtr = std::unique_ptr<SDL_Texture, void (*)(SDL_Texture *)>;
void texture_deleter(SDL_Texture *t)
{
    if (t)
        SDL_DestroyTexture(t);
}

using SDLWindowPtr = std::unique_ptr<SDL_Window, void (*)(SDL_Window *)>;
void sdl_window_deleter(SDL_Window *w)
{
    if (w)
        SDL_DestroyWindow(w);
}

using SDLRendererPtr = std::unique_ptr<SDL_Renderer, void (*)(SDL_Renderer *)>;
void sdl_renderer_deleter(SDL_Renderer *r)
{
    if (r)
        SDL_DestroyRenderer(r);
}

bool get_current_dims(
    const image_payload_t *payload, const view_param_t &view, int64_t &width, int64_t &height)
{

    if (payload->matching_size && payload->bufs.size() > 0)
    {
        width = payload->bufs[0].cols();
        height = payload->bufs[0].rows();
        return true;
    }

    if (view.current_buf < 0 && view.current_buf >= payload->bufs.size())
        return false;

    width = payload->bufs[view.current_buf].cols();
    height = payload->bufs[view.current_buf].rows();

    return true;
}

void fix_view(const image_payload_t *payload, view_param_t &view)
{
    if (view.current_buf >= payload->bufs.size())
        view.current_buf = int64_t(payload->bufs.size()) - 1;
    if (view.current_buf < 0)
        view.current_buf = 0;

    if (payload->bufs.size() != 3 || !payload->matching_size)
        view.display = MONO;
}

void redraw_buffer(const image_payload_t *payload,
    view_param_t &view,
    SDL_Renderer *renderer,
    TexturePtr &texture)
{
    if (payload->bufs.size() == 0)
        return;
    fix_view(payload, view);
    int64_t w, h;
    if (!get_current_dims(payload, view, w, h))
        return;

    std::vector<uint8_t> rendered;

    if (view.display == MONO)
    {
        rendered = render_grayscale(payload->bufs[view.current_buf], view.lo, view.hi);
    }
    else
    {
        rendered = render_rgb(
            payload->bufs[0], payload->bufs[1], payload->bufs[2], view.lo, view.hi);
    }

    texture = TexturePtr{
        SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGB24, SDL_TEXTUREACCESS_STATIC, w, h),
        texture_deleter};

    SDL_UpdateTexture(texture.get(), nullptr, rendered.data(), w * 3);
}

void window_thread_proc(image_payload_t *payload, std::mutex *mtx, std::atomic_bool *done)
{

    SDL_Init(SDL_INIT_VIDEO);

    SDL_DisplayMode sdl_dm;
    SDL_GetCurrentDisplayMode(0, &sdl_dm);

    SDLWindowPtr window{SDL_CreateWindow("Aquila Preview",
                            sdl_dm.w * 0.3,
                            0,
                            sdl_dm.w * 0.7,
                            int(sdl_dm.h * 0.6),
                            SDL_WINDOW_RESIZABLE | SDL_WINDOW_ALWAYS_ON_TOP),
        sdl_window_deleter};

    if (!window)
    {
        std::cerr << "Creating window failed" << std::endl;
        return;
    }

    SDLRendererPtr renderer{SDL_CreateRenderer(window.get(), -1, SDL_RENDERER_SOFTWARE),
        sdl_renderer_deleter};

    TexturePtr texture{nullptr, texture_deleter};

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
            auto key = event.key.keysym.sym;
            switch (event.type)
            {
            case SDL_KEYDOWN:
                switch (key)
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
                case SDLK_o:
                case SDLK_MINUS:
                    view.scale /= std::sqrt(2);
                    break;
                case SDLK_i:
                case SDLK_PLUS:
                    view.scale *= std::sqrt(2);
                    break;
                case SDLK_p:
                {
                    int64_t w, h;
                    if (!get_current_dims(payload, view, w, h))
                        break;
                    view.center_x = 0;
                    view.center_y = 0;
                    view.scale = std::min(view.height / float(h), view.width / float(w));
                    break;
                }
                case SDLK_u:
                    view.scale = 1;
                    break;
                case SDLK_z:
                    view.lo = 0;
                    view.hi = 1;
                    view.scaling = ABSOLUTE;
                    redraw_buffer(payload, view, renderer.get(), texture);
                    break;
                case SDLK_x:
                    view.lo = payload->im_av - payload->im_sd;
                    view.hi = payload->im_av + payload->im_sd;
                    view.scaling = SIGMA;
                    redraw_buffer(payload, view, renderer.get(), texture);
                    break;
                case SDLK_a:
                case SDLK_s:
                {
                    float sign = (key == SDLK_a) ? -1 : 1;
                    float magnitude = view.scaling == SIGMA ? 0.05 * payload->im_sd : 0.05;
                    view.lo += sign * magnitude;
                    redraw_buffer(payload, view, renderer.get(), texture);
                    break;
                }
                case SDLK_d:
                case SDLK_f:
                {
                    float sign = (key == SDLK_d) ? -1 : 1;
                    float magnitude = view.scaling == SIGMA ? 0.05 * payload->im_sd : 0.05;
                    view.hi += sign * magnitude;
                    redraw_buffer(payload, view, renderer.get(), texture);
                    break;
                }
                case SDLK_0:
                    view.display = RGB;
                    redraw_buffer(payload, view, renderer.get(), texture);
                    break;
                case SDLK_1:
                    view.display = MONO;
                    if (payload && payload->bufs.size() >= 1)
                        view.current_buf = 0;
                    redraw_buffer(payload, view, renderer.get(), texture);
                    break;
                case SDLK_2:
                    view.display = MONO;
                    if (payload && payload->bufs.size() >= 2)
                        view.current_buf = 1;
                    redraw_buffer(payload, view, renderer.get(), texture);
                    break;
                case SDLK_3:
                    view.display = MONO;
                    if (payload && payload->bufs.size() >= 3)
                        view.current_buf = 2;
                    redraw_buffer(payload, view, renderer.get(), texture);
                    break;
                case SDLK_4:
                    view.display = MONO;
                    if (payload && payload->bufs.size() >= 4)
                        view.current_buf = 3;
                    redraw_buffer(payload, view, renderer.get(), texture);
                    break;
                case SDLK_5:
                    view.display = MONO;
                    if (payload && payload->bufs.size() >= 5)
                        view.current_buf = 4;
                    redraw_buffer(payload, view, renderer.get(), texture);
                    break;
                case SDLK_6:
                    view.display = MONO;
                    if (payload && payload->bufs.size() >= 6)
                        view.current_buf = 5;
                    redraw_buffer(payload, view, renderer.get(), texture);
                    break;
                case SDLK_7:
                    view.display = MONO;
                    if (payload && payload->bufs.size() >= 7)
                        view.current_buf = 6;
                    redraw_buffer(payload, view, renderer.get(), texture);
                    break;
                case SDLK_8:
                    view.display = MONO;
                    if (payload && payload->bufs.size() >= 8)
                        view.current_buf = 7;
                    redraw_buffer(payload, view, renderer.get(), texture);
                    break;
                case SDLK_9:
                    view.display = MONO;
                    if (payload && payload->bufs.size() >= 9)
                        view.current_buf = 8;
                    redraw_buffer(payload, view, renderer.get(), texture);
                    break;
                case SDLK_q:
                    *done = true;
                    break;
                }
                break;
            case SDL_WINDOWEVENT:
                if (event.window.event == SDL_WINDOWEVENT_RESIZED)
                {
                    view.width = event.window.data1;
                    view.height = event.window.data2;
                }
                break;
            case SDL_USEREVENT:
                if (event.user.code == REQUEST_REDRAW)
                {
                    redraw_buffer(payload, view, renderer.get(), texture);
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
            int64_t w, h;
            if (!get_current_dims(payload, view, w, h))
                continue;
            auto [src_rect, dst_rect] = compute_src_dst_rect(w, h, view);
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
    if (const auto *imval = value_cast<values::BufferValue>(v))
    {
        if (!thread || thread->done)
            thread = std::make_unique<WindowThread>();
        thread->update({imval});
        return;
    }
    if (const auto *seq = value_cast<SequenceValue>(v))
    {
        std::vector<const values::BufferValue *> bufs;
        for (const auto &item : seq->items)
        {
            auto im = value_cast<values::BufferValue>(item.get());
            if (im)
                bufs.push_back(im);
        }
        if (bufs.size() > 0)
        {
            if (!thread || thread->done)
                thread = std::make_unique<WindowThread>();
            thread->update(bufs);
            return;
        }
    }
}

void AquilaWindow::WindowThread::update(const std::vector<const values::BufferValue *> &bufs)
{
    std::lock_guard lock(payload_mutex);
    payload.bufs.clear();
    if (bufs.size() == 0)
        return;

    float av_tot = 0, sd_tot = 0;
    int64_t width = 0, height = 0;
    payload.matching_size = true;

    for (auto im : bufs)
    {
        Buffer<real_buf_t> buf(im->buffer);
        if (width == 0 && height == 0)
        {
            width = buf.cols();
            height = buf.rows();
        }
        else
        {
            if (buf.cols() != width && buf.rows() != height)
                payload.matching_size = false;
        }
        float av, sd;
        avsd_2d(c_const_buf(buf), &av, &sd);
        av_tot += av;
        sd_tot += sd * sd;
        payload.bufs.push_back(std::move(buf));
    }

    payload.im_av = av_tot / bufs.size();
    payload.im_sd = std::sqrt(sd_tot / bufs.size());

    SDL_Event event;
    event.type = SDL_USEREVENT;
    event.user.code = REQUEST_REDRAW;
    SDL_PushEvent(&event);
}

}; // namespace aquila::app
