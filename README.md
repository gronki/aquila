# Aquila: LRGB astrophotography processor with Fortran heart

![Aquila](snap/gui/aquila.png)

**AppImage**

[Click here](https://github.com/gronki/aquila/releases) to download **AppImage**.

```sh
sudo install aquila-<version>-x86_64.AppImage /usr/local/aquila
```

**Snap**

[![aquila](https://snapcraft.io/aquila/badge.svg)](https://snapcraft.io/aquila)
[![Get it from the Snap Store](https://snapcraft.io/en/dark/install.svg)](https://snapcraft.io/aquila)

```sh
sudo snap install aquila
sudo snap alias aquila.lrgb aqlrgb
sudo snap alias aquila.stack aqstack
sudo snap alias aquila.cli aqcli
```

**aquila** is a command-line toolkit for astrophotography data reduction and compositing on Linux.
While graphical tools exist for this workflow, they rarely integrate well with the terminal — the natural environment for batch-processing hundreds of FITS files, writing reproducible pipelines, and automating nightly runs.
Aquila is designed to fit into that world: fast, scriptable, and unobtrusive.

Currently, the package consists of following programs:

1. ``aqstack`` (via snap: ``aquila.stack``) — stacking and calibration of monochromatic CCD frames (bias, dark, flat, alignment, sigma-clipping)
2. ``aqlrgb`` (via snap: ``aquila.lrgb``) — compositing multi-filter data into colour images with luminance, white-balance, and stretching controls
3. ``aqcli`` (via snap: ``aquila.cli``) — a scripting interpreter for building full end-to-end image processing pipelines

## Installation

### Simple methods

**[Click here](https://github.com/gronki/aquila/releases) to download packages for Ubuntu/Debian.**. This is the recommended method for most users.

You can also quickly build ready-to-use Aquila Docker image (~200 MB):

```
docker build -t aquila https://github.com/gronki/aquila.git
docker run -it aquila
```

### Build from source

To build the program on Ubuntu/Debian, first install the dependencies:

```sh
apt-get update && apt-get install -y --no-install-recommends cmake make gcc g++ gfortran libreadline-dev pkg-config
```

Manual build and installation:


```sh
mkdir build && cd build && cmake .. -DCMAKE_BUILD_TYPE=Release -DAQUILA_OPENMP=On -DCMAKE_INSTALL_PREFIX=/opt/aquila && cmake --build . && sudo cmake --install .
```

Remember to add to your ``.bashrc``:

```sh
export PATH="${PATH}:/opt/aquila/bin"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/opt/aquila/lib/aquila"
```


## Usage

Use option ``-h`` or ``-help`` to get the information below.

### aqstack

``aqstack`` handles the calibration and stacking side of the workflow: it subtracts bias, dark, and flat frames, identifies and corrects hot pixels, optionally aligns frames, and combines them using your choice of average, median, or sigma-clipped mean. It is designed to run efficiently even on large frame sets, with temperature filtering to keep only frames from the same thermal session and optional resampling for drizzle-style oversampling.

```
usage: aqstack [STRATEGY] [OPTIONS] FILE1 [FILE2 ...] -o OUTPUT
STRATEGY can be: bias, dark, flat, process, align, final
      -o/-output FILENAME  specifies the output filename
                 -average  stack by average value
                  -median  stack by median
                 -sigclip  stack by 3-sigma clipped average
          -align [METHOD]  align frames (isometric). METHOD can be:
                           polygon: quadrangle matching {def.}
                           gravity_only: use gravity align method
                           gravity: use polygon matching and fine-tune
                           using gravity
            -ref FILENAME  align to this frame rather than first frame
   -resample [FACTOR=1.5]  resample before stacking (only with -align)
                           FACTOR is scale to be applied
             -norm[alize]  normalize to average before stacking
                -no-stack  process but do not stack images
        -suffix/-S SUFFIX  suffix that will be added to file names
                           when using -nostack {def.: _r}
   -temp/-T TEMP [DT=0.5]  stack only frames with given CCD temperature
                           DT gives allowed deviation in temperature
                           in Celsius
           -bias FILENAME  subtract this master bias
           -flat FILENAME  remove this master flat
           -dark FILENAME  remove this master dark
    [-no]-hot [SIGMA=5.0]  find hot pixels on dark and correct them
                           in the image frames (if dark is given) {def.: ON}
           [-no]-hot-only  do not remove dark, just correct hot pixels
                           {def.: OFF}
[-no]-darkopt [SIGMA=5.0]  optimize dark to minimize correlation
                           if sigma is nonzero, only background will be used.
                           SIGMA=0 forces to use all pixels {def.: OFF}
         [-no]-dirty-dark  subtract bias from dark (only if not done before!)
                           {def.: OFF}
```

### aqlrgb

``aqlrgb`` takes calibrated, aligned frames from multiple filters and combines them into a colour image. It supports the classic LRGB workflow — using a high-SNR luminance channel to sharpen the colour data — as well as narrowband palette compositing. It can equalise colour balance across channels, suppress background gradients, and apply nonlinear stretches (sqrt, asinh, log) before writing the result to FITS or PNG.

```
prepares the aligned images for RGB processing
usage: aqlrgb [L] R G B [-o FILE] [options]
R, G, B are color frames and L is optional luminance
            -o/-output  specifies the output file name
                        (allowed formats: fits, png)
                -split  save as 3 files fits rather than one cube
                        for example, if image.fits is given to -o, three files
                        image.r.fits, image.g.fits, image.b.fits will be written
        -smooth [FWHM]  smoothes color while preserving luminance
                        if FWHM not given, default value (2.5) will be used
         -wb/-equalize  attempt to make stars white
                        (works best if background is small)
       -bg/-background  attempt to make background black
                        (do not use for strong nebulosity)
     -sqrt/-asinh/-log  compress the image levels before saving
  -sqrt2/-asinh2/-log2  same but using luminosity
                        (boosts star colors but can kill some details)
               -h[elp]  prints help
```

## Scripting with aqcli

``aqcli`` is a small domain-specific language for writing image processing pipelines. Rather than stringing together shell commands or writing Python glue code, you can express a full reduction workflow — loading frames, stacking, compositing, stretching, saving — as a readable script that runs top to bottom.

The language is expression-oriented: operations return values that can be assigned to variables or chained with the pipe operator ``%``. Routines accept positional and keyword arguments; arrays of frames flow through pipelines element-wise.

### Example: stacking a luminance sequence

```
lights = path("lights/L_frame_00{1,2,3,4,5,6,7,8}.fits") % file()

stars      = findstar(lights)
alignments = register(stars, stars % item(1))

! alignment will be applied here once projection is in place

stack(lights, method: "average") % save("stack_L.fits")
```

Lines beginning with ``!`` are comments.

### Example: SHO narrowband composite

```
S = file("stack_S.fits")
H = file("stack_H.fits")
O = file("stack_O.fits")

R = mix(H, 0.9, S, 3)
G = mix(H, 0.9, O, 1, S, -0.8)
B = mix(O, 3.3)

rgb = lrgb(H, R, G, B)
rgb % save("sho.fits")
```

The pipe operator ``%`` passes the result of the left-hand side as the first argument of the right-hand side, making it easy to build readable processing chains without intermediate variables.

## Changelog

- 210111: removed flux ratios, changed triangles to quadrangles
- 210109: auto-flip, polygon align
- 210104: dark optimization (simple), cleaning dark from bias
- 210103: hot pixels are corrected when dark is loaded
- 200717: hot pixel correction
- 200716: now temperature filter is before loading images, which saves memory

## Planned Features

- color images
- large image handling
- use 32 bits for image storage
- more non-linear distortions
- proper drizzling
- better control over colors
- LRGB and edit commands
