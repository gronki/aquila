# Aquila: LRGB astrophotography processor with Fortran heart

![Aquila](snap/gui/aquila.png)

**aquila** is a command-line toolkit for astrophotography data reduction and compositing on Linux.
While graphical tools exist for this workflow, they rarely integrate well with the terminal — the natural environment for batch-processing hundreds of FITS files, writing reproducible pipelines, and automating nightly runs.
Aquila is designed to fit into that world: fast, scriptable, and unobtrusive.

Currently, the package consists of following programs:

1. ``aqstack``— stacking and calibration of monochromatic CCD frames (bias, dark, flat, alignment, sigma-clipping)
2. ``aqlrgb`` — compositing multi-filter data into colour images with luminance, white-balance, and stretching controls
3. ``aqcli`` — a scripting interpreter for building full end-to-end image processing pipelines

## Installation

**Snap**

The recommended way to install and use Aquila is to obtain it from the Snap store.

[![aquila](https://snapcraft.io/aquila/badge.svg)](https://snapcraft.io/aquila)
[![Get it from the Snap Store](https://snapcraft.io/en/dark/install.svg)](https://snapcraft.io/aquila)

```sh
sudo snap install aquila
```

**AppImage**

* Download Stable [x86_64 / PC](https://github.com/gronki/aquila/releases/download/stable/aquila-x86_64.AppImage) and [ARM64 / Rpi / Mac](https://github.com/gronki/aquila/releases/download/stable/aquila-aarch64.AppImage)

[Click here](https://github.com/gronki/aquila/releases) to see all **AppImage** releases.

```sh
curl -L https://github.com/gronki/aquila/releases/download/stable/aquila-$(uname -m).AppImage -o aquila
chmod +x aquila
# test
./aquila
sudo install aquila /usr/local/bin/aquila
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

``aqcli`` is a small language for writing image processing pipelines. Rather than
stringing together shell commands or writing Python glue, a whole reduction —
loading frames, calibrating, aligning, stacking, compositing, stretching,
saving — is one readable chain. Type ``help`` at the prompt for the list of
operations and their arguments.

Everything is an expression:

- ``|`` passes the result on its left as the **first argument** of the operation
  on its right, so ``load "a.fits" | bin 2`` means ``bin(load("a.fits"), 2)``.
  Inside a chain the parentheses are optional: ``asinh factor: 30`` is
  ``asinh(factor: 30)``.
- Arguments that have a default are **keyword-only** and written with a colon:
  ``stack method: "sigclip"``, not ``stack("sigclip")``.
- ``as`` names a result, and splits a sequence into several names at once.
  ``=`` assigns as well: ``t = register(stars, ref)``.
- ``[a, b, c]`` is a sequence, and operations map over sequences element-wise,
  so one chain processes every channel at once.
- ``load`` and ``path`` expand shell-style patterns including braces, so
  ``load "stack_{R,G,B}.fits"`` gives three frames as one sequence.
- ``#`` starts a comment. Each line is one complete expression; a chain cannot
  be split across lines.

### Example: calibrate, align and stack one channel

```
# hotpixel mask from the dark frame
load "dark.fits" | hot_find() | as hot

# frames to stack, with the hotpixels repaired
load "Light/R/*.fits" | hot_fix hot | as imgs

# stars of every frame, then align to the first one and stack
imgs | findstar() | as stars
stars | register item(stars, 1) | project imgs | normalize() | stack method: "sigclip" | save "stack_R.fits"
```

``findstar()`` with no arguments takes its frame from the pipe, and since
``imgs`` is a sequence, ``findstar`` and ``project`` run on every frame.
``item(stars, 1)`` picks the first star list as the alignment reference, and
``normalize()`` equalizes the levels before stacking.

### Example: background subtraction and stretch

```
load "stack_{R,G,B}.fits" | div 66666 | as R, G, B

[R, G, B] | sub bg([R, G, B]) | wb what: "stars" | add 5e-4 | asinh factor: 30 | mul 1.4 | asinh factor: 10 | as RR, GG, BB

png_rgb RR, GG, BB, "rgb.png"
```

``bg`` estimates the sky gradient and ``sub`` removes it, ``wb`` white-balances
on the stars, and the two ``asinh`` passes do the stretch. Scalars broadcast
over frames, and a sequence of scalars applies one value per channel, which is
how per-channel tweaks are written:

```
[R, G, B] | sub [1e-3, 2e-3, 3e-3] | mul [1, 0.9, 0.8]
```

The same pairing applies to filenames — saving a sequence needs one name per
frame:

```
[R, G, B] | save ["ch_R.fits", "ch_G.fits", "ch_B.fits"]
```

``path`` expands patterns the way bash does, so braces are a shorter way to
write that list. Brace expansion does not depend on the files existing, which
makes it useful for output names as well:

```
[R, G, B] | save path("ch{R,G,B}.fits")
```

### Example: narrowband composite

``mix`` forms linear combinations of frames and ``lrgb`` re-applies a luminance
to colour channels:

```
load "stack_{S,H,O}.fits" | div 66666 | as S, H, O

R = mix(H, 0.9, S, 3)
G = mix(H, 0.9, O, 1, S, -0.8)
B = mix(O, 3.3)

lrgb mix(1, R, 1, G, 1, B), R, G, B | as RR, GG, BB
png_rgb RR, GG, BB, "sho.png"
```

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
