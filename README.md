# aquila astrophotography package

**aquila** is a package designed to be a minimalist tool for initial steps of astrophotography data processing.
There is a shortage of easy to use tools for Linux.
Moreover, most tools in existence do not work well from terminal, which is a natural tool for managing and organizing hundreds of image files during stacking etc.
Popular tools like IRAF are not friendly for quick use by amateur astronomers.
**aquila** is intended to fill that void.

Currently, the package consists of following programs:

1. ``aqstack`` for stacking and reduction of monochromatic CCD images
2. ``aqlrgb`` for compositing images from many filters into one color picture

## Usage

Use option ``-h`` or ``-help`` to get the information below.

### aqstack

```
usage: aqstack [STRATEGY] [OPTIONS] FILE1 [FILE2 ...] -o OUTPUT
STRATEGY can be: bias, dark, flat, process, align, final
    -o/-output FILENAME  specifies the output filename
               -average  stack by average value
                -median  stack by median
               -sigclip  stack by 3-sigma clipped average
                 -align  align frames
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
         [-no]-hot-only  do not remove dark, just correct hot pixels {def.: OFF}
  [-no]-darkopt [SIGMA]  optimize dark to minimize correlation
                         if sigma is given (such as 3.0), only background
                         will be used {def.: OFF}
       [-no]-dirty-dark  subtract bias from dark (only if not done before!)
                         {def.: OFF}
```

### aqlrgb

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

## Changelog

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