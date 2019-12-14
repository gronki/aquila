convolution.o: globals.o
convolution.o: globals.o
test_transforms.o: transforms.o globals.o
test_findstar.o: globals.o kernels.o convolution.o findstar.o
png.o: globals.o
test_align_2.o: new_align.o legacy_align.o globals.o findstar.o
new_align.o: globals.o findstar.o
test_legacy_align.o: findstar.o kernels.o legacy_align.o globals.o convolution.o
transforms.o: globals.o
test_autorot.o: new_align.o globals.o fftw.o findstar.o
kernels.o: globals.o
findstar.o: globals.o
framehandling.o: globals.o
aqconvol.o: globals.o kernels.o convolution.o framehandling.o
test_addsuffix.o: framehandling.o
test_framehandling.o: globals.o framehandling.o
test_align.o: transforms.o globals.o
statistics.o: globals.o
hotpixel.o: statistics.o globals.o
template.o: globals.o
test_kernels.o: globals.o kernels.o
test_png.o: png.o globals.o
framehandling.o: globals.o
framehandling.o: globals.o
legacy_align.o: transforms.o findstar.o globals.o
test_background.o: statistics.o kernels.o convolution.o globals.o
aqstack.o: findstar.o kernels.o legacy_align.o statistics.o framehandling.o globals.o convolution.o
framehandling.o: globals.o
transforms.o: globals.o
test_deconvolution.o: globals.o kernels.o convolution.o framehandling.o
test_hotpixel.o: hotpixel.o globals.o framehandling.o
test_example.o: globals.o
aqlrgb.o: png.o kernels.o statistics.o framehandling.o globals.o convolution.o

test_transforms: transforms.o test_transforms.o globals.o
test_findstar: findstar.o kernels.o test_findstar.o globals.o convolution.o
test_align_2: findstar.o test_align_2.o legacy_align.o new_align.o globals.o transforms.o
test_legacy_align: findstar.o kernels.o test_legacy_align.o legacy_align.o transforms.o globals.o convolution.o
test_hdr: test_hdr.o
test_autorot: fftw.o findstar.o new_align.o globals.o test_autorot.o
test_hash_2: test_hash_2.o
test_addsuffix: globals.o framehandling.o test_addsuffix.o
test_framehandling: globals.o test_framehandling.o framehandling.o
aqconvol: kernels.o framehandling.o aqconvol.o globals.o convolution.o
test_align: transforms.o globals.o test_align.o
template: globals.o template.o
test_kernels: test_kernels.o globals.o kernels.o
test_png: png.o globals.o test_png.o
test_background: kernels.o test_background.o statistics.o globals.o convolution.o
aqstack: aqstack.o findstar.o kernels.o legacy_align.o statistics.o globals.o framehandling.o transforms.o convolution.o
test_deconvolution: kernels.o framehandling.o globals.o convolution.o test_deconvolution.o
test_hotpixel: test_hotpixel.o hotpixel.o statistics.o framehandling.o globals.o
test_example: globals.o test_example.o
aqlrgb: png.o kernels.o aqlrgb.o statistics.o framehandling.o globals.o convolution.o
