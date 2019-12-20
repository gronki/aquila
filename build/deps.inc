aqconvol.o: convolution.o framehandling.o globals.o kernels.o
aqlrgb.o: convolution.o framehandling.o globals.o kernels.o png.o statistics.o
aqstack.o: convolution.o findstar.o framehandling.o globals.o kernels.o new_align.o statistics.o
convolution.o: globals.o
convolution.o: globals.o
findstar.o: globals.o
framehandling.o: globals.o
framehandling.o: globals.o
framehandling.o: globals.o
framehandling.o: globals.o
hotpixel.o: globals.o statistics.o
kernels.o: globals.o
legacy_align.o: findstar.o globals.o
new_align.o: findstar.o globals.o
png.o: globals.o
statistics.o: globals.o
template.o: globals.o
test_addsuffix.o: framehandling.o
test_align.o: globals.o transforms.o
test_align_2.o: findstar.o globals.o legacy_align.o new_align.o
test_autorot.o: fftw.o findstar.o globals.o new_align.o
test_background.o: convolution.o globals.o kernels.o statistics.o
test_deconvolution.o: convolution.o framehandling.o globals.o kernels.o
test_example.o: globals.o
test_findstar.o: convolution.o findstar.o globals.o kernels.o
test_framehandling.o: framehandling.o globals.o
test_hotpixel.o: framehandling.o globals.o hotpixel.o
test_kernels.o: globals.o kernels.o
test_legacy_align.o: convolution.o findstar.o globals.o kernels.o legacy_align.o
test_png.o: globals.o png.o
test_transforms.o: globals.o transforms.o
transforms.o: globals.o
transforms.o: globals.o

aqconvol: aqconvol.o convolution.o framehandling.o globals.o kernels.o
aqlrgb: aqlrgb.o convolution.o framehandling.o globals.o kernels.o png.o statistics.o
aqstack: aqstack.o convolution.o findstar.o framehandling.o globals.o kernels.o new_align.o statistics.o
template: globals.o template.o
test_addsuffix: framehandling.o globals.o test_addsuffix.o
test_align: globals.o test_align.o transforms.o
test_align_2: findstar.o globals.o legacy_align.o new_align.o test_align_2.o
test_autorot: fftw.o findstar.o globals.o new_align.o test_autorot.o
test_background: convolution.o globals.o kernels.o statistics.o test_background.o
test_deconvolution: convolution.o framehandling.o globals.o kernels.o test_deconvolution.o
test_example: globals.o test_example.o
test_findstar: convolution.o findstar.o globals.o kernels.o test_findstar.o
test_framehandling: framehandling.o globals.o test_framehandling.o
test_hash_2: test_hash_2.o
test_hdr: test_hdr.o
test_hotpixel: framehandling.o globals.o hotpixel.o statistics.o test_hotpixel.o
test_kernels: globals.o kernels.o test_kernels.o
test_legacy_align: convolution.o findstar.o globals.o kernels.o legacy_align.o test_legacy_align.o
test_png: globals.o png.o test_png.o
test_transforms: globals.o test_transforms.o transforms.o
