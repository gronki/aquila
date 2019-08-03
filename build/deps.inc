test_findstar.o: findstar.o convolution.o kernels.o globals.o
findstar.o: globals.o
aqconvol.o: convolution.o kernels.o globals.o framehandling.o
test_deconvolution.o: convolution.o kernels.o globals.o framehandling.o
fftw.o: fftw3.f03
convolution.o: globals.o
test_legacy_align.o: convolution.o kernels.o legacy_align.o globals.o findstar.o
test_addsuffix.o: framehandling.o
convolution.o: globals.o
statistics.o: globals.o
template.o: globals.o
framehandling.o: globals.o
aqstack.o: convolution.o kernels.o legacy_align.o globals.o findstar.o framehandling.o
framehandling.o: globals.o
framehandling.o: globals.o
aqlrgb.o: convolution.o kernels.o globals.o statistics.o framehandling.o
legacy_align.o: findstar.o globals.o transforms.o
test_align.o: globals.o transforms.o
test_background.o: convolution.o kernels.o globals.o statistics.o
png.o: globals.o
test_kernels.o: kernels.o globals.o
transforms.o: globals.o
test_transforms.o: globals.o transforms.o
kernels.o: globals.o
test_example.o: globals.o
test_png.o: png.o globals.o
transforms.o: globals.o
test_framehandling.o: globals.o framehandling.o
framehandling.o: globals.o
