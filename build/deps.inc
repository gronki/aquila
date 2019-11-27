test_framehandling.o: globals.o framehandling.o
transforms.o: globals.o
framehandling.o: globals.o
test_transforms.o: globals.o transforms.o
framehandling.o: globals.o
test_legacy_align.o: legacy_align.o convolution.o kernels.o globals.o findstar.o
fftw.o: fftw3.f03
aqlrgb.o: framehandling.o png.o convolution.o kernels.o globals.o statistics.o
test_example.o: globals.o
framehandling.o: globals.o
test_align_2.o: findstar.o globals.o legacy_align.o new_align.o
convolution.o: globals.o
new_align.o: findstar.o globals.o
convolution.o: globals.o
test_align.o: globals.o transforms.o
png.o: globals.o
transforms.o: globals.o
test_addsuffix.o: framehandling.o
test_png.o: globals.o png.o
test_deconvolution.o: kernels.o globals.o framehandling.o convolution.o
kernels.o: globals.o
aqconvol.o: kernels.o globals.o framehandling.o convolution.o
test_kernels.o: kernels.o globals.o
framehandling.o: globals.o
test_findstar.o: findstar.o kernels.o globals.o convolution.o
findstar.o: globals.o
test_background.o: statistics.o kernels.o globals.o convolution.o
template.o: globals.o
statistics.o: globals.o
legacy_align.o: findstar.o globals.o transforms.o
aqstack.o: framehandling.o legacy_align.o convolution.o kernels.o globals.o statistics.o findstar.o
