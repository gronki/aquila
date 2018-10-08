convolution.o   : globals.o 
findstar.o      : globals.o 
kernels.o       : globals.o 
legacy_align.o  : globals.o transforms.o findstar.o 
test_background.o: globals.o kernels.o statistics.o convolution.o findstar.o 
test_example.o  : globals.o 
test_kernels.o  : kernels.o globals.o 
test_align.o    : globals.o transforms.o 
test_findstar.o : kernels.o globals.o findstar.o convolution.o 
test_legacy_align.o: globals.o kernels.o legacy_align.o findstar.o             \
    convolution.o 
test_transforms.o: globals.o transforms.o 
projections.o   : globals.o transforms.o 
statistics.o    : globals.o 
transforms.o    : globals.o 
