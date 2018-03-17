convolution.o   : globals.o 
findstar.o      : globals.o 
kernels.o       : globals.o 
background.o    : globals.o kernels.o statistics.o convolution.o findstar.o 
example.o       : globals.o 
test.o          : kernels.o globals.o 
test_transforms.o: globals.o transforms.o 
statistics.o    : globals.o 
transforms.o    : globals.o 
