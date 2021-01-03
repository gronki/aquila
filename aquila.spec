Name:           aquila
Version:        210103
Release:        1%{?dist}
Summary:        aquila

License:        MIT
URL:            https://github.com/gronki/aquila
Source0:        https://github.com/gronki/aquila/archive/%{version}.tar.gz

BuildRequires:  cfitsio-devel fftw3-devel libpng-devel
Requires:       cfitsio fftw3 libpng

%description
command-line utilities for astronomical image processing

%prep
%autosetup

%build
%set_build_flags
make -C build FFLAGS="$FFLAGS -funsafe-math-optimizations -fopenmp"
#make -C build FFLAGS="%{build_fflags} -funsafe-math-optimizations -fopenmp"

%install
rm -rf "%{buildroot}"
make -C build install prefix="%{_prefix}" DESTDIR="%{buildroot}"

%files
#%license LICENSE
%doc README.md
%{_bindir}/aqstack
%{_bindir}/aqlrgb
