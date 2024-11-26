Name:           aquila
Version:        210111
Release:        1%{?dist}
Summary:        aquila

License:        BSD 3-clause
URL:            https://github.com/gronki/aquila
Source0:        aquila-%{version}.tar.gz

# below dependencies are for Fedora
BuildRequires:  gcc-fortran cfitsio-devel libpng-devel
Requires:       libgfortran cfitsio libpng

%description
command-line utilities for astronomical image processing

%prep
%autosetup

%build
%set_build_flags
${FC} -O0 fpm.F90 -o fpm

%install
rm -rf "%{buildroot}"
FPM_FFLAGS="${FCFLAGS} -fno-lto -O3 -funsafe-math-optimizations -faggressive-function-elimination -g"
[ $(arch) == "x86_64" ] && FPM_FFLAGS="${FPM_FFLAGS} -mavx2"
export FPM_FFLAGS
./fpm install --verbose --profile release --prefix "%{buildroot}%{_prefix}"

%files
#%license LICENSE
%doc README.md
%{_bindir}/*
