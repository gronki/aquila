Name:           aquila
Version:        210111
Release:        1%{?dist}
Summary:        aquila

License:        MIT
URL:            https://github.com/gronki/aquila
Source0:        aquila-%{version}.tar.gz

BuildRequires:  cfitsio-devel libpng-devel
Requires:       cfitsio libpng

%description
command-line utilities for astronomical image processing

%prep
%autosetup

%build
%set_build_flags
export FPM_FFLAGS="${FPM_FFLAGS} -g"
fpm clean --all
fpm build
pwd

%install
rm -rf "%{buildroot}"
pwd
export FPM_FFLAGS="${FPM_FFLAGS} -g"
fpm install --no-rebuild --prefix "%{buildroot}%{_prefix}"

%files
#%license LICENSE
%doc README.md
%{_bindir}/*
