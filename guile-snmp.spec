%define ver          0.1
%define rel          1
%define prefix       /usr
%define home_page    http://github.com/tcolgate/guile-snmp
%define docprefix    %{prefix}/share

Summary: A guile binding for net-snmp
Name: guile-snmp
Version: %{ver}
Release: %{rel}
URL: %{home_page}
Source0: %{name}-%{version}.tar.gz
License: GPLv3
Group: Applications/System
BuildRoot: %{_tmppath}/%{name}-root

%description

Guile SNMP is a wrapper for the Net-SNMP libraries. it is intended
to provide a basic wrapper for direct useage of the Net-SNMP libraries
from Guile and an easy to use dialect of Scheme targeted at easier
SNMP reqperting.

%prep
%setup -q -n %{name}-%{version}

%build
[ ! -r configure ] && ./autogen.sh
%configure
make

%install
rm -rf ${RPM_BUILD_ROOT}
make DESTDIR=$RPM_BUILD_ROOT install

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,root,root)
%doc ChangeLog INSTALL COPYING README
%doc doc/*
%{_bindir}/*
%{_libdir}/*
%{_prefix}/share/*

%changelog
* Fri Mar 2 2011 Tristan Colgate <tcolgate@gmail.com>
- Shamelessly stolen from swig since it's a very simple and elegant
  spec file.
