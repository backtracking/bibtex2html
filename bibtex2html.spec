Summary: Convert BibTeX source files to HTML amongst other things
Name: bibtex2html
Version: 1.66
Release: 1
License: GNU/GPL
Group: Applications/Publishing
URL: http://www.lri.fr/~filliatr/bibtex2html/
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-buildroot
Packager: Edward Grace <ej.grace@imperial.ac.uk>

%description
Tools for filtering BibTeX files, generating HTML files from the
BibTeX sources and generating BibTeX files from LaTeX aux files.
%prep
%setup -q

%build
./configure --prefix=/usr
make


%install
rm -rf $RPM_BUILD_ROOT
make install prefix=$RPM_BUILD_ROOT/%{_prefix} 
cd $RPM_BUILD_ROOT
# Following command generates a list of files that were installed
find -type f | sed 's/^\.//; s/\(\.[0-9]\)$/\1.gz/;' > %{_tmppath}/%{name}-%{version}-MANIFEST


%clean
rm -rf $RPM_BUILD_ROOT

# Use the generated file list to name the files in the package.
%files -f %{_tmppath}/%{name}-%{version}-MANIFEST
%defattr(-,root,root,-)
%doc


%changelog
* Thu Feb 19 2004 Edward Grace <ej.grace@imperial.ac.uk> 1.66-1
- Trivial modification.  Update version numbers to build latest version.

* Fri Oct 10 2003 Michael Daum <micha@nats.informatik.uni-hamburg.de> 1.65-3
- fixed install prefix
- fixed manifesting compressed manual pages on suse

* Tue Oct  7 2003 Edward Grace <ej.grace@imperial.ac.uk> 1.65-2
- Second build, this time I have included more info on the package

* Tue Oct  7 2003 Edward Grace <ej.grace@imperial.ac.uk> 
- Initial build.
