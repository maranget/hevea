Name: hevea
Version: VERSION
Release: 1
Summary: Hevea, a fast LaTeX to HTML translator
Source: ftp://ftp.inria.fr/INRIA/Projects/para/hevea/UNSTABLE/hevea-VERSION.tar.gz
Copyright: freely redistributable
Group: Development/Languages
Vendor: INRIA Rocquencourt
URL: http://para.inria.fr/~maranget/hevea/

%description

HEVEA is a LaTeX to HTML translator.  The input language is a fairly
complete subset of LaTeX2e (old LaTeX style is also accepted) and the
output language is HTML that is (hopefully) correct with respect to
version 3.2.

This package is a binary installation of the hevea system.

    This software includes the Objective Caml run-time system, 
    which is copyright 1995 INRIA.
%prep
%setup
%build
make TARGET=opt LIBDIR=/usr/lib/hevea
rm -rf /usr/lib/hevea
make install LIBDIR=/usr/lib/hevea BINDIR=/usr/bin
%files
/usr/bin/hevea
/usr/bin/hacha
/usr/bin/imagen
/usr/lib/hevea/article.hva
/usr/lib/hevea/book.hva
/usr/lib/hevea/report.hva
/usr/lib/hevea/seminar.hva
/usr/lib/hevea/contents_motif.gif
/usr/lib/hevea/cutfoot-eng.html
/usr/lib/hevea/cutfoot-fra.html
/usr/lib/hevea/footer.tex
/usr/lib/hevea/hevea.hva
/usr/lib/hevea/hevea.sty
/usr/lib/hevea/ams.hva
/usr/lib/hevea/mathaccents.hva
/usr/lib/hevea/next_motif.gif
/usr/lib/hevea/previous_motif.gif

