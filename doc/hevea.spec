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
version 4.0 (transitional)

This package is a binary installation of the hevea system.

    This software includes the Objective Caml run-time system, 
    which is copyright 1995 INRIA.
%prep
%setup
%build
make TARGET=opt LIBDIR=/usr/lib/hevea depend all
rm -rf /usr/lib/hevea
make install LIBDIR=/usr/lib/hevea BINDIR=/usr/bin
%files
/usr/bin/hevea
/usr/bin/hacha
/usr/bin/imagen
/usr/lib/hevea/cutfoot-eng.html
/usr/lib/hevea/cutfoot-fra.html
/usr/lib/hevea/contents_motif.gif
/usr/lib/hevea/next_motif.gif
/usr/lib/hevea/previous_motif.gif
/usr/lib/hevea/footer.tex
/usr/lib/hevea/hevea.sty
/usr/lib/hevea/html/hevea.hva
/usr/lib/hevea/html/symb.hva
/usr/lib/hevea/html/symb-text.hva
/usr/lib/hevea/html/symb-eng.hva
/usr/lib/hevea/html/symb-fra.hva
/usr/lib/hevea/html/symb-mathml.hva
/usr/lib/hevea/html/article.hva
/usr/lib/hevea/html/book.hva
/usr/lib/hevea/html/report.hva
/usr/lib/hevea/html/seminar.hva
/usr/lib/hevea/html/ams.hva
/usr/lib/hevea/html/mathaccents.hva
/usr/lib/hevea/html/multind.hva
/usr/lib/hevea/info/hevea.hva
/usr/lib/hevea/info/article.hva
/usr/lib/hevea/info/book.hva
/usr/lib/hevea/info/report.hva
/usr/lib/hevea/info/seminar.hva
/usr/lib/hevea/text/hevea.hva
/usr/lib/hevea/text/symb.hva
/usr/lib/hevea/text/article.hva
/usr/lib/hevea/text/book.hva
/usr/lib/hevea/text/report.hva
/usr/lib/hevea/text/seminar.hva