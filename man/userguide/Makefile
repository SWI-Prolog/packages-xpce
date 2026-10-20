.SUFFIXES: .tex .dvi .plx .pl .eps .pdf

SWIPLTEX=../../../../man
PCETEX=../../TeX
DOCTOTEX=../../bin/doc2tex
PLTOTEX=../../bin/pl2tex
CORRECTINDEX=../../bin/correctindex
MAKEINDEX=makeindex -s $(PCETEX)/makeindex.ist
DOC=xpceusg
LOG=$(DOC).log
PDF=$(DOC).pdf
RERUN="Rerun to get cross-references right"
CLHIER=figs/pceclasshierarchy.pdf
PCEVERSION=../../VERSION

COMMON=	intro.tex starting.tex globalarch.tex online.tex debug.tex \
	interface.tex graphics.tex drawing.tex devices.tex pceprolog.tex \
	memory.tex \
	glossary.tex techniques.tex report.tex font.tex hyper.tex \
	udc.tex dragdrop.tex dragdropdemo.tex control.tex whoiswho.tex \
	panes.tex \
	namedref.tex ugraphics.tex code.tex classvar.tex diaedit.tex \
	dialog.tex wysiwyg.tex frame.tex error.tex application.tex \
	resource.tex images.tex summary.tex body.tex driver.tex \
	mswin.tex balloon.tex threads.tex status.tex \
	libs.tex findfile.tex toc.tex tabular.tex libdia.tex mlingual.tex \
	docrender.tex libplot.tex printing.tex

XPCETEX=	xpceusg.tex version.tex $(COMMON)

all:	$(PDF)
pdf:	$(PDF)

epsimages=	$(shell find . -name '*.eps')
pdfimages=	$(epsimages:.eps=.pdf)

pdfimg:	$(pdfimages)

$(PDF):	$(XPCETEX) $(LOGO) $(CLHIER)
	$(MAKE) pdfimg
	@TEXINPUTS=.:$(SWIPLTEX):$(PCETEX):$$TEXINPUTS; \
	echo "**********************************************************"; \
	echo "* Running pdflatex $(DOC)"; \
	echo "**********************************************************"; \
	export TEXINPUTS; \
	rm -f $(LOG); \
	while test ! -e $(LOG) || grep -q $(RERUN) $(LOG); do \
	  $(CORRECTINDEX) $(DOC).idx; \
	  $(MAKEINDEX) $(DOC).idx; \
	  touch $(DOC).ind; \
	  pdflatex $(DOC) || exit 1; \
	done

html:	$(XPCETEX) $(CLHIER)
	latex2html xpceusg

tex:	$(XPCETEX)

# version.tex defines \productversion from the xpce VERSION file
# and \productdate and \productyear from the current date.  The file is
# only rewritten if its content changes.

version.tex: FORCE
	@v=`tr -d ' \t\r\n' < $(PCEVERSION)`; \
	m=`LANG=POSIX date '+%B'`; \
	y=`date '+%Y'`; \
	printf '\\newcommand{\\productversion}{%s}\n\\newcommand{\\productdate}{%s}\n\\newcommand{\\productyear}{%s}\n' \
	  "$$v" "$$m $$y" "$$y" > $@.tmp; \
	if cmp -s $@.tmp $@; then rm $@.tmp; else mv $@.tmp $@; fi

FORCE:

# Recreate the screenshots in figs from the example code.  Use e.g.
# `make screenshots FIGS="dialog employee"` to make specific figures.
# See screenshots/screenshots.pl.

SWIPL=swipl

screenshots:
	screenshots/shoot.sh $(SWIPL) screenshots/screenshots.pl $(FIGS)

.PHONY: screenshots FORCE

bib:	xpceusg.aux
	(TEXINPUTS=$(PCETEX):$$TEXINPUTS; \
	 BIBINPUTS=$$TEXINPUTS; \
	 export TEXINPUTS BIBINPUTS; bibtex xpceusg)

figs/class:
	mkdir -p $@

$(CLHIER):
	swipl -f none --no-packs mkclasshier.pl $@

$(TEX):	$(DOCTOTEX)

dragdropdemo.tex:	examples/dragdrop.pl
	$(PLTOTEX) examples/dragdrop.pl > $@
wysiwyg.tex:		examples/wysiwyg.pl
	$(PLTOTEX) examples/wysiwyg.pl > $@
summary.tex:		figs/class summary.plx classes.pl
	swipl -f none --no-packs  classes.pl summary.plx summary.tmp
	$(DOCTOTEX) summary.tmp > summary.tex
	rm -f summary.tmp

.plx.tex:
	$(DOCTOTEX) $*.plx > $*.tex

.pl.tex:
	$(PLTOTEX) $*.pl > $*.tex

clean::
	rm -rf $(XPCETEX) $(PWTEX) \
		*.out *.dvi *.toc *.aux *.idx *.ind *.blg *.log *.ilg \
		*~ figs/*~ \
		figs/pceclasshierarchy.pdf \
		figs/class \
		$(LOGO) summary.tmp version.tex


distclean: clean
	rm -f $(PDF)

upload:
	rsync -P $(PDF) plweb@oehoe:srv/plweb/data/download/xpce/doc/prolog/userguide.pdf
	rsync -P $(PDF) plweb@us:srv/plweb/data/download/xpce/doc/prolog/userguide.pdf
