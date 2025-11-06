.SUFFIXES: .tex .dvi .doc .pl .eps .pdf

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

COMMON=	intro.tex starting.tex globalarch.tex online.tex debug.tex \
	interface.tex graphics.tex pceprolog.tex memory.tex \
	glossary.tex techniques.tex report.tex font.tex hyper.tex \
	udc.tex dragdrop.tex dragdropdemo.tex control.tex whoiswho.tex \
	namedref.tex ugraphics.tex code.tex classvar.tex diaedit.tex \
	dialog.tex wysiwyg.tex frame.tex error.tex application.tex \
	resource.tex images.tex summary.tex body.tex driver.tex \
	mswin.tex balloon.tex printing.tex threads.tex status.tex \
	libs.tex findfile.tex toc.tex tabular.tex libdia.tex mlingual.tex \
	httpd.tex httpd-ex.tex docrender.tex libplot.tex

XPCETEX=	xpceusg.tex $(COMMON)

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
summary.tex:		figs/class summary.doc classes.pl
	swipl -f none --no-packs  classes.pl summary.doc summary.tmp
	$(DOCTOTEX) summary.tmp > summary.tex
	rm -f summary.tmp

.doc.tex:
	$(DOCTOTEX) $*.doc > $*.tex

.pl.tex:
	$(PLTOTEX) $*.pl > $*.tex

clean::
	rm -rf $(XPCETEX) $(PWTEX) \
		*.out *.dvi *.toc *.aux *.idx *.ind *.blg *.log *.ilg \
		*~ figs/*~ \
		figs/pceclasshierarchy.pdf \
		figs/class \
		$(LOGO) summary.tmp


distclean: clean
	rm -f $(PDF)

upload:
	rsync -P $(PDF) plweb@oehoe:srv/plweb/data/download/xpce/doc/prolog/userguide.pdf
