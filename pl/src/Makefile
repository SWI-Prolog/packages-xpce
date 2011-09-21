#   Makefile for SWI-Prolog <-> PCE interface
#
################################################################
# NOTE NOTE NOTE
#
# This Makefile is normally started from the Makefile in the build-directory.
# The following parameters are passed into it:
#
# $(PL)		SWI-Prolog executable
# $(PLBASE)	SWI-Prolog's home directory
# $(PLARCH)	SWI-Prolog's architecture identifier
# $(PLINCL)     SWI-Prolog's include files
# $(builddir)	Path from XPCE home to build directory
# $(C?FLAGS)	The various C-compiler flags
# $(SOEXTRAOBJ)	Possibly non-standard .o files for creating pl2xpce.so
# $(GCCLIB)	The path to -lgcc
# $(SO)		Extension for shared libraries
################################################################

.SUFFIXES: .qlf .pl

LIBDIR=		../../lib/$(PLARCH)
libXPCE=	$(LIBDIR)/libXPCE.a
BINDIR=		../../$(builddir)
OBJDIR=		../../$(builddir)/pl
CONFIG=		../../$(builddir)/config.h

CPLIFLAGS=	-I$(PLINCL) -I../../$(builddir) -I../../src
PL=		pl
PLLD=		plld -pl $(PL)
ITFOBJ=		$(addprefix $(OBJDIR)/, interface.o link.o pcecall.o)
ITFSRC=		interface.c table.c
SOITF=		$(addprefix $(OBJDIR)/, so-interface.o pcecall.o)
OBJ=		$(ITFOBJ)
XPCE=		$(BINDIR)/xpce
QLF=		$(BINDIR)/xpce.qlf
XPCESO=		$(LIBDIR)/pl2xpce.$(SO)
SOEXTR=		$(addprefix $(OBJDIR)/, $(SOEXTRAOBJ))
CITFFLAGS=	-DSWI -DHAVE_CONFIG_H

CFLAGS=		$(COFLAGS) $(CWFLAGS) $(CMFLAGS) $(CPLIFLAGS) $(CITFFLAGS)

PCEPLS=		pce_expand.pl ../lib/english/pce_messages.pl pce_pl.pl \
		pce_principal.pl pce_error.pl pce_autoload.pl pce_global.pl \
		pce_expansion.pl pce_realise.pl pce_editor.pl \
		pce_goal_expansion.pl
BOOTQLF=	pce.qlf
LIBQLFS=	pce_manual.qlf dialog/dialog.qlf emacs/emacs.qlf pcedraw.qlf

################################################################
# The main targets
################################################################

soall:		$(XPCESO)

common:		$(PLBASE)/pl.rc $(PLBASE)/xpce.rc $(PLBASE)/xpce \
		$(PLBASE)/Makefile \
		$(PLBASE)/xpce/prolog/lib/INDEX.pl

################################################################
# Objects for $(OBJDIR)
################################################################

$(SOITF):	$(OBJDIR)
$(OBJDIR):
		mkdir -p $@

$(OBJDIR)/interface.o:	$(ITFSRC)
		$(CC) -c $(CFLAGS) $< -o $@
$(OBJDIR)/link.o:	link.c
		$(CC) -c $(CFLAGS) $< -o $@
$(OBJDIR)/pcecall.o:	pcecall.c
		$(CC) -c $(CFLAGS) $(XINCLUDES) $< -o $@

################################################################$
# Creating pl2xpce.so
################################################################

$(OBJDIR)/so-interface.o: interface.c
		$(CC) -DO_SHAREDLIBRARY $(CFLAGS) -c $< -o $@

$(XPCESO):	$(SOITF) $(libXPCE) $(SOEXTR)
		$(PLLD) -shared $(LDSOFLAGS) $(SOITF) $(SOEXTR) -L$(LIBDIR) \
		-lXPCE $(XLIBS) $(NETLIBS) -o $(XPCESO);

################################################################$
# QLF generation
################################################################

libqlfs:	$(addprefix ../../prolog/lib/, $(LIBQLFS))
bootqlf:	$(addprefix ../../prolog/lib/, $(BOOTQLF))

../../prolog/lib/pce.qlf: $(addprefix ../../prolog/boot/, $(PCEPLS))
		$(PL) -g "qcompile('../../prolog/lib/pce.pl')" -t halt

.pl.qlf:
		$(PL) -g "pce_welcome,qcompile('$*')" -t halt

################################################################
# Link xpce to the SWI-Prolog library directory
################################################################

$(PLBASE)/pl.rc:	plrc
	cp plrc $@

$(PLBASE)/xpce.rc:	plrc
	cp plrc $@

$(PLBASE)/xpce:
	(cd $(PLBASE); rm -f xpce; ln -s $(PCEHOME) xpce)

$(PLBASE)/xpce/lib/$(PLARCH):
	(cd ../../lib; rm -f $(PLARCH); ln -s ../$(builddir) $(PLARCH))

$(PLBASE)/Makefile: Makefile.bin
	-cp Makefile.bin $@

$(PLBASE)/xpce/prolog/lib/INDEX.pl:
	$(PL) -g "make_library_index('$(PLBASE)/xpce/prolog/lib')" -t halt

################################################################
# Cleanup
################################################################

clean:
		$(RM) -f *.o core a.out shitf.c *~

distclean:	clean
		$(RM) -f xpce *.qlf mangle domangle ctr0.o *.so
