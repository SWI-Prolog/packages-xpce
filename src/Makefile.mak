################################################################
# Makefile for XPCE as a SWI-Prolog package for MS-Windows
#
# Author:	Jan Wielemaker
# E-mail:	J.Wielemaker@uva.nl
# Copyright:	University of Amsterdam
# Copying:	LGPL.  See file COPYING or http://www.gnu.org/
#
# This makefile assumes the xpce sources are in the packages
# directory of SWI-Prolog (...\pl\packages).
#
# This makefile is for use with Microsoft NMAKE.  The configuration
# for Unix-based platforms uses GNU autoconf.
#
# Configuration is done in the ...\pl\src\rules.mk file
################################################################

PLHOME=..\..\..
!include $(PLHOME)\src\rules.mk

INCLUDE=$(INCLUDE);..\include
XLIBS=$(UXLIB) $(PLLIB) libjpeg.lib xpm.lib comdlg32.lib

XPCEDLL=xpce.dll
PL2XPCE=pl2xpce
LDFLAGS=$(LDFLAGS) /BASE:0x1010000

all:	$(PL2XPCE).dll xpce-stub.exe

################################################################
# XPCE's modules
################################################################

MODULES=	adt ari evt gnu gra itf ker men fmt msg prg rel \
		rgx txt unx win img box msw

################################################################
# ADT 		--- Abstract Data Types
################################################################

ADTOBJS=	adt\area.obj adt\atable.obj adt\attribute.obj \
		adt\bool.obj adt\chain.obj adt\chaintable.obj \
		adt\constant.obj adt\date.obj adt\dict.obj \
		adt\dictitem.obj adt\hashtable.obj adt\number.obj \
		adt\point.obj adt\real.obj adt\region.obj \
		adt\sheet.obj adt\size.obj adt\tuple.obj \
		adt\vector.obj

################################################################
# ARI 		--- Arithmetic Operations
################################################################

ARIOBJS=	ari\equation.obj ari\expression.obj

################################################################
# EVT 		--- Event Handling Primitives
################################################################

EVTOBJS=	evt\clickgesture.obj evt\conngesture.obj \
		evt\event.obj evt\eventnode.obj evt\eventtree.obj \
		evt\gesture.obj evt\handler.obj evt\handlergroup.obj \
		evt\modifier.obj evt\movegesture.obj \
		evt\mvolgesture.obj evt\popupgesture.obj \
		evt\recogniser.obj evt\resizegesture.obj \
		evt\rzolgesture.obj evt\edittextgest.obj \
		evt\browserselgesture.obj evt\resizetabslice.obj

################################################################
# GNU 		--- GNU-Project Libraries
################################################################

GNUOBJS=	gnu\getdate.obj

gnu\gregex.obj:	gnu\gregex.c
		$(CC) -I. -I $(PLHOME)\include $(CFLAGS) /Dpce_source \
			/Fo$@ gnu\gregex.c

################################################################
# GRA 		--- Graphics Classes
################################################################

GRAOBJS=	gra\arc.obj gra\arrow.obj gra\bitmap.obj gra\box.obj \
		gra\circle.obj gra\colour.obj gra\connection.obj \
		gra\cursor.obj gra\device.obj gra\ellipse.obj \
		gra\figure.obj gra\font.obj gra\format.obj \
		gra\graphical.obj gra\handle.obj gra\image.obj \
		gra\joint.obj gra\line.obj gra\link.obj \
		gra\listbrowser.obj gra\node.obj gra\path.obj \
		gra\postscript.obj gra\scrollbar.obj gra\text.obj \
		gra\tree.obj gra\visual.obj gra\pixmap.obj \
		gra\elevation.obj gra\pen.obj gra\draw.obj \
		gra\colourmap.obj gra\bezier.obj gra\hsv.obj

################################################################
# ITF 		--- Host Interface Layer
################################################################

ITFOBJS=	itf\c.obj itf\host.obj itf\interface.obj \
		itf\cpointer.obj itf\asfile.obj itf\console.obj \
		itf\stub.obj itf\xmalloc.obj itf\iostream.obj \
		itf\srcsink.obj itf\rc.obj itf\hostdata.obj \
		itf\public.obj

################################################################
# KER 		--- Kernel modules
################################################################

KEROBJS=	ker\alloc.obj ker\assoc.obj ker\behaviour.obj \
		ker\class.obj ker\conversion.obj ker\debug.obj \
		ker\declarations.obj ker\error.obj ker\gc.obj \
		ker\getmethod.obj ker\glob.obj ker\global.obj \
		ker\goodies.obj ker\passing.obj ker\method.obj \
		ker\name.obj ker\object.obj ker\programobject.obj \
		ker\save.obj ker\self.obj ker\sendmethod.obj \
		ker\srclocation.obj ker\timer.obj ker\trace.obj \
		ker\type.obj ker\variable.obj ker\xref.obj \
		ker\classvar.obj ker\inline.obj

ker\name.obj:	h\names.ic h\names.ih

################################################################
# MEN 		--- Menu (Dialog) items
################################################################

MENOBJS=	men\button.obj men\dialogitem.obj men\label.obj \
		men\menu.obj men\menubar.obj men\menuitem.obj \
		men\popup.obj men\slider.obj men\textitem.obj \
		men\tab.obj men\diagroup.obj men\tabstack.obj \
		men\labelbox.obj men\intitem.obj

################################################################
# FMT 		--- Layout managers
################################################################

FMTOBJS=	fmt\layoutmgr.obj fmt\layoutitf.obj \
		fmt\table.obj fmt\tabcell.obj fmt\tabslice.obj

################################################################
# BOX 		--- Typesetting stuff
################################################################

BOXOBJS=	box\boxes.obj box\hbox.obj box\tbox.obj \
		box\parbox.obj box\grbox.obj box\rubber.obj \
		box\lbox.obj

################################################################
# MSG 		--- Executable (message) Objects
################################################################

MSGOBJS=	msg\and.obj msg\assign.obj msg\binding.obj \
		msg\block.obj msg\code.obj msg\create.obj \
		msg\equal.obj msg\function.obj msg\if.obj \
		msg\message.obj msg\nonequal.obj msg\not.obj \
		msg\obtain.obj msg\or.obj msg\progn.obj msg\quote.obj \
		msg\var.obj msg\when.obj msg\while.obj \
		msg\nameref.obj

################################################################
# PRG 		--- Language Definition Classes
################################################################

PRGOBJS=	prg\operator.obj prg\parser.obj prg\tokeniser.obj

################################################################
# REL 		--- Relation Classes
################################################################

RELOBJS=	rel\constraint.obj rel\hyper.obj rel\identity.obj \
		rel\relation.obj rel\spatial.obj

################################################################
# RGX		--- Henry Spencer's regex library
################################################################

RGXOBJS=	rgx\regcompW.obj rgx\regexecW.obj \
		rgx\regfree.obj rgx\regerror.obj

REGCOBJS=	rgx\regcomp.c \
		rgx\regc_lex.c rgx\regc_color.c rgx\regc_nfa.c \
		rgx\regc_cvec.c rgx\regc_locale.c

rgx\regcompA.obj:	$(REGCOBJS)
		$(CC) -I. -c $(CFLAGS) rgx\regcomp.c /Fo$@
rgx\regcompW.obj:	$(REGCOBJS)
		$(CC) -I. -c $(CFLAGS) -DREG_WIDE rgx\regcomp.c /Fo$@
rgx\regexecA.obj:	rgx\regexec.c
		$(CC) -I. -c $(CFLAGS) rgx\regexec.c /Fo$@
rgx\regexecW.obj:	rgx\regexec.c
		$(CC) -I. -c $(CFLAGS) -DREG_WIDE rgx\regexec.c /Fo$@
rgx\regfree.obj:	rgx\regfree.c
		$(CC) -I. -c $(CFLAGS) -DREG_WIDE rgx\regfree.c /Fo$@
rgx\regerror.obj:	rgx\regerror.c
		$(CC) -I. -c $(CFLAGS) -DREG_WIDE rgx\regerror.c /Fo$@


################################################################
# TXT 		--- Text Representation and Manipulation Classes
################################################################

TXTOBJS=	txt\chararray.obj txt\editor.obj txt\fragment.obj \
		txt\keybinding.obj txt\regex.obj txt\str.obj \
		txt\string.obj txt\style.obj txt\syntax.obj \
		txt\textbuffer.obj txt\textcursor.obj \
		txt\textimage.obj txt\textmargin.obj txt\undo.obj \
		txt\utf8.obj txt\i18n.obj

################################################################
# UNX 		--- Unix File, Process and Network Classes
################################################################

UNXOBJS=	unx\directory.obj unx\file.obj unx\process.obj \
		unx\socket.obj unx\stream.obj

################################################################
# WIN 		--- Windows and Frames
################################################################

WINOBJS=	win\browser.obj win\decorate.obj win\dialog.obj \
		win\display.obj win\displaymgr.obj win\frame.obj \
		win\picture.obj win\setup.obj win\tile.obj \
		win\view.obj win\window.obj win\application.obj \
		win\tileadjust.obj win\monitor.obj

################################################################
# IMG 		--- Platform independent low-level image stuff
################################################################

IMGOBJS=	img\jdatasrc.obj img\jdatadst.obj img\imgutil.obj \
		img\gifread.obj img\giftoxpm.obj img\gifwrite.obj

################################################################
# MSW: The MS-Windows binding
################################################################

MSWOBJS=	msw\mscolour.obj msw\msevent.obj msw\msmenu.obj \
		msw\msreadimage.obj \
		msw\mscommon.obj msw\msfont.obj msw\msmetafile.obj \
		msw\msstream.obj msw\mscursor.obj \
		msw\msframe.obj msw\msppm.obj msw\mstimer.obj \
		msw\msdisplay.obj msw\msimage.obj \
		msw\msprinter.obj msw\mswin.obj \
		msw\msdraw.obj msw\msjpeg.obj msw\msprocess.obj \
		msw\mswindow.obj

################################################################
# Join all the objects
################################################################

OBJECTS=	$(ADTOBJS) \
		$(ARIOBJS) \
		$(EVTOBJS) \
		$(GNUOBJS) \
		$(GRAOBJS) \
		$(ITFOBJS) \
		$(KEROBJS) \
		$(MENOBJS) \
		$(FMTOBJS) \
		$(BOXOBJS) \
		$(MSGOBJS) \
		$(PRGOBJS) \
		$(RELOBJS) \
		$(RGXOBJS) \
		$(TXTOBJS) \
		$(UNXOBJS) \
		$(WINOBJS) \
		$(IMGOBJS) \
		$(MSWOBJS)

$(XPCEDLL):	$(OBJECTS)
		$(LD) $(LDFLAGS) /out:$@ /dll $(OBJECTS) $(LIBS) $(XLIBS)

################################################################
# Names
################################################################

prepare:	h\names.ih

NAMESRC=	$(OBJECTS:.obj=.c) gra\graphstate.c

h\names.ih:	find_names.exe
		find_names.exe h/names.ic h/names.ih -- h\*.h $(NAMESRC)

find_names.exe: find_names.obj
		$(LD) $(LDFLAGS) /out:$@ /subsystem:console \
			find_names.obj setargv.obj $(LIBS)


################################################################
# Build SWI-Prolog interface
################################################################

PLOBJ=		$(OBJECTS) ..\pl\src\interface.obj ..\pl\src\pcecall.obj

$(PL2XPCE).dll:	prepare $(PLOBJ)
		@echo Linking $@ ...
		@$(LD) $(LDFLAGS) /out:$@ /dll $(PLOBJ) $(LIBS) $(XLIBS)

################################################################
# Stand-alone toplevel (xpce.exe)
################################################################

xpce-stub.exe:	xpce-stub.res xpce-stub.obj
		$(LD) $(LDFLAGS) /subsystem:windows /out:$@ xpce-stub.obj $(PLLIB) xpce-stub.res $(LIBS)

xpce-stub.res:	..\pl\src\xpce-stub.rc ..\pl\src\xpce.ico
		$(RSC) /fo$@ ..\pl\src\xpce-stub.rc

xpce-stub.obj:	..\pl\src\xpce-stub.c
		@$(CC) -I $(PLHOME)\include $(CFLAGS) /Fo$@ ..\pl\src\xpce-stub.c

################################################################
# Installation program
################################################################

xpce-install:	xpce-install.exe

# NOTE: setargv.obj ensures main() is fet with expanded arguments (see
# MSVC documentation).

xpce-install.exe: xpce-install.obj
		$(LD) /out:$@ /subsystem:console \
			xpce-install.obj setargv.obj $(LIBS)

################################################################
# Installation
################################################################

IBASE=	$(PLBASE)\xpce
IDIRS=	appl-help \
	bitmaps \
	bitmaps\16x16 \
	bitmaps\32x32 \
	bitmaps\patterns \
	man \
	man\reference \
	man\reference\class \
	prolog \
	prolog\boot \
	prolog\contrib \
	prolog\contrib\rubik \
	prolog\demo \
	prolog\lib \
	prolog\lib\compatibility \
	prolog\lib\dialog \
	prolog\lib\dialog\bitmaps \
	prolog\lib\doc \
	prolog\lib\doc\icons \
	prolog\lib\draw \
	prolog\lib\emacs \
	prolog\lib\english \
	prolog\lib\http \
	prolog\lib\man \
	prolog\lib\math \
	prolog\lib\plot \
	prolog\lib\xref \
	prolog\lib\swi \
	prolog\lib\trace \
	prolog\lib\trace\icons \
	prolog\lib\trace\icons\16x16

MANINDEX=$(IBASE)\man\reference\index.obj

README=	ChangeLog \
	Defaults \
	Defaults.user \
	INFO \
	README \
	VERSION

INSTALL=xpce-install.exe -n

ITRG=	xpce-install.exe \
	ibindir idirs idll ilib irc iindex imanidx ireadme ixpce-stub \
	classindex

install:	$(ITRG)

html-install::

ibindir::
		@if not exist "$(BINDIR)\$(NULL)" mkdir "$(BINDIR)"

idirs::
		@for %d in ($(IDIRS)) do \
		  @if not exist "$(IBASE)\%d\$(NULL)" mkdir "$(IBASE)\%d"

idll::
		$(INSTALL) $(PL2XPCE).dll "$(BINDIR)"
!IF "$(DBG)" == "true"
		$(INSTALL) $(PL2XPCE).pdb "$(BINDIR)"
!ENDIF


ixpce-stub:	xpce-stub.exe
		$(INSTALL) xpce-stub.exe "$(BINDIR)"

ilib::
		@for %d in ($(IDIRS)) do \
		  @echo Installing files in %d ... & \
		  $(INSTALL) ..\%d\* "$(IBASE)\%d"

iindex::
		chdir "$(IBASE)\prolog\lib" & \
		  "$(PLBASE)\bin\swipl.exe" \
			-f none -F none \
			-g make_library_index('.') \
			-t halt

classindex::
		chdir "$(IBASE)\prolog\lib" & \
		  "$(PLBASE)\bin\swipl-win.exe" \
			-f none \
			-g pce_make_library_index('.') \
			-t halt
irc::
		$(INSTALL) ..\pl\src\swipl-rc "$(PLBASE)\swipl-win.rc"

ireadme::
		$(INSTALL) -C .. $(README) "$(IBASE)"

################################################################
# Manual index
################################################################

imanidx:	"$(MANINDEX)"

"$(MANINDEX)":	..\man\reference\*.doc ..\man\reference\class\*.doc
		chdir "$(IBASE)\man\reference" & \
		"$(PLBASE)\bin\swipl-win.exe" \
		  -g "[library('man/man_index')],pce_make_manual_index('index.obj')" \
		  -t halt

################################################################
# Uninstalling
################################################################

uninstall::
		del $(PLBASE)\bin\pl2xpce.dll
		del $(PLBASE)\swipl.rc
		rmdir /s /d $(PLBASE)\xpce

################################################################
# Cleanup
################################################################

clean::
		@echo off & for %d in ($(MODULES)) do \
		  $(CMD) /c "chdir %d & if exist *.obj del *.obj"
		@echo off & for %d in ($(MODULES)) do \
		  $(CMD) /c "chdir %d & if exist *~ del *~"
		$(CMD) /c "chdir ..\pl\src & if exist *.obj del *.obj"
		$(CMD) /c "chdir ..\pl\src & if exist *~ del *~"
		if exist xpce-install.exe del xpce-install.exe
		if exist find_names.exe del find_names.exe
		if exist *.obj del *.obj
		if exist *~ del *~

distclean:	clean
		-del *.dll *.lib *.exp *.dbg *.ilk *.pdb 2>nul

