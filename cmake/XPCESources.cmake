set(ADT_SRC	area.c atable.c attribute.c bool.c chain.c chaintable.c
		constant.c date.c dict.c dictitem.c hashtable.c number.c
		point.c real.c region.c sheet.c size.c tuple.c vector.c)

set(ARI_SRC	equation.c expression.c)

set(EVT_SRC	clickgesture.c conngesture.c event.c
		eventnode.c eventtree.c gesture.c handler.c
		handlergroup.c modifier.c movegesture.c
		mvolgesture.c popupgesture.c recogniser.c
		resizegesture.c rzolgesture.c edittextgest.c
		browserselgesture.c resizetabslice.c)

set(GNU_SRC	getdate.c)

set(GRA_SRC	arc.c arrow.c bitmap.c box.c circle.c colour.c
		connection.c cursor.c device.c ellipse.c figure.c
		font.c format.c graphical.c handle.c image.c
		joint.c line.c link.c listbrowser.c node.c path.c
		postscript.c scrollbar.c text.c tree.c visual.c
		pixmap.c elevation.c pen.c draw.c colourmap.c
		bezier.c hsv.c)

set(ITF_SRC	c.c host.c interface.c cpointer.c asfile.c console.c
		stub.c xmalloc.c iostream.c srcsink.c rc.c hostdata.c
		public.c)

set(KER_SRC	alloc.c assoc.c behaviour.c class.c conversion.c
		debug.c declarations.c error.c gc.c
		getmethod.c glob.c global.c goodies.c passing.c
		method.c name.c object.c programobject.c save.c
		self.c sendmethod.c srclocation.c timer.c
		trace.c type.c variable.c xref.c classvar.c inline.c)

set(MEN_SRC	button.c dialogitem.c label.c menu.c menubar.c
		menuitem.c popup.c slider.c textitem.c tab.c diagroup.c
		tabstack.c labelbox.c intitem.c)

set(FMT_SRC	layoutmgr.c layoutitf.c
		table.c tabcell.c tabslice.c)

set(BOX_SRC	boxes.c hbox.c tbox.c parbox.c grbox.c rubber.c
		lbox.c)

set(MSG_SRC	and.c assign.c binding.c block.c code.c create.c
		equal.c function.c if.c message.c nonequal.c
		not.c obtain.c or.c progn.c quote.c var.c when.c while.c
		nameref.c)

set(PRG_SRC	operator.c parser.c tokeniser.c)

set(REL_SRC	constraint.c hyper.c identity.c relation.c
		spatial.c)

set(RGX_SRC	regcompW.c regexecW.c
		regfree.c regerror.c)

set(TXT_SRC	chararray.c editor.c fragment.c keybinding.c
		regex.c str.c string.c style.c syntax.c
		textbuffer.c textcursor.c textimage.c
		textmargin.c undo.c utf8.c i18n.c)

set(UNX_SRC	directory.c file.c process.c socket.c stream.c)

set(WIN_SRC	browser.c decorate.c dialog.c display.c
		displaymgr.c frame.c picture.c tileadjust.c
		setup.c tile.c view.c window.c application.c
		monitor.c)

set(IMG_SRC	jdatasrc.c jdatadst.c jpegtoxpm.c gifread.c giftoxpm.c
		gifwrite.c imgutil.c)

if(WIN32)
set(MSW_SRC	mscolour.c mscursor.c msdisplay.c msdraw.c msevent.c
		msfont.c msframe.c msimage.c msstream.c mstimer.c
		mswindow.c msmenu.c mswin.c msppm.c msprinter.c
		mscommon.c msmetafile.c msreadimage.c msjpeg.c
		msprocess.c)
else()
set(X11_SRC	canvas.c fshell.c xcommon.c xconvert.c x11-compat.c xppm.c
		xcolour.c xcursor.c xdisplay.c xdraw.c xevent.c xfont.c
		xframe.c ximage.c xstream.c xtimer.c xwindow.c x11.c xmenu.c
		xdnd.c xunix.c xjpeg.c)
endif(WIN32)

set(XPCE_SUBDIRS adt ari evt gnu gra itf ker men fmt box msg prg rel rgx
		 txt unx win img)
if(WIN32)
  set(XPCE_SUBDIRS ${XPCE_SUBDIRS} msw)
else()
  set(XPCE_SUBDIRS ${XPCE_SUBDIRS} x11)
endif()

set(XPCE_SOURCES)
foreach(d ${XPCE_SUBDIRS})
  string(TOUPPER ${d} ud)
  prepend(${ud}_SRC src/${d} ${${ud}_SRC})
  set(XPCE_SOURCES ${XPCE_SOURCES} ${${ud}_SRC})
endforeach()

################
# SWI-Prolog interface

set(SWIPL_SRC interface.c link.c pcecall.c)
prepend(SWIPL_SRC swipl/ ${SWIPL_SRC})

set(HOST_INTERFACE_SOURCES ${SWIPL_SRC})
