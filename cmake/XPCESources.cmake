################
# C sources
################

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
		msprocess.c msuxnt.c)
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

set(SWIPL_SRC interface.c pcecall.c)
prepend(SWIPL_SRC swipl/ ${SWIPL_SRC})

set(HOST_INTERFACE_SOURCES ${SWIPL_SRC})

################
# Prolog libraries
################

set(XPCE_DATA_DIRS
    prolog/boot

    prolog/lib prolog/lib/compatibility prolog/lib/dialog prolog/lib/doc
    prolog/lib/draw prolog/lib/emacs prolog/lib/english prolog/lib/http
    prolog/lib/man prolog/lib/math prolog/lib/plot prolog/lib/swi
    prolog/lib/trace prolog/lib/trace/icons prolog/lib/trace/icons/16x16
    prolog/lib/xref prolog/lib/dialog/bitmaps prolog/lib/doc/icons

    prolog/contrib
    prolog/contrib/rubik

    prolog/demo

    bitmaps bitmaps/16x16 bitmaps/32x32 bitmaps/patterns

    appl-help

    man man/faq man/reference man/reference/class)

set(XPCE_DATA_prolog_boot pce_autoload.pl pce_editor.pl pce_error.pl
    pce_expand.pl pce_expansion.pl pce_global.pl pce_goal_expansion.pl
    pce_keybinding.pl pce_pl.pl pce_portray.pl pce_principal.pl
    pce_realise.pl)

set(XPCE_DATA_prolog_lib area.pl autowin.pl dragdict.pl dragdrop.pl
    draw_extend.pl edit_dialog.pl emacs_extend.pl emacs_tags.pl file_item.pl
    find_file_dialog.pl find_file.pl getpass.pl gradient.pl gui_tracer.pl
    help_message.pl http_client.pl hyper.pl imageops.pl isocomp.pl
    keybinding.pl make_dialog.pl MKINDEX.pl Overview
    password_item.pl pce_arm.pl pce_arrow_item.pl pce_class_index.pl
    pce_colour_item.pl pce_configeditor.pl pce_config.pl pce_cxx_headers.pl
    pce_debug.pl pce_dispatch.pl pce_drag_and_drop_dict_item.pl
    pce_drag_and_drop.pl pcedraw.pl pce_editable_text.pl pce_edit.pl
    pce_emacs.pl pce_float_item.pl pce_font_item.pl pce_grapher.pl
    pce_graphical_browser.pl pce_helper.pl pce_help_file.pl pce_history.pl
    pce_identifier_item.pl pce_image_browser.pl pce_image_item.pl
    pce_image.pl pce_loadcxx.pl pce_main.pl pce_manual.pl pce_meta.pl
    pce_objects.pl pce.pl pce_postscript.pl pce_progress.pl
    pce_prolog_xref.pl pce_prompter.pl pce_regex_compat.pl pce_renew.pl
    pce_report.pl pce_require.pl pce_selection.pl pce_select_set_item.pl
    pce_server.pl pce_set_item.pl pce_shell.pl pce_style_item.pl
    pce_tagged_connection.pl pce_template.pl pce_tick_box.pl pce_toc.pl
    pce_type.pl pce_unclip.pl pce_util.pl pce_xref.pl persistent_frame.pl
    portray_object.pl print_graphics.pl print_text.pl
    prolog_predicate_item.pl prolog_predicate.pl qrecompile.pl
    scaledbitmap.pl scan_arguments.pl sp_fcompile.pl splash_screen.pl
    stayup_popup.pl swi_compatibility.pl swi_edit.pl
    swi_hooks.pl swi_ide.pl swi_preferences.pl tabbed_window.pl tabular.pl
    toc_filesystem.pl toolbar.pl url_image.pl Xserver.pl)

set(XPCE_DATA_prolog_lib_compatibility bitmap.pl event_speak.pl frozen.pl
    global.pl resource.pl send.pl)

set(XPCE_DATA_prolog_lib_doc browser.pl emit.pl form.pl html.pl
    layout.pl load.pl objects.pl README sp_errors.pl t2.pl table.pl test.pl
    url_fetch.pl util.pl vfont.pl window.pl xml_browse.pl xml_hierarchy.pl)

set(XPCE_DATA_prolog_lib_doc_icons back.xpm forward.xpm reload.xpm
    source.xpm)

set(XPCE_DATA_prolog_lib_dialog attribute.pl behaviour.pl dialog.pl
    font.pl generate.pl image_item.pl label.pl layout.pl load.pl menuitem.pl
    meta.pl pretty_print.pl prompter.pl proto.pl README util.pl)

set(XPCE_DATA_prolog_lib_dialog_bitmaps button.bm choice.bm cycle.bm
    editor.bm image.bm label.bm list.bm reporter.bm slider.bm text_item.bm
    toggle.bm)

set(XPCE_DATA_prolog_lib_draw align.pl attribute.pl canvas.pl config.pl draw.pl
    exportpl.pl gesture.pl importpl.pl menu.pl README shapes.pl undo.pl)

set(XPCE_DATA_prolog_lib_emacs annotate_mode.pl application.pl bookmarks.pl
    buffer_menu.pl buffer.pl chr_mode.pl c_mode.pl cpp_mode.pl dde_server.pl
    emacs_chrome_server.pl emacs.pl find.pl fundamental_mode.pl gdb.pl
    help.pl history.pl hit_list.pl html_mode.pl java_mode.pl xsb_mode.pl
    javascript_mode.pl language_mode.pl latex_mode.pl logtalk_mode.pl
    man_mode.pl outline_mode.pl prolog_mode.pl prompt.pl rdf_mode.pl
    script_mode.pl server.pl sgml_mode.pl shell.pl swi_prolog.pl
    text_mode.pl turtle_mode.pl window.pl yaml_mode.pl cmake_mode.pl)

set(XPCE_DATA_prolog_lib_english pce_messages.pl)

set(XPCE_DATA_prolog_lib_http html_hierarchy.pl html_refman.pl html_write.pl
    httpd.pl http_image.pl http_man.pl README run.pl xref.pl)

set(XPCE_DATA_prolog_lib_man behaviour_item.pl classification.dat classmap.pl
    man_index.pl p_card.pl pce_op.pl p_data.pl showevent.pl util.pl
    v_card.pl v_class.pl v_editor.pl v_error.pl v_global.pl v_group.pl
    v_hierarchy.pl v_inherit.pl v_inspector.pl v_instance.pl v_manual.pl
    v_module.pl v_search.pl v_select.pl v_statistics.pl v_summary.pl
    v_tile.pl v_topic.pl v_visual.pl)

set(XPCE_DATA_prolog_lib_math expandmath.pl)

set(XPCE_DATA_prolog_lib_plot axis.pl barchart.pl demo.pl plotter.pl README)

set(XPCE_DATA_prolog_lib_swi pce_debug_monitor.pl pce_profile.pl swi_console.pl
    thread_monitor.pl)

set(XPCE_DATA_prolog_lib_trace browse.pl browse_xref.pl clause.pl
    emacs_debug_modes.pl exceptions.pl gui.pl pltracer.hlp pprint.pl
    query.pl README settings.pl source.pl stack.pl status.pl test.pl
    trace.pl util.pl viewterm.pl)

set(XPCE_DATA_prolog_lib_trace_icons abort.xpm breakpoint.xpm break.xpm bug.xpm
    builtin.xpm butterfly.xpm call.xpm classext.xpm classvar.xpm class.xpm
    closedir.xpm creep.xpm debug.xpm details.xpm det.xpm
    down.xpm dynamic.xpm dyn.xpm edit.xpm except.xpm exit.xpm export.xpm
    eyes.xpm fact.xpm fail.xpm finish.xpm foreign.xpm get.xpm grammar.xpm
    import.xpm interactor.xpm interrupt.xpm into.xpm ivar.xpm leap.xpm
    list.xpm loadfailed.xpm loading.xpm locate.xpm meta.xpm mini-globe.xpm
    mini-run.xpm module.xpm ndet.xpm nodebug.xpm nospy.xpm nostopspy.xpm
    nostop.xpm opendir.xpm openmodule.xpm plfile.xpm plincludedfile.xpm
    plloadedfile.xpm pred.xpm redo.xpm retry.xpm send.xpm skip.xpm spy.xpm
    stack.xpm stop.xpm undefined.xpm undefpred.xpm unrefpred.xpm up.xpm
    user.xpm warnpred.xpm)

set(XPCE_DATA_prolog_lib_trace_icons_16x16 butterfly.xpm dbgsettings.xpm)

set(XPCE_DATA_prolog_lib_xref common.pl mkcommon.pl quintus.pl sicstus.pl)

set(XPCE_DATA_prolog_contrib contrib.pl README)

set(XPCE_DATA_prolog_contrib_rubik maplist.pl README rubikpce.pl rubik.pl)

set(XPCE_DATA_prolog_demo chess.pl colour.pl constraint.pl cursor.pl
    event_hierarchy.pl fontviewer.pl ftplog.pl graph.pl hsvcolour.pl
    imageviewer.pl ispell.pl juggler.pl kangaroo.pl pce_demo.pl)

set(XPCE_DATA_bitmaps bishop.bm box.bm bullet.bm bullseye.bm busy_bee.bm
    cassette.bm chessboard.bm chesssquare.bm close.bm closedir.xpm
    concept.bm conceptLink.bm confirm.bm confirm_name.bm console_tile.bm
    creating.bm cross.bm cycle.bm desktop.bm dir.bm domain.bm
    draw_cconnect.bm draw_connect.bm draw_edit.bm draw_line.bm draw_path.bm
    draw_proto.bm draw_resize.bm draw_text.bm ellipse.bm fatleft_arrow.bm
    fatright_arrow.bm file.bm folder.bm fragment.bm globe.bm go.bm group.bm
    hand.bm happy.bm help.bm hierarchy.bm hourgl10.bm hourgl1.bm hourgl2.bm
    hourgl3.bm hourgl4.bm hourgl5.bm hourgl6.bm hourgl7.bm hourgl8.bm
    hourgl9.bm hourgl.bm juggler1.bm juggler2.bm juggler3.bm juggler4.bm
    juggler5.bm kangro10.bm kangro11.bm kangro1.bm kangro2.bm kangro3.bm
    kangro4.bm kangro5.bm kangro6.bm kangro7.bm kangro8.bm kangro9.bm
    king.bm knight.bm left_arrow.bm line.bm link.bm linking.bm magnify.xpm
    main_link.bm mark.bm ms_down_arrow.bm ms_left_arrow.bm ms_right_arrow.bm
    ms_up_arrow.bm nomark.bm nosticky.bm note.bm off_marked.bm off_toggle.bm
    ol_cycle.bm ol_pulldown.bm ol_pullright.bm on_marked.bm on_toggle.bm
    opendir.xpm other_link.bm paste.bm pawn.bm pce16.xpm pce.bm
    pinned.xpm pin.xpm printer.bm queen.bm question.bm README right_arrow.bm
    rook_64.bm rook.bm sad.bm select.bm selecting.bm slant_left.bm
    slant_right.bm sticky.bm support.bm text.bm textedit.bm thermo.bm
    toggle_off.bm toggle_on.bm transcript.bm trash.bm typing.bm web.bm)

set(XPCE_DATA_bitmaps_16x16 alert.xpm arrow_length.xpm arrows.bm
    arrow_wing.xpm back.xpm binocular.xpm book2.xpm bookmarks.xpm
    builtin_classflash.xpm builtin_class.xpm closedir.xpm copy.xpm
    cpalette1.xpm cpalette2.xpm cut.xpm delete.xpm distribute.xpm doc.xpm
    done.xpm down.xpm drawing.xpm drive.xpm duplicate.xpm edit.xpm error.xpm
    exclamation.xpm eye.xpm false.xpm fatleft_arrow.xpm fatright_arrow.xpm
    fillpattern.bm font.xpm foot.xpm forward.xpm funcdoc.xpm ghost.xpm
    graph.xpm handpoint.xpm help.xpm hierarchy.xpm manual.xpm newdir.xpm
    new.xpm noimg.xpm note.xpm ok.xpm opendir.xpm open.xpm paste.xpm pce.xpm
    pen.xpm preddoc.xpm print.xpm profiler.xpm redo.xpm refresh.xpm
    saveall.xpm save.xpm stop.xpm trashcan.xpm undo.xpm up.xpm
    user_classflash.xpm user_class.xpm user.xpm valign.xpm
    vcr_fast_forward.xpm vcr_forward.xpm vga16.xpm wipeall.xpm)

set(XPCE_DATA_bitmaps_32x32 books.xpm buffers.xpm dbgsettings.xpm
    doc_pl.xpm doc_x.xpm drawing.xpm help.xpm pensil.xpm viewer.xpm
    vishier.xpm)

set(XPCE_DATA_bitmaps_patterns Arches.xbm Balls.xbm Bats.xbm brick_2.xbm
    brick_block.xbm brick_cobble.xbm brick_diag_block.xbm
    brick_diag_hering.xbm brick_diag.xbm brick_hering.xbm brick_overlap.xbm
    brick_pat1.xbm bricks_weave.xbm brick.xbm Bumps.xbm Carpet.xbm
    check_stag.xbm check.xbm chev_box.xbm chev_circle.xbm chev_same.xbm
    chev_stag.xbm chev_swap.xbm Circle_hex.xbm Circles.xbm Circle_tile.xbm
    Crabcan.xbm Diamonds.xbm dots_big_diag.xbm dots_big.xbm
    dots_diag_wide.xbm dots_diag.xbm dots_hex.xbm dots_open_diag.xbm
    dots_rand2.xbm dots_rand.xbm dots_skew2.xbm dots_skew.xbm dots_stag.xbm
    dots_wide.xbm dots.xbm fence_chain_half.xbm fence_chain.xbm
    fence_knitting.xbm fence_link1.xbm fence_link2.xbm fence_sqknot.xbm
    Fish_escher.xbm Fishes.xbm Grey_32_00.xbm Grey_32_01.xbm Grey_32_02.xbm
    Grey_32_03.xbm Grey_32_04.xbm Grey_32_05.xbm Grey_32_06.xbm
    Grey_32_07.xbm Grey_32_08.xbm Grey_32_09.xbm Grey_32_10.xbm
    Grey_32_11.xbm Grey_32_12.xbm Grey_32_13.xbm Grey_32_14.xbm
    Grey_32_15.xbm Grey_32_16.xbm Grey_32_17.xbm Grey_32_18.xbm
    Grey_32_19.xbm Grey_32_20.xbm Grey_32_21.xbm Grey_32_22.xbm
    Grey_32_23.xbm Grey_32_24.xbm Grey_32_25.xbm Grey_32_26.xbm
    Grey_32_27.xbm Grey_32_28.xbm Grey_32_29.xbm Grey_32_30.xbm
    Grey_32_31.xbm Grey_32_32.xbm Grey_9_0.xbm Grey_9_1.xbm Grey_9_2.xbm
    Grey_9_3.xbm Grey_9_4.xbm Grey_9_5.xbm Grey_9_6.xbm Grey_9_7.xbm
    Grey_9_8.xbm Grey_9_9.xbm grid16.xbm grid4.xbm grid8.xbm grid_diag2.xbm
    grid_diag4.xbm grid_diag8.xbm Hearts.xbm Interferance.xbm Ironcoat.xbm
    Kapow.xbm line_diag3_1.xbm line_diag3_2.xbm line_diag4_1.xbm
    line_diag4_2.xbm line_diag4_3.xbm line_diag8_1.xbm line_diag8_2.xbm
    line_diag8_3.xbm line_diag8_4.xbm line_hex_small.xbm line_hex.xbm
    line_horz2_1.xbm line_vert2_1.xbm line_vert3_1.xbm line_vert3_2.xbm
    line_vert4_1.xbm line_vert4_2.xbm line_vert4_3.xbm line_vert5_1.xbm
    line_vert5_2.xbm line_vert8_4.xbm line_wave.xbm Movietone.xbm Ovals.xbm
    plaid4.xbm plaid8.xbm plaid.xbm Rain.xbm random_1.xbm random_2.xbm
    random_3.xbm random_4.xbm Scales.xbm Snake.xbm Spirals.xbm Spiral.xbm
    Squared.xbm Squares.xbm Squarez.xbm Stars.xbm stich_diag2.xbm
    stich_diag.xbm stich_horz.xbm stich_vert.xbm Suntile.xbm Tellipse.xbm
    Tiles.xbm Trelis.xbm Tripoint.xbm tweed2_2.xbm tweed4_3.xbm tweed4_4.xbm
    tweed5_5.xbm tweed_crab.xbm tweed_cross.xbm tweeddish.xbm tweed.xbm
    Tyres.xbm Ubalu2.xbm Ubalu.xbm Vertigo.xbm Vibration.xbm
    weave_crazy_long.xbm weave_crazy.xbm weave_diag_1.xbm weave_diag_2.xbm
    weave_diag_3.xbm weave_diag_4.xbm weave_diag_root.xbm weave_diag.xbm
    weave_rect_dbl.xbm weave_rect_long.xbm weave_rect_loose.xbm
    weave_rect_wide2.xbm weave_rect_wide.xbm weave_rect.xbm
    weave_tied_cross.xbm weave_wide2.xbm weave_wide.xbm Wiggly.xbm
    Zigzag.xbm)

set(XPCE_DATA_appl-help customise.hlp dialog.hlp emacs.hlp
    event_monitor.hlp help.hlp pcedraw.hlp pcefaq.hlp plprefs.hlp)

set(XPCE_DATA_man_faq faq.html)

set(XPCE_DATA_man_reference bug_fixes.doc changes.doc errors.doc
    examples.doc groups.doc objects.doc predicates.doc tools.doc
    topics.doc)

set(XPCE_DATA_man_reference_class and.doc application.doc arc.doc
    area.doc arrow.doc assign.doc attribute.doc behaviour.doc
    bezier_curve.doc binary_condition.doc binary_expression.doc binding.doc
    bitmap.doc block.doc bool.doc box.doc browser.doc
    browser_select_gesture.doc button.doc chain.doc chain_hyper.doc
    chain_table.doc char_array.doc circle.doc class.doc class_variable.doc
    click_gesture.doc code.doc code_vector.doc colour.doc colour_map.doc
    connect_gesture.doc connection.doc constant.doc constraint.doc
    c_pointer.doc create.doc cursor.doc date.doc device.doc dialog.doc
    dialog_group.doc dialog_item.doc dict.doc dict_item.doc directory.doc
    display.doc display_manager.doc divide.doc @=.doc editor.doc
    elevation.doc ellipse.doc eq.doc equal.doc error.doc event.doc
    event_node.doc event_tree.doc figure.doc file.doc font.doc format.doc
    fragment.doc frame.doc function.doc gesture.doc get_method.doc
    graphical.doc grbox.doc greateq.doc greater.doc handle.doc handler.doc
    handler_group.doc hash_table.doc hbox.doc host_data.doc host.doc
    hyper.doc identity.doc if.doc image.doc int_item.doc joint.doc
    key_binding.doc label_box.doc label.doc layout_manager.doc lbox.doc
    less.doc lesseq.doc line.doc link.doc list_browser.doc menu_bar.doc
    menu.doc menu_item.doc message.doc method.doc minus.doc modifier.doc
    monitor.doc move_gesture.doc move_outline_gesture.doc name.doc
    nameref.doc node.doc not.doc noteq.doc number.doc object.doc obtain.doc
    operator.doc or.doc parbox.doc parser.doc path.doc pce.doc picture.doc
    pixmap.doc plus.doc point.doc popup.doc popup_gesture.doc process.doc
    progn.doc program_object.doc prolog_term.doc quote_function.doc real.doc
    recogniser.doc regex.doc region.doc relation.doc relation_table.doc
    resize_gesture.doc resize_outline_gesture.doc
    resize_table_slice_gesture.doc resource.doc rubber.doc scroll_bar.doc
    send_method.doc sheet.doc size.doc slider.doc socket.doc
    source_location.doc source_sink.doc spatial.doc stream.doc string.doc
    style.doc syntax_table.doc tab.doc table_cell.doc table_column.doc
    table.doc table_row.doc table_slice.doc tab_stack.doc tbox.doc
    text_buffer.doc text_cursor.doc text.doc text_image.doc text_item.doc
    text_margin.doc tile_adjuster.doc tile.doc timer.doc times.doc
    tokeniser.doc tree.doc tuple.doc type.doc var.doc variable.doc
    vector.doc view.doc visual.doc vmi.doc when.doc while.doc
    window_decorator.doc window.doc win_metafile.doc win_printer.doc)

################
# QLF DEPENDENCIES

prepend(XPCE_QLF_pce ../boot/ ${XPCE_DATA_prolog_boot})
set(XPCE_QLF_pce ${XPCE_QLF_pce}
    swi_compatibility.pl english/pce_messages.pl)
set(XPCE_QLF_trace trace/clause.pl trace/util.pl trace/source.pl
    trace/gui.pl trace/settings.pl trace/stack.pl trace/viewterm.pl
    trace/stack.pl)
set(XPCE_QLF_emacs emacs/window.pl emacs/buffer.pl emacs/application.pl
    emacs/buffer_menu.pl emacs/server.pl emacs/history.pl
    emacs/fundamental_mode.pl emacs/language_mode.pl emacs/outline_mode.pl
    emacs/bookmarks.pl)
