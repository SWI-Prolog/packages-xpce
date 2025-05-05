#define COMMON(type) SO_LOCAL type

/* /swi40/jan/src/pl/packages/xpce/src/txt/chararray.c */
COMMON(status)	initialiseCharArray(CharArray n, CharArray value);
COMMON(Name)	getValueCharArray(CharArray n);
COMMON(status)	equalCharArray(CharArray n1, CharArray n2, BoolObj ign_case);
COMMON(status)	prefixCharArray(CharArray n1, CharArray n2, BoolObj ign_case);
COMMON(status)	suffixCharArray(CharArray n, CharArray s, BoolObj ign_case);
COMMON(status)	isWideCharArray(Any s);
COMMON(CharArray) getCopyCharArray(CharArray n);
COMMON(CharArray) getCapitaliseCharArray(CharArray n);
COMMON(CharArray) getLabelNameCharArray(CharArray n);
COMMON(CharArray) getDowncaseCharArray(CharArray n);
COMMON(CharArray) getAppendCharArray(CharArray n1, CharArray n2);
COMMON(CharArray) getDeleteSuffixCharArray(CharArray n, CharArray s);
COMMON(CharArray) getEnsureSuffixCharArray(CharArray n, CharArray s);
COMMON(CharArray) getSubCharArray(CharArray n, Int start, Int end);
COMMON(Int)	getSizeCharArray(Any n);
COMMON(void)	initCharArrays(void);
COMMON(CharArray) CtoScratchCharArray(const char *s);
COMMON(CharArray) StringToScratchCharArray(const PceString s);
COMMON(void)	doneScratchCharArray(CharArray n);
COMMON(CharArray) CtoCharArray(char *s);
COMMON(status)	makeClassCharArray(Class class);

/* /swi40/jan/src/pl/packages/xpce/src/txt/editor.c */
COMMON(status)	normaliseEditor(Editor e, Int start, Int end);
COMMON(Any)	ReceiverOfEditor(Editor e);
COMMON(status)	forwardModifiedEditor(Editor e, BoolObj val);
COMMON(status)	scrollToEditor(Editor e, Int pos, Int screenline);
COMMON(status)  scrollVerticalEditor(Editor e, Name dir, Name unit, Int amount);
COMMON(status)	selectionEditor(Editor e, Int from, Int to, Name status);
COMMON(Point)	getSelectionEditor(Editor e);
COMMON(StringObj) getSelectedEditor(Editor e);
COMMON(status)	formatEditor(Editor e, CharArray fmt, int argc, Any *argv);
COMMON(status)	clearEditor(Editor e);
COMMON(status)	backgroundEditor(Editor e, Any bg);
COMMON(status)	colourEditor(Editor e, Any c);
COMMON(status)	makeClassEditor(Class class);

/* /swi40/jan/src/pl/packages/xpce/src/txt/fragment.c */
COMMON(status)	makeClassFragment(Class class);

/* /swi40/jan/src/pl/packages/xpce/src/txt/keybinding.c */
COMMON(Any)	getFunctionKeyBinding(KeyBinding kb, EventId id);
COMMON(status)	functionKeyBinding(KeyBinding kb, EventId id, Any f);
COMMON(status)	typedKeyBinding(KeyBinding kb, Any id, Graphical receiver);
COMMON(status)	makeClassKeyBinding(Class class);
COMMON(KeyBinding) KeyBindingText(void);
COMMON(KeyBinding) KeyBindingTextItem(void);
COMMON(KeyBinding) KeyBindingTextItemView(void);

/* /swi40/jan/src/pl/packages/xpce/src/txt/regex.c */
COMMON(status)	ignoreCaseRegex(Regex re, BoolObj val);
COMMON(status)	compileRegex(Regex re, BoolObj optimize);
COMMON(status)	search_string_regex(Regex re, PceString s);
COMMON(status)	searchRegex(Regex re, Any obj, Int start, Int end);
COMMON(Int)	getMatchRegex(Regex re, Any obj, Int start, Int end);
COMMON(status)	matchRegex(Regex re, Any obj, Int start, Int end);
COMMON(Int)	getRegisterEndRegex(Regex re, Int which);
COMMON(status)	makeClassRegex(Class class);

/* /swi40/jan/src/pl/packages/xpce/src/txt/str.c */
COMMON(int)	str_allocsize(PceString s);
COMMON(void)	str_pad(PceString s);
COMMON(void)	str_alloc(PceString s);
COMMON(void)	str_unalloc(PceString s);
COMMON(PceString)	str_init(PceString s, PceString proto, charA *data);
COMMON(PceString)	fstr_inithdr(PceString s, int iswide, void *data, int len);
COMMON(status)	str_set_n_ascii(PceString str, size_t len, char *text);
COMMON(status)	str_set_n_wchar(PceString str, size_t len, wchar_t *text);
COMMON(status)	str_set_ascii(PceString str, char *text);
COMMON(status)	str_set_utf8(PceString str, const char *text);
COMMON(status)	str_set_static(PceString str, const char *text);
COMMON(status)	str_iswide(PceString s);
COMMON(void)	str_ncpy(PceString dest, int at, PceString src, int from, int len);
COMMON(void)	str_cpy(PceString dest, PceString src);
COMMON(charA *)	str_textp(PceString s, int i);
COMMON(void)	str_upcase(PceString str, int from, int to);
COMMON(void)	str_downcase(PceString str, int from, int to);
COMMON(int)	str_cmp(PceString s1, PceString s2);
COMMON(int)	str_icase_cmp(PceString s1, PceString s2);
COMMON(int)	str_eq(PceString s1, PceString s2);
COMMON(int)	str_icase_eq(PceString s1, PceString s2);
COMMON(int)	str_prefix_offset(PceString s1, unsigned int offset, PceString s2);
COMMON(int)	str_prefix(PceString s1, PceString s2);
COMMON(int)	str_icase_prefix(PceString s1, PceString s2);
COMMON(int)	str_suffix(PceString s1, PceString s2);
COMMON(int)	str_icase_suffix(PceString s1, PceString s2);
COMMON(int)	str_sub(PceString s1, PceString s2);
COMMON(int)	str_icasesub(PceString s1, PceString s2);
COMMON(int)	str_next_index(PceString s, int from, wint_t chr);
COMMON(int)	str_next_rindex(PceString s, int from, wint_t chr);
COMMON(int)	str_index(PceString s, wint_t chr);
COMMON(int)	str_rindex(PceString s, wint_t chr);
COMMON(int)	str_count_chr(PceString s, int from, int to, wint_t chr);
COMMON(int)	str_lineno(PceString s, int at);
COMMON(wint_t)	str_fetch(PceString s, int idx);
COMMON(int)	str_store(PceString s, int idx, unsigned int chr);
COMMON(PceString)	str_nl(PceString proto);
COMMON(PceString)	str_spc(PceString proto);
COMMON(PceString)	str_tab(PceString proto);
COMMON(void)	str_strip(PceString s);
COMMON(int)	str_common_length(PceString s1, PceString s2);
COMMON(int)	str_icase_common_length(PceString s1, PceString s2);
COMMON(tmp_string *) str_tmp_init(tmp_string *tmp);
COMMON(wint_t)	str_tmp_put(tmp_string *tmp, wint_t c);
COMMON(void)	str_tmp_done(tmp_string *tmp);

/* /swi40/jan/src/pl/packages/xpce/src/txt/string.c */
COMMON(StringObj) create_string_from_str(PceString s, int tmp);
COMMON(StringObj) StringToString(PceString s);
COMMON(StringObj) StringToTempString(PceString s);
COMMON(StringObj) CtoString(const char *s);
COMMON(StringObj) staticCtoString(const char *s);
COMMON(StringObj) CtoTempString(char *s);
COMMON(status)	initialiseStringv(StringObj str, CharArray fmt, int argc, Any *argv);
COMMON(status)	valueString(StringObj s1, CharArray s2);
COMMON(status)	insertCharacterString(StringObj str, Int chr, Int where, Int times);
COMMON(status)	upcaseString(StringObj s);
COMMON(status)	deleteString(StringObj str, Int start, Int length);
COMMON(status)	insertString(StringObj s1, Int n, CharArray s2);
COMMON(status)	str_insert_string(StringObj str, Int where, PceString s);
COMMON(StringObj) getSubString(StringObj n, Int start, Int end);
COMMON(status)	makeClassString(Class class);

/* /swi40/jan/src/pl/packages/xpce/src/txt/style.c */
COMMON(status)	boldStyle(Style s, BoolObj on);
COMMON(status)	makeClassStyle(Class class);

/* /swi40/jan/src/pl/packages/xpce/src/txt/syntax.c */
COMMON(status)	makeClassSyntaxTable(Class class);

/* /swi40/jan/src/pl/packages/xpce/src/txt/textbuffer.c */
COMMON(status)	changedTextBuffer(TextBuffer tb);
COMMON(status)	ChangedRegionTextBuffer(TextBuffer tb, Int start, Int end);
COMMON(status)	ChangedFragmentListTextBuffer(TextBuffer tb);
COMMON(status)	clearTextBuffer(TextBuffer tb);
COMMON(status)	insertFileTextBuffer(TextBuffer tb, Int where, SourceSink file, Int times);
COMMON(status)	insertTextBuffer(TextBuffer tb, Int where, CharArray ca, Int times);
COMMON(status)	CAppendTextBuffer(TextBuffer tb, const char *text);
COMMON(status)	appendTextBuffer(TextBuffer tb, CharArray ca, Int times);
COMMON(status)	deleteTextBuffer(TextBuffer tb, Int where, Int times);
COMMON(status)	saveTextBuffer(TextBuffer tb, SourceSink file, Int from, Int len);
COMMON(status)	CmodifiedTextBuffer(TextBuffer tb, BoolObj val);
COMMON(status)	characterTextBuffer(TextBuffer tb, Int where, Int c);
COMMON(status)	transposeTextBuffer(TextBuffer tb, Int f1, Int t1, Int f2, Int t2);
COMMON(status)	downcaseTextBuffer(TextBuffer tb, Int from, Int len);
COMMON(status)	upcaseTextBuffer(TextBuffer tb, Int from, Int len);
COMMON(status)	capitaliseTextBuffer(TextBuffer tb, Int from, Int len);
COMMON(Int)	getScanTextBuffer(TextBuffer tb, Int from, Name unit, Int amount, Name az);
COMMON(StringObj) getContentsTextBuffer(TextBuffer tb, Int from, Int length);
COMMON(status)	parsep_line_textbuffer(TextBuffer tb, intptr_t here);
COMMON(intptr_t) scan_textbuffer(TextBuffer tb, intptr_t from, Name unit, intptr_t amount, int az);
COMMON(Int)	getMatchingBracketTextBuffer(TextBuffer tb, Int idx, Int bracket);
COMMON(Int)	getSkipBlanksTextBuffer(TextBuffer tb, Int where, Name direction, BoolObj skipnl);
COMMON(Int)	getLineNumberTextBuffer(TextBuffer tb, Int i);
COMMON(intptr_t) find_textbuffer(TextBuffer tb, intptr_t here, PceString str, intptr_t times, char az, int ec, int wm);
COMMON(int)	match_textbuffer(TextBuffer, intptr_t, PceString, int, int);
COMMON(intptr_t) fill_line_textbuffer(TextBuffer tb, intptr_t here, intptr_t to, int sc, int rm, int justify);
COMMON(status)	sortTextBuffer(TextBuffer tb, Int from, Int to);
COMMON(intptr_t) count_lines_textbuffer(TextBuffer tb, intptr_t f, intptr_t t);
COMMON(int)	start_of_line_n_textbuffer(TextBuffer tb, int lineno);
COMMON(int)	fetch_textbuffer(TextBuffer tb, intptr_t where);
COMMON(status)	change_textbuffer(TextBuffer tb, intptr_t where, PceString s);
COMMON(status)	str_sub_text_buffer(TextBuffer tb, PceString s, intptr_t start, intptr_t len);
COMMON(status)	insert_textbuffer(TextBuffer tb, intptr_t where, intptr_t times, PceString s);
COMMON(status)	delete_textbuffer(TextBuffer tb, intptr_t where, intptr_t length);
COMMON(status)	makeClassTextBuffer(Class class);

/* /swi40/jan/src/pl/packages/xpce/src/txt/textcursor.c */
COMMON(status)	setTextCursor(TextCursor c, Int x, Int y, Int w, Int h, Int b);
COMMON(status)	makeClassTextCursor(Class class);

/* /swi40/jan/src/pl/packages/xpce/src/txt/textimage.c */
COMMON(status)	InsertTextImage(TextImage ti, Int where, Int amount);
COMMON(status)	ChangedRegionTextImage(TextImage ti, Int from, Int to);
COMMON(status)	ChangedEntireTextImage(TextImage ti);
COMMON(status)	get_character_box_textimage(TextImage ti, int index, int *x, int *y, int *w, int *h, int *b);
COMMON(Int)	getLinesTextImage(TextImage ti);
COMMON(Int)	getIndexTextImage(TextImage ti, EventObj ev);
COMMON(status)	computeTextImage(TextImage ti);
COMMON(status)	startTextImage(TextImage ti, Int start, Int skip);
COMMON(status)	centerTextImage(TextImage ti, Int position, Int screen_line);
COMMON(Int)	getStartTextImage(TextImage ti, Int line);
COMMON(status)	backgroundTextImage(TextImage ti, Any bg);
COMMON(status)	tabDistanceTextImage(TextImage ti, Int tab);
COMMON(status)	tabStopsTextImage(TextImage ti, Vector v);
COMMON(Int)	getViewTextImage(TextImage ti);
COMMON(status)	bubbleScrollBarTextImage(TextImage ti, ScrollBar sb);
COMMON(Int)	getScrollStartTextImage(TextImage ti, Name dir, Name unit, Int amount);
COMMON(Int)	getUpDownColumnTextImage(TextImage ti, Int here);
COMMON(Int)	getUpDownCursorTextImage(TextImage ti, Int here, Int updown, Int column);
COMMON(Int)	getBeginningOfLineCursorTextImage(TextImage ti, Int here);
COMMON(Int)	getEndOfLineCursorTextImage(TextImage ti, Int here);
COMMON(status)	ensureVisibleTextImage(TextImage ti, Int caret);
COMMON(status)	makeClassTextImage(Class class);

/* /swi40/jan/src/pl/packages/xpce/src/txt/textmargin.c */
COMMON(status)	makeClassTextMargin(Class class);

/* /swi40/jan/src/pl/packages/xpce/src/txt/undo.c */
COMMON(void)	destroyUndoBuffer(UndoBuffer ub);
COMMON(Int)	getUndoTextBuffer(TextBuffer tb);
COMMON(status)	undoTextBuffer(TextBuffer tb);
COMMON(status)	undoBufferSizeTextBuffer(TextBuffer tb, Int size);
COMMON(status)	markUndoTextBuffer(TextBuffer tb);
COMMON(status)	resetUndoTextBuffer(TextBuffer tb);
COMMON(status)	checkpointUndoTextBuffer(TextBuffer tb);
COMMON(void)	register_insert_textbuffer(TextBuffer tb, long int where, long int len);
COMMON(void)	register_delete_textbuffer(TextBuffer tb, long where, long len);
COMMON(void)	register_change_textbuffer(TextBuffer tb, long int where, long int len);

/* /swi40/jan/src/pl/packages/xpce/src/txt/utf8.c */
COMMON(char *)	F_UTF8_GET_CHAR(const char *in, int *chr);
COMMON(char *)	F_UTF8_PUT_CHAR(char *out, int chr);
COMMON(size_t)  F_UTF8_STRLEN(const char *s, size_t len);
COMMON(size_t)  F_UTF8_ENCLENW(const wchar_t *s, size_t len);
COMMON(size_t)  F_UTF8_ENCLENA(const char *s, size_t len);

/* /swi40/jan/src/pl/packages/xpce/src/txt/i18n.c */
COMMON(wchar_t *) charArrayToWC(CharArray ca, size_t *len);
COMMON(char *)	stringToUTF8(PceString s);
COMMON(char *)	charArrayToUTF8(CharArray ca);
COMMON(char *)	charArrayToMB(CharArray ca);
COMMON(char *)	nameToMB(Name nm);
COMMON(char *)	nameToUTF8(Name nm);
COMMON(wchar_t *) nameToWC(Name nm, size_t *len);
COMMON(Name)	UTF8ToName(const char *utf8);
COMMON(Name)	MBToName(const char *mb);
COMMON(Name)	WCToName(const wchar_t *wc, size_t len);
COMMON(StringObj) WCToString(const wchar_t *wc, size_t len);
COMMON(Name)	FNToName(const char *name);
COMMON(char *)	charArrayToFN(CharArray ca);
COMMON(char *)	stringToFN(PceString s);
COMMON(char *)	nameToFN(Name nm);
