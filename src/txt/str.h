/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef XPCE_TXT_STR_H_INCLUDED
#define XPCE_TXT_STR_H_INCLUDED

/* Functions defined in txt/str.c */

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
COMMON(status)	str_iswide(PceString s);
COMMON(void)	str_ncpy(PceString dest, int at, PceString src, int from, int len);
COMMON(void)	str_cpy(PceString dest, PceString src);
COMMON(charA *)	str_textp(PceString s, int i);
COMMON(void)	str_upcase(PceString str, int from, int to);
COMMON(void)	str_downcase(PceString str, int from, int to);
COMMON(void)	str_translate(PceString str, int chrfrom, int chrto);
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
COMMON(int)	str_next_index(PceString s, int from, uchar_t chr);
COMMON(int)	str_next_rindex(PceString s, int from, uchar_t chr);
COMMON(int)	str_index(PceString s, uchar_t chr);
COMMON(int)	str_rindex(PceString s, uchar_t chr);
COMMON(int)	str_count_chr(PceString s, int from, int to, uchar_t chr);
COMMON(int)	str_lineno(PceString s, int at);
COMMON(uchar_t)	str_fetch(PceString s, int idx);
COMMON(int)	str_store(PceString s, int idx, unsigned int chr);
COMMON(PceString)	str_nl(PceString proto);
COMMON(PceString)	str_spc(PceString proto);
COMMON(PceString)	str_tab(PceString proto);
COMMON(void)	str_strip(PceString s);
COMMON(int)	str_common_length(PceString s1, PceString s2);
COMMON(int)	str_icase_common_length(PceString s1, PceString s2);
COMMON(tmp_string *) str_tmp_init(tmp_string *tmp);
COMMON(uchar_t)	str_tmp_put(tmp_string *tmp, uchar_t c);
COMMON(void)	str_tmp_done(tmp_string *tmp);

#endif /*XPCE_TXT_STR_H_INCLUDED*/
