/*  unicode_heavy.pl — valid Prolog heavily using NFD and double-width chars.
 *
 *  This file is in the public domain.
 *
 *  NFD (Normative Decomposition) text uses a base character followed by a
 *  combining mark as separate code points, e.g. "e" + U+0301 instead of "é".
 *  Double-width characters are East-Asian CJK/kana/hangul glyphs that each
 *  occupy two terminal columns.
 *
 *  The file does not need to make semantic sense; it just needs to be
 *  syntactically valid Prolog and to stress-test editors that compute
 *  visual columns from character counts rather than from glyph widths.
 *
 *  NFD atoms below contain literal combining marks (U+0301 acute, U+0308
 *  diaeresis, U+0303 tilde, U+0327 cedilla, U+0302 circumflex) as raw
 *  UTF-8 bytes — they are NOT written as \uXXXX escape sequences.
 */

:- module(unicode_heavy, [
      language/2,
      city/2,
      greeting/2,
      nfd_word/2,
      mixed_line/1,
      sentence//1
   ]).
:- encoding(utf8).

		 /*******************************
		 *   DOUBLE-WIDTH: CJK, KANA   *
		 *******************************/

/* 日本語 — Japanese */
language(japanese,   '日本語').
language(chinese,    '中文').
language(korean,     '한국어').
language(arabic,     'العربية').
language(greek,      'ελληνικά').
language(hiragana,   'あいうえおかきくけこさしすせそ').
language(katakana,   'アイウエオカキクケコサシスセソ').
language(hangul,     '가나다라마바사아자차카타파하').
language(cjk_ext_a,  '㐀㐁㐂㐃㐄㐅㐆').
language(yi,         'ꀀꀁꀂꀃꀄꀅꀆꀇ').

/* 城市 — cities with their local-script names */
city(tokyo,    '東京').
city(beijing,  '北京').
city(seoul,    '서울').
city(taipei,   '臺北').
city(osaka,    '大阪').
city(shanghai, '上海').
city(busan,    '부산').
city(kyoto,    '京都').
city(nanjing,  '南京').
city(incheon,  '인천').

/* 挨拶 — greetings */
greeting(japanese, 'こんにちは世界').
greeting(chinese,  '你好世界').
greeting(korean,   '안녕하세요 세계').
greeting(thai,     'สวัสดีชาวโลก').

		 /*******************************
		 *   NFD: COMBINING MARKS      *
		 *******************************/

/*  Each atom below is written in NFD: the combining mark follows its base
 *  character as a literal Unicode code point in the UTF-8 source.
 *
 *  café      — e + U+0301 (combining acute accent)
 *  naïve     — i + U+0308 (combining diaeresis)
 *  résumé    — e + U+0301, repeated at end
 *  señor     — n + U+0303 (combining tilde)
 *  Zürich    — u + U+0308
 *  garçon    — c + U+0327 (combining cedilla)
 *  rôle      — o + U+0302 (combining circumflex)
 */

nfd_word(french,  'café').         % e + combining acute U+0301
nfd_word(french,  'naïve').        % i + combining diaeresis U+0308
nfd_word(french,  'résumé').       % e+0301, e+0301
nfd_word(spanish, 'señor').        % n + combining tilde U+0303
nfd_word(spanish, 'piñata').       % n + combining tilde U+0303
nfd_word(german,  'Zürich').       % u + combining diaeresis U+0308
nfd_word(german,  'Köln').         % o + combining diaeresis U+0308
nfd_word(german,  'Düsseldorf').   % u + combining diaeresis U+0308
nfd_word(french,  'garçon').       % c + combining cedilla U+0327
nfd_word(french,  'rôle').         % o + combining circumflex U+0302
nfd_word(french,  'naïveté').      % i+0308, e+0301
nfd_word(german,  'für').          % u + combining diaeresis U+0308
nfd_word(german,  'schön').        % o + combining diaeresis U+0308
nfd_word(spanish, 'mañana').       % n + combining tilde U+0303
nfd_word(spanish, 'año').          % n + combining tilde U+0303
nfd_word(portuguese, 'ação').      % a+0303, a+0303 (combining tilde on a)
nfd_word(portuguese, 'coração').   % a+0303 twice

		 /*******************************
		 *   MIXED NFD + DOUBLE-WIDTH  *
		 *******************************/

/*  Atoms that interleave CJK wide glyphs with NFD Latin.  An editor must
 *  not confuse a combining mark (zero visual columns) with a CJK cell
 *  (two visual columns).
 */

mixed_line('東京 café 北京').
mixed_line('中文 Zürich 日本語').
mixed_line('こんにちは crêpe アイウエオ señor').
mixed_line('한국어 garçon 가나다라').
mixed_line('αβγ café αβγ naïve ωψχ').
mixed_line('résumé 東京 Köln 大阪 piñata 京都').
mixed_line('北京 für 서울 schön 臺北 rôle 上海').
mixed_line('한국어 mañana 가나다라 año 나다라마').

		 /*******************************
		 *  PREDICATES WITH WIDE NAMES *
		 *******************************/

'東京の電話番号'(03).
'大阪の電話番号'(06).
'서울의 전화번호'(02).

'漢字テスト'(X) :-
    member(X, ['一', '二', '三', '四', '五',
               '六', '七', '八', '九', '十']).

'片仮名テスト'(X) :-
    member(X, ['ア', 'イ', 'ウ', 'エ', 'オ',
               'カ', 'キ', 'ク', 'ケ', 'コ']).

		 /*******************************
		 *   DCG WITH UNICODE TOKENS   *
		 *******************************/

sentence(Words) --> cjk_words(Words).

cjk_words([W|Ws]) --> cjk_word(W), cjk_words(Ws).
cjk_words([])     --> [].

cjk_word('日本') --> ['日本'].
cjk_word('語')   --> ['語'].
cjk_word('中文') --> ['中文'].
cjk_word('テスト') --> ['テスト'].
cjk_word('한국') --> ['한국'].
cjk_word('어')   --> ['어'].

		 /*******************************
		 *   LIST DATA                 *
		 *******************************/

cjk_list(['東京', '大阪', '京都', '名古屋', '札幌',
          '서울', '부산', '인천', '北京', '上海']).

nfd_list(['café', 'naïve', 'résumé', 'señor', 'Zürich',
          'Köln', 'garçon', 'rôle', 'piñata', 'mañana']).

mixed_list(['東京 café', '大阪 résumé', '서울 señor',
            '北京 Zürich', 'こんにちは naïve', 'αβγ garçon']).

		 /*******************************
		 *   STRING OPERATIONS         *
		 *******************************/

joined_cjk(J) :-
    atom_concat('東京', '大阪', J).

joined_nfd(J) :-
    atom_concat('café', ' et ', T),
    atom_concat(T, 'naïve', J).

split_demo :-
    atom_chars('中文テスト', Cs),
    length(Cs, N),
    format("chars: ~w  length: ~w~n", [Cs, N]).

nfd_length_demo :-
    atom_codes('café', Codes),       % 5 codes (e + combining acute = 2)
    length(Codes, N),
    format("café code count: ~w~n", [N]).

		 /*******************************
		 *   COMMENT STRESS TEST       *
		 *******************************/

/*  以下はコメントのストレステストです。
 *  この行には日本語と中文が混在しています。
 *  한국어도 포함되어 있습니다.
 *  NFD: café résumé naïve señor Zürich garçon rôle
 *  double-width: 东西南北 上下左右 春夏秋冬 日月星辰
 *  wide emoji: 🌏 🗾 🏯 🗼 🍣 🍜 🍱
 *  SMP CJK Ext B: 𠀀 𠀁 𠀂 𠀃 𠁂 𠂉 𠂊 𠂍
 *  inline mix: file [🌏] saved 𠀀 to 北京 path ok
 */

%  一行コメント: 東京・大阪・京都・名古屋・札幌
%  한 줄 주석: 서울, 부산, 인천, 대구, 광주
%  单行注释: 北京、上海、广州、深圳、成都
%  NFD line: café señor Zürich naïve résumé garçon

		 /*******************************
		 *   DEMO PREDICATE            *
		 *******************************/

demo :-
    forall(language(_, Name),
           format("language: ~w~n", [Name])),
    forall(city(_, Name),
           format("city: ~w~n", [Name])),
    forall(nfd_word(_, W),
           format("NFD word: ~w~n", [W])),
    forall(mixed_line(L),
           format("mixed: ~w~n", [L])).
