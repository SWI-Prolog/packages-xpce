/*  Figures for docrender.plx: "Document rendering primitives"
*/

:- module(fig_docrender, []).
:- use_module(library(pce)).
:- use_module('../screenshots', [settle/0, settle/1, last_frame/1]).
:- use_module(library(strings)).

:- multifile
    screenshot:figure/2.

%   \figref{parbox1}: parbox/0 of the section "Document rendering
%   primitives" (docrender.plx).  The code below is copied from the
%   guide.  The window is enlarged as the user would do, which makes
%   the parbox reformat the text.

screenshot:figure(parbox1, Frame) :-
    parbox,
    last_frame(Frame),
    settle,
    send(Frame, size, size(330, 190)).

parbox :-
        send(new(W, window), open),
        send(W, display, new(P, parbox(W?width)), point(10,10)),
        send(W, resize_message, message(P, width, @arg2?width-20)),

        send(P, alignment, justify),
        send_list(P,
                  [ append(grbox(box(40,40), left)),
                    cdata('This is the central class of the '),
                    cdata('document-rendering support.  It is '),
                    cdata('a subclass of '),
                    cdata('device', style(underline := @on)),
                    cdata(', placing '),
                    cdata('hbox', style(underline := @on)),
                    cdata(' objects as words within a paragraph. '),
                    cdata('Parbox devices have a <->width, realise '),
                    cdata('alignment and adjustment and can place text '),
                    cdata('around '),
                    cdata('floating ', style(font := italic)),
                    cdata(' objects.')
                  ]).

%   \figref{dochtml}: show_html/1 of the section "Using the doc/emit
%   library" (docrender.plx) showing the HTML document from the guide.
%   The code below is copied from the guide.  The window is enlarged to
%   show the whole document.

screenshot:figure(dochtml, Frame) :-
    html_text(HTML),
    tmp_file_stream(text, File, Out),
    write(Out, HTML),
    close(Out),
    show_html(File),
    delete_file(File),
    last_frame(Frame),
    settle,
    send(Frame, size, size(450, 380)).

:- use_module(library('doc/load')).
:- use_module(doc(emit)).
:- use_module(doc(html)).
:- use_module(library(sgml)).

show_html(File) :-
        send(new(P, picture), open),
        send(P, display, new(PB, pbox), point(10,10)),
        send(P, resize_message, message(PB, width, @arg2?width - 20)),

        load_html_file(File, Tokens),
        send(PB, show, Tokens).

html_text({|string||
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">

<html>
  <head>
    <title>Document rendering</title>
  </head>
<body>
<h1>SWI-Prolog SGML-DOM</h1>

<p>
SWI-Prolog provides a library for loading XML, SGML and
HTML files and convert them to a complex Prolog term.  This
term has the format

<p>
<pre>
element(Name,
        ListOfNameValue
        ListOfContent)
</pre>

<p>
Where <var>ListOfNameValue</var> reflects the attribute-list of the
element and <var>ListOfContent</var> is mixed list of atoms representing
<em>CDATA</em> and <b>element/3</b> terms representing nested elements.
</body>
</html>
|}).
