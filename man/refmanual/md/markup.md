# class markup {#class-markup}

A markup is a string that holds
[Pango markup](https://docs.gtk.org/Pango/pango_markup.html), for
example `"Saved <b>%s</b>"`.  Being a string, a markup can be used
wherever text is accepted.  Its class tells the receiver that the text
is markup: a label renders it (see `label<-selection`), as does a
list_browser for the label of an item (see `dict_item<-label`) and its
own `list_browser<-label`, an editor for its `editor<-label` and a
decorated window for its label (see `window_decorator->label`), while
receivers
that cannot render markup use its text without the markup.  See class
markup_text for the tags and their attributes.  Colour attributes also
accept xpce colour names, notably theme colours such as `ui_link`,
and `<tt>` uses xpce's fixed-width font.

Formatting with a markup as format makes the format markup and the
arguments text.  The text for the arguments is escaped, unless the
argument is itself a markup.  For example, the label below shows the
file name in bold, whatever characters it holds:

	send(Label, format,
	     markup("Cannot open <b>%s</b>"), '<a&b>.txt').

A markup used as format without arguments is used as is.  This makes
it safe to pass a markup that is formatted already as the format of
`object->report`:

	send(Window, report, warning,
	     markup("Cannot open <b>%s</b>", File)).

Formatting a string, or writing to the console, with a markup as format
gives the text without the markup.  This is how ->report messages that
end up in an alert box or on the console are shown.

## Send methods {#class-markup-send}

- markup->initialise: format=[char_array], argument=any ...
    Create from markup or, if arguments are given, as ->format with
    escaped arguments (see above).  Fails with a syntax error if the
    result is not valid markup.

## Get methods {#class-markup-get}

- markup<-convert: text=char_array -> markup
    Convert plain text to markup, escaping `<`, `>`, `&` and quotes.

- markup<-text: -> string
    The text without the markup.
