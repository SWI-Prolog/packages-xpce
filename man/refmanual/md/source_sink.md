# class source_sink {#class-source_sink}


## Send methods {#class-source_sink-send}

- source_sink->encoding: {octet,ascii,iso_latin_1,text,utf8,unicode_be,unicode_le,wchar}
    Encoding of the data-source:

    - binary
    	The source contains binary data (i.e. an octed stream).

    - iso_latin_1
    	The source contains text in ISO Latin-1 format.

    - utf8
    	The source contains UCS/UNICODE characters encoded using
    	the multibyte UTF-8 encoding.

    - unicode_le, unicode_be
    	The source contains 16-but UNICODE-2 characters in
    	little- or big endian encoding

    - wchar
    	The source contains a sequence of wchar_t objects.  This
    	internally format is implementation defined, both in
    	terms of the number of bytes per character as the
    	byte-order.

    See also `file->initialise`.

