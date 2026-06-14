# class text_change {#class-text_change}

Record of a single edit to a `text_buffer`: the range that was
replaced and the text that replaced it.  text_change objects are the
payload of the `text_buffer<-changes` chain and let `editor`,
`fragment` and `view` follow the buffer's edit history.

Positions are 0-based and use UTF-16 code units to match the LSP/DOM
conventions other parts of xpce talk to.

@see class text_buffer
@see class fragment
@see class editor


## Instance variables {#class-text_change-instvars}

- text_change<-start_line: int
    0-based line where the change starts.

- text_change<-start_position: int
    0-based position on `<-start_line`, counted in UTF-16 units.

- text_change<-end_line: int
    0-based line where the change ends.

- text_change<-end_position: int
    0-based position on `<-end_line`, counted in UTF-16 units.

- text_change<-length: int
    Length of the replaced region, in UTF-16 units.

- text_change<-replacement: string*
    Text used to replace the region (`@nil` for a pure deletion).


## Send methods {#class-text_change-send}

- text_change->initialise: start_line=int, start_position=int, end_line=[int], end_position=[int], length=[int], replacement=[string*]
    Build a change record from its range and replacement text.
    `end_line`/`end_position` default to start; `length` defaults
    to 0.


## Get methods {#class-text_change-get}

- text_change<-size: -> int
    Approximate size of this change in bytes (its memory footprint).
    Used by `text_buffer` when bounding the size of the undo/redo
    history.
