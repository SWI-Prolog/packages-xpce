# class grbox {#class-grbox}

A `grbox` (graphical box) wraps an arbitrary xpce graphical so it can
participate in the typesetting of a `parbox`.  The encapsulated
graphical is displayed on the parbox device and otherwise behaves like
any other graphical.  See class `parbox` for an overview of the
document rendering system.

@see class parbox
@see class hbox
@see class tbox


## Instance variables {#class-grbox-instvars}

- grbox<-graphical: graphical
    The encapsulated graphical.  This may be any xpce graphical
    object; it is displayed on the parbox device and behaves like
    any other graphical.

- grbox<->alignment: {top,center,bottom,left,right}
    Placement inside the paragraph:

    - `@nil` (or unset)
        The graphical is treated like a `tbox`: it is placed inline
        at the position where it appears in `parbox<-content`.
    - `left`
        The graphical is floated to the left margin of the first
        free line and the remaining text flows around it.
    - `right`
        As `left` but anchored against the right margin.
    - `top`, `center`, `bottom`
        Vertical alignment relative to the baseline of the line on
        which the grbox sits (see also the ABSTRACT_HBOX
        ascent/descent slots).


## Send methods {#class-grbox-send}

- grbox->initialise: graphical=graphical, alignment=[{top,center,bottom,left,right}], rubber=[rubber]*
    Create a grbox wrapping the given graphical with optional
    alignment and a `rubber` for stretch/shrink behaviour.

- grbox->compute
    Recompute the wrapped graphical and refresh the grbox dimensions
    (width, ascent, descent) to match.
