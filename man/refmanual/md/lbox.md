# class lbox {#class-lbox}

Typesetting list-box: a `device` that lays out child graphicals
vertically inside a fixed pixel width, similar to a TeX `\vbox`.
Used by the typesetting helpers in classes `parbox`, `hbox` and
`tbox`.

@see class parbox
@see class hbox
@see class tbox


## Send methods {#class-lbox-send}

- lbox->initialise: width=[0..]
    Create an lbox object of the specified pixel width.

- lbox->append: label=[graphical], item=[graphical]
    Append a labelled item and trigger a re-layout.  When omitted, the
    label and item are created using <-new_label and <-new_item.  If
    there is no label, only the item is displayed.

- lbox->compute
    Recompute the layout of the contained graphicals.

- lbox->geometry: x=[int], y=[int], width=[int], height=[int]
    Standard `graphical->geometry` override that re-flows the
    contents when the width changes.
