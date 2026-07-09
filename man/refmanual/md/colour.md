# class colour {#class-colour}

A `colour` object represents a 32-bit RGBA colour value (8 bits each
for red, green, blue and alpha).  Colours are typically attached to
graphical objects:

	?- send(new(P, picture), open),
	   send(P, display, new(B, box(100, 100))),
	   send(B, colour, red),
	   send(B, fill, blue).

Like fonts and cursors, colour objects are reusable — they can be
attached to multiple graphicals.  xpce will not create the same
colour twice; instead it returns the existing colour object from the
`@colours` table (see `<-lookup`).  Application code normally just
names a colour and lets the type-checker convert.

xpce ships with the classic SVG/CSS named-colour set (red, blue,
dark_green, …) in the hash table `@colour_names`; any name not in
that table is looked up via the OS colour database.  Colours may
also be specified in CSS hex notation (`#RGB`, `#RGBA`, `#RRGGBB`,
`#RRGGBBAA`) or constructed from RGB or HSV triples — see
`->initialise`.

@see device->foreground
@see graphical<->colour
@see window->colour


## Class variables {#class-colour-classvars}

- colour.hilite_factor: real = 0.9
    Default factor for `<-hilite`.

- colour.reduce_factor: real = 0.6
    Default factor for `<-reduce`.

- colour.fade_factor: real = 0.5
    Default alpha multiplier for `<-fade`.


## Instance variables {#class-colour-instvars}

- colour<-name: name|int
    Identifier of the colour.  Either a name from `@colour_names`
    (or the OS colour database), a CSS hex form
    (`#RRGGBB` / `#RRGGBBAA`) or, for anonymous colours, the encoded
    integer value.

- colour<-kind: {named,rgb}
    `named` when the colour was constructed by name (so resolution
    happens lazily through the colour database); `rgb` when the
    caller supplied numeric components directly.

- colour<-rgba: [int]
    Encoded 32-bit RGBA value as an integer (or @default for a
    not-yet-resolved named colour).  Use the `<-red`, `<-green`,
    `<-blue` and `<-alpha` getters to extract components.


## Send methods {#class-colour-send}

- colour->initialise: name=[name], red=[0..360], green=[0..255], blue=[0..255], alpha=[0..255], model=[{rgb,hsv}]
    Create a colour from a name and/or numeric components.

    Named colours improve readability — `@colour_names` maps
    well-known names (the SVG/CSS palette) to RGB triples.  Names
    in the form `#RGB`, `#RGBA`, `#RRGGBB` or `#RRGGBBAA` are
    interpreted as CSS hex literals.

    When `name` is @default, the colour is built from the numeric
    components.  The default model is `rgb` and each component is
    an integer 0..255.  The first component is therefore named
    `red` but actually carries the *hue* (0..360) when
    `model=hsv`.

    Examples:

    	new(C1, colour(red))               %% named
    	new(C2, colour(@default, 255, 0, 0))           %% RGB triple
    	new(C3, colour(@default, 180, 50, 50, 255, hsv)) %% HSV

    See `<-hue`/`<-saturnation`/`<-value` for the HSV model.

- colour->unlink
    Drop the colour from `@colours` and release any per-colour
    resources.  Called automatically on destruction.

- colour->equal: colour
    Succeed if both colours have the same encoded RGBA value.  Use
    in preference to comparing object references because two
    `colour(@default, ...)` literals can yield distinct objects with
    identical components.


## Get methods {#class-colour-get}

- colour<-red: -> 0..255
- colour<-green: -> 0..255
- colour<-blue: -> 0..255
- colour<-alpha: -> 0..255
    The four 8-bit components of the colour's RGBA value.  Triggers
    lazy resolution of a `<-kind: named` colour against the colour
    database on first access.

- colour<-hue: -> 0..360
- colour<-saturnation: -> 0..100
- colour<-value: -> 0..100
    Query the colour in the HSV (Hue/Saturation/Value) model:

    - `<-hue` selects the basic colour (think of it as a position
      on the rainbow);
    - `<-saturnation` is how intense the colour is — 0 is grey,
      100 is fully saturated;
    - `<-value` is brightness — 0 is black, 100 is white.

    HSV is more convenient than RGB for shading and palette
    arithmetic.  Colours may be created in this space via
    `->initialise` with `model=hsv`.  Reasoning in colour spaces is
    beyond the scope of this manual.

- colour<-intensity: -> 0..255
    Grey value of the colour, computed as the weighted average

    	(20*R + 32*G + 18*B) / 70

    which closely matches ITU‑R BT.601 luma.  Useful for
    converting colour to a monochrome substitute.

- colour<-distance: colour|int -> int
    Perceptual distance to the argument colour (or the encoded RGB
    integer), computed as CIEDE2000 ΔE₀₀ in CIE Lab space.  Smaller
    is closer; values up to about 2 are considered visually
    indistinguishable.

- colour<-hilite: factor=[0.0..1.0] -> colour
- colour<-reduce: factor=[0.0..1.0] -> colour
    Return a lighter (`<-hilite`) or darker (`<-reduce`) variant.
    Used by class `elevation` to derive the lit and shadow sides of
    a 3-D box.  `0.0` returns the colour unchanged; `1.0` returns
    white (`<-hilite`) or black (`<-reduce`).  Defaults come from
    the `hilite_factor` / `reduce_factor` class variables.

    The derived colour is associated with the original; asking for
    the same modification again returns the cached object, and
    destroying the original lets the derivative go.

- colour<-fade: factor=[0.0..1.0] -> colour
    Return a colour with the same RGB but alpha multiplied by
    `factor`.  `0.0` yields a fully transparent variant; `1.0`
    leaves alpha unchanged.  The default factor comes from the
    `fade_factor` class variable (0.5).

    Complements `graphical->opacity`: setting the fill or pen to a
    faded colour lets a single graphical carry both an alpha
    component and its shape/stroke identity, without a global
    graphical opacity slot.  Like `<-hilite` and `<-reduce`, the
    result is cached against the source colour via `@colours`.

- colour<-convert: name -> colour
    Type-checker hook.  First looks the name up in `@colours`
    (already-constructed colours).  Otherwise:

    - If the name starts with `#` and has 3, 4, 6 or 8 hex digits
      after it, interpret it as a CSS hex literal:

    	#c0453f    ->  R=192, G=69, B=63, A=255

      With three or four hex digits, each digit is doubled
      (`#abc` ≡ `#aabbcc`).  With four or eight digits the trailing
      byte is the alpha channel.

    - Otherwise the name is passed to the OS colour database for
      resolution.

- colour<-lookup: [name|int], red=[0..360], green=[0..255], blue=[0..255], alpha=[0..255], model=[{rgb,hsv}] -> colour
    Return an existing colour from `@colours` matching the
    arguments, or fail.  Used internally to deduplicate equivalent
    colours; mirrors the signature of `->initialise`.

- colour<-storage_reference: -> name
    If the colour is `<-kind: named`, return its `<-name`.
    Otherwise return `#RRGGBB` (or `#RRGGBBAA` when alpha is not
    255).  The result is suitable as input to `<-convert`.

    See also `object <-storage_reference`, `object ->save_in_file`
    and `file <-object`.
