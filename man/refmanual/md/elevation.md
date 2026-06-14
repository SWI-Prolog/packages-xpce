# class elevation {#class-elevation}

General description of an elevated area.  Elevations are mainly
used to describe the edges of dialog_item objects.  Elevations
are reusable named objects.  In its most simple form, an
elevation object is simply created from its <-height:

	new(X, elevation(1))

describes a <-kind: 3d elevation suitable for a colour
environment.

Elevations come in two flavours: <-kind: 3d will paint a hilited
edge on the top and left sides of the object and a reduced
edge on the bottom and right sides.  By default these colours
are computed from the context background colour using
`colour <-hilite` and `colour <-reduce`.

A <-kind: shadow elevation is designed for monochrome displays.
It will paint a shadow at the bottom and right side of the
objects to simulate the object elevated above the surface.


## Instance variables {#class-elevation-instvars}

- elevation<-colour: [{hilited,reduced}|colour|pixmap]
    Colour or pixmap object used to fill the top of the elevated
    area.  When @default, only the edges will be painted.

- elevation<-height: int
    Height above the surface.  Actually this is the with of the
    edges painted with <-relief and <-shadow.

- elevation<-kind: {3d,shadow}
    How the elevation is realised.  The default is _3d_, which
    implies the top- and left-edges are painted using the
    <-relief colour and the bottom- and right sides using the
    <-shadow colour.  <-kind `shadow` will paint a shadow
    image at the bottom-right side of the object.

- elevation<-name: name|int*
    Identifier for reuse.  All elevation objects created with a non
    @nil <-name are stored in the table @elevations.   <-convert
    will convert elevation identifiers to elevation objects.

- elevation<-relief: [colour|pixmap]
    Colour/pixmap used at `light` side.  When @default, the
    elevation object will invoke `colour <-hilite` using the
    current background.  See also <-shadow.

- elevation<-shadow: [colour|pixmap]
    Colour used to paint the right and bottom edges of the object.
    When @default, the elevation object will invoke `colour
    <-reduce' on the current background colour.


## Send methods {#class-elevation-send}

- elevation->initialise: name=[name|int]*, height=[int], colour=[{hilited,reduced}|colour|pixmap], relief=[colour|pixmap], shadow=[colour|pixmap], kind=[{3d,shadow}], background=[{reduced}|colour|pixmap]
    Create from <-name and description parameters.  In most cases
    the user will create an elevation object simply from its <-height:

    	?- new(X, elevation(1)).

    This will create a default elevation object.  See the various
    attributes for the interpretation of the default values.

- elevation->unlink
    If the <-name is not @nil, delete this elevation from the reuse
    table @elevations.


## Get methods {#class-elevation-get}

- elevation<-lookup: name=[name|int]*, height=[int], colour=[{hilited,reduced}|colour|pixmap], relief=[colour|pixmap], shadow=[colour|pixmap], kind=[{3d,shadow}], background=[{reduced}|colour|pixmap] -> elevation

- elevation<-convert: name|int -> elevation
    The <-convert and <-lookup methods realise reuse of elevation
    objects from the @elevations table.

- elevation<-modify: attribute={height,colour,relief,shadow,kind,background}, value=any -> elevation
    As elevation objects are reusable if they are named (see
    ->initialise), attributes of named elevation objects cannot be
    modified.  This method returns a new elevation object with the
    requested attribute modified if the <-name is not @nil and just
    modifies the attribute otherwise.

