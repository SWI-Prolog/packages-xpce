# class spatial {#class-spatial}

A spatial describes how two rectangular areas are related.  In
principle, spatials can be attached to any two objects that understand
the messages <-area to obtain their area and ->set to set their area.

In it's full glory, a spatial consists of six expressions that are
organised in two pairs of two and two singles.  One pair describe the
relative X-positions, the other pair the relative Y position and the two
remaining expressions describe the relation in their width and height.

The first pair is `x_from`, `x_to`; both `=` objects (equations) describe
the X-coordinate `xref` of the *reference-point* of each area.  The
spatial will attempt to keep the values of `xref` of both equations
equal.  When evaluated, the variables `x` and `w` are refer to the
corresponding attributes of the areas.

The second pair is `y_from, y_to`.  It is similar to the previous pair,
but these equations describe the Y-coordinate `yref` of the reference
point.  `y` and `h` are the active variables.

The equations `w_to` and `h_to` describe the relation between their
sizes.  The variable `w` refers to the width of the first object, `w2`
or the second.  Similar, `h` is the height of the first and `h2` of the
second object.

Any of these equations may be @default, indicating *no* relation
between the corresponding arguments.

Note that spatials are reusable.  Use this feature to reach at readable
code:

	:- pce_global(@center, make_center_spatial).

	make_center_spatial(S) :-
		new(S, spatial(xref = x+w/2, yref = y+h/2,
		               xref = x+w/2, yref = y+h/2)).

	...
	new(_, constraint(Box, Text, @center))
	...

Or, to put a label-box just above a graphical at the left side, half the
width of the graphical.

	:- pce_global(@top_left_label_spatial,
		      new(spatial(xref = x, yref = y,
				  xref = x, yref = y + h,
				  w2 = w/2))).


**Hints**

- Spatials are useful, but don't exaggerate. Certainly under the current
  implementation, complex networks of spatials lead to very slow response.
  Figure objects (allowing for nested graphicals, similar to grouping in
  many drawing applications) are often a good alternative.

- When relating many objects to another, of which one generally
  moves/resizes, relate *all* other objects to this one object. This makes
  the result better predictable.

@see =<-var
@see class figure
@see class expression


## Instance variables {#class-spatial-instvars}

- spatial<->w_to: =*
- spatial<->h_to: =*
    Equation (= object) relating the following variables:

	| w  | `area <-width` of the `constraint <-from` graphical  |
	| h  | `area <-height` of the `constraint <-from` graphical |
	| w2 | `area <-width` of the `constraint <-to` graphical    |
	| h2 | `area <-height` of the `constraint <-to` graphical   |

    Examples:

	| w = w2      | Equally wide                         |
	| w = w2 + 10 | `constraint <-to` is 10 pixels wider |

- spatial<->x_to: =*
- spatial<->y_from: =*
- spatial<->y_to: =*
- spatial<->x_from: =*
    These equations define the *reference* points on both graphical objects
    constrained.  They should be defaulted or specified in pairs: if
    <-x_from is specified so must <-x_to and likewise for <-y_from and
    <-y_to.

    The reference points of both objects are constrained to be at the same
    location.

    During the evaluation of the = object, the following variables are
    available:

	| xref	| the x coordinate of the reference-point   |
	| yef	| the y coordinate of the reference-point   |
	| x	| <-x of the graphical's `graphical <-area` |
	| y	| <-y					    |
	| w	| <-width				    |
	| h	| <-height				    |


