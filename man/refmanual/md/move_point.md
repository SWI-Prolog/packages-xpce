# class move_point {#class-move_point}

A move_point is a point of a path that is reached without drawing,
starting a new sub-path.  Sub-paths allow for shapes with holes:

	new(P, path),
	send_list(P, append, [ point(0,0), point(100,0), point(100,100),
	                       point(0,100), move_point(25,25), point(75,25),
	                       point(75,75), point(25,75) ]),
	send(P, closed, @on),
	send(P, fill_rule, even_odd),
	send(P, fill, colour(orange)).

See also class path and `path<-fill_rule`.

## Instance variables {#class-move_point-instvars}

- move_point<-closed: bool*
    Whether the sub-path that starts here is closed.  If @nil, it is
    closed if the path is (see `path<-closed`).

    **Defaults**: @nil

## Send methods {#class-move_point-send}

- move_point->initialise: x=[int], y=[int], closed=[bool]*
    Create a point that starts a sub-path.
