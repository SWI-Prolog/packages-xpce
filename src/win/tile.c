/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status	layoutTile(TileObj t, Int ax, Int ay, Int aw, Int ah);
static status	computeTile(TileObj t);
static void	relatedTile(TileObj t1, TileObj t2, Any manager);
static void	invalidateCanResizeTile(TileObj t);
static status	ICanResizeTile(TileObj t, Name dir);

#define Max(a, b)	(valInt(a) > valInt(b) ? (a) : (b))
#define Min(a, b)	(valInt(a) < valInt(b) ? (a) : (b))
#define MAX_TILE_MEMBERS 200
#define INT_INFINITE toInt(PCE_MAX_INT)


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A tile manages a set of  non-overlapping areas.  It  is used to manage
the areas of the windows in a frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		/********************************
		*         CREATE/DESTROY	*
		********************************/

static status
initialiseTile(TileObj t, Any object, Int w, Int h)
{ if ( notNil(object) )
  { if ( isDefault(w) )
      w = get(object, NAME_width, EAV);
    if ( isDefault(h) )
      h = get(object, NAME_height, EAV);
  }

  assign(t, enforced,	 OFF);		/* building stage */
  assign(t, idealWidth,  w);		/* ideal size */
  assign(t, idealHeight, h);
  assign(t, horStretch,  toInt(100));	/* stretchabilities */
  assign(t, horShrink,   toInt(100));
  assign(t, verStretch,  toInt(100));
  assign(t, verShrink,   toInt(100));
  assign(t, canResize,   DEFAULT);
  assign(t, resized,     NAME_none);
  assign(t, orientation, NAME_none);
  assign(t, members, NIL);		/* subtiles */
  assign(t, super,   NIL);		/* super-tile */
  assign(t, manager, NIL);		/* managing frame/device */
  assign(t, object,  object);		/* managed object */
					/* Actual area */
  assign(t, area, newObject(ClassArea, ZERO, ZERO, w, h, EAV));

  return obtainClassVariablesObject(t);
}


static status
unlinkTile(TileObj t)
{ if ( notNil(t->members) )
  { clearChain(t->members);
    assign(t, members, NIL);
  }

  succeed;
}

		/********************************
		*         UNRELATE TILE		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A member of this tile has been deleted.  If there is only one member
left, remove this tile for the hierarchy.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
cleanTile(TileObj t)
{ if ( t->members->size == ONE )
  { TileObj child = getHeadChain(t->members);

    if ( notNil(t->super) )
    { TileObj super = t->super;

      replaceChain(super->members, t, child);
      assign(child, super, super);
    } else
    { assign(child, super, NIL);
      assign(child, manager, t->manager);
      freeObject(t);
    }

    invalidateCanResizeTile(getRootTile(child));
    computeTile(getRootTile(child));
  } else
  { invalidateCanResizeTile(getRootTile(t));
    computeTile(t);
  }

  succeed;
}


status
unrelateTile(TileObj t)
{ if ( notNil(t->super) )
  { TileObj super = t->super;

    deleteChain(t->super->members, t);
    assign(t, super, NIL);
    assign(t, manager, NIL);
    invalidateCanResizeTile(t);
    cleanTile(super);
  }

  succeed;
}

		/********************************
		*    CREATING TILE HIERARCHY	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Splitting a tile puts a new super  in   its  place among its siblings, so
the super must present to them what   the  tile presented: its ideal size
along the axis its parent divides.   computeTile() cannot know that: on
that axis it takes the largest of the  members, which is the size the new
window asks for and not the size there is room for.

It does not take over the tile's  stretchability there.  A tile that the
user has given a size (see setTile())  has none left, and inheriting that
would freeze the new pair: no longer  resizable and holding on to the new
member's ideal size.  A split rearranges, so the pair starts fresh.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define TILE_STRETCH toInt(100)

/* Splitting one tile must not move the others.  Their ideal sizes are
 * what a layout starts from, and those may have drifted from what is on
 * the screen: a tile the user has given a size holds it through a zero
 * stretch rather than through its ideal.  Commit the layout as it stands
 * before the split, so that the split divides `t' and leaves the rest of
 * the row alone.
 */

static void
commitLayoutTile(TileObj t)
{ if ( notNil(t->super) && t->super->enforced == ON )
  { Name orientation = t->super->orientation;
    Cell cell;

    for_cell(cell, t->super->members)
    { TileObj t2 = cell->value;
      Int size = (orientation == NAME_horizontal ? t2->area->w
						 : t2->area->h);

      if ( valInt(size) <= 0 )		/* not laid out (yet) */
	continue;

      if ( orientation == NAME_horizontal )
	assign(t2, idealWidth, size);
      else
	assign(t2, idealHeight, size);
    }
  }
}


static void
splitOfTile(TileObj super, TileObj t, Name orientation)
{ if ( orientation == NAME_horizontal )	/* parent divides the height */
  { assign(super, idealHeight, t->idealHeight);
    assign(super, verStretch,  TILE_STRETCH);
    assign(super, verShrink,   TILE_STRETCH);
  } else				/* parent divides the width */
  { assign(super, idealWidth, t->idealWidth);
    assign(super, horStretch, TILE_STRETCH);
    assign(super, horShrink,  TILE_STRETCH);
  }
}


static TileObj
toTile(Any obj)
{ if ( instanceOfObject(obj, ClassTile) )
    return (TileObj) obj;

  return answerObject(ClassTile, obj, EAV);
}


static status
computeTile(TileObj t)
{ Int w   = ZERO;
  Int h   = ZERO;
  Int vsh=ZERO, vst=ZERO, hsh=ZERO, hst=ZERO;

  DEBUG(NAME_tile, Cprintf("computeTile(%s) --> ", pp(t)));

  if ( t->orientation == NAME_horizontal )
  { Cell cell;

    hsh = ZERO;
    hst = ZERO;
    vsh = INT_INFINITE;
    vst = INT_INFINITE;

    for_cell(cell, t->members)
    { TileObj t2 = cell->value;

      w = add(w, t2->idealWidth);
      h = Max(h, t2->idealHeight);
      hsh = Max(hsh, t2->horShrink);
      hst = Max(hst, t2->horStretch);
      vsh = Min(vsh, t2->verShrink);
      vst = Min(vst, t2->verStretch);
      w = add(w, t->border);
    }

    assign(t, idealWidth,  w);
    assign(t, horShrink,   hsh);
    assign(t, horStretch,  hst);
    assign(t, idealHeight, h);
    assign(t, verShrink,   vsh);
    assign(t, verStretch,  vst);
  } else if ( t->orientation == NAME_vertical )
  { Cell cell;

    vsh = ZERO;
    vst = ZERO;
    hsh = INT_INFINITE;
    hst = INT_INFINITE;

    for_cell(cell, t->members)
    { TileObj t2 = cell->value;

      w = Max(w, t2->idealWidth);
      h = add(h, t2->idealHeight);
      hsh = Min(hsh, t2->horShrink);
      hst = Min(hst, t2->horStretch);
      vsh = Max(vsh, t2->verShrink);
      vst = Max(vst, t2->verStretch);
      h = add(h, t->border);
    }

    assign(t, idealWidth,  w);
    assign(t, horShrink,   hsh);
    assign(t, horStretch,  hst);
    assign(t, idealHeight, h);
    assign(t, verShrink,   vsh);
    assign(t, verStretch,  vst);
  }

  DEBUG(NAME_tile,
	if ( t->orientation == NAME_horizontal ||
	     t->orientation == NAME_vertical )
	  Cprintf("%s, %dx%d, -%dx+%d, -%dy+%d\n",
		  pp(t->orientation),
		  valInt(w), valInt(h),
		  valInt(hsh), valInt(hst),
		  valInt(vsh), valInt(vst));
	else
	  Cprintf("\n"));

  succeed;
}


/* ->border used to be the only border there was, so it sets both: whoever
   asks for no border at all is asking for none around the hierarchy
   either.  ->border_root afterwards is what tells them apart.
*/

static status
borderTile(TileObj t, Int border)
{ assign(t, border, border);
  assign(t, border_root, border);

  succeed;
}


/* A tile put above another takes over its appearance.  It may be the root
   of the hierarchy now, and <-border_root is the manager's business.
*/

static void
inheritTile(TileObj t, TileObj from)
{ assign(t, border,      from->border);
  assign(t, border_root, from->border_root);
  assign(t, enforced,    from->enforced);
}


static status
nonDelegatingLeftRightTile(TileObj t, TileObj t2, Name where)
{ TileObj super;
  Any manager;
  int split = FALSE;

  t = getRootTile(t);
  if ( !(manager=managerTile(t2)) )
    manager = managerTile(t);

  if ( notNil(t2->super) && t2->super->orientation == NAME_horizontal )
  { super = t2->super;

    if ( where == NAME_right )
      insertAfterChain(super->members, t, t2);
    else
      insertBeforeChain(super->members, t, t2);

    assign(t, super, super);
  } else
  { Chain ch;

    commitLayoutTile(t2);
    super = newObject(ClassTile, NIL, ZERO, ZERO, EAV);

    if ( where == NAME_right )
      ch = newObject(ClassChain, t2, t, EAV);
    else
      ch = newObject(ClassChain, t, t2, EAV);

    assign(super, orientation, NAME_horizontal);
    assign(super, members, ch);
    assign(super->area, x, t->area->x);
    assign(super->area, y, t->area->y);
    if ( notNil(t2->super) )
    { replaceChain(t2->super->members, t2, super);
      assign(super, super, t2->super);
    }
    assign(t2, super, super);
    assign(t,  super, super);
    inheritTile(super, t2);
    split = TRUE;
  }

  relatedTile(t, t2, manager);

  TRY(computeTile(super));
  if ( split )
    splitOfTile(super, t2, NAME_horizontal);

  succeed;
}


static status
leftTile(TileObj t, Any obj, BoolObj delegate)
{ TileObj t2 = toTile(obj);
  TileObj super;
  Any manager;

  if ( delegate == OFF )
    return nonDelegatingLeftRightTile(t, t2, NAME_left);

					/* One already has a super tile */
  if ( notNil(t->super)  &&
       (t->super->orientation  == NAME_vertical ||
	notNil(t->super->super)) )
    return leftTile(t->super, t2, ON);
  if ( notNil(t2->super) &&
       (t2->super->orientation == NAME_vertical ||
	notNil(t2->super->super)) )
    return leftTile(t, t2->super, ON);
					/* both left-to-right */
  if ( notNil(t->super) && notNil(t2->super) )
    return leftTile(t->super, t2->super, ON);

  if ( !(manager=managerTile(t2)) )
    manager = managerTile(t);

  if ( notNil(t->super) )
  { super = t->super;
    appendChain(super->members, t2);
  } else if ( notNil(t2->super) )
  { super = t2->super;
    prependChain(super->members, t);
  } else
  { super = newObject(ClassTile, NIL, ZERO, ZERO, EAV);
    assign(super, orientation, NAME_horizontal);
    assign(super, members, newObject(ClassChain, t, t2, EAV));
    assign(super->area, x, t->area->x);
    assign(super->area, y, t->area->y);
  }

  assign(t,  super, super);
  assign(t2, super, super);
  relatedTile(t, t2, manager);
  computeTile(super);

  succeed;
}


static status
rightTile(TileObj t, Any obj, BoolObj delegate)
{ if ( delegate == OFF )
    return nonDelegatingLeftRightTile(t, toTile(obj), NAME_right);

  return leftTile(toTile(obj), t, ON);
}

					/* can be merged with left version */
static status
nonDelegatingAboveBelowTile(TileObj t, TileObj t2, Name where)
{ TileObj super;
  Any manager;
  int split = FALSE;

  t = getRootTile(t);
  if ( !(manager=managerTile(t2)) )
    manager = managerTile(t);

  if ( notNil(t2->super) && t2->super->orientation == NAME_vertical )
  { super = t2->super;

    if ( where == NAME_below )
      insertAfterChain(super->members, t, t2);
    else
      insertBeforeChain(super->members, t, t2);

    assign(t, super, super);
  } else
  { Chain ch;

    commitLayoutTile(t2);
    super = newObject(ClassTile, NIL, ZERO, ZERO, EAV);

    if ( where == NAME_below )
      ch = newObject(ClassChain, t2, t, EAV);
    else
      ch = newObject(ClassChain, t, t2, EAV);

    assign(super, orientation, NAME_vertical);
    assign(super, members, ch);
    assign(super->area, x, t->area->x);
    assign(super->area, y, t->area->y);
    if ( notNil(t2->super) )
    { replaceChain(t2->super->members, t2, super);
      assign(super, super, t2->super);
    }
    assign(t2, super, super);
    assign(t,  super, super);
    inheritTile(super, t2);
    split = TRUE;
  }

  relatedTile(t, t2, manager);

  TRY(computeTile(super));
  if ( split )
    splitOfTile(super, t2, NAME_vertical);

  succeed;
}


static status
aboveTile(TileObj t, Any obj, BoolObj delegate)
{ TileObj t2 = toTile(obj);
  TileObj super;
  Any manager;

  if ( delegate == OFF )
    return nonDelegatingAboveBelowTile(t, t2, NAME_above);

					/* One already has a super tile */
  if ( notNil(t->super)  &&
       (t->super->orientation  == NAME_horizontal ||
	notNil(t->super->super)) )
    return aboveTile(t->super, t2, ON);
  if ( notNil(t2->super) &&
       (t2->super->orientation == NAME_horizontal ||
	notNil(t2->super->super)) )
    return aboveTile(t, t2->super, ON);

  if ( notNil(t->super) && notNil(t2->super) )
    return aboveTile(t->super, t2->super, ON);

  if ( !(manager=managerTile(t2)) )
    manager = managerTile(t);

  if ( notNil(t->super) )
  { super = t->super;
    appendChain(super->members, t2);
  } else if ( notNil(t2->super) )
  { super = t2->super;
    prependChain(super->members, t);
  } else
  { super = newObject(ClassTile, NIL, ZERO, ZERO, EAV);
    assign(super, orientation, NAME_vertical);
    assign(super, members, newObject(ClassChain, t, t2, EAV));
    assign(super->area, x, t->area->x);
    assign(super->area, y, t->area->y);
  }

  assign(t,  super, super);
  assign(t2, super, super);

  relatedTile(t, t2, manager);
  computeTile(super);

  succeed;
}


static status
belowTile(TileObj t, Any obj, BoolObj delegate)
{ if ( delegate == OFF )
    return nonDelegatingAboveBelowTile(t, toTile(obj), NAME_below);

  return aboveTile(toTile(obj), t, ON);
}


		/********************************
		*        LAYOUT MANAGEMENT	*
		********************************/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The <-manager of a tile hierarchy is the object that owns it: the frame
whose windows these are, or -- see class tab_frame in library(tab_frame)
-- the device that displays them.  It is what makes `window ->below' and
friends work for both: relateWindow() asks the target hierarchy who runs
it and leaves the attaching and detaching to that object, which does so
with ->attach_window and ->detach_window.

The manager is kept on the root, as that is the only tile that is there
for as long as the hierarchy is.  Relating two hierarchies may introduce
a new root and dropping a member may take one away, so both move it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Any
managerTile(TileObj t)
{ t = getRootTile(t);

  return isNil(t->manager) ? NULL : t->manager;
}


void
setManagerTile(TileObj t, Any manager)
{ if ( manager )
    assign(getRootTile(t), manager, manager);
}


/* <-can_resize says whether the gap after a tile can be dragged.  It is
 * derived from where the tile sits among its siblings and from what they
 * can do, and cached, as it is asked for on every pointer move.  Relating
 * or dropping a tile changes both, for tiles all over the hierarchy, so
 * the cache goes.  An explicit ->can_resize goes with it: it answers a
 * question about an arrangement that no longer holds.
 */

static void
invalidateCanResizeTile(TileObj t)
{ assign(t, canResize, DEFAULT);

  if ( notNil(t->members) )
  { Cell cell;

    for_cell(cell, t->members)
      invalidateCanResizeTile(cell->value);
  }
}


/* Called after t1 and t2 have been related.  The tiles that used to be
 * roots may no longer be one, so the manager moves to the new root, and
 * the hierarchy has to work out again what it can resize.
 */

static void
relatedTile(TileObj t1, TileObj t2, Any manager)
{ if ( notNil(t1->super) )
    assign(t1, manager, NIL);
  if ( notNil(t2->super) )
    assign(t2, manager, NIL);

  setManagerTile(t1, manager);
  invalidateCanResizeTile(getRootTile(t1));
}


static Any
getManagerTile(TileObj t)
{ Any manager = managerTile(t);

  if ( manager )
    answer(manager);

  fail;
}


static status
managerTileMethod(TileObj t, Any manager)
{ assign(getRootTile(t), manager, manager);

  succeed;
}


TileObj
getRootTile(TileObj t)
{ while(notNil(t->super))
    t = t->super;

  answer(t);
}


status
distribute_stretches(stretch *s, int n, int w)
{ int ok = FALSE;
  int i;
  int maxloop = n;

  if ( w <= 0 )
  { for(i=0; i<n; i++)
      s[i].size = 0;

    succeed;
  }

  while( !ok && maxloop-- > 0 )
  { int total_ideal = 0, total_stretch = 0, total_shrink = 0;
    int grow, growed;
    int is_pos;
    Stretch sp;

    for(i = 0; i < n; i++)
    { total_ideal   += s[i].ideal;
      total_stretch += s[i].stretch;
      total_shrink  += s[i].shrink;
      DEBUG(NAME_tile, Cprintf("%-2d %-3d <- %-3d -> %-3d\n",
			       i, s[i].shrink, s[i].ideal, s[i].stretch));
    }

    grow = w - total_ideal;

    if ( grow < 0 && total_shrink == 0 )
    { for(is_pos = 0, i = 0; i < n; i++)	/* who still has something */
        if ( (s[i].ideal > 0 || s[i].shrink > 0) &&
	     s[i].ideal > s[i].minimum )	/* to give */
	  is_pos++;
      if ( is_pos == 0 )		/* nobody can: share it out over all */
	is_pos = n;
    } else
      is_pos = n;

    DEBUG(NAME_tile, Cprintf("grow = %d, is_pos = %d\n", grow, is_pos));

    for(growed = 0, i = 0; i < n; i++)
    { int grow_this;

      if ( grow >= 0 )
      { grow_this = (total_stretch==0 ? grow / n
				      : (grow * s[i].stretch) / total_stretch);
      } else
      {	/* a tile that is as small as it may get has nothing to give:
	   what it would have given has to come from the others */
	if ( (s[i].ideal == 0 && s[i].shrink == 0) ||
	     s[i].ideal <= s[i].minimum )
	  grow_this = 0;
	else
	  grow_this = (total_shrink ==0 ? grow / is_pos
					: (grow * s[i].shrink) / total_shrink);
      }
      s[i].size = s[i].ideal + grow_this;
      growed += grow_this;
    }

    if ( grow != growed )
    { int do_grow = (grow > 0);
      int stretchables;
      int per_stretchable;
      int stretchall;
      int m;

      DEBUG(NAME_tile, Cprintf("Show grow %d, done %d\n", grow, growed));

      if ( grow < 0 )			/* normalise */
      { grow = -grow;
	growed = -growed;
      }

      for(i=0, stretchables=0; i < n; i++)
      { if ( (do_grow ? s[i].stretch : s[i].shrink) > 0)
	  stretchables++;
      }
					/* No one wants, so all have to */
      if ( stretchables == 0 )
      { stretchables = is_pos;
	stretchall = FALSE;
      } else
      { stretchall = TRUE;
      }
					/* distribute outside --> inside */
      per_stretchable = (grow - growed + stretchables - 1) / stretchables;

      for( i=0, m=n; growed < grow && m-- > 0; i++ )
      { int j = (i%2 ? i : n - i - 1);

	if ( stretchall ||
	     (do_grow ? s[j].stretch : s[i].shrink) > 0 )
	{ int to_grow = (grow - growed < per_stretchable ? grow - growed
							 : per_stretchable);

					/* don't go below the minimum */
	  if ( !do_grow	&& to_grow > s[j].size - s[j].minimum )
	    to_grow = s[j].size - s[j].minimum;
	  if ( to_grow < 0 )		/* already past zero: leave it to the
					   minimum check below, which pins it
					   and shares the rest out again */
	    to_grow = 0;

	  s[j].size += (do_grow ? to_grow : -to_grow);
	  growed += to_grow;
	}
      }
    }

    ok = TRUE;
    for(sp=s, i = 0; i < n; i++, sp++)	/* Accept min and max */
    { if ( sp->size < sp->minimum )
      { sp->ideal = sp->minimum;
        sp->shrink = 0;
	DEBUG(NAME_tile, Cprintf("%d is too small; setting to %d\n",
				 i, sp->minimum));
	ok = FALSE;
      } else if ( sp->size > sp->maximum )
      { sp->ideal = sp->maximum;
	sp->stretch = 0;
	DEBUG(NAME_tile, Cprintf("%d is too large; setting to %d\n",
				 i, sp->maximum));
	ok = FALSE;
      }
    }
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Combine stretches in their `natural' direction.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
sum_stretches(stretch *sp, int len, stretch *r)
{ r->ideal   = 0;
  r->minimum = 0;
  r->maximum = 0;
  r->shrink  = 0;
  r->stretch = 0;

  for( ; len-- > 0; sp++)
  { r->shrink  = max(r->shrink, sp->shrink);
    r->stretch = max(r->stretch, sp->stretch);
    r->ideal   += sp->ideal;
    r->minimum += sp->minimum;
    if ( r->maximum < INT_MAX )
    { r->maximum += sp->maximum;
      if ( r->maximum > INT_MAX || r->maximum < 0 )
	r->maximum = INT_MAX;
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Join a sequence of stretches that  have  to   be  forced  to be the same
width. Minimum and maximum are easy. The ideal is deduced by starting at
the average and then using the applicable   stretch or schrink to arrive
at a new weighted average. This process is iterated for at most 4 times.

The shrink and stretchability of the join  should be a weighted average,
where low values have more weight.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
join_stretches(stretch *stretches, int len, stretch *r)
{ stretch *sp;
  int i;
  int avg;
  int maxloop;

  r->minimum = 0;
  r->maximum = INT_MAX;

  DEBUG(NAME_stretch, Cprintf("Joining %d stretches\n", len));

  for(sp=stretches, i=len ; i-- > 0; sp++)
  { r->minimum = max(r->minimum, sp->minimum);
    r->maximum = min(r->maximum, sp->maximum);
    DEBUG(NAME_stretch, Cprintf("\t%d %d..%d <-%d ->%d\n",
				sp->ideal,
				sp->minimum, sp->maximum,
				sp->shrink, sp->stretch));
  }

  for(avg=0, sp=stretches, i=len; i-- > 0; sp++)
    avg += sp->ideal;
  avg /= len;

  for(maxloop = 4; maxloop-- > 0;)
  { int wavg = 0;
    int tw = 0;

    for(sp=stretches, i=len; i-- > 0; sp++)
    { int w0;

      w0 = (sp->ideal < avg ? sp->stretch : sp->shrink);

      if ( w0 != 0 )
	w0 = max(1000/w0, 1);
      else
	w0 = 100000;

      wavg += sp->ideal*w0;
      tw   += w0;
    }
    wavg = (wavg+tw/2)/tw;
    if ( wavg == avg )
      break;
    avg = wavg;
  }

  r->ideal = avg;

  { int stretchavg = 0, shrinkavg = 0;
    int twstretch = 0, twshrink = 0;

    for(sp=stretches, i=len; i-- > 0; sp++)
    { int w0;

      w0 = (sp->stretch ? max(1000/sp->stretch, 1) : 100000);
      stretchavg += sp->stretch*w0;
      twstretch += w0;

      w0 = (sp->shrink ? max(1000/sp->shrink, 1) : 100000);
      shrinkavg += sp->shrink*w0;
      twshrink += w0;
    }

    r->shrink  = (stretchavg+twstretch/2)/twstretch;
    r->stretch = (shrinkavg+twshrink/2)/twshrink;
  }

  DEBUG(NAME_stretch, Cprintf("--> %d %d..%d <-%d ->%d\n",
			      r->ideal,
			      r->minimum, r->maximum,
			      r->shrink, r->stretch));
}



/* A tile is given a size for either of two reasons.  The window in it
 * asks for the room its content needs -- a dialog that has laid its
 * items out afresh sends ->request_geometry, see requestGeometryWindow()
 * in window.c -- or somebody hands out sizes: the user dragging the gap
 * after a tile, or a manager giving its panes their share.  The latter
 * arrives through `tile ->width' and ->height and is what `hand' says.
 *
 * Only a size given by hand makes the tile hold on to it: a menu bar
 * that grows a row asks for the room and is laid out again like anything
 * else.  Holding on to a size means giving up stretch and shrink, which
 * is what ICanResizeTile() reads to see whether the user may drag an
 * edge, so a tile that could be resized before is marked <-resized to say
 * that it still can.  It is only ever a mark on what a tile could already
 * do: a strip that was fixed stays fixed, or the user could drag it shut
 * -- and a strip dragged shut cannot be opened again.
 */

static void
markResizedTile(TileObj t, Name dim)	/* NAME_width or NAME_height */
{ if ( t->resized == NAME_none )
    assign(t, resized, dim);
  else if ( t->resized != dim )
    assign(t, resized, NAME_both);
}


static int
isResizedTile(TileObj t, Name dim)
{ return t->resized == dim || t->resized == NAME_both;
}


/* Must the tiles before `t' hold on to the size they have?  A size given
 * by hand is a share: it is held, and the ones before it hold theirs, or
 * the layout hands the room straight back.  A window asking for the room
 * its content needs is not, and then there has to be something after it
 * to give way: freezing everything before the last tile of a stack buys
 * nothing and leaves the stack without stretch at all, after which a
 * resize of the frame is shared out over all of it, dialogs included.
 */

static int
freezeBeforeTile(TileObj t, int hand)
{ Cell tail;

  if ( hand )
    return TRUE;

  tail = t->super->members->tail;
  return notNil(tail) && tail->value != t;
}


static status
set_tile(TileObj t, Int x, Int y, Int w, Int h, int hand)
{ TileObj super;

  DEBUG(NAME_tile,
	Cprintf("setTile(%s, %s, %s, %s, %s) ",
		pp(t), pp(x), pp(y), pp(w), pp(h));
	Cprintf("enforced = %s\n", pp(t->enforced)));

  /* Give a tile at least the border as its size, so it cannot become
   * too small to resize.  A size of exactly 0 means there is nothing to
   * show, though: keep that, or an empty window (e.g. a dialog holding
   * only a natively displayed menu_bar) still claims a visible strip.
   * A size given by hand does not say that: dragging a gap shut puts
   * the tile out of reach of the pointer and there is no way back.
   */
  if ( notDefault(w) && (hand || valInt(w) > 0) &&
       valInt(w) < valInt(t->border) )
    w = t->border;
  if ( notDefault(h) && (hand || valInt(h) > 0) &&
       valInt(h) < valInt(t->border) )
    h = t->border;

  if ( notDefault(w) )
  { assign(t, idealWidth, w);

    if ( t->enforced == ON && notNil(t->super) &&
	 freezeBeforeTile(t, hand) )
    { Cell cell;
      int before = TRUE;
      int hs = 0, hg = 0;

      for_cell(cell, t->super->members)
      { TileObj t2 = cell->value;

	if ( before )
	{ if ( t2 != t && valInt(t2->area->w) > 0 )
	    assign(t2, idealWidth, t2->area->w); /* the size they have: it is */
					/* not <-ideal_width once anything */
					/* has had to give way */
	  if ( hand && ICanResizeTile(t2, NAME_horizontal) )
	    markResizedTile(t2, NAME_width);	/* but stay resizable, see */
	  assign(t2, horStretch, ZERO);		/* ICanResizeTile() */
	  assign(t2, horShrink,  ZERO);	/* hold on to the size they have */
	  if ( t2 == t )
	    before = FALSE;
	} else
	{ hs += valInt(t2->horShrink);
	  hg += valInt(t2->horStretch);
	}
      }
      if ( hs == 0 || hg == 0 )
      { before = TRUE;

	for_cell(cell, t->super->members)
	{ TileObj t2 = cell->value;

	  if ( before )
	  { if ( t2 == t )
	      before = FALSE;
	  } else
	  { if ( hs == 0 )
	      assign(t2, horShrink, ONE);
	    if ( hg == 0 )
	      assign(t2, horStretch, ONE);
	  }
	}
      }
    }
  }
  if ( notDefault(h) )
  { assign(t, idealHeight, h);

    if ( t->enforced == ON && notNil(t->super) &&
	 freezeBeforeTile(t, hand) )
    { Cell cell;
      int before = TRUE;
      int vs = 0, vg = 0;

      for_cell(cell, t->super->members)
      { TileObj t2 = cell->value;

	if ( before )
	{ if ( t2 != t && valInt(t2->area->h) > 0 )
	    assign(t2, idealHeight, t2->area->h);	/* see above */
	  if ( hand && ICanResizeTile(t2, NAME_vertical) )
	    markResizedTile(t2, NAME_height);
	  assign(t2, verStretch, ZERO);
	  assign(t2, verShrink,  ZERO);
	  if ( t2 == t )
	    before = FALSE;
	} else
	{ vs += valInt(t2->verShrink);
	  vg += valInt(t2->verStretch);
	}
      }
      if ( vs == 0 || vg == 0 )
      { before = TRUE;

	for_cell(cell, t->super->members)
	{ TileObj t2 = cell->value;

	  if ( before )
	  { if ( t2 == t )
	      before = FALSE;
	  } else
	  { if ( vs == 0 )
	      assign(t2, verShrink, ONE);
	    if ( vg == 0 )
	      assign(t2, verStretch, ONE);
	  }
	}
      }
    }
  }

  if ( t->enforced != ON )
  { for(super = t->super; notNil(super); super = super->super)
      computeTile(super);
  } else
  { if ( notNil(t->super) )
      layoutTile(getRootTile(t), DEFAULT, DEFAULT, DEFAULT, DEFAULT);
    else
      layoutTile(t, x, y, w, h);
  }

  succeed;
}


status
setTile(TileObj t, Int x, Int y, Int w, Int h)
{ return set_tile(t, x, y, w, h, FALSE);
}


static status
unenforceTile(TileObj t)
{ assign(t, enforced, OFF);

  if ( notNil(t->members) )
  { Cell cell;

    for_cell(cell, t->members)
      unenforceTile(cell->value);
  }

  succeed;
}


status
enforceTile(TileObj t, BoolObj val)
{ if ( val == OFF )
  { unenforceTile(t);
    computeTile(t);
  } else
  { if ( t->enforced == OFF )
    { assign(t, enforced, ON);

      layoutTile(t, DEFAULT, DEFAULT, t->idealWidth, t->idealHeight);
    }
  }

  succeed;
}


/* A member that has no ideal size and cannot stretch will be laid out
 * with size 0.  Such a member must not get a separating border either,
 * or it contributes a visible gap while showing nothing.  This happens
 * for a dialog holding only a menu_bar that is displayed natively (see
 * ws_has_native_menubar()): without this the frame shows a double
 * border above its content.
 */

static int
non_empty_tiles(TileObj t)
{ Cell cell;
  int n = 0;

  for_cell(cell, t->members)
  { TileObj t2 = cell->value;

    if ( t->orientation == NAME_horizontal )
    { if ( valInt(t2->idealWidth) > 0 || valInt(t2->horStretch) > 0 )
	n++;
    } else
    { if ( valInt(t2->idealHeight) > 0 || valInt(t2->verStretch) > 0 )
	n++;
    }
  }

  return n;
}


/* <->hor_shrink and <->ver_shrink are an encouragement to get smaller, and
   distribute_stretches() shares what has to be given up in proportion to
   it.  On its own that asks a small tile for as many pixels as a large
   one, which drives the small one to nothing: two terminals beside a
   thread monitor left the monitor with no height at all.  What a tile has
   to give is the encouragement over what it is: turn it into that.
*/

static int
shrinkability(int weight, int ideal)
{ if ( weight <= 0 || ideal <= 0 )
    return 0;

  return (weight * ideal + 99) / 100;	/* never 0 for a tile that may give */
}


/* A tile is never laid out smaller than this, so that the window in it
   stays visible and can be grabbed: a pane squeezed to nothing cannot be
   dragged back.  A tile that wants less than this -- a menu bar is as
   high as its buttons and no more -- keeps what it wants; the minimum is
   an offer of room, not a demand for it.
*/

#define MIN_TILE_SIZE 20

static int
tile_minimum(TileObj t, bool horizontal)
{ int ideal = valInt(horizontal ? t->idealWidth : t->idealHeight);

  if ( t->orientation == NAME_none ||	/* a window, or nothing inside */
       isNil(t->members) )		/* to ask */
    return ideal < MIN_TILE_SIZE ? ideal : MIN_TILE_SIZE;

  { bool along = ((t->orientation == NAME_horizontal) == horizontal);
    int border = valInt(t->border);
    int nvis   = non_empty_tiles(t);
    int min    = 0;
    Cell cell;

    for_cell(cell, t->members)
    { int m = tile_minimum(cell->value, horizontal);

      if ( along )
	min += m;			/* they sit next to each other */
      else if ( m > min )
	min = m;			/* they cover one another */
    }

    if ( along && nvis > 1 )
      min += border * (nvis-1);

    return min < ideal ? min : ideal;	/* never more than it asks for */
  }
}


static status
layoutTile(TileObj t, Int ax, Int ay, Int aw, Int ah)
{ int border = valInt(t->border);
  int nvis = isNil(t->members) ? 0 : non_empty_tiles(t);
  int borders = nvis > 0 ? nvis-1 : 0;
  int x, y, w, h;
  bool placed = false;

  assign(t, enforced, ON);

  if ( notDefault(aw) && valInt(aw) < 0 ) aw = ZERO;
  if ( notDefault(ah) && valInt(ah) < 0 ) ah = ZERO;

  setArea(t->area, ax, ay, aw, ah);
  x = valInt(t->area->x);
  y = valInt(t->area->y);
  w = valInt(t->area->w);
  h = valInt(t->area->h);

  if ( isNil(t->super) )		/* the outer border is its own: a manager
				   may want none of it and still separate
				   the tiles inside */
  { int root = valInt(t->border_root);

    x += root;
    y += root;
    w -= root*2;
    h -= root*2;
  }

  if ( t->orientation == NAME_none )
    return send(t->object, NAME_doSet,
		toInt(x), toInt(y), toInt(w), toInt(h), EAV);

  int x0 = x, y0 = y;			/* where the box starts */

  DEBUG(NAME_tile, Cprintf("enter: layoutTile(%s) (%s)\n",
			   pp(t), pp(t->orientation)));
  if ( t->orientation == NAME_horizontal )
  { stretch s[MAX_TILE_MEMBERS];
    Stretch sp;
    Cell cell;

    sp = s;
    for_cell(cell, t->members)
    { TileObj t2 = cell->value;

      sp->minimum = tile_minimum(t2, true);
      sp->maximum = INT_MAX;
      sp->ideal   = valInt(t2->idealWidth);
      sp->stretch = valInt(t2->horStretch);
      sp->shrink  = shrinkability(valInt(t2->horShrink), sp->ideal);
      sp++;
    }

    distribute_stretches(s, sp-s, w - border*borders);

    sp = s;
    for_cell(cell, t->members)
    { TileObj t2 = cell->value;
      int size = sp->size;
      int room = x0 + w - x;		/* what is left of the box */

      if ( sp->size > 0 && placed )
	x += border;
      if ( size > room )		/* the minimums do not fit: keep it
					   inside rather than out of reach */
	size = room > 0 ? room : 0;
      layoutTile(t2, toInt(x), toInt(y), toInt(size), toInt(h));
      x += size;
      if ( sp->size > 0 )
	placed = true;
      sp++;
    }
  } else /*if ( t->orientation == NAME_vertical )*/
  { stretch s[MAX_TILE_MEMBERS];
    Stretch sp;
    Cell cell;

    sp = s;
    for_cell(cell, t->members)
    { TileObj t2 = cell->value;

      sp->minimum = tile_minimum(t2, false);
      sp->maximum = INT_MAX;
      sp->ideal   = valInt(t2->idealHeight);
      sp->stretch = valInt(t2->verStretch);
      sp->shrink  = shrinkability(valInt(t2->verShrink), sp->ideal);
      sp++;
    }

    distribute_stretches(s, sp-s, h - border*borders);

    sp = s;
    for_cell(cell, t->members)
    { TileObj t2 = cell->value;
      int size = sp->size;
      int room = y0 + h - y;		/* what is left of the box */

      if ( sp->size > 0 && placed )
	y += border;
      if ( size > room )		/* the minimums do not fit: keep it
					   inside rather than out of reach */
	size = room > 0 ? room : 0;
      layoutTile(t2, toInt(x), toInt(y), toInt(w), toInt(size));
      y += size;
      if ( sp->size > 0 )
	placed = true;
      sp++;
    }
  }

  DEBUG(NAME_tile, Cprintf("exit: layoutTile(%s)\n", pp(t)));

  succeed;
}


static status
areaTile(TileObj t, Area a)
{ return setTile(t, a->x, a->y, a->w, a->h);
}


static status
positionTile(TileObj t, Point p)
{ return setTile(t, p->x, p->y, DEFAULT, DEFAULT);
}


static status
sizeTile(TileObj t, Size s)
{ return setTile(t, DEFAULT, DEFAULT, s->w, s->h);
}


static status
xTile(TileObj t, Int x)
{ return setTile(t, x, DEFAULT, DEFAULT, DEFAULT);
}


static status
yTile(TileObj t, Int y)
{ return setTile(t, DEFAULT, y, DEFAULT, DEFAULT);
}


/* ->width and ->height are how a size is handed out: the frame's
 * separator drag (see tileResizeEvent() in frame.c), the resize gesture
 * of a manager and the shares it gives its panes all end here.
 */

static status
widthTile(TileObj t, Int w)
{ return set_tile(t, DEFAULT, DEFAULT, w, DEFAULT, TRUE);
}


static status
heightTile(TileObj t, Int h)
{ return set_tile(t, DEFAULT, DEFAULT, DEFAULT, h, TRUE);
}


static status
cornerTile(TileObj t, Point pos)
{ return setTile(t, DEFAULT, DEFAULT, sub(pos->x, t->area->x),
				      sub(pos->y, t->area->y));
}


static status
centerTile(TileObj t, Point pos)
{ return setTile(t, dif(pos->x, t->area->w),
		    dif(pos->y, t->area->h),
		    DEFAULT, DEFAULT);
}


		 /*******************************
		 *	  SET OPERATIONS	*
		 *******************************/

static status
forAllTile(TileObj t, Code msg)
{ if ( notNil(t->object) )
    TRY(forwardCodev(msg, 1, &t->object));

  if ( notNil(t->members) )
  { TileObj st;

    for_chain(t->members, st, TRY(forAllTile(st, msg)));
  }

  succeed;
}

		 /*******************************
		 *	  RESIZE SUPPORT	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the tile in a tile-stack that  is above/left-of the separation line
in which `pos' lies.  This is used by resizeTileEventFrame() to find the
tile to resize.

A tile is considered only a candidate for  resizing if it can be resized
and there is at least one tile below/right of it that can be resized.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* Can `t' take part in a resize along `dir'?  A tile that has been given
 * a size by hand no longer stretches -- set_tile() takes that away so that
 * it holds the size it was given -- but it can be given another one, which
 * is what dragging its edge does.  A tile that never stretched is fixed by
 * whoever built it and stays that way: asking for the room its content
 * needs is not the same as being given a size, and does not make its edge
 * a handle.
 */

static status
ICanResizeTile(TileObj t, Name dir)
{ if ( dir == NAME_horizontal )
  { if ( isResizedTile(t, NAME_width) ||
	 t->horShrink != ZERO || t->horStretch != ZERO )
      succeed;
  } else
  { if ( isResizedTile(t, NAME_height) ||
	 t->verShrink != ZERO || t->verStretch != ZERO )
      succeed;
  }

  fail;
}


BoolObj
getCanResizeTile(TileObj t)
{ if ( isDefault(t->canResize) )
  { if ( notNil(t->super) )
    { if ( ICanResizeTile(t, t->super->orientation) )
      { Cell cell;
	int before = TRUE;

	for_cell(cell, t->super->members)
	{ TileObj t2 = cell->value;

	  if ( before )
	  { if ( t == t2 )
	      before = FALSE;
	  } else
	  { if ( ICanResizeTile(t2, t->super->orientation) )
	    { assign(t, canResize, ON);
	      goto out;
	    }
	  }
	}
      }
    }

    assign(t, canResize, OFF);
  }

out:
  answer(t->canResize);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
After a hand resize the tiles left of (above) the dragged edge hold the
size they were given: setTile() takes their stretch and shrink away for
that.  What is left is a stack in which one tile takes all that a resize
of the frame brings and the others take none, so the panes no longer keep
the relative sizes the user just gave them.  ->rebalance turns the layout
as it is now into the wish: every tile asks for the size it has, and asks
for its share of what is added in proportion to that size.

<->hor_shrink is proportional already: layoutTile() hands it to
distribute_stretches() through shrinkability(), which multiplies it by
the ideal size.  <->hor_stretch is a flat weight -- three tiles of 100,
200 and 300 with the default 100 each grow by the same number of pixels
-- so it is the one that has to be made proportional.  The weights are
normalised to average 100, the default, as computeTile() hands them up to
the super-tile with Max()/Min() and a weight in pixels would there
outvote everything else.

A tile that never stretched is fixed by whoever built it (a menu bar is
as high as its buttons and no more) and must stay that way;
ICanResizeTile() tells it from one that set_tile() has zeroed after a
size was given by hand, which is marked <-resized.  A tile laid out with no size at all is left alone as
well: a share of nothing is nothing, and it would never come back.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
rebalanceTile(TileObj t)
{ Cell cell;

  if ( isNil(t->members) || t->orientation == NAME_none )
    succeed;

  { bool horizontal = (t->orientation == NAME_horizontal);
    int total = 0, n = 0;

    for_cell(cell, t->members)
    { TileObj t2 = cell->value;
      int size = valInt(horizontal ? t2->area->w : t2->area->h);

      if ( size > 0 && ICanResizeTile(t2, t->orientation) )
      { total += size;
	n++;
      }
    }

    for_cell(cell, t->members)
    { TileObj t2 = cell->value;
      int size = valInt(horizontal ? t2->area->w : t2->area->h);

      if ( total > 0 && size > 0 && ICanResizeTile(t2, t->orientation) )
      { int weight = (100*n*size + total/2)/total;

	if ( weight < 1 )		/* it must keep a say */
	  weight = 1;

	DEBUG(NAME_tile, Cprintf("rebalance %s: %d (%d)\n",
				 pp(t2), size, weight));

	if ( horizontal )
	{ assign(t2, idealWidth, toInt(size));
	  assign(t2, horStretch, toInt(weight));
	  assign(t2, horShrink,  toInt(100));
	} else
	{ assign(t2, idealHeight, toInt(size));
	  assign(t2, verStretch,  toInt(weight));
	  assign(t2, verShrink,   toInt(100));
	}
      }
    }
  }

  for_cell(cell, t->members)
    rebalanceTile(cell->value);

  succeed;
}

void *
forResizeAreaTile(TileObj t, for_tile_func func, Any ctx)
{ if ( notNil(t->members) )
  { Cell cell;

    for_cell(cell, t->members)
    { TileObj t2 = cell->value;
      TileObj t3;
      void *rc = forResizeAreaTile(t2, func, ctx);

      if ( rc )
	return rc;

      if ( notNil(cell->next) )
	t3 = cell->next->value;
      else
	break;

      if ( t->orientation == NAME_horizontal )
      { if ( getCanResizeTile(t2) == ON )
	{ int x0 = valInt(t2->area->x) + valInt(t2->area->w);
	  int x1 = valInt(t3->area->x);

	  if ( x1 > x0 )		/* there is a gap: see below */
	  { void *rc = (*func)(ctx, t2,
			       toInt(x0), t->area->y,
			       toInt(x1-x0), t->area->h);

	    if ( rc )
	      return rc;
	  }
	}
      } else
      { if ( getCanResizeTile(t2) == ON )
	{ int y0 = valInt(t2->area->y) + valInt(t2->area->h);
	  int y1 = valInt(t3->area->y);

	  if ( y1 > y0 )		/* there is a gap: see below */
	  { void *rc = (*func)(ctx, t2,
			       t->area->x, toInt(y0),
			       t->area->w, toInt(y1-y0));

	    if ( rc  )
	      return rc;
	  }
	}
      }
    }
  }

  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
<-resize_areas returns a chain of  areas   that  cover the gaps between
resizable sub-tiles.  The frame paints these  using the window system (see
ws_draw_resize_frame()).  A tile hierarchy that  lives inside a graphical
device (see class tab_frame) paints them itself and thus needs the areas.

A member laid out with no size at all leaves no gap after it -- see the
comment at non_empty_tiles(), which is what withholds the border in that
case.  Reporting one anyway is worse than useless: the gap is a rectangle
of zero height, ws_draw_resize_area_frame() draws a line down the middle
of it, and the middle of nothing is the first row of the tile that
follows.  On MacOS, where the menus are shown natively and the dialog
that carries the menu bar asks for no height, that line lands on the top
row of the pane below.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void *
add_resize_area_tile(Any ctx, TileObj t, Int x, Int y, Int w, Int h)
{ Chain ch = ctx;

  appendChain(ch, newObject(ClassArea, x, y, w, h, EAV));

  return NULL;				/* continue */
}


static Chain
getResizeAreasTile(TileObj t)
{ Chain ch = answerObject(ClassChain, EAV);

  forResizeAreaTile(t, add_resize_area_tile, ch);

  answer(ch);
}


TileObj
getSubTileToResizeTile(TileObj t, Point pos)
{ if ( pointInArea(t->area, pos) && notNil(t->members) )
  { Cell cell;

    DEBUG(NAME_tile, Cprintf("getSubTileToResizeTile() at %s, %s: ",
			     pp(pos->x), pp(pos->y)));


					/* in the area of a sub-tile */
    for_cell(cell, t->members)
    { TileObj t2 = cell->value;

      if ( pointInArea(t2->area, pos) && notNil(t2->members) )
      { TileObj t3 = getSubTileToResizeTile(t2, pos);

	if ( t3 )
	  answer(t3);
      }
    }

    for_cell(cell, t->members)
    { TileObj t2 = cell->value;
      TileObj t3;

      if ( notNil(cell->next) )
	t3 = cell->next->value;
      else
	break;

					/* +/- 1: be a bit relaxed */
      if ( t->orientation == NAME_horizontal )
      { if ( valInt(pos->x) >= valInt(t2->area->x) + valInt(t2->area->w) - 1 &&
	     valInt(pos->x) <= valInt(t3->area->x) + 1 )
	{ if ( getCanResizeTile(t2) == ON )
	  { DEBUG(NAME_tile, Cprintf("%s\n", pp(t2)));
	    answer(t2);
	  } else
	    goto no;
	}
      } else
      { if ( valInt(pos->y) >= valInt(t2->area->y) + valInt(t2->area->h) - 1 &&
	     valInt(pos->y) <= valInt(t3->area->y) + 1 )
	{ if ( getCanResizeTile(t2) == ON )
	  { DEBUG(NAME_tile, Cprintf("%s\n", pp(t2)));
	    answer(t2);
	  } else
	    goto no;
	}
      }
    }
  }

no:
  DEBUG(NAME_tile, Cprintf("NONE\n"));
  fail;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "object=object*", "width=[int]", "height=[int]" };
static char *T_xADintD_yADintD_widthADintD_heightADintD[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };
static char *T_associate[] =
	{ "object", "delegate=[bool]" };

/* Instance Variables */

static vardecl var_tile[] =
{ IV(NAME_idealWidth, "int", IV_BOTH,
     NAME_dimension, "Desired width of the tile"),
  IV(NAME_idealHeight, "int", IV_BOTH,
     NAME_dimension, "Desired height of the tile"),
  IV(NAME_horStretch, "int", IV_BOTH,
     NAME_resize, "Encouragement to get wider"),
  IV(NAME_horShrink, "int", IV_BOTH,
     NAME_resize, "Encouragement to get smaller"),
  IV(NAME_verStretch, "int", IV_BOTH,
     NAME_resize, "Encouragement to get higher"),
  IV(NAME_verShrink, "int", IV_BOTH,
     NAME_resize, "Encouragement to get lower"),
  IV(NAME_canResize, "[bool]", IV_SEND,
     NAME_resize, "Can be resized by user?"),
  IV(NAME_resized, "{none,width,height,both}", IV_NONE,
     NAME_resize, "Has been given a size by ->width and ->height"),
  SV(NAME_border, "int", IV_GET|IV_STORE, borderTile,
     NAME_appearance, "Distance between areas"),
  IV(NAME_borderRoot, "int", IV_BOTH,
     NAME_appearance, "Distance around the root tile"),
  IV(NAME_orientation, "{none,horizontal,vertical}", IV_GET,
     NAME_layout, "Direction of adjacent sub-tiles"),
  IV(NAME_members, "chain*", IV_GET,
     NAME_organisation, "Managed tiles (subtiles)"),
  IV(NAME_super, "tile*", IV_GET,
     NAME_organisation, "Tile that manages me"),
  IV(NAME_object, "object*", IV_GET,
     NAME_client, "Object managed"),
  IV(NAME_manager, "object*", IV_NONE,
     NAME_organisation, "Frame or device managing the hierarchy"),
  SV(NAME_area, "area", IV_GET|IV_STORE, areaTile,
     NAME_dimension, "Area of the object"),
  IV(NAME_enforced, "bool", IV_GET,
     NAME_layout, "If @on, the tile's layout will be enforced")
};

/* Send Methods */

static senddecl send_tile[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseTile,
     DEFAULT, "Create from object, width and height"),
  SM(NAME_unlink, 0, NULL, unlinkTile,
     DEFAULT, "Unlink sub and super-tiles"),
  SM(NAME_center, 1, "point", centerTile,
     NAME_dimension, "Set center by moving tile"),
  SM(NAME_corner, 1, "point", cornerTile,
     NAME_dimension, "Set point opposite to origin"),
  SM(NAME_height, 1, "int", heightTile,
     NAME_dimension, "Set H of tile"),
  SM(NAME_position, 1, "point", positionTile,
     NAME_dimension, "Set XY of the tile"),
  SM(NAME_set, 4, T_xADintD_yADintD_widthADintD_heightADintD, setTile,
     NAME_dimension, "Set XYWH of entire tile"),
  SM(NAME_size, 1, "size", sizeTile,
     NAME_dimension, "Set WH of the tile"),
  SM(NAME_width, 1, "int", widthTile,
     NAME_dimension, "Set W of tile"),
  SM(NAME_x, 1, "int", xTile,
     NAME_dimension, "Set X of the tile"),
  SM(NAME_y, 1, "int", yTile,
     NAME_dimension, "Set Y of the tile"),
  SM(NAME_forAll, 1, "code", forAllTile,
     NAME_iterate, "Iterate over all <-object's"),
  SM(NAME_above, 2, T_associate, aboveTile,
     NAME_layout, "Place a tile above me"),
  SM(NAME_below, 2, T_associate, belowTile,
     NAME_layout, "Place a tile below me"),
  SM(NAME_enforce, 1, "[bool]", enforceTile,
     NAME_layout, "Enforce the tile layout"),
  SM(NAME_layout, 4, T_xADintD_yADintD_widthADintD_heightADintD, layoutTile,
     NAME_layout, "Compute subtile layout and adjust objects"),
  SM(NAME_left, 2, T_associate, leftTile,
     NAME_layout, "Place a tile to my left"),
  SM(NAME_right, 2, T_associate, rightTile,
     NAME_layout, "Place a tile to my right"),
  SM(NAME_compute, 0, NULL, computeTile,
     NAME_update, "Compute ideal sizes from sub-tiles"),
  SM(NAME_unrelate, 0, NULL, unrelateTile,
     NAME_layout, "Remove me from my super-tile"),
  SM(NAME_manager, 1, "object*", managerTileMethod,
     NAME_organisation, "Object that manages this hierarchy"),
  SM(NAME_rebalance, 0, NULL, rebalanceTile,
     NAME_resize, "Ask for the current sizes, and shares in proportion")
};

/* Get Methods */

static getdecl get_tile[] =
{ GM(NAME_root, 0, "tile", NULL, getRootTile,
     NAME_organisation, "Root of the tile-hierarchy"),
  GM(NAME_subTileToResize, 1, "tile", "point", getSubTileToResizeTile,
     NAME_event, "Tile above or left-of gap at point"),
  GM(NAME_canResize, 0, "bool", NULL, getCanResizeTile,
     NAME_resize, NULL),
  GM(NAME_resizeAreas, 0, "chain", NULL, getResizeAreasTile,
     NAME_resize, "New chain of area for the resizable gaps"),
  GM(NAME_manager, 0, "object", NULL, getManagerTile,
     NAME_organisation, "Object that manages this hierarchy")
};

/* Resources */

static classvardecl rc_tile[] =
{ RC(NAME_border, "int", "4",
     "Border between subtiles"),
  RC(NAME_borderRoot, "int", "4",
     "Border around the root tile")
};

/* Class Declaration */

static Name tile_termnames[] =
	{ NAME_object, NAME_idealWidth, NAME_idealHeight };

ClassDecl(tile_decls,
          var_tile, send_tile, get_tile, rc_tile,
          1, tile_termnames);


status
makeClassTile(Class class)
{ return declareClass(class, &tile_decls);
}
