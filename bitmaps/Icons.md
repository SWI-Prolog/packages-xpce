  Prompt for Claude: SWI-Prolog Debugger Icon Set

  You are to design a coherent set of SVG icons for the SWI-Prolog source-level
  debugger (the "Prolog Tracer"). These replace a set of aged 16×16 pixel icons.
   The new icons should be 64×64, clean, modern, and remain clearly recognisable
   when scaled down to ~20×20.

  ---
  Style specification (apply to every icon without exception)

  - Canvas: 64×64 px, transparent background
  - Style: flat / semi-flat. No gradients except a very subtle linear gradient
  (10–15% lightness shift) where it aids readability
  - Stroke: 2.5 px, #334155 (dark slate), round caps and joins, used for
  outlines only
  - Corner radius: 4 px on rectangular shapes
  - Padding: keep all shapes within a 6 px inset (usable area 52×52)
  - Colour palette (stay strictly within these):
    - Indigo #6366f1 — primary action / arrows
    - Emerald #10b981 — success / exit / loaded
    - Rose #f43f5e — error / fail / stop / warning
    - Amber #f59e0b — caution / dynamic / non-deterministic
    - Sky #38bdf8 — information / call port / module
    - Slate #64748b — neutral / disabled / file
    - White #ffffff — fill for shapes that need contrast
    - Dark #1e293b — text glyphs, strong outlines
  - Typography: if a letter is used (e.g. "C" for foreign), use a bold
  sans-serif glyph, centred, sized to fill ~60% of the canvas
  - Badging: several icons are a base icon with a small 18×18 badge (circle or
  rounded square) in the bottom-right corner. The badge has a 2 px white border
  to separate it from the base

  ---
  Icon descriptions

  Produce one SVG file per icon, named exactly as listed.

  Debugger action buttons (toolbar)					[DONE]

  ┌───────────────┬──────────────────────────────────────────────────────────┐
  │   Filename    │                       Description                        │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ creep.png     │ Indigo right-pointing arrow, single step. Bold filled    │
  │               │ arrow. Equivalent to "step into next port".              │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ into.png      │ Indigo arrow pointing straight down, "step into"         │
  │               │ sub-goal.                                                │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ leap.png      │ Indigo right arrow with a small dashed indigo arc above  │
  │               │ it, meaning "jump to next spy-point".                    │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ skip.png      │ Indigo right arrow with a curved arc over the top,       │
  │               │ meaning "skip over this goal".                           │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ retry.png     │ Amber circular counter-clockwise arrow (↺), "retry from  │
  │               │ the start of this goal".                                 │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ finish.png    │ Emerald right arrow entering a vertical bar (→           │
  ├────────────────┼─────────────────────────────────────────────────────────┤
  │ interactor.png │ A terminal window outline with ?- prompt in dark, "open │
  │                │  Prolog interactor".                                    │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ fail.png      │ Rose downward-bent arrow or a thumbs-down shape, "force  │
  │               │ this goal to fail".                                      │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ abort.png     │ Rose circle with a horizontal white bar (⊘ style),       │
  │               │ "abort to top level".                                    │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ stop.png      │ Rose filled octagon (stop-sign shape) with a white       │
  │               │ square inside, "halt execution".                         │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ break.png     │ Sky rounded rectangle containing the Prolog prompt glyph │
  │               │  ?- in dark, "break to interactive prompt".              │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ interrupt.png │ Amber lightning bolt, "send interrupt signal".           │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ nodebug.png   │ Slate ladybug silhouette with a rose diagonal slash      │
  │               │ across it, "turn off debug mode".                        │
  └───────────────┴──────────────────────────────────────────────────────────┘

  Debug mode indicators

  ┌────────────────┬─────────────────────────────────────────────────────────┐
  │    Filename    │                       Description                       │
  ├────────────────┼─────────────────────────────────────────────────────────┤
  │ debug.png      │ Red ladybug with black spots, cheerful, "debug mode     │
  │                │ active".                                                │
  ├────────────────┼─────────────────────────────────────────────────────────┤
  │ breakpoint.png │ Rose filled circle (12 px) centred, with a white ring,  │
  │                │ classic breakpoint marker.                              │
  ├────────────────┼─────────────────────────────────────────────────────────┤
  │ nostop.png     │ A breakpoint circle (rose) with a rose diagonal slash,  │
  │                │ "no stop here".                                         │
  ├────────────────┼─────────────────────────────────────────────────────────┤
  │ eyes.png       │ Two open eyes side-by-side in dark slate, "watch /      │
  │                │ observe".                                               │
  └────────────────┴─────────────────────────────────────────────────────────┘

  Prolog execution ports (call stack colours)				[DONE]

  ┌────────────┬─────────────────────────────────────────────────────────────┐
  │  Filename  │                         Description                         │
  ├────────────┼─────────────────────────────────────────────────────────────┤
  │ call.png   │ Sky rounded square with a right-pointing chevron › in       │
  │            │ white, "Call port".                                         │
  ├────────────┼─────────────────────────────────────────────────────────────┤
  │ exit.png   │ Emerald rounded square with a tick ✓ in white, "Exit port". │
  ├────────────┼─────────────────────────────────────────────────────────────┤
  │ redo.png   │ Amber rounded square with a circular arrow in white, "Redo  │
  │            │ port".                                                      │
  ├────────────┼─────────────────────────────────────────────────────────────┤
  │ except.png │ Rose rounded square with a white lightning bolt, "Exception │
  │            │  port".                                                     │
  └────────────┴─────────────────────────────────────────────────────────────┘

  Predicate classification						[DONE]

  ┌───────────────┬──────────────────────────────────────────────────────────┐
  │   Filename    │                       Description                        │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ det.png       │ Indigo arrow pointing right with a single vertical bar   │
  │               │ at the tip (→|), "deterministic".                        │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ ndet.png      │ Amber arrow that forks into two arrows at the tip (→⑂),  │
  │               │ "non-deterministic".                                     │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ fact.png      │ A horizontal rule (─) with a dot at the left end,        │
  │               │ representing head. with no body.                         │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ pred.png      │ Slate horizontal rule with a dot and a short descending  │
  │               │ line representing head :- body.                          │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ grammar.png   │ Indigo double-headed right arrow (→→), "DCG grammar      │
  │               │ rule".                                                   │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ dynamic.png   │ Amber filled circle with a small indigo right-arrow      │
  │               │ badge bottom-right, "dynamic predicate".                 │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ dyn.png       │ Smaller amber filled circle, used as inline badge.       │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ meta.png      │ Indigo M letter in a circle, "meta-predicate".           │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ builtin.png   │ Dark rounded square with a white Ω (omega) or ⊢ symbol,  │
  │               │ "built-in predicate".                                    │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ foreign.png   │ Bold dark letter C in sky colour, "foreign (C)           │
  │               │ predicate".                                              │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ undefined.png │ Rose ? in a circle, "undefined".                         │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ undefpred.png │ Slate predicate arrow with a rose ? badge, "undefined    │
  │               │ predicate".                                              │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ unrefpred.png │ Slate predicate arrow with an amber ! badge,             │
  │               │ "unreferenced predicate".                                │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ warnpred.png  │ Slate predicate arrow with an amber triangle warning     │
  │               │ badge, "predicate has warnings".                         │
  ├───────────────┼──────────────────────────────────────────────────────────┤
  │ user.png      │ A simple person silhouette (head circle + shoulder arc)  │
  │               │  in slate, "user module".                                │
  └───────────────┴──────────────────────────────────────────────────────────┘

  Module and file system					        [DONE]

  ┌────────────────────┬─────────────────────────────────────────────────────┐
  │      Filename      │                     Description                     │
  ├────────────────────┼─────────────────────────────────────────────────────┤
  │ module.png         │ Sky rounded square with a white hexagon outline     │
  │                    │ inside, representing a Prolog module.               │
  ├────────────────────┼─────────────────────────────────────────────────────┤
  │ openmodule.png     │ Same as module.png with the top-right corner folded │
  │                    │  open.                                              │
  ├────────────────────┼─────────────────────────────────────────────────────┤
  │ import.png         │ Sky module icon with a small indigo inward arrow    │
  │                    │ badge bottom-right, "imported predicate".           │
  ├────────────────────┼─────────────────────────────────────────────────────┤
  │ export.png         │ Sky module icon with a small emerald outward arrow  │
  │                    │ badge bottom-right, "exported predicate".           │
  ├────────────────────┼─────────────────────────────────────────────────────┤
  │ plfile.png         │ Slate document icon (folded top-right corner), no   │
  │                    │ badge, "Prolog source file".                        │
  ├────────────────────┼─────────────────────────────────────────────────────┤
  │ plincludedfile.png │ Same document with a small sky + badge, "included   │
  │                    │ file".                                              │
  ├────────────────────┼─────────────────────────────────────────────────────┤
  │ plloadedfile.png   │ Same document with a small emerald ✓ badge,         │
  │                    │ "successfully loaded file".                         │
  ├────────────────────┼─────────────────────────────────────────────────────┤
  │ loadfailed.png     │ Same document with a rose ✗ badge, "file failed to  │
  │                    │ load".                                              │
  ├────────────────────┼─────────────────────────────────────────────────────┤
  │ loading.png        │ Same document with an amber circular arrow badge,   │
  │                    │ "file being loaded".                                │
  ├────────────────────┼─────────────────────────────────────────────────────┤
  │ closedir.png       │ Classic folder icon in amber, closed.               │
  ├────────────────────┼─────────────────────────────────────────────────────┤
  │ opendir.png        │ Classic folder icon in amber, open (top flap        │
  │                    │ lifted).                                            │
  └────────────────────┴─────────────────────────────────────────────────────┘

  OOP / XPCE message passing

  ┌──────────────┬───────────────────────────────────────────────────────────┐
  │   Filename   │                        Description                        │
  ├──────────────┼───────────────────────────────────────────────────────────┤
  │              │ Slate rounded rectangle with a thin horizontal line       │
  │ class.png    │ one-third from the top, representing a class box (like a  │
  │              │ UML class).                                               │
  ├──────────────┼───────────────────────────────────────────────────────────┤
  │ classext.png │ Same class box with a small + in the bottom-right, "class │
  │              │  extension".                                              │
  ├──────────────┼───────────────────────────────────────────────────────────┤
  │ classvar.png │ Class box with a small bold V badge, "class variable".    │
  ├──────────────┼───────────────────────────────────────────────────────────┤
  │ ivar.png     │ A small cylinder in slate, "instance variable".           │
  ├──────────────┼───────────────────────────────────────────────────────────┤
  │ send.png     │ An arrow pointing right entering a rounded rectangle,     │
  │              │ "send message".                                           │
  ├──────────────┼───────────────────────────────────────────────────────────┤
  │ get.png      │ An arrow pointing left leaving a rounded rectangle, "get  │
  │              │ message".                                                 │
  └──────────────┴───────────────────────────────────────────────────────────┘

  Navigation and utilities				      [DONE (debug-2)]

  ┌────────────────┬─────────────────────────────────────────────────────────┐
  │    Filename    │                       Description                       │
  ├────────────────┼─────────────────────────────────────────────────────────┤
  │ up.png         │ Bold indigo upward arrow with a horizontal base line,   │
  │                │ "go up the call stack".                                 │
  ├────────────────┼─────────────────────────────────────────────────────────┤
  │ down.png       │ Bold indigo downward arrow with a horizontal base line, │
  │                │  "go down the call stack".                              │
  ├────────────────┼─────────────────────────────────────────────────────────┤
  │ spy.png        │ Dark indigo binoculars shape (two circles joined),      │
  │                │ "spy-point active".                                     │
  ├────────────────┼─────────────────────────────────────────────────────────┤
  │ nospy.png      │ Slate binoculars with a rose diagonal slash, "remove    │
  │                │ spy-point".                                             │
  ├────────────────┼─────────────────────────────────────────────────────────┤
  │ nostopspy.png  │ Binoculars + breakpoint circle combined, with a rose    │
  │                │ slash, "neither stop nor spy".                          │
  ├────────────────┼─────────────────────────────────────────────────────────┤
  │ stack.png      │ Three stacked horizontal rounded bars in                │
  │                │ indigo/sky/slate, "show call stack".                    │
  │                │ A stylised blue butterfly with indigo wings and sky     │
  ├────────────────┼─────────────────────────────────────────────────────────┤
  │ butterfly.png  │ highlights, SWI-Prolog mascot. Keep it recognisable but │
  │                │  clean it up at 64×64.                                  │
  ├────────────────┼─────────────────────────────────────────────────────────┤
  │ edit.png       │ A pencil pointing bottom-left in slate/amber, "edit     │
  │                │ source".                                                │
  ├────────────────┼─────────────────────────────────────────────────────────┤
  │ details.png    │ A magnifying glass in slate, "show details /            │
  │                │ properties".                                            │
  ├────────────────┼─────────────────────────────────────────────────────────┤
  │ list.png       │ A document with three horizontal lines inside, "show    │
  │                │ listing".                                               │
  └────────────────┴─────────────────────────────────────────────────────────┘

  Brand / special

  ┌────────────────┬─────────────────────────────────────────────────────────┐
  │    Filename    │                       Description                       │
  ├────────────────┼─────────────────────────────────────────────────────────┤
  │ mini-globe.png │ A small globe with latitude/longitude lines in sky and  │
  │                │ slate, "global / world".                                │
  ├────────────────┼─────────────────────────────────────────────────────────┤
  │ mini-run.png   │ A simplified running figure in indigo, "run".           │
  └────────────────┴─────────────────────────────────────────────────────────┘

  Buttons							[DONE]

  edit.svg  Classical button for opening a file in an editor
  up.svg    Classical button for going to the parent directory
  refresh.svg Classical button to refresh the page
  dbgsettings Settings for the debugger, i.e. a cogwheel with some
              indicating this is for debugging.

  Visual Hierarchy

  I need 4 icons of design size 24x24 that represent an XPCE class.
  There are two dimensions:
    - Whether the class is built-in or a user-defined class.
    - Whether the instance can be made visible by flashing it.

  The icon names are
    - builtin_class.svg
    - user_class.svg
    - builtin_classflash.svg
    - user_classflash.png

  PceDraw

  I need icons of design size 24x24 for a grey80 background with the following
  names and function:

    - open.svg
      Open (load) a file
    - save.svg
      Save to a file
    - print.svg
      Print current document
    - undo.svg
      Undo last operation
    - cut.svg
      Cut current object
    - copy.svg
      Copy current object
    - paste.svg
      Paste object
    - duplicate.svg
      Duplicate current object, adding a copy to the document
    - distribute.svg
      Distribute (graphical) objects, either horizontally or
      vertically, depending on their rough distribution.

  These icons are for PceDraw, the xpce drawing tool, but except duplicate
  and distribute they should be applicable for arbitrary documents.

  Thread Monitor

  I need the following icons for the thread monitor.  These icons needs to
  be line-height (24x24) and are displayed against the current background (can be light or dark).  Functions:

    - thread-running.svg
      Running figure.  Background will change in part to green to indicate
      CPU usage, so it must display good against the background and green.
    - thread-true.svg
      Thread completed with success
    - thread-false.svg
      Thread completed with goal failure
    - thread-exception.svg
      Thread completed with an exception
    - thread-exited.svg
      Thread completed due to thread_exit/1 (with a value).
    - thread-profiling.svg
      Thread is being profiled.

  Delivery instructions

  - One .svg file per icon, filename matching the table (substitute .svg for
  .png)
  - Use viewBox="0 0 64 64", no fixed width/height attributes
  - No embedded raster images — pure SVG paths, circles, rectangles
  - Group logically related elements with <g> and add a brief <!-- comment -->
  naming the icon
  - After generating all SVGs, provide a one-line shell command using
  rsvg-convert to batch-convert them all to 20×20 PNG

