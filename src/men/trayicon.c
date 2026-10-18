/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

/* An icon in the system tray (the notification area) with an optional
 * tooltip and menu.  The menu is built from a popup.  Selecting an
 * entry executes the popup as if the item was selected in the popup,
 * with the tray_icon as context.
 */

static status
initialiseTrayIcon(TrayIcon t, Image image, CharArray tooltip,
		   PopupObj popup)
{ assign(t, image,   image);
  assign(t, tooltip, isDefault(tooltip) ? NIL : tooltip);
  assign(t, popup,   isDefault(popup) ? NIL : popup);
  t->ws_ref = NULL;

  return ws_create_tray(t);
}


static status
unlinkTrayIcon(TrayIcon t)
{ ws_destroy_tray(t);

  succeed;
}


static status
imageTrayIcon(TrayIcon t, Image image)
{ assign(t, image, image);

  return ws_tray_image(t);
}


static status
tooltipTrayIcon(TrayIcon t, CharArray tooltip)
{ assign(t, tooltip, tooltip);

  return ws_tray_tooltip(t);
}


static status
popupTrayIcon(TrayIcon t, PopupObj popup)
{ assign(t, popup, popup);

  return ws_tray_menu(t);
}


static status
updateTrayIcon(TrayIcon t)
{ return ws_tray_menu(t);
}


static status
sdlTestClickTrayIcon(TrayIcon t, CharArray label)
{ return ws_tray_test_click(t, label);
}


/* Type declarations */

static char *T_initialise[] =
        { "image=image", "tooltip=[char_array]*", "popup=[popup]*" };

/* Instance Variables */

static vardecl var_tray_icon[] =
{ SV(NAME_image, "image", IV_GET|IV_STORE, imageTrayIcon,
     NAME_appearance, "Icon shown in the tray"),
  SV(NAME_tooltip, "char_array*", IV_GET|IV_STORE, tooltipTrayIcon,
     NAME_appearance, "Text shown when the pointer is on the icon"),
  SV(NAME_popup, "popup*", IV_GET|IV_STORE, popupTrayIcon,
     NAME_menu, "Menu of the icon"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_internal, "SDL tray")
};

/* Send Methods */

static senddecl send_tray_icon[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseTrayIcon,
     DEFAULT, "Create from image, tooltip and popup"),
  SM(NAME_unlink, 0, NULL, unlinkTrayIcon,
     DEFAULT, "Remove the icon from the tray"),
  SM(NAME_update, 0, NULL, updateTrayIcon,
     NAME_menu, "Rebuild the menu after the popup changed"),
  SM(NAME_sdlTestClick, 1, "label=char_array", sdlTestClickTrayIcon,
     NAME_test, "Click the menu entry with this label (testing)")
};

/* Class Declaration */

static Name tray_icon_termnames[] = { NAME_image, NAME_tooltip, NAME_popup };

ClassDecl(tray_icon_decls,
          var_tray_icon, send_tray_icon, NULL, NULL,
          3, tray_icon_termnames);

status
makeClassTrayIcon(Class class)
{ declareClass(class, &tray_icon_decls);

  succeed;
}
