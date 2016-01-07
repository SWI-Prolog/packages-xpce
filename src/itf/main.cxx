/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org/projects/xpce/
    Copyright (C): 1985-2007, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include <pce/Pce.h>
#include <pce/Chain.h>

static PceArg PcNframes("frames");
static PceArg PcNkind("kind");
static PceArg PcNtoplevel("toplevel");

extern "C" {
int	pceInitialise(int handles, char *home, int argc, char **argv);
int	pceDispatch(int fd, int timeout);
void	Cprintf(const char *fmt, ...);
}

int
main(int argc, char* argv[])
{ int frames = TRUE;

  if ( !pceInitialise(0, (char *)0, argc, argv) )
  { Cprintf("Sorry, failed to initialise XPCE\n");
    exit(1);
  }

  if ( !pceInitApplication(argc, argv) )
  { Cprintf("Failed to run pceInitApplication()\n");
    exit(1);
  }

  while(frames)
  { PceCell cell;

    pceDispatch(0, 1000);

    for(frames = FALSE, cell = AsChain(TheDisplay.get(PcNframes)).head();
	cell;
	++cell)
    { if ( cell.value().get(PcNkind) == PcNtoplevel )
      { frames = TRUE;
	break;
      }
    }
  }

  exit(0);
}
