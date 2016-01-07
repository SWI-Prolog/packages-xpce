/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2008-2011, University of Amsterdam
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This program is  used  to  generate   the  built-in  name  database from
NAME_<id> found in the  sources.  It  must   be  called  in  the  source
directory using

	find_names h/names.ic h/names.ih -- file ...

Older versions used POSIX scripting for this, but this has been moved to
a plain C program to facilitate   building  on on-POSIX platforms (read:
M$-Windows).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define LINESIZE 1024

const char *name = "NAME_";

char **names;
int allocated = 0;
int count = 0;

static void
save_name(const char *name)
{ if ( count >= allocated )
  { if ( allocated )
    { allocated *= 2;

      names = realloc(names, allocated*sizeof(char*));
    } else
    { allocated = 1024;
      names = malloc(allocated*sizeof(char*));
    }
  }

  names[count++] = strdup(name);
}



static int
scan_file(const char *file)
{ FILE *fd = fopen(file, "r");
  char buf[LINESIZE];
  size_t len = strlen(name);

  if ( !fd )
  { fprintf(stderr, "Warning: could not open %s (skipped)\n", file);
    return 0;
  }

  while(fgets(buf, sizeof(buf), fd))
  { const char *s;

    for(s = buf; *s; s++)
    { if ( s[0] == 'N' && strncmp(name, s, len) == 0 )
      { const char *start = s = s + len;
	char nbuf[200];

	if ( strncmp(s, "MAX", 3) == 0 ) /* avoid NAME_MAX */
	  continue;
	while(*s && (isalnum(*s & 0xff) || *s == '_') )
	  s++;
	strncpy(nbuf, start, s-start);
	nbuf[s-start] = '\0';
	save_name(nbuf);
      }
    }
  }

  fclose(fd);
  return 1;
}


static int
cmp_names(const void *p1, const void *p2)
{ return strcmp(*(char**)p1, *(char**)p2);
}


static void
sort_names()
{ if ( count > 0 )
  { int in, out;

    qsort(names, count, sizeof(char*), cmp_names);
    for(in=1, out=1; in<count; in++)
    { if ( strcmp(names[out-1], names[in]) == 0 )
      { free(names[in]);
      } else
      { names[out++] = names[in];
      }
    }

    count = out;
  }
}

static void
emit_names(FILE *ic, FILE *ih)
{ int i;

  for(i=0; i<count; i++)
  { const char *name = names[i];
    char prolog[LINESIZE];
    const char *s;
    char *q;

    for(s=name, q=prolog; *s; s++)
    { if ( isupper(*s & 0xff) )
      { *q++ = '_';
	*q++ = *s + 'a' - 'A';
      } else
      { *q++ = *s;
      }
    }
    *q = '\0';

    fprintf(ic, "  BUILTIN_NAME(\"%s\")\n", prolog);
    fprintf(ih, "#define NAME_%s (&builtin_names[%d])\n", name, i);
  }
}


int
main(int argc, char **argv)
{ int i;
  const char *program = argv[0];
  FILE *ic, *ih;

  argc--;				/* skip program */
  argv++;
  if ( argc < 3 || strcmp(argv[2], "--") )
  { fprintf(stderr, "Usage: %s ic-file ih-file -- file ...\n", program);
    exit(1);
  }

  if ( !(ic = fopen(argv[0], "w")) ||
       !(ih = fopen(argv[1], "w")) )
  { fprintf(stderr, "%s: Could not open output\n", program);
    exit(1);
  }

  argc -= 3;
  argv += 3;

  for(i=1; i<argc; i++)
    scan_file(argv[i]);

  sort_names();
  emit_names(ic, ih);

  fclose(ic);
  fclose(ih);

  return 0;
}
