/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org/projects/xpce/
    Copyright (c)  1995-2013, University of Amsterdam
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
#include <fcntl.h>
#include <h/interface.h>
#include <errno.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Thread objects as SWI-Prolog streams. This module  is used by the Prolog
interface through pce_open/3. It should be merged with iostream.c, which
defines almost the same, providing XPCE with uniform access to a variety
of objects.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct pce_file_handle * PceFileHandle;

struct pce_file_handle
{ long		magic;			/* PCE_IO_MAGIC */
  Any		object;			/* object `file-i-fied' */
  long		point;			/* current position */
  int		flags;			/* general flags field */
  IOENC		encoding;		/* Stream encoding used */
  int		my_flags;		/* private flags */
};

#define		PCE_IO_MAGIC	0x72eb9ace

#define		MY_ISSTREAM	0x0001	/* data a a byte-stream */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Open flags recognised:

	PCE_RDONLY	Reading only
	PCE_WRONLY	Writing only
	PCE_RDWR	Reading and writing
	PCE_APPEND	Keep appending
	PCE_TRUNC	Tuncate object prior to writing
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static PceFileHandle *handles;		/* array of handles */
static int max_handles=0;		/* # handles allocated */

static int
allocFileHandle()
{ int handle;

  for(handle = 0; handle < max_handles; handle++)
  { if ( handles[handle] == NULL )
      return handle;
  }

  { PceFileHandle *newhandles;
    int n;

    if ( max_handles == 0 )
    { n = 16;
      newhandles = pceMalloc(sizeof(PceFileHandle) * n);
    } else
    { n = max_handles*2;
      newhandles = pceRealloc(handles, sizeof(PceFileHandle) * n);
    }

    if ( newhandles )
    { int rval = max_handles;

      memset(&newhandles[max_handles], 0,
	     sizeof(PceFileHandle) * (n-max_handles));
      max_handles = n;
      handles = newhandles;

      return rval;
    }

    errno = ENOMEM;
    return -1;
  }
}


static int
pceOpen_nolock(Any obj, int flags, void *encoding)
{ int handle = allocFileHandle();
  PceFileHandle h;

  if ( handle < 0 )
    return handle;

  if ( !isProperObject(obj) )
  { errno = EINVAL;
    return -1;
  }

  if ( flags & PCE_WRONLY )
  { if ( !hasSendMethodObject(obj, NAME_writeAsFile) )
    { errno = EACCES;
      return -1;
    }

    if ( flags & PCE_TRUNC )
    { if ( !hasSendMethodObject(obj, NAME_truncateAsFile) ||
	   !send(obj, NAME_truncateAsFile, EAV) )
      { errno = EACCES;
	return -1;
      }
    }
  }
  if ( flags & PCE_RDONLY )
  { if ( !hasGetMethodObject(obj, NAME_readAsFile) )
    { errno = EACCES;
      return -1;
    }
  }

  h = alloc(sizeof(struct pce_file_handle));
  h->object = obj;
  addRefObj(obj);			/* so existence check is safe */
  h->flags = flags;
  h->point = 0L;
  h->my_flags = 0;

  if ( instanceOfObject(obj, ClassStream) )
  { h->my_flags |= MY_ISSTREAM;
    h->encoding = ENC_OCTET;
  } else
  { h->encoding = ENC_WCHAR;
  }

  handles[handle] = h;
  h->magic = PCE_IO_MAGIC;

  if ( encoding )
  { IOENC *ep = encoding;

    *ep = h->encoding;
  }

  return handle;
}

int
pceOpen(Any obj, int flags, void *encoding)
{ int rc;

  pceMTLock(LOCK_PCE);
  rc = pceOpen_nolock(obj, flags, encoding);
  pceMTUnlock(LOCK_PCE);

  return rc;
}


static int
pceClose_nolock(int handle)
{ PceFileHandle h;

  if ( handle >= 0 && handle < max_handles &&
       (h = handles[handle]) )
  { delRefObject(NIL, h->object);	/* handles deferred unalloc() */
    h->magic = 0;
    unalloc(sizeof(struct pce_file_handle), h);
    handles[handle] = NULL;

    return 0;
  }

  errno = EBADF;
  return -1;
}


int
pceClose(int handle)
{ int rc;

  pceMTLock(LOCK_PCE);
  rc = pceClose_nolock(handle);
  pceMTUnlock(LOCK_PCE);

  return rc;
}


static PceFileHandle
findHandle(int handle)
{ PceFileHandle h;

  if ( handle >= 0 &&
       handle < max_handles &&
       (h = handles[handle]) &&
       h->magic == PCE_IO_MAGIC )
    return h;

  errno = EBADF;
  return NULL;
}



static ssize_t
pceWrite_nolock(int handle, const char *buf, size_t size)
{ PceFileHandle h;

  if ( !(h=findHandle(handle)) )
    return -1;

  if ( h->flags & (PCE_RDWR|PCE_WRONLY) )
  { string s;
    CharArray ca;
    status rval;
    Int where = (h->flags & PCE_APPEND ? (Int) DEFAULT : toInt(h->point));
    const wchar_t *wbuf = (const wchar_t*)buf;
    const wchar_t *end = (const wchar_t*)&buf[size];
    const wchar_t *f;

    if ( isFreedObj(h->object) )
    { errno = EIO;
      return -1;
    }

    if ( (h->my_flags & MY_ISSTREAM) )
    { str_set_n_ascii(&s, size, (char*)buf);
    } else
    { assert(size%sizeof(wchar_t) == 0);

      for(f=wbuf; f<end; f++)
      { if ( *f > 0xff )
	  break;
      }

      if ( f == end )
      { charA *asc = alloca(size);
	charA *t = asc;

	for(f=wbuf; f<end; )
	  *t++ = (charA)*f++;

	str_set_n_ascii(&s, size/sizeof(wchar_t), (char*)asc);
      } else
      { str_set_n_wchar(&s, size/sizeof(wchar_t), (wchar_t*)wbuf);
      }
    }

    ca = StringToScratchCharArray(&s);

    if ( (rval = send(h->object, NAME_writeAsFile, where, ca, EAV)) )
      h->point += (long)size/sizeof(wchar_t);
    doneScratchCharArray(ca);

    if ( rval )
      return size;

    errno = EIO;
    return -1;
  } else
  { errno = EBADF;
    return -1;
  }
}


ssize_t
pceWrite(int handle, const char *buf, size_t size)
{ ssize_t rc;

  pceMTLock(LOCK_PCE);
  rc = pceWrite_nolock(handle, buf, size);
  pceMTUnlock(LOCK_PCE);

  return rc;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note: pos is measured  in  bytes.  If   we  use  wchar  encoding we must
compensate for this.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static long
pceSeek_nolock(int handle, long offset, int whence)
{ PceFileHandle h;

  offset /= sizeof(wchar_t);

  if ( (h=findHandle(handle)) )
  { Int size;

    if ( isFreedObj(h->object) )
    { errno = EIO;
      return -1;
    }

    switch(whence)
    { case PCE_SEEK_SET:
	h->point = offset;
        break;
      case PCE_SEEK_CUR:
        h->point += offset;
        break;
      case PCE_SEEK_END:
      { if ( hasGetMethodObject(h->object, NAME_sizeAsFile) &&
	     (size = get(h->object, NAME_sizeAsFile, EAV)) )
	{ h->point = valInt(size) - offset;
	  break;
	} else
	{ errno = EPIPE;		/* better idea? */
	  return -1;
	}
      }
      default:
      { errno = EINVAL;
	return -1;
      }
    }
    return h->point * sizeof(wchar_t);
  } else
  { errno = EBADF;
    return -1;
  }
}


long
pceSeek(int handle, long offset, int whence)
{ long rc;

  pceMTLock(LOCK_PCE);
  rc = pceSeek_nolock(handle, offset, whence);
  pceMTUnlock(LOCK_PCE);

  return rc;
}


/* see also Sread_object() */

static ssize_t
pceRead_nolock(int handle, char *buf, size_t size)
{ PceFileHandle h;

  if ( !(h=findHandle(handle)) )
    return -1;

  if ( h->flags & (PCE_RDWR|PCE_RDONLY) )
  { Any argv[2];
    CharArray sub;
    int chread;

    if ( isFreedObj(h->object) )
    { errno = EIO;
      return -1;
    }

    argv[0] = toInt(h->point);
    argv[1] = toInt(size/sizeof(wchar_t));

    if ( (sub = getv(h->object, NAME_readAsFile, 2, argv)) &&
	 instanceOfObject(sub, ClassCharArray) )
    { String s = &sub->data;

      assert(s->s_size <= size/sizeof(wchar_t));

      if ( isstrA(s) )
      { charW *dest = (charW*)buf;
	const charA *f = s->s_textA;
	const charA *e = &f[s->s_size];

	while(f<e)
	  *dest++ = *f++;
      } else
      { memcpy(buf, s->s_textW, s->s_size*sizeof(charW));
      }

      chread = s->s_size * sizeof(wchar_t);
      h->point += s->s_size;
    } else
    { errno = EIO;
      chread = -1;
    }

    return chread;
  } else
  { errno = EBADF;
    return -1;
  }
}


ssize_t
pceRead(int handle, char *buf, size_t size)
{ ssize_t rc;

  pceMTLock(LOCK_PCE);
  rc = pceRead_nolock(handle, buf, size);
  pceMTUnlock(LOCK_PCE);

  return rc;
}


int
pceControl_nolock(int handle, int cmd, void *closure)
{ PceFileHandle h;

  if ( !(h=findHandle(handle)) )
    return -1;

  switch(cmd)
  { case PCE_SETENCODING:
      if ( (h->my_flags & MY_ISSTREAM) )
	return 0;
  }

  errno = EPERM;
  return -1;
}

int
pceControl(int handle, int cmd, void *closure)
{ int rc;

  pceMTLock(LOCK_PCE);
  rc = pceControl_nolock(handle, cmd, closure);
  pceMTUnlock(LOCK_PCE);

  return rc;
}


const char *
pceOsError()
{ return strName(getOsErrorPce(PCE));
}
