/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <h/kernel.h>
#include <h/unix.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>
#include <errno.h>
#include <string.h>

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#else
#ifndef MAXPATHLEN
#define MAXPATHLEN 256
#endif
#endif

#if HAVE_SYS_ACCESS_H			/* AIX 3.2.5 */
#include <sys/access.h>
#endif

#if defined(__linux__) && !defined(PATH_MAX)
#include <linux/limits.h>
#endif

static status	kindFile(FileObj f, Name encoding);
static Sheet	FileFilters;

static status
initialiseFile(FileObj f, Name name, Name encoding)
{ Name fn;

  initialiseSourceSink((SourceSink)f);

  if ( isDefault(encoding) )
    encoding = NAME_text;

  assign(f, status, NAME_closed);
  assign(f, bom, DEFAULT);
  assign(f, path, DEFAULT);
#ifdef __WINDOWS__
  assign(f, newline_mode, NAME_dos);
#else
  assign(f, newline_mode, NAME_posix);
#endif
  f->fd = NULL;

  kindFile(f, encoding);

  if ( isDefault(name) )
  {
#ifdef HAVE_MKSTEMP
    char namebuf[100];
    int fileno;
    char *s;

    if ( (s=getenv("TMPDIR")) && strlen(s) < sizeof(namebuf)-13 )
    { strcpy(namebuf, s);
      strcat(namebuf, "/xpce-XXXXXX");
    } else
      strcpy(namebuf, "/tmp/xpce-XXXXXX");

    if ( (fileno = mkstemp(namebuf)) < 0 )
      return errorPce(f, NAME_openFile, NAME_write, getOsErrorPce(PCE));
    if ( (f->fd = Sfdopen(fileno, "w")) == NULL )
    { close(fileno);
      return errorPce(f, NAME_openFile, NAME_write, getOsErrorPce(PCE));
    }

    name = CtoName(namebuf);
    assign(f, status, NAME_tmpWrite);
#else					/* use unsafe tmpnam */
#ifdef HAVE_TMPNAM
    char namebuf[L_tmpnam];
    char *s = tmpnam(namebuf);

    if ( s )
    { name = CtoName(s);
      /*Cprintf("tmpnam() returns %s\n", s);*/
    } else
    { return errorPce(f, NAME_noTempFile, getOsErrorPce(PCE));
    }
#else
#ifdef HAVE_TEMPNAM			/* Prefer this on __WINDOWS__ */
    char *s = tempnam("c:\\tmp", "xpce");

    if ( s )
    { name = CtoName(s);
      /*Cprintf("tempnam() returns %s\n", s);*/
      free(s);
    } else
    { return errorPce(f, NAME_noTempFile, getOsErrorPce(PCE));
    }
#else
    Cprintf("No temporary files on this platform");
    fail;
#endif
#endif
#endif
  }

  if ( (fn=expandFileName(name)) )
  { assign(f, name, fn);
    succeed;
  } else
    fail;
}


Name
expandFileName(Name in)
{ wchar_t expanded[MAXPATHLEN];
  int len;

  if ( (len=expandFileNameW(charArrayToWC((CharArray)in, NULL),
			    expanded, MAXPATHLEN)) > 0 )
  {
#if O_XOS
     wchar_t lng[MAXPATHLEN];
     char buf[MAXPATHLEN];

     if ( _xos_long_file_nameW(expanded, lng, MAXPATHLEN) &&
	  _xos_canonical_filenameW(lng, buf, sizeof(buf), 0) )
     { return UTF8ToName(buf);
     } else
     { errorPce(in, NAME_representation, NAME_nameTooLong);
       fail;
     }
#else
     return WCToName(expanded, len);
#endif
  }

  fail;
}



static status
kindFile(FileObj f, Name encoding)
{ if ( f->status != NAME_closed )
    return errorPce(f, NAME_noChangeAfterOpen);

  if ( encoding == NAME_text )
  { if ( !isName(f->encoding) )
      assign(f, encoding, getClassVariableValueObject(f, NAME_encoding));
    assign(f, kind, NAME_text);
  } else if ( encoding == NAME_binary || encoding == NAME_octet )
  { assign(f, kind, NAME_binary);
    assign(f, encoding, NAME_octet);
  } else
  { assign(f, encoding, encoding);
    assign(f, kind, NAME_text);
  }

  succeed;
}


Name
getOsNameFile(FileObj f)
{ if ( notDefault(f->path) )
    answer(f->path);

  answer(f->name);
}


static status
unlinkFile(FileObj f)
{ return closeFile(f);
}


static status
storeFile(FileObj f, FileObj file)
{ return storeSlotsObject(f, file);
}


static status
loadFile(FileObj f, IOSTREAM *fd, ClassDef def)
{ TRY(loadSlotsObject(f, fd, def));	/* reopen? */

  if ( isNil(f->path) )
    assign(f, path, DEFAULT);		/* backward compatibility load */
  if ( !isName(f->kind) )
    assign(f, kind, NAME_binary);	/* same */
  if ( !isName(f->encoding) )
    assign(f, encoding, (f->kind == NAME_binary ? NAME_octet : NAME_text));
  if ( !isDefault(f->bom) && !isBoolean(f->bom) )
    assign(f, bom, DEFAULT);
  if ( !isName(f->newline_mode) )
  {
#ifdef __WINDOWS__
    assign(f, newline_mode, NAME_dos);
#else
    assign(f, newline_mode, NAME_posix);
#endif
  }
  assign(f, status, NAME_closed);
  f->fd = NULL;

  succeed;
}


static FileObj
getConvertFile(Class class, Name name)
{ answer(answerObject(ClassFile, name, EAV));
}


status
closeFile(FileObj f)
{ if ( f->status != NAME_closed )
  { status rval = checkErrorFile(f);

    Sclose(f->fd);
    f->fd = NULL;
    assign(f, status, NAME_closed);

    return rval;
  }

  succeed;
}


status
existsFile(FileObj f, BoolObj mustbefile)
{ STAT_TYPE buf;
  const char *fn = charArrayToFN((CharArray)f->name);

#ifdef HAVE_ACCESS
  if ( mustbefile == OFF )
  { if ( access(fn, F_OK) == 0 )
      succeed;
    fail;
  }
#endif
  if ( STAT_FUNC(fn, &buf) == -1 )
    fail;
  if ( mustbefile != OFF && (buf.st_mode & S_IFMT) != S_IFREG )
    fail;
  succeed;
}


status
sameOsPath(const char *s1, const char *s2)
{ if ( streq(s1, s2) )
    succeed;

#if O_XOS
  return _xos_same_file(s1, s2);
#endif

#if __unix__
  { struct stat buf1;
    struct stat buf2;

    if ( stat(s1, &buf1) == 0 &&
	 stat(s2, &buf2) == 0 &&
	 buf1.st_ino == buf2.st_ino &&
	 buf1.st_dev == buf2.st_dev )
      succeed;
  }
#endif

  fail;
}


static status
sameFile(FileObj f1, FileObj f2)
{ Name n1 = getOsNameFile(f1);
  Name n2 = getOsNameFile(f2);

  if ( !n1 || !n2 )
    fail;

  return sameOsPath(strName(n1), strName(n2));
}



static status
absolutePathFile(FileObj f)
{ char path[MAXPATHLEN];

  if ( absolutePath(charArrayToUTF8((CharArray)f->name), path, sizeof(path)) > 0 )
  { assign(f, path, UTF8ToName(path));
    succeed;
  }

  return errorPce(f, NAME_representation, NAME_nameTooLong);
}


Name
getAbsolutePathFile(FileObj f)
{ char path[MAXPATHLEN];

  if ( notDefault(f->path) )
    answer(f->path);

  if ( absolutePath(charArrayToUTF8((CharArray)f->name), path, sizeof(path)) > 0 )
    return UTF8ToName(path);

  errorPce(f, NAME_representation, NAME_nameTooLong);
  fail;
}


status
isAbsoluteFile(FileObj f)
{ return isAbsolutePath(charArrayToUTF8((CharArray)f->name));
}


#define CPBUFSIZE 4096

#ifndef O_BINARY
#define O_BINARY 0
#endif

#ifdef __WINDOWS__
#include <fcntl.h>
#endif

#ifndef O_RDONLY
#define O_RDONLY _O_RDONLY
#define O_WRONLY _O_WRONLY
#endif

static int
open_file(FileObj f, int access, ...)
{ va_list args;
  int mode;
  int fd = -1;

  va_start(args, access);
  mode = va_arg(args, int);
  va_end(args);

  fd = open(charArrayToFN((CharArray)f->name), access, mode);

  if ( fd < 0 )
    errorPce(f, NAME_openFile,
	     (access & O_RDONLY) ? NAME_read : NAME_write,
	     getOsErrorPce(PCE));

  return fd;
}


static status
copyFile(FileObj to, FileObj from)
{ int fdfrom, fdto;
  char buf[CPBUFSIZE];
  status rval;
  int n;

  if ( (fdfrom = open_file(from, O_RDONLY|O_BINARY)) < 0 )
    fail;
  if ( (fdto = open_file(to, O_WRONLY|O_BINARY|O_CREAT|O_TRUNC, 0666)) < 0 )
  { close(fdfrom);
    fail;
  }

  while( (n = read(fdfrom, buf, CPBUFSIZE)) > 0 )
  { char *b = buf;

    while(n > 0)
    { int n2;

      if ( (n2=write(fdto, b, n)) < 0 )
      { errorPce(to, NAME_ioError, getOsErrorPce(PCE));
	rval = FAIL;
	goto out;
      }
      b += n2;
      n -= n2;
    }
  }
  if ( n < 0 )
  { errorPce(from, NAME_ioError, getOsErrorPce(PCE));
    rval = FAIL;
  } else
    rval = SUCCEED;

out:
  close(fdfrom);
  close(fdto);

  return rval;
}


static status
backup_name(const char *old, const char *ext, char *bak, size_t len)
{ if ( strlen(old) + strlen(ext) + 1 <= len )
  { sprintf(bak, "%s%s", old, ext);
    succeed;
  } else
  { errno = ENAMETOOLONG;
    fail;
  }
}


static Name
getBackupFileNameFile(FileObj f, Name ext)
{ char bak[MAXPATHLEN*2];

  if ( backup_name(nameToUTF8(f->name),
		   isDefault(ext) ? "~" : nameToUTF8(ext),
		   bak, sizeof(bak)) )
    answer(UTF8ToName(bak));

  errorPce(f, NAME_representation, NAME_nameTooLong);
  fail;
}


static status
backupFile(FileObj f, Name ext)
{ if ( existsFile(f, ON) )
  { Name newname = get(f, NAME_backupFileName, ext, EAV);
    const char *new;
    const char *old = nameToFN(getOsNameFile(f));
    int fdfrom = -1, fdto = -1;
    status rval = FAIL;

    if ( newname )
      new = nameToFN(newname);
    else
      fail;				/* or succeed? */

    if ( (fdfrom = open(old, O_RDONLY)) >= 0 &&
	 (fdto   = open(new, O_WRONLY|O_CREAT|O_TRUNC, 0666)) >= 0 )
    { char buf[CPBUFSIZE];
      int n;

      while( (n = read(fdfrom, buf, CPBUFSIZE)) > 0 )
      { if ( write(fdto, buf, n) != n )
	{ rval = FAIL;
	  goto out;
	}
      }
      rval = (n == 0) ? SUCCEED : FAIL;
    }

out:
    if ( rval == FAIL )
      errorPce(f, NAME_backupFile, newname, getOsErrorPce(PCE));

    if ( fdfrom >= 0 )
      close(fdfrom);
    if ( fdto >= 0 )
      close(fdto);

    return rval;
  }

  succeed;
}


static status
accessFile(FileObj f, Name mode)
{ int m;
  Name name = getOsNameFile(f);

  if ( name )
  { if ( mode == NAME_read )
      m = R_OK;
    else if ( mode == NAME_write || mode == NAME_append )
      m = W_OK;
    else /*if ( mode == NAME_execute )*/
#ifdef X_OK
      m = X_OK;
#else
      m = R_OK;
#endif

    if ( access(strName(name), m) == 0 )
      succeed;
  }

  fail;
}


static Attribute
getFilterFile(FileObj f)
{ Cell cell;

  closeFile(f);

  for_cell(cell, FileFilters->attributes)
  { char path[MAXPATHLEN];
    Attribute a = cell->value;
    Name extension = a->name;
    STAT_TYPE buf;

    if ( !isName(extension) )
    { errorPce(extension, NAME_unexpectedType, TypeName);
      fail;
    }

    sprintf(path, "%s%s", strName(f->name), strName(extension));
    if ( STAT_FUNC(path, &buf) == 0 &&
	 (buf.st_mode & S_IFMT) == S_IFREG )
    { if ( !isName(a->value) )
      { errorPce(a->value, NAME_unexpectedType, TypeName);
	fail;
      }

      answer(a);
    }
  }

  fail;
}


status
doBOMFile(FileObj f)
{ assert(f->fd);			/* must be open */

  if ( f->kind == NAME_text )
  { if ( f->status == NAME_read )
    { if ( f->bom != OFF )
      { if ( ScheckBOM(f->fd) < 0 )
	{ error:

	  reportErrorFile(f);
	  closeFile(f);

	  fail;
	}

	assign(f, bom, (f->fd->flags & SIO_BOM) ? ON : OFF);
	if ( f->bom == ON )
	  assign(f, encoding, encoding_to_name(f->fd->encoding));
      }
    } else				/* write */
    { if ( f->bom == ON )
      { if ( SwriteBOM(f->fd) < 0 )
	{ goto error;
	}
      }
    }
  }

  succeed;
}


status
openFile(FileObj f, Name mode, Name filter, CharArray extension)
{ CharArray path;
  Name name = getOsNameFile(f);
  char fdmode[3];

  if ( f->status == NAME_tmpWrite )
  { if ( mode == NAME_write || mode == NAME_append )
    { assign(f, status, NAME_write);
      succeed;
    }
  }

  closeFile(f);

  if ( !name )
    fail;

  if ( isDefault(filter) )
    filter = f->filter;

  if ( isDefault(extension) )
    path = (CharArray) name;
  else
    path = getAppendCharArray((CharArray) name, extension);

  if ( mode == NAME_write )
    fdmode[0] = 'w';
  else if ( mode == NAME_append )
    fdmode[0] = 'a';
  else /* read */
    fdmode[0] = 'r';

  if ( f->kind == NAME_text )
    fdmode[1] = '\0';
  else
    fdmode[1] = 'b';

  fdmode[2] = '\0';

  if ( isNil(filter) )
  { DEBUG(NAME_file, Cprintf("Opening %s (%s) using mode %s\n",
			     pp(f->name), pp(f), fdmode));
    f->fd = Sopen_file(charArrayToFN(path), fdmode);
  } else
#ifndef HAVE_POPEN
  { return errorPce(f, NAME_noPopen);
  }
#else
  { char cmd[LINESIZE];
    const char *fn = nameToFN(filter);
    const char *pn = charArrayToFN(path);
    const char *rn = (mode == NAME_read ? "<" : mode == NAME_write ? ">" : ">>");

    if ( fdmode[0] == 'a' )
      fdmode[0] = 'w';

    if ( strlen(fn)+strlen(pn)+7 > LINESIZE )
      return errorPce(f, NAME_representation, NAME_nameTooLong);

    sprintf(cmd, "%s %s \"%s\"", fn, rn, pn);
    f->fd = Sopen_pipe(cmd, fdmode);
  }
#endif /*HAVE_POPEN*/

  if ( f->fd == NULL )
  { if ( isNil(filter) && mode == NAME_read && errno == ENOENT )
    { Attribute a;

      if ( (a = get(f, NAME_filter, EAV)) )
      { if ( !isName(a->value) || !isName(a->name) )
	  fail;
	return openFile(f, mode, a->value, a->name);
      }
    }

    return errorPce(f, NAME_openFile, mode, getOsErrorPce(PCE));
  }

  if ( mode == NAME_append )
    mode = NAME_write;
  assign(f, status, mode);
  assign(f, filter, filter);

  if ( mode == NAME_read )
  { if ( !doBOMFile(f) )
      fail;
    if ( !setStreamEncodingSourceSink((SourceSink)f, f->fd) )
    { closeFile(f);
      fail;
    }
  } else
  { if ( !setStreamEncodingSourceSink((SourceSink)f, f->fd) )
    { closeFile(f);
      fail;
    }

    if ( mode != NAME_append && !doBOMFile(f) )
      fail;
  }

  succeed;
}


status
removeFile(FileObj f)
{ Name name = getOsNameFile(f);

  closeFile(f);				/* Ok? */

  if ( remove(nameToFN(name)) == 0 )
    succeed;
  if ( existsFile(f, OFF) )
    return errorPce(f, NAME_removeFile, getOsErrorPce(PCE));

  fail;
}


static status
nameFile(FileObj f, Name name)
{ int rval;
  Name ofn = getOsNameFile(f);
  Name nfn = expandFileName(name);

  if ( !nfn )
    fail;

  if ( existsFile(f, OFF) )
  { const char *ofns = nameToFN(ofn);
    const char *nfns = nameToFN(nfn);

#ifdef HAVE_RENAME
    remove(nfns);
    rval = rename(ofns, nfns);
#else
    unlink(nfns);
    if ((rval = link(ofns, nfns)) == 0 && (rval = unlink(ofns)) != 0)
      unlink(nfns);
#endif /*__unix__*/

    if ( rval == 0 )
    { assign(f, name, nfn);
      succeed;
    }

    return errorPce(f, NAME_renameFile, name, getOsErrorPce(PCE));
  } else
  { assign(f, name, name);
    succeed;
  }
}


static status
check_file(FileObj f, Name mode)
{ if ( (mode == f->status) ||
       (mode == NAME_write && f->status == NAME_append) ||
       (mode == NAME_open && f->status != NAME_closed) )
    succeed;

  return errorPce(f, NAME_notOpenFile, mode);
}


static Int
getIndexFile(FileObj f)
{ TRY( check_file(f, NAME_open) );

  answer(toInt(Stell(f->fd)));
}


static status
seekFile(FileObj f, Int index, Name whence)
{ TRY( check_file(f, NAME_open) );
  if ( notNil(f->filter) )
    return errorPce(f, NAME_cannotSeekNonFile);

  if ( isDefault(whence) )
    whence = NAME_start;

  if ( Sseek(f->fd, valInt(index), whence == NAME_start ? 0 :
				   whence == NAME_here ? 1 :
				   2 ) == -1 )
    return errorPce(f, NAME_seekFile, index, whence, getOsErrorPce(PCE));

  succeed;
}


static status
append_file(FileObj f, String str)
{ TRY( check_file(f, NAME_write) );

  if ( f->encoding == NAME_octet )
  { if ( Sfwrite(str->s_text,
		 isstrA(str) ? sizeof(charA) : sizeof(charW),
		 str->size,
		 f->fd) != str->size )
      return reportErrorFile(f);
  } else
  { if ( isstrA(str) )
    { const charA *s = str->s_textA;
      const charA *e = &s[str->size];

      for(; s<e; s++)
      { if ( Sputcode(*s, f->fd) < 0 )
	  return reportErrorFile(f);
      }
    } else
    { const charW *s = str->s_textW;
      const charW *e = &s[str->size];

      for(; s<e; s++)
      { if ( Sputcode(*s, f->fd) < 0 )
	  return reportErrorFile(f);
      }
    }
  }

  succeed;
}


static status
newlineFile(FileObj f)
{ return append_file(f, str_nl(NULL));	/* only ASCII */
}


static status
appendFile(FileObj f, CharArray str)
{ return append_file(f, &str->data);
}


static status
formatFile(FileObj f, CharArray fmt, int argc, Any *argv)
{ string s;

  TRY(str_writefv(&s, fmt, argc, argv));
  append_file(f, &s);
  str_unalloc(&s);

  succeed;
}


static status
flushFile(FileObj f)
{ if ( f->fd )
    Sflush(f->fd);

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We can use the faster fstat() here, but confirmed by various messages on
the web, MS-Windows implementation  of   _fstat()  is  broken, returning
EBADF for perfectly valid filedescriptors.  Shouldn't make a difference,
only slow ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
statFile(FileObj f, STAT_TYPE *buf)
{
#ifndef __WINDOWS__
  int fno;

  if ( f->fd != NULL && (fno = Sfileno(f->fd)) >= 0)
  { return FSTAT_FUNC(fno, buf);
  } else
#endif
  { Name name = getOsNameFile(f);

    return STAT_FUNC(nameToFN(name), buf);
  }
}



static Int
getSizeFile(FileObj f)
{ STAT_TYPE buf;

  if ( statFile(f, &buf) == -1 )
  { errorPce(f, NAME_cannotStat, getOsErrorPce(PCE));
    fail;
  }

  answer(toInt(buf.st_size));
}


static Date
getTimeFile(FileObj f, Name which)
{ STAT_TYPE buf;

  if ( isDefault(which) )
    which = NAME_modified;

  if ( statFile(f, &buf) < 0 )
  { errorPce(f, NAME_cannotStat, getOsErrorPce(PCE));
    fail;
  }

  if ( which == NAME_modified )
    answer(CtoDate(buf.st_mtime));
  else
    answer(CtoDate(buf.st_atime));
}


Name
getBaseNameFile(FileObj f)
{ const char *ufn = nameToUTF8(f->name);

  answer(UTF8ToName(baseName(ufn)));
}


static Name
getDirectoryNameFile(FileObj f)
{ char dir[MAXPATHLEN];

  dirName(nameToUTF8(getOsNameFile(f)), dir, sizeof(dir));

  answer(UTF8ToName(dir));
}


static StringObj
getReadLineFile(FileObj f)
{ tmp_string tmp;
  StringObj rval;

  TRY( check_file(f, NAME_read) );

  str_tmp_init(&tmp);

  for(;;)
  { int c = Sgetcode(f->fd);

    if ( c == EOF )
    { if ( tmp.s.size == 0 )
	fail;
      break;
    }

    str_tmp_put(&tmp, (wint_t)c);
    if ( c == '\n' )
      break;
  }

  rval = StringToString(&tmp.s);
  str_tmp_done(&tmp);

  return rval;
}


static StringObj
getReadFile(FileObj f, Int n)
{ size_t size;
  StringObj s;

  TRY( check_file(f, NAME_read) );
  if ( isDefault(n) )
  { Int here = getIndexFile(f);
    Int len  = getSizeFile(f);

    if ( !here || !len )
      fail;
    n = sub(len, here);
  }

  size = valInt(n);
  if ( size > STR_MAX_SIZE )
  { errorPce(f, NAME_stringTooLong, toInt(size));
    fail;
  }

  if ( f->encoding == NAME_octet )
  { size_t m;

    s = answerObject(ClassString, EAV);
    str_unalloc(&s->data);
    str_inithdr(&s->data, FALSE);
    s->data.size = (int)size;
    str_alloc(&s->data);

    if ( (m = Sfread(s->data.s_textA, 1, size, f->fd)) != size )
    { deleteString(s, toInt(m), DEFAULT); 			/* TBD: error? */
    }
  } else
  { tmp_string tmp;
    int c;

    str_tmp_init(&tmp);
    while(tmp.s.size < size && (c = Sgetcode(f->fd)) != EOF )
    { str_tmp_put(&tmp, (wint_t)c);
    }
    if ( !checkErrorFile(f) )
    { str_tmp_done(&tmp);
      fail;
    }
    s = StringToString(&tmp.s);
    str_tmp_done(&tmp);
  }

  answer(s);
}


static Int
getCharacterFile(FileObj f)
{ int chr;

  TRY( check_file(f, NAME_read) );
  if ( Sfeof(f->fd) )
    fail;

  chr = Sgetcode(f->fd);

  answer(toInt(chr));
}


		/********************************
		*       SAVE/LOAD SUPPORT	*
		********************************/

status
reportErrorFile(FileObj f)
{ errorPce(f, NAME_ioError, getOsErrorPce(PCE));
  fail;
}


status
checkErrorFile(FileObj f)
{ if ( f->fd == NULL )
    succeed;

  if ( Sferror(f->fd) )
    return reportErrorFile(f);

  succeed;
}


status
storeCharFile(FileObj f, int c)
{ if ( f->encoding == NAME_octet )
    Sputc(c, f->fd);
  else
    Sputcode(c, f->fd);

  return checkErrorFile(f);
}


void
putstdw(unsigned long w, IOSTREAM *fd)
{
#ifndef WORDS_BIGENDIAN
  union
  { unsigned long l;
    unsigned char c[4];
  } cvrt;
  unsigned long rval;

  cvrt.l = w;
  rval = (cvrt.c[0] << 24) |
         (cvrt.c[1] << 16) |
	 (cvrt.c[2] << 8) |
	  cvrt.c[3];
  Sputw(rval, fd);
#else /*WORDS_BIGENDIAN*/
  Sputw(w, fd);
#endif /*WORDS_BIGENDIAN*/
}


status
storeWordFile(FileObj f, Any w)
{ putstdw((unsigned long) w, f->fd);

  return checkErrorFile(f);
}

#ifdef WORDS_BIGENDIAN
static const int double_byte_order[] = { 7,6,5,4,3,2,1,0 };
#else
static const int double_byte_order[] = { 0,1,2,3,4,5,6,7 };
#endif

#define BYTES_PER_DOUBLE (sizeof(double_byte_order)/sizeof(int))

status
storeDoubleFile(FileObj file, double f)
{ unsigned char *cl = (unsigned char *)&f;
  unsigned int i;

  for(i=0; i<BYTES_PER_DOUBLE; i++)
    Sputc(cl[double_byte_order[i]], file->fd);

  return checkErrorFile(file);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
storeStringFile() stores a string to a   file. For compatibility reasons
the format is somewhat strange. If the string  is 8-bit, it is stored as
length followed by  the  character  data.   Otherwise  it  is  stores as
NEGATIVE length followed by a sequence of UTF-8 character codes.

Note that if the string is wide but need not be, it is saved as if it is
an ISO Latin-1 string.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
storeStringFile(FileObj f, String s)
{ if ( isstrA(s) )
  { TRY(storeWordFile(f, (Any) (long)s->size));
    Sfwrite(s->s_textA, sizeof(char), s->size, f->fd);

    DEBUG(NAME_save, Cprintf("Saved ISO string, %ld chars\n", s->size));
  } else if ( !str_iswide(s) )
  { const charW *w = s->s_textW;
    const charW *e = &w[s->size];

    TRY(storeWordFile(f, (Any) (long)s->size));
    for( ; w<e; w++)
    { if ( Sputc(*w, f->fd) < 0 )
	return checkErrorFile(f);
    }

    DEBUG(NAME_save,
	  Cprintf("Saved converted ISO string, %ld chars\n", s->size));
  } else
  { IOENC oenc;
    const charW *w = s->s_textW;
    const charW *e = &w[s->size];

    TRY(storeWordFile(f, (Any) -(long)s->size));
    oenc = f->fd->encoding;
    f->fd->encoding = ENC_UTF8;
    for( ; w<e; w++)
    { if ( Sputcode(*w, f->fd) < 0 )
      { f->fd->encoding = oenc;
	return checkErrorFile(f);
      }
    }
    f->fd->encoding = oenc;

    DEBUG(NAME_save, Cprintf("Saved wide string, %ld chars\n", s->size));
  }

  return checkErrorFile(f);
}


status
storeNameFile(FileObj f, Name n)
{ return storeStringFile(f, &n->data);
}


status
storeIntFile(FileObj f, Int i)
{ return storeWordFile(f, (Any) valInt(i));
}


		/********************************
		*             PATHS		*
		********************************/

static int
waccess(const wchar_t *name, int m)
{ string s;
  const char *ufn;

  str_set_n_wchar(&s, wcslen(name), (wchar_t *)name);
  ufn = stringToFN(&s);

  DEBUG(NAME_find, Cprintf("find: trying \"%s\"\n", ufn));

  return access(ufn, m);
}


#ifndef X_OK
#define X_OK 0
#endif

status
findFile(FileObj f, CharArray path, Name mode)
{ wchar_t *base;
  wchar_t basebuf[MAXPATHLEN];
  const wchar_t *pathstr;
  size_t bl;
  int m;

  if ( isAbsolutePath(nameToUTF8(f->name)) )
    succeed;

  base = charArrayToWC((CharArray)f->name, &bl);
  if ( base[0] == '.' )
    succeed;

  if ( isDefault(mode) || mode == NAME_read )
    m = R_OK;
  else if ( mode == NAME_write || mode == NAME_append )
    m = W_OK;
  else /*if ( mode == NAME_execute )*/
    m = X_OK;

  if ( notDefault(f->path) && access(nameToFN(f->path), m) == 0 )
    succeed;

  if ( bl+1 > MAXPATHLEN )
    return errorPce(f, NAME_representation, NAME_nameTooLong);
  wcscpy(basebuf, base);
  base = basebuf;

  if ( isDefault(path) )
    pathstr = L".";
  else
    pathstr = charArrayToWC(path, NULL);

  while( pathstr && *pathstr )
  { wchar_t name[MAXPATHLEN];
    wchar_t bin[MAXPATHLEN];
    const wchar_t *end = pathstr;
    size_t l;

#ifdef __WINDOWS__
    if ( end[0] < 0x80 && isalpha(end[0]) && end[1] == ':' )
      end += 2;
#endif

    if ( (end = wcschr(end, ':')) == NULL )
    { wcscpy(name, pathstr);
      pathstr = NULL;
    } else
    { wcsncpy(name, pathstr, end-pathstr);
      name[end-pathstr] = EOS;
      pathstr = &end[1];
    }

    if ( wcschr(name, L'$') || name[0] == L'~' )
    { if ( (l=expandFileNameW(name, bin, MAXPATHLEN)) > 0 )
	wcsncpy(name, bin, l);
      else
	continue;
    } else
    { l = wcslen(name);
    }

    name[l] = '/';
    wcscpy(&name[l+1], base);

    if ( waccess(name, m) == 0 )
    { assign(f, path, WCToName(name, wcslen(name)));
      succeed;
    }
  }

  return errorPce(f, NAME_cannotFindFile, path);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_seek[] =
        { "byte=int", "from=[{start,here,end}]" };
static char *T_format[] =
        { "format=char_array", "argument=any ..." };
static char *T_open[] =
        { "mode={read,write,append}", "filter=[name]",
	  "extension=[char_array]" };
static char *T_find[] =
        { "path=[char_array]", "access=[{read,write,append,execute}]" };
static char *T_initialise[] =
        { "path=[name]",
	  "encoding=[{text,binary,iso_latin_1,utf8,unicode_be,unicode_le}]"
	};

/* Instance Variables */

static vardecl var_file[] =
{ SV(NAME_name, "name=name", IV_GET|IV_STORE, nameFile,
     NAME_path, "Name of the file"),
  IV(NAME_path, "path=[name]", IV_BOTH,
     NAME_path, "Full path-name of the file"),
  SV(NAME_kind, "{text,binary}", IV_GET|IV_STORE, kindFile,
     NAME_fileType, "Text or binary file"),
  IV(NAME_status, "{closed,read,write,tmp_write}", IV_GET,
     NAME_open, "(How) opened or closed?"),
  IV(NAME_filter, "command=name*", IV_BOTH,
     NAME_filter, "Name of input/output filter used"),
  IV(NAME_bom, "[bool]", IV_BOTH,
     NAME_encoding, "Byte Order Mark"),
  IV(NAME_newlineMode, "{posix,dos,detect}", IV_BOTH,
     NAME_encoding, "Newline representation"),
  IV(NAME_fd, "alien:FILE *", IV_NONE,
     NAME_internal, "Unix file (stream) handle")
};

/* Send Methods */

static senddecl send_file[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseFile,
     DEFAULT, "Create from name and kind"),
  SM(NAME_unlink, 0, NULL, unlinkFile,
     DEFAULT, "Close file"),
  SM(NAME_encoding, 1, "{text,binary,iso_latin_1,utf8,unicode_be,unicode_le}",
     kindFile, NAME_fileType, "Specify text or binary encoding type"),
  SM(NAME_backup, 1, "extension=[name]", backupFile,
     NAME_copy, "Make a backup by adding extension (~)"),
  SM(NAME_copy, 1, "from=file", copyFile,
     NAME_copy, "Copy to destination file"),
  SM(NAME_remove, 0, NULL, removeFile,
     NAME_delete, "Unlink from Unix file system"),
  SM(NAME_find, 2, T_find, findFile,
     NAME_location, "Find file in search-path"),
  SM(NAME_seek, 2, T_seek, seekFile,
     NAME_location, "Seek to index from {start,here,end}"),
  SM(NAME_close, 0, NULL, closeFile,
     NAME_open, "Close file"),
  SM(NAME_open, 3, T_open, openFile,
     NAME_open, "Open file in mode, read/write through filter"),
  SM(NAME_absolutePath, 0, NULL, absolutePathFile,
     NAME_path, "Convert <-name to an absolute path"),
  SM(NAME_isAbsolute, 0, NULL, isAbsoluteFile,
     NAME_path, "Test if <-name specifies an absolute path"),
  SM(NAME_access, 1, "mode={read,write,append,execute}", accessFile,
     NAME_test, "Test if file has access"),
  SM(NAME_exists, 1, "must_be_file=[bool]", existsFile,
     NAME_test, "Test if file exists"),
  SM(NAME_same, 1, "file=file", sameFile,
     NAME_test, "Test if two paths refer to the same physical file"),
  SM(NAME_append, 1, "text=char_array", appendFile,
     NAME_write, "Append string to file"),
  SM(NAME_flush, 0, NULL, flushFile,
     NAME_write, "Flush pending output"),
  SM(NAME_format, 2, T_format, formatFile,
     NAME_write, "Format arguments and ->append"),
  SM(NAME_newline, 0, NULL, newlineFile,
     NAME_write, "Append newline to file")
};

/* Get Methods */

static getdecl get_file[] =
{ GM(NAME_convert, 1, "file", "path=name", getConvertFile,
     DEFAULT, "Convert name to file"),
  GM(NAME_backupFileName, 1, "char_array", "extension=[char_array]",
     getBackupFileNameFile,
     NAME_copy, "Name for storing ->backup data"),
  GM(NAME_size, 0, "bytes=int", NULL, getSizeFile,
     NAME_dimension, "Size in characters"),
  GM(NAME_filter, 0, "extension_and_filter=attribute", NULL, getFilterFile,
     NAME_filter, "Determine input filter from extension"),
  GM(NAME_index, 0, "byte=int", NULL, getIndexFile,
     NAME_location, "Current index (Unix tell())"),
  GM(NAME_absolutePath, 0, "path=name", NULL, getAbsolutePathFile,
     NAME_path, "Convert <-name to an absolute path"),
  GM(NAME_baseName, 0, "name", NULL, getBaseNameFile,
     NAME_path, "Base name of file in directory"),
  GM(NAME_directoryName, 0, "name", NULL, getDirectoryNameFile,
     NAME_path, "Directory name of file"),
  GM(NAME_character, 0, "char", NULL, getCharacterFile,
     NAME_read, "Read next character as ASCII value"),
  GM(NAME_read, 1, "string", "count=[int]", getReadFile,
     NAME_read, "New string width next n characters"),
  GM(NAME_readLine, 0, "string", NULL, getReadLineFile,
     NAME_read, "New string with next line"),
  GM(NAME_time, 1, "date=date", "which_time=[{modified,access}]", getTimeFile,
     NAME_time, "New date holding modification/access time")
};

/* Resources */

#define rc_file NULL
/*
static classvardecl rc_file[] =
{
};
*/

/* Class Declaration */

static Name file_termnames[] = { NAME_name };

ClassDecl(file_decls,
          var_file, send_file, get_file, rc_file,
          1, file_termnames,
          "$Rev$");

status
makeClassFile(Class class)
{ declareClass(class, &file_decls);
  setLoadStoreFunctionClass(class, loadFile, storeFile);

#if defined(__WINDOWS__)
  featureClass(class, NAME_caseSensitive,  OFF);
  featureClass(class, NAME_casePreserving, ON);
  featureClass(class, NAME_8plus3names,    OFF);
#else
  featureClass(class, NAME_caseSensitive,  ON);
  featureClass(class, NAME_casePreserving, ON);
  featureClass(class, NAME_8plus3names,    OFF);
#endif

  FileFilters = globalObject(NAME_compressionFilters, ClassSheet,
			     newObject(ClassAttribute,
				       CtoName(".Z"),
				       CtoName("uncompress"),
				       EAV),
			     newObject(ClassAttribute,
				       CtoName(".gz"),
				       CtoName("gunzip"),
				       EAV),
			     EAV);

  succeed;
}
