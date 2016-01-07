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

#ifndef XOS_INCLUDED
#define XOS_INCLUDED

#ifndef __XOS__
#define __XOS__ 1
#endif

#include <direct.h>

#ifndef __XOS_KERNEL__
#define malloc(size)		_xos_malloc(size)
#define realloc(buf, size)	_xos_realloc(buf, size)
#define chdir(path)		_xos_chdir(path)
#define open			_xos_open
#define fopen(path, mode)	_xos_fopen(path, mode)
#define opendir(path)		_xos_opendir(path)
#define readdir(dp)		_xos_readdir(dp)
#define stat(path, buf)		_xos_stat(path, buf)
#define access(path, mode)	_xos_access(path, mode)
#define getcwd(buf, size)	_xos_getcwd(buf, size)
#define unlink(path)		_xos_unlink(path)
#define remove(path)		_xos_remove(path)
#define rename(old, new)	_xos_rename(old, new)
#define chmod(path, mode)	_xos_chmod(path, mode)
#define mkdir(path)		_xos_mkdir(path)
#define rmdir(path)		_xos_rmdir(path)
#endif

void *		_xos_malloc(size_t bytes);
void *		_xos_realloc(void *mem, size_t bytes);
int		_xos_chdir(const char *path);
int		_xos_open(const char *path, int access, ...);
FILE *		_xos_fopen(const char *path, const char *mode);
DIR *		_xos_opendir(const char *path);
struct dirent *	_xos_readdir(DIR *dp);
int		_xos_access(const char *path, int mode);
int		_xos_stat(const char *path, struct stat *buf);
char *		_xos_getcwd(char *buffer, size_t bytes);
int		_xos_unlink(const char *path);
int		_xos_remove(const char *path);
int		_xos_rename(const char *old, const char *new);
int		_xos_chmod(const char *path, int mode);
int		_xos_mkdir(const char *path);
int		_xos_rmdir(const char *path);

char *		_xos_canonical_filename(const char *in, char *out);
char *		_xos_os_filename(const char *in, char *out);
char *		_xos_limited_os_filename(const char *in, char *out);

#endif /*XOS_INCLUDED*/
