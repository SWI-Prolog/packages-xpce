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

#ifndef _PCE_UNX_INCLUDED
#define _PCE_UNX_INCLUDED

#include <h/graphics.h>
#include <unx/proto.h>

#ifdef HAVE_UXNT_H
#include <uxnt.h>
#endif


		 /*******************************
		 *	   AUTOCONF STUFF	*
		 *******************************/

#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

		 /*******************************
		 *	 DOS/UNIX PROBLEMS	*
		 *******************************/

#if defined(O_DOSFILENAMES) && O_DOSFILENAMES
#define IsDirSep(c) ((c) == '/' || (c) == '\\')
#else
#define IsDirSep(c) ((c) == '/')
#endif

#ifdef O_XOS
#define STAT_TYPE struct _stati64
#define STAT_FUNC _xos_stat
#define FSTAT_FUNC _fstati64
#else
#define STAT_TYPE struct stat
#define STAT_FUNC stat
#define FSTAT_FUNC fstat
#endif

		 /*******************************
		 *     AND THE NEAT STUFF!	*
		 *******************************/

#define ABSTRACT_STREAM \
  Code		input_message;		/* Message forwarded on input */ \
  Any		record_separator;	/* Separate input records */ \
  intptr_t	wrfd;			/* FD to write to process */ \
  intptr_t	rdfd;			/* FD to read from process */ \
  FILE *	rdstream;		/* Stream to read from process */ \
  WsRef		ws_ref;			/* Window System Handle */ \
  unsigned char * input_buffer;		/* Input buffer */ \
  intptr_t	input_allocated;	/* Allocated size of buffer */ \
  intptr_t	input_p;		/* Pointer into input buffer */


NewClass(fileobj)
  ABSTRACT_SOURCE_SINK
  Name		name;			/* name of the file */
  Name		path;			/* full path-name of the file */
  Name		kind;			/* {text,binary} */
  Name		status;			/* current open mode */
  Name		filter;			/* I/O filter used */
  BoolObj	bom;			/* Use/has BOM marker? */
  Name		newline_mode;		/* Newline mode {dos,posix} */
  IOSTREAM     *fd;			/* file descriptor */
End;


NewClass(rc)
  ABSTRACT_SOURCE_SINK
  Name		name;			/* name of the resource */
  Name		rc_class;		/* class of the resource */
  Any		context;		/* Module info */
End;


NewClass(directory)
  Name		name;			/* name of directory */
  Name		path;			/* full path name */
  intptr_t      modified;		/* time stamp (was: time_t) */
End;


NewClass(stream)
  ABSTRACT_STREAM
End;


NewClass(process)
  ABSTRACT_STREAM
  CharArray	name;			/* name of command ran */
  Vector	arguments;		/* vector of arguments */
  Name		status;			/* status of process */
  Any		code;			/* Signal/exit status */
  BoolObj	use_tty;		/* use a tty? */
  Name		tty;			/* Pseudo tty used */
  Code		terminate_message;	/* message forwarded o terminate */
  Int		pid;			/* Process id */
  Directory	directory;		/* Initial working dir */
  Sheet		environment;		/* Child environment  */
End;


NewClass(socketobj)
  ABSTRACT_STREAM
  Any		address;		/* Address for the socket  */
  Name		domain;			/* {unix,inet}  */
  Name		status;			/* status of socket */
  Code		accept_message;		/* message forwarded on accept */
  Chain		clients;		/* Chain of accepted sockets */
  Socket	master;			/* Master socket (listen)  */
  FileObj	authority;		/* Authority-file */
End;

#endif /* _PCE_UNX_INCLUDED */
