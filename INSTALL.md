# Installing XPCE/Prolog from source

Normally, xpce is provided as `packages/xpce` in the SWI-Prolog sources.
It     is     build      as      part       of      SWI-Prolog.      See
http://www.swi-prolog.org/build/index.txt for building   SWI-Prolog  and
xpce.

The documentation below is in part out of date, but left as a reference.
Configuration, compilation and installation is based on the GNU autoconf
package.

## Required tools

	- GNU-Make
	Non-gnu versions of make will fail.

	- GCC or clang
	Necessary on most machines, though you might get away with
	another ANSI C compiler.

	- libXpm
	The X11 XPM (XPixMap) format libary for handling coloured and
	masked images.  For any popular Unix platform you should be
	able to find this package.  For the Windows built we provide
	a port at https://github.com/SWI-Prolog/libXpm.git

	- libjpeg
	The JPEG group library for handling JPEG images.

	- Freetype
	Not really required, but if present the system will use the
	Freetype library to realise scalable and antialiased fonts.
	Its presence is detected by looking for the program xft-config,
	which is also used to determine compile and link flags.  If
	you want to build WITHOUT XFT and you have the library installed,
	set the environment variable XFTCONFIG=false.


For all other required tools, both the   GNU,  BSD and System-V versions
are supposed to work properly.

## Bluffers Installation Guide

To install XPCE/SWI-Prolog from the sources:

	1. Build SWI-Prolog according to the instructions and install it.
	2. Determine an installation prefix (normally use the same as for
	   SWI-Prolog, we use `linux' in this example).
	3. Run the following commands

		% mkdir linux
		% cd linux
		% ../src/configure
		% make
		% make install

# Detailed Installation Guide

We start at the SWI-Prolog  installation,   as  this  needs some special
considerations on some platforms.


## Choosing the build-directory

You can place the directory for   building  XPCE/Prolog anywhere. A good
choice is /usr/local/src or, if you are   installing  as a private user,
$HOME/src. Unpack the Prolog and XPCE archives from the same directory:

	% gunzip < pl-<version>.tar.gz | tar xvfB -
	% gunzip < xpce-<version>.tar.gz | tar xvfB -

NOTES:

	- Some versions of tar hate reading from a pipe.  In that case
	use `gunzip file.tar.gz' followed by `tar xvf file.tar' to extract
	the archives

	- If you are using GNU-tar, `tar zxvf file.tar.gz' is easier.


## Choosing a build sub-directory

For easy cleanup or building for   multiple directories, both SWI-Prolog
and XPCE are normally built in a   directory  next to the src directory.
The name is not important. Good examples  are `sunos' `linux', etc. In
the examples below, we use `linux'.


## The Destination Prefix (configure --prefix=dir)

GNU autoconf-based packages accept the   flag  --prefix=<dir> to specify
the destination. The installation will use the following subdirectories:

```
	bin			For making *links* to the executables
	man/man1		For installing the manual pages
	include			For installing the SWI-Prolog.h header file
	lib/pl-<version>	For installing libs, executables, etc.
	lib			For installing public shared objects (if any)
```

The default prefix is /usr/local.  If   you  choose  another one (assume
/home/projects/bigmoney), do (if the  directory   structure  is  not yet
available):

	% cd /home/projects/bigmoney
	% mkdir bin man man/man1 include lib

Make sure the binary directory is in your PATH!

## Preparing SWI-Prolog

	% cd pl-<version>
	% mkdir linux
	% cd linux
	% ../src/configure

## Shared libraries or not?

By default, the installation atempts to   build SWI-Prolog and load xpce
as a shared object (.so file on   most  Unix machines) into Prolog. This
installation is better to maintain and   prepares SWI-Prolog for loading
custom extensions written in C in a well supported manner.

Now, shared libraries are  used  on  all   systems  and  the  the static
alternative is no longer actively maintained. When  porting to a new OS,
please check the shared library facilities   of the SWI-Prolog plld tool
and make sure it runs on your environment.

## Preparing XPCE

XPCE extracts most information about  the configuration from SWI-Prolog,
so it is generally much easier. Most of the configure run is simple, but
configure needs to find out where the   X11  libraries and include files
are  and  you  may  have   multiple.    Normally,   it  will  first  try
/usr/include/X11 and /usr/lib. If  you  have   only  one  version of X11
around, generally configure will be able to find it.

If you have multiple copies of X11 around,   you have to decide which to
use. If you are compiling for local  usage,   use  the  one most of your
local packages use to improve resource   sharing. Otherwise, use the one
distributed with the OS or known to be most commonly in use by your user
community. For example, our system has the   MIT X11R6 libs and includes
in  /usr/lib  and  /usr/include/X11  and  the  OpenWindows  versions  in
/usr/openwin/lib  and  /usr/openwin/include/X11.  For  local  usage,  we
configure without options to use the X11R6  version, used by most of the
other X11 software we run locally. For distribution, we configure using:

	--x-includes=/usr/openwin/include --x-libraries=/usr/openwin/lib

## Checking Program versions

Just to make sure your programs are accessible and of the right version,
try:

	% swipl -v
	SWI-Prolog version 5.3.9 for i686-linux
	% make -v
	GNU Make 3.80
	...
	% gcc -v
	gcc -v
	Reading specs from /usr/lib/gcc-lib/i586-suse-linux/3.3.1/specs
	...
	gcc version 3.3.1 (SuSE Linux)

Now, run configure:

	% cd xpce-<version>
	% mkdir linux
	% ../src/configure <options>

## Building the library

Now, make the XPCE library:

	% make xpce

If you have configured for using shared libraries, the -fPIC flag should
be passed to the compiler. If this is  not the case, please look closely
at the steps above. If all goes well, the compilation should finish with
few warnings, resulting in the library libXPCE.a


## Building the interface:

With write premission to the Prolog home directory, now do the following
to make the interface.

	% make pl-itf

This command will run  either  `make   pl-shared'  or  `make pl-static',
depending on whether SWI-Prolog handles shared objects. The first builds
the pl2xpce.so shared object, and the   second builds an XPCE executable
holding both the Prolog kernel and the XPCE library called `xpce'.

Both versions will put various things   in the SWI-Prolog home directory
to make XPCE known to Prolog:

	* A link to the xpce build-directory
	* A swipl.rc script to register the xpce library
	* A Makefile to recompile the XPCE/Prolog quick-load-files

## Testing XPCE

Now, if you have build  for  a   shared  object,  xpce  is a dynamically
loadable Prolog library, so to test it simply do:

	% swipl
	?- manpce.

which should start the manual tools.

## Installing the XPCE library

Finally, to install the XPCE libraries, do:

	% make install

Which will:

	- Create a directory xpce-<version> in the Prolog home and
	copy the Prolog libraries, manual data and other resources
	into this directory.

	- In the Prolog home, make a link from xpce to xpce-<version>

	- run `make' in the Prolog home directory to make quick-load
	versions of some large and frequently used library packages.

	- in $exec_prefix/bin, make links to the xpce and xpce-client
	executables
