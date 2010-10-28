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

#define OEMRESOURCE 1			/* get OBM_* constants */
#define BoolObj PceBool
#include "include.h"
#include <h/unix.h>
#undef BoolObj
#include <math.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#define OsError() getOsErrorPce(PCE)

#ifdef O_XPM
#define FOR_MSW 1
#ifdef __CYGWIN__XXX
#include <xpm.h>
#else
#include <msw/xpm.h>
#endif
#endif

#undef offset
#define offset(s, f) ((int)&((s *)NULL)->f)

/* Using ws_ref for storing the bits and the xref mechanism for storing
   the Windows HBITMAP handle
*/


static int	ws_sizeof_bits(int w, int h);
static status	ws_attach_xpm_image(Image image, XpmImage* xpmimg,
				    XpmInfo* xpminfo);

#define rfloat inline_float		/* local inline version */

static inline int
rfloat(float f)
{ if (f > 0.0)
    return (int) (f+0.4999999);

  return (int) (f-0.4999999);
}


void
ws_init_image(Image image)
{ image->ws_ref = NULL;
}


static WsImage
attach_ws_image(Image image)
{ if ( !image->ws_ref )
  { image->ws_ref = alloc(sizeof(ws_image));

    memset(image->ws_ref, 0, sizeof(ws_image));
  }

  return image->ws_ref;
}


void
ws_destroy_image(Image image)
{ WsImage r;
/*Xref xref;*/

  if ( (r=image->ws_ref) )
  { if ( r->data )
      pceFree(r->data);
    if ( r->msw_info )
      pceFree(r->msw_info);
    if ( r->icon )
      DestroyIcon(r->icon);
    unalloc(sizeof(ws_image), image->ws_ref);

    image->ws_ref = NULL;
  }

/* Done by ws_close_image()
  while((xref = unregisterXrefObject(image, DEFAULT)))
  { HBITMAP bm = (HBITMAP) xref->xref;
    ZDeleteObject(bm);
  }
*/
}


status
ws_store_image(Image image, FileObj file)
{ HBITMAP bm;
  DisplayObj d = image->display;

  if ( isNil(d) )
    d = CurrentDisplay(image);

  if ( (bm = getXrefObject(image, d)) )
  { Sputc('P', file->fd);
    DEBUG(NAME_ppm, Cprintf("Saving PNM image from index %d\n",
			    Stell(file->fd)));
    if ( write_pnm_file(file->fd, bm, 0, 0, PNM_RUNLEN) < 0 )
      fail;
  }

  succeed;
}


status
loadXImage(Image image, IOSTREAM *fd)
{ fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
loadPNMImage() is used for loading saved-objects holding images.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
loadPNMImage(Image image, IOSTREAM *fd)
{ Name kind;
  HBITMAP bm = read_ppm_file(fd, &kind);

  if ( isNil(image->display) )
    assign(image, display, CurrentDisplay(NIL));

  if ( bm )
  { BITMAP bitmap;

    if ( !GetObject(bm, sizeof(BITMAP), &bitmap) )
      Cprintf("loadPPMImage(): GetObject() failed\n");
    assign(image->size, w, toInt(bitmap.bmWidth));
    assign(image->size, h, toInt(bitmap.bmHeight));
    assign(image, kind, kind);
    assign(image, depth, toInt(bitmap.bmPlanes * bitmap.bmBitsPixel));
    registerXrefObject(image, image->display, (void *) bm);

    succeed;
  }

  fail;
}


status
ws_load_old_image(Image image, IOSTREAM *fd)
{ fail;
}


#ifndef BM				/* SDK tells me this exists, but */
#define BM 0x4d42			/* it doesn't.  Hope it is correct! */
#endif

static int
color_quads_in_bitmap_info(BITMAPINFOHEADER *hdr)
{ if ( hdr->biClrUsed )
    return hdr->biClrUsed;
  else if ( hdr->biBitCount == 24 )
    return 0;				/* direct color */
  else
  { assert(hdr->biBitCount <= 8);
    return 1 << hdr->biBitCount;
  }
}

static BITMAPINFO *
read_bitmap_info(Image img, IOSTREAM *fd)
{ BITMAPINFOHEADER bmih;
  int rgbquads;
  BITMAPINFO *bmi;

  if ( Sfread(&bmih, sizeof(bmih), 1, fd) != 1 )
  { checkErrorSourceSink(img->file, fd);
    return NULL;
  }
  rgbquads = color_quads_in_bitmap_info(&bmih);
  DEBUG(NAME_image, Cprintf("%dx%d; %d rgbquads\n",
			    bmih.biWidth, bmih.biHeight, rgbquads));
  bmi = pceMalloc(sizeof(bmih) + sizeof(RGBQUAD)*rgbquads);
  memcpy(&bmi->bmiHeader, &bmih, sizeof(bmih));
  if ( Sfread(&bmi->bmiColors, sizeof(RGBQUAD), rgbquads, fd) != rgbquads )
  { checkErrorSourceSink(img->file, fd);
    return NULL;
  }

  return bmi;
}


static void
register_colours(BITMAPINFO *bmi)
{ int ncolors = 0;

  if ( bmi->bmiHeader.biClrImportant > 0 )
    ncolors = bmi->bmiHeader.biClrImportant;
  else if ( bmi->bmiHeader.biClrUsed > 0 )
    ncolors = bmi->bmiHeader.biClrUsed;
  else if ( bmi->bmiHeader.biBitCount <= 8 )
    ncolors = 1<<bmi->bmiHeader.biBitCount;

  if ( ncolors )
  { DisplayObj d = CurrentDisplay(NIL);
    RGBQUAD *colours = (RGBQUAD *)((LPSTR)bmi + (WORD)(bmi->bmiHeader.biSize));
    int i;

    for(i=0; i<ncolors; i++, colours++)
    { char xcolorname[8];
      Colour c;

      sprintf(xcolorname, "#%02x%02x%02x",
	      colours->rgbRed,
	      colours->rgbGreen,
	      colours->rgbBlue);

      if ( (c = checkType(CtoKeyword(xcolorname), TypeColour, NIL)) )
	(void)getXrefObject(c, d); /* open it */
    }
  }
}


void
attach_dib_image(Image image, BITMAPINFO *bmi, BYTE *bits)
{ WsImage wsi;
  BITMAPINFOHEADER *bmih = &bmi->bmiHeader;

  register_colours(bmi);

  wsi = attach_ws_image(image);
  wsi->data     = bits;
  wsi->msw_info = bmi;

  assign(image->size, w, toInt(bmih->biWidth));
  assign(image->size, h, toInt(bmih->biHeight));
  assign(image, depth, toInt(bmih->biBitCount));
  assign(image, kind, image->depth == ONE ? NAME_bitmap : NAME_pixmap);
}


static status
ws_load_windows_bmp_file(Image image, IOSTREAM *fd)
{ BITMAPFILEHEADER bmfh;
  BITMAPINFO *bmi;
  BITMAPINFOHEADER *bmih;
  BYTE *aBitmapBits;
  int databytes;
  long pos = Stell(fd);

  if ( Sfread(&bmfh, sizeof(bmfh), 1, fd) != 1 ||
       bmfh.bfType != BM ||
       !(bmi=read_bitmap_info(image, fd)) )
  { Sseek(fd, pos, SIO_SEEK_SET);
    fail;				/* not a MS-Windows .bmp file */
  }
  databytes = bmfh.bfSize - bmfh.bfOffBits;
  bmih = &bmi->bmiHeader;
  DEBUG(NAME_image,
	Cprintf("%dx%dx%d image; %d data bytes\n",
		bmih->biWidth, bmih->biHeight, bmih->biBitCount, databytes));
  aBitmapBits = pceMalloc(databytes);
  if ( Sfread(aBitmapBits, sizeof(BYTE), databytes, fd) != databytes )
  { pceFree(bmi);
    pceFree(aBitmapBits);

    return checkErrorSourceSink(image->file, fd);
  }

  attach_dib_image(image, bmi, aBitmapBits);
  succeed;
}


#define LOAD_ICO_FROM_STREAM 1

#ifdef LOAD_ICO_FROM_STREAM

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Read the ICO myself from the file. This  work is based on the Win32 SDK.
Note that for reading the ICONDIR header,   the  file is 16-bit aligned,
while the structure in memory is 32-bit aligned.  Hence, we read 3 times
sizeof(WORD).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define offsetof(type, field) (int)(&((type *)NULL)->field)

typedef struct
{ BYTE	bWidth;               /* Width of the image */
  BYTE	bHeight;              /* Height of the image (times 2) */
  BYTE	bColorCount;          /* Number of colors in image (0 if >=8bpp) */
  BYTE	bReserved;            /* Reserved */
  WORD	wPlanes;              /* Color Planes */
  WORD	wBitCount;            /* Bits per pixel */
  DWORD	dwBytesInRes;         /* how many bytes in this resource? */
  DWORD	dwImageOffset;        /* where in the file is this image */
} ICONDIRENTRY, *LPICONDIRENTRY;

typedef struct
{ WORD  	idReserved;   /* Reserved */
  WORD  	idType;       /* resource type (1 for icons) */
  WORD  	idCount;      /* how many images? */
  ICONDIRENTRY	idEntries[1]; /* the entries for each image */
} ICONDIR, *LPICONDIR;

typedef struct
{ UINT		Width, Height, Colors; /* Width, Height and bpp */
  LPBYTE	lpBits;                /* ptr to DIB bits */
  DWORD		dwNumBytes;            /* how many bytes? */
  LPBITMAPINFO	lpbi;                  /* ptr to header */
  LPBYTE	lpXOR;                 /* ptr to XOR image bits */
  LPBYTE	lpAND;                 /* ptr to AND image bits */
} ICONIMAGE, *LPICONIMAGE;

#endif /*LOAD_ICO_FROM_STREAM*/

static status
ws_load_windows_ico_file(Image image, IOSTREAM *fd)
{ HICON hi;

#ifdef LOAD_ICO_FROM_STREAM
  long start = Stell(fd);
  int hdr_size = 3 * sizeof(WORD);
  ICONDIR *dir = pceMalloc(hdr_size);
  ICONIMAGE *iimg = NULL;
  int id;
  long res_offset, res_size;

  if ( Sfread(dir, 1, hdr_size, fd) != hdr_size )
  { error:
    Sseek(fd, start, SIO_SEEK_SET);
    if ( dir )
      pceFree(dir);
    if ( iimg )
      pceFree(iimg);

    return FALSE;
  }
  if ( dir->idType != 1 &&
       dir->idType != 2 )
    goto error;				/* bad type */

  DEBUG(NAME_icon,
	Cprintf("Type = %d, count = %d\n", dir->idType, dir->idCount));
  if ( dir->idCount < 1 || dir->idCount > 100 )	/* doesn't make sense */
  { DEBUG(NAME_icon, Cprintf("idCount = %d???\n", dir->idCount));
    goto error;
  }

  dir = pceRealloc(dir, offsetof(ICONDIR, idEntries[dir->idCount]));
  if ( Sfread(&dir->idEntries[0], sizeof(ICONDIRENTRY), dir->idCount, fd)
							!= (dir->idCount) )
  { DEBUG(NAME_icon,
	  Cprintf("Failed to read %d directory entries\n", dir->idCount));
    goto error;
  }

  id = LookupIconIdFromDirectory((LPBYTE)dir, TRUE);
  if ( id < 0 || id >= dir->idCount )
  { id = LookupIconIdFromDirectory((LPBYTE)dir, FALSE);
    if ( id < 0 || id >= dir->idCount )
      goto error;
  }

  res_offset = dir->idEntries[id].dwImageOffset;
  res_size   = dir->idEntries[id].dwBytesInRes;
  if ( res_size < 0 || res_size > 100000 )
    goto error;

  DEBUG(NAME_icon,
	Cprintf("At id=%d, offset=%ld, size=%d\n",
		id, res_offset, res_size));

  pceFree(dir);
  dir = NULL;

  iimg = pceMalloc(res_size);
  Sseek(fd, res_offset, SIO_SEEK_SET);
  if ( Sfread(iimg, 1, res_size, fd) != res_size )
    goto error;

  hi=CreateIconFromResource((LPBYTE)iimg, res_size, TRUE, 0x00030000);

#else /*LOAD_ICO_FROM_STREAM*/
  char *fname;

  if ( instanceOfObject(image->file, ClassFile) )
  { FileObj file = (FileObj) image->file;

    fname = strName(getOsNameFile(file));
  } else
  { Cprintf("Cannot (yet) load .ICO image from %s", pp(image->file));
    fail;
  }

  hi=(HICON)LoadCursorFromFile(fname);

#endif /*LOAD_ICO_FROM_STREAM*/

  if ( hi )
  { ICONINFO info;

    if ( GetIconInfo(hi, &info) )
    { BITMAP bm;
      int destroyicon = TRUE;
      int iscolor = TRUE;

      if ( isNil(image->display) )
	assign(image, display, CurrentDisplay(NIL));

      if ( !info.fIcon )
	assign(image, hot_spot, newObject(ClassPoint,
					  toInt(info.xHotspot),
					  toInt(info.yHotspot), EAV));

      if ( info.hbmColor && GetObject(info.hbmColor, sizeof(bm), &bm) )
      { HBITMAP copy = CopyImage(info.hbmColor, IMAGE_BITMAP,
				 0, 0, LR_COPYRETURNORG);
	if ( !copy )
	{ copy = info.hbmColor;
	  destroyicon = FALSE;
	}
	assign(image->size, w, toInt(bm.bmWidth));
	assign(image->size, h, toInt(bm.bmHeight));
	assign(image,       depth, toInt(bm.bmPlanes * bm.bmBitsPixel));
	assign(image,	    kind,  image->depth == ONE ? NAME_bitmap
	       					       : NAME_pixmap);

	registerXrefObject(image, image->display, copy);
      } else
	iscolor = FALSE;

      if ( GetObject(info.hbmMask, sizeof(bm), &bm) )
      { Image mask;
	HBITMAP copy;

	if ( !iscolor )			/* Upper half is mask, lower image */
	{ HDC dhdc = CreateCompatibleDC(NULL);
	  HDC shdc = CreateCompatibleDC(dhdc);
	  HBITMAP bimg = ZCreateBitmap(bm.bmWidth, bm.bmHeight/2, 1, 1, NULL);
	  HBITMAP bmsk = ZCreateBitmap(bm.bmWidth, bm.bmHeight/2, 1, 1, NULL);
	  HBITMAP dobm, sobm;
	  int w = bm.bmWidth;
	  int h = bm.bmHeight/2;

	  assign(image, kind, NAME_bitmap);
	  assign(image, depth, ONE);
	  assign(image->size, w, toInt(w));
	  assign(image->size, h, toInt(h));

	  sobm = ZSelectObject(shdc, info.hbmMask); /* source */
	  dobm = ZSelectObject(dhdc, bimg);         /* dest */
	  BitBlt(dhdc, 0, 0, w, h, shdc, 0, h, SRCCOPY);
	  ZSelectObject(dhdc, bmsk);
	  BitBlt(dhdc, 0, 0, w, h, shdc, 0, 0, SRCCOPY);
	  ZSelectObject(dhdc, dobm);
	  ZSelectObject(shdc, sobm);
	  DeleteDC(shdc);
	  DeleteDC(dhdc);

	  assign(image, mask, newObject(ClassImage, NIL,
					image->size->w, image->size->h,
					NAME_bitmap, EAV));

	  registerXrefObject(image, image->display, bimg);
	  registerXrefObject(image->mask, image->display, bmsk);

	  DestroyIcon(hi);
#ifdef LOAD_ICO_FROM_STREAM
	  pceFree(iimg);
#endif

	  succeed;
	}

	copy = CopyImage(info.hbmMask, IMAGE_BITMAP,
				 0, 0, LR_COPYRETURNORG);
	if ( !copy )
	{ copy = info.hbmMask;
	  destroyicon = FALSE;
	}

	assign(image, mask, newObject(ClassImage, NIL,
				      image->size->w, image->size->h,
				      NAME_bitmap, EAV));
	mask = image->mask;

	assign(mask->size, w, toInt(bm.bmWidth));
	assign(mask->size, h, toInt(bm.bmHeight));
	assign(mask,       depth, toInt(bm.bmPlanes * bm.bmBitsPixel));
	assign(mask,	   kind,  image->depth == ONE ? NAME_bitmap
	       					       : NAME_pixmap);

	registerXrefObject(mask, image->display, copy);
      }

      if ( destroyicon )
	DestroyIcon(hi);
      else
      { static int warned = FALSE;

	if ( !warned++ )
	  Cprintf("Warning: could not copy icon images\n");
      }

#ifdef LOAD_ICO_FROM_STREAM
      pceFree(iimg);
#endif
      succeed;
    }
  }

#ifdef LOAD_ICO_FROM_STREAM
  pceFree(iimg);
#endif
  fail;
}


#ifdef O_XPM
#ifdef HAVE_LIBJPEG
#include <img/jpeg.h>
#endif
#ifdef O_GIF
#include <img/gif.h>
#endif

static int
readXpmImage(IOSTREAM *fd, Image image, XpmImage *img, XpmInfo *info)
{ int rval;
  int size;
  long offset = Stell(fd);

#ifdef O_GIF
  if ( (rval=XpmReadGIF(fd, img)) == XpmSuccess )
    return XpmSuccess;
#endif

  if ( (size = Ssize(fd)) > 0 )
  { int malloced;
    char *buffer;

    if ( size < 10000 )
    { buffer = (char *)alloca(size+1);
      malloced = FALSE;
    } else
    { buffer = pceMalloc(size+1);
      malloced = TRUE;
    }

    if ( Sfread(buffer, sizeof(char), size, fd) != size )
    { if ( malloced )
	pceFree(buffer);
      Sclose(fd);
      return XpmOpenFailed;
    }

    buffer[size] = '\0';
    rval = XpmCreateXpmImageFromBuffer(buffer, img, info);
    if ( rval != XpmSuccess )
      Sseek(fd, offset, 0);
    if ( malloced )
      pceFree(buffer);
  } else
  { rval = XpmFileInvalid;
  }

  return rval;
}


status
ws_create_image_from_xpm_data(Image image, char **data, DisplayObj d)
{ XpmImage img;
  XpmInfo info;

  assign(image, display, d);

  XpmCreateXpmImageFromData(data, &img, &info);
  ws_attach_xpm_image(image, &img, &info);
  XpmFreeXpmImage(&img);

  succeed;
}


static status
ws_attach_xpm_image(Image image, XpmImage* xpmimg, XpmInfo* xpminfo)
{ XImage *img, *shape;
  HDC hdc;
  DisplayObj d = image->display;
  int as = XpmAttributesSize();
  XpmAttributes *atts = (XpmAttributes *)alloca(as);
  int rval;

  if ( isNil(d) )			/* fix the display reference */
  { d = CurrentDisplay(image);
    assign(image, display, d);
  }
  openDisplay(d);

  memset(atts, 0, as);

					/* Step 1: hot-stop handling */
  if ( xpminfo->valuemask & XpmHotspot )
  { assign(image, hot_spot, newObject(ClassPoint,
				      toInt(xpminfo->x_hotspot),
				      toInt(xpminfo->y_hotspot), EAV));
  } else
  { assign(image, hot_spot, NIL);
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Second step. Analyse the colours in the   info  section, and add them to
the display's colour map.  In  the  future,   this  should  use  a local
colourmap, and the bitmap should be converted into a DIB. I've done part
of that, and left the code for later.  This is the XPMTODIB.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#undef XPMTODIB				/* make sure ... */

#ifndef XPMTODIB
  if ( (xpminfo->valuemask & XpmReturnColorTable) &&
       instanceOfObject(d->colour_map, ClassColourMap) )
  { XpmColor *xpmc = xpmimg->colorTable;
    int n;

    for(n=0; n<xpmimg->ncolors; n++, xpmc++)
    { Colour c;

      if ( streq_ignore_case(xpmc->c_color, "none") )
	continue;			/* the transparent colour */

      if ( (c = checkType(CtoKeyword(xpmc->c_color), TypeColour, NIL)) )
	(void)getXrefObject(c, d); /* open it */
    }

    if ( (atts->colormap = getPaletteColourMap(d->colour_map)) )
      atts->valuemask |= XpmColormap;
  }

#else /*XPMTODIB*/

  if ( xpminfo->valuemask & XpmReturnColorTable )
  { LOGPALETTE *lp;
    PALETTEENTRY *pe;
    XpmColor *xpmc = xpmimg->colorTable;
    int n;
    int pentries=0;

    lp = pceMalloc(offset(LOGPALETTE, palPalEntry[xpmimg->ncolors]));
    lp->palVersion    = 0x300;
    pe                = &lp->palPalEntry[0];

    for(n=0; n<xpmimg.ncolors; n++, xpmc++)
    { Colour c;

      if ( streq_ignore_case(xpmc->c_color, "none") )
	continue;			/* the transparent colour */

      if ( (c = checkType(CtoKeyword(xpmc->c_color), TypeColour, NIL)) )
      { COLORREF rgb = (COLORREF)getXrefObject(c, d);

	pe->peRed   = valInt(c->red)   >> 8;
	pe->peGreen = valInt(c->green) >> 8;
	pe->peBlue  = valInt(c->blue)  >> 8;
	pe->peFlags = 0;
	pentries++;
	pe++;
      }
    }
    lp->palNumEntries = pentries;

    if ( !(hpal = CreatePalette(lp)) )
      Cprintf("%s: failed to create colour palette\n", pp(image));

    pceFree(lp);
  }
#endif /*XPMTODIB*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Final step. Convert the XpmImage into a Windows bitmap. First select our
palette, so we can be sure the  proper   colours  will be in the devices
palette.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  hdc = GetDC(NULL);
  switch((rval=XpmCreateImageFromXpmImage(&hdc, xpmimg, &img, &shape, atts)))
  { case XpmNoMemory:
      return sysPce("Not enough memory");
    case XpmSuccess:
      break;
    default:
      return errorPce(image, NAME_unknownError, toInt(rval));
  }
  ReleaseDC(NULL, hdc);

#ifdef XPMTODIB
  if ( hpal )
    DeleteObject(hpal);
#endif

  assign(image, kind, img->depth == 1 ? NAME_bitmap : NAME_pixmap);
  assign(image->size, w, toInt(img->width));
  assign(image->size, h, toInt(img->height));
  assign(image, depth, toInt(img->depth));
  registerXrefObject(image, d, img->bitmap);
  XImageFree(img);

  if ( shape )
  { assign(image, mask, newObject(ClassImage, NIL,
				  toInt(shape->width),
				  toInt(shape->height),
				  NAME_bitmap, EAV));
    registerXrefObject(image->mask, d, shape->bitmap);
    XImageFree(shape);
  }

  succeed;
}
#endif


status
ws_load_image_file(Image image)
{ status rval = FAIL;
  IOSTREAM *fd;

  if ( !(fd = Sopen_object(image->file, "rbr")) )
    return errorPce(image->file, NAME_openFile,
		    NAME_read, getOsErrorPce(PCE));

  if ( read_jpeg_file(fd, image) )
  { rval = SUCCEED;
    goto out;
  }

#ifdef O_XPM
{ int xpmrval;
  XpmImage xpmimg;
  XpmInfo  xpminfo;

  memset(&xpmimg,  0, sizeof(xpmimg));
  memset(&xpminfo, 0, sizeof(xpminfo));

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
First step. Read the file  into  an   XpmImage,  so  we  can extract and
register the colours.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  xpminfo.valuemask = (XpmColorTable|XpmReturnColorTable);
  xpmrval = readXpmImage(fd, image, &xpmimg, &xpminfo);
  switch(xpmrval)
  { case XpmFileInvalid:
      break;
    case XpmNoMemory:
      Sclose(fd);
      return sysPce("Not enough memory");
    case XpmSuccess:
      Sclose(fd);
      rval = ws_attach_xpm_image(image, &xpmimg, &xpminfo);
      XpmFreeXpmImage(&xpmimg);
      return rval;
    default:
      Sclose(fd);
      return errorPce(image, NAME_unknownError, toInt(xpmrval));
  }
}
#endif /*O_XPM*/

  { int w, h;
    unsigned char *data;

    if ( (data = read_bitmap_data(fd, &w, &h)) )
    { ws_create_image_from_x11_data(image, data, w, h);
      pceFree(data);
      rval = SUCCEED;
    } else if ( ws_load_windows_bmp_file(image, fd) )
    { rval = SUCCEED;
    } else if ( ws_load_windows_ico_file(image, fd) )
    { rval = SUCCEED;
    } else
      rval = loadPNMImage(image, fd);
  }

out:
  Sclose(fd);

  return rval;
}


static FileObj
mustBeFile(SourceSink ss)
{ if ( instanceOfObject(ss, ClassFile) )
    return (FileObj)ss;

  errorPce(ss, NAME_unexpectedType, nameToType(NAME_file));
  fail;
}


status
ws_save_image_file(Image image, SourceSink into, Name fmt)
{ DisplayObj d = image->display;

  if ( isNil(d) )
    d = CurrentDisplay(image);

  if ( fmt == NAME_xbm )
  { return errorPce(image, NAME_noImageFormat, NAME_xbm);
  } else if ( fmt == NAME_xpm )
  {
#ifdef O_XPM
    int as = XpmAttributesSize();
    XpmAttributes *atts = (XpmAttributes *)alloca(as);
    char *fname;
    DisplayObj d = image->display;
    HPALETTE ohpal, hpal;
    XImage ximg, xmsk, *xmskp = NULL;
    HDC hdc;
    int rval;
    FileObj file;

    if ( !(file=mustBeFile(into)) )
      fail;
    fname = strName(getOsNameFile(file));

    if ( isNil(d) )
      d = CurrentDisplay(image);
    if ( instanceOfObject(d->colour_map, ClassColourMap) )
      hpal = getPaletteColourMap(d->colour_map);
    else
      hpal = NULL;

    hdc = CreateCompatibleDC(NULL);
    if ( hpal )
    { ohpal = SelectPalette(hdc, hpal, FALSE);
      RealizePalette(hdc);
    } else
      ohpal = NULL;

    memset(atts, 0, as);
    ximg.width  = atts->width  = valInt(image->size->w);
    ximg.height = atts->height = valInt(image->size->h);
    ximg.depth  = valInt(image->depth);
    ximg.bitmap = (HBITMAP) getXrefObject(image, d);
    atts->valuemask = XpmSize;

    if ( notNil(image->hot_spot) )
    { atts->x_hotspot = valInt(image->hot_spot->x);
      atts->y_hotspot = valInt(image->hot_spot->y);
      atts->valuemask |= XpmHotspot;
    }
    if ( notNil(image->mask) )
    { Image mask = image->mask;

      xmsk.width  = valInt(mask->size->w);
      xmsk.height = valInt(mask->size->h);
      xmsk.depth  = valInt(mask->depth);
      xmsk.bitmap = (HBITMAP) getXrefObject(mask, d);
      xmskp = &xmsk;
    }

    rval = XpmWriteFileFromImage(&hdc,
				 fname,
				 &ximg,
				 xmskp,
				 atts);
    if ( hpal )
      SelectPalette(hdc, ohpal, FALSE);
    DeleteDC(hdc);

    if ( rval != XpmSuccess )
      return errorPce(image, NAME_xError);

    succeed;
#else
    return errorPce(image, NAME_noImageFormat, NAME_xpm);
#endif
  } else if ( fmt == NAME_jpeg )
  {
#ifdef HAVE_LIBJPEG
    HBITMAP bm;
    IOSTREAM *fd;
    status rval;

    if ( !(bm = getXrefObject(image, d)) )
      fail;

    if ( (fd = Sopen_object(into, "wbr")) )
    { if ( write_jpeg_file(fd, image, bm) < 0 )
	rval = errorPce(image, NAME_xError);
      else
	rval = SUCCEED;

      Sclose(fd);
      return rval;
    }

    fail;
#else
    return errorPce(image, NAME_noImageFormat, NAME_jpeg);
#endif
  } else if ( fmt == NAME_gif )
  {
#ifdef O_GIFWRITE
    HBITMAP bm, msk = NULL;
    IOSTREAM *fd;
    status rval;

    if ( !(bm = getXrefObject(image, d)) )
      fail;
    if ( notNil(image->mask) )
      msk = getXrefObject(image->mask, d);

    if ( (fd = Sopen_object(into, "wbr")) )
    { if ( write_gif_file(fd, image, bm, msk) < 0 )
	rval = errorPce(image, NAME_xError);
      else
	rval = SUCCEED;

      Sclose(fd);
      return rval;
    }

    fail;
#else
    return errorPce(image, NAME_noImageFormat, NAME_gif);
#endif
  } else
  { int pnm_fmt;
    HBITMAP bm;
    status rval;

    if ( fmt == NAME_pnm )	pnm_fmt = PNM_PNM;
    else if ( fmt == NAME_pbm )	pnm_fmt = PNM_PBM;
    else if ( fmt == NAME_pgm )	pnm_fmt = PNM_PGM;
    else if ( fmt == NAME_ppm )	pnm_fmt = PNM_PPM;
    else fail;

    if ( (bm = getXrefObject(image, d)) )
    { IOSTREAM *fd;

      if ( (fd = Sopen_object(into, "wbr")) )
      { if ( write_pnm_file(fd, bm, 0, 0, PNM_RAWBITS) < 0 )
	  rval = errorPce(image, NAME_xError);
	else
	  rval = SUCCEED;
      } else
	rval = FAIL;

      Sclose(fd);
    } else
      rval = FAIL;

    return rval;
  }
  assert(0);
  fail;
}


static HBITMAP
windows_bitmap_from_dib(Image image)
{ WsImage wsi;

  if ( (wsi=image->ws_ref) && wsi->msw_info )
  { HDC hdc;
    HBITMAP bm;

    if ( image->kind == NAME_bitmap )
    { int w = valInt(image->size->w);
      int h = valInt(image->size->h);
      HBITMAP obm;

      bm = ZCreateBitmap(w, h, 1, 1, NULL);
      hdc = CreateCompatibleDC(NULL);
      obm = ZSelectObject(hdc, bm);
      if ( StretchDIBits(hdc, 0, 0, w, h, 0, 0, w, h,
			 wsi->data, wsi->msw_info,
			 DIB_RGB_COLORS, SRCCOPY) == GDI_ERROR )
	Cprintf("StretchDIBits() failed");
      ZSelectObject(hdc, obm);
      DeleteDC(hdc);
    } else
    { HPALETTE ohpal, hpal;
      DisplayObj d = image->display;

      if ( isNil(d) )
	d = CurrentDisplay(image);
      if ( instanceOfObject(d->colour_map, ClassColourMap) )
	hpal = getPaletteColourMap(d->colour_map);
      else
	hpal = NULL;

      hdc = GetDC(NULL);
      if ( hpal )
      { ohpal = SelectPalette(hdc, hpal, FALSE);
	RealizePalette(hdc);
      } else
	ohpal = NULL;

      bm = ZCreateDIBitmap(hdc,
			   (LPBITMAPINFOHEADER) wsi->msw_info,
			   CBM_INIT,
			   wsi->data,
			   (LPBITMAPINFO) wsi->msw_info,
			   DIB_RGB_COLORS);
      assign(image, depth, toInt(GetDeviceCaps(hdc, BITSPIXEL)));
      assign(image, kind, image->depth == ONE ? NAME_bitmap : NAME_pixmap);

      if ( hpal )
	SelectPalette(hdc, ohpal, FALSE);
      ReleaseDC(NULL, hdc);

      if ( !bm )
	Cprintf("ZCreateDIBitmap(): %s\n",
		strName(WinStrError(GetLastError())));
    }

    return bm;
  }

  return NULL;
}


static HBITMAP
windows_bitmap_from_bits(Image image)
{ WsImage r;

  if ( (r=image->ws_ref) )
  { HBITMAP bm;

    if ( r->msw_info )
    { bm = windows_bitmap_from_dib(image);
    } else if ( r->data )
    { bm = ZCreateBitmap(r->w, r->h, 1, 1, NULL);
      SetBitmapBits(bm, ws_sizeof_bits(r->w, r->h), r->data);
    } else
      bm = 0;

    return bm;
  }

  return 0;
}


status
ws_open_image(Image image, DisplayObj d)
{ HBITMAP bm;
  HBRUSH brush = 0;
  int w = valInt(image->size->w);
  int h = valInt(image->size->h);
  WsImage r;

  assign(image, display, d);

  if ( (r=image->ws_ref) && r->data )
  { bm = windows_bitmap_from_bits(image);
    if ( bm )
    { registerXrefObject(image, d, (void *) bm);

      succeed;
    }

    fail;
  }

  if ( notNil(image->file) )
  { if ( loadImage(image, DEFAULT, DEFAULT) &&
	 (r=image->ws_ref) && r->data &&
	 (bm = windows_bitmap_from_bits(image)) )
    { registerXrefObject(image, d, (void *) bm);
      succeed;
    }

    fail;
  }

  if ( image->access == NAME_read )
    brush = standardWindowsBrush(image);

  if ( w != 0 && h != 0 &&
       (image->access == NAME_both || brush) )
  { assign(image, display, d);

    if ( image->kind == NAME_pixmap )
    { HDC hdc = GetDC(NULL);
      BITMAP bitmap;

      if ( isDefault(image->background) )
	assign(image, background, d->background);
      if ( isDefault(image->foreground) )
	assign(image, foreground, d->foreground);

      bm = ZCreateCompatibleBitmap(hdc, w, h);
      GetObject(bm, sizeof(BITMAP), &bitmap);
      assign(image, depth, toInt(bitmap.bmPlanes * bitmap.bmBitsPixel));
      ReleaseDC(NULL, hdc);
    } else
    { assign(image, depth, ONE);
      bm = ZCreateBitmap(w, h, 1, 1, NULL);
    }

    if ( bm )
    { registerXrefObject(image, d, (void *) bm);
      d_image(image, 0, 0, w, h);
      if ( brush )
      { r_clear(0, 0, w, h);		/* clear to deal with hollow, etc */
	r_fill(0, 0, w, h, image);	/* @win_..._brush */
      } else
	r_clear(0, 0, w, h);
      d_done();

      succeed;
    }
  }

  fail;
}


void
ws_close_image(Image image, DisplayObj d)
{ Xref r;

  while( (r = unregisterXrefObject(image, d)) )
    ZDeleteObject((HBITMAP) r->xref);
}


status
ws_resize_image(Image image, Int w, Int h)
{ if ( notNil(image->display) )
  { DisplayObj d = image->display;
    HBITMAP sbm = (HBITMAP) getExistingXrefObject(image, d);

    if ( sbm )
    { if ( w == ZERO || h == ZERO )
      { XcloseImage(image, d);
      } else
      { HDC hdcsrc = CreateCompatibleDC(NULL);
	HDC hdcdst = CreateCompatibleDC(hdcsrc);
	HBITMAP osbm = ZSelectObject(hdcsrc, sbm);
	HBITMAP  dbm = ZCreateCompatibleBitmap(hdcsrc, valInt(w), valInt(h));
	HBITMAP odbm = ZSelectObject(hdcdst, dbm);
	int minw = min(valInt(w), valInt(image->size->w));
	int minh = min(valInt(h), valInt(image->size->h));

	BitBlt(hdcdst, 0, 0, minw, minh, hdcsrc, 0, 0, SRCCOPY);

	ZSelectObject(hdcsrc, osbm);
	ZSelectObject(hdcdst, odbm);
	DeleteDC(hdcsrc);
	DeleteDC(hdcdst);

	XcloseImage(image, d);
	registerXrefObject(image, d, (void *) dbm);
      }
    }
  }

  return setSize(image->size, w, h);
}


#define USE_MS_ZOOM 1

#ifdef USE_MS_ZOOM

Image
ws_scale_image(Image image, int w, int h)
{ Image copy = answerObject(ClassImage, NIL,
			    toInt(w), toInt(h), image->kind, EAV);
  DisplayObj d = image->display;

  if ( isNil(d) )
    d = CurrentDisplay(image);

  if ( copy && d )
  { HBITMAP sbm = (HBITMAP) getXrefObject(image, d);

    if ( sbm )
    { HPALETTE ohpalsrc=0, ohpaldst=0, hpal;
      HDC hdcsrc   = CreateCompatibleDC(NULL);
      HDC hdcdst   = CreateCompatibleDC(hdcsrc);
      HBITMAP osbm, dbm, odbm;

      if ( instanceOfObject(d->colour_map, ClassColourMap) )
	hpal = getPaletteColourMap(d->colour_map);
      else
	hpal = NULL;

      if ( hpal )
      { ohpalsrc = SelectPalette(hdcsrc, hpal, FALSE);
	RealizePalette(hdcsrc);
	ohpaldst = SelectPalette(hdcdst, hpal, FALSE);
	RealizePalette(hdcdst);
      }

      osbm = ZSelectObject(hdcsrc, sbm);
       dbm = ZCreateCompatibleBitmap(hdcsrc, w, h);
      odbm = ZSelectObject(hdcdst, dbm);

      StretchBlt(hdcdst,
		 0, 0, w, h,		/* dest rectangle */
		 hdcsrc,
		 0, 0, valInt(image->size->w), valInt(image->size->h),
		 SRCCOPY);

      ZSelectObject(hdcsrc, osbm);
      ZSelectObject(hdcdst, odbm);

      if ( ohpalsrc )
	SelectPalette(hdcsrc, ohpalsrc, FALSE);
      if ( ohpaldst )
	SelectPalette(hdcdst, ohpaldst, FALSE);

      DeleteDC(hdcsrc);
      DeleteDC(hdcdst);

      registerXrefObject(copy, d, (void *) dbm);
    }
  }

  answer(copy);
}

#else /*USE_MS_ZOOM*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Using BitBlt() based zoom looks simple, but  in fact produces really bad
results. The below algorithm is ported   from ../x11/ximage.c, where the
original source is documented. It  is  not   perfect,  but  surely a lot
better.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static unsigned int *
buildIndex(unsigned width, unsigned rwidth)
{ float         fzoom;
  int		 zoom;
  unsigned int *index;
  unsigned int  a;


  if ( width == rwidth )
  { zoom  = FALSE;
    fzoom = 100.0;
  } else
  { zoom  = TRUE;
    fzoom = (float)rwidth / (float) width;
  }

  index = pceMalloc(sizeof(unsigned int) * rwidth);

  if ( zoom )
  { for(a=0; a < rwidth; a++)
      index[a] = rfloat((float)a/fzoom);
  } else
  { for(a=0; a < rwidth; a++)
      index[a] = a;
  }

  return index;
}


Image
ws_scale_image(Image image, int w, int h)
{ Image copy = answerObject(ClassImage, NIL,
			    toInt(w), toInt(h), image->kind, EAV);
  DisplayObj d = image->display;

  if ( isNil(d) )
    d = CurrentDisplay(image);

  if ( copy && d )
  { HBITMAP sbm = (HBITMAP) getXrefObject(image, d);

    if ( sbm )
    { HPALETTE ohpalsrc=0, ohpaldst=0, hpal;
      HDC hdcsrc, hdcdst;
      HBITMAP osbm, dbm, odbm;

      if ( instanceOfObject(d->colour_map, ClassColourMap) )
	hpal = getPaletteColourMap(d->colour_map);
      else
	hpal = NULL;

      hdcsrc = CreateCompatibleDC(NULL);
      osbm   = ZSelectObject(hdcsrc, sbm);
      if ( hpal )
      { ohpalsrc = SelectPalette(hdcsrc, hpal, FALSE);
	RealizePalette(hdcsrc);
      }
      hdcdst = CreateCompatibleDC(hdcsrc);
      dbm    = ZCreateCompatibleBitmap(hdcsrc, w, h);
      odbm   = ZSelectObject(hdcdst, dbm);
      if ( hpal )
      { ohpaldst = SelectPalette(hdcdst, hpal, FALSE);
	RealizePalette(hdcdst);
      }

      { unsigned int *xindex, *yindex;
	unsigned int  x, y, xsrc, ysrc;

	xindex = buildIndex(valInt(image->size->w),  w);
	yindex = buildIndex(valInt(image->size->h), h);

	for(y = 0; y < (unsigned int) h; y++)
	{ ysrc = yindex[y];

	  for(x= 0; x < (unsigned int)w; x++)
	  { COLORREF value;
	    xsrc = xindex[x];

	    value = GetPixel(hdcsrc, xsrc, ysrc);
	    if ( hpal )
	    { int r = GetRValue(value);
	      int g = GetGValue(value);
	      int b = GetBValue(value);

	      value = PALETTERGB(r, g, b);
	      /* value |= 0x02000000; is the same, but there is no
	         significant improvement, so lets keep it portable
	       */
	    }
	    SetPixel(hdcdst, x, y, value);
	  }
	}

        pceFree(xindex);
	pceFree(yindex);
      }

      ZSelectObject(hdcsrc, osbm);
      ZSelectObject(hdcdst, odbm);

      if ( ohpalsrc )
	SelectPalette(hdcsrc, ohpalsrc, FALSE);
      if ( ohpaldst )
	SelectPalette(hdcdst, ohpaldst, FALSE);

      DeleteDC(hdcsrc);
      DeleteDC(hdcdst);

      registerXrefObject(copy, d, (void *) dbm);
    }
  }

  answer(copy);
}

#endif /*USE_MS_ZOOM*/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
XFORM is a transformation matrix of the format:

	( eM11, eM21, 0 )	(  a   b  0 )
	( eM12, eM22, 0 )   =	(  c   d  0 )
	( eDx,  eDy,  1 )       ( Tx  Ty  1 )

This realises the transformation:

	Xdev = eM11*x + eM12*y + eDx
	Ydev = eM21*x + eM22*y + eDy

		     =

	Xdev = ax + cy + Tx
	Ydev = bx + dy + Ty

The current implementation of copy_bits() is   incorrect. It assumes the
transformation  represented  is  the  combination   of  a  rotation  and
translation, instead of being a general  matrix inversion. Also, it uses
the fact that M$  XFORM  represents   clockwise  rotation,  while  we do
anti-clockwise rotation.

The copy_bits() gives much better  results   then  the  Windows-NT (4.0)
world transform and is thus both on Windows 95 and NT.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
copy_bits(HDC dst, int dx0, int dy0, int dx1, int dy1,
	  HDC src, int sx0, int sy0,
	  XFORM *xform)
{ int dx, dy;
  float a = xform->eM11;
  float b = xform->eM21;
  float c = xform->eM12;
  float d = xform->eM22;
  int  Tx = rfloat(xform->eDx);
  int  Ty = rfloat(xform->eDy);

  for(dy = dy0; dy < dy1; dy++)
  { for(dx = dx0; dx < dx1; dx++)
    { int sx, sy;
      COLORREF pxl;

      sx = rfloat(a*(float)(dx-Tx) + c*(float)(dy-Ty)) + sx0;
      sy = rfloat(b*(float)(dx-Tx) + d*(float)(dy-Ty)) + sy0;

      pxl = GetPixel(src, sx, sy);
      if ( pxl != CLR_INVALID )
	SetPixel(dst, dx, dy, pxl);
    }
  }
}


#define falmost(f1, f2) (fabs((f1)-(f2)) < 0.001)

Image
ws_rotate_image(Image image, float a)	/* 0.0<angle<360.0 */
{ int ow = valInt(image->size->w);
  int oh = valInt(image->size->h);
  int w, h;
  float angle = (a * M_PI)/180.0;
  float sina, cosa;
  int rot90;				/* rotation by 0,90,180,270 */
  Image copy;				/* rotated image */
  DisplayObj d;

  if ( falmost(angle, M_PI/2) )		/* 90 degrees */
  { w = oh;
    h = ow;
    sina = 1.0;
    cosa = 0.0;
    rot90 = TRUE;
  } else if ( falmost(angle, M_PI) )	/* 180 degrees */
  { w = ow;
    h = oh;
    cosa = -1.0;
    sina = 0.0;
    rot90 = TRUE;
  } else if ( falmost(angle, 3*M_PI/2) ) /* 270 degrees */
  { w = oh;
    h = ow;
    sina = -1.0;
    cosa = 0.0;
    rot90 = TRUE;
  } else
  { rot90 = FALSE;
    sina = sin(angle);
    cosa = cos(angle);

    w = fabs((float)oh*sina) + fabs((float)ow*cosa) + 0.99999;
    h = fabs((float)oh*cosa) + fabs((float)ow*sina) + 0.99999;
  }

  copy = answerObject(ClassImage, NIL,
		      toInt(w), toInt(h), image->kind, EAV);
  d = image->display;

  if ( isNil(d) )
    d = CurrentDisplay(image);

  if ( copy && d )
  { HBITMAP sbm = (HBITMAP) getXrefObject(image, d);

    if ( sbm )
    { HDC hdcsrc   = CreateCompatibleDC(NULL);
      HDC hdcdst   = CreateCompatibleDC(hdcsrc);
      HBITMAP osbm = ZSelectObject(hdcsrc, sbm);
      HBITMAP  dbm = ZCreateCompatibleBitmap(hdcsrc, w, h);
      HBITMAP odbm = ZSelectObject(hdcdst, dbm);
      XFORM xform;
#if 0					/* see below */
      int ogm;
#endif
      float acangle = angle;		/* anti-clockwise-angle */

      angle = -angle;			/* Windows wants clockwise */

      registerXrefObject(copy, d, (void *) dbm);
      assign(copy, background, image->background);
      assign(copy, foreground, image->foreground);
      { Any bg = (isDefault(copy->background) ? d->background
					      : copy->background);
	COLORREF rgb = (COLORREF) getXrefObject(bg, d);
	HBRUSH hbrush = ZCreateSolidBrush(GetNearestColor(hdcdst, rgb));
	RECT rect;

	rect.left   = 0;
	rect.right  = w;
	rect.top    = 0;
	rect.bottom = h;

	FillRect(hdcdst, &rect, hbrush);
	ZDeleteObject(hbrush);
      }

      xform.eM11 = cos(angle);
      xform.eM12 = sin(angle);
      xform.eM21 = -xform.eM12;
      xform.eM22 = xform.eM11;
      xform.eDx  = 0;
      xform.eDy  = 0;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Determine the required translation.  Note that w = 64 means pixels 0..63,
hence the -1 applied to the w and h here.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

      if ( acangle < M_PI/2 )		/* 0<angle<90 */
      { xform.eDy = sin(acangle) * (float)(ow-1);
      } else if ( acangle < M_PI )	/* 90<angle<180 */
      { xform.eDy = h-1;
	xform.eDx = -cos(acangle) * (float)(ow-1);
      } else if ( acangle < 3*M_PI/2 )	/* 180<angle<270 */
      { xform.eDx = w-1;
	xform.eDy = h-1 + (sin(acangle) * (float)(ow-1));
      } else				/* 270<angle<360 */
      { xform.eDx = -sin(acangle) * (float)(oh-1);
      }

      DEBUG(NAME_rotate,
	    Cprintf("dx=%g, dy=%g, w = %d, h = %d\n",
		    xform.eDx, xform.eDy, w, h));

#if 0					/* use Win32 native algorithm */
      if ( (ogm = SetGraphicsMode(hdcdst, GM_ADVANCED)) &&
	   SetWorldTransform(hdcdst, &xform) )
      { BitBlt(hdcdst,
	       0, 0, w, h,		/* dest rectangle */
	       hdcsrc,
	       0, 0,
	       SRCCOPY);

	xform.eM11 = 1.0;
	xform.eM12 = 0.0;
	xform.eM21 = 0.0;
	xform.eM22 = 1.0;
	xform.eDx  = 0;
	xform.eDy  = 0;

	SetWorldTransform(hdcdst, &xform);
	SetGraphicsMode(hdcdst, ogm);
      } else
#endif
      { copy_bits(hdcdst,
		  0, 0, w, h,		/* dest rectangle */
		  hdcsrc,
		  0, 0, &xform);
      }

      ZSelectObject(hdcsrc, osbm);
      ZSelectObject(hdcdst, odbm);
      DeleteDC(hdcsrc);
      DeleteDC(hdcdst);
    }
  }

  answer(copy);
}


		 /*******************************
		 *       COLOUR --> MONO	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is rather simple, but it will do   for the moment. A `real' version
should consider the actual colours, properly  dithering the colour image
on the monochrome one.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Image
ws_monochrome_image(Image image)
{ Image mono;
  Int w = image->size->w;
  Int h = image->size->h;

  mono = answerObject(ClassImage, NIL, w, h, NAME_bitmap, EAV);
  d_image(mono, 0, 0, valInt(w), valInt(h));
  d_modify();
  r_image(image, 0, 0, 0, 0, valInt(w), valInt(h), OFF);
  d_done();

  answer(mono);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Windows  transparent  images   are   drawn    by   first   clearing  the
non-transparent pieces of the image to black  and that adding the values
from the real image. This only works if   the real image is black at the
places where it must be   transparent. ws_prepare_image_mask() is called
after a new mask has been  assigned  to   an  image  and  its task is to
blacken the transparent parts of the main image.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
ws_prepare_image_mask(Image image)
{ int w = valInt(image->size->w);
  int h = valInt(image->size->h);

  if ( notNil(image->mask) && w && h )
  { DisplayObj d = image->display;
    HBITMAP obm, bm;
    HBITMAP omsk, msk;

    if ( isNil(d) )
      d = CurrentDisplay(image);

    bm  = (HBITMAP) getXrefObject(image, d);
    msk = (HBITMAP) getXrefObject(image->mask, d);

    if ( bm && msk )
    { HDC  hdc = CreateCompatibleDC(NULL);
      HDC mhdc = CreateCompatibleDC(hdc);
      COLORREF oldbg, oldfg;

      obm  = ZSelectObject(hdc, bm);
      omsk = ZSelectObject(mhdc, msk);

      oldbg = SetBkColor(hdc, RGB(0,0,0));
      oldfg = SetTextColor(hdc, RGB(255,255,255));
      BitBlt(hdc, 0, 0, w, h, mhdc, 0, 0, SRCAND);
      SetBkColor(hdc, oldbg);
      SetTextColor(hdc, oldfg);

      ZSelectObject(hdc, obm);
      ZSelectObject(mhdc, omsk);

      DeleteDC(mhdc);
      DeleteDC(hdc);
    }
  }
}

		 /*******************************
		 *	    POSTSCRIPT		*
		 *******************************/

void
ws_postscript_image(Image image, Int depth, int iscolor)
{ int w = valInt(image->size->w);
  int h = valInt(image->size->h);
  int d = isDefault(depth) ? 0 : valInt(depth);

  d_image(image, 0, 0, w, h);
  postscriptDC(d_current_hdc(), 0, 0, w, h, d, iscolor);
/*postscriptDrawable(0, 0, w, h);*/
  d_done();
}

#undef roundup
#define roundup(v, n)		((((v)+(n)-1)/(n))*(n))
#define rescale(v, o, n)	((v) * (n) / (o))

typedef struct
{ int bits;				/* bit remaining */
  int depth;				/* postscript depth */
  int val;				/* current value */
  int count;				/* # emited bytes */
} ps_stat;


static void
put_value(ps_stat *s, int val)
{ static char print[] = { '0', '1', '2', '3', '4', '5', '6', '7',
			  '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };
  s->bits -= s->depth;
  s->val |= val << s->bits;

  if ( s->bits == 0 )
  { ps_put_char(print[(s->val >> 4) & 0xf]);
    ps_put_char(print[s->val & 0xf]);
    if ( (++s->count % 32) == 0 )
      ps_put_char('\n');
    s->bits = 8; s->val = 0;
  }
}


status
postscriptDC(HDC hdc,				/* HDC to print from */
	     int fx, int fy, int w, int h,	/* area to print */
	     int depth,				/* PostScript depth */
	     int iscolor)			/* Write color postscript */
{ int x, y, w8, psbright;
  ps_stat stat;
  int bmdepth  = GetDeviceCaps(hdc, BITSPIXEL);

  if ( depth == 0 )			/* PostScript depth is 1, 2, 4, or 8 */
  { depth = bmdepth;

    if ( depth == 3 )
      depth = 2;
    else if ( depth > 4 && depth < 8 )
      depth = 4;
    else if ( depth > 8 )
      depth = 8;
  }

  w8 = roundup(w, 8);
  psbright = (1<<depth)-1;
  stat.count = 0;
  stat.val = 0;
  stat.bits = 8;
  stat.depth = depth;

  for(y = fy; y < fy+h; y++)
  { for(x = fx; x < fx+w8; x++)
    { if ( x < fx+w )
      { COLORREF c = GetPixel(hdc, x, y);
	int r = GetRValue(c);
	int b = GetBValue(c);
	int g = GetGValue(c);

	if ( psbright != 255 )
	{ r = rescale(r, 256, psbright);
	  g = rescale(g, 256, psbright);
	  b = rescale(b, 256, psbright);
	}

	if ( iscolor )
	{ put_value(&stat, r);
	  put_value(&stat, g);
	  put_value(&stat, b);
	} else
	{ int i = (r*20 + g*32 + b*18)/(20+32+18);

	  if ( i > psbright )
	    i = psbright;
	  put_value(&stat, i);
	}
      } else
      { if ( iscolor )
	{ int i;

	  for(i=0; i < 3; i++)
	    put_value(&stat, psbright);
	} else
	  put_value(&stat, psbright);
      }
    }
  }

  succeed;
}


static int
ws_sizeof_bits(int w, int h)
{ int bytes = ((w+15)/16) * 2 * h;

  return ((bytes + 3)/4) * 4;		/* round on longs */
}


static unsigned int
mirror_byte(unsigned int b)
{ unsigned int copy = 0;
  int n;

  for(n=0; n<8; n++, b >>= 1)
  { copy <<= 1;
    if ( b & 0x01L )
      copy |= 0x01L;
  }

  return copy;
}


void
ws_create_image_from_x11_data(Image image, unsigned char *data, int w, int h)
{ WsImage r;
  unsigned short *dest;
  int y;
  int byte = 0;

  r = attach_ws_image(image);
  r->w = w;
  r->h = h;
  dest = r->data = pceMalloc(ws_sizeof_bits(w, h));

  for(y=0; y<h; y++)
  { int x;
    unsigned short s = 0;		/* make compiler happy */

    for(x=0; x<w; x+=8)
    { unsigned char b = ~mirror_byte(*data++);

      if ( ++byte == 1 )
      { s = b;
      } else
      { s |= (unsigned short) b << 8;
	*dest++ = s;
	byte = 0;
      }
    }

    if ( byte )
    { *dest++ = s;
      byte = 0;
    }
  }

  assign(image, depth, ONE);
  setSize(image->size, toInt(w), toInt(h));
}


void *
ws_image_bits_for_cursor(Image image, Name kind, int w, int h)
{ WsImage r;
  unsigned short *c, *cbits = pceMalloc(ws_sizeof_bits(w, h));
  unsigned short *d, *dbits;
  Image im = getMonochromeImage(image);
  int alloced;
  int dw, dh;
  int x, y;
  int saidpad=0;

  if ( !im )
    return NULL;

  if ( ((r = im->ws_ref) && r->data) && !r->msw_info )
  { dbits = r->data;
    alloced = 0;
    dw = r->w;
    dh = r->h;
  } else
  { HBITMAP bm;
    int bytes;

    if ( isNil(im->display) )
      assign(im, display, CurrentDisplay(NIL));

    bm = (HBITMAP) getXrefObject(im, im->display);
    dw = valInt(im->size->w);
    dh = valInt(im->size->h);
    bytes = ws_sizeof_bits(dw, dh);
    dbits = alloc(bytes);
    alloced = bytes;

    DEBUG(NAME_cursor, Cprintf("Alloced %d bytes at 0x%lx\n",
			       alloced, (long) dbits));

    if ( bytes != GetBitmapBits(bm, bytes, dbits) )
      Cprintf("GetBitmapBits() failed\n");

    DEBUG(NAME_cursor, Cprintf("Got %d bytes image from %s\n",
			       bytes, pp(im)));
  }

  for(y=0; y<h; y++)
  { c = cbits + y*((w+15)/16);
    d = dbits + y*((dw+15)/16);
    x = 0;

    DEBUG(NAME_cursor, Cprintf("Copy line %d from %p to %p\n", y, d, c));

    if ( y < dh )
    { for(; x < w && x < dw; x += 16)
	*c++ = ~(*d++);
      if ( x-dw > 0 )			/* need partial padding */
      { unsigned short mask = 0xffff0000L >> (16-(x-dw));
	unsigned short m2 = ((mask >> 8) & 0x00ff) | ((mask << 8) & 0xff00);

	DEBUG(NAME_cursor,
	      if ( saidpad++ == 0 )
	        Cprintf("mask = 0x%04x; ms = 0x%04x\n", mask, m2));

	c[-1] &= m2;
      }
    }
    for(; x < w; x += 16)
      *c++ = 0x0000;
  }

  if ( alloced )
    unalloc(alloced, dbits);
  if ( im != image )
    freeObject(im);

  DEBUG(NAME_cursor, Cprintf("Returning %dx%d bits\n", w, h));

  return cbits;
}

		 /*******************************
		 *	       ICONS		*
		 *******************************/

static Image
black_mask(int w, int h)
{ static Image img = NULL;

  if ( ! img )
  { img = newObject(ClassImage, NIL, toInt(w), toInt(h), NAME_bitmap, EAV);
    lockObject(img, ON);
    send(img, NAME_invert, EAV);
  }

  return img;
}


HICON
ws_icon_from_image(Image img)
{ WsImage r = attach_ws_image(img);

  if ( !r->icon )
  { int iw = GetSystemMetrics(SM_CXICON);
    int ih = GetSystemMetrics(SM_CYICON);
    int freemask = FALSE;
    Image image, imask;
    HICON icon;

#ifdef O_SCALE_ICON
    if ( valInt(img->size->w) != iw || valInt(img->size->h) != ih )
      image = get(img, NAME_scale,
		  answerObject(ClassSize, toInt(iw), toInt(ih), EAV),
		  EAV);
    else
#endif
      image = img;

    if ( notNil(image->mask) )
    { if ( image->mask->kind == NAME_pixmap )
      { imask = getMonochromeImage(image->mask);
	freemask = TRUE;
      } else
	imask = image->mask;
    } else
      imask = black_mask(iw, ih);

    if ( isNil(image->display) )
      assign(image, display, CurrentDisplay(NIL));
    assign(imask, display, image->display);

#if 1
  { ICONINFO iinfo;

    iinfo.fIcon = TRUE;
    iinfo.xHotspot = 0;
    iinfo.yHotspot = 0;
    iinfo.hbmMask  = (HBITMAP) getXrefObject(imask, imask->display);
    iinfo.hbmColor = (HBITMAP) getXrefObject(image, image->display);

    icon = CreateIconIndirect(&iinfo);
  }
#else
  { HBITMAP hbm, hmsk;
    BITMAP bm, msk;
    BYTE *bmbits, *mskbits;

    hbm  = (HBITMAP) getXrefObject(image, image->display);
    hmsk = (HBITMAP) getXrefObject(imask, imask->display);
    GetObject(hbm,  sizeof(bm), &bm);
    GetObject(hmsk, sizeof(msk), &msk);
    bmbits  = pceMalloc(ih * bm.bmWidthBytes);
    mskbits = pceMalloc(ih * msk.bmWidthBytes);

    GetBitmapBits(hbm,  ih * bm.bmWidthBytes,  bmbits);
    GetBitmapBits(hmsk, ih * msk.bmWidthBytes, mskbits);

    icon = CreateIcon(PceHInstance,
		      iw, ih,
		      (BYTE)bm.bmPlanes, (BYTE)bm.bmBitsPixel,
		      mskbits,
		      bmbits);

    pceFree(bmbits);
    pceFree(mskbits);
  }
#endif

    if ( image != img )
      freeObject(image);
    if ( freemask )
      freeObject(imask);

    r->icon = icon;
  }

  return r->icon;
}


		 /*******************************
		 *    WINDOWS SYSTEM BRUSHES	*
		 *******************************/

struct system_brush
{ char *name;
  int  id;
};

struct system_image
{ char *name;
  int  id;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Windows system colors as obtained from GetSysColor()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static struct system_brush window_brushes[] =
{ { "win_black_brush",	BLACK_BRUSH },
  { "win_dkgray_brush",	DKGRAY_BRUSH },
  { "win_gray_brush",	GRAY_BRUSH },
  { "win_hollow_brush",	HOLLOW_BRUSH },
  { "win_ltgray_brush",	LTGRAY_BRUSH },
  { "win_null_brush",	NULL_BRUSH },
  { NULL,		0 }
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Win32 predefined images.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static struct system_image window_images[] =
{ { "win_btncorners",	OBM_BTNCORNERS },
  { "win_btsize", 	OBM_BTSIZE },
  { "win_check",	OBM_CHECK },
  { "win_checkboxes",	OBM_CHECKBOXES },
  { "win_close",	OBM_CLOSE },
  { "win_combo",	OBM_COMBO },
  { "win_dnarrow",	OBM_DNARROW },
  { "win_dnarrowd",	OBM_DNARROWD },
  { "win_dnarrowi",	OBM_DNARROWI },
  { "win_lfarrow",	OBM_LFARROW },
  { "win_lfarrowd",	OBM_LFARROWD },
  { "win_lfarrowi",	OBM_LFARROWI },
  { "win_mnarrow",	OBM_MNARROW },
/* Not supported in NT 4.0
  { "win_old_close",	OBM_OLD_CLOSE },
  { "win_old_dnarrow",	OBM_OLD_DNARROW },
  { "win_old_lfarrow",	OBM_OLD_LFARROW },
  { "win_old_reduce",	OBM_OLD_REDUCE },
  { "win_old_restore",	OBM_OLD_RESTORE },
  { "win_old_rgarrow",	OBM_OLD_RGARROW },
  { "win_old_uparrow",	OBM_OLD_UPARROW },
  { "win_old_zoom",	OBM_OLD_ZOOM },
*/
  { "win_reduce",	OBM_REDUCE },
  { "win_reduced",	OBM_REDUCED },
  { "win_restore",	OBM_RESTORE },
  { "win_restored",	OBM_RESTORED },
  { "win_rgarrow",	OBM_RGARROW },
  { "win_rgarrowd",	OBM_RGARROWD },
  { "win_rgarrowi",	OBM_RGARROWI },
  { "win_size",		OBM_SIZE },
  { "win_uparrow",	OBM_UPARROW },
  { "win_uparrowd",	OBM_UPARROWD },
  { "win_uparrowi",	OBM_UPARROWI },
  { "win_zoom",		OBM_ZOOM },
  { "win_zoomd",	OBM_ZOOMD },
  { NULL,		0 }
};


static void
ws_system_brushes(DisplayObj d)
{ struct system_brush *sb = window_brushes;

  for( ; sb->name; sb++)
  { Name name = CtoKeyword(sb->name);
    HBRUSH brush = GetStockObject(sb->id);

    if ( brush )
    { Image image = globalObject(name, ClassImage, name,
				 toInt(16), toInt(16), NAME_pixmap, EAV);
      assign(image, access, NAME_read);
      declareWindowsBrush(image, brush);
    } else
      Cprintf("Could not GetStockObject for %s\n", sb->name);
  }
}


void
ws_system_images()
{ static int done = FALSE;
  struct system_image *si = window_images;
  DisplayObj d = CurrentDisplay(NIL);

  if ( done )
    return;
  done = TRUE;

  for( ; si->name; si++)
  { Name name = CtoKeyword(si->name);
    HBITMAP bm = LoadBitmap(NULL, (const char *)si->id);

    if ( bm )
    { BITMAP bitmap;
      Image image;
      Name kind;
      int depth;

      if ( !GetObject(bm, sizeof(BITMAP), &bitmap) )
	Cprintf("ws_system_images(): GetObject() failed\n");

      depth = bitmap.bmPlanes * bitmap.bmBitsPixel;
      kind = (depth == 1 ? NAME_bitmap : NAME_pixmap);
      image = globalObject(name, ClassImage, name,
			   toInt(bitmap.bmWidth),
			   toInt(bitmap.bmHeight),
			   kind,
			   EAV);
      assign(image, depth, toInt(depth));
      registerXrefObject(image, d, (void *)bm);
      assign(image, access, NAME_read);
    } else
      Cprintf("Could not LoadBitmap for %s\n", si->name);
  }

  ws_system_brushes(d);
}


		 /*******************************
		 *	      PALETTE		*
		 *******************************/

ColourMap
ws_colour_map_for_image(Image img)
{ fail;
}
