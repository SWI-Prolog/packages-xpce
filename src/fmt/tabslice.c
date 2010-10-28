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

#include <box/boxes.h>			/* to exploit class rubber */

static status computeRubberTableRow(TableRow row);
static status computeRubberTableColumn(TableColumn col);

static status
initialiseTableSlice(TableSlice c)
{ initialiseVectorv((Vector)c, 0, NULL);

  assign(c, background, DEFAULT);
  assign(c, selected,   OFF);
  assign(c, end_group,  OFF);
  assign(c, name,	NIL);
  assign(c, index,      ZERO);
  assign(c, width,      ZERO);
  assign(c, reference,  ZERO);
  assign(c, position,   ZERO);
  assign(c, fixed,      OFF);
  assign(c, displayed,  ON);
/*assign(c, table,      NIL);
  assign(c, rubber,     NIL);
*/

  succeed;
}


Any
findNamedSlice(Vector v, Name name)
{ TableSlice slice;

  for_vector(v, slice,
	     if ( instanceOfObject(slice, ClassTableSlice) &&
		  slice->name == name )
	       answer(slice);
	    );

  fail;
}


static status
endGroupTableSlice(TableSlice slice, BoolObj end)
{ if ( slice->end_group != end )
  { assign(slice, end_group, end);
    if ( notNil(slice->table) )
      changedTable(slice->table);
  }

  succeed;
}


static status
rubberTableSlice(TableSlice slice, Rubber rubber)
{ if ( isDefault(rubber) )
  { if ( instanceOfObject(slice, ClassTableColumn) )
      return computeRubberTableColumn((TableColumn)slice);
    else
      return computeRubberTableRow((TableRow)slice);
  } else
  { if ( slice->rubber != rubber )	/* equalRubber? */
    { assign(slice, rubber, rubber);
      if ( notNil(slice->table) )
	changedTable(slice->table);
    }
  }

  succeed;
}


static status
widthTableSlice(TableSlice slice, Int width)
{ if ( notDefault(width) )
  { assign(slice, width, width);
    assign(slice, fixed, ON);
  } else
  { assign(slice, fixed, OFF);
  }

  if ( notNil(slice->table) )
    return requestComputeLayoutManager((LayoutManager)slice->table, DEFAULT);

  succeed;
}


static status
displayedTableSlice(TableSlice slice, BoolObj val)
{ if ( slice->displayed != val )
  { assign(slice, displayed, val);

    if ( notNil(slice->table) )
      return requestComputeLayoutManager((LayoutManager)slice->table, DEFAULT);
  }

  succeed;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

/* Instance Variables */

static vardecl var_table_slice[] =
{ IV(NAME_table, "table*", IV_GET,
     NAME_organisation, "Table I belong to"),
  IV(NAME_background, "[colour|pixmap]", IV_GET,
     NAME_colour, "Default background of the cells"),
  IV(NAME_selected, "bool", IV_GET,
     NAME_selection, "If @on, all cells in the row/column are selected"),
  IV(NAME_alignment, "{top,bottom,left,right,center,reference,stretch}",
     IV_GET,
     NAME_layout, "Default alignment of cells"),
  SV(NAME_endGroup, "bool", IV_GET|IV_STORE, endGroupTableSlice,
     NAME_appearance, "Row/column ends a group (rules)"),
  IV(NAME_name, "name*", IV_BOTH,
     NAME_name, "Name of the column/row"),
  IV(NAME_index, "int", IV_GET,
     NAME_position, "X/Y position for column/row"),
  IV(NAME_fixed, "bool", IV_BOTH,
     NAME_layout, "<-width and <-reference are fixed"),
  IV(NAME_width, "0..", IV_GET,
     NAME_layout, "Total width/height of the column/row"),
  IV(NAME_reference, "int", IV_GET,
     NAME_layout, "Location of the reference"),
  IV(NAME_position, "int", IV_GET,
     NAME_layout, "X/Y-offset of the column/row"),
  IV(NAME_rubber, "rubber*", IV_GET,
     NAME_layout, "How to handle forced width/height"),
  SV(NAME_displayed, "bool", IV_GET|IV_STORE, displayedTableSlice,
     NAME_visibility, "If @on, row/column is visible")
};

/* Send Methods */

static senddecl send_table_slice[] =
{ SM(NAME_initialise, 0, NULL, initialiseTableSlice,
     DEFAULT, "Initialise abstract instance"),
  SM(NAME_width, 1, "[int]", widthTableSlice,
     NAME_layout, "Set (fixed) width of the table slice"),
  SM(NAME_rubber, 1, "[rubber]*", rubberTableSlice,
     DEFAULT, NULL)
};

/* Get Methods */

#define get_table_slice NULL
/*
static getdecl get_table_slice[] =
{ GM(NAME_convert, 1, "table_slice", "graphical",
     getConvertTableSlice,
     DEFAULT, "Convert graphical object")
};
*/

/* Resources */

#define rc_table_slice NULL
/*
static classvardecl rc_table_slice[] =
{
};
*/

/* Class Declaration */

ClassDecl(table_slice_decls,
          var_table_slice,
	  send_table_slice,
	  get_table_slice,
	  rc_table_slice,
          0, NULL,
          "$Rev$");

status
makeClassTableSlice(Class class)
{ return declareClass(class, &table_slice_decls);
}


		 /*******************************
		 *	       COLUMN		*
		 *******************************/

static Any				/* TBD: merge */
getIf(Any cell, Name method, Any def)	/* can be optimised further */
{ Any rval;

  if ( hasGetMethodObject(cell, method) &&
       (rval = getv(cell, method, 0, NULL)) )
    answer(rval);

  answer(def);
}


static status
initialiseTableColumn(TableColumn col, Name halign)
{ initialiseTableSlice((TableSlice)col);

  if ( isDefault(halign) )
    halign = NAME_left;

  assign(col, alignment, halign);

  succeed;
}


static status
unlinkTableColumn(TableColumn col)
{ if ( notNil(col->table) && !isFreeingObj(col->table) )
    send(col->table, NAME_delete, col, EAV);

  return unlinkVector((Vector)col);
}


static void
changedImageTableColumn(TableColumn col)
{ Table tab;
  Device dev;

  if ( notNil(tab = col->table) && notNil(dev=tab->device) )
    changedImageGraphical(dev, col->position, 0, col->width, tab->area->h);
}


static status
halignTableColumn(TableColumn col, Name halign)
{ assign(col, alignment, halign);

  succeed;
}


static Name
getHalignTableColumn(TableColumn col)
{ answer(col->alignment);
}


static status
backgroundTableColumn(TableColumn col, Any bg)
{ if ( col->background != bg )
  { assign(col, background, bg);
    changedImageTableColumn(col);
  }

  succeed;
}


static status
selectedTableColumn(TableColumn col, BoolObj selected)
{ if ( col->selected != selected )
  { assign(col, selected, selected);
    changedImageTableColumn(col);
  }

  succeed;
}


static TableCell
getCellTableColumn(TableColumn col, Int y)
{ Table tab = col->table;
  TableRow row = getElementVector(tab->rows, y);

  if ( row && notNil(row) )
    answer(getCellTableRow(row, col->index));

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Set the <-width of the column to the max of the width of the graphicals.
If objects have horizontal alignment, maximize  left and right sides and
sum.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
computeTableColumn(TableColumn col)
{ Table tab = col->table;
  int ymin = valInt(getLowIndexVector(tab->rows));
  int ymax = valInt(getHighIndexVector(tab->rows));
  int rows = 0;
  int l=0, r=0, w=0;
  int y;

  for(y=ymin; y<=ymax; y++)
  { TableCell cell = getCellTableColumn(col, toInt(y));

    if ( cell && cell->col_span == ONE && notNil(cell->image) )
    { Graphical gr = cell->image;
      int grw;
      int px, py;

      ComputeGraphical(gr);
      table_cell_padding(cell, &px, &py);
      grw = valInt(gr->area->w);

      if ( getHalignTableCell(cell) == NAME_reference )
      {	Point ref = getIf(gr, NAME_reference, FAIL);
	int rx = (ref ? valInt(ref->x) : 0);

	l = max(l, px+rx);
	r = max(r, px+grw-rx);
      } else
      { w = max(w, 2*px+grw);
      }
    }

    if ( cell )
      rows++;
  }

  w = max(w, l+r);
  assign(col, width, toInt(w));
  assign(col, reference, toInt(l));
  DEBUG(NAME_table, Cprintf("Column %d set to width = %d\n",
			    valInt(col->index), w));

  succeed;
}


static status
forAllTableColumn(TableColumn col, Code code)
{ Table tab = col->table;
  int ymin = valInt(getLowIndexVector(tab->rows));
  int ymax = valInt(getHighIndexVector(tab->rows));
  int y;

  for(y=ymin; y<=ymax; y++)
  { TableCell cell = getCellTableColumn(col, toInt(y));

    if ( cell )
    { Any av[2];

      av[0] = cell;
      av[1] = toInt(y);
      if ( !forwardCodev(code, 2, av) )
	fail;
    }
  }

  succeed;
}


static status
computeRubberTableColumn(TableColumn col)
{ Table tab = col->table;
  int ymin = valInt(getLowIndexVector(tab->rows));
  int ymax = valInt(getHighIndexVector(tab->rows));
  int y;
  stretch *stretches = alloca((ymax-ymin+1)*sizeof(stretch));
  int nstretches = 0;
  stretch joined;

  for(y=ymin; y<=ymax; y++)
  { TableCell cell = getCellTableColumn(col, toInt(y));

    if ( cell )
    { if ( cell->col_span == ONE )
	cell_stretchability(cell, NAME_column, &stretches[nstretches++]);
    }
  }

  if ( nstretches > 0 )
  { Rubber r;

    join_stretches(stretches, nstretches, &joined);
    r = newObject(ClassRubber,
		  ONE,
		  toInt(joined.stretch),
		  toInt(joined.shrink),
		  EAV);
    assign(r, minimum, toInt(joined.minimum));
    assign(r, maximum, toInt(joined.maximum));
    assign(r, natural, toInt(joined.ideal));

    assign(col, rubber, r);
  } else
  { assign(col, rubber, NIL);
  }

  succeed;
}


/* Type declarations */

static char T_halign[]    = "{left,right,center,reference,stretch}";
static char T_defhalign[] = "[{left,right,center,reference,stretch}]";

/* Instance Variables */

static vardecl var_table_column[] =
{ IV(NAME_alignment, T_halign, IV_GET|IV_REDEFINE,
     NAME_layout, "Default alignment of cells")
};

/* Send Methods */

static senddecl send_table_column[] =
{ SM(NAME_initialise, 1, T_defhalign, initialiseTableColumn,
     DEFAULT, "Initialise table column"),
  SM(NAME_unlink, 0, NULL, unlinkTableColumn,
     DEFAULT, "Remove from <-table"),
  SM(NAME_background, 1, "[colour|pixmap]", backgroundTableColumn,
     NAME_colour, NULL),
  SM(NAME_selected, 1, "bool", selectedTableColumn,
     NAME_selection, NULL),
  SM(NAME_halign, 1, T_halign, halignTableColumn,
     NAME_alignment, "Default horizontal alignment"),
  SM(NAME_compute, 0, NULL, computeTableColumn,
     NAME_layout, "Compute dimensions of the column"),
  SM(NAME_forAll, 1, "code", forAllTableColumn,
     NAME_iterate, "Run code on all cells in column")
};

/* Get Methods */

static getdecl get_table_column[] =
{ GM(NAME_halign, 0, T_halign, NULL,
     getHalignTableColumn,
     NAME_alignment, "Default horizontal alignment"),
  GM(NAME_cell, 1, "table_cell", "int",
     getCellTableColumn,
     NAME_contents, "Cell at indicated row")
};

/* Resources */

#define rc_table_column NULL
/*
static classvardecl rc_table_column[] =
{
};
*/

/* Class Declaration */

ClassDecl(table_column_decls,
          var_table_column,
	  send_table_column,
	  get_table_column,
	  rc_table_column,
          0, NULL,
          "$Rev$");

status
makeClassTableColumn(Class class)
{ return declareClass(class, &table_column_decls);
}


		 /*******************************
		 *	       ROW		*
		 *******************************/

static status
initialiseTableRow(TableRow col, Name valign)
{ initialiseTableSlice((TableSlice)col);

  if ( isDefault(valign) )
    valign = NAME_top;

  assign(col, alignment, valign);

  succeed;
}


static status
unlinkTableRow(TableRow row)
{ if ( notNil(row->table) && !isFreeingObj(row->table) )
    send(row->table, NAME_delete, row, EAV);

  return unlinkVector((Vector)row);
}


static status
valignTableRow(TableRow col, Name valign)
{ assign(col, alignment, valign);

  succeed;
}


static Name
getHalignTableRow(TableRow col)
{ answer(col->alignment);
}


static void
changedImageTableRow(TableRow row)
{ Table tab;
  Device dev;

  if ( notNil(tab = row->table) && notNil(dev=tab->device) )
    changedImageGraphical(dev, 0, row->position, tab->area->w, row->width);
}


static status
backgroundTableRow(TableRow row, Any bg)
{ if ( row->background != bg )
  { assign(row, background, bg);
    changedImageTableRow(row);
  }

  succeed;
}


static status
selectedTableRow(TableRow row, BoolObj selected)
{ if ( row->selected != selected )
  { assign(row, selected, selected);
    changedImageTableRow(row);
  }

  succeed;
}


TableCell
getCellTableRow(TableRow row, Any x)
{ TableCell cell;

  if ( !isInteger(x) )
  { if ( notNil(row->table) )
    { TableColumn col = findNamedSlice(row->table->columns, x);

      if ( col )
	x = col->index;
      else
	fail;
    } else
      fail;				/* error */
  }

  if ( (cell = getElementVector((Vector)row, x)) && notNil(cell) )
    answer(cell);

  fail;
}


status
cellTableRow(TableRow row, Int col, TableCell cell)
{ TableCell old;

  if ( (old=getCellTableRow(row, col)) )
  { if ( old != cell )
    { if ( notNil(cell) )
	freeObject(old);
    } else
      succeed;				/* no change */
  }

  return elementVector((Vector)row, col, cell);
}


status
indexTableRow(TableRow row, Int index)
{ for_vector_i(row, TableCell cell, i,
	       { if ( cell->row == row->index &&
		      cell->column == toInt(i) )
		   assign(cell, row, index);
	       });

  assign(row, index, index);

  succeed;
}


static status
computeTableRow(TableRow row)
{ int xmin = valInt(getLowIndexVector((Vector)row));
  int xmax = valInt(getHighIndexVector((Vector)row));
  int t=0, b=0, h=0;
  int x;

  for(x=xmin; x<=xmax; x++)
  { TableCell cell = getCellTableRow(row, toInt(x));

    if ( cell && cell->row_span == ONE && notNil(cell->image) )
    { Graphical gr = cell->image;
      int grh;
      int px, py;

      ComputeGraphical(gr);
      table_cell_padding(cell, &px, &py);
      grh = valInt(gr->area->h);

      if ( getValignTableCell(cell) == NAME_reference )
      { Point ref = getIf(gr, NAME_reference, FAIL);
	int ry = (ref ? valInt(ref->y) : 0);

	t = max(t, py+ry);
	b = max(b, py+grh-ry);
      } else
      { h = max(h, 2*py+grh);
      }
    }
  }

  h = max(h, t+b);
  assign(row, width,     toInt(h));
  assign(row, reference, toInt(t));

  succeed;
}


static status
computeRubberTableRow(TableRow row)
{ Cprintf("computeRubberTableRow(): Not implemented");

  fail;
}


static status
appendTableRow(TableRow r, TableCell cell)
{ int i = valInt(getHighIndexVector((Vector)r));

  if ( notNil(r->table) )
  { return send(r->table, NAME_append, cell, toInt(i+1), r->index, EAV);
  } else
  { int cs = valInt(cell->col_span);

    i++;
    assign(cell, column, toInt(i));
    for( ; cs-- > 0; i++)
      cellTableRow(r, toInt(i), cell);

    succeed;
  }
}


/* Type declarations */

static char T_valign[] = "{top,bottom,center,reference,stretch}";
static char T_defvalign[] = "[{top,bottom,center,reference,stretch}]";

/* Instance Variables */

static vardecl var_table_row[] =
{ IV(NAME_alignment, T_valign, IV_GET|IV_REDEFINE,
     NAME_layout, "Default alignment of cells")
};

/* Send Methods */

static senddecl send_table_row[] =
{ SM(NAME_initialise, 1, T_defvalign, initialiseTableRow,
     DEFAULT, "Initialise table column"),
  SM(NAME_unlink, 0, NULL, unlinkTableRow,
     DEFAULT, "Remove from <-table"),
  SM(NAME_background, 1, "[colour|pixmap]", backgroundTableRow,
     NAME_colour, NULL),
  SM(NAME_selected, 1, "bool", selectedTableRow,
     NAME_selection, NULL),
  SM(NAME_valign, 1, T_valign, valignTableRow,
     NAME_alignment, "Default vertical alignment"),
  SM(NAME_compute, 0, NULL, computeTableRow,
     NAME_layout, "Compute dimensions of the row"),
  SM(NAME_height, 1, "[int]", widthTableSlice,
     NAME_layout, "Set (fixed) height of the table row"),
  SM(NAME_append, 1, "table_cell", appendTableRow,
     NAME_cell, "Append cell to row")
};

/* Get Methods */

static getdecl get_table_row[] =
{ GM(NAME_valign, 0, T_valign, NULL,
     getHalignTableRow,
     NAME_alignment, "Default horizontal alignment"),
  GM(NAME_cell, 1, "table_cell", "int|name",
     getCellTableRow,
     NAME_contents, "Cell at indicated column")
};

/* Resources */

#define rc_table_row NULL
/*
static classvardecl rc_table_row[] =
{
};
*/

/* Class Declaration */

ClassDecl(table_row_decls,
          var_table_row,
	  send_table_row,
	  get_table_row,
	  rc_table_row,
          0, NULL,
          "$Rev$");

status
makeClassTableRow(Class class)
{ return declareClass(class, &table_row_decls);
}

