;+
; NAME:
;
;   Ascii2LAS_GUI
;
; PURPOSE:
;
;
;
; AUTHOR:
;
;   John Armston
;   Joint Remote Sensing Research Program
;   Centre for Spatial Environmental Research
;   School of Geography, Planning and Environmental Management
;   The University of Queensland
;   Brisbane QLD 4072, Australia
;   http://gpem.uq.edu.au/jrsrp
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS:
;
;
;
; KEYWORDS:
;
;
;
; OUTPUTS:
;
;
;
; RESTRICTIONS:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;    Written by John Armston, 2005.
;    Header and licence added for RSC LAS Tools, October 2010.
;
;-
;###########################################################################
;
; LICENSE
;
;   This file is part of RSC LAS Tools
;   Copyright (C) 2010  John Armston.
;
;   This program is free software: you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation, either version 3 of the License, or
;   (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU General Public License
;   along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;###########################################################################

PRO Ascii2LAS_GUI

  FORWARD_FUNCTION FSC_Droplist, FSC_INPUTFIELD
  
  ; A LIDAR program.
  tlb = Widget_Base(Title='Import ASCII', Column=2, tlb_frame_attr=1, xpad=3, ypad=3, space=2)
  
  ; Variables
  infile = dialog_pickfile(filter=['*.asc','*.txt','*.dat','*.csv'], /fix_filter, /multiple_files, /must_exist, title='Please Select ASCII File/s', dialog_parent=tlb)
  if (infile[0] EQ '') then return
  infile_bn = FILE_BASENAME(infile)
  infile_dn = FILE_DIRNAME(infile)
  text = WIDGET_LABEL(tlb, value='ASCII files selected for import:', frame=0, /align_left)
  wTree = WIDGET_TREE(tlb)
  wtRoot = WIDGET_TREE(wTree, VALUE=infile_dn[0], /FOLDER, /EXPANDED)
  for i = 0L, n_elements(infile)-1L, 1L do begin
    wtLeaf = WIDGET_TREE(wtRoot, VALUE=infile_bn[i])
  endfor
  
  Base2 = widget_base(tlb, column=1, frame=1)
  delimiterList = ['Tab','Whitespace','Comma','Semicolon']
  delimiter_droplist = FSC_Droplist(Base2, Value=delimiterList, Index=0, title='Column Delimiter : ')
  field2 = FSC_INPUTFIELD(Base2, Title='Number of header lines to skip : ', LabelSize=155, Value=0, /IntegerValue, /Positive, LabelAlign=1)
  columns = (lindgen(1,9)+1L)
  fields = ['Time','First Easting','First Northing','First Elevation', 'First Intensity','Last Easting','Last Northing','Last Elevation', 'Last Intensity']
  table = widget_table(Base2, background_color=[255,255,255], column_labels=['Column'], column_widths=100, /editable, format='(i2)', $
    foreground_color=[0,0,0], row_labels=fields, value=columns, alignment=1, xsize=1, ysize=9, $
    scr_xsize=200, scr_ysize=200, /all_events, uvalue='ChangeColumns')
  text = WIDGET_LABEL(Base2, value='To change a column number, type a new value and press enter.', frame=0, /align_left)
  text = WIDGET_LABEL(Base2, value='A maximum two returns (e.g. first & last) can be imported.', frame=0, /align_left)
  text = WIDGET_LABEL(Base2, value='If you have a single return datset, set the last return columns to 0.', frame=0, /align_left)
  text = WIDGET_LABEL(Base2, value='Data must be time sequential or have multiple returns linked.', frame=0, /align_left)
  text = WIDGET_LABEL(Base2, value='If there is no time field, set the time column to 0.', frame=0, /align_left)
  
  Base1 = widget_base(tlb, column=1, frame=1)
  field1 = FSC_INPUTFIELD(Base1, Title='Minimum distance between first and last returns (m) : ', LabelSize=250, Value=4.90, /FloatValue, /Positive, LabelAlign=1, Decimal=2)
  text = WIDGET_LABEL(Base1, value=' Distances less than this value indicate singular returns.', frame=0, /align_left)
  text = WIDGET_LABEL(Base1, value=' 4.9m applies to the Optech ALTM3025 Lidar System.', frame=0, /align_left)
  text = WIDGET_LABEL(Base1, value=' Set to 0.0 to skip the check for singular returns.', frame=0, /align_left)
  
  Base3 = widget_base(tlb, column=1, frame=1)
  text = WIDGET_LABEL(Base3, value='Return classification:', frame=0, /align_left)
  text = WIDGET_LABEL(Base3, value='Set to ground if you have an ASCII file with ground returns only (e.g. for a DEM).', frame=0, /align_left)
  fields = ['Unclassified', 'Non-ground', 'Ground']
  class = cw_bgroup(Base3, fields, column=1, /exclusive, set_value=0)
  
  ; Do the rest
  tlb3 = widget_base(tlb, column=1, xsize=!QRSC_LIDAR_XSIZE)
  button = Widget_Button(tlb3, Value='Start Import', UValue='StartAscii2LAS')
  button = Widget_Button(tlb3, Value='Cancel', UValue='Quit')
  Widget_Control, tlb, /Realize, Set_UValue={ $
    delimiterList:delimiterList, $  ; delimiter
    delimiter_droplist:delimiter_droplist, $ ; delimiter
    table:table, $ ; table ID
    columns:columns, $ ; Column numbers
    field1:field1, $ ; limit
    field2:field2, $ ; skipline
    infile:infile, $ ; filename/s
    class:class $ ; return classification
    }
  XManager, 'RSC_LAS_Tools', tlb, /No_Block
  
END


