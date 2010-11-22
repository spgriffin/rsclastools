;+
; NAME:
;
;   LAS2Shapefile_GUI
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

PRO LAS2Shapefile_GUI

  FORWARD_FUNCTION FSC_Droplist, FSC_INPUTFIELD
  
  ; A LIDAR program.
  tlb = Widget_Base(Title='Export LAS', Column=2, tlb_frame_attr=1, xpad=3, ypad=3, space=2)
  
  ; Variables
  infile = dialog_pickfile(filter=['*.las'], /fix_filter, /multiple_files, /must_exist, title='Please Select LAS File/s (Ctrl-click to select multiple files)', dialog_parent=tlb)
  if (infile[0] EQ '') then return
  infile_bn = FILE_BASENAME(infile)
  infile_dn = FILE_DIRNAME(infile)
  text = WIDGET_LABEL(tlb, value='LAS Files Selected for Export:', frame=0, /align_center)
  wTree = WIDGET_TREE(tlb)
  wtRoot = WIDGET_TREE(wTree, VALUE=infile_dn[0], /FOLDER, /EXPANDED)
  for i = 0L, n_elements(infile)-1L, 1L do begin
    wtLeaf = WIDGET_TREE(wtRoot, VALUE=infile_bn[i])
  endfor
  
  ; Fields to export
  tlb1 = widget_base(tlb, column=1, xsize=!QRSC_LIDAR_XSIZE)
  text1 = WIDGET_LABEL(tlb1, value='Fields to Export as Shapefile Attributes', frame=0, /align_center)
  Base1 = widget_base(tlb1, column=1, frame=1)
  fields = ['Time', $
    'Elevation', $
    'Intensity', $
    'Height', $
    'Return Number', $
    'Number of Returns', $
    'Classification', $
    'Scan Direction Flag', $
    'Edge of Flight Line', $
    'Scan Angle Rank', $
    'User Data', $
    'Point Source ID', $
    'Red Image Channel', $
    'Green Image Channel', $
    'Blue Image Channel']
  table = cw_bgroup(Base1, fields, column=2, /nonexclusive)
  text = WIDGET_LABEL(Base1, value='Select one or more fields to export.', frame=0, /align_left)
  
  ; Output file information
  tlb3 = widget_base(tlb, column=1, xsize=!QRSC_LIDAR_XSIZE)
  text3 = WIDGET_LABEL(tlb3, value='Output File Information', frame=0, /align_center)
  Base3 = widget_base(tlb3, column=1, frame=1)
  text = WIDGET_LABEL(Base3, value='The output is an XYZ Point Shapefile.', frame=0, /align_left)
  text = WIDGET_LABEL(Base3, value='Output filename is the input with a ".shp" suffix.', frame=0, /align_left)
  text = WIDGET_LABEL(Base3, value='For example, Input : t1.las', frame=0, /align_left)
  text = WIDGET_LABEL(Base3, value='Output : t1.shp, t1.shx, t1.dbf', frame=0, /align_left)
  
  ; Do the rest
  tlb4 = widget_base(tlb, column=1, xsize=!QRSC_LIDAR_XSIZE)
  button = Widget_Button(tlb4, Value='Start Export', UValue='StartLAS2Shapefile')
  button = Widget_Button(tlb4, Value='Cancel', UValue='Quit')
  Widget_Control, tlb, /Realize, Set_UValue={ $
    table:table, $ ; ; Field list widget ID
    infile:infile} ; filename/s
  XManager, 'RSC_LAS_Tools', tlb, /No_Block
  
END


