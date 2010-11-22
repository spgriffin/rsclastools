;+
; NAME:
;
;   TilePointInterpHeights_GUI
;
; PURPOSE:
;
;   GUI interface for generating AGH LAS files
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

PRO TilePointInterpHeights_GUI

  FORWARD_FUNCTION FSC_Droplist, FSC_INPUTFIELD
  
  ; Main
  tlb = WIDGET_BASE(TITLE='LAS Product: Calculate Above Ground Heights', tlb_frame_attr=1, column=2, xpad=3, ypad=3, space=3)
  
  ; Get the input file/s
  inFile = dialog_pickfile(filter='*.las', /fix_filter, /multiple_files, /must_exist, title='Please Select LAS File/s (Ctrl-click to select multiple files)', dialog_parent=tlb)
  if (inFile[0] EQ '') then return
  nFiles = n_elements(inFile)
  
  ; Create list of interpolation methods
  interpList = ['Nearest Neighbour', $
    'Linear', $
    'Inverse Distance', $
    'Natural Neighbour', $
    'Polynomial Regression']
    
  ; Create the widget that will record the user parameters
  infile_bn = FILE_BASENAME(inFile)
  infile_dn = FILE_DIRNAME(inFile)
  text = WIDGET_LABEL(tlb, value='LAS files selected : ', frame=0, /align_left)
  wTree = WIDGET_TREE(tlb)
  wtRoot = WIDGET_TREE(wTree, VALUE=infile_dn[0], /FOLDER, /EXPANDED)
  for i = 0L, n_elements(infile)-1L, 1L do begin
    wtLeaf = WIDGET_TREE(wtRoot, VALUE=infile_bn[i])
  endfor
  
  tlb3 = widget_base(tlb, column=1, xsize=!QRSC_LIDAR_XSIZE)
  text3 = WIDGET_LABEL(tlb3, value='LAS File Settings', frame=0, /align_center)
  Base3 = widget_base(tlb3, column=1, frame=1)
  text = WIDGET_LABEL(Base3, value='LAS field to store calculated heights:', frame=0, /align_left)
  text = WIDGET_LABEL(Base3, value='Point Source ID (0.01 scale factor).', frame=0, /align_left)
  text = WIDGET_LABEL(Base3, value='Elevation (Z offset and scale factor).', frame=0, /align_left)
  fields = ['Point Source ID', 'Elevation']
  output_type = cw_bgroup(Base3, fields, column=1, /exclusive, set_value=0)
  text = WIDGET_LABEL(Base3, value='The LAS field used is stored in systemID header field.', frame=0, /align_left)
  
  tlb4 = widget_base(tlb, column=1, xsize=!QRSC_LIDAR_XSIZE)
  text4 = WIDGET_LABEL(tlb4, value='Tiling Settings', frame=0, /align_center)
  Base4 = widget_base(tlb4, column=1, frame=1)
  tilexsize = FSC_INPUTFIELD(Base4, Title='X tile size (m) : ', Value=100, /IntegerValue, /Positive, LabelAlign=1)
  tileysize = FSC_INPUTFIELD(Base4, Title='Y tile size (m) : ', Value=100, /IntegerValue, /Positive, LabelAlign=1)
  
  tlb2 = widget_base(tlb, column=1, xsize=!QRSC_LIDAR_XSIZE)
  text2 = WIDGET_LABEL(tlb2, value='Interpolation Settings', frame=0, /align_center)
  Base2 = widget_base(tlb2, column=1, frame=1)
  method_droplist = FSC_Droplist(Base2, Value=interpList, Index=3, title='Height Interpolation Method : ')
  null = FSC_INPUTFIELD(Base2, Title='Missing data value * + : ', Value=-1, /FloatValue, LabelAlign=1)
  power = FSC_INPUTFIELD(Base2, Title='Power Parameter * + : ', Value=2, /IntegerValue, LabelAlign=1)
  smoothing = FSC_INPUTFIELD(Base2, Title='Smoothing Parameter * : ', Value=0.0, /FloatValue, LabelAlign=1, decimal=2, /positive)
  min_points = FSC_INPUTFIELD(Base2, Title='# points per local fit * + : ', Value=6, /IntegerValue, LabelAlign=1, /positive)
  sectors = FSC_INPUTFIELD(Base2, Title='# sectors * + : ', Value=6, /IntegerValue, LabelAlign=1, /positive)
  text = WIDGET_LABEL(Base2, value='* Inverse Distance method.', frame=0, /align_left)
  text = WIDGET_LABEL(Base2, value=' + Polynomial regression method.', frame=0, /align_left)
  
  tlb6 = widget_base(tlb, column=1,xsize=!QRSC_LIDAR_XSIZE)
  text6 = WIDGET_LABEL(tlb6, value='Output File Information', frame=0, /align_center)
  Base6 = widget_base(tlb6, column=1, frame=1)
  text = WIDGET_LABEL(Base6, value='Output filename is the input LAS filename with "_AGH" appended.', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='For example, Input : t1.las', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='Output : t1_AGH.las', frame=0, /align_left)
  
  Base7 = widget_base(tlb, column=1,xsize=!QRSC_LIDAR_XSIZE)
  button = Widget_Button(Base7, Value='Start AGH Calculation', UValue='StartTilePointInterpHeights')
  button = Widget_Button(Base7, Value='Cancel', UValue='Quit')
  
  ; Do the rest
  Widget_Control, tlb, /Realize, Set_UValue={ $
    tilexsize:tilexsize, $ ; X tile size
    tileysize:tileysize, $ ; Y tile size
    null:null, $ ; null value
    method_droplist:method_droplist, $ ; interpolation method
    power:power, $ ; power value for IDW
    smoothing:smoothing, $ ; smoothing parameter
    min_points:min_points, $ ; min_points
    sectors:sectors, $ ; sectors
    infile:infile, $ ; filename/s
    interpList:interpList, $ ;Interpolation Methods
    output_type:output_type} ; Method to store calculated heights
    
  XManager, 'RSC_LAS_Tools', tlb, /No_Block
  
END


