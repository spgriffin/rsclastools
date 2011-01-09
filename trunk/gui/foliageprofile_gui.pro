;+
; NAME:
;
;   FoliageProfile_GUI
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

PRO FoliageProfile_GUI

  FORWARD_FUNCTION FSC_Droplist, FSC_INPUTFIELD
  
  ; A LIDAR program.
  tlb = Widget_Base(Title='Derive Vertical Profile', Column=2, tlb_frame_attr=1, xpad=3, ypad=3, space=2)
  
  ; Variables
  infile = dialog_pickfile(filter='*.las', /fix_filter, /multiple_files, /must_exist, title='Please Select LAS File/s (Ctrl-click to select multiple files)', dialog_parent=tlb)
  if (infile[0] EQ '') then return
  infile_bn = FILE_BASENAME(infile)
  infile_dn = FILE_DIRNAME(infile)
  text = WIDGET_LABEL(tlb, value='LAS files selected : ', frame=0, /align_left)
  wTree = WIDGET_TREE(tlb)
  wtRoot = WIDGET_TREE(wTree, VALUE=infile_dn[0], /FOLDER, /EXPANDED)
  for i = 0L, n_elements(infile)-1L, 1L do begin
    wtLeaf = WIDGET_TREE(wtRoot, VALUE=infile_bn[i])
  endfor
  
  tlb1 = widget_base(tlb, column=1, xsize=!QRSC_LIDAR_XSIZE)
  text1 = WIDGET_LABEL(tlb1, value='Vertical Profile Settings', frame=0, /align_center)
  Base1 = widget_base(tlb1, column=1, frame=1)
  fields = ['Return Counts', 'Intensity']
  type = cw_bgroup(Base1, fields, column=1, /exclusive, label_top='Profile Type : ', set_value=0)
  
  interval = FSC_INPUTFIELD(Base1, Title='Height Bin Size (m) : ', Value=0.5, /FloatValue, LabelAlign=1, decimal=2, /positive)
  max_height = FSC_INPUTFIELD(Base1, Title='Maximum Return Height (m) : ', Value=100.0, /FloatValue, LabelAlign=1, decimal=2, /positive)
  
  ; Create list of rhov rhog methods
  rhovgList = ['Mean / Mean', $
    'Max / Mean', $
    'Percentile /  Mean', $
    'Constant /  Mean', $
    'Constant']
    
  tlb4 = widget_base(tlb, column=1, xsize=!QRSC_LIDAR_XSIZE)
  text4 = WIDGET_LABEL(tlb4, value='Settings Unique to Intensity Profile Type', frame=0, /align_center)
  Base4 = widget_base(tlb4, column=1, frame=1)
  rhovg_droplist = FSC_Droplist(Base4, Value=rhovgList, Index=0, title='Plant/Ground Reflectivity Ratio : ')
  text = WIDGET_LABEL(Base4, value=' The ratio is calculated from the first returns.', frame=0, /align_left)
  text = WIDGET_LABEL(Base4, value=' Be aware this requires a number of assumptions.', frame=0, /align_left)
  rhovg_percentile = FSC_INPUTFIELD(Base4, Title='Plant Intensity Percentile (0-1): ', Value=0.99, /FloatValue, LabelAlign=1, decimal=2)
  constant = FSC_INPUTFIELD(Base4, Title='Constant Value : ', Value=1.0, /FloatValue, LabelAlign=1, decimal=2)
  text = WIDGET_LABEL(Base4, value=' e.g. A value determined from field spectra or optimisation.', frame=0, /align_left)
  
  tlb6 = widget_base(tlb, column=1,xsize=!QRSC_LIDAR_XSIZE)
  text5 = WIDGET_LABEL(tlb6, value='Output File Information', frame=0, /align_center)
  Base6 = widget_base(tlb6, column=1, frame=1)
  text = WIDGET_LABEL(Base6, value='Output filename is the input with a ".csv" suffix.', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='The profile type is appended to the filename.', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='For example, Input : t1.las', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='Output : t1_VerticalProfile_Intensity.csv', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='Columns of the comma delimited output file :', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='1. Height', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='2. Fractional Cover', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='3. Cumulative Gap Fraction', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='4. Derivative of Cumulative Gap Fraction', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='5. Apparant Foliage Profile', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='6. Relative Apparant Foliage Profile', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='For intensity derived profiles, the last line is : ', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='Plant/Ground Reflectance Ratio', frame=0, /align_left)
  
  ; Do the rest
  tlb7 = widget_base(tlb, column=1, xsize=!QRSC_LIDAR_XSIZE)
  button = Widget_Button(tlb7, Value='Derive Vertical Profiles', UValue='StartFoliageProfile')
  button = Widget_Button(tlb7, Value='Cancel', UValue='Quit')
  Widget_Control, tlb, /Realize, Set_UValue={ $
    max_height:max_height, $ ; maximum height to consider
    rhovg_droplist:rhovg_droplist, $ ; rhovg
    rhovgList:rhovgList, $ ; rhovg types
    constant:constant, $ ; Constant
    rhovg_percentile:rhovg_percentile, $ ; rhovg percentile
    type:type, $ ; type widget ID
    interval:interval, $ ; bin size
    infile:infile} ; filename/s
  XManager, 'RSC_LAS_Tools', tlb, /No_Block
  
END


