;+
; NAME:
;
;   LidarPoint_TerrainMetrics_GUI
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

PRO LidarPoint_TerrainMetrics_GUI

  FORWARD_FUNCTION FSC_Droplist, FSC_INPUTFIELD
  
  ; Main
  tlb = WIDGET_BASE(TITLE='ASCII Product: Terrain Metrics', tlb_frame_attr=1, column=1, xpad=3, ypad=3, space=3)
  
  ; Get the input file/s
  inFile = dialog_pickfile(filter='*.las', /fix_filter, /multiple_files, /must_exist, title='Please Select LAS File/s (Ctrl-click to select multiple files)', dialog_parent=tlb)
  if (inFile[0] EQ '') then return
  nFiles = n_elements(inFile)
  
  ; Create list of product types
  productList = ['Slope', $
    'Aspect', $
    'Local Roughness']
    
  ; Create the widget that will record the user parameters
  infile_bn = FILE_BASENAME(inFile)
  infile_dn = FILE_DIRNAME(inFile)
  text = WIDGET_LABEL(tlb, value='LAS files selected : ', frame=0, /align_left)
  wTree = WIDGET_TREE(tlb)
  wtRoot = WIDGET_TREE(wTree, VALUE=infile_dn[0], /FOLDER, /EXPANDED)
  for i = 0L, n_elements(infile)-1L, 1L do begin
    wtLeaf = WIDGET_TREE(wtRoot, VALUE=infile_bn[i])
  endfor
  
  tlb1 = widget_base(tlb, column=1, xsize=!QRSC_LIDAR_XSIZE)
  text1 = WIDGET_LABEL(tlb1, value='ASCII Product Settings', frame=0, /align_center)
  Base1 = widget_base(tlb1, column=1, frame=1)
  prod_droplist = FSC_Droplist(Base1, Value=productList, Index=0, title='Product Type : ')
  null = FSC_INPUTFIELD(Base1, Title='"No Data" Value : ', Value=-1.0, /FloatValue, LabelAlign=1, decimal=2)
  
  tlb2 = widget_base(tlb, column=1,xsize=!QRSC_LIDAR_XSIZE)
  text2 = WIDGET_LABEL(tlb2, value='Output File Information', frame=0, /align_center)
  Base2 = widget_base(tlb2, column=1, frame=1)
  text = WIDGET_LABEL(Base2, value='Output filename is the input with a ".csv" suffix.', frame=0, /align_left)
  text = WIDGET_LABEL(Base2, value='The product type is appended to the filename.', frame=0, /align_left)
  text = WIDGET_LABEL(Base2, value='For example, Input : t1.las', frame=0, /align_left)
  text = WIDGET_LABEL(Base2, value='Output : t1_slope_GroundReturns.csv', frame=0, /align_left)
  text = WIDGET_LABEL(Base2, value='Columns of the comma delimited output file :', frame=0, /align_left)
  text = WIDGET_LABEL(Base2, value='1. LAS File', frame=0, /align_left)
  text = WIDGET_LABEL(Base2, value='2. Terrain Metric', frame=0, /align_left)
  text = WIDGET_LABEL(Base2, value='3. Number Of Ground Returns', frame=0, /align_left)
  
  Base3 = widget_base(tlb, column=1,xsize=!QRSC_LIDAR_XSIZE)
  button = Widget_Button(Base3, Value='Start ASCII Products', UValue='StartPoint_TerrainMetrics')
  button = Widget_Button(Base3, Value='Cancel', UValue='Quit')
  
  ; Do the rest
  Widget_Control, tlb, /Realize, Set_UValue={ $
    prod_droplist:prod_droplist, $ ; product
    null:null, $ ; null value
    infile:infile, $ ; filename/s
    productList:productList} ; Products
  XManager, 'RSC_LAS_Tools', tlb, /No_Block
  
END


