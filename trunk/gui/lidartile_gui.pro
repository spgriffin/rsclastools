;+
; NAME:
;
;   LidarTile_GUI
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

PRO LidarTile_GUI

  FORWARD_FUNCTION FSC_INPUTFIELD
  
  ; A LIDAR program.
  tlb = Widget_Base(Title='Spatial Tiling', Column=1, tlb_frame_attr=1, xpad=3, ypad=3, space=2)
  
  ; Variables
  infile = dialog_pickfile(filter='*.las', /fix_filter, /multiple_files, /must_exist, title='Please Select LAS File/s (Ctrl-click to select multiple files)', dialog_parent=tlb)
  if (infile[0] EQ '') then return
  infile_bn = FILE_BASENAME(infile)
  infile_dn = FILE_DIRNAME(infile)
  text = WIDGET_LABEL(tlb, value='LAS files selected for tiling:', frame=0, /align_left)
  wTree = WIDGET_TREE(tlb)
  wtRoot = WIDGET_TREE(wTree, VALUE=infile_dn[0], /FOLDER, /EXPANDED)
  for i = 0L, n_elements(infile)-1L, 1L do begin
    wtLeaf = WIDGET_TREE(wtRoot, VALUE=infile_bn[i])
  endfor
  
  field1 = FSC_INPUTFIELD(tlb, Title='X tile size (m) : ', LabelSize=200, Value=1000, /IntegerValue, /Positive, LabelAlign=1)
  field2 = FSC_INPUTFIELD(tlb, Title='Y tile size (m) : ', LabelSize=200, Value=1000, /IntegerValue, /Positive, LabelAlign=1)
  fields = ['Merge LAS files into a single tile grid']
  mergeflag = cw_bgroup(tlb, fields, column=1, /nonexclusive)
  text = WIDGET_LABEL(tlb, value='If not selected each LAS file will be tiled separately', frame=0, /align_left)
  text = WIDGET_LABEL(tlb, value='The output files are named <prefix>_<x>_<y>.las,', frame=0, /align_left)
  text = WIDGET_LABEL(tlb, value='where x and y are the row and column numbers, respectively.', frame=0, /align_left)
  text = WIDGET_LABEL(tlb, value='If LAS file are merged the <prefix> is the prefix of the first LAS file.', frame=0, /align_left)
  
  ; Set up TABing between fields.
  field1->SetTabNext, field2->GetTextID()
  field2->SetTabNext, field1->GetTextID()
  
  ; Do the rest
  button = Widget_Button(tlb, Value='Create Tiles', UValue='StartLidarTile')
  button = Widget_Button(tlb, Value='Cancel', UValue='Quit')
  Widget_Control, tlb, /Realize, Set_UValue={ $
    field1:field1, $ ; X tile size
    field2:field2, $ ; Y tile size
    mergeflag:mergeflag, $ ; Flag for merging
    infile:infile} ; filename/s
  XManager, 'RSC_LAS_Tools', tlb, /No_Block
  
END


