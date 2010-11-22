;+
; NAME:
;
;   LidarFilter_GUI
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

PRO LidarFilter_GUI

  FORWARD_FUNCTION FSC_INPUTFIELD
  
  ; A LIDAR program.
  tlb = Widget_Base(Title='Elevation Filter', Column=1, tlb_frame_attr=1, xpad=3, ypad=3, space=2)
  
  ; Variables
  infile = dialog_pickfile(filter='*.las', /fix_filter, /multiple_files, /must_exist, title='Please Select LAS File/s (Ctrl-click to select multiple files)', dialog_parent=tlb)
  if (infile[0] EQ '') then return
  infile_bn = FILE_BASENAME(infile)
  infile_dn = FILE_DIRNAME(infile)
  text = WIDGET_LABEL(tlb, value='LAS files selected for filtering:', frame=0, /align_left)
  wTree = WIDGET_TREE(tlb)
  wtRoot = WIDGET_TREE(wTree, VALUE=infile_dn[0], /FOLDER, /EXPANDED)
  for i = 0L, n_elements(infile)-1L, 1L do begin
    wtLeaf = WIDGET_TREE(wtRoot, VALUE=infile_bn[i])
  endfor
  Base1 = widget_base(tlb, column=1, frame=1)
  field1 = FSC_INPUTFIELD(Base1, Title='Initial disk radius (m) : ', LabelSize=200, Value=1.0, /FloatValue, /Positive, LabelAlign=1, Decimal=2)
  text = WIDGET_LABEL(Base1, value=' Unless testing, set equal to the grid spacing.', frame=0, /align_left)
  Base2 = widget_base(tlb, column=1, frame=1)
  field2 = FSC_INPUTFIELD(Base2, Title='Maximum disk radius (m) : ', LabelSize=200, Value=7.0, /FloatValue, /Positive, LabelAlign=1, Decimal=2)
  text = WIDGET_LABEL(Base2, value=' Set to the expected maximum crown radius.', frame=0, /align_left)
  Base3 = widget_base(tlb, column=1, frame=1)
  field3 = FSC_INPUTFIELD(Base3, Title='Initial elevation difference threshold (m) : ', LabelSize=200, Value=0.5, /FloatValue, Decimal=2, /Positive, LabelAlign=1)
  text = WIDGET_LABEL(Base3, value=' Read the paper before changing this (see Help -> About).', frame=0, /align_left)
  Base4 = widget_base(tlb, column=1, frame=1)
  field4 = FSC_INPUTFIELD(Base4, Title='Slope of terrain (%) : ', LabelSize=200, Value=30, /IntegerValue, /Positive, LabelAlign=1)
  text = WIDGET_LABEL(Base4, value=' Just set higher values for more complex terrain.', frame=0, /align_left)
  Base5 = widget_base(tlb, column=1, frame=1)
  field5 = FSC_INPUTFIELD(Base5, Title='Grid spacing (m) : ', LabelSize=200, Value=1.0, /FloatValue, Decimal=2, /Positive, LabelAlign=1)
  text = WIDGET_LABEL(Base5, value=' Set to the average return spacing.', frame=0, /align_left)
  Base6 = widget_base(tlb, column=1, frame=1)
  field6 = FSC_INPUTFIELD(Base6, Title='Error tolerance (m) : ', LabelSize=200, Value=0.15, /FloatValue, Decimal=2, /Positive, LabelAlign=1)
  text = WIDGET_LABEL(Base6, value=' Set >= to the elevation uncertainty.', frame=0, /align_left)
  
  ; Set up TABing between fields.
  field1->SetTabNext, field2->GetTextID()
  field2->SetTabNext, field3->GetTextID()
  field3->SetTabNext, field4->GetTextID()
  field4->SetTabNext, field5->GetTextID()
  field5->SetTabNext, field6->GetTextID()
  field6->SetTabNext, field1->GetTextID()
  
  ; Do the rest
  button = Widget_Button(tlb, Value='Reset parameters to default', UValue='ResetDefaults')
  button = Widget_Button(tlb, Value='Start Filter', UValue='StartFilter')
  button = Widget_Button(tlb, Value='Cancel', UValue='Quit')
  Widget_Control, tlb, /Realize, Set_UValue={ $
    field1:field1, $ ; bstart
    field2:field2, $ ; bmax
    field3:field3, $ ; dh0
    field4:field4, $ ; slope
    field5:field5, $ ; resolution
    field6:field6, $ ; threshold
    infile:infile} ; filename/s
  XManager, 'RSC_LAS_Tools', tlb, /No_Block
  
END


