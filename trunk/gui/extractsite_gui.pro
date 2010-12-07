;+
; NAME:
;
;   ExtractSite_GUI
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

PRO ExtractSite_GUI

  FORWARD_FUNCTION FSC_FileSelect
  
  ; A LIDAR program.
  tlb = Widget_Base(Title='Subset LAS File', Column=1, tlb_frame_attr=1, xpad=3, ypad=3, space=2)
  
  ; Variables
  infile = dialog_pickfile(filter='*.las', /fix_filter, /multiple_files, /must_exist, title='Please Select LAS File/s (Ctrl-click to select multiple files)', dialog_parent=tlb)
  if (infile[0] EQ '') then return
  infile_bn = FILE_BASENAME(infile)
  infile_dn = FILE_DIRNAME(infile)
  text = WIDGET_LABEL(tlb, value='LAS files selected for subsetting :', frame=0, /align_left)
  wTree = WIDGET_TREE(tlb)
  wtRoot = WIDGET_TREE(wTree, VALUE=infile_dn[0], /FOLDER, /EXPANDED)
  for i = 0L, n_elements(infile)-1L, 1L do begin
    wtLeaf = WIDGET_TREE(wtRoot, VALUE=infile_bn[i])
  endfor
  
  tlb1 = widget_base(tlb, column=1, xsize=!QRSC_LIDAR_XSIZE)
  text1 = WIDGET_LABEL(tlb1, value='Shape of subset', frame=0, /align_center)
  Base1 = widget_base(tlb1, column=1, frame=1)
  fields = ['Rectangle', 'Ellipse']
  type = cw_bgroup(Base1, fields, column=1, /exclusive, set_value=1)
  
  tlb2 = widget_base(tlb, column=1, xsize=!QRSC_LIDAR_XSIZE)
  text2 = WIDGET_LABEL(tlb2, value='Subset Locations', frame=0, /align_center)
  Base2 = widget_base(tlb2, column=1, frame=1)
  filenameID = FSC_FileSelect(Base2, ObjectRef=site_file, /MustExist, SelectTitle='Select CSV File : ', filter='*.csv', xSize=25)
  text = WIDGET_LABEL(Base2, value='The format of each line must be : ', frame=0, /align_left)
  text = WIDGET_LABEL(Base2, value='Name, Easting, Northing, Major Axis, Minor Axis, Azimuth', frame=0, /align_left)
  text = WIDGET_LABEL(Base2, value='e.g. gold02, 547612.51, 6965427.00, 50.0, 50.0, 0.0', frame=0, /align_left)
  text = WIDGET_LABEL(Base2, value='Easting, Northing, Major Axis and Minor Axis are in metres.', frame=0, /align_left)
  text = WIDGET_LABEL(Base2, value='Minor Axis = Major Axis * sqrt(1 - Eccentricity^2)', frame=0, /align_left)
  text = WIDGET_LABEL(Base2, value='Azimuth is orientation of the major axis from north (degrees).', frame=0, /align_left)
  
  tlb3 = widget_base(tlb, column=1, xsize=!QRSC_LIDAR_XSIZE)
  text3 = WIDGET_LABEL(tlb3, value='Output File Information', frame=0, /align_center)
  Base3 = widget_base(tlb3, column=1, frame=1)
  text3 = WIDGET_LABEL(Base3, value='Output filename is the input LAS filename with "_<name>" appended.', frame=0, /align_left)
  text3 = WIDGET_LABEL(Base3, value='For example, Input : t1.las', frame=0, /align_left)
  text3 = WIDGET_LABEL(Base3, value='Output : t1_site1.las', frame=0, /align_left)
  text3 = WIDGET_LABEL(Base3, value='Note that all VLR and WDP are not copied to the output files.', frame=0, /align_left)
  
  ; Do the rest
  button = Widget_Button(tlb, Value='Extract Subsets', UValue='StartExtractSite')
  button = Widget_Button(tlb, Value='Cancel', UValue='Quit')
  Widget_Control, tlb, /Realize, Set_UValue={ $
    site_file:site_file, $ ; locations object
    type:type, $ ; shape widget ID
    infile:infile} ; filename/s
  XManager, 'RSC_LAS_Tools', tlb, /No_Block
  
END


