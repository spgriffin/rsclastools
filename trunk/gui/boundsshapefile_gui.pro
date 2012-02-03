;+
; NAME:
;
;   BoundsShapefile_GUI
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

PRO BoundsShapefile_GUI
  
  compile_opt idl2
  FORWARD_FUNCTION FSC_Droplist, FSC_INPUTFIELD
  
  ; A LIDAR program.
  tlb = Widget_Base(Title='Create Extent KML/SHP file', Column=2, tlb_frame_attr=1, xpad=3, ypad=3, space=2)
  
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
  
  Base1 = widget_base(tlb, column=1, frame=1)
  text = WIDGET_LABEL(Base1, value='Format of output file/s:', frame=0, /align_left)
  fields = ['SHP','KML']
  text = WIDGET_LABEL(Base1, value='Output shapefiles have area and z/y range as attributes.', frame=0, /align_left)
  filetype = cw_bgroup(Base1, fields, column=1, /exclusive, set_value=0)
  text = WIDGET_LABEL(Base1, value='This option avoids reading in all the LAS point data:', frame=0, /align_left)
  boundstype = cw_bgroup(Base1, 'Use extents from LAS header', /NONEXCLUSIVE, column=1, set_value=0)
  text = WIDGET_LABEL(Base1, value='This value determines how many LAS points to read into memory at once:', frame=0, /align_left)
  splitsize = FSC_INPUTFIELD(Base1, Title='Size of LAS point buffer : ', LabelSize=155, Value=1000000, /IntegerValue, /Positive, LabelAlign=1)
  projList = ['MGA94','UTM WGS84','British National Grid','Geographic']
  inproj_droplist = FSC_Droplist(Base1, Value=projList, Index=0, title='Input LAS file projection : ')
  outproj_droplist = FSC_Droplist(Base1, Value=projList, Index=3, title='Output file projection : ')
  text = WIDGET_LABEL(Base1, value='Output projection is always Geographic for KML files.', frame=0, /align_left)
  zone = FSC_INPUTFIELD(Base1, Title='MGA94/UTM zone : ', LabelSize=155, Value=55, /IntegerValue, /Positive, LabelAlign=1)
  hemiList = ['South','North']
  hemi_droplist = FSC_Droplist(Base1, Value=hemiList, Index=0, title='UTM WGS84 hemisphere : ')
  text = WIDGET_LABEL(Base1, value='Output files are named <prefix>_extent.[shp/kml]', frame=0, /align_left)
  text = WIDGET_LABEL(Base1, value='where <prefix> is the input LAS filename', frame=0, /align_left)
  ; Do the rest
  tlb2 = widget_base(tlb, column=1, xsize=!QRSC_LIDAR_XSIZE)
  button = Widget_Button(tlb2, Value='Create extent KML/SHP file', UValue='StartBoundsShapefile')
  button = Widget_Button(tlb2, Value='Cancel', UValue='Quit')
  Widget_Control, tlb, /Realize, Set_UValue={ $
    inproj_droplist:inproj_droplist, $  ; Input projection
    outproj_droplist:outproj_droplist, $ ; Output projection
    zone:zone, $ ; UTM/MGA94 zone
    hemi_droplist:hemi_droplist, $ ; UTM hemisphere
    infile:infile, $ ; LAS file/s
    filetype:filetype, $ ; Output file type
    boundstype:boundstype, $ ; whether to use the header extents or not
    splitsize:splitsize $ ; size of las data segments to read
    }
  XManager, 'RSC_LAS_Tools', tlb, /No_Block
  
END


