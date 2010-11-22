;+
; NAME:
;
;   LidarBin_ProfileStatistics_GUI
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

PRO LidarBin_ProfileStatistics_GUI

  FORWARD_FUNCTION FSC_Droplist, FSC_INPUTFIELD
  
  ; Main
  tlb = WIDGET_BASE(TITLE='GeoTIFF Product: Profile Statistics', tlb_frame_attr=1, column=2, xpad=3, ypad=3, space=3)
  
  ; Get the input file/s
  inFile = dialog_pickfile(filter='*.las', /fix_filter, /multiple_files, /must_exist, title='Please Select LAS File/s (Ctrl-click to select multiple files)', dialog_parent=tlb)
  if (inFile[0] EQ '') then return
  nFiles = n_elements(inFile)
  
  ; Create list of return types
  returnList = ['First', $
    'Last', $
    'Singular', $
    'All', $
    'Ground', $
    'Non-ground (All)', $
    'Non-ground (First)']
    
  ; Create list of return units
  unitList = ['Elevation', $
    'Intensity', $
    'Height']
    
  ; Create list of projections
  projList = ['Map Grid Australia 1994', $
    'British National Grid', $
    'UTM (WGS84)']
    
  ; Create list of stats
  statsList = ['Mean', $
    'Variance', $
    'Skewness', $
    'Kurtosis', $
    'Maximum', $
    'Minimum', $
    'Median', $
    'Standard Deviation', $
    'Mean Absolute Deviation', $
    'Range', $
    'Density', $
    'Total', $
    'Number of Returns']
    
  ; Work out file bounds
  xMin = dblarr(nFiles)
  xMax = dblarr(nFiles)
  yMin = dblarr(nFiles)
  yMax = dblarr(nFiles)
  for a = 0L, nFiles-1L do begin
    ReadHeaderLAS, inFile[a], header
    xMin[a] = header.xMin
    xMax[a] = header.xMax
    yMin[a] = header.yMin
    yMax[a] = header.yMax
  endfor
  
  ; Create the widget that will record the user parameters
  infile_bn = FILE_BASENAME(inFile)
  infile_dn = FILE_DIRNAME(inFile)
  text = WIDGET_LABEL(tlb, value='Selected LAS Files (double-click to edit bounds)', frame=0, /align_center)
  wTree = WIDGET_TREE(tlb)
  wtRoot = WIDGET_TREE(wTree, VALUE=infile_dn[0], /FOLDER, /EXPANDED)
  file_bm = replicate(0B,16,16,3)
  for i = 0L, nFiles-1L, 1L do begin
    wtFile = WIDGET_TREE(wtRoot, VALUE=infile_bn[i], /FOLDER, /EXPANDED, BITMAP=file_bm)
    wtLeaf = WIDGET_TREE(wtFile, VALUE='UL Easting : ' + strtrim(string(xMin[i],format='(f10.2)'),2), UVALUE='ChangeBounds')
    wtLeaf = WIDGET_TREE(wtFile, VALUE='UL Northing : ' + strtrim(string(yMax[i],format='(f10.2)'),2), UVALUE='ChangeBounds')
    wtLeaf = WIDGET_TREE(wtFile, VALUE='LR Easting : ' + strtrim(string(xMax[i],format='(f10.2)'),2), UVALUE='ChangeBounds')
    wtLeaf = WIDGET_TREE(wtFile, VALUE='LR Northing : ' + strtrim(string(yMin[i],format='(f10.2)'),2), UVALUE='ChangeBounds')
  endfor
  
  tlb1 = widget_base(tlb, column=1, xsize=!QRSC_LIDAR_XSIZE)
  text1 = WIDGET_LABEL(tlb1, value='Raster Product Settings', frame=0, /align_center)
  Base1 = widget_base(tlb1, column=1, frame=1)
  stats_droplist = FSC_Droplist(Base1, Value=statsList, Index=0, title='Product Type : ')
  resolution = FSC_INPUTFIELD(Base1, Title='Bin size (m) : ', Value=25.0, /FloatValue, /Positive, LabelAlign=1, decimal=2)
  null = FSC_INPUTFIELD(Base1, Title='"No Data" value : ', Value=-1.0, /FloatValue, LabelAlign=1, decimal=2)
  proj_droplist = FSC_Droplist(Base1, Value=projList, Index=0, title='Projection : ')
  zone = FSC_INPUTFIELD(Base1, Title='MGA/UTM Zone : ', Value=55, /IntegerValue, /Positive, LabelAlign=1)
  return_droplist = FSC_Droplist(Base1, Value=returnList, Index=3, title='Return Type : ')
  max_height = FSC_INPUTFIELD(Base1, Title='Maximum value to use : ', Value=1000.0, /FloatValue, LabelAlign=1, decimal=2, /positive)
  height_threshold = FSC_INPUTFIELD(Base1, Title='Minimum value to use : ', Value=0.0, /FloatValue, LabelAlign=1, decimal=2)
  unit_droplist = FSC_Droplist(Base1, Value=unitList, Index=2, title='Variable : ')
  
  tlb6 = widget_base(tlb, column=1,xsize=!QRSC_LIDAR_XSIZE)
  text6 = WIDGET_LABEL(tlb6, value='Output File Information', frame=0, /align_center)
  Base6 = widget_base(tlb6, column=1, frame=1)
  text = WIDGET_LABEL(Base6, value='Output filename is the input with a ".tif" suffix.', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='The statistic and return type is appended to the filename.', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='For example, Input : t1.las', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='Output : t1_FractionalCover_Median_FirstReturns.tif', frame=0, /align_left)
  
  Base7 = widget_base(tlb, column=1,xsize=!QRSC_LIDAR_XSIZE)
  button = Widget_Button(Base7, Value='Start Data Binning', UValue='StartBin_ProfileStatistics')
  button = Widget_Button(Base7, Value='Cancel', UValue='Quit')
  
  ; Do the rest
  Widget_Control, tlb, /Realize, Set_UValue={ $
    proj_droplist:proj_droplist, $ ; projection
    stats_droplist:stats_droplist, $ ; statistic method
    return_droplist:return_droplist, $ ; return type
    xMax:xMax, $ ; x bounds
    yMax:yMax, $ ; y bounds
    xMin:xMin, $ ; x bounds
    yMin:yMin, $ ; y bounds
    null:null, $ ; null value
    zone:zone, $ ; MGA/UTM zone
    unitList:unitList, $ ; unit
    unit_droplist:unit_droplist, $ ; unit
    resolution:resolution, $ ; spatial resolution
    height_threshold:height_threshold, $ ; height threshold
    infile:infile, $ ; filename/s
    statsList:statsList, $ ; Products
    projList:projList, $ ; Projection types
    returnList:returnList, $ ; Return type
    max_height:max_height} ; maximum height to consider
    
  XManager, 'RSC_LAS_Tools', tlb, /No_Block
  
END

