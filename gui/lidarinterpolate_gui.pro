;+
; NAME:
;
;   LidarInterpolate_GUI
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

PRO LidarInterpolate_GUI

  FORWARD_FUNCTION FSC_Droplist, FSC_INPUTFIELD
  
  ; Main
  tlb = WIDGET_BASE(TITLE='GeoTIFF Product: Generic Interpolation', tlb_frame_attr=1, column=2, xpad=3, ypad=3, space=3)
  
  ; Get the input file/s
  inFile = dialog_pickfile(filter='*.las', /fix_filter, /multiple_files, /must_exist, title='Please Select LAS File/s (Ctrl-click to select multiple files)', dialog_parent=tlb)
  if (inFile[0] EQ '') then return
  nFiles = n_elements(inFile)
  
  ; Create list of available data products
  productList = ['Elevation', $
    'Intensity', $
    'Number_of_returns', $
    'Classification', $
    'User_field', $
    'Source_field']
    
  ; Create list of available data products
  projList = ['Map Grid Australia 1994', $
    'British National Grid', $
    'UTM (WGS84)']
    
  ; Create list of interpolation methods
  interpList = ['Nearest Neighbour', $
    'Linear', $
    'Inverse Distance', $
    'Quintic Polynomial', $
    'Natural Neighbour', $
    'Polynomial Regression', $
    'Kriging', $
    'Radial Basis Function']
    
  ; Create list of available returns
  returnList = ['Ground', $
    'Non-ground', $
    'First', $
    'Last', $
    'All']
    
  ; Create list of semivariogram models
  krigingList = ['Linear', $
    'Exponential', $
    'Gaussian', $
    'Spherical']
    
  ; Create list of RBF
  rbf_functionList = ['Inverse Multiquadric', $
    'Multilog', $
    'Multiquadric', $
    'Natural Cubic Spline', $
    'Thin Plate Spline']
    
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
  text1 = WIDGET_LABEL(tlb, value='Raster Settings', frame=0, /align_center)
  Base1 = widget_base(tlb, column=1, frame=1)
  prod_droplist = FSC_Droplist(Base1, Value=productList, Index=0, title='Product Type : ')
  return_droplist = FSC_Droplist(Base1, Value=returnList, Index=0, title='Return Type : ')
  resolution = FSC_INPUTFIELD(Base1, Title='Spatial resolution (m) : ', Value=1.0, /FloatValue, /Positive, LabelAlign=1, decimal=2)
  null = FSC_INPUTFIELD(Base1, Title='"No Data" Value * : ', Value=-1.0, /FloatValue, LabelAlign=1, decimal=2)
  text = WIDGET_LABEL(Base1, value='* Must be < elevation/intensity minimum.', frame=0, /align_left)
  proj_droplist = FSC_Droplist(Base1, Value=projList, Index=0, title='Projection : ')
  zone = FSC_INPUTFIELD(Base1, Title='MGA/UTM Zone : ', Value=55, /IntegerValue, /Positive, LabelAlign=1)
  fields = ['Mask blocks with no data']
  domask = cw_bgroup(Base1, fields, column=1, /nonexclusive)
  maskwin = FSC_INPUTFIELD(Base1, Title='Mask block size: ', Value=21, /IntegerValue, LabelAlign=1, /positive)
  text2 = WIDGET_LABEL(tlb, value='Interpolation Settings', frame=0, /align_center)
  Base2 = widget_base(tlb, column=1, frame=1)
  method_droplist = FSC_Droplist(Base2, Value=interpList, Index=2, title='Interpolation Method : ')
  power = FSC_INPUTFIELD(Base2, Title='Power Parameter * + : ', Value=2, /IntegerValue, LabelAlign=1)
  smoothing = FSC_INPUTFIELD(Base2, Title='Smoothing Parameter * ^ : ', Value=0.0, /FloatValue, LabelAlign=1, decimal=2, /positive)
  min_points = FSC_INPUTFIELD(Base2, Title='# points per local fit * + - ^ : ', Value=6, /IntegerValue, LabelAlign=1, /positive)
  sectors = FSC_INPUTFIELD(Base2, Title='# sectors * + - ^ : ', Value=6, /IntegerValue, LabelAlign=1, /positive)
  kriging_droplist = FSC_Droplist(Base2, Value=krigingList, Index=1, title='Variogram Function - : ')
  function_droplist = FSC_Droplist(Base2, Value=functionList, Index=0, title='Radial Basis Function ^ : ')
  range = FSC_INPUTFIELD(Base2, Title='Range - : ', Value=8.0, /FloatValue, LabelAlign=1, decimal=2, /positive)
  nugget = FSC_INPUTFIELD(Base2, Title='Nugget - : ', Value=0.0, /FloatValue, LabelAlign=1, decimal=2, /positive)
  scale = FSC_INPUTFIELD(Base2, Title='Sill - : ', Value=1.0, /FloatValue, LabelAlign=1, decimal=2, /positive)
  text = WIDGET_LABEL(Base2, value='* Inverse Distance method.', frame=0, /align_left)
  text = WIDGET_LABEL(Base2, value=' + Polynomial regression method.', frame=0, /align_left)
  text = WIDGET_LABEL(Base2, value=' - Kriging method.', frame=0, /align_left)
  text = WIDGET_LABEL(Base2, value=' ^ Radial basis function method.', frame=0, /align_left)
  Base3 = widget_base(tlb, column=1)
  button = Widget_Button(Base3, Value='Start Interpolation', UValue='StartInterpRaster')
  button = Widget_Button(Base3, Value='Cancel', UValue='Quit')
  text = WIDGET_LABEL(Base3, value='Output filename is the input with a ".tif" suffix.', frame=0, /align_left)
  text = WIDGET_LABEL(Base3, value='The product and method are appended to the filename.', frame=0, /align_left)
  text = WIDGET_LABEL(Base3, value='For example, Input : t1.las', frame=0, /align_left)
  text = WIDGET_LABEL(Base3, value='Output : t1_Elevation_InverseDistance.tif', frame=0, /align_left)
  
  ; Do the rest
  Widget_Control, tlb, /Realize, Set_UValue={ $
    prod_droplist:prod_droplist, $ ; product
    method_droplist:method_droplist, $ ; interpolation method
    return_droplist:return_droplist, $ ; return
    kriging_droplist:kriging_droplist, $ ; kriging
    rbf_function_droplist:rbf_function_droplist, $ ; RBF
    proj_droplist:proj_droplist, $ ; projection
    xMax:xMax, $ ; x bounds
    yMax:yMax, $ ; y bounds
    xMin:xMin, $ ; x bounds
    yMin:yMin, $ ; y bounds
    null:null, $ ; null value
    range:range, $ ; variogram range
    nugget:nugget, $ ; nugget range
    scale:scale, $ ; scale range
    zone:zone, $ ; MGA/UTM zone
    power:power, $ ; power value for IDW
    smoothing:smoothing, $ ; smoothing parameter
    min_points:min_points, $ ; total/min points to use
    sectors:sectors, $ ; number of sectors to use
    resolution:resolution, $ ; spatial resolution
    infile:infile, $ ; filename/s
    productList:productList, $ ; Products
    interpList:interpList, $ ;Interpolation Methods
    returnList:returnList, $ ; Return types
    krigingList:krigingList, $ ; Semivariogram types
    domask:domask, $ ; Check to mask
    maskwin:maskwin, $ ; Mask block window
    projList:projList} ; Projection types
  XManager, 'RSC_LAS_Tools', tlb, /No_Block
  
END


