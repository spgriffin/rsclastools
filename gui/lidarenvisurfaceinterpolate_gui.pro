;+
; NAME:
;
;   LidarENVISurfaceInterpolate_GUI
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

PRO LidarENVISurfaceInterpolate_GUI

  FORWARD_FUNCTION FSC_Droplist, FSC_INPUTFIELD
  
  ; Main
  tlb = WIDGET_BASE(TITLE='ENVI Product: Lidar Surface', tlb_frame_attr=1, column=2, xpad=3, ypad=3, space=3)
  
  ; Get the input file/s
  inFile = dialog_pickfile(filter='*.las', /fix_filter, /multiple_files, /must_exist, title='Please Select LAS File/s (Ctrl-click to select multiple files)', dialog_parent=tlb)
  if (inFile[0] EQ '') then return
  nFiles = n_elements(inFile)
  
  ; Create list of available data products
  projList = ['Map Grid Australia 1994', $
    'British National Grid', $
    'UTM (WGS84)']
    
  ; Create list of interpolation methods
  interpList = ['Nearest Neighbour', $
    'Linear', $
    'Inverse Distance', $
    'Natural Neighbour', $
    'Polynomial Regression']
    
  ; Create list of hemispheres
  hemiList = ['South', $
    'North']
    
  ; Create list of products
  productList = ['Digital Elevation Model', $
    'Intensity Image', $
    'Canopy Height Model']
    
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
  text = WIDGET_LABEL(tlb, value='Selected LAS Files. Check the bounds for errors.', frame=0, /align_center)
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
  product_droplist = FSC_Droplist(Base1, Value=productList, Index=0, title='Surface product : ')
  resolution = FSC_INPUTFIELD(Base1, Title='Spatial resolution (m) : ', Value=1.0, /FloatValue, /Positive, LabelAlign=1, decimal=2)
  null = FSC_INPUTFIELD(Base1, Title='"No Data" Value * : ', Value=-1.0, /FloatValue, LabelAlign=1, decimal=2)
  text = WIDGET_LABEL(Base1, value='* Must be < elevation minimum.', frame=0, /align_left)
  proj_droplist = FSC_Droplist(Base1, Value=projList, Index=0, title='Projection : ')
  zone = FSC_INPUTFIELD(Base1, Title='MGA/UTM Zone : ', Value=55, /IntegerValue, /Positive, LabelAlign=1)
  hemi_droplist = FSC_Droplist(Base1, Value=hemiList, Index=0, title='UTM hemisphere : ')
  tilexsize = FSC_INPUTFIELD(Base1, Title='X tile size (m) : ', Value=100, /IntegerValue, /Positive, LabelAlign=1)
  tileysize = FSC_INPUTFIELD(Base1, Title='Y tile size (m) : ', Value=100, /IntegerValue, /Positive, LabelAlign=1)
  text = WIDGET_LABEL(Base1, value='For selected LAS files : ', frame=0, /align_left)
  fields = ['Create a single surface from all LAS files', 'Create a separate surface for each LAS file']
  surfacetype = cw_bgroup(Base1, fields, column=1, /exclusive, set_value=0)
  text = WIDGET_LABEL(Base1, value='Output file format : ', frame=0, /align_left)
  format_fields = ['ENVI', 'GeoTIFF']
  formats = cw_bgroup(Base1, format_fields, column=2, /exclusive, set_value=0)
  text = WIDGET_LABEL(Base1, value='Warning: Currently writing to GeoTIFF requires all data to be in memory', frame=0, /align_left)
  text2 = WIDGET_LABEL(tlb, value='Interpolation Settings', frame=0, /align_center)
  Base2 = widget_base(tlb, column=1, frame=1)
  method_droplist = FSC_Droplist(Base2, Value=interpList, Index=0, title='Interpolation Method : ')
  text = WIDGET_LABEL(Base2, value='Inverse distance and polynomial regression interpolation settings : ', frame=0, /align_left)
  power = FSC_INPUTFIELD(Base2, Title='Power Parameter : ', Value=2, /IntegerValue, LabelAlign=1)
  smoothing = FSC_INPUTFIELD(Base2, Title='Smoothing Parameter : ', Value=0.0, /FloatValue, LabelAlign=1, decimal=2, /positive)
  min_points = FSC_INPUTFIELD(Base2, Title='# points per local fit : ', Value=6, /IntegerValue, LabelAlign=1, /positive)
  sectors = FSC_INPUTFIELD(Base2, Title='# sectors : ', Value=6, /IntegerValue, LabelAlign=1, /positive)
  Base3 = widget_base(tlb, column=1)
  text = WIDGET_LABEL(Base3, value='Information', frame=0, /align_center)
  Base4 = widget_base(Base3, column=1, frame=1)
  text = WIDGET_LABEL(Base4, value='Surface creation is more memory efficient and faster than previously.', frame=0, /align_left)
  text = WIDGET_LABEL(Base4, value='LAS files are now split into many tiles before interpolation.', frame=0, /align_left)
  text = WIDGET_LABEL(Base4, value='All temporary tiles are stored in the system tmp directory', frame=0, /align_left)
  text = WIDGET_LABEL(Base4, value='It is assumed the geographic bounds in the LAS file headers are correct.', frame=0, /align_left)
  text = WIDGET_LABEL(Base4, value='Only first returns are used for the Intensity Image and Canopy Height Model', frame=0, /align_left)
  text = WIDGET_LABEL(Base4, value='The output filename is : ', frame=0, /align_left)
  text = WIDGET_LABEL(Base4, value='RSCLASTools_TiledSurface_<method>_<product>_<resolution>.', frame=0, /align_left)
  text = WIDGET_LABEL(Base4, value='e.g. RSCLASTools_TiledSurface_NaturalNeighbor_Elevation_0050 (resolution is in cm)', frame=0, /align_left)
  Base5 = widget_base(Base3, column=1)
  text = WIDGET_LABEL(Base5, value='', frame=0, /align_center)
  button = Widget_Button(Base5, Value='Start surface creation', UValue='StartENVISurfaceInterpRaster',xsize=!QRSC_LIDAR_XSIZE)
  button = Widget_Button(Base5, Value='Cancel', UValue='Quit',xsize=!QRSC_LIDAR_XSIZE)
  
  ; Do the rest
  Widget_Control, tlb, /Realize, Set_UValue={ $
    product_droplist:product_droplist, $ ; surface product
    method_droplist:method_droplist, $ ; interpolation method
    hemi_droplist:hemi_droplist, $ ; UTM hemisphere
    proj_droplist:proj_droplist, $ ; projection
    xMax:xMax, $ ; x bounds
    yMax:yMax, $ ; y bounds
    xMin:xMin, $ ; x bounds
    yMin:yMin, $ ; y bounds
    null:null, $ ; null value
    zone:zone, $ ; MGA/UTM zone
    hemiList:hemiList, $ ; UTM hemisphere list
    tilexsize:tilexsize, $ ; tilexsize
    tileysize:tileysize, $ ; tileysize
    power:power, $ ; power value for IDW
    smoothing:smoothing, $ ; smoothing parameter
    min_points:min_points, $ ; total/min points to use
    sectors:sectors, $ ; number of sectors to use
    resolution:resolution, $ ; spatial resolution
    infile:infile, $ ; filename/s
    interpList:interpList, $ ;Interpolation Methods
    productList:productList, $ ;Surface products
    projList:projList, $ ; Projection types
    formats:formats, $ ; file format
    surfacetype:surfacetype} ; Single or separate surfaces
  XManager, 'RSC_LAS_Tools', tlb, /No_Block
  
END


