;+
; NAME:
;
;   LidarPoint_CanopyMetrics_GUI
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

PRO LidarPoint_CanopyMetrics_GUI

  FORWARD_FUNCTION FSC_Droplist, FSC_INPUTFIELD
  
  ; Main
  tlb = WIDGET_BASE(TITLE='ASCII Product: Canopy Metrics', tlb_frame_attr=1, column=2, xpad=3, ypad=3, space=3)
  
  ; Get the input file/s
  inFile = dialog_pickfile(filter='*.las', /fix_filter, /multiple_files, /must_exist, title='Please Select LAS File/s (Ctrl-click to select multiple files)', dialog_parent=tlb)
  if (inFile[0] EQ '') then return
  nFiles = n_elements(inFile)
  
  ; Create list of product types
  productList = ['Fractional Cover', $
    'Height Percentile', $
    'Canopy Openness Index', $
    'Canopy Relief Ratio', $
    'Plant Area Index Proxy', $
    'Density Deciles']
    
  coverList = ['Count Ratio', $
    'Weighted Sum', $
    'Intensity Ratio']
    
  percentileList = ['Counts', $
    'Intensity', $
    'Cover (Counts)']
    
  ; Create list of return types
  returnList = ['First', $
    'Last', $
    'Singular', $
    'All']
    
  ; Create list of rhov rhog methods
  rhovgList = ['Mean / Mean', $
    'Max / Mean', $
    'Percentile /  Mean', $
    'Constant /  Mean', $
    'Constant']
    
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
  return_droplist = FSC_Droplist(Base1, Value=returnList, Index=0, title='Return Type : ')
  text = WIDGET_LABEL(Base1, value='Exclude following classes from non-ground returns : ', frame=0, /align_left)
  fields = ['Water','Buildings']
  excludeTable = cw_bgroup(Base1, fields, column=2, /nonexclusive)
  max_height = FSC_INPUTFIELD(Base1, Title='Maximum Height Used (m) : ', Value=100.0, /FloatValue, LabelAlign=1, decimal=2, /positive)
  
  tlb3 = widget_base(tlb, column=1, xsize=!QRSC_LIDAR_XSIZE)
  text3 = WIDGET_LABEL(tlb3, value='Lidar Index and Fractional Cover Product Settings', frame=0, /align_center)
  Base3 = widget_base(tlb3, column=1, frame=1)
  cover_droplist = FSC_Droplist(Base3, Value=coverList, Index=0, title='Method : ')
  height_threshold = FSC_INPUTFIELD(Base3, Title='Lower (>) Height Threshold (m) : ', Value=0.5, /FloatValue, LabelAlign=1, decimal=2)
  text = WIDGET_LABEL(Base3, value='Lower height threshold is also used as lower bound of Density Deciles', frame=0, /align_left)
  height_threshold_top = FSC_INPUTFIELD(Base3, Title='Upper (<=) Height Threshold (m) : ', Value=0.0, /FloatValue, LabelAlign=1, decimal=2)
  text = WIDGET_LABEL(Base3, value=' Weights for "Weighted Sum" : ', frame=0, /align_left)
  weight_VegGnd = FSC_INPUTFIELD(Base3, Title=' First Returns Only : ', Value=0.50, /FloatValue, LabelAlign=1, decimal=2)
  weight_Double = FSC_INPUTFIELD(Base3, Title=' First & Last Returns : ', Value=0.75, /FloatValue, LabelAlign=1, decimal=2)
  weight_Single = FSC_INPUTFIELD(Base3, Title=' Singular Returns : ', Value=1.00, /FloatValue, LabelAlign=1, decimal=2)
  text = WIDGET_LABEL(Base3, value='Calibration of "Count Ratio" to cover metrics:', frame=0, /align_left)
  text = WIDGET_LABEL(Base3, value='Note: This has only been tested for a range of sites within Queensland', frame=0, /align_left)
  text = WIDGET_LABEL(Base3, value='(Armston et al., 2009).', frame=0, /align_left)
  fields = ['None', 'Plant Projective Cover', 'Foliage Projective Cover']
  metrictype = cw_bgroup(Base3, fields, column=1, /exclusive, set_value=0)
  
  tlb4 = widget_base(tlb, column=1, xsize=!QRSC_LIDAR_XSIZE)
  text4 = WIDGET_LABEL(tlb4, value='Height Percentile Product Settings', frame=0, /align_center)
  Base4 = widget_base(tlb4, column=1, frame=1)
  percentile_droplist = FSC_Droplist(Base4, Value=percentileList, Index=0, title='Method : ')
  height_percentile = FSC_INPUTFIELD(Base4, Title='Height Percentile (0-1) : ', Value=0.95, /FloatValue, LabelAlign=1, decimal=2)
  text = WIDGET_LABEL(Base4, value='Height percentile is also used as upper bound of Density Deciles', frame=0, /align_left)
  text = WIDGET_LABEL(Base4, value='Settings for the Cover (Counts) method : ', frame=0, /align_left)
  vertical_binsize = FSC_INPUTFIELD(Base4, Title='Vertical Bin Size (m) : ', Value=0.15, /FloatValue, LabelAlign=1, decimal=2)
  
  tlb5 = widget_base(tlb, column=1, xsize=!QRSC_LIDAR_XSIZE)
  text5 = WIDGET_LABEL(tlb5, value='Settings Unique to Intensity Derived Products', frame=0, /align_center)
  Base5 = widget_base(tlb5, column=1, frame=1)
  rhovg_droplist = FSC_Droplist(Base5, Value=rhovgList, Index=0, title='Plant/Ground Reflectivity Ratio : ')
  text = WIDGET_LABEL(Base5, value=' The ratio is calculated from the first returns.', frame=0, /align_left)
  text = WIDGET_LABEL(Base5, value=' Be aware this requires a number of assumptions.', frame=0, /align_left)
  rhovg_percentile = FSC_INPUTFIELD(Base5, Title='Plant Intensity Percentile (0-1) : ', Value=0.99, /FloatValue, LabelAlign=1, decimal=2)
  constant = FSC_INPUTFIELD(Base5, Title='Constant Value : ', Value=1.0, /FloatValue, LabelAlign=1, decimal=2)
  text = WIDGET_LABEL(Base5, value=' e.g. A value determined from field spectra or optimisation.', frame=0, /align_left)
  
  tlb6 = widget_base(tlb, column=1,xsize=!QRSC_LIDAR_XSIZE)
  text6 = WIDGET_LABEL(tlb6, value='Output File Information', frame=0, /align_center)
  Base6 = widget_base(tlb6, column=1, frame=1)
  text = WIDGET_LABEL(Base6, value='Output filename is the input with a ".csv" suffix.', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='The product type is appended to the filename.', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='For example, Input : t1.las', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='Output : t1_FractionalCover-Intensity.csv', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='Columns of the comma delimited output file :', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='1. LAS File', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='2. Statistic', frame=0, /align_left)
  text = WIDGET_LABEL(Base6, value='3. Number Of Returns', frame=0, /align_left)
  
  Base7 = widget_base(tlb, column=1,xsize=!QRSC_LIDAR_XSIZE)
  button = Widget_Button(Base7, Value='Start ASCII Products', UValue='StartPoint_CanopyMetrics')
  button = Widget_Button(Base7, Value='Cancel', UValue='Quit')
  
  ; Do the rest
  Widget_Control, tlb, /Realize, Set_UValue={ $
    prod_droplist:prod_droplist, $ ; product
    cover_droplist:cover_droplist, $; cover method
    percentile_droplist:percentile_droplist, $; percentile method
    rhovg_droplist:rhovg_droplist, $ ; rhovg
    return_droplist:return_droplist, $ ; return type
    vertical_binsize:vertical_binsize, $  ; vertical binsize
    null:null, $ ; null value
    infile:infile, $ ; filename/s
    productList:productList, $ ; Products
    coverList:coverList, $ ; Cover methods
    percentileList:percentileList, $ ; Percentile methods
    rhovgList:rhovgList, $ ; rhovg types
    returnList:returnList, $ ; Return type
    constant:constant, $ ; Constant
    max_height:max_height, $ ; maximum height to consider
    rhovg_percentile:rhovg_percentile, $ ; rhovg percentile
    height_threshold:height_threshold, $ ; height threshold
    height_threshold_top:height_threshold_top, $ ; upper height threshold
    height_percentile:height_percentile, $ ; height percentile
    weight_VegGnd:weight_VegGnd, $ ; weight Veg/Gnd
    weight_Double:weight_Double, $ ; weight Double
    weight_Single:weight_Single, $ ; weight Single
    excludeTable:excludeTable, $ ; classes to exclude
    metrictype:metrictype}; cover metric
  XManager, 'RSC_LAS_Tools', tlb, /No_Block
  
END


