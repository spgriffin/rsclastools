;+
; NAME:
;
;   interpraster_start
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

PRO InterpRaster_start, event

  Widget_Control, event.top, Get_UValue=info
  
  ; Open log file
  j_fpart = strtrim(round(systime(/julian)*10), 2)
  openw,  loglun, file_dirname(info.infile[0], /mark_directory) + 'LidarInterpolation_Processing_Notes_' + j_fpart + '.txt', /get_lun, width=100
  printf, loglun, 'Input files: ', info.infile
  start_mem = memory(/current)
  t = systime(1)
  
  ; Derive product
  resolution = info.resolution->Get_Value()
  zone = info.zone->Get_Value()
  power = info.power->Get_Value()
  product_type = info.prod_droplist->GetSelection()
  null = info.null->Get_Value()
  smoothing = info.smoothing->Get_Value()
  min_points = info.min_points->Get_Value()
  sectors = info.sectors->Get_Value()
  Widget_Control, info.domask, Get_Value=domask
  maskwin = info.maskwin->Get_Value()
  case product_type of
    info.productList[0]: z = 2
    info.productList[1]: z = 3
    info.productList[2]: z = 4
    info.productList[3]: z = 5
    info.productList[4]: z = 7
    info.productList[5]: z = 8
  endcase
  case info.method_droplist->GetSelection() of
    info.interpList[0]: method = 'NearestNeighbor'
    info.interpList[1]: method = 'Linear'
    info.interpList[2]: method = 'InverseDistance'
    info.interpList[3]: method = 'Quintic'
    info.interpList[4]: method = 'NaturalNeighbor'
    info.interpList[5]: method = 'PolynomialRegression'
    info.interpList[6]: method = 'Kriging'
    info.interpList[7]: method = 'RadialBasisFunction'
  endcase
  case info.proj_droplist->GetSelection() of
    info.projList[0]: proj = 28300L + zone
    info.projList[1]: proj = 27700L
    info.projList[2]: proj = 32700L + zone
  endcase
  case info.kriging_droplist->GetSelection() of
    info.krigingList[0]: type = 1 ; Linear
    info.krigingList[1]: type = 2 ; Exponential
    info.krigingList[2]: type = 3 ; Gaussian
    info.krigingList[3]: type = 4 ; Spherical
  endcase
  variogram = [type, info.range->Get_Value(), info.nugget->Get_Value(), info.scale->Get_Value()]
  return_type = info.return_droplist->GetSelection()
  function_type = info.rbf_function_droplist->GetSelection()
  Widget_Control, event.top, /Destroy
  LidarInterpolate, info.infile, info.returnList, return_type, info.xMax, info.xMin, info.yMax, info.yMin, $
    resolution, power, method, product_type, proj, z, null, smoothing, min_points, sectors, variogram, $
    domask=domask[0], maskwin=maskwin, function_type=function_type
    
  ; Write to and close log file
  printf, loglun, 'Time required: ', systime(1) - t, ' seconds'
  printf, loglun, 'Memory required: ', memory(/highwater) - start_mem, ' bytes'
  printf, loglun, 'Spatial resolution: ', resolution, ' m'
  printf, loglun, 'Interpolation method: ', method
  free_lun, loglun
  
  ; Make file directory cwd
  cd, file_dirname(info.infile[0])
  
END
