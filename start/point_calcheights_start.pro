;+
; NAME:
;
;   point_calcheights_start
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

PRO Point_CalcHeights_start, event

  Widget_Control, event.top, Get_UValue=info
  
  ; Open log file
  j_fpart = strtrim(round(systime(/julian)*10), 2)
  openw,  loglun, file_dirname(info.infile[0], /mark_directory) + 'CalcHeights_Processing_Notes_' + j_fpart + '.txt', /get_lun, width=200
  printf, loglun, 'Input files: ', info.infile
  start_mem = memory(/current)
  t = systime(1)
  
  ; Derive product
  power = info.power->Get_Value()
  null = info.null->Get_Value()
  smoothing = info.smoothing->Get_Value()
  min_points = info.min_points->Get_Value()
  sectors = info.sectors->Get_Value()
  case info.method_droplist->GetSelection() of
    info.interpList[0]: height_method = 'NearestNeighbor'
    info.interpList[1]: height_method = 'Linear'
    info.interpList[2]: height_method = 'InverseDistance'
    info.interpList[3]: height_method = 'NaturalNeighbor'
    info.interpList[4]: height_method = 'PolynomialRegression'
    info.interpList[5]: height_method = 'Kriging'
  endcase
  case info.kriging_droplist->GetSelection() of
    info.krigingList[0]: type = 1 ; Linear
    info.krigingList[1]: type = 2 ; Exponential
    info.krigingList[2]: type = 3 ; Gaussian
    info.krigingList[3]: type = 4 ; Spherical
  endcase
  variogram = [type, info.range->Get_Value(), info.nugget->Get_Value(), info.scale->Get_Value()]
  function_type = info.rbf_function_droplist->GetSelection()
  Widget_Control, info.output_type, Get_Value=output_type
  Widget_Control, event.top, /Destroy
  InterpolateHeight, info.infile, height_method, null, smoothing, power, min_points, sectors, variogram, output_type, function_type=function_type
  
  ; Write to and close log file
  printf, loglun, 'Time required: ', systime(1) - t, ' seconds'
  printf, loglun, 'Memory required: ', memory(/highwater) - start_mem, ' bytes'
  printf, loglun, 'Interpolation method: ', height_method
  free_lun, loglun
  
  ; Make file directory cwd
  cd, file_dirname(info.infile[0])
  
END
