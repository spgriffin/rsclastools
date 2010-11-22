;+
; NAME:
;
;   envisurfaceinterpraster_start
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

PRO ENVISurfaceInterpRaster_start, event

  Widget_Control, event.top, Get_UValue=info
  
  ; Open log file
  j_fpart = strtrim(round(systime(/julian)*10), 2)
  openw,  loglun, file_dirname(info.infile[0], /mark_directory) + 'Surface_LidarInterpolation_Processing_Notes_' + j_fpart + '.txt', /get_lun, width=100
  printf, loglun, 'Input files: ', info.infile
  start_mem = memory(/current)
  t = systime(1)
  
  ; Derive product
  resolution = info.resolution->Get_Value()
  zone = info.zone->Get_Value()
  null = info.null->Get_Value()
  tilexsize = info.tilexsize->Get_Value()
  tileysize = info.tileysize->Get_Value()
  power = info.power->Get_Value()
  smoothing = info.smoothing->Get_Value()
  min_points = info.min_points->Get_Value()
  sectors = info.sectors->Get_Value()
  Widget_Control, info.surfacetype, Get_Value=surfacetype
  case info.hemi_droplist->GetSelection() of
    info.hemiList[0]: hemisphere = 'South'
    info.hemiList[1]: hemisphere = 'North'
  endcase
  case info.method_droplist->GetSelection() of
    ; Create list of interpolation methods
    info.interpList[0]: method = 'NearestNeighbor'
    info.interpList[1]: method = 'Linear'
    info.interpList[2]: method = 'InverseDistance'
    info.interpList[3]: method = 'NaturalNeighbor'
    info.interpList[4]: method = 'PolynomialRegression'
  endcase
  case info.product_droplist->GetSelection() of
    ; Create list of products
    info.productList[0]: productType = 'Elevation'
    info.productList[1]: productType = 'Intensity'
    info.productList[2]: productType = 'Height'
  endcase
  case info.proj_droplist->GetSelection() of
    info.projList[0]: proj = 0
    info.projList[1]: proj = 1
    info.projList[2]: proj = 2
  endcase
  Widget_Control, info.formats, Get_Value=formats
  outFormat = (formats eq 1) ? 'GeoTIFF' : 'ENVI'
  Widget_Control, event.top, /Destroy
  TileInterpolateSurface, info.infile, method=method, resolution=resolution, zone=zone, tilesize=[tilexsize,tileysize], null=null, hemisphere=hemisphere, $
    min_points=min_points, sectors=sectors, smoothing=smoothing, proj=proj, productType=productType, separate=surfacetype, $
    outFormat=outFormat
    
  ; Write to and close log file
  printf, loglun, 'Time required: ', systime(1) - t, ' seconds'
  printf, loglun, 'Memory required: ', memory(/highwater) - start_mem, ' bytes'
  printf, loglun, 'Spatial resolution: ', resolution, ' m'
  printf, loglun, 'Interpolation method: ', method
  free_lun, loglun
  
  ; Make file directory cwd
  cd, file_dirname(info.infile[0])
  
END
