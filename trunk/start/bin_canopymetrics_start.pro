;+
; NAME:
;
;   bin_canopymetrics_start
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

PRO Bin_CanopyMetrics_start, event

  Widget_Control, event.top, Get_UValue=info
  
  ; Open log file
  j_fpart = strtrim(round(systime(/julian)*10), 2)
  openw,  loglun, file_dirname(info.infile[0], /mark_directory) + 'CanopyMetric_Processing_Notes_' + j_fpart + '.txt', /get_lun, width=200
  printf, loglun, 'Input files: ', info.infile
  start_mem = memory(/current)
  t = systime(1)
  
  ; Derive product
  Widget_Control, info.metrictype, Get_Value=metrictype
  bin_size = info.resolution->Get_Value()
  zone = info.zone->Get_Value()
  coverType = info.cover_droplist->GetSelection()
  percentileType = info.percentile_droplist->GetSelection()
  case info.prod_droplist->GetSelection() of
    'Height Percentile': productType = 'Height Percentile - ' + percentileType
    'Fractional Cover': productType = 'Fractional Cover - ' + coverType
    else: productType = info.prod_droplist->GetSelection()
  endcase
  null = info.null->Get_Value()
  constant = info.constant->Get_Value()
  height_threshold = info.height_threshold->Get_Value()
  height_threshold_top = info.height_threshold_top->Get_Value()
  rhovg_percentile = info.rhovg_percentile->Get_Value()
  height_percentile = info.height_percentile->Get_Value()
  vbinsize = info.vertical_binsize->Get_Value()
  weights = [info.weight_Double->Get_Value(),info.weight_Single->Get_Value(),info.weight_VegGnd->Get_Value()]
  max_height = info.max_height->Get_Value()
  case info.proj_droplist->GetSelection() of
    info.projList[0]: proj = 28300L + zone
    info.projList[1]: proj = 27700L
    info.projList[2]: proj = 32700L + zone
  endcase
  case info.rhovg_droplist->GetSelection() of
    info.rhovgList[0]: rhovg_method = 1
    info.rhovgList[1]: rhovg_method = 2
    info.rhovgList[2]: rhovg_method = 3
    info.rhovgList[3]: rhovg_method = 4
    info.rhovgList[4]: rhovg_method = 5
  endcase
  ReturnType = info.return_droplist->GetSelection()
  StatsType = 'Canopy'
  Widget_Control, event.top, /Destroy
  LidarBin, info.infile, info.xMin, info.xMax, info.yMin, info.yMax, ProductType, StatsType, $
    ReturnType, unit, bin_size, height_threshold, weights, null, no_obs, height_percentile, rhovg_method, $
    rhovg_percentile, constant, interval, proj, max_height, metrictype, height_threshold_top=height_threshold_top, $
    vbinsize=vbinsize
    
  ; Write to and close log file
  printf, loglun, 'Time required: ', systime(1) - t, ' seconds'
  printf, loglun, 'Memory required: ', memory(/highwater) - start_mem, ' bytes'
  printf, loglun, 'Product type: ', productType
  printf, loglun, 'Bin size: ', bin_size, ' m'
  printf, loglun, 'Return type: ', ReturnType
  free_lun, loglun
  
  ; Make file directory cwd
  cd, file_dirname(info.infile[0])
  
END
