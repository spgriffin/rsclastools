;+
; NAME:
;
;   bin_profilestatistics_start
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

PRO Bin_ProfileStatistics_start, event

  Widget_Control, event.top, Get_UValue=info
  
  ; Open log file
  j_fpart = strtrim(round(systime(/julian)*10), 2)
  openw,  loglun, file_dirname(info.infile[0], /mark_directory) + 'Statistics_Processing_Notes_' + j_fpart + '.txt', /get_lun, width=200
  printf, loglun, 'Input files: ', info.infile
  start_mem = memory(/current)
  t = systime(1)
  
  ; Derive product
  bin_size = info.resolution->Get_Value()
  zone = info.zone->Get_Value()
  null = info.null->Get_Value()
  height_threshold = info.height_threshold->Get_Value()
  max_height = info.max_height->Get_Value()
  case info.proj_droplist->GetSelection() of
    info.projList[0]: proj = 28300L + zone
    info.projList[1]: proj = 27700L
    info.projList[2]: proj = 32700L + zone
  endcase
  ReturnType = info.return_droplist->GetSelection()
  StatsType = info.stats_droplist->GetSelection()
  unit = info.unit_droplist->GetSelection()
  case info.unit_droplist->GetSelection() of
    info.unitList[0]: unit = 2
    info.unitList[1]: unit = 3
    info.unitList[2]: unit = 0
  endcase
  Widget_Control, event.top, /Destroy
  LidarBin, info.infile, info.xMin, info.xMax, info.yMin, info.yMax, 'Statistics', StatsType, $
    ReturnType, unit, bin_size, height_threshold, weights, null, no_obs, height_percentile, rhovg_method, $
    rhovg_percentile, constant, interval, proj, max_height, 0
    
  ; Write to and close log file
  printf, loglun, 'Time required: ', systime(1) - t, ' seconds'
  printf, loglun, 'Memory required: ', memory(/highwater) - start_mem, ' bytes'
  printf, loglun, 'Statistic: ', StatsType
  printf, loglun, 'Bin size: ', bin_size, ' m'
  printf, loglun, 'Return type: ', ReturnType
  free_lun, loglun
  
  ; Make file directory cwd
  cd, file_dirname(info.infile[0])
  
END
