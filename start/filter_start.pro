;+
; NAME:
;
;   filter_start
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

PRO Filter_start, event

  Widget_Control, event.top, Get_UValue=info
  
  ; Open log file
  j_fpart = strtrim(round(systime(/julian)*10), 2)
  openw,  loglun, file_dirname(info.infile[0], /mark_directory) + 'ElevationFilter_Processing_Notes_' + j_fpart + '.txt', /get_lun, width=200
  printf, loglun, 'Input files: ', info.infile
  start_mem = memory(/current)
  t = systime(1)
  
  ; Derive product
  dh0 = info.field3->Get_Value()
  slope = info.field4->Get_Value() / 100.0
  resolution = info.field5->Get_Value()
  b_max = round(info.field2->Get_Value() / resolution)
  b_start = round(info.field1->Get_Value() / resolution)
  height_threshold = info.field6->Get_Value()
  Widget_Control, event.top, /Destroy
  LidarFilter, info.infile, dh0, slope, resolution, b_max, b_start, height_threshold
  
  ; Write to and close log file
  printf, loglun, 'Time required: ', systime(1) - t, ' seconds'
  printf, loglun, 'Memory required: ', memory(/highwater) - start_mem, ' bytes'
  printf, loglun, 'Initial elevation difference threshold: ', dh0, ' m'
  printf, loglun, 'Slope: ', slope*100L, '%'
  printf, loglun, 'Grid spacing: ', resolution, ' m'
  printf, loglun, 'Maximum disk radius: ', b_max, ' m'
  printf, loglun, 'Initial disk radius: ', b_start, ' m'
  printf, loglun, 'Error threshold: ', height_threshold, ' m'
  free_lun, loglun
  
  ; Make file directory cwd
  cd, file_dirname(info.infile[0])
  
END
