;+
; NAME:
;
;   getStatistic
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

FUNCTION getStatistic, data, method, null, area, limits=limits

  ; Error handling
  catch, theError
  if theError ne 0 then begin
    catch, /cancel
    return, null
  endif
  
  ; Determine data subset to use
  if keyword_set(limits) then begin
    index = where(logical_and(data ge limits[0], data le limits[1]), count)
    if (count gt 0) then begin
      data = data[index]
    endif else begin
      return, null
    endelse
  endif
  
  ; Derive metric
  count = n_elements(data)
  case method of
    'Mean': metric = mean(data)
    'Variance': metric = (count gt 1) ? variance(data) : null
    'Skewness': metric = (count gt 1) ? skewness(data) : null
    'Kurtosis': metric = (count gt 1) ? kurtosis(data) : null
    'Minimum': metric = min(data)
    'Maximum': metric = max(data)
    'Mean Absolute Deviation': metric = meanabsdev(data)
    'Standard Deviation': metric = stddev(data)
    'Median': metric = median(data)
    'Range': metric = max(data) - min(data)
    'Density': metric = float(count) / area
    'Total' : metric = float(total(data))
    else: metric = float(count)
  endcase
  
  return, metric
  
END
