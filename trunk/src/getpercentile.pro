;+
; NAME:
;
;   GetPercentile
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

FUNCTION GetPercentile, height, percentile, intensity=intensity, binsize=binsize, method=method

  forward_function bsort
  
  ; Error handling
  catch, theError
  if theError ne 0 then begin
    catch, /cancel
    return, null
  endif
  
  count = n_elements(height)
  if (count gt 0) then begin
    s_index = bsort(height)
    s_height = height[s_index]
    case method of
      'Counts': begin
        rank = round(count * percentile)
        value = s_height[rank-1L]
      end
      'Intensity': begin
        s_intensity = intensity[s_index]
        c_intensity = total(s_intensity, /cumulative) / total(s_intensity)
        min_p = min(abs(c_intensity - percentile), idx)
        value = s_height[idx]
      end
      'Cover (Counts)': begin
        vidx = where(height gt 0, vcnt)
        if (vcnt gt 0) then begin
          count_bin = histogram(height[vidx], locations=locations, binsize=binsize, min=0)
          cover_bin = count_bin / float(n_elements(height))
          cum_cover_bin = total(cover_bin, /cumulative) / total(cover_bin)
          value = interpol(locations,cum_cover_bin,percentile)
        endif else begin
          value = 0
        endelse
      end
    endcase
  endif else begin
    value = 0.0
  endelse
  
  return, value
  
END
