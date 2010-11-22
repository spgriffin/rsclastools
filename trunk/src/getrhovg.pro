;+
; NAME:
;
;   GetRhoVG
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

FUNCTION GetRhoVG, height, intensity, height_threshold, method, percentile, constant, null, rho_v=rho_v, rho_g=rho_g

  ; Error handling
  catch, theError
  if theError ne 0 then begin
    catch, /cancel
    return, null
  endif
  
  ; Return constant if passed
  if (method EQ 5) then begin
    rho_v = null
    rho_g = null
    return, constant
  endif
  
  ; Index the height data
  rhov_index = where(height GE height_threshold, rhov_count)
  rhog_index = where(height EQ 0.0, rhog_count)
  
  if ((rhog_count GT 0) AND (rhov_count GT 0)) then begin
    case method of
      1: begin
        rho_g = mean(intensity[rhog_index])
        rho_v = mean(intensity[rhov_index])
        rhovg = rho_v / rho_g
      end
      2: begin
        rho_g = mean(intensity[rhog_index])
        rho_v = max(intensity[rhov_index])
        rhovg = rho_v / rho_g
      end
      3: begin
        rho_g = mean(intensity[rhog_index])
        intensity_bin = histogram(intensity[rhov_index], locations=locations, binsize=1.0)
        if (n_elements(locations) GE 3) then begin
          intensity_bin_cum = total(intensity_bin, /cumulative) / total(float(intensity_bin))
          rho_v = interpol(locations, intensity_bin_cum, percentile)
        endif else begin
          rho_v = percentile * max(intensity[rhov_index])
        endelse
        rhovg = rho_v / rho_g
      end
      4: begin
        rho_g = mean(intensity[rhog_index])
        rho_v = constant
        rhovg = rho_v / rho_g
      end
      else: return, null
    endcase
  endif
  
  if ((rhog_count EQ 0) AND (rhov_count GT 0)) then rhovg = 1.0
  if ((rhog_count GT 0) AND (rhov_count EQ 0)) then rhovg = 0.0
  if ((rhog_count EQ 0) AND (rhov_count EQ 0)) then rhovg = null
  
  return, rhovg
  
END
