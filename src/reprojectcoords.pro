;+
; NAME:
;
;   ReprojectCoords
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

FUNCTION ReprojectCoords, x_coords, y_coords, in_proj, out_proj, zone=zone, hemisphere=hemisphere

  ; Keywords
  compile_opt idl2
  if not keyword_set(zone) then zone = 55
  if not keyword_set(hemisphere) then hemisphere = 'South'
  
  ; Check if anything needs to be done
  if (in_proj eq out_proj) then return, transpose([[x_coords],[y_coords]])
  
  ; Set input projection
  case in_proj of
    'UTM WGS84': begin
      utmzone = (hemisphere eq 'South') ? -zone : zone
      inProjMap = map_proj_init('UTM', datum=8, zone=utmzone)
    end
    'MGA94': begin
      clng = zone * 6 - 183
      inProjMap = map_proj_init('Transverse Mercator', SEMIMAJOR_AXIS=6378137.0, SEMIMINOR_AXIS=6356752.3, MERCATOR_SCALE=0.999600, $
        CENTER_LONGITUDE=clng, FALSE_EASTING=500000.0, FALSE_NORTHING=10000000.0, /GCTP)
    end
    'British National Grid': begin
      inProjMap = map_proj_init('Transverse Mercator', datum=9, CENTER_LONGITUDE=-2,CENTER_LATITUDE=49,FALSE_EASTING=400000.0, FALSE_NORTHING=-100000.0, /GCTP)
    end
    else: begin
      errMsg = dialog_message('Input projection not supported.', /error, title='ReprojectCoords.pro')
      stop
    end
  endcase
  
  ; Convert input coordinates to lat/long
  if (n_elements(inProjMap) gt 0 or in_proj eq 'Geographic') then begin
    if (in_proj ne 'Geographic') then begin
      lnglat = map_proj_inverse(x_coords, y_coords, map_structure=inProjMap)
      lng = reform(lnglat[0,*])
      lat = reform(lnglat[1,*])
    endif else begin
      lng = x_in
      lat = y_in
    endelse
  endif else begin
    errMsg = dialog_message('Input projection not supported.', /error, title='ReprojectCoords.pro')
    stop
  endelse
  
  ; Set output projection
  case out_proj of
    'UTM WGS84': begin
      utmzone = (hemisphere eq 'South') ? -zone : zone
      outProjMap = map_proj_init('UTM', datum=8, zone=utmzone)
      xy_out = map_proj_forward(lng, lat, map_structure=outProjMap)
    end
    'MGA94': begin
      clng = zone * 6 - 183
      outProjMap = map_proj_init('Transverse Mercator', SEMIMAJOR_AXIS=6378137.0, SEMIMINOR_AXIS=6356752.3, MERCATOR_SCALE=0.999600, $
        CENTER_LONGITUDE=clng, FALSE_EASTING=500000.0, FALSE_NORTHING=10000000.0, /GCTP)
      xy_out = map_proj_forward(lng, lat, map_structure=outProjMap)
    end
    'British National Grid': begin
      outProjMap = map_proj_init('Transverse Mercator', datum=9, CENTER_LONGITUDE=-2,CENTER_LATITUDE=49,FALSE_EASTING=400000.0, FALSE_NORTHING=-100000.0, /GCTP)
      xy_out = map_proj_forward(lng, lat, map_structure=outProjMap)
    end
    'Geographic': begin
      xy_out = transpose([[lng],[lat]])
    end
    else: begin
      errMsg = dialog_message('Output projection not supported.', /error, title='ReprojectCoords.pro')
      stop
    end
  endcase
  
  return, xy_out
  
END
