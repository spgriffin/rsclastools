;+
; NAME:
;
;   TerrainMetric
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

FUNCTION TerrainMetric, elevation, easting, northing, productType, null

  ; Error handling
  catch, theError
  if theError ne 0 then begin
    catch, /cancel
    return, null
  endif
  
  ; Derive metric
  count = n_elements(elevation)
  metric = null
  case productType of
    'Slope': begin ; Ground slope
      if (count GT 3) then begin
        ;coeff = svdfit(transpose([[easting], [northing]]), 2, elevation, status=status)
        coeff = regress(transpose([[easting], [northing]]), elevation, status=status)
        metric = (status EQ 0) ? atan(sqrt(total(coeff[1]^2, /NAN))) * !radeg : null
      endif else begin
        metric = null
      endelse
    end
    'Aspect': begin; Ground aspect
      if (count GT 3) then begin
        ;coeff = svdfit(transpose([[easting], [northing]]), 2, elevation, status=status)
        coeff = regress(transpose([[easting], [northing]]), elevation, status=status)
        metric = (status EQ 0) ? atan(coeff[1], coeff[2]) * !radeg + ((coeff[1] lt 0.0) * 360.0) : null
      endif else begin
        metric = null
      endelse
    end
    'Local Roughness': begin; Ground local roughness
      if (count GT 3) then begin
        ;coeff = svdfit(transpose([[easting], [northing]]), 2, elevation, status=status, yfit=yfit)
        coeff = regress(transpose([[easting], [northing]]), elevation, status=status, const=const)
        ;metric = (status EQ 0) ? stddev(elevation - yfit) : null
        metric = (status EQ 0) ? stddev(elevation - (const + easting * coeff[0] + northing * coeff[1])) : null
      endif else begin
        metric = null
      endelse
    end
    else: metric = float(count)
  endcase
  
  return, metric
  
END
