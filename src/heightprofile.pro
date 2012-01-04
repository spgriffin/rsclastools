;+
; NAME:
;
;   HeightProfile
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

FUNCTION HeightProfile, height, intensity, first, null, binsize, height_threshold, rhovg_method, rhovg_percentile, $
    counts, locations, rhov_rhog, constant, count_profile, intensity_profile
    
  forward_function GetRhoVG
  
  ; Error handling
  catch, theError
  if theError ne 0 then begin
    catch, /cancel
    return, null
  endif
  
  ; Index ground and non-ground returns
  gnd_idx = where(height LE 0.0, gnd_cnt, complement=veg_idx, ncomplement=veg_cnt)
  if (gnd_cnt GT 0) then height[gnd_idx] = 0.0
  if (veg_cnt EQ 0) then return, 0.0
  
  ; Bin the height data
  counts = histogram(height[veg_idx], binsize=binsize, locations=locations, min=-binsize, max=ceil(max(height)), reverse_indices=ri)
  
  ; Get the intensity profile
  iProfile = float(counts)
  for i = 0L, n_elements(locations) - 1L, 1L do begin
    iProfile[i] = (ri[i] NE ri[i+1L]) ? total(intensity[veg_idx[ri[ri[i]:ri[i+1L]-1L]]]) : 0.0
  endfor
  
  
  ; ****************** Do count metrics ********************
  
  ; Determine foliage profile metrics if necessary
  ;'"Height","Fractional_Cover","Cum_Pgap","Derivative","Apparant","Relative"'
  nBins = n_elements(locations)
  if arg_present(count_profile) then begin
    count_profile = fltarr(6, nBins)
    ; height
    count_profile[0,*] = locations
    ; fractional cover
    count_profile[1,*] = float(counts) / float(gnd_cnt+veg_cnt)
    ; cumulative GAP FRACTION (1 at top of canopy)
    count_profile[2,*] = 1.0-reverse(total(reverse(reform(count_profile[1,*])), 0, /cumulative, /NAN))
    ; derivative
    count_profile[3,1:nBins-1] = (count_profile[2,1:nBins-1] - count_profile[2,0:nBins-2]) / binsize
    ; apparent
    idx = where(count_profile[2,*] lt 1, cnt, complement=null_idx,ncomplement=null_count)
    count_profile[4,1:nBins-1] = (alog(count_profile[2,1:nBins-1]) - alog(count_profile[2,0:nBins-2])) / binsize
    if (null_count gt 0) then count_profile[4,null_idx] = 0.0
    ;relative
    count_profile[5,*] = count_profile[4,*] / total(count_profile[4,*], /NAN)
  endif
  
  ; Don't go any further if there is no ground returns (required to calculate rhov_rhog)
  if (gnd_cnt EQ 0) then return, null
  
  
  ; ****************** Do intensity metrics ********************
  
  ; Calculate rhov_rhog
  first_idx = where(first EQ 1, first_cnt)
  if (first_cnt GT 0) then begin
    if (rhovg_method EQ 5) then begin
      rhov_rhog = constant
    endif else begin
      rhov_rhog = GetRhoVG(height[first_idx], intensity[first_idx], height_threshold, rhovg_method, rhovg_percentile, constant,null)
    endelse
  endif else begin
    return, null
  endelse
  
  ; Calculate Fractional Cover
  if (rhov_rhog GT 0.0) then begin
    rv0 = total(iProfile) ; Determine total return intensity from the vegetation
    rg = total(intensity[gnd_idx]) ; Determine total return intensity from the ground
    iFC = iProfile / (rv0 + rhov_rhog * rg) ; Determine Fractional Cover
  endif else begin
    return, rhov_rhog
  endelse
  
  ; Determine foliage profile metrics if necessary
  if arg_present(intensity_profile) then begin
    intensity_profile = fltarr(6, nBins)
    ; height
    intensity_profile[0,*] = locations
    ; fractional cover
    intensity_profile[1,*] = iFC
    ; cumulative GAP FRACTION (1 at top of canopy)
    intensity_profile[2,*] = 1.0-reverse(total(reverse(reform(count_profile[1,*])), 0, /cumulative, /NAN))
    ; derivative
    intensity_profile[3,1:nBins-1] = (intensity_profile[2,1:nBins-1] - intensity_profile[2,0:nBins-2]) / binsize
    ; apparent
    idx = where(intensity_profile[2,*] lt 1, cnt, complement=null_idx,ncomplement=null_count)
    intensity_profile[4,1:nBins-1] = (alog(intensity_profile[2,1:nBins-1]) - alog(intensity_profile[2,0:nBins-2])) / binsize
    if (null_count gt 0) then intensity_profile[4,null_idx] = 0.0
    ;relative
    intensity_profile[5,0] = intensity_profile[4,*] / total(intensity_profile[4,*], /NAN)
  endif
  
  ; Return result
  return, iFC
  
  
END
