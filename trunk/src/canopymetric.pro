;+
; NAME:
;
;   CanopyMetric
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

FUNCTION CanopyMetric, data, height, first, last, single, exclude, productType, returnType, height_threshold, $
    weights, null, percentile, rhovg_method, rhovg_percentile, constant, interval, $
    height_threshold_top=height_threshold_top, vbinsize=vbinsize
    
  forward_function HeightProfile, GetPercentile
  
  ; Error handling
  catch, theError
  if theError ne 0 then begin
    catch, /cancel
    return, null
  endif
  
  ; Index return type to use
  no_obs = n_elements(height)
  case returnType of
    'First': index = where(first EQ 1, count)
    'Last': index = where(last EQ 1, count)
    'Singular': index = where(single EQ 1, count)
    'All': begin
      count = n_elements(height)
      index = lindgen(count)
    end
    else: return, null
  endcase
  
  ; If there are classes to exclude, set them to zero height (treat them as ground)
  ex_idx = where(exclude EQ 1, ex_cnt)
  if (ex_cnt GT 0) then begin
    height[ex_idx] = 0.0
  endif
  
  ; Check for upper height threshold
  if (height_threshold_top le 0.0) then begin
    height_threshold_top_temp = max(height)
  endif else begin
    height_threshold_top_temp = height_threshold_top
  endelse
  if (height_threshold_top_temp le height_threshold) then begin
    height_threshold_top_temp = max(height) > height_threshold
  endif
  
  ; Derive metric
  case productType of
    'Plant Area Index Proxy': begin ; Plant Area Index (Proxy)
      findex = where(first EQ 1, fcount)
      lindex = where(last EQ 1, lcount)
      sindex = where(single EQ 1, scount)
      efe = (fcount GT 0) ? total((Height[findex] GT height_threshold) AND (Height[findex] LE height_threshold_top_temp)) : 0.0
      lfe = (lcount GT 0) ? total((Height[lindex] GT height_threshold) AND (Height[lindex] LE height_threshold_top_temp)) : 0.0
      sfe = (scount GT 0) ? total((Height[sindex] GT height_threshold) AND (Height[sindex] LE height_threshold_top_temp)) : 0.0
      fcover = (count GT 0) ? total((Height[index] GT height_threshold) AND (Height[index] LE height_threshold_top_temp)) / float(count) : 0.0
      metric = ((lfe + sfe) gt 0) ? ((efe / (lfe + sfe)) * fcover) : null
    end
    'Fractional Cover - Count Ratio': begin ; Count Fractional Cover
      metric = (count GT 0) ? total((Height[index] GT height_threshold) AND (Height[index] LE height_threshold_top_temp)) / float(count) : null
    end
    'Fractional Cover - Weighted Sum': begin; Weighted Fractional Cover
      double_veg = total((height GT height_threshold) AND (last EQ 1) AND (single EQ 0))
      single_veg = total((single EQ 1) AND (height GT height_threshold))
      last_idx = where(last EQ 1, last_cnt)
      first_idx = where(first EQ 1, first_cnt)
      if (last_cnt GT 0) then begin
        gnd_veg = total((height[last_idx] LE height_threshold) AND (height[first_idx] GT height_threshold))
      endif else begin
        gnd_veg = 0.0
      endelse
      metric = (weights[0] * double_veg + weights[1] * single_veg + weights[2] * gnd_veg) / float(no_obs)
    end
    'Fractional Cover - Intensity Ratio': begin ; Intensity Fractional Cover
      if (count GT 0) then begin
        iFC = HeightProfile(height[index], data[index].inten, first[index], null, interval, height_threshold, $
          rhovg_method, rhovg_percentile, counts, locations, rhov_rhog, constant)
        if (n_elements(iFC) GT 1) then begin
          iFC = reverse(total(reverse(iFC), /cumulative))
          metric = iFC[where(locations EQ height_threshold)]
        endif else begin
          metric = iFC
        endelse
      endif else begin
        metric = null
      endelse
    end
    'Canopy Openness Index': begin; HCOI
      idx = where(height[index] GT height_threshold, cnt)
      if (count GT 0) then begin
        maxh = max(height[index[idx]])
        n = float(cnt)
        if (maxh GT 0.0) then begin
          value = 1.0 / n * total((replicate(maxh, n) - height[index[idx]]) / maxh)
        endif else begin
          value = 0.0
        endelse
      endif else begin
        value = null
      endelse
    end
    'Canopy Relief Ratio': begin ; Canopy relief ratio
      if (count GT 0) then begin
        idx = where(height[index] GT height_threshold, cnt)
        if (cnt GT 0) then begin
          metric = (mean(height[index[idx]]) - min(height[index[idx]])) / (max(height[index[idx]]) - min(height[index[idx]]))
        endif else begin
          metric = 0.0
        endelse
      endif else begin
        metric = null
      endelse
    end
    'Height Percentile - Counts': begin ; Percentile height (returns)
      if (count GT 0) then begin
        metric = GetPercentile(height[index], percentile, method='Counts')
      endif else begin
        metric = null
      endelse
    end
    'Height Percentile - Intensity': begin ; Percentile height (intensity)
      if (count GT 0) then begin
        metric = GetPercentile(height[index], percentile, intensity=data[index].inten, method='Intensity')
      endif else begin
        metric = null
      endelse
    end
    'Height Percentile - Cover (Counts)': begin ; Percentile height (cover)
      if (count GT 0) then begin
        metric = GetPercentile(height[index], percentile, intensity=data[index].inten, binsize=vbinsize, method='Cover (Counts)')
      endif else begin
        metric = null
      endelse
    end
    'Density Deciles': begin ; Density deciles (for Jackie et al.;maybe useful for biomass prediction)
      if (count GT 0) then begin
        upperHP = GetPercentile(height[index], percentile, method='Counts')
        dBinCounts = histogram(height[index], max=upperHP, min=height_threshold, nbins=10, locations=locations)
        metric = total(dBinsCounts, /cumulative) / float(total(dBinCounts))
      endif else begin
        metric = null
      endelse
    end
    'Fractional Cover Profile': begin ; Vertical cover fraction profile (counts)
      if (count GT 0) then begin
        gnd_idx = where(height[index] le height_threshold, gnd_cnt, complement=veg_idx, ncomplement=veg_cnt)
        if (gnd_cnt gt 0) then height[index[gnd_idx]] = 0.0
        if (veg_cnt gt 0) then begin
          vCounts = histogram(height[index[veg_idx]], binsize=vbinsize, locations=locations, $
            min=0.0, max=ceil(height_threshold_top_temp))
          metric = float(vCounts) / float(gnd_cnt+veg_cnt)
        endif else begin
          nz = ceil((ceil(height_threshold_top_temp)+vbinsize) / vbinsize)
          metric = fltarr(nz)
        endelse
      endif else begin
        metric = null
      endelse
    end
    'Apparent Foliage Profile': begin ; Apparent foliage profile (counts)
      if (count GT 0) then begin
        gnd_idx = where(height[index] le height_threshold, gnd_cnt, complement=veg_idx, ncomplement=veg_cnt)
        if (gnd_cnt gt 0) then height[index[gnd_idx]] = 0.0
        if (veg_cnt gt 0) then begin
          vCounts = histogram(height[index[veg_idx]], binsize=vbinsize, locations=locations, $
            min=0.0, max=ceil(height_threshold_top_temp))
          nz = n_elements(locations)
          coverProfile = float(vCounts) / float(gnd_cnt+veg_cnt) ; Fractional cover profile
          gapProfileCum = 1.0 - reverse(total(reverse(reform(coverProfile)), 0, /cumulative, /NAN)) ; cumulative GAP FRACTION (1 at top of canopy)
          idx = where(gapProfileCum lt 1.0, cnt, complement=null_idx,ncomplement=null_count)
          metric = fltarr(nz)
          metric[1:nz-1] = (alog(gapProfileCum[1:nz-1]) - alog(gapProfileCum[0:nz-2])) / vbinsize ; apparent foliage profile
          if (null_count gt 0) then metric[null_idx] = 0.0
        endif else begin
          nz = ceil((ceil(height_threshold_top_temp)+vbinsize) / vbinsize)
          metric = fltarr(nz)
        endelse
      endif else begin
        metric = null
      endelse
    end
    else: metric = float(count)
  endcase
  
  return, metric
  
END
