;+
; NAME:
;
;   LidarBin
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

PRO LidarBin, infile, xMin, xMax, yMin, yMax, ProductType, StatsType, $
    ReturnType, unit, bin_size, height_threshold, weights, null, no_obs, percentile, $
    rhovg_method, rhovg_percentile, constant, interval, proj, max_height, metrictype, $
    height_threshold_top=height_threshold_top, vbinsize=vbinsize
    
  forward_function ProfileStatistics, WriteGeotiff, filterReturns, CalibrateCover, CanopyMetric, TerrainMetric
  
  ; Error handling
  catch, theError
  if theError ne 0 then begin
    if (n_elements(progressbar) EQ 1) then progressBar->Destroy
    catch, /cancel
    help, /last_message, output=errText
    progressbar->Destroy
    errMsg = dialog_message(errText, /error, title='Error calculating data bin metric.')
    return
  endif
  
  ; Loop through each file
  for j = 0L, n_elements(infile)-1L do begin
  
    ; Read the input file and get veg heights
    ReadLAS, infile[j], header, data
    if (min(data.(5)) EQ 0) then begin
      ok = dialog_message(infile[j] + ': Unclassified returns', /error)
      continue
    endif
    if (StatsType NE 'Terrain') then begin
      if (n_elements(unit) eq 0) then unit = 0
      if (unit eq 0) then begin
        case string(header.systemID) of
          'Height: Source': begin
            height = data.(8) * 0.01
          end
          'Height: Elev': begin
            height = data.(2) * header.zScale + header.zOffset
          end
          else: begin
            if (n_elements(progressbar) EQ 1) then progressBar->Destroy
            errMsg = dialog_message('Point heights not calculated.', /error, title='LidarBin.pro')
            return
          end
        endcase
      endif
    endif
    if (statsType EQ 'Canopy') then begin
      index = where(height GT max_height, count, complement=noBirds)
      if (count GT 0) then begin
        data = data[noBirds]
        height = height[noBirds]
      endif
    endif
    
    ; Extent
    bin_min_x = xMin[j]
    bin_max_x = xMax[j]
    bin_min_y = yMin[j]
    bin_max_y = yMax[j]
    
    ; Get return types
    first = filterReturns(data, type=1, n=1)
    single = filterReturns(data, type=7)
    last = filterReturns(data, type=2)
    
    ; Index returns
    case returnType of
      'First': begin
        index = where(first EQ 1, count)
      end
      'Last': begin
        index = where(last EQ 1, count)
      end
      'Singular': begin
        index = where(single EQ 1, count)
      end
      'Ground': begin
        ground = filterReturns(data, type=4)
        index = where(ground EQ 1, count)
      end
      'Non-ground (First)': begin
        non_ground = filterReturns(data, type=5)
        non_gnd_first = non_ground < first
        index = where(non_gnd_first EQ 1, count)
      end
      'Non-ground (All)': begin
        non_ground = filterReturns(data, type=5)
        index = where(non_ground EQ 1, count)
      end
      'First (n>1)': begin
        first_ngt1 = filterReturns(data, type=8, n=1)
        index = where(first_ngt1 EQ 1, count)
      end
      'Last (n>1)': begin
        last_ngt1 = filterReturns(data, type=9)
        index = where(last_ngt1 EQ 1, count)
      end
      'All': begin
        index = dindgen(header.nPoints)
        count = header.nPoints
      end
      else: return
    endcase
    
    ; Bin the data
    no_obs = hist2d_riLidar((data.(0) * header.xScale + header.xOffset), (data.(1) * header.yScale + header.yOffset), $
      ri, n_cols, n_rows, bin1=bin_size, bin2=bin_size, min1=bin_min_x, min2=bin_min_y, max1=bin_max_x, max2=bin_max_y)
    grid = float(no_obs)
    area = float(bin_size)^2
    
    ; Start progress bar
    bcount = 0.0
    btotal = n_cols * n_rows
    progressBar=Obj_New('progressbar', Color='Forest Green', Text=infile[j], title='GeoTIFF Product', /fast_loop)
    progressBar->Start
    
    ; Derive bin statistic
    for i = 0L, n_elements(grid) - 1L do begin
    
      if (ri[i] NE ri[i+1L]) then begin
        case statsType of
          'Canopy': begin
            grid[i] = CanopyMetric(data[ri[ri[i]:ri[i+1L]-1L]], height[ri[ri[i]:ri[i+1L]-1L]], $
              first[ri[ri[i]:ri[i+1L]-1L]], last[ri[ri[i]:ri[i+1L]-1L]], single[ri[ri[i]:ri[i+1L]-1L]], $
              productType, returnType, height_threshold, weights, null, percentile, rhovg_method, rhovg_percentile, constant, $
              interval, height_threshold_top=height_threshold_top, vbinsize=vbinsize)
          end
          'Terrain': begin
            grid[i] = TerrainMetric(data[index[ri[ri[i]:ri[i+1L]-1L]]].(2) * header.zScale + header.zOffset, $
              data[index[ri[ri[i]:ri[i+1L]-1L]]].(0) * header.xScale + header.xOffset, $
              data[index[ri[ri[i]:ri[i+1L]-1L]]].(1) * header.yScale + header.yOffset, $
              productType, null)
          end
          else: begin
            if (unit GT 0) then begin
              array = data[index[ri[ri[i]:ri[i+1L]-1L]]].(unit)
              if (unit EQ 2) then array = temporary(array) * header.zScale + header.zOffset
            endif else begin
              array = height[index[ri[ri[i]:ri[i+1L]-1L]]]
            endelse
            idx = where((array GE height_threshold) AND (array LE max_height), cnt)
            grid[i] =  (cnt GT 0) ? ProfileStatistics(array[idx], StatsType, null, area) : null
          end
        endcase
      endif else begin
        grid[i] = null
      endelse
      
      bcount += 1
      if progressBar->CheckCancel() then begin
        ok = Dialog_Message('Data binning cancelled')
        progressBar->Destroy
        return
      endif
      progressbar->Update, bcount / btotal * 100.0
      
    endfor
    
    ; Calibrate Lidar fractional cover if necessary
    if (metrictype GT 0) AND (ProductType EQ 'Fractional Cover - Count Ratio') then begin
      grid = CalibrateCover(grid, metrictype=metrictype, null=null)
    endif
    
    ; Write the result to geoTIFF
    fparts = strsplit(infile[j], '.', /extract)
    if ((statsType NE 'Canopy') AND (statsType NE 'Terrain')) then begin
      case unit of
        2: unit_name = '_Elevation'
        3: unit_name = '_Intensity'
        else: unit_name = '_Height'
      endcase
      product_name = strcompress(strjoin(strsplit(statsType, '()', /extract)), /remove_all) + unit_name
    endif else begin
      product_name = strcompress(strjoin(strsplit(productType, '()', /extract)), /remove_all)
      p_parts = strsplit(product_name, '-', /extract)
      if (p_parts[0] eq 'HeightPercentile') then begin
        percentile_str = string(percentile*100.0,format='(I03)')
        product_name = percentile_str + product_name
      endif
    endelse
    returnType_str = strcompress(strjoin(strsplit(returnType, '()', /extract)), /remove_all)
    filename = strjoin([fparts[0], product_name, returnType_str + 'Returns'], '_') + '.tif'
    writegeotiff, temporary(rotate(grid,7)), double(xMin[j]), double(yMax[j]), proj, filename, cell_size=double(bin_size)
    
    ; End progress bar
    progressbar->Destroy
    
  endfor
  
END
