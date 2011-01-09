;+
; NAME:
;
;   LidarPoint
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

PRO LidarPoint, infile, ProductType, StatsType, $
    ReturnType, unit, height_threshold, weights, null, no_obs, percentile, $
    rhovg_method, rhovg_percentile, constant, interval, max_height, metrictype, $
    height_threshold_top=height_threshold_top, vbinsize=vbinsize
    
  forward_function ProfileStatistics, filterReturns, CalibrateCover, CanopyMetric, TerrainMetric
  
  ; Start progress bar
  bcount = 0.0
  btotal = n_elements(infile)
  progressBar=Obj_New('progressbar', Color='Forest Green', Text='Point Product', title='ASCII Product', /fast_loop)
  progressBar->Start
  
  ; Error handling
  catch, theError
  if theError ne 0 then begin
    if (n_elements(progressbar) EQ 1) then progressbar->Destroy
    catch, /cancel
    help, /last_message, output=errText
    errMsg = dialog_message(errText, /error, title='Error calculating point product.')
    return
  endif
  
  ; Initialise output ASCII file
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
  outputDir = file_dirname(infile[0], /mark_directory)
  returnType_str = strcompress(strjoin(strsplit(returnType, '()', /extract)), /remove_all)
  outputFile = outputDir + 'Point_' + strjoin([product_name, returnType_str + 'Returns'], '_') + '.csv'
  openw, lun, outputFile, /get_lun
  printf, lun, '"File","' + product_name + '","No_Returns"'
  
  ; Loop through each file
  for j = 0L, n_elements(infile)-1L do begin
  
    ; Read the input file and get veg heights
    progressBar -> SetProperty, Text = strtrim(infile[j], 2)
    ReadLAS, infile[j], header, data
    if (min(data.class) EQ 0) then begin
      ok = dialog_message(infile[j] + ': Unclassified returns', /error)
      continue
    endif
    if (statsType NE 'Terrain') then begin
      case string(header.systemID) of
        'Height: Source': begin
          height = data.source * 0.01
        end
        'Height: Elev': begin
          height = data.z * header.zScale + header.zOffset
        end
        else: begin
          progressBar->Destroy
          errMsg = dialog_message('Point heights not calculated.', /error, title='LidarPoint.pro')
          return
        end
      endcase
    endif
    if (statsType EQ 'Canopy') then begin
      index = where(height GT max_height, count, complement=noBirds)
      if (count GT 0) then begin
        data = data[noBirds]
        height = height[noBirds]
      endif
    endif
    
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
    
    ; Derive statistic
    case statsType of
      'Canopy': begin
        value = CanopyMetric(data, height, first, last, single, productType, returnType, height_threshold, $
          weights, null, percentile, rhovg_method, rhovg_percentile, constant, interval, height_threshold_top=height_threshold_top, vbinsize=vbinsize)
      end
      'Terrain': begin
        value = TerrainMetric(data[index].z * header.zScale + header.zOffset, $
          data[index].x * header.xScale + header.xOffset, $
          data[index].y * header.yScale + header.yOffset, $
          productType, null)
      end
      else: begin
        if (statsType EQ 'Density') then begin
          ConvexHull, data[index].x * header.xScale + header.xOffset, data[index].y * header.xScale + header.xOffset, px, py
          extent = Obj_New('IDLanROI', px, py)
          status = extent->IDLanROI::ComputeGeometry(area=area)
          if (status EQ 0) then area = null
          Obj_Destroy, extent
        endif else begin
          area = null
        endelse
        if (unit GT 0) then begin
          array = data[index].(unit)
          if (unit EQ 2) then array = temporary(array) * header.zScale + header.zOffset
        endif else begin
          array = height[index]
        endelse
        index = where((array GE height_threshold) AND (array LE max_height), count)
        value =  (count GT 0) ? ProfileStatistics(array[index], StatsType, null, area) : null
      end
    endcase
    
    ; Calibrate Lidar fractional cover if necessary
    if (metrictype GT 0) AND (ProductType EQ 'Fractional Cover - Count Ratio') then begin
      value = CalibrateCover(value, metrictype=metrictype, null=null)
    endif
    
    ; Write the result to ASCII
    line = [file_basename(infile[j]), string(value, format='(f9.3)'), strtrim(count, 2)]
    line = strcompress(strjoin(line, ','), /remove_all)
    printf, lun, line
    
    ; Update progress bar
    bcount += 1
    if progressBar->CheckCancel() then begin
      ok = Dialog_Message('Point product cancelled')
      progressBar->Destroy
      return
    endif
    progressbar->Update, bcount / btotal * 100.0
    
  endfor
  
  ; Close output file
  close, lun
  free_lun, lun
  
  ; End progress bar
  progressbar->Destroy
  
END
