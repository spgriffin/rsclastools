;+
; NAME:
;
;   LidarAttributeSplit
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

PRO LidarAttributeSplit, infile, type=type, splitsize=splitsize

  ; Error handling
  forward_function WriteLidarAttributeFile, bsort
  catch, theError
  if theError ne 0 then begin
    if (n_elements(progressbar) EQ 1) then progressbar->Destroy
    catch, /cancel
    help, /last_message, output=errText
    errMsg = dialog_message(errText, /error, title='Error subsetting LAS file.')
    return
  endif
  
  ; Keywords
  if not keyword_set(type) then type = 'Source ID'
  if not keyword_set(splitsize) then splitsize = 1000000D
  name = strcompress(type, /remove_all)
  
  ; Start progress bar
  bcount = 0
  progressBar=Obj_New('progressbar', Color='Forest Green', Text='Subsetting LAS file...', title='Subset by attribute', /fast_loop)
  progressBar->Start
  
  
  for i = 0L, n_elements(infile)-1L do begin
  
    ; Open file
    progressBar -> SetProperty, Text=strtrim(infile[i],2)
    assocLun = 1
    ReadLAS, infile[i], las_header, las_data, assocLun=assocLun
    no_points = las_header.nPoints
    
    ; Work out number of splits
    buffer = splitsize / 10D
    if (splitsize GT (las_header.nPoints + buffer)) then begin
      no_splits = 1
    endif else begin
      no_splits = ceil(las_header.nPoints / splitsize)
    endelse
    if (i eq 0) then begin
      btotala = replicate(no_splits, n_elements(infile))
    endif else begin
      btotala[i] = no_splits
    endelse
    btotal = total(btotala)
    no_sp = round(no_points / no_splits)
    fparts = strsplit(infile[i], '.', /extract)
    
    for j = 0L, no_splits - 1L, 1L do begin
    
      ; Determine split
      case j of
        0: begin
          lower = 0L
          upper = (no_sp - 1L)
        end
        no_splits-1L: begin
          lower = j * no_sp
          upper = no_points - 1L
        end
        else: begin
          lower = j * no_sp
          upper = lower + no_sp
        end
      endcase
      
      ; Read data
      nPoints = upper - lower + 1L
      sp_index = lindgen(nPoints) + lower
      outDataStr = InitDataLAS(pointFormat=las_header.pointFormat)
      outData = replicate(outDataStr, nPoints)
      for k = 0L, nPoints-1L do begin
        outData[k] = las_data[sp_index[k]]
      endfor
      case type of
        'Source ID': begin
          tmpdata = outData.Source
        end
        'Return Number': begin
          tmpdata = ishft(ishft(outData.nreturn,5),-5)
        end
      endcase
      
      ; Write attribute subset
      attributes = tmpdata[uniq(tmpdata, bsort(tmpdata))]
      for k = 0L, n_elements(attributes)-1L, 1L do begin
        outputFile = WriteLidarAttributeFile(infile[i], name, attributes[k])
        index = where(tmpdata eq attributes[k], count)
        openw, outputLun, outputFile, /get_lun, /swap_if_big_endian, /append
        ReadHeaderLas, outputFile, temp_header
        point_lun, outputLun, temp_header.dataOffset
        writeu, outputLun, outData[index]
        temp_header.nPoints += count
        tmp_header_nReturns = temp_header.nReturns
        temp_header.nReturns = histogram(ishft(ishft(outData[index].nReturn, 5), -5), min=1, max=5, input=tmp_header_nReturns)
        temp_header.xMin = (min(outData[index].x) * temp_header.xScale + temp_header.xOffset) < temp_header.xMin
        temp_header.xMax = (max(outData[index].x) * temp_header.xScale + temp_header.xOffset) > temp_header.xMax
        temp_header.yMin = (min(outData[index].y) * temp_header.yScale + temp_header.yOffset) < temp_header.yMin
        temp_header.yMax = (max(outData[index].y) * temp_header.yScale + temp_header.yOffset) > temp_header.yMax
        temp_header.zMin = (min(outData[index].z) * temp_header.zScale + temp_header.zOffset) < temp_header.zMin
        temp_header.zMax = (max(outData[index].z) * temp_header.zScale + temp_header.zOffset) > temp_header.zMax
        point_lun, outputLun, 0
        writeu, outputLun, temp_header
        free_lun, outputLun
      endfor
      
      ; Update progress bar
      bcount += 1
      if progressBar->CheckCancel() then begin
        ok=Dialog_Message('Splitting by attribute cancelled')
        progressBar->Destroy
        return
      endif
      progressbar->Update,(float(bcount)/btotal)*100.0
      
    endfor
    
    ; Close associated las file
    free_lun, assocLun
    
  endfor
  
  ; End progress bar
  progressbar->Destroy
  
END
