;+
; NAME:
;
;   LidarSplitter
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

PRO LidarSplitter, infile, no_splits, buffer, flag=flag

  ; Start progress bar
  bcount = 0
  btotal = no_splits * n_elements(infile)
  progressBar=Obj_New('progressbar', Color='Forest Green', Text='Splitting LAS file...', title='Temporal Tiling', /fast_loop)
  progressBar->Start
  
  ; Error handling
  catch, theError
  if theError ne 0 then begin
    if (n_elements(progressbar) EQ 1) then progressbar->Destroy
    catch, /cancel
    help, /last_message, output=errText
    errMsg = dialog_message(errText, /error, title='Error splitting LAS file.')
    return
  endif
  
  for i = 0L, n_elements(infile)-1L do begin
  
    progressBar -> SetProperty, Text=strtrim(infile[i],2)
    assocLun = 1
    ReadLAS, infile[i], las_header, las_data, assocLun=assocLun
    no_points = las_header.nPoints
    no_sp = round(no_points / no_splits)
    n_buffer = floor(no_sp * (buffer/100.0))
    fparts = strsplit(infile[i], '.', /extract)
    
    for j = 0L, no_splits - 1L, 1L do begin
    
      outputHeader = las_header
      outputHeader.softwareID = byte('IDL ' + !version.release)
      outputFile = fparts[0] + '_s' + strtrim(string(j+1,format='(I03)'),2) + '.' + fparts[1]
      date = bin_date(systime(/utc))
      day = julday(date[1],date[2],date[0]) - julday(1,1,date[0]) + 1
      outputHeader.day  = uint(day)
      outputHeader.year = uint(date[0])
      
      case j of
        0: begin
          lower = 0L
          upper = (no_sp - 1L) + n_buffer
        end
        no_splits-1L: begin
          lower = j * no_sp - n_buffer
          upper = no_points - 1L
        end
        else: begin
          lower = j * no_sp - n_buffer
          upper = lower + no_sp + 2 * n_buffer
        end
      endcase
      
      nPoints = upper - lower + 1L
      sp_index = lindgen(nPoints) + lower
      outDataStr = InitDataLAS(pointFormat=las_header.pointFormat)
      outData = replicate(outDataStr, nPoints)
      for k = 0L, nPoints-1L do begin
        outData[k] = las_data[sp_index[k]]
      endfor
      
      if keyword_set(flag) then begin
        outData.User = 0
        if (lower gt 0L) then outData[0L:(n_buffer-1L)].User = 1
        if (upper lt (no_points - 1L)) then outData[(nPoints-n_buffer-1L):(nPoints-1L)].User = 1
      endif
      
      outputHeader.nPoints = nPoints
      outputHeader.nReturns = histogram(ishft(ishft(outData.nreturn,5),-5), min=1, max=5)
      outputHeader.xMin = min(outData.(0)) * las_header.xScale + las_header.xOffset
      outputHeader.xMax = max(outData.(0)) * las_header.xScale + las_header.xOffset
      outputHeader.yMin = min(outData.(1)) * las_header.yScale + las_header.yOffset
      outputHeader.yMax = max(outData.(1)) * las_header.yScale + las_header.yOffset
      outputHeader.zMin = min(outData.(2)) * las_header.zScale + las_header.zOffset
      outputHeader.zMax = max(outData.(2)) * las_header.zScale + las_header.zOffset
      if (total(outputHeader.nReturns) NE outputHeader.nPoints) then begin
        outputHeader.nReturns[0] += (outputHeader.nPoints - total(outputHeader.nReturns))
      endif
      
      WriteLAS, outputFile, outputHeader, outData, pointFormat=outputHeader.pointFormat
      
      ; Update progress bar
      bcount += 1
      if progressBar->CheckCancel() then begin
        ok=Dialog_Message('Transect splitting cancelled')
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
