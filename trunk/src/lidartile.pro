;+
; NAME:
;
;   LidarTile
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

PRO LidarTile, infile, no_columns, no_rows

  ; Start progress bar
  bcount = 0
  nTiles = no_rows * no_columns
  btotal = nTiles * n_elements(infile)
  progressBar=Obj_New('progressbar', Color='Forest Green', Text='Tiling LAS file...', title='Spatial Tiling', /fast_loop)
  progressBar->Start
  
  ; Error handling
  catch, theError
  if theError ne 0 then begin
    if (n_elements(progressbar) EQ 1) then progressbar->Destroy
    catch, /cancel
    help, /last_message, output=errText
    errMsg = dialog_message(errText, /error, title='Error tiling LAS file.')
    return
  endif
  
  for i = 0L, n_elements(infile)-1L do begin
  
    progressBar -> SetProperty, Text=strtrim(infile[i],2)
    ReadLAS, infile[i], las_header, las_data, /check
    fparts = strsplit(infile[i], '.', /extract)
    date = bin_date(systime(/utc))
    day = julday(date[1],date[2],date[0]) - julday(1,1,date[0]) + 1
    outputHeader = las_header
    outputHeader.softwareID = byte('IDL ' + !version.release)
    outputHeader.day  = uint(day)
    outputHeader.year = uint(date[0])
    xRange = las_header.xMax - las_header.xMin
    yRange = las_header.yMax - las_header.yMin
    xDiv = xRange / no_columns
    yDiv = yRange / no_rows
    tileCoord = no_columns * (0L > floor(((las_data.(1) * las_header.yScale + las_header.yOffset) - las_header.yMin) / yDiv) < (no_rows - 1L)) $
      + (0L > floor(((las_data.(0)  * las_header.xScale + las_header.xOffset) - las_header.xMin) / xDiv) < (no_columns - 1L))
    data_bin = histogram(tileCoord, min=0L, max=nTiles-1L, reverse_indices=ri)
    
    for j = 0L, n_elements(data_bin) - 1L, 1L do begin
    
      dims = array_indices([no_columns,no_rows], j, /dimensions)
      outputFile = fparts[0] + '_' + strtrim(dims[0]+1,2) + '_' + strtrim(dims[1]+1,2) + '.' + fparts[1]
      if (ri[j] ne ri[j+1L]) then begin
      
        outputHeader.nPoints = data_bin[j]
        outputHeader.nReturns = histogram(ishft(ishft(las_data[ri[ri[j]:ri[j+1L]-1L]].nreturn,5),-5), min=1, max=5)
        outputHeader.xMin = min(las_data[ri[ri[j]:ri[j+1L]-1L]].(0)) * las_header.xScale + las_header.xOffset
        outputHeader.xMax = max(las_data[ri[ri[j]:ri[j+1L]-1L]].(0)) * las_header.xScale + las_header.xOffset
        outputHeader.yMin = min(las_data[ri[ri[j]:ri[j+1L]-1L]].(1)) * las_header.yScale + las_header.yOffset
        outputHeader.yMax = max(las_data[ri[ri[j]:ri[j+1L]-1L]].(1)) * las_header.yScale + las_header.yOffset
        outputHeader.zMin = min(las_data[ri[ri[j]:ri[j+1L]-1L]].(2)) * las_header.zScale + las_header.zOffset
        outputHeader.zMax = max(las_data[ri[ri[j]:ri[j+1L]-1L]].(2)) * las_header.zScale + las_header.zOffset
        outputHeader.pointLength = n_tags(las_data[ri[ri[j]:ri[j+1L]-1L]], /data_length)
        outputHeader.pointFormat = (outputHeader.pointLength EQ 20) ? 0 : 1
        if (total(outputHeader.nReturns) NE outputHeader.nPoints) then begin
          outputHeader.nReturns[0] += (outputHeader.nPoints - total(outputHeader.nReturns))
        endif
        WriteLAS, outputFile, outputHeader, las_data[ri[ri[j]:ri[j+1L]-1L]]
        
        ; Update progress bar
        bcount += 1
        if progressBar->CheckCancel() then begin
          ok=Dialog_Message('Tiling cancelled')
          progressBar->Destroy
          return
        endif
        progressbar->Update,(float(bcount)/btotal)*100.0
        
      endif
      
    endfor
    
  endfor
  
  ; End progress bar
  progressbar->Destroy
  
END
