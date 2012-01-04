;+
; NAME:
;
;   CheckHeaderLAS
;
; PURPOSE:
;
;   Read the LAS data in blocks and recalculate all the important header values.
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

PRO CheckHeaderLAS, infile, splitsize=splitsize

  ; Keywords
  forward_function InitDataLAS
  if not keyword_set(splitsize) then splitsize = 1e6
  
  ; Initialise things
  assocLun = 1
  ReadLAS, infile, las_header, las_data, assocLun=assocLun
  outputHeader = las_header
  
  ; Update data independent header values
  outputHeader.signature  = byte('LASF')
  outputHeader.softwareID = byte(strjoin(['RSC LAS Tools', !QRSC_LIDAR_VERSION, ', IDL', !version.release], ' '))
  date = bin_date(systime(/utc))
  day = julday(date[1],date[2],date[0]) - julday(1,1,date[0]) + 1
  outputHeader.day  = uint(day)
  outputHeader.year = uint(date[0])
  
  ; Work out number of splits
  buffer = splitsize / 10D
  if (splitsize GT (las_header.nPoints + buffer)) then begin
    no_splits = 1
  endif else begin
    no_splits = ceil(las_header.nPoints / splitsize)
  endelse
  no_sp = round(las_header.nPoints / no_splits)
  
  ; Loop through each split
  outputHeader.nPoints = 0D
  outputHeader.nReturns = ulonarr(5)
  for j = 0L, no_splits - 1L, 1L do begin
  
    case j of
      0: begin
        lower = 0L
        upper = (no_sp - 1L)
      end
      no_splits-1L: begin
        lower = j * no_sp
        upper = las_header.nPoints - 1L
      end
      else: begin
        lower = j * no_sp
        upper = lower + no_sp
      end
    endcase
    
    nPoints = upper - lower + 1L
    sp_index = lindgen(nPoints) + lower
    outDataStr = InitDataLAS(pointFormat=las_header.pointFormat)
    outData = replicate(outDataStr, nPoints)
    for k = 0L, nPoints-1L do begin
      outData[k] = las_data[sp_index[k]]
    endfor
    
    outputHeader.nPoints += nPoints
    nReturns = outputHeader.nReturns
    outputHeader.nReturns = histogram(ishft(ishft(outData.nreturn,5),-5), min=1, max=5, input=nReturns)
    outputHeader.xMin = min(outData.x) * las_header.xScale + las_header.xOffset
    outputHeader.xMax = max(outData.x) * las_header.xScale + las_header.xOffset
    outputHeader.yMin = min(outData.y) * las_header.yScale + las_header.yOffset
    outputHeader.yMax = max(outData.y) * las_header.yScale + las_header.yOffset
    outputHeader.zMin = min(outData.z) * las_header.zScale + las_header.zOffset
    outputHeader.zMax = max(outData.z) * las_header.zScale + las_header.zOffset
    outputHeader.pointLength = n_tags(outData, /data_length)
    if (total(outputHeader.nReturns) NE outputHeader.nPoints) then begin
      outputHeader.nReturns[0] += (outputHeader.nPoints - total(outputHeader.nReturns))
    endif
    
  endfor
  
  ; Check data dependent header values
  fInfo = file_info(infile)
  outputHeader.pointLength = n_tags(outData, /data_length)
  case outputHeader.pointLength of
    20: outputHeader.pointFormat = 0
    28: outputHeader.pointFormat = 1
    26: outputHeader.pointFormat = 2
    34: outputHeader.pointFormat = 3
    57: outputHeader.pointFormat = 4
    63: outputHeader.pointFormat = 5
  endcase
  if (outputHeader.pointFormat ge 4) then begin
    eopData = (outputHeader.wdp gt 0) ? outputHeader.wdp : fInfo.size
  endif else begin
    eopData = fInfo.size
  endelse
  if outputHeader.dataOffset ne (eopData - outputHeader.nPoints * outputHeader.pointLength) then begin
    outputHeader.dataOffset = eopData - outputHeader.nPoints * outputHeader.pointLength
  endif
  
  ; Write new header to the file
  free_lun, assocLun
  openw, outputLun, infile, /get_lun, /swap_if_big_endian, /append
  point_lun, outputLun, 0
  writeu, outputLun, outputHeader
  free_lun, outputLun
  
END
