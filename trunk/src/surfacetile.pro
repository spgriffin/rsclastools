;+
; NAME:
;
;   SurfaceTile
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

FUNCTION SurfaceTile, infiles, tileXsize=tileXsize,tileYsize=tileYsize, splitsize=splitsize, tmp=tmp, resolution=resolution

  ; Keywords
  forward_function InitDataLAS, GetMasterHeader
  if not keyword_set(splitsize) then splitsize = 1000000D
  if not keyword_set(resolution) then resolution = 0.5D
  if not keyword_set(tileXsize) then tileXsize = 100.0
  if not keyword_set(tileYsize) then tileYsize = 100.0
  buffer = splitsize / 10D
  
  ; Get tile parameters
  masterHeader = GetMasterHeader(infiles, xbuffer=tileXsize, ybuffer=tileYsize)
  xrange = masterHeader.xMax - masterHeader.xMin
  yrange = masterHeader.yMax - masterHeader.yMin
  xDiv = tileXsize - (tileXsize MOD resolution)
  yDiv = tileYsize - (tileYsize MOD resolution)
  ncol = ceil(xrange / xDiv)
  nrow = ceil(yrange / yDiv)
  nTiles = ncol * nrow
  
  ; Initialise output tiles
  fparts = strsplit(infiles[0], '.', /extract)
  outputFiles = strarr(nTiles)
  tilexmax = dblarr(nTiles)
  tileymax = dblarr(nTiles)
  tilexmin = dblarr(nTiles)
  tileymin = dblarr(nTiles)
  tileCol = intarr(nTiles)
  tileRow = intarr(nTiles)
  tileEmpty = intarr(nTiles)
  outputHeader = masterHeader
  outputHeader.nPoints = 0
  outputHeader.nReturns = lonarr(5)
  outputHeader.xMin = 10e6
  outputHeader.yMin = 10e6
  outputHeader.zMin = 10e6
  outputHeader.xMax = -10e6
  outputHeader.yMax = -10e6
  outputHeader.zMax = -10e6
  outputHeader.nRecords = 0
  case outputHeader.versionMinor of
    0: begin
      outputHeader.globalEncoding = 0US
      outputHeader.headerSize = 227US
      outputHeader.dataOffset = 227UL
    end
    1: begin
      outputHeader.globalEncoding = 0US
      outputHeader.headerSize = 227US
      outputHeader.dataOffset = 227UL
    end
    2: begin
      outputHeader.globalEncoding = 1US
      outputHeader.headerSize = 227US
      outputHeader.dataOffset = 227UL
    end
    3: begin
      outputHeader.globalEncoding = 128US
      outputHeader.headerSize = 235US
      outputHeader.dataOffset = 235UL
      outputHeader.wdp = 0LL
    end
  endcase
  case outputHeader.pointFormat of
    0: outputHeader.pointLength = 20US
    1: outputHeader.pointLength = 28US
    2: outputHeader.pointLength = 26US
    3: outputHeader.pointLength = 34US
    4: outputHeader.pointLength = 57US
    5: outputHeader.pointLength = 63US
  endcase
  date = bin_date(systime(/utc))
  day = julday(date[1],date[2],date[0]) - julday(1,1,date[0]) + 1
  outputHeader.softwareID = byte('IDL ' + !version.release)
  outputHeader.day  = uint(day)
  outputHeader.year = uint(date[0])
  for i = 0L, nTiles-1L, 1L do begin
    dims = array_indices([ncol,nrow], i, /dimensions)
    outputFiles[i] = fparts[0] + '_' + strtrim(dims[0]+1,2) + '_' + strtrim(dims[1]+1,2) + '.las'
    if keyword_set(tmp) then outputFiles[i] = filepath(file_basename(outputFiles[i]), /tmp)
    tilexmax[i] = xDiv * (dims[0] + 1) + floor(masterHeader.xMin)
    tileymax[i] = yDiv * (dims[1] + 1) + floor(masterHeader.yMin)
    tilexmin[i] = xDiv * dims[0] + floor(masterHeader.xMin)
    tileymin[i] = yDiv * dims[1] + floor(masterHeader.yMin)
    tileCol[i] = dims[0] + 1
    tileRow[i] = dims[1] + 1
    WriteLAS, outputFiles[i], outputHeader, /nodata
  endfor
  
  ; Loop through each las file
  nFiles = n_elements(infiles)
  for k = 0L, nFiles-1L, 1L do begin
  
    ; Read input file
    assocLun = 1
    ReadLAS, infiles[k], las_header, las_data, assocLun=assocLun
    if (splitsize GT (las_header.nPoints + buffer)) then begin
      nSplits = 1
    endif else begin
      nSplits = ceil(las_header.nPoints / splitsize)
    endelse
    
    ; Read the LAS file in chunks
    for i = 0L, nSplits-1L, 1L do begin
    
      ; Get indices
      lower = i * splitsize
      upper = (lower + splitsize - 1UL) < (las_header.nPoints - 1UL)
      
      ; Read the chunk
      nPoints = upper - lower + 1UL
      p_index = lindgen(nPoints) + lower
      tempDataStr = InitDataLAS(pointFormat=las_header.pointFormat)
      tempData = replicate(tempDataStr, nPoints)
      for j = 0L, nPoints-1L do begin
        tempData[j] = las_data[p_index[j]]
      endfor
      
      ; Define which tile each return is in
      tileCoord = ncol * (0L > floor(((tempData.y * las_header.yScale + las_header.yOffset) - floor(masterHeader.yMin)) / yDiv) < (nrow - 1L)) $
        + (0L > floor(((tempData.x * las_header.xScale + las_header.xOffset) - floor(masterHeader.xMin)) / xDiv) < (ncol - 1L))
      data_bin = histogram(tileCoord, min=0L, max=nTiles-1L, reverse_indices=ri)
      
      ; Make sure scaling and offsets are consistent for all tiles
      if (nFiles GT 1) then begin
        tempData.x = long(((tempData.x * las_header.xScale + las_header.xOffset) - outputHeader.xOffset) / outputHeader.xScale)
        tempData.y = long(((tempData.y * las_header.yScale + las_header.yOffset) - outputHeader.yOffset) / outputHeader.yScale)
        tempData.z = long(((tempData.z * las_header.zScale + las_header.zOffset) - outputHeader.zOffset) / outputHeader.zScale)
      endif
      
      ; Write the returns to their correct tile
      for j = 0L, nTiles - 1L, 1L do begin
        if (ri[j] NE ri[j+1L]) then begin
          openw, outputLun, outputFiles[j], /get_lun, /swap_if_big_endian, /append
          writeu, outputLun, tempData[ri[ri[j]:ri[j+1L]-1L]]
          ReadHeaderLas, outputFiles[j], temp_header
          temp_header.nPoints += n_elements(ri[ri[j]:ri[j+1L]-1L])
          nReturns = temp_header.nReturns
          temp_header.nReturns = histogram(reform([ishft(ishft(tempData[ri[ri[j]:ri[j+1L]-1L]].nReturn, 5), -5)]), min=1, max=5, input=nReturns)
          temp_header.xMax = (max(tempData[ri[ri[j]:ri[j+1L]-1L]].x) * outputHeader.xScale + outputHeader.xOffset) > temp_header.xMax
          temp_header.yMax = (max(tempData[ri[ri[j]:ri[j+1L]-1L]].y) * outputHeader.yScale + outputHeader.yOffset) > temp_header.yMax
          temp_header.zMax = (max(tempData[ri[ri[j]:ri[j+1L]-1L]].z) * outputHeader.zScale + outputHeader.zOffset) > temp_header.zMax
          temp_header.xMin = (min(tempData[ri[ri[j]:ri[j+1L]-1L]].x) * outputHeader.xScale + outputHeader.xOffset) < temp_header.xMin
          temp_header.yMin = (min(tempData[ri[ri[j]:ri[j+1L]-1L]].y) * outputHeader.yScale + outputHeader.yOffset) < temp_header.yMin
          temp_header.zMin = (min(tempData[ri[ri[j]:ri[j+1L]-1L]].z) * outputHeader.zScale + outputHeader.zOffset) < temp_header.zMin
          point_lun, outputLun, 0
          writeu, outputLun, temp_header
          free_lun, outputLun
        endif
      endfor
      
    endfor
    
    free_lun, assocLun
    
  endfor
  
  ; Generate file list
  for i = 0L, nTiles-1L, 1L do begin
    readHeaderlas, outputFiles[i], las_header
    finfo = file_info(outputFiles[i])
    ;if (las_header.headerSize eq finfo.size) then begin
    if (las_header.nPoints eq 0) then begin
      tileEmpty[i] = 1
      file_delete, outputFiles[i], /quiet
    endif
  endfor
  
  ; Return structure of necessary tile info
  return, create_struct('name', outputFiles, $
    'col', tileCol, $
    'row', tileRow, $
    'xMin', tilexmin, $
    'yMin', tileymin, $
    'xMax', tilexmax, $
    'yMax', tileymax, $
    'empty', tileEmpty, $
    'ulx', min(tilexmin), $
    'uly', max(tileymax), $
    'lrx', max(tilexmax), $
    'lry', min(tileymin), $
    'ncols', ncol, $
    'nrows', nrow, $
    'nTiles', nTiles, $
    'xScale', outputHeader.xScale, $
    'yScale', outputHeader.yScale, $
    'zScale', outputHeader.zScale, $
    'xOffset', outputHeader.xOffset, $
    'yOffset', outputHeader.yOffset, $
    'zOffset', outputHeader.zOffset $
    )
    
END
