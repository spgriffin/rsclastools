;+
; NAME:
;
;   Ascii2LAS
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

PRO Ascii2LAS, infile, columns, delimiter, limit, skipline, class

  FORWARD_FUNCTION InitHeaderLAS, InitDataLAS
  
  ; Error handling
  catch, theError
  if theError ne 0 then begin
    if (n_elements(progressbar) EQ 1) then progressbar->Destroy
    catch, /cancel
    help, /last_message, output=errText
    errMsg = dialog_message(errText, /error, title='Error importing ASCII')
    return
  endif
  
  ; Set the subset size
  subSize = 1d5
  
  for i = 0L, n_elements(infile) - 1L do begin
  
    ; Initialise the output file
    fparts = strsplit(infile[i], '.', /extract)
    outputFile = fparts[0] + '.las'
    las_data = InitDataLAS(pointFormat=3)
    outputHeader = InitHeaderLAS(pointFormat=3,versionMinor=3)
    outputHeader.systemID = byte('ASCII import')
    outputHeader.xScale = 0.01D
    outputHeader.yScale = 0.01D
    outputHeader.zScale = 0.01D
    outputHeader.xOffset = 0D
    outputHeader.yOffset = 0D
    outputHeader.zOffset = 0D
    outputHeader.pointFormat = 1
    outputHeader.versionMajor = 1
    outputHeader.versionMinor = 2
    WriteLAS, outputFile, outputHeader, /nodata, /check
    openw, outputLun, outputFile, /get_lun, /swap_if_big_endian, /append
    time = 0D
    outputHeader.xMin = 10e6
    outputHeader.yMin = 10e6
    outputHeader.zMin = 10e6
    outputHeader.xMax = -10e6
    outputHeader.yMax = -10e6
    outputHeader.zMax = -10e6
    
    ; Start progress bar
    bcount = 0
    progressBar=Obj_New('progressbar', Color='Forest Green', Text='Converting ASCII to LAS...', title=infile[i], /fast_loop)
    progressBar->Start
    progressBar -> SetProperty, Text=strtrim(infile[i],2)
    
    ; Open the ASCII file
    no_lines = file_lines(infile[i])
    if (no_lines eq 0) then begin
      ok = Dialog_Message(infile[i] + ' has no valid lines.', /error)
      continue
    endif
    openr, lun, infile[i], /get_lun
    if skipline then skip_lun, lun, skipline, /lines
    
    ; Determine # subsets
    nSub  = ceil((no_lines-skipline) / subSize)
    leftSize = (no_lines-skipline) - subSize * (nSub - 1)
    
    ; Read the ASCII file by line
    for j = 0L, nSub-1L do begin
    
      ; Determine number of lines in subset
      if j eq (nSub-1L) then begin
        tempSize = leftSize
      endif else begin
        tempSize = subSize
      endelse
      
      ; Process each line in subset
      for k = 0L, tempSize-1L do begin
      
        ; Initialise record
        dataTemp = ''
        readf, lun, dataTemp
        dataTemp = strtrim(dataTemp,2)
        
        ; Update progress bar
        bcount += 1
        if progressBar->CheckCancel() then begin
          ok=Dialog_Message('ASCII import cancelled')
          progressBar->Destroy
          return
        endif
        progressbar->Update, (float(bcount) / nSub) * 100.0
        
        ; Check the specified data format matches the line
        no_fields = total(columns GE 0)
        lparts = strsplit(dataTemp, delimiter, /extract, count=nparts)
        if (nparts lt 3) then continue
        
        ; Write time stamp of first return
        if (columns[0] eq -1) then begin
          las_data.time = (time + 0.001D)
          offset = 1
        endif else begin
          las_data.time = double(lparts[columns[0]])
          offset = 0
        endelse
        
        ; Write first return
        if (nparts ge no_fields) then begin ; the case when all fields are present for that line
          las_data.x = long(lparts[columns[1]] / 0.01D)
          las_data.y = long(lparts[columns[2]] / 0.01D)
          las_data.z = long(lparts[columns[3]] / 0.01D)
          if (columns[4] ne -1) then las_data.inten= long(lparts[columns[4]])
          singular_test = 0
          outputHeader.xMin <= double(lparts[columns[1]])
          outputHeader.xMax >= double(lparts[columns[1]])
          outputHeader.yMin <= double(lparts[columns[2]])
          outputHeader.yMax >= double(lparts[columns[2]])
          outputHeader.zMin <= double(lparts[columns[3]])
          outputHeader.zMax >= double(lparts[columns[3]])
        endif else begin ; the case for singular returns for a line amongst > 1 return lines.
          if (columns[3] lt columns[7]) then begin ; line order first then last returns
            las_data.x = long(lparts[columns[1]] / 0.01D)
            las_data.y = long(lparts[columns[2]] / 0.01D)
            las_data.z = long(lparts[columns[3]] / 0.01D)
            if (columns[4] ne -1) then las_data.inten = long(lparts[columns[4]])
            outputHeader.xMin <= double(lparts[columns[1]])
            outputHeader.xMax >= double(lparts[columns[1]])
            outputHeader.yMin <= double(lparts[columns[2]])
            outputHeader.yMax >= double(lparts[columns[2]])
            outputHeader.zMin <= double(lparts[columns[3]])
            outputHeader.zMax >= double(lparts[columns[3]])
          endif else begin ; line order last then first returns
            las_data.x = long(lparts[columns[5]] / 0.01D)
            las_data.y = long(lparts[columns[6]] / 0.01D)
            las_data.z = long(lparts[columns[7]] / 0.01D)
            if (columns[8] ne -1) then las_data.inten = long(lparts[columns[8]])
            outputHeader.xMin <= double(lparts[columns[5]])
            outputHeader.xMax >= double(lparts[columns[5]])
            outputHeader.yMin <= double(lparts[columns[6]])
            outputHeader.yMax >= double(lparts[columns[6]])
            outputHeader.zMin <= double(lparts[columns[7]])
            outputHeader.zMax >= double(lparts[columns[7]])
          endelse
          singular_test = 1
        endelse
        
        ; Write classification of it exists
        if (class ne 0) then las_data.class = byte(class)
        
        ; Write return types information
        if ((columns[7] eq -1) or (singular_test eq 1)) then begin ; Single return
          las_data.nreturn = swap_endian(9B, /swap_if_big_endian) ; reads, '00001001', value, format='(B)'
          writeu, outputLun, las_data
          outputHeader.nReturns[0]++
          outputHeader.nPoints++
        endif else begin ; Multiple return
          elev_diff = abs(float(lparts[columns[3]]) - float(lparts[columns[7]]))
          if (elev_diff GE limit) then begin ; Two returns
            las_data.nreturn = swap_endian(17B, /swap_if_big_endian) ; reads, '00010001', value, format='(B)'
            writeu, outputLun, las_data
            outputHeader.nReturns[0]++
            outputHeader.nPoints++
            las_data.x = long(lparts[columns[5]] / 0.01D)
            las_data.y = long(lparts[columns[6]] / 0.01D)
            las_data.z = long(lparts[columns[7]] / 0.01D)
            if (columns[8] ne -1) then las_data.inten = long(lparts[columns[8]])
            las_data.nreturn = swap_endian(18B, /swap_if_big_endian) ; reads, '00010010', value, format='(B)'
            writeu, outputLun, las_data
            outputHeader.nReturns[1]++
            outputHeader.nPoints++
            outputHeader.xMin <= double(lparts[columns[5]])
            outputHeader.xMax >= double(lparts[columns[5]])
            outputHeader.yMin <= double(lparts[columns[6]])
            outputHeader.yMax >= double(lparts[columns[6]])
            outputHeader.zMin <= double(lparts[columns[7]])
            outputHeader.zMax >= double(lparts[columns[7]])
          endif else begin ; Singular return
            las_data.nreturn = swap_endian(9B, /swap_if_big_endian) ; reads, '00001001', value, format='(B)'
            writeu, outputLun, las_data
            outputHeader.nReturns[0]++
            outputHeader.nPoints++
          endelse
        endelse
        
      endfor ; end of line
    endfor ; end of subset
    
    ; Close files and update output file header
    free_lun, lun
    point_lun, outputLun, 0
    writeu, outputLun, outputHeader
    free_lun, outputLun
    
    ; End progress bar
    progressbar->Destroy
    
  endfor
  
  
END
