;+
; NAME:
;
;   Ascii2LAS_TLS
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

PRO Ascii2LAS_TLS, infile

  FORWARD_FUNCTION InitHeaderLAS, InitDataLAS
  
  ; Error handling
  catch, theError
  if theError ne 0 then begin
    if (n_elements(progressbar) EQ 1) then progressbar->Destroy
    catch, /cancel
    help, /last_message, output=errText
    errMsg = dialog_message(errText, /error, title='Error importing PTS')
    return
  endif
  
  for i = 0L, n_elements(infile) - 1L do begin
  
    ; Start progress bar
    bcount = 0
    progressBar=Obj_New('progressbar', Color='Forest Green', Text='Converting PTS to LAS...', title=infile[i], /fast_loop)
    progressBar->Start
    progressBar -> SetProperty, Text=strtrim(infile[i],2)
    
    ; Initialise the output file
    fparts = strsplit(infile[i], '.', /extract)
    outputFile = fparts[0] + '.las'
    las_data = InitDataLAS(pointFormat=2)
    outputHeader = InitHeaderLAS(pointFormat=2)
    outputHeader.systemID = byte('PTS import')
    outputHeader.xScale = 0.01D
    outputHeader.yScale = 0.01D
    outputHeader.zScale = 0.01D
    outputHeader.xOffset = 0D
    outputHeader.yOffset = 0D
    outputHeader.zOffset = 0D
    outputHeader.pointFormat = 2
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
    
    ; Open the ASCII file
    no_lines = file_lines(infile[i])
    if (no_lines eq 0) then begin
      ok = Dialog_Message(infile[i] + ' has no valid lines.', /error)
      return
    endif
    openr, lun, infile[i], /get_lun
    
    
    ; Read the ASCII file by line
    psid = 0US
    for k = 0L, no_lines-1L, 1L do begin
    
      ; Initialise record
      dataTemp = ''
      readf, lun, dataTemp
      dataTemp = strtrim(dataTemp,2)
      
      ; Check the specified data format matches the line
      lparts = strsplit(dataTemp, ' ', /extract, count=nparts)
      if (nparts eq 1) then begin
        psid += 1US
        continue
      endif
      
      ; Write first return
      las_data.(0) = ulong(lparts[0] / 0.01D)
      las_data.(1) = ulong(lparts[1] / 0.01D)
      las_data.(2) = uint(lparts[2] / 0.01D)
      las_data.(3) = uint(abs(lparts[3]))
      outputHeader.xMin <= double(lparts[0])
      outputHeader.xMax >= double(lparts[0])
      outputHeader.yMin <= double(lparts[1])
      outputHeader.yMax >= double(lparts[1])
      outputHeader.zMin <= double(lparts[2])
      outputHeader.zMax >= double(lparts[2])
      las_data.(4) = swap_endian(9B, /swap_if_big_endian) ; reads, '00001001', value, format='(B)'
      las_data.source = uint(psid)
      las_data.red = uint(lparts[4])
      las_data.green = uint(lparts[5])
      las_data.blue = uint(lparts[6])
      writeu, outputLun, las_data
      outputHeader.nReturns[0]++
      outputHeader.nPoints++
      
      ; Update progress bar
      bcount += 1
      if progressBar->CheckCancel() then begin
        ok=Dialog_Message('PTS import cancelled')
        progressBar->Destroy
        return
      endif
      progressbar->Update, (float(bcount) / no_lines) * 100.0
      
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
