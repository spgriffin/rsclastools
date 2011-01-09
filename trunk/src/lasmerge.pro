;+
; NAME:
;
;   LASmerge
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

PRO LASmerge, inFiles, outFile, new_psid=new_psid, buffer=buffer

  catch, theError
  if theError ne 0 then begin
    catch, /cancel
    help, /last_message, output=errText
    errMsg = dialog_message(errText, /error, title='Error merging points.')
    return
  endif
  
  ; Start progress bar
  bcount = 0L
  progressBar=Obj_New('progressbar', Color='Forest Green', Text=strtrim(inFiles[0],2), title='Merging LAS files...', /fast_loop)
  progressBar->Start
  
  ; Initialise the output file
  ReadHeaderLAS, inFiles[0], outputHeader
  outputHeader.systemID = byte('Merged LAS')
  outputHeader.softwareID = byte('IDL ' + !version.release)
  date = bin_date(systime(/utc))
  day = julday(date[1],date[2],date[0]) - julday(1,1,date[0]) + 1
  outputHeader.day  = uint(day)
  outputHeader.year = uint(date[0])
  outputHeader.xScale = 0.01D
  outputHeader.yScale = 0.01D
  outputHeader.zScale = 0.01D
  outputHeader.xOffset = 0D
  outputHeader.yOffset = 0D
  outputHeader.zOffset = 0D
  outputHeader.nPoints = 0
  WriteLAS, outFile, outputHeader, /nodata
  openw, outputLun, outFile, /get_lun, /swap_if_big_endian, /append
  outputHeader.xMin = 10e6
  outputHeader.yMin = 10e6
  outputHeader.zMin = 10e6
  outputHeader.xMax = -10e6
  outputHeader.yMax = -10e6
  outputHeader.zMax = -10e6
  outputHeader.nRecords = 0
  outputHeader.nReturns = ulonarr(5)
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
  
  ; Process each LAS file
  nFiles = n_elements(inFiles)
  for i = 0L, nFiles-1L, 1L do begin
  
    ; Read in the LAS files (WARNING: memory may be an issue)
    progressBar -> SetProperty, Text=strtrim(inFiles[i],2)
    readLAS, inFiles[i], header, data
    
    ; Remove buffer returns if necessary
    if keyword_set(buffer) then begin
      index = where(data.user eq 0, count)
      if (count gt 0) then data = data[index] else continue
    endif else begin
      count = header.nPoints
    endelse
    
    ; Add a new PSID if necessary
    if keyword_set(new_psid) then data.source = i+1L
    
    ; Write the data
    writeu, outputLun, data
    
    ; Update header
    nReturns = outputHeader.nReturns
    outputHeader.nReturns = histogram([ishft(ishft(data.nreturn,5),-5)], min=1, max=5, input=nReturns)
    outputHeader.nPoints += count
    outputHeader.xMin <= header.xMin
    outputHeader.xMax >= header.xMax
    outputHeader.yMin <= header.yMin
    outputHeader.yMax >= header.yMax
    outputHeader.zMin <= header.zMin
    outputHeader.zMax >= header.zMax
    
    ; Update progress bar
    bcount += 1
    if progressBar->CheckCancel() then begin
      ok=Dialog_Message('LAS merge cancelled')
      progressBar->Destroy
      return
    endif
    progressbar->Update, (float(bcount) / nFiles) * 100.0
    
  endfor
  
  ; Write output header
  if (total(outputHeader.nReturns) NE outputHeader.nPoints) then begin
    outputHeader.nReturns[0] += (outputHeader.nPoints - total(outputHeader.nReturns))
  endif
  point_lun, outputLun, 0
  writeu, outputLun, outputHeader
  free_lun, outputLun
  
  ; End progress bar
  progressbar->Destroy
  
END
