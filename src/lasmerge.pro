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
    errMsg = dialog_message(errText, /error, title='Error filtering points.')
    return
  endif
  
  ; Process each LAS file
  for i = 0L, n_elements(inFiles)-1L do begin
  
    ; Read in the LAS files (WARNING: memory may be an issue)
    readLAS, inFiles[i], header, data
    
    ; Remove buffer returns if necessary
    if keyword_set(buffer) then begin
      index = where(data.(7) eq 0, count)
      if (count gt 0) then data = data[index]
    endif
    
    ; Merge the data data
    if keyword_set(new_psid) then data.(8) = i+1L
    all_data = (i eq 0) ? temporary(data) : [temporary(all_data), temporary(data)]
    
  endfor
  
  ; Create the header for the new file (the data has been subsetted so will have different attributes)
  outputHeader = header
  outputHeader.softwareID = byte('IDL ' + !version.release)
  date = bin_date(systime(/utc))
  day = julday(date[1],date[2],date[0]) - julday(1,1,date[0]) + 1
  outputHeader.day  = uint(day)
  outputHeader.year = uint(date[0])
  outputHeader.nPoints = n_elements(all_data.Time)
  outputHeader.nReturns = histogram(ishft(ishft(all_data.nreturn,5),-5), min=1, max=5)
  outputHeader.xMin = min(all_data.(0)) * header.xScale + header.xOffset
  outputHeader.xMax = max(all_data.(0)) * header.xScale + header.xOffset
  outputHeader.yMin = min(all_data.(1)) * header.yScale + header.yOffset
  outputHeader.yMax = max(all_data.(1)) * header.yScale + header.yOffset
  outputHeader.zMin = min(all_data.(2)) * header.zScale + header.zOffset
  outputHeader.zMax = max(all_data.(2)) * header.zScale + header.zOffset
  
  ; Write the data subset (sorted by time) to the new LAS file
  writeLAS, outFile, outputHeader, all_data[bsort(all_data.Time)], pointFormat=outputHeader.pointFormat
  ok = dialog_message('LAS files merged.', /information)
  
END
