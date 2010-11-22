;+
; NAME:
;
;   GetMasterHeader
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

FUNCTION GetMasterHeader, infiles, xbuffer=xbuffer, ybuffer=ybuffer
  
  ; Initialise
  ReadHeaderLas, infiles[0], masterHeader
  masterHeader.xMin = 10e6
  masterHeader.yMin = 10e6
  masterHeader.xMax = -10e6
  masterHeader.yMax = -10e6
  
  ; Read LAS file headers
  for i = 0L, n_elements(infiles)-1L, 1L do begin
    ReadHeaderLas, infiles[i], tempHeader
    masterHeader.xMin = tempHeader.xMin < masterHeader.xMin
    masterHeader.yMin = tempHeader.yMin < masterHeader.yMin
    masterHeader.xMax = tempHeader.xMax > masterHeader.xMax
    masterHeader.yMax = tempHeader.yMax > masterHeader.yMax
  endfor

  ; Buffer
  if keyword_set(xbuffer) then begin
    masterHeader.xMin -= xbuffer
    masterHeader.xMax += xbuffer
  endif
  if keyword_set(ybuffer) then begin
    masterHeader.yMin -= ybuffer
    masterHeader.yMax += ybuffer
  endif
  
  ; Return structure of extent
  return, masterHeader
  
END
