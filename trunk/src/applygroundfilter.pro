;+
; NAME:
;
;   ApplyGroundFilter
;
; PURPOSE:
;
;   Interpolate return elevations to an irregular grid and derive above-ground height (AGH).
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

PRO ApplyGroundFilter, tileStruct, col_n, row_n, b_start, bmax, dh0, $
    slope, resolution, height_threshold
    
  ; Keywords
  compile_opt idl2
  forward_function filterReturns, GroundFilter
  
  ; Read tiles
  nTiles = n_elements(tileStruct.name)
  for i = 0L, nTiles-1L, 1L do begin
    if (tileStruct.empty[i] EQ 0) then begin
      cdiff = abs(tileStruct.col[i] - col_n)
      rdiff = abs(tileStruct.row[i] - row_n)
      if (cdiff LE 1 AND rdiff LE 1) then begin
        ReadLAS, tileStruct.name[i], header, data
        if (cdiff EQ 0 AND rdiff EQ 0) then begin
          outFile = tileStruct.name[i]
          cindex = ulindgen(header.nPoints) + n_elements(all_data)
        endif
        all_data = n_elements(all_data) EQ 0 ? temporary(data) : [temporary(all_data), temporary(data)]
      endif
    endif
  endfor
  
  ; Do the interpolation
  nPoints = n_elements(all_data)
  if (nPoints ge 3) then begin
    class = GroundFilter(all_data.x * header.xScale + header.xOffset,all_data.y * header.yScale + header.yOffset, $
      all_data.z * header.zScale + header.zOffset, b_start=b_start,bmax=bmax,dh0=dh0,slope=slope,cell_size=resolution, $
      height_threshold=height_threshold)
  endif else begin
    class = bytarr(nPoints)
  endelse
  
  ; Write output to file
  ReadLAS, outFile, header, data
  ; For the class field, bits 1-4 are the acctual classification (32 possible values)
  ; So the following should apply
  ;   reads, '00100000', value, format='(B)' - For ground (2) returns
  ;   reads, '00010000', value, format='(B)' - For unclassified (1) returns
  ;   class = ishft(byte(value),-4)
  ; But every LAS file I've ever received, I've had to apply the following
  ;   reads, '00000001', value, format='(B)' - For ground (2) returns
  ;   reads, '00000010', value, format='(B)' - For unclassified (1) returns
  ;   class = ishft(ishft(byte(value),-4),4)
  data.class = class[cindex]
  WriteLAS, outFile, header, data
  
END
