;+
; NAME:
;
;   SurfaceFilter
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

FUNCTION SurfaceFilter, tileStruct, col_n, row_n, resolution=resolution, b_start=b_start, $
    bmax=bmax,dh0=dh0,slope=slope,height_threshold=height_threshold
    
  ; Keywords
  forward_function GroundFilter
  if not keyword_set(resolution) then resolution = 0.5D
  if not keyword_set(b_start) then b_start = 1.0
  if not keyword_set(bmax) then bmax = 7.0
  if not keyword_set(dh0) then dh0 = 0.3
  if not keyword_set(slope) then slope = 0.3
  if not keyword_set(height_threshold) then height_threshold = 0.15
  
  ; Read tiles
  nTiles = n_elements(tileStruct.name)
  for i = 0L, nTiles-1L, 1L do begin
    if (tileStruct.empty[i] EQ 0) then begin
      cdiff = abs(tileStruct.col[i] - col_n)
      rdiff = abs(tileStruct.row[i] - row_n)
      if (cdiff LE 1 AND rdiff LE 1) then begin
        ReadLAS, tileStruct.name[i], header, data
        if (cdiff EQ 0 AND rdiff EQ 0) then begin
          data.User = 1
        endif else begin
          data.User = 0
        endelse
        all_data = n_elements(all_data) EQ 0 ? temporary(data) : [temporary(all_data), temporary(data)]
      endif
    endif
  endfor
  
  ; Do filter
  easting = all_data.x * header.xScale + header.xOffset
  northing = all_data.y * header.yScale + header.yOffset
  elevation = all_data.z * header.zScale + header.zOffset
  class = GroundFilter(easting,northing,elevation,b_start=b_start,bmax=bmax,dh0=dh0, $
    slope=slope,cell_size=resolution,height_threshold=height_threshold)
  all_data.Class = temporary(class)
  
  ; Return classified
  all_data = all_data[where(all_data.User EQ 1)]
  return, all_data
  
END


