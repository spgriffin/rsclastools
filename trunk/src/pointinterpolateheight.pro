;+
; NAME:
;
;   PointInterpolateHeight
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

FUNCTION PointInterpolateHeight, tileStruct, col_n, row_n, method, null, min_points, sectors, smoothing, outputType

  ; Keywords
  compile_opt idl2
  forward_function filterReturns, InitDataLAS, filterReturns
  
  ; Get size of tile neighbourhood
  nTiles = n_elements(tileStruct.name)
  nPointsTotal = 0ULL
  inNeighbourhood = intarr(nTiles)
  inNeighbourhoodIndex = ulon64arr(nTiles)
  for i = 0L, nTiles-1L, 1L do begin
    if (tileStruct.empty[i] EQ 0) then begin
      cdiff = abs(tileStruct.col[i] - col_n)
      rdiff = abs(tileStruct.row[i] - row_n)
      if (cdiff LE 1 AND rdiff LE 1) then begin
        ReadHeaderLas, tileStruct.name[i], temp_header
        inNeighbourhoodIndex[i] = nPointsTotal
        nPointsTotal += temp_header.nPoints
        if (cdiff EQ 0 AND rdiff EQ 0) then begin
          inNeighbourhood[i] = 2
        endif else begin
          inNeighbourhood[i] = 1
        endelse
      endif
    endif
  endfor
  
  ; Read tile neighbourhood
  dataStr = InitDataLAS(pointFormat=temp_header.pointFormat)
  all_data = replicate(dataStr, nPointsTotal)
  in_idx = where(inNeighbourhood gt 0, in_count)
  for i = 0L, in_count-1L, 1L do begin
    ReadLAS, tileStruct.name[in_idx[i]], header, data
    if (inNeighbourhood[in_idx[i]] EQ 2) then begin
      inFile = tileStruct.name[in_idx[i]]
      fparts = strsplit(inFile, '.', /extract)
      outFile = fparts[0] + '_AGH.las'
      cindex = ul64indgen(header.nPoints) + inNeighbourhoodIndex[in_idx[i]]
    endif
    all_data[inNeighbourhoodIndex[in_idx[i]]:inNeighbourhoodIndex[in_idx[i]]+header.nPoints-1ULL] = temporary(data)
  endfor
  
  ; Get locations for interpolation
  nPoints = n_elements(all_data)
  gnd = filterReturns(all_data, type=4)
  gindex = where(gnd EQ 1, gcount, complement=vindex, ncomplement=vcount)
  
  ; Do interpolation
  easting = all_data[gindex].x * header.xScale + header.xOffset
  northing = all_data[gindex].y * header.yScale + header.yOffset
  zdata = all_data[gindex].z * header.zScale + header.zOffset
  grid_input, easting, northing, zdata, easting, northing, zdata
  triangulate, easting, northing, triangles
  
  ; Do the interpolation
  if (vcount gt 0) then begin
    case method of
      'NearestNeighbor': begin
        outData = griddata(easting, northing, $
          zdata, method=method, triangles=triangles, missing=null, $
          xout=all_data[vindex].x * header.xScale + header.xOffset, yout=all_data[veg_idx].y * header.yScale + header.yOffset)
      end
      'Linear': begin
        outData = griddata(easting, northing, $
          zdata, method=method, triangles=triangles, missing=null, $
          xout=all_data[vindex].x * header.xScale + header.xOffset, yout=all_data[veg_idx].y * header.yScale + header.yOffset)
      end
      'InverseDistance': begin
        outData = griddata(easting, northing, $
          zdata, power=power, method=method, triangles=triangles, min_points=min_points, missing=null, sectors=sectors, empty_sectors=1, $
          xout=all_data[vindex].x * header.xScale + header.xOffset, yout=all_data[veg_idx].y * header.yScale + header.yOffset, smoothing=smoothing)
      end
      'NaturalNeighbor': begin
        outData = griddata(easting, northing, $
          zdata, method=method, triangles=triangles, missing=null, $
          xout=all_data[vindex].x * header.xScale + header.xOffset, yout=all_data[vindex].y * header.yScale + header.yOffset)
      end
      'PolynomialRegression': begin
        outData = griddata(easting, northing, $
          zdata, power=power, method=method, triangles=triangles, min_points=min_points, missing=null, sectors=sectors, empty_sectors=1, $
          xout=all_data[vindex].x * header.xScale + header.xOffset, yout=all_data[vindex].y * header.yScale + header.yOffset)
      end
    endcase
    
    ; Calculate height
    height = fltarr(nPoints)
    height[vindex] = (all_data[vindex].z * header.zScale + header.zOffset) - outData
    height = temporary(height) > 0.0
    
  endif else begin
  
    height = fltarr(nPoints)
    
  endelse
  
  ; Sometime crazy interpolations occur at the edges where there is no buffer tile
  ; In these cases set the height value to null
  eindex = where(height GT 150.0, ecount)
  if (ecount GT 0) then height[eindex] = null
  
  ; Write output to file
  ReadLAS, inFile, header, data
  case outputType of
    0: begin ; Point source ID
      data.source = 0L
      data.source = long(height[cindex] / 0.01D)
      header.systemID = 0B
      header.systemID = byte('Height: Source')
    end
    1: begin ; Elevation
      data.z = long(height[cindex] / 0.01D)
      header.systemID = 0B
      header.systemID = byte('Height: Elev')
      header.zMax = max(height[cindex])
      header.zMin = min(height[cindex])
      header.zScale = 0.01D
      header.zOffset = 0D
    end
  endcase
  WriteLAS, outFile, header, data
  
  ; Return AGH tile
  return, outFile
  
END
