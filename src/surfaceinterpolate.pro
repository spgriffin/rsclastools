;+
; NAME:
;
;   SurfaceInterpolate
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

FUNCTION SurfaceInterpolate, tileStruct, col_n, row_n, method, resolution, null, min_points, sectors, smoothing, productType

  ; Keywords
  forward_function NNfix, filterReturns, GetEdgePoints
  
  ; Read tiles
  nTiles = n_elements(tileStruct.name)
  for i = 0L, nTiles-1L, 1L do begin
    if (tileStruct.empty[i] EQ 0) then begin
      cdiff = abs(tileStruct.col[i] - col_n)
      rdiff = abs(tileStruct.row[i] - row_n)
      if (cdiff LE 1 AND rdiff LE 1) then begin
        ReadLAS, tileStruct.name[i], header, data
        all_data = n_elements(all_data) EQ 0 ? temporary(data) : [temporary(all_data), temporary(data)]
      endif
    endif
  endfor
  
  ; Contruct grid for interpolation
  index = where(tileStruct.col EQ col_n AND tileStruct.row EQ row_n)
  half_pixel = resolution / 2D
  ncol = (tileStruct.xMax[index] - tileStruct.xMin[index]) / resolution
  nrow = (tileStruct.yMax[index] - tileStruct.yMin[index]) / resolution
  xout = dindgen(ncol) * resolution + replicate(tileStruct.xMin[index] + half_pixel, ncol)
  yout = dindgen(nrow) * resolution + replicate(tileStruct.yMin[index] + half_pixel, nrow)
  
  ; Do interpolation
  case productType of
    'Elevation': begin
      class = ishft(ishft(all_data.class, 4), -4)
      gindex = where(class EQ 2, gcount)
      if (gcount lt 6) then return, replicate(null,ncol,nrow)
      easting = all_data[gindex].east * header.xScale + header.xOffset
      northing = all_data[gindex].north * header.yScale + header.yOffset
      zvalue = all_data[gindex].elev * header.zScale + header.zOffset
      myDelVar, all_data
      grid_input, easting, northing, zvalue, easting, northing, zvalue, epsilon=half_pixel, duplicates='Min'
    end
    'Intensity': begin
      first = filterReturns(all_data, type=1, n=1)
      findex = where(first EQ 1, fcount)
      if (fcount lt 6) then return, replicate(null,ncol,nrow)
      easting = all_data[findex].east * header.xScale + header.xOffset
      northing = all_data[findex].north * header.yScale + header.yOffset
      zvalue = float(all_data[findex].inten)
      myDelVar, all_data
      grid_input, easting, northing, zvalue, easting, northing, zvalue, epsilon=half_pixel, duplicates='Avg'
    end
    'Height': begin
      first = filterReturns(all_data, type=1, n=1)
      findex = where(first EQ 1, fcount)
      if (fcount lt 6) then return, replicate(null,ncol,nrow)
      easting = all_data[findex].east * header.xScale + header.xOffset
      northing = all_data[findex].north * header.yScale + header.yOffset
      case string(header.systemID) of
        'Height: Source': begin
          zvalue = all_data[findex].(8) * 0.01
        end
        'Height: Elev': begin
          zvalue = all_data[findex].(2) * header.zScale + header.zOffset
        end
      endcase
      myDelVar, all_data
      grid_input, easting, northing, zvalue, easting, northing, zvalue, epsilon=half_pixel, duplicates='Max'
    end
  endcase
  
  ; Do triangulation and work out edge points
  triangulate, easting, northing, triangles, chull, connectivity=clist
  bounds = GetEdgePoints(easting, northing, chull, clist)
  
  case method of
    'NearestNeighbor': begin
      surf = griddata(easting, northing, zvalue, method=method, triangles=triangles, /grid, xout=xout, yout=yout, missing=null)
    end
    'Linear': begin
      surf = griddata(easting, northing, zvalue, method=method, triangles=triangles, /grid, xout=xout, yout=yout, missing=null)
    end
    'InverseDistance': begin
      surf = griddata(easting, northing, zvalue, method=method, triangles=triangles, /grid, xout=xout, yout=yout, missing=null, $
        min_points=min_points, sectors=sectors, smoothing=smoothing, empty_sectors=1)
    end
    'NaturalNeighbor': begin
      surf = griddata(easting, northing, zvalue, method=method, triangles=triangles, /grid, xout=xout, yout=yout, missing=null)
      surf = NNfix(surf, resolution)
    end
    'PolynomialRegression': begin
      surf = griddata(easting, northing, zvalue, power=power, method=method, triangles=triangles, $
        /grid, xout=xout, yout=yout, missing=null, min_points=min_points, sectors=sectors, empty_sectors=1)
    end
  endcase
  
  ; Remove silly interpolations
  mask = where(finite(surf))
  index = where(surf[mask] LT (min(zvalue)-1.0) OR surf[mask] GT (max(zvalue)+1.0), count)
  if (count GT 0) then surf[mask[index]] = null
  
  ; Remove extrapolations
  inside = Obj_New('IDLanROI', easting[bounds], northing[bounds])
  xout_temp = reform(rebin(xout, ncol, nrow, /sample), ncol * nrow)
  yout_temp = reform(transpose(rebin(yout, nrow, ncol, /sample)), ncol * nrow)
  inside_index = inside->ContainsPoints(temporary(xout_temp), temporary(yout_temp))
  null_index = where((temporary(inside_index) NE 1), null_count)
  if (null_count GT 0) then surf[null_index] = null
  Obj_Destroy, inside
  
  ; Return surface
  return, surf
  
END
