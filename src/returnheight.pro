;+
; NAME:
;
;   ReturnHeight
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

FUNCTION ReturnHeight, data, header, method, null, smoothing, power, min_points, variogram

  ; Error handling
  catch, theError
  if theError ne 0 then begin
    catch, /cancel
    help, /last_message, output=errText
    errMsg = dialog_message(errText, /error, title='Error interpolating return height.')
    return, null
  endif
  
  gnd = filterReturns(data, type=4)
  gnd_idx = where(gnd EQ 1, gnd_cnt, complement=veg_idx, ncomplement=veg_cnt)
  triangulate, data[gnd_idx].(0) * header.xScale + header.xOffset, data[gnd_idx].(1) * header.yScale + header.yOffset, triangles
  
  case method of
    'NearestNeighbor': begin
      outData = griddata(data[gnd_idx].(0) * header.xScale + header.xOffset, data[gnd_idx].(1) * header.yScale + header.yOffset, $
        data[gnd_idx].(2) * header.zScale + header.zOffset, method=method, triangles=triangles, missing=null, $
        xout=data[veg_idx].(0) * header.xScale + header.xOffset, yout=data[veg_idx].(1) * header.yScale + header.yOffset)
    end
    'Linear': begin
      outData = griddata(data[gnd_idx].(0) * header.xScale + header.xOffset, data[gnd_idx].(1) * header.yScale + header.yOffset, $
        data[gnd_idx].(2) * header.zScale + header.zOffset, method=method, triangles=triangles, missing=null, $
        xout=data[veg_idx].(0) * header.xScale + header.xOffset, yout=data[veg_idx].(1) * header.yScale + header.yOffset)
    end
    'InverseDistance': begin
      outData = griddata(data[gnd_idx].(0) * header.xScale + header.xOffset, data[gnd_idx].(1) * header.yScale + header.yOffset, $
        data[gnd_idx].(2) * header.zScale + header.zOffset, power=power, method=method, triangles=triangles, min_points=min_points, missing=null, $
        xout=data[veg_idx].(0) * header.xScale + header.xOffset, yout=data[veg_idx].(1) * header.yScale + header.yOffset, smoothing=smoothing)
    end
    'NaturalNeighbor': begin
      outData = griddata(data[gnd_idx].(0) * header.xScale + header.xOffset, data[gnd_idx].(1) * header.yScale + header.yOffset, $
        data[gnd_idx].(2) * header.zScale + header.zOffset, method=method, triangles=triangles, missing=null, $
        xout=data[veg_idx].(0) * header.xScale + header.xOffset, yout=data[veg_idx].(1) * header.yScale + header.yOffset)
    end
    'PolynomialRegression': begin
      outData = griddata(data[gnd_idx].(0) * header.xScale + header.xOffset, data[gnd_idx].(1) * header.yScale + header.yOffset, $
        data[gnd_idx].(2) * header.zScale + header.zOffset, power=power, method=method, triangles=triangles, min_points=min_points, missing=null, $
        xout=data[veg_idx].(0) * header.xScale + header.xOffset, yout=data[veg_idx].(1) * header.yScale + header.yOffset)
    end
    'Kriging': begin
      outData = griddata(data[gnd_idx].(0) * header.xScale + header.xOffset, data[gnd_idx].(1) * header.yScale + header.yOffset, $
        data[gnd_idx].(2) * header.zScale + header.zOffset, method=method, triangles=triangles, min_points=min_points, missing=null, $
        xout=data[veg_idx].(0) * header.xScale + header.xOffset, yout=data[veg_idx].(1) * header.yScale + header.yOffset, variogram=variogram)
    end
  endcase
  
  height = fltarr(header.nPoints)
  height[veg_idx] = (data[veg_idx].(2) * header.zScale + header.zOffset) - outData
  return, height
  
END
