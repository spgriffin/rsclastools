;+
; NAME:
;
;   SurfaceBin
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

FUNCTION SurfaceBin, tileStruct, col_n, row_n, resolution, null, productType, productOptions

  ; Keywords
  forward_function filterReturns, hist_nd, ProfileStatistics, HeightCoverMetric, TerrainMetric
  
  ; Read the data
  tIndex = where(tileStruct.col EQ col_n AND tileStruct.row EQ row_n)
  ReadLAS, tileStruct.name[tIndex[0]], header, data
  
  if (productType ne 'Canopy Metric') then begin
    ; Get classification
    case productOptions.class of
      'Ground': begin
        class = filterReturns(data, type=4)
      end
      'Non-Ground': begin
        class = filterReturns(data, type=5)
      end
      'All': begin
        class = replicate(1, header.nPoints)
      end
    endcase
    ; Get return types
    case productOptions.returnType of
      'First': begin
        first = filterReturns(data, type=1, n=1)
        index = where(first EQ 1 AND class EQ 1, count)
      end
      'Last': begin
        last = filterReturns(data, type=2)
        index = where(last EQ 1 AND class EQ 1, count)
      end
      'Singular': begin
        single = filterReturns(data, type=7)
        index = where(single EQ 1 AND class EQ 1, count)
      end
      'All': begin
        index = where(class EQ 1, count)
      end
    endcase
  endif else begin
    index = ulindgen(header.nPoints)
    count = header.nPoints
  endelse
  
  ; Get data field
  case productOptions.field of
    'Height': begin
      case string(header.systemID) of
        'Height: Source': begin
          field = data.(8) * 0.01
        end
        'Height: Elev': begin
          field = data.(2) * header.zScale + header.zOffset
        end
        else: begin
          errMsg = dialog_message('Point heights not calculated.', /error, title='TileBinSurface.pro')
          stop
        end
      endcase
    end
    'Elevation': field = data.(2) * header.zScale + header.zOffset
    'Intensity': field = data.(3)
  endcase
  
  ; If the tile has the required data...
  if (count gt 0) then begin
  
    ; Contruct grid for binning, input is an NxP array representing P data points in N dimensions
    v = transpose([[data[index].(0) * header.xScale + header.xOffset], [data[index].(1) * header.yScale + header.yOffset]])
    surf = float(hist_nd(reform(v,2,count), resolution, reverse_indices=ri, /omit_upper, $
      min=[tileStruct.xMin[tIndex[0]],tileStruct.yMin[tIndex[0]]], max=[tileStruct.xMax[tIndex[0]],tileStruct.yMax[tIndex[0]]]))
      
    ; Calculate metric
    for i = 0L, n_elements(surf) - 1L, 1L do begin
      if (ri[i] NE ri[i+1L]) then begin
        case productType of
          'Statistic': begin
            area = float(resolution)^2
            surf[i] = ProfileStatistics(field[index[ri[ri[i]:ri[i+1L]-1L]]], productOptions.method, null, area)
          end
          'Canopy Metric': begin
            subfirst = filterReturns(data[index[ri[ri[i]:ri[i+1L]-1L]]], type=1, n=1)
            sublast = filterReturns(data[index[ri[ri[i]:ri[i+1L]-1L]]], type=2)
            subsingle = filterReturns(data[index[ri[ri[i]:ri[i+1L]-1L]]], type=7)
            surf[i] = HeightCoverMetric(field[index[ri[ri[i]:ri[i+1L]-1L]]], intensity=data[index[ri[ri[i]:ri[i+1L]-1L]]].(3), $
              productOptions, first=subfirst, last=sublast, single=subsingle, null=null)
          end
          'Terrain Metric': begin
            surf[i] = TerrainMetric(data[index[ri[ri[i]:ri[i+1L]-1L]]].(2) * header.zScale + header.zOffset, $
              data[index[ri[ri[i]:ri[i+1L]-1L]]].(0) * header.xScale + header.xOffset, $
              data[index[ri[ri[i]:ri[i+1L]-1L]]].(1) * header.yScale + header.yOffset, $
              productOptions.method, null)
          end
        endcase
      endif else begin
        surf[i] = null
      endelse
    endfor
    
  endif else begin
  
    ; Create a null surface
    xn = floor((tileStruct.xMax[tIndex[0]]-tileStruct.xMin[tIndex[0]]) / resolution)
    yn = floor((tileStruct.yMax[tIndex[0]]-tileStruct.yMin[tIndex[0]]) / resolution)
    surf = replicate(null,xn,yn)
    
  endelse
  
  ; Return surface
  return, surf
  
END



