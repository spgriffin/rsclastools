;+
; NAME:
;
;   InterpolateHeight
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

PRO InterpolateHeight, infile, method, null, smoothing, power, min_points, sectors, variogram, output_type, function_type=function_type

  ; Start progress bar
  bcount = 0.0
  btotal = n_elements(infile)
  progressBar=Obj_New('progressbar', Color='Forest Green', Text='Calculating Point Heights', title='LAS Product', /fast_loop)
  progressBar->Start
  
  ; Error handling
  catch, theError
  if theError ne 0 then begin
    if (n_elements(progressbar) EQ 1) then progressbar->Destroy
    catch, /cancel
    help, /last_message, output=errText
    errMsg = dialog_message(errText, /error, title='Error calculating point heights.')
    return
  endif
  
  for i = 0L, n_elements(infile)-1L do begin
  
    ; Read the input file
    ReadLAS, infile[i], header, data
    progressBar -> SetProperty, Text=strtrim(infile[i],2)
    
    ; Get data for interpolation
    gnd = filterReturns(data, type=4)
    gnd_idx = where(gnd EQ 1, gnd_cnt, complement=veg_idx, ncomplement=veg_cnt)
    easting = data[gnd_idx].(0) * header.xScale + header.xOffset
    northing = data[gnd_idx].(1) * header.yScale + header.yOffset
    zdata = data[gnd_idx].(2) * header.zScale + header.zOffset
    grid_input, easting, northing, zdata, easting, northing, zdata
    triangulate, easting, northing, triangles
    
    ; Do the interpolation
    if (veg_cnt gt 0) then begin
      case method of
        'NearestNeighbor': begin
          outData = griddata(easting, northing, $
            zdata, method=method, triangles=triangles, missing=null, $
            xout=data[veg_idx].(0) * header.xScale + header.xOffset, yout=data[veg_idx].(1) * header.yScale + header.yOffset)
        end
        'Linear': begin
          outData = griddata(easting, northing, $
            zdata, method=method, triangles=triangles, missing=null, $
            xout=data[veg_idx].(0) * header.xScale + header.xOffset, yout=data[veg_idx].(1) * header.yScale + header.yOffset)
        end
        'InverseDistance': begin
          outData = griddata(easting, northing, $
            zdata, power=power, method=method, triangles=triangles, min_points=min_points, missing=null, sectors=sectors, empty_sectors=1, $
            xout=data[veg_idx].(0) * header.xScale + header.xOffset, yout=data[veg_idx].(1) * header.yScale + header.yOffset, smoothing=smoothing)
        end
        'NaturalNeighbor': begin
          outData = griddata(easting, northing, $
            zdata, method=method, triangles=triangles, missing=null, $
            xout=data[veg_idx].(0) * header.xScale + header.xOffset, yout=data[veg_idx].(1) * header.yScale + header.yOffset)
        end
        'PolynomialRegression': begin
          outData = griddata(easting, northing, $
            zdata, power=power, method=method, triangles=triangles, min_points=min_points, missing=null, sectors=sectors, empty_sectors=1, $
            xout=data[veg_idx].(0) * header.xScale + header.xOffset, yout=data[veg_idx].(1) * header.yScale + header.yOffset)
        end
        'Kriging': begin
          outData = griddata(easting, northing, $
            zdata, method=method, triangles=triangles, min_points=min_points, missing=null, sectors=sectors, empty_sectors=1, $
            xout=data[veg_idx].(0) * header.xScale + header.xOffset, yout=data[veg_idx].(1) * header.yScale + header.yOffset, variogram=variogram)
        end
        'RadialBasisFunction': begin
          outData = griddata(easting, northing, zdata, method=method, triangles=triangles, smoothing=smoothing, function_type=function_type, $
            /grid, xout=data[veg_idx].(0) * header.xScale + header.xOffset, yout=data[veg_idx].(1) * header.yScale + header.yOffset, $
            missing=null, min_points=min_points, sectors=sectors, empty_sectors=1)
        end
      endcase
      
      ; Calculate height
      height = fltarr(header.nPoints)
      height[veg_idx] = (data[veg_idx].(2) * header.zScale + header.zOffset) - outData
      height = temporary(height) > 0.0
      
    endif else begin
    
      height = fltarr(header.nPoints)
      
    endelse
    
    
    ; Write output to file
    fparts = strsplit(infile[i],'.', /extract)
    outfile = fparts[0] + '_PointHeights.las'
    case output_type of
      0: begin ; Point source ID
        data.(8) = 0L
        data.(8) = long(height / 0.01D)
        header.systemID = 0B
        header.systemID = byte('Height: Source')
      end
      1: begin ; Elevation
        data.(2) = long(height / 0.01D)
        header.systemID = 0B
        header.systemID = byte('Height: Elev')
      end
    endcase
    WriteLAS, outfile, header, data, /check, pointFormat=header.pointFormat
    
    ; Update progress bar
    bcount += 1.0
    if progressBar->CheckCancel() then begin
      ok=Dialog_Message('Point height calculation cancelled')
      progressBar->Destroy
      return
    endif
    progressbar->Update, bcount / btotal * 100.0
    
  endfor
  
  ; End progress bar
  progressbar->Destroy
  
END
