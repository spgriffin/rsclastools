;+
; NAME:
;
;   LidarInterpolate
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

PRO LidarInterpolate, infile, returnList, return_type, xMax, xMin, yMax, yMin, $
    resolution, power, method, product_type, proj, z, null, smoothing, min_points, $
    sectors, variogram, domask=domask, maskwin=maskwin, function_type=function_type
    
  forward_function NNfix, filterReturns, hist2d_riLidar
  
  ; Start progress bar
  bcount = 0.0
  btotal = n_elements(infile) * 3.0
  progressBar=Obj_New('progressbar', Color='Forest Green', Text='Interpolating LAS files', title='GeoTIFF Product', /fast_loop)
  progressBar->Start
  
  ; Error handling
  catch, theError
  if theError ne 0 then begin
    if (n_elements(progressbar) EQ 1) then progressbar->Destroy
    catch, /cancel
    help, /last_message, output=errText
    errMsg = dialog_message(errText, /error, title='Error interpolating points.')
    return
  endif
  
  ; Loop through each input file
  for i = 0L, n_elements(infile)-1L do begin
  
    ; Read the input file and index returns to interpolate
    ReadLAS, infile[i], las_header, las_data
    case return_type of
      returnList[0]: index = filterReturns(las_data, type=4)
      returnList[1]: index = filterReturns(las_data, type=5)
      returnList[2]: index = filterReturns(las_data, type=1, n=1)
      returnList[3]: index = filterReturns(las_data, type=2)
      returnList[4]: index = replicate(1, las_header.nPoints)
    endcase
    
    idx = where(index EQ 1, count)
    if (count EQ 0) then begin
      ok = dialog_message(strtrim(infile[i],2)+' : No valid returns.', /error)
      continue
    endif
    easting = las_data[idx].(0) * las_header.xscale + las_header.xoffset
    northing = las_data[idx].(1) * las_header.yscale + las_header.yoffset
    case z of
      2: begin
        zdata = las_data[idx].(2) * las_header.zscale + las_header.zoffset
      end
      3: begin
        zdata = las_data[idx].(3)
      end
      4: begin
        zdata = ishft(ishft(las_data[idx].nreturn,2),-5)
      end
      5: begin
        zdata = las_data[idx].(4)
        zdata = ishft(ishft(zdata,4),-4)
      end
      7: begin
        zdata = las_data[idx].(7) * 0.1
      end
      8: begin
        if (string(las_header.systemID) eq 'Height: Source') then zdata = las_data[idx].(8) * 0.01
        if (string(las_header.systemID) eq 'Height: Elev') then zdata = las_data[idx].(2) * las_header.zScale + las_header.zOffset
      end
      else: begin
        ok=Dialog_Message('zdata type not currently supported.')
        progressBar->Destroy
        return
      end
    endcase
    
    MyDelVar, las_data
    grid_input, easting, northing, zdata, easting, northing, zdata, epsilon=resolution/2D, duplicates ='Min', exclude = dup_idx
    
    ; Interpolate the data
    progressBar -> SetProperty, Text=strtrim(infile[i],2)
    bcount += 1.0
    if progressBar->CheckCancel() then begin
      ok=Dialog_Message('Raster interpolation cancelled')
      progressBar->Destroy
      return
    endif
    progressbar->Update, bcount / btotal * 100.0
    triangulate, easting, northing, triangles
    half_pixel = resolution / 2D
    xout = dindgen(round((xMax[i] - xMin[i]) / resolution)) * resolution
    ncol = n_elements(xout)
    xout += replicate(xMin[i] + half_pixel, ncol)
    yout = dindgen(round((yMax[i] - yMin[i]) / resolution)) * resolution
    nrow = n_elements(yout)
    yout += replicate(yMin[i] + half_pixel, nrow)
    case method of
      'NearestNeighbor': begin
        outData = griddata(easting, northing, zdata, method=method, triangles=triangles, $
          /grid, xout=xout, yout=yout, missing=null)
      end
      'Linear': begin
        outData = griddata(easting, northing, zdata, method=method, triangles=triangles, $
          /grid, xout=xout, yout=yout, missing=null)
      end
      'InverseDistance': begin
        outData = griddata(easting, northing, zdata, power=power, method=method, triangles=triangles, $
          /grid, xout=xout, yout=yout, missing=null, smoothing=smoothing, min_points=min_points, $
          sectors=sectors, empty_sectors=1)
      end
      'Quintic': begin
        outData = griddata(easting, northing, zdata, method=method, triangles=triangles, delta=resolution, $
          dimension=[ncol,nrow], start=[xMin[i],yMin[i]], missing=null)
      end
      'NaturalNeighbor': begin
        outData = griddata(easting, northing, zdata, method=method, triangles=triangles, $
          /grid, xout=xout, yout=yout, missing=null)
        outData = NNfix(outData, resolution)
      end
      'PolynomialRegression': begin
        outData = griddata(easting, northing, zdata, power=power, method=method, triangles=triangles, $
          /grid, xout=xout, yout=yout, missing=null, min_points=min_points, $
          sectors=sectors, empty_sectors=1)
      end
      'Kriging': begin
        ; Detrend the surface
        lin_surf = sfit(transpose([[easting], [northing], [zdata]]), 1, /irregular, kx=kx)
        ; Do the kriging
        outData = griddata(easting, northing, zdata - lin_surf, method=method, triangles=triangles, $
          /grid, xout=xout, yout=yout, missing=null, min_points=min_points, variogram=variogram, $
          sectors=sectors, empty_sectors=1)
        ; Retrend the surface
        nobs = n_elements(outData)
        outData = temporary(outData) + kx[0,0] + $
          kx[0,1] * reform(rebin(xout, ncol, nrow, /sample), nobs) + $
          kx[1,0] * reform(transpose(rebin(yout, nrow, ncol, /sample)), nobs) + $
          kx[1,1] * (reform(rebin(xout, ncol, nrow, /sample), nobs) * reform(transpose(rebin(yout, nrow, ncol, /sample)), nobs))
      end
      'RadialBasisFunction': begin
        outData = griddata(easting, northing, zdata, method=method, triangles=triangles, $
          /grid, xout=xout, yout=yout, missing=null, smoothing=smoothing, min_points=min_points, $
          sectors=sectors, empty_sectors=1, function_type=function_type)
      end
    endcase
    
    ; Update progress bar
    bcount += 1.0
    if progressBar->CheckCancel() then begin
      ok=Dialog_Message('Interpolation cancelled')
      progressBar->Destroy
      return
    endif
    progressbar->Update, bcount / btotal * 100.0
    
    ; Set out-of-bounds and extrapolated predictions to null
    ConvexHull, easting, northing, px, py
    inside = Obj_New('IDLanROI', px, py)
    nobs = n_elements(outData)
    xout_temp = reform(rebin(xout, ncol, nrow, /sample), nobs)
    yout_temp = reform(transpose(rebin(yout, nrow, ncol, /sample)), nobs)
    inside_index = inside->ContainsPoints(temporary(xout_temp), temporary(yout_temp))
    null_index = where((temporary(inside_index) NE 1), null_count)
    if (null_count GT 0) then outData[null_index] = null
    Obj_Destroy, inside
    bad_index = where((outData LT null) or (outData GT (max(zdata)+1.0)), bad_count)
    if (bad_count GT 0) then outData[bad_index] = null
    
    ; Create a mask to set extrapolated predictions to null
    if keyword_set(domask) then begin
      no_obs = hist2d_riLidar(easting, northing, ri, ncol, nrow, bin1=resolution, bin2=resolution, $
        min1=min(xout)-half_pixel, min2=min(yout)-half_pixel, max1=max(xout)+half_pixel, max2=max(yout)+half_pixel)
      no_obs = morph_close(temporary(no_obs gt 0), replicate(1,21,21))
      bad_index = where(no_obs eq 0, bad_count)
      if (bad_count gt 0) then outData[bad_index] = null
    endif
    
    ; Write out to GeoTIFF
    fparts = strsplit(infile[i], '.', /extract)
    filename = strjoin([fparts[0], strtrim(product_type,2), method], '_') + '.tif'
    writegeotiff, temporary(rotate(outData,7)), double(xout[0]-half_pixel), double(yout[n_elements(yout)-1L]+half_pixel), proj, filename, cell_size=double(resolution)
    bcount += 1
    if progressBar->CheckCancel() then begin
      ok=Dialog_Message('Interpolation cancelled')
      progressBar->Destroy
      return
    endif
    progressbar->Update, bcount / btotal * 100.0
    
  endfor
  
  ; End progress bar
  progressbar->Destroy
  
END
