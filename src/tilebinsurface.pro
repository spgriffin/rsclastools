;+
; NAME:
;
;   TileBinSurface
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

PRO TileBinSurface, lasfiles, resolution=resolution, zone=zone, tilesize=tilesize, null=null, hemisphere=hemisphere, $
    proj=proj, productType=productType, productOptions=productOptions, separate=separate, outFormat=outFormat
    
  ; Error handling
  catch, theError
  if theError ne 0 then begin
    if (n_elements(progressbar) EQ 1) then progressbar->Destroy
    catch, /cancel
    help, /last_message, output=errText
    errMsg = dialog_message(errText, /error, title='Error creating surface.')
    return
  endif
  
  ; Keywords and system stuff
  RSC_LAS_Tools_SysVar
  forward_function SurfaceTile, SurfaceBin
  if not keyword_set(outFormat) then outFormat = 'ENVI'
  if not keyword_set(resolution) then resolution = 5.0
  if not keyword_set(zone) then zone = 55
  if not keyword_set(tilesize) then tilesize = [100.0,100.0]
  if not keyword_set(null) then null = 0.0
  if not keyword_set(hemisphere) then hemisphere='South'
  if not keyword_set(proj) then proj=''
  if not keyword_set(productType) then productType = 'Statistic'
  case productType of
    'Statistic': if not keyword_set(productOptions) then productOptions = {method:'Maximum',returnType:'All',field:'Height',class:'All'}
    'Canopy Metric': begin
      if not keyword_set(productOptions) then productOptions = {method:'Fractional Cover - Count Ratio', field:'Height', returnType:'First', class:'All', $
        height_threshold:0.5, weights:[1.0,1.0,0.5], percentile:0.99, rhovg_method:1, $
        rhovg_percentile:0.99, constant:0.62, height_threshold_top:100.0, vbinsize:0.15}
    end
    'Terrain Metric': if not keyword_set(productOptions) then productOptions = {method:'Slope',returnType:'All',field:'Elevation',class:'Ground'}
  endcase
  
  ; Check heights if necessary
  if (productOptions.field eq 'Height') then begin
    for i = 0L, n_elements(lasfiles)-1L, 1L do begin
      ReadHeaderLas, lasfiles[i], tempHeader
      hType = string(tempheader.systemID)
      if ((hType ne 'Height: Source') and (hType ne 'Height: Elev')) then begin
        errMsg = dialog_message('Point heights not calculated for' + lasfiles[i], /error, title='TileBinSurface')
        return
      endif
    endfor
  endif
  
  ; Start progress bar
  progressBar=Obj_New('progressbar', Color='Forest Green', Text='Creating surface...', title='Surface Product', /fast_loop)
  progressBar->Start
  bcount = 0.0
  
  ; Process all files separately or all at once
  opath = file_dirname(lasfiles[0])
  resolution_str = string(resolution*100.0,format='(I06)')
  productOption_str = strjoin([productOptions.method, productOptions.returnType], '_')
  n_surfaces = keyword_set(separate) ? n_elements(lasfiles) : 1L
  for j = 0L, n_surfaces - 1L, 1L do begin
  
    ; Output filename
    if not keyword_set(separate) then begin
      outfile = filepath(strcompress(strjoin(['RSCLASTools',productType,productOption_str,resolution_str], '_'),/remove_all), root_dir=opath)
      las_input = lasfiles
    endif else begin
      fparts = strsplit(file_basename(lasfiles[j]), '.', /extract)
      outfile = filepath(strcompress(strjoin([fparts[0],'RSCLASTools',productType,productOption_str,resolution_str], '_'),/remove_all), root_dir=opath)
      las_input = lasfiles[j]
      progressbar->Update, 0D
    endelse
    openw, lun, outfile, /get_lun
    
    ; Tile
    progressBar -> SetProperty, Text="Tiling LAS data"
    tileStruct = SurfaceTile(las_input, tileXsize=tilesize[0],tileYsize=tilesize[1],/tmp,resolution=resolution)
    ncols = (tileStruct.lrx - tileStruct.ulx) / resolution
    nrows = (tileStruct.uly - tileStruct.lry) / resolution
    btotal = float(tileStruct.nrows)
    if not keyword_set(separate) then begin
      progressBar -> SetProperty, Text="Binning all LAS files"
    endif else begin
      progressBar -> SetProperty, Text=strtrim(las_input,2)
    endelse
    
    ; Interpolate, stitch
    for row = 1L, tileStruct.nrows, 1L do begin
      index = where(tileStruct.row EQ (tileStruct.nrows-row+1L))
      nlines = (tileStruct.yMax[index[0]] - tileStruct.yMin[index[0]]) / resolution
      temp = replicate(null, ncols, nlines)
      for i = 0L, tileStruct.ncols-1L, 1L do begin
        if (tileStruct.empty[index[i]] EQ 0) then begin
          surf = SurfaceBin(tileStruct, tileStruct.col[index[i]], tileStruct.row[index[i]], resolution, null, productType, productOptions)
          xstart = (tileStruct.xMin[index[i]] - tileStruct.ulx) / resolution
          temp[xstart, 0] = reverse(surf, 2)
        endif
      endfor
      writeu, lun, temp
      ; Update progress bar
      bcount += 1.0
      if progressBar->CheckCancel() then begin
        ok=Dialog_Message('Surface creation cancelled')
        progressBar->Destroy
        return
      endif
      progressbar->Update, bcount / btotal * 100.0
    endfor
    myDelVar, temp
    free_lun, lun
    
    ; Delete temporary tiles
    index = where(tileStruct.empty eq 0, count)
    if (count gt 0) then file_delete, tileStruct.name[index], /quiet
    
    ; Write the ENVI header file
    if (outFormat eq 'ENVI') then begin
      writeENVIhdr, outfile, zone, resolution, tileStruct.ulx, tileStruct.uly, ncols, nrows, proj, hemisphere, productOptions.method
    endif else begin
      writegeotiff, outfile, tileStruct.ulx, tileStruct.uly, proj, outfile+'.tif', cell_size=resolution, /assocInput, $
        ncols=ncols, nrows=nrows, zone=zone
      file_delete, outfile, /quiet
    endelse
    
  endfor
  
  ; End progress bar
  progressbar->Destroy
  
END
