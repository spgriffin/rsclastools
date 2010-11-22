;+
; NAME:
;
;   TileInterpolateSurface
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

PRO TileInterpolateSurface, lasfiles, method=method, resolution=resolution, zone=zone, tilesize=tilesize, null=null, hemisphere=hemisphere, $
    min_points=min_points, sectors=sectors, smoothing=smoothing, proj=proj, productType=productType, separate=separate, $
    outFormat=outFormat
    
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
  forward_function SurfaceTile, SurfaceInterpolate
  if not keyword_set(outFormat) then outFormat = 'ENVI'
  if not keyword_set(method) then method = 'NaturalNeighbor'
  if not keyword_set(resolution) then resolution = 0.5
  if not keyword_set(zone) then zone = 55
  if not keyword_set(tilesize) then tilesize = [100.0,100.0]
  if not keyword_set(null) then null = 0.0
  if not keyword_set(min_points) then min_points=6
  if not keyword_set(sectors) then sectors=6
  if not keyword_set(smoothing) then smoothing=0
  if not keyword_set(hemisphere) then hemisphere='South'
  if not keyword_set(proj) then proj=0
  if not keyword_set(productType) then productType='Elevation'
  RSC_LAS_Tools_SysVar
  
  ; Check heights if necessary
  if (productType eq 'Height') then begin
    for i = 0L, n_elements(lasfiles)-1L, 1L do begin
      ReadHeaderLas, lasfiles[i], tempHeader
      hType = string(tempheader.systemID)
      if ((hType ne 'Height: Source') and (hType ne 'Height: Elev')) then begin
        errMsg = dialog_message('Point heights not calculated for' + lasfiles[i], /error, title='TileInterpolateSurface')
        return
      endif
    endfor
  endif
  
  ; Start progress bar
  progressBar=Obj_New('progressbar', Color='Forest Green', Text='Initialising...', title='Surface Product', /fast_loop)
  progressBar->Start
  bcount = 0.0
  
  ; Process all files separately or all at once
  opath = file_dirname(lasfiles[0])
  resolution_str = string(resolution*100.0,format='(I04)')
  n_surfaces = keyword_set(separate) ? n_elements(lasfiles) : 1L
  for j = 0L, n_surfaces - 1L, 1L do begin
  
    ; Output filename
    if not keyword_set(separate) then begin
      outfile = filepath(strjoin(['RSCLASTools_TiledSurface',method,productType,resolution_str], '_'), root_dir=opath)
      las_input = lasfiles
    endif else begin
      fparts = strsplit(file_basename(lasfiles[j]), '.', /extract)
      outfile = filepath(strjoin([fparts[0],'RSCLASTools_TiledSurface',method,productType,resolution_str], '_'), root_dir=opath)
      las_input = lasfiles[j]
      progressBar -> SetProperty, Text=strtrim(las_input,2)
      progressbar->Update, 0D
    endelse
    openw, lun, outfile, /get_lun
    
    ; Tile
    progressBar -> SetProperty, Text="Tiling LAS data"
    tileStruct = SurfaceTile(las_input, tileXsize=tilesize[0],tileYsize=tilesize[1],/tmp,resolution=resolution)
    ncols = (tileStruct.lrx - tileStruct.ulx) / resolution
    nrows = (tileStruct.uly - tileStruct.lry) / resolution
    btotal = float(tileStruct.nrows)
    
    ; Interpolate, stitch
    progressBar -> SetProperty, Text="Interpolating LAS data"
    for row = 1L, tileStruct.nrows, 1L do begin
      index = where(tileStruct.row EQ (tileStruct.nrows-row+1L))
      nlines = (tileStruct.yMax[index[0]] - tileStruct.yMin[index[0]]) / resolution
      temp = replicate(null,ncols, nlines)
      for i = 0L, tileStruct.ncols-1L, 1L do begin
        if (tileStruct.empty[index[i]] EQ 0) then begin
          surf = SurfaceInterpolate(tileStruct, tileStruct.col[index[i]], tileStruct.row[index[i]], method, resolution, $
            null, min_points, sectors, smoothing, productType)
          xstart = (tileStruct.xMin[index[i]] - tileStruct.ulx) / resolution
          temp[xstart, 0] = reverse(surf, 2)
        endif
      endfor
      writeu, lun, temp
      ; Update progress bar
      bcount += 1.0
      if progressBar->CheckCancel() then begin
        ok=Dialog_Message('LAS interpolation cancelled')
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
      writeENVIhdr, outfile, zone, resolution, tileStruct.ulx, tileStruct.uly, ncols, nrows, proj, hemisphere, productType
    endif else begin
      writegeotiff, outfile, tileStruct.ulx, tileStruct.uly, proj, outfile+'.tif', cell_size=resolution, /assocInput, ncols=ncols, nrows=nrows
      file_delete, outfile, /quiet
    endelse
    
  endfor
  
  ; End progress bar
  progressbar->Destroy
  
END
