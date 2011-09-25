;+
; NAME:
;
;   TileInterpolateHeight
;
; PURPOSE:
;
;   Tile LAS file and interpolate height of returns above the ground
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

PRO TileInterpolateHeight, lasfiles, method=method, tilesize=tilesize, null=null, $
    min_points=min_points, sectors=sectors, smoothing=smoothing, outputType=outputType, tmp=tmp
    
  ; Error handling
  catch, theError
  if theError ne 0 then begin
    if (n_elements(progressbar) EQ 1) then progressbar->Destroy
    catch, /cancel
    help, /last_message, output=errText
    errMsg = dialog_message(errText, /error, title='Error interpolating return heights.')
    return
  endif
  
  ; Keywords and system stuff
  forward_function SurfaceTile, PointInterpolateHeight
  if not keyword_set(method) then method = 'NaturalNeighbor'
  if not keyword_set(tilesize) then tilesize = [100.0,100.0]
  if not keyword_set(null) then null = 0.0
  if not keyword_set(min_points) then min_points=6
  if not keyword_set(sectors) then sectors=6
  if not keyword_set(smoothing) then smoothing=0
  RSC_LAS_Tools_SysVar
  
  ; Tile LAS files
  tileStruct = SurfaceTile(lasfiles, tileXsize=tilesize[0],tileYsize=tilesize[1],tmp=tmp,resolution=resolution, /progress)
  ncols = (tileStruct.lrx - tileStruct.ulx) / resolution
  nrows = (tileStruct.uly - tileStruct.lry) / resolution
  
  ; Start progress bar
  progressBar=Obj_New('progressbar', Color='Forest Green', Text='Initialising...', title='Interpolating return heights...', /fast_loop)
  progressBar->Start
  btotal = float(tileStruct.nTiles)
  bcount = 0D
  progressbar->Update, bcount
  
  ; Interpolate ground height
  progressBar -> SetProperty, Text="Interpolating LAS data"
  outputTiles = strarr(tileStruct.nTiles)
  for i = 0L, tileStruct.nTiles-1L, 1L do begin
    if (tileStruct.empty[i] EQ 0) then begin
      outputTiles[i] = PointInterpolateHeight(tileStruct, tileStruct.col[i], tileStruct.row[i], $
        method, null, min_points, sectors, smoothing, outputType)
    endif
    ; Update progress bar
    bcount += 1.0
    if progressBar->CheckCancel() then begin
      ok=Dialog_Message('LAS interpolation cancelled')
      progressBar->Destroy
      return
    endif
    progressbar->Update, bcount / btotal * 100.0
  endfor
  file_delete, tileStruct.name, /quiet
  
  ; Write data back to original files
  progressBar -> SetProperty, Text="Writing LAS data"
  bcount = 0D
  progressbar->Update, bcount
  opath = file_dirname(lasfiles[0])
  outputFiles = lasfiles
  for i = 0L, n_elements(lasfiles)-1L, 1L do begin
    ReadHeaderLas, lasfiles[i], temp_header
    fparts = strsplit(file_basename(lasfiles[i]), '.', /extract)
    outputFiles[i] = filepath(strjoin([fparts[0],'AGH.las'], '_'), root_dir=opath)
    openw, outputLun, outputFiles[i], /get_lun, /swap_if_big_endian
    case outputType of
      0: begin ; Point source ID
        temp_header.systemID = 0B
        temp_header.systemID = byte('Height: Source')
      end
      1: begin ; Elevation
        temp_header.systemID = 0B
        temp_header.systemID = byte('Height: Elev')
      end
    endcase
    temp_header.dataoffset = temp_header.headersize ; No inheritance of VLR's for RSC LAS Tools processing
    temp_header.xScale = tileStruct.xScale
    temp_header.yScale = tileStruct.yScale
    temp_header.zScale = tileStruct.zScale
    temp_header.xOffset = tileStruct.xOffset
    temp_header.yOffset = tileStruct.yOffset
    temp_header.zOffset = tileStruct.zOffset
    writeu, outputLun, temp_header
    free_lun, outputLun
  endfor
  for i = 0L, tileStruct.nTiles-1L, 1L do begin
    if (tileStruct.empty[i] eq 0) then begin
      progressBar -> SetProperty, Text=strtrim(outputTiles[i],2)
      ReadLAS, outputTiles[i], tile_header, tile_data
      uFileID = tile_data[uniq(tile_data.user, sort(tile_data.user))].user
      for j = 0L, n_elements(uFileID)-1L, 1L do begin
        index = where(tile_data.user eq uFileID[j])
        openw, outputLun, outputFiles[uFileID[j]], /get_lun, /swap_if_big_endian, /append
        writeu, outputLun, tile_data[index]
        free_lun, outputLun
      endfor
      file_delete, outputTiles[i], /quiet
    endif
    bcount += 1.0
    if progressBar->CheckCancel() then begin
      ok=Dialog_Message('LAS writing cancelled')
      progressBar->Destroy
      return
    endif
    progressbar->Update, bcount / float(tileStruct.nTiles) * 100.0
  endfor
  
  ; End progress bar
  progressbar->Destroy
  
END
