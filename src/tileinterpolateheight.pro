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
    min_points=min_points, sectors=sectors, smoothing=smoothing, outputType=outputType
    
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
  forward_function SurfaceTile
  if not keyword_set(method) then method = 'NaturalNeighbor'
  if not keyword_set(tilesize) then tilesize = [100.0,100.0]
  if not keyword_set(null) then null = 0.0
  if not keyword_set(min_points) then min_points=6
  if not keyword_set(sectors) then sectors=6
  if not keyword_set(smoothing) then smoothing=0
  RSC_LAS_Tools_SysVar
  
  ; Start progress bar
  progressBar=Obj_New('progressbar', Color='Forest Green', Text='Initialising...', title='Surface Product', /fast_loop)
  progressBar->Start
  
  ; Process all files separately
  opath = file_dirname(lasfiles[0])
  for j = 0L, n_elements(lasfiles) - 1L, 1L do begin
  
    ; Output filename
    fparts = strsplit(file_basename(lasfiles[j]), '.', /extract)
    outfile = filepath(strjoin([fparts[0],'AGL.las'], '_'), root_dir=opath)
    las_input = lasfiles[j]
    progressBar -> SetProperty, Text=strtrim(las_input,2)
    bcount = 0D
    progressbar->Update, 0D
    
    ; Tile
    progressBar -> SetProperty, Text="Tiling LAS data"
    tileStruct = SurfaceTile(las_input, tileXsize=tilesize[0],tileYsize=tilesize[1],/tmp,resolution=resolution)
    ncols = (tileStruct.lrx - tileStruct.ulx) / resolution
    nrows = (tileStruct.uly - tileStruct.lry) / resolution
    btotal = float(tileStruct.nrows) + 1.0
    
    ; Interpolate
    progressBar -> SetProperty, Text="Interpolating LAS data"
    for row = 1L, tileStruct.nrows, 1L do begin
      index = where(tileStruct.row EQ (tileStruct.nrows-row+1L))
      for i = 0L, tileStruct.ncols-1L, 1L do begin
        if (tileStruct.empty[index[i]] EQ 0) then begin
          PointInterpolateHeight, tileStruct, tileStruct.col[index[i]], tileStruct.row[index[i]], method, $
            null, min_points, sectors, smoothing, outputType
        endif
      endfor
      ; Update progress bar
      bcount += 1.0
      if progressBar->CheckCancel() then begin
        ok=Dialog_Message('LAS interpolation cancelled')
        progressBar->Destroy
        return
      endif
      progressbar->Update, bcount / btotal * 100.0
    endfor
    
    ; Stitch and sort temporary tiles
    progressBar -> SetProperty, Text="Stitching LAS data"
    openw, outputLun, outfile, /get_lun
    ReadHeaderLas, lasfiles[j], temp_header
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
    writeu, outputLun, temp_header
    point_lun, outputLun, temp_header.dataOffset
    for i = 0L, tileStruct.nTiles-1L, 1L do begin
      if (tileStruct.empty[i] EQ 0) then begin
        ReadLAS, tileStruct.name[i], temp_header, temp_data
        writeu, outputLun, temp_data
      endif
    endfor
    free_lun, outputLun
    ; Update progress bar
    bcount += 1.0
    if progressBar->CheckCancel() then begin
      ok=Dialog_Message('LAS interpolation cancelled')
      progressBar->Destroy
      return
    endif
    progressbar->Update, bcount / btotal * 100.0
    
    ; Delete temporary tiles
    index = where(tileStruct.empty eq 0, count)
    if (count gt 0) then file_delete, tileStruct.name[index], /quiet
    
  endfor
  
  ; End progress bar
  progressbar->Destroy
  
END
