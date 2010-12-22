;+
; NAME:
;
;   CreateDEM
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

PRO CreateDEM, lasfile, method=method, resolution=resolution, zone=zone, tilesize=tilesize, null=null

  ; Keywords and system stuff
  start_mem = memory(/current)
  start_time = systime(1)
  forward_function SurfaceTile, SurfaceInterpolate, RSC_LAS_Tools_SysVar
  if not keyword_set(method) then method = 'NaturalNeighbor'
  if not keyword_set(resolution) then resolution = 0.5
  if not keyword_set(zone) then zone = 55
  if not keyword_set(tilesize) then tilesize = 20.0
  if not keyword_set(null) then null = 0.0
  RSC_LAS_Tools_SysVar
  
  ; Initialize output file
  fparts = strsplit(lasfile, '.', /extract)
  outfile = fparts[0]
  openw, lun, outfile, /get_lun
  
  ; Tile, interpolate, stitch
  print, "Tiling " + lasfile
  tileStruct = SurfaceTile(lasfile,tilesize=tilesize,/tmp,resolution=resolution)
  ncols = (tileStruct.lrx - tileStruct.ulx) / resolution
  nrows = (tileStruct.uly - tileStruct.lry) / resolution
  print, "Interpolating " + lasfile
  for row = 1L, tileStruct.nrows, 1L do begin
    index = where(tileStruct.row EQ (tileStruct.nrows-row+1L))
    nlines = (tileStruct.yMax[index[0]] - tileStruct.yMin[index[0]]) / resolution
    temp = replicate(null,ncols, nlines)
    for i = 0L, tileStruct.ncols-1L, 1L do begin
      if (tileStruct.empty[index[i]] EQ 0) then begin
        surf = SurfaceInterpolate(tileStruct, tileStruct.col[index[i]], tileStruct.row[index[i]], method=method, resolution=resolution, null=null)
        xstart = (tileStruct.xMin[index[i]] - tileStruct.ulx) / resolution
        temp[xstart, 0] = reverse(surf, 2)
      endif
    endfor
    writeu, lun, temp
  endfor
  myDelVar, temp
  free_lun, lun
  
  ; Delete temporary tiles
  file_delete, tileStruct.name, /quiet
  
  ; Write the ENVI header file
  writeENVIhdr, outfile, zone, resolution, tileStruct.ulx, tileStruct.uly, ncols, nrows
  print, outfile + ' created.'
  print, 'Memory required (Mb) : ', (memory(/highwater) - start_mem) / 1048576.0
  print, 'Time required (min) : ', (systime(1) - start_time) / 60.0
  
END


