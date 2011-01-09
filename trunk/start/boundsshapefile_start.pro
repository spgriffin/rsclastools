;+
; NAME:
;
;   boundsshapefile_start
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

PRO BoundsShapefile_start, event

  Widget_Control, event.top, Get_UValue=info
  
  in_proj = info.inproj_droplist->GetSelection()
  out_proj = info.outproj_droplist->GetSelection()
  zone = info.zone->Get_Value()
  hemisphere = info.hemi_droplist->GetSelection()
  Widget_Control, info.filetype, Get_Value=filetype
  Widget_Control, info.boundstype, Get_Value=boundstype
  Widget_Control, event.top, /Destroy
  if (info.infile[0] EQ '') then return
  nFiles = n_elements(info.infile)
  for i = 0L, nFiles-1L, 1L do begin
    if (boundstype[0] eq 0) then begin
      ReadLAS, info.infile[i], las_header, las_data
      ConvexHull, las_data.x * las_header.xScale + las_header.xOffset, las_data.y * las_header.yScale + las_header.yOffset, px, py
    endif else begin
      ReadHeaderLAS, info.infile[i], las_header
      ConvexHull, [las_header.xMin,las_header.xMax,las_header.xMax,las_header.xMin], [las_header.yMax,las_header.yMax,las_header.yMin,las_header.yMin], px, py
    endelse
    fparts = strsplit(info.infile[i], '.', /extract)
    outfile = fparts[0] + '_extent'
    CreateBoundsShapefile, outfile, px, py, file_basename(info.infile[i]), kml=filetype, in_proj=in_proj, out_proj=out_proj, zone=zone, hemisphere=hemisphere
  endfor
  if keyword_set(do_kml) then begin
    ok = dialog_message('Extent KML file/s created.', /information)
  endif else begin
    ok = dialog_message('Extent shapefile/s created.', /information)
  endelse
  
  ; Make file directory cwd
  cd, file_dirname(info.infile[0])
  
END
