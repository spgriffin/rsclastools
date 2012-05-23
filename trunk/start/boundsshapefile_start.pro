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
  splitsize = info.splitsize->Get_Value()
  hemisphere = info.hemi_droplist->GetSelection()
  Widget_Control, info.filetype, Get_Value=filetype
  Widget_Control, info.boundstype, Get_Value=boundstype
  Widget_Control, event.top, /Destroy
  if (info.infile[0] EQ '') then return
  nFiles = n_elements(info.infile)
  for i = 0L, nFiles-1L, 1L do begin
    if (boundstype[0] eq 0) then begin

      ; Open LAS file
      assocLun = 1
      ReadLAS, info.infile[i], las_header, las_data, assocLun=assocLun
      px = las_header.xMin
      py = las_header.yMin
      
      ; Work out number of splits
      if (splitsize GE las_header.nPoints) then begin
        no_splits = 1
      endif else begin
        no_splits = ceil(las_header.nPoints / splitsize)
      endelse
      
      ; Loop through each split
      for j = 0L, no_splits - 1L, 1L do begin
      
        ; Determine split
        case j of
          0: begin
            lower = 0L
            upper = splitsize - 1L
          end
          no_splits-1L: begin
            lower = j * splitsize
            upper = las_header.nPoints - 1L
          end
          else: begin
            lower = j * splitsize
            upper = lower + splitsize
          end
        endcase
        
        ; Read the split
        nPoints = upper - lower + 1L
        sp_index = lindgen(nPoints) + lower
        dataStr = InitDataLAS(pointFormat=las_header.pointFormat)
        data = replicate(dataStr, nPoints)
        for k = 0L, nPoints-1L do begin
          data[k] = las_data[sp_index[k]]
        endfor
        
        ; Get partial convex hull
        x1 = [data.x * las_header.xScale + las_header.xOffset, px]
        y1 = [data.y * las_header.yScale + las_header.yOffset, py]
        ConvexHull, x1, y1, px, py
        
      endfor
      
      ; Close assoc las file
      free_lun, assocLun
      
    endif else begin
    
      ReadHeaderLAS, info.infile[i], las_header
      ConvexHull, [las_header.xMin,las_header.xMax,las_header.xMax,las_header.xMin], [las_header.yMax,las_header.yMax,las_header.yMin,las_header.yMin], px, py
      
    endelse
    
    fparts = strsplit(info.infile[i], '.', /extract)
    outfile = fparts[0] + '_extent'
    CreateBoundsShapefile, outfile, px, py, las_header.zMax, file_basename(info.infile[i]), kml=filetype, in_proj=in_proj, out_proj=out_proj, zone=zone, hemisphere=hemisphere
    
  endfor
  
  ; Notify that we've finished
  if (filetype eq 1) then begin
    ok = dialog_message('Extent KML file/s created.', /information)
  endif else begin
    ok = dialog_message('Extent shapefile/s created.', /information)
  endelse
  
  ; Make file directory cwd
  cd, file_dirname(info.infile[0])
  
END
