;+
; NAME:
;
;   extractsite_start
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

PRO ExtractSite_start, event

  Widget_Control, event.top, Get_UValue=info
  
  site_file = info.site_file->GetFilename()
  Widget_Control, info.type, Get_Value=shape
  type = (shape[0] EQ 1) ? 2 : 1
  Widget_Control, event.top, /Destroy
  no_lines = file_lines(site_file)
  lines = strarr(no_lines)
  openr, lun, site_file, /get_lun
  readf, lun, lines
  free_lun, lun
  name = strarr(no_lines)
  easting = dblarr(no_lines)
  northing = dblarr(no_lines)
  major_axis = fltarr(no_lines)
  minor_axis = fltarr(no_lines)
  azimuth = fltarr(no_lines)
  for i = 0L, no_lines-1L do begin
    lparts = strsplit(lines[i], ',', /extract, count=lcount)
    if (lcount NE 6) then begin
      ok = dialog_message('Site file must be comma delimited and have six columns.', /error)
      return
    endif
    name[i] = lparts[0]
    easting[i] = double(lparts[1])
    northing[i] = double(lparts[2])
    major_axis[i] = float(lparts[3])
    minor_axis[i] = float(lparts[4])
    azimuth[i] = float(lparts[5])
  endfor
  ExtractSite, info.infile, type, name, easting, northing, major_axis, minor_axis, azimuth
  
  ; Make file directory cwd
  cd, file_dirname(info.infile[0])
  
END
