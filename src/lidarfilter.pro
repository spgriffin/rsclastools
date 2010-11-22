;+
; NAME:
;
;   LidarFilter
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

PRO LidarFilter, infile, dh0, slope, resolution, b_max, b_start, height_threshold

  catch, theError
  if theError ne 0 then begin
    catch, /cancel
    help, /last_message, output=errText
    errMsg = dialog_message(errText, /error, title='Error filtering points.')
    return
  endif
  
  for i = 0L, n_elements(infile)-1L do begin
    ReadLAS, infile[i], las_header, las_data, /check
    easting = double(las_data.(0)) * las_header.xscale + las_header.xoffset
    northing = double(las_data.(1)) * las_header.yscale + las_header.yoffset
    elevation = double(las_data.(2)) * las_header.zscale + las_header.zoffset
    data = create_struct('Easting',temporary(easting),'Northing',temporary(northing),'Elevation',temporary(elevation))
    pmFilter,data,b_start=b_start,bmax=bmax,dh0=dh0,slope=slope,cell_size=resolution, progress_string=strtrim(infile[i],2),height_threshold=height_threshold
    las_data.(5) = data.class
    WriteLAS, infile[i], las_header, las_data, /check
  endfor
  
END
