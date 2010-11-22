;+
; NAME:
;
;   ENVI2GEOTIFF
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

PRO ENVI2GEOTIFF, filename, x0, y0, proj, cell_size=cell_size

  ; Error handling
  catch, theError
  if theError ne 0 then begin
    catch, /cancel
    help, /last_message, output=errText
    errMsg = dialog_message(errText, /error, title='Error writing GeoTIFF.')
    return
  endif
  
  ; Initialise variables
  if not keyword_set(cell_size) then cell_size = 1D
  
  ; Setup the geotiff structure
  geo = create_struct('MODELTIEPOINTTAG', [0D, 0D, 0D, x0, y0, 0D])
  geo = create_struct(geo, 'MODELPIXELSCALETAG', [cell_size, cell_size, 0D])
  geo = create_struct(geo, 'GTRASTERTYPEGEOKEY', 1)
  geo = create_struct(geo, 'GTMODELTYPEGEOKEY', 1)
  geo = create_struct(geo, 'PROJECTEDCSTYPEGEOKEY', proj)
  
  openr, 1, 'images.dat' ;Open the file as file unit 1.
  A = ASSOC(1, BYTARR(256, 256, /NOZERO)) ;Make an associated variable.
  
  ; Write the geotiff
  write_tiff, filename, grid, geotiff=geo, /float, description=filename
  
  
END
