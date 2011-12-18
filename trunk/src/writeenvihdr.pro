;+
; NAME:
;
;   WriteENVIhdr
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

PRO WriteENVIhdr, outfile, zone, resolution, xOrigin, yOrigin, ncol, nrow, proj, hemisphere, productType

  openw, hlun, outfile + '.hdr', /get_lun
  central_meridian = zone * 6 - 183
  
  printf, hlun, 'ENVI'
  printf, hlun, 'description = {' + outfile + '}'
  printf, hlun, 'samples = ' + strtrim(ncol, 2)
  printf, hlun, 'lines   = ' + strtrim(nrow, 2)
  printf, hlun, 'bands   = 1'
  printf, hlun, 'header offset = 0'
  printf, hlun, 'file type = ENVI Standard'
  printf, hlun, 'data type = 4'
  printf, hlun, 'interleave = bsq'
  printf, hlun, 'byte order = 0'
  case proj of
    0: begin
      printf, hlun, 'map info = {Map Grid of Australia (MGA 94) Zone ' + strtrim(zone, 2) + ', 1, 1, ' + $
        strtrim(xOrigin, 2) + ', ' + strtrim(yOrigin, 2) + ', ' + strtrim(resolution, 2) + ', ' + strtrim(resolution, 2) + $
        ', ' + strtrim(zone, 2) + ', Geocentric Datum of Australia 1994, units=Meters}'
      printf, hlun, 'projection info = {3, 6378137.0, 6356752.3, 0.000000, ' + strtrim(central_meridian, 2) + $
        ', 500000.0, 10000000.0, 0.999600, Geocentric Datum of Australia 1994, Map Grid of Australia (MGA 94) Zone ' + $
        strtrim(zone, 2) + ', units=Meters}'
    end
    1: begin
      printf, hlun, 'United Kingdom, 1, 1, ' + strtrim(xOrigin, 2) + ', ' + strtrim(yOrigin, 2) + ', ' + strtrim(resolution, 2) + ', ' + strtrim(resolution, 2) + ', WGS-84, units=Meters}'
      printf, hlun, 'projection info = {3, 6377563.4, 6356256.9, 49.000000, -2.000000, 400000.0, -100000.0, 0.999601, WGS-84, United Kingdom, units=Meters}'
    end
    2: begin
      printf, hlun, 'map info = {UTM, 1.000, 1.000, ' + strtrim(xOrigin, 2) + ', ' + strtrim(yOrigin, 2) + ', ' + strtrim(resolution, 2) + $
        ', ' + strtrim(resolution, 2) + ', ' + strtrim(zone, 2) + ', ' + strtrim(hemisphere, 2) + ', WGS-84, units=Meters}'
    end
  endcase
  printf, hlun, 'band names = {' + productType + '}'
  
  free_lun, hlun
  
END
