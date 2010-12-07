;+
; NAME:
;
;   WriteLidarAttributeFile
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

FUNCTION WriteLidarAttributeFile, infile, type, attribute

  fparts = strsplit(infile, '.', /extract)
  outputFile = fparts[0] + '_' + strtrim(type,2) + '_' + strtrim(attribute,2) + '.las'
  ReadHeaderLas, infile, header
  header.nPoints = 0
  header.softwareID = byte('IDL ' + !version.release)
  date = bin_date(systime(/utc))
  day = julday(date[1],date[2],date[0]) - julday(1,1,date[0]) + 1
  header.day  = uint(day)
  header.year = uint(date[0])
  if not file_test(outputFile) then WriteLAS, outputFile, header, /nodata, pointFormat=header.pointFormat
  return, outputFile
  
END


