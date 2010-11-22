;+
; NAME:
;
;   lidarmerge_start
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

PRO LidarMerge_start, event

  Widget_Control, event.top, Get_UValue=info
  
  outFile = info.field1->Get_Value()
  Widget_Control, info.bufferflag, Get_Value=bufferflag
  fparts = strsplit(outFile, '.', /extract, count=count)
  if (fparts[count-1L] ne 'las') then outFile += '.las'
  Widget_Control, event.top, /Destroy
  dirName = file_dirname(info.infile[0], /mark_directory)
  outFile = dirName + outFile
  LASmerge, info.infile, outFile, /new_psid, buffer=bufferflag[0]
  
  ; Make file directory cwd
  cd, file_dirname(info.infile[0])
  
END
