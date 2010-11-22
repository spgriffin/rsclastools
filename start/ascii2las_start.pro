;+
; NAME:
;
;   ascii2las_start
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

PRO Ascii2LAS_start, event

  Widget_Control, event.top, Get_UValue=info
  
  ; Open log file
  j_fpart = strtrim(round(systime(/julian)*10), 2)
  openw,  loglun, file_dirname(info.infile[0], /mark_directory) + 'Ascii2LAS_Processing_Notes_' + j_fpart + '.txt', /get_lun, width=200
  printf, loglun, 'Input files: ', info.infile
  start_mem = memory(/current)
  t = systime(1)
  
  ; Derive product
  Widget_Control, info.class, Get_Value=class
  limit = info.field1->Get_Value()
  skipline = info.field2->Get_Value()
  delimiter_type = info.delimiter_droplist->GetSelection()
  case delimiter_type of
    info.delimiterList[0]: delimiter = string(9B)
    info.delimiterList[1]: delimiter = ' '
    info.delimiterList[2]: delimiter = ','
    info.delimiterList[3]: delimiter = ';'
  endcase
  Widget_Control, event.top, /Destroy
  ascii2LAS, info.infile, reform(info.columns)-1L, delimiter, limit, skipline, class
  
  ; Write to and close log file
  printf, loglun, 'Time required: ', systime(1) - t, ' seconds'
  printf, loglun, 'Memory required: ', memory(/highwater) - start_mem, ' bytes'
  printf, loglun, 'Column delimiter: ', delimiter
  printf, loglun, 'No. lines skipped: ', skipline
  printf, loglun, 'Minimum distance between first and last returns: ', limit, ' m'
  printf, loglun, 'Return classification: ', class
  free_lun, loglun
  
  ; Make file directory cwd
  cd, file_dirname(info.infile[0])
  
END
