;+
; NAME:
;
;   changebounds_start
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

PRO changebounds_start, event

  forward_function textbox
  Widget_Control, event.top, Get_UValue=info
  
  if (event.clicks EQ 2) then begin
    Widget_Control, event.ID, GET_VALUE=value
    full_string = strsplit(value, ':', /extract)
    varname = textbox(Group_Leader=event.top, label='Coordinate : ', Cancel=cancelled, XSize=200, $
      Value=full_string[1L], title='Provide New Coordinate ...')
    if (cancelled NE 1) then begin
      old_coord = strtrim(string(full_string[1],format='(f10.2)'),2)
      full_string[1] = strtrim(string(varname,format='(f10.2)'),2)
      widget_control, event.ID, Set_Value=strjoin(full_string,': ')
      case strtrim(full_string[0],2) of
        'UL Easting':  begin
          index = where(strmatch(strtrim(string(info.xMin,format='(f10.2)'),2), old_coord), count)
          if (count gt 0) then info.xMin[index] = double(full_string[1])
        end
        'LR Easting':  begin
          index = where(strmatch(strtrim(string(info.xMax,format='(f10.2)'),2), old_coord), count)
          if (count gt 0) then info.xMax[index] = double(full_string[1])
        end
        'UL Northing': begin
          index = where(strmatch(strtrim(string(info.yMax,format='(f10.2)'),2), old_coord), count)
          if (count gt 0) then info.yMax[index] = double(full_string[1])
        end
        'LR Northing': begin
          index = where(strmatch(strtrim(string(info.yMin,format='(f10.2)'),2), old_coord), count)
          if (count gt 0) then info.yMin[index] = double(full_string[1])
        end
      endcase
      Widget_Control, event.top, Set_UValue=info
    endif else begin
      Widget_Control, event.ID, Set_Value=value
    endelse
  endif
  
END
