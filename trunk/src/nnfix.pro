;+
; NAME:
;
;   NNfix
;
; PURPOSE:
;
;
;
; AUTHOR:
;
;   Nick Goodwin
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
;    Written by Nick Goodwin, 2010.
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

FUNCTION NNfix, surf, resolution

  X_ind = [-1,0,1,-1,1,-1,0,1]
  Y_ind = [1,1,1,0,0,-1,-1,-1]
  dims = size(surf, /dimensions)
  xrange = dims[0]
  yrange = dims[1]
  
  sub1 = surf[1:xrange-2,1:yrange-2]
  sub2 = surf[2:xrange-1,1:yrange-2]
  sub3 = surf[0:xrange-3,1:yrange-2]
  
  diff1 = abs(sub1 - sub2) / float(resolution)
  diff2 = abs(sub1 - sub3) / float(resolution)
  vals = where(diff1 GT 2.0 AND diff2 GT 2.0, count) ; i.e. slope gt 90 degrees
  
  if (count GT 0) then begin
    yloc = floor(vals / float(xrange-2))
    xloc = (vals - (yloc * float(xrange-2)))
    for i = 0L, count-1L do begin
      surf[xloc[i]+1,yloc[i]+1] = mean(surf[xloc[i]+1+X_ind, yloc[i]+1+Y_ind])
    endfor
  endif
  
  return, surf
  
END
