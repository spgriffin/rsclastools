;+
; NAME:
;
;   lidartile_start
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

PRO LidarTile_start, event

  Widget_Control, event.top, Get_UValue=info
  
  ; Get variables
  tileXsize = info.field1->Get_Value()
  tileYsize = info.field2->Get_Value()
  Widget_Control, info.mergeflag, Get_Value=mergeflag
  Widget_Control, event.top, /Destroy
  resolution = 1.0
  
  ; Start progress bar
  bcount = 0
  btotal = (mergeflag eq 1) ? 1L : n_elements(info.infile)
  progressBar=Obj_New('progressbar', Color='Forest Green', Text='Tiling LAS file...', title='Spatial Tiling', /fast_loop)
  progressBar->Start
  if (mergeflag eq 1) then begin
    tileStruct= SurfaceTile(info.infile, tileXsize=tileXsize,tileYsize=tileYsize, resolution=resolution)
    bcount += 1
    if progressBar->CheckCancel() then begin
      ok=Dialog_Message('Tiling cancelled')
      progressBar->Destroy
      return
    endif
    progressbar->Update,(float(bcount)/btotal)*100.0
  endif else begin
    for i = 0L, n_elements(info.infile)-1L, 1L do begin
      tileStruct= SurfaceTile(info.infile[i], tileXsize=tileXsize,tileYsize=tileYsize, resolution=resolution)
      bcount += 1
      if progressBar->CheckCancel() then begin
        ok=Dialog_Message('Tiling cancelled')
        progressBar->Destroy
        return
      endif
      progressbar->Update,(float(bcount)/btotal)*100.0
    endfor
  endelse
  
  ; End progress bar
  progressbar->Destroy
  
  ; Make file directory cwd
  cd, file_dirname(info.infile[0])
  
END
