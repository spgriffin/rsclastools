;+
; NAME:
;
;   LidarLAScheck_GUI
;
; PURPOSE:
;
;   GUI interface for checking LAS header values
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

PRO LidarLAScheck_GUI

  ; Open file/s
  infile = dialog_pickfile(filter='*.las', /fix_filter, /multiple_files, /must_exist, title='Please Select LAS File/s (Ctrl-click to select multiple files)')
  if (infile[0] EQ '') then return
  
  ; Start progress bar
  bcount = 0
  btotal = n_elements(infile)
  progressBar=Obj_New('progressbar', Color='Forest Green', Text='Reading LAS data...', title='Fixing header values', /fast_loop)
  progressBar->Start
  
  ; Error handling
  catch, theError
  if theError ne 0 then begin
    if (n_elements(progressbar) EQ 1) then progressbar->Destroy
    catch, /cancel
    help, /last_message, output=errText
    errMsg = dialog_message(errText, /error, title='Error fixing LAS file.')
    return
  endif
  
  ; Loop through each selected file
  if (n_elements(infile) GT 0L) then begin
  
    for i = 0L, n_elements(infile)-1L, 1L do begin
    
      progressBar -> SetProperty, Text=strtrim(infile[i],2)
      CheckHeaderLAS, infile[i]
      
      ; Update progress bar
      bcount += 1
      if progressBar->CheckCancel() then begin
        ok=Dialog_Message('LAS header fixing cancelled')
        progressBar->Destroy
        return
      endif
      progressbar->Update,(float(bcount)/btotal)*100.0
      
    endfor
    
  endif
  
  ; End progress bar
  progressbar->Destroy
  
  ; Make file directory cwd
  cd, file_dirname(infile[0])
  
END


