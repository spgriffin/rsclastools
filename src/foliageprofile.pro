;+
; NAME:
;
;   FoliageProfile
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

PRO FoliageProfile, infile, type, null, binsize, height_threshold, rhovg_method, rhovg_percentile, constant, max_height

  forward_function HeightProfile, filterReturns
  
  ; Start progress bar
  bcount = 0
  btotal = n_elements(infile)
  progressBar=Obj_New('progressbar', Color='Forest Green', Text='Deriving vertical profiles...', title='Vertical Profile', /fast_loop)
  progressBar->Start
  
  ; Get type name
  case type of
    1: name = 'Counts'
    2: name = 'Intensity'
    else: return
  endcase
  
  ; Error handling
  catch, theError
  if theError ne 0 then begin
    if (n_elements(progressbar) EQ 1) then progressbar->Destroy
    catch, /cancel
    help, /last_message, output=errText
    errMsg = dialog_message(errText, /error, title='Error creating vertical profile.')
    return
  endif
  
  ; Loop through each input file
  for i = 0L, n_elements(infile)-1L do begin
  
    ; Read the input file and get veg heights
    ReadLAS, infile[i], header, data
    fparts = strsplit(infile[i], '.', /extract)
    progressBar -> SetProperty, Text=strtrim(infile[i],2)
    
    ; Get heights
    case string(header.systemID) of
      'Height: Source': begin
        height = data.source * 0.01
      end
      'Height: Elev': begin
        height = data.z * header.zScale + header.zOffset
      end
      else: begin
        progressBar->Destroy
        errMsg = dialog_message('Point heights not calculated.', /error, title='FoliageProfile.pro')
        return
      end
    endcase
    
    ; Get profile
    index = where(height GT max_height, count, complement=no_birds)
    if (count GT 0) then begin
      data = data[no_birds]
      height = height[no_birds]
    endif
    first = filterReturns(data, type=1, n=1)
    iFC = HeightProfile(height, data.inten, first, null, binsize, height_threshold, rhovg_method, rhovg_percentile, $
      counts, locations, rhov_rhog, constant, count_profile, intensity_profile)
      
    ; Set profile type
    if (type eq 1) then begin
      if (n_elements(count_profile) eq 0) then begin
        continue
      endif else begin
        profile = temporary(count_profile)
      endelse
    endif else begin
      if (n_elements(intensity_profile) eq 0) then begin
        continue
      endif else begin
        profile = temporary(intensity_profile)
      endelse
    endelse
    
    ; Write output
    outputFile = strtrim(fparts[0],2) + '_VerticalProfile_' + name + '.csv'
    openw, lun, outputFile, /get_lun
    printf, lun, '"Height (m)","Fractional Cover","Gap Probability (P)","Waveform (dP/dz)","Apparant Foliage Profile (dlog(P)/dz)","Relative Apparant Foliage Profile"'
    dims = size(profile, /dimensions)
    for j = 0L, dims[1] - 1L do begin
      line = [reform(string(profile[*,j], format='(f12.6)'))]
      line = strcompress(strjoin(line, ','), /remove_all)
      printf, lun, line
    endfor
    if (type EQ 2) then printf, lun, strcompress(strjoin(['"Reflectance_Ratio"', string(rhov_rhog, format='(f9.3)')], ','), /remove_all)
    close, lun
    free_lun, lun
    
    ; Update progress bar
    bcount += 1
    if progressBar->CheckCancel() then begin
      ok=Dialog_Message('Derivation of vertical profile cancelled')
      progressBar->Destroy
      return
    endif
    progressbar->Update, (float(bcount) / btotal) * 100.0
    
  endfor
  
  ; End progress bar
  progressbar->Destroy
  
END
