;+
; NAME:
;
;   LAS2ASCII
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

PRO LAS2ASCII, infile, columns, null, splitsize=splitsize

  ; Start progress bar
  progressBar=Obj_New('progressbar', Color='Forest Green', Text='Converting data...', title='LAS to ASCII...', /fast_loop)
  progressBar->Start
  
  ; Error handling
  catch, theError
  if theError ne 0 then begin
    if (n_elements(progressbar) EQ 1) then progressbar->Destroy
    catch, /cancel
    help, /last_message, output=errText
    errMsg = dialog_message(errText, /error, title='Error exporting LAS')
    return
  endif
  
  ; Variables
  if not keyword_set(splitsize) then splitsize = 1000000D
  fields = ['"Time"', $
    '"Easting"', $
    '"Northing"', $
    '"Elevation"', $
    '"Intensity"', $
    '"Height"', $
    '"Return Number"', $
    '"Number of Returns"', $
    '"Classification"', $
    '"Scan Direction Flag"', $
    '"Edge of Flight Line"', $
    '"Scan Angle Rank"', $
    '"User Data"', $
    '"Point Source ID"']
    
  ; Loop through LAS files
  for i = 0L, n_elements(infile) - 1L do begin
  
    ; Open the ASCII file
    progressBar -> SetProperty, Text=strtrim(infile[i],2)
    progressbar->Update, 0D
    fparts = strsplit(infile[i], '.', /extract)
    outputFile = fparts[0] + '.csv'
    openw, lun, outputFile, /get_lun
    index = where(columns EQ 1, count)
    if (count EQ 0) then return
    printf, lun, strjoin(fields[index], ',')
    
    ; Open LAS file
    assocLun = 1
    ReadLAS, infile[i], header, all_data, assocLun=assocLun
    no_points = header.nPoints
    bcount = 0D
    
    ; Work out number of splits
    buffer = splitsize / 10D
    if (splitsize GT (header.nPoints + buffer)) then begin
      no_splits = 1
    endif else begin
      no_splits = ceil(header.nPoints / splitsize)
    endelse
    no_sp = round(no_points / no_splits)
    
    ; Loop through each split
    for j = 0L, no_splits - 1L, 1L do begin
    
      ; Determine split
      case j of
        0: begin
          lower = 0L
          upper = (no_sp - 1L)
        end
        no_splits-1L: begin
          lower = j * no_sp
          upper = no_points - 1L
        end
        else: begin
          lower = j * no_sp
          upper = lower + no_sp
        end
      endcase
      
      ; Read the split
      nPoints = upper - lower + 1L
      sp_index = lindgen(nPoints) + lower
      dataStr = InitDataLAS(pointFormat=header.pointFormat)
      data = replicate(dataStr, nPoints)
      for k = 0L, nPoints-1L do begin
        data[k] = all_data[sp_index[k]]
      endfor
      
      ; Generate fields where necessary
      if (columns[5] EQ 1) then begin
        case string(header.systemID) of
          'Height: Source': begin
            height = data.(8) * 0.01
          end
          'Height: Elev': begin
            height = data.(2) * header.zScale + header.zOffset
          end
          else: begin
            progressBar->Destroy
            errMsg = dialog_message('Point heights not calculated.', /error, title='LAS2Ascii.pro')
            return
          end
        endcase
      endif else begin
        height = replicate(null, nPoints)
      endelse
      
      return_n = (columns[6] EQ 1) ? ishft(ishft(data.nreturn,5),-5) : replicate(null, nPoints)
      n_return = (columns[7] EQ 1) ? ishft(ishft(data.nreturn,2),-5) : replicate(null, nPoints)
      scan_flag = (columns[9] EQ 1) ? ishft(ishft(data.nreturn,1),-7) : replicate(null, nPoints)
      edge_flag = (columns[10] EQ 1) ? ishft(ishft(data.nreturn,0),-7) : replicate(null, nPoints)
      
      ; Write each return to the ASCII file
      no_fields = total(columns)
      for k = 0D, nPoints - 1D, 1D do begin
        line = [string(data[k].Time, format='(f12.5)'), $
          string(data[k].East * header.xScale + header.xOffset, format='(f10.2)'), $
          string(data[k].North * header.yScale + header.yOffset, format='(f10.2)'), $
          string(data[k].Elev * header.zScale + header.zOffset, format='(f8.2)'), $
          string(data[k].Inten, format='(i4)'), $
          string(Height[k], format='(f8.2)'), $
          string(return_n[k], format='(i1)'), $
          string(n_return[k], format='(i1)'), $
          string(data[k].Class, format='(i1)'), $
          string(scan_flag[k], format='(i1)'), $
          string(edge_flag[k], format='(i1)'), $
          string(data[k].Angle, format='(i3)'), $
          string(data[k].User, format='(i4)'), $
          string(data[k].Source, format='(i)')]
        line = strcompress(strjoin(line[index], ','), /remove_all)
        printf, lun, line
        bcount += 1D
        progressbar->Update, bcount / double(no_points) * 100D
      endfor
      
      ; Clean up arrays and close output file
      MyDelVar, data
      MyDelVar, height
      MyDelVar, return_n
      MyDelVar, n_return
      MyDelVar, scan_flag
      MyDelVar, edge_flag
      
      ; Check progress bar
      if progressBar->CheckCancel() then begin
        ok=Dialog_Message('LAS export cancelled')
        progressBar->Destroy
        return
      endif
      
    endfor
    
    ; Close ascii file
    close, lun
    free_lun, lun
    
  endfor
  
  ; End progress bar
  progressbar->Destroy
  
END
