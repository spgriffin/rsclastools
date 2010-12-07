;+
; NAME:
;
;   ExtractSite
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

PRO ExtractSite, infile, shape, name, easting, northing, major_axis, minor_axis, azimuth

  FORWARD_FUNCTION InitHeaderLAS
  
  ; Start progress bar
  bcount = 0
  btotal = n_elements(infile) * n_elements(name)
  progressBar=Obj_New('progressbar', Color='Forest Green', Text='Extracting data...', title='Subsetting LAS file', /fast_loop)
  progressBar->Start
  
  ; Error handling
  catch, theError
  if theError ne 0 then begin
    if (n_elements(progressbar) EQ 1) then progressbar->Destroy
    catch, /cancel
    help, /last_message, output=errText
    errMsg = dialog_message(errText, /error, title='Error extracting site.')
    return
  endif
  
  ; Get shape name
  case shape of
    1: shape_name = 'Rectangle'
    2: shape_name = 'Ellipse'
    else: return
  endcase
  
  ; Adjust azimuth
  index = where(azimuth GT 270.0, count, complement=index2, ncomplement=count2)
  if (count GT 0) then azimuth[index] = (temporary(azimuth[index]) - 90.0) * !PI / 180.0
  if (count2 GT 0) then azimuth[index2] = (temporary(azimuth[index2]) + 90.0) * !PI / 180.0
  
  ; Loop through each input file
  for i = 0L, n_elements(infile)-1L do begin
  
    ; If there are no sites within the bounds of this LAS file, then skip
    ReadHeaderLAS, infile[i], header
    px = [header.xMin,header.xMax,header.xMax,header.xMin]
    py = [header.yMax,header.yMax,header.yMin,header.yMin]
    inside = Obj_New('IDLanROI', px, py)
    inside_index = inside->ContainsPoints(easting, northing)
    index = where(inside_index gt 0, count)
    if (count gt 0) then begin
    
      ; Read the input file
      ReadLAS, infile[i], header, data
      fparts = strsplit(infile[i], '.', /extract)
      progressBar -> SetProperty, text=strtrim(infile[i], 2)
      
      ; Loop through each extraction location
      for j = 0L, n_elements(name)-1L do begin
      
        ; Check if individual point is inside
        inside_index = inside->ContainsPoints(easting[j], northing[j])
        if (inside_index[0] gt 0) then begin
        
          ; Extract returns within radius/block
          case shape of
            1: begin ; Rectangle
              x = ((data.(0) * header.xScale + header.xOffset) - easting[j]) * cos(azimuth[j]) - $
                ((data.(1) * header.yScale + header.yOffset) - northing[j]) * sin(azimuth[j])
              y = ((data.(0) * header.xScale + header.xOffset) - easting[j]) * sin(azimuth[j]) + $
                ((data.(1) * header.yScale + header.yOffset) - northing[j]) * cos(azimuth[j])
              distancex = sqrt((x / major_axis[j])^2)
              distancey = sqrt((y / minor_axis[j])^2)
              index = where((distancex LE 1.0) AND (distancey LE 1.0), count)
              if (count GT 0L) then begin
                dataSub = data[index]
              endif else begin
                dataSub = 0
              endelse
            end
            2: begin ; Ellipse
              x = ((data.(0) * header.xScale + header.xOffset) - easting[j]) * cos(azimuth[j]) - $
                ((data.(1) * header.yScale + header.yOffset) - northing[j]) * sin(azimuth[j])
              y = ((data.(0) * header.xScale + header.xOffset) - easting[j]) * sin(azimuth[j]) + $
                ((data.(1) * header.yScale + header.yOffset) - northing[j]) * cos(azimuth[j])
              distance = sqrt((x / major_axis[j])^2 + (y / minor_axis[j])^2)
              index = where(distance LE 1.0, count)
              if (count GT 0L) then begin
                dataSub = data[index]
              endif else begin
                dataSub = 0
              endelse
            end
            else: return
          endcase
          if (size(dataSub, /type) NE 8) then continue
          
          ; Generate the header
          outputHeader = header
          outputHeader.xMin = min(dataSub.(0)) * 0.01D
          outputHeader.xMax = max(dataSub.(0)) * 0.01D
          outputHeader.yMin = min(dataSub.(1)) * 0.01D
          outputHeader.yMax = max(dataSub.(1)) * 0.01D
          outputHeader.zMin = min(dataSub.(2)) * 0.01D
          outputHeader.zMax = max(dataSub.(2)) * 0.01D
          outputHeader.nPoints = count
          outputHeader.nReturns = histogram([ishft(ishft(dataSub.nReturn,5),-5)], min=1, max=5)
          outputHeader.xScale = 0.01D
          outputHeader.yScale = 0.01D
          outputHeader.zScale = 0.01D
          outputHeader.nRecords = 0
          case outputHeader.pointFormat of
            0: begin
              outputHeader.headerSize = 227US
              outputHeader.dataOffset = 227UL
            end
            1: begin
              outputHeader.headerSize = 227US
              outputHeader.dataOffset = 227UL
            end
            2: begin
              outputHeader.headerSize = 227US
              outputHeader.dataOffset = 227UL
            end
            3: begin
              outputHeader.headerSize = 227US
              outputHeader.dataOffset = 227UL
            end
            4: begin
              outputHeader.headerSize = 235US
              outputHeader.dataOffset = 235UL
              outputHeader.wdp = 0LL
            end
            5: begin
              outputHeader.headerSize = 235US
              outputHeader.dataOffset = 235UL
              outputHeader.wdp = 0LL
            end
          endcase
          if (total(outputHeader.nReturns) NE outputHeader.nPoints) then begin
            outputHeader.nReturns[0] += (outputHeader.nPoints - total(outputHeader.nReturns))
          endif
          
          ; Write output
          outputFile = strtrim(fparts[0],2) + '_' + shape_name + '_' + strtrim(name[j],2) + '.' + fparts[1]
          WriteLAS, outputFile, outputHeader, dataSub, pointFormat=outputHeader.pointFormat
          
        endif
        
        ; Update progress bar
        bcount += 1
        if progressBar->CheckCancel() then begin
          ok=Dialog_Message('Site extraction cancelled')
          progressBar->Destroy
          return
        endif
        progressbar->Update, (float(bcount) / btotal) * 100.0
        
      endfor
      
      Obj_Destroy, inside
      
    endif else begin
    
      ; Update progress bar
      bcount += n_elements(name)
      if progressBar->CheckCancel() then begin
        ok=Dialog_Message('Site extraction cancelled')
        progressBar->Destroy
        return
      endif
      progressbar->Update, (float(bcount) / btotal) * 100.0
      
    endelse
    
  endfor
  
  ; End progress bar
  progressbar->Destroy
  
END
