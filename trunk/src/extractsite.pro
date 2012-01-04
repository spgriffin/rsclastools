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
  nSites = n_elements(name)
  for i = 0L, n_elements(infile)-1L do begin
  
    ; If there are no sites remotely within the bounds of this LAS file, then skip
    ; Need to do bounds in case of site bounds being bigger than las bounds
    ReadHeaderLAS, infile[i], header
    px = [header.xMin,header.xMax,header.xMax,header.xMin]
    py = [header.yMax,header.yMax,header.yMin,header.yMin]
    inside = Obj_New('IDLanROI', px, py)
    iwidth = header.xMax - header.xMin
    ilength = header.yMax - header.yMin
    inside_index = lonarr(nSites)
    gt_index = lonarr(nSites)   
    for j = 0L, n_elements(name)-1L do begin
      px_p = [easting[j]-major_axis[j]/2.0,easting[j]+major_axis[j]/2.0,easting[j]+major_axis[j]/2.0,easting[j]-major_axis[j]/2.0]
      py_p = [northing[j]+major_axis[j]/2.0,northing[j]+major_axis[j]/2.0,northing[j]-major_axis[j]/2.0,northing[j]-major_axis[j]/2.0]
      if (iwidth gt major_axis[j] and iwidth gt major_axis[j]) then begin
        inside_index[j] = max(inside->ContainsPoints(px_p,py_p))
      endif else begin
        inside_p = Obj_New('IDLanROI', px_p, py_p)
        inside_index[j] = inside->ContainsPoints(header.xMax-iwidth/2.0,header.yMax-ilength/2.0)
        Obj_Destroy, inside_p
        gt_index[j] = 1
      endelse
    endfor
    index = where(logical_or(inside_index gt 0, gt_index eq 1), count)
    if (count gt 0) then begin
    
      ; Read the input file
      ReadLAS, infile[i], header, data
      fparts = strsplit(infile[i], '.', /extract)
      progressBar -> SetProperty, text=strtrim(infile[i], 2)
      bcount += (nSites - count)
      
      ; Loop through each extraction location
      for j = 0L, count-1L do begin
      
        ; Extract returns within radius/block
        case shape of
          1: begin ; Rectangle
            x = ((data.x * header.xScale + header.xOffset) - easting[index[j]]) * cos(azimuth[index[j]]) - $
              ((data.y * header.yScale + header.yOffset) - northing[index[j]]) * sin(azimuth[index[j]])
            y = ((data.x * header.xScale + header.xOffset) - easting[index[j]]) * sin(azimuth[index[j]]) + $
              ((data.y * header.yScale + header.yOffset) - northing[index[j]]) * cos(azimuth[index[j]])
            distancex = sqrt((x / major_axis[index[j]])^2)
            distancey = sqrt((y / minor_axis[index[j]])^2)
            idx = where((distancex LE 1.0) AND (distancey LE 1.0), cnt)
            if (cnt GT 0L) then begin
              dataSub = data[idx]
            endif else begin
              dataSub = 0
            endelse
          end
          2: begin ; Ellipse
            x = ((data.x * header.xScale + header.xOffset) - easting[index[j]]) * cos(azimuth[index[j]]) - $
              ((data.y * header.yScale + header.yOffset) - northing[index[j]]) * sin(azimuth[index[j]])
            y = ((data.x * header.xScale + header.xOffset) - easting[index[j]]) * sin(azimuth[index[j]]) + $
              ((data.y * header.yScale + header.yOffset) - northing[index[j]]) * cos(azimuth[index[j]])
            distance = sqrt((x / major_axis[index[j]])^2 + (y / minor_axis[index[j]])^2)
            idx = where(distance LE 1.0, cnt)
            if (cnt GT 0L) then begin
              dataSub = data[idx]
            endif else begin
              dataSub = 0
            endelse
          end
          else: return
        endcase
        if (size(dataSub, /type) NE 8) then continue
        
        ; Generate the header
        outputHeader = header
        outputHeader.xMin = min(dataSub.x) * header.xScale + header.xOffset
        outputHeader.xMax = max(dataSub.x) * header.xScale + header.xOffset
        outputHeader.yMin = min(dataSub.y) * header.yScale + header.yOffset
        outputHeader.yMax = max(dataSub.y) * header.yScale + header.yOffset
        outputHeader.zMin = min(dataSub.z) * header.zScale + header.zOffset
        outputHeader.zMax = max(dataSub.z) * header.zScale + header.zOffset
        outputHeader.nPoints = count
        outputHeader.nReturns = histogram([ishft(ishft(dataSub.nReturn,5),-5)], min=1, max=5)
        outputHeader.nRecords = 0
        case outputHeader.pointFormat of
          0: begin
            outputHeader.headerSize = 227US
            outputHeader.dataOffset = 229UL
            outputHeader.pointLength = 20US
          end
          1: begin
            outputHeader.headerSize = 227US
            outputHeader.dataOffset = 227UL
            outputHeader.pointLength = 28US
          end
          2: begin
            outputHeader.headerSize = 227US
            outputHeader.dataOffset = 227UL
            outputHeader.pointLength = 26US
          end
          3: begin
            outputHeader.headerSize = 227US
            outputHeader.dataOffset = 227UL
            outputHeader.pointLength = 34US
          end
          4: begin
            outputHeader.headerSize = 235US
            outputHeader.dataOffset = 235UL
            outputHeader.pointLength = 57US
            outputHeader.wdp = 0LL
          end
          5: begin
            outputHeader.headerSize = 235US
            outputHeader.dataOffset = 235UL
            outputHeader.pointLength = 63US
            outputHeader.wdp = 0LL
          end
        endcase
        if (total(outputHeader.nReturns) NE outputHeader.nPoints) then begin
          outputHeader.nReturns[0] += (outputHeader.nPoints - total(outputHeader.nReturns))
        endif
        
        ; Write output
        outputFile = strtrim(fparts[0],2) + '_' + shape_name + '_' + strtrim(name[index[j]],2) + '.' + fparts[1]
        WriteLAS, outputFile, outputHeader, dataSub
        
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
