;+
; NAME:
;
;   LAS2Shapefile
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

PRO LAS2Shapefile, infile, columns, null, splitsize=splitsize

  ; Start progress bar
  progressBar=Obj_New('progressbar', Color='Forest Green', Text='Converting data...', title='LAS to Shapefile...', /fast_loop)
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
  
  ; Keywords
  if not keyword_set(splitsize) then splitsize = 1000000D
  
  ; Loop through LAS files
  for i = 0L, n_elements(infile) - 1L do begin
  
    ; Initialise
    progressBar -> SetProperty, Text=strtrim(infile[i],2)
    progressbar->Update, 0D
    fparts = strsplit(infile[i], '.', /extract)
    outputFile = fparts[0] + '.shp'
    
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
      if (columns[3] EQ 1) then begin
        case string(header.systemID) of
          'Height: Source': begin
            height = data.(8) * 0.01
          end
          'Height: Elev': begin
            height = data.(2) * header.zScale + header.zOffset
          end
          else: begin
            progressBar->Destroy
            errMsg = dialog_message('Point heights not calculated. Export cancelled.', /error, title='LAS2Shapefile.pro')
            return
          end
        endcase
      endif else begin
        height = replicate(null, nPoints)
      endelse
      return_n = (columns[4] EQ 1) ? ishft(ishft(data.nreturn,5),-5) : replicate(null, nPoints)
      n_return = (columns[5] EQ 1) ? ishft(ishft(data.nreturn,2),-5) : replicate(null, nPoints)
      scan_flag = (columns[7] EQ 1) ? ishft(ishft(data.nreturn,1),-7) : replicate(null, nPoints)
      edge_flag = (columns[8] EQ 1) ? ishft(ishft(data.nreturn,0),-7) : replicate(null, nPoints)
      
      ;Create the shapefile and define the entity type to Polygon
      mynewshape=OBJ_NEW('IDLffShape', outputFile, /update, entity_type=11)
      
      ;Set the attribute definitions for the Shapefile
      if (columns[0] EQ 1) then mynewshape->AddAttribute, 'Time', 5, 13, precision=5
      if (columns[1] EQ 1) then mynewshape->AddAttribute, 'Elevation', 5, 9, precision=2
      if (columns[2] EQ 1) then mynewshape->AddAttribute, 'Intensity', 3, 4
      if (columns[3] EQ 1) then mynewshape->AddAttribute, 'Height', 5, 9, precision=2
      if (columns[4] EQ 1) then mynewshape->AddAttribute, 'ReturnN', 3, 2
      if (columns[5] EQ 1) then mynewshape->AddAttribute, 'NReturn', 3, 2
      if (columns[6] EQ 1) then mynewshape->AddAttribute, 'Class', 3, 2
      if (columns[7] EQ 1) then mynewshape->AddAttribute, 'Scan', 3, 2
      if (columns[8] EQ 1) then mynewshape->AddAttribute, 'Edge', 3, 2
      if (columns[9] EQ 1) then mynewshape->AddAttribute, 'Angle', 3, 4
      if (columns[10] EQ 1) then mynewshape->AddAttribute, 'User', 3, 4
      if (columns[11] EQ 1) then mynewshape->AddAttribute, 'Source', 3, 5
      if (columns[12] EQ 1) then mynewshape->AddAttribute, 'Red', 3, 4
      if (columns[13] EQ 1) then mynewshape->AddAttribute, 'Green', 3, 4
      if (columns[14] EQ 1) then mynewshape->AddAttribute, 'Blue', 3, 5
      
      for k = 0D, nPoints - 1D, 1D do begin
      
        ;Create structure for the entity
        entNew = {IDL_SHAPE_ENTITY}
        
        ; Define the values for the new entity
        dims = size(vertices, /dimensions)
        entNew.SHAPE_TYPE = 11
        entNew.ISHAPE = bcount
        entNew.BOUNDS = [data[k].East * header.xScale + header.xOffset, data[k].North * header.yScale + header.yOffset, $
          data[k].Elev * header.zScale + header.zOffset, 0.0, $
          data[k].East * header.xScale + header.xOffset, data[k].North * header.yScale + header.yOffset, $
          data[k].Elev * header.zScale + header.zOffset, 0.0]
        entNew.N_VERTICES = 1
        
        ;Create structure for the attributes
        attrNew = mynewshape ->GetAttributes(/ATTRIBUTE_STRUCTURE)
        
        ;Define the values for the attributes
        columns_index = total(columns, /cumulative) - 1L
        if (columns[0] EQ 1) then attrNew.(columns_index[0]) = data[k].Time
        if (columns[1] EQ 1) then attrNew.(columns_index[1]) = data[k].Elev * header.zScale + header.zOffset
        if (columns[2] EQ 1) then attrNew.(columns_index[2]) = data[k].Inten
        if (columns[3] EQ 1) then attrNew.(columns_index[3]) = height[k]
        if (columns[4] EQ 1) then attrNew.(columns_index[4]) = return_n[k]
        if (columns[5] EQ 1) then attrNew.(columns_index[5]) = n_return[k]
        if (columns[6] EQ 1) then attrNew.(columns_index[6]) = data[k].Class
        if (columns[7] EQ 1) then attrNew.(columns_index[7]) = scan_flag[k]
        if (columns[8] EQ 1) then attrNew.(columns_index[8]) = edge_flag[k]
        if (columns[9] EQ 1) then attrNew.(columns_index[9]) = data[k].Angle
        if (columns[10] EQ 1) then attrNew.(columns_index[10]) = data[k].User
        if (columns[11] EQ 1) then attrNew.(columns_index[11]) = data[k].Source
        if (columns[12] EQ 1) then attrNew.(columns_index[12]) = data[k].Red
        if (columns[13] EQ 1) then attrNew.(columns_index[13]) = data[k].Green
        if (columns[14] EQ 1) then attrNew.(columns_index[14]) = data[k].Blue
        
        ;Add the new entity to the shapefile
        mynewshape -> PutEntity, entNew
        
        ;Add the attributes to the shapefile
        mynewshape -> SetAttributes, bcount, attrNew
        
        ; Clean up the entity
        mynewshape->IDLffShape::DestroyEntity, entNew
        
        ; Update progress bar
        bcount += 1D
        progressbar->Update, bcount / double(nPoints) * 100D
        
      endfor
      
      ; Clean up arrays
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
    
    ;Close the shapefile
    obj_destroy, mynewshape
    free_lun, assocLun
    
  endfor
  
  ; End progress bar
  progressbar->Destroy
  
END
