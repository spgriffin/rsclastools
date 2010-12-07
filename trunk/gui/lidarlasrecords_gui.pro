;+
; NAME:
;
;   LidarLASrecords_GUI
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
;    Written by John Armston, 2010.
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

PRO LidarLASrecords_GUI

  forward_function InitHeaderLAS, InitRecordLAS, InitWPDLAS
  
  names = [ $
    'Reserved', $
    'User ID', $
    'Record ID', $
    'Record length after header', $
    'Description' $
    ]
    
  wpdNames = [ $
    'Bit per sample', $
    'Waveform compression type', $
    'Number of samples', $
    'Temporal sample spacing', $
    'Digitizer gain', $
    'Digitizer offset' $
    ]
    
  ; Base widget.
  tlb = WIDGET_BASE(Column=1, TITLE='LAS Variable Length Record Headers', xsize=!QRSC_LIDAR_XSIZE, tlb_frame_attr=1, ysize=430)
  
  ; Tree widget
  wTree = WIDGET_TREE(tlb, ysize=400)
  
  ; Open file/s
  infile = dialog_pickfile(filter='*.las', /fix_filter, /multiple_files, /must_exist, title='Please Select LAS File/s (Ctrl-click to select multiple files)')
  if (infile[0] EQ '') then return
  
  ; Loop through each selected file
  if (n_elements(infile) GT 0L) then begin
  
    wtRoot = lonarr(n_elements(infile))
    for i = 0L, n_elements(infile)-1L, 1L do begin
    
      ; Make filename a folder at the root level of the visible tree.
      wtRoot[i] = WIDGET_TREE(wTree, VALUE=infile[i], /FOLDER)
      
      ; Read file header
      header = InitHeaderLAS()
      openr, inputLun, infile[i], /get_lun, /swap_if_big_endian
      readu, inputLun, header
      if (header.pointFormat ge 4) then begin
        wdp = 0ULL
        readu, inputLun, wdp
        header = create_struct(header, 'wdp', wdp)
      endif
      
      ; Read variable length records
      if (header.nRecords gt 0) then begin
      
        ; Read variable length records
        wtRecord = lonarr(header.nRecords)
        
        for a=0,header.nRecords-1 do begin
        
          ; Read the VLR
          wtRecord[a] = WIDGET_TREE(wtRoot[i], VALUE='Variable Length Record ' + strtrim(a+1,2), /FOLDER)
          record = InitRecordLAS(/noData)
          readu, inputLun, record
          
          ; Check if there is data and skip if so
          if (record.recordLength gt 0) then begin
            if (Record.recordID ge 100 and Record.recordID lt 356) then begin
              wpdHeader = InitWPDLAS()
              readu, inputLun, wpdHeader
            endif else begin
              point_lun, -inputLun, pos
              point_lun, inputLun, pos + record.recordLength
            endelse
          endif
          
          ; Create leaves
          wtLeaf = lonarr(n_tags(record))
          struct_names = tag_names(record)
          for j = 0L, n_tags(record)-1L, 1L do begin
            recordStr = record.(j)
            wtLeaf[j] = WIDGET_TREE(wtRecord[a], VALUE = strtrim(names[j],2) + ': ' + strtrim(recordStr,2))
          endfor
          
          ; Add WPD header if necessary
          if (n_elements(wpdHeader) gt 0) then begin
            wtWPDheader = WIDGET_TREE(wtRecord[a], VALUE='WDP Header', /FOLDER)
            wtLeaf = lonarr(n_tags(wpdHeader))
            struct_names = tag_names(wpdHeader)
            for j = 0L, n_tags(wpdHeader)-1L, 1L do begin
              case wpdNames[j] of
                'Bit per sample': wpdHeaderStr = string(wpdHeader.(j),format='(I)')
                'Waveform compression type': wpdHeaderStr = string(wpdHeader.(j),format='(I)')
                else: wpdHeaderStr = wpdHeader.(j)
              endcase
              wtLeaf[j] = WIDGET_TREE(wtWPDheader, VALUE = strtrim(wpdNames[j],2) + ': ' + strtrim(wpdHeaderStr,2))
            endfor
          endif
          
        endfor
        
      endif else begin
      
        wtLeaf = lonarr(1)
        wtLeaf[0] = WIDGET_TREE(wtRoot[i], VALUE = 'No Variable Length Records')
        
      endelse
      
      ; Close input file
      free_lun, inputLun
      
    endfor
    
    ; Create a 'Close' button
    wDone = WIDGET_BUTTON(tlb, VALUE='Close', UVALUE='Quit')
    
    ; Realize the widgets and run XMANAGER to manage them
    Widget_Control, tlb, /realize
    XManager, 'RSC_LAS_Tools', tlb, /No_Block
    
  endif
  
  ; Make file directory cwd
  cd, file_dirname(infile[0])
  
END


