;+
; NAME:
;
;   LidarLASinfo_GUI
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

PRO LidarLASinfo_GUI

  names = [ $
    'File signature', $
    'File source ID', $
    'Reserved', $
    'Project ID - GUID data 1', $
    'Project ID - GUID data 2', $
    'Project ID - GUID data 3', $
    'Project ID - GUID data 4', $
    'Version major', $
    'Version minor', $
    'System identifier', $
    'Generating software', $
    'File creation day of year', $
    'File creation year', $
    'Header size', $
    'Offset to point data', $
    'Number of variable length records', $
    'Point data format ID', $
    'Point data record length', $
    'Number of point records', $
    'Number of points by return', $
    'X Scale factor', $
    'Y Scale factor', $
    'Z Scale factor', $
    'X Offset', $
    'Y Offset', $
    'Z Offset', $
    'Max Easting', $
    'Min Easting', $
    'Max Northing', $
    'Min Northing', $
    'Max Elevation', $
    'Min Elevation' $
    ]
    
  ; Base widget.
  tlb = WIDGET_BASE(Column=1, TITLE='LAS Info', xsize=!QRSC_LIDAR_XSIZE, tlb_frame_attr=1, ysize=430)
  
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
      ReadHeaderLAS, infile[i], header
      
      ; Create leaves
      wtLeaf = lonarr(n_tags(header))
      struct_names = tag_names(header)
      for j = 0L, n_tags(header)-1L, 1L do begin
        if (strtrim(strlowcase(struct_names[j]),2) EQ 'nreturns') then begin
          wtLeaf[j] = WIDGET_TREE(wtRoot[i], VALUE = strtrim(names[j],2) + ': [' + strjoin(strtrim(header.(j),2),', ') + ']')
        endif else begin
          case strtrim(strupcase(struct_names[j]),2) of
            'VERSIONMAJOR': headerStr = string(header.(j), format='(I)')
            'VERSIONMINOR': headerStr = string(header.(j), format='(I)')
            'POINTFORMAT': headerStr = string(header.(j), format='(I)')
            else: headerStr = header.(j)
          endcase
          wtLeaf[j] = WIDGET_TREE(wtRoot[i], VALUE = strtrim(names[j],2) + ': ' + strtrim(headerStr,2))
        endelse
      endfor
      
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


