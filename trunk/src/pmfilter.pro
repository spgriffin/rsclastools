;+
; NAME:
;
;   PMFilter
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

PRO PMFilter,data,b_start=b_start,bmax=bmax,dh0=dh0,slope=slope,cell_size=cell_size,progress_string=progress_string,height_threshold=height_threshold

  ; Error handling
  catch, theError
  if theError ne 0 then begin
    if (n_elements(progressbar) EQ 1) then progressbar->Destroy
    catch, /cancel
    help, /last_message, output=errText
    errMsg = dialog_message(errText, /error, title='Error filtering LAS file.')
    return
  endif
  
  ; Set defaults if parameters not provided
  if not keyword_set(b_start) then b_start=1.0
  if not keyword_set(bmax) then bmax=7.0
  if not keyword_set(dh0) then dh0=0.5
  if not keyword_set(slope) then slope=0.3
  if not keyword_set(cell_size) then cell_size=1.0
  if not keyword_set(height_threshold) then height_threshold=0.15
  
  ; Regularise the grid
  counts = hist2d_rilidar(data.x,data.y,reverse_indices,n_cols,n_rows,BIN1=cell_size,BIN2=cell_size, $
    MIN1=min(data.x),MIN2=min(data.y),MAX1=max(data.x),MAX2=max(data.y))
  elevation_grid = float(temporary(counts))
  for i=0L,n_elements(elevation_grid)-1L,1L do begin
    elevation_grid[i]=(reverse_indices[i] NE reverse_indices[i+1L]) ? min(data.z[reverse_indices[reverse_indices[i]:reverse_indices[i+1L]-1L]]) : -1.0
  endfor
  null_flag=elevation_grid EQ -1.0
  null_index=where(null_flag EQ 1,null_index_count,complement=real_index)
  wflag=elevation_grid*0.0
  elevation_grid[null_index]=0.0
  
  ; Determine the maximum elevation difference threshold
  dhmax=max(elevation_grid[real_index])-min(elevation_grid[real_index])
  
  ; Copy array elevation_all to array B.
  B=elevation_grid
  
  ; Determine series of w using a simple linear equation, where w <= maximum window size.
  w=findgen(bmax+1.0)
  
  ; Set the elevation difference threshold as the initial
  dht=dh0
  
  ; Start progress bar
  bcount=0.0
  btotal=0.0
  for i=1L,n_elements(w)-1L,1L do begin
    btemp=float(shift(dist(2.0*w[i]+1.0),w[i],w[i]) LE w[i])
    btotal+=total(btemp*2.0)
  endfor
  if not keyword_set(progress_string) then progress_string='LiDAR'
  progressBar=Obj_New('progressbar',Color='Forest Green',Text=progress_string,title='Elevation filtering...',/fast_loop)
  progressBar->Start
  
  ; for each window radius w
  for q=1L,n_elements(w)-1L,1L do begin
  
    ; Create kernel
    if (w[q] lt b_start) then continue
    kernel_base = float(shift(dist(2.0 * w[q] + 1.0), w[q], w[q]) LE w[q])
    kernel_size = 2.0 * w[q] + 1.0
    
    ; Erosion
    Zf=elevation_grid
    for r=0L,(kernel_size^2)-1L,1L do begin
      if (kernel_base[r] EQ 1.0) then begin
        kernel=fltarr(kernel_size,kernel_size)
        kernel[r]=1.0
        if (null_index_count GT 0) then begin
          elevation_grid[null_index]= !Values.F_NaN
          temp_grid=convol(elevation_grid,kernel,/center,/edge_truncate,/nan,missing=0.0)
          elevation_grid[null_index]=0.0
        endif else begin
          temp_grid=convol(elevation_grid,kernel,/center,/edge_truncate)
        endelse
        Zf = (temp_grid < Zf) + (temp_grid EQ 0.0) * Zf
        Zf[null_index]=0.0
        ; Update progress bar
        bcount+=1
        if progressBar->CheckCancel() then begin
          ok=Dialog_Message('Elevation filter cancelled')
          progressBar->Destroy
          return
        endif
        progressbar->Update,(float(bcount)/btotal)*100.0
      endif
    endfor
    
    ; Dilation
    Z=Zf
    for s=0L,(kernel_size^2)-1L,1L do begin
      if (kernel_base[s] EQ 1.0) then begin
        kernel=fltarr(kernel_size,kernel_size)
        kernel[s]=1.0
        if (null_index_count GT 0) then begin
          Zf[null_index]= !Values.F_NaN
          temp_grid=convol(Zf,kernel,/center,/edge_truncate,/nan,missing=0.0)
          Zf[null_index]=0.0
        endif else begin
          temp_grid=convol(Zf,kernel,/center,/edge_truncate)
        endelse
        Z= temp_grid > Z
        Z[null_index]=0.0
        ; Update progress bar
        bcount+=1
        if progressBar->CheckCancel() then begin
          ok=Dialog_Message('Elevation filter cancelled')
          progressBar->Destroy
          return
        endif
        progressbar->Update,(float(bcount)/btotal)*100.0
      endif
    endfor
    
    ; Classification
    wflag_temp=(elevation_grid-Z) GT dht
    wflag=(wflag_temp*dht)>temporary(wflag)
    
    ; Put the filtered array of points back to array elevation_grid
    elevation_grid=Z
    
    ; Determine the elevation difference threshold
    if (dht GT dhmax) then begin
      dht=dhmax
    endif else begin
      dht=slope * (w[q]-w[q-1]) * cell_size + dh0
    endelse
    
    
  endfor ;end window size loop
  
  ; Designate each cell value as ground, non-ground or unclassified
  type = (wflag GT 0) * 2 + (null_flag EQ 1)
  
  ; Classify original data points
  class=bytarr(n_elements(data.z))
  for i=0L,n_elements(elevation_grid)-1L,1L do begin
    index = (type[i] EQ 1) ?  -1.0 : reverse_indices[reverse_indices[i]:reverse_indices[i+1L]-1L]
    if (index[0] NE -1.0) then begin
      null=min(data.z[index],min_index)
      class[index[min_index]] = (type[i] EQ 2) ? 1B : 2B
    endif
  endfor
  
  ; Detect ground data points omitted in the grid regularisation
  idx1 = where(class EQ 0, cnt1)
  idx2 = where(class EQ 2, cnt2)
  if (cnt1 GT 0) then begin
    triangulate, data.x[idx2], data.y[idx2], triangles
    elev_temp = griddata(data.x[idx2], data.y[idx2], data.z[idx2], method='NaturalNeighbor', $
      triangles=triangles, xout=data.x[idx1], yout=data.y[idx1])
    gnd_idx = where(abs(data.z[idx1] - elev_temp) le height_threshold, gnd_cnt, complement=veg_idx, ncomplement=veg_cnt)
    class[idx1[gnd_idx]] = 2B
    class[idx1[veg_idx]] = 1B
  endif
  
  ; Update Structure
  data=create_struct('Class', class, data)
  
  ; End progress bar
  progressbar->Destroy
  
END
