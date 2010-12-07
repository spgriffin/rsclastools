;+
; NAME:
;
;   filterReturns
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

FUNCTION filterReturns, data, type=type, n=n, limit=limit

  ; Error handling
  catch, theError
  if theError ne 0 then begin
    catch, /cancel
    help, /last_message, output=errText
    errMsg = dialog_message(errText, /error, title='Error filtering returns.')
    return, -1
  endif
  
  case type of
  
    1: begin ; nth return
      if not keyword_set(n) then n=1
      return_n = ishft(ishft(data.nreturn,5),-5)
      index = where(return_n EQ n, count)
      result = bytarr(n_elements(data.(0)))
      if (count GT 0L) then result[index] = 1
    end
    
    2: begin ; Last return
      return_n = ishft(ishft(data.nreturn,5),-5)
      n_return = ishft(ishft(data.nreturn,2),-5)
      index = where((return_n EQ n_return) EQ 1, count)
      result = bytarr(n_elements(data.(0)))
      if (count GT 0L) then result[index] = 1
    end
    
    3: begin ; Unique returns
      xMax = max(data.x, min=xMin)
      yMax = max(data.y, min=yMin)
      zMin = min(data.z)
      eastRange  = ulong64(xMax - xMin)
      northRange = ulong64(yMax - yMin)
      uniqCoords = eastRange * northRange * (data.z  - zMin) $
        + eastRange *              (data.y - yMin) $
        +                          (data.x  - xMin)
      uniqCoords = uniq(bsort(uniqCoords))
      result = bytarr(n_elements(data.(0)))
      result[uniqCoords] = 1
    end
    
    ; For the class field, bits 1-4 are the acctual classification (32 possible values)
    ; So the following should apply
    ;   reads, '00100000', value, format='(B)' - For ground (2) returns
    ;   reads, '00010000', value, format='(B)' - For unclassified (1) returns
    ;   class = ishft(byte(value),-4)
    ; But every LAS file I've ever received, I've had to apply the following
    ;   reads, '00000001', value, format='(B)' - For ground (2) returns
    ;   reads, '00000010', value, format='(B)' - For unclassified (1) returns
    ;   class = ishft(ishft(byte(value),-4),4)
    
    4: begin ; Ground returns
      class = ishft(ishft(data.Class,4),-4)
      index = where(class eq 2, count)
      result = bytarr(n_elements(data.(0)))
      if (count GT 0L) then result[index] = 1
    end
    
    5: begin ; Non-ground returns
      class = ishft(ishft(data.Class,4),-4)
      index = where(data.Class ne 2, count)
      result = bytarr(n_elements(data.(0)))
      if (count GT 0L) then result[index] = 1
    end
    
    6: begin ; Duplicate returns (Duplicate returns can be unique)
      return_n = ishft(ishft(data.nreturn,5),-5)
      index = where(return_n GT 1, count)
      result = bytarr(n_elements(data.(0)))
      if (count GT 0) then begin
        elev_diff = data[index].(2) - data[index-1L].(2)
        idx = where(elev_diff LT limit, cnt)
        if (cnt GT 0) then result[index[idx]] = 1
      endif
    end
    
    7: begin ; Singular returns
      n_return = ishft(ishft(data.nreturn,2),-5)
      index = where(n_return EQ 1, count)
      result = bytarr(n_elements(data.(0)))
      if (count GT 0L) then result[index] = 1
    end
    
    8: begin ; First returns (n > 1)
      if not keyword_set(n) then n=1
      return_n = ishft(ishft(data.nreturn,5),-5)
      n_return = ishft(ishft(data.nreturn,2),-5)
      index = where((return_n EQ n) and (n_return GT 1), count)
      result = bytarr(n_elements(data.(0)))
      if (count GT 0L) then result[index] = 1
    end
    
    9: begin ; Last returns (n > 1)
      return_n = ishft(ishft(data.nreturn,5),-5)
      n_return = ishft(ishft(data.nreturn,2),-5)
      index = where(((return_n EQ n_return) EQ 1) and (n_return GT 1), count)
      result = bytarr(n_elements(data.(0)))
      if (count GT 0L) then result[index] = 1
    end
    
    else: result = replicate(1L, n_elements(data.(0)))
    
  endcase
  
  ; Return the index of good points
  return, result
  
END
