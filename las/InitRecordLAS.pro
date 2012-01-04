;+
; NAME:
;
;       InitRecordLAS
;
; PURPOSE:
;
;       This function initializes a structure to read a variable length record from an .las
;       lidar file.
;
;       For more information on the .las lidar data format, see http://www.lasformat.org
;
; AUTHOR:
;
;       David Streutker
;       Boise Center Aerospace Laboratory
;       Idaho State University
;       322 E. Front St., Ste. 240
;       Boise, ID  83702
;       http://geology.isu.edu/BCAL
;
; CALLING SEQUENCE:
;
;       record = InitRecordLAS(EVLR=EVLR,NODATA=NODATA)
;
; RETURN VALUE:
;
;       The function returns a structure corresponding to the variable length record of the .las
;       file specification. Set the NODATA keyword to return a structure that does not contain
;       the data pointer. Set the EVLR keyword to return an extended variable length record.
;
; MODIFICATION HISTORY:
;
;       Written by David Streutker, July 2007.
;       Added option to initialise an extended variable length record. John Armston, Nov 2010.
;
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright � 2007 David Streutker, Idaho State University.
;
; This software is provided "as-is", without any express or
; implied warranty. In no event will the authors be held liable
; for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; For more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################

function InitRecordLAS, eVLR=eVLR, noData=noData

  compile_opt idl2
  
  ; Define the variable length header structure
  if keyword_set(eVLR) then begin
    record = {formatR2,                       $
      signature       : 0US,                  $ ; Record signature
      userID          : bytarr(16),           $ ; User ID
      recordID        : 0US,                  $ ; Record ID
      recordLength    : 0ULL,                 $ ; Record length after header, 8 bytes
      description     : bytarr(32)            $ ; Description
      }
      if ~ keyword_set(noData) then record = {formatR3, inherits formatR2, data : ptr_new(/allocate)}    ; Data pointer
  endif else begin
    record = {formatR0,                       $
      signature       : 0US,                  $ ; Record signature
      userID          : bytarr(16),           $ ; User ID
      recordID        : 0US,                  $ ; Record ID
      recordLength    : 0US,                  $ ; Record length after header, 2 bytes
      description     : bytarr(32)            $ ; Description
      }
      if ~ keyword_set(noData) then record = {formatR1, inherits formatR0, data : ptr_new(/allocate)}    ; Data pointer
  endelse
  
  return, record
  
end
