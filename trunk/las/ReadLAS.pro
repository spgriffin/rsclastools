;+
; NAME:
;
;       ReadLAS
;
; PURPOSE:
;
;       This program reads the header and point data from a .las file.
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
;       ReadLAS, inputFile, header, data, records=records, check=check, projection=projection, nodata=nodata, assoclun=assoclun
;
; RETURN VALUE:
;
;       The program returns a structure containing the header information and an array of
;       structures containing the point data from the specified .las file.
;
;       Set the RECORDS keyword to a named variable that will contain a structure or array of structures
;       of the variable length records.  If no records exist, a value of -1 is returned.
;
;       Set the CHECK keyword to correct any internal inconsistancies in the header.
;
;       Set the NODATA keyword to prohibit reading the point data and return only the header.
;
;       Set the ASSOCLUN keyword to a named variable to use associated input/output to read the data in the
;       file.  The variable will be returned set the associated LUN value.
;
;       Set the WDP keyword to a named variable to contain the waveform data packets for LAS 1.3 files.
;
; DEPENDENCIES:
;
;       InitHeaderLAS
;       InitRecordLAS
;       InitDataLAS
;       RecordsToProj
;
; MODIFICATION HISTORY:
;
;       Written by David Streutker, March 2006.
;       Changed CLOSE command to FREE_LUN, April 2006
;       Added RECORDS keyword, August 2006
;       Added CHECK keyword, September 2006
;       Added NODATA, PROJECTION, and ASSOCLUN keywords, June 2007
;       Minor change to check keyword code, March 2008. John Armston.
;       Updated for LAS 1.2 format. Removed point records check. 2010. John Armston.
;       Update for LAS 1.3 format. Removed CHECK keyword. Nov 2010. John Armston.
;
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright � 2006 David Streutker, Idaho State University.
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

pro ReadLAS, inputFile, header, data, records=records, noData=noData, assocLun=assocLun, wdp=wdp

  compile_opt idl2
  FORWARD_FUNCTION InitHeaderLAS, InitDataLAS, InitRecordLAS
  
  ; Create the header structure
  header = InitHeaderLAS()
  
  ; Get info about the file.  Then open the file and read the header
  fInfo = file_info(inputFile)
  openr, inputLun, inputFile, /get_lun, /swap_if_big_endian
  readu, inputLun, header
  if (header.pointFormat ge 4) then begin
    wdp = 0ULL
    readu, inputLun, wdp
    header = create_struct(header, 'wdp', wdp)
  endif
  
  ; If the header indicates that the file contains variable length records, read them
  if (header.nRecords gt 0) then begin
  
    ; Define and read variable length records
    records = replicate(InitRecordLAS(), header.nRecords)
    
    for a=0,header.nRecords-1 do begin
    
      ; Read the VLR
      tempRecord = InitRecordLAS(/noData)
      readu, inputLun, tempRecord
      
      ; Read the VLR data
      if (tempRecord.recordLength gt 0) then begin
        ; Check if the record data is a WPD
        if (tempRecord.recordID ge 100 and tempRecord.recordID lt 356) then begin
          dataTemp = InitWPDLAS()
        endif else begin
          dataTemp = bytarr(tempRecord.recordLength)
        endelse
        readu, inputLun, dataTemp
        records[a] = create_struct(tempRecord, 'data', ptr_new(dataTemp))
      endif else begin
        records[a] = create_struct(tempRecord, 'data', ptr_new())
      endelse
      
    endfor
    
  endif else begin
  
    records = -1
    
  endelse
  
  
  ; Read point data start signature if the file is in the LAS 1.0 format
  if (header.versionMajor eq 1 and header.versionMinor eq 0) then begin
    pointStart = bytarr(2)
    readu, inputLun, pointStart
  endif else begin
    point_lun, inputLun, header.dataOffset
  endelse
  
  if ~ keyword_set(noData) then begin
  
    ; Define a point data structure
    dataStr = InitDataLAS(pointFormat=header.pointFormat)
    
    if keyword_set(assocLun) then begin
    
      data = assoc(inputLun, dataStr, header.dataOffset, /packed)
      
      assocLun = inputLun
      
    endif else begin
    
      ; Create an array of data structures to contain all of the point data
      data = replicate(dataStr, header.nPoints)
      
      ; Read the point data
      point_lun, inputLun, header.dataOffset
      readu,     inputLun, data
      free_lun,  inputLun
      
    endelse
    
  endif else begin
  
    free_lun, inputLun
    
  endelse
  
end
