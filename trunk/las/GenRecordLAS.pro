;+
; NAME:
;
;       GenRecordLAS
;
; PURPOSE:
;
;       This function generates a structure for mandatory variable length records for .las
;       files created by RSC Lidar Tools, which has restricted projections and WPD format.
;
; AUTHOR:
;
;       John Armston
;       Joint Remote Sensing Research Program
;       Centre for Spatial Environmental Research
;       School of Geography, Planning and Environmental Management
;       The University of Queensland
;       Brisbane QLD 4072, Australia
;       http://gpem.uq.edu.au/jrsrp
;
; CALLING SEQUENCE:
;
;       record = GenRecordLAS(PROJ=PROJ,WPD=WPD,ZONE=ZONE)
;
; RETURN VALUE:
;
;       The function returns a structure corresponding to variable length records of the .las
;       file specification.
;
; MODIFICATION HISTORY:
;
;       Written by John Armston, November 2010.
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

FUNCTION GenRecordLAS, proj=proj, wpd=wpd, wdp=wdp, zone=zone

  compile_opt idl2
  forward_function InitWPDLAS
  
  ; Generate GeoKeyDirectoryTag Record
  ; Bugger geotiff, I'm going with EPSG in the descriptor field
  if keyword_set(proj) then begin
    record = InitRecordLAS(/noData)
    record.userID = byte('LASF_Projection')
    record.recordID = 34735
    case proj of
      'UTM': begin
        epsg = strtrim(28300L + zone, 2)
        record.description = byte(strjoin(['EPSG',epsg], ':', /single))
      end
      'MGA94': begin
        epsg = strtrim(32700L + zone, 2)
        record.description = byte(strjoin(['EPSG',epsg], ':', /single))
      end
      'BNG': begin
        record.description = byte('EPSG:27700')
      end
    endcase
  endif
  
  ; Generate Waveform Packet Descriptor Record
  if keyword_set(wpd) then begin
    record = InitRecordLAS()
    record.userID = byte('LASF_Spec')
    record.recordID = 100
    record.description = byte('WaveformPacketDescriptor')
    *record.data = InitWPDLAS()
    record.recordLength = 26B
  endif

  ; Generate Waveform Packet Descriptor Record
  if keyword_set(wdp) then begin
    record = InitRecordLAS(/eVLR,/noData)
    record.userID = byte('LASF_Spec')
    record.recordID = 65535
    record.description = byte('WaveformDataPackets')
  endif
  
  ; Return the record
  return, record
  
END
