;+
; NAME:
;
;       InitWPDLAS
;
; PURPOSE:
;
;       This function generates a structure containing the waveform packet descriptor (WPD).
;       The structure is contained with the WDP record.
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
;       record = InitWPDLAS()
;
; RETURN VALUE:
;
;       The function returns a structure corresponding to the waveform packet
;       descriptor of the LAS 1.3 file specification.
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

FUNCTION InitWPDLAS

  compile_opt idl2
  
  ; Define the waveform packet descriptor use defined record
  ; Currently set for Reigl LMSQ560 waveform data received from ARA
  wpd = {formatWPD0,                 $
    bitsPerSample       : 16B,          $ ; Bits per sample
    waveformCompression : 0B,           $ ; Waveform compression type
    numberSamples       : 50UL,         $ ; Number of samples
    temporalSpacing     : 1000UL,       $ ; Temporal sample spacing
    digitizerGain       : 1D,           $ ; Digitizer gain
    digitizerOffset     : 0D            $ ; Digitizer offset
    }
    
  return, wpd
  
END
