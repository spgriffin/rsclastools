;+
; NAME:
;
;   Licence_GUI
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

PRO Licence_GUI, mainwidgetid

  ; Main
  cd, current=cwd
  popd
  cd, current=nwd
  
  ; Licence
  text = ['LICENCE', $
    '', $
    strjoin(['RSC LAS Tools Version', !QRSC_LIDAR_VERSION], ' '), $
    strjoin([!QRSC_LIDAR_DATE, 'John Armston.'], ' '), $
    '', $
    'This program is free software: you can redistribute it and/or modify', $
    'it under the terms of the GNU General Public Licence as published by', $
    'the Free Software Foundation, either version 3 of the Licence, or', $
    '(at your option) any later version.', $
    '', $
    'This program is distributed in the hope that it will be useful,', $
    'but WITHOUT ANY WARRANTY; without even the implied warranty of', $
    'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the', $
    'GNU General Public Licence for more details.', $
    '', $
    'You should have received a copy of the GNU General Public Licence', $
    'along with this program.  If not, see <http://www.gnu.org/licenses/>.']
    
  ; Display licence
  infile = 'licence.txt'
  xdisplayfile, infile, title='Licence', done_button='Close', width=125, height=50, text=text, /grow_to_screen, group=mainwidgetid
  pushd, nwd
  cd, cwd
  
END
