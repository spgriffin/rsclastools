;+
; NAME:
;
;   About_GUI
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

PRO About_GUI, mainwidgetid

  ; Main
  cd, current=cwd
  popd
  cd, current=nwd
  
  ; Readme
  text = [strjoin(['RSC LAS Tools Version', !QRSC_LIDAR_VERSION], ' '), $
    !QRSC_LIDAR_DATE, $
    'See http://code.google.com/p/rsclastools for information.', $
    '', $
    '', $
    'If running Windows, then you just need to double click the "rsc_las_tools.sav" file, or run the IDL Virtual Machine from the start menu.', $
    'If running from the command line then enter the following line:', $
    'idl -vm=<filepath>', $
    'where <filepath> is the full path to the "rsc_las_tools.sav" file', $
    'e.g. idl -vm=/usr/people/armstonj/rsc_las_tools.sav', $
    '', $
    '', $
    'John Armston', $
    '', $
    'Joint Remote Sensing Research Program', $
    '  Remote Sensing Centre', $
    '  Department of Environment and Resource Management', $
    '  80 Meiers Road, Indooroopilly, QLD 4068, Australia', $
    '  ---', $
    '  Biophysical Remote Sensing Group', $
    '  School of Geography, Planning and Environmental Management', $
    '  The University of Queensland', $
    '  Brisbane QLD 4072, Australia', $
    '', $
    'Email: j.armston@uq.edu.au']
    
  ; Display readme
  infile = 'readme.txt'
  xdisplayfile, infile, title='About RSC LAS Tools', done_button='Close', width=125, height=50, text=text, /grow_to_screen, group=mainwidgetid
  pushd, nwd
  cd, cwd
  
END
