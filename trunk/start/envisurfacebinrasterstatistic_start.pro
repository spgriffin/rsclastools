;+
; NAME:
;
;   envisurfacebinrasterstatistic_start
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

PRO ENVISurfaceBinRasterStatistic_start, event

  Widget_Control, event.top, Get_UValue=info
  
  ; Open log file
  j_fpart = strtrim(round(systime(/julian)*10), 2)
  openw,  loglun, file_dirname(info.infile[0], /mark_directory) + 'Surface_LidarBinning_Processing_Notes_' + j_fpart + '.txt', /get_lun, width=100
  printf, loglun, 'Input files: ', info.infile
  start_mem = memory(/current)
  t = systime(1)
  
  ; Derive product
  Widget_Control, info.tmpflag, Get_Value=tmp
  resolution = info.resolution->Get_Value()
  zone = info.zone->Get_Value()
  null = info.null->Get_Value()
  minVal = info.minVal->Get_Value()
  maxVal = info.maxVal->Get_Value()
  tilexsize = info.tilexsize->Get_Value()
  tileysize = info.tileysize->Get_Value()
  Widget_Control, info.surfacetype, Get_Value=surfacetype
  case info.hemi_droplist->GetSelection() of
    info.hemiList[0]: hemisphere = 'South'
    info.hemiList[1]: hemisphere = 'North'
  endcase
  returnType = info.return_droplist->GetSelection()
  fieldType = info.field_droplist->GetSelection()
  productType = info.product_droplist->GetSelection()
  classType = info.class_droplist->GetSelection()
  case info.proj_droplist->GetSelection() of
    info.projList[0]: proj = 'MGA94'
    info.projList[1]: proj = 'BNG'
    info.projList[2]: proj = 'UTM'
  endcase
  Widget_Control, info.excludeTable, Get_Value=excludeTable
  Widget_Control, event.top, /Destroy
  productOptions = {method:productType,returnType:returnType,field:fieldType,class:classType,limits:[minVal,maxVal], excludeWater:excludeTable[0], excludeBuildings:excludeTable[1]}
  TileBinSurface, info.infile, resolution=resolution, zone=zone, tilesize=[tilexsize,tileysize], null=null, hemisphere=hemisphere, $
    proj=proj, productType='Statistic', separate=surfacetype, productOptions=productOptions,tmp=tmp
    
  ; Write to and close log file
  printf, loglun, 'Time required: ', systime(1) - t, ' seconds'
  printf, loglun, 'Memory required: ', memory(/highwater) - start_mem, ' bytes'
  printf, loglun, 'Spatial resolution: ', resolution, ' m'
  printf, loglun, 'Tile size: ', tilexsize, tileysize, ' m'
  free_lun, loglun
  
  ; Make file directory cwd
  cd, file_dirname(info.infile[0])
  
END
