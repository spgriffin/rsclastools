;+
; NAME:
;
;   RSC_LAS_Tools
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

PRO RSC_LAS_TOOLS

  COMPILE_OPT IDL2
  
  ; Set system variables
  RSC_LAS_Tools_SysVar
  cd, current=cwd
  pushd, cwd
  
  ; Check version
  version = float(strmid(!Version.Release,0,3))
  if (version LT 6.3) then begin
    ok = dialog_message('Minimum IDL Version required is 6.3. Yours is ' + !Version.Release + '.', /information)
    return
  endif
  
  ; Do menus
  tlb = Widget_Base(Title=strjoin(['RSC LAS Tools', !QRSC_LIDAR_VERSION], ' '), Row=1, MBar=menuBase, xsize=!QRSC_LIDAR_XSIZE)
  fileMenu = Widget_Button(menuBase, Value='File', /Menu)
  headerMenu = Widget_button(fileMenu, Value='LAS Header', /Menu)
  button = Widget_button(headerMenu, Value='View', UValue='LasInfo')
  button = Widget_button(headerMenu, Value='Fix', UValue='LasCheck')  
  exportMenu = Widget_button(fileMenu, Value='Export LAS', /Menu)
  button = Widget_button(exportMenu, Value='ASCII', UValue='LAS2Ascii')
  button = Widget_button(exportMenu, Value='Shapefile', UValue='LAS2Shapefile')
  importMenu = Widget_Button(fileMenu, Value='Import to LAS', /Menu)
  button = Widget_button(importMenu, Value='ASCII', UValue='Ascii2LAS')
  button = Widget_button(importMenu, Value='TLS PTS', UValue='Ascii2LAS_TLS')
  button = Widget_Button(fileMenu, Value='"ENVI Lidar Tools" 3D Viewer', UValue='Visualise', /separator)
  button = Widget_Button(fileMenu, Value='Exit', UValue='Quit', /separator)
  
  mapMenu = Widget_Button(menuBase, Value='Map', /Menu)
  subsetMenu_sub = Widget_Button(mapMenu, Value='Subset LAS File', /Menu)
  button = Widget_Button(subsetMenu_sub, Value='Location', UValue='ExtractSite')
  button = Widget_Button(subsetMenu_sub, Value='Attribute', UValue='SplitAttributeData')
  mapMenu_sub = Widget_Button(mapMenu, Value='Tile LAS File', /Menu)
  button = Widget_button(mapMenu_sub, Value='Spatial', UValue='LidarTile')
  button = Widget_button(mapMenu_sub, Value='Temporal', UValue='SplitData')
  button = Widget_Button(mapMenu, Value='Merge LAS Files', UValue='LidarMerge')
  button = Widget_button(mapMenu, Value='Create Extent KML/SHP file', UValue='BoundsShapefile', /separator)
  
  interpMenu = Widget_Button(menuBase, Value='Interpolation', /Menu)
  button = Widget_Button(interpMenu, Value='Generate Above Ground Height LAS File', UValue='TilePointInterpHeights')
  button = Widget_Button(interpMenu, Value='Interpolated Lidar Surfaces', UValue='InterpENVISurfaceRaster')
  
  clasMenu = Widget_Button(menuBase, Value='Classification', /Menu)
  button = Widget_Button(clasMenu, Value='Ground Filter', UValue='TileGroundFilter')
  surfMenu = Widget_button(clasMenu, Value='Binned Lidar Surfaces', /Menu, /separator)
  button = Widget_Button(surfMenu, Value='Statistics', UValue='BinENVISurfaceBinRasterStatistic')
  button = Widget_Button(surfMenu, Value='Canopy Metrics', UValue='BinENVISurfaceBinRasterCanopy')
  button = Widget_Button(surfMenu, Value='Terrain Metrics', UValue='BinENVISurfaceBinRasterTerrain')
  pointMenu = Widget_Button(clasMenu, Value='Point ASCII Products', /Menu)
  button = Widget_Button(pointMenu, Value='Vertical Profile', UValue='FoliageProfile')
  button = Widget_Button(pointMenu, Value='Statistics', UValue='Point_ProfileStatistics')
  button = Widget_Button(pointMenu, Value='Canopy Metrics', UValue='Point_CanopyMetrics')
  button = Widget_Button(pointMenu, Value='Terrain Metrics', UValue='Point_TerrainMetrics')
  
  oldMenu = Widget_Button(menuBase, Value='Deprecated Stuff', /Menu)
  ointerpMenu = Widget_Button(oldMenu, Value='Interpolation', /Menu)
  button = Widget_Button(ointerpMenu, Value='Calculate Return Heights', UValue='Point_CalcHeights')
  button = Widget_Button(ointerpMenu, Value='Interpolated GeoTIFF Products', UValue='InterpRaster')
  oclassMenu = Widget_Button(oldMenu, Value='Classification', /Menu)
  button = Widget_Button(oclassMenu, Value='Elevation Filter', UValue='ElevFilter')
  gridMenu = Widget_button(oclassMenu, Value='Binned GeoTIFF Products', /Menu, /separator)
  button = Widget_Button(gridMenu, Value='Statistics', UValue='Bin_ProfileStatistics')
  button = Widget_Button(gridMenu, Value='Canopy Metrics', UValue='Bin_CanopyMetrics')
  button = Widget_Button(gridMenu, Value='Terrain Metrics', UValue='Bin_TerrainMetrics')

  helpMenu = Widget_Button(menuBase, Value='Help', /Menu)
  button = Widget_Button(helpMenu, Value='Readme', UValue='About')
  button = Widget_Button(helpMenu, Value='Licence', UValue='Licence')
  
  ; Do the rest
  Widget_Control, tlb, /Realize, Set_UValue={tlb:tlb}
  XManager, 'RSC_LAS_TOOLS', tlb, /No_Block
  
END
