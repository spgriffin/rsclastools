;+
; NAME:
;
;   RSC_LAS_Tools_Event
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

PRO RSC_LAS_Tools_Event, event

  COMPILE_OPT IDL2
  
  ; The LIDAR event handler
  Widget_Control, event.id, Get_UValue=thisEvent
  
  ; What kind of event is this?
  type = size(thisEvent, /type)
  noEvent = n_elements(thisEvent)
  if ((noEvent GT 0L) AND (type EQ 7L)) then begin
    case thisEvent of
    
      'Implement': ok = dialog_message('Not Yet Available', /error)
      
      'ChangeColumns': begin
        Widget_Control, event.top, Get_UValue=info
        Widget_Control, info.table, Get_Value=table_value
        info.columns = table_value
        Widget_Control, event.top, Set_UValue=info
      end
      
      'ResetDefaults': begin
        Widget_Control, event.top, Get_UValue=info
        info.field1->Set_Value, 1.0
        info.field2->Set_Value, 7.0
        info.field3->Set_Value, 0.5
        info.field4->Set_Value, 30
        info.field5->Set_Value, 1.0
        info.field6->Set_Value, 0.15
      end
      
      'ChangeBounds': changebounds_start, event
      
      'Visualise': Visualize3D
      
      'About': begin
        Widget_Control, event.top, Get_UValue=info
        About_GUI, info.tlb
      end
      
      'Licence': begin
        Widget_Control, event.top, Get_UValue=info
        Licence_GUI, info.tlb
      end
      
      'LasInfo': LidarLASinfo_GUI

      'LasCheck': LidarLAScheck_GUI
      
      'LAS2Shapefile': LAS2Shapefile_GUI
      'StartLAS2Shapefile': LAS2Shapefile_start, event
      
      'LAS2Ascii': LAS2Ascii_GUI
      'StartLAS2Ascii': LAS2Ascii_start, event
      
      'Ascii2LAS': Ascii2LAS_GUI
      'StartAscii2LAS': Ascii2LAS_start, event
      
      'Ascii2LAS_TLS': Ascii2LAS_TLS_GUI
      'StartAscii2LAS_TLS': Ascii2LAS_TLS_start, event
      
      'BoundsShapefile': BoundsShapefile_GUI
      'StartBoundsShapefile': BoundsShapefile_start, event
      
      'ElevFilter': LidarFilter_GUI
      'StartFilter': Filter_start, event
      
      'SplitData': LidarSplitter_GUI
      'StartSplitData': SplitData_start, event
      
      'SplitAttributeData': LidarAttributeSplitter_GUI
      'StartSplitAttributeData': SplitAttributeData_start, event
      
      'LidarMerge': LidarMerge_GUI
      'StartLidarMerge': LidarMerge_start, event
      
      'LidarTile': LidarTile_GUI
      'StartLidarTile': LidarTile_start, event
      
      'ExtractSite': ExtractSite_GUI
      'StartExtractSite': ExtractSite_start, event
      
      'InterpRaster': LidarInterpolate_GUI
      'StartInterpRaster': InterpRaster_start, event
      
      'FoliageProfile': FoliageProfile_GUI
      'StartFoliageProfile': FoliageProfile_start, event
      
      'Bin_CanopyMetrics': LidarBin_CanopyMetrics_GUI
      'StartBin_CanopyMetrics': Bin_CanopyMetrics_start, event
      
      'Bin_TerrainMetrics': LidarBin_TerrainMetrics_GUI
      'StartBin_TerrainMetrics': Bin_TerrainMetrics_start, event
      
      'Bin_ProfileStatistics': LidarBin_ProfileStatistics_GUI
      'StartBin_ProfileStatistics': Bin_ProfileStatistics_start, event
      
      'Point_CanopyMetrics': LidarPoint_CanopyMetrics_GUI
      'StartPoint_CanopyMetrics': Point_CanopyMetrics_start, event
      
      'Point_TerrainMetrics': LidarPoint_TerrainMetrics_GUI
      'StartPoint_TerrainMetrics': Point_TerrainMetrics_start, event
      
      'Point_ProfileStatistics': LidarPoint_ProfileStatistics_GUI
      'StartPoint_ProfileStatistics': Point_ProfileStatistics_start, event
      
      'Point_CalcHeights': LidarPoint_CalcHeights_GUI
      'StartPoint_CalcHeights': Point_CalcHeights_start, event
      
      'InterpENVISurfaceRaster': LidarENVISurfaceInterpolate_GUI
      'StartENVISurfaceInterpRaster': ENVISurfaceInterpRaster_start, event
      
      'BinENVISurfaceBinRasterStatistic': LidarENVISurfaceBinStatistic_GUI
      'StartENVISurfaceBinRasterStatistic': ENVISurfaceBinRasterStatistic_start, event
      
      'BinENVISurfaceBinRasterTerrain': LidarENVISurfaceBinTerrain_GUI
      'StartENVISurfaceBinRasterTerrain': ENVISurfaceBinRasterTerrain_start, event
      
      'BinENVISurfaceBinRasterCanopy': LidarENVISurfaceBinCanopy_GUI
      'StartENVISurfaceBinRasterCanopy': ENVISurfaceBinRasterCanopy_start, event
      
      'Quit': Widget_Control, event.top, /Destroy
      
      else: return
      
    endcase
  endif
  
END
