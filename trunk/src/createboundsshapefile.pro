;+
; NAME:
;
;   CreateBoundsShapefile
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
;    KML added based on examples in http://code.google.com/apis/kml/documentation/kml_tut.html, July 2010.
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

PRO CreateBoundsShapefile, outfile, xcoords, ycoords, zcoord, name, kml=kml, in_proj=in_proj, out_proj=out_proj, zone=zone, hemisphere=hemisphere

  compile_opt idl2
  forward_function ReprojectCoords
  
  ; Keywords
  if not keyword_set(in_proj) then in_proj = 'MGA94'
  if not keyword_set(out_proj) then out_proj = 'Geographic'
  if not keyword_set(zone) then zone = 55
  if not keyword_set(hemisphere) then hemisphere = 'South'
  
  if keyword_set(kml) then begin
  
    openw,  lun, outfile + '.kml', /get_lun
    printf, lun, '<?xml version="1.0" encoding="UTF-8"?>'
    printf, lun, '<kml xmlns="http://www.opengis.net/kml/2.2">'
    printf, lun, '<Document>'
    printf, lun, '<Style id="transBluePoly">'
    printf, lun, '<LineStyle>'
    printf, lun, '<width>1.5</width>'
    printf, lun, '</LineStyle>'
    printf, lun, '<PolyStyle>'
    printf, lun, '<color>7dff0000</color>'
    printf, lun, '</PolyStyle>'
    printf, lun, '</Style>'
    printf, lun, '<Placemark>'
    printf, lun, '<name>' + name + '</name>'
    printf, lun, '<styleUrl>#transBluePoly</styleUrl>'
    printf, lun, '<Polygon>'
    printf, lun, '<extrude>1</extrude>'
    printf, lun, '<altitudeMode>clampToGround</altitudeMode>'
    printf, lun, '<outerBoundaryIs>'
    printf, lun, '<LinearRing>'
    printf, lun, '<coordinates>'
    
    ; Convert the coordinates to lat/long
    lnglat = ReprojectCoords(xcoords, ycoords, in_proj, 'Geographic', zone=zone, hemisphere=hemisphere)
    for i = 0L, n_elements(xcoords)-1L, 1L do begin
      printf, lun, strjoin([strtrim(lnglat[0,i], 2),strtrim(lnglat[1,i], 2),zcoord], ',')
    endfor
    
    printf, lun, '</coordinates>'
    printf, lun, '</LinearRing>'
    printf, lun, '</outerBoundaryIs>'
    printf, lun, '</Polygon>'
    printf, lun, '</Placemark>'
    printf, lun, '</Document>'
    printf, lun, '</kml>'
    close, lun
    
  endif else begin
  
    ; Convert the coordinates to lat/long
    vertices = ReprojectCoords(xcoords, ycoords, in_proj, out_proj, zone=zone, hemisphere=hemisphere)
    ;vertices = transpose([[xyout[0,*]],[xyout[1,*]]])
    
    ;Create the shapefile and define the entity type as Polygon
    mynewshape=OBJ_NEW('IDLffShape', outfile + '.shp', /update, entity_type=5)
    
    ;Set the attribute definitions for the Shapefile
    mynewshape->AddAttribute, 'NAME', 7, 100
    mynewshape->AddAttribute, 'AREA', 5, 12, precision=3
    mynewshape->AddAttribute, 'CENTRE_X', 5, 10, precision=3
    mynewshape->AddAttribute, 'CENTRE_Y', 5, 12, precision=3
    mynewshape->AddAttribute, 'PERIMETER', 5, 12, precision=3
    
    ;Create structure for the entity
    entNew = {IDL_SHAPE_ENTITY}
    
    ; Define the values for the entity
    dims = size(vertices, /dimensions)
    entNew.SHAPE_TYPE = 5L
    entNew.ISHAPE = 1L
    entNew.BOUNDS = [min(vertices[0,*]), min(vertices[1,*]), 0.0, 0.0, $
      max(vertices[0,*]), max(vertices[1,*]), 0.0, 0.0]
    entNew.N_VERTICES = dims[1]
    p_vertices = ptr_new(vertices)
    entNew.VERTICES = p_vertices
    
    ; Calculate attributes
    extentObj = Obj_New('IDLanROI', vertices[0,*], vertices[1,*])
    status = extentObj->IDLanROI::ComputeGeometry(AREA=las_area, CENTROID=las_centroid, PERIMETER=las_perimeter)
    Obj_Destroy, extentObj
    
    ;Create structure for the attributes
    attrNew = mynewshape ->GetAttributes(/attribute_structure)
    
    ;Define the values for the attributes
    attrNew.ATTRIBUTE_0 = name
    attrNew.ATTRIBUTE_1 = las_area
    attrNew.ATTRIBUTE_2 = las_centroid[0]
    attrNew.ATTRIBUTE_3 = las_centroid[1]
    attrNew.ATTRIBUTE_4 = las_perimeter
    
    ;Add the new entity to the shapefile
    mynewshape -> PutEntity, entNew
    
    ;Add the attributes to the shapefile
    mynewshape -> SetAttributes, 0, attrNew
    
    ;Close the shapefile
    mynewshape->IDLffShape::DestroyEntity, entNew
    mynewshape->Close
    obj_destroy, mynewshape
    ptr_free, p_vertices
    
  endelse
  
END
