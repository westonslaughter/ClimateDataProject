# Normalized Differential Vegetation Index Script
SSI Climate, Land Cover, and Water Quality Research

This repository contains code used for data extraction and summary for NDVI data in the catchments of our monitoring sites.

Tasks | Responsible
------- | --------
The mapping functions that create NDVI mean valuessketchy, needs review, code copied from [this stack](https://gis.stackexchange.com/questions/320821/getting-ndvi-data-only-for-march-from-2000-2003-using-google-earth-engine/320830) | DP/WS
Cloud masking function should be throughly reviewed and QAQCd | WS, Future AmeriCorps

Delete when complete!

# Metadata
* *landsat7collection*: USGS Landsat 7 Collection 1 Tier 1, 30m resolution
* *catchments*: Deer Creek site catchments, constructed from 30m DEM and monitoring site locations in ArcGIS
* *NHD*: NHD Flowline layer export of Deer Creek, Little Deer Creek and Squirrel Creek, clipped to DC_watershed in QGIS
* *NHD_waterbodies*: NHD waterbody layer export of Scotts Flat, Deer Creek Reservoir (Lower Scotts Flat) and Lake Wildwood selected in QGIS
